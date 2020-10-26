#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defun ease-linear (x)
  (declare (optimize speed))
  (declare (type single-float x))
  x)

(defun ease-cubic-in (x)
  (declare (optimize speed))
  (declare (type single-float x))
  (expt x 3))

(defun ease-cubic-out (x)
  (declare (optimize speed))
  (declare (type single-float x))
  (1+ (expt (1- x) 3)))

(defun ease-cubic-in-out (x)
  (declare (optimize speed))
  (declare (type single-float x))
  (if (< x 0.5)
      (/ (expt (* 2 x) 3) 2)
      (1+ (/ (expt (* 2 (1- x)) 3) 2))))

(defmacro push* (value place)
  (let ((val (gensym "VAL"))
        (old (gensym "OLD")))
    `(loop with ,val = ,value
           for ,old = ,place
           until (atomics:cas ,place ,old (list* ,val ,old)))))

(defmacro pop* (place)
  (let ((old (gensym "OLD")))
    `(loop for ,old = ,place
           until (atomics:cas ,place ,old (rest ,old))
           finally (return (car ,old)))))

(defmethod segment ((idx integer) (chain mixed:chain) &optional (errorp T))
  (let ((segments (mixed:segments chain)))
    (if (<= 0 idx (1- (length segments)))
        (aref (mixed:segments chain) idx)
        (when errorp (error "No segment at index~%  ~d" idx)))))

(defmethod segment ((name symbol) (chain mixed:chain) &optional (errorp T))
  (or (loop for segment across (mixed:segments chain)
            do (when (eql name (name segment))
                 (return segment)))
      (when errorp (error "No segment with name~%  ~s" name))))

(defun find-symbol* (package name)
  (loop (restart-case
            (return (or (and (find-package (string package))
                             (find-symbol (string name) (string package)))
                        (error "Symbol ~a:~a is not present." (string package) (string name))))
          (retry ()
            :report "Retry the evaluation")
          (use-value (value)
            :report "Supply a symbol to use"
            (return value)))))

(defmacro lazy-symbol (package name)
  `(find-symbol* ,(string package) ,(string name)))

(defun add-to (target &rest parts)
  (dolist (part parts target)
    (mixed:add part target)))

(defun removef (plist &rest keys)
  (loop for (key val) on plist by #'cddr
        for found = (member key keys)
        unless found collect key
        unless found collect val))

(defun set-process-priority (&optional (priority :normal))
  #+windows
  (cffi:foreign-funcall "SetPriorityClass"
                        :pointer (cffi:foreign-funcall "GetCurrentProcess" :pointer)
                        :uint16 (ecase priority
                                  (:idle     #x00000040)
                                  (:low      #x00004000)
                                  (:normal   #x00000020)
                                  (:high     #x00000080)
                                  (:realtime #x00000100))
                        :bool)
  #+unix
  (cffi:foreign-funcall "setpriority"
                        :int 0 :uint32 0 :int
                        (ecase priority
                          (:idle      19)
                          (:low        5)
                          (:normal     0)
                          (:high      -5)
                          (:realtime -20))
                        :int))

(defun set-thread-priority (&optional (priority :normal))
  #+windows
  (cffi:foreign-funcall "SetThreadPriority"
                        :pointer (cffi:foreign-funcall "GetCurrentThread" :pointer)
                        :int (ecase priority
                               (:idle    -15)
                               (:low      -1)
                               (:normal    0)
                               (:high      2)
                               (:realtime 15))
                        :bool)
  #+linux ;; Turns out linux violates the posix spec and niceness is the way to go.
  (set-process-priority priority)
  #+(and unix (not linux))
  (cffi:with-foreign-objects ((policy :int)
                              (param :int))
    (cffi:foreign-funcall "pthread_getschedparam"
                          :pointer (cffi:foreign-funcall "pthread_self" :pointer)
                          :pointer policy
                          :pointer param
                          :int)
    (let ((policy (cffi:mem-ref policy :int)))
      (setf (cffi:mem-ref param :int) (ecase priority
                                        (:idle      1)
                                        (:low      40)
                                        (:normal   50)
                                        (:high     60)
                                        (:realtime 99)))
      (cffi:foreign-funcall "pthread_setschedparam"
                            :pointer (cffi:foreign-funcall "pthread_self" :pointer)
                            :int policy
                            :pointer param
                            :int))))
