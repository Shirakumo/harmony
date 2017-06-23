#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defvar *in-processing-thread* NIL)

(defclass server ()
  ((segment-map :initform NIL :accessor segment-map)
   (buffersize :initarg :buffersize :reader buffersize)
   (samplerate :initarg :samplerate :reader samplerate)
   (device :initarg :device :accessor device)
   (pipeline-mixer :initform NIL :accessor pipeline-mixer)
   (buffers :initform NIL :accessor buffers)
   (thread :initform NIL :accessor thread)
   ;; Synchronisation state
   (evaluation-queue :initform NIL :accessor evaluation-queue)
   (evaluation-lock :initform NIL :accessor evaluation-lock))
  (:default-initargs
   :buffersize 441
   :samplerate 44100))

(defmethod initialize-instance :after ((server server) &key)
  (check-type (buffersize server) (integer 1))
  (check-type (samplerate server) (integer 1))
  (setf (segment-map server) (make-hash-table :test 'eql))
  (setf (evaluation-queue server) (make-array 32 :adjustable T :fill-pointer 0 :initial-element NIL))
  (setf (evaluation-lock server) (bt:make-lock (format NIL "~a evaluation lock." server))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type T)
    (format stream "~@[~*running~]" (thread server))))

(defmethod segment ((name symbol) (server server))
  (gethash name (segment-map server)))

(defmethod (setf segment) ((segment segment) (name symbol) (server server))
  (setf (gethash name (segment-map server)) segment))

(defmethod start ((server server))
  (when (thread server)
    (error "~a is already running." server))
  (unless (device server)
    (error "No device has been assigned to ~a yet.~
            Did you compile a pipeline?" server))
  (unless (pipeline-mixer server)
    (error "No mixer object has been assigned to ~a yet.~
            Did you compile a pipeline?" server))
  (setf (thread server) T)
  (let ((thread (bt:make-thread (lambda () (process server))
                                :name (format NIL "~a process thread." server)
                                :initial-bindings `((*standard-output* . ,*standard-output*)
                                                    (*error-output* . ,*error-output*)
                                                    (*query-io* . ,*query-io*)))))
    (setf (thread server) thread)
    thread))

(defmethod stop ((server server))
  (when (thread server)    
    (let ((thread (thread server)))
      (setf (thread server) NIL)
      (restart-case
          (loop for i from 0
                while (bt:thread-alive-p thread)
                do (sleep 0.01)
                   (when (= i 100)
                     (with-simple-restart (continue "Continue waiting.")
                       (error "~a's thread is not shutting down." server))))
        (abort ()
          :report "Attempt to forcibly terminate the thread."
          (bt:destroy-thread thread))
        (ignore ()
          :report "Return and ignore the running thread."))
      thread)))

(defmethod process ((server server))
  (let ((mixer (handle (pipeline-mixer server)))
        (device (device server))
        (samples (buffersize server))
        (evaluation-lock (evaluation-lock server))
        (back-queue (make-array 32 :adjustable T :fill-pointer 0 :initial-element NIL)))
    (cl-mixed-cffi:mixer-start mixer)
    (let ((*in-processing-thread* T))
      (unwind-protect
           (loop while (thread server)
                 do (with-simple-restart (continue "Continue with fingers crossed.")
                      (cond ((< 0 (fill-pointer (evaluation-queue server)))
                             (bt:with-lock-held (evaluation-lock)
                               (rotatef back-queue (evaluation-queue server)))
                             (loop for i from 0 below (fill-pointer back-queue)
                                   do (funcall (aref back-queue i))
                                      (setf (aref back-queue i) NIL))
                             (setf (fill-pointer back-queue) 0)
                             ;; Properties might have changed.
                             (setf mixer (handle (pipeline-mixer server)))
                             (setf device (device server)))
                            ((paused-p device)
                             (bt:thread-yield))
                            (T
                             (cl-mixed-cffi:mixer-mix samples mixer)))))
        (cl-mixed-cffi:mixer-end mixer)
        (setf (thread server) NIL)))))

(defun call-in-server-thread (function server &key synchronize timeout values)
  (cond ((not (thread server))
         (funcall function))
        ((and synchronize (not *in-processing-thread*))
         (let ((lock (bt:make-lock))
               (monitor (bt:make-condition-variable))
               (values-list ()))
           (bt:with-lock-held (lock)
             (bt:with-lock-held ((evaluation-lock server))
               (vector-push-extend (if values
                                       (lambda ()
                                         (unwind-protect
                                              (setf values-list (multiple-value-list (funcall function)))
                                           (bt:condition-notify monitor)))
                                       (lambda ()
                                         (unwind-protect
                                              (funcall function)
                                           (bt:condition-notify monitor))))
                                   (evaluation-queue server)))
             (bt:condition-wait monitor lock :timeout timeout)
             (values-list values-list))))
        (T
         (bt:with-lock-held ((evaluation-lock server))
           (vector-push-extend function (evaluation-queue server))))))

(defmacro with-body-in-server-thread ((server &key synchronize timeout) &body body)
  `(call-in-server-thread (lambda () ,@body) ,server :synchronize ,synchronize :timeout ,timeout))

(defmethod paused-p ((server server))
  (paused-p (device server)))

(defmethod (setf paused-p) (value (server server))
  (setf (paused-p (device server)) value))

(defmethod pause ((server server))
  (pause (device server)))

(defmethod resume ((server server))
  (resume (device server)))
