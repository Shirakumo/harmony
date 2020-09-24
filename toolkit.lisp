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
