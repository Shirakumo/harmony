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

(defmethod segment ((idx integer) (chain mixed:chain))
  (aref (mixed:segments chain) idx))
