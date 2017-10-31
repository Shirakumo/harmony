#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass fadable (segment)
  ((start-volume :initform 1.0 :accessor start-volume)
   (target-volume :initform 1.0 :accessor target-volume)
   (fade-count :initform 0 :accessor fade-count)
   (fade-end :initform 0 :accessor fade-end)
   (easing-function :initform #'ease-linear :accessor easing-function)))

(defmethod initialize-instance :after ((fadable fadable) &key volume fade-time fade-from)
  (when fade-time
    (fade fadable volume fade-time :from (or fade-from 0.0))))

(defmethod fade ((fadable fadable) to time &key (by (easing-function fadable)) from)
  (let ((target (floor (* time (samplerate (server fadable)))))
        (from  (coerce (or from (volume fadable)) 'single-float))
        (to (coerce to 'single-float)))
    (check-type by function)
    (with-body-in-server-thread ((server fadable))
      (setf (fade-count fadable) 0)
      (setf (fade-end fadable) target)
      (setf (easing-function fadable) by)
      (setf (start-volume fadable) from)
      (setf (target-volume fadable) to)
      (setf (volume fadable) from))))

(declaim (inline perform-fading))
(defun perform-fading (fadable samples)
  (declare (optimize speed))
  (let ((count (fade-count fadable))
        (end (fade-end fadable)))
    (declare (type (unsigned-byte 32) count end))
    (when (< count end)
      (let* ((start (start-volume fadable))
             (target (target-volume fadable))
             (x (/ (coerce count 'single-float) end))
             (volume (+ start (* (- target start)
                                 (the single-float (funcall (the function (easing-function fadable)) x))))))
        (declare (type single-float start target))
        (setf (volume fadable) volume))
      (incf (fade-count fadable) samples))))

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
