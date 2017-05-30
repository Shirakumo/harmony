#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass fadable (cl-mixed:c-object)
  ((start-volume :initform 1.0 :accessor start-volume)
   (target-volume :initform 1.0 :accessor target-volume)
   (fade-count :initform 0 :accessor fade-count)
   (fade-end :initform 0 :accessor fade-end)
   (easing-function :initform #'ease-linear :accessor easing-function)))

(defmethod fade ((fadable fadable) to time &key (by (easing-function fadable)))
  (let ((target (floor (* time (samplerate (server source))))))
    (with-body-in-server-thread ((server source))
      (setf (target-volume fadable) to)
      (setf (fade-count fadable) 0)
      (setf (fade-end fadable) target)
      (setf (easing-function fadable) by))))

(declaim (inline adjust-volume))
(defun adjust-volume (fadable samples)
  (let ((current (volume fadable))
        (target (target-volume fadable)))
    (when (/= current target)
      (let ((x (/ (fade-count fadable)
                  (fade-end fadable)))
            (start (start-volume fadable)))
        (setf (volume fadable) (+ start (* (- target start) (funcall (easing-function fadable) x)))))
      (incf (fade-count fadable) samples))))

(defun ease-linear (x)
  x)
