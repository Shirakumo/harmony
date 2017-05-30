#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony.core)

(defclass fadable (c-object)
  ((start-volume :initform 1.0 :accessor start-volume)
   (target-volume :initform 1.0 :accessor target-volume)
   (fade-count :initform 0 :accessor fade-count)
   (fade-end :initform 0 :accessor fade-end)
   (easing-function :initform #'ease-linear :accessor easing-function)))

(defmethod fade ((fadable fadable) to time &key (by (easing-function fadable)))
  (let ((target (floor (* time (samplerate (server source))))))
    (setf (fade-count fadable) 0)
    (setf (fade-end fadable) target)
    (setf (easing-function fadable) by)
    ;; Set target volume last to avoid having to synchronise.
    ;; Might still mean things get screwed for a bit if you
    ;; are unlucky.
    (setf (target-volume fadable) to)))

(declaim (inline perform-fading))
(defun perform-fading (fadable samples)
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

(defun ease-cubic-in (x)
  (expt x 3))

(defun ease-cubic-out (x)
  (1+ (expt (1- x) 3)))

(defun ease-cubic-in-out (x)
  (if (< x 0.5)
      (/ (expt (* 2 x) 3) 2)
      (1+ (/ (expt (* 2 (1- x)) 3) 2))))
