#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass buffer-drain (cl-mixed:virtual drain)
  ((buffers :initform #() :accessor buffers)
   (sample-position :initform 0 :accessor sample-position))
  (:default-initargs
   :buffers (error "BUFFERS required.")))

(defmethod initialize-instance :after ((drain buffer-drain) &key buffers)
  (setf (buffers drain) (coerce buffers 'vector)))

(defmethod process ((drain buffer-drain) samples)
  (let* ((buffers (buffers drain))
         (inputs (inputs drain))
         (pos (* (sample-position drain) (cffi:foreign-type-size :float)))
         (write (min samples (- (cl-mixed:size (aref buffers 0)) (sample-position drain))))
         (bytes (* write (cffi:foreign-type-size :float))))
    (loop for i from 0 below (length buffers)
          for buffer = (cl-mixed:data (aref buffers i))
          for input = (cl-mixed:data (aref inputs i))
          do (memcpy (cffi:inc-pointer buffer pos) input bytes))
    (incf (sample-position drain) write)))

(defmethod cl-mixed:start ((drain buffer-drain))
  (setf (sample-position drain) 0))
