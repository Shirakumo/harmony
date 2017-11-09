#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(in-package #:org.shirakumo.fraf.harmony)

(defclass buffer-source (virtual source)
  ((buffers :initform #() :accessor buffers))
  (:default-initargs
   :buffers (error "BUFFERS required.")))

(defmethod initialize-instance :after ((source buffer-source) &key buffers)
  (setf (buffers source) (coerce buffers 'vector)))

(defmethod seek-to-sample ((source buffer-source) position)
  (setf (sample-position source) position))

(defmethod process ((source buffer-source) samples)
  (let* ((buffers (buffers source))
         (outputs (outputs source))
         (size (* (cl-mixed:size (aref buffers 0)) (cffi:foreign-type-size :float)))
         (pos (* (sample-position source) (cffi:foreign-type-size :float)))
         (bytes (* samples (cffi:foreign-type-size :float)))
         (read (min bytes (- size pos))))
    (loop for i from 0 below (length buffers)
          for buffer = (cl-mixed:data (aref buffers i))
          for output = (cl-mixed:data (aref outputs i))
          do (memcpy output (cffi:inc-pointer buffer pos) read)
             (when (< read bytes)
               (cond ((looping-p source)
                      (loop while (< 0 bytes)
                            for offset from (+ pos read) by read
                            do (decf bytes read)
                               (setf (sample-position source) 0)
                               (memcpy (cffi:inc-pointer output (- offset pos))
                                       (cffi:inc-pointer buffer offset) read)))
                     (T
                      (memclear (cffi:inc-pointer output read) (- bytes read))
                      (setf (ended-p source) T)))))))
