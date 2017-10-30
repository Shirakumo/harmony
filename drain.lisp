#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass drain (segment)
  ())

(defmethod initialize-instance :after ((drain drain) &key)
  (setf (cl-mixed-cffi:direct-segment-mix (handle drain)) (cffi:callback drain-mix)))

(cffi:defcallback drain-mix :void ((samples cl-mixed-cffi:size_t) (segment :pointer))
  (let ((drain (pointer->object segment)))
    (when drain
      (process drain samples))))

(defclass pack-drain (drain)
  ((remix-factor :initform 0 :accessor remix-factor)
   (channel :initform NIL :accessor channel)))

(defmethod initialize-instance :After ((drain pack-drain) &key)
  (setf (channel drain) (initialize-channel drain))
  (setf (remix-factor drain) (coerce (/ (samplerate (server drain))
                                        (samplerate (channel drain)))
                                     'single-float)))

;; FIXME: call the packing before decode
(defmethod process :before ((drain pack-drain) samples)
  )
