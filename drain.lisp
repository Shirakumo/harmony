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
   (packed-audio :initform NIL :accessor packed-audio)
   (pack-mix-function :initform NIL :accessor unpack-mix-function)))

(defmethod initialize-instance ((drain pack-drain) &key)
  (call-next-method)
  (setf (remix-factor drain) (coerce (/ (samplerate (server drain))
                                        (samplerate (channel drain)))
                                     'single-float))
  (setf (packed-audio drain) (initialize-packed-audio drain))
  (cl-mixed::with-error-on-failure ()
    (cl-mixed-cffi:make-segment-packer (handle (packed-audio source)) (samplerate (server source)) (handle source)))
  (setf (pack-mix-function source) (cl-mixed-cffi:direct-segment-mix (handle source))))

(defmethod process :around ((drain pack-drain) samples)
  (let ((endpoint-samples (* samples (remix-factor source))))
    ;; Pack
    (cffi:foreign-funcall-pointer
     (pack-mix-function source) ()
     cl-mixed-cffi:size_t samples
     :pointer (handle source))
    ;; Encode
    (call-next-method source endpoint-samples)))
