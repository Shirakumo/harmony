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
   (pack-mix-function :initform NIL :accessor pack-mix-function)))

(defmethod initialize-instance ((drain pack-drain) &key)
  (call-next-method)
  (setf (packed-audio drain) (initialize-packed-audio drain))
  (setf (remix-factor drain) (coerce (/ (samplerate (server drain))
                                        (samplerate (packed-audio drain)))
                                     'single-float))
  (cl-mixed::with-error-on-failure ()
    (cl-mixed-cffi:make-segment-packer (handle (packed-audio drain)) (samplerate (server drain)) (handle drain)))
  (setf (pack-mix-function drain) (cl-mixed-cffi:direct-segment-mix (handle drain))))

(defmethod process :around ((drain pack-drain) samples)
  (let ((endpoint-samples (floor (* samples (remix-factor drain)))))
    ;; Pack
    (cffi:foreign-funcall-pointer
     (pack-mix-function drain) ()
     cl-mixed-cffi:size_t samples
     :pointer (handle drain))
    ;; Encode
    (call-next-method drain endpoint-samples)))

(defmethod pause ((drain drain))
  (setf (paused-p drain) T)
  drain)

(defmethod resume ((drain drain))
  (setf (paused-p drain) NIL)
  drain)

(cl-mixed::define-field-accessor volume pack-drain :float :volume)
(cl-mixed::define-field-accessor bypass pack-drain :bool :bypass)
