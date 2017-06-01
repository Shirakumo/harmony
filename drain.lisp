#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass drain (cl-mixed:drain segment)
  ((decoder :initform (constantly T) :accessor decoder)
   (channel-function :initform NIL :accessor channel-function)
   (remix-factor :initform 0 :accessor remix-factor)))

(defmethod initialize-instance ((drain drain) &rest args &key server)
  (apply #'call-next-method
         drain
         :channel NIL
         :samplerate (samplerate server)
         args)
  (setf (slot-value drain 'channel) (initialize-channel drain)))

(defmethod initialize-instance :after ((drain drain) &key)
  (setf (remix-factor drain) (coerce (/ (samplerate (server drain))
                                         (samplerate (channel drain)))
                                      'single-float))
  (setf (channel-function drain) (cl-mixed-cffi:direct-segment-mix (handle drain)))
  (setf (cl-mixed-cffi:direct-segment-mix (handle drain)) (cffi:callback drain-mix)))

(cffi:defcallback drain-mix :void ((samples cl-mixed-cffi:size_t) (segment :pointer))
  (let* ((drain (pointer->object segment))
         (real-samples (floor samples (remix-factor drain))))
    ;; Process the channel to get the samples from the buffers
    (cffi:foreign-funcall-pointer
     (channel-function drain) ()
     cl-mixed-cffi:size_t samples
     :pointer segment
     :void)
    ;; Decode samples from the drain
    (funcall (decoder drain) real-samples drain)))
