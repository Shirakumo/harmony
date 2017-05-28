#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass drain (cl-mixed:drain)
  ((decoder :initform (lambda (samples drain)) :accessor decoder)
   (server :initarg :server :initform NIL :accessor server)
   (channel-function :initform NIL :accessor channel-function)
   (remix-factor :initform 0 :accessor remix-factor)))

(defmethod initialize-instance ((drain drain) &rest args &key server)
  (unless server
    (error "The SERVER initarg is required, but not given."))
  (setf (server drain) server)
  (apply #'call-next-method
         :channel (initialize-channel drain)
         :samplerate (samplerate server)
         args))

(defmethod initialize-instance :after ((drain drain) &key)
  (setf (remix-factor source) (coerce (/ (samplerate (server source))
                                         (cl-mixed:samplerate (cl-mixed:channel source)))
                                      'single-float))
  (setf (channel-function source) (cl-mixed-cffi:direct-segment-mix (cl-mixed:handle source)))
  (setf (cl-mixed-cffi:direct-segment-mix (cl-mixed:handle source)) (cffi:callback drain-mix)))

(cffi:defcallback drain-mix :void ((samples cl-mixed-cffi:size_t) (segment :pointer))
  (let* ((drain (cl-mixed:pointer->object segment))
         (real-samples (floor samples (remix-factor drain))))
    ;; Process the channel to get the samples from the buffers
    (cffi:foreign-funcall-pointer
     (channel-function drain) ()
     cl-mixed-cffi:size_t samples
     :pointer segment
     :void)
    ;; Decode samples from the drain
    (funcall (decoder drain) real-samples drain)))
