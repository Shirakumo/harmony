#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass source (cl-mixed:source)
  ((looping-p :initarg :loop :initform NIL :accessor looping-p)
   (paused-p :initarg :paused-p :initform NIL :accessor paused-p)
   (ended-p :initform NIL :accessor ended-p)
   (decoder :initform (lambda (samples source)) :accessor decoder)
   (server :initarg :server :initform NIL :accessor server)
   (sample-position :initform 0 :accessor sample-position)
   (remix-factor :initform 0 :accessor remix-factor)
   (channel-function :initform NIL :accessor channel-function)))

(defmethod initialize-instance ((source source) &rest args &key server)
  (unless server
    (error "The SERVER initarg is required, but not given."))
  (setf (server source) server)
  (apply #'call-next-method
         :channel (initialize-channel source)
         :samplerate (samplerate server)
         args))

(defmethod initialize-instance :after ((source source) &key)
  (setf (remix-factor source) (coerce (/ (cl-mixed:samplerate (cl-mixed:channel source))
                                         (samplerate (server source)))
                                      'single-float))
  (setf (channel-function source) (cl-mixed-cffi:direct-segment-mix (cl-mixed:handle source)))
  (setf (cl-mixed-cffi:direct-segment-mix (cl-mixed:handle source)) (cffi:callback source-mix)))

(defgeneric initialize-channel (source))

(defmethod (setf paused-p) :before (value (source source))
  (when value
    (unless (paused-p source)
      (with-server-lock ((server source))
        (mapc #'cl-mixed:clear (cl-mixed:outputs source))))))

(defmethod pause ((source source))
  (setf (paused-p source) T))

(defmethod resume ((source source))
  (when (ended-p source)
    (seek source 0))
  (setf (paused-p source) NIL))

(defmethod stop ((source source))
  (setf (ended-p source) T))

(defgeneric seek (source position &key mode by))

(defmethod seek :around ((source source) position &key (mode :absolute) (by :sample))
  (ecase by
    (:second
     (setf position (round (* position (cl-mixed:samplerate (cl-mixed:channel source)))))))
  (ecase mode
    (:relative
     (setf mode :absolute)
     (incf position (sample-position source))))
  (call-next-method source position :mode :absolute :by :sample)
  (setf (ended-p source) NIL)
  (setf (sample-position source) position))

;; (defmethod position ((source source))
;;   (values (sample-position source)
;;           (coerce (/ (sample-position source)
;;                      (cl-mixed:samplerate (cl-mixed:channel source)))
;;                   'single-float)))

;; (defmethod (setf position) (value (source source))
;;   (seek source value :mode :absolute :by :sample))

(cffi:defcallback source-mix :void ((samples cl-mixed-cffi:size_t) (segment :pointer))
  (let* ((source (cl-mixed:pointer->object segment))
         (real-samples (floor samples (remix-factor source))))
    (unless (paused-p source)
      ;; We need to handle ended-p like this in order to make
      ;; sure that the last samples that were processed before
      ;; ended-p was set still get out before we clear the
      ;; buffers (by setting paused-p to T).
      (cond ((ended-p source)
             (setf (paused-p source) T))
            (T
             ;; Decode samples from the source
             (funcall (decoder source) real-samples source)
             ;; Process the channel to get the samples into buffers
             (cffi:foreign-funcall-pointer
              (channel-function source) ()
              cl-mixed-cffi:size_t samples
              :pointer segment
              :void)
             ;; Count current stream position
             (incf (sample-position source) real-samples))))))
