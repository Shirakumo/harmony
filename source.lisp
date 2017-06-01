#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass source (cl-mixed:source fadable)
  ((looping-p :initarg :loop :initform NIL :accessor looping-p)
   (paused-p :initarg :paused :initform NIL :accessor paused-p)
   (ended-p :initform NIL :accessor ended-p)
   (decoder :initform (constantly T) :accessor decoder)
   (sample-position :initform 0 :accessor sample-position)
   (remix-factor :initform 0 :accessor remix-factor)
   (channel-function :initform NIL :accessor channel-function)
   (mixer :initform NIL :accessor mixer)))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type T)
    (format stream "~:[~; looping~]~:[~; paused~]~:[~; ended~]~:[~; playing~]"
            (looping-p source) (paused-p source) (ended-p source)
            (and (not (paused-p source)) (not (ended-p source))))))

(defmethod initialize-instance ((source source) &rest args &key server)
  (apply #'call-next-method
         source
         :channel NIL
         :samplerate (samplerate server)
         args)
  (setf (slot-value source 'channel) (initialize-channel source)))

(defmethod initialize-instance :after ((source source) &key)
  (setf (remix-factor source) (coerce (/ (samplerate (channel source))
                                         (samplerate (server source)))
                                      'single-float))
  (setf (channel-function source) (cl-mixed-cffi:direct-segment-mix (handle source)))
  (setf (cl-mixed-cffi:direct-segment-mix (handle source)) (cffi:callback source-mix)))

(defgeneric initialize-channel (source))

(defmethod (setf paused-p) :before (value (source source))
  (when value
    (unless (paused-p source)
      (with-body-in-server-thread ((server source))
        (map NIL #'clear (outputs source))))))

(defmethod pause ((source source))
  (setf (paused-p source) T))

(defmethod resume ((source source))
  (when (ended-p source)
    (seek source 0))
  (setf (paused-p source) NIL))

(defmethod add :before ((source source) (mixer mixer))
  (when (mixer source)
    (error "~a is already attached to ~a." source mixer)))

(defmethod add :after ((source source) (mixer mixer))
  (setf (mixer source) mixer))

(defmethod withdraw :before ((source source) (mixer mixer))
  (unless (eq mixer (mixer source))
    (error "~a is not attached to ~a." source mixer)))

(defmethod withdraw :after ((source source) (mixer mixer))
  (setf (mixer source) NIL))

(defmethod (setf ended-p) :after (value (source source))
  (when (and value (mixer source))
    (withdraw source (mixer source))))

(defmethod stop ((source source))
  (setf (ended-p source) T))

(defmethod seek :around ((source source) position &key (mode :absolute) (by :sample))
  (ecase by
    (:second
     (setf position (round (* position (samplerate (channel source)))))))
  (ecase mode
    (:relative
     (setf mode :absolute)
     (incf position (sample-position source))))
  (seek-to-sample source position)
  (setf (ended-p source) NIL)
  (setf (sample-position source) position))

(defgeneric seek-to-sample (source position))

(cffi:defcallback source-mix :void ((samples cl-mixed-cffi:size_t) (segment :pointer))
  (let* ((source (pointer->object segment))
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
             (perform-fading source samples)
             (incf (sample-position source) real-samples))))))
