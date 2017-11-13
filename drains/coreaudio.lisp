#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-coreaudio
  (:nicknames #:org.shirakumo.fraf.harmony.drains.coreaudio)
  (:use #:cl #:harmony)
  (:export
   #:coreaudio-error
   #:code
   #:coreaudio-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.coreaudio)

(define-condition coreaudio-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "CoreAudio error ~d" (code c)))))

(defmacro with-error (() &body body)
  (let ((error (gensym "ERROR")))
    `(let ((,error (progn ,@body)))
       (when (/= ,error 0)
         (error 'coreaudio-error :code ,error)))))

(defclass coreaudio-drain (pack-drain)
  ((audio-unit :initform NIL :accessor audio-unit)
   (paused-p :initform NIL :accessor paused-p)
   (channels :initarg :channels :reader channels)
   (processed :initform NIL :accessor processed))
  (:default-initargs
   :channels 2))

(defmethod initialize-instance :after ((drain coreaudio-drain) &key)
  (setf (samples (context drain))
        (setf (buffersize (context drain)) 512))
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-packed-audio ((drain coreaudio-drain))
  (cl-mixed:make-packed-audio
   NIL
   (* (buffersize (context drain))
      (cl-mixed:samplesize :float)
      (channels drain))
   :float
   (channels drain)
   :alternating
   (samplerate (context drain))))

(defmethod process ((drain coreaudio-drain) samples)
  (setf (processed drain) T)
  (loop while (processed drain)
        do (sleep 0.005)))

(cffi:defcallback buffer-render harmony-coreaudio-cffi:os-status
    ((handle :pointer)
     (action-flags :pointer)
     (time-stamp :pointer)
     (bus-number :uint32)
     (frames :uint32)
     (io-data :pointer))
  (declare (ignore action-flags time-stamp bus-number))
  (let* (#+sbcl (sb-sys:*interrupts-enabled* NIL)
         #+sbcl (sb-kernel:*gc-inhibit* T)
         (drain (cl-mixed:pointer->object handle))
         (bytes (* frames (cffi:foreign-type-size :float)))
         (buffer (cffi:foreign-slot-pointer io-data
                                            '(:struct harmony-coreaudio-cffi:audio-buffer-list)
                                            'harmony-coreaudio-cffi::buffers)))
    (loop until (processed drain)
          do (sleep 0.005))
    (memcpy (harmony-coreaudio-cffi:audio-buffer-data buffer)
            (cl-mixed:data (packed-audio drain))
            bytes)
    (setf (processed drain) NIL))
  harmony-coreaudio-cffi:no-err)

(defmethod (setf paused-p) :before (value (drain coreaudio-drain))
  (with-body-in-mixing-context ((context drain))
    (with-error ()
      (if value
          (harmony-coreaudio-cffi:audio-output-unit-stop (audio-unit drain))
          (harmony-coreaudio-cffi:audio-output-unit-start (audio-unit drain))))))

(defun create-component-description (description)
  ;; This is always the same. Why we need this at all, I don't know. #justapplethings
  (setf (harmony-coreaudio-cffi:audio-component-description-component-type description)
        harmony-coreaudio-cffi:kAudioUnitType_Output)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-sub-type description)
        harmony-coreaudio-cffi:kAudioUnitSubType_DefaultOutput)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-manufacturer description)
        harmony-coreaudio-cffi:kAudioUnitManufacturer_Apple)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-flags description)
        0)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-flags-mask description)
        0))

(defun create-stream-description (stream samplerate channels)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-sample-rate stream)
        (coerce samplerate 'double-float))
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-format-id stream)
        harmony-coreaudio-cffi:kAudioFormatLinearPCM)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-format-flags stream)
        harmony-coreaudio-cffi:kAudioFormatFlagsNativeFloatPacked)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-bytes-per-packet stream)
        (* 4 channels))
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-frames-per-packet stream)
        1)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-bytes-per-frame stream)
        (* 4 channels))
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-channels-per-frame stream)
        channels)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-bits-per-channel stream)
        (* 4 8)))

(defun create-callback-description (callback data)
  (setf (harmony-coreaudio-cffi:au-render-callback-struct-input-proc callback)
        (cffi:callback buffer-render))
  (setf (harmony-coreaudio-cffi:au-render-callback-struct-input-proc-ref-con callback)
        data))

(cffi:defcallback start :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (when drain
      (cffi:with-foreign-objects ((description '(:struct harmony-coreaudio-cffi:audio-component-description))
                                  (stream '(:struct harmony-coreaudio-cffi:audio-stream-basic-description))
                                  (callback '(:struct harmony-coreaudio-cffi:au-render-callback-struct))
                                  (unit 'harmony-coreaudio-cffi:audio-unit))
        ;; Prepare needed information
        (create-component-description description)
        (create-stream-description stream (samplerate (context drain)) (channels drain))
        (create-callback-description callback (cl-mixed:handle drain))
        ;; Search for device
        (let ((component (harmony-coreaudio-cffi:audio-component-find-next (cffi:null-pointer) description)))
          (when (cffi:null-pointer-p component)
            (error "No component found."))
          (with-error ()
            (harmony-coreaudio-cffi:audio-component-instance-new component unit))
          (let ((unit (cffi:mem-ref unit :pointer)))
            ;; Set unit properties
            (with-error ()
              (harmony-coreaudio-cffi:audio-unit-set-property
               unit
               harmony-coreaudio-cffi:kAudioUnitProperty_SetRenderCallback
               harmony-coreaudio-cffi:kAudioUnitScope_Input
               0
               callback
               (cffi:foreign-type-size '(:struct harmony-coreaudio-cffi:au-render-callback-struct))))
            (with-error ()
              (harmony-coreaudio-cffi:audio-unit-set-property
               unit
               harmony-coreaudio-cffi:kAudioUnitProperty_StreamFormat
               harmony-coreaudio-cffi:kAudioUnitScope_Input
               0
               stream
               (cffi:foreign-type-size '(:struct harmony-coreaudio-cffi:audio-stream-basic-description))))
            ;; Fire it up!
            (with-error ()
              (harmony-coreaudio-cffi:audio-unit-initialize unit))
            (with-error ()
              (harmony-coreaudio-cffi:audio-output-unit-start unit))
            (setf (audio-unit drain) unit))))
      (if (audio-unit drain) 1 0))))

(cffi:defcallback end :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (when drain
      (let ((unit (audio-unit drain)))
        (when unit
          (harmony-coreaudio-cffi:audio-output-unit-stop unit)
          (harmony-coreaudio-cffi:audio-unit-uninitialize unit)
          (harmony-coreaudio-cffi:audio-component-instance-dispose unit)))
      1)))
