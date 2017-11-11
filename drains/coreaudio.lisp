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

(defclass coreaudio-drain (drain cl-mixed:virtual)
  ((audio-unit :initform NIL :accessor audio-unit)
   (paused-p :initform NIL :accessor paused-p)
   (ring-buffers :initform NIL :accessor ring-buffers)
   (channels :initarg :channels :reader channels))
  (:default-initargs
   :channels 2))

(defmethod process ((drain coreaudio-drain) samples)
  (let ((ring (ring-buffers drain)))
    (loop for buffer across (cl-mixed:inputs drain)
          do (harmony-coreaudio-cffi:ring-buffer-write ring (cl-mixed:data buffer) (* samples 4))
             (cffi:incf-pointer ring (cffi:foreign-type-size '(:struct harmony-coreaudio-cffi:ring-buffer))))))

(defmethod cl-mixed:info ((drain coreaudio-drain))
  (list :min-inputs (channels drain)
        :max-inputs (channels drain)
        :outputs 0))

(defmethod (setf paused-p) :before (value (drain coreaudio-drain))
  (with-body-in-server-thread ((server drain))
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

(defun create-callback-description (callback ring-buffers)
  (setf (harmony-coreaudio-cffi:au-render-callback-struct-input-proc callback)
        (cffi:callback harmony-coreaudio-cffi:ring-buffer-render))
  (setf (harmony-coreaudio-cffi:au-render-callback-struct-input-proc-ref-con callback)
        ring-buffers))

(defmethod cl-mixed:start ((drain coreaudio-drain))
  (cffi:with-foreign-objects ((description '(:struct harmony-coreaudio-cffi:audio-component-description))
                              (stream '(:struct harmony-coreaudio-cffi:audio-stream-basic-description))
                              (callback '(:struct harmony-coreaudio-cffi:au-render-callback-struct))
                              (unit 'harmony-coreaudio-cffi:audio-unit))
    ;; Allocate our string buffers
    (let ((ring (cffi:foreign-alloc '(:struct harmony-coreaudio-cffi:ring-buffer)
                                    :count (channels drain))))
      (setf (ring-buffers drain) ring)
      (dotimes (i (channels drain))
        (harmony-coreaudio-cffi:make-ring-buffer (* 1024 4) ring)
        (cffi:incf-pointer ring (cffi:foreign-type-size '(:struct harmony-coreaudio-cffi:ring-buffer)))))
    ;; Prepare needed information
    (create-component-description description)
    (create-stream-description stream (samplerate (server drain)) (channels drain))
    (create-callback-description callback (ring-buffers drain))
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
  (if (audio-unit drain) 1 0))

(defmethod cl-mixed:end ((drain coreaudio-drain))
  (let ((unit (audio-unit drain))
        (ring (ring-buffers drain)))
    (when unit
      (harmony-coreaudio-cffi:audio-output-unit-stop unit)
      (harmony-coreaudio-cffi:audio-unit-uninitialize unit)
      (harmony-coreaudio-cffi:audio-component-instance-dispose unit))
    (when ring
      (dotimes (i (channels drain))
        (harmony-coreaudio-cffi:free-ring-buffer ring)
        (cffi:incf-pointer ring (cffi:foreign-type-size '(:struct harmony-coreaudio-cffi:ring-buffer))))
      (cffi:foreign-free (ring-buffers drain))))
  1)
