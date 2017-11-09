#|
This file is a part of harmony
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-coreaudio-cffi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.coreaudio.cffi)
  (:use #:cl #:cffi)
  (:export
   ))
(in-package #:org.shirakumo.fraf.harmony.drains.coreaudio.cffi)

;; https://github.com/rweichler/coreaudio-examples/blob/master/CH07_AUGraphSineWave/main.c
(define-foreign-library audio-unit
    (:darwin (:framework "AudioUnit")))

(define-foreign-library audio-toolbox
    (:darwin (:framework "AudioToolbox")))

(use-foreign-library audio-unit)
(use-foreign-library audio-toolbox)

;; Constants
(alexandria:define-constant kAudioUnitType_Output "auou" :test 'equal)
(alexandria:define-constant kAudioUnitSubType_DefaultOutput "def " :test 'equal)
(alexandria:define-constant kAudioUnitManufacturer_Apple "appl" :test 'equal)
(alexandria:define-constant kAudioFormatLinearPCM "lpcm" :test 'equal)
(defconstant kAudioUnitProperty_StreamFormat 8)
(defconstant kAudioUnitProperty_SetRenderCallback 23)
(defconstant kAudioUnitScope_Input 1)
(defconstant kAudioFormatFlagsNativeEndian 0)
(defconstant kAudioFormatFlagIsFloat #x1)
(defconstant kAudioFormatFlagIsPacked #x8)
(defconstant kAudioFormatFlagsNativeFloatPacked (logior kAudioFormatFlagsNativeEndian
                                                        kAudioFormatFlagIsFloat
                                                        kAudioFormatFlagIsPacked))
(defconstant no-err 0)

;; Types
(define-foreign-type os-type () ()
  (:actual-type :int32))

(define-parse-method os-type ()
  (make-instance 'os-type))

(defmethod translate-to-foreign (string (type os-type))
  (let ((int 0))
    (dotimes (i 4 int)
      (setf (ldb (byte 8 (* (- 3 i) 8)) int) (char-code (aref string i))))))

(defmethod translate-from-foreign (integer (type os-type))
  (let ((string (make-string 4)))
    (dotimes (i 4 string)
      (setf (aref string i) (code-char (ldb (byte 8 (* (- 3 i) 8)) integer))))))

(defmethod free-translated-object (pointer (type os-type) param)
  (declare (ignore param))
  (foreign-string-free pointer))

(defctype os-status :int32)
(defctype audio-component :pointer)
(defctype component-instance :pointer)
(defctype component-result :int32)
(defctype audio-component-instance :pointer)
(defctype audio-unit component-instance)
(defctype audio-unit-property-id :uint32)
(defctype audio-unit-scope :uint32)
(defctype audio-unit-element :uint32)
(defctype audio-format-id os-type)
(defctype audio-format-flags :uint32)

;; Enums
(defcenum render-action-flags
    (:pre-render #.(ash 1 2))
  (:post-render #.(ash 1 3))
  (:output-is-silence #.(ash 1 4))
  (:preflight #.(ash 1 5))
  (:render #.(ash 1 6))
  (:complete #.(ash 1 7))
  (:post-render-error #.(ash 1 8))
  (:do-not-check-render-args #.(ash 1 9)))

;; Structs
(defcstruct (component-instance-record :conc-name component-instance-record-)
    (data :long :count 1))

(defcstruct (audio-component-description :conc-name audio-component-description-)
    (component-type os-type)
  (component-sub-type os-type)
  (component-manufacturer os-type)
  (component-flags :uint32)
  (component-flags-mask :uint32))

(defcstruct (audio-stream-basic-description :conc-name audio-stream-basic-description-)
    (sample-rate :double)
  (format-id audio-format-id)
  (format-flags audio-format-flags)
  (bytes-per-packet :uint32)
  (frames-per-packet :uint32)
  (bytes-per-frame :uint32)
  (channels-per-frame :uint32)
  (bits-per-channel :uint32)
  (reserved :uint32))

(defcstruct (au-render-callback-struct :conc-name au-render-callback-struct-)
    (input-proc :pointer)
  (input-proc-ref-con :pointer))

(defcstruct (smpte-time :conc-name smpte-time-)
    (subframes :int16)
  (subframe-divisor :int16)
  (counter :uint32)
  (type :uint32)
  (flags :uint32)
  (hours :int16)
  (minutes :int16)
  (seconds :int16)
  (frames :int16))

(defcstruct (audio-time-stamp :conc-name audio-time-stamp-)
    (sample-time :double)
  (host-time :uint64)
  (rate-scalar :double)
  (word-clock-time :uint64)
  (smpte-time (:struct smpte-time))
  (flags :uint32)
  (reserved :uint32))

(defcstruct (audio-buffer :conc-name audio-buffer-)
    (number-channels :uint32)
  (data-byte-size :uint32)
  (data :pointer))

(defcstruct (audio-buffer-list :conc-name audio-buffer-list-)
    (number-buffers :uint32)
  (buffers (:struct audio-buffer) :count 1))

;; Funcs
(defcfun (audio-component-find-next "AudioComponentFindNext") audio-component
  (component audio-component)
  (description :pointer))

(defcfun (audio-component-instance-new "AudioComponentInstanceNew") os-status
  (component audio-component)
  (output :pointer))

(defcfun (audio-component-instance-dispose "AudioComponentInstanceDispose") os-status
  (component audio-component-instance))

(defcfun (audio-unit-set-property "AudioUnitSetProperty") component-result
  (unit audio-unit)
  (property audio-unit-property-id)
  (scope audio-unit-scope)
  (element audio-unit-element)
  (data :pointer)
  (size :uint32))

(defcfun (audio-unit-initialize "AudioUnitInitialize") component-result
  (unit audio-unit))

(defcfun (audio-unit-uninitialize "AudioUnitUninitialize") component-result
  (unit audio-unit))

(defcfun (audio-output-unit-start "AudioOutputUnitStart") os-status
  (unit audio-unit))

(defcfun (audio-output-unit-stop "AudioOutputUnitStop") os-status
  (unit audio-unit))

(sb-ext:defglobal +phase+ 0)
(defcallback sine-wave-render os-status ((ref-con :pointer)
                                         (action-flags :pointer)
                                         (time-stamp :pointer)
                                         (bus-number :uint32)
                                         (frames :uint32)
                                         (io-data :pointer))
  (declare (ignore ref-con action-flags time-stamp bus-number))
  (let ((sb-sys:*interrupts-enabled* NIL)
        (sb-kernel:*gc-inhibit* T))
    (let ((buffers (foreign-slot-pointer io-data '(:struct audio-buffer-list) 'buffers)))
      (dotimes (i frames)
        (let ((sample (sin (coerce (* 2 PI 440 +phase+ 1/44100) 'single-float))))
          (dotimes (c (audio-buffer-list-number-buffers io-data))
            (let* ((buffer (inc-pointer buffers (* c (foreign-type-size '(:struct audio-buffer)))))
                   (data (audio-buffer-data buffer)))
              (setf (mem-aref data :float i) sample))))
        (setf +phase+ (mod (1+ +phase+) 44100)))))
  no-err)

(defun test ()
  (with-foreign-objects ((description '(:struct audio-component-description))
                         (stream '(:struct audio-stream-basic-description))
                         (input '(:struct au-render-callback-struct))
                         (unit 'audio-unit))
    ;; This is always the same. Why we need this at all, I don't know. #justapplethings
    (setf (audio-component-description-component-type description) kAudioUnitType_Output)
    (setf (audio-component-description-component-sub-type description) kAudioUnitSubType_DefaultOutput)
    (setf (audio-component-description-component-manufacturer description) kAudioUnitManufacturer_Apple)
    (setf (audio-component-description-component-flags description) 0)
    (setf (audio-component-description-component-flags-mask description) 0)
    ;; Make sure we get the format we need.
    (setf (audio-stream-basic-description-sample-rate stream) 44100.0d0)
    (setf (audio-stream-basic-description-format-id stream) kAudioFormatLinearPCM)
    (setf (audio-stream-basic-description-format-flags stream) kAudioFormatFlagsNativeFloatPacked)
    (setf (audio-stream-basic-description-bytes-per-packet stream) 4)
    (setf (audio-stream-basic-description-frames-per-packet stream) 1)
    (setf (audio-stream-basic-description-bytes-per-frame stream) 8)
    (setf (audio-stream-basic-description-channels-per-frame stream) 2)
    (setf (audio-stream-basic-description-bits-per-channel stream) (* 4 8))
    ;; For some reason setting a callback needs an intermediary struct.
    (setf (au-render-callback-struct-input-proc input) (callback sine-wave-render))
    (setf (au-render-callback-struct-input-proc-ref-con input) (null-pointer))
    
    (let ((component (audio-component-find-next (null-pointer) description)))
      (when (null-pointer-p component)
        (error "No component found."))
      (audio-component-instance-new component unit)
      (let ((unit (mem-ref unit :pointer)))
        (audio-unit-set-property unit
                                 kAudioUnitProperty_SetRenderCallback
                                 kAudioUnitScope_Input
                                 0
                                 input
                                 (foreign-type-size '(:struct au-render-callback-struct)))
        (audio-unit-set-property unit
                                 kAudioUnitProperty_StreamFormat
                                 kAudioUnitScope_Input
                                 0
                                 stream
                                 (foreign-type-size '(:struct audio-stream-basic-description)))
        (audio-unit-initialize unit)
        (audio-output-unit-start unit)
        (unwind-protect (loop (sleep 0.1))
          (audio-output-unit-stop unit)
          (audio-unit-uninitialize unit)
          (audio-component-instance-dispose unit))))))

;; Ring buffer impl
(defcstruct (ring-buffer :conc-name ring-buffer-)
    (data :pointer)
  (size :uint32)
  (read-start :uint32)
  (write-start :uint32))

(defun make-ring-buffer (size)
  (let ((ring (foreign-alloc '(:struct ring-buffer))))
    (setf (ring-buffer-data ring) (foreign-alloc :uint8 :count size))
    (setf (ring-buffer-size ring) size)
    (setf (ring-buffer-read-start ring) 0)
    (setf (ring-buffer-write-start ring) 0)
    ring))

(defun free-ring-buffer (ring)
  (foreign-free (ring-buffer-data ring))
  (foreign-free ring)
  T)

(defcfun (memcpy "memcpy") :pointer
  (dest :pointer)
  (source :pointer)
  (num :uint64))

(defun ring-buffer-write (ring data bytes)
  (declare (type foreign-pointer ring data)
           (type (unsigned-byte 32) bytes))
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((buffer (ring-buffer-data ring))
        (size (ring-buffer-size ring))
        (read-start (ring-buffer-read-start ring))
        (write-start (ring-buffer-write-start ring)))
    (declare (type foreign-pointer buffer)
             (type (integer 1 #.(expt 2 32)) size)
             (type (unsigned-byte 32) read-start write-start))
    (loop for free of-type (unsigned-byte 32) = (mod (- read-start write-start 1) size)
          do (print free)
             (cond ((< write-start read-start)
                    (memcpy (inc-pointer buffer write-start) data (min free bytes)))
                   (T
                    (let ((to-end (- size write-start)))
                      (memcpy (inc-pointer buffer write-start) data to-end)
                      (memcpy buffer (inc-pointer data to-end) (- (min free bytes) to-end)))))
             (setf write-start (setf (ring-buffer-write-start ring)
                                     (print (mod (1- read-start) size))))
             (cond ((<= bytes free)
                    (return))
                   (T ;; Wait for more data to write to be available
                    (decf bytes free)
                    (loop until (= (the (unsigned-byte 32) (ring-buffer-read-start ring))
                                   (the (unsigned-byte 32) (ring-buffer-write-start ring)))
                          do (sleep 0.0001))
                    (setf read-start write-start))))))

(defun ring-buffer-read (ring data bytes)
  (declare (type foreign-pointer ring data)
           (type (unsigned-byte 32) bytes))
  (declare (optimize speed (safety 1) (debug 0)))
  (let ((buffer (ring-buffer-data ring))
        (size (ring-buffer-size ring))
        (read-start (ring-buffer-read-start ring))
        (write-start (ring-buffer-write-start ring)))
    (declare (type foreign-pointer buffer)
             (type (integer 1 #.(expt 2 32)) size)
             (type (unsigned-byte 32) read-start write-start))
    (loop for free of-type (unsigned-byte 32) = (mod (- write-start read-start) size)
          do (cond ((< read-start write-start)
                    (memcpy (inc-pointer buffer read-start) data (min free bytes)))
                   (T
                    (let ((to-end (- size read-start)))
                      (memcpy (inc-pointer buffer read-start) data to-end)
                      (memcpy buffer (inc-pointer data to-end) (- (min free bytes) to-end)))))
             (setf read-start (setf (ring-buffer-read-start ring)
                                    (mod write-start size)))
             (cond ((<= bytes free)
                    (return))
                   (T ;; Wait for more data to read to be available
                    (decf bytes free)
                    (loop until (/= (the (unsigned-byte 32) (ring-buffer-read-start ring))
                                    (the (unsigned-byte 32) (ring-buffer-write-start ring)))
                          do (sleep 0.0001))
                    (setf write-start read-start))))))
