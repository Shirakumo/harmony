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
(defconstant kAudioUnitProperty_SetRenderCallback 23)
(defconstant kAudioUnitScope_Input 1)
(defconstant kAudioUnitType_Output "auou")
(defconstant kAudioUnitSubType_DefaultOutput "def ")
(defconstant kAudioUnitManufacturer_Apple "appl")
(defconstant no-err 0)

;; Types
(define-foreign-type os-type () ()
  (:actual-type :pointer))

(define-parse-method os-type ()
  (make-instance 'os-type))

(defmethod translate-to-foreign (string (type os-type))
  (foreign-string-alloc string :encoding :ascii :null-terminated-p NIL :end 4))

(defmethod translate-from-foreign (pointer (type os-type))
  (foreign-string-to-lisp pointer :encoding :ascii :count 4))

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
  (let ((buffers (foreign-slot-pointer io-data '(:struct audio-buffer-list) 'buffers)))
    (dotimes (i frames)
      (let ((sample (sin (* 2 PI 440 +phase+ 1/44100))))
        (dotimes (c (audio-buffer-list-number-buffers io-data))
          (let* ((buffer (mem-aref buffers '(:struct audio-buffer) c))
                 (data (audio-buffer-data buffer)))
            (setf (cffi:mem-aref data :float i) sample))))
      (setf +phase+ (mod (1+ +phase+) 44100))))
  no-err)

(defun test ()
  (cffi:with-foreign-objects ((description '(:struct audio-component-description))
                              (input '(:struct au-render-callback-struct))
                              (unit 'audio-unit))
    (setf (audio-component-description-component-type description) kAudioUnitType_Output)
    (setf (audio-component-description-component-sub-type description) kAudioUnitSubType_DefaultOutput)
    (setf (audio-component-description-component-manufacturer description) kAudioUnitManufacturer_Apple)
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
        (audio-unit-initialize unit)
        (audio-output-unit-start unit)
        (unwind-protect (loop (sleep 0.1))
          (audio-output-unit-stop unit)
          (audio-unit-uninitialize unit)
          (audio-component-instance-dispose unit))))))
