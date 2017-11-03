#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-wasapi-cffi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.wasapi.cffi)
  (:use #:cl #:cffi)
  (:export
   ))
(in-package #:org.shirakumo.fraf.harmony.drains.wasapi.cffi)

(define-foreign-library ole32
  (:windows "Ole32.dll"))

(use-foreign-library ole32)

(defmacro defcomfun ((struct method &rest options) return-type &body args)
  (let ((structg (gensym "STRUCT"))
        (name (intern (format NIL "~a-~a" struct method))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (,structg ,@(mapcar #'first args))
         (cffi:foreign-funcall-pointer
          (,(intern (format NIL "%~a" name))
           (vtbl ,structg))
          ,options
          :pointer ,structg
          ,@(mapcar (lambda (a) (reverse a)) args)
          ,return-type)))))

(defmacro defcomstruct (name &body methods)
  (let ((methods (list* `(query-interface hresult)
                        `(add-ref ulong)
                        `(release ulong)
                        methods)))
    `(progn
       (defcstruct (,name :conc-name ,(format NIL "%~a-" name))
         ,@(loop for method in methods
                 collect (list (first method) :pointer)))

       ,@(loop for (method return . args) in methods
               collect `(defcomfun (,name ,method) ,return
                          ,@args)))))

(trivial-indent:define-indentation defcomstruct (4 &rest (&whole 2 4 &rest 2)))

;; https://github.com/EddieRingle/portaudio/blob/master/src/hostapi/wasapi/mingw-include/audioclient.h
;; https://github.com/EddieRingle/portaudio/blob/master/src/hostapi/wasapi/mingw-include/mmdeviceapi.h

(define-constant IID_IAudioClient #(#x1CB9AD4C #xDBFA #x4c32 #xB1 #x78 #xC2 #xF5 #x68 #xA7 #x03 #xB2))
(define-constant IID_IAudioRenderClient #(#xF294ACFC #x3146 #x4483 #xA7 #xBF #xAD #xDC #xA7 #xC2 #x60 #xE2))
(define-constant CLSID_MMDeviceEnumerator #(#xBCDE0395 #xE52F #x467C #x8E #x3D #xC4 #x57 #x92 #x91 #x69 #x2E))
(define-constant IID_IMMDeviceEnumerator #(#xA95664D2 #x9614 #x4F35 #xA7 #x46 #xDE #x8D #xB6 #x36 #x17 #xE6))
(define-constant AUDCLNT_STREAMFLAGS_EVENTCALLBACK #x00040000)
(define-constant DEVICE_STATE_ACTIVE #x00000001)
(define-constant WAVE_FORMAT_EXTENSIBLE #x0000FFFE)
(define-constant CLSCTX-ALL 23)
(define-constant INFINITE -1)

(defctype hresult :uint32)
(defctype word :uint16)
(defctype dword :uint32)
(defctype refiid :pointer)
(defctype refclsid :pointer)
(defctype lpunknown :pointer)

(defcenum audclnt-sharemode
  :shared
  :exclusive)

(defcenum audclnt-bufferflags
  (:data-discontinuity 1)
  (:silent 2)
  (:timestamp-error 4))

(defcenum result
  (:ok 0)
  (:false 1)
  (:already-initialized 2290679810)
  (:bufduration-period-not-equal 2290679827)
  (:buffer-size-error 2290679830)
  (:buffer-size-not-aligned 2290679833)
  (:cpuusage-exceeded 2290679831)
  (:device-invalidated 2290679812)
  (:device-in-use 2290679818)
  (:endpoint-create-failed 2290679823)
  (:exclusive-mode-not-allowed 2290679822)
  (:invalid-device-period 2290679840)
  (:service-not-running 2290679824)
  (:unsupported-format 2290679816)
  (:wrong-endpoint-type 2290679811)
  (:invalid-arg 2147942487)
  (:out-of-memory 2147942414)
  (:bad-pointer 2147500035))

(defcstruct (guid)
  (data1 dword)
  (data2 word)
  (data3 word)
  (data4 byte :count 8))

(defcstruct (waveformat-ex)
  (format-tag word)
  (channels word)
  (samples-per-sec dword)
  (avg-bytes-per-sec dword)
  (block-align word)
  (bits-per-sample word)
  (size word))

(defcstruct (waveformat-extensible)
  (format waveformat-ex)
  (samples word)
  (dw-channel-mask word)
  (sub-format guid))

(defcstruct (com :conc-name ||)
  (vtbl :pointer))

(defcomstruct imm-device-enumerator
  (enum-audio-endpoints hresult
    (data-flow edataflow)
    (state-mask dword)
    (devices :pointer))

  (get-default-audio-endpoint hresult
    (data-flow edataflow)
    (role erole)
    (endpoint :pointer))

  (get-device hresult
    (pwstrid lpcwstr)
    (device :pointer))

  (register-endpoint-notification-callback hresult
    (client :pointer))

  (unregister-endpoint-notification-callback hresult
    (client :pointer)))

(defcomstruct imm-device-collection
  (get-count hresult
    (devices :pointer))
  
  (item hresult
    (device-id :uint)
    (device :pointer)))

(defcomstruct (imm-device %imm-device-)
  (activate hresult
    (id refiid)
    (cls-ctx dword)
    (activation-params :pointer)
    (interface :pointer))
  
  (open-property-store hresult
    (access dword)
    (properties :pointer))
  
  (get-id hresult
    (str-id :pointer))
  
  (get-state hresult
    (state :pointer)))

(defcomstruct (i-audio-client :conc-name %i-audio-client-)
  (initialize hresult
    (share-mode audclnt-sharemode)
    (stream-flags dword)
    (buffer-duration reference-time)
    (preiodicity reference-time)
    (format :pointer)
    (audio-session-guid lpcguid))
  
  (get-buffer-size hresult
    (num-buffer-frames :pointer))
  
  (get-stream-latency hresult
    (latency :pointer))
  
  (get-current-padding hresult
    (num-padding-frames :pointer))
  
  (is-format-supported hresult
    (share-mode audclnt-sharemode)
    (format :pointer)
    (closest-match :pointer))
  
  (get-mix-format hresult
    (device-format :pointer))
  
  (get-device-period hresult
    (default-device-period :pointer)
    (minimum-device-period :pointer))
  
  (start hresult)
  
  (stop hresult)
  
  (reset hresult)
  
  (set-event-handle hresult
    (event-handle handle))
  
  (get-service hresult
    (riid refiid)
    (service :pointer)))

(defcomstruct i-audio-render-client
  (get-buffer hresult
    (num-frames-requested :uint32)
    (data :pointer))
  
  (release-buffer hresult
    (num-frames-written :uint32)
    (flags dword)))

(defcfun (co-initialize "CoInitialize") hresult
  (nullable :pointer))

(defcfun (co-uninintialize "CoUninitialize") :void)

(defcfun (co-create-instance "CoCreateInstance") hresult
  (rclsid refclsid)
  (punkouter lpunknown)
  (dwclscontext dword)
  (riid refiid)
  (ppv :pointer))

(defcfun (av-set-mm-thread-characteristics "AvSetMmThreadCharacteristics") handle
  (task-name lpctstr)
  (task-index lpdword))

(defcfun (av-revert-mm-thread-characteristics "AvRevertMmThreadCharacteristics") :bool
  (handle handle))

(defcfun (wait-for-single-object "WaitForSingleObject") dword
  (handle handle)
  (milliseconds dword))

(defcfun (create-event "CreateEvent") handle
  (event-attribute lpsecurity-attributes)
  (manual-reset :bool)
  (initial-state :bool)
  (name :string))

(defcfun (close-handle "CloseHandle") :bool
  (object handle))

(defcfun (set-event "SetEvent") :bool
  (event handle))
