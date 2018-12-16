#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-wasapi-cffi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.wasapi.cffi)
  (:use #:cl #:cffi)
  (:shadow #:byte #:sleep)
  (:export
   #:ole32
   #:avrt
   #:AUDCLNT-STREAMFLAGS-EVENTCALLBACK
   #:DEVICE-STATE-ACTIVE
   #:WAVE-FORMAT-EXTENSIBLE
   #:CLSCTX-ALL
   #:INFINITE
   #:CP-UTF8
   #:word
   #:dword
   #:refiid
   #:refclsid
   #:lpunknown
   #:byte
   #:ulong
   #:wstring
   #:reference-time
   #:lpcguid
   #:handle
   #:lpdword
   #:wait-result
   #:sharedmode
   #:bufferflags
   #:hresult
   #:dataflow
   #:role
   #:channel-mask
   #:coinit
   #:guid
   #:guid-data1
   #:guid-data2
   #:guid-data3
   #:guid-data4
   #:waveformat-ex
   #:waveformat-ex-format-tag
   #:waveformat-ex-channels
   #:waveformat-ex-samples-per-sec
   #:waveformat-ex-avg-bytes-per-sec
   #:waveformat-ex-block-align
   #:waveformat-ex-bits-per-sample
   #:waveformat-ex-size
   #:waveformat-extensible
   #:waveformat-extensible-format
   #:waveformat-extensible-samples
   #:waveformat-extensible-channel-mask
   #:waveformat-extensible-sub-format
   #:property-key
   #:property-key-fmtid
   #:property-key-pid
   #:com
   #:vtbl
   #:imm-device-enumerator-
   #:imm-device-enumerator-query-interface
   #:imm-device-enumerator-add-ref
   #:imm-device-enumerator-release
   #:imm-device-enumerator-enum-audio-endpoints
   #:imm-device-enumerator-get-default-audio-endpoint
   #:imm-device-enumerator-get-device
   #:imm-device-enumerator-register-endpoint-notification-callback
   #:imm-device-enumerator-unregister-endpoint-notification-callback
   #:imm-device-collection
   #:imm-device-collection-query-interface
   #:imm-device-collection-add-ref
   #:imm-device-collection-release
   #:imm-device-collection-get-count
   #:imm-device-collection-item
   #:imm-device
   #:imm-device-query-interface
   #:imm-device-add-ref
   #:imm-device-release
   #:imm-device-activate
   #:imm-device-open-property-store
   #:imm-device-get-id
   #:imm-device-get-state
   #:i-audio-client
   #:i-audio-client-query-interface
   #:i-audio-client-add-ref
   #:i-audio-client-release
   #:i-audio-client-initialize
   #:i-audio-client-get-buffer-size
   #:i-audio-client-get-stream-latency
   #:i-audio-client-get-current-padding
   #:i-audio-client-is-format-supported
   #:i-audio-client-get-mix-format
   #:i-audio-client-get-device-period
   #:i-audio-client-start
   #:i-audio-client-stop
   #:i-audio-client-reset
   #:i-audio-client-set-event-handle
   #:i-audio-client-get-service
   #:i-audio-render-client
   #:i-audio-render-client-query-interface
   #:i-audio-render-client-add-ref
   #:i-audio-render-client-release
   #:i-audio-render-client-get-buffer
   #:i-audio-render-client-release-buffer
   #:i-property-store
   #:i-property-store-query-interface
   #:i-property-store-add-ref
   #:i-property-store-release
   #:i-property-store-commit
   #:i-property-store-get-at
   #:i-property-store-get-count
   #:i-property-store-get-value
   #:i-property-store-set-value
   #:i-audio-session-control
   #:i-audio-session-control-query-interface
   #:i-audio-session-control-add-ref
   #:i-audio-session-control-release
   #:i-audio-session-control-get-state
   #:i-audio-session-control-get-display-name
   #:i-audio-session-control-set-display-name
   #:i-audio-session-control-get-icon-path
   #:i-audio-session-control-set-icon-path
   #:i-audio-session-control-get-grouping-param
   #:i-audio-session-control-set-grouping-param
   #:i-audio-session-control-register-audio-session-notification
   #:i-audio-session-control-unregister-audio-session-notification
   #:co-initialize
   #:co-uninitialize
   #:co-create-instance
   #:co-task-mem-free
   #:av-set-mm-thread-characteristics
   #:av-revert-mm-thread-characteristics
   #:wait-for-single-object
   #:sleep
   #:get-last-error
   #:create-event
   #:close-handle
   #:set-event
   #:wide-char-to-multi-byte
   #:multi-byte-to-wide-char
   #:wstring->string
   #:string->wstring
   #:compose-channel-mask
   #:channel-mask-for-channel-count
   #:make-guid
   #:move-guid
   #:guid=
   #:IID-IAudioClient
   #:IID-IAudioRenderClient
   #:IID-IAudioSessionControl
   #:IID-IMMDeviceEnumerator
   #:CLSID-MMDeviceEnumerator
   #:KSDATAFORMAT-SUBTYPE-PCM
   #:KSDATAFORMAT-SUBTYPE-IEEE-FLOAT
   #:com-release
   #:encode-wave-format
   #:decode-wave-format))
(in-package #:org.shirakumo.fraf.harmony.drains.wasapi.cffi)

(define-foreign-library ole32
  (:windows "Ole32.dll"))

(define-foreign-library avrt
  (:windows "Avrt.dll"))

(use-foreign-library ole32)
(use-foreign-library avrt)

;; https://github.com/EddieRingle/portaudio/blob/master/src/hostapi/wasapi/mingw-include/audioclient.h
;; https://github.com/EddieRingle/portaudio/blob/master/src/hostapi/wasapi/mingw-include/mmdeviceapi.h

;; FIXME: better naming
(defconstant AUDCLNT-STREAMFLAGS-EVENTCALLBACK #x00040000)
(defconstant DEVICE-STATE-ACTIVE #x00000001)
(defconstant WAVE-FORMAT-EXTENSIBLE #x0000FFFE)
(defconstant CLSCTX-ALL 23)
(defconstant INFINITE (1- (expt 2 32)))
(defconstant CP-UTF8 65001)

(defctype word :uint16)
(defctype dword :uint32)
(defctype refiid :pointer)
(defctype refclsid :pointer)
(defctype lpunknown :pointer)
(defctype byte :uint8)
(defctype ulong :unsigned-long)
(defctype wstring :pointer)
(defctype reference-time :long-long)
(defctype lpcguid :pointer)
(defctype handle :pointer)
(defctype lpdword :pointer)

(defcenum (wait-result dword)
  (:abandoned #x00000080)
  (:object-0  #x00000000)
  (:timeout   #x00000102)
  (:failed    #xFFFFFFFF))

(defcenum sharemode
  :shared
  :exclusive)

(defcenum bufferflags
  (:data-discontinuity 1)
  (:silent 2)
  (:timestamp-error 4))

(defcenum hresult
  (:already-initialized 2290679810)
  (:bad-pointer 2147500035)
  (:bufduration-period-not-equal 2290679827)
  (:buffer-error 2290679832)
  (:buffer-operation-pending 2290679819)
  (:buffer-size-error 2290679830)
  (:buffer-size-not-aligned 2290679833)
  (:buffer-too-large 2290679814)
  (:cpuusage-exceeded 2290679831)
  (:device-in-use 2290679818)
  (:device-invalidated 2290679812)
  (:endpoint-create-failed 2290679823)
  (:exclusive-mode-not-allowed 2290679822)
  (:false 1)
  (:invalid-arg 2147942487)
  (:invalid-device-period 2290679840)
  (:invalid-size 2290679817)
  (:nointerface 2147500034)
  (:not-initialized 2290679809)
  (:ok 0)
  (:out-of-memory 2147942414)
  (:out-of-order 2290679815)
  (:service-not-running 2290679824)
  (:unsupported-format 2290679816)
  (:wrong-endpoint-type 2290679811)
  (:class-not-registered 2147746132)
  (:no-aggregation 2147746064)
  (:no-interface 2147500034)
  (:pointer 2147500035))

(defcenum dataflow
  :render
  :capture
  :all
  :dataflow-enum-count)

(defcenum role
  :console
  :multimedia
  :communications
  :role-enum-count)

(defcenum channel-mask
  (:front-left             #x1)
  (:front-right            #x2)
  (:front-center           #x4)
  (:low-frequency          #x8)
  (:back-left              #x10)
  (:back-right             #x20)
  (:front-left-of-center   #x40)
  (:front-right-of-center  #x80)
  (:back-center            #x100)
  (:side-left              #x200)
  (:side-right             #x400)
  (:top-center             #x800)
  (:top-front-left         #x1000)
  (:top-front-center       #x2000)
  (:top-front-right        #x4000)
  (:top-back-left          #x8000)
  (:top-back-center        #x10000)
  (:top-back-right         #x20000)
  (:reserved               #x80000000))

(defcenum coinit
  (:apartment-threaded #x2)
  (:multi-threaded #x0)
  (:disable-ole1dde #x4)
  (:speed-over-memory #x8))

(defmacro defcomfun ((struct method &rest options) return-type &body args)
  (let* ((*print-case* (readtable-case *readtable*))
         (structg (gensym "STRUCT"))
         (name (intern (format NIL "~a-~a" struct method))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (,structg ,@(mapcar #'first args))
         (foreign-funcall-pointer
          (,(intern (format NIL "%~a" name))
           (vtbl ,structg))
          ,options
          :pointer ,structg
          ,@(loop for (name type) in args
                  collect type collect name)
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

(defcstruct (guid :conc-name guid-)
  (data1 dword)
  (data2 word)
  (data3 word)
  (data4 byte :count 8))

(defcstruct (waveformat-ex :conc-name waveformat-ex-)
  (format-tag word)
  (channels word)
  (samples-per-sec dword)
  (avg-bytes-per-sec dword)
  (block-align word)
  (bits-per-sample word)
  (size word))

(defcstruct (waveformat-extensible :conc-name waveformat-extensible-)
  (format (:struct waveformat-ex))
  (samples word :offset 18)
  (channel-mask dword)
  (sub-format (:struct guid)))

(defcstruct (property-key :conc-name property-key)
  (fmtid (:struct guid))
  (pid dword))

(defcstruct (com :conc-name ||)
  (vtbl :pointer))

(defcomstruct imm-device-enumerator
    (enum-audio-endpoints hresult
                          (data-flow dataflow)
                          (state-mask dword)
                          (devices :pointer))

  (get-default-audio-endpoint hresult
                              (data-flow dataflow)
                              (role role)
                              (endpoint :pointer))

  (get-device hresult
              (pwstrid wstring)
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

(defcomstruct imm-device
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

(defcomstruct i-audio-client
    (initialize hresult
                (share-mode sharemode)
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
                       (share-mode sharemode)
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
                  (flags bufferflags)))

(defcomstruct i-property-store
    (commit hresult)
  
  (get-at hresult
          (prop dword)
          (pkey :pointer))

  (get-count hresult
             (props :pointer))

  (get-value hresult
             (key :pointer)
             (value :pointer))

  (set-value hresult
             (key :pointer)
             (value :pointer)))

(defcomstruct i-audio-session-control
    (get-state hresult
               (retval :pointer))

  (get-display-name hresult
                    (retval :pointer))

  (set-display-name hresult
                    (value :pointer)
                    (event-context :pointer))

  (get-icon-path hresult
                 (retval :pointer))

  (set-icon-path hresult
                 (value :pointer)
                 (event-context :pointer))

  (get-grouping-param hresult
                      (retval :pointer))

  (set-grouping-param hresult
                      (value :pointer)
                      (event-context :pointer))

  (register-audio-session-notification hresult
                                       (new-notifications :pointer))
  
  (unregister-audio-session-notification hresult
                                         (new-notifications :pointer)))

(defcfun (co-initialize "CoInitializeEx") hresult
  (nullable :pointer)
  (init coinit))

(defcfun (co-uninitialize "CoUninitialize") :void)

(defcfun (co-create-instance "CoCreateInstance") hresult
  (rclsid refclsid)
  (punkouter lpunknown)
  (dwclscontext dword)
  (riid refiid)
  (ppv :pointer))

(defcfun (co-task-mem-free "CoTaskMemFree") :void
  (pointer :pointer))

(defcfun (av-set-mm-thread-characteristics "AvSetMmThreadCharacteristicsW") handle
  (task-name wstring)
  (task-index lpdword))

(defcfun (av-revert-mm-thread-characteristics "AvRevertMmThreadCharacteristics") :bool
  (handle handle))

(defcfun (wait-for-single-object "WaitForSingleObject") wait-result
  (handle handle)
  (milliseconds dword))

(defcfun (sleep "Sleep") :void
  (miliseconds dword))

(defcfun (get-last-error "GetLastError") dword)

(defcfun (create-event "CreateEventW") handle
  (event-attribute :pointer)
  (manual-reset :bool)
  (initial-state :bool)
  (name wstring))

(defcfun (close-handle "CloseHandle") :bool
  (object handle))

(defcfun (set-event "SetEvent") :bool
  (event handle))

(defcfun (wide-char-to-multi-byte "WideCharToMultiByte") :int
  (code-page :uint)
  (flags dword)
  (wide-char-str :pointer)
  (wide-char :int)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (default-char :pointer)
  (used-default-char :pointer))

(defcfun (multi-byte-to-wide-char "MultiByteToWideChar") :int
  (code-page :uint)
  (flags dword)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (wide-char-str :pointer)
  (wide-char :int))

(defun wstring->string (pointer)
  (let ((bytes (wide-char-to-multi-byte CP-UTF8 0 pointer -1 (null-pointer) 0 (null-pointer) (null-pointer))))
    (with-foreign-object (string :uchar bytes)
      (wide-char-to-multi-byte CP-UTF8 0 pointer -1 string bytes (null-pointer) (null-pointer))
      (foreign-string-to-lisp string :encoding :utf-8))))

(defun string->wstring (string)
  (with-foreign-string (string string)
    (let* ((chars (multi-byte-to-wide-char CP-UTF8 0 string -1 (null-pointer) 0))
           (pointer (foreign-alloc :uint16 :count chars)))
      (multi-byte-to-wide-char CP-UTF8 0 string -1 pointer chars)
      pointer)))

(defun compose-channel-mask (&rest channels)
  (let ((i 0))
    (dolist (channel channels i)
      (setf i (logior i (foreign-enum-value 'channel-mask channel))))))

(defun channel-mask-for-channel-count (channels)
  (case channels
    (1 (compose-channel-mask :front-center))
    (2 (compose-channel-mask :front-left :front-right))
    (3 (compose-channel-mask :front-left :front-center :front-right))
    (4 (compose-channel-mask :front-left :front-right :back-left :back-right))
    (5 (compose-channel-mask :front-left :front-right :back-left :back-right :low-frequency))
    (6 (compose-channel-mask :front-left :front-center :front-right :back-left :back-right :low-frequency))
    (T (compose-channel-mask))))

(defun make-guid (d1 d2 d3 &rest d4)
  (let ((ptr (foreign-alloc '(:struct guid))))
    (setf (guid-data1 ptr) d1)
    (setf (guid-data2 ptr) d2)
    (setf (guid-data3 ptr) d3)
    (loop for i from 0 below 8
          for d in d4
          do (setf (mem-aref (foreign-slot-pointer ptr '(:struct guid) 'data4) 'byte i)
                   d))
    ptr))

(defun move-guid (from to)
  (setf (guid-data1 to) (guid-data1 from))
  (setf (guid-data2 to) (guid-data2 from))
  (setf (guid-data3 to) (guid-data3 from))
  (loop for i from 0 below 8
        do (setf (mem-aref (foreign-slot-pointer to '(:struct guid) 'data4) 'byte i)
                 (mem-aref (foreign-slot-pointer from '(:struct guid) 'data4) 'byte i)))
  to)

(defun guid= (a b)
  (and
   (= (guid-data1 a) (guid-data1 b))
   (= (guid-data2 a) (guid-data2 b))
   (= (guid-data3 a) (guid-data3 b))
   (loop for i from 0 below 8
         always (= (mem-aref (foreign-slot-pointer a '(:struct guid) 'data4) 'byte i)
                   (mem-aref (foreign-slot-pointer b '(:struct guid) 'data4) 'byte i)))))

(defvar IID-IAudioClient
  (make-guid #x1CB9AD4C #xDBFA #x4c32 #xB1 #x78 #xC2 #xF5 #x68 #xA7 #x03 #xB2))
(defvar IID-IAudioRenderClient
  (make-guid #xF294ACFC #x3146 #x4483 #xA7 #xBF #xAD #xDC #xA7 #xC2 #x60 #xE2))
(defvar IID-IAudioSessionControl
  (make-guid #xf4b1a599 #x7266 #x4319 #xa8 #xca #xe7 #x0a #xcb #x11 #xe8 #xcd))
(defvar IID-IMMDeviceEnumerator
  (make-guid #xA95664D2 #x9614 #x4F35 #xA7 #x46 #xDE #x8D #xB6 #x36 #x17 #xE6))
(defvar CLSID-MMDeviceEnumerator
  (make-guid #xBCDE0395 #xE52F #x467C #x8E #x3D #xC4 #x57 #x92 #x91 #x69 #x2E))
(defvar KSDATAFORMAT-SUBTYPE-PCM
  (make-guid #x00000001 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))
(defvar KSDATAFORMAT-SUBTYPE-IEEE-FLOAT
  (make-guid #x00000003 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))

(defun com-release (pointer)
  (foreign-funcall-pointer
   (mem-aref (vtbl pointer) :pointer 2)
   ()
   :pointer pointer
   ulong))

(defun encode-wave-format (ptr samplerate channels format)
  (let ((bit-depth (ecase format
                     ((:double :int64) 64)
                     ((:float :int32) 32)
                     ((:int24) 24)
                     ((:int16) 16)
                     ((:uint8) 8)))
        (format (case format
                  ((:double :float) KSDATAFORMAT-SUBTYPE-IEEE-FLOAT)
                  (T KSDATAFORMAT-SUBTYPE-PCM))))
    ;; Clear the data.
    (loop for i from 0 below (foreign-type-size '(:struct waveformat-extensible))
          do (setf (mem-ref ptr :uchar i) 0))
    ;; The EX struct is at the beginning, so we can reuse the pointer.
    (setf (waveformat-ex-format-tag ptr) WAVE-FORMAT-EXTENSIBLE)
    (setf (waveformat-ex-size ptr) 22)
    (setf (waveformat-ex-channels ptr) channels)
    (setf (waveformat-ex-samples-per-sec ptr) samplerate)
    (setf (waveformat-ex-bits-per-sample ptr) (if (= 24 bit-depth) 32 bit-depth))
    (setf (waveformat-ex-block-align ptr) (/ (* channels (waveformat-ex-bits-per-sample ptr)) 8))
    (setf (waveformat-ex-avg-bytes-per-sec ptr) (* samplerate (waveformat-ex-block-align ptr)))
    (setf (waveformat-extensible-samples ptr) bit-depth)
    (setf (waveformat-extensible-channel-mask ptr) (channel-mask-for-channel-count channels))
    (move-guid format (foreign-slot-pointer ptr '(:struct waveformat-extensible) 'sub-format)))
  ptr)

(defun decode-wave-format (ptr)
  (values (waveformat-ex-samples-per-sec ptr)
          (waveformat-ex-channels ptr) 
          (if (guid= (foreign-slot-pointer ptr '(:struct waveformat-extensible) 'sub-format)
                     KSDATAFORMAT-SUBTYPE-IEEE-FLOAT)
              (case (waveformat-extensible-samples ptr)
                (64 :double)
                (32 :float)
                (T :WTF))
              (case (waveformat-extensible-samples ptr)
                (64 :int64)
                (32 :int32)
                (24 :int24)
                (16 :int16)
                (8 :int8)
                (T :WTF)))))
