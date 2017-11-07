#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-wasapi-cffi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.wasapi.cffi)
  (:use #:cl #:cffi)
  (:shadow #:byte)
  (:export
   #:ole32
   #:audclnt-sharemode
   #:audclnt-bufferflags
   #:hresult
   #:dataflow
   #:role
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
   #:waveformat-extensible-dw-channel-mask
   #:waveformat-extensible-sub-format
   #:com
   #:vtbl
   #:imm-device-enumerator
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
   #:co-initialize
   #:co-uninitialize
   #:co-create-instance
   #:av-set-mm-thread-characteristics
   #:av-revert-mm-thread-characteristics
   #:wait-for-single-object
   #:create-event
   #:close-handle
   #:set-event))
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
(defconstant AUDCLNT_STREAMFLAGS_EVENTCALLBACK #x00040000)
(defconstant DEVICE_STATE_ACTIVE #x00000001)
(defconstant WAVE_FORMAT_EXTENSIBLE #x0000FFFE)
(defconstant CLSCTX-ALL 23)
(defconstant INFINITE (1- (expt 2 32)))
(defconstant CP_UTF8 65001)

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

(defcenum audclnt-sharemode
  :shared
  :exclusive)

(defcenum audclnt-bufferflags
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
  (:wrong-endpoint-type 2290679811))

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

(defmacro defcomfun ((struct method &rest options) return-type &body args)
  (let ((structg (gensym "STRUCT"))
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

(defun com-release (pointer)
  (foreign-funcall-pointer
   (mem-aref (vtbl pointer) :pointer 2)
   ()
   :pointer pointer
   ulong))

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
  (samples word)
  (channel-mask word)
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
    (flags audclnt-bufferflags)))

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

(defcfun (co-initialize "CoInitialize") hresult
  (nullable :pointer))

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

(defcfun (wait-for-single-object "WaitForSingleObject") dword
  (handle handle)
  (milliseconds dword))

;; undef?
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
  (let ((bytes (wide-char-to-multi-byte CP_UTF8 0 pointer -1 (null-pointer) 0 (null-pointer) (null-pointer))))
    (with-foreign-object (string :uchar bytes)
      (wide-char-to-multi-byte CP_UTF8 0 pointer -1 string bytes (null-pointer) (null-pointer))
      (foreign-string-to-lisp string :encoding :utf-8))))

(defun string->wstring (string)
  (with-foreign-string (string string)
    (let* ((chars (multi-byte-to-wide-char CP_UTF8 0 string -1 (null-pointer) 0))
           (pointer (foreign-alloc :uint16 :count chars)))
      (multi-byte-to-wide-char CP_UTF8 0 string -1 pointer chars)
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

(defvar IID_IAudioClient
  (make-guid #x1CB9AD4C #xDBFA #x4c32 #xB1 #x78 #xC2 #xF5 #x68 #xA7 #x03 #xB2))
(defvar IID_IAudioRenderClient
  (make-guid #xF294ACFC #x3146 #x4483 #xA7 #xBF #xAD #xDC #xA7 #xC2 #x60 #xE2))
(defvar IID_IAudioSessionControl
  (make-guid #xf4b1a599 #x7266 #x4319 #xa8 #xca #xe7 #x0a #xcb #x11 #xe8 #xcd))
(defvar IID_IMMDeviceEnumerator
  (make-guid #xA95664D2 #x9614 #x4F35 #xA7 #x46 #xDE #x8D #xB6 #x36 #x17 #xE6))
(defvar CLSID_MMDeviceEnumerator
  (make-guid #xBCDE0395 #xE52F #x467C #x8E #x3D #xC4 #x57 #x92 #x91 #x69 #x2E))
(defvar KSDATAFORMAT_SUBTYPE_PCM
  (make-guid #x00000001 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))
(defvar KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
  (make-guid #x00000003 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))

(defun fill-wave-format (ptr samplerate channels bit-depth &optional (format KSDATAFORMAT_SUBTYPE_PCM))
  ;; Clear the data.
  (loop for i from 0 below (foreign-type-size '(:struct waveformat-extensible))
        do (setf (mem-ref ptr :uchar i) 0))
  ;; The EX struct is at the beginning, so we can reuse the pointer.
  (setf (waveformat-ex-format-tag ptr) WAVE_FORMAT_EXTENSIBLE)
  (setf (waveformat-ex-size ptr) 22)
  (setf (waveformat-ex-channels ptr) channels)
  (setf (waveformat-ex-samples-per-sec ptr) samplerate)
  (setf (waveformat-ex-bits-per-sample ptr) bit-depth)
  (setf (waveformat-ex-block-align ptr) (* channels 1/8 bit-depth))
  (when (= 24 bit-depth)
    (setf (waveformat-ex-bits-per-sample ptr) 32)
    (setf (waveformat-ex-block-align ptr) (* channels 32/8)))
  (setf (waveformat-ex-avg-bytes-per-sec ptr) (* samplerate (waveformat-ex-block-align ptr)))
  (setf (waveformat-extensible-samples ptr) bit-depth)
  (setf (waveformat-extensible-channel-mask ptr) (channel-mask-for-channel-count channels))
  (move-guid format (foreign-slot-pointer ptr '(:struct waveformat-extensible) 'sub-format))
  ptr)

(defmacro with-error (&body body)
  (let ((err (gensym "ERR")))
    `(let ((,err (progn ,@body)))
       (unless (eql :ok ,err)
         (error "Call failed, return code was ~s." ,err)))))

(defvar *com-initialized* NIL)

(defun ensure-com-initialized ()
  (unless *com-initialized*
    (with-error (co-initialize (null-pointer)))
    (setf *com-initialized* T)))

(defun cleanup-com ()
  (when *com-initialized*
    (co-uninitialize)))

(defmacro with-deref ((var type) &body init)
  `(with-foreign-object (,var ,type)
     (with-error ,@init)
     (mem-ref ,var ,type)))

(defmacro with-com-object (var init &body body)
  `(let ((,var (with-deref (,var :pointer) ,init)))
     (unwind-protect
          ,@body
       (com-release ,var))))

(defun enumerate-devices ()
  (ensure-com-initialized)
  (with-com-object enumerator
      (co-create-instance CLSID_MMDEVICEENUMERATOR
                          (null-pointer)
                          CLSCTX-ALL
                          IID_IMMDEVICEENUMERATOR
                          enumerator)
    (with-com-object collection
        (imm-device-enumerator-enum-audio-endpoints enumerator
                                                    :render
                                                    DEVICE_STATE_ACTIVE
                                                    collection)
      (loop for i from 0 below (with-deref (count :uint)
                                 (imm-device-collection-get-count collection count))
            collect (with-com-object device
                        (imm-device-collection-item collection
                                                    i
                                                    device)
                      (let ((id (with-deref (id :pointer)
                                  (imm-device-get-id device id))))
                        (unwind-protect
                             (wstring->string id)
                          (co-task-mem-free id))))))))

(defun get-audio-client (&optional id)
  (ensure-com-initialized)
  (with-com-object enumerator
      (co-create-instance CLSID_MMDEVICEENUMERATOR
                          (null-pointer)
                          CLSCTX-ALL
                          IID_IMMDEVICEENUMERATOR
                          enumerator)
    (with-com-object device
        (if id
            (let ((wstring (string->wstring id)))
              (unwind-protect
                   (imm-device-enumerator-get-device enumerator wstring device)
                (foreign-free wstring)))
            (imm-device-enumerator-get-default-audio-endpoint enumerator :render :multimedia device))
      (with-deref (client :pointer)
        (imm-device-activate device IID_IAUDIOCLIENT CLSCTX-ALL (null-pointer) client)))))

(defun is-format-supported (audio-client samplerate channels bit-depth)
  (with-foreign-object (wave '(:struct waveformat-extensible))
    (fill-wave-format wave samplerate channels bit-depth)
    (with-foreign-object (closest :pointer)
      (unwind-protect
           (values (eql :ok (i-audio-client-is-format-supported audio-client :shared wave closest))
                   (mem-ref (mem-ref closest :pointer) '(:struct waveformat-extensible)))
        (co-task-mem-free (mem-ref closest :pointer))))))

(defun get-mix-format (audio-client)
  (with-deref (format :pointer)
    (i-audio-client-get-mix-format audio-client format)))

(defun get-device-period (audio-client)
  (with-foreign-objects ((default 'reference-time)
                         (minimum 'reference-time))
    (with-error (i-audio-client-get-device-period audio-client default minimum))
    (values (cffi:mem-ref default 'reference-time)
            (cffi:mem-ref minimum 'reference-time))))

(defun reference-time->seconds (reference-time)
  (* reference-time 100 (expt 10 -9)))

(defun seconds->reference-time (seconds)
  (* seconds 1/100 (expt 10 9)))

(defun initialize-audio-client (audio-client event-handle &key format)
  (let ((format (or format (get-mix-format audio-client))))
    (with-error (i-audio-client-initialize audio-client
                                           :shared
                                           AUDCLNT_STREAMFLAGS_EVENTCALLBACK
                                           0
                                           0
                                           format
                                           (null-pointer)))
    (with-error (i-audio-client-set-event-handle audio-client event-handle))
    (values audio-client format)))

(defun set-audio-client-label (audio-client label)
  (with-com-object session
      (i-audio-client-get-service audio-client IID_IAUDIOSESSIONCONTROL session)
    (let ((label (string->wstring label)))
      (i-audio-session-control-set-display-name session label (null-pointer))
      (foreign-free label))
    audio-client))

(defun call-with-audio-started (audio-client function)
  (with-error (i-audio-client-start audio-client))
  (unwind-protect
       (funcall function)
    (with-error (i-audio-client-stop audio-client))))

(defmacro with-audio-started (audio-client &body body)
  `(call-with-audio-started ,audio-client (lambda () ,@body)))

(defmacro with-render-client ((buffer frames) audio-client &body body)
  (let ((client (gensym "CLIENT"))
        (render (gensym "RENDER"))
        (bodyg (gensym "BODY"))
        (buffer-frames (gensym "BUFFER-FRAMES"))
        (padding-frames (gensym "PADDING-FRAMES")))
    `(let ((,client ,audio-client))
       (with-com-object ,render
           (i-audio-client-get-service ,client IID_IAUDIORENDERCLIENT ,render)
         (with-foreign-objects ((,buffer :pointer)
                                (,padding-frames :uint32))
           (let ((,buffer-frames (with-deref (,buffer-frames :uint32)
                                   (i-audio-client-get-buffer-size ,audio-client ,buffer-frames))))
             (macrolet ((with-buffer-active (() &body ,bodyg)
                          `(call-with-buffer-active ,',client ,',render ,',buffer ,',buffer-frames ,',padding-frames
                                                    (lambda (,',buffer ,',frames)
                                                      ,@,bodyg))))
               ,@body)))))))

(defun call-with-buffer-active (client render buffer buffer-frames padding-frames function)
  (declare (type foreign-pointer client render buffer padding-frames))
  (declare (type (unsigned-byte 32) buffer-frames))
  (declare (type function function))
  (declare (optimize speed))
  (with-error (i-audio-client-get-current-padding client padding-frames))
  (let ((frames (- buffer-frames (mem-ref padding-frames :uint32))))
    (with-error (i-audio-render-client-get-buffer render frames buffer))
    (let ((buffer (mem-ref buffer :pointer))
          (pass NIL))
      (unwind-protect
           (progn (funcall function buffer frames)
                  (setf pass T))
        (if pass
            (with-error (i-audio-render-client-release-buffer render frames 0))
            (with-error (i-audio-render-client-release-buffer render 0 :silent)))))))


(defun fill-with-zeroes (buffer frames)
  (dotimes (i (* 2 frames))
    (setf (cffi:mem-aref buffer :float i) 0.0s0)))

(sb-ext:defglobal *phase* 0)
(defun fill-with-sine (buffer frames)
  (declare (optimize speed (safety 0)))
  (declare (type (unsigned-byte 32) frames *phase*))
  (declare (type cffi:foreign-pointer buffer))
  (dotimes (i frames)
    (let ((sample (coerce (* 2 PI 440 *phase* 1/44100) 'single-float)))
      (setf (cffi:mem-aref buffer :float i) sample)
      (setf (cffi:mem-aref buffer :float (1+ i)) sample)
      (setf *phase* (mod (1+ *phase*) 44100)))))

(defun test (client event)
  (declare (optimize speed))
  (with-render-client (buffer frames) client
    (with-audio-started client
      (loop (with-buffer-active ()
              (fill-with-sine buffer frames))
            (wait-for-single-object event INFINITE)))))
