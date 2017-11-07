#|
This file is a part of harmony
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-wasapi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.wasapi)
  (:use #:cl #:harmony)
  (:export
   #:wasapi-error
   #:code
   #:wasapi-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.wasapi)

(defvar *com-initialized* NIL)

(define-condition wasapi-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "WASAPI error: ~a" (code c)))))

(defmacro with-error (() &body body)
  (let ((error (gensym "ERROR")))
    `(let ((,error (progn ,@body)))
       (unless (eql ,error :ok)
         (error 'wasapi-error :code ,error)))))

(defmacro with-deref ((var type) &body init)
  `(cffi:with-foreign-object (,var ,type)
     (with-error () ,@init)
     (cffi:mem-ref ,var ,type)))

(defmacro with-com-object (var init &body body)
  `(let ((,var (with-deref (,var :pointer) ,init)))
     (unwind-protect
          ,@body
       (harmony-wasapi-cffi:com-release ,var))))

(defun ensure-com-initialized ()
  (unless *com-initialized*
    (with-error ()
      (harmony-wasapi-cffi:co-initialize (cffi:null-pointer) :multi-threaded))
    (setf *com-initialized* T)))

(defun cleanup-com ()
  (when *com-initialized*
    (harmony-wasapi-cffi:co-uninitialize)))

(defun enumerate-devices ()
  (ensure-com-initialized)
  (with-com-object enumerator
      (harmony-wasapi-cffi:co-create-instance
       harmony-wasapi-cffi:CLSID-MMDEVICEENUMERATOR
       (cffi:null-pointer)
       harmony-wasapi-cffi:CLSCTX-ALL
       harmony-wasapi-cffi:IID-IMMDEVICEENUMERATOR
       enumerator)
    (with-com-object collection
        (harmony-wasapi-cffi:imm-device-enumerator-enum-audio-endpoints
         enumerator
         :render
         harmony-wasapi-cffi:DEVICE-STATE-ACTIVE
         collection)
      (loop for i from 0 below (with-deref (count :uint)
                                 (harmony-wasapi-cffi:imm-device-collection-get-count collection count))
            collect (with-com-object device
                        (harmony-wasapi-cffi:imm-device-collection-item
                         collection
                         i
                         device)
                      (let ((id (with-deref (id :pointer)
                                  (harmony-wasapi-cffi:imm-device-get-id device id))))
                        (unwind-protect
                             (harmony-wasapi-cffi:wstring->string id)
                          (harmony-wasapi-cffi:co-task-mem-free id))))))))

(defun find-audio-client (&optional id)
  (ensure-com-initialized)
  (with-com-object enumerator
      (harmony-wasapi-cffi:co-create-instance
       harmony-wasapi-cffi:CLSID-MMDEVICEENUMERATOR
       (cffi:null-pointer)
       harmony-wasapi-cffi:CLSCTX-ALL
       harmony-wasapi-cffi:IID-IMMDEVICEENUMERATOR
       enumerator)
    (with-com-object device
        (if id
            (let ((wstring (harmony-wasapi-cffi:string->wstring id)))
              (unwind-protect
                   (harmony-wasapi-cffi:imm-device-enumerator-get-device enumerator wstring device)
                (cffi:foreign-free wstring)))
            (harmony-wasapi-cffi:imm-device-enumerator-get-default-audio-endpoint
             enumerator :render :multimedia device))
      (with-deref (client :pointer)
        (harmony-wasapi-cffi:imm-device-activate
         device
         harmony-wasapi-cffi:IID-IAUDIOCLIENT
         harmony-wasapi-cffi:CLSCTX-ALL
         (cffi:null-pointer)
         client)))))

(defun format-supported-p (audio-client samplerate channels format &optional (mode :shared))
  (cffi:with-foreign-object (wave '(:struct harmony-wasapi-cffi:waveformat-extensible))
    (harmony-wasapi-cffi:encode-wave-format wave samplerate channels format)
    (cffi:with-foreign-object (closest :pointer)
      (let ((pass (harmony-wasapi-cffi:i-audio-client-is-format-supported
                   audio-client mode wave closest)))
        (let ((closest (cffi:mem-ref closest :pointer)))
          (unwind-protect
               (multiple-value-bind (samplerate channels sample-format)
                   (unless (cffi:null-pointer-p closest) (harmony-wasapi-cffi:decode-wave-format closest))
                 (values (eql :ok pass) samplerate channels sample-format))
            (harmony-wasapi-cffi:co-task-mem-free closest)))))))

(defun mix-format (audio-client)
  (with-deref (format :pointer)
    (harmony-wasapi-cffi:i-audio-client-get-mix-format audio-client format)))

(defun reference-time->seconds (reference-time)
  (* reference-time 100 (expt 10 -9)))

(defun seconds->reference-time (seconds)
  (ceiling (* seconds 1/100 (expt 10 9))))

(defun device-period (audio-client)
  (cffi:with-foreign-objects ((default 'harmony-wasapi-cffi:reference-time)
                              (minimum 'harmony-wasapi-cffi:reference-time))
    (with-error ()
      (harmony-wasapi-cffi:i-audio-client-get-device-period audio-client default minimum))
    (values (reference-time->seconds (cffi:mem-ref default 'harmony-wasapi-cffi:reference-time))
            (reference-time->seconds (cffi:mem-ref minimum 'harmony-wasapi-cffi:reference-time)))))

(defun audio-client-label (audio-client)
  (with-com-object session
      (harmony-wasapi-cffi:i-audio-client-get-service
       audio-client
       harmony-wasapi-cffi:IID-IAUDIOSESSIONCONTROL
       session)
    (cffi:with-foreign-object (label :pointer)
      (harmony-wasapi-cffi:i-audio-session-control-get-display-name session label)
      (harmony-wasapi-cffi:wstring->string (cffi:mem-ref label :pointer)))))

(defun (setf audio-client-label) (label audio-client)
  (with-com-object session
      (harmony-wasapi-cffi:i-audio-client-get-service
       audio-client
       harmony-wasapi-cffi:IID-IAUDIOSESSIONCONTROL
       session)
    (let ((label (harmony-wasapi-cffi:string->wstring label)))
      (harmony-wasapi-cffi:i-audio-session-control-set-display-name session label (cffi:null-pointer))
      (cffi:foreign-free label))
    label))

(defclass wasapi-drain (pack-drain)
  ((paused-p :initform NIL :accessor paused-p)
   (mode :initform :shared :accessor mode)
   (client :initform NIL :accessor client)
   (render :initform NIL :accessor render)
   (event :initform NIL :accessor event)
   (program-name :initform NIL :initarg :program-name :accessor program-name)
   (audio-client-id :initform NIL :initarg :audio-client-id :accessor audio-client-id))
  (:default-initargs
   :program-name "Harmony"))

(defmethod initialize-instance :after ((drain wasapi-drain) &key)
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-packed-audio ((drain wasapi-drain))
  ;; FIXME: allow picking a device
  ;; FIXME: allow picking shared/exclusive mode
  (setf (event drain) (harmony-wasapi-cffi:create-event (cffi:null-pointer) 0 0 (cffi:null-pointer)))
  (let ((client (find-audio-client (audio-client-id drain))))
    (unwind-protect
         (multiple-value-bind (ok samplerate channels sample-format)
             (format-supported-p client (samplerate (server drain)) 2 :float)
           (declare (ignore ok))
           (when (program-name drain)
             (setf (audio-client-label client) (program-name drain)))
           (setf (client drain) client)
           (cl-mixed:make-packed-audio
            NIL
            (* (buffersize (server drain))
               (cl-mixed:samplesize sample-format)
               channels)
            sample-format
            channels
            :alternating
            samplerate))
      (unless (client drain)
        (harmony-wasapi-cffi:com-release client)))))

(cffi:defcfun (memcpy "memcpy") :pointer
  (dest :pointer)
  (source :pointer)
  (num cl-mixed-cffi:size_t))

(defmethod process ((drain wasapi-drain) frames)
  (let* ((pack (cl-mixed:packed-audio drain))
         (source (cl-mixed:data pack))
         (server (server drain))
         (client (client drain))
         (render (render drain))
         (target (with-deref (target :pointer)
                   (harmony-wasapi-cffi:i-audio-render-client-get-buffer render frames target))))
    (memcpy target source (* frames
                             (cl-mixed:samplesize (cl-mixed:encoding pack))
                             (cl-mixed:channels pack)))
    (harmony-wasapi-cffi:i-audio-render-client-release-buffer render frames 0)
    (loop (harmony-wasapi-cffi:wait-for-single-object (event drain) harmony-wasapi-cffi:INFINITE)
          (setf frames (- (with-deref (frames :uint32)
                            (harmony-wasapi-cffi:i-audio-client-get-buffer-size client frames))
                          (with-deref (frames :uint32)
                            (harmony-wasapi-cffi:i-audio-client-get-current-padding client frames))))
          (when (< 0 frames)
            (setf (samples server) frames)
            (return)))))

(defmethod (setf paused-p) :before (value (drain wasapi-drain))
  (with-body-in-server-thread ((server drain))
    (with-error ()
      (if value
          (harmony-wasapi-cffi:i-audio-client-stop (client drain))
          (harmony-wasapi-cffi:i-audio-client-start (client drain))))))

(cffi:defcallback start :int ((segment :pointer))
  (let* ((drain (cl-mixed:pointer->object segment))
         (mode (mode drain))
         (client (client drain))
         (format (mix-format client))
         ;; Attempt to get a buffer as large as our internal ones.
         (buffer-duration (seconds->reference-time (/ (buffersize (server drain))
                                                      (samplerate (server drain))))))
    (unless (render drain)
      (with-error ()
        (harmony-wasapi-cffi:i-audio-client-initialize
         client
         mode
         harmony-wasapi-cffi:AUDCLNT-STREAMFLAGS-EVENTCALLBACK
         buffer-duration
         (ecase mode (:shared 0) (:exclusive buffer-duration))
         format
         (cffi:null-pointer)))
      (with-error ()
        (harmony-wasapi-cffi:i-audio-client-set-event-handle
         client
         (event drain)))
      (harmony-wasapi-cffi:co-task-mem-free format)
      (setf (render drain) (with-deref (render :pointer)
                             (harmony-wasapi-cffi:i-audio-client-get-service
                              client HARMONY-WASAPI-CFFI:IID-IAUDIORENDERCLIENT render)))
      (with-error ()
        (harmony-wasapi-cffi:i-audio-client-start client))
      ;; Force sample count to WASAPI desired amount.
      (setf (samples (server drain))
            (with-deref (frames :uint32)
              (harmony-wasapi-cffi:i-audio-client-get-buffer-size client frames))))))

(cffi:defcallback end :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (with-error ()
      (harmony-wasapi-cffi:i-audio-client-stop (client drain)))
    (when (render drain)
      (harmony-wasapi-cffi:i-audio-render-client-release (render drain))
      (setf (render drain) NIL)))
  1)
