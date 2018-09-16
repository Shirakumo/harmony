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

(defmacro with-null-check (() &body body)
  (let ((result (gensym "RESULT")))
    `(let ((,result (progn ,@body)))
       (if (cffi:null-pointer-p ,result)
           (error 'wasapi-error :code (harmony-wasapi-cffi:get-last-error))
           ,result))))

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

(defun format-supported-p (audio-client samplerate channels sample-format &optional (mode :shared))
  (cffi:with-foreign-object (wave '(:struct harmony-wasapi-cffi:waveformat-extensible))
    (harmony-wasapi-cffi:encode-wave-format wave samplerate channels sample-format)
    (cffi:with-foreign-object (closest :pointer)
      (let ((pass (harmony-wasapi-cffi:i-audio-client-is-format-supported
                   audio-client mode wave closest)))
        (let ((closest (cffi:mem-ref closest :pointer)))
          (unwind-protect
               (multiple-value-bind (samplerate channels sample-format)
                   (cond ((and (eql :ok pass) (cffi:null-pointer-p closest))
                          (values samplerate channels sample-format))
                         ((not (cffi:null-pointer-p closest))
                          (harmony-wasapi-cffi:decode-wave-format closest)))
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
   (frames-to-write :initform 0 :accessor frames-to-write)
   (frame-buffer-size :initform 0 :accessor frame-buffer-size)
   (frame-buffer :initform (cffi:null-pointer) :accessor frame-buffer)
   (program-name :initform NIL :initarg :program-name :accessor program-name)
   (audio-client-id :initform NIL :initarg :audio-client-id :accessor audio-client-id))
  (:default-initargs
   :program-name "Harmony"))

(defmethod initialize-instance :after ((drain wasapi-drain) &key)
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-packed-audio ((drain wasapi-drain))
  (cl-mixed:make-packed-audio
   NIL
   (* (buffersize (context drain))
      (cl-mixed:samplesize :float)
      2)
   :float
   2
   :alternating
   44100))

(defmethod process ((drain wasapi-drain) processed-frames)
  (declare (optimize speed))
  (let* ((pack (cl-mixed:packed-audio drain))
         (source (cl-mixed:data pack))
         (render (render drain))
         (client (client drain))
         (frame-buffer-size (frame-buffer-size drain))
         (frames-to-write (frames-to-write drain))
         (frame-buffer (frame-buffer drain)))
    (declare (type (unsigned-byte 32) processed-frames frame-buffer-size frames-to-write))
    (declare (type cffi:foreign-pointer frame-buffer))
    (loop
       (cond ((<= frames-to-write 0)
              ;; No more frames to write, fetch a new buffer.
             ;;;; For whatever fucking reason I can't get the event waiting to work. It just
             ;;;; always returns immediately.
              ;; (harmony-wasapi-cffi:wait-for-single-object (event drain) harmony-wasapi-cffi:INFINITE)
              (harmony-wasapi-cffi:sleep (floor (* processed-frames (/ 1000 44100 2))))
              (setf frame-buffer-size
                    (min processed-frames
                         (- (with-deref (frames :uint32)
                              (harmony-wasapi-cffi:i-audio-client-get-buffer-size client frames))
                            (with-deref (frames :uint32)
                              (harmony-wasapi-cffi:i-audio-client-get-current-padding client frames)))))
              (setf frames-to-write frame-buffer-size)
              (setf frame-buffer
                    (with-deref (target :pointer)
                      (harmony-wasapi-cffi:i-audio-render-client-get-buffer render frames-to-write target))))
             ((= processed-frames frames-to-write)
              ;; We can fit our entire buffer into the target, so write.
              (memcpy frame-buffer source (* processed-frames
                                             (cl-mixed:samplesize (cl-mixed:encoding pack))
                                             (cl-mixed:channels pack)))
              (harmony-wasapi-cffi:i-audio-render-client-release-buffer render frame-buffer-size 0)
              (setf (frames-to-write drain) 0)
              (return))
             (T
              ;; We can't fit the entire buffer in there, write what we can for now.
              (memcpy frame-buffer source (* frames-to-write
                                             (cl-mixed:samplesize (cl-mixed:encoding pack))
                                             (cl-mixed:channels pack)))
              (harmony-wasapi-cffi:i-audio-render-client-release-buffer render frame-buffer-size 0)
              (decf processed-frames frames-to-write)
              (setf frames-to-write 0))))))

(defmethod (setf paused-p) :before (value (drain wasapi-drain))
  (with-body-in-mixing-context ((context drain))
    (with-error ()
      (if value
          (harmony-wasapi-cffi:i-audio-client-stop (client drain))
          (harmony-wasapi-cffi:i-audio-client-start (client drain))))))

(cffi:defcallback start :int ((segment :pointer))
  ;; FIXME: allow picking a device
  ;; FIXME: allow picking shared/exclusive mode
  (let* ((drain (cl-mixed:pointer->object segment))
         (mode (mode drain))
         ;; Attempt to get a buffer as large as our internal ones.
         (buffer-duration (seconds->reference-time (/ (buffersize (context drain))
                                                      (samplerate (context drain)))))
         (pack (packed-audio drain))
         (client (find-audio-client (audio-client-id drain)))
         (format (mix-format client)))
    (unwind-protect
         (multiple-value-bind (ok samplerate channels sample-format)
             (format-supported-p client (samplerate (context drain)) 2 :float)
           (declare (ignore ok))
           (setf (client drain) client)
           ;; Construct the audio pack in case we need to convert.
           ;; Usually WASAPI seems to want 44100, 2, float, for shared
           ;; so we should be fine. In case that's not always what we
           ;; get, and in case we ever support exclusive mode, let's
           ;; do it proper, though.
           (setf (cl-mixed:channels pack) channels)
           (setf (cl-mixed:samplerate pack) samplerate)
           (setf (cl-mixed:encoding pack) sample-format)
           (setf (cl-mixed:size pack) (* (buffersize (context drain))
                                         (cl-mixed:samplesize sample-format)
                                         channels))))
    ;; Initialise the rest
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
      (setf (event drain) (with-null-check ()
                            (harmony-wasapi-cffi:create-event (cffi:null-pointer) 0 0 (cffi:null-pointer))))
      (with-error ()
        (harmony-wasapi-cffi:i-audio-client-set-event-handle
         client
         (event drain)))
      (when (program-name drain)
        (setf (audio-client-label client) (program-name drain)))
      (harmony-wasapi-cffi:co-task-mem-free format)
      (setf (frame-buffer-size drain)
            (with-deref (frames :uint32)
              (harmony-wasapi-cffi:i-audio-client-get-buffer-size client frames)))
      (setf (render drain)
            (with-deref (render :pointer)
              (harmony-wasapi-cffi:i-audio-client-get-service
               client HARMONY-WASAPI-CFFI:IID-IAUDIORENDERCLIENT render)))
      (with-error ()
        (harmony-wasapi-cffi:i-audio-client-start client)))
    (if (render drain) 1 0)))

(cffi:defcallback end :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (when (event drain)
      (harmony-wasapi-cffi:close-handle (event drain)))
    (with-error ()
      (harmony-wasapi-cffi:i-audio-client-stop (client drain)))
    (when (render drain)
      (harmony-wasapi-cffi:i-audio-render-client-release (render drain))
      (setf (render drain) NIL))
    (when (client drain)
      (harmony-wasapi-cffi:i-audio-client-release (client drain))
      (setf (client drain) NIL)))
  1)
