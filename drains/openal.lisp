#|
This file is a part of harmony
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-openal
  (:nicknames #:org.shirakumo.fraf.harmony.drains.openal)
  (:use #:cl #:harmony)
  (:export
   #:openal-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.openal)

(al:load-libraries)
(alc:load-libraries)

(defmacro with-cleanup-on-failure (cleanup &body body)
  (let ((pass (gensym "PASS")))
    `(let ((,pass NIL))
       (unwind-protect
            (prog1 ,@body
              (setf ,pass T))
         (unless ,pass
           ,cleanup)))))

(defclass openal-drain (pack-drain)
  ((context :initform NIL :accessor context)
   (device :initform NIL :accessor device)
   (source :initform NIL :accessor source)
   (buffer :initform NIL :accessor buffer)
   (paused-p :initform NIL :accessor paused-p)))

(defmethod initialize-instance :after ((drain openal-drain) &key)
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-packed-audio ((drain openal-drain))
  (let ((device (or (alc:open-device)
                    (error "Failed to open device."))))
    (with-cleanup-on-failure (alc:close-device device)
      (let ((context (or (alc:create-context device)
                         (error "Failed to create context."))))
        (with-cleanup-on-failure (alc:destroy-context context)
          (unless (alc:make-context-current context)
            (error "Failed to make context current."))
          (unless (al:extension-present-p "AL_EXT_FLOAT32")
            (error "Required FLOAT32 extension is not present."))
          (setf (device drain) device)
          (setf (context drain) context))))
    (cl-mixed:make-packed-audio
     NIL
     (* (buffersize (context drain))
        (cl-mixed:samplesize :float)
        2)
     :float
     2
     :alternating
     (samplerate (context drain)))))

(defmethod process ((drain openal-drain) samples)
  (let* ((packed-audio (cl-mixed:packed-audio drain))
         (source (source drain))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding packed-audio))
                   (cl-mixed:channels packed-audio))))
    ;; Wait until the buffer is done.
    (loop while (= 0 (al:get-source source :buffers-processed))
          do (sleep 0.0001))
    (let ((buffer (first (al:source-unqueue-buffers source))))
      ;; Fill it up and queue it again.
      (al:buffer-data buffer #x10011 (cl-mixed:data packed-audio) bytes (cl-mixed:samplerate packed-audio))
      (al:source-queue-buffers source (list buffer)))))

(defmethod (setf paused-p) :after (value (drain openal-drain))
  (with-body-in-mixing-context ((context drain))
    (if value
        (al:source-pause (source drain))
        (al:source-play (source drain)))))

(defmethod pause ((drain openal-drain))
  (setf (paused-p drain) T))

(defmethod resume ((drain openal-drain))
  (setf (paused-p drain) NIL))

(cffi:defcallback start :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (alc:make-context-current (context drain))
    (let ((source (al:gen-source))
          (buffer (al:gen-buffer)))
      (al:listener :position #(0 0 1))
      (al:listener :velocity #(0 0 0))
      (al:listener :orientation #(0 0 1 0 1 0))
      (al:source source :pitch 1)
      (al:source source :gain 1)
      (al:source source :position #(0 0 0))
      (al:source source :velocity #(0 0 0))
      (al:source source :looping NIL)
      (setf (source drain) source)
      (setf (buffer drain) buffer)
      
      (al:buffer-data (buffer drain) #x10011 (cffi:null-pointer) 0 (cl-mixed:samplerate (packed-audio drain)))
      (al:source-queue-buffers (source drain) (list (buffer drain)))
      (al:source-play (source drain))))
  1)

(cffi:defcallback end :int ((segment :pointer))
  (al:source-stop (source (cl-mixed:pointer->object segment)))
  1)
