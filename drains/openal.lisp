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
  (let* ((device (alc:open-device))
         (context (alc:create-context device))
         (source (al:gen-source))
         (buffer (al:gen-buffer)))
    (print (al:get-error))
    (alc:make-context-current context)
    (al:listener :position #(0 0 1))
    (al:listener :velocity #(0 0 0))
    (al:listener :orientation #(0 0 1 0 1 0))
    (al:source source :pitch 1)
    (al:source source :gain 1)
    (al:source source :position #(0 0 0))
    (al:source source :velocity #(0 0 0))
    (al:source source :looping NIL)
    (setf (device drain) device)
    (setf (context drain) context)
    (setf (source drain) source)
    (setf (buffer drain) buffer)
    (cl-mixed:make-packed-audio
     NIL
     (* (buffersize (server drain))
        (cl-mixed:samplesize :signed-16)
        2)
     :signed-16
     2
     :alternating
     (samplerate (server drain)))))

(defmethod process ((drain openal-drain) samples)
  (let* ((packed-audio (cl-mixed:packed-audio drain))
         (source (source drain))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding packed-audio))
                   (cl-mixed:channels packed-audio))))
    ;; Wait until the buffer is done.
    (loop while (= 0 (al:get-source source :buffers-processed))
          do (sleep 0.00001))
    (let ((buffer (first (al:source-unqueue-buffers source))))
      ;; Fill it up and queue it again.
      (al:buffer-data buffer :stereo16 (cl-mixed:data packed-audio) bytes (cl-mixed:samplerate packed-audio))
      (al:source-queue-buffers source (list buffer)))))

(defmethod (setf paused-p) :after (value (drain openal-drain))
  (with-body-in-server-thread ((server drain))
    (if value
        (al:source-pause (source drain))
        (al:source-play (source drain)))))

(defmethod pause ((drain openal-drain))
  (setf (paused-p drain) T))

(defmethod resume ((drain openal-drain))
  (setf (paused-p drain) NIL))

(cffi:defcallback start :int ((segment :pointer))
  (let ((segment (cl-mixed:pointer->object segment)))
    (alc:make-context-current (context segment))
    (al:buffer-data (buffer segment) :stereo16 (cffi:null-pointer) 0 (cl-mixed:samplerate (packed-audio segment)))
    (al:source-queue-buffers (source segment) (list (buffer segment)))
    (al:source-play (source segment)))
  1)

(cffi:defcallback end :int ((segment :pointer))
  (al:source-stop (source (cl-mixed:pointer->object segment)))
  1)
