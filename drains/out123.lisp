#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)
(defpackage #:org.shirakumo.fraf.harmony.drains.out123
  (:nicknames #:harmony-out123)
  (:use #:cl #:harmony)
  (:export
   #:out123-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.out123)

(defclass out123-drain (drain)
  ((device :initform NIL :accessor device)))

(defmethod initialize-instance :after ((drain out123-drain) &key)
  (setf (decoder drain) #'decode))

(defmethod initialize-channel ((drain out123-drain))
  (let ((out (cl-out123:make-output NIL :name (or name (cl-out123:device-default-name
                                                        "Harmony")))))
    (setf (device server) out)
    (cl-out123:connect out)
    ;; FIXME: probe device and figure out the best channel configuration for it.    
    (multiple-value-bind (rate channels encoding) (cl-out123:playback-format out)
      (cl-mixed:make-channel NIL
                             (* (buffersize (server source))
                                (cl-mixed:samplesize encoding))
                             encoding
                             channels
                             :alternating
                             rate))))

(defun decode (samples drain)
  (let* ((channel (channel drain))
         (buffer (cl-mixed:data channel))
         (bytes (* samples (cl-mixed:samplesize (cl-mixed-cffi:channel-encoding channel)))))
    (cl-out123:play-directly (device drain) buffer bytes)))

(defmethod paused-p ((drain out123-drain))
  (cl-out123:playing (device drain)))

(defmethod (setf paused-p) (value (drain out123-drain))
  (with-server-lock ((server drain))
    (if value
        (cl-out123:pause (device drain))
        (cl-out123:resume (device drain)))))

(defmethod pause ((drain out123-drain))
  (with-server-lock ((server drain))
    (cl-out123:pause (device drain))))

(defmethod resume ((drain out123-drain))
  (with-server-lock ((server drain))
    (cl-out123:resume (device drain))))

(cffi:defcallback start :int ((segment :pointer))
  (cl-out123:start (device (cl-mixed:pointer->object segment)))
  1)

(cffi:defcallback end :int ((segment :pointer))
  (cl-out123:stop (device (cl-mixed:pointer->object segment)))
  1)
