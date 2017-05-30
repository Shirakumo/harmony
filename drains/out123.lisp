#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-out123
  (:nicknames #:org.shirakumo.fraf.harmony.drains.out123)
  (:use #:cl #:harmony)
  (:export
   #:out123-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.out123)

(defclass out123-drain (drain)
  ((name :initform NIL :initarg :name :accessor name)
   (device :initform NIL :accessor device)))

(defmethod initialize-instance :after ((drain out123-drain) &key)
  (setf (decoder drain) #'decode)
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-channel ((drain out123-drain))
  (let ((out (cl-out123:make-output NIL :name (or (name drain)
                                                  (cl-out123:device-default-name "Harmony")))))
    (cl-out123:connect out)
    (cl-out123:start out :rate (samplerate (server drain))
                         :channels 2)
    (setf (device drain) out)
    (multiple-value-bind (rate channels encoding) (cl-out123:playback-format out)
      (cl-out123:stop out)
      (cl-mixed:make-channel NIL
                             (* (buffersize (server drain))
                                (cl-mixed:samplesize encoding)
                                channels)
                             encoding
                             channels
                             :alternating
                             rate))))

(defun decode (samples drain)
  (let* ((channel (cl-mixed:channel drain))
         (buffer (cl-mixed:data channel))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding channel))
                   (cl-mixed:channels channel))))
    (cl-out123:play-directly (device drain) buffer bytes)))

(defmethod paused-p ((drain out123-drain))
  (not (cl-out123:playing (device drain))))

(defmethod (setf paused-p) (value (drain out123-drain))
  (with-body-in-server-thread ((server drain))
    (if value
        (cl-out123:pause (device drain))
        (cl-out123:resume (device drain)))))

(defmethod pause ((drain out123-drain))
  (with-body-in-server-thread ((server drain))
    (cl-out123:pause (device drain))))

(defmethod resume ((drain out123-drain))
  (with-body-in-server-thread ((server drain))
    (cl-out123:resume (device drain))))

(cffi:defcallback start :int ((segment :pointer))
  (cl-out123:start (device (cl-mixed:pointer->object segment)))
  1)

(cffi:defcallback end :int ((segment :pointer))
  (cl-out123:stop (device (cl-mixed:pointer->object segment)))
  1)
