#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-alsa
  (:nicknames #:org.shirakumo.fraf.harmony.drains.alsa)
  (:use #:cl #:harmony)
  (:export
   #:alsa-error
   #:code
   #:alsa-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.alsa)

(define-condition alsa-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "ALSA error ~d: ~a"
                                 (code c) (harmony-alsa-cffi:strerror (code c))))))

(defmacro with-error (() &body body)
  (let ((error (gensym "ERROR")))
    `(let ((,error (progn ,@body)))
       (when (< ,error 0)
         (error 'alsa-error :code ,error)))))

(defclass alsa-drain (pack-drain)
  ((pcm :initform NIL :accessor pcm)
   (paused-p :initform NIL :accessor paused-p)
   (program-name :initform NIL :initarg :program-name :accessor program-name))
  (:default-initargs
   :program-name "Harmony"))

(defmethod initialize-instance :after ((drain alsa-drain) &key)
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-packed-audio ((drain alsa-drain))
  (cl-mixed:make-packed-audio
   NIL
   (* (buffersize (server drain))
      (cl-mixed:samplesize :float)
      2)
   :float
   2
   :alternating
   (samplerate (server drain))))

(defmethod process ((drain alsa-drain) samples)
  (let* ((pack (cl-mixed:packed-audio drain))
         (buffer (cl-mixed:data pack))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding pack))
                   (cl-mixed:channels pack))))
    (with-error ()
      (let ((error (harmony-alsa-cffi:pcm-writei (pcm drain) buffer bytes)))
        (when (< error 0)
          (setf error (harmony-alsa-cffi:pcm-recover (pcm drain) error 0)))
        error))))

(defmethod (setf paused-p) :before (value (drain alsa-drain))
  (with-body-in-server-thread ((server drain))
    (with-error ()
      (harmony-alsa-cffi:pcm-pause (pcm drain) (if value 1 0)))))

(cffi:defcallback start :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (unless (pcm drain)
      (cffi:with-foreign-object (pcm :pointer)
        (with-error ()
          (harmony-alsa-cffi:pcm-open pcm (program-name drain) :playback 0))
        (let ((pcm (cffi:mem-ref pcm :pointer)))
          (with-error ()
            (harmony-alsa-cffi:pcm-set-params pcm :float :rw-interleaved 2
                                              (samplerate (server drain))
                                              1 10000)))))
    (if (pcm drain) 1 0)))

(cffi:defcallback end :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (when (pcm drain)
      (harmony-alsa-cffi:pcm-close (pcm drain))
      (setf (pcm drain) NIL)))
  1)
