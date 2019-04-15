#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-alsa
  (:nicknames #:org.shirakumo.fraf.harmony.drains.alsa)
  (:use #:cl #:harmony)
  (:shadow #:start #:end)
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
   (paused-p :initform NIL :accessor paused-p)))

(defmethod initialize-instance :after ((drain alsa-drain) &key)
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-packed-audio ((drain alsa-drain))
  (cl-mixed:make-packed-audio
   NIL
   (* (buffersize (context drain))
      (cl-mixed:samplesize :float)
      2)
   :float
   2
   :alternating
   (samplerate (context drain))))

(defmethod process ((drain alsa-drain) samples)
  (let* ((pack (cl-mixed:packed-audio drain))
         (buffer (cl-mixed:data pack)))
    (let ((error (harmony-alsa-cffi:pcm-writei (pcm drain) buffer samples)))
      (when (< error 0)
        (setf error (harmony-alsa-cffi:pcm-recover (pcm drain) error 0))
        (when (< error 0)
          (error 'alsa-error :code error))))))

(defmethod (setf paused-p) :before (value (drain alsa-drain))
  (with-body-in-mixing-context ((context drain))
    (with-error ()
      (harmony-alsa-cffi:pcm-pause (pcm drain) (if value 1 0)))))

(cffi:defcallback start :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (unless (pcm drain)
      (cffi:with-foreign-object (pcm :pointer)
        (with-error ()
          (harmony-alsa-cffi:pcm-open pcm "default" :playback 0))
        (let ((pcm (cffi:mem-ref pcm :pointer)))
          (with-error ()
            (harmony-alsa-cffi:pcm-set-params pcm :float-le :rw-interleaved 2
                                              (samplerate (context drain))
                                              1 10000)) ;; 1ms
          (setf (pcm drain) pcm))))
    (if (pcm drain) 1 0)))

(cffi:defcallback end :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (when (pcm drain)
      (with-error ()
        (harmony-alsa-cffi:pcm-drain (pcm drain)))
      (harmony-alsa-cffi:pcm-close (pcm drain))
      (setf (pcm drain) NIL)))
  1)
