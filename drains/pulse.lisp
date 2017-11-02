#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-pulse
  (:nicknames #:org.shirakumo.fraf.harmony.drains.pulse)
  (:use #:cl #:harmony)
  (:export
   #:pulse-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.pulse)

(define-condition pulse-error (error)
  ((code :initarg :code :accessor code)
   (paused-p :initform NIL :accessor paused-p)
   (program-name :initform NIL :initarg :program-name :accessor program-name))
  (:report (lambda (c s) (format s "Pulse error ~d: ~a"
                                 (code c) (harmony-pulse-cffi:strerror (code c))))))

(defmacro with-error ((errorvar) &body body)
  `(cffi:with-foreign-object (,errorvar :int)
     (when (< (progn ,@body) 0)
       (error 'pulse-error :code (cffi:mem-ref ,errorvar :int)))))

(defclass pulse-drain (pack-drain)
  ((simple :initform NIL :accessor simple)))

(defmethod initialize-instance :after ((drain pulse-drain) &key)
  (setf (cl-mixed-cffi:direct-segment-start (cl-mixed:handle drain)) (cffi:callback start))
  (setf (cl-mixed-cffi:direct-segment-end (cl-mixed:handle drain)) (cffi:callback end)))

(defmethod initialize-packed-audio ((drain pulse-drain))
  (cl-mixed:make-packed-audio
   NIL
   (* (buffersize (server drain))
      (cl-mixed:samplesize :float)
      2)
   :float
   2
   :alternating
   (samplerate (server drain))))

(defmethod process ((drain pulse-drain) samples)
  (let* ((pack (cl-mixed:packed-audio drain))
         (buffer (cl-mixed:data pack))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding pack))
                   (cl-mixed:channels pack))))
    (with-error (err)
      (harmony-pulse-cffi:simple-write (simple drain) buffer bytes err))))

(defmethod (setf paused-p) :before (value (drain pulse-drain))
  (when value
    (with-body-in-server-thread ((server drain))
      (with-error (err)
        (harmony-pulse-cffi:simple-drain (simple drain) err)))))

(cffi:defcallback start :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (unless (simple segment)
      (cffi:with-foreign-object (sample-spec '(:struct harmony-pulse-cffi:sample-spec))
        (setf (harmony-pulse-cffi:sample-spec-format sample-spec) :float32le)
        (setf (harmony-pulse-cffi:sample-spec-rate sample-spec) (samplerate (server drain)))
        (setf (harmony-pulse-cffi:sample-spec-channels sample-spec) 2)
        (with-error (error)
          (setf (simple drain) (harmony-pulse-cffi:simple-new
                                (cffi:null-pointer) (program-name drain)
                                :playback (cffi:null-pointer) (program-name drain)
                                sample-spec (cffi:null-pointer) (cffi:null-pointer)
                                error))
          (if (cffi:null-pointer-p (simple drain)) -1 1)))))
  1)

(cffi:defcallback end :int ((segment :pointer))
  (let ((drain (cl-mixed:pointer->object segment)))
    (when (simple drain)
      (with-error (err)
        (harmony-pulse-cffi:simple-drain (simple drain) err))
      (harmony-pulse-cffi:simple-free (simple drain))
      (setf (simple drain) NIL)))
  1)
