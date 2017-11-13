#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass mixing-context ()
  ((samples :initarg :samples :initform 0 :accessor samples)
   (buffersize :initarg :buffersize :initform 0 :accessor buffersize)
   (samplerate :initarg :samplerate :initform 0 :reader samplerate))
  (:default-initargs
   :buffersize 441
   :samplerate 44100))

(defmethod initialize-instance :after ((context mixing-context) &key samples buffersize samplerate)
  (check-type samples (or null (integer 1)))
  (check-type buffersize (or null (integer 1)))
  (check-type samplerate (integer 1))
  (unless buffersize
    (setf (slot-value context 'buffersize) (/ samplerate 100)))
  (if samples
      (when (< (buffersize context) samples)
        (error "Number of samples cannot be greater than the buffer size."))
      (setf (samples context) (buffersize context))))

(defgeneric call-in-mixing-context (function context &key &allow-other-keys))

(defmethod call-in-mixing-context (function (context mixing-context) &key)
  (funcall function))

(defmacro with-body-in-mixing-context ((context &rest args) &body body)
  `(call-in-mixing-context (lambda () ,@body) ,context ,@args))
