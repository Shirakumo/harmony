#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass mixing-context ()
  ((buffersize :initarg :buffersize :initform 0 :accessor buffersize)
   (samplerate :initarg :samplerate :initform 0 :reader samplerate))
  (:default-initargs
   :buffersize 100
   :samplerate 48000))

(defmethod initialize-instance :after ((context mixing-context) &key samplerate (buffersize (/ samplerate 100)))
  (check-type buffersize (integer 50))
  (check-type samplerate (integer 1000)))

(defgeneric call-in-mixing-context (function context &key &allow-other-keys))

(defmethod call-in-mixing-context (function (context mixing-context) &key)
  (funcall function))

(defmacro with-body-in-mixing-context ((context &rest args) &body body)
  `(call-in-mixing-context (lambda () ,@body) ,context ,@args))
