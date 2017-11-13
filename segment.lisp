#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass segment (cl-mixed:segment)
  ((context :initarg :context :accessor context)
   (name :initarg :name :reader name))
  (:default-initargs
   :context (error "CONTEXT initarg required.")
   :name NIL))

(defmethod print-object ((segment segment) stream)
  (print-unreadable-object (segment stream :type T)
    (format stream "~@[~s~]" (name segment))))
