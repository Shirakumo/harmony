#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass mixer (segment)
  ((buffers :initform NIL :accessor buffers)))

(defmethod initialize-instance :after ((segment mixer) &key)
  (let ((buffers (make-array (channels segment))))
    (dotimes (i (length buffers))
      (setf (aref buffers i) (cl-mixed:make-buffer (buffersize (server segment)))))
    (setf (buffers segment) buffers)))

(defmethod add :before ((segment segment) (mixer mixer))
  (let ((buffers (buffers mixer)))
    (dotimes (i (length buffers))
      (setf (cl-mixed:output-field :buffer i segment) (aref buffers i)))))

(defmethod add :around ((segment segment) (mixer mixer))
  (with-body-in-server-thread ((server mixer)
                               ;; Apparently synchronising is unbearably slow.
                               #-(and sbcl windows) :synchronize #-(and sbcl windows) T)
    (call-next-method)))

(defmethod withdraw :around ((segment segment) (mixer mixer))
  (with-body-in-server-thread ((server mixer) :synchronize T)
    (call-next-method)))

(defmethod sources ((mixer mixer))
  (loop for v being the hash-values of (cl-mixed:sources mixer)
        collect v))

(defclass linear-mixer (cl-mixed:linear-mixer mixer)
  ()
  (:default-initargs
   :channels 2))

(defclass space-mixer (cl-mixed:space mixer)
  ())

(defmethod channels ((segment space-mixer))
  1)
