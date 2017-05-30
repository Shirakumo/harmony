#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass default-server (server)
  ())

(defmethod initialize-instance :after ((server default-server) &key)
  (let ((pipeline (make-pipeline server)))
    (compile-pipeline pipeline server)))

(defmethod make-pipeline ((server default-server))
  (let ((pipeline (make-instance 'pipeline))
        (music (make-instance 'linear-mixer))
        (sfx (make-instance 'space-mixer))
        (voice (make-instance 'space-mixer))
        (master (make-instance 'linear-mixer)))
    (connect pipeline music 0 master 0)
    (connect pipeline music 1 master 1)
    (connect pipeline sfx 0 master 2)
    (connect pipeline sfx 1 master 3)
    (connect pipeline voice 0 master 4)
    (connect pipeline voice 1 master 5)
    pipeline))
