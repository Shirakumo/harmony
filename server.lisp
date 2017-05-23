#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass server ()
  (thread
   lock
   device
   mixer
   buffers))

(defgeneric start (server))
(defgeneric stop (server))

(defclass pipeline ()
  (nodes))

(defgeneric compile-pipeline (pipeline server))
