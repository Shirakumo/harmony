#|
This file is a part of harmony
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-wasapi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.wasapi)
  (:use #:cl #:harmony)
  (:export
   #:wasapi-error
   #:code
   #:wasapi-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.wasapi)

