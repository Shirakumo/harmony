#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.harmony
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:atomics #:org.shirakumo.atomics))
  (:use #:cl)
  (:import-from #:org.shirakumo.fraf.mixed #:volume)
  (:export))
