#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-coreaudio
  (:nicknames #:org.shirakumo.fraf.harmony.drains.coreaudio)
  (:use #:cl #:harmony)
  (:export
   #:coreaudio-error
   #:code
   #:coreaudio-drain))
(in-package #:org.shirakumo.fraf.harmony.drains.coreaudio)

