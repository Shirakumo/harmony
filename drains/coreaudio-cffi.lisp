#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-coreaudio-cffi
  (:nicknames #:org.shirakumo.fraf.harmony.drains.coreaudio.cffi)
  (:use #:cl #:cffi)
  (:export
   ))
(in-package #:org.shirakumo.fraf.harmony.drains.coreaudio.cffi)

;; http://kaniini.dereferenced.org/2014/08/31/CoreAudio-sucks.html
;; https://stackoverflow.com/questions/28513924/how-can-i-use-apples-core-audio-c-api-to-create-a-simple-real-time-i-o-stream
