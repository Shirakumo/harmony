#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-simple
  (:nicknames #:org.shirakumo.fraf.harmony.simple)
  (:use #:cl #:harmony #:harmony-out123 #:harmony-mp3)
  (:shadow #:start #:stop #:pause #:paused-p #:resume)
  ;; default.lisp
  (:export
   #:*server*
   #:default-server
   #:make-pipeline
   #:play)
  ;; files.lisp
  (:export
   #:source-type
   #:define-source-type)
  ;; reexport
  (:export
   #:mp3-source
   #:out123-drain
   #:linear-mixer
   #:space-mixer
   #:add
   #:withdraw
   #:sources
   #:channels
   #:pause
   #:resume
   #:start
   #:stop
   #:seek
   #:fade
   #:looping-p
   #:paused-p
   #:ended-p
   #:location
   #:velocity
   #:direction
   #:volume
   #:segment))
