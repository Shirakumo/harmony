#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-simple
  (:nicknames #:org.shirakumo.fraf.harmony.simple)
  (:use #:cl #:harmony #:harmony-out123 #:harmony-mp3)
  (:shadow #:start #:started-p #:stop #:pause #:paused-p #:resume #:play #:segment)
  ;; default.lisp
  (:export
   #:*server*
   #:default-server
   #:make-pipeline
   #:play
   #:segment)
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
   #:started-p
   #:stop
   #:seek
   #:fade
   #:looping-p
   #:paused-p
   #:ended-p
   #:location
   #:velocity
   #:direction
   #:volume))
