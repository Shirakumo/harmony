#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-simple
  (:nicknames #:org.shirakumo.fraf.harmony.simple)
  (:use #:cl #:harmony)
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
   #:add
   #:withdraw
   #:sources
   #:channels
   #:pause
   #:resume
   #:initialize
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
