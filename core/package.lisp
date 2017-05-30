#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-core
  (:nicknames #:org.shirakumo.fraf.harmony.core)
  (:use #:cl #:cl-mixed)
  (:shadow #:source #:drain #:space)
  ;; drain.lisp
  (:export
   #:drain
   #:decoder
   #:server
   #:paused-p
   #:pause
   #:resume
   #:initialize-channel)
  ;; pipeline.lisp
  (:export
   #:out-port
   #:output
   #:in-port
   #:input
   #:in-ports
   #:node
   #:ports
   #:segment
   #:make-node
   #:finalize-segment
   #:pipeline
   #:nodes
   #:ensure-node
   #:connect
   #:disconnect
   #:sever
   #:allocate-buffers
   #:compile-pipeline)
  ;; server.lisp
  (:export
   #:server
   #:buffersize
   #:samplerate
   #:device
   #:start
   #:stop
   #:process
   #:call-in-server-thread
   #:with-body-in-server-thread
   #:paused-p
   #:pause
   #:resume)
  ;; source.lisp
  (:export
   #:source
   #:looping-p
   #:paused-p
   #:ended-p
   #:decoder
   #:server
   #:sample-position
   #:initialize-channel
   #:pause
   #:resume
   #:stop
   #:seek)
  ;; re-exports from cl-mixed
  (:export
   #:volume
   #:bypass))
