#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony
  (:nicknames #:org.shirakumo.fraf.harmony)
  (:use #:cl)
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
   #:call-with-server-lock
   #:with-server-lock
   #:paused-p
   #:pause
   #:resume)
  ;; source.lisp
  (:export
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
  ;; toolkit.lisp
  (:export))
