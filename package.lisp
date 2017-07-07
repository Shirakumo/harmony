#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony
  (:nicknames #:org.shirakumo.fraf.harmony)
  (:use #:cl)
  (:import-from #:cl-mixed
                #:inputs #:outputs #:input #:output #:segments
                #:channel #:add #:withdraw #:clear #:end #:info
                #:handle #:pointer->object #:free
                #:samplerate #:channels #:encoding #:channels
                #:volume #:bypass #:location #:velocity
                #:direction #:up #:input-location #:input-velocity
                #:soundspeed #:doppler-factor #:min-distance
                #:max-distance #:rolloff #:attenuation)
  ;; sources/buffer.lisp
  (:export
   #:buffer-source
   #:data
   #:size)
  ;; drain.lisp
  (:export
   #:drain
   #:decoder
   #:server
   #:paused-p
   #:pause
   #:resume
   #:initialize-channel)
  ;; fadable.lisp
  (:export
   #:fade)
  ;; mixers.lisp
  (:export
   #:mixer
   #:add
   #:withdraw
   #:sources
   #:channels
   #:linear-mixer
   #:space-mixer
   #:location
   #:velocity
   #:direction
   #:up
   #:soundspeed
   #:doppler-factor
   #:min-distance
   #:max-distance
   #:rolloff
   #:attenuation)
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
  ;; segment.lisp
  (:export
   #:volume)
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
   #:mixer
   #:sample-position
   #:initialize-channel
   #:pause
   #:resume
   #:stop
   #:seek
   #:seek-to-sample
   #:location
   #:velocity
   #:source-type
   #:define-source-type
   #:play))
