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
                #:packed-audio #:add #:withdraw #:clear #:end #:info
                #:handle #:pointer->object #:free
                #:samplerate #:channels #:encoding #:channels
                #:volume #:bypass #:location #:velocity
                #:direction #:up #:input-location #:input-velocity
                #:soundspeed #:doppler-factor #:min-distance
                #:max-distance #:rolloff #:attenuation)
  ;; drain.lisp
  (:export
   #:drain
   #:pack-drain
   #:remix-factor
   #:packed-audio
   #:pack-mix-function)
  ;; fadable.lisp
  (:export
   #:fadable
   #:fade)
  ;; files.lisp
  (:export
   #:source-type
   #:define-source-type
   #:file-source
   #:file)
  ;; mixers.lisp
  (:export
   #:mixer
   #:buffers
   #:channels-per-source
   #:add
   #:withdraw
   #:sources
   #:basic-mixer
   #:space-mixer
   #:location
   #:velocity
   #:input-location
   #:input-velocity
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
   #:segment
   #:server
   #:name
   #:volume)
  ;; server.lisp
  (:export
   #:server
   #:samples
   #:buffersize
   #:samplerate
   #:device
   #:start
   #:started-p
   #:stop
   #:run
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
   #:sample-position
   #:process
   #:pause
   #:resume
   #:stop
   #:seek
   #:seek-to-sample
   #:sample-count
   #:play
   #:unpack-source
   #:remix-factor
   #:packed-audio
   #:unpack-mix-function
   #:initialize-packed-audio
   #:fill-for-unpack-source)
  ;; toolkit.lisp
  (:export
   #:memcpy
   #:memset
   #:memclear
   #:ease-linear
   #:ease-cubic-in
   #:ease-cubic-out
   #:ease-cubic-in-out)
  ;; drains/buffer.lisp
  (:export
   #:buffer-drain
   #:buffers)
  ;; sources/buffer.lisp
  (:export
   #:buffer-source
   #:buffers))
