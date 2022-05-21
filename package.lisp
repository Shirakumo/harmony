#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.harmony
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:atomics #:org.shirakumo.atomics))
  (:use #:cl)
  ;; environment.lisp
  (:export
   #:environment
   #:state
   #:active-p
   #:music-segment
   #:transition)
  ;; segment.lisp
  (:export
   #:buffer
   #:from
   #:from-location
   #:to
   #:to-location
   #:segment
   #:name
   #:chain
   #:connect
   #:disconnect
   #:downstream
   #:upstream
   #:source
   #:repeat
   #:repeat-start
   #:on-end
   #:on-frame-change)
  ;; server.lisp
  (:export
   #:*server*
   #:server
   #:allocate-buffer
   #:allocate-unpacker
   #:free-buffer
   #:free-unpacker
   #:segment
   #:started-p
   #:run-task
   #:run
   #:call-in-mixing-context
   #:with-server
   #:dot-server)
  ;; simple.lisp
  (:export
   #:detect-platform-drain
   #:make-simple-server
   #:maybe-start-simple-server
   #:play
   #:create
   #:voices
   #:clear)
  ;; toolkit.lisp
  (:export
   #:add-to)
  ;; voice.lisp
  (:export
   #:voice
   #:make-source-for
   #:make-source-for-path-type
   #:track-end
   #:source
   #:stop))

(defpackage #:org.shirakumo.fraf.harmony.user
  (:use #:org.shirakumo.fraf.mixed #:org.shirakumo.fraf.harmony)
  (:shadowing-import-from #:org.shirakumo.fraf.harmony
    #:buffer #:segment #:source #:repeat #:connect #:from #:to #:clear)
  (:shadowing-import-from #:org.shirakumo.fraf.mixed #:chain)
  (:export
   #:start
   #:end
   #:free
   #:add
   #:withdraw
   #:output
   #:outputs
   #:input
   #:inputs
   #:input-field
   #:output-field
   #:field
   #:segment
   #:segments
   #:seek
   #:frame-count
   #:frame-position
   #:repeat
   #:repeat-start
   #:done-p
   #:bypass
   #:volume
   #:location
   #:velocity
   #:play
   #:create
   #:stop
   #:*server*
   #:server
   #:make-simple-server
   #:maybe-start-simple-server
   #:voices
   #:clear
   #:started-p
   #:with-server
   #:environment
   #:state
   #:active-p
   #:music-segment
   #:transition))
