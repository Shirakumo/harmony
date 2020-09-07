#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.harmony
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:atomics #:org.shirakumo.atomics))
  (:use #:cl)
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
   #:on-end)
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
   #:with-server)
  ;; simple.lisp
  (:export
   #:detect-platform-drain
   #:make-simple-server
   #:maybe-start-simple-server
   #:play
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
   #:source))

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
   #:done-p
   #:bypass
   #:volume
   #:location
   #:velocity
   #:play
   #:*server*
   #:server
   #:make-simple-server
   #:maybe-start-simple-server
   #:voices
   #:clear
   #:started-p
   #:with-server))
