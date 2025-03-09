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
   #:mixing-queue-full
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

(unless (find-package '#:org.shirakumo.fraf.harmony.user)
  (defpackage #:org.shirakumo.fraf.harmony.user
    (:use #:org.shirakumo.fraf.mixed #:org.shirakumo.fraf.harmony)
    (:shadowing-import-from #:org.shirakumo.fraf.harmony
                            #:buffer #:segment #:source #:repeat #:connect #:from #:to #:clear)
    (:shadowing-import-from #:org.shirakumo.fraf.mixed #:chain)))

(let ((symbols ()))
  (do-symbols (symb '#:org.shirakumo.fraf.harmony.user) (push symb symbols))
  (export symbols '#:org.shirakumo.fraf.harmony.user))
