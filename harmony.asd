#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem harmony
  :version "2.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A common lisp sound server and sound processing library."
  :homepage "https://Shirakumo.github.io/harmony/"
  :bug-tracker "https://github.com/Shirakumo/harmony/issues"
  :source-control (:git "https://github.com/Shirakumo/harmony.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "server")
               (:file "segment")
               (:file "voice")
               (:file "simple")
               (:file "environment")
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:cl-mixed
               :atomics
               :bordeaux-threads
               :stealth-mixin
               (:feature :windows :cl-mixed-wasapi)
               (:feature :windows :cl-mixed-winmm)
               (:feature :linux :cl-mixed-alsa)
               (:feature :linux :cl-mixed-pulse)
               (:feature :darwin :cl-mixed-coreaudio)
               (:feature :bsd :cl-mixed-oss)))
