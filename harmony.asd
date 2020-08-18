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
               (:file "mixing-context")
               (:file "server")
               (:file "mixers")
               (:file "fadable")
               (:file "files")
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:cl-mixed
               :bordeaux-threads
               (:if-feature :windows :cl-mixed-wasapi)
               (:if-feature :windows :cl-mixed-winmm)
               (:if-feature :linux :cl-mixed-alsa)
               (:if-feature :linux :cl-mixed-pulse)
               (:if-feature :darwin :cl-mixed-coreaudio)
               (:if-feature :bsd :cl-mixed-oss)
               :cl-mixed-jack))
