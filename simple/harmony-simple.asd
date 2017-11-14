#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem harmony-simple
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An easy to use common lisp sound system."
  :homepage "https://github.com/Shirakumo/harmony"
  :serial T
  :components ((:file "package")
               (:file "default")
               (:file "documentation"))
  :depends-on (:harmony
               (:feature :linux :harmony-alsa)
               (:feature :windows :harmony-wasapi)
               (:feature :darwin :harmony-coreaudio)
               :harmony-mp3
               :harmony-wav
               :harmony-flac
               :documentation-utils))
