#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem harmony-flac
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "FLAC sound source for Harmony."
  :homepage "https://github.com/Shirakumo/harmony"
  :serial T
  :components ((:file "flac"))
  :depends-on (:harmony
               :cl-flac))
