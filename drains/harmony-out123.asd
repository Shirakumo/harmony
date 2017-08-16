#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem harmony-out123
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "OUT123 based playback drain for Harmony"
  :homepage "https://github.com/Shirakumo/harmony"
  :serial T
  :components ((:file "out123"))
  :depends-on (:harmony
               :cl-out123))
