#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem harmony-mp3
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "MP3 sound source for Harmony."
  :homepage "https://github.com/Shirakumo/harmony"
  :serial T
  :components ((:file "mp3"))
  :depends-on (:harmony
               :cl-mpg123))
