#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem harmony-wav
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "WAV sound source for Harmony."
  :homepage "https://github.com/Shirakumo/harmony"
  :serial T
  :components ((:file "wav"))
  :depends-on (:harmony
               :static-vectors
               :cffi))
