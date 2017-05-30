#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem harmony-core
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Base components of the Harmony sound system."
  :homepage "https://github.com/Shirakumo/harmony"
  :serial T
  :components ((:file "package")
               (:file "server")
               (:file "source")
               (:file "drain")
               (:file "pipeline")
               (:file "documentation"))
  :depends-on (:cl-mixed
               :flow
               :bordeaux-threads))
