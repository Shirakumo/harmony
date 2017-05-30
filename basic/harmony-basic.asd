#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem harmony-basic
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
               :harmony-out123
               :documentation-utils))
