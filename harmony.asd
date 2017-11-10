#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem harmony
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A common lisp sound server and sound processing library."
  :homepage "https://github.com/Shirakumo/harmony"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "segment")
               (:file "server")
               (:file "mixers")
               (:file "fadable")
               (:file "source")
               (:file "drain")
               (:file "pipeline")
               (:file "files")
               (:module "sources"
                :components ((:file "buffer")))
               (:file "documentation"))
  :depends-on (:cl-mixed
               :flow
               :bordeaux-threads))
