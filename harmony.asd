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
  :homepage "https://Shirakumo.github.io/harmony/"
  :bug-tracker "https://github.com/Shirakumo/harmony/issues"
  :source-control (:git "https://github.com/Shirakumo/harmony.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "mixing-context")
               (:file "segment")
               (:file "server")
               (:file "mixers")
               (:file "fadable")
               (:file "source")
               (:file "drain")
               (:file "pipeline")
               (:file "files")
               (:module "drains"
                :components ((:file "buffer")))
               (:module "sources"
                :components ((:file "buffer")))
               (:file "documentation"))
  :depends-on (:cl-mixed
               :flow
               :bordeaux-threads))
