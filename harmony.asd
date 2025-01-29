(asdf:defsystem harmony
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A common lisp sound server and sound processing library."
  :homepage "https://Shirakumo.github.io/harmony/"
  :bug-tracker "https://github.com/Shirakumo/harmony/issues"
  :source-control (:git "https://github.com/Shirakumo/harmony.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "server")
               (:file "segment")
               (:file "voice")
               (:file "simple")
               (:file "environment")
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:cl-mixed
               :atomics
               :bordeaux-threads
               :stealth-mixin
               (:feature (:and (:not :harmony-no-extensions) :windows (:not :harmony-no-wasapi)) :cl-mixed-wasapi)
               (:feature (:and (:not :harmony-no-extensions) :windows (:not :harmony-no-winmm)) :cl-mixed-winmm)
               (:feature (:and (:not :harmony-no-extensions) :linux (:not :harmony-no-alsa)) :cl-mixed-alsa)
               (:feature (:and (:not :harmony-no-extensions) :linux (:not :harmony-no-pulse)) :cl-mixed-pulse)
               (:feature (:and (:not :harmony-no-extensions) :linux (:not :harmony-no-pipewire)) :cl-mixed-pipewire)
               (:feature (:and (:not :harmony-no-extensions) :darwin (:not :harmony-no-coreaudio)) :cl-mixed-coreaudio)
               (:feature (:and (:not :harmony-no-extensions) :bsd (:not :harmony-no-oss)) :cl-mixed-oss)))
