(in-package #:cl-user)
(let ((systems '(:harmony
                 :harmony-simple
                 :harmony-alsa
                 :harmony-coreaudio
                 :harmony-openal
                 :harmony-out123
                 :harmony-pulse
                 :harmony-wasapi
                 :harmony-flac
                 :harmony-mp3
                 :harmony-wav)))
  (mapcar #'asdf:load-system *systems*)
  (setf (staple:packages :harmony) *systems*))
