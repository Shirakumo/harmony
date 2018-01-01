(defvar *systems* '(:harmony
                    :harmony-simple
                    :harmony-alsa
                    :harmony-coreaudio
                    :harmony-openal
                    :harmony-out123
                    :harmony-pulse
                    :harmony-wasapi
                    :harmony-flac
                    :harmony-mp3
                    :harmony-wav))

(ql:quickload *systems*)

(defmethod staple:system-options append ((system (eql (asdf:find-system :harmony))))
  (list
   :packages *systems*))
