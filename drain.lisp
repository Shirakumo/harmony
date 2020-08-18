
(defun detect-platform-drain ()
  (cond ((org.shirakumo.fraf.mixed.jack:jack-present-p)
         'org.shirakumo.fraf.mixed.jack:drain)
        (T
         #+windows
         (let ((major (ldb (byte 8 8) (cffi:foreign-funcall "GetVersion" :int32))))
           (if (<= 6 major) ; WASAPI started with Windows Vista (6.0)
               'org.shirakumo.fraf.mixed.wasapi:drain
               'org.shirakumo.fraf.mixed.winmm:drain))
         #+linux
         (if (org.shirakumo.fraf.mixed.pulse:pulse-present-p)
             'org.shirakumo.fraf.mixed.pulse:drain
             'org.shirakumo.fraf.mixed.alsa:drain)
         #+darwin
         'org.shirakumo.fraf.mixed.coreaudio:drain
         #+bsd
         'org.shirakumo.fraf.mixed.oss:drain)))

(defun make-drain (&key (channels 2) (target-samplerate 48000) (program-name "Harmony"))
  (let ((pack (mixed:make-pack 100 :float channels target-samplerate)))
    (make-instance (detect-platform-drain) :program-name program-name)))
