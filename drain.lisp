#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defun detect-platform-drain ()
  (cond ((org.shirakumo.fraf.mixed.jack:jack-present-p)
         'org.shirakumo.fraf.mixed.jack:drain)
        (T
         #+windows
         (let ((major (ldb (byte 8 8) (cffi:foreign-funcall "GetVersion" :int32)))
               (minor (ldb (byte 8 0) (cffi:foreign-funcall "GetVersion" :int32))))
           (if (<= 6 major) 
               'org.shirakumo.fraf.mixed.wasapi:drain ; WASAPI since Vista (6.0)
               'org.shirakumo.fraf.mixed.winmm:drain))
         #+linux
         (if (org.shirakumo.fraf.mixed.pulse:pulse-present-p)
             'org.shirakumo.fraf.mixed.pulse:drain
             'org.shirakumo.fraf.mixed.alsa:drain)
         #+darwin
         'org.shirakumo.fraf.mixed.coreaudio:drain
         #+bsd
         'org.shirakumo.fraf.mixed.oss:drain)))

(defun make-drain (&key (target-channels 2) (internal-samplerate 48000) (samples (/ target-samplerate 100)) (program-name "Harmony"))
  (let ((pack (mixed:make-pack samples :float channels internal-samplerate)))
    (make-instance (detect-platform-drain) :program-name program-name)))
