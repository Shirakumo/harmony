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

(defun construct-output (&key (target-channels 2) (server *server*) (program-name (name server)))
  (let* ((packer (mixed:make-packer :channels target-channels :samplerate (samplerate server)))
         (drain (make-instance (detect-platform-drain) :pack (mixed:pack packer) :program-name program-name))
         (channels (mixed:channels packer))
         (chain (mixed:make-chain :name :output)))
    (when (/= channels target-channels)
      (let ((convert (mixed:make-channel-convert :in target-channels :out channels)))
        (connect convert T packer T)
        (mixed:add convert chain)))
    (mixed:add packer chain)
    (mixed:add drain chain)
    chain))

(defun create-simple-server (&key (name "Harmony") (samplerate mixed:*default-samplerate*) (latency 0.01))
  (let ((server (make-instance 'server :name name :samplerate samplerate :buffersize (ceiling (* latency samplerate)))))
    (mixed:add (make-instance 'mixed:chain :name :sources) server)
    (mixed:add (make-instance 'mixed:basic-mixer :name :music :channels 2) server)
    (mixed:add (make-instance 'mixed:basic-mixer :name :speech :channels 2) server)
    (mixed:add (make-instance 'mixed:space-mixer :name :sfx) server)
    (mixed:add (construct-output :server server) server)
    server))

(defun play (source &key name (mixer :sfx) effects (server *server*) loop (on-end :free))
  (let ((mixer (segment mixer server))
        (sources (segment :sources server))
        (segment (make-instance 'faucet :name name :source source :segments effects :loop loop :on-end on-end)))
    (with-server (server)
      (mixed:add segment sources)
      (mixed:add segment mixer))
    segment)
