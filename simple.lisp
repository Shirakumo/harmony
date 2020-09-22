#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defun detect-platform-drain ()
  (let (#+windows (version (cffi:foreign-funcall "GetVersion" :int32)))
    (cond #+bsd
          ((probe-file "/dev/dsp")
           'org.shirakumo.fraf.mixed.oss:drain)
          #+windows
          ((<= 6 (ldb (byte 8 0) version))  ; WASAPI since Vista (6.0)
           'org.shirakumo.fraf.mixed.wasapi:drain)
          #+windows
          (T
           'org.shirakumo.fraf.mixed.winmm:drain)
          #+linux
          ((org.shirakumo.fraf.mixed.pulse:pulse-present-p)
           'org.shirakumo.fraf.mixed.pulse:drain)
          #+linux
          (T
           'org.shirakumo.fraf.mixed.alsa:drain)
          #+darwin
          (T
           'org.shirakumo.fraf.mixed.coreaudio:drain))))

(defun construct-output (&key (drain (detect-platform-drain)) (target-channels 2) (server *server*) (program-name (name server)))
  (format *error-output* "[Harmony] Will use ~s for output.~%" drain)
  (let* ((packer (mixed:make-packer :channels target-channels :samplerate (samplerate server)))
         (drain (make-instance drain :pack (mixed:pack packer) :program-name program-name))
         (channels (mixed:channels packer))
         (chain (make-instance 'mixed:chain :name :output)))
    (when (/= channels target-channels)
      (let ((convert (mixed:make-channel-convert :in target-channels :out channels)))
        (connect convert T packer T)
        (mixed:add convert chain)))
    (add-to chain packer drain)))

(defun make-simple-server (&key (name "Harmony") (samplerate mixed:*default-samplerate*) (latency 0.01))
  (let* ((server (make-instance 'server :name name :samplerate samplerate :buffersize (ceiling (* latency samplerate))))
         (sources (make-instance 'mixed:chain :name :sources))
         (music (make-instance 'mixed:basic-mixer :name :music :channels 2))
         (speech (make-instance 'mixed:basic-mixer :name :speech))
         (effect (make-instance 'mixed:space-mixer :name :effect))
         (master (make-instance 'mixed:basic-mixer :name :master))
         (output (construct-output :server server)))
    (connect music T master T)
    (connect speech T master T)
    (connect effect T master T)
    (connect master T (segment 0 output) T)
    (add-to server sources music speech effect master output)))

(defun maybe-start-simple-server (&rest initargs)
  (unless *server*
    (apply #'make-simple-server initargs))
  (unless (started-p *server*)
    (mixed:start *server*)))

(defun play (source &key name (mixer :effect) effects (server *server*) repeat (on-end :free) location velocity (volume 1.0) (if-exists :error))
  (when (and name (segment name server NIL))
    (ecase if-exists
      (:error
       (error "A segment with the requested name already exists."))
      (:restart
       (mixed:seek (segment name server NIL) 0))
      ((:replace :supersede)
       (setf (mixed:done-p (segment name server NIL)) T))
      ((:ignore NIL)
       (return-from play NIL))))
  (let* ((mixer (segment mixer server))
         (sources (segment :sources server))
         (voice (make-instance 'voice :source source :name name :effects effects :repeat repeat :on-end on-end :channels (mixed:channels mixer))))
    ;; Allocate buffers and start segment now while we're still synchronous to catch errors
    ;; and avoid further latency/allocation in the mixing thread.
    (loop for i from 0 below (length (mixed:outputs voice))
          do (setf (mixed:output i voice) (allocate-buffer server)))
    (setf (mixed:volume voice) volume)
    (setf (segment name server) voice)
    (mixed:start voice)
    (with-server (server)
      (mixed:add voice sources)
      (connect voice T mixer T)
      (when location (setf (mixed:location voice) location))
      (when velocity (setf (mixed:velocity voice) velocity)))
    voice))

(defmethod voices ((server (eql T)))
  (voices *server*))

(defmethod voices ((server server))
  (coerce (mixed:segments (segment :sources server)) 'list))

(defmethod clear ((server (eql T)))
  (clear *server*))

(defmethod clear ((server server))
  (let ((sources (segment :sources server)))
    (with-server (server :synchronize T)
      (loop until (= 0 (length (mixed:segments sources)))
            do (mixed:free (aref (mixed:segments sources) 0))))))

(defmethod mixed:location ((server server))
  (mixed:location (segment :effect server)))

(defmethod (setf mixed:location) (location (server server))
  (setf (mixed:location (segment :effect server)) location))

(defmethod mixed:velocity ((server server))
  (mixed:velocity (segment :effect server)))

(defmethod (setf mixed:velocity) (velocity (server server))
  (setf (mixed:velocity (segment :effect server)) velocity))

(defmethod mixed:volume ((server server))
  (mixed:volume (segment :master server)))

(defmethod (setf mixed:volume) (volume (server server))
  (setf (mixed:volume (segment :master server)) volume))
