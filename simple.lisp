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

(defun construct-output (&key (drain (detect-platform-drain)) (source-channels 2) (target-channels source-channels) (server *server*) (program-name (name server)))
  (let* ((packer (mixed:make-packer :channels target-channels :samplerate (samplerate server)))
         (drain (make-instance drain :pack (mixed:pack packer) :program-name program-name))
         (channels (mixed:channels packer))
         (chain (make-instance 'mixed:chain :name :output)))
    (mixed:revalidate packer)
    (when (/= channels source-channels)
      (let ((convert (mixed:make-channel-convert :in source-channels :out channels)))
        (connect convert T packer T)
        (mixed:add convert chain)))
    (mixed:match-channel-order packer (mixed:channel-order drain))
    (format *error-output* "[Harmony] Will use ~a for output (~ax~a @ ~akHz)~%"
            (string (class-name (class-of drain))) channels (mixed:encoding packer) (mixed:samplerate packer))
    (add-to chain packer drain)))

(defun make-simple-server (&key (name "Harmony") (samplerate mixed:*default-samplerate*) (latency 0.01) effects (output-channels 2))
  (let* ((server (make-instance 'server :name name :samplerate samplerate :buffersize (ceiling (* latency samplerate))))
         (sources (make-instance 'mixed:chain :name :sources))
         (music (make-instance 'mixed:basic-mixer :name :music :channels 2))
         (speech (make-instance 'mixed:basic-mixer :name :speech :channels 2))
         (effect (make-instance 'mixed:space-mixer :name :effect))
         (master (make-instance 'mixed:basic-mixer :name :master :channels 2))
         (output (construct-output :server server :source-channels 2 :target-channels output-channels))
         (last master))
    (connect music T master T)
    (connect speech T master T)
    (connect effect T master T)
    (add-to server sources music speech effect master)
    (dolist (effect effects)
      (let ((effect (ensure-effect-segment effect 2)))
        (connect last T effect T)
        (setf last effect)
        (add-to server last)))
    (connect last T (segment 0 output) T)
    (add-to server output)))

(defun maybe-start-simple-server (&rest initargs)
  (unless *server*
    (apply #'make-simple-server initargs))
  (unless (started-p *server*)
    (mixed:start *server*)))

(defun ensure-segment (segment-ish &optional (server *server*))
  (etypecase segment-ish
    (segment segment-ish)
    (null (error "No segment given."))
    (T (segment segment-ish server))))

(defgeneric play (source &key))

(defmethod play (source &key name (mixer :effect) effects (server *server*) repeat (repeat-start 0) (on-end :free) location velocity (volume 1.0) (if-exists :error) synchronize reset)
  (let ((mixer (ensure-segment mixer server))
        (sources (segment :sources server))
        (voice (when name (segment name server NIL))))
    (case if-exists
      (:ignore
       (when voice (return-from play voice)))
      ((NIL)
       (when voice (return-from play NIL)))
      (T
       (setf voice (etypecase source
                     (voice source)
                     ((or segment pathname)
                      (create source :name name :mixer mixer :effects effects :server server
                                     :repeat repeat :repeat-start repeat-start :on-end on-end
                                     :volume volume :if-exists if-exists))
                     (T (ensure-segment source server))))))
    (when reset
      (mixed:seek voice 0))
    ;; FIXME: what do we do if the source is already chained but on a different
    ;;        mixer? Sounds like unexpected behaviour, but I honestly don't know
    ;;        why you'd ever want to move a voice to a different mixer.
    ;; KLUDGE: this also seems like a source for race conditions.
    (unless (chain voice)
      (with-server (server :synchronize synchronize)
        (unless (chain voice)
          (mixed:add voice sources)
          (connect voice T mixer T))
        (when location (setf (mixed:location voice) location))
        (when velocity (setf (mixed:velocity voice) velocity))))
    voice))

(defun create (source &key name (mixer :effect) effects (server *server*) repeat (repeat-start 0) (on-end :disconnect) (volume 1.0) (if-exists :error))
  (let ((mixer (ensure-segment mixer server))
        (voice (when name (segment name server NIL))))
    (when voice
      (ecase if-exists
        (:error
         (error "A segment with the requested name already exists."))
        (:restart
         (mixed:seek voice 0)
         (when volume (setf (mixed:volume voice) volume))
         (return-from create voice))
        (:stop
         (return-from create (stop voice)))
        ((:replace :supersede)
         (setf (repeat voice) NIL)
         (setf (mixed:done-p voice) T))
        (:ignore
         (return-from create voice))
        ((NIL)
         (return-from create NIL))))
    (let ((voice (make-instance 'voice :source source :name name :effects effects
                                       :repeat repeat :repeat-start repeat-start
                                       :on-end on-end :channels (mixed:channels mixer))))
      ;; Allocate buffers and start segment now while we're still synchronous to catch errors
      ;; and avoid further latency/allocation in the mixing thread.
      (loop for i from 0 below (length (mixed:outputs voice))
            do (setf (mixed:output i voice) (allocate-buffer server)))
      (when name
        (setf (segment name server) voice))
      (mixed:start voice)
      (setf (mixed:volume voice) volume)
      voice)))

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

(defmethod stop (name)
  (stop (segment name *server*)))

(defmethod stop ((server server))
  (mixed:end server))
