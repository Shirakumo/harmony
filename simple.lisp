#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defun detect-platform-drain ()
  (let (#+windows (version (cffi:foreign-funcall "GetVersion" :int32)))
    (or (cond #+bsd
              ((probe-file "/dev/dsp")
               'org.shirakumo.fraf.mixed.oss:drain)
              #+windows
              ((<= 6 (ldb (byte 8 0) version)) ; WASAPI since Vista (6.0)
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
               'org.shirakumo.fraf.mixed.coreaudio:drain))
        'org.shirakumo.fraf.mixed.dummy:drain)))

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
    (format *error-output* "~&[Harmony] Will use ~s for output (~ax~a @ ~akHz)~%"
            (class-name (class-of drain)) channels (mixed:encoding packer) (mixed:samplerate packer))
    (add-to chain packer drain)))

(defun make-simple-server (&key (name "Harmony") (samplerate mixed:*default-samplerate*) (drain (detect-platform-drain)) (latency 0.01)
                                (output-channels 2) effects (mixers '(:music :speech (:effect mixed:space-mixer))))
  (let* ((server (make-instance 'server :name name :samplerate samplerate :buffersize (ceiling (* latency samplerate))))
         (sources (make-instance 'mixed:chain :name :sources))
         (master (make-instance 'mixed:basic-mixer :name :master :channels 2))
         (output (construct-output :drain drain :server server :source-channels 2 :target-channels output-channels)))
    (add-to server sources)
    (flet ((add-effects (source effects)
             (dolist (effect effects source)
               (let ((effect (ensure-effect-segment effect 2)))
                 (connect source T effect T)
                 (add-to server effect)
                 (setf source effect)))))
      (dolist (mixer mixers)
        (destructuring-bind (name &optional (type 'mixed:basic-mixer) &rest args &key effects &allow-other-keys)
            (if (listp mixer) mixer (list mixer))
          (remf args :effects)
          (let* ((mixer (apply #'make-instance type :name name args)))
            (add-to server mixer)
            (connect (add-effects mixer effects) T master T))))
      (add-to server master)
      (connect (add-effects master effects) T (segment 0 output) T))
    (let ((segments (mixed:segments output)))
      (mixed:match-channel-order (aref segments (- (length segments) 2))
                                 (mixed:channel-order (aref segments (- (length segments) 1)))))
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

(defmethod play (source &key name (class 'voice) (mixer :effect) effects (server *server*) repeat (repeat-start 0) (on-end :free) location velocity (volume 1.0) (if-exists :error) synchronize reset)
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
                      (create source :name name :class class :mixer mixer :effects effects
                                     :server server :repeat repeat :repeat-start repeat-start
                                     :on-end on-end :volume volume :if-exists if-exists))
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

(defun create (source &rest args &key name (class 'voice) (mixer :effect) (server *server*) (on-end :disconnect) (volume 1.0) (if-exists :error) &allow-other-keys)
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
    (let* ((args (loop for (k v) on args by #'cddr
                       for valid = (not (member k '(:class :mixer :server :on-end :volume :if-exists)))
                       when valid collect k when valid collect v))
           (voice (apply #'make-instance class :source source :on-end on-end :channels (mixed:channels mixer) args)))
      ;; Allocate buffers and start segment now while we're still synchronous to catch errors
      ;; and avoid further latency/allocation in the mixing thread.
      (loop for i from 0 below (length (mixed:outputs voice))
            do (setf (mixed:output i voice) (allocate-buffer server)))
      (when (name voice)
        (setf (segment (name voice) server) voice))
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

(defmethod mixed:min-distance ((server server))
  (mixed:min-distance (segment :effect server)))

(defmethod (setf mixed:min-distance) (min-distance (server server))
  (setf (mixed:min-distance (segment :effect server)) min-distance))

(defmethod mixed:max-distance ((server server))
  (mixed:max-distance (segment :effect server)))

(defmethod (setf mixed:max-distance) (max-distance (server server))
  (setf (mixed:max-distance (segment :effect server)) max-distance))

(defmethod mixed:rolloff ((server server))
  (mixed:rolloff (segment :effect server)))

(defmethod (setf mixed:rolloff) (rolloff (server server))
  (setf (mixed:rolloff (segment :effect server)) rolloff))

(defmethod mixed:attenuation ((server server))
  (mixed:attenuation (segment :effect server)))

(defmethod (setf mixed:attenuation) (attenuation (server server))
  (setf (mixed:attenuation (segment :effect server)) attenuation))

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
