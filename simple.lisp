(in-package #:org.shirakumo.fraf.harmony)

(defun detect-platform-segment (type)
  (mixed:init)
  (macrolet ((try (segment &optional predicate)
               `(let ((type (resolve-segment-type type ,segment NIL)))
                  ,(if predicate
                       `(when (and type ,predicate) type)
                       'type)))
             (c (package function &rest args)
               `(funcall (or (and (find-package ,(string package))
                                  (find-symbol ,(string function) ,(string package)))
                             (constantly NIL))
                         ,@args)))
    (or (try :sdl2)
        (try :out123)
        (try :jack (c org.shirakumo.fraf.mixed.jack jack-present-p))
        #+nx (try :nxau)
        #+bsd (try :oss (probe-file "/dev/dsp"))
        #+windows (try :wasapi (<= 6 (ldb (byte 8 0) (cffi:foreign-funcall "GetVersion" :int32))))
        #+windows (try :xaudio2)
        #+windows (try :winmm)
        #+linux (try :aaudio)
        #+linux (try :pulse (c org.shirakumo.fraf.mixed.pulse pulse-present-p))
        #+linux (try :alsa)
        #+linux (try :oss)
        #+darwin (try :coreaudio)
        (try :dummy))))

(defun resolve-segment-type (type segment &optional (errorp T))
  (flet ((find-segment (package)
           (or (and (find-package package)
                    (find-symbol (string type) package))
               (and errorp (error "~:@(~a~) ~s is not loaded." type package)))))
    (case segment
      (:aaudio (find-segment :org.shirakumo.fraf.mixed.aaudio))
      (:alsa (find-segment :org.shirakumo.fraf.mixed.alsa))
      (:coreaudio (find-segment :org.shirakumo.fraf.mixed.coreaudio))
      (:dummy (find-segment :org.shirakumo.fraf.mixed.dummy))
      (:jack (find-segment :org.shirakumo.fraf.mixed.jack))
      (:nxau (find-segment :org.shirakumo.fraf.mixed.nxau))
      (:oss (find-segment :org.shirakumo.fraf.mixed.oss))
      (:out123 (find-segment :org.shirakumo.fraf.mixed.out123))
      (:pulse (find-segment :org.shirakumo.fraf.mixed.pulse))
      (:sdl2 (find-segment :org.shirakumo.fraf.mixed.sdl2))
      (:wasapi (find-segment :org.shirakumo.fraf.mixed.wasapi))
      (:winmm (find-segment :org.shirakumo.fraf.mixed.winmm))
      (:xaudio2 (find-segment :org.shirakumo.fraf.mixed.xaudio2))
      (:default (resolve-segment-type type (detect-platform-segment type)))
      (T (if (subtypep segment type)
             segment
             (error "~s is not a known ~(~a~) type" segment type))))))

(defun detect-platform-drain ()
  (detect-platform-segment 'mixed:drain))

(defun construct-output (&key (drain (detect-platform-drain)) (source-channels 2) (target-channels source-channels) (samplerate (samplerate *server*)) (program-name (name *server*)) device)
  (let* ((type (resolve-segment-type 'mixed:drain drain))
         (packer (mixed:make-packer :channels target-channels :samplerate samplerate))
         (drain (if (subtypep type 'mixed:device-drain)
                    (make-instance type :pack (mixed:pack packer) :name :drain :program-name program-name :device device)
                    (make-instance type :pack (mixed:pack packer) :name :drain :program-name program-name)))
         (channels (mixed:channels packer))
         (chain (make-instance 'mixed:chain :name :output)))
    (setf (slot-value packer 'name) :packer)
    (mixed:revalidate packer)
    (let ((convert (mixed:make-channel-convert :in source-channels :out channels)))
      (setf (slot-value convert 'name) :upmix)
      (connect convert T packer T)
      (mixed:add convert chain))
    (format *error-output* "~&[Harmony] Will use ~s for output (~ax~a @ ~akHz)~%"
            (class-name (class-of drain)) channels (mixed:encoding packer) (mixed:samplerate packer))
    (add-to chain packer drain)))

(defun make-simple-server (&key (name "Harmony") (samplerate mixed:*default-samplerate*) (drain (detect-platform-drain)) device (latency 0.01)
                                (output-channels 2) effects (mixers '(:music :speech (:effect mixed:space-mixer))))
  (mixed:init)
  (let* ((server (make-instance 'server :name name :samplerate samplerate :buffersize (ceiling (* latency samplerate))))
         (sources (make-instance 'mixed:chain :name :sources))
         (master (make-instance 'mixed:basic-mixer :name :master :channels 2))
         (output (construct-output :drain drain :samplerate samplerate :program-name name
                                   :source-channels 2 :target-channels output-channels :device device)))
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
                     ((or source pathname)
                      (create source :name name :class class :mixer mixer :effects effects
                                     :server server :repeat repeat :repeat-start repeat-start
                                     :on-end on-end :volume volume :if-exists if-exists))
                     (segment source)
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

(defmethod stop ((segment segment))
  (with-server (*server* :synchronize NIL)
    (disconnect segment T)
    (mixed:withdraw segment T)))
