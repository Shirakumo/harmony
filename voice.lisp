#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defun ensure-effect-segment (segment-ish channels)
  (flet ((channel-mismatch (segment)
           (error "Cannot connect~%  ~a~%as an effect segment as it does not comply with the required channel count of ~d"
                  segment channels)))
    (etypecase segment-ish
      (segment
       (destructuring-bind (&key min-inputs max-inputs &allow-other-keys) (mixed:info segment-ish)
         (unless (<= min-inputs channels max-inputs)
           (channel-mismatch segment-ish))
         segment-ish))
      ((or symbol class cons)
       (let* ((init (if (listp segment-ish) segment-ish (list segment-ish)))
              (proto (apply #'make-instance init)))
         (destructuring-bind (&key min-inputs max-inputs &allow-other-keys) (mixed:info proto)
           (cond ((<= min-inputs channels max-inputs)
                  proto)
                 ((= min-inputs max-inputs 1)
                  (let ((bundle (mixed:make-bundle channels)))
                    (setf (aref (mixed:segments bundle) 0) proto)
                    (loop for i from 1 below channels
                          do (setf (aref (mixed:segments bundle) i) (apply #'make-instance init)))
                    bundle))
                 (T
                  (mixed:free proto)
                  (channel-mismatch proto)))))))))

(defclass voice (mixed:chain)
  ())

(defmethod print-object ((voice voice) stream)
  (print-unreadable-object (voice stream :type T)
    (format stream "~@[~a ~]" (name voice))
    (let ((source (source voice)))
      (cond ((mixed:done-p source)
             (write-string "DONE" stream))
            ((null (mixed:frame-count source))
             (write-string "STREAM" stream))
            (T
             (format stream "~2d%" (floor (* (/ (mixed:byte-position source) (mixed:framesize source) (mixed:frame-count source)) 100))))))))

(defgeneric make-source-for (source &rest initargs)
  (:method ((source pathname) &rest initargs)
    (if (pathname-type source)
        (apply #'make-source-for-path-type source (intern (string-upcase (pathname-type source)) "KEYWORD") initargs)
        (error "Pathname has no type:~%  ~a" source))))

(defgeneric make-source-for-path-type (pathname type &rest initargs)
  (:method (source type &rest initargs)
    (macrolet ((maybe-make-drain (package system)
                 `(apply #'make-instance
                         (handler-bind (#+quicklisp
                                        (error (lambda (e)
                                                 (declare (ignore e))
                                                 (ql:quickload ,(string system))
                                                 (invoke-restart 'retry))))
                           (lazy-symbol ,package source))
                         :file source initargs)))
      (ecase type ;; static deferral. Not great, but can't do it otherwise with ASDF.
        (:mp3 (maybe-make-drain org.shirakumo.fraf.mixed.mpg123 cl-mixed-mpg123))
        (:wav (maybe-make-drain org.shirakumo.fraf.mixed.wav cl-mixed-wav))
        (:flac (maybe-make-drain org.shirakumo.fraf.mixed.flac cl-mixed-flac))))))

(defmethod initialize-instance :after ((voice voice) &key source effects repeat repeat-start (on-end :free) channels)
  (flet ((free (_) (declare (ignore _))
           (with-server (*server* :synchronize NIL)
             (mixed:free voice)))
         (disconnect (_) (declare (ignore _))
           (with-server (*server* :synchronize NIL)
             (disconnect voice T)
             (mixed:withdraw voice T))))
    (let ((unpacker (allocate-unpacker *server*))
          (on-end (ecase on-end
                    (:free #'free)
                    (:disconnect #'disconnect))))
      (mixed:add (make-source-for source :pack (mixed:pack unpacker) :repeat repeat :repeat-start repeat-start :on-end on-end) voice)
      (mixed:add unpacker voice)
      (mixed:revalidate unpacker)
      (dolist (effect effects)
        (let ((outputs (getf (mixed:info (voice-end voice)) :outputs)))
          (mixed:add (ensure-effect-segment effect outputs) voice)))
      (let ((outputs (length (mixed:outputs (voice-end voice)))))
        (when (and channels (/= channels outputs))
          (mixed:add (mixed:make-channel-convert :in outputs :out channels) voice))))))

(defmethod mixed:free :before ((voice voice))
  (when (< 0 (length (mixed:segments voice)))
    (mixed:withdraw voice T)
    (when (name voice)
      (setf (segment (name voice) *server*) NIL))
    (disconnect voice T)))

(defmethod mixed:free :after ((voice voice))
  (when (< 0 (length (mixed:segments voice)))
    (mixed:free (source voice))
    (free-unpacker (mixed:unpacker voice) *server*)
    (loop for i from 2 below (length (mixed:segments voice))
          for segment = (aref (mixed:segments voice) i)
          do (disconnect segment T)
             (mixed:free segment))))

(defmethod mixed:add :before ((segment segment) (voice voice))
  (when (< 1 (length (mixed:segments voice)))
    (connect (voice-end voice) T segment T)))

(defmethod source ((voice voice))
  (aref (mixed:segments voice) 0))

(defmethod mixed:unpacker ((voice voice))
  (aref (mixed:segments voice) 1))

(defmethod mixed:volume ((voice voice))
  (mixed:volume (mixed:unpacker voice)))

(defmethod (setf mixed:volume) (value (voice voice))
  (setf (mixed:volume (mixed:unpacker voice)) value))

(defun voice-end (voice)
  (aref (mixed:segments voice) (1- (length (mixed:segments voice)))))

(defmethod connect ((from voice) from-loc to to-loc)
  (connect (voice-end from) from-loc to to-loc))

(defmethod disconnect ((from voice) from-loc &key (direction :output))
  (unless (eq direction :output)
    (error "Cannot disconnect voice from input, as it does not have any."))
  (disconnect (voice-end from) from-loc :direction :output))

(defmethod repeat ((voice voice))
  (repeat (source voice)))

(defmethod (setf repeat) (value (voice voice))
  (setf (repeat (source voice)) value))

(defmethod mixed:outputs ((from voice))
  (mixed:outputs (voice-end from)))

(defmethod mixed:output (location (from voice))
  (mixed:output location (voice-end from)))

(defmethod (setf mixed:output) (value location (from voice))
  (setf (mixed:output location (voice-end from)) value))

(defmethod mixed:done-p ((voice voice))
  (mixed:done-p (source voice)))

(defmethod (setf mixed:done-p) (value (voice voice))
  (setf (mixed:done-p (source voice)) value))

(defmethod mixed:location ((voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (mixed:input-location buffer (to buffer))))

(defmethod (setf mixed:location) (location (voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (setf (mixed:input-location buffer (to buffer)) location)))

(defmethod mixed:velocity ((voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (mixed:input-velocity buffer (to buffer))))

(defmethod (setf mixed:velocity) (velocity (voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (setf (mixed:input-velocity buffer (to buffer)) velocity)))

(defmethod mixed:seek ((voice voice) position &rest args)
  (apply #'mixed:seek (source voice) position args)
  voice)

(defmethod stop ((voice voice))
  (with-server (*server* :synchronize NIL)
    (disconnect voice T)
    (mixed:withdraw voice T))
  voice)
