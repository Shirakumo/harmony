#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass voice (mixed:chain)
  ())

(defgeneric make-source-for (source &rest initargs)
  (:method ((source pathname) &rest initargs)
    (if (pathname-type source)
        (apply #'make-source-for-path-type source (intern (string-upcase (pathname-type source)) "KEYWORD") initargs)
        (error "Pathname has no type:~%  ~a" source))))

(defgeneric make-source-for-path-type (pathname type &rest initargs)
  (:method (source type &rest initargs)
    (macrolet ((maybe-make-drain (package)
                 `(apply #'make-instance (lazy-symbol ,package source (error "~a is not loaded." ,(string package)))
                         :file source initargs)))
      (ecase type ;; static deferral. Not great, but can't do it otherwise with ASDF.
        (:mp3 (maybe-make-drain org.shirakumo.fraf.mixed.mpg123))
        (:wav (maybe-make-drain org.shirakumo.fraf.mixed.wav))
        (:flac (maybe-make-drain org.shirakumo.fraf.mixed.flac))))))

(defmethod initialize-instance :after ((voice voice) &key source effects repeat (on-end :free))
  (flet ((free (_) (declare (ignore _))
           (mixed:free voice))
         (disconnect (_) (declare (ignore _))
           (disconnect voice T)
           (mixed:withdraw voice T)))
    (let ((unpacker (allocate-unpacker *server*))
          (on-end (ecase on-end
                    (:free #'free)
                    (:disconnect #'disconnect))))
      (mixed:add (make-source-for source :pack (mixed:pack unpacker) :repeat repeat :on-end on-end) voice)
      (mixed:add unpacker voice)
      (loop for previous = unpacker then segment
            for segment in effects
            do (connect previous T segment T)))))

(defmethod mixed:free :before ((voice voice))
  (when (< 0 (length (mixed:segments voice)))
    (mixed:withdraw voice T)
    (disconnect voice T)))

(defmethod mixed:free :after ((voice voice))
  (when (< 0 (length (mixed:segments voice)))
    (mixed:free (source voice))
    (free-unpacker (mixed:unpacker voice) *server*)
    (loop for i from 2 below (length (mixed:segments voice))
          for segment = (aref (mixed:segments voice) i)
          do (disconnect segment T)
             (mixed:free segment))))

(defmethod source ((voice voice))
  (aref (mixed:segments voice) 0))

(defmethod mixed:unpacker ((voice voice))
  (aref (mixed:segments voice) 1))

(defmethod volume ((voice voice))
  (volume (mixed:unpacker voice)))

(defmethod (setf volume) (value (voice voice))
  (setf (volume (mixed:unpacker voice)) value))

(defun voice-end (voice)
  (aref (mixed:segments voice) (1- (length (mixed:segments voice)))))

(defmethod connect ((from voice) from-loc to to-loc)
  (connect (voice-end from) from-loc to to-loc))

(defmethod disconnect ((from voice) from-loc &key (direction :output))
  (unless (eq direction :output)
    (error "Cannot disconnect voice from input, as it does not have any."))
  (disconnect (voice-end from) from-loc :direction :output))

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

(defmethod location ((voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (mixed:input-location buffer (to buffer))))

(defmethod (setf location) (location (voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (setf (mixed:input-location buffer (to buffer)) location)))

(defmethod velocity ((voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (mixed:input-velocity buffer (to buffer))))

(defmethod (setf velocity) (velocity (voice voice))
  (let ((buffer (mixed:output 0 voice)))
    (setf (mixed:input-velocity buffer (to buffer)) location)))
