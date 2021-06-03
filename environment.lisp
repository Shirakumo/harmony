#|
 This file is a part of harmony
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defgeneric transition (thing to &key))

(defclass environment ()
  ((segments :initform #() :accessor segments)
   (segment-sets :initform (make-hash-table :test 'eql) :accessor segment-sets)
   (next-index :initform 0 :accessor next-index)
   (state :initform NIL :accessor state)))

(defmethod shared-initialize :after ((environment environment) slots &key (sets NIL sets-p))
  (when sets-p
    (clrhash (segment-sets environment))
    (let* ((sources (delete-duplicates (loop for set in sets append (rest set)) :test #'equal))
           (source-table (make-hash-table :test 'equal))
           (segments (make-array (length sources))))
      (loop for source in sources
            for i from 0
            for initargs = (if (listp source) source (list source))
            for segment = (apply #'create (first source)
                                 :class 'music-segment
                                 :mixer :music
                                 :on-end :call-track-end
                                 :if-exists :ignore
                                 :volume 0.0
                                 (rest source))
            do (setf (aref segments i) segment)
               (setf (environment segment) environment)
               (setf (gethash source source-table) segment))
      (setf (segments environment) segments)
      (loop for (state . tracks) in sets
            for arr = (make-array (length tracks))
            do (loop for source in tracks
                     for i from 0
                     do (setf (aref arr i) (gethash source source-table)))
               (setf (gethash state (segment-sets environment)) arr)))))

(defmethod active-p ((environment environment))
  (not (null (state environment))))

(defmethod mixed:end ((environment environment))
  (loop for segment across (segments environment)
        do (stop segment)))

(defmethod mixed:free ((environment environment))
  (loop for segment across (segments environment)
        do (mixed:free segment))
  (setf (segments environment) #())
  (clrhash (segment-sets environment)))

(defmethod (setf state) :before (state (environment environment))
  (unless (eql state (state environment))
    (let ((old (find T (segments environment) :key #'active-p))
          (new-set (or (gethash state (segment-sets environment)))))
      (setf (next-index environment) 0)
      (cond ((null state)
             (when old (transition old 0.0)))
            ((null new-set)
             (error "No segment set named~%  ~s~%in~%  ~s" state environment))
            (old
             (setf (pending-transition old) environment))
            (T
             (transition (aref new-set 0) 1.0))))))

(defclass music-segment (voice)
  ((fade-rate :initform 0.0 :accessor fade-rate)
   (target-fade :initform 0.0 :accessor target-fade)
   (pending-transition :initform NIL :accessor pending-transition)
   (transition-fun :initarg :transition-fun :initform (constantly T) :accessor transition-fun)
   (environment :initarg :environment :initform NIL :accessor environment)))

(defmethod initialize-instance :around ((segment music-segment) &rest args)
  (apply #'call-next-method segment :on-end :call-track-end args))

(defmethod shared-initialize :after ((segment music-segment) slots &key transition-points transition-interval (transition-offset 0))
  (when transition-points
    (let ((points (make-array (length transition-points) :element-type '(unsigned-byte 32) :initial-contents transition-points)))
      (setf (transition-fun segment)
            (lambda (old new)
              (declare (type (unsigned-byte 32) old new))
              (declare (optimize speed))
              (loop for point across (the (simple-array (unsigned-byte 32) (*)) points)
                    while (<= point new)
                    do (when (or (< new old) (< old point))
                         (return T)))))))
  (when transition-interval
    (locally (declare (type (unsigned-byte 32) transition-interval transition-offset))
      (setf (transition-fun segment)
            (lambda (old new)
              (declare (type (unsigned-byte 32) old new))
              (declare (optimize speed))
              (and (<= transition-offset new)
                   (/= (floor old transition-interval)
                       (floor new transition-interval))))))))

(defmethod frame-change ((segment music-segment) old new)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) old new))
  (let ((transition (pending-transition segment)))
    (when transition
      (when (funcall (the function (transition-fun segment)) old new)
        (setf (pending-transition segment) NIL)
        (transition segment transition))))
  (let ((rate (fade-rate segment))
        (samples (- new old)))
    (declare (type single-float rate))
    (when (and (/= 0.0 rate) (< 0 samples))
      (let* ((diff (* samples rate))
             (target (target-fade segment)))
        (declare (type single-float target diff))
        (if (< 0.0 rate)
            (when (<= target (incf (mixed:volume segment) diff))
              (setf (mixed:volume segment) target)
              (setf (fade-rate segment) 0.0))
            (when (<= (incf (mixed:volume segment) diff) target)
              (setf (mixed:volume segment) target)
              (setf (fade-rate segment) 0.0)
              (when (= 0.0 target)
                (stop segment))))))))

(defmethod track-end ((segment music-segment) source)
  (let ((env (environment segment)))
    (if env
        (transition segment env :sync NIL)
        (mixed:seek source (repeat-start source) :by :second))))

(declaim (inline %sync))
(defun %sync (thing with)
  ;; IMPORTANT: we **have** to manually bypass mixed:seek heer in order to
  ;;            avoid triggering FRAME-CHANGE
  (let ((position (mixed:frame-position with)))
    (mixed:seek-to-frame thing position)
    (setf (slot-value thing 'mixed:frame-position) position)))

(defmethod transition ((segment music-segment) (environment environment) &key (sync T))
  (transition segment 0.0)
  (when (state environment)
    (let* ((index (next-index environment))
           (set (gethash (state environment) (segment-sets environment)))
           (next (aref set index)))
      (transition next 1.0)
      (when sync (%sync next segment))
      (setf (next-index environment) (mod (1+ index) (length set))))))

(defmethod transition ((segment music-segment) (to real) &key (in 1.0))
  (unless (active-p segment)
    (play segment :mixer :music)
    (setf (mixed:volume segment) 0.0))
  (let* ((samples-per-step (round (* (mixed:samplerate segment) in)))
         (rate (/ (- to (mixed:volume segment)) samples-per-step)))
    (setf (target-fade segment) (float to 1f0))
    (setf (fade-rate segment) (float rate 1f0))
    segment))

(defmethod transition ((from music-segment) (to music-segment) &key (in 1.0) (volume 1.0) sync)
  (when sync (%sync to from))
  (transition from 0.0 :in in)
  (transition to volume :in in)
  to)
