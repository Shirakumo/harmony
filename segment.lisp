#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(stealth-mixin:define-stealth-mixin buffer () mixed:buffer
  ((from :initform NIL :accessor from)
   (from-location :initform NIL :accessor from-location)
   (to :initform NIL :accessor to)
   (to-location :initform NIL :accessor to-location)))

(stealth-mixin:define-stealth-mixin segment () mixed:segment
  ((name :initarg :name :initform NIL :reader name)
   (chain :initform NIL :accessor chain)))

(defmethod (setf mixed:output-field) :after ((buffer buffer) (field (eql :buffer)) (location integer) (segment segment))
  (setf (from buffer) segment)
  (setf (from-location buffer) location))

(defmethod (setf mixed:output-field) :after ((buffer buffer) (field (eql :pack)) (location integer) (segment segment))
  (setf (from buffer) segment)
  (setf (from-location buffer) location))

(defmethod (setf mixed:input-field) :after ((buffer buffer) (field (eql :buffer)) (location integer) (segment segment))
  (setf (to buffer) segment)
  (setf (to-location buffer) location))

(defmethod (setf mixed:input-field) :after ((buffer buffer) (field (eql :pack)) (location integer) (segment segment))
  (setf (to buffer) segment)
  (setf (to-location buffer) location))

(defmethod connect ((from segment) from-loc (to segment) to-loc)
  (let ((buffer (allocate-buffer *server*)))
    (mixed:connect from from-loc to to-loc buffer)))

(defmethod connect ((from segment) (all (eql T)) (to segment) (_all (eql T)))
  (loop for i from 0 below (getf (info from) :outputs)
        do (connect from i to i *server*)))

(defmethod disconnect ((from segment) from-loc &key (direction :output))
  (let ((buffer (ecase direction
                  (:output (mixed:output from from-loc))
                  (:input (mixed:input from from-loc)))))
    (setf (mixed:output (from buffer) (from-location buffer)) NIL)
    (setf (mixed:input (to buffer) (to-location buffer)) NIL)
    (free-buffer buffer *server*)))

(defmethod disconnect ((from segment) (all (eql T)) &key (direction :output))
  (loop for i from 0 below (ecase direction
                             (:output (getf (info from) :outputs))
                             (:input (getf (info from) :inputs)))
        do (disconnect from i :direction direction)))

(defmethod mixed:add :after ((segment segment) (chain mixed:chain))
  (setf (chain segment) chain))

(defmethod mixed:withdraw :after ((segment segment) (chain mixed:chain))
  (when (eql (chain segment) chain)
    (setf (chain segment) NIL)))

(defmethod mixed:withdraw ((segment segment) (chain (eql T)))
  (when (chain segment)
    (mixed:withdraw segment (chain segment))))

(defun default-source-end (source)
  (disconnect source T))

(stealth-mixin:define-stealth-mixin source (segment) mixed:source
  ((repeat :initarg :repeat :initform 0 :accessor repeat)
   (on-end :initarg :on-end :initform #'default-source-end :accessor on-end)))

(defmethod (setf mixed:done-p) :around (value (source source))
  (case (repeat source)
    ((0 null)
     (call-next-method)
     (funcall (on-end source) source))
    ((T)
     (mixed:seek source 0))
    (T
     (mixed:seek source 0)
     (decf (repeat source))))
  value)

(defmethod mixed:unpacker ((source source))
  (to (mixed:pack source)))

;;; Always delegate to pack, since we never want to interfere between
;;; a source and its pack
(defmethod connect ((from source) from-loc (to segment) to-loc)
  (connect (mixed:unpacker from) from-loc to to-loc))

(defmethod disconnect ((from source) from-loc &key (direction :output))
  (disconnect (mixed:unpacker from) from-loc :direction direction))

(defmethod mixed:volume ((source source))
  (mixed:volume (mixed:unpacker source)))

(defmethod (setf mixed:volume) (value (source source))
  (setf (mixed:volume (mixed:unpacker source)) value))

(defclass source-chain (mixed:chain)
  ())

(defgeneric make-source-for (source &rest initargs)
  (:method ((source pathname) &rest initargs)
    (if (pathname-type source)
        (apply #'make-source-for-path-type source (intern (string-upcase (pathname-type source)) "KEYWORD") initargs)
        (error "Pathname has no type:~%  ~a" source))))

(defgeneric make-source-for-path-type (pathname type &rest initargs)
  (:method (source type &rest initargs)
    (macrolet ((maybe-make-drain (type)
                 `(apply #'make-instance (lazy-symbol ,package drain (error "~a is not loaded." ,(string package)))
                         :file pathname initargs))))
    (ecase type
      (:mp3 (maybe-make-drain org.shirakumo.fraf.mixed.mpg123))
      (:wav (maybe-make-drain org.shirakumo.fraf.mixed.wav))
      (:flac (maybe-make-drain org.shirakumo.fraf.mixed.flac)))))

(defmethod initialize-instance :after ((source-chain source-chain) &key source segments loop (on-end :free))
  (flet ((free (_) (declare (ignore _))
           (mixed:free source-chain))
         (disconnect (_) (declare (ignore _))
           (disconnect source-chain T)
           (mixed:withdraw source-chain T)))
    (let ((unpacker (allocate-unpacker *server*))
          (on-end (ecase on-end
                    (:free #'free)
                    (:disconnect #'disconnect))))
      (mixed:add (make-source-for source :pack (mixed:pack unpacker) :loop loop :on-end on-end) source-chain)
      (mixed:add unpacker source-chain)
      (loop for previous = pack then segment
            for segment in segments
            do (connect previous T segment T)))))

(defmethod mixed:free :before ((source-chain source-chain))
  (mixed:withdraw source-chain T)
  (mixed:disconnect source-chain T))

(defmethod mixed:free :after ((source-chain source-chain))
  (mixed:free (source source-chain))
  (free-unpacker (mixed:unpacker source-chain))
  (loop for i from 2 below (length (mixed:segments source-chain))
        for segment = (aref (mixed:segments source-chain) i)
        do (disconnect segment T)
           (mixed:free segment)))

(defmethod source ((source-chain source-chain))
  (aref (mixed:segments source-chain) 0))

(defmethod mixed:unpacker ((source-chain source-chain))
  (aref (mixed:segments source-chain) 1))

(defmethod mixed:volume ((source-chain source-chain))
  (mixed:volume (mixed:unpacker source-chain)))

(defmethod (setf mixed:volume) (value (source-chain source-chain))
  (setf (mixed:volume (mixed:unpacker source-chain)) value))

(defun source-chain-end (source-chain)
  (aref (mixed:segments source-chain) (1- (length (mixed:segments source-chain)))))

(defmethod mixed:outputs ((from source-chain))
  (mixed:outputs (source-chain-end from)))

(defmethod mixed:output (location (from source-chain))
  (mixed:output location (source-chain-end from)))

(defmethod connect ((from source-chain) from-loc to to-loc)
  (connect (source-chain-end from) from-loc to to-loc))

(defmethod disconnect ((from source-chain) from-loc &key (direction :output))
  (unless (eq direction :output)
    (error "Cannot disconnect source-chain from input, as it does not have any."))
  (disconnect (source-chain-end from) from-loc :direction :output))
