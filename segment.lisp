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
  ((name :initarg :name :initform NIL :reader name)))

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

(stealth-mixin:define-stealth-mixin source (segment) mixed:source
  ((repeat :initarg :repeat :initform 0 :accessor repeat)))

(defmethod (setf mixed:done-p) :around (value (source source))
  (case (repeat source)
    ((0 null)
     (call-next-method)
     (disconnect source T))
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

(defclass faucet (mixed:chain)
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

(defmethod initialize-instance :after ((faucet faucet) &key source segments)
  (let ((unpacker (allocate-unpacker *server*)))
    (mixed:add (make-source-for source :pack (mixed:pack unpacker)) faucet)
    (mixed:add unpacker faucet)
    (loop for previous = pack then segment
          for segment in segments
          do (connect previous T segment T))))

(defmethod mixed:free :after ((faucet faucet))
  (mixed:free (source faucet))
  (free-unpacker (mixed:unpacker faucet))
  (loop for i from 2 below (length (mixed:segments faucet))
        for segment = (aref (mixed:segments faucet) i)
        do (disconnect segment T)
           (mixed:free segment)))

(defmethod source ((faucet faucet))
  (aref (mixed:segments faucet) 0))

(defmethod mixed:unpacker ((faucet faucet))
  (aref (mixed:segments faucet) 1))

(defmethod mixed:volume ((faucet faucet))
  (mixed:volume (mixed:unpacker faucet)))

(defmethod (setf mixed:volume) (value (faucet faucet))
  (setf (mixed:volume (mixed:unpacker faucet)) value))

(defun faucet-end (faucet)
  (aref (mixed:segments faucet) (1- (length (mixed:segments faucet)))))

(defmethod mixed:outputs ((from faucet))
  (mixed:outputs (faucet-end from)))

(defmethod mixed:output (location (from faucet))
  (mixed:output location (faucet-end from)))

(defmethod connect ((from faucet) from-loc to to-loc)
  (connect (faucet-end from) from-loc to to-loc))

(defmethod disconnect ((from faucet) from-loc &key (direction :output))
  (unless (eq direction :output)
    (error "Cannot disconnect faucet from input, as it does not have any."))
  (disconnect (faucet-end from) from-loc :direction :output))
