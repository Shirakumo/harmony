#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(stealth-mixin:define-stealth-mixin buffer () mixed::bip-buffer
  ((from :initform NIL :accessor from)
   (from-location :initform NIL :accessor from-location)
   (to :initform NIL :accessor to)
   (to-location :initform NIL :accessor to-location)))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type T)
    (format stream "~a <-> ~a" (from buffer) (to buffer))))

(stealth-mixin:define-stealth-mixin segment () mixed:segment
  ((name :initarg :name :initform NIL :reader name)
   (chain :initform NIL :accessor chain)))

(defmethod print-object ((segment segment) stream)
  (print-unreadable-object (segment stream :type T)
    (format stream "~@[~s~]" (name segment))))

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
  (loop for i from 0 below (getf (mixed:info from) :outputs)
        do (connect from i to i)))

(defmethod connect ((from segment) (all (eql T)) (to mixed:basic-mixer) (_all (eql T)))
  (loop for i from 0 below (getf (mixed:info from) :outputs)
        for j from (length (mixed:inputs to))
        do (connect from i to j)))

(defmethod connect ((from segment) (all (eql T)) (to mixed:space-mixer) (_all (eql T)))
  (when (< 1 (getf (mixed:info from) :outputs))
    (error "Cannot connect a segment with more than one output to a space mixer; dangling buffers."))
  (connect from 0 to (1+ (length (mixed:inputs to)))))

(defmethod disconnect ((from segment) from-loc &key (direction :output))
  (let ((buffer (ecase direction
                  (:output (mixed:output from from-loc))
                  (:input (mixed:input from from-loc)))))
    (setf (mixed:output (from buffer) (from-location buffer)) NIL)
    (setf (mixed:input (to buffer) (to-location buffer)) NIL)
    (free-buffer buffer *server*)))

(defmethod disconnect ((from segment) (all (eql T)) &key (direction :output))
  (loop for i from 0 below (ecase direction
                             (:output (getf (mixed:info from) :outputs))
                             (:input (getf (mixed:info from) :inputs)))
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

(defmethod downstream ((segment segment) index)
  (to (mixed:output index segment)))

(defmethod upstream ((segment segment) index)
  (from (mixed:input index segment)))

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

(defmethod volume ((source source))
  (volume (mixed:unpacker source)))

(defmethod (setf volume) (value (source source))
  (setf (volume (mixed:unpacker source)) value))
