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
  (print-unreadable-object (buffer stream :type T :identity T)
    (format stream "~a <-> ~a" (from buffer) (to buffer))))

(stealth-mixin:define-stealth-mixin segment () mixed:segment
  ((name :initarg :name :initform NIL :reader name)
   (chain :initform NIL :accessor chain)))

(defmethod print-object ((segment segment) stream)
  (print-unreadable-object (segment stream :type T :identity (null (name segment)))
    (format stream "~@[~s~]" (name segment))))

(defmethod (setf mixed:pack) :after ((buffer buffer) (segment segment))
  (typecase segment
    ((or mixed:source mixed:packer)
     (setf (from buffer) segment))
    ((or mixed:drain mixed:unpacker)
     (setf (to buffer) segment))))

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
  (let ((buffer (or (mixed:output from-loc from) (allocate-buffer *server*))))
    (mixed:connect from from-loc to to-loc buffer)))

(defmethod connect ((from segment) (all (eql T)) (to segment) (_all (eql T)))
  (loop for i from 0 below (getf (mixed:info from) :outputs)
        do (connect from i to i)))

(defmethod connect ((from segment) (all (eql T)) (to mixed:basic-mixer) (_all (eql T)))
  (loop for i from 0 below (getf (mixed:info from) :outputs)
        do (connect from i to T)))

(defmethod connect ((from segment) (all (eql T)) (to mixed:space-mixer) (_all (eql T)))
  (when (< 1 (getf (mixed:info from) :outputs))
    (error "Cannot connect a segment with more than one output to a space mixer; dangling buffers."))
  (connect from 0 to T))

(defmethod connect ((from segment) (all (eql T)) (to mixed:plane-mixer) (_all (eql T)))
  (when (< 1 (getf (mixed:info from) :outputs))
    (error "Cannot connect a segment with more than one output to a space mixer; dangling buffers."))
  (connect from 0 to T))

(defmethod disconnect ((from segment) from-loc &key (direction :output))
  (let ((buffer (ecase direction
                  (:output (mixed:output from-loc from))
                  (:input (mixed:input from-loc from)))))
    (when buffer
      (when (from buffer)
        (ignore-errors (setf (mixed:output (from-location buffer) (from buffer)) NIL)))
      (when (to buffer)
        (ignore-errors (setf (mixed:input (to-location buffer) (to buffer)) NIL)))
      (free-buffer buffer *server*))))

(defmethod disconnect ((from segment) (all (eql T)) &key (direction :output))
  (when (mixed:handle from)
    (loop for i from 0 below (ecase direction
                               (:output (getf (mixed:info from) :outputs))
                               (:input (getf (mixed:info from) :inputs)))
          do (disconnect from i :direction direction))))

(defmethod mixed:add :before ((segment segment) (chain mixed:chain))
  (when (chain segment)
    (cerror "Do it anyway." "Segment~%  ~a~%is already present on chain~%  ~a~%cannot add it to~%  ~a"
           segment (chain segment) chain)))

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
   (repeat-start :initarg :repeat-start :initform 0 :accessor repeat-start)
   (on-end :initarg :on-end :initform #'default-source-end :accessor on-end)
   (on-frame-change :initarg :on-frame-change :initform (constantly nil) :accessor on-frame-change)))

(defmethod (setf mixed:done-p) :around (value (source source))
  (if value
      (case (repeat source)
        ((0 NIL)
         (call-next-method)
         (funcall (on-end source) source))
        ((T)
         (mixed:seek source (repeat-start source) :by :second))
        (T
         (mixed:seek source (repeat-start source) :by :second)
         (decf (repeat source))))
      (call-next-method))
  value)

(defmethod (setf mixed:frame-position) :before (new (source source))
  (funcall (on-frame-change source) source new))

(defmethod mixed:unpacker ((source source))
  (to (mixed:pack source)))

;;; Always delegate to pack, since we never want to interfere between
;;; a source and its pack
(defmethod connect ((from source) from-loc (to segment) to-loc)
  (connect (mixed:unpacker from) from-loc to to-loc))

(defmethod disconnect ((from source) from-loc &key (direction :output))
  (when (mixed:unpacker from)
    (disconnect (mixed:unpacker from) from-loc :direction direction)))

(defmethod mixed:volume ((source source))
  (mixed:volume (mixed:unpacker source)))

(defmethod (setf mixed:volume) (value (source source))
  (setf (mixed:volume (mixed:unpacker source)) value))
