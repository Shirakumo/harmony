#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-mp3
  (:nicknames #:org.shirakumo.fraf.harmony.sources.mp3)
  (:use #:cl #:harmony)
  (:export
   #:mp3-source))
(in-package #:org.shirakumo.fraf.harmony.sources.mp3)

(define-source-type "mp3" mp3-source)

(defclass mp3-source (unpack-source file-source)
  ((mp3-file :initform NIL :accessor mp3-file)
   (channels :initarg :channels :accessor channels))
  (:default-initargs :channels 2))

(defmethod initialize-packed-audio ((source mp3-source))
  (let ((file (cl-mpg123:make-file (file source)
                                   :accepted-format (list (samplerate (server source))
                                                          (channels source)
                                                          :float))))
    (cl-mpg123:connect file)
    (setf (mp3-file source) file)
    (multiple-value-bind (rate channels encoding) (cl-mpg123:file-format file)
      (cl-mixed:make-packed-audio
       NIL
       (* (buffersize (server source))
          (cl-mixed:samplesize encoding)
          channels)
       encoding
       channels
       :alternating
       rate))))

(defmethod seek-to-sample ((source mp3-source) position)
  (cl-mpg123:seek (file source) position :mode :absolute :by :sample))

(defmethod sample-count ((source mp3-source))
  (cl-mpg123:frame-count (mp3-file source)))

(defmethod process ((source mp3-source) samples)
  (fill-for-unpack-source source samples #'cl-mpg123:read-directly (mp3-file source)))
