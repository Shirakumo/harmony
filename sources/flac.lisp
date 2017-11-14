#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-flac
  (:nicknames #:org.shirakumo.fraf.harmony.sources.flac)
  (:use #:cl #:harmony)
  (:export
   #:flac-source))
(in-package #:org.shirakumo.fraf.harmony.sources.flac)

(define-source-type "flac" flac-source)

(defclass flac-source (unpack-source file-source)
  ((flac-file :initform NIL :accessor flac-file)
   (channels :initarg :channels :accessor channels))
  (:default-initargs :channels 2))

(defmethod initialize-packed-audio ((source flac-source))
  (let ((file (cl-flac:make-file (file source))))
    (setf (flac-file source) file)
    (cl-mixed:make-packed-audio
     NIL
     0
     :float
     (cl-flac:channels file)
     :alternating
     (cl-flac:samplerate file))))

(defmethod seek-to-sample ((source flac-source) position)
  (cl-flac:seek (flac-file source) position))

(defmethod sample-count ((source flac-source))
  (cl-flac:frame-count (flac-file source)))

(defmethod process ((source flac-source) samples)
  (fill-for-unpack-source source samples #'cl-flac:read-directly (flac-file source)))
