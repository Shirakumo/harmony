#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:harmony-wav
  (:nicknames #:org.shirakumo.fraf.harmony.sources.wav)
  (:use #:cl #:harmony)
  (:export
   #:wav-source))
(in-package #:org.shirakumo.fraf.harmony.sources.wav)

(cffi:defcfun (memcpy "memcpy") :pointer
  (dest :pointer)
  (source :pointer)
  (num cl-mixed-cffi:size_t))

(define-source-type "wav" wav-source)

(defclass wav-source (unpack-source file-source)
  ((wav-stream :initform NIL :accessor wav-stream)
   (samplesize :initform 0 :accessor samplesize)))

(defun decode-int (stream &optional (size 4))
  (let ((int 0))
    (dotimes (i size int)
      (setf (ldb (byte 8 (* 8 i)) int) (read-byte stream)))))

(defun decode-label (stream)
  (map-into (make-string 4) (lambda () (code-char (read-byte stream)))))

(defun check-label (stream label)
  (let ((found (decode-label stream)))
    (unless (string= found label)
      (error "Not a valid RIFF file: encountered ~s instead of ~s."
             found label))))

(defun decode-wav-header (stream)
  (check-label stream "RIFF")
  (dotimes (i 4) (read-byte stream))
  (check-label stream "WAVE")
  (check-label stream "fmt ")
  (dotimes (i 4) (read-byte stream))
  (let ((audio-format (decode-int stream 2))
        (channels (decode-int stream 2))
        (samplerate (decode-int stream 4))
        (byterate (decode-int stream 4))
        (block-align (decode-int stream 2))
        (bits-per-sample (decode-int stream 2)))
    (declare (ignore byterate block-align))
    (unless (= 1 audio-format)
      (error "Unsupported audio format (~d) in file." audio-format))
    (check-label stream "data")
    (dotimes (i 4) (read-byte stream))
    (values channels samplerate (/ bits-per-sample 8))))

(defmethod initialize-packed-audio ((source wav-source))
  (let ((stream (open (file source) :element-type '(unsigned-byte 8))))
    (unwind-protect
         (multiple-value-bind (channels samplerate samplesize)
             (decode-wav-header stream)
           (setf (samplesize source) samplesize)
           (setf (wav-stream source) stream)
           (cl-mixed:make-packed-audio
            NIL
            (* (buffersize (server source))
               samplesize
               channels)
            (ecase samplesize
              (1 :uint8)
              (2 :int16))
            channels
            :alternating
            samplerate))
      (unless (wav-stream source)
        (close stream)))))

(defmethod seek-to-sample ((source wav-source) position)
  (file-position (wav-stream source)
                 (+ 44 (* position (samplesize source)))))

(defun read-directly (stream buffer bytes)
  (let ((read-buffer (load-time-value (static-vectors:make-static-vector 4096)))
        (read-total 0))
    (loop for read = (read-sequence read-buffer stream :end (min (- bytes read-total) 4096))
          until (= 0 read)
          do (incf read-total read)
             (memcpy buffer (static-vectors:static-vector-pointer read-buffer) read))
    read-total))

(defmethod process ((source wav-source) samples)
  (fill-for-unpack-source source samples #'read-directly (wav-stream source)))
