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

(define-source-type "wav" wav-source)

(defclass wav-source (unpack-source file-source)
  ((wav-stream :initform NIL :accessor wav-stream)
   (data-end :initform 0 :accessor data-end)
   (data-start :initform 44 :accessor data-start)
   (samplesize :initform 0 :accessor samplesize)))

(defun evenify (int)
  (if (evenp int)
      int
      (1+ int)))

(defun decode-int (stream size)
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

(defun decode-block (stream)
  (let ((start (file-position stream))
        (label (decode-label stream))
        (size (decode-int stream 4)))
    (list* :label label
           :start start
           :size size
           :end (+ start size)
           (cond ((string= label "fmt ")
                  (list :audio-format (decode-int stream 2)
                        :channels (decode-int stream 2)
                        :samplerate (decode-int stream 4)
                        :byterate (decode-int stream 4)
                        :block-align (decode-int stream 2)
                        :bits-per-sample (decode-int stream 2)))
                 (T
                  (file-position stream (evenify (+ start size)))
                  NIL)))))

(defun determine-sample-format (format)
  (case (getf format :audio-format)
    (1 (ecase (/ (getf format :bits-per-sample) 8)
         (1 :uint8)
         (2 :int16)
         (3 :int24)
         (4 :int32)))
    (3 :float)
    (T (error "Unsupported audio format (~d) in file." (getf format :audio-format)))))

(defun decode-wav-header (stream)
  (check-label stream "RIFF")
  (dotimes (i 4) (read-byte stream))
  (check-label stream "WAVE")
  (let* ((blocks (loop for block = (ignore-errors (decode-block stream))
                       while block collect block))
         (format (find "fmt " blocks :key #'second :test #'string=))
         (data (find "data" blocks :key #'second :test #'string=)))
    (unless format
      (error "Format block not found in RIFF file."))
    (unless data
      (error "Data block not found in RIFF file."))
    (file-position stream (getf data :start))
    (values (getf format :channels)
            (getf format :samplerate)
            (determine-sample-format format)
            (getf data :start)
            (getf data :size))))

(defmethod initialize-packed-audio ((source wav-source))
  (let ((stream (open (file source) :element-type '(unsigned-byte 8))))
    (unwind-protect
         (multiple-value-bind (channels samplerate sampleformat start size)
             (decode-wav-header stream)
           (setf (samplesize source) (cl-mixed-cffi:samplesize sampleformat))
           (setf (wav-stream source) stream)
           (setf (data-start source) start)
           (setf (data-end source) (+ start size))
           (cl-mixed:make-packed-audio
            NIL
            (* (buffersize (context source))
               (cl-mixed-cffi:samplesize sampleformat)
               channels)
            sampleformat
            channels
            :alternating
            samplerate))
      (unless (wav-stream source)
        (close stream)))))

(defmethod seek-to-sample ((source wav-source) position)
  (file-position (wav-stream source)
                 (+ (data-start source)
                    (min (* position (samplesize source))
                         (data-end source)))))

(defmethod sample-count ((source wav-source))
  (/ (- (data-end source) (data-start source))
     (cl-mixed:channels (packed-audio source))
     (samplesize source)))

(defun read-directly (source buffer bytes)
  (let* ((stream (wav-stream source))
         (bytes (min bytes (- (data-end source) (file-position stream))))
         (read-buffer (load-time-value (static-vectors:make-static-vector 4096)))
         (read-total 0))
    (loop for read = (read-sequence read-buffer stream :end (min (- bytes read-total) 4096))
          until (= 0 read)
          do (incf read-total read)
             (memcpy buffer (static-vectors:static-vector-pointer read-buffer) read))
    read-total))

(defmethod process ((source wav-source) samples)
  (fill-for-unpack-source source samples #'read-directly source))
