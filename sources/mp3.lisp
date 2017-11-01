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

(defmethod process ((source mp3-source) samples)
  (let* ((file (mp3-file source))
         (pack (cl-mixed:packed-audio source))
         (buffer (cl-mixed:data pack))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding pack))
                   (cl-mixed:channels pack)))
         (read (cl-mpg123:read-directly file buffer bytes)))
    (when (< read bytes)
      (cond ((looping-p source)
             ;; We loop to catch corner cases where the file does not even fill a single
             ;; buffer. This is exceedingly unlikely to ever be the case, but w/e.
             (loop while (< read bytes)
                   do (cl-mpg123:seek file 0)
                      (let ((new-read (cl-mpg123:read-directly file buffer (- bytes read))))
                        (incf read new-read)
                        (setf (sample-position source) new-read))))
            (T
             ;; Make sure to pad out the rest of the buffer with zeroes.
             (loop for i from read below bytes
                   do (setf (cffi:mem-aref buffer :uint8) 0))
             (setf (ended-p source) T))))))
