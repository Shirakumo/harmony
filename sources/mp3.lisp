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

(defclass mp3-source (source)
  ((file :initform NIL :accessor file)
   (source-file :initarg :file :accessor source-file)
   (channels :initarg :channels :accessor channels)
   (samplesize :initform NIL :accessor samplesize)))

(defmethod initialize-instance :after ((source mp3-source) &key)
  (setf (decoder source) #'decode))

(defmethod initialize-channel ((source mp3-source))
  (let ((file (cl-mpg123:make-file (source-file source)
                                   :accepted-format (list (samplerate (server source))
                                                          (channels source)
                                                          :float))))
    (cl-mpg123:connect file)
    (setf (file source) file)
    (multiple-value-bind (rate channels encoding) (cl-mpg123:file-format file)
      (cl-mixed:make-channel NIL
                             (* (buffersize (server source))
                                (cl-mixed:samplesize encoding)
                                channels)
                             encoding
                             channels
                             :alternating
                             rate))))

(defmethod seek-to-sample ((source mp3-source) position)
  (cl-mpg123:seek (file source) position :mode :absolute :by :sample))

(defun decode (samples source)
  (let* ((file (file source))
         (channel (cl-mixed:channel source))
         (buffer (cl-mixed:data channel))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding channel))
                   (cl-mixed:channels channel)))
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
