#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(in-package #:org.shirakumo.fraf.harmony)

(defclass buffer-source (source)
  ((data :initarg :data :accessor data)
   (size :initarg :size :accessor size)
   (channelargs :initform NIL :accessor channelargs))
  (:default-initargs
   :data (error "DATA required.")
   :size (error "SIZE required.")
   :encoding :float
   :channels 2
   :layout :alternating))

(defmethod initialize-instance :before ((source buffer-source) &key encoding channels layout samplerate source-samplerate)
  (setf (decoder source) #'decode-buffer)
  (unless source-samplerate
    (setf source-samplerate samplerate))
  (setf (channelargs source) (list encoding
                                   channels
                                   layout
                                   source-samplerate)))

(defmethod initialize-channel ((source buffer-source))
  (destructuring-bind (encoding channels layout source-samplerate)
      (channelargs source)
    (apply #'cl-mixed:make-channel
           NIL (* (buffersize (server source))
                  (cl-mixed:samplesize encoding)
                  channels)
           encoding channels layout source-samplerate)))

(defmethod seek-to-sample ((source buffer-source) position))

(cffi:defcfun (memcpy "memcpy") :pointer
  (dest :pointer)
  (source :pointer)
  (num cl-mixed-cffi:size_t))

(defun decode-buffer (samples source)
  (let* ((data (data source))
         (size (size source))
         (channel (cl-mixed:channel source))
         (pos (* (sample-position source)
                 (cl-mixed:samplesize (cl-mixed:encoding channel))
                 (cl-mixed:channels channel)))
         (buffer (cl-mixed:data channel))
         (bytes (* samples
                   (cl-mixed:samplesize (cl-mixed:encoding channel))
                   (cl-mixed:channels channel)))
         (read (min bytes (- size pos))))
    (memcpy buffer (cffi:inc-pointer data pos) read)
    (when (< read bytes)
      (cond ((looping-p source)
             (loop while (< 0 bytes)
                   for offset from (+ pos read) by read
                   do (decf bytes read)
                      (setf (sample-position source) 0)
                      (memcpy (cffi:inc-pointer buffer (- offset pos))
                              (cffi:inc-pointer data offset) read)))
            (T
             (loop for i from read below bytes
                   do (setf (cffi:mem-aref buffer :uchar i) 0))
             (setf (ended-p source) T))))))
