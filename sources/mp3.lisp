#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)
(defpackage #:org.shirakumo.fraf.harmony.sources.mp3
  (:nicknames #:harmony-mp3)
  (:use #:cl #:harmony)
  (:export
   ))

(defclass mp3-source (cl-mixed:virtual source)
  ((file :initform NIL :accessor file)
   (source :initform NIL :accessor source)
   (samplesize :initform NIL :accessor samplesize)))

(defmethod initialize-instance :after ((mp3 mp3-source) &key file)
  (let ((file (cl-mpg123:make-file file :accepted-format '(44100 2 :float))))
    (cl-mpg123:connect file)
    (setf (cl-mixed-cffi:direct-segment-mix (cl-mixed:handle mp3)) (cffi:callback mix))))

(defmethod (setf cl-mixed:output-field) ((value buffer) (field (eql :buffer)) location (mp3 mp3-source))
  (cl-mixed-cffi:segment-set-out field location (cl-mixed:handle value) (source mp3)))

(defmethod cl-mixed:info ((mp3 mp3-source))
  (list :name "mp3-source"
        :description "Playback from an MP3 file."
        :flags ()
        :min-inputs 0
        :max-inputs 0
        :outputs (cl-mpg123:channels (file mp3))
        :fields ()))

(cffi:defcallback mix :void ((samples cl-mixed-cffi:size_t) (segment :pointer))
  (let* ((mp3 (cl-mixed:pointer->object segment))
         (file (file mp3))
         (source (source mp3))
         (buffer (cl-mixed-cffi:channel-data source)))
    (let* ((samples (* samples (samplesize mp3)))
           (read (cl-mpg123:read-directly file buffer samples)))
      (when (< read samples)
        (cond ((looping-p mp3)
               ;; We loop to catch corner cases where the file does not even fill a single
               ;; buffer. This is exceedingly unlikely to ever be the case, but w/e.
               (loop while (< read samples)
                     do (cl-mpg123:seek file 0)
                        (incf read (cl-mpg123:read-directly file buffer (- samples read)))))
              (T
               ;; Make sure to pad out the rest of the buffer with zeroes.
               (loop for i from read below samples
                     do (setf (cffi:mem-aref buffer :uint8) 0))
               (stop mp3)))))
    (cl-mixed-cffi:segment-mix samples source)))
