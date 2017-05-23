#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass server ()
  ((buffersize :initarg :buffersize :accessor buffersize)
   (samplerate :initarg :samplerate :accessor samplerate)
   (device :initform NIL :accessor device)
   (channel :initform NIL :accessor channel)
   (mixer :initform NIL :accessor mixer)
   (buffers :initform NIL :accessor buffers)
   (thread :initform NIL :accessor thread)
   ;; Synchronisation state
   (access-wanted-p :initform NIL :accessor access-wanted-p)
   (access-lock :initform NIL :accessor access-lock)
   (process-lock :initform NIL :accessor process-lock)
   (process-monitor :initform NIL :accessor process-monitor)
   (request-lock :initform NIL :accessor request-lock)
   (request-monitor :initform NIL :accessor request-monitor))
  (:default-initargs
   :buffersize 441
   :samplerate 44100))

(defmethod initialize-instance :after ((server server) &key driver)
  (check-type (buffersize server) (integer 1))
  (check-type (samplerate server) (integer 1))
  (setf (device server) (cl-out123:make-output driver))
  (setf (channel server) (cl-mixed:make-channel NIL (* 4 (buffersize server)) :float 2 :alternating (samplerate server)))
  (setf (mixer server) (cl-mixed:make-mixer))
  (setf (access-lock server) (bt:make-lock (format NIL "~a access lock." server)))
  (setf (process-lock server) (bt:make-lock (format NIL "~a process lock." server)))
  (setf (request-lock server) (bt:make-lock (format NIL "~a request lock." server)))
  (setf (process-monitor server) (bt:make-condition-variable :name (format NIL "~a process monitor." server)))
  (setf (request-monitor server) (bt:make-condition-variable :name (format NIL "~a request monitor." server))))

(defmethod start (server)
  (when (thread server)
    (error "~a is already running." server))
  (setf (thread server) T)
  (let ((thread (bt:make-thread (lambda () (process server)))))
    (setf (thread server) thread)
    thread))

(defmethod stop (server)
  (unless (thread server)
    (error "~a is not running." server))
  (let ((thread (thread server)))
    (setf (thread server) NIL)
    (restart-case
        (loop for i from 0
              while (bt:thread-alive-p thread)
              do (sleep 0.01)
                 (when (= i 100)
                   (with-simple-restart (continue "Continue waiting.")
                     (error "~a's thread is not shutting down." server))))
      (abort ()
        :report "Attempt to forcibly terminate the thread."
        (bt:destroy-thread thread))
      (ignore ()
        :report "Return and ignore the running thread."))
    thread))

(defmethod process ((server server))
  (let ((mixer (mixer server))
        (device (device server))
        (channel (channel server))
        (buffer (cl-mixed:data (channel server)))
        (samples (buffersize server))
        (process-lock (process-lock server))
        (process-monitor (process-monitor server))
        (request-lock (request-lock server))
        (request-monitor (request-monitor server)))
    (cl-out123:connect device)
    (cl-out123:start device :rate (cl-mixed:samplerate channel)
                            :channels (cl-mixed:channels channel)
                            :encoding (cl-mixed:encoding channel))
    (cl-mixed:start mixer)
    (let ((bytes (* samples (cl-mixed:samplesize (cl-out123:encoding device)))))
      (bt:with-lock-held (process-lock)
        (unwind-protect
             (loop while (thread server)
                   do (when (access-wanted-p server)
                        (loop while (access-wanted-p server)
                              do (bt:condition-notify request-monitor)
                                 (bt:condition-wait (process-monitor) (process-lock))))
                      (cl-mixed:mix mixer samples)
                      (cl-out123:play-directly device buffer bytes))
          (cl-mixed:end mixer)
          (cl-out123:stop out)
          (cl-out123:disconnect out))))))

(defun call-with-server-lock (function server &key timeout)
  (bt:with-lock-held ((access-lock server))
    (setf (access-wanted-p server) T)
    (unwind-protect
         (bt:with-lock-held ((request-lock server))
           (unless (bt:condition-wait (request-monitor server) (request-lock server) :timeout timeout)
             (unwind-protect
                  (bt:with-lock-held ((process-lock server))
                    (funcall function))
               (bt:condition-notify (process-monitor)))))
      (setf (access-wanted-p server) NIL))))

(defmacro with-server-lock ((server &key timeout) &body body)
  `(call-with-server-lock (lambda () ,@body) ,server :timeout ,timeout))

(defclass pipeline ()
  (nodes))

(defgeneric compile-pipeline (pipeline server))
