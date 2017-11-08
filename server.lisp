#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defvar *in-processing-thread* NIL)

(defclass server ()
  ((segment-map :initform NIL :accessor segment-map)
   (samples :initarg :samples :initform 0 :accessor samples)
   (buffersize :initarg :buffersize :initform 0 :accessor buffersize)
   (samplerate :initarg :samplerate :initform 0 :reader samplerate)
   (device :initarg :device :accessor device)
   (segment-sequence :initform NIL :accessor segment-sequence)
   (buffers :initform NIL :accessor buffers)
   (thread :initform NIL :accessor thread)
   ;; Synchronisation state
   (evaluation-queue-head :initform NIL :accessor evaluation-queue-head)
   (evaluation-queue-tail :initform NIL :accessor evaluation-queue-tail)
   (evaluation-lock :initform NIL :accessor evaluation-lock))
  (:default-initargs
   :buffersize 441
   :samplerate 44100))

(defmethod initialize-instance :after ((server server) &key samples buffersize samplerate)
  (check-type samples (or null (integer 1)))
  (check-type buffersize (or null (integer 1)))
  (check-type samplerate (integer 1))
  (setf (segment-map server) (make-hash-table :test 'eql))
  (unless buffersize (setf (slot-value server 'buffersize) (/ samplerate 100)))
  (if samples
      (when (< (buffersize server) samples)
        (error "Number of samples cannot be greater than the buffer size."))
      (setf (samples server) (buffersize server)))
  (let ((cons (cons T NIL)))
    (setf (evaluation-queue-head server) cons)
    (setf (evaluation-queue-tail server) cons))
  (setf (evaluation-lock server) (bt:make-lock (format NIL "~a evaluation lock." server))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type T)
    (format stream "~@[~*running~]" (thread server))))

(defmethod segment ((name symbol) (server server))
  (gethash name (segment-map server)))

(defmethod (setf segment) ((segment segment) (name symbol) (server server))
  (setf (gethash name (segment-map server)) segment))

(defmethod start ((server server))
  (when (started-p server)
    (error "~a is already running." server))
  (unless (device server)
    (error "No device has been assigned to ~a yet.~
            Did you compile a pipeline?" server))
  (unless (segment-sequence server)
    (error "No mixer object has been assigned to ~a yet.~
            Did you compile a pipeline?" server))
  (setf (thread server) T)
  (let ((thread (bt:make-thread (lambda ()
                                  (unwind-protect (run server)
                                    (setf (thread server) NIL)))
                                :name (format NIL "~a process thread." server)
                                :initial-bindings `((*standard-output* . ,*standard-output*)
                                                    (*error-output* . ,*error-output*)
                                                    (*query-io* . ,*query-io*)
                                                    (*trace-output* . ,*trace-output*)))))
    (setf (thread server) thread)))

(defmethod started-p ((server server))
  (not (null (thread server))))

(defmethod stop ((server server))
  (when (thread server)    
    (let ((thread (thread server)))
      (setf (thread server) NIL)
      (restart-case
          (loop for i from 0
                while (bt:thread-alive-p thread)
                do (sleep 0.1)
                   (when (= i 100)
                     (with-simple-restart (continue "Continue waiting.")
                       (error "~a's thread is not shutting down." server))))
        (abort ()
          :report "Attempt to forcibly terminate the thread."
          (bt:destroy-thread thread))
        (ignore ()
          :report "Return and ignore the running thread."))
      thread)))

(defmethod run ((server server))
  (let ((sequence (handle (segment-sequence server)))
        (device (device server))
        (evaluation-lock (evaluation-lock server))
        (evaluation-queue (evaluation-queue-head server)))
    (let ((*in-processing-thread* T))
      (cl-mixed:start (segment-sequence server))
      (unwind-protect
           (loop while (thread server)
                 do (with-simple-restart (continue "Continue with fingers crossed.")
                      (when (cdr evaluation-queue)
                        (bt:with-lock-held (evaluation-lock)
                          ;; Process first one and remove it from the queue.
                          (funcall (cadr evaluation-queue))
                          (setf (cdr evaluation-queue) (cddr evaluation-queue))
                          (unless (cdr evaluation-queue)
                            (setf (evaluation-queue-tail server) evaluation-queue)))
                        ;; Properties might have changed.
                        (setf sequence (handle (segment-sequence server)))
                        (setf device (device server)))
                      (cond ((paused-p device)
                             (sleep (/ (buffersize server)
                                       (samplerate server))))
                            (T
                             (cl-mixed-cffi:segment-sequence-mix (samples server) sequence)))))
        (cl-mixed:end (segment-sequence server))))))

(defun call-in-server-thread (function server &key synchronize timeout values)
  (flet ((push-function (function)
           (let ((cons (cons function NIL)))
             (setf (cdr (evaluation-queue-tail server)) cons)
             (setf (evaluation-queue-tail server) cons))))
    (cond ((not (thread server))
           (funcall function))
          ((and synchronize (not *in-processing-thread*))
           (let* ((lock (bt:make-lock))
                  (monitor (bt:make-condition-variable))
                  (values-list ()))
             (bt:with-lock-held (lock)
               (bt:with-lock-held ((evaluation-lock server))
                 (push-function (if values
                                    (lambda ()
                                      (unwind-protect
                                           (setf values-list (multiple-value-list (funcall function)))
                                        (bt:condition-notify monitor)))
                                    (lambda ()
                                      (unwind-protect
                                           (funcall function)
                                        (bt:condition-notify monitor))))))
               (bt:condition-wait monitor lock :timeout timeout)
               (values-list values-list))))
          (T
           (bt:with-lock-held ((evaluation-lock server))
             (push-function function))))))

(defmacro with-body-in-server-thread ((server &key synchronize timeout values) &body body)
  `(call-in-server-thread (lambda () ,@body) ,server :synchronize ,synchronize
                                                     :timeout ,timeout
                                                     :values ,values))

(defmethod paused-p ((server server))
  (paused-p (device server)))

(defmethod (setf paused-p) (value (server server))
  (setf (paused-p (device server)) value))

(defmethod pause ((server server))
  (pause (device server))
  server)

(defmethod resume ((server server))
  (resume (device server))
  server)

(defmethod (setf buffersize) :before (size (server server))
  (unless (= size (buffersize server))
    (loop for buffer across (buffers server)
          do (setf (cl-mixed:size buffer) size))))
