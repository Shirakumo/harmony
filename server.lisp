#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defvar *in-processing-thread* NIL)
(defvar *server* NIL)

(defclass server (mixed:chain)
  ((segment-map :initform (make-hash-table :test 'eql) :reader segment-map)
   (free-buffers :initform () :accessor free-buffers)
   (free-unpackers :initform () :accessor free-unpackers)
   (thread :initform NIL :accessor thread)
   (queue :initform (make-array 32 :element-type T) :reader queue)
   (samplerate :initform 48000 :initarg :samplerate :accessor samplerate)
   (buffersize :initform NIL :initarg :buffersize :accessor buffersize)
   (name :initform "Harmony")))

(defmethod initialize-instance :after ((server server) &key)
  (setf *server* server)
  (unless (buffersize server)
    (setf (buffersize server) (floor (samplerate server) 100))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type T)
    (format stream "~s~@[~* running~]" (name server) (started-p server))))

(defmethod allocate-buffer ((server server))
  (or (pop* (slot-value server 'free-buffers))
      (mixed:make-buffer (buffersize server))))

(defmethod allocate-unpacker ((server server))
  (or (pop* (slot-value server 'free-unpackers))
      (mixed:make-unpacker :frames (buffersize server) :samplerate (samplerate server))))

(defmethod free-buffer (buffer (server server))
  (setf (from buffer) NIL)
  (setf (to buffer) NIL)
  (push* buffer (slot-value server 'free-buffers)))

(defmethod free-unpacker (unpacker (server server))
  (disconnect unpacker T)
  (push* unpacker (slot-value server 'free-unpackers)))

(defmethod segment (name (server (eql T)))
  (segment name *server*))

(defmethod segment ((name symbol) (server server))
  (or (gethash name (segment-map server))
      (error "No such segment ~s" name)))

(defmethod (setf segment) (segment name (server (eql T)))
  (setf (segment name *server*) segment))

(defmethod (setf segment) ((segment mixed:segment) (name symbol) (server server))
  (setf (gethash name (segment-map server)) segment))

(defmethod (setf segment) ((null null) (name symbol) (server server))
  (remhash name (segment-map server))
  NIL)

(defmethod mixed:free :before ((server server))
  (mixed:end server))

(defmethod mixed:free :after ((server server))
  (labels ((rec (chain)
             (loop for segment across (mixed:segments chain)
                   do (when (typep segment 'mixed:chain)
                        (rec segment))
                      (mixed:free segment))))
    (rec server))
  (clrhash (segment-map server))
  (loop for buffer = (pop (free-buffers server))
        while buffer do (mixed:free buffer))
  (loop for unpacker = (pop (free-unpackers server))
        while unpacker do (mixed:free unpacker)))

(defmethod mixed:add :after ((segment mixed:segment) (server server))
  (when (name segment)
    (setf (segment (name segment) server) segment)))

(defmethod mixed:withdraw :after ((segment mixed:segment) (server server))
  (when (name segment)
    (setf (segment (name segment) server) NIL)))

(defmethod mixed:start :before ((server server))
  (when (started-p server)
    (error "~a is already running." server)))

(defmethod mixed:start ((server server))
  (call-next-method)
  (setf (thread server) T)
  (let ((thread (bt:make-thread (lambda () (run server))
                                :name (format NIL "~a processing thread" server)
                                :initial-bindings `((*standard-output* . ,*standard-output*)
                                                    (*error-output* . ,*error-output*)
                                                    (*query-io* . ,*query-io*)
                                                    (*trace-output* . ,*trace-output*)))))
    (setf (thread server) thread))
  server)

(defmethod volume ((name symbol))
  (volume (segment name *server*)))

(defmethod (setf volume) (value (name symbol))
  (setf (volume (segment name *server*)) value))

(defmethod location ((name symbol))
  (location (segment name *server*)))

(defmethod (setf location) (location (name symbol))
  (setf (location (segment name *server*)) location))

(defmethod velocity ((name symbol))
  (velocity (segment name *server*)))

(defmethod (setf velocity) (velocity (name symbol))
  (setf (velocity (segment name *server*)) velocity))

(defmethod started-p ((server server))
  (and (thread server)
       (or (null (bt:threadp (thread server)))
           (bt:thread-alive-p (thread server)))))

(defmethod mixed:end ((server server))
  (when (thread server)
    ;; Clear the queue
    (setf (svref (queue server) 0) 0)
    (fill (queue server) NIL :start 1)
    ;; Wait until our processing thread is shut down
    (unless *in-processing-thread*
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
    ;; Call the sequence end.
    (call-next-method)))

(defmethod run ((server server))
  (let ((queue (queue server))
        (*in-processing-thread* T)
        (*server* server))
    (unwind-protect
         (loop while (thread server)
               do (with-simple-restart (continue "Continue with fingers crossed.")
                    ;; Loop until we can successfully clear the queue.
                    (loop with i = 0
                          for end = (svref queue 0)
                          while (< 0 end)
                          ;; Loop until we've reached the end of the queue.
                          do (loop while (< i end)
                                   for function = (svref queue (1+ i))
                                   do (when function
                                        (funcall function)
                                        (setf (svref queue (1+ i)) NIL)
                                        (incf i)))
                          until (atomics:cas (svref queue 0) end 0))
                    (mixed:mix server)))
      (mixed:end server)
      (setf (thread server) NIL))))

(defmethod call-in-mixing-context (function (server server) &key (synchronize T) timeout)
  (flet ((push-function (function)
           ;; Loop until there's space available and until we actually update the queue.
           (loop with queue = (queue server)
                 for old = (svref (queue server) 0)
                 do (if (<= (length queue) old)
                        (bt:thread-yield)
                        (when (atomics:cas (svref queue 0) old (1+ old))
                          (setf (svref queue (1+ old)) function)
                          (return))))))
    (if (or (not synchronize) *in-processing-thread*)
        (push-function function)
        (let* ((lock (bt:make-lock))
               (monitor (bt:make-condition-variable))
               (values-list ()))
          (bt:with-lock-held (lock)
            (push-function (lambda ()
                             (unwind-protect
                                  (setf values-list (multiple-value-list (funcall function)))
                               (bt:condition-notify monitor))))
            (bt:condition-wait monitor lock :timeout timeout)
            (values-list values-list))))))

(defmacro with-server ((&optional (server '*server*) &rest args &key synchronize timeout) &body body)
  (declare (ignore synchronize timeout))
  `(call-in-mixing-context (lambda () ,@body) ,server ,@args))
