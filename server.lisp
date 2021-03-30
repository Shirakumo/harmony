#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defvar *in-processing-thread* NIL)
(defvar *in-processing-queue* NIL)
(defvar *server* NIL)

(defclass server (mixed:chain)
  ((segment-map :initform (make-hash-table :test 'equal) :reader segment-map)
   (free-buffers :initform () :accessor free-buffers)
   (free-unpackers :initform () :accessor free-unpackers)
   (thread :initform NIL :accessor thread)
   (queue :reader queue)
   (samplerate :initform 48000 :initarg :samplerate :accessor samplerate)
   (buffersize :initform NIL :initarg :buffersize :accessor buffersize)
   (name :initform "Harmony")))

(defmethod initialize-instance :after ((server server) &key (queue-size 64))
  (setf *server* server)
  (setf (slot-value server 'queue) (make-array queue-size :element-type T :initial-element NIL))
  (setf (svref (queue server) 0) 1)
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
  (mixed:clear buffer)
  (push* buffer (slot-value server 'free-buffers)))

(defmethod free-unpacker (unpacker (server server))
  (disconnect unpacker T)
  (mixed:withdraw unpacker T)
  (mixed:clear (mixed:pack unpacker))
  (when (mixed:handle unpacker)
    (mixed:end unpacker)
    (push* unpacker (slot-value server 'free-unpackers))))

(defmethod segment (name (server (eql T)) &optional (errorp T))
  (segment name *server* errorp))

(defmethod segment ((name symbol) (server server) &optional (errorp T))
  (or (gethash name (segment-map server))
      (when errorp (error "No such segment ~s" name))))

(defmethod (setf segment) (segment name (server (eql T)))
  (setf (segment name *server*) segment))

(defmethod (setf segment) ((segment mixed:segment) name (server server))
  (unless name
    (error "NAME cannot be NIL."))
  (let ((existing (gethash name (segment-map server))))
    (when (and existing (not (eq segment existing)))
      (error "Segment with name ~s already exists." name)))
  (setf (gethash name (segment-map server)) segment))

(defmethod (setf segment) ((null null) name (server server))
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
  (mixed:clear server)
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
  (set-process-priority :high)
  (call-next-method)
  (setf (thread server) T)
  (let ((thread (bt:make-thread (lambda ()
                                  (set-thread-priority :high)
                                  (run server))
                                :name "harmony"
                                :initial-bindings `((*standard-output* . ,*standard-output*)
                                                    (*error-output* . ,*error-output*)
                                                    (*query-io* . ,*query-io*)
                                                    (*trace-output* . ,*trace-output*)))))
    (setf (thread server) thread))
  server)

(defmethod mixed:volume ((name symbol))
  (mixed:volume (segment name *server*)))

(defmethod (setf mixed:volume) (value (name symbol))
  (setf (mixed:volume (segment name *server*)) value))

(defmethod mixed:location ((name symbol))
  (mixed:location (segment name *server*)))

(defmethod (setf mixed:location) (location (name symbol))
  (setf (mixed:location (segment name *server*)) location))

(defmethod mixed:velocity ((name symbol))
  (mixed:velocity (segment name *server*)))

(defmethod (setf mixed:velocity) (velocity (name symbol))
  (setf (mixed:velocity (segment name *server*)) velocity))

(defmethod started-p ((server server))
  (and (thread server)
       (or (null (bt:threadp (thread server)))
           (bt:thread-alive-p (thread server)))))

(defmethod mixed:end ((server server))
  (when (thread server)
    ;; Clear the queue
    (setf (svref (queue server) 0) 1)
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
    (call-next-method))
  server)

(defmethod run-task ((server server) task)
  (when task
    (tagbody repeat
       (restart-case (funcall task)
         (abort (&optional e)
           :report "Abort executing the task."
           (declare (ignore e)))
         (retry (&optional e)
           :report "Retry executing the task."
           (declare (ignore e))
           (go repeat))))))

(defmethod run ((server server))
  (declare (optimize speed (safety 1)))
  (let ((queue (queue server))
        (*in-processing-thread* T)
        (*server* server)
        (handle (mixed:handle server)))
    (unwind-protect
         (loop while (thread server)
               do (with-simple-restart (continue "Continue with fingers crossed.")
                    ;; Loop until we can successfully clear the queue.
                    (loop with *in-processing-queue* = T
                          with i = 1
                          for end = (the (unsigned-byte 32) (svref queue 0))
                          while (< 1 end)
                          ;; Loop until we've reached the end of the queue.
                          do (loop while (< i end)
                                   for task = (svref queue i)
                                   do (run-task server task)
                                      (setf (svref queue i) NIL)
                                      (incf i))
                          until (atomics:cas (svref queue 0) end 1))
                    (mixed-cffi:segment-mix handle)
                    ;; KLUDGE: without this attempting a full GC on SBCL
                    ;;         will cause it to lock up indefinitely. Bad!
                    #+sbcl
                    (when (or sb-impl::*gc-pending*
                              sb-impl::*stop-for-gc-pending*)
                      (sb-unix::receive-pending-interrupt))))
      (mixed:end server)
      (setf (thread server) NIL))))

(defmethod call-in-mixing-context ((function function) (server server) &key (synchronize T) timeout)
  (declare (optimize speed))
  (flet ((push-function (function)
           ;; Loop until there's space available and until we actually update the queue.
           (loop with queue of-type simple-vector = (queue server)
                 for fill of-type (unsigned-byte 32) = (svref (queue server) 0)
                 do (if (< fill (length queue))
                        (when (atomics:cas (svref queue 0) fill (1+ fill))
                          (setf (svref queue fill) function)
                          (return))
                        ;; Drop the event on the floor.
                        (restart-case
                            (progn (warn "Mixing queue is full.")
                                   (return))
                          (abort ()
                            (return))
                          (continue ()
                            (bt:thread-yield)))))))
    (cond (*in-processing-queue*
           (funcall function))
          ((or (not synchronize) *in-processing-thread*)
           (push-function function))
          (T
           (let* ((lock (bt:make-lock))
                  (monitor (bt:make-condition-variable))
                  (values-list ()))
             (bt:with-lock-held (lock)
               (push-function (lambda ()
                                (unwind-protect
                                     (setf values-list (multiple-value-list (funcall function)))
                                  (bt:condition-notify monitor))))
               (bt:condition-wait monitor lock :timeout timeout)
               (values-list values-list)))))))

(defmacro with-server ((&optional (server '*server*) &rest args &key synchronize timeout) &body body)
  (declare (ignore synchronize timeout))
  `(call-in-mixing-context (lambda () ,@body) ,server ,@args))
