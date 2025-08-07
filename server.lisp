(in-package #:org.shirakumo.fraf.harmony)

(define-condition mixing-queue-full (error)
  ((server :initarg :server :reader server))
  (:report (lambda (c s) (format s "The mixing queue is full. The server is likely stalled out somehow."))))

(defvar *in-processing-thread* NIL)
(defvar *in-processing-queue* NIL)
(defvar *server* NIL)

(defclass server (mixed:chain)
  ((segment-map :initform (make-hash-table :test 'equal) :reader segment-map)
   (free-buffers :initform #-ccl () #+ccl (vector ()) :accessor free-buffers)
   (free-unpackers :initform #-ccl () #+ccl (vector ()) :accessor free-unpackers)
   (thread :initform NIL :accessor thread)
   (queue :reader queue)
   (samplerate :initform mixed:*default-samplerate* :initarg :samplerate :accessor samplerate :reader mixed:samplerate)
   (buffersize :initform NIL :initarg :buffersize :accessor buffersize)
   (name :initform "Harmony")
   (paused :initform NIL :accessor paused-p)))

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
  ;; FIXME: Some segments need a different buffer size (buffer-size-hint),
  ;;        we should really respect that if possible.
  (or (pop* #-ccl (slot-value server 'free-buffers)
            #+ccl (svref (slot-value server 'free-buffers) 0))
      (mixed:make-buffer (buffersize server))))

(defmethod allocate-unpacker ((server server))
  (or (pop* #-ccl (slot-value server 'free-unpackers)
            #+ccl (svref (slot-value server 'free-unpackers) 0))
      (mixed:make-unpacker :frames (buffersize server) :samplerate (samplerate server))))

(defmethod free-buffer (buffer (server server))
  (setf (from buffer) NIL)
  (setf (to buffer) NIL)
  (mixed:clear buffer)
  (push* buffer #-ccl (slot-value server 'free-buffers)
                #+ccl (svref (slot-value server 'free-buffers) 0)))

(defmethod free-unpacker (unpacker (server server))
  (disconnect unpacker T)
  (mixed:withdraw unpacker T)
  (mixed:clear (mixed:pack unpacker))
  (when (mixed:handle unpacker)
    (mixed:end unpacker)
    (setf (mixed:samplerate unpacker) (samplerate server))
    (push* unpacker #-ccl (slot-value server 'free-unpackers)
                    #+ccl (svref (slot-value server 'free-unpackers) 0))))

(defmethod segment (name (server (eql T)) &optional (errorp T))
  (segment name *server* errorp))

(defmethod segment (name (server server) &optional (errorp T))
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
  (ignore-errors
   (mixed:end server)))

(defmethod mixed:free :after ((server server))
  (labels ((rec (chain)
             (loop for segment across (mixed:segments chain)
                   do (when (typep segment 'mixed:chain)
                        (rec segment))
                      (when (typep segment 'mixed:c-object)
                        (mixed:free segment)))))
    (rec server))
  (mixed:clear server)
  (clrhash (segment-map server))
  (loop for buffer = (pop #-ccl (free-buffers server)
                          #+ccl (svref (free-buffers server) 0))
        while buffer do (mixed:free buffer))
  (loop for unpacker = (pop #-ccl (free-unpackers server)
                            #+ccl (svref (free-unpackers server) 0))
        while unpacker do (mixed:free unpacker)))

(defmethod mixed:add :after ((segment mixed:segment) (server server))
  (when (name segment)
    (setf (segment (name segment) server) segment)))

(defmethod mixed:withdraw :after ((segment mixed:segment) (server server))
  (when (name segment)
    (setf (segment (name segment) server) NIL)))

(defmethod mixed:device ((server server))
  (mixed:device (segment :drain (segment :output server))))

(defmethod (setf mixed:device) (value (server server))
  (let* ((drain (segment :drain (segment :output server)))
         (convert (segment :upmix (segment :output server)))
         (packer (segment :packer (segment :output server)))
         (prev (mixed:device server))
         (count (mixed:channels drain)))
    (call-in-mixing-context
     (lambda ()
       (handler-case
           (prog1 (setf (mixed:device drain) value)
             (when (/= count (mixed:channels drain))
               (mixed:end packer)
               (disconnect convert T :direction :output)
               (mixed::revalidate packer)
               (setf (mixed:channel-count-out convert) (mixed:channels drain))
               (connect convert T packer T)
               (mixed:start packer)))
         (error (e)
           (format *error-output* "~&Failed to switch output device: ~a~%" e)
           (setf (mixed:device drain) prev))))
     server :synchronize T)))

(defmethod mixed:start :before ((server server))
  (when (started-p server)
    (error "~a is already running." server)))

(defmethod mixed:start ((server server))
  (set-process-priority :high)
  (setf (thread server) T)
  (let ((thread (bt:make-thread (lambda ()
                                  (set-thread-priority :high)
                                  ;; KLUDGE: have to call mixed_start here to ensure we're in the same thread
                                  ;;         as the main loop. This makes error propagation a hassle though...
                                  (mixed::with-error-on-failure ()
                                    (org.shirakumo.fraf.mixed.cffi:segment-start (mixed:handle server)))
                                  (run server))
                                :name "harmony"
                                :initial-bindings `((*standard-output* . ,*standard-output*)
                                                    (*error-output* . ,*error-output*)
                                                    (*query-io* . ,*query-io*)
                                                    (*trace-output* . ,*trace-output*)))))
    (setf (thread server) thread))
  server)

(defun stop-processing-thread (server)
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
      thread)))

(defmacro define-name-alias (function)
  `(progn
     (defmethod ,function ((name symbol))
       (,function (segment name *server*)))

     (defmethod (setf ,function) (value (name symbol))
       (setf (,function (segment name *server*)) value))))

(define-name-alias mixed:volume)
(define-name-alias mixed:location)
(define-name-alias mixed:velocity)
(define-name-alias mixed:direction)
(define-name-alias mixed:up)
(define-name-alias mixed:soundspeed)
(define-name-alias mixed:doppler-factor)
(define-name-alias mixed:min-distance)
(define-name-alias mixed:max-distance)
(define-name-alias mixed:rolloff)
(define-name-alias mixed:attenuation)

(defmethod mixed:add (source (target symbol))
  (mixed:add source (segment target *server* T)))

(defmethod mixed:add ((source symbol) target)
  (mixed:add (segment source *server* T) target))

(defmethod mixed:withdraw (source (target symbol))
  (mixed:withdraw source (segment target *server* T)))

(defmethod mixed:withdraw ((source symbol) target)
  (mixed:withdraw (segment source *server* T) target))

(defmethod connect ((source symbol) source-location drain drain-location)
  (connect (segment source *server* T) source-location drain drain-location))

(defmethod disconnect ((segment symbol) location &rest args &key &allow-other-keys)
  (apply #'disconnect (segment segment *server* T) location args))

(defmethod started-p ((server server))
  (and (thread server)
       (or (null (bt:threadp (thread server)))
           (bt:thread-alive-p (thread server)))))

(defmethod mixed:end ((server server))
  (when (thread server)
    ;; Clear the queue
    (setf (svref (queue server) 0) 1)
    (fill (queue server) NIL :start 1)
    (stop-processing-thread server)
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
    (declare (type simple-vector queue))
    (declare (type cffi:foreign-pointer handle))
    (unwind-protect
         (loop while (thread server)
               do (with-simple-restart (continue "Continue with fingers crossed.")
                    ;; Loop until we can successfully clear the queue.
                    (loop with *in-processing-queue* = T
                          with i of-type (unsigned-byte 32) = 1
                          for end of-type (unsigned-byte 32) = (the (unsigned-byte 32) (svref queue 0))
                          while (< 1 end)
                          ;; Loop until we've reached the end of the queue.
                          do (loop while (< i end)
                                   do (let ((task (svref queue i)))
                                        (run-task server task)
                                        (setf (svref queue i) NIL)
                                        (incf i)))
                          until (atomics:cas (svref queue 0) end 1))
                    (if (paused-p server)
                        (sleep 0.01)
                        (mixed-cffi:segment-mix handle))
                    ;; KLUDGE: without this attempting a full GC on SBCL
                    ;;         will cause it to lock up indefinitely. Bad!
                    #+sbcl
                    (when (or sb-impl::*gc-pending*
                              sb-impl::*stop-for-gc-pending*)
                      (sb-unix::receive-pending-interrupt))))
      (when (mixed:handle server)
        (mixed:end server))
      (setf (thread server) NIL))))

(declaim (notinline handle-full-mixing-queue))
(defun handle-full-mixing-queue (server)
  (restart-case
      (error 'mixing-queue-full :server server)
    (continue (&optional condition)
      :report "Continue waiting"
      (declare (ignore condition))
      (bt:thread-yield))
    (restart (&optional condition)
      :report "Restart the server forcibly."
      :test (lambda (&optional condition)
              (declare (ignore condition))
              (not *in-processing-thread*))
      (declare (ignore condition))
      (stop-processing-thread server)
      (mixed:start server))
    (clear-queue (&optional condition)
      :report "Clear the queue forcibly."
      (declare (ignore condition))
      (loop for fill = (svref (queue server) 0)
            until (atomics:cas (svref (queue server) 0) fill 0)))))

(declaim (inline push-mixing-queue))
(defun push-mixing-queue (function server)
  ;; Loop until there's space available and until we actually update the queue.
  (loop with queue of-type simple-vector = (queue server)
        for fill of-type (unsigned-byte 32) = (svref (queue server) 0)
        do (if (< fill (length queue))
               (when (atomics:cas (svref queue 0) fill (1+ fill))
                 (setf (svref queue fill) function)
                 (return))
               (restart-case (handle-full-mixing-queue server)
                 (abort () (return))))))

(defmethod call-in-mixing-context ((function function) (server server) &key (synchronize T) timeout)
  (declare (optimize speed (safety 1)))
  (cond ((or *in-processing-queue* (null (thread server)))
         (funcall function))
        ((or (not synchronize) *in-processing-thread*)
         (push-mixing-queue function server))
        (T
         (let* ((lock (bt:make-lock))
                (monitor (bt:make-condition-variable))
                (values-list #1='#:NO-VALUE)
                (wait (etypecase timeout
                        (null 0.5)
                        (real (float timeout 0f0)))))
           (bt:with-lock-held (lock)
             (push-mixing-queue 
              (lambda ()
                (unwind-protect
                     (setf values-list (multiple-value-list (funcall function)))
                  (bt:condition-notify monitor)))
              server))
           (loop (bt:with-lock-held (lock)
                   (bt:condition-wait monitor lock :timeout wait))
                 (cond ((not (eq values-list #1#))
                        (return (values-list values-list)))
                       (timeout
                        (return (values NIL :timeout)))
                       (T
                        (bt:thread-yield))))))))

(defmacro with-server ((&optional (server '*server*) &rest args &key synchronize timeout) &body body)
  (declare (ignore synchronize timeout))
  `(call-in-mixing-context (lambda () ,@body) ,server ,@args))

(defun dot-server (&key (server *server*) (file #p "~/server.dot") (convert "svg"))
  (let ((table (make-hash-table :test 'eq))
        (index 0))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (format stream "~&digraph server {~%")
      (format stream "~&  graph [splines=ortho];~%")
      (format stream "~&  node [shape=record];~%")
      (labels ((traverse (thing)
                 (when thing
                   (unless (gethash thing table)
                     (setf (gethash thing table) (format NIL "S_~d" (incf index)))
                     (typecase thing
                       ((or mixed:chain mixed:bundle)
                        (loop for child across (mixed:segments thing)
                              do (traverse child)))
                       (mixed:segment
                        (format stream "~&  ~a [label = ~s];" (gethash thing table)
                                (format NIL "~a~@[ ~a~] ~a" (type-of thing) (name thing) index))
                        (loop for buffer across (mixed:inputs thing)
                              do (traverse buffer))
                        (loop for buffer across (mixed:outputs thing)
                              do (traverse buffer))
                        (typecase thing
                          ((or mixed:packer mixed:unpacker)
                           (traverse (mixed:pack thing))))))))))
        (traverse server))
      (labels ((connect (from to buffer)
                 (when (and from to)
                   (format stream "~&  ~a -> ~a [label=~s];" (gethash from table) (gethash to table)
                           (format NIL "~a ~a" (type-of buffer) (subseq (gethash buffer table) 2)))))
               (traverse (thing)
                 (typecase thing
                   ((or mixed:chain mixed:bundle)
                    (format stream "~&  subgraph cluster~a {~%" (gethash thing table))
                    (format stream "~&  label = ~s;"
                            (format NIL "~a~@[ ~a~] ~a" (type-of thing) (name thing)
                                    (subseq (gethash thing table) 2)))
                    (loop for child across (mixed:segments thing)
                          do (traverse child))
                    (format stream "~&  }~%"))
                   (mixed:segment
                    (loop for buffer across (mixed:inputs thing)
                          when buffer
                          do (connect (from buffer) thing buffer))
                    (typecase thing
                      ((or mixed:packer mixed:source)
                       (connect thing (to (mixed:pack thing)) (mixed:pack thing)))
                      ((or mixed:unpacker mixed:drain)
                       (connect (from (mixed:pack thing)) thing (mixed:pack thing))))))))
        (traverse server))
      (format stream "~&}~%"))
    (when convert
      #+asdf
      (uiop:run-program (list "dot"
                              (uiop:native-namestring file)
                              (format NIL "-T~a" convert)
                              "-o" (uiop:native-namestring (make-pathname :type convert :defaults file)))))))

(defmethod describe-object :after ((server server) stream)
  (format stream "~&~%Pipeline:~%")
  (org.shirakumo.text-draw:tree
   server (lambda (element)
            (typecase element
              (mixed:chain
               (coerce (mixed:segments element) 'list))
              (mixed:bundle
               (coerce (mixed:segments element) 'list))))
   :stream stream :max-depth NIL))
