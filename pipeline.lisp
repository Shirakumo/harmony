#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass out-port (flow:out-port flow:n-port)
  ((output :initarg :output :accessor output)))

(defclass in-port (flow:in-port flow:1-port)
  ((input :initarg :input :accessor input)))

(defclass in-ports (flow:in-port flow:n-port)
  ())

(defclass node ()
  ((ports :initarg :ports :initform () :accessor flow:ports)
   (segment :initarg :segment :accessor segment)))

(defmethod make-node ((segment cl-mixed:segment))
  (destructuring-bind (&key max-inputs outputs &allow-other-keys)
      (cl-mixed:info segment)
    (let ((node (make-instance 'node :segment segment)))
      (dotimes (i outputs)
        (push (make-instance 'out-port :output i) (flow:ports node)))
      (if (< (expt 2 32) max-inputs)
          (push (make-instance 'in-ports) (flow:ports node))
          (dotimes (i max-inputs)
            (push (make-instance 'in-ports :input i) (flow:ports node))))
      (setf (flow:ports node) (nreverse (flow:ports node)))
      node)))

(defmethod finalize-segment ((node node))
  (let ((segment (segment node)))
    (loop with in = 0 with out = 0
          for port in (flow:ports node)
          do (cond ((typep port 'out-port)
                    (setf (cl-mixed:output out segment)
                          (flow:attribute node 'buffer))
                    (incf out))
                   ((typep port 'in-port)
                    (let ((port (flow:left (first (flow:connections port)))))
                      (setf (cl-mixed:input in segment)
                            (flow:attribute port 'buffer)))
                    (incf in))
                   ((typep port 'in-ports)
                    (dolist (connection (flow:connections port))
                      (setf (cl-mixed:input in segment)
                            (flow:attribute (flow:left connection) 'buffer))
                      (incf in)))))
    segment))

(defmethod nth-out (n (node node))
  (loop with i = 0
        for port in (flow:ports node)
        do (when (typep port 'out-port)
             (when (= i n)
               (return port))
             (incf i))))

(defmethod nth-in (n (node node))
  (loop with i = 0
        for port in (flow:ports node)
        do (cond ((typep port 'in-ports)
                  (return port))
                 ((typep port 'in-port)
                  (when (= i n)
                    (return port))
                  (incf i)))))

(defclass pipeline ()
  ((nodes :initform (make-hash-table :test 'eq) :accessor nodes)))

(defmethod ensure-node ((node node) pipeline)
  node)

(defmethod ensure-node ((segment cl-mixed:segment) (pipeline pipeline))
  (or (gethash segment (nodes pipeline))
      (setf (gethash segment (nodes pipeline))
            (make-node segment))))

(defmethod connect ((pipeline pipeline)
                    (source cl-mixed:segment) source-output
                    (target cl-mixed:segment) target-input)
  (let ((source (ensure-node source pipeline))
        (target (ensure-node target pipeline)))
    (flow:connect (nth-out source-output source)
                  (nth-in target-input target)
                  'flow:directed-connection)
    pipeline))

(defun allocate-buffers (nodes buffersize)
  (let* ((buffer-count (loop for node in nodes
                             when (flow:ports node)
                             maximize (loop for port in (flow:ports node)
                                            when (flow:attribute port 'buffer)
                                            maximize (flow:attribute port 'buffer))))
         (buffers (make-array buffer-count)))
    (dotimes (i buffer-count)
      (setf (aref buffers i) (cl-mixed:make-buffer buffersize)))
    (dolist (node nodes nodes)
      (loop for port in (flow:ports node)
            for buffer = (flow:attribute port 'buffer)
            do (when buffer
                 (setf (flow:attribute port 'buffer) (aref buffers buffer)))))))

(defmethod compile-pipeline ((pipeline pipeline) (server server))
  (let ((nodes (flow:allocate-ports (nodes pipeline) :attribute 'buffer))
        (mixer (cl-mixed:make-mixer)))
    (allocate-buffers nodes (buffersize server))
    (dolist (node nodes)
      (cl-mixed:add (finalize-segment node) mixer))
    (with-server-lock (server)
      (when (mixer server)
        (cl-mixed:end mixer)
        (dolist (segment (cl-mixed:segments mixer))
          (cl-mixed:free segment))
        (cl-mixed:free (mixer server)))
      (setf (mixer server) mixer))))
