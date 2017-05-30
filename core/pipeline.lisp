#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony.core)

(defclass out-port (flow:out-port flow:n-port)
  ())

(defclass in-port (flow:in-port flow:1-port)
  ())

(defclass in-ports (flow:in-port flow:n-port)
  ())

(flow:define-node node ()
  ((ports :initarg :ports :initform () :accessor flow:ports)
   (segment :initarg :segment :accessor segment)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~a" (segment node))))

(defmethod make-node ((segment segment))
  (destructuring-bind (&key max-inputs outputs flags &allow-other-keys)
      (info segment)
    (let ((node (make-instance 'node :segment segment)))
      (loop for i from 0 below outputs
            for port = (make-instance 'out-port :node node :slot i)
            do (when (find :inplace flags)
                 (setf (flow:attribute port :in-place) T))
               (push port (flow:ports node)))
      (if (< (expt 2 32) max-inputs) ;; "infinity-large"
          (push (make-instance 'in-ports :node node :slot 'n) (flow:ports node))
          (dotimes (i max-inputs)
            (push (make-instance 'in-port :node node :slot i) (flow:ports node))))
      (setf (flow:ports node) (nreverse (flow:ports node)))
      node)))

(defmethod finalize-segment ((node node))
  (let ((segment (segment node)))
    (loop with in = 0
          for port in (flow:ports node)
          do (cond ((typep port 'out-port)
                    (setf (output (flow:slot port) segment)
                          (flow:attribute port 'buffer)))
                   ((typep port 'in-port)
                    (let ((buffer (flow:attribute (flow:left (first (flow:connections port))) 'buffer)))
                      (setf (input (flow:slot port) segment) buffer)))
                   ((typep port 'in-ports)
                    (dolist (connection (flow:connections port))
                      (setf (input in segment)
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

(defmethod print-object ((pipeline pipeline) stream)
  (print-unreadable-object (pipeline stream :type T)
    (format stream "~a nodes" (hash-table-count (nodes pipeline)))))

(defmethod ensure-node ((node node) pipeline)
  node)

(defmethod ensure-node ((segment segment) (pipeline pipeline))
  (or (gethash segment (nodes pipeline))
      (setf (gethash segment (nodes pipeline))
            (make-node segment))))

(defmethod connect ((pipeline pipeline)
                    (source segment) source-output
                    (target segment) target-input)
  (let ((source (ensure-node source pipeline))
        (target (ensure-node target pipeline)))
    (flow:connect (nth-out source-output source)
                  (nth-in target-input target)
                  'flow:directed-connection)
    pipeline))

(defmethod disconnect ((pipeline pipeline)
                       (source segment) source-output
                       (target segment) target-input)
  (let ((source (ensure-node source pipeline))
        (target (ensure-node target pipeline)))
    (flow:disconnect (nth-out source-output source)
                     (nth-in target-input target))
    pipeline))

(defmethod sever ((pipeline pipeline) (segment segment))
  (flow:sever (ensure-node segment pipeline))
  pipeline)

(defun allocate-buffers (nodes buffersize)
  (let* ((buffer-count (1+ (loop for node in nodes
                                 when (flow:ports node)
                                 maximize (loop for port in (flow:ports node)
                                                when (flow:attribute port 'buffer)
                                                maximize (flow:attribute port 'buffer)))))
         (buffers (make-array buffer-count)))
    (dotimes (i buffer-count)
      (setf (aref buffers i) (make-buffer buffersize)))
    (dolist (node nodes buffers)
      (loop for port in (flow:ports node)
            for buffer = (flow:attribute port 'buffer)
            do (when buffer
                 (setf (flow:attribute port 'buffer) (aref buffers buffer)))))))

(defmethod compile-pipeline ((pipeline pipeline) (server server))
  (let* ((nodes (loop for node being the hash-values of (nodes pipeline)
                      collect node))
         (nodes (flow:allocate-ports nodes :attribute 'buffer))
         (mixer (make-mixer))
         (buffers (allocate-buffers nodes (buffersize server))))
    (dolist (node nodes)
      (add (finalize-segment node) mixer))
    (with-body-in-server-thread (server)
      (when (mixer server)
        (end mixer)
        (map NIL #'free (segments mixer))
        (free (mixer server)))
      (map NIL #'free (buffers server))
      (setf (buffers server) buffers)
      (setf (mixer server) mixer))))
