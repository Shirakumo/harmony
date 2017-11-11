#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

(defclass out-port (flow:out-port flow:n-port)
  ())

(defclass in-port (flow:in-port flow:1-port)
  ())

(defclass in-ports (flow:in-port flow:n-port)
  ())

(defclass node (flow:dynamic-node)
  ())

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~a" (flow:attribute node 'segment))))

(defmethod initialize-instance :after ((node node) &key segment)
  (setf (flow:attribute node 'segment) segment))

(defmethod make-node ((segment cl-mixed:segment))
  (destructuring-bind (&key max-inputs outputs flags &allow-other-keys)
      (info segment)
    (let ((node (make-instance 'node :segment segment)))
      (loop for i from 0 below outputs
            for port = (make-instance 'out-port :node node :name i)
            do (when (find :inplace flags)
                 (setf (flow:attribute port :in-place) T))
               (push port (flow:ports node)))
      (if (< (expt 2 32) max-inputs) ;; "infinity-large"
          (push (make-instance 'in-ports :node node :name 'n) (flow:ports node))
          (dotimes (i max-inputs)
            (push (make-instance 'in-port :node node :name i) (flow:ports node))))
      (setf (flow:ports node) (nreverse (flow:ports node)))
      node)))

(defmethod complete-segment ((node node))
  (let ((segment (flow:attribute node 'segment)))
    (loop with in = 0
          for port in (flow:ports node)
          do (cond ((typep port 'out-port)
                    (setf (output (flow:name port) segment)
                          (flow:attribute port 'buffer)))
                   ((typep port 'in-port)
                    (let ((buffer (flow:attribute (flow:left (first (flow:connections port))) 'buffer)))
                      (setf (input (flow:name port) segment) buffer)))
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
             (incf i))
        finally (error "There is no output at index ~a on ~a."
                       n node)))

(defmethod nth-in (n (node node))
  (loop with i = 0
        for port in (flow:ports node)
        do (cond ((typep port 'in-ports)
                  (return port))
                 ((typep port 'in-port)
                  (when (= i n)
                    (return port))
                  (incf i)))
        finally (error "There is no input at index ~a on ~a."
                       n node)))

(defclass pipeline ()
  ((nodes :initform (make-hash-table :test 'eq) :accessor nodes)))

(defmethod print-object ((pipeline pipeline) stream)
  (print-unreadable-object (pipeline stream :type T)
    (format stream "~a nodes" (hash-table-count (nodes pipeline)))))

(defmethod ensure-node ((node node) pipeline)
  node)

(defmethod ensure-node ((segment cl-mixed:segment) (pipeline pipeline))
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
                       (source cl-mixed:segment) source-output
                       (target cl-mixed:segment) target-input)
  (let ((source (ensure-node source pipeline))
        (target (ensure-node target pipeline)))
    (flow:disconnect (nth-out source-output source)
                     (nth-in target-input target))
    pipeline))

(defmethod sever ((pipeline pipeline) (segment cl-mixed:segment))
  (flow:sever (ensure-node segment pipeline))
  pipeline)

(defun allocate-buffers (nodes buffersize &optional old-buffers)
  (let* ((buffer-count (1+ (loop for node in nodes
                                 when (flow:ports node)
                                 maximize (loop for port in (flow:ports node)
                                                when (flow:attribute port 'buffer)
                                                maximize (flow:attribute port 'buffer)))))
         (buffers (make-array buffer-count)))
    (map-into buffers #'identity old-buffers)
    (loop for i from (length old-buffers) below buffer-count
          do (setf (aref buffers i) (cl-mixed:make-buffer buffersize)))
    (dolist (node nodes buffers)
      (loop for port in (flow:ports node)
            for buffer = (flow:attribute port 'buffer)
            do (when buffer
                 (setf (flow:attribute port 'buffer) (aref buffers buffer)))))))

(defmethod compile-pipeline ((pipeline pipeline) (server server))
  (let* ((nodes (loop for node being the hash-values of (nodes pipeline)
                      collect node))
         (nodes (flow:allocate-ports nodes :attribute 'buffer))
         (device)
         (old-sequence (segment-sequence server))
         (sequence (cl-mixed:make-segment-sequence))
         (old-buffers (buffers server))
         (buffers (allocate-buffers nodes (buffersize server) old-buffers)))
    (clrhash (segment-map server))
    (loop for node in nodes
          for segment = (complete-segment node)
          do (add (complete-segment node) sequence)
             (setf (segment (name segment) server) segment)
          finally (setf device segment))
    (with-body-in-server-thread (server :synchronize T)
      (when (and (started-p server) old-sequence)
        (cl-mixed:end old-sequence))
      (setf (device server) device)
      (setf (buffers server) buffers)
      (setf (segment-sequence server) sequence)
      (when (started-p server)
        (cl-mixed:start sequence)))
    (when old-sequence
      (free old-sequence))
    (when old-buffers
      ;; Free buffers that were not re-used
      (loop for i from (length buffers) below (length old-buffers)
            do (free (aref old-buffers i))))))
