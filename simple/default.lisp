#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony.simple)

(defvar *server* NIL)

(defclass default-server (server)
  ((output-spec :initarg :output-spec :accessor output-spec))
  (:default-initargs
   :output-spec '(harmony-out123:out123-drain)))

(defmethod initialize-instance :after ((server default-server) &key)
  (let ((pipeline (make-pipeline server)))
    (compile-pipeline pipeline server)))

(defun make-segment (type &rest args)
  (apply #'make-instance type :server *server* args))

(defmethod make-pipeline ((server default-server))
  (let* ((*server* server)
         (pipeline (make-instance 'pipeline))
         (output (apply #'make-segment (output-spec server)))
         (master (make-segment 'basic-mixer :name :master))
         (music (make-segment 'basic-mixer :name :music))
         (sfx (make-segment 'space-mixer :name :sfx))
         (voice (make-segment 'space-mixer :name :voice)))
    (connect pipeline master 0 output 0)
    (connect pipeline master 1 output 1)
    (connect pipeline music 0 master 0)
    (connect pipeline music 1 master 1)
    (connect pipeline sfx 0 master 2)
    (connect pipeline sfx 1 master 3)
    (connect pipeline voice 0 master 4)
    (connect pipeline voice 1 master 5)
    (setf (volume master) 0.8)
    (setf (volume sfx) 0.8)
    pipeline))

(defun ensure-segment (segment-ish server)
  (etypecase segment-ish
    (harmony:segment segment-ish)
    (symbol (or (segment segment-ish server)
                (error "No segment called ~a on ~a"
                       segment-ish server)))))

(defun play (source-ish mixer &rest initargs &key (server *server*) &allow-other-keys)
  (let ((initargs (copy-list initargs)))
    (remf initargs :server)
    (apply #'harmony:play server source-ish mixer initargs)))

(defmethod add ((source source) (name symbol))
  (add source (ensure-segment name *server*)))

(defmethod withdraw ((source source) (name symbol))
  (add source (ensure-segment name *server*)))

(defun initialize (&rest initargs)
  (unless *server*
    (setf *server* (apply #'make-instance 'default-server initargs))
    (start *server*)))

(defun start (&optional (thing *server* t-p))
  (harmony:start thing))

(defun started-p (&optional (thing *server*))
  (and thing (harmony:started-p thing)))

(defun stop (&optional (thing *server*))
  (harmony:stop thing))

(defun pause (&optional (thing *server*))
  (harmony:pause thing))

(defun paused-p (&optional (thing *server*))
  (harmony:paused-p thing))

(defun resume (&optional (thing *server*))
  (harmony:resume thing))

(defun segment (name &optional (thing *server*))
  (harmony:segment name thing))
