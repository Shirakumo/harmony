#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony.simple)

(defvar *server* NIL)

(defclass default-server (server)
  ())

(defmethod initialize-instance :after ((server default-server) &key)
  (let ((pipeline (make-pipeline server)))
    (compile-pipeline pipeline server)))

(defun make-segment (type &rest args)
  (apply #'make-instance type :server *server* args))

(defmethod make-pipeline ((server default-server))
  (let* ((*server* server)
         (pipeline (make-instance 'pipeline))
         (output (make-segment 'out123-drain))
         (master (make-segment 'linear-mixer :name :master))
         (music (make-segment 'linear-mixer :name :music))
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
    (segment segment-ish)
    (symbol (or (segment segment-ish server)
                (error "No segment called ~a on ~a"
                       segment-ish server)))))

(defun play (file mixer &key (server *server*)
                             paused
                             loop
                             fade
                             (volume 1.0)
                             (type (source-type (pathname-type file)))
                             location
                             velocity)
  (harmony:play server file mixer
                :paused paused :loop loop :fade fade
                :volume volume :type type
                :location location :velocity velocity))

(defmethod add ((source source) (name symbol))
  (add source (ensure-segment name *server*)))

(defmethod withdraw ((source source) (name symbol))
  (add source (ensure-segment name *server*)))

(setf *server* (make-instance 'default-server))

(defun start (&optional (thing *server*))
  (harmony:start thing))

(defun stop (&optional (thing *server*))
  (harmony:stop thing))

(defun pause (&optional (thing *server*))
  (harmony:pause thing))

(defun paused-p (&optional (thing *server*))
  (harmony:paused-p thing))

(defun resume (&optional (thing *server*))
  (harmony:resume thing))
