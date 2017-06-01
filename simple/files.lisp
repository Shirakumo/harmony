#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony.simple)

(defvar *filetype-source-map* (make-hash-table :test 'equalp))

(defun source-type (name)
  (or (gethash name *filetype-source-map*)
      (error "Unknown file type ~a." name)))

(defun (setf source-type) (type name)
  (setf (gethash name *filetype-source-map*) type))

(defmacro define-source-type (name type)
  `(setf (source-type ,(string name)) ',type))

(define-source-type "mp3" mp3-source)
