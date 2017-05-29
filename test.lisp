(ql:quickload '(harmony-mp3 harmony-out123))

(defvar *server* (make-instance 'harmony:server))

(defvar *pipeline* (make-instance 'harmony:pipeline))

(let ((out (make-instance 'harmony-out123:out123-drain
                          :server *server*))
      (mp3 (make-instance 'harmony-mp3:mp3-source
                          :server *server*
                          :file #p"/home/linus/Media/Emancipator/Soon It Will Be Cold Enough/01-emancipator-eve.mp3"))
      (mp2 (make-instance 'harmony-mp3:mp3-source
                          :server *server*
                          :file #p"/home/linus/Media/Emancipator/Soon It Will Be Cold Enough/01-emancipator-eve.mp3"))
      (mix-l (cl-mixed:make-linear-mixer))
      (mix-r (cl-mixed:make-linear-mixer)))
  (harmony:connect *pipeline* mp3 0 mix-l 0)
  (harmony:connect *pipeline* mp3 1 mix-r 0)
  (harmony:connect *pipeline* mp2 0 mix-l 1)
  (harmony:connect *pipeline* mp2 1 mix-r 1)
  (harmony:connect *pipeline* mix-l 0 out 0)
  (harmony:connect *pipeline* mix-r 0 out 1)
  (setf (harmony:looping-p mp3) T)
  (setf (harmony:device *server*) out))

(harmony:compile-pipeline *pipeline* *server*)
;; (harmony:start *server*)
