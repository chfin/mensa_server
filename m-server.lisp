;;;; m-server.lisp

(in-package #:m-server)

(defvar *current-acp* nil)

(defun start-http-server ()
  (when *current-acp*
    (hunchentoot:stop *current-acp*))
  (setf *current-acp* (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (hunchentoot:start *current-acp*))

(defun stop-server ()
  (when *current-acp*
    (hunchentoot:stop *current-acp*)
    (setf *current-acp* nil)))