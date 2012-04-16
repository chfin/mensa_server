;;;; server.lisp

(in-package #:m-server)

(defvar *backend* (make-instance 'plain-backend))

(hunchentoot:define-easy-handler (holdup :uri "/holdup")
    (p)
  (setf (hunchentoot:content-type*) "application/json")
  (if (holdup? *backend* p)
      "true"
      "false"))