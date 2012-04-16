;;;; server.lisp

(in-package #:m-server)

(hunchentoot:define-easy-handler (holdup :uri "/holdup")
    (p)
  (setf (hunchentoot:content-type*) "application/json")
  (if (holdup? p)
      "true"
      "false"))