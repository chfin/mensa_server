;;;; server.lisp

(in-package #:m-server)

(defvar *backend* (make-instance 'plain-backend))

(hunchentoot:define-easy-handler (holdup :uri "/holdup")
    (p)
  (setf (hunchentoot:content-type*) "application/json")
  (if (holdup? *backend* p)
      "true"
      "false"))

(hunchentoot:define-easy-handler (signalholdup :uri "/signalholdup")
    (id pw pl)
  (when (authenticate *backend* id pw)
    (signal-holdup *backend* id pl))
  nil)

(hunchentoot:define-easy-handler (holdupsignaled :uri "/holdupsignaled")
    (id pw pl)
  (setf (hunchentoot:content-type*) "application/json")
  (when (authenticate *backend* id pw)
    (if (holdup-signaled? *backend* id pl)
	"true"
	"false")))