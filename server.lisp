;;;; server.lisp

(in-package #:m-server)

(defvar *backend* (make-instance 'plain-backend))

(defmacro defget (name (&rest params) &body body)
  `(hunchentoot:define-easy-handler (,name)
       ,params
     (setf (hunchentoot:content-type*) "application/json")
     ,@body))

(defmacro defpost (name (&rest params) &body body)
  `(hunchentoot:define-easy-handler (,name)
       ,params
     ,@body
     nil))

(defget holdup (p)
  (if (holdup? *backend* p)
      "true"
      "false"))

(defpost signalholdup (id pw pl)
	 (when (authenticate *backend* id pw)
	   (signal-holdup *backend* id pl)))

(defget holdupsignaled (id pw pl)
  (when (authenticate *backend* id pw)
    (if (holdup-signaled? *backend* id pl)
	"true"
	"false")))