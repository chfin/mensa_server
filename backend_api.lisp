;;;; backend_api.lisp

(in-package #:m-server)

(defparameter *hu-exp-time* 60
  "The expiration time of a holdup signal in seconds.")
(defparameter *hu-threshold* 3
  "The required number of signals for a holdup.")

(defgeneric holdup-signaled? (backend user-id place))
(defgeneric signal-holdup (backend user-id place))
(defgeneric holdup? (backend place))