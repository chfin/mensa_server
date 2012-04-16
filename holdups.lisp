;;;; holdups.lisp

(in-package #:m-server)

;;; holdups.lisp goes here.

(defvar *hu-signals* nil
  "A list of all holdup signals")
(defparameter *hu-exp-time* 60
  "The expiration time of a holdup signal in seconds.")
(defparameter *hu-threshold* 3
  "The required number of signals for a holdup.")

(defun expire-signals ()
  "Remove expired signals."
  (let ((now (- (get-universal-time) *hu-exp-time*)))
    (setf *hu-signals*
	  (remove-if (lambda (entry)
		       (< (car entry) now))
		     *hu-signals*))))

(defun holdup-signaled? (user-id)
  "Has the user an active signal?"
  (expire-signals)
  (when (member-if (lambda (a) (equal user-id (cdr a)))
		   *hu-signals*)
    t))

(defun signal-holdup (user-id)
  "Signal a holdup."
  (unless (holdup-signaled? user-id)
    (push (cons (get-universal-time) user-id)
	  *hu-signals*)))

(defun holdup? ()
  "Is there a holdup?"
  (expire-signals)
  (>= (length *hu-signals*) *hu-threshold*))