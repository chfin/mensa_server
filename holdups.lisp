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

(defun holdup-signaled? (user-id place)
  "Has the user an active signal?"
  (expire-signals)
  (when (member-if (lambda (s)
		     (and (equal user-id (cadr s))
			  (equal place (caddr s))))
		   *hu-signals*)
    t))

(defun signal-holdup (user-id place)
  "Signal a holdup."
  (unless (holdup-signaled? user-id)
    (push (list (get-universal-time) user-id place)
	  *hu-signals*)))

(defun holdup? (place)
  "Is there a holdup?"
  (expire-signals)
  (>= (length (remove-if-not (lambda (s) (equal place (caddr s)))
			     *hu-signals*))
      *hu-threshold*))