;;;; mem-backend.lisp

(in-package #:m-server)

(defvar *hu-signals* nil
  "A list of all holdup signals")

(defclass plain-backend ()
  ((hu-signals :initform nil
	       :accessor hu-signals)))

(defun expire-signals (backend)
  "Remove expired signals."
  (let ((now (- (get-universal-time) *hu-exp-time*)))
    (setf (hu-signals backend)
	  (remove-if (lambda (entry)
		       (< (car entry) now))
		     (hu-signals backend)))))

(defmethod holdup-signaled? ((backend plain-backend) user-id place)
  "Has the user an active signal?"
  (expire-signals backend)
  (when (member-if (lambda (s)
		     (and (equal user-id (cadr s))
			  (equal place (caddr s))))
		   (hu-signals backend))
    t))

(defmethod signal-holdup ((backend plain-backend) user-id place)
  "Signal a holdup."
  (unless (holdup-signaled? backend user-id place)
    (push (list (get-universal-time) user-id place)
	  (hu-signals backend))))

(defmethod holdup? ((backend plain-backend) place)
  "Is there a holdup?"
  (expire-signals backend)
  (>= (length (remove-if-not (lambda (s) (equal place (caddr s)))
			     (hu-signals backend)))
      *hu-threshold*))