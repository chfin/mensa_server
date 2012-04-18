;;;; mem-backend.lisp

(in-package #:m-server)

(defvar *hu-signals* nil
  "A list of all holdup signals")

(defclass plain-backend (base-backend)
  ((hu-signals :initform nil
	       :accessor hu-signals)
   (accounts :initform '((1 "ich@ich.de" "password" "ich"))
	     :accessor accounts)
   (candidates :initform nil
	       :accessor candidates)))

;;; holdups
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
  (let ((id (parse-integer user-id)))
    (unless (holdup-signaled? backend id place)
      (push (list (get-universal-time) id place)
	    (hu-signals backend)))))

(defmethod holdup? ((backend plain-backend) place)
  "Is there a holdup?"
  (expire-signals backend)
  (>= (length (remove-if-not (lambda (s) (equal place (caddr s)))
			     (hu-signals backend)))
      *hu-threshold*))

;;; accounts

(defmethod authenticate ((backend plain-backend) user-id pw)
  (let ((id (parse-integer user-id)))
    (when (member-if (lambda (a)
		       (and (equal id (car a))
			    (equal pw (caddr a))))
		     (accounts backend))
      t)))

(defmethod get-id ((backend plain-backend) mail pw)
  (car (find-if (lambda (a)
		  (and (equal mail (cadr a))
		       (equal pw (caddr a))))
		(accounts backend))))

(defmethod get-info ((backend plain-backend) user-id)
  (let ((id (parse-integer user-id)))
    (cadr (find-if (lambda (a)
		     (equal id (car a)))
		   (accounts backend)))))

(defmethod new-account ((backend plain-backend) mail pw name)
  (let ((code (format nil "~a" (random 65535))))
    (format t "This is the activation code for ~a <~a>: ~a~%" name mail code)
    (push (list code mail pw name) (candidates backend))
    code))

(defmethod activate-account ((backend plain-backend) mail code)
  (let ((cnd (find-if (lambda (c)
			(and (equal code (car c))
			     (equal mail (cadr c))))
		      (candidates backend)))
	(max (reduce #'max (mapcar #'car (accounts backend)))))
    (when cnd
      (setf (candidates backend)
	    (remove-if (lambda (c)
			 (equal mail (cadr c)))
		       (candidates backend)))
      (push (cons (1+ max) (cdr cnd)) (accounts backend)))))

(defmethod delete-account ((backend plain-backend) user-id)
  (let ((id (parse-integer user-id)))
    (setf (accounts backend)
	  (remove-if (lambda (a)
		       (equal id (car a)))
		     (accounts backend)))))

(defmethod search-accounts ((backend plain-backend) term)
  (let* ((names (mapcar (lambda (a)
			  (cons (cadddr a) (car a)))
			(accounts backend)))
	 (matches (remove-if-not (lambda (n)
				   (cl:search term (car n) :test #'char-equal))
				 names))
	 (sorted (sort matches
		       (lambda (s1 s2)
			 (< (levenshtein:distance term (car s1))
			    (levenshtein:distance term (car s2)))))))
    (mapcar (lambda (s)
	      (list (cons :name (car s)) (cons :id (cdr s))))
	    sorted)))