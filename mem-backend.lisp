;;;; mem-backend.lisp

(in-package #:m-server)

(defclass plain-backend (base-backend)
  ((hu-signals :initform nil
	       :accessor hu-signals)
   (accounts :initform '((1 "ich@ich.de" "password" "ich"))
	     :accessor accounts)
   (candidates :initform nil
	       :accessor candidates)
   (enquiries :initform nil
	      :accessor enqs)
   (contacts :initform nil
	     :accessor conts)
   (locations :initform (make-hash-table)
	      :accessor locs)))

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

;;; contacts

(defun contact-pair (id1 id2)
  (let ((i1 (parse-integer id1))
	(i2 (parse-integer id2)))
    (if (< i1 i2) (cons i1 i2) (cons i2 i1))))

(defmethod subscr ((backend plain-backend) user-id contact-id)
  (let ((enq (cons (parse-integer contact-id)
		   (parse-integer user-id)))
	(cp (contact-pair user-id contact-id)))
    (unless (member cp (conts backend) :test #'equal)
      (pushnew enq (enqs backend) :test #'equal))))

(defmethod unsubscr ((backend plain-backend) user-id contact-id)
  (setf (conts backend)
	(delete (contact-pair user-id contact-id)
		(conts backend) :test #'equal)))

(defmethod get-contacts ((backend plain-backend) user-id)
  (let* ((id (parse-integer user-id))
	 (cs (remove-if-not (lambda (c)
			      (or (equal id (car c))
				  (equal id (cdr c))))
			    (conts backend))))
    (mapcar (lambda (c)
	      (if (equal id (car c))
		  (cdr c)
		  (car c)))
	    cs)))

(defmethod get-enquiries ((backend plain-backend) user-id)
  (let* ((id (parse-integer user-id))
	 (enq (remove-if-not (lambda (e) (equal id (car e)))
			     (enqs backend))))
    (mapcar #'cdr enq)))

(defmethod accept-enq ((backend plain-backend) user-id contact-id)
  (let ((enq (cons (parse-integer user-id)
		   (parse-integer contact-id)))
	(cp (contact-pair user-id contact-id)))
    (princ enq)
    (when (member enq (enqs backend) :test #'equal)
      (princ "inner")
      (setf (enqs backend) (delete enq (enqs backend) :test #'equal))
      (pushnew cp (conts backend) :test #'equal))))

(defmethod refuse-enq ((backend plain-backend) user-id contact-id)
  (let ((enq (cons (parse-integer user-id)
		   (parse-integer contact-id))))
    (setf (enqs backend) (delete enq (enqs backend) :test #'equal))))

;;; locations
;;location: id => (place ((:x . x) (:y . y)) exptime)

(defun print-loc (key val)
  (let ((place (car val))
	(pos (json:encode-json-to-string (cadr val)))
	(exp (caddr val)))
    (format t "User #~a, sits at pos ~a in the ~a until ~a.~%"
	    key pos place exp)))

(defun describe-locs (backend)
  (maphash #'print-loc (locs backend)))

(defun expire-locs (backend)
  (let ((now (get-universal-time)))
    (maphash (lambda (key loc)
	       (when (< (caddr loc) now)
		 (remhash key (locs (backend)))))
	     (locs backend))))

(defun find-loc (backend id)
  (let ((loc (gethash id (locs backend))))
    (list (car loc) (cadr loc))))

(defmethod get-locations ((backend plain-backend) user-id place)
  (expire-locs backend)
  (let* ((id (parse-integer user-id))
	 (cs (get-contacs backend user-id)))
    (mapcan (lambda (c)
	      (let ((loc (find-loc backend c)))
		(when (equal place (car loc))
		  (list (cons :id c)
			(cons :loc (cadr loc))))))
	    cs)))

(defmethod set-location ((backend plain-backend) user-id pos place duration)
  (let ((id (parse-integer user-id))
	(p (json:decode-json-from-string pos))
	(dur (parse-integer duration)))
    (setf (gethash id (locs backend))
	  (list place p (+ dur (get-universal-time))))))

(defmethod get-location ((backend plain-backend) user-id)
  (expire-locs backend)
  (let* ((id (parse-integer user-id))
	 (loc (find-loc backend id)))
    (when loc
      (list (cons :pl (car loc))
	    (cons :pos (cadr loc))))))

(defmethod remove-location ((backend plain-backend) user-id)
  (remhash (parse-integer user-id) (locs backend)))