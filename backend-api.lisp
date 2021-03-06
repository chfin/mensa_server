;;;; backend-api.lisp

(in-package #:m-server)

(defparameter *hu-exp-time* 60
  "The expiration time of a holdup signal in seconds.")
(defparameter *hu-threshold* 3
  "The required number of signals for a holdup.")

(defclass base-backend ()
  ((places :initform '("mensa" "pub")
	   :reader get-places)))

(defmacro do-auth ((id pw) &body body)
  `(when (authenticate *backend* ,id ,pw)
     ,@body))

(defgeneric holdup-signaled? (backend user-id place))
(defgeneric signal-holdup (backend user-id place))
(defgeneric holdup? (backend place))

(defgeneric authenticate (backtend user-id pw))
(defgeneric get-id (backend mail pw))
(defgeneric get-info (backend user-id))

(defgeneric new-account (backend mail pw name))
(defgeneric activate-account (backend mail code))
(defgeneric delete-account (backend user-id))
(defgeneric search-accounts (backend term))

(defgeneric subscr (backend user-id contact-id))
(defgeneric unsubscr (backend user-id contact-id))
(defgeneric get-contacs (backend user-id))

(defgeneric get-enquiries (backend user-id))
(defgeneric accept-enq (backend user-id contact-id))
(defgeneric refuse-enq (backend user-id contact-id))

(defgeneric get-locations (backend user-id place))
(defgeneric set-location (backend user-id pos place duration))
(defgeneric get-location (backend user-id))
(defgeneric remove-location (backend user-id))