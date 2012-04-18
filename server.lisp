;;;; server.lisp

(in-package #:m-server)

(defparameter *backend* (make-instance 'plain-backend))

(defmacro defget (name (&rest params) &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,(format nil "/~(~a~)" name))
       ,params
     (setf (hunchentoot:content-type*) "application/json")
     ,@body))

(defmacro defpost (name (&rest params) &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,(format nil "/~(~a~)" name))
       ,params
     ,@body
     nil))

(defmacro do-auth ((id pw) &body body)
  `(when (authenticate *backend* ,id ,pw)
     ,@body))

;;;holdups

(defget holdup (p)
  (if (holdup? *backend* p)
      "true"
      "false"))

(defpost signalholdup (id pw pl)
	 (do-auth (id pw)
	   (signal-holdup *backend* id pl)))

(defget holdupsignaled (id pw pl)
  (do-auth (id pw)
    (if (holdup-signaled? *backend* id pl)
	"true"
	"false")))

;;; accounts

(defget getid (mail pw)
  (get-id *backend* mail pw))

(defget getinfo (id pw ri)
  (do-auth (id pw)
    (get-info *backend* ri)))

(defget register (mail pw name)
	 (new-account *backend* mail pw name))

(defpost activate (mail code)
	 (activate-account *backend* mail code))

(defpost unregister (id pw)
	 (do-auth (id pw)
	   (delete-account *backend* id)))

(defget search_acc (term)
  (json:encode-json-to-string (search-accounts *backend* term)))

;;; locations

(defget places ()
  (json:encode-json-to-string (get-places *backend*)))

;;; contacts
