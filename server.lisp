;;;; server.lisp

(in-package #:m-server)

(defvar *backend* (make-instance 'plain-backend))

(defget echo (p)
  (format nil "You said: ~a" p))

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
  (search-accounts *backend* term))

;;; locations

(defget places ()
  (get-places *backend*))

;;; contacts

(defpost subscribe (id pw ci)
	 (do-auth (id pw)
	   (subscr *backend* id ci)))

(defpost unsubscribe (id pw ci)
	 (do-auth (id pw)
	   (unsubscr *backend* id ci)))

(defget contacts (id pw)
  (do-auth (id pw)
    (get-contacts *backend* id)))

(defget enquiries (id pw)
  (do-auth (id pw)
    (get-enquiries *backend* id)))

(defpost accept (id pw ci)
	 (do-auth (id pw)
	   (accept-enq *backend* id ci)))

(defpost refuse (id pw ci)
	 (do-auth (id pw)
	   (refuse-enq *backend* id ci)))