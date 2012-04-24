;;;; m-server.lisp

(in-package #:m-server)

(defvar *current-acp* nil)

(defun start-http-server ()
  (when *current-acp*
    (hunchentoot:stop *current-acp*))
  (setf *current-acp* (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (hunchentoot:start *current-acp*))

(defun start-https-server ()
  (when *current-acp*
    (hunchentoot:stop *current-acp*))
  (setf *current-acp*
	(make-instance 'hunchentoot:easy-ssl-acceptor :port 8080
		       :ssl-privatekey-file (find-comp '("ssl" "server.key"))
		       :ssl-certificate-file (find-comp '("ssl" "server.crt"))))
  (hunchentoot:start *current-acp*))

(defun stop-server ()
  (when *current-acp*
    (hunchentoot:stop *current-acp*)
    (setf *current-acp* nil)))

(defmacro defget (name (&rest params) &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,(format nil "/~(~a~)" name))
       ,params
     ;(setf (hunchentoot:content-type*) "application/json")
     (json:encode-json-to-string (progn ,@body))))

(defmacro defpost (name (&rest params) &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,(format nil "/~(~a~)" name))
       ,params
     ,@body
     nil))