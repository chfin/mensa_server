;;;; tpd2-frontend.lisp

(in-package #:m-server)

(defun convert-param (param)
  `(,param (babel:octets-to-string ,param)))

(defvar *con* nil
  "tpd2 connection object")
(defvar *loop-thread* nil
  "tpd2 event loop thread")

(defun start-http-server ()
  (stop-server)
  (setf *con* (tpd2:http-start-server 8080))
  (setf *loop-thread*
	(bordeaux-threads:make-thread #'tpd2:event-loop
				      :name "tpd2-thread")))

(defun stop-server ()
  (when *con*
    (tpd2:hangup *con*)
    (setf *con* nil))
  (when *loop-thread*
    (bordeaux-threads:destroy-thread *loop-thread*)
    (setf *loop-thread* nil)))

(defmacro defget (name params &body body)
  `(tpd2:defpage ,(format nil "/~(~a~)" name) ,params
     (tpd2:with-sendbuf ()
       (let ,(mapcar #'convert-param params)
	 (json:encode-json-to-string (progn ,@body))))))

(defmacro defpost (name params &body body)
  `(tpd2:defpage ,(format nil "/~(~a~)" name) ,params
     (let ,(mapcar #'convert-param params)
       ,@body)
     (make-instance 'tpd2:sendbuf)))