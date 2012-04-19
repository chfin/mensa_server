;;;; tpd2-frontend.lisp

(in-package #:m-server)

(defun convert-param (param)
  `(,param (babel:octets-to-string ,param)))

(defmacro defget (name params &body body)
  `(tpd2:defpage ,(format nil "/~(~a~)" name) ,params
     (tpd2:with-sendbuf ()
       (let ,(mapcar #'convert-param params)
	 ,@body))))

(defmacro defpost (name params &body body)
  `(tpd2:defpage ,(format nil "/~(~a~)" name) ,params
     (let ,(mapcar #'convert-param params)
       ,@body)
     (make-instance 'tpd2:sendbuf)))