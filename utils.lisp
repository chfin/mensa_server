;;;; utils.lisp

(in-package #:m-server)

(defun find-comp (lst)
  (labels ((find-c (lst)
	     (if lst
		 (asdf:find-component (find-c (cdr lst)) (car lst))
		 "m-server-core")))
    (asdf:component-pathname (find-c (reverse lst)))))