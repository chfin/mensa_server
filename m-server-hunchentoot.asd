;;;; m-server-hunchentoot.asd

(asdf:defsystem #:m-server-hunchentoot
    :serial t
    :depends-on (#:m-server #:hunchentoot)
    :components ((:file "ht-frontend")
		 (:file "server")))