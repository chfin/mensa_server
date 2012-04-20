;;;; m-server.asd

(asdf:defsystem #:m-server
    :serial t
    :depends-on (#:m-server-core #:hunchentoot)
    :components ((:file "ht-frontend")
		 (:file "server")))