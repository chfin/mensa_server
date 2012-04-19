;;;; m-server-hunchentoot.asd

(asdf:defsystem #:m-server-tpd2
    :serial t
    :depends-on (#:m-server #:teepeedee2 #:babel)
    :components ((:file "tpd2-frontend")
		 (:file "server")))