;;;; m-server-tpd2.asd

(asdf:defsystem #:m-server-tpd2
    :serial t
    :depends-on (#:m-server-core #:teepeedee2 #:babel #:bordeaux-threads)
    :components ((:file "tpd2-frontend")
		 (:file "server")))