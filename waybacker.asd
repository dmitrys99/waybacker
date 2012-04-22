(in-package :cl-user)

#+ALLEGRO
(require :aserve)

(asdf:defsystem :waybacker
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :description "Use Wayback Machine at archive.org to update web pages and blogs"
    :licence "Lesser Lisp General Public License"
    :depends-on (#-:ALLEGRO :aserve
			    :mtlisp :wuwei :clsql
			    :drakma :s-xml :cl-json :cl-smtp  :cl-ppcre
			    :zs3	;heroku only
			    :cl-paypal	;hey why not
			    )
    :serial t
    :components 
    ((:static-file "wayback.asd")
     (:module :lib
	      :components
	      ((:file "lxml")))
     (:module :src
	      :serial t
	      :components
	      ((:file "pkg")
	       (:file "utils")
	       (:file "wayback")
	       (:file "oauth2-google")
	       (:file "blogger")
	       (:file "web-ui")
	       (:file "zs3")		
	       (:file "heroku")
	       ))))
