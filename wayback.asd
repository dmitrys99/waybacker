(in-package :cl-user)

; Load this file and then run
; (asdf:operate 'asdf:load-op :waybacker)

#+ALLEGRO
(require :aserve)

(asdf:defsystem :waybacker
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :description "Use Wayback Machine at archive.org to update web pages and blogs"
    :licence "Lesser Lisp General Public License"
    ; +++ not sure wuwei is actually needed
    :depends-on (#-:ALLEGRO :aserve
			    :mtlisp :wuwei 
			    :drakma :s-xml :cl-json :cl-smtp  :cl-ppcre)
    :serial t
    :components 
    ((:file "pkg")
     (:file "utils")
     (:file "wayback")
     (:file "oauth2-google")
     (:file "blogger")
     (:file "web-ui")
     ))
