(in-package :cl-user)

#+ALLEGRO
(require :aserve)

(asdf:defsystem :waybacker
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :description "Use Wayback Machine at archive.org to update web pages and blogs"
    :licence "Lesser Lisp General Public License"
    ; +++ not sure wuwei is actually needed
    :depends-on (#-:ALLEGRO :aserve :mtlisp :cl-ppcre :wuwei)
    :serial t
    :components 
    ((:file "pkg")
     (:file "wayback")
     (:file "wayback-ui")
     ))
