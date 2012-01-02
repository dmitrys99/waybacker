(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/repos/swframes/lib/lxml.lisp"))	; +++ temp loc

(defpackage :waybacker
  (:nicknames :wb)
  (:use :cl :net.aserve
	:net.html.generator :wu
;	:cl-oauth
	:wuwei :lxml
	)
  ;; sigh
  (:shadowing-import-from :net.aserve "REQUEST-URI" "REQUEST-METHOD")
  )

