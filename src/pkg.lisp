(in-package :cl-user)

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

