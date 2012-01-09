(in-package :cl-user)

(defvar *wb-here* *load-pathname*)
(load "~/quicklisp.setup.lisp")

;;; NOTE: this is patched from default version
(load "/misc/repos/portableaserve/aserve/aserve.asd")	;need more uptodate version
(load "/misc/repos/wuwei/wuwei.asd")	;need more uptodate version

(load (merge-pathnames "wayback.asd" *wb-here*))

(ql:quickload :waybacker)

(load (make-pathname :defaults *wb-here* :name "secrets"))
(net.aserve:start :port 8080)
(in-package :wb)
