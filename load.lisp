(defvar *wb-here* *load-pathname*)

;;; need patched version of this, not in ql yet

;;; NOTE: this is patched from default version
(load "~/repos/portableaserve/aserve/aserve.asd")	;need more uptodate version
;(load "~/repos/aserve/load.cl")

(load "~/repos/wuwei/wuwei.asd")	;need more uptodate version
(load (merge-pathnames "wayback.asd" *wb-here*))

(ql:quickload :waybacker)

(load (make-pathname :defaults *wb-here* :name "secrets"))
(net.aserve:start :port 8080)
(in-package :wb)
