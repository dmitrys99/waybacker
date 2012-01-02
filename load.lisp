(defvar *waybacker-dir*  (pathname-directory *load-pathname*))

;;; need patched version of this, not in ql yet

;;; These don't work, and shouldn't be necessary...I am baffled.
;(load "~/repos/aserve/aserve.asd")	;need more uptodate version
;(load "~/repos/aserve/load.cl")
(load "~/repos/wuwei/wuwei.asd")	;need more uptodate version
(load (make-pathname :directory (append (butlast *waybacker-dir*) '("cl-oauth")) :defaults "cl-oauth.asd"))
(load (make-pathname :directory *waybacker-dir* :defaults "wayback.asd"))


(ql:quickload :waybacker)

(load (make-pathname :directory *waybacker-dir* :defaults "secrets.lisp"))
