(defvar *waybacker-dir*  (pathname-directory *load-pathname*))

;;; need patched version of this, not in ql yet
(load (make-pathname :directory (append (butlast *waybacker-dir*) '("cl-oauth")) :defaults "cl-oauth.asd"))
(load (make-pathname :directory *waybacker-dir* :defaults "wayback.asd"))

(ql:quickload :waybacker)

