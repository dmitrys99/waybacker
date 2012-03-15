(in-package :cl-user)

(print ">>> Building system....")

(load (make-pathname :directory *build-dir* :defaults "waybacker.asd"))

(ql:quickload :waybacker)

;;; Copy wuwei public files to build
(asdf:run-shell-command
 (format nil "cp -r ~Apublic ~A"
	 (namestring (asdf:component-pathname (asdf:find-system :wuwei)))
 	 (namestring (make-pathname :directory (append *build-dir* '("wupub")))) 
	 ))

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
