(in-package :cl-user)

(print ">>> Building system....")

(load (make-pathname :directory *build-dir* :defaults "waybacker.asd"))

(ql:quickload :waybacker)

;;; Copy wuwei public files to build
(wu:heroku-install-wupub-files)

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
