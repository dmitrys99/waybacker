(in-package :cl-user)

(print ">>> Building system....")

;;; +++ need fixed version of wuwei?

(load (make-pathname :directory *build-dir* :defaults "wayback.asd"))

(ql:quickload :wayback)

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
