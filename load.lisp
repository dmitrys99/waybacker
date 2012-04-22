(in-package :cl-user)

;;; Local load
(load "~/quicklisp/setup.lisp")

(defvar *build-dir* (pathname-directory (truename *load-pathname*)))

;;; Load local copies of portableaserve and wuwei, since quicklisp's are broken
;;; (You can get these from github.com/mtravers.  Put them the repos directory).
(let* ((asds (directory (make-pathname :directory  (append *build-dir* '( "repos" :wild-inferiors))
				       :name :wild
				       :type "asd")))
       (directories (remove-duplicates (mapcar #'pathname-directory asds) :test #'equal)))
  (dolist (d directories)
    (push (make-pathname :directory d) asdf:*central-registry*)))

(load (make-pathname :directory *build-dir* :defaults "heroku-setup.lisp"))

(load (make-pathname :directory *build-dir* :defaults "secrets"))

(initialize-application)

(net.aserve:start :port 1666)
(in-package :wb)
