(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(#-ALLEGRO :aserve :mtlisp :cl-ppcre)))

#+ALLEGRO
(require :aserve)

(defparameter *user-agent* "waybacker")

;;; Returns list of URLs. Not sure if this works well for things with only very old archives...
(defun waybacks (url)
  (let* ((dir-url (format nil "http://wayback.archive.org/web/*/~A" url))
	 (directory (net.aserve.client:do-http-request dir-url :user-agent *user-agent*)))
    (remove-duplicates (match-re-multiple "\"(http://web.archive.org/web/.+?/.+?)\"" directory) :test #'string-equal)))

(defun wayback (url)
  (car (sort (waybacks url) #'string-greaterp)))

;;; for utilities
(defun match-re-multiple (re string &key (return :string))
  (do ((finger 0)
       (result nil))
      (())
    (multiple-value-bind (start end gstarts gends)
	(ppcre:scan re string :start finger)
      (if start
	  (progn
	    (push (ecase return
		    (:string (subseq string (svref gstarts 0) (svref gends 0)))
		    (:index (cons (svref gstarts 0) (svref gends 0))))
		  result)
	    (setq finger end))
	  (return (nreverse result))))))

(defun check-url (url &key full?)
  (mt:report-and-ignore-errors
    (multiple-value-bind (content resp headers)
	(net.aserve.client::do-http-request url :user-agent *user-agent* :method :head) ;  soemtimes works, sometimes not
      (declare (ignore content headers))
      (case resp
	(200 t)
	(405
	 (if full?
	     (values nil resp)
	     (check-url url :full? t)))
	(t
	 (values nil resp))))))
	     
(defun process-url (url &rest keys)
  (apply #'process-text (net.aserve.client:do-http-request url :user-agent *user-agent*) keys))

(defun url-excluded? (url excludes)
  (some #'(lambda (exclude) (search exclude url)) excludes))

;;: Find URLs, check if they are live, if not try and substitute them
(defun process-text (text &key excludes reporter)
  (let* ((url-idxs (match-re-multiple "[\\\"\\\'](http://.+?\\..+?)[\\\"\\\']" text :return :index))
	 (total (length url-idxs))
	 (excluded 0) (good 0) (bad 0) (substitutes 0)
	 (results
	  (mt:collecting
	    (dolist (url-idx url-idxs)
	      (let ((url (subseq text (car url-idx) (cdr url-idx)))
		    sub)
		(if (url-excluded? url excludes)
		    (incf excluded)
		    (progn
		      (format t "~%Checking ~A..." url)	
		      (multiple-value-bind (ok? problem)
			  (check-url url)
			(if ok?
			    (progn (princ "OK") (incf good) (when reporter (funcall reporter url :good)))
			    (progn
			      (format t "bad ~A, looking for substitute..." problem)
			      (incf bad)
			      (setf sub (wayback url))
			      (when sub
				(incf substitutes)
				(format t "found ~A" sub)
				(mt:collect (list url sub)))
			      (when reporter (funcall reporter url :bad problem sub))
			      ))))))))))
    (format t "~%~A URLs total, ~A excluded, ~A good, ~A bad, ~A substitutes" total excluded good bad substitutes)
    results))

(defun transform-text (text &rest options)
  (dolist (transform (apply #'process-text text options) text)
    (setq text (mt:string-replace text (car transform) (cadr transform)))))

;;; Given a URL, fix up its contents and print the transformed result on the console.
(defun transform-url (url &rest options)
  (princ (apply #'transform-text (net.aserve.client:do-http-request url :user-agent *user-agent*) options))
  nil)
