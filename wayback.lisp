(ql:quickload '(:aserve :mtlisp :cl-ppcre))

#|
Status: basically working 

Todo:
- a UI!
- exclude self URLs
- capture errors
- handle "bad" URLs (eg with ?)
- blogger interface
|#

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
  (multiple-value-bind (content resp headers)
      (net.aserve.client::do-http-request url :user-agent *user-agent* :method :head) ;  soemtimes works, sometimes not
    (declare (ignore content headers))
    (case resp
      (200 t)
      (405
       (if full?
	   nil				;+++ should log or whatever here too
	   (check-url url :full? t)))
      (t
       (format t "~%~A: ~A" url resp)
       nil))))
	     
;;: Find URLs, check if they are live, if not try and substitute them
(defun process-text (text)
  (let ((url-idxs (match-re-multiple "[\\\"\\\'](http://.*?)[\\\"\\\']" text :return :index)))
    (dolist (url-idx url-idxs)
      (let ((url (subseq text (car url-idx) (cdr url-idx))))
	  (format t "~%Checking ~A..." url)	
	(if (mt:report-and-ignore-errors (check-url url))
	    (princ "OK")
	    (progn
	      (format t "bad, looking for substitute...")
	      (setq wurl (wayback url))
	      (princ wurl)))))))

za
