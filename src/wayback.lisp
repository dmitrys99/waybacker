(in-package :wb)

(setq net.uri:*strict-parse* nil)	;slack!

;;; works with drakma, not so well with net.aserve.client, which should not be used...
(defparameter *user-agent* :firefox)	;pretend to be firefox -- just a string like "waybacker" results in some bad responses

;;; Returns list of URLs. Not sure if this works well for things with only very old archives...
(defun waybacks (url)
  (let* ((uri (puri:uri url))
	 (frag? (puri:uri-fragment uri))
	 (unfragged (if frag? (progn
				(setf (puri:uri-fragment uri) nil)
				(princ-to-string uri))
			url))
	 (dir-url (format nil "http://wayback.archive.org/web/*/~A" unfragged))
	 (directory (drakma:http-request dir-url :user-agent :drakma))) ;for archive.org, let's be more honest.
    (flet ((refrag (url)
	     (if frag?
	       (format nil "~A#~A" url frag?)
	       url)))
      (mapcar #'refrag
	      (remove-duplicates (match-re-multiple "\"(http://web.archive.org/web/.+?/.+?)\"" directory) :test #'string-equal)))))

;;; Return the most recent wayback url corresponding to argument
(mt:def-cached-function wayback (url)
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

(defun defrag (url)
  (let ((uri (puri:uri url)))
    (setf (puri:uri-fragment uri) nil)
    (princ-to-string uri)))

(defparameter *timeout* 30)		;seconds

;;; Returns: T/NIL, if nil, code and error msg as second and third values
(defun check-url (url &key full?)
  (handler-case
      (let ((url (defrag url)))
	(acl-compat.mp:with-timeout (*timeout* (return-from check-url (values nil 999 "Timed out")))
	  (multiple-value-bind (content resp headers uri stream foo msg)
	      (drakma:http-request url :user-agent *user-agent* :method (if full? :get :head)) ;  soemtimes works, sometimes not
	    (declare (ignore content headers uri stream foo))
	    (case resp
	      (200 t)
	      ((405 403)
	       (if full?
		   (values nil resp msg)
		   (check-url url :full? t)))
	      (t
	       (values nil resp msg))))))
    (error (condition)
      (values nil (type-of condition) (princ-to-string condition)))
    ))
	     
(defun process-page (url &rest keys)
  (apply #'process-text (drakma:http-request url :user-agent *user-agent*) keys))

(defun url-excluded? (url excludes)
  (some #'(lambda (exclude) (search exclude url)) excludes))

;;: Find URLs, check if they are live, if not try and substitute them
;;; +++ what about not http: urls?
;;; reporter is a fn of three arguments, URL, STATUS (:GOOD, :BAD, or a HTTP fail status code), optional subst, option error message
(defun process-text (text &key excludes reporter)
  (let* ((url-idxs (match-re-multiple "[\\\"\\\'](https?://.+?\\..+?)[\\\"\\\']" text :return :index))
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
		      (multiple-value-bind (ok? code error-string)
			  (check-url url)
			(if ok?
			    (progn (princ "OK") (incf good) (when reporter (funcall reporter url :good)))
			    (progn
			      (format t "bad ~A: ~A, looking for substitute..." code error-string)
			      (incf bad)
			      (setf sub (mt:report-and-ignore-errors (wayback url)))
			      (when sub
				(incf substitutes)
				(format t "found ~A" sub)
				(mt:collect (list url sub)))
			      (when reporter (funcall reporter url code sub error-string))
			      ))))))))))
    (format t "~%~A URLs total, ~A excluded, ~A good, ~A bad, ~A substitutes" total excluded good bad substitutes)
    results))

(defun transform-text (text &rest options)
  (dolist (transform (apply #'process-text text options) text)
    (setq text (mt:string-replace text (car transform) (cadr transform)))))

;;; Given a URL, fix up its contents and print the transformed result on the console.
(defun transform-url (url &rest options)
  (princ (apply #'transform-text (drakma:http-request url :user-agent *user-agent*) options))
  nil)
