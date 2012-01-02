(in-package :wb)

;;; Experiments in incremental refresh

(publish :path "/incremental"
	 :function 'incremental)

(defun incremental (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:html
      (:head
       (javascript-includes "prototype.js"))
      (:body
       (:h1 "watch me grow")
       ((:table :id "table")
	((:tr :id "tr0")
	 (:td (:princ "0"))
	 (:td (:princ "start me up"))))
       (let ((i 0))
	 (link-to-remote "foo"
			 (ajax-continuation (:keep t)
			   (render-update 
			     (:insert :bottom "table" (html (:tr (:td (:princ (incf i))) (:td "foobar")))))))))))))


;;; test with a realer generator
(defun incremental (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:html
      (:head
       (javascript-includes "prototype.js"))
      (:body
       (:h1 "watch me grow")
       ((:table :id "table")
	((:tr :id "tr0")
	 (:td (:princ "0"))
	 (:td (:princ "start me up"))))
       (:script
	(:princ
	 (remote-function
	  (ajax-continuation (:keep t)
	    (render-update 
	      (:insert :bottom "table" (html (:tr (:td (:princ "yo")) (:td "foobar"))))))
	  :in-function? nil)
	 )))))))


;;; OK, this version kind of works.

(defmacro with-incremental-emission (() &body body)
  `(let ((q nil))
     (labels ((emit (string)
	      (mt:push-end string q))
	    (get-next-contents ()
	      (without-interrupts
		(dolist (elt q)
		  (write-string elt *html-stream*))
		(setq q nil))
	      (html-write-getter))
	    (html-write-getter ()
	      (html
	       (:script
		(:princ
		 (remote-function
		  (ajax-continuation ()
		    (mp:process-wait "waiting" #'(lambda () (not (null q))))
		    (render-update 
		      (:insert :bottom "container" (get-next-contents))))
		  :in-function? nil)
		 )))))
       ;; starter
       (html-write-getter)
       (mt:in-background "incremental" 
	 ,@body 
;       (html-write-terminator)
	 ))))

(defun incremental (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:html
      (:head
       (javascript-includes "prototype.js"))
      (:body
       (:h1 "watch me grow")
       ((:table :id "container")
	((:tr :id "tr0")
	 (:td (:princ "0"))
	 (:td (:princ "start me up"))))
       (with-incremental-emission ()
	 (dotimes (i (random 20))
	   (sleep (random 15))
	   (emit (html-string (:tr (:td (:princ i)) (:td (:princ "blah")))))))       
       )))))



;;; Redefines
;;; An incremental version (no content yet, just the inc display)
(defun wayback-response (req ent)
  (with-http-response-and-body (req ent)
    (let ((url (request-query-value "url" req))
	  (options nil))
      (html
       (:html
	(:head
	 (javascript-includes "prototype.js"))
	(:body
	 (:h3 (:princ-safe (format nil "Repair log for ~A" url)))
	 ((:table :id "container")
	  )
	 (with-incremental-emission ()
	   (process-url url :reporter #'(lambda (url &optional status problem sub)
					  (emit (html-string (:tr (:td (:princ-safe url))
								  (:td (:princ status))
								  (:td (:princ-safe problem))
								  (:td (:princ-safe sub)))))))
	   )))))))
