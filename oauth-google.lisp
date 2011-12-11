(in-package :wb)

(ql:quickload '(:cl-oauth :s-xml))
(load "~/repos/swframes/lib/lxml.lisp")	; +++ temp loc
(use-package :cl-oauth)
(use-package :wuwei)
(use-package :lxml)

(defparameter *key* "wuwei.name")
(defparameter *secret* "xoHi0NAM5RW9_C0sK-M5VUuB")

(defparameter *callback-uri* "http://4en5.localtunnel.com/oauth")
(defparameter *callback-port* 8080
  "Port to listen on for the callback")

(defparameter *get-request-token-endpoint* "https://www.google.com/accounts/OAuthGetRequestToken")
(defparameter *auth-request-token-endpoint* "https://www.google.com/accounts/OAuthAuthorizeToken")
(defparameter *get-access-token-endpoint* "https://www.google.com/accounts/OAuthGetAccessToken")
(defparameter *consumer-token* (make-consumer-token :key *key* :secret *secret*))

(def-session-variable *request-token* nil)
(def-session-variable *access-token* nil)

(defun get-access-token ()
  (obtain-access-token *get-access-token-endpoint* *request-token*))

;;; get a request token
(defun get-request-token (scope)
  ;; TODO: scope could be a list.
  (obtain-request-token
    *get-request-token-endpoint*
    *consumer-token*
    :callback-uri *callback-uri*
    :user-parameters `(("scope" . ,scope))))

(defun obtain-access ()
;  (setf *request-token* (get-request-token "http://www.google.com/calendar/feeds/"))
  (setf *request-token* (get-request-token "http://www.blogger.com/feeds/"))
  (let ((auth-uri (make-authorization-uri *auth-request-token-endpoint* *request-token*)))
    (format t "Please authorize the request token at this URI: ~A~%" (puri:uri auth-uri))))

(def-session-variable *request-token* nil)

(publish :path "/obtain"
	 :function #'(lambda (req ent)
		       (wu:with-session (req ent) 
			 (setf *request-token* (get-request-token "http://www.blogger.com/feeds/"))
			 (let ((auth-uri (make-authorization-uri *auth-request-token-endpoint* *request-token*)))
			   (net.aserve:with-http-response (req ent)
			     (net.aserve:with-http-body (req ent)
			       (html
				 (render-scripts
				   (:redirect auth-uri)))))))))

(net.aserve:start :port 8080)

(publish :path "/"
	 :function #'(lambda (req ent)
		       (net.aserve:with-http-response (req ent)
			 (net.aserve:with-http-body (req ent)
			   (html (:h2 (:princ "sanity check"))
				 (:princ "you fail"))))))


(net.aserve:publish :path "/oauth"	;defined by *callback-url*
		    :function 'oauth-aserve)

(defun coerce-to-string (ss)
  (if (stringp ss)
      ss
      (babel:octets-to-string ss :encoding :iso-8859-1)))

(defun parse-xml (xml)
  (let ((s-xml:*ignore-namespaces* t))
    (s-xml:parse-xml-string (coerce-to-string xml) :output-type :lxml)))

(defun oauth-aserve (req ent)
  (setq *request* req)
  (wu:with-session (req ent)
    (handler-case
	(authorize-request-token-from-request
	 (lambda (rt-key)
	   (assert *request-token*)
	   (unless (equal (url-encode rt-key) (token-key *request-token*))
	     (warn "Keys differ: ~S / ~S~%" (url-encode rt-key) (token-key *request-token*)))
	   *request-token*))
      (error (c)
	(warn "Couldn't verify request token authorization: ~A" c)))
    (when (request-token-authorized-p *request-token*)
      (format t "Successfully verified request token with key ~S~%" (token-key *request-token*))
      (setf *access-token* (get-access-token))
      ;; test request:
      (let* ((result (access-protected-resource
		     "http://www.blogger.com/feeds/default/blogs"
		     *access-token*))
	     (blogs
	      (blog-list-interpreter (parse-xml result))))
	(net.aserve:with-http-response (req ent)
	  (net.aserve:with-http-body (req ent)
	    (html 
	      (:h2 (:princ "blogs gotten"))
	      (:ul
	       (dolist (b blogs)
		 (html
		   (:li ((:a :href (process-blog-link b)) (:princ (car b))))))))))))))




;;; Doesn't deal with >page of blogs (who has that many)
(defun blog-list-interpreter (lxml)
  (let* ((blog-entries
	  (lxml::lxml-find-elements-with-tag lxml :|entry|))
	 (blog-names
	  (mapcar #'(lambda (e)
		      (cadr (lxml::lxml-subelement e :|title|) ))
		  blog-entries))
	 (blog-ids
	  (mapcar #'(lambda (e)
		      ;; +++ have to pull actual blog id out of this with a regexp
		      (cadr (lxml::lxml-subelement e :|id|) ))
		  blog-entries)	  ))
    (mapcar #'list blog-names blog-ids)))

(defun process-blog-link (b)
  (format nil "/process-blog?id=~A" (cadr b)))

(publish :path "/process-blog"
	 :function 'process-blog)

(defun process-blog (req ent)
  (wu:with-session (req ent)
    (let ((id (net.aserve:request-query-value "id" req)))
      (let* ((url (format nil "http://www.blogger.com/feeds/~A/posts/default" id)) ;supposedly all posts on omni?
	     (result (access-protected-resource url *access-token*))
	     (xml (parse-xml result))
	     (total (parse-integer (cadr (lxml-subelement xml :|openSearch:totalResults|))))
	     (per-page (parse-integer (cadr (lxml-subelement xml :|openSearch:itemsPerPage|))))
	     (logfile "/tmp/waybacker-log.html")) ;+++ temp of course
	;; we have first page
	(dolist (entry (lxml-find-elements-with-tag xml :|entry|))
	  (process-entry entry :logfile logfile))
	(dotimes (page (ceiling total per-page))
	  (unless (zerop page)
	    (process-page id page per-page :logfile logfile)))))))

(defun process-page (id page page-size &key logfile)
  (let* ((url (format nil "http://www.blogger.com/feeds/~A/posts/default?start-index=~A&max-results=~A" id (* page page-size) page-size))
	 (result (access-protected-resource url *access-token*))
	 (xml (parse-xml result)))
    (dolist (entry (lxml-find-elements-with-tag xml :|entry|))
      (process-entry entry :logfile logfile))))

(defmacro with-logging ((s file) &body body)
  `(with-open-file (,s ,file :direction :output :if-exists :append :if-does-not-exist :create)
     (let ((*html-stream* s))
       (html ,@body))))


(defun process-entry (entry-xml &key update? logfile)
  (let ((content (cadr (lxml-subelement entry-xml :|content|)))
	(title (cadr (lxml-subelement entry-xml :|title|)))
	(good 0)
	(total 0)
	(subs 0)
	(fails 0))
    (with-logging (s logfile)
      (:princ (format nil "Working on ~A" title))
      :br)
    (process-text content
		  :reporter
		  #'(lambda (url status &optional problem sub)
		      (incf total)
		      (case status
			(:good (incf good))
			(:bad
			 (if sub (incf subs) (incf fails))
			 (with-open-file (s logfile :direction :output :if-exists :append :if-does-not-exist :create)
			   (let ((*html-stream* s))
			     (html (:li
					  ((:a :href url)
					   (:princ-safe url))
					  :br
					  (:princ-safe problem)
					  :br
					  (when sub
					    (html
					      ((:a :href sub)
					       (:princ-safe sub))))))))))))
    (with-open-file (s logfile :direction :output :if-exists :append :if-does-not-exist :create)
      (let ((*html-stream* s))
	(html (:br (:princ (format nil "~A total links, ~A still valid, ~A substitutions, ~A failures"
				   total
				   good
				   subs
				   fails)))
	      :hr)))))



      
	 
	 
