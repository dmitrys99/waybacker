(in-package :wb)

(defparameter *key* "wuwei.name")
(defparameter *secret* "xoHi0NAM5RW9_C0sK-M5VUuB")

;;; Values from API console (not sure these are right, and doesn't help)
;(defparameter *key* "453380357476-k2h67ojck9ou4sce6r2jvgml8fsq263b.apps.googleusercontent.com")
;(defparameter *secret* "IGiueat52twXud_xqS5OuY7X")

(defparameter *callback-uri* "http://56hp.localtunnel.com/oauth")
(defparameter *callback-port* 8090
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

;;; No longer called
(defun obtain-access ()
;  (setf *request-token* (get-request-token "http://www.google.com/calendar/feeds/"))
  (setf *request-token* (get-request-token "http://www.blogger.com/feeds/"))
  (let ((auth-uri (make-authorization-uri *auth-request-token-endpoint* *request-token*)))
    (format t "Please authorize the request token at this URI: ~A~%" (puri:uri auth-uri))))

(def-session-variable *request-token* nil)

(publish :path "/obtain"
	 :content-type "text/html"
	 :function #'(lambda (req ent)
		       (wu:with-session (req ent) 
			 (setf *request-token* (get-request-token "http://www.blogger.com/feeds/"))
			 (let ((auth-uri (make-authorization-uri *auth-request-token-endpoint* *request-token*)))
			   (net.aserve:with-http-response (req ent)
			     (net.aserve:with-http-body (req ent)
			       (html
				 (render-scripts
				   (:redirect auth-uri)))))))))

;(net.aserve:start :port *callback-port*)

(publish :path "/"
	 :content-type "text/html"
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

(defun access-protected-resource-with-error (url &rest other-args)
  (multiple-value-bind  (result status prob-hint prob-advice)
      (apply #'access-protected-resource url *access-token* other-args)
    (case status
      (200 t)
      (t (error "Failed to get protected resource ~A: ~A: ~A: ~A" url status prob-hint prob-advice)))))

(defun oauth-aserve (req ent)
  (setq *request* req)
  (wu:with-session (req ent)
    (progn ; wu:with-html-error-handling	;+++ doesn't really work here, need to rethingk
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
	(let* ((result (access-protected-resource-with-error "http://www.blogger.com/feeds/default/blogs"))
	       (blogs
		(blog-list-interpreter (parse-xml result))))
	  (net.aserve:with-http-response (req ent)
	    (net.aserve:with-http-body (req ent)
	      (html 
		(:h2 (:princ "blogs gotten"))
		(:ul
		 (dolist (b blogs)
		   (html
		     (:li ((:a :href (process-blog-link b)) (:princ (car b)))))))))))))))

;;; Doesn't deal with >page of blogs (who has that many?)
(defun blog-list-interpreter (lxml)
  (let* ((blog-entries
	  (lxml::lxml-find-elements-with-tag lxml :|entry|))
	 (blog-names
	  (mapcar #'(lambda (e)
		      (cadr (lxml::lxml-subelement e :|title|) ))
		  blog-entries))
	 (blog-ids
	  (mapcar #'(lambda (e)
		      (let ((raw (cadr (lxml::lxml-subelement e :|id|) )))
			;; tag:blogger.com,1999:user-02356162954308418556.blog-15644559
			(multiple-value-bind (match strings)
			    (ppcre:scan-to-strings "blog-(\\d+)" raw)
			  (svref strings 0))))
		  blog-entries)	  ))
    (mapcar #'list blog-names blog-ids)))

(defun process-blog-link (b)
  (format nil "/process-blog?id=~A" (cadr b)))

(publish :path "/process-blog"
	 :function 'process-blog)

(defun process-blog (req ent)
  (wu:with-session (req ent)
    (progn ; wu:with-html-error-handling
    (let ((id (net.aserve:request-query-value "id" req)))
      (let* ((url (format nil "http://www.blogger.com/feeds/~A/posts/default" id)) ;supposedly all posts on omni?
	     (result (access-protected-resource-with-error url))
	     (xml (parse-xml result))
	     (title (cadr (lxml-subelement xml :|title|)))
	     (total (parse-integer (cadr (lxml-subelement xml :|openSearch:totalResults|))))
	     (per-page (parse-integer (cadr (lxml-subelement xml :|openSearch:itemsPerPage|))))
;	     (logfile "/tmp/waybacker-log.html")
	     (logfile (blog-result-file id))
	     (start-time (get-universal-time))
	     ) ;+++ temp of course
	(with-http-response-and-body (req ent)
	(html (:princ (format nil "Working on ~A..." title)))
	;; 
	(with-open-file (s logfile :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (write `(,title ,id) :stream s))
	'(with-open-file (s logfile :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (let ((*html-stream* s))
	    (html
	      (:head
	       ((:link :rel "stylesheet" :type "text/css" :href "http://wuwei.name/wupub/wuwei.css")))
	      (:h1 (:princ (format nil "Report for ~A" title))))))
	;; we have first page
	(dolist (entry (lxml-find-elements-with-tag xml :|entry|))
	  (process-entry entry :logfile logfile :update? t)) ;+++ TEMP turn on update by default
	(dotimes (page (ceiling total per-page))
	  (unless (zerop page)
	    (process-page id page per-page :logfile logfile)))
	(html (:princ "Finished!")
	      :br
	      (:princ (format nil "Took ~A seconds" (- (get-universal-time) start-time))))
	))))))

(defun process-page (id page page-size &key logfile)
  (let* ((url (format nil "http://www.blogger.com/feeds/~A/posts/default?start-index=~A&max-results=~A" id (* page page-size) page-size))
	 (result (access-protected-resource-with-error url))
	 (xml (parse-xml result)))
    (dolist (entry (lxml-find-elements-with-tag xml :|entry|))
      (process-entry entry :logfile logfile :update? t)))) ;+++ TEMP turn on update by default

(defmacro with-logging ((s file) &body body)
  `(with-open-file (,s ,file :direction :output :if-exists :append :if-does-not-exist :create)
     (let ((*html-stream* s))
       (html ,@body))))

(defun update-entry (entry new-content)
  (let* ((entry-xml (saved-entry-xml entry))
	 (content-elt (lxml-subelement entry-xml :|content|)))
    (setf (cadr content-elt) new-content)
    (access-protected-resource-with-error (saved-entry-edit-url entry)
					  `(:request-method :put
							    :drakma-args '(:content ,(s-xml:print-xml-string entry-xml))))
    ))

(defun process-entry (entry-xml &key update? logfile)
  (let* ((content-elt (lxml-subelement entry-xml :|content|))
	 (id (let ((raw (cadr (lxml::lxml-subelement entry-xml :|id|) )))
	       ;; "tag:blogger.com,1999:blog-15644559.post-8259843894206778183"
	       (multiple-value-bind (match strings)
		   (ppcre:scan-to-strings "post-(\\d+)" raw)
		 (svref strings 0))))
	 (content (cadr content-elt))
	 (title (cadr (lxml-subelement entry-xml :|title|)))
	 (url (lxml-attribute (lxml-find-element-with-attribute entry-xml :|link| :|rel| "alternate")
			      :|href|))

	 (good 0)
	 (total 0)
	 (subs 0)
	 (fails 0)
	 transforms)
    (with-logging (s logfile)
      (:princ "Working on ")
      ((:a :href url)
       (:princ title))
      :br
      (:princ id))
    (setq transforms
	  (process-text content
			:excludes '("http://www.blogger.com")
			:reporter
			#'(lambda (url status &optional problem sub)
			    (incf total)
			    (case status
			      (:good (incf good))
			      (:bad
			       (if sub (incf subs) (incf fails))
			       (with-logging (s logfile)
				 (:li
				  ((:a :href url)
				   (:princ-safe url))
				  (:princ " ")
				  (:princ-safe problem)
				  :br
				  (when sub
				    (html
				      ((:a :href sub)
				       (:princ-safe sub)))))))))))
    (with-logging (s logfile)
      (:br (:princ (format nil "~A total links, ~A still valid, ~A substitutions, ~A failures"
			   total
			   good
			   subs
			   fails)))
      :hr
      :newline)
    (when (and update? transforms)
      (let ((new-text content))
	(dolist (transform transforms)
	  (setq new-text (mt:string-replace new-text (car transform) (cadr transform))))
	(update-entry entry-xml new-text)
	))))


;;; stored result pages

(defun blog-result-file (blog-id)
  (make-pathname :defaults "/tmp/foo.lisp" :name (mt:fast-string blog-id)))

(publish :path "/blog-results"
	 :function 'blog-results)

;;; +++ security!
(defun blog-results (req ent)
  (wu:with-session (req ent)
    (with-http-response-and-body (req ent)
      (let ((id (net.aserve:request-query-value "id" req)))
	(if (probe-file (blog-result-file id))
	    (with-open-file (s (blog-result-file id))
	      (let* ((header (read s))
		     (title (car header)))
		(html
		  (:head
		   ((:link :rel "stylesheet" :type "text/css" :href "/wupub/wuwei.css"))
		   ((:script :type "text/javascript" :src "/wupub/prototype.js"))
		   ((:script :type "text/javascript" :src "/wupub/wuwei.js"))
		   )
		  :newline
		  (:body
		   (:h1 (format nil "Results for ~A" title))))
		(do ((entry (read s) (read s nil :eof)))
		    ((eq entry :eof))
		  (render-entry entry))))
	    ;; no file
	    (html (:h1 (:princ-safe (format nil "No results for blog ~A" id))))
	    )))))

;;; Basic (obso)
(defun render-entry (entry)
  (destructuring-bind ((id title url edit-url)
		       (total good subs fails)
		       sub-data transforms) entry
    (html
      :newline
      (:hr
       (:h4 ((:a :href url) (:princ-safe title)))
       (:ul
	(dolist (sub sub-data)
	  (html (:li ((:a href (car sub)) (:princ-safe (car sub)))
		     (when (cadr sub)
		       (html 
			 :br
			 ((:a href (cadr sub)) (:princ-safe (cadr sub)))
			 ))))))
       (:br (:princ (format nil "~A total links, ~A still valid, ~A substitutions, ~A failures"
			    total
			    good
			    subs
			    fails)))))))

(defstruct saved-entry
  id
  title
  url
  edit-url
  xml
  total-count
  good-count
  sub-count
  fail-count
  sub-data
  transforms)

(defun do-substitutions (entry)
  (let* ((entry-xml (saved-entry-xml entry))
	 (content-elt (lxml-subelement entry-xml :|content|))
	 (new-text (cadr content-elt)))
    (dolist (transform transforms)
      (setq new-text (mt:string-replace new-text (car transform) (cadr transform))))
    (update-entry entry new-text)
    ))

;;; Get me rewrite!
(defun render-entry (entry)
    (let* ((id (saved-entry-id entry))
	   (sub-data (saved-entry-sub-data entry))
	   (button-id (mt:string+ "b" id)))
      (html
	:newline
	(:hr
	 (:h4 ((:a :href url) (:princ-safe (saved-entry-title entry))))
	 ((:form :method "POST" 
		 :onsubmit (unless (zerop subs)
			     (remote-function
			      (ajax-continuation ()
				(with-session (wu::req wu::ent) ;+++ !
				  (do-substitutions entry)
				  (render-update
				    (:replace button-id (:b "Done!"))
				    (:alert (format nil "OK: ~A" entry))
				    )
				  ))
			      :form t
			      :spinner button-id
			      :complete ""))) ;disable button +++
	  (:ul
	   (dolist (sub sub-data)
	     (html (:li ((:a href (car sub)) (:princ-safe (car sub)))
			(when (cadr sub)
			  (html 
			    :br
			    ((:a href (cadr sub)) (:princ-safe (cadr sub)))
			    ))))))
	  (:br (:princ (format nil "~A total links, ~A still valid, ~A substitutions, ~A failures"
			       total
			       good
			       subs
			       fails)))

	  (unless (zerop subs)
	    (html :br ((:input :type "submit" :value "Repair" :id button-id))))
	  )))))

      

;;; Redef to save data
(defmacro with-saving ((s file) &body body)
  `(with-open-file (,s ,file :direction :output :if-exists :append :if-does-not-exist :create)
     ,@body))

(defun process-entry (entry-xml &key update? logfile)
  (let* ((content-elt (lxml-subelement entry-xml :|content|))
	 (id (let ((raw (cadr (lxml::lxml-subelement entry-xml :|id|) )))
	       ;; "tag:blogger.com,1999:blog-15644559.post-8259843894206778183"
	       (multiple-value-bind (match strings)
		   (ppcre:scan-to-strings "post-(\\d+)" raw)
		 (svref strings 0))))
	 (content (cadr content-elt))
	 (title (cadr (lxml-subelement entry-xml :|title|)))
	 (url (lxml-attribute (lxml-find-element-with-attribute entry-xml :|link| :|rel| "alternate")
			      :|href|))
	(edit-url (lxml-attribute (lxml-find-element-with-attribute entry-xml :|link| :|rel| "edit")
				  :|href|))
	 (good 0)
	 (total 0)
	 (subs 0)
	 (fails 0)
	 transforms sub-data)
    (setq sub-data
	  (mt:collecting
	    (setq transforms
		  (process-text content
				:excludes '("http://www.blogger.com")
				:reporter
				#'(lambda (url status &optional problem sub)
				    (incf total)
				    (case status
				      (:good (incf good))
				      (:bad
				       (if sub (incf subs) (incf fails))
				       (mt:collect (list url sub)))))))))
    (with-saving (s logfile)
      (terpri s)
      (write
       (make-saved-entry
	:id id
	:title title
	:url url
	:edit-url edit-url
	:xml entry-xml
	:sub-data sub-data
	:transforms transforms)
       :readably t
       :stream s))))



