(in-package :wb)

;;; The actual UI (for Blogger version)

(defvar *virtual-host* nil)

(publish :path "/sanity" 
	 :function #'(lambda (req ent)
		       (with-http-response-and-body (req ent)
			 (html
			   (:h1 "you fail")))))
	 

;;; :::::::::::::::: Home page

(publish :path "/"
;	 :host *virtual-host*
	 :content-type "text/html"
	 :function #'(lambda (req ent)
		       (setq *req req)
		       (wu:with-http-response-and-body (req ent)
			 (html (:h2 (:princ "welcome"))
			       (:princ "Welcome to Blogger Link Refresher") :br
			       ((:a :href "/obtain") "Sign in via Google")))))

;;; :::::::::::::::: Sign into google (redirects)

(publish :path "/obtain"
;	 :host *virtual-host*
	 :content-type "text/html"
	 :function #'(lambda (req ent)
		       (let ((auth-uri (get-auth-code-uri)))
			   (wu:with-http-response-and-body (req ent)
			     (html
			       (render-scripts
				 (:redirect auth-uri)))))))

;;; :::::::::::::::: OAuth callback, gets and displays list of blogs

(publish :path "/oauth2callback"
;	 :host *virtual-host*
	 :function 'oauth2callback)

(defun oauth2callback (req ent)
  (let ((error (request-query-value "error" req))
	(code (request-query-value "code" req)))
    (wu:with-session (req ent)
      (with-http-response-and-body (req ent)
	(if error
	    (html 
	      (:h1 "Not authorized")
	      (:princ-safe error))
	    (multiple-value-bind (access-token refresh-token)
		(get-access-token code)
	      ;; we have access token, stash it in session var
	      (setq *access-token* access-token
		    *refresh-token* refresh-token)
	      (let* ((result (coerce-to-string (access-protected-resource-with-error "http://www.blogger.com/feeds/default/blogs")))
		     (blogs
		      (blog-list-interpreter (parse-xml result))))
		  (when wu::*developer-mode*
		    (html
		      (:h2 "Access tokens")
		      "Access: " (:princ-safe *access-token*) :br
		      "Refresh: " (:princ-safe *refresh-token*) :br))
		  (html 
		    (:h2 (:princ "blogs gotten"))
		    (:ul
		     (dolist (b blogs)
		       (html
			 (:li ((:a :href (process-blog-link b)) (:princ (car b)))))))))
	      ))))))

(defun process-blog-link (b)
  (format nil "/process-blog?id=~A" (cadr b)))

;;; :::::::::::::::: Process a blog (gets info and shows summary; actual processing happens in background)

(publish :path "/process-blog"
;	 :host *virtual-host*
	 :function 'process-blog-ui)

(defun process-blog-ui (req ent)
  (wu:with-session (req ent)
    (wu:with-html-error-handling
    (let ((id (net.aserve:request-query-value "id" req)))
      (let* ((url (format nil "http://www.blogger.com/feeds/~A/posts/default" id)) ;supposedly all posts on omni?
	     (result (access-protected-resource-with-error url))
	     (xml (parse-xml result))
	     (title (cadr (lxml-subelement xml :|title|)))
	     (total (parse-integer (cadr (lxml-subelement xml :|openSearch:totalResults|))))
	     (email (get-user-email))
	     ) 
	(with-http-response-and-body (req ent)
	  (html
	  (:princ (format nil "Working on ~A.  There are ~A posts." title total))
	  :p
	  (:princ (format nil "We'll send you an email at ~A when it's done, with a link to the results." email))
	  ))
	;; +++ this is probably going to require something more robust...
	(let ((access-token *access-token*)) ;rigamarole to get session vars to bkgnd process (+++ might need other token eventually)
	  (mt:in-background (format nil "Worker for ~A ~A" id title)
	    (handler-case
		(let ((*access-token* access-token))
		  (process-blog xml)
		  (send-done-email email title id))
	      (error (condition)
		(with-saving (s (blog-result-file id))
		  (write `(error ,(type-of condition) ,(princ-to-string condition)) :stream s))
		(write `(error ,(type-of condition) ,(princ-to-string condition)) :stream *debug-io*))
	      ))))))))

;;; :::::::::::::::: Post email

;;; for gmail, this has to be an application-specific password: https://accounts.google.com/IssuedAuthSubTokens
;;; DO NOT check this into source control! It gives full access to a google account
(defparameter *smtp-gmail-name* nil)
(defparameter *smtp-gmail-password* nil) 

(defun our-host ()
  (puri:uri-host (puri:uri *oauth2-callback*)))

;;; It is I think possible to do this through OAuth (which would avoid having to store a password). But it might
;;; require hacking cl-smtp and I don't want to bother right now.
(defun send-done-email (to title id)
  (cl-smtp:send-email "smtp.gmail.com"
		      "waybacker@hyperphor.com"
		      to
		      (format nil "Your blog ~A has been processed" title)
		      nil
		      :ssl :tls
		      :authentication `(:login ,*smtp-gmail-name* ,*smtp-gmail-password*)
		      :html-message 		      (html-string 
			(:princ (format nil "Your blog ~A has been processed by Waybacker." title))
			:p
			((:a :href (format nil "http://~A/blog-results?id=~A" (our-host) id)) "Click here")
			" to see the results.")
		      ))

;;; :::::::::::::::: Display results

(publish :path "/blog-results"
;	 :host *virtual-host*
	 :content-type "text/html"
	 :function 'blog-results)

;;; +++ security!
(defun blog-results (req ent)
  (wu:with-session (req ent)
    (with-http-response-and-body (req ent)
      (with-html-error-handling 
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
		   (:h1 (format nil "Results for ~A" title))
		   (do ((entry (read s) (read s nil :eof)))
		       ((eq entry :eof))
		     (render-entry entry))))))
	    ;; no file
	    (html (:h1 (:princ-safe (format nil "No results for blog ~A" id))))
	    ))))))

(defun render-entry (entry)
    (let* ((id (saved-entry-id entry))
	   (sub-data (saved-entry-sub-data entry))
	   (button-id (mt:string+ "b" id)))
      (html
	:newline
	(:hr
	 (:h4 ((:a :href (saved-entry-url entry)) (:princ-safe (saved-entry-title entry))))
	 ((:form :method "POST" 
		 :onsubmit (unless (zerop (saved-entry-sub-count entry))
			     (remote-function
			      (ajax-continuation ()
				(with-session (wu::req wu::ent) ;+++ !
				  (do-substitutions entry)
				  (render-update
				    (:replace button-id (:b "Done!"))
				    )
				  ))
			      :form t
			      :spinner button-id
			      :complete ""))) ;disable button +++
	  (:ul
	   (dolist (sub-list sub-data)
	     (destructuring-bind (url sub status prob) sub-list
	     (html (:li ((:a href url) (:princ-safe url))
			(when (or status prob)
			  (html :br
				(:princ-safe (format nil "~A: ~A" status prob))))
			(when sub
			  (html :br
				"Refresh to: "
				((:a href sub) (:princ-safe sub))))
			)))))

	  (:br (:princ (format nil "~A total links, ~A still valid, ~A substitutions, ~A failures"
			       (saved-entry-total-count entry)
			       (saved-entry-good-count entry)
			       (saved-entry-sub-count entry)
			       (saved-entry-fail-count entry))))
	  
	  (unless (zerop (saved-entry-sub-count entry))
	    (html :br ((:input :type "submit" :value "Repair" :id button-id))))
	  )))))

(defun do-substitutions (entry)
  (let* ((entry-xml (saved-entry-xml entry))
	 (content-elt (lxml-subelement entry-xml :|content|))
	 (new-text (cadr content-elt)))
    (dolist (transform (saved-entry-transforms entry))
      (setq new-text (mt:string-replace new-text (car transform) (cadr transform))))
    (update-entry entry new-text)
    ))
