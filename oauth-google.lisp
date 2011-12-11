(in-package :wb)

(ql:quickload '(:cl-oauth :s-xml))
(load "~/repos/swframes/lib/lxml.lisp")	; +++ temp loc
(use-package :cl-oauth)
(use-package :wuwei)
(use-package :lxml)

(defparameter *key* "wuwei.name")
(defparameter *secret* "xoHi0NAM5RW9_C0sK-M5VUuB")

(defparameter *callback-uri* "http://53km.localtunnel.com/oauth")
(defparameter *callback-port* 8080
  "Port to listen on for the callback")

(defparameter *get-request-token-endpoint* "https://www.google.com/accounts/OAuthGetRequestToken")
(defparameter *auth-request-token-endpoint* "https://www.google.com/accounts/OAuthAuthorizeToken")
(defparameter *get-access-token-endpoint* "https://www.google.com/accounts/OAuthGetAccessToken")
(defparameter *consumer-token* (make-consumer-token :key *key* :secret *secret*))

(def-session-variable  *request-token* nil)
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
    (let ((result (access-protected-resource
;		   "http://www.google.com/calendar/feeds/default/allcalendars/full?orderby=starttime"
		   "http://www.blogger.com/feeds/default/blogs"
;		   "http://www.blogger.com/feeds/15644559/posts/default" ;supposedly all posts on omni?
		   *access-token*)))
      (setq *blogs* (parse-xml result))
      (net.aserve:with-http-response (req ent)
	(net.aserve:with-http-body (req ent)
	  (html (:h2 (:princ "blogs gotten"))
		(:pre (pprint *blogs* net.aserve::*html-stream*)))))))))

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
		      (cadr (lxml::lxml-subelement e :|id|) ))
		  blog-entries)	  ))
    (values blog-names blog-ids)))
	 
	 
