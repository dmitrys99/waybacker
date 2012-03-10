(in-package :wb)

;;; Zeroth-order web UI

#|
Todo:
- this takes forever, too long for a standard web response
- the result should indicate transformed URLs in some way
- maybe iframes, with a log panel and a page panel
|#

(publish :path "/wayback-form"
	 :function 'wayback-form)

(defun wayback-form (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:html
      (:body
       (:h1 "Waybacker")
       ((:form :action "/wayback-response")
	"URL:"
	((:input :name "url"))
	((:input :type "submit"))))))))

(publish :path "/wayback-response"
	 :function 'wayback-response)

;;; Returns the transformed page, which is not that useful.  Needs a frame or something...
(defun wayback-response (req ent)
  (with-http-response-and-body (req ent)
    (let ((url (request-query-value "url" req))
	  (options nil))
      (write-string (apply #'transform-text (drakma:http-request url :user-agent *user-agent*) options)
		    *html-stream*))))


; (start :port 3333)


