(in-package :wb)

;;; These values are obtained (for Google) from here: https://code.google.com/apis/console
(defparameter *oauth2-client-id* nil)
(defparameter *oauth2-client-secret* nil)
(defparameter *oauth2-callback* "")
(defparameter *oauth2-scopes* "http://www.blogger.com/feeds/ https://www.google.com/m8/feeds")
(defparameter *oauth-auth-endpoint* "https://accounts.google.com/o/oauth2/auth")
(defparameter *oauth-token-endpoint* "https://accounts.google.com/o/oauth2/token")

#| To debug locally
- run localtunnel to get id
- (localtunnel-setup <4-char-id>)
- verify that it works
- in API console, set callback appropriately |#
(defun localtunnel-setup (id)
  (setf *oauth2-callback* (format nil "http://~A.localtunnel.com/oauth2callback" id)))

(def-session-variable *access-token* nil)
(def-session-variable *refresh-token* nil) 

;;; This URI gets passed to client through a redirect
(defun get-auth-code-uri ()
  (let ((endpoint (puri:uri *oauth-auth-endpoint*))
	(parameters 
	 `(("response_type" . "code")
	   ("client_id" . ,*oauth2-client-id*)
	   ("redirect_uri" . ,*oauth2-callback* )
	   ("scope" .  ,*oauth2-scopes*) ;second one is for contacts
	   ("access_type" . "offline"))))
    (setf (puri:uri-query endpoint)
	  (drakma::alist-to-url-encoded-string parameters :latin1))
    (puri:render-uri endpoint nil)))

(defun access-protected-resource (url access-token &rest other-args)
  (assert access-token)
  (apply #'drakma:http-request url 
	 :additional-headers
	 `(("GData-Version" . "2")	;+++ Google specific, should be controlled by caller
	   ("Authorization" . ,(format nil "Bearer ~A" access-token)))
	 other-args))

(defun access-protected-resource-with-error (url &rest other-args)
  (multiple-value-bind  (result status prob-hint prob-advice)
      (apply #'access-protected-resource url *access-token* other-args)
    (case status
      (200 result)
      (t (error "Failed to get protected resource ~A: ~A ~A ~A ~A" url status result prob-hint prob-advice)))))

(defun get-access-token (code)
  (let ((parameters `(("code" . ,code)
		      ("client_id" . ,*oauth2-client-id*)
		      ("client_secret" . ,*oauth2-client-secret*)
		      ("redirect_uri" . ,*oauth2-callback*) ;+++ I THINK this is the same one as in the first step...
		      ("grant_type" . "authorization_code"))))
    (multiple-value-bind (body status headers) ;...
	(drakma:http-request endpoint *oauth-token-endpoint*
			     :method :post
			     :parameters parameters)
      (let ((json-response
	     (json::decode-json-from-string 
	       (coerce-to-string body))))
      (if (= status 200)
	  (let ((access-token (cdr (assoc :ACCESS--TOKEN json-response)))
		(refresh-token (cdr (assoc :REFRESH--TOKEN json-response))))
	    (values access-token refresh-token))
	  (error "Failed to get access token ~A ~A" status json-response))))))


