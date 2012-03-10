(in-package :wb)

(defun cl-user::initialize-application ()
  (setq *smtp-gmail-name* (ccl:getenv "GMAIL_NAME"))
  (setq *smtp-gmail-password* (ccl:getenv "GMAIL_PASSWORD"))
  (setq *oauth-client-id* (ccl:getenv "OAUTH_CLIENT_ID"))
  (setq *oauth-client-secret* (ccl:getenv "OAUTH_CLIENT_SECRET"))
  (setq *oauth2-callback* (ccl:getenv "OAUTH_CALLBACK")) 
  )
