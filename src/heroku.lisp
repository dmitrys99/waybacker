(in-package :wb)

(defun cl-user::initialize-application ()
  (setq *smtp-gmail-name* (getenv "GMAIL_NAME"))
  (setq *smtp-gmail-password* (getenv "GMAIL_PASSWORD"))
  (setq *oauth-client-id* (getenv "OAUTH_CLIENT_ID"))
  (setq *oauth-client-secret* (getenv "OAUTH_CLIENT_SECRET"))
  (setq *oauth2-callback* (getenv "OAUTH_CALLBACK")) ;+++ ???
