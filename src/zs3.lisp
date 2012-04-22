(in-package :wb)

(defun local-zs3-setup ()
  (setq zs3:*credentials* 
	(zs3:file-credentials "amazon-credentials")))

(defun heroku-zs3-init ()
  (setq zs3:*credentials* 
	(list (ccl:getenv "AMAZON_ACCESS_KEY_ID")
	      (ccl:getenv "AMAZON_SECRET_ACCESS_KEY"))))

(defun setup ()
  (zs3:create-bucket "waybacker"))

#|
playing around
(zs3:bucket-exists-p "timebank")
(zs3:put-file "~/repos/waybacker/README" "waybacker" "bogus")
(zs3:query-bucket "waybacker")
(zs3:all-keys "waybacker") ;;; takes a long time, 2300 entries
(zs3:all-keys "timebank" :prefix "photos/973")
|#

