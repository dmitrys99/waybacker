(in-package :wb)

(setq zs3:*credentials* (zs3:file-credentials "amazon-credentials"))

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

