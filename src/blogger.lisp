(in-package :wb)

;;; code for munging blogger specifically

;;; Doesn't deal with >page of blogs (who has that many?)
;;; Values: a list of (blog id) pairs
;;;         author name
(defun blog-list-interpreter (lxml)
  (let* ((blog-entries
	  (lxml::lxml-find-elements-with-tag lxml :|entry|))
	 (blog-names
	  (mapcar #'(lambda (e)
		      (lxml-subelement-contents e :|title|) )
		  blog-entries))
	 (blog-ids
	  (mapcar #'(lambda (e)
		      (let ((raw (lxml-subelement-contents e :|id|) ))
			(extract-blog-id raw)
			))
		  blog-entries)	  )
	 (author (cadr (car (lxml::lxml-find-elements-with-tag lxml :|author| :|name|))))
	 )
    (values (mapcar #'list blog-names blog-ids)
	    author)))

;;; pull id out of tag:blogger.com,1999:user-02356162954308418556.blog-15644559
(defun extract-blog-id (string)
  (multiple-value-bind (match strings)
      (ppcre:scan-to-strings "blog-(\\d+)" string)
    (svref strings 0)))

;;; stored results

(defun blog-result-file (blog-id)
  (merge-pathnames (make-pathname :name (mt:fast-string blog-id) :type "lisp")
		   *results-directory*))

#|
;;; File version
(defvar *results-directory* "./results/")

(defmacro with-saving ((s file) &body body)

     ,@body))

(defun probe-results-file? (blog-id)
  (let ((file (merge-pathnames (make-pathname :name (mt:fast-string blog-id) :type "lisp")
			       *results-directory*)))
    (probe-file file)))

|#

(defun blog-result-file (blog-id)
  (merge-pathnames (make-pathname :name (mt:fast-string blog-id) :type "lisp")
		   *results-directory*))

;;; S3 Version
(defvar *results-directory* "/tmp/")

(defun store-blog-result-file (blog-id)
  (zs3:put-file (blog-result-file blog-id)
		"waybacker"
		(blog-result-key blog-id)))

(defun blog-result-key (id)
  (mt:string+ "result/" id))

(defun probe-results-file? (blog-id)
  (let ((file (merge-pathnames (make-pathname :name (mt:fast-string blog-id) :type "lisp")
			       *results-directory*)))
    (or (probe-file file)
	;; +++ don't like ignore errors, but there is no API call to probe file!
	(mt:report-and-ignore-errors (zs3:get-file "waybacker" (blog-result-key id) file)))))

;;; End S3 specific

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
  sub-data				;list of (url sub status error-string)
  transforms)

;;; argument is parsed xml from initial query
(defun process-blog (xml &key update?)
  (let* ((title (cadr (lxml-subelement xml :|title|))) 
	 (id (extract-blog-id (lxml-subelement-contents xml :|id|)))
	 (per-page (parse-integer (lxml-subelement-contents xml :|openSearch:itemsPerPage|)))
	 (total (parse-integer (lxml-subelement-contents xml :|openSearch:totalResults|)))
	 (logfile (blog-result-file id)))
    (with-open-file (s logfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write `(,title ,id) :stream s))
    ;; we have first page
    (dolist (entry (lxml-find-elements-with-tag xml :|entry|))
      (process-entry entry :logfile logfile :update? update?))
    (dotimes (page (ceiling total per-page))
      (unless (zerop page)
	(process-blogger-page id page per-page :logfile logfile)))
    (store-blog-result-file id)
    ))

(defun process-blogger-page (id page page-size &key logfile)
  (let* ((url (format nil "http://www.blogger.com/feeds/~A/posts/default?start-index=~A&max-results=~A" id (* page page-size) page-size))
	 (result (access-protected-resource-with-error url))
	 (xml (parse-xml result)))
    (dolist (entry (lxml-find-elements-with-tag xml :|entry|))
      (process-entry entry :logfile logfile :update? nil))))

(defun lxml-subelement-contents (xml tag)
  (let ((elt (lxml-subelement xml tag)))
    (and (listp elt) (cadr elt))))

(defparameter *blogger-excludes* '("http://www.blogger.com" "http://web.archive.org"))
;;; http://photos1.blogger.com/blogger/5271/1454/1600/oilrigs.gif
;;; nytimes.com (common and causes redirect loops)

(defun process-entry (entry-xml &key update? logfile)
  (let* ((content-elt (lxml-subelement entry-xml :|content|))
	 (id (let ((raw (lxml-subelement-contents entry-xml :|id|)))
	       ;; "tag:blogger.com,1999:blog-15644559.post-8259843894206778183"
	       (multiple-value-bind (match strings)
		   (ppcre:scan-to-strings "post-(\\d+)" raw)
		 (assert match)	;better
		 (svref strings 0))))
	 (content (cadr content-elt))
	 (title (lxml-subelement-contents entry-xml :|title|))
	 (url (lxml-attribute (lxml-find-element-with-attribute entry-xml :|link| :|rel| "alternate")
			      :|href|))
	 (edit-url (lxml-attribute (lxml-find-element-with-attribute entry-xml :|link| :|rel| "edit")
				   :|href|))
	 (good 0)
	 (total 0)
	 (subs 0)
	 (fails 0)
	 transforms
	 (sub-data
	  (mt:collecting
	    (setq transforms
		  (process-text content
				:excludes *blogger-excludes*
				:reporter
				#'(lambda (url status &optional sub error-string)
				    (incf total)
				    (if (eq status :good)
					(incf good)
					(progn 
					  (if sub (incf subs) (incf fails))
					  (mt:collect (list url sub status error-string)))))))))
	 (entry 
	  (make-saved-entry
	   :id id
	   :title title
	   :url url
	   :edit-url edit-url
	   :xml entry-xml
	   :sub-data sub-data
	   :transforms transforms
	   :total-count total
	   :good-count good
	   :sub-count subs
	   :fail-count fails)))
    (with-open-file (s logfile :direction :output :if-exists :append :if-does-not-exist :create)
      (terpri s)
      (write
       entry
       :readably t
       :stream s))
    (when update?
      (do-substitutions entry))
    ))

(defun update-entry (entry new-content)
  (let* ((entry-xml (saved-entry-xml entry))
	 (content-elt (lxml-subelement entry-xml :|content|)))
    ;; we need to add this, sigh
    (setf (car entry-xml)
	  (append (car entry-xml) '(:|xmlns| "http://www.w3.org/2005/Atom"
				    :|xmlns:thr| "http://purl.org/syndication/thread/1.0"
				    :|xmlns:gd| "http://schemas.google.com/g/2005")))
    (setf (cadr content-elt) new-content)
    (access-protected-resource-with-error (saved-entry-edit-url entry)
					  :method :put
					  :content-type "application/atom+xml" ;+++ must be present or drakma gets confused
					  :content (s-xml:print-xml-string entry-xml))))



;;; Not Blogger, but a Google API

;;; requires session vars to be set

(defun get-user-email ()
  (let* ((res (coerce-to-string (access-protected-resource-with-error
				 "https://www.google.com/m8/feeds/contacts/default/full"))) ;this gets all, not sure how to just get ours
	 (xml (parse-xml res))
	 (email (lxml-subelement-contents xml :|id|)))
    email))

;;; Debugging
(defun count-forms (file)
  (with-open-file (s file)
    (do ((count 0 (+ 1 count))
	 (form (read s nil :eof) (read s nil :eof)))
	((eq form :eof) count))))
