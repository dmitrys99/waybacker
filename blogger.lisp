(in-package :wb)

;;; code for munging blogger specifically

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
			(extract-blog-id raw)
			))
		  blog-entries)	  ))
    (mapcar #'list blog-names blog-ids)))

;;; pull id out of tag:blogger.com,1999:user-02356162954308418556.blog-15644559
(defun extract-blog-id (string)
  (multiple-value-bind (match strings)
      (ppcre:scan-to-strings "blog-(\\d+)" string)
    (svref strings 0)))

;;; stored results

(defun blog-result-file (blog-id)
  (make-pathname :defaults "/tmp/foo.lisp" :name (mt:fast-string blog-id))) ;+++ tmp is bad!

(defmacro with-saving ((s file) &body body)
  `(with-open-file (,s ,file :direction :output :if-exists :append :if-does-not-exist :create)
     ,@body))

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

;;; argument is parsed xml from initial query
(defun process-blog (xml)
  (let* ((title (cadr (lxml-subelement xml :|title|))) 
	 (id (extract-blog-id (cadr (lxml-subelement xml :|id|))))
	 (per-page (parse-integer (cadr (lxml-subelement xml :|openSearch:itemsPerPage|))))
	 (total (parse-integer (cadr (lxml-subelement xml :|openSearch:totalResults|))))
	 (logfile (blog-result-file id)))
    (with-open-file (s logfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write `(,title ,id) :stream s))
    ;; we have first page
    (dolist (entry (lxml-find-elements-with-tag xml :|entry|))
      (process-entry entry :logfile logfile :update? t)) ;+++ TEMP turn on update by default
    (dotimes (page (ceiling total per-page))
      (unless (zerop page)
	(process-page id page per-page :logfile logfile)))
    ))

(defun process-page (id page page-size &key logfile)
  (let* ((url (format nil "http://www.blogger.com/feeds/~A/posts/default?start-index=~A&max-results=~A" id (* page page-size) page-size))
	 (result (access-protected-resource-with-error url))
	 (xml (parse-xml result)))
    (dolist (entry (lxml-find-elements-with-tag xml :|entry|))
      (process-entry entry :logfile logfile :update? t))))

(defun process-entry (entry-xml &key update? logfile)
  (let* ((content-elt (lxml-subelement entry-xml :|content|))
	 (id (let ((raw (cadr (lxml::lxml-subelement entry-xml :|id|) )))
	       ;; "tag:blogger.com,1999:blog-15644559.post-8259843894206778183"
	       (multiple-value-bind (match strings)
		   (ppcre:scan-to-strings "post-(\\d+)" raw)
		 (assert match)	;better
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
	:transforms transforms
	:total-count total
	:good-count good
	:sub-count subs
	:fail-count fails)
       :readably t
       :stream s))))

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
