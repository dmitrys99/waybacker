(in-package :oauth)

(defun make-aserve-request-adapter ()
  (make-request-adapter
   :request-object-fn (lambda () *request*) ;handler must set
   :request-uri-fn (lambda (request)
		     (puri:uri (net.aserve::request-uri request))) ;too simple?
;   :request-method-fn 'hunchentoot:request-method*
;   :abort-request-fn 'hunchentoot:abort-request-handler
;   :auth-parameters-fn (lambda (request) nil) ; TODO
;   :post-parameters-fn 'hunchentoot:post-parameters*
   :get-parameters-fn 'net.aserve:request-query ))

(setq *request-adapter* (make-aserve-request-adapter))
