;;; -*- Mode: Scheme; scheme48-package: sparql-http-client -*-

;;;; Schemantic Web
;;;; SPARQL Client

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (http-get-sparql service-uri request)
  (if (not (eq? 'HTTP (uri-scheme service-uri)))
      (error "Non-HTTP service URI for HTTP SPARQL query:" service-uri))
  (if (uri-query service-uri)
      (error "Service URI must not have a query for HTTP SPARQL query:"
             service-uri))
  (call-with-http-response 'GET
      (make-uri 'HTTP
                (uri-authority service-uri)
                (uri-path service-uri)
                (string-append "query=" (sparql->condensed-string request))
                (uri-fragment service-uri))
      '()                               ;No header fields
      #f                                ;No entity body
    (lambda (http-response entity-input-port)
      (process-sparql-response service-uri http-response entity-input-port))))

(define (http-post-sparql service-uri query)
  (if (not (eq? 'HTTP (uri-scheme service-uri)))
      (error "Non-HTTP service URI for HTTP SPARQL query:" service-uri))
  (if (uri-query service-uri)
      (error "Service URI must not have a query for HTTP SPARQL query:"
             service-uri))
  (call-with-http-response 'POST
      service-uri
      (list (make-http-header-field 'CONTENT-TYPE
                                    "application/x-www-form-urlencoded"))
      (list->string                     ;++ Lame-o, poor-man's form/URI-
       (cdr                             ;++ encoding.  We strip off the leading
        (string->list                   ;++ question mark with the CDR, and
         (uri->string                   ;++ forget about escaping amperands.
          (make-uri #f                  ;++ URI->STRING deals with the percent
                    #f                  ;++ encoding.
                    '()
                    (string-append "query="
                                   (sparql->condensed-string query))
                    #f)))))
    (lambda (http-response entity-input-port)
      (process-sparql-response service-uri http-response entity-input-port))))

(define (process-sparql-response service-uri http-response entity-input-port)
  (if (not (eq? 'SUCCESS (http-response/status-type http-response)))
      (error "Failure!"
             http-response
             service-uri
             (read-http-entity-body http-response entity-input-port))
      (let ((variables #f)
            (binding-sets '()))
        (parse-sparql-results
         entity-input-port
         (lambda (vs)
           (set! variables vs))
         (lambda (binding-set)
           (set! binding-sets (cons binding-set binding-sets))))
        (values variables (reverse binding-sets)))))
