;;; -*- Mode: Scheme; scheme48-package: simple-http-client -*-

;;;; Schemantic Web
;;;; Simple HTTP Client

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (http-head uri header-fields)
  (call-with-http-response 'HEAD uri header-fields #f
    (lambda (http-response body-input-port)
      body-input-port                   ;ignore
      http-response)))

(define (http-get uri header-fields)
  (call-with-http-response 'GET uri header-fields #f
    (lambda (http-response body-input-port)
      (values http-response
              (read-http-entity-body http-response body-input-port)))))

(define (http-post uri header-fields body)
  (call-with-http-response 'POST uri header-fields body
    (lambda (http-response body-input-port)
      (values http-response
              (read-http-entity-body http-response body-input-port)))))

(define (read-http-entity-body http-response input-port)
  (or (let ((header-fields (http-response/header-fields http-response)))
        (cond ((first-http-header-field-value header-fields 'CONNECTION #f)
               => (lambda (value)
                    (and (string-ci=? "close" value)
                         (read-all input-port))))
              ((first-http-header-field-value header-fields 'CONTENT-LENGTH #f)
               => (lambda (value)
                    (cond ((number->string value #d10)
                           => (lambda (content-length)
                                (read-string-of-length content-length
                                                       input-port)))
                          (else #f))))
              (else #f)))
      (begin
        (warn "Unable to determine entity length of response:" http-response)
        #f)))

;;; I wonder whether this interface ought to separate the authority and
;;; the request URI, since they really are distinct, and requests to
;;; proxies must send absolute URIs.

(define (call-with-http-response method uri header-fields body receiver)
  (receive (uri authority) (decompose-http-request-uri uri)
    (call-with-http-connection authority
      (lambda (connection)
        (send-http-request connection method uri header-fields body)
        (close-output-port (http-connection/output-port connection))
        (receiver (receive-http-response connection)
                  (http-connection/input-port connection))))))

(define $default-http-uri-authority (make-fluid #f))
(define (default-http-uri-authority) (fluid $default-http-uri-authority))
(define (with-default-http-uri-authority authority thunk)
  (let-fluid $default-http-uri-authority authority thunk))

(define (decompose-http-request-uri uri)
  (cond ((or (uri-authority uri)
             (default-http-uri-authority))
         => (lambda (authority)
              (values (make-uri #f      ;No scheme,
                                #f      ; no authority -- relative.
                                (uri-path uri)
                                (uri-query uri)
                                (uri-fragment uri))
                      (make-uri-authority (uri-authority-userinfo authority)
                                          (uri-authority-host authority)
                                          (or (uri-authority-port authority)
                                              80)))))
        (else
         (error "Unable to determine authority for HTTP request URI:" uri))))

;;;; HTTP Connections

;++ Implement a concurrent connection pool.

(define-record-type <http-connection>
    (make-http-connection uri-authority input-port output-port)
    http-connection?
  (uri-authority http-connection/uri-authority)
  (input-port http-connection/input-port)
  (output-port http-connection/output-port))

(define (call-with-http-connection authority receiver)
  (let ((connection (open-http-connection authority)))
    (receive results (receiver connection)
      (close-http-connection connection)
      (apply values results))))

(define (open-http-connection authority)
  (receive (input-port output-port)
           (socket-client (uri-authority-host authority)
                          (uri-authority-port authority))
    (make-http-connection authority input-port output-port)))

(define (http-connection-open? connection)
  connection                            ;ignore
  #t)

(define (close-http-connection connection)
  (close-input-port (http-connection/input-port connection))
  (close-output-port (http-connection/output-port connection)))

(define (uri-authority/host-string authority)
  (string-append (uri-authority-host authority)
                 (let ((port (uri-authority-port authority)))
                   (if (and port (not (= port 80)))
                       (string-append ":" (number->string port #d10))
                       ""))))

(define (http-connection/host-string connection)
  (uri-authority/host-string (http-connection/uri-authority connection)))

(define (send-http-request connection method uri header-fields body)
  (write-http-request method
                      uri
                      (adjoin-http-header-fields
                       (list (make-http-header-field
                              'HOST
                              (http-connection/host-string connection)))
                       header-fields
                       (if (string? body)
                           (list (make-http-header-field
                                  'CONTENT-LENGTH
                                  (number->string (string-length body) #d10)))
                           '()))
                      body
                      (http-connection/output-port connection)))

(define (receive-http-response connection)
  (read-http-response (http-connection/input-port connection)))

;;;; HTTP Versions

(define (make-http-version major minor)
  (cons major minor))

(define (http-version? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (exact-nonnegative-integer? (cdr object))))

(define (exact-nonnegative-integer? object)
  (and (integer? object)
       (exact? object)
       (>= object 0)))

(define (http-version/major version) (car version))
(define (http-version/minor version) (cdr version))

(define (http-version=? a b)
  (and (= (http-version/major a)
          (http-version/major b))
       (= (http-version/minor a)
          (http-version/minor b))))

(define (http-version<=? a b)
  (or (< (http-version/major a)
         (http-version/major b))
      (and (= (http-version/major a)
              (http-version/major b))
           (<= (http-version/minor a)
               (http-version/minor b)))))

(define earliest-http-version (make-http-version 1 0))
(define latest-http-version (make-http-version 1 0))

;;;; HTTP Requests

;++ Perhaps this should use the format abstraction.

(define (write-http-request method uri header-fields body port)
  (write-http-request-line method uri port)
  (write-http-header-fields header-fields port)
  (write-http-body body port)
  (force-output port))

(define (write-http-request-line method uri port)
  (write-http-method method port)
  (write-char #\space port)
  (write-http-request-uri uri port)
  (write-char #\space port)
  (write-string "HTTP/" port)
  (write-http-version latest-http-version port)
  (write-crlf port))

(define (write-http-method method port)
  (write-string
   (cond ((symbol? method) (string-upcase (symbol->string method)))
         ((string? method) method)
         (else (error "Invalid HTTP request method:" method)))
   port))

(define (write-http-request-uri uri port)
  (cond ((eq? '*  uri) (write-char #\* port))
        ((uri?    uri) (write-uri uri port))
        ((string? uri) (write-string uri port))
        ((and (pair? uri)
              (list? uri)
              (every string? uri))
         (for-each (lambda (path-component)
                     (write-char #\/ port)
                     (write-string path-component port))
                   uri))
        (else
         (error "Invalid HTTP request URI:" uri))))

(define (write-http-version version port)
  (write (http-version/major version) port)
  (write-char #\. port)
  (write (http-version/minor version) port))

(define (write-http-header-fields header-fields port)
  (for-each (lambda (header-field)
              (write (http-header-field/name header-field) port)
              (write-string ": " port)
              (write-string (http-header-field/value header-field) port)
              (write-crlf port))
            header-fields)
  (write-crlf port))

(define (write-crlf port)
  (write-char (ascii->char #x0D) port)
  (write-char (ascii->char #x0A) port))

(define (write-http-body body port)
  (cond ((procedure? body) (body port))
        ((string? body) (write-string body port))
        ((not body) (values))
        (else (error "Invalid HTTP message body:" body))))

;;;; HTTP Responses

(define-record-type <http-response>
    (make-http-response version status-type status-code reason header-fields)
    http-response?
  (version http-response/version)
  (status-type http-response/status-type)
  (status-code http-response/status-code)
  (reason http-response/reason)
  (header-fields http-response/header-fields))

(define (http-response/first-header-field-value http-response name default)
  (first-http-header-field-value (http-response/header-fields http-response)
                                 name
                                 default))

(define (http-response/all-header-field-values http-response name)
  (all-http-header-field-values (http-response/header-fields http-response)
                                name))

(define (read-http-response input-port)
  (parse-input-chars http-parser:response
                     input-port
                     #f                 ;No context
                     (lambda (response context stream)
                       context stream   ;ignore
                       response)
                     (lambda (perror context stream)
                       (apply error
                              "Malformed HTTP response:"
                              (parse-error/position perror)
                              (parse-error/messages perror)))))

;;;;; Response Parser

(define-parser http-parser:response
  (*parser
      ((parser:string= "HTTP/"))
      (version http-parser:version)
      (http-parser:lws)
      (status-code http-parser:status-code)
      (http-parser:lws)
      (reason http-parser:reason-phrase)
      (http-parser:crlf)
      ;; This eats the final CRLF.
      (header-fields http-parser:header-fields)
    (parser:return
     (make-http-response version
                         (http-status-code->type status-code)
                         status-code
                         reason
                         header-fields))))

(define (http-status-code->type status-code)
  (case (quotient status-code 100)
    ((1) 'INFORMATIONAL)
    ((2) 'SUCCESS)
    ((3) 'REDIRECTION)
    ((4) 'CLIENT-ERROR)
    ((5) 'SERVER-ERROR)
    (else #f)))

(define-parser http-parser:version
  (*parser
      (major http-parser:version-part)
      ((parser:char= #\.))
      (minor http-parser:version-part)
    (let ((version (make-http-version major minor)))
      (if (or (http-version<=? version earliest-http-version)
              (http-version<=? latest-http-version version))
          (parser:return version)
          (parser:error "Unsupported HTTP version:" version)))))

(define-parser http-parser:version-part
  (parser:decimal-string->number
   (parser:string:at-least 1 (parser:char-in-set char-set:digit))
   (lambda (string)
     (parser:error "Non-numeric version part:" string))))

(define-parser http-parser:status-code
  (parser:decimal-string->number
   (parser:string:exactly 3 (parser:char-in-set char-set:digit))
   (lambda (string)
     (parser:error "Non-numeric status code:" string))))

(define-parser (parser:decimal-string->number string-parser error-parser)
  (*parser (string string-parser)
    (cond ((string->number string #d10)
           => parser:return)
          (else (error-parser string)))))

(define-parser http-parser:reason-phrase
  (parser:string:repeated (parser:char-not-in-set http-char-set:crlf)))

;;;;;; Parsing Header Fields

(define-parser http-parser:header-fields
  (parser:list:repeated-until http-parser:crlf http-parser:header-field))

(define-parser http-parser:header-field
  (*parser
      (name http-parser:header-field-name)
      (value http-parser:header-field-value)
    (parser:return
     (make-http-header-field name value))))

(define-parser http-parser:header-field-name
  (parser:map intern
    (parser:string:at-least-until 1 (parser:char= #\:)
      (parser:char-in-set http-char-set:header-field-name))))

(define-parser http-parser:header-field-value
  (*parser
      (initial-line http-parser:header-field-initial-line)
      (continuation-lines
       (parser:list:repeated http-parser:header-field-continuation-line))
    (parser:return
     ;++ I wonder whether it would be better to have header fields map
     ;++ names to *lists* of values, for each separate line.  I don't
     ;++ think that the intent of the HTTP RFC is for implementations
     ;++ to distinguish this, but it would not surprise me if certain
     ;++ protocols did.
     (string-join/infix " " (cons initial-line continuation-lines)))))

(define-parser http-parser:header-field-initial-line
  (parser:sequence http-parser:lws* http-parser:header-field-line))

(define-parser http-parser:header-field-continuation-line
  (parser:sequence http-parser:lws+ http-parser:header-field-line))

(define-parser http-parser:header-field-line
  (parser:map string-trim-right
    (parser:string:repeated-until http-parser:crlf
      (parser:char-in-set http-char-set:header-field-value))))

;;;;;; Parsing Utilities and Character Sets

(define http-char-set:lws               ;Linear White Space
  (char-set (ascii->char #x09)          ;  TAB
            (ascii->char #x20)))        ;  SPC

(define-parser http-parser:lws
  (parser:char-in-set http-char-set:lws))

(define-parser http-parser:lws*
  (parser:noise:repeated http-parser:lws))

(define-parser http-parser:lws+
  (parser:noise:at-least 1 http-parser:lws))

(define http-char-set:crlf
  (char-set (ascii->char #x0D)
            (ascii->char #x0A)))

(define-parser http-parser:crlf
  (let ((carriage-return (parser:char= (ascii->char #x0D)))
        (line-feed (parser:char= (ascii->char #x0A))))
    (parser:choice
     (parser:sequence carriage-return
                      (parser:optional-noise line-feed))
     line-feed)))

(define http-char-set:separator
  (char-set #\( #\) #\< #\> #\@
            #\, #\; #\: #\\ #\"
            #\/ #\[ #\] #\? #\=
            #\{ #\} #\space
            ;; Horizontal tab
            (ascii->char #x09)))

(define http-char-set:token
  (char-set-complement
   (char-set-union char-set:iso-control
                   http-char-set:separator)))

(define http-char-set:header-field-name
  ;; http-char-set:token
  (char-set-complement (char-set-adjoin http-char-set:crlf #\:)))

(define http-char-set:header-field-value
  (char-set-union (char-set-complement char-set:iso-control)
                  http-char-set:lws))

;;;; Header Fields

(define (make-http-header-field name value) (cons name value))
(define (http-header-field/name header-field) (car header-field))
(define (http-header-field/value header-field) (cdr header-field))

(define (first-http-header-field header-fields name)
  (assoc name header-fields))

(define (first-http-header-field-value header-fields name default)
  (cond ((first-http-header-field header-fields name) => cdr)
        (else default)))

(define (find-http-header-field-value header-fields name)
  (string-join/infix "," (all-http-header-field-values name header-fields)))

(define (all-http-header-fields header-fields name)
  (collect-list (for header-field (in-list header-fields))
      (if (eq? name (http-header-field/name header-field)))
    header-field))

(define (all-http-header-field-values header-fields name)
  (collect-list (for header-field (in-list header-fields))
      (if (eq? name (http-header-field/name header-field)))
    (http-header-field/value header-field)))

(define (adjoin-http-header-fields left header-fields right)
  (define (clean other-header-fields initial-tail)
    (collect-list (initial initial-tail)
        (for header-field (in-list other-header-fields))
        (if (not (first-http-header-field
                  header-fields
                  (http-header-field/name header-field))))
      header-field))
  (clean left (append header-fields (clean right '()))))

(define (parse-http-header-field-value parser header-fields name context
                                       win lose)
  (parse-stream parser
                (stream-http-header-field-value header-fields name)
                #f                      ;No position
                (lambda (position token) token position)
                context
                win
                lose))

(define (stream-http-header-field-value header-fields name)
  (let ((comma (stream-cons #\, stream-nil)))
    (lazy-loop recur ((for header-field (in-list header-fields))
                      (with prefix stream-nil))
      => stream-nil
      (if (eq? name (http-header-field/name header-field))
          (stream-append prefix
                         (string->stream
                          (http-header-field/value header-field))
                         (recur (=> prefix comma)))
          (recur)))))

(define (stream-append . streams)
  (lazy-loop outer ((for stream (in-list streams)))
    => stream-nil
    (lazy-loop inner ((with stream stream (stream-cdr stream))
                      (while (stream-pair? stream)))
      => (outer)
      (stream-cons (stream-car stream) (inner)))))

;;;; Utilities

(define (read-string-of-length length input-port)
  (let* ((string (make-string length))
         (length-read (read-block string 0 length input-port #t)))
    (cond ((eof-object? length-read) "")
          ((< length-read length) (substring string 0 length-read))
          (else string))))

(define (read-all input-port)
  (let ((buffer-size #x1000))
    (let ((buffer (make-string buffer-size))
          (output-port (open-output-string)))
      (loop continue ()
        (let ((octets (read-block buffer 0 buffer-size input-port #t)))
          (if (not (eof-object? octets))
              (begin
                (write-block buffer 0 octets output-port)
                (if (= octets buffer-size)
                    (continue))))))
      (get-output-string output-port))))

(define (string-join/infix separator strings)
  (string-join strings separator 'INFIX))

(define (intern string)
  (string->symbol (string-reader-case string)))

(define reader-case-map
  (collect-string-of-length #x100 (for index (up-from 0 (to #x100)))
    (ascii->char index)))

(let ((lowercase "abcdefghijklmnopqrstuvwxyz")
      (uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (define (fill-case-map from-case to-case)
    (loop ((for from-char (in-string from-case))
           (for to-char (in-string to-case)))
      (string-set! reader-case-map (char->ascii from-char) to-char)))
  (if (char=? #\T (string-ref (symbol->string 't) 0))
      (fill-case-map lowercase uppercase)
      (fill-case-map uppercase lowercase)))

(define (char-reader-case char)
  (string-ref reader-case-map (char->ascii char)))

(define (string-reader-case string)
  (collect-string-of-length (string-length string)
      (for char (in-string string))
    (char-reader-case char)))

(define (stream-append . streams)
  (loop outer-recur ((with streams streams))
    (if (pair? streams)
        (let ((stream (car streams))
              (streams (cdr streams)))
          (loop inner-recur ((with stream stream))
            (lazy (if (stream-pair? stream)
                      (stream-cons (stream-car stream)
                                   (lazy (inner-recur (stream-cdr stream))))
                      (outer-recur streams)))))
        stream-nil)))
