;;; -*- Mode: Scheme; scheme48-package: sparql-results-ssax -*-

;;;; Schemantic Web
;;;; SPARQL Results Parser, with SSAX

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define sparql-variables-handler
  (match-lambda
    (`(,handle-variables ,handle-bindings)
     handle-variables)))

(define sparql-bindings-handler
  (match-lambda
    (`(,handle-variables ,handle-bindings)
     handle-bindings)))

(define (parse-sparql-results input-port handle-variables handle-bindings)
  (sparql-results-parser input-port
                         `(START (,handle-variables ,handle-bindings))))

(define sparql-results-parser
  (ssax:make-parser
;; DOCTYPE              handle-doctype
;; UNDECL-ROOT          handle-undecl-root
;; DECL-ROOT            handle-decl-root
   NEW-LEVEL-SEED       handle-new-level-seed
   FINISH-ELEMENT       handle-finish-element
   CHAR-DATA-HANDLER    handle-char-data
;; PI                   ()
   ))

;; (define (parse-sparql-results input-port handle-variables handle-bindings)
;;   (sparql-results-parser 'SPARQL
;;                          input-port
;;                          #f             ;elems (for validation)
;;                          '()            ;entities
;;                          '()            ;namespaces
;;                          #t             ;preserve-ws?
;;                          `(START (,handle-variables ,handle-bindings))))

;; (define sparql-results-parser
;;   (ssax:make-elem-parser handle-new-level-seed
;;                          handle-finish-element
;;                          handle-char-data
;;                          ;; No processing instruction handlers.
;;                          ()))

(define (handle-new-level-seed name attributes namespaces expected-content
                               seed)
  expected-content                      ;ignore
  (match seed
    (`(START ,handlers)
     (if (not (name=? name 'SPARQL))
         (error "Invalid SPARQL results root element:" name attributes))
     `(START-HEAD ,handlers))
    (`(START-HEAD ,handlers)
     (if (not (name=? name 'HEAD))
         (error "Invalid SPARQL results header:" name attributes))
     `(HEAD () ,handlers))
    (`(HEAD ,variables ,handlers)
     (if (not (name=? name 'VARIABLE))
         (error "Invalid SPARQL results variable specifier:"
                name
                attributes))
     (cond ((find-attribute 'NAME attributes)
            => (lambda (attribute)
                 `(VARIABLE ,(cons (attribute-value attribute) variables)
                            ,handlers)))
           (else
            (error "No `name' attribute in `variable' element:"
                   attributes))))
    (`(VARIABLE ,variables ,handlers)
     (error "`variable' elements should have no children." name attributes))
    (`(START-RESULTS ,handlers)
     (if (not (name=? name 'RESULTS))
         (error "Invalid SPARQL results list start:" name attributes))
     `(RESULTS ,handlers))
    (`(RESULTS ,handlers)
     (if (not (name=? name 'RESULT))
         (error "Invalid SPARQL results list element:" name attributes))
     `(BINDINGS () ,handlers))
    (`(BINDINGS ,bindings ,handlers)
     (if (not (name=? name 'BINDING))
         (error "Invalid SPARQL binding:" name attributes))
     (cond ((find-attribute 'NAME attributes)
            => (lambda (attribute)
                 `(BINDING ,(attribute-value attribute) ,bindings ,handlers)))
           (else
            (error "No `name' attribute in `binding' element:" attributes))))
    (`(VARIABLE ,variable-name ,handlers)
     (error "Variables should not have child elements:" name attributes))
    (`(BINDING ,variable-name ,bindings ,handlers)
     ((lambda (datum)
        `(BINDING-VALUE ,datum () ,variable-name ,bindings ,handlers))
      (case (name->symbol name)
        ((URI) 'URI)
        ((BNODE) 'BNODE)
        ((LITERAL)
         (cond ((find-attribute 'DATATYPE attributes)
                => (lambda (attribute)
                     `(TYPED-LITERAL ,(cdr attribute))))
               ((find-attribute (make-name 'XML 'LANG) attributes)
                => (lambda (attribute)
                     `(PLAIN-LITERAL ,(cdr attribute))))
               (else
                'PLAIN-LITERAL)))
        (else
         (error "Illegal binding value:" name attributes)))))
    (else
     (error "Bogus SPARQL results seed:" seed))))

(define (handle-char-data string1 string2 seed)
  (match seed
    (`(BINDING-VALUE ,type ,value-fragments ,variable-name ,bindings ,handlers)
     `(BINDING-VALUE ,type
                     (,string2 ,string1 ,@value-fragments)
                     ,variable-name
                     ,bindings
                     ,handlers))
    (else
     (if (and (string-whitespace? string1)
              (string-whitespace? string2))
         seed
         (error "Character data in bogus context:" seed)))))

(define (string-whitespace? string)
  (loop continue ((for char (in-string string)))
    (and (char-whitespace? char)
         (continue))))

(define (handle-finish-element name attributes namespaces parent-seed seed)
  (match seed
    (`(HEAD ,variables ,handlers)
     (if (name=? name 'HEAD)
         (begin ((sparql-variables-handler handlers) (reverse variables))
                `(START-RESULTS ,handlers))
         (error "")))
    (`(VARIABLE ,variables ,handlers)
     `(HEAD ,variables ,handlers))
    (`(RESULTS ,handlers)
     `(DONE ,handlers))
    (`(DONE ,handlers)
     'DONE!)
    (`(BINDINGS ,bindings ,handlers)
     ((sparql-bindings-handler handlers) (reverse bindings))
     `(RESULTS ,handlers))
    (`(BINDING ,variable-name ,bindings ,handlers)
     (error "BINDING state should not have been encountered on element end.")
     seed)
    (`(BINDING-VALUE ,type ,value-fragments ,variable-name ,bindings ,handlers)
     `(BINDING-RETURN
       ((,variable-name . ,(process-binding type value-fragments))
        . ,bindings)
       ,handlers))
    (`(BINDING-RETURN . ,junk)
     (if (not (name=? name 'BINDING))
         (error "Misplaced seed:" seed))
     `(BINDINGS . ,junk))
    (else
     (error "Bogus SPARQL results seed:" seed))))

(define (process-binding type value-fragments)
  (let ((value (car (ssax:reverse-collect-str-drop-ws value-fragments))))
    (match type
      ('URI
       value)
      ('BNODE
       (make-rdf-bnode value))
      ('PLAIN-LITERAL
       (make-rdf-plain-literal value #f))
      (`(PLAIN-LITERAL ,language-tag)
       (make-rdf-plain-literal value language-tag))
      (`(TYPED-LITERAL ,datatype-uri)
       (make-rdf-typed-literal value datatype-uri))
      (else
       (error "Bogus binding type:" type)))))

(define (find-attribute name attributes)
  (assoc name attributes))

(define (attribute-name attribute)
  (car attribute))

(define (attribute-value attribute)
  (cdr attribute))

(define (make-name prefix local-part)
  (cons prefix local-part))

(define (name=? a b)
  (or (eq? a b)
      (and (pair? a)
           (eq? (cdr a) b))))

(define (name->symbol name)
  (cdr name))
