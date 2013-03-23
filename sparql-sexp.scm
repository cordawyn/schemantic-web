;;; -*- Mode: Scheme; scheme48-package: sparql-sexp -*-

;;;; Schemantic Web
;;;; SPARQL in S-Expressions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (sparql->condensed-string form)
  (format-to-string (format-with-condensed-whitespace)
                    (compile-sparql form)))

(define (sparql->string form)
  (format-to-string
   (format-options
    (format-with-line-tracking 0 0)
    (format-with-indentation 0))
   (compile-sparql form)))

(define (write-sparql form output-port)
  (format-to-port output-port
                  (format-options
                   (format-with-line-tracking 0 0)
                   (format-with-indentation 0))
                  (compile-sparql form)))

(define (write-sparql-condensed form output-port)
  (format-to-port output-port
                  (format-with-condensed-whitespace)
                  (compile-sparql form)))

(define (compile-sparql form)
  (match form
    ((('PROLOGUE declarations ___) query)
     (format:sequence (compile-sparql-prologue declarations)
                      (compile-sparql-query query)))
    (query
     (compile-sparql-query query))))

(define (compile-sparql-prologue prologue)
  ((lambda (match-prefixes)
     (match prologue
       ((('BASE base-iri) . declarations)
        (format:sequence (sparql-format:base (sparql-format:iri-ref base-iri))
                         (match-prefixes declarations)))
       (declarations
        (match-prefixes declarations))))
   (lambda (declarations)
     (format:list
      (map (match-lambda
            (('PREFIX prefix expansion)
             (sparql-format:prefix prefix (sparql-format:iri-ref expansion)))
            (('PREFIX expansion)
             (sparql-format:prefix "" (sparql-format:iri-ref expansion)))
            (declaration
             (error "Illegal SPARQL prologue declaration:"
                    declaration)))
           declarations)))))

(define (compile-sparql-query query)
  (cond ((and (pair? query)
              (assq (car query) *query-compilators*))
         => (lambda (entry)
              ((cdr entry) query)))
        (else
         (error "Malformed SPARQL query:" query))))

(define *query-compilators* '())

(define (*define-query-compilator keyword compilator)
  (cond ((assq keyword *query-compilators*)
         => (lambda (entry)
              (warn "Redefining SPARQL query compilator:" keyword)
              (set-cdr! entry compilator)))
        (else
         (set! *query-compilators*
               (cons (cons keyword compilator)
                     *query-compilators*))))
  keyword)

(define-syntax define-query-compilator*
  (syntax-rules ()
    ((DEFINE-QUERY-COMPILATOR* keyword ((ignored . pattern) body) ...)
     (*DEFINE-QUERY-COMPILATOR keyword
       (LAMBDA (QUERY)
         (MATCH (CDR QUERY)
           (pattern body)
           ...
           (ELSE
            (ERROR "Malformed SPARQL query:" QUERY))))))))

(define-syntax define-query-compilator
  (syntax-rules ()
    ((DEFINE-QUERY-COMPILATOR (keyword . pattern) body)
     (DEFINE-QUERY-COMPILATOR* keyword
       ((keyword . pattern)
        body)))))

(define-query-compilator ('SELECT variables (options ___) graph-patterns ___)
  (receive (duplicate-modifier datasets solution-modifiers)
           (parse-select-options options)
    (sparql-format:select duplicate-modifier
                          (if (eq? variables '*)
                              '*
                              ;++ check that they are variables
                              (map compile-sparql-expression variables))
                          datasets
                          (compile-sparql-patterns graph-patterns)
                          solution-modifiers)))

(define-query-compilator ('CONSTRUCT (options ___) (triples ___)
                            graph-patterns ___)
  (receive (datasets solution-modifiers)
           (parse-construct-options options)
    (sparql-format:construct (compile-sparql-patterns triples)
                             datasets
                             (compile-sparql-patterns graph-patterns)
                             solution-modifiers)))

(define-query-compilator ('DESCRIBE resources (options ___) graph-patterns ___)
  (receive (datasets solution-modifiers)
           (parse-describe-options options)
    (sparql-format:describe (if (eq? resources '*)
                                '*
                                (map compile-sparql-expression
                                     (if (pair? resources)
                                         resources
                                         (cons resources '()))))
                            datasets
                            (and (pair? graph-patterns)
                                 (compile-sparql-patterns graph-patterns))
                            solution-modifiers)))

(define-query-compilator ('ASK (options ___) graph-patterns ___)
  (let ((datasets (parse-ask-options options)))
    (sparql-format:ask datasets (compile-sparql-patterns graph-patterns))))

(define (parse-select-options options)
  (loop continue ((for option (in-list options))
                  (with duplicate-modifier #f)
                  (with datasets #f)
                  (with solution-modifiers '()))
    => (values duplicate-modifier
               (and datasets (reverse datasets))
               (reverse solution-modifiers))
    (match option
      ((or 'DISTINCT 'REDUCED)
       (if duplicate-modifier
           (error "Multiple duplicate modifiers:" duplicate-modifier option)
           (continue (=> duplicate-modifier option))))
      (((or 'FROM 'FROM-NAMED) dataset)
       (continue (=> datasets
                     (cons ((case (car option)
                              ((FROM) sparql-format:from-dataset)
                              ((FROM-NAMED) sparql-format:from-named-dataset))
                            (compile-sparql-expression dataset))
                           (or datasets '())))))
      (('ORDER-BY expressions ___)
       (continue (=> solution-modifiers
                     (cons (sparql-format:order-by:list
                            (compile-sparql-expressions expressions))
                           solution-modifiers))))
      (((or 'LIMIT 'OFFSET) operand)
       (continue (=> solution-modifiers
                     (cons (case (car option)
                             ((LIMIT) (sparql-format:limit operand))
                             ((OFFSET) (sparql-format:offset operand)))
                           solution-modifiers))))
      (else
       (error "Invalid SPARQL SELECT query option:" option)))))

(define (parse-construct-options options)
  (loop continue ((for option (in-list options))
                  (with datasets #f)
                  (with solution-modifiers '()))
    => (values (and datasets (reverse datasets))
               (reverse solution-modifiers))
    (match option
      (((or 'FROM 'FROM-NAMED) dataset)
       (continue (=> datasets
                     (cons ((case (car option)
                              ((FROM) sparql-format:from-dataset)
                              ((FROM-NAMED) sparql-format:from-named-dataset))
                            (compile-sparql-expression dataset))
                           (or datasets '())))))
      (('ORDER-BY expressions ___)
       (continue (=> solution-modifiers
                     (cons (sparql-format:order-by:list
                            (compile-sparql-expressions expressions))
                           solution-modifiers))))
      (((or 'LIMIT 'OFFSET) operand)
       (continue (=> solution-modifiers
                     (cons (case (car option)
                             ((LIMIT) (sparql-format:limit operand))
                             ((OFFSET) (sparql-format:offset operand)))
                           solution-modifiers))))
      (else
       (error "Invalid SPARQL CONSTRUCT query option:" option)))))

(define (parse-describe-options options)
  (parse-construct-options options))

(define (parse-ask-options options)
  (loop continue ((for option (in-list options))
                  (with datasets #f))
    => (and datasets (reverse datasets))
    (match option
      (((or 'FROM 'FROM-NAMED) dataset)
       (continue (=> datasets
                     (cons ((case (car option)
                              ((FROM) sparql-format:from-dataset)
                              ((FROM-NAMED) sparql-format:from-named-dataset))
                            (compile-sparql-expression dataset))
                           datasets))))
      (else
       (error "Invalid SPARQL ASK query option:" option)))))

(define (compile-sparql-pattern pattern)
  (cond ((and (pair? pattern)
              (assq (car pattern) *pattern-compilators*))
         => (lambda (entry)
              ((cdr entry) pattern)))
        (else
         (error "Malformed SPARQL pattern:" pattern))))

(define (compile-sparql-patterns patterns)
  (sparql-format:pattern-list (map compile-sparql-pattern patterns)))

(define *pattern-compilators* '())

(define (*define-pattern-compilator keyword compilator)
  (cond ((assq keyword *pattern-compilators*)
         => (lambda (entry)
              (warn "Redefining SPARQL pattern compilator:" keyword)
              (set-cdr! entry compilator)))
        (else
         (set! *pattern-compilators*
               (cons (cons keyword compilator)
                     *pattern-compilators*))))
  keyword)

(define-syntax define-pattern-compilator*
  (syntax-rules ()
    ((DEFINE-PATTERN-COMPILATOR* keyword ((ignored . pattern) body) ...)
     (*DEFINE-PATTERN-COMPILATOR keyword
       (LAMBDA (PAT)
         (MATCH (CDR PAT)
           (pattern body)
           ...
           (ELSE
            (ERROR "Malformed SPARQL pattern:" PAT))))))))

(define-syntax define-pattern-compilator
  (syntax-rules ()
    ((DEFINE-PATTERN-COMPILATOR (keyword . pattern) procedure)
     (DEFINE-PATTERN-COMPILATOR* keyword
       ((keyword . pattern)
        procedure)))))

(define-pattern-compilator ('TRIPLE subject predicate object-list ___)
  (sparql-format:triple-pattern
   (compile-sparql-expression subject)
   (sparql-format:property (compile-sparql-predicate predicate)
                           (sparql-format:object-list
                            (compile-sparql-expressions object-list)))))

(define-pattern-compilator ('TRIPLES subject (predicates object-lists ___) ___)
  (sparql-format:triple-pattern
   (compile-sparql-expression subject)
   (compile-sparql-property-list predicates object-lists)))

(define (compile-sparql-property-list predicates object-lists)
  (sparql-format:property-list
   (map (lambda (predicate object-list)
          (sparql-format:property
           (compile-sparql-predicate predicate)
           (sparql-format:object-list
            (compile-sparql-expressions object-list))))
        predicates
        object-lists)))

(define-pattern-compilator ('GROUP patterns ___)
  (sparql-format:group (compile-sparql-patterns patterns)))

(define-pattern-compilator ('FILTER expression)
  (sparql-format:filter (compile-sparql-expression expression)))

(define-pattern-compilator ('OPTIONAL patterns ___)
  (sparql-format:optional (compile-sparql-patterns patterns)))

(define-pattern-compilator ('UNION patterns ___)
  (sparql-format:union-list (map compile-sparql-pattern patterns)))

(define-pattern-compilator ('GRAPH graph patterns ___)
  (sparql-format:graph-pattern (compile-sparql-expression graph)
                               (compile-sparql-patterns patterns)))

(define (compile-sparql-expression expression)
  (cond ((pair? expression)
         (cond ((assq (car expression) *expression-compilators*)
                => (lambda (entry)
                     ((cdr entry) expression)))
               (else
                (compile-sparql-call expression))))
        ((number? expression)
         (sparql-format:number expression))
        ((boolean? expression)
         (sparql-format:boolean expression))
        ((or (string? expression) (uri? expression))
         (sparql-format:iri-ref expression))
        ((symbol? expression)
         (sparql-format:variable expression))
        (else
         (error "Malformed SPARQL expression:" expression))))

(define (compile-sparql-expressions expressions)
  (map compile-sparql-expression expressions))

(define (compile-sparql-predicate predicate) ;++ crock
  (if (eq? predicate 'A) "a" (compile-sparql-expression predicate)))

(define (compile-sparql-call expression)
  (sparql-format:call-list
   (let ((operator (car expression)))
     (if (symbol? operator)
         (sparql-format:keyword operator)
         ;; This should really check whether the operator is an IRI reference.
         (compile-sparql-expression operator)))
   (compile-sparql-expressions (cdr expression))))

(define *expression-compilators* '())

(define (*define-expression-compilator keyword compilator)
  (cond ((assq keyword *expression-compilators*)
         => (lambda (entry)
              (warn "Redefining SPARQL expression compilator:" keyword)
              (set-cdr! entry compilator)))
        (else
         (set! *expression-compilators*
               (cons (cons keyword compilator)
                     *expression-compilators*))))
  keyword)

(define-syntax define-expression-compilator*
  (syntax-rules ()
    ((DEFINE-EXPRESSION-COMPILATOR* keyword
       ((ignored . pattern) body)
       ...)
     (*DEFINE-EXPRESSION-COMPILATOR keyword
       (LAMBDA (EXPRESSION)
         (MATCH (CDR EXPRESSION)
           (pattern body)
           ...
           (ELSE
            (ERROR "Malformed SPARQL expression:" EXPRESSION))))))))

(define-syntax define-expression-compilator
  (syntax-rules ()
    ((DEFINE-EXPRESSION-COMPILATOR (keyword . pattern) body)
     (DEFINE-EXPRESSION-COMPILATOR* keyword
       ((keyword . pattern)
        body)))))

(define-expression-compilator* ':
  ((': prefix local-part)
   (sparql-format:prefixed-name prefix local-part))
  ((': local-part)
   (sparql-format:prefixed-name "" local-part)))

(define-expression-compilator ('? name)
  (sparql-format:variable name))

(define-expression-compilator ('$ name)
  (sparql-format:variable name))

(define-expression-compilator ('_ name)
  (sparql-format:named-bnode name))

(define-expression-compilator ('BNODE (predicates object-lists ___) ___)
  (sparql-format:bnode
   (compile-sparql-property-list predicates object-lists)))

(define-expression-compilator ('COLLECTION elements ___)
  (sparql-format:collection-list (compile-sparql-expressions elements)))

(define-expression-compilator* 'LITERAL
  (('LITERAL lexical-form)
   (sparql-format:plain-literal lexical-form #f))
  (('LITERAL 'PLAIN lexical-form)
   (sparql-format:plain-literal lexical-form #f))
  (('LITERAL ('PLAIN language-tag) lexical-form)
   (sparql-format:plain-literal lexical-form language-tag))
  (('LITERAL ('TYPED datatype-iri) lexical-form)
   (sparql-format:typed-literal lexical-form
                                (compile-sparql-expression datatype-iri)))
  (('LITERAL language-tag lexical-form)
   (sparql-format:plain-literal lexical-form language-tag)))

((lambda (procedure)                    ;Unary operators
   (for-each procedure
             `((~ ,sparql-format:~)
               (! ,sparql-format:!))))
 (lambda (entry)
   (let ((keyword (car entry))
         (formatter (cadr entry)))
     (define-expression-compilator (keyword operand)
       (formatter (compile-sparql-expression operand))))))

((lambda (procedure)                    ;Binary operators
   (for-each procedure
             `((- ,sparql-format:-)
               (/ ,sparql-format:/))))
 (lambda (entry)
   (let ((keyword (car entry))
         (formatter (cadr entry)))
     (define-expression-compilator (keyword a b)
       (formatter (compile-sparql-expression a)
                  (compile-sparql-expression b))))))

((lambda (procedure)                    ;N-ary operators
   (for-each procedure
             `((+ ,sparql-format:+:list)
               (* ,sparql-format:*:list)
               (AND ,sparql-format:and)
               (OR ,sparql-format:or))))
 (lambda (entry)
   (let ((keyword (car entry))
         (formatter (cadr entry)))
     (define-expression-compilator (keyword operands ___)
       (formatter (compile-sparql-expressions operands))))))

((lambda (procedure)                    ;Binary relations
   (for-each procedure
             `((= ,sparql-format:=:list)
               (!= ,sparql-format:!=:list)
               (< ,sparql-format:<:list)
               (> ,sparql-format:>:list)
               (<= ,sparql-format:<=:list)
               (>= ,sparql-format:>=:list))))
 (lambda (entry)
   (let ((keyword (car entry))
         (formatter (cadr entry)))
     (define-expression-compilator (keyword a b comparands ___)
       (formatter (compile-sparql-expression a)
                  (compile-sparql-expression b)
                  (compile-sparql-expressions comparands))))))
