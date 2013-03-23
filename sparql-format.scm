;;; -*- Mode: Scheme; scheme48-package: sparql-format -*-

;;;; Schemantic Web
;;;; SPARQL Format

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-format (sparql-format:prefix prefix expansion)
  (format:indented-line
   (sparql-format:with-keyword 'PREFIX
                               (format:sequence (or prefix "") ":")
                               expansion)))

(define-format (sparql-format:base base-iri)
  (format:indented-line (sparql-format:with-keyword 'BASE base-iri)))

;;;; Nodes

(define-format (sparql-format:iri-ref iri-ref)
  (format:bracketed "<" ">"
    (cond ((string? iri-ref)
           ;++ Check whether it is a valid IRI reference.
           iri-ref)
          ((uri? iri-ref)
           (uri->string iri-ref))
          (else
           (error "Invalid SPARQL IRI reference:" iri-ref)))))

(define-format (sparql-format:prefixed-name prefix local-part)
  (format:sequence prefix ":" local-part))

(define-format (sparql-format:?variable name)
  (format:sequence "?" name))

(define-format (sparql-format:$variable name)
  (format:sequence "$" name))

(define-format (sparql-format:variable name)
  (format:sequence (sparql-format:variable-prefix) name))

(define-format (sparql-format:variable-prefix)
  ;++ refer to the format state
  "?")

(define-format (sparql-format:properties . properties)
  (sparql-format:property-list properties))

(define-format (sparql-format:property-list properties)
  (format:join/infix (format:sequence ";"
                                      (format:line-break)
                                      (format:indentation))
                     properties))

(define-format (sparql-format:property predicate objects)
  (format:sequence predicate
                   (format:soft-break)
                   (format:with-indentation +2 objects)))

(define-format (sparql-format:objects . objects)
  (sparql-format:object-list objects))

(define-format (sparql-format:object-list objects)
  (format:join/infix (format:sequence "," (format:soft-break)) objects))

;;;;; Blank Nodes and Literals

(define-format (sparql-format:named-bnode name)
  (format:sequence "_:" name))

(define-format (sparql-format:bnode . properties)
  (format:bracketed "[ " " ]"
    (format:with-indentation +1
      (sparql-format:property-list properties))))

(define-format (sparql-format:collection . elements)
  (sparql-format:collection-list elements))

(define-format (sparql-format:collection-list elements)
  (format:bracketed "(" ")"
    (format:with-indentation +1
      (format:join/infix (format:soft-break) elements))))

(define-format (sparql-format:plain-literal lexical-form language-tag)
  (format:sequence (sparql-format:quoted-string lexical-form)
                   (if language-tag
                       (format:sequence "@" language-tag)
                       (format:empty))))

(define-format (sparql-format:typed-literal lexical-form datatype-iri)
  (format:sequence (sparql-format:quoted-string lexical-form)
                   "^^"
                   datatype-iri))

;++ This should choose intelligently between string formats.

(define-format (sparql-format:quoted-string lexical-form)
  (format:bracketed #\" #\"
    (collect-display (for char (in-string lexical-form))
      (cond ((assv char sparql-literal-escapes)
             => (lambda (entry)
                  (string-append "\\" (cdr entry))))
            (else char)))))

(define sparql-literal-escapes
  (map (lambda (entry)
         (cons (if (number? (car entry)) (ascii->char (car entry)) (car entry))
               (cdr entry)))
       `((#\"   #\")
         (#\\   #\\)
         (#x08  #\b)
         (#x0A  #\n)
         (#x0C  #\f)
         (#x0D  #\r))))

;;;; Top-Level Query Formats

(define-format (sparql-format:query keyword . body)
  (format:indented-line (sparql-format:with-keyword:list keyword body)))

(define-format (sparql-format:select duplicate-modifier
                                     variables
                                     datasets
                                     graph-pattern
                                     solution-modifiers)
  (sparql-format:query 'SELECT
    (sparql-format:duplicate-modifier duplicate-modifier)
    (format:join/infix (format:soft-break)
                       (if (eq? variables '*) (list "*") variables))
    (sparql-format:datasets datasets)
    (sparql-format:with-keyword 'WHERE (sparql-format:group graph-pattern))
    (sparql-format:solution-modifiers solution-modifiers)))

(define-format (sparql-format:construct template
                                        datasets
                                        graph-pattern
                                        solution-modifiers)
  (sparql-format:query 'CONSTRUCT
    (sparql-format:group template)
    (sparql-format:datasets datasets)
    (sparql-format:with-keyword 'WHERE (sparql-format:group graph-pattern))
    (sparql-format:solution-modifiers solution-modifiers)))

(define-format (sparql-format:describe resources
                                       datasets
                                       graph-pattern
                                       solution-modifiers)
  (sparql-format:query 'DESCRIBE
    (format:join/prefix (format:soft-break)
                        (if (eq? resources '*) (list "*") resources))
    (sparql-format:datasets datasets)
    (if graph-pattern
        (sparql-format:with-keyword 'WHERE (sparql-format:group graph-pattern))
        (format:empty))
    (sparql-format:solution-modifiers solution-modifiers)))

(define-format (sparql-format:ask datasets graph-pattern)
  (sparql-format:query 'ASK
    (sparql-format:datasets datasets)
    (sparql-format:group graph-pattern)))

;;; Internal utilities for the above; don't call these manually.

(define-format (sparql-format:duplicate-modifier duplicate-modifier)
  (if duplicate-modifier
      (sparql-format:keyword duplicate-modifier)
      (format:empty)))

(define-format (sparql-format:datasets datasets)
  (if datasets
      (format:with-indentation +2
        (format:indented-line
         (format:join/infix (format:soft-break) datasets)))
      (format:empty)))

(define-format (sparql-format:group pattern)
  (format:bracketed (format:indented-line "{") (format:indented-line "}")
    (format:with-indentation +2 pattern)))

(define-format (sparql-format:solution-modifiers solution-modifiers)
  (format:join/infix (format:soft-break) solution-modifiers))

;;; Use these to construct arguments to the former.

(define-format (sparql-format:from-dataset dataset)
  (sparql-format:with-keyword 'FROM dataset))

(define-format (sparql-format:from-named-dataset dataset)
  (sparql-format:with-keyword '(FROM NAMED) dataset))

(define-format (sparql-format:order-by condition . conditions)
  (sparql-format:order-by:list (cons condition conditions)))

(define-format (sparql-format:order-by:list conditions)
  (sparql-format:with-keyword:list '(ORDER BY) conditions))

(define-format (sparql-format:limit number)
  (sparql-format:with-keyword 'LIMIT (format:number-with-radix number #d10)))

(define-format (sparql-format:offset number)
  (sparql-format:with-keyword 'OFFSET (format:number-with-radix number #d10)))

;;;; Graph Patterns

(define-format (sparql-format:patterns . patterns)
  (sparql-format:pattern-list patterns))

(define-format (sparql-format:pattern-list patterns)
  (map format:indented-line patterns))

(define-format (sparql-format:triple-pattern subject . properties)
  (format:sequence subject
                   (format:soft-break)
                   (format:with-indentation +2
                     (sparql-format:property-list properties))
                   (format:soft-break)
                   "."))

(define-format (sparql-format:filter expression)
  (sparql-format:with-keyword 'FILTER expression))

(define-format (sparql-format:optional pattern)
  (sparql-format:with-keyword 'OPTIONAL (sparql-format:group pattern)))

(define-format (sparql-format:union . patterns)
  (sparql-format:union-list patterns))

(define-format (sparql-format:union-list patterns)
  (if (pair? patterns)
      (format:bracketed (format:indented-line "{") (format:indented-line "}")
        (format:join/infix (format:indented-line "}"
                                                 (format:soft-break)
                                                 (sparql-format:keyword 'UNION)
                                                 (format:soft-break)
                                                 "{")
          patterns))
      (sparql-format:group (sparql-format:pattern-list '()))))

(define-format (sparql-format:graph-pattern graph pattern)
  (sparql-format:with-keyword 'GRAPH graph (sparql-format:group pattern)))

;;;; Expressions

(define-format (sparql-format:parenthesis expression)
  (format:sequence "(" (format:with-indentation +1 expression) ")"))

(define-format (sparql-format:call-list operator operands)
  (format:sequence operator
                   (format:soft-break)
                   (sparql-format:parenthesis
                    (sparql-format:object-list operands))))

(define-format (sparql-format:call operator . operands)
  (sparql-format:call-list operator operands))

(define-format (sparql-format:unary operator operand)
  (sparql-format:parenthesis
   (format:sequence operator (format:soft-break) operand)))

(define-format (sparql-format:~ operand)
  (sparql-format:unary "-" operand))

(define-format (sparql-format:! operand)
  (sparql-format:unary "!" operand))

(define-format (sparql-format:binary left-operand operator right-operand)
  (sparql-format:parenthesis
   (sparql-format:bare-binary left-operand operator right-operand)))

(define-format (sparql-format:bare-binary left-operand operator right-operand)
  (format:sequence left-operand
                   (format:soft-break)
                   operator
                   (format:soft-break)
                   right-operand))

(define-format (sparql-format:binary-reduction operator operands identity)
  (if (pair? operands)
      (sparql-format:parenthesis
       (reduce-right (lambda (left-operand right-operand)
                       (sparql-format:bare-binary left-operand
                                                  operator
                                                  right-operand))
               #f                       ;No identity
               operands))
      identity))

(define-format (sparql-format:+:list addends)
  (sparql-format:binary-reduction "+" addends (format:number 0)))

(define-format (sparql-format:+ . addends)
  (sparql-format:+:list addends))

(define-format (sparql-format:- minuend subtrahend)
  (sparql-format:binary minuend "-" subtrahend))

(define-format (sparql-format:*:list multiplicands)
  (sparql-format:binary-reduction "*" multiplicands (format:number 1)))

(define-format (sparql-format:* . multiplicands)
  (sparql-format:*:list multiplicands))

(define-format (sparql-format:/ dividend divisor)
  (sparql-format:binary dividend "/" divisor))

;;;;; Boolean Expressions

(define-format (sparql-format:boolean boolean)
  (if boolean (sparql-format:true) (sparql-format:false)))

(define-format (sparql-format:true) "true")
(define-format (sparql-format:false) "false")

(define-format (sparql-format:and:list conjuncts)
  (sparql-format:binary-reduction "&&" conjuncts (sparql-format:true)))

(define-format (sparql-format:and . conjuncts)
  (sparql-format:and:list conjuncts))

(define-format (sparql-format:or:list disjuncts)
  (sparql-format:binary-reduction "||" disjuncts (sparql-format:false)))

(define-format (sparql-format:or . conjuncts)
  (sparql-format:or:list conjuncts))

(define-format (sparql-format:comparison comparator a b comparands)
  (if (pair? comparands)
      (sparql-format:and:list
       (let recur ((a a) (b b) (comparands comparands))
         (cons (sparql-format:binary a comparator b)
               (if (pair? (cddr comparands))
                   (recur b (car comparands) (cdr comparands))
                   '()))))
      (sparql-format:binary a comparator b)))

(define-format (sparql-format:=:list a b comparands)
  (sparql-format:comparison "=" a b comparands))

(define-format (sparql-format:!=:list a b comparands)
  (sparql-format:comparison "!=" a b comparands))

(define-format (sparql-format:<:list a b comparands)
  (sparql-format:comparison "<" a b comparands))

(define-format (sparql-format:>:list a b comparands)
  (sparql-format:comparison ">" a b comparands))

(define-format (sparql-format:<=:list a b comparands)
  (sparql-format:comparison "<=" a b comparands))

(define-format (sparql-format:>=:list a b comparands)
  (sparql-format:comparison ">=" a b comparands))

(define-format (sparql-format:= a b . comparands)
  (sparql-format:=:list a b comparands))

(define-format (sparql-format:!= a b . comparands)
  (sparql-format:!=:list a b comparands))

(define-format (sparql-format:< a b . comparands)
  (sparql-format:<:list a b comparands))

(define-format (sparql-format:> a b . comparands)
  (sparql-format:>:list a b comparands))

(define-format (sparql-format:<= a b . comparands)
  (sparql-format:<=:list a b comparands))

(define-format (sparql-format:>= a b . comparands)
  (sparql-format:>=:list a b comparands))

(define-format (sparql-format:with-keyword keyword . elements)
  (sparql-format:with-keyword:list keyword elements))

(define-format (sparql-format:with-keyword:list keyword elements)
  (format:join
      (format:sequence                  ;Prefix
       (if (list? keyword)
           (sparql-format:keyword:list keyword)
           (sparql-format:keyword keyword))
       (format:soft-break))
      (format:soft-break)               ;Infix
      (format:empty)                    ;Suffix
    elements))

(define-format (sparql-format:keyword . symbols)
  (sparql-format:keyword:list symbols))

(define-format (sparql-format:keyword:list symbols)
  (format:join/infix (format:non-breaking-space)
                     (map (lambda (symbol)
                            (string-upcase (symbol->string symbol)))
                          symbols)))

(define-format (sparql-format:number number)
  (format:number number))
