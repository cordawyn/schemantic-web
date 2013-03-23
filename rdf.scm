;;; -*- Mode: Scheme; scheme48-package: rdf -*-

;;;; Schemantic Web
;;;; RDF: Resource Description Framework

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <rdf-triple>
    (make-rdf-triple subject predicate object)
    rdf-triple?
  (subject rdf-triple/subject)
  (predicate rdf-triple/predicate)
  (object rdf-triple/object))

(define (rdf-triple=? a b)
  (and (rdf-subject=? (rdf-triple/subject a) (rdf-triple/subject b))
       (rdf-predicate=? (rdf-triple/predicate a) (rdf-triple/predicate b))
       (rdf-object=? (rdf-triple/object a) (rdf-triple/object b))))

(define (rdf-triple-hash-mod triple modulus)
  (modulo
   (bitwise-xor (rdf-subject-hash-mod (rdf-triple/subject triple) modulus)
                (rdf-predicate-hash-mod (rdf-triple/predicate triple) modulus)
                (rdf-object-hash-mod (rdf-triple/object triple) modulus))
   modulus))

(define (rdf-triple-hash triple)
  (bitwise-xor (rdf-subject-hash (rdf-triple/subject triple))
               (rdf-predicate-hash (rdf-triple/predicate triple))
               (rdf-object-hash (rdf-triple/object triple))))

(define (rdf-subject=? subject-a subject-b)
  (define (lose) (error "Illegal RDF subjects:" subject-a subject-b))
  (cond ((rdf-uri-ref? subject-a)
         (cond ((rdf-uri-ref? subject-b)
                (rdf-uri-ref=? subject-a subject-b))
               ((not (rdf-bnode? subject-b))
                (lose))
               (else #f)))
        ((rdf-bnode? subject-a)
         (cond ((rdf-bnode? subject-b)
                (rdf-bnode=? subject-a subject-b))
               ((not (rdf-uri-ref? subject-b))
                (lose))
               (else #f)))
        (else (lose))))

(define (rdf-subject-hash-mod subject modulus)
  (define (lose) (error "Illegal RDF subject:" subject))
  (cond ((rdf-uri-ref? subject) (rdf-uri-ref-hash-mod subject modulus))
        ((rdf-bnode? subject) (rdf-bnode-hash-mod subject modulus))
        (else (lose))))

(define (rdf-subject-hash subject)
  (define (lose) (error "Illegal RDF subject:" subject))
  (cond ((rdf-uri-ref? subject) (rdf-uri-ref-hash subject))
        ((rdf-bnode? subject) (rdf-bnode-hash subject))
        (else (lose))))

(define (rdf-predicate=? predicate-a predicate-b)
  (if (and (rdf-uri-ref? predicate-a)
           (rdf-uri-ref? predicate-b))
      (rdf-uri-ref=? predicate-a predicate-b)
      (error "Illegal RDF predicates:" predicate-a predicate-b)))

(define (rdf-predicate-hash-mod predicate modulus)
  (if (rdf-uri-ref? predicate)
      (rdf-uri-ref-hash-mod predicate modulus)
      (error "Illegal RDF predicate:" predicate)))

(define (rdf-predicate-hash predicate)
  (if (rdf-uri-ref? predicate)
      (rdf-uri-ref-hash predicate)
      (error "Illegal RDF predicate:" predicate)))

(define (rdf-object=? object-a object-b)
  (define (lose) (error "Illegal RDF objects:" object-a object-b))
  (cond ((rdf-uri-ref? object-a)
         (cond ((rdf-uri-ref? object-b)
                (rdf-uri-ref=? object-a object-b))
               ((not (or (rdf-bnode? object-b) (rdf-literal? object-b)))
                (lose))
               (else #f)))
        ((rdf-bnode? object-a)
         (cond ((rdf-bnode? object-b)
                (rdf-bnode=? object-a object-b))
               ((not (or (rdf-uri-ref? object-b) (rdf-literal? object-b)))
                (lose))
               (else #f)))
        ((rdf-literal? object-a)
         (cond ((rdf-literal? object-b)
                (rdf-literal=? object-a object-b))
               ((not (or (rdf-uri-ref? object-b) (rdf-bnode? object-b)))
                (lose))
               (else #f)))
        (else (lose))))

(define (rdf-object-hash-mod object modulus)
  (cond ((rdf-uri-ref? object)
         (rdf-uri-ref-hash-mod object modulus))
        ((rdf-bnode? object)
         (rdf-bnode-hash-mod object modulus))
        ((rdf-literal? object)
         (rdf-literal-hash-mod object modulus))
        (else
         (error "Illegal RDF object:" object))))

(define (rdf-object-hash object)
  (cond ((rdf-uri-ref? object)
         (rdf-uri-ref-hash object))
        ((rdf-bnode? object)
         (rdf-bnode-hash object))
        ((rdf-literal? object)
         (rdf-literal-hash object))
        (else
         (error "Illegal RDF object:" object))))

;;;; RDF Nodes

;;; What horrid lossage RDF URI references are.  *Why* are they not
;;; actually URIs, normalized according to the scheme-independent rules
;;; in the RFC?

;++ These should be interned.

(define (rdf-uri-ref? object) (string? object))
(define (rdf-uri-ref->string ref) ref)
(define (string->rdf-uri-ref string) string)
(define (rdf-uri-ref=? uri-ref-a uri-ref-b) (string=? uri-ref-a uri-ref-b))
(define (rdf-uri-ref-hash uri-ref) (string-hash uri-ref))
(define (rdf-uri-ref-hash-mod uri-ref modulus)
  (string-hash-mod uri-ref modulus))

(define (prefixed-rdf-uri-ref prefix suffix)
  (string-append prefix suffix))

(define rdf-ns-prefix "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(define (rdf-uri-ref:rdf suffix)
  (prefixed-rdf-uri-ref rdf-ns-prefix suffix))

(define xsd-ns-prefix "http://www.w3.org/2001/XMLSchema#")

(define (rdf-uri-ref:xsd suffix)
  (prefixed-rdf-uri-ref xsd-ns-prefix suffix))

;++ Is this better named `bnode' or `blank'?  `Bnode' is a noun, but
;++ terrible as an English word; `blank' is the opposite; and
;++ `blank-node' is too long.

(define-record-type <rdf-bnode>
    (make-rdf-bnode name)
    rdf-bnode?
  (name rdf-bnode/name))

(define (rdf-bnode=? bnode-a bnode-b)   ;++ Intern bnodes so we can use EQ?.
  (equal? (rdf-bnode/name bnode-a) (rdf-bnode/name bnode-b)))

;++ lossage

(define (rdf-bnode-hash bnode)
  #xDEADBEEF)

(define (rdf-bnode-hash-mod bnode modulus)
  (modulo #xDEADBEEF modulus))

;;;;; RDF Literals

(define-record-type <rdf-literal>
    (make-rdf-literal lexical-form class annotation)
    rdf-literal?
  (lexical-form rdf-literal/lexical-form)
  (class rdf-literal/class)
  (annotation rdf-literal/annotation))

(define (make-rdf-plain-literal lexical-form language-tag)
  (make-rdf-literal lexical-form 'PLAIN language-tag))

(define (rdf-plain-literal? object)
  (and (rdf-literal? object)
       (eq? 'PLAIN (rdf-literal/class object))))

(define (rdf-plain-literal/language-tag literal)
  (if (not (eq? 'PLAIN (rdf-literal/class literal)))
      (error "Non-plain RDF literal has no language tag:" literal))
  (rdf-literal/annotation literal))

(define (make-rdf-typed-literal lexical-form datatype-uri)
  (make-rdf-literal lexical-form 'TYPED datatype-uri))

(define (rdf-typed-literal? object)
  (and (rdf-literal? object)
       (eq? 'TYPED (rdf-literal/class object))))

(define (rdf-typed-literal/datatype-uri literal)
  (if (not (eq? 'TYPED (rdf-literal/class literal)))
      (error "Non-typed RDF literal has no datatype URI:" literal))
  (rdf-literal/annotation literal))

(define (disclose-rdf-literal literal)
  (let ((annotation (rdf-literal/annotation literal)))
    (case (rdf-literal/class literal)   ;Annotation is...
      ((PLAIN)                          ;  ...optional language tag.
       `(PLAIN ,@(if annotation (cons annotation '()) '())
               ,(rdf-literal/lexical-form literal)))
      ((TYPED)                          ;  ...datatype URI.
       `(TYPED ,(rdf-uri-ref->string annotation)
               ,(rdf-literal/lexical-form literal)))
      (else
       `(ILLEGAL ,(rdf-literal/class literal)
                 ,annotation
                 ,(rdf-literal/lexical-form literal))))))

;;;;;; RDF Literal Comparison and Hashing

(define (rdf-literal=? literal-a literal-b)
  (and (eq? (rdf-literal/class literal-a) (rdf-literal/class literal-b))
       (let ((annotation-a (rdf-literal/annotation literal-a))
             (annotation-b (rdf-literal/annotation literal-b)))
         (case (rdf-literal/class literal-a)
           ((PLAIN) (equal? annotation-a annotation-b))
           ((TYPED) (rdf-uri-ref=? annotation-a annotation-b))
           (else (error "Illegal RDF literals:" literal-a literal-b))))
       (equal? (rdf-literal/lexical-form literal-a)
               (rdf-literal/lexical-form literal-b))))

(define (rdf-literal-hash-mod literal modulus)
  (modulo
   (bitwise-xor (string-hash-mod (rdf-literal/lexical-form literal) modulus)
                (let ((annotation (rdf-literal/annotation literal)))
                  (case (rdf-literal/class literal)
                    ((PLAIN)
                     (if annotation
                         (string-hash-mod annotation modulus)
                         (modulo #xDEADBEEF modulus)))
                    ((TYPED) (rdf-uri-ref-hash-mod annotation modulus))
                    (else (error "Illegal RDF literal:" literal)))))
   modulus))


(define (rdf-literal-hash literal)
  (bitwise-xor (string-hash (rdf-literal/lexical-form literal))
               (let ((annotation (rdf-literal/annotation literal)))
                 (case (rdf-literal/class literal)
                   ((PLAIN)
                    (if annotation
                        (string-hash annotation)
                        #xCAFE))
                   ((TYPED) (rdf-uri-ref-hash annotation))
                   (else (error "Illegal RDF literal:" literal))))))
