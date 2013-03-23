;;; -*- Mode: Scheme; scheme48-package: rdf-turtle-parser -*-

;;;; Schemantic Web
;;;; RDF Turtle Parser

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; The version of RDF Turtle parsed by this code is described at
;;; <http://www.dajobe.org/2004/01/turtle/2006-09-11/>.

;;; The parser uses the noise repetition operator because we pass each
;;; triple off to a user-supplied triple handler, and any result is
;;; accumulated in the user's state, rather than using the parser's
;;; value to accumulate anything.

(define-parser turtle-parser:document
  (parser:sequence
   (parser:noise:repeated-until (parser:end) turtle-parser:statement)
   (parser:map turtle-context/user-state (parser:context))))

(define-parser turtle-parser:statement
  (parser:choice (parser:sequence (parser:choice turtle-parser:directive
                                                 turtle-parser:triples)
                                  turtle-parser:ws*
                                  (parser:char= #\.)
                                  turtle-parser:ws*)
                 turtle-parser:ws+))

(define-parser turtle-parser:directive
  (*parser
      ((parser:string= "@prefix"))
      (turtle-parser:ws+)
      (prefix-name (parser:optional #f turtle-parser:prefix-name))
      ((parser:char= #\:))
      (turtle-parser:ws+)
      (prefix-expansion turtle-parser:uri-ref)
    (turtle:add-prefix-expansion prefix-name prefix-expansion)))

(define-parser turtle-parser:triples
  (*parser
      (subject turtle-parser:subject)
      (turtle-parser:ws+)
    (turtle-parser:predicate/object-list subject)))

;;;; Predicate/Object Lists

(define-parser (turtle-parser:predicate/object-list subject)
  (*parser
      (predicate turtle-parser:verb)
      (turtle-parser:ws+)
      ((turtle-parser:object-list subject predicate))
    (turtle-parser:predicate/object-list-continuation subject)))

(define-parser turtle-parser:verb
  (parser:choice (parser:backtrackable
                  (*parser
                      ((parser:char= #\a))
                      ((parser:peek turtle-parser:ws))
                    (parser:return rdf:type)))
                 turtle-parser:predicate))

(define-parser (turtle-parser:object-list subject predicate)
  (let ((object-parser (turtle-parser:object->triple subject predicate)))
    (*parser
        (object-parser)
        (turtle-parser:ws*)
      (parser:noise:repeated
       (*parser
           ((parser:char= #\,))
           (turtle-parser:ws*)
           (object-parser)
         turtle-parser:ws*)))))

(define-parser (turtle-parser:predicate/object-list-continuation subject)
  (parser:optional-noise
   (*parser
       ((parser:char= #\;))
       (turtle-parser:ws*)
     (parser:optional-noise
      (*parser
          (predicate turtle-parser:verb)
          (turtle-parser:ws+)
          ((turtle-parser:object-list subject predicate))
        (turtle-parser:predicate/object-list-continuation subject))))))

(define-parser (turtle-parser:object->triple subject predicate)
  (*parser (object turtle-parser:object)
    (turtle:add-triple (make-rdf-triple subject predicate object))))

;;;; Resources

(define-parser turtle-parser:subject
  (parser:choice turtle-parser:resource
                 turtle-parser:blank))

(define-parser turtle-parser:predicate
  turtle-parser:resource)

(define-parser turtle-parser:object
  (parser:choice turtle-parser:literal
                 turtle-parser:blank
                 turtle-parser:resource))

(define-parser turtle-parser:resource
  (*parser (uri-ref
            (parser:choice turtle-parser:uri-ref turtle-parser:qname))
    (if (match-string? uri-matcher:uri-reference uri-ref)
        (parser:return (string->rdf-uri-ref uri-ref))
        (parser:error
         (string-append "Malformed URI reference `" uri-ref "'")))))

(define-parser turtle-parser:uri-ref
  (parser:bracketed-string (parser:char= #\<) (parser:char= #\>)
    (turtle-parser:string-char turtle-char-set:ucharacter
                               turtle-string-escapes:uri-ref)))

(define-parser turtle-parser:literal
  (parser:choice turtle-parser:string
                 turtle-parser:number
                 turtle-parser:boolean))

(define-parser turtle-parser:string
  (*parser (lexical-form turtle-parser:quoted-string)
    (parser:choice
     (*parser (datatype-uri turtle-parser:literal-datatype-uri)
       (parser:return (make-rdf-typed-literal lexical-form datatype-uri)))
     (*parser (language-tag
               (parser:choice turtle-parser:literal-language-tag
                              (parser:return #f)))
       (parser:return (make-rdf-plain-literal lexical-form language-tag))))))

(define-parser turtle-parser:literal-language-tag
  (parser:sequence
   (parser:char= #\@)
   (parser:match->string
    (matcher:sequence
     (matcher:at-least 1
       (matcher:char-in-set turtle-char-set:language-initial))
     (matcher:repeated
      (matcher:sequence
       (matcher:char= #\-)
       (matcher:at-least 1
         (matcher:char-in-set turtle-char-set:language-trailing))))))))

(define-parser turtle-parser:literal-datatype-uri
  (parser:sequence (parser:string= "^^") turtle-parser:resource))

(define-parser turtle-parser:boolean
  (*parser (boolean
            (parser:backtrackable
             (parser:choice (parser:string= "true")
                            (parser:string= "false"))))
    (parser:return (make-rdf-typed-literal boolean xsd:boolean))))

;;;;; Numbers

(define-parser turtle-parser:number
  (parser:sequence (parser:choice (parser:char= #\+)
                                  (parser:char= #\-)
                                  (parser:char-in-set char-set:digit))
                   (parser:error "Number parsing is not yet supported.")))

;;;;; Strings

(define-parser turtle-parser:quoted-string
  (parser:sequence
   (parser:char= #\")
   (parser:choice
    (parser:sequence (parser:backtrackable (parser:string= "\"\""))
                     turtle-parser:long-string-contents)
    turtle-parser:short-string-contents)))

(define-parser turtle-parser:long-string-contents
  (parser:string:repeated-until
      (parser:backtrackable (parser:string= "\"\"\""))
    (turtle-parser:string-char turtle-char-set:lcharacter
                               turtle-string-escapes:long-string)))

(define-parser turtle-parser:short-string-contents
  (parser:string:repeated-until (parser:char= #\")
    (turtle-parser:string-char turtle-char-set:scharacter
                               turtle-string-escapes:short-string)))

(define-parser (turtle-parser:string-char char-set escapes)
  (parser:choice (*parser (char (parser:char= #\\))
                   (turtle-parser:string-escape escapes))
                 (parser:char-in-set char-set)))

(define-parser (turtle-parser:string-escape escapes)
  (*parser (escape-char (parser:char))
    (cond ((char=? escape-char #\u) (turtle-parser:unicode-escape 4))
          ((char=? escape-char #\U) (turtle-parser:unicode-escape 8))
          ((assv escape-char escapes)
           => (lambda (entry)
                (parser:return (cadr entry))))
          (else
           (parser:error
            (string-append "Invalid string escape `\\"
                           (string escape-char)
                           "'"))))))

(define-parser (turtle-parser:unicode-escape length)
  (*parser (hex-string (parser:hex-string length))
    (parser:return
     (let ((number (string->number hex-string #x10)))
       (if (< number ascii-limit)
           (ascii->char number)
           #\?)))))                     ;++ fix

(define turtle-string-escapes
  `((#\\ #\\)
    (#\t ,(ascii->char #x09))           ;Horizontal tab
    (#\n ,(ascii->char #x0A))           ;Line feed
    (#\r ,(ascii->char #x0D))))         ;Carriage return

(define turtle-string-escapes:long-string
  `((#\" #\")
    ,@turtle-string-escapes))

(define turtle-string-escapes:short-string
  turtle-string-escapes:long-string)

(define turtle-string-escapes:uri-ref
  `((#\> #\>)
    ,@turtle-string-escapes))

;;;;; Blank Nodes

(define-parser turtle-parser:blank
  (parser:choice turtle-parser:bnode:named
                 turtle-parser:bnode:empty
                 turtle-parser:bnode:compound
                 turtle-parser:bnode:collection))

(define-parser turtle-parser:bnode:named
  (*parser (node-id turtle-parser:node-id)
    (parser:return (make-rdf-bnode node-id))))

(define-parser turtle-parser:node-id
  (parser:sequence (parser:string= "_:") turtle-parser:name))

(define-parser turtle-parser:bnode:empty
  (parser:backtrackable
   ;; This is a little silly, but the Turtle specification does not
   ;; explictly allow a space between these brackets.
   (parser:sequence (parser:string= "[]") turtle:new-anonymous-bnode)))

(define-parser turtle-parser:bnode:compound
  (parser:bracketed
      (parser:sequence (parser:char= #\[) turtle-parser:ws*)
      (parser:sequence turtle-parser:ws* (parser:char= #\]))
    (*parser (subject turtle:new-anonymous-bnode)
        ((turtle-parser:predicate/object-list subject))
      (parser:return subject))))

;;; The following ugliness is the obvious recursive parser translated
;;; by hand according to the tail recursion modulo CONS pattern.

(define-parser turtle-parser:bnode:collection
  (*parser ((parser:char= #\( ))
      (turtle-parser:ws*)
    (parser:choice
     (parser:sequence (parser:char= #\) ) (parser:return rdf:nil))
     (*parser (bnode turtle:new-anonymous-bnode)
       (let loop ((pair bnode))
         (*parser
             ((turtle-parser:object->triple pair rdf:first))
             (turtle-parser:ws*)
           (parser:choice
            (*parser
                ((parser:char= #\) ))
                ((turtle:add-triple (make-rdf-triple pair rdf:rest rdf:nil)))
              (parser:return bnode))
            (*parser
                (rest turtle:new-anonymous-bnode)
                ((turtle:add-triple (make-rdf-triple pair rdf:rest rest)))
              (loop rest)))))))))

;;;; Names

(define-parser turtle-parser:qname
  (*parser
      (prefix-name (parser:optional #f turtle-parser:prefix-name))
      ((parser:char= #\:))
      (suffix-text (parser:optional #f turtle-parser:name))
      (prefix-expansion (turtle:expand-prefix prefix-name))
    (if prefix-expansion
        (parser:return
         (if suffix-text
             (string-append prefix-expansion suffix-text)
             prefix-expansion))
        (parser:error
         (string-append "Unknown prefix `" prefix-name "'")))))

(define-parser turtle-parser:prefix-name
  (parser:match->string
   (matcher:sequence
    (matcher:char-in-set (char-set-delete turtle-char-set:name-initial #\_))
    (matcher:repeated (matcher:char-in-set turtle-char-set:name-trailing)))))

(define-parser turtle-parser:name
  (parser:match->string
   (matcher:sequence
    (matcher:char-in-set turtle-char-set:name-initial)
    (matcher:repeated (matcher:char-in-set turtle-char-set:name-trailing)))))

;;;; Miscellaneous

(define-parser turtle-parser:ws
  (parser:choice (parser:char-in-set turtle-char-set:ws)
                 turtle-parser:comment))

(define-parser turtle-parser:ws*
  (parser:noise:repeated turtle-parser:ws))

(define-parser turtle-parser:ws+
  (parser:noise:at-least 1 turtle-parser:ws))

(define-parser turtle-parser:comment
  (parser:sequence
   (parser:char= #\#)
   (parser:noise:repeated
    (parser:char-not-in-set turtle-char-set:line-break))))

(define-parser (parser:hex-string length)
  (parser:string:exactly length (parser:char-in-set char-set:hex-digit)))

;;;; Turtle Character Sets

;;; We use SRFI 14's UCS-RANGE->CHAR-SET several times here.  The upper
;;; bounds are all one off from what you'll find in the Turtle
;;; specification, because, for whatever stupid reason, SRFI 14's
;;; UCS-RANGE->CHAR-SET works with exclusive upper bounds.  I am sorry.

(define turtle-char-set:line-break
  (char-set (ascii->char #x0A)          ;Line feed
            (ascii->char #x0D)))        ;Carriage return

(define turtle-char-set:ws
  (char-set (ascii->char #x09)          ;Horizontal tab
            (ascii->char #x0A)          ;Line feed
            (ascii->char #x0D)          ;Carriage return
            (ascii->char #x20)))        ;Horizontal space

(define turtle-char-set:character
  (char-set-union (ucs-range->char-set #x20 #x5C)
                  (ucs-range->char-set #x5D #x110000)))

(define turtle-char-set:ucharacter
  (char-set-delete turtle-char-set:character #\>))

(define turtle-char-set:scharacter
  (char-set-delete turtle-char-set:character #\"))

(define turtle-char-set:lcharacter
  (char-set-adjoin turtle-char-set:character
                   (ascii->char #x09)   ;Horizontal tab
                   (ascii->char #x0A)   ;Carriage return
                   (ascii->char #x0D))) ;Line feed

(define turtle-char-set:language-initial
  char-set:lower-case)

(define turtle-char-set:language-trailing
  (char-set-union char-set:lower-case
                  char-set:digit))

(define turtle-char-set:name-initial
  (char-set-union (char-set #\_)
                  char-set:letter
                  (ucs-range->char-set #x00C0 #x00D7)
                  (ucs-range->char-set #x00D8 #x00F7)
                  (ucs-range->char-set #x00F8 #x0300)
                  (ucs-range->char-set #x0370 #x037E)
                  (ucs-range->char-set #x037F #x2000)
                  (ucs-range->char-set #x200C #x200E)
                  (ucs-range->char-set #x2070 #x2190)
                  (ucs-range->char-set #x2C00 #x2FF0)
                  (ucs-range->char-set #x3001 #xD800)
                  (ucs-range->char-set #xF900 #xFDD0)
                  (ucs-range->char-set #xFDF0 #xFFFE)
                  (ucs-range->char-set #x10000 #xF0000)))

(define turtle-char-set:name-trailing
  (char-set-union turtle-char-set:name-initial
                  (char-set #\- (ascii->char #x00B7))
                  char-set:digit
                  (ucs-range->char-set #x0300 #x0370)
                  (ucs-range->char-set #x203F #x2041)))

(define-record-type <turtle-context>
    (%make-turtle-context triple-handler user-state prefix-map bnode-number)
    turtle-context?
  (triple-handler turtle-context/triple-handler)
  (user-state turtle-context/user-state)
  (prefix-map turtle-context/prefix-map)
  (bnode-number turtle-context/bnode-number))

(define (make-turtle-parser-context triple-handler initial-user-state)
  (%make-turtle-context triple-handler initial-user-state '() 0))

(define (turtle-context/increment-bnode-number context)
  (%make-turtle-context (turtle-context/triple-handler context)
                        (turtle-context/user-state context)
                        (turtle-context/prefix-map context)
                        (+ 1 (turtle-context/bnode-number context))))

(define (turtle-context/add-triple context triple)
  (%make-turtle-context (turtle-context/triple-handler context)
                        ((turtle-context/triple-handler context)
                         triple
                         (turtle-context/user-state context))
                        (turtle-context/prefix-map context)
                        (turtle-context/bnode-number context)))

(define (turtle-context/add-prefix-expansion context name expansion)
  (%make-turtle-context (turtle-context/triple-handler context)
                        (turtle-context/user-state context)
                        (cons (cons name expansion)
                              (turtle-context/prefix-map context))
                        (turtle-context/bnode-number context)))

(define (turtle-context/expand-prefix context name)
  (any (if name
           (lambda (entry)
             (and (string? (car entry))
                  (string=? (car entry) name)
                  (cdr entry)))
           (lambda (entry)
             (and (not (car entry))
                  (cdr entry))))
       (turtle-context/prefix-map context)))

;;;; Turtle Context-Related Parsers

(define-parser turtle:new-anonymous-bnode
  (parser:extend (parser:context)
    (lambda (context)
      (let ((number (turtle-context/bnode-number context)))
        (parser:sequence
         (parser:set-context (turtle-context/increment-bnode-number context))
         (parser:return
          (make-rdf-bnode
           (string-append "gen" (number->string number #d10)))))))))

(define-parser (turtle:add-triple triple)
  (parser:modify-context
   (lambda (context)
     (turtle-context/add-triple context triple))))

(define-parser (turtle:add-prefix-expansion name expansion)
  (parser:modify-context
   (lambda (context)
     (turtle-context/add-prefix-expansion context name expansion))))

(define-parser (turtle:expand-prefix name)
  (parser:call-with-context
   (lambda (context)
     (turtle-context/expand-prefix context name))))

;;;; Standard URI References

(define rdf-prefix
  "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(define (rdf: suffix)
  (string->rdf-uri-ref (string-append rdf-prefix suffix)))

(define rdf:type (rdf: "type"))
(define rdf:first (rdf: "first"))
(define rdf:rest (rdf: "rest"))
(define rdf:nil (rdf: "nil"))

(define xsd-prefix
  "http://www.w3.org/2001/XMLSchema#")

(define (xsd: suffix)
  (string->rdf-uri-ref (string-append xsd-prefix suffix)))

(define xsd:boolean (xsd: "boolean"))
(define xsd:decimal (xsd: "decimal"))
(define xsd:double (xsd: "double"))
(define xsd:integer (xsd: "integer"))
