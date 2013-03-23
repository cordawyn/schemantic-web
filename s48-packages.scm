;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web
;;;; Package Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;;; URIs

(define-structures ((uris uris-interface)
                    (uri-parser uri-parser-interface))
  (open scheme
        srfi-1                          ;List Library
        srfi-6                          ;Basic String Ports
        srfi-9                          ;define-record-type
        srfi-13                         ;String Library
        srfi-14                         ;Character-Set Library
        srfi-23                         ;Error
        ascii
        (subset i/o (write-string))
        (subset extended-ports (call-with-string-output-port))
        matcher-combinators
        text-matcher-combinators
        parser-combinators
        text-parser-combinators
        parse-errors
        )
  (optimize auto-integrate)

  (open tables weak)
  (begin
    ;++ This is bogus.  Also, `internment camp' might, just might, not
    ;++ be quite the right name, but it works for now.
    (define (make-string-internment-camp) (make-string-table))
    (define (intern internment-camp string generator)
      (define (generate)
        (let ((datum (generator)))
          (table-set! internment-camp string (make-weak-pointer datum))
          datum))
      (cond ((table-ref internment-camp string)
             => (lambda (weak-pointer)
                  (or (weak-pointer-ref weak-pointer)
                      (generate))))
            (else (generate))))
    (define (soft-intern internment-camp string)
      (cond ((table-ref internment-camp string)
             => weak-pointer-ref)
            (else #f)))
    )

  (files uri)

  ;; Make URIs print nicely.
  (open (subset define-record-types (define-record-discloser)))
  (begin
    ;; Using symbols for the tag avoids quotes when pretty-printed.
    ;; Using STRING->SYMBOL lets us maintain the case.
    (define-record-discloser <uri>
      (let ((symbol (string->symbol "URI")))
        (lambda (uri)
          `(,symbol ,(%uri-string uri)))))
    (define-record-discloser <uri-authority>
      (let ((symbol (string->symbol "URI-authority")))
        (lambda (authority)
          `(,symbol ,(uri-authority-string authority)))))
    ))

(define-structure uri-tests (export uri-tests)
  (open scheme simple-testing uris uri-parser)
  (optimize auto-integrate)
  (files test-uri))

;;;; RDF Structures

(define-structure rdf rdf-interface
  (open scheme
        srfi-9                          ;define-record-type
        (modify srfi-13                 ;string-lib
                (alias (string-hash string-hash-mod)))
        srfi-23                         ;error
        bitwise                         ; For hashing
        uris
        )
  (optimize auto-integrate)
  (files rdf)

  ;; Make RDF data structures print nicely.
  (open (subset define-record-types (define-record-discloser)))
  (begin

    (define-record-discloser <rdf-triple>
      (lambda (triple)
        (list 'RDF-TRIPLE
              (let ((subject (rdf-triple/subject triple)))
                (cond ((rdf-uri-ref? subject) (rdf-uri-ref->string subject))
                      ((rdf-bnode? subject) `(BNODE ,(rdf-bnode/name subject)))
                      (else `(ILLEGAL-SUBJECT ,subject))))
              (let ((predicate (rdf-triple/predicate triple)))
                (if (rdf-uri-ref? predicate)
                    (rdf-uri-ref->string predicate)
                    `(ILLEGAL-PREDICATE ,predicate)))
              (let ((object (rdf-triple/object triple)))
                (cond ((rdf-uri-ref? object) (rdf-uri-ref->string object))
                      ((rdf-bnode? object) `(BNODE ,(rdf-bnode/name object)))
                      ((rdf-literal? object)
                       `(LITERAL ,@(disclose-rdf-literal object)))
                      (else `(ILLEGAL-OBJECT ,object)))))))

    (define-record-discloser <rdf-bnode>
      (lambda (bnode)
        (list 'RDF-BNODE (rdf-bnode/name bnode))))

    (define-record-discloser <rdf-literal>
      (lambda (literal)
        (cons 'RDF-LITERAL (disclose-rdf-literal literal))))))

;;;;; RDF Graph Structures

(define-structure rdf-list-graphs rdf-graphs-interface
  (open (modify scheme (hide member))   ; SRFI 1 redefines it.
        srfi-1                          ;List Library
        srfi-9                          ;define-record-types
        rdf
        )
  (optimize auto-integrate)
  (files rdf-list-graph))

(define-structure rdf-simple-graphs rdf-graphs-interface
  (open (modify scheme (hide member))   ; SRFI 1 redefines it
        srfi-1                          ;List Library
        srfi-9                          ;define-record-type
        rdf
        rdf-maps
        foof-loop
        nested-foof-loop
        )
  (optimize auto-integrate)
  (files rdf-simple-graph))

(define-structure rdf-maps-internal rdf-maps-internal-interface
  (open scheme tables rdf foof-loop)
  (optimize auto-integrate)
  (files s48-rdf-map))

;;; This is better than writing a number of definitions in
;;; s48-rdf-map.scm, because any redefinitions will be reflected in
;;; clients...in theory, anyway, if a certain bug weren't present in
;;; Scheme48.

(def rdf-maps
  (modify rdf-maps-internal
          ;; The modification commands are processed right-to-left.
          (hide map/delete! map/insert! map/intern! map/lookup
                map/search map/size map/update! map/modify! map/walk
                in-map)
          (alias
           (map->alist rdf-object-map->alist)
           (map/datum-list rdf-object-map/datum-list)
           (map/delete! rdf-object-map/delete!)
           (map/key-list rdf-object-map/key-list)
           (map/insert! rdf-object-map/insert!)
           (map/intern! rdf-object-map/intern!)
           (map/lookup rdf-object-map/lookup)
           (map/search rdf-object-map/search)
           (map/size rdf-object-map/size)
           (map/update! rdf-object-map/update!)
           (map/modify! rdf-object-map/modify!)
           (map/walk rdf-object-map/walk)
           (in-map in-rdf-object-map)

           (map->alist rdf-predicate-map->alist)
           (map/datum-list rdf-predicate-map/datum-list)
           (map/delete! rdf-predicate-map/delete!)
           (map/key-list rdf-predicate-map/key-list)
           (map/insert! rdf-predicate-map/insert!)
           (map/intern! rdf-predicate-map/intern!)
           (map/lookup rdf-predicate-map/lookup)
           (map/search rdf-predicate-map/search)
           (map/size rdf-predicate-map/size)
           (map/update! rdf-predicate-map/update!)
           (map/modify! rdf-predicate-map/modify!)
           (map/walk rdf-predicate-map/walk)
           (in-map in-rdf-predicate-map)

           (map->alist rdf-subject-map->alist)
           (map/datum-list rdf-subject-map/datum-list)
           (map/delete! rdf-subject-map/delete!)
           (map/key-list rdf-subject-map/key-list)
           (map/insert! rdf-subject-map/insert!)
           (map/intern! rdf-subject-map/intern!)
           (map/lookup rdf-subject-map/lookup)
           (map/search rdf-subject-map/search)
           (map/size rdf-subject-map/size)
           (map/update! rdf-subject-map/update!)
           (map/modify! rdf-subject-map/modify!)
           (map/walk rdf-subject-map/walk)
           (in-map in-rdf-subject-map)
           )))

;;;; RDF Parsers

(define-structure rdf-nt-parser rdf-nt-parser-interface
  (open scheme
        srfi-1                          ;List Library
        srfi-9                          ;define-record-type
        srfi-14                         ;Character-Set Library
        srfi-23                         ;Error
        ascii
        uri-parser
        rdf
        matcher-combinators
        text-matcher-combinators
        parser-combinators
        text-parser-combinators
        )
  (optimize auto-integrate)
  (files rdf-nt-parser))

(define-structure rdf-nt-parser-tests (export rdf-nt-parser-tests)
  (open scheme
        srfi-23                         ;error
        simple-testing
        rdf
        rdf-nt-parser
        text-parser-combinators
        parse-errors
        )
  (optimize auto-integrate)
  (files test-rdf-nt-parser))

(define-structure rdf-turtle-parser rdf-turtle-parser-interface
  (open scheme
        srfi-1                          ;List Library
        srfi-9                          ;define-record-type
        srfi-14                         ;Character-Set Library
        srfi-23                         ;Error
        ascii
        uri-parser
        rdf
        matcher-combinators
        text-matcher-combinators
        parser-combinators
        text-parser-combinators
        )
  (optimize auto-integrate)
  (files rdf-turtle-parser))

(define-structure rdf-turtle-parser-tests (export rdf-turtle-parser-tests)
  (open scheme
        srfi-23                         ;error
        simple-testing
        rdf
        rdf-turtle-parser
        text-parser-combinators
        parse-errors
        )
  (optimize auto-integrate)
  (files test-rdf-turtle-parser))

;; (define-structure rdf-xml-parser ...)

;;;; SPARQL Client

(define-structure sparql-format sparql-format-interface
  (open scheme
        srfi-1                          ;List Library
        srfi-6                          ;Basic String Ports
        srfi-13                         ;String Library
        srfi-23                         ;error
        uris
        ascii
        format-combinators
        foof-loop
        nested-foof-loop
        )
  (optimize auto-integrate)
  (files sparql-format))

(define-structure sparql-sexp sparql-sexp-interface
  (open scheme
        receiving
        srfi-23                         ;error
        (subset simple-signals (warn))  ;++ gnargh
        uris
        pattern-matching
        foof-loop
        format-driver
        format-combinators
        sparql-format
        )
  (files sparql-sexp))

(define-structure sparql-sexp-tests (export sparql-sexp-tests)
  (open scheme
        simple-testing
        sparql-sexp
        format-combinators
        format-driver
        )
  (optimize auto-integrate)
  (files test-sparql-sexp))

(define-structure sparql-results-ssax sparql-results-interface
  (open scheme
        srfi-23
        pattern-matching
        foof-loop
        ssax-vanilla
        rdf
        )
  (optimize auto-integrate)
  (files sparql-results-ssax))

(define sparql-results sparql-results-ssax)

(define-structure sparql-http-client sparql-http-client-interface
  (open scheme
        simple-signals
        simple-http-client
        sparql-sexp
        sparql-results
        uris
      )
  (optimize auto-integrate)
  (files sparql-http-client))

(define-structure simple-http-client simple-http-client-interface
  (open scheme
        receiving
        srfi-1                          ;List Library
        srfi-6                          ;Basic String Ports
        srfi-9                          ;define-record-type
        srfi-13                         ;String Library
        srfi-14                         ;Character Set Library
        ascii
        simple-signals
        fluids
        sockets
        (subset i/o (write-string write-block read-block force-output))
        uris
        foof-loop
        nested-foof-loop
        laziness
        lazy-streams
        parser-combinators
        text-parser-combinators
        matcher-combinators
        text-matcher-combinators
        parse-errors
        format-combinators
        format-driver
        )
  (optimize auto-integrate)
  (files s48-http-client))
