;;; -*- Mode: Scheme; scheme48-package: rdf-examples -*-

;;;; Schemantic Web Examples
;;;; RDF Miscellanea

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (read-turtle-file pathname)
  (parse-file turtle-parser:document
              pathname
              (make-turtle-parser-context
               (lambda (triple graph)
                 (rdf-graph/add-triple! graph triple)
                 graph)
               (make-rdf-graph))
              (lambda (graph context stream)
                context stream          ;ignore
                graph)
              (lambda (perror context stream)
                context stream
                (apply error "Parse error:"
                       (parse-error/position perror)
                       (parse-error/messages perror)))))

(define rdf-prefix "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define (rdf: local) (string->rdf-uri-ref (string-append rdf-prefix local)))

(define rdfs-prefix "http://www.w3.org/2000/01/rdf-schema#")
(define (rdfs: local) (string->rdf-uri-ref (string-append rdfs-prefix local)))

;;;; Showing Test Manifests

(define mf-prefix "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#")
(define (mf: local) (string->rdf-uri-ref (string-append mf-prefix local)))

(define qt-prefix "http://www.w3.org/2001/sw/DataAccess/tests/test-query#")
(define (qt: local) (string->rdf-uri-ref (string-append qt-prefix local)))

(define (display-test-manifest graph output-port)
  (rdf-graph/for-each-matching-subject graph (rdf: "type") (mf: "Manifest")
    (lambda (manifest)
      (rdf-graph/for-each-matching-object graph manifest (rdfs: "comment")
        (lambda (comment)
          (display-rdf-object comment output-port)
          (newline output-port)))
      (display "  Entries:" output-port)
      (rdf-graph/for-each-matching-object graph manifest (mf: "entries")
        (lambda (entries-start)
          (rdf-graph/for-each-collection-element graph entries-start
            (lambda (entry)
              (newline output-port)
              (display "   " output-port)
              (rdf-graph/for-each-matching-object graph entry (mf: "name")
                (lambda (name)
                  (display " " output-port)
                  (display-rdf-object name output-port)))
              (newline output-port)
              (rdf-graph/for-each-matching-object graph entry (rdfs: "comment")
                (lambda (comment)
                  (display "      " output-port)
                  (display-rdf-object comment output-port)
                  (newline output-port))))))))))

(define (rdf-graph/for-each-collection-element graph start procedure)
  (rdf-graph/for-each-matching-object graph start (rdf: "first") procedure)
  (rdf-graph/for-each-matching-object graph start (rdf: "rest")
    (lambda (rest)
      (rdf-graph/for-each-collection-element graph rest procedure))))

(define (display-rdf-object object output-port)
  (cond ((rdf-uri-ref? object)
         (display "<" output-port)
         (display (rdf-uri-ref->string object) output-port)
         (display ">" output-port))
        ((rdf-bnode? object)
         (display "_:" output-port)
         (display (rdf-bnode/name object) output-port))
        ((rdf-literal? object)
         (cond ((rdf-plain-literal? object)
                (cond ((rdf-plain-literal/language-tag object)
                       => (lambda (language-tag)
                            (display "(" output-port)
                            (display language-tag output-port)
                            (display ")" output-port)
                            (write-char #\space output-port)))))
               ((rdf-typed-literal? object)
                (display "<" output-port)
                (display (rdf-uri-ref->string
                          (rdf-typed-literal/datatype-uri object))
                         output-port)
                (display ">" output-port)
                (write-char #\space output-port)))
         (display (rdf-literal/lexical-form object) output-port))
        (else
         (error "Illegal RDF object:" object))))
