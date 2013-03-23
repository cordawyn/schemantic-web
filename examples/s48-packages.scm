;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web Examples
;;;; Package Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-module (make-rdf-examples rdf-graphs)
  (define-structure rdf-examples (export)
    (open scheme
          srfi-23                       ;error
          parser-combinators
          text-parser-combinators
          parse-errors
          rdf
          rdf-turtle-parser
          rdf-graphs
          )
    (optimize auto-integrate)
    (files rdf))
  rdf-examples)

(def rdf-simple-examples (make-rdf-examples rdf-simple-graphs))
(def rdf-list-examples (make-rdf-examples rdf-list-graphs))

(define rdf-examples rdf-list-examples)

(define-structure sparql-examples sparql-examples-interface
  (open scheme
        sparql-http-client
        rdf
        )
  (optimize auto-integrate)
  (files sparql))
