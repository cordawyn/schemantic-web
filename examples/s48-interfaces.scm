;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web Examples
;;;; Interface Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface rdf-examples-interface
  (export
    read-turtle-file
    display-test-manifest
    ))

(define-interface sparql-examples-interface
  (export
    neurocommons
    neurocommons-sparql-endpoint
    nc:pubmed-annotation-classes
    nc:pmids-for-articles-mentioning-gene-products-of-entrez-gene-5999
    nc:mesh-terms-for-pubmed-paper
    dbpedia
    dbpedia-sparql-endpoint
    dbpedia:persons-born-in-berlin-before-1700
    ))
