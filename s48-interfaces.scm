;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Schemantic Web
;;;; Interface Descriptions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface uris-interface
  (export
    make-uri
    uri?
    absolute-uri?
    relative-uri?
    uri-absolute?
    uri-relative?
    uri-scheme
    uri-authority
    uri-path
    uri-query
    uri-fragment
    uri=?

    ;; URI Authorities
    make-uri-authority
    uri-authority?
    uri-authority-userinfo
    uri-authority-host
    uri-authority-port
    uri-authority=?

    ;; URI Component Predicates
    uri-scheme?
    uri-userinfo?
    uri-host?
    uri-port?
    uri-path?
    uri-path-absolute?
    uri-path-relative?
    uri-query?
    uri-fragment?

    ;; URI Operations
    merge-uris

    ;; URI->String Conversion
    uri->string
    uri-authority->string
    write-uri
    write-uri-authority

    ;; String->URI Conversion
    object->uri
    object->absolute-uri
    object->relative-uri
    maybe-string->uri
    maybe-string->absolute-uri
    maybe-string->relative-uri
    string->uri
    string->absolute-uri
    string->relative-uri
    ))

;;;; RDF Utilities

(define-interface rdf-interface
  (export
    make-rdf-triple
    rdf-triple?
    rdf-triple/subject
    rdf-triple/predicate
    rdf-triple/object
    rdf-triple=?
    rdf-triple-hash
    rdf-triple-hash-mod

    rdf-subject=?       rdf-subject-hash        rdf-subject-hash-mod
    rdf-predicate=?     rdf-predicate-hash      rdf-predicate-hash-mod
    rdf-object=?        rdf-object-hash         rdf-object-hash-mod

    make-rdf-bnode
    rdf-bnode?
    rdf-bnode/name
    rdf-bnode=?
    rdf-bnode-hash

    make-rdf-plain-literal
    make-rdf-typed-literal
    rdf-literal?
    rdf-plain-literal?
    rdf-typed-literal?
    rdf-literal/lexical-form
    rdf-plain-literal/language-tag
    rdf-typed-literal/datatype-uri
    rdf-literal=?
    rdf-literal-hash

    rdf-uri-ref?
    rdf-uri-ref->string
    string->rdf-uri-ref
    rdf-uri-ref=?
    rdf-uri-ref-hash
    ))

;;;;; RDF Graphs and Maps

(define-interface rdf-graphs-interface
  (export
    make-rdf-graph
    rdf-graph?
    rdf-graph/add-triple!
    rdf-graph/all-objects
    rdf-graph/all-predicates
    rdf-graph/all-subjects
    rdf-graph/all-triples
    rdf-graph/for-each-matching-object
    rdf-graph/for-each-matching-predicate
    rdf-graph/for-each-matching-subject
    rdf-graph/for-each-object
    rdf-graph/for-each-predicate
    rdf-graph/for-each-subject
    rdf-graph/for-each-triple-by-object
    rdf-graph/for-each-triple-by-predicate
    rdf-graph/for-each-triple-by-subject
    rdf-graph/for-each-triple
    rdf-graph/matching-objects
    rdf-graph/matching-predicates
    rdf-graph/matching-subjects
    rdf-graph/purge-matching-objects!
    rdf-graph/purge-matching-predicates!
    rdf-graph/purge-matching-subjects!
    rdf-graph/purge-triples-by-object!
    rdf-graph/purge-triples-by-predicate!
    rdf-graph/purge-triples-by-subject!
    rdf-graph/remove-triple!
    rdf-graph/size
    rdf-graph/triples-by-object
    rdf-graph/triples-by-predicate
    rdf-graph/triples-by-subject
    ))

(define-interface rdf-maps-internal-interface
  (export
    (in-map :syntax)
    make-rdf-object-map
    make-rdf-predicate-map
    make-rdf-subject-map
    map->alist
    map/datum-list
    map/delete!
    map/key-list
    map/insert!
    map/intern!
    map/lookup
    map/modify!
    map/search
    map/size
    map/update!
    map/walk
    ))

(define-interface rdf-maps-interface
  (export
    (in-rdf-object-map :syntax)
    (in-rdf-predicate-map :syntax)
    (in-rdf-subject-map :syntax)
    make-rdf-object-map
    make-rdf-predicate-map
    make-rdf-subject-map
    rdf-object-map->alist
    rdf-object-map/datum-list
    rdf-object-map/delete!
    rdf-object-map/key-list
    rdf-object-map/insert!
    rdf-object-map/intern!
    rdf-object-map/lookup
    rdf-object-map/modify!
    rdf-object-map/search
    rdf-object-map/size
    rdf-object-map/update!
    rdf-object-map/walk
    rdf-predicate-map->alist
    rdf-predicate-map/datum-list
    rdf-predicate-map/delete!
    rdf-predicate-map/key-list
    rdf-predicate-map/insert!
    rdf-predicate-map/intern!
    rdf-predicate-map/lookup
    rdf-predicate-map/modify!
    rdf-predicate-map/search
    rdf-predicate-map/size
    rdf-predicate-map/update!
    rdf-predicate-map/walk
    rdf-subject-map->alist
    rdf-subject-map/datum-list
    rdf-subject-map/delete!
    rdf-subject-map/key-list
    rdf-subject-map/insert!
    rdf-subject-map/intern!
    rdf-subject-map/lookup
    rdf-subject-map/modify!
    rdf-subject-map/search
    rdf-subject-map/size
    rdf-subject-map/update!
    rdf-subject-map/walk
    ))

;;;; Parsers

(define-interface uri-parser-interface
  (export
    uri-parser:uri-reference
    uri-parser:uri
    uri-parser:relative-ref
    uri-parser:absolute-uri             ;No fragment
    uri-matcher:uri-reference
    uri-matcher:uri
    uri-matcher:relative-ref
    uri-matcher:absolute-uri
    ))

(define-interface rdf-nt-parser-interface
  (export
    nt-parser:document
    make-nt-parser-context
    ))

(define-interface rdf-turtle-parser-interface
  (export
    turtle-parser:document
    make-turtle-parser-context
    ))

(define-interface sparql-format-interface
  (export
    sparql-format:!
    sparql-format:+
    sparql-format:+:list
    sparql-format:-
    sparql-format:*
    sparql-format:*:list
    sparql-format:/
    sparql-format:=
    sparql-format:=:list
    sparql-format:!=
    sparql-format:!=:list
    sparql-format:<
    sparql-format:<:list
    sparql-format:>
    sparql-format:>:list
    sparql-format:<=
    sparql-format:<=:list
    sparql-format:>=
    sparql-format:>=:list
    sparql-format:~
    sparql-format:and
    sparql-format:and:list
    sparql-format:ask
    sparql-format:base
    sparql-format:bnode
    sparql-format:boolean
    sparql-format:call
    sparql-format:call-list
    sparql-format:collection
    sparql-format:collection-list
    sparql-format:construct
    sparql-format:describe
    sparql-format:false
    sparql-format:filter
    sparql-format:from-dataset
    sparql-format:from-named-dataset
    sparql-format:graph-pattern
    sparql-format:group
    sparql-format:iri-ref
    sparql-format:keyword
    sparql-format:limit
    sparql-format:named-bnode
    sparql-format:number
    sparql-format:object-list
    sparql-format:objects
    sparql-format:offset
    sparql-format:optional
    sparql-format:or
    sparql-format:or:list
    sparql-format:order-by
    sparql-format:order-by:list
    sparql-format:parenthesis
    sparql-format:pattern-list
    sparql-format:plain-literal
    sparql-format:prefix
    sparql-format:prefixed-name
    sparql-format:properties
    sparql-format:property
    sparql-format:property-list
    sparql-format:select
    sparql-format:triple-pattern
    sparql-format:true
    sparql-format:typed-literal
    sparql-format:union
    sparql-format:union-list
    sparql-format:$variable
    sparql-format:?variable
    sparql-format:variable
    sparql-format:variable-prefix
    ))

(define-interface sparql-sexp-interface
  (export
    compile-sparql
    compile-sparql-prologue
    compile-sparql-query
    compile-sparql-pattern
    compile-sparql-patterns
    compile-sparql-expression
    compile-sparql-expressions
    sparql->condensed-string
    sparql->string
    write-sparql
    write-sparql-condensed
    ))

(define-interface sparql-results-interface
  (export
    parse-sparql-results
    ))

(define-interface sparql-http-client-interface
  (export
    http-get-sparql
    http-post-sparql
    ))

(define-interface simple-http-client-interface
  (export
    call-with-http-response
    default-http-uri-authority
    earliest-http-version
    http-get
    http-head
    http-header-field/name
    http-header-field/value
    http-post
    http-response?
    http-response/header-fields
    http-response/reason
    http-response/status-code
    http-response/status-type
    http-response/version
    http-version?
    http-version=?
    http-version<=?
    http-version/major
    http-version/minor
    latest-http-version
    make-http-header-field
    make-http-response
    make-http-version
    read-http-entity-body
    with-default-http-uri-authority
    ))
