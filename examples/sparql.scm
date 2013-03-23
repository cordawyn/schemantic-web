;;; -*- Mode: Scheme; scheme48-package: sparql-examples -*-

;;;; Schemantic Web Examples
;;;; SPARQL

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define neurocommons-sparql-endpoint
  "http://sparql.neurocommons.org:8890/sparql/")

(define (neurocommons query)
  (http-get-sparql neurocommons-sparql-endpoint query))

(define (nc:pubmed-annotation-classes)
  (neurocommons
   '((prologue
      (prefix rdfs "http://www.w3.org/2000/01/rdf-schema#")
      (prefix owl "http://www.w3.org/2002/07/owl#"))
     (select (class documentation)
         ((from
           "http://sw.neurocommons.org/2007/2007-03-15/pubmed-annotations"))
       (triple class (: rdf "type") (: owl "Class"))
       (triple class (: rdfs "comment") documentation)))))

(define (nc:pmids-for-articles-mentioning-gene-products-of-entrez-gene-5999)
  (neurocommons
   '((prologue (prefix nc "http://sw.neurocommons.org/2007/annotations#"))
     (select (pmid) (distinct)
       (triple pubmed (: nc "has-id") pmid)
       (triple pubmed (: nc "has-abstract") abstract)
       (triple span (: nc "has-context") abstract)
       (triple phrase (: nc "has-context") span)
       (triple phrase (: nc "has-nc0.0-interpretation") ggp)
       (triple ggp
               (: nc "if-gene-described-by")
               "http://sw.neurocommons.org/2007/entrez-gene/5999")))))

(define (nc:mesh-terms-for-pubmed-paper paper-uri-ref)
  (neurocommons
   `((prologue
      (prefix rdfs "http://www.w3.org/2000/01/rdf-schema#")
      (prefix owl "http://www.w3.org/2002/07/owl#")
      (prefix mesh "http://purl.org/commons/record/mesh/")
      (prefix skos "http://www.w3.org/2004/02/skos/core#")
      (prefix sc "http://purl.org/science/owl/sciencecommons/"))
     (select (p term name qualifier)
         ((from "http://purl.org/commons/hcls/20070416")
          (from "http://purl.org/commons/hcls/pubmesh")
          distinct)
       (graph "http://purl.org/commons/hcls/pubmesh"
         (triple ,(rdf-uri-ref->string paper-uri-ref) p term))
       (graph "http://purl.org/commons/hcls/20070416"
         (union (triple term (: skos "prefLabel") name)
                (group
                 (triple term (: skos "narrower") uterm)
                 (triple uterm (: skos "prefLabel") name)
                 (triple term (: sc "is_qualified_by") qterm)
                 (triple qterm (: skos "prefLabel") qualifier))))))))

(define dbpedia-sparql-endpoint "http://dbpedia.org/sparql/")

(define (dbpedia query)
  (http-get-sparql dbpedia-sparql-endpoint query))

(define (dbpedia:persons-born-in-berlin-before-1700)
  (dbpedia
   '((prologue
      (prefix foaf "http://xmlns.com/foaf/0.1/")
      (prefix dbpedia "http://dbpedia.org/")
      (prefix xsd "http://www.w3.org/2001/XMLSchema#"))
     (select (name birthdate) ()
       (triples person
         ((: dbpedia "birthplace")      "http://dbpedia.org/resource/Berlin")
         ((: dbpedia "birth")           birthdate)
         ((: foaf "name")               name))
       (filter (< birthdate
                  (literal (typed (: xsd "date"))
                           "1700-01-01")))))))
