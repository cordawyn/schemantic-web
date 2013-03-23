;;; -*- Mode: Scheme; scheme48-package: sparql-sexp-tests -*-

;;;; Schemantic Web
;;;; Tests for SPARQL in S-Expressions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-test-suite sparql-sexp-tests
  "SPARQL in S-expressions")

;++ All of the results have spaces appended to them, because I haven't
;++ implemented pending soft breaking yet, which would allow a final
;++ trailing soft break in the formatter to be empty.

;;; The SPARQL queries with which these correspond from the W3C TR have
;;; more periods than in the document.  This is because we add a full
;;; stop after every triple pattern, whether we need it or not, for the
;;; sake of consistency.  We could also put infix full stops in pattern
;;; lists, which would omit the useless full stop at the end of a
;;; pattern group, but which would add more ones after non-triple
;;; patterns like filter patterns or optional patterns.

(define-test-case sparql-sexp-tests section-2.1 ()
  (test-equal "SELECT ?title WHERE { <http://example.org/book/book1> <http://purl.org/dc/elements/1.1/title> ?title . } "
    (sparql->condensed-string
     '(select (title) ()
        (triple "http://example.org/book/book1"
                "http://purl.org/dc/elements/1.1/title"
                title)))))

(define-test-case sparql-sexp-tests section-2.2 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name ?mbox WHERE { ?x foaf:name ?name . ?x foaf:mbox ?mbox . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name mbox) ()
         (triple x (: foaf "name") name)
         (triple x (: foaf "mbox") mbox))))))

(define-test-case sparql-sexp-tests section-2.3.1 ()
  (test-equal "SELECT ?v WHERE { ?v ?p \"cat\" . } "
    (sparql->condensed-string
     '(select (v) () (triple v p (literal "cat")))))
  (test-equal "SELECT ?v WHERE { ?v ?p \"cat\"@en . } "
    (sparql->condensed-string
     '(select (v) () (triple v p (literal (plain en) "cat"))))))

(define-test-case sparql-sexp-tests section-2.3.2 ()
  (test-equal "SELECT ?v WHERE { ?v ?p 42 . } "
    (sparql->condensed-string
     '(select (v) () (triple v p 42)))))

(define-test-case sparql-sexp-tests section-2.3.3 ()
  (test-equal "SELECT ?v WHERE { ?v ?p \"abc\"^^<http://example.org/datatype#specialDatatype> . } "
    (sparql->condensed-string
     '(select (v) ()
        (triple v
                p
                (literal (typed "http://example.org/datatype#specialDatatype")
                         "abc"))))))

(define-test-case sparql-sexp-tests section-2.4 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?x ?name WHERE { ?x foaf:name ?name . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (x name) ()
         (triple x (: foaf "name") name))))))

(define-test-case sparql-sexp-tests section-2.5 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX org: <http://example.com/ns#> CONSTRUCT { ?x foaf:name ?name . } WHERE { ?x org:employeeName ?name . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix org "http://example.com/ns#"))
       (construct ()
           ((triple x (: foaf "name") name))
         (triple x (: org "employeeName") name))))))

(define-test-case sparql-sexp-tests section-3.1 ()
  (test-equal "PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?title WHERE { ?x dc:title ?title . FILTER REGEX (?title, \"^SPARQL\") } "
    (sparql->condensed-string
     '((prologue (prefix dc "http://purl.org/dc/elements/1.1/"))
       (select (title) ()
         (triple x (: dc "title") title)
         (filter (regex title (literal "^SPARQL")))))))
  (test-equal "PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?title WHERE { ?x dc:title ?title . FILTER REGEX (?title, \"web\", \"i\") } "
    (sparql->condensed-string
     '((prologue (prefix dc "http://purl.org/dc/elements/1.1/"))
       (select (title) ()
         (triple x (: dc "title") title)
         (filter (regex title (literal "web") (literal "i"))))))))

(define-test-case sparql-sexp-tests section-3.2 ()
  (test-equal "PREFIX dc: <http://purl.org/dc/elements/1.1/> PREFIX ns: <http://example.org/ns#> SELECT ?title ?price WHERE { ?x ns:price ?price . FILTER (?price < 30.5) ?x dc:title ?title . } "
    (sparql->condensed-string
     '((prologue (prefix dc "http://purl.org/dc/elements/1.1/")
                 (prefix ns "http://example.org/ns#"))
       (select (title price) ()
         (triple x (: ns "price") price)
         (filter (< price 30.5))
         (triple x (: dc "title") title))))))

(define (pattern->condensed-string pattern)
  (format-to-string (format-with-condensed-whitespace)
                    (compile-sparql-pattern pattern)))

(define (patterns->condensed-string patterns)
  (format-to-string (format-with-condensed-whitespace)
                    (compile-sparql-patterns patterns)))

(define (expression->condensed-string expression)
  (format-to-string (format-with-condensed-whitespace)
                    (compile-sparql-expression expression)))

(define-test-case sparql-sexp-tests section-4.1.4 ()
  (test-equal "[ :p \"v\" ]"
    (expression->condensed-string
     '(bnode ((: p) (literal "v")))))
  (test-equal "[ :p \"v\" ] :q \"w\" ."
    (pattern->condensed-string
     '(triple (bnode ((: p) (literal "v")))
              (: q)
              (literal "w"))))
  (test-equal ":x :q [ :p \"v\" ] ."
    (pattern->condensed-string
     '(triple (: x)
              (: q)
              (bnode ((: p) (literal "v"))))))
  (test-equal "[ foaf:name ?name; foaf:mbox <mailto:alice@example.org> ]"
    (expression->condensed-string
     '(bnode ((: foaf "name") name)
             ((: foaf "mbox") "mailto:alice@example.org")))))

(define-test-case sparql-sexp-tests section-4.2 ()
  (test-equal "SELECT ?title WHERE { <http://example.org/book/book1> <http://purl.org/dc/elements/1.1/title> ?title . } "
    (sparql->condensed-string
     '(select (title) ()
        (triple "http://example.org/book/book1"
                "http://purl.org/dc/elements/1.1/title"
                title))))
  (test-equal "PREFIX dc: <http://purl.org/dc/elements/1.1/> PREFIX : <http://example.org/book/> SELECT ?title WHERE { :book1 dc:title ?title . } "
    (sparql->condensed-string
     '((prologue (prefix dc "http://purl.org/dc/elements/1.1/")
                 (prefix "http://example.org/book/"))
       (select (title) ()
         (triple (: "book1") (: dc "title") title)))))
  (test-equal "BASE <http://example.org/book/> PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?title WHERE { <book1> dc:title ?title . } "
    (sparql->condensed-string
     '((prologue (base "http://example.org/book/")
                 (prefix dc "http://purl.org/dc/elements/1.1/"))
       (select (title) ()
         (triple "book1" (: dc "title") title))))))

(define-test-case sparql-sexp-tests section-4.2.1 ()
  (test-equal "?x foaf:name ?name; foaf:mbox ?mbox ."
    (pattern->condensed-string
     '(triples x
               ((: foaf "name") name)
               ((: foaf "mbox") mbox))))
  (test-equal "?x foaf:name ?name . ?x foaf:mbox ?mbox . "
    (patterns->condensed-string
     '((triple x (: foaf "name") name)
       (triple x (: foaf "mbox") mbox)))))

(define-test-case sparql-sexp-tests section-4.2.2 ()
  (test-equal "?x foaf:nick \"Alice\", \"Alice_\" ."
    (pattern->condensed-string
     '(triple x (: foaf "nick") (literal "Alice") (literal "Alice_"))))
  (test-equal "?x foaf:nick \"Alice\" . ?x foaf:nick \"Alice_\" . "
    (patterns->condensed-string
     '((triple x (: foaf "nick") (literal "Alice"))
       (triple x (: foaf "nick") (literal "Alice_")))))
  (test-equal "?x foaf:name ?name; foaf:nick \"Alice\", \"Alice_\" ."
    (pattern->condensed-string
     '(triples x
               ((: foaf "name") name)
               ((: foaf "nick") (literal "Alice") (literal "Alice_")))))
  (test-equal "?x foaf:name ?name . ?x foaf:nick \"Alice\" . ?x foaf:nick \"Alice_\" . "
    (patterns->condensed-string
     '((triple x (: foaf "name") name)
       (triple x (: foaf "nick") (literal "Alice"))
       (triple x (: foaf "nick") (literal "Alice_"))))))

(define-test-case sparql-sexp-tests section-4.2.3 ()
  (test-equal "(1 ?x 3 4) :p \"w\" ."
    (pattern->condensed-string
     '(triple (collection 1 x 3 4) (: "p") (literal "w"))))
  (test-equal "(1 [ :p :q ] (2))"
    (expression->condensed-string
     '(collection 1 (bnode ((: "p") (: "q"))) (collection 2)))))

(define-test-case sparql-sexp-tests section-4.2.4 ()
  (test-equal "?x a :Class1 . [ a :appClass ] :p \"v\" . "
    (patterns->condensed-string
     '((triple x a (: "Class1"))
       (triple (bnode (a (: "appClass")))
               (: "p")
               (literal "v"))))))

(define-test-case sparql-sexp-tests section-5.2 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name ?mbox WHERE { ?x foaf:name ?name . ?x foaf:mbox ?mbox . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name mbox) ()
         (triple x (: foaf "name") name)
         (triple x (: foaf "mbox") mbox)))))
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name ?mbox WHERE { { ?x foaf:name ?name . } { ?x foaf:mbox ?mbox . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name mbox) ()
         (group (triple x (: foaf "name") name))
         (group (triple x (: foaf "mbox") mbox)))))))

(define-test-case sparql-sexp-tests section-5.2.1 ()
  (test-equal "{ } "
    (pattern->condensed-string '(group)))
  (test-equal "SELECT ?x WHERE { } "
    (sparql->condensed-string '(select (x) ()))))

(define-test-case sparql-sexp-tests section-5.2.2 ()
  (test-equal "{ ?x foaf:name ?name . ?x foaf:mbox ?mbox . FILTER REGEX (?name, \"Smith\") } "
    (pattern->condensed-string
     '(group (triple x (: foaf "name") name)
             (triple x (: foaf "mbox") mbox)
             (filter (regex name (literal "Smith"))))))
  (test-equal "{ FILTER REGEX (?name, \"Smith\") ?x foaf:name ?name . ?x foaf:mbox ?mbox . } "
    (pattern->condensed-string
     '(group (filter (regex name (literal "Smith")))
             (triple x (: foaf "name") name)
             (triple x (: foaf "mbox") mbox))))
  (test-equal "{ ?x foaf:name ?name . FILTER REGEX (?name, \"Smith\") ?x foaf:mbox ?mbox . } "
    (pattern->condensed-string
     '(group (triple x (: foaf "name") name)
             (filter (regex name (literal "Smith")))
             (triple x (: foaf "mbox") mbox)))))

(define-test-case sparql-sexp-tests section-5.2.3
  (test-equal "{ ?x foaf:name ?name . ?x foaf:mbox ?mbox . } "
    (pattern->condensed-string
     '(group (triple x (: foaf "name") name)
             (triple x (: foaf "mbox") mbox))))
  (test-equal "{ ?x foaf:name ?name . FILTER REGEX (?name, \"Smith\") ?x foaf:mbox ?mbox . } "
    (pattern->condensed-string
     '(group (triple x (: foaf "name") name)
             (filter (regex name (literal "Smith")))
             (triple x (: foaf "mbox") mbox))))
  (test-equal "{ ?x foaf:name ?name . { } ?x foaf:mbox ?mbox . } "
    (pattern->condensed-string
     '(group (triple x (: foaf "name") name)
             (group)
             (triple x (: foaf "mbox") mbox)))))

(define-test-case sparql-sexp-tests section-6.1 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name ?mbox WHERE { ?x foaf:name ?name . OPTIONAL { ?x foaf:mbox ?mbox . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name mbox) ()
         (triple x (: foaf "name") name)
         (optional (triple x (: foaf "mbox") mbox)))))))

(define-test-case sparql-sexp-tests section-6.2 ()
  (test-equal "PREFIX dc: <http://purl.org/dc/elements/1.1/> PREFIX ns: <http://example.org/ns#> SELECT ?title ?price WHERE { ?x dc:title ?title . OPTIONAL { ?x ns:price ?price . FILTER (?price < 30) } } "
    (sparql->condensed-string
     '((prologue (prefix dc "http://purl.org/dc/elements/1.1/")
                 (prefix ns "http://example.org/ns#"))
       (select (title price) ()
         (triple x (: dc "title") title)
         (optional (triple x (: ns "price") price)
                   (filter (< price 30))))))))

(define-test-case sparql-sexp-tests section-6.3 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name ?mbox ?hpage WHERE { ?x foaf:name ?name . OPTIONAL { ?x foaf:mbox ?mbox . } OPTIONAL { ?x foaf:homepage ?hpage . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name mbox hpage) ()
         (triple x (: foaf "name") name)
         (optional (triple x (: foaf "mbox") mbox))
         (optional (triple x (: foaf "homepage") hpage)))))))

(define-test-case sparql-sexp-tests section-7 ()
  (test-equal "PREFIX dc10: <http://purl.org/dc/elements/1.0/> PREFIX dc11: <http://purl.org/dc/elements/1.1/> SELECT ?x ?y WHERE { { ?book dc10:title ?x . } UNION { ?book dc11:title ?y . } } "
    (sparql->condensed-string
     '((prologue (prefix dc10 "http://purl.org/dc/elements/1.0/")
                 (prefix dc11 "http://purl.org/dc/elements/1.1/"))
       (select (x y) ()
         (union (triple book (: dc10 "title") x)
                (triple book (: dc11 "title") y)))))))

(define-test-case sparql-sexp-tests section-8.2.1 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name FROM <http://example.org/foaf/aliceFoaf> WHERE { ?x foaf:name ?name . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name) ((from "http://example.org/foaf/aliceFoaf"))
         (triple x (: foaf "name") name))))))

;;; This one does not actually appear in the technical report; it is
;;; constructed from the previous one.

(define-test-case sparql-sexp-tests section-8.2.2 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name FROM NAMED <http://example.org/alice> FROM NAMED <http://example.org/bob> WHERE { ?x foaf:name ?name . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name)
           ((from-named "http://example.org/alice")
            (from-named "http://example.org/bob"))
         (triple x (: foaf "name") name))))))

(define-test-case sparql-sexp-tests section-8.2.3 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?who ?g ?mbox FROM <http://example.org/dft.ttl> FROM NAMED <http://example.org/alice> FROM NAMED <http://example.org/bob> WHERE { ?g dc:publisher ?who . GRAPH ?g { ?x foaf:mbox ?mbox . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix dc "http://purl.org/dc/elements/1.1/"))
       (select (who g mbox)
           ((from "http://example.org/dft.ttl")
            (from-named "http://example.org/alice")
            (from-named "http://example.org/bob"))
         (triple g (: dc "publisher") who)
         (graph g
           (triple x (: foaf "mbox") mbox)))))))

(define-test-case sparql-sexp-tests section-8.3.1 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?src ?bobnick FROM NAMED <http://example.org/foaf/aliceFoaf> FROM NAMED <http://example.org/foaf/bobFoaf> WHERE { GRAPH ?src { ?x foaf:mbox <mailto:bob@work.example> . ?x foaf:nick ?bobnick . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (src bobnick)
           ((from-named "http://example.org/foaf/aliceFoaf")
            (from-named "http://example.org/foaf/bobFoaf"))
         (graph src
           (triple x (: foaf "mbox") "mailto:bob@work.example")
           (triple x (: foaf "nick") bobnick)))))))

(define-test-case sparql-sexp-tests section-8.3.2 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX data: <http://example.org/foaf/> SELECT ?nick FROM NAMED <http://example.org/foaf/aliceFoaf> FROM NAMED <http://example.org/foaf/bobFoaf> WHERE { GRAPH data:bobFoaf { ?x foaf:mbox <mailto:bob@work.example> . ?x foaf:nick ?nick . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix data "http://example.org/foaf/"))
       (select (nick)
           ((from-named "http://example.org/foaf/aliceFoaf")
            (from-named "http://example.org/foaf/bobFoaf"))
         (graph (: data "bobFoaf")
           (triple x (: foaf "mbox") "mailto:bob@work.example")
           (triple x (: foaf "nick") nick)))))))

(define-test-case sparql-sexp-tests section-8.3.3 ()
  (test-equal "PREFIX data: <http://example.org/foaf/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT ?mbox ?nick ?ppd FROM NAMED <http://example.org/foaf/aliceFoaf> FROM NAMED <http://example.org/foaf/bobFoaf> WHERE { GRAPH data:aliceFoaf { ?alice foaf:mbox <mailto:alice@work.example>; foaf:knows ?whom . ?whom foaf:mbox ?mbox; rdfs:seeAlso ?ppd . ?ppd a foaf:PersonalProfileDocument . } GRAPH ?ppd { ?w foaf:mbox ?mbox; foaf:nick ?nick . } } "
    (sparql->condensed-string
     '((prologue (prefix data "http://example.org/foaf/")
                 (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix rdfs "http://www.w3.org/2000/01/rdf-schema#"))
       (select (mbox nick ppd)
           ((from-named "http://example.org/foaf/aliceFoaf")
            (from-named "http://example.org/foaf/bobFoaf"))
         (graph (: data "aliceFoaf")
           (triples alice
                    ((: foaf "mbox") "mailto:alice@work.example")
                    ((: foaf "knows") whom))
           (triples whom
                    ((: foaf "mbox") mbox)
                    ((: rdfs "seeAlso") ppd))
           (triple ppd a (: foaf "PersonalProfileDocument")))
         (graph ppd
           (triples w
                    ((: foaf "mbox") mbox)
                    ((: foaf "nick") nick))))))))

(define-test-case sparql-sexp-tests section-8.3.4 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?name ?mbox ?date WHERE { ?g dc:publisher ?name; dc:date ?date . GRAPH ?g { ?person foaf:name ?name; foaf:mbox ?mbox . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix dc "http://purl.org/dc/elements/1.1/"))
       (select (name mbox date) ()
         (triples g
                  ((: dc "publisher") name)
                  ((: dc "date") date))
         (graph g
           (triples person
                    ((: foaf "name") name)
                    ((: foaf "mbox") mbox))))))))

(define-test-case sparql-sexp-tests section-9.1 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name WHERE { ?x foaf:name ?name . } ORDER BY ?name "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name) ((order-by name))
         (triple x (: foaf "name") name)))))
  (test-equal "PREFIX : <http://example.org/ns#> PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ?name WHERE { ?x foaf:name ?name; :empId ?emp . } ORDER BY DESC (?emp) "
    (sparql->condensed-string
     '((prologue (prefix "http://example.org/ns#")
                 (prefix foaf "http://xmlns.com/foaf/0.1/")
                 ;++ I don't think this is necessary, but I need to
                 ;++ clarify something in the specification -- the use
                 ;++ of the `xsd:' prefix as literal or implied
                 ;++ <http://www.w3.org/2001/XMLSchema#>.
                 (prefix xsd "http://www.w3.org/2001/XMLSchema#"))
       (select (name) ((order-by (desc emp)))
         (triples x
                  ((: foaf "name") name)
                  ((: "empId") emp))))))
  ;; This last one is not written correctly in the document: it omits
  ;; two prefix declarations.
  (test-equal "PREFIX : <http://example.org/ns#> PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ?name WHERE { ?x foaf:name ?name; :empId ?emp . } ORDER BY ?name DESC (?emp) "
    (sparql->condensed-string
     '((prologue (prefix "http://example.org/ns#")
                 (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix xsd "http://www.w3.org/2001/XMLSchema#"))
       (select (name) ((order-by name (desc emp)))
         (triples x
                  ((: foaf "name") name)
                  ((: "empId") emp)))))))

;;; Superfluous examples in 9.2 and 9.3 skipped.

(define-test-case sparql-sexp-tests section-9.3.1 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT DISTINCT ?name WHERE { ?x foaf:name ?name . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name) (distinct)
         (triple x (: foaf "name") name))))))

(define-test-case sparql-sexp-tests section-9.3.2 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT REDUCED ?name WHERE { ?x foaf:name ?name . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name) (reduced)
         (triple x (: foaf "name") name))))))

(define-test-case sparql-sexp-tests section-9.4 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name WHERE { ?x foaf:name ?name . } ORDER BY ?name LIMIT 5 OFFSET 10 "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name)
           ((order-by name)
            (limit 5)
            (offset 10))
         (triple x (: foaf "name") name))))))

(define-test-case sparql-sexp-tests section-9.5 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name WHERE { ?x foaf:name ?name . } LIMIT 20 "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (name) ((limit 20))
         (triple x (: foaf "name") name))))))

(define-test-case sparql-sexp-tests section-10.1 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?namex ?namey ?nicky WHERE { ?x foaf:knows ?y; foaf:name ?namex . ?y foaf:name ?namey . OPTIONAL { ?y foaf:nick ?nicky . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (select (namex namey nicky) ()
         (triples x
                  ((: foaf "knows") y)
                  ((: foaf "name") namex))
         (triple y (: foaf "name") namey)
         (optional (triple y (: foaf "nick") nicky)))))))

(define-test-case sparql-sexp-tests section-10.2 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX vcard: <http://www.w3.org/2001/vcard-rdf/3.0#> CONSTRUCT { <http://example.org/person#Alice> vcard:PN ?name . } WHERE { ?x foaf:name ?name . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix vcard "http://www.w3.org/2001/vcard-rdf/3.0#"))
       (construct ()
           ((triple "http://example.org/person#Alice"
                    (: vcard "PN")
                    name))
         (triple x (: foaf "name") name))))))

(define-test-case sparql-sexp-tests section-10.2.1 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX vcard: <http://www.w3.org/2001/vcard-rdf/3.0#> CONSTRUCT { ?x vcard:N _:v . _:v vcard:givenName ?gname . _:v vcard:familyName ?fname . } WHERE { { ?x foaf:firstname ?gname . } UNION { ?x foaf:givenname ?gname . } { ?x foaf:surname ?fname . } UNION { ?x foaf:family_name ?fname . } } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix vcard "http://www.w3.org/2001/vcard-rdf/3.0#"))
       (construct ()
           ((triple x (: vcard "N") (_ "v"))
            (triple (_ "v") (: vcard "givenName") gname)
            (triple (_ "v") (: vcard "familyName") fname))
         (union (triple x (: foaf "firstname") gname)
                (triple x (: foaf "givenname") gname))
         (union (triple x (: foaf "surname") fname)
                (triple x (: foaf "family_name") fname)))))))

(define-test-case sparql-sexp-tests section-10.2.2 ()
  (test-equal "CONSTRUCT { ?s ?p ?o . } WHERE { GRAPH <http://example.org/aGraph> { ?s ?p ?o . } } "
    (sparql->condensed-string
     '(construct () ((triple s p o))
        (graph "http://example.org/aGraph"
          (triple s p o)))))
  ;; This example in the document omitted the `xsd:' prefix.
  (test-equal "PREFIX dc: <http://purl.org/dc/elements/1.1/> PREFIX app: <http://example.org/ns#> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> CONSTRUCT { ?s ?p ?o . } WHERE { GRAPH ?g { ?s ?p ?o . } { ?g dc:publisher <http://www.w3.org/> . } { ?g dc:date ?date . } FILTER (app:customDate (?date) > \"2005-02-28T00:00:00Z\"^^xsd:dateTime) } "
    (sparql->condensed-string
     '((prologue (prefix dc "http://purl.org/dc/elements/1.1/")
                 (prefix app "http://example.org/ns#")
                 (prefix xsd "http://www.w3.org/2001/XMLSchema#"))
       (construct ()
           ((triple s p o))
         (graph g (triple s p o))
         (group (triple g (: dc "publisher") "http://www.w3.org/"))
         (group (triple g (: dc "date") date))
         (filter (> ((: app "customDate") date)
                    (literal (typed (: xsd "dateTime"))
                             "2005-02-28T00:00:00Z"))))))))

(define-test-case sparql-sexp-tests section-10.2.3 ()
  ;++ The formatter abstraction needs to be able to determine whether
  ;++ any output was added in order to change the `[ ]' ugliness into
  ;++ `[]'.
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX site: <http://example.org/stats#> CONSTRUCT { [  ] foaf:name ?name . } WHERE { [  ] foaf:name ?name; site:hits ?hits . } ORDER BY DESC (?hits) LIMIT 2 "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/")
                 (prefix site "http://example.org/stats#"))
       (construct ((order-by (desc hits))
                   (limit 2))
           ((triple (bnode) (: foaf "name") name))
         (triples (bnode)
                  ((: foaf "name") name)
                  ((: site "hits") hits)))))))

(define-test-case sparql-sexp-tests section-10.3 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> ASK { ?x foaf:name \"Alice\" . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (ask ()
         (triple x (: foaf "name") (literal "Alice"))))))
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> ASK { ?x foaf:name \"Alice\"; foaf:mbox <mailto:alice@work.example> . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (ask ()
         (triples x
                  ((: foaf "name") (literal "Alice"))
                  ((: foaf "mbox") "mailto:alice@work.example")))))))

(define-test-case sparql-sexp-tests section-10.4.1 ()
  (test-equal "DESCRIBE <http://example.org/> "
    (sparql->condensed-string
     '(describe "http://example.org/" ()))))

(define-test-case sparql-sexp-tests section-10.4.2 ()
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> DESCRIBE ?x WHERE { ?x foaf:mbox <mailto:alice@org> . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (describe x ()
         (triple x (: foaf "mbox") "mailto:alice@org")))))
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> DESCRIBE ?x WHERE { ?x foaf:name \"Alice\" . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (describe x ()
         (triple x (: foaf "name") (literal "Alice"))))))
  (test-equal "PREFIX foaf: <http://xmlns.com/foaf/0.1/> DESCRIBE ?x ?y WHERE { ?x foaf:knows ?y . } "
    (sparql->condensed-string
     '((prologue (prefix foaf "http://xmlns.com/foaf/0.1/"))
       (describe (x y) ()
         (triple x (: foaf "knows") y))))))

;;; I give up!
