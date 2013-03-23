;;; -*- Mode: Scheme; scheme48-package: rdf-list-graphs -*-

;;;; Schemantic Web
;;;; RDF Graphs as Triple Lists

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <rdf-graph>
    (%make-rdf-graph size triples)
    rdf-graph?
  (size rdf-graph/size set-rdf-graph/size!)
  (triples rdf-graph/triples set-rdf-graph/triples!))

(define (make-rdf-graph)
  (%make-rdf-graph 0 '()))

(define (rdf-graph/add-triple! graph triple)
  (let ((triples (rdf-graph/triples graph)))
    (if (not (member triple triples rdf-triple=?))
        (begin
          (set-rdf-graph/triples! graph (cons triple triples))
          (set-rdf-graph/size! graph (+ 1 (rdf-graph/size graph)))))))

(define (rdf-graph/remove-triple! graph triple)
  (let ((triples (rdf-graph/triples graph)))
    (if (member triple triples rdf-triple=?)
        (begin
          (set-rdf-graph/triples! graph (delete! triple triples rdf-triple=?))
          (set-rdf-graph/size! graph (+ -1 (rdf-graph/size graph)))))))

(define (rdf-graph/purge-matching-objects! graph subject predicate)
  (%rdf-graph/purge-matching! graph
                              subject   rdf-triple/subject      rdf-subject=?
                              predicate rdf-triple/predicate
                              rdf-predicate=?))

(define (rdf-graph/purge-matching-predicates! graph subject object)
  (%rdf-graph/purge-matching! graph
                              subject   rdf-triple/subject      rdf-subject=?
                              object    rdf-triple/object       rdf-object=?))

(define (rdf-graph/purge-matching-subjects! graph predicate object)
  (%rdf-graph/purge-matching! graph
                              predicate rdf-triple/predicate    rdf-predicate=?
                              object    rdf-triple/object       rdf-object=?))

(define (%rdf-graph/purge-matching! graph a triple/a a=? b triple/b b=?)
  (let ((size (rdf-graph/size graph)))
    (set-rdf-graph/triples!
     graph
     (remove! (lambda (triple)
                (and (a=? a (triple/a triple))
                     (b=? b (triple/b triple))
                     (begin (set! size (- size 1)) #t)))
              (rdf-graph/triples graph)))
    (set-rdf-graph/size! graph size)))

(define (rdf-graph/purge-triples-by-subject! graph subject)
  (%rdf-graph/purge-by! graph subject rdf-triple/subject rdf-subject=?))

(define (rdf-graph/purge-triples-by-predicate! graph predicate)
  (%rdf-graph/purge-by! graph predicate rdf-triple/predicate rdf-predicate=?))

(define (rdf-graph/purge-triples-by-object! graph object)
  (%rdf-graph/purge-by! graph object rdf-triple/object rdf-object=?))

(define (%rdf-graph/purge-by! graph key triple/key key=?)
  (let ((size (rdf-graph/size graph)))
    (set-rdf-graph/triples!
     graph
     (remove! (lambda (triple)
                (and (key=? key (triple/key triple))
                     (begin (set! size (- size 1)) #t)))
              (rdf-graph/triples graph)))
    (set-rdf-graph/size! graph size)))

(define (rdf-graph/for-each-triple graph procedure)
  (for-each procedure (rdf-graph/triples graph)))

(define (rdf-graph/all-triples graph)
  (list-copy (rdf-graph/triples graph)))

(define (rdf-graph/for-each-subject graph procedure)
  (for-each procedure (rdf-graph/all-subjects graph)))

(define (rdf-graph/for-each-predicate graph procedure)
  (for-each procedure (rdf-graph/all-predicates graph)))

(define (rdf-graph/for-each-object graph procedure)
  (for-each procedure (rdf-graph/all-objects graph)))

(define (rdf-graph/all-subjects graph)
  (delete-duplicates! (map rdf-triple/subject (rdf-graph/triples graph))
                      rdf-subject=?))

(define (rdf-graph/all-predicates graph)
  (delete-duplicates! (map rdf-triple/predicate (rdf-graph/triples graph))
                      rdf-predicate=?))

(define (rdf-graph/all-objects graph)
  (delete-duplicates! (map rdf-triple/object (rdf-graph/triples graph))
                      rdf-object=?))

(define (rdf-graph/for-each-triple-by-subject graph subject procedure)
  (for-each (lambda (triple)
              (if (rdf-subject=? subject (rdf-triple/subject triple))
                  (procedure triple)))
            (rdf-graph/triples graph)))

(define (rdf-graph/for-each-triple-by-predicate graph predicate procedure)
  (for-each (lambda (triple)
              (if (rdf-predicate=? predicate (rdf-triple/predicate triple))
                  (procedure triple)))
            (rdf-graph/triples graph)))

(define (rdf-graph/for-each-triple-by-object graph object procedure)
  (for-each (lambda (triple)
              (if (rdf-object=? object (rdf-triple/object triple))
                  (procedure triple)))
            (rdf-graph/triples graph)))

(define (rdf-graph/triples-by-subject graph subject)
  (filter (lambda (triple)
            (rdf-subject=? subject (rdf-triple/subject triple)))
          (rdf-graph/triples graph)))

(define (rdf-graph/triples-by-predicate graph predicate)
  (filter (lambda (triple)
            (rdf-predicate=? predicate (rdf-triple/predicate triple)))
          (rdf-graph/triples graph)))

(define (rdf-graph/triples-by-object graph object)
  (filter (lambda (triple)
            (rdf-object=? object (rdf-triple/object triple)))
          (rdf-graph/triples graph)))

(define (rdf-graph/for-each-matching-object graph subject predicate procedure)
  (for-each procedure
            (rdf-graph/matching-objects graph subject predicate)))

(define (rdf-graph/for-each-matching-predicate graph subject object procedure)
  (for-each procedure
            (rdf-graph/matching-objects graph subject object)))

(define (rdf-graph/for-each-matching-subject graph predicate object procedure)
  (for-each procedure
            (rdf-graph/matching-subjects graph predicate object)))

(define (rdf-graph/matching-objects graph subject predicate)
  (%rdf-graph/match graph       rdf-triple/object       rdf-object=?
                    subject     rdf-triple/subject      rdf-subject=?
                    predicate   rdf-triple/predicate    rdf-predicate=?))

(define (rdf-graph/matching-predicates graph subject object)
  (%rdf-graph/match graph       rdf-triple/predicate    rdf-predicate=?
                    subject     rdf-triple/subject      rdf-subject=?
                    object      rdf-triple/object       rdf-object=?))

(define (rdf-graph/matching-subjects graph predicate object)
  (%rdf-graph/match graph       rdf-triple/subject      rdf-subject=?
                    predicate   rdf-triple/predicate    rdf-predicate=?
                    object      rdf-triple/object       rdf-object=?))

(define (%rdf-graph/match graph triple/c c=? a triple/a a=? b triple/b b=?)
  (delete-duplicates!
   (filter-map (lambda (triple)
                 (and (a=? a (triple/a triple))
                      (b=? b (triple/b triple))
                      (triple/c triple)))
               (rdf-graph/triples graph))
   c=?))
