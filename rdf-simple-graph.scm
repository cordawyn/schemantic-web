;;; -*- Mode: Scheme; scheme48-package: rdf-simple-graphs -*-

;;;; Schemantic Web
;;;; Simple RDF Graph Implementation

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; An RDF graph here is represented as a pair of indices, of subjects
;;; and of objects.  The subject index maps subject nodes (RDF URI
;;; references or blank bodes) to predicate indices, which in turn map
;;; predicate URI references to the respective object nodes (RDF URI
;;; references, blank nodes, or literals).  The object index does
;;; similarly, in the reverse direction.  Whether it ought to include
;;; or whether it ought to exclude RDF literals is unclear.
;;;
;;; Because many operations are meaningful both in the direction of
;;; subject -> object and in its reverse, we use the terminology `key'
;;; for the resource that we are starting out with and `datum' for the
;;; resource we are looking for.  For example, if we have a subject and
;;; a predicate, and we are looking for all the related objects, then
;;; keys are subjects and data are objects; however, if we have an
;;; object and a predicate, and we seek all related subjects, then keys
;;; are objects and data are subjects.  This allows many procedures
;;; that would otherwise be duplicated to be abstracted concisely.

(define-record-type <rdf-graph>
    (%make-rdf-graph size subject-index object-index)
    rdf-graph?
  (size rdf-graph/size set-rdf-graph/size!)
  (subject-index rdf-graph/subject-index)
  (object-index rdf-graph/object-index))

(define (make-rdf-graph)
  (%make-rdf-graph 0
                   (make-rdf-subject-map)
                   (make-rdf-object-map)))

(define (rdf-graph/add-triple! graph triple)
  (let ((subject (rdf-triple/subject triple))
        (predicate (rdf-triple/predicate triple))
        (object (rdf-triple/object triple)))
    (rdf-predicate-map/modify!
        (rdf-subject-map/intern! (rdf-graph/subject-index graph) subject
          make-rdf-predicate-map)
        predicate
        '()                             ;Default initial value
      (lambda (objects)
        (cons object objects)))
    (rdf-predicate-map/modify!
        (rdf-object-map/intern! (rdf-graph/object-index graph) object
          make-rdf-predicate-map)
        predicate
        '()                             ;Default initial value
      (lambda (subjects)
        (cons subject subjects))))
  (rdf-graph/resize! graph +1))

(define (rdf-graph/resize! graph adjustment)
  (set-rdf-graph/size! graph (+ adjustment (rdf-graph/size graph))))

(define (rdf-graph/for-each-triple graph procedure)
  (let ((subject-index (rdf-graph/subject-index graph)))
    (iterate!
        (for subject predicate-index (in-rdf-subject-map subject-index))
         (for predicate objects (in-rdf-predicate-map predicate-index))
          (for object (in-list objects))
      (procedure subject predicate object))))

(define (rdf-graph/all-triples graph)
  (let ((subject-index (rdf-graph/subject-index graph)))
    (collect-list
        (for subject predicate-index (in-rdf-subject-map subject-index))
         (for predicate objects (in-rdf-predicate-map predicate-index))
          (for object (in-list objects))
      object)))

;;;; Component Listing

(define (rdf-graph/all-subjects graph)
  (%rdf-graph/all-keys graph rdf-graph/subject-index rdf-subject-map/walk))

(define (rdf-graph/all-objects graph)
  (%rdf-graph/all-keys graph rdf-graph/object-index rdf-object-map/walk))

(define (%rdf-graph/all-keys graph index walk)
  (let ((keys '()))
    (walk (index graph)
          (lambda (key predicate-index)
            predicate-index             ;ignore
            (set! keys (cons key keys))))
    keys))

(define (rdf-graph/for-each-subject graph procedure)
  (%rdf-graph/for-each-key graph procedure
                           rdf-graph/subject-index rdf-subject-map/walk))

(define (rdf-graph/for-each-object graph procedure)
  (%rdf-graph/for-each-key graph procedure
                           rdf-graph/object-index rdf-object-map/walk))

(define (%rdf-graph/for-each-key graph procedure index walk)
  (walk (index graph)
        (lambda (key predicate-index)
          predicate-index               ;ignore
          (procedure key))))

(define (rdf-graph/all-predicates graph)
  (rdf-predicate-map/key-list
   (%rdf-graph/all-predicates graph)))

(define (rdf-graph/for-each-predicate graph procedure)
  (rdf-predicate-map/walk (%rdf-graph/all-predicates graph)
    (lambda (predicate flag)
      flag                              ;ignore
      (procedure predicate))))

(define (%rdf-graph/all-predicates graph)
  (let ((predicate-set (make-rdf-predicate-map))
        (subject-index (rdf-graph/subject-index graph)))
    (iterate!
        (for subject predicate-index (in-rdf-subject-map subject-index))
         (for predicate objects (in-rdf-predicate-map predicate-index))
      (rdf-predicate-map/insert! predicate-set predicate #t))
    predicate-set))

;;;; Removing and Purging Triples

;;; `Removing' is of a single triple; `purging' is of a class of
;;; triples.

(define (rdf-graph/remove-triple! graph triple)
  (let ((subject (rdf-triple/subject triple))
        (predicate (rdf-triple/predicate triple))
        (object (rdf-triple/object triple)))
    (%rdf-graph/remove-triple! graph subject predicate object
                               rdf-graph/subject-index
                               rdf-subject-map/lookup
                               rdf-object=?)
    (%rdf-graph/remove-triple! graph object predicate subject
                               rdf-graph/object-index
                               rdf-object-map/lookup
                               rdf-subject=?))
  (rdf-graph/resize! graph -1))

(define (%rdf-graph/remove-triple! graph key predicate datum index lookup
                                   datum=?)
  (cond ((lookup (index graph) key #f)
         => (lambda (predicate-index)
              (rdf-predicate-map/update! predicate-index predicate
                (lambda (data replace delete)
                  delete                ;ignore
                  (if (member datum data datum=?)
                      (replace (delete! datum data datum=?))))
                (lambda (insert)
                  insert                ;ignore
                  (values)))))))

(define (rdf-graph/purge-matching-objects! graph subject predicate)
  (%rdf-graph/purge-matching! graph subject predicate
                              rdf-graph/subject-index rdf-subject-map/update!
                              rdf-graph/object-index rdf-object-map/lookup
                              rdf-subject=?))

(define (rdf-graph/purge-matching-subjects! graph predicate object)
  (%rdf-graph/purge-matching! graph object predicate
                              rdf-graph/object-index rdf-object-map/update!
                              rdf-graph/subject-index rdf-subject-map/lookup
                              rdf-object=?))

(define (%rdf-graph/purge-matching! graph key predicate
                                    key-index update-key!
                                    datum-index lookup-datum
                                    datum=?)
  (update-key! (key-index graph) key
    (lambda (predicate-index replace delete)
      replace                           ;ignore
      (rdf-predicate-map/update! predicate-index predicate
        (lambda (data replace delete)
          replace                       ;ignore
          (delete)
          (for-each (lambda (datum)
                      (%rdf-graph/remove-triple! graph datum predicate key
                                                 datum-index lookup-datum
                                                 datum=?)
                      (rdf-graph/resize! graph -1))
                    data))
        (lambda (insert)
          insert                        ;ignore
          (values)))
      (if (zero? (rdf-predicate-map/size predicate-index))
          (delete)))
    (lambda (insert)
      insert                            ;ignore
      (values))))

;; (define (rdf-graph/purge-predicates! graph subject object) ...)

;;;; Querying Graphs

(define (rdf-graph/for-each-triple-by-subject graph subject procedure)
  (%rdf-graph/for-each-triple-by-key graph subject procedure
                                     rdf-graph/subject-index
                                     rdf-subject-map/search
                                     make-rdf-triple))

(define (rdf-graph/for-each-triple-by-object graph object procedure)
  (%rdf-graph/for-each-triple-by-key graph object procedure
                                     rdf-graph/object-index
                                     rdf-object-map/search
                                     make-rdf-elpirt))

(define (make-rdf-elpirt object predicate subject)
  (make-rdf-triple subject predicate object))

(define (%rdf-graph/for-each-triple-by-key graph key procedure
                                           index
                                           search
                                           triple-constructor)
  (search (index graph) key
    (lambda (predicate-index)
      (iterate!
          (for predicate data (in-rdf-predicate-map predicate-index))
           (for datum (in-list data))
        (procedure (triple-constructor key predicate datum))))
    (lambda ()
      '())))

(define (rdf-graph/for-each-triple-by-predicate graph predicate procedure)
  (let ((subject-index (rdf-graph/subject-index graph)))
    (iterate!
        (for subject predicate-index (in-rdf-subject-map subject-index))
         (for object
              (in-list
               (rdf-predicate-map/lookup predicate-index predicate '())))
      (procedure (make-rdf-triple subject predicate object)))))

(define (rdf-graph/triples-by-subject graph subject)
  (%rdf-graph/triples-by-key graph subject
                             rdf-graph/for-each-triple-by-subject))

(define (rdf-graph/triples-by-predicate graph predicate)
  (%rdf-graph/triples-by-key graph predicate
                             rdf-graph/for-each-triple-by-predicate))

(define (rdf-graph/triples-by-object graph object)
  (%rdf-graph/triples-by-key graph object
                             rdf-graph/for-each-triple-by-object))

(define (%rdf-graph/triples-by-key graph key iterator)
  (let ((triples '()))
    (iterator graph key
      (lambda (triple)
        (set! triples (cons triple triples))))
    triples))

(define (rdf-graph/for-each-matching-object graph subject predicate procedure)
  (%rdf-graph/for-each-matching-datum graph subject predicate procedure
                                      rdf-graph/subject-index
                                      rdf-subject-map/search))

(define (rdf-graph/for-each-matching-subject graph predicate object procedure)
  (%rdf-graph/for-each-matching-datum graph object predicate procedure
                                      rdf-graph/object-index
                                      rdf-object-map/search))

(define (%rdf-graph/for-each-matching-datum graph key predicate procedure
                                            index search)
  (search (index graph) key
    (lambda (predicate-index)
      (for-each procedure
                (rdf-predicate-map/lookup predicate-index predicate '())))
    (lambda ()
      (values))))

(define (rdf-graph/for-each-matching-predicate graph subject object procedure)
  (rdf-subject-map/search (rdf-graph/subject-index graph) subject
    (lambda (predicate-index)
      (iterate!
          (for predicate objects (in-rdf-predicate-map predicate-index))
           (if (member object objects rdf-object=?))
        (procedure predicate)))
    (lambda ()
      (values))))

(define (rdf-graph/matching-objects graph subject predicate)
  (%rdf-graph/matching-data graph subject predicate
                            rdf-graph/for-each-matching-object))

(define (rdf-graph/matching-predicates graph subject object)
  (%rdf-graph/matching-data graph subject object
                            rdf-graph/for-each-matching-predicate))

(define (rdf-graph/matching-subjects graph object predicate)
  (%rdf-graph/matching-data graph object predicate
                            rdf-graph/for-each-matching-subject))

(define (%rdf-graph/matching-data graph a b iterator)
  (let ((data '()))
    (iterator graph a b (lambda (datum) (set! data (cons datum data))))
    data))
