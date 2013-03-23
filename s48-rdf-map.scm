;;; -*- Mode: Scheme; scheme48-package: rdf-maps-internal -*-

;;;; Schemantic Web
;;;; Maps Keyed by RDF Resources

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (map/lookup map key default)
  (cond ((table-ref map key) => decanonicalize-false)
        (else default)))

(define (map/search map key if-found if-not-found)
  (cond ((table-ref map key)
         => (lambda (datum)
              (if-found (decanonicalize-false datum))))
        (else (if-not-found))))

(define (map/insert! map key datum)
  (table-set! map key (canonicalize-false datum)))

(define (map/delete! map key)
  (table-set! map key #f))

(define (map/update! map key if-found if-not-found)
  (cond ((table-ref map key)
         => (lambda (datum)
              (if-found (decanonicalize-false datum)
                        (lambda (datum*) ;replace
                          (table-set! map key (canonicalize-false datum*)))
                        (lambda ()       ;delete
                          (table-set! map key #f)))))
        (else
         (if-not-found (lambda (datum)   ;insert
                         (table-set! map key (canonicalize-false datum)))))))

(define (map/modify! map key default modifier)
  (table-set! map
              key
              (modifier (cond ((table-ref map key) => decanonicalize-false)
                              (else default)))))

(define (map/intern! map key generator)
  (cond ((table-ref map key) => decanonicalize-false)
        (else
         (let ((datum (generator)))
           (table-set! map key (canonicalize-false datum))
           datum))))

(define (map/size map)
  ;; (collect-count (for key datum (in-map map)))
  (let ((size 0))
    (table-walk (lambda (key datum)
                  key datum             ;ignore
                  (set! size (+ size 1)))
                map)
    size))

;;;; Iteration / Listing / Specialized Maps

(define (map/walk map procedure)
  ;; (loop ((for key datum (in-map map)))
  ;;   (procedure key datum))
  (table-walk procedure map))

(define-syntax in-map
  (syntax-rules ()
    ((IN-MAP (key-variable datum-variable) (map-expression) next . rest)
     (next (((ALIST)                       ;Outer bindings
             (MAP->ALIST map-expression)))
           ((ALIST ALIST (CDR ALIST)))     ;Loop variables
           ()                              ;Entry bindings
           ((NOT (PAIR? ALIST)))           ;Termination conditions
           (((key-variable datum-variable) ;Body bindings
             (LET ((ENTRY (CAR ALIST)))
               (VALUES (CAR ENTRY)
                       (CDR ENTRY)))))
           ()                              ;Final bindings
           . rest))))

(define (map/key-list map)
  (let ((keys '()))
    (map/walk map (lambda (key datum) datum (set! keys (cons key keys))))
    keys))

(define (map/datum-list map)
  (let ((data '()))
    (map/walk map (lambda (key datum) key (set! data (cons datum data))))
    data))

(define (map->alist map)
  (let ((alist '()))
    (map/walk map
      (lambda (key datum)
        (set! alist (cons (cons key datum) alist))))
    alist))

(define make-rdf-subject-map
  (make-table-maker rdf-subject=? rdf-subject-hash))

(define make-rdf-predicate-map
  (make-table-maker rdf-predicate=? rdf-predicate-hash))

(define make-rdf-object-map
  (make-table-maker rdf-object=? rdf-object-hash))

;;;; False Canonicalization

(define canonicalize-false)
(define decanonicalize-false)

(let ((false-token (cons 'FALSE '())))
  (set! canonicalize-false
        (lambda (datum)
          (if (not datum)
              false-token
              datum)))
  (set! decanonicalize-false
        (lambda (datum)
          (if (eq? datum false-token)
              #f
              datum))))
