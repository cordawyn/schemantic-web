;;; -*- Mode: Scheme; scheme48-package: rdf-nt-parser-tests -*-

;;;; Schemantic Web
;;;; RDF N-Triples Parser Tests

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-test-suite rdf-nt-parser-tests
  "RDF N-Triples Parser")

;;; Utility for generating lists of triples to compare against.

(define (sexp->triples sexp)
  (map (lambda (triple)
         (let ((subject (car triple))
               (predicate (cadr triple))
               (object (caddr triple)))
           (make-rdf-triple
            (cond ((and (pair? subject) (eq? (car subject) 'BNODE))
                   (make-rdf-bnode (cadr subject)))
                  ((string? subject)
                   (string->rdf-uri-ref subject))
                  (else (error "Illegal subject:" subject)))
            (if (string? predicate)
                (string->rdf-uri-ref predicate)
                (error "Illegal predicate:" predicate))
            (cond ((pair? object)
                   (case (car object)
                     ((BNODE)
                      (make-rdf-bnode (cadr object)))
                     ((TYPED-LITERAL)
                      (make-rdf-typed-literal (cadr object)
                                              (string->rdf-uri-ref
                                               (caddr object))))
                     ((PLAIN-LITERAL)
                      (make-rdf-plain-literal (cadr object)
                                              (caddr object)))
                     (else (error "Illegal object:" object))))))))
       sexp))

(define-test-case rdf-nt-parser-tests 0 ()
  ;; This should actually check that we got the right triples, but
  ;; that's too much work right now.
  (parse-file nt-parser:document
              "tests/rdf-nt/test.nt"
              (make-nt-parser-context
               (lambda (triple graph)
                 triple graph           ;ignore
                 'DUMMY-RDF-GRAPH)
               'DUMMY-RDF-GRAPH)
              (lambda (graph context stream)
                context stream          ;ignore
                graph)
              (lambda (perror context stream)
                context stream          ;ignore
                (apply error
                       "Parse error:"
                       (parse-error/position perror)
                       (parse-error/messages perror)))))
