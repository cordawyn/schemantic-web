;;; -*- Mode: Scheme; scheme48-package: rdf-turtle-parser-tests -*-

;;;; Schemantic Web
;;;; RDF Turtle Parser Tests

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-test-suite rdf-turtle-parser-tests
  "RDF Turtle Parser")

(define (test-turtle-file name loser)
  (parse-file turtle-parser:document
              (string-append "tests/rdf-turtle/" name ".ttl")
              (make-turtle-parser-context
               (lambda (triple graph)
                 triple graph           ;ignore
                 'DUMMY-RDF-GRAPH)
               'DUMMY-RDF-GRAPH)
              (lambda (graph context stream)
                context stream          ;ignore
                graph)
              (lambda (perror context stream)
                context stream          ;ignore
                (loser perror))))

(define-syntax define-good-test
  (syntax-rules ()
    ((DEFINE-GOOD-TEST name)
     (DEFINE-TEST-CASE RDF-TURTLE-PARSER-TESTS name ()
       (TEST-TURTLE-FILE name
                         (LAMBDA (PERROR)
                           (APPLY ERROR
                                  "Parse error:"
                                  (PARSE-ERROR/POSITION PERROR)
                                  (PARSE-ERROR/MESSAGES PERROR))))))))

(define-good-test "test-00")
(define-good-test "test-01")
(define-good-test "test-02")
(define-good-test "test-03")
(define-good-test "test-04")
(define-good-test "test-05")
(define-good-test "test-06")
(define-good-test "test-07")
(define-good-test "test-08")
(define-good-test "test-09")
(define-good-test "test-10")
(define-good-test "test-11")
(define-good-test "test-12")
(define-good-test "test-13")
(define-good-test "test-14")
(define-good-test "test-15")
(define-good-test "test-16")
(define-good-test "test-17")
(define-good-test "test-18")
(define-good-test "test-19")
(define-good-test "test-20")
(define-good-test "test-21")
(define-good-test "test-22")
(define-good-test "test-23")
(define-good-test "test-24")
(define-good-test "test-25")

;;; The bad tests should also check the position of the error.

(define-syntax define-bad-test
  (syntax-rules ()
    ((DEFINE-BAD-TEST name)
     (DEFINE-TEST-CASE RDF-TURTLE-PARSER-TESTS name ()
       (TEST-PREDICATE PARSE-ERROR?
                       (TEST-TURTLE-FILE name
                           (LAMBDA (PERROR) PERROR)))))))

(define-bad-test "bad-00")
(define-bad-test "bad-01")
(define-bad-test "bad-02")
(define-bad-test "bad-03")
(define-bad-test "bad-04")
(define-bad-test "bad-05")
(define-bad-test "bad-06")
(define-bad-test "bad-07")
(define-bad-test "bad-08")
(define-bad-test "bad-09")
(define-bad-test "bad-10")
(define-bad-test "bad-11")
(define-bad-test "bad-12")
(define-bad-test "bad-13")
(define-bad-test "bad-14")
