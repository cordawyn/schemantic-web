;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Auxiliary Utilities for the Schemantic Web
;;;; Package Definitions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-structure foof-loop foof-loop-interface
  (open scheme
        extended-parameter-operators
        laziness
        )
  (for-syntax
   (open scheme
         simple-signals
         (subset nodes (schemify))))
  (begin
    (define-syntax syntactic-error
      (lambda (form rename compare)
        (apply syntax-error (map schemify (cdr form))))
      ()))        ;No auxiliary names
  (files let-values foof-loop))

(define-structure nested-foof-loop nested-foof-loop-interface
  (open scheme
        foof-loop
        extended-parameter-operators
        receiving
        (subset srfi-1 (append-reverse))
        srfi-6                          ;Basic String Ports
        lazy-streams
        )
  (files nested-foof-loop))

(define-structure extended-parameter-operators
    extended-parameter-operators-interface
  (open scheme)
  (files syn-param))

(define-structures
    ((format-combinators format-combinators-interface)
     (format-driver format-driver-interface)
     (format-state format-state-interface))
  (open scheme
        receiving
        srfi-6                          ;Basic String Ports
        srfi-9                          ;define-record-type
        srfi-23                         ;error
        (subset i/o (write-string))
        )
  (optimize auto-integrate)

  (begin
    (define (char-break? char)
      (or (char=? char #\space)
          (char=? char #\newline)))

    (define (char-line-break? char)
      (char=? char #\newline))

    (define (string-search-line-break string start end)
      (and (< start end)
           (if (char-line-break? (string-ref string start))
               (+ start 1)
               (string-search-line-break string (+ start 1) end)))))

  (files format))

(define-structure pattern-matching pattern-matching-interface
  (open scheme srfi-23)
  (files match))
