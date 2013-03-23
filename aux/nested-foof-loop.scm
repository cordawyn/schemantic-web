;;; -*- Mode: Scheme -*-

;;;; Nested Iteration and Recursion, Version 9 (BETA)

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-syntax iterate*
  (syntax-rules (PARALLEL NESTED DO LET LET-VALUES IF NOT AND OR)
    ((ITERATE* ((state initial) ...) stepper
       expression)
     (LET ((state initial) ...)
       (stepper (LAMBDA () expression))))

    ((ITERATE* ((state initial) ...) stepper
         (PARALLEL (iterator ...) ...)
         clause0 clause1+ ...)
     (LOOP ITER ((WITH state initial)
                 ...
                 (iterator ...)
                 ...)
       => (VALUES* state ...)
       (RECEIVE (state ...)
                (ITERATE* ((state state) ...) stepper
                    clause0 clause1+ ...)
         (ITER state ...))))

    ((ITERATE* ((state initial) ...) stepper
         (NESTED clause ...)
         clause0 clause1+ ...)
     (ITERATE* ((state initial) ...) stepper
         clause ... clause0 clause1+ ...))

    ((ITERATE* ((state initial) ...) stepper
         (DO command ...)
         clause0 clause1+ ...)
     (BEGIN command ...
            (ITERATE* ((state initial) ...) stepper
                clause0 clause1+ ...)))

    ((ITERATE* ((state initial) ...) stepper
         (LET ((variable value) ...))
         clause0 clause1+ ...)
     (LET ((variable value) ...)
       (ITERATE* ((state initial) ...) stepper
           clause0 clause1+ ...)))

    ((ITERATE* ((state initial) ...) stepper
         (LET variable value)
         clause0 clause1+ ...)
     (LET ((variable value))
       (ITERATE* ((state initial) ...) stepper
           clause0 clause1+ ...)))

    ((ITERATE* ((state initial) ...) stepper
         (LET-VALUES ((bvl expression) ...))
         clause0 clause1+ ...)
     (LET-VALUES ((bvl expression) ...)
       (ITERATE* ((state initial) ...) stepper
           clause0 clause1+ ...)))

    ((ITERATE* ((state initial) ...) stepper
         (LET-VALUES bvl expression)
         clause0 clause1+ ...)
     (RECEIVE bvl expression
       (ITERATE* ((state initial) ...) stepper
           clause0 clause1+ ...)))

    ((ITERATE* ((state initial) ...) stepper
         (IF condition)
         clause0 clause1+ ...)
     (IF condition
         (ITERATE* ((state initial) ...) stepper
             clause0 clause1+ ...)
         (VALUES* initial ...)))

    ((ITERATE* ((state initial) ...) stepper
         ((iterator ...) ...)
         clause0 clause1+ ...)
     (ITERATE* ((state initial) ...) stepper
         (PARALLEL (iterator ...) ...)
         clause0 clause1+ ...))

    ;** This clause must come last!  It would shadow the others.
    ((ITERATE* ((state initial) ...) stepper
         (iterator ...)
         clause0 clause1+ ...)
     (ITERATE* ((state initial) ...) stepper
         (PARALLEL (iterator ...))
         clause0 clause1+ ...))))

(define-syntax recur*
  (syntax-rules ()
    ((RECUR* base-case combiner clause0 clause1+ ...)
     (%RECUR LOOP base-case combiner clause0 clause1+ ...))))

(define-syntax lazy-recur*
  (syntax-rules ()
    ((LAZY-RECUR* base-case combiner clause0 clause1+ ...)
     (%RECUR LAZY-LOOP base-case combiner clause0 clause1+ ...))))

(define-syntax %recur
  (syntax-rules (PARALLEL NESTED DO LET LET-VALUES IF NOT AND OR)
    ((%RECUR looper recursion combiner expression)
     (combiner (LAMBDA () expression)
               (LAMBDA () recursion)))

    ((%RECUR looper recursion combiner
         (PARALLEL (iterator ...) ...)
         clause0 clause1+ ...)
     (looper REC ((iterator ...) ...)
       => recursion
       (%RECUR looper (REC) combiner clause0 clause1+ ...)))

    ;++ It would be nice to be able to share the rest of this between
    ;++ ITERATE and RECUR, but the deluge of macrotic overhead would
    ;++ obscure the intent.

    ((%RECUR looper recursion combiner
         (NESTED clause ...)
         clause0 clause1+ ...)
     (%RECUR looper recursion combiner
         clause ... clause0 clause1+ ...))

    ((%RECUR looper recursion combiner
         (DO command ...)
         clause0 clause1+ ...)
     (BEGIN command ...
            (%RECUR looper recursion combiner
                clause0 clause1+ ...)))

    ((%RECUR looper recursion combiner
         (LET ((variable value) ...))
         clause0 clause1+ ...)
     (LET ((variable value) ...)
       (%RECUR looper recursion combiner clause0 clause1+ ...)))

    ((%RECUR looper recursion combiner
         (LET variable value)
         clause0 clause1+ ...)
     (LET ((variable value))
       (%RECUR looper recursion combiner
           clause0 clause1+ ...)))

    ((%RECUR looper recursion combiner
         (LET-VALUES ((bvl expression) ...))
         clause0 clause1+ ...)
     (LET-VALUES ((bvl expression) ...)
       (%RECUR looper recursion combiner
           clause0 clause1+ ...)))

    ((%RECUR looper recursion combiner
         (LET-VALUES bvl expression)
         clause0 clause1+ ...)
     (LET-VALUES ((bvl expression))
       (%RECUR looper recursion combiner
           clause0 clause1+ ...)))

    ((%RECUR looper recursion combiner
         (IF condition)
         clause0 clause1+ ...)
     (IF condition
         (%RECUR looper recursion combiner
             clause0 clause1+ ...)
         recursion))

    ((%RECUR looper recursion combiner
         ((iterator ...) ...)
         clause0 clause1+ ...)
     (%RECUR looper recursion combiner
         (PARALLEL (iterator ...) ...)
         clause0 clause1+ ...))

    ((%RECUR looper recursion combiner
         (iterator ...)
         clause0 clause1+ ...)
     (%RECUR looper recursion combiner
         (PARALLEL (iterator ...))
         clause0 clause1+ ...))))

;;;; Conveniences

(define-syntax iterate
  (syntax-rules ()
    ((ITERATE ((state initial) ...) stepper clause0 clause1+ ...)
     (ITERATE* ((state initial) ...)
         (LAMBDA (BODY-THUNK) (stepper (BODY-THUNK) state ...))
         clause0 clause1+ ...))))

(define-syntax iterate!
  (syntax-rules ()
    ((ITERATE! clause0 clause1+ ...)
     (ITERATE* ()                       ;No state
         (LAMBDA (BODY-THUNK) (BODY-THUNK) (VALUES*))
         clause0 clause1+ ...))))

(define-syntax recur
  (syntax-rules ()
    ((RECUR base-case combiner clause0 clause1+ ...)
     (RECUR* base-case
         (LAMBDA (BODY-THUNK RECURSION-THUNK)
           (combiner (BODY-THUNK) (RECURSION-THUNK)))
         clause0 clause1+ ...))))

(define-syntax lazy-recur
  (syntax-rules ()
    ((LAZY-RECUR base-case combiner clause0 clause1+ ...)
     (LAZY-RECUR* base-case
         (LAMBDA (BODY-THUNK RECURSION-THUNK)
           (combiner (BODY-THUNK) (RECURSION-THUNK)))
         clause0 clause1+ ...))))

;++ Hack for MIT Scheme, whose multiple return values are broken.

(define-syntax values*
  (syntax-rules ()
    ((VALUES* single) single)
    ((VALUES* multiple ...) (VALUES multiple ...))))

;;;;; More Conveniences

(define-syntax iterate-values
  (syntax-rules (=>)
    ((ITERATE-VALUES ((state initial) ...)
         => result
         clause0 clause1+ ...)
     (RECEIVE (state ...)
              (ITERATE-VALUES ((state initial) ...)
                  clause0 clause1+ ...)
       result))

    ((ITERATE-VALUES updater ((state initial) ...)
         => result
         clause0 clause1+ ...)
     (RECEIVE (state ...)
              (ITERATE-VALUES updater ((state initial) ...)
                  clause0 clause1+ ...)
       result))

    ((ITERATE-VALUES ((state initial) ...) clause0 clause1+ ...)
     (ITERATE* ((state initial) ...)
         (LAMBDA (BODY-THUNK) (BODY-THUNK))
         clause0 clause1+ ...))

    ((ITERATE-VALUES updater ((state initial) ...) clause0 clause1+ ...)
     ;++ This should be visible only in the final expression.  However,
     ;++ that requires tail patterns, which are non-standard.
     (WITH-EXTENDED-PARAMETER-OPERATORS
         ((updater (VALUES (state . state) ...)))
       (ITERATE-VALUES ((state initial) ...) clause0 clause1+ ...)))))

(define-syntax recur-values
  (syntax-rules (=>)
    ((RECUR-VALUES base-case => result clause0 clause1+ ...)
     (CALL-WITH-VALUES (LAMBDA ()
                         (RECUR-VALUES base-case clause0 clause1+ ...))
       result))

    ((RECUR-VALUES base-case clause0 clause1+ ...)
     (RECUR* base-case
         (LAMBDA (COMBINER-THUNK RECURSION-THUNK)
           (CALL-WITH-VALUES RECURSION-THUNK (COMBINER-THUNK)))
         clause0 clause1+ ...))))

;;;; Collecting Lists & Streams

(define-syntax collect-list-reverse
  (syntax-rules (INITIAL)
    ((COLLECT-LIST-REVERSE (INITIAL tail-expression)
                           clause0 clause1+ ...)
     (ITERATE ((TAIL tail-expression)) CONS clause0 clause1+ ...))
    ((COLLECT-LIST-REVERSE clause0 clause1+ ...)
     (COLLECT-LIST-REVERSE (INITIAL '()) clause0 clause1+ ...))))

;;; The first definition of COLLECT-LIST is probably the one that you
;;; want.  On the other hand, what follows in comments is elegant, and
;;; shows the flexibility of the mchanism, especially when compared
;;; with the definition of COLLECT-STREAM.

(define-syntax collect-list
  (syntax-rules (INITIAL)
    ((COLLECT-LIST (INITIAL tail-expression) clause0 clause1+ ...)
     (APPEND-REVERSE (COLLECT-LIST-REVERSE clause0 clause1+ ...)
                     tail-expression))
    ((COLLECT-LIST clause0 clause1+ ...)
     (REVERSE (COLLECT-LIST-REVERSE clause0 clause1+ ...)))))

; (define-syntax collect-list
;   (syntax-rules (INITIAL)
;     ((COLLECT-LIST (INITIAL tail-expression) clause0 clause1+ ...)
;      (RECUR tail-expression CONS clause0 clause1+ ...))
;     ((COLLECT-LIST clause0 clause1+ ...)
;      (COLLECT-LIST (INITIAL '()) clause0 clause1+ ...))))

(define-syntax collect-stream
  (syntax-rules (INITIAL)
    ((COLLECT-STREAM (INITIAL tail-expression) clause0 clause1+ ...)
     (LAZY-RECUR tail-expression STREAM-CONS clause0 clause1+ ...))
    ((COLLECT-STREAM clause0 clause1+ ...)
     (COLLECT-STREAM (INITIAL STREAM-NIL) clause0 clause1+ ...))))

(define-syntax collect-list!
  (syntax-rules (INITIAL)
    ((COLLECT-LIST! (INITIAL tail-expression) clause0 clause1+ ...)
     (LET ((PAIR (CONS #F tail-expression)))
       (COLLECT-LIST-INTO! PAIR clause0 clause1+ ...)
       (CDR PAIR)))
    ((COLLECT-LIST! clause0 clause1+ ...)
     (COLLECT-LIST! (INITIAL '()) clause0 clause1+ ...))))

(define-syntax collect-list-into!
  (syntax-rules ()
    ((COLLECT-LIST-INTO! pair-expression clause0 clause1+ ...)
     (ITERATE ((PAIR pair-expression))
         (LAMBDA (DATUM PAIR)
           (LET ((TAIL (CONS DATUM (CDR PAIR))))
             (SET-CDR! PAIR TAIL)
             TAIL))
         clause0 clause1+ ...))))

;;;; Collecting Vectors and Strings

(define-syntax collect-vector
  (syntax-rules ()
    ((COLLECT-VECTOR clause0 clause1+ ...)
     (LIST->VECTOR (COLLECT-LIST clause0 clause1+ ...)))))

(define-syntax collect-string
  (syntax-rules ()
    ((COLLECT-STRING clause0 clause1+ ...)
     (LIST->STRING (COLLECT-LIST clause0 clause1+ ...)))))

;;; For the following two definitions, we defer the responsibility of
;;; bounds checking and error signalling to VECTOR-SET! and
;;; STRING-SET!.  This may not be a good idea.

(define-syntax collect-vector-of-length
  (syntax-rules ()
    ((COLLECT-VECTOR-OF-LENGTH length clause0 clause1+ ...)
     (LET ((VECTOR (MAKE-VECTOR length)))
       (ITERATE ((INDEX 0))
           (LAMBDA (DATUM INDEX)
             (VECTOR-SET! VECTOR INDEX DATUM)
             (+ INDEX 1))
           clause0 clause1+ ...)
       VECTOR))))

(define-syntax collect-string-of-length
  (syntax-rules ()
    ((COLLECT-STRING-OF-LENGTH length clause0 clause1+ ...)
     (LET ((STRING (MAKE-STRING length)))
       (ITERATE ((INDEX 0))
           (LAMBDA (DATUM INDEX)
             (STRING-SET! STRING INDEX DATUM)
             (+ INDEX 1))
           clause0 clause1+ ...)
       STRING))))

;;; The following definition of COLLECT-DISPLAY can collect any object,
;;; whose printed representation is computed using DISPLAY; it relies
;;; on SRFI 6 (Basic String Ports) to accomplish this.

(define-syntax collect-display
  (syntax-rules ()
    ((COLLECT-DISPLAY clause0 clause1+ ...)
     (LET ((OUTPUT-PORT (OPEN-OUTPUT-STRING)))
       (ITERATE ()                      ;No state
           (LAMBDA (DATUM)
             (DISPLAY DATUM OUTPUT-PORT)
             (VALUES))
           clause0 clause1+ ...)
       (GET-OUTPUT-STRING OUTPUT-PORT)))))

;;;;; Expanding Vector and String Collection

;;; These are slower than the definitions with lists.  Go figure.

; (define-syntax collect-vector
;   (syntax-rules ()
;     ((COLLECT-VECTOR clause0 clause1+ ...)
;      (%COLLECT-VECTOR
;       (MAKE-VECTOR VECTOR-LENGTH VECTOR-SET! IN-VECTOR)
;       DATUM
;       ()           ;No check for the data.
;       clause0 clause1+ ...))))
;
; (define-syntax collect-string
;   (syntax-rules ()
;     ((COLLECT-STRING clause0 clause1+ ...)
;      (%COLLECT-VECTOR
;       (MAKE-STRING STRING-LENGTH STRING-SET! IN-STRING)
;       DATUM
;       ((IF (NOT (CHAR? DATUM))
;            (ERROR "Non-character in COLLECT-STRING:" DATUM)))
;       clause0 clause1+ ...))))
;
; (define-syntax %collect-vector
;   (syntax-rules ()
;     ((%COLLECT-VECTOR
;       (make-vector vector-length vector-set! in-vector)
;       datum (check ...)
;       clause0 clause1+ ...)
;      (RECEIVE (LENGTH CHUNK-INDEX CHUNK CHUNKS)
;          (ITERATE ((LENGTH 0)
;                    (CHUNK-INDEX 0)
;                    (CHUNK (make-vector #x10))
;                    (CHUNKS '()))
;              (LAMBDA (datum LENGTH CHUNK-INDEX CHUNK CHUNKS)
;                check ...
;                (LET ((CHUNK-LENGTH (vector-length CHUNK)))
;                  (IF (< CHUNK-INDEX CHUNK-LENGTH)
;                      (BEGIN
;                        (vector-set! CHUNK CHUNK-INDEX datum)
;                        (VALUES LENGTH
;                                (+ CHUNK-INDEX 1)
;                                CHUNK
;                                CHUNKS))
;                      (LET ((CHUNK*
;                             (make-vector
;                              (IF (>= CHUNK-LENGTH #x1000)
;                                  #x1000
;                                  (* CHUNK-LENGTH 2)))))
;                        (vector-set! CHUNK* 0 datum)
;                        (VALUES (+ LENGTH CHUNK-LENGTH)
;                                1        ;We filled in the first slot,
;                                CHUNK*   ;  so start at index 1.
;                                (CONS CHUNK CHUNKS))))))
;              clause0 clause1+ ...)
;        (LET* ((TOTAL-LENGTH (+ LENGTH CHUNK-INDEX))
;               (RESULT (make-vector TOTAL-LENGTH)))
;          (LOOP ((FOR ELEMENT OFFSET (in-vector CHUNK 0 CHUNK-INDEX)))
;            (vector-set! RESULT (+ LENGTH OFFSET) ELEMENT))
;          (LOOP ((FOR CHUNK (IN-LIST CHUNKS))
;                 (WITH BASE LENGTH BASE*)
;                 (LET BASE* (- BASE (vector-length CHUNK))))
;            (LOOP ((FOR ELEMENT OFFSET (in-vector CHUNK)))
;              (vector-set! RESULT (+ BASE* OFFSET) ELEMENT)))
;          RESULT)))))

;;;; Numerical Collection

(define-syntax collect-sum
  (syntax-rules (INITIAL)
    ((COLLECT-SUM (INITIAL value-expression) clause0 clause1+ ...)
     (ITERATE ((SUM value-expression)) + clause0 clause1+ ...))
    ((COLLECT-SUM clause0 clause1+ ...)
     (COLLECT-SUM (INITIAL 0) clause0 clause1+ ...))))

(define-syntax collect-product
  (syntax-rules (INITIAL)
    ((COLLECT-PRODUCT (INITIAL value-expression) clause0 clause1+ ...)
     (ITERATE ((PRODUCT value-expression)) * clause0 clause1+ ...))
    ((COLLECT-PRODUCT clause0 clause1+ ...)
     (COLLECT-PRODUCT (INITIAL 1) clause0 clause1+ ...))))

(define-syntax collect-count
  (syntax-rules ()
    ((COLLECT-COUNT clause0 clause1+ ...)
     (COLLECT-SUM clause0 clause1+ ... 1))))

(define-syntax collect-average
  (syntax-rules ()
    ((COLLECT-AVERAGE clause0 clause1+ ...)
     (RECEIVE (SUM COUNT)
              (ITERATE ((SUM 0)
                        (COUNT 0))
                  (LAMBDA (DATUM SUM COUNT)
                    (VALUES (+ SUM DATUM)
                            (+ COUNT 1)))
                  clause0 clause1+ ...)
       (/ SUM COUNT)))))

(define-syntax collect-min
  (syntax-rules (INITIAL)
    ((COLLECT-MIN (INITIAL minimum-expression) clause0 clause1+ ...)
     (ITERATE ((MINIMUM minimum-expression)) MIN clause0 clause1+ ...))
    ((COLLECT-MIN clause0 clause1+ ...)
     (ITERATE ((MINIMUM #F))
         (LAMBDA (DATUM MINIMUM)
           (IF (AND DATUM MINIMUM)
               (MIN DATUM MINIMUM)
               (OR DATUM MINIMUM)))
         clause0 clause1+ ...))))

(define-syntax collect-max
  (syntax-rules (INITIAL)
    ((COLLECT-MAX (INITIAL maximum-expression) clause0 clause1+ ...)
     (ITERATE ((MAXIMUM maximum-expression)) MAX clause0 clause1+ ...))
    ((COLLECT-MAX clause0 clause1+ ...)
     (ITERATE ((MAXIMUM #F))
         (LAMBDA (DATUM MAXIMUM)
           (IF (AND DATUM MAXIMUM)
               (MAX DATUM MAXIMUM)
               (OR DATUM MAXIMUM)))
         clause0 clause1+ ...))))
