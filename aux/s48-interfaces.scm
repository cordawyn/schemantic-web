;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Auxiliary Packages for the Schemantic Web
;;;; Interface Definitions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface foof-loop-interface
  (export
    ((loop
      lazy-loop
      syntactic-error
      loop-clause-error

      ;; Accumulators
      listing
      listing-reverse
      appending
      appending-reverse
      listing!
      listing-into!
      summing
      multiplying
      minimizing
      maximizing

      in-list
      in-lists

      in-vector
      in-vector-reverse
      in-string
      in-string-reverse

      in-port
      in-file

      up-from
      down-from
      ) :syntax)
    ))

(define-interface nested-foof-loop-interface
  (export
    ((
      collect-average
      collect-count
      collect-display
      collect-list
      collect-list!
      collect-list-into!
      collect-list-reverse
      collect-max
      collect-min
      collect-product
      collect-stream
      collect-string
      collect-string-of-length
      collect-sum
      collect-vector
      collect-vector-of-length
      iterate
      iterate*
      iterate!
      iterate-values
      lazy-recur
      lazy-recur*
      recur
      recur*
      recur-values
      ) :syntax)
    ))

(define-interface extended-parameter-operators-interface
  (export
    (with-extended-parameter-operators :syntax)
    (with-extended-parameter-operators* :syntax)
    ))

(define-interface format-combinators-interface
  (export
    (define-format :syntax)
    format:bracketed
    format:bracketed-list
    format:call-with-output-port
    format:char
    format:delayed
    format:display
    format:empty
    format:indentation
    format:indented-line
    format:join
    format:join/infix
    format:join/prefix
    format:join/suffix
    format:line
    format:line-break
    format:line-start
    format:list
    format:named
    format:non-breaking-space
    format:number
    format:number-with-radix
    format:sequence
    format:soft-break
    format:string
    format:with-indentation
    ))

(define-interface format-driver-interface
  (export
    format-options
    format-to-port
    format-to-string
    format-with-char-handler
    format-with-condensed-whitespace
    format-with-indentation
    format-with-indenter
    format-with-line-break-handler
    format-with-line-start-handler
    format-with-line-tracking
    format-with-property
    format-with-soft-break-handler
    format-with-string-handler
    format-with-tab-width
    space-indenter
    tab-indenter
    ))

(define-interface format-state-interface
  (export
    apply-format
    canonicalize-format
    format-state?
    format-state/insert-property
    format-state/lookup-property
    format-state/modify-property
    format-state/property
    format-state/search-property
    format-state/string-handler
    format-state/update-property
    format-state-with-output-port
    format->procedure
    make-format-state
    procedure->format
    ))

(define-interface pattern-matching-interface
  (export
    (match              :syntax)
    (match-lambda       :syntax)
    (match-lambda*      :syntax)
    (match-let          :syntax)
    (match-let*         :syntax)
    (match-letrec       :syntax)
    ))
