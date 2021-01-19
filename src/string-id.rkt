#lang rosette/safe

(require "memory-common.rkt")
(require "map.rkt")
(require (prefix-in std: racket/base))
(require racket/pretty)

(provide (all-defined-out))

(define string-id-table null)
(define string-id-map imap-empty)
(define string-id-counter 0)

;provide an unique id to each string; equal? strings have same id
(define (real-string-id s)
	(define id (imap-get string-id-map s))
	(define id-true (if (not (equal? id not-found)) id
		(begin
			(set! string-id-table (cons (cons s string-id-counter) string-id-table))
;			(pretty-print string-id-table)
			(set! string-id-map (imap-set string-id-map s string-id-counter))
			(set! string-id-counter (+ 1 string-id-counter))
			(- string-id-counter 1))))
	id-true)

(define (maybe-string-id s)
	(if (number? s) s (real-string-id s)))

(define (string-id s)
	(maybe-string-id s))
