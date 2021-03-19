#lang rosette/safe

(require "memory-common.rkt")
(require "map.rkt")
(require (prefix-in std: racket/base))
(require racket/pretty)

(provide (all-defined-out))

(define string-id-table null)
(define string-id-map (imap-empty default-type))
(define string-id-counter 0)
(define string-id-int-offset 1000)

;provide an unique id to each string; equal? strings have same id
(define (real-string-id s)
	(define id (imap-get string-id-map s default-type))
	(define id-true (if (not (equal? id not-found)) id
		(begin
			(set! string-id-table (cons (cons s string-id-counter) string-id-table))
;			(pretty-print string-id-table)
			(set! string-id-map (imap-set string-id-map s string-id-counter))
			(set! string-id-counter (+ 1 string-id-counter))
			(- string-id-counter 1))))
	(maybe-add-string-value-of s id-true)
	id-true)

(define (maybe-string-id s)
	(if (number? s) s (real-string-id s)))

(define (string-id s)
	(maybe-string-id s))

(define (string-id-pure s)
	(imap-get string-id-map s default-type))


;number -> string
(define string-value-of-table (imap-empty default-type))

(define (maybe-add-string-value-of s id)
	(if (and (std:string? s) (std:string->number s) (is-not-found? (imap-get string-value-of-table (std:string->number s) default-type)))
		(set! string-value-of-table (imap-set string-value-of-table (std:string->number s) id))
		#f))

(define (string-value-of v)
	(imap-get string-value-of-table v default-type))
