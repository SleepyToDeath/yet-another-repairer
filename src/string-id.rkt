#lang rosette/safe

(require "map.rkt")
(require (prefix-in std: racket/base))

(provide maybe-string-id string-id)

(define string-id-map imap-empty)
(define string-id-counter 0)

;provide an unique id to each string; equal? strings have same id
(define (string-id s)
	(define id (imap-get string-id-map s))
	(define id-true (if (not (= id not-found)) id
		(begin
			(set! string-id-map (imap-set string-id-map s string-id-counter))
			(set! string-id-counter (+ 1 string-id-counter))
			(- string-id-counter 1))))
	id-true)

(define (maybe-string-id s)
	(if (std:string? s) (string-id s) s))

