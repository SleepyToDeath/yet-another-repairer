#lang rosette/safe

(require (for-syntax racket/match racket/syntax))

(require rosette/lib/match) 

(provide (all-defined-out))

(define-syntax (id2tmp stx)
	(define id (cadr (syntax-e stx)))
	(define pred (format-symbol "__~a" (syntax->datum id)))
	(datum->syntax id pred))

(define-syntax match-define
	(syntax-rules ()

		[(match-define (cons name1 name2) value)
			(begin
			(define name1 #f) 
			(define name2 #f) 
			(match value [(cons n1 n2) (begin (set! name1 n1) (set! name2 n2))]))]

		[(match-define (list name ...) value)
			(begin
			(define name #f) ... 
			(define tmp null)
			(match value [(list name ...) (set! tmp (list name ...))])
			(define (ass-n-pop)
				(define ret (car tmp))
				(set! tmp (cdr tmp))
				ret)
			(set! name (ass-n-pop)) ...)]))

