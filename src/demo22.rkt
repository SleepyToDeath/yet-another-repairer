#lang rosette/safe

(require rosette/lib/match)   ; provides `match`
(require "string-id.rkt")

(string-id "var1")

(string-id "var2")

(string-id "var1")

(string-id "var2")

(define (f _name)
	(define name (string-id _name))
	(+ name 1) )

(f "var2")

(define-symbolic* i integer?)

i

(maybe-string-id (+ 1 i))

(define a 1)

(struct teststr (a b) #:transparent)

(define s (teststr a 2))

s

(set! a 2)

s

(let ([a (cons 1 2)] [b (cons 1 2)])
	(equal? a b))

(equal? 1 1)

(equal? (list 1 2) (list 1 2 3))

(define b 2)

(equal? a b)

(equal? a (list 2 b))

(match (cons a b) [(cons a b)
	(begin
		(define c a)
		c)])

(define-symbolic* e integer?)

(define l1 (list 1 2 3 4 5))
(define l2 (list 1 2 3 4 e))
(define b1 (equal? l1 l2))

(define debug-sol (optimize #:maximize (list (if b1 1 0))
          #:guarantee (assert #t)))

debug-sol
