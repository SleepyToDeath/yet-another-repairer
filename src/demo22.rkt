#lang rosette/safe

(require rosette/lib/match)   ; provides `match`
(require "string-id.rkt")
(require "match-define.rkt")

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

(match (cons 1 (cons 2 (cons 3 null))) [(list a b c) (println c)])

(cons 1 (cons 2 3))
(list 1 2 3)

(match-define (list aa bb cc) (list 1 2 3))
aa
bb
cc

(match-define (cons aaa bbb) (cons 4 5))
aaa
bbb

(match-define (list a4 b4) (list 7 8))
a4
b4

(define (foo x)
	(list x (+ x x) (* x x)))

(match-define (list x1 x2 x3) (foo 10))
x1
x2
x3

(equal? -1 (list 1 2 3))
