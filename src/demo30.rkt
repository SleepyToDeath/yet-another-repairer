#lang rosette/safe
(require "formula.rkt")

(struct T (x y) #:transparent)

(define-symbolic c1 c2 integer?)

c1
c2
(asserts)

(define t1 (T 1 2))
(define t2 (T 3 4))

(define b1 (if (> c1 0) t1 t2))

b1
(asserts)

(define b2 (if (> c2 0) b1 #f))

b2
(asserts)

(define c (reflection-map T? (lambda (t) (reflection-map number? identity (list (T-y t)))) (list b2)))

c
(asserts)
