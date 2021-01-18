#lang rosette/safe
(require "formula.rkt")

(struct T (x y) #:transparent)

(define-symbolic c1 c2 integer?)

c1
c2
(asserts)

(define t1 (list 1 2))
(define t2 (list 3 4))

(define b1 (if (> c1 0) t1 t2))

(display "\nb1:\n")
b1
(asserts)

(define b2 (if (> c2 0) b1 #f))

(display "\nb2:\n")
b2
(asserts)

(define c (reflection-map list? (lambda (t) (reflection-map number? identity (list (cadr t)))) (list b2)))

(display "\nc:\n")
c
(asserts)

(define d1 (maybe cadr b2))

(display "\nd1:\n")
d1
(asserts)

(define d2 (reflection-map number? identity (list d1)))

(display "\nd2:\n")
d2
(asserts)
