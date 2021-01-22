#lang rosette/safe
(require "formula.rkt")

(require (prefix-in std: racket/base))

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

(define d1 (maybe identity cadr b2 #f))

(display "\nd1:\n")
d1
(asserts)

(define d2 (reflection-map number? identity (list d1)))

(display "\nd2:\n")
d2
(asserts)

(define t3 (list 1 2 3 4))
(define t4 (list 5 6 7 8 9))

(define t34 (if (> c1 0) t3 t4))

(display "\nt34:\n")
t34
(asserts)

(define t5 (if (equal? (car t34) 1) (cons 0 t34) t34))

(display "\nt5:\n")
t5
(asserts)

(define len1 (length (asserts)))
(if (equal? (car t34) 1) (std:error "br1!") #f)
(define len2 (length (asserts)))
(if (> len2 len1) (std:error "br1!!") #f)
