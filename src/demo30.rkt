#lang rosette/safe
(require "formula.rkt")

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))

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

(define d1 (maybe-do identity cadr b2 #f))

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

(define t34+ (cons 0 t34))

(display "\nt34+:\n")
t34+
(asserts)

(define t5 (if (equal? (car t34) 1) (cons 0 t34) t34))

(display "\nt5:\n")
t5
(asserts)

(define t3.maybe (if (> c1 0) t3 #f))

(define t3+ (if (list? t3.maybe) (cons 0 t3.maybe) #f))

(display "\nt3+:\n")
t3+
(asserts)

(define t3+.maybe (ormap (lambda (c3) (if (> c2 0) c3 #f)) (list t3+)))

(display "\nt3+.maybe:\n")
t3+.maybe
(asserts)

(define t3++ (if (list? t3+.maybe) (append (list -1) t3+.maybe) #f))

;(define t3++ (cons -1 t3+.maybe))

(display "\nt3++:\n")
t3++
(display "\n")
(asserts)

(struct st (lsts) #:transparent)

(define (ass-top st0 index value)
	(define v (car (st-lsts st0)))
	(define v+ (std:list-set v index value))
	(st (cons v+ (cdr (st-lsts st0)))))

(define st1 (st (list (list 1 2 3) (list 2 3 4))))

st1

(define st2 (ass-top st1 1 0))

st1
st2

(define st3 (if (> 0 c1) st1 st2))

st3


(struct AB (a b) #:transparent)

(define ab (AB (AB (AB 1 2) (AB 3 4)) (AB 5 6)))

ab

(define abab (std:struct-copy AB ab))

abab

(define bb (AB-b (AB-a abab)))

bb

(set! bb (AB 0 4))

bb

ab

abab

(define ab.maybe (if (> 0 c1) ab #f))

(define ab.maybe.maybe (if ab.maybe 0 1))

ab.maybe.maybe



(define i1 10)

(define bv1 (integer->bitvector i1 (bitvector 32)))

i1
bv1

(define-symbolic* i2 integer?)

(define bv2 (integer->bitvector i2 (bitvector 32)))

(define i2+ (bitvector->integer bv2))

i2
bv2
i2+

(define-symbolic* b3 (bitvector 32))



(define-generics A
	[fooA A])

(define-generics B
	[fooB B])

(struct C (id) #:transparent
	#:methods gen:A
	[
		(define (fooA c)
			(display "this is A\n"))
		]

	#:methods gen:B
	[
		(define (fooB c)
			(display "this is B\n"))
		])

(define ccc (C 1))

(fooA ccc)
(fooB ccc)

(define e 1)

