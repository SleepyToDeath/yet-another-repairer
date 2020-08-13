#lang rosette

(require rosette/query/debug rosette/lib/render)

(define f (~> integer? integer?))

(define vec (list 1 2))

;(define-symbolic idx integer?)

(define sol (solve 
	(begin
		(assert (= (f 0) (list-ref vec 0)))
		(assert (= (f 1) (list-ref vec 1))))))

(define g (evaluate f sol))

g

(define v0 (g 0))
(define v1 (list-ref vec 0))

v0
v1


(define/debug ret 
              (equal? (f 1) (f 1)))


ret

(define cmp (equal? ret #t))

cmp

(define ucore (debug [f integer? boolean?] (assert (equal? ret #t))))

(render ucore)

