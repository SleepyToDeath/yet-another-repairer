#lang rosette

(require rosette/query/debug rosette/lib/render)
(require "interpreter3.rkt")


(define/debug m
	(jimple
	(ass-const 0 = 1)
	(ass-const 1 = 1)
	(ass-op 2 = 0 + 1)
	(halt)
	(nop)))

m

(define/debug m2 
	(jimple
		(ass-const 0 = 1)
		(ass-const 1 = 2)
		(ass-const 2 = 3)
		(ass-op 3 = 0 + 1)
		(ass-op 4 = 3 - 2)
		(halt)
		(nop)))
	
m2 

(define ucore (debug [integer?] (assert (= m2 0))))

(render ucore)

