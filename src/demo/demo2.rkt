#lang rosette

(require rosette/query/debug rosette/lib/render)
(require "interpreter2.rkt" "machine.rkt")


(define/debug m (craft-machine 
	(jimple
	(ass-const 0 = 1)
	(ass-const 1 = 1)
	(ass-op 2 = 0 + 1)
	(halt)
	(nop))
	null))


(define result (last (machine-mem (run m))))

result


(define/debug m2 (craft-machine
	(jimple
		(ass-const 0 = 1)
		(ass-const 1 = 2)
		(ass-const 2 = 3)
		(ass-op 3 = 0 + 1)
		(ass-op 4 = 3 - 2)
		(halt)
		(nop))
	null))
	
(define result2 (last (machine-mem (run m2))))

result2

(define ucore (debug [integer?] (assert (= result2 6))))

(render ucore)

