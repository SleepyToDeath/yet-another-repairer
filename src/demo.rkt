#lang rosette

(require rosette/query/debug rosette/lib/render)
(require "interpreter.rkt" "machine.rkt")

(define/debug m (craft-machine
	(jimple
		(ass-const "r1" = 1)
		(ass-const "r2" = 1)
		(ass-op "r3" = "r1" + "r2")
		(halt)
		(nop))
	(hash)))
	
(define result (hash-ref (machine-mem (run m)) "r3"))

(provide result)

(define/debug m2 (craft-machine
	(jimple
		(ass-const "r1" = 1)
		(ass-const "r2" = 2)
		(ass-const "r3" = 3)
		(ass-op "r4" = "r1" + "r2")
		(ass-op "r5" = "r4" - "r3")
		(halt)
		(nop))
	(hash)))
	
(define result2 (hash-ref (machine-mem (run m2)) "r5"))

result2

(define ucore (debug [integer?] (assert (= result2 6))))

(render ucore)

