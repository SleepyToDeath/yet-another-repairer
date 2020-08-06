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

