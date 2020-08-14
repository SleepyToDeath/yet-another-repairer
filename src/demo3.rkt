#lang rosette

(require rosette/query/debug rosette/lib/render)
(require "interpreter3.rkt")
(require "machine3.rkt")
(require "util.rkt")

(define f (~> machine? integer?))


(define/debug (m)
	(jimple
		(ass-const 0 = 1)
		(ass-const 1 = 2)
		(ass-const 2 = 3)
		(ass-op 3 = 0 + 1)
		(ass-op 4 = 3 - 2)
		(halt)
		(nop)))
	
(m)

(define ucore (debug [f] (assert (= (m) 6))))

(save-pict (render ucore) "debug.jpeg")

