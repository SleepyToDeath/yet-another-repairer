#lang rosette

(require rosette/query/debug)
(require "machine3.rkt")

(define (inst-nop m)
	(continue m))

(define (inst-halt m)
	(last (machine-mem m)))

(define (inst-ass-const m var-l const)
	(update-ass m const))

(define (inst-ass-var m var-l var-r)
	(let ([var-r-v (list-ref (machine-mem m) var-r)])
		(update-ass m var-r-v)))

(define (inst-ass-op m var-l var-r1 op var-r2)
	(let ([var-r1-v (list-ref (machine-mem m) var-r1)]
		  [var-r2-v (list-ref (machine-mem m) var-r2)])
		(update-ass m (op var-r1-v var-r2-v))))

(provide inst-nop inst-halt inst-ass-const inst-ass-var inst-ass-op)

