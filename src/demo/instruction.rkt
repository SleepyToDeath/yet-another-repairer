#lang rosette

(require rosette/query/debug)
(require "machine.rkt")

(define (inst-nop m)
	m
)

(define (inst-halt m)
	(machine (machine-prog m) (machine-mem m) pc-fin)
)

(define (inst-ass-const var-l const)
	(define (update m)
		(machine (machine-prog m) (append (machine-mem m) (list const)) (+ (machine-pc m) 1)))
	update)

(define (inst-ass-var var-l var-r)
	(define (update m)
		(let ([var-r-v (list-ref (machine-mem m) var-r)])
			(machine (machine-prog m) (append (machine-mem m) (list var-r-v)) (+ (machine-pc m) 1))))
	update)

(define (inst-ass-op var-l var-r1 op var-r2)
	(define (update m)
		(let ([var-r1-v (list-ref (machine-mem m) var-r1)]
			  [var-r2-v (list-ref (machine-mem m) var-r2)])
			(machine (machine-prog m) (append (machine-mem m) (list (op var-r1-v var-r2-v))) (+ (machine-pc m) 1))))
	update)

(provide inst-nop inst-halt inst-ass-const inst-ass-var inst-ass-op)

