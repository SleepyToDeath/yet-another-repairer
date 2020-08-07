#lang rosette

(require rosette/query/debug)
(require "machine.rkt")

(define (inst-nop m)
	m
)

(define (inst-halt m)
	(machine (machine-prog m) (machine-mem m) pc-fin)
)

(define/debug (inst-ass-const var-l const)
	(define (update m)
		(machine (machine-prog m) (hash-set (machine-mem m) var-l const) (+ (machine-pc m) 1)))
	update)

(define/debug (inst-ass-var var-l var-r)
	(define (update m)
		(define var-r-v (hash-ref (machine-mem m) var-r))
		(machine (machine-prog m) (hash-set (machine-mem m) var-l ) (+ (machine-pc m) 1)))
	update)

(define/debug (inst-ass-op var-l var-r1 op var-r2)
	(define (update m)
		(define var-r1-v (hash-ref (machine-mem m) var-r1))
		(define var-r2-v (hash-ref (machine-mem m) var-r2))
		(machine (machine-prog m) (hash-set (machine-mem m) var-l (op var-r1-v var-r2-v)) (+ (machine-pc m) 1)))
	update)

(provide inst-nop inst-halt inst-ass-const inst-ass-var inst-ass-op)

