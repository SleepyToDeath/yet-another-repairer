#lang rosette

;(require (for-syntax racket/match))

(require "instruction.rkt")

(define-syntax-rule (jimple src ...)
	(list src ...))

(define-syntax-rule (ass-const var-l = const) 
	(inst-ass-const var-l const))

(define-syntax-rule (ass-var var-l = var-r) 
	(inst-ass-var var-l var-r))

(define-syntax-rule (ass-op var-l = var-r1 op var-r2)
	(inst-ass-op var-l var-r1 op var-r2))

(define-syntax-rule (nop)
	inst-nop)

(define-syntax-rule (halt)
	inst-halt)

(provide jimple ass-const ass-var ass-op halt nop)

