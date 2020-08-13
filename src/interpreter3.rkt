#lang rosette

;(require (for-syntax racket/match))

(require "instruction3.rkt" "machine3.rkt")

(define-syntax-rule (jimple src ...)
	(let* ([prog (list src ...)]
		   [m (craft-machine prog null)])
		(continue m)))

(define-syntax-rule (ass-const var-l = const) 
	(lambda (m) (inst-ass-const m var-l const)))

(define-syntax-rule (ass-var var-l = var-r) 
	(lambda (m) (inst-ass-var m var-l var-r)))

(define-syntax-rule (ass-op var-l = var-r1 op var-r2)
	(lambda (m) (inst-ass-op m var-l var-r1 op var-r2)))

(define-syntax-rule (nop)
	(lambda (m) (inst-nop m)))

(define-syntax-rule (halt)
	(lambda (m) (inst-halt m)))

(provide jimple ass-const ass-var ass-op halt nop)

