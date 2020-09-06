#lang rosette/safe

(require "syntax-jimple.rkt")
(require "syntax.rkt")

; Syntax check
(define line1 
	(stat (stat-ass 
		(variable 1) 
		(expr (expr-binary 
			(expr (expr-var (variable 2))) 
			(op +)
			(expr (expr-const (const 1)))))
	)))

(ast-check line1)




; Enumerator
(define labels '(1 2 3))
(define vars '(1 2 3))
(define nums '(1 2 3))
(define ops '(+ -))

(define ctxt (syntax-context vars nums ops labels))

(define hole (stat-enum ctxt 10))

(define sketch 
	(stats (stats-multi
		(stats (stats-single line1))
		(stats (stats-single hole)))))

sketch

