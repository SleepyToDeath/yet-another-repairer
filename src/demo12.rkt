#lang rosette/safe

(require "syntax.rkt")

;expr ::= expr expr
;		  | node
;node ::= int

;(LHS expr et expr-1 expr-2)
;(RHS expr-1 (t1 expr) (t2 expr))
;(RHS expr-2 (t node))
;
;(TERM node i)

(define test-expr 
	(expr (expr-1 
		(expr (expr-2 
			(node 1))) 
		(expr (expr-2 
			(node 2))))))

(ast-check test-expr)


