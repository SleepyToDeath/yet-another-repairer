#lang rosette/safe

(require "demo12.rkt")
(require "syntax.rkt")

(define test-expr 
	(expr (expr-1 
		(expr (expr-2 
			(node 1))) 
		(expr (expr-2 
			(node 2))))))

(ast-check test-expr)


