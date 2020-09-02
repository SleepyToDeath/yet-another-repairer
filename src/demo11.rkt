#lang rosette/safe

(require "syntax.rkt")

;expr ::= expr expr
;		  | node
;node ::= int

(struct expr (et) #:transparent
	#:methods gen:ast
	[ 
		(define (ast-check t)
			(and
				(or
					(expr-1? (expr-et t))
					(expr-2? (expr-et t))
				)
				(expanded-check (expr-et t))))
	]
)


(struct expr-1 (t1 t2) #:transparent
	#:methods gen:expanded
	[
		(define (expanded-check et)
			(and
				(and 
					(expr? (expr-1-t1 et))
					(expr? (expr-1-t2 et))
				)
				(and
					(ast-check (expr-1-t1 et))
					(ast-check (expr-1-t2 et))
				)))
	] 
)


(struct expr-2 (t) #:transparent
	#:methods gen:expanded
	[
		(define (expanded-check et)
			(and
				(and
					(node? (expr-2-t et))
				)
				(and
					(ast-check (expr-2-t et))
				)))
	]
)

(struct node (i) #:transparent
	#:methods gen:ast
	[
		(define (ast-check t) 
			#t)
	]
)

(define test-expr 
	(expr (expr-1 
		(expr (expr-2 
			(node 1))) 
		(expr (expr-2 
			(node 2))))))

(ast-check test-expr)


