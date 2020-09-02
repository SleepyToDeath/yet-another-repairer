#lang rosette/safe

(require "syntax.rkt")

(define-syntax-rule (LHS name rname rhs ...)
	(struct name (rname) #:transparent
		#:methods gen:ast
		[ 
			(define (ast-check __t)
				(define __et ((id2accessor name rname) __t))
				(and
					(or
						((id2pred rhs) __et) ...
					)
					(expanded-check __et)))
		]
	)
)

(define-syntax-rule (RHS name (lname lhs) ...)
	(struct name (lname ...) #:transparent
		#:methods gen:expanded
		[
			(define (expanded-check __et)
				(and
					(and 
						((id2pred lhs) ((id2accessor name lname) __et)) ...
					)
					(and
						(ast-check ((id2accessor name lname) __et)) ...
					)))
		] 
	)
)

(define-syntax-rule (TERM name val ...)
	(struct name (val ...) #:transparent
		#:methods gen:ast
		[
			(define (ast-check __t) 
				#t)
		]
	)
)



;expr ::= expr expr
;		  | node
;node ::= int

(LHS expr et expr-1 expr-2)
(RHS expr-1 (t1 expr) (t2 expr))
(RHS expr-2 (t node))

(TERM node i)

;(define test-expr 
;	(expr (expr-1 
;		(expr (expr-2 
;			(node 1))) 
;		(expr (expr-2 
;			(node 2))))))
;
;(ast-check test-expr)

(provide expr expr-1 expr-2 node)

