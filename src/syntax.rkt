#lang rosette/safe


(define-generics ast
	[ast-check ast])

(define-generics expanded
	[expanded-check expanded])

(provide gen:ast gen:expanded ast-check expanded-check)







(define-syntax (id2pred stx)
	(define pred (string->symbol (string-append (symbol->string (car (cdr (syntax->datum stx)))) "?")))
	(datum->syntax stx pred))

(define-syntax (id2accessor stx)
	(define acc 
		(string->symbol 
			(string-append 
				(symbol->string (car (cdr (syntax->datum stx))))
				"-"
				(symbol->string (car (cdr (cdr (syntax->datum stx))))))))
	(datum->syntax stx acc))

(provide id2pred id2accessor)






; [TODO] there's some problem with expansion order of exported macros
;		 these three macros must be copy-pasted in the file defining the syntax
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


(provide LHS RHS TERM)

;(LHS expr et expr-1 expr-2)
;(RHS expr-1 (t1 expr) (t2 expr))
;(RHS expr-2 (t node))
;
;(TERM node i)
;
;(provide node expr expr-1 expr-2)
;
;(define test-expr 
;	(expr (expr-1 
;		(expr (expr-2 
;			(node 1))) 
;		(expr (expr-2 
;			(node 2))))))
;
;(ast-check test-expr)

