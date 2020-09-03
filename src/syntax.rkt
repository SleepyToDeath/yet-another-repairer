#lang rosette/safe

(require (for-syntax racket/match))

(define-generics ast
	[ast-check ast])

(define-generics expanded
	[expanded-check expanded])

(struct context (vars nums) #:transparent)

(provide gen:ast gen:expanded ast-check expanded-check)
(provide all-defined-out)



(define-syntax (id2enum stx)
	(define enum (string->symbol (string-append (symbol->string (car (cdr (syntax->datum stx)))) "-enum")))
	(datum->syntax stx enum))

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

(provide id2pred id2accessor id2enum)



(begin-for-syntax
	(define (id2enum id)
		(define enum (string->symbol (string-append (symbol->string (syntax->datum id)) "-enum")))
		(datum->syntax id enum))
)


; [TODO] there's some problem with expansion order of exported macros
;		 these three macros must be copy-pasted in the file defining the syntax

;(define-syntax (LHS stx)
;	(match (syntax->list stx)
;		[(list LHS name ( rname ::= rhs ... ))
;
;			(datum->syntax stx `(begin
;
;		))]))
;

(define-syntax-rule (LHS name ( rname ::= rhs ... ))
	(begin
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
)

(define-syntax-rule (RHS name (lname : lhs) ...)
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




