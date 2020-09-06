#lang rosette/safe

(require (for-syntax racket/match racket/syntax))

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

;=================== AST interface ================
(define-generics ast
	[ast-check ast])

(define-generics expanded
	[expanded-check expanded])

(struct syntax-context (vars nums ops labels) #:transparent)

(provide gen:ast gen:expanded ast-check expanded-check)
(provide syntax-context)
;==================================================



;============ Syntax Defining Macros ==============
(define-syntax-rule (LHS-C name ( rname ::= rhs ... ))
	(begin
	(struct name (rname) #:transparent
		#:methods gen:ast
		[ 
			(define (ast-check __t)
				(define __et ((id2acc name rname) __t))
				(and
					(or
						((id2pred rhs) __et) ...
					)
					(expanded-check __et)))
		]
	)
	(provide name))
)

(define-syntax-rule (RHS-C name (lname : lhs) ...)
	(begin
	(struct name (lname ...) #:transparent
		#:methods gen:expanded
		[
			(define (expanded-check __et)
				(and
					(and 
						((id2pred lhs) ((id2acc name lname) __et)) ...
					)
					(and
						(ast-check ((id2acc name lname) __et)) ...
					)))
		] 
	)
	(provide name))
)
;[!] users should define their own enumerator for each terminal
;	 name-enum: ctxt -> name
(define-syntax-rule (TERM name val ...)
	(begin
	(struct name (val ...) #:transparent
		#:methods gen:ast
		[
			(define (ast-check __t) 
				#t)
		]
	)
	(provide name))
)

(define-syntax-rule (LHS-E name -> (name-enum ::= rhs-enum ...))
	(begin
	(define (name-enum ctxt depth-limit) 
		(if (> depth-limit 0)
			(name (choose* 
					(rhs-enum ctxt (- depth-limit 1)) ... ))
			(invalid 0)))
	(provide name-enum)))

(define-syntax-rule (RHS-E name -> name-enum (lhs-enum ...))
	(begin
	(define (name-enum ctxt depth-limit) 
		(name (lhs-enum ctxt depth-limit) ... ))
	(provide name-enum)))


(provide LHS-C LHS-E RHS-C RHS-E TERM)
;=================================================



;==================== Helpers ====================
(define-syntax (id2enum stx)
	(define id (cadr (syntax-e stx)))
	(define enum (format-symbol "~a-enum" (syntax->datum id)))
	(datum->syntax id enum))

(define-syntax (id2pred stx)
	(define id (cadr (syntax-e stx)))
	(define pred (format-symbol "~a?" (syntax->datum id)))
	(datum->syntax id pred))

(define-syntax (id2acc stx)
	(define id-s (cadr (syntax-e stx)))
	(define id-f (caddr (syntax-e stx)))
	(define acc (format-symbol "~a-~a" (syntax->datum id-s) (syntax->datum id-f)))
	(datum->syntax id-f acc))

(TERM invalid any)

(define (invalid-enum ctxt depth-limit) (invalid 0))

(provide id2pred id2acc id2enum invalid)
;==================================================


