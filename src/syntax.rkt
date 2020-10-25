#lang rosette/safe

(require (for-syntax racket/match racket/syntax))

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;=================== AST interface ================
(define-generics ast
	[ast-check ast])

(define-generics expanded
	[expanded-check expanded])

(struct syntax-context (vars consts ops labels) #:transparent)
;==================================================



;============ Syntax Defining Macros ==============
(define-syntax-rule (LHS-C name ( rname ::= rhs ... ))
	(struct name (rname) #:transparent
		#:methods gen:ast
		[ 
			(define (ast-check __t)
				(define __et ((id2acc name rname) __t))
				(define flag
				(and
					(or
						((id2pred rhs) __et) ...
					)
					(expanded-check __et)))
				;(if flag flag (begin (print name) (display "\n")))
				flag)
		]
	)
)

(define-syntax-rule (RHS-C-List name (lname : lhs))
	(struct name (lname) #:transparent
		#:methods gen:expanded
		[
			(define (expanded-check __et)
				(and
					(andmap (id2pred lhs) ((id2acc name lname) __et))
					(andmap ast-check ((id2acc name lname) __et))))
		]
	)
)

	

(define-syntax-rule (RHS-C name (lname : lhs) ...)
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
)

;[!] users should define their own enumerator for each terminal
;	 name-enum: ctxt -> name
(define-syntax-rule (TERM name val ...)
	(struct name (val ...) #:transparent
		#:methods gen:ast
		[
			(define (ast-check __t) 
				#t)
		]
	)
)

(define-syntax-rule (LHS-E name -> (name-enum ::= rhs-enum ...))
	(define (name-enum ctxt depth-limit)
		(if (> depth-limit 0)
			(let ([rhs-list (list (rhs-enum ctxt (- depth-limit 1)) ... )])
			(name (apply choose* rhs-list)))
			(invalid 0))))

(define-syntax-rule (RHS-E name -> name-enum (lhs-enum ...))
	(define (name-enum ctxt depth-limit) 
		(name (lhs-enum ctxt depth-limit) ... )))
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

(struct invalid (any) #:transparent
		#:methods gen:ast
		[(define (ast-check __t) #f)])

(define (invalid-enum ctxt depth-limit) (invalid -1))
;==================================================


