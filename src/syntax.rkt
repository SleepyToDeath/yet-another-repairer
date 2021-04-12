#lang rosette/safe

(require (for-syntax racket/match racket/syntax))

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;=================== AST interface ================
(define-generics ast
	[ast-check ast]
	[ast-get ast]) ;get rhs

(define-generics expanded
	[expanded-check expanded]
	[expanded-get count expanded]) ;get first `count` sub-tree
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
				(if flag flag (begin (print name) (display "\n")))
				flag)

			(define (ast-get __t)
				((id2acc name rname) __t))
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

			(define (expanded-get __lvl __et)
				(list ((id2acc name lname) __et)))
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

			(define (expanded-get __lvl __et)
				(define __elements (list ((id2acc name lname) __et) ...))
				(take __elements __lvl))
		] 
	)
)

;[!] users should define their own enumerator for each terminal
;	 name-enum: ctxt -> name
(define-syntax-rule (TERM name val)
	(struct name (val) #:transparent
		#:methods gen:ast
		[
			(define (ast-check __t) 
				#t)

			(define (ast-get __t)
				((id2acc name val) __t))
		]
	)
)

#|
(define-syntax-rule (LHS-E name -> (name-enum ::= rhs-enum ...))
	(define (name-enum ctxt depth-limit)
		(if (> depth-limit 0)
			(begin
			(define rhs-list (list (rhs-enum ctxt (- depth-limit 1)) ...))

			)
			null)))

(define-syntax-rule (RHS-E name -> name-enum (lhs-enum ...))
	(define (name-enum ctxt depth-limit) 
		(append (name (lhs-enum ctxt depth-limit) ... )))
	|#

(define-syntax-rule (LHS-E name -> (name-enum ::= rhs-enum ...))
	(define (name-enum ctxt depth-limit)
		(if (> depth-limit 0)
			(letrec ([rhs-list (list (rhs-enum ctxt (- depth-limit 1)) ... )]
				 	 [rhs-list-valid 
					 	(foldl (lambda (rhs l) (if (expanded-check rhs) (cons rhs l) l))
							null
							rhs-list)])
					(name (if (empty? rhs-list-valid) (invalid 0) (apply choose* rhs-list-valid))))
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



;=================== Utils ========================
;avoid typing the very long syntax node name when reading contents
;used if RHS has one element
(define (syntax-unwrap lvl ast)
	(if (zero? lvl)
		ast
		(syntax-unwrap (- lvl 1) (if (ast? ast) (ast-get ast) (car (expanded-get 1 ast))))))

;used if RHS has two elements
(define (syntax-unwrap2 lvl ast)
	(define p (expanded-get 2 (syntax-unwrap (- lvl 1) ast)))
	(cons (first p) (second p)))

;used if RHS has three elements
(define (syntax-unwrap3 lvl ast)
	(expanded-get 3 (syntax-unwrap (- lvl 1) ast)))

;used if RHS has four elements
(define (syntax-unwrap4 lvl ast)
	(expanded-get 4 (syntax-unwrap (- lvl 1) ast)))
;==================================================
