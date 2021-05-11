#lang rosette/safe

(require (for-syntax racket/match racket/syntax))
(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;=================== AST interface ================
(define-generics ast
	[ast-expand-next ctxt ast depth]
	[ast-check ast]
	[ast-get ast]) ;get rhs

(define-generics expanded
	[expanded-expand-next ctxt expanded depth]
	[expanded-check expanded]
	[expanded-get count expanded]) ;get first `count` sub-tree

(struct syntax-context (vars types fields funcs consts ops labels def-list-gen) #:transparent)
;==================================================



;============ Syntax Defining Macros ==============
(define-syntax-rule (LHS-C name ( rname ::= rhs ... ))
	(struct name (rname) #:transparent
		#:methods gen:ast
		[ 
			(define (ast-check __ast)
				(define __et ((id2acc name rname) __ast))
				(define flag
				(and
					(or
						((id2pred rhs) __et) ...
					)
					(expanded-check __et)))
;				(if flag flag (begin (print name) (display "\n")))
				flag)

			(define (ast-get __ast)
				((id2acc name rname) __ast))

			(define (ast-expand-next __ctxt __ast __depth)
				(if (= __depth 0) null
					(if ((id2acc name rname) __ast)
						(map (lambda (rhs+) (name rhs+)) (expanded-expand-next __ctxt ((id2acc name rname) __ast) __depth))
						(map (lambda (rhs+) (name (lookup-default-rhs rhs+))) (lookup-default-lhs name)))))
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

			(define (expanded-expand-next __ctxt __ast __depth)
				(define __ast+ 
					(if ((id2acc name lname) __ast)
						__ast
						(name ((syntax-context-def-list-gen __ctxt) name))))
				(foldl 
					(lambda (__index __lst)
						(define (__expander __ast0)
							(define __lhsl ((id2acc name lname) __ast0))
							(define __lhs0 (list-ref __lhsl __index))
							(if __lhs0
								(map 
									(lambda (__lhs+) (std:struct-copy name __ast0 [lname (std:list-set __lhsl __index __lhs+)])) 
									(ast-expand-next __ctxt __lhs0 (- __depth 1)))
								(map 
									(lambda (__lhs+) (std:struct-copy name __ast0 [lname (std:list-set __lhsl __index __lhs+)])) 
									(ast-expand-next __ctxt (lhs #f) (- __depth 1)))))
						(if (equal? (length __lst) 1) 
							(__expander (car __lst))
							__lst))
					(list __ast+)
					(std:range (length ((id2acc name lname) __ast+)))))
		]
	)
)

	
;(define-syntax-rule (RHS-D name (lname : lhs) ...)
;	(add-default-rhs name (name (lname (lhs #f)) ...)))

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

			(define (expanded-expand-next __ctxt __ast __depth)
				(foldl (lambda (__expander __lst)
					(if (equal? (length __lst) 1) 
						(__expander (car __lst))
						__lst))
					(list __ast)
					(list
						(lambda (__ast0) 
							(define __lhs0 ((id2acc name lname) __ast0))
							(if __lhs0
								(map 
									(lambda (__lhs+) (std:struct-copy name __ast0 [lname __lhs+])) 
									(ast-expand-next __ctxt __lhs0 (- __depth 1)))
								(map 
									(lambda (__lhs+) (std:struct-copy name __ast0 [lname __lhs+])) 
									(ast-expand-next  __ctxt (lhs #f) (- __depth 1)))))
						...)))
		])
)

;[!] users should define their own enumerator for each terminal
;	 name-enum: ctxt -> name
(define-syntax-rule (TERM name val)
	(struct name (val) #:transparent
		#:methods gen:ast
		[
			(define (ast-check __ast) 
				((id2acc name val) __ast))

			(define (ast-get __ast)
				((id2acc name val) __ast))

			(define (ast-expand-next __ctxt __ast __depth)
				(if (< __depth 0) null
					(if ((id2acc name val) __ast) (list __ast)
						((id2enum name) __ctxt __depth))))
		]
	)
)

(define-syntax-rule (LHS-E name ( rname ::= rhs ... ))
	(add-lhs-options name (list rhs ...)))

(define-syntax-rule (RHS-E name (lname : lhs) ...)
	(add-default-rhs name (name (lhs #f) ...)))
;=================================================


;==================== Enumerator Helpers ====================
(define lhs-options-list null)
(define (add-lhs-options lhs rhs-list)
	(set! lhs-options-list (cons (cons lhs rhs-list) lhs-options-list)))
(define (lookup-default-lhs lhs)
	(cdr (findf (lambda (entry) (equal? (car entry) lhs)) lhs-options-list)))

(define default-rhs-list null)
(define (add-default-rhs rhs ast-def)
	(set! default-rhs-list (cons (cons rhs ast-def) default-rhs-list)))
(define (lookup-default-rhs rhs)
	(cdr (findf (lambda (entry) (equal? (car entry) rhs)) default-rhs-list)))


;==================== Syntax Helpers ====================
(define-syntax (id2enum stx)
	(define id (cadr (syntax-e stx)))
	(define enum (format-symbol "~a-enum" (syntax->datum id)))
	(datum->syntax id enum))

(define-syntax (id2pred stx)
	(define id (cadr (syntax-e stx)))
	(define pred (format-symbol "~a?" (syntax->datum id)))
	(datum->syntax id pred))

(define-syntax (id2self stx)
	(define id-s (cadr (syntax-e stx)))
	(define id-f (caddr (syntax-e stx)))
	(datum->syntax id-s (syntax->datum id-f)))

(define-syntax (id2acc stx)
	(define id-s (cadr (syntax-e stx)))
	(define id-f (caddr (syntax-e stx)))
	(define acc (format-symbol "~a-~a" (syntax->datum id-s) (syntax->datum id-f)))
	(datum->syntax id-f acc))

(struct invalid (any) #:transparent
		#:methods gen:ast
		[(define (ast-check __ast) #f)])

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
