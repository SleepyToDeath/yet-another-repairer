#lang rosette/safe

(require "syntax.rkt")

;================== Boilerplate =================
(define-syntax-rule (LHS name ( rname ::= rhs ... ))
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
;=================================================



(LHS stat (rhs ::= stat-ass stat-jmp stat-label))
	(RHS stat-ass (target : variable) (value : expr))
	(RHS stat-jmp (condition : expr) (target : label))
	(RHS stat-label (name : label))

(LHS expr (rhs ::= expr-const expr-var expr-binary))
	(RHS expr-const (value : const))
	(RHS expr-var (name : variable))
	(RHS expr-binary (operand1 : expr) (operator : op) (operand2 : expr))

(TERM variable v)
(TERM const v)
(TERM label v)
(TERM op v)

