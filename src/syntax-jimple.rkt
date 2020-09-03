#lang rosette/safe

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require "syntax.rkt")


;================== Boilerplate =================
(define-syntax-rule (LHS-C name ( rname ::= rhs ... ))
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

(define-syntax-rule (RHS-C name (lname : lhs) ...)
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
			(name (choose* 
					(rhs-enum ctxt (- depth-limit 1)) ... ))
			(invalid 0))))

(define-syntax-rule (RHS-E name -> name-enum (lhs-enum ...))
	(define (name-enum ctxt depth-limit) 
		(name (lhs-enum ctxt depth-limit) ... )))
;=================================================





;================== Real Syntax =================
;syntax check
(LHS-C stats (rhs ::= stats-multi stats-single))
	(RHS-C stats-multi (l : stats) (r : stats))
	(RHS-C stats-single (head : stat))

(LHS-C stat (rhs ::= stat-ass stat-jmp stat-label))
	(RHS-C stat-ass (target : variable) (value : expr))
	(RHS-C stat-jmp (condition : expr) (target : label))
	(RHS-C stat-label (name : label))

(LHS-C expr (rhs ::= expr-const expr-var expr-binary))
	(RHS-C expr-const (value : const))
	(RHS-C expr-var (name : variable))
	(RHS-C expr-binary (operand1 : expr) (operator : op) (operand2 : expr))

(TERM variable v)
(TERM const v)
(TERM label v)
(TERM op v)

(TERM invalid any)


;enumerator
(LHS-E stats -> (stats-enum ::= stats-multi-enum stats-single-enum))
	(RHS-E stats-multi -> stats-multi-enum (stats-enum stats-enum))
	(RHS-E stats-single -> stats-single-enum (stat-enum))

(LHS-E stat -> (stat-enum ::= stat-ass-enum stat-jmp-enum stat-label-enum))
	(RHS-E stat-ass -> stat-ass-enum (variable-enum expr-enum))
	(RHS-E stat-jmp -> stat-jmp-enum (expr-enum label-enum))
	(RHS-E stat-label -> stat-label-enum (label-enum))

(LHS-E expr -> (expr-enum ::= expr-const-enum expr-var-enum expr-binary-enum))
	(RHS-E expr-const -> expr-const-enum (const-enum))
	(RHS-E expr-var -> expr-var-enum (variable-enum))
	(RHS-E expr-binary -> expr-binary-enum (expr-enum op-enum expr-enum))

;[TODO] implement
(define (variable-enum ctxt depth-limit) (variable 0))
(define (const-enum ctxt depth-limit) (const 0))
(define (label-enum ctxt depth-limit) (label 0))
(define (op-enum ctxt depth-limit) (op 0))

(define (invalid-enum ctxt depth-limit) (invalid 0))
;=================================================


(provide stats stat expr variable const label op invalid)
(provide stats-enum stat-enum expr-enum variable-enum const-enum label-enum op-enum invalid-enum)

;test
(stat null)

(stats-enum null 10)

