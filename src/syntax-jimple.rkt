#lang rosette/safe

(require "syntax.rkt")
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;============= Syntax Definition & Check =============
(LHS-C program (rhs ::= program-def))
	(RHS-C program-def (gv : globals) (fc : functions) (mm : members)

(LHS-C globals

(LHS-C functions

(LHS-C members

(LHS-C stats (rhs ::= stats-list))
	(RHS-C-List stats-list (st : stat))

(LHS-C stat (rhs ::= stat-ass stat-jmp stat-label stat-nop stat-ret))
	(RHS-C stat-ass (lvalue : variable) (rvalue : expr))
	(RHS-C stat-jmp (condition : expr) (target : label))
	(RHS-C stat-label (name : label))
	(RHS-C stat-nop (any : nop))
	(RHS-C stat-ret (any : nop))

(LHS-C expr (rhs ::= expr-const expr-var expr-binary))
	(RHS-C expr-const (value : const))
	(RHS-C expr-var (name : variable))
	(RHS-C expr-binary (operand1 : expr) (operator : op) (operand2 : expr))

(TERM variable v)
(TERM const v)
(TERM label v)
(TERM op v)
(TERM nop any)
;=====================================================



;=================== Enumerators =====================
(LHS-E stats -> (stats-enum ::= stats-multi-enum stats-single-enum))
	(RHS-E stats-multi -> stats-multi-enum (stats-enum stats-enum))
	(RHS-E stats-single -> stats-single-enum (stat-enum))

(LHS-E stat -> (stat-enum ::= stat-ass-enum))
	(RHS-E stat-ass -> stat-ass-enum (variable-enum expr-enum))
	(RHS-E stat-jmp -> stat-jmp-enum (expr-enum label-enum))
	(RHS-E stat-label -> stat-label-enum (label-enum))


;(define (expr-enum ctxt depth-limit)
;	(if (> depth-limit 0)
;		(letrec
;			([c (expr-const-enum ctxt (- depth-limit 1))]
;			[v (expr-var-enum ctxt (- depth-limit 1))]
;			[b (expr-binary-enum ctxt (- depth-limit 1))]
;			[e (expr (choose* c v b))])
;			(begin
;			(println e)
;			e)
;			)
;		(invalid 0)))

(LHS-E expr -> (expr-enum ::= expr-const-enum expr-var-enum expr-binary-enum))
	(RHS-E expr-const -> expr-const-enum (const-enum))
	(RHS-E expr-var -> expr-var-enum (variable-enum))
	(RHS-E expr-binary -> expr-binary-enum (expr-enum op-enum expr-enum))

;[TODO] implement
(define (variable-enum ctxt depth-limit)
	(if (< depth-limit 0) (invalid 0)
		(variable (apply choose* (syntax-context-vars ctxt)))))

(define (const-enum ctxt depth-limit)
	(if (< depth-limit 0) (invalid 0)
		(const (apply choose* (syntax-context-consts ctxt)))))

(define (label-enum ctxt depth-limit)
	(if (< depth-limit 0) (invalid 0)
		(label (apply choose* (syntax-context-labels ctxt)))))

(define (op-enum ctxt depth-limit)
	(if (< depth-limit 0) (invalid 0)
		(op (apply choose* (syntax-context-ops ctxt)))))
;=====================================================



;====================== Helpers ======================
(define (println any)
	(begin
		(print any)
		(display "\n")))

(define (ast-print ast)
	(match ast
		[(stats (stats-multi l r)) 
			(begin
				(ast-print l) 
				(ast-print r))]
		[(stats (stats-single s))
			(println s)]))
;=====================================================

