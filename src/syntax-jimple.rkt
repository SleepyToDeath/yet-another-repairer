#lang rosette/safe

(require "syntax.rkt")
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;============= Syntax Definition & Check =============
;main function should be named "main"
(LHS-C program (rhs ::= program-def))
	(RHS-C program-def (globals : variable-declares) (functions : function-declares) (members : member-declares))

;--------------------------------------------------------
(LHS-C function-declares (rhs ::= function-list))
	(RHS-C-List function-list (fl : function-declare))

(LHS-C member-declares (rhs ::= member-list))
	(RHS-C-List member-list (ml : field))

(LHS-C variable-declares (rhs ::= variable-list))
	(RHS-C-List variable-list (vl : variable-init))

;--------------------------------------------------------
(LHS-C function-declare (rhs ::= function-content))
	(RHS-C function-content (name : function-name) (args : arguments) (local-variables : variable-declares) (statements : stats))

;--------------------------------------------------------
(LHS-C stats (rhs ::= stat-list))
	(RHS-C-List stat-list (sl : stat))

(LHS-C stat (rhs ::= stat-ass stat-jmp stat-label stat-static-call stat-virtual-call stat-nop stat-ret))
	(RHS-C stat-ass (lvalue : lexpr) (rvalue : expr))
	(RHS-C stat-jmp (condition : expr) (target : label))
	(RHS-C stat-label (name : label))
	(RHS-C stat-static-call (ret : variable) (func : function-name) (args : arguments))
	(RHS-C stat-virtual-call (ret : variable) (obj : variable) (func : function-name) (args : arguments))
	(RHS-C stat-nop (any : nop))
	(RHS-C stat-ret (v : variable))

(LHS-C lexpr (rhs ::= expr-var expr-array expr-field))
(LHS-C dexpr (rhs ::= expr-var expr-const))
(LHS-C expr (rhs ::= expr-const expr-var expr-binary))
	(RHS-C expr-const (value : const))
	(RHS-C expr-var (name : variable))
	(RHS-C expr-binary (operand1 : expr) (operator : op) (operand2 : expr))
	(RHS-C expr-array (array : variable) (index : expr))
	(RHS-C expr-field (obj : variable) (fname : field))

(LHS-C variable-init (rhs ::= variable-no-value variable-with-value))
	(RHS-C variable-with-value (vn : variable) (vv : expr))
	(RHS-C variable-no-value (vn : variable))

(LHS-C arguments (rhs ::= argument-list))
	(RHS-C-List argument-list (al : variable))

;--------------------------------------------------------
(TERM function-name name)
(TERM field name)
(TERM variable name)
(TERM const v)
(TERM label v)
(TERM op v)
(TERM nop any)
;=====================================================



;=================== Enumerators =====================
;(LHS-E stats -> (stats-enum ::= stats-multi-enum stats-single-enum))
;	(RHS-E stats-multi -> stats-multi-enum (stats-enum stats-enum))
;	(RHS-E stats-single -> stats-single-enum (stat-enum))

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
		[(stats rhs) (ast-print rhs)]
		[(stat-list sl)
			(map (lambda (st) (print st) #t) sl)]))
;=====================================================

