#lang rosette/safe

(require "syntax.rkt")
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

(define bool-type-name "bool")
(define int-type-name "int")
(define string-type-name "string")

;============= Syntax Definition & Check =============
;main function should be named "main"
(LHS-C program (rhs ::= class-list))
	(RHS-C-List class-list (cl : class-def))

;if no extend, put an #f in name string
;if no interface, put a null list
(LHS-C class-def (rhs ::= class-default))
	(RHS-C class-default (name : type-name) (extend : type-name) (implements : interface-implements) 
		(globals : field-declares) (fields : field-declares) 
		(static-functions : function-declares) (member-functions : function-declares))

;--------------------------------------------------------
(LHS-C function-declares (rhs ::= function-list))
	(RHS-C-List function-list (fl : function-declare))

(LHS-C field-declares (rhs ::= field-list))
	(RHS-C-List field-list (fl : field))

(LHS-C variable-declares (rhs ::= variable-list))
	(RHS-C-List variable-list (vl : variable))

(LHS-C interface-implements (rhs ::= interface-name-list))
	(RHS-C-List interface-name-list (il : type-name))

(LHS-C arguments-caller (rhs ::= argument-caller-list))
	(RHS-C-List argument-caller-list (al : dexpr))

(LHS-C variable-definitions (rhs ::= variable-definition-list))
	(RHS-C-List variable-definition-list (vl : variable-definition))

(LHS-C types (rhs ::= type-list))
	(RHS-C-List type-list (tl : type-name))

;--------------------------------------------------------
(LHS-C function-declare (rhs ::= function-content))
	(RHS-C function-content (name : func-name) (args : variable-definitions) (local-variables : variable-definitions) (statements : stats))

;--------------------------------------------------------
(LHS-C stats (rhs ::= stat-list))
	(RHS-C-List stat-list (sl : stat))

(LHS-C stat (rhs ::= stat-ass stat-jmp stat-label stat-static-call stat-virtual-call stat-special-call stat-nop stat-ret stat-new stat-newarray))
	(RHS-C stat-new (v : variable))
	(RHS-C stat-newarray (v : variable) (size : dexpr))
	(RHS-C stat-ass (lvalue : lexpr) (rvalue : expr))
	(RHS-C stat-jmp (condition : expr) (target : label))
	(RHS-C-List stat-switch (cases : stat-case))
	(RHS-C stat-label (name : label))
	(RHS-C stat-static-call (ret : variable) (class : type-name) (func : func-name) (arg-types : types) (args : arguments-caller))
	(RHS-C stat-virtual-call (ret : variable) (obj : variable) (class : type-name) (func : func-name) (arg-types : types) (args : arguments-caller))
	(RHS-C stat-special-call (ret : variable) (obj : variable) (class : type-name) (func : func-name) (arg-types : types) (args : arguments-caller))
	(RHS-C stat-nop (any : nop))
	(RHS-C stat-ret (v : dexpr))

(LHS-C stat-case (rhs ::= case-br case-default))
	(RHS-C case-br (k : const) (l : label))
	(RHS-C case-default (l : label))

(LHS-C lexpr (rhs ::= expr-var expr-array expr-field))
(LHS-C dexpr (rhs ::= expr-var expr-const))
(LHS-C expr (rhs ::= expr-const expr-var expr-binary expr-array expr-field))
	(RHS-C expr-const (value : const))
	(RHS-C expr-var (name : variable))
	(RHS-C expr-binary (operand1 : expr) (operator : op) (operand2 : expr))
	(RHS-C expr-array (array : variable) (index : expr))
	(RHS-C expr-field (obj : variable) (class : type-name) (fname : field))

;"int" "string" "real" "bool" for primitive type names
(LHS-C variable-definition (rhs ::= variable-n-type))
	(RHS-C variable-n-type (name : variable) (type : type-name))
;--------------------------------------------------------
(TERM type-name name)
(TERM func-name name)
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
	(RHS-E stat-ass -> stat-ass-enum (lexpr-enum expr-enum))
;	(RHS-E stat-jmp -> stat-jmp-enum (expr-enum label-enum))
;	(RHS-E stat-label -> stat-label-enum (label-enum))


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

(LHS-E lexpr -> (lexpr-enum ::= expr-field-enum expr-var-enum))
(LHS-E expr -> (expr-enum ::= expr-field-enum expr-array-enum expr-const-enum expr-var-enum expr-binary-enum))
;(LHS-E expr -> (expr-enum ::= expr-const-enum expr-var-enum))
	(RHS-E expr-array -> expr-array-enum (variable-enum expr-enum))
	(RHS-E expr-field -> expr-field-enum (variable-enum type-name-enum field-enum))
	(RHS-E expr-const -> expr-const-enum (const-enum))
	(RHS-E expr-var -> expr-var-enum (variable-enum))
	(RHS-E expr-binary -> expr-binary-enum (expr-enum op-enum expr-enum))

;[TODO] implement
(define (variable-enum ctxt depth-limit)
	(if (< depth-limit 0) (invalid 0)
		(if (empty? (syntax-context-vars ctxt)) (invalid 0)
			(variable (apply choose* (syntax-context-vars ctxt))))))

(define (type-name-enum ctxt depth-limit)
	(if (< depth-limit 0) (invalid 0)
		(if (empty? (syntax-context-types ctxt)) (invalid 0)
			(type-name (apply choose* (syntax-context-types ctxt))))))

(define (field-enum ctxt depth-limit)
	(if (< depth-limit 0) (invalid 0)
		(if (empty? (syntax-context-fields ctxt)) (invalid 0)
			(field (apply choose* (syntax-context-fields ctxt))))))

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
(define (println++ title any)
	(begin
		(display title)
		(println any)))

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

