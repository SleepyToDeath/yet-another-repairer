#lang rosette/safe

(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "match-define.rkt")
(require "semantics-computational.rkt")
(require "memory-common.rkt")
(require "semantics-common.rkt")
(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

(define (print-location l)
	(pretty-print
		(match l
			[(location cls func line inst selector)
				(location (id->string (class-name cls)) (id->string (function-name func)) line (inst-restore-strings inst) selector)])))

(define (eprint-location l)
	(eprintf
		(pretty-format (match l
			[(location cls func line inst selector)
				(location (id->string (class-name cls)) (id->string (function-name func)) line (inst-restore-strings inst) selector)])))
	(eprintf "\n"))

(define (ast-restore-strings ast)
	(ast-traverser ast (lambda (ast val default)
		(if (not (or (std:string? val) (number? val) (pair? val))) default
			(if (or (const? ast) (std:string? val)) val
				(if (number? val) (id->string val)
					(id->string (car val))))))))

(define (inst-restore-strings inst)
	(define (expr-restore-strings expr) 
		(match expr
			[(iexpr-const value type) (iexpr-const value (id->string type))]
			[(iexpr-var name) (iexpr-var (id->string name))]
			[(iexpr-binary op expr1 expr2) (iexpr-binary op (expr-restore-strings expr1) (expr-restore-strings expr2))]
			[(iexpr-array arr-name index) (iexpr-array (id->string arr-name) (expr-restore-strings index))]
			[(iexpr-field obj-name cls-name fname) (iexpr-field (id->string obj-name) (id->string cls-name) (id->string fname))]))

	(match inst
		[(inst-nop _) inst]
		[(inst-init classname) (inst-init (id->string classname))]
		[(inst-newarray v-name size-expr) (inst-newarray (id->string v-name) (expr-restore-strings size-expr))]
		[(inst-new v-name) (inst-new (id->string v-name))]
		[(inst-ret v-expr) (inst-ret (expr-restore-strings v-expr))]
		[(inst-long-jump cls-name func-name) inst]
		[(inst-static-call ret cls-name func-name arg-types args) 
			(inst-static-call (id->string ret) (id->string cls-name) (id->string func-name) 
				(map id->string arg-types) (map expr-restore-strings args))]
		[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
			(inst-virtual-call (id->string ret) (id->string obj-name) (id->string cls-name) (id->string func-name) 
				(map id->string arg-types) (map expr-restore-strings args))]
		[(inst-special-call ret obj-name cls-name func-name arg-types args)
			(inst-special-call (id->string ret) (id->string obj-name) (id->string cls-name) (id->string func-name) 
				(map id->string arg-types) (map expr-restore-strings args))]
		[(inst-ass vl vr) (inst-ass (ast-restore-strings vl) (expr-restore-strings vr))]
		[(inst-switch cnd cases default-l) (inst-switch (expr-restore-strings cnd) cases default-l)]
		[(inst-jmp condition label) (inst-jmp (expr-restore-strings condition) label)]))

(set-formatter! inst-restore-strings)
