#lang rosette/safe

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require "match-define.rkt")
(require "string-id.rkt")
(require "formula.rkt")
(require "map.rkt")

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "jimple/operators.rkt")
(require "jimple/jimple-parser.rkt")
(require "type-checker.rkt")

(require "memory-common.rkt")
(require "semantics-common.rkt")
(require "semantics-computational.rkt")

(provide (all-defined-out))

;======================== Helper & Config ======================
(define (list-replace l pred e)
	(map (lambda (e__) (if (pred e__) e e__)) l))

(define (list-insert l pos v)
	(append (take l pos) (list v) (drop l pos)))

(define search-depth 3)
(define inf-depth 100)

(define bridge-var-name (string-id "__bridge__"))


;======================== Enumerator ======================
;verifier: check if a completed program satisfies spec
;pruner: check if a partial program can be eliminated early
;updater: update context by current ast
(define (ast-dfs ast ctxt verifier pruner updater depth)
	(pretty-print ast)
	(if (not (pruner ast)) 
		(begin (display "pruned\n") #f)
		(if (ast-check ast)
			;finished
			(verifier ast)
			(begin
			(define maybe-asts (ast-expand-next ctxt ast depth))
			;unfinished but can't expand within depth limit
			(if (null? maybe-asts) 
				(begin (display "out of depth bound\n") #f)
				;unfinished and can be expanded
				(ormap (lambda (ast+) (ast-dfs ast+ (updater ctxt ast+) verifier pruner updater depth)) maybe-asts))))))


;======================== Early Checker  ======================
;(struct syntax-context (vars types fields funcs consts ops labels def-list-gen) #:transparent)
;(struct location (class func line inst selector) #:transparent)
(define (location->ctxt ast bugl mac)
	(define func (location-func bugl))
	(define fname (function-name func))
	(define cls (location-class bugl))
	(define cname (class-name cls))
	(define line-mac (location-line bugl))
	(define line-ast (if (equal? fname func-name-init) (- line-mac 1) line-mac))

	(define vars (cons bridge-var-name (map car (function-locals func))))
	(define types null)
	(define fields (map (lambda (fname) (cons cname fname)) 
		(map car (append (class-vfields cls) (class-sfields cls)))))
	(define funcs null)
	(define consts (list 0 1 2 3))
	(define ops (list bvand bvor bvxor op-mod op-cmp equal? op-neq op-gt op-ge op-lt op-le bvlshr op-add op-sub op-mul op-div))
	; don't change jump target
	(define labels 
		(match (list-ref (function-prog func) line-mac)
			[(inst-jmp condition label) (list label)]
			[_ null]))
	(syntax-context vars types fields funcs consts ops labels identity))

;ast: enumerated statement
;set default argument list by function name
(define (real-context-updater ctxt ast mac)
	(define func (ast->func ast mac))
	(if (not func) ctxt
		(begin
		(define empty-arg-list (map (lambda (x) #f) (function-args func)))
		(define dummy-arg-list (map (lambda (x) (type-name "int")) (function-args func)))
		(define (def-list-gen node)
			(match ast
				[type-list dummy-arg-list]
				[argument-caller-list empty-arg-list]))
		(std:struct-copy syntax-context ctxt [def-list-gen def-list-gen]))))


(define (real-pruner ast mac)
	(define func (ast->func ast mac))
	(define (arg-type-checker)
		#t)
	(define (ret-type-checker)
		#t)
	(and 
		(arg-type-checker)
		(ret-type-checker)))


(define (ast->func ast mac)

	(define (find-vfunc mac cname fname)
		(ormap (lambda (cls) 
			(if (not (equal? cname (class-name cls))) #f 
				(findf (lambda (f) 
					(equal? fname (function-name f)))
					(class-vfuncs cls))))
			(machine-classes mac)))

	(define (find-sfunc mac cname fname)
		(ormap (lambda (cls) 
			(if (not (equal? cname (class-name cls))) #f 
				(findf (lambda (f) 
					(equal? fname (function-name f)))
					(class-sfuncs cls))))
			(machine-classes mac)))

	(match ast
		[(stat s) (ast->func s mac)]
		[(stat-calls s) (ast->func s mac)]

		[(stat-static-call ret cls-name func arg-types args) 
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) #f
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(define fname (string-id (func-name-name func)))
				(find-sfunc mac cname fname)))]

		[(stat-virtual-call ret obj cls-name func arg-types args)
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) #f
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(define fname (string-id (func-name-name func)))
				(find-vfunc mac cname fname)))]

		[(stat-special-call ret obj cls-name func arg-types args)
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) #f
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(define fname (string-id (func-name-name func)))
				(find-sfunc mac cname fname)))]

		[_ #f]))


;======================== Spec Checker ======================
;[TODO] check no recursion

;insert before current `newl`
(define (insert-stat ast stat-sketch newl)
	(define func (location-func newl))
	(define fname (function-name func))
	(define line (if (equal? fname func-name-init) (- (location-line newl) 1) (location-line newl)))
	(update-prog ast newl
		(lambda (ast) 
			(update-func ast newl 
				(lambda (ast) (stats (stat-list (list-insert ast line stat-sketch))))))))

(define (replace-stat ast stat-sketch bugl)
	(define func (location-func bugl))
	(define fname (function-name func))
	(define line (if (equal? fname func-name-init) (- (location-line bugl) 1) (location-line bugl)))
	(update-prog ast bugl
		(lambda (ast) 
			(update-func ast bugl 
				(lambda (ast) (stats (stat-list (std:list-set ast line stat-sketch))))))))

(define (update-func funcs-ast loc lupdater)
	(define func (location-func loc))
	(define fname (function-name func))

	(define (is-target-func? func-ast)
		(equal? fname (string-id (func-name-name (function-content-name (function-declare-rhs func-ast))))))

	(define maybe-func-ast (findf is-target-func? funcs-ast))
	(if (not maybe-func-ast) funcs-ast
		(begin
		(define func-ast maybe-func-ast)
		(define insts-ast (stat-list-sl (stats-rhs (function-content-statements (function-declare-rhs func-ast)))))
		(define insts-ast-sketch (lupdater insts-ast)) 
		(define func-ast-sketch (function-declare (std:struct-copy function-content (function-declare-rhs func-ast) [statements insts-ast-sketch])))
		(list-replace funcs-ast is-target-func? func-ast-sketch))))


(define (update-prog ast loc fupdater)
	(define cls (location-class loc))
	(define cname (class-name cls))

	;shorthands
	(define (is-target-cls? cls-ast)
		(equal? cname (string-id (type-name-name (class-default-name (class-def-rhs cls-ast))))))

	;find class in AST
	(define clss-ast (class-list-cl (program-rhs ast)))
	(define cls-ast (findf is-target-cls? clss-ast))
	(define vfuncs-ast (function-list-fl (function-declares-rhs (class-default-virtual-functions (class-def-rhs cls-ast)))))
	(define sfuncs-ast (function-list-fl (function-declares-rhs (class-default-static-functions (class-def-rhs cls-ast)))))

	(define cls-ast-sketch (class-def (std:struct-copy class-default (class-def-rhs cls-ast) 
		[virtual-functions (function-declares (function-list (fupdater vfuncs-ast)))]
		[static-functions (function-declares (function-list (fupdater sfuncs-ast)))])))
	(define clss-ast-sketch (list-replace clss-ast is-target-cls? cls-ast-sketch))

	(program (class-list clss-ast-sketch)))


(define (machine-type-check? mac)
	(define (func-type-check? func)
		(define (inst-type-check? inst)
;			(display "type checking instruction: ")
;			(pretty-print inst)
			;return expression's type if type checks, #f otherwise
			(define (expr-type-check? expr) 
				(match expr
					[(iexpr-const value type) type]
					[(iexpr-var name)
						(if (equal? name var-this-name) (string-id "void") (lookup-type name func))]
					[(iexpr-binary op expr1 expr2)
						(begin
						(define t1 (expr-type-check? expr1))
						(define t2 (expr-type-check? expr2))
;						(pretty-print (list t1 t2))
						(define op-check? (and t1 t2 (op-type-check? op t1 t2)))
						(if op-check? (op-return-type op t1 t2) #f))]
					[(iexpr-array arr-name index)
						(lookup-type arr-name func)]
					[(iexpr-field obj-name cls-name fname)
;						(pretty-print (sfield-id cls-name fname))
						(if (and (not (equal? obj-name (string-id (variable-name void-receiver)))) (not (is-a? (lookup-type obj-name func) cls-name mac))) #f
							(imap-get (machine-tmap mac) (sfield-id cls-name fname) default-type))]))

			(match inst
				[(inst-nop _) #t]
				[(inst-init classname) #t]
				[(inst-newarray v-name size-expr) #t]
				[(inst-new v-name) #t]
				[(inst-ret v-expr) 
					(begin
					(define rt (expr-type-check? v-expr))
					(if (not rt) #f
						(is-a? rt (function-ret func) mac)))]
				[(inst-long-jump cls-name func-name) #t]
				[(inst-static-call ret cls-name func-name arg-types args) 
					(andmap (lambda (at aexpr)
						(define at+ (expr-type-check? aexpr))
						(and at+ (is-a? at+ at mac)))
						arg-types
						args)]
				[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
					(andmap (lambda (at aexpr)
						(define at+ (expr-type-check? aexpr))
						(and at+ (is-a? at+ at mac)))
						arg-types
						args)]
				[(inst-special-call ret obj-name cls-name func-name arg-types args)
					(andmap (lambda (at aexpr)
						(define at+ (expr-type-check? aexpr))
						(and at+ (is-a? at+ at mac)))
						arg-types
						args)]
				[(inst-ass vl vr)  
					(begin
					(define lt (expr-type-check? (ast->expression vl)))
					(define rt (expr-type-check? vr))
;					(pretty-print (list lt rt))
					(and lt rt (is-a? lt rt mac)))]
				[(inst-switch cnd cases default-l) #t]
				[(inst-jmp condition label) 
					(expr-type-check? condition)]))
		(andmap (lambda (i) (if (inst-type-check? i) #t (begin (display "Type error: ") (pretty-print i) #f))) (function-prog func)))
	(andmap func-type-check? (all-functions mac)))
