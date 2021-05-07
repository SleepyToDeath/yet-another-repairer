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

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "jimple/operators.rkt")

(require "semantics-common.rkt")

(provide (all-defined-out))

(define (list-replace l pred e)
	(map (lambda (e__) (if (pred e__) e e__)) l))

(define (list-insert l pos v)
	(append (take l pos) (list v) (drop l pos)))

(define search-depth 3)

;(struct syntax-context (vars types fields funcs consts ops labels def-list-gen) #:transparent)
;(struct location (class func line inst selector) #:transparent)
(define (location->ctxt ast bugl mac)
	(define func (location-func bugl))
	(define fname (function-name func))
	(define cls (location-class bugl))
	(define cname (class-name cls))
	(define line-mac (location-line bugl))
	(define line-ast (if (equal? fname func-name-init) (- line-mac 1) line-mac))

	(define vars (map car (function-locals func)))
	(define types null)
	(define fields (map (lambda (fname) (cons cname fname)) 
		(map car (append (class-vfields cls) (class-sfields cls)))))
	(define funcs null)
	(define consts (list 0 1 2 3))
	(define ops (list bvand bvor bvxor op-mod op-cmp equal? op-neq op-gt op-ge op-lt op-le bvlshr op-add op-sub op-mul op-div))
	; don't change jump target
	(define labels 
		(match (list-ref (function-program func) line-mac)
			[(inst-jmp condition label) (list label)]
			[_ null]))
	(syntax-context vars types fields funcs consts ops labels context-updater))

;ast: enumerated statement
;set default argument list by function name
(define (context-updater ast mac)
	(define func (ast->func ast mac))
	(define empty-arg-list (map (lambda (x) #f) (function-args func)))
	(define dummy-arg-list (map (lambda (x) (type-name "int")) (function-args func)))
	(define (def-list-gen node)
		(match ast
			[type-list dummy-arg-list]
			[argument-caller-list empty-arg-list])))


(define (pruner ast mac extra)
	(define func (ast-> func ast mac))
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
			(if (not (equal? cname (class-name name))) #f 
				(findf (lambda (f) 
					(if (not (equal? fname (function-name f)) #f f)) 
					(class-vfuncs cls))) 
			(machine-classes mac))))

	(define (find-sfunc mac cname fname)
		(ormap (lambda (cls) 
			(if (not (equal? cname (class-name name))) #f 
				(findf (lambda (f) 
					(if (not (equal? fname (function-name f)) #f f)) 
					(class-sfuncs cls))) 
			(machine-classes mac))))

	(match ast
		[(stat s) (ast->func s)]
		[(stat-call s) (ast->func s)]

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

	


;insert before current `newl`
(define (insert-stat ast stat-sketch newl)
	(define func (location-func newl))
	(define fname (function-name func))
	(define line (if (equal? fname func-name-init) (- (location-line newl) 1) (location-line newl)))
	(update-prog ast
		(lambda (ast) 
			(update-func ast newl 
				(lambda (ast) (stats (stat-list (list-insert ast line stat-sketch))))))))

(define (replace-stat ast stat-sketch bugl)
	(define func (location-func bugl))
	(define fname (function-name func))
	(define line (if (equal? fname func-name-init) (- (location-line bugl) 1) (location-line bugl)))
	(update-prog ast
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


;verifier: check if a completed program satisfies spec
;pruner: check if a partial program can be eliminated early
;updater: update context by current ast
(define (ast-dfs ast ctxt verifier pruner updater depth)
	(if (not (pruner ast)) #f
		(if (ast-check ast)
			;finished
			(verifier ast)
			(begin
			(define maybe-asts (ast-expand-next ctxt ast depth))
			;unfinished but can't expand within depth limit
			(if (null? maybe-asts) #f 
				;unfinished and can be expanded
				(ormap (lambda (ast+) (ast-dfs ast+ (updater ctxt ast+) verifier pruner updater depth)) maybe-asts))))))

