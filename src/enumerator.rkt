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
(require "model.rkt")

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

;======================== Enumerator ======================
;verifier: check if a completed program satisfies spec
;pruner: check if a partial program can be eliminated early
;updater: update context by current ast
(define (ast-dfs ast ctxt verifier pruner updater depth)
	(if (not (pruner ast)) 
		#f
		(if (ast-check ast)
			;finished
			(verifier ast)
			(begin
			(define maybe-asts (ast-expand-next ctxt ast depth))
			;unfinished but can't expand within depth limit
			(if (null? maybe-asts) 
				#f;(begin (display "out of depth bound\n") #f)
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

	(define vars (cons bridge-var-name (location->vars bugl)))
	(define types (map class-name (machine-classes mac)))
	(define fields (map (lambda (fname) (cons cname fname)) 
		(map car (append (class-vfields cls) (class-sfields cls)))))
	(define funcs (remove func-name-init (remove-duplicates (map function-name (all-functions mac)))))
	(define consts (list 1))
;	(define ops (list bvand bvor bvxor op-mod op-cmp equal? op-neq op-gt op-ge op-lt op-le bvlshr op-add op-sub op-mul op-div))
	(define ops (list equal? op-neq op-add op-sub))
;	(define ops (list op-add op-sub))
;	(define ops (list equal? op-neq))
	; don't change jump target
	(define labels 
		(match (list-ref (function-prog func) line-mac)
			[(inst-jmp condition label) (list label)]
			[_ null]))
	(syntax-context vars types fields funcs consts ops labels (lambda (x) #f) real-depth-updater))

(define var-use-threshold 2)

(define (location->vars bugl)
	(define func (location-func bugl))
	(define line (location-line bugl))
	(define insts (function-prog func))
	(define local-vars (map car (append (function-args func) (function-locals func))))
	(define (vars-at-line l)
		(if (or (< l 0) (>= l (length insts))) null
			(inst->vars (list-ref insts l))))
	(define near-vars (remove-duplicates (append (vars-at-line line) (vars-at-line (- line 1)) (vars-at-line (+ line 1)))))
	(define all-vars-bag (apply append (map inst->vars insts)))
	(define all-vars (remove-duplicates all-vars-bag))
	(define (var-count name)
		(count (lambda (name+) (equal? name name+)) all-vars-bag))
	(define frequent-vars (filter 
		(lambda (name) (> (var-count name) var-use-threshold)) 
		all-vars))
	(filter (lambda (name) (member name local-vars)) (remove-duplicates (append near-vars frequent-vars))))

(define (inst->vars inst)
	(match inst
		[(inst-newarray v-name size-expr) (list v-name)]
		[(inst-new v-name) (list v-name)]
		[(inst-ret v-expr) (iexpr->vars v-expr)]
		[(inst-static-call ret cls-name func-name arg-types args) (cons ret (apply append (map iexpr->vars args)))]
		[(inst-virtual-call ret obj-name cls-name func-name arg-types args) (cons obj-name (cons ret (apply append (map iexpr->vars args))))]
		[(inst-special-call ret obj-name cls-name func-name arg-types args) (cons obj-name (cons ret (apply append (map iexpr->vars args))))]
		[(inst-ass vl vr) (append (expr->vars vl) (iexpr->vars vr))]
		[(inst-jmp condition label) (iexpr->vars condition)]
		[(inst-switch cnd cases default-l) (iexpr->vars cnd)]
		[_ null]))

(define (expr->vars e)
	(match e
		[(lexpr rhs) (expr->vars rhs)]
		[(dexpr rhs) (expr->vars rhs)]
		[(expr rhs) (expr->vars rhs)]
		[(expr-const value) (expr->vars value)]
		[(expr-var name) (expr->vars name)]
		[(expr-binary operand1 operator operand2) (append (expr->vars operand1) (expr->vars operand2))]
		[(expr-array array index) (append (expr->vars array) (expr->vars index))]
		[(expr-field obj class fname) (expr->vars obj)]
		[(variable name) (list (string-id name))]))

(define (iexpr->vars e)
	(match e
		[(iexpr-const value type) null]
		[(iexpr-var name) (list name)]
		[(iexpr-binary op expr1 expr2) (append (iexpr->vars expr1) (iexpr->vars expr2))]
		[(iexpr-array arr-name index) (cons arr-name (iexpr->vars index))]
		[(iexpr-field obj-name cls-name fname) (list obj-name)]))

;ast: enumerated statement
;set default argument list by function name
(define (real-context-updater ctxt ast mac)
	(define func (ast->func ast mac))
	(if (or (not-a-function-error? func) (invalid-function-error? func)) ctxt
		(begin
		(define empty-arg-list (map (lambda (x) #f) (function-args func)))
		(define (def-list-gen node)
			(match node
				[(type-list _) (map type-name (map cdr (function-args func)))]
				[(argument-caller-list _) empty-arg-list]))
		(std:struct-copy syntax-context ctxt [def-list-gen def-list-gen]))))


(define (real-pruner ast mac)
	;[TODO] not necessary, but improves performance
	(define (arg-type-checker)
		#t)
	(define (invalid-func-checker)
		(define maybe-func (ast->func ast mac))
		(and 
			(monitor-reason "invalid function" (not (invalid-function-error? maybe-func)))))
;			(monitor-reason "not a function" (not (not-a-function-error? maybe-func)))))
	(define (bridge-var-checker)
		(and 
			(monitor-reason "unwanted var" (not (unwanted-bridge-var? ast)))
			(monitor-reason "missing var" (not (missing-bridge-var? ast)))))
	(and 
		(bridge-var-checker)
		(invalid-func-checker)
		(arg-type-checker)
	))


(define (real-depth-updater depth ast)
	(match ast
		[(stat-jmp _ _) (+ depth 0)]
		[(stat-ass _ _) (+ depth 0)]
		[_ depth]))

(struct invalid-function-error (msg) #:transparent)
(struct not-a-function-error (msg) #:transparent)
(define default-msg -1)

;invoke statement to the invoked function
;[TODO] return all versions
(define (ast->func ast mac)
	(define (find-vfunc mac cname fname)
		(define ret
			(ormap (lambda (cls) 
				(if (not (equal? cname (class-name cls))) #f
					(findf (lambda (f) 
						(equal? fname (function-name f)))
						(class-vfuncs cls))))
				(machine-classes mac)))
		(if ret ret (invalid-function-error -1)))

	(define (find-sfunc mac cname fname)
		(define ret
			(ormap (lambda (cls) 
				(if (not (equal? cname (class-name cls))) #f
					(findf (lambda (f) 
						(equal? fname (function-name f)))
						(class-sfuncs cls))))
				(machine-classes mac)))
		(if ret ret (invalid-function-error -1)))

	(match ast
		[(stat s) (ast->func s mac)]
		[(stat-calls s) (ast->func s mac)]

		[(stat-static-call ret cls-name func arg-types args) 
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) (not-a-function-error default-msg)
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(define fname (string-id (func-name-name func)))
				(find-sfunc mac cname fname)))]

		[(stat-virtual-call ret obj cls-name func arg-types args)
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) (not-a-function-error default-msg)
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(define fname (string-id (func-name-name func)))
				(find-vfunc mac cname fname)))]

		[(stat-special-call ret obj cls-name func arg-types args)
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) (not-a-function-error default-msg)
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(define fname (string-id (func-name-name func)))
				(find-sfunc mac cname fname)))]

		[_ (not-a-function-error default-msg)]))

(define (ast->cls ast mac)
	(match ast
		[(stat s) (ast->cls s mac)]
		[(stat-calls s) (ast->cls s mac)]

		[(stat-static-call ret cls-name func arg-types args) 
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) (not-a-function-error default-msg)
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(imap-get (machine-cmap mac) cname default-type)))]

		[(stat-virtual-call ret obj cls-name func arg-types args)
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) (not-a-function-error default-msg)
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(imap-get (machine-cmap mac) cname default-type)))]

		[(stat-special-call ret obj cls-name func arg-types args)
			(if (not (and cls-name (type-name-name cls-name) func (func-name-name func))) (not-a-function-error default-msg)
				(begin
				(define cname (string-id (type-name-name cls-name)))
				(imap-get (machine-cmap mac) cname default-type)))]

		[_ (not-a-function-error default-msg)]))

(define (ast->sid ast)
	(match (car (ast->instruction ast #f #f))
		[(inst-static-call ret cls-name func-name arg-types args) 
			(sfunc-id cls-name func-name arg-types)]
		[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
			(sfunc-id cls-name func-name arg-types)]
		[(inst-special-call ret obj-name cls-name func-name arg-types args)
			(sfunc-id cls-name func-name arg-types)]))

(define (location->sid bugl)
	(define cls (location-class bugl))
	(define func (location-func bugl))
	(sfunc-id (class-name cls) (function-name func) (map cdr (function-args func))))

(define (using-bridge-var? ast)
	(match ast
		[(stat rhs) (using-bridge-var? rhs)]
		[(stat-ass lvalue rvalue) (using-bridge-var? rvalue)]
		[(stat-ret v) (using-bridge-var? v)]
		[(stat-jmp condition target) (using-bridge-var? condition)]
		[(dexpr rhs) (using-bridge-var? rhs)]
		[(lexpr rhs) (using-bridge-var? rhs)]
		[(expr rhs) (using-bridge-var? rhs)]
		[(expr-const value) #f]
		[(expr-binary operand1 operator operand2) (or (using-bridge-var? operand1) (using-bridge-var? operand2))]
		[(expr-array array index) (or (using-bridge-var? array) (using-bridge-var? index))]
		[(expr-field obj class fname) (using-bridge-var? obj)]
		[(expr-var name) (using-bridge-var? name)]
		[(variable name) (equal? (string-id name) bridge-var-name)]
		[_ #f]))

(define (unwanted-bridge-var? ast)
	(match ast
		[(stat rhs) (unwanted-bridge-var? rhs)]
		[(stat-ass lvalue rvalue) (unwanted-bridge-var? lvalue)]
		[(lexpr rhs) (unwanted-bridge-var? rhs)]
		[(expr-var name) (unwanted-bridge-var? name)]
		[(variable name) (equal? (string-id name) bridge-var-name)]

		[(stat-calls rhs) (unwanted-bridge-var? rhs)]
		[(stat-static-call ret cls-name func arg-types args) (unwanted-bridge-var? args)]
		[(stat-virtual-call ret obj cls-name func arg-types args) (unwanted-bridge-var? args)]
		[(stat-special-call ret obj cls-name func arg-types args) (unwanted-bridge-var? args)]
		[(arguments-caller rhs) (unwanted-bridge-var? rhs)]
		[(argument-caller-list al) (if al (ormap (lambda (arg) (unwanted-bridge-var? arg)) al) #f)]
		[(dexpr rhs) (unwanted-bridge-var? rhs)]
		[_ #f]))

(define (missing-bridge-var? ast)
	(match ast
		[(stat rhs) (missing-bridge-var? rhs)]
		[(stat-calls rhs) (missing-bridge-var? rhs)]
		[(stat-static-call ret cls-name func arg-types args) (and (variable-name ret) (not (equal? (string-id (variable-name ret)) bridge-var-name)))]
		[(stat-virtual-call ret obj cls-name func arg-types args) (and (variable-name ret) (not (equal? (string-id (variable-name ret)) bridge-var-name)))]
		[(stat-special-call ret obj cls-name func arg-types args) (and (variable-name ret) (not (equal? (string-id (variable-name ret)) bridge-var-name)))]
		[_ #f]))


;======================== Spec Checker ======================

(define (get-invoke-ret-type ast-invoke mac)
	(function-ret (ast->func ast-invoke mac)))

(define (define-bridge-var ast-prog ast-invoke ret-type loc)
	(update-prog ast-prog loc (lambda (ast-funcs)
		(map (lambda (ast-func) (if (not (is-target-func? loc ast-func)) ast-func
			(struct-update function-declare ast-func [rhs (lambda (rhs) 
				(struct-update function-content rhs [local-variables (lambda (lhs)
					(struct-update variable-definitions lhs [rhs (lambda (rhs)
						(struct-update variable-definition-list rhs [vl (lambda (lst)
							(cons (variable-definition (variable-n-type (variable bridge-var-name) (type-name ret-type))) lst))]))]))]))])))
		ast-funcs))))

;(define (fix-arg-type invoke-stat loc)
;
;	(define (args->types args)
;		(types (type-list
;			(map (lambda (arg)
;				(define func (location-func loc))
;				(type-name 
;					(match (dexpr-rhs arg) 
;						[(expr-var v) (lookup-type (variable-name v) func)]
;						[(expr-const v) (jtype-of (const-v v))])))
;				(argument-caller-list-al (arguments-caller-rhs args))))))
;
;	(match invoke-stat
;		[(stat-calls rhs) (stat (fix-arg-type rhs loc))]
;		[(stat rhs) (stat (fix-arg-type rhs loc))]
;		[(stat-static-call ret cls-name func arg-types args) 
;			(stat-static-call ret cls-name func (args->types args) args)]
;		[(stat-virtual-call ret obj cls-name func arg-types args)
;			(stat-virtual-call ret obj cls-name func (args->types args) args)]
;		[(stat-special-call ret obj cls-name func arg-types args)
;			(stat-special-call ret obj cls-name func (args->types args) args)]))
;
;	(RHS-C stat-static-call (ret : variable) (class : type-name) (func : func-name) (arg-types : types) (args : arguments-caller))
;	(RHS-C stat-virtual-call (ret : variable) (obj : variable) (class : type-name) (func : func-name) (arg-types : types) (args : arguments-caller))
;	(RHS-C stat-special-call (ret : variable) (obj : variable) (class : type-name) (func : func-name) (arg-types : types) (args : arguments-caller))

(define (macl->astl sts line)
	(cdr (foldl (lambda (st ll)
		(if (<= (car ll) 0) ll
			(if (stat-label? (stat-rhs st)) 
				(cons (car ll) (+ (cdr ll) 1))
				(cons (- (car ll) 1) (+ (cdr ll) 1)))))
		(cons line 0)
		sts)))

;insert before current `newl`
(define (insert-stat ast stat-sketch newl)
	(define func (location-func newl))
	(define fname (function-name func))
	(define line (if (equal? fname func-name-init) (- (location-line newl) 1) (location-line newl)))
	(update-prog ast newl
		(lambda (ast) 
			(update-func ast newl 
				(lambda (ast) 
					(stats (stat-list (list-insert ast (macl->astl ast line) stat-sketch))))))))

(define (replace-stat ast stat-sketch bugl)
	(define func (location-func bugl))
	(define fname (function-name func))
	(define line (if (equal? fname func-name-init) (- (location-line bugl) 1) (location-line bugl)))
	(update-prog ast bugl
		(lambda (ast) 
			(update-func ast bugl 
				(lambda (ast) 
					(stats (stat-list (std:list-set ast (macl->astl ast line) stat-sketch))))))))

(define (is-target-func? loc func-ast)
	(define func (location-func loc))
	(define fname (function-name func))
	(equal? fname (string-id (func-name-name (function-content-name (function-declare-rhs func-ast))))))

(define (update-func funcs-ast loc lupdater)
	(define maybe-func-ast (findf (curry is-target-func? loc) funcs-ast))
	(if (not maybe-func-ast) funcs-ast
		(begin
		(define func-ast maybe-func-ast)
		(define insts-ast (stat-list-sl (stats-rhs (function-content-statements (function-declare-rhs func-ast)))))
		(define insts-ast-sketch (lupdater insts-ast)) 
		(define func-ast-sketch (function-declare (std:struct-copy function-content (function-declare-rhs func-ast) [statements insts-ast-sketch])))
		(list-replace funcs-ast (curry is-target-func? loc) func-ast-sketch))))

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

(define (inst-type-check? mac cls func inst)
;	(pretty-print inst)
	(define (expr-type-check? expr) 
		(match expr
			[(iexpr-const value type) type]
			[(iexpr-var name)
				(if (equal? name var-this-name) (class-name cls) (lookup-type name func))]
			[(iexpr-binary op expr1 expr2)
				(begin
				(define t1 (expr-type-check? expr1))
				(define t2 (expr-type-check? expr2))
;				(pretty-print (list t1 t2))
				(define op-check? (and t1 t2 (op-type-check? op t1 t2)))
				(if op-check? (op-return-type op t1 t2) #f))]
			[(iexpr-array arr-name index)
				(lookup-type arr-name func)]
			[(iexpr-field obj-name cls-name fname)
;						(pretty-print (sfield-id cls-name fname))
				(if (not (lookup-type obj-name func)) #f
					(if (and 
							(not (equal? obj-name (string-id (variable-name void-receiver)))) 
							(not (is-a? (lookup-type obj-name func) cls-name mac))) 
						#f
						(imap-get (machine-tmap mac) (sfield-id cls-name fname) default-type)))]))
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
;			(pretty-print (sfunc-id cls-name func-name arg-types))
			(and 
				(monitor-reason "func not found" (not (is-not-found? (imap-get (machine-fmap mac) (sfunc-id cls-name func-name arg-types) default-type))))
				(andmap (lambda (at aexpr)
					(define at+ (expr-type-check? aexpr))
					(monitor-reason "arg type" (and at+ (is-a? at+ at mac))))
					arg-types
					args))]
		[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
			(and 
				(monitor-reason "func not found" (not (is-not-found? (imap-get (machine-fmap mac) (sfunc-id cls-name func-name arg-types) default-type))))
				(andmap (lambda (at aexpr)
					(define at+ (expr-type-check? aexpr))
					(monitor-reason "arg type" (and at+ (is-a? at+ at mac))))
					arg-types
					args))]
		[(inst-special-call ret obj-name cls-name func-name arg-types args)
			(and 
				(not (is-not-found? (imap-get (machine-fmap mac) (sfunc-id cls-name func-name arg-types) default-type)))
				(andmap (lambda (at aexpr)
					(define at+ (expr-type-check? aexpr))
					(and at+ (is-a? at+ at mac)))
					arg-types
					args))]
		[(inst-ass vl vr)  
			(begin
			(define lt (expr-type-check? (ast->expression vl)))
			(define rt (expr-type-check? vr))
;			(pretty-print (list lt rt))
			(and lt rt (or (is-a? rt lt mac) (is-a? lt rt mac))))]
		[(inst-switch cnd cases default-l) #t]
		[(inst-jmp condition label) 
					(expr-type-check? condition)]))

(define (machine-type-check? mac)
	(define (func-type-check? cls.func)
		(match-define (cons cls func) cls.func)
		(andmap 
			(lambda (i) (if (inst-type-check? mac cls func i) #t (begin (display "Type error: ") (pretty-print i) #f))) 
			(function-prog func)))
	(andmap func-type-check? (all-class-functions mac)))

(define visited-sid-map (imap-empty default-type))
(register-reset! (lambda () (set! visited-sid-map (imap-empty default-type))) #t)

(define (machine-prepare-recursion mac) 
	(define all-visited-sids-0 (curry all-visited-sids identity))
	(define sids (all-sids mac))
	(map (lambda (func)
		(set! visited-sid-map (imap-set visited-sid-map func (do-n-ret pretty-print (all-visited-sids-0 mac func)) default-type)))
		sids))

(define (contains-target-quick? sid target-sid)
	(member target-sid (imap-get visited-sid-map sid default-type)))

#|
(define (machine-has-recursion? mac) 
	(define contains-target-ori? (curry contains-target-or-rec? identity))
	(define sids (all-sids mac))
	(ormap (lambda (func)
;		(pretty-print func)
		(contains-target-ori? mac func (list func)))
		sids))

(define (machine-all-check? mac)
	(and (machine-type-check? mac) (monitor-reason "recursion" (not (machine-has-recursion? mac)))))
|#


(define contains-target-list null)
(define (reset-contains-target-cache!)
	(set! contains-target-list null))
(register-reset! reset-contains-target-cache! #f)
;if a function will (transitively) call any target function
(define (contains-target? func-getter mac sid target-sids)
;	(pretty-print sid)
	(if (or (member sid contains-target-list) (member sid target-sids)) #t
		(begin
		(define func (func-getter (imap-get (machine-fmap mac) sid default-type)))
		(define prog (function-prog func))
		(define ret (ormap (lambda (inst)
			(match inst
				[(inst-nop _) #f]
				[(inst-init classname) #f]
				[(inst-newarray v-name size-expr) #f]
				[(inst-new v-name) #f]
				[(inst-ret v-expr) #f]
				[(inst-long-jump cls-name func-name) 
					(if (model-lookup cls-name func-name) #f
						(contains-target? func-getter mac (sfunc-id cls-name func-name null) target-sids))]
				[(inst-static-call ret cls-name func-name arg-types args) 
					(if (model-lookup cls-name func-name) #f
						(contains-target? func-getter mac (sfunc-id cls-name func-name arg-types) target-sids))]
				[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
					(if (model-lookup cls-name func-name) #f
						(begin
						(define vid (vfunc-id func-getter mac cls-name func-name arg-types))
						(define sids-invoked 
							(map second
								(filter (lambda (fsv) (and (not (is-interface-func? (func-getter (first fsv)))) (equal? (third fsv) vid)))
									(all-vf-sid-vids func-getter mac))))
						(ormap (lambda (sid) (contains-target? func-getter mac sid target-sids)) sids-invoked)))]
				[(inst-special-call ret obj-name cls-name func-name arg-types args)
					(if (model-lookup cls-name func-name) #f
						(contains-target? func-getter mac (sfunc-id cls-name func-name arg-types) target-sids))]
				[(inst-ass vl vr)  #f]
				[(inst-switch cnd cases default-l) #f]
				[(inst-jmp condition label) #f]))
			prog))
		(if ret (set! contains-target-list (cons sid contains-target-list)) #f)
		ret)))

(define visited-sids null)

(define (all-visited-sids func-getter mac sid)
	(set! visited-sids null)
	(contains-target-pure? func-getter mac sid null)
	visited-sids)

(define (contains-target-or-rec? func-getter mac sid target-sids)
	(set! visited-sids null)
	(contains-target-pure? func-getter mac sid target-sids))

(define (contains-target-pure? func-getter mac sid target-sids)
	(if (member sid visited-sids) #t
		(begin
		(set! visited-sids (cons sid visited-sids))
		(define func (func-getter (imap-get (machine-fmap mac) sid default-type)))
		(define prog (function-prog func))
		(define ret (ormap (lambda (inst)
			(match inst
				[(inst-nop _) #f]
				[(inst-init classname) #f]
				[(inst-newarray v-name size-expr) #f]
				[(inst-new v-name) #f]
				[(inst-ret v-expr) #f]
				[(inst-long-jump cls-name func-name) 
					(if (model-lookup cls-name func-name) #f
						(if (member (sfunc-id cls-name func-name null) target-sids) #t
							(contains-target-pure? func-getter mac (sfunc-id cls-name func-name null) target-sids)))]
				[(inst-static-call ret cls-name func-name arg-types args) 
					(if (model-lookup cls-name func-name) #f
						(if (member (sfunc-id cls-name func-name arg-types) target-sids) #t
						(contains-target-pure? func-getter mac (sfunc-id cls-name func-name arg-types) target-sids)))]
				[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
					(if (model-lookup cls-name func-name) #f
						(begin
						(define vid (vfunc-id func-getter mac cls-name func-name arg-types))
						(define sids-invoked 
							(map second
								(filter (lambda (fsv) (and (not (is-interface-func? (func-getter (first fsv)))) (equal? (third fsv) vid)))
									(all-vf-sid-vids func-getter mac))))
						(ormap (lambda (sid) 
							(if (member sid target-sids) #t
								(contains-target-pure? func-getter mac sid target-sids))) sids-invoked)))]
				[(inst-special-call ret obj-name cls-name func-name arg-types args)
					(if (model-lookup cls-name func-name) #f
						(if (member (sfunc-id cls-name func-name arg-types) target-sids) #t
							(contains-target-pure? func-getter mac (sfunc-id cls-name func-name arg-types) target-sids)))]
				[(inst-ass vl vr)  #f]
				[(inst-switch cnd cases default-l) #f]
				[(inst-jmp condition label) #f]))
			prog))
		(set! visited-sids (cdr visited-sids))
		ret)))

(define (find-new-func mac oldc oldf)
	(ormap (lambda (newcf) 
		(if (and 
			(equal? (class-name oldc) (class-name (car newcf)))
			(equal? (function-name oldf) (function-name (cdr newcf)))
			(equal? (function-args oldf) (function-args (cdr newcf))))
			(cdr newcf)
			#f))
		(all-class-functions mac)))



;======================== Debug ========================
;(define (monitor-reason msg result)
;	result) 

(define (monitor-reason msg result)
	(if (not result) (display (~a "Fail check " msg "\n")) #f)
	result)
