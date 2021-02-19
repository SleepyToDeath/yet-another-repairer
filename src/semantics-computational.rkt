#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require "string-id.rkt")
(require "jimple/jimple-parser.rkt")
(require "memory-common.rkt")
(require "match-define.rkt")
(require "formula.rkt")
(require "model.rkt")
(require "semantics-common.rkt")
(require racket/format)
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`
(require racket/pretty)

(provide (all-defined-out))

;======================== Execution Interface ===========================
;machine(init) X list of names -> machine(fin)
(define (compute mac)
	(define mac-init (build-virtual-table mac))
	(function-call-simple mac-init (machine-boot mac-init)))

(define (function-call-simple mac func)
;	(pretty-print (machine-mem mac))
	(define mac-decl (std:struct-copy machine mac [mem 
		(foldl 
			(lambda (var-def mem) (memory-sdecl mem (string-id (car var-def)))) 
			(machine-mem mac) 
			(append (function-args func) (function-locals func)))]))
	(function-exec (std:struct-copy machine mac-decl [pc pc-init]) func))

(define (function-call mac func args)
	(define mac-reset (std:struct-copy machine mac [pc pc-init][mem (memory-spush (machine-mem mac))]))
	(define mac-decl (std:struct-copy machine mac-reset [mem 
		(foldl 
			(lambda (var-def mem) (memory-sdecl mem (string-id (car var-def)))) 
			(machine-mem mac-reset) 
			(append (function-args func) (function-locals func)))]))
	(define mac-input (std:struct-copy machine mac-decl [mem
		(foldl 
			(lambda (arg-src arg-dst mem) (memory-swrite mem (string-id (car arg-dst)) arg-src))
			(machine-mem mac-decl)
			args 
			(function-args func))]))
;	(pretty-print (machine-mem mac-input))
	(function-exec mac-input func))

(define (function-exec mac func)
	(if (equal? (machine-pc mac) pc-ret) 
		mac
		(let ([inst-cur (list-ref (function-prog func) (machine-pc mac))])
;			(display "\n\n")
;			(println (machine-mem mac))
;			(display "\n")
;			(println string-id-map)
			(display "\n")
			(println inst-cur)
			(display "\n\n")
			(set-context! mac)
			(function-exec (inst-exec inst-cur mac func) func))))

;machine X list of (key, value) -> machine(with input inserted into memory)
(define (assign-input mac input)
	(define mem0 (memory-spush (machine-mem mac)))
	(define mem-ass 
		(foldl (lambda (kv mem-cur) (memory-swrite (memory-sdecl mem-cur (string-id (car kv))) (string-id (car kv)) (cdr kv))) mem0 input))
	(std:struct-copy machine mac [mem mem-ass]))

;machine X list of (key, value) -> boolean
(define (compare-output mac output)
	(define mem0 (machine-mem mac))
	(foldl (lambda (kv fml-cur) (and fml-cur (equal? (cdr kv) 
			((lambda ()
				(begin
					(define ret-v (memory-sread mem0 (string-id (car kv))))
					(println ret-v)
					ret-v
					)))
			))) #t output))

;machine X list of string(output var names)
;(define (get-output mac output-names)
;	(define mem0 (machine-mem mac))
;	(map (lambda (name) (memory-sread mem0 (string-id name))) output-names))

;======================== AST Interpreter ===========================

;some contexts
(define class-name-main (string-id "dummy"))
(define class-names-clinit null)
(define func-main #f)
(define funcs-clinit null)


(define (ast->machine ast)
	(define classes (foldl 
		(lambda (class-ast cl) (cons (ast->class class-ast) cl))
		null
		(class-list-cl (program-rhs ast))))
	(define cmap (foldl (lambda (cls cm) (imap-set cm (class-name cls) cls)) imap-empty classes))
	(define boot (build-boot-func))
	(define mac-init (machine boot classes cmap imap-empty memory-empty pc-init))
	mac-init)

(define (ast->class ast)
	(define rhs (class-def-rhs ast))
	(match rhs
		[(class-default name-ast extend-ast interfaces-ast globals-ast fields-ast sfuncs-ast vfuncs-ast)
			(letrec 
				([name (string-id (type-name-name name-ast))]
				[extend (if (type-name-name extend-ast) (string-id (type-name-name extend-ast)) #f)]
				[interfaces 
					(map (lambda (ast) (string-id (type-name-name ast))) (interface-name-list-il (interface-implements-rhs interfaces-ast)))]
				[sfuncs 
					(map (lambda (ast) (ast->function name ast)) (function-list-fl (function-declares-rhs sfuncs-ast)))]
				[vfuncs
					(map (lambda (ast) (ast->function name ast)) (function-list-fl (function-declares-rhs vfuncs-ast)))]
				[fields 
					(map (lambda (ast) (string-id (field-name ast))) (field-list-fl (field-declares-rhs fields-ast)))]
				[globals
					(map (lambda (ast) (string-id (field-name ast))) (field-list-fl (field-declares-rhs globals-ast)))])
				(class name extend interfaces sfuncs vfuncs globals fields))]))

;(struct function (name prog lmap args locals) #:transparent)
;(LHS-C function-declare (rhs ::= function-content))
;	(RHS-C function-content (name : function-name) (args : arguments) (local-variables : variable-declares) (statements : stats))
(define (ast->function classname ast)
	(define rhs (function-declare-rhs ast))
	(match rhs
		[(function-content name-ast args-ast local-vars-ast statements-ast)
			(letrec 
				([name (string-id (func-name-name name-ast))]
				 [args (variable-definitions->list args-ast)]
				 [local-vars (variable-definitions->list local-vars-ast)]
				 [func-sig 
					(function name 
						(if (std:equal? name func-name-init) (list (inst-init classname)) null) 
						imap-empty args local-vars)])
				(begin
					(define func 
						(foldl (lambda (st func)
							(letrec 
								([ret-pair (ast->instruction st (function-lmap func) (length (function-prog func)))]
								 [inst (car ret-pair)]
								 [lmap-new (cdr ret-pair)])
								(std:struct-copy function func 
									[prog (if inst (append (function-prog func) (list inst)) (function-prog func))]
									[lmap lmap-new])))
							func-sig
							(stat-list-sl (stats-rhs statements-ast))))

					(if (equal? name func-name-main) 
						(begin 
							(set! func-main func) 
							(set! class-name-main classname)) #f)

					(if (equal? name func-name-clinit) 
						(begin 
							(set! funcs-clinit (cons func funcs-clinit)) 
							(set! class-names-clinit (cons classname class-names-clinit))) #f)

					func))]))

;ast X lmap X line-number -> instruction X lmap(updated)
(define (ast->instruction ast lmap line-num)
	(pretty-print ast)
	(match ast
		[(stat s) (ast->instruction s lmap line-num)]
		[(stat-ass target rvalue) 
			(begin
			(cons (inst-ass target (ast->expression rvalue)) lmap))]
		[(stat-jmp condition target) (cons (inst-jmp (ast->expression condition) (label-v target)) lmap)]
		[(stat-switch condition cases) 
			(cons
				(foldl (lambda (c i)
						(match (stat-case-rhs c)
							[(case-br k l) (std:struct-copy inst-switch i [cases (cons (cons (const-v k) (label-v l)) (inst-switch-cases i))])]
							[(case-default l) (std:struct-copy inst-switch i [default-label (label-v l)])]))
					(inst-switch (ast->expression condition) null #f)
					(case-list-cl  (stat-case-list-rhs cases)))
				lmap)]
		[(stat-label here) (cons #f (imap-set lmap (label-v here) line-num))]
		[(stat-nop any) (cons (inst-nop nullptr) lmap)]
		[(stat-ret v) (cons (inst-ret (ast->expression v)) lmap)]
		[(stat-new v) (cons (inst-new (string-id (variable-name v))) lmap)]
		[(stat-newarray v size) (cons (inst-newarray (string-id (variable-name v)) (ast->expression size)) lmap)]
	;(RHS-C stat-newarray (v : variable) (size : dexpr))
		[(stat-static-call ret cls-name func arg-types args) 
			(cons 
				(inst-static-call (string-id (variable-name ret)) (string-id (type-name-name cls-name)) (string-id (func-name-name func))
					(map (lambda (ast) (string-id (type-name-name ast))) (type-list-tl (types-rhs arg-types)))
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]
		[(stat-virtual-call ret obj cls-name func arg-types args)
			(cons 
				(inst-virtual-call (string-id (variable-name ret)) (string-id (variable-name obj)) 
					(string-id (type-name-name cls-name)) (string-id (func-name-name func))
					(map (lambda (ast) (string-id (type-name-name ast))) (type-list-tl (types-rhs arg-types)))
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]
		[(stat-special-call ret obj cls-name func arg-types args)
			(cons 
				(inst-special-call (string-id (variable-name ret)) (string-id (variable-name obj)) 
					(string-id (type-name-name cls-name)) (string-id (func-name-name func))
					(map (lambda (ast) (string-id (type-name-name ast))) (type-list-tl (types-rhs arg-types)))
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]))

(define (ast->expression ast)
	(match ast
		[(expr e) (ast->expression e)]
		[(lexpr e) (ast->expression e)]
		[(dexpr e) (ast->expression e)]
		[(expr-const c) (iexpr-const (if (std:string? (const-v c)) (string-id (const-v c)) (const-v c)))]
		[(expr-var v) (iexpr-var (string-id (variable-name v)))]
		[(expr-binary expr1 o expr2) (iexpr-binary (op-v o) (ast->expression expr1) (ast->expression expr2))]
		[(expr-array array index) (iexpr-array (string-id (variable-name array)) (ast->expression index))]
		[(expr-field obj class fname) (iexpr-field (string-id (variable-name obj)) (string-id (type-name-name class)) (string-id (field-name fname)))]))

(define (build-boot-func)
	;(define iboot (inst-boot globals))
	(define icall-clinit (map (lambda (name) (inst-static-call var-void-ret name func-name-clinit null null)) class-names-clinit))
	(define icall-main (inst-long-jump class-name-main func-name-main))
	(define iret (inst-ret (iexpr-var var-ret-name)))
	(function func-name-boot (append icall-clinit (list icall-main iret)) imap-empty null (list (cons var-ret-name "int"))))

(define (build-virtual-table mac) 
	(display "Memory before buiding virtual table:\n")
;	(pretty-print (machine-mem mac))
	(define classes (machine-classes mac))
	(define (process-class cls mac)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
		(define vfields (if (equal? cls-name class-name-root) 
			(cons field-name-class (class-vfields cls))
			(class-vfields cls)))

		(define mac-sfuncs (foldl 
			(lambda (sf mac) 
				(define sid (sfunc-id cls-name (function-name sf) (map cdr (function-args sf))))
;				(define mem-1 (memory-sforce-write (machine-mem mac) sid sid 0))
				(define fmap-1 (imap-set (machine-fmap mac) sid sf))
				(std:struct-copy machine mac [fmap fmap-1]))
			mac sfuncs))

		(define mac-sfields (foldl 
			(lambda (sf mac) 
				(std:struct-copy machine mac 
					[mem (memory-sdecl (machine-mem mac) (sfield-id cls-name sf))])) 
			mac-sfuncs sfields))

		(define mac-vfuncs (foldl 
			(lambda (vf mac) 
				(define vid (vfunc-id mac cls-name (function-name vf) (map cdr (function-args vf))))
				(define sid (sfunc-id cls-name (function-name vf) (map cdr (function-args vf))))
				(define mem-1 (memory-fdecl (machine-mem mac) vid)) 
				(define fmap-1 (imap-set (machine-fmap mac) sid vf))
				(std:struct-copy machine mac [mem mem-1] [fmap fmap-1]))
			mac-sfields vfuncs))

		(define mac-vfields (foldl 
			(lambda (vf mac) 
				(std:struct-copy machine mac 
					[mem (memory-fdecl (machine-mem mac) (vfield-id mac cls-name vf))])) 
			mac-vfuncs vfields))

;		(println string-id-table)
		mac-vfields)

;	(define mem-push (memory-spush (machine-mem mac)))
	(define mac-cls (foldl process-class mac classes))
	(define mem-reserve-obj (cdr (memory-alloc (machine-mem mac-cls) vt-size)))
	(std:struct-copy machine mac-cls [mem mem-reserve-obj]))

(define (variable-definitions->list ast)
	(map 
		(lambda (ast) (variable-definition->pair ast))
		(variable-definition-list-vl (variable-definitions-rhs ast))))

(define (variable-definition->pair ast)
	(cons 
		(string-id (variable-name (variable-n-type-name (variable-definition-rhs ast))))
		(string-id (type-name-name (variable-n-type-type (variable-definition-rhs ast))))))


;======================== Instructions ===========================
;assign virtual function to "this" from class information
;"this" should be give by caller of special call
(struct inst-init (classname) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 
		(define classname (inst-init-classname i))
		(define mem-0 (machine-mem m))
		(define addr (memory-sread mem-0 var-this-name))

		(define mem-bind-func (foldl
			(lambda (func mem) 
				(define vid (vfunc-id m classname (function-name func) (map cdr (function-args func))))
				(define sid (sfunc-id classname (function-name func) (map cdr (function-args func))))
				(if (is-not-found? (memory-fread mem vid addr))
					(memory-fwrite mem vid addr sid) 
					mem))
			mem-0
			(class-vfuncs (imap-get (machine-cmap m) classname))))

		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-bind-func] [pc pc-next]))])


(struct inst-ass (vl vr) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 

		(define mem0 (machine-mem m))
		(define v-new (expr-eval (inst-ass-vr i) m))

		(define rhs (lexpr-rhs (inst-ass-vl i)))
		(define mem-new 
			(match rhs
				[(expr-var v) (memory-swrite mem0 (string-id (variable-name v)) v-new)]
				[(expr-array arr idx)
					(letrec
						([addr (memory-sread mem0 (string-id (variable-name arr)))]
						[idx-e (ast->expression idx)]
						[idx-v (expr-eval idx-e m)])
						(memory-awrite mem0 addr idx-v v-new))]
				[(expr-field obj cls fname)
					(if (equal? obj void-receiver)
						(memory-swrite mem0 (sfield-id (string-id (type-name-name cls)) (string-id (field-name fname))) v-new)
						(letrec
							([addr (memory-sread mem0 (string-id (variable-name obj)))])
							(memory-fwrite mem0 (vfield-id m (string-id (type-name-name cls)) (string-id (field-name fname))) addr v-new)))]
				[_ #f]))

		(define pc-next (+ 1 (machine-pc m)))

;		(pretty-print mem-new)

		(std:struct-copy machine m [mem mem-new] [pc pc-next]))])

;expr X (list of (int X label)) X (maybe label)
(struct inst-switch (cnd cases default-label) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(match i [(inst-switch cnd cases default-l)
			(begin
			(define lmap (function-lmap f))
			(define cnd-v (expr-eval cnd m))
			(define cases-default (if default-l
				(append cases (list (cons cnd-v default-l)))
				(append cases (list (cons cnd-v (+ 1 (machine-pc m)))))))
			(define label-new (ormap (lambda (k.l)
					(if (equal? (car k.l) cnd-v) (cdr k.l) #f))
				cases-default))
			(define pc-new (imap-get lmap label-new))
			(std:struct-copy machine m [pc pc-new]))]))])

;expr X label(int)
(struct inst-jmp (condition label) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)

		(define lmap (function-lmap f))
		(define iaddr (inst-jmp-label i))
		(define pc-jmp (imap-get lmap iaddr))
		(define pc-next (+ 1 (machine-pc m)))

		(define c (expr-eval (inst-jmp-condition i) m))
		(define pc-new (if c pc-jmp pc-next))

		(std:struct-copy machine m [pc pc-new]))])

(struct inst-nop (any) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define pc-next (+ 1 (machine-pc m)))
		(std:struct-copy machine m [pc pc-next]))])

(struct inst-newarray (v-name size) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(match i [(inst-newarray v-name size-expr)
			(begin
			(define mem-0 (machine-mem m))
			(define size (expr-eval size-expr m))
			(match-define (cons addr mem-alloc) (memory-alloc mem-0 size))
			(display (~a "new array: " addr))
			(define pc-next (+ 1 (machine-pc m)))
			(define mem-ass (memory-swrite mem-alloc v-name addr))
			(std:struct-copy machine m [pc pc-next][mem mem-ass]))]))])

(struct inst-new (v-name) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem-0 (machine-mem m))
		(define v-name (inst-new-v-name i))
		(match-define (cons addr mem-alloc) (memory-new mem-0))
		(define pc-next (+ 1 (machine-pc m)))
		(define mem-ass (memory-swrite mem-alloc v-name addr))
;		(pretty-print mem-ass)
		(std:struct-copy machine m [pc pc-next][mem mem-ass]))])

(struct inst-ret (v-expr) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define ret-value (expr-eval (inst-ret-v-expr i) m))
		(display (~a "return value:" ret-value "\n"))
		(define mem-ret (memory-sforce-write (machine-mem m) var-ret-name ret-value 0))
		(std:struct-copy machine m [pc pc-ret][mem mem-ret]))])

(struct inst-long-jump (cls-name func-name) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define sid (sfunc-id (inst-long-jump-cls-name i) (inst-long-jump-func-name i) null))
		(define func (imap-get (machine-fmap m) sid))
		(function-call-simple m func))])

(struct inst-static-call (ret cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem-0 (machine-mem m))
		(define cls-name (inst-static-call-cls-name i))
		(define func-name (inst-static-call-func-name i))
		(define ret (inst-static-call-ret i))
		(define args (map (lambda (arg) (expr-eval arg m)) (inst-static-call-args i)))
		(define pc-next (+ 1 (machine-pc m)))
		
		(define mfunc (model-lookup cls-name func-name))
		(if mfunc 
			(std:struct-copy machine m [mem (mfunc mem-0 ret args)][pc pc-next])

			(begin

			(define sid (sfunc-id cls-name func-name (inst-static-call-arg-types i)))
			;no need to read from memory
			(define func (imap-get (machine-fmap m) sid))
			(define ret (inst-static-call-ret i))

			(define mac-ret (function-call m func args))
			(define mem-ret (machine-mem mac-ret))
			(define ret-value (memory-sread mem-ret var-ret-name))
			
			(define mem-pop (memory-spop mem-ret))
			(define mem-ass (memory-swrite mem-pop ret ret-value))

			(std:struct-copy machine m [mem mem-ass][pc pc-next]))))])
		
(struct inst-virtual-call (ret obj-name cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		;(println i)
		(define mem-0 (machine-mem m))
		(define obj-addr (memory-sread mem-0 (inst-virtual-call-obj-name i)))
		(define cls-name (inst-virtual-call-cls-name i))
		(define func-name (inst-virtual-call-func-name i))
		(define ret (inst-virtual-call-ret i))
		(define args (map (lambda (arg) (expr-eval arg m)) (inst-virtual-call-args i)))
		(define pc-next (+ 1 (machine-pc m)))

		(define mfunc (model-lookup cls-name func-name))
		(if mfunc 
			(std:struct-copy machine m [mem (mfunc mem-0 obj-addr ret args)][pc pc-next])

			(begin


			(define vid (vfunc-id m cls-name func-name (inst-virtual-call-arg-types i)))
;			(display (~a "vid: " vid "\n"))
			(define sid (memory-fread mem-0 vid obj-addr))
;			(display (~a "sid: " sid "\n"))
			(define func (imap-get (machine-fmap m) sid))
			;push an extra scope to avoid overwriting "this" of the current scope
			(define mem-this (memory-sforce-write (memory-spush mem-0) var-this-name obj-addr 0))
			(define mac-this (std:struct-copy machine m [mem mem-this]))

			(define mac-ret (function-call mac-this func args))
			(define mem-ret (machine-mem mac-ret))
			(define ret-value (memory-sread mem-ret var-ret-name))
			;pop callee and callee's "this"
			(define mem-pop (memory-spop (memory-spop mem-ret)))
			(define mem-ass (memory-swrite mem-pop (inst-virtual-call-ret i) ret-value))

			(std:struct-copy machine m [mem mem-ass][pc pc-next]))))])

;in my understanding, special call = virtual call - virtual, at least for init
(struct inst-special-call (ret obj-name cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem-0 (machine-mem m))
		(define obj-addr (memory-sread mem-0 (inst-special-call-obj-name i)))
		(define cls-name (inst-special-call-cls-name i))
		(define func-name (inst-special-call-func-name i))
		(define ret (inst-special-call-ret i))
		(define args (map (lambda (arg) (expr-eval arg m)) (inst-special-call-args i)))
		(define pc-next (+ 1 (machine-pc m)))

		(define mfunc (model-lookup cls-name func-name))
		(if mfunc 
			(std:struct-copy machine m [mem (mfunc mem-0 obj-addr ret args)][pc pc-next])

			(begin

			(define sid (sfunc-id (inst-special-call-cls-name i) (inst-special-call-func-name i) (inst-special-call-arg-types i)))
			;never virtual
			(define func (imap-get (machine-fmap m) sid))
			;push an extra scope to avoid overwriting "this" of the current scope
			(define mem-this (memory-sforce-write (memory-spush mem-0) var-this-name obj-addr 0))
			(define mac-this (std:struct-copy machine m [mem mem-this]))

			(define mac-ret (function-call mac-this func args))
			(define mem-ret (machine-mem mac-ret))
			(define ret-value (memory-sread mem-ret var-ret-name))
			;pop callee and callee's "this"
			(define mem-pop (memory-spop (memory-spop mem-ret)))
			(define mem-ass (memory-swrite mem-pop (inst-special-call-ret i) ret-value))

			(std:struct-copy machine m [mem mem-ass][pc pc-next]))))])
		


;======================== Expressions ===========================
(struct iexpr-const (value) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(iexpr-const-value e))])

(struct iexpr-var (name) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define ret (if (equal? (iexpr-var-name e) var-this-name)
			(memory-sforce-read (machine-mem m) (iexpr-var-name e) 1)
			(memory-sforce-read (machine-mem m) (iexpr-var-name e) 0)))
		(defer-eval "var read: " (cons (iexpr-var-name e) ret))
		ret)])

(struct iexpr-binary (op expr1 expr2) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define v1 (expr-eval-dispatch (iexpr-binary-expr1 e) m))
		(define v2 (expr-eval-dispatch (iexpr-binary-expr2 e) m))
;		(defer-eval e v1)
;		(defer-eval e v2)
;		(display (~a "Binary op v1: " v1 " v2: " v2))
		((iexpr-binary-op e) v1 v2))])

(struct iexpr-array (arr-name index) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m) 
		(define mem0 (machine-mem m))
		(define arr-addr (memory-sforce-read mem0 (iexpr-array-arr-name e) 0))
		(define idx (expr-eval-dispatch (iexpr-array-index e) m))
		(define ret (memory-aread mem0 arr-addr idx))
;		(display (~a "array read index: " idx " value: " ret))
		ret)])

(struct iexpr-field (obj-name cls-name fname) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define mem0 (machine-mem m))
		(define fname (iexpr-field-fname e))
		(define cls-name (iexpr-field-cls-name e))
		(define obj-name (iexpr-field-obj-name e))
		(define ret 
			(if (equal? obj-name (string-id (variable-name void-receiver)))
				(memory-sforce-read mem0 (sfield-id cls-name fname) 0)
				(let([obj-addr (memory-sforce-read mem0 obj-name 0)])
					(memory-fread mem0 (vfield-id m cls-name fname) obj-addr))))
		(defer-eval "field read: " (list obj-name cls-name fname ret))
		ret)])

