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
(require "type-checker.rkt")
(require racket/format)
(require racket/string)
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`
(require racket/pretty)

(provide (all-defined-out))

(define line-counter-c 0)

;======================== Execution Interface ===========================
;machine(init) X list of names -> machine(fin)
(define (compute mac)
	(define mac-init (build-virtual-table mac))
;	(display (~a "mem size 2.1: " (memory-heap-size (machine-mem mac-init)) " \n"))
;	(pretty-print string-id-table)
	(function-call-simple mac-init (machine-boot mac-init)))

(define (function-call-simple mac func)
;	(pretty-print (machine-mem mac))
	;[-TODO] arg & local type
	(define mac-decl (std:struct-copy machine mac [mem 
		(foldl 
			(lambda (var-def mem) (memory-sdecl mem (string-id (car var-def)) (jtype->mtype (cdr var-def)))) 
			(machine-mem mac) 
			(append (function-args func) (function-locals func) (list (cons var-ret-name (function-ret func)))))]))
;	(display (~a "mem size 2.2: " (memory-heap-size (machine-mem mac-decl)) " \n"))
	(set! line-counter-c 0)
	(function-exec (std:struct-copy machine mac-decl [pc pc-init][fc func])))

(define (function-call mac func args)
	(define mac-reset (std:struct-copy machine mac [pc pc-init][fc func][mem (memory-spush (machine-mem mac))]))
	;[-TODO] arg & local type
	(define mac-decl (std:struct-copy machine mac-reset [mem 
		(foldl 
			(lambda (var-def mem) (memory-sdecl mem (string-id (car var-def)) (jtype->mtype (cdr var-def)))) 
			(machine-mem mac-reset) 
			(append (function-args func) (function-locals func) (list (cons var-ret-name (function-ret func)))))]))
	;[-TODO] arg & local type
	(define mac-input (std:struct-copy machine mac-decl [mem
		(foldl 
			(lambda (arg-src arg-dst mem) (memory-sforce-write mem (string-id (car arg-dst)) arg-src 0 (jtype->mtype (cdr arg-dst))))
			(machine-mem mac-decl)
			args 
			(function-args func))]))

;	(display "Calling function: \n")
;	(pretty-print (machine-fc mac-input))

	(function-exec mac-input))


(define (function-exec mac)
	(define func (machine-fc mac))
	(if (equal? (machine-pc mac) pc-ret) 
		mac
		(let ([inst-cur (list-ref (function-prog func) (machine-pc mac))])
;			(display "\n\n")
;			(pretty-print (machine-mem mac))
;			(display "\n")
;			(println string-id-map)
;			(display "\n")
;			(pretty-print inst-cur)
			(set! line-counter-c (+ line-counter-c 1))
;			(display (~a "mem size 2.2: " (memory-heap-size (machine-mem mac)) " \n"))
;			(display (~a "Lines of code: " line-counter-c))
;			(display "\n\n")
			(set-context! mac)
			(function-exec (inst-exec inst-cur mac func)))))

;machine X list of int -> machine(with input inserted into memory)
(define (assign-input mac input)
;	(pretty-print input)
	(define mem0 (memory-spush (machine-mem mac)))
	(reset-parameter-names)
	;[-TODO] input-type
	(define mem-ass 
		(foldl 
			(lambda (v.t mem-cur) 
				(memory-sforce-write mem-cur (next-parameter-name) (car v.t) 0 (jtype->mtype (string-id (cdr v.t)))))
			mem0 
			input))
	(std:struct-copy machine mac [mem mem-ass]))

;[-TODO] return type
;machine X list of (key, value) -> boolean
(define (compare-output mac output)
	(define mem0 (machine-mem mac))
	(foldl (lambda (kv fml-cur) (and fml-cur (equal? (cdr kv) 
		(do-n-ret pretty-print (memory-sforce-read mem0 (string-id (car kv)) 0)))))
		#t output))

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
	(set! class-names-clinit null)
	(set! funcs-clinit null)
	(define classes (foldl 
		(lambda (class-ast cl) (cons (ast->class class-ast) cl))
		null
		(class-list-cl (program-rhs ast))))
	(define cmap (foldl (lambda (cls cm) (imap-set cm (class-name cls) cls default-type)) (imap-empty default-type) classes))
	(define boot (build-boot-func))
	(define mac-init (machine boot classes cmap (imap-empty default-type) (imap-empty default-type) memory-empty pc-init #f))
	mac-init)

(define (ast->class ast)
;	(display "--------------------ast->class\n")
	(define rhs (class-def-rhs ast))
	(match rhs
		[(class-default name-ast extend-ast interfaces-ast sfields-ast vfields-ast sfuncs-ast vfuncs-ast)
			(letrec 
				([name (string-id (type-name-name name-ast))]
				[extend (if (type-name-name extend-ast) (string-id (type-name-name extend-ast)) #f)]
				[interfaces (map (lambda (ast) (string-id (type-name-name ast))) (syntax-unwrap 2 interfaces-ast))]
				[sfuncs (map (lambda (ast) (ast->function name ast)) (syntax-unwrap 2 sfuncs-ast))]
				[vfuncs	(map (lambda (ast) (ast->function name ast)) (syntax-unwrap 2 vfuncs-ast))]
				[sfields (variable-definitions->list sfields-ast)]
				[vfields (append sfields (variable-definitions->list vfields-ast))])
				(class name extend interfaces sfuncs vfuncs sfields vfields))]))

;(struct function (name prog lmap args locals) #:transparent)
;(LHS-C function-declare (rhs ::= function-content))
;	(RHS-C function-content (name : function-name) (args : arguments) (local-variables : variable-declares) (statements : stats))
(define (ast->function classname ast)
;	(display "--------------------ast->function\n")
	(define rhs (function-declare-rhs ast))
	(match rhs
		[(function-content name-ast args-ast ret-ast local-vars-ast statements-ast)
			(letrec 
				([name (string-id (func-name-name name-ast))]
				 [args (variable-definitions->list args-ast)]
				 [local-vars (variable-definitions->list local-vars-ast)]
				 [ret (string-id (syntax-unwrap 1 ret-ast))]
				 [func-sig 
					(function name 
						(if (std:equal? name func-name-init) (list (inst-init classname)) null) 
						(imap-empty default-type) args local-vars ret)])
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
;	(display "--------------------ast->instruction\n")
;	(pretty-print ast)
	(match ast
		[(stat s) (ast->instruction s lmap line-num)]
		[(stat-calls s) (ast->instruction s lmap line-num)]
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
		[(stat-label here) (cons #f (imap-set lmap (label-v here) line-num default-type))]
		[(stat-nop any) (cons (inst-nop (nullptr default-type)) lmap)]
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
			;[!] it is assumed that special calls do not have return value
			(cons 
				(inst-special-call var-void-ret (string-id (variable-name obj)) 
					(string-id (type-name-name cls-name)) (string-id (func-name-name func))
					(map (lambda (ast) (string-id (type-name-name ast))) (type-list-tl (types-rhs arg-types)))
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]))

(define (ast->expression ast)
;	(display "--------------------ast->expression\n")
	(match ast
		[(expr e) (ast->expression e)]
		[(lexpr e) (ast->expression e)]
		[(dexpr e) (ast->expression e)]
		[(expr-const c) (iexpr-const 
			(if (std:string? (const-v c)) (string-id (const-v c)) (const-v c))
			(if (std:string? (const-v c)) string-type-name (jtype-of (const-v c))))]
		[(expr-var v) (iexpr-var (string-id (variable-name v)))]
		[(expr-binary expr1 o expr2) (iexpr-binary (op-v o) (ast->expression expr1) (ast->expression expr2))]
		[(expr-array array index) (iexpr-array (string-id (variable-name array)) (ast->expression index))]
		[(expr-field obj class fname) (iexpr-field (string-id (variable-name obj)) (string-id (type-name-name class)) (string-id (field-name fname)))]))

(define (build-boot-func)
	(define icall-clinit (map (lambda (name) (inst-static-call var-void-ret name func-name-clinit null null)) class-names-clinit))
	(define main-arg-types (map cdr (function-args func-main)))
	(reset-parameter-names)
	(define main-arg-vars (map (lambda (x) (iexpr-var (next-parameter-name))) main-arg-types))
	(define icall-main (inst-static-call var-ret-name class-name-main func-name-main main-arg-types main-arg-vars))
	(define iret (inst-ret (iexpr-var var-ret-name)))
	(function func-name-boot 
		(append icall-clinit (list icall-main iret)) 
		(imap-empty default-type) 
		(callee-arg-names (function-args func-main))
		(list (cons var-ret-name (string-id int-type-name))) 
		(string-id int-type-name)))

(define (build-virtual-table mac) 
	(define classes (machine-classes mac))
	(define (process-class cls mac)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
;		(display "sfields:\n")
;		(pretty-print sfields)
		(define vfields (class-vfields cls))

		(define mac-sfuncs (foldl 
			(lambda (sf mac) 
				(define sid (sfunc-id cls-name (function-name sf) (map cdr (function-args sf))))
;				(pretty-print (list "static func:" (function-name sf) sid))
;				(define mem-1 (memory-sforce-write (machine-mem mac) sid sid 0))
				(define fmap-1 (imap-set (machine-fmap mac) sid sf default-type))
				(std:struct-copy machine mac [fmap fmap-1]))
			mac sfuncs))

		(define mac-sfields (foldl 
			(lambda (sf mac) 
;				(pretty-print (sfield-id cls-name (car sf)))
				;[?] why did we use this?
;				(define mem-decl (memory-sdecl (machine-mem mac) (sfield-id cls-name (car sf))))
				;static fields are treated as virtual fields of a special void receiver object
				(define mem-decl (memory-fdecl (machine-mem mac) (vfield-id mac cls-name (car sf))))
				(define tmap-1 (imap-set (machine-tmap mac) (sfield-id cls-name (car sf)) (cdr sf) default-type))
				(std:struct-copy machine mac 
					[mem mem-decl][tmap tmap-1])) 
			mac-sfuncs sfields))

		(define mac-vfuncs (foldl 
			(lambda (vf mac) 
				(define vid (vfunc-id-ori mac cls-name (function-name vf) (map cdr (function-args vf))))
				(define sid (sfunc-id cls-name (function-name vf) (map cdr (function-args vf))))
;				(pretty-print (list "virtual func:" cls-name (function-name vf) sid vid))
				(define mem-1 (memory-fdecl (machine-mem mac) vid)) 
				(define fmap-1 (imap-set (machine-fmap mac) sid vf default-type))
				(std:struct-copy machine mac [mem mem-1] [fmap fmap-1]))
			mac-sfields vfuncs))

;		(display "#3\n")

		(define mac-vfields (foldl 
			(lambda (vf mac) 
				(define tmap-1 (imap-set (machine-tmap mac) (sfield-id cls-name (car vf)) (cdr vf) default-type))
				(define mem-decl (memory-fdecl (machine-mem mac) (vfield-id mac cls-name (car vf))))
				(std:struct-copy machine mac 
					[mem mem-decl][tmap tmap-1])) 
			mac-vfuncs vfields))

;		(println string-id-table)
		mac-vfields)

;	(define mem-push (memory-spush (machine-mem mac)))
	(define mac-cls (foldl process-class mac classes))
	(define mem-reserve-obj (cdr (memory-alloc (machine-mem mac-cls) vt-size)))
	(match-define (cons addr mem-void-receiver) (memory-new mem-reserve-obj))
	(set-void-receiver-addr addr)
	(std:struct-copy machine mac-cls [mem mem-void-receiver]))

(define (variable-definitions->list ast)
;	(display "--------------------var-def->list\n")
	(map variable-definition->pair (syntax-unwrap 2 ast)))

(define (variable-definition->pair ast)
	(define n.t (syntax-unwrap2 2 ast))
	(define name (syntax-unwrap 1 (car n.t)))
	(define type0 (syntax-unwrap 1 (cdr n.t)))
	(define type (if (and (std:string? type0) (string-suffix? type0 "[]")) (std:substring type0 0 (- (std:string-length type0) 2)) type0))
	(cons (string-id name) (string-id type)))


;======================== Instructions ===========================
;assign virtual function to "this" from class information
;"this" should be give by caller of special call
(struct inst-init (classname) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 
		(define classname (inst-init-classname i))
		(define mem-0 (machine-mem m))
		(define addr (memory-sforce-read mem-0 var-this-name 1))

;		(display (~a "classname: " classname "\n"))
;		(display (~a "obj addr: " addr "\n"))

		(define fid-class-name (vfield-id m classname field-name-class))
;		(display (~a "fid-class-name: " fid-class-name "\n"))
		(define maybe-old-name (memory-fread mem-0 fid-class-name addr name-type))
;		(display (~a "maybe-old-name: " maybe-old-name "\n"))
		(define maybe-class-name (if (equal? maybe-old-name (not-found name-type)) classname maybe-old-name))
;		(display (~a "maybe-class-name: " maybe-class-name "\n"))
		(define mem-bind (memory-fwrite mem-0 fid-class-name addr maybe-class-name name-type))

#|
		(define mem-bind (foldl
			(lambda (func mem) 
				(define vid (vfunc-id-ori m classname (function-name func) (map cdr (function-args func))))
				(display (~a "vid: " vid "\n"))
				(define sid (sfunc-id classname (function-name func) (map cdr (function-args func))))
				(display (~a "sid: " sid "\n"))
				(if (is-not-found? (memory-fread mem vid addr))
					(memory-fwrite mem vid addr sid) 
					mem))
			mem-0
			(class-vfuncs (imap-get (machine-cmap m) classname))))
		|#

		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-bind] [pc pc-next]))])


(struct inst-ass (vl vr) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 

		(define mem0 (machine-mem m))
		(match-define (cons v-new v-new-jt) (expr-eval (inst-ass-vr i) m))

;		(display (~a "assignment lvalue: " (inst-ass-vl i) " rvalue: " v-new "\n"))
		(define rhs (lexpr-rhs (inst-ass-vl i)))
		(define mem-new 
			(match rhs
				;[-TODO] expr type
				[(expr-var v) (memory-sforce-write mem0 (string-id (variable-name v)) v-new 0 (jtype->mtype v-new-jt))]
				[(expr-array arr idx)
					(letrec
						([addr (memory-sforce-read mem0 (string-id (variable-name arr)) 0)]
						[idx-e (ast->expression idx)]
						[idx-v (car (expr-eval idx-e m))])
						(memory-awrite mem0 addr idx-v v-new (jtype->mtype v-new-jt)))]
				[(expr-field obj cls fname)
					(letrec
						([addr 
							(if (equal? obj void-receiver) 
								addr-void-receiver
								(memory-sforce-read mem0 (string-id (variable-name obj)) 0))])
						(memory-fwrite mem0 
							(vfield-id m (string-id (type-name-name cls)) (string-id (field-name fname)))
							addr 
							v-new 
							(jtype->mtype v-new-jt)))]
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
			(define cnd-v (car (expr-eval cnd m)))
			(define cases-default (if default-l
				(append cases (list (cons cnd-v default-l)))
				(append cases (list (cons cnd-v (+ 1 (machine-pc m)))))))
			(define label-new (ormap (lambda (k.l)
					(if (equal? (car k.l) cnd-v) (cdr k.l) #f))
				cases-default))
			(define pc-new (imap-get lmap label-new default-type))
			(std:struct-copy machine m [pc pc-new]))]))])

;expr X label(int)
(struct inst-jmp (condition label) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)

		(define lmap (function-lmap f))
		(define iaddr (inst-jmp-label i))
		(define pc-jmp (imap-get lmap iaddr default-type))
		(define pc-next (+ 1 (machine-pc m)))

		(define c (car (expr-eval (inst-jmp-condition i) m)))
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
			(define size (car (expr-eval size-expr m)))
			(match-define (cons addr mem-alloc) (memory-alloc mem-0 size))
;			(display (~a "new array: " addr))
			(define pc-next (+ 1 (machine-pc m)))
			(define mem-ass (memory-sforce-write mem-alloc v-name addr 0 addr-type))
			(std:struct-copy machine m [pc pc-next][mem mem-ass]))]))])

(struct inst-new (v-name) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem-0 (machine-mem m))
		(define v-name (inst-new-v-name i))
		(match-define (cons addr mem-alloc) (memory-new mem-0))
;		(display (~a "new object: " addr))
		(define pc-next (+ 1 (machine-pc m)))
		(define mem-ass (memory-sforce-write mem-alloc v-name addr 0 addr-type))
;		(pretty-print mem-ass)
		(std:struct-copy machine m [pc pc-next][mem mem-ass]))])

(struct inst-ret (v-expr) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		;[TODO] return type
		(define ret-value (car (expr-eval (inst-ret-v-expr i) m)))
		(define ret-jtype (function-ret f))
;		(display (~a "return value:" ret-value "\n"))
		(define mem-ret (memory-sforce-write (machine-mem m) var-ret-name ret-value 0 (jtype->mtype ret-jtype)))
		(std:struct-copy machine m [pc pc-ret][mem mem-ret]))])

(struct inst-long-jump (cls-name func-name) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define sid (sfunc-id (inst-long-jump-cls-name i) (inst-long-jump-func-name i) null))
		(define func (imap-get (machine-fmap m) sid default-type))
		(function-call-simple m func))])

(struct inst-static-call (ret cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem-0 (machine-mem m))
		(define cls-name (inst-static-call-cls-name i))
		(define func-name (inst-static-call-func-name i))
		(define ret (inst-static-call-ret i))
		(define args (map (lambda (arg) (car (expr-eval arg m))) (inst-static-call-args i)))
		(define pc-next (+ 1 (machine-pc m)))
		
		(define mfunc (model-lookup cls-name func-name))
		(if mfunc 
			(std:struct-copy machine m [mem (mfunc mem-0 ret args)][pc pc-next])

			(begin

			(define sid (sfunc-id cls-name func-name (inst-static-call-arg-types i)))
			;no need to read from memory
			(define func (imap-get (machine-fmap m) sid default-type))
			(define ret (inst-static-call-ret i))

			(define mac-ret (function-call m func args))
			(define mem-ret (machine-mem mac-ret))
			;[TODO] return type
			(define ret-value (memory-sforce-read mem-ret var-ret-name 0))
			
			(define mem-pop (memory-spop mem-ret))
			(define mem-ass (memory-sforce-write mem-pop ret ret-value 0 (jtype->mtype (function-ret func))))

			(std:struct-copy machine m [mem mem-ass][pc pc-next]))))])
		
(struct inst-virtual-call (ret obj-name cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		;(println i)
		(define mem-0 (machine-mem m))
		(define obj-addr (memory-sforce-read mem-0 (inst-virtual-call-obj-name i) 0))
		(define cls-name (inst-virtual-call-cls-name i))
		(define func-name (inst-virtual-call-func-name i))
		(define ret (inst-virtual-call-ret i))
		(define args (map (lambda (arg) (car (expr-eval arg m))) (inst-virtual-call-args i)))
		(define pc-next (+ 1 (machine-pc m)))

		(define mfunc (model-lookup cls-name func-name))
		(if mfunc 
			(std:struct-copy machine m [mem (mfunc mem-0 obj-addr ret args)][pc pc-next])

			(begin


			(define vid (vfunc-id-ori m cls-name func-name (inst-virtual-call-arg-types i)))
;			(pretty-print mem-0)
;			(display (~a "vid: " vid "\n"))
;			(display (~a "obj name: " (inst-virtual-call-obj-name i) "\n"))
;			(display (~a "obj addr: " obj-addr "\n"))
			(define fid-class-name (vfield-id m cls-name field-name-class))
;			(display (~a "fid-class-name: " fid-class-name "\n"))
			(define classname-true (memory-fread mem-0 fid-class-name obj-addr name-type))
;			(display (~a "classname: " classname-true "\n"))
			(define sid (sfunc-id-pure classname-true func-name (inst-virtual-call-arg-types i)))
;			(display (~a "sid: " sid "\n"))
			(define func (imap-get (machine-fmap m) sid default-type))
			;push an extra scope to avoid overwriting "this" of the current scope
			(define mem-this (memory-sforce-write (memory-spush mem-0) var-this-name obj-addr 0 addr-type))
			(define mac-this (std:struct-copy machine m [mem mem-this]))

			(define mac-ret (function-call mac-this func args))
			(define mem-ret (machine-mem mac-ret))
			;[TODO] return type
			(define ret-value (memory-sforce-read mem-ret var-ret-name 0))
			;pop callee and callee's "this"
			(define mem-pop (memory-spop (memory-spop mem-ret)))
			(define mem-ass (memory-sforce-write mem-pop (inst-virtual-call-ret i) ret-value 0 (jtype->mtype (function-ret func))))

			(std:struct-copy machine m [mem mem-ass][pc pc-next]))))])

;in my understanding, special call = virtual call - virtual, at least for init
(struct inst-special-call (ret obj-name cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem-0 (machine-mem m))
		(define obj-addr (memory-sforce-read mem-0 (inst-special-call-obj-name i) 0))
		(define cls-name (inst-special-call-cls-name i))
		(define func-name (inst-special-call-func-name i))
		(define ret (inst-special-call-ret i))
		(define args (map (lambda (arg) (car (expr-eval arg m))) (inst-special-call-args i)))
		(define pc-next (+ 1 (machine-pc m)))

		(define mfunc (model-lookup cls-name func-name))
		(if mfunc 
			(std:struct-copy machine m [mem (mfunc mem-0 obj-addr ret args)][pc pc-next])

			(begin

			(define sid (sfunc-id (inst-special-call-cls-name i) (inst-special-call-func-name i) (inst-special-call-arg-types i)))
			;never virtual
			(define func (imap-get (machine-fmap m) sid default-type))
			;push an extra scope to avoid overwriting "this" of the current scope
			(define mem-this (memory-sforce-write (memory-spush mem-0) var-this-name obj-addr 0 addr-type))
			(define mac-this (std:struct-copy machine m [mem mem-this]))

			(define mac-ret (function-call mac-this func args))
			(define mem-ret (machine-mem mac-ret))
			;[TODO] return type
			(define ret-value (memory-sforce-read mem-ret var-ret-name 0))
			;pop callee and callee's "this"
			(define mem-pop (memory-spop (memory-spop mem-ret)))
			(define mem-ass (memory-sforce-write mem-pop (inst-special-call-ret i) ret-value 0 (jtype->mtype (function-ret func))))

			(std:struct-copy machine m [mem mem-ass][pc pc-next]))))])
		


;======================== Expressions ===========================
(struct iexpr-const (value type) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(cons (iexpr-const-value e) (iexpr-const-type e)))])

(struct iexpr-var (name) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		;[-TODO] expr type
		(define name (iexpr-var-name e))
		(define func (machine-fc m))
		(define jtype (if (equal? name var-this-name) (string-id "void") (lookup-type name func)))
		(define ret (if (equal? name var-this-name)
			(memory-sforce-read (machine-mem m) name 1)
			(memory-sforce-read (machine-mem m) name 0)))
;		(display (~a "var read: " (cons (iexpr-var-name e) ret) " type: " jtype "\n"))
;		(defer-eval "var read: " (cons (iexpr-var-name e) ret))
		(cons ret jtype))])

(struct iexpr-binary (op expr1 expr2) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(match-define (cons v1 t1) (expr-eval-dispatch (iexpr-binary-expr1 e) m))
		(match-define (cons v2 t2) (expr-eval-dispatch (iexpr-binary-expr2 e) m))
;		(defer-eval e v1)
;		(defer-eval e v2)
		(define op (iexpr-binary-op e))
		(define tr (op-return-type op t1 t2))
;		(display (~a "Binary op v1: " v1 " v2: " v2 " result: " (op v1 v2) "\n"))
		(cons (op v1 v2) tr))])

(struct iexpr-array (arr-name index) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m) 
		(define mem0 (machine-mem m))
		(define arr-name (iexpr-array-arr-name e))
		(define func (machine-fc m))
		(define jtype (lookup-type arr-name func))
		(define arr-addr (memory-sforce-read mem0 arr-name 0))
		(match-define (cons idx itype) (expr-eval-dispatch (iexpr-array-index e) m))
		(define ret (memory-aread mem0 arr-addr idx (jtype->mtype jtype)))
;		(display (~a "array read index: " idx " value: " ret))
		(cons ret jtype))])

(struct iexpr-field (obj-name cls-name fname) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define mem0 (machine-mem m))
		(define fname (iexpr-field-fname e))
		(define cls-name (iexpr-field-cls-name e))
		(define obj-name (iexpr-field-obj-name e))
		;[-TODO] field type
		(define jtype (imap-get (machine-tmap m) (sfield-id cls-name fname) default-type))
		(define mtype (jtype->mtype jtype))
		(define ret 
			(let
				([obj-addr (if (equal? obj-name var-void-receiver-name) addr-void-receiver
					(memory-sforce-read mem0 obj-name 0))])
				(memory-fread mem0 (vfield-id m cls-name fname) obj-addr mtype)))
;		(defer-eval "field read: " (list obj-name cls-name fname ret))
;		(display (~a "field read: " (list obj-name cls-name fname ret) "\n"))
		(cons ret jtype))])

