#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;======================== Definitions ===========================
;mem: memory
;pc: int
;boot: boot function
;classes: list of classes
(struct machine (boot classes mem pc) #:transparent)

;fields: list of var names
;functs: list of functions
;field-val-ast: init value of fields in ast, #f if no init value
(struct class (sfuncs vfuncs sfields vfields) #:transparent)

;name: string
;prog: list of instructions;
;lmap: imap: label(int) -> instruction index(int)
;args: list of strings
;locals: list of strings
(struct function (name prog lmap args locals) #:transparent)

;inst-exec: machine(before exec) -> machine(after exec)
(define-generics instruction
	[inst-exec instruction machine function])

;expr-eval: expr -> ret(int/bool) X machine(after side effect))
(define-generics expression
	[expr-eval expression machine])

;[TODO?] What is the clean way to do this?
(define (expr-eval-dispatch e m) (expr-eval e m))

(define pc-ret -1)
(define pc-init 0)
(define var-ret-name "__return__")
(define var-this-name "this")
(define func-name-main "main")
(define func-name-boot "__boot__")
(define machine-empty (machine #f null memory-empty pc-init))


;======================== Execution Interface ===========================
;machine(init) X list of names -> machine(fin)
(define (compute mac args)
	(function-call mac (machine-boot mac) args))

(define (function-call mac func args)
	(define mac-reset (std:struct-copy machine mac [pc pc-init][mem (memory-spush (machine-mem mac))]))
	(define mac-decl (std:struct-copy machine mac-reset [mem 
		(foldl 
			(lambda (name mem) (memory-sdecl mem name)) 
			(machine-mem mac-reset) 
			(append (function-args func) (function-locals func)))]))
	(define mac-input (std:struct-copy machine mac-decl [mem
		(foldl 
			(lambda (name0 name1 mem) (memory-swrite mem (expr-eval name1 mac) (memory-sread (machine-mem mac) name0)))
			(machine-mem mac-decl)
			args 
			(function-args func))]))
	(function-exec mac-input func))

(define (function-exec mac func)
	(if (= (machine-pc mac) pc-ret) 
		mac
		(let ([inst-cur (list-ref (function-prog func) (machine-pc mac))])
			(function-exec (inst-exec inst-cur mac) func))))

;machine X list of (key, value) -> machine(with input inserted into memory)
(define (assign-input mac input)
	(define mem0 (memory-spush (machine-mem mac)))
	(define mem-ass 
		(foldl (lambda (kv mem-cur) (memory-swrite (memory-sdecl mem-cur (car kv)) (car kv) (cdr kv))) mem0 input))
	(std:struct-copy machine mac [mem mem-ass]))

;machine X list of (key, value) -> boolean
(define (compare-output mac output)
	(define mem0 (machine-mem mac))
	(foldl (lambda (kv fml-cur) (= (cdr kv) (memory-sread mem0 (car kv)))) #t output))

;======================== Execution Interface ===========================


;======================== AST Interpreter ===========================
(define (ast->machine ast)
	(define classes (foldl 
		(lambda (class-ast cl) (cons (ast->class class-ast) cl))
		null
		(class-list-cl (program-rhs ast))))
;	(define globals (foldl
;		(lambda (cls gl) (foldl
;			(lambda (sfield sfield-val-ast gl) (if sfield-val-ast (cons (cons sfield sfield-val-ast) gl) gl))
;			null
;			(class-sfields cls) (class-sfield-val-asts cls)))
;		null
;		classes))
	(define boot (build-boot-func))
	(define mem (build-virtual-table memory-empty))
	(machine boot classes mem pc-init))

(define (ast->class ast)
	(match (class-def-rhs ast)
		[(class-default globals-ast fields-ast sfuncs-ast vfuncs-ast)
			(letrec 
				([sfuncs 
					(map (lambda (ast) (ast->function ast)) (function-list-fl (function-declares-rhs sfuncs-ast)))]
				[vfuncs
					(map (lambda (ast) (ast->function ast)) (function-list-fl (function-declares-rhs vfuncs-ast)))]
				[fields 
					(map (lambda (ast) (field-name ast)) (field-list-fl (field-declares-rhs fields-ast)))]
				[globals
					(map (lambda (ast) (field-name ast)) (field-list-fl (field-declares-rhs globals-ast)))])
				(class sfuncs vfuncs globals fields))]))

;(struct function (name prog lmap args locals) #:transparent)
;(LHS-C function-declare (rhs ::= function-content))
;	(RHS-C function-content (name : function-name) (args : arguments) (local-variables : variable-declares) (statements : stats))
(define (ast->function ast)
	(match (function-declare-rhs ast)
		[(function-content name-ast args-ast local-vars-ast statements-ast)
			(letrec 
				([name (func-name-name name-ast)]
				[args (map (lambda (ast) (variable-name ast)) args-ast)]
				[local-vars (map 
					(lambda (ast) (variable-name ast))
					(variable-list-vl (variable-declares-rhs local-vars-ast)))]
				[func-sig (function name null imap-empty args local-vars)])
				(foldl (lambda (st func)
					(letrec ([ret-pair (ast->instruction st (function-lmap func) (length (function-prog func)))]
						[inst (car ret-pair)]
						[lmap-new (cdr ret-pair)])
						(std:struct-copy function func 
							[prog (if inst (append (function-prog func) (list inst)) (function-prog func))]
							[lmap lmap-new])))
					func-sig
					(stat-list-sl (stats-rhs statements-ast))))]))

;ast X lmap X line-number -> instruction X lmap(updated)
(define (ast->instruction ast lmap line-num)
	(match ast
		[(stat s) (ast->instruction s lmap line-num)]
		[(stat-ass name rvalue) (cons (inst-ass (variable-name name) (ast->expression rvalue)) lmap)]
		[(stat-jmp condition target) (cons (inst-jmp (ast->expression condition) (label-v target)) lmap)]
		[(stat-label here) (cons #f (imap-set lmap (label-v here) line-num))]
		[(stat-nop any) (cons (inst-nop nullptr) lmap)]
		[(stat-ret any) (cons (inst-ret nullptr) lmap)]
		[(stat-static-call ret func args) 
			(cons 
				(inst-static-call (variable-name ret) (function-name func) 
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]
		[(stat-virtual-call ret obj func args)
			(cons 
				(inst-virtual-call (variable-name ret) (variable-name obj) (function-name func) 
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]
		[(stat-special-call ret obj func args)
			(cons 
				(inst-static-call (variable-name ret) (variable-name obj) (function-name func) 
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]))

(define (ast->expression ast)
	(match ast
		[(expr e) (ast->expression e)]
		[(lexpr e) (ast->expression e)]
		[(dexpr e) (ast->expression e)]
		[(expr-const c) (iexpr-const (const-v c))]
		[(expr-var v) (iexpr-var (variable-name v))]
		[(expr-binary expr1 o expr2) (iexpr-binary (op-v o) (ast->expression expr1) (ast->expression expr2))]
		[(expr-array array index) (iexpr-array (variable-name array) (ast->expression index))]
		[(expr-field obj fname) (iexpr-field (variable-name obj) (field-name fname))]))

(define (build-boot-func)
	;(define iboot (inst-boot globals))
	(define icall (inst-static-call var-ret-name func-name-main null))
	(function func-name-boot (list icall) imap-empty null null))

(define (build-virtual-table classes mem) 
	(define (process-class cls mem)
		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
		(define vfields (class-vfields cls))
		(define mem-sfuncs (foldl (lambda (sf mem) (memory-sforce-write mem (function-name sf) sf)) mem sfuncs))
		(define mem-sfields (foldl (lambda (sf mem) (memory-sdecl mem sf)) mem-sfuncs sfields))
		(define mem-vfuncs (foldl (lambda (vf mem) (memory-fdecl mem vf)) mem-sfields vfuncs))
		(define mem-vfields (foldl (lambda (vf mem) (memory-fdecl mem vf)) mem-vfuncs vfields))
		mem-vfields)
	(define mem-push (memory-spush mem))
	(foldl process-class mem-push classes))


;======================== Instructions ===========================
;globals: list of (cons var-name(string) value(ast))
(struct inst-boot (globals) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 
		(define (init-var vi mem)
			(define name (car vi))
			(define value-ast (cdr vi))
			(define value (expr-eval (ast->expression value-ast)))
			(memory-swrite mem name value))
		(foldl init-var (machine-mem m) (inst-boot-globals i))
	)])

;assign virtual function to "this" from class information
;"this" should be give by caller of special call
(struct inst-init (class) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 
		(define mem-0 (machine-mem m))
		(define addr (memory-sread mem-0 var-this-name))

		(define mem-bind-func (foldl
			(lambda (func mem) 
				(if (= (memory-fread (function-name func) addr) not-found) 
					(memory-fwrite (function-name func) addr func) 
					mem))
			mem-0
			(class-vfuncs (inst-init-class i))))

		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-bind-func] [pc pc-next]))])

;addr(int) X iexpr
(struct inst-ass (vl vr) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 

		(define mem0 (machine-mem m))
		(define v-new (expr-eval (inst-ass-vr i) m))

		(define mem-new 
			(match (inst-ass-vl i)
				[(expr-var v) (memory-swrite mem0 (variable-name v) v-new)]
				[(expr-array arr idx)
					(letrec
						([addr (memory-sread mem0 (variable-name arr))]
						[idx-e (ast->expression idx)]
						[idx-v (expr-eval idx-e m)])
						(memory-awrite mem0 addr idx-v v-new))]
				[(expr-field obj fname)
					(letrec
						([addr (memory-sread mem0 (variable-name obj))])
						(memory-fwrite mem0 addr (field-name fname) v-new))]))

		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-new] [pc pc-next]))])

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

(struct inst-ret (v) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define ret-value (memory-sread (machine-mem m) (inst-ret-v i)))
		(define mem-ret (memory-sforce-write (machine-mem m) var-ret-name ret-value))
		(std:struct-copy machine m [pc pc-ret][mem mem-ret]))])

(struct inst-static-call (ret func-name args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define func (memory-sread (machine-mem m) (inst-static-call-func-name i)))
		(define args (inst-static-call-args i))
		(define ret (inst-static-call-ret i))

		(define mac-ret (function-call m func args))
		(define mem-ret (machine-mem mac-ret))
		(define ret-value (memory-sread mem-ret var-ret-name))
		
		(define mem-pop (memory-spop mem-ret))
		(define mem-ass (memory-swrite mem-pop ret))
		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-ass][pc pc-next]))])
		
(struct inst-virtual-call (ret obj-name func-name args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem0 (machine-mem m))
		(define obj-addr (memory-sread mem0 (inst-virtual-call-obj-name i)))
		;virtual
		(define func (memory-fread mem0 obj-addr (inst-virtual-call-func-name i)))
		(define args (inst-static-call-args i))
		;push an extra scope to avoid overwriting "this" of the current scope
		(define mem-this (memory-sforce-write (memory-spush mem0) var-this-name obj-addr))
		(define mac-this (std:struct-copy machine m [mem mem-this]))

		(define mac-ret (function-call mac-this func args))
		(define mem-ret (machine-mem mac-ret))
		(define ret-value (memory-sread mem-ret var-ret-name))
		;pop callee and callee's "this"
		(define mem-pop (memory-spop (memory-spop mem-ret)))
		(define mem-ass (memory-swrite mem-pop (inst-virtual-call-ret i)))
		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-ass][pc pc-next]))])

;in my understanding, special call = virtual call - virtual, at least for init
(struct inst-special-call (ret obj-name func-name args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem0 (machine-mem m))
		(define obj-addr (memory-sread mem0 (inst-special-call-obj-name i)))
		;never virtual
		(define func (memory-sread (machine-mem m) (inst-special-call-func-name i)))
		(define args (inst-static-call-args i))
		;push an extra scope to avoid overwriting "this" of the current scope
		(define mem-this (memory-sforce-write (memory-spush mem0) var-this-name obj-addr))
		(define mac-this (std:struct-copy machine m [mem mem-this]))

		(define mac-ret (function-call mac-this func args))
		(define mem-ret (machine-mem mac-ret))
		(define ret-value (memory-sread mem-ret var-ret-name))
		;pop callee and callee's "this"
		(define mem-pop (memory-spop (memory-spop mem-ret)))
		(define mem-ass (memory-swrite mem-pop (inst-special-call-ret i)))
		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-ass][pc pc-next]))])
		


;======================== Expressions ===========================
(struct iexpr-const (value) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(iexpr-const-value e))])

(struct iexpr-var (name) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(memory-sread (machine-mem m) (iexpr-var-name e)))])

(struct iexpr-binary (op expr1 expr2) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define v1 (expr-eval-dispatch (iexpr-binary-expr1 e) m))
		(define v2 (expr-eval-dispatch (iexpr-binary-expr2 e) m))
		((iexpr-binary-op e) v1 v2))])

(struct iexpr-array (arr-name index) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m) 
		(define mem0 (machine-mem m))
		(define arr-addr (memory-sread mem0 (iexpr-array-arr-name e)))
		(define idx (expr-eval-dispatch (iexpr-array-index e) m))
		(memory-aread mem0 arr-addr idx))])

(struct iexpr-field (obj-name fname) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define mem0 (machine-mem m))
		(define obj-addr (memory-sread mem0 (iexpr-field-obj-name e)))
		(define fname (iexpr-field-fname e))
		(memory-fread mem0 fname obj-addr))])

