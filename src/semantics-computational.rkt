#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;======================== Definitions ===========================
;prog: list of instructions;
;lmap: imap: label(int) -> instruction index(int)
(struct function (name prog lmap args locals) #:transparent)

;mem: memory
;pc: int
(struct machine (funcs mem pc) #:transparent)

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
(define machine-empty (machine null memory-empty pc-init))


;======================== Execution Interface ===========================
;machine(init) X list of names -> machine(fin)
(define (compute mac args)
	(function-call mac (list-ref (machine-funcs mac) 0) args))

(define (function-call mac func args)
	(define mac-reset (std:struct-copy machine mac [pc pc-init][mem (memory-spush (machine-mem mac))]))
	(define mac-decl (std:struct-copy machine mac-reset [mem 
		(foldl 
			(lambda (name mem) (memory-sdecl mem name)) 
			(machine-mem mac-reset) 
			(append (function-args func) (function-locals func)))]))
	(define mac-input (std:struct-copy machine mac-decl [mem
		(foldl 
			(lambda (name0 name1 mem) (memory-swrite mem name1 (memory-sread (machine-mem mac) name0)))
			(machine-mem mac-decl)
			args 
			(function-args func))]))
	(function-exec mac-input func))

(define (function-exec mac func)
	(if (= (machine-pc mac) pc-ret) 
		mac
		(let ([inst-cur (list-ref (machine-prog m) (machine-pc m))])
			(function-exec (inst-exec inst-cur m) func))))

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



;======================== AST Interpreter ===========================
(define (ast->machine ast)
	(

(define (ast->function ast)

;ast -> instruction X function(lmap updated)
(define (ast->instruction ast m)
	(match ast
		[(stat s) (ast->instruction s m)]
		[(stat-ass addr rvalue) (cons (inst-ass (variable-v addr) (ast->expression rvalue)) m)]
		[(stat-jmp condition target) (cons (inst-jmp (ast->expression condition) (label-v target)) m)]
		[(stat-label here)
			(begin
				(define lmap-new (imap-set (machine-lmap m) (label-v here) (length (machine-prog m))))
				(cons #f (std:struct-copy machine m [lmap lmap-new])))]
		[(stat-nop any) (cons (inst-nop nullptr) m)]
		[(stat-ret any) (cons (inst-ret nullptr) m)]))

(define (ast->expression ast)
	(match ast
		[(expr e) (ast->expression e)]
		[(lexpr e) (ast->expression e)]
		[(dexpr e) (ast->expression e)]
		[(expr-const c) (iexpr-const (const-v c))]
		[(expr-var v) (iexpr-var (variable-v v))]
		[(expr-binary expr1 o expr2) (iexpr-binary (op-v o) (ast->expression expr1) (ast->expression expr2))]))

(define (build-boot-func funcs fields globals)
	(define iboot (inst-boot funcs fields globals))
	(define icall (inst-static-call var-ret-name func-name-main null))
	(function func-name-boot (cons iboot icall) imap-empty null null))



;======================== Instructions ===========================
;funcs: list of functions
;fields: list of field names
;globals: list of ast(variable-init)
(struct inst-boot (sfuncs mfuncs fields globals) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 
		(define sfuncs (inst-boot-sfuncs i))
		(define mfuncs (inst-boot-mfuncs i))
		(define fields (inst-boot-fields i))
		(define globals (inst-boot-globals i))

		(define mem0 (machine-mem m))
		(define mem-push (memory-spush mem0))
		(define mem-sfuncs (foldl (lambda (func mem) (memory-sforce-write mem (function-name func) func)) mem-push funcs))
		(define mem-fields (foldl (lambda (field mem) (memory-fdecl mem field)) mem-funcs fields)))])




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
						(memory-fwrite mem0 addr (field-name fname) v-new))]

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
		(define func (memory-sread (machine-mem m) func-name))
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
		(define obj-addr (memory-sread mem0 obj-name))
		(define func (memory-fread mem0 obj-addr func-name))
		(define args (inst-static-call-args i))
		;push an extra scope to avoid overwriting "this" of the current scope
		(define mem-this (memory-sforce-write (memory-spush mem0) var-this-name obj-addr))
		(define mac-this (std:struct-copy machine m [mem mem-this]))

		(define mac-ret (function-call mac-this func args))
		(define mem-ret (machine-mem mac-ret))
		(define ret-value (memory-sread mem-ret var-ret-name))
		;pop callee and callee's "this"
		(define mem-pop (memory-spop (memory-spop mem-ret)))
		(define mem-ass (memory-swrite mem-pop ret))
		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-ass][pc pc-next]))])
		


;======================== Expressions ===========================
(struct iexpr-const (value) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(iexpr-const-value e))])

(struct iexpr-var (addr) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define value (memory-load (machine-mem m) (iexpr-var-addr e) ))
		value)])

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

