#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require "string-id.rkt")
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;======================== Definitions ===========================
;mem: memory
;pc: int
;boot: boot function
;classes: list of classes
(struct machine (boot classes cmap mem pc) #:transparent)

;fields: list of var names
;functs: list of functions
;field-val-ast: init value of fields in ast, #f if no init value
(struct class (name extend implements sfuncs vfuncs sfields vfields) #:transparent)

;name: string
;prog: list of instructions;
;lmap: imap: label(int) -> instruction index(int)
;args: list of (string(name) X string(type))
;locals: list of (string(name) X string(type))
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
(define machine-empty (machine #f null imap-empty memory-empty pc-init))

(define class-name-main "dummy")


;============================= Utils ===================================
(define (lookup-virtual-function mac cls func arg-types) 
	(if cls
		(begin
			(define cls-0 (imap-get (machine-cmap mac) cls))

			(define base-name (ormap 
				(lambda (cls-cur) (lookup-virtual-function cls-cur func arg-types)) 
				(cons (class-extend cls-0) (class-implements cls-0))))

			(if base-name base-name
				(if 
					(ormap (lambda (func-cur) (invoke-same-sig? func-cur func arg-types)) (class-vfuncs cls-0)) 
					(vfunc-sig->string cls func arg-types)
					#f)))
		#f))

(define (lookup-virtual-field mac cls field)
	(if cls
		(begin
			(define cls-0 (imap-get (machine-cmap mac) cls))

			(define base-name (ormap 
				(lambda (cls-cur) (lookup-virtual-field mac cls-cur field)) 
				(cons (class-extend cls-0) (class-implements cls-0))))

			(if base-name base-name
				(if 
					(ormap (lambda (f) (equal? f field)) (class-vfields cls-0)) 
					(std:string-append cls "::::" field)
					#f)))
		#f))

(define (vfunc-id mac cls func arg-types) (string-id (lookup-virtual-function cls func arg-types)))

(define (vfield-id mac cls field) (string-id (lookup-virtual-field mac cls field)))

(define (sfunc-id cls func arg-types) (string-id (sfunc-sig->string cls func arg-types)))

(define (sfield-id cls field) (string-id (std:string-append cls "::" field)))

;signature of a field is its name
;signature of a function is its name and arg types
(define (function-same-sig? func-1 func-2) 
	(and
		(equal? (function-name func-1) (function-name func-2))
		(andmap (lambda (arg-1 arg-2) (equal? (cdr arg-1) (cdr arg-2))) (function-args func-1) (function-args func-2))))

(define (invoke-same-sig? func invoked-name invoked-arg-types)
	(and
		(equal? (function-name func) invoked-name)
		(andmap (lambda (arg-1 arg-2) (equal? (cdr arg-1) arg-2)) (function-args func) invoked-arg-types)))

(define (vfunc-sig->string cls func arg-types)
	(apply std:string-append 
		(append 
			(list cls "::::" func)
			(foldl (lambda (s l) (cons "," (cons s l))) null arg-types))))

(define (sfunc-sig->string cls func arg-types)
	(apply std:string-append 
		(append 
			(list cls "::" func)
			(foldl (lambda (s l) (cons "," (cons s l))) null arg-types))))

(define (print-machine m)
	(begin
		(display "======================\n")
		(display "Boot Function:\n")
		(print-func (machine-boot m) "    ")
		(display "Classes:\n")
		(map (lambda (c) (print-class c "    ")) (machine-classes m))
		(display "Class Name Map:\n")
		(display "    ")
		(println (machine-cmap m))
		(display "Memory:\n")
		(display "    ")
		(println (machine-mem m))
		(display "PC:\n")
		(display "    ")
		(println (machine-pc m))
		(display "======================\n")))

;(struct class (name extend implements sfuncs vfuncs sfields vfields) #:transparent)
(define (print-class c indent)
	(begin
		(display indent)
		(println (class-name c))
		(display indent)
		(println (class-extend c))
		(display indent)
		(println (class-implements c))
		(map (lambda (f) (print-func f (std:string-append indent "    "))) (class-sfuncs c))
		(map (lambda (f) (print-func f (std:string-append indent "    "))) (class-vfuncs c))
		(map (lambda (v) (begin (display indent) (println v))) (class-sfields c))
		(map (lambda (v) (begin (display indent) (println v))) (class-vfields c))))
		

;(struct function (name prog lmap args locals) #:transparent)
(define (print-func f indent)
	(begin
		(display indent)
		(println (function-name f))
		(display indent)
		(println (function-prog f))
		(display indent)
		(println (function-lmap f))
		(display indent)
		(println (function-args f))
		(display indent)
		(println (function-locals f))))


;======================== Execution Interface ===========================
;machine(init) X list of names -> machine(fin)
(define (compute mac)
	(function-call mac (machine-boot mac) null))

(define (function-call mac func args)
	(define mac-reset (std:struct-copy machine mac [pc pc-init][mem (memory-spush (machine-mem mac))]))
	(define mac-decl (std:struct-copy machine mac-reset [mem 
		(foldl 
			(lambda (var-def mem) (memory-sdecl mem (string-id (car var-def)))) 
			(machine-mem mac-reset) 
			(append (function-args func) (function-locals func)))]))
	(define mac-input (std:struct-copy machine mac-decl [mem
		(foldl 
			(lambda (name0 name1 mem) (memory-swrite mem (string-id (car name1)) (memory-sread (machine-mem mac) name0)))
			(machine-mem mac-decl)
			args 
			(function-args func))]))
	(function-exec mac-input func))

(define (function-exec mac func)
	(if (= (machine-pc mac) pc-ret) 
		mac
		(let ([inst-cur (list-ref (function-prog func) (machine-pc mac))])
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
	(foldl (lambda (kv fml-cur) (= (cdr kv) (memory-sread mem0 (string-id (car kv))))) #t output))

;machine X list of string(output var names)
;(define (get-output mac output-names)
;	(define mem0 (machine-mem mac))
;	(map (lambda (name) (memory-sread mem0 (string-id name))) output-names))

;======================== AST Interpreter ===========================
(define (ast->machine ast)
	(define classes (foldl 
		(lambda (class-ast cl) (cons (ast->class class-ast) cl))
		null
		(class-list-cl (program-rhs ast))))
	(define cmap (foldl (lambda (cls cm) (imap-set cm (class-name cls) cls)) imap-empty classes))
	(define boot (build-boot-func))
	(define mac-init (machine boot classes cmap memory-empty pc-init))
	(build-virtual-table classes mac-init))

(define (ast->class ast)
	(match (class-def-rhs ast)
		[(class-default name-ast extend-ast interfaces-ast globals-ast fields-ast sfuncs-ast vfuncs-ast)
			(letrec 
				([name (type-name-name name-ast)]
				[extend (type-name-name extend-ast)]
				[interfaces 
					(map (lambda (ast) (type-name-name ast)) (interface-name-list-il (interface-implements-rhs interfaces-ast)))]
				[sfuncs 
					(map (lambda (ast) (ast->function name ast)) (function-list-fl (function-declares-rhs sfuncs-ast)))]
				[vfuncs
					(map (lambda (ast) (ast->function name ast)) (function-list-fl (function-declares-rhs vfuncs-ast)))]
				[fields 
					(map (lambda (ast) (field-name ast)) (field-list-fl (field-declares-rhs fields-ast)))]
				[globals
					(map (lambda (ast) (field-name ast)) (field-list-fl (field-declares-rhs globals-ast)))])
				(class name extend interfaces sfuncs vfuncs globals fields))]))

;(struct function (name prog lmap args locals) #:transparent)
;(LHS-C function-declare (rhs ::= function-content))
;	(RHS-C function-content (name : function-name) (args : arguments) (local-variables : variable-declares) (statements : stats))
(define (ast->function classname ast)
	(match (function-declare-rhs ast)
		[(function-content name-ast args-ast local-vars-ast statements-ast)
			(letrec 
				([name (func-name-name name-ast)]
				[args (variable-definitions->list args-ast)]
				[local-vars (variable-definitions->list local-vars-ast)]
				[func-sig (function name null imap-empty args local-vars)])
				(begin
					(if (equal? name func-name-main) (set! class-name-main classname) #f)
					(foldl (lambda (st func)
						(letrec ([ret-pair (ast->instruction st (function-lmap func) (length (function-prog func)))]
							[inst (car ret-pair)]
							[lmap-new (cdr ret-pair)])
							(std:struct-copy function func 
								[prog (if inst (append (function-prog func) (list inst)) (function-prog func))]
								[lmap lmap-new])))
						func-sig
						(stat-list-sl (stats-rhs statements-ast)))))]))

;ast X lmap X line-number -> instruction X lmap(updated)
(define (ast->instruction ast lmap line-num)
	(match ast
		[(stat s) (ast->instruction s lmap line-num)]
		[(stat-ass target rvalue) (cons (inst-ass target (ast->expression rvalue)) lmap)]
		[(stat-jmp condition target) (cons (inst-jmp (ast->expression condition) (label-v target)) lmap)]
		[(stat-label here) (cons #f (imap-set lmap (label-v here) line-num))]
		[(stat-nop any) (cons (inst-nop nullptr) lmap)]
		[(stat-ret v) (cons (inst-ret (variable-name v)) lmap)]
		[(stat-static-call ret cls-name func arg-types args) 
			(cons 
				(inst-static-call (variable-name ret) (type-name-name cls-name) (function-name func) 
					(map (lambda (ast) (type-name-name ast)) (type-list-tl (types-rhs arg-types)))
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]
		[(stat-virtual-call ret obj cls-name func arg-types args)
			(cons 
				(inst-virtual-call (variable-name ret) (variable-name obj) (type-name-name cls-name) (function-name func) 
					(map (lambda (ast) (type-name-name ast)) (type-list-tl (types-rhs arg-types)))
					(map ast->expression (argument-caller-list-al (arguments-caller-rhs args))))
				lmap)]
		[(stat-special-call ret obj cls-name func arg-types args)
			(cons 
				(inst-special-call (variable-name ret) (variable-name obj) (type-name-name cls-name) (function-name func) 
					(map (lambda (ast) (type-name-name ast)) (type-list-tl (types-rhs arg-types)))
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
		[(expr-field obj class fname) (iexpr-field (variable-name obj) (type-name-name class) (field-name fname))]))

(define (build-boot-func)
	;(define iboot (inst-boot globals))
	(define icall (inst-static-call var-ret-name class-name-main func-name-main null null))
	(define iret (inst-ret var-ret-name))
	(function func-name-boot (list icall iret) imap-empty null (list (cons var-ret-name "int"))))

(define (build-virtual-table classes mac) 
	(define (process-class cls mem)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
		(define vfields (class-vfields cls))

		(define mem-sfuncs (foldl (lambda (sf mem) (memory-sforce-write mem 
			(sfunc-id cls-name (function-name sf) (map cdr (function-args sf))) sf)) mem sfuncs))
		(define mem-sfields (foldl (lambda (sf mem) (memory-sdecl mem (sfield-id cls-name sf))) mem-sfuncs sfields))

		(define mem-vfuncs (foldl (lambda (vf mem) (memory-fdecl mem 
			(vfunc-id cls-name (function-name vf) (map cdr (function-args vf))))) mem-sfields vfuncs))
		(define mem-vfields (foldl (lambda (vf mem) (memory-fdecl mem (vfield-id mac cls-name vf))) mem-vfuncs vfields))

		mem-vfields)

	(define mem-push (memory-spush (machine-mem mac)))
	(std:struct-copy machine mac [mem (foldl process-class mem-push classes)]))

(define (variable-definitions->list ast)
	(map 
		(lambda (ast) (variable-definition->pair ast))
		(variable-definition-list-vl (variable-definitions-rhs ast))))

(define (variable-definition->pair ast)
	(cons 
		(variable-name (variable-n-type-name (variable-definition-rhs ast))) 
		(type-name-name (variable-n-type-type (variable-definition-rhs ast)))))


;======================== Instructions ===========================
;globals: list of (cons var-name(string) value(ast))
(struct inst-boot (globals) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 
		(define (init-var vi mem)
			(define name (car vi))
			(define value-ast (cdr vi))
			(define value (expr-eval (ast->expression value-ast)))
			(memory-swrite mem (string-id name) value))
		(foldl init-var (machine-mem m) (inst-boot-globals i))
	)])

;assign virtual function to "this" from class information
;"this" should be give by caller of special call
(struct inst-init (class) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f) 
		(define mem-0 (machine-mem m))
		(define addr (memory-sread mem-0 (string-id var-this-name)))

		(define mem-bind-func (foldl
			(lambda (func mem) 
				(if (= (memory-fread (string-id (function-name func)) addr) not-found) 
					(memory-fwrite (string-id (function-name func)) addr func) 
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

		(println v-new)
		(define mem-new 
			(match (lexpr-rhs (inst-ass-vl i))
				[(expr-var v) (memory-swrite mem0 (string-id (variable-name v)) v-new)]
				[(expr-array arr idx)
					(letrec
						([addr (memory-sread mem0 (string-id (variable-name arr)))]
						[idx-e (ast->expression idx)]
						[idx-v (expr-eval idx-e m)])
						(memory-awrite mem0 addr idx-v v-new))]
				[(expr-field obj cls fname)
					(letrec
						([addr (memory-sread mem0 (string-id (variable-name obj)))])
						(memory-fwrite mem0 addr (vfield-id (type-name-name cls) (field-name fname)) v-new))]))

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
		(println (inst-ret-v i))
		(define ret-value (memory-sread (machine-mem m) (string-id (inst-ret-v i))))
		(println ret-value)
		(define mem-ret (memory-sforce-write (machine-mem m) (string-id var-ret-name) ret-value))
		(std:struct-copy machine m [pc pc-ret][mem mem-ret]))])

(struct inst-static-call (ret cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(println i)
		(define func (memory-sread (machine-mem m) (sfunc-id (inst-static-call-cls-name i) (inst-static-call-func-name i) (inst-static-call-arg-types i))))
		(define args (map car (inst-static-call-args i)))
		(define ret (string-id (inst-static-call-ret i)))
		(println++ "Ret Var: " ret)

		(define mac-ret (function-call m func args))
		(define mem-ret (machine-mem mac-ret))
		(define ret-value (memory-sread mem-ret (string-id var-ret-name)))
		
		(define mem-pop (memory-spop mem-ret))
		(define mem-ass (memory-swrite mem-pop ret ret-value))
		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-ass][pc pc-next]))])
		
(struct inst-virtual-call (ret obj-name cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem0 (machine-mem m))
		(define obj-addr (memory-sread mem0 (string-id (inst-virtual-call-obj-name i))))
		;virtual
		(define func (memory-fread mem0 obj-addr (vfunc-id m (inst-virtual-call-cls-name i) (inst-virtual-call-func-name i) (inst-virtual-call-args i))))
		(define args (map car (inst-static-call-args i)))
		;push an extra scope to avoid overwriting "this" of the current scope
		(define mem-this (memory-sforce-write (memory-spush mem0) (string-id var-this-name) obj-addr))
		(define mac-this (std:struct-copy machine m [mem mem-this]))

		(define mac-ret (function-call mac-this func args))
		(define mem-ret (machine-mem mac-ret))
		(define ret-value (memory-sread mem-ret (string-id var-ret-name)))
		;pop callee and callee's "this"
		(define mem-pop (memory-spop (memory-spop mem-ret)))
		(define mem-ass (memory-swrite mem-pop (string-id (inst-virtual-call-ret i)) ret-value))
		(define pc-next (+ 1 (machine-pc m)))

		(std:struct-copy machine m [mem mem-ass][pc pc-next]))])

;in my understanding, special call = virtual call - virtual, at least for init
(struct inst-special-call (ret obj-name cls-name func-name arg-types args) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m f)
		(define mem0 (machine-mem m))
		(define obj-addr (memory-sread mem0 (string-id (inst-special-call-obj-name i))))
		;never virtual
		(define func (memory-sread (machine-mem m) (sfunc-id (inst-special-call-cls-name i) (inst-special-call-func-name i) (inst-special-call-args i))))
		(define args (map car inst-special-call-args i))
		;push an extra scope to avoid overwriting "this" of the current scope
		(define mem-this (memory-sforce-write (memory-spush mem0) (string-id var-this-name) obj-addr))
		(define mac-this (std:struct-copy machine m [mem mem-this]))

		(define mac-ret (function-call mac-this func args))
		(define mem-ret (machine-mem mac-ret))
		(define ret-value (memory-sread mem-ret (string-id var-ret-name)))
		;pop callee and callee's "this"
		(define mem-pop (memory-spop (memory-spop mem-ret)))
		(define mem-ass (memory-swrite mem-pop (string-id (inst-special-call-ret i)) ret-value))
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
		(memory-sread (machine-mem m) (string-id (iexpr-var-name e))))])

(struct iexpr-binary (op expr1 expr2) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
;		(println++ "Binary Expr: " e)
		(define v1 (expr-eval-dispatch (iexpr-binary-expr1 e) m))
		(define v2 (expr-eval-dispatch (iexpr-binary-expr2 e) m))
;		(println++ "v1: " v1)
;		(println++ "v2: " v2)
		((iexpr-binary-op e) v1 v2))])

(struct iexpr-array (arr-name index) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m) 
		(define mem0 (machine-mem m))
		(define arr-addr (memory-sread mem0 (string-id (iexpr-array-arr-name e))))
		(define idx (expr-eval-dispatch (iexpr-array-index e) m))
		(memory-aread mem0 arr-addr idx))])

(struct iexpr-field (obj-name cls-name fname) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define mem0 (machine-mem m))
		(define fname (iexpr-field-fname e))
		(define cls-name (iexpr-field-cls-name e))
		(define obj-name (iexpr-field-obj-name e))
		(define obj-addr (memory-sread mem0 (string-id obj-name)))
		(memory-fread mem0 (vfield-id m cls-name fname) obj-addr))])

