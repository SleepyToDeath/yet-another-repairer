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
(require racket/format)
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`
(require racket/pretty)

(provide (all-defined-out))

;======================== Definitions ===========================
;mem: memory
;pc: int
;boot: boot function
;classes: list of classes
;cmap: name to class
;fmap: function sid to function (function pointer in memory is sid)
(struct machine (boot classes cmap fmap mem pc) #:transparent)

;
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
(define var-void-ret (string-id "__NONE__"))
(define var-ret-name (string-id "__return__"))
(define var-this-name (string-id "@this"))
(define func-name-main (string-id "main"))
(define func-name-boot (string-id "__boot__"))
(define func-name-init (string-id "<init>"))
(define func-name-clinit (string-id "<clinit>"))
(define delimiter-static (string-id "::"))
(define delimiter-virtual (string-id "::::"))
(define delimiter-minor (string-id ","))
(define field-name-class (string-id "__CLASS__"))
(define class-name-root (string-id "java.lang.Object"))

(define param-prefix "@parameter")
(define parameter-counter 0)
(define (reset-parameter-names)
	(set! parameter-counter 0))
(define (next-parameter-name)
	(define name (std:string-append param-prefix (std:number->string parameter-counter)))
	(set! parameter-counter (+ 1 parameter-counter))
	(string-id name))

(define machine-empty (machine #f null imap-empty imap-empty memory-empty pc-init))

;dynamically set, used to provide machine-level context, avoid using it too much
(define current-context machine-empty)
(define (set-context! mac)
	(set! current-context mac))
	
(define var-void-receiver-name (string-id "__no_receiver"))
(define addr-void-receiver nullptr)
(define (set-void-receiver-addr addr)
	(set! addr-void-receiver addr))
	

;============================= Utils ===================================
(define (lookup-virtual-function mac cls func arg-types) 
	(if cls
		(begin
			(display (~a "class name: " cls " func name: " func "\n"))
			(define cls-0 (imap-get (machine-cmap mac) cls))

			(define base-name (ormap 
				(lambda (cls-cur) (lookup-virtual-function mac cls-cur func arg-types)) 
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
			(display (~a "class name: " cls "\n"))
			(define cls-0 (imap-get (machine-cmap mac) cls))
			(display (~a "class vfields: " (class-vfields cls-0) " class name: " cls " field name: " field "\n"))

			(define base-name (ormap 
				(lambda (cls-cur) (lookup-virtual-field mac cls-cur field)) 
				(cons (class-extend cls-0) (class-implements cls-0))))

			(if base-name base-name
				(if 
					(ormap (lambda (f) (equal? f field)) (class-vfields cls-0)) 
					(list cls delimiter-virtual field)
					#f)))
		#f))

;virtual functions sharing same signature will have same vid
(define (vfunc-id mac cls func arg-types) (string-id (do-n-ret pretty-print (lookup-virtual-function mac cls func arg-types))))

(define (vfield-id mac cls field) (string-id (lookup-virtual-field mac cls field)))

(define (sfunc-id cls func arg-types) (string-id (sfunc-sig->string cls func arg-types)))
(define (sfunc-id-pure cls func arg-types) (string-id-pure (sfunc-sig->string cls func arg-types)))

(define (sfield-id cls field) (string-id (list cls delimiter-static field)))

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
	(append 
		(list cls delimiter-virtual func)
		(foldl (lambda (s l) (cons delimiter-minor (cons s l))) null arg-types)))

(define (sfunc-sig->string cls func arg-types)
	(append 
		(list cls delimiter-static func)
		(foldl (lambda (s l) (cons delimiter-minor (cons s l))) null arg-types)))

