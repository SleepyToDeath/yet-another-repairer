#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/format)

(provide (all-defined-out))

;(define global-fml #t)

;(define (add-global-fml fml)
;	(set! global-fml (and fml global-fml)))

;Usage:
;	1.The maps should be used in an immutable manner
;	2.Concrete map: use `imap-emtpy` to get a new empty map
;	  and `imap-get`, `imap-set` to read & write
;	3.Symbolic map: use `imap-sym-new` to get a new symbolic map.
;	  Use `imap-sym-reset` to copy a base map(concrete/symbolic),
;	  then use it as if it's concrete. Finally, use `imap-sym-get-fml`
;	  to get a formula describing the relation between the updated map
;	  and the base map.
;	4.A concrete map can map anything to anything. But a symbolic
;	  map can only map int to int.

;============= Definition & Operations ===========

;----------------- Generic --------------------
(define-generics imap
	[imap-get-func imap]
	[imap-get imap index]
	[imap-set imap index value])

(define (imap-contains? m index)
	(not (is-not-found? (imap-get m index))))

(define (is-not-found? v)
	(equal? v not-found))

(define (imap-batch-set imap kvlist)
	(foldl (lambda (kv m) (imap-set m (car kv) (cdr kv))) imap kvlist))

;[!]compare only func, ignore pending updates
(define (imap-is-update f-new f-base updates)
;	(define f-new (imap-get-func m-new))
;	(define f-base (imap-get-func m-base))
;	(display "\n f-new: ")
;	(print f-new)
;	(display "\n f-base: ")
;	(print f-base)
;	(display "\n")
	(define-symbolic* x integer?)
	(forall (list x) (foldr
		(lambda (kv fml) (if (equal? x (car kv)) (equal? (f-new x) (cdr kv)) fml)) 
		(equal? (f-new x) (f-base x))
		updates)))

;[!]compare only func, ignore pending updates
(define (imap-is-copy f-new f-base)
;	(define f-new (imap-get-func m-new))
;	(define f-base (imap-get-func m-base))
	(define-symbolic* x integer?)
	(forall (list x) (equal? (f-new x) (f-base x))))

(define (imap-get-dispatch m index)
	(imap-get m index))

(define (imap-get-func-dispatch m)
	(imap-get-func m))
;----------------- Concrete --------------------
(struct imap-conc (func) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-conc-func m))

		(define (imap-get m index)
			(define f (imap-conc-func m))
			(f index))

		(define (imap-set m index value)
			(define oldf (imap-conc-func m))
			(define newf (lambda (args)
				(if (equal? args nullptr) nullptr
					(if (equal? args index) value (oldf args)))))
			(std:struct-copy imap-conc m [func newf]))
	])

;----------------- Symbolic ---------------------
(struct imap-sym (func-sym func-base updates) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-sym-func-sym m))

		(define (imap-get m index)
;			((imap-sym-func-sym m) index))
			(define pending	(ormap
				(lambda (kv) (if (equal? (car kv) index) (cdr kv) #f))
				(imap-sym-updates m)))
;			(if pending pending (imap-get-dispatch (imap-sym-imap-base m) index)))
			(if pending pending ((imap-sym-func-base m) index)))

		(define (imap-set m index value)
;			(pretty-print (~a "\n imap store: " index " : " value "\n"))
			(std:struct-copy imap-sym m [updates (cons (cons index value) (imap-sym-updates m))]))
	])

(define (imap-sym-reset m m-base)
;	(define-symbolic* func-base (~> integer? integer?))
	;[!] Assert!
;	(assert (imap-is-copy func-base (imap-get-func m-base)))
	(imap-sym (imap-get-func m) (imap-get-func m-base) null))

(define (imap-sym-get-fml m)
	(imap-is-update (imap-get-func m) (imap-get-func m) (imap-sym-updates m)))

(define (imap-sym-new)
	(define-symbolic* func-sym (~> integer? integer?))
	(imap-sym func-sym imap-empty null))
;==================================================

;============= Default Values ===========
(define (default-func x) not-found)

(define imap-empty (imap-conc default-func))

(define nullptr -1)
(define not-found -666)
;========================================


