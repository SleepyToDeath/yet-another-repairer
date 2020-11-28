#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/format)
(require "match-define.rkt")

(provide (all-defined-out))

(define imap-current-selector #t)
(define (imap-set-selector i)
	(set! imap-current-selector i))

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
;(define (imap-is-update f-new f-base updates)
;	(define-symbolic* x integer?)
;	(forall (list x) (foldr
;		(lambda (kv fml) (if (equal? x (car kv)) (equal? (f-new x) (cdr kv)) fml)) 
;		(equal? (f-new x) (f-base x))
;		updates)))

;[!]compare only func, ignore pending updates
;(define (imap-is-copy f-new f-base)
;	(define f-new (imap-get-func m-new))
;	(define f-base (imap-get-func m-base))
;	(define-symbolic* x integer?)
;	(forall (list x) (equal? (f-new x) (f-base x))))

;(define (imap-get-dispatch m index)
;	(imap-get m index))

;(define (imap-get-func-dispatch m)
;	(imap-get-func m))
;----------------- Concrete --------------------
(struct imap-conc (func) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-conc-func m))

		(define (imap-get m index)
;			(imap-add-key index)
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
(struct imap-sym (func-dummy func-base updates) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-sym-func-dummy m))

		(define (imap-get m index)
			(define-symbolic* v-dummy integer?)
;			(imap-add-key index)
;			(imap-add-fml (imap-sym-func-dummy m) (equal? v-dummy (imap-sym-real-get m index)))
			(assert (implies imap-current-selector (equal? v-dummy (imap-sym-real-get m index))))
			v-dummy)

		(define (imap-set m index value)
;			(pretty-print (~a "\n imap store: " index " : " value "\n"))
			(std:struct-copy imap-sym m [updates (cons (cons index value) (imap-sym-updates m))]))
	])

(define (imap-sym-real-get m index)
	(define pending	(ormap
		(lambda (kv) (if (equal? (car kv) index) (cdr kv) #f))
		(imap-sym-updates m)))
	(define func-base (imap-sym-func-base m))
;	(pretty-print func-base)
	(if pending pending 
		(if (imap-func-is-dummy func-base)
			(imap-get (imap-get imap-dummy2map func-base) index)
			(func-base index))))

(define (imap-sym-reset m m-base)
	(imap-sym (imap-get-func m) (imap-get-func m-base) null))

;should only be called once, otherwise only the last one will work
(define (imap-sym-get-fml m)
	(define-symbolic* fml-deferred boolean?)
	(imap-add-sym-map (imap-sym-func-dummy m) m)
	(imap-add-deferred-fml (imap-sym-func-dummy m) fml-deferred)
	fml-deferred)

(define (imap-sym-new)
	(define func-dummy (imap-new-dummy))
	(imap-add-dummy func-dummy)
	(imap-sym func-dummy imap-empty null))
;==================================================

;============= Default Values ===========
(define (default-func x) not-found)

(define imap-empty (imap-conc default-func))

(define nullptr -1)
(define not-found -666)
;========================================

;================== Generate Deferred Formulae ====================
;To avoid `forall`, which is very slow to solve, we explicitly list
;the formula for all keys, which hopefully will be faster to solve......
;This must happen after all keys are accessed at the end of encoding.
(define imap-dummy2map imap-empty)
(define imap-dummy2deferred imap-empty)
(define imap-dummy2fml imap-empty)
(define imap-dummy-list null)
(define imap-dummy-counter 0)

(define (imap-add-sym-map func-dummy m)
	(set! imap-dummy2map (imap-set imap-dummy2map func-dummy m)))

(define (imap-add-deferred-fml func-dummy fml)
	(set! imap-dummy2deferred (imap-set imap-dummy2deferred func-dummy fml)))

(define (imap-add-dummy func-dummy)
	(set! imap-dummy2fml (imap-set imap-dummy2fml func-dummy #t))
	(set! imap-dummy-list (cons func-dummy imap-dummy-list)))

(define (imap-new-dummy)
	(set! imap-dummy-counter (+ 1 imap-dummy-counter))
	imap-dummy-counter)

(define imap-func-is-dummy number?)

(define (imap-gen-deferred)
	(andmap 
		(lambda (func-dummy) 
			(define m (imap-get imap-dummy2map func-dummy))
			(define fml-deferred (imap-get imap-dummy2deferred func-dummy))
			(define fml-true (imap-get imap-dummy2fml func-dummy))
			(equal? fml-true fml-deferred))
		imap-dummy-list))
	
