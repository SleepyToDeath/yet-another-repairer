#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/format)
(require "match-define.rkt")

(provide (all-defined-out))

;(define imap-current-selector #f)
;(define (imap-set-selector i)
;	(set! imap-current-selector i))

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

(define (imap-get-dispatch m index)
	(imap-get m index))

;(define (imap-get-func-dispatch m)
;	(imap-get-func m))
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
(struct imap-sym (func-dummy func-base func-true updates fml-deferred) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-sym-func-dummy m))

		(define (imap-get m index)
			(imap-add-section-key index)
			(define pending	(ormap
				(lambda (kv) (if (equal? (car kv) index) (cdr kv) #f))
				(imap-sym-updates m)))
			(define func-base (imap-sym-func-base m))
			(if pending pending 
				(if (imap-func-is-dummy func-base)
					((imap-sym-func-true (imap-get-dispatch imap-dummy2map func-base)) index)
					(func-base index))))

		(define (imap-set m index value)
;			(pretty-print (~a "\n imap store: " index " : " value "\n"))
			(std:struct-copy imap-sym m [updates (cons (cons index value) (imap-sym-updates m))]))
	])

(define (imap-sym-key-fml m index)
	(equal? ((imap-sym-func-true m) index) (imap-get m index)))

(define (imap-sym-reset m m-base)
	(set! imap-section-keys null)
	(define-symbolic* fml-deferred boolean?)
	(imap-sym (imap-get-func m) (imap-get-func m-base) (imap-sym-func-true m) null fml-deferred))

;should only be called once for each section, otherwise only the last one will work
;imap-sym -> boolean(placeholder symbol for deferred fml)
(define (imap-sym-get-fml m)
	(imap-add-sym-map (imap-sym-func-dummy m) m)
	(imap-add-dummy (imap-sym-func-dummy m))
	(cons (imap-sym-fml-deferred m) imap-section-keys))

(define (imap-sym-new)
	(define-symbolic* func-true (~> integer? integer?))
	(define func-dummy (imap-new-dummy))
	(imap-sym func-dummy default-func func-true null #f))
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

;(define imap-deferred-fmls null)
;(define (imap-add-deferred-fml fml)
;	(pretty-print (length imap-deferred-fmls))
;	(set! imap-deferred-fmls (cons fml imap-deferred-fmls)))

(define imap-section-keys null)
(define (imap-add-section-key key)
	(set! imap-section-keys (cons key imap-section-keys)))

(define imap-dummy2map imap-empty)
(define (imap-add-sym-map func-dummy m)
	(set! imap-dummy2map (imap-set imap-dummy2map func-dummy m)))

(define imap-dummy-list null)
(define (imap-add-dummy func-dummy)
	(set! imap-dummy-list (cons func-dummy imap-dummy-list)))

(define imap-dummy-counter 0)
(define (imap-new-dummy)
	(set! imap-dummy-counter (+ 1 imap-dummy-counter))
	imap-dummy-counter)

(define imap-func-is-dummy number?)

