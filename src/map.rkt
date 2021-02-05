#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/format)
(require "formula.rkt")
(require "memory-common.rkt")
(require "match-define.rkt")

;I'm just being lazy, but please avoid using anything in 'symbolic' section directly
(provide (all-defined-out))

;(define imap-current-selector #f)
;(define (imap-set-selector i)
;	(set! imap-current-selector i))

;Usage:
;	1.The maps should be used in an immutable manner
;	2.Concrete map: use `imap-emtpy` to get a new empty map
;	  and `imap-get`, `imap-set` to read & write
;	3.Symbolic map: use `imap-sym-scoped-new` to get a new symbolic map.
;	  Use `imap-sym-scoped-reset` to copy a base map(concrete/symbolic),
;	  then use it as if it's concrete(except that a commit is needed for
;	  updates to be seen by future reads). Finally, use `imap-sym-scoped-get-fml`
;	  to get a formula describing the relation between the updated map
;	  and the base map.
;	4.A concrete map can map anything to anything. But a symbolic
;	  map can only map int to int.
;	5.Use imap-sym-scoped-select to make a merging node from
;	  a list of sources, each with a condition.

;============= Definition & Operations ===========

;----------------- Generic --------------------
(define-generics imap
	[imap-get-func imap]
	[imap-get imap index]
	[imap-get2 imap index extra]
	[imap-set imap index value])

(define (imap-contains? m index)
	(not (is-not-found? (imap-get m index))))

(define (imap-batch-set imap kvlist)
	(foldl (lambda (kv m) (imap-set m (car kv) (cdr kv))) imap kvlist))

(define (imap-get-dispatch m index)
	(imap-get m index))

(define (imap-get-func-dispatch m)
	(imap-get-func m))

(define (imap-set-dispatch m index value)
	(imap-set m index value))

;----------------- Concrete --------------------
(struct imap-conc (func) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-conc-func m))

		(define (imap-get m index)
			(define f (imap-conc-func m))
			(f index))

		(define (imap-get2 m index extra)
			(imap-get m index))

		(define (imap-set m index value)
			(define oldf (imap-conc-func m))
			(define newf (lambda (args)
				(if (equal? args nullptr) nullptr
					(if (equal? args index) value (oldf args)))))
			(std:struct-copy imap-conc m [func newf]))
	])

;----------------- Symbolic ---------------------
(struct imap-sym (func-dummy func-base func-true func-base-true updates committed-updates fml-deferred) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-sym-func-dummy m))

		(define (imap-get m index)
			(force-error (equal? (imap-sym-func-dummy m) invalid-id) "reading from mem 0!")
			(define ret (imap-sym-real-get m index))
			ret)

		(define (imap-get2 m index extra)
			(imap-get m index))

		(define (imap-set m index value)
			(force-error (equal? (imap-sym-func-dummy m) invalid-id) "writing to mem 0!")
			(std:struct-copy imap-sym m [updates (cons (cons index value) (imap-sym-updates m))]))
	])

(define (imap-sym-real-get m index)
	(define pending	(ormap identity (map
		(lambda (kv) (if (equal? (car kv) index) (cdr kv) #f))
		(cons (cons nullptr nullptr) (imap-sym-committed-updates m)))))
	(define func-base-true (imap-sym-func-base-true m))
	(if (number? pending) pending (func-base-true index)))

(define (imap-sym-key-fml-debug m index)
	(defer-eval "equal?" (cons ((imap-sym-func-true m) index) (imap-sym-real-get m index))))

(define (imap-sym-key-not-found m index)
	(equal? ((imap-sym-func-true m) index) not-found))

(define (imap-sym-key-fml m index)
	(define ret (equal? ((imap-sym-func-true m) index) (imap-sym-real-get m index)))
	ret)

(define (imap-sym-reset m m-base)
	(define-symbolic* fml-deferred boolean?)
	(define func-base (imap-get-func m-base))
	(imap-sym (imap-get-func m) func-base (imap-sym-func-true m) 
		(if (imap-func-is-dummy? func-base) (imap-sym-func-true m-base) func-base)
		null null fml-deferred))

;make updates so far visible to new reads
(define (imap-sym-commit m)
	(std:struct-copy imap-sym m [updates null] [committed-updates (append (imap-sym-updates m) (imap-sym-committed-updates m))]))

;should only be called once for each section, otherwise only the last one will work
;(define (imap-sym-finish m)
;	(imap-add-sym-map (imap-sym-func-dummy m) m)
;	(imap-add-dummy (imap-sym-func-dummy m)))

(define (imap-sym-get-fml m)
	(force-error (equal? (imap-sym-func-dummy m) invalid-id) "getting fml from mem 0!")
	(imap-sym-fml-deferred m))

(define (imap-sym-new id)
	(define-symbolic* func-true (~> integer? integer?))
	(imap-sym id default-func func-true default-func null null #f))

;----------------- Symbolic Wrapper For Tracking Keys ---------------------
(struct imap-sym-tracked (imap keys) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(force-error #t "imap-get-func shouldn't be called on imap-sym-tracked"))

		(define (imap-get m index)
			(imap-add-section-key (cons index #f))
			(define ret
				(if (not (imap-sym? (imap-sym-tracked-imap m)))
					not-found
					(imap-get-dispatch (imap-sym-tracked-imap m) index)))
			(defer-eval "imap get" (cons index ret))
			ret)

		(define (imap-get2 m index extra)
			(imap-get m index))

		(define (imap-set m index value)
			(defer-eval "imap set" (cons index value))
			(if (not (imap-sym? (imap-sym-tracked-imap m)))
				m
				(std:struct-copy imap-sym-tracked m [imap (imap-set-dispatch (imap-sym-tracked-imap m) index value)])))
	])

	(define (imap-sym-tracked-reset m m-base)
		(set! imap-section-keys null)
		(if (imap-conc? m-base)
			(imap-sym-tracked (imap-sym-reset (imap-sym-tracked-imap m) m-base) null)
			(imap-sym-tracked (imap-sym-reset (imap-sym-tracked-imap m) (imap-sym-tracked-imap m-base)) (imap-sym-tracked-keys m-base))))

	(define (imap-sym-tracked-commit m)
		(define keys imap-section-keys)
		(define ms-commit (maybe-do imap-sym? #f (imap-sym-tracked-imap m) imap-sym-commit))
		(set! imap-section-keys null)

		(display (~a "Committed " (length keys) " keys\n"))
		(imap-sym-tracked
			ms-commit
			(append (imap-sym-tracked-keys m) keys)))

;	(define (imap-sym-tracked-get-fml m)
;		(imap-sym-finish (imap-sym-tracked-imap m))
;		(imap-sym-get-fml (imap-sym-tracked-imap m)))

	(define (imap-sym-tracked-new id)
		(imap-sym-tracked (imap-sym-new id) null))

	(define (imap-sym-tracked-select candidates summary?)

		(define f-select (maybe-select imap-sym-null (lambda (m) (equal? (imap-sym-func-dummy m) invalid-id))))

		(define candidates-unwrapped (map (lambda (cnd.m) (cons (car cnd.m) (imap-sym-tracked-imap (cdr cnd.m)))) candidates))

		(define m-new 
			(if (and 
					(equal? (length candidates) 1) 
					(is-concrete-value (caar candidates)) 
					(caar candidates))
				(imap-sym-tracked-imap (cdar candidates))
				(f-select candidates-unwrapped summary?)))

		(define k-new
			(foldl 
				(lambda (p.m keys)
					(append 
						keys
						(filter (lambda (key+id.1)
							(andmap (lambda (key+id.2) (not (equal? (cdr key+id.1) (cdr key+id.2)))) keys))
							(imap-sym-tracked-keys (cdr p.m)))))
				null
				candidates))

		(display (~a "Keys num : " (length k-new) "\n"))

		(imap-sym-tracked m-new k-new))




;				(begin	
;					(define maybe-m-new 
;						(ormap identity 
;							(map 
;								(lambda (p.m) 
;									(define maybe-imap (imap-sym-tracked-imap (cdr p.m)))
;									(if 
;										(and 
;											(car p.m) 
;											(imap-sym? maybe-imap)
;											(not (equal? (imap-sym-func-dummy maybe-imap) invalid-id)))
;										maybe-imap #f))
;								candidates)))
;					(define m-new (if (or summary? (imap-sym? maybe-m-new)) maybe-m-new imap-sym-null))
;					m-new)))

			;(apply append (map (lambda (p.m) (imap-sym-tracked-keys (cdr p.m))) candidates))))

;----------------- Symbolic Wrapper For Tracking Keys' Scopes ---------------------

(struct imap-sym-scoped (imap scope) #:transparent
	#:methods gen:imap
	[
		(define (imap-get-func m)
			(imap-get-func-dispatch (imap-sym-scoped-imap m)))

		(define (imap-get2 m index scope)
			(define ret (imap-get-dispatch (imap-sym-scoped-imap m) index))
			(imap-replace-first-key (cons index scope))
			ret)

		(define (imap-get m index)
			(imap-get-dispatch (imap-sym-scoped-imap m) index))

		(define (imap-set m index value)
			(std:struct-copy imap-sym-scoped m 
				[imap (imap-set-dispatch (imap-sym-scoped-imap m) index value)]))
	])


	(define (imap-sym-scoped-reset m m-base scope)
		(if (imap-conc? m-base)
			(imap-sym-scoped (imap-sym-tracked-reset (imap-sym-scoped-imap m) m-base) scope)
			(imap-sym-scoped (imap-sym-tracked-reset (imap-sym-scoped-imap m) (imap-sym-scoped-imap m-base)) scope)))

	(define (imap-sym-scoped-commit m)
		(std:struct-copy imap-sym-scoped m [imap (imap-sym-tracked-commit (imap-sym-scoped-imap m))]))

	(define (imap-sym-scoped-get-fml m)
		(define ms (imap-sym-tracked-imap (imap-sym-scoped-imap m)))
		(imap-sym-get-fml ms))

;	(define (imap-sym-scoped-get-fml-pure m)
;		(define ms (imap-sym-tracked-imap (imap-sym-scoped-imap m)))
;		(imap-sym-get-fml ms))

	(define (imap-sym-scoped-new id)
		(imap-sym-scoped (imap-sym-tracked-new id) #f))

	(define (imap-sym-scoped-select candidates merged-scope summary?)
		(imap-sym-scoped 
			(imap-sym-tracked-select (map (lambda (cnd.m) (cons (car cnd.m) (imap-sym-scoped-imap (cdr cnd.m)))) candidates) summary?)
			merged-scope))

	(define (imap-sym-scoped-update-scope m scope)
		(std:struct-copy imap-sym-scoped m [scope scope]))

;==================================================


;============= Default Values ===========
(define (default-func x) not-found)

(define imap-empty (imap-conc default-func))

(define imap-sym-null
	(imap-sym invalid-id invalid-id default-func default-func null null #t))

(define imap-sym-tracked-null
	(imap-sym-tracked imap-sym-null null))

(define imap-sym-scoped-null
	(imap-sym-scoped imap-sym-tracked-null #f))
;========================================

;================== Generate Deferred Formulae ====================
;To avoid `forall`, which is very slow to solve, we explicitly list
;the formula for all keys, which hopefully will be faster to solve......
;This must happen after all keys are accessed at the end of encoding.

(define imap-section-keys null)
(define (imap-add-section-key key)
	(set! imap-section-keys (cons key imap-section-keys)))
(define (imap-replace-first-key key)
	(set! imap-section-keys (cons key (cdr imap-section-keys))))

(define imap-func-is-dummy? number?)

;(define all-symbols null)
;(define (global-add-symbol sym)
;	(set! all-symbols (cons sym all-symbols)))
;================== Helpers ====================
(define (imap-sym-lookback m)
	(display (~a "map track: " (imap-sym-func-base (imap-unwrap m)) " ~> " (imap-sym-func-dummy (imap-unwrap m))
				" #updates: " (length (imap-sym-committed-updates (imap-unwrap m))) "\n")))

(define (imap-unwrap m)
	(imap-sym-tracked-imap (imap-sym-scoped-imap m)))
