#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/format)
(require racket/list)
(require "formula.rkt")
(require "memory-common.rkt")
(require "match-define.rkt")

(provide imap-get imap-set imap-batch-set imap-is-conc?
		 imap-new imap-reset imap-commit imap-summary imap-select imap-gen-binding
		 imap-null imap-empty)

;Usage:
;	1.The maps should be used in an immutable manner
;	2.Concrete map: use `imap-emtpy` to get a new empty map
;	  and `imap-get`, `imap-set` to read & write
;	3.Symbolic map: use `(imap-new)` to get a new symbolic map.
;	  Use `imap-reset` to copy a base map(concrete/symbolic),
;	  then use it as if it's concrete(except that a commit is needed for
;	  updates to be seen by future reads). Finally, use `imap-summary`
;	  to get a formula describing the relation between the updated map
;	  and the base map.
;	4.A concrete map can map anything to anything. But a symbolic
;	  map can only map int to a specified type.
;	5.Use imap-select to make a phi node state from
;	  a list of input states, each with a condition.
;	6.The summary generations are delayed to the end of encoding 
;	  since it is not clear what keys should be preserved before that.
;	  Use `imap-preserve` to generate real preserve formulae and
;	  assert their conjunction is equivalent to the placeholder
;	  returned during the encoding. Do this for each state and
;	  its corresponding keys (currently, it's all keys......)
;	7.During symbolic execution (summary), all control flows in 
;	  `imap-sym-wrapper` must always be concrete. Branches are
;	  limited to within `imap-sym`.

;============= Definition & Operations ===========

;----------------- Generic --------------------
(define-generics imap
	[imap-get imap index type]
	[imap-set imap index value type]
	[imap-is-conc? imap]
	[imap-get-func imap])

(define-generics imap-isym
	[imap-get-id imap-isym]
	[imap-reset imap-isym base]
	[imap-commit imap-isym]
	[imap-summary imap-isym]
	[imap-preserve imap-isym index])
  

(define (imap-batch-set imap kvlist)
	(foldl (lambda (kv m) (imap-set m (car kv) (cdr kv))) imap kvlist))

;re-dispatch, what is the decent way to do this?
(define (imap-get+ m index)
	(imap-get m index))
(define (imap-set+ m index value)
	(imap-set m index value))
(define (imap-get-func+ m)
	(imap-get-func m))
(define (imap-reset+ m base)
	(imap-reset m base))
(define (imap-commit+ m)
	(imap-commit m))
(define (imap-summary+ m)
	(imap-summary m))
(define (imap-preserve+ m key)
	(imap-preserve m key))


;----------------- Concrete --------------------
(struct imap-conc (func) #:transparent
	#:methods gen:imap
	[
		(define (imap-get m index type)
			(define f (imap-conc-func m))
			(f index))

		(define (imap-set m index value type)
			(define oldf (imap-conc-func m))
			(define newf (lambda (args)
				(if (equal? args nullptr) nullptr
					(if (equal? args index) value (oldf args)))))
			(std:struct-copy imap-conc m [func newf]))

		(define (imap-get-func m)
			(imap-conc-func m))

		(define (imap-is-conc? m)
			#t)
	])

;----------------- Symbolic ---------------------
(struct imap-sym (id id-base func func-base updates updates-committed fml-deferred) #:transparent
	#:methods gen:imap
	[	;ignore types
		(define (imap-get m index type)
			(force-error (equal? (imap-sym-id m) invalid-id) "reading from mem 0!")
			(imap-sym-real-get m index))

		(define (imap-set m index value type)
			(force-error (equal? (imap-sym-id m) invalid-id) "writing to mem 0!")
			(std:struct-copy imap-sym m [updates (cons (cons index value) (imap-sym-updates m))]))

		(define (imap-get-func m)
			(imap-sym-func m))

		(define (imap-is-conc? m)
			#f)
	]

	#:methods gen:imap-isym
	[
		(define (imap-get-id m)
			(imap-sym-id m))

		(define (imap-reset m m-base)
			(define-symbolic* fml-deferred boolean?)
			(define func-base (imap-get-func m-base))
			(define id-base (imap-get-id m-base))
			(imap-sym (imap-get-id m) id-base (imap-get-func m) func-base
				null null fml-deferred))

		;make updates so far visible to new reads
		(define (imap-sym-commit m)
			(std:struct-copy imap-sym m [updates null] [updates-committed (append (imap-sym-updates m) (imap-sym-updates-committed m))]))

		(define (imap-summary m)
			(force-error (equal? (imap-sym-id m) invalid-id) "getting fml from mem 0!")
			(imap-sym-fml-deferred m))

		(define (imap-preserve m index)
			(define ret (equal? ((imap-sym-func m) index) (imap-sym-real-get m index)))
			ret)
	])

(define (imap-sym-real-get m index)
	(define pending	(ormap identity (map
		(lambda (kv) (if (equal? (car kv) index) (cdr kv) #f))
		(cons (cons nullptr nullptr) (imap-sym-updates-committed m)))))
	(define func-base (imap-sym-func-base m))
	(if (number? pending) pending (func-base index)))


;----------------- Symbolic Wrapper For Dispatching Types, Merging States and Tracking Keys ---------------------
(struct imap-sym-wrapper (imaps) #:transparent
	#:methods gen:imap
	[
		(define (imap-get m index type)
			(define ms (imap-get-typed m type))
			(imap-add-index (cons index type))
			(do-n-ret
				(lambda (ret) (defer-eval "imap get" (list index ret type)))
				(if (not (imap-sym? ms))
					not-found
					(imap-get+ ms index))))

		(define (imap-set m index value type)
			(defer-eval "imap set" (list index value type))
			(define ms (imap-get-typed m type))
			(if (not (imap-sym? ms)) m
				(imap-sym-wrapper (imap-set-typed m type (imap-set+ ms index value type)))))

		(define (imap-get-func m)
			(force-error #t "imap-get-func shouldn't be called on imap-sym-wrapper"))

		(define (imap-is-conc? m)
			#f)
	]

	#:methods gen:imap-isym
	[
		(define (imap-get-id m)
			(force-error #t "imap-get-id shouldn't be called on imap-sym-wrapper"))

		(define (imap-reset m m-base)
			(imap-sym-wrapper 
				(map (lambda (type)
					(if (imap-conc? m-base)
						(imap-reset+ (imap-get-typed m type) m-base)
						(imap-reset+ (imap-get-typed m type) (imap-get-typed m-base type)))) all-types-ordered)))

		(define (imap-commit m)
			(imap-sym-wrapper 
				(map (lambda (ms) (maybe-do imap-sym? #f ms imap-commit+)) 
					 (imap-sym-wrapper-imaps m))))

		(define (imap-summary m)
			(andmap+ imap-summary+ (imap-sym-wrapper-imaps m)))

		(define (imap-preserve m index)
			(force-error #t "imap-preserve shouldn't be called on imap-sym-wrapper"))
	])

	(define (imap-get-typed m type)
		(list-ref (imap-sym-wrapper-imaps m) (type->ordinal type)))
	(define (imap-set-typed m type ms)
		(list-set (imap-sym-wrapper-imaps m) (type->ordinal type) ms))

	(define (imap-select candidates summary?)
		(define f-select (maybe-select imap-sym-null (lambda (m) (equal? (imap-sym-id m) invalid-id))))
		(imap-sym-wrapper
			(map
				(lambda (type)
					(define (unwrap cnd.m)
						(cons (car cnd.m) 
							  (list-ref (imap-sym-wrapper-imaps (cdr cnd.m)) (type->ordinal type))))
					(define candidates-unwrapped (map unwrap candidates))
					(if (and (equal? (length candidates) 1) 
							 (is-concrete-value? (caar candidates)) 
							 (caar candidates)) ;only one input and the condition is constant #t
						(cdar candidates)
						(f-select candidates-unwrapped summary?)))
				all-types-ordered)))

	(define (imap-new id)
		(define (sym-gen type)
			(define-symbolic* func (~> integer? type))
			(imap-sym id invalid-id func default-func null null #f))
		(imap-sym-wrapper (map sym-gen all-types-ordered)))



	(define (imap-gen-binding)
		(define all-keys imap-all-indices)
		(pretty-print (~a "Totally " (length all-keys) " keys"))

		(andmap+ (lambda (type)
			(define (mem-get-typed id type)
				(imap-get-typed (memory-heap (vector-ref memory-id-map id)) type))
			(define (id2keys id)
				(map car (imap-sym-updates-committed (mem-get-typed id type))))
			(define (contain-key? id key)
				(ormap identity (map (lambda (key0) (equal? key key0)) (id2keys id))))
			(define (smart-preserve ms)
				(lambda (key)
					(if (is-concrete-value? key)
						(imap-preserve ms key)
						((lambda () 
							(define-symbolic* key-sym type)
							(and (equal? key-sym key)
								 (imap-preserve ms key-sym)))))))

			(define all-typed-keys (map cdr (filter (lambda (k.t) (equal? (cdr k.t) type)) all-keys)))

			(define fml-maybe-wrong
				(andmap+ (lambda (mem-id)
					(define ms (mem-get-typed mem-id type))
					(define fml-true (andmap+ (smart-preserve ms) (id2keys mem-id)))
					(define fml-deferred (imap-sym-fml-deferred ms))
					(equal? fml-deferred fml-true))
				memory-id-list))

			(define fml-always-right
				(andmap+ (lambda (mem-id)
					(define ms (mem-get-typed mem-id type))
					(andmap+ (lambda (key) 
						(if (contain-key? mem-id key) #t
							((smart-preserve ms) key)))
					all-typed-keys))
				memory-id-list))

			(and fml-maybe-wrong fml-always-right))
		all-types-ordered))

;============= Default Values ===========
(define (default-func x) not-found)

(define imap-empty (imap-conc default-func))

(define imap-sym-null
	(imap-sym invalid-id invalid-id default-func default-func null null #t))

(define imap-sym-wrapper-null
	(imap-sym-wrapper (map (lambda (x) imap-sym-null) all-types-ordered)))

(define (imap-null)
	imap-sym-wrapper-null)
;========================================

;================== Generate Deferred Formulae ====================
;To avoid `forall`, which is very slow to solve, we explicitly list
;the formula for all keys, which hopefully will be faster to solve......
;This must happen after all keys are accessed at the end of encoding.
(define imap-all-indices null)
(define (imap-clear-indices!)
	(set! imap-all-indices null))
(define (imap-add-index key)
	(set! imap-all-indices (cons key imap-all-indices)))

