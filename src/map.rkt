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
	[imap-get imap index]
	[imap-set imap index value]
	[imap-get-func imap])

(define-generics imap-isym
	[imap-get-id imap-isym]
	[imap-reset imap-isym base]
	[imap-commit imap-isym]
	[imap-summary imap-isym]
	[imap-preserve imap-isym index])
  

(define (imap-contains? m index)
	(not (is-not-found? (imap-get m index))))

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
		(define (imap-get m index)
			(define f (imap-conc-func m))
			(f index))

		(define (imap-set m index value)
			(define oldf (imap-conc-func m))
			(define newf (lambda (args)
				(if (equal? args nullptr) nullptr
					(if (equal? args index) value (oldf args)))))
			(std:struct-copy imap-conc m [func newf]))

		(define (imap-get-func m)
			(imap-conc-func m))
	])

;----------------- Symbolic ---------------------
(struct imap-sym (id id-base func func-base updates updates-committed fml-deferred) #:transparent
	#:methods gen:imap
	[
		(define (imap-get m index)
			(force-error (equal? (imap-sym-id m) invalid-id) "reading from mem 0!")
			(imap-sym-real-get m index))

		(define (imap-set m index value)
			(force-error (equal? (imap-sym-id m) invalid-id) "writing to mem 0!")
			(std:struct-copy imap-sym m [updates (cons (cons index value) (imap-sym-updates m))]))

		(define (imap-get-func m)
			(imap-sym-func m))
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

#|
(define (imap-sym-key-fml-debug m index)
	(defer-eval "equal?" 
		(list 
			index
			((imap-sym-func-true m) index) 
			(imap-sym-real-get m index) 
			(equal? ((imap-sym-func-true m) index) (imap-sym-real-get m index)))))

(define (imap-sym-key-not-found m index)
	(equal? ((imap-sym-func-true m) index) not-found))
|#


;----------------- Symbolic Wrapper For Merging and Tracking Keys ---------------------
(struct imap-sym-wrapper (imap) #:transparent
	#:methods gen:imap
	[
		(define (imap-get m index)
			(imap-add-get-key (cons index #f))
			(define ret
				(if (not (imap-sym? (imap-sym-wrapper-imap m)))
					not-found
					(imap-get+ (imap-sym-wrapper-imap m) index)))
			(defer-eval "imap get" (cons index ret))
			ret)

		(define (imap-set m index value)
			(defer-eval "imap set" (cons index value))
			(if (not (imap-sym? (imap-sym-wrapper-imap m)))
				m
				(std:struct-copy imap-sym-wrapper m [imap (imap-set+ (imap-sym-wrapper-imap m) index value)])))

		(define (imap-get-func m)
			(force-error #t "imap-get-func shouldn't be called on imap-sym-wrapper"))
	]

	#:methods gen:imap-isym
	[
		(define (imap-get-id m)
			(force-error #t "imap-get-id shouldn't be called on imap-sym-wrapper"))

		(define (imap-reset m m-base)
			(if (imap-conc? m-base)
				(imap-sym-wrapper (imap-reset+ (imap-sym-wrapper-imap m) m-base))
				(imap-sym-wrapper (imap-reset+ (imap-sym-wrapper-imap m) (imap-sym-wrapper-imap m-base)))))

		(define (imap-commit m)
			(imap-sym-wrapper (maybe-do imap-sym? #f (imap-sym-wrapper-imap m) imap-commit+)))

		(define (imap-summary m)
			(imap-summary+ (imap-sym-wrapper-imap m)))

		(define (imap-preserve m index)
			(imap-preserve+ (imap-sym-wrapper-imap m) index))
	])


	(define (imap-select candidates summary?)
		(define f-select (maybe-select imap-sym-null (lambda (m) (equal? (imap-sym-id m) invalid-id))))
		(define candidates-unwrapped (map (lambda (cnd.m) (cons (car cnd.m) (imap-sym-wrapper-imap (cdr cnd.m)))) candidates))
		(define m-new 
			(if (and 
					(equal? (length candidates) 1) 
					(is-concrete-value? (caar candidates)) 
					(caar candidates))
				(imap-sym-wrapper-imap (cdar candidates))
				(f-select candidates-unwrapped summary?)))
		(imap-sym-wrapper m-new))

	(define (imap-new id type)
		(define-symbolic* func-true (~> integer? type))
		(imap-sym-wrapper (imap-sym id invalid-id func-true default-func null null #f)))


;============= Default Values ===========
(define (default-func x) not-found)

(define imap-empty (imap-conc default-func))

(define imap-sym-null
	(imap-sym invalid-id invalid-id default-func default-func null null #t))

(define imap-sym-wrapper-null
	(imap-sym-wrapper imap-sym-null))
;========================================

;================== Generate Deferred Formulae ====================
;To avoid `forall`, which is very slow to solve, we explicitly list
;the formula for all keys, which hopefully will be faster to solve......
;This must happen after all keys are accessed at the end of encoding.
(define imap-all-get-keys null)
(define (imap-clear-get-keys!)
	(set! imap-all-get-keys null))
(define (imap-add-get-key key)
	(set! imap-all-get-keys (cons key imap-all-get-keys)))

;================== Helpers ====================
;(define (imap-sym-lookback m)
;	(display (~a "map track: " (imap-sym-func-base (imap-unwrap m)) " ~> " (imap-sym-id (imap-unwrap m))
;				" #updates: " (length (imap-sym-committed-updates (imap-unwrap m))) "\n")))
