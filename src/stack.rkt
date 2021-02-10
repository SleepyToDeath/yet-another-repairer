#lang rosette/safe

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require racket/pretty)
(require "map.rkt")
(require "formula.rkt")
(require "memory-common.rkt")

(provide (all-defined-out))


;====================== Static Version ==========================
(struct static-scope (keys array array-out) #:transparent)
(struct static-stack (scopes) #:transparent)

(define (stack-static-read mem name)
	(define ret.maybe (ormap 
		(lambda (sc) (if (member name (static-scope-keys sc)) (list-ref (static-scope-array sc) name) #f)) 
		(static-stack-scopes (memory-stack mem))))
	(if ret.maybe ret.maybe not-found))

(define (stack-static-write mem name value)
	(define found? #f)
	(std:struct-copy memory mem [stack
		(static-stack 
			(map 
				(lambda (sc) (if found? sc 
					(if (or 
							(not (member name (static-scope-keys sc))) 
							(is-invalid? (list-ref (static-scope-array sc) name)))
						sc
						(begin 
						(set! found? #t) 
						(std:struct-copy static-scope sc [array (std:list-set (static-scope-array sc) name value)])))))
				(static-stack-scopes (memory-stack mem))))]))

(define (stack-static-force-read mem name lvl)
	(define ret (list-ref (static-scope-array (list-ref (static-stack-scopes (memory-stack mem)) lvl)) name))
	(defer-eval "stack read: " (list (length (static-stack-scopes (memory-stack mem))) lvl name ret))
	ret)

(define (stack-static-force-write mem name value lvl)
	(define mem-decl (if (zero? lvl) (stack-static-decl mem name) mem))
	(define scs (static-stack-scopes (memory-stack mem-decl)))
	(defer-eval "stack write: " (list (length scs) lvl name value (not (is-invalid? (list-ref (static-scope-array (list-ref scs lvl)) name)))))
	(if (is-invalid? (list-ref (static-scope-array (list-ref scs lvl)) name))
		mem
		(std:struct-copy memory mem-decl [stack
			(static-stack 
				(std:list-set scs lvl 
					(std:struct-copy static-scope (list-ref scs lvl) [array (std:list-set (static-scope-array (list-ref scs lvl)) name value)])))])))

(define (stack-static-decl mem name)
	(defer-eval "stack decl: " name)
	(define st (memory-stack mem))
	(define scs (static-stack-scopes st))
	(define sc (car scs))
	(define keys (static-scope-keys sc))
	(define array (static-scope-array sc))
	(define array-out (static-scope-array-out sc))
	(define array.new (if (member name keys) array (std:list-set array name nullptr)))
	(define array-out.new (if (member name keys) array-out (std:list-set array-out name 
		((lambda () (begin
			(define-symbolic* vs integer?) 
			(display (~a "stack decl: " name " "))
			(print-fml vs)
			vs))))))
	(define ret (if (is-invalid? (list-ref (static-scope-array (car scs)) name))
		mem
		(std:struct-copy memory mem 
			[stack 
				(static-stack 
					(cons 
						(std:struct-copy static-scope sc [array array.new] [array-out array-out.new] [keys (std:sort (std:remove-duplicates (cons name keys)) <)])
						(cdr scs)))])))
	ret)
	
(define (stack-static-push mem)
	(define scs (static-stack-scopes (memory-stack mem)))
	(std:struct-copy memory mem 
		[stack 
			(static-stack 
				(cons static-scope-empty scs))]))

(define (stack-static-pop mem)
	(define scs (static-stack-scopes (memory-stack mem)))
	(std:struct-copy memory mem 
		[stack 
			(static-stack 
				(cdr scs))]))

(define (stack-static-reset st st-base)
	(define scs-base (static-stack-scopes st-base))
;	(pretty-print scs-base)
	(define ret (static-stack
		(map (lambda (sc-base) 
			(define keys (static-scope-keys sc-base))
			(define array-empty (static-scope-array static-scope-empty))
			(define array-base (static-scope-array-out sc-base))
			(define array.new (foldl (lambda (key arr)
					(std:list-set arr key (list-ref array-base key)))
				array-empty
				keys))
			(define array-out.new (foldl (lambda (key arr)
					(define-symbolic* vs integer?)
					(std:list-set arr key vs))
				array-empty
				keys))
			(static-scope keys array.new array-out.new))
			scs-base)))
;	(display "Reset stack:\n")
;	(pretty-print ret)
	ret)

;candidates: list of (cnd . stack)
(define (stack-static-select candidates summary?)
	(define template (cdar candidates))
	;keys are static, should be consistant even in invalid states
	(define stack-invalid (static-stack 
		(map (lambda (sc) (std:struct-copy static-scope static-scope-invalid [keys (static-scope-keys sc)])) (static-stack-scopes template))))
	(define f-select (maybe-select stack-invalid (lambda (x) #f)))
	(f-select candidates #f))

;do nothing; generate real content at reset
(define (stack-static-new)
	static-stack-empty)

(define (stack-static-get-fml st)
;	(display "Finish stack:\n")
;	(pretty-print st)
	(andmap+ (lambda (sc)
		(andmap+ (lambda (key)
			(define ret (equal?
				(list-ref (static-scope-array sc) key)
				(list-ref (static-scope-array-out sc) key)))
			(defer-eval "stack-fml: " (list ret 
				key
				(list-ref (static-scope-array sc) key)
				(list-ref (static-scope-array-out sc) key)))
			(inspect ret)
;			(pretty-print eval-pending)
;			(print-fml ret)
			ret)
			(static-scope-keys sc)))
		(static-stack-scopes st)))

(define static-scope-invalid
	(static-scope
		null
		(std:build-list scope-size (lambda (x) invalid-state))
		(std:build-list scope-size (lambda (x) invalid-state))))

(define static-scope-empty 
	(static-scope
		null
		(std:build-list scope-size (lambda (x) not-found))
		(std:build-list scope-size (lambda (x) not-found))))

(define static-stack-empty
	(static-stack null))






;====================== Dynamic Version ==========================

#|

(define (stack-dynamic-read mem name)
	;return not-found if empty
	(if (stack-empty? mem) not-found
		(begin
		(define bases (stack-meta-bases (memory-s-meta mem)))
		(define top0 (car bases))
		(define top1 (if (null? (cdr bases)) top0 (cadr bases)))
		(define butt0 (car (reverse bases)))
		(define butt1 (if (null? (cdr bases)) butt0 (cadr (reverse bases))))
		(define maybe-ret
			(map 
				(lambda (scope-base)
					(define cur-addr (+ scope-base name))
					(define cur-val (imap-get2 (memory-addr-space mem) cur-addr scope-base))
					(if (is-not-found? cur-val) #f cur-val))
				(list top0 top1 butt1 butt0)))
		(if (first maybe-ret) (first maybe-ret) 
			(if (second maybe-ret) (second maybe-ret)
				(if (third maybe-ret) (third maybe-ret)
					(if (fourth maybe-ret) (fourth maybe-ret)
						not-found)))))))

(define (stack-dynamic-write mem name value)
	;return input if empty
	(if (stack-empty? mem) mem
		(begin
		(define bases (stack-meta-bases (memory-s-meta mem)))
		(define top0 (car bases))
		(define top1 (if (null? (cdr bases)) top0 (cadr bases)))
		(define butt0 (car (reverse bases)))
		(define butt1 (if (null? (cdr bases)) butt0 (cadr (reverse bases))))
		(define maybe-updated 
			(map 
				(lambda (scope-base)
					(define cur-addr (+ scope-base name))
					(define cur-val (imap-get2 (memory-addr-space mem) cur-addr scope-base))
					(if (is-not-found? cur-val) #f cur-addr))
				(list top0 top1 butt1 butt0)))
		(define maybe-addr 
			(if (first maybe-updated) (first maybe-updated) 
				(if (second maybe-updated) (second maybe-updated)
					(if (third maybe-updated) (third maybe-updated)
						(if (fourth maybe-updated) (fourth maybe-updated)
							nullptr)))))
		(std:struct-copy memory mem [addr-space (imap-set (memory-addr-space mem) maybe-addr value)]))))

(define (stack-dynamic-force-read mem name lvl)
	(if (stack-empty? mem) not-found
		(begin
		(define bases (stack-meta-bases (memory-s-meta mem)))
		(define top0 (list-ref bases lvl))
		(define addr (+ top0 name))
		(imap-get2 (memory-addr-space mem) addr top0))))

(define (stack-dynamic-force-write mem name value lvl)
	(if (stack-empty? mem) mem
		(begin
		(define bases (stack-meta-bases (memory-s-meta mem)))
		(define top0 (list-ref bases lvl))
		(define addr (+ top0 name))
		(std:struct-copy memory mem [addr-space (imap-set (memory-addr-space mem) addr value)]))))
	
(define (stack-dynamic-decl mem name)
	(define bases (stack-meta-bases (memory-s-meta mem)))
	(define top0 (car bases))
	(define addr (+ top0 name))
	(std:struct-copy memory mem [addr-space (imap-set (memory-addr-space mem) addr nullptr)]))

(define (stack-dynamic-push mem)
	(define top (stack-meta-top (memory-s-meta mem)))
	(define bases (stack-meta-bases (memory-s-meta mem)))
	(define top+ (+ top scope-size))
	(define bases+ (cons top+ bases))
	(define imap (memory-addr-space mem))
	(if (imap-conc? imap)
		(std:struct-copy memory mem 
			[s-meta (stack-meta bases+ top+)])
		(std:struct-copy memory mem 
			[s-meta (stack-meta bases+ top+)]
			[addr-space (imap-sym-scoped-update-scope imap (cons top+ (imap-sym-scoped-scope imap)))])))

(define (stack-dynamic-pop mem)
	(define top (stack-meta-top (memory-s-meta mem)))
	(define bases (stack-meta-bases (memory-s-meta mem)))
	(define bases+ (cdr bases))
	(std:struct-copy memory mem [s-meta (stack-meta bases+ top)]))

;do nothing
(define (stack-dynamic-reset st st-base)
	st-base)

;do nothing
(define (stack-dynamic-select candidates summary?)
	#f)

;do nothing
(define (stack-dynamic-new)
	#f)

;do nothing
(define (stack-dynamic-get-fml st)
	#t)

;================================================================

|#



;========================== Interface ===========================

;(define (stack-empty? mem)
;	(empty? (stack-meta-bases (memory-s-meta mem))))

;read the first defined name in stack
;m: imap
(define stack-read stack-static-read)

;write to the first defined name in stack
;m: imap
(define stack-write stack-static-write)

(define stack-force-read stack-static-force-read)

;declare at the current top level scope
;default value is nullptr
;mem shouldn't be empty
(define stack-decl stack-static-decl)

;push a scope to the top
(define stack-push stack-static-push)

;pop a scope from the top
(define stack-pop stack-static-pop)

(define stack-force-write stack-static-force-write)

(define stack-new stack-static-new)

(define stack-reset stack-static-reset)

(define stack-select stack-static-select)

(define stack-get-fml stack-static-get-fml)

