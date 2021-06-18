#lang rosette/safe

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require racket/pretty)
(require racket/list)
(require "map.rkt")
(require "formula.rkt")
(require "memory-common.rkt")

(provide stack-read stack-write
stack-force-read stack-force-write 
stack-push stack-pop stack-decl
stack-new stack-reset stack-select stack-summary stack-gen-binding
stack-empty)


;====================== Static Version ==========================
;key: (cons name type)
(struct static-scope (keys array array-out) #:transparent)

;scopes: list of static-scope
;updates: list of list of keys, each list of keys correspond to a scope
(struct static-stack (scopes updates) #:transparent)

;banned
(define (stack-static-read mem name type)
	(define ret.maybe (ormap 
		(lambda (sc) (if (member name (map car (static-scope-keys sc))) 
						 (list-ref (static-scope-array sc) name) 
						 #f)) 
		(static-stack-scopes (memory-stack mem))))
	(if ret.maybe ret.maybe (not-found type)))

;banned
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
	(list-ref (static-scope-array (list-ref (static-stack-scopes (memory-stack mem)) lvl)) name))

(define (stack-static-force-write mem name value lvl type)
	(define mem-decl (if (zero? lvl) (stack-static-decl mem name type) mem))
	(define scs (static-stack-scopes (memory-stack mem-decl)))
	(define updates (static-stack-updates (memory-stack mem-decl)))
	(std:struct-copy memory mem-decl [stack
		(static-stack 
			(if (is-invalid? (list-ref (static-scope-array (list-ref scs lvl)) name))
				scs
				(std:list-set scs lvl 
					(std:struct-copy static-scope (list-ref scs lvl) [array (std:list-set (static-scope-array (list-ref scs lvl)) name value)])))
			(std:list-set updates lvl
				(cons name (list-ref updates lvl))))]))

(define (stack-static-decl mem name type)
	(define st (memory-stack mem))
	(define scs (static-stack-scopes st))
	(define updates (static-stack-updates st))
	(define sc (car scs))
	(define keys (static-scope-keys sc))
	(define array (static-scope-array sc))
	(define array-out (static-scope-array-out sc))
	(define array.new (if (member (cons name type) keys) array (std:list-set array name (nullptr type))))
	(define array-out.new (if (member (cons name type) keys) array-out (std:list-set array-out name 
		((lambda () (define-symbolic* vs type) vs)))))
	(define ret (if (is-invalid? (list-ref (static-scope-array (car scs)) name))
		mem
		(std:struct-copy memory mem 
			[stack 
				(static-stack 
					(cons 
						(std:struct-copy static-scope sc 
							[array array.new] 
							[array-out array-out.new] 
							[keys (std:sort (remove-duplicates (cons (cons name type) keys)) (lambda (x y) (< (car x) (car y))))])
						(cdr scs))
					updates)])))
	ret)
	
(define (stack-static-push mem)
	(define scs (static-stack-scopes (memory-stack mem)))
	(define updates (static-stack-updates (memory-stack mem)))
	(std:struct-copy memory mem 
		[stack 
			(static-stack 
				(cons (static-scope-empty null) scs)
				(cons null updates))]))

(define (stack-static-pop mem)
	(define scs (static-stack-scopes (memory-stack mem)))
	(define updates (static-stack-updates (memory-stack mem)))
	(std:struct-copy memory mem 
		[stack 
			(static-stack 
				(cdr scs)
				(cdr updates))]))

(define (stack-static-reset st st-base)
	(define scs-base (static-stack-scopes st-base))
	(define updates-base (static-stack-updates st-base))
	(static-stack
		(map (lambda (sc-base) 
			(define keys (static-scope-keys sc-base))
			(define array-empty (static-scope-array (static-scope-empty keys)))
			(define array-base (static-scope-array-out sc-base))
			(define array.new (foldl (lambda (key arr)
					(std:list-set arr (car key) (list-ref array-base (car key))))
				array-empty
				keys))
			(define array-out.new (foldl (lambda (key arr)
					(define-symbolic* vs (cdr key))
					(std:list-set arr (car key) vs))
				array-empty
				keys))
			(static-scope keys array.new array-out.new))
			scs-base)
		(map (lambda (x) null) updates-base)))

;candidates: list of (cnd . stack)
(define (stack-static-select candidates summary?)
	(define template (cdar candidates))
	;keys are static, should be consistant even in invalid states
	(define scs-invalid
		(map (lambda (sc) 
			(std:struct-copy static-scope 
				(static-scope-invalid (static-scope-keys sc)) 
				[keys (static-scope-keys sc)])) 
			(static-stack-scopes template)))
	(define f-select (maybe-select scs-invalid (lambda (scs) (is-invalid? (last (static-scope-array (car scs)))))))
	(define scs-new (f-select (map (lambda (cnd.st) (cons (car cnd.st) (static-stack-scopes (cdr cnd.st)))) candidates) #f))
	(define all-updates (map static-stack-updates (map cdr candidates)))
	(define empty-updates (map (lambda (x) null) (car all-updates)))
	(define union-updates (foldl (lambda (updates union) (map append updates union)) empty-updates all-updates))
	(define updates-new (map remove-duplicates union-updates))
	(static-stack scs-new updates-new))

;do nothing; generate real content at reset
(define (stack-static-new)
	static-stack-empty)

(define (stack-static-summary st)
	(andmap+ (lambda (sc updates)
		(pending-always-right! (andmap+
			(lambda (key)
				(if (member (car key) updates) #t
					(equal?
						(list-ref (static-scope-array sc) (car key))
						(list-ref (static-scope-array-out sc) (car key)))))
			(static-scope-keys sc)))
		(andmap+ (lambda (key)
			(equal?
				(list-ref (static-scope-array sc) key)
				(list-ref (static-scope-array-out sc) key)))
			updates))
		(static-stack-scopes st) (static-stack-updates st)))

(define (stack-static-gen-binding)
	(andmap+ identity always-right-fmls))

(define always-right-fmls null)
(define (clear-always-right!)
	(set! always-right-fmls null))
(define (pending-always-right! fml)
	(set! always-right-fmls (cons fml always-right-fmls)))
(register-reset! clear-always-right!)

(define (arr-gen keys value-gen)
	(foldl (lambda (key arr) 
		(list-set arr (car key) (value-gen (cdr key))))
		(std:build-list scope-size (lambda (x) (value-gen default-type)))
		keys))

(define (static-scope-invalid keys)
	(define invalid-array (arr-gen keys invalid-state))
	(static-scope null invalid-array invalid-array))

(define (static-scope-empty keys)
	(define empty-array (arr-gen keys not-found))
	(static-scope null empty-array empty-array))

(define static-stack-empty
	(static-stack null null))



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

(define stack-summary stack-static-summary)

(define stack-empty static-stack-empty)

(define stack-gen-binding stack-static-gen-binding)
