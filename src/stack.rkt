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
stack-new stack-reset stack-select stack-summary
stack-empty)


;====================== Static Version ==========================
;key: (cons name type)
(struct static-scope (keys array array-out) #:transparent)
(struct static-stack (scopes) #:transparent)

(define (stack-static-read mem name type)
	(define ret.maybe (ormap 
		(lambda (sc) (if (member name (map car (static-scope-keys sc))) 
						 (list-ref (static-scope-array sc) name) 
						 #f)) 
		(static-stack-scopes (memory-stack mem))))
	(if ret.maybe ret.maybe (not-found type)))

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
	(if (is-invalid? (list-ref (static-scope-array (list-ref scs lvl)) name))
		mem
		(std:struct-copy memory mem-decl [stack
			(static-stack 
				(std:list-set scs lvl 
					(std:struct-copy static-scope (list-ref scs lvl) [array (std:list-set (static-scope-array (list-ref scs lvl)) name value)])))])))

(define (stack-static-decl mem name type)
;	(pretty-print (list mem name type))
	(define st (memory-stack mem))
	(define scs (static-stack-scopes st))
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
							[keys (std:sort (std:remove-duplicates (cons (cons name type) keys)) (lambda (x y) (< (car x) (car y))))])
						(cdr scs)))])))
	ret)
	
(define (stack-static-push mem)
	(define scs (static-stack-scopes (memory-stack mem)))
	(std:struct-copy memory mem 
		[stack 
			(static-stack 
				(cons (static-scope-empty null) scs))]))

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
			scs-base)))
;	(display "Reset stack:\n")
;	(pretty-print ret)
	ret)

;candidates: list of (cnd . stack)
(define (stack-static-select candidates summary?)
	(define template (cdar candidates))
	;keys are static, should be consistant even in invalid states
	(define stack-invalid (static-stack 
		(map (lambda (sc) 
			(std:struct-copy static-scope 
				(static-scope-invalid (static-scope-keys sc)) 
				[keys (static-scope-keys sc)])) 
			(static-stack-scopes template))))
	(define f-select (maybe-select stack-invalid (lambda (st) (is-invalid? (last (static-scope-array (car (static-stack-scopes st))))))))
	(f-select candidates #f))

;do nothing; generate real content at reset
(define (stack-static-new)
	static-stack-empty)

(define (stack-static-summary st)
;	(display "Finish stack:\n")
;	(pretty-print st)
	(andmap+ (lambda (sc)
		(andmap+ (lambda (key)
			(define ret (equal?
				(list-ref (static-scope-array sc) (car key))
				(list-ref (static-scope-array-out sc) (car key))))
;			(defer-eval "stack-fml: " (list ret 
;				key
;				(list-ref (static-scope-array sc) (car key))
;				(list-ref (static-scope-array-out sc) (car key))))
;			(inspect ret)
;			(pretty-print eval-pending)
;			(print-fml ret)
			ret)
			(static-scope-keys sc)))
		(static-stack-scopes st)))

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
	(static-stack null))



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

