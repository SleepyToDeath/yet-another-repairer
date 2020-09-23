#lang rosette/safe

(require (prefix-in std: racket/base))
(require "map.rkt")

(provide (all-defined-out))

;map X scope(next scope)
(struct scope (imap next) #:transparent)
(define scope-empty (scope imap-empty #f))

;scope(the top one)
(struct stack (top) #:transparent)
(define stack-empty (stack #f))

(define (stack-empty? st) 
	(stack-top st))

;read the first defined name in stack
(define (stack-read st name)
	;use the first definition in the highest possible scope
	(define (rec-read sc)
		(define cur (imap-get (scope-imap sc) name))
		(if (not (= cur not-found)) cur
			(if (not (scope-next sc)) nullptr
				(rec-read (scope-next sc)))))
	;return not-found if empty
	(if (not (stack-top st)) not-found
		(rec-read (stack-top st))))

;write to the first defined name in stack
(define (stack-write st name value)
	;use the first definition in the highest possible scope, return the updated scope
	(define (rec-write sc)
		(define cur (imap-get (scope-imap sc) name))
		(if (not (= cur not-found)) 
			(std:struct-copy scope sc [imap (imap-set (scope-imap sc) name value)])
			(if (not (scope-next sc)) sc
				(std:struct-copy scope sc [next (rec-write (scope-next sc))]))))
	;return input if empty
	(if (not (stack-top st)) st
		(stack (rec-write (stack-top st)))))
	
;declare at the current top level scope
;default value is nullptr
(define (stack-decl st name)
	(stack (std:struct-copy scope (stack-top st) [imap (imap-set (stack-top st) name nullptr)])))

;push a scope to the top
(define (stack-push st)
	(stack (std:struct-copy scope scope-empty [next (stack-top st)])))

;pop a scope from the top
(define (stack-pop st)
	(stack (scope-next (stack-top st))))

