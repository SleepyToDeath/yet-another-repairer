#lang rosette/safe

(require (prefix-in std: racket/base))
(require "map.rkt")
(require "memory-common.rkt")

(provide (all-defined-out))

(define (stack-empty m)
	(equal? (imap-get m stack-top) stack-bottom))

;read the first defined name in stack
;m: imap
(define (stack-read m name)
	;use the first definition in the highest possible scope
	(define (rec-read scope-base)
		(define cur-addr (scope-abs-addr scope-base name))
		(define cur-val (imap-get m cur-addr))
		(if (not (is-not-found? cur-val)) cur-val
			(if (equal? scope-base stack-bottom) not-found
				(rec-read (imap-get m scope-base)))))
	;return not-found if empty
	(if (stack-empty m) not-found
		(rec-read (imap-get m stack-pointer-addr))))

;write to the first defined name in stack
;m: imap
(define (stack-write m name value)
	;use the first definition in the highest possible scope, return the updated scope
	(define (rec-write scope-base)
		(define cur-addr (scope-abs-addr scope-base name))
		(define cur-val (imap-get m cur-addr))
		(if (not (is-not-found? cur-val)) 
			(imap-set m cur-addr value)
			(if (equal? scope-base stack-bottom) m
				(rec-write (imap-get m scope-base)))))
	;return input if empty
	(if (stack-empty m) m
		(rec-write (imap-get m stack-pointer-addr))))
	
;declare at the current top level scope
;default value is nullptr
;m: imap
(define (stack-decl m name)
	(define top-scope-base (imap-get m stack-pointer-addr))
	(define addr (scope-abs-addr top-scope-base name))
	(imap-set addr nullptr))

;push a scope to the top
;m: imap
(define (stack-push m)
	(define current-top (imap-get m stack-top-addr))
	(define current-pointer (imap-get m stack-pointer-addr))
	(define new-top (+ current-top scope-size))
	(imap-batch-set m (list (cons current-top current-pointer) (cons stack-pointer-addr current-top) (cons stack-top-addr new-top))))

;pop a scope from the top
;m: imap
(define (stack-pop m)
	(define current-pointer (imap-get m stack-pointer-addr))
	(define next-pointer (imap-get m current-pointer))
	(imap-set m stack-pointer-addr next-pointer))

;absolute address of a name in a scope
(define (scope-abs-addr scope-base name)
	(+ scope-base name 1))
