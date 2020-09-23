#lang rosette/safe

(require (prefix-in std: racket/base))
(require "map.rkt")

(provide (all-defined-out))

;============= Definition & Operations ===========
(struct memory (stack heap top) #:transparent)

;read from stack
(define (memory-sread mem name)
	(stack-read (memory-stack mem) name))

;write to stack
(define (memory-swrite mem name value)
	(std:struct-copy memory mem 
		[stack (stack-write (memory-stack mem) name value)]))

;declare variable on current stack-top scope
(define (memory-decl mem name)
	(std:struct-copy memory mem [stack (stack-decl (memory-stack mem) name)]))

;push a scope to stack
(define (memory-push mem)
	(std:struct-copy memory mem [stack (stack-push (memory-stack mem))]))

;pop a scope from stack
(define (memory-pop mem)
	(std:struct-copy memory mem [stack (stack-pop (memory-stack mem))]))
	
;read from heap
(define (memory-hread mem addr)
	(imap-get (memory-heap mem) addr))

;write to heap
(define (memory-hwrite mem addr value)
	(std:struct-copy memory mem [heap (imap-set (memory-heap mem) addr value)]))

;allocate memory on heap
;memory X size -> memory(new) X addr(allocated)
(define (memory-alloc mem size)
	(cons (memory-top mem) (std:struct-copy memory mem [top (+ (memory-top mem) size)])))
;==================================================


;============= Default Values ===========
(define memory-empty (memory stack-empty imap-empty 0))
;========================================


;============== Helpers =================
(define (memory->list mem from to)
	(define app (lambda (i)
		(if (= i to) null
		(cons (memory-load mem i) (app (+ i 1)))))) 
	(app from))
;========================================

