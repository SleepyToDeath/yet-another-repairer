#lang rosette/safe

(require (prefix-in std: racket/base))
(require "map.rkt")
(require "stack.rkt")

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
(define (memory-sdecl mem name)
	(std:struct-copy memory mem [stack (stack-decl (memory-stack mem) name)]))

;push a scope to stack
(define (memory-spush mem)
	(std:struct-copy memory mem [stack (stack-push (memory-stack mem))]))

;pop a scope from stack
(define (memory-spop mem)
	(std:struct-copy memory mem [stack (stack-pop (memory-stack mem))]))
	
;read from heap
(define (memory-hread mem addr)
	(imap-get (memory-heap mem) addr))

;write to heap
(define (memory-hwrite mem addr value)
	(std:struct-copy memory mem [heap (imap-set (memory-heap mem) addr value)]))

;declare a new field (a field map is a map from obj-addr to field-addr)
;return (fid X new memory)
(define (memory-fdecl mem) 
	(define ret-pair (memory-alloc mem 1))
	(define addr (car ret-pair))
	(define mem-tmp (cdr ret-pair))
	(cons addr (memory-hwrite mem-tmp addr imap-empty)))

;read a field value of an object
(define (memory-fread mem fid obj-addr)
	(define fmap (memory-hread mem fid))
	(define faddr (imap-get fmap obj-addr))
	(memory-hread mem faddr))

;write to a field of an object
;automatically allocate memory for the field if it's a new object
(define (memory-fwrite mem fid obj-addr value) 
	(define fmap (memory-hread mem fid))
	(define faddr (imap-get fmap obj-addr))
	(define ret-pair 
		(if (= faddr not-found)
			(memory-alloc mem 1)
			(cons faddr mem)))
	(define mem-before-write
		(if (= faddr not-found)
			(memory-hwrite (cdr ret-pair) fid (imap-set fmap obj-addr (car ret-pair)))
			mem))
	(memory-hwrite mem-before-write (car ret-pair) value))

;read value under an index from an array
(define (memory-aread mem arr-addr index) 
	(memory-hread mem (+ arr-addr index)))

;write to an index of an array
(define (memory-awrite mem arr-addr index value)
	(memory-hwrite mem (+ arr-addr index) value))

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
		(cons (memory-hread mem i) (app (+ i 1)))))) 
	(app from))
;========================================

