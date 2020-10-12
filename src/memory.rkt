#lang rosette/safe

(require (prefix-in std: racket/base))
(require "map.rkt")
(require "stack.rkt")
(require "string-id.rkt")

(provide (all-defined-out))

;============= Definition & Operations ===========
(struct memory (stack heap names top) #:transparent)

;-----------------Stack Operations---------------
;read from stack
(define (memory-sread mem _name)
	(define name (maybe-string-id _name))
	(stack-read (memory-stack mem) name))

;write to stack
(define (memory-swrite mem _name value)
	(define name (maybe-string-id _name))
	(std:struct-copy memory mem 
		[stack (stack-write (memory-stack mem) name value)]))

;declare variable on current stack-top scope
;overwrites existing value
(define (memory-sdecl mem _name)
	(define name (maybe-string-id _name))
	(std:struct-copy memory mem [stack (stack-decl (memory-stack mem) name)]))

;decl & write
(define (memory-sforce-write mem _name value)
	(define name (maybe-string-id _name))
	(memory-swrite (memory-sdecl mem name) name value))

;push a scope to stack
(define (memory-spush mem)
	(std:struct-copy memory mem [stack (stack-push (memory-stack mem))]))

;pop a scope from stack
(define (memory-spop mem)
	(std:struct-copy memory mem [stack (stack-pop (memory-stack mem))]))
	

;-----------------Heap Operations---------------
;read from heap
(define (memory-hread mem addr)
	(imap-get (memory-heap mem) addr))

;write to heap
(define (memory-hwrite mem addr value)
	(std:struct-copy memory mem [heap (imap-set (memory-heap mem) addr value)]))

;(define (memory-hupdate mem addr update)
;	(define iheap (memory-heap mem))
;	(std:struct-copy memory mem [heap (imap-set iheap (update (imap-get iheap addr)))]))

;allocate memory on heap
;memory X size -> memory(new) X addr(allocated)
(define (memory-alloc mem size)
	(cons (memory-top mem) (std:struct-copy memory mem [top (+ (memory-top mem) size)])))


;-----------------Field Access---------------
;declare a new field (a field map is a map from obj-addr to field-addr)
;return (new memory)
;does not overwrite if exists
(define (memory-fdecl mem _name) 
	(define name (maybe-string-id _name))
	(if (= (imap-get (memory-names mem) name) not-found)
		(begin
			(define ret-pair (memory-alloc mem 1))
			(define addr (car ret-pair))
			(define mem-tmp (cdr ret-pair))
			(define mem-tmp2 (std:struct-copy memory mem-tmp [names (imap-set (memory-names mem-tmp) name addr)]))
			(memory-hwrite mem-tmp2 addr imap-empty))
		mem))


;read a field value of an object
(define (memory-fread mem _fname obj-addr)
	(define fname (maybe-string-id _fname))
	(define fid (imap-get (memory-names mem) fname))
	(define fmap (memory-hread mem fid))
	(define faddr (imap-get fmap obj-addr))
	(memory-hread mem faddr))

;write to a field of an object
;automatically allocate memory for the field if it's a new object
(define (memory-fwrite mem _fname obj-addr value) 
	(define fname (maybe-string-id _fname))
	(define fid (imap-get (memory-names mem) fname))
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


;-----------------Array Access----------------
;array =  a memory range of any size on heap

;read value under an index from an array
(define (memory-aread mem arr-addr index) 
	(memory-hread mem (+ arr-addr index)))

;write to an index of an array
(define (memory-awrite mem arr-addr index value)
	(memory-hwrite mem (+ arr-addr index) value))

;==================================================


;============= Default Values ===========
(define memory-empty (memory stack-empty imap-empty imap-empty 0))
;========================================


;============== Helpers =================
(define (memory->list mem from to)
	(define app (lambda (i)
		(if (= i to) null
		(cons (memory-hread mem i) (app (+ i 1)))))) 
	(app from))
;========================================

