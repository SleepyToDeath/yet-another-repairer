#lang rosette/safe

(require (prefix-in std: racket/base))
(require "map.rkt")
(require "stack.rkt")
(require "heap.rkt")
(require "string-id.rkt")

(provide (all-defined-out))


;============= Definition & Operations ===========
(struct memory (imap) #:transparent)

;-----------------Stack Operations---------------
;read from stack
(define (memory-sread mem name)
	(stack-read (memory-imap mem) name))

;write to stack
(define (memory-swrite mem name value)
	(std:struct-copy memory mem 
		[imap (stack-write (memory-imap mem) name value)]))

;declare variable on current stack-top scope
;overwrites existing value
(define (memory-sdecl mem name)
	(std:struct-copy memory mem [imap (stack-decl (memory-imap mem) name)]))

;decl & write
(define (memory-sforce-write mem name value)
	(memory-swrite (memory-sdecl mem name) name value))

;push a scope to stack
(define (memory-spush mem)
	(std:struct-copy memory mem [imap (stack-push (memory-imap mem))]))

;pop a scope from stack
(define (memory-spop mem)
	(std:struct-copy memory mem [imap (stack-pop (memory-imap mem))]))
	

;-----------------Heap Operations---------------
;read from heap
(define (memory-hread mem addr)
	(imap-get (memory-imap mem) addr))

;write to heap
(define (memory-hwrite mem addr value)
	(std:struct-copy memory mem [imap (imap-set (memory-imap mem) addr value)]))

;allocate memory on heap
;memory X size -> addr(allocated) X memory(new) 
(define (memory-alloc mem size)
	(define current-top (imap-get (memory-imap mem) heap-top-addr))
	(cons current-top (std:struct-copy memory mem [imap (imap-set (memory-imap mem) heap-top-addr (+ current-top size))])))


;-----------------Field Access---------------
;declare a new field (a field map is a map from obj-addr to field-addr)
;return (new memory)
;does not overwrite if exists
(define (memory-fdecl mem _name) 
	(define name (maybe-string-id _name))
	(if (equal? (imap-get (memory-names mem) name) not-found)
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
		(if (equal? faddr not-found)
			(memory-alloc mem 1)
			(cons faddr mem)))
	(define mem-before-write
		(if (equal? faddr not-found)
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
(define memory-empty 
	(imap-batch-set imap-empty (list 
		(cons heap-top-addr vt-base-addr) 
		(cons stack-top-addr stack-bottom-addr) 
		(cons stack-pointer-addr stack-bottom-addr))))
;========================================

;======================= Symbolic Operations =========================
;Usage: 
;	See "map.rkt". Memory is just a wrapper for imap

(define (memory-sym-new)
	(memory (imap-sym-new)))

(define (memory-sym-reset m m-base)
	(memory (imap-sym-reset (memory-imap m) (memory-imap m-base))))

(define (memory-sym-get-fml m)
	(imap-sym-get-fml (memory-imap m)))

