#lang rosette/safe

(require (prefix-in std: racket/base))
(require "map.rkt")
(require "stack.rkt")
(require "string-id.rkt")
(require "memory-common.rkt")
(require "match-define.rkt")

(provide (all-defined-out))


;============= Definition & Operations ===========
(struct memory (imap) #:transparent)

;-----------------Stack Operations---------------
;read from stack
(define (memory-sread mem name)
	(stack-read (memory-imap mem) name))

;write to stack
(define (memory-swrite mem name value)
	(memory (stack-write (memory-imap mem) name value)))

;declare variable on current stack-top scope
;overwrites existing value
(define (memory-sdecl mem name)
	(memory (stack-decl (memory-imap mem) name)))

;decl & write
(define (memory-sforce-write mem name value)
	(memory-swrite (memory-sdecl mem name) name value))

;push a scope to stack
(define (memory-spush mem)
	(memory (stack-push (memory-imap mem))))

;pop a scope from stack
(define (memory-spop mem)
	(memory (stack-pop (memory-imap mem))))
	

;-----------------Heap Operations---------------
;read from heap
(define (memory-hread mem addr)
	(imap-get (memory-imap mem) addr))

;write to heap
(define (memory-hwrite mem addr value)
	(memory (imap-set (memory-imap mem) addr value)))

;allocate memory on heap
;memory X size -> addr(allocated) X memory(new) 
(define (memory-alloc mem size)
	(define current-top (imap-get (memory-imap mem) heap-top-addr))
	(cons current-top (memory (imap-set (memory-imap mem) heap-top-addr (+ current-top size)))))

;must use this to new object
(define (memory-new mem)
	(define current-top (imap-get (memory-imap mem) obj-top-addr))
	(cons current-top (memory (imap-set (memory-imap mem) obj-top-addr (+ current-top 1)))))
	
;-----------------Field Access---------------
;declare a new field (a field map is a map from obj-addr to field-addr)
;return (new memory)
;does not overwrite if exists
(define (memory-fdecl mem name) 
	(if (equal? (memory-vt-base mem name) not-found)
		(begin
			(match-define (cons addr mem-alloc) (memory-alloc mem vt-size))
			(memory
				(imap-batch-set (memory-imap mem-alloc) 
					(list 
					(cons (+ name vt-index-addr) addr)
					(cons obj-butt-addr (memory-hread mem-alloc heap-top-addr))
					(cons obj-top-addr (memory-hread mem-alloc heap-top-addr))))))
		mem))

;read a field value of an object
(define (memory-fread mem fname obj-addr)
	(define vt-addr (memory-vt-base mem fname))
	(define faddr (memory-vt-lookup mem vt-addr obj-addr))
	(memory-hread mem faddr))

;write to a field of an object
(define (memory-fwrite mem fname obj-addr value) 
	(define vt-addr (memory-vt-base mem fname))
	(define faddr (memory-vt-lookup mem vt-addr obj-addr))
	(memory-hwrite mem faddr value))

;return baes address of a virtual table
(define (memory-vt-base mem name)
	(imap-get (memory-imap mem) (+ name vt-index-addr)))

;return entry address of a virtual table
(define (memory-vt-lookup mem vt-addr obj-addr)
	(+ vt-addr (- obj-addr (memory-hread mem obj-butt-addr))))
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
	(memory (imap-batch-set imap-empty (list 
		(cons heap-top-addr vt-base-addr) 
		(cons obj-top-addr vt-base-addr)
		(cons obj-butt-addr vt-base-addr)
		(cons stack-top-addr stack-bottom) 
		(cons stack-pointer-addr stack-bottom)))))
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

;equivalent to (memory-sym-get-fml (memory-sym-reset m m-base))
(define (memory-is-copy m m-base)
	(imap-is-copy (memory-imap m) (memory-imap m-base)))
