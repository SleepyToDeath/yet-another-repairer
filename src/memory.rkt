#lang rosette/safe

(require rosette/lib/match)   ; provides `match`
(require (prefix-in std: racket/base))
(require racket/format)
(require "formula.rkt")
(require "map.rkt")
(require "stack.rkt")
(require "string-id.rkt")
(require "memory-common.rkt")
(require "match-define.rkt")

(provide (all-defined-out))


;============= Definition & Operations ===========

;-----------------Stack Operations---------------
;read from stack
(define (memory-sread mem name)
	(stack-read mem name))

;write to stack
(define (memory-swrite mem name value)
	(stack-write mem name value))

;declare variable on current stack-top scope
;overwrites existing value
(define (memory-sdecl mem name)
	(stack-decl mem name))

;skip decl and lookup, force write to a certain level
(define (memory-sforce-write mem name value lvl) 
	(stack-force-write mem name value lvl))

;skip lookup, force read from a certain level
(define (memory-sforce-read mem name lvl)
	(stack-force-read mem name lvl))

;push a scope to stack
(define (memory-spush mem)
	(stack-push mem))

;pop a scope from stack
(define (memory-spop mem)
	(stack-pop mem))
	

;-----------------Heap Operations---------------
;read from heap
(define (memory-hread mem addr)
	(imap-get2 (memory-addr-space mem) addr no-scope))

;write to heap
(define (memory-hwrite mem addr value)
	(std:struct-copy memory mem [addr-space (imap-set (memory-addr-space mem) addr value)]))

;allocate memory on heap
;memory X size -> addr(allocated) X memory(new) 
(define (memory-alloc mem size)
	(define current-o-top (heap-meta-o-top (memory-h-meta mem)))
	(define current-a-top (heap-meta-a-top (memory-h-meta mem)))
	(cons current-a-top (std:struct-copy memory mem [h-meta (heap-meta current-o-top (+ current-a-top size))])))

;must use this to new object
(define (memory-new mem)
	(define current-o-top (heap-meta-o-top (memory-h-meta mem)))
	(define current-a-top (heap-meta-a-top (memory-h-meta mem)))
	(cons current-o-top (std:struct-copy memory mem [h-meta (heap-meta (+ current-o-top 1) current-a-top)])))
	
;-----------------Field Access---------------
;declare a new field (a field map is a map from obj-addr to field-addr)
;return (new memory)
;does not overwrite if exists
(define (memory-fdecl mem name) 
	(if (equal? (memory-vt-base mem name) not-found)
		(match (memory-v-meta mem)
			[(vtab-meta n2t vtop)
				(begin
				(define n2t+ (imap-set n2t name vtop))
				(define vtop+ (+ vtop vt-size))
				(memory
					(vtab-meta n2t+ vtop+)
					(memory-s-meta mem)
					(heap-meta vtop+ vtop+)
					(memory-addr-space mem)))])
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
	(imap-get2 (vtab-meta-name2tab (memory-v-meta mem)) name no-scope))

;return entry address of a virtual table
;must be used after initialization
(define (memory-vt-lookup mem vt-addr obj-addr)
	(+ vt-addr (- obj-addr (vtab-meta-top (memory-v-meta mem)))))
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
	(memory 
		(vtab-meta imap-empty vt-base-addr)
		(stack-meta null stack-bottom)
		(heap-meta vt-base-addr vt-base-addr)
		imap-empty))
;========================================

;======================= Symbolic Operations ========================
;Usage: 
;	See "map.rkt". Memory is just a wrapper for imap

(define (memory-sym-new summary?)
	(if summary? #f
		(std:struct-copy memory memory-empty [addr-space (imap-sym-scoped-new)])))

(define (memory-sym-reset m m-base summary?)
	(if summary? m-base 
		(std:struct-copy memory m-base 
			[addr-space 
				(imap-sym-scoped-reset 
					(memory-addr-space m) 
					(memory-addr-space m-base) 
					(stack-meta-bases (memory-s-meta m-base)))])))

(define (memory-sym-commit m)
	(std:struct-copy memory m [addr-space (imap-sym-scoped-commit (memory-addr-space m))]))

(define (memory-sym-get-fml m summary?)
	(if summary? #t
		(imap-sym-scoped-get-fml (memory-addr-space m))))

;candidates: list of (condition X memory)
;only select address space, others are static
(define (memory-select candidates)
	(if (equal? 1 (length candidates)) (cdr (car candidates))
		(begin
		(define stack-top-new (apply max (map (lambda (p+m) (stack-meta-top (memory-s-meta (cdr p+m)))) candidates)))
		(define obj-top-new (apply max (map (lambda (p+m) (heap-meta-o-top (memory-h-meta (cdr p+m)))) candidates)))
		(define heap-top-new (apply max (map (lambda (p+m) (heap-meta-a-top (memory-h-meta (cdr p+m)))) candidates))) 
		(std:struct-copy memory (cdr (car candidates)) 
			[s-meta (std:struct-copy stack-meta (memory-s-meta (cdar candidates)) [top stack-top-new])]
			[h-meta (heap-meta obj-top-new heap-top-new)]
			[addr-space 
				(imap-sym-scoped-select 
					(map (lambda (p+m) (cons (car p+m) (memory-addr-space (cdr p+m)))) candidates)
					(remove-duplicates (apply append (map (compose imap-sym-scoped-scope memory-addr-space cdr) candidates))))]))))
;====================================================================


;======================= Helper ========================
(define (memory-print-id name m)
	(defer-eval "" (~a "current state id: " name " : " 
		(imap-sym-func-dummy (imap-sym-tracked-imap (imap-sym-scoped-imap (memory-addr-space m)))) " <~ " 
		(imap-sym-func-base (imap-sym-tracked-imap (imap-sym-scoped-imap (memory-addr-space m))))  "\n")))

(define (in-scope? key.scope scope)
	(or
		(equal? (cdr key.scope) no-scope)
		(member (cdr key.scope) scope)))
;=======================================================



