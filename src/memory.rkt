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
;(define (memory-sread mem name type)
;	(stack-read mem name type))

;write to stack
;(define (memory-swrite mem name value type)
;	(stack-write mem name value type))

;declare variable on current stack-top scope
;overwrites existing value
(define (memory-sdecl mem name type)
	(stack-decl mem name type))

;skip decl and lookup, force write to a certain level
(define (memory-sforce-write mem name value lvl type) 
	(stack-force-write mem name value lvl type))

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
(define (memory-hread mem addr type)
	(pretty-print addr)
	(imap-get (memory-heap mem) addr type))

;write to heap
(define (memory-hwrite mem addr value type)
	(std:struct-copy memory mem [heap (imap-set (memory-heap mem) addr value type)]))

;allocate array on heap
;memory X size -> addr(allocated) X memory(new) 
(define (memory-alloc mem size)
	(begin
	(define current-o-top (heap-meta-o-top (memory-h-meta mem)))
	(define current-a-top (heap-meta-a-top (memory-h-meta mem)))
	(define mem-size (memory-hwrite mem current-a-top size int-type))
	(cons (+ 1 current-a-top) (std:struct-copy memory mem-size [h-meta (heap-meta current-o-top (+ current-a-top size 1))]))))

;must use this to new object
(define (memory-new mem)
	(begin
	(define current-o-top (heap-meta-o-top (memory-h-meta mem)))
	(define current-a-top (heap-meta-a-top (memory-h-meta mem)))
	(cons current-o-top (std:struct-copy memory mem [h-meta (heap-meta (+ current-o-top 1) current-a-top)]))))
	
;-----------------Field Access---------------
;declare a new field (a field map is a map from obj-addr to field-addr)
;return (new memory)
;does not overwrite if exists
(define (memory-fdecl mem name) 
	(if (equal? (memory-vt-base mem name) (not-found addr-type))
		(match (memory-v-meta mem)
			[(vtab-meta n2t vtop)
				(begin
				(define n2t+ (imap-set n2t name vtop #f))
				(define vtop+ (+ vtop vt-size))
				(std:struct-copy memory mem
					[v-meta (vtab-meta n2t+ vtop+)]
					[h-meta (heap-meta vtop+ vtop+)]))])
		mem))

;read a field value of an object
(define (memory-fread mem fname obj-addr type)
	(define vt-addr (memory-vt-base mem fname))
	(define faddr (memory-vt-lookup mem vt-addr obj-addr))
	(memory-hread mem faddr type))

;write to a field of an object
(define (memory-fwrite mem fname obj-addr value type) 
;	(display (~a "memory-fwrite: " (list fname obj-addr value type)))
	(define vt-addr (memory-vt-base mem fname))
	(define faddr (memory-vt-lookup mem vt-addr obj-addr))
	(memory-hwrite mem faddr value type))

;return baes address of a virtual table
(define (memory-vt-base mem name)
	(imap-get (vtab-meta-name2tab (memory-v-meta mem)) name addr-type))

;return entry address of a virtual table
;must be used after initialization
(define (memory-vt-lookup mem vt-addr obj-addr)
	(+ vt-addr (- obj-addr (vtab-meta-top (memory-v-meta mem)))))

(define (memory-obj-index mem obj-addr)
	(- obj-addr (vtab-meta-top (memory-v-meta mem))))

;-----------------Array Access----------------
;array =  a memory range of any size on heap

;read value under an index from an array
(define (memory-aread mem arr-addr index type) 
	(memory-hread mem (+ arr-addr index) type))

;write to an index of an array
(define (memory-awrite mem arr-addr index value type)
	(memory-hwrite mem (+ arr-addr index) value type))

;==================================================




;============= Default Values ===========
(define memory-empty 
	(memory 
		invalid-id
		(vtab-meta (imap-empty addr-type) vt-base-addr)
		(heap-meta vt-base-addr vt-base-addr)
		imap-typed-empty
		stack-empty))

(define memory-invalid
	(std:struct-copy memory memory-empty [heap imap-typed-null]))

(memory-archive invalid-id memory-invalid)

(define (memory-heap-size mem)
	(imap-size (memory-heap mem)))
;========================================




;======================= Symbolic Operations ========================
(define (memory-sym-new summary?)
	(if summary? #f
		(begin
		(define new-id (memory-new-id))
		(std:struct-copy memory memory-empty [id new-id] [stack (stack-new)] [heap (imap-new new-id)]))))

;declares that m derives all states from m-base
(define (memory-sym-reset m m-base summary?)
	(if summary? m-base 
		(std:struct-copy memory m-base 
			[id (memory-id m)]
			[stack
				(stack-reset
					(memory-stack m)
					(memory-stack m-base))]
			[heap
				(imap-reset 
					(memory-heap m) 
					(memory-heap m-base))])))

;make writes so far visible to future reads
;only affect heap, stack always commit real-time
(define (memory-sym-commit m)
	(std:struct-copy memory m [heap (imap-commit (memory-heap m))]))

;This finishes a section. 
;No further operations(read/write) should be done on this state
;Only after this can a state be used as the base for a reset.
;Returns a formula that describes all updates during this section.
(define (memory-sym-summary m summary?)
	(if summary? #t
		(begin
		(memory-add-id (memory-id m))
		(memory-archive (memory-id m) m)
		(define fml-1 
			(imap-summary (memory-heap m)))
		(define fml-2
			(stack-summary (memory-stack m)))
		(and+ fml-1 fml-2))))

(define (memory-sym-ssummary m)
	(stack-summary (memory-stack m)))

;candidates: list of (condition X memory)
(define (memory-select candidates summary?)
	(if (and (equal? 1 (length candidates)) (imap-is-conc? (memory-heap (cdar candidates)))) (cdar candidates)
		(begin
		(define (extract-max f)
			(apply max (map f candidates)))
		(define obj-top-new (extract-max (compose heap-meta-o-top memory-h-meta cdr)))
		(define heap-top-new (extract-max (compose heap-meta-a-top memory-h-meta cdr)))

		(define mem-template (cdar candidates))

		(std:struct-copy memory mem-template
			[h-meta (heap-meta obj-top-new heap-top-new)]
			[stack
				(stack-select
					(map (lambda (cnd.m) (cons (car cnd.m) (memory-stack (cdr cnd.m)))) candidates) summary?)]
			[heap 
				(imap-select 
					(map (lambda (cnd.m) (cons (car cnd.m) (memory-heap (cdr cnd.m)))) candidates) summary?)]))))

;generate real formulae for preserved and updated states
(define (memory-gen-binding)
	(define f1 (stack-gen-binding))
	(define l2 (imap-gen-binding))
	(cons f1 l2))

;====================================================================
