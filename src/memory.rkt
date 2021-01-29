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
	(imap-get2 (memory-heap mem) addr no-scope))

;write to heap
(define (memory-hwrite mem addr value)
	(std:struct-copy memory mem [heap (imap-set (memory-heap mem) addr value)]))

;allocate memory on heap
;memory X size -> addr(allocated) X memory(new) 
(define (memory-alloc mem size)
	(begin
	(define current-o-top (heap-meta-o-top (memory-h-meta mem)))
	(define current-a-top (heap-meta-a-top (memory-h-meta mem)))
	(cons current-a-top (std:struct-copy memory mem [h-meta (heap-meta current-o-top (+ current-a-top size))]))))

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
	(if (equal? (memory-vt-base mem name) not-found)
		(match (memory-v-meta mem)
			[(vtab-meta n2t vtop)
				(begin
				(define n2t+ (imap-set n2t name vtop))
				(define vtop+ (+ vtop vt-size))
				(std:struct-copy memory mem
					[v-meta (vtab-meta n2t+ vtop+)]
					[h-meta (heap-meta vtop+ vtop+)]))])
		mem))

;read a field value of an object
(define (memory-fread mem fname obj-addr)
	(begin
	(define vt-addr (memory-vt-base mem fname))
	(define faddr (memory-vt-lookup mem vt-addr obj-addr))
	(memory-hread mem faddr)))

;write to a field of an object
(define (memory-fwrite mem fname obj-addr value) 
	(begin
	(define vt-addr (memory-vt-base mem fname))
	(define faddr (memory-vt-lookup mem vt-addr obj-addr))
	(memory-hwrite mem faddr value)))

;return baes address of a virtual table
(define (memory-vt-base mem name)
	(imap-get2 (vtab-meta-name2tab (memory-v-meta mem)) name no-scope))

;return entry address of a virtual table
;must be used after initialization
(define (memory-vt-lookup mem vt-addr obj-addr)
	(+ vt-addr (- obj-addr (vtab-meta-top (memory-v-meta mem)))))

(define (memory-obj-index mem obj-addr)
	(- obj-addr (vtab-meta-top (memory-v-meta mem))))
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
		nullptr
		(vtab-meta imap-empty vt-base-addr)
;		(stack-meta null stack-bottom)
		(heap-meta vt-base-addr vt-base-addr)
		imap-empty
		static-stack-empty))
;========================================

;======================= Symbolic Operations ========================
(define (memory-sym-new summary?)
	(if summary? #f
		(begin
		(define new-id (memory-new-id))
		(std:struct-copy memory memory-empty [id new-id] [stack (stack-new)] [heap (imap-sym-scoped-new new-id)]))))

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
				(imap-sym-scoped-reset 
					(memory-heap m) 
					(memory-heap m-base) 
					null)])))

;make writes so far visible to future reads
;only affect heap, stack always commit real-time
(define (memory-sym-commit m)
	(std:struct-copy memory m [heap (imap-sym-scoped-commit (memory-heap m))]))

;This finishes a section. 
;No further operations(read/write) should be done on this state
;Only after this can a state be used as the base for a reset.
;Returns a formula that describes all updates during this section.
(define (memory-sym-get-fml m summary?)
	(if summary? #t
		(begin
		(memory-add-id (memory-id m))
		(memory-archive (memory-id m) m)
		(and
			(imap-sym-scoped-get-fml (memory-heap m))
			(stack-get-fml (memory-stack))))))

;candidates: list of (condition X memory)
(define (memory-select candidates summary?)
;	(map (lambda (p.m) (pretty-print (cons (car p.m) (fml-to-print (cdr p.m))))) candidates)
;	(if (equal? 1 (length candidates)) (if summary? (if (car (car candidates)) (cdr (car candidates)) #f) (cdr (car candidates)))
	(if (and (equal? 1 (length candidates)) (imap-conc? (memory-heap (cdar candidates)))) (cdar candidates)
		(begin

		;remove maybe
;		(define candidates-semi-conc (reflection-map memory? identity (map cdr candidates)))
;		(display (~a "Candidate mems: " (length candidates-semi-conc) "\n"))

		(define (extract-max f)
			(apply max (map f candidates)))
;			(apply max (reflection-map number? identity 
;				(map f candidates-semi-conc))))

;		(define stack-top-new (extract-max (compose stack-meta-top memory-s-meta cdr)))
		(define obj-top-new (extract-max (compose heap-meta-o-top memory-h-meta cdr)))
		(define heap-top-new (extract-max (compose heap-meta-a-top memory-h-meta cdr)))

		(define mem-template (cdar candidates))

		(define ret (std:struct-copy memory mem-template
;			[s-meta (std:struct-copy stack-meta (memory-s-meta mem-template) [top stack-top-new])]
			[h-meta (heap-meta obj-top-new heap-top-new)]
			[stack
				(stack-select
					(map (lambda (cnd.m) (cons (car cnd.m) (memory-stack (cdr cnd.m)))) candidates) summary?)]
			[heap 
				(imap-sym-scoped-select 
					(map (lambda (cnd.m) (cons (car cnd.m) (memory-heap (cdr cnd.m)))) candidates)
					(remove-duplicates (apply append (map (compose imap-sym-scoped-scope memory-heap cdr) candidates)))
					summary?)]))
		ret)))

(define (memory-is-null mem)
	(not 
		(or (imap-conc? (memory-heap mem))
			(imap-sym? (imap-unwrap (memory-heap mem))))))

;generate real formulae for preserved and updated states
(define (memory-gen-binding mem)

	(define all-keys (imap-sym-tracked-keys (imap-sym-scoped-imap (memory-heap mem))))
	(pretty-print (~a "Totally " (length all-keys) " keys"))

	(define (id2keys id)
		(map car (imap-sym-committed-updates (imap-unwrap (vector-ref memory-id-map id)))))

	(define (contain-key? id key)
		(ormap identity (map (lambda (key0) (equal? key key0)) (id2keys id))))

	(display "\n ###############################################7.1 \n")

;		(check-asserts 11)

	(pretty-print (~a (length memory-id-list) " states:"))
	(pretty-print memory-id-list)

	(define fml-maybe-wrong
		(andmap identity (map (lambda (mem-id)
			(if (equal? mem-id 0) (std:error "processing mem 0!") #f)
			(pretty-print (~a "Generate keys for state #" mem-id))
			(define mem (vector-ref memory-id-map mem-id))
;				(pretty-print mem)
			(define fml-true 
				(andmap identity (map (lambda (key) 
						(define ret (if (is-concrete-value key)
							(imap-sym-key-fml (imap-unwrap mem) key)
							((lambda () 
								(define-symbolic* key-sym integer?)
								(and 
									(equal? key-sym key)
									(imap-sym-key-fml (imap-unwrap mem) key-sym))))))
						ret)
					(id2keys mem-id))))
			(define fml-deferred (imap-sym-fml-deferred (imap-unwrap mem)))
			(equal? fml-deferred fml-true))
			memory-id-list)))

	(display "\n ###############################################7.2 \n")

	(define kounter 0)

	(define fml-always-right
		(andmap identity (map (lambda (mem-id)
				(if (equal? mem-id 0) (std:error "processing mem 0!") #f)
				(define mem (vector-ref memory-id-map mem-id))
				(pretty-print (~a "Generate keys for state #" mem-id))
				(pretty-print (imap-sym-scoped-scope mem))
;					(imap-sym-lookback mem)
				(define ret (andmap identity (map (lambda (key.scope) 
						(if (in-scope? key.scope (imap-sym-scoped-scope mem))
							(begin
							(set! kounter (+ 1 kounter))
							(if (contain-key? mem-id (car key.scope)) #t
;									(and
									(if (is-concrete-value (car key.scope))
										(imap-sym-key-fml (imap-unwrap mem) (car key.scope))
										((lambda () 
											(define-symbolic* key-sym integer?)
											(and 
												(equal? key-sym (car key.scope))
												(imap-sym-key-fml (imap-unwrap mem) key-sym)))))))
							(imap-sym-key-not-found (imap-unwrap mem) (car key.scope))))
					all-keys)))
;					(display (~a "Memory id: " mem-id " formula size: " (size-of ret)))
				ret)
			memory-id-list)))

	(display (~a "Totally " kounter " keys in scope\n"))
	(defer-eval "Totally keys in scope: " kounter)

	(display "\n ###############################################7.3 \n")
	
	(define fml-code-bind (and fml-maybe-wrong fml-always-right))

	fml-code-bind)



;====================================================================


;======================= Helper ========================
(define (memory-print-id name m)
	(defer-eval "" (~a "current state id: " name " : " 
		(maybe-do imap-sym? #f (imap-sym-tracked-imap (imap-sym-scoped-imap (memory-heap m))) imap-sym-func-dummy) " <~ " 
		(maybe-do imap-sym? #f (imap-sym-tracked-imap (imap-sym-scoped-imap (memory-heap m))) imap-sym-func-base)  "\n")))

(define (in-scope? key.scope scope)
	#t)
;	(or
;		(equal? (cdr key.scope) no-scope)
;		(member (cdr key.scope) scope)))
;=======================================================


;====================== Tracking States ===================
(define memory-id-list null)
(define (memory-add-id id)
	(set! memory-id-list (cons id memory-id-list)))

(define memory-id-map (list->vector (std:build-list max-program-length (lambda (x) not-found))))
(define (memory-archive id mem)
	(vector-set! memory-id-map id mem))
(memory-archive 0 imap-sym-null)

(define memory-id-counter 0)
(define (memory-new-id)
	(set! memory-id-counter (+ 1 memory-id-counter))
	(display (~a "New state id: " memory-id-counter "\n"))
	memory-id-counter)


