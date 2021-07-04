#lang rosette/safe

(require "string-id.rkt")
(require "memory.rkt")
(require "match-define.rkt")
(require "memory-common.rkt")
(require "formula.rkt")
(require "type-checker.rkt")
(require "semantics-common.rkt")
(require racket/format)

(provide model-lookup)

;How to add a built-in model:
;
;Function: just register the implementation to model-list 
;
;Class: 
;	If you don't ever need to create an instance:
;		Add all functions in the same way as above
;
;	Otherwise:
;		(1) register all its functions to model-list
;		(2) add an empty class with only inheritance information to 
;			every project's source file
;		(3) if you want to reuse memory management for fields in `memory.rkt`, 
;			also add any such fields to the class in the source file



;list of ((class-name X function-name) X virtual-function-model/static-function-model)

;virtual-function-model: (memory X object-addr X ret-var-name X list of argument) -> memory
;used by virtual invoke & special invoke

;static-function-model: (memory X ret-var-name X list of argument) -> memory
;used by static invoke
(define model-list (list
	(cons 
		(cons "example.class.name" "example-virtual-function-name")
		(lambda (mem obj ret args) mem))

	(cons 
		(cons "example.class.name" "example-static-function-name")
		(lambda (mem ret args) mem))))

(define (model-register cname fname handler)
	(set! model-list (cons (cons (cons cname fname) handler) model-list)))
	
; (class-name X function-name) -> function
(define (model-lookup cname fname)
	(ormap (lambda (mod)
			(define cname-m (string-id (caar mod)))
			(define fname-m (string-id (cdar mod)))
			(if (and (equal? cname cname-m) (equal? fname fname-m)) (cdr mod) #f))
		model-list))





;################ Java Library ##################

;============ Capacity Limits ============
(define model-all-capacity 500)
(define (model-all-hash-func x mem) 
;	(do-n-ret (lambda (y) (force-error (or (< y 0) (>= y model-all-capacity)) (~a "Hash our of bound!\n" x " -> " y "\n")))
		(if (< x 0) 0
			(+ 1 (if (> x (vtab-meta-top (memory-v-meta mem)))
					(- x (vtab-meta-top (memory-v-meta mem)))
					x))))
;(define (model-all-hash-func x) (modulo x 47))

;================= Map ===================
;[!] Need source file
;currently a copy of HashMap

;hash func: h(x) = x modulo a large prime number
;[!] Assuming collision free

(define map-class-name (string-id "java.util.Map"))
(define map-hash-func model-all-hash-func)
(define map-max-capacity model-all-capacity)
(define map-fname-kv (string-id "KVStore"))
(define (map-fid-kv)
	(vfield-id current-context map-class-name map-fname-kv))

(define Map-funcs (list
	(cons
		(cons "java.util.Map" "<init>")
		(lambda (mem obj ret args)
			(define fid-class-name (vfield-id current-context map-class-name field-name-class))
			(define mem-bind (memory-fwrite mem fid-class-name obj map-class-name name-type))
			(match-define (cons addr-kv mem-arr) (memory-alloc mem-bind map-max-capacity))
			(defer-eval current-spec-id "Map.<init>" (list obj addr-kv fid-class-name args))
			(define mem-ass (memory-fwrite mem-arr (map-fid-kv) obj addr-kv addr-type))
			mem-ass
		))

	;[?] how to do this?
	(cons
		(cons "java.util.Map" "values")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.Map" "remove")
		(lambda (mem obj ret args) 
			(define key (map-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (map-fid-kv) obj addr-type))
			(defer-eval current-spec-id "Map.remove" (list obj addr-kv key args))
			(define mem-rm (memory-awrite mem addr-kv key (not-found addr-type) addr-type))
			mem-rm
		))

	(cons
		(cons "java.util.Map" "get")
		(lambda (mem obj ret args)
			(define key (map-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (map-fid-kv) obj addr-type))
			(define value.maybe (memory-aread mem addr-kv key addr-type))
			(define value (if (equal? value.maybe (not-found addr-type)) (nullptr addr-type) value.maybe))
			(defer-eval current-spec-id "Map.get" (list obj addr-kv key value args))
			(display (~a "Map.get: " (list obj addr-kv key value args) "\n"))
			(define mem-ret (memory-sforce-write mem ret value 0 addr-type))
			mem-ret
		))

	(cons
		(cons "java.util.Map" "put")
		(lambda (mem obj ret args)
			(define key (map-hash-func (first args) mem))
			(define value (second args))
			(define addr-kv (memory-fread mem (map-fid-kv) obj addr-type))
			(defer-eval current-spec-id "Map.put" (list obj addr-kv key value args))
			(define mem-put (memory-awrite mem addr-kv key value addr-type))
			mem-put
		))

	(cons
		(cons "java.util.Map" "containsKey")
		(lambda (mem obj ret args)
			(define key (map-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (map-fid-kv) obj addr-type))
			(define value (memory-aread mem addr-kv key addr-type))
			(define flag (not (equal? value (not-found addr-type))))
			(defer-eval current-spec-id "Map.containsKey" (list obj addr-kv key value flag args))
			(define mem-ret (memory-sforce-write mem ret flag 0 addr-type))
			mem-ret
		))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) Map-funcs)

;==============================================




;================= HashMap ===================
;[!] Need source file

;hash func: h(x) = x modulo a large prime number
;[!] Assuming collision free

(define hashmap-class-name (string-id "java.util.HashMap"))
(define hashmap-hash-func model-all-hash-func)
(define hashmap-max-capacity model-all-capacity)
(define hashmap-fname-kv (string-id "KVStore"))
(define (hashmap-fid-kv)
	(vfield-id current-context hashmap-class-name hashmap-fname-kv))

(define HashMap-funcs (list
	(cons
		(cons "java.util.HashMap" "<init>")
		(lambda (mem obj ret args)
			(define fid-class-name (vfield-id current-context hashmap-class-name field-name-class))
			(define mem-bind (memory-fwrite mem fid-class-name obj hashmap-class-name name-type))
			(match-define (cons addr-kv mem-arr) (memory-alloc mem-bind hashmap-max-capacity))
			(defer-eval current-spec-id "HashMap.<init>" (list obj addr-kv fid-class-name args))
			(define mem-ass (memory-fwrite mem-arr (hashmap-fid-kv) obj addr-kv addr-type))
			mem-ass
		))

	;[?] how to do this?
	(cons
		(cons "java.util.HashMap" "values")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.HashMap" "remove")
		(lambda (mem obj ret args) 
			(define key (hashmap-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (hashmap-fid-kv) obj addr-type))
			(defer-eval current-spec-id "HashMap.remove" (list obj addr-kv key args))
			(define mem-rm (memory-awrite mem addr-kv key (not-found addr-type) addr-type))
			mem-rm
		))

	(cons
		(cons "java.util.HashMap" "get")
		(lambda (mem obj ret args)
			(define key (hashmap-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (hashmap-fid-kv) obj addr-type))
			(define value.maybe (memory-aread mem addr-kv key addr-type))
			(define value (if (equal? value.maybe (not-found addr-type)) (nullptr addr-type) value.maybe))
;			(display (~a "HashMap.get: " (list obj addr-kv key value args) "\n"))
			(defer-eval current-spec-id "HashMap.get" (list obj addr-kv key value args))
			(define mem-ret (memory-sforce-write mem ret value 0 addr-type))
			mem-ret
		))

	(cons
		(cons "java.util.HashMap" "put")
		(lambda (mem obj ret args)
			(define key (hashmap-hash-func (first args) mem))
			(define value (second args))
			(define addr-kv (memory-fread mem (hashmap-fid-kv) obj addr-type))
			(defer-eval current-spec-id "HashMap.put" (list obj addr-kv key value args))
			(define mem-put (memory-awrite mem addr-kv key value addr-type))
			mem-put
		))

	(cons
		(cons "java.util.HashMap" "containsKey")
		(lambda (mem obj ret args)
			(define key (hashmap-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (hashmap-fid-kv) obj addr-type))
			(define value (memory-aread mem addr-kv key addr-type))
			(define flag (not (equal? value (not-found addr-type))))
			(defer-eval current-spec-id "HashMap.containsKey" (list obj addr-kv key value flag args))
			(define mem-ret (memory-sforce-write mem ret flag 0 addr-type))
			mem-ret
		))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) HashMap-funcs)

;==============================================

;================= HashSet ===================
;[!] Need source file

;hash func: h(x) = x modulo a large prime number
;[!] Assuming collision free

(define hashset-class-name (string-id "java.util.HashSet"))
(define hashset-hash-func model-all-hash-func)
(define hashset-max-capacity model-all-capacity)
(define hashset-fname-v (string-id "VStore"))
(define (hashset-fid-v)
	(vfield-id current-context hashset-class-name hashset-fname-v))
(define hashset-exists-flag 1)

(define HashSet-funcs (list
	(cons
		(cons "java.util.HashSet" "<init>")
		(lambda (mem obj ret args)
			(define fid-class-name (vfield-id current-context hashset-class-name field-name-class))
			(define mem-bind (memory-fwrite mem fid-class-name obj hashset-class-name addr-type))
			(match-define (cons addr-kv mem-arr) (memory-alloc mem-bind hashset-max-capacity))
			(define mem-ass (memory-fwrite mem-arr (hashset-fid-v) obj addr-kv addr-type))
			mem-ass
		))

	(cons
		(cons "java.util.HashSet" "remove")
		(lambda (mem obj ret args) 
			(define key (hashset-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (hashset-fid-v) obj addr-type))
			(define mem-rm (memory-awrite mem addr-kv key (not-found addr-type) addr-type))
			mem-rm
		))

	(cons
		(cons "java.util.HashSet" "add")
		(lambda (mem obj ret args)
			(define key (hashset-hash-func (first args) mem))
			(define value hashset-exists-flag)
			(define addr-kv (memory-fread mem (hashset-fid-v) obj addr-type))
			(define mem-put (memory-awrite mem addr-kv key value addr-type))
			mem-put
		))

	(cons
		(cons "java.util.HashSet" "contains")
		(lambda (mem obj ret args)
			(define key (hashset-hash-func (first args) mem))
			(define addr-kv (memory-fread mem (hashset-fid-v) obj addr-type))
			(define value (memory-aread mem addr-kv key addr-type))
			(define flag (equal? value hashset-exists-flag))
			(define mem-ret (memory-sforce-write mem ret flag 0 addr-type))
			mem-ret
		))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) HashSet-funcs)

;==============================================

;================ ArrayList ===================
;[!] Need source file

(define arraylist-max-capacity 5)

(define ArrayList-funcs (list

	;[TODO] dummy
	(cons 
		(cons "java.util.ArrayList" "<init>")
		(lambda (mem obj ret args)
			(define fid-class-name (vfield-id current-context (string-id "java.util.ArrayList") field-name-class))
			(define mem-bind (memory-fwrite mem fid-class-name obj (string-id "java.util.ArrayList") name-type))
			mem-bind
		))

	;[TODO] dummy
	(cons 
		(cons "java.util.ArrayList" "add")
		(lambda (mem obj ret args)
			mem
		))

	;[TODO] dummy
	(cons 
		(cons "java.util.ArrayList" "remove")
		(lambda (mem obj ret args)
			mem
		))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) ArrayList-funcs)
;==============================================

;================= Optional ===================
(define Optional-funcs (list ))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) Optional-funcs)
;==============================================

;================== String ====================
(define String-funcs (list
	(cons
		(cons "java.lang.String" "valueOf")
		(lambda (mem ret args)
			(define vi (first args))
			(define mem-ret (memory-sforce-write mem ret (string-value-of vi) 0 string-type))
			mem-ret))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) String-funcs)
;==============================================

;=================== Enum =====================
(define Enum-funcs (list
	(cons
		(cons "java.lang.String" "valueOf")
		(lambda (mem ret args)
			mem))
))
(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) Enum-funcs)
;==============================================

;================= Object =====================
(define Object-funcs (list
	(cons
		;[!] not true clone, just returning the original one
		(cons "java.lang.Object" "clone")
		(lambda (mem obj ret args)
			(define mem-ret (memory-sforce-write mem ret obj 0 addr-type))
			mem-ret))
))
(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) Object-funcs)
;==============================================

;=================== Math =====================
(define math-random-num 1)

(define Math-funcs (list
	(cons
		(cons "java.lang.Math" "random")
		(lambda (mem ret args)
			(define mem-ret (memory-sforce-write mem ret math-random-num 0 int-type))
			mem-ret))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) Math-funcs)
;==============================================

;================ Collection ==================
(define Collection-funcs (list	))
;==============================================







;################ Network Data Types ##################
(define mac-class-name (string-id "org.projectfloodlight.openflow.types.MacAddress"))
(define mac-fname-raw (string-id "rawValue"))
(define (mac-fid-raw)
	(vfield-id current-context mac-class-name mac-fname-raw))
(define MacAddress-funcs (list
	(cons
		(cons "org.projectfloodlight.openflow.types.MacAddress" "<init>")
		(lambda (mem obj ret args)
			(define fid-class-name (vfield-id current-context mac-class-name field-name-class))
			(define mem-bind (memory-fwrite mem fid-class-name obj mac-class-name name-type))
			(define mem-ass (memory-fwrite mem-bind (mac-fid-raw) obj (first args) bv-type))
			mem-ass))

	(cons
		(cons "org.projectfloodlight.openflow.types.MacAddress" "of")
		(lambda (mem ret args)
			(match-define (cons obj mem-alloc) (memory-new mem))

			(define fid-class-name (vfield-id current-context mac-class-name field-name-class))
			(define mem-bind (memory-fwrite mem-alloc fid-class-name obj mac-class-name name-type))
			(define mem-ass (memory-fwrite mem-bind (mac-fid-raw) obj (first args) bv-type))

			(define mem-ret (memory-sforce-write mem ret obj 0 addr-type))
			mem-ret))
))
			

;(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) MacAddress-funcs)
