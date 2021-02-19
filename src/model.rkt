#lang rosette/safe

(require "string-id.rkt")
(require "memory.rkt")
(require "match-define.rkt")
(require "memory-common.rkt")
(require "formula.rkt")
(require "semantics-common.rkt")

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





;================= HashMap ===================
;[!] Need source file

;hash func: h(x) = x modulo a large prime number
;[!] Assuming collision free

(define (hashmap-hash-func x) (modulo x 179))
(define hashmap-max-capacity 200)
(define hashmap-fname-kv (string-id "KVStore"))

(define HashMap-funcs (list
	(cons
		(cons "java.util.HashMap" "<init>")
		(lambda (mem obj ret args)
			(define fid-class-name (vfield-id current-context (string-id "java.util.HashMap") field-name-class))
			(define mem-bind (memory-fwrite mem fid-class-name obj (string-id "java.util.HashMap")))
			(match-define (cons addr-kv mem-arr) (memory-alloc mem-bind hashmap-max-capacity))
			(define mem-ass (memory-fwrite mem-arr hashmap-fname-kv obj addr-kv))
			mem-ass
		))

	;[?] how to do this?
	(cons
		(cons "java.util.HashMap" "values")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.HashMap" "remove")
		(lambda (mem obj ret args) 
			(define key (hashmap-hash-func (first args)))
			(define addr-kv (memory-fread mem hashmap-fname-kv obj))
			(define mem-rm (memory-awrite mem addr-kv key not-found))
			mem-rm
		))

	(cons
		(cons "java.util.HashMap" "get")
		(lambda (mem obj ret args)
			(define key (hashmap-hash-func (first args)))
			(define addr-kv (memory-fread mem hashmap-fname-kv obj))
			(define value.maybe (memory-aread mem addr-kv key))
			(define value (if (equal? value.maybe not-found) nullptr value.maybe))
			(define mem-ret (memory-sforce-write mem ret value 0))
			mem-ret
		))

	(cons
		(cons "java.util.HashMap" "put")
		(lambda (mem obj ret args)
			(define key (hashmap-hash-func (first args)))
			(define value (second args))
			(define addr-kv (memory-fread mem hashmap-fname-kv obj))
			(define mem-put (memory-awrite mem addr-kv key value))
			mem-put
		))

	(cons
		(cons "java.util.HashMap" "containsKey")
		(lambda (mem obj ret args)
			(define key (hashmap-hash-func (first args)))
			(define addr-kv (memory-fread mem hashmap-fname-kv obj))
			(define value (memory-aread mem addr-kv key))
			(define flag (not (equal? value not-found)))
			(define mem-ret (memory-sforce-write mem ret flag 0))
			mem-ret
		))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) HashMap-funcs)

;==============================================

;================= HashSet ===================
;[!] Need source file

;hash func: h(x) = x modulo a large prime number
;[!] Assuming collision free

(define (hashset-hash-func x) (modulo x 17))
(define hashset-max-capacity 20)
(define hashset-fname-v (string-id "VStore"))
(define hashset-exists-flag 1)

(define HashSet-funcs (list
	(cons
		(cons "java.util.HashSet" "<init>")
		(lambda (mem obj ret args)
			(define fid-class-name (vfield-id current-context (string-id "java.util.HashSet") field-name-class))
			(define mem-bind (memory-fwrite mem fid-class-name obj (string-id "java.util.HashSet")))
			(match-define (cons addr-kv mem-arr) (memory-alloc mem-bind hashset-max-capacity))
			(define mem-ass (memory-fwrite mem-arr hashset-fname-v obj addr-kv))
			mem-ass
		))

	(cons
		(cons "java.util.HashSet" "remove")
		(lambda (mem obj ret args) 
			(define key (hashset-hash-func (first args)))
			(define addr-kv (memory-fread mem hashset-fname-v obj))
			(define mem-rm (memory-awrite mem addr-kv key not-found))
			mem-rm
		))

	(cons
		(cons "java.util.HashSet" "add")
		(lambda (mem obj ret args)
			(define key (hashset-hash-func (first args)))
			(define value hashset-exists-flag)
			(define addr-kv (memory-fread mem hashset-fname-v obj))
			(define mem-put (memory-awrite mem addr-kv key value))
			mem-put
		))

	(cons
		(cons "java.util.HashSet" "contains")
		(lambda (mem obj ret args)
			(define key (hashset-hash-func (first args)))
			(define addr-kv (memory-fread mem hashset-fname-v obj))
			(define value (memory-aread mem addr-kv key))
			(define flag (equal? value hashset-exists-flag))
			(define mem-ret (memory-sforce-write mem ret flag 0))
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
			(define mem-bind (memory-fwrite mem fid-class-name obj (string-id "java.util.ArrayList")))
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

;================== String ====================
(define String-funcs (list
	(cons
		(cons "java.lang.String" "valueOf")
		(lambda (mem ret args)
			(define vi (first args))
			(define mem-ret (memory-sforce-write mem ret (+ vi string-id-int-offset) 0))
			mem-ret))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) String-funcs)
;==============================================

;=================== Math =====================
(define math-random-num 1)

(define Math-funcs (list
	(cons
		(cons "java.lang.Math" "random")
		(lambda (mem ret args)
			(define mem-ret (memory-sforce-write mem ret math-random-num 0))
			mem-ret))
))

(map (lambda (m) (model-register (caar m) (cdar m) (cdr m))) Math-funcs)
;==============================================

;================ Collection ==================
(define Collection-funcs (list	))
;==============================================
