#lang rosette/safe

(require "string-id.rkt")
(require "memory.rkt")
(require "match-define.rkt")
(require "memory-common.rkt")
(require "formula.rkt")

(provide model-lookup)

;How to add a built-in model:
;Function: just register the implementation to model-list 
;Class: (1) register all its functions to model-list
;		(2) add an empty class with only inheritance information to 
;			every project's source file
;		(3) if you want to reuse memory management for fields in `memory.rkt`, 
;			also add any such fields to the class in source file

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
	(set! model-list (cons (cons (cons cname fname) model-list) model-list)))
	
; (class-name X function-name) -> function
(define (model-lookup cname fname)
	(ormap (lambda (mod)
			(define cname-m (string-id (caar mod)))
			(define fname-m (string-id (cdar mod)))
			(if (and (equal? cname cname-m) (equal? fname fname-m)) (cdr mod) #f))
		model-list))





;================= HashMap ===================

(define HashMap-funcs (list
	(cons
		(cons "java.util.HashMap" "<init>")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.HashMap" "values")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.HashMap" "remove")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.HashMap" "get")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.HashMap" "put")
		(lambda (mem obj ret args) mem))

	(cons
		(cons "java.util.HashMap" "containsKey")
		(lambda (mem obj ret args) mem))
))

;==============================================


;================ Collection ==================

(define Collection-funcs (list

	(cons
		(cons "java.util.HashMap" "containsKey")
		(lambda (mem obj ret args) mem))


))

;==============================================
