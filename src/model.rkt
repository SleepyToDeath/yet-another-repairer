#lang rosette/safe

(require "string-id.rkt")
(require "memory.rkt")
(require "match-define.rkt")
(require "memory-common.rkt")
(require "formula.rkt")

(provide model-lookup)

;list of ((class-name X function-name) X virtual-function-model/static-function-model)
;virtual-function-model: (memory X object-addr X list of argument) -> memory
;used by virtual invoke & special invoke
;static-function-model: (memory X ret-var-name X list of argument) -> memory
;used by static invoke
(define model-list (list
	(cons 
		(cons "example.class.name" "example-virtual-function-name")
		(lambda (mem obj ret args) mem))

	(cons 
		(cons "example.class.name" "example-static-function-name")
		(lambda (mem ret args) mem))

))
	
; (class-name X function-name) -> function
(define (model-lookup cname fname)
	(ormap (lambda (mod)
			(define cname-m (string-id (caar mod)))
			(define fname-m (string-id (cdar mod)))
			(if (and (equal? cname cname-m) (equal? fname fname-m)) (cdr mod) #f))
		model-list))


