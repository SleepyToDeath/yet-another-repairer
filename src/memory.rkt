#lang rosette/safe

(require racket/base)

(provide (all-defined-out))

;============= Definition & Operations ===========
(struct memory (func top) #:transparent )

(define (memory-load mem index)
	(define f (memory-func mem))
	(f index))

(define (memory-store mem index value)
	(define oldf (memory-func mem))
	(define newf (lambda (args)
                 (if (equal? args index) value (oldf args))))
	(struct-copy memory mem [func newf]))

;only used to update memory
;will return new memory
;use memory-top to get the allocated address
(define (memory-alloc mem)
	(struct-copy memory mem [top (+ (memory-top mem) 1)]))
;==================================================


;============= Default Values ===========
(define nullptr -1)

(define (default-func x) nullptr)

(define empty-memory (memory default-func nullptr))
;========================================

