#lang rosette/safe

(require racket/base)
(require "map.rkt")

(provide (all-defined-out))

;============= Definition & Operations ===========
(struct memory (imap top) #:transparent )

(define (memory-load mem index)
	(imap-get (memory-imap mem) index))

(define (memory-store mem index value)
	(struct-copy memory mem 
		[imap (imap-set (memory-imap mem) index value)]))

;only used to update memory
;will return new memory
;use memory-top to get the allocated address
(define (memory-alloc mem)
	(struct-copy memory mem [top (+ (memory-top mem) 1)]))
;==================================================


;============= Default Values ===========
(define memory-empty (memory imap-empty nullptr))
;========================================

