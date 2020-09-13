#lang rosette/safe

(require (prefix-in std: racket/base))

(provide (all-defined-out))

;============= Definition & Operations ===========
(struct imap (func) #:transparent)

(define (imap-get m index)
	(define f (imap-func m))
	(f index))

(define (imap-set m index value)
	(define oldf (imap-func m))
	(define newf (lambda (args)
                 (if (= args index) value (oldf args))))
	(std:struct-copy imap m [func newf]))
;==================================================


;============= Default Values ===========
(define nullptr -1)

(define (default-func x) nullptr)

(define imap-empty (imap default-func))
;========================================

