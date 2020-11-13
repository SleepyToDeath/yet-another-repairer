#lang rosette/safe

(require (prefix-in std: racket/base))

(provide (all-defined-out))

;============= Definition & Operations ===========
(struct imap (func ilog) #:transparent)

(define (imap-get m index)
;	(print "<--")
;	(std:println index)
	(define f (imap-func m))
	(f index))

(define (imap-set m index value)
	(define oldf (imap-func m))
	(define newf (lambda (args)
		(if (equal? args nullptr) nullptr
			(if (equal? args index) value (oldf args)))))

;	(define oldlog (imap-ilog m))
;	(define newlog (cons (cons index (if (number? value) value #f)) oldlog))

	(std:struct-copy imap m [func newf][ilog newlog]))

(define (imap-contains? m index)
	(not (is-not-found? (imap-get m index))))

(define (is-not-found? v)
	(equal? v not-found))
;==================================================


;============= Default Values ===========
(define nullptr -1)
(define not-found -666)

(define (default-func x) not-found)

(define imap-empty (imap default-func null))
;========================================

