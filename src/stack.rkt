#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/format)
(require "map.rkt")
(require "memory-common.rkt")

(provide (all-defined-out))

(define (stack-empty? mem)
	(empty? (stack-meta-bases (memory-s-meta mem))))

;read the first defined name in stack
;m: imap
(define (stack-read mem name)
	;return not-found if empty
	(if (stack-empty? mem) not-found
		(begin
		(define bases (stack-meta-bases (memory-s-meta mem)))
		(define top0 (car bases))
		(define top1 (if (null? (cdr bases)) top0 (cadr bases)))
		(define butt0 (car (reverse bases)))
		(define butt1 (if (null? (cdr bases)) butt0 (cadr (reverse bases))))
		(define maybe-ret (ormap identity
			(map 
				(lambda (scope-base)
					(define cur-addr (+ scope-base name))
					(define cur-val (imap-get (memory-addr-space mem) cur-addr))
					(if (is-not-found? cur-val) #f cur-val))
				(list top0 top1 butt1 butt0))))
		(if maybe-ret maybe-ret
			(begin
			(pretty-print (~a "not found: " name))
			(pretty-print mem)
			not-found)))))

;write to the first defined name in stack
;m: imap
(define (stack-write mem name value)
	;return input if empty
	(if (stack-empty? mem) mem
		(begin
		(define bases (stack-meta-bases (memory-s-meta mem)))
		(define top0 (car bases))
		(define top1 (if (null? (cdr bases)) top0 (cadr bases)))
		(define butt0 (car (reverse bases)))
		(define butt1 (if (null? (cdr bases)) butt0 (cadr (reverse bases))))
		(define maybe-updated 
			(ormap identity
				(map 
					(lambda (scope-base)
						(define cur-addr (+ scope-base name))
						(define cur-val (imap-get (memory-addr-space mem) cur-addr))
						(if (is-not-found? cur-val) #f
							(std:struct-copy memory mem [addr-space (imap-set (memory-addr-space mem) cur-addr value)])))
				(list top0 top1 butt1 butt0))))
		(if maybe-updated maybe-updated mem))))
	
;declare at the current top level scope
;default value is nullptr
;mem shouldn't be empty
(define (stack-decl mem name)
	(define bases (stack-meta-bases (memory-s-meta mem)))
	(define top0 (car bases))
	(define addr (+ top0 name))
	(std:struct-copy memory mem [addr-space (imap-set (memory-addr-space mem) addr nullptr)]))

;push a scope to the top
(define (stack-push mem)
	(define top (stack-meta-top (memory-s-meta mem)))
	(define bases (stack-meta-bases (memory-s-meta mem)))
	(define top+ (+ top scope-size))
	(define bases+ (cons top+ bases))
	(std:struct-copy memory mem [s-meta (stack-meta bases+ top+)]))

;pop a scope from the top
(define (stack-pop mem)
	(define top (stack-meta-top (memory-s-meta mem)))
	(define bases (stack-meta-bases (memory-s-meta mem)))
	(define bases+ (cdr bases))
	(std:struct-copy memory mem [s-meta (stack-meta bases+ top)]))

