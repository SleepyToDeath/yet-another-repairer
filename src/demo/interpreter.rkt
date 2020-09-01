#lang rosette

;(require (for-syntax racket/match))

(require "instruction.rkt")

(define-syntax ass-const
	(syntax-rules ()
		[(ass-const var-l = const) (define var-l const)]))

(define-syntax ass-var
	(syntax-rules ()
		[(_ var-l = var-r) (define var-l var-r)]))

(define-syntax ass-op 
	(syntax-rules ()
		[(_ var-l = var-r1 op var-r2) (define var-l (op var-r1 var-r2))]))

(define-syntax jimple
	(syntax-rules ()
		[(_ statement ...) (let ([statement] ...) z)]))


;(define-syntax (ass-const stx)
;	(syntax-case stx ()
;		[ (_ var-l = const) 
;			#'(inst-ass-const var-l const) ]))

;(define-syntax (ass-var stx)
;	(syntax-case stx ()
;		[ (_ var-l = var-r) 
;			#'(inst-ass-var var-l var-r) ]))
;
;(define-syntax (ass-op stx)
;	(syntax-case stx ()
;		[ (_ var-l = var-r1 op var-r2) 
;			#'(inst-ass-op var-l var-r1 op var-r2) ]))
;
(define-syntax (nop stx)
	(syntax-case stx ()
		[ (_) 
			#'inst-nop ]))

(define-syntax (halt stx)
	(syntax-case stx ()
		[ (_) 
			#'inst-halt ]))

(provide jimple ass-const ass-var ass-op halt nop)
