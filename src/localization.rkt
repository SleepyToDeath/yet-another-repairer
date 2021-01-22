#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/format)
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require "match-define.rkt")
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")
(require "formula.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))

(provide (all-defined-out))

;spec: list of (input . output)
;ast X spec -> location
(define (localize-bug ast spec)

	(match-define (list mac soft hard) (ast->relation ast))

	(define funcs-init (cons
		(sfunc-id class-name-main func-name-main (map cdr (function-args func-main)))
		(map (lambda (cls-cl f-cl)
			(sfunc-id cls-cl func-name-clinit (map cdr (function-args f-cl))))
			class-names-clinit
			funcs-clinit)))

	(define ret (localize-bug-in-funcs mac soft hard spec funcs-init))
	(pretty-print string-id-table)
	ret)

;selectors: list of 
;funcs: list of sid
(define (localize-bug-in-funcs mac locations encoder spec funcs)
	(display "\n Encoding: \n")
;	(pretty-print (asserts))
	(clear-asserts!)
	(clear-pending-eval)

	(define sum (apply + (map (lambda (l) (if (location-selector l) 1 0)) locations)))
	(define one-bug (equal? sum (- (length locations) 1)))
	(define no-bug (equal? sum (length locations)))
	(define hard (andmap identity (map (lambda (io) (encoder (car io) (cdr io) funcs)) spec)))

	(display "\n Solving: \n")
;	(pretty-print (map fml-to-print (asserts)))
	(display (~a "!!!!!!!!!!!!!!!#n Asserts: " (length (asserts)) "\n"))
	(output-smt #t)
	(define debug-sol (solve (assert (and hard one-bug))))
;	(define debug-sol (optimize #:maximize (list sum)
;			  #:guarantee (assert (and hard))))
	
	(display "\n Model: \n")
	(pretty-print debug-sol)
;	((lambda () (print-pending-eval debug-sol) (display "\n")))

	(define bugl (ormap (lambda (l) (if (evaluate (location-selector l) debug-sol) #f l)) locations))
	(display "\n ++++++++++++++++++++ Bug Location: ++++++++++++++++++++++\n")
	(pretty-print bugl)
	
;	(pretty-print string-id-table)

;	(std:error "Halt!")

	(match (location-inst bugl)
		[(inst-static-call ret cls-name func-name arg-types args) 
			(localize-bug-in-funcs mac locations encoder spec 
				(list (sfunc-id cls-name func-name arg-types)))]

		[(inst-special-call ret obj-name cls-name func-name arg-types args)
			(localize-bug-in-funcs mac locations encoder spec 
				(list (sfunc-id cls-name func-name arg-types)))]

		[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
			(letrec
				([vid (vfunc-id-alt mac cls-name func-name arg-types)]
				 [vfuncs (filter (lambda (f) (equal? (function-formula-vid f) vid)) (all-vfunctions mac))])
				(localize-bug-in-funcs mac locations encoder spec 
					(map function-formula-sid vfuncs)))]

		[_ bugl]))


(define (location->sketch ast location)
	ast)


