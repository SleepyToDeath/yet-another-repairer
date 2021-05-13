#lang rosette/safe

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
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
(require "semantics-common.rkt")
(require "formula.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "jimple/operators.rkt")
(require "enumerator.rkt")

(provide (all-defined-out))

(define visited-locations null)
(define (add-visited-location l)
	(set! visited-locations (cons l visited-locations)))
(define (assert-visited-locations)
	(map (lambda (l) (assert (location-selector l))) visited-locations))
	

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

	(define ret (localize-bug-in-funcs ast mac soft hard spec funcs-init))
	(pretty-print string-id-table)
	ret)

;selectors: list of 
;funcs: list of sid
(define (localize-bug-in-funcs ast mac locations encoder spec funcs)
	(display "\n Encoding: \n")
	(display (~a "target funcs: " funcs "\n"))
	(display "visited: \n") 
	(map print-location visited-locations)
	(clear-asserts!)
	(clear-pending-eval)
	(reset-contains-target-cache)
	(assert-visited-locations)

	(define sum (apply + (map (lambda (l) (if (location-selector l) 1 0)) locations)))
	(define one-bug (equal? sum (- (length locations) 1)))
	(define no-bug (equal? sum (length locations)))
	(define hard (andmap identity (map (lambda (io) (encoder (car io) (cdr io) funcs)) spec)))
	(define max-sat-sum (apply + (map (lambda (l) (if l 1 0)) max-sat-list)))
	(define debug-max-sat (> max-sat-sum (- (length max-sat-list) 7)))

	(display "\n Solving: \n")
	(display (~a "!!!!!!!!!!!!!!!#n Asserts: " (length (asserts)) "\n"))
	(output-smt #t)

;	/* default version one bug */
	(define debug-sol (solve (assert (and hard one-bug))))

;	/* no bug */
;	(define debug-sol (solve (assert (and hard no-bug))))

;	/* maximize satisfiable lines */
;	(define debug-sol (optimize #:maximize (list sum)
;			  #:guarantee (assert (and hard))))

;	/* ??? */
;	(define debug-sol (solve (assert (and hard debug-max-sat))))

;	/* maximize satisfiable clauses, more detailed than lines, for debugging this tool itself only */
;	(define debug-sol (optimize #:maximize (list max-sat-sum)
;			  #:guarantee (assert (and no-bug hard))))
	
	(display "\n Model: \n")
;	(pretty-print debug-sol)

	(if (unsat? debug-sol)
		#f
		(begin
		(DEBUG-DO (display (~a (evaluate max-sat-sum debug-sol) "/" (length max-sat-list) "\n")))
		(DEBUG-DO ((lambda () (print-pending-eval debug-sol) (display "\n"))))

		(define bugl (ormap (lambda (l) (if (evaluate (location-selector l) debug-sol) #f l)) locations))
		(display "\n ++++++++++++++++++++ Bug Location: ++++++++++++++++++++++\n")
		(print-location bugl)
		(add-visited-location bugl)
;		(map print-location locations)
		
		(DEBUG-DO (pretty-print string-id-table))
		(DEBUG-DO (std:error "Halt!"))

		(pretty-print string-id-table)
	;	(std:error "Halt!")

		(define maybe-l (match (location-inst bugl)
			[(inst-static-call ret cls-name func-name arg-types args) 
				(localize-bug-in-funcs ast mac locations encoder spec 
					(list (sfunc-id cls-name func-name arg-types)))]

			[(inst-special-call ret obj-name cls-name func-name arg-types args)
				(localize-bug-in-funcs ast mac locations encoder spec 
					(list (sfunc-id cls-name func-name arg-types)))]

			[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
				(letrec
					([vid (vfunc-id-alt mac cls-name func-name arg-types)]
					 [vfuncs (filter (lambda (f) (equal? (function-formula-vid f) vid)) (all-vfunctions mac))])
					(localize-bug-in-funcs ast mac locations encoder spec 
						(map function-formula-sid vfuncs)))]

			[_ (try-fixing ast mac spec bugl)]))

		(if maybe-l maybe-l (localize-bug-in-funcs ast mac locations encoder spec funcs)))))


(define (try-fixing ast mac spec bugl)

	(define ctxt (location->ctxt ast bugl mac))

	(define (search-first)
		(display "------- enum first --------\n")
		(define verifier
			(lambda (stat-sketch)
				(define prog-sketch (replace-stat ast stat-sketch bugl))
				(if (using-bridge-var? stat-sketch)
					(search-second prog-sketch)
					(program-sketch->constraint prog-sketch spec))))
		(define updater
			(lambda (ctxt ast) (real-context-updater ctxt ast mac)))
		(define pruner
			(lambda (ast) (real-pruner ast mac)))
		(ast-dfs (stat #f) ctxt verifier pruner updater search-depth))

	(define (search-second ast)
		(display "------- enum second --------\n")
		(define verifier
			(lambda (invoke-sketch)
				(define prog-sketch (define-bridge-var (insert-stat ast invoke-sketch bugl) invoke-sketch (get-invoke-ret-type invoke-sketch mac) bugl))
				(program-sketch->constraint prog-sketch spec)))
		(define updater
			(lambda (ctxt ast) (real-context-updater ctxt ast mac)))
		(define pruner
			(lambda (ast) (real-pruner ast mac)))
		(ast-dfs (stat-calls #f) ctxt verifier pruner updater inf-depth))

	(search-first))


(define (program-sketch->constraint prog-sketch spec)
	(define mac-sketch (ast->machine prog-sketch))
	(if (not (machine-all-check? (build-virtual-table mac-sketch))) #f
		(begin
		(define (spec->fml io)
			(match-define (cons input output) io)
			(define mac-in (assign-input mac-sketch input))
			(define mac-fin (compute mac-in))
			(compare-output mac-fin output))
		(define constraint (andmap+ spec->fml spec))
		(if constraint
			(begin
			(display "+++++++++++++ Fixed program: +++++++++++++++++\n") 
			(pretty-print prog-sketch)) #f)
		constraint)))


