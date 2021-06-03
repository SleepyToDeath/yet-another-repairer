#lang rosette/safe

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match   ; provides `match`
		 rosette/query/core)

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

	(display (~a "total locations: " (length soft) "\n"))

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
;	(define hard (andmap identity (map (lambda (io) (encoder (car io) (cdr io) funcs)) spec)))
	(define hard (andmap identity (map (lambda (io) (encoder (car io) (cdr io) funcs)) (list (car spec)))))
	(display (~a "Number of asserts: " (length (asserts)) "\n"))
;	(define max-sat-sum (apply + (map (lambda (l) (if l 1 0)) max-sat-list)))
;	(display (~a "Number of asserts: " (length (asserts)) "\n"))
;	(define debug-max-sat (> max-sat-sum (- (length max-sat-list) 7)))
;	(display (~a "Number of asserts: " (length (asserts)) "\n"))

	(display "\n Solving: \n")
	(display (~a "!!!!!!!!!!!!!!!#n Asserts: " (length (asserts)) "\n"))
	(pretty-print (asserts))
	(output-smt #t)

;	/* default version one bug */
	(define debug-sol (solve (assert (and (andmap+ identity hard) one-bug) "TTTTTTTEST!!!!!!!!!!!!!!")))

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
	(pretty-print debug-sol)

	(if (unsat? debug-sol)
		(begin
;		(match (core (∃-debug (append hard (list one-bug) (asserts)) #:muc #t)) [l (map print-fml l)])
		#f)

		(begin
;		(DEBUG-DO (display (~a (evaluate max-sat-sum debug-sol) "/" (length max-sat-list) "\n")))
;		(DEBUG-DO ((lambda () (print-pending-eval debug-sol) (display "\n"))))
		((lambda () (print-pending-eval debug-sol) (display "\n")))

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

			[_ (try-fixing ast spec bugl)]))

		(if maybe-l maybe-l (localize-bug-in-funcs ast mac locations encoder spec funcs)))))

(define meta-counter 0)
(define first-counter 0)
(define second-counter 0)

(define (try-fixing ast spec bugl)


	(++ meta-counter)
	(display (~a "============ Tried " meta-counter " iterations =============\n"))

	(define mac (ast->machine ast))

	(define ctxt (location->ctxt ast bugl mac))
	(display "============ context collected =============:\n")
	(pretty-print ctxt)

(if (not (and
		(equal? (location-line bugl) 3)
		(equal? (function-name (location-func bugl)) (string-id "setServerID")))) #f
	(begin

	(define (search-first)
		(define verifier
			(lambda (stat-sketch)

				(display "------- enum first --------\n")
				(++ first-counter)
				(pretty-print first-counter)
				(pretty-print second-counter)

				(define prog-sketch (replace-stat ast stat-sketch bugl))
				(if (using-bridge-var? stat-sketch)
					(search-second prog-sketch)
					(monitor-reason "spec" (program-sketch->constraint prog-sketch spec)))))
		(define updater
			(lambda (ctxt ast) (real-context-updater ctxt ast mac)))
		(define pruner
			(lambda (ast) (monitor-reason "pruner" (real-pruner ast mac))))
		(ast-dfs patch-line-1 ctxt verifier pruner updater search-depth))

	(define (search-second ast)
		(define verifier
			(lambda (invoke-sketch)

				(display "------- enum second --------\n")
				(++ second-counter)
				(pretty-print first-counter)
				(pretty-print second-counter)

				(pretty-print invoke-sketch)
				(define prog-sketch (define-bridge-var (insert-stat ast invoke-sketch bugl) invoke-sketch (get-invoke-ret-type invoke-sketch mac) bugl))
				(monitor-reason "spec" (program-sketch->constraint prog-sketch spec))))
		(define updater
			(lambda (ctxt ast) (real-context-updater ctxt ast mac)))
		(define pruner
			(monitor-reason "pruner" (lambda (ast) (real-pruner ast mac))))
		(ast-dfs patch-line-2 ctxt verifier pruner updater inf-depth))

	(search-first))))


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


(define patch-line-1 (stat (stat-jmp 
	(expr (expr-binary 
		(expr (expr-var (variable bridge-var-name))) 
		(op op-neq) 
		(expr (expr-const (const 1)))))
	(label "label1"))))

(define patch-line-2 (stat (stat-virtual-call 
	(variable bridge-var-name) 
	(variable (string-id "r0")) 
	(type-name (string-id "org.projectfloodlight.openflow.types.IPv4Address"))
	(func-name (string-id "equals"))
	(types (type-list (list (type-name (string-id "java.lang.Object")))))
	(arguments-caller (argument-caller-list (list (dexpr (expr-var (variable (string-id "$r1" ))))))))))

