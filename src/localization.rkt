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
(require "map.rkt")
(require "memory.rkt")
(require "memory-common.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "jimple/operators.rkt")
(require "enumerator.rkt")
(require "multitask.rkt")

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
	(clear-specs!)
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
;	(pretty-print (asserts))
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
;	(pretty-print debug-sol)

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
		(eval-specs! debug-sol)
		
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

(define one-line-candidates null)
(define first-line-candidates null)
(define second-line-candidates null)

(define (reset-candidates!)
	(set! one-line-candidates null)
	(set! first-line-candidates null)
	(set! second-line-candidates null))

(define (add-candidate1 mac bugl candi)
	(define inst (car (ast->instruction candi (imap-empty default-type) 0)))
	(if (not (inst-type-check? mac (location-class bugl) (location-func bugl) inst)) #f
		(set! one-line-candidates (cons candi one-line-candidates))))

(define (add-candidate2.1 mac bugl candi)
	(define inst (car (ast->instruction candi (imap-empty default-type) 0)))
	(if (not (inst-type-check? mac (location-class bugl) (location-func bugl) inst)) #f
		(set! first-line-candidates (cons candi first-line-candidates))))

(define usable-classes (list
	(string-id "org.projectfloodlight.openflow.types.IPv4Address")
	(string-id "org.projectfloodlight.openflow.types.MacAddress")
))

(define (add-candidate2.2 mac bugl candi)
	(if (not (member (class-name (ast->cls candi mac)) usable-classes)) #f
		(if (not-a-function-error? (ast->func candi mac)) #f
			(begin
			(define inst (car (ast->instruction candi (imap-empty default-type) 0)))
			(if (not (inst-type-check? mac (location-class bugl) (location-func bugl) inst)) #f
				(set! second-line-candidates (cons candi second-line-candidates)))))))
		

(define (try-fixing ast spec bugl)

	(clear-asserts!)
	(++ meta-counter)
	(reset-candidates!)
	(display (~a "============ Tried " meta-counter " iterations =============\n"))
	(eprintf (~a "============ Tried " meta-counter " iterations =============\n"))

	(print-location bugl)

	(if (equal? (function-name (location-func bugl)) func-name-main)
		(begin (display "In main function. Skipped.\n") #f)
		(begin

		(define mac (build-virtual-table (ast->machine ast)))

		(define ctxt (location->ctxt ast bugl mac))
		(display "============ context collected =============:\n")
		(pretty-print ctxt)

		(define (search-first)
			(define dummy-verifier
				(lambda (stat-sketch)
					(if (using-bridge-var? stat-sketch)
						(add-candidate2.1 mac bugl stat-sketch)
						(add-candidate1 mac bugl stat-sketch))
					#f))
			(define updater
				(lambda (ctxt ast) (real-context-updater ctxt ast mac)))
			(define pruner
				(lambda (ast) (monitor-reason "pruner" (real-pruner ast mac))))
			(ast-dfs sketch-line-1 ctxt dummy-verifier pruner updater search-depth))

		(define (search-second)
			(define dummy-verifier
				(lambda (invoke-sketch)
					(add-candidate2.2 mac bugl invoke-sketch)
					#f))
			(define updater
				(lambda (ctxt ast) (real-context-updater ctxt ast mac)))
			(define pruner
				(monitor-reason "pruner" (lambda (ast) (real-pruner ast mac))))
			(ast-dfs sketch-line-2 ctxt dummy-verifier pruner updater inf-depth))

		(search-first)
		(search-second)
		
		(display (~a "***************** Totally " 
			(length one-line-candidates) " + " 
			(length first-line-candidates) " + " 
			(length second-line-candidates) " candidates *********************\n"))
		(eprintf (~a "***************** Totally " 
			(length one-line-candidates) " + " 
			(length first-line-candidates) " + " 
			(length second-line-candidates) " candidates *********************\n"))


		(or 
			(ormap (lambda (l) 
					(display "\nChecking candidate: \n")
					(define prog-sketch (replace-stat ast l bugl))
					(define t0 (std:current-inexact-milliseconds))
					(monitor-reason "spec" 
						(if (not (monitor-reason "pre check" (pre-check prog-sketch))) #f
							(begin
							(define t1 (std:current-inexact-milliseconds))
							(display (~a "pre check took " (- t1 t0) " milliseconds\n"))
							(define ret (sat-spec? (location-selector bugl) mac l))
							(define t2 (std:current-inexact-milliseconds))
							(display (~a "spec check took " (- t2 t1) " milliseconds\n"))
							ret))))

;					(program-sketch->constraint prog-sketch spec l)))
				one-line-candidates)

			(ormap (lambda (l2)
					(display "\nChecking candidate: \n")
					(define prog-sketch (replace-stat ast (car l2) bugl))
					(define prog-sketch2 (define-bridge-var 
						(insert-stat prog-sketch (cadr l2) bugl) 
						(cadr l2) 
						(get-invoke-ret-type (cadr l2) mac) bugl))
					(monitor-reason "spec" (program-sketch->constraint prog-sketch2 spec l2)))
				(std:cartesian-product first-line-candidates second-line-candidates)))

)))

(define spec-counter 0)

(define (pre-check prog-sketch)
	(std:with-handlers ([std:exn:fail? (lambda (x) 
			(display "Execption!\n") 
			(clear-asserts!) 
			#f)])
		(begin
		(define mac-sketch (ast->machine prog-sketch))
		(machine-all-check? (build-virtual-table mac-sketch)))))

(define (program-sketch->constraint prog-sketch spec patch)
	(define t0 (std:current-inexact-milliseconds))
	(std:with-handlers ([std:exn:fail? (lambda (x) 
			(display "Execption!\n") 
			(clear-asserts!) 
			(define t1 (std:current-inexact-milliseconds))
			(display (~a "took " (- t1 t0) " milliseconds\n"))
			(display (~a "max map size: " max-map-size " \n"))
			#f)])
		(begin
		(define mac-sketch (ast->machine prog-sketch))
		(if (not (machine-all-check? (build-virtual-table mac-sketch))) #f
			(begin
			(define (spec->fml io)
				(match-define (cons input output) io)
				(define mac-in (assign-input mac-sketch input))
				(define mac-fin (compute mac-in))
				(compare-output mac-fin output))
			(display "---checking spec!---\n")
			(++ spec-counter)
			(display (~a "checked spec " spec-counter " times\n"))
			(pretty-print patch)
			(define constraint (andmap+ spec->fml spec))
			(define t1 (std:current-inexact-milliseconds))
			(display (~a "took " (- t1 t0) " milliseconds\n"))
			(display (~a "max map size: " max-map-size " \n"))
			(if constraint
				(begin
				(display "+++++++++++++ Fixed program: +++++++++++++++++\n") 
				(pretty-print prog-sketch)) #f)
			constraint)))))


(define sketch-line-1 (stat #f))

(define sketch-line-2 (stat-calls #f))

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

