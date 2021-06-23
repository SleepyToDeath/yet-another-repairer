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
(define (localize-bug ast spec-bad spec-good)

	(match-define (list mac soft hard) (ast->relation ast))

	(define funcs-init (cons
		(sfunc-id class-name-main func-name-main (map cdr (function-args func-main)))
		(map (lambda (cls-cl f-cl)
			(sfunc-id cls-cl func-name-clinit (map cdr (function-args f-cl))))
			class-names-clinit
			funcs-clinit)))

	(display (~a "total locations: " (length soft) "\n"))

	(define ret (localize-bug-in-funcs ast mac soft hard spec-bad spec-good funcs-init))
	(pretty-print string-id-table)
	ret)

;selectors: list of 
;funcs: list of sid
(define (localize-bug-in-funcs ast mac locations encoder spec-bad spec-good funcs)
	(display "\n Encoding: \n")
	(display (~a "target funcs: " funcs "\n"))
	(display "visited: \n") 
	(map print-location visited-locations)
	(clear-asserts!)
	(clear-pending-eval)
	(clear-specs!)
	(reset-contains-target-cache)
	(assert-visited-locations)
	(do-all-resets!)

;	[TODO] solve all examples, add jmp condition based on correct/incorrect examples

	(set-spec-id! spec-id-bad)
	(define hard-bad (apply append (map (lambda (io) (encoder (car io) (cdr io) spec-id-bad funcs)) spec-bad)))
	(set-spec-id! spec-id-good)
	(define hard-good (apply append (map (lambda (io) (encoder (car io) (cdr io) spec-id-good funcs)) spec-good)))

;	(pretty-print (asserts))
	
	(finalize-selectors!)

;	(pretty-print valid-selectors)

	(define sum (apply + (map (lambda (id) (if id 1 0)) valid-selectors)))
	(define one-bug (equal? sum (- (length valid-selectors) 1)))
	(define no-bug (equal? sum (length valid-selectors)))

	(display "\n Solving: \n")
;	/* default version one bug */
	(define sol-bad (solve (assert (and (andmap+ identity hard-bad) one-bug))))
	(define sol-good (solve (assert (and (andmap+ identity hard-good) no-bug))))

;	/* no bug */
;	(define sol-bad (solve (assert (and hard no-bug))))

;	/* maximize satisfiable lines */
;	(define sol-bad (optimize #:maximize (list sum)
;			  #:guarantee (assert (and hard))))

;	/* ??? */
;	(define sol-bad (solve (assert (and hard debug-max-sat))))

;	/* maximize satisfiable clauses, more detailed than lines, for debugging this tool itself only */
;	(define sol-bad (optimize #:maximize (list max-sat-sum)
;			  #:guarantee (assert (and no-bug hard))))
	
	(display "\n Model: \n")
;	(pretty-print sol-bad)

	(if (or (unsat? sol-bad) (unsat? sol-good))
		(begin
;		(match (core (∃-debug (append hard (list one-bug) (asserts)) #:muc #t)) [l (map print-fml l)])
		(if (unsat? sol-bad) (display "bad unsat!\n") #f)
		(if (unsat? sol-good) (display "good unsat!\n") #f)
		#f)

		(begin
		(define bugl (ormap (lambda (l) 
			(define id (location-selector l))
			(if	(and 
					(member (~a id) (map (lambda (b) (~a b)) valid-selectors))
					(not (evaluate id sol-bad)))
				l
				#f))
			locations))
		(display "\n ++++++++++++++++++++ Bug Location: ++++++++++++++++++++++\n")
		(print-location bugl)
		(add-visited-location bugl)
;		(eval-specs! sol-bad)
		
		(DEBUG-DO (pretty-print string-id-table))
		(DEBUG-DO (std:error "Halt!"))

		(pretty-print string-id-table)

;		(display "\nbad deferred values:\n")
;		(print-pending-eval spec-id-bad sol-bad)
;		(display "good deferred values:\n")
;		(print-pending-eval spec-id-good sol-good)
;		(std:error "Halt!")

		(define maybe-l (match (location-inst bugl)
			[(inst-static-call ret cls-name func-name arg-types args) 
				(localize-bug-in-funcs ast mac locations encoder spec-bad spec-good
					(list (sfunc-id cls-name func-name arg-types)))]

			[(inst-special-call ret obj-name cls-name func-name arg-types args)
				(localize-bug-in-funcs ast mac locations encoder spec-bad spec-good
					(list (sfunc-id cls-name func-name arg-types)))]

			[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
				(letrec
					([vid (vfunc-id-alt mac cls-name func-name arg-types)]
					 [vfuncs (filter (lambda (f) (equal? (function-formula-vid f) vid)) (all-vfunctions mac))])
					(localize-bug-in-funcs ast mac locations encoder spec-bad spec-good
						(map function-formula-sid vfuncs)))]

			[_ (try-fixing ast spec-bad spec-good bugl sol-bad sol-good)]))

		(if maybe-l maybe-l (localize-bug-in-funcs ast mac locations encoder spec-bad spec-good funcs)))))

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
		

(define (try-fixing ast spec-bad spec-good bugl sol-bad sol-good)

	(clear-asserts!)
	(++ meta-counter)
	(reset-candidates!)
	(display (~a "============ Tried " meta-counter " iterations =============\n"))
	(eprintf (~a "============ Tried " meta-counter " iterations =============\n"))

	(print-location bugl)
	(eprint-location bugl)

	(if #f;(equal? (function-name (location-func bugl)) func-name-main)
		(begin (display "In main function. Skipped.\n") #f)
		(begin

		(define mac.tmp (build-virtual-table (ast->machine ast)))
		(define func-new (find-new-func mac.tmp (location-class bugl) (location-func bugl)))
		(define mac (std:struct-copy machine mac.tmp [fc func-new]))

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
					(pretty-print l)
					(define prog-sketch (replace-stat ast l bugl))
					(define t0 (std:current-inexact-milliseconds))
					(monitor-reason "spec" 
						(if (not (monitor-reason "pre check" (pre-check prog-sketch))) #f
							(begin
							(define t1 (std:current-inexact-milliseconds))
							(display (~a "pre check took " (- t1 t0) " milliseconds\n"))
							(define ret (sat-spec? (location-selector bugl) mac (car (ast->instruction l #f #f)) sol-bad sol-good))
							(define t2 (std:current-inexact-milliseconds))
							(display (~a "spec check took " (- t2 t1) " milliseconds\n"))
							ret))))

;					(program-sketch->constraint prog-sketch spec l)))
				one-line-candidates)

			(ormap (lambda (l2)
				(define spec-step1 #f)
				(ormap (lambda (l1)
					(++ second-counter)
					(display "\nChecking candidate: \n")
					(pretty-print l2)
					(pretty-print l1)
					(display (~a "Checked " second-counter " patches\n"))
					(eprintf (~a "Checked " second-counter " patches\n"))
					(define t0 (std:current-inexact-milliseconds))
					(define prog-sketch (replace-stat ast l1 bugl))
					(define prog-sketch2 (define-bridge-var 
						(insert-stat prog-sketch l2 bugl) 
						l2
						(get-invoke-ret-type l2 mac) bugl))
					(if (not (monitor-reason "pre check" (pre-check prog-sketch2))) #f
						(std:with-handlers ([std:exn:fail? (lambda (x) 
								(display "Execption!\n") 
								(clear-asserts!) 
								#f)])
						(begin
						;renew machine
						(define mac.tmp (build-virtual-table (ast->machine prog-sketch2)))
						(define func-new (find-new-func mac.tmp (location-class bugl) (location-func bugl)))
						(define mac (std:struct-copy machine mac.tmp [fc func-new]))
						;timer
						(define t1 (std:current-inexact-milliseconds))
						(display (~a "pre check took " (- t1 t0) " milliseconds\n"))
						;spec check
						(set! spec-step1 (if spec-step1 spec-step1 
							(step-spec-0 (location-selector bugl) mac (car (ast->instruction l2 #f #f)) sol-bad sol-good)))
						(define ret (monitor-reason "spec" (sat-spec-continue? spec-step1 mac (car (ast->instruction l1 #f #f)) sol-bad sol-good)))
						;timer
						(define t2 (std:current-inexact-milliseconds))
						(display (~a "spec check took " (- t2 t1) " milliseconds\n"))
						;return
						ret))))
					first-line-candidates))
				second-line-candidates))

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

