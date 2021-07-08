#lang rosette/safe

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match   ; provides `match`
		 rosette/query/core)

(require "domain-config.rkt")
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
(require "format.rkt")

(provide (all-defined-out))

(define visited-locations null)
(define (add-visited-location l)
	(set! visited-locations (cons l visited-locations)))
(define (assert-visited-locations)
	(map (lambda (l) (assert (location-selector l))) visited-locations))
	
(define total-loc-time 0)
(define (more-loc-time! t)
	(set! total-loc-time (+ t total-loc-time)))

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
;	(set! funcs-init (list 271))

	(display (~a "total locations: " (length soft) "\n"))

	(define ret (localize-bug-in-funcs ast mac soft hard spec-bad spec-good funcs-init))
	(pretty-print string-id-table)
	(display (~a "Total localization time: " total-loc-time "\n"))
	ret)

;selectors: list of 
;funcs: list of sid
(define (localize-bug-in-funcs ast mac locations encoder spec-bad spec-good funcs)
	(display2 "\n Encoding: \n")
	(display (~a "target funcs: " funcs "\n"))
	(display "visited: \n") 
	(map print-location visited-locations)
	(pretty-print global-resets)
	(do-all-resets! #t)

	(define (solve-localize spec-id spec bug-num)
		(pretty-print task-resets)
		(do-all-resets! #f)
		(assert-visited-locations)

		(timer-on)
		(set-spec-id! spec-id)
		(define hard (apply append (map (lambda (io) (encoder (car io) (cdr io) spec-id funcs)) spec)))
		(define t1 (timer-check))
		(display2 (~a "\n Encoded. Took " t1 " ms \n"))
		(more-loc-time! t1)

		(finalize-selectors!)

		(define sum (apply + (map (lambda (id) (if id 1 0)) valid-selectors)))
		(define bug-sum (equal? sum (- (length valid-selectors) bug-num)))

		(display2 "\n Solving: \n")
		(timer-on)
;		(match (core (∃-debug (append hard (list bug-sum) (asserts)) #:muc #t)) [l (map print-fml l)])
		(define sol (solve (assert (and (andmap+ identity hard) bug-sum))))
;		(define sol (optimize 	#:maximize (list (count-truth hard))
;								#:guarantee (assert bug-sum)))
		(define t2 (timer-check))
		(display2 (~a "\n Solved. Took " t2 " ms \n"))
		(more-loc-time! t2)

		sol)

;	(define sol-good (solve (assert #t)))
;	(define sol-bad (solve (assert #t)))

	(define sol-bad (solve-localize spec-id-bad spec-bad 1))
	(define bad-selectors valid-selectors)
	(define sol-good
		(if (unsat? sol-bad)
			(begin ;skip if bad is already unsat
				(do-all-resets! #f)
				(solve (assert #t)))
			(solve-localize spec-id-good spec-good 0)))

;	(display "\n Model: \n")
;	(pretty-print sol-good)

	(if (or (unsat? sol-bad) (unsat? sol-good))
		(begin
;		(match (core (∃-debug (append hard (list one-bug) (asserts)) #:muc #t)) [l (map print-fml l)])
		(if (unsat? sol-bad) (display2 "bad unsat!\n") #f)
		(if (unsat? sol-good) (display2 "good unsat!\n") #f)
		#f)

		(begin
		(define bugl (ormap (lambda (l) 
			(define id (location-selector l))
			(if	(and 
					(member (~a id) (map (lambda (b) (~a b)) bad-selectors))
					(not (evaluate id sol-bad)))
				l
				#f))
			locations))

		(pretty-print string-id-table)
		(eprintf (pretty-format string-id-table))

;		(display "\nbad deferred values:\n")
;		(print-pending-eval spec-id-bad sol-bad)
;		(display "good deferred values:\n")
;		(print-pending-eval spec-id-good sol-good)

		(display2 "\n ++++++++++++++++++++ Bug Location: ++++++++++++++++++++++\n")
		(print-location bugl)
		(eprint-location bugl)
		(add-visited-location bugl)



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

(define (add-candidate2.2 mac bugl candi)
	(if (not (member (class-name (ast->cls candi mac)) synthesis-usable-classes)) #f
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

		(machine-prepare-recursion mac)

		(or 
			(ormap (lambda (l) 
					(++ second-counter)
					(display "\nChecking candidate: \n")
					(pretty-print (ast-restore-strings l))
					(display (~a "Checked " second-counter " patches\n"))
					(eprintf (~a "Checked " second-counter " patches\n"))
					(timer-on)
					(define ret.maybe (monitor-reason "spec" 
						(sat-spec? (location-selector bugl) mac (car (ast->instruction l #f #f)) sol-bad sol-good)))
					(display (~a "spec check took " (timer-reset) " milliseconds\n"))
					(define ret (if (not ret.maybe) #f
						(begin
						(define prog-sketch (replace-stat ast l bugl))
						(define ret.final (program-sketch->constraint prog-sketch (append spec-bad spec-good)))
						(display (~a "final confirm took " (timer-reset) " milliseconds\n"))
						ret.final)))
					ret)
				one-line-candidates)

			(ormap (lambda (l2)
				(define spec-step1 #f)
				(define spec-step1-attempted #f)
				(define bridge-var-type (get-invoke-ret-type l2 mac))
				(if (contains-target-quick? (ast->sid l2) (location->sid bugl)) #f
					(ormap (lambda (l1)
						(++ second-counter)
						(display "\nChecking candidate: \n")
						(pretty-print (ast-restore-strings l2))
						(pretty-print (ast-restore-strings l1))
						(display2 (~a "Checked " second-counter " patches\n"))
						(timer-on)

	;					(define prog-sketch (replace-stat ast l1 bugl))
	;					(define prog-sketch2 (define-bridge-var 
	;						(insert-stat prog-sketch l2 bugl) 
	;						l2
	;						(get-invoke-ret-type l2 mac) bugl))
						(define (pre-check)
							(inject-type! (list (cons bridge-var-name bridge-var-type)))
							(inst-type-check? mac (location-class bugl) (location-func bugl) (car (ast->instruction l1 #f #f))))

						(if (or (and spec-step1-attempted (not spec-step1))
								(not (monitor-reason "pre check" (pre-check)))) #f
							(std:with-handlers ([std:exn:fail? (lambda (x) 
									(pretty-print x)
									(display "Execption!\n") 
									(clear-asserts!) 
									#f)])
							(begin
							;renew machine
	;						(define mac.tmp (build-virtual-table (ast->machine prog-sketch2)))
	;						(define func-new (find-new-func mac.tmp (location-class bugl) (location-func bugl)))
	;						(define mac (std:struct-copy machine mac.tmp [fc func-new]))
							;timer
							(display (~a "pre check took " (timer-reset) " milliseconds\n"))
							;spec check
							(set! spec-step1 (if spec-step1 spec-step1 
								(begin
								(display "~~ first step ~~\n")
								(set! spec-step1-attempted #t)
								(step-spec-0 (location-selector bugl) mac (car (ast->instruction l2 #f #f)) sol-bad sol-good))))
							(display "~~ second step ~~\n")
							(define ret.maybe (monitor-reason "spec" (sat-spec-continue? spec-step1 mac (car (ast->instruction l1 #f #f)) sol-bad sol-good)))
							;timer
							(display (~a "spec check took " (timer-reset) " milliseconds\n"))
							;final confirm
							(define ret (if (not ret.maybe) #f
								(begin
								(define prog-sketch (replace-stat ast l1 bugl))
								(define prog-sketch2 (define-bridge-var 
									(insert-stat prog-sketch l2 bugl) 
									l2
									(get-invoke-ret-type l2 mac) bugl))
								(define ret.final (program-sketch->constraint prog-sketch2 (append spec-bad spec-good)))
								(display (~a "final confirm took " (timer-reset) " milliseconds\n"))
								ret.final)))
							;return
							ret))))
						first-line-candidates)))
				second-line-candidates))

)))



(define (program-sketch->constraint prog-sketch spec)
	(std:with-handlers ([std:exn:fail? (lambda (x) 
			(display "Execption!\n") 
			(clear-asserts!) 
			#f)])
		(begin
		(define mac-sketch (ast->machine prog-sketch))
		(define (spec->fml io)
			(match-define (cons input output) io)
			(define mac-in (assign-input mac-sketch input))
			(define mac-fin (compute mac-in))
			(do-n-ret pretty-print (compare-output mac-fin output)))
		(define constraint (andmap+ spec->fml spec))
		(pretty-print constraint)
		constraint)))


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

(define patch-line-6.2 
(stat-calls
 (stat-virtual-call
  (variable 15)
  (variable 94)
  (type-name 2)
  (func-name 78)
  (types (type-list null))
  (arguments-caller (argument-caller-list null)))))

(define patch-line-6.1
(stat
 (stat-ass
  (lexpr (expr-var (variable 65)))
  (expr
   (expr-binary
    (expr (expr-var (variable 63)))
    (op op-sub)
    (expr (expr-var (variable 15))))))))


;(define spec-counter 0)

;(define (pre-check prog-sketch)
;	(std:with-handlers ([std:exn:fail? (lambda (x) 
;			(display "Execption!\n") 
;			(clear-asserts!) 
;			#f)])
;		(begin
;		(define mac-sketch (ast->machine prog-sketch))
;		(machine-all-check? (build-virtual-table mac-sketch)))))


