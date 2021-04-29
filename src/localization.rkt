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

	(display "asserts: \n") 
;	(pretty-print (asserts))

	(display "\n Solving: \n")
	(display (~a "!!!!!!!!!!!!!!!#n Asserts: " (length (asserts)) "\n"))
;	(pretty-print (asserts))
;	(pretty-print hard)
;	(check-asserts 0)
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
	(display "+++++++++++++ Trying to Fix +++++++++++++++++\n") 
	(define ctxt (location->ctxt ast bugl))
	(pretty-print ctxt)
	(display "+++++++++++++ Context Collected +++++++++++++++++\n") 
	(display "+++++++++++++ Start Enumerating +++++++++++++++++\n") 
	
	(define enum-counter 0)
	(define type-check-counter 0)

	(define (stat-sketch->constraint stat-sketch)
		(display (~a enum-counter " programs explored\n"))
		(display  "enumerated statement: " )
		(pretty-print stat-sketch)
		(set! enum-counter (+ 1 enum-counter))
		(define prog-sketch (location->sketch ast stat-sketch bugl))
		(define mac-sketch (ast->machine prog-sketch))
		(if (not (machine-type-check? (build-virtual-table mac-sketch))) #f
			(begin
			(set! type-check-counter (+ 1 type-check-counter))
			(display (~a type-check-counter " programs type-checks\n"))
			(define (spec->fml io)
				(match-define (cons input output) io)
				(display "+++++ Filling Input +++++\n") 
				(define mac-in (assign-input mac-sketch input))
				(display "+++++ Computing +++++\n") 
				(define mac-fin (compute mac-in))
				(display "+++++ Comparing Output +++++\n") 
				(compare-output mac-fin output))
			(define constraint (andmap+ spec->fml spec))
			(if constraint
				(begin
				(display "+++++++++++++ Fixed program: +++++++++++++++++\n") 
				(pretty-print prog-sketch)) #f)
			constraint)))

	(if (ast-dfs (stat #f) ctxt stat-sketch->constraint search-depth) #t
		(begin
		(display "+++++++++++++ Synthesis Failed +++++++++++++++++\n") 
		#f)))
	
;	(display "+++++++++++++ Synthesizing +++++++++++++++++\n") 
;	(define start-time (std:current-inexact-milliseconds))
;	(clear-asserts!)
;	(define syn-sol 
;		(synthesize
;			#:forall null
;			#:guarantee (assert constraint)))

;	(define finish-time (std:current-inexact-milliseconds))
;	(display (format "Synthesis took ~a milliseconds. Search depth: ~a\n" (- finish-time start-time) search-depth))

;	(if (unsat? syn-sol) 
;		(begin
;		(display "+++++++++++++ Synthesis Failed +++++++++++++++++\n") 
;		#f)
;
;		(begin
;		(display "+++++++++++++ Fixed program: +++++++++++++++++\n") 
;		(pretty-print (evaluate sketch syn-sol))
;		bugl)))

(define search-depth 3)

(define (location->sketch ast stat-sketch bugl)
	;extract parameters
	(define func (location-func bugl))
	(define cls (location-class bugl))
	(define cname (class-name cls))
	(define fname (function-name func))
	(define line (if (equal? fname func-name-init) (- (location-line bugl) 1) (location-line bugl)))

	;shorthands
	(define (is-target-cls? cls-ast)
		(equal? cname (string-id (type-name-name (class-default-name (class-def-rhs cls-ast))))))

	(define (is-target-func? func-ast)
;		#f)
		(equal? fname (string-id (func-name-name (function-content-name (function-declare-rhs func-ast))))))

	(define (list-replace l pred e)
		(map (lambda (e__) (if (pred e__) e e__)) l))

	;find class in AST
	(define clss-ast (class-list-cl (program-rhs ast)))
	(define cls-ast (findf is-target-cls? clss-ast))
	(define vfuncs-ast (function-list-fl (function-declares-rhs (class-default-virtual-functions (class-def-rhs cls-ast)))))
	(define sfuncs-ast (function-list-fl (function-declares-rhs (class-default-static-functions (class-def-rhs cls-ast)))))

	;find function in AST
	(define (maybe-replace-func funcs-ast)
		(define maybe-func-ast (findf is-target-func? funcs-ast))
		(if (not maybe-func-ast) funcs-ast
			;find and replace instruction in AST
			(begin
			(define func-ast maybe-func-ast)
			(define insts-ast (stat-list-sl (stats-rhs (function-content-statements (function-declare-rhs func-ast)))))
			(define inst-ast (list-ref insts-ast line))
;			(define inst-ast-sketch
;				(stat (stat-ass (stat-ass-lvalue (stat-rhs inst-ast)) (expr-enum ctxt search-depth))))
;			(define insts-ast-sketch (stats (stat-list (std:list-set insts-ast line inst-ast-sketch))))
;			(define insts-ast-sketch (stats (stat-list (std:list-set insts-ast line (stat-enum ctxt search-depth)))))
			(define insts-ast-sketch (stats (stat-list (std:list-set insts-ast line stat-sketch))))
;			(display (~a "replacing line " line ":\n"))
;			(pretty-print insts-ast-sketch)
			(define func-ast-sketch (function-declare (std:struct-copy function-content (function-declare-rhs func-ast) [statements insts-ast-sketch])))
;			(pretty-print func-ast-sketch)

			;repack function
			(list-replace funcs-ast is-target-func? func-ast-sketch))))

	;repack class
	(define cls-ast-sketch (class-def (std:struct-copy class-default (class-def-rhs cls-ast) 
		[virtual-functions (function-declares (function-list (maybe-replace-func vfuncs-ast)))]
		[static-functions (function-declares (function-list (maybe-replace-func sfuncs-ast)))])))
	(define clss-ast-sketch (list-replace clss-ast is-target-cls? cls-ast-sketch))

	;repack program
	(program (class-list clss-ast-sketch)))

			
		

;(struct syntax-context (vars types fields funcs consts ops labels) #:transparent)
;(struct location (class func line inst selector) #:transparent)
(define (location->ctxt ast bugl)
	(define func (location-func bugl))
	(define cls (location-class bugl))
	(define cname (class-name cls))

	(define vars (map car (function-locals func)))
	(define types null)
	(define fields (map (lambda (fname) (cons cname fname)) 
		(map car (append (class-vfields cls) (class-sfields cls)))))
	(define funcs null)
	(define consts (list 0 1 2 3))
	(define ops (list bvand bvor bvxor op-mod op-cmp equal? op-neq op-gt op-ge op-lt op-le bvlshr op-add op-sub op-mul op-div))
	(define labels null)
	(syntax-context vars types fields funcs consts ops labels))
		

(define (print-location l)
	(pretty-print
		(match l
			[(location cls func line inst selector)
				(location (class-name cls) (function-name func) line inst selector)])))

