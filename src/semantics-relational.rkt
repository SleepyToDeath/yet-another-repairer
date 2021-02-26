#lang rosette/safe

(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require "match-define.rkt")
(require "semantics-computational.rkt")
(require "jimple/jimple-parser.rkt")
(require "memory-common.rkt")
(require "formula.rkt")
(require "model.rkt")
(require "semantics-common.rkt")
(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;============================= Definition ====================================
;class: string
;lids: list of symbolic boolean
(struct function-formula (func lids lstates ret-lstate fmls vid sid class) #:transparent)

;pmark : boolean value deciding whether this line is executed
;mem-out : memory symbol of this line after its execution
;mem-in-list : list of (cons pmark mem) where each pair is a possible predecessor
;				of this line. The mem-out with a true pmark will be chosen to be mem-in.
(struct lstate (pmark mem-in-list mem-out) #:transparent)

;cnd: the entire subtree is considered only if cnd is true
(struct invoke-tree (root cnd subtrees) #:transparent)

;(string X string X int)
(struct location (class func line inst selector) #:transparent)
;============================= Top Level Interface ====================================
;ast ->  line ids(list of sym bool) X (input(list of key & value) -> output(list of key & value) -> relation)
(define (ast->relation ast)
	(define mac-raw (ast->machine ast))
	(define mac (initialize-machine-encoding mac-raw))

	(define soft-cons 
		(apply append 
			(map (lambda (func-fml) 
				(match func-fml
					[(function-formula func lids _ _ _ _ _ class)
						(map 
							(lambda (line inst selector) (location class (function-name func) line inst selector))
							(std:range (length lids)) 
							(function-prog func)
							lids)]))
				(all-functions mac))))

	(define sum (apply + (map (lambda (l) (if (location-selector l) 1 0)) soft-cons)))
	(define no-bug (equal? sum (length soft-cons)))

	(define (hard-cons input output target-sids) 

		(set! memory-id-list null)
		(imap-clear-get-keys!)

		(define (assign-input mac input)
			(define mem0 (machine-mem mac))
			(define mem-push (memory-spush mem0))
			(reset-parameter-names)
			(match-define (cons mem-ass fml-ass)
				(foldl 
					(lambda (v mem+fml) 
						(define-symbolic* vi integer?)
						(define fml (equal? vi v))
						(cons 
							(memory-sforce-write (car mem+fml) (next-parameter-name) vi 0) 
							(and (cdr mem+fml) fml)))
					(cons mem-push #t) input))
			(define mem-ret (memory-sdecl mem-ass var-ret-name))
			(cons (std:struct-copy machine mac [mem mem-ret]) fml-ass))

		(define (compare-output mac output)
			(define mem0 (machine-mem mac))
			(andmap identity (map
				(lambda (kv) (equal? (cdr kv) (memory-sforce-read mem0 (string-id (car kv)) 0)))
				output)))

		(match-define (cons mac-ass0 fml-ass0) (assign-input mac input))

		(define mac-ass (build-virtual-table-alt mac-ass0))

		(define fml-ass (and fml-ass0 (memory-sym-sget-fml (machine-mem mac-ass))))

		(define boot-lstate (prepend-starting-mem-in (alloc-lstate (machine-boot mac-ass)) #t (machine-mem mac-ass)))

		(define fml-cfi (starting-pmark boot-lstate))
;		(assert fml-cfi)
;		(assert no-bug)
		(display "\n ###############################################0 \n")
		(set-context! mac-ass)
		(map (lambda (cls) (pretty-print (cons (class-name cls) (class-vfields cls)))) (machine-classes mac-ass))
		(define all-invokes (invoke->relation boot-lstate mac-ass target-sids #f))
		(display "\n ###############################################1 \n")
		(define mem-done-sym (root-invoke-ret-mem all-invokes #f))
		(define mem-done (memory-sym-reset (memory-sym-new #f) mem-done-sym #f))
		(define mac-done (std:struct-copy machine mac-ass [mem mem-done]))
		(display "\n ###############################################4 \n")
		(define fml-out (compare-output mac-done output))
		(display "\n ###############################################5 \n")
		(define mem-all-done (memory-sym-commit mem-done))

		(define (extract-fml itree) 
			(match itree
				[(invoke-tree root cnd subs)
					(begin
					(display "v---------fml-------------v\n")
					(match root
						[(function-formula func lids _ _ _ _ _ class)
							(pretty-print (list (function-name func) class))])
;					(print-fml (function-formula-fmls root))
					(define ret1 
						(function-formula-fmls root))
					(define ret2
						(andmap+ extract-fml subs))
					(define ret (and ret2 ret1))
					(match root
						[(function-formula func lids _ _ _ _ _ class)
							(pretty-print (list (function-name func) class))])
					(display "^---------fml-------------^\n")
;					(test-assert! ret1)
;					(test-assert! ret2)
;					(test-assert! ret)
					ret)]))

		(define fml-code-1 (memory-sym-get-fml mem-all-done #f))
		(define fml-code-2 (extract-fml all-invokes))

		(display "---------fml0-------------\n")
		(define fml-code (and fml-code-1 fml-code-2))
		(inspect fml-code-1)
;		(test-assert! fml-code-1)
;		(test-assert! fml-code-2)
		(display "\n ###############################################6 \n")
		(define fml-boot-is-correct (andmap identity (function-formula-lids boot-lstate)))
		(display "\n ###############################################7 \n")
		(define fml-code-bind (memory-gen-binding mem-all-done))
		(display "\n ###############################################8 \n")
		(and fml-cfi fml-out fml-ass fml-code fml-code-bind))
;		(and fml-cfi fml-code))
;		(print-fml fml-out)
;		(print-fml fml-code)
;		(and fml-cfi fml-code))

	(list mac soft-cons hard-cons))


;============================= Helper Functions ====================================

;----------------------- Initialization -------------------------
(define (initialize-machine-encoding mac)
	(define mac-tmp (std:struct-copy machine mac [classes
		(map 
			(lambda (cls) (std:struct-copy class cls 
				[sfuncs (map (lambda (x) (alloc-lid #f (class-name cls) x)) (class-sfuncs cls))]
				[vfuncs (map (lambda (x) (alloc-lid mac (class-name cls) x)) (class-vfuncs cls))]))
			(machine-classes mac))]))
	(std:struct-copy machine mac-tmp 
		[boot (alloc-lid #f (string-id "dummy") (machine-boot mac))] 
		[cmap (foldl 
			(lambda (cls cm) (imap-set cm (class-name cls) cls)) 
			imap-empty 
			(machine-classes mac-tmp))]))

(define (invoke-same-sig-alt? func-fml invoked-name invoked-arg-types)
	(define func (function-formula-func func-fml))
	(and
		(equal? (function-name func) invoked-name)
		(andmap (lambda (arg-1 arg-2) (equal? (cdr arg-1) arg-2)) (function-args func) invoked-arg-types)))

(define (lookup-virtual-function-alt mac cls func arg-types) 
	(if cls
		(begin
			(define cls-0 (imap-get (machine-cmap mac) cls))

			(define base-name (ormap 
				(lambda (cls-cur) (lookup-virtual-function-alt mac cls-cur func arg-types)) 
				(cons (class-extend cls-0) (class-implements cls-0))))

			(if base-name base-name
				(if 
					(ormap (lambda (func-cur) (invoke-same-sig-alt? func-cur func arg-types)) (class-vfuncs cls-0)) 
					(vfunc-sig->string cls func arg-types)
					#f)))
		#f))


(define (vfunc-id-alt mac cls func arg-types) (string-id (lookup-virtual-function-alt mac cls func arg-types)))


(define (build-virtual-table-alt mac) 
	(define classes (machine-classes mac))
	(define (process-class cls mac)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
;		(define vfields (if (equal? cls-name class-name-root) 
;			(cons field-name-class (class-vfields cls))
;			(class-vfields cls)))
		(define vfields (class-vfields cls))

		(define mac-sfuncs (foldl 
			(lambda (sf mac) 
				(define sid (sfunc-id cls-name (function-name (function-formula-func sf)) (map cdr (function-args (function-formula-func sf)))))
;				(pretty-print (machine-mem mac))
;				(define mem-1 (memory-sforce-write (machine-mem mac) sid sid 0))
				(define fmap-1 (imap-set (machine-fmap mac) sid sf))
				(std:struct-copy machine mac [fmap fmap-1]))
			mac sfuncs))

		(define mac-sfields (foldl 
			(lambda (sf mac) 
				(std:struct-copy machine mac 
					[mem (memory-sdecl (machine-mem mac) (sfield-id cls-name sf))])) 
			mac-sfuncs sfields))

		(define mac-vfuncs (foldl 
			(lambda (vf mac) 
				(define vid (vfunc-id-alt mac cls-name (function-name (function-formula-func vf)) (map cdr (function-args (function-formula-func vf)))))
				(define sid (sfunc-id cls-name (function-name (function-formula-func vf)) (map cdr (function-args (function-formula-func vf)))))
				(define mem-1 (memory-fdecl (machine-mem mac) vid)) 
				(define fmap-1 (imap-set (machine-fmap mac) sid vf))
				(std:struct-copy machine mac [mem mem-1] [fmap fmap-1]))
			mac-sfields vfuncs))

		(define mac-vfields (foldl 
			(lambda (vf mac) 
				(std:struct-copy machine mac 
					[mem (memory-fdecl (machine-mem mac) (vfield-id mac cls-name vf))])) 
			mac-vfuncs vfields))

		mac-vfields)

;	(define mem-push (memory-spush (machine-mem mac)))
	(define mac-cls (foldl process-class mac classes))
	(define mem-reserve-obj (cdr (memory-alloc (machine-mem mac-cls) vt-size)))
	(std:struct-copy machine mac-cls [mem mem-reserve-obj]))

(define (is-interface-func? func-fml)
	(null? (function-prog (function-formula-func func-fml))))



;----------------------- Allocation -------------------------
;funcion -> function-formula (with line id, pmark is empty)
(define (alloc-lid mac clsname func)
	(function-formula func 
		(map (lambda (any) (define-symbolic* line-id boolean?) line-id) (function-prog func))
		null
		#f
		#t
		(if mac (vfunc-id mac clsname (function-name func) (map cdr (function-args func))) #f)
		(sfunc-id clsname (function-name func) (map cdr (function-args func)))
		clsname))

;function-formula -> function-formula (with pmark)
(define (alloc-lstate func-fml)
	(std:struct-copy function-formula func-fml 
		[ret-lstate (lstate-new)]
		[lstates (map (lambda (any) (lstate-new)) (function-prog (function-formula-func func-fml)))]))

(define (lstate-new)
	(lstate 
		((lambda () (define-symbolic* path-mark boolean?) path-mark))
		null
		(memory-sym-new #f)))





;----------------------- Getter & Setter -------------------------
(define (get-lstate func-fml pc)
	(list-ref (function-formula-lstates func-fml) pc))

(define (starting-lstate func-fml)
	(car (function-formula-lstates func-fml)))

(define (ending-lstate func-fml)
	(function-formula-ret-lstate func-fml))

;return a new func-fml
(define (prepend-starting-mem-in func-fml pmark mem)
	(prepend-mem-in func-fml pmark mem pc-init))

;return a new func-fml
(define (prepend-ending-mem-in func-fml pmark mem) 
	(define st-old (ending-lstate func-fml))
	(define st-new (std:struct-copy lstate st-old [mem-in-list (cons (cons pmark mem) (lstate-mem-in-list st-old))]))
	(std:struct-copy function-formula func-fml [ret-lstate st-new]))

;return a new func-fml
(define (prepend-mem-in func-fml cnd mem pc) 
	(define st-old (get-lstate func-fml pc))
	(define st-new (std:struct-copy lstate st-old [mem-in-list (cons (cons cnd mem) (lstate-mem-in-list st-old))]))
;	(if (> (length (lstate-mem-in-list st-old)) 0) (begin (pretty-print st-old) (pretty-print st-new)) #f)
	(std:struct-copy function-formula func-fml [lstates (std:list-set (function-formula-lstates func-fml) pc st-new)]))

(define (append-mem-in func-fml cnd mem pc) 
	(define st-old (get-lstate func-fml pc))
	(define st-new (std:struct-copy lstate st-old [mem-in-list (append (lstate-mem-in-list st-old) (list (cons cnd mem)))]))
;	(if (not (null? (lstate-mem-in-list st-old))) (begin (display "Very Good\n") (pretty-print st-old) (pretty-print st-new)) (display "Good\n"))
	(std:struct-copy function-formula func-fml [lstates (std:list-set (function-formula-lstates func-fml) pc st-new)]))

(define (get-mem-in-list func-fml pc)
	(lstate-mem-in-list (get-lstate func-fml pc)))

(define (ending-mem-out func-fml)
	(lstate-mem-out (ending-lstate func-fml)))

(define (get-mem-out func-fml pc)
	(lstate-mem-out (get-lstate func-fml pc)))

(define (starting-pmark func-fml)
	(lstate-pmark (starting-lstate func-fml)))

(define (ending-pmark func-fml)
	(lstate-pmark (ending-lstate func-fml)))

(define (get-pmark func-fml pc)
	(lstate-pmark (get-lstate func-fml pc)))




(define (get-lid func-fml pc)
	(list-ref (function-formula-lids func-fml) pc))

(define (all-functions mac)
	(apply append
		(map (lambda (cls) (append (class-sfuncs cls) (class-vfuncs cls))) (machine-classes mac))))

(define (all-vfunctions mac)
	(apply append
		(map (lambda (cls) (class-vfuncs cls)) (machine-classes mac))))



;return a new func-fml
(define (append-fml func-fml fml)
	(std:struct-copy function-formula func-fml [fmls (and (function-formula-fmls func-fml) fml)]))

;============================= Encoder Implementation ====================================
;[!] Starting from this point, mac is a constant. 
;	 There is no "update". All formulae are relations between symbolic states.

;relation building state
;pc : not true pc, just the position when scanning through the program
(struct rbstate (funcs pc func-fml mac target-sids summary?) #:transparent)


; function X machine -> invoke-tree of function-formula from all functions in `mac` transitively invoked by `func`
; if summary? then this will compute a summary (root-invoke-ret-mem will contain all updates)
; otherwise it is guaranteed to encode this invoked function (but may not recursively do so)
(define (invoke->relation func-fml mac target-sids summary?)
	(display (~a "sid for invoked function is: " (function-formula-sid func-fml) "\n"))
	(display "\n ###############################################2 \n")
	(define ret (insts->relation func-fml mac target-sids summary?))
	(display (~a "sid for invoked function is: " (function-formula-sid func-fml) "\n"))
	(display "\n ###############################################3 \n")
	ret)

; "This function" is guaranteed to be at the beginning of the list.
(define (root-invoke itree)
	(invoke-tree-root itree))

(define (root-invoke-ret-mem itree summary?)
	(memory-select (lstate-mem-in-list (ending-lstate (root-invoke itree))) summary?))
	
(define (insts->relation func-fml mac target-sids summary?)
	(match (foldl inst->relation
					(rbstate null pc-init func-fml mac target-sids summary?) 
					(function-prog (function-formula-func func-fml)))
		[(rbstate funcs pc func-fml mac sid sum?) 
				(invoke-tree func-fml #t funcs)]))

(define (inst->relation inst st)
	(define ret (inst->relation.real inst st))
	(display "\n updated pc:\n")
	(pretty-print (rbstate-pc ret))
;	(check-asserts 0)
	ret)

;instruction X rbstate -> rbstate
(define (inst->relation.real inst st)


	(match st [(rbstate funcs pc func-fml mac target-sids summary?)
		(begin


		(define func (function-formula-func func-fml))
		(define mark (get-pmark func-fml pc))
		(define id (get-lid func-fml pc))
		(define in-target? (and (not summary?) (member (function-formula-sid func-fml) target-sids)))
;		(imap-set-selector id)

		(defer-eval "instruction: " inst)
		(display "\nInstruction:\n")
		(println inst)
		(println mark)
		(println id)
		(display (~a "In Target? " (if in-target? "++++++++++++"  "------------") "\n"))
		(display (~a "Summary? " (if summary? "++++++++++++"  "------------") "\n"))

		(if (not in-target?) (assert id) #f)

		(define fml-feasible-path (implies mark (ormap car (get-mem-in-list func-fml pc))))
		(if (not summary?) (assert fml-feasible-path) #f)

		(define mem-in (memory-select (get-mem-in-list func-fml pc) summary?))
;		(pretty-print mem-in)
		(define mem-0 (memory-sym-reset (get-mem-out func-fml pc) mem-in summary?))
;		(display "State id: ")  
;		(print (fml-to-struct (imap-sym-func-dummy (imap-sym-tracked-imap (imap-sym-scoped-imap (memory-addr-space mem-0))))))
;		(display " \n")
		;used only for expr-eval
		(define mac-eval-ctxt (std:struct-copy machine mac [mem mem-0]))


;	(pretty-print mem-0)


		(define (next-mark) 
			(get-pmark func-fml (+ 1 pc)))

		(define (label-mark label) 
			(define new-pc (imap-get (function-lmap func) label))
			(get-pmark func-fml new-pc))

		(define (select-fml? fml)
			(if in-target?
				(begin 
;				(display (~a "selector application: " (implies id fml) "\n"))
				(implies id fml))
				fml))

		;[?] should `equal?` be changed to `implies` ?
		(define (iassert-pc-next fml-path fml-op)
;			(set! fml-op #t)
			(if summary? #t
				(and 
					fml-path
					(implies mark (and fml-op (next-mark))))))

		(define (iassert-pc-ret fml-path fml-op)
;			(set! fml-op #t)
			(if summary? #t
				(and
					fml-path
					(implies mark (and fml-op (ending-pmark func-fml))))))

		(define (iassert-pc-branch fml-op cnd-t cnd-f label)
;			(set! fml-op #t)
			(if summary? #t
				(letrec ([fml-t (equal? cnd-t (label-mark label))]
						 [fml-f (equal? cnd-f (next-mark))]
						 [fml-cnd (and fml-t fml-f)]
						 [fml-br (or (label-mark label) (next-mark))]
						 [fml-path (implies mark (and fml-cnd fml-br fml-op))])
						fml-path)))

		;cases: (list of (cnd X pc))
		(define (iassert-pc-switch fml-op cases)
			(if summary? #t
				(begin
				(define fml-cnds (andmap+ 
					(lambda (cnd.pc) (equal? (car cnd.pc) (get-pmark func-fml (cdr cnd.pc))))
					cases))
				(define fml-br (ormap 
					(lambda (cnd.pc) (get-pmark func-fml (cdr cnd.pc)))
					cases))
				(define fml-path (implies mark (and fml-op fml-cnds fml-br)))
				fml-path)))

		(define (iassert-pc-invoke fml-path fml-op func-fmls cnds)
;			(set! fml-op #t)
			(if summary? #t
				(letrec	([fml-cnds (andmap
								(lambda (func-fml cnd)
;									(display "-------Problematic Fml---------\n")
;									(print-fml fml-ops)
									(and 
										(equal? cnd (starting-pmark func-fml))))
									;	(implies cnd fml-op))) 
								func-fmls cnds)]
						 [fml-brs (ormap starting-pmark func-fmls)])
						(and
							fml-path
							(equal? mark (and fml-op fml-cnds fml-brs (next-mark)))))))

		(define (long-jump-setup func-fml-callee mem)
			(define mem-0 (memory-sym-reset (memory-sym-new summary?) mem summary?))
			(define func (function-formula-func func-fml-callee))

			(define mem-decl (memory-sym-commit
				(foldl 
					(lambda (var-def mem) (memory-sdecl mem (car var-def))) 
					mem-0
					(append (function-args func) (function-locals func)))))

			(define fml-in (memory-sym-get-fml mem-decl summary?))
			(define mem-input (memory-sym-reset (memory-sym-new summary?) mem-decl (not in-target?)))
			(define func-fml-in (prepend-starting-mem-in func-fml-callee (if (or summary? in-target?) #t mark) mem-input))
			(cons func-fml-in fml-in))

		(define (invoke-setup func-fml-callee mem args)
;			(memory-print mem)
			(define mem-0 (memory-sym-reset (memory-sym-new summary?) mem summary?))
			(if (not summary?) (memory-print-id "mem-0" mem-0) #f)
			(define func (function-formula-func func-fml-callee))
			(define mem-push (memory-spush mem-0))
			(if (not summary?) (memory-print-id "mem-push" mem-push) #f)
			(define mem-decl (memory-sym-commit
				(foldl 
					(lambda (var-def mem) (memory-sdecl mem (car var-def))) 
					mem-push
					(append (function-args func) (function-locals func)))))
			(if (not summary?) (memory-print-id "mem-decl" mem-decl) #f)
			(define mem-arg (memory-sym-commit
				(foldl 
					(lambda (arg-src arg-dst mem) (memory-sforce-write mem (car arg-dst) (expr-eval arg-src mac-eval-ctxt) 0))
					mem-decl
					args 
					(function-args func))))
			(if (not summary?) (memory-print-id "mem-arg" mem-arg) #f)
			(define fml-in (memory-sym-get-fml mem-arg summary?))
			(define mem-input (memory-sym-reset (memory-sym-new summary?) mem-arg (not in-target?)))
			(if (not summary?) (memory-print-id "mem-input" mem-input) #f)
			(define func-fml-in (prepend-starting-mem-in func-fml-callee (if (or summary? in-target?) #t mark) mem-input))
			(cons func-fml-in fml-in))

		; bool X memory X int(if with branch)/#f(if no branch) -> rbstate
		(define (update-rbstate fml-new mem-out pc-opt-br)
			(if summary?
				(update-rbstate-verbose fml-new mem-out pc-opt-br #t #t)
				(update-rbstate-verbose fml-new mem-out pc-opt-br mark mark)))

		(define (update-rbstate-verbose fml-new mem-out pc-opt-br cnd-next cnd-br)
;			(test-assert! fml-new)
			(inspect fml-new)
			(define pc-next (+ 1 pc))
			(define func-fml-next (append-mem-in func-fml cnd-next mem-out pc-next))
			(define func-fml-br (if pc-opt-br (append-mem-in func-fml-next cnd-br mem-out pc-opt-br) func-fml-next))
			(define func-fml-new (append-fml func-fml-br fml-new))
			(pretty-print inst)
			(display (~a "Output state id: " (memory-id mem-out) "\n"))
			(std:struct-copy rbstate st [pc pc-next] [func-fml func-fml-new]))

		;cases: (list of (cnd X pc))
		(define (update-rbstate-switch fml-new mem-out cases)
			(display "Switch encoded.\n")
			(pretty-print cases)
			(inspect fml-new)
			(print-fml fml-new)
			(define func-fml-br (foldl (lambda (cnd.pc func-fml-cur)
					(append-mem-in func-fml-cur (car cnd.pc) mem-out (cdr cnd.pc)))
				func-fml 
				cases))
			(define func-fml-new (append-fml func-fml-br fml-new))
			(pretty-print inst)
			(display (~a "Output state id: " (memory-id mem-out) "\n"))
			(std:struct-copy rbstate st [pc (+ 1 pc)] [func-fml func-fml-new]))

		(define (update-mem-only mem-new)
			(define mem-commit (memory-sym-commit mem-new))
			(define fml-update (memory-sym-get-fml mem-commit summary?))
			(define fml-new (iassert-pc-next #t (select-fml? fml-update)))
			(update-rbstate fml-new mem-commit #f))

		(define (add-invoke-condition itree cnd)
			(std:struct-copy invoke-tree itree [cnd cnd]))

		(define (vid2sid mac classname vid)
			(print-fml classname)
			(if classname
				(begin
				(define cls-0 (imap-get (machine-cmap mac) classname))
				(define maybe-this-sid 
					(ormap identity
						(map 
							(lambda (func-fml-cur)
								(if (equal? vid (function-formula-vid func-fml-cur)) (function-formula-sid func-fml-cur) #f))
							(maybe-do class? null cls-0 class-vfuncs))))
				(if maybe-this-sid maybe-this-sid 
					(begin
					(define base-sid 
						(ormap 
							(lambda (sid) (if (is-not-found? sid) #f sid))
							(map 
								(lambda (cls-cur) (vid2sid mac cls-cur vid)) 
								(maybe-do class? null cls-0 (lambda (c) (cons (class-extend c) (class-implements c)))))))
					(if base-sid base-sid not-found))))
				not-found))

;		(if (and summary? (memory-is-null mem-in)) (std:struct-copy rbstate st [pc (+ 1 pc)])
		(match inst 
			[(inst-nop _) 
				(update-rbstate (iassert-pc-next #t #t) mem-in #f null)]

			[(inst-init classname)
				(begin
				(define addr (memory-sforce-read mem-0 var-this-name 1))
				(define fid-class-name (vfield-id mac classname field-name-class))
				(defer-eval "fid-class-name" fid-class-name)
				(define maybe-old-name (memory-fread mem-0 fid-class-name addr))
				(define maybe-class-name (if (equal? maybe-old-name not-found) classname maybe-old-name))
				(define mem-bind (memory-fwrite mem-0 fid-class-name addr maybe-class-name))

				(define mem-commit (memory-sym-commit mem-bind))
				(define fml-update (memory-sym-get-fml mem-commit summary?))
				(define fml-new (iassert-pc-next #t fml-update))
				(assert id)
				(update-rbstate fml-new mem-commit #f))]

				;(update-mem-only mem-bind))]

			[(inst-newarray v-name size-expr) 
				(begin
				(define size (expr-eval size-expr mac-eval-ctxt))
				(match-define (cons addr mem-alloc) (memory-alloc mem-0 size))
				(define mem-ass (memory-sforce-write mem-alloc v-name addr 0))
				(assert id)
				(update-mem-only mem-ass))]

			[(inst-new v-name) 
				(begin
				(match-define (cons addr mem-alloc) (memory-new mem-0))
				(define mem-ass (memory-sforce-write mem-alloc v-name addr 0))
				(assert id)
				(update-mem-only mem-ass))]

			[(inst-ret v-expr) 
				(begin
				(define ret-value (expr-eval v-expr mac-eval-ctxt))
				(if summary? #f (defer-eval inst ret-value))
				(define mem-ret (memory-sym-commit (memory-sforce-write mem-0 var-ret-name ret-value 0)))
				(define fml-update (memory-sym-get-fml mem-ret summary?))
;				(define fml-ret (select-fml? fml-update))
				(define fml-ret fml-update)
				(define fml-path (iassert-pc-ret #t fml-ret))
				(define func-fml-ret (prepend-ending-mem-in func-fml (if summary? #t mark) mem-ret))
				(define func-fml-new (append-fml func-fml-ret fml-path))
				(assert id)
				(pretty-print inst)
				(display (~a "Output state id: " (memory-id mem-ret) "\n"))
				(inspect fml-path)
;				(test-assert! fml-path)
				;(display (~a "Output state id: " (imap-sym-func-dummy (imap-sym-tracked-imap (imap-sym-scoped-imap (memory-addr-space mem-ret)))) " \n"))
				;(display (~a "Output state id: " (if mem-ret (imap-sym-func-dummy (imap-sym-tracked-imap (imap-sym-scoped-imap (memory-addr-space mem-ret)))) #f) " \n"))
				(std:struct-copy rbstate st [pc (+ 1 pc)] [func-fml func-fml-new]))]

			[(inst-long-jump cls-name func-name)
				(begin
				(define sid (sfunc-id cls-name func-name null))

				(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid)))
				(match-define (cons func-fml-in fml-in) (long-jump-setup func-invoked mem-in))
				(define funcs-ret (invoke->relation func-fml-in mac target-sids (or summary? in-target?)))

				(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? in-target?)))
				(define mem-ret.tmp2 (if in-target? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
				(define fml-sum (if in-target? (memory-sym-get-fml mem-ret.tmp2 summary?) #t))

				(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
				(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
				(if summary? #f (defer-eval inst ret-val))
				(define mem-ass (memory-sym-commit (memory-sforce-write mem-ret var-ret-name ret-val 0)))
				(define fml-ret (memory-sym-get-fml mem-ass summary?))

				;[TODO] use an extra selector for fml-sum
				(define fml-op (select-fml? (and fml-in fml-sum fml-ret)))
;				(display "fml-in:\n")
;				(print-fml fml-in)
;				(display "fml-sum:\n")
;				(print-fml fml-sum)
;				(display "fml-ret:\n")
;				(print-fml fml-ret)
;				(pretty-print mem-ass)
				;(define fml-op (and fml-in fml-ret))
				(define fml-new (iassert-pc-invoke #t fml-op (list func-fml-in) (list #t)))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))]

			[(inst-static-call ret cls-name func-name arg-types args) 
				(begin
				(define args-v (map (lambda (arg) (expr-eval arg mac-eval-ctxt)) args))
				(define mfunc (model-lookup cls-name func-name))
				(if mfunc 
					(update-mem-only (mfunc mem-0 ret args-v))

					(begin

					(define sid (sfunc-id cls-name func-name arg-types))
					(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid)))
					(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-in args))
					(define funcs-ret (invoke->relation func-fml-in mac target-sids (or summary? in-target?)))
					(pretty-print inst)

					(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? in-target?)))
					(display (~a "immediate return state id: " (memory-id mem-ret.tmp) "\n"))
					(define mem-ret.tmp2 (if in-target? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
					(display (~a "committed return state id: " (memory-id mem-ret.tmp2) "\n"))
					(define fml-sum (if in-target? (memory-sym-get-fml mem-ret.tmp2 summary?) #t))

					(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
					(display (~a "resolved state id: " (memory-id mem-ret) "\n"))
					(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
					(if summary? #f (defer-eval inst ret-val))
					(define mem-pop (memory-spop mem-ret))
					(define mem-ass (memory-sym-commit (memory-sforce-write mem-pop ret ret-val 0)))
					(define fml-ret (memory-sym-get-fml mem-ass summary?))

					;[TODO] use an extra selector for fml-sum
					(define fml-op (select-fml? (and fml-in fml-sum fml-ret)))
					;(define fml-op (and fml-in fml-ret))
					(define fml-new (iassert-pc-invoke #t fml-op (list func-fml-in) (list #t)))

					(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))))]

			[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define obj-addr (memory-sforce-read mem-0 obj-name 0))
				(define args-v (map (lambda (arg) (expr-eval arg mac-eval-ctxt)) args))

				(define mfunc (model-lookup cls-name func-name))
				(if mfunc 
					(update-mem-only (mfunc mem-0 obj-addr ret args-v))

					(begin

					(define mem--1 (memory-sym-reset (memory-sym-new summary?) mem-in summary?))

					(define vid (vfunc-id-alt mac cls-name func-name arg-types))
					(define funcs-invoked (map alloc-lstate 
						(filter (lambda (f) (and (not (is-interface-func? f)) (equal? (function-formula-vid f) vid))) 
							(all-vfunctions mac))))
					(define fid-class-name (vfield-id mac cls-name field-name-class))
					(define classname-true (memory-fread mem--1 fid-class-name obj-addr))
					(define true-func-invoked-sid (sfunc-id-pure classname-true func-name arg-types))

					;push an extra scope to avoid overwriting "this" of the current scope
					(define mem-this (memory-sym-commit (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr 0)))
					(define fml-this (memory-sym-get-fml mem-this summary?))
					(if (not summary?) (memory-print-id "mem-this" mem-this) #f)

					(define (invoke-candidate fcan)
						(begin
						(match-define (cons func-fml-in fml-in) (invoke-setup fcan mem-this args))
						;an invoke tree without condition
						(define funcs-ret (invoke->relation func-fml-in mac target-sids (or summary? in-target?)))
						(pretty-print inst)

						(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? in-target?)))
						(define mem-ret.tmp2 (if in-target? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
						(define fml-sum (if in-target? (memory-sym-get-fml mem-ret.tmp2 summary?) #t))
						(if (not summary?) (memory-print-id "mem-ret.tmp2" mem-ret.tmp2) #f)

						(define mem-ret (memory-sym-reset (if summary? mem-0 (memory-sym-new summary?)) mem-ret.tmp2 summary?))
						(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
						(if summary? #f (defer-eval inst ret-val))
						(define mem-pop (memory-spop (memory-spop mem-ret)))
						(define mem-ass (memory-sym-commit (memory-sforce-write mem-pop ret ret-val 0)))
						(define fml-ret (memory-sym-get-fml mem-ass summary?))
						(if (not summary?) (memory-print-id "mem-ass" mem-ass) #f)
						(define cnd (equal? (function-formula-sid fcan) true-func-invoked-sid))
	;					(display (~a "this function sid: " (function-formula-sid fcan) " true sid: " true-func-invoked-sid "\n"))
						(list func-fml-in cnd fml-in mem-ass fml-ret funcs-ret fml-sum)))
					
					(define ret-pack (map invoke-candidate funcs-invoked))

					(define func-fml-ins (map first ret-pack))
					(define cnds (map second ret-pack))
					(display "conditions:\n")
					(pretty-print cnds)
					(defer-eval "Virtual call conditions:" cnds)
					;[TODO] use an extra selector for fml-sum
					(define fml-call (and
						(andmap third ret-pack)
						(andmap fifth ret-pack)
						(andmap seventh ret-pack)))
					(define mem-ass (memory-select (map cons cnds (map fourth ret-pack)) summary?))
	;				(pretty-print mem-ass)
					(define funcs-ret (map sixth ret-pack))

					(display "=========2.5\n")
					(define fml-op (select-fml? (and fml-this fml-call)))
					(define fml-new (iassert-pc-invoke #t fml-op func-fml-ins cnds))

					(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (append funcs-ret funcs)]))))]

			[(inst-special-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define obj-addr (memory-sforce-read mem-0 obj-name 0))
				(define args-v (map (lambda (arg) (expr-eval arg mac-eval-ctxt)) args))

				(define mfunc (model-lookup cls-name func-name))
				(if mfunc 
					(update-mem-only (mfunc mem-0 obj-addr ret args-v))

					(begin

					(define mem--1 (memory-sym-reset (memory-sym-new summary?) mem-in summary?))

					(define sid (sfunc-id cls-name func-name arg-types))
					(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid)))

					;push an extra scope to avoid overwriting "this" of the current scope
					(define mem-this (memory-sym-commit (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr 0)))
					(define fml-this (memory-sym-get-fml mem-this summary?))

					(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-this args))
					(define funcs-ret (invoke->relation func-fml-in mac target-sids (or in-target? summary?)))
					(pretty-print inst)

					(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? in-target?)))
					(define mem-ret.tmp2 (if in-target? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
					(define fml-sum (if in-target? (memory-sym-get-fml mem-ret.tmp2 summary?) #t))

					(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
					(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
					(if summary? #f (defer-eval inst ret-val))
					(define mem-pop (memory-spop (memory-spop mem-ret)))
					(define mem-ass (memory-sym-commit (memory-sforce-write mem-pop ret ret-val 0)))
					(define fml-ret (memory-sym-get-fml mem-ass summary?))

					;[TODO] use an extra selector for fml-sum
					(define fml-op (select-fml? (and fml-this fml-in fml-sum fml-ret)))
					;(define fml-op (and fml-this fml-in fml-ret))
					(define fml-new (iassert-pc-invoke #t fml-op (list func-fml-in) (list #t)))

					(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))))]

			[(inst-ass vl vr) 
				(begin
				(define value (expr-eval vr mac-eval-ctxt))
				(if summary? #f (defer-eval inst value))
;				(defer-cons (equal? value 6))
				(define rhs (lexpr-rhs vl))
				(define mem-new 
					(match rhs
						[(expr-var v) (memory-sforce-write mem-0 (string-id (variable-name v)) value 0)]
						[(expr-array arr idx)
							(letrec
								([addr (memory-sforce-read mem-0 (string-id (variable-name arr)) 0)]
								[idx-e (ast->expression idx)]
								[idx-v (expr-eval idx-e mac-eval-ctxt)])
								(memory-awrite mem-0 addr idx-v value))]
						[(expr-field obj cls fname)
							(if (equal? obj void-receiver)
								(memory-sforce-write mem-0 (sfield-id (string-id (type-name-name cls)) (string-id (field-name fname))) value 0)
								(letrec
									([addr (memory-sforce-read mem-0 (string-id (variable-name obj)) 0)])
									(memory-fwrite mem-0 (vfield-id mac (string-id (type-name-name cls)) (string-id (field-name fname))) addr value)))]))
				(memory-print-id "mem-new" mem-new)
				(update-mem-only mem-new))]

			;[!]there might be bug if reading from heap
			[(inst-switch cnd cases default-l)
				(begin
				(define lmap (function-lmap func))
				(define cnd-v (expr-eval cnd mac-eval-ctxt))
				(define default-pc (if default-l (imap-get lmap default-l) (+ 1 pc)))

				(define cases-cnd (map (lambda (k.l)
						(cons (select-fml? (equal? (car k.l) cnd-v)) (imap-get lmap (cdr k.l))))
					cases))
				(define neg-cnd (foldl (lambda (cnd.pc fml) (and (not (car cnd.pc)) fml)) #t cases-cnd))
				(define cases-cnd+ (cons (cons neg-cnd default-pc) cases-cnd))

				(define cases-mark (map (lambda (k.l)
						(cons mark (imap-get lmap (cdr k.l))))
					cases))
				(define neg-mark (foldl (lambda (cnd.pc fml) (and (not (car cnd.pc)) fml)) #t cases-mark))
				(define cases-mark+ (cons (cons neg-mark default-pc) cases-mark))

				(define fml-new (iassert-pc-switch #t cases-cnd+))
				(update-rbstate-switch fml-new mem-in (if summary? cases-cnd+ cases-mark+)))]

			;[!]there might be bug if reading from heap
			[(inst-jmp condition label)
				(begin
				(define lmap (function-lmap func))
				(define cnd (expr-eval condition mac-eval-ctxt))
				(define fml-update #t)
				(define fml-new (iassert-pc-branch (select-fml? fml-update) (select-fml? cnd) (select-fml? (not cnd)) label))
				(define pc-br (imap-get (function-lmap func) label))
				(if summary? 
					(update-rbstate-verbose fml-new mem-in pc-br (not cnd) cnd)
					(update-rbstate fml-new mem-in pc-br)))]))]))

