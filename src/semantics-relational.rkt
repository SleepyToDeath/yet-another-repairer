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
(require "type-checker.rkt")
(require "semantics-common.rkt")
(require "enumerator.rkt")
(require "domain-config.rkt")
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

(define valid-selectors null)
(define (clear-valid-selectors!)
	(set! valid-selectors null))
(define (add-valid-selector! l)
	(set! valid-selectors (cons l valid-selectors)))
(define (finalize-selectors!)
	(set! valid-selectors (remove-duplicates valid-selectors (lambda (a b) (equal? (~a a) (~a b))))))
(register-reset! clear-valid-selectors! #f)
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
							(lambda (line inst selector) (location (imap-get (machine-cmap mac) class default-type) func line inst selector))
							(std:range (length lids)) 
							(function-prog func)
							lids)]))
				(all-functions mac))))

	(define sum (apply + (map (lambda (l) (if (location-selector l) 1 0)) soft-cons)))
	(define no-bug (equal? sum (length soft-cons)))

	(define (hard-cons input output spec-id target-sids) 

		(memory-clear-id-list!)
		(imap-clear-indices!)

		(define (assign-input mac input)
			(define mem0 (machine-mem mac))
			(define mem-push (memory-spush mem0))
			(reset-parameter-names)
			(match-define (cons mem-ass fml-ass)
				(foldl 
					(lambda (v.t mem+fml) 
						(define-symbolic* vi (jtype->mtype (string-id (cdr v.t))))
						(define fml (equal? vi (car v.t)))
						(cons 
							(memory-sforce-write (car mem+fml) (next-parameter-name) vi 0 (jtype->mtype (string-id (cdr v.t))))
							(and (cdr mem+fml) fml)))
					(cons mem-push #t) 
					input))
			(define mem-ret (memory-sdecl mem-ass var-ret-name default-type))
			(cons (std:struct-copy machine mac [mem mem-ret]) fml-ass))

		(define (compare-output mac output)
			(define mem0 (machine-mem mac))
			(andmap identity (map
				;[!] assuming output is always int
				(lambda (kv) (equal? (cdr kv) (memory-sforce-read mem0 (string-id (car kv)) 0)))
				output)))

		(match-define (cons mac-ass0 fml-ass0) (assign-input mac input))

		(define mac-ass (build-virtual-table-alt mac-ass0))

		(define fml-ass (and (memory-sym-ssummary (machine-mem mac-ass)) fml-ass0))

		(define boot-lstate (prepend-starting-mem-in (alloc-lstate (machine-boot mac-ass)) #t (machine-mem mac-ass)))

		(define fml-cfi (starting-pmark boot-lstate))
;		(test-assert! no-bug)
;		(test-assert! fml-cfi)

		(display "\n ###############################################0 \n")
		(set-context! mac-ass)
		(map (lambda (cls) (pretty-print (cons (class-name cls) (class-vfields cls)))) (machine-classes mac-ass))
		(define all-invokes (invoke->relation boot-lstate mac-ass spec-id target-sids #f))
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
					(define ret1 
						(function-formula-fmls root))
					(define ret2
						(apply append (map extract-fml subs)))
					(define ret (append ret2 (list ret1)))
					ret)]))

		(define fml-code-1 (memory-sym-summary mem-all-done #f))
		(define fml-code-2 (extract-fml all-invokes))

		(display "---------fml0-------------\n")
		(define fml-code (append (list fml-code-1) fml-code-2))
		(display "\n ###############################################6 \n")
		(define fml-boot-is-correct (andmap identity (function-formula-lids boot-lstate)))
		(display "\n ###############################################7 \n")
;		(test-assert! fml-code)
		(define fml-code-bind (memory-gen-binding))
;		(test-assert! fml-code-bind)
		(display "\n ###############################################8 \n")
		;(pretty-print (list fml-cfi fml-code fml-code-bind fml-ass fml-out))
;		(and fml-cfi fml-code fml-code-bind))
		(display (~a "Number of asserts: " (length (asserts)) "\n"))
;		(assert (andmap+ identity 
;			(append (list fml-cfi) fml-code (list fml-ass) (list fml-out))))
;		fml-code-bind)
;		(append (list fml-cfi) fml-code fml-code-bind (list fml-ass)))
		(append (list fml-cfi) fml-code fml-code-bind (list fml-ass) (list fml-out)))

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
			(lambda (cls cm) (imap-set cm (class-name cls) cls default-type)) 
			(imap-empty default-type)
			(machine-classes mac-tmp))]))

(define vfunc-id-alt (curry vfunc-id function-formula-func))

(define (build-virtual-table-alt mac) 
	(define classes (machine-classes mac))
	(define (process-class cls mac)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
		(define vfields (class-vfields cls))

		(define mac-sfuncs (foldl 
			(lambda (sf mac) 
				(define sid (sfunc-id cls-name (function-name (function-formula-func sf)) (map cdr (function-args (function-formula-func sf)))))
;				(pretty-print (machine-mem mac))
;				(define mem-1 (memory-sforce-write (machine-mem mac) sid sid 0))
				(define fmap-1 (imap-set (machine-fmap mac) sid sf default-type))
				(std:struct-copy machine mac [fmap fmap-1]))
			mac sfuncs))

		(define mac-sfields (foldl 
			(lambda (sf mac) 
				;[?] why did we use this?
;				(define mem-decl (memory-sdecl (machine-mem mac) (sfield-id cls-name (car sf))))
				;[TODO] field type
				;static fields are treated as virtual fields of a special void receiver object
				(define mem-decl (memory-fdecl (machine-mem mac) (vfield-id mac cls-name (car sf))))
				(define tmap-1 (imap-set (machine-tmap mac) (sfield-id cls-name (car sf)) (cdr sf) default-type))
				(std:struct-copy machine mac 
					[mem mem-decl][tmap tmap-1])) 
			mac-sfuncs sfields))

		(define mac-vfuncs (foldl 
			(lambda (vf mac) 
				(define vid (vfunc-id-alt mac cls-name (function-name (function-formula-func vf)) (map cdr (function-args (function-formula-func vf)))))
				(define sid (sfunc-id cls-name (function-name (function-formula-func vf)) (map cdr (function-args (function-formula-func vf)))))
				(define mem-1 (memory-fdecl (machine-mem mac) vid)) 
				(define fmap-1 (imap-set (machine-fmap mac) sid vf default-type))
				(std:struct-copy machine mac [mem mem-1] [fmap fmap-1]))
			mac-sfields vfuncs))

		(define mac-vfields (foldl 
			(lambda (vf mac) 
				(define tmap-1 (imap-set (machine-tmap mac) (sfield-id cls-name (car vf)) (cdr vf) default-type))
				(define mem-decl (memory-fdecl (machine-mem mac) (vfield-id mac cls-name (car vf))))
				(std:struct-copy machine mac 
					[mem mem-decl][tmap tmap-1])) 
			mac-vfuncs vfields))

		mac-vfields)

;	(define mem-push (memory-spush (machine-mem mac)))
	(define mac-cls (foldl process-class mac classes))
	(define mem-reserve-obj (cdr (memory-alloc (machine-mem mac-cls) vt-size)))
	(match-define (cons addr mem-void-receiver) (memory-new mem-reserve-obj))
	(set-void-receiver-addr addr)
	(std:struct-copy machine mac-cls [mem mem-void-receiver]))


;----------------------- Allocation -------------------------
;function -> function-formula (with line id, pmark is empty)
(define (alloc-lid mac clsname func)
	(function-formula func 
		(map (lambda (any) (define-symbolic* line-id boolean?) line-id) (function-prog func))
		null
		#f
		#t
		(if mac (vfunc-id-ori mac clsname (function-name func) (map cdr (function-args func))) #f)
		(sfunc-id clsname (function-name func) (map cdr (function-args func)))
		clsname))

;function-formula -> function-formula (with pmark)
(define (alloc-lstate func-fml)
	(display "allocating new lstates\n")
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
	(if (>= pc (length (function-formula-lstates func-fml))) #f
		(list-ref (function-formula-lstates func-fml) pc)))

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
	(define lst (get-lstate func-fml pc))
	(if (not lst) #f
		(lstate-pmark lst)))


(define (get-lid func-fml pc)
	(list-ref (function-formula-lids func-fml) pc))


(define contains-target-alt? (curry contains-target? function-formula-func))


;return a new func-fml
(define (append-fml func-fml fml)
	(std:struct-copy function-formula func-fml [fmls (and (function-formula-fmls func-fml) fml)]))

;============================= Encoder Implementation ====================================
;[!] Starting from this point, mac is a constant. 
;	 There is no "update". All formulae are relations between symbolic states.

;relation building state
;pc : not true pc, just the position when scanning through the program
(struct rbstate (funcs pc func-fml mac spec-id target-sids summary?) #:transparent)


; function X machine -> invoke-tree of function-formula from all functions in `mac` transitively invoked by `func`
; if summary? then this will compute a summary (root-invoke-ret-mem will contain all updates)
; otherwise it is guaranteed to encode this invoked function (but may not recursively do so)
(define (invoke->relation func-fml mac spec-id target-sids summary?)
;	(display (~a "sid for invoked function is: " (function-formula-sid func-fml) "\n"))
;	(display "\n ###############################################2 \n")
	(define ret (insts->relation func-fml mac spec-id target-sids summary?))
;	(display (~a "sid for invoked function is: " (function-formula-sid func-fml) "\n"))
;	(display "\n ###############################################3 \n")
	ret)

; "This function" is guaranteed to be at the beginning of the list.
(define (root-invoke itree)
	(invoke-tree-root itree))

(define (root-invoke-ret-mem itree summary?)
	(display "################# Func Return! ##################\n")
	(memory-select (lstate-mem-in-list (ending-lstate (root-invoke itree))) summary?))
	
(define (insts->relation func-fml mac spec-id target-sids summary?)
	(match (foldl inst->relation
					(rbstate null pc-init func-fml mac spec-id target-sids summary?) 
					(function-prog (function-formula-func func-fml)))
		[(rbstate funcs pc func-fml mac spec-id sid sum?) 
				(invoke-tree func-fml #t funcs)]))

(define (inst->relation inst st)
	(define ret (inst->relation.real inst st))
;	(display "\n updated pc:\n")
;	(pretty-print (rbstate-pc ret))
;	(check-asserts 0)
	ret)

(define line-counter 0)

;instruction X rbstate -> rbstate
(define (inst->relation.real inst st)


	(match st [(rbstate funcs pc func-fml mac spec-id target-sids summary?)
		(begin


		(define func (function-formula-func func-fml))
		(define mark (get-pmark func-fml pc))
		(define id (get-lid func-fml pc))
		(define in-target? (and (not summary?) (member (function-formula-sid func-fml) target-sids)))
		(define trigger-summary? in-target?) ;updated later

		(set! line-counter (+ line-counter 1))
		(add-valid-selector! id)

;		(display (~a "Lines of code: " line-counter "\n"))
		(defer-eval spec-id "instruction: " inst)
		(defer-eval spec-id "path mark " mark)
		(defer-eval spec-id "id " id)
		(println inst)
		(pretty-print mark)
		(pretty-print id)
		(display (~a "In Target? " (if in-target? "++++++++++++"  "------------") "\n"))
		(display (~a "Summary? " (if summary? "++++++++++++"  "------------") "\n"))

		(if (not in-target?) (assert id) #f)

		(define fml-feasible-path (implies mark (ormap car (get-mem-in-list func-fml pc))))
		(if (not summary?) (assert fml-feasible-path) #f)

		;[!] assume no unreachable code
		(define mem-in (memory-select (get-mem-in-list func-fml pc) summary?))
		(define mem-0 (memory-sym-reset (get-mem-out func-fml pc) mem-in summary?))

		;used only for expr-eval
		(define mac-eval-ctxt (std:struct-copy machine mac [mem mem-0][fc func]))

		(display (~a "mem-id: " (memory-id mem-0) "\n"))
		(defer-eval current-spec-id "mem-id " (memory-id mem-0))
		(display "\n")


;	(pretty-print mem-0)

		(define (label-pc label)
			(imap-get (function-lmap func) label default-type))

		(define (next-mark) 
			(get-pmark func-fml (+ 1 pc)))

		(define (label-mark label) 
			(get-pmark func-fml (label-pc label)))

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
					(implies mark (and fml-op (not (next-mark)) (ending-pmark func-fml))))))

		(define (iassert-pc-branch fml-op cnd-t cnd-f label)
;			(set! fml-op #t)
			(if summary? #t
				(letrec ([fml-t (select-fml? (equal? cnd-t (and (not (next-mark)) (label-mark label))))]
						 [fml-f (select-fml? (equal? cnd-f (next-mark)))]
						 [fml-cnd (if (equal? (+ 1 pc) (label-pc label)) #t (and fml-t fml-f))]
						 [fml-br (or (label-mark label) (next-mark))]
						 [fml-path (implies mark (and fml-cnd fml-br fml-op))])
						fml-path)))

		;cases: (list of (cnd X pc))
		(define (iassert-pc-switch fml-op cases)
			(if summary? #t
				(begin
				(define fml-cnds (andmap+ 
					(lambda (cnd.pc) (implies (car cnd.pc) (get-pmark func-fml (cdr cnd.pc))))
					cases))
				(define fml-br (ormap 
					(lambda (cnd.pc) (get-pmark func-fml (cdr cnd.pc)))
					cases))
				(define fml-path (implies mark (and fml-op fml-cnds fml-br)))
				fml-path)))

		(define (iassert-pc-invoke fml-op func-fmls cnds)
;			(set! fml-op #t)
			(if summary? #t
				(letrec	([fml-cnds (andmap+
							(lambda (func-fml cnd) (equal? cnd (starting-pmark func-fml)))
							func-fmls cnds)]
						 [fml-brs (ormap starting-pmark func-fmls)])
						(implies mark (and fml-op fml-cnds fml-brs (next-mark))))))

		(define (long-jump-setup func-fml-callee mem) (force-error #t "long jump is obsolete\n"))
#|
			(define mem-0 (memory-sym-reset (memory-sym-new summary?) mem summary?))
			(define func (function-formula-func func-fml-callee))

			(define mem-decl (memory-sym-commit
				(foldl 
					(lambda (var-def mem) (memory-sdecl mem (car var-def) (jtype->mtype (cdr var-def))))
					mem-0
					(append (function-args func) (function-locals func)))))

			(define fml-in (memory-sym-summary mem-decl summary?))
			(define mem-input (memory-sym-reset (memory-sym-new summary?) mem-decl (not trigger-summary?)))
			(define func-fml-in (prepend-starting-mem-in func-fml-callee (if (or summary? trigger-summary?) #t mark) mem-input))
			(cons func-fml-in fml-in))
|#

		(define (invoke-setup func-fml-callee mem args)
;			(memory-print mem)
;			(pretty-print args)
;			(pretty-print mem)
;			(display "invoke setup #1\n")
			(define mem-0 (memory-sym-reset (memory-sym-new summary?) mem summary?))
			(define func (function-formula-func func-fml-callee))
			(define mem-push (memory-spush mem-0))
;			(display "invoke setup #2\n")
			(define mem-decl (memory-sym-commit
				(foldl 
					(lambda (var-def mem) (memory-sdecl mem (car var-def) (jtype->mtype (cdr var-def)))) 
					mem-push
					(append (function-args func) (function-locals func) (list (cons var-void-ret default-type) (cons var-ret-name (function-ret (function-formula-func func-fml-callee))))))))
;			(display "invoke setup #3\n")
			(define mem-arg (memory-sym-commit
				(foldl 
					(lambda (arg-src arg-dst mem) 
						(defer-eval current-spec-id "arg ass " (list arg-src arg-dst (expr-eval arg-src mac-eval-ctxt)))
						(memory-sforce-write mem (car arg-dst) (car (expr-eval arg-src mac-eval-ctxt)) 0 (jtype->mtype (cdr arg-dst))))
					mem-decl
					args 
					(function-args func))))
			(define fml-in (memory-sym-summary mem-arg summary?))
;			(pretty-print mem-arg)
;			(display "invoke setup #4\n")
			(define mem-input (memory-sym-reset (memory-sym-new summary?) mem-arg (not trigger-summary?)))
			(define func-fml-in (prepend-starting-mem-in func-fml-callee (if (or summary? trigger-summary?) #t mark) mem-input))
;			(display "invoke setup #5\n")
			(cons func-fml-in fml-in))

		; bool X memory X int(if with branch)/#f(if no branch) -> rbstate
		(define (update-rbstate fml-new mem-out pc-opt-br)
			(if summary?
				(update-rbstate-verbose fml-new mem-out pc-opt-br #t #t)
				(update-rbstate-verbose fml-new mem-out pc-opt-br mark mark)))

		(define (update-rbstate-verbose fml-new mem-out pc-opt-br cnd-next cnd-br)
			(define pc-next (+ 1 pc))
			(define func-fml-next (prepend-mem-in func-fml cnd-next mem-out pc-next))
			(define func-fml-br (if pc-opt-br (prepend-mem-in func-fml-next cnd-br mem-out pc-opt-br) func-fml-next))
			(define func-fml-new (append-fml func-fml-br fml-new))
			(std:struct-copy rbstate st [pc pc-next] [func-fml func-fml-new]))

		;cases: (list of (cnd X pc))
		(define (update-rbstate-switch fml-new mem-out cases)
;			(display "Switch encoded.\n")
;			(pretty-print cases)
;			(print-fml fml-new)
			(define func-fml-br (foldl (lambda (cnd.pc func-fml-cur)
					(append-mem-in func-fml-cur (car cnd.pc) mem-out (cdr cnd.pc)))
				func-fml 
				cases))
			(define func-fml-new (append-fml func-fml-br fml-new))
;			(pretty-print inst)
;			(display (~a "Output state id: " (memory-id mem-out) "\n"))
			(std:struct-copy rbstate st [pc (+ 1 pc)] [func-fml func-fml-new]))

		(define (update-mem-only mem-new)
			(define mem-commit (memory-sym-commit mem-new))
			(define fml-update (memory-sym-summary mem-commit summary?))
			(define fml-new (iassert-pc-next #t (select-fml? fml-update)))
			(update-rbstate fml-new mem-commit #f))

		(match inst 
			[(inst-nop _) 
				(update-rbstate (iassert-pc-next #t #t) mem-in #f)]

			[(inst-init classname)
				(begin
				(define addr (memory-sforce-read mem-0 var-this-name 1))
				(define fid-class-name (vfield-id mac classname field-name-class))
				(define maybe-old-name (memory-fread mem-0 fid-class-name addr addr-type))
				(define maybe-class-name (if (equal? maybe-old-name (not-found name-type)) classname maybe-old-name))
				(define mem-bind (memory-fwrite mem-0 fid-class-name addr maybe-class-name name-type))

				(define mem-commit (memory-sym-commit mem-bind))
				(define fml-update (memory-sym-summary mem-commit summary?))
				(define fml-new (iassert-pc-next #t fml-update))
				(assert id)
				(update-rbstate fml-new mem-commit #f))]

				;(update-mem-only mem-bind))]

			[(inst-newarray v-name size-expr) 
				(begin
				(define size (car (expr-eval size-expr mac-eval-ctxt)))
				(match-define (cons addr mem-alloc) (memory-alloc mem-0 size))
				(define mem-ass (memory-sforce-write mem-alloc v-name addr 0 addr-type))
				(assert id)
				(update-mem-only mem-ass))]

			[(inst-new v-name) 
				(begin
				(match-define (cons addr mem-alloc) (memory-new mem-0))
				(define mem-ass (memory-sforce-write mem-alloc v-name addr 0 addr-type))
				(assert id)
				(update-mem-only mem-ass))]

			[(inst-ret v-expr) 
				(begin
				(define ret-value (car (expr-eval v-expr mac-eval-ctxt)))
				(define ret-jtype (function-ret func))
				(define mem-ret.tmp (memory-sforce-write mem-0 var-ret-name ret-value 0 (jtype->mtype ret-jtype)))
				(define mem-ret (memory-sym-commit mem-ret.tmp))
				(define fml-update (memory-sym-summary mem-ret summary?))
				(define fml-ret (select-fml? fml-update))
				(define fml-path (iassert-pc-ret #t fml-ret))
				(define func-fml-ret (prepend-ending-mem-in func-fml (if summary? #t mark) mem-ret))
				(define func-fml-new (append-fml func-fml-ret fml-path))
				(defer-eval spec-id "return value" ret-value)
;				(assert id)
				(if summary? #f (add-spec id spec-id 
					(memory-sym-reset (memory-sym-new summary?) mem-in summary?)
					(memory-sym-reset (memory-sym-new summary?) (memory-sym-commit mem-ret) summary?) 
					inst inst-ret? #f mark))
				(std:struct-copy rbstate st [pc (+ 1 pc)] [func-fml func-fml-new]))]

			[(inst-long-jump cls-name func-name)
				(begin
				(define sid (sfunc-id cls-name func-name null))
				(set! trigger-summary? (or in-target? (and (not summary?) (not (contains-target-alt? mac sid target-sids)))))

				(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid default-type)))
				(match-define (cons func-fml-in fml-in) (long-jump-setup func-invoked mem-in))
				(define funcs-ret (invoke->relation func-fml-in mac spec-id target-sids (or summary? trigger-summary?)))

				(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? trigger-summary?)))
				(define mem-ret.tmp2 (if trigger-summary? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
				(define fml-sum (if trigger-summary? (memory-sym-summary mem-ret.tmp2 summary?) #t))

				(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
				(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
				(define mem-ass (memory-sym-commit (memory-sforce-write mem-ret var-ret-name ret-val 0 
					(jtype->mtype (function-ret (function-formula-func func-invoked))))))
				(define fml-ret (memory-sym-summary mem-ass summary?))

				(define fml-op (select-fml? (and fml-in fml-sum fml-ret)))
				(define fml-new (iassert-pc-invoke fml-op (list func-fml-in) (list #t)))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))]

			[(inst-static-call ret cls-name func-name arg-types args) 
				(begin
				(if (member cls-name localization-good-classes) (assert id) #f)
				(define args-v (map (lambda (arg) (car (expr-eval arg mac-eval-ctxt))) args))
				(define mfunc (model-lookup cls-name func-name))
				(if mfunc 
					(begin
;					(assert id)
					(update-mem-only (mfunc mem-0 ret args-v)))

					(begin

					(define sid (sfunc-id cls-name func-name arg-types))
					(set! trigger-summary? (or in-target? (and (not summary?) (not (contains-target-alt? mac sid target-sids)))))

					(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid default-type)))
					(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-in args))
					(define funcs-ret (invoke->relation func-fml-in mac spec-id target-sids (or summary? trigger-summary?)))
;					(pretty-print inst)

					(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? trigger-summary?)))
					(define mem-ret.tmp2 (if trigger-summary? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
					(define fml-sum (if trigger-summary? (memory-sym-summary mem-ret.tmp2 summary?) #t))

					(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
					(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
					(define mem-pop (memory-spop mem-ret))
					(define mem-ass (memory-sym-commit (memory-sforce-write mem-pop ret ret-val 0 
						(jtype->mtype (function-ret (function-formula-func func-invoked))))))
					(define fml-ret (memory-sym-summary mem-ass summary?))

					(define fml-op (select-fml? (and fml-in fml-sum fml-ret)))
					;(define fml-op (and fml-in fml-ret))
					(define fml-new (iassert-pc-invoke fml-op (list func-fml-in) (list #t)))

					(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))))]

			[(inst-virtual-call ret obj-name cls-name func-name arg-types args)

				(begin
				(if (member cls-name localization-good-classes) (assert id) #f)
				(define obj-addr0 (memory-sforce-read mem-0 obj-name 0))
				(define args-v (map (lambda (arg) (car (expr-eval arg mac-eval-ctxt))) args))

				(define mfunc (model-lookup cls-name func-name))
				(if mfunc 
					(begin
;					(assert id)
					(update-mem-only (mfunc mem-0 obj-addr0 ret args-v)))

					(begin
					(define mem--1 (memory-sym-reset (memory-sym-new summary?) mem-in summary?))
					(define obj-addr (memory-sforce-read mem--1 obj-name 0))
					(set! mac-eval-ctxt (std:struct-copy machine mac-eval-ctxt [mem mem--1]))
					(define vid (vfunc-id-alt mac cls-name func-name arg-types))
					(define funcs-invoked (map alloc-lstate 
						(filter (lambda (f) (and (not (is-interface-func? (function-formula-func f))) (equal? (function-formula-vid f) vid))) 
							(all-vfunctions mac))))
					(define fid-class-name (vfield-id mac cls-name field-name-class))
					(define classname-true (memory-fread mem--1 fid-class-name obj-addr name-type))
					(define true-func-invoked-sid (sfunc-id-pure classname-true func-name arg-types))

;					(defer-eval current-spec-id "obj-name" obj-name)
;					(defer-eval current-spec-id "obj-addr" obj-addr)
;					(defer-eval current-spec-id "fid-class-name" fid-class-name)
;					(defer-eval current-spec-id "classname-true" classname-true)
;					(defer-eval current-spec-id "true sid" true-func-invoked-sid)

					;push an extra scope to avoid overwriting "this" of the current scope
					(define mem-this (memory-sym-commit (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr 0 addr-type)))
					(define fml-this (memory-sym-summary mem-this summary?))

					(define (invoke-candidate fcan)
						(begin
						;[?] can different styles of callee be mixed in one calling instruction? I think yes, but note for potential bugs
						(set! trigger-summary? (or in-target? (and (not summary?) (not (contains-target-alt? mac (function-formula-sid fcan) target-sids)))))
;						(display "virtaul call #6\n")

						(match-define (cons func-fml-in fml-in) (invoke-setup fcan mem-this args))
						;an invoke tree without condition
						(define funcs-ret (invoke->relation func-fml-in mac spec-id target-sids (or summary? trigger-summary?)))
;						(pretty-print inst)

						(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? trigger-summary?)))
						(define mem-ret.tmp2 (if trigger-summary? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
						(define fml-sum (if trigger-summary? (memory-sym-summary mem-ret.tmp2 summary?) #t))

						(define cnd (equal? (function-formula-sid fcan) true-func-invoked-sid))
						(list func-fml-in cnd fml-in mem-ret.tmp2 funcs-ret fml-sum)))

					(define ret-pack (map invoke-candidate funcs-invoked))

					(define cnds (map second ret-pack))

					(define mem-ret.tmp2 (memory-select (map cons cnds (map fourth ret-pack)) summary?))
					(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
					(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
					(define mem-pop (memory-spop (memory-spop mem-ret)))
					(define mem-ass (memory-sym-commit (memory-sforce-write mem-pop ret ret-val 0 
						(jtype->mtype (function-ret (function-formula-func (car funcs-invoked)))))))
					(define fml-ret (memory-sym-summary mem-ass summary?))

					(define func-fml-ins (map first ret-pack))
					(define fml-call (and
						(andmap third ret-pack)
						(andmap sixth ret-pack)))

					(define funcs-ret (map fifth ret-pack))

					(define fml-op (select-fml? (and fml-ret fml-this fml-call)))
					(define fml-new (iassert-pc-invoke fml-op func-fml-ins cnds))

					(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (append funcs-ret funcs)]))))]

			[(inst-special-call ret obj-name cls-name func-name arg-types args)
				(begin
				(assert id)
				(if (member cls-name localization-good-classes) (assert id) #f)
				(define obj-addr0 (memory-sforce-read mem-0 obj-name 0))
				(define args-v (map (lambda (arg) (car (expr-eval arg mac-eval-ctxt))) args))

				(define mfunc (model-lookup cls-name func-name))
				(if mfunc 
					(begin
;					(assert id)
					(update-mem-only (mfunc mem-0 obj-addr0 ret args-v)))

					(begin
					(define mem--1 (memory-sym-reset (memory-sym-new summary?) mem-in summary?))
					(set! mac-eval-ctxt (std:struct-copy machine mac-eval-ctxt [mem mem--1]))
					(define obj-addr (memory-sforce-read mem--1 obj-name 0))
					(define sid (sfunc-id cls-name func-name arg-types))
					(set! trigger-summary? (or in-target? (and (not summary?) (not (contains-target-alt? mac sid target-sids)))))
					(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid default-type)))

					;push an extra scope to avoid overwriting "this" of the current scope
					(define mem-this (memory-sym-commit (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr 0 addr-type)))
					(define fml-this (memory-sym-summary mem-this summary?))

					(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-this args))
					(define funcs-ret (invoke->relation func-fml-in mac spec-id target-sids (or trigger-summary? summary?)))

					(define mem-ret.tmp (root-invoke-ret-mem funcs-ret (or summary? trigger-summary?)))
					(define mem-ret.tmp2 (if trigger-summary? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
					(define fml-sum (if trigger-summary? (memory-sym-summary mem-ret.tmp2 summary?) #t))

					(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
					(define ret-val (memory-sforce-read mem-ret var-ret-name 0))
					(define mem-pop (memory-spop (memory-spop mem-ret)))
					(define mem-ass (memory-sym-commit (memory-sforce-write mem-pop ret ret-val 0 
						(jtype->mtype (function-ret (function-formula-func func-invoked))))))
					(define fml-ret (memory-sym-summary mem-ass summary?))

					(define fml-op (select-fml? (and fml-this fml-in fml-sum fml-ret)))
					(define fml-new (iassert-pc-invoke fml-op (list func-fml-in) (list #t)))

					(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))))]

			[(inst-ass vl vr) 
				(begin
				(if (equal? vr (iexpr-var var-this-name)) (assert id) #f)
				(match-define (cons value jtype) (expr-eval vr mac-eval-ctxt))
				(define rhs (lexpr-rhs vl))
				(define mem-new 
					(match rhs
						[(expr-var v) (memory-sforce-write mem-0 (string-id (variable-name v)) value 0 (jtype->mtype jtype))]
						[(expr-array arr idx)
							(letrec
								([addr (memory-sforce-read mem-0 (string-id (variable-name arr)) 0)]
								[idx-e (ast->expression idx)]
								[idx-v (car (expr-eval idx-e mac-eval-ctxt))])
								(memory-awrite mem-0 addr idx-v value (jtype->mtype jtype)))]
						[(expr-field obj cls fname)
							(letrec
								([addr (if (equal? obj void-receiver) addr-void-receiver
									(memory-sforce-read mem-0 (string-id (variable-name obj)) 0))])
								(memory-fwrite mem-0 (vfield-id mac (string-id (type-name-name cls)) (string-id (field-name fname))) addr value (jtype->mtype jtype)))]))
				(defer-eval spec-id "new-v" value)
				(if summary? #f (add-spec id spec-id 
					(memory-sym-reset (memory-sym-new summary?) mem-in summary?)
					(memory-sym-reset (memory-sym-new summary?) (memory-sym-commit mem-new) summary?) 
					inst inst-ass? #f mark))
				(update-mem-only mem-new))]

			;[!]there might be bug if reading from heap
			[(inst-switch cnd cases default-l)
				(begin
				(set! mac-eval-ctxt (std:struct-copy machine mac-eval-ctxt [mem mem-in]))
				(define lmap (function-lmap func))
				(define cnd-v (car (expr-eval cnd mac-eval-ctxt)))
				(define default-pc (if default-l (imap-get lmap default-l default-type) (+ 1 pc)))

				(define cases-cnd (map (lambda (k.l)
						(cons (select-fml? (equal? (car k.l) cnd-v)) (imap-get lmap (cdr k.l) default-type)))
					cases))
				(define neg-cnd (foldl (lambda (cnd.pc fml) (and (not (car cnd.pc)) fml)) #t cases-cnd))
				(define cases-cnd+ (cons (cons neg-cnd default-pc) cases-cnd))

				(define cases-mark (map (lambda (k.l)
						(cons mark (imap-get lmap (cdr k.l) default-type)))
					cases))
				(define neg-mark (foldl (lambda (cnd.pc fml) (and (not (car cnd.pc)) fml)) #t cases-mark))
				(define cases-mark+ (cons (cons neg-mark default-pc) cases-mark))

				(define fml-new (iassert-pc-switch #t cases-cnd+))
				(update-rbstate-switch fml-new mem-in (if summary? cases-cnd+ cases-mark+)))]

			[(inst-jmp condition label)
				(begin
				(define mem-0+ (memory-sym-commit mem-0))
				(assert (memory-sym-summary mem-0+ summary?))
				(if (equal? (imap-get (function-lmap func) label default-type) (+ pc 1))
					(update-rbstate (iassert-pc-next #t #t) mem-0+ #f)
					(begin
					(define lmap (function-lmap func))
					(define cnd (car (expr-eval condition mac-eval-ctxt)))
;					(defer-eval spec-id "condition: " cnd)
					(defer-eval current-spec-id "cnd real & cnd expected: " (list cnd (not (next-mark))))
					(define fml-update #t)
					(define fml-new (iassert-pc-branch (select-fml? fml-update) cnd (not cnd) label))
;					(pretty-print fml-new)
;					(defer-eval current-spec-id "jmp encoding: " fml-new)
					(define pc-br (imap-get (function-lmap func) label default-type))
					(if summary? #f (add-spec id spec-id 
						(memory-sym-reset (memory-sym-new summary?) mem-in summary?)
						(memory-sym-reset (memory-sym-new summary?) mem-in summary?)
						inst inst-jmp? (not (next-mark)) mark))
					(if summary? 
						(update-rbstate-verbose fml-new mem-0+ pc-br (not cnd) cnd)
;						(update-rbstate-verbose fml-new mem-0+ pc-br (and mark (select-fml? (not cnd))) (and mark (select-fml? cnd)))))))]))]))
						(update-rbstate fml-new mem-0+ pc-br)))))]))]))





;========================== Local Spec ==========================

(struct local-spec (spec-id mem-in mem-out inst-ori inst-type take-branch? executed?) #:transparent)

;selector -> list of 
(define spec-map (imap-empty default-type))

(define (clear-specs!)
	(set! spec-map (imap-empty default-type)))
(register-reset! clear-specs! #t)

(define (add-spec id0 spec-id mem-in mem-out inst-ori inst-type take-branch? executed?)
	(define id (~a id0))
	(define cur (imap-get spec-map id default-type))
	(define cur+ (if (is-not-found? cur) null cur))
	(set! spec-map (imap-set spec-map id (cons (local-spec spec-id mem-in mem-out inst-ori inst-type take-branch? executed?) cur+) default-type)))

;ast: ast of entire program
;return: spec after this line
(define (step-spec-0 id0 mac inst sol-bad sol-good)
	(define id (~a id0))
	(define specs (imap-get spec-map id default-type))
	(step-spec specs mac inst sol-bad sol-good))

;ast: ast of entire program
;return: spec after this line
(define (step-spec specs mac inst sol-bad sol-good)
	(map (lambda (spec)
		(define sol (if (equal? (local-spec-spec-id spec) spec-id-good) sol-good sol-bad))
;			(match (local-spec-spec-id spec) 
;				[spec-id-good sol-good] 
;				[spec-id-bad sol-bad]))
		(if (not (evaluate (local-spec-executed? spec) sol)) spec
			(begin
			(define mac-spec (std:struct-copy machine mac [mem (evaluate (local-spec-mem-in spec) sol)]))
			(set-context! mac-spec)
			(define mac-post (inst-exec inst mac-spec #f))
			(std:struct-copy local-spec spec [mem-in (machine-mem mac-post)]))))
	specs))

;ast: ast of entire program
;[!] this is an incomplete check. it's based on the assumption that the patch only makes necessary changes
(define (sat-spec? id0 mac inst sol-bad sol-good)
	(define id (~a id0))
	(define specs (imap-get spec-map id default-type))
	(sat-spec-continue? specs mac inst sol-bad sol-good))

(define (sat-spec-continue? specs mac inst sol-bad sol-good)
	(andmap (lambda (spec-sym)
		(define sol (if (equal? (local-spec-spec-id spec-sym) spec-id-good) sol-good sol-bad))
;			(match (local-spec-spec-id spec-sym) 
;				[spec-id-good sol-good] 
;				[spec-id-bad sol-bad]))
		(if (not ((local-spec-inst-type spec-sym) inst)) #f
		(if (not (evaluate (local-spec-executed? spec-sym) sol)) #t
			(begin
			(define spec (evaluate spec-sym sol))
			(match spec [(local-spec spec-id mem-in mem-out inst-ori inst-type take-branch? executed?)
				(begin
				(define mac-spec (std:struct-copy machine mac [mem mem-in]))
				(pretty-print (list inst-ori inst-type take-branch?))
;				(define-symbolic* x integer?)
;				(pretty-print (memory-hread mem-in x integer?))
				(match inst 
					[(inst-jmp condition label) 
						(begin
						(define c (car (expr-eval condition mac-spec)))
						(pretty-print condition)
						(pretty-print (if (iexpr-binary? condition) (expr-eval (iexpr-binary-expr1 condition) mac-spec) #f))
						(pretty-print c)
						(equal? c take-branch?))]
					[(inst-ass vl vr)  
						(if (not (same-vl? inst-ori inst)) #f
							(begin
							(match-define (cons v-new v-new-jt) (expr-eval vr mac-spec))
							(pretty-print v-new)
							(match (lexpr-rhs vl)
								[(expr-var v) (equal? v-new (do-n-ret pretty-print (memory-sforce-read mem-out (string-id (variable-name v)) 0)))]
								[_ #f])))]
					[(inst-ret v-expr)  
						(begin
						(match-define (cons ret-value ret-jtype) (expr-eval v-expr mac-spec))
						(equal? ret-value (memory-sforce-read mem-out var-ret-name 0)))]))])))))
		specs))

(define (same-vl? inst1 inst2)
	(define vl1 (lexpr-rhs (inst-ass-vl inst1)))
	(define vl2 (lexpr-rhs (inst-ass-vl inst2)))
	(and 
		(expr-var? vl1) 
		(expr-var? vl2)
		(equal? 
			(string-id (variable-name (expr-var-name vl1)))
			(string-id (variable-name (expr-var-name vl2))))))
