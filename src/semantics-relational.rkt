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
(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))
(require racket/format)
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;============================= Definition ====================================
(struct function-formula (func lids lstates ret-lstate fmls vid sid) #:transparent)

;pmark : boolean value deciding whether this line is executed
;mem-out : memory symbol of this line after its execution
;mem-in-list : list of (cons pmark mem) where each pair is a possible predecessor
;				of this line. The mem-out with a true pmark will be chosen to be mem-in.
(struct lstate (pmark mem-in-list mem-out) #:transparent)

;cnd: the entire subtree is considered only if cnd is true
(struct invoke-tree (root cnd subtrees) #:transparent)
;============================= Top Level Interface ====================================
;ast ->  line ids(list of sym bool) X (input(list of key & value) -> output(list of key & value) -> relation)
(define (ast->relation ast)
	(define mac-raw (ast->machine ast))
	(define mac (initialize-machine-encoding mac-raw))

	(define soft-cons 
		(map (lambda (b) (if b 1 0)) 
			(apply append (map (lambda (func) (function-formula-lids func)) (all-functions mac)))))

	(define (hard-cons input output) 

		(set! imap-dummy-list null)

		(define (assign-input mac input)
			(define mem0 (machine-mem mac))
			(define mem-push (memory-spush mem0))
			(match-define (cons mem-ass fml-ass)
				(foldl 
					(lambda (kv mem+fml) 
						(define-symbolic* vi integer?)
						(define fml (equal? vi (cdr kv)))
						(cons 
							(memory-sforce-write (car mem+fml) (string-id (car kv)) vi) 
							(and (cdr mem+fml) fml)))
					(cons mem-push #t) input))
			(define mem-ret (memory-sdecl mem-ass var-ret-name))
			(cons (std:struct-copy machine mac [mem mem-ret]) fml-ass))

		(define (compare-output mac output)
			(define mem0 (machine-mem mac))
			(andmap 
				(lambda (kv) (equal? (cdr kv) (memory-sread mem0 (string-id (car kv)))))
				output))

		(match-define (cons mac-ass0 fml-ass) (assign-input mac input))
		(define mac-ass (build-virtual-table-alt mac-ass0))
		(define boot-lstate (prepend-starting-mem-in (alloc-lstate (machine-boot mac-ass)) #t (machine-mem mac-ass)))
		(display "\n ###############################################0 \n")
		(define all-invokes (invoke->relation boot-lstate mac-ass 59 #f))
		(display "\n ###############################################1 \n")
		(define mac-done (std:struct-copy machine mac-ass [mem (root-invoke-ret-mem all-invokes)]))
		(display "\n ###############################################4 \n")
		(define fml-out (compare-output mac-done output))
		(display "\n ###############################################5 \n")

		(define (extract-fml itree) 
			(match itree
				[(invoke-tree root cnd subs)
					(and 
						(function-formula-fmls root) 
						(andmap extract-fml subs))]))

		(define fml-code (extract-fml all-invokes))

		(display "\n ###############################################6 \n")
		(define fml-boot-is-correct (andmap identity (function-formula-lids boot-lstate)))
		(display "\n ###############################################7 \n")

		(define all-keys (imap-sym-tracked-keys (memory-addr-space (machine-mem mac-done))))
		(pretty-print (~a "Totally " (length all-keys) " keys"))

		(define (id2keys id)
			(map car (imap-sym-committed-updates (vector-ref imap-dummy2map id))))

		(define (contain-key? id key)
			(ormap (lambda (key0) (equal? key key0)) (id2keys id)))

		(display "\n ###############################################7.1 \n")

		(pretty-print (~a (length imap-dummy-list) " states:"))
		(pretty-print imap-dummy-list)

		(define fml-maybe-wrong
			(andmap (lambda (mem-id)
				(define mem (vector-ref imap-dummy2map mem-id))
				(define fml-true 
					(andmap (lambda (key) 
							(if (is-concrete-value key)
								(imap-sym-key-fml mem key)
								((lambda () 
									(define-symbolic* key-sym integer?)
									(and 
										(equal? key-sym key)
										(imap-sym-key-fml mem key-sym))))))
						(id2keys mem-id)))
				(define fml-deferred (imap-sym-fml-deferred mem))
				(equal? fml-deferred fml-true))
				imap-dummy-list))
#|
		(map (lambda (mem-id)
			(define mem (vector-ref imap-dummy2map mem-id))
			(defer-eval "^^^^^^^^^ content of formula ^^^^^^^^^" (imap-sym-fml-deferred mem))
			(map (lambda (key)
					(defer-eval "maybe-wrong-key: " (cons mem-id key))
					(imap-sym-key-fml-debug mem key))
				(id2keys mem-id)))
			imap-dummy-list)
|#
		

		(display "\n ###############################################7.2 \n")

		(define fml-always-right
			(andmap (lambda (mem-id)
					(define mem (vector-ref imap-dummy2map mem-id))
					(pretty-print (~a "Generate keys for state #" mem-id))
					(imap-sym-lookback mem)
					(andmap (lambda (key+id) 
							(if (contain-key? mem-id (car key+id)) #t
								(if (is-concrete-value (car key+id))
									(imap-sym-key-fml mem (car key+id))
									((lambda () 
										(define-symbolic* key-sym integer?)
										(and 
											(equal? key-sym (car key+id))
											(imap-sym-key-fml mem key-sym)))))))
						all-keys))
				imap-dummy-list))

#|
		(map (lambda (mem-id)
			(define mem (vector-ref imap-dummy2map mem-id))
			(map (lambda (key.id)
					(defer-eval "always-correct-key: " (cons mem-id (if (contain-key? mem-id (car key.id)) "xxxxxxxx" (car key.id))))
					(imap-sym-key-fml-debug mem (car key.id)))
				all-keys))
			imap-dummy-list)
|#

		(display "\n ###############################################7.3 \n")
		
		(define fml-code-bind (and fml-maybe-wrong fml-always-right))

		(display "\n ###############################################8 \n")
		(pretty-print fml-out)
		(and fml-boot-is-correct (starting-pmark boot-lstate) fml-ass fml-out fml-code fml-code-bind))
;		(and fml-boot-is-correct (starting-pmark boot-lstate) fml-code fml-ass fml-code-bind))

	(cons soft-cons hard-cons))


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
		[boot (alloc-lid #f "dummy" (machine-boot mac))] 
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

;(define (build-virtual-table-alt mac) 
;	(define classes (machine-classes mac))
;	(define (process-class cls mem)
;		(define cls-name (class-name cls))
;
;		(define sfuncs (class-sfuncs cls))
;		(define vfuncs (class-vfuncs cls))
;		(define sfields (class-sfields cls))
;		(define vfields (class-vfields cls))
;
;		(define mem-sfuncs (foldl (lambda (sf-fml mem) (memory-sforce-write mem 
;			(sfunc-id cls-name (function-name (function-formula-func sf-fml)) (map cdr (function-args (function-formula-func sf-fml)))) sf-fml)) mem sfuncs))
;		(define mem-sfields (foldl (lambda (sf mem) (memory-sdecl mem (sfield-id cls-name sf))) mem-sfuncs sfields))
;
;		(define mem-vfuncs (foldl (lambda (vf-fml mem) (memory-fdecl mem 
;			(vfunc-id-alt mac cls-name (function-name (function-formula-func vf-fml)) (map cdr (function-args (function-formula-func vf-fml)))))) mem-sfields vfuncs))
;		(define mem-vfields (foldl (lambda (vf mem) (memory-fdecl mem (vfield-id mac cls-name vf))) mem-vfuncs vfields))
;
;		(println string-id-table)
;		mem-vfields)
;
;	(define mem-push (memory-spush (machine-mem mac)))
;	(define mem-reserve-obj (cdr (memory-alloc mem-push vt-size)))
;	(std:struct-copy machine mac [mem (foldl process-class mem-reserve-obj classes)]))


(define (build-virtual-table-alt mac) 
	(define classes (machine-classes mac))
	(define (process-class cls mac)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
		(define vfields (cons field-name-class (class-vfields cls)))

		(define mac-sfuncs (foldl 
			(lambda (sf mac) 
				(define sid (sfunc-id cls-name (function-name (function-formula-func sf)) (map cdr (function-args (function-formula-func sf)))))
				(pretty-print (machine-mem mac))
				(define mem-1 (memory-sforce-write (machine-mem mac) sid sid))
				(define fmap-1 (imap-set (machine-fmap mac) sid sf))
				(std:struct-copy machine mac [mem mem-1] [fmap fmap-1]))
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

	(define mem-push (memory-spush (machine-mem mac)))
	(define mac-cls (foldl process-class (std:struct-copy machine mac [mem mem-push]) classes))
	(define mem-reserve-obj (cdr (memory-alloc (machine-mem mac-cls) vt-size)))
	(std:struct-copy machine mac-cls [mem mem-reserve-obj]))





;----------------------- Allocation -------------------------
;funcion -> function-formula (with line id, pmark is empty)
(define (alloc-lid mac clsname func)
	(function-formula func 
		(map (lambda (any) (define-symbolic* line-id boolean?) line-id) (function-prog func))
		null
		#f
		#t
		(if mac (vfunc-id mac clsname (function-name func) (map cdr (function-args func))) #f)
		(sfunc-id clsname (function-name func) (map cdr (function-args func)))))

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
(define (prepend-mem-in func-fml pmark mem pc) 
	(define st-old (get-lstate func-fml pc))
	(define st-new (std:struct-copy lstate st-old [mem-in-list (cons (cons pmark mem) (lstate-mem-in-list st-old))]))
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
(struct rbstate (funcs pc func-fml mac target-sid summary?) #:transparent)


; function X machine -> invoke-tree of function-formula from all functions in `mac` transitively invoked by `func`
; if summary? then this will compute a summary (root-invoke-ret-mem will contain all updates)
; otherwise it is guaranteed to encode this invoked function (but may not recursively do so)
(define (invoke->relation func-fml mac target-sid summary?)
	(display (~a "sid for invoked function is: " (function-formula-sid func-fml) "\n"))
	(display "\n ###############################################2 \n")
	(define ret (insts->relation func-fml mac target-sid summary?))
	(display "\n ###############################################3 \n")
	ret)

; "This function" is guaranteed to be at the beginning of the list.
(define (root-invoke itree)
	(invoke-tree-root itree))

(define (root-invoke-ret-mem itree)
	(memory-select (lstate-mem-in-list (ending-lstate (root-invoke itree)))))
	
(define (insts->relation func-fml mac target-sid summary?)
	(match (foldl inst->relation
					(rbstate null pc-init func-fml mac target-sid summary?) 
					(function-prog (function-formula-func func-fml)))
		[(rbstate funcs pc func-fml mac sid sum?) 
				(invoke-tree func-fml #t funcs)]))

(define (inst->relation inst st)
	(define ret (inst->relation.real inst st))
	(display "\n updated pc:\n")
	(pretty-print (rbstate-pc ret))
	ret)

;instruction X rbstate -> rbstate
(define (inst->relation.real inst st)


	(match st [(rbstate funcs pc func-fml mac target-sid summary?)
		(begin
		(define func (function-formula-func func-fml))
		(define mark (get-pmark func-fml pc))
		(define id (get-lid func-fml pc))
		(define in-target? (and (not summary?) (equal? (function-formula-sid func-fml) target-sid)))
;		(imap-set-selector id)

		(defer-eval "instruction: " inst)
		(display "\nInstruction:\n")
		(println inst)
		(println mark)
		(println id)
		(display (~a "In Target? " (if in-target? "++++++++++++"  "------------") "\n"))
		(display (~a "Summary? " (if summary? "++++++++++++"  "------------") "\n"))

		(define mem-in (memory-select (get-mem-in-list func-fml pc)))
		(define mem-0 (memory-sym-reset (get-mem-out func-fml pc) mem-in summary?))
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
				(display (~a "selector application: " (implies id fml) "\n"))
				(implies id fml))
				fml))

		(define (iassert-pc-next fml-path fml-op)
			(if summary? #t
				(and 
					fml-path
					(equal? mark (and fml-op (next-mark))))))

		(define (iassert-pc-ret fml-path fml-op)
			(if summary? #t
				(and
					fml-path
					(equal? mark (and fml-op (ending-pmark func-fml))))))

		(define (iassert-pc-branch fml-op cnd-t cnd-f label)
			(if summary? #t
				(letrec ([fml-t (equal? cnd-t (label-mark label))]
						 [fml-f (equal? cnd-f (next-mark))]
						 [fml-cnd (and fml-t fml-f)]
						 [fml-br (or (label-mark label) (next-mark))]
						 [fml-path (equal? mark (and fml-cnd fml-br fml-op))])
						fml-path)))

		(define (iassert-pc-invoke fml-path fml-ops func-fmls cnds)
			(if summary? #t
				(letrec	([fml-cnds (andmap 
								(lambda (func-fml fml-op cnd)
									(and 
										(equal? cnd (starting-pmark func-fml))
										(implies cnd fml-op))) 
								func-fmls fml-ops cnds)]
						 [fml-brs (ormap starting-pmark func-fmls)])
						(and
							fml-path
							(equal? mark (and fml-cnds fml-brs (next-mark)))))))

		(define (invoke-setup func-fml-callee mem args)
;			(memory-print mem)
			(define mem-0 (memory-sym-reset (memory-sym-new summary?) mem summary?))
			(memory-print-id "mem-0" mem-0)
			(define func (function-formula-func func-fml-callee))
			(define mem-push (memory-spush mem-0))
			(memory-print-id "mem-push" mem-push)
			(define mem-decl (memory-sym-commit
				(foldl 
					(lambda (var-def mem) (memory-sdecl mem (car var-def))) 
					mem-push
					(append (function-args func) (function-locals func)))))
			(memory-print-id "mem-decl" mem-decl)
			(define mem-arg (memory-sym-commit
				(foldl 
					(lambda (arg-src arg-dst mem) (memory-swrite mem (car arg-dst) (expr-eval arg-src mac-eval-ctxt)))
					mem-decl
					args 
					(function-args func))))
			(memory-print-id "mem-arg" mem-arg)
			(define fml-in (memory-sym-get-fml mem-arg summary?))
			(define mem-input (memory-sym-reset (memory-sym-new summary?) mem-arg (not in-target?)))
			(memory-print-id "mem-input" mem-input)
			(define func-fml-in (prepend-starting-mem-in func-fml-callee mark mem-input))
			(cons func-fml-in fml-in))

		; bool X memory X int(if with branch)/#f(if no branch) -> rbstate
		(define (update-rbstate fml-new mem-out pc-opt-br)
			(define pc-next (+ 1 pc))
			(define func-fml-next (prepend-mem-in func-fml mark mem-out pc-next))
			(define func-fml-br (if pc-opt-br (prepend-mem-in func-fml-next mark mem-out pc-opt-br) func-fml-next))
			(define func-fml-new (append-fml func-fml-br fml-new))
			(pretty-print inst)
			(display (~a "Output state id: " (imap-sym-func-dummy (imap-sym-tracked-imap (memory-addr-space mem-out))) " \n"))
			(std:struct-copy rbstate st [pc pc-next] [func-fml func-fml-new]))

		(define (update-mem-only mem-new)
			(define mem-commit (memory-sym-commit mem-new))
			(define fml-update (memory-sym-get-fml mem-commit summary?))
			(define fml-new (iassert-pc-next #t (select-fml? fml-update)))
			(update-rbstate fml-new mem-commit #f))

		(define (add-invoke-condition itree cnd)
			(std:struct-copy invoke-tree itree [cnd cnd]))

		(define (vid2sid mac classname vid)
			(if classname
				(begin
				(define maybe-this-sid 
					(ormap 
						(lambda (func-fml-cur)
							(if (equal? vid (function-formula-vid func-fml-cur)) (function-formula-sid func-fml-cur) #f))
						(class-vfuncs (imap-get (machine-cmap mac) classname))))
				(if maybe-this-sid maybe-this-sid 
					(begin
					(define cls-0 (imap-get (machine-cmap mac) classname))
					(define base-sid (ormap 
						(lambda (cls-cur) (vid2sid mac cls-cur vid)) 
						(cons (class-extend cls-0) (class-implements cls-0))))
					(if base-sid base-sid #f))))
			#f))

		(match inst 
			[(inst-nop _) 
				(update-rbstate (iassert-pc-next #t #t) mem-in #f null)]

			[(inst-init classname)
				(begin
				(define addr (memory-sread mem-0 var-this-name))
				(define mem-bind (memory-fwrite mem-0 field-name-class addr classname))
				(update-mem-only mem-bind))]

			[(inst-new v-name) 
				(begin
				(match-define (cons addr mem-alloc) (memory-new mem-0))
				(define mem-ass (memory-swrite mem-alloc v-name addr))
				(update-mem-only mem-ass))]

			[(inst-ret v-expr) 
				(begin
				(define ret-value (expr-eval v-expr mac-eval-ctxt))
				(if summary? #f (defer-eval inst ret-value))
				(define mem-ret (memory-sym-commit (memory-sforce-write mem-0 var-ret-name ret-value)))
				(define fml-update (memory-sym-get-fml mem-ret summary?))
				(define fml-ret (select-fml? fml-update))
				(define fml-path (iassert-pc-ret #t fml-ret))
				(define func-fml-ret (prepend-ending-mem-in func-fml mark mem-ret))
				(define func-fml-new (append-fml func-fml-ret fml-path))
				(pretty-print inst)
				(display (~a "Output state id: " (imap-sym-func-dummy (imap-sym-tracked-imap (memory-addr-space mem-ret))) " \n"))
				(std:struct-copy rbstate st [pc (+ 1 pc)] [func-fml func-fml-new]))]

			[(inst-static-call ret cls-name func-name arg-types args) 
				(begin
				(define sid (sfunc-id cls-name func-name arg-types))
				(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid)))
				(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-in args))
				(define funcs-ret (invoke->relation func-fml-in mac target-sid (or summary? in-target?)))

				(define mem-ret.tmp (root-invoke-ret-mem funcs-ret))
				(define mem-ret.tmp2 (if in-target? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
				(define fml-sum (if in-target? (memory-sym-get-fml mem-ret.tmp2 summary?) #t))

				(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
				(define ret-val (memory-sread mem-ret var-ret-name))
				(if summary? #f (defer-eval inst ret-val))
				(define mem-pop (memory-spop mem-ret))
				(define mem-ass (memory-sym-commit (memory-swrite mem-pop ret ret-val)))
				(define fml-ret (memory-sym-get-fml mem-ass summary?))

				;[TODO] use an extra selector for fml-sum
				(define fml-op (select-fml? (and fml-in fml-sum fml-ret)))
				;(define fml-op (and fml-in fml-ret))
				(define fml-new (iassert-pc-invoke #t (list fml-op) (list func-fml-in) (list #t)))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))]

			[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define mem--1 (memory-sym-reset (memory-sym-new summary?) mem-in summary?))
				(define obj-addr (memory-sread mem--1 obj-name))
				(define vid (vfunc-id-alt mac cls-name func-name arg-types))
				(define funcs-invoked (map alloc-lstate (filter (lambda (f) (equal? (function-formula-vid f) vid)) (all-vfunctions mac))))
				(define classname-true (memory-fread mem--1 field-name-class obj-addr))
				(define true-func-invoked-sid (vid2sid mac classname-true vid))
			;	(display (~a "true vid: " vid " true sid: " (vid2sid mac 11 vid) " true sid found: " true-func-invoked-sid " true class name:\n")) 
			;	(pretty-print (fml-to-print classname-true))


				;(define func-invoked (car (filter 
				;	(lambda (f) (equal? (function-formula-sid f) true-func-invoked-sid))
				;	funcs-invoked)))

				;push an extra scope to avoid overwriting "this" of the current scope
				(define mem-this (memory-sym-commit (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr)))
				(define fml-this (memory-sym-get-fml mem-this summary?))
				(memory-print-id "mem-this" mem-this)

				(define (invoke-candidate fcan)
					(begin
					(match-define (cons func-fml-in fml-in) (invoke-setup fcan mem-this args))
					;an invoke tree without condition
					(define funcs-ret (invoke->relation func-fml-in mac target-sid (or summary? in-target?)))

					(define mem-ret.tmp (root-invoke-ret-mem funcs-ret))
					(define mem-ret.tmp2 (if in-target? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
					(define fml-sum (if in-target? (memory-sym-get-fml mem-ret.tmp2 summary?) #t))
					(memory-print-id "mem-ret.tmp2" mem-ret.tmp2)

					(define mem-ret (memory-sym-reset (if summary? mem-0 (memory-sym-new summary?)) mem-ret.tmp2 summary?))
					(define ret-val (memory-sread mem-ret var-ret-name))
					(if summary? #f (defer-eval inst ret-val))
					(define mem-pop (memory-spop (memory-spop mem-ret)))
					(define mem-ass (memory-sym-commit (memory-swrite mem-pop ret ret-val)))
					(define fml-ret (memory-sym-get-fml mem-ass summary?))
					(memory-print-id "mem-ass" mem-ass)
					(define cnd (equal? (function-formula-sid fcan) true-func-invoked-sid))
;					(display (~a "this function sid: " (function-formula-sid fcan) " true sid: " true-func-invoked-sid "\n"))
					(list func-fml-in cnd fml-in mem-ass fml-ret funcs-ret fml-sum)))
				
				(define ret-pack (map invoke-candidate funcs-invoked))

				(define func-fml-ins (map first ret-pack))
				(define cnds (map second ret-pack))
;				(display "conditions:\n")
;				(pretty-print cnds)
				(defer-eval "Virtual call conditions:" cnds)
				;[TODO] use an extra selector for fml-sum
				(define fml-call (and
					(andmap third ret-pack)
					(andmap fifth ret-pack)
					(andmap seventh ret-pack)))
				(define mem-ass (memory-select (map cons cnds (map fourth ret-pack))))
				(define funcs-ret (map sixth ret-pack))

				(define fml-op (select-fml? (and fml-this fml-call)))
				(define fml-new (iassert-pc-invoke #t (list fml-op) func-fml-ins cnds))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (append funcs-ret funcs)]))]

			[(inst-special-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define mem--1 (memory-sym-reset (memory-sym-new summary?) mem-in summary?))
				(define obj-addr (memory-sread mem--1 obj-name))
				(define sid (sfunc-id cls-name func-name arg-types))
				(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid)))

				;push an extra scope to avoid overwriting "this" of the current scope
				(define mem-this (memory-sym-commit (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr)))
				(define fml-this (memory-sym-get-fml mem-this summary?))

				(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-this args))
				(define funcs-ret (invoke->relation func-fml-in mac target-sid (or in-target? summary?)))

				(define mem-ret.tmp (root-invoke-ret-mem funcs-ret))
				(define mem-ret.tmp2 (if in-target? (memory-sym-commit mem-ret.tmp) mem-ret.tmp))
				(define fml-sum (if in-target? (memory-sym-get-fml mem-ret.tmp2 summary?) #t))

				(define mem-ret (memory-sym-reset mem-0 mem-ret.tmp2 summary?))
				(define ret-val (memory-sread mem-ret var-ret-name))
				(if summary? #f (defer-eval inst ret-val))
				(define mem-pop (memory-spop (memory-spop mem-ret)))
				(define mem-ass (memory-sym-commit (memory-swrite mem-pop ret ret-val)))
				(define fml-ret (memory-sym-get-fml mem-ass summary?))

				;[TODO] use an extra selector for fml-sum
				(define fml-op (select-fml? (and fml-this fml-in fml-sum fml-ret)))
				;(define fml-op (and fml-this fml-in fml-ret))
				(define fml-new (iassert-pc-invoke #t (list fml-op) (list func-fml-in) (list #t)))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (cons funcs-ret funcs)]))]

			[(inst-ass vl vr) 
				(begin
				(define value (expr-eval vr mac-eval-ctxt))
				(if summary? #f (defer-eval inst value))
;				(defer-cons (equal? value 6))
				(define rhs (lexpr-rhs vl))
				(define mem-new 
					(match rhs
						[(expr-var v) (memory-swrite mem-0 (string-id (variable-name v)) value)]
						[(expr-array arr idx)
							(letrec
								([addr (memory-sread mem-0 (string-id (variable-name arr)))]
								[idx-e (ast->expression idx)]
								[idx-v (expr-eval idx-e mac-eval-ctxt)])
								(memory-awrite mem-0 addr idx-v value))]
						[(expr-field obj cls fname)
							(if (equal? obj void-receiver)
								(memory-swrite mem-0 (sfield-id (string-id (type-name-name cls)) (string-id (field-name fname))) value)
								(letrec
									([addr (memory-sread mem-0 (string-id (variable-name obj)))])
									(memory-fwrite mem-0 (vfield-id mac (string-id (type-name-name cls)) (string-id (field-name fname))) addr value)))]))
				(memory-print-id "mem-new" mem-new)
				(update-mem-only mem-new))]

			[(inst-jmp condition label)
				(begin
				(define lmap (function-lmap func))
				(define value (expr-eval condition mac-eval-ctxt))
				(define fml-update #t)
				(define fml-new (iassert-pc-branch (select-fml? fml-update) (select-fml? value) (select-fml? (not value)) label))
				(define pc-br (imap-get (function-lmap func) label))
				(update-rbstate fml-new mem-in pc-br))]))]))

