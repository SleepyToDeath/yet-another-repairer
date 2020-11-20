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
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;============================= Debug ========================================
(define eval-pending null)

;============================= Definition ====================================
(struct function-formula (func lids lstates ret-lstate fmls vid sid) #:transparent)

;pmark : boolean value deciding whether this line is executed
;mem-out : memory symbol of this line after its execution
;mem-in-list : list of (cons pmark mem) where each pair is a possible predecessor
;				of this line. The mem-out with a true pmark will be chosen to be mem-in.
(struct lstate (pmark mem-in-list mem-out))

;============================= Top Level Interface ====================================
;ast ->  line ids(list of sym bool) X (input(list of key & value) -> output(list of key & value) -> relation)
(define (ast->relation ast)
	(define mac-raw (ast->machine ast))
	(define mac (build-virtual-table-alt (initialize-machine-encoding mac-raw)))

	(define soft-cons 
		(map (lambda (b) (if b 1 0)) 
			(apply append (map (lambda (func) (function-formula-lids func)) (all-functions mac)))))

	(define (hard-cons input output) 

		(define (assign-input mac input)
			(define mem0 (memory-spush (machine-mem mac)))
			(match-define (cons mem-ass fml-ass)
				(foldl 
					(lambda (kv mem+fml) 
						(define-symbolic* vi integer?)
						(define fml (equal? vi (cdr kv)))
						(cons 
							(memory-sforce-write (car mem+fml) (string-id (car kv)) vi) 
							(and (cdr mem+fml) fml)))
					(cons mem0 #t) input))
			(cons (std:struct-copy machine mac [mem mem-ass]) fml-ass))

		(define (compare-output mac output)
			(define mem0 (machine-mem mac))
			(foldl (lambda (kv fml-cur) (equal? (cdr kv) 
				(memory-sread mem0 (string-id (car kv)))
			)) #t output))

		(match-define (cons mac-ass fml-ass) (assign-input mac input))
		(define boot-lstate (alloc-lstate (machine-boot mac-ass)))
		(define all-invokes (invoke->relation boot-lstate mac-ass))
		(define mac-done (std:struct-copy machine mac-ass [mem (root-invoke-ret-mem all-invokes)]))
		(define fml-out (compare-output mac-done output))
		(define fml-code (andmap (lambda (func) (function-formula-fmls func)) all-invokes))
;		(display "\nfml-ass!\n")
;		(println fml-ass)
;		(display "\nfml-out!\n")
;		(println fml-out)
;		(display "\nfml-code!\n")
;		(println fml-code)
;		(println (get-lid boot-lstate 1))
		(define fml-boot-is-correct (apply and (function-formula-lids func-fml)))
		(and fml-boot-is-correct (starting-pmark boot-lstate) fml-ass fml-out fml-code))
;		(and (starting-pmark boot-lstate) fml-code fml-ass))

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


(define (build-virtual-table mac) 
	(define classes (machine-classes mac))
	(define (process-class cls mac)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
		(define vfields (class-vfields cls))

		(define mac-sfuncs (foldl 
			(lambda (sf mac) 
				(define sid (sfunc-id cls-name (function-name sf) (map cdr (function-args sf))))
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
				(define vid (vfunc-id-alt mac cls-name (function-name vf) (map cdr (function-args vf))))
				(define sid (sfunc-id cls-name (function-name vf) (map cdr (function-args vf))))
				(define mem-1 (memory-fdecl (machine-mem mac) vid)) 
				(define fmap-1 (imap-set (machine-fmap mac) sid vf))
				(std:struct-copy machine mac [mem mem-1] [fmap fmap-1]))
			mac-sfields vfuncs))

		(define mac-vfields (foldl 
			(lambda (vf mac) 
				(std:struct-copy machine mac 
					[mem (memory-fdecl (machine-mem mac) (vfield-id mac cls-name vf))])) 
			mac-vfuncs vfields))

		(println string-id-table)
		mac-vfields)

	(define mem-push (memory-spush (machine-mem mac)))
	(define mem-reserve-obj (cdr (memory-alloc mem-push vt-size)))
	(foldl process-class (std:struct-copy machine mac [mem mem-reserve-obj]) classes))





;----------------------- Allocation -------------------------
;funcion -> function-formula (with line id, pmark is empty)
(define (alloc-lid mac clsname func)
	(function-formula func 
		(map (lambda (any) (define-symbolic* line-id boolean?) line-id) (function-prog func))
		null
		#f
		#t
		(if mac (vfunc-id-alt mac clsname (function-name func) (map cdr (function-args func))) #f)
		(sfunc-id clsname (function-name func) (map cdr (function-args func)))))

;function-formula -> function-formula (with pmark)
(define (alloc-lstate func-fml)
	(std:struct-copy function-formula func-fml 
		[ret-lstate (lstate-new)]
		[lstates (map (lambda (any) (lstate-new)) (function-prog (function-formula-func func-fml)))]))

(define (lstate-new)
	(lstate 
		((lambda () (define-symbolic* path-mark-ret boolean?) path-mark-ret))
		null
		(memory-sym-new)))





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
	(std:struct-copy function-formula func-fml [ret-lstates st-new]))

;return a new func-fml
(define (prepend-mem-in func-fml pmark mem pc) 
	(define st-old (get-lstate func-fml pc))
	(define st-new (std:struct-copy lstate st-old [mem-in-list (cons (cons pmark mem) (lstate-mem-in-list st-old))]))
	(std:struct-copy function-formula func-fml [lstates (list-set (function-formula-lstates func-fml) pc st-new)]))

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
(struct rbstate (funcs pc func-fml mac) #:transparent)


; function X machine -> list of function-formula from all functions in `mac` transitively invoked by `func`
; "This function" is guaranteed to be at the beginning of the list.
(define (invoke->relation func-fml mac)
	(insts->relation func-fml mac))

; "This function" is guaranteed to be at the beginning of the list.
(define (root-invoke func-fmls)
	(car func-fmls))

(define (root-invoke-ret-mem func-fmls)
	(ormap (lambda (p+m) (if (car p+m) (cdr p+m) #f)) (lstate-mem-in-list (get-ending-lstate (root-invoke func-fmls)))))
	
(define (insts->relation func-fml mac)
	(match (foldl inst->relation
					(rbstate null pc-init func-fml mac) 
					(function-prog (function-formula-func func-fml)))
		[(rbstate funcs pc func-fml mac) 
				(cons func-fml funcs)]))



;instruction X rbstate -> rbstate
(define (inst->relation inst st)


	(match st [(rbstate funcs pc func-fml mac)
		(begin
		(define func (function-formula-func func-fml))
		(define mark (get-pmark func-fml pc))
		(define id (get-lid func-fml pc))

		(define mem-in (ormap (lambda (p+m) (if (car p+m) (cdr p+m) #f)) (get-mem-in-list func-fml pc)))
		(define mem-0 (memory-sym-reset (get-mem-out func-fml pc) mem-in))
		;used only for expr-eval
		(define mac-eval-ctxt (std:struct-copy machine mac [mem mem-0]))

	(display "\nInstruction:\n")
	(println inst)
	(println mark)
	(println id)


		(define (next-mark) 
			(get-pmark func-fml (+ 1 pc)))

		(define (label-mark label) 
			(define new-pc (imap-get (function-lmap func) label))
			(get-pmark func-fml new-pc))

		(define (select-fml? fml)
			(implies id fml))

		(define (iassert-pc-next fml-path fml-op)
			(and 
				fml-path
				(equal? mark (and fml-op (next-mark)))))

		(define (iassert-pc-ret fml-path fml-op)
			(and
				fml-path
				(equal? mark (and fml-op (function-formula-ret-pmark func-fml)))))

		(define (iassert-pc-branch cnd-t cnd-f label)
			(letrec ([fml-t (equal? cnd-t (label-mark label))]
					 [fml-f (equal? cnd-f (next-mark))]
					 [fml-cnd (and fml-t fml-f)]
					 [fml-br (or (label-mark label) (next-mark))]
					 [fml-path (equal? mark (and fml-cnd fml-br))])
					fml-path))

		(define (iassert-pc-invoke fml-path fml-ops func-fmls cnds)
			(letrec	([fml-cnds (andmap 
							(lambda (func-fml fml-op cnd)
								(and 
									(equal? cnd (starting-pmark func-fml))
									(implies cnd fml-op))) 
							func-fmls fml-ops cnds)]
					 [fml-brs (ormap ending-pmark func-fmls)])
					(and
						fml-path
						(equal? mark (and fml-cnds fml-brs (next-mark))))))

		(define (invoke-setup func-fml-callee mem args)
			(define mem-0 (memory-sym-reset (memory-sym-new) mem))
			(define func (function-formula-func func-fml-callee))
			(define mem-push (memory-spush mem-0))
			(define mem-decl
				(foldl 
					(lambda (var-def mem) (memory-sdecl mem (string-id (car var-def)))) 
					mem-push
					(append (function-args func) (function-locals func))))
			(define mem-input 
				(foldl 
					(lambda (arg-src arg-dst mem) (memory-swrite mem (string-id (car arg-dst)) (expr-eval arg-src mac-eval-ctxt)))
					mem-decl
					args 
					(function-args func)))
			(define func-fml-in (prepend-starting-mem-in func-fml-callee mark mem-input))
			(define fml-in (memory-sym-get-fml mem-input))
			(cons func-fml-in fml-in))

		; bool X memory X int(if with branch)/#f(if no branch) -> rbstate
		(define (update-rbstate fml-new mem-out pc-opt-br)
			(define pc-next (+ 1 pc))
			(define func-fml-next (prepend-mem-in func-fml mark mem-out pc-next))
			(define func-fml-br (if pc-opt-br (prepend-mem-in func-fml-next mark mem-out pc-opt-br) func-fml-next))
			(define func-fml-new (append-fml func-fml-br fml-new))
			(std:struct-copy rbstate st [pc pc-next] [func-fml func-fml-new]))

		(define (update-mem-only mem-new)
			(define fml-new (iassert-pc-next #t (select-fml? (memory-sym-get-fml mem-new))))
			(update-rbstate fml-new mem-new #f))




		(match inst 
			[(inst-nop _) 
				(update-rbstate (iassert-pc-next #t #t) mem-0 #f)]

			[(inst-init classname)
				(begin
				(define addr (memory-sread mem-0 var-this-name))
				(define mem-bind-func
					(foldl
						(lambda (func-fml-cur mem) 
							(begin
							(define vid (function-formula-vid func-fml-cur))
							(define sid (function-formula-sid func-fml-cur))
							(if (is-not-found? (memory-fread mem vid addr))
								(memory-fwrite mem vid addr sid)
								mem))
						mem-0
						(class-vfuncs (imap-get (machine-cmap mac) classname)))))
				(update-mem-only mem-bind-func))]

			[(inst-new v-name) 
				(begin
				(match-define (cons addr mem-alloc) (memory-new mem-0))
				(define mem-ass (memory-swrite mem-alloc v-name addr))
				(update-mem-only mem-ass))]

			[(inst-ret v-expr) 
				(begin
				(define ret-value (expr-eval v-expr mac-eval-ctxt))
				(define mem-ret (memory-sforce-write mem-0 var-ret-name ret-value))
				(define fml-ret (select-fml? (memory-sym-get-fml mem-ret)))
				(define fml-path (iassert-pc-ret fml-write fml-new))
				(define func-fml-ret (prepend-ending-mem-in func-fml mark mem-ret))
				(define func-fml-new (append-fml func-fml-ret fml-path))
				(std:struct-copy rbstate st [pc pc-ret] [func-fml func-fml-new]))]

			[(inst-static-call ret cls-name func-name arg-types args) 
				(begin
				(define sid (sfunc-id cls-name func-name arg-types))
				(define func-invoked (alloc-lstate (imap-get (machine-fmap mac) sid)))
				(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-in args))
				(define funcs-ret (invoke->relation func-fml-in mac))

				(define mem-ret (memory-sym-reset mem-0 (root-invoke-mem-ret funcs-ret)))
				(define ret-val (memory-sread mem-ret var-ret-name))
				(define mem-pop (memory-spop mem-ret))
				(define mem-ass (memory-swrite mem-pop ret ret-val))
				(define fml-ret (memory-sym-get-fml mem-ass))

				(define fml-op (select-fml? (and fml-in fml-ret)))
				(define fml-new (iassert-pc-invoke #t (list fml-op) (list func-fml-in) (list #t)))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (append funcs funcs-ret)]))]

			[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define mem--1 (memory-sym-reset (memory-sym-new) mem-in))
				(define obj-addr (memory-sread mem--1 obj-name))
				(define vid (vfunc-id-alt mac cls-name func-name arg-types))
				(define funcs-invoked (map alloc-lstate (filter (lambda (f) (equal? (function-formula-vid f) vid)) (all-vfunctions mac))))
				(define true-func-invoked-sid (memory-fread mem--1 vid obj-addr))
				(define func-invoked (car (filter 
					(lambda (f) (equal? (function-formula-sid f) true-func-invoked-sid)))
					funcs-invoked))

				;push an extra scope to avoid overwriting "this" of the current scope
				(define mem-this (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr))
				(define fml-this (memory-sym-get-fml mem-this))

				(define mem-ret (memory-sym-reset mem-0 (root-invoke-mem-ret funcs-ret)))
				(define ret-val (memory-sread mem-ret var-ret-name))
				(define mem-pop (memory-spop (memory-spop mem-ret)))
				(define mem-ass (memory-swrite mem-pop ret ret-val))
				(define fml-ret (memory-sym-get-fml mem-ass))

				(define fml-op (select-fml? (and fml-this fml-in fml-ret)))
				(define fml-new (iassert-pc-invoke #t (list fml-op) (list func-fml-in) (list #t)))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (append funcs funcs-ret)]))]

;			(define uncertain-rets (map 
;				(lambda (f) 
;					(match-define (cons func-fml-in fml-in) (invoke-setup f mem-this args))
;					(define funcs-ret (invoke->relation func-fml-in mac))
;
;					(define mem-ret (memory-sym-reset (memory-sym-new) (root-invoke-mem-ret funcs-ret)))
;					(define ret-value (memory-sread mem-ret var-ret-name))
;					(define mem-pop (memory-spop (memory-spop mem-ret)))
;					(define mem-ass (memory-swrite mem-pop ret ret-value))
;					(define fml-ret (memory-sym-get-fml mem-ass))
;
;					(list funcs-ret mem-ass (and fml-ret fml-in) (equal? (function-formula-sid f) true-func-invoked-sid)))
;				funcs-invoked))
;			
;			(match-define (list funcs-ret mem-ass fml-ret cnd) (car (filter cadddr uncertain-rets)))
;
;			(define fml-rets (map caddr uncertain-rets))
;			(define cnds (map cadddr uncertain-rets))
;
;			(define pc-next (+ 1 pc))
;			(define mac-new (std:struct-copy machine mac [mem mem-ass]))
;			(define fml-new (iassert-pc-invoke #t fml-rets funcs-invoked cnds))
;
;			(std:struct-copy rbstate st [funcs (append funcs funcs-ret)] [mac mac-new] [pc pc-next] [fml (and fml fml-new)]))]

			[(inst-special-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define mem--1 (memory-sym-reset (memory-sym-new) mem-in))
				(define obj-addr (memory-sread mem--1 obj-name))
				(define sid (sfunc-id cls-name func-name arg-types))
				(define func-invoked (alloc-lstate (imap-get (machine-fmap m) sid)))

				;push an extra scope to avoid overwriting "this" of the current scope
				(define mem-this (memory-sforce-write (memory-spush mem--1) var-this-name obj-addr))
				(define fml-this (memory-sym-get-fml mem-this))

				(match-define (cons func-fml-in fml-in) (invoke-setup func-invoked mem-this args))
				(define funcs-ret (invoke->relation func-fml-in mac))

				(define mem-ret (memory-sym-reset mem-0 (root-invoke-mem-ret funcs-ret)))
				(define ret-val (memory-sread mem-ret var-ret-name))
				(define mem-pop (memory-spop (memory-spop mem-ret)))
				(define mem-ass (memory-swrite mem-pop ret ret-val))
				(define fml-ret (memory-sym-get-fml mem-ass))

				(define fml-op (select-fml? (and fml-this fml-in fml-ret)))
				(define fml-new (iassert-pc-invoke #t (list fml-op) (list func-fml-in) (list #t)))

				(std:struct-copy rbstate (update-rbstate fml-new mem-ass #f) [funcs (append funcs funcs-ret)]))]

			[(inst-ass vl vr) 
				(begin
				(define value (expr-eval vr mac-eval-ctxt))
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
				(update-mem-only mem-new))]

			[(inst-jmp condition label)
				(begin
				(define lmap (function-lmap func))
				(define value (expr-eval condition mac-eval-ctxt))
				(define fml-new (iassert-pc-branch (select-fml? value) (select-fml? (not value)) label))
				(define pc-br (imap-get (function-lmap func) label))
				(update-rbstate fml-new mem-0 pc-br))]

