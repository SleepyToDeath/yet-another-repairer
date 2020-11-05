#lang rosette/safe

(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require "match-define.rkt")
(require "semantics-computational.rkt")
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;============================= Definition ====================================
(struct function-formula (func lids pmarks ret-pmark fmls) #:transparent)

(define (print-func-fml func-fml)
	(match func-fml
		[(function-formula func lids pmarks ret-pmark fmls)
		 (begin
		 	(println lids)
		 	(println pmarks)
		 	(println ret-pmark)
		 	(println fmls)
		 	(print-func func ""))]))

;============================= Top Level Interface ====================================
;ast ->  line ids(list of sym bool) X (input(list of key & value) -> output(list of key & value) -> relation)
(define (ast->relation ast)
	(define mac-raw (ast->machine ast))
	(define mac (build-virtual-table-alt (initialize-machine-encoding mac-raw)))

	(define soft-cons 
		(map (lambda (b) (if b 1 0)) 
			(apply append (map (lambda (func) (function-formula-lids func)) (all-functions mac)))))

	;[TODO] add input output
	(define (hard-cons input output) 

		(define (assign-input mac input)
			(define mem0 (memory-spush (machine-mem mac)))
			(match-define (cons mem-ass fml-ass)
				(foldl 
					(lambda (kv mem+fml) 
						(define-symbolic* vi integer?)
						(define fml (= vi (cdr kv)))
						(cons 
							(memory-sforce-write (car mem+fml) (string-id (car kv)) vi) 
							(and (cdr mem+fml) fml)))
					(cons mem0 #t) input))
			(cons (std:struct-copy machine mac [mem mem-ass]) fml-ass))

		(define (compare-output mac output)
			(define mem0 (machine-mem mac))
			(foldl (lambda (kv fml-cur) (= (cdr kv) (memory-sread mem0 (string-id (car kv))))) #t output))

		(match-define (cons mac-ass fml-ass) (assign-input mac input))
		(define boot-pmark (alloc-pmark (machine-boot mac-ass)))
		(match-define (cons all-invokes mac-done) (invoke->relation boot-pmark mac-ass null))
		(define fml-out (compare-output mac-done output))
		(define fml-code (andmap (lambda (func) (function-formula-fmls func)) all-invokes))
		(and (starting-pmark boot-pmark) fml-ass fml-out fml-code))

	(cons soft-cons hard-cons))


;============================= Helper Functions ====================================
(define (initialize-machine-encoding mac)
	(define mac-tmp (std:struct-copy machine mac [classes
		(map 
			(lambda (cls) (std:struct-copy class cls 
				[sfuncs (map alloc-lid (class-sfuncs cls))]
				[vfuncs (map alloc-lid (class-vfuncs cls))]))
			(machine-classes mac))]))
	(std:struct-copy machine mac-tmp 
		[boot (alloc-lid (machine-boot mac))] 
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


(define (vfunc-id-alt mac cls func arg-types) (lookup-virtual-function-alt mac cls func arg-types))

(define (build-virtual-table-alt mac) 
	(define classes (machine-classes mac))
	(define (process-class cls mem)
		(define cls-name (class-name cls))

		(define sfuncs (class-sfuncs cls))
		(define vfuncs (class-vfuncs cls))
		(define sfields (class-sfields cls))
		(define vfields (class-vfields cls))

		(define mem-sfuncs (foldl (lambda (sf-fml mem) (memory-sforce-write mem 
			(sfunc-id cls-name (function-name (function-formula-func sf-fml)) (map cdr (function-args (function-formula-func sf-fml)))) sf-fml)) mem sfuncs))
		(define mem-sfields (foldl (lambda (sf mem) (memory-sdecl mem (sfield-id cls-name sf))) mem-sfuncs sfields))

		(define mem-vfuncs (foldl (lambda (vf-fml mem) (memory-fdecl mem 
			(vfunc-id-alt mac cls-name (function-name (function-formula-func vf-fml)) (map cdr (function-args (function-formula-func vf-fml)))))) mem-sfields vfuncs))
		(define mem-vfields (foldl (lambda (vf mem) (memory-fdecl mem (vfield-id mac cls-name vf))) mem-vfuncs vfields))

		mem-vfields)

	(define mem-push (memory-spush (machine-mem mac)))
	(std:struct-copy machine mac [mem (foldl process-class mem-push classes)]))



;funcion -> function-formula (with line id, pmark is empty)
(define (alloc-lid func)
	(function-formula func 
		(map (lambda (any) (define-symbolic* line-id boolean?) line-id) (function-prog func))
		null
		#f
		#t))

;function-formula -> function-formula (with pmark)
(define (alloc-pmark func-fml)
	(std:struct-copy function-formula func-fml 
		[ret-pmark ((lambda () (define-symbolic* path-mark-ret boolean?) path-mark-ret))] 
		[pmarks	(map (lambda (any) (define-symbolic* path-mark boolean?) path-mark) (function-prog (function-formula-func func-fml)))]))

;function-formula -> bool?
(define (starting-pmark func-fml)
	(car (function-formula-pmarks func-fml)))

;function-formula -> bool?
(define (ending-pmark func-fml)
	(function-formula-ret-pmark func-fml))

;function-formula X int(pc) -> bool?
(define (get-pmark func-fml pc)
;	(display "\nget-pmark\n")
;	(print-func-fml func-fml)
	(list-ref (function-formula-pmarks func-fml) pc))

;function-formula X int(pc) -> bool?
(define (get-lid func-fml pc)
;	(display "\nget-lid\n")
;	(print-func-fml func-fml)
	(list-ref (function-formula-lids func-fml) pc))

(define (all-functions mac)
	(apply append
		(map (lambda (cls) (append (class-sfuncs cls) (class-vfuncs cls))) (machine-classes mac))))




;============================= Encoder Implementation ====================================
;function X machine -> list of function-formula from all functions in `mac` transitively invoked by `func` X machine
(define (invoke->relation func-fml mac args)
	(display "\nInvoking:\n")
	(print-func-fml func-fml)
	(insts->relation func-fml (invoke-setup func-fml mac args)))

;basically a copy of `function-call`
(define (invoke-setup func-fml mac args)
	(define func (function-formula-func func-fml))
	(define mac-reset (std:struct-copy machine mac [pc pc-init][mem (memory-spush (machine-mem mac))]))
	(define mac-decl (std:struct-copy machine mac-reset [mem 
		(foldl 
			(lambda (var-def mem) (memory-sdecl mem (string-id (car var-def)))) 
			(machine-mem mac-reset) 
			(append (function-args func) (function-locals func)))]))
	(define mac-input (std:struct-copy machine mac-decl [mem
		(foldl 
			(lambda (arg-src arg-dst mem) (memory-swrite mem (string-id (car arg-dst)) (expr-eval arg-src mac)))
			(machine-mem mac-decl)
			args 
			(function-args func))]))
	mac-input)



;relation building state
(struct rbstate (funcs pc fml func-fml mac) #:transparent)

(define (insts->relation func-fml mac)
	(match (foldl inst->relation
					(rbstate null 0 #t func-fml mac) 
					(function-prog (function-formula-func func-fml)))
		[(rbstate funcs pc fml func-fml mac) 
				(begin
				(display "\nReturn Formula:\n")
				(println fml)
				(cons (cons (std:struct-copy function-formula func-fml [fmls fml]) funcs) mac))]))



;instruction X rbstate -> rbstate
(define (inst->relation inst st)

	(display "\nInstruction:\n")
	(println inst)
;	(println st)

	(match st [(rbstate funcs pc fml func-fml mac)
		(begin
		(define func (function-formula-func func-fml))
		(define mark (get-pmark func-fml pc))
		(define id (get-lid func-fml pc))

		(define (next-mark) (get-pmark func-fml (+ 1 pc)))
		(define (label-mark label) 
			(define new-pc (imap-get (function-lmap func) label))
			(get-pmark func-fml new-pc))

		(define (select-fml? fml)
			(implies id fml))

		(define (assert-pc-next fml-path fml-op)
			(and 
				fml-path
				(equal? mark (and fml-op (next-mark)))))

		(define (assert-pc-ret fml-path fml-op)
			(and
				fml-path
				(equal? mark (and fml-op (function-formula-ret-pmark func-fml)))))

		(define (assert-pc-branch cnd-t cnd-f label)
			(letrec ([fml-t (equal? cnd-t (label-mark label))]
					 [fml-f (equal? cnd-f (next-mark))]
					 [fml-cnd (and fml-t fml-f)]
					 [fml-br (or (label-mark label) (next-mark))]
					 [fml-path (equal? mark (and fml-cnd fml-br))])
					fml-path))

		(define (assert-pc-invoke fml-path fml-op func-fmls cnds)
			(letrec	([fml-cnds (andmap (lambda (func-fml cnd) (equal? cnd (starting-pmark func-fml))) func-fmls cnds)]
					 [fml-brs (ormap ending-pmark func-fmls)])
					(and
						fml-path
						(equal? mark (and fml-op fml-cnds fml-brs (next-mark))))))

		; (int -> any) X int -> any X formula
		(define (maybe-happen f v)
			(maybe-happen-alt f v nullptr))

		(define (maybe-happen-alt f v v-alt)
			(define-symbolic* shadow-key integer?)
			(letrec ([fml-key (= shadow-key (if mark v v-alt))]
					 [result (f shadow-key)])
					(cons result fml-key)))



		(match inst 
			[(inst-nop _) 
				(letrec ([fml-switch (select-fml? #t)]
						 [fml-path (assert-pc-next #t fml-switch)])
						(std:struct-copy rbstate st [pc (+ 1 pc)] [fml (and fml fml-path)]))]

			[(inst-init classname)
				(letrec ([mem-0 (machine-mem mac)]
						 [addr (memory-sread mem-0 var-this-name)]
						 [mem-bind-func+fml 
							(foldl
								(lambda (func-fml mem+fml) 
									(define func-id (vfunc-id-alt mac classname 
																(function-name (function-formula-func func-fml)) 
																(map cdr (function-args (function-formula-func func-fml)))))
									(if (is-not-found? (memory-fread (car mem+fml) func-id addr))
										(match (maybe-happen (lambda (addr) (memory-fwrite (car mem+fml) func-id addr func-fml)) addr)
											[(cons mem fml) (cons mem (and fml (cdr mem+fml)))])
										mem+fml))
								(cons mem-0 #t)
								(class-vfuncs (imap-get (machine-cmap mac) classname)))]
						 [pc-next (+ 1 pc)])
						(std:struct-copy rbstate st 
							[mac (std:struct-copy machine mac [mem (car mem-bind-func+fml)])] 
							[fml (and fml (cdr mem-bind-func+fml))] 
							[pc pc-next]))]

			[(inst-new v-name) 
				(begin
				(define mem-0 (machine-mem mac))
				(match-define (cons addr+mem fml-alloc) (maybe-happen-alt (lambda (size) (memory-alloc mem-0 size)) 1 0))
				(match-define (cons addr mem-alloc) addr+mem)
				(match-define (cons mem-ass fml-write) (maybe-happen (lambda (addr) (memory-swrite mem-alloc v-name addr)) addr))
				(define mac-new (std:struct-copy machine mac [mem mem-ass]))
				(define fml-new (assert-pc-next (and fml-alloc fml-write) #t))
				(std:struct-copy rbstate st [mac mac-new] [pc (+ 1 pc)] [fml (and fml fml-new)]))]


			[(inst-ret v-expr) 
				(begin
				(define-symbolic* vs integer?)
				(define ret-value (expr-eval v-expr mac))
				(define fml-ret-val (equal? vs ret-value))
				(match-define (cons mem-ret fml-write) (maybe-happen (lambda (key) (memory-sforce-write (machine-mem mac) key ret-value)) var-ret-name))
				(define mac-new (std:struct-copy machine mac [mem mem-ret]))
				(define fml-new (select-fml? fml-ret-val))
				(define fml-path (assert-pc-ret fml-write fml-new))
				(std:struct-copy rbstate st [mac mac-new] [pc pc-ret] [fml (and fml fml-path)]))]


			[(inst-static-call ret cls-name func-name arg-types args) 
				(begin
				(define func-invoked (alloc-pmark (memory-sread (machine-mem mac) (sfunc-id cls-name func-name arg-types))))
				(match-define (cons funcs-ret mac-ret) (invoke->relation func-invoked mac args))

				(define mem-ret (machine-mem mac-ret))
				(define-symbolic* ret-value integer?) 
				(define fml-ret (= ret-value (memory-sread mem-ret var-ret-name)))
				(define mem-pop (memory-spop mem-ret))
				(match-define (cons mem-ass fml-ass) (maybe-happen (lambda (ret) (memory-swrite mem-pop ret ret-value)) ret))

				(define pc-next (+ 1 pc))
				(define mac-new (std:struct-copy machine mac [mem mem-ass]))
				(define fml-new (assert-pc-invoke fml-ass fml-ret (list func-invoked) (list #t)))
				
				(std:struct-copy rbstate st [funcs (append funcs funcs-ret)] [mac mac-new] [pc pc-next] [fml (and fml fml-new)]))]


			[(inst-virtual-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define mem0 (machine-mem mac))
				(define obj-addr (memory-sread mem0 obj-name))
				(define func-invoked (alloc-pmark (memory-fread mem0 (vfunc-id-alt mac cls-name func-name arg-types) obj-addr)))

				;push an extra scope to avoid overwriting "this" of the current scope
				;callee's stack, no side effect on caller, no need to shadow write
				(define mem-this (memory-sforce-write (memory-spush mem0) var-this-name obj-addr))
				(define mac-this (std:struct-copy machine mac [mem mem-this]))

				(match-define (cons funcs-ret mac-ret) (invoke->relation func-invoked mac-this args))
				(define mem-ret (machine-mem mac-ret))
				(define-symbolic* ret-value integer?) 
				(define fml-ret (= ret-value (memory-sread mem-ret var-ret-name)))
				;pop callee and callee's "this"
				(define mem-pop (memory-spop (memory-spop mem-ret)))
				(match-define (cons mem-ass fml-ass) (maybe-happen (lambda (ret) (memory-swrite mem-pop ret ret-value)) ret))

				(define pc-next (+ 1 pc))
				(define mac-new (std:struct-copy machine mac [mem mem-ass]))
				(define fml-new (assert-pc-invoke fml-ass fml-ret (list func-invoked) (list #t)))

				(std:struct-copy rbstate st [funcs (append funcs funcs-ret)] [mac mac-new] [pc pc-next] [fml (and fml fml-new)]))]


			[(inst-special-call ret obj-name cls-name func-name arg-types args)
				(begin
				(define mem0 (machine-mem mac))
				(define obj-addr (memory-sread mem0 obj-name))
				(define func-invoked (alloc-pmark (memory-sread mem0 (sfunc-id cls-name func-name arg-types))))

				;push an extra scope to avoid overwriting "this" of the current scope
				;callee's stack, no side effect on caller, no need to shadow write
				(define mem-this (memory-sforce-write (memory-spush mem0) var-this-name obj-addr))
				(define mac-this (std:struct-copy machine mac [mem mem-this]))

				(match-define (cons funcs-ret mac-ret) (invoke->relation func-invoked mac-this args))
				(define mem-ret (machine-mem mac-ret))
				(define-symbolic* ret-value integer?) 
				(define fml-ret (= ret-value (memory-sread mem-ret var-ret-name)))
				;pop callee and callee's "this"
				(define mem-pop (memory-spop (memory-spop mem-ret)))
				(match-define (cons mem-ass fml-ass) (maybe-happen (lambda (ret) (memory-swrite mem-pop ret ret-value)) ret))

				(define pc-next (+ 1 pc))
				(define mac-new (std:struct-copy machine mac [mem mem-ass]))
				(define fml-new (assert-pc-invoke fml-ass fml-ret (list func-invoked) (list #t)))

				(std:struct-copy rbstate st [funcs (append funcs funcs-ret)] [mac mac-new] [pc pc-next] [fml (and fml fml-new)]))]

			[(inst-ass vl vr) 
				(begin
				(define mem (machine-mem mac))
				(define value (expr-eval vr mac))
				(define-symbolic* vs integer?)
				(match-define (cons mem-new fml-key) (maybe-happen (lambda (vl) (memory-swrite mem vl vs)) vl))

				(define fml-value (= value vs))
				(define fml-switch (select-fml? fml-value))
				(define fml-new (assert-pc-next fml-key fml-switch))
				(define mac-new (std:struct-copy machine mac [mem mem-new]))
				(define pc-next (+ 1 pc))
				
				(std:struct-copy rbstate st [mac mac-new] [pc pc-next] [fml (and fml fml-new)]))]

			[(inst-jmp condition label)
				(letrec
					([mem (machine-mem mac)]
					 [lmap (function-lmap func)]
					 [value (expr-eval condition mac)]
					 [fml-new (assert-pc-branch (select-fml? value) (select-fml? (not value)) label)]
					 [pc-next (+ 1 pc)])
					(std:struct-copy rbstate st [pc pc-next] [fml (and fml fml-new)]))]))]))

					;[fml-t (equal? (implies id value) (label-mark label))]
					;[fml-f (equal? (implies id (not value)) (next-mark))]
					;[fml-switch (and fml-t fml-f)]
					;[fml-br (or (label-mark label) (next-mark))]
					;[fml-path (equal? mark (and fml-switch fml-br))])
					
		

