#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require "semantics-computational.rkt")
(require (prefix-in std: racket/base))
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

(struct function-formula (func lids pmarks fmls) #:transparent)

;ast ->  line ids(list of sym bool) X (input(list of key & value) -> output(list of key & value) -> relation)
(define (ast->relation ast)
	(define mac-raw (ast->machine ast))
	(define mac (initialize-machine-encoding mac-raw))

	(define all-invokes (invoke->relation (alloc-pmarks (machine-boot mac))))

	(define soft-cons 
		(map (lambda (b) (if b 1 0)) 
			(apply append (map (lambda (func) (function-formula-line-ids func)) (all-functions mac)))))

	;[TODO] add input output
	(define (hard-cons input output) 
		(apply and (map (lambda (func) (function-formula-fmls func)) all-invokes)))

	(cons soft-cons hard-cons))

;	(lambda (input output)
;		;example-specific path mark
;		(define inst-tris (map
;			(lambda (inst id) 
;				(define-symbolic* path-mark boolean?) 
;				(cons inst (cons id path-mark)))
;			(machine-prog mac) line-ids))
;		(define mem-input (foldl (lambda (kv mem) (define-symbolic* vs integer?) (memory-store mem (car kv) vs)) memory-empty input))
;		(define fml-input (foldl 
;			(lambda (kv fml) 
;				(and fml 
;					(=
;						(memory-load mem-input (car kv))
;						(cdr kv))))
;			#t
;			input))
;
;		(define mac-input (std:struct-copy machine mac [mem mem-input][prog inst-tris]))
;
;		;encode program
;		(define pc-fml-mac (foldl inst->relation.wrapper (cons 0 (cons #t mac-input)) (machine-prog mac-input)))
;
;		;encode output
;		(define fml-exec (cadr pc-fml-mac))
;		(define mem-output (machine-mem (cddr pc-fml-mac)))
;		(define fml-output (foldl (lambda (kv fml) (and fml (= (memory-load mem-output (car kv)) (cdr kv)))) #t output))
;		(define mark0 (cddr (car (machine-prog mac-input))))
;		
;		;final result
;		(and mark0 fml-exec fml-input fml-output))))


(define (initialize-machine-encoding mac)
	(define mac-tmp (std:struct-copy machine mac [classes
		(map 
			(lambda (cls) (std:struct-copy class cls 
				[sfuncs (map alloc-lid (class-sfuncs cls))]
				[vfuncs (map alloc-lid (class-vfuncs cls))])
			(machine-classes mac)))]))
	(std:struct-copy machine mac-tmp 
		[boot (alloc-lid (machine-boot mac))] 
		[cmap (foldl 
			(lambda (cls cm) (imap-set cm (class-name cls) cls)) 
			imap-empty 
			(machine-classes mac-tmp))]))


;funcion -> function-formula (with line id, pmark is empty)
(define (alloc-lid func)
	(function-formula func 
		(map (lambda (any) (define-symbolic* line-id boolean?) line-id) (function-prog func))
		null
		#t))

;function-formula -> function-formula (with pmark)
(define (alloc-pmark func)
	(std:struct-copy function-formula func-fml [pmarks
		(map (lambda (any) (define-symbolic* path-mark boolean?) path-mark) (function-prog func))]))

;function-formula -> bool?
(define (starting-pmark func-fml)
	(car (function-formula-pmarks func-fml)))

;function-formula X int(pc) -> bool?
(define (get-pmark func-fml pc)
	(list-ref (function-formula-pmarks func-fml) pc))

;function-formula X int(pc) -> bool?
(define (get-lid func-fml pc)
	(list-ref (function-formula-lids func-fml) pc))

(define (all-functions mac)
	(apply append
		(map (lambda (cls) (append (class-sfuncs cls) (class-vfuncs cls))) (machine-classes mac))))




;function X machine -> list of function-formula from all functions in `mac` transitively invoked by `func`
(define (invoke->relation func mac)
	(insts->relation func (invoke-setup func mac)))

;basically a copy of `function-call`
(define (invoke-setup func mac)
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
				(cons (std:struct-copy function-formula func-fml [fmls fml]) funcs)]))



;instruction X machine -> relation X machine
(define (inst->relation inst st)

	(define-symbolic* vs integer?)
	(define-symbolic* shadow-key integer?)

	(match st [(rbstate funcs pc fml func-fml mac)
		(begin
			(define func (function-formula-func func-fml))
			(define (next-mark) (get-pmark func-fml (+ 1 pc)))
			(define (label-mark label) 
				(define new-pc (imap-get (function-lmap func) label))
				(get-pmark func-fml pc))


	(match inst 
		[(inst-nop _) 
			(letrec 
				([fml-switch (implies id #t)]
				[fml-path (equal? mark (and fml-switch (next-mark)))])
			(cons fml-path mac))]

		[(inst-ret _) (cons #t mac)]

		[(inst-ass vl vr) 
			(letrec 
				([mem (machine-mem mac)]
				[value (expr-eval vr mac)]
				[mem-new (memory-store mem shadow-key vs)]
				[mac-new (std:struct-copy machine mac [mem mem-new])]

				[fml-key (= shadow-key (if mark vl nullptr))]
				[fml-value (= value vs)]
				[fml-new fml-value]
				[fml-switch (implies id fml-new)]
				[fml-path (and 
					fml-key
					(equal? mark (and fml-switch (next-mark))))])

				(cons fml-path mac-new))
		]

		[(inst-jmp condition label)
			(letrec
				([mem (machine-mem mac)]
				[lmap (machine-lmap mac)]
				[value (expr-eval condition mac)]
				[fml-t (equal? (implies id value) (label-mark label))]
				[fml-f (equal? (implies id (not value)) (next-mark))]
				[fml-switch (and fml-t fml-f)]
				[fml-br (or (label-mark label) (next-mark))]
				[fml-path (equal? mark (and fml-switch fml-br))])
				(cons fml-path mac))
		]))
				
	

