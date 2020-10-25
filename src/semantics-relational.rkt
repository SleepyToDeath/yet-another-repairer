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

	(define all-funcs 
		(cons (function->relation (machine-boot mac))
			(apply append (map 
				(lambda (cls) (append
					(map function->relation (class-sfuncs cls))
					(map function->relation (class-vfuncs cls))))
				(machine-classes mac)))))

	(define soft-cons (apply append (map (lambda (func) (function-formula-line-ids func)) all-funcs)))
	;[TODO] add input output
	(define (hard-cons input output) 
		(apply and (map (lambda (func) (function-formula-fmls func)) all-funcs))))

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
				[sfuncs (map function->function-formula (class-sfuncs cls))]
				[vfuncs (map function->function-formula (class-vfuncs cls))])
			machine-classes))]))
	(std:struct-copy machine mac-tmp [cmap
		(foldl (lambda (cls cm) (imap-set cm (class-name cls) cls)) imap-empty (machine-classes mac-tmp))))


(define (function->function-formula func)
	(function-formula func 
		(map (lambda (any) (define-symbolic* line-id boolean?) line-id) (function-prog func))
		(map (lambda (any) (define-symbolic* path-mark boolean?) path-mark) (function-prog func))
		#t))


(define (function->relation func mac)



(define (inst->relation.wrapper inst-tri pc-fml-mac)
	(define inst (car inst-tri))
	(define id (cadr inst-tri))
	(define mark (cddr inst-tri))
	(define pc (car pc-fml-mac))
	(define fml (cadr pc-fml-mac))
	(define mac (cddr pc-fml-mac))

	(define ret-pair (inst->relation pc inst id mark mac))

	(define pc-new (+ pc 1))
	(define fml-new (and fml (car ret-pair)))
	(define mac-new (cdr ret-pair))

	(cons pc-new (cons fml-new mac-new)))

;instruction X machine -> relation X machine
(define (inst->relation mac func pc inst id mark)

	(define-symbolic* vs integer?)
	(define-symbolic* shadow-key integer?)

	(define (next-mark) (cddr (list-ref (machine-prog mac) (+ 1 pc))))
	(define (label-mark label) 
		(define new-pc (imap-get (machine-lmap mac) label))
		(cddr (list-ref (machine-prog mac) new-pc)))

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
				
	

