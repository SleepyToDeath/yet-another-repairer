#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require racket/base)
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;prog: list of instruction;
;mem: memory
;pc: int
;lmap: imap: label(int) -> instruction index(int)
(struct machine (prog lmap mem pc) #:transparent)

;inst-exec: machine(before exec) -> machine(after exec)
(define-generics instruction
	[inst-exec instruction machine])

;expr-eval: expr -> ret(int/bool) X machine(after side effect))
(define-generics expression
	[expr-eval expression machine])

;[TODO?] What is the clean way to do this?
(define (expr-eval-dispatch e m) (expr-eval e m))

(define pc-ret -1)
(define pc-init 0)
(define machine-empty (machine null imap-empty memory-empty pc-init))

(define (compute m)
	(if (= (machine-pc m) pc-ret) 
		m	
		(let ([inst-cur (list-ref (machine-prog m) (machine-pc m))])
			(compute (inst-exec inst-cur m)))))



;[TODO?] memory allocation
(define (ast->machine ast)
	(__ast->machine ast machine-empty))

(define (__ast->machine ast m)
	(match ast
		[(stats (stats-multi l r))
			(begin
				(define m1 (__ast->machine l m))
				(__ast->machine r m1))]
		[(stats (stats-single head))
			(begin
				(define ret-pair (ast->instruction head m))
				(define i-new (car ret-pair))
				(define m-new (cdr ret-pair))
				(define prog-new (if i-new (append (machine-prog m) (list i-new)) (machine-prog m)))
				(struct-copy machine m-new [prog prog-new]))]))

;ast -> instruction X machine(lmap updated)
(define (ast->instruction ast m)
	(match ast
		[(stat (stat-ass (variable addr) rvalue)) (cons (inst-ass addr (ast->expression rvalue)) m)]
		[(stat (stat-jmp condition (label target))) (cons (inst-jmp (ast->expression condition) target) m)]
		[(stat (stat-label (label here)))
			(begin
				(define lmap-new (imap-set (machine-lmap m) here (length (machine-prog m))))
				(cons #f (struct-copy machine m [lmap lmap-new])))]
		[(stat (stat-nop any)) (cons (inst-nop nullptr) m)]
		[(stat (stat-ret any)) (cons (inst-ret nullptr) m)]))

(define (ast->expression ast)
	(match ast
		[(expr (expr-const (const v))) (iexpr-const v)]
		[(expr (expr-var (variable v))) (iexpr-var v)]
		[(expr (expr-binary expr1 (op v) expr2)) (iexpr-binary v (ast->expression expr1) (ast->expression expr2))]))




;addr(int) X iexpr
(struct inst-ass (vl vr) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m) 

		(define v-new (expr-eval (inst-ass-vr i) m))

		(define mem-new (memory-store (machine-mem m) (inst-ass-vl i) v-new))
		(define pc-next (+ 1 (machine-pc m)))

		(struct-copy machine m [mem mem-new] [pc pc-next]))])

;expr X label(int)
(struct inst-jmp (condition label) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m)

		(define lmap (machine-lmap m))
		(define iaddr (inst-jmp-label i))
		(define pc-jmp (imap-get lmap iaddr))
		(define pc-next (+ 1 (machine-pc m)))

		(define c (expr-eval (inst-jmp-condition i) m))
		(define pc-new (if c pc-jmp pc-next))

		(struct-copy machine m [pc pc-new]))])

(struct inst-nop (any) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m)
		(define pc-next (+ 1 (machine-pc m)))
		(struct-copy machine m [pc pc-next]))])

(struct inst-ret (any) #:transparent
	#:methods gen:instruction
	[(define (inst-exec i m)
		(struct-copy machine m [pc pc-ret]))])




(struct iexpr-const (value) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(iexpr-const-value e))])

(struct iexpr-var (addr) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define value (memory-load (machine-mem m) (iexpr-var-addr e) ))
		value)])


(struct iexpr-binary (op expr1 expr2) #:transparent
	#:methods gen:expression
	[(define (expr-eval e m)
		(define v1 (expr-eval-dispatch (iexpr-binary-expr1 e) m))
		(define v2 (expr-eval-dispatch (iexpr-binary-expr2 e) m))
		((iexpr-binary-op e) v1 v2))])


