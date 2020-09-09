#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "map.rkt")
(require "semantics-computationl.rkt")
(require racket/base)
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

;[TODO] include l & z

;ast -> input -> relation
(define (ast->relation ast)
	(define ma (ast->mahchine))
	(lambda (input)
		(foldl (inst->relation-wrapper) (cons #t ma) (machine-prog ma))))

(define (inst->relation-wrapper inst fm)
	(define ret-pair (inst->relation inst (cdr fm)))
	(define fml-new (and (car fm) (car ret-pair)))
	(define ma-new (cdr ret-pair))
	(cons fml-new ma-new))

;instruction X machine -> relation X machine
(define (inst->relation inst ma)
	(define-symbolic* vs integer?)
	(match inst 
		[(inst-nop _) (cons #t ma)]
		[(inst-ret _) (cons #t ma)]
		[(inst-ass vl vr) 
			(letrec 
				([mem (machine-mem ma)]
				[value (expr-eval vr ma)]
				[mem-new (memory-set mem vl vs)]
				[ma-new (struct-copy machine ma [mem mem-new])]
				[fml-new (= value vs)])
				(cons fml-new ma-new))]
		[(inst-jmp condition label)
			(letrec
				([mem (machine-mem ma)]
				[lmap (machine-lmap ma)]
				[value (expr-eval condition ma)]
				[fml-new (cdar ret-tuple)])
				(cons #t ma))]))
				
	

