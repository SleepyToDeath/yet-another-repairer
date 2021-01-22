#lang rosette/safe

(require (prefix-in std: racket/match))
(require (prefix-in std: racket/base))

(require "memory-common.rkt")
(require racket/format)
(require racket/pretty)
(require rosette/lib/match)   ; provides `match`
(require rosette/base/core/polymorphic)

(provide (all-defined-out))

(struct expr-fp (op children) #:transparent)
(struct const-fp (id type) #:transparent)

(define (fml-to-print e)
	(if (union? e) 
		(expr-fp "U" (map (lambda (gv) (cons (fml-to-print (car gv)) (fml-to-print (cdr gv)))) (union-contents e)))
		(match e
			[(expression op child ...) (expr-fp op (map fml-to-print child))]
			[(constant id type) (const-fp id type)]
			[x x])))

(define size-limit 0)

(define (size-of-limited e limit)
	(set! size-limit limit)
	(size-of-limited-real e))

(define (size-of-limited-real e)
	(set! size-limit (- size-limit 1))
	(if (> size-limit 0)
		(if (union? e) 
			(apply + (map (lambda (gv) (+ (size-of-limited-real (car gv)) (size-of-limited-real (cdr gv)))) (union-contents e)))
			(match e
				[(expression op child ...) (+ 1 (apply + (map size-of-limited-real child)))]
				[(constant id type) 1]
				[_ 1]))
		0))

(define (size-of e)
	(if (union? e) 
		(apply + (map (lambda (gv) (+ (size-of (car gv)) (size-of (cdr gv)))) (union-contents e)))
		(match e
			[(expression op child ...) (+ 1 (apply + (map size-of child)))]
			[(constant id type) 1]
			[_ 1])))

(define (is-concrete-value e)
	(if (union? e) #f
		(match e
			[(expression op child ...) #f]
			[(constant id type) #f]
			[_ #t])))

;A map that applies the function(f) to every POSSIBLE element of the list(l) 
;	identified by a predicate(p) and flatten the result(guards are discarded)
;Used to map a symbolic list to a concrete one
(define (reflection-map p f l)
	(define (symbolic->list e)
;		(display (~a "---: " e "\n"))
		(if (union? e) 
			(apply append (map (lambda (g.v) (symbolic->list (cdr g.v))) (union-contents e)))
			(match e
				[(expression op child ...) 
					(if (equal? op ite) 
						(apply append (map symbolic->list (cdr child)))
						(apply append (map symbolic->list child)))]
				[(constant id type) null]
				[e (if (p e) (list e) null)])))
	(map f (apply append (map symbolic->list l))))

;(define (maybe f s)
;	(if s (f s) #f))

(define (maybe pred f s default)
	(if (pred s) (f s) default))

(define (force-error cnd msg)
	(define len1 (length (asserts)))
	(if cnd (std:error msg) #f)
	(define len2 (length (asserts)))
	(if (> len2 len1) (std:error msg) #f))

;============================= Debug ========================================
(define eval-pending null)
(define (defer-eval msg value)
	(set! eval-pending (cons (cons msg value) eval-pending)))

(define (print-pending-eval sol)
	(map (lambda (m.v) 
			(display (~a (car m.v) " : " ))
			(print (evaluate (cdr m.v) sol))
			(display  "\n"))
		eval-pending))

(define (clear-pending-eval)
	(set! eval-pending null))

(define cons-pending null)
(define (defer-cons v)
	(set! cons-pending (cons v cons-pending)))

(define (check-asserts index)
	(display (~a "!!!!!!!!!!!!!!!#" index " Asserts: " (length (asserts)) "\n"))
	(define fail? (unsat? (solve (assert #t))))
	(display (~a "Unsat? " fail? "\n"))
	;(if (equal? (length (asserts)) 1) (pretty-print (asserts)) #f)
	(pretty-print (asserts))
	(if fail? (std:error "Asserts are infeasible!") #f))
