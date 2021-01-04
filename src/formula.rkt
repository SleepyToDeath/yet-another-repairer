#lang rosette/safe

(require (prefix-in std: racket/match))
(require racket/format)
(require racket/pretty)
(require rosette/lib/match)   ; provides `match`

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
		(match e
			[(expression op child ...) 
				(begin 
				(display "Op: ") 
				(print op) 
				(display "\n") 
				(display (length child))
				(display "\n") 
				(+ 1 (apply + (map size-of-limited-real child))))]
			[(constant id type) (begin (print id) (display "\n\n") 1)]
			[x (begin (print x) (display "\n\n")1)])
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

;============================= Debug ========================================
(define eval-pending null)
(define (defer-eval msg value)
	(set! eval-pending (cons (cons msg value) eval-pending)))

(define (print-pending-eval sol)
	(map (lambda (m.v) (display (~a (car m.v) " : " (evaluate (cdr m.v) sol) "\n"))) eval-pending))

(define (clear-pending-eval)
	(set! eval-pending null))

(define cons-pending null)
(define (defer-cons v)
	(set! cons-pending (cons v cons-pending)))


