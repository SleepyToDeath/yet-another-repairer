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

(define (print-fml e)
	(print-fml-struct (fml-to-struct e))
	(display "\n"))

(define (fml-to-struct e)
	(if (union? e) 
		(expr-fp "U" (map (lambda (gv) (cons (fml-to-struct (car gv)) (fml-to-struct (cdr gv)))) (union-contents e)))
		(match e
			[(expression op child ...) (expr-fp op (map fml-to-struct child))]
			[(constant id type) (const-fp id type)]
			[x x])))

(define (print-fml-struct e)
	(if (> (size-of-struct e) 5) (display "\n") #f)
	(match e
		[(expr-fp op children) 
			(begin
			(display " (")
			(print op) 
			(map print-fml-struct children)
			(display ")"))]
		[(const-fp id type) (begin (display " ") (display id))]
		[(cons x y) (begin (print-fml-struct x) (print-fml-struct y))]
		[x (begin (display " ") (display x))]))

(define (size-of-struct e)
	(match e
		[(expr-fp op children) 
			(+ 1 (apply + (map size-of-struct children)))]
		[(const-fp id type) 1]
		[(cons x y) (+ (size-of-struct x) (size-of-struct y))]
		[x 1]))

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

(define (force-error cnd msg)
	(define len1 (length (asserts)))
	(if cnd (std:error msg) #f)
	(define len2 (length (asserts)))
	(if (> len2 len1) (std:error msg) #f))

(define (int->symbolic v)
	(define-symbolic* vs integer?)
	(assert (equal? v vs))
	vs)

(define (andmap+ f l)
	(foldl (lambda (e fml) (and (f e) fml)) #t l))

(define (and+ a b)
	(and a b))


;========================== Optional Type ===================================
(define (maybe-do+ s f)
	(maybe-do identity #f s f))

(define (maybe-do pred default s f)
	(if (pred s) (f s) default))

(define (maybe-select invalid-state is-invalid-state?)
	(lambda (candidates summary?)
		(define ret.maybe (ormap
			(lambda (cnd.v)
				(if (and 
						(car cnd.v) 
						(cdr cnd.v)
						(not (is-invalid-state? (cdr cnd.v))))
					(cdr cnd.v)
					#f))
			candidates))
		(if (or summary? ret.maybe) ret.maybe invalid-state)))

;============================= Timer ========================================
(define last-time 0)
(define (timer-on)
	(set! last-time (std:current-inexact-milliseconds)))
(define (timer-check)
	(- (std:current-inexact-milliseconds) last-time))

;============================= Debug ========================================
(define DEBUG-ON #t)
(define-syntax-rule (DEBUG-DO something)
	(if DEBUG-ON something #f))

(define eval-pending null)
(define (defer-eval msg value)
	(set! eval-pending (cons (cons msg value) eval-pending)))

(define (print-pending-eval sol)
	(pretty-print (evaluate eval-pending sol)))
;	(map (lambda (m.v) 
;			(display (~a (car m.v) " : " ))
;			(pretty-print (evaluate (cdr m.v) sol)))
;			(display  "\n"))
;		eval-pending))

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
	(if fail? (force-error #t "Asserts are infeasible!") #f))

(define max-sat-list null)
(define (add-max-sat fml)
	(set! max-sat-list (cons fml max-sat-list)))

(define (inspect fml)
	(add-max-sat fml)
	(defer-eval "Inspecting formula: " fml))

(define stored-asserts null)
(define (store-asserts)
	(set! stored-asserts (asserts)))
(define (restore-asserts)
	(clear-asserts!)
	(map (lambda (p) (assert p)) stored-asserts))
;test if asserting the extra predicate will result in unsat
;will restore asserts after test
;will throw error if unsat
(define (test-assert p)
	(display "Testing new assert...\n")
	(store-asserts)
	(define fail? (unsat? (solve (assert p))))
	(if fail? (begin (print-fml p) (force-error #t "Asserts are infeasible!")) #f)
	(restore-asserts))
;test if asserting the extra predicate will result in unsat
;will keep the new assert after test
;will throw error if unsat
(define (test-assert! p)
	(display "Testing new assert...\n")
	(assert p)
	(define fail? (unsat? (solve (assert #t))))
	(if fail? (begin (print-fml p) (force-error #t "Asserts are infeasible!")) #f))

(define (test-no-branch)
	(define len-0 (length (asserts)))
	(std:error "Not even wrong")
	(define len-1 (length (asserts)))
	(if (> len-1 len-0) (force-error #t (take (asserts) (- len-1 len-0))) #f))

(define (do-n-ret f v)
	(f v)
	v)
