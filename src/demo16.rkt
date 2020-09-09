#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "semantics-computational.rkt")

;if (v1 < 1)
;	v2 = 2
;else
;	v2 = v1 + 2
;v3 = v2
;
;	|
;	| java -> jimple
;	V
;
;[0] v1 = x
;[1] jmp (v1 < 1) l1

;[2] v2 = v1 + 2  | b1
;[3] jmp (#t) l2  | b1

;[4] label l1	  | b2
;[5] v2 = 2       | b2

;[6] label l2     
;[7] v3 = v2
;[8] y = v3
;
;	|
;	| jimple -> ast
;	V
;

(define (build-program x)
	(define line0
		(stat (stat-ass
			(variable 1)
			(expr (expr-const (const x))))))

	(define line1
		(stat (stat-jmp
			(expr (expr-binary
				(expr (expr-var (variable 1))) 
				(op <)
				(expr (expr-const (const 1)))))
			(label 1))))

	(define line2
		(stat (stat-ass 
			(variable 2) 
			(expr (expr-binary 
				(expr (expr-var (variable 1))) 
				(op +)
				(expr (expr-const (const 2))))))))

	(define line3
		(stat (stat-jmp
			(expr (expr-const (const #t)))
			(label 2))))

	(define line4
		(stat (stat-label
			(label 1))))

	(define line5
		(stat (stat-ass 
			(variable 2) 
			(expr (expr-const (const 2))))))

	(define line6
		(stat (stat-label
			(label 2))))

	(define line7
		(stat (stat-ass 
			(variable 3) 
			(expr (expr-var (variable 2))))))

	(define test-program 
		(stats (stats-multi
			(stats (stats-multi
			(stats (stats-multi
			(stats (stats-multi
			(stats (stats-multi
			(stats (stats-multi
			(stats (stats-multi
			(stats (stats-multi
				(stats (stats-single line0))
				(stats (stats-single line1))))
				(stats (stats-single line2))))
				(stats (stats-single line3))))
				(stats (stats-single line4))))
				(stats (stats-single line5))))
				(stats (stats-single line6))))
				(stats (stats-single line7))))
			(stats (stats-single (stat (stat-ret (nop 0))))))))
	
	test-program
)

;f(-1) = 2
;f(0) = 2
;f(1) = 3
;f(2) = 4
(define p-1 (build-program -1))
(define p0 (build-program 0))
(define p1 (build-program 1))
(define p2 (build-program 2))

(define (check-n-run p)
	(begin
	(println (ast-check p))
	(define test-machine (ast->machine p))
	(define result-machine (compute test-machine))
	(define mem (machine-mem result-machine))
	(memory-load mem 3)
	))

(check-n-run p-1)
(check-n-run p0)
(check-n-run p1)
(check-n-run p2)


