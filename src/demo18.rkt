#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "memory.rkt")
(require "semantics-computational.rkt")
(require "semantics-relational.rkt")

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

(define test-ast
	(letrec 

	([line0
		(stat (stat-jmp
			(expr (expr-binary
				(expr (expr-var (variable 1))) 
				(op <)
				(expr (expr-const (const 1)))))
			(label 1)))]

	[line1
		(stat (stat-ass 
			(variable 2) 
			(expr (expr-binary 
				(expr (expr-var (variable 1))) 
				(op +)
				(expr (expr-const (const 2)))))))]

	[line2
		(stat (stat-jmp
			(expr (expr-const (const #t)))
			(label 2)))]

	[line3
		(stat (stat-label
			(label 1)))]

	[line4
		(stat (stat-ass 
			(variable 2) 
			(expr (expr-const (const 2)))))]

	[line5
		(stat (stat-label
			(label 2)))]

	[line6
		(stat (stat-ass 
			(variable 3) 
			(expr (expr-var (variable 2)))))]

	[test-program 
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
			(stats (stats-single (stat (stat-ret (nop 0)))))))])
	
	test-program))

;f(0) = 2
(define input1 (list (cons 1 0)))
(define output1 (list (cons 3 2)))

;f(1) = 2
(define input2 (list (cons 1 1)))
(define output2 (list (cons 3 2)))

;f(2) = 3
(define input3 (list (cons 1 2)))
(define output3 (list (cons 3 3)))


(define lf (ast->relation test-ast))

(define ids (car lf))
(define fml-gen (cdr lf))

(define tf1 (fml-gen input1 output1))
(define tf2 (fml-gen input2 output2))
(define tf3 (fml-gen input3 output3))

(define (b2i b)
  (if b 1 0))

(define hard-constraint (and tf1 tf2 tf3))

(define debug-sol (optimize #:maximize (list (foldl (lambda (b s) (+ s (b2i b))) 0 ids))
          #:guarantee (assert hard-constraint)))


(define SEARCH-DEPTH 5)

(define (count-lines ast)
	(match ast
		[(stats (stats-single any)) 1]
		[(stats (stats-multi l r)) (+ (count-lines l) (count-lines r))]))

(define ctxt-enum #f)

;ast X int -> ast
(define (enum-line ast line)
	(if (< line 0) ast
		(match ast
			[(stats (stats-multi l r))
			 (let
				([l-count (count-lines l)])
				(if (< line l-count) 
					(stats (stats-multi (enum-line l line) r)) 
					(stats (stats-multi l (enum-line r (- line l-count))))))]
			[(stats (stats-single any))
				(if (= line 0) (stats (stats-single (stat-enum ctxt-enum SEARCH-DEPTH))) (error "Line number over end of program"))])))
		

(define sketch (cdr (foldl 
	(lambda (id pc-ast)
		(define pc (car pc-ast))
		(define ast (cdr pc-ast))
		(if (evaluate id debug-sol) (cons (+ pc 1) ast) (cons (+ pc 1) (enum-line ast pc))))
	(cons 0 test-ast)
	ids)))

(define (sketch->spec skt input output)
	(compare-output (compute (assign-input (ast->machine skt) input)) output))

(define syn-sol 
	(synthesize
		#:forall null
		#:guarantee (assert (and 
			(sketch->spec sketch input1 output1)
			(sketch->spec sketch input2 output2)
			(sketch->spec sketch input3 output3)))))

(evaluate syn-sol sketch)

