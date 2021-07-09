#lang rosette/safe

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require (prefix-in std: racket/base))

(require "../demo-parser.rkt")

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

;(current-bitwidth #f)

(define program-text
  (std:string-append
    "jmp v1 < 1 l1; \n"
    "v2 = v1 + 2; \n"
    "jmp 1 < 2 l2; \n"
    "label l1: \n"
    "v2 = 2; \n"
    "label l2: \n"
    "v3 = v2; \n"
    "return; \n"))

(define parsed-program
  (parse (tokenize (std:open-input-string program-text))))

(define parsed-ast
  (interpret-program parsed-program))

(define test-ast parsed-ast)

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

(display "\n Debug: \n")
debug-sol


(define SEARCH-DEPTH 5)

(define (count-lines ast)
	(match ast
		[(stats (stats-single any)) 1]
		[(stats (stats-multi l r)) (+ (count-lines l) (count-lines r))]))

(define (extract-vars ast)
	(match ast
		[(stats (stats-multi l r)) (append (extract-vars l) (extract-vars r))]
		[(stats (stats-single s)) (extract-vars s)]
		[(stat (stat-ass l r)) (append (extract-vars l) (extract-vars r))]
		[(variable v) (list v)]
		[(expr (expr-var v)) (extract-vars v)]
		[(expr (expr-binary l o r)) (append (extract-vars l) (extract-vars r))]
		[_ null]))

(define (extract-labels ast)
	(match ast
		[(stats (stats-multi l r)) (append (extract-labels l) (extract-labels r))]
		[(stats (stats-single s)) (extract-labels s)]
		[(stat (stat-label v)) (extract-labels v)]
		[(label v) (list v)]
		[_ null]))

(define consts (list 0 1 2 3 4 5))
(define ops (list + -))
(define vars (remove-duplicates (extract-vars test-ast)))
(define labels (extract-labels test-ast))
(define ctxt-enum (syntax-context vars consts ops labels))

(display "\n Enumeration Context: \n")

ctxt-enum

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
				(if (= line 0) (stats (stats-single (stat-enum ctxt-enum SEARCH-DEPTH))) (std:error "Line number over end of program"))])))

(define sketch (cdr (foldl 
	(lambda (id pc-ast)
		(define pc (car pc-ast))
		(define ast (cdr pc-ast))
		(if (evaluate id debug-sol) (cons (+ pc 1) ast) (cons (+ pc 1) (enum-line ast pc))))
	(cons 0 test-ast)
	ids)))

(display "\n Sketch: \n")
(ast-print sketch)

(define (sketch->spec skt input output)
	(compare-output (compute (assign-input (ast->machine skt) input)) output))

(define (compute-output-list skt input)
	(define mem (machine-mem (compute (assign-input (ast->machine skt) input))))
	(memory->list mem 0 5)
)

;(sketch->spec test-ast input1 output1)
;(sketch->spec test-ast input2 output2)
;(sketch->spec test-ast input3 output3)

;(ast-check test-ast)
;(ast-check sketch)

;(op-enum ctxt-enum 1)

(define syn-sol 
	(synthesize
		#:forall null
		#:guarantee (assert 
			(and 
				(ast-check sketch)
				(sketch->spec sketch input1 output1)
				(sketch->spec sketch input2 output2)
				(sketch->spec sketch input3 output3)
			)
		)))

(define result (evaluate sketch syn-sol))

(display "\n Synthesis Result: \n")
(ast-print result)

