#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "match-define.rkt")
(require "localization.rkt")
(require "string-id.rkt")
(require "map.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")
(require "formula.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))


(define class-0 (p:build-ast-file (p:parse-to-stx
"
public class Test
{
	public static int main()
	{
		int[] $r1;
		$r1 = newarray (int)[r1];
		$r1[r2] = r3;
		return 1;
	}
}
")))

(define buggy (program
	(class-list (list class-0))))

;(define buggy (program (class-list (list class-0))))

(pretty-print buggy)

(define input0 null)
(define output0 (list (cons var-ret-name 0)))

(define input1 (list (cons "r1" 4) (cons "r2" 5) (cons "r3" 6)))
(define output1 (list (cons var-ret-name 1)))

(define mac (ast->machine buggy))
(define mac-in (assign-input mac input1))
(define mac-fin (compute mac-in))
(define result (compare-output mac-fin output1))
result


(pretty-print string-id-table)

(display "===============================================================================================================\n")
(display "================================================ Encoding ... =================================================\n")
(display "===============================================================================================================\n")


(output-smt #t)
(define bugl (localize-bug buggy (list (cons input1 output1))))
(pretty-print bugl)


#|
(output-smt #t)

(clear-pending-eval)

(match-define (cons soft hard) (ast->relation buggy))

(println string-id-map)

;(define tf1 (hard input1 output1))
;(define tf2 (hard input2 output2))
(define tf3 (hard input2 output2))

;(define tf3++ (forall (append (list $r1 $r2 $r3 $ret) all-symbols)
;	(implies 
;		(and 
;			(> $ret 0)
;			(> $r1 0)
;			(> $r2 0)
;			(> $r3 0))
;		(implies 
;			tf3
;			(equal? $ret (+ $r1 $r2 $r3))))))

(display "\n")

(display "Top Formula:\n")
(pretty-print tf3)

(display "\nAsserts\n")
;(pretty-print (asserts))
(pretty-print (length (asserts)))

;(define debug-tf (equal? 0 (list-ref soft 11)))
;(define debug-tf (andmap identity cons-pending))
;(define debug-tf (equal? 1 (first soft)))
(define debug-tf #t)

(display "\nDebug-tf\n")
debug-tf

all-symbols

(define fml-no-bug (equal? (apply + soft) (length soft)))
(define fml-one-bug (equal? (apply + soft) (- (length soft) 1)))

(display "\nSolution:\n")
;(define debug-sol (optimize #:maximize (list (apply + soft))
;          #:guarantee (assert (and tf3 debug-tf))))
;(define nobug-sol (solve (assert tf3)))

(define debug-sol (solve (assert (and tf3 fml-one-bug))))

;nobug-sol

;(pretty-print (fml-to-print tf3))
(display "\n")
(pretty-print string-id-table)

debug-sol
(unsat? debug-sol)
;(core debug-sol)

;soft


|#
