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

(define class-A (p:build-ast-file (p:parse-to-stx
"
public class A {
	public int n;
	static void <init>(int)
	{
		A r0;
		int $r1;
		r0 := @this: A;
		$r1 := @parameter0: int;
		r0.<A: int n> = $r1;
		return r0;
	}

	public int add(int)
	{
		A r0;
		int $r1, $r2, $r3;
		r0 := @this: A;
		$r1 := @parameter0: int;
		$r2 = r0.<A: int n>;
		$r3 = $r1 + $r2;
		if $r3 > 0 goto label1;
		return $r3;
     label1:
		return $r3;
	}
}
")))

(define class-B (p:build-ast-file (p:parse-to-stx
"
public class Test
{
	public static int main()
	{
		int r4;
		int r5;
		A r6;
		r6 = new A;
		specialinvoke r6.<A: void <init>(int)>(r1);
		r4 = virtualinvoke r6.<A: int add(int)>(r2);
		r5 = r4 + r3;
		return r5;
	}
}
")))

(define class-0 (p:build-ast-file (p:parse-to-stx
"
public class Test
{
	public static int main()
	{
		return 0;
	}
}
")))

(define buggy (program
	(class-list (list class-A class-B))))

(pretty-print buggy)

(define input1 (list (cons "r1" 4) (cons "r2" 5) (cons "r3" 6)))
(define output1 (list (cons var-ret-name 15)))

(define input2 (list (cons "r1" 1) (cons "r2" 2) (cons "r3" 3)))
(define output2 (list (cons var-ret-name 6)))

(define-symbolic* $r1 $r2 $r3 $ret integer?)

(define input3 (list (cons "r1" $r1) (cons "r2" $r2) (cons "r3" $r3)))
(define output3 (list (cons var-ret-name $ret)))





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
(define bugl (localize-bug buggy (list (cons input1 output1) (cons input2 output2))))
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
