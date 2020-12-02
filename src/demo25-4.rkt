#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "match-define.rkt")
(require "string-id.rkt")
(require "map.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")
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
		r5 = r4 - r3;
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
	(class-list (list class-0))))

(pretty-print buggy)

;(define input1 (list (cons "r1" 4) (cons "r2" 5) (cons "r3" 6)))
;(define output1 (list (cons var-ret-name 15)))

(define input2 (list (cons "r1" 1) (cons "r2" 2) (cons "r3" 3)))
(define output2 (list (cons var-ret-name 6)))






(define mac (ast->machine buggy))
(define mac-in (assign-input mac input2))
(define mac-fin (compute mac-in))
(define result (compare-output mac-fin output2))
result



(display "===============================================================================================================\n")
(display "================================================ Encoding ... =================================================\n")
(display "===============================================================================================================\n")




(output-smt #t)

(match-define (cons soft hard) (ast->relation buggy))

(println string-id-map)

;(define tf1 (hard input1 output1))
(define tf2 (hard input2 output2))

(display "\n")

(display "Top Formula:\n")
(pretty-print tf2)

(display "\nAsserts\n")
;(pretty-print (asserts))
(pretty-print (length (asserts)))

;(define debug-tf (equal? 0 (list-ref soft 11)))
(define debug-tf (andmap identity cons-pending))

(display "\nDebug-tf\n")
debug-tf

(display "\nSolution:\n")
(define debug-sol (optimize #:maximize (list (apply + soft))
          #:guarantee (assert (and tf2 debug-tf))))

debug-sol

soft

(display "\n")
(pretty-print string-id-table)


