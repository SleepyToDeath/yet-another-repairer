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
(require "semantics-common.rkt")
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
	public static int main(int, int, int)
	{
		int r1, r2, r3;
		int r4;
		int r5;
		A r6;
		r1 := @parameter0: int;
		r2 := @parameter1: int;
		r3 := @parameter2: int;
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
	public static int main(int)
	{
//        int i0;
        int $b1;

//        i0 := @parameter0: int;

        $b1 = 0;

        goto label1;

     label1:
        return $b1;
	}
}
")))

(define buggy (program
	(class-list (list class-A class-B))))

;(define buggy (program (class-list (list class-0))))

(pretty-print buggy)

(define input1 (list (cons 4 "int") (cons 5 "int") (cons 6 "int")))
(define output1 (list (cons var-ret-name 15)))

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


