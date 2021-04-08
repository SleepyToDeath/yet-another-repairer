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
(require "memory-common.rkt")
(require "formula.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))


(define class-obj (p:build-ast-file (p:parse-to-stx
"
public class java.lang.Object
{
	public int __CLASS__;
	public static void <init>() {
        java.lang.Object r0;

        r0 := @this: java.lang.Object;

		return r0;
	}

    public boolean equals(java.lang.Object)
	{
        java.lang.Object r0, r1;

        r0 := @this: java.lang.Object;

        r1 := @parameter0: java.lang.Object;

        if r0 != r1 goto label1;

        return 1;

     label1:
        return 0;
	}

	//not real clone
	public java.lang.Object clone()
	{
        java.lang.Object r0;

        r0 := @this: java.lang.Object;
		
		return r0;
	}

}
")))

(define class-A (p:build-ast-file (p:parse-to-stx
"
public class A extends java.lang.Object {
	public int n;
	static void <init>(int)
	{
		A r0;
		int $r1;
		r0 := @this: A;
        specialinvoke r0.<java.lang.Object: void <init>()>();
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
	public static int main(int, int, int)
	{
		int r1, r2, r3;
		int r4, r44;
		int r5;
		A r6;
		r1 := @parameter0: int;
		r2 := @parameter1: int;
		r3 := @parameter2: int;
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
	public static int main(long)
	{
		long l0, l1;
		l0 := @parameter0: long;
        l1 = l0 & 281474976710655L;
        return 15;
	}
}
")))

(define buggy (program
	(class-list (list class-obj class-A class-B))))

;(define buggy (program (class-list (list class-0))))

(pretty-print buggy)

(define input1 (list (cons 4 "int") (cons 5 "int") (cons 6 "int")))
;(define input1 (list (cons (bv 123 bv-type) "long")))
;(define input1 (list (cons 1 "int")))
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


