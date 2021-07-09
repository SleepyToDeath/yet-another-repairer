#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`
(require rosette/solver/smt/z3)

(require (prefix-in p: "../jimple/jimple-parser.rkt"))
(require "../match-define.rkt")
(require "../localization.rkt")
(require "../string-id.rkt")
(require "../map.rkt")
(require "../syntax.rkt")
(require "../syntax-jimple.rkt")
(require "../semantics-relational.rkt")
(require "../semantics-computational.rkt")
(require "../semantics-common.rkt")
(require "../memory-common.rkt")
(require "../formula.rkt")
(require (prefix-in p: "../jimple/jimple-parser.rkt"))


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

(define class-B (p:build-ast-file (p:parse-to-stx
"
public class B extends java.lang.Object {
	public static void <init>()
	{
		B r0;
		r0 := @this: B;
        specialinvoke r0.<java.lang.Object: void <init>()>();
		return;
	}

	public boolean funcB()
	{
		return 1;
	}
}
")))

(define class-A (p:build-ast-file (p:parse-to-stx
"
public class A extends java.lang.Object {
	public static void <init>()
	{
		A r0;
		r0 := @this: A;
        specialinvoke r0.<java.lang.Object: void <init>()>();
		return;
	}

	public boolean funcA2()
	{
		return 1;
	}

	public boolean funcA(A)
	{
        A r0;
        boolean $z1;

        r0 := @this: A;

        if r0 != null goto label1;

		$z1 = virtualinvoke r0.<A: boolean funcA2()>();

     label1:
        return 1;
	}


}
")))

(define class-Main (p:build-ast-file (p:parse-to-stx
"
public class Test extends java.lang.Object
{
	public static int main(long)
	{
		A a;
		a = new A;
        specialinvoke a.<A: void <init>()>();
		virtualinvoke a.<A: boolean funcA(A)>(a);
		return 1;
	}
}
")))

(define buggy (program
	(class-list (list class-obj class-A class-B class-Main))))

(define input1 (list (cons (bv 200 bv-type) "long")))
(define output1 (list (cons var-ret-name 1)))

;(define input1 (list (cons 1 "int") (cons 2 "int")))
;(define output1 (list (cons var-ret-name 2)))

(define mac (ast->machine buggy))
(define mac-in (assign-input mac input1))
(define mac-fin (compute mac-in))
(define result (compare-output mac-fin output1))
result


(pretty-print string-id-table)

(display "===============================================================================================================\n")
(display "================================================ Encoding ... =================================================\n")
(display "===============================================================================================================\n")

(current-solver (z3 #:options
   (std:hash ':produce-unsat-cores 'true
         ':auto-config 'true
         ':smt.relevancy 2
         ':smt.mbqi.max_iterations 10000000)))

(output-smt #t)
(pretty-print (solver-features (current-solver)))
(pretty-print (solver-options (current-solver)))
(define bugl (localize-bug buggy null (list (cons input1 output1))))
(pretty-print bugl)

