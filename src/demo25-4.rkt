#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`
(require rosette/solver/smt/z3)

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
	public static void <init>(int)
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
		int $r1;
		$r1 := @parameter0: int;
		return $r1;
	}
}
")))

(define class-B (p:build-ast-file (p:parse-to-stx
"
public class Test
{
	public static int main(int, int, int)
	{
		int r4;
		int r5;
		r5 = 0;
		r4 = 0;
		if r4 != 1 goto label1;
		r5 = 15;
		label1:
		return r5;
	}
}
")))

(define class-0 (p:build-ast-file (p:parse-to-stx
"
public class Test
{
	public static int main(int, int)
	{
		int r1, r2, r3;
		r1 := @parameter0: int;
		r2 := @parameter1: int;
		r3 = r1;
        return r3;
	}
}
")))

(define buggy (program
	(class-list (list class-obj class-A class-B))))

;(define buggy (program
;	(class-list (list class-B))))

;(define buggy (program (class-list (list class-0))))

(pretty-print buggy)

(define input1 (list (cons 6 "int") (cons 5 "int") (cons 4 "int")))
(define output1 (list (cons var-ret-name 15)))

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
(define bugl (localize-bug buggy (list (cons input1 output1))))
(pretty-print bugl)


