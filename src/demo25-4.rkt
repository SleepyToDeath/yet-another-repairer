#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "match-define.rkt")
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))

(define class-A (p:build-ast-file (p:parse-to-stx
"
class People
{
	public int age;
	public static void <init>(int)
	{
		People r0;
		int i0;
		r0 := @this: People;
		i0 := @parameter0: int;
		r0.<People: int age> = i0;
		return r0;
	}
	public int year_to_adult()
	{
		People r0;
		int $i0, $i1;
		r0 := @this: People;
		$i0 = r0.<People: int age>;
		$i1 = $i0;
		return $i1;
	}
	public boolean is_adult()
	{
		People r0;
		int $i0;
		boolean $z0;
		r0 := @this: People;
		$i0 = virtualinvoke r0.<People: int year_to_adult()>();
		return $i0;
	}
}
")))

(define class-B (p:build-ast-file (p:parse-to-stx
"
public class Test
{
	public static int main()
	{
		People $r0;
		boolean $z0;
		$r0 = new People;
		specialinvoke $r0.<People: void <init>(int)>(15);
		$z0 = virtualinvoke $r0.<People: boolean is_adult()>();
		return $z0;
	}
}
")))


(define buggy (program
	(class-list (list class-A class-B))))

(pretty-print buggy)

(define input1 null)
(define output1 (list (cons var-ret-name 0)))

(match-define (cons soft hard) (ast->relation buggy))

(println string-id-map)

(define tf (hard input1 output1))

;(display "\n")

;(display "Top Formula:\n")
;tf

(display "\nAsserts\n")
(asserts)

(display "\nSolution:\n")
(define debug-sol (optimize #:maximize soft
          #:guarantee (assert tf)))

debug-sol

soft
