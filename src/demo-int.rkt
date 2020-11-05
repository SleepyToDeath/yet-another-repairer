#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-computational.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))

(define class-A (p:build-ast-file (p:parse-to-stx
"
public class A {
  private int f1;
  public int f2;
  public void <init>() {
    A r0;
    r0 := @this: A;
    r0.<A: int f2> = 2;
    return;
  }
  public void setF1(int) {
    A r0;
    int i0;
    r0 := @this: A;
    i0 := @parameter0: int;
    r0.<A: int f1> = i0;
    return;
  }
  public int getF1() {
    A r0;
    int $i0;
    r0 := @this: A;
    $i0 = r0.<A: int f1>;
    return $i0;
  }
}")))

(define class-T (p:build-ast-file (p:parse-to-stx
"
public class T {
  public void <init>() {
    T r0;
    r0 := @this: T;
    return;
  }
  public static int main() {
    A $r0;
    int i0, i1, $i2, i3;
    i0 := @parameter0: int;
    $r0 = new A;
    specialinvoke $r0.<A: void <init>()>();
    virtualinvoke $r0.<A: void setF1(int)>(i0);
    i1 = virtualinvoke $r0.<A: int getF1()>();
    $i2 = $r0.<A: int f2>;
    i3 = $i2 + i1;
    return i3;
  }
}")))

(define prog (program
	(class-list (list class-A class-T))))

(ast-check prog)

(pretty-print prog)

(define input (list (cons "@parameter0" 1)))
(define output (list (cons var-ret-name 3)))

(compare-output (compute (assign-input (ast->machine prog) input)) output)

