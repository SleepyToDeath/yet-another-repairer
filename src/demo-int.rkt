#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-computational.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))

(define (string-append-newline . strs)
  (apply std:string-append
    (map (lambda (line)
                 (std:string-append line "\n"))
         strs)))

(define class-Object (p:build-ast-file (p:parse-to-stx (string-append-newline
  "public class java.lang.Object {"
  "  public static void <init>() {"
  "  }"
  "}"))))

(define class-A (p:build-ast-file (p:parse-to-stx (string-append-newline
  "class A extends java.lang.Object {"
  "  private int a;"
  "  static void <init>() {"
  "    A r0;"
  "    r0 := @this: A;"
  "    specialinvoke r0.<java.lang.Object: void <init>()>();"
  "    return;"
  "  }"
  "  public void set(int) {"
  "    A r0;"
  "    int i0;"
  "    r0 := @this: A;"
  "    i0 := @parameter0: int;"
  "    r0.<A: int a> = i0;"
  "    return;"
  "  }"
  "  public int get() {"
  "    A r0;"
  "    int $i0;"
  "    r0 := @this: A;"
  "    $i0 = r0.<A: int a>;"
  "    return $i0;"
  "  }"
  "}"))))

(define class-T (p:build-ast-file (p:parse-to-stx (string-append-newline
  "public class T extends java.lang.Object {"
  "  public static void <init>() {"
  "    T r0;"
  "    r0 := @this: T;"
  "    specialinvoke r0.<java.lang.Object: void <init>()>();"
  "    return;"
  "  }"
  "  public int foo(int) {"
  "    A $r0;"
  "    int i0, $i1;"
  "    T r2;"
  "    r2 := @this: T;"
  "    i0 := @parameter0: int;"
  "    $r0 = new A;"
  "    specialinvoke $r0.<A: void <init>()>();"
  "    if i0 <= 0 goto label1;"
  "    virtualinvoke $r0.<A: void set(int)>(10);"
  "    goto label2;"
  "  label1:"
  "    virtualinvoke $r0.<A: void set(int)>(20);"
  "  label2:"
  "    $i1 = virtualinvoke $r0.<A: int get()>();"
  "    return $i1;"
  "  }"
  "  public static int main() {"  
  "    T $r0;"
  "    int i0;"
  "    $r0 = new T;"
  "    specialinvoke $r0.<T: void <init>()>();"
  "    i0 = virtualinvoke $r0.<T: int foo(int)>(15);"
  "    return i0;"
  "  }"
  "}"))))

(define prog (program
	(class-list (list class-Object class-A class-T))))

(ast-check prog)

(pretty-print prog)

(define input null)
(define output (list (cons var-ret-name 10)))

(compare-output (compute (assign-input (ast->machine prog) input)) output)

