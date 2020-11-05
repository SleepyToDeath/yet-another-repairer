#lang rosette/safe

(require (prefix-in std: racket/base))
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

(define class-A (p:build-ast-file (p:parse-to-stx (string-append-newline
  "class A extends java.lang.Object {"
  "  private int a;"
  "  void <init>() {"
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
  "  public void <init>() {"
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
;  "    $i1 = virtualinvoke $r0.<A: int get()>();"
  "    return $i1;"
  "  }"
  "}"))))

(define prog (program
	(class-list (list class-A class-T))))

(ast-check prog)

prog

;(define mac (ast->machine prog))

;(define mac-in (assign-input mac (list (cons "var-1" 1) (cons "var-2" 2))))

;(define mac-comp (compute mac-in))

;(compare-output mac-comp (list (cons var-ret-name 3)))

;(println string-id-map)
