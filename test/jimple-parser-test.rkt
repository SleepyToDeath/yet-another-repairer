#lang rosette/safe

(require rackunit)
(require (prefix-in std: racket/base))
(require "../src/jimple/jimple-parser.rkt")
(require (prefix-in ast: "../src/syntax-jimple.rkt"))


(define (string-append-newline . strs)
  (apply std:string-append
    (map (lambda (line)
                 (std:string-append line "\n"))
         strs)))


(test-case "file"
  (check-equal? (build-ast-file (parse-to-stx "public class A {}"))
                (ast:class-def
                  (ast:class-default
                    (ast:type-name "A")
                    (ast:type-name #f)
                    (ast:interface-implements (ast:interface-name-list null))
                    (ast:field-declares (ast:field-list null))
                    (ast:field-declares (ast:field-list null))
                    (ast:function-declares (ast:function-list null))
                    (ast:function-declares (ast:function-list null))))
                "file test 1")
  (check-equal? (build-ast-file (parse-to-stx "public class A extends B implements C, D {}"))
                (ast:class-def
                  (ast:class-default
                    (ast:type-name "A")
                    (ast:type-name "B")
                    (ast:interface-implements
                      (ast:interface-name-list (list (ast:type-name "C") (ast:type-name "D"))))
                    (ast:field-declares (ast:field-list null))
                    (ast:field-declares (ast:field-list null))
                    (ast:function-declares (ast:function-list null))
                    (ast:function-declares (ast:function-list null))))
                "file test 2")
)

(test-case "field"
  (check-equal? (build-ast-file (parse-to-stx (string-append-newline
                    "public class A {"
                    "  public static int a;"
                    "  private double b;"
                    "  float c;"
                    "}")))
                (ast:class-def
                  (ast:class-default
                    (ast:type-name "A")
                    (ast:type-name #f)
                    (ast:interface-implements (ast:interface-name-list null))
                    (ast:field-declares (ast:field-list (list (ast:field "a"))))
                    (ast:field-declares (ast:field-list (list (ast:field "b") (ast:field "c"))))
                    (ast:function-declares (ast:function-list null))
                    (ast:function-declares (ast:function-list null))))
                "field test 1")
)

(test-case "method"
  (check-equal? (build-ast-file (parse-to-stx (string-append-newline
                    "public class A {"
                    "  public void foo();"
                    "}")))
                (ast:class-def
                  (ast:class-default
                    (ast:type-name "A")
                    (ast:type-name #f)
                    (ast:interface-implements (ast:interface-name-list null))
                    (ast:field-declares (ast:field-list null))
                    (ast:field-declares (ast:field-list null))
                    (ast:function-declares (ast:function-list null))
                    (ast:function-declares (ast:function-list (list
                      (ast:function-declare
                        (ast:function-content
                          (ast:func-name "foo")
                          (ast:variable-definitions (ast:variable-definition-list null))
                          (ast:variable-definitions (ast:variable-definition-list null))
                          (ast:stats (ast:stat-list null)))))))))
                "method test 1")
  (check-equal? (build-ast-file (parse-to-stx (string-append-newline
                    "public class A {"
                    "  public int foo(int, double) {}"
                    "  private static void bar(java.lang.String[], java.util.List) {}"
                    "}")))
                (ast:class-def
                  (ast:class-default
                    (ast:type-name "A")
                    (ast:type-name #f)
                    (ast:interface-implements (ast:interface-name-list null))
                    (ast:field-declares (ast:field-list null))
                    (ast:field-declares (ast:field-list null))
                    (ast:function-declares (ast:function-list (list
                      (ast:function-declare
                        (ast:function-content
                          (ast:func-name "bar")
                          (ast:variable-definitions
                            (ast:variable-definition-list (list
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter0")
                                  (ast:type-name "java.lang.String[]")))
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter1")
                                  (ast:type-name "java.util.List"))))))
                          (ast:variable-definitions (ast:variable-definition-list null))
                          (ast:stats (ast:stat-list null)))))))
                    (ast:function-declares (ast:function-list (list
                      (ast:function-declare
                        (ast:function-content
                          (ast:func-name "foo")
                          (ast:variable-definitions
                            (ast:variable-definition-list (list
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter0")
                                  (ast:type-name "int")))
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter1")
                                  (ast:type-name "double"))))))
                          (ast:variable-definitions (ast:variable-definition-list null))
                          (ast:stats (ast:stat-list null)))))))))
                "method test 2")
)

(define (single-func-in-class class-name func-decl)
  (ast:class-def
    (ast:class-default
      (ast:type-name class-name)
      (ast:type-name #f)
      (ast:interface-implements (ast:interface-name-list null))
      (ast:field-declares (ast:field-list null))
      (ast:field-declares (ast:field-list null))
      (ast:function-declares (ast:function-list null))
      (ast:function-declares (ast:function-list (list func-decl))))))


(test-case "declaration"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int a, b;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "a") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "b") (ast:type-name "int"))))))
          (ast:stats (ast:stat-list null)))))
    "declaration 1")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int a, b;"
      "    double c;"
      "    java.util.List d;"
      "    java.lang.String[] e;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "a") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "b") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "c") (ast:type-name "double")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "d") (ast:type-name "java.util.List")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "e") (ast:type-name "java.lang.String[]"))))))
          (ast:stats (ast:stat-list null)))))
    "declaration 2")
)

(test-case "stmt-assignment"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int a, b;"
      "    a = b;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "a") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "b") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "a")))
                (ast:expr (ast:expr-var (ast:variable "b")))))))))))
    "stmt assigment 1")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int a1, a2;"
      "    double b1, b2;"
      "    java.lang.String c;"
      "    A d;"
      "    a1 = 1;"
      "    a2 = -1;"
      "    b1 = 1.5;"
      "    b2 = -1.5;"
      "    c = \"str\";"
      "    d = null;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "a1") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "a2") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "b1") (ast:type-name "double")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "b2") (ast:type-name "double")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "c") (ast:type-name "java.lang.String")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "d") (ast:type-name "A"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "a1")))
                (ast:expr (ast:expr-const (ast:const 1)))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "a2")))
                (ast:expr (ast:expr-const (ast:const -1)))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "b1")))
                (ast:expr (ast:expr-const (ast:const 1.5)))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "b2")))
                (ast:expr (ast:expr-const (ast:const -1.5)))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "c")))
                (ast:expr (ast:expr-const (ast:const "str")))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "d")))
                (ast:expr (ast:expr-const (ast:const "null")))))))))))
    "stmt assigment 2")
)

(test-case "stmt-new"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    B b;"
      "    b = new B;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "b") (ast:type-name "B"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-new (ast:variable "b")))))))))
    "stmt new 1")
)

(test-case "stmt-identity"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    A r0;"
      "    r0 := @this: A;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "r0") (ast:type-name "A"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "r0")))
                (ast:expr (ast:expr-var (ast:variable "@this")))))))))))
    "stmt identity 1")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo(int) {"
      "    int r1;"
      "    r1 := @parameter0: int;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "@parameter0") (ast:type-name "int"))))))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "r1") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "r1")))
                (ast:expr (ast:expr-var (ast:variable "@parameter0")))))))))))
    "stmt identity 2")
)


(test-case "stmt-identity-nt"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo(int) {"
      "    int r1;"
      "    r1 := @parameter0;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "@parameter0") (ast:type-name "int"))))))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "r1") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "r1")))
                (ast:expr (ast:expr-var (ast:variable "@parameter0")))))))))))
    "stmt identity no type 1")
)

(test-case "stmt-label"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    l1:"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-label (ast:label "l1")))))))))
    "stmt label 1")
)

(test-case "stmt-goto"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    goto l1;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-jmp
                (ast:expr (ast:expr-const (ast:const #t)))
                (ast:label "l1")))))))))
    "stmt goto 1")
)

(test-case "stmt-nop"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    nop;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-nop (ast:nop #f)))))))))
    "stmt nop 1")
)

(test-case "stmt-return"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    return;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ret (ast:dexpr (ast:expr-const void-return-value))))))))))
    "stmt return 1")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public int foo() {"
      "    int a;"
      "    return a;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "a") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ret (ast:dexpr (ast:expr-var (ast:variable "a")))))))))))
    "stmt return 2")
)

(test-case "stmt-invoke"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    T r0;"
      "    specialinvoke r0.<java.lang.Object: void <init>()>();"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "r0") (ast:type-name "T"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-special-call
                (ast:variable "r0")
                (ast:variable "r0")
                (ast:type-name "java.lang.Object")
                (ast:func-name "<init>")
                (ast:types (ast:type-list null))
                (ast:arguments-caller (ast:argument-caller-list null))))))))))
    "stmt invoke 1")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    T r0;"
      "    int a, b;"
      "    virtualinvoke r0.<T: int bar(int, int)>(a, b);"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition (ast:variable-n-type (ast:variable "r0") (ast:type-name "T")))
              (ast:variable-definition (ast:variable-n-type (ast:variable "a") (ast:type-name "int")))
              (ast:variable-definition (ast:variable-n-type (ast:variable "b") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-virtual-call
                void-return-var
                (ast:variable "r0")
                (ast:type-name "T")
                (ast:func-name "bar")
                (ast:types (ast:type-list (list (ast:type-name "int") (ast:type-name "int"))))
                (ast:arguments-caller (ast:argument-caller-list (list
                  (ast:dexpr (ast:expr-var (ast:variable "a")))
                  (ast:dexpr (ast:expr-var (ast:variable "b"))))))))))))))
    "stmt invoke 2")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int a;"
      "    staticinvoke <T: int baz(int)>(a);"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition (ast:variable-n-type (ast:variable "a") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-static-call
                void-return-var
                (ast:type-name "T")
                (ast:func-name "baz")
                (ast:types (ast:type-list (list (ast:type-name "int"))))
                (ast:arguments-caller (ast:argument-caller-list (list
                  (ast:dexpr (ast:expr-var (ast:variable "a"))))))))))))))
    "stmt invoke 3")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    T r0;"
      "    int x, a, b;"
      "    x = virtualinvoke r0.<T: int bar(int, int)>(a, b);"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition (ast:variable-n-type (ast:variable "r0") (ast:type-name "T")))
              (ast:variable-definition (ast:variable-n-type (ast:variable "x") (ast:type-name "int")))
              (ast:variable-definition (ast:variable-n-type (ast:variable "a") (ast:type-name "int")))
              (ast:variable-definition (ast:variable-n-type (ast:variable "b") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-virtual-call
                (ast:variable "x")
                (ast:variable "r0")
                (ast:type-name "T")
                (ast:func-name "bar")
                (ast:types (ast:type-list (list (ast:type-name "int") (ast:type-name "int"))))
                (ast:arguments-caller (ast:argument-caller-list (list
                  (ast:dexpr (ast:expr-var (ast:variable "a")))
                  (ast:dexpr (ast:expr-var (ast:variable "b"))))))))))))))
    "stmt invoke 4")
)

(test-case "expr-array-ref"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int[] x;"
      "    int y, z;"
      "    y = x[z];"
      "    x[y] = z;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "x") (ast:type-name "int[]")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "y") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "z") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "y")))
                (ast:expr (ast:expr-array (ast:variable "x") (ast:expr (ast:expr-var (ast:variable "z")))))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-array (ast:variable "x") (ast:expr (ast:expr-var (ast:variable "y")))))
                (ast:expr (ast:expr-var (ast:variable "z")))))))))))
    "expr array reference 1")
)

(test-case "expr-field-ref"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int x, y;"
      "    B b;"
      "    b.<B: int f1> = x;"
      "    y = b.<B: int f2>;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "x") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "y") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "b") (ast:type-name "B"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-field (ast:variable "b") (ast:type-name "B") (ast:field "f1")))
                (ast:expr (ast:expr-var (ast:variable "x")))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "y")))
                (ast:expr (ast:expr-field (ast:variable "b") (ast:type-name "B") (ast:field "f2")))))))))))
    "expr field reference 1")
)

(test-case "expr-binop"
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    int x, y;"
      "    bool z;"
      "    y = x + 1;"
      "    z = x > y;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "x") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "y") (ast:type-name "int")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "z") (ast:type-name "bool"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "y")))
                (ast:expr (ast:expr-binary
                  (ast:expr (ast:expr-var (ast:variable "x")))
                  (ast:op +)
                  (ast:expr (ast:expr-const (ast:const 1)))))))
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "z")))
                (ast:expr (ast:expr-binary
                  (ast:expr (ast:expr-var (ast:variable "x")))
                  (ast:op >)
                  (ast:expr (ast:expr-var (ast:variable "y")))))))))))))
    "expr binary operation 1")
)

