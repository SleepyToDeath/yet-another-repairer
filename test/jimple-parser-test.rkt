#lang rosette/safe

(require rackunit)
(require racket/base)
(require "../src/jimple/jimple-parser.rkt")
(require (prefix-in ast: "../src/syntax-jimple.rkt"))


(define (string-append-newline . strs)
  (apply string-append
    (map (lambda (line)
                 (string-append line "\n"))
         strs)))


(test-case "file"
  (check-equal? (build-ast-file (parse-to-stx "public class A {}"))
                (ast:class-def
                  (ast:class-default
                    (ast:type-name "A")
                    (ast:type-name #f)
                    (ast:interface-implements null)
                    (ast:field-declares null)
                    (ast:field-declares null)
                    (ast:function-declares null)
                    (ast:function-declares null)))
                "file test 1")
  (check-equal? (build-ast-file (parse-to-stx "public class A extends B implements C, D {}"))
                (ast:class-def
                  (ast:class-default
                    (ast:type-name "A")
                    (ast:type-name "B")
                    (ast:interface-implements (list (ast:type-name "C") (ast:type-name "D")))
                    (ast:field-declares null)
                    (ast:field-declares null)
                    (ast:function-declares null)
                    (ast:function-declares null)))
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
                    (ast:interface-implements null)
                    (ast:field-declares (list (ast:field "a")))
                    (ast:field-declares (list (ast:field "b") (ast:field "c")))
                    (ast:function-declares null)
                    (ast:function-declares null)))
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
                    (ast:interface-implements null)
                    (ast:field-declares null)
                    (ast:field-declares null)
                    (ast:function-declares null)
                    (ast:function-declares (list
                      (ast:function-declare
                        (ast:function-content
                          (ast:func-name "foo")
                          (ast:variable-definitions (ast:variable-definition-list null))
                          (ast:variable-definitions (ast:variable-definition-list null))
                          (ast:stats (ast:stat-list null))))))))
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
                    (ast:interface-implements null)
                    (ast:field-declares null)
                    (ast:field-declares null)
                    (ast:function-declares (list
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
                          (ast:stats (ast:stat-list null))))))
                    (ast:function-declares (list
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
                          (ast:stats (ast:stat-list null))))))))
                "method test 2")
)

(define (single-func-in-class class-name func-decl)
  (ast:class-def
    (ast:class-default
      (ast:type-name class-name)
      (ast:type-name #f)
      (ast:interface-implements null)
      (ast:field-declares null)
      (ast:field-declares null)
      (ast:function-declares null)
      (ast:function-declares (list func-decl)))))


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
              (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "a")))
                (ast:expr (ast:expr-var (ast:variable "b"))))))))))
    "stmt assigment 1")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    java.lang.String a;"
      "    a = \"bbb\";"
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
                (ast:variable-n-type (ast:variable "a") (ast:type-name "java.lang.String"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "a")))
                (ast:expr (ast:expr-const (ast:const "bbb"))))))))))
    "stmt assigment 2")
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
              (ast:stat-label (ast:label "l1"))))))))
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
              (ast:stat-jmp
                (ast:expr (ast:expr-const (ast:const #t)))
                (ast:label "l1"))))))))
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
              (ast:stat-nop (ast:nop #f))))))))
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
              (ast:stat-ret (ast:dexpr (ast:expr-const void-return-value)))))))))
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
              (ast:stat-ret (ast:dexpr (ast:expr-var (ast:variable "a"))))))))))
    "stmt return 2")
)

