#lang rosette/safe

(require rackunit)
(require racket/base)
(require "../src/jimple/jimple-parser.rkt")
(require (prefix-in ast: "../src/syntax-jimple.rkt"))

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
  (check-equal? (build-ast-file (parse-to-stx (string-append
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
  (check-equal? (build-ast-file (parse-to-stx (string-append
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
                          (ast:arguments-callee (ast:argument-callee-list null))
                          (ast:variable-declares (ast:variable-list null))
                          (ast:stats (ast:stat-list null))))))))
                "method test 1")
  (check-equal? (build-ast-file (parse-to-stx (string-append
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
                          (ast:arguments-callee
                            (ast:argument-callee-list (list
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter0")
                                  (ast:type-name "java.lang.String[]")))
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter1")
                                  (ast:type-name "java.util.List"))))))
                          (ast:variable-declares (ast:variable-list null))
                          (ast:stats (ast:stat-list null))))))
                    (ast:function-declares (list
                      (ast:function-declare
                        (ast:function-content
                          (ast:func-name "foo")
                          (ast:arguments-callee
                            (ast:argument-callee-list (list
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter0")
                                  (ast:type-name "int")))
                              (ast:variable-definition
                                (ast:variable-n-type
                                  (ast:variable "@parameter1")
                                  (ast:type-name "double"))))))
                          (ast:variable-declares (ast:variable-list null))
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
    (build-ast-file (parse-to-stx (string-append
      "public class A {"
      "  public void foo() {"
      "    int a, b;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:arguments-callee (ast:argument-callee-list null))
          (ast:variable-declares
            (ast:variable-list (list
              (ast:variable "a")
              (ast:variable "b"))))
          (ast:stats (ast:stat-list null)))))
    "declaration 1")
  (check-equal?
    (build-ast-file (parse-to-stx (string-append
      "public class A {"
      "  public void foo() {"
      "    int a, b;"
      "    doulbe c;"
      "    java.util.List d;"
      "    java.lang.String[] e;"
      "  }"
      "}")))
    (single-func-in-class "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:arguments-callee (ast:argument-callee-list null))
          (ast:variable-declares
            (ast:variable-list (list
              (ast:variable "a")
              (ast:variable "b")
              (ast:variable "c")
              (ast:variable "d")
              (ast:variable "e"))))
          (ast:stats (ast:stat-list null)))))
    "declaration 2")
)

