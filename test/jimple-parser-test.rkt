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
;  (check-equal? (build-ast-file (parse-to-stx (string-append
;                    "public class A {"
;                    "  public int foo(int a, double b) {}"
;                    "  private static void bar(List c, Set d) {}"
;                    "}")))
;                (ast:class-def
;                  (ast:class-default
;                    (ast:type-name "A")
;                    (ast:type-name #f)
;                    (ast:interface-implements null)
;                    (ast:field-declares null)
;                    (ast:field-declares null)
;                    (ast:function-declares null)
;                    (ast:function-declares null)))
;                "method test 2")
)

