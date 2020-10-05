#lang rosette/safe

(require rackunit)
(require racket/base)
(require "../src/jimple/jimple-parser.rkt")
(require (prefix-in ast: "../src/syntax-jimple.rkt"))

(test-case "file"
  (check-equal? (build-ast-file (parse-to-stx "public class A {}"))
                (ast:class-def
                  (ast:class-default
                    (ast:cls-name "A")
                    (ast:field-declares null)
                    (ast:field-declares null)
                    (ast:function-declares null)
                    (ast:function-declares null)))
                "file test 1")
  (check-equal? (build-ast-file (parse-to-stx "public class A extends B implements C {}"))
                (ast:class-def
                  (ast:class-default
                    (ast:cls-name "A")
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
                    (ast:cls-name "A")
                    (ast:field-declares (list (ast:field "a")))
                    (ast:field-declares (list (ast:field "b") (ast:field "c")))
                    (ast:function-declares null)
                    (ast:function-declares null)))
                "field test 1")
)

