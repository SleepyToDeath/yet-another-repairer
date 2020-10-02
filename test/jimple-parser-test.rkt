#lang rosette/safe

(require rackunit)
(require racket/base)
(require "../src/jimple/jimple-parser.rkt")
(require (prefix-in ast: "../src/syntax-jimple.rkt"))

(test-case "file"
  (check-equal? (text-to-ast "public class A {}")
                (ast:class-def
                  (ast:class-default
                    (ast:cls-name "A")
                    (ast:field-declares null)
                    (ast:field-declares null)
                    (ast:function-declares null)
                    (ast:function-declares null)))
                "file test 1")
  (check-equal? (text-to-ast "public class A extends B implements C {}")
                (ast:class-def
                  (ast:class-default
                    (ast:cls-name "A")
                    (ast:field-declares null)
                    (ast:field-declares null)
                    (ast:function-declares null)
                    (ast:function-declares null)))
                "file test 2")
)

