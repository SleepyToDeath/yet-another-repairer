#lang rosette/safe

(require rackunit)
(require racket/base)
(require "../src/jimple/jimple-parser.rkt")

(test-case "file"
  (check-equal? (syntax->datum (text-to-ast "public class A {}"))
                "A"
                "file test 1")
  (check-equal? (syntax->datum (text-to-ast "public class A extends B implements C {}"))
                "A"
                "file test 2")
)

