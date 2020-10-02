#lang rosette/safe

(require rackunit)
(require racket/base)
(require "../src/jimple/jimple-parser.rkt")

(test-case "file"
  (check-equal? (syntax->datum (text-to-ast "public class A {}"))
                '(file (modifier "public") (file_type "class") (class_name "A") (file_body))
                "file test 1")
  (check-equal? (syntax->datum (text-to-ast "public class A extends B implements C {}"))
                '(file
                   (modifier "public")
                   (file_type "class")
                   (class_name "A")
                   (extends_clause (class_name "B"))
                   (implements_clause (class_name_list (class_name "C")))
                   (file_body))
                "file test 2")
)

