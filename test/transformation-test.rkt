#lang rosette/safe

(require rackunit)
(require (prefix-in std: racket/base))
(require "../src/jimple/jimple-parser.rkt")
(require "../src/jimple/jimple-utils.rkt")
(require (prefix-in ast: "../src/syntax-jimple.rkt"))


(define (string-append-newline . strs)
  (apply std:string-append
    (map (lambda (line)
                 (std:string-append line "\n"))
         strs)))


(define (single-virtual-func-in-program class-name func-decl)
  (ast:program
    (ast:class-list (list
      (ast:class-def
        (ast:class-default
          (ast:type-name class-name)
          (ast:type-name #f)
          (ast:interface-implements (ast:interface-name-list null))
          (ast:field-declares (ast:field-list null))
          (ast:field-declares (ast:field-list null))
          (ast:function-declares (ast:function-list null))
          (ast:function-declares (ast:function-list (list func-decl)))))))))


(define (single-static-func-in-program class-name func-decl)
  (ast:program
    (ast:class-list (list
      (ast:class-def
        (ast:class-default
          (ast:type-name class-name)
          (ast:type-name #f)
          (ast:interface-implements (ast:interface-name-list null))
          (ast:field-declares (ast:field-list null))
          (ast:field-declares (ast:field-list null))
          (ast:function-declares (ast:function-list (list func-decl)))
          (ast:function-declares (ast:function-list null))))))))


(test-case "get-class"
  (check-equal?
    (transform-all (ast:program (ast:class-list (list (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public void foo() {"
      "    java.lang.String x;"
      "    java.lang.Class y;"
      "    y = virtualinvoke x.<java.lang.Object: java.lang.Class getClass()>();"
      "  }"
      "}")))))))
    (single-virtual-func-in-program "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "foo")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "x") (ast:type-name "java.lang.String")))
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "y") (ast:type-name "java.lang.Class"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "y")))
                (ast:expr (ast:expr-field
                  (ast:variable "x")
                  (ast:type-name "java.lang.Object")
                  (ast:field "__CLASS__")))))))))))
    "getClass 1")
)

(test-case "remove-input-var"
  (check-equal?
    (transform-all (ast:program (ast:class-list (list (build-ast-file (parse-to-stx (string-append-newline
      "public class A {"
      "  public static int main() {"
      "    int input;"
      "    int x;"
      "    input = 10;"
      "    x = input;"
      "  }"
      "}")))))))
    (single-static-func-in-program "A"
      (ast:function-declare
        (ast:function-content
          (ast:func-name "main")
          (ast:variable-definitions (ast:variable-definition-list null))
          (ast:variable-definitions
            (ast:variable-definition-list (list
              (ast:variable-definition
                (ast:variable-n-type (ast:variable "x") (ast:type-name "int"))))))
          (ast:stats
            (ast:stat-list (list
              (ast:stat (ast:stat-ass
                (ast:lexpr (ast:expr-var (ast:variable "x")))
                (ast:expr (ast:expr-var (ast:variable "input")))))))))))
    "remove input variable 1")
)

