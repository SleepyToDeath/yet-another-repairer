#lang rosette/safe

(require rackunit)
(require (prefix-in std: racket/base))
(require "../src/jimple/jimple-utils.rkt")
(require "../src/jimple/jimple-parser.rkt")
(require (prefix-in ast: "../src/syntax-jimple.rkt"))


(define (string-append-newline . strs)
  (apply std:string-append
    (map (lambda (line)
                 (std:string-append line "\n"))
         strs)))

(define prog-ast
  (build-ast-program (parse-to-stx (string-append-newline
    "public class A {"
    "  public void <init>() { return; }"
    "  public int foo(int, double) {}"
    "  private static void bar(java.lang.String[], java.util.List) {}"
    "}"))))

(define transformed-ast (transform-all prog-ast))

(test-case "trans-init-static"
  (check-equal? (length
                  (ast:function-list-fl
                    (ast:function-declares-rhs
                      (ast:class-default-static-functions
                        (ast:class-def-rhs
                          (car
                            (ast:class-list-cl
                              (ast:program-rhs transformed-ast))))))))
                2
                "static init test 1")
  (check-equal? (length
                  (ast:function-list-fl
                    (ast:function-declares-rhs
                      (ast:class-default-member-functions
                        (ast:class-def-rhs
                          (car
                            (ast:class-list-cl
                              (ast:program-rhs transformed-ast))))))))
                1
                "static init test 2")
)

(test-case "trans-init-return"
  (check-equal? (car
                  (ast:stat-list-sl
                    (ast:stats-rhs
                      (ast:function-content-statements
                        (ast:function-declare-rhs
                          (car
                            (ast:function-list-fl
                              (ast:function-declares-rhs
                                (ast:class-default-static-functions
                                  (ast:class-def-rhs
                                    (car
                                      (ast:class-list-cl
                                        (ast:program-rhs transformed-ast)))))))))))))
                (ast:stat (ast:stat-ret (ast:dexpr (ast:expr-var (ast:variable "r0")))))
                "init return test 1")
)

