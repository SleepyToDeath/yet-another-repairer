#lang rosette/safe

(provide text-to-ast)

(require (prefix-in std: racket/base))
(require (prefix-in p: syntax/parse))
(require "jimple-grammar.rkt")
(require "jimple-lexer.rkt")
(require "../syntax.rkt")
(require (prefix-in ast: "../syntax-jimple.rkt"))


; assuming there is only one class in the program
(define (build-program file-text)
  (ast:program (ast:class-list (list (text-to-ast file-text)))))

(define (text-to-ast file-text)
  (build-ast-file (parse (tokenize (std:open-input-string file-text)))))

(define (build-ast-file file-stx)
  (p:syntax-parse file-stx
    [({p:~literal j_file}
        ({p:~literal modifier} modifiers ...)
        ({p:~literal file_type} type)
        ({p:~literal class_name} name)
        (p:~optional ({p:~literal extends_clause} ext))
        (p:~optional ({p:~literal implements_clause} impl))
        ({p:~literal file_body} (p:~optional body)))
     (if (p:attribute body)
         (let ([ret-list (build-ast-file-body #'body)])
           (ast:class-def
             (ast:class-default
               (ast:cls-name (std:syntax-e #'name))
               (ast:field-declares (first ret-list))
               (ast:field-declares (second ret-list))
               (ast:function-declares (third ret-list))
               (ast:function-declares (fourth ret-list)))))
         (ast:class-def
           (ast:class-default
             (ast:cls-name (std:syntax-e #'name))
             (ast:field-declares null)
             (ast:field-declares null)
             (ast:function-declares null)
             (ast:function-declares null))))]))

(define (build-ast-file-body file-body-stx)
  (list null null null null))

