#lang rosette/safe

(provide text-to-ast)

(require (prefix-in std: racket/base))
(require (prefix-in p: syntax/parse))
(require "jimple-grammar.rkt")
(require "jimple-lexer.rkt")
(require "../syntax.rkt")
(require (prefix-in ast: "../syntax-jimple.rkt"))


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
     #'name]))

