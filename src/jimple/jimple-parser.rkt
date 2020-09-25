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

; file ::= modifier* file_type class_name extends_clause? implements_clause? file_body 
(define (build-ast-file file-stx)
  file-stx)

