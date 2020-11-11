#lang racket

(require brag/support)
(require br-parser-tools/lex)

(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)])

(define (tokenize ip)
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos
       [(:+ digit)
        (token 'INTEGER (string->number lexeme))]
       [(:or "+" "-" "*" "/" "<" "<=" ">" ">=" "==" "!=")
        (token 'BOP lexeme)]
       ["("
        (token "(" lexeme)]
       [")"
        (token ")" lexeme)]
       [";"
        (token ";" lexeme)]
       [":"
        (token ":" lexeme)]
       ["="
        (token "=" lexeme)]
       ["jmp"
        (token "jmp" lexeme)]
       ["label"
        (token "label" lexeme)]
       ["return"
        (token "return" lexeme)]
       [(:: (:or letter "_") (:* (:or letter digit "_")))
        (token 'ID lexeme)]
       [whitespace
        (token 'WHITESPACE lexeme #:skip? #t)]
       [(eof)
        (void)]))
    (define (next-token) (my-lexer ip))
    next-token)

(provide tokenize)

