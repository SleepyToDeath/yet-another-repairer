#lang rosette

(require brag/support)
(require br-parser-tools/lex)
(require "grammar.rkt")

(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)])

(define (tokenize ip)
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos
       [(:+ digit)
        (token 'INTEGER (string->number lexeme))]
       [(:or "+" "-" "*" "/")
        (token 'AOP lexeme)]
       [(:or "<" "<=" ">" ">=" "==" "!=")
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

(define program-text
  (string-append
    "v1 = x; \n"
    "jmp (v1 < 1) l1; \n"
    "v2 = v1 + 2; \n"
    "jmp (1) l2; \n"
    "label l1: \n"
    "v2 = 2; \n"
    "label l2: \n"
    "v3 = v2; \n"
    "y = v3; \n"))

(define parsed-program
    (parse (tokenize (open-input-string program-text))))

(syntax->datum parsed-program)

