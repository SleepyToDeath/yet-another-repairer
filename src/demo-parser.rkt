#lang racket

(require syntax/parse)
(require "grammar.rkt")
(require "demo-lexer.rkt")

(define (interpret-program program-stx)
  (syntax-parse program-stx
    [({~literal program} stmt-stxs ...)
	 (for ([stmt-stx (syntax->list #'(stmt-stxs ...))])
	      (interpret-stmt stmt-stx))]))

(define (interpret-stmt stmt-stx)
  (syntax-parse stmt-stx
    [({~literal statement} ({~literal stmt-ass} stmt-ass-stx ...))
     (interpret-stmt-ass #'(stmt-ass-stx ...))]
    [({~literal statement} ({~literal stmt-jmp} stmt-jmp-stx ...))
     (interpret-stmt-jmp #'(stmt-jmp-stx ...))]
    [({~literal statement} ({~literal stmt-label} stmt-label-stx ...))
     (interpret-stmt-label #'(stmt-label-stx ...))]
    [({~literal statement} ({~literal stmt-ret} stmt-ret-stx ...))
     (interpret-stmt-ret #'(stmt-ret-stx ...))]))

(define (interpret-stmt-ass stmt-ass-stx)
  (display "ass") (newline))

(define (interpret-stmt-jmp stmt-ass-stx)
  (display "jmp") (newline))

(define (interpret-stmt-label stmt-ass-stx)
  (display "label") (newline))

(define (interpret-stmt-ret stmt-ass-stx)
  (display "ret") (newline))

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

;(syntax->datum parsed-program)

(interpret-program parsed-program)

