#lang racket

(require (prefix-in demo: syntax/parse))
(require "grammar.rkt")
(require "demo-lexer.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")

(define (interpret-program program-stx)
  (demo:syntax-parse program-stx
    [({demo:~literal program} stmt-stxs ...)
     (begin
       (define tree
         (stats (stats-single (stat (stat-nop (nop 0))))))
       (for ([stmt-stx (syntax->list #'(stmt-stxs ...))])
            (set! tree (stats (stats-multi tree (stats (stats-single (interpret-stmt stmt-stx)))))))
       tree)]))

(define (interpret-stmt stmt-stx)
  (demo:syntax-parse stmt-stx
    [({demo:~literal statement} ({demo:~literal stmt-ass} stmt-ass-stx ...))
     (interpret-stmt-ass #'(stmt-ass-stx ...))]
    [({demo:~literal statement} ({demo:~literal stmt-jmp} stmt-jmp-stx ...))
     (interpret-stmt-jmp #'(stmt-jmp-stx ...))]
    [({demo:~literal statement} ({demo:~literal stmt-label} stmt-label-stx ...))
     (interpret-stmt-label #'(stmt-label-stx ...))]
    [({demo:~literal statement} ({demo:~literal stmt-ret} stmt-ret-stx ...))
     (interpret-stmt-ret #'(stmt-ret-stx ...))]))

(define (interpret-stmt-ass stmt-ass-stx)
  (demo:syntax-parse stmt-ass-stx
    [(id-stx "=" expr-stx ";")
     (stat-ass
       (interpret-id #'id-stx)
       (interpret-expr #'expr-stx))]))

(define (interpret-stmt-jmp stmt-jmp-stx)
  (demo:syntax-parse stmt-jmp-stx
    [("jmp" expr-stx label-stx ";")
     (stat-jmp
       (interpret-expr #'expr-stx)
       (interpret-label #'label-stx))]))

(define (interpret-stmt-label stmt-label-stx)
  (demo:syntax-parse stmt-label-stx
    [("label" label-stx ":")
     (stat-label
       (interpret-label #'label-stx))]))

(define (interpret-stmt-ret stmt-ret-stx)
  (stat-ret (nop 0)))

(define (interpret-expr expr-stx)
  (variable 1000))
;  (demo:syntax-parse expr-stx
;    [({demo:~literal j-expr} ({demo:~literal ident} id-stx))
;     (interpret-id #'id-stx)]
;    [({demo:~literal j-expr} "(" paren-stx ")")
;     (interpret-expr-paren #'paren-stx)]
;    [({demo:~literal j-expr} ({demo:~literal expr} lhs-stx) op-text ({demo:~literal expr} rhs-stx))
;     (variable 1000)]
;))

(define (interpret-label label-stx)
  (demo:syntax-parse label-stx
    [({demo:~literal ident} l)
     (label (syntax-e #'l))]))

(define (interpret-id id-stx)
  (demo:syntax-parse id-stx
    [({demo:~literal ident} l)
     (variable (syntax-e #'l))]))

(define (interpret-int int-stx)
  (const (syntax-e int-stx)))

(define (interpret-expr-bop bop-stx)
  (demo:syntax-parse bop-stx
    [(lhs op rhs)
     (expr-binary
       (interpret-expr #'lhs)
       (interpret-op #'op)
       (interpret-expr #'rhs))]))

(define (interpret-op op-stx)
  (op (syntax-e op-stx)))
     
(define (interpret-expr-paren paren-stx)
  (demo:syntax-parse paren-stx
    [("(" expr ")")
     (interpret-expr #'expr)]))


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

(define parsed-ast
  (interpret-program parsed-program))

;parsed-ast

(ast-print parsed-ast)

