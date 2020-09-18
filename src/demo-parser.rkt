#lang rosette/safe

(require (prefix-in std: racket/base))
(require (prefix-in demo: syntax/parse))
(require "grammar.rkt")
(require "demo-lexer.rkt")
(require "demo2/syntax.rkt")
(require (prefix-in j: "demo2/syntax-jimple.rkt"))

(define (interpret-program program-stx)
  (demo:syntax-parse program-stx
    [({demo:~literal program} stmt-stxs ...)
     (begin
       (define tree
         (j:stats (j:stats-single (j:stat (j:stat-nop (j:nop 0))))))
       (std:for ([stmt-stx (std:syntax->list #'(stmt-stxs ...))])
            (set! tree (j:stats (j:stats-multi tree (j:stats (j:stats-single (j:stat (interpret-stmt stmt-stx))))))))
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
     (j:stat-ass
       (interpret-id #'id-stx)
       (interpret-expr #'expr-stx))]))

(define (interpret-stmt-jmp stmt-jmp-stx)
  (demo:syntax-parse stmt-jmp-stx
    [("jmp" expr-stx label-stx ";")
     (j:stat-jmp
       (interpret-expr #'expr-stx)
       (interpret-label #'label-stx))]))

(define (interpret-stmt-label stmt-label-stx)
  (demo:syntax-parse stmt-label-stx
    [("label" label-stx ":")
     (j:stat-label
       (interpret-label #'label-stx))]))

(define (interpret-stmt-ret stmt-ret-stx)
  (j:stat-ret (j:nop 0)))

(define (interpret-expr expr-stx)
  (demo:syntax-parse expr-stx
    [({demo:~literal j-expr} ({demo:~literal literal} literal-stx))
     (j:expr (j:expr-const (interpret-literal #'literal-stx)))]
    [({demo:~literal j-expr} ({demo:~literal ident} id-stx))
     (j:expr (j:expr-var (interpret-id #'(ident id-stx))))]
    [({demo:~literal j-expr} lhs-stx op-stx rhs-stx)
     (interpret-expr-bop #'lhs-stx #'op-stx #'rhs-stx)]
))

(define (interpret-label label-stx)
  (demo:syntax-parse label-stx
    [({demo:~literal ident} l)
     (j:label (std:syntax-e #'l))]))

(define (interpret-id id-stx)
  (demo:syntax-parse id-stx
    [({demo:~literal ident} id)
     (j:variable (std:syntax-e #'id))]))

(define (interpret-literal literal-stx)
  (j:const (std:syntax-e literal-stx)))

(define (interpret-expr-bop lhs-stx op-stx rhs-stx)
  (j:expr (j:expr-binary
    (interpret-expr lhs-stx)
    (interpret-op op-stx)
    (interpret-expr rhs-stx))))

(define (interpret-op op-stx)
  (demo:syntax-parse op-stx
    [({demo:~literal binop} "+")
     (j:op +)]
    [({demo:~literal binop} "-")
     (j:op -)]
    [({demo:~literal binop} "==")
     (j:op =)]
    [({demo:~literal binop} "<")
     (j:op <)]))

(define (interpret-expr-paren paren-stx)
  (demo:syntax-parse paren-stx
    [("(" expr ")")
     (interpret-expr #'expr)]))


(define program-text
  (std:string-append
;    "v1 = x; \n"
    "jmp v1 < 1 l1; \n"
    "v2 = v1 + 2; \n"
    "jmp 1 == 1 l2; \n"
    "label l1: \n"
    "v2 = 2; \n"
    "label l2: \n"
    "v3 = v2; \n"
    "return; \n"))
;    "y = v3; \n"))

(define parsed-program
  (parse (tokenize (std:open-input-string program-text))))

(std:syntax->datum parsed-program)

(define parsed-ast
  (interpret-program parsed-program))

;parsed-ast

(j:ast-print parsed-ast)

(provide parsed-ast)

