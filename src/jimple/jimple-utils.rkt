#lang rosette/safe

(provide transform-all)

(require rosette/lib/match)
(require (prefix-in std: racket/base))
(require (prefix-in l: racket/list))
(require (prefix-in p: "jimple-parser.rkt"))
(require (prefix-in ast: "../syntax-jimple.rkt"))


(define (transform-all prog-ast)
  (trans-init-return
    (trans-init-static prog-ast)))


(define (trans-init-static prog-ast)
  (ast:program (ast:class-list
    (map trans-init-static-class
         (ast:class-list-cl (ast:program-rhs prog-ast))))))


(define (trans-init-static-class class-ast)
  (let* ([c (ast:class-def-rhs class-ast)]
         [static-virtual (trans-init-static-funcs
                           (ast:class-default-static-functions c)
                           (ast:class-default-member-functions c))]
         [static-funcs (car static-virtual)]
         [virtual-funcs (cdr static-virtual)])
    (ast:class-def (ast:class-default
      (ast:class-default-name c)
      (ast:class-default-extend c)
      (ast:class-default-implements c)
      (ast:class-default-globals c)
      (ast:class-default-fields c)
      static-funcs
      virtual-funcs))))


(define (trans-init-static-funcs statics virtuals)
  (define static-list (ast:function-list-fl (ast:function-declares-rhs statics)))
  (define virtual-list (ast:function-list-fl (ast:function-declares-rhs virtuals)))
  (define new-s-list (cons (findf init-func? virtual-list) static-list))
  (define new-v-list (filter (compose not init-func?) virtual-list))
  (cons (ast:function-declares (ast:function-list new-s-list))
        (ast:function-declares (ast:function-list new-v-list))))


(define (init-func? func-decl)
  (equal? "<init>"
          (ast:func-name-name (ast:function-content-name (ast:function-declare-rhs func-decl)))))


; assume <init>'s have already been transformed to static methods
(define (trans-init-return prog-ast)
  (ast:program (ast:class-list
    (map trans-init-return-class
         (ast:class-list-cl (ast:program-rhs prog-ast))))))


(define (trans-init-return-class class-ast)
  (define c (ast:class-def-rhs class-ast))
    (match c
      [(ast:class-default name extend implements globals fields statics virtuals)
         (ast:class-def
           (ast:class-default name extend implements globals fields (trans-init-return-funcs statics) virtuals))]))


(define (trans-init-return-funcs func-decls)
  (define func-list (ast:function-list-fl (ast:function-declares-rhs func-decls)))
  (define init-index (l:index-where func-list init-func?))
  (ast:function-declares (ast:function-list
    (l:list-update func-list init-index trans-init-return-func))))


(define (trans-init-return-func func-decl)
  (define func (ast:function-declare-rhs func-decl))
  (ast:function-declare (ast:function-content
    (ast:function-content-name func)
    (ast:function-content-args func)
    (ast:function-content-local-variables func)
    (ast:stats (ast:stat-list
      (map trans-init-return-stmt
           (ast:stat-list-sl (ast:stats-rhs
             (ast:function-content-statements func)))))))))


(define (trans-init-return-stmt stmt)
  (define s (ast:stat-rhs stmt))
  (match s
    [(ast:stat-ret v)
       (if (equal? v (ast:dexpr (ast:expr-const p:void-return-value)))
           (ast:stat (ast:stat-ret (ast:dexpr (ast:expr-var (ast:variable "r0")))))
           stmt)]
    [_ stmt]))

