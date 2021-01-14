#lang rosette/safe

(provide transform-all)
(provide trans-init-static)
(provide trans-init-return)

(require (prefix-in std: racket/base))
(require (prefix-in ast: "../syntax-jimple.rkt"))


(define (transform-all prog-ast)
  (trans-init-static (trans-init-return prog-ast)))


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
  (cons (ast:function-list (ast:function-declares new-s-list))
        (ast:function-list (ast:function-declares new-v-list))))


(define (init-func? func-decl)
  (equal? "<init>"
          (ast:func-name-name (ast:function-content-name (ast:function-declare-rhs func-decl)))))


(define (trans-init-return prog-ast)
  prog-ast)

