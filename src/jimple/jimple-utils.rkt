#lang rosette/safe

(provide transform-all)

(require rosette/lib/match)
(require (prefix-in std: racket/base))
(require (prefix-in l: racket/list))
(require (prefix-in p: "jimple-parser.rkt"))
(require (prefix-in ast: "../syntax-jimple.rkt"))

;============ Constants ============
(define dtype-field-name "__CLASS__")
(define input-var-name "input")


(define (transform-all prog-ast)
  ((compose
     trans-remove-input
     trans-get-class
     trans-init-return
     trans-init-static) prog-ast))


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
  (define new-s-list (let ([initfunc (findf init-func? virtual-list)])
                       (if initfunc
                           (cons initfunc static-list)
                           static-list)))
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
  (if init-index
      (ast:function-declares (ast:function-list
        (l:list-update func-list init-index trans-init-return-func)))
      func-decls))


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


(define (trans-get-class prog-ast)
  (ast:program (ast:class-list
    (map trans-get-class-class
         (ast:class-list-cl (ast:program-rhs prog-ast))))))


(define (trans-get-class-class class-ast)
  (define c (ast:class-def-rhs class-ast))
    (match c
      [(ast:class-default name extend implements globals fields statics virtuals)
         (ast:class-def
           (ast:class-default name extend implements globals fields (trans-get-class-funcs statics) (trans-get-class-funcs virtuals)))]))


(define (trans-get-class-funcs func-decls)
  (define func-list (ast:function-list-fl (ast:function-declares-rhs func-decls)))
  (ast:function-declares (ast:function-list
    (map trans-get-class-func func-list))))


(define (trans-get-class-func func-decl)
  (define func (ast:function-declare-rhs func-decl))
  (ast:function-declare (ast:function-content
    (ast:function-content-name func)
    (ast:function-content-args func)
    (ast:function-content-local-variables func)
    (ast:stats (ast:stat-list
      (map trans-get-class-stmt
           (ast:stat-list-sl (ast:stats-rhs
             (ast:function-content-statements func)))))))))


(define (trans-get-class-stmt stmt)
  (define s (ast:stat-rhs stmt))
  (match s
    [(ast:stat-virtual-call ret obj class func arg-types args)
       (if (and (equal? (ast:type-name-name class) "java.lang.Object")
                (equal? (ast:func-name-name func) "getClass")
                (equal? (ast:type-list-tl (ast:types-rhs arg-types)) null))
           (ast:stat (ast:stat-ass
             (ast:lexpr (ast:expr-var ret))
             ; imprecise type for field access
             (ast:expr (ast:expr-field obj class (ast:field dtype-field-name)))))
           stmt)]
    [_ stmt]))


(define (trans-remove-input prog-ast)
  (ast:program (ast:class-list
    (map trans-remove-input-class
         (ast:class-list-cl (ast:program-rhs prog-ast))))))


(define (trans-remove-input-class class-ast)
  (define c (ast:class-def-rhs class-ast))
    (match c
      [(ast:class-default name extend implements globals fields statics virtuals)
         (ast:class-def
           (ast:class-default name extend implements globals fields (trans-remove-input-funcs statics) virtuals))]))


(define (trans-remove-input-funcs func-decls)
  (define func-list (ast:function-list-fl (ast:function-declares-rhs func-decls)))
  (define main-index (l:index-where func-list main-func?))
  (if main-index
      (ast:function-declares (ast:function-list
        (l:list-update func-list main-index trans-remove-input-func)))
      func-decls))


(define (main-func? func-decl)
  (and (equal? "main"
               (ast:func-name-name (ast:function-content-name (ast:function-declare-rhs func-decl))))
       (equal? null
               (ast:variable-definition-list-vl (ast:variable-definitions-rhs
                 (ast:function-content-args (ast:function-declare-rhs func-decl)))))))


(define (trans-remove-input-func func-decl)
  (define func (ast:function-declare-rhs func-decl))
  (ast:function-declare (ast:function-content
    (ast:function-content-name func)
    (ast:function-content-args func)
    (ast:variable-definitions (ast:variable-definition-list
      (filter (compose not input-var-defn?)
              (ast:variable-definition-list-vl (ast:variable-definitions-rhs
                (ast:function-content-local-variables func))))))
    (ast:stats (ast:stat-list
      (filter (compose not input-var-assn-stmt?)
              (ast:stat-list-sl (ast:stats-rhs
                (ast:function-content-statements func)))))))))


(define (input-var-defn? var-defn)
  (equal? input-var-name
          (ast:variable-name
            (ast:variable-n-type-name
              (ast:variable-definition-rhs var-defn)))))


(define (input-var-assn-stmt? stmt)
  (define s (ast:stat-rhs stmt))
  (match s
    [(ast:stat-ass lvalue rvalue)
       (equal? lvalue (ast:lexpr (ast:expr-var (ast:variable input-var-name))))]
    [_ #f]))

