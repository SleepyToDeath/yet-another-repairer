#lang rosette/safe

(provide parse-to-stx)
(provide build-ast-program)
(provide build-ast-file)
(provide void-return-value)
(provide void-return-var)
(provide void-receiver)

(require (prefix-in std: racket/format))
(require (prefix-in std: racket/base))
(require (prefix-in l: racket/list))
(require (prefix-in s: racket/string))
(require (prefix-in p: syntax/parse))
(require (prefix-in r: rosette/lib/match))
(require "jimple-grammar.rkt")
(require "jimple-lexer.rkt")
(require "operators.rkt")
(require "../syntax.rkt")
(require (prefix-in ast: "../syntax-jimple.rkt"))
;(require (prefix-in m: "../map.rkt"))
(require (prefix-in m: "../memory-common.rkt"))


;============ Constants ============
(define void-return-value (ast:const -233))
(define void-return-var (ast:variable "__void_return"))
(define void-receiver (ast:variable "__no_receiver"))
(define param-prefix "@parameter")
(define lengthof-index -1)


;============ Helper functions ============
(define (has-static-modifier? modifier-list)
  (ormap [lambda (m) (std:string=? m "static")] modifier-list))

(define (has-volatile-modifier? modifier-list)
  (ormap [lambda (m) (std:string=? m "volatile")] modifier-list))

(define (parse-to-stx text)
  (parse (tokenize (std:open-input-string text))))

(define (string-repeat n str)
  (s:string-append* (l:make-list n str)))


;============ AST construction ============
; assuming there is only one class in the program
(define (build-ast-program file-text)
  (ast:program (ast:class-list (list (build-ast-file file-text)))))

(define (build-ast-file file-stx)
  (p:syntax-parse file-stx
    [({p:~literal j_file}
        modifiers ...
        ({p:~literal file_type} file_type)
        class-name
        (p:~optional ({p:~literal extends_clause} ext))
        (p:~optional ({p:~literal implements_clause} impls))
        ({p:~literal file_body} members ...))
     (begin
       (define global-list null)
       (define field-list null)
       (define static-func-list null)
       (define virtual-func-list null)
       (std:for ([member-stx (std:syntax->list #'(members ...))])
                (let ([ret-list (build-ast-member member-stx)])
                  (begin
                    (set! global-list (append global-list (first ret-list)))
                    (set! field-list (append field-list (second ret-list)))
                    (set! static-func-list (append static-func-list (third ret-list)))
                    (set! virtual-func-list (append virtual-func-list (fourth ret-list))))))
       (ast:class-def
         (ast:class-default
           (build-ast-type-name #'class-name)
           (if (p:attribute ext)
               (build-ast-type-name #'ext)
               (ast:type-name #f))
           (if (p:attribute impls)
               (build-ast-implements #'impls)
               (ast:interface-implements (ast:interface-name-list null)))
           (ast:variable-definitions (ast:variable-definition-list global-list))
           (ast:variable-definitions (ast:variable-definition-list field-list))
           (ast:function-declares (ast:function-list static-func-list))
           (ast:function-declares (ast:function-list virtual-func-list)))))]))


(define (build-ast-type-name class-name-stx)
  (p:syntax-parse class-name-stx
    [({p:~literal class_name} name)
     (ast:type-name (std:syntax-e #'name))]))


(define (build-ast-implements implements-stx)
  (p:syntax-parse implements-stx
    [({p:~literal class_name_list} names ...)
     (ast:interface-implements
       (ast:interface-name-list
         (map build-ast-type-name (std:syntax->list #'(names ...)))))]))


(define (build-ast-member member-stx)
  (p:syntax-parse member-stx
    [({p:~literal field} p:~rest f)
     (let ([mod-name (build-ast-field member-stx)])
       (if (has-static-modifier? (first mod-name))
           (list (list (second mod-name)) null null null)
           (list null (list (second mod-name)) null null)))]
    [({p:~literal method} p:~rest m)
     (let ([mod-name (build-ast-method member-stx)])
	   (if (has-volatile-modifier? (first mod-name))
	       (list null null null null)
         (if (has-static-modifier? (first mod-name))
           (list null null (list (second mod-name)) null)
           (list null null null (list (second mod-name))))))]))


(define (build-ast-field field-stx)
  (p:syntax-parse field-stx
    [({p:~literal field}
        modifiers ...
        ({p:~literal j_type} type)
        ({p:~literal name} name))
     (let ([ms (build-ast-modifiers #'(modifiers ...))]
           [f  (ast:variable-definition
                 (ast:variable-n-type
                   (ast:variable (std:syntax-e #'name))
                   (ast:type-name (build-ast-nonvoid-type #'type))))])
       (list ms f))]))


(define (build-ast-modifiers modifiers-stx)
  (map build-ast-modifier (std:syntax->list modifiers-stx)))


(define (build-ast-modifier modifier-stx)
  (p:syntax-parse modifier-stx
    [({p:~literal modifier} modifier)
     (std:syntax-e #'modifier)]))


(define (build-ast-method method-stx)
  (p:syntax-parse method-stx
    [({p:~literal method}
        modifiers ...
        ret-type
        method-name
        "("
        (p:~optional param-list)
        ")"
        (p:~optional ({p:~literal throws_clause} throws))
        method-body)
     (let* ([body-contents (build-ast-method-body #'method-body)]
            [decls (first body-contents)]
            [stmts (second body-contents)]
            [ms (build-ast-modifiers #'(modifiers ...))]
            [method (ast:function-declare
                      (ast:function-content
                        (build-ast-method-name #'method-name)
                        (if (p:attribute param-list)
                          (build-ast-method-params #'param-list)
                          (ast:variable-definitions (ast:variable-definition-list null)))
                        (ast:type-name (build-ast-type #'ret-type))
                        (ast:variable-definitions (ast:variable-definition-list decls))
                        (ast:stats (ast:stat-list stmts))))])
       (list ms method))]))


(define (build-ast-method-name method-name-stx)
  (p:syntax-parse method-name-stx
    [({p:~literal name} name)
     (ast:func-name (std:syntax-e #'name))]))


(define (build-ast-method-params params-stx)
  (p:syntax-parse params-stx
    [({p:~literal parameter_list} params ...)
     (begin
       (define index 0)
       (define param-list null)
       (std:for ([param-stx (std:syntax->list #'(params ...))])
                (begin
                  (set! param-list (cons (build-ast-method-param param-stx index) param-list))
                  (set! index (+ index 1))))
       (ast:variable-definitions (ast:variable-definition-list (std:reverse param-list))))]))


(define (build-ast-method-param param-stx param-index)
  (p:syntax-parse param-stx
    [({p:~literal parameter} nonvoid-type)
     (ast:variable-definition
        (ast:variable-n-type
          (ast:variable (std:string-append param-prefix (std:number->string param-index)))
          (ast:type-name (build-ast-nonvoid-type #'nonvoid-type))))]))


(define (build-ast-nonvoid-type nonvoid-type-stx)
  (p:syntax-parse nonvoid-type-stx
    [({p:~literal nonvoid_type} typename array-brackets ...)
     (std:string-append
       (std:syntax-e #'typename)
       (string-repeat (length (std:syntax->list #'(array-brackets ...))) "[]"))]))


(define (build-ast-type type-stx)
  (p:syntax-parse type-stx
    [({p:~literal j_type} "void")
     "void"]
    [({p:~literal j_type} nonvoid-type)
     (build-ast-nonvoid-type #'nonvoid-type)]))


(define (build-ast-method-body method-body-stx)
  (p:syntax-parse method-body-stx
    [({p:~literal method_body} elements ...)
     (begin
       (define decl-list null)
       (define stmt-list null)
       (std:for ([element-stx (std:syntax->list #'(elements ...))])
                (let ([decl-stmt (build-ast-func-element element-stx)])
                  (begin
                    (set! decl-list (append decl-list (first decl-stmt)))
                    (set! stmt-list (append stmt-list (second decl-stmt))))))
       (list decl-list stmt-list))]))


(define (build-ast-func-element element-stx)
  (p:syntax-parse element-stx
    [({p:~literal declaration} p:~rest _)
     (let ([decl-list (build-ast-declaration element-stx)]
           [stmt-list null])
       (list decl-list stmt-list))]
    [({p:~literal statement} stmt)
     (let ([decl-list null]
           [stmt-list (list (build-ast-statement #'stmt))])
       (list decl-list stmt-list))]
    [({p:~literal catch_clause} p:~rest _)
     (list null null)]))


(define (build-ast-jimple-type jimple-type-stx)
  (p:syntax-parse jimple-type-stx
    [({p:~literal jimple_type} "unknown")
     "unknown"]
    [({p:~literal jimple_type} nonvoid-type)
     (build-ast-nonvoid-type #'nonvoid-type)]))


(define (build-ast-declaration decl-stx)
  (p:syntax-parse decl-stx
    [({p:~literal declaration}
        jimple-type
        ({p:~literal local_name_list} names ...))
     (let ([type (build-ast-jimple-type #'jimple-type)])
       (map (lambda (name-stx)
                    (ast:variable-definition
                      (ast:variable-n-type
                        (build-ast-name name-stx)
                        (ast:type-name type))))
            (std:syntax->list #'(names ...))))]))


(define (build-ast-name name-stx)
  (p:syntax-parse name-stx
    [({p:~literal name} name)
     (ast:variable (std:syntax-e #'name))]))


(define (build-ast-statement stmt-stx)
  (ast:stat (build-ast-statement-dispatch stmt-stx)))

(define (build-ast-statement-dispatch stmt-stx)
  (p:syntax-parse stmt-stx
    [({p:~literal assign_stmt} _ _)
     (build-ast-stmt-ass stmt-stx)]
    [({p:~literal identity_stmt} _ _ _)
     (build-ast-stmt-ident stmt-stx)]
    [({p:~literal identity_no_type_stmt} _ _)
     (build-ast-stmt-ident-nt stmt-stx)]
    [({p:~literal label_stmt} _)
     (build-ast-stmt-label stmt-stx)]
    [({p:~literal goto_stmt} _)
     (build-ast-stmt-goto stmt-stx)]
    [({p:~literal if_stmt} _ _)
     (build-ast-stmt-if stmt-stx)]
    [({p:~literal tableswitch_stmt} p:~rest _)
     (build-ast-stmt-tableswitch stmt-stx)]
    [({p:~literal lookupswitch_stmt} p:~rest _)
     (build-ast-stmt-lookupswitch stmt-stx)]
    [({p:~literal nop_stmt})
     (build-ast-stmt-nop stmt-stx)]
    [({p:~literal throw_stmt} _)
     (build-ast-stmt-throw stmt-stx)]
    [({p:~literal return_stmt} (p:~optional _))
     (build-ast-stmt-return stmt-stx)]
    [({p:~literal ret_stmt} (p:~optional _))
     (std:error "Ret statement should not occur")]
    [({p:~literal invoke_stmt} _)
     (build-ast-stmt-invoke stmt-stx)]
  ))


(define (build-ast-stmt-ass stmt-ass-stx)
  (p:syntax-parse stmt-ass-stx
    ; simple new
    [({p:~literal assign_stmt}
        ({p:~literal variable} lhs-var)
        ({p:~literal j_expression}
           ({p:~literal simple_new} _)))
     (ast:stat-new
       (let ([lhs (build-ast-variable #'lhs-var)])
         (ast:expr-var-name lhs)))]
    ; newarray
    [({p:~literal assign_stmt}
        ({p:~literal variable} lhs-var)
        ({p:~literal j_expression}
           ({p:~literal new_array}
              type
              ({p:~literal immediate} imm))))
     (let ([lhs (build-ast-variable #'lhs-var)]
           [size (build-ast-expr-immediate #'imm)])
         (ast:stat-newarray
           (ast:expr-var-name lhs) 
           (ast:dexpr size)))]
    ; invoke statement with non-void return
    [({p:~literal assign_stmt}
        ({p:~literal variable} lhs-var)
        ({p:~literal j_expression}
           ({p:~literal invoke_expr} expr)))
     (build-ast-stmt-invoke-return #'(invoke_stmt (invoke_expr expr)) #'lhs-var)]
    ; all other cases
    [({p:~literal assign_stmt}
        ({p:~literal variable} lhs-var)
        ({p:~literal j_expression} rhs-expr))
     (ast:stat-ass
       (ast:lexpr (build-ast-variable #'lhs-var))
       (build-ast-expression #'rhs-expr))]))


(define (build-ast-stmt-ident stmt-ident-stx)
  (p:syntax-parse stmt-ident-stx
    [({p:~literal identity_stmt} lhs-var rhs-var rhs-type)
     (ast:stat-ass
       (ast:lexpr (build-ast-variable #'lhs-var))
       (ast:expr (ast:expr-var (build-ast-at-ident #'rhs-var))))]))


(define (build-ast-stmt-ident-nt stmt-ident-nt-stx)
  (p:syntax-parse stmt-ident-nt-stx
    [({p:~literal identity_no_type_stmt} lhs-var rhs-var)
     (ast:stat-ass
       (ast:lexpr (build-ast-variable #'lhs-var))
       (ast:expr (ast:expr-var (build-ast-at-ident #'rhs-var))))]))


(define (build-ast-stmt-goto stmt-goto-stx)
  (p:syntax-parse stmt-goto-stx
    [({p:~literal goto_stmt}
        ({p:~literal label_name} name))
     (ast:stat-jmp
       (ast:expr (ast:expr-const (ast:const #t)))
       (ast:label (std:syntax-e #'name)))]))


(define (build-ast-stmt-if stmt-if-stx)
  (p:syntax-parse stmt-if-stx
    [({p:~literal if_stmt}
        ({p:~literal bool_expr} expr)
        ({p:~literal label_name} name))
     (ast:stat-jmp
       (build-ast-expression #'expr)
       (ast:label (std:syntax-e #'name)))]))


(define (build-ast-stmt-tableswitch stmt-switch-stx)
  (p:syntax-parse stmt-switch-stx
    [({p:~literal tableswitch_stmt}
        ({p:~literal immediate} imm)
        cases ...)
     (ast:stat-switch
       (ast:dexpr (build-ast-expr-immediate #'imm))
       (ast:stat-case-list (ast:case-list
         (map build-ast-case (std:syntax->list #'(cases ...))))))]))


(define (build-ast-stmt-lookupswitch stmt-switch-stx)
  (p:syntax-parse stmt-switch-stx
    [({p:~literal lookupswitch_stmt}
        ({p:~literal immediate} imm)
        cases ...)
     (ast:stat-switch
       (ast:dexpr (build-ast-expr-immediate #'imm))
       (ast:stat-case-list (ast:case-list
         (map build-ast-case (std:syntax->list #'(cases ...))))))]))


(define (build-ast-case case-stx)
  (p:syntax-parse case-stx
    [({p:~literal case_stmt} "default" ({p:~literal label_name} name))
     (ast:stat-case
       (ast:case-default
         (ast:label (std:syntax-e #'name))))]
    [({p:~literal case_stmt} "case" int-const ({p:~literal label_name} name))
     (ast:stat-case
       (ast:case-br
         (ast:expr-const-value (build-ast-const-int #'int-const))
         (ast:label (std:syntax-e #'name))))]))


(define (build-ast-stmt-nop stmt-nop-stx)
  (p:syntax-parse stmt-nop-stx
    [({p:~literal nop_stmt})
     (ast:stat-nop (ast:nop #f))]))


(define (build-ast-stmt-throw stmt-throw-stx)
  (p:syntax-parse stmt-throw-stx
    [({p:~literal throw_stmt} _)
     ; treat throw statement as no-op
     (ast:stat-nop (ast:nop #f))]))


(define (build-ast-stmt-label stmt-label-stx)
  (p:syntax-parse stmt-label-stx
    [({p:~literal label_stmt}
        ({p:~literal label_name} name))
     (ast:stat-label (ast:label (std:syntax-e #'name)))]))


(define (build-ast-stmt-return stmt-return-stx)
  (p:syntax-parse stmt-return-stx
    [({p:~literal return_stmt}
        (p:~optional ({p:~literal immediate} imm)))
     (ast:stat-ret
       (ast:dexpr
         (if (p:attribute imm)
             (build-ast-expr-immediate #'imm)
             (ast:expr-const void-return-value))))]))


(define (build-ast-stmt-invoke stmt-invoke-stx)
  (p:syntax-parse stmt-invoke-stx
    [({p:~literal invoke_stmt}
        ({p:~literal invoke_expr} invoke-expr))
     (build-ast-expr-invoke #'invoke-expr)]))


(define (build-ast-stmt-invoke-return stmt-invoke-stx return-var-stx)
  (define stmt (build-ast-stmt-invoke stmt-invoke-stx))
  (define lhs (ast:expr-var-name (build-ast-variable return-var-stx)))
  (r:match stmt
    [(ast:stat-static-call ret cls-name func arg-types args)
     (ast:stat-static-call lhs cls-name func arg-types args)]
    [(ast:stat-virtual-call ret obj cls-name func arg-types args)
     (ast:stat-virtual-call lhs obj cls-name func arg-types args)]
    ; specialinvoke does not have a return value
    ;[(ast:stat-special-call ret obj cls-name func arg-types args)
    ; (ast:stat-special-call lhs obj cls-name func arg-types args)]
    [else (std:error "Unknown invoke type")]))


(define (build-ast-expression expr-stx)
  (ast:expr (build-ast-expression-dispatch expr-stx)))

(define (build-ast-expression-dispatch expr-stx)
  (p:syntax-parse expr-stx
    [({p:~literal simple_new} _)
     (build-ast-expr-new-simple expr-stx)]
    [({p:~literal new_array} _ _)
     (build-ast-expr-new-array expr-stx)]
    [({p:~literal new_multiarray} p:~rest _)
     (std:error "New multiarray is not supported yet")]
    [({p:~literal cast_expr} _ _)
     (build-ast-expr-cast expr-stx)]
    [({p:~literal instanceof_expr} p:~rest _)
     (std:error "Instanceof expression is not supported yet")]
    [({p:~literal array_ref} _ _)
     (build-ast-expr-array-ref expr-stx)]
    [({p:~literal field_ref} p:~rest _)
     (build-ast-expr-field-ref expr-stx)]
    [({p:~literal binop_expr} _ _ _)
     (build-ast-expr-binop expr-stx)]
    [({p:~literal unop_expr} _ _)
     (build-ast-expr-unop expr-stx)]
    [({p:~literal immediate} imm)
     (build-ast-expr-immediate #'imm)]))


(define (build-ast-expr-new-simple expr-new-stx)
  (std:error "Unreachable: simple new is handled in assignment"))


(define (build-ast-expr-new-array expr-new-stx)
  (std:error "Unreachable: newarray is handled in assignment"))


(define (build-ast-expr-invoke expr-invoke-stx)
  (p:syntax-parse expr-invoke-stx
    [({p:~literal nonstatic_invoke_expr} p:~rest _)
     (build-ast-expr-nonstatic-invoke expr-invoke-stx)]
    [({p:~literal static_invoke_expr} p:~rest _)
     (build-ast-expr-static-invoke expr-invoke-stx)]
    [({p:~literal dynamic_invoke_expr} p:~rest _)
     (std:error "Dynamic invoke is not supported yet")]))


(define (build-ast-expr-nonstatic-invoke expr-invoke-stx)
  (p:syntax-parse expr-invoke-stx
    [({p:~literal nonstatic_invoke_expr} invoke-type receiver-name method-sig (p:~optional arg-list))
     (let* ([cmd-str (build-ast-invoke-type #'invoke-type)]
            [receiver (build-ast-name #'receiver-name)]
            [ret-list (build-ast-method-sig #'method-sig)]
            [cls-name (first ret-list)]
            [m-name (second ret-list)]
            [param-types (third ret-list)]
            [args (if (p:attribute arg-list)
                      (build-ast-arg-list #'arg-list)
                      (ast:arguments-caller (ast:argument-caller-list null)))])
       (case cmd-str
         [("virtualinvoke") (ast:stat-virtual-call void-return-var receiver cls-name m-name param-types args)]
         [("specialinvoke") (ast:stat-special-call receiver receiver cls-name m-name param-types args)]
         [("interfaceinvoke") (ast:stat-virtual-call void-return-var receiver cls-name m-name param-types args)]
         [else (std:error (std:string-append "Unknown invoke type: " cmd-str))]))]))


(define (build-ast-expr-static-invoke expr-invoke-stx)
  (p:syntax-parse expr-invoke-stx
    [({p:~literal static_invoke_expr} method-sig (p:~optional arg-list))
     (let* ([ret-list (build-ast-method-sig #'method-sig)]
            [cls-name (first ret-list)]
            [m-name (second ret-list)]
            [param-types (third ret-list)]
            [args (if (p:attribute arg-list)
                      (build-ast-arg-list #'arg-list)
                      (ast:arguments-caller (ast:argument-caller-list null)))])
       (ast:stat-static-call void-return-var cls-name m-name param-types args))]))


(define (build-ast-invoke-type invoke-type-stx)
  (p:syntax-parse invoke-type-stx
    [({p:~literal nonstatic_invoke} invoke-type)
     (std:syntax-e #'invoke-type)]))


(define (build-ast-method-sig method-sig-stx)
  (p:syntax-parse method-sig-stx
    [({p:~literal method_signature} class-name ret-type method-name (p:~optional param-list))
     (let ([cls-name (build-ast-type-name #'class-name)]
           [m-name (build-ast-method-name #'method-name)]
           [param-types (if (p:attribute param-list)
                            (build-ast-param-types #'param-list)
                            (ast:types (ast:type-list null)))])
       (list cls-name m-name param-types))]))


(define (build-ast-param-types param-types-stx)
  (p:syntax-parse param-types-stx
    [({p:~literal parameter_list} params ...)
     (ast:types
       (ast:type-list
         (map build-ast-param-type (std:syntax->list #'(params ...)))))]))


(define (build-ast-param-type param-type-stx)
  (p:syntax-parse param-type-stx
    [({p:~literal parameter} nonvoid-type)
     (ast:type-name (build-ast-nonvoid-type #'nonvoid-type))]))


(define (build-ast-arg-list arg-list-stx)
  (p:syntax-parse arg-list-stx
    [({p:~literal arg_list} imms ...)
     (ast:arguments-caller
       (ast:argument-caller-list
         (map build-ast-arg (std:syntax->list #'(imms ...)))))]))


(define (build-ast-arg arg-stx)
  (p:syntax-parse arg-stx
    [({p:~literal immediate} imm)
     (ast:dexpr (build-ast-expr-immediate #'imm))]))


(define (build-ast-expr-cast expr-cast-stx)
  (p:syntax-parse expr-cast-stx
    [({p:~literal cast_expr}
        type
        ({p:~literal immediate} imm))
     (build-ast-expr-immediate #'imm)]))


(define (build-ast-expr-array-ref expr-array-stx)
  (p:syntax-parse expr-array-stx
    [({p:~literal array_ref}
        name
        ({p:~literal immediate} imm))
     (let ([base (build-ast-name #'name)]
           [index (build-ast-expr-immediate #'imm)])
       (ast:expr-array base (ast:expr index)))]))


(define (build-ast-expr-field-ref expr-field-stx)
  (p:syntax-parse expr-field-stx
    [({p:~literal field_ref} (p:~optional name) signature)
     (let* ([ctype-fname (build-ast-field-sig #'signature)]
            [ctype (first ctype-fname)]
            [fname (second ctype-fname)]
            [receiver (if (p:attribute name)
                          (build-ast-name #'name)
                          void-receiver)])
       (ast:expr-field receiver ctype fname))]))


(define (build-ast-field-sig field-sig-stx)
  (p:syntax-parse field-sig-stx
    [({p:~literal field_signature} class-name f-type f-name)
     (let ([ctype (build-ast-type-name #'class-name)]
           [fname (build-ast-field-name #'f-name)])
       (list ctype fname))]))


(define (build-ast-field-name field-name-stx)
  (p:syntax-parse field-name-stx
    [({p:~literal name} f-name)
     (ast:field (std:syntax-e #'f-name))]))


(define (build-ast-expr-binop expr-binop-stx)
  (p:syntax-parse expr-binop-stx
    [({p:~literal binop_expr}
        ({p:~literal immediate} lhs)
        bop
        ({p:~literal immediate} rhs))
     (let ([l (build-ast-expr-immediate #'lhs)]
           [o (build-ast-expr-bop #'bop)]
           [r (build-ast-expr-immediate #'rhs)])
       (ast:expr-binary (ast:expr l) o (ast:expr r)))]))


(define (build-ast-expr-bop expr-bop-stx)
  (p:syntax-parse expr-bop-stx
    [({p:~literal binop} "&") (ast:op bvand)]
    [({p:~literal binop} "|") (ast:op bvor)]
    [({p:~literal binop} "^") (ast:op bvxor)]
    [({p:~literal binop} "%") (ast:op op-mod)]
    [({p:~literal binop} "cmp") (ast:op op-cmp)]
    ;[({p:~literal binop} "cmpg") ???]
    ;[({p:~literal binop} "cmpl") ???]
    [({p:~literal binop} "==") (ast:op equal?)]
    [({p:~literal binop} "!=") (ast:op op-neq)]
    [({p:~literal binop} ">") (ast:op op-gt)]
    [({p:~literal binop} ">=") (ast:op op-ge)]
    [({p:~literal binop} "<") (ast:op op-lt)]
    [({p:~literal binop} "<=") (ast:op op-le)]
    ;[({p:~literal binop} "<<") ???]
    ;[({p:~literal binop} ">>") ???]
    [({p:~literal binop} ">>>") (ast:op bvlshr)]
    [({p:~literal binop} "+") (ast:op op-add)]
    [({p:~literal binop} "-") (ast:op op-sub)]
    [({p:~literal binop} "*") (ast:op op-mul)]
    [({p:~literal binop} "/") (ast:op op-div)]))


(define (build-ast-expr-unop expr-unop-stx)
  (p:syntax-parse expr-unop-stx
    [({p:~literal unop_expr} ({p:~literal unop} "lengthof") ({p:~literal immediate} imm))
     ; `lengthof x` is parsed as array access `x[-1]`
     (ast:expr-array
       (ast:expr-var-name (build-ast-expr-immediate #'imm))
       (ast:expr (ast:expr-const (ast:const lengthof-index))))]
    [({p:~literal unop_expr} ({p:~literal unop} "neg") ({p:~literal immediate} imm))
     (std:error "Neg is not supported yet")]))


(define (build-ast-expr-immediate expr-imm-stx)
  (p:syntax-parse expr-imm-stx
    [({p:~literal name} _)
     (build-ast-variable expr-imm-stx)]
    [({p:~literal int_const} p:~rest _)
     (build-ast-const-int expr-imm-stx)]
    [({p:~literal float_const} p:~rest _)
     (build-ast-const-float expr-imm-stx)]
    [({p:~literal class_const} _)
     (build-ast-const-class expr-imm-stx)]
    ["null" (build-ast-const-null expr-imm-stx)]
    [_ (build-ast-const-str expr-imm-stx)]))


(define (build-ast-variable var-stx)
  (p:syntax-parse var-stx
    [({p:~literal array_ref} _ _)
     (build-ast-expr-array-ref var-stx)]
    [({p:~literal field_ref} p:~rest _)
     (build-ast-expr-field-ref var-stx)]
    [({p:~literal name} _)
     (ast:expr-var (build-ast-name var-stx))]))


(define (build-ast-at-ident at-ident-stx)
  (ast:variable (std:syntax-e at-ident-stx)))


(define (build-ast-const-int const-int-stx)
  (p:syntax-parse const-int-stx
    [({p:~literal int_const} (p:~optional minus-sign) lit)
     (ast:expr-const (ast:const (if (p:attribute minus-sign)
                                    (- (std:syntax-e #'lit))
                                    (std:syntax-e #'lit))))]))


(define (build-ast-const-float const-float-stx)
  (p:syntax-parse const-float-stx
    [({p:~literal float_const} (p:~optional minus-sign) lit)
     (ast:expr-const (ast:const (if (p:attribute minus-sign)
                                    (- (std:syntax-e #'lit))
                                    (std:syntax-e #'lit))))]))


(define (build-ast-const-class const-class-stx)
  (p:syntax-parse const-class-stx
    [({p:~literal class_const} cls)
     (ast:expr-const (ast:const (convert-type-abbrv (std:syntax-e #'cls))))]))


(define (convert-type-abbrv abbrv-str)
  (if (and (s:string-prefix? abbrv-str "L") (s:string-suffix? abbrv-str ";"))
      (s:string-replace
        (std:substring abbrv-str 1 (- (std:string-length abbrv-str) 1))
        "/"
        ".")
      (std:error (std:string-append "Unknown type abbreviation " abbrv-str))))


(define (build-ast-const-null null-str-stx)
  (ast:expr-const (ast:const (m:nullptr m:addr-type))))


(define (build-ast-const-str const-str-stx)
  (ast:expr-const (ast:const (std:syntax-e const-str-stx))))


;(define program-text
;  (std:string-append
;      "public class A {\n"
;      "  public void foo() {\n"
;      "    java.lang.String a;\n"
;      "    a = \"bbb\";\n"
;      "  }\n"
;      "}\n"))
;
;(define (tokens)
;  (define next-token
;    (tokenize (std:open-input-string program-text)))
;  (std:for ([i (std:build-list 80 std:values)])
;           (print (next-token))(newline)))
;
;(display "TOKENS:")(newline)
;(tokens)
;
;(define parsed-program
;  (parse-to-stx program-text))
;
;(display "DATUM:")(newline)
;(std:syntax->datum parsed-program)
;
;(define tree
;  (build-ast-file parsed-program))
;
;(display "AST:")(newline)
;tree
