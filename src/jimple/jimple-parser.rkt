#lang rosette/safe

(provide parse-to-stx)
(provide build-ast-program)
(provide build-ast-file)

(require (prefix-in std: racket/base))
(require (prefix-in p: syntax/parse))
(require "jimple-grammar.rkt")
(require "jimple-lexer.rkt")
(require "../syntax.rkt")
(require (prefix-in ast: "../syntax-jimple.rkt"))


;============ Constants ============
(define param-prefix "@parameter")


;============ Helper functions ============
(define (has-static-modifier? modifier-list)
  (ormap [lambda (m) (std:string=? m "static")] modifier-list))

(define (parse-to-stx text)
  (parse (tokenize (std:open-input-string text))))


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
               (ast:interface-implements null))
           (ast:field-declares global-list)
           (ast:field-declares field-list)
           (ast:function-declares static-func-list)
           (ast:function-declares virtual-func-list))))]))


(define (build-ast-type-name class-name-stx)
  (p:syntax-parse class-name-stx
    [({p:~literal class_name} name)
     (ast:type-name (std:syntax-e #'name))]))


(define (build-ast-implements implements-stx)
  (p:syntax-parse implements-stx
    [({p:~literal class_name_list} names ...)
     (ast:interface-implements
       (map build-ast-type-name (std:syntax->list #'(names ...))))]))


(define (build-ast-member member-stx)
  (p:syntax-parse member-stx
    [({p:~literal field} p:~rest f)
     (let ([mod-name (build-ast-field member-stx)])
       (if (has-static-modifier? (first mod-name))
           (list (list (second mod-name)) null null null)
           (list null (list (second mod-name)) null null)))]
    [({p:~literal method} p:~rest m)
     (let ([mod-name (build-ast-method member-stx)])
       (if (has-static-modifier? (first mod-name))
           (list null null (list (second mod-name)) null)
           (list null null null (list (second mod-name)))))]))


(define (build-ast-field field-stx)
  (p:syntax-parse field-stx
    [({p:~literal field}
        modifiers ...
        ({p:~literal j_type} type)
        ({p:~literal name} name))
     (let ([ms (build-ast-modifiers #'(modifiers ...))]
           [f  (ast:field (std:syntax-e #'name))])
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
        ({p:~literal j_type} ret-type)
        method-name
        (p:~optional param-list)
        (p:~optional ({p:~literal throws_clause} throws))
        method-body)
     (let* ([body-contents (build-ast-method-body #'method-body)]
            [local-vars (first body-contents)]
            [stmts (second body-contents)]
            [ms (build-ast-modifiers #'(modifiers ...))]
            [method (ast:function-declare
                      (ast:function-content
                        (build-ast-method-name #'method-name)
                        (if (p:attribute param-list)
                          (build-ast-method-params #'param-list)
                          (ast:arguments-callee (ast:argument-callee-list null)))
                        (ast:variable-declares (ast:variable-list local-vars))
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
       (std:for ([param-stx (std:reverse (std:syntax->list #'(params ...)))])
                (begin
                  (set! param-list (cons (build-ast-method-param param-stx index) param-list))
                  (set! index (+ index 1))))
       (ast:arguments-callee (ast:argument-callee-list param-list)))]))


(define (build-ast-method-param param-stx param-index)
  (p:syntax-parse param-stx
    [({p:~literal parameter}
        ({p:~literal nonvoid_type} nonvoid-type))
     (ast:variable-definition
        (ast:variable-n-type
          (ast:variable (std:string-append param-prefix (std:number->string param-index)))
          (ast:type-name (build-ast-nonvoid-type #'nonvoid-type))))]))


(define (build-ast-type type-stx)
  (p:syntax-parse type-stx
    ["void"
     "void"]
    [({p:~literal nonvoid_type} nonvoid-type)
     (build-ast-nonvoid-type #'nonvoid-type)]))


(define (build-ast-nonvoid-type nonvoid-type-stx)
  (p:syntax-parse nonvoid-type-stx
    [({p:~literal base_type_no_name} base-type-no-name array-brackets ...)
     ;TODO: fix this
     (build-ast-base-type-no-name #'base-type-no-name)]))


(define (build-ast-base-type-no-name base-type-no-name-stx)
  (std:syntax-e #'base-type-no-name))


(define (build-ast-method-body method-body-stx)
  (list null null))

