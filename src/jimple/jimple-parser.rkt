#lang rosette/safe

(provide parse-to-stx)
(provide build-ast-program)
(provide build-ast-file)

(require (prefix-in std: racket/base))
(require (prefix-in l: racket/list))
(require (prefix-in s: racket/string))
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
            [decls (first body-contents)]
            [stmts (second body-contents)]
            [ms (build-ast-modifiers #'(modifiers ...))]
            [method (ast:function-declare
                      (ast:function-content
                        (build-ast-method-name #'method-name)
                        (if (p:attribute param-list)
                          (build-ast-method-params #'param-list)
                          (ast:variable-definitions (ast:variable-definition-list null)))
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
       (list decl-list stmt-list))]))


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
  (p:syntax-parse stmt-stx
    [({p:~literal assign_stmt} _ _)
     (build-ast-stmt-ass stmt-stx)]
    [({p:~literal label_stmt} _)
     (build-ast-stmt-label stmt-stx)]
    [({p:~literal goto_stmt} _)
     (build-ast-stmt-goto stmt-stx)]
    [({p:~literal if_stmt} _ _)
     (build-ast-stmt-if stmt-stx)]
    [({p:~literal nop_stmt})
     (build-ast-stmt-nop stmt-stx)]
  ))


(define (build-ast-stmt-ass stmt-ass-stx)
  (p:syntax-parse stmt-ass-stx
    [({p:~literal assign_stmt}
        ({p:~literal variable} lhs-var)
        ({p:~literal j_expression} rhs-expr))
     (ast:stat-ass
       (ast:lexpr (build-ast-variable #'lhs-var))
       (ast:expr (build-ast-expression #'rhs-expr)))]))


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
       (build-ast-bool-expr #'expr)
       (ast:label (std:syntax-e #'name)))]))


(define (build-ast-stmt-nop stmt-nop-stx)
  (p:syntax-parse stmt-nop-stx
    [({p:~literal nop_stmt})
     (ast:stat-nop (ast:nop #f))]))


(define (build-ast-stmt-label stmt-label-stx)
  (p:syntax-parse stmt-label-stx
    [({p:~literal label_stmt}
        ({p:~literal label_name} name))
     (ast:stat-label (ast:label (std:syntax-e #'name)))]))


(define (build-ast-expression expr-stx)
  (p:syntax-parse expr-stx
    [({p:~literal immediate} imm)
     (build-ast-expr-immediate #'imm)]
  ))


(define (build-ast-bool-expr bool-expr-stx)
  (std:error "Not impelemented yet"))


(define (build-ast-expr-immediate expr-imm-stx)
  (p:syntax-parse expr-imm-stx
    [({p:~literal name} name)
     (build-ast-variable expr-imm-stx)]
    [({p:~literal j_constant} const)
     (build-ast-constant expr-imm-stx)]))


(define (build-ast-variable var-stx)
  (p:syntax-parse var-stx
    [({p:~literal reference} p:~rest _)
     (std:error "not implemented yet")]
    [({p:~literal name} p:~rest _)
     (ast:expr-var (build-ast-name var-stx))]))


(define (build-ast-constant const-stx)
  (p:syntax-parse const-stx
    [({p:~literal j_constant} c)
     (ast:expr-const (ast:const (std:syntax-e #'c)))]))


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
