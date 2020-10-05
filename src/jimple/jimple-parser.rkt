#lang rosette/safe

(provide parse-to-stx)
(provide build-ast-program)

(provide build-ast-field)
(provide build-ast-file)
(provide build-ast-modifiers)

(require (prefix-in std: racket/base))
(require (prefix-in p: syntax/parse))
(require "jimple-grammar.rkt")
(require "jimple-lexer.rkt")
(require "../syntax.rkt")
(require (prefix-in ast: "../syntax-jimple.rkt"))


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
  (begin
    (define clazz-name null)
    (define global-list null)
    (define field-list null)
    (define static-func-list null)
    (define virtual-func-list null)
    (p:syntax-parse file-stx
      [({p:~literal j_file}
          modifiers ...
          ({p:~literal file_type} file_type)
          ({p:~literal class_name} name)
          (p:~optional ({p:~literal extends_clause} ext))
          (p:~optional ({p:~literal implements_clause} impl))
          ({p:~literal file_body} members ...))
       (begin
         (set! clazz-name (std:syntax-e #'name))
         (std:for ([member-stx (std:syntax->list #'(members ...))])
                  (let ([ret-list (build-ast-member member-stx)])
                    (begin
                      (set! global-list (append global-list (first ret-list)))
                      (set! field-list (append field-list (second ret-list)))
                      (set! static-func-list (append static-func-list (third ret-list)))
                      (set! virtual-func-list (append virtual-func-list (fourth ret-list)))))))])
    (ast:class-def
      (ast:class-default
        (ast:cls-name clazz-name)
        (ast:field-declares global-list)
        (ast:field-declares field-list)
        (ast:function-declares static-func-list)
        (ast:function-declares virtual-func-list)))))


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
  (std:error "not implemented yet"))

