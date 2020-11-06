#lang rosette/safe

(require (prefix-in std: racket/base))
(require rosette/lib/angelic)
(require racket/pretty)

(require "match-define.rkt")
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")

(define main-func (function-declare (function-content 
	(func-name "main")
	(variable-definitions (variable-definition-list null))
	(variable-definitions (variable-definition-list (list 
		(variable-definition (variable-n-type (variable "var-1") (type-name int-type-name)))
		(variable-definition (variable-n-type (variable "var-0") (type-name int-type-name))))))
	(stats (stat-list (list
		(stat (stat-ass 
			(lexpr (expr-var (variable "var-0")))
			(expr (expr-const (const 10)))))
		(stat (stat-ret (dexpr (expr-var (variable "var-0")))))))))))

(define class-1 (class-def (class-default
	(type-name "HelloWorld")
	(type-name #f)
	(interface-implements (interface-name-list null))
	(field-declares (field-list null))
	(field-declares (field-list (list (field "field-1"))))
	(function-declares (function-list (list main-func)))
	(function-declares (function-list null)))))


(define buggy (program
	(class-list (list class-1))))


;(ast-check prog-1)

;(define mac (ast->machine prog-1))

;(define mac-in (assign-input mac (list (cons "var-1" 1) (cons "var-2" 2))))

;(define mac-comp (compute mac-in))

;(compare-output mac-comp (list (cons var-ret-name 3)))

(println string-id-map)

(define input1 (list (cons "var-2" 1)))
(define output1 (list (cons var-ret-name 10)))

(match-define (cons soft hard) (ast->relation buggy))

(define tf (hard input1 output1))

;(display "\n")
;(pretty-print tf)
;(display "\n")
;soft
(display "\n")

tf

(define debug-sol (optimize #:maximize null
          #:guarantee (assert tf)))

debug-sol
