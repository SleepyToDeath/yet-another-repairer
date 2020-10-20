#lang rosette/safe

(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-computational.rkt")

(define main-func (function-declare (function-content 
	(func-name "main")
	(variable-definitions (variable-definition-list null))
;		(variable-definition (variable-n-type (variable "param0") (type-name "int"))))))
	(variable-definitions (variable-definition-list (list 
;		(variable-definition (variable-n-type (variable "var-1") (type-name "int"))) 
;		(variable-definition (variable-n-type (variable "var-2") (type-name "int"))) 
		(variable-definition (variable-n-type (variable "var-3") (type-name "int"))))))
	(stats (stat-list (list
		(stat (stat-ass 
			(lexpr (expr-var (variable "var-3"))) 
			(expr (expr-binary 
				(expr (expr-var (variable "var-1")))
				(op +)
				(expr (expr-var (variable "var-2")))))))
		(stat (stat-ret (variable "var-3")))))))))

(define class-1 (class-def (class-default
	(type-name "helloworld")
	(type-name #f)
	(interface-implements (interface-name-list null))
	(field-declares (field-list (list (field "global-1"))))
	(field-declares (field-list (list (field "field-1"))))
	(function-declares (function-list (list main-func)))
	(function-declares (function-list null)))))

(define prog-1 (program
	(class-list (list class-1))))

(ast-check prog-1)

(define mac (ast->machine prog-1))

(define mac-in (assign-input mac (list (cons "var-1" 1) (cons "var-2" 2))))

(define mac-comp (compute mac-in))

(compare-output mac-comp (list (cons var-ret-name 3)))

(println string-id-map)
