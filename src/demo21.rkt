#lang rosette/safe

(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-computational.rkt")

(define main-func (function-declare (function-content 
	(func-name "main")
	(argument-callee (argument-callee-list null))
	(field-declares (field-list (list (field "var-1") (field "var-2") (field "var-3"))))
	(stats (stat-list (list (stat (stat-ret (variable "var-3")))))))))

(define class-1 (class-def (class-default
	(field-declares (field-list (list (field "global-1"))))
	(field-declares (field-list (list (field "field-1"))))
	(function-declares (function-list main-func))
	(function-declares (function-list null)))))

(define prog-1 (program
	(class-list (list class-1))))

(ast-check prog-1)


