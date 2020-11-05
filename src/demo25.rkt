#lang rosette/safe

(require (prefix-in std: racket/base))
(require rosette/lib/angelic)

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
		(variable-definition (variable-n-type (variable "var-4") (type-name "ClassA")))
		(variable-definition (variable-n-type (variable "var-3") (type-name "int")))
		(variable-definition (variable-n-type (variable "var-5") (type-name "int"))))))
	(stats (stat-list (list
		(stat (stat-new (variable "var-4")))
		(stat (stat-special-call 
			(variable "var-4") 
			(variable "var-4") (type-name "ClassA") (func-name "<init>") 
			(types (type-list null))
			(arguments-caller (argument-caller-list null))))
		(stat (stat-virtual-call 
			(variable "dummy")
			(variable "var-4") (type-name "ClassA") (func-name "set-field-1")
			(types (type-list (list (type-name "int"))))
			(arguments-caller (argument-caller-list (list (dexpr (expr-var (variable "var-1"))))))))
		(stat (stat-virtual-call 
			(variable "var-5")
			(variable "var-4") (type-name "ClassA") (func-name "get-field-1")
			(types (type-list null))
			(arguments-caller (argument-caller-list null))))
		(stat (stat-ass 
			(lexpr (expr-var (variable "var-3"))) 
			(expr (expr-binary 
				(expr (expr-var (variable "var-5")))
				(op +)
				(expr (expr-field (variable "var-4") (type-name "ClassA") (field "field-2")))))))
		(stat (stat-ret (dexpr (expr-var (variable "var-3")))))))))))

(define get-field-1 (function-declare (function-content
	(func-name "get-field-1")
	(variable-definitions (variable-definition-list null))
	(variable-definitions (variable-definition-list (list
		(variable-definition (variable-n-type (variable "t-mp") (type-name "int"))))))
	(stats (stat-list (list
		(stat (stat-ass 
			(lexpr (expr-var (variable "t-mp")))
			(expr (expr-field (variable "this") (type-name "ClassA") (field "field-1")))))
		(stat (stat-ret (dexpr (expr-var (variable "t-mp")))))))))))

(define set-field-1 (function-declare (function-content
	(func-name "set-field-1")
	(variable-definitions (variable-definition-list (list
		(variable-definition (variable-n-type (variable "param0") (type-name "int"))))))
	(variable-definitions (variable-definition-list null))
	(stats (stat-list (list
		(stat (stat-ass 
			(lexpr (expr-field (variable "this") (type-name "ClassA") (field "field-1")))
;			(expr (expr-field (variable "this") (type-name "ClassA") (field "field-2")))))
			(expr (expr-var (variable "param0")))))
		(stat (stat-ret (dexpr (expr-var (variable "dummy")))))))))))

(define init-func (function-declare (function-content
	(func-name "<init>")
	(variable-definitions (variable-definition-list null))
	(variable-definitions (variable-definition-list null))
	(stats (stat-list (list
		(stat (stat-ass
			(lexpr (expr-field (variable "this") (type-name "ClassA") (field "field-2")))
			(expr (expr-const (const 2)))))
		(stat (stat-ret (dexpr (expr-var (variable "this")))))))))))

(define class-1 (class-def (class-default
	(type-name "helloworld")
	(type-name #f)
	(interface-implements (interface-name-list null))
	(field-declares (field-list (list (field "global-1"))))
	(field-declares (field-list (list (field "field-1"))))
	(function-declares (function-list (list main-func)))
	(function-declares (function-list null)))))

(define class-2 (class-def (class-default
	(type-name "ClassA")
	(type-name #f)
	(interface-implements (interface-name-list null))
	(field-declares (field-list null))
	(field-declares (field-list (list (field "field-2") (field "field-1"))))
	(function-declares (function-list (list init-func)))
	(function-declares (function-list (list set-field-1 get-field-1))))))

;(println string-id-map)

(define buggy (program
	(class-list (list class-1 class-2))))


;(ast-check prog-1)

;(define mac (ast->machine prog-1))

;(define mac-in (assign-input mac (list (cons "var-1" 1) (cons "var-2" 2))))

;(define mac-comp (compute mac-in))

;(compare-output mac-comp (list (cons var-ret-name 3)))

(println string-id-map)

(define input1 (list (cons "var-1" 1)))
(define output1 (list (cons var-ret-name 3)))

(define input2 (list (cons "var-1" 5) ))
(define output2 (list (cons var-ret-name 7)))

(match-define (cons soft hard) (ast->relation buggy))

(define tf (hard input1 output1))

tf
soft

(define debug-sol (optimize #:maximize soft
          #:guarantee (assert tf)))

debug-sol
