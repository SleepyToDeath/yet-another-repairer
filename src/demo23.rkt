#lang rosette/safe

(require (prefix-in std: racket/base))

(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-computational.rkt")

(display "Preparing\n")

(define SEARCH-DEPTH 5)
(define consts (list 0 1 2))
(define ops (list + -))
(define vars (list (string-id "param0") (string-id "this") (string-id "ClassA") (string-id "field-1") (string-id "field-2")))
(define labels (list 0 1 2))

(define ctxt-enum (syntax-context vars consts ops labels))

(define main-func (function-declare (function-content 
	(func-name "main")
	(variable-definitions (variable-definition-list null))
	(variable-definitions (variable-definition-list (list 
		(variable-definition (variable-n-type (variable "var-1") (type-name "int")))
		(variable-definition (variable-n-type (variable "var-4") (type-name "ClassA")))
		(variable-definition (variable-n-type (variable "var-3") (type-name "int"))))))
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
			(arguments-caller (argument-caller-list (list (dexpr (expr-const (const 1))))))))
		(stat (stat-virtual-call 
			(variable "var-1")
			(variable "var-4") (type-name "ClassA") (func-name "get-field-1")
			(types (type-list null))
			(arguments-caller (argument-caller-list null))))
		(stat (stat-ass 
			(lexpr (expr-var (variable "var-3"))) 
			(expr (expr-binary 
				(expr (expr-var (variable "var-1")))
				(op +)
				(expr (expr-field (variable "var-4") (type-name "ClassA") (field "field-2")))))))
		(stat (stat-ret (dexpr (expr-var (variable "var-3")))))))))))

(define get-field-1 (function-declare (function-content
	(func-name "get-field-1")
	(variable-definitions (variable-definition-list null))
	(variable-definitions (variable-definition-list (list
		(variable-definition (variable-n-type (variable "tmp") (type-name "int"))))))
	(stats (stat-list (list
		(stat (stat-ass 
			(lexpr (expr-var (variable "tmp")))
			(expr (expr-field (variable "this") (type-name "ClassA") (field "field-1")))))
		(stat (stat-ret (dexpr (expr-var (variable "tmp")))))))))))

(define set-field-1 (function-declare (function-content
	(func-name "set-field-1")
	(variable-definitions (variable-definition-list (list
		(variable-definition (variable-n-type (variable "param0") (type-name "int"))))))
	(variable-definitions (variable-definition-list null))
	(stats (stat-list (list
		(stat-enum ctxt-enum SEARCH-DEPTH)
;		(stat (stat-ass 
;			(lexpr (expr-field (variable (string-id "this")) (type-name (string-id "ClassA")) (field (string-id "field-1"))))
;			(lexpr (expr-field (variable-enum ctxt-enum SEARCH-DEPTH) (type-name-enum ctxt-enum SEARCH-DEPTH) (field (string-id "field-1"))))
;			(expr (expr-var (variable (string-id "param0"))))))
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


(define sketch (program
	(class-list (list class-1 class-2))))

;(ast-check prog-1)

;(define mac (ast->machine prog-1))

;(define mac-in (assign-input mac (list (cons "var-1" 1) (cons "var-2" 2))))

;(define mac-comp (compute mac-in))

;(compare-output mac-comp (list (cons var-ret-name 3)))

;(println string-id-map)

(define input1 (list (cons "var-1" 1) (cons "var-2" 2)))
(define output1 (list (cons var-ret-name 3)))

(define (sketch->spec skt input output)
	(compare-output (compute (assign-input (ast->machine skt) input)) output))

(define start-time (std:current-inexact-milliseconds))

(display "Synthesis Starts\n")

(define syn-sol 
	(synthesize
		#:forall null
		#:guarantee (assert 
			(and 
				#t
				(ast-check sketch)
				(sketch->spec sketch input1 output1)
			)
		)))

(define result (evaluate sketch syn-sol))

result

(display "Synthesis Finishes\n")

(define finish-time (std:current-inexact-milliseconds))

(display (format "Synthesis took ~a milliseconds. Search depth: ~a\n" (- finish-time start-time) SEARCH-DEPTH))
