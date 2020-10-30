#lang rosette/safe

(require (prefix-in std: racket/base))
(require rosette/lib/angelic)

(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-computational.rkt")

(display "Preparing\n")

(define SEARCH-DEPTH 5)
(define vars (list (string-id "var-0") (string-id "var-1") ))
(define types null)
(define fields null)
(define funcs null)
(define consts (list 0 1 2))
(define ops (list + -))
(define labels (list 0 1 2))

(define ctxt-enum (syntax-context vars types fields funcs consts ops labels))

(define main-func (function-declare (function-content 
	(func-name "main")
	(variable-definitions (variable-definition-list null))
	(variable-definitions (variable-definition-list (list 
		(variable-definition (variable-n-type (variable "var-1") (type-name int-type-name)))
		(variable-definition (variable-n-type (variable "var-0") (type-name int-type-name))))))
	(stats (stat-list (list
;		(stat (stat-new (variable "var-0")))
;		(stat (stat-special-call 
;			(variable "var-0") 
;			(variable "var-0") (type-name "HelloWorld") (func-name "<init>") 
;			(types (type-list null))
;			(arguments-caller (argument-caller-list null))))
		(stat (stat-ass 
;			(lexpr (expr-field (variable "var-0") (type-name "HelloWorld") (field "field-1")) )
			(lexpr-enum ctxt-enum SEARCH-DEPTH)
;			(lexpr (expr-var (variable "var-0")))
			(expr (expr-const (const 10)))))
		(stat (stat-ret (dexpr (expr-var (variable "var-0")))))))))))

;(define init-func (function-declare (function-content
;	(func-name "<init>")
;	(variable-definitions (variable-definition-list null))
;	(variable-definitions (variable-definition-list null))
;	(stats (stat-list 
;		(stat (stat-ret (dexpr (expr-var (variable "this"))))))))))

(define class-1 (class-def (class-default
	(type-name "HelloWorld")
	(type-name #f)
	(interface-implements (interface-name-list null))
	(field-declares (field-list null))
	(field-declares (field-list (list (field "field-1"))))
	(function-declares (function-list (list main-func)))
	(function-declares (function-list null)))))

(define sketch (program
	(class-list (list class-1))))

;(ast-check prog-1)

;(define mac (ast->machine prog-1))

;(define mac-in (assign-input mac (list (cons "var-1" 1) (cons "var-2" 2))))

;(define mac-comp (compute mac-in))

;(compare-output mac-comp (list (cons var-ret-name 3)))

;(println string-id-map)

(define input1 null)
(define output1 (list (cons var-ret-name 10)))

(define (sketch->spec skt input output)
	(define maybe-mac (ast->machine skt))
	(compare-output (compute (assign-input maybe-mac input)) output))

(define start-time (std:current-inexact-milliseconds))

(display "Synthesis Starts\n")

sketch

(display "Final Constraint:\n")
(sketch->spec sketch input1 output1)

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

(println string-id-map)
