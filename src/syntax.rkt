#lang rosette/safe

;(require typed-racket-datatype)

(define-generics ast
	[ast-check ast])

(define-generics expanded
	[expanded-check expanded])

(define-syntax-rule (lhs name (rhs rname) ...)
	#f
)

(define-syntax-rule (term name vname ...)
	#f
)

(define-syntax-rule (rhs name (lhs lname) ...)
	#f
)


(provide all-defined-out)
