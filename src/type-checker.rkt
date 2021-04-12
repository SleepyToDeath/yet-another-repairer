#lang rosette/safe
(require (prefix-in std: racket/base))
(require racket/format)

(require "formula.rkt")
(require "string-id.rkt")
(require "memory-common.rkt")
(require "syntax-jimple.rkt")
(require "jimple/operators.rkt")

(provide jtype->mtype op-return-type op-type-check? jtype-of)

(define supported-types (list "void" "boolean" "byte" "short" "char" "int" "long"))

(define unsupported-types (list "float" "double"))

(define memory-map
	(list
	(cons "void" int-type)
	(cons "boolean" bool-type)
	(cons "byte" int-type)
	(cons "short" int-type)
	(cons "char" int-type)
	(cons "int" int-type)
	(cons "long" bv-type)))

(define generic-op-list 
	(list 
		op-add op-sub op-mul op-div op-mod 
		op-gt op-ge op-lt op-le 
		op-neq op-cmp))

(define return-map
	(list
		(cons bvand "long")
		(cons bvor "long")
		(cons bvxor "long")
		(cons bvlshr "long")
		(cons equal? "boolean")
		(cons op-gt "boolean") 
		(cons op-ge "boolean") 
		(cons op-lt "boolean")
		(cons op-le "boolean")
		(cons op-neq "boolean") 
		(cons op-cmp "int")))

(define operand-map
	(list
		(list bvand bv-type bv-type)
		(list bvor bv-type bv-type)
		(list bvxor bv-type bv-type)
		(list bvlshr bv-type bv-type)))

(define (op-return-type op t1 t2)
	(define mt1 (jtype->mtype t1))
	(define mt2 (jtype->mtype t2))
	(define maybe-type (ormap (lambda (p) (if (equal? op (car p)) (cdr p) #f)) return-map))
	(cond
		[maybe-type (string-id maybe-type)]
		[(member op generic-op-list) t1]
		[else (force-error #t "Return type not known for \n")]))

;if the operands type check for the operator
(define (op-type-check? op t1 t2)
	(define mt1 (jtype->mtype t1))
	(define mt2 (jtype->mtype t2))
	(cond
		[(member op generic-op-list) (equal? mt1 mt2)]
		[(equal? op equal?) #t]
		[else (ormap (lambda (tuple) (equal? tuple (list op mt1 mt2))) operand-map)]))
	

;jimple type to memory type
(define (jtype->mtype jtype)
;	(if (not jtype) (force-error #t "Error type\n") #f)
	(if (not jtype) (std:error "Error type") #f)
	(if (member jtype (map string-id unsupported-types)) (force-error #t (~a "Unsupported-type: " jtype)) #f)
	(define maybe-primitive 
		(ormap 
			(lambda (j.m) (if (equal? jtype (string-id (car j.m))) (cdr j.m) #f)) 
			memory-map))
	(if maybe-primitive maybe-primitive addr-type))


(define jtype-map
	(list 
		(cons int-type "int")
		(cons bv-type "long")
		(cons bool-type "boolean")))

;return a string of type name in java rather than a predicate
(define (jtype-of v)
	(define mtype (type-of v))
	(ormap (lambda (types) (if (equal? mtype (car types)) (cdr types) #f)) jtype-map))
