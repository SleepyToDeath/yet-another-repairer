#lang rosette/safe
(require (prefix-in std: racket/base))
(require racket/format)

(require "map.rkt")
(require "formula.rkt")
(require "string-id.rkt")
(require "memory-common.rkt")
(require "semantics-common.rkt")
(require "syntax-jimple.rkt")
(require "jimple/operators.rkt")

(provide jtype->mtype op-return-type op-type-check? jtype-of is-a?)

(define supported-types (list "void" "mboolean" "boolean" "byte" "short" "char" "int" "long"))

(define unsupported-types (list "float" "double"))

(define memory-map
	(list
	(cons "any" any-type)
	(cons "java.lang.String" string-type)
	(cons "void" int-type)
	(cons "null" int-type)
	(cons "boolean" bool-type)
	(cons "mboolean" mbool-type)
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
		(cons equal? "mboolean")
		(cons op-gt "mboolean") 
		(cons op-ge "mboolean") 
		(cons op-lt "mboolean")
		(cons op-le "mboolean")
		(cons op-neq "mboolean") 
		(cons op-cmp "int")))

(define operand-map
	(list
		(list bvand bv-type bv-type)
		(list bvor bv-type bv-type)
		(list bvxor bv-type bv-type)
		(list bvlshr bv-type int-type)))

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
	(if (or (equal? any-type mt1) (equal? any-type mt2)) #t
		(cond
			[(member op generic-op-list)  (equal? mt1 mt2)]
			[(equal? op equal?) #t]
			[else (ormap (lambda (tuple) (equal? tuple (list op mt1 mt2))) operand-map)])))
	

;jimple type to memory type
(define (jtype->mtype jtype)
	(if (not jtype) (std:error "Error type") #f)
	(if (member jtype (map string-id unsupported-types)) (force-error #t (~a "Unsupported-type: " jtype)) #f)
	(define maybe-primitive (primitive-type? jtype))
	(if maybe-primitive maybe-primitive addr-type))


(define jtype-map
	(list 
		(cons any-type "any")
		(cons int-type "int")
		(cons bv-type "long")
		(cons mbool-type "mboolean")))

;return a string of type name in java rather than a predicate
(define (jtype-of v)
	(if (equal? (nullptr int-type) v) (string-id null-type-name)
		(begin
		(define mtype (type-of v))
		(define maybe-jtype (ormap (lambda (types) (if (equal? mtype (car types)) (string-id (cdr types)) #f)) jtype-map))
		(if maybe-jtype maybe-jtype "unknown"))))

(define (primitive-type? jtype)
	(ormap 
		(lambda (j.m) (if (equal? jtype (string-id (car j.m))) (cdr j.m) #f)) 
		memory-map))

(define (is-subclass? returned receiver mac)
;	(display (~a "Checking subclass: " returned " <|? " receiver "\n"))
	(if (equal? returned (string-id null-type-name)) #t
		(if (primitive-type? returned) #f
			(if (equal? returned receiver) #t
				(begin
				(define cls (imap-get (machine-cmap mac) returned default-type))
				(ormap 
					(lambda (base) (if base (is-subclass? base receiver mac) #f))
					(cons (class-extend cls) (class-implements cls))))))))

(define (is-a? returned receiver mac)
	(if (or (equal? returned (string-id "any")) (equal? receiver (string-id "any"))) #t
		(if (equal? returned (string-id "mboolean")) #f
			(if (primitive-type? receiver)
				(equal? (jtype->mtype returned) (jtype->mtype receiver))
				(is-subclass? returned receiver mac)))))

