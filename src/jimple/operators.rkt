#lang rosette/safe

(require (prefix-in mem: "../memory-common.rkt"))
(require (prefix-in std: racket/base))

(provide (all-defined-out))




;op-list a list of operators corresponding to all-types-ordered
(define (typed-op op-list)
	(lambda (x y)
		((list-ref op-list (mem:type->ordinal (type-of x))) x y)))

(define op-add (typed-op (list + bvadd)))
(define op-sub (typed-op (list - bvsub)))
(define op-mul (typed-op (list * bvadd)))
(define op-div (typed-op (list quotient bvadd)))
(define op-mod (typed-op (list modulo bvsmod)))

(define op-gt (typed-op (list > bvsgt)))
(define op-ge (typed-op (list >= bvsge)))
(define op-lt (typed-op (list < bvslt)))
(define op-le (typed-op (list <= bvsle)))

(define op-neq (lambda (x y) (not (equal? x y))))
(define op-cmp (lambda (x y) (if (equal? x y) 0 (if (op-gt x y) 1 -1))))




;===================== Obsolete =======================

#|
(define (bitwise-op-gen bit-op)
	(define (bitwise-op a b width)
		(std:error "This bitwise op should not be used!")
		(if (equal? width 0) 0
			(let 
				([b1 (modulo a 2)]
				 [b2 (modulo b 2)]
				 [base (bitwise-op (quotient a 2) (quotient b 2) (- width 1))])
				(if (bit-op b1 b2) (* 2 base) 
					(+ (* 2 base) 1)))))
	bitwise-op)
(define b-and (bitwise-op-gen (lambda (b1 b2) (and (equal? b1 1) (equal? b2 1)))))
(define b-or (bitwise-op-gen (lambda (b1 b2) (or (equal? b1 1) (equal? b2 1)))))
(define b-xor (bitwise-op-gen (lambda (b1 b2) (equal? b1 b2))))
(define (b-rshift a w)
	(std:error "Bitwise op should not be used!")
	(if (equal? w 0) a
		(if (> w 0) (b-rshift (quotient a 2) (- w 1))
			(b-rshift (* a 2) (+ w 1)))))
|#


#|
(define bitwise-op-width 2)

(define (b-lshift a w)
	(define ba (integer->bitvector a (bitvector bitwise-op-width)))
	(define bw (integer->bitvector w (bitvector bitwise-op-width)))
	(define ret (bvshl ba bw))
	(bitvector->integer ret))

(define (b-rshift a w)
	(define ba (integer->bitvector a (bitvector bitwise-op-width)))
	(define bw (integer->bitvector w (bitvector bitwise-op-width)))
	(define ret (bvlshr ba bw))
	(bitvector->integer ret))

(define (b-andl a b)
	(define ba (integer->bitvector a (bitvector bitwise-op-width)))
	(define bb (integer->bitvector b (bitvector bitwise-op-width)))
	(define ret (bvand ba bb))
	(bitvector->integer ret))

(define (b-orl a b)
	(define ba (integer->bitvector a (bitvector bitwise-op-width)))
	(define bb (integer->bitvector b (bitvector bitwise-op-width)))
	(define ret (bvor ba bb))
	(bitvector->integer ret))

(define (b-xorl a b)
	(define ba (integer->bitvector a (bitvector bitwise-op-width)))
	(define bb (integer->bitvector b (bitvector bitwise-op-width)))
	(define ret (bvor ba bb))
	(bitvector->integer ret))
|#
