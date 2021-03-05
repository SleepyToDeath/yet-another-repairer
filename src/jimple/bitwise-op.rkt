#lang rosette/safe

(provide (all-defined-out))
(require (prefix-in std: racket/base))

(define bitwise-op-width 48)

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
