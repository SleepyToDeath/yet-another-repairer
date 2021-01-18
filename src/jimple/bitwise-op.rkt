#lang rosette/safe

(provide (all-defined-out))
(require (prefix-in std: racket/base))

(define (bitwise-op-gen bit-op)
	(define (bitwise-op a b width)
		(std:error "Bitwise op should not be used!")
		(if (equal? width 0) 0
			(let 
				([b1 (modulo a 2)]
				 [b2 (modulo b 2)]
				 [base (bitwise-op (quotient a 2) (quotient b 2) (- width 1))])
				(if (bit-op b1 b2) (* 2 base) 
					(+ (* 2 base) 1)))))
	bitwise-op)


(define (b-andl a b)
	(b-and a b 32))

(define b-and (bitwise-op-gen (lambda (b1 b2) (and (equal? b1 1) (equal? b2 1)))))

(define (b-orl a b)
	(b-or a b 32))

(define b-or (bitwise-op-gen (lambda (b1 b2) (or (equal? b1 1) (equal? b2 1)))))

(define (b-xorl a b)
	(b-xor a b 32))

(define b-xor (bitwise-op-gen (lambda (b1 b2) (equal? b1 b2))))

(define (b-rshift a w)
	(std:error "Bitwise op should not be used!")
	(if (equal? w 0) a
		(if (> w 0) (b-rshift (quotient a 2) (- w 1))
			(b-rshift (* a 2) (+ w 1)))))

(define (b-lshift a w)
	(b-rshift a (- 0 w)))
