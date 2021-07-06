#lang rosette

(require "../match-define.rkt")
(require racket/pretty)

(define (test-1)

	(define-symbolic* x y z boolean?)

	(define fml1 (implies x (and y z)))
	(define fml2 (and (implies x y) (implies x z)))
	(pretty-print fml1)
	(pretty-print fml2)
	(define cmb (equal? fml1 fml2))

	(define sol (solve (assert (not cmb))))

	(pretty-print sol))

(define (test-2)
	
	(define-symbolic* x y z integer?)

	(define fml1 (equal? z (+ x y)))

	(define fml2 (forall (list x y z) (implies fml1 (equal? z (+ x y)))))

	(define sol (solve (assert fml2)))

	sol)

(test-2)

