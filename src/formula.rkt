#lang rosette/safe

(require racket/pretty)
(require rosette/lib/match)   ; provides `match`

(provide (all-defined-out))

(define size-limit 0)

(define (size-of-limited e limit)
	(set! size-limit limit)
	(size-of-limited-real e))

(define (size-of-limited-real e)
	(set! size-limit (- size-limit 1))
	(if (> size-limit 0)
		(match e
			[(expression op child ...) (+ 1 (apply + (map size-of-limited-real child)))]
			[(constant id type) 1]
			[_ 1])
		0))

(define (size-of e)
	(match e
		[(expression op child ...) (+ 1 (apply + (map size-of child)))]
		[(constant id type) 1]
		[_ 1]))

