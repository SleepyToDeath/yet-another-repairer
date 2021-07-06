#lang rosette/safe

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(define-symbolic v1 integer?)

;if v < 1
;	ret = 1 - v1
;else
;	ret = 1 + v1
;
; f(-2) = 3
; f(2) = 3

(define v2 (- 1 v1))
(define v3 (+ 1 v1))
(define g1 (< v1 1))
(define ret (if g1 v2 v3))

(define v2-2 (+ 1 v1))
(define ret2 (if g1 v2-2 v3))

(define g1-3 #t)
(define ret3 (if g1-3 v2 v3))

ret
ret2
ret3

(define final-spec (= ret 3))

(define sol
	(solve (assert final-spec)))

sol

(evaluate v1 sol)

