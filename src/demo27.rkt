#lang rosette

(require "match-define.rkt")

(define (fc i)
	(+ i 1))


(define-symbolic fa (~> integer? integer?))
(define-symbolic fb (~> integer? integer?))

(define fml0 (equal? (fb 1) 2))
(define fml1 (equal? (fb 2) 3))
;(define-symbolic x integer?)

(define (update fa fb k v pmark) 
	(define-symbolic* x integer?)
	(forall (list x) (if (and (equal? x k) pmark) (equal? (fa x) v) (equal? (fa x) (fb x)))))

(define fml (and fml0 fml1 (update fa fb 3 4)))

fml

(define sol (solve (assert fml)))

sol

(define ga (evaluate fa sol))
(define gb (evaluate fb sol))

(define-symbolic y integer?)
(ga y)
(gb y)

(ga 3)

(define (always-different)
	(define-symbolic* x integer?)
	x)

(define (get-vector len) 
	(list->vector (for/list ([_ (range len)]) (always-different))))

(define (update-list la k v)
	(define lb (get-vector 10))
	(cons (equal? la lb) (vector-set! lb k v)))

(define la (get-vector 10))

(vector-set! la 1 2)
(vector-set! la 2 3)

(match-define (cons fmll lb) (update-list la 3 4))

fmll

