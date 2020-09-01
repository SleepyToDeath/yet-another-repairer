#lang rosette/safe

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(struct my-expr (l r) #:transparent)
(struct my-num (n) #:transparent)
(struct invalid (dummy))

(define (??expr nums depth)
	(if (> depth 0)
		(choose* (my-expr (??expr nums (- depth 1)) (??expr nums (- depth 1)))
				 (my-num (apply choose* nums)))
		(invalid 0)))

(define nums (list 0 1 2 3))

(define sketch (??expr nums 10))

(define (depth-of expr)
	(match expr
		[(my-expr l r) (+ 1 (max (depth-of l) (depth-of r)))]
		[(my-num v) v]
		[_ 100]))

(define (verify expr)
	(if (= (depth-of expr) 5) #t #f))

(define M
  (synthesize
    #:forall null
    #:guarantee (assert (verify sketch))))

(evaluate sketch M)


