#lang rosette

(require rosette/query/debug rosette/lib/render rosette/lib/synthax)

(define-symbolic l1 l2 l3 l4 boolean?)

(define (get-tf x y)
  (define-symbolic* guard1 boolean?)
  (define-symbolic* index1 index2 index3 integer?)
  (define-symbolic* i integer?)

  (define tf (and (implies l1 (equal? guard1 (< index1 1)))
                 (implies l2 (= index2 2))
                 (implies l3 (= index3 (+ index1 2)))
                 (implies l4 (= i (if guard1 index2 index3)))))
  (define in (= index1 x))
  (define out (= i y))

  (and in tf out)
)

(define tf1 (get-tf 1 2))
(define tf2 (get-tf 2 3))

(define hard-constraint (and tf1 tf2))

(define (b2i b)
  (if b 1 0))

(optimize #:maximize (list (+ (b2i l1) (b2i l2) (b2i l3) (b2i l4)))
          #:guarantee (assert hard-constraint))

