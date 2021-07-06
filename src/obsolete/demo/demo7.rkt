#lang rosette/safe

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

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

  (and in tf out))

(define tf1 (get-tf 1 2))
(define tf2 (get-tf 2 3))

(define hard-constraint (and tf1 tf2 l4))

(define (b2i b)
  (if b 1 0))

(optimize #:maximize (list (+ (b2i l1) (b2i l2) (b2i l3) (b2i l4)))
          #:guarantee (assert hard-constraint))

(struct plus (left right) #:transparent)
(struct minus (left right) #:transparent)
(struct literal (val) #:transparent)
(struct variable (id) #:transparent)

;index := index + 1
;index := ??
;E ::= plus E E | minus E E | literal val | var id
;val \in Integer
;id \in VarName

(define (??expr vals ids)
  (define val (apply choose* vals))
  (define id (apply choose* ids))
  (define t1 (choose* (literal val)
                      (variable id)))
  (define t2 (choose* (literal val)
                      (variable id)))
  (choose* (plus t1 t2)
           (minus t1 t2)
           t1))

(define (interpret p ctx)
  (match p
    [(plus a b)  (+ (interpret a ctx) (interpret b ctx))]
    [(minus a b) (- (interpret a ctx) (interpret b ctx))]
    [(literal v) v] 
    [(variable a) (vector-ref ctx a)]
    [_ p]))

(define vals (list 0 1 2 3))
;(define ids (list index1 index2 index3 i))
(define ids (list 0 1 2 3))

(define ctx1 (vector 1 -1 -1 -1))
(define ctx2 (vector 2 -1 -1 -1))

;assume the location is l3 and l3 should be index3 := ??
(define sketch (??expr vals ids))

(define-symbolic x integer?)

(define M
  (synthesize
    #:forall (list x)
    #:guarantee (assert (and (= (interpret sketch ctx1) 2)
                             (= (interpret sketch ctx2) 3)))))

(evaluate sketch M)

