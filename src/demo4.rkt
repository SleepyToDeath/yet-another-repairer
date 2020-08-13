#lang rosette

(require rosette/query/debug rosette/lib/render)

(define f (~> integer? integer?))

(define/debug ret 
              (equal? (f 1) (f 1)))

ret

(define ucore (debug [f integer? boolean?] (assert (= ret #t))))

(render ucore)

