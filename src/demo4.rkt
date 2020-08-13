#lang rosette

(require rosette/query/debug rosette/lib/render)

(define f (~> integer? integer?))

(define/debug (foo x)
  (+ (f x) 1))

(define (bar x)
  (+ 1 (f x)))

(define (same f1 f2 x)
  (assert (= (f1 x) (f2 x))))

(define ucore (debug [integer?] (same foo bar 100)))

(render ucore)

