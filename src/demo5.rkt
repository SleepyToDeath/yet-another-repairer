#lang rosette

(require rosette/query/debug rosette/lib/render rosette/lib/synthax)

(require "util.rkt")

(define-symbolic f (~> integer? integer?))

(define fp (~> integer? integer?))

(f 1)

(define vec (vector 0 1))

(define/debug (foo x)
  (+ (f x) 2))

(define (bar x)
  (+ 1 (f x)))

(define (same f1 f2 x)
  (assert (= (f1 x) (f2 x))))

(= (foo 100) (bar 100))

(define ucore (debug [integer?] (same foo bar 100)))

(save-pict (render ucore) "debug.jpeg")



