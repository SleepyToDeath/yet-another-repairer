#lang rosette

(require rosette/query/debug rosette/lib/render rosette/lib/synthax)

(require "util.rkt")

(define f (~> _ boolean?))

(define/debug (pos? x)
	(if (< x 0) #t #f))

f 
pos?

(define (succ x)
	(if (pos? x) (+ x 1) (- x 1)))
 
(define ucore (debug [f] (assert (= (succ 1) 2))))

(save-pict (render ucore) "debug.jpeg")



