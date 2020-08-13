#lang rosette

(require rosette/query/debug rosette/lib/render rosette/lib/synthax)

(require "util.rkt")

(define vec (vector 0 1))

(define num (vector-ref vec 1))

(define/debug (sum x)
	(+ x (vector-ref vec 1)))
 
(define ucore (debug [integer?] (assert (= (sum 1) 0))))

(save-pict (render ucore) "debug.jpeg")



