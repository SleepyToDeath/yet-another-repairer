#lang rosette/safe

(provide (all-defined-out))

(define nullptr -1)
(define not-found -666)

(define heap-top-addr 2)
(define heap-bottom 10000)

(define stack-top-addr 1)
(define stack-bottom 100)

(define virtual-table-size 1000)
(define scope-size 1000)
