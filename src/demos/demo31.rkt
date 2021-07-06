#lang rosette/safe
(require "../formula.rkt")

(require (prefix-in std: racket/base))
(require (prefix-in std: racket/list))

(define a #f)
(define b (list 5 6 7 8 9))

(define-symbolic* cnd boolean?)

(define d (if cnd a b))

d

(if d d (list 1 2 3))
