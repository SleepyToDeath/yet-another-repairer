#lang rosette/safe

(require "memory.rkt")
(require "map.rkt")

(define-symbolic* vs integer?)
(define-symbolic* shadow-key integer?)

(define mem0 (memory-alloc memory-empty))
(define addr0 (memory-top mem0))
(define value0 233)

(define mem0-w (memory-store mem0 shadow-key vs))
(define fml-1 (= vs value0))
(define fml-2 (= shadow-key addr0))
(define fml-3 (= (memory-load mem0 addr0) nullptr))
(define fml-4 (= (memory-load mem0-w addr0) value0))

(define hard-constraint (and fml-1 fml-2 fml-3 fml-4))

fml-1
fml-2
fml-3
fml-4
hard-constraint
mem0-w

(optimize #:maximize (list 1)
          #:guarantee (assert hard-constraint))

