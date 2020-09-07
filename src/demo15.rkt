#lang rosette/safe

(require "memory.rkt")

(define mem0 (memory-alloc empty-memory))
(define addr0 (memory-top mem0))
(define value0 233)
(define mem0-w (memory-store mem0 addr0 value0))
(memory-load mem0 addr0)
(memory-load mem0-w addr0)

