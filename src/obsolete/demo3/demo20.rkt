#lang rosette/safe

(require "memory.rkt")
(require "map.rkt")

(define mem0 memory-empty)

;============= test stack =============
(define mem1 (memory-spush mem0))
(define mem2 (memory-sdecl mem1 "var1"))
(define mem3 (memory-sdecl mem2 "var2"))
(define mem4 (memory-swrite mem3 "var1" 1111))
(define mem5 (memory-swrite mem4 "var2" 2222))
(memory-sread mem5 "var1")
(memory-sread mem5 "var2")

(define mem6 (memory-spush mem5))
(define mem7 (memory-sdecl mem6 "var1"))
(define mem8 (memory-swrite mem7 "var1" 1122))
(newline)
(memory-sread mem8 "var1")
(memory-sread mem8 "var2")

(define mem9 (memory-spop mem8))
(newline)
(memory-sread mem9 "var1")
(memory-sread mem9 "var2")


;============= test heap plain =============
(define ret-pair (memory-alloc mem9 1))
(define mem10 (cdr ret-pair))
(define addr (car ret-pair))

(define mem11 (memory-hwrite mem10 addr 3333))
(newline)
addr
(memory-hread mem11 addr)


;============= test array =============
(define ret-pair2 (memory-alloc mem11 10))
(define arr1 (car ret-pair2))
(define mem12 (cdr ret-pair2))
(define index1 5)
(define mem13 (memory-awrite mem12 arr1 index1 4444))
(newline)
arr1
(memory-aread mem13 arr1 index1)



;============= test field access =============
(define fname1 "field1")
(define mem14 (memory-fdecl mem13 fname1))
(define ret-pair4 (memory-alloc mem14 1))
(define obj1 (car ret-pair4))
(define mem15 (cdr ret-pair4))
(define mem16 (memory-fwrite mem15 fname1 obj1 5555))
(newline)
fname1
obj1
(memory-fread mem16 fname1 obj1)

