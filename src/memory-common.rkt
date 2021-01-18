#lang rosette/safe

(provide (all-defined-out))

; Dynamic Memory Layout:
;       ============================================================================
; name: | virtual tables | objects | arraies | stack          |
;       ----------------------------------------------------------------------------
; size: | vt-size * n    | vt-size | n * n   | scope-size * n |
;       ============================================================================

;Tips:
;	1.No memory is reused to avoid GC
;	2.All tops take the maximum input at branches to avoid uncertainty

;vt-num = max number of different member fields/functions
(define vt-num 1000)
;vt-size = max number of objects
(define vt-size 1000)
;scope-size = max number of different variable names
(define scope-size 1000)

(define vt-base-addr 0)
;no need to know beginning of arraies

(define stack-bottom (* (* 2 vt-num) vt-size))

(define max-program-length 1000)

;v-meta: vtab-meta
;s-meta: stack-meta
;addr-space: int -> int, the dynamic part of memory
(struct memory (v-meta s-meta h-meta addr-space) #:transparent)

;name2tab: int(field name) -> int(addr of virtual table)
;top: the end of virtual table region, also beginning of object region, become fixed after machine initialization
(struct vtab-meta (name2tab top) #:transparent)

;bases: a list of stack base address, first element is stack top scope
;top: allocation top of stack area
(struct stack-meta (bases top) #:transparent)

;a-top: top of array area
;o-top: top of object area
(struct heap-meta (o-top a-top) #:transparent)

(define no-scope 0)
