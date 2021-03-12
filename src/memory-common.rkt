#lang rosette/safe
(require (prefix-in std: racket/base))
(require racket/format)

(provide (all-defined-out))

; Dynamic Memory Layout:
;       ============================================================================
; name: | virtual tables | objects | arraies |
;       ----------------------------------------------------------------------------
; size: | vt-size * n    | vt-size | n * n   |
;       ============================================================================

;Tips:
;	1.No memory is reused to avoid GC
;	2.All tops take the maximum input at branches to avoid uncertainty

;vt-num = max number of different member fields/functions
(define vt-num 1000)
;vt-size = max number of objects
(define vt-size 1000)
;scope-size = max number of different variable names
(define scope-size 500)

(define vt-base-addr 0)
;no need to know beginning of arraies

(define stack-bottom (* (* 2 vt-num) vt-size))

(define max-program-length 10000)

;v-meta: vtab-meta
;s-meta: stack-meta
;addr-space: int -> int, the dynamic part of memory
(struct memory (id v-meta h-meta heap stack) #:transparent)

;name2tab: int(field name) -> int(addr of virtual table)
;top: the end of virtual table region, also beginning of object region, become fixed after machine initialization
(struct vtab-meta (name2tab top) #:transparent)

;bases: a list of stack base address, first element is stack top scope
;top: allocation top of stack area
;(struct stack-meta (bases top) #:transparent)

;a-top: top of array area
;o-top: top of object area
(struct heap-meta (o-top a-top) #:transparent)

(define invalid-id 0)
(define no-scope 0)
(define nullptr -1)
(define not-found -66666666)
(define invalid-state -314159265)

(define (is-not-found? v)
	(equal? v not-found))

(define (is-invalid? v)
	(equal? v invalid-state))

(define bv-width 64)
(define int-type integer?)
(define bv-type (bitvector bv-width))

(define (type->ordinal type)
	(if (equal? (type int-type)) 0 1))

(define all-types-ordered (list int-type bv-type))





;====================== Tracking States ===================
(define memory-id-list null)
(define (memory-add-id id)
	(set! memory-id-list (cons id memory-id-list)))

(define memory-id-map (list->vector (std:build-list max-program-length (lambda (x) not-found))))
(define (memory-archive id mem)
	(vector-set! memory-id-map id mem))

(define memory-id-counter invalid-id)
(define (memory-new-id)
	(set! memory-id-counter (+ 1 memory-id-counter))
	(display (~a "New state id: " memory-id-counter "\n"))
	memory-id-counter)


