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

;====================== Struct Definition ========================
;v-meta: vtab-meta
;s-meta: stack-meta
;addr-space: int -> int, the dynamic part of memory
(struct memory (id v-meta h-meta heap stack) #:transparent)

;name2tab: int(field name) -> int(addr of virtual table)
;top: the end of virtual table region, also beginning of object region, become fixed after machine initialization
(struct vtab-meta (name2tab top) #:transparent)

;a-top: top of array area
;o-top: top of object area
(struct heap-meta (o-top a-top) #:transparent)



;====================== Parameters ========================
;[+] => impacts performance;  [-] => does not impact performance

;[-]
;vt-num = max number of different member fields/functions
(define vt-num 1000)

;[-]
;vt-size = max number of objects
(define vt-size 1000)

;[+]
;scope-size = max number of different variable names
(define scope-size 500)

;[-]
(define vt-base-addr 0)

;[-]
(define max-program-length 10000)

;[-]
(define invalid-id 0)

;[?]
(define bv-width 64)




;====================== Memory Types =======================

(define bv-type (bitvector bv-width))

(define int-type integer?)

(define addr-type int-type)

(define default-type int-type)



(define all-types-ordered (list int-type bv-type))

(define (type->ordinal type)
	(if (equal? type int-type) 0 1))

(define (type-of v)
	(ormap (lambda (pred) (if (pred v) pred #f)) all-types-ordered))



;====================== Constant Values ========================
(define (nullptr type)
	(list-ref nullptr-list (type->ordinal type)))

(define nullptr-list (list -1 (bv-type -1)))

(define nullptr0 (nullptr int-type))

(define (is-nullptr? v)
	(equal? v (nullptr (type-of v))))



(define (not-found type)
	(list-ref not-found-list (type->ordinal type)))

(define not-found-list (list -6666666 (bv-type -6666666)))

(define (is-not-found? v)
	(equal? v (not-found (type-of v))))



(define (invalid-state type)
	(list-ref invalid-state-list (type->ordinal type)))

(define invalid-state-list (list -314159265 (bv-type -314159265)))

(define (is-invalid? v)
	(equal? v (invalid-state (type-of v))))


;====================== Tracking States =======================
(define memory-id-list null)
(define (memory-add-id id)
	(set! memory-id-list (cons id memory-id-list)))

(define memory-id-map (list->vector (std:build-list max-program-length (lambda (x) (not-found int-type)))))
(define (memory-archive id mem)
	(vector-set! memory-id-map id mem))

(define memory-id-counter invalid-id)
(define (memory-new-id)
	(set! memory-id-counter (+ 1 memory-id-counter))
	(display (~a "New state id: " memory-id-counter "\n"))
	memory-id-counter)


