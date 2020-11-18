#lang rosette/safe

(provide (all-defined-out))

; Memory Layout:
;       ========================================================================================================================================
; name:  heap top | stack top | stack pointer | obj-top | obj-butt | virtual table index | virtual tables | objects | arraies | scopes         |
;       ----------------------------------------------------------------------------------------------------------------------------------------
; size:  1        | 1         | 1             | 1       | 1        | vt-num              | vt-size * n    | vt-size | n * n   | scope-size * n |
;       ========================================================================================================================================

; Scope Layout:
;       =========================
; name:  next scope addr | imap |
;       -------------------------
; size:  1               | n    |
;       =========================

;Tips:
;	No memory is reused to avoid GC!!!
;	heap top = end of the last array
;	stack top = end of the last allocated scope (might be freed)
;	stack pointer = beginning of the scope at current top of stack (logically, not `stack top`)
;	next scope = the scope at one level lower in the stack

;vt-num = max number of different member fields/functions
(define vt-num 1024)
;vt-size = max number of objects
(define vt-size 1024)
;scope-size = max number of different variable names
(define scope-size 1024)

(define heap-top-addr 1)
(define obj-top-addr 4)
(define obj-butt-addr 5)
(define vt-index-addr 6)
(define vt-base-addr (+ vt-index-addr vt-num))
;no need to know beginning of arraies

(define stack-top-addr 2)
(define stack-pointer-addr 3)
(define stack-bottom (* (* 2 vt-num) vt-size))

