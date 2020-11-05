#!/usr/bin/env racket
#lang racket

; Pretty print S expressions
; Usage: ./pretty-print.rkt < input > output

(require racket/pretty)

(define (pretty-write-all)

  (define next (read))
 
  (when (not (eof-object? next))
    (pretty-write next)
    (pretty-write-all)))
      
(pretty-write-all)
