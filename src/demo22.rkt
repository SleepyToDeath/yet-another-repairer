#lang rosette/safe

(require "string-id.rkt")

(string-id "var1")

(string-id "var2")

(string-id "var1")

(string-id "var2")

(define (f _name)
	(define name (string-id _name))
	(+ name 1) )

(f "var2")

(define-symbolic* i integer?)

i

(maybe-string-id (+ 1 i))

