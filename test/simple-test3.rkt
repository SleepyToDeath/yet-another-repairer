#lang rosette

(require rackunit
         "../src/simple-syntax-transformer.rkt")

(test-case "transformer"
           (check-equal? a 2 "transformer 1")
           (check-equal? b 2 "transformer 2"))
