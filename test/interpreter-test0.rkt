#lang rosette

(require rackunit
         "../src/demo.rkt")

(test-case "interpreter"
           (check-equal? (m2) 0 "interpreter 0"))
