#lang rosette

(require rackunit
         "../src/demo.rkt")

(test-case "interpreter"
           (check-equal? result 2 "interpreter 0"))
