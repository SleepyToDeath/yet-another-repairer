#lang rosette

(require rackunit
         "../src/simple.rkt")

(test-case "my operations"
           (check-equal? (my-plus 1 1) 2 "my plus")
           (check-equal? (my-minus 1 1) 0 "my minus"))

