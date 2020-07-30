#lang rosette

(require rackunit
         "../src/simple.rkt")

(test-case "my operations"
           (check-equal? (my-plus 1 2) 3 "my plus")
           (check-equal? (my-minus 3 2) 1 "my minus"))

