#lang rosette

(define-syntax (foo stx) #'+)

(define (wtf f) (f 1 1))

(define a (wtf foo))


(define-syntax (bar stx) (datum->syntax stx (cons '+ (cdr (syntax->datum stx)))))

(define b (bar 1 1))


(provide a b foo bar)
