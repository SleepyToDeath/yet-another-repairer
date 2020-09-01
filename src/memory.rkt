#lang rosette/safe

(struct mymap (func) #:transparent )

(define (mymap-load mmap index)
  (define f (mymap-func mmap))
  (f index))

(define (mymap-store mmap index value)
  (define oldf (mymap-func mmap))
  (define newf (lambda (args)
                 (if (equal? args index) value (oldf args))))
  (mymap newf))

(define nullptr -1)

(define (default-func x)
	nullptr)

(define addr-counter 0)

(define (malloc)
	(begin
		(set! addr-counter (+ addr-counter 1))
		addr-counter))

(provide all-defined-out)

