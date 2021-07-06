#lang rosette

;(require "instruction.rkt")

; prog = [TODO] list of instructions??? (or CFG? or AST? How to implement goto?)
; instruction = a function updating mem & pc
; mem = a list
; pc = a position in prog
; instructions are marked as debug target
(struct machine (prog mem pc)
	#:transparent
)

(define (craft-program src)
	(define (program pc) 
		(list-ref src pc)
	)
	program
)

(define (craft-machine src input)
	(machine (craft-program src) input pc-init)
)

(define pc-init 0)
(define pc-fin -1)

; run: init machine -> finish machine
(define (run m)
	(cond
		[(= (machine-pc m) pc-fin) m]
		[else (run (((machine-prog m) (machine-pc m)) m))]
	)
)

(provide (struct-out machine) craft-machine run pc-init pc-fin)

