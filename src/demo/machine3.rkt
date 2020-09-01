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

(define pc-init 0)
(define pc-fin -1)

(define (craft-program src)
	(define (program pc) 
		(list-ref src pc)
	)
	program
)

(define (craft-machine src input)
	(machine (craft-program src) input pc-init)
)

;[TODO] dummy
(define (lookup-label m l)
	0
)

(define (continue m)
	(((machine-prog m) (machine-pc m)) m))

(define (update-ass m v)
	(continue (struct-copy machine m [mem (append (machine-mem m) (list v))][pc (+ (machine-pc m) 1)])))

(define (update-br m l)
	(continue (struct-copy machine m [pc (lookup-label m l)])))
	

(provide (struct-out machine) craft-machine pc-init pc-fin continue update-ass update-br lookup-label)

