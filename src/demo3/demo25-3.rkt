#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require "match-define.rkt")
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))

(define class-A (p:build-ast-file (p:parse-to-stx
"
public class T {
  public static int main() {
	  int r0;
	  r0 = 1 + 2;
	  return r0;
  }
}")))

(define buggy (program
	(class-list (list class-A))))

(define input1 (list (cons "r1" 1)))
(define output1 (list (cons var-ret-name 30)))

(match-define (cons soft hard) (ast->relation buggy))

(println string-id-map)

(define tf (hard input1 output1))

(display "\n")

(display "Top Formula:\n")
tf

(display "\nAsserts\n")
(asserts)

(display "\nSolution:\n")
(define debug-sol (optimize #:maximize soft
          #:guarantee (assert tf)))

debug-sol

soft
