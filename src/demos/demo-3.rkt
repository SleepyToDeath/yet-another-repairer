#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require "../localization.rkt")
(require "../match-define.rkt")
(require "../string-id.rkt")
(require "../syntax.rkt")
(require "../syntax-jimple.rkt")
(require "../semantics-relational.rkt")
(require "../semantics-computational.rkt")
(require "../semantics-common.rkt")
(require "../memory-common.rkt")
(require "../formula.rkt")
(require racket/format)
(require (prefix-in p: "../jimple/jimple-parser.rkt"))
(require (prefix-in p: "../jimple/jimple-utils.rkt"))

(define src-dir "../../benchmark/benchmark3/sootOutput/")

(define src-classes (list
"java.lang.Object.jimple"
;"java.lang.Enum.jimple"
;"net.floodlightcontroller.firewall.FirewallRule$FirewallAction.jimple"
"net.floodlightcontroller.firewall.FirewallRule.jimple"
"net.floodlightcontroller.firewall.FirewallRuleTest.jimple"
"org.projectfloodlight.openflow.types.MacAddress.jimple"
))

(define class-src-list (map (lambda (src-class) (begin 
	(display (~a "Parsing src file: " src-class "\n"))
	(std:read-string 9999999 (std:open-input-file (std:string-append src-dir src-class))))) src-classes))

(define cls-list (map (lambda (class-src) (p:build-ast-file (p:parse-to-stx class-src))) class-src-list))

(define buggy (p:transform-all (program
	(class-list cls-list))))

;(pretty-print buggy)

(define input1 (list (cons (bv 100 bv-type) "long")))
(define output1 (list (cons var-ret-name 1)))

(define input2 (list (cons (bv 200 bv-type) "long")))
(define output2 (list (cons var-ret-name 0)))

(define mac1 (ast->machine buggy))
(define mac-in1 (assign-input mac1 input1))
(define mac-fin1 (compute mac-in1))
(define result1 (compare-output mac-fin1 output1))
result1

(define mac2 (ast->machine buggy))
(define mac-in2 (assign-input mac2 input2))
(define mac-fin2 (compute mac-in2))
(define result2 (compare-output mac-fin2 output2))
result2

(pretty-print string-id-table)
(display "===============================================================================================================\n")
(display "================================================ Encoding ... =================================================\n")
(display "===============================================================================================================\n")

(output-smt #t)
(define bugl (localize-bug buggy (list (cons input1 output1)) (list (cons input2 output2))))
(pretty-print bugl)

;(match-define (cons soft hard) (ast->relation buggy))

;(define tf1 (hard input1 output1))

;(display "\n")
;(pretty-print string-id-table)

;(display "\n Solving: \n")


;(define fml-no-bug (equal? (apply + soft) (length soft)))
;(define fml-one-bug (equal? (apply + soft) (- (length soft) 1)))

;(define debug-sol (optimize #:maximize (list (apply + soft))
;          #:guarantee (assert (and tf1))))

;(define debug-sol (solve (assert (and tf1 fml-one-bug))))

;debug-sol

;((lambda ()
;(print-pending-eval debug-sol)
;(display "\n")))
