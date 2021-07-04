#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require "localization.rkt")
(require "match-define.rkt")
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")
(require "semantics-common.rkt")
(require "formula.rkt")
(require racket/format)
(require (prefix-in p: "jimple/jimple-parser.rkt"))
(require (prefix-in p: "jimple/jimple-utils.rkt"))

(define src-dir "../benchmark/benchmark2/sootOutput/")

(define src-classes (list
"java.lang.Object.jimple"
"java.util.Collection.jimple"
"java.util.HashMap.jimple"
"java.util.ArrayList.jimple"
"java.util.HashSet.jimple"
"net.floodlightcontroller.loadbalancer.ILoadBalancerService.jimple"
"net.floodlightcontroller.loadbalancer.LBPool.jimple"
"net.floodlightcontroller.loadbalancer.LBVip.jimple"
"net.floodlightcontroller.loadbalancer.LoadBalancer.jimple"
"net.floodlightcontroller.loadbalancer.LoadBalancerTest.jimple"))

(define class-src-list (map (lambda (src-class) (begin 
	(display (~a "Parsing src file: " src-class "\n"))
	(std:read-string 9999999 (std:open-input-file (std:string-append src-dir src-class))))) src-classes))

(define cls-list (map (lambda (class-src) (p:build-ast-file (p:parse-to-stx class-src))) class-src-list))

(define buggy (p:transform-all (program
	(class-list cls-list))))

(define input1 (list (cons (string-id "0") "string")))
(define output1 (list (cons var-ret-name 0)))

(define input2 (list (cons (string-id "1") "string")))
(define output2 (list (cons var-ret-name -1)))

(define (verify input output)
	(define mac (ast->machine buggy))
	(define mac-in (assign-input mac input))
	(define mac-fin (compute mac-in))
	(compare-output mac-fin output))

(pretty-print string-id-table)
(display2 "======== Verify examples ========\n")
(display2 "Example 1:\n")
(display2 (verify input1 output1))
(display2 "\n")
(display2 "Example 2:\n")
(display2 (verify input2 output2))
(display2 "\n")

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
