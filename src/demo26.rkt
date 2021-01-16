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
(require "formula.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))

(define src-dir "../benchmark/benchmark1/sootOutput/")

(define src-classes (list
"net.floodlightcontroller.dhcpserver.DHCPInstance$DHCPInstanceBuilder.jimple"
"net.floodlightcontroller.dhcpserver.DHCPInstance.jimple"
"net.floodlightcontroller.dhcpserver.DHCPInstanceTest.jimple"
"org.projectfloodlight.openflow.types.IPv4Address.jimple"
"java.lang.Object.jimple"))

(define class-src-list (map (lambda (src-class) (std:read-string 9999999 (std:open-input-file (std:string-append src-dir src-class)))) src-classes))

(define cls-list (map (lambda (class-src) (p:build-ast-file (p:parse-to-stx class-src))) class-src-list))

(define buggy (program
	(class-list cls-list)))

(pretty-print buggy)

(define input1 null)
(define output1 (list (cons var-ret-name 0)))

(define mac (ast->machine buggy))
(pretty-print string-id-map)

(define mac-in (assign-input mac input1))

(define mac-fin (compute mac-in))

(define result (compare-output mac-fin output1))

result

(pretty-print string-id-table)
(display "===============================================================================================================\n")
(display "================================================ Encoding ... =================================================\n")
(display "===============================================================================================================\n")

(output-smt #t)
(define bugl (localize-bug buggy (list (cons input1 output1))))
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
