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
(require "../formula.rkt")
(require "../enumerator.rkt")
(require racket/format)
(require (prefix-in p: "../jimple/jimple-parser.rkt"))

(define src-dir "../../benchmark/benchmark10/sootOutput/")

(define src-classes (list
    "java.lang.Enum.jimple"
	"java.lang.Object.jimple"
	"java.util.HashSet.jimple"
	"java.util.HashMap.jimple"
;	"java.util.Set.jimple"
	"net.floodlightcontroller.core.FloodlightContext.jimple"
	"net.floodlightcontroller.core.IFloodlightProviderService.jimple"
	"net.floodlightcontroller.core.IListener$Command.jimple"
	"net.floodlightcontroller.core.IListener.jimple"
	"net.floodlightcontroller.core.IOFMessageListener.jimple"
	"net.floodlightcontroller.core.IOFSwitch.jimple"
	"net.floodlightcontroller.linkdiscovery.internal.LinkDiscoveryManager.jimple"
	"net.floodlightcontroller.linkdiscovery.internal.LinkDiscoveryManagerTest.jimple"
	"net.floodlightcontroller.linkdiscovery.internal.MockFloodlightProvider.jimple"
	"net.floodlightcontroller.linkdiscovery.internal.MockOFSwitch.jimple"
	"net.floodlightcontroller.packet.LLDP.jimple"
	"net.floodlightcontroller.packet.LLDPTLV.jimple"
	"net.floodlightcontroller.topology.NodePortTuple.jimple"
	"org.openflow.protocol.OFPhysicalPort.jimple"
	"org.openflow.protocol.OFPort.jimple"))


(define class-src-list (map (lambda (src-class) (begin 
	(display (~a "Parsing src file: " src-class "\n"))
	(std:read-string 9999999 (std:open-input-file (std:string-append src-dir src-class))))) src-classes))

(define cls-list (map (lambda (class-src) (p:build-ast-file (p:parse-to-stx class-src))) class-src-list))

(define buggy (program
	(class-list cls-list)))

(define input1 (list (cons 1 "short")))
(define output1 (list (cons var-ret-name 1)))
(define input2 (list (cons 3 "short")))
(define output2 (list (cons var-ret-name 1)))

(define (verify input output)
	(define mac (ast->machine buggy))
	(define mac-in (assign-input mac input))
	(define mac-fin (compute mac-in))
	(machine-prepare-recursion mac-fin)
	(compare-output mac-fin output))

(verify input1 output1)
(display2 (~a "Executed " line-counter-c " lines of code\n"))
(verify input2 output2)
(display2 (~a "Executed " line-counter-c " lines of code\n"))

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
