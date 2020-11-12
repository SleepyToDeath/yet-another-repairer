#lang rosette/safe

(require (prefix-in std: racket/base))
(require racket/pretty)
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(require "match-define.rkt")
(require "string-id.rkt")
(require "syntax.rkt")
(require "syntax-jimple.rkt")
(require "semantics-relational.rkt")
(require "semantics-computational.rkt")
(require (prefix-in p: "jimple/jimple-parser.rkt"))

(define src-dir "../benchmark/benchmark1/sootOutput/")

(define src-classes (list
"net.floodlightcontroller.dhcpserver.DHCPInstance$DHCPInstanceBuilder.jimple"
"net.floodlightcontroller.dhcpserver.DHCPInstance.jimple"
"net.floodlightcontroller.dhcpserver.DHCPInstanceTest.jimple"
"org.projectfloodlight.openflow.types.IPv4Address.jimple"))

(define class-src-list (map (lambda (src-class) (std:read-string 9999999 (std:open-input-file (std:string-append src-dir src-class)))) src-classes))

(define cls-list (map (lambda (class-src) (p:build-ast-file (p:parse-to-stx class-src))) class-src-list))

(define buggy (program
	(class-list cls-list)))

(pretty-print buggy)
