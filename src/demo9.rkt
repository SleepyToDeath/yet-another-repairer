#lang rosette/safe

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(define-symbolic l0 l1 l2 l3 l4 l5 l6  boolean?)

;	 public void setServerIP(IPv4Address paramIP)
;    {
;	     IPv4Address r0, $r1;
;        DHCPInstance r2;
;        --java.lang.IllegalArgumentException $r3;
;		 ret_exception


(struct mymap (func))

(define (mymap-load mmap indices)
  (define f (mymap-func mmap))
  (apply f indices))

(define (mymap-store mmap value indices)
  (define oldf (mymap-func mmap))
  (define newf (lambda args
                 (if (andmap equal? args indices) value (apply oldf args))))
  (mymap newf))

(define (default-func x)
	0)

(define (get-tf x y)
	(define-symbolic* paramIP this integer?)
	(define-symbolic* r0 $r1 r2 r2-shadow integer?)
	(define-symbolic* ret-e1 ret-e2 ret-e3 boolean?)
	(define-symbolic* z0 z1 z2 z3 z4 z5 z6 z7 z8 boolean?)

	(define-symbolic* guard1 boolean?)

	(define serverIP-map1 (mymap default-func))
	(define serverIP-map2 (mymap-store serverIP-map1 r2-shadow r0))
	(define serverIP-map3 (if guard1 serverIP-map1 serverIP-map2))

	(define IPv4Address-NONE 0)

; 		init
	(define tf (and (implies l0 (equal? z0 (and (equal? ret-e1 #f) z1)))

;        r2 := @this: DHCPInstance;
					(implies l1 (equal? z1 (and (= r2 this) z2)))

;        r0 := @parameter0: IPv4Address;
					(implies l2 (equal? z2 (and (= r0 paramIP) z3)))

;        $r1 = <IPv4Address: IPv4Address NONE>;
					(implies l3 (equal? z3 (and (= $r1 IPv4Address-NONE) z4)))

;        if r0 != $r1 goto label1;
					(implies l4 (equal? z4 (and 
												(equal? guard1 (not (= r0 $r1))) 
												(or (and (not guard1) z5) (and guard1 z6)))))
					;A /\ (B \/ C) === (A /\ B) \/ (A /\ C)

;        --$r3 = new java.lang.IllegalArgumentException;

;        --specialinvoke $r3.<java.lang.IllegalArgumentException: void <init>(java.lang.String)>("Empty argument!");

;        --throw $r3;
;		 ret-e = true;
					(implies l5 (equal? z5 (and 
												(equal? ret-e2 #t) 
												(equal? ret-e3 ret-e2)
												z7)))
;		 return;

;     label1:
;        r2.<DHCPInstance: IPv4Address serverIP> = r0;
					(implies l6 (equal? z6 
									(and 
										(= r2-shadow (if guard1 r2 0))
										(and (not (= (mymap-load serverIP-map2 r2) 0)) (= (mymap-load serverIP-map1 r2) 0))
										(equal? ret-e3 ret-e1)
										z7)))))

	(define in (= x paramIP))
	(define out (equal? y ret-e3))

	(and z0 tf in out))

(define tf1 (get-tf 0 #t))

(define hard-constraint tf1)

(define (b2i b)
  (if b 1 0))

(optimize #:maximize (list (+ (b2i l0) (b2i l1) (b2i l2) (b2i l3) (b2i l4) (b2i l5) (b2i l6)))
          #:guarantee (assert hard-constraint))


