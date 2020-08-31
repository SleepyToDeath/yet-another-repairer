#lang rosette/safe

(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`

(define-symbolic l0 l1 l2 l3 l4 l5 l6  boolean?)
(define-symbolic lc0 lc1 lc2 lc3 lc4 lc5 lc6  boolean?)
(define-symbolic lp0 lp1 lp2 lp3 lp4 lp5 lp6  boolean?)

;	 public void setServerIP(IPv4Address paramIP)
;    {
;	     IPv4Address r0, $r1;
;        DHCPInstance r2;
;        --java.lang.IllegalArgumentException $r3;
;		 ret_exception


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

(define (get-tf x y)
	(define-symbolic* paramIP integer?)
	(define-symbolic* this paramIP-obj integer?)
	(define-symbolic* r0 $r1 r2 r2-shadow integer?)
	(define-symbolic* ret-e1 ret-e2 ret-e3 boolean?)
	(define-symbolic* z0 zi1 zi2 zi3 z1 z2 z3 z4 z5 z6 z7 z8 boolean?)
	(define-symbolic* zc0 boolean?)

	(define-symbolic* guard1 boolean?)

	(define serverIP-map1 (mymap default-func))
	(define serverIP-map2 (mymap-store serverIP-map1 r2-shadow r0))
	(define serverIP-map3 (if guard1 serverIP-map1 serverIP-map2))

	(define-symbolic* IPv4Address-NONE integer?)


	(define-symbolic* zp00 zp01 zp02 boolean?)
	
	;[TODO] global memory
	(define (tf-init-ip secs this zp0) 

		(set! addr-counter (+ addr-counter 1))
		(define current-addr addr-counter)

		(define-symbolic* rp0 rp0-shadow integer?)
		(define-symbolic* ip0 integer?)
		(define-symbolic* zp1 zp2 boolean?)

		(define secs-map1 (mymap default-func))
		(define secs-map2 (mymap-store secs-map1 rp0-shadow ip0))

;    public void <init>(int)
;    {
;        IPv4Address r0;
;        int i0;
		(and
			(= this current-addr)
;        r0 := @this: IPv4Address;
			(equal? zp0 (and (implies lp0 (= rp0 this)) zp1))

;        i0 := @parameter0: int;
			(equal? zp1 (and (implies lp1 (= ip0 secs)) zp2))

;        specialinvoke r0.<java.lang.Object: void <init>()>();
		;[TODO]

;        r0.<IPv4Address: int secs> = i0;
			(equal? zp2
						(implies lp2 
							(and
								(= rp0-shadow (if zp2 rp0 nullptr))
								(= (mymap-load secs-map2 rp0) ip0))))))
;        return;
;    }


	(define tf-init-ip-1 (tf-init-ip paramIP paramIP-obj zp01))

; 		init
	(define tf-set-ip (and 
					(equal? z0 (and (implies l0 (equal? ret-e1 #f)) zi1))
					(equal? zi1 (and zc0 zi2))
					(equal? zi2 (and zp01 z1))

;        r2 := @this: DHCPInstance;
					(equal? z1 (and (implies l1 (= r2 this)) z2))

;        r0 := @parameter0: IPv4Address;
					(equal? z2 (and (implies l2 (= r0 paramIP-obj)) z3))

;        $r1 = <IPv4Address: IPv4Address NONE>;
					(equal? z3 (and (implies l3 (= $r1 IPv4Address-NONE)) z4))

;        if r0 != $r1 goto label1;
					(equal? z4 (and 
									(implies l4 (equal? guard1 (not (= r0 $r1))))
									(or (and (implies l4 (not guard1)) z5) (and (implies l4 guard1) z6))))
					;A /\ (B \/ C) === (A /\ B) \/ (A /\ C)

;        --$r3 = new java.lang.IllegalArgumentException;

;        --specialinvoke $r3.<java.lang.IllegalArgumentException: void <init>(java.lang.String)>("Empty argument!");

;        --throw $r3;
;		 ret-e = true;
					(equal? z5 (and (implies l5 
												(and (equal? ret-e2 #t) 
												(equal? ret-e3 ret-e2)))
												z7))
;		 return;

;     label1:
;        r2.<DHCPInstance: IPv4Address serverIP> = r0;
					(equal? z6 
									(and 
										(implies l6 
											(and
											(= r2-shadow (if z6 r2 nullptr))
											(= (mymap-load serverIP-map2 r2) r0)
											(equal? ret-e3 ret-e1)))
										z7))))

	(define-symbolic* rc0 integer?)
	(define-symbolic* zc1 zc2 boolean?)

	(define tf-init-ip-0 (implies lc1 (tf-init-ip 0 rc0 zp00)))

	(define tf-clinit (and 
;    static void <clinit>()
;    {
;        IPv4Address $r0;

;        $r0 = new IPv4Address;
		 ; [TODO] dtype[rc0] = IPv4Address

;        specialinvoke $r0.<IPv4Address: void <init>(int)>(0);
			(equal? zc0 (and (implies lc1 zp00) zc1))

;        <IPv4Address: IPv4Address NONE> = $r0;
			(equal? zc1 (implies lc2 (= IPv4Address-NONE rc0)))))

;        return;
;    }

	(define tf-ip-equal #t)

	(define in (= x paramIP))
	(define out (equal? y ret-e3))
	(define this-0 (= this 0))

	(and z0 tf-set-ip in out tf-clinit tf-init-ip-0 tf-init-ip-1 this-0))

(define tf1 (get-tf 0 #t))
(define tf2 (get-tf 1 #f))

(define hard-constraint (and tf1 tf2 l0 l1 l2 lc1 lc2 lp0 lp1 lp2))

(define (b2i b)
  (if b 1 0))

(optimize #:maximize (list (+ (b2i l1) (b2i l2) (b2i l3) (b2i l4) (b2i l5) (b2i l6)))
          #:guarantee (assert hard-constraint))


