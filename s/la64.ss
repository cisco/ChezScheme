(define-registers
  (reserved
   [%tc        %r22 #t 22]
   [%sfp       %r12 #f 12]
   [%ap        %r13 #f 13])
  (allocable
   [%ac0 %r14 #f 14]
   [%xp  %r15 #f 15]
   [%ts  %r16 #f 16]
   [%td  %r17 #f 17]
   [%ac1 %r18 %deact #f 18]
   [%yp  %r19 #f 19]
   [%cp  %r20 #f 20]
   [     %r4 %Carg1 %Cretval #f 4]
   [     %r5 %Carg2 #f 5]
   [     %r6 %Carg3 #f 6]
   [     %r7 %Carg4 #f 7]
   [     %r8 %Carg5 #f 8]
   [     %r9 %Carg6 #f 9]
   [     %r10 %Carg7 #f 10]
   [     %r11 %Carg8 #f 11]
;; [     %r2         #f 2] ; tp, unallocatable
;; [     %r18 #t 18]
;; [     %r19 #t 19]
;; [     %r20 #t 20]
;; [     %r21 #t 21] ; reserved
;; [     %r22 #t 22]
   [     %r23 #t 23]
   [     %r24 #t 24]
   [     %r25 #t 25]
   [     %r26 #t 26]
   [     %r27 #t 27]
   [     %r28 #t 28]
   [     %r29 #t 29]
;; [     %r30 #t 30]
;; [     %r31 #t 31]
   )
  (machine-dependent
   [%real-zero %r0 #f 0]
   [%ra %r1 #f 1]
   [%sp %r3 #t 3]
   [%jump %scratch %r30 #t 30]
   [%cond          %r31 #t 31]
   [%Cfparg1 %Cfpretval %f0 #f 0]
   [%Cfparg2 %f1           #f  1]
   [%Cfparg3 %f2           #f  2]
   [%Cfparg4 %f3           #f  3]
   [%Cfparg5 %f4           #f  4]
   [%Cfparg6 %f5           #f  5]
   [%Cfparg7 %f6           #f  6]
   [%Cfparg8 %f7           #f  7]
   [%flreg1  %f8           #f  8]
   [%flreg2  %f9           #f  9]))




(module (md-handle-jump)
  (import asm-module)

  (define-syntax seq
    (lambda (x)
      (syntax-case x ()
        [(_ e ... ex)
         (with-syntax ([(t ...) (generate-temporaries #'(e ...))])
           #'(let ([t e] ...)
               (with-values ex
                 (case-lambda
                   [(x*) (cons* t ... x*)]
                   [(x* p) (values (cons* t ... x*) p)]))))])))

  (define lmem? mref?)

  (define mem?
    (lambda (x)
      (or (lmem? x) (literal@? x))))

  (define lvalue->ur
    (lambda (x k)
      (if (mref? x)
          (let ([u (make-tmp 'l)])
            (seq
             (set-ur=mref u x)
             (k u)))
          (k x))))

   (define mref->mref
    (lambda (a k)
      (define return
        (lambda (x0 x1 imm)
          (safe-assert (or (eq? x1 %zero) (eqv? imm 0)))
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm)))))
      (nanopass-case (L15c Triv) a
                     [(mref ,lvalue0 ,lvalue1 ,imm)
                      (lvalue->ur lvalue0
                                  (lambda (x0)
                                    (lvalue->ur lvalue1
                                                (lambda (x1)
                                                  (cond ; either x1 is %zero or imm is 0
                                                   [(and (eq? x1 %zero) (signed12? imm))
                                                    (return x0 %zero imm)]
                                                   [else
                                                    (let ([u (make-tmp 'mref)])
                                                      (seq
                                                       (build-set! ,u (immediate ,imm))
                                                       (if (eq? x1 %zero)
                                                           (return x0 u 0)
                                                           (seq
                                                            (build-set! ,u (asm ,null-info ,asm-add ,u ,x1))
                                                            (return x0 u 0)))))])))))])))

   (define mem->mem
     (lambda (a k)
       (cond
        [(literal@? a)
         (let ([u (make-tmp 'mem)])
           (seq
            (build-set! ,u ,(literal@->literal a))
            (k (with-output-language (L15d Lvalue) `(mref ,u ,%zero 0)))))]
        [else (mref->mref a k)])))

   (define set-ur=mref
     (lambda (ur mref)
       (mref->mref mref
                   (lambda (mref)
                     (build-set! ,ur ,mref)))))

   (define imm-signed12?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (signed12? imm)]
                      [else #f])))

   (define imm-unsigned12?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (unsigned12? imm)]
                      [else #f])))

   (define imm-shamt?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (shamt? imm)]
                      [else #f])))

   (define imm-signed14?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (signed14?)]
                      [else #f])))

   (define imm-signed16?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (signed16? imm)]
                      [else #f])))

   (define imm-signed20?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (signed20? imm)]
                      [else #f])))

   (define imm-signed26?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (signed26? imm)]
                      [else #f])))

   (define md-handle-jump
     (lambda (t)
       (with-output-language (L15d Tail)
                             (define long-form
                               (lambda (e)
                                 (let ([tmp (make-tmp 'utmp)])
                                   (values
                                    (in-context Effect `(set! ,(make-live-info) ,tmp ,e))
                                    `(jump ,tmp)))))
                             (nanopass-case (L15c Triv) t
                                            [,lvalue
                                             (if (mem? lvalue)
                                                 (mem->mem lvalue (lambda (e) (values '() `(jump ,e))))
                                                 (values '() `(jump ,lvalue)))]
                                            [(literal ,info)
                                             (guard (and (not (info-literal-indirect? info))
                                                         (memq (info-literal-type info) '(entry library-code))))
                                        ; NB: really need to use unspillable or mark %ip (aka %ts) killed here but can't without extending jump syntax
                                             (values '() `(jump (literal ,info)))]
                                            [(label-ref ,l ,offset)
                                        ; NB: really need to use unspillable or mark %ip (aka %ts) killed here but can't without extending jump syntax
                                             (values '() `(jump (label-ref ,l ,offset)))]
                                            [else (long-form t)]))))

  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (memq 'ur aty*)
             (and (memq 'imm12 aty*) (imm-signed12? a))
             (and (memq 'imm16 aty*) (imm-signed14? a))
             (and (memq 'imm16 aty*) (imm-signed16? a))
             (and (memq 'imm20 aty*) (imm-signed20? a))
             (and (memq 'imm26 aty*) (imm-signed26? a))
             (and (memq 'shamt aty*) (imm-shamt? a))
             (and (memq 'mem aty*) (mem? a))))]))

  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
          [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
          [(and (memq 'imm12 aty*) (imm-signed12? a)) (k (imm->imm a))]
          [(and (memq 'imm14 aty*) (imm-signed14? a)) (k (imm->imm a))]
          [(and (memq 'imm16 aty*) (imm-signed16? a)) (k (imm->imm a))]
          [(and (memq 'imm20 aty*) (imm-signed20? a)) (k (imm->imm a))]
          [(and (memq 'imm26 aty*) (imm-signed26? a)) (k (imm->imm a))]
          [(and (memq 'shamt aty*) (imm-shamt? a)) (k (imm->imm a))]
          [(memq 'ur aty*)
           (cond
            [(ur? a) (k a)]
            [(imm? a)
             (let ([u (make-tmp 'cim)])
               (seq
                (build-set! ,u ,(imm->imm a))
                (k u)))]
            [(mem? a)
             (mem->mem a
                       (lambda (a)
                         (let ([u (make-tmp 'cmem)])
                           (seq
                            (build-set! ,u ,a)
                            (k u)))))]
            [else (sorry! 'coerce-opnd "unexpected operand ~s" a)])]
          [else (sorry! 'coerce-opnd "cannot coerce ~s to ~s" a aty*)]))]))

  (define-syntax define-instruction
    (lambda (x)
      (define make-value-clause
        (lambda (fmt)
          (syntax-case fmt (mem ur)
            [(op (c mem) (a ur))
             #`(lambda (c a)
                 (if (lmem? c)
                     (coerce-opnd a '(ur)
                                  (lambda (a)
                                    (mem->mem c
                                              (lambda (c)
                                                (rhs c a)))))
                     (next c a)))]
            [(op (c ur) (a aty ...) ...)
             #`(lambda (c a ...)
                 (if (and (coercible? a '(aty ...)) ...)
                     #,(let f ([a* #'(a ...)] [aty** #'((aty ...) ...)])
                         (if (null? a*)
                             #'(if (ur? c)
                                   (rhs c a ...)
                                   (let ([u (make-tmp 'uu)])
                                     (seq
                                      (rhs u a ...)
                                      (mref->mref c
                                                  (lambda (c)
                                                    (build-set! ,c ,u))))))
                             #`(coerce-opnd #,(car a*) '#,(car aty**)
                                            (lambda (#,(car a*)) #,(f (cdr a*) (cdr aty**))))))
                     (next c a ...)))])))

      (define-who make-pred-clause
        (lambda (fmt)
          (syntax-case fmt ()
            [(op (a aty ...) ...)
             #`(lambda (a ...)
                 (if (and (coercible? a '(aty ...)) ...)
                     #,(let f ([a* #'(a ...)] [aty** #'((aty ...) ...)])
                         (if (null? a*)
                             #'(rhs a ...)
                             #`(coerce-opnd #,(car a*) '#,(car aty**)
                                            (lambda (#,(car a*)) #,(f (cdr a*) (cdr aty**))))))
                     (next a ...)))])))

      (define-who make-effect-clause
        (lambda (fmt)
          (syntax-case fmt ()
            [(op (a aty ...) ...)
             #`(lambda (a ...)
                 (if (and (coercible? a '(aty ...)) ...)
                     #,(let f ([a* #'(a ...)] [aty** #'((aty ...) ...)])
                         (if (null? a*)
                             #'(rhs a ...)
                             #`(coerce-opnd #,(car a*) '#,(car aty**)
                                            (lambda (#,(car a*)) #,(f (cdr a*) (cdr aty**))))))
                     (next a ...)))])))


      (syntax-case x (definitions)
        [(k context (sym ...) (definitions defn ...) [(op (a aty ...) ...) ?rhs0 ?rhs1 ...] ...)
                                        ; potentially unnecessary level of checking, but the big thing is to make sure
                                        ; the number of operands expected is the same on every clause of define-intruction
         (and (not (null? #'(op ...)))
              (andmap identifier? #'(sym ...))
              (andmap identifier? #'(op ...))
              (andmap identifier? #'(a ... ...))
              (andmap identifier? #'(aty ... ... ...)))
         (with-implicit (k info return with-output-language)
           (with-syntax ([((opnd* ...) . ignore) #'((a ...) ...)])
             (define make-proc
               (lambda (make-clause)
                 (let f ([op* #'(op ...)]
                         [fmt* #'((op (a aty ...) ...) ...)]
                         [arg* #'((a ...) ...)]
                         [rhs* #'((?rhs0 ?rhs1 ...) ...)])
                   (if (null? op*)
                       #'(lambda (opnd* ...)
                           (sorry! name "no match found for ~s" (list opnd* ...)))
                       #`(let ([next #,(f (cdr op*) (cdr fmt*) (cdr arg*) (cdr rhs*))]
                               [rhs (lambda #,(car arg*)
                                      (let ([#,(car op*) name])
                                        #,@(car rhs*)))])
                           #,(make-clause (car fmt*)))))))
             (unless (let ([a** #'((a ...) ...)])
                       (let* ([a* (car a**)] [len (length a*)])
                         (andmap (lambda (a*) (fx= (length a*) len)) (cdr a**))))
               (syntax-error x "mismatched instruction arities"))
             (cond
              [(free-identifier=? #'context #'value)
               #`(let ([fvalue (lambda (name)
                                 (lambda (info opnd* ...)
                                   defn ...
                                   (with-output-language (L15d Effect)
                                                         (#,(make-proc make-value-clause) opnd* ...))))])
                   (begin
                     (safe-assert (eq? (primitive-type (%primitive sym)) 'value))
                     (primitive-handler-set! (%primitive sym) (fvalue 'sym)))
                   ...)]
              [(free-identifier=? #'context #'pred)
               #`(let ([fpred (lambda (name)
                                (lambda (info opnd* ...)
                                  defn ...
                                  (with-output-language (L15d Pred)
                                                        (#,(make-proc make-pred-clause) opnd* ...))))])
                   (begin
                     (safe-assert (eq? (primitive-type (%primitive sym)) 'pred))
                     (primitive-handler-set! (%primitive sym) (fpred 'sym)))
                   ...)]
              [(free-identifier=? #'context #'effect)
               #`(let ([feffect (lambda (name)
                                  (lambda (info opnd* ...)
                                    defn ...
                                    (with-output-language (L15d Effect)
                                                          (#,(make-proc make-effect-clause) opnd* ...))))])
                   (begin
                     (safe-assert (eq? (primitive-type (%primitive sym)) 'effect))
                     (primitive-handler-set! (%primitive sym) (feffect 'sym)))
                   ...)]
              [else (syntax-error #'context "unrecognized context")])))]
        [(k context (sym ...) cl ...) #'(k context (sym ...) (definitions) cl ...)]
        [(k context sym cl ...) (identifier? #'sym) #'(k context (sym) (definitions) cl ...)])))


  (define info-cc-eq (make-info-condition-code 'eq? #f #t))
  (define asm-eq (asm-relop info-cc-eq))

  (define-instruction value (+)
    [(op (z ur) (x ur) (y imm12 ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,y))])

  (define-instruction value (+/carry)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add/carry ,x ,y))])

  (define-instruction value (+/ovfl)
    [(op (z ur) (x ur) (y ur))
     (let ([u1 (make-tmp 'u1+)]
           [u2 (make-tmp 'u2+)]
           [u3 (make-tmp 'u3+)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u3 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,null-info ,asm-add/ovfl ,x ,y ,u1 ,u2 ,u3))))])

  (define-instruction value (-)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub ,x ,y))])

  (define-instruction value (-/ovfl)
    [(op (z ur) (x ur) (y ur))
     (let ([u1 (make-tmp 'u1-)]
           [u2 (make-tmp 'u2-)]
           [u3 (make-tmp 'u3-)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u3 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,null-info ,asm-sub/ovfl ,x ,y ,u1 ,u2 ,u3))))])

  (define-instruction value (-/eq)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub/eq ,x ,y))])

  (define-instruction value (*)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,x ,y))])

  (define-instruction value (*/ovfl)
    [(op (z ur) (x ur) (y ur))
     (let ([u1 (make-tmp 'u1*)]
           [u2 (make-tmp 'u2*)]
           [u3 (make-tmp 'u3*)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u3 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,null-info ,asm-mul/ovfl ,x ,y ,u1 ,u2 ,u3))))])

  (define-instruction value (/)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-div ,x ,y))])

  (let ()
    (define select-op
      (lambda (op)
        (case op
          [(logand) asm-logand]
          [(logor) asm-logor]
          [(logxor) asm-logxor]
          [else (sorry! #f "unexpected logical operator ~s" op)])))

    (define-instruction value (logand logor logxor)
      [(op (z ur) (x ur) (y imm12 ur))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,x ,y))]))

  (define-instruction value (lognot)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))])

  (let ()
    (define select-op
      (lambda (op)
        (case op
          [(sll) asm-sll]
          [(srl) asm-srl]
          [(sra) asm-sra])))

    (define-instruction value (sll srl sra)
      [(op (z ur) (x ur) (y shamt ur))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,x ,y))]))

  (define-instruction value (move)
    [(op (z mem) (x ur))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem imm12))
     `(set! ,(make-live-info) ,z ,x)])

  (define-instruction value lea1 ;;@ todo addi?
                                        ; NB: would be simpler if offset were explicit operand
                                        ; NB: why not one version of lea with %zero for y in lea1 case?
    [(op (z ur) (x ur)) ;;@ z = x + offset
     (begin
       (let ([offset (info-lea-offset info)])
         (if (signed12? offset)
             `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x (immediate ,offset))) ;;@ not Trivited yet, use (immediate)
             (let ([u (make-tmp 'lea1)])
               (seq
                `(set! ,(make-live-info) ,u (immediate ,offset))
                `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,u)))))))])

  (define-instruction value lea2
                                        ; NB: would be simpler if offset were explicit operand
    [(op (z ur) (x ur) (y ur)) ;;@ z = x + y + offset
     (let ([offset (info-lea-offset info)] [u (make-tmp 'lea2)])
       (seq
        (if (signed12? offset)
            `(set! ,(make-live-info) ,u (asm ,info ,asm-add ,y (immediate ,offset)))
            (seq
             `(set! ,(make-live-info) ,u (immediate ,offset))
             `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,u ,y))))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,u))))])

  (define-instruction value (sext8 sext16 sext32 zext8 zext16 zext32)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-move/extend op) ,x))])

  (let ()
    (define imm-zero (with-output-language (L15d Triv) `(immediate 0)))
    (define load/store
      (lambda (x y w k) ; logic of w depends on its type
        (with-output-language (L15d Effect)
                              (if (ur? w) ;;@ todo base:x index:y offset:w, one of y and w must be 0?
                                  (if (eq? y %zero) ; take w as rs
                                      (k x w imm-zero)
                                      (let ([u (make-tmp 'ls1)]) ;; load/store 1
                                        (seq
                                         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w)) ;;@ todo w <- w + y?
                                         (k x u imm-zero))))
                                  (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
                                    (if (signed12? n)
                                        (if (eq? y %zero)
                                            (let ([w (in-context Triv `(immediate ,n))])
                                              (k x y w))
                                            (let ([u (make-tmp 'ls2)])
                                              (seq
                                               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w)) ;;@ todo w <- w + y?
                                               (k x u imm-zero))))
                                        (let ([u (make-tmp 'ls3)])
                                          (seq
                                           `(set! ,(make-live-info) ,u (immediate ,n))
                                           (if (eq? y %zero)
                                               (k x u imm-zero)
                                               (seq
                                                `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,u)) ;;@ changed the base to u instead
                                                (k u y imm-zero)))))))))))

    (define-instruction value (load)
      [(op (z ur) (x ur) (y ur) (w imm12 ur))
       (let ([type (info-load-type info)])
         (load/store x y w
                     (lambda (x y w)
                       (let ([instr `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-load type) ,x ,y ,w))])
                         (if (info-load-swapped? info) ;; change endianness
                             (seq
                              instr
                              `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-swap type) ,z)))
                             instr)))))])

    (define-instruction effect (store)
      [(op (x ur) (y ur) (w imm12 ur) (z ur))
       (let ([type (info-load-type info)])
         (load/store x y w
                     (lambda (x y w)
                       (if (info-load-swapped? info) ;; change endianness
                           (let ([u (make-tmp 'u)])
                             (seq
                              `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-swap type) ,z))
                              `(asm ,null-info ,(asm-store type) ,x ,y ,w ,u)))
                           `(asm ,null-info ,(asm-store type) ,x ,y ,w ,z)))))]))

  (define-instruction effect push
    [(op (x ur)) `(asm ,info ,asm-push ,x)])

  (define-instruction value pop
    [(op (z ur)) `(set! ,(make-live-info) ,z (asm ,info ,asm-pop))])

  ;; floating-point operations always get operands from memory first
  (let ()
    (define pick-asm-op
      (lambda (op info)
        (let ([flreg (info-loadfl-flreg info)])
          (case op
            [(load-single->double load-double->single) (asm-fl-load/cvt op flreg)]
            [(store-single->double) (asm-fl-store/cvt op flreg)]
            [else (asm-fl-load/store op flreg)]))))

    (define-instruction effect (load-single->double
                                load-double->single
                                store-single->double
                                store-single
                                store-double
                                load-single
                                load-double)
      [(op (x ur) (y ur) (z imm12))
       (if (eq? y %zero)
           `(asm ,info ,(pick-asm-op op info) ,x ,z)
           (let ([u (make-tmp 'fl1)])
             (seq
              `(set! ,(make-live-info) ,u (asm ,info ,asm-add ,x ,y))
              `(asm ,info ,(pick-asm-op op info) ,u ,z))))]
      [(op (x ur) (y ur) (z ur))
       (let ([u (make-tmp 'fl2)])
         (seq
          `(set! ,(make-live-info) ,u (asm ,info ,asm-add ,x ,z))
          (if (eq? y %zero)
              `(asm ,info ,(pick-asm-op op info) ,u (immediate 0))
              (seq
               `(set! ,(make-live-info) ,u (asm ,info ,asm-add ,u ,y))
               `(asm ,info ,(pick-asm-op op info) ,u (immediate 0))))))]))

  (define-instruction effect (flt)
    [(op (x ur) (y ur))
     `(asm ,info ,asm-flt ,x ,y)])

  (define-instruction effect (fl+ fl- fl/ fl*)
    [(op (x ur) (y ur) (z ur))
     `(asm ,info ,(asm-flop-2 op) ,x ,y ,z)])

  (define-instruction value (trunc)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-trunc ,x))])

  ;; pred all return multiple values
  (define-instruction pred (fl= fl< fl<=)
    [(op (x ur) (y ur))
     (let ([info (make-info-condition-code op #f #f)]) ;;@ todo check params
       (values '() `(asm ,info ,(asm-fl-relop info) ,x ,y)))])

  (define-instruction effect (flsqrt)
    [(op (x ur) (y ur)) `(asm ,info ,asm-flsqrt ,x ,y)])

  (define-instruction effect inc-cc-counter
    [(op (x ur) (w imm12 ur) (z imm12 ur)) ;;@ base offset val
     (let ([u1 (make-tmp 'inc1)] [u2 (make-tmp 'inc2)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-add ,x ,w))
        `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
        `(asm ,null-info ,asm-inc-cc-counter ,u1 ,z ,u2)))])

  (define-instruction effect inc-profile-counter
    [(op (x mem) (y imm12 ur))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u ,x)
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,u ,y))
        `(set! ,(make-live-info) ,x ,u)))])

  (define-instruction value (read-time-stamp-counter)
    [(op (z ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-read-time-stamp-counter))])

  (define-instruction value (read-performance-monitoring-counter)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-read-performance-monitoring-counter ,x))])

  (define-instruction value (get-tc) ;;@ from arm64.ss
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-get-tc ,u))))])

  (define-instruction value (activate-thread) ;;@ from arm64.ss
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-activate-thread ,u))))])

  (define-instruction effect (deactivate-thread) ;;@ from ppc32.ss
    [(op)
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(asm ,info ,asm-deactivate-thread ,u)))])

  (define-instruction effect (unactivate-thread) ;;@ from ppc32.ss
    [(op (z ur))
     (safe-assert (eq? z %Carg1))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(asm ,info ,asm-unactivate-thread ,u)))])

  (define-instruction value (asmlibcall)
    [(op (z ur))
     (let ([u (make-tmp 'asmlib)]) ;; for building jump addr
       (seq
            `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
            `(set! ,(make-live-info) ,z
                   (asm ,info ,(asm-library-call (info-asmlib-libspec info) (info-asmlib-save-ra? info)) ,u ,(info-kill*-live*-live* info) ...))))])

  (define-instruction effect (asmlibcall!)
    [(op)
     (let ([u (make-tmp 'asmlib!)])
       (seq
            `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
            `(asm ,info ,(asm-library-call! (info-asmlib-libspec info) (info-asmlib-save-ra? info)) ,u ,(info-kill*-live*-live* info) ...)))])

  ;;@ todo assert needed?
  (safe-assert (reg-callee-save? %tc))  ; no need to save-restore
  (define-instruction effect (c-simple-call)
    [(op)
     (let ([u (make-tmp 'c-simple)])
       (seq
            `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
            `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info) (info-c-simple-call-save-ra? info)) ,u)))])

  (define-instruction pred (eq? u< > < <= >=)
    [(op (x ur) (y ur))
     (let ([info (if (eq? op 'eq?)
                     info-cc-eq
                     (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))]) ;;@ todo asm-relop

  (define-instruction pred (condition-code)
    [(op) (values '() `(asm ,info ,(asm-condition-code info)))])

  (define-instruction pred (type-check?)
    [(op (x ur) (mask ur) (type ur)) ;;@ todo smarter
     (let ([u (make-tmp 'u)])
       (values
        (with-output-language (L15d Effect)
                              `(set! ,(make-live-info) ,u (asm ,null-info ,asm-logand ,x ,mask)))
        `(asm ,info-cc-eq ,asm-eq ,u ,type)))])

  (define-instruction pred (logtest log!test)
    [(op (x ur) (y ur))                       ;;@ todo types
     (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,x ,y))])

  (let ()
    (define lea->reg                ; put base+index+offset in one reg
      (lambda (x y w k)
        (with-output-language (L15d Effect)
                              (define add-offset
                                (lambda (r)
                                  (if (eqv? (nanopass-case (L15d Triv) w [(immediate ,imm) imm]) 0)
                                      (k r)
                                      (let ([u (make-tmp 'u)])
                                        (seq
                                         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,r ,w))
                                         (k u))))))
                              (if (eq? y %zero)
                                  (add-offset x)
                                  (let ([u (make-tmp 'u)])
                                    (seq
                                     `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,y))
                                     (add-offset u)))))))

    (define-instruction pred (lock!) ;;@ todo memory alignment
      [(op (x ur) (y ur) (w imm12))
       (let ([u (make-tmp 'u)])
         (values
          (lea->reg x y w
                    (lambda (r)
                      (with-output-language (L15d Effect)
                                            (seq
                                             `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
                                             `(asm ,null-info ,asm-lock ,r ,u)))))
          `(asm ,info-cc-eq ,asm-eq ,u ,%real-zero)))])

    (define-instruction effect (locked-incr! locked-decr!)
      [(op (x ur) (y ur) (w imm12))
       (lea->reg x y w
                 (lambda (r)
                   (let ([u1 (make-tmp 'u1)] [u2 (make-tmp 'u2)])
                     (seq
                      `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
                      `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
                      `(asm ,null-info ,(asm-lock+/- op) ,r ,u1 ,u2)))))])

    (define-instruction effect (cas)
      [(op (x ur) (y ur) (w imm12) (old ur) (new ur))
       (lea->reg x y w
                 (lambda (r)
                   (let ([u1 (make-tmp 'u1)] [u2 (make-tmp 'u2)])
                     (seq
                      `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
                      `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
                      `(asm ,info ,asm-cas ,r ,old ,new ,u1 ,u2)))))]))

  (define-instruction effect (pause)
    [(op) `(asm ,info ,asm-fence)])

  (define-instruction effect (c-call)
    [(op (x ur))
     (seq
;;      `(set! ,(make-live-info) ,ura (asm ,null-info ,asm-kill))
      `(asm ,info ,asm-indirect-call ,x ,(info-kill*-live*-live* info) ...))])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect (invoke-prelude)
    [(op) `(set! ,(make-live-info) ,%tc ,%Carg1)])

  )

(module asm-module (asm-add asm-add/carry asm-add/ovfl asm-sub asm-sub/ovfl asm-sub/eq
                    asm-mul asm-mul/ovfl asm-div asm-logand asm-logor asm-logxor asm-lognot asm-logtest
                    asm-read-performance-monitoring-counter asm-read-time-stamp-counter asm-inc-cc-counter
                    asm-enter asm-sll asm-srl asm-sra asm-flsqrt asm-trunc asm-flt
                    asm-cas asm-relop asm-fl-relop asm-flop-2 asm-save-flrv asm-restore-flrv
                    asm-direct-jump asm-indirect-jump asm-literal-jump asm-condition-code
                    asm-jump asm-conditional-jump asm-library-jump
                    asm-get-tc asm-activate-thread asm-deactivate-thread asm-unactivate-thread
                    asm-push asm-pop asm-return asm-c-return asm-data-label asm-kill
                    asm-rp-header asm-fl-load/cvt asm-fl-store/cvt asm-fl-load/store
                    asm-load asm-store asm-fence asm-swap asm-lock asm-lock+/- asm-move asm-move/extend
                    asm-return-address asm-indirect-call asm-library-call asm-library-call! asm-c-simple-call
                    asm-foreign-call asm-foreign-callable
                    asm-size signed12? unsigned12? signed14? signed16? signed20? signed26? shamt? )

    (define-syntax define-op
    (lambda (x)
      (syntax-case x ()
        [(k op handler e ...)
         (with-syntax ([op (construct-name #'k "asmop-" #'op)])
           #'(define-syntax op
               (syntax-rules ()
                 [(_ mneu arg (... ...))
                  (handler 'mneu e ... arg (... ...))])))]))) ;;@ e: opcodes

  (define-syntax emit
    (lambda (x)
      (syntax-case x ()
        [(k op x ...)
         (with-syntax ([emit-op (construct-name #'k "asmop-" #'op)])
           #'(emit-op op x ...))])))

  (define-op revb.2h two-R-op #b1101)
  (define-op revb.2w two-R-op #b1110)
  (define-op revb.d  two-R-op #b1111)
  (define-op ext.w.h two-R-op #b10110)
  (define-op ext.w.b two-R-op #b10111)
  (define-op fsqrt.d two-R-op #b100010100010010)
  (define-op fcvt.d.s  two-R-op #b100011001001001)
  (define-op fcvt.s.d  two-R-op #b100011001000110)
  (define-op ffint.l.d two-R-op #b100011011001010)
  (define-op ffint.d.l two-R-op #b100011101001010)
  (define-op rdtime.d  two-R-op #b11010)

  (define-op add.d three-R-op #b100001)
  (define-op sub.d three-R-op #b100011)
  (define-op mul.d three-R-op #b111011)
  (define-op div.d three-R-op #b1000100)
  (define-op fadd.d three-R-op #b1000000010)
  (define-op fsub.d three-R-op #b1000000110)
  (define-op fmul.d three-R-op #b1000001010)
  (define-op fdiv.d three-R-op #b1000001110)
  (define-op slt  three-R-op #b100100)
  (define-op sltu three-R-op #b100101)
  (define-op and  three-R-op #b101001)
  (define-op or   three-R-op #b101010)
  (define-op xor  three-R-op #b101011)
  (define-op sll.d three-R-op #b110001)
  (define-op srl.d three-R-op #b110010)
  (define-op sra.d three-R-op #b110011)
  (define-op ldx.b three-R-op #b00111000000000000)
  (define-op ldx.h three-R-op #b00111000000001000)
  (define-op ldx.w three-R-op #b00111000000010000)
  (define-op ldx.d three-R-op #b00111000000011000)
  (define-op ldx.bu three-R-op #b00111000001000000)
  (define-op ldx.hu three-R-op #b00111000001001000)
  (define-op ldx.wu three-R-op #b00111000001010000)
  (define-op stx.b three-R-op #b00111000000100000)
  (define-op stx.h three-R-op #b00111000000101000)
  (define-op stx.w three-R-op #b00111000000110000)
  (define-op stx.d three-R-op #b00111000000111000)
  (define-op fldx.s three-R-op #b00111000001100000)
  (define-op fldx.d three-R-op #b00111000001101000)
  (define-op fstx.s three-R-op #b00111000001110000)
  (define-op fstx.d three-R-op #b00111000001111000)

  (define-op slli.d shift-imm-op #b1000001)
  (define-op srli.d shift-imm-op #b1000101)
  (define-op srai.d shift-imm-op #b1001001)

  (define-op slti  two-R-imm12-op #b1000)
  (define-op sltui two-R-imm12-op #b1001)
  (define-op addi.d two-R-imm12-op #b1011)
  (define-op addi.w two-R-imm12-op #b1010)
  (define-op andi two-R-imm12-op #b1101)
  (define-op ori  two-R-imm12-op #b1110)
  (define-op xori two-R-imm12-op #b1111)
  (define-op ld.b two-R-imm12-op #b10100000)
  (define-op ld.h two-R-imm12-op #b10100001)
  (define-op ld.w two-R-imm12-op #b10100010)
  (define-op ld.d two-R-imm12-op #b10100011)
  (define-op ld.bu two-R-imm12-op #b10101000)
  (define-op ld.hu two-R-imm12-op #b10101001)
  (define-op ld.wu two-R-imm12-op #b10101010)
  (define-op st.b two-R-imm12-op #b10100100)
  (define-op st.h two-R-imm12-op #b10100101)
  (define-op st.w two-R-imm12-op #b10100110)
  (define-op st.d two-R-imm12-op #b10100111)
  (define-op fld.s two-R-imm12-op #b10101100)
  (define-op fst.s two-R-imm12-op #b10101101)
  (define-op fld.d two-R-imm12-op #b10101110)
  (define-op fst.d two-R-imm12-op #b10101111)

  (define-op ll.d two-R-imm14-op #b100010)
  (define-op sc.d two-R-imm14-op #b100011)

  (define-op lu12i.w one-R-imm20-op #b1010)
  (define-op lu32i.d one-R-imm20-op #b1011)
  (define-op pcaddi  one-R-imm20-op #b1100)
  (define-op pcaddu12i  one-R-imm20-op #b1110)

  (define-op beqz one-R-imm21-op #b10000)
  (define-op bnez one-R-imm21-op #b10001)
  (define-op bceqz one-R-imm21-op #b10010)
  (define-op bcnez one-R-imm21-op #b10010)

  (define-op jirl two-R-imm16-op #b10011)
  (define-op bne  two-R-imm16-op #b10111)

  (define-op b  imm26-op #b10100)
  (define-op bl imm26-op #b10101)

  (define-op ibar sync-op #b111000011100101)

  #|
  cd(cc reg)=0
  cond(all compare quiet): ceq #x4
  clt #x2
  cle #x6

  use bceqz to test fp/cc and set %cond:
  |#
  (define-op fcmp.d fp-cmp-op #b11000010)

  (define two-R-op
    (lambda (op opcode dest src code*)
      (emit-code (op dest src code*)
                 [10 opcode]
                 [5 (ax-ea-reg-code src)]
                 [0 (ax-ea-reg-code dest)])))

  (define shift-imm-op
    (lambda (op opcode dest src shamt code*)
      (emit-code (op dest src shamt code*)
                 [16 opcode]
                 [10 (ax-imm-data shamt op)]
                 [5 (ax-ea-reg-code src)]
                 [0 (ax-ea-reg-code dest)])))

  (define three-R-op
    (lambda (op opcode dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
                 [15 opcode]
                 [10 (ax-ea-reg-code src1)]
                 [5 (ax-ea-reg-code src0)]
                 [0 (ax-ea-reg-code dest)])))

  (define two-R-imm12-op
    (lambda (op opcode dest src0 imm12 code*)
      (emit-code (op dest src0 imm12 code*)
                 [22 opcode]
                 [10 (ax-imm-data imm12 op)]
                 [5 (ax-ea-reg-code src0)]
                 [0 (ax-ea-reg-code dest)])))

  (define two-R-imm14-op
    (lambda (op opcode dest src0 imm14 code*)
      (emit-code (op dest src0 imm14 code*)
                 [24 opcode]
                 [10 (ax-imm-data imm14 op)]
                 [5 (ax-ea-reg-code src0)]
                 [0 (ax-ea-reg-code dest)])))

  (define two-R-imm16-op
    (lambda (op opcode link src imm16 code*)
      (emit-code (op link src imm16 code*)
                 [26 opcode]
                 [10 (ax-imm-data imm16 op)]
                 [5 (ax-ea-reg-code src)]
                 [0 (ax-ea-reg-code link)])))

  (define one-R-imm20-op
    (lambda (op opcode dest imm20 code*)
      (emit-code (op dest imm20 code*)
                 [25 opcode]
                 [5 (ax-imm-data imm20 op)]
                 [0 (ax-ea-reg-code dest)])))

  (define one-R-imm21-op
    (lambda (op opcode src imm21 code*)
      (let ([offs0  (logand (ax-imm-data imm21 op) #xffff)] ; offs[15:0]
            [offs16 (logand (ash (ax-imm-data imm21 op) -16) #b11111)] ; offs[20:16]
            [s (case op
                 [(bceqz) #b0000]
                 [(bcnez) #b0100]
                 [else (ax-ea-reg-code src)])])
        (emit-code (op src imm21 code*)
                   [26 opcode]
                   [10 offs0]
                   [5 s]
                   [0 offs16]))))

  (define imm26-op
    (lambda (op opcode imm26 code*)
      (let ([offs0  (logand (ax-imm-data imm26 op) #xffff)] ; offs[15:0]
            [offs16 (logand (ash (ax-imm-data imm26 op) -16) #b1111111111)]) ; offs[25:16] 10 bits
        (emit-code (op imm26 code*)
                   [26 opcode]
                   [10 offs0]
                   [0  offs16]))))

  (define sync-op
    (lambda (op opcode code*)
      (emit-code (op code*)
                 [0 opcode])))

  (define-who fp-cmp-op
    (lambda (op opcode cmp src0 src1 code*)
      (emit-code (op cmp src0 src1 code*)
                 [20 opcode]
                 [15 (case cmp
                       [(fl=) #x4]
                       [(fl<) #x2]
                       [(fl<=) #x6]
                       [else (sorry! who "unrecognized op ~s" cmp)])]
                 [10 (ax-ea-reg-code src1)]
                 [5 (ax-ea-reg-code src0)]
                 [3 0]
                 [0 0])))


  (define shamt?
    (lambda (imm)
      (and (fixnum? imm) (fx<= imm (expt 2 6)))))
  (define signed12?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 11))
                               imm
                               (fx- (expt 2 11) 1)))))
  (define unsigned12?
    (lambda (imm)
      (and (fixnum? imm)
           ($fxu< imm (expt 2 12)))))
  (define signed14?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 13))
                               imm
                               (fx- (expt 2 13) 1)))))
  (define signed16?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 15))
                               imm
                               (fx- (expt 2 15) 1)))))
  (define signed20?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 19))
                               imm
                               (fx- (expt 2 19) 1)))))
  (define signed26?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 25))
                               imm
                               (fx- (expt 2 25) 1)))))
  (define signed32?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 31))
                               imm
                               (fx- (expt 2 31) 1)))))
  ;; for jirl
  (define jump-disp?
    (lambda (imm)
      (and (fixnum? imm)
           (= (mod imm 4) 0)
           (fx<= (fx- (expt 2 17))
                 imm
                 (fx- (expt 2 17) 1)))))
  ;; for b and bl, 28-bit offset, but the lower 2 bits are not encoded
  (define branch-disp?
    (lambda (imm)
      (and (fixnum? imm)
           (= (mod imm 4) 0)
           (fx<= (fx- (expt 2 27))
                 imm
                 (fx- (expt 2 27) 1)))))
  ;; 23-bit offset, but the lower 2 bits are not encoded
  (define cond-branch-disp?
    (lambda (imm)
      (and (fixnum? imm)
           (= (mod imm 4) 0)
           (fx<= (fx- (expt 2 22))
                 imm
                 (fx- (expt 2 22) 1)))))

  (define-who ax-ea-reg-code
    (lambda (ea)
      (if (pair? ea)
          (record-case ea
            [(reg) r (reg-mdinfo r)]
            [else (sorry! who "ea=~s" ea)])
          (reg-mdinfo ea))))

  (define ax-imm-data
    (case-lambda
     [(ea) (if (pair? ea)
               (record-case ea
                            [(imm) (n) n]
                            [else ($oops 'assembler-internal "ax-imm-data ea=~s" ea)])
               (if (number? ea)
                   ea
                   ($oops 'assembler-internal "ax-imm-data ea=~s" ea)))]
     [(ea op) (if (pair? ea)
                  (record-case ea
                               [(imm) (n) n]
                               [else ($oops 'assembler-internal "ax-imm-data ea=~s ~s" ea op)])
                  (if (number? ea)
                      ea
                      ($oops 'assembler-internal "ax-imm-data ea=~s ~s" ea op)))]))

  (define ax-reg?
    (lambda (ea)
      (record-case ea
        [(reg) ignore #t]
        [else #f])))

  (define ax-imm?
    (lambda (ea)
      (record-case ea
        [(imm) ignore #t]
        [else #f])))

  (define-syntax emit-code
    (lambda (x)
                                        ; NB: probably won't need emit-code to weed out #f
      (define build-maybe-cons*
        (lambda (e* e-ls)
          (if (null? e*)
              e-ls
              #`(let ([t #,(car e*)] [ls #,(build-maybe-cons* (cdr e*) e-ls)])
                  (if t (cons t ls) ls)))))
      (syntax-case x ()
        [(_ (op opnd ... ?code*) chunk ...)
         (build-maybe-cons* #'((build long (byte-fields chunk ...))) ;;@ long: all instructions are 32 bits
                            #'(aop-cons* `(asm ,op ,opnd ...) ?code*))])))

  (define-syntax build
    (syntax-rules ()
      [(_ x e)
       (and (memq (datum x) '(byte word long quad)) (integer? (datum e)))
       (quote (x . e))]
      [(_ x e)
       (memq (datum x) '(byte word long quad))
       (cons 'x e)]))

  (define-syntax byte-fields
    (syntax-rules ()
      [(byte-fields (n e) ...)
       (andmap fixnum? (datum (n ...)))
       (+ (bitwise-arithmetic-shift-left e n) ...)]))


  ;; make sure x is 32-bit before using these
  (define upper20
    (lambda (x)
      (ash x -12)))
  (define lower12
    (lambda (x)
      (fxlogand x #xFFF)))

  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm la64-abs la64-jump la64-call) 0]
        [(byte) 1]
        [(word) 2]
        [(long) 4]
        [(quad code-top-link abs) 8] ;; aligned with compile.ss
        [else ($oops 'assembler-internal "unknown size: ~a" (car x))])))

  (define asm-add
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (record-case src1
                [(imm) (n) (emit addi.d dest src0 n code*)]
                [else (emit add.d dest src0 src1 code*)]))))

  ; carry if dest < src0(or src1)
  (define asm-add/carry
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit add.d dest src0 src1
                    (emit sltu `(reg . ,%cond) dest src0 code*)))))

                                        ; src0 src1 dest
                                        ; +    +    -
                                        ; -    -    +
                                        ; slt t1 zero src0
                                        ; slt t2 zero src1
                                        ; slt t3 zero dest
                                        ; xor t1 t1 t2
                                        ; xor t3 t3 t2  # sign(dest) and sign(src0/src1) differ
                                        ; xori t1 t1 1  # src0 and src1 have the same sign
                                        ; and %cond t1 t3

  (define asm-add/ovfl ;;@ todo imm? optimization?
    (lambda (code* dest src0 src1 t1 t2 t3)
      (Trivit (dest src0 src1 t1 t2 t3)
              (emit add.d dest src0 src1
                    (emit slt t1 `(reg . ,%real-zero) src0
                          (emit slt t2 `(reg . ,%real-zero) src1
                                (emit slt t3 `(reg . ,%real-zero) dest
                                      (emit xor t1 t1 t2
                                            (emit xor t3 t3 t2
                                                  (emit xori t1 t1 1
                                                        (emit and `(reg . ,%cond) t1 t3 code*)))))))))))


  (define asm-sub
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub.d dest src0 src1 code*))))
                                        ; dest src0 src1
                                        ; -    +    -
                                        ; +    -    +
                                        ; sub  dest src0 src1
                                        ; slt  t1 zero dest
                                        ; slt  t2 zero src1
                                        ; slt  t3 zero src0  # t3=1 if src0>0; t3=0 src0<=0
                                        ; xor  t1 t1 t2      # t1=0 if signs are the same
                                        ; xor  t3 t3 t2      # check whether the signs of src0 and src1 differ
                                        ; xori t1 t1 1       # t1=1 if signs are the same
                                        ; and  %cond t1 t3
  (define asm-sub/ovfl
    (lambda (code* dest src0 src1 t1 t2 t3)
      (Trivit (dest src0 src1 t1 t2 t3)
              (emit sub.d dest src0 src1
                    (emit slt t1 `(reg . ,%real-zero) dest
                          (emit slt t2 `(reg . ,%real-zero) src1
                                (emit slt t3 `(reg . ,%real-zero) src0
                                      (emit xor t1 t1 t2
                                            (emit xor t3 t3 t2
                                                  (emit xori t1 t1 1
                                                        (emit and `(reg . ,%cond) t1 t3 code*)))))))))))

  (define asm-sub/eq
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub.d dest src0 src1
                    (emit sltui `(reg . ,%cond) dest 1 code*))))) ;; set if dest == 0

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit mul.d dest src0 src1 code*))))

                                        ; case1: pos*pos=neg
                                        ; case2: neg*pos=pos or pos*neg=pos
                                        ; case3: neg*neg=neg
                                        ;
                                        ; case1 and case3:
                                        ; src0 src1 dest
                                        ; t2   t3   t1
                                        ; +    +    -
                                        ; -    -    -
                                        ; case2:
                                        ; +    -    +
                                        ; -    +    -
  (define asm-mul/ovfl
    (lambda (code* dest src0 src1 t0 t1 t2)
      (Trivit (dest src0 src1 t0 t1 t2)
              (let ([zero `(reg . ,%real-zero)])
                (emit mul.d dest src0 src1
                      (emit slt t0 zero dest
                            (emit slt t1 zero src0
                                  (emit slt t2 zero src1
                                        ;; case1
                                        (emit xor t1 t1 t2
                                              (emit xori t1 t1 1
                                                    (emit xori t0 t0 1
                                                          (emit and `(reg . ,%cond) t0 t1
                                                                ;; case2
                                                                (emit slt t0 zero dest
                                                                      (emit slt t1 zero src0
                                                                            (emit xor t1 t1 t2
                                                                                  (emit and t1 t1 t0
                                                                                        (emit or `(reg . ,%cond) `(reg . ,%cond) t1 code*)))))))))))))))))

  (define asm-div
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit div.d dest src0 src1 code*))))

  (module (asm-logand asm-logor asm-logxor)
    (define-syntax asm-logicalop
      (lambda (x)
        (syntax-case x ()
          [(_ op opi)
           #'(lambda (code* dest src0 src1)
               (Trivit (dest src0 src1)
                       (record-case src1
                         [(imm) (n) (emit opi dest src0 n code*)]
                         [else (emit op dest src0 src1 code*)])))])))

    (define asm-logand (asm-logicalop and andi))
    (define asm-logor (asm-logicalop or ori))
    (define asm-logxor (asm-logicalop xor xori)))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit xori dest src -1 code*))))

  ;; test whether common bits are set
  ;; result <- (not (zero? (and x y)))
  (define asm-logtest
    (lambda (i? info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
                (values
                 (emit and `(reg . ,%cond) x y
                       (emit sltui `(reg . ,%cond) `(reg . ,%cond) 1
                             (emit xori `(reg . ,%cond) `(reg . ,%cond) 1 '())))
                 (let-values ([(l1 l2) (if i? (values l2 l1) (values l1 l2))])
                   (asm-conditional-jump info l2 l1 offset)))))))

  (module (asm-sll asm-srl asm-sra)
    (define-syntax asm-shiftop
      (lambda (x)
        (syntax-case x ()
          [(_ op opi)
           #'(lambda (code* dest src0 src1)
               (Trivit (dest src0 src1)
                       (record-case src1
                         [(imm) (n) (emit opi dest src0 n code*)]
                         [else (emit op dest src0 src1 code*)])))])))

    (define asm-sll (asm-shiftop sll.d slli.d))
    (define asm-srl (asm-shiftop srl.d srli.d))
    (define asm-sra (asm-shiftop sra.d srai.d)))

  (define-who asm-fl-load/cvt
    (lambda (op flreg)
      (lambda (code* base offset)
        (Trivit (base offset)
                (let ([flreg2 `(reg . ,%flreg2)])
                  (case op
                    [(load-single->double)
                     (emit fld.s flreg2 base offset
                           (emit fcvt.d.s flreg flreg2 code*))]
                    [(load-double->single)
                     (emit fld.d flreg2 base offset
                           (emit fcvt.s.d flreg flreg2 code*))]
                    [else (sorry! who "unrecognized op ~s" op)]))))))

  (define-who asm-fl-store/cvt
    (lambda (op flreg)
      (lambda (code* base offset)
        (Trivit (base offset)
                (let ([flreg2 `(reg . ,%flreg2)])
                  (case op
                    [(store-single->double)
                     (emit fcvt.d.s flreg2 flreg
                           (emit fst.d flreg2 base offset code*))]
                    [else (sorry! who "unrecognized op ~s" op)]))))))

  (define-who asm-fl-load/store
    (lambda (op flreg)
      (lambda (code* base offset)
        (Trivit (base offset)
                (case op
                  [(load-single) (emit fld.s flreg base offset code*)]
                  [(load-double) (emit fld.d flreg base offset code*)]
                  [(store-single) (emit fst.s flreg base offset code*)]
                  [(store-double) (emit fst.d flreg base offset code*)]
                  [else (sorry! who "unrecognized op ~s" op)])))))

  (define-who asm-load
    (lambda (type)
      (rec asm-load-internal
           (lambda (code* dest base index offset)
             (let ([n (nanopass-case (L16 Triv) offset
                                     [(immediate ,imm) imm]
                                     [else (sorry! who "unexpected non-immediate offset ~s" offset)])])
               (Trivit (dest base)
                       (cond
                        [(eq? index %zero)
                         (case type
                           [(integer-64 unsigned-64) (emit ld.d dest base n code*)]
                           [(integer-32) (emit ld.w dest base n code*)]
                           [(unsigned-32) (emit ld.wu dest base n code*)]
                           [(integer-16) (emit ld.h dest base n code*)]
                           [(unsigned-16) (emit ld.hu dest base n code*)]
                           [(integer-8) (emit ld.b dest base n code*)]
                           [(unsigned-8) (emit ld.bu dest base n code*)]
                           [else (sorry! who "unexpected mref type ~s" type)])]
                        [(eqv? n 0) ;; maybe (Trivit index)
                         (case type
                           [(integer-64 unsigned-64) (emit ldx.d dest base index code*)]
                           [(integer-32) (emit ldx.w dest base index code*)]
                           [(unsigned-32) (emit ldx.wu dest base index code*)]
                           [(integer-16) (emit ldx.h dest base index code*)]
                           [(unsigned-16) (emit ldx.hu dest base index code*)]
                           [(integer-8) (emit ldx.b dest base index code*)]
                           [(unsigned-8) (emit ldx.bu dest base index code*)]
                           [else (sorry! who "unexpected mref type ~s" type)])]
                        [else (sorry! who "expected zero index or 0 offset, got ~s and ~s" index offset)])))))))

  (define-who asm-store
    (lambda (type)
      (rec asm-store-internal
           (lambda (code* base index offset src)
             (let ([n (nanopass-case (L16 Triv) offset
                                     [(immediate ,imm) imm]
                                     [else (sorry! who "unexpected non-immediate offset ~s" offset)])])
               (Trivit (src base)
                       (cond
                        [(eq? index %zero)
                         (case type
                           [(integer-64 unsigned-64) (emit st.d src base n code*)]
                           [(integer-32 unsigned-32) (emit st.w src base n code*)]
                           [(integer-16 unsigned-16) (emit st.h src base n code*)]
                           [(integer-8 unsigned-8) (emit st.b src base n code*)]
                           [else (sorry! who "unexpected mref type ~s" type)])]
                        [(eqv? n 0)
                         (case type
                           [(integer-64 unsigned-64) (emit stx.d src base index code*)]
                           [(integer-32 unsigned-32) (emit stx.w src base index code*)]
                           [(integer-16 unsigned-16) (emit stx.h src base index code*)]
                           [(integer-8 unsigned-8) (emit stx.b src base index code*)]
                           [else (sorry! who "unexpected mref type ~s" type)])]
                        [else (sorry! who "expected zero index or 0 offset, got ~s and ~s" index offset)])))))))

  (define-who asm-flop-2
    (lambda (op)
      (lambda (code* src1 src2 dest) ;; args contain the base addr of fp numbers, thus need the fp disp
        (Trivit (src1 src2 dest)
                (let ([flreg1 `(reg . ,%flreg1)]
                      [flreg2 `(reg . ,%flreg2)]
                      [disp (constant flonum-data-disp)])
                  (emit fld.d flreg1 src1 disp
                        (emit fld.d flreg2 src2 disp
                              (let ([code* (emit fst.d flreg1 dest disp code*)]) ;;@ %flreg1 holds the result
                                (case op
                                  [(fl+) (emit fadd.d flreg1 flreg1 flreg2 code*)]
                                  [(fl-) (emit fsub.d flreg1 flreg1 flreg2 code*)]
                                  [(fl*) (emit fmul.d flreg1 flreg1 flreg2 code*)]
                                  [(fl/) (emit fdiv.d flreg1 flreg1 flreg2 code*)]
                                  [else (sorry! who "unrecognized op ~s" op)])))))))))

  (define asm-flsqrt ;;@ load, calc, store
    (lambda (code* src dest)
      (Trivit (src dest)
              (let ([flreg1 `(reg . ,%flreg1)]
                    [disp (constant flonum-data-disp)])
                (emit fld.d flreg1 src disp
                      (emit fsqrt.d flreg1 flreg1
                            (emit fst.d flreg1 dest disp code*)))))))

  (define asm-trunc ;;@ flonum to fixnum
    (lambda (code* dest src)
      (Trivit (dest src)
              (let ([flreg1 `(reg . ,%flreg1)]
                    [disp (constant flonum-data-disp)])
                (emit fld.d flreg1 src disp
                      (emit ffint.l.d dest flreg1 code*))))))

  (define asm-flt ;;@ fixnum to flonum
    (lambda (code* src dest)
      (Trivit (src dest) ;;@ todo flonumreg holds addr?
              (let ([flreg1 `(reg . ,%flreg1)]
                    [disp (constant flonum-data-disp)])
                (emit ffint.d.l flreg1 src
                      (emit fst.d flreg1 dest disp code*))))))

  (define-who asm-swap ;;@ big <-> little-endian
    (lambda (type)
      (rec asm-swap-internal
           (lambda (code* dest src)
             (Trivit (dest src)
                     (let ([zero `(reg . ,%real-zero)])
                       (case type ;;@ rethink sign-extension stuff
                         [(integer-16) (emit revb.2h dest src code*)]
                         [(unsigned-16) (emit revb.2h dest src
                                              (emit andi dest dest #xFFFF code*))] ; since revb.2h has sign-extension
                         [(integer-32) (emit revb.2w dest src
                                             (emit slli.d dest dest 32
                                                   (emit srai.d dest dest 32 code*)))]
                         [(unsigned-32) (emit revb.2w dest src
                                              (emit slli.d dest dest 32
                                                    (emit srli.d dest dest 32 code*)))]
                         [(integer-64 unsigned-64) (emit revb.d dest src code*)]
                         [else (sorry! who "unexpected asm-swap type argument ~s" type)])))))))

  (define asm-lock ;;@ check operands of sc.d, see if can be used in both places
                                        ;    ll.d tmp, [src]
                                        ;    bnez  tmp,  L
                                        ;    addi.d tmp, %real-zero, 1
                                        ;    sc.d tmp, tmp, [src]
                                        ;L:

    (lambda (code* src tmp)
      (Trivit (src tmp)
              (emit ll.d tmp src 0
                    (emit bnez tmp 12
                          (emit addi.d tmp `(reg . ,%real-zero) 1
                                (emit sc.d tmp src 0 code*))))))) ;;@todo sc.d semantics

  (define-who asm-lock+/-
                                        ;S:
                                        ;    ll.d tmp1, [src]
                                        ;    addi.d tmp1, tmp1, +/-1
                                        ;    sc.d tmp2, tmp1, [src]
                                        ;    bnez  tmp2, S[-12]
                                        ;    sltui %cond tmp1 1 # set %cond if tmp1=0
    (lambda (op)
      (lambda (code* src tmp1 tmp2)
        (Trivit (src tmp1 tmp2)
                (emit ll.d tmp1 src 0
                      (let ([code* (emit sc.d tmp1 src 0
                                         (emit bnez tmp2 -12
                                               (emit sltui `(reg . ,%cond) tmp1 1 code*)))])
                        (case op
                          [(locked-incr!) (emit addi.d tmp1 tmp1 1 code*)]
                          [(locked-decr!) (emit addi.d tmp1 tmp1 -1 code*)]
                          [else (sorry! who "unexpected op ~s" op)])))))))

  (define asm-cas
                                        ;cas:
                                        ;   ll.d tmp1, src
                                        ;   bne  tmp1, old, L[12]
                                        ;   sc.d tmp2, new, [src] # tmp2!=0 if store fails
                                        ;   sltui %cond tmp2 1    # %cond=1 if tmp2=0(succeed)
                                        ;L:
    (lambda (code* src old new tmp1 tmp2)
      (Trivit (src old new tmp1 tmp2)
              (emit ll.d tmp1 src 0
                    (emit addi.d tmp2 new 0 ; backup
                          (emit bne tmp1 old 16
                                (emit sc.d new src 0
                                      (emit sltui `(reg . ,%cond) new 1
                                            (emit add.d new tmp2 %real-zero code*)))))))))

  (define-who asm-relop
    (lambda (info)
      (rec asm-relop-internal
           (lambda (l1 l2 offset x y)
             (Trivit (x y) ;;@ todo recheck
                     ;;(unless (and (ax-reg? x)) (sorry! who "unexpected operand(s) ~s" x))
                     (values
                      (let ([type (info-condition-code-type info)]
                            [cond `(reg . ,%cond)])
                        (case type
                          [(eq?) (emit xor cond x y
                                       (emit sltui cond cond 1 '()))]
                          [(u<) (emit sltu cond x y '())]
                          [(<) (emit slt cond x y '())]
                          [(>) (emit slt cond y x '())]
                          [(<=) (emit slt cond y x
                                      (emit xori cond cond 1 '()))]
                          [(>=) (emit slt cond x y
                                      (emit xori cond cond 1 '()))]))
                      (asm-conditional-jump info l1 l2 offset)))))))

  (define-who asm-fl-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
                (values
                 (let ([flreg1 `(reg . ,%flreg1)]
                       [flreg2 `(reg . ,%flreg2)]
                       [cond `(reg . ,%cond)]
                       [disp (constant flonum-data-disp)]
                       [zero `(reg . ,%real-zero)]
                       [op (info-condition-code-type info)])
                   (emit fld.d flreg1 x disp
                         (emit fld.d flreg2 y disp
                               (emit fcmp.d op flreg1 flreg2 ; internally use the first flag reg
                                     (emit bceqz '() 3 ; ditto '(): placeholder
                                           (emit addi.d cond zero 1
                                                 (emit b 2
                                                       (emit addi.d cond zero 0 '()))))))))
                 (asm-conditional-jump info l1 l2 offset))))))

  (define asm-read-performance-monitoring-counter
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit addi.d dest `(reg . ,%real-zero) 0 code*))))

  (define asm-read-time-stamp-counter
    (lambda (code* dest)
      (Trivit (dest)
              (emit rdtime.d dest `(reg . ,%real-zero) code*))))

  (define asm-inc-cc-counter ;; load, add, store back
    (lambda (code* addr val tmp)
      (Trivit (addr val tmp)
              (emit ld.d tmp `(reg . ,%real-zero) addr
                    (emit add.d tmp tmp val
                          (emit st.d tmp `(reg . ,%real-zero) addr code*))))))

  (define asm-enter values)

  (define asm-fence ;;@ ibar? or dbar? or others?
    (lambda (code*)
      (emit ibar code*)))

  (define ax-mov32
    (lambda (dest n code*)
      (let ([upper (upper20 n)]
            [lower (lower12 n)])
        (safe-assert (signed20? upper))
        (emit lu12i.w dest upper
              (emit ori dest dest lower code*)))))
  (define ax-mov64
    (lambda (dest n code*)
      (emit pcaddi dest 0
            (emit ld.d dest dest 12
                  (emit b 3
                        `((quad . ,n) (unquote-splicing code*)))))))

  (define-who asm-move
    (lambda (code* dest src)
                                        ; move pseudo instruction used by set! case in select-instruction
                                        ; guarantees dest is a reg and src is reg, mem, or imm OR dest is
                                        ; mem and src is reg.
      (Trivit (dest src)
              (define (bad!) (sorry! who "unexpected combination of src ~s and dest ~s" src dest))
              (cond
               [(ax-reg? dest)
                (record-case src
                  [(reg) ignore (emit addi.d dest src 0 code*)]
                  [(imm) (n)
                   (if (signed12? n)
                       (emit addi.d dest `(reg . ,%real-zero) n code*)
                       (if (signed32? n)
                           (ax-mov32 dest n code*)
                           (ax-mov64 dest n code*)))]
                  [(literal) stuff
                   (ax-mov64 dest 0
                             (asm-helper-relocation code* (cons 'la64-abs stuff)))]
                  [(disp) (n breg)
                   (safe-assert (signed12? n))
                   (emit ld.d dest breg n code*)]
                  [(index) (n ireg breg) ;;@ todo check this; result usually won't exceed 2^31
                   (safe-assert (eqv? n 0))
                   (emit ldx.d dest breg ireg code*)]
                  [else (bad!)])]
               [(ax-reg? src)
                (record-case dest
                  [(disp) (n breg)
                   (safe-assert (signed12? n))
                   (emit st.d src breg n code*)]
                  [(index) (n ireg breg)
                   (safe-assert (eqv? n 0))
                   (emit stx.d src ireg breg code*)]
                  [else (bad!)])]))))

  (define-who asm-move/extend ;;@ todo check
    (lambda (op)
      (lambda (code* dest src)
        (Trivit (dest src)
                (case op
                  [(sext8) (emit ext.w.b dest src code*)]
                  [(sext16) (emit ext.w.h dest src code*)]
                  [(sext32) (emit addi.w dest src 0 code*)]
                  [(zext8) (emit andi dest src #xff code*)] ; note that andi zero-extends the imm
                  [(zext16) (emit slli.d dest src 48
                                  (emit srli.d dest dest 48 code*))]
                  [(zext32) (emit slli.d dest src 32
                                  (emit srli.d dest dest 32 code*))]
                  [else (sorry! who "unexpected op ~s" op)])))))

  (define asm-save-flrv
    (lambda (code*)
      (let ([sp (cons 'reg %sp)]) ;;@ todo check if offset is aligned
        (emit addi.d sp sp -16
              (emit fst.d %Cfpretval sp 0 code*)))))

  (define asm-restore-flrv
    (lambda (code*)
      (let ([sp (cons 'reg %sp)])
        (emit fld.d %Cfpretval sp 0
              (emit addi.d sp sp 16 code*)))))

  (define asm-condition-code
    (lambda (info)
      (rec asm-check-flag-internal
           (lambda (l1 l2 offset)
             (values '() (asm-conditional-jump info l1 l2 offset))))))

  (define-who asm-return-address ;;@todo offset
    (lambda (dest l incr-offset next-addr)
      (make-rachunk dest l incr-offset next-addr
                    (or (cond ;; next-offset
                         [(local-label-offset l) =>
                          (lambda (offset)
                            (let ([disp (fx- next-addr (fx- offset incr-offset) -8)])
                              (cond
                               [(signed32? disp)
                                (Trivit (dest)
                                        (emit pcaddu12i dest (upper20 disp)
                                              (emit ori dest dest (lower12 disp) '())))]
                               [else #f])))]
                         [else #f])
                        ;;@ (label-ref ...) is processed by (make-funcrel) in (Trivit-rand), that is, into a 'literal form
                        (asm-move '() dest (with-output-language (L16 Triv) `(label-ref ,l ,incr-offset)))))))

  (define-who asm-jump
    (lambda (l next-addr)
      (make-gchunk l next-addr
                   (cond
                    [(local-label-offset l) =>
                     (lambda (offset)
                       (let ([disp (fx- next-addr offset)])
                         (cond
                          [(eqv? disp 0) '()]
                          [(branch-disp? disp) (emit b (+ disp 4) '())] ;;@ todo offset
                          [(signed32? disp) (emit pcaddu12i %jump (upper20 (+ disp 12))
                                                  (emit ori %jump %jump (lower12 (+ disp 12))
                                                        (emit jirl %real-zero %jump 0 '())))]
                          [else (sorry! who "disp value not supported")])))]
                    [else
                                        ; label must be somewhere above.  generate something so that a hard loop
                                        ; doesn't get dropped.  this also has some chance of being the right size
                                        ; for the final branch instruction.
                     (emit pcaddu12i %jump 0
                           (emit ori %jump %jump 0
                                 (emit jirl %real-zero %jump 0 '())))]))))

  ;;@ todo reverse case not handled
  (define asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      (define get-disp
        (lambda (next-addr l)
          (cond
           [(and (local-label? l) (local-label-offset l)) =>
            (lambda (offset)
              (let ([disp (fx- next-addr offset)])
                (safe-assert (signed32? disp))
                disp))] ;;@ not using (label ...) as in others
           [else 0])))
      (make-cgchunk info l1 l2 next-addr
                    (let ( #;[type (info-condition-code-type info)] ; of no use since %cond is set accordingly
                          [disp1 (get-disp next-addr l1)]
                          [disp2 (get-disp next-addr l2)])
                      (cond
                       ;; inverted
                       [(fx= disp1 0)
                        (safe-assert (signed32? disp2))
                        (if (cond-branch-disp? (+ disp2 4))
                            (emit beqz `(reg . ,%cond) (+ disp2 4) '())
                            (emit beqz `(reg . ,%cond) 8
                                  (emit b 4 ;; fall through
                                        (emit pcaddu12i `(reg . ,%jump) (upper20 (+ disp2 12)) ;; 2 instr below
                                              (emit ori `(reg . ,%jump) `(reg . ,%jump) (lower12 (+ disp2 12))
                                                    (emit jirl `(reg . ,%real-zero) `(reg . ,%jump) 0 '()))))))]
                       ;; normal
                       [(fx= disp2 0)
                        (safe-assert (signed32? disp1))
                        (if (cond-branch-disp? (+ disp1 4))
                            (emit bnez `(reg . ,%cond) (+ disp1 4) '())
                            (emit bnez `(reg . ,%cond) 8
                                  (emit b 4 ;; fall through
                                        (emit pcaddu12i `(reg . ,%jump) (upper20 (+ disp1 12)) ;; 2 instr below
                                              (emit ori `(reg . ,%jump) `(reg . ,%jump) (lower12 (+ disp1 12))
                                                    (emit jirl `(reg . ,%real-zero) `(reg . ,%jump) 0 '()))))))]
                       ;; others
                       [else
                        (safe-assert (signed32? (+ disp1 24))
                                     (signed32? (+ disp2 12)))
                        (emit bnez `(reg . ,%cond) 8
                              (emit b 4
                                    (emit pcaddu12i `(reg . ,%jump) (upper20 (+ disp1 24))
                                          (emit ori `(reg . ,%jump) `(reg . ,%jump) (lower12 (+ disp1 24))
                                                (emit jirl `(reg . ,%real-zero) `(reg . ,%jump) 0
                                                      (emit pcaddu12i `(reg . ,%jump) (upper20 (+ disp2 12))
                                                            (emit ori `(reg . ,%jump) `(reg . ,%jump) (lower12 (+ disp2 12))
                                                                  (emit jirl `(reg . ,%real-zero) `(reg . ,%jump) 0 '()))))))))])))))

  (define asm-direct-jump
    (lambda (l offset)
      (asm-helper-jump '() (make-funcrel 'la64-jump l offset))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
                       `(la64-jump ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info))))))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
                       `(la64-jump ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec save-ra?)
      (let ([target `(la64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
             (lambda (code* dest tmp . ignore) ;; retval setup by intrinsics
               (asm-helper-call code* target save-ra? tmp))))))

  (define asm-library-call!
    (lambda (libspec save-ra?)
      (let ([target `(la64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
             (lambda (code* tmp . ignore)
               (asm-helper-call code* target save-ra? tmp))))))

  ;; for things like split-and-resize, handle-apply-overflood, Sreturn, foreign-entry
  ;; where no retval is needed
  (define asm-c-simple-call
    (lambda (entry save-ra?)
      (let ([target `(la64-call 0 (entry ,entry))])
        (rec asm-c-simple-call-internal
             (lambda (code* tmp . ignore)
               (asm-helper-call code* target save-ra? tmp))))))

  (define-who asm-indirect-call
    (lambda (code* dest . ignore)
      (Trivit (dest)
              (unless (ax-reg? dest) (sorry! who "unexpected dest ~s" dest))
              (emit jirl %ra dest 0 code*))))

  (define-who asm-indirect-jump ;; no link
    (lambda (src)
      (Trivit (src)
              (record-case src
                [(reg) ignore (emit jirl `(reg . ,%real-zero) `(reg . ,src) 0 '())]
                ;; the following two load EA from memory first
                [(disp) (n breg)
                 (safe-assert (signed12? n))
                 (emit ld.d `(reg . ,%jump) breg n
                       (emit jirl `(reg . ,%real-zero) `(reg . ,%jump) 0 '()))]
                [(index) (n ireg breg)
                 (safe-assert (eqv? n 0))
                 (emit ldx.d `(reg . ,%jump) ireg breg
                       (emit jirl `(reg . ,%real-zero) `(reg . ,%jump) 0 '()))]
                [else (sorry! who "unexpected src ~s" src)]))))

  (define asm-get-tc
    (let ([target `(la64-call 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* dest tmp . ignore) ;; retval is put into %Cretval automatically
        (asm-helper-call code* target #f tmp))))

  (define asm-activate-thread
    (let ([target `(la64-call 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* dest tmp . ignore) ; dest is ignored, since it is always Cretval
        (asm-helper-call code* target #f tmp))))

  (define asm-deactivate-thread
    (let ([target `(la64-call 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code* tmp . ignore)
        (asm-helper-call code* target #f tmp))))

  (define asm-unactivate-thread
    (let ([target `(la64-call 0 (entry ,(lookup-c-entry unactivate-thread)))])
      (lambda (code* tmp . ignore)
        (asm-helper-call code* target #f tmp))))

  (define asm-push ;;@ todo alignment
    (lambda (code* x)
      (Trivit (x)
              (let ([sp `(reg . ,%sp)])
                (emit addi.d sp sp -16
                      (emit st.d x sp 0 code*))))))

  (define asm-pop
    (lambda (code* dest)
      (Trivit (dest)
              (let ([sp `(reg . ,%sp)])
                (emit ld.d dest sp 0
                      (emit addi.d sp sp 16 code*))))))

  (define asm-return
    (lambda ()
      (emit jirl `(reg . ,%real-zero) `(reg . ,%ra) 0 '())))

  (define asm-c-return
    (lambda (info)
      (emit jirl `(reg . ,%real-zero) `(reg . ,%ra)  0 '())))

  (define asm-helper-jump ;; no need to save ra
    (lambda (code* reloc) ;;@ todo need saving %cond?
      (let ([zero `(reg . ,%real-zero)]
            [sp `(reg . ,%sp)]
            [jump `(reg . ,%jump)])
        (emit pcaddi jump 0
              (emit ld.d jump jump 12
                    (emit b 3
                          `((quad . 0)
                            (unquote-splicing (emit jirl zero jump 0
                                                    (asm-helper-relocation code* reloc))))))))))

  (define asm-helper-call ;; need to save ra
    (lambda (code* reloc save-ra? tmp)
      (define maybe-save-ra
        (lambda (code* p)
          (if save-ra?
              (let ([sp `(reg . ,%sp)]
                    [ra `(reg . ,%ra)])
                (emit addi.d sp sp -16 ;; save %ra on the stack
                      (emit st.d ra sp 0
                            (p (emit ld.d ra sp 0
                                     (emit addi.d sp sp 16 code*))))))
              (p code*))))
      (let ([zero `(reg . ,%real-zero)])
        (maybe-save-ra code*
                       (lambda (code*)
                         (emit pcaddi tmp 0
                               (emit ld.d tmp tmp 12
                                     (emit b 3
                                           `((quad . 0)
                                             (unquote-splicing (emit jirl %ra tmp 0
                                                               (asm-helper-relocation code* reloc))))))))))))

  (define asm-kill
    (lambda (code* dest)
      code*))

  (define asm-helper-relocation
    (lambda (code* reloc)
      (cons* reloc (aop-cons* `(asm "relocation:" ,reloc) code*))))

  (define asm-data-label
    (lambda (code* l offset func code-size)
      (let ([rel (make-funcrel 'abs l offset)])
        (cons* rel (aop-cons* `(asm "mrv point:" ,rel) code*)))))

  (define asm-rp-header
    (let ([mrv-error `(abs ,(constant code-data-disp)
                           (library-code ,(lookup-libspec values-error)))])
      (lambda (code* mrvl fs lpm func code-size)
        (cons*
         (if (target-fixnum? lpm)
             `(quad . ,(fix lpm)) ;; 64-bit
             `(abs 0 (object ,lpm)))
         (aop-cons* `(asm livemask: ,(format "~b" lpm))
                    '(code-top-link)
                    (aop-cons* `(asm code-top-link)
                               `(quad . ,fs)
                               (aop-cons* `(asm "frame size:" ,fs)
                                          (if mrvl
                                              (asm-data-label code* mrvl 0 func code-size)
                                              (cons*
                                               mrv-error
                                               (aop-cons* `(asm "mrv point:" ,mrv-error)
                                                          code*))))))))))

(module (asm-foreign-call asm-foreign-callable)
    (define make-vint (lambda () (vector %Carg1 %Carg2 %Carg3 %Carg4 %Carg5 %Carg6 %Carg7 %Carg8)))
    (define make-vfp (lambda () (vector %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4 %Cfparg5 %Cfparg6 %Cfparg7 %Cfparg8)))
    (define align
      (lambda (b x)
        (let ([k (- b 1)])
          (fxlogand (fx+ x k) (fxlognot k)))))

    (define (classify-type type)
      (nanopass-case (Ltype Type) type
                     [(fp-ftd& ,ftd) (classify-eightbytes ftd)]
                     [else #f]))

    (define (classified-size type)
      (nanopass-case (Ltype Type) type
                     [(fp-ftd& ,ftd) ($ftd-size ftd)]
                     [else #f]))

    ;; classify-eightbytes: returns '(memory) or a nonemtpy list of 'integer/'float
    ;; Non-Windows: SYSV ABI is a more general classification of
    ;; 8-byte segments into 'integer, 'float, or 'memory modes
    (define (classify-eightbytes ftd)
	  (define (merge t1 t2)
	    (cond
	     [(eq? t1 t2) t1]
	     [(eq? t1 'no-class) t2]
	     [(eq? t2 'no-class) t1]
	     [(eq? t1 'memory) 'memory]
	     [(eq? t2 'memory) 'memory]
	     [else 'integer]))
	  (cond
	   [(or (> ($ftd-size ftd) 16) ; more than 2 eightbytes => passed in memory
	        (fx= 0 ($ftd-size ftd)))
	    '(memory)]
	   [else
	    (let ([classes (make-vector (fxsrl (align ($ftd-size ftd) 8) 3) 'no-class)])
	      (let loop ([mbrs ($ftd->members ftd)])
	        (cond
	         [(null? mbrs)
		      (vector->list classes)]
	         [else
		      (let ([kind (caar mbrs)]
		            [size (cadar mbrs)]
		            [offset (caddar mbrs)])
		        (cond
		         [(not (fx= offset (align offset size)))
		          ;; misaligned
		          '(memory)]
		         [else
		          (let* ([pos (fxsrl offset 3)]
			             [class (vector-ref classes pos)]
			             [new-class (merge class (if (eq? kind 'float) 'float 'integer))])
		            (cond
		             [(eq? new-class 'memory)
			          '(memory)]
		             [else
			          (vector-set! classes pos new-class)
			          (loop (cdr mbrs))]))]))])))]))

    (define (count v l)
      (cond
       [(null? l) 0]
       [(eq? (car l) v) (fx+ 1 (count v (cdr l)))]
       [else (count v (cdr l))]))

    (define (result-fits-in-registers? result-classes)
      (and result-classes
           (not (eq? 'memory (car result-classes)))
           (or (null? (cdr result-classes))
               (null? (cddr result-classes)))))

    ;; An argument is put in registeres depending on how many
    ;; registers are left
    (define (pass-here-by-stack? classes iint ints ifp fps)
      (or (eq? 'memory (car classes))
          (fx> (fx+ iint ints) 8)
          (fx> (fx+ ifp fps) 8)))


    (module (push-registers pop-registers push-registers-size)
      (define (move-registers regs load?)
        (define vfp (make-vfp))
        (define (fp-reg? reg)
          (let loop ([i (fx- (vector-length vfp) 1)])
            (or (eq? reg (vector-ref vfp i))
                (and (fx> i 0) (loop (fx- i 1))))))
        (with-output-language (L13 Effect)
                              (let loop ([regs regs] [offset 0])
                                (let* ([reg (car regs)]
                                       [e (cond
                                           [(fp-reg? reg)
                                            `(inline ,(make-info-loadfl reg) ,(if load? %load-double %store-double) ,%sp ,%real-zero (immediate ,offset))]
                                           [load? `(set! ,reg ,(%mref ,%sp ,offset))]
                                           [else `(set! ,(%mref ,%sp ,offset) ,reg)])]
                                       [regs (cdr regs)])
                                  (if (null? regs)
                                      e
                                      `(seq ,e ,(loop regs (fx+ offset 8))))))))
      (define (push-registers-size regs)
        (align (fx* 8 (length regs)) 16))
      (define (push-registers regs)
        (with-output-language (L13 Effect)
                              (%seq
                               (set! ,%sp ,(%inline - ,%sp (immediate ,(push-registers-size regs))))
                               ,(move-registers regs #f))))
      (define (pop-registers regs)
        (with-output-language (L13 Effect)
                              (%seq
                               ,(move-registers regs #t)
                               (set! ,%sp ,(%inline + ,%sp (immediate ,(push-registers-size regs))))))))


    (define asm-foreign-call
      (with-output-language (L13 Effect)
                            (letrec ([load-double-stack ;;@ calling others, get the flonums from memory and put them onto the stack
                                      (lambda (offset)
                                        (lambda (x) ; requires var
                                          (%seq
                                           (inline ,(make-info-loadfl %flreg1) ,%load-double ,x ,%real-zero ,(%constant flonum-data-disp))
                                           (inline ,(make-info-loadfl %flreg1) ,%store-double ,%sp ,%real-zero (immediate ,offset)))))]
                                     [load-single-stack
                                      (lambda (offset)
                                        (lambda (x) ; requires var
                                          (%seq
                                           (inline ,(make-info-loadfl %flreg1) ,%load-double->single ,x ,%real-zero ,(%constant flonum-data-disp))
                                           (inline ,(make-info-loadfl %flreg1) ,%store-single ,%sp ,%real-zero (immediate ,offset)))))]
                                     [load-int-stack
                                      (lambda (offset)
                                        (lambda (rhs) ; requires rhs
                                          `(set! ,(%mref ,%sp ,offset) ,rhs)))]
                                     [load-double-reg
                                      (lambda (fpreg)
                                        (lambda (x) ; requires var
                                          `(inline ,(make-info-loadfl fpreg) ,%load-double ,x ,%real-zero ,(%constant flonum-data-disp))))]
                                     [load-single-reg
                                      (lambda (fpreg)
                                        (lambda (x) ; requires var
                                          `(inline ,(make-info-loadfl fpreg) ,%load-double->single ,x ,%real-zero ,(%constant flonum-data-disp))))]
                                     [load-int-reg
                                      (lambda (ireg)
                                        (lambda (x)
                                          `(set! ,ireg ,x)))]
                                     [load-content-stack
                                      (lambda (offset len)
                                        (lambda (x) ; requires var
                                          (let loop ([offset offset] [x-offset 0] [len len])
                                            (cond
                                             [(= len 0) `(nop)]
                                             [(>= len 8)
                                              `(seq
                                                (set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-64 #f)
                                                                                    ,%load ,x ,%real-zero (immediate ,x-offset)))
                                                ,(loop (fx+ offset 8) (fx+ x-offset 8) (fx- len 8)))]
                                             [(>= len 4)
                                              `(seq
                                                (set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-32 #f)
                                                                                    ,%load ,x ,%real-zero (immediate ,x-offset)))
                                                ,(loop (fx+ offset 4) (fx+ x-offset 4) (fx- len 4)))]
                                             [(>= len 2)
                                              `(seq
                                                (set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-16 #f)
                                                                                    ,%load ,x ,%real-zero (immediate ,x-offset)))
                                                ,(loop (fx+ offset 2) (fx+ x-offset 2) (fx- len 2)))]
                                             [else
                                              `(set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-8 #f)
                                                                                   ,%load ,x ,%real-zero (immediate ,x-offset)))]))))]
                                     [load-content-regs
                                      (lambda (classes size iint ifp vint vfp)
                                        (lambda (x) ; requires var
                                          (let loop ([size size] [iint iint] [ifp ifp] [classes classes] [x-offset 0])
                                            (cond
                                             [(null? classes) `(nop)]
                                             [(eq? 'float (car classes))
                                              (cond
                                               [(fx= size 4)
                                                ;; Must be the last element
                                                `(inline ,(make-info-loadfl (vector-ref vfp ifp)) ,%load-single ,x ,%real-zero (immediate ,x-offset))]
                                               [else
                                                `(seq
                                                  (inline ,(make-info-loadfl (vector-ref vfp ifp)) ,%load-double ,x ,%real-zero (immediate ,x-offset))
                                                  ,(loop (fx- size 8) iint (fx+ ifp 1) (cdr classes) (fx+ x-offset 8)))])]
                                             ;; Remaining cases are integers:
                                             [(>= size 8)
                                              `(seq
                                                (set! ,(vector-ref vint iint) (inline ,(make-info-load 'integer-64 #f)
                                                                                      ,%load ,x ,%real-zero (immediate ,x-offset)))
                                                ,(loop (fx- size 8) (fx+ iint 1) ifp (cdr classes) (fx+ x-offset 8)))]
                                             ;; Remaining cases must be the last element
                                             [else
                                              (let loop ([reg (vector-ref vint iint)] [size size] [x-offset x-offset])
                                                (cond
                                                 [(= size 4)
                                                  `(set! ,reg (inline ,(make-info-load 'unsigned-32 #f)
                                                                      ,%load ,x ,%real-zero (immediate ,x-offset)))]
                                                 [(= size 2)
                                                  `(set! ,reg (inline ,(make-info-load 'unsigned-16 #f)
                                                                      ,%load ,x ,%real-zero (immediate ,x-offset)))]
                                                 [(= size 1)
                                                  `(set! ,reg (inline ,(make-info-load 'unsigned-8 #f)
                                                                      ,%load ,x ,%real-zero (immediate ,x-offset)))]
                                                 [(> size 4)
                                                  ;; 5, 6, or 7: multiple steps to avoid reading too many bytes
                                                  (let ([tmp %scratch]) ;; ?? ok to use %r31?
                                                    (%seq
                                                     ,(loop reg (fx- size 4) (fx+ x-offset 4))
                                                     (set! ,reg ,(%inline sll ,reg (immediate 32)))
                                                     ,(loop tmp 4 x-offset)
                                                     (set! ,reg ,(%inline + ,reg ,tmp))))]
                                                 [else
                                                  ;; 3: multiple steps to avoid reading too many bytes
                                                  (let ([tmp %scratch]) ;; ?? ok to use %r31?
                                                    (%seq
                                                     ,(loop reg (fx- size 2) (fx+ x-offset 2))
                                                     (set! ,reg ,(%inline sll ,reg (immediate 16)))
                                                     ,(loop tmp 2 x-offset)
                                                     (set! ,reg ,(%inline + ,reg ,tmp))))]))]))))]
                                     [add-regs
                                      (lambda (ints ir vr regs)
                                        (cond
                                         [(fx= 0 ints) regs]
                                         [else
                                          (add-regs (fx- ints 1) (fx+ ir 1) vr
                                                    (cons (vector-ref vr ir) regs))]))]
                                     [do-args
                                      (lambda (types vint vfp)
                                        (let loop ([types types] [locs '()] [regs '()] [fp-regs '()] [iint 0] [ifp 0] [isp 0])
                                          (if (null? types)
                                              (values isp ifp locs regs fp-regs)
                                              (nanopass-case (Ltype Type) (car types)
                                                             [(fp-double-float)
                                                              (if (< ifp 8)
                                                                  (loop (cdr types)
                                                                        (cons (load-double-reg (vector-ref vfp ifp)) locs)
                                                                        regs (cons (vector-ref vfp ifp) fp-regs) iint (fx+ ifp 1) isp)
                                                                  (loop (cdr types)
                                                                        (cons (load-double-stack isp) locs)
                                                                        regs fp-regs iint ifp (fx+ isp 8)))]
                                                             [(fp-single-float)
                                                              (if (< ifp 8)
                                                                  (loop (cdr types)
                                                                        (cons (load-single-reg (vector-ref vfp ifp)) locs)
                                                                        regs (cons (vector-ref vfp ifp) fp-regs) iint (fx+ ifp 1) isp)
                                                                  (loop (cdr types)
                                                                        (cons (load-single-stack isp) locs)
                                                                        regs fp-regs iint ifp (fx+ isp 8)))]
                                                             [(fp-ftd& ,ftd)
                                                              (let* ([classes (classify-eightbytes ftd)]
                                                                     [ints (count 'integer classes)]
                                                                     [fps (count 'float classes)])
                                                                (cond
                                                                 [(pass-here-by-stack? classes iint ints ifp fps)
                                                                  ;; pass on the stack
                                                                  (loop (cdr types)
                                                                        (cons (load-content-stack isp ($ftd-size ftd)) locs)
                                                                        regs fp-regs iint ifp (fx+ isp (align ($ftd-size ftd) 8)))]
                                                                 [else
                                                                  ;; pass in registers
                                                                  (loop (cdr types)
                                                                        (cons (load-content-regs classes ($ftd-size ftd) iint ifp vint vfp) locs)
                                                                        (add-regs ints iint vint regs) (add-regs fps ifp vfp fp-regs)
                                                                        (fx+ iint ints) (fx+ ifp fps) isp)]))]
                                                             [else
                                                              (if (< iint 8)
                                                                  (let ([reg (vector-ref vint iint)])
                                                                    (loop (cdr types)
                                                                          (cons (load-int-reg reg) locs)
                                                                          (cons reg regs) fp-regs
                                                                          (fx+ iint 1) ifp isp))
                                                                  (loop (cdr types)
                                                                        (cons (load-int-stack isp) locs)
                                                                        regs fp-regs iint ifp (fx+ isp 8)))]))))])
                              (define (add-deactivate adjust-active? t0 live* result-live* e)
                                (cond
                                 [adjust-active?
                                  (let ([save-and-restore
                                         (lambda (regs e)
                                           (cond
                                            [(null? regs) e]
                                            [else (%seq ,(push-registers regs) ,e ,(pop-registers regs))]))])
                                    (%seq
                                     (set! ,%deact ,t0)
                                     ,(save-and-restore (cons %deact live*) (%inline deactivate-thread))
                                     ,e
                                     ,(save-and-restore result-live* `(set! ,%Cretval ,(%inline activate-thread)))))]
                                 [else e]))
                              (define (add-save-fill-target fill-result-here? frame-size locs)
                                (cond
                                 [fill-result-here?
                                  ;; The callee isn't expecting a pointer to fill with the result.
                                  ;; Stash the pointer as an extra argument, and then when the
                                  ;; function returns, we'll move register content for the result
                                  ;; into the pointer's target
                                  (values (fx+ frame-size (constant ptr-bytes))
                                          (append locs
                                                  (list
                                                   (lambda (x) ; requires var
                                                     `(set! ,(%mref ,%sp ,frame-size) ,x)))))]
                                 [else
                                  (values frame-size locs)]))
                              (define (add-fill-result c-call saved-offset classes size)
                                (let loop ([classes classes] [offset 0] [iregs (reg-list %Carg1 %Carg2)] [fpregs (reg-list %Cfparg1 %Cfparg2)] [size size])
                                  (cond
                                   [(null? classes)
                                    `(seq
                                      ,c-call
                                      (set! ,%cond ,(%mref ,%sp ,saved-offset)))] ; use a random caller-saved reg
                                   [(eq? 'float (car classes))
                                    `(seq
                                      ,(loop (cdr classes) (fx+ offset 8) iregs (cdr fpregs) (fx- size 8))
                                      ,(case size
                                         [(4) `(inline ,(make-info-loadfl (car fpregs)) ,%store-single ,%cond ,%real-zero (immediate ,offset))]
                                         [else `(inline ,(make-info-loadfl (car fpregs)) ,%store-double ,%cond ,%real-zero (immediate ,offset))]))]
                                   [else
                                    `(seq
                                      ,(loop (cdr classes) (fx+ offset 8) (cdr iregs) fpregs (fx- size 8))
                                      ,(let ([ireg (car iregs)])
                                         (case size
                                           ;; still use %cond for the job
                                           [(1) `(inline ,(make-info-load 'integer-8 #f) ,%store
                                                         ,%cond ,%real-zero (immediate ,offset) ,ireg)]
                                           [(2) `(inline ,(make-info-load 'integer-16 #f) ,%store
                                                         ,%cond ,%real-zero (immediate ,offset) ,ireg)]
                                           [(3) (%seq
                                                 (inline ,(make-info-load 'integer-16 #f) ,%store
                                                         ,%cond ,%real-zero (immediate ,offset) ,ireg)
                                                 (set! ,ireg ,(%inline srl ,ireg (immediate 16)))
                                                 (inline ,(make-info-load 'integer-8 #f) ,%store
                                                         ,%cond ,%real-zero (immediate ,(fx+ 2 offset)) ,ireg))]
                                           [(4) `(inline ,(make-info-load 'integer-32 #f) ,%store
                                                         ,%cond ,%real-zero (immediate ,offset) ,ireg)]
                                           [(5 6 7) (%seq
                                                     (inline ,(make-info-load 'integer-32 #f) ,%store
                                                             ,%cond ,%real-zero (immediate ,offset) ,ireg)
                                                     (set! ,ireg ,(%inline srl ,ireg (immediate 32)))
                                                     ,(case size
                                                        [(5)
                                                         `(inline ,(make-info-load 'integer-8 #f) ,%store
                                                                  ,%cond ,%real-zero (immediate ,(fx+ 4 offset)) ,ireg)]
                                                        [(6)
                                                         `(inline ,(make-info-load 'integer-16 #f) ,%store
                                                                  ,%cond ,%real-zero (immediate ,(fx+ 4 offset)) ,ireg)]
                                                        [(7)
                                                         (%seq
                                                          (inline ,(make-info-load 'integer-16 #f) ,%store
                                                                  ,%cond ,%real-zero (immediate ,(fx+ 4 offset)) ,ireg)
                                                          (set! ,ireg ,(%inline srl ,ireg (immediate 16)))
                                                          (inline ,(make-info-load 'integer-8 #f) ,%store
                                                                  ,%cond ,%real-zero (immediate ,(fx+ 6 offset)) ,ireg))]))]
                                           [else `(set! ,(%mref ,%cond ,offset) ,ireg)])))])))
                              (define (get-result-regs fill-result-here? result-type result-classes)
                                (if fill-result-here?
                                    (let loop ([classes result-classes] [iregs (reg-list %r10 %r11)] [fpregs (reg-list %Cfparg1 %Cfparg2)])
                                      (cond
                                       [(null? classes) '()]
                                       [(eq? 'float (car classes))
                                        (cons (car fpregs) (loop (cdr classes) iregs (cdr fpregs)))]
                                       [else
                                        (cons (car iregs) (loop (cdr classes) (cdr iregs) fpregs))]))
                                    (nanopass-case (Ltype Type) result-type
                                                   [(fp-double-float) (list %Cfpretval)]
                                                   [(fp-single-float) (list %Cfpretval)]
                                                   [(fp-void) '()]
                                                   [else (list %Carg1)])))
                              (define returnem
                                (lambda (frame-size locs ccall r-loc)
                                        ; need to maintain 16-byte alignment, ignoring the return address
                                        ; pushed by call instruction, which counts as part of callee's frame
                                        ; tc is callee-save; no need to save
                                  (let ([frame-size (align 16 frame-size)])
                                    (values (lambda ()
                                              (if (fx= frame-size 0)
                                                  `(nop)
                                                  `(set! ,%sp ,(%inline - ,%sp (immediate ,frame-size)))))
                                            (reverse locs)
                                            ccall
                                            r-loc
                                            (lambda ()
                                              (if (fx= frame-size 0)
                                                  `(nop)
                                                  `(set! ,%sp ,(%inline + ,%sp (immediate ,frame-size)))))))))
                              (lambda (info)
                                (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
                                (let* ([conv* (info-foreign-conv* info)]
                                       [arg-type* (info-foreign-arg-type* info)]
                                       [result-type (info-foreign-result-type info)]
                                       [result-classes (classify-type result-type)]
                                       [result-size (classified-size result-type)]
                                       [fill-result-here? (result-fits-in-registers? result-classes)]
                                       [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)])
                                  (with-values (do-args (if fill-result-here? (cdr arg-type*) arg-type*) (make-vint) (make-vfp))
                                    (lambda (frame-size nfp locs live* fp-live*)
                                      (with-values (add-save-fill-target fill-result-here? frame-size locs)
                                        (lambda (frame-size locs)
                                          (returnem frame-size locs
                                                    (lambda (t0)
                                                      (let* ([t (if adjust-active? %deact t0)] ; need a register if `adjust-active?`
                                                             [c-call
                                                              (add-deactivate adjust-active?
                                                                              t0
                                                                              (append fp-live* live*)
                                                                              (get-result-regs fill-result-here? result-type result-classes)
                                                                              `(inline ,(make-info-kill*-live* (reg-list %Cretval) live*) ,%c-call ,t))]) ;;@ todo
                                                        (cond
                                                         [fill-result-here?
                                                          (add-fill-result c-call (fx- frame-size (constant ptr-bytes)) result-classes result-size)]
                                                         [else c-call])))
                                                    (nanopass-case (Ltype Type) result-type
                                                                   [(fp-double-float)
                                                                    (lambda (lvalue)
                                                                      `(inline ,(make-info-loadfl %Cfpretval) ,%store-double ,lvalue ,%real-zero
                                                                               ,(%constant flonum-data-disp)))]
                                                                   [(fp-single-float)
                                                                    (lambda (lvalue)
                                                                      `(inline ,(make-info-loadfl %Cfpretval) ,%store-single->double ,lvalue ,%real-zero
                                                                               ,(%constant flonum-data-disp)))]
                                                                   [(fp-integer ,bits)
                                                                    (case bits
                                                                      [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline sext8 ,%Cretval)))]
                                                                      [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline sext16 ,%Cretval)))]
                                                                      [(32) (lambda (lvalue) `(set! ,lvalue ,(%inline sext32 ,%Cretval)))]
                                                                      [(64) (lambda (lvalue) `(set! ,lvalue ,%Cretval))]
                                                                      [else ($oops 'assembler-internal
                                                                                   "unexpected asm-foreign-procedures fp-integer size ~s"
                                                                                   bits)])]
                                                                   [(fp-unsigned ,bits)
                                                                    (case bits
                                                                      [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline zext8 ,%Cretval)))]
                                                                      [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline zext16 ,%Cretval)))]
                                                                      [(32) (lambda (lvalue) `(set! ,lvalue ,(%inline zext32 ,%Cretval)))]
                                                                      [(64) (lambda (lvalue) `(set! ,lvalue ,%Cretval))]
                                                                      [else ($oops 'assembler-internal
                                                                                   "unexpected asm-foreign-procedures fp-unsigned size ~s"
                                                                                   bits)])]
                                                                   [else (lambda (lvalue) `(set! ,lvalue ,%Cretval))])))))))))))

    #|
    stack layout:                       ; ; ; ; ;
    sp+256: incoming stack args         ; ; ; ; ;
    sp+248: active state   8            ; ; ; ; ;
    sp+240: &-return space 8            ; ; ; ; ;
    sp+112: saved reg args(int and float) x10-x17, f10-f17 8*16=128 ; ; ; ; ;
    sp+0:   callee-saved regs: ra, x2, x8, x9, x18-x27     8*14=112 ;;@ todo need to save fp regs? f8-f9 f18-f27 ; ; ; ; ;
    |#


    (define asm-foreign-callable
      (with-output-language (L13 Effect)
                            (let ()
                              (define load-double-stack
                                (lambda (offset)
                                  (lambda (x) ; requires var
                                    (%seq
                                     (inline ,(make-info-loadfl %flreg1) ,%load-double ,%sp ,%real-zero (immediate ,offset))
                                     (inline ,(make-info-loadfl %flreg1) ,%store-double ,x ,%real-zero ,(%constant flonum-data-disp))))))
                              (define load-single-stack
                                (lambda (offset)
                                  (lambda (x) ; requires var
                                    (%seq
                                     (inline ,(make-info-loadfl %flreg1) ,%load-single->double ,%sp ,%real-zero (immediate ,offset))
                                     (inline ,(make-info-loadfl %flreg1) ,%store-double ,x ,%real-zero ,(%constant flonum-data-disp))))))
                              (define load-int-stack
                                (lambda (type offset)
                                  (lambda (lvalue)
                                    (nanopass-case (Ltype Type) type
                                                   [(fp-integer ,bits)
                                                    (case bits
                                                      [(8) `(set! ,lvalue (inline ,(make-info-load 'integer-8 #f) ,%load ,%sp ,%real-zero (immediate ,offset)))]
                                                      [(16) `(set! ,lvalue (inline ,(make-info-load 'integer-16 #f) ,%load ,%sp ,%real-zero (immediate ,offset)))]
                                                      [(32) `(set! ,lvalue (inline ,(make-info-load 'integer-32 #f) ,%load ,%sp ,%real-zero (immediate ,offset)))]
                                                      [(64) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                                                      [else ($oops 'assembler-internal
                                                                   "unexpected load-int-stack fp-integer size ~s"
                                                                   bits)])]
                                                   [(fp-unsigned ,bits)
                                                    (case bits
                                                      [(8) `(set! ,lvalue (inline ,(make-info-load 'unsigned-8 #f) ,%load ,%sp ,%real-zero (immediate ,offset)))]
                                                      [(16) `(set! ,lvalue (inline ,(make-info-load 'unsigned-16 #f) ,%load ,%sp ,%real-zero (immediate ,offset)))]
                                                      [(32) `(set! ,lvalue (inline ,(make-info-load 'unsigned-32 #f) ,%load ,%sp ,%real-zero (immediate ,offset)))]
                                                      [(64) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                                                      [else ($oops 'assembler-internal
                                                                   "unexpected load-int-stack fp-unsigned size ~s"
                                                                   bits)])]
                                                   [else `(set! ,lvalue ,(%mref ,%sp ,offset))]))))
                              (define load-stack-address
                                (lambda (offset)
                                  (lambda (lvalue) ; requires lvalue
                                    `(set! ,lvalue ,(%inline + ,%sp (immediate ,offset))))))
                              (define save-arg-regs
                                (lambda (types)
                                  (define vint (make-vint))
                                  (define vfp (make-vfp))
                                  (let f ([types types] [iint 0] [ifp 0] [isp 112])
                                    (if (or (null? types) (and (fx>= iint 8) (fx>= ifp 8)))
                                        `(nop)
                                        (nanopass-case (Ltype Type) (car types)
                                                       [(fp-double-float)
                                                        (if (< ifp 8)
                                                            (%seq
                                                             (inline ,(make-info-loadfl (vector-ref vfp ifp)) ,%store-double
                                                                     ,%sp ,%real-zero (immediate ,isp))
                                                             ,(f (cdr types) iint (fx+ ifp 1) (fx+ isp 8)))
                                                            (f (cdr types) iint ifp isp))]
                                                       [(fp-single-float)
                                                        (if (< ifp 8)
                                                            (%seq
                                                             (inline ,(make-info-loadfl (vector-ref vfp ifp)) ,%store-single
                                                                     ,%sp ,%real-zero (immediate ,isp))
                                                             ,(f (cdr types) iint (fx+ ifp 1) (fx+ isp 8)))
                                                            (f (cdr types) iint ifp isp))]
                                                       [(fp-ftd& ,ftd)
                                                        (let* ([classes (classify-eightbytes ftd)]
                                                               [ints (count 'integer classes)]
                                                               [fps (count 'float classes)])
                                                          (cond
                                                           [(pass-here-by-stack? classes iint ints ifp fps)
                                                            ;; receive on the stack
                                                            (f (cdr types) iint ifp isp)]
                                                           [else
                                                            ;; receive via registers
                                                            (let reg-loop ([classes classes] [iint iint] [ifp ifp] [isp isp])
                                                              (cond
                                                               [(null? classes)
                                                                (f (cdr types) iint ifp isp)]
                                                               [(eq? (car classes) 'float)
                                                                `(seq
                                                                  (inline ,(make-info-loadfl (vector-ref vfp ifp)) ,%store-double
                                                                          ,%sp ,%real-zero (immediate ,isp))
                                                                  ,(reg-loop (cdr classes) iint (fx+ ifp 1) (+ isp 8)))]
                                                               [else
                                                                `(seq
                                                                  (set! ,(%mref ,%sp ,isp) ,(vector-ref vint iint))
                                                                  ,(reg-loop (cdr classes) (fx+ iint 1) ifp (+ isp 8)))]))]))]
                                                       [else
                                                        (if (< iint 8)
                                                            (%seq
                                                             (set! ,(%mref ,%sp ,isp) ,(vector-ref vint iint))
                                                             ,(f (cdr types) (fx+ iint 1) ifp (fx+ isp 8)))
                                                            (f (cdr types) iint ifp isp))])))))
                              (define do-stack
                                (lambda (types adjust-active?)
                                        ; risp is where incoming register args are stored
                                        ; sisp is where incoming stack args are stored
                                  (let f ([types types]
                                          [locs '()]
                                          [iint 0]
                                          [ifp 0]
                                          [risp 112]
                                          [sisp 256])
                                    (if (null? types)
                                        locs
                                        (nanopass-case (Ltype Type) (car types)
                                                       [(fp-double-float)
                                                        (if (= ifp 8)
                                                            (f (cdr types)
                                                               (cons (load-double-stack sisp) locs)
                                                               iint ifp risp (fx+ sisp 8))
                                                            (f (cdr types)
                                                               (cons (load-double-stack risp) locs)
                                                               iint (fx+ ifp 1) (fx+ risp 8) sisp))]
                                                       [(fp-single-float)
                                                        (if (= ifp 8)
                                                            (f (cdr types)
                                                               (cons (load-single-stack sisp) locs)
                                                               iint ifp risp (fx+ sisp 8))
                                                            (f (cdr types)
                                                               (cons (load-single-stack risp) locs)
                                                               iint (fx+ ifp 1) (fx+ risp 8) sisp))]
                                                       [(fp-ftd& ,ftd)
                                                        (let* ([classes (classify-eightbytes ftd)]
                                                               [ints (count 'integer classes)]
                                                               [fps (count 'float classes)])
                                                          (cond
                                                           [(pass-here-by-stack? classes iint ints ifp fps)
                                                            ;; receive on the stack
                                                            (f (cdr types)
                                                               (cons (load-stack-address sisp) locs)
                                                               iint ifp risp (fx+ sisp ($ftd-size ftd)))]
                                                           [else
                                                            ;; receive via registers; `save-args-regs` has saved
                                                            ;; the registers in a suitable order so that the data
                                                            ;; is contiguous on the stack
                                                            (f (cdr types)
                                                               (cons (load-stack-address risp) locs)
                                                               (fx+ iint ints) (fx+ ifp fps) (fx+ risp (fx* 8 (fx+ ints fps))) sisp)]))]
                                                       [else
                                                        (if (= iint 8)
                                                            (f (cdr types)
                                                               (cons (load-int-stack (car types) sisp) locs)
                                                               iint ifp risp (fx+ sisp 8))
                                                            (f (cdr types)
                                                               (cons (load-int-stack (car types) risp) locs)
                                                               (fx+ iint 1) ifp (fx+ risp 8) sisp))])))))
                              (define (do-result result-type result-classes adjust-active?) ;;@ load return val on stack into reg
                                (nanopass-case (Ltype Type) result-type
                                               [(fp-ftd& ,ftd)
                                                (cond
                                                 [(result-fits-in-registers? result-classes)
                                                  ;; Copy content of result area on stack into
                                                  ;; the integer and floating-point registers
                                                  (let loop ([result-classes result-classes]
                                                             [offset 240]
                                                             [int* (list %Carg1 %Carg2)]
                                                             [fp* (list %Cfpretval %f1)]
                                                             [accum '()]
                                                             [live* '()]
                                                             [fp-live* '()])
                                                    (cond
                                                     [(null? result-classes)
                                                      (values (lambda ()
                                                                (if (pair? (cdr accum)) `(seq ,(car accum) ,(cadr accum)) (car accum)))
                                                              live*
                                                              fp-live*)]
                                                     [(eq? (car result-classes) 'integer)
                                                      (loop (cdr result-classes)
                                                            (fx+ offset 8)
                                                            (cdr int*)
                                                            fp*
                                                            (cons `(set! ,(car int*) ,(%mref ,%sp ,offset))
                                                                  accum)
                                                            (cons (car int*) live*)
                                                            fp-live*)]
                                                     [(eq? (car result-classes) 'float)
                                                      (loop (cdr result-classes)
                                                            (fx+ offset 8)
                                                            int*
                                                            (cdr fp*)
                                                            (cons `(inline ,(make-info-loadfl (car fp*)) ,%load-double ,%sp ,%real-zero (immediate ,offset))
                                                                  accum)
                                                            live*
                                                            (cons (car fp*) fp-live*))]))]
                                                 [else
                                                  (values (lambda ()
                                                            ;; put the return value pointer in return(1st arg) reg
                                                            `(set! ,%Cretval ,(%mref ,%sp 112)))
                                                          (list %Cretval)
                                                          '())])]
                                               [(fp-double-float)
                                                (values
                                                 (lambda (x)
                                                   `(inline ,(make-info-loadfl %Cfpretval) ,%load-double ,x ,%real-zero ,(%constant flonum-data-disp)))
                                                 '()
                                                 (list %Cfpretval))]
                                               [(fp-single-float)
                                                (values
                                                 (lambda (x)
                                                   `(inline ,(make-info-loadfl %Cfpretval) ,%load-double->single ,x ,%real-zero ,(%constant flonum-data-disp)))
                                                 '()
                                                 (list %Cfpretval))]
                                               [(fp-void)
                                                (values (lambda () `(nop))
                                                        '()
                                                        '())]
                                               [else
                                                (values (lambda (x)
                                                          `(set! ,%Cretval ,x))
                                                        (list %Cretval)
                                                        '())]))
                              (define (unactivate result-regs)
                                (let ([e `(seq
                                           (set! ,%Carg1 ,(%mref ,%sp ,(+ (push-registers-size result-regs) 176)))
                                           ,(%inline unactivate-thread ,%Carg1))])
                                  (if (null? result-regs)
                                      e
                                      (%seq
                                       ,(push-registers result-regs)
                                       ,e
                                       ,(pop-registers result-regs)))))
                              (lambda (info)
                                (let ([conv* (info-foreign-conv* info)]
                                      [arg-type* (info-foreign-arg-type* info)]
                                      [result-type (info-foreign-result-type info)])
                                  (let* ([result-classes (classify-type result-type)]
                                         [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)]
                                         [synthesize-first? (and result-classes
                                                                 (result-fits-in-registers? result-classes))]
                                         [locs (do-stack (if synthesize-first? (cdr arg-type*) arg-type*) adjust-active?)])
                                    (let-values ([(get-result result-regs result-fp-regs) (do-result result-type result-classes adjust-active?)])
                                      (values
                                       (lambda ()
                                         (%seq
                                          ,(%seq
                                            (set! ,%sp ,(%inline - ,%sp (immediate 224)))
                                            ,(%inline push ,%ra)
                                            ,(%inline push ,%r23)
                                            ,(%inline push ,%r24)
                                            ,(%inline push ,%r25)
                                            ,(%inline push ,%r26)
                                            ,(%inline push ,%r27)
                                            ,(%inline push ,%r28)
                                            ,(%inline push ,%r29)
                                            ,(%inline push ,%r30)
                                            ,(%inline push ,%r31)
                                            ,(save-arg-regs arg-type*))
                                          ,(if-feature pthreads
                                                       ((lambda (e)
                                                          (if adjust-active?
                                                              (%seq
                                                               (set! ,%Cretval ,(%inline activate-thread))
                                                               (set! ,(%mref ,%sp 248) ,%Cretval)
                                                               ,e)
                                                              e))
                                                        (%seq
                                                         (set! ,%Cretval ,(%inline get-tc))
                                                         (set! ,%tc ,%Cretval)))
                                                       ;;@ not threaded, just call get-thread-context
                                                       `(set! ,%tc (literal ,(make-info-literal #f 'entry (lookup-c-entry thread-context) 0))))))
                                       (let ([locs (reverse locs)])
                                         (if synthesize-first?
                                             (cons (load-stack-address 240) ; space on stack for results to be returned via registers ;;@ todo
                                                   locs)
                                             locs))
                                       get-result
                                       (lambda ()
                                         (define callee-save-regs ;;@ todo
                                           (list %r3 %r8 %r9 %r18 %r19 %r23 %r24 %r25 %r26 %r27 %r28 %r29 %r30 %r31))
                                         (in-context Tail
                                                     ((lambda (e)
                                                        (if adjust-active?
                                                            (%seq
                                                             ,(unactivate (append result-fp-regs result-regs))
                                                             ,e)
                                                            e))
                                                      (%seq
                                                       ,(%seq
                                                         (set! ,%r31 ,(%inline pop))
                                                         (set! ,%r30 ,(%inline pop))
                                                         (set! ,%r29 ,(%inline pop))
                                                         (set! ,%r28 ,(%inline pop))
                                                         (set! ,%r27 ,(%inline pop))
                                                         (set! ,%r26 ,(%inline pop))
                                                         (set! ,%r25 ,(%inline pop))
                                                         (set! ,%r24 ,(%inline pop))
                                                         (set! ,%r23 ,(%inline pop))
                                                         (set! ,%ra ,(%inline pop))
                                                         (set! ,%sp ,(%inline + ,%sp (immediate 224))))
                                                       (asm-c-return ,null-info ,callee-save-regs ... ,result-regs ...)))))))))))))) ;; foreign


  )
