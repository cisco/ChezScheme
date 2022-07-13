;; TODO
;; check various jump offsets
;; unify cond jump offset
;; generate better far jumps
;; those mentioned in the Debug log

(define-registers
  (reserved
   [%tc  %r22 #t 22]
   [%sfp %r12 #f 12]
   [%ap  %r13 #f 13])
  (allocable
   [%ac0 %r14 #f 14]
   [%xp  %r15 #f 15]
   [%ts  %r16 #f 16]
   [%td  %r17 #f 17]
   [%ac1 %r18 %deact #f 18]
   [%yp  %r19 #f 19]
   [%cp  %r20 #f 20]
   [     %r4  %Carg1 %Cretval #f 4]
   [     %r5  %Carg2 #f 5]
   [     %r6  %Carg3 #f 6]
   [     %r7  %Carg4 #f 7]
   [     %r8  %Carg5 #f 8]
   [     %r9  %Carg6 #f 9]
   [     %r10 %Carg7 #f 10]
   [     %r11 %Carg8 #f 11]
;; [     %r2  #f 2] ; tp, unallocatable
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
   [%jump %scratch0 %r30 #t 30]
   [%cond %scratch1 %r31 #t 31]
   [%Cfparg1 %Cfpretval %f0 #f 0]
   [%Cfparg2 %f1           #f  1]
   [%Cfparg3 %f2           #f  2]
   [%Cfparg4 %f3           #f  3]
   [%Cfparg5 %f4           #f  4]
   [%Cfparg6 %f5           #f  5]
   [%Cfparg7 %f6           #f  6]
   [%Cfparg8 %f7           #f  7]
   [%flreg1  %f8           #f  8]
   [%flreg2  %f9           #f  9]
   [%flreg3  %f10          #f  10]))

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

   (define imm-shamt?
     (lambda (x)
       (nanopass-case (L15c Triv) x
                      [(immediate ,imm) (shamt? imm)]
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
                                             (values '() `(jump (literal ,info)))]
                                            [(label-ref ,l ,offset)
                                             (values '() `(jump (label-ref ,l ,offset)))]
                                            [else (long-form t)]))))

  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (memq 'ur aty*)
             (and (memq 'imm12 aty*) (imm-signed12? a))
             (and (memq 'shamt aty*) (imm-shamt? a))
             (and (memq 'mem aty*) (mem? a))))]))

  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
          [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
          [(and (memq 'imm12 aty*) (imm-signed12? a)) (k (imm->imm a))]
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
     `(set! ,(make-live-info) ,z (asm ,null-info ,asm-add/ovfl ,x ,y))])

  (define-instruction value (-)
    [(op (z ur) (x ur) (y imm12))
     (let ([n (nanopass-case (L15d Triv) y [(immediate ,imm) imm])])
       (with-output-language (L15d Effect)
                             `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x (immediate ,(- n))))))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub ,x ,y))])

  (define-instruction value (-/ovfl)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,null-info ,asm-sub/ovfl ,x ,y))])

  (define-instruction value (-/eq)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub/eq ,x ,y))])

  (define-instruction value (*)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,x ,y))])

  (define-instruction value (*/ovfl)
    [(op (z ur) (x ur) (y ur))
     (let ([u1 (make-tmp 'u1*)] [u2 (make-tmp 'u2*)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,null-info ,asm-mul/ovfl ,x ,y ,u1 ,u2))))])

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
          [(sll) asm-sll]
          [(srl) asm-srl]
          [(sra) asm-sra]
          [else (sorry! #f "unexpected logical operator ~s" op)])))

    (define-instruction value (logand logor logxor)
      [(op (z ur) (x ur) (y imm12 ur))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,x ,y))])

    (define-instruction value (sll srl sra)
      [(op (z ur) (x ur) (y shamt ur))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,x ,y))]))

  (define-instruction value (lognot)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))])

  (define-instruction value (move)
    [(op (z mem) (x ur))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem imm12))
     `(set! ,(make-live-info) ,z ,x)])

  (define-instruction value lea1 ;;@ todo addi?
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

    (define-instruction effect (load-single->double load-double->single store-single->double
                                store-single store-double load-single load-double)
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
     (let ([info (make-info-condition-code op #f #f)])
       (values '() `(asm ,info ,(asm-fl-relop info) ,x ,y)))])

  (define-instruction effect (flsqrt)
    [(op (x ur) (y ur)) `(asm ,info ,asm-flsqrt ,x ,y)])

  (define-instruction effect inc-cc-counter
    [(op (x ur) (w imm12 ur) (z imm12 ur))
     (let ([u1 (make-tmp 'inc1)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(asm ,null-info ,asm-inc-cc-counter ,x ,w ,z ,u1)))])

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

  (define-instruction pred (> <= >=)
    [(op (x ur) (y ur))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))])

  (define-instruction pred (eq? u< < logtest log!test)
    [(op (x ur) (y imm12 ur))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))])

  (define-instruction pred (condition-code)
    [(op) (values '() `(asm ,info ,(asm-condition-code info)))])

  (define-instruction pred (type-check?)
    [(op (x ur) (mask imm12 ur) (type imm12 ur))
     (let ([u (make-tmp 'u)])
       (values
        (with-output-language (L15d Effect)
                              `(set! ,(make-live-info) ,u (asm ,null-info ,asm-logand ,x ,mask)))
        `(asm ,info-cc-eq ,asm-eq ,u ,type)))])

  (let ()
    ;; put base+index+offset in one reg
    (define lea->reg
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
     `(asm ,info ,asm-indirect-call ,x ,(info-kill*-live*-live* info) ...)])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect (invoke-prelude)
    [(op) `(set! ,(make-live-info) ,%tc ,%Carg1)])

  )

(module asm-module (asm-add asm-add/carry asm-add/ovfl asm-sub asm-sub/ovfl asm-sub/eq
                    asm-mul asm-mul/ovfl asm-div asm-logand asm-logor asm-logxor asm-lognot
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
                    asm-size signed12? unsigned12? signed14? signed16? signed20? signed26? shamt?)

  (define-syntax define-op
    (lambda (x)
      (syntax-case x ()
        [(k op handler e ...)
         (with-syntax ([op (construct-name #'k "asmop-" #'op)])
                      #'(define-syntax op
                          (syntax-rules ()
                            [(_ mneu arg (... ...))
                             (handler 'mneu e ... arg (... ...))])))])))

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

  (define-op slti   two-R-imm12-op #b1000)
  (define-op sltui  two-R-imm12-op #b1001)
  (define-op addi.d two-R-imm12-op #b1011)
  (define-op addi.w two-R-imm12-op #b1010)
  (define-op andi  two-R-imm12-op #b1101)
  (define-op ori   two-R-imm12-op #b1110)
  (define-op xori  two-R-imm12-op #b1111)
  (define-op ld.b  two-R-imm12-op #b10100000)
  (define-op ld.h  two-R-imm12-op #b10100001)
  (define-op ld.w  two-R-imm12-op #b10100010)
  (define-op ld.d  two-R-imm12-op #b10100011)
  (define-op ld.bu two-R-imm12-op #b10101000)
  (define-op ld.hu two-R-imm12-op #b10101001)
  (define-op ld.wu two-R-imm12-op #b10101010)
  (define-op st.b  two-R-imm12-op #b10100100)
  (define-op st.h  two-R-imm12-op #b10100101)
  (define-op st.w  two-R-imm12-op #b10100110)
  (define-op st.d  two-R-imm12-op #b10100111)
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
  (define-op pcaddu18i  one-R-imm20-op #b1111)

  (define-op beqz  one-R-imm23-op #b10000)
  (define-op bnez  one-R-imm23-op #b10001)
  (define-op bceqz one-R-imm23-op #b10010)
  (define-op bcnez one-R-imm23-op #b10010)

  (define-op jirl two-R-imm18-op #b10011)
  (define-op bne  two-R-imm18-op #b10111)
  (define-op beq  two-R-imm18-op #b10110)

  (define-op b  imm28-op #b10100)
  (define-op bl imm28-op #b10101)

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
      (safe-assert (shamt? (ax-imm-data shamt op)))
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
      (safe-assert (signed12? (ax-imm-data imm12 op)))
      (emit-code (op dest src0 imm12 code*)
                 [22 opcode]
                 [10 (ax-imm-data imm12 op)]
                 [5 (ax-ea-reg-code src0)]
                 [0 (ax-ea-reg-code dest)])))

  (define two-R-imm14-op
    (lambda (op opcode dest src0 imm14 code*)
      (safe-assert (signed14? (ax-imm-data imm14 op)))
      (emit-code (op dest src0 imm14 code*)
                 [24 opcode]
                 [10 (ax-imm-data imm14 op)]
                 [5 (ax-ea-reg-code src0)]
                 [0 (ax-ea-reg-code dest)])))

  (define one-R-imm20-op
    (lambda (op opcode dest imm20 code*)
      (safe-assert (signed20? (ax-imm-data imm20 op)))
      (emit-code (op dest imm20 code*)
                 [25 opcode]
                 [5 (ax-imm-data imm20 op)]
                 [0 (ax-ea-reg-code dest)])))

  (define two-R-imm18-op
    (lambda (op opcode link src imm18 code*)
      (safe-assert (signed18? (ax-imm-data imm18 op))
                   (code-disp? (ax-imm-data imm18 op))) ; offset in multiples of 4
      (emit-code (op link src imm18 code*)
                 [26 opcode]
                 [10 (fxsra (ax-imm-data imm18 op) 2)]
                 [5 (ax-ea-reg-code src)]
                 [0 (ax-ea-reg-code link)])))

  (define one-R-imm23-op
    (lambda (op opcode src imm23 code*)
      (safe-assert (signed23? (ax-imm-data imm23 op))
                   (code-disp? (ax-imm-data imm23 op))) ; offset in multiples of 4
      (let* ([imm (fxsra (ax-imm-data imm23 op) 2)]
             [offs0  (fxlogand imm #xffff)] ; offs[15:0]
             [offs16 (fxlogand (ash imm -16) #b11111)] ; offs[20:16]
             [s (case op
                  [(bceqz) #b0000]
                  [(bcnez) #b0100]
                  [else (ax-ea-reg-code src)])])
        (emit-code (op src imm23 code*)
                   [26 opcode]
                   [10 offs0]
                   [5 s]
                   [0 offs16]))))

  (define imm28-op
    (lambda (op opcode imm28 code*)
      (safe-assert (signed28? (ax-imm-data imm28 op))
                   (code-disp? (ax-imm-data imm28 op))) ; offset in multiples of 4
      (let* ([imm (fxsra (ax-imm-data imm28 op) 2)]
             [offs0  (fxlogand imm #xffff)] ; offs[15:0]
             [offs16 (fxlogand (ash imm -16) #b1111111111)]) ; offs[25:16] 10 bits
        (emit-code (op imm28 code*)
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
      (and (fixnum? imm) (fx<= 0 imm (expt 2 6)))))
  (define signed12?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 11))
                               imm
                               (fx- (expt 2 11) 1)))))
  (define unsigned12?
    (lambda (imm)
      (and (fixnum? imm) (fx<= 0 imm (expt 2 12)))))
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
  (define signed18?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 17))
                               imm
                               (fx- (expt 2 17) 1)))))
  (define signed20?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 19))
                               imm
                               (fx- (expt 2 19) 1)))))
  (define signed23?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 22))
                               imm
                               (fx- (expt 2 22) 1)))))
  (define signed26?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 25))
                               imm
                               (fx- (expt 2 25) 1)))))
  (define signed28?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 27))
                               imm
                               (fx- (expt 2 27) 1)))))
  (define signed32?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 31))
                               imm
                               (fx- (expt 2 31) 1)))))
  (define signed38?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 37))
                               imm
                               (fx- (expt 2 37) 1)))))
  ;; for jirl
  (define jump-disp?
    (lambda (imm)
      (and (fixnum? imm)
           (code-disp? imm)
           (fx<= (fx- (expt 2 17))
                 imm
                 (fx- (expt 2 17) 1)))))
  ;; for b and bl, 28-bit offset, but the lower 2 bits are not encoded
  (define branch-disp?
    (lambda (imm)
      (and (fixnum? imm)
           (code-disp? imm)
           (fx<= (fx- (expt 2 27))
                 imm
                 (fx- (expt 2 27) 1)))))
  ;; 23-bit offset, but the lower 2 bits are not encoded
  (define cond-branch-disp?
    (lambda (imm)
      (and (fixnum? imm)
           (code-disp? imm)
           (fx<= (fx- (expt 2 22))
                 imm
                 (fx- (expt 2 22) 1)))))
  (define code-disp?
    (lambda (imm)
      (and (fixnum? imm)
           ;; lower two bits are zero
           (not (fxlogtest imm #b11)))))

  (define-who ax-ea-reg-code
    (lambda (ea)
      (if (pair? ea)
          (record-case ea
            [(reg) r (reg-mdinfo r)]
            [else (sorry! who "not a reg: ea=~s" ea)])
          (if (reg? ea)
              (reg-mdinfo ea)
              (sorry! who "not a reg: ea=~s" ea)))))

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
       (fx+ (bitwise-arithmetic-shift-left e n) ...)]))

  (define upper20-32
    (lambda (x)
      (ash (fx+ x #x800) -12)))
  (define lower12-32
    (lambda (x)
      (fx- x (ash (upper20-32 x) 12))))

  (define upper20-38
    (lambda (x)
      (ash (fx+ x #x20000) -18)))
  (define lower18-38
    (lambda (x)
      (fx- x (ash (upper20-38 x) 18))))

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
              (emit addi.d %scratch0 src0 0
                    (emit add.d dest src0 src1
                          (emit sltu %cond dest %scratch0 code*))))))

  (define sign-flip
    (lambda (r a b code*)
      (emit xor a a b
            (emit xori b b -1
                  (emit xor b b r
                        (emit or a a b
                              (emit xori a a -1
                                    (emit srli.d %cond a 63 code*))))))))

  (define asm-add/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit addi.d %scratch0 src0 0
                    (emit addi.d %scratch1 src1 0
                          (emit add.d dest src0 src1
                                (sign-flip dest %scratch0 %scratch1 code*)))))))

  (define asm-sub
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub.d dest src0 src1 code*))))

  (define asm-sub/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit addi.d %scratch0 src0 0
                    (emit addi.d %scratch1 src1 0
                          (emit sub.d dest src0 src1
                                (emit xori %scratch1 %scratch1 -1
                                      (sign-flip dest %scratch0 %scratch1 code*))))))))

  (define asm-sub/eq
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub.d dest src0 src1
                    (emit sltui %cond dest 1 code*))))) ;; set if dest == 0

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit mul.d dest src0 src1 code*))))

  (define asm-mul/ovfl
    (lambda (code* dest src0 src1 t1 t2)
      (Trivit (dest src0 src1 t1 t2)
              (let ([zero `(reg . ,%real-zero)]
                    [cond `(reg . ,%cond)]
                    [t0 %scratch0])
                (emit addi.d t1 src0 0
                      (emit addi.d t2 src1 0
                            (emit mul.d dest src0 src1
                                  ;; if src0 == 0 || src1 == 0, then no ovfl
                                  (emit sltui t0 t1 1
                                        (emit sltui cond t2 1
                                              (emit or cond t0 cond
                                                    (emit bnez cond 20
                                                          ;; if dest/src0 != src1, ovfl!
                                                          (emit div.d t0 dest t1
                                                                (emit beq t0 t2 12
                                                                      (emit addi.d cond zero 1
                                                                            (emit b 8
                                                                                  (emit addi.d cond zero 0 code*))))))))))))))))

  (define asm-div
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit div.d dest src0 src1 code*))))

  (module (asm-logand asm-logor asm-logxor asm-sll asm-srl asm-sra)
          (define-syntax gen-op
            (lambda (x)
              (syntax-case x ()
                [(_ op opi)
                 #'(lambda (code* dest src0 src1)
                     (Trivit (dest src0 src1)
                             (record-case src1
                                          [(imm) (n) (emit opi dest src0 n code*)]
                                          [else (emit op dest src0 src1 code*)])))])))

          (define asm-logand (gen-op and andi))
          (define asm-logor (gen-op or ori))
          (define asm-logxor (gen-op xor xori))
          (define asm-sll (gen-op sll.d slli.d))
          (define asm-srl (gen-op srl.d srli.d))
          (define asm-sra (gen-op sra.d srai.d)))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit xori dest src -1 code*))))

  (define-who asm-fl-load/cvt
    (lambda (op flreg)
      (lambda (code* base offset)
        (Trivit (base offset)
                (case op
                  [(load-single->double)
                   (emit fld.s %flreg2 base offset
                         (emit fcvt.d.s flreg %flreg2 code*))]
                  [(load-double->single)
                   (emit fld.d %flreg2 base offset
                         (emit fcvt.s.d flreg %flreg2 code*))]
                  [else (sorry! who "unrecognized op ~s" op)])))))

  (define-who asm-fl-store/cvt
    (lambda (op flreg)
      (lambda (code* base offset)
        (Trivit (base offset)
                (case op
                  [(store-single->double)
                   (emit fcvt.d.s %flreg2 flreg
                         (emit fst.d %flreg2 base offset code*))]
                  [else (sorry! who "unrecognized op ~s" op)])))))

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
      ;; args contain the base addr of fp numbers, thus need the fp disp
      (lambda (code* src1 src2 dest)
        (Trivit (src1 src2 dest)
                (let ([disp (constant flonum-data-disp)])
                  (emit fld.d %flreg1 src1 disp
                        (emit fld.d %flreg2 src2 disp
                              (let ([code* (emit fst.d %flreg1 dest disp code*)])
                                (case op
                                  [(fl+) (emit fadd.d %flreg1 %flreg1 %flreg2 code*)]
                                  [(fl-) (emit fsub.d %flreg1 %flreg1 %flreg2 code*)]
                                  [(fl*) (emit fmul.d %flreg1 %flreg1 %flreg2 code*)]
                                  [(fl/) (emit fdiv.d %flreg1 %flreg1 %flreg2 code*)]
                                  [else (sorry! who "unrecognized op ~s" op)])))))))))

  (define asm-flsqrt ;;@ load, calc, store
    (lambda (code* src dest)
      (Trivit (src dest)
              (let ([disp (constant flonum-data-disp)])
                (emit fld.d %flreg1 src disp
                      (emit fsqrt.d %flreg2 %flreg1
                            (emit fst.d %flreg2 dest disp code*)))))))

  (define asm-trunc ;;@ flonum to fixnum
    (lambda (code* dest src)
      (Trivit (dest src)
              (let ([disp (constant flonum-data-disp)])
                (emit fld.d %flreg1 src disp
                      (emit ffint.l.d dest %flreg1 code*))))))

  (define asm-flt ;;@ fixnum to flonum
    (lambda (code* src dest)
      (Trivit (src dest) ;;@ todo flonumreg holds addr?
              (let ([disp (constant flonum-data-disp)])
                (emit ffint.d.l %flreg1 src
                      (emit fst.d %flreg1 dest disp code*))))))

  (define-who asm-swap ;;@ big <-> little-endian
    (lambda (type)
      (rec asm-swap-internal
           (lambda (code* dest src)
             (Trivit (dest src)
                     (let ([zero `(reg . ,%real-zero)])
                       (case type ;;@ rethink sign-extension stuff
                         [(integer-16)
                          (emit revb.2h dest src
                                (emit slli.d dest dest 48
                                      (emit srai.d dest dest 48 code*)))]
                         [(unsigned-16)
                          (emit revb.2h dest src
                                (emit slli.d dest dest 48
                                      (emit srli.d dest dest 48 code*)))]
                         [(integer-32)
                          (emit revb.2w dest src
                                (emit slli.d dest dest 32
                                      (emit srai.d dest dest 32 code*)))]
                         [(unsigned-32)
                          (emit revb.2w dest src
                                (emit slli.d dest dest 32
                                      (emit srli.d dest dest 32 code*)))]
                         [(integer-64 unsigned-64) (emit revb.d dest src code*)]
                         [else (sorry! who "unexpected asm-swap type argument ~s" type)])))))))

  (define asm-lock
    ;;    ll.d t0, [addr]
    ;;    bnez  t0,  L
    ;;    addi.d t0, %real-zero, 1
    ;;    sc.d t0, t0, [addr]
    ;;L:
    (lambda (code* addr t0)
      (Trivit (addr t0)
              (emit ll.d t0 addr 0
                    (emit bnez t0 12
                          (emit addi.d t0 %real-zero 1
                                (emit sc.d t0 addr 0 code*))))))) ;;@todo sc.d semantics

  (define-who asm-lock+/-
    ;;S:
    ;;    ll.d t0, [addr]
    ;;    addi.d t0, t0, +/-1
    ;;    sc.d t1, t0, [addr]
    ;;    bnez  t1, S[-12]
    ;;    sltui %cond t0 1 # set %cond if t0=0
    (lambda (op)
      (lambda (code* addr t0 t1)
        (Trivit (addr t0 t1)
                (emit ll.d t0 addr 0
                      (let ([code* (emit sc.d t0 addr 0 ;;@todo check this
                                         (emit bnez t0 -12
                                               (emit sltui %cond t1 1 code*)))])
                        (case op
                          [(locked-incr!) (emit addi.d t0 t0 1
                                                (emit addi.d t1 t0 0 code*))] ; backup
                          [(locked-decr!) (emit addi.d t0 t0 -1
                                                (emit addi.d t1 t0 0 code*))]
                          [else (sorry! who "unexpected op ~s" op)])))))))

  (define asm-cas
    ;;cas:
    ;;   ll.d t0, addr
    ;;   bne  t0, old, L[12]
    ;;   sc.d t1, new, [addr] # t1!=0 if store fails
    ;;   sltui %cond t1 1    # %cond=1 if t1=0(succeed)
    ;;L:
    (lambda (code* addr old new t0 t1)
      (Trivit (addr old new t0 t1)
              (emit ll.d t0 addr 0
                    (emit addi.d t1 new 0 ; backup
                          (emit bne t0 old 16
                                (emit sc.d new addr 0
                                      (emit sltui %cond new 1
                                            (emit b 8
                                                  (emit xor %cond %cond %cond code*))))))))))

  (define-who asm-relop
    (lambda (info)
      (rec asm-relop-internal
           (lambda (l1 l2 offset x y)
             (let ([op (info-condition-code-type info)])
               (Trivit (x y)
                       (define (bad!) (sorry! who "second operand cannot be imm: op ~s ~s" op x y))
                       (record-case y
                                    [(reg) ignore
                                     (values
                                      (case op
                                        [(eq?) (emit xor %scratch0 x y
                                                     (emit sltui %cond %scratch0 1 '()))]
                                        [(u<) (emit sltu %cond x y '())]
                                        [(<) (emit slt %cond x y '())]
                                        [(>) (emit slt %cond y x '())]
                                        [(<=) (emit slt %scratch0 y x
                                                    (emit xori %cond %scratch0 1 '()))]
                                        [(>=) (emit slt %scratch0 x y
                                                    (emit xori %cond %scratch0 1 '()))]
                                        [(logtest) (emit and %cond x y '())]
                                        [(log!test) (emit and %scratch0 x y
                                                          (emit sltui %cond %scratch0 1 '()))])
                                      (asm-conditional-jump info l1 l2 offset))]
                                    [(imm) (n)
                                     (values
                                      (case op
                                        [(eq?) (emit xori %scratch0 x n
                                                     (emit sltui %cond %scratch0 1 '()))]
                                        [(u<) (emit sltui %cond x n '())]
                                        [(<) (emit slti %cond x n '())]
                                        [(logtest) (emit andi %cond x n '())]
                                        [(log!test) (emit andi %scratch0 x n
                                                          (emit sltui %cond %scratch0 1 '()))]
                                        [else (bad!)])
                                      (asm-conditional-jump info l1 l2 offset))])))))))

  (define-who asm-fl-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
                (values
                 (let ([disp (constant flonum-data-disp)]
                       [op (info-condition-code-type info)])
                   (emit fld.d %flreg1 x disp
                         (emit fld.d %flreg2 y disp
                               (emit fcmp.d op %flreg1 %flreg2 ; internally use the first flag reg
                                     (emit bceqz '() 12 ; ditto '(): placeholder
                                           (emit addi.d %cond %real-zero 1
                                                 (emit b 8
                                                       (emit addi.d %cond %real-zero 0 '()))))))))
                 (asm-conditional-jump info l1 l2 offset))))))

  (define asm-read-performance-monitoring-counter
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit addi.d dest %real-zero 0 code*))))

  (define asm-read-time-stamp-counter
    (lambda (code* dest)
      (Trivit (dest)
              (emit rdtime.d dest %real-zero code*))))

  (define-who inc-cc-helper
    (lambda (val t code*)
      (nanopass-case (L16 Triv) val
                     [(immediate ,imm) (emit addi.d t t imm code*)]
                     [,x (emit add.d t t x code*)]
                     [else (sorry! who "unexpected val format ~s" val)])))

  (define-who asm-inc-cc-counter ;; load, add, store back
    (lambda (code* base offset val t)
      (Trivit (base t)
              (nanopass-case (L16 Triv) offset
                             [(immediate ,imm)
                              (emit ld.d t base imm
                                    (inc-cc-helper val t
                                                   (emit st.d t base imm code*)))]
                             [,x
                              (emit ldx.d t base x
                                    (inc-cc-helper val t
                                                   (emit stx.d t base x code*)))]
                             [else (sorry! who "unexpected offset format ~s" offset)]))))

  (define asm-enter values)

  (define asm-fence ;;@ ibar? or dbar? or others?
    (lambda (code*)
      (emit ibar code*)))

  (define ax-mov32
    (lambda (dest n code*) ;;@ todo check 32bit load semantics
      (let ([upper (upper20-32 n)]
            [lower (lower12-32 n)])
        (safe-assert (signed20? upper))
        (emit lu12i.w dest upper
              (emit ori dest dest lower code*)))))
  (define ax-mov64
    (lambda (dest n code*)
      (emit pcaddi dest 0
            (emit ld.d dest dest 12
                  (emit b 12
                        (cons* `(quad . ,n)
                               (aop-cons* `(asm "quad:" ,n) code*)))))))

  (define-who asm-move
    (lambda (code* dest src)
      ;; move pseudo instruction used by set! case in select-instruction
      ;; guarantees dest is a reg and src is reg, mem, or imm OR dest is
      ;; mem and src is reg.
      (Trivit (dest src)
              (define (bad!) (sorry! who "unexpected combination of src ~s and dest ~s" src dest))
              (cond
               [(ax-reg? dest)
                (record-case src
                  [(reg) ignore (emit addi.d dest src 0 code*)]
                  [(imm) (n)
                   (if (signed12? n)
                       (emit addi.d dest %real-zero n code*)
                       (if (signed32? n)
                           (ax-mov32 dest n code*)
                           (ax-mov64 dest n code*)))]
                  [(literal) stuff
                   (ax-mov64 dest 0
                             (asm-helper-relocation code* (cons 'la64-abs stuff)))]
                  [(disp) (n breg)
                   (safe-assert (signed12? n))
                   (emit ld.d dest breg n code*)]
                  [(index) (n ireg breg)
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
                  [else (bad!)])]
               [else (bad!)]))))

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

  (define-who asm-return-address
    (lambda (dest l incr-offset next-addr)
      (make-rachunk dest l incr-offset next-addr
                    (or (cond ;; next-offset
                         [(local-label-offset l) =>
                          (lambda (offset)
                            (let ([disp (fx- next-addr (fx- offset incr-offset) -8)])
                              (cond
                               [(signed32? disp)
                                (Trivit (dest)
                                        (emit pcaddu12i dest (upper20-32 disp)
                                              ;;@ todo addi.d or ori?
                                              (emit addi.d dest dest (lower12-32 disp) '())))]
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
                          ;;@ todo use `b` for 28-bit offset
                          [(signed38? disp) (emit pcaddu18i %jump (upper20-38 (fx+ disp 8))
                                                  (emit jirl %real-zero %jump (lower18-38 (fx+ disp 8)) '()))]
                          [else (sorry! who "disp value not supported")])))]
                    [else
                     ;; label must be somewhere above.  generate something so that a hard loop
                     ;; doesn't get dropped.  this also has some chance of being the right size
                     ;; for the final branch instruction.
                     (emit pcaddu18i %jump 0
                           (emit jirl %real-zero %jump 0 '()))]))))

  (define asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      (define get-disp
        (lambda (next-addr l)
          (cond
           [(and (local-label? l) (local-label-offset l)) =>
            (lambda (offset)
              (let ([disp (fx- next-addr offset)])
                (safe-assert (signed32? disp))
                disp))]
           [else 0])))
      (make-cgchunk info l1 l2 next-addr
                    (let ([disp1 (get-disp next-addr l1)]
                          [disp2 (get-disp next-addr l2)])
                      (cond
                       ;; inverted
                       [(fx= disp1 0)
                        (safe-assert (signed23? (fx+ disp2 4)))
                        (emit beqz %cond (fx+ disp2 4) '())]
                       ;; normal
                       [(fx= disp2 0)
                        (safe-assert (signed23? (fx+ disp1 4)))
                        (emit bnez %cond (fx+ disp1 4) '())]
                       ;; others
                       [else
                        (safe-assert (signed23? (fx+ disp1 8))
                                     (signed28? (fx+ disp2 4)))
                        (emit bnez %cond (fx+ disp1 8)
                              (emit b (fx+ disp2 4) '()))])))))

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
                [(reg) ignore (emit jirl %real-zero src 0 '())]
                ;; the following two load EA from memory first
                [(disp) (n breg)
                 (safe-assert (signed12? n))
                 (emit ld.d %jump breg n
                       (emit jirl %real-zero %jump 0 '()))]
                [(index) (n ireg breg)
                 (safe-assert (eqv? n 0))
                 (emit ldx.d %jump ireg breg
                       (emit jirl %real-zero %jump 0 '()))]
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
              (emit addi.d %sp %sp -16
                    (emit st.d x %sp 0 code*)))))

  (define asm-pop
    (lambda (code* dest)
      (Trivit (dest)
              (emit ld.d dest %sp 0
                    (emit addi.d %sp %sp 16 code*)))))

  (define asm-return
    (lambda ()
      (emit jirl %real-zero %ra 0 '())))

  (define asm-c-return
    (lambda (info)
      (emit jirl %real-zero %ra 0 '())))

  (define asm-helper-jump ;; no need to save ra
    (lambda (code* reloc) ;;@ todo need saving %cond?
      (emit pcaddi %jump 0
            (emit ld.d %jump %jump 12
                  (emit b 12
                        (cons* `(quad . 0)
                               (aop-cons* `(asm "quad jump addr")
                                          (emit jirl %real-zero %jump 0
                                                (asm-helper-relocation code* reloc)))))))))

  (define asm-helper-call ;; need to save ra
    (lambda (code* reloc save-ra? tmp)
      (define maybe-save-ra
        (lambda (code* p)
          (if save-ra?
              (emit addi.d %sp %sp -16 ;; save %ra on the stack
                    (emit st.d %ra %sp 0
                          (p (emit ld.d %ra %sp 0
                                   (emit addi.d %sp %sp 16 code*)))))
              (p code*))))
      (maybe-save-ra code*
                     (lambda (code*)
                       (emit pcaddi tmp 0
                             (emit ld.d tmp tmp 12
                                   (emit b 12
                                         (cons* `(quad . 0)
                                                (aop-cons* `(asm "quad call addr")
                                                           (emit jirl %ra tmp 0
                                                                 (asm-helper-relocation code* reloc)))))))))))

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
              (let ([k (fx- b 1)])
                (fxlogand (fx+ x k) (fxlognot k)))))

          ;; Currently:
          ;; no foreign-callable
          ;; no multithreading support
          ;; no large return value

          (define-who asm-foreign-call
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
                                                                   [(fp-ftd& ,ftd) (sorry! who "indirect arguments not supported")]
                                                                   [else
                                                                    (if (< iint 8)
                                                                        (loop (cdr types)
                                                                              (cons (load-int-reg (vector-ref vint iint)) locs)
                                                                              (cons (vector-ref vint iint) regs) fp-regs
                                                                              (fx+ iint 1) ifp isp)
                                                                        (loop (cdr types)
                                                                              (cons (load-int-stack isp) locs)
                                                                              regs fp-regs iint ifp (fx+ isp 8)))]))))])
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
                                             [result-type (info-foreign-result-type info)])
                                        (with-values (do-args arg-type* (make-vint) (make-vfp))
                                          (lambda (frame-size nfp locs live* fp-live*)
                                            (returnem frame-size
                                                      locs
                                                      (lambda (t0)
                                                        `(inline ,(make-info-kill*-live* (reg-list %Cretval) live*) ,%c-call ,t0))
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
                                                                     [else (lambda (lvalue) `(set! ,lvalue ,%Cretval))])))))))))

          (define-who asm-foreign-callable
            (lambda (info)
              (sorry! who "foreign-callable not supported")
              (values 'c-init 'c-args 'c-result 'c-return)))

          ) ;; foreign


  )
