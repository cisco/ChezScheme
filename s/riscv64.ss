;;; riscv64.ss

;;; SECTION 1: registers
(define-registers
  (reserved
   [%tc  %x8 #t 8]
   [%sfp %x6 #f 6]
   [%ap  %x7 #f 7])
  (allocable
   [%ac0 %x28 #f 28]
   [%xp  %x29 #f 29]
   [%ts  %x5 #f 5]
   [%td  %x9 #t 9]
   [%ac1 %x18 %deact #t 18]
   [%yp  %x19 #t 19]
   [%cp  %x20 #t 20]
   [     %x10 %Carg1 %Cretval #f 10]
   [     %x11 %Carg2 #f 11]
   [     %x12 %Carg3 #f 12]
   [     %x13 %Carg4 #f 13]
   [     %x14 %Carg5 #f 14]
   [     %x15 %Carg6 #f 15]
   [     %x16 %Carg7 #f 16]
   [     %x17 %Carg8 #f 17]
;; [     %x3         #f 3] ; gp, unallocatable
;; [     %x4         #f 4] ; tp, unallocatable
;; [     %x18 #t 18]
;; [     %x19 #t 19]
;; [     %x20 #t 20]
   [     %x21 #t 21]
   [     %x22 #t 22]
   [     %x23 #t 23]
   [     %x24 #t 24]
   [     %x25 #t 25]
   [     %x26 #t 26]
   [     %x27 #t 27]
;; [     %x28 #f 28]
;; [     %x29 #f 29]
;; [     %x30 #f 30]
   )
  (machine-dependent
   [%real-zero %x0 #f 0]
   [%ra %x1 #f 1]
   [%sp %x2 #t 2]
   [%jump %scratch0 %x30 #f 30]
   ;; for carry/ovfl flag, since RISC-V has no flag regs
   [%cond %scratch1 %x31 #f 31]
   [%Cfparg1 %Cfpretval %f10 #f 10]
   [%Cfparg2 %f11           #f  11]
   [%Cfparg3 %f12           #f  12]
   [%Cfparg4 %f13           #f  13]
   [%Cfparg5 %f14           #f  14]
   [%Cfparg6 %f15           #f  15]
   [%Cfparg7 %f16           #f  16]
   [%Cfparg8 %f17           #f  17]
   [%flreg1  %f1            #f  1]
   [%flreg2  %f2            #f  2]
   [%flreg3  %f3            #f  3]))

;;; SECTION 2: instructions
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
     ;; sth wrong with the reg allocator: z will be the same
     ;; as u1 if u1 if created using make-tmp
     (let ([u1 (make-precolored-unspillable 'u1 %x12) #;(make-tmp 'u1)]
           [u2 (make-precolored-unspillable 'u2 %x17) #;(make-tmp 'u2)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-mul/ovfl ,x ,y ,u1 ,u2))))])

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

  (define-instruction value lea1
    [(op (z ur) (x ur))
     ;; z = x + offset
     (let ([offset (info-lea-offset info)])
       (if (signed12? offset)
           `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x (immediate ,offset))) ;;@ not Trivited yet, use (immediate)
           (let ([u (make-tmp 'lea1)])
             (seq
              `(set! ,(make-live-info) ,u (immediate ,offset))
              `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,u))))))])

  (define-instruction value lea2
    [(op (z ur) (x ur) (y ur))
     ;; z = x + y + offset
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
      (lambda (x y w k) ;; logic of w depends on its type
        (with-output-language (L15d Effect)
                              (if (ur? w)
                                  (if (eq? y %zero)
                                      (k x w imm-zero)
                                      (let ([u (make-tmp 'ls1)])
                                        (seq
                                         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w))
                                         (k x u imm-zero))))
                                  (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
                                    (if (signed12? n)
                                        (if (eq? y %zero)
                                            (let ([w (in-context Triv `(immediate ,n))])
                                              (k x y w))
                                            (let ([u (make-tmp 'ls2)])
                                              (seq
                                               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w))
                                               (k x u imm-zero))))
                                        (let ([u (make-tmp 'ls3)])
                                          (seq
                                           `(set! ,(make-live-info) ,u (immediate ,n))
                                           (if (eq? y %zero)
                                               (k x u imm-zero)
                                               (seq
                                                `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,u))
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
                           (let ([u (make-tmp 'unique-bob)])
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
              `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,y))
              `(asm ,info ,(pick-asm-op op info) ,u ,z))))]
      [(op (x ur) (y ur) (z ur))
       (let ([u (make-tmp 'fl2)])
         (seq
          `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,z))
          (if (eq? y %zero)
              `(asm ,info ,(pick-asm-op op info) ,u (immediate 0))
              (seq
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,u ,y))
               `(asm ,info ,(pick-asm-op op info) ,u (immediate 0))))))]))

  (define-instruction effect (flt)
    [(op (x ur) (y ur)) `(asm ,info ,asm-flt ,x ,y)])

  (define-instruction value (trunc)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-trunc ,x))])

  (define-instruction effect (flsqrt)
    [(op (x ur) (y ur)) `(asm ,info ,asm-flsqrt ,x ,y)])

  (define-instruction effect (fl+ fl- fl/ fl*)
    [(op (x ur) (y ur) (z ur))
     `(asm ,info ,(asm-flop-2 op) ,x ,y ,z)])

  ;; pred all return multiple values
  (define-instruction pred (fl= fl< fl<=)
    [(op (x ur) (y ur))
     (let ([info (make-info-condition-code op #f #f)])
       (values '() `(asm ,info ,(asm-fl-relop info) ,x ,y)))])

  (define-instruction effect inc-cc-counter
    ;; base offset val
    [(op (x ur) (w imm12) (z imm12 ur))
     (let ([u1 (make-tmp 'inc1)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(asm ,null-info ,asm-inc-cc-counter ,x ,w ,z ,u1)))]
    [(op (x ur) (w ur) (z imm12 ur))
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

  (define-instruction value (get-tc) ;; from arm64.ss
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-get-tc ,u))))])

  (define-instruction value (activate-thread) ;; from arm64.ss
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-activate-thread ,u))))])

  (define-instruction effect (deactivate-thread) ;; from ppc32.ss
    [(op)
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(asm ,info ,asm-deactivate-thread ,u)))])

  (define-instruction effect (unactivate-thread) ;; from ppc32.ss
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
            `(asm ,info ,(asm-library-call! (info-asmlib-libspec info)
                                            (info-asmlib-save-ra? info)) ,u ,(info-kill*-live*-live* info) ...)))])

  (safe-assert (reg-callee-save? %tc))
  (define-instruction effect (c-simple-call)
    [(op)
     (let ([u (make-tmp 'c-simple)])
       (seq
            `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
            `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info)
                                            (info-c-simple-call-save-ra? info)) ,u)))])

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
    (define lea->reg ;; put base+index+offset in one reg
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
    [(op (x ur)) `(asm ,info ,asm-indirect-call ,x ,(info-kill*-live*-live* info) ...)])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect (invoke-prelude)
    [(op) `(set! ,(make-live-info) ,%tc ,%Carg1)])

  )


;;; SSECTION 3: assembler
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
                    asm-size signed12? shamt?)

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

  (define-op add bin-op #b0110011 #b000 #b0000000)
  (define-op sub bin-op #b0110011 #b000 #b0100000)
  (define-op mul bin-op #b0110011 #b000 #b0000001)
  (define-op div bin-op #b0110011 #b100 #b0000001)
  (define-op and bin-op #b0110011 #b111 #b0000000)
  (define-op or  bin-op #b0110011 #b110 #b0000000)
  (define-op xor bin-op #b0110011 #b100 #b0000000)
  (define-op slt bin-op #b0110011 #b010 #b0000000)
  (define-op sltu bin-op #b0110011 #b011 #b0000000)
  (define-op sll  bin-op #b0110011 #b001 #b0000000)
  (define-op srl  bin-op #b0110011 #b101 #b0000000)
  (define-op sra  bin-op #b0110011 #b101 #b0100000)
  (define-op fadd.d bin-op #b1010011 #b111 #b0000001) ; #b111: dynamic rounding mode
  (define-op fsub.d bin-op #b1010011 #b111 #b0000101) ; #b111: dynamic rounding mode
  (define-op fmul.d bin-op #b1010011 #b111 #b0001001) ; #b111: dynamic rounding mode
  (define-op fdiv.d bin-op #b1010011 #b111 #b0001101) ; #b111: dynamic rounding mode
  (define-op feq.d bin-op #b1010011 #b010 #b1010001)
  (define-op flt.d bin-op #b1010011 #b001 #b1010001)
  (define-op fle.d bin-op #b1010011 #b000 #b1010001)

  (define-op fsqrt.d  bin-op #b1010011 #b111 #b0101101) ; #b111: dynamic rounding mode
  (define-op fcvt.l.d bin-op #b1010011 #b111 #b1100001) ; #b111: dynamic rounding mode
  (define-op fcvt.d.l bin-op #b1010011 #b111 #b1101001) ; #b111: dynamic rounding mode
  (define-op fcvt.s.d bin-op #b1010011 #b111 #b0100000) ; #b111: dynamic rounding mode
  (define-op fcvt.d.s bin-op #b1010011 #b111 #b0100001) ; #b111: dynamic rounding mode

  (define-op addi  bin-imm-op  #b0010011 #b000)
  (define-op andi  bin-imm-op  #b0010011 #b111)
  (define-op ori   bin-imm-op  #b0010011 #b110)
  (define-op xori  bin-imm-op  #b0010011 #b100)
  (define-op sltiu bin-imm-op  #b0010011 #b011)
  (define-op slti  bin-imm-op  #b0010011 #b010)
  (define-op jalr  bin-imm-op  #b1100111 #b000)

  (define-op flw bin-imm-op #b0000111 #b010)
  (define-op fld bin-imm-op #b0000111 #b011)
  (define-op ld  bin-imm-op #b0000011 #b011)
  (define-op lw  bin-imm-op #b0000011 #b010)
  (define-op lwu bin-imm-op #b0000011 #b110)
  (define-op lh  bin-imm-op #b0000011 #b001)
  (define-op lhu bin-imm-op #b0000011 #b101)
  (define-op lb  bin-imm-op #b0000011 #b000)
  (define-op lbu bin-imm-op #b0000011 #b100)

  (define-op slli shift-imm-op #b0010011 #b001 #b000000)
  (define-op srli shift-imm-op #b0010011 #b101 #b000000)
  (define-op srai shift-imm-op #b0010011 #b101 #b010000)

  (define-op fsw store-op #b0100111 #b010)
  (define-op fsd store-op #b0100111 #b011)
  (define-op sd  store-op #b0100011 #b011)
  (define-op sw  store-op #b0100011 #b010)
  (define-op sh  store-op #b0100011 #b001)
  (define-op sb  store-op #b0100011 #b000)

  (define-op lr.d atomic-op #b0101111 #b00010)
  (define-op sc.d atomic-op #b0101111 #b00011)

  (define-op lui   imm20-op #b0110111)
  (define-op auipc imm20-op #b0010111)

  (define-op fence  fence-op   #b0001111 #b000)
  (define-op rdtime counter-op #b1110011 #xC01) ; counter number 0xC01

  (define-op jal jal-op #b1101111) ; offset in multiples of 2 bytes

  (define-op bne conditional-branch-op #b1100011 #b001) ; offset in multiples of 2 bytes
  (define-op beq conditional-branch-op #b1100011 #b000) ; offset in multiples of 2 bytes

  (define bin-op
    (lambda (op opcode funct3 funct7 dest rs1 rs2 code*)
      (let ([rs-2 (case op
                   [(fsqrt.d) #b00000]
                   [(fcvt.l.d fcvt.d.l) #b00010]
                   [(fcvt.s.d) #b00001]
                   [(fcvt.d.s) #b00000]
                   [else (ax-ea-reg-code rs2)])])
        (emit-code (op dest rs1 rs2 code*)
                   [25 funct7]
                   [20 rs-2]
                   [15 (ax-ea-reg-code rs1)]
                   [12 funct3]
                   [7 (ax-ea-reg-code dest)]
                   [0 opcode]))))

  (define bin-imm-op
    (lambda (op opcode funct3 dest base imm12 code*)
      (safe-assert (signed12? (ax-imm-data imm12 op)))
      (emit-code (op dest base imm12 code*)
                 [20 (ax-imm-data imm12 op)]
                 [15 (ax-ea-reg-code base)]
                 [12 funct3]
                 [7 (ax-ea-reg-code dest)]
                 [0 opcode])))

  (define shift-imm-op
    (lambda (op opcode funct3 funct7 dest rs1 shamt code*)
      (safe-assert (shamt? shamt)) ;; 6 bit
      (emit-code (op dest rs1 shamt code*)
                 [26 funct7]
                 [20 (ax-imm-data shamt op)]
                 [15 (ax-ea-reg-code rs1)]
                 [12 funct3]
                 [7 (ax-ea-reg-code dest)]
                 [0 opcode])))

  (define store-op
    (lambda (op opcode funct3 src base imm12 code*)
      (safe-assert (signed12? (ax-imm-data imm12 op)))
      (emit-code (op src base imm12 code*)
                 [25 (fxlogand #b1111111 (ash (ax-imm-data imm12 op) -5))] ;; upper 7 bits of imm12
                 [20 (ax-ea-reg-code src)]
                 [15 (ax-ea-reg-code base)]
                 [12 funct3]
                 [7 (fxlogand (ax-imm-data imm12 op) #b11111)] ;; lower 5 bits of imm12
                 [0 opcode])))

  (define atomic-op
    (lambda (op opcode funct5 dest rs1 rs2 code*)
      (emit-code (op dest rs1 rs2 code*)
                 [27 funct5]
                 [26 #b0] ;;@ todo
                 [25 #b0]
                 [20 (if (eq? op 'lr.d)
                         #b00000
                         (ax-ea-reg-code rs2))]
                 [15 (ax-ea-reg-code rs1)]
                 [12 #b011]
                 [7 (ax-ea-reg-code dest)]
                 [0 opcode])))

  (define imm20-op
    (lambda (op opcode dest imm20 code*)
      (safe-assert (signed20? (ax-imm-data imm20 op)))
      (emit-code (op dest imm20 code*)
                 [12 (ax-imm-data imm20 op)]
                 [7 (ax-ea-reg-code dest)]
                 [0 opcode])))

  (define conditional-branch-op
    (lambda (op opcode funct3 rs1 rs2 imm13 code*)
      (safe-assert (fxeven? (ax-imm-data imm13 op))
                   (signed13? (ax-imm-data imm13 op)))
      (let ([imm (/ (ax-imm-data imm13 op) 2)])
        (emit-code (op rs1 rs2 imm13 code*)
                   [25 (fxlogor (fxsll (fxlogand (ash imm -11) #b1) 6)
                                (fxlogand (ash imm -4) #b111111))]
                   [20 (ax-ea-reg-code rs2)]
                   [15 (ax-ea-reg-code rs1)]
                   [12 funct3]
                   [7 (fxlogor (fxsll (fxlogand imm #b1111) 1)
                               (fxlogand (ash imm -10) #b1))]
                   [0 opcode]))))

  (define jal-op
    (lambda (op opcode dest imm21 code*)
      (safe-assert (fxeven? (ax-imm-data imm21 op))
                   (signed21? (ax-imm-data imm21 op)))
      (let ([imm (/ (ax-imm-data imm21 op) 2)])
        (emit-code (op dest imm21 code*)
                   [31 (fxlogand (ash imm -19) #b1)]
                   [21 (fxlogand imm #b1111111111)]
                   [20 (fxlogand (ash imm -10) #b1)]
                   [12 (fxlogand (ash imm -11) #xFF)]
                   [7 (ax-ea-reg-code dest)]
                   [0 opcode]))))

  (define counter-op
    (lambda (op opcode csr dest code*)
      (emit-code (op dest code*)
                 [20 csr]
                 [15 #b00000]
                 [12 #b010] ; CSRRS
                 [7 (ax-ea-reg-code dest)]
                 [0 opcode])))

  (define fence-op
    (lambda (op opcode funct3 code*)
      (emit-code (op code*)
                 [0 #b00001111111100000000000000001111])))

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
    (lambda (ea op)
      (if (pair? ea)
          (record-case ea
            [(imm) (n) n]
            [else ($oops 'assembler-internal "ax-imm-data ea=~s ~s" ea op)])
          (if (number? ea)
              ea
              ($oops 'assembler-internal "ax-imm-data ea=~s ~s" ea op)))))

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
       (and (memq (datum x) '(long quad)) (integer? (datum e)))
       (quote (x . e))]
      [(_ x e)
       (memq (datum x) '(long quad))
       (cons 'x e)]))

  (define-syntax byte-fields
    (syntax-rules ()
      [(byte-fields (n e) ...)
       (andmap fixnum? (datum (n ...)))
       (fx+ (bitwise-arithmetic-shift-left e n) ...)]))

  (define shamt?
    (lambda (imm)
      (and (fixnum? imm) (fx<= 0 imm (expt 2 6))))) ;; RV64I supports shift up to 2^6
  (define signed12?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 11))
                              imm
                              (fx- (expt 2 11) 1)))))
  (define signed13?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 12))
                               imm
                               (fx- (expt 2 12) 1)))))
  (define signed20?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 19))
                               imm
                               (fx- (expt 2 19) 1)))))
  (define signed21?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 20))
                               imm
                               (fx- (expt 2 20) 1)))))
  (define signed32?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 31))
                               imm
                               (fx- (expt 2 31) 1)))))
  (define jump-disp?
    (lambda (x)
      (and (fixnum? x)
           (fx<= (fx- (expt 2 31)) x (fx- (expt 2 31) 1))
           (not (fxlogtest x #b11))))) ;; 4-byte aligned
  (define cond-jump-disp?
    (lambda (x)
      (and (fixnum? x)
           (fx<= (fx- (expt 2 12))
                 x
                 (fx- (expt 2 12) 1))))) ;; 13 bits
  ;; see RISC-V ABI
  (define upper20
    (lambda (x)
      (ash (fx+ x #x800) -12)))
  (define lower12
    (lambda (x)
      (fx- x (ash (upper20 x) 12))))

  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm riscv64-abs riscv64-jump riscv64-call) 0]
        [(byte) 1]
        [(word) 2]
        [(long) 4]
        [else 8])))

  (define asm-add
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (record-case src1
                [(imm) (n) (emit addi dest src0 n code*)]
                [else (emit add dest src0 src1 code*)]))))

  ;; carry if dest < src0 (or src1)
  (define asm-add/carry
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit addi %scratch0 src0 0
                    (emit add dest src0 src1
                          (emit sltu %cond dest %scratch0 code*))))))

  ;; ovfl detection logic borrowed from Racket's pb interpreter
  (define sign-flip
    (lambda (r a b code*)
      (emit xor a a b
            (emit xori b b -1
                  (emit xor b b r
                        (emit or a a b
                              (emit xori a a -1
                                    (emit srli %cond a 63 code*))))))))

  ;; src0 src1 dest
  ;; +    +    -
  ;; -    -    +
  (define asm-add/ovfl ;;@ todo imm? optimization?
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit addi %scratch0 src0 0
                    (emit addi %scratch1 src1 0
                          (emit add dest src0 src1
                                (sign-flip dest %scratch0 %scratch1 code*)))))))

  (define asm-sub
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub dest src0 src1 code*))))

  ;; dest src0 src1
  ;; -    +    -
  ;; +    -    +
  (define asm-sub/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit addi %scratch0 src0 0
                    (emit addi %scratch1 src1 0
                          (emit sub dest src0 src1
                                (emit xori %scratch1 %scratch1 -1
                                      (sign-flip dest %scratch0 %scratch1 code*))))))))

  (define asm-sub/eq
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub dest src0 src1
                    (emit sltiu %cond dest 1 code*))))) ;; set if dest == 0

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit mul dest src0 src1 code*))))

  ;; case1: pos*pos=neg
  ;; case2: neg*pos=pos or pos*neg=pos
  ;; case3: neg*neg=neg
  (define asm-mul/ovfl
    (lambda (code* dest src0 src1 t1 t2)
      (Trivit (dest src0 src1 t1 t2)
              (let ([zero `(reg . ,%real-zero)]
                    [cond `(reg . ,%cond)]
                    [t0 %scratch0])
                (emit addi t1 src0 0
                      (emit addi t2 src1 0
                            (emit mul dest src0 src1
                                  ;; if src0 == 0 || src1 == 0, then no ovfl
                                  (emit sltiu t0 t1 1
                                        (emit sltiu cond t2 1
                                              (emit or cond t0 cond
                                                    (emit bne cond zero 20
                                                          ;; if dest/src0 != src1, ovfl!
                                                          (emit div t0 dest t1
                                                                (emit beq t0 t2 12
                                                                      (emit addi cond zero 1
                                                                            (emit jal zero 8
                                                                                  (emit addi cond zero 0 code*))))))))))))))))

  (define asm-div
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit div dest src0 src1 code*))))

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
          (define asm-sll (gen-op sll slli))
          (define asm-srl (gen-op srl srli))
          (define asm-sra (gen-op sra srai)))

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
                   (emit flw %flreg2 base offset
                         (emit fcvt.d.s flreg %flreg2 '() code*))]
                  [(load-double->single)
                   (emit fld %flreg2 base offset
                         (emit fcvt.s.d flreg %flreg2 '() code*))]
                  [else (sorry! who "unrecognized op ~s" op)])))))

  (define-who asm-fl-store/cvt
    (lambda (op flreg)
      (lambda (code* base offset)
        (Trivit (base offset)
                (case op
                  [(store-single->double)
                   (emit fcvt.d.s %flreg2 flreg '()
                         (emit fsd %flreg2 base offset code*))]
                  [else (sorry! who "unrecognized op ~s" op)])))))

  (define-who asm-fl-load/store
    (lambda (op flreg)
      (lambda (code* base offset)
        (Trivit (base offset)
                (case op
                  [(load-single) (emit flw flreg base offset code*)]
                  [(load-double) (emit fld flreg base offset code*)]
                  [(store-single) (emit fsw flreg base offset code*)]
                  [(store-double) (emit fsd flreg base offset code*)]
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
                           [(integer-64 unsigned-64) (emit ld dest base n code*)]
                           [(integer-32) (emit lw dest base n code*)]
                           [(unsigned-32) (emit lwu dest base n code*)]
                           [(integer-16) (emit lh dest base n code*)]
                           [(unsigned-16) (emit lhu dest base n code*)]
                           [(integer-8) (emit lb dest base n code*)]
                           [(unsigned-8) (emit lbu dest base n code*)]
                           [else (sorry! who "unexpected mref type ~s" type)])]
                        [(eqv? n 0) ;; maybe (Trivit index)
                         (case type
                           [(integer-64 unsigned-64) (emit add %scratch0 base index
                                                           (emit ld dest %scratch0 0 code*))]
                           [(integer-32) (emit add %scratch0 base index
                                               (emit lw dest %scratch0 0 code*))]
                           [(unsigned-32) (emit add %scratch0 base index
                                                (emit lwu dest %scratch0 0 code*))]
                           [(integer-16) (emit add %scratch0 base index
                                               (emit lh dest %scratch0 0 code*))]
                           [(unsigned-16) (emit add %scratch0 base index
                                                (emit lhu dest %scratch0 0 code*))]
                           [(integer-8) (emit add %scratch0 base index
                                              (emit lb dest %scratch0 0 code*))]
                           [(unsigned-8) (emit add %scratch0 base index
                                               (emit lbu dest %scratch0 0 code*))]
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
                           [(integer-64 unsigned-64) (emit sd src base n code*)]
                           [(integer-32 unsigned-32) (emit sw src base n code*)]
                           [(integer-16 unsigned-16) (emit sh src base n code*)]
                           [(integer-8 unsigned-8) (emit sb src base n code*)]
                           [else (sorry! who "unexpected mref type ~s" type)])]
                        [(eqv? n 0)
                         (case type
                           [(integer-64 unsigned-64) (emit add %scratch0 base index
                                                           (emit sd src %scratch0 0 code*))]
                           [(integer-32 unsigned-32) (emit add %scratch0 base index
                                                           (emit sw src %scratch0 0 code*))]
                           [(integer-16 unsigned-16) (emit add %scratch0 base index
                                                           (emit sh src %scratch0 0 code*))]
                           [(integer-8 unsigned-8) (emit add %scratch0 base index
                                                         (emit sb src %scratch0 0 code*))]
                           [else (sorry! who "unexpected mref type ~s" type)])]
                        [else (sorry! who "expected zero index or 0 offset, got ~s and ~s" index offset)])))))))

  (define-who asm-flop-2
    (lambda (op)
      ;; args contain the base addr of fp numbers, thus need the fp disp
      (lambda (code* src1 src2 dest)
        (Trivit (src1 src2 dest)
                (let ([disp (constant flonum-data-disp)])
                  (emit fld %flreg1 src1 disp
                        (emit fld %flreg2 src2 disp
                              (let ([code* (emit fsd %flreg3 dest disp code*)])
                                (case op
                                  [(fl+) (emit fadd.d %flreg3 %flreg1 %flreg2 code*)]
                                  [(fl-) (emit fsub.d %flreg3 %flreg1 %flreg2 code*)]
                                  [(fl*) (emit fmul.d %flreg3 %flreg1 %flreg2 code*)]
                                  [(fl/) (emit fdiv.d %flreg3 %flreg1 %flreg2 code*)]
                                  [else (sorry! who "unrecognized op ~s" op)])))))))))

  ;; load, calc, store
  (define asm-flsqrt
    (lambda (code* src dest)
      (Trivit (src dest)
              (let ([disp (constant flonum-data-disp)])
                (emit fld %flreg1 src disp
                      (emit fsqrt.d %flreg1 %flreg1 '()
                            (emit fsd %flreg1 dest disp code*)))))))

  ;; flonum to fixnum
  (define asm-trunc
    (lambda (code* dest src)
      (Trivit (dest src)
              (let ([disp (constant flonum-data-disp)])
                (emit fld %flreg1 src disp
                      (emit fcvt.l.d dest %flreg1 '() code*))))))

  ;; fixnum to flonum
  (define asm-flt
    (lambda (code* src dest)
      (Trivit (src dest) ;;@ todo flonumreg holds addr?
              (let ([disp (constant flonum-data-disp)])
                (emit fcvt.d.l %flreg1 src '() ;; '() placeholder
                      (emit fsd %flreg1 dest disp code*))))))

  ;; big <-> little-endian
  (define-who asm-swap
    (lambda (type)
      (rec asm-swap-internal
           (lambda (code* dest src)
             (let ([t0 %scratch0])
               (Trivit (dest src)
                       ;; Hopefully every RISC-V CPU will implement the B extension.
                       (define dance
                         (lambda (right left code*)
                           (emit srli t0 src right
                                 (emit andi t0 t0 #xff
                                       (if (= left 0)
                                           (emit or dest dest t0 code*)
                                           (emit slli t0 t0 left
                                                 (emit or dest dest t0 code*)))))))
                       (case type
                         [(integer-16)
                          ;; 1st byte
                          (emit andi t0 src #xff
                                (emit slli t0 t0 56
                                      (emit srai dest t0 48
                                            ;; 2nd byte
                                            (dance 8 0 code*))))]
                         [(unsigned-16)
                          (emit andi t0 src #xff
                                (emit slli dest t0 8
                                      (dance 8 0 code*)))]
                         [(integer-32)
                          ;; 1st byte
                          (emit andi t0 src #xff
                                (emit slli t0 t0 56
                                      (emit srai dest t0 32
                                            ;; 2nd and so on
                                            (dance 8 16
                                                   (dance 16 8
                                                          (dance 24 0 code*))))))]
                         [(unsigned-32)
                          ;; 1st byte
                          (emit andi t0 src #xff
                                (emit slli dest t0 24
                                      ;; 2nd and so on
                                      (dance 8 16
                                             (dance 16 8
                                                    (dance 24 0 code*)))))]
                         [(integer-64 unsigned-64)
                          (emit andi t0 src #xff
                                (emit slli dest t0 56
                                      (dance 8 48
                                             (dance 16 40
                                                    (dance 24 32
                                                           (dance 32 24
                                                                  (dance 40 16
                                                                         (dance 48 8
                                                                                (dance 56 0 code*)))))))))]
                         [else (sorry! who "unexpected asm-swap type argument ~s" type)])))))))

  (define asm-lock ;;@ check operands of sc.d, see if can be used in both places
    ;;    lr.d tmp, [addr]
    ;;    bne  tmp, %real-zero, L
    ;;    addi tmp, %real-zero, 1
    ;;    sc.d tmp, tmp, [addr]
    ;;L:
    (lambda (code* addr t0)
      (Trivit (addr t0)
              (emit lr.d t0 addr '()
                    (emit bne t0 %real-zero 12
                          (emit addi t0 %real-zero 1
                                (emit sc.d t0 addr t0 code*)))))))

  (define-who asm-lock+/-
    ;;S:
    ;;    lr.d t0, [addr]
    ;;    addi t0, t0, +/-1
    ;;    sc.d t1, t0, [addr]
    ;;    bne  t1, %real-zero, S[-12]
    ;;    sltiu %cond t0 1 # set %cond if t0=0
    (lambda (op)
      (lambda (code* addr t0 t1)
        (Trivit (addr t0 t1)
                (emit lr.d t0 addr '()
                      (let ([code* (emit sc.d t1 addr t0
                                         (emit bne t1 %real-zero -12
                                               (emit sltiu %cond t0 1 code*)))])
                        (case op
                          [(locked-incr!) (emit addi t0 t0 1 code*)]
                          [(locked-decr!) (emit addi t0 t0 -1 code*)]
                          [else (sorry! who "unexpected op ~s" op)])))))))

  (define asm-cas
    ;;cas:
    ;;   lr.d t0, addr
    ;;   bne  t0, old, L[12]
    ;;   sc.d t1, new, [addr] # t1!=0 if store fails
    ;;   sltiu %cond t1 1    # %cond=1 if t1=0(succeed)
    ;;L:
    (lambda (code* addr old new t0 t1)
      (Trivit (addr old new t0 t1)
              (emit lr.d t0 addr '()
                    (emit bne t0 old 16
                          (emit sc.d t1 addr new
                                (emit sltiu %cond t1 1
                                      (emit jal %real-zero 8
                                            (emit xor %cond %cond %cond code*)))))))))

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
                                                   (emit sltiu %cond %scratch0 1 '()))]
                                      [(u<) (emit sltu %cond x y '())]
                                      [(<) (emit slt %cond x y '())]
                                      [(>) (emit slt %cond y x '())]
                                      [(<=) (emit slt %scratch0 y x
                                                  (emit xori %cond %scratch0 1 '()))]
                                      [(>=) (emit slt %scratch0 x y
                                                  (emit xori %cond %scratch0 1 '()))]
                                      [(logtest) (emit and %cond x y '())]
                                      [(log!test) (emit and %scratch0 x y
                                                        (emit sltiu %cond %scratch0 1 '()))])
                                    (asm-conditional-jump info l1 l2 offset))]
                                  [(imm) (n)
                                   (values
                                    (case op
                                      [(eq?) (emit xori %scratch0 x n
                                                   (emit sltiu %cond %scratch0 1 '()))]
                                      [(u<) (emit sltiu %cond x n '())]
                                      [(<) (emit slti %cond x n '())]
                                      [(logtest) (emit andi %cond x n '())]
                                      [(log!test) (emit andi %scratch0 x n
                                                        (emit sltiu %cond %scratch0 1 '()))]
                                      [else (bad!)])
                                    (asm-conditional-jump info l1 l2 offset))])))))))

  (define-who asm-fl-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
                (values
                 (let ([disp (constant flonum-data-disp)])
                   (emit fld %flreg1 x disp
                         (emit fld %flreg2 y disp
                               (let ([op (info-condition-code-type info)])
                                 (case op
                                   [(fl=) (emit feq.d %cond %flreg1 %flreg2 '())]
                                   [(fl<) (emit flt.d %cond %flreg1 %flreg2 '())]
                                   [(fl<=) (emit fle.d %cond %flreg1 %flreg2 '())]
                                   [else (sorry! who "unrecognized op ~s" op)])))))
                 (asm-conditional-jump info l1 l2 offset))))))

  (define asm-read-performance-monitoring-counter
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit addi dest %real-zero 0 code*))))

  (define asm-read-time-stamp-counter
    (lambda (code* dest)
      (Trivit (dest)
              (emit rdtime dest code*))))

  (define-who inc-cc-helper
    (lambda (val t code*)
      (nanopass-case (L16 Triv) val
                     [(immediate ,imm) (emit addi t t imm code*)]
                     [,x (emit add t t x code*)]
                     [else (sorry! who "unexpected val format ~s" val)])))

  (define asm-inc-cc-counter ;; load, add, store back
    (case-lambda
     [(code* base offset val t)
      (Trivit (base offset t)
              (emit ld t base offset
                    (inc-cc-helper val t
                                   (emit sd t base offset code*))))]
     [(code* addr val t)
      (Trivit (addr t)
              (emit ld t addr 0
                    (inc-cc-helper val t
                                   (emit sd t addr 0 code*))))]))

  (define asm-fence
    (lambda (code*)
      (emit fence code*)))

  (define ax-mov32
    (lambda (dest n code*)
      (let ([upper (upper20 n)]
            [lower (lower12 n)])
        (safe-assert (signed20? upper))
        (emit lui dest upper
              (emit addi dest dest lower
                    (aop-cons* `(asm "long:" ,n) code*))))))

  (define ax-mov64
    (lambda (dest n code*)
      (emit auipc dest 0
            (emit ld dest dest 12
                  (emit jal %real-zero 12
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
                  [(reg) ignore (emit addi dest src 0 code*)]
                  [(imm) (n)
                   (if (signed12? n)
                       (emit addi dest %real-zero n code*)
                       (if (signed32? n)
                           (ax-mov32 dest n code*)
                           (ax-mov64 dest n code*)))]
                  [(literal) stuff
                   (ax-mov64 dest 0
                             (asm-helper-relocation code* (cons 'riscv64-abs stuff)))]
                  [(disp) (n breg)
                   (safe-assert (signed12? n))
                   (emit ld dest breg n code*)]
                  [(index) (n ireg breg) ;;@ todo check this; result usually won't exceed 2^31
                   (safe-assert (eqv? n 0))
                   (emit add %scratch0 ireg breg
                         (emit ld dest %scratch0 0 code*))]
                  [else (bad!)])]
               [(ax-reg? src)
                (record-case dest
                  [(disp) (n breg)
                   (safe-assert (signed12? n))
                   (emit sd src breg n code*)]
                  [(index) (n ireg breg)
                   (safe-assert (eqv? n 0))
                   (emit add %scratch0 ireg breg
                         (emit sd src %scratch0 0 code*))]
                  [else (bad!)])]
               [else (bad!)]))))

  (define-who asm-move/extend ;;@ todo simplify some of them
    (lambda (op)
      (lambda (code* dest src)
        (Trivit (dest src)
                (case op
                  [(sext8) (emit slli dest src 56
                                 (emit srai dest dest 56 code*))]
                  [(sext16) (emit slli dest src 48
                                  (emit srai dest dest 48 code*))]
                  [(sext32) (emit slli dest src 32
                                  (emit srai dest dest 32 code*))]
                  [(zext8) (emit slli dest src 56
                                 (emit srli dest dest 56 code*))]
                  [(zext16) (emit slli dest src 48
                                  (emit srli dest dest 48 code*))]
                  [(zext32) (emit slli dest src 32
                                  (emit srli dest dest 32 code*))]
                  [else (sorry! who "unexpected op ~s" op)])))))

  (define asm-save-flrv
    (lambda (code*)
      (let ([sp (cons 'reg %sp)]) ;;@ todo check if offset is aligned
        (emit addi sp sp -16
              (emit fsd %Cfpretval sp 0 code*)))))

  (define asm-restore-flrv
    (lambda (code*)
      (let ([sp (cons 'reg %sp)])
        (emit fld %Cfpretval sp 0
              (emit addi sp sp 16 code*)))))

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
                                        (emit auipc dest (upper20 disp)
                                              (emit addi dest dest (lower12 disp) '())))]
                               [else #f])))]
                         [else #f])
                        ;;@ (label-ref ...) is processed by (make-funcrel) in (Trivit-rand), that is, into a 'literal form
                        (asm-move '() dest (with-output-language (L16 Triv) `(label-ref ,l ,incr-offset)))))))

  ;;@ todo use auipc + addi
  (define-who asm-jump
    (lambda (l next-addr)
      (make-gchunk l next-addr
                   (cond
                    [(local-label-offset l) =>
                     (lambda (offset)
                       (let ([disp (fx- next-addr offset)])
                         (cond
                          [(eqv? disp 0) '()]
                          ;;@ todo add jal for 12-bit offset?
                          [(jump-disp? disp) (emit auipc %jump (upper20 (fx+ disp 8))
                                                   ;;@ todo disp is recomputed in munge? yes
                                                   ;; 8 is obtained from debugging
                                                   (emit jalr %real-zero %jump (lower12 (fx+ disp 8)) '()))]
                          [else (sorry! who "disp value not supported")])))]
                    [else
                     ;; label must be somewhere above.  generate something so that a hard loop
                     ;; doesn't get dropped.  this also has some chance of being the right size
                     ;; for the final branch instruction.
                     (emit auipc %jump 0
                           (emit jalr %real-zero %jump 0 '()))]))))

  ;; reverse case not handled
  ;; It is, I guess, used when the first operand is
  ;; imm but the arch has no insn to have the first
  ;; operand as imm, so we have to encode the imm as
  ;; the second operand, hence the condition needs
  ;; to be reversed.
  (define asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      ;; normally:
      ;; [cond jump l1]
      ;; l2:           # disp2 = 0
      ;;     ...
      ;; l1:
      ;;     ...
      ;; inverted:
      ;; [inverted cond jump l2]
      ;; l1:           # disp1 = 0
      ;;    ...
      ;; l2:
      ;;    ...
      ;; no disp is 0:
      ;;    [cond jump to 1f]
      ;;    [jmp l2]
      ;; 1: [jmp l1]
      ;;     ...
      ;; l2:
      ;;     ...
      ;; l1:
      ;;     ...
      (define get-disp
        (lambda (next-addr l)
          (cond
           [(and (local-label? l) (local-label-offset l)) =>
            (lambda (offset)
              (let ([disp (fx- next-addr offset)])
                (safe-assert (signed32? disp))
                disp))] ;;@ not using (label ...) as in others
           [else 0])))
      (safe-assert (and (local-label? l1) (local-label? l2)))
      (make-cgchunk info l1 l2 next-addr
                    (let ([disp1 (get-disp next-addr l1)]
                          [disp2 (get-disp next-addr l2)])
                      (cond
                       ;; inverted
                       [(fx= disp1 0)
                        (safe-assert (signed32? disp2))
                        (if (or (and (fx<= 0 (fx+ disp2 4) (fx- (expt 2 11) 1)))
                                (and (fx<= (fx- (expt 2 12)) (fx+ disp2 4) 0)))
                            (emit beq %cond %real-zero (fx+ disp2 4) '())
                            (emit beq %cond %real-zero 8
                                  (emit jal %real-zero 12 ;; fall through
                                        (emit auipc %jump (upper20 (fx+ disp2 8)) ;; 2 instr below
                                              (emit jalr %real-zero %jump (lower12 (fx+ disp2 8)) '())))))]
                       ;; normal
                       [(fx= disp2 0)
                        (safe-assert (signed32? disp1))
                        (if (or (and (fx<= 0 (fx+ disp1 4) (fx- (expt 2 11) 1)))
                                (and (fx<= (fx- (expt 2 12)) (fx+ disp1 4) 0)))
                            (emit bne %cond %real-zero (fx+ disp1 4) '())
                            (emit bne %cond %real-zero 8
                                  (emit jal %real-zero 12 ;; fall through
                                        (emit auipc %jump (upper20 (fx+ disp1 8)) ;; 2 instr below
                                              (emit jalr %real-zero %jump (lower12 (fx+ disp1 8)) '())))))]
                       ;; others
                       [else
                        (safe-assert (signed32? (fx+ disp1 8)) (signed32? (fx+ disp2 16)))
                        (emit bne %cond %real-zero 12
                              (emit auipc %jump (upper20 (fx+ disp2 16)) ;; 4 instr below
                                    (emit jalr %real-zero %jump (lower12 (fx+ disp2 16))
                                          (emit auipc %jump (upper20 (fx+ disp1 8)) ;; 2 instr below
                                                (emit jalr %real-zero %jump (lower12 (fx+ disp1 8)) '())))))])))))

  (define asm-direct-jump
    (lambda (l offset)
      (asm-helper-jump '() (make-funcrel 'riscv64-jump l offset))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
                       `(riscv64-jump ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info))))))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
                       `(riscv64-jump ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec save-ra?)
      (let ([target `(riscv64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
             (lambda (code* dest tmp . ignore) ;; retval setup by intrinsics
               (asm-helper-call code* target save-ra? tmp))))))

  (define asm-library-call!
    (lambda (libspec save-ra?)
      (let ([target `(riscv64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
             (lambda (code* tmp . ignore)
               (asm-helper-call code* target save-ra? tmp))))))

  ;; for things like split-and-resize, handle-apply-overflood, Sreturn, foreign-entry
  ;; where no retval is needed
  (define asm-c-simple-call
    (lambda (entry save-ra?)
      (let ([target `(riscv64-call 0 (entry ,entry))])
        (rec asm-c-simple-call-internal
             (lambda (code* tmp . ignore)
               (asm-helper-call code* target save-ra? tmp))))))

  (define-who asm-indirect-call
    (lambda (code* dest . ignore)
      (Trivit (dest)
              (unless (ax-reg? dest) (sorry! who "unexpected dest ~s" dest))
              (emit jalr %ra dest 0 code*))))

  (define-who asm-indirect-jump ;; no link
    (lambda (src)
      (Trivit (src)
              (record-case src
                [(reg) ignore (emit jalr %real-zero src 0 '())]
                ;; the following two load EA from memory first
                [(disp) (n breg)
                 (safe-assert (signed12? n))
                 (emit ld %jump breg n
                       (emit jalr %real-zero %jump 0 '()))]
                [(index) (n ireg breg)
                 (safe-assert (eqv? n 0))
                 (emit add %scratch1 ireg breg
                       (emit ld %jump %cond 0
                             (emit jalr %real-zero %jump 0 '())))]
                [else (sorry! who "unexpected src ~s" src)]))))

  (define asm-get-tc
    (let ([target `(riscv64-call 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* dest tmp . ignore) ;; retval is put into %Cretval automatically
        (asm-helper-call code* target #f tmp))))

  (define asm-activate-thread
    (let ([target `(riscv64-call 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* dest tmp . ignore) ; dest is ignored, since it is always Cretval
        (asm-helper-call code* target #f tmp))))

  (define asm-deactivate-thread
    (let ([target `(riscv64-call 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code* tmp . ignore)
        (asm-helper-call code* target #f tmp))))

  (define asm-unactivate-thread
    (let ([target `(riscv64-call 0 (entry ,(lookup-c-entry unactivate-thread)))])
      (lambda (code* tmp . ignore)
        (asm-helper-call code* target #f tmp))))

  (define asm-push
    (lambda (code* x)
      (Trivit (x)
              (emit addi %sp %sp -16
                    (emit sd x %sp 0 code*)))))

  (define asm-pop
    (lambda (code* dest)
      (Trivit (dest)
              (emit ld dest %sp 0
                    (emit addi %sp %sp 16 code*)))))

  (define asm-enter values)

  (define asm-return
    (lambda ()
      (emit jalr %real-zero %ra 0 '())))

  (define asm-c-return
    (lambda (info)
      (emit jalr %real-zero %ra 0 '())))

  ;; no need to save ra
  ;; eed saving %cond? No need since it's always clobbered before use.
  (define asm-helper-jump
    (lambda (code* reloc)
      (emit auipc %jump 0
            (emit ld %jump %jump 12
                  (emit jal %real-zero 12
                        (cons* `(quad . 0)
                               (aop-cons* `(asm "quad jump addr")
                                          (emit jalr %real-zero %jump 0
                                                (asm-helper-relocation code* reloc)))))))))

  ;; need to save ra
  (define asm-helper-call
    (lambda (code* reloc save-ra? tmp)
      (define maybe-save-ra
        (lambda (code* p)
          (if save-ra?
              (emit addi %sp %sp -16 ;; save %ra on the stack
                    (emit sd %ra %sp 0
                          (p (emit ld %ra %sp 0
                                   (emit addi %sp %sp 16 code*)))))
              (p code*))))
      (maybe-save-ra code*
                     (lambda (code*)
                       (emit auipc tmp 0
                             (emit ld tmp tmp 12
                                   (emit jal %real-zero 12
                                         (cons* `(quad . 0)
                                                (aop-cons* `(asm "quad call addr")
                                                           (emit jalr %ra tmp 0
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
