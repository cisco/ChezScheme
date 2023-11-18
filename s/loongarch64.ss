(define-registers
  (reserved
   [%tc  %r22 #t 22 uptr]
   [%sfp %r23 #t 23 uptr]
   [%ap  %r24 #t 24 uptr]
   [%trap %r25 #t 25 uptr])
  (allocable
   [%ac0 %r26 #t 26 uptr]
   [%xp  %r27 #t 27 uptr]
   [%ts  %r28 #t 28 uptr]
   [%td  %r29 #t 29 uptr]
   ;; [%ac1 %r18 %deact #f 18]
   ;; [%yp  %r19 #f 19]
   [%cp  %r30 #t 30 uptr]
   [     %r4  %Carg1 %Cretval #f 4 uptr]
   [     %r5  %Carg2 #f 5 uptr]
   [     %r6  %Carg3 #f 6 uptr]
   [     %r7  %Carg4 #f 7 uptr]
   [     %r8  %Carg5 #f 8 uptr]
   [     %r9  %Carg6 #f 9 uptr]
   [     %r10 %Carg7 #f 10 uptr]
   [     %r11 %Carg8 #f 11 uptr]
   ;; [     %r12 #f 12]
   ;; [     %r13 #f 13]
   [     %r14 #f 14 uptr]
   [     %r15 #f 15 uptr]
   [     %r16 #f 16 uptr]
   [     %r17 #f 17 uptr]
   ;; [     %r2  #f 2] ; tp, unallocatable
   ;; [     %r18 #f 18]
   [     %r19 #f 19 uptr]
   [     %r20 #f 20 uptr]
   ;; [     %r20 #t 20]
   ;; [     %r21 #t 21] ; reserved
   ;; [     %r22 #t 22]
   ;; [     %r23 #t 23]
   ;; [     %r24 #t 24]
   ;; [     %r25 #t 25]
   ;; [     %r26 #t 26]
   ;; [     %r27 #t 27]
   ;; [     %r28 #t 28]
   ;; [     %r29 #t 29]
   ;; [     %r30 #t 30]
   [     %r31 #t 31 uptr]
   [%fp1 %f12 #f 12 fp]
   [%fp2 %f13 #f 13 fp]
   [%fp3 %f14 #f 14 fp]
   [%fp4 %f15 #f 15 fp]
   [%fp5 %f16 #f 16 fp]
   [%fp6 %f17 #f 17 fp]
   )
  (machine-dependent
   [%real-zero %r0 #f 0 uptr]
   [%ra %r1 #f 1 uptr]
   [%sp %r3 #t 3 uptr]
   [%scratch0 %jump %r12 #f 12 uptr]
   [%scratch1 %r18 #f 18 uptr]
   [%cond %r13 #f 13 uptr]
   [%Cfparg1 %Cfpretval %f0 #f 0 fp]
   [%Cfparg2 %f1 #f 1 fp]
   [%Cfparg3 %f2 #f 2 fp]
   [%Cfparg4 %f3 #f 3 fp]
   [%Cfparg5 %f4 #f 4 fp]
   [%Cfparg6 %f5 #f 5 fp]
   [%Cfparg7 %f6 #f 6 fp]
   [%Cfparg8 %f7 #f 7 fp]
   ;; hard-coded in cpnanopass.ss
   [%flreg1  %f8 #f 8 fp]
   [%flreg2  %f9 #f 9 fp]
   [%flreg3  %f10 #f 10 fp]
   [%fpscratch  %f11 #f 11 fp]))

(module (md-handle-jump
         mem->mem
         fpmem->fpmem
         coercible?
         coerce-opnd)
  (import asm-module)

  (define mref->mref
    (lambda (a k)
      (define return
        (lambda (x0 x1 imm type)
          (safe-assert (or (eq? x1 %zero) (eqv? imm 0)))
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm, type)))))
      (nanopass-case (L15c Triv) a
                     [(mref ,lvalue0 ,lvalue1 ,imm, type)
                      (lvalue->ur lvalue0
                                  (lambda (x0)
                                    (lvalue->ur lvalue1
                                                (lambda (x1)
                                                  (cond ; either x1 is %zero or imm is 0
                                                   [(and (eq? x1 %zero) (signed12? imm))
                                                    (return x0 %zero imm type)]
                                                   [else
                                                    (let ([u (make-tmp 'mref)])
                                                      (seq
                                                       (build-set! ,u (immediate ,imm))
                                                       (if (eq? x1 %zero)
                                                           (return x0 u 0 type)
                                                           (seq
                                                            (build-set! ,u (asm ,null-info ,asm-add ,u ,x1))
                                                            (return x0 u 0 type)))))])))))])))

   (define mem->mem
     (lambda (a k)
       (cond
        [(literal@? a)
         (let ([u (make-tmp 'mem)])
           (seq
            (build-set! ,u ,(literal@->literal a))
            (k (with-output-language (L15d Lvalue) `(mref ,u ,%zero 0 uptr)))))]
        [else (mref->mref a k)])))

   (define fpmem->fpmem mem->mem)

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
         (or (and (memq 'imm12 aty*) (imm-signed12? a))
             (and (memq 'shamt aty*) (imm-shamt? a))
             (and (memq 'mem aty*) (mem? a))
             (and (memq 'fpmem aty*) (fpmem? a))))]))

  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
          [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
          [(and (memq 'fpmem aty*) (fpmem? a)) (fpmem->fpmem a k)]
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
            [else (sorry! 'coerce-opnd "unexpected uptr operand ~s" a)])]
          [(memq 'fpur aty*)
           (cond
            [(fpur? a) (k a)]
            [(fpmem? a)
             (fpmem->fpmem a
                           (lambda (a)
                             (let ([u (make-tmp 'u 'fp)])
                               (seq
                                (build-set! ,u ,a)
                                (k u)))))]
            [else
             (sorry! 'coerce-opnd "unexpected fp operand ~s" a)])]
          [else (sorry! 'coerce-opnd "cannot coerce ~s to ~s" a aty*)]))]))

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
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add/ovfl ,x ,y))])

  (define-instruction value (-)
    [(op (z ur) (x ur) (y imm12))
     (let ([n (nanopass-case (L15d Triv) y [(immediate ,imm) imm])])
       (with-output-language (L15d Effect)
                             `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x (immediate ,(- n))))))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub ,x ,y))])

  (define-instruction value (-/ovfl)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub/ovfl ,x ,y))])

  (define-instruction value (-/eq)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub/eq ,x ,y))])

  (define-instruction value (*)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,x ,y))])

  (define-instruction value (*/ovfl)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul/ovfl ,x ,y))])

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

  (define-instruction value (fpmove)
    [(op (x fpmem) (y fpur)) `(set! ,(make-live-info) ,x ,y)]
    [(op (x fpur) (y fpmem fpur)) `(set! ,(make-live-info) ,x ,y)])

  (define-instruction value (lea1) ;;@ todo addi?
    [(op (z ur) (x ur))            ;;@ z = x + offset
     (begin
       (let ([offset (info-lea-offset info)])
         (if (signed12? offset)
             `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x (immediate ,offset))) ;;@ not Trivited yet, use (immediate)
             (let ([u (make-tmp 'lea1)])
               (seq
                `(set! ,(make-live-info) ,u (immediate ,offset))
                `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,u)))))))])

  (define-instruction value (lea2)
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
                              (if (ur? w) ;; base:x index:y offset:w
                                  (if (eq? y %zero)
                                      (k x w imm-zero)
                                      (let ([u (make-tmp 'ls1)])
                                        (seq
                                         ;; w <- w + y
                                         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w))
                                         (k x u imm-zero))))
                                  (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
                                    (if (signed12? n)
                                        (if (eq? y %zero)
                                            (let ([w (in-context Triv `(immediate ,n))])
                                              (k x y w))
                                            (let ([u (make-tmp 'ls2)])
                                              (seq
                                               ;; w <- w + y
                                               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w))
                                               (k x u imm-zero))))
                                        (let ([u (make-tmp 'ls3)])
                                          (seq
                                           `(set! ,(make-live-info) ,u (immediate ,n))
                                           (if (eq? y %zero)
                                               (k x u imm-zero)
                                               (seq
                                                ;; changed the base to u instead
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
                           (let ([u (make-tmp 'u)])
                             (seq
                              `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-swap type) ,z))
                              `(asm ,null-info ,(asm-store type) ,x ,y ,w ,u)))
                           `(asm ,null-info ,(asm-store type) ,x ,y ,w ,z)))))]))

  (define-instruction effect (push)
    [(op (x ur)) `(asm ,info ,asm-push ,x)])

  (define-instruction value (pop)
    [(op (z ur)) `(set! ,(make-live-info) ,z (asm ,info ,asm-pop))])

  (define-instruction value (load-single->double)
    [(op (x fpur) (y fpmem))
     (seq
      `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove-single ,y))
      `(set! ,(make-live-info) ,x (asm ,null-info ,asm-single->double ,x)))])

  (define-instruction effect (store-double->single)
    [(op (x fpmem) (y fpur))
     (let ([u (make-tmp 'us 'fp)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-double->single ,y))
        `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove-single ,u))))])

  (define-instruction effect (store-single)
    [(op (x fpmem) (y fpur))
     `(asm ,info ,asm-fpmove-single ,x ,y)])

  (define-instruction value (load-single)
    [(op (x fpur) (y fpmem))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove-single ,y))])

  (define-instruction value (single->double)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-single->double ,y))])

  (define-instruction value (double->single)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-double->single ,y))])

  (define-instruction value (fpt)
    [(op (x fpur) (y ur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpt ,y))])

  (define-instruction value (fptrunc)
    [(op (z ur) (x fpur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-fptrunc ,x))])

  (define-instruction value (fp+ fp- fp/ fp*)
    [(op (x fpur) (y fpur) (z fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpop-2 op) ,y ,z))])

  ;; pred all return multiple values
  (define-instruction pred (fp= fp< fp<=)
    [(op (x fpur) (y fpur))
     (let ([info (make-info-condition-code op #f #f)])
       (values '() `(asm ,info ,(asm-fp-relop info) ,x ,y)))])

  (define-instruction value (fpsingle)
    [(op (x fpur) (y fpur))
     (seq
      `(set! ,(make-live-info) ,x (asm ,info ,asm-double->single ,y))
      `(set! ,(make-live-info) ,x (asm ,info ,asm-single->double ,x)))])

  ;; move bits only
  (let ()
    (define (mem->mem mem new-type)
      (nanopass-case (L15d Triv) mem
                     [(mref ,x0 ,x1 ,imm ,type)
                      (with-output-language (L15d Lvalue) `(mref ,x0 ,x1 ,imm ,new-type))]))

    (define-instruction value (fpcastto)
      [(op (x mem) (y fpur)) `(set! ,(make-live-info) ,(mem->mem x 'fp) ,y)]
      [(op (x ur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcastto ,y))])

    (define-instruction value (fpcastfrom)
      [(op (x fpmem) (y ur)) `(set! ,(make-live-info) ,(mem->mem x 'uptr) ,y)]
      [(op (x fpur) (y ur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcastfrom ,y))]))

  (define-instruction value (fpsqrt)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsqrt ,y))])

  (define-instruction effect (inc-cc-counter)
    [(op (x ur) (w imm12 ur) (z imm12 ur))
     (let ([u1 (make-tmp 'inc1)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
        `(asm ,null-info ,asm-inc-cc-counter ,x ,w ,z ,u1)))])

  (define-instruction effect (inc-profile-counter)
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
                                  (nanopass-case (L15d Triv) w
                                                 [(immediate ,imm)
                                                  (if (eqv? imm 0)
                                                      (k r)
                                                      (let ([u (make-tmp 'u)])
                                                        (seq
                                                         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,r ,w))
                                                         (k u))))]
                                                 [else
                                                  (let ([u (make-tmp 'u)])
                                                    (seq
                                                     `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,r ,w))
                                                     (k u)))])))
                              (if (eq? y %zero)
                                  (add-offset x)
                                  (let ([u (make-tmp 'u)])
                                    (seq
                                     `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,y))
                                     (add-offset u)))))))

    (define-instruction pred (lock!)
      [(op (x ur) (y ur) (w imm12))
       (values
        (lea->reg x y w
                  (lambda (r)
                    (with-output-language (L15d Effect)
                                          `(asm ,info ,asm-lock ,r))))
        ;; lock held if %scratch1 == 1
        `(asm ,info-cc-eq ,asm-eq ,%scratch1 (immediate 1)))])

    (define-instruction effect (locked-incr! locked-decr!)
      [(op (x ur) (y ur) (w imm12))
       (lea->reg x y w
                 (lambda (r)
                   `(asm ,info ,(asm-lock+/- op) ,r)))])

    (define-instruction effect (cas)
      [(op (x ur) (y ur) (w imm12 ur) (old ur) (new ur))
       (lea->reg x y w
                 (lambda (r)
                   `(asm ,info ,asm-cas ,r ,old ,new)))])
    )

  ;; TODO add hints
  (define-instruction effect (store-store-fence acquire-fence release-fence)
    [(op) `(asm ,info ,asm-fence)])

  (define-instruction effect (pause)
    [(op) `(asm ,info ,asm-fence)])

  (define-instruction effect (c-call)
    [(op (x ur))
     `(asm ,info ,asm-indirect-call ,x ,(info-kill*-live*-live* info) ...)])

  (define-instruction effect (save-flrv)
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect (restore-flrv)
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect (invoke-prelude)
    [(op) `(set! ,(make-live-info) ,%tc ,%Carg1)])

  )

(module asm-module (asm-add asm-add/carry asm-add/ovfl asm-sub asm-sub/ovfl asm-sub/eq
                    asm-mul asm-mul/ovfl asm-div asm-logand asm-logor asm-logxor asm-lognot
                    asm-read-performance-monitoring-counter asm-read-time-stamp-counter asm-inc-cc-counter
                    asm-enter asm-sll asm-srl asm-sra asm-fpsqrt asm-fptrunc asm-fpt asm-fpcastto asm-fpcastfrom
                    asm-single->double asm-double->single
                    asm-cas asm-relop asm-fp-relop asm-fpop-2 asm-save-flrv asm-restore-flrv
                    asm-direct-jump asm-indirect-jump asm-literal-jump asm-condition-code
                    asm-jump asm-conditional-jump asm-library-jump
                    asm-get-tc asm-activate-thread asm-deactivate-thread asm-unactivate-thread
                    asm-push asm-pop asm-return asm-c-return asm-kill
                    asm-load asm-store asm-fence asm-swap asm-lock asm-lock+/-
                    asm-move asm-fpmove asm-fpmove-single asm-move/extend
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

  (define-op revb.2h two-R-op #b1100)
  (define-op revb.2w two-R-op #b1110)
  (define-op revb.d  two-R-op #b1111)
  (define-op ext.w.h two-R-op #b10110)
  (define-op ext.w.b two-R-op #b10111)
  (define-op fsqrt.d two-R-op #b100010100010010)
  (define-op fcvt.d.s    two-R-op #b100011001001001)
  (define-op fcvt.s.d    two-R-op #b100011001000110)
  (define-op ffint.l.d   two-R-op #b100011011001010)
  (define-op ffint.d.l   two-R-op #b100011101001010)
  (define-op ftintrz.l.d two-R-op #b100011010101010) ; round toward zero
  (define-op movfr2gr.d  two-R-op #b100010100101110)
  (define-op movfr2gr.s  two-R-op #b100010100101101)
  (define-op movgr2fr.d  two-R-op #b100010100101010)
  (define-op rdtime.d  two-R-op #b11010)

  (define-op add.d  three-R-op #b100001)
  (define-op sub.d  three-R-op #b100011)
  (define-op mul.d  three-R-op #b111011)
  (define-op mulh.d three-R-op #b111100)
  (define-op div.d three-R-op #b1000100)
  (define-op fadd.d three-R-op #b1000000010)
  (define-op fsub.d three-R-op #b1000000110)
  (define-op fmul.d three-R-op #b1000001010)
  (define-op fdiv.d three-R-op #b1000001110)
  (define-op fmin.d three-R-op #b1000010110)
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
  ;;(define-op addi.w two-R-imm12-op #b1010)
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
  (define-op dbar sync-op #b111000011100100)

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
                 [10 (fxlogand (ax-imm-data shamt op) #b111111)]
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
      (safe-assert (or (unsigned12? (ax-imm-data imm12 op)) (signed12? (ax-imm-data imm12 op))))
      (emit-code (op dest src0 imm12 code*)
                 [22 opcode]
                 ;; need to extract bits in case negative numbers interfere with opcode
                 [10 (fxlogand (ax-imm-data imm12 op) #xFFF)]
                 [5 (ax-ea-reg-code src0)]
                 [0 (ax-ea-reg-code dest)])))

  (define two-R-imm14-op
    (lambda (op opcode dest src0 imm14 code*)
      (safe-assert (signed14? (ax-imm-data imm14 op)))
      (emit-code (op dest src0 imm14 code*)
                 [24 opcode]
                 [10 (fxlogand (ax-imm-data imm14 op) #b11111111111111)]
                 [5 (ax-ea-reg-code src0)]
                 [0 (ax-ea-reg-code dest)])))

  (define one-R-imm20-op
    (lambda (op opcode dest imm20 code*)
      (safe-assert (or (signed20? (ax-imm-data imm20 op)) (unsigned20? (ax-imm-data imm20 op))))
      (emit-code (op dest imm20 code*)
                 [25 opcode]
                 [5 (fxlogand (ax-imm-data imm20 op) #xFFFFF)]
                 [0 (ax-ea-reg-code dest)])))

  (define two-R-imm18-op
    (lambda (op opcode link src imm18 code*)
      (safe-assert (or (signed18? (ax-imm-data imm18 op)) (unsigned18? (ax-imm-data imm18 op)))
                   (code-disp? (ax-imm-data imm18 op))) ; offset in multiples of 4
      (emit-code (op link src imm18 code*)
                 [26 opcode]
                 [10 (fxlogand (fxsra (ax-imm-data imm18 op) 2) #xFFFF)]
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
             [offs0  (fxlogand imm #xFFFF)] ; offs[15:0]
             [offs16 (fxlogand (ash imm -16) #b1111111111)]) ; offs[25:16] 10 bits
        (emit-code (op imm28 code*)
                   [26 opcode]
                   [10 offs0]
                   [0  offs16]))))

  (define sync-op
    (lambda (op opcode code*)
      (emit-code (op code*)
                 [15 opcode]
                 [0 0])))

  (define-who fp-cmp-op
    (lambda (op opcode cmp src0 src1 code*)
      (emit-code (op cmp src0 src1 code*)
                 [20 opcode]
                 [15 (case cmp
                       [(fp=) #x4]
                       [(fp<) #x2]
                       [(fp<=) #x6]
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
      ($fxu< imm (expt 2 12))))
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
  (define unsigned18?
    (lambda (imm)
      ($fxu< imm (expt 2 18))))
  (define signed18?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 17))
                               imm
                               (fx- (expt 2 17) 1)))))
  (define unsigned20?
    (lambda (imm)
      ($fxu< imm (expt 2 20))))
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

  ;; for building 32-bit immediate
  (define upper20-32
    (lambda (x)
      (fxsrl (fxlogand #xFFFFF000 x) 12)))
  (define lower12-32
    (lambda (x)
      (fxlogand #xFFF x)))
  ;; for building 32-bit pc-relative addr
  (define upper20-32+pc
    (lambda (x)
      (ash (fx+ x #x800) -12)))
  (define lower12-32+pc
    (lambda (x)
      (fx- x (ash (upper20-32+pc x) 12))))
  ;; for building 38-bit pc-relative addr
  (define upper20-38
    (lambda (x)
      (ash (fx+ x #x20000) -18)))
  (define lower18-38
    (lambda (x)
      (fx- x (ash (upper20-38 x) 18))))

  (define pcaddu18i?
    (lambda (x)
      (<= (- (expt 2 37))
          x
          (- (expt 2 37) 1 #x20000))))

  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm loongarch64-abs loongarch64-jump loongarch64-call) 0]
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

  (define asm-add/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit add.d %cond src0 src1
                    (emit slt %scratch0 %cond src0
                          (emit slti %scratch1 src1 0
                                (emit addi.d dest %cond 0
                                      (emit xor %cond %scratch0 %scratch1 code*))))))))

  (define asm-sub
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub.d dest src0 src1 code*))))

  (define asm-sub/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub.d %cond src0 src1
                    (emit slt %scratch0 src0 %cond
                          (emit slti %scratch1 src1 0
                                (emit addi.d dest %cond 0
                                      (emit xor %cond %scratch0 %scratch1 code*))))))))

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
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit mulh.d %scratch1 src0 src1
                    (emit mul.d dest src0 src1
                          (emit srai.d %cond dest 63
                                (emit bne %scratch1 %cond 12
                                      (emit addi.d %cond %real-zero 0
                                            (emit b 8
                                                  (emit addi.d %cond %real-zero 1
                                                        code*))))))))))
  (define asm-div
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit div.d dest src0 src1 code*))))


  (module (asm-logand asm-logor asm-logxor asm-sll asm-srl asm-sra)
    (define-syntax gen-log-op
      (lambda (x)
        (syntax-case x ()
          [(_ op opi)
           #'(lambda (code* dest src0 src1)
               (Trivit (dest src0 src1)
                       (record-case src1
                         [(imm) (n)
                          ;; LoongArch logical ops do not sign-extend the immediate,
                          ;; so negative numbers must be loaded first
                          (if (fx< n 0)
                              (emit addi.d %scratch0 %r0 n
                                    (emit op dest src0 %scratch0 code*))
                              (emit opi dest src0 n code*))]
                         [else (emit op dest src0 src1 code*)])))])))
    (define-syntax gen-shift-op
      (lambda (x)
        (syntax-case x ()
          [(_ op opi)
           #'(lambda (code* dest src0 src1)
               (Trivit (dest src0 src1)
                       (record-case src1
                         [(imm) (n) (emit opi dest src0 n code*)]
                         [else (emit op dest src0 src1 code*)])))])))

    (define asm-logand (gen-log-op and andi))
    (define asm-logor  (gen-log-op or  ori))
    (define asm-logxor (gen-log-op xor xori))
    (define asm-sll (gen-shift-op sll.d slli.d))
    (define asm-srl (gen-shift-op srl.d srli.d))
    (define asm-sra (gen-shift-op sra.d srai.d)))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit addi.d %scratch0 %real-zero -1
                    (emit xor dest src %scratch0 code*)))))

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
                        [(integer-32)  (emit ld.w dest base n code*)]
                        [(unsigned-32) (emit ld.wu dest base n code*)]
                        [(integer-16)  (emit ld.h dest base n code*)]
                        [(unsigned-16) (emit ld.hu dest base n code*)]
                        [(integer-8)   (emit ld.b dest base n code*)]
                        [(unsigned-8)  (emit ld.bu dest base n code*)]
                        [else (sorry! who "unexpected mref type ~s" type)])]
                     [(eqv? n 0) ;; maybe (Trivit index)
                      (case type
                        [(integer-64 unsigned-64) (emit ldx.d dest base index code*)]
                        [(integer-32)  (emit ldx.w dest base index code*)]
                        [(unsigned-32) (emit ldx.wu dest base index code*)]
                        [(integer-16)  (emit ldx.h dest base index code*)]
                        [(unsigned-16) (emit ldx.hu dest base index code*)]
                        [(integer-8)   (emit ldx.b dest base index code*)]
                        [(unsigned-8)  (emit ldx.bu dest base index code*)]
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
                        [(integer-8  unsigned-8)  (emit st.b src base n code*)]
                        [else (sorry! who "unexpected mref type ~s" type)])]
                     [(eqv? n 0)
                      (case type
                        [(integer-64 unsigned-64) (emit stx.d src base index code*)]
                        [(integer-32 unsigned-32) (emit stx.w src base index code*)]
                        [(integer-16 unsigned-16) (emit stx.h src base index code*)]
                        [(integer-8  unsigned-8)  (emit stx.b src base index code*)]
                        [else (sorry! who "unexpected mref type ~s" type)])]
                     [else (sorry! who "expected zero index or 0 offset, got ~s and ~s" index offset)])))))))

  (define-who asm-fpop-2
    (lambda (op)
      (lambda (code* dest src1 src2)
        (Trivit (dest src1 src2)
                (case op
                  [(fp+) (emit fadd.d dest src1 src2 code*)]
                  [(fp-) (emit fsub.d dest src1 src2 code*)]
                  [(fp*) (emit fmul.d dest src1 src2 code*)]
                  [(fp/) (emit fdiv.d dest src1 src2 code*)]
                  [else (sorry! who "unrecognized op ~s" op)])))))

  (define-who asm-fpmove
    ;; fpmove pseudo instruction is used by set! case in
    ;; select-instructions! and generate-code; at most one of src or
    ;; dest can be an mref, and then the offset is double-aligned
    (lambda (code* dest src)
      (gen-fpmove who code* dest src #t)))

  (define-who asm-fpmove-single
    (lambda (code* dest src)
      (gen-fpmove who code* dest src #f)))

  (define gen-fpmove
    (lambda (who code* dest src double?)
      (Trivit (dest src)
              (record-case dest
                [(disp) (imm reg)
                 (if double?
                     (emit fst.d src reg imm code*)
                     (emit fst.s src reg imm code*))]
                [(index) (n ireg breg)
                 (safe-assert (signed12? n))
                 (emit add.d %scratch0 ireg breg
                       (if double?
                           (emit fst.d src %scratch0 n code*)
                           (emit fst.s src %scratch0 n code*)))]
                [else
                 (record-case src
                   [(disp) (imm reg)
                    (if double?
                        (emit fld.d dest reg imm code*)
                        (emit fld.s dest reg imm code*))]
                   [(index) (n ireg breg)
                    (safe-assert (signed12? n))
                    (emit add.d %scratch0 ireg breg
                          (if double?
                              (emit fld.d dest %scratch0 n code*)
                              (emit fld.s dest %scratch0 n code*)))]
                   ;; just move
                   [else (emit fmin.d dest src src code*)])]))))

  (define-who asm-single->double
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit fcvt.d.s dest src code*))))

  (define-who asm-double->single
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit fcvt.s.d dest src code*))))

  (define asm-fpsqrt
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit fsqrt.d dest src code*))))

  (define asm-fptrunc ;;@ flonum to fixnum
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit ftintrz.l.d %flreg1 src
                    (emit movfr2gr.d dest %flreg1 code*)))))

  (define asm-fpt ;;@ fixnum to flonum
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit movgr2fr.d %flreg1 src
                    (emit ffint.d.l dest %flreg1 code*)))))

  ;; move bit patterns between fp and int regs
  (define asm-fpcastto
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit movfr2gr.d dest src code*))))
  (define asm-fpcastfrom
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit movgr2fr.d dest src code*))))
  (define asm-fpcastto-single
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit movfr2gr.s dest src code*))))

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
    ;;    ll.d  t0, [addr], 0
    ;;    bnez  t0, L0
    ;;    addi.d t0, %real-zero, 1 # lock
    ;;    sc.d  t0, [addr], 0
    ;;    sltu  t0, %real-zero, t0 # success if t0>0 (actually ==1)
    ;;    b L1
    ;;L0:
    ;;    xor t0, t0, t0           # t0==0 when fails
    ;;L1:
    (lambda (code* addr)
      (let ([t0 %scratch1])
        (Trivit (addr)
                (emit ll.d t0 addr 0
                      (emit bnez t0 20
                            (emit addi.d t0 %real-zero 1
                                  (emit sc.d t0 addr 0
                                        (emit sltu t0 %real-zero t0
                                              (emit b 8
                                                    (emit xor t0 t0 t0 code*)))))))))))

  (define-who asm-lock+/-
    ;;S:
    ;;    ll.d   t0, [addr], 0
    ;;    addi.d t0, t0, +/-1
    ;;    add.d  t1, %real-zero, t0 # backup
    ;;    sc.d   t0, [addr], 0
    ;;    beqz   t0, S[-16]         # fail if t0==0
    ;;    sltui  %cond, t1, 1       # set %cond if t1 goes to 0
    (lambda (op)
      (lambda (code* addr)
        (Trivit (addr)
                (let ([t0 %scratch0]
                      [t1 %scratch1])
                  (emit ll.d t0 addr 0
                        (let ([code* (emit sc.d t0 addr 0
                                           (emit beqz t0 -16
                                                 (emit sltui %cond t1 1 code*)))])
                          (case op
                            [(locked-incr!) (emit addi.d t0 t0 1
                                                  (emit addi.d t1 t0 0 code*))]
                            [(locked-decr!) (emit addi.d t0 t0 -1
                                                  (emit addi.d t1 t0 0 code*))]
                            [else (sorry! who "unexpected op ~s" op)]))))))))

  (define asm-cas
    ;;cas:
    ;;   ll.d   t0, [addr], 0
    ;;   addi.d t1, new, 0
    ;;   bne    t0, old, L0
    ;;   sc.d   t1, [addr], 0
    ;;   sltu   %cond, %real-zero, t1  # success if t1>0
    ;;   b L1
    ;;L0:
    ;;   xor    %cond, %cond, %cond
    ;;L1:
    (lambda (code* addr old new)
      (Trivit (addr old new)
              (let ([t0 %scratch0]
                    [t1 %scratch1])
                (emit ll.d t0 addr 0
                      (emit addi.d t1 new 0 ; backup new
                            (emit bne t0 old 16
                                  (emit sc.d t1 addr 0
                                        (emit sltu %cond %real-zero t1
                                              (emit b 8
                                                    ;; in case %cond contains other value
                                                    (emit xor %cond %cond %cond code*)))))))))))

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
                          [(eq?)
                           (if  (< n 0)
                               (emit addi.d %scratch0 %real-zero n
                                     (emit xor %scratch1 x %scratch0
                                           (emit sltui %cond %scratch1 1 '())))
                               (emit xori %scratch0 x n
                                     (emit sltui %cond %scratch0 1 '())))]
                          [(u<) (emit sltui %cond x n '())]
                          [(<) (emit slti %cond x n '())]
                          [(logtest)
                           (if (< n 0)
                               (emit addi.d %scratch0 %real-zero n
                                     (emit and %cond x %scratch0 '()))
                               (emit andi %cond x n '()))]
                          [(log!test)
                           (if (< n 0)
                               (emit addi.d %scratch1 %real-zero n
                                     (emit and %scratch0 x %scratch1
                                           (emit sltui %cond %scratch0 1 '())))
                               (emit andi %scratch0 x n
                                     (emit sltui %cond %scratch0 1 '())))]
                          [else (bad!)])
                        (asm-conditional-jump info l1 l2 offset))])))))))

  (define-who asm-fp-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
                (values
                 (let ([op (info-condition-code-type info)])
                   (emit fcmp.d op x y      ; internally use the first flag reg
                         (emit bceqz '() 12 ; '(): placeholder
                               (emit addi.d %cond %real-zero 1
                                     (emit b 8
                                           (emit addi.d %cond %real-zero 0 '()))))))
                 (asm-conditional-jump info l1 l2 offset))))))

  (define asm-read-performance-monitoring-counter
    (lambda (code* dest src)
      (Trivit (dest src)
              (emit addi.d dest %real-zero 0 code*))))

  (define asm-read-time-stamp-counter
    (lambda (code* dest)
      (Trivit (dest)
              (emit rdtime.d dest %real-zero code*))))

  (define-who asm-inc-cc-counter ;; load, add, store back
    (lambda (code* base offset val t)
      (define-who inc-cc-helper
        (lambda (val t code*)
          (nanopass-case (L16 Triv) val
                         [(immediate ,imm) (emit addi.d t t imm code*)]
                         [,x (emit add.d t t x code*)]
                         [else (sorry! who "unexpected val format ~s" val)])))
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

  (define asm-fence
    (lambda (code*)
      (emit dbar code*)))

  (define ax-mov32
    (lambda (dest n code*)
      (let ([upper (upper20-32 n)]
            [lower (lower12-32 n)])
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
                             (asm-helper-relocation code* (cons 'loongarch64-abs stuff)))]
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
                  [(sext32) (emit slli.d dest src 32
                                  (emit srai.d dest dest 32 code*))]
                  [(zext8) (emit andi dest src #xff code*)] ; andi zero-extends the imm
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
                          (let ([incr-offset (adjust-return-point-offset incr-offset l)])
                            (lambda (offset)
                              (let ([disp (fx- next-addr (fx- offset incr-offset) -8)])
                                (cond
                                 [(signed32? disp)
                                  (Trivit (dest)
                                          (emit pcaddu12i dest (upper20-32+pc disp)
                                                ;;@ todo addi.d or ori?
                                                (emit addi.d dest dest (lower12-32+pc disp) '())))]
                                 [else #f]))))]
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
                          [(signed28? (fx+ disp 4))
                           (emit b (fx+ disp 4) '())]
                          [else (sorry! who "disp value not supported: ~a~n" disp)])))]
                    [else
                     ;; label must be somewhere above.  generate something so that a hard loop
                     ;; doesn't get dropped.  this also has some chance of being the right size
                     ;; for the final branch instruction.
                     (emit b 0 '())]))))

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
      (let ([offset (adjust-return-point-offset offset l)])
        (asm-helper-jump '() (make-funcrel 'loongarch64-jump l offset)))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
                       `(loongarch64-jump ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info))))))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
                       `(loongarch64-jump ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec save-ra?)
      (let ([target `(loongarch64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
             (lambda (code* dest tmp . ignore) ;; retval setup by intrinsics
               (asm-helper-call code* target save-ra? tmp))))))

  (define asm-library-call!
    (lambda (libspec save-ra?)
      (let ([target `(loongarch64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
             (lambda (code* tmp . ignore)
               (asm-helper-call code* target save-ra? tmp))))))

  ;; for things like split-and-resize, handle-apply-overflood, Sreturn, foreign-entry
  ;; where no retval is needed
  (define asm-c-simple-call
    (lambda (entry save-ra?)
      (let ([target `(loongarch64-call 0 (entry ,entry))])
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
    (let ([target `(loongarch64-call 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* dest tmp . ignore) ;; retval is put into %Cretval automatically
        (asm-helper-call code* target #f tmp))))

  (define asm-activate-thread
    (let ([target `(loongarch64-call 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* dest tmp . ignore) ; dest is ignored, since it is always Cretval
        (asm-helper-call code* target #f tmp))))

  (define asm-deactivate-thread
    (let ([target `(loongarch64-call 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code* tmp . ignore)
        (asm-helper-call code* target #f tmp))))

  (define asm-unactivate-thread
    (let ([target `(loongarch64-call 0 (entry ,(lookup-c-entry unactivate-thread)))])
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

  (module (asm-foreign-call asm-foreign-callable)
    ;; Exception: uninitialized %Carg1 when using direct vectors
    (define int-argument-regs (lambda () (vector %Carg1 %Carg2 %Carg3 %Carg4 %Carg5 %Carg6 %Carg7 %Carg8)))
    (define fp-argument-regs  (lambda () (vector %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4 %Cfparg5 %Cfparg6 %Cfparg7 %Cfparg8)))

    (define align
      (lambda (b x)
        (let ([k (fx- b 1)])
          (fxlogand (fx+ x k) (fxlognot k)))))

    (define-record-type cat
      (nongenerative #{cat ker6am0odx7w7wg5n44fxof76-1})
      (sealed #t)
      (fields place            ; 'int, 'fp, 'mix ('fp + 'int/'fp), or 'stack
              regs             ; list of registers
              size/s           ; size in bytes for 'stack, list of sizes for register
              offsets          ; for 'mix mode
              by-reference))   ; #f or size of referenced data

    (define categorize-arguments
      (lambda (types varargs-after)
        (let ([int-argument-regs (int-argument-regs)]
              [fp-argument-regs (fp-argument-regs)])
          (let loop ([types types] [i 0] [fpi 0] [varargs-after varargs-after])
            (let ([next-varargs-after (and varargs-after (if (fx> varargs-after 0) (fx- varargs-after 1) 0))])
              (define (fp-arg)
              ;; `double` and `float` arguments go into floating-point registers,
              ;; unless they're varargs or we've run out of fp registers; next
              ;; best is an integer register; if neither of those, on the stack
              (cond
               [(and (< fpi 8)
                     (not (eqv? varargs-after 0)))
                (cons (make-cat 'fp (list (vector-ref fp-argument-regs fpi)) '(8) #f #f)
                      (loop (cdr types) i (fx+ fpi 1) next-varargs-after))]
               [(< i 8)
                (cons (make-cat 'int (list (vector-ref int-argument-regs i)) '(8) #f #f)
                      (loop (cdr types) (fx+ i 1) fpi next-varargs-after))]
               [else
                (cons (make-cat 'stack '() 8 #f #f)
                      (loop (cdr types) i fpi next-varargs-after))]))
            (if (null? types)
                '()
                (nanopass-case (Ltype Type) (car types)
                               [(fp-double-float) (fp-arg)]
                               [(fp-single-float) (fp-arg)]
                               [(fp-ftd& ,ftd)
                                ;; A non-union with one floating-point member is treated
                                ;;  like that member by itself.
                                ;; A non-union that has exactly two members, at least one as
                                ;;  floating point, and both within word size: uses registers,
                                ;;  if available of the right sorts (= 'mix mode)
                                ;; Otherwise, a compound value of size no more than
                                ;;  a word is treated like an word-sized integer.
                                ;; A compound value of more than one word and less than
                                ;; two can use two registers, if available.
                                (let* ([size ($ftd-size ftd)]
                                       [members ($ftd->members ftd)]
                                       [type-of car]
                                       [size-of cadr]
                                       [offset-of caddr])
                                  (cond
                                   [(and (< fpi 8)
                                         (not ($ftd-union? ftd))
                                         (and (null? (cdr members))
                                              (eq? (type-of (car members)) 'float)))
                                    ;; in one fp register
                                    (cons (make-cat 'fp (list (vector-ref fp-argument-regs fpi)) (list (cadar members)) #f #f)
                                          (loop (cdr types) i (fx+ fpi 1) next-varargs-after))]
                                   [(and (< fpi 8)
                                         (not ($ftd-union? ftd))
                                         (and (pair? (cdr members))
                                              (null? (cddr members))
                                              (or (eq? (type-of (car members)) 'float)
                                                  (eq? (type-of (cadr members)) 'float))
                                              (if (and (eq? (type-of (car members)) 'float)
                                                       (eq? (type-of (cadr members)) 'float))
                                                  (< fpi 7)
                                                  (< i 8))
                                              (fx<= (size-of (car members)) 8)
                                              (fx<= (size-of (cadr members)) 8)))
                                    ;; in two fp registers or one fp and one integer
                                    (let ([t1 (type-of (car members))]
                                          [t2 (type-of (cadr members))]
                                          [s1 (size-of (car members))]
                                          [s2 (size-of (cadr members))])
                                      (let ([both-float? (and (eq? t1 'float)
                                                              (eq? t2 'float))])
                                        (cons (make-cat 'mix
                                                        (list (if (eq? t1 'float)
                                                                  (vector-ref fp-argument-regs fpi)
                                                                  (vector-ref int-argument-regs i))
                                                              (if (eq? t2 'float)
                                                                  (vector-ref fp-argument-regs (if both-float?
                                                                                                   (fx+ fpi 1)
                                                                                                   fpi))
                                                                  (vector-ref int-argument-regs i)))
                                                        (list s1 s2)
                                                        (list (offset-of (car members))
                                                              (offset-of (cadr members)))
                                                        #f)
                                              (loop (cdr types)
                                                    (if both-float? i (fx+ i 1))
                                                    (if both-float? (fx+ fpi 2) (fx+ fpi 1))
                                                    next-varargs-after))))]
                                   [(and (> size (constant ptr-bytes))
                                         (<= size (* 2 (constant ptr-bytes)))
                                         (<= i 6))
                                    ;;@ 8 < size <= 16
                                    ;; in two int registers (and never split across registers and stack?)
                                    (cons (make-cat 'int (list (vector-ref int-argument-regs i)
                                                               (vector-ref int-argument-regs (fx+ i 1)))
                                                    (list 8 (fx- size 8))
                                                    #f
                                                    #f)
                                          (loop (cdr types) (fx+ i 2) fpi next-varargs-after))]
                                   [(and (or (<= size (constant ptr-bytes))
                                             (> size (* 2 (constant ptr-bytes))))
                                         (< i 8))
                                    ;; in one int register, by-reference if more than size 16
                                    (let ([by-ref? (> size (* 2 (constant ptr-bytes)))])
                                      (cons (make-cat 'int (list (vector-ref int-argument-regs i))
                                                      (if by-ref? '(8) (list size))
                                                      #f
                                                      (and by-ref?
                                                           size))
                                            (loop (cdr types) (fx+ i 1) fpi next-varargs-after)))]
                                   [else
                                    ;; on stack, and by-reference if more than size 16
                                    (let ([stk-size (if (<= size (* 2 (constant ptr-bytes)))
                                                        (align 8 size)
                                                        8)])
                                      (cons (make-cat 'stack '() stk-size #f (and (> size (* 2 (constant ptr-bytes)))
                                                                                  size))
                                            (loop (cdr types) i fpi next-varargs-after)))]))]
                               [else
                                ;; integers, scheme-object, etc. --- always in a register,
                                ;; unless we've run out
                                (cond
                                 [(>= i 8)
                                  (cons (make-cat 'stack '() 8 #f #f)
                                        (loop (cdr types) i fpi next-varargs-after))]
                                 [else
                                  (cons (make-cat 'int (list (vector-ref int-argument-regs i)) '(8) #f #f)
                                        (loop (cdr types) (fx+ i 1) fpi next-varargs-after))])])))))))

    (define (result-via-pointer-argument? type)
      (nanopass-case (Ltype Type) type
                     [(fp-ftd& ,ftd) (> ($ftd-size ftd) 16)]
                     [else #f]))

    ;; result only meaningful if not `(result-via-pointer-argument? type)`
    (define (categorize-result type)
      (car (categorize-arguments (list type) #f)))

    (define register-save-or-restore
      (lambda (regs offset save?)
        (safe-assert (andmap reg? regs))
        (with-output-language (L13 Effect)
                              (cond
                               [(null? regs) `(nop)]
                               [else
                                (let ([n (length regs)])
                                  (define (set-each-reg set-one)
                                    (let loop ([regs regs] [offset offset])
                                      (cond
                                       [(null? (cdr regs))
                                        (set-one (with-output-language (L13 Lvalue)
                                                                       (if (eq? (reg-type (car regs)) 'fp)
                                                                           (%mref ,%sp ,%zero ,offset fp)
                                                                           (%mref ,%sp ,offset)))
                                                 (car regs))]
                                       [else
                                        (%seq
                                         ,(loop (list (car regs)) offset)
                                         ,(loop (cdr regs) (fx+ offset (constant ptr-bytes))))])))
                                  (if save?
                                      (set-each-reg (lambda (mem reg) `(set! ,mem ,reg)))
                                      (set-each-reg (lambda (mem reg) `(set! ,reg ,mem)))))]))))

    (define save-and-restore
      (lambda (regs e)
        (safe-assert (andmap reg? regs))
        (with-output-language (L13 Effect)
                              (let ([n (length regs)])
                                (%seq
                                 (set! ,%sp ,(%inline - ,%sp (immediate ,(* n (constant ptr-bytes)))))
                                 ,(register-save-or-restore regs 0 #t)
                                 ,e
                                 ,(register-save-or-restore regs 0 #f)
                                 (set! ,%sp ,(%inline + ,%sp (immediate ,(* n (constant ptr-bytes))))))))))

    (include "ffi-help.ss")

    ;; load values from memory into two regs
    (define (unpack-two-registers src src-offset regs sizes offsets)
      (let ([r1 (car regs)]
            [r2 (cadr regs)]
            [s1 (car sizes)]
            [s2 (cadr sizes)]
            [off2 (fx+ (cadr offsets) src-offset)])
        (with-output-language (L13 Effect)
                              (%seq
                               ,(if (eq? (reg-type r1) 'fp)
                                    (if (fx= s1 4)
                                        `(set! ,r1 ,(%inline load-single ,(%mref ,src ,%zero ,src-offset fp)))
                                        `(set! ,r1 ,(%mref ,src ,%zero ,src-offset fp)))
                                    (memory-to-reg r1 src src-offset s1 #t %scratch1))
                               ,(if (eq? (reg-type r2) 'fp)
                                    (if (fx= s2 4)
                                        `(set! ,r2 ,(%inline load-single ,(%mref ,src ,%zero ,off2 fp)))
                                        `(set! ,r2 ,(%mref ,src ,%zero ,off2 fp)))
                                    (memory-to-reg r2 src off2 s2 #t %scratch1))))))

    (define-who asm-foreign-call
      (with-output-language (L13 Effect)
                            (letrec ( ;; no enough arg regs, put them on the stack
                                     [load-double-stack
                                      (lambda (offset)
                                        (lambda (x) ; unboxed
                                          `(set! ,(%mref ,%sp ,%zero ,offset fp) ,x)))]
                                     [load-single-stack
                                      (lambda (offset)
                                        (lambda (x) ; unboxed
                                          (%inline store-double->single ,(%mref ,%sp ,%zero ,offset fp) ,x)))]
                                     [load-int-stack
                                      (lambda (offset)
                                        (lambda (rhs) ; requires rhs
                                          `(set! ,(%mref ,%sp ,offset) ,rhs)))]
                                     ;; load Scheme floats into float regs
                                     [load-double-reg
                                      (lambda (fpreg)
                                        (lambda (x) ; unboxed
                                          `(set! ,fpreg ,x)))]
                                     [load-single-reg
                                      (lambda (fpreg)
                                        (lambda (x) ; unboxed
                                          `(set! ,fpreg ,(%inline double->single ,x))))]
                                     ;; float, but no float reg is available, so put it in int regs following ABI
                                     [load-double-into-int-reg
                                      (lambda (reg)
                                        (lambda (x) ; unboxed
                                          `(set! ,reg ,(%inline fpcastto ,x))))]
                                     [load-single-into-int-reg
                                      (lambda (reg)
                                        (lambda (x) ; unboxed
                                          `(seq
                                            (set! ,%fpscratch ,(%inline double->single ,x))
                                            (set! ,reg ,(%inline fpcastto ,%fpscratch)))))]
                                     [load-double-indirect-reg
                                      (lambda (fpreg)
                                        (lambda (x)
                                          ;; data in ftd, so flonum-data-disp not needed
                                          `(set! ,fpreg ,(%mref ,x ,%zero ,0 fp))))]
                                     [load-single-indirect-reg
                                      (lambda (fpreg)
                                        (lambda (x)
                                          `(set! ,fpreg ,(%inline load-single ,(%mref ,x ,%zero ,0 fp)))))]
                                     [load-two-indirect-regs
                                      (lambda (regs sizes offsets)
                                        (lambda (x)
                                          (unpack-two-registers x 0 regs sizes offsets)))]
                                     [load-int-reg
                                      (lambda (ireg)
                                        (lambda (x)
                                          `(set! ,ireg ,x)))]
                                     [load-indirect-stack
                                      (lambda (offset from-offset size)
                                        (lambda (x) ; requires var
                                          (memory-to-memory %sp offset x from-offset size %scratch1)))]
                                     [load-indirect-reg
                                      (lambda (reg size unsigned?)
                                        (lambda (x) ; requires var
                                          (memory-to-reg reg x 0 size unsigned? %scratch1)))]
                                     [load-indirect-two-regs
                                      (lambda (reg1 reg2 size)
                                        (lambda (x) ; requires var
                                          (%seq
                                           (set! ,reg1 ,(%mref ,x 0))
                                           ,(memory-to-reg reg2 x 8 (fx- size 8) #t %scratch1))))]
                                     [do-args
                                      (lambda (types varargs-after)
                                        (let* ([cats (categorize-arguments types varargs-after)])
                                          (let loop ([types types] [cats cats] [locs '()] [regs '()] [isp 0])
                                            (if (null? types)
                                                (values (reverse locs) regs isp)
                                                (let* ([cat (car cats)]
                                                       [reg (and (pair? (cat-regs cat))
                                                                 (car (cat-regs cat)))])
                                                  (define use-fp-reg
                                                    (lambda (load-reg) (loop (cdr types) (cdr cats)
                                                                             (cons load-reg locs)
                                                                             (cons reg regs) isp)))
                                                  (define use-int-reg
                                                    (lambda (load-reg) (loop (cdr types) (cdr cats)
                                                                             (cons load-reg locs)
                                                                             (cons reg regs) isp)))
                                                  (define use-stack
                                                    (lambda (load-stack) (loop (cdr types) (cdr cats)
                                                                               (cons load-stack locs)
                                                                               regs (fx+ isp 8))))
                                                  (nanopass-case (Ltype Type) (car types)
                                                                 [(fp-double-float)
                                                                  (case (cat-place cat)
                                                                    [(fp) (use-fp-reg (load-double-reg reg))]
                                                                    [(int) (use-int-reg (load-double-into-int-reg reg))]
                                                                    [else (use-stack (load-double-stack isp))])]
                                                                 [(fp-single-float)
                                                                  (case (cat-place cat)
                                                                    [(fp) (use-fp-reg (load-single-reg reg))]
                                                                    [(int) (use-int-reg (load-single-into-int-reg reg))]
                                                                    [else (use-stack (load-single-stack isp))])]
                                                                 ;; need to move the aggregate data into regs or onto the stack
                                                                 [(fp-ftd& ,ftd)
                                                                  (case (cat-place cat)
                                                                    [(fp)
                                                                     ;; must be 1 register
                                                                     (use-fp-reg (if (fx= 4 ($ftd-size ftd))
                                                                                     (load-single-indirect-reg reg)
                                                                                     (load-double-indirect-reg reg)))]
                                                                    [(int)
                                                                     ;; can be 1 or 2 registers
                                                                     (let ([int-regs (cat-regs cat)])
                                                                       (loop (cdr types) (cdr cats)
                                                                             (cons (if (null? (cdr int-regs))
                                                                                       (if (cat-by-reference cat)
                                                                                           ;; by reference
                                                                                           (load-int-reg (car int-regs))
                                                                                           ;; size <= 16, load from memory to reg(s)
                                                                                           (load-indirect-reg (car int-regs) ($ftd-size ftd) ($ftd-unsigned? ftd)))
                                                                                       (load-indirect-two-regs (car int-regs) (cadr int-regs) ($ftd-size ftd)))
                                                                                   locs)
                                                                             (append int-regs regs) isp))]
                                                                    [(mix)
                                                                     ;; 2 registers, one of which is a float-point register
                                                                     (let ([to-regs (cat-regs cat)])
                                                                       (loop (cdr types) (cdr cats)
                                                                             (cons (load-two-indirect-regs to-regs (cat-size/s cat) (cat-offsets cat))
                                                                                   locs)
                                                                             (append to-regs regs) isp))]
                                                                    [else
                                                                     (cond
                                                                      [(cat-by-reference cat)
                                                                       ;; pass by reference on stack
                                                                       (use-stack (load-int-stack isp))]
                                                                      [else
                                                                       ;; copy to stack
                                                                       (let ([size ($ftd-size ftd)])
                                                                         (loop (cdr types) (cdr cats)
                                                                               (cons (load-indirect-stack isp 0 size) locs)
                                                                               regs (fx+ isp (align 8 size))))])])]
                                                                 [else
                                                                  (case (cat-place cat)
                                                                    [(int) (use-int-reg (load-int-reg reg))]
                                                                    [else (use-stack (load-int-stack isp))])]))))))]
                                     ;; ftd result now in regs, move them to Scheme memory
                                     [add-fill-result
                                      (lambda (needed? cat frame-size e)
                                        (cond
                                         [needed? (%seq
                                                   ,e
                                                   ;; get stashed pointer:
                                                   (set! ,%Carg3 ,(%mref ,%sp ,frame-size))
                                                   ,(let ([regs (cat-regs cat)]
                                                          [sizes (cat-size/s cat)])
                                                      (case (cat-place cat)
                                                        [(int)
                                                         (cond
                                                          [(pair? (cdr regs))
                                                           (%seq
                                                            (set! ,(%mref ,%Carg3 0) ,(car regs)) ; first size must be 8
                                                            ,(reg-to-memory %Carg3 8 (cadr sizes) (cadr regs)))]
                                                          [else
                                                           (reg-to-memory %Carg3 0 (car sizes) (car regs))])]
                                                        ;;@ ftype values are stored without tags,
                                                        ;;@ So no flonum-data-disp is needed below.
                                                        [(fp)
                                                         ;; must be single register
                                                         (if (fx= 4 (car sizes))
                                                             (%inline store-single ,(%mref ,%Carg3 ,%zero 0 fp) ,(car regs))
                                                             `(set! ,(%mref ,%Carg3 ,%zero 0 fp) ,(car regs)))]
                                                        [(mix)
                                                         ;; can be a mixture of ints, doubles, singles
                                                         (let ([offsets (cat-offsets cat)]
                                                               [r1 (car regs)]
                                                               [r2 (cadr regs)])
                                                           (%seq
                                                            ,(if (eq? (reg-type r1) 'fp)
                                                                 (if (fx= 4 (car sizes))
                                                                     (%inline store-single ,(%mref ,%Carg3 ,%zero 0 fp) ,r1)
                                                                     `(set! ,(%mref ,%Carg3 ,%zero 0 fp) ,r1))
                                                                 (reg-to-memory %Carg3 0 (car sizes) r1))
                                                            ,(if (eq? (reg-type r2) 'fp)
                                                                 (if (fx= 4 (cadr sizes))
                                                                     (%inline store-single ,(%mref ,%Carg3 ,%zero ,(cadr offsets) fp) ,r2)
                                                                     `(set! ,(%mref ,%Carg3 ,%zero ,(cadr offsets) fp) ,r2))
                                                                 (reg-to-memory %Carg3 (cadr offsets) (cadr sizes) r2))))]
                                                        [else ($oops 'assembler-internal "unexpected result place")])))]
                                         [else e]))]
                                     [add-deactivate
                                      (lambda (adjust-active? t0 live* result-live* k)
                                        (cond
                                         [adjust-active?
                                          (%seq
                                           (set! ,%ac0 ,t0)
                                           ,(save-and-restore live* (%inline deactivate-thread))
                                           ,(k %ac0)
                                           ,(save-and-restore result-live* `(set! ,%Cretval ,(%inline activate-thread))))]
                                         [else (k t0)]))])
                              (define returnem
                                (lambda (frame-size locs ccall r-loc)
                                  ;; need to maintain 16-byte alignment, ignoring the return address
                                  ;; pushed by call instruction, which counts as part of callee's frame
                                  ;; tc is callee-save; no need to save
                                  (let ([frame-size (align 16 frame-size)])
                                    (values (lambda ()
                                              (if (fx= frame-size 0)
                                                  `(nop)
                                                  `(set! ,%sp ,(%inline - ,%sp (immediate ,frame-size)))))
                                            locs
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
                                       [ftd-result? (nanopass-case (Ltype Type) result-type
                                                                   [(fp-ftd& ,ftd) #t]
                                                                   [else #f])]
                                       [pass-result-ptr? (result-via-pointer-argument? result-type)]
                                       [arg-type* (if (and ftd-result?
                                                           (not pass-result-ptr?))
                                                      (cdr arg-type*)
                                                      arg-type*)]
                                       [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)])
                                  (with-values (do-args arg-type* (extract-varargs-after-conv conv*))
                                    (lambda (locs live* frame-size)
                                      (returnem (if (and ftd-result?
                                                         (not pass-result-ptr?))
                                                    (fx+ frame-size 8) ;;@ for stash
                                                    frame-size)
                                                (cond
                                                 [(and ftd-result?
                                                       (not pass-result-ptr?))
                                                  ;; stash extra argument on the stack to be retrieved after call and filled with the result:
                                                  (cons (load-int-stack frame-size) locs)]
                                                 [else locs])
                                                (lambda (t0 not-varargs?)
                                                  (let* ([cat (categorize-result result-type)]
                                                         [result-reg* (if pass-result-ptr?
                                                                          '()
                                                                          (cat-regs cat))])
                                                    (add-fill-result
                                                     ;;@ foreign-procedure always generates the first arg which points to
                                                     ;;@ the place where the retval is to be stored. But since this ftd is
                                                     ;;@ less than 16 bytes, there's no need to actually pass it to C functions,
                                                     ;;@ so don't use that. BUT, we still have to store the retval in regs into
                                                     ;;@ the place pointed to by the pointer.
                                                     (and ftd-result? (not pass-result-ptr?)) cat frame-size
                                                     (add-deactivate adjust-active? t0 live* result-reg*
                                                                     (lambda (t0)
                                                                       `(inline ,(make-info-kill*-live* (add-caller-save-registers result-reg*) live*) ,%c-call ,t0))))))
                                                (nanopass-case (Ltype Type) result-type
                                                               ;;@ TODO check the two
                                                               [(fp-double-float)
                                                                (lambda (lvalue) ; unboxed
                                                                  `(set! ,lvalue ,%Cfpretval))]
                                                               [(fp-single-float)
                                                                (lambda (lvalue) ; unboxed
                                                                  `(set! ,lvalue ,(%inline single->double ,%Cfpretval)))]
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
      #|
        Frame Layout
                   +---------------------------+
                   |    incoming stack args    |
                   +---------------------------+<- 16-byte boundary arg offset
                   |       saved reg args      |
                   +---------------------------+<- 8-byte boundary  arg regs offset
                   |    by-reference copies    |
                   +---------------------------+<- 8-byte boundary  copy offset
                   |     activatation state    |
                   |       if necessary        |
                   +---------------------------+<- 8-byte boundary  active state offset
                   |      &-return space       |
                   |       if necessary        |
                   +---------------------------+<- 8-byte boundary  return offset
                   |   callee-save regs + ra   |
                   +---------------------------+<- 8-byte boundary  callee-save offset
                   |      maybe padding        |
                   +---------------------------+<- 16-byte boundary
      |#
      (with-output-language (L13 Effect)
                            (let ()
                              (define load-double-stack
                                (lambda (offset)
                                  (lambda (x) ; requires var
                                    `(set! ,(%mref ,x ,%zero ,(constant flonum-data-disp) fp)
                                           ,(%mref ,%sp ,%zero ,offset fp)))))
                              (define load-single-stack
                                (lambda (offset)
                                  (lambda (x) ; requires var
                                    `(set! ,(%mref ,x ,%zero ,(constant flonum-data-disp) fp)
                                           ,(%inline load-single->double ,(%mref ,%sp ,%zero ,offset fp))))))
                              (define load-word-stack
                                (lambda (offset)
                                  (lambda (lvalue)
                                    `(set! ,lvalue ,(%mref ,%sp ,offset)))))
                              (define load-int-stack
                                (lambda (type offset)
                                  (lambda (lvalue)
                                    (nanopass-case (Ltype Type) type
                                                   [(fp-integer ,bits)
                                                    (case bits
                                                      [(8) `(set! ,lvalue (inline ,(make-info-load 'integer-8 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                                                      [(16) `(set! ,lvalue (inline ,(make-info-load 'integer-16 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                                                      [(32) `(set! ,lvalue (inline ,(make-info-load 'integer-32 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                                                      [(64) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                                                      [else (sorry! who "unexpected load-int-stack fp-integer size ~s" bits)])]
                                                   [(fp-unsigned ,bits)
                                                    (case bits
                                                      [(8) `(set! ,lvalue (inline ,(make-info-load 'unsigned-8 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                                                      [(16) `(set! ,lvalue (inline ,(make-info-load 'unsigned-16 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                                                      [(32) `(set! ,lvalue (inline ,(make-info-load 'unsigned-32 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                                                      [(64) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                                                      [else (sorry! who "unexpected load-int-stack fp-unsigned size ~s" bits)])]
                                                   [else `(set! ,lvalue ,(%mref ,%sp ,offset))]))))
                              ;; just pass the pointer on the stack to Scheme code
                              (define load-stack-address
                                (lambda (offset)
                                  (lambda (lvalue)
                                    `(set! ,lvalue ,(%inline + ,%sp (immediate ,offset))))))
                              (define do-args
                                ;; all of the args are on the stack at this point, and in argument order
                                ;; for registers, and stack arguments separate
                                (lambda (arg-type* arg-cat* reg-offset stack-arg-offset return-offset copy-offset
                                                   synthesize-first? ftd-result?)
                                  (let loop ([types arg-type*]
                                             [cats arg-cat*]
                                             [locs '()]
                                             [reg-offset reg-offset]
                                             [stack-arg-offset stack-arg-offset]
                                             [copy-offset copy-offset])
                                    (if (null? types)
                                        (let ([locs (reverse locs)])
                                          (cond
                                           [synthesize-first?
                                            (cons (load-stack-address return-offset) locs)]
                                           [else locs]))
                                        (let ([cat (car cats)]
                                              [type (car types)])
                                          (define use-reg
                                            (lambda (load-reg)
                                              (loop (cdr types) (cdr cats) (cons load-reg locs)
                                                    (fx+ reg-offset 8) stack-arg-offset copy-offset)))
                                          (define use-stack
                                            (lambda (load-stack)
                                              (loop (cdr types) (cdr cats) (cons load-stack locs)
                                                    reg-offset (fx+ stack-arg-offset 8) copy-offset)))
                                          (nanopass-case (Ltype Type) type
                                                         [(fp-double-float)
                                                          (case (cat-place cat)
                                                            ;; for int we can also load directly since it's stored as float
                                                            [(fp int) (use-reg (load-double-stack reg-offset))]
                                                            [else (use-stack (load-double-stack stack-arg-offset))])]
                                                         [(fp-single-float)
                                                          (case (cat-place cat)
                                                            [(fp int) (use-reg (load-single-stack reg-offset))]
                                                            [else (use-stack (load-single-stack stack-arg-offset))])]
                                                         [(fp-ftd& ,ftd)
                                                          (cond
                                                           [(cat-by-reference cat)
                                                            ;; register or stack contains pointer to data; we
                                                            ;; copy to ensure that Scheme side doesn't mutate caller's data
                                                            (let* ([load (lambda (x)
                                                                           (%seq
                                                                            ,(if (eq? (cat-place cat) 'stack)
                                                                                 ((load-word-stack stack-arg-offset) x)
                                                                                 ((load-word-stack reg-offset) x))
                                                                            ,(memory-to-memory %sp copy-offset x 0
                                                                                               (cat-by-reference cat) %scratch1)
                                                                            (set! ,x ,(%inline + ,%sp (immediate ,copy-offset)))))])
                                                              (if (eq? (cat-place cat) 'stack)
                                                                  (use-stack load)
                                                                  (use-reg load)))]
                                                           [else
                                                            ;; point to argument on stack; even in the case of two registers,
                                                            ;; they'll have been copied there in sequence as needed, but 'mix
                                                            ;; sequences may need to be compacted
                                                            (case (cat-place cat)
                                                              [(mix)
                                                               (let* ([load (load-stack-address reg-offset)]
                                                                      [load (cond
                                                                             [(fx= 8 (cadr (cat-offsets cat)))
                                                                              ;; no compaction needed
                                                                              load]
                                                                             [else
                                                                              ;; compact
                                                                              (lambda (x)
                                                                                (%seq
                                                                                 ,(memory-to-memory %sp (fx+ reg-offset (cadr (cat-offsets cat)))
                                                                                                    %sp (fx+ reg-offset 8)
                                                                                                    (cadr (cat-size/s cat))
                                                                                                    %scratch1)
                                                                                 ,(load x)))])])
                                                                 (loop (cdr types) (cdr cats)
                                                                       (cons load locs)
                                                                       (fx+ reg-offset 16) stack-arg-offset copy-offset))]
                                                              [(int fp)
                                                               ;; one or two registers
                                                               (let ([load (load-stack-address reg-offset)])
                                                                 (if (null? (cdr (cat-regs cat)))
                                                                     (use-reg load)
                                                                     (loop (cdr types) (cdr cats)
                                                                           (cons load locs)
                                                                           (fx+ reg-offset 16) stack-arg-offset copy-offset)))]
                                                              [else (use-reg (load-stack-address stack-arg-offset))])])]
                                                         [else
                                                          ;; integer, scheme-object, etc.
                                                          (case (cat-place cat)
                                                            [(int) (use-reg (load-int-stack type reg-offset))]
                                                            [else (use-stack (load-int-stack type stack-arg-offset))])]))))))
                              (define do-result
                                (lambda (result-type result-cat synthesize-first? return-offset)
                                  (let ([regs (cat-regs result-cat)])
                                    (nanopass-case (Ltype Type) result-type
                                                   ;; move retvals from Scheme side to C
                                                   [(fp-double-float)
                                                    (lambda (rhs) ; boxed
                                                      `(set! ,(car regs) ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp)))]
                                                   [(fp-single-float)
                                                    (lambda (rhs) ; boxed
                                                      `(set! ,(car regs)
                                                             ,(%inline double->single ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp))))]
                                                   [(fp-void)
                                                    (lambda () `(nop))]
                                                   [(fp-ftd& ,ftd)
                                                    (cond
                                                     [(not synthesize-first?)
                                                      ;; we passed the pointer to be filled, so nothing more to do here
                                                      (safe-assert (null? regs))
                                                      (lambda () `(nop))]
                                                     ;;@ size < 16, need to put retval in regs (synthesize)
                                                     [else
                                                      (case (cat-place result-cat)
                                                        [(fp)
                                                         ;; always one register
                                                         (if (fx= 4 (car (cat-size/s result-cat)))
                                                             (lambda ()
                                                               `(set! ,(car regs) ,(%inline load-single ,(%mref ,%sp ,%zero ,return-offset fp))))
                                                             (lambda ()
                                                               `(set! ,(car regs) ,(%mref ,%sp ,%zero ,return-offset fp))))]
                                                        [(mix)
                                                         (lambda ()
                                                           (unpack-two-registers %sp return-offset regs
                                                                                 (cat-size/s result-cat) (cat-offsets result-cat)))]
                                                        [(int)
                                                         (cond
                                                          [(null? (cdr regs))
                                                           (lambda ()
                                                             (memory-to-reg (car regs) %sp return-offset
                                                                            ($ftd-size ftd) ($ftd-unsigned? ftd) %scratch1))]
                                                          [else
                                                           (lambda ()
                                                             (%seq
                                                              (set! ,(car regs) ,(%mref ,%sp ,return-offset))
                                                              ;; we don't have to be careful about the size, because we
                                                              ;; allocated a multiple of a word size for the synthesized pointer,
                                                              ;; and sign extension is not relevant
                                                              (set! ,(cadr regs) ,(%mref ,%sp ,(fx+ return-offset 8)))))])]
                                                        [else
                                                         ($oops 'assembler-internal "unexpected result place")])])]
                                                   [else
                                                    ;; integer, scheme-object, etc.
                                                    (lambda (x)
                                                      `(set! ,(car regs) ,x))]))))
                              (lambda (info)
                                (define callee-save-regs+ra (cons* %ra
                                                                   ;; reserved:
                                                                   %tc %sfp %ap %trap
                                                                   ;; allocable:
                                                                   (get-allocable-callee-save-regs 'all)))
                                (let ([arg-type* (info-foreign-arg-type* info)]
                                      [result-type (info-foreign-result-type info)])
                                  (let ([pass-result-ptr? (result-via-pointer-argument? result-type)]
                                        [result-cat (categorize-result result-type)])
                                    (let* ([result-regs (if pass-result-ptr?
                                                            '()
                                                            (cat-regs result-cat))]
                                           [ftd-result? (nanopass-case (Ltype Type) result-type
                                                                       [(fp-ftd& ,ftd) #t]
                                                                       [else #f])]
                                           ;;@ size < 16, not passed as ptr, need to synthesize the result in return register(s)
                                           [synthesize-first? (and ftd-result?
                                                                   (not pass-result-ptr?))]
                                           [arg-type* (if synthesize-first?
                                                          (cdr arg-type*)
                                                          arg-type*)]
                                           [conv* (info-foreign-conv* info)]
                                           [arg-cat* (categorize-arguments arg-type* (extract-varargs-after-conv conv*))]
                                           [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)]
                                           [arg-regs (apply append (map cat-regs arg-cat*))])
                                      (let* ([arg-reg-bytes (fx* (length arg-regs) 8)]
                                             [copy-bytes (apply + (map (lambda (cat)
                                                                         (or (cat-by-reference cat)
                                                                             0))
                                                                       arg-cat*))]
                                             [active-state-bytes (if adjust-active? 8 0)]
                                             [return-bytes (if synthesize-first?
                                                               ;;@ main Scheme code first stores the results in the &-return space
                                                               ;;@ for the code here to synthethize
                                                               (fx* (length result-regs) 8)
                                                               0)]
                                             [callee-save-bytes (fx* 8 (length callee-save-regs+ra))]
                                             [total-unpadded-bytes (fx+ arg-reg-bytes copy-bytes active-state-bytes
                                                                        return-bytes callee-save-bytes)]
                                             [total-bytes (align 16 total-unpadded-bytes)])
                                        (let* ([callee-save-offset (fx- total-bytes total-unpadded-bytes)]
                                               [return-offset (fx+ callee-save-offset callee-save-bytes)]
                                               [active-state-offset (fx+ return-offset return-bytes)]
                                               [copy-offset (fx+ active-state-offset active-state-bytes)]
                                               [arg-regs-offset (fx+ copy-offset copy-bytes)]
                                               [args-offset total-bytes])
                                          (values
                                           (lambda ()
                                             (%seq
                                              (set! ,%sp ,(%inline - ,%sp (immediate ,total-bytes)))
                                              ;; save argument register values to the stack so we don't lose the values
                                              ;; across possible calls to C while setting up the tc and allocating memory
                                              ,(register-save-or-restore arg-regs arg-regs-offset #t)
                                              ;; save the callee save registers & return address
                                              ,(register-save-or-restore callee-save-regs+ra callee-save-offset #t)
                                              ;; maybe activate
                                              ,(if adjust-active?
                                                   `(seq
                                                     (set! ,%Cretval ,(%inline activate-thread))
                                                     (set! ,(%mref ,%sp ,active-state-offset) ,%Cretval))
                                                   `(nop))
                                              ;; set up tc for benefit of argument-conversion code, which might allocate
                                              ,(if-feature pthreads
                                                           (%seq
                                                            (set! ,%Cretval ,(%inline get-tc))
                                                            (set! ,%tc ,%Cretval))
                                                           `(set! ,%tc (literal ,(make-info-literal
                                                                                  #f 'entry (lookup-c-entry thread-context) 0))))))
                                           ;; list of procedures that marshal arguments from their C stack locations
                                           ;; to the Scheme argument locations
                                           (do-args arg-type* arg-cat* arg-regs-offset args-offset return-offset copy-offset
                                                    synthesize-first? ftd-result?)
                                           (do-result result-type result-cat synthesize-first? return-offset)
                                           (lambda ()
                                             (in-context
                                              Tail
                                              (%seq
                                               ,(if adjust-active?
                                                    (%seq
                                                     ;; We need *(sp+active-state-offset) in %Carg1,
                                                     ;; but that can also be a return register.
                                                     ;; Meanwhle, sp may change before we call unactivate.
                                                     ;; So, move to %scratch1 for now, then %Carg1 later:
                                                     (set! ,%scratch1 ,(%mref ,%sp ,active-state-offset))
                                                     ,(save-and-restore
                                                       result-regs
                                                       `(seq
                                                         (set! ,%Carg1 ,%scratch1)
                                                         ,(%inline unactivate-thread ,%Carg1))))
                                                    `(nop))
                                               ;; restore the callee save registers
                                               ,(register-save-or-restore callee-save-regs+ra callee-save-offset #f)
                                               (set! ,%sp ,(%inline + ,%sp (immediate ,total-bytes)))
                                               ;; done
                                               (asm-c-return ,null-info ,callee-save-regs+ra ... ,result-regs ...))))))))))))))

    ) ;; foreign
  )
