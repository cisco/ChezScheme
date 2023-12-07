;;; riscv64.ss

;;; SECTION 1: registers
(define-registers
  (reserved
   [%tc  %x8  #t 8 uptr]
   [%sfp %x9  #t 9 uptr]
   [%ap  %x18 #t 18 uptr]
   [%trap %r27 #t 27 uptr])
  (allocable
   [%ac0 %x19 #t 19 uptr]
   [%xp  %x20 #t 20 uptr]
   [%ts  %x5  #f 5 uptr]
   [%td  %x21 #t 21 uptr]
   [%cp  %x22 #t 22 uptr]
   [     %x10 %Carg1 %Cretval #f 10 uptr]
   [     %x11 %Carg2 #f 11 uptr]
   [     %x12 %Carg3 #f 12 uptr]
   [     %x13 %Carg4 #f 13 uptr]
   [     %x14 %Carg5 #f 14 uptr]
   [     %x15 %Carg6 #f 15 uptr]
   [     %x16 %Carg7 #f 16 uptr]
   [     %x17 %Carg8 #f 17 uptr]
   [     %x6  #f 6 uptr]
   [     %x7  #f 7 uptr]
   [     %x28 #f 28 uptr]
   [     %x23 #t 23 uptr]
   [     %x24 #t 24 uptr]
   [     %x25 #t 25 uptr]
   [     %x26 #t 26 uptr]
   [%fp1 %f0 #f 0 fp]
   [%fp2 %f1 #f 1 fp]
   [%fp3 %f2 #f 2 fp]
   [%fp4 %f3 #f 3 fp]
   [%fp5 %f4 #f 4 fp]
   [%fp6 %f5 #f 5 fp]
   )
  (machine-dependent
   [%real-zero %x0 #f 0 uptr]
   [%ra %x1 #f 1 uptr]
   [%sp %x2 #t 2 uptr]
   [%scratch0 %jump %x30 #f 30 uptr]
   [%scratch1 %x31 #f 31 uptr]
   [%cond %x29 #f 29 uptr]  ; for carry/ovfl flag, since RISC-V has no flag regs
   [%Cfparg1 %Cfpretval %f10 #f 10 fp]
   [%Cfparg2 %f11           #f  11 fp]
   [%Cfparg3 %f12           #f  12 fp]
   [%Cfparg4 %f13           #f  13 fp]
   [%Cfparg5 %f14           #f  14 fp]
   [%Cfparg6 %f15           #f  15 fp]
   [%Cfparg7 %f16           #f  16 fp]
   [%Cfparg8 %f17           #f  17 fp]
   [%fpscratch %f7          #f  7 fp]))

;;; SECTION 2: instructions
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
          (safe-assert (signed12? imm))
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm ,type)))))
      (nanopass-case (L15c Triv) a
        [(mref ,lvalue0 ,lvalue1 ,imm ,type)
         (lvalue->ur lvalue0
                     (lambda (x0)
                       (lvalue->ur lvalue1
                                   (lambda (x1)
                                     (cond
                                       [(signed12? imm)
                                        (return x0 x1 imm type)]
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

  (define-instruction value single->double
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-single->double ,y))])

  (define-instruction value double->single
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-double->single ,y))])

  (define-instruction value (fpmove)
    [(op (x fpmem) (y fpur)) `(set! ,(make-live-info) ,x ,y)]
    [(op (x fpur) (y fpmem fpur)) `(set! ,(make-live-info) ,x ,y)])

  (define-instruction value (fpsingle)
    [(op (x fpur) (y fpur))
     (seq
      `(set! ,(make-live-info) ,x (asm ,info ,asm-double->single ,y))
      `(set! ,(make-live-info) ,x (asm ,info ,asm-single->double ,x)))])

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

  (define-instruction value (fpt)
    [(op (x fpur) (y ur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpt ,y))])

  (define-instruction value (fptrunc)
    [(op (z ur) (x fpur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-fptrunc ,x))])

  (define-instruction value (fpsqrt)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsqrt ,y))])

  (define-instruction value (fp+ fp- fp/ fp*)
    [(op (x fpur) (y fpur) (z fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpop-2 op) ,y ,z))])

  ;; pred all return multiple values
  (define-instruction pred (fp= fp< fp<=)
    [(op (x fpur) (y fpur))
     (let ([info (make-info-condition-code op #f #f)])
       (values '() `(asm ,info ,(asm-fp-relop info) ,x ,y)))])

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
      [(op (x ur) (y ur) (w imm12 ur) (old ur) (new ur))
       (lea->reg x y w
                 (lambda (r)
                   (let ([u1 (make-tmp 'u1)] [u2 (make-tmp 'u2)])
                     (seq
                      `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
                      `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
                      `(asm ,info ,asm-cas ,r ,old ,new ,u1 ,u2)))))]))


  (define-instruction effect (store-store-fence)
    [(op)
     `(asm ,info ,(asm-fence 'store-store))])

  (define-instruction effect (acquire-fence)
    [(op)
     `(asm ,info ,(asm-fence 'acquire))])

  (define-instruction effect (release-fence)
    [(op)
     `(asm ,info ,(asm-fence 'release))])

  (define-instruction effect (pause)
    [(op) `(asm ,info ,(asm-fence 'store-store))])

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
                    asm-enter asm-sll asm-srl asm-sra asm-fpsqrt asm-fptrunc asm-fpt
                    asm-cas asm-relop asm-fp-relop asm-fpop-2 asm-save-flrv asm-restore-flrv
                    asm-direct-jump asm-indirect-jump asm-literal-jump asm-condition-code
                    asm-jump asm-conditional-jump asm-library-jump
                    asm-get-tc asm-activate-thread asm-deactivate-thread asm-unactivate-thread
                    asm-push asm-pop asm-return asm-c-return asm-kill
                    asm-load asm-store asm-fence asm-swap asm-lock asm-lock+/- asm-move asm-move/extend
                    asm-fpmove asm-fpmove-single asm-single->double asm-double->single
                    asm-fpcastto asm-fpcastfrom
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
  (define-op mulh bin-op #b0110011 #b001 #b0000001)
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
  (define-op fmin.d bin-op #b1010011 #b000 #b0010101)
  (define-op feq.d bin-op #b1010011 #b010 #b1010001)
  (define-op flt.d bin-op #b1010011 #b001 #b1010001)
  (define-op fle.d bin-op #b1010011 #b000 #b1010001)

  (define-op fsqrt.d  bin-op #b1010011 #b111 #b0101101) ; #b111: dynamic rounding mode
  (define-op fcvt.l.d bin-op #b1010011 #b001 #b1100001) ; #b001: round toward zero
  (define-op fcvt.d.l bin-op #b1010011 #b111 #b1101001) ; #b111: dynamic rounding mode
  (define-op fcvt.s.d bin-op #b1010011 #b111 #b0100000) ; #b111: dynamic rounding mode
  (define-op fcvt.d.s bin-op #b1010011 #b111 #b0100001) ; #b111: dynamic rounding mode

  (define-op fmov.x.d bin-op #b1010011 #b000 #b1110001)
  (define-op fmov.d.x bin-op #b1010011 #b000 #b1111001)

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
                   [(fmov.x.d fmov.d.x) #b00000]
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
    (lambda (op opcode funct3 mode code*)
      (emit-code (op code*)
        [28 #b000]
        [24 (case mode
              [(store-store) #b0001] ; W
              [(acquire) #b0001] ; R
              [(release) #b0011] ; RW
              [else ($oops 'assembler-internal "unrecognized fence mode")])]
        [20 (case mode
              [(store-store acquire) #b0011] ; RW
              [(release) #b0010] ; R
              [else ($oops 'assembler-internal "unrecognized fence mode")])]
        [0 #b00000000000000001111])))

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
  (define jump-disp?
    (lambda (x)
      (and (signed-32? x)
           (not (logtest x #b11))))) ;; 4-byte aligned
  (define cond-jump-disp?
    (lambda (x)
      (and (fixnum? x)
           (fx<= (fx- (expt 2 12))
                 x
                 (fx- (expt 2 12) 1))))) ;; 13 bits
  ;; see RISC-V ISA
  (define upper20
    (lambda (x)
      (ash (+ x #x800) -12)))
  (define lower12
    (lambda (x)
      (- x (ash (upper20 x) 12))))
  (define luiable?
    (lambda (x)
      (<= (- (expt 2 31))
          x
          ;; we since add #x800 in `upper20`, we must avoid numbers
          ;; close to the upper end:
          (- (expt 2 31) 1 #x800))))

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

  (define asm-add/ovfl ;;@ todo imm? optimization?
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit add %cond src0 src1 ; using `%cond` as temporary dest
              ;; overflow if these are not the same:
              ;;  * result is less than first argument
              ;;  * second argument is negative
              (emit slt %scratch0 %cond src0
                    (emit slti %scratch1 src1 0
                          (emit addi dest %cond 0 ; move dest into place
                                (emit xor %cond %scratch0 %scratch1
                                      code*))))))))

  (define asm-sub
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub dest src0 src1 code*))))

  (define asm-sub/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit sub %cond src0 src1 ; using `%cond` as temporary dest
              ;; overflow if these are not the same:
              ;;  * result is greater than first argument
              ;;  * second argument is negative
              (emit slt %scratch0 src0 %cond
                    (emit slti %scratch1 src1 0
                          (emit addi dest %cond 0 ; move dest into place
                                (emit xor %cond %scratch0 %scratch1
                                      code*))))))))

  (define asm-sub/eq
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit sub dest src0 src1
                    (emit sltiu %cond dest 1 code*))))) ;; set if dest == 0

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit mul dest src0 src1 code*))))

  ;; overflow if hi64(src0*src1) != (lo64(src0*src1) >> 63)
  (define asm-mul/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
              (emit mulh %scratch1 src0 src1
                    (emit mul dest src0 src1
                          (emit srai %cond dest 63
                                (emit bne %scratch1 %cond 12
                                      (emit addi %cond %real-zero 0
                                            (emit jal %real-zero 8
                                                  (emit addi %cond %real-zero 1
                                                        code*))))))))))

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
                        [else
                         (safe-assert (signed12? n))
                         (case type
                           [(integer-64 unsigned-64) (emit add %scratch0 base index
                                                           (emit ld dest %scratch0 n code*))]
                           [(integer-32) (emit add %scratch0 base index
                                               (emit lw dest %scratch0 n code*))]
                           [(unsigned-32) (emit add %scratch0 base index
                                                (emit lwu dest %scratch0 n code*))]
                           [(integer-16) (emit add %scratch0 base index
                                               (emit lh dest %scratch0 n code*))]
                           [(unsigned-16) (emit add %scratch0 base index
                                                (emit lhu dest %scratch0 n code*))]
                           [(integer-8) (emit add %scratch0 base index
                                              (emit lb dest %scratch0 n code*))]
                           [(unsigned-8) (emit add %scratch0 base index
                                               (emit lbu dest %scratch0 n code*))]
                           [else (sorry! who "unexpected mref type ~s" type)])])))))))

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

  (define-who asm-fpop-2
    (lambda (op)
      ;; args contain the base addr of fp numbers, thus need the fp disp
      (lambda (code* dest src1 src2)
        (Trivit (dest src1 src2)
          (case op
            [(fp+) (emit fadd.d dest src1 src2 code*)]
            [(fp-) (emit fsub.d dest src1 src2 code*)]
            [(fp*) (emit fmul.d dest src1 src2 code*)]
            [(fp/) (emit fdiv.d dest src1 src2 code*)]
            [else (sorry! who "unrecognized op ~s" op)])))))

  (define asm-fpsqrt
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fsqrt.d dest src '() code*))))

  ;; big <-> little-endian
  (define-who asm-swap
    (lambda (type)
      (rec asm-swap-internal
           (lambda (code* dest src)
             (let ([t0 %scratch0]
                   [t1 %scratch1]) ; holds original src value, in case src = dest
               (Trivit (dest src)
                 ;; Maybe one day the B extension for RISC-V will be useful here
                 (define dance
                   (lambda (right left code*)
                     (emit srli t0 t1 right
                           (emit andi t0 t0 #xff
                                 (if (= left 0)
                                     (emit or dest dest t0 code*)
                                     (emit slli t0 t0 left
                                           (emit or dest dest t0 code*)))))))
                 (emit addi t1 src 0
                       (case type
                         [(integer-16)
                          ;; 1st byte
                          (emit slli t0 t1 56
                                (emit srai dest t0 48
                                      ;; 2nd byte
                                      (dance 8 0 code*)))]
                       [(unsigned-16)
                        (emit andi t0 t1 #xff
                              (emit slli dest t0 8
                                    (dance 8 0 code*)))]
                       [(integer-32)
                        ;; 1st byte
                        (emit slli t0 t1 56
                              (emit srai dest t0 32
                                    ;; 2nd and so on
                                    (dance 8 16
                                           (dance 16 8
                                                  (dance 24 0 code*)))))]
                       [(unsigned-32)
                        ;; 1st byte
                        (emit andi t0 t1 #xff
                              (emit slli dest t0 24
                                    ;; 2nd and so on
                                    (dance 8 16
                                           (dance 16 8
                                                  (dance 24 0 code*)))))]
                       [(integer-64 unsigned-64)
                        (emit slli dest t1 56
                              (dance 8 48
                                     (dance 16 40
                                            (dance 24 32
                                                   (dance 32 24
                                                          (dance 40 16
                                                                 (dance 48 8
                                                                        (dance 56 0 code*))))))))]
                       [else (sorry! who "unexpected asm-swap type argument ~s" type)]))))))))

  (define asm-lock ;;@ check operands of sc.d, see if can be used in both places
    ;;    lr.d tmp, [addr]
    ;;    bne  tmp, %real-zero, L # if tmp != 0, will look the same as sc.d failure
    ;;    addi tmp, %real-zero, 1
    ;;    sc.d tmp, tmp, [addr]
    ;;    sltui %cond, tmp, 1 # set %cond if tmp=0, meaning success
    ;;L:
    (lambda (code* addr t0)
      (Trivit (addr t0)
              (emit lr.d t0 addr '()
                    (emit bne t0 %real-zero 12
                          (emit addi t0 %real-zero 1
                                (emit sc.d t0 addr t0
                                      (emit sltiu %cond t0 1 code*))))))))

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

  (define-who asm-fp-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
          (values
           (let ([op (info-condition-code-type info)])
             (case op
               [(fp=) (emit feq.d %cond x y '())]
               [(fp<) (emit flt.d %cond x y '())]
               [(fp<=) (emit fle.d %cond x y '())]
               [else (sorry! who "unrecognized op ~s" op)]))
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
    (lambda (mode)
      (lambda (code*)
        (emit fence mode code*))))

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
                       (if (luiable? n)
                           (ax-mov32 dest n code*)
                           (ax-mov64 dest n code*)))]
                  [(literal) stuff
                   (ax-mov64 dest 0
                             (asm-helper-relocation code* (cons 'riscv64-abs stuff)))]
                  [(disp) (n breg)
                   (safe-assert (signed12? n))
                   (emit ld dest breg n code*)]
                  [(index) (n ireg breg)
                   (safe-assert (signed12? n))
                   (emit add %scratch0 ireg breg
                         (emit ld dest %scratch0 n code*))]
                  [else (bad!)])]
               [(ax-reg? src)
                (record-case dest
                  [(disp) (n breg)
                   (safe-assert (signed12? n))
                   (emit sd src breg n code*)]
                  [(index) (n ireg breg)
                   (safe-assert (signed12? n))
                   (emit add %scratch0 ireg breg
                         (emit sd src %scratch0 n code*))]
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
               (emit fsd src reg imm code*)
               (emit fsw src reg imm code*))]
          [(index) (n ireg breg)
           (safe-assert (signed12? n))
           (emit add %scratch0 ireg breg
                 (if double?
                     (emit fsd src %scratch0 n code*)
                     (emit fsw src %scratch0 n code*)))]
          [else
           (record-case src
             [(disp) (imm reg)
              (if double?
                  (emit fld dest reg imm code*)
                  (emit flw dest reg imm code*))]
             [(index) (n ireg breg)
              (safe-assert (signed12? n))
              (emit add %scratch0 ireg breg
                    (if double?
                        (emit fld dest %scratch0 n code*)
                        (emit flw dest %scratch0 n code*)))]
             [else (emit fmin.d dest src src code*)])]))))

  (define-who asm-single->double
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fcvt.d.s dest src '() code*))))

  (define-who asm-double->single
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fcvt.s.d dest src '() code*))))

  (define-who asm-fpcastto
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fmov.x.d dest src '() code*))))

  (define-who asm-fpcastfrom
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fmov.d.x dest src '() code*))))

  ;; flonum to fixnum
  (define-who asm-fptrunc
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fcvt.l.d dest src '() code*))))

  ;; fixnum to flonum
  (define-who asm-fpt
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fcvt.d.l dest src '() code*))))

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
                          (let ([incr-offset (adjust-return-point-offset incr-offset l)])
                            (lambda (offset)
                              (let ([disp (fx- next-addr (fx- offset incr-offset) -8)])
                                (cond
                                  [(luiable? disp)
                                   (Trivit (dest)
                                     (emit auipc dest (upper20 disp)
                                           (emit addi dest dest (lower12 disp) '())))]
                                  [else #f]))))]
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
                (safe-assert (luiable? disp))
                disp))] ;;@ not using (label ...) as in others
           [else 0])))
      (safe-assert (and (local-label? l1) (local-label? l2)))
      (make-cgchunk info l1 l2 next-addr
                    (let ([disp1 (get-disp next-addr l1)]
                          [disp2 (get-disp next-addr l2)])
                      (cond
                       ;; inverted
                       [(fx= disp1 0)
                        (safe-assert (luiable? disp2))
                        (if (or (and (fx<= 0 (fx+ disp2 4) (fx- (expt 2 11) 1)))
                                (and (fx<= (fx- (expt 2 12)) (fx+ disp2 4) 0)))
                            (emit beq %cond %real-zero (fx+ disp2 4) '())
                            (emit beq %cond %real-zero 8
                                  (emit jal %real-zero 12 ;; fall through
                                        (emit auipc %jump (upper20 (fx+ disp2 8)) ;; 2 instr below
                                              (emit jalr %real-zero %jump (lower12 (fx+ disp2 8)) '())))))]
                       ;; normal
                       [(fx= disp2 0)
                        (safe-assert (luiable? disp1))
                        (if (or (and (fx<= 0 (fx+ disp1 4) (fx- (expt 2 11) 1)))
                                (and (fx<= (fx- (expt 2 12)) (fx+ disp1 4) 0)))
                            (emit bne %cond %real-zero (fx+ disp1 4) '())
                            (emit bne %cond %real-zero 8
                                  (emit jal %real-zero 12 ;; fall through
                                        (emit auipc %jump (upper20 (fx+ disp1 8)) ;; 2 instr below
                                              (emit jalr %real-zero %jump (lower12 (fx+ disp1 8)) '())))))]
                       ;; others
                       [else
                        (safe-assert (luiable? (fx+ disp1 8)) (luiable? (fx+ disp2 16)))
                        (emit bne %cond %real-zero 12
                              (emit auipc %jump (upper20 (fx+ disp2 16)) ;; 4 instr below
                                    (emit jalr %real-zero %jump (lower12 (fx+ disp2 16))
                                          (emit auipc %jump (upper20 (fx+ disp1 8)) ;; 2 instr below
                                                (emit jalr %real-zero %jump (lower12 (fx+ disp1 8)) '())))))])))))

  (define asm-direct-jump
    (lambda (l offset)
      (let ([offset (adjust-return-point-offset offset l)])
        (asm-helper-jump '() (make-funcrel 'riscv64-jump l offset)))))

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
                       (emit ld %jump %scratch1 0
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

  (module (asm-foreign-call asm-foreign-callable)
    (define int-argument-regs (vector %Carg1 %Carg2 %Carg3 %Carg4 %Carg5 %Carg6 %Carg7 %Carg8))
    (define fp-argument-regs (vector %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4 %Cfparg5 %Cfparg6 %Cfparg7 %Cfparg8))
    (define align
      (lambda (b x)
        (let ([k (fx- b 1)])
          (fxlogand (fx+ x k) (fxlognot k)))))

    (define-record-type cat
      (nongenerative #{cat ker6am0odx7w7wg5n44fxof76-0})
      (sealed #t)
      (fields place            ; 'int, 'fp, 'mix ('fp + 'int/'fp), or 'stack
              regs             ; list of registers
              size/s           ; size in bytes for 'stack, list of sizes for register
              offsets          ; for 'mix mode
              by-reference))   ; #f or size of referenced data

    (define categorize-arguments
      (lambda (types varargs-after)
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
                            (loop (cdr types) (fx+ i 1) fpi next-varargs-after))])]))))))

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
        (letrec ([load-double-stack
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
                 [load-double-reg
                  (lambda (fpreg)
                    (lambda (x) ; unboxed
                      `(set! ,fpreg ,x)))]
                 [load-single-reg
                  (lambda (fpreg)
                    (lambda (x) ; unboxed
                      `(set! ,fpreg ,(%inline double->single ,x))))]
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
                                                          ;; copy to stack
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
                                (fx+ frame-size 8)
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
                                 (and ftd-result? (not pass-result-ptr?)) cat frame-size
                                 (add-deactivate
                                  adjust-active? t0 live* result-reg*
                                  (lambda (t0)
                                    `(inline ,(make-info-kill*-live* (add-caller-save-registers result-reg*) live*) ,%c-call ,t0))))))
                            (nanopass-case (Ltype Type) result-type
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
                   +---------------------------+<- 16-byte boundary
                   |       saved reg args      | 
                   +---------------------------+<- 8-byte boundary
                   |    by-reference copies    |
                   +---------------------------+<- 8-byte boundary
                   |     activatation state    | 
                   |       if necessary        |
                   +---------------------------+<- 8-byte boundary
                   |      &-return space       |
                   |       if necessary        |
                   +---------------------------+<- 8-byte boundary
                   |   callee-save regs + ra   | 
                   +---------------------------+<- 8-byte boundary
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
                            (loop (cdr types) (cdr cats) (cons load-reg locs) (fx+ reg-offset 8) stack-arg-offset copy-offset)))
                        (define use-stack
                          (lambda (load-stack)
                            (loop (cdr types) (cdr cats) (cons load-stack locs) reg-offset (fx+ stack-arg-offset 8) copy-offset)))
                        (nanopass-case (Ltype Type) type
                          [(fp-double-float)
                           (case (cat-place cat)
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
                                              ,(memory-to-memory %sp copy-offset x 0 (cat-by-reference cat) %scratch1)
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
                  [(fp-double-float)
		   (lambda (rhs) ; boxed
                     `(set! ,(car regs) ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp)))]
		  [(fp-single-float)
                   (lambda (rhs) ; boxed
                     `(set! ,(car regs) ,(%inline double->single ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp))))]
                  [(fp-void)
                   (lambda () `(nop))]
                  [(fp-ftd& ,ftd)
                   (cond
                     [(not synthesize-first?)
                      ;; we passed the pointer to be filled, so nothing more to do here
                      (safe-assert (null? regs))
                      (lambda () `(nop))]
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
                           (unpack-two-registers %sp return-offset regs (cat-size/s result-cat) (cat-offsets result-cat)))]
                        [(int)
                         (cond
                           [(null? (cdr regs))
                            (lambda ()
                              (memory-to-reg (car regs) %sp return-offset ($ftd-size ftd) ($ftd-unsigned? ftd) %scratch1))]
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
                                           (fx* (length result-regs) 8)
                                           0)]
                         [callee-save-bytes (fx* 8 (length callee-save-regs+ra))]
                         [total-unpadded-bytes (fx+ arg-reg-bytes copy-bytes active-state-bytes return-bytes callee-save-bytes)]
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
                             `(set! ,%tc (literal ,(make-info-literal #f 'entry (lookup-c-entry thread-context) 0))))))
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
