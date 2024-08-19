;;; arm64.ss

;;; SECTION 1: registers
;;; ABI:
;;;  Register usage:
;;;   r0-r7: C argument/result registers, caller-save
;;;   r8: indirect-result register, caller-save
;;;   r9-18: caller-save
;;;   r19-28: callee-save
;;;   r29: frame pointer, callee-save
;;;   r30: a.k.a. lr, link register
;;;   sp: stack pointer or (same register number) zero register
;;;   --------
;;;   v0-v7: FP registers used for C arguments/results, caller-save
;;;   v8-v15: callee-save for low 64 bits
;;;   v16-v31: caller-save
;;;  Alignment:
;;;   stack must be 16-byte aligned, essentially always

(define-registers
  (reserved
    [%tc  %r19                  #t 19 uptr]
    [%sfp %r20                  #t 20 uptr]
    [%ap  %r21                  #t 21 uptr]
    [%trap %r22                 #t 22 uptr])
  (allocable
    [%ac0 %r23                  #t 23 uptr]
    [%xp  %r24                  #t 24 uptr]
    [%ts  %r8                   #f  8 uptr]
    [%td  %r25                  #t 25 uptr]
    [%cp  %r26                  #t 26 uptr]
    [     %r0  %Carg1 %Cretval  #f  0 uptr]
    [     %r1  %Carg2           #f  1 uptr]
    [     %r2  %Carg3 %reify1   #f  2 uptr]
    [     %r3  %Carg4 %reify2   #f  3 uptr]
    [     %r4  %Carg5 %save1    #f  4 uptr]
    [     %r5  %Carg6           #f  5 uptr]
    [     %r6  %Carg7           #f  6 uptr]
    [     %r7  %Carg8           #f  7 uptr]
    [     %r9                   #f  9 uptr]
    [     %r12                  #f 12 uptr]
    [     %r13                  #f 13 uptr]
    [     %r14                  #f 14 uptr]
    [     %r15                  #f 15 uptr]
    [     %lr                   #f 30 uptr] ; %lr is trashed by 'c' calls including calls to hand-coded routines like get-room
    [%fp1           %v16        #f 16 fp]
    [%fp2           %v17        #f 17 fp]
    [%fp3           %v18        #f 18 fp]
    [%fp4           %v19        #f 19 fp]
    [%fp5           %v20        #f 20 fp]
    [%fp6           %v21        #f 21 fp]
  )
  (machine-dependent
    [%jmptmp %argtmp            #f 10 uptr]
    [%argtmp2                   #f 11 uptr]
    [%sp %real-zero             #t 31 uptr]
    [%Cfparg1 %Cfpretval      %v0   #f  0 fp]
    [%Cfparg2                 %v1   #f  1 fp]
    [%Cfparg3                 %v2   #f  2 fp]
    [%Cfparg4                 %v3   #f  3 fp]
    [%Cfparg5                 %v4   #f  4 fp]
    [%Cfparg6                 %v5   #f  5 fp]
    [%Cfparg7                 %v6   #f  6 fp]
    [%Cfparg8                 %v7   #f  7 fp]
    ;; etc., but FP registers v8-v15 are preserved
    ))

;;; SECTION 2: instructions
(module (md-handle-jump ; also sets primitive handlers
         mem->mem
         fpmem->fpmem
         coercible?
         coerce-opnd)
  (import asm-module)

  (define imm-funkymask?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (and (funkymask imm) #t)]
        [else #f])))

  (define imm-unsigned12?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (unsigned12? imm)]
        [else #f])))

  (define imm-neg-unsigned12?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (unsigned12? (- imm))]
        [else #f])))

  (define imm-constant?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) #t]
        [else #f])))

  (define-pass imm->negate-imm : (L15c Triv) (ir) -> (L15d Triv) ()
    (Triv : Triv (ir) -> Triv ()
      [(immediate ,imm) `(immediate ,(- imm))]
      [else (sorry! who "~s is not an immediate" ir)]))

  (define mref->mref
    (lambda (a k)
      (define return
        (lambda (x0 x1 imm type)
          ; arm load & store instructions support index or offset but not both
          (safe-assert (or (eq? x1 %zero) (eqv? imm 0)))
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm ,type)))))
      (nanopass-case (L15c Triv) a
        [(mref ,lvalue0 ,lvalue1 ,imm ,type)
         (lvalue->ur lvalue0
           (lambda (x0)
             (lvalue->ur lvalue1
               (lambda (x1)
                 (cond
                   [(and (eq? x1 %zero) (or (signed9? imm)
                                            (aligned-offset? imm)))
                    (return x0 %zero imm type)]
                   [(and (not (eq? x1 %zero)) (unsigned12? imm))
                    (let ([u (make-tmp 'u)])
                      (seq
                       (build-set! ,u (asm ,null-info ,(asm-add #f) ,x1 (immediate ,imm)))
                       (return x0 u 0 type)))]
                   [(and (not (eq? x1 %zero)) (unsigned12? (- imm)))
                    (let ([u (make-tmp 'u)])
                      (seq
                       (build-set! ,u (asm ,null-info ,(asm-sub #f) ,x1 (immediate ,(- imm))))
                       (return x0 u 0 type)))]
                   [else
                    (let ([u (make-tmp 'u)])
                      (seq
                        (build-set! ,u (immediate ,imm))
                        (if (eq? x1 %zero)
                            (return x0 u 0 type)
                            (seq
                              (build-set! ,u (asm ,null-info ,(asm-add #f) ,u ,x1))
                              (return x0 u 0 type)))))])))))])))

  (define mem->mem
    (lambda (a k)
      (cond
        [(literal@? a)
         (let ([u (make-tmp 'u)])
           (seq
             (build-set! ,u ,(literal@->literal a))
             (k (with-output-language (L15d Lvalue) `(mref ,u ,%zero 0 uptr)))))]
        [else (mref->mref a k)])))

  (define fpmem->fpmem mem->mem)

  ;; `define-instruction` code takes care of `ur` and `fpur`, to which
  ;; all type-compatible values must convert
  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (and (memq 'unsigned12 aty*) (imm-unsigned12? a))
             (and (memq 'neg-unsigned12 aty*) (imm-neg-unsigned12? a))
             (and (memq 'funkymask aty*) (imm-funkymask? a))
             (and (memq 'imm-constant aty*) (imm-constant? a))
             (and (memq 'mem aty*) (mem? a))
             (and (memq 'fpmem aty*) (fpmem? a))))]))

  ;; `define-instruction` doesn't try to cover `ur` and `fpur`
  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
           [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
           [(and (memq 'fpmem aty*) (fpmem? a)) (fpmem->fpmem a k)]
           [(and (memq 'unsigned12 aty*) (imm-unsigned12? a)) (k (imm->imm a))]
           [(and (memq 'neg-unsigned12 aty*) (imm-neg-unsigned12? a)) (k (imm->negate-imm a))]
           [(and (memq 'funkymask aty*) (imm-funkymask? a)) (k (imm->imm a))]
           [(and (memq 'imm-constant aty*) (imm-constant? a)) (k (imm->imm a))]
           [(memq 'ur aty*)
            (cond
              [(ur? a) (k a)]
              [(imm? a)
               (let ([u (make-tmp 'u)])
                 (seq
                   (build-set! ,u ,(imm->imm a))
                   (k u)))]
              [(mem? a)
               (mem->mem a
                 (lambda (a)
                   (let ([u (make-tmp 'u)])
                     (seq
                       (build-set! ,u ,a)
                       (k u)))))]
              [else (sorry! 'coerce-opnd "unexpected operand ~s" a)])]
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
               (sorry! 'coerce-opnd "unexpected operand ~s" a)])]
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
  (define asm-eq (asm-relop info-cc-eq #f))

  ; x is not the same as z in any clause that follows a clause where (x z)
  ; and y is coercible to one of its types, however:
  ; WARNING: do not assume that if x isn't the same as z then x is independent
  ; of z, since x might be an mref with z as it's base or index

  (define-instruction value (- -/ovfl -/eq -/pos)
    [(op (z ur) (x ur) (y unsigned12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub (not (eq? op '-))) ,x ,y))]
    [(op (z ur) (x ur) (y neg-unsigned12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (not (eq? op '-))) ,x ,y))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub (not (eq? op '-))) ,x ,y))])

  (define-instruction value (+ +/ovfl +/carry)
    [(op (z ur) (x ur) (y unsigned12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (not (eq? op '+))) ,x ,y))]
    [(op (z ur) (x ur) (y neg-unsigned12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub (not (eq? op '+))) ,x ,y))]
    [(op (z ur) (x unsigned12) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (not (eq? op '+))) ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (not (eq? op '+))) ,x ,y))])

  (define-instruction value (*)
    ; no imm form available
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,x ,y))])

  (define-instruction value (*/ovfl) ; z flag set iff no overflow
    ; no imm form available
    [(op (z ur) (x ur) (y ur))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-smulh ,x ,y))
         `(set! ,(make-live-info) ,z (asm ,null-info ,asm-mul ,x ,y))
         `(asm ,null-info ,asm-cmp/asr63 ,u ,z)))])

  (define-instruction value (/)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-div ,x ,y))])

  (define-instruction value (logand)
    [(op (z ur) (x ur) (y funkymask))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,x ,y))]
    [(op (z ur) (x funkymask) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,x ,y))])

  (let ()
    (define select-op (lambda (op) (if (eq? op 'logor) asm-logor asm-logxor)))
    (define-instruction value (logor logxor)
      [(op (z ur) (x funkymask) (y ur))
       `(set! ,(make-live-info) ,z (asm ,info ,((select-op op) #f) ,y ,x))]
      [(op (z ur) (x ur) (y funkymask ur))
       `(set! ,(make-live-info) ,z (asm ,info ,((select-op op) #f) ,x ,y))]))

  (define-instruction value (lognot)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))])

  (define-instruction value (sll srl sra)
    [(op (z ur) (x ur) (y imm-constant ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-shiftop op) ,x ,y))])

  (define-instruction value popcount
    [(op (z ur) (x ur))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-popcount ,x ,u))))])

  (define-instruction value (move)
    [(op (z mem) (x ur))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem imm-constant))
     `(set! ,(make-live-info) ,z ,x)])

  (let ()
    (define build-lea1
      (lambda (info z x)
        (let ([offset (info-lea-offset info)])
          (with-output-language (L15d Effect)
            (cond
              [(unsigned12? offset)
               `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,x (immediate ,offset)))]
              [(unsigned12? (- offset))
               `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub #f) ,x (immediate ,(- offset))))]
              [else
               (let ([u (make-tmp 'u)])
                 (seq
                  `(set! ,(make-live-info) ,u (immediate ,offset))
                  `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,x ,u))))])))))

    (define-instruction value lea1
      ;; NB: would be simpler if offset were explicit operand
      ;; NB: why not one version of lea with %zero for y in lea1 case?
      [(op (z ur) (x ur)) (build-lea1 info z x)])

    (define-instruction value lea2
      ;; NB: would be simpler if offset were explicit operand
      [(op (z ur) (x ur) (y ur))
       (let ([u (make-tmp 'u)])
         (seq
          (build-lea1 info u x)
          `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,y ,u))))]))

  (define-instruction value (sext8 sext16 sext32 zext8 zext16 zext32)
    [(op (z ur) (x ur)) `(set! ,(make-live-info) ,z (asm ,info ,(asm-move/extend op) ,x))])

  (let ()
    (define imm-zero (with-output-language (L15d Triv) `(immediate 0)))
    (define load/store
      (lambda (x y w type k) ; x ur, y ur, w ur or imm
        (with-output-language (L15d Effect)
          (if (ur? w)
              (if (eq? y %zero)
                  (k x w imm-zero)
                  (let ([u (make-tmp 'u)])
                    (seq
                      `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,y ,w))
                      (k x u imm-zero))))
              (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
                (cond
                  [(and (eq? y %zero)
                        (aligned-offset? n (case type
                                             [(unsigned-32 integer-32) 2]
                                             [(unsigned-16 integer-16) 1]
                                             [(unsigned-8 integer-8) 0]
                                             [else 3])))
                   (let ([w (in-context Triv `(immediate ,n))])
                     (k x y w))]
                  [(and (eq? y %zero) (signed9? n))
                   (let ([w (in-context Triv `(immediate ,n))])
                     (k x y w))]
                  [(and (not (eq? y %zero)) (unsigned12? n))
                   (let ([w (in-context Triv `(immediate ,n))])
                     (let ([u (make-tmp 'u)])
                       (seq
                        `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,y ,w))
                        (k x u imm-zero))))]
                  [(and (not (eq? y %zero)) (unsigned12? (- n)))
                   (let ([w (in-context Triv `(immediate ,(- n)))])
                     (let ([u (make-tmp 'u)])
                       (seq
                        `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-sub #f) ,y ,w))
                        (k x u imm-zero))))]
                  [else
                   (let ([u (make-tmp 'u)])
                     (seq
                       `(set! ,(make-live-info) ,u (immediate ,n))
                       (if (eq? y %zero)
                           (k x u imm-zero)
                           (seq
                            `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,u))
                            (k u y imm-zero)))))]))))))
    (define-instruction value (load)
      [(op (z ur) (x ur) (y ur) (w ur imm-constant))
       (let ([type (info-load-type info)])
         (load/store x y w type
           (lambda (x y w)
             (let ([instr `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-load type) ,x ,y ,w))])
               (if (info-load-swapped? info)
                   (seq
                     instr
                     `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-swap type) ,z)))
                   instr)))))])
    (define-instruction effect (store)
      [(op (x ur) (y ur) (w ur imm-constant) (z ur))
       (let ([type (info-load-type info)])
         (load/store x y w type
           (lambda (x y w)
             (if (info-load-swapped? info)
                 (let ([u (make-tmp 'unique-bob)])
                   (seq
                     `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-swap type) ,z))
                     `(asm ,null-info ,(asm-store type) ,x ,y ,w ,u)))
                 `(asm ,null-info ,(asm-store type) ,x ,y ,w ,z)))))]))

  (define-instruction value (load-single->double)
    [(op (x fpur) (y fpmem))
     (let ([u (make-tmp 'u 'fp)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-fpmove-single ,y))
        `(set! ,(make-live-info) ,x (asm ,info ,(asm-fl-cvt 'single->double) ,u))))])

  (define-instruction effect (store-double->single)
    [(op (x fpmem) (y fpur))
     (let ([u (make-tmp 'u 'fp)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-fl-cvt 'double->single) ,y))
        `(asm ,info ,asm-fpmove-single ,x ,u)))])

  (define-instruction effect (store-single)
    [(op (x fpmem) (y fpur))
     `(asm ,info ,asm-fpmove-single ,x ,y)])

  (define-instruction value (load-single)
    [(op (x fpur) (y fpmem))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove-single ,y))])

  (define-instruction value (single->double double->single)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fl-cvt op) ,y))])

  (define-instruction value (fpt)
    [(op (x fpur) (y ur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpt ,y))])

  (define-instruction value (fptrunc)
    [(op (x ur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fptrunc ,y))])

  (define-instruction value (fpsingle)
    [(op (x fpur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsingle ,y))])

  (define-instruction value (fpmove)
    [(op (x fpmem) (y fpur)) `(set! ,(make-live-info) ,x ,y)]
    [(op (x fpur) (y fpmem fpur)) `(set! ,(make-live-info) ,x ,y)])

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

  (define-instruction value (fp+ fp- fp/ fp*)
    [(op (x fpur) (y fpur) (z fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpop-2 op) ,y ,z))])

  (define-instruction value (fpsqrt)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsqrt ,y))])

  (define-instruction pred (fp= fp< fp<=)
    [(op (x fpur) (y fpur))
     (let ([info (make-info-condition-code op #f #f)])
       (values '() `(asm ,info ,(asm-fp-relop info) ,x ,y)))])

  (define-instruction effect (inc-cc-counter)
    [(op (x ur) (w unsigned12) (z ur unsigned12))
     (let ([u1 (make-tmp 'u1)] [u2 (make-tmp 'u2)])
       (seq
        `(set! ,(make-live-info) ,u1 (asm ,null-info ,(asm-add #f) ,x ,w))
        `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
        `(asm ,null-info ,asm-inc-cc-counter ,u1 ,z ,u2)))])

  (define-instruction effect (inc-profile-counter)
    [(op (x mem) (y unsigned12))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u ,x)
         `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,u ,y))
         `(set! ,(make-live-info) ,x ,u)))])

  (define-instruction value (read-time-stamp-counter)
    [(op (z ur)) `(set! ,(make-live-info) ,z (asm ,null-info
                                                  ;; CNTPCT_EL0
                                                  ,(asm-read-counter #b11 #b011 #b1110 #b0000 #b001)))])

  (define-instruction value (read-performance-monitoring-counter)
    [(op (z ur) (x ur)) `(set! ,(make-live-info) ,z (immediate 0))])

  ;; no kills since we expect to be called when all necessary state has already been saved
  (define-instruction value (get-tc)
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,info ,asm-get-tc ,ulr))))])

  (define-instruction value activate-thread
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,info ,asm-activate-thread ,ulr))))])

  (define-instruction effect deactivate-thread
    [(op)
     (let ([ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-deactivate-thread ,ulr)))])

  (define-instruction effect unactivate-thread
    [(op (x ur))
     (safe-assert (eq? x %Carg1))
     (let ([ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-unactivate-thread ,x ,ulr)))])

  (define-instruction value (asmlibcall)
    [(op (z ur))
     (if (info-asmlib-save-ra? info)
         `(set! ,(make-live-info) ,z (asm ,info ,(asm-library-call (info-asmlib-libspec info) #t) ,(info-kill*-live*-live* info) ...))
         (let ([ulr (make-precolored-unspillable 'ulr %lr)])
           (seq
            `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
            `(set! ,(make-live-info) ,z (asm ,info ,(asm-library-call (info-asmlib-libspec info) #f) ,ulr ,(info-kill*-live*-live* info) ...)))))])

  (define-instruction effect (asmlibcall!)
    [(op)
     (if (info-asmlib-save-ra? info)
         (let ([ulr (make-precolored-unspillable 'ulr %lr)])
           `(asm ,info ,(asm-library-call! (info-asmlib-libspec info) #t) ,(info-kill*-live*-live* info) ...))
         (let ([ulr (make-precolored-unspillable 'ulr %lr)])
           (seq
            `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
            `(asm ,info ,(asm-library-call! (info-asmlib-libspec info) #f) ,ulr ,(info-kill*-live*-live* info) ...))))])

  (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
  (define-instruction effect (c-simple-call)
    [(op)
     (if (info-c-simple-call-save-ra? info)
         `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info) #t))
         (let ([ulr (make-precolored-unspillable 'ulr %lr)])
           (seq
            `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
            `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info) #f) ,ulr))))])

  (define-instruction pred (eq? u< < > <= >=)
    [(op (y unsigned12) (x ur))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #t #t))])
       (values '() `(asm ,info ,(asm-relop info #f) ,x ,y)))]
    [(op (y neg-unsigned12) (x ur))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #t #t))])
       (values '() `(asm ,info ,(asm-relop info #t) ,x ,y)))]
    [(op (x ur) (y ur unsigned12))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info #f) ,x ,y)))]
    [(op (x ur) (y neg-unsigned12))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info #t) ,x ,y)))])

  (define-instruction pred (condition-code)
    [(op) (values '() `(asm ,info ,(asm-condition-code info)))])

  (define-instruction pred (type-check?)
    [(op (x ur) (mask funkymask ur) (type unsigned12 ur))
     (let ([tmp (make-tmp 'u)])
       (values
         (with-output-language (L15d Effect)
           `(set! ,(make-live-info) ,tmp (asm ,null-info ,(asm-logand #f) ,x ,mask)))
         `(asm ,info-cc-eq ,asm-eq ,tmp ,type)))])

  (define-instruction pred (logtest log!test)
    [(op (x funkymask) (y ur))
     (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,y ,x))]
    [(op (x ur) (y ur funkymask))
     (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,x ,y))])

  (let ()
    (define lea->reg
      (lambda (x y w k)
        (with-output-language (L15d Effect)
          (define add-offset
            (lambda (r)
              (let ([i (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
                (cond
                  [(eqv? i 0) (k r)]
                  [(unsigned12? i)
                   (let ([u (make-tmp 'u)])
                     (seq
                      `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,r ,w))
                      (k u)))]
                  [else
                   (let ([u (make-tmp 'u)])
                     (seq
                      `(set! ,(make-live-info) ,u ,w)
                      `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,r ,u))
                      (k u)))]))))
          (if (eq? y %zero)
              (add-offset x)
              (let ([u (make-tmp 'u)])
                (seq
                  `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,y))
                  (add-offset u)))))))
    ;; NB: compiler implements init-lock! and unlock! as word store of zero
    (define-instruction pred (lock!)
      [(op (x ur) (y ur) (w imm-constant))
       (let ([u (make-tmp 'u)]
             [u2 (make-tmp 'u2)])
         (values
           (lea->reg x y w
             (lambda (r)
               (with-output-language (L15d Effect)
                 (seq
                   `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
                   `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
                   `(asm ,null-info ,asm-lock ,r ,u ,u2)))))
           `(asm ,info-cc-eq ,asm-eq ,u (immediate 0))))])
    (define-instruction effect (locked-incr! locked-decr!)
      [(op (x ur) (y ur) (w imm-constant))
       (lea->reg x y w
         (lambda (r)
           (let ([u1 (make-tmp 'u1)] [u2 (make-tmp 'u2)])
             (seq
               `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-kill))
               `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
               `(asm ,null-info ,(asm-lock+/- op) ,r ,u1 ,u2)))))])
    (define-instruction effect (cas)
      [(op (x ur) (y ur) (w imm-constant) (old ur) (new ur))
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
    ;; NB: use sqrt or something like that?
    [(op) '()])

  (define-instruction effect (debug)
    [(op)
     `(asm ,info ,asm-debug)])

  (define-instruction effect (c-call)
    [(op (x ur))
     (let ([ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-indirect-call ,x ,ulr ,(info-kill*-live*-live* info) ...)))])

  (define-instruction effect (pop-multiple)
    [(op) `(asm ,info ,(asm-pop-multiple (info-kill*-kill* info)))])

  (define-instruction effect (push-multiple)
    [(op) `(asm ,info ,(asm-push-multiple (info-kill*-live*-live* info)))])

  (define-instruction effect (pop-fpmultiple)
    [(op) `(asm ,info ,(asm-pop-fpmultiple (info-kill*-kill* info)))])

  (define-instruction effect (push-fpmultiple)
    [(op) `(asm ,info ,(asm-push-fpmultiple (info-kill*-live*-live* info)))])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,(asm-push-fpmultiple (list %Cfpretval)))])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,(asm-pop-fpmultiple (list %Cfpretval)))])

  (define-instruction effect (invoke-prelude)
    [(op) `(set! ,(make-live-info) ,%tc ,%Carg1)])
)

;;; SECTION 3: assembler
(module asm-module (; required exports
                     asm-move asm-move/extend asm-load asm-store asm-swap asm-library-call asm-library-call! asm-library-jump
                     asm-mul asm-smulh asm-div asm-add asm-sub asm-logand asm-logor asm-logxor
                     asm-pop-multiple asm-shiftop asm-logand asm-lognot asm-cmp/asr63 asm-popcount
                     asm-logtest asm-fp-relop asm-relop asm-push-multiple asm-push-fpmultiple asm-pop-fpmultiple
                     asm-indirect-jump asm-literal-jump
                     asm-direct-jump asm-return-address asm-jump asm-conditional-jump
                     asm-indirect-call asm-condition-code
                     asm-fpmove-single asm-fl-cvt asm-fpt asm-fpmove asm-fpcastto asm-fpcastfrom
                     asm-fptrunc asm-fpsingle
                     asm-lock asm-lock+/- asm-cas asm-fence
                     asm-fpop-2 asm-fpsqrt asm-c-simple-call
                     asm-return asm-c-return asm-size
                     asm-enter asm-foreign-call asm-foreign-callable
                     asm-debug
                     asm-read-counter
                     asm-inc-cc-counter
                     signed9? unsigned12? aligned-offset? funkymask shifted16
                     ; threaded version specific
                     asm-get-tc
                     asm-activate-thread asm-deactivate-thread asm-unactivate-thread
                     ; machine dependent exports
                     asm-kill)

  (define ax-register?
    (case-lambda
      [(x) (record-case x [(reg) r #t] [else #f])]
      [(x reg) (record-case x [(reg) r (eq? r reg)] [else #f])]))

  (define-who ax-ea-reg-code
    (lambda (ea)
      (record-case ea
        [(reg) r (reg-mdinfo r)]
        [else (sorry! who "ea=~s" ea)])))

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

  (define-who ax-imm-data
    (lambda (ea)
      (record-case ea
        [(imm) (n) n]
        [else (sorry! who "ax-imm-data ea=~s" ea)])))

  ; define-op sets up assembly op macros--
  ; the opcode and all other expressions are passed to the specified handler--
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

  ;;; note that the assembler isn't clever--you must be very explicit about
  ;;; which flavor you want, and there are a few new varieties introduced
  ;;; (commented-out opcodes are not currently used by the assembler--
  ;;; spaces are left to indicate possible size extensions)

  (define-op movzi   movzi-op #b10) ; 16-bit immediate, shifted
  (define-op movki   movzi-op #b11) ; 16-bit immediate, shifted
  (define-op movi    movi-op)  ; immediate encoded as a mask

  (define-op addi  add-imm-op  #b0) ; selector is at bit 30 (op)
  (define-op subi  add-imm-op  #b1)

  (define-op andi  logical-imm-op  #b00)
  (define-op orri  logical-imm-op  #b01)
  (define-op eori  logical-imm-op  #b10)

  (define-op add   binary-op  #b0)
  (define-op sub   binary-op  #b1)

  (define-op and   logical-op  #b00)
  (define-op orr   logical-op  #b01)
  (define-op eor   logical-op  #b10)

  (define-op cmp        cmp-op  #b1101011 #b00 0)
  (define-op tst        cmp-op  #b1101010 #b00 0)
  (define-op cmp/asr63  cmp-op  #b1101011 #b10 63)

  (define-op cmpi  cmp-imm-op #b1) ; selector is at bit 30 (op)
  (define-op cmni  cmp-imm-op #b0)
  (define-op tsti  logical-imm-op #b11 #f `(reg . ,%real-zero))

  (define-op mov   mov-op  #b1 #b0) ; selectors are a bit 31 (sf) and 21 (N)
  (define-op movw  mov-op  #b0 #b0)
  (define-op mvn   mov-op  #b1 #b1)

  (define-op lsli  shifti-op #b10 'l) ; selector is at bit 29 (opc)
  (define-op lsri  shifti-op #b10 'r)
  (define-op asri  shifti-op #b00 'r)

  (define-op lsl  shift-op #b00) ; selector is at bit 10 (op2)
  (define-op lsr  shift-op #b01)
  (define-op asr  shift-op #b10)

  (define-op sxtb extend-op  #b100 #b1 #b000111) ; selectors are at bits 29 (sfc+opc), 22 (N), and 10 (imms)
  (define-op sxth extend-op  #b100 #b1 #b001111)
  (define-op sxtw extend-op  #b100 #b1 #b011111)
  (define-op uxtb extend-op  #b010 #b0 #b000111)
  (define-op uxth extend-op  #b010 #b0 #b001111)

  (define-op mul   mul-op  #b000) ; selector is at bit 21
  (define-op smulh mul-op  #b010)

  (define-op sdiv  div-op)

  (define-op cnt    cnt-op)
  (define-op addv.b addv.b-op)

  ;; scaled variants (offset must be aligned):
  (define-op ldri    load-imm-op  3 #b11 #b0 #b01) ; selectors are at bits 30 (size), 26, and 22 (opc)
  (define-op ldrbi   load-imm-op  0 #b00 #b0 #b01)
  (define-op ldrhi   load-imm-op  1 #b01 #b0 #b01)
  (define-op ldrwi   load-imm-op  2 #b10 #b0 #b01)
  (define-op ldrfi   load-imm-op  3 #b11 #b1 #b01)
  (define-op ldrfsi  load-imm-op  2 #b10 #b1 #b01) ; single-precision

  (define-op ldrsbi  load-imm-op  0 #b00 #b0 #b10)
  (define-op ldrshi  load-imm-op  1 #b01 #b0 #b10)
  (define-op ldrswi  load-imm-op  2 #b10 #b0 #b10)

  (define-op stri    load-imm-op  3 #b11 #b0 #b00)
  (define-op strbi   load-imm-op  0 #b00 #b0 #b00)
  (define-op strhi   load-imm-op  1 #b01 #b0 #b00)
  (define-op strwi   load-imm-op  2 #b10 #b0 #b00)
  (define-op strfi   load-imm-op  3 #b11 #b1 #b00)
  (define-op strfsi  load-imm-op  2 #b10 #b1 #b00) ; single-precision

  ;; unscaled variants (offset must be signed9):
  (define-op lduri    load-unscaled-imm-op  #b11 #b0 #b01) ; selectors are at bits 30 (size), 26, and 22 (opc)
  (define-op ldurbi   load-unscaled-imm-op  #b00 #b0 #b01)
  (define-op ldurhi   load-unscaled-imm-op  #b01 #b0 #b01)
  (define-op ldurwi   load-unscaled-imm-op  #b10 #b0 #b01)
  (define-op ldurfi   load-unscaled-imm-op  #b11 #b1 #b01)
  (define-op ldurfsi  load-unscaled-imm-op  #b10 #b1 #b01) ; single-precision

  (define-op ldursbi  load-unscaled-imm-op  #b00 #b0 #b10)
  (define-op ldurshi  load-unscaled-imm-op  #b01 #b0 #b10)
  (define-op ldurswi  load-unscaled-imm-op  #b10 #b0 #b10)

  (define-op sturi    load-unscaled-imm-op  #b11 #b0 #b00)
  (define-op sturbi   load-unscaled-imm-op  #b00 #b0 #b00)
  (define-op sturhi   load-unscaled-imm-op  #b01 #b0 #b00)
  (define-op sturwi   load-unscaled-imm-op  #b10 #b0 #b00)
  (define-op sturfi   load-unscaled-imm-op  #b11 #b1 #b00)
  (define-op sturfsi  load-unscaled-imm-op  #b10 #b1 #b00) ; single-precision

  (define-op ldr     load-op     #b11 #b0 #b01)  ; selectors are at bits 30 (size), 26, and 22 (opc)
  (define-op ldrw    load-op     #b10 #b0 #b01)
  (define-op ldrh    load-op     #b01 #b0 #b01)
  (define-op ldrb    load-op     #b00 #b0 #b01)
  (define-op ldrf    load-op     #b11 #b1 #b01)
  (define-op ldrfs   load-op     #b10 #b1 #b01)

  (define-op ldrsw   load-op     #b10 #b0 #b10)
  (define-op ldrsh   load-op     #b01 #b0 #b10)
  (define-op ldrsb   load-op     #b00 #b0 #b10)

  (define-op str     load-op     #b11 #b0 #b00)
  (define-op strw    load-op     #b10 #b0 #b00)
  (define-op strh    load-op     #b01 #b0 #b00)
  (define-op strb    load-op     #b00 #b0 #b00)
  (define-op strf    load-op     #b11 #b1 #b00)
  (define-op strfs   load-op     #b10 #b1 #b00)

  (define-op ldr/postidx  load-idx-op  #b01 #b0 #b01) ; selectors are at bits 22 (opc), 26, and 10
  (define-op str/preidx   load-idx-op  #b00 #b0 #b11)

  (define-op ldrf/postidx load-idx-op  #b01 #b1 #b01)
  (define-op strf/preidx  load-idx-op  #b00 #b1 #b11)

  (define-op ldrp/postidx loadp-idx-op  #b10 #b0 #b001 #b1) ; selectors are at bits 30 (opc), 26, 23, and 22 (L)
  (define-op strp/preidx  loadp-idx-op  #b10 #b0 #b011 #b0)

  (define-op ldrpf/postidx loadp-idx-op  #b01 #b1 #b001 #b1)
  (define-op strpf/preidx  loadp-idx-op  #b01 #b1 #b011 #b0)

  (define-op ldxr   ldxr-op      #b1 `(reg . ,%real-zero))
  (define-op stxr   ldxr-op      #b0)

  (define-op dmbst    dmb-op #b1110)
  (define-op dmbish   dmb-op #b1011)
  (define-op dmbishld dmb-op #b1001)
  (define-op dmbishst dmb-op #b1010)

  (define-op bnei  branch-imm-op       (ax-cond 'ne))
  (define-op beqi  branch-imm-op       (ax-cond 'eq))
  (define-op brai  branch-imm-op       (ax-cond 'al))

  (define-op br    branch-reg-op       #b00)
  (define-op blr   branch-reg-op       #b01)

  (define-op b     branch-always-label-op)

  (define-op beq   branch-label-op     (ax-cond 'eq))
  (define-op bne   branch-label-op     (ax-cond 'ne))
  (define-op blt   branch-label-op     (ax-cond 'lt))
  (define-op ble   branch-label-op     (ax-cond 'le))
  (define-op bgt   branch-label-op     (ax-cond 'gt))
  (define-op bge   branch-label-op     (ax-cond 'ge))
  (define-op bcc   branch-label-op     (ax-cond 'cc))
  (define-op bcs   branch-label-op     (ax-cond 'cs))
  (define-op bvc   branch-label-op     (ax-cond 'vc))
  (define-op bvs   branch-label-op     (ax-cond 'vs))
  (define-op bls   branch-label-op     (ax-cond 'ls))
  (define-op bhi   branch-label-op     (ax-cond 'hi))

  (define-op adr   adr-op)
  (define-op ret   ret-op)

  (define-op fcvt.s->d  fcvt-op  #b00 #b01)
  (define-op fcvt.d->s  fcvt-op  #b01 #b00)

  (define-op fcvtzs  fdcvt-op  #b11 #b000) ; selectors are at bits 19 (mode) and 1 6(opcode)
  (define-op scvtf   fdcvt-op  #b00 #b010)

  (define-op fmov       fmov-op  #b0 #b000 #b1) ; selectors are at bits 31, 16, and 14
  (define-op fmov.f->g  fmov-op  #b1 #b110 #b0)
  (define-op fmov.g->f  fmov-op  #b1 #b111 #b0)

  (define-op fcmp fcmp-op)

  (define-op rev    rev-op  #b11) ; selector is at bit 10 (opc)
  (define-op rev16  rev-op  #b01)
  (define-op rev32  rev-op  #b10)

  (define-op mrs  mrs-op)

  (define-op und  und-op)

  (define-op fadd  f-arith-op  #b0010) ; selector is at bit 12
  (define-op fsub  f-arith-op  #b0011)
  (define-op fmul  f-arith-op  #b0000)
  (define-op fdiv  f-arith-op  #b0001)

  (define-op fsqrt fsqrt-op)

  (define movzi-op
    (lambda (op opc dest imm shift code*)
      (emit-code (op dest imm shift code*)
        [31 #b1]
        [29 opc]
        [23 #b100101]
        [21 shift] ; `shift` is implicitly multiplied by 16
        [5  imm]
        [0  (ax-ea-reg-code dest)])))

  (define movi-op
    (lambda (op dest imm n+immr+imms code*)
      (let ([n (car n+immr+imms)]
            [immr (cadr n+immr+imms)]
            [imms (caddr n+immr+imms)])
        (emit-code (op dest imm n+immr+imms code*)
          [23 #b101100100]
          [22 n]
          [16 immr]
          [10 imms]
          [5  #b11111]
          [0  (ax-ea-reg-code dest)]))))

  (define add-imm-op
    (lambda (op opcode set-cc? dest src imm code*)
      (emit-code (op dest src imm (and set-cc? #t) code*)
        [31 #b1]
        [30 opcode]
        [29 (if set-cc? #b1 #b0)]
        [24 #b10001]
        [22 #b00] ; shift
        [10 imm]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define logical-imm-op
    (lambda (op opcode set-cc? dest src imm code*)
      (safe-assert (not set-cc?)) ; but opcode may imply setting condition codes
      (let ([n+immr+imms (funkymask imm)])
        (let ([n (car n+immr+imms)]
              [immr (cadr n+immr+imms)]
              [imms (caddr n+immr+imms)])
          (emit-code (op dest src imm code*)
            [31 #b1]
            [29 opcode]
            [23 #b100100]
            [22 n]
            [16 immr]
            [10 imms]
            [5  (ax-ea-reg-code src)]
            [0  (ax-ea-reg-code dest)])))))

  (define binary-op
    (lambda (op opcode set-cc? dest src0 src1 code*)
      (emit-code (op dest src0 src1 (and set-cc? #t) code*)
        [31 #b1]
        [30 opcode]
        [29 (if set-cc? #b1 #b0)]
        [24 #b01011]
        [22 #b00] ; shift type (applied to src1)
        [21 #b0]
        [16 (ax-ea-reg-code src1)]
        [10 #b000000] ; shift amount
        [5  (ax-ea-reg-code src0)]
        [0  (ax-ea-reg-code dest)])))

  (define logical-op
    (lambda (op opcode set-cc? dest src0 src1 code*)
      (safe-assert (not set-cc?))
      (emit-code (op dest src0 src1 code*)
        [31 #b1]
        [29 opcode]
        [24 #b01010]
        [22 #b00] ; shift type (applied to src1)
        [21 #b0]
        [16 (ax-ea-reg-code src1)]
        [10 #b000000] ; shift amount
        [5  (ax-ea-reg-code src0)]
        [0  (ax-ea-reg-code dest)])))

  (define cmp-op
    (lambda (op opcode shift-type shift-amt src0 src1 code*)
      (emit-code (op src0 src1 code*)
        [31 #b1]
        [24 opcode]
        [22 shift-type] ; applied to src1
        [21 #b0]
        [16 (ax-ea-reg-code src1)]
        [10 shift-amt]
        [5  (ax-ea-reg-code src0)]
        [0  #b11111])))

  (define cmp-imm-op
    (lambda (op opc src imm code*)
      (safe-assert (unsigned12? imm))
      (emit-code (op src imm code*)
        [31 #b1]
        [30 opc]
        [24 #b110001]
        [22 #b00] ; shift amount (applied to immediate)
        [10 imm]
        [5  (ax-ea-reg-code src)]
        [0  #b11111])))

  (define mov-op
    (lambda (op sz neg dest src code*)
      (emit-code (op dest src code*)
        [31 sz]
        [22 #b010101000]
        [21 neg]
        [16 (ax-ea-reg-code src)]
        [5  #b11111]
        [0  (ax-ea-reg-code dest)])))

  (define shifti-op
    (lambda (op opcode dir dest src imm code*)
      (emit-code (op dest src imm code*)
        [31 #b1]
        [29 opcode]
        [22 #b1001101]
        [16 (if (eq? dir 'l)
                (fx- 64 imm)
                imm)]
        [10 (if (eq? dir 'l)
                (fx- 63 imm)
                63)]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define shift-op
    (lambda (op opcode dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        [29 #b100]
        [21 #b11010110]
        [16 (ax-ea-reg-code src1)]
        [12 #b0010]
        [10 opcode]
        [5  (ax-ea-reg-code src0)]
        [0  (ax-ea-reg-code dest)])))

  (define extend-op
    (lambda (op sf+opc n imms-as-op2 dest src code*)
      (emit-code (op dest src code*)
        [29 sf+opc]
        [23 #b100110]
        [22 n]
        [16 #b000000]
        [10 imms-as-op2]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define mul-op
    (lambda (op opcode dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        [29 #b100]
        [24 #b11011]
        [21 opcode]
        [16 (ax-ea-reg-code src1)]
        [10 #b011111]
        [5  (ax-ea-reg-code src0)]
        [0  (ax-ea-reg-code dest)])))

  (define div-op
    (lambda (op dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        [29 #b100]
        [21 #b11010110]
        [16 (ax-ea-reg-code src1)]
        [10 #b000011]
        [5  (ax-ea-reg-code src0)]
        [0  (ax-ea-reg-code dest)])))

  (define cnt-op
    (lambda (op dest src code*)
      (emit-code (op dest src code*)
        [29 #b000]
        [24 #b01110]
        [22 #b00] ; size
        [17 #b10000]
        [10 #b0010110]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define addv.b-op
    (lambda (op dest src code*)
      (emit-code (op dest src code*)
        [29 #b000]
        [24 #b01110]
        [22 #b00] ; size: 00 => b
        [17 #b11000]
        [10 #b1101110]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define load-imm-op
    (lambda (op scale size kind opc dest src imm code*)
      (emit-code (op dest src imm code*)
        [30 size]
        [27 #b111]
        [26 kind]
        [24 #b01]
        [22 opc]
        [10 (fxsrl imm scale)]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define load-unscaled-imm-op
    (lambda (op size kind opc dest src imm code*)
      (emit-code (op dest src imm code*)
        [30 size]
        [27 #b111]
        [26 kind]
        [24 #b00]
        [22 opc]
        [21 #b0]
        [12 (fxand imm #x1FF)]
        [10 #b00]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define load-op
    (lambda (op size kind opc dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        [30 size]
        [27 #b111]
        [26 kind]
        [24 #b00]
        [22 opc]
        [21 #b1]
        [16 (ax-ea-reg-code src1)]
        [13 #b011] ; option, where #x011 => 64-bit source address
        [12 #b0] ; shift
        [10 #b10]
        [5  (ax-ea-reg-code src0)]
        [0  (ax-ea-reg-code dest)])))

  (define load-idx-op
    (lambda (op opc mode idx dest src imm code*)
      (emit-code (op dest src imm code*)
        [30 #b11]
        [27 #b111]
        [26 mode]
        [24 #b00]
        [22 opc]
        [21 #b0]
        [12 (fxand imm (fx- (fxsll 1 9) 1))]
        [10 idx]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

    (define loadp-idx-op
      (lambda (op opc mode opx l dest0 dest1 src imm code*)
        (emit-code (op dest0 dest1 src imm code*)
          [30 opc]
          [27 #b101]
          [26 mode]
          [23 opx]
          [22 l]
          [15 (fxand (fxsrl imm 3) (fx- (fxsll 1 7) 1))]
          [10 (ax-ea-reg-code dest1)]
          [5  (ax-ea-reg-code src)]
          [0 (ax-ea-reg-code dest0)])))

  (define ldxr-op
    (lambda (op mode dest2 dest src code*)
      (emit-code (op dest2 dest src code*)
        [30 #b11]
        [23 #b0010000]
        [22 mode]
        [21 0]
        [16 (ax-ea-reg-code dest2)]
        [15 #b0]
        [10 #b11111]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define dmb-op
    (lambda (op mode code*)
      (emit-code (op code*)
        [22 #b1101010100]
        [16 #b000011]
        [12 #b0011]
        [8  mode]
        [5  #b101]
        [0  #b11111])))

  (define branch-imm-op
    (lambda (op cond-bits imm code*)
      (safe-assert (branch-disp? imm))
      (emit-code (op imm code*)
        [24 #b01010100]
        [5  (fxand (fxsra imm 2) (fx- (fxsll 1 19) 1))]
        [4  #b0]
        [0  cond-bits])))

  (define branch-reg-op
    (lambda (op opcode reg code*)
      (emit-code (op reg code*)
        [24 #b11010110]
        [23 #b0]
        [21 opcode]
        [16 #b11111]
        [12 #b0000]
        [10 #b00]
        [5  (ax-ea-reg-code reg)]
        [0  #b00000])))

  (define-who branch-always-label-op
    (lambda (op dest code*)
      (record-case dest
        [(label) (offset l)
         (safe-assert (uncond-branch-disp? offset))
         (emit-code (op dest code*)
           [26 #b000101]
           [0  (fxand (fxsra (fx+ offset 4) 2) (fx- (fxsll 1 26) 1))])]
        [else (sorry! who "unexpected dest ~s" dest)])))

  (define-who branch-label-op
    (lambda (op cond-bits dest code*)
      (define (emit-branch offset)
        (safe-assert (branch-disp? (+ offset 4)))
        (emit-code (op dest code*)
          [24 #b01010100]
          [5  (fxand (fxsra (fx+ offset 4) 2) (fx- (fxsll 1 19) 1))]
          [4  #b0]
          [0  cond-bits]))
      (record-case dest
        [(label) (offset l) (emit-branch offset)]
        [(imm) (n) (emit-branch n)] ; generated for long branches
        [else (sorry! who "unexpected dest ~s" dest)])))

  (define adr-op
    (lambda (op dest imm code*)
      (emit-code (op dest imm code*)
        [31 #b0]
        [29 (fxand imm #b11)]
        [24 #b10000]
        [5  (fxand (fxsra imm 2) (fx- (fxsll 1 19) 1))]
        [0  (ax-ea-reg-code dest)])))

  (define ret-op
    (lambda (op src code*)
      (emit-code (op src code*)
        [25 #b1101011]
        [21 #b0010]
        [16 #b11111]
        [12 #b0000]
        [10 #b00]
        [5  (ax-ea-reg-code src)]
        [0  #b00000])))

  (define fcvt-op
    (lambda (op type opc dest src code*)
      (emit-code (op dest src code*)
        [24 #b00011110]
        [22 type]
        [17 #b10001]
        [15 opc]
        [10 #b10000]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define fdcvt-op
    (lambda (op mode opcode dest src code*)
      (emit-code (op dest src code*)
        [29 #b100]
        [24 #b11110]
        [22 #b01] ; type
        [21 #b1]
        [19 mode]
        [16 opcode]
        [10 #b000000]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define fmov-op
    (lambda (op sf opcode opsel dest src code*)
      (emit-code (op dest src code*)
        [31 sf]
        [24 #b0011110]
        [22 #b01] ; type
        [21 #b1]
        [19 #b00]
        [16 opcode]
        [15 #b0]
        [14 opsel]
        [10 #b0000]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define f-arith-op
    (lambda (op opcode dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        [29 #b000]
        [24 #b11110]
        [22 #b01] ; type
        [21 #b1]
        [16 (ax-ea-reg-code src1)]
        [12 opcode]
        [10 #b10]
        [5  (ax-ea-reg-code src0)]
        [0  (ax-ea-reg-code dest)])))

  (define fsqrt-op
    (lambda (op dest src code*)
      (emit-code (op dest src code*)
        [29 #b000]
        [24 #b11110]
        [22 #b01] ; type
        [21 #b1]
        [17 #b0000]
        [15 #b11] ; opc
        [10 #b10000]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define fcmp-op
    (lambda (op src0 src1 code*)
      (emit-code (op src0 src1 code*)
        [24 #b00011110]
        [22 #b01]
        [21 #b1]
        [16 (ax-ea-reg-code src1)]
        [10 #b001000]
        [5  (ax-ea-reg-code src0)]
        [3  #b00] ; opc
        [0  #b000])))

  (define rev-op
    (lambda (op opc dest src code*)
      (emit-code (op dest src code*)
        [29 #b110]
        [21 #b11010110]
        [16 #b00000]
        [12 #b0000]
        [10 opc]
        [5  (ax-ea-reg-code src)]
        [0  (ax-ea-reg-code dest)])))

  (define mrs-op
    (lambda (op op0 op1 crn crm op2 dest code*)
      (emit-code (op dest code*)
        [22 #b1101010100]
        [20 #b11]
        [19 op0]
        [16 op1]
        [12 crn]
        [8  crm]
        [5  op2]
        [0 (ax-ea-reg-code dest)])))

  (define und-op
    (lambda (op code*)
      (emit-code (op code*)
        [0 0])))

  ;; asm helpers

  (define-who ax-cond
    (lambda (x)
      (case x
        [(eq) #b0000] ; fl=
        [(ne) #b0001]
        [(cs) #b0010] ; u<
        [(cc) #b0011] ; u>=, fl< (for fl<, do we need this and mi?)
        [(mi) #b0100] ; fl< (for fl<, do we need this and cc?)
        [(pl) #b0101]
        [(vs) #b0110]
        [(vc) #b0111]
        [(hi) #b1000] ; u>
        [(ls) #b1001] ; u<=, fl<=
        [(ge) #b1010] ; fl>=
        [(lt) #b1011]
        [(gt) #b1100] ; fl>
        [(le) #b1101]
        [(al) #b1110]
        [else (sorry! who "unrecognized cond name ~s" x)])))

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
         (let ([safe-check (lambda (e)
                             (if (fx= (debug-level) 0)
                                 e
                                 #`(let ([code #,e])
                                     (unless (<= 0 code (sub1 (expt 2 32)))
                                       (sorry! 'emit-code "bad result ~s for ~s"
                                               code
                                               (list op opnd ...)))
                                     code)))])
           (build-maybe-cons* #`((build long #,(safe-check #`(byte-fields chunk ...))))
             #'(aop-cons* `(asm ,op ,opnd ...) ?code*)))])))

  (define-syntax build
    (syntax-rules ()
      [(_ x e)
       (and (memq (datum x) '(byte word long)) (integer? (datum e)))
       (begin
         (safe-assert (fixnum? (datum e)))
         (quote (x . e)))]
      [(_ x e)
       (memq (datum x) '(byte word long))
       (cons 'x e #;(let ([x e]) (safe-assert (not (eqv? x #x53401c17))) x))]))

  (define-syntax byte-fields
    ; NB: make more efficient for fixnums
    (syntax-rules ()
      [(byte-fields (n e) ...)
       (andmap fixnum? (datum (n ...)))
       (+ (bitwise-arithmetic-shift-left e n) ...)]))

  (define signed9?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 8)) imm (fx- (expt 2 8) 1)))))

  (define unsigned12?
    (lambda (imm)
      (and (fixnum? imm)  ($fxu< imm (expt 2 12)))))

  (define aligned-offset?
    (case-lambda
     [(imm) (aligned-offset? imm (constant log2-ptr-bytes))]
     [(imm log2-bytes)
      (and (fixnum? imm)
           (eqv? 0 (fxand imm (fx- (fxsll 1 log2-bytes) 1)))
           ($fxu< imm (expt 2 (fx+ 12 log2-bytes))))]))

  (define funkymask
    (lambda (imm)
      ;; encode as `(list N immr imms)`, based on the LLVM implementation.
      (cond
        [(eqv? imm 0) #f]  ; can't do all 0s
        [(eqv? imm -1) #f] ; can't do all 1s
        [(>= imm (sub1 (expt 2 63))) #f]  ; can't do all 1s or more
        [(<= imm (- (expt 2 63))) #f] ; can't less than most negative
        [else
         ;; Immediate is representable in 64 bits without being 0 or -1.
         ;; First, find the smallest width that can be replicated to match `imm`:
         (let* ([imm (bitwise-and imm (sub1 (expt 2 64)))] ; view as positive
                [width (let loop ([width 32])
                        (let ([mask (sub1 (bitwise-arithmetic-shift-left 1 width))])
                          (if (= (bitwise-and imm mask)
                                 (bitwise-and (bitwise-arithmetic-shift-right imm width) mask))
                              (if (fx= width 2)
                                  2
                                  (loop (fxsrl width 1)))
                              (fx* width 2))))])
           (let ([v (bitwise-and imm (sub1 (bitwise-arithmetic-shift-left 1 width)))])
             ;; The encoding will work if v matches 1*0*1* or 0*1*0*
             (let* ([count-trailing (lambda (val v)
                                      (let loop ([v v])
                                        (if (= val (bitwise-and v 1))
                                            (fx+ 1 (loop (bitwise-arithmetic-shift-right v 1)))
                                            0)))]
                    [0s (count-trailing 0 v)]
                    [1s (count-trailing 1 (bitwise-arithmetic-shift-right v 0s))]
                    [vx (bitwise-arithmetic-shift-right v (fx+ 0s 1s))])
               (let-values ([(rotate total-1s)
                             (cond
                               [(eqv? 0 vx)
                                (if (fx= 0s 0)
                                    ;; No rotation needed
                                    (values 0 1s)
                                    ;; Rotate left to fill in `0s` zeros, and the encoding works
                                    (values (fx- width 0s) 1s))]
                               [(eqv? 0 0s)
                                ;; There could be more 1s at the top that we can rotate around
                                (let* ([0s (count-trailing 0 vx)])
                                  ;; Assert: 0s < width - 1s
                                  (cond
                                    [(= (bitwise-arithmetic-shift vx 0s)
                                        (sub1 (bitwise-arithmetic-shift-left 1 (fx- width 0s 1s))))
                                     ;; All 1s are in lowest bits or highest bits, so rotate
                                     (values (fx- width 0s 1s)
                                             (fx- width 0s))]
                                    [else (values #f #f)]))]
                               [else (values #f #f)])])
                 (and rotate
                      (list (if (fx= width 64) 1 0)
                            rotate
                            (bitwise-ior (case width
                                           [(2)  #b111100]
                                           [(4)  #b111000]
                                           [(8)  #b110000]
                                           [(16) #b100000]
                                           [else 0])
                                         (fx- total-1s 1))))))))])))

  (define shifted16
    (lambda (imm)
      (let loop ([shift 0])
        (and (fx< shift 4)
             (if (= imm (bitwise-and (bitwise-arithmetic-shift-left #xFFFF (fx* shift 16)) imm))
                 (cons (bitwise-arithmetic-shift-right imm (fx* shift 16)) shift)
                 (loop (fx+ shift 1)))))))

  (define branch-disp?
    (lambda (x)
      (and (fixnum? x)
           (fx<= (- (expt 2 20)) x (- (expt 2 20) 1))
           (not (fxlogtest x #b11)))))

  (define uncond-branch-disp?
    (lambda (x)
      (let ([x (+ x 4)]) ; because `branch-always-label-op` adds 4
        (and (fixnum? x)
             (fx<= (- (expt 2 27)) x (- (expt 2 27) 1))
             (not (fxlogtest x #b11))))))

  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm arm64-abs arm64-jump arm64-call) 0]
        [(long) 4]
        [else 8])))

  (define ax-mov64
    (lambda (dest n code*)
      (emit movzi dest (logand n #xffff) 0
        (emit movki dest (logand (bitwise-arithmetic-shift-right n 16) #xffff) 1
          (emit movki dest (logand (bitwise-arithmetic-shift-right n 32) #xffff) 2
            (emit movki dest (logand (bitwise-arithmetic-shift-right n 48) #xffff) 3
               code*))))))

  (define ax-movi
    (lambda (dest n code*)
      (cond
        [(shifted16 n) =>
         (lambda (imm+shift)
           (emit movzi dest (car imm+shift) (cdr imm+shift) code*))]
        [(funkymask n) =>
         (lambda (n+immr+imms)
           (emit movi dest n n+immr+imms code*))]
        [(unsigned12? n)
         (emit movzi dest 0 0
           (emit addi #f dest dest n code*))]
        [(unsigned12? (- n))
         (emit movzi dest 0 0
           (emit subi #f dest dest (- n) code*))]
        [else
         (let loop ([n n] [shift 0] [init? #t])
           (cond
             [(or (eqv? n 0) (fx= shift 4)) code*]
             [else
              (let ([m (logand n #xFFFF)])
                (cond
                  [(eqv? m 0)
                   (loop (bitwise-arithmetic-shift-right n 16) (fx+ shift 1) init?)]
                  [else
                   (let ([code* (loop (bitwise-arithmetic-shift-right n 16) (fx+ shift 1) #f)])
                     (if init?
                         (emit movzi dest m shift code*)
                         (emit movki dest m shift code*)))]))]))])))

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
             [(reg) ignore (emit mov dest src code*)]
             [(imm) (n)
              (ax-movi dest n code*)]
             [(literal) stuff
              (ax-mov64 dest 0
                (asm-helper-relocation code* (cons 'arm64-abs stuff)))]
             [(disp) (n breg)
              (cond
                [(aligned-offset? n)
                 (emit ldri dest `(reg . ,breg) n code*)]
                [else
                 (assert (signed9? n))
                 (emit lduri dest `(reg . ,breg) n code*)])]
             [(index) (n ireg breg)
              (safe-assert (eqv? n 0))
              (emit ldr dest `(reg . ,breg) `(reg . ,ireg) code*)]
             [else (bad!)])]
          [(ax-reg? src)
           (record-case dest
             [(disp) (n breg)
              (cond
                [(aligned-offset? n)
                 (emit stri src `(reg . ,breg) n code*)]
                [else
                 (assert (signed9? n))
                 (emit sturi src `(reg . ,breg) n code*)])]
             [(index) (n ireg breg)
              (safe-assert (eqv? n 0))
              (emit str src `(reg . ,breg) `(reg . ,ireg) code*)]
             [else (bad!)])]
          [else (bad!)]))))

  (define-who asm-move/extend
    (lambda (op)
      (lambda (code* dest src)
        (Trivit (dest src)
          (case op
            [(sext8) (emit sxtb dest src code*)]
            [(sext16) (emit sxth dest src code*)]
            [(sext32) (emit sxtw dest src code*)]
            [(zext8) (emit uxtb dest src code*)]
            [(zext16) (emit uxth dest src code*)]
            [(zext32) (emit movw dest src code*)] ; movw zero-extends
            [else (sorry! who "unexpected op ~s" op)])))))

  (module (asm-add asm-sub asm-logand asm-logor asm-logxor)
    (define-syntax asm-binop
      (syntax-rules ()
        [(_ opi op)
         (lambda (set-cc?)
           (lambda (code* dest src0 src1)
             (Trivit (dest src0 src1)
               (record-case src1
                 [(imm) (n) (emit opi set-cc? dest src0 n code*)]
                 [else (emit op set-cc? dest src0 src1 code*)]))))]))

    (define asm-add (asm-binop addi add))
    (define asm-sub (asm-binop subi sub))
    (define asm-logand (asm-binop andi and))
    (define asm-logor (asm-binop orri orr))
    (define asm-logxor (asm-binop eori eor)))

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit mul dest src0 src1 code*))))

  (define asm-div
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit sdiv dest src0 src1 code*))))

  (define asm-smulh
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit smulh dest src0 src1 code*))))

  (define-who asm-cmp/asr63
    (lambda (code* src0 src1)
      (Trivit (src0 src1)
        (emit cmp/asr63 src0 src1 code*))))

  (define-who asm-fl-cvt
    (lambda (op)
      (lambda (code* dest src)
        (Trivit (dest src)
          (case op
            [(single->double)
             (emit fcvt.s->d dest src code*)]
            [(double->single)
             (emit fcvt.d->s dest src code*)]
            [else (sorry! who "unrecognized op ~s" op)])))))

  (define-who asm-load
    (lambda (type)
      (rec asm-load-internal
        (lambda (code* dest base index offset)
          (let ([n (nanopass-case (L16 Triv) offset
                     [(immediate ,imm) imm]
                     [else (sorry! who "unexpected non-immediate offset ~s" offset)])])
            ;; Assuming that `n` is either aligned and in range (fits
            ;; unsigned in 12 bits after shifting by type bits) or unaligned
            ;; and small (fits in 9 bits)
            (Trivit (dest base)
              (cond
                [(eq? index %zero)
                 (cond
                   [(signed9? n)
                    (case type
                      [(integer-64 unsigned-64) (emit lduri dest base n code*)]
                      [(integer-32) (emit ldurswi dest base n code*)]
                      [(unsigned-32) (emit ldurwi dest base n code*)]
                      [(integer-16) (emit ldurshi dest base n code*)]
                      [(unsigned-16) (emit ldurhi dest base n code*)]
                      [(integer-8) (emit ldursbi dest base n code*)]
                      [(unsigned-8) (emit ldurbi dest base n code*)]
                      [else (sorry! who "unexpected mref type ~s" type)])]
                   [else
                    (case type
                      [(integer-64 unsigned-64) (emit ldri dest base n code*)]
                      [(integer-32) (emit ldrswi dest base n code*)]
                      [(unsigned-32) (emit ldrwi dest base n code*)]
                      [(integer-16) (emit ldrshi dest base n code*)]
                      [(unsigned-16) (emit ldrhi dest base n code*)]
                      [(integer-8) (emit ldrsbi dest base n code*)]
                      [(unsigned-8) (emit ldrbi dest base n code*)]
                      [else (sorry! who "unexpected mref type ~s" type)])])]
                [(eqv? n 0)
                  (Trivit (index)
                    (case type
                      [(integer-64 unsigned-64) (emit ldr dest base index code*)]
                      [(integer-32) (emit ldrsw dest base index code*)]
                      [(unsigned-32) (emit ldrw dest base index code*)]
                      [(integer-16) (emit ldrsh dest base index code*)]
                      [(unsigned-16) (emit ldrh dest base index code*)]
                      [(integer-8) (emit ldrsb dest base index code*)]
                      [(unsigned-8) (emit ldrb dest base index code*)]
                      [else (sorry! who "unexpected mref type ~s" type)]))]
                [else (sorry! who "expected %zero index or 0 offset, got ~s and ~s" index offset)])))))))

  (define-who asm-store
    (lambda (type)
      (rec asm-store-internal
        (lambda (code* base index offset src)
          (let ([n (nanopass-case (L16 Triv) offset
                     [(immediate ,imm) imm]
                     [else (sorry! who "unexpected non-immediate offset ~s" offset)])])
            ;; Assuming that `n` is aligned and in range (fits
            ;; unsigned in 12 bits after shifting by type bits)
            (Trivit (src base)
              (cond
                [(eq? index %zero)
                 (cond
                   [(signed9? n)
                    (case type
                      [(integer-64 unsigned-64) (emit sturi src base n code*)]
                      [(integer-32 unsigned-32) (emit sturwi src base n code*)]
                      [(integer-16 unsigned-16) (emit sturhi src base n code*)]
                      [(integer-8 unsigned-8) (emit sturbi src base n code*)]
                      [else (sorry! who "unexpected mref type ~s" type)])]
                   [else
                    (case type
                      [(integer-64 unsigned-64) (emit stri src base n code*)]
                      [(integer-32 unsigned-32) (emit strwi src base n code*)]
                      [(integer-16 unsigned-16) (emit strhi src base n code*)]
                      [(integer-8 unsigned-8) (emit strbi src base n code*)]
                      [else (sorry! who "unexpected mref type ~s" type)])])]
                [(eqv? n 0)
                  (Trivit (index)
                    (case type
                      [(integer-64 unsigned-64) (emit str src base index code*)]
                      [(integer-32 unsigned-32) (emit strw src base index code*)]
                      [(integer-16 unsigned-16) (emit strh src base index code*)]
                      [(integer-8 unsigned-8) (emit strb src base index code*)]
                      [else (sorry! who "unexpected mref type ~s" type)]))]
                [else (sorry! who "expected %zero index or 0 offset, got ~s and ~s" index offset)])))))))

  (define-who asm-fpop-2
    (lambda (op)
      (lambda (code* dest src1 src2)
        (Trivit (dest src1 src2)
          (case op
            [(fp+) (emit fadd dest src1 src2 code*)]
            [(fp-) (emit fsub dest src1 src2 code*)]
            [(fp*) (emit fmul dest src1 src2 code*)]
            [(fp/) (emit fdiv dest src1 src2 code*)]
            [else (sorry! who "unrecognized op ~s" op)])))))

  (define asm-fpsqrt
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fsqrt dest src code*))))

  (define-who asm-fpsingle
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fcvt.d->s dest src
          (emit fcvt.s->d dest dest
            code*)))))

  (define asm-fptrunc
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fcvtzs dest src code*))))

  (define asm-fpt
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit scvtf dest src code*))))

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
               (cond
                 [(aligned-offset? imm)
                  (emit strfi src (cons 'reg reg) imm code*)]
                 [else
                  (safe-assert (signed9? imm))
                  (emit sturfi src (cons 'reg reg) imm code*)])
               (cond
                 [(aligned-offset? imm 2)
                  (emit strfsi src (cons 'reg reg) imm code*)]
                 [else
                  (safe-assert (signed9? imm))
                  (emit sturfsi src (cons 'reg reg) imm code*)]))]
          [(index) (n ireg breg)
           (cond
             [(fx= n 0)
              (if double?
                  (emit strf src (cons 'reg ireg) (cons 'reg breg) code*)
                  (emit strfs src (cons 'reg ireg) (cons 'reg breg) code*))]
             [else
              (sorry! who "cannot handle indexed fp dest ref")])]
          [else
           (record-case src
             [(disp) (imm reg)
              (if double?
                  (cond
                    [(aligned-offset? imm)
                     (emit ldrfi dest (cons 'reg reg) imm code*)]
                    [else
                     (safe-assert (signed9? imm))
                     (emit ldurfi dest (cons 'reg reg) imm code*)])
                  (cond
                    [(aligned-offset? imm 2)
                     (emit ldrfsi dest (cons 'reg reg) imm code*)]
                    [else
                     (safe-assert (signed9? imm))
                     (emit ldurfsi dest (cons 'reg reg) imm code*)]))]
             [(index) (n ireg breg)
              (cond
                [(fx= n 0)
                 (if double?
                     (emit ldrf dest (cons 'reg ireg) (cons 'reg breg) code*)
                     (emit ldrfs dest (cons 'reg ireg) (cons 'reg breg) code*))]
                [else
                 (sorry! who "cannot handle indexed fp src ref")])]
             [else (emit fmov dest src code*)])]))))

  (define asm-fpcastto
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fmov.f->g dest src code*))))

  (define asm-fpcastfrom
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fmov.g->f dest src code*))))

  (define-who asm-swap
    (lambda (type)
      (rec asm-swap-internal
        (lambda (code* dest src)
          (Trivit (dest src)
            (case type
              [(integer-16) (emit rev16 dest src
                              (emit sxth dest dest code*))]
              [(unsigned-16) (emit rev16 dest src
                               (emit uxth dest dest code*))]
              [(integer-32) (emit rev32 dest src
                              (emit sxtw dest dest code*))]
              [(unsigned-32) (emit rev32 dest src
                               (emit movw dest dest code*))]
              [(integer-64 unsigned-64) (emit rev dest src code*)]
              [else (sorry! who "unexpected asm-swap type argument ~s" type)]))))))

  (define asm-lock
    ;  tmp = 1 # in case load result is not 0
    ;  tmp2 = ldxr src
    ;  cmp tmp2, 0
    ;  bne L1
    ;  tmp2 = 1
    ;  tmp = stxr tmp2, src
    ;L1:
    (lambda (code* src tmp tmp2)
      (Trivit (src tmp tmp2)
        (emit movzi tmp 1 0
          (emit ldxr tmp2 src
            (emit cmpi tmp2 0
              (emit bnei 12
                (emit movzi tmp2 1 0
                  (emit stxr tmp tmp2 src code*)))))))))

  (define-who asm-lock+/-
    ; L:
    ;   tmp1 = ldxr src
    ;   tmp1 = tmp1 +/- 1
    ;   tmp2 = stxr tmp1, src
    ;   cmp tmp2, 0
    ;   bne L
    ;   cmp tmp1, 0
    (lambda (op)
      (lambda (code* src tmp1 tmp2)
        (Trivit (src tmp1 tmp2)
          (emit ldxr tmp1 src
            (let ([code* (emit stxr tmp2 tmp1 src
                           (emit cmpi tmp2 0
                             (emit bnei -16
                               (emit cmpi tmp1 0 code*))))])
              (case op
                [(locked-incr!) (emit addi #f tmp1 tmp1 1 code*)]
                [(locked-decr!) (emit subi #f tmp1 tmp1 1 code*)]
                [else (sorry! who "unexpected op ~s" op)])))))))

  (define-who asm-cas
    ;   tmp = ldxr src
    ;   cmp tmp, old
    ;   bne L
    ;   tmp2 = stxr new, src
    ;   cmp tmp2, 0
    ; L:
    (lambda (code* src old new tmp1 tmp2)
      (Trivit (src old new tmp1 tmp2)
        (emit ldxr tmp1 src
          (emit cmp tmp1 old
            (emit bnei 12
              (emit stxr tmp2 new src
                (emit cmpi tmp2 0
                   code*))))))))

  ;; Based in part on https://www.cl.cam.ac.uk/~pes20/cpp/cpp0xmappings.html
  (define-who asm-fence
    (lambda (kind)
      (lambda (code*)
        (case kind
          [(store-store) (emit dmbishst code*)]
          [(acquire) (emit dmbishld code*)]
          [(release) (emit dmbish code*)]
          [else (sorry! who "unexpected kind ~s" kind)]))))

  (define asm-fp-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
          (values
           (emit fcmp x y '())
           (asm-conditional-jump info l1 l2 offset))))))

  (define-who asm-relop
    (lambda (info negated-imm?)
      (rec asm-relop-internal
        (lambda (l1 l2 offset x y)
          (Trivit (x y)
            (unless (ax-reg? x) (sorry! who "unexpected first operand ~s" x))
            (values
              (record-case y
                [(imm) (n) (if negated-imm?
                               (emit cmni x n '())
                               (emit cmpi x n '()))]
                [(reg) ignore (safe-assert (not negated-imm?)) (emit cmp x y '())]
                [else (sorry! who "unexpected second operand ~s" y)])
              (asm-conditional-jump info l1 l2 offset)))))))

  (define asm-condition-code
    (lambda (info)
      (rec asm-check-flag-internal
        (lambda (l1 l2 offset)
          (values '() (asm-conditional-jump info l1 l2 offset))))))

  (define asm-pop-multiple
    (lambda (regs)
      (lambda (code*)
        (asm-multiple regs #t code*
                      (lambda (sp reg code*)
                        (emit ldr/postidx reg sp 16 code*))
                      (lambda (sp reg1 reg2 code*)
                        (emit ldrp/postidx reg1 reg2 sp 16 code*))))))

  (define asm-push-multiple
    (lambda (regs)
      (lambda (code*)
        (asm-multiple regs #f code*
                      (lambda (sp reg code*)
                        (emit str/preidx reg sp -16 code*))
                      (lambda (sp reg1 reg2 code*)
                        (emit strp/preidx reg1 reg2 sp -16 code*))))))

  (define asm-pop-fpmultiple
    (lambda (regs)
      (lambda (code*)
        (asm-multiple regs #t code*
                      (lambda (sp reg code*)
                        (emit ldrf/postidx reg sp 16 code*))
                      (lambda (sp reg1 reg2 code*)
                        (emit ldrpf/postidx reg1 reg2 sp 16 code*))))))

  (define asm-push-fpmultiple
    (lambda (regs)
      (lambda (code*)
        (asm-multiple regs #f code*
                      (lambda (sp reg code*)
                        (emit strf/preidx reg sp -16 code*))
                      (lambda (sp reg1 reg2 code*)
                        (emit strpf/preidx reg1 reg2 sp -16 code*))))))

  (define (asm-multiple regs rev? code* one two)
    (let ([sp `(reg . ,%sp)])
      (let loop ([regs regs] [code* code*])
        (cond
          [(null? regs) code*]
          [(null? (cdr regs))
           (one sp (cons 'reg (car regs)) code*)]
          [rev?
           (two sp (cons 'reg (car regs)) (cons 'reg (cadr regs)) (loop (cddr regs) code*))]
          [else
           (loop (cddr regs) (two sp (cons 'reg (car regs)) (cons 'reg (cadr regs)) code*))]))))

  (define asm-debug
    (lambda (code*)
      (emit und code*)))

  (define asm-read-counter
    (lambda (op0 op1 crn crm op2)
      (lambda (code* dest)
        (Trivit (dest)
          (emit mrs op0 op1 crn crm op2 dest code*)))))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
        `(arm64-jump ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec save-ra?)
      (let ([target `(arm64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* dest . ignore) ; ignore arguments, which must be in fixed locations
            (asm-helper-call code* target save-ra?))))))

  (define asm-library-call!
    (lambda (libspec save-ra?)
      (let ([target `(arm64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* . ignore) ; ignore arguments, which must be in fixed locations
            (asm-helper-call code* target save-ra?))))))

  (define asm-c-simple-call
    (lambda (entry save-ra?)
      (let ([target `(arm64-call 0 (entry ,entry))])
        (rec asm-c-simple-call-internal
          (lambda (code* . ignore)
            (asm-helper-call code* target save-ra?))))))

  (define-who asm-indirect-call
    (lambda (code* dest lr . ignore)
      (safe-assert (eq? lr %lr))
      (Trivit (dest)
        (unless (ax-reg? dest) (sorry! who "unexpected dest ~s" dest))
        (emit blr dest code*))))

  (define asm-direct-jump
    (lambda (l offset)
      (let ([offset (adjust-return-point-offset offset l)])
        (asm-helper-jump '() (make-funcrel 'arm64-jump l offset)))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
        `(arm64-jump ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info))))))

  (define-who asm-indirect-jump
    (lambda (src)
      (Trivit (src)
        (record-case src
          [(reg) ignore (emit br src '())]
          [(disp) (n breg)
           (cond
             [(signed9? n)
              (emit lduri `(reg . ,%jmptmp) `(reg . ,breg) n
                 (emit br `(reg . ,%jmptmp) '()))]
             [(aligned-offset? n)
              (emit ldri `(reg . ,%jmptmp) `(reg . ,breg) n
                 (emit br `(reg . ,%jmptmp) '()))]
             [else
              (safe-assert (or (unsigned12? n) (unsigned12? (- n))))
              (let ([code* (emit ldri `(reg . ,%jmptmp) `(reg . ,%jmptmp) 0
                             (emit br `(reg . ,%jmptmp) '()))])
                (if (unsigned12? n)
                    (emit addi #f `(reg . ,%jmptmp) `(reg . ,breg) n code*)
                    (emit subi #f `(reg . ,%jmptmp) `(reg . ,breg) (- n) code*)))])]
          [(index) (n ireg breg)
           (safe-assert (eqv? n 0))
           (emit ldr `(reg . ,%jmptmp) `(reg . ,breg) `(reg . ,ireg)
             (emit br `(reg . ,%jmptmp) '()))]
          [else (sorry! who "unexpected src ~s" src)]))))

  (define asm-logtest
    (lambda (i? info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
          (values
            (record-case y
              [(imm) (n) (emit tsti x n '())]
              [else (emit tst x y '())])
            (let-values ([(l1 l2) (if i? (values l2 l1) (values l1 l2))])
              (asm-conditional-jump info l2 l1 offset)))))))

  (define asm-get-tc
    (let ([target `(arm64-call 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* dest . ignore) ; dest is ignored, since it is always Cretval
        (asm-helper-call code* target #f))))

  (define asm-activate-thread
    (let ([target `(arm64-call 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* dest . ignore)
        (asm-helper-call code* target #t))))

  (define asm-deactivate-thread
    (let ([target `(arm64-call 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code* . ignore)
        (asm-helper-call code* target #f))))

  (define asm-unactivate-thread
    (let ([target `(arm64-call 0 (entry ,(lookup-c-entry unactivate-thread)))])
      (lambda (code* arg-reg . ignore)
        (asm-helper-call code* target #f))))

  (define-who asm-return-address
    (lambda (dest l incr-offset next-addr)
      (make-rachunk dest l incr-offset next-addr
        (or (cond
              [(local-label-offset l) =>
               (lambda (offset)
                 (let ([incr-offset (adjust-return-point-offset incr-offset l)])
                   (let ([disp (fx+ (fx- next-addr (fx- offset incr-offset)) 4)])
                     (cond
                       [($fxu< disp (expt 2 21))
                        (Trivit (dest)
                          (emit adr dest disp '()))]
                      [else #f]))))]
              [else #f])
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
                 [(uncond-branch-disp? disp) (emit b `(label ,disp ,l) '())]
                 [else (sorry! who "no support for code objects > 256MB in length")])))]
          [else
            ;; label must be somewhere above.  generate something so that a hard loop
            ;; doesn't get dropped.  this also has some chance of being the right size
            ;; for the final branch instruction.
            (emit b `(label 0 ,l) '())]))))

  (define-who asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      (define get-disp-opnd
        (lambda (next-addr l)
          (if (local-label? l)
              (cond
                [(local-label-offset l) =>
                 (lambda (offset)
                   (let ([disp (fx- next-addr offset)])
                     (values disp `(label ,disp ,l))))]
                [else (values 0 `(label 0 ,l))])
              (sorry! who "unexpected label ~s" l))))
      (let ([type (info-condition-code-type info)]
            [reversed? (info-condition-code-reversed? info)])
        (make-cgchunk info l1 l2 next-addr
          (let ()
            (define-syntax pred-case
              (lambda (x)
                (define b-asm-size 4)
                (define build-bop-seq
                  (lambda (bop opnd1 opnd2 l2 body)
                    #`(let ([code* (emit #,bop #,opnd1 code*)])
                        (safe-assert (= (asm-size* code*) #,b-asm-size))
                        (let-values ([(ignore #,opnd2) (get-disp-opnd (fx+ next-addr #,b-asm-size) #,l2)])
                          #,body))))
                (define ops->code
                  (lambda (bop opnd)
                    #`(emit #,bop #,opnd code*)))
                (define handle-reverse
                  (lambda (e opnd l)
                    (syntax-case e (r?)
                      [(r? c1 c2) #`(if reversed? #,(ops->code #'c1 opnd) #,(ops->code #'c2 opnd))]
                      [_ (ops->code e opnd)])))
                (define handle-inverse
                  (lambda (e)
                    (syntax-case e (i?)
                      [(i? c1 c2)
                       #`(cond
                           [(and (fx= disp1 0)
                                 (branch-disp? (fx+ disp2 #,b-asm-size)))
                            #,(handle-reverse #'c1 #'opnd2 #'l2)]
                           [(and (fx= disp2 0)
                                 (branch-disp? (fx+ disp1 #,b-asm-size)))
                            #,(handle-reverse #'c2 #'opnd1 #'l1)]
                           [(branch-disp? (fx+ disp1 (fx* 2 #,b-asm-size)))
                            #,(build-bop-seq #'b #'opnd2 #'opnd1 #'l1
                                (handle-reverse #'c2 #'opnd1 #'l1))]
                           [(branch-disp? (fx+ disp2 (fx* 2 #,b-asm-size)))
                            #,(build-bop-seq #'b #'opnd1 #'opnd2 #'l2
                                (handle-reverse #'c1 #'opnd2 #'l2))]
                           [else
                            (let ([code* #,(build-bop-seq #'b #'opnd1 #'opnd2 #'l2
                                             #'(emit b opnd2 code*))])
                              #,(handle-reverse #'c2 #``(imm #,b-asm-size) #'step))])]
                      [_ ($oops 'handle-inverse "expected an inverse in ~s" e)])))
                (syntax-case x ()
                  [(_ [(pred ...) cl-body] ...)
                   (with-syntax ([(cl-body ...) (map handle-inverse #'(cl-body ...))])
                     #'(let ([code* '()])
                         (let-values ([(disp1 opnd1) (get-disp-opnd next-addr l1)]
                                      [(disp2 opnd2) (get-disp-opnd next-addr l2)])
                           (case type
                             [(pred ...) cl-body] ...
                             [else (sorry! who "~s branch type is currently unsupported" type)]))))])))
            (pred-case
              [(eq?) (i? bne beq)]
              [(u<) (i? (r? bls bcs) (r? bhi bcc))]
              [(<) (i? (r? ble bge) (r? bgt blt))]
              [(<=) (i? (r? blt bgt) (r? bge ble))]
              [(>) (i? (r? bge ble) (r? blt bgt))]
              [(>=) (i? (r? bgt blt) (r? ble bge))]
              [(overflow) (i? bvc bvs)]
              [(positive) (i? ble bgt)]
              [(multiply-overflow) (i? beq bne)] ; result of comparing sign bit of low word with all bits in high word: eq if no overflow, ne if oveflow
              [(carry) (i? bcc bcs)]
              [(fp<) (i? (r? ble bcs) (r? bgt bcc))]
              [(fp<=) (i? (r? blt bhi) (r? bge bls))]
              [(fp=) (i? bne beq)]))))))

  (define asm-helper-jump
    (lambda (code* reloc)
      (let ([jmp-tmp (cons 'reg %jmptmp)])
        (ax-mov64 jmp-tmp 0
          (emit br jmp-tmp
            (asm-helper-relocation code* reloc))))))

  (define asm-kill
    (lambda (code* dest)
      code*))

  (define ax-save/restore
    ;; push/pop while maintaining 16-byte alignment
    (lambda (code* reg-ea p)
      (let ([sp (cons 'reg %sp)])
        (emit str/preidx reg-ea sp -16
          (p (emit ldr/postidx reg-ea sp 16 code*))))))

  (define asm-helper-call
    (lambda (code* reloc save-ra?)
      ;; NB: kills %lr
      (let ([jmp-tmp (cons 'reg %jmptmp)])
        (define maybe-save-ra
          (lambda (code* p)
            (if save-ra?
                (ax-save/restore code* (cons 'reg %lr) p)
                (p code*))))
        (maybe-save-ra code*
          (lambda (code*)
            (ax-mov64 jmp-tmp 0
              (emit blr jmp-tmp
                (asm-helper-relocation code* reloc))))))))

  (define asm-helper-relocation
    (lambda (code* reloc)
      (cons* reloc (aop-cons* `(asm "relocation:" ,reloc) code*))))

  ; NB: reads from %lr...should be okay if declare-intrinsics sets up return-live* properly
  (define asm-return (lambda () (emit ret (cons 'reg %lr) '())))

  (define asm-c-return (lambda (info) (emit ret (cons 'reg %lr) '())))

  (define-who asm-shiftop
    (lambda (op)
      (lambda (code* dest src0 src1)
        (Trivit (dest src0 src1)
          (record-case src1
            [(imm) (n)
             ;; When `n` fits in a fixnum, the compiler may generate
             ;; a bad shift that is under a guard, so force it to 63 bits
             (let ([n (fxand n 63)])
               (cond
                 [(fx= n 0)
                  ;; shift by 0 is just a move
                  (emit mov dest src0 code*)]
                 [else
                  (case op
                    [(sll) (emit lsli dest src0 n code*)]
                    [(srl) (emit lsri dest src0 n code*)]
                    [(sra) (emit asri dest src0 n code*)]
                    [else (sorry! 'shiftop "unrecognized ~s" op)])]))]
            [else
             (case op
               [(sll) (emit lsl dest src0 src1 code*)]
               [(srl) (emit lsr dest src0 src1 code*)]
               [(sra) (emit asr dest src0 src1 code*)]
               [else (sorry! 'shiftop "unrecognized ~s" op)])])))))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit mvn dest src code*))))

  (define asm-popcount
    (lambda (code* dest src tmp)
      (Trivit (dest src tmp)
        (emit fmov.g->f tmp src
          (emit cnt tmp tmp
            (emit addv.b tmp tmp
              (emit fmov.f->g dest tmp code*)))))))

  (define asm-enter values)

  (define-who asm-inc-cc-counter
    (lambda (code* addr val tmp)
      (Trivit (addr val tmp)
        (define do-ldr
          (lambda (offset k code*)
            (emit ldri tmp addr offset (k (emit stri tmp addr offset code*)))))
        (define do-add/cc
          (lambda (code*)
            (record-case val
              [(imm) (n) (emit addi #t tmp tmp n code*)]
              [else (emit add #t tmp tmp val code*)])))
        (do-ldr 0
          do-add/cc
          (emit bnei 16
            (do-ldr 8
              (lambda (code*)
                (emit addi #f tmp tmp 1 code*))
              code*))))))

  (module (asm-foreign-call asm-foreign-callable)
    (define align (lambda (b x) (let ([k (- b 1)]) (fxlogand (fx+ x k) (fxlognot k)))))
    (define (double-member? m) (and (eq? (car m) 'float)
                                    (fx= (cadr m) 8)))
    (define (float-member? m) (and (eq? (car m) 'float)
                                   (fx= (cadr m) 4)))
    (define (indirect-result-that-fits-in-registers? result-type)
      (nanopass-case (Ltype Type) result-type
        [(fp-ftd& ,ftd)
         (let* ([members ($ftd->members ftd)]
                [num-members (length members)])
           (or (fx<= ($ftd-size ftd) 4)
               (and (fx= num-members 1)
                    ;; a struct containing only int64 is not returned in a register
                    (or (not ($ftd-compound? ftd))))
               (and (fx<= num-members 4)
                    (or (andmap double-member? members)
                        (andmap float-member? members)))))]
        [else #f]))
    (define int-argument-regs (list %Carg1 %Carg2 %Carg3 %Carg4
                                    %Carg5 %Carg6 %Carg7 %Carg8))
    (define fp-argument-regs (list %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4
                                   %Cfparg5 %Cfparg6 %Cfparg7 %Cfparg8))
    (define save-and-restore
      (lambda (regs e)
        (safe-assert (andmap reg? regs))
        (with-output-language (L13 Effect)
          (let ([save-and-restore-gp
                 (lambda (regs e)
                   (let* ([regs (filter (lambda (r) (not (eq? (reg-type r) 'fp))) regs)])
                     (cond
                      [(null? regs) e]
                      [else
                       (%seq
                        (inline ,(make-info-kill*-live* '() regs) ,%push-multiple)
                        ,e
                        (inline ,(make-info-kill*-live* regs '()) ,%pop-multiple))])))]
                [save-and-restore-fp
                 (lambda (regs e)
                   (let ([fp-regs (filter (lambda (r) (eq? (reg-type r) 'fp)) regs)])
                     (cond
                      [(null? fp-regs) e]
                      [else
                       (%seq
                        (inline ,(make-info-kill*-live* '() fp-regs) ,%push-fpmultiple)
                        ,e
                        (inline ,(make-info-kill*-live* fp-regs '()) ,%pop-fpmultiple))])))])
            (save-and-restore-gp regs (save-and-restore-fp regs e))))))

    (define-record-type cat
      (nongenerative #{cat jqrttgvpydsbdo0l736l43udu-1})
      (sealed #t)
      (fields place            ; 'int, 'fp, or 'stack
              regs             ; list of registers
              size             ; size in bytes
              pad              ; extra trailing size (for 'stack place) in bytes
              indirect-bytes)) ; #f or extra bytes on stack for indirect

    (define alignment-via-lookahead
      (lambda (size types int* fp* fp-in-int? stack-align varargs-after k)
        (constant-case machine-type-name
          [(arm64osx tarm64osx arm64ios tarm64ios)
           (cond
             [(eqv? 0 varargs-after) (k (align 8 size) 0 0)]
             [else
              ;; On Mac OS, a non-varargs stack argument does not have to use a
              ;; multiple of 8, but we need to work out any padding that
              ;; is needed to get alignment right for the next argument
              ;; (and to end on 8-byte alignment). Currently, we're
              ;; assuming max alignment of 8.
              (let ()
                (define (next-is-stack types)
                  (let ([end-this-align (fxand #x7 (fx+ stack-align size))]
                        [next-align (cond
                                      [(null? types) 8]
                                      [else (nanopass-case (Ltype Type) (car types)
                                              [(fp-double-float) 8]
                                              [(fp-single-float) 4 #;(if (and (eq? kind 'float)
                                               (= stack-align (align 8 stack-align)))
                                               4
                                               8)]
                                              [(fp-ftd& ,ftd) (cond
                                                                [(> ($ftd-size ftd) 16) 8]
                                                                [else ($ftd-alignment ftd)])]
                                              [(fp-integer ,bits) (fxquotient bits 8)]
                                              [(fp-unsigned ,bits) (fxquotient bits 8)]
                                              [else 8])])])
                    (cond
                      [(fx= 0 (fxand end-this-align (fx- next-align 1)))
                       (k size 0 end-this-align)]
                      [else
                       (k size (- next-align end-this-align) next-align)])))
                ;; Look past arguments that fit in registers
                (let loop ([types types] [int* int*] [fp* fp*])
                  (cond
                    [(null? types) (next-is-stack types)]
                    [else
                     (let ()
                       (define (use-int-regs n)
                         (let use ([n n] [int* int*])
                           (cond
                             [(null? int*) (next-is-stack types)]
                             [(fx> n 1) (use (fx- n 1) (cdr int*))]
                             [else (loop (cdr types) (cdr int*) fp*)])))
                       (define (use-fp-regs n)
                         (cond
                           [fp-in-int? (use-int-regs n)]
                           [else
                            (let use ([n n] [fp* fp*])
                              (cond
                                [(null? fp*) (next-is-stack types)]
                                [(fx> n 1) (use (fx- n 1) (cdr fp*))]
                                [else (loop (cdr types) int* (cdr fp*))]))]))
                       (nanopass-case (Ltype Type) (car types)
                         [(fp-double-float) (use-fp-regs 1)]
                         [(fp-single-float) (use-fp-regs 1)]
                         [(fp-ftd& ,ftd) (cond
                                           [(> ($ftd-size ftd) 16) (next-is-stack types)]
                                           [else
                                            (let ([members ($ftd->members ftd)])
                                              (cond
                                                [(and (fx= 8 ($ftd-alignment ftd))
                                                      (andmap double-member? members))
                                                 (use-fp-regs (length members))]
                                                [(and (fx= 4 ($ftd-alignment ftd))
                                                      (andmap float-member? members))
                                                 (use-fp-regs (length members))]
                                                [else
                                                 (use-int-regs (fxquotient (align ($ftd-size ftd) 8) 8))]))])]
                         [else (use-int-regs 1)]))])))])]
          [else
           (k (align 8 size) 0 0)])))

    (define rest-of
      (lambda (regs n next-varargs-after)
        (constant-case machine-type-name
          [(arm64osx tarm64osx arm64ios tarm64ios)
           (cond
             [(eqv? next-varargs-after 0)
              ;; All the rest go on the stack
              '()]
             [else
              (list-tail regs n)])]
          [else
           (list-tail regs n)])))

    ;; remove members of an `&`-wrapped ftd that have the
    ;; same the same type and offset, since the AArch64 ABI
    ;; is in terns of individually accessible elements
    (define filter-union
      (lambda (members)
        (let loop ([members members])
          (cond
            [(null? members) '()]
            [(member (car members) (cdr members)) (loop (cdr members))]
            [else (cons (car members) (loop (cdr members)))]))))

    (define categorize-arguments
      (lambda (types varargs-after)
        (let loop ([types types] [int* int-argument-regs] [fp* fp-argument-regs]
                   [varargs-after varargs-after]
                   ;; accumulate alignment from previous args so we can compute any
                   ;; needed padding and alignment after this next argument
                   [stack-align 0])
          (let ([next-varargs-after (and varargs-after (if (fx> varargs-after 0) (fx- varargs-after 1) 0))]
                [fp-in-int? (constant-case machine-type-name
                              [(arm64nt tarm64nt) (and varargs-after #t)]
                              [else #f])])
            (if (null? types)
                '()
                (nanopass-case (Ltype Type) (car types)
                  [(fp-double-float)
                   (cond
                     [(if fp-in-int? (null? int*) (null? fp*))
                      (cons (make-cat 'stack '() 8 0 #f) (loop (cdr types) int* '() next-varargs-after 0))]
                     [fp-in-int?
                      (cons (make-cat 'int (list (car int*)) 8 0 #f)
                            (loop (cdr types) (rest-of int* 1 next-varargs-after) (rest-of fp* 0 next-varargs-after)
                                  next-varargs-after
                                  stack-align))]
                     [else
                      (cons (make-cat 'fp (list (car fp*)) 8 0 #f)
                            (loop (cdr types) (rest-of int* 0 next-varargs-after) (rest-of fp* 1 next-varargs-after)
                                  next-varargs-after
                                  stack-align))])]
                  [(fp-single-float)
                   (cond
                     [(if fp-in-int? (null? int*) (null? fp*))
                      (alignment-via-lookahead
                       4 (cdr types) int* fp* fp-in-int? stack-align varargs-after
                       (lambda (bytes pad stack-align)
                         (cons (make-cat 'stack '() bytes pad #f) (loop (cdr types) int* '() next-varargs-after stack-align))))]
                     [fp-in-int?
                      (cons (make-cat 'int (list (car int*)) 8 0 #f)
                            (loop (cdr types) (rest-of int* 1 next-varargs-after) (rest-of fp* 0 next-varargs-after)
                                  next-varargs-after
                                  stack-align))]
                     [else
                      (cons (make-cat 'fp (list (car fp*)) 8 0 #f)
                            (loop (cdr types) (rest-of int* 0 next-varargs-after)(rest-of fp* 1 next-varargs-after)
                                  next-varargs-after
                                  stack-align))])]
                  [(fp-ftd& ,ftd)
                   (let* ([size ($ftd-size ftd)]
                          [members (filter-union ($ftd->members ftd))]
                          [num-members (length members)]
                          [doubles? (and (fx= 8 ($ftd-alignment ftd))
                                         (fx<= num-members 4)
                                         (andmap double-member? members))]
                          [floats? (and (fx= 4 ($ftd-alignment ftd))
                                        (fx<= num-members 4)
                                        (andmap float-member? members))])
                     (cond
                       [(and doubles? (not fp-in-int?))
                        ;; Sequence of up to 4 doubles that may fit in registers
                        (cond
                          [(fx>= (length fp*) num-members)
                           ;; Allocate each double to a register
                           (cons (make-cat 'fp (list-head fp* num-members) (fx* 8 num-members) 0 #f)
                                 (loop (cdr types) (rest-of int* 0 next-varargs-after) (rest-of fp* num-members next-varargs-after)
                                       next-varargs-after
                                       stack-align))]
                          [else
                           ;; Stop using fp registers, put on stack
                           (cons (make-cat 'stack '() size 0 #f)
                                 (loop (cdr types) int* '() next-varargs-after 0))])]
                       [(and floats? (not fp-in-int?))
                        ;; Sequence of up to 4 floats that may fit in registers
                        (cond
                          [(fx>= (length fp*) num-members)
                           ;; Allocate each float to a register
                           (cons (make-cat 'fp (list-head fp* num-members) (fx* 8 num-members) 0 #f)
                                 (loop (cdr types) (rest-of int* 0 next-varargs-after) (rest-of fp* num-members next-varargs-after)
                                       next-varargs-after
                                       stack-align))]
                          [else
                           ;; Stop using fp registers, put on stack
                           (alignment-via-lookahead
                            size (cdr types) int* fp* fp-in-int? stack-align varargs-after
                            (lambda (size pad stack-align)
                              (cons (make-cat 'stack '() size pad #f)
                                    (loop (cdr types) int* '() next-varargs-after stack-align))))])]
                       [(fx> size 16)
                        ;; Indirect; pointer goes in a register or on the stack
                        (cond
                          [(null? int*)
                           ;; Pointer on the stack
                           (cons (make-cat 'stack '() (constant ptr-bytes) 0 (align 8 size))
                                 (loop (cdr types) '() fp* next-varargs-after 0))]
                          [else
                           ;; Pointer in register
                           (cons (make-cat 'int (list (car int*)) 8 0 (align 8 size))
                                 (loop (cdr types) (rest-of int* 1 next-varargs-after) (rest-of fp* 0 next-varargs-after)
                                       next-varargs-after
                                       stack-align))])]
                       [else
                        ;; Maybe put in integer registers
                        (let* ([regs (fxquotient (align 8 size) 8)])
                          (cond
                            [(fx<= regs (length int*))
                             ;; Fits in registers
                             (cons (make-cat 'int (list-head int* regs) (align 8 size) 0 #f)
                                   (loop (cdr types) (rest-of int* regs next-varargs-after) (rest-of fp* 0 next-varargs-after)
                                         next-varargs-after
                                         stack-align))]
                            [else
                             ;; Stop using int registers, put on stack
                             (alignment-via-lookahead
                              size (cdr types) int* fp* fp-in-int? stack-align varargs-after
                              (lambda (size pad stack-align)
                                (cons (make-cat 'stack '() size pad #f)
                                      (loop (cdr types) '() fp* next-varargs-after stack-align))))]))]))]
                  [else
                   ;; integers, scheme-object, etc.
                   (cond
                     [(null? int*)
                      (let ([size (nanopass-case (Ltype Type) (car types)
                                    [(fp-integer ,bits) (fxquotient bits 8)]
                                    [(fp-unsigned ,bits) (fxquotient bits 8)]
                                    [else 8])])
                        (alignment-via-lookahead
                         size (cdr types) int* fp* fp-in-int? stack-align varargs-after
                         (lambda (size pad stack-align)
                           (cons (make-cat 'stack '() size pad #f) (loop (cdr types) '() fp* next-varargs-after stack-align)))))]
                     [else
                      (cons (make-cat 'int (list (car int*)) 8 0 #f)
                            (loop (cdr types) (rest-of int* 1 next-varargs-after) (rest-of fp* 0 next-varargs-after)
                                  next-varargs-after stack-align))])]))))))

    (define get-registers
      (lambda (cats kind)
        (let loop ([cats cats])
          (cond
            [(null? cats) '()]
            [(or (eq? kind 'all) (eq? kind (cat-place (car cats))))
             (append (cat-regs (car cats))
                     (loop (cdr cats)))]
            [else (loop (cdr cats))]))))

    (include "ffi-help.ss")

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
                  (lambda (offset size)
                    (lambda (rhs) ; requires rhs
                      (let ([int-type (case size
                                        [(1) 'unsigned-8]
                                        [(2) 'unsigned-16]
                                        [(4) 'unsigned-32]
                                        [else #f])])
                        (cond
                          [(not int-type) `(set! ,(%mref ,%sp ,offset) ,rhs)]
                          [else
                           (let ([tmp %argtmp])
                             (%seq
                              (set! ,tmp ,rhs)
                              (inline ,(make-info-load int-type #f) ,%store ,%sp ,%zero (immediate ,offset) ,tmp)))]))))]
                 [load-indirect-stack
                  ;; used both for arguments passed on stack and argument passed as a pointer to deeper on the stack
                  (lambda (offset from-offset size)
                    (lambda (x) ; requires var
                      (memory-to-memory %sp offset x from-offset size %argtmp)))]
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
                      ;; used only when arguments are not put in fp registers,
                      ;; so ok to use `%Cfparg1`
                      `(seq
                        (set! ,%Cfparg1 ,(%inline double->single ,x))
                        (set! ,reg ,(%inline fpcastto ,%Cfparg1)))))]
                 [load-boxed-double-reg
                  (lambda (fpreg fp-disp)
                    (lambda (x) ; address (always a var) of a flonum
                      `(set! ,fpreg ,(%mref ,x ,%zero ,fp-disp fp))))]
                 [load-boxed-single-reg
                  (lambda (fpreg fp-disp)
                    (lambda (x) ; address (always a var) of a float
                      `(set! ,fpreg ,(%inline load-single ,(%mref ,x ,%zero ,fp-disp fp)))))]
                 [load-int-reg
                  (lambda (ireg)
                    (lambda (x)
                      `(set! ,ireg ,x)))]
                 [load-int-indirect-reg
                  (lambda (ireg from-offset size unsigned?)
                    (lambda (x)
                      (memory-to-reg ireg x from-offset size unsigned? %argtmp)))]
                 [compute-stack-argument-space
                  ;; We'll save indirect arguments on the stack, too, but they have to be beyond any
                  ;; arguments that the callee expects. So, calculate how much the callee shoudl expect.
                  (lambda (cats)
                    (let loop ([cats cats] [isp 0])
                      (if (null? cats)
                          isp
                          (let ([cat (car cats)])
                            (if (eq? (cat-place cat) 'stack)
                                (loop (cdr cats) (fx+ isp (cat-size cat) (cat-pad cat)))
                                (loop (cdr cats) isp))))))]
                 [compute-stack-indirect-space
                  (lambda (cats)
                    (let loop ([cats cats] [isp 0])
                      (if (null? cats)
                          isp
                          (let ([cat (car cats)])
                            (loop (cdr cats) (fx+ isp (or (cat-indirect-bytes cat) 0)))))))]
                 [do-args
                  (lambda (types cats indirect-start)
                    (let loop ([types types] [cats cats] [locs '()] [isp 0] [ind-sp indirect-start])
                      (if (null? types)
                          locs
                          (let ([cat (car cats)]
                                [type (car types)]
                                [cats (cdr cats)]
                                [types (cdr types)])
                            (nanopass-case (Ltype Type) type
                              [(fp-double-float)
                               (cond
                                 [(eq? 'fp (cat-place cat))
                                  (loop types cats
                                        (cons (load-double-reg (car (cat-regs cat))) locs)
                                        isp ind-sp)]
                                 [(eq? 'int (cat-place cat))
                                  (loop types cats
                                        (cons (load-double-into-int-reg (car (cat-regs cat))) locs)
                                        isp ind-sp)]
                                 [else
                                  (loop types cats
                                        (cons (load-double-stack isp) locs)
                                        (fx+ isp (cat-size cat) (cat-pad cat)) ind-sp)])]
                              [(fp-single-float)
                               (cond
                                 [(eq? 'fp (cat-place cat))
                                  (loop types cats
                                        (cons (load-single-reg (car (cat-regs cat))) locs)
                                        isp ind-sp)]
                                 [(eq? 'int (cat-place cat))
                                  (loop types cats
                                        (cons (load-single-into-int-reg (car (cat-regs cat))) locs)
                                        isp ind-sp)]
                                 [else
                                  (loop types cats
                                        (cons (load-single-stack isp) locs)
                                        (fx+ isp (cat-size cat) (cat-pad cat)) ind-sp)])]
                              [(fp-ftd& ,ftd)
                               (let ([size ($ftd-size ftd)])
                                 (case (cat-place cat)
                                   [(int)
                                    (let ([indirect-bytes (cat-indirect-bytes cat)])
                                      (cond
                                        [indirect-bytes
                                         ;; pointer to an indirect argument
                                         (safe-assert (fx= 1 (length (cat-regs cat))))
                                         (loop types cats
                                               (cons (let ([ind (load-indirect-stack ind-sp 0 size)])
                                                       (lambda (x)
                                                         (%seq
                                                          ,(ind x)
                                                          (set! ,(car (cat-regs cat)) ,(%inline + ,%sp (immediate ,ind-sp))))))
                                                     locs)
                                               isp (fx+ ind-sp indirect-bytes))]
                                        [else
                                         ;; argument copied to one or more integer registers
                                         (let i-loop ([int* (cat-regs cat)] [size size] [offset 0] [proc #f])
                                           (cond
                                             [(null? int*)
                                              (loop types cats
                                                    (cons proc locs)
                                                    isp ind-sp)]
                                             [else
                                              (i-loop (cdr int*) (fx- size 8) (fx+ offset 8)
                                                      (let ([new-proc (load-int-indirect-reg (car int*) offset (fxmin size 8) ($ftd-unsigned? ftd))])
                                                        (if proc
                                                            (lambda (x) (%seq ,(proc x) ,(new-proc x)))
                                                            new-proc)))]))]))]
                                   [(fp)
                                    (let ([double? (double-member? (car ($ftd->members ftd)))])
                                      ;; argument copied to one or more integer registers
                                      (let f-loop ([fp* (cat-regs cat)] [offset 0] [proc #f])
                                        (cond
                                          [(null? fp*)
                                           (loop types cats
                                                 (cons proc locs)
                                                 isp ind-sp)]
                                          [else
                                           (f-loop (cdr fp*) (fx+ offset (if double? 8 4))
                                                   (let ([new-proc (if double?
                                                                       (load-boxed-double-reg (car fp*) offset)
                                                                       (load-boxed-single-reg (car fp*) offset))])
                                                     (if proc
                                                         (lambda (x) (%seq ,(proc x) ,(new-proc x)))
                                                         new-proc)))])))]
                                   [else
                                    (let ([indirect-bytes (cat-indirect-bytes cat)]
                                          [size-on-stack (cat-size cat)])
                                      (cond
                                        [indirect-bytes
                                         ;; pointer (passed on stack) to an indirect argument (also on stack)
                                         (safe-assert (fx= size-on-stack 8))
                                         (loop types cats
                                               (cons (let ([ind (load-indirect-stack ind-sp 0 size-on-stack)])
                                                       (lambda (x)
                                                         (%seq
                                                          ,(ind x)
                                                          (set! ,(%mref ,%sp ,isp) ,(%inline + ,%sp ,ind)))))
                                                     locs)
                                               (fx+ isp size-on-stack) (fx+ ind-sp indirect-bytes))]
                                        [else
                                         ;; argument copied to stack
                                         (loop types cats
                                               (cons (load-indirect-stack isp 0 size) locs)
                                               (fx+ isp size-on-stack (cat-pad cat)) ind-sp)]))]))]
                              [else
                               ;; integer, scheme-object, etc.
                               (cond
                                 [(eq? 'int (cat-place cat))
                                  (loop types cats
                                        (cons (load-int-reg (car (cat-regs cat))) locs)
                                        isp ind-sp)]
                                 [else
                                  (loop types cats
                                        (cons (load-int-stack isp (cat-size cat)) locs)
                                        (fx+ isp (cat-size cat) (cat-pad cat)) ind-sp)])])))))]
                 [add-fill-result
                  ;; may destroy the values in result registers
                  (lambda (result-cat result-type args-frame-size fill-result-here? e)
                    (nanopass-case (Ltype Type) result-type
                      [(fp-ftd& ,ftd)
                       (let* ([size ($ftd-size ftd)]
                              [tmp %argtmp])
                         (case (and fill-result-here?
                                    (cat-place result-cat))
                           [(int)
                            ;; result is in integer registers
                            (let loop ([int* (cat-regs result-cat)] [offset 0] [size size])
                              (cond
                               [(null? int*) `(seq ,e (set! ,tmp ,(%mref ,%sp ,args-frame-size)))]
                               [else
                                (%seq ,(loop (cdr int*) (fx+ offset 8) (fx- size 8))
                                      ,(reg-to-memory tmp offset (fxmin size 8) (car int*)))]))]
                           [(fp)
                            ;; result is in fp registers, so going to either double or float elements
                            (let* ([double? (double-member? (car ($ftd->members ftd)))])
                              (let loop ([fp* (cat-regs result-cat)] [offset 0])
                                (cond
                                 [(null? fp*) `(seq ,e (set! ,tmp ,(%mref ,%sp ,args-frame-size)))]
                                 [double?
                                  (%seq ,(loop (cdr fp*) (fx+ offset 8))
                                        (set! ,(%mref ,tmp ,%zero ,offset fp) ,(car fp*)))]
                                 [else
                                  (%seq ,(loop (cdr fp*) (fx+ offset 4))
                                        ,(%inline store-single ,(%mref ,tmp ,%zero ,offset fp) ,(car fp*)))])))]
                           [else
                            ;; we passed the pointer to be filled, so nothing more to do here
                            e]))]
                      [else
                       ;; anything else
                       e]))]
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
          (lambda (info)
            (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
            (let* ([arg-type* (info-foreign-arg-type* info)]
                   [result-type (info-foreign-result-type info)]
                   [ftd-result? (nanopass-case (Ltype Type) result-type
                                  [(fp-ftd& ,ftd) #t]
                                  [else #f])]
                   [arg-type* (if ftd-result?
                                  (cdr arg-type*)
                                  arg-type*)]
                   [conv* (info-foreign-conv* info)]
                   [arg-cat* (categorize-arguments arg-type* (extract-varargs-after-conv conv*))]
                   [result-cat (car (categorize-arguments (list result-type) #f))]
                   [result-reg* (cat-regs result-cat)]
                   [fill-result-here? (and ftd-result?
                                           (not (cat-indirect-bytes result-cat))
                                           (not (eq? 'stack (cat-place result-cat))))]
                   [arg-stack-bytes (align 16 (compute-stack-argument-space arg-cat*))]
                   [indirect-stack-bytes (align 16 (compute-stack-indirect-space arg-cat*))]
                   [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)]
                   [locs (do-args arg-type* arg-cat* arg-stack-bytes)]
                   [live* (get-registers arg-cat* 'all)]
                   [live* (if (and ftd-result? (not fill-result-here?))
                              (cons %r8 live*)
                              live*)]
                   [frame-size (align 16 (fx+ arg-stack-bytes
                                              indirect-stack-bytes
                                              (if fill-result-here?
                                                  8
                                                  0)))]
                   [adjust-frame (lambda (op)
                                   (lambda ()
                                     (if (fx= frame-size 0)
                                         `(nop)
                                         `(set! ,%sp (inline ,null-info ,op ,%sp (immediate ,frame-size))))))])
              (values
               (adjust-frame %-)
               (let ([locs (reverse locs)])
                 (cond
                  [fill-result-here?
                   ;; stash extra argument on the stack to be retrieved after call and filled with the result:
                   (cons (load-int-stack (fx+ arg-stack-bytes indirect-stack-bytes) 8) locs)]
                  [ftd-result?
                   ;; callee expects pointer to fill for return in %r8:
                   (cons (lambda (rhs) `(set! ,%r8 ,rhs)) locs)]
                  [else locs]))
               (lambda (t0 not-varargs?)
                 (add-fill-result result-cat result-type (fx+ arg-stack-bytes indirect-stack-bytes) fill-result-here?
                                  (add-deactivate adjust-active? t0 live* result-reg*
                                                  (lambda (t0)
                                                    `(inline ,(make-info-kill*-live* (add-caller-save-registers result-reg*) live*) ,%c-call ,t0)))))
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
                    [else (sorry! who "unexpected asm-foreign-procedures fp-integer size ~s" bits)])]
                 [(fp-unsigned ,bits)
                  (case bits
                    [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline zext8 ,%Cretval)))]
                    [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline zext16 ,%Cretval)))]
                    [(32) (lambda (lvalue) `(set! ,lvalue ,(%inline zext32 ,%Cretval)))]
                    [(64) (lambda (lvalue) `(set! ,lvalue ,%Cretval))]
                    [else (sorry! who "unexpected asm-foreign-procedures fp-unsigned size ~s" bits)])]
                 [else (lambda (lvalue) `(set! ,lvalue ,%Cretval))])
               (adjust-frame %+)))
            ))))

    (define-who asm-foreign-callable
      #|
        Frame Layout
                   +---------------------------+
                   |                           |
                   |    incoming stack args    |
                   |                           |
                   +---------------------------+<- 16-byte boundary
                   |    saved int reg args     |
                   |    + %r8 for indirect     |
                   |    + maybe padding        |
                   +---------------------------+<- 16-byte boundary
                   |                           |
                   |   saved float reg args    |
                   |    + maybe padding        |
                   +---------------------------+<- 16-byte boundary
                   |                           |
                   |     activatation state    |
                   |       if necessary        |
                   +---------------------------+<- 16-byte boundary
                   |                           |
                   |      &-return space       |
                   |       if necessary        |
                   +---------------------------+<- 16-byte boundary
                   |                           |
                   |   callee-save regs + lr   |
                   |   callee-save fpregs      |
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
            ;; all of the args are on the stack at this point, though not contiguous since
            ;; we push all of the int reg args with one set of push instructions and all of the
            ;; float reg args with another set of push instructions
            (lambda (arg-type* arg-cat* init-int-reg-offset float-reg-offset stack-arg-offset return-offset
                               synthesize-first? indirect-result?)
              (let loop ([types arg-type*]
                         [cats arg-cat*]
                         [locs '()]
                         [int-reg-offset (if indirect-result? (fx+ init-int-reg-offset 8) init-int-reg-offset)]
                         [float-reg-offset float-reg-offset]
                         [stack-arg-offset stack-arg-offset])
                (if (null? types)
                    (let ([locs (reverse locs)])
                      (cond
                       [synthesize-first?
                        (cons (load-stack-address return-offset) locs)]
                       [indirect-result?
                        (cons (load-word-stack init-int-reg-offset) locs)]
                       [else locs]))
                    (let ([cat (car cats)]
                          [type (car types)]
                          [cats (cdr cats)]
                          [types (cdr types)])
                      (nanopass-case (Ltype Type) type
                        [(fp-double-float)
                         (case (cat-place cat)
                           [(fp)
                            (loop types cats
                                  (cons (load-double-stack float-reg-offset) locs)
                                  int-reg-offset (fx+ float-reg-offset 8) stack-arg-offset)]
                           [(int)
                            (loop types cats
                                  (cons (load-double-stack int-reg-offset) locs)
                                  (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset)]
                           [else
                            (loop types cats
                                  (cons (load-double-stack stack-arg-offset) locs)
                                  int-reg-offset float-reg-offset (fx+ stack-arg-offset (cat-size cat) (cat-pad cat)))])]
                        [(fp-single-float)
                         (case (cat-place cat)
                           [(fp)
                            (loop types cats
                                  (cons (load-single-stack float-reg-offset) locs)
                                  int-reg-offset (fx+ float-reg-offset 8) stack-arg-offset)]
                           [(int)
                            (loop types cats
                                  (cons (load-single-stack int-reg-offset) locs)
                                  (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset)]
                           [else
                            (loop types cats
                                  (cons (load-single-stack stack-arg-offset) locs)
                                  int-reg-offset float-reg-offset (fx+ stack-arg-offset (cat-size cat) (cat-pad cat)))])]
                        [(fp-ftd& ,ftd)
                         (case (cat-place cat)
                           [(int)
                            (let ([indirect-bytes (cat-indirect-bytes cat)])
                              (cond
                               [indirect-bytes
                                ;; pointer to an indirect argument
                                (safe-assert (fx= (length (cat-regs cat)) 1))
                                (loop types cats
                                      (cons (load-word-stack int-reg-offset) locs)
                                      (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset)]
                               [else
                                ;; point to argument on stack
                                (loop types cats
                                      (cons (load-stack-address int-reg-offset) locs)
                                      (fx+ int-reg-offset (cat-size cat) (cat-pad cat)) float-reg-offset stack-arg-offset)]))]
                           [(fp)
                            ;; point to argument, but if they're floats, then we need to
                            ;; shift double-sized registers into float-sized elements
                            (loop types cats
                                  (cons (let ([proc (load-stack-address float-reg-offset)]
                                              [members ($ftd->members ftd)])
                                          (cond
                                           [(or (null? (cdr members))
                                                (double-member? (car members)))
                                            proc]
                                           [else
                                            ;; instead of compacting here, it might be nicer to
                                            ;; save registers in packed form in the first place
                                            ;; (which means that `(cat-size cat)` would be a multiple of 4)
                                            (lambda (lvalue)
                                              (let loop ([members (cdr members)]
                                                         [dest-offset (fx+ float-reg-offset 4)]
                                                         [src-offset (fx+ float-reg-offset 8)])
                                                (if (null? members)
                                                    (proc lvalue)
                                                    (let ([tmp %argtmp])
                                                      (%seq
                                                       (set! ,tmp (inline ,(make-info-load 'unsigned-32 #f) ,%load ,%sp ,%zero (immediate ,src-offset)))
                                                       (inline ,(make-info-load 'unsigned-32 #f) ,%store ,%sp ,%zero (immediate ,dest-offset) ,%argtmp)
                                                       ,(loop (cdr members) (fx+ dest-offset 4) (fx+ src-offset 8)))))))]))
                                        locs)
                                  int-reg-offset (fx+ float-reg-offset (cat-size cat) (cat-pad cat)) stack-arg-offset)]
                           [else
                            (let ([indirect-bytes (cat-indirect-bytes cat)])
                              (cond
                               [indirect-bytes
                                ;; pointer (passed on stack) to an indirect argument (also on stack)
                                (safe-assert (fx= (cat-size cat) 8))
                                (loop types cats
                                      (cons (load-word-stack stack-arg-offset) locs)
                                      int-reg-offset float-reg-offset (fx+ stack-arg-offset 8))]
                               [else
                                ;; point to argument on stack
                                (loop types cats
                                      (cons (load-stack-address stack-arg-offset) locs)
                                      int-reg-offset float-reg-offset (fx+ stack-arg-offset (cat-size cat) (cat-pad cat)))]))])]
                        [else
                         ;; integer, scheme-object, etc.
                         (case (cat-place cat)
                           [(int)
                            (loop types cats
                                  (cons (load-int-stack type int-reg-offset) locs)
                                  (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset)]
                           [else
                            (loop types cats
                                  (cons (load-int-stack type stack-arg-offset) locs)
                                  int-reg-offset float-reg-offset (fx+ stack-arg-offset (cat-size cat) (cat-pad cat)))])]))))))
          (define do-result
            (lambda (result-type result-cat synthesize-first? return-stack-offset)
              (nanopass-case (Ltype Type) result-type
                [(fp-double-float)
                 (lambda (rhs)
                   `(set! ,%Cfpretval ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp)))]
                [(fp-single-float)
                 (lambda (rhs)
                   `(set! ,%Cfpretval ,(%inline double->single ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp))))]
                [(fp-void)
                 (lambda () `(nop))]
                [(fp-ftd& ,ftd)
                 (cond
                  [(cat-indirect-bytes result-cat)
                   ;; we passed the pointer to be filled, so nothing more to do here
                   (lambda () `(nop))]
                  [else
                   (case (cat-place result-cat)
                     [(int)
                      (let ([to-regs
                             (lambda (x offset)
                               (let loop ([int* (cat-regs result-cat)] [offset offset] [size ($ftd-size ftd)])
                                 (cond
                                  [(null? int*) `(nop)]
                                  [else
                                   (safe-assert (not (eq? (car int*) x)))
                                   (%seq
                                    ,(loop (cdr int*) (fx+ offset 8) (fx- size 8))
                                    ,(memory-to-reg (car int*) x offset (fxmin size 8) ($ftd-unsigned? ftd) %argtmp))])))])
                        (if synthesize-first?
                            (lambda ()
                              (to-regs %sp return-stack-offset))
                            (lambda (x)
                              (to-regs x 0))))]
                     [(fp)
                      (let* ([double? (double-member? (car ($ftd->members ftd)))])
                        (let ([to-regs
                               (lambda (x offset)
                                 (let loop ([fp* (cat-regs result-cat)] [offset offset])
                                   (cond
                                    [(null? fp*) `(nop)]
                                    [double?
                                     (%seq ,(loop (cdr fp*) (fx+ offset 8))
                                           (set! ,(car fp*) ,(%mref ,x ,%zero ,offset fp)))]
                                    [else
                                     (%seq ,(loop (cdr fp*) (fx+ offset 4))
                                           (set! ,(car fp*) ,(%inline load-single ,(%mref ,x ,%zero ,offset fp))))])))])
                          (if synthesize-first?
                              (lambda ()
                                (to-regs %sp return-stack-offset))
                              (lambda (x)
                                (to-regs x 0)))))]
                     [else
                      ;; we passed the pointer to be filled, so nothing more to do here
                      (lambda () `(nop))])])]
                [else
                 ;; integer, scheme-object, etc.
                 (lambda (x)
                   `(set! ,%Cretval ,x))])))
          (lambda (info)
            (define callee-save-regs+lr (cons* %lr
                                               ;; reserved:
                                               %tc %sfp %ap %trap
                                               ;; allocable:
                                               (get-allocable-callee-save-regs 'uptr)))
            (define callee-save-fpregs  (get-allocable-callee-save-regs 'fp))
            (define isaved (length callee-save-regs+lr))
            (define fpsaved (length callee-save-fpregs))
            (let* ([arg-type* (info-foreign-arg-type* info)]
                   [result-type (info-foreign-result-type info)]
                   [ftd-result? (nanopass-case (Ltype Type) result-type
                                  [(fp-ftd& ,ftd) #t]
                                  [else #f])]
                   [arg-type* (if ftd-result?
                                  (cdr arg-type*)
                                  arg-type*)]
                   [conv* (info-foreign-conv* info)]
                   [arg-cat* (categorize-arguments arg-type* (extract-varargs-after-conv conv*))]
                   [result-cat (car (categorize-arguments (list result-type) #f))]
                   [synthesize-first? (and ftd-result?
                                           (not (cat-indirect-bytes result-cat))
                                           (not (eq? 'stack (cat-place result-cat))))]
                   [indirect-result? (and ftd-result? (not synthesize-first?))]
                   [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)]
                   [arg-regs (let ([regs (get-registers arg-cat* 'int)])
                               (if indirect-result?
                                   (cons %r8 regs)
                                   regs))]
                   [arg-fp-regs (get-registers arg-cat* 'fp)]
                   [result-regs (get-registers (list result-cat) 'all)])
                (let ([int-reg-bytes (fx* (align 2 (length arg-regs)) 8)]
                      [float-reg-bytes (fx* (align 2 (length arg-fp-regs)) 8)]
                      [active-state-bytes (if adjust-active? 16 0)]
                      [return-bytes (if synthesize-first? (align 16 (cat-size result-cat)) 0)]
                      [callee-save-bytes (fx* 8
                                              (fx+ (align 2 (length callee-save-regs+lr))
                                                   (align 2 (length callee-save-fpregs))))])
                  (let* ([return-offset callee-save-bytes]
                         [active-state-offset (fx+ return-offset return-bytes)]
                         [arg-fpregs-offset (fx+ active-state-offset active-state-bytes)]
                         [arg-regs-offset (fx+ arg-fpregs-offset float-reg-bytes)]
                         [args-offset (fx+ arg-regs-offset int-reg-bytes)])
                    (values
                     (lambda ()
                       (%seq
                        ;; save argument register values to the stack so we don't lose the values
                        ;; across possible calls to C while setting up the tc and allocating memory
                        ,(if (null? arg-regs) `(nop) `(inline ,(make-info-kill*-live* '() arg-regs) ,%push-multiple))
                        ,(if (null? arg-fp-regs) `(nop) `(inline ,(make-info-kill*-live* '() arg-fp-regs) ,%push-fpmultiple))
                        ;; make room for active state and/or return bytes
                        ,(let ([len (+ active-state-bytes return-bytes)])
                           (if (fx= len 0) `(nop) `(set! ,%sp ,(%inline - ,%sp (immediate ,len)))))
                        ;; save the callee save registers & return address
                        (inline ,(make-info-kill*-live* '() callee-save-regs+lr) ,%push-multiple)
                        (inline ,(make-info-kill*-live* '() callee-save-fpregs) ,%push-fpmultiple)
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
                     (do-args arg-type* arg-cat* arg-regs-offset arg-fpregs-offset args-offset return-offset
                              synthesize-first? indirect-result?)
                     (do-result result-type result-cat synthesize-first? return-offset)
                     (lambda ()
                       (in-context Tail
                        (%seq
                         ,(if adjust-active?
                              (%seq
                               ;; We need *(sp+active-state-offset) in %Carg1,
                               ;; but that can also be a return register.
                               ;; Meanwhle, sp may change before we call unactivate.
                               ;; So, move to %r2 for now, then %Carg1 later:
                               (set! ,%argtmp ,(%mref ,%sp ,active-state-offset))
                               ,(save-and-restore
                                 result-regs
                                 `(seq
                                   (set! ,%Carg1 ,%argtmp)
                                   ,(%inline unactivate-thread ,%Carg1))))
                              `(nop))
                         ;; restore the callee save registers
                         (inline ,(make-info-kill* callee-save-fpregs) ,%pop-fpmultiple)
                         (inline ,(make-info-kill* callee-save-regs+lr) ,%pop-multiple)
                         ;; deallocate space for pad & arg reg values
                         (set! ,%sp ,(%inline + ,%sp (immediate ,(fx+ active-state-bytes return-bytes float-reg-bytes int-reg-bytes))))
                         ;; done
                         (asm-c-return ,null-info ,callee-save-regs+lr ... ,callee-save-fpregs ... ,result-regs ...)))))))))))))
)
