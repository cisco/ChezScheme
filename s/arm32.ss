;;; arm32.ss
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; SECTION 1: registers
;;; ABI:
;;;  Register usage:
;;;   r0-r3 aka a1-a4: C argument registers, caller-save
;;;   r4-r8, r10, r11 aka v1-v5, v7, v8: callee-save
;;;   r9 aka v6, sb, or tr: platform-specific, callee-save
;;;   r12 aka ip: caller-save (possibly usurped by linker at call boundaries)
;;;   r13 aka sp: C stack pointer
;;;   r14 aka lr: link register
;;;   r15 aka pc: program counter
;;;   --------
;;;   s0-s31: single-precision registers (with vfp-v2) overlap with d0-d15
;;;   d0-d15: double-precision registers (with vfp-v2)
;;;   d16-d31: double-precision registers (with vfp-v3)
;;;  Alignment:
;;;   double-floats & 64-bit integers are 8-byte aligned in structs
;;;   double-floats & 64-bit integers are 8-byte aligned on the stack
;;;   stack must be 8-byte aligned at call boundaries (otherwise 4-byte)
;;;  Parameter passing:
;;;   8- and 16-bit integer arguments zero- or sign-extended to 32-bits
;;;   32-bit integer arguments passed in a1-a4, then on stack
;;;   64-bit integer arguments passed in a1 or a3, then on stack
;;;       little-endian: a1 (a3) holds lsw, a2 (a4) holds msw
;;;       big-endian: a1 (a3) holds msw, a2 (a4) holds lsw
;;;   8- and 16-bit integer return value zero- or sign-extended to 32-bits
;;;   32-bit integer return value returned in r0 (aka a1)
;;;   64-bit integer return value passed in r0 & r1 (aka a1 & a2)
;;;       little-endian: r0 holds lsw, r1 holds msw
;;;       big-endian: r0 holds msw, r1 holds lsw
;;;   single-floats passed in s0-s15
;;;   double-floats passed in d0-d7 (overlapping single)
;;;   float return value returned in s0 or d0
;;;   must allocate to a single-float reg if it's passed by for double-float alignment
;;;     (e.g., single, double, single => s0, d1, s1)
;;;   ... unless a double has been stack-allocated
;;;     (e.g., 15 singles, double => s0-s14, stack, stack)
;;;   stack grows downwards.  first stack args passed at lowest new frame address.
;;;   return address passed in LR
;;; questions:
;;;   least significant bit is always designated as bit 0...how does this affect
;;;      bit fields in big-endian mode?
;;; meta questions:
;;;   should we have both little- and big-endian support?
;;;   can pidora (or some other linux distribution) run using both little- and big-endian modes?

(define-registers
  (reserved
    [%tc  %r9                   #t  9 uptr]
    [%sfp %r10                  #t 10 uptr]
    [%ap  %r5                   #t  5 uptr]
    #;[%esp]
    #;[%eap]
    [%trap %r8                  #t  8 uptr])
  (allocable
    [%ac0 %r4                   #t  4 uptr]
    [%xp  %r6                   #t  6 uptr]
    [%ts  %ip                   #f 12 uptr]
    [%td  %r11                  #t 11 uptr]
    #;[%ret]
    [%cp  %r7                   #t  7 uptr]
    #;[%ac1]
    #;[%yp]
    [     %r0  %Carg1 %Cretval  #f  0 uptr]
    [     %r1  %Carg2           #f  1 uptr]
    [     %r2  %Carg3 %reify1   #f  2 uptr]
    [     %r3  %Carg4 %reify2   #f  3 uptr]
    [     %lr                   #f 14 uptr] ; %lr is trashed by 'c' calls including calls to hand-coded routines like get-room
    [%fp1           %d8  %s16   #t 16 fp]   ; allocable fp regs must not overlap with any half registers
    [%fp2           %d9  %s18   #t 18 fp]
  )
  (machine-dependent
    [%sp                        #t 13 uptr]
    [%pc                        #f 15 uptr]
    [%Cfparg1 %Cfpretval %d0  %s0   #f  0 fp] ; < 32: low bit goes in D, N, or M bit, high bits go in Vd, Vn, Vm
    [%Cfparg1b                %s1   #f  1 fp]
    [%Cfparg2            %d1  %s2   #f  2 fp]
    [%Cfparg2b                %s3   #f  3 fp]
    [%Cfparg3            %d2  %s4   #f  4 fp]
    [%Cfparg3b                %s5   #f  5 fp]
    [%Cfparg4            %d3  %s6   #f  6 fp]
    [%Cfparg4b                %s7   #f  7 fp]
    [%Cfparg5            %d4  %s8   #f  8 fp]
    [%Cfparg5b                %s9   #f  9 fp]
    [%Cfparg6            %d5  %s10  #f 10 fp]
    [%Cfparg6b                %s11  #f 11 fp]
    [%Cfparg7            %d6  %s12  #f 12 fp]
    [%Cfparg7b                %s13  #f 13 fp]
    [%Cfparg8            %d7  %s14  #f 14 fp]
    [%Cfparg8b                %s15  #f 15 fp]
    ;; etc., but other FP registers are preserved
    #;[                  %d16       #t 32 fp] ; >= 32: high bit goes in D, N, or M bit, low bits go in Vd, Vn, Vm
    #;[                  %d17       #t 33 fp]
    ; etc.
    ))

;;; SECTION 2: instructions
(module (md-handle-jump  ; also sets primitive handlers
         mem->mem
         fpmem->fpmem
         coercible?
         coerce-opnd)
  (import asm-module)

  (define imm-funky12?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (and (funky12 imm) #t)]
        [else #f])))

  (define imm-negate-funky12?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (and (funky12 (- imm)) #t)]
        [else #f])))

  (define imm-lognot-funky12?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (and (funky12 (lognot imm)) #t)]
        [else #f])))

  (define imm-shift-count?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (shift-count? imm)]
        [else #f])))

  (define imm-unsigned8?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (unsigned8? imm)]
        [else #f])))

  (define imm-unsigned12?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (unsigned12? imm)]
        [else #f])))

  (define imm-constant?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) #t]
        [else #f])))

  (define uword8?
    (lambda (imm)
      (and (fixnum? imm) ($fxu< imm (expt 2 10)) (not (fxlogtest imm #b11)))))

  (define imm-uword8?
    ;; immediate is a nonnegative 8-bit word offset
    ;; effectively 8-bit unsigned left-shifted by 2
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (uword8? imm)]
        [else #f])))

  (define-pass imm->negate-imm : (L15c Triv) (ir) -> (L15d Triv) ()
    (Triv : Triv (ir) -> Triv ()
      [(immediate ,imm) `(immediate ,(- imm))]
      [else (sorry! who "~s is not an immediate" ir)]))

  (define-pass imm->lognot-imm : (L15c Triv) (ir) -> (L15d Triv) ()
    (Triv : Triv (ir) -> Triv ()
      [(immediate ,imm) `(immediate ,(lognot imm))]
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
                   [(and (eq? x1 %zero) (or (unsigned12? imm) (unsigned12? (- imm))))
                    (return x0 %zero imm type)]
                   [(funky12 imm)
                    ; NB: dubious value?  check to see if it's exercised
                    (let ([u (make-tmp 'u)])
                      (seq
                        (build-set! ,u (asm ,null-info ,(asm-add #f) ,x0 (immediate ,imm)))
                        (return u x1 0 type)))]
                   [(funky12 (- imm))
                    ; NB: dubious value?  check to see if it's exercised
                    (let ([u (make-tmp 'u)])
                      (seq
                        (build-set! ,u (asm ,null-info ,(asm-sub #f) ,x0 (immediate ,(- imm))))
                        (return u x1 0 type)))]
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

  (define fpmem->fpmem ; allows mem argument, too
    (lambda (a k)
      (define return
        (lambda (x0 x1 imm)
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm fp)))))
      (nanopass-case (L15c Triv) a
        [(mref ,lvalue0 ,lvalue1 ,imm ,type)
         (lvalue->ur lvalue0
           (lambda (x0)
             (lvalue->ur lvalue1
               (lambda (x1)
                 (cond
                   [(not (and (<= 0 imm #x3FF)
                              (fx= 0 (fxand imm #b11))))
                    ;; offset not aligned or out of range
                    (let ([u (make-tmp 'umov)])
                      (seq
                       (build-set! ,u (asm ,null-info ,(asm-add #f) ,x0 (immediate ,imm)))
                       (if (eq? x1 %zero)
                           (return u %zero 0)
                           (seq
                            (build-set! ,u (asm ,null-info ,(asm-add #f) ,u ,x1))
                            (return u %zero 0)))))]
                   [(not (eq? x1 %zero))
                    (let ([u (make-tmp 'umov)])
                      (seq
                       (build-set! ,u (asm ,null-info ,(asm-add #f) ,x0 ,x1))
                       (return u %zero imm)))]
                   [else
                    (return x0 %zero imm)])))))])))

  ;; `define-instruction` code takes care of `ur` and `fpur`, to which
  ;; all type-compatible values must convert
  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (and (memq 'funky12 aty*) (imm-funky12? a))
             (and (memq 'negate-funky12 aty*) (imm-negate-funky12? a))
             (and (memq 'lognot-funky12 aty*) (imm-lognot-funky12? a))
             (and (memq 'shift-count aty*) (imm-shift-count? a))
             (and (memq 'unsigned8 aty*) (imm-unsigned8? a))
             (and (memq 'unsigned12 aty*) (imm-unsigned12? a))
             (and (memq 'imm-constant aty*) (imm-constant? a))
             (and (memq 'uword8 aty*) (imm-uword8? a))
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
           [(and (memq 'funky12 aty*) (imm-funky12? a)) (k (imm->imm a))]
           [(and (memq 'negate-funky12 aty*) (imm-negate-funky12? a)) (k (imm->negate-imm a))]
           [(and (memq 'lognot-funky12 aty*) (imm-lognot-funky12? a)) (k (imm->lognot-imm a))]
           [(and (memq 'shift-count aty*) (imm-shift-count? a)) (k (imm->imm a))]
           [(and (memq 'unsigned8 aty*) (imm-unsigned8? a)) (k (imm->imm a))]
           [(and (memq 'unsigned12 aty*) (imm-unsigned12? a)) (k (imm->imm a))]
           [(and (memq 'imm-constant aty*) (imm-constant? a)) (k (imm->imm a))]
           [(and (memq 'uword8 aty*) (imm-uword8? a)) (k (imm->imm a))]
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
           ; NB: really need to use unspillable or mark %ip (aka %ts) killed here but can't without extending jump syntax
           (values '() `(jump (literal ,info)))]
          [(label-ref ,l ,offset)
           ; NB: really need to use unspillable or mark %ip (aka %ts) killed here but can't without extending jump syntax
           (values '() `(jump (label-ref ,l ,offset)))]
          [else (long-form t)]))))

  (define info-cc-eq (make-info-condition-code 'eq? #f #t))
  (define asm-eq (asm-relop info-cc-eq))

  ; x is not the same as z in any clause that follows a clause where (x z)
  ; and y is coercible to one of its types, however:
  ; WARNING: do not assume that if x isn't the same as z then x is independent
  ; of z, since x might be an mref with z as it's base or index

  (define-instruction value (- -/ovfl -/eq -/pos)
    [(op (z ur) (x ur) (y funky12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub (not (eq? op '-))) ,x ,y))]
    [(op (z ur) (x funky12) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-rsb (not (eq? op '-))) ,y ,x))]
    [(op (z ur) (x ur) (y negate-funky12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (not (eq? op '-))) ,x ,y))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub (not (eq? op '-))) ,x ,y))])

  (define-instruction value (+ +/ovfl +/carry)
    [(op (z ur) (x ur) (y funky12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (not (eq? op '+))) ,x ,y))]
    [(op (z ur) (x funky12) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (not (eq? op '+))) ,y ,x))]
    [(op (z ur) (x ur) (y negate-funky12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub (not (eq? op '+))) ,x ,y))]
    [(op (z ur) (x negate-funky12) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub (not (eq? op '+))) ,y ,x))]
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
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,null-info ,asm-smull ,x ,y ,u))
         `(asm ,null-info ,(asm-cmp/shift 31 'sra) ,u ,z)))])

  ; NB: only on ARMv7VE implementations
  #;(define-instruction value (/)
    ; does not affect condition codes
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-div ,x ,y))])

  (define-instruction value (logand)
    [(op (z ur) (x ur) (y funky12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,x ,y))]
    [(op (z ur) (x funky12) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,y ,x))]
    [(op (z ur) (x ur) (y lognot-funky12))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-bic #f) ,x ,y))]
    [(op (z ur) (x lognot-funky12) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-bic #f) ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,x ,y))])

  (let ()
    (define select-op (lambda (op) (if (eq? op 'logor) asm-logor asm-logxor)))
    (define-instruction value (logor logxor)
      [(op (z ur) (x funky12) (y ur))
       `(set! ,(make-live-info) ,z (asm ,info ,((select-op op) #f) ,y ,x))]
      [(op (z ur) (x ur) (y funky12 ur))
       `(set! ,(make-live-info) ,z (asm ,info ,((select-op op) #f) ,x ,y))]))

  (define-instruction value (lognot)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))])

  (define-instruction value (sll srl sra)
    [(op (z ur) (x ur) (y ur shift-count))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-shiftop op) ,x ,y))])

  (define-instruction value (move)
    [(op (z mem) (x ur))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem imm-constant))
     `(set! ,(make-live-info) ,z ,x)])

  (define-instruction value lea1
    ; NB: would be simpler if offset were explicit operand
    ; NB: why not one version of lea with %zero for y in lea1 case?
    [(op (z ur) (x ur))
     (begin
       (let ([offset (info-lea-offset info)])
         (if (funky12 offset)
             `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,x (immediate ,offset)))
             (let ([u (make-tmp 'u)])
               (seq
                 `(set! ,(make-live-info) ,u (immediate ,offset))
                 `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,x ,u)))))))])

  (define-instruction value lea2
    ; NB: would be simpler if offset were explicit operand
    [(op (z ur) (x ur) (y ur))
     (let ([offset (info-lea-offset info)] [u (make-tmp 'u)])
       (seq
         (if (funky12 offset)
             `(set! ,(make-live-info) ,u (asm ,info ,(asm-add #f) ,y (immediate ,offset)))
             (seq
               `(set! ,(make-live-info) ,u (immediate ,offset))
               `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,u ,y))))
         `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,x ,u))))])

  (define-instruction value (sext8 sext16 zext8 zext16)
    [(op (z ur) (x ur)) `(set! ,(make-live-info) ,z (asm ,info ,(asm-move/extend op) ,x))])

  (let ()
    (define imm-zero (with-output-language (L15d Triv) `(immediate 0)))
    (define load/store
      (lambda (x y w imm8? k) ; x ur, y ur, w ur or imm
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
                  [(if imm8?
                       (or (unsigned8? n) (unsigned8? (- n)))
                       (or (unsigned12? n) (unsigned12? (- n))))
                   (let ([w (in-context Triv `(immediate ,n))])
                     (if (or (eq? y %zero) (fx= n 0))
                         (k x y w)
                         (let ([u (make-tmp 'u)])
                           (seq
                             `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,y))
                             (k u %zero w)))))]
                  [(funky12 n) =>
                   ; NB: dubious value?  check to see if it's exercised
                   (lambda (n)
                     (let ([u (make-tmp 'u)])
                       (seq
                         `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x (immediate ,n)))
                         (k u y imm-zero))))]
                  [(funky12 (- n)) =>
                   ; NB: dubious value?  check to see if it's exercised
                   (lambda (n)
                     (let ([u (make-tmp 'u)])
                       (seq
                         `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-sub #f) ,x (immediate ,n)))
                         (k u y imm-zero))))]
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
         (load/store x y w (memq type '(integer-16 unsigned-16 integer-8))
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
         (load/store x y w (memq type '(integer-16 unsigned-16))
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
    [(op (x fpur) (y fpmem fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove-single ,y))])

  (define-instruction value (single->double double->single)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fl-cvt op) ,y))])

  (let ()
    (define (fpmem->mem mem dir)
      (with-output-language (L15d Triv)
        (nanopass-case (L15d Triv) mem
          [(mref ,x1 ,x2 ,imm ,type)
           (safe-assert (eq? type 'fp))
           (let ([delta (constant-case native-endianness
                          [(little) (if (eq? dir 'lo) 0 4)]
                          [(big) (if (eq? dir 'hi) 0 4)])])
             `(mref ,x1 ,x2 ,(fx+ imm delta) uptr))]
          [else (sorry! 'fpmem->mem "unexpected reference ~s" mem)])))

    (define-instruction value (fpt)
      [(op (x fpur) (y ur))
       (let ([u (make-tmp 'u 'fp)])
         (seq
          `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
          `(set! ,(make-live-info) ,x (asm ,info ,asm-fpt ,y, u))))])

    (define-instruction value (fpmove)
      [(op (x fpmem) (y fpur)) `(set! ,(make-live-info) ,x ,y)]
      [(op (x fpur) (y fpmem fpur)) `(set! ,(make-live-info) ,x ,y)])

    (define-instruction value (fpcastto/hi)
      [(op (x ur) (y fpmem)) `(set! ,(make-live-info) ,x ,(fpmem->mem y 'hi))]
      [(op (x ur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpcastto 'hi) ,y))])

    (define-instruction value (fpcastto/lo)
      [(op (x ur) (y fpmem)) `(set! ,(make-live-info) ,x ,(fpmem->mem y 'lo))]
      [(op (x ur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpcastto 'lo) ,y))])

    (define-instruction value (fpcastfrom)
      [(op (x fpmem) (hi ur) (lo ur)) (seq
                                       `(set! ,(make-live-info) ,(fpmem->mem x 'lo) ,lo)
                                       `(set! ,(make-live-info) ,(fpmem->mem x 'hi) ,hi))]
      [(op (x fpur) (hi ur) (lo ur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcastfrom ,lo ,hi))]))

  (define-instruction value (fp+ fp- fp/ fp*)
    [(op (x fpur) (y fpur) (z fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpop-2 op) ,y ,z))])

  (define-instruction value (fpsqrt)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsqrt ,y))])

  (define-instruction value (fptrunc)
    [(op (z ur) (x fpur))
     (let ([u (make-tmp 'u 'fp)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,asm-fptrunc ,x ,u))))])

  (define-instruction value (fpsingle)
    [(op (x fpur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsingle ,y))])

  (define-instruction pred (fp= fp< fp<=)
    [(op (x fpur) (y fpur))
     (let ([info (make-info-condition-code op #f #f)])
       (values '() `(asm ,info ,(asm-fp-relop info) ,x ,y)))])

  (define-instruction effect (inc-cc-counter)
    [(op (x ur) (w ur funky12) (z funky12 ur))
     (let ([u1 (make-tmp 'u1)] [u2 (make-tmp 'u2)])
       (seq
         `(set! ,(make-live-info) ,u1 (asm ,null-info ,(asm-add #f) ,x ,w))
         `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
         `(asm ,null-info ,asm-inc-cc-counter ,u1 ,z ,u2)))])

  (define-instruction effect (inc-profile-counter)
    [(op (x mem) (y ur funky12))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u ,x)
         `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,u ,y))
         `(set! ,(make-live-info) ,x ,u)))])

  (define-instruction value (read-time-stamp-counter)
    [(op (z ur)) `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-read-counter 1)))])

  (define-instruction value (read-performance-monitoring-counter)
    [(op (z ur) (x unsigned8))
     ; could require unsigned1 and skip the fxmin...but no point burdening instruction scheduler with an additional one-off type
     (let ([imm (nanopass-case (L15d Triv) x [(immediate ,imm) (fxmin imm 1)])])
       `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-read-counter (fx+ imm 2)))))]
    [(op (z ur) (x ur)) `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-read-counter) ,x))])

  ;; no kills since we expect to be called when all necessary state has already been saved
  (define-instruction value (get-tc)
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)] [ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,info ,asm-get-tc ,u ,ulr))))])

  (define-instruction value activate-thread
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)] [ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,info ,asm-activate-thread ,u ,ulr))))])

  (define-instruction effect deactivate-thread
    [(op)
     (let ([u (make-tmp 'u)] [ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-deactivate-thread ,u ,ulr)))])

  (define-instruction effect unactivate-thread
    [(op (x ur))
     (safe-assert (eq? x %Carg1))
     (let ([u (make-tmp 'u)] [ulr (make-precolored-unspillable 'ulr %lr)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-unactivate-thread ,x ,u ,ulr)))])

  (define-instruction value (asmlibcall)
    [(op (z ur))
     (let ([u (make-tmp 'u)])
       (if (info-asmlib-save-ra? info)
           (seq
             `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
             `(set! ,(make-live-info) ,z (asm ,info ,(asm-library-call (info-asmlib-libspec info) #t) ,u ,(info-kill*-live*-live* info) ...)))
           (let ([ulr (make-precolored-unspillable 'ulr %lr)])
             (seq
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
               `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
               `(set! ,(make-live-info) ,z (asm ,info ,(asm-library-call (info-asmlib-libspec info) #f) ,u ,ulr ,(info-kill*-live*-live* info) ...))))))])

  (define-instruction effect (asmlibcall!)
    [(op)
     (let ([u (make-tmp 'u)])
       (if (info-asmlib-save-ra? info)
           (let ([ulr (make-precolored-unspillable 'ulr %lr)])
             (seq
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
               `(asm ,info ,(asm-library-call! (info-asmlib-libspec info) #t) ,u ,(info-kill*-live*-live* info) ...)))
           (let ([ulr (make-precolored-unspillable 'ulr %lr)])
             (seq
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
               `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
               `(asm ,info ,(asm-library-call! (info-asmlib-libspec info) #f) ,u ,ulr ,(info-kill*-live*-live* info) ...)))))])

  (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
  (define-instruction effect (c-simple-call)
    [(op)
     (let ([u (make-tmp 'u)])
       (if (info-c-simple-call-save-ra? info)
           (seq
             `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
             `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info) #t) ,u))
           (let ([ulr (make-precolored-unspillable 'ulr %lr)])
             (seq
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
               `(set! ,(make-live-info) ,ulr (asm ,null-info ,asm-kill))
               `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info) #f) ,u ,ulr)))))])

  (define-instruction pred (eq? u< < > <= >=)
    [(op (y funky12) (x ur))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #t #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))]
    [(op (x ur) (y ur funky12))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))])

  (define-instruction pred (condition-code)
    [(op) (values '() `(asm ,info ,(asm-condition-code info)))])

  (define-instruction pred (type-check?)
    [(op (x ur) (mask funky12 ur) (type funky12 ur))
     (let ([tmp (make-tmp 'u)])
       (values
         (with-output-language (L15d Effect)
           `(set! ,(make-live-info) ,tmp (asm ,null-info ,(asm-logand #f) ,x ,mask)))
         `(asm ,info-cc-eq ,asm-eq ,tmp ,type)))])

  (define-instruction pred (logtest log!test)
    [(op (x funky12) (y ur))
     (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,y ,x))]
    [(op (x ur) (y ur funky12))
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
                  [(funky12 i)
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
    ; NB: compiler ipmlements init-lock! and unlock! as 32-bit store of zero 
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
    ; NB: user sqrt or something like that?
    [(op) '()])

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

  (define-instruction effect (vpush-multiple)
    [(op) `(asm ,info ,(asm-vpush-multiple (info-vpush-reg info) (info-vpush-n info)))])

  (define-instruction effect (vpop-multiple)
    [(op) `(asm ,info ,(asm-vpop-multiple (info-vpush-reg info) (info-vpush-n info)))])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect (invoke-prelude)
    [(op) `(set! ,(make-live-info) ,%tc ,%Carg1)])
)

;;; SECTION 3: assembler
(module asm-module (; required exports
                     asm-move asm-move/extend asm-load asm-store asm-swap asm-library-call asm-library-call! asm-library-jump
                     asm-mul asm-smull asm-cmp/shift asm-add asm-sub asm-rsb asm-logand asm-logor asm-logxor asm-bic
                     asm-pop-multiple asm-shiftop asm-logand asm-lognot
                     asm-logtest asm-fp-relop asm-relop asm-push-multiple asm-vpush-multiple asm-vpop-multiple
                     asm-indirect-jump asm-literal-jump
                     asm-direct-jump asm-return-address asm-jump asm-conditional-jump
                     asm-indirect-call asm-condition-code
                     asm-fpmove-single asm-fl-cvt asm-fpt asm-fpmove asm-fpcastto asm-fpcastfrom
                     asm-fptrunc asm-fpsingle
                     asm-lock asm-lock+/- asm-cas asm-fence
                     asm-fpop-2 asm-fpsqrt asm-c-simple-call
                     asm-save-flrv asm-restore-flrv asm-return asm-c-return asm-size
                     asm-enter asm-foreign-call asm-foreign-callable
                     asm-read-counter
                     asm-inc-cc-counter
                     funky12
                     shift-count? unsigned8? unsigned12?
                     ; threaded version specific
                     asm-get-tc
                     asm-activate-thread asm-deactivate-thread asm-unactivate-thread
                     ; machine dependent exports
                     asm-kill
                     info-vpush-reg info-vpush-n)

  (define-record-type info-vpush (nongenerative)
    (parent info)
    (sealed #t)
    (fields reg n))

  (define ax-register?
    (case-lambda
      [(x) (record-case x [(reg) r #t] [else #f])]
      [(x reg) (record-case x [(reg) r (eq? r reg)] [else #f])]))

  (define-who ax-ea-reg-code
    (lambda (ea)
      (record-case ea
        [(reg) r (reg-mdinfo r)]
        [else (sorry! who "ea=~s" ea)])))

  (define ax-register-list
    (lambda (r*)
      (fold-left
        (lambda (a r) (fx+ a (fxsll 1 (reg-mdinfo r))))
        0 r*)))

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

  (define-op movi1  movi-a1-op  #b00111010)
  (define-op mvni   movi-a1-op  #b00111110)
  (define-op movi2  movi-a2-op  #b00110000) ; ARMv6T, ARMv7
  (define-op movt   movi-a2-op  #b00110100) ; ARMv6T, ARMv7

  (define-op addi  binary-imm-op  #b0010100)
  (define-op addci binary-imm-op  #b0010101)
  (define-op subi  binary-imm-op  #b0010010)
  (define-op rsbi  binary-imm-op  #b0010011)
  (define-op andi  binary-imm-op  #b0010000)
  (define-op orri  binary-imm-op  #b0011100)
  (define-op eori  binary-imm-op  #b0010001)
  (define-op bici  binary-imm-op  #b0011110)

  (define-op add   binary-op      #b0000100)
  (define-op sub   binary-op      #b0000010)
  (define-op rsb   binary-op      #b0000011)
  (define-op and   binary-op      #b0000000)
  (define-op orr   binary-op      #b0001100)
  (define-op eor   binary-op      #b0000001)
  (define-op bic   binary-op      #b0001110)

  (define-op cmp         cmp-op         #b0001010)
  (define-op tst         cmp-op         #b0001000)
  (define-op cmp/shift   cmp-op         #b0001010)
  (define-op cmpi  cmp-imm-op     #b0011010)
  (define-op tsti  cmp-imm-op     #b0011000)

  (define-op mov unary-op        #b0001101 #f) ; note: for mov, bits 5-11 must be zero, corresponding to 00 shift type and 00000 shift count
  (define-op mvn unary-op        #b0001111 #f)

  (define-op shifti shifti-op)
  (define-op shift shift-op)

  (define-op sxtb extend-op      #b01101010)
  (define-op sxth extend-op      #b01101011)
  (define-op uxtb extend-op      #b01101110)
  (define-op uxth extend-op      #b01101111)

  (define-op mul  mul-op         #b0000000)
  (define-op smull  mull-op      #b0000110)

  (define-op ldri    load-imm-op #b1 #b0 #b010 #b0 #b1)
  (define-op ldrbi   load-imm-op #b1 #b0 #b010 #b1 #b1)
  (define-op stri    load-imm-op #b1 #b0 #b010 #b0 #b0)
  (define-op strbi   load-imm-op #b1 #b0 #b010 #b1 #b0)

  (define-op str/preidx load-imm-op #b1 #b1 #b010 #b0 #b0)
  (define-op ldr/postidx load-imm-op #b0 #b0 #b010 #b0 #b1)

  (define-op ldrlit  load-lit-op)

  (define-op ldrshi  load-noshift-imm-op #b1 #b1111)
  (define-op ldrhi   load-noshift-imm-op #b1 #b1011)
  (define-op ldrdi   load-noshift-imm-op #b0 #b1101)
  (define-op ldrsbi  load-noshift-imm-op #b1 #b1101)
  (define-op strhi   load-noshift-imm-op #b0 #b1011)
  (define-op strdi   load-noshift-imm-op #b0 #b1111)

  (define-op ldr     load-op     #b011 #b0 #b1)
  (define-op ldrb    load-op     #b011 #b1 #b1)
  (define-op str     load-op     #b011 #b0 #b0)
  (define-op strb    load-op     #b011 #b1 #b0)

  (define-op ldrsh   load-noshift-op #b0 #b1 #b1111)
  (define-op ldrh    load-noshift-op #b0 #b1 #b1011)
  (define-op ldrd    load-noshift-op #b0 #b0 #b1101)
  (define-op ldrsb   load-noshift-op #b0 #b1 #b1101)
  (define-op strh    load-noshift-op #b0 #b0 #b1011)
  (define-op strd    load-noshift-op #b0 #b0 #b1111)

  (define-op ldrex ldrex-op      #b00011001)
  (define-op strex strex-op      #b00011000)

  (define-op dmbst    dmb-op #b1110)
  (define-op dmbish   dmb-op #b1011)
  (define-op dmbishst dmb-op #b1010)

  (define-op bnei  branch-imm-op       (ax-cond 'ne))
  (define-op brai  branch-imm-op       (ax-cond 'al))

  (define-op bx    branch-reg-op       (ax-cond 'al) #b0001)
  (define-op blx   branch-reg-op       (ax-cond 'al) #b0011)

  (define-op bra   branch-label-op     (ax-cond 'al))
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

  (define-op popm  pm-op #b10001011) 
  (define-op pushm pm-op #b10010010)
  (define-op vpushm vpm-op #b11010 #b10)
  (define-op vpopm  vpm-op #b11001 #b11)

  (define-op vldr.sgl vldr/vstr-op #b1010 #b01)
  (define-op vldr.dbl vldr/vstr-op #b1011 #b01)
  (define-op vstr.sgl vldr/vstr-op #b1010 #b00)
  (define-op vstr.dbl vldr/vstr-op #b1011 #b00)

  (define-op vmov.gpr->s32 vmov.gpr-op #b0)
  (define-op vmov.s32->gpr vmov.gpr-op #b1)
  (define-op vmov.gprgpr->s64 vmov.gpr64-op #b0)
  (define-op vmov.fpr vmov.fpr-op)

  (define-op vcvt.sgl->dbl vcvt-op #b01 #b110111)
  (define-op vcvt.dbl->sgl vcvt-op #b11 #b110111)
  (define-op vcvt.s32->dbl vcvt-op #b11 #b111000)
  (define-op vcvt.dbl->s32 vcvt-op #b11 #b111101)

  (define-op vcmp vcmp-op)
  (define-op fpscr->apsr fpscr->apsr-op)

  (define-op rev   rev-op #b01101011 #b0011)
  (define-op rev16 rev-op #b01101011 #b1011)
  (define-op revsh rev-op #b01101111 #b1011)

  (define-op mrs mrs-op)
  (define-op msr msr-op)

  (define-op mcr mrc/mcr-op #b0)
  (define-op mrc mrc/mcr-op #b1)

  (define-op vadd vadd-op #b11 #b0 #b11100)

  (define-op vsub vadd-op #b11 #b1 #b11100)
  (define-op vmul vadd-op #b10 #b0 #b11100)
  (define-op vdiv vadd-op #b00 #b0 #b11101)

  (define-op vsqrt vsqrt-op)

  (define-who movi-a1-op
    (lambda (op opcode dest-ea f12 code*)
      (emit-code (op dest-ea f12 code*) ; encoding A1
        [28 (ax-cond 'al)]
        [20 opcode]
        [16 #b0000]
        [12 (ax-ea-reg-code dest-ea)]
        [0  f12])))

  (define-who movi-a2-op ; ARMv6T, ARMv7
    (lambda (op opcode dest-ea u16 code*)
      (emit-code (op dest-ea u16 code*) ; movi encoding A2
        [28 (ax-cond 'al)]
        [20 opcode]
        [16 (fxsrl u16 12)]
        [12 (ax-ea-reg-code dest-ea)]
        [0  (fxlogand u16 #xfff)])))

  (define shift-op
    (lambda (op dest-ea src0-ea src1-ea shift-type code*)
      (emit-code (shift-type dest-ea src0-ea src1-ea code*)
        [28 (ax-cond 'al)]
        [21 #b0001101]
        [20 #b0] 
        [16 #b0000]
        [12 (ax-ea-reg-code dest-ea)]
        [8  (ax-ea-reg-code src1-ea)]
        [7  #b0]
        [5  (ax-shift-type shift-type)]
        [4  #b1]
        [0  (ax-ea-reg-code src0-ea)])))

  (define shifti-op
    (lambda (op dest-ea src0-ea n shift-type code*)
      (emit-code (shift-type dest-ea src0-ea n code*)
        [28 (ax-cond 'al)]
        [21 #b0001101]
        [20 #b0] 
        [16 #b0000]
        [12 (ax-ea-reg-code dest-ea)]
        [7  n]
        [5  (ax-shift-type shift-type)]
        [4  #b0]
        [0  (ax-ea-reg-code src0-ea)])))

  (define pm-op
    (lambda (op opcode regs code*)
      (emit-code (op regs code*)
        [28 (ax-cond 'al)]
        [20 opcode]
        [16 #b1101]
        [0  (ax-register-list regs)])))

  (define binary-imm-op ; 12-bit immediate
    (lambda (op opcode set-cc? dest-ea opnd-ea n code*)
      (emit-code (op set-cc? dest-ea opnd-ea n code*)
        [28 (ax-cond 'al)]
        [21 opcode]
        [20 (if set-cc? #b1 #b0)] 
        [16 (ax-ea-reg-code opnd-ea)]
        [12 (ax-ea-reg-code dest-ea)]
        [0  (funky12 n)])))

  (define binary-op
    (lambda (op opcode set-cc? dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op set-cc? dest-ea opnd0-ea opnd1-ea code*)
        [28 (ax-cond 'al)]
        [21 opcode]
        [20 (if set-cc? #b1 #b0)] 
        [16 (ax-ea-reg-code opnd0-ea)]
        [12 (ax-ea-reg-code dest-ea)]
        [7  #b00000] ; shift value
        [5  #b00]    ; shift type
        [4  #b0]
        [0  (ax-ea-reg-code opnd1-ea)])))

  (define mull-op
    (lambda (op opcode destlo-ea desthi-ea opnd0-ea opnd1-ea code*)
      (emit-code (op destlo-ea desthi-ea opnd0-ea opnd1-ea code*)
        [28 (ax-cond 'al)]
        [21 opcode]
        [20 #b0]  ; don't need no stinking z & n bits
        [16 (ax-ea-reg-code desthi-ea)]
        [12 (ax-ea-reg-code destlo-ea)]
        [8  (ax-ea-reg-code opnd1-ea)]
        [4  #b1001]
        [0  (ax-ea-reg-code opnd0-ea)])))

  (define unary-op
    (lambda (op opcode set-cc? dest-ea opnd-ea code*)
      (emit-code (op set-cc? dest-ea opnd-ea code*)
        [28 (ax-cond 'al)]
        [21 opcode]
        [20 (if set-cc? #b1 #b0)] 
        [16 #b0000]
        [12 (ax-ea-reg-code dest-ea)]
        [7  #b00000] ; shift value
        [5  #b00]    ; shift type
        [4  #b0]
        [0  (ax-ea-reg-code opnd-ea)])))

  (define cmp-op
    (case-lambda
      [(op opcode opnd0-ea opnd1-ea code*)
       (emit-code (op opnd0-ea opnd1-ea code*)
         [28 (ax-cond 'al)]
         [21 opcode]
         [20 #b1]
         [16 (ax-ea-reg-code opnd0-ea)]
         [12 #b0000]
         [7  #b00000] ; shift value
         [5  (ax-shift-type 'sll)]
         [4  #b0]
         [0  (ax-ea-reg-code opnd1-ea)])]
      [(op opcode shift-count shift-type opnd0-ea opnd1-ea code*)
       (emit-code (op opnd0-ea shift-type opnd1-ea shift-count code*)
         [28 (ax-cond 'al)]
         [21 opcode]
         [20 #b1]
         [16 (ax-ea-reg-code opnd0-ea)]
         [12 #b0000]
         [7  shift-count] ; shift value
         [5  (ax-shift-type shift-type)]
         [4  #b0]
         [0  (ax-ea-reg-code opnd1-ea)])]))

  (define cmp-imm-op
    (lambda (op opcode opnd-ea n code*)
      (emit-code (op opnd-ea n code*)
        [28 (ax-cond 'al)]
        [21 opcode]
        [20 #b1] 
        [16 (ax-ea-reg-code opnd-ea)]
        [12 #b0000]
        [0  (funky12 n)])))

  (define extend-op
    (lambda (op opcode dest-ea opnd-ea code*)
      (emit-code (op dest-ea opnd-ea code*)
        [28 (ax-cond 'al)]
        [20 opcode]
        [16 #b1111] 
        [12 (ax-ea-reg-code dest-ea)]
        [10 #b00] ; ROR value (0, 8, 16, 24)
        [4  #b000111]
        [0  (ax-ea-reg-code opnd-ea)])))

  (define mul-op
    (lambda (op opcode set-cc? dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op set-cc? dest-ea opnd0-ea opnd1-ea code*)
        [28 (ax-cond 'al)]
        [21 opcode]
        [20 (if set-cc? #b1 #b0)] 
        [16 (ax-ea-reg-code dest-ea)]
        [12 #b0000]
        [8  (ax-ea-reg-code opnd1-ea)]
        [4  #b1001]
        [0 (ax-ea-reg-code opnd0-ea)])))

  (define ldrex-op
    (lambda (op opcode dest-ea opnd-ea code*)
      (emit-code (op dest-ea opnd-ea code*)
        [28 (ax-cond 'al)]
        [20 opcode]
        [16 (ax-ea-reg-code opnd-ea)]
        [12 (ax-ea-reg-code dest-ea)]
        [8  #b1111]
        [4  #b1001]
        [0  #b1111])))

  (define strex-op
    (lambda (op opcode dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op dest-ea opnd0-ea opnd1-ea code*)
        [28 (ax-cond 'al)]
        [20 opcode]
        [16 (ax-ea-reg-code opnd1-ea)]
        [12 (ax-ea-reg-code dest-ea)]
        [8  #b1111]
        [4  #b1001]
        [0  (ax-ea-reg-code opnd0-ea)])))

  (define dmb-op
    (lambda (op opcode code*)
      (emit-code (op code*)
        [4 #b1111010101111111111100000101]
        [0 opcode])))

  (define branch-imm-op
    (lambda (op cond-bits disp code*)
      (emit-code (op disp code*)
        [28 cond-bits]
        [24 #b1010]
        [0  (fxlogand disp #xffffff)])))

  (define-who branch-label-op
    (lambda (op cond-bits dest code*)
      (record-case dest
        [(label) (offset l)
         (emit-code (op dest code*)
           [28 cond-bits]
           [24 #b1010]
           [0  (fxlogand (fxsrl (fx- offset 4) 2) #xffffff)])]
        [else (sorry! who "unexpected dest ~s" dest)])))

  (define-who branch-reg-op
    (lambda (op condition-code opcode dest code*)
      (emit-code (op dest code*)
        [28 condition-code]
        [20 #b00010010]
        [8  #b111111111111]
        [4  opcode]
        [0  (ax-ea-reg-code dest)])))

  (define mrs-op
    (lambda (op dest code*)
      (emit-code (op dest code*)
        [28 (ax-cond 'al)]
        [16 #b000100001111]
        [12 (ax-ea-reg-code dest)]
        [0  #b000000000000])))

  (define msr-op
    (lambda (op mask src code*)
      (emit-code (op mask src code*)
        [28 (ax-cond 'al)]
        [20 #b00010010]
        [18 mask]
        [4  #b00111100000000]
        [0 (ax-ea-reg-code src)])))

  (define-who mrc/mcr-op
    (lambda (op dir cond coproc opc1 dest-ea CRn CRm opc2 code*)
      (emit-code (op cond coproc opc1 dest-ea CRn CRm opc2 code*) ; encoding A1
        [28 (ax-cond cond)]
        [24 #b1110]
        [21 opc1]
        [20 dir]
        [16 CRn]
        [12 (ax-ea-reg-code dest-ea)]
        [8 coproc]
        [5 opc2]
        [4 1]
        [0 CRm])))

  (define vldr/vstr-op
    (lambda (op opc1 opc2 flreg reg offset code*)
      (safe-assert (and (<= 0 offset #x3FF)
                        (fx= 0 (fxand offset #b11))))
      (let-values ([(d vd) (ax-flreg->bits flreg)])
        (emit-code (op flreg reg offset code*)
          [28 (ax-cond 'al)]
          [24 #b1101]
          ; NB: what's the source of the following comment?
          [23 #b1] ; U bit for adding or subtracting offset. using SP requires offset #-0
          [22 d]
          [20 opc2]
          [16 (ax-ea-reg-code reg)]
          [12 vd]
          [8  opc1]
          [0  (fxsrl offset 2)]))))

  (define vmov.gpr-op
    (lambda (op dir flreg flreg-delta gpreg code*)
      (let-values ([(n vn) (ax-flreg->bits flreg flreg-delta)])
        (emit-code (op flreg gpreg flreg-delta code*)
          [28 (ax-cond 'al)]
          [21 #b1110000]
          [20 dir]
          [16 vn]
          [12 (ax-ea-reg-code gpreg)]
          [8  #b1010]
          [7  n]
          [0  #b0010000]))))

  (define vmov.gpr64-op
    (lambda (op dir flreg gpreglo gpreghi code*)
      (let-values ([(n vn) (ax-flreg->bits flreg)])
        (emit-code (op flreg gpreglo gpreghi code*)
          [28 (ax-cond 'al)]
          [23 #b11000]
          [22 1]
          [21 0]
          [20 dir] ; 0 to fp, 1 from fp
          [16 (ax-ea-reg-code gpreghi)]
          [12 (ax-ea-reg-code gpreglo)]
          [10 #b10]
          [8 #b11]
          [6 #b00]
          [5 n]
          [4 1]
          [0 vn]))))

  (define vmov.fpr-op
    (lambda (op destreg srcreg code*)
      (let-values ([(d vd) (ax-flreg->bits destreg)]
                   [(m vm) (ax-flreg->bits srcreg)])
        (emit-code (op destreg srcreg code*)
          [28 (ax-cond 'al)]
          [23 #b11101]
          [22 0] ; D
          [20 #b11]
          [19 d]
          [16 #b000]
          [12 vd]
          [10 #b10]
          [8 #b11]
          [7 0]
          [6 1]
          [5 m]
          [4 0]
          [00 vm]))))

  (define vcvt-op
    (lambda (op szop opc2 dest src code*)
      (let-values ([(d vd) (ax-flreg->bits dest)]
                   [(m vm) (ax-flreg->bits src)])
        (emit-code (op dest src code*)
          [28 (ax-cond 'al)]
          [23 #b11101]
          [22 d]
          [16 opc2]
          [12 vd]
          [9  #b101]
          [7  szop]
          [6  #b1]
          [5  m]
          [4  #b0]
          [0  vm]))))

  (define vcmp-op
    (lambda (op src1 src2 code*)
      (let-values ([(d vd) (ax-flreg->bits src1)]
                   [(m vm) (ax-flreg->bits src2)])
        (emit-code (op src1 src2 code*)
          [28 (ax-cond 'al)]
          [23 #b11101]
          [22 d]
          [16 #b110100]
          [12 vd]
          [9  #b101]
          [6  #b101]
          [5  m]
          [4  #b0]
          [0  vm]))))

  (define fpscr->apsr-op
    (lambda (op code*)
      (emit-code (op code*)
        [28 (ax-cond 'al)]
        [16 #b111011110001]
        [12 #b1111]
        [0  #b101000010000])))

  (define vpm-op
    (lambda (op opcode opcode2 flreg n code*)
      (let-values ([(d vd) (ax-flreg->bits flreg)])
        (emit-code (op flreg n code*)
          [28 (ax-cond 'al)]
          [23 opcode]
          [22 d]
          [20 opcode2]
          [16 #b1101]
          [12 vd]
          [8  #b1011]
          [0  (fxsll n 1)]))))

  (define rev-op
    (lambda (op opcode1 opcode2 dest-ea src-ea code*)
      (emit-code (op dest-ea src-ea code*)
        [28 (ax-cond 'al)]
        [20 opcode1]
        [16 #b1111]
        [12 (ax-ea-reg-code dest-ea)]
        [8  #b1111]
        [4  opcode2]
        [0  (ax-ea-reg-code src-ea)])))

  (define load-op
    (lambda (op opcode1 opcode2 opcode3 dest-ea base-ea index-ea code*)
      (emit-code (op dest-ea base-ea index-ea code*)
        [28 (ax-cond 'al)]
        [25 opcode1]
        [24 #b1] ; P "Pay attention to index register"
        [23 #b1] ; U "Upward (add index)"
        [22 opcode2]
        [21 #b0] ; W "Write back" (post-increment/decrement)
        [20 opcode3]
        [16 (ax-ea-reg-code base-ea)]
        [12 (ax-ea-reg-code dest-ea)]
        [7  #b00000] ; shift amount
        [5  #b00]    ; shift type
        [4  #b0]
        [0  (ax-ea-reg-code index-ea)])))

  (define load-noshift-op
    (lambda (op opcode1 opcode2 opcode3 dest-ea base-ea index-ea code*)
      (emit-code (op dest-ea base-ea index-ea code*)
        [28 (ax-cond 'al)]
        [25 #b000]
        [24 #b1] ; P "Pay attention to index register"
        [23 #b1] ; U "Upward (add index)"
        [22 opcode1]
        [21 #b0] ; W "Write back" (post-increment/decrement)
        [20 opcode2]
        [16 (ax-ea-reg-code base-ea)]
        [12 (ax-ea-reg-code dest-ea)]
        [8  #b0000]
        [4  opcode3]
        [0  (ax-ea-reg-code index-ea)])))

  (define load-imm-op
    (lambda (op P W opcode1 opcode2 opcode3 dest-ea base-ea orig-n code*)
      (let-values ([(U n) (if (fx< orig-n 0) (values 0 (fx- orig-n)) (values 1 orig-n))])
        (emit-code (op dest-ea base-ea orig-n code*)
          [28 (ax-cond 'al)]
          [25 opcode1]
          [24 P] ; P "Pay attention to index register"
          [23 U]   ; U "Upward (add index)"
          [22 opcode2]
          [21 W] ; W "Write back" (post-increment/decrement)
          [20 opcode3]
          [16 (ax-ea-reg-code base-ea)]
          [12 (ax-ea-reg-code dest-ea)]
          [0  n]))))

  (define load-noshift-imm-op
    (lambda (op opcode1 opcode2 dest-ea base-ea orig-n code*)
      (let-values ([(U n) (if (fx< orig-n 0) (values 0 (fx- orig-n)) (values 1 orig-n))])
        (emit-code (op dest-ea orig-n code*)
          [28 (ax-cond 'al)]
          [25 #b000]
          [24 #b1] ; P "Pay attention to index register"
          [23 U]   ; U "Upward (add index)"
          [22 #b1]
          [21 #b0] ; W "Write back" (post-increment/decrement)
          [20 opcode1]
          [16 (ax-ea-reg-code base-ea)]
          [12 (ax-ea-reg-code dest-ea)]
          [8  (fxsrl n 4)]
          [4  opcode2]
          [0  (fxlogand n #xf)]))))

  (define load-lit-op
    (lambda (op dest-ea orig-disp code*)
      (let-values ([(U disp) (if (fx< orig-disp 0) (values 0 (fx- orig-disp)) (values 1 orig-disp))])
        (emit-code (op dest-ea orig-disp code*)
          [28 (ax-cond 'al)]
          [25 #b010]
          [24 #b1]
          [23 U]   ; U "Upward (add index)"
          [22 #b0]
          [21 #b0]
          [20 #b1]
          [16 #b1111]
          [12 (ax-ea-reg-code dest-ea)]
          [0  disp]))))

  (define vadd-op
    (lambda (op opcode1 opcode2 opcode3 dest opnd1 opnd2 code*)
      (let-values ([(d vd) (ax-flreg->bits dest)]
                   [(n vn) (ax-flreg->bits opnd1)]
                   [(m vm) (ax-flreg->bits opnd2)])
        (emit-code (op dest opnd1 opnd2 code*)
          [28 (ax-cond 'al)]
          [23 opcode3]
          [22 d]
          [20 opcode1]
          [16 vn]
          [12 vd]
          [9  #b101]
          [8  #b1]   ;; sz = 1 for double
          [7  n]
          [6  opcode2]
          [5  m]
          [4  #b0]
          [0  vm]))))

  (define vsqrt-op
    (lambda (op dest src code*)
      (let-values ([(d vd) (ax-flreg->bits dest)]
                   [(m vm) (ax-flreg->bits src)])
        (emit-code (op dest src code*)
          [28 (ax-cond 'al)]
          [23 #b11101]
          [22 d]
          [20 #b11]
          [16 #b0001]
          [12 vd]
          [9  #b101]
          [8  #b1]   ;; sz = 1 for double
          [7  #b1]
          [6  #b1]
          [5  m]
          [4  #b0]
          [0  vm]))))

  ; asm helpers

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

  (define-who ax-shift-type
    (lambda (op)
      (case op
        [(sll) #b00]
        [(srl) #b01]
        [(sra) #b10]
        [(ror) #b11]
        [else ($oops who "unsupported op ~s" op)])))

  (define ax-flreg->bits
    (case-lambda
     [(flreg) (ax-flreg->bits flreg 0)]
     [(flreg flreg-delta)
      (let ([n (fx+ (reg-mdinfo flreg) flreg-delta)])
        (if (fx< n 32)
            (values (fxlogand n 1) (fxsrl n 1))
            (values (fxsrl n 4) (fxlogand n #b1111))))]))

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
         (build-maybe-cons* #'((build long (byte-fields chunk ...)))
           #'(aop-cons* `(asm ,op ,opnd ...) ?code*))])))

  #;(define-syntax emit-code
    (lambda (x)
      (syntax-case x ()
        [(_ (op opnd ... ?code*) chunk ...)
         (fold-right cons #'(aop-cons* `(asm ,op ,opnd ...) ?code*)
           #'((build long (byte-fields chunk ...))))])))

  (define-syntax build
    (syntax-rules ()
      [(_ x e)
       (and (memq (datum x) '(byte word long)) (integer? (datum e)))
       (quote (x . e))]
      [(_ x e)
       (memq (datum x) '(byte word long))
       (cons 'x e)]))

  (define-syntax byte-fields
    ; NB: make more efficient for fixnums
    (syntax-rules ()
      [(byte-fields (n e) ...)
       (andmap fixnum? (datum (n ...)))
       (+ (bitwise-arithmetic-shift-left e n) ...)]))

  (define ax-byte-size?
    (lambda (n)
      (<= -128 n 127)))

  (define ax-range?
    (lambda (low x high)
      (record-case x
        [(imm) (n) (<= low n high)]
        [else #f])))

  (define ax-ea-branch-disp
    (lambda (dest-ea)
      (record-case dest-ea
        [(literal) stuff (cons 'rel stuff)]
        [else ($oops 'assembler-internal
                "ax-ea-branch-disp dest-ea=~s" dest-ea)])))

  (define funky12
    (lambda (n)
      (define (try limit n)
        (let* ([k (fxlogand (bitwise-first-bit-set n) (fxlognot 1))]
               [n (bitwise-arithmetic-shift-right n k)])
          (and (and (fixnum? n) (#%$fxu< n #x100))
               (fxlogor (fxsll (fx- limit k) 7) n))))
      (if (and (fixnum? n) (#%$fxu< n #x100))
          n
          (and (<= (- (expt 2 31)) n (- (expt 2 32) 1)) ; bounds check lets caller be sloppy about, e.g., NOT-ing or negating input
               (let ([n (if (< n 0) (+ (expt 2 32) n) n)])
                 (or (try 32 n)
                     (try 8
                       (logor
                         (bitwise-arithmetic-shift-left (logand n #xffffff) 8)
                         (bitwise-arithmetic-shift-right n 24)))))))))

  ;; restrict funky12 so that an code offset n will not fit
  ;; if a smaller offset wouldn't fit, which prevents bouncing
  ;; in the loop that computes label offsets
  (define code-offset-funky12
    (lambda (n)
      (safe-assert (and (fixnum? n) (fx= 0 (fxand n 3))))
      (and (fixnum? n)
           (#%$fxu< n #x400)
           (funky12 n))))

  (define shift-count?
    (lambda (imm)
      ; can also allow 0 for lsl and 32 (represented as 0) for lsr, asr
      ; but all three agree on the meaning of [1..31]
      (and (fixnum? imm) (fx<= 1 imm 31))))

  (define unsigned8?
    (lambda (imm)
      (and (fixnum? imm) ($fxu< imm (expt 2 8)))))

  (define unsigned12?
    (lambda (imm)
      (and (fixnum? imm) ($fxu< imm (expt 2 12)))))

  (define branch-disp?
    (lambda (x)
      (and (fixnum? x)
           ; -4 accounts for fact that pc reads as the instruction after next, not next
           (fx<= (- (expt 2 25)) (fx- x 4) (- (expt 2 25) 1))
           (not (fxlogtest x #b11)))))

  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm arm32-abs arm32-jump arm32-call) 0]
        [else 4])))

  (define ax-mov32
    (lambda (dest n code*)
      ; NB: ARMv6T, ARMv7 only
      #;(emit movi dest (logand n #xffff)
          (emit movt dest (fxlogand (bitwise-arithmetic-shift-right n 16) #xffff) code*))
      ; instead place n at pc+8, load from there, and branch around
      (emit ldrlit dest 0
        (emit brai 0
          (cons* `(long . ,n) (aop-cons* `(asm "long:" ,n) code*))))))

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
             [(reg) ignore (emit mov dest src code*)]
             [(imm) (n)
              (cond
                [(funky12 n) =>
                 (lambda (f12)
                   (emit movi1 dest f12 code*))]
                [(funky12 (lognot n)) =>
                 (lambda (f12)
                   (emit mvni dest f12 code*))]
                [else (ax-mov32 dest n code*)])]
             [(literal) stuff
              (ax-mov32 dest 0
                (asm-helper-relocation code* (cons 'arm32-abs stuff)))]
             [(disp) (n breg)
              (safe-assert (or (unsigned12? n) (unsigned12? (- n))))
              (emit ldri dest `(reg . ,breg) n code*)]
             [(index) (n ireg breg)
              (safe-assert (eqv? n 0))
              (emit ldr dest `(reg . ,breg) `(reg . ,ireg) code*)]
             [else (bad!)])]
          [(ax-reg? src)
           (record-case dest
             [(disp) (n breg)
              (safe-assert (or (unsigned12? n) (unsigned12? (- n))))
              (emit stri src `(reg . ,breg) n code*)]
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
            [(zext8) (emit uxtb dest src code*)]
            [(zext16) (emit uxth dest src code*)]
            [else (sorry! who "unexpected op ~s" op)])))))

  (module (asm-add asm-sub asm-rsb asm-logand asm-logor asm-logxor asm-bic)
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
    (define asm-rsb (asm-binop rsbi rsb))
    (define asm-logand (asm-binop andi and))
    (define asm-logor (asm-binop orri orr))
    (define asm-logxor (asm-binop eori eor))
    (define asm-bic (asm-binop bici bic)))

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit mul #f dest src0 src1 code*))))

  (define asm-smull
    (lambda (code* dest src0 src1 tmp)
      (Trivit (dest src0 src1 tmp)
        (emit smull dest tmp src0 src1 code*))))

  (define asm-cmp/shift
    (lambda (count type)
      (lambda (code* src0 src1)
        (Trivit (src0 src1)
          (emit cmp/shift count type src0 src1 code*)))))

  (define-who asm-fl-cvt
    (lambda (op)
      (lambda (code* dest-reg src-reg)
        (case op
          [(single->double)
           (emit vcvt.sgl->dbl dest-reg src-reg code*)]
          [(double->single)
           (emit vcvt.dbl->sgl dest-reg src-reg code*)]
          [else (sorry! who "unrecognized op ~s" op)]))))

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
                    [(integer-32 unsigned-32) (emit ldri dest base n code*)]
                    [(integer-16) (emit ldrshi dest base n code*)]
                    [(unsigned-16) (emit ldrhi dest base n code*)]
                    [(integer-8) (emit ldrsbi dest base n code*)]
                    [(unsigned-8) (emit ldrbi dest base n code*)]
                    [else (sorry! who "unexpected mref type ~s" type)])]
                [(eqv? n 0)
                  (Trivit (index)
                    (case type
                      [(integer-32 unsigned-32) (emit ldr dest base index code*)]
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
            (Trivit (src base)
              (cond
                [(eq? index %zero)
                  (case type
                    [(integer-32 unsigned-32) (emit stri src base n code*)]
                    [(integer-16 unsigned-16) (emit strhi src base n code*)]
                    [(integer-8 unsigned-8) (emit strbi src base n code*)]
                    [else (sorry! who "unexpected mref type ~s" type)])]
                [(eqv? n 0)
                  (Trivit (index)
                    (case type
                      [(integer-32 unsigned-32) (emit str src base index code*)]
                      [(integer-16 unsigned-16) (emit strh src base index code*)]
                      [(integer-8 unsigned-8) (emit strb src base index code*)]
                      [else (sorry! who "unexpected mref type ~s" type)]))]
                [else (sorry! who "expected %zero index or 0 offset, got ~s and ~s" index offset)])))))))

  (define-who asm-fpop-2
    (lambda (op)
      (lambda (code* dest src1 src2)
        (case op
          [(fp+) (emit vadd dest src1 src2 code*)]
          [(fp-) (emit vsub dest src1 src2 code*)]
          [(fp*) (emit vmul dest src1 src2 code*)]
          [(fp/) (emit vdiv dest src1 src2 code*)]
          [else (sorry! who "unrecognized op ~s" op)]))))

  (define asm-fpsqrt
    (lambda (code* dest src)
      (emit vsqrt dest src code*)))

  (define asm-fpsingle
    (lambda (code* dest-reg src-reg)
      (emit vcvt.dbl->sgl dest-reg src-reg 
        (emit vcvt.sgl->dbl dest-reg dest-reg code*))))

  (define asm-fptrunc
    (lambda (code* dest flonumreg tmpreg)
      (Trivit (dest)
        (emit vcvt.dbl->s32 tmpreg flonumreg
            (emit vmov.s32->gpr tmpreg 0 dest code*)))))

  (define asm-fpt
    (lambda (code* dest src tmpreg)
      (Trivit (src)
        (emit vmov.gpr->s32 tmpreg 0 src
          (emit vcvt.s32->dbl dest tmpreg code*))))) 

  (define-who asm-fpmove
    ;; fpmove pseudo instruction is used by set! case in
    ;; select-instructions! and generate-code; at most one of src or
    ;; dest can be an mref, and then the offset is double-aligned
    (lambda (code* dest src)
      (gen-fpmove who code* dest src #t)))

  (define-who asm-fpmove-single
    ;; fpmove pseudo instruction is used by set! case in
    ;; select-instructions! and generate-code; at most one of src or
    ;; dest can be an mref, and then the offset is double-aligned
    (lambda (code* dest src)
      (gen-fpmove who code* dest src #f)))

  (define gen-fpmove
    (lambda (who code* dest src double?)
      (let ([dest-it dest]
            [src-it src])
        (Trivit (dest-it src-it)
          (record-case dest-it
            [(disp) (imm reg)
             (safe-assert (fx= 0 (fxand imm #b11)))
             (if double?
                 (emit vstr.dbl src (cons 'reg reg) imm code*)
                 (emit vstr.sgl src (cons 'reg reg) imm code*))]
            [(index) (n ireg breg) (sorry! who "cannot handle indexed fp dest ref")]
            [else
             (record-case src-it
               [(disp) (imm reg)
                (safe-assert (fx= 0 (fxand imm #b11)))
                (if double?
                    (emit vldr.dbl dest (cons 'reg reg) imm code*)
                    (emit vldr.sgl dest (cons 'reg reg) imm code*))]
               [(index) (n ireg breg) (sorry! who "cannot handle indexed fp src ref")]
               [else (emit vmov.fpr dest src code*)])])))))

  (define asm-fpcastto
    (lambda (part)
      (lambda (code* dest src)
        (Trivit (dest)
          (if (eq? part 'lo)
              (emit vmov.s32->gpr src 0 dest code*)
              (emit vmov.s32->gpr src 1 dest code*))))))

  (define asm-fpcastfrom
    (lambda (code* dest lo-src hi-src)
      (Trivit (lo-src hi-src)
        (emit vmov.gprgpr->s64 dest lo-src hi-src code*))))

  (define-who asm-swap
    (lambda (type)
      (rec asm-swap-internal
        (lambda (code* dest src)
          (Trivit (dest src)
            (case type
              [(integer-16) (emit revsh dest src code*)]
              [(unsigned-16) (emit rev16 dest src code*)]
              [(integer-32 unsigned-32) (emit rev dest src code*)]
              [else (sorry! who "unexpected asm-swap type argument ~s" type)]))))))

  (define asm-lock
    ;  tmp = 1 # in case load result is not 0
    ;  tmp2 = ldrex src
    ;  cmp tmp2, 0
    ;  bne L1 (+2)
    ;  tmp2 = 1
    ;  tmp = strex tmp2, src
    ;L1:
    (lambda (code* src tmp tmp2)
      (Trivit (src tmp tmp2)
        (emit movi1 tmp 1
          (emit ldrex tmp2 src
            (emit cmpi tmp2 0
              (emit bnei 1
                (emit movi1 tmp2 1
                  (emit strex tmp tmp2 src code*)))))))))

  (define-who asm-lock+/-
    ; L:
    ;   tmp1 = ldrex src
    ;   tmp1 = tmp1 +/- 1
    ;   tmp2 = strex tmp1, src
    ;   cmp tmp2, 0
    ;   bne L (-6)
    ;   cmp tmp1, 0
    (lambda (op)
      (lambda (code* src tmp1 tmp2)
        (Trivit (src tmp1 tmp2)
          (emit ldrex tmp1 src
            (let ([code* (emit strex tmp2 tmp1 src
                           (emit cmpi tmp2 0
                             (emit bnei -6
                               (emit cmpi tmp1 0 code*))))])
              (case op
                [(locked-incr!) (emit addi #f tmp1 tmp1 1 code*)]
                [(locked-decr!) (emit subi #f tmp1 tmp1 1 code*)]
                [else (sorry! who "unexpected op ~s" op)])))))))

  (define-who asm-cas
    ;   tmp = ldrex src
    ;   cmp tmp, old
    ;   bne L (+2)
    ;   tmp2 = strex new, src
    ;   cmp tmp2, 0
    ; L:
    (lambda (code* src old new tmp1 tmp2)
      (Trivit (src old new tmp1 tmp2)
        (emit ldrex tmp1 src
          (emit cmp tmp1 old
            (emit bnei 1
              (emit strex tmp2 new src
                (emit cmpi tmp2 0
                   code*))))))))

  ;; Based in part on https://www.cl.cam.ac.uk/~pes20/cpp/cpp0xmappings.html
  (define-who asm-fence
    (lambda (kind)
      (lambda (code*)
        (constant-case arm-isa-version
          [(6)
           (emit mcr 'al #b1111 #b000 `(reg . ,%r0) #b0111 #b1010 #b101 code*)]
          [(7)
           (case kind
             [(store-store) (emit dmbishst code*)]
             [(acquire) (emit dmbish code*)]
             [(release) (emit dmbish code*)]
             [else (sorry! who "unexpected kind ~s" kind)])]))))

  (define asm-fp-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (values
          (emit vcmp x y (emit fpscr->apsr '()))
          (asm-conditional-jump info l1 l2 offset)))))

  (define-who asm-relop
    (lambda (info)
      (rec asm-relop-internal
        (lambda (l1 l2 offset x y)
          (Trivit (x y)
            (unless (ax-reg? x) (sorry! who "unexpected first operand ~s" x))
            (values
              (record-case y
                [(imm) (n) (emit cmpi x n '())]
                [(reg) ignore (emit cmp x y '())]
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
        (emit popm regs code*))))

  (define asm-push-multiple
    (lambda (regs)
      (lambda (code*)
        (emit pushm regs code*))))

  (define asm-vpush-multiple
    (lambda (reg n)
      (lambda (code*)
        (emit vpushm reg n code*))))

  (define asm-vpop-multiple
    (lambda (reg n)
      (lambda (code*)
        (emit vpopm reg n code*))))

  (define asm-save-flrv
    (lambda (code*)
      (let ([sp (cons 'reg %sp)])
        (emit subi #f sp sp 8
          (emit vstr.dbl %Cfpretval sp 0 code*)))))

  (define asm-restore-flrv
    (lambda (code*)
      (let ([sp (cons 'reg %sp)])
        (emit vldr.dbl %Cfpretval sp 0
          (emit addi #f sp sp 8 code*)))))

  (define asm-read-counter
    (case-lambda
      [(k)
       (lambda (code* dest)
         (Trivit (dest)
           (emit mrc 'al 15 0 dest 15 12 k code*)))]
      [()
       (lambda (code* dest src)
         (Trivit (dest src)
           (emit cmpi src 0
             (emit mrc 'eq 15 0 dest 15 12 2
               (emit mrc 'ne 15 0 dest 15 12 3 code*)))))]))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
        `(arm32-jump ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec save-ra?)
      (let ([target `(arm32-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* dest jmp-tmp . ignore) ; ignore arguments, which must be in fixed locations
            (asm-helper-call code* target save-ra? jmp-tmp))))))

  (define asm-library-call!
    (lambda (libspec save-ra?)
      (let ([target `(arm32-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* jmp-tmp . ignore) ; ignore arguments, which must be in fixed locations
            (asm-helper-call code* target save-ra? jmp-tmp))))))

  (define asm-c-simple-call
    (lambda (entry save-ra?)
      (let ([target `(arm32-call 0 (entry ,entry))])
        (rec asm-c-simple-call-internal
          (lambda (code* jmp-tmp . ignore)
            (asm-helper-call code* target save-ra? jmp-tmp))))))

  (define-who asm-indirect-call
    (lambda (code* dest lr . ignore)
      (safe-assert (eq? lr %lr))
      (Trivit (dest)
        (unless (ax-reg? dest) (sorry! who "unexpected dest ~s" dest))
        (emit blx dest code*))))

  (define asm-direct-jump
    (lambda (l offset)
      (let ([offset (adjust-return-point-offset offset l)])
        (asm-helper-jump '() (make-funcrel 'arm32-jump l offset)))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
        `(arm32-jump ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info))))))

  (define-who asm-indirect-jump
    (lambda (src)
      (Trivit (src)
        (record-case src
          [(reg) ignore (emit bx src '())]
          [(disp) (n breg)
           (safe-assert (or (unsigned12? n) (unsigned12? (- n))))
           (emit ldri `(reg . ,%pc) `(reg . ,breg) n '())]
          [(index) (n ireg breg)
           (safe-assert (eqv? n 0))
           (emit ldr `(reg . ,%pc) `(reg . ,breg) `(reg . ,ireg) '())]
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
    (let ([target `(arm32-call 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* dest jmp-tmp . ignore) ; dest is ignored, since it is always Cretval
        (asm-helper-call code* target #f jmp-tmp))))

  (define asm-activate-thread
    (let ([target `(arm32-call 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* dest jmp-tmp . ignore)
        (asm-helper-call code* target #t jmp-tmp))))

  (define asm-deactivate-thread
    (let ([target `(arm32-call 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code* jmp-tmp . ignore)
        (asm-helper-call code* target #f jmp-tmp))))

  (define asm-unactivate-thread
    (let ([target `(arm32-call 0 (entry ,(lookup-c-entry unactivate-thread)))])
      (lambda (code* arg-reg jmp-tmp . ignore)
        (asm-helper-call code* target #f jmp-tmp))))

  (define-who asm-return-address
    (lambda (dest l incr-offset next-addr)
      (make-rachunk dest l incr-offset next-addr
        (or (cond
              [(local-label-offset l) =>
               (lambda (offset)
                 (let ([incr-offset (adjust-return-point-offset incr-offset l)])
                   (let ([disp (fx- next-addr (fx- offset incr-offset) 4)])
                     (cond
                       [(code-offset-funky12 disp)
                        (Trivit (dest)
                          ; aka adr, encoding A1
                          (emit addi #f dest `(reg . ,%pc) disp '()))]
                       [(code-offset-funky12 (- disp))
                        (Trivit (dest)
                          ; aka adr, encoding A2
                          (emit subi #f dest `(reg . ,%pc) (- disp) '()))]
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
                 [(branch-disp? disp) (emit bra `(label ,disp ,l) '())]
                 ; will have to deal with this on architectures with smaller displacements.
                 ; problem is we'll need a temp reg, and we discover this way past register
                 ; allocation.  so possibly compute the max possible code-object size at
                 ; instruction selection time.  when max possible size exceeds branch range
                 ; (plus or minus), supply asm-jump and others like it an unspillable.  don't
                 ; want to supply an unspillable for smaller code objects since this
                 ; unnecessarily constrains the register allocator.
                 [else (sorry! who "no support for code objects > 32MB in length")])))]
          [else
            ; label must be somewhere above.  generate something so that a hard loop
            ; doesn't get dropped.  this also has some chance of being the right size
            ; for the final branch instruction.
            (emit bra `(label 0 ,l) '())]))))

  (define-who asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      (define get-disp-opnd
        (lambda (next-addr l)
          (if (local-label? l)
              (cond
                [(local-label-offset l) =>
                 (lambda (offset)
                   (let ([disp (fx- next-addr offset)])
                     (unless (branch-disp? disp) (sorry! who "no support for code objects > 32MB in length"))
                     (values disp `(label ,disp ,l))))]
                [else (values 0 `(label 0 ,l))])
              (sorry! who "unexpected label ~s" l))))
      (let ([type (info-condition-code-type info)]
            [reversed? (info-condition-code-reversed? info)])
        (make-cgchunk info l1 l2 next-addr
          (let ()
            (define-syntax pred-case
              (lambda (x)
                (define build-bop-seq
                  (lambda (bop opnd1 opnd2 l2 body)
                    #`(let ([code* (emit #,bop #,opnd1 code*)])
                        (let-values ([(ignore #,opnd2) (get-disp-opnd (fx+ next-addr (asm-size* code*)) #,l2)])
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
                           [(fx= disp1 0) #,(handle-reverse #'c1 #'opnd2 #'l2)]
                           [(fx= disp2 0) #,(handle-reverse #'c2 #'opnd1 #'l1)]
                           [else #,(build-bop-seq #'bra #'opnd2 #'opnd1 #'l1
                                     (handle-reverse #'c2 #'opnd1 #'l1))])]
                      [_ #`(cond
                             [(fx= disp1 0) #,(handle-reverse e #'opnd2 #'l2)]
                             [else #,(build-bop-seq #'bra #'opnd1 #'opnd2 #'l2
                                       (handle-reverse e #'opnd2 #'l2))])])))
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
              [(multiply-overflow) (i? beq bne)] ; result of comparing sign bit of low word with all bits in high word: eq if no overflow, ne if overflow
              [(positive) (i? ble bgt)]
              [(carry) (i? bcc bcs)]
              [(fp<) (i? (r? ble bcs) (r? bgt bcc))]
              [(fp<=) (i? (r? blt bhi) (r? bge bls))]
              [(fp=) (i? bne beq)]))))))

  (define asm-helper-jump
    (lambda (code* reloc)
      ; NB: kills %ts, unbeknownst to the instruction scheduler
      ; NB: jmp-tmp should be included in jump syntax, introduced by md-handle-jump, and passed in from code generator
      ; NB: probably works despite this since %ts is never live at the jmp point anyway
      (let ([jmp-tmp (cons 'reg %ts)])
        (ax-mov32 jmp-tmp 0
          (emit bx jmp-tmp
            (asm-helper-relocation code* reloc))))))

  (define asm-kill
    (lambda (code* dest)
      code*))

  (define ax-save/restore
    ; push/pop while maintaining 8-byte alignment
    (lambda (code* reg-ea p)
      (let ([sp (cons 'reg %sp)])
        (emit str/preidx reg-ea sp -8
          (p (emit ldr/postidx reg-ea sp 8 code*))))))

  (define asm-helper-call
    (lambda (code* reloc save-ra? jmp-tmp)
      ; NB: kills %lr
      (let ([jmp-tmp (cons 'reg jmp-tmp)])
        (define maybe-save-ra
          (lambda (code* p)
            (if save-ra?
                (ax-save/restore code* (cons 'reg %lr) p)
                (p code*))))
        (maybe-save-ra code*
          (lambda (code*)
            (ax-mov32 jmp-tmp 0
              (emit blx jmp-tmp
                (asm-helper-relocation code* reloc))))))))

  (define asm-helper-relocation
    (lambda (code* reloc)
      (cons* reloc (aop-cons* `(asm "relocation:" ,reloc) code*))))

  ; NB: reads from %lr...should be okay if declare-intrinsics sets up return-live* properly
  (define asm-return (lambda () (emit bx (cons 'reg %lr) '())))

  (define asm-c-return (lambda (info) (emit bx (cons 'reg %lr) '())))

  (define-who asm-shiftop
    (lambda (op)
      (lambda (code* dest src0 src1)
        (Trivit (dest src0 src1)
          (record-case src1
            [(imm) (n) (emit shifti dest src0 n op code*)]
            [else (emit shift dest src0 src1 op code*)])))))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit mvn dest src code*))))

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
          (emit bnei 2
            (do-ldr 4
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
    (define num-int-regs 4) ; number of integer registers normally usd by the ABI
    (define num-dbl-regs 8) ; number of `double` registers normally usd by the ABI
    (define sgl-regs (lambda () (list %Cfparg1 %Cfparg1b %Cfparg2 %Cfparg2b %Cfparg3 %Cfparg3b %Cfparg4 %Cfparg4b
                                      %Cfparg5 %Cfparg5b %Cfparg6 %Cfparg6b %Cfparg7 %Cfparg7b %Cfparg8 %Cfparg8b)))
    (define save-and-restore
      (lambda (regs e)
        (safe-assert (andmap reg? regs))
        (with-output-language (L13 Effect)
          (let ([save-and-restore-gp
                 (lambda (regs e)
                   (let* ([regs (filter (lambda (r) (not (eq? (reg-type r) 'fp))) regs)]
                          [regs (if (fxodd? (length regs))
                                    (cons %tc regs) ; keep doubleword aligned
                                    regs)])
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
                       (let ([info (make-info-vpush (car fp-regs) (length fp-regs))])
                         (%seq
                          (inline ,info ,%vpush-multiple)
                          ,e
                          (inline ,info ,%vpop-multiple)))])))])
            (save-and-restore-gp regs (save-and-restore-fp regs e))))))
    (define-who asm-foreign-call
      (with-output-language (L13 Effect)
        (define int-regs (lambda () (list %Carg1 %Carg2 %Carg3 %Carg4)))
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
                 [load-int64-stack
                  (lambda (offset)
                    (lambda (lorhs hirhs) ; requires rhs
                      (%seq
                       (set! ,(%mref ,%sp ,offset) ,lorhs)
                       (set! ,(%mref ,%sp ,(fx+ offset 4)) ,hirhs))))]
                 [load-int-indirect-stack
                  (lambda (offset from-offset size unsigned?)
                    (lambda (x) ; requires var
                      (case size
                        [(3)
                         (%seq
                          (set! ,(%mref ,%sp ,offset)
                                (inline ,(make-info-load 'integer-16 #f) ,%load ,x ,%zero (immediate ,from-offset)))
                          (set! ,(%mref ,%sp ,(fx+ offset 2))
                                (inline ,(make-info-load 'integer-8 #f) ,%load ,x ,%zero (immediate ,(fx+ from-offset 2)))))]
                        [else
                         `(set! ,(%mref ,%sp ,offset)
                                ,(case size
                                   [(1) `(inline ,(make-info-load (if unsigned? 'unsigned-8 'integer-8) #f) ,%load ,x ,%zero (immediate ,from-offset))]
                                   [(2) `(inline ,(make-info-load (if unsigned? 'unsigned-16 'integer-16) #f) ,%load ,x ,%zero (immediate ,from-offset))]
                                   [(4) (%mref ,x ,from-offset)]))])))]
                 [load-int64-indirect-stack
                  (lambda (offset from-offset)
                    (lambda (x) ; requires var
                      (%seq
                       (set! ,(%mref ,%sp ,offset) ,(%mref ,x ,from-offset))
                       (set! ,(%mref ,%sp ,(fx+ offset 4)) ,(%mref ,x ,(fx+ from-offset 4))))))]
                 [load-double-reg
                  (lambda (fpreg)
                    (lambda (x) ; unboxed
                      `(set! ,fpreg ,x)))]
                 [load-single-reg
                  (lambda (fpreg single?)
                    (lambda (x) ; unboxed
                      (let ([%op (if single? %load-single %double->single)])
                        `(set! ,fpreg (inline ,null-info ,%op ,x)))))]
                 [load-double-int-reg
                  (lambda (loreg hireg)
                    (lambda (x) ; unboxed
                      (%seq
                       (set! ,loreg ,(%inline fpcastto/lo ,x))
                       (set! ,hireg ,(%inline fpcastto/hi ,x)))))]
                 [load-single-int-reg
                  (lambda (reg)
                    (lambda (x) ; unboxed
                      (%seq
                       ;; we can use `%Cfparg1` because this only happens
                       ;; when FP registers are not used for arguments
                       (set! ,%Cfparg1 ,(%inline double->single ,x))
                       (set! ,reg ,(%inline fpcastto/lo ,%Cfparg1)))))]
                 [load-boxed-double-reg
                  (lambda (fpreg fp-disp)
                    (lambda (x) ; address (always a var) of a flonum
                      `(set! ,fpreg ,(%mref ,x ,%zero ,fp-disp fp))))]
                 [load-boxed-single-reg
                  (lambda (fpreg fp-disp single?)
                    (lambda (x) ; address (always a var) of a flonum
                      (let ([%op (if single? %load-single %double->single)])
                        `(set! ,fpreg (inline ,null-info ,%op ,(%mref ,x ,%zero ,fp-disp fp))))))]
                 [load-int-reg
                  (lambda (ireg)
                    (lambda (x)
                      `(set! ,ireg ,x)))]
                 [load-int64-reg
                  (lambda (loreg hireg)
                    (lambda (lo hi)
                      (%seq
                       (set! ,loreg ,lo)
                       (set! ,hireg ,hi))))]
                 [load-int-indirect-reg
                  (lambda (ireg from-offset size unsigned?)
                    (lambda (x)
                      (case size
                        [(3)
                         (let ([tmp %lr]) ; ok to use %lr here?
                           (%seq
                            (set! ,ireg (inline ,(make-info-load 'integer-16 #f) ,%load ,x ,%zero (immediate ,from-offset)))
                            (set! ,tmp (inline ,(make-info-load 'integer-8 #f) ,%load ,x ,%zero (immediate ,(fx+ from-offset 2))))
                            (set! ,tmp ,(%inline sll ,tmp (immediate 16)))
                            (set! ,ireg ,(%inline + ,ireg ,tmp))))]
                        [else
                         `(set! ,ireg ,(case size
                                         [(1) `(inline ,(make-info-load (if unsigned? 'unsigned-8 'integer-8) #f) ,%load ,x ,%zero (immediate ,from-offset))]
                                         [(2) `(inline ,(make-info-load (if unsigned? 'unsigned-16 'integer-16) #f) ,%load ,x ,%zero (immediate ,from-offset))]
                                         [(4) (%mref ,x ,from-offset)]))])))]
                 [load-int64-indirect-reg
                  (lambda (loreg hireg from-offset)
                    (lambda (x)
                      (%seq
                       (set! ,loreg ,(%mref ,x ,from-offset))
                       (set! ,hireg ,(%mref ,x ,(fx+ from-offset 4))))))]
                 [do-args
                  (lambda (types varargs?)
                    ; sgl* is always of even-length, i.e., has a sgl/dbl reg first
                    ; bsgl is set to "b" single (second half of double) if we have one to fill
                    (let loop ([types types] [locs '()] [live* '()] [int* (int-regs)] [sgl* (if varargs? '() (sgl-regs))] [bsgl #f] [isp 0])
                      (if (null? types)
                          (values isp locs live*)
                          (nanopass-case (Ltype Type) (car types)
                            [(fp-double-float)
                             (cond
                              [(and varargs?
                                    ;; For varargs, treat a double like a 64-bit integer
                                    (let ([int* (if (even? (length int*)) int* (cdr int*))])
                                      (and (pair? int*)
                                           int*)))
                               => (lambda (int*)
                                    (loop (cdr types)
                                          (cons (load-double-int-reg (car int*) (cadr int*)) locs)
                                          (cons* (car int*) (cadr int*) live*) (cddr int*) sgl* bsgl isp))]
                              [(null? sgl*)
                               (let ([isp (align 8 isp)])
                                 (loop (cdr types)
                                       (cons (load-double-stack isp) locs)
                                       live* int* '() #f (fx+ isp 8)))]
                              [else
                               (loop (cdr types)
                                     (cons (load-double-reg (car sgl*)) locs)
                            (cons (car sgl*) live*) int* (cddr sgl*) bsgl isp)])]
                            [(fp-single-float)
                             (cond
                              [bsgl
                               (loop (cdr types)
                                     (cons (load-single-reg bsgl #f) locs)
                                     (cons bsgl live*) int* sgl* #f isp)]
                              [(and (not (null? sgl*))
                                    (not varargs?))
                               (loop (cdr types)
                                     (cons (load-single-reg (car sgl*) #f) locs)
                                     (cons (car sgl*) live*) int* (cddr sgl*) (cadr sgl*) isp)]
                              [(and varargs?
                                    (not (null? int*)))
                               (loop (cdr types)
                                     (cons (load-single-int-reg (car int*)) locs)
                                     (cons* (car int*) live*) (cdr int*) sgl* bsgl isp)]
                              [else
                               (loop (cdr types)
                                     (cons (load-single-stack isp) locs)
                                     live* int* '() #f (fx+ isp 4))])]
                            [(fp-ftd& ,ftd)
                             (let ([size ($ftd-size ftd)]
                                   [members ($ftd->members ftd)]
                                   [combine-loc (lambda (loc f)
                                                  (if loc
                                                      (lambda (x) (%seq ,(loc x) ,(f x)))
                                                      f))])
                               (case ($ftd-alignment ftd)
                                 [(8)
                                  (let* ([int* (if (even? (length int*)) int* (cdr int*))]
                                         [num-members (length members)]
                                         [doubles? (and (not varargs?)
                                                        (fx<= num-members 4)
                                                        (andmap double-member? members))])
                                    ;; Sequence of up to 4 doubles that fits in registers?
                                    (cond
                                     [(and doubles?
                                           (fx>= (length sgl*) (fx* 2 num-members)))
                                      ;; Allocate each double to a register
                                      (let dbl-loop ([size size] [offset 0] [live* live*] [sgl* sgl*] [loc #f])
                                        (cond
                                         [(fx= size 0)
                                          (loop (cdr types) (cons loc locs) live* int* sgl* #f isp)]
                                         [else
                                          (dbl-loop (fx- size 8) (fx+ offset 8) (cons (car sgl*) live*) (cddr sgl*)
                                                    (combine-loc loc (load-boxed-double-reg (car sgl*) offset)))]))]
                                     [else
                                      ;; General case; for non-doubles, use integer registers while available,
                                      ;;  possibly splitting between registers and stack
                                      (let obj-loop ([size size] [offset 0] [loc #f]
                                                     [live* live*] [int* int*] [isp isp])
                                        (cond
                                         [(fx= size 0)
                                          (loop (cdr types) (cons loc locs) live* int* sgl* bsgl isp)]
                                         [else
                                          (if (or (null? int*) doubles?)
                                              (let ([isp (align 8 isp)])
                                                (obj-loop (fx- size 8) (fx+ offset 8)
                                                          (combine-loc loc (load-int64-indirect-stack isp offset))
                                                          live* int* (fx+ isp 8)))
                                              (obj-loop (fx- size 8) (fx+ offset 8)
                                                        (combine-loc loc (load-int64-indirect-reg (car int*) (cadr int*) offset))
                                                        (cons* (car int*) (cadr int*) live*) (cddr int*) isp))]))]))]
                                 [else
                                  (let* ([num-members (length members)]
                                         [floats? (and (not varargs?)
                                                       (fx<= num-members 4)
                                                       (andmap float-member? members))])
                                    ;; Sequence of up to 4 floats that fits in registers?
                                    (cond
                                     [(and floats?
                                           (not varargs?)
                                           (fx>= (fx+ (length sgl*) (if bsgl 1 0)) num-members))
                                      ;; Allocate each float to register
                                      (let flt-loop ([size size] [offset 0] [sgl* sgl*] [bsgl bsgl] [loc #f] [live* live*])
                                        (cond
                                         [(fx= size 0)
                                          (loop (cdr types) (cons loc locs) live* int* sgl* bsgl isp)]
                                         [else
                                          (flt-loop (fx- size 4) (fx+ offset 4)
                                                    (if bsgl sgl* (cddr sgl*))
                                                    (if bsgl #f (cadr sgl*))
                                                    (combine-loc loc (load-boxed-single-reg (or bsgl (car sgl*)) offset #t))
                                                    (cons (or bsgl (car sgl*)) live*))]))]
                                     [else
                                      ;; General case; use integer registers while available,
                                      ;;  possibly splitting between registers and stack
                                      (let obj-loop ([size size] [offset 0] [loc #f]
                                                     [live* live*] [int* int*] [isp isp])
                                        (cond
                                         [(fx<= size 0)
                                          (loop (cdr types) (cons loc locs) live* int* sgl* bsgl isp)]
                                         [else
                                          (if (or (null? int*) floats?)
                                              (obj-loop (fx- size 4) (fx+ offset 4)
                                                        (combine-loc loc (load-int-indirect-stack isp offset (fxmin size 4) ($ftd-unsigned? ftd)))
                                                        live* int* (fx+ isp 4))
                                              (obj-loop (fx- size 4) (fx+ offset 4)
                                                        (combine-loc loc (load-int-indirect-reg (car int*) offset (fxmin size 4) ($ftd-unsigned? ftd)))
                                                        (cons (car int*) live*) (cdr int*) isp))]))]))]))]
                            [else
                             (if (nanopass-case (Ltype Type) (car types)
                                                [(fp-integer ,bits) (fx= bits 64)]
                                                [(fp-unsigned ,bits) (fx= bits 64)]
                                                [else #f])
                                 (let ([int* (if (even? (length int*)) int* (cdr int*))])
                                   (if (null? int*)
                                       (let ([isp (align 8 isp)])
                                         (loop (cdr types)
                                               (cons (load-int64-stack isp) locs)
                                               live* '() sgl* bsgl (fx+ isp 8)))
                                       (loop (cdr types)
                                             (cons (load-int64-reg (car int*) (cadr int*)) locs)
                                             (cons* (car int*) (cadr int*) live*) (cddr int*) sgl* bsgl isp)))
                                 (if (null? int*)
                                     (loop (cdr types)
                                           (cons (load-int-stack isp) locs)
                                           live* '() sgl* bsgl (fx+ isp 4))
                                     (loop (cdr types)
                                           (cons (load-int-reg (car int*)) locs)
                                           (cons (car int*) live*) (cdr int*) sgl* bsgl isp)))]))))]
                 [add-fill-result
                  (lambda (fill-result-here? result-type args-frame-size e)
                    (cond
                     [fill-result-here?
                      (nanopass-case (Ltype Type) result-type
                        [(fp-ftd& ,ftd)
                         (let* ([members ($ftd->members ftd)]
                                [num-members (length members)]
                                ;; result pointer is stashed on the stack after all arguments:
                                [dest-x %r2]
                                [init-dest-e `(seq ,e (set! ,dest-x ,(%mref ,%sp ,args-frame-size)))])
                           (cond
                            [(and (fx<= num-members 4)
                                  (or (andmap double-member? members)
                                      (andmap float-member? members)))
                             ;; double/float results are in floating-point registers
                             (let ([double? (and (pair? members) (double-member? (car members)))])
                               (let loop ([members members] [sgl* (sgl-regs)] [offset 0] [e init-dest-e])
                                 (cond
                                  [(null? members) e]
                                  [else
                                   (loop (cdr members)
                                         (if double? (cddr sgl*) (cdr sgl*))
                                         (fx+ offset (if double? 8 4))
                                         `(seq
                                           ,e
                                           ,(if double?
                                                `(set! ,(%mref ,dest-x ,%zero ,offset fp) ,(car sgl*))
                                                (%inline store-single ,(%mref ,dest-x ,%zero ,offset fp) ,(car sgl*)))))])))]
                            [else
                             ;; result is in %Cretval and maybe %r1
                             `(seq
                               ,init-dest-e
                               ,(case ($ftd-size ftd)
                                  [(1) `(inline ,(make-info-load 'integer-8 #f) ,%store ,dest-x ,%zero (immediate 0) ,%Cretval)]
                                  [(2) `(inline ,(make-info-load 'integer-16 #f) ,%store ,dest-x ,%zero (immediate 0) ,%Cretval)]
                                  [(3) (%seq
                                        (inline ,(make-info-load 'integer-16 #f) ,%store ,dest-x ,%zero (immediate 0) ,%Cretval)
                                        (set! ,%Cretval ,(%inline srl ,%Cretval (immediate 16)))
                                        (inline ,(make-info-load 'integer-8 #f) ,%store ,dest-x ,%zero (immediate 2) ,%Cretval))]
                                  [(4) `(set! ,(%mref ,dest-x ,0) ,%Cretval)]
                                  [(8) `(seq
                                         (set! ,(%mref ,dest-x ,0) ,%Cretval)
                                         (set! ,(%mref ,dest-x ,4) ,%r1))]))]))])]
                     [else e]))]
                 [get-result-regs
                  (lambda (result-type varargs?)
                    (nanopass-case (Ltype Type) result-type
                      [(fp-double-float)
                       (if varargs?
                           (list %r1 %Cretval)
                           (list %Cfpretval))]
                      [(fp-single-float)
                       (if varargs?
                           (list %Cretval)
                           (list %Cfpretval))]
                      [(fp-integer ,bits)
                       (case bits
                         [(64) (list %r1 %Cretval)]
                         [else (list %Cretval)])]
                      [(fp-unsigned ,bits)
                       (case bits
                         [(64) (list %r1 %Cretval)]
                         [else (list %Cretval)])]
                      [(fp-ftd& ,ftd)
                       (let* ([members ($ftd->members ftd)]
                              [num-members (length members)])
                         (cond
                          [(and (fx<= num-members 4)
                                (or (andmap double-member? members)
                                    (andmap float-member? members)))
                           ;; double/float results are in floating-point registers
                           (let ([double? (and (pair? members) (double-member? (car members)))])
                             (let loop ([members members] [sgl* (sgl-regs)])
                               (cond
                                [(null? members) '()]
                                [double?
                                 (cons (car sgl*) (loop (cdr members) (cddr sgl*)))]
                                [else
                                 (cons (car sgl*) (if (null? (cdr members))
                                                      '()
                                                      (loop (cddr members) (cddr sgl*))))])))]
                          [else
                           ;; result is in %Cretval and maybe %r1
                           (case ($ftd-size ftd)
                             [(8) (list %Cretval %r1)]
                             [else (list %Cretval)])]))]
                      [else (list %r0)]))]
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
                   [conv* (info-foreign-conv* info)]
                   [varargs? (ormap (lambda (conv) (and (pair? conv) (eq? (car conv) 'varargs))) conv*)]
                   [result-type (info-foreign-result-type info)]
                   [result-reg* (get-result-regs result-type varargs?)]
                   [fill-result-here? (indirect-result-that-fits-in-registers? result-type)]
                   [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)])
              (with-values (do-args (if fill-result-here? (cdr arg-type*) arg-type*)
                                    varargs?)
                (lambda (args-frame-size locs live*)
                  (let* ([frame-size (align 8 (+ args-frame-size
                                                 (if fill-result-here?
                                                     4
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
                         (cons (load-int-stack args-frame-size) locs)]
                        [else locs]))
                     (lambda (t0 not-varargs?)
                       (add-fill-result fill-result-here? result-type args-frame-size
                                        (add-deactivate adjust-active? t0 live* result-reg*
                                                        (lambda (t0)
                                                          `(inline ,(make-info-kill*-live* (add-caller-save-registers result-reg*) live*) ,%c-call ,t0)))))
                     (nanopass-case (Ltype Type) result-type
                       [(fp-double-float)
                        (if varargs?
                            (lambda (lvalue) ; unboxed
                              `(set! ,lvalue ,(%inline fpcastfrom ,%r1 ,%Cretval)))
                            (lambda (lvalue) ; unboxed
                              `(set! ,lvalue ,%Cfpretval)))]
                       [(fp-single-float)
                        (if varargs?
                            (lambda (lvalue) ; unboxed
                              (let ([t %Cfpretval]) ; should be ok as a temporary register
                                `(seq
                                  (set! ,t ,(%inline fpcastfrom ,%r1 ,%Cretval)) ; we don't actually care about the hi/%r1 part
                                  (set! ,lvalue ,(%inline single->double ,t)))))
                            (lambda (lvalue) ; unboxed
                              `(set! ,lvalue ,(%inline single->double ,%Cfpretval))))]
                       [(fp-integer ,bits)
                        (case bits
                          [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline sext8 ,%r0)))]
                          [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline sext16 ,%r0)))]
                          [(32) (lambda (lvalue) `(set! ,lvalue ,%r0))]
                          [(64) (lambda (lvlow lvhigh)
                                  `(seq
                                    (set! ,lvhigh ,%r1)
                                    (set! ,lvlow ,%r0)))]
                          [else (sorry! who "unexpected asm-foreign-procedures fp-integer size ~s" bits)])]
                       [(fp-unsigned ,bits)
                        (case bits
                          [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline zext8 ,%r0)))]
                          [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline zext16 ,%r0)))]
                          [(32) (lambda (lvalue) `(set! ,lvalue ,%r0))]
                          [(64) (lambda (lvlow lvhigh)
                                  `(seq
                                    (set! ,lvhigh ,%r1)
                                    (set! ,lvlow ,%r0)))]
                          [else (sorry! who "unexpected asm-foreign-procedures fp-unsigned size ~s" bits)])]
                       [else (lambda (lvalue) `(set! ,lvalue ,%r0))])
                     (adjust-frame %+))))))))))

    (define-who asm-foreign-callable
      #|
        Frame Layout
                   +---------------------------+
                   |                           |
                   |    incoming stack args    |
  sp+52+R+X+Y+Z+W: |                           |
                   +---------------------------+<- 8-byte boundary
                   |                           | 
                   |    saved int reg args     | 0-4 words
    sp+52+R+X+Y+Z: |                           |
                   +---------------------------+
                   |                           | 
                   |   pad word if necessary   | 0-1 words
      sp+52+R+X+Y: |                           |
                   +---------------------------+<- 8-byte boundary
                   |                           | 
                   |   saved float reg args    | 0-16 words
        sp+52+R+X: |                           |
                   +---------------------------+<- 8-byte boundary
                   |                           | 
                   |      &-return space       | up to 8 words
          sp+52+R: |                           |
                   +---------------------------+<- 8-byte boundary
                   |     activatation state    | 
                   |          and/or           | 0-2 words
            sp+52: |   pad word if necessary   |
                   +---------------------------+
                   |                           |
                   |   callee-save regs + lr   | 13 words
             sp+0: |   callee-save fpregs      |
                   +---------------------------+<- 8-byte boundary

      X = 0 or 4 (depending on whether pad is present)
      Y = int-reg-bytes
      Z = float-reg-bytes
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
          (define load-int-stack
            (lambda (type offset)
              (lambda (lvalue)
                (nanopass-case (Ltype Type) type
                  [(fp-integer ,bits)
                   (case bits
                     [(8) `(set! ,lvalue (inline ,(make-info-load 'integer-8 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                     [(16) `(set! ,lvalue (inline ,(make-info-load 'integer-16 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                     [(32) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                     [else (sorry! who "unexpected load-int-stack fp-integer size ~s" bits)])]
                  [(fp-unsigned ,bits)
                   (case bits
                     [(8) `(set! ,lvalue (inline ,(make-info-load 'unsigned-8 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                     [(16) `(set! ,lvalue (inline ,(make-info-load 'unsigned-16 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                     [(32) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                     [else (sorry! who "unexpected load-int-stack fp-unsigned size ~s" bits)])]
                  [else `(set! ,lvalue ,(%mref ,%sp ,offset))]))))
          (define load-int64-stack
            (lambda (offset)
              (lambda (lolvalue hilvalue)
                (%seq
                 (set! ,lolvalue ,(%mref ,%sp ,offset))
                 (set! ,hilvalue ,(%mref ,%sp ,(fx+ offset 4)))))))
          (define load-stack-address
            (lambda (offset)
              (lambda (lvalue)
                `(set! ,lvalue ,(%inline + ,%sp (immediate ,offset))))))
          (define count-reg-args
            (lambda (types synthesize-first? varargs?)
                    ; bsgl? is #t iff we have a "b" single (second half of double) float reg to fill
              (let f ([types types] [iint (if synthesize-first? -1 0)] [idbl 0] [bsgl? #f])
                (if (null? types)
                    (values iint idbl)
                    (nanopass-case (Ltype Type) (car types)
                      [(fp-double-float)
                       (if varargs?
                           (let ([iint (align 2 iint)])
                             (f (cdr types) (if (fx< iint num-int-regs) (fx+ iint 2) iint) idbl bsgl?))
                           (if (fx< idbl 8)
                               (f (cdr types) iint (fx+ idbl 1) bsgl?)
                               (f (cdr types) iint idbl #f)))]
                      [(fp-single-float)
                       (if varargs?
                           (f (cdr types) (if (fx< iint num-int-regs) (fx+ iint 1) iint) idbl bsgl?)
                           (if bsgl?
                               (f (cdr types) iint idbl #f)
                               (if (fx< idbl 8)
                                   (f (cdr types) iint (fx+ idbl 1) #t)
                                   (f (cdr types) iint idbl #f))))]
                      [(fp-ftd& ,ftd)
                       (let* ([size ($ftd-size ftd)]
                              [members ($ftd->members ftd)]
                              [num-members (length members)])
                         (cond
                          [(and (fx<= num-members 4)
                                (not varargs?)
                                (andmap double-member? members))
                           ;; doubles are either in registers or all on stack
                           (if (fx<= (fx+ idbl num-members) 8)
                               (f (cdr types) iint (fx+ idbl num-members) #f)
                               ;; no more floating-point registers should be used, but ok if we count more
                               (f (cdr types) iint idbl #f))]
                          [(and (fx<= num-members 4)
                                (not varargs?)
                                (andmap float-member? members))
                           ;; floats are either in registers or all on stack
                           (let ([amt (fxsrl (align 2 (fx- num-members (if bsgl? 1 0))) 1)])
                             (if (fx<= (fx+ idbl amt) 8)
                                 (let ([odd-floats? (fxodd? num-members)])
                                   (if bsgl?
                                       (f (cdr types) iint (+ idbl amt) (not odd-floats?))
                                       (f (cdr types) iint (+ idbl amt) odd-floats?)))
                                 ;; no more floating-point registers should be used, but ok if we count more
                                 (f (cdr types) iint idbl #f)))]
                          [(fx= 8 ($ftd-alignment ftd))
                           (f (cdr types) (fxmin 4 (fx+ (align 2 iint) (fxsrl size 2))) idbl bsgl?)]
                          [else
                           (let ([size (align 4 size)])
                             (f (cdr types) (fxmin 4 (fx+ iint (fxsrl size 2))) idbl bsgl?))]))]
                      [else
                       (if (nanopass-case (Ltype Type) (car types)
                                          [(fp-integer ,bits) (fx= bits 64)]
                                          [(fp-unsigned ,bits) (fx= bits 64)]
                                          [else #f])
                           (let ([iint (align 2 iint)])
                             (f (cdr types) (if (fx< iint num-int-regs) (fx+ iint 2) iint) idbl bsgl?))
                           (f (cdr types) (if (fx< iint num-int-regs) (fx+ iint 1) iint) idbl bsgl?))])))))
          (define do-stack
            ; all of the args are on the stack at this point, though not contiguous since
            ; we push all of the int reg args with one push instruction and all of the
            ; float reg args with another (v)push instruction; the saved int regs
            ; continue on into the stack variables, which is convenient when a struct
            ; argument is split across registers and the stack
            (lambda (types saved-reg-bytes pre-pad-bytes return-bytes float-reg-bytes post-pad-bytes int-reg-bytes
                           synthesize-first? varargs?)
              (let* ([return-space-offset (fx+ saved-reg-bytes pre-pad-bytes)]
                     [float-reg-offset (fx+ return-space-offset return-bytes)]
                     [int-reg-offset (fx+ float-reg-offset float-reg-bytes post-pad-bytes)]
                     [stack-arg-offset (fx+ int-reg-offset int-reg-bytes)])
                (let loop ([types (if synthesize-first? (cdr types) types)]
                           [locs '()]
                           [iint 0]
                           [idbl 0]
                           [bsgl-offset #f]
                           [int-reg-offset int-reg-offset]
                           [float-reg-offset float-reg-offset]
                           [stack-arg-offset stack-arg-offset])
                  (if (null? types)
                      (let ([locs (reverse locs)])
                        (if synthesize-first?
                            (cons (load-stack-address return-space-offset)
                                  locs)
                            locs))
                      (nanopass-case (Ltype Type) (car types)
                        [(fp-double-float)
                         (cond
                          [(and varargs?
                                ;; For varargs, treat a double like a 64-bit integer
                                (let ([iint (align 2 iint)])
                                  (and (fx< iint num-int-regs)
                                       iint)))
                           => (lambda (new-iint)
                                (let ([int-reg-offset (if (fxeven? iint) int-reg-offset (fx+ int-reg-offset 4))]
                                      [iint new-iint])
                                  (loop (cdr types)
                                        (cons (load-double-stack int-reg-offset) locs)
                                        (fx+ iint 2) idbl bsgl-offset (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset)))]
                          [(and (not varargs?)
                                (< idbl num-dbl-regs))
                           (loop (cdr types)
                                 (cons (load-double-stack float-reg-offset) locs)
                                 iint (fx+ idbl 1) bsgl-offset int-reg-offset (fx+ float-reg-offset 8) stack-arg-offset)]
                          [else
                           (let ([stack-arg-offset (align 8 stack-arg-offset)]
                                 [iint (if varargs? (align 2 iint) iint)]) ; use up register if argument didn't fit
                             (loop (cdr types)
                                   (cons (load-double-stack stack-arg-offset) locs)
                                   iint num-dbl-regs #f int-reg-offset float-reg-offset (fx+ stack-arg-offset 8)))])]
                        [(fp-single-float)
                         (cond
                          [bsgl-offset
                           (loop (cdr types)
                                 (cons (load-single-stack bsgl-offset) locs)
                                 iint idbl #f int-reg-offset float-reg-offset stack-arg-offset)]
                          [(and (< idbl num-dbl-regs)
                                (not varargs?))
                           (loop (cdr types)
                                 ; with big-endian ARM might need to adjust offset +/- 4 since pair of
                                 ; single floats in a pushed double float might be reversed
                                 (cons (load-single-stack float-reg-offset) locs)
                                 iint (fx+ idbl 1) (fx+ float-reg-offset 4) int-reg-offset (fx+ float-reg-offset 8) stack-arg-offset)]
                          [(and varargs?
                                (fx< iint num-int-regs))
                           (loop (cdr types)
                                 (cons (load-single-stack int-reg-offset) locs)
                                 (fx+ iint 1) idbl bsgl-offset (fx+ int-reg-offset 4) float-reg-offset stack-arg-offset)]
                          [else
                           (loop (cdr types)
                                 (cons (load-single-stack stack-arg-offset) locs)
                                 iint num-dbl-regs #f int-reg-offset float-reg-offset (fx+ stack-arg-offset 4))])]
                        [(fp-ftd& ,ftd)
                         (let* ([size ($ftd-size ftd)]
                                [members ($ftd->members ftd)]
                                [num-members (length members)])
                           (cond
                            [(and (not varargs?)
                                  (fx<= num-members 4)
                                  (andmap double-member? members))
                             ;; doubles are either in registers or all on stack
                             (if (fx<= (fx+ idbl num-members) num-dbl-regs)
                                 (loop (cdr types)
                                       (cons (load-stack-address float-reg-offset) locs)
                                       iint (fx+ idbl num-members) #f int-reg-offset (fx+ float-reg-offset size) stack-arg-offset)
                                 (let ([stack-arg-offset (align 8 stack-arg-offset)])
                                   (loop (cdr types)
                                         (cons (load-stack-address stack-arg-offset) locs)
                                         iint num-dbl-regs #f int-reg-offset #f (fx+ stack-arg-offset size))))]
                            [(and (not varargs?)
                                  (fx<= num-members 4)
                                  (andmap float-member? members))
                             ;; floats are either in registers or all on stack
                             (let ([amt (fxsrl (align 2 (fx- num-members (if bsgl-offset 1 0))) 1)])
                               (if (fx<= (fx+ idbl amt) num-dbl-regs)
                                   (let ([odd-floats? (fxodd? num-members)])
                                     (if bsgl-offset
                                         (let ([dbl-size (align 8 (fx- size 4))])
                                           (loop (cdr types)
                                                 (cons (load-stack-address bsgl-offset) locs)
                                                 iint (fx+ idbl amt) (if odd-floats? #f (+ bsgl-offset size)) int-reg-offset
                                                 (fx+ float-reg-offset dbl-size) stack-arg-offset))
                                         (let ([dbl-size (align 8 size)])
                                           (loop (cdr types)
                                                 (cons (load-stack-address float-reg-offset) locs)
                                                 iint (fx+ idbl amt) (and odd-floats? (fx+ float-reg-offset size)) int-reg-offset
                                                 (fx+ float-reg-offset dbl-size) stack-arg-offset))))
                                   (loop (cdr types)
                                         (cons (load-stack-address stack-arg-offset) locs)
                                         iint num-dbl-regs #f int-reg-offset float-reg-offset (fx+ stack-arg-offset size))))]
                            [(fx= 8 ($ftd-alignment ftd))
                             (let ([int-reg-offset (if (fxeven? iint) int-reg-offset (fx+ int-reg-offset 4))]
                                   [iint (align 2 iint)]
                                   [amt (fxsrl size 2)])
                               (if (fx< iint num-int-regs) ; argument starts in registers, may continue on stack
                                   (loop (cdr types)
                                         (cons (load-stack-address int-reg-offset) locs)
                                         (fxmin num-int-regs (fx+ iint amt)) idbl bsgl-offset (fx+ int-reg-offset size) float-reg-offset
                                         (fx+ stack-arg-offset (fxmax 0 (fx* 4 (fx- (fx+ iint amt) 4)))))
                                   (let ([stack-arg-offset (align 8 stack-arg-offset)])
                                     (loop (cdr types)
                                           (cons (load-stack-address stack-arg-offset) locs)
                                           iint idbl bsgl-offset int-reg-offset float-reg-offset (fx+ stack-arg-offset size)))))]
                            [else
                             (let* ([size (align 4 size)]
                                    [amt (fxsrl size 2)])
                               (if (fx< iint num-int-regs) ; argument starts in registers, may continue on stack
                                   (loop (cdr types)
                                         (cons (load-stack-address int-reg-offset) locs)
                                         (fxmin num-int-regs (fx+ iint amt)) idbl bsgl-offset (fx+ int-reg-offset size) float-reg-offset
                                         (fx+ stack-arg-offset (fxmax 0 (fx* 4 (fx- (fx+ iint amt) 4)))))
                                   (loop (cdr types)
                                         (cons (load-stack-address stack-arg-offset) locs)
                                         iint idbl bsgl-offset int-reg-offset float-reg-offset (fx+ stack-arg-offset size))))]))]
                        [else
                         (if (nanopass-case (Ltype Type) (car types)
                                            [(fp-integer ,bits) (fx= bits 64)]
                                            [(fp-unsigned ,bits) (fx= bits 64)]
                                            [else #f])
                             (let ([int-reg-offset (if (fxeven? iint) int-reg-offset (fx+ int-reg-offset 4))]
                                   [iint (align 2 iint)])
                               (if (fx= iint num-int-regs)
                                   (let ([stack-arg-offset (align 8 stack-arg-offset)])
                                     (loop (cdr types)
                                           (cons (load-int64-stack stack-arg-offset) locs)
                                           iint idbl bsgl-offset int-reg-offset float-reg-offset (fx+ stack-arg-offset 8)))
                                   (loop (cdr types)
                                         (cons (load-int64-stack int-reg-offset) locs)
                                         (fx+ iint 2) idbl bsgl-offset (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset)))
                             (if (fx= iint num-int-regs)
                                 (loop (cdr types)
                                       (cons (load-int-stack (car types) stack-arg-offset) locs)
                                       iint idbl bsgl-offset int-reg-offset float-reg-offset (fx+ stack-arg-offset 4))
                                 (loop (cdr types)
                                       (cons (load-int-stack (car types) int-reg-offset) locs)
                                       (fx+ iint 1) idbl bsgl-offset (fx+ int-reg-offset 4) float-reg-offset stack-arg-offset)))]))))))
          (define do-result
            (lambda (result-type synthesize-first? varargs? return-stack-offset)
              (nanopass-case (Ltype Type) result-type
                [(fp-ftd& ,ftd)
                 (let* ([members ($ftd->members ftd)]
                        [num-members (length members)])
                   (cond
                    [(and (not varargs?)
                          (fx<= 1 num-members 4)
                          (or (andmap double-member? members)
                              (andmap float-member? members)))
                     ;; double/float results returned in floating-point registers
                     (values
                      (lambda ()
                        (let ([double? (and (pair? members) (double-member? (car members)))])
                          (let loop ([members members] [sgl* (sgl-regs)] [offset return-stack-offset] [e #f])
                            (cond
                             [(null? members) e]
                             [else
                              (loop (cdr members)
                                    (if double? (cddr sgl*) (cdr sgl*))
                                    (fx+ offset (if double? 8 4))
                                    (let ([new-e
                                           (if double?
                                               `(set! ,(car sgl*) ,(%mref ,%sp ,%zero ,offset fp))
                                               `(set! ,(car sgl*) ,(%inline load-single ,(%mref ,%sp ,%zero ,offset fp))))])
                                      (if e `(seq ,e ,new-e) new-e)))]))))
                      (let ([double? (and (pair? members) (double-member? (car members)))])
                        (let loop ([members members] [sgl* (sgl-regs)] [aligned? #t])
                          (cond
                           [(null? members) '()]
                           [else (let ([regs (loop (cdr members)
                                                   (if double? (cddr sgl*) (cdr sgl*))
                                                   (or double? (not aligned?)))])
                                   (if aligned? (cons (car sgl*) regs) regs))])))
                      ($ftd-size ftd))]
                    [else
                     (case ($ftd-size ftd)
                       [(8)
                        (values (lambda ()
                                  `(seq
                                    (set! ,%Cretval ,(%mref ,%sp ,return-stack-offset))
                                    (set! ,%r1 ,(%mref ,%sp ,(fx+ 4 return-stack-offset)))))
                                (list %Cretval %r1)
                                8)]
                       [else
                        (values (lambda ()
                                  (case ($ftd-size ftd)
                                    [(1)
                                     (let ([rep (if ($ftd-unsigned? ftd) 'unsigned-8 'integer-8)])
                                       `(set! ,%Cretval (inline ,(make-info-load rep #f) ,%load ,%sp ,%zero (immediate ,return-stack-offset))))]
                                    [(2)
                                     (let ([rep (if ($ftd-unsigned? ftd) 'unsigned-16 'integer-16)])
                                       `(set! ,%Cretval (inline ,(make-info-load rep #f) ,%load ,%sp ,%zero (immediate ,return-stack-offset))))]
                                    [else `(set! ,%Cretval ,(%mref ,%sp ,return-stack-offset))]))
                                (list %Cretval)
                                4)])]))]
                [(fp-double-float)
                 (values (if varargs?
                             (lambda (rhs)
                               (let-values ([(endreg otherreg) (constant-case native-endianness
                                                                              [(little) (values %Cretval %r1)]
                                                                              [(big) (values %r1 %Cretval)])])
                                 `(seq
                                   (set! ,endreg ,(%mref ,rhs ,(constant flonum-data-disp)))
                                   (set! ,otherreg ,(%mref ,rhs ,(fx+ 4 (constant flonum-data-disp)))))))
                             (lambda (rhs)
                               `(set! ,%Cfpretval ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp))))
                         (if varargs?
                             (list %Cretval %r1)
                             (list %Cfpretval))
                         0)]
                [(fp-single-float)
                 (values (if varargs?
                             (lambda (rhs)
                               `(seq
                                 (set! ,%Cfpretval ,(%inline double->single ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp)))
                                 (set! ,%Cretval ,(%inline fpcastto/lo ,%Cfpretval))))
                             (lambda (rhs)
                               `(set! ,%Cfpretval ,(%inline double->single ,(%mref ,rhs ,%zero ,(constant flonum-data-disp) fp)))))
                         (if varargs?
                             (list %Cretval)
                             (list %Cfpretval))
                         0)]
                [(fp-void)
                 (values (lambda () `(nop))
                         '()
                         0)]
                [else
                 (cond
                  [(nanopass-case (Ltype Type) result-type
                     [(fp-integer ,bits) (fx= bits 64)]
                     [(fp-unsigned ,bits) (fx= bits 64)]
                     [else #f])
                   (values (lambda (lo hi)
                             `(seq
                               (set! ,%Cretval ,lo)
                               (set! ,%r1 ,hi)))
                           (list %Cretval %r1)
                           0)]
                  [else
                   (values (lambda (x)
                             `(set! ,%Cretval ,x))
                           (list %Cretval)
                           0)])])))
          (lambda (info)
            (define callee-save-regs+lr (list %r4 %r5 %r6 %r7 %r8 %r9 %r10 %r11 %lr))
            (define callee-save-fpregs  (list %fp1 %fp2)) ; must be consecutive
            (define isaved (length callee-save-regs+lr))
            (define fpsaved (length callee-save-fpregs))
            (safe-assert (andmap (lambda (r)
                                   (or (not (reg-callee-save? r))
                                       (if (eq? (reg-type r) 'fp)
                                           (memq r callee-save-fpregs)
                                           (memq r callee-save-regs+lr))))
                                 (vector->list regvec)))
            (let* ([arg-type* (info-foreign-arg-type* info)]
                   [conv* (info-foreign-conv* info)]
                   [varargs? (ormap (lambda (conv) (and (pair? conv) (eq? (car conv) 'varargs))) conv*)]
                   [result-type (info-foreign-result-type info)]
                   [synthesize-first? (indirect-result-that-fits-in-registers? result-type)]
                   [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)])
              (let-values ([(iint idbl) (count-reg-args arg-type* synthesize-first? varargs?)])
                (let ([saved-reg-bytes (fx+ (fx* isaved 4) (fx* fpsaved 8))]
                      [pre-pad-bytes (if (fxeven? isaved)
                                         (if adjust-active? 8 0)
                                         4)]
                      [int-reg-bytes (fx* iint 4)]
                      [post-pad-bytes (if (fxeven? iint) 0 4)]
                      [float-reg-bytes (fx* idbl 8)])
                  (let-values ([(get-result result-regs return-bytes)
                                (do-result result-type synthesize-first? varargs?
                                           (fx+ saved-reg-bytes pre-pad-bytes))])
                    (let ([return-bytes (align 8 return-bytes)])
                      (values
                       (lambda ()
                         (%seq
                          ; save argument register values to the stack so we don't lose the values
                          ; across possible calls to C while setting up the tc and allocating memory
                          ,(if (fx= iint 0) `(nop) `(inline ,(make-info-kill*-live* '() (list-head (list %Carg1 %Carg2 %Carg3 %Carg4) iint)) ,%push-multiple))
                          ; pad if necessary to force 8-byte boundary, and make room for indirect return:
                          ,(let ([len (+ post-pad-bytes return-bytes)])
                             (if (fx= len 0) `(nop) `(set! ,%sp ,(%inline - ,%sp (immediate ,len)))))
                          ,(if (fx= idbl 0) `(nop) `(inline ,(make-info-vpush %Cfparg1 idbl) ,%vpush-multiple))
                          ; pad if necessary to force 8-byte boundary after saving callee-save-regs+lr
                          ,(if (fx= pre-pad-bytes 0) `(nop) `(set! ,%sp ,(%inline - ,%sp (immediate ,pre-pad-bytes))))
                          ; save the callee save registers & return address
                          (inline ,(make-info-kill*-live* '() callee-save-regs+lr) ,%push-multiple)
                          (inline ,(make-info-vpush (car callee-save-fpregs) fpsaved) ,%vpush-multiple)
                          ; maybe activate
                          ,(if adjust-active?
                               `(seq
                                 (set! ,%Cretval ,(%inline activate-thread))
                                 (set! ,(%mref ,%sp ,saved-reg-bytes) ,%Cretval))
                               `(nop))
                          ; set up tc for benefit of argument-conversion code, which might allocate
                          ,(if-feature pthreads
                                       (%seq
                                        (set! ,%r0 ,(%inline get-tc))
                                        (set! ,%tc ,%r0))
                                       `(set! ,%tc (literal ,(make-info-literal #f 'entry (lookup-c-entry thread-context) 0))))))
                       ; list of procedures that marshal arguments from their C stack locations
                       ; to the Scheme argument locations
                       (do-stack arg-type* saved-reg-bytes pre-pad-bytes return-bytes float-reg-bytes post-pad-bytes int-reg-bytes
                                 synthesize-first? varargs?)
                       get-result
                       (lambda ()
                         (in-context Tail
                                     (%seq
                                      ,(if adjust-active?
                                           (%seq
                                            ;; We need *(sp+saved-reg-bytes) in %Carg1,
                                            ;; but that can also be a return register.
                                            ;; Meanwhle, sp may change before we call unactivate.
                                            ;; So, move to %r2 for now, then %Carg1 later:
                                            (set! ,%r2 ,(%mref ,%sp ,saved-reg-bytes))
                                            ,(save-and-restore
                                              result-regs
                                              `(seq
                                                (set! ,%Carg1 ,%r2)
                                                ,(%inline unactivate-thread ,%Carg1))))
                                           `(nop))
                                      ; restore the callee save registers
                                      (inline ,(make-info-vpush (car callee-save-fpregs) fpsaved) ,%vpop-multiple)
                                      (inline ,(make-info-kill* callee-save-regs+lr) ,%pop-multiple)
                                      ; deallocate space for pad & arg reg values
                                      (set! ,%sp ,(%inline + ,%sp (immediate ,(fx+ pre-pad-bytes int-reg-bytes return-bytes post-pad-bytes float-reg-bytes))))
                                      ; done
                                                          (asm-c-return ,null-info ,callee-save-regs+lr ... ,result-regs ...)))))))))))))))
)
