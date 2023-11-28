;;; x86_64.ss
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
(if-feature windows
  (define-registers
    (reserved
      [%tc  %r14           #t 14 uptr]
      [%sfp %r13           #t 13 uptr]
      [%ap  %rdi           #t  7 uptr]
      #;[%esp]
      #;[%eap]
      #;[%trap])
    (allocable
      [%ac0 %rbp           #t  5 uptr]
      [%xp  %r12           #t 12 uptr]
      [%ts  %rax %Cretval  #f  0 uptr]
      [%td  %rbx           #t  3 uptr]
      [%ac1 %r10 %deact    #f 10 uptr]
      [%yp  %r11           #f 11 uptr]
      [%cp  %r15           #t 15 uptr]
      [#;%ret %rsi         #t  6 uptr]
      [     %rdx %Carg2    #f  2 uptr]
      [     %r8  %Carg3    #f  8 uptr]
      [     %r9  %Carg4    #f  9 uptr]
      [     %rcx %Carg1    #f  1 uptr] ; last to avoid use as a Scheme argument
      [%fp1                #f  4 fp]
      [%fp2                #f  5 fp]
      [%fp3                #t  6 fp]
      [%fp4                #t  7 fp]
      [%fp5                #t  8 fp]
      [%fp6                #t  9 fp]
      [%fp7                #t  10 fp]
      [%fp8                #t  11 fp])
    (machine-dependent
      [%Cfparg1 %Cfpretval #f  0 fp]  ; xmm 0-5 are caller-save
      [%Cfparg2            #f  1 fp]  ; xmm 6-15 are callee-save
      [%Cfparg3            #f  2 fp]
      [%Cfparg4            #f  3 fp]
      [%sp                 #t  4 uptr]))
  (define-registers
    (reserved
      [%tc  %r14           #t 14 uptr]
      [%sfp %r13           #t 13 uptr]
      [%ap  %r9  %Carg6    #f  9 uptr]
      #;[%esp]
      #;[%eap]
      #;[%trap])
    (allocable
      [%ac0 %rbp           #t  5 uptr]
      [%xp  %r12           #t 12 uptr]
      [%ts  %rax %Cretval  #f  0 uptr]
      [%td  %rbx           #t  3 uptr]
      [%ac1 %r10 %deact    #f 10 uptr]
      [%yp  %r11           #f 11 uptr]
      [%cp  %r15           #t 15 uptr]
      [#;%ret %r8  %Carg5  #f  8 uptr]
      [     %rdi %Carg1    #f  7 uptr]
      [     %rsi %Carg2    #f  6 uptr]
      [     %rdx %Carg3    #f  2 uptr]
      [     %rcx %Carg4    #f  1 uptr]
      [%fp1                #f  8 fp]
      [%fp2                #f  9 fp]
      [%fp3                #f  10 fp]
      [%fp4                #f  11 fp]
      [%fp5                #f  12 fp]
      [%fp6                #f  13 fp]
      [%fp7                #f  14 fp]
      [%fp8                #f  15 fp])
    (machine-dependent
      [%Cfparg1 %Cfpretval #f  0 fp] ; no FP registers are callee-save
      [%Cfparg2            #f  1 fp]
      [%Cfparg3            #f  2 fp]
      [%Cfparg4            #f  3 fp]
      [%Cfparg5            #f  4 fp]
      [%Cfparg6            #f  5 fp]
      [%Cfparg7            #f  6 fp]
      [%Cfparg8            #f  7 fp]
      [%sp                 #t  4 uptr])))

;;; SECTION 2: instructions
(module (md-handle-jump  ; also sets primitive handlers
         mem->mem
         fpmem->fpmem
         coercible?
         coerce-opnd
         acsame-mem
         acsame-ur)
  (import asm-module)

  (define real-imm32?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm)
         (constant-case ptr-bits
           [(32) #t]                   ; allows 2^31...2^32-1 per immediate?
           [(64) (signed-32? imm)])]   ; 2^31...2^32-1 aren't 32-bit values on 64-bit machines
        [else #f])))

  (define negatable-real-imm32?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (<= #x-7FFFFFFF imm #x7FFFFFFF)]
        [else #f])))

  (define mref->mref
    (lambda (a k)
      (define return
        (lambda (x0 x1 imm type)
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm ,type)))))
      (nanopass-case (L15c Triv) a
        [(mref ,lvalue0 ,lvalue1 ,imm ,type)
         (lvalue->ur lvalue0
           (lambda (x0)
             (lvalue->ur lvalue1
               (lambda (x1)
                 (if (signed-32? imm)
                     (return x0 x1 imm type)
                     (let ([u (make-tmp 'u)])
                       (seq
                         (build-set! ,u (immediate ,imm))
                         (if (eq? x1 %zero)
                             (return x0 u 0 type)
                             (seq
                               (build-set! ,u (asm ,null-info ,asm-add ,u ,x1))
                               (return x0 u 0 type))))))))))])))

  (define mem->mem
    (lambda (a k)
      (cond
        [(literal@? a)
         (let ([u (make-tmp 'u)])
           (seq
             (build-set! ,u ,(literal@->literal a))
             (k (with-output-language (L15d Lvalue) `(mref ,u ,%zero 0 ptr)))))]
        [else (mref->mref a k)])))

  (define fpmem->fpmem mem->mem)

  ;; `define-instruction` code takes care of `ur` and `fpur`, to which
  ;; all type-compatible values must convert
  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (and (memq 'imm32 aty*) (imm32? a))
             (and (memq 'imm aty*) (imm? a))
             (and (memq 'zero aty*) (imm0? a))
             (and (memq 'real-imm32 aty*) (real-imm32? a))
             (and (memq 'negatable-real-imm32 aty*) (negatable-real-imm32? a))
             (and (memq 'mem aty*) (mem? a))
             (and (memq 'fpmem aty*) (fpmem? a))))]))

  ;; `define-instruction` doesn't try to cover `ur` and `fpur`
  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
           [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
           [(and (memq 'fpmem aty*) (fpmem? a)) (mem->mem a k)]
           [(and (memq 'imm32 aty*) (imm32? a)) (k (imm->imm a))]
           [(and (memq 'imm aty*) (imm? a)) (k (imm->imm a))]
           [(and (memq 'zero aty*) (imm0? a)) (k (imm->imm a))]
           [(and (memq 'real-imm32 aty*) (real-imm32? a)) (k (imm->imm a))]
           [(and (memq 'negatable-real-imm32 aty*) (negatable-real-imm32? a)) (k (imm->imm a))]
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
               (mem->mem a
                 (lambda (a)
                   (let ([u (make-tmp 'u 'fp)])
                     (seq
                       (build-set! ,u ,a)
                       (k u)))))]
              [else
               (sorry! 'coerce-opnd "unexpected operand ~s" a)])]
           [else (sorry! 'coerce-opnd "cannot coerce ~s to ~s" a aty*)]))]))

  (define-who extract-imm
    (lambda (e)
      (nanopass-case (L15d Triv) e
        [(immediate ,imm) imm]
        [else (sorry! who "~s is not an immediate" e)])))

  (define md-handle-jump
    (lambda (t)
      (with-output-language (L15d Tail)
        (nanopass-case (L15c Triv) t
          [,lvalue
           (if (mem? lvalue)
               (mem->mem lvalue
                 (lambda (mref)
                   (values '() `(jump ,mref))))
               (values '() `(jump ,lvalue)))]
          [(literal ,info)
           (guard (and (not (info-literal-indirect? info))
                       (memq (info-literal-type info) '(entry library-code))))
           (values '() `(jump (literal ,info)))]
          [(label-ref ,l ,offset)
           (values '() `(jump (label-ref ,l ,offset)))]
          [else
           (let ([tmp (make-tmp 'utmp)])
             (values
               (with-output-language (L15d Effect) `(set! ,(make-live-info) ,tmp ,t))
               `(jump ,tmp)))]))))

  (define-syntax acsame-mem
    (lambda (stx)
      (syntax-case stx ()
        [(_ orig c cty (b bty* ...) k)
         #'(mem->mem c
             (lambda (c)
               (k c b)))]
        [(_ orig c cty k)
         #'(mem->mem c
                     (lambda (c)
                       (k c)))])))

  (define-syntax acsame-ur
    (lambda (stx)
      (syntax-case stx ()
        [(moi orig c cty (b bty* ...) k)
         #`(cond
             [(ur? c) (k c b)]
             [(lmem? c)
              (nanopass-case (L15c Triv) c
                [(mref ,lvalue0 ,lvalue1 ,imm ,type)
                 ;; TODO: does this use too many registers? (no longer special casing fv x0, x1 case)
                 (lvalue->ur
                  lvalue0
                  (lambda (x0)
                    (lvalue->ur
                     lvalue1
                     (lambda (x1)
                       (let ([u1 (make-tmp 'u)])
                         (if (signed-32? imm)
                             (seq
                              (build-set! ,u1 (mref ,x0 ,x1 ,imm ,type))
                              (k u1 b)
                              (build-set! (mref ,x0 ,x1 ,imm ,type) ,u1))
                             (let ([u2 (make-tmp 'u)])
                               (seq
                                (build-set! ,u2 ,imm)
                                (build-set! ,x1 (asm ,null-info ,asm-add ,x1 ,u2))
                                (build-set! ,u1 (mref ,x0 ,x1 0 ,type))
                                (k u1 b)
                                (build-set! (mref ,x0 ,x1 0 ,type) ,u1)))))))))])]
             ;; can't be literal@ since literals can't be lvalues
             [else (sorry! 'moi "unexpected operand ~s" c)])]
        [(moi orig c cty k)
         #`(if (ur? c)
               (k c)
               (mem->mem c
                         (lambda (c)
                           (let ([u (make-tmp 'u)])
                             (seq
                              (build-set! ,u ,c)
                              (k u)
                              (build-set! ,c ,u))))))])))

  ; x is not the same as z in any clause that follows a clause where (x z)
  ; and y is coercible to one of its types, however:
  ; WARNING: do not assume that if x isn't the same as z then x is independent
  ; of z, since x might be an mref with z as it's base or index

  (define-instruction value (-)
    [(op (z mem) (x z) (y ur imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub ,x ,y))]
    [(op (z mem) (x zero) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-negate ,y))]
    [(op (z ur) (x z) (y ur mem imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub ,x ,y))]
    [(op (z ur) (x zero) (y ur))
     (seq
       `(set! ,(make-live-info) ,z ,y)
       `(set! ,(make-live-info) ,z (asm ,info ,asm-negate ,z)))]
    [(op (z ur) (x ur mem imm32) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub-negate ,y ,x))]
    [(op (z ur) (x ur) (y negatable-real-imm32))
     (seq
       `(move-related ,z ,x)
       `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea1 (- (extract-imm y))) ,x)))]
    [(op (z ur) (x mem imm32) (y ur))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,y)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-sub-negate ,t ,x))
         `(set! ,(make-live-info) ,z ,t)))]
    [(op (z ur) (x ur) (y ur mem imm32))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-sub ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))])

  (define-instruction value (-/ovfl -/eq -/pos) ; must set condition codes, so can't use lea or sub-negate
    [(op (z mem) (x z) (y ur imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub ,x ,y))]
    [(op (z mem) (x zero) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-negate ,y))]
    [(op (z ur) (x z) (y ur mem imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub ,x ,y))]
    [(op (z ur) (x zero) (y ur))
     (seq
       `(set! ,(make-live-info) ,z ,y)
       `(set! ,(make-live-info) ,z (asm ,info ,asm-negate ,z)))]
    [(op (z ur) (x ur) (y ur mem imm32))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-sub ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))])

  (define-instruction value (+)
    [(op (z mem) (x z) (y ur imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,y))]
    [(op (z mem) (x ur imm32) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,x))]
    [(op (z ur) (x z) (y ur mem imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,y))]
    [(op (z ur) (x ur mem imm32) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,x))]
    [(op (z ur) (x ur) (y real-imm32))
     (seq
       `(move-related ,z ,x)
       `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea1 (extract-imm y)) ,x)))]
    [(op (z ur) (x real-imm32) (y ur))
     (seq
       `(move-related ,z ,y)
       `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea1 (extract-imm x)) ,y)))]
    [(op (z ur) (x ur) (y mem imm32))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-add ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))]
    [(op (z ur) (x mem imm32) (y ur))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,y)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-add ,t ,x))
         `(set! ,(make-live-info) ,z ,t)))]
    [(op (z ur) (x ur) (y ur))
     (seq
       `(move-related ,z ,y)
       `(move-related ,z ,x)
       `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea2 0) ,x ,y)))])

  (define-instruction value (+/ovfl +/carry) ; must set condition codes, so can't use lea
    [(op (z mem) (x z) (y ur imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,y))]
    [(op (z mem) (x ur imm32) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,x))]
    [(op (z ur) (x z) (y ur mem imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,y))]
    [(op (z ur) (x ur mem imm32) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,z ,x))]
    [(op (z ur) (x ur) (y mem imm32))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-add ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))]
    [(op (z ur) (x mem imm32) (y ur))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,y)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-add ,t ,x))
         `(set! ,(make-live-info) ,z ,t)))]
    [(op (z ur) (x ur) (y ur))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-add ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))])

  (define-instruction value (* */ovfl) ; */ovfl must set multiply-overflow flag on overflow
    [(op (z ur) (x z) (y ur mem))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,z ,y))]
    [(op (z ur) (x ur mem) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,z ,x))]
    [(op (z ur) (x ur mem) (y imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-muli ,x ,y))]
    [(op (z ur) (x imm32) (y ur mem))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-muli ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,asm-mul ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))])

  (define-instruction value (/)
    (definitions
      (define go
        (lambda (z x y)
          (let ([urax (make-precolored-unspillable 'urax %rax)]
                [urdx (make-precolored-unspillable 'urdx %rdx)])
            (with-output-language (L15d Effect)
              (seq
                `(set! ,(make-live-info) ,urax ,x)
                `(set! ,(make-live-info) ,urdx (asm ,null-info ,asm-sext-rax->rdx ,urax))
                `(set! ,(make-live-info) ,urax (asm ,null-info ,asm-div ,urax ,urdx ,y))
                `(set! ,(make-live-info) ,z ,urax)))))))
    [(op (z mem) (x ur mem imm) (y ur mem)) (go z x y)]
    [(op (z ur) (x ur mem imm) (y ur mem)) (go z x y)])

  (define-instruction value (logand logor logxor)
    [(op (z mem) (x z) (y ur imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-addop op) ,z ,y))]
    [(op (z mem) (x ur imm32) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-addop op) ,z ,x))]
    [(op (z ur) (x z) (y ur mem imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-addop op) ,z ,y))]
    [(op (z ur) (x ur mem imm32) (y z))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-addop op) ,z ,x))]
    [(op (z ur) (x ur) (y mem imm32))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,(asm-addop op) ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))]
    [(op (z ur) (x ur mem imm32) (y ur))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,y)
         `(set! ,(make-live-info) ,t (asm ,info ,(asm-addop op) ,t ,x))
         `(set! ,(make-live-info) ,z ,t)))])

  (define-instruction value (lognot)
    [(op (z mem) (x z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))]
    [(op (z ur) (x z))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))]
    [(op (z ur) (x ur mem imm32))
     (seq
        `(set! ,(make-live-info) ,z ,x)
        `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,z)))])

  ; TODO: use lea for certain constant shifts when x != z
  (define-instruction value (sll srl sra)
    (definitions
      (define go
        (lambda (info op z x y)
          (let ([urcx (make-precolored-unspillable 'urcx %rcx)])
            (with-output-language (L15d Effect)
              (seq
                `(set! ,(make-live-info) ,urcx ,y)
                `(set! ,(make-live-info) ,z (asm ,info ,(asm-shiftop op) ,x ,urcx))))))))
    [(op (z mem) (x z) (y imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-shiftop op) ,x ,y))]
    ;; NB: need to return in these cases?
    [(op (z mem) (x z) (y ur mem imm)) (go info op z x y)]
    [(op (z ur) (x z) (y imm32))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-shiftop op) ,x ,y))]
    [(op (z ur) (x z) (y ur mem imm)) (go info op z x y)]
    [(op (z ur) (x ur mem imm32) (y imm32))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         `(set! ,(make-live-info) ,t (asm ,info ,(asm-shiftop op) ,t ,y))
         `(set! ,(make-live-info) ,z ,t)))]
    [(op (z ur) (x ur mem imm32) (y ur mem imm))
     (let ([t (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,t ,x)
         (go info op t t y)
         `(set! ,(make-live-info) ,z ,t)))])

  (define-instruction value popcount
    [(op (z ur) (x ur mem))
     ;; Direct POPCNT instruction variant, works with corresponding `popcount-op`:
     #;
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-popcount (info-inline? info)) ,x))

     ;; Link-editable variant, for corresponding `popcount-op`:
     (let ([urcx (make-precolored-unspillable 'urcx %rcx)]
           [urax (make-precolored-unspillable 'urax %rax)])
       (seq
        `(set! ,(make-live-info) ,urcx ,x)
        `(set! ,(make-live-info) ,urax (asm ,info ,(asm-popcount (info-inline? info)) ,urcx))
        `(set! ,(make-live-info) ,z ,urax)))])

  (define-instruction value move
    [(op (z mem) (x ur imm32))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem imm))
     ; NOTE: risc arch's will need to deal with limitations on imm
     `(set! ,(make-live-info) ,z ,x)])

  (define-instruction value lea1
    [(op (z ur) (x ur))
     (let ([offset (info-lea-offset info)])
       (if (signed-32? offset)
           `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea1 (info-lea-offset info)) ,x))
           (let ([u (make-tmp 'u)])
             (seq
               `(set! ,(make-live-info) ,u (immediate ,offset))
               `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea2 0) ,x ,u))))))])

  (define-instruction value lea2
    [(op (z ur) (x ur) (y ur))
     (let ([offset (info-lea-offset info)])
       (if (signed-32? offset)
           `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea2 (info-lea-offset info)) ,x ,y))
           (let ([u (make-tmp 'u)])
             (seq
               `(set! ,(make-live-info) ,u (immediate ,offset))
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,u ,y))
               `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea2 0) ,x ,u))))))])

  (define-instruction value (sext8 sext16 sext32 zext8 zext16 zext32)
    [(op (z ur) (x ur mem)) `(set! ,(make-live-info) ,z (asm ,info ,(asm-move/extend op) ,x))])

  (define-instruction value (load)
    (definitions
      (define maybe-swap
        (lambda (info z expr)
          (with-output-language (L15d Effect)
            (if (info-load-swapped? info)
                (seq
                  expr
                  `(set! ,(make-live-info) ,z (asm ,info ,(asm-swap (info-load-type info)) ,z)))
                expr)))))
    [(op (z ur) (x ur) (y ur) (w imm32))
     (maybe-swap info z
       `(set! ,(make-live-info) ,z (asm ,info ,(asm-load (info-load-type info)) ,x ,y ,w)))]
    [(op (z ur) (x ur) (y ur) (w ur))
     (maybe-swap info z
       (if (eq? y %zero)
           `(set! ,(make-live-info) ,z (asm ,info ,(asm-load (info-load-type info)) ,x ,w (immediate 0)))
           (let ([u (make-tmp 'u)])
             (seq
               `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-lea2 0) ,y ,w))
               `(set! ,(make-live-info) ,z (asm ,info ,(asm-load (info-load-type info)) ,x ,u (immediate 0)))))))])

  (define-instruction effect (store)
    (definitions
      (define maybe-swap
        (lambda (info w k)
          (with-output-language (L15d Effect)
            (if (info-load-swapped? info)
                (let ([u (make-tmp 'u)])
                  (seq
                    `(set! ,(make-live-info) ,u ,w)
                    `(set! ,(make-live-info) ,u (asm ,info ,(asm-swap (info-load-type info)) ,u))
                    (k u)))
                (k w))))))
    [(op (x ur) (y ur) (z imm32) (w ur real-imm32))
     (maybe-swap info w
       (lambda (w)
         `(asm ,info ,(asm-store (info-load-type info)) ,x ,y ,z ,w)))]
    [(op (x ur) (y ur) (z ur) (w ur real-imm32))
     (maybe-swap info w
       (lambda (w)
         (if (eq? y %zero)
             `(asm ,info ,(asm-store (info-load-type info)) ,x ,z (immediate 0) ,w)
             (let ([u (make-tmp 'u)])
               (seq
                 `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-lea2 0) ,y ,z))
                 `(asm ,info ,(asm-store (info-load-type info)) ,x ,u (immediate 0) ,w))))))])

  (define-instruction value (load-single->double)
    [(op (x fpur) (y fpmem))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fl-cvt 'single->double) ,y))])

  (define-instruction value (single->double double->single)
    [(op (x fpur) (y fpmem fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fl-cvt op) ,y))])

  (define-instruction effect (store-double->single)
    [(op (x fpmem) (y fpmem fpur))
     (let ([u (make-tmp 'u 'fp)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-fl-cvt 'double->single) ,y))
        `(asm ,info ,asm-store-single ,x ,u)))])

  (define-instruction effect (store-single)
    [(op (x fpmem) (y fpur))
     `(asm ,info ,asm-store-single ,x ,y)])

  (define-instruction value (load-single)
    [(op (x fpur) (y fpmem))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-load-single ,y))])

  (define-instruction value (get-double)
    [(op (z ur) (y fpur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-get-double ,y))])

  (define-instruction value (fpt)
    [(op (x fpur) (y ur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpt ,y))])

  (define-instruction value (fpmove)
    [(op (x fpmem) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove ,y))]
    [(op (x fpur) (y fpmem)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove ,y))]
    [(op (x fpur) (y fpur)) `(set! ,(make-live-info) ,x ,y)])

  (define-instruction value (fpcastto)
    [(op (x mem) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove ,y))]
    [(op (x ur) (y fpmem)) `(set! ,(make-live-info) ,x (asm ,info ,asm-move ,y))]
    [(op (x ur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcast ,y))])

  (define-instruction value (fpcastfrom)
    [(op (x fpmem) (y ur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-move ,y))]
    [(op (x fpur) (y mem)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpmove ,y))]
    [(op (x fpur) (y ur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcast ,y))])

  (define-instruction value (fp+ fp- fp* fp/)
    [(op (x fpur) (y fpur) (z fpmem fpur))
     (seq
      `(move-related ,x ,y)
      `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpop-2 op) ,y ,z)))]
    [(op (x fpur) (y fpmem) (z fpmem fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpop-2 op) ,y ,z))])

  (define-instruction value (fpsqrt)
    [(op (x fpur) (y fpmem fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsqrt ,y))])

  (define-instruction value (fpsingle)
    [(op (x fpur) (y fpmem fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsingle ,y))])

  (define-instruction effect inc-cc-counter
    [(op (x ur) (y imm32 ur) (z imm32 ur)) `(asm ,info ,asm-inc-cc-counter ,x ,y ,z)])

  (define-instruction effect inc-profile-counter 
    [(op (x ur mem) (y imm32 ur)) `(asm ,info ,asm-inc-profile-counter ,x ,y)])

  (define-instruction value (fptrunc)
    [(op (z ur) (x fpmem fpur)) `(set! ,(make-live-info) ,z (asm ,info ,asm-fptrunc ,x))])

  (define-instruction value get-tc
    [(op (z ur))
     ; always used with %rax as the lhs.  we take advantage of this to use z also
     ; as the jump temp (rax is assumed by linker's x86_64_set_jump)
     (safe-assert (eq? z %rax))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-get-tc))])

  (define-instruction value activate-thread
    [(op (z ur))
     (safe-assert (eq? z %rax)) ; see get-tc
     `(set! ,(make-live-info) ,z (asm ,info ,asm-activate-thread))])

  (define-instruction effect deactivate-thread
    [(op)
     `(asm ,info ,asm-deactivate-thread)])

  (define-instruction effect unactivate-thread
    [(op (x ur))
     (safe-assert (eq? x %Carg1))
     `(asm ,info ,asm-unactivate-thread ,x)])

  ; TODO: risc architectures will have to take info-asmlib-save-ra? into account
  (define-instruction value asmlibcall
    [(op (z ur))
     (let ([urax (make-precolored-unspillable 'urax %rax)]) ; rax is assumed by linker's x86_64_set_jump
       (seq
         `(set! ,(make-live-info) ,urax (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,info ,(asm-library-call (info-asmlib-libspec info)) ,urax ,(info-kill*-live*-live* info) ...))))])

  (define-instruction effect asmlibcall!
    [(op)
     (let ([urax (make-precolored-unspillable 'urax %rax)]) ; rax is assumed by linker's x86_64_set_jump
       (seq
         `(set! ,(make-live-info) ,urax (asm ,null-info ,asm-kill))
         `(asm ,info ,(asm-library-call (info-asmlib-libspec info)) ,urax ,(info-kill*-live*-live* info) ...)))])

  (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
  (define-instruction effect (c-simple-call)
    [(op)
     (let ([urax (make-precolored-unspillable 'urax %rax)]) ; rax is assumed by linker's x86_64_set_jump
       (seq
         `(set! ,(make-live-info) ,urax (asm ,null-info ,asm-kill))
         `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info)) ,urax)))])
  
  (define-instruction value pop
    [(op (z ur)) `(set! ,(make-live-info) ,z (asm ,info ,asm-pop))])

  (define-instruction pred (fp= fp< fp<=)
    [(op (x fpmem) (y fpur))
     (let ([info (make-info-condition-code op #t #f)]) ; NB: reversed? flag is assumed to be #t
       (values '() `(asm ,info ,(asm-fp-relop info) ,x ,y)))]
    [(op (x fpur) (y fpur))
     (let ([info (make-info-condition-code op #t #f)]) ; NB: reversed? flag is assumed to be #t
       (values '() `(asm ,info ,(asm-fp-relop info) ,x ,y)))])

  (define-instruction pred (eq? u< < > <= >=)
    ; the idea (following from the intel x86/x86_64 documentation)
    ; is that we want to squeeze this into a CMP that allows one of
    ; the following formats:
    ; CMP r/m, imm
    ; CMP r/m, r
    ; CMP r, r/m
    ; the last format we may want to drop, since it uses a different
    ; format from the one above it, but is interchangeable with it,
    ; if we reverse the operands.
    [(op (x mem) (y ur imm32))
     (let ([info (make-info-condition-code op #f #t)])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))]
    [(op (x ur) (y mem))
     (let ([info (make-info-condition-code op #t #t)])
       (values '() `(asm ,info ,(asm-relop info) ,y ,x)))]
    [(op (x imm32) (y ur mem))
     (let ([info (make-info-condition-code op #t #t)])
       (values '() `(asm ,info ,(asm-relop info) ,y ,x)))]
    [(op (x ur) (y ur imm32))
     (let ([info (make-info-condition-code op #f #t)])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))])

  (define-instruction pred (condition-code)
    [(op) (values '() `(asm ,info ,(asm-condition-code info)))])

  (let ()
    (define imm->imm32
      (lambda (y w k)
        (nanopass-case (L15d Triv) w
          [(immediate ,imm)
           (if (signed-32? imm)
               (k y w)
               (let ([tmp (make-tmp 'u)]
                     [zero (with-output-language (L15d Triv)
                             `(immediate 0))])
                 (with-output-language (L15d Effect)
                   (seq
                    `(set! ,(make-live-info) ,tmp ,w)
                    (if (eq? y %zero)
                        (k tmp zero)
                        (seq
                         `(set! ,(make-live-info) ,tmp (asm ,null-info ,asm-add ,tmp ,y))
                         (k tmp zero)))))))])))

    (let* ([info-cc-eq (make-info-condition-code 'eq? #f #t)]
           [asm-eq (asm-relop info-cc-eq)])
      (define-instruction pred (type-check?)
        [(op (x ur mem) (mask imm32 ur) (type imm32 ur))
         (let ([tmp (make-tmp 'u)])
           (values
            (with-output-language (L15d Effect)
              (seq
               `(set! ,(make-live-info) ,tmp ,x)
               `(set! ,(make-live-info) ,tmp (asm ,null-info ,asm-logand ,tmp ,mask))))
            `(asm ,info-cc-eq ,asm-eq ,tmp ,type)))])

      (define-instruction pred (logtest log!test)
        [(op (x mem) (y ur imm32))
         (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,x ,y))]
        [(op (x ur imm32) (y mem))
         (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,y ,x))]
        [(op (x imm32) (y ur))
         (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,y ,x))]
        [(op (x ur) (y ur imm32))
         (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,x ,y))])

      (define-instruction pred (lock!)
        [(op (x ur) (y ur) (w imm))
         (imm->imm32
          y w
          (lambda (y w)
            (let ([uts (make-precolored-unspillable 'uts %ts)])
              (values
               (nanopass-case (L15d Triv) w
                 [(immediate ,imm)
                  (with-output-language (L15d Effect)
                    (seq
                     `(set! ,(make-live-info) ,uts (immediate 1))
                     `(set! ,(make-live-info) ,uts
                            (asm ,info ,asm-exchange ,uts
                                 (mref ,x ,y ,imm uptr)))))])
               `(asm ,info-cc-eq ,asm-eq ,uts (immediate 0))))))]))

    (define-instruction effect (locked-incr!)
      [(op (x ur) (y ur) (w imm))
       (imm->imm32
        y w
        (lambda (y w)
          `(asm ,info ,asm-locked-incr ,x ,y ,w)))])

    (define-instruction effect (locked-decr!)
      [(op (x ur) (y ur) (w imm))
       (imm->imm32
        y w
        (lambda (y w)
          `(asm ,info ,asm-locked-decr ,x ,y ,w)))])

    (define-instruction effect (cas)
      [(op (x ur) (y ur) (w imm) (old ur) (new ur))
       (imm->imm32
        y w
        (lambda (y w)
          (let ([urax (make-precolored-unspillable 'urax %rax)])
            (with-output-language (L15d Effect)
              (seq
               `(set! ,(make-live-info) ,urax ,old)
               ;; NB: may modify %rax:
               `(asm ,info ,asm-locked-cmpxchg ,x ,y ,w ,urax ,new))))))]))

  (define-instruction effect (pause)
    [(op) `(asm ,info ,asm-pause)])

  (define-instruction effect (debug)
    [(op) `(asm ,info ,asm-debug)])

  (define-instruction value read-performance-monitoring-counter
    [(op (z ur) (x ur mem imm))
     (safe-assert (eq? z %rax))
     (safe-assert (and (info-kill*? info) (memq %rdx (info-kill*-kill* info))))
     (let ([urcx (make-precolored-unspillable 'urcx %rcx)])
       (seq
         `(set! ,(make-live-info) ,urcx ,x)
         `(set! ,(make-live-info) ,z (asm ,info ,asm-read-performance-monitoring-counter ,urcx))))])

  (define-instruction value read-time-stamp-counter
    [(op (z ur))
     (safe-assert (eq? z %rax))
     (safe-assert (and (info-kill*? info) (memq %rdx (info-kill*-kill* info))))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-read-time-stamp-counter))])

  ;; currently returns ECX from CPUID function 1
  (define-instruction value cpuid
    [(op (z ur))
     (safe-assert (eq? z %rax))
     (safe-assert (and (info-kill*? info)
                       (memq %rbx (info-kill*-kill* info))
                       (memq %rcx (info-kill*-kill* info))
                       (memq %rdx (info-kill*-kill* info))))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-cpuid))])

  ; NB: shouldn't need to list (info-kill*-live*-live* info) ... here, since we've already
  ; NB: computed spillable/register live sets
  (define-instruction effect (c-call)
    [(op (x ur mem)) `(asm ,info ,asm-indirect-call ,x ,(info-kill*-live*-live* info) ...)])

  (define-instruction effect (push)
    [(op (x ur)) `(asm ,info ,asm-push ,x)])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect invoke-prelude
   ; align sp on 16-byte boundary, taking into account 8-byte
   ; return address already pushed by caller
    [(op)
     (seq
       `(set! ,(make-live-info) ,%sp (asm ,info ,asm-sub ,%sp (immediate 8)))
       `(set! ,(make-live-info) ,%tc ,%Carg1))])
  )

;;; SECTION 3: assembler
(module asm-module (; required exports
                     asm-move asm-move/extend asm-load asm-store asm-swap asm-library-call asm-library-jump
                     asm-mul asm-muli asm-addop asm-add asm-sub asm-negate asm-sub-negate
                     asm-pop asm-shiftop asm-sll asm-logand asm-lognot
                     asm-logtest asm-fp-relop asm-relop asm-push asm-indirect-jump asm-literal-jump
                     asm-direct-jump asm-return-address asm-jump asm-conditional-jump
                     asm-lea1 asm-lea2 asm-indirect-call asm-condition-code
                     asm-fl-cvt asm-store-single asm-load-single asm-fpt asm-fptrunc asm-div asm-popcount
                     asm-exchange asm-pause asm-debug asm-locked-incr asm-locked-decr asm-locked-cmpxchg
                     asm-fpsqrt asm-fpop-2 asm-fpmove asm-fpcast asm-fpsingle
                     asm-c-simple-call
                     asm-save-flrv asm-restore-flrv asm-return asm-c-return asm-size
                     asm-enter asm-foreign-call asm-foreign-callable
                     asm-inc-profile-counter
                     asm-inc-cc-counter asm-read-time-stamp-counter asm-read-performance-monitoring-counter
                     asm-cpuid
                     ; threaded version specific
                     asm-get-tc asm-activate-thread asm-deactivate-thread asm-unactivate-thread
                     ; machine dependent exports
                     asm-sext-rax->rdx asm-store-single->double asm-kill asm-get-double)

  (define ax-register?
    (case-lambda
      [(x) (record-case x [(reg) r #t] [else #f])]
      [(x reg) (record-case x [(reg) r (eq? r reg)] [else #f])]))

  (define ax-fp-register?
    (lambda (x) (record-case x [(reg) r (eq? 'fp (reg-type r))] [else #f])))

  (define ax-ea-reg-code
    (lambda (ea)
      (record-case ea
        [(reg) r (reg-mdinfo r)]
        [else (sorry! 'ax-ea-reg-code "ea=~s" ea)])))

  (define ax-imm-data
    (lambda (ea)
      (record-case ea
        [(imm) (n) n]
        [else ($oops 'assembler-internal "ax-imm-data ea=~s" ea)])))

  ; define-op sets up assembly op macros--
  ; suffixes are a sub-list of (b w l 1)--
  ; the opcode, the size (byte word, long, quad), and all other expressions
  ; are passed to the specified handler--
  ; for prefix 'p' and each suffix 's' a macro of the form 'ps' is set up--
  ; if no suffix is specified the prefix is defined as a macro
  (define-syntax define-op
    (lambda (x)
      (syntax-case x ()
        [(k prefix (suffix ...) handler e ...)
         (let ([suffix* (datum (suffix ...))])
           (unless (andmap (lambda (x) (memq x '(b w l *))) suffix*)
             (syntax-error x (format "invalid suffix list ~s" suffix*)))
           (with-syntax ([(op ...) (map (lambda (x)
                                          (if (eq? x '*)
                                              (construct-name #'k "asmop-" #'prefix)
                                              (construct-name #'k "asmop-" #'prefix x)))
                                     suffix*)]
                         [(size ...) (map (lambda (x)
                                            (case x [(b) #'byte] [(w) #'word] [(l) #'long] [(*) #'quad]))
                                       suffix*)])
             #'(begin
                 (define-syntax op
                   (syntax-rules ()
                     [(_ mneu arg (... ...))
                      (handler 'mneu 'size e ... arg (... ...))]))
                 ...)))]
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

  (define-op asl  (*) unary-op  #b1101001 #b100) ; shifts by CL
  (define-op lsr  (*) unary-op  #b1101001 #b101) ; shifts by CL
  (define-op asr  (*) unary-op  #b1101001 #b111) ; shifts by CL
  (define-op asli (*) shifti-op #b1100000 #b100)
  (define-op lsri (*) shifti-op #b1100000 #b101)
  (define-op asri (*) shifti-op #b1100000 #b111)

  (define-op addi (#;b *) addi-op   #b100000 #b000)
  (define-op subi (#;b *) addi-op   #b100000 #b101)
  (define-op cmpi (b *) addi-op   #b100000 #b111)

  (define-op ori  (#;b *) logi-op #b001)
  (define-op andi (b *) logi-op #b100)
  (define-op xori (#;b *) logi-op #b110)
  (define-op testi (b *) testi-op #b1111011 #b000)

  (define-op movi (b w l *) movi-op #b000)

  (define-op mov    (b w l *) binary-op #b100010)
  (define-op movsb            mulq-op #b10111110)
  (define-op movsw            mulq-op #b10111111)
  (define-op movsl            quad-op #b01100011)
  (define-op movzb            mulq-op #b10110110)
  (define-op movzw            mulq-op #b10110111)

  (define-op add  (#;b *) binary-op #b000000)
  (define-op or   (#;b *) binary-op #b000010)
  (define-op and  (#;b *) binary-op #b001000)
  (define-op sub  (#;b *) binary-op #b001010)
  (define-op xor  (#;b *) binary-op #b001100)
  (define-op test (#;b *) test-op   #b1000010)
  (define-op cmp  (#;b *) binary-op #b001110)
  (define-op xchg (#;b *) xchg-op   #b1000011)
  (define-op bswap (#;l *) bswap-op)
 
  (define-op divsax (*) unary-op   #b1111011  #b111)
  #;(define-op mulsax (*) unary-op   #b1111011  #b100)
  (define-op muls mulq-op #b10101111)
  (define-op mulsi muliq-op    #b01101001)

  (define-op lea       quad-op     #b10001101)

  (define-op pop      push-op #b01011)
  (define-op push     push-op #b01010)
  (define-op pushi    pushi-op)
  (define-op pushf     byte-op     #b10011100)
  (define-op popf      byte-op     #b10011101)

  #;(define-op nop       byte-op    #b10010000)
  (define-op ret       byte-op    #b11000011)
  (define-op extad quad-byte-op   #b10011001) ; extend rax to rdx

  (define-op rdtsc     two-byte-op     #b1111 #b00110001) ; read time-stamp counter
  (define-op rdpmc     two-byte-op     #b1111 #b00110011) ; read performance monitoring counter
  (define-op pause     two-byte-op #b11110011 #b10010000) ; equivalent to rep nop

  (define-op int3      byte-op     #b11001100)

  (define-op dec (#;b *) unary-op  #b1111111 #b001)
  (define-op inc (#;b *) unary-op  #b1111111 #b000)
  (define-op neg (b *) unary-op  #b1111011 #b011) ; was commented out in x86_64macros
  (define-op not (#;b *) unary-op  #b1111011 #b010)

  (define-op locked-dec (#;b *) locked-unary-op #b1111111 #b001)
  (define-op locked-inc (#;b *) locked-unary-op #b1111111 #b000)

  (define-op locked-cmpxchg (*) locked-cmpxchg-op)

  (define-op popcount (*) popcount-op)

  (define-op cpuid     two-byte-op     #b1111 #b10100010)

  ; also do inc-reg dec-reg

  ; the following are forms of the call instruction and push the return address
  (define-op call      jump-op #b010) ; reg/mem indirect
  #;(define-op bsrl      branch-always-op long #b11101000) ; pc-relative
  (define-op bsr       bsr-op)

  ; the followin<g are forms of the jmp instruction
  (define-op jmp       jump-op #b100) ; reg/mem indirect
  (define-op bra       bra-op)

  (define-op bvs branch-op #b0000) ; jump on overflow
  (define-op bvc branch-op #b0001) ; jump on not overflow
  (define-op bcs branch-op #b0010) ; jump on below (carry set)
  (define-op bcc branch-op #b0011) ; jump on not below (carry clear)
  (define-op beq branch-op #b0100) ; jump on equal
  (define-op bne branch-op #b0101) ; jump on not equal
  (define-op bls branch-op #b0110) ; jump on less or same (below or equal)
  (define-op bhi branch-op #b0111) ; jump on higher (above)
  (define-op blt branch-op #b1100) ; jump on less than
  (define-op bge branch-op #b1101) ; jump on greater than or equal
  (define-op ble branch-op #b1110) ; jump on less than or equal
  (define-op bgt branch-op #b1111) ; jump on greater than

  ; SSE2 instructions
  (define-op sse.addsd     sse-op1 #xF2 #x58 0)
  (define-op sse.andpd     sse-op1 #x66 #x54 0)
  (define-op sse.cvtss2sd  sse-op1 #xF3 #x5A 0)
  (define-op sse.cvtsd2ss  sse-op1 #xF2 #x5A 0)
  (define-op sse.cvttsd2si sse-op1 #xF2 #x2C 1)
  (define-op sse.cvtsi2sd  sse-op1 #xF2 #x2A 1)
  (define-op sse.divsd     sse-op1 #xF2 #x5E 0)
  (define-op sse.movd      sse-op2 #x66 #x6E #x7E 1)
  (define-op sse.movsd     sse-op2 #xF2 #x10 #x11 0)
  (define-op sse.movss     sse-op2 #xF3 #x10 #x11 0)
  (define-op sse.mulsd     sse-op1 #xF2 #x59 0)
  (define-op sse.sqrtsd    sse-op1 #xF2 #x51 0)
  (define-op sse.subsd     sse-op1 #xF2 #x5C 0)
  (define-op sse.ucomisd   sse-op1 #x66 #x2E 0)
  (define-op sse.xorpd     sse-op1 #x66 #x57 0)

  (define sse-op1
    (lambda (op prefix-code op-code w source dest-reg code*)
      (emit-code (op source dest-reg code*)
        (build byte prefix-code)
        (ax-ea-rex w source dest-reg #f)
        (build byte #x0F)
        (build byte op-code)
        (ax-ea-modrm-reg source dest-reg)
        (ax-ea-sib source)
        (ax-ea-addr-disp source))))

  (define sse-op2
    (lambda (op prefix-code dstreg-op-code srcreg-op-code w source dest code*)
      (cond
        [(ax-fp-register? source)
         (emit-code (op source dest code*)
           (build byte prefix-code)
           (ax-ea-rex w dest source #f)
           (build byte #x0F)
           (build byte srcreg-op-code)
           (ax-ea-modrm-reg dest source)
           (ax-ea-sib dest)
           (ax-ea-addr-disp dest))]
        [(ax-fp-register? dest)
         (emit-code (op source dest code*)
           (build byte prefix-code)
           (ax-ea-rex w source dest #f)
           (build byte #x0F)
           (build byte dstreg-op-code)
           (ax-ea-modrm-reg source dest)
           (ax-ea-sib source)
           (ax-ea-addr-disp source))]
        [else
          ($oops 'assembler-internal "sse-op2 source=~s dest=~s" source dest)])))

  (define mulq-op
    ; used for movs/movz as well as mulsq
    (lambda (op op-code source-ea dest-reg code*)
      (emit-code (op source-ea dest-reg code*)
        (ax-ea-rex 1 source-ea dest-reg 'quad)
        (build byte #b00001111)
        (build byte op-code)
        (ax-ea-modrm-reg source-ea dest-reg)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea))))

  (define muliq-op
    (lambda (op op-code imm-data source-ea dest-reg code*)
      (emit-code (op imm-data source-ea dest-reg code*)
        (ax-ea-rex 1 source-ea dest-reg 'quad)
        (build byte op-code)
        (ax-ea-modrm-reg source-ea dest-reg)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea)
        (build long (ax-imm-data imm-data)))))

  (define quad-op
    (lambda (op op-code source-ea dest-reg code*)
      (emit-code (op source-ea dest-reg code*)
        (ax-ea-rex 1 source-ea dest-reg 'quad)
        (build byte op-code)
        (ax-ea-modrm-reg source-ea dest-reg)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea))))

  (define test-op
    (lambda (op size op-code source-ea reg code*)
      (emit-code (op source-ea reg code*)
        (ax-ea-rex (if (eq? size 'quad) 1 0) source-ea reg size)
        (build byte
          (byte-fields
            [1 op-code]
            [0 (ax-size-code size)]))
        (ax-ea-modrm-reg source-ea reg)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea))))

  (define unary-op
    (lambda (op size op-code ttt-code dest-ea code*)
      (emit-code (op dest-ea code*)
        (ax-ea-rex (if (eq? size 'quad) 1 0) dest-ea #f size)
        (build byte
          (byte-fields
            [1 op-code]
            [0 (ax-size-code size)]))
        (ax-ea-modrm-ttt dest-ea ttt-code)
        (ax-ea-sib dest-ea)
        (ax-ea-addr-disp dest-ea))))

  (define locked-unary-op
    (lambda (op size op-code ttt-code dest-ea code*)
      (emit-code (op dest-ea code*)
        (build byte #xf0) ; lock prefix
        (ax-ea-rex (if (eq? size 'quad) 1 0) dest-ea #f size)
        (build byte
          (byte-fields
            [1 op-code]
            [0 (ax-size-code size)]))
        (ax-ea-modrm-ttt dest-ea ttt-code)
        (ax-ea-sib dest-ea)
        (ax-ea-addr-disp dest-ea))))

  (define locked-cmpxchg-op
    (lambda (op size dest-ea new-reg code*)
      (begin
        (emit-code (op dest-ea new-reg code*)
          (build byte #xf0) ; lock prefix
          (ax-ea-rex (if (eq? size 'quad) 1 0) dest-ea new-reg size)
          (build byte #x0f)
          (build byte
            (byte-fields
              [1 #b1011000]
              [0 (ax-size-code size)]))
          (ax-ea-modrm-reg dest-ea new-reg)
          (ax-ea-sib dest-ea)
          (ax-ea-addr-disp dest-ea)))))

  (define pushi-op
    (lambda (op imm-ea code*)
      (if (ax-range? -128 imm-ea 127)
          (emit-code (op imm-ea code*)
            (build byte 106)
            (ax-ea-imm-data 'byte imm-ea))
          (emit-code (op imm-ea code*)
            (build byte 104)
            (ax-ea-imm-data 'long imm-ea)))))

  ; imm-data can be either an (imm n) or else a (literal size addr) record.
  ;
  (define testi-op
    (lambda (op size op-code ttt-code imm-ea dest-ea code*)
      (emit-code (op imm-ea dest-ea code*)
        (ax-ea-rex (if (eq? size 'quad) 1 0) dest-ea #f size)
        (build byte
          (byte-fields
            [1 op-code]
            [0 (ax-size-code size)]))
        (ax-ea-modrm-ttt dest-ea ttt-code)
        (ax-ea-sib dest-ea)
        (ax-ea-addr-disp dest-ea)
        (ax-ea-imm-data size imm-ea))))

  (define logi-op
    (lambda (op size ttt-code imm-ea dest-ea code*)
      (if (and (eq? size 'quad)
               (record-case imm-ea
                 [(imm) (n) (<= -128 n 127)]
                 [else #f]))
          (emit-code (op imm-ea dest-ea code*)
            (ax-ea-rex 1 dest-ea #f size)
            (build byte
              (byte-fields
                [1 #b1000001]
                [0 (ax-size-code size)]))
            (ax-ea-modrm-ttt dest-ea ttt-code)
            (ax-ea-sib dest-ea)
            (ax-ea-addr-disp dest-ea)
            (ax-ea-imm-data 'byte imm-ea))
          (emit-code (op imm-ea dest-ea code*)
            (ax-ea-rex 1 dest-ea #f size)
            (build byte
              (byte-fields
                [1 #b1000000]
                [0 (ax-size-code size)]))
            (ax-ea-modrm-ttt dest-ea ttt-code)
            (ax-ea-sib dest-ea)
            (ax-ea-addr-disp dest-ea)
            (ax-ea-imm-data size imm-ea)))))

  (define addi-op
    (lambda (op size op-code ttt-code imm-ea dest-ea code*)
      (if (and (eq? size 'quad)
               (record-case imm-ea
                 [(imm) (n) (<= -128 n 127)]
                 [else #f]))
          (emit-code (op imm-ea dest-ea code*)
            (ax-ea-rex 1 dest-ea #f size)
            (build byte
              (byte-fields
                [2 op-code]
                [1 1]
                [0 (ax-size-code size)]))
            (ax-ea-modrm-ttt dest-ea ttt-code)
            (ax-ea-sib dest-ea)
            (ax-ea-addr-disp dest-ea)
            (ax-ea-imm-data 'byte imm-ea))
          (emit-code (op imm-ea dest-ea code*)
            (ax-ea-rex (if (eq? size 'quad) 1 0) dest-ea #f size)
            (build byte
              (byte-fields
                [2 op-code]
                [1 0]
                [0 (ax-size-code size)]))
            (ax-ea-modrm-ttt dest-ea ttt-code)
            (ax-ea-sib dest-ea)
            (ax-ea-addr-disp dest-ea)
            (ax-ea-imm-data size imm-ea)))))

  (define movi-op
    (lambda (op size ttt-code imm-ea dest-ea code*)
      (cond
        [(and (eq? size 'quad)
              (record-case dest-ea [(reg) stuff #t] [else #f])
              (not (ax-range? #x-80000000 imm-ea #x7fffffff)))
         (emit-code (op imm-ea dest-ea code*)
           (ax-ea-rex 1 dest-ea #f 'quad)
           (build byte
             (byte-fields
               [4 #b1011]
               [3 (ax-size-code size)]
               [0 (fxlogand (ax-ea-reg-code dest-ea) 7)]))
           (ax-ea-imm-data 'full-quad imm-ea))]
        [else
          (emit-code (op imm-ea dest-ea code*)
            (and (eq? size 'word) (build byte 102))
            (ax-ea-rex (if (eq? size 'quad) 1 0) dest-ea #f size)
            (build byte
              (byte-fields
                [1 #b1100011]
                [0 (ax-size-code size)]))
            (ax-ea-modrm-ttt dest-ea ttt-code)
            (ax-ea-sib dest-ea)
            (ax-ea-addr-disp dest-ea)
            (ax-ea-imm-data size imm-ea))])))

  ;;; always need byte immediate data for shift ops
  (define shifti-op
    (lambda (op size op-code ttt-code imm-ea dest-ea code*)
      (emit-code (op imm-ea dest-ea code*)
        (ax-ea-rex (if (eq? size 'quad) 1 0) dest-ea #f size)
        (build byte
          (byte-fields
            [1 op-code]
            [0 (ax-size-code size)]))
        (ax-ea-modrm-ttt dest-ea ttt-code)
        (ax-ea-sib dest-ea)
        (ax-ea-addr-disp dest-ea)
        (ax-ea-imm-data 'byte imm-ea))))

  (define binary-op
    (lambda (op size op-code source dest code*)
      (cond
        [(ax-register? source)
         (emit-code (op source dest code*)
           (and (eq? size 'word) (build byte 102))
           (ax-ea-rex (if (eq? size 'quad) 1 0) dest source size)
           (build byte
             (byte-fields
               [2 op-code]
               [1 0]
               [0 (ax-size-code size)]))
           (ax-ea-modrm-reg dest source)
           (ax-ea-sib dest)
           (ax-ea-addr-disp dest))]
        [(ax-register? dest)
         (emit-code (op source dest code*)
           (and (eq? size 'word) (build byte 102))
           (ax-ea-rex (if (eq? size 'quad) 1 0) source dest size)
           (build byte
             (byte-fields
               [2 op-code]
               [1 1]
               [0 (ax-size-code size)]))
           (ax-ea-modrm-reg source dest)
           (ax-ea-sib source)
           (ax-ea-addr-disp source))]
        [else
          ($oops 'assembler-internal "binary-op source=~s dest=~s" source dest)])))

  (define xchg-op
    (lambda (op size op-code source dest code*)
      (cond
        [(ax-register? source)
         (emit-code (op source dest code*)
           (ax-ea-rex (if (eq? size 'quad) 1 0) dest source size)
           (build byte
             (byte-fields
               [1 op-code]
               [0 (ax-size-code size)]))
           (ax-ea-modrm-reg dest source)
           (ax-ea-sib dest)
           (ax-ea-addr-disp dest))]
        [(ax-register? dest)
         (emit-code (op source dest code*)
           (ax-ea-rex (if (eq? size 'quad) 1 0) source dest size)
           (build byte
             (byte-fields
               [1 op-code]
               [0 (ax-size-code size)]))
           (ax-ea-modrm-reg source dest)
           (ax-ea-sib source)
           (ax-ea-addr-disp source))]
        [else
          ($oops 'assembler-internal "xchg-op source=~s dest=~s" source dest)])))

  (define branch-op
    (lambda (op condition-code disp code*)
      (record-case disp
        [(label) (offset l)
                 (if (and (fixnum? offset) (fx<= -128 offset 127))
                     (emit-code (op disp code*)
                       (build byte
                         (byte-fields
                           [4 7]
                           [0 condition-code]))
                       (build byte offset))
                     (emit-code (op disp code*)
                       (build byte 15)
                       (build byte
                         (byte-fields
                           [4 8]
                           [0 condition-code]))
                       (build long offset)))]
        [else
          (emit-code (op disp code*)
            (build byte 15)
            (build byte
              (byte-fields
                [4 8]
                [0 condition-code]))
            (ax-ea-branch-disp disp))])))

  (define jump-op
    (lambda (op ttt-code dest-ea code*)
      (emit-code (op dest-ea code*)
        (ax-ea-rex 0 dest-ea #f 'quad)
        (build byte 255)
        (ax-ea-modrm-ttt dest-ea ttt-code)
        (ax-ea-sib dest-ea)
        (ax-ea-addr-disp dest-ea))))

  (define bra-op
    (lambda (op disp code*)
      (record-case disp
        [(label) (offset l)
                 (if (and (fixnum? offset) (fx<= -128 offset 127))
                     (emit-code (op disp code*)
                       (build byte #b11101011)
                       (build byte offset))
                     (emit-code (op disp code*)
                       (build byte #b11101001)
                       (build long offset)))]
        [else 
          (emit-code (op disp code*)
            (build byte #b11101001)
            (ax-ea-branch-disp disp))])))

  ;; TODO: not useful on x86_64?
  (define bsr-op
    (lambda (op disp code*)
      (emit-code (op disp code*)
        (build byte #b11101000)
        (if (pair? disp)
            (ax-ea-branch-disp disp)
            (build long disp)))))

  (define byte-op
    (lambda (op op-code code*)
      (emit-code (op code*)
        (build byte op-code))))

  (define two-byte-op
    (lambda (op op-code1 op-code2 code*)
      (emit-code (op code*)
        (build byte op-code1)
        (build byte op-code2))))

  (define bswap-op
    (lambda (op size reg code*)
      (begin
        (unless (ax-register? reg)
          ($oops 'assembler-internal "(bswap-op) ~s is not a real register" reg))
        (emit-code (op reg code*)
          (ax-ea-rex (if (eq? size 'quad) 1 0) reg #f size)
          (build byte #b00001111)
          (build byte
            (byte-fields
              [3 #b11001]
              [0 (fxlogand (ax-ea-reg-code reg) 7)]))))))

  (define quad-byte-op
    (lambda (op op-code code*)
      (emit-code (op code*)
        (build byte #x48) ; rex prefix w/rex.w bit set
        (build byte op-code))))

  (define push-op
    (lambda (op op-code reg code*)
      (begin
        (unless (ax-register? reg)
          ($oops 'assembler-internal "(push-op) ~s is not a real register" reg))
        (emit-code (op reg code*)
          (ax-ea-rex 0 reg #f 'quad)
          (build byte
            (byte-fields
              [3 op-code]
              [0 (fxlogand (ax-ea-reg-code reg) 7)]))))))

  ;; Direct POPCNT instruction variant:
  #;
  (define popcount-op
    (lambda (op size dest-reg src-ea inline? code*)
      (let ([code* (emit-code (op src-ea dest-reg code*)
                     (build byte #xF3)
                     (ax-ea-rex (if (eq? size 'quad) 1 0) src-ea dest-reg size)
                     (build byte #x0F)
                     (build byte #xB8)
                     (ax-ea-modrm-reg src-ea dest-reg)
                     (ax-ea-sib src-ea)
                     (ax-ea-addr-disp src-ea))])
        (if (not (and (ax-register? src-ea)
                      (fx= (ax-ea-reg-code src-ea)
                           (ax-ea-reg-code dest-reg))))
            (emit xor dest-reg dest-reg code*) ; avoid false dependency
            code*))))

  ;; Link-editable variant:
  (define popcount-op
    (let ([target `(x86_64-popcount ,(constant code-data-disp) (library ,(lookup-libspec popcount-slow)))])
      (lambda (op size dest-rax src-rcx inline? code*)
        (safe-assert (and (ax-register? dest-rax) (ax-register? src-rcx)))
        (record-case dest-rax
          [(reg) dest-rax
           (record-case src-rcx
            [(reg) src-rcx
             (safe-assert (and (eq? dest-rax %rax) (eq? src-rcx %rcx)))
             (cond
              [(not inline?)
               ;; Set up a call to `popcount-slow`, which the linker
               ;; can replace with a POPCNT instruction:
               (asm-helper-call code* target dest-rax)]
              [else
               ;; Used for the body of `popcount-slow`.
               ;; This is the sequence generated by LLVM's __builtin_popcountl(),
               ;; but with pushes and pops to save used registers other than the
               ;; result register %rax.
               (emit-literal-code (op dest-rax src-rcx code*)
                 51              ; pushq   %rcx
                 57              ; pushq   %rdi
                 48 89 c8        ; movq    %rcx, %rax
                 48 d1 e8        ; shrq    %rax
                 48 bf 55 55 55 55 55 55 55 55 ; movabsq $6148914691236517205, %rdi
                 48 21 c7        ; andq    %rax, %rdi
                 48 29 f9        ; subq    %rdi, %rcx
                 48 b8 33 33 33 33 33 33 33 33 ; movabsq $3689348814741910323, %rax
                 48 89 cf        ; movq    %rcx, %rdi
                 48 21 c7        ; andq    %rax, %rdi
                 48 c1 e9 02     ; shrq    $2, %rcx
                 48 21 c1        ; andq    %rax, %rcx
                 48 01 f9        ; addq    %rdi, %rcx
                 48 89 c8        ; movq    %rcx, %rax
                 48 c1 e8 04     ; shrq    $4, %rax
                 48 8d 04 08     ; leaq    (%rax,%rcx), %rax
                 48 bf 0f 0f 0f 0f 0f 0f 0f 0f ; movabsq $1085102592571150095, %rdi
                 48 21 c7        ; andq    %rax, %rdi
                 48 b8 01 01 01 01 01 01 01 01 ; movabsq $72340172838076673, %rax
                 48 0f af c7     ; imulq   %rdi, %rax
                 48 c1 e8 38     ; shrq    $56, %rax
                 5f              ; popq    %rdi
                 59)])])]))))    ; popq    %rcx

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
         (build-maybe-cons* #'(chunk ...)
           #'(aop-cons* `(asm ,op ,opnd ...) ?code*))])))

  (define-syntax emit-literal-code
    (lambda (x)
      (syntax-case x ()
        [(_ (op opnd ... ?code*) hexlike ...)
         #`(emit-code (op opnd ... ?code*) (encode-hex-like hexlike) ...)])))

  (define-syntax encode-hex-like
    (lambda (x)
      (syntax-case x ()
        [(k hexlike)
         (let ([n (let ([v (syntax->datum #'hexlike)])
                    (if (number? v)
                        ;; parsed as decimal; reparse as hex
                        (string->number (number->string v) 16)
                        ;; parsed as ymbol
                        (string->number (symbol->string v) 16)))])
           (with-syntax ([n (datum->syntax #'k n)])
             #`(build byte n)))])))

  (define-who ax-size-code
    (lambda (x)
      (case x
        [(byte) 0]
        [(word) 1]
        [(long) 1]
        [(quad) 1]
        [else (sorry! who "invalid size ~s" x)])))

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
       (fx+ (fxsll e n) ...)]))

  (define ax-ea-rex
    (lambda (w ea maybe-reg size)
      (define (rex-required? x)
        ; rex prefix is required to access lsb of RSP, RBP, RSI, RDI
        ; (and R8-R15, but they require rex prefix anyway)
        (and (eq? size 'byte)
             (record-case x
               [(reg) r (fx<= 4 (reg-mdinfo r) 7)]
               [else #f])))
      (define (rex.r)
        (if maybe-reg
            (record-case maybe-reg
              [(reg) r (fxsrl (reg-mdinfo r) 3)]
              [else ($oops 'assembler-internal "maybe-reg=~s" maybe-reg)])
            0))
      (define (build-rex x b)
        (let ([b (build byte
                   (byte-fields
                     [4 #b0100]
                     [3 w]
                     [2 (rex.r)]
                     [1 x]
                     [0 b]))])
          (and (or (not (fx= (cdr b) #x40))
                   (rex-required? ea)
                   (and maybe-reg (rex-required? maybe-reg)))
               b)))
      (record-case ea
        [(index) (size index-reg base-reg)
         (build-rex
           (fxsrl (reg-mdinfo index-reg) 3)
           (fxsrl (reg-mdinfo base-reg) 3))]
        [(riprel) stuff (build-rex 0 0)]
        [(disp) (size reg) (build-rex 0 (fxsrl (reg-mdinfo reg) 3))]
        [(reg) r (build-rex 0 (fxsrl (reg-mdinfo r) 3))]
        [else ($oops 'assembler-internal "ax-ea-rex ea ~s" ea)])))

  (define ax-ea-addr-disp
    (lambda (dest-ea)
      (record-case dest-ea
        [(index) (size index-reg base-reg)
         (cond
           [(and (fxzero? size) (not (eq? base-reg %rbp)) (not (eq? base-reg %r13))) #f]
           [(ax-byte-size? size) (build byte size)]
           [else (build long size)])]
        [(riprel) (disp) (build long disp)]
        [(disp) (size reg)
         (cond
           [(and (fxzero? size) (not (eq? reg %rbp)) (not (eq? reg %r13))) #f] ; indirect
           [(ax-byte-size? size) (build byte size)]
           [else (build long size)])]
        [(reg) r #f]
        [else ($oops 'assembler-internal "ax-ea-addr-disp dest-ea=~s" dest-ea)])))

  (define ax-ea-sib
    (let ([ax-ss-index-base
           (lambda (index-reg base-reg)
             (build byte
               (byte-fields
                 [6 #b00]          ; 2 bits, scaled by bytes.
                 [3 index-reg]     ; 3 bits, index register.
                 [0 base-reg])))]) ; 3 bits, base register.
      (lambda (dest-ea)
        (record-case dest-ea
          [(index) (size index-reg base-reg)
           (ax-ss-index-base (fxlogand (reg-mdinfo index-reg) 7)
             (fxlogand (reg-mdinfo base-reg) 7))]
          [(riprel) stuff #f]
          [(disp) (size reg)
           (and (or (eq? reg %sp) (eq? reg %r12))
                (ax-ss-index-base #b100 #b100))]
          [(reg) r #f]
          [else ($oops 'assembler-internal "ax-ea-sib dest-ea=~s" dest-ea)]))))

  (define ax-ea-modrm-reg
    (lambda (dest-ea reg)
      (ax-ea-modrm-ttt dest-ea (fxlogand (ax-ea-reg-code reg) 7))))

  (define ax-ea-modrm-ttt
    (letrec
      ([ax-mod-ttt-r/m
        (lambda (mod ttt r/m)
          (build byte
            (byte-fields
              [6 mod]     ; 2 bits
              [3 ttt]     ; 3 bits
              [0 r/m])))] ; 3 bits
       [ax-r/m ; 3 bits
        (lambda (dest-ea)
          (record-case dest-ea
            [(index) (size index-reg base-reg) #b100]
            [(riprel) stuff #b101]
            [(disp) (size reg) (fxlogand (reg-mdinfo reg) 7)]
            [(reg) r (fxlogand (reg-mdinfo r) 7)]
            [else ($oops 'assembler-internal
                    "ax-r/m dest-ea=~s" dest-ea)]))]
       [ax-mod ; 2 bits
        (lambda (dest-ea)
          (record-case dest-ea
            [(index) (size index-reg base-reg)
             (cond
               [(and (fxzero? size) (not (eq? base-reg %rbp)) (not (eq? base-reg %r13))) #b00]
               [(ax-byte-size? size) #b01]
               [else #b10])]
            [(riprel) stuff #b00]
            [(disp) (size reg)
             (cond
               [(and (fxzero? size) (not (eq? reg %rbp)) (not (eq? reg %r13))) #b00] ; indirect
               [(ax-byte-size? size) #b01]
               [else #b10])]
            [(reg) r #b11]
            [else ($oops 'assembler-internal "ax-mod dest-ea ~s" dest-ea)]))])
      (lambda (dest-ea ttt)
        (ax-mod-ttt-r/m (ax-mod dest-ea) ttt (ax-r/m dest-ea)))))

  (define ax-ea-imm-data
    (lambda (size imm-data)
      (case size
        [(full-quad)
         (record-case imm-data
           [(literal) stuff (cons 'abs stuff)]
           [(funcrel) stuff (cons 'funcrel (ax-ea-imm-data 'quad stuff))] ; added, not sure if this works for x86_64
           [(imm) (n) (cons 'quad n)]
           [else ($oops 'assembler-internal
                   "ax-ea-imm-data size=~s imm-data=~s" size imm-data)])]
        [else
         (record-case imm-data
           [(imm) (n) (cons (if (eq? size 'quad) 'long size) n)]
           [else ($oops 'assembler-internal
                   "ax-ea-imm-data size=~s imm-data=~s" size imm-data)])])))

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

  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm x86_64-jump x86_64-call x86_64-popcount) 0]
        [(byte) 1]
        [(word) 2]
        [(long) 4]
        [else 8])))

  (define-who asm-move
    (lambda (code* dest src)
      (Trivit (dest src)
        (record-case src
          [(imm) (n)
                 (if (and (eqv? n 0) (record-case dest [(reg) r #t] [else #f]))
                     (emit xor dest dest code*)
                     (emit movi src dest code*))]
          [(literal) stuff (emit movi src dest code*)]
          [else (emit mov src dest code*)]))))

  (define-who asm-move/extend
    (lambda (op)
      (lambda (code* dest src)
        (Trivit (dest src)
          (case op
            [(sext8) (emit movsb src dest code*)]
            [(sext16) (emit movsw src dest code*)]
            [(sext32) (emit movsl src dest code*)]
            [(zext8) (emit movzb src dest code*)]
            [(zext16) (emit movzw src dest code*)]
            [(zext32) (emit movl src dest code*)]
            [else (sorry! who "unexpected op ~s" op)])))))

  (define asm-fl-cvt
    (lambda (op)
      (lambda (code* dest-reg src)
        (Trivit (src)
          (case op
            [(single->double) (emit sse.cvtss2sd src (cons 'reg dest-reg) code*)]
            [(double->single) (emit sse.cvtsd2ss src (cons 'reg dest-reg) code*)])))))

  (define asm-fpsingle
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit sse.cvtsd2ss src dest
          (emit sse.cvtss2sd dest dest code*)))))

  (define asm-store-single->double
    (lambda (flreg)
      (lambda (code* base index offset)
        (let ([dest (build-mem-opnd base index offset)] [flreg (cons 'reg flreg)])
          (emit sse.cvtss2sd flreg flreg
            (emit sse.movsd flreg dest code*))))))

  (define asm-store-single
    (lambda (code* dest flreg)
      (Trivit (dest)
        (emit sse.movss (cons 'reg flreg) dest code*))))

  (define asm-load-single
    (lambda (code* flreg src)
      (Trivit (src)
        (emit sse.movss src (cons 'reg flreg) code*))))

  (define asm-get-double
    (lambda (code* dst flreg)
      (emit sse.movd (cons 'reg flreg) (cons 'reg dst) code*)))

  (define asm-fpt
    (lambda (code* dest src)
      (Trivit (dest src)
         (emit sse.cvtsi2sd src dest code*))))

  (define asm-fpop-2
    (lambda (op)
      (lambda (code* dest-reg src1 src2)
        (define (emit-it src dest code*)
          (case op
            [(fp+) (emit sse.addsd src dest code*)]
            [(fp-) (emit sse.subsd src dest code*)]
            [(fp*) (emit sse.mulsd src dest code*)]
            [(fp/) (emit sse.divsd src dest code*)]))
        (cond
          [(eq? dest-reg src1)
           (Trivit (dest-reg src2)
             (emit-it src2 dest-reg code*))]
          [(eq? dest-reg src2)
           (if (memq op '(fp+ fp*))
               (Trivit (dest-reg src1)
                 (emit-it src1 dest-reg code*))
               ;; Assuming that any subtraction or division will be
               ;; done before we try to fill C arguments...
               (Trivit (dest-reg src1 src2)
                 (emit sse.movsd src2 (cons 'reg %Cfparg1)
                   (emit sse.movsd src1 dest-reg
                         (emit-it (cons 'reg %Cfparg1) dest-reg code*)))))]
          [else
           (Trivit (dest-reg src1 src2)
             (if (equal? src1 src2)
                 ;; avoid redundant load
                 (emit sse.movsd src1 dest-reg
                       (emit-it dest-reg dest-reg code*))
                 (emit sse.movsd src1 dest-reg
                       (emit-it src2 dest-reg code*))))]))))

  (define asm-fpsqrt
    (lambda (code* dest-reg src)
      (Trivit (dest-reg src)
        (emit sse.sqrtsd src dest-reg code*))))

  (define asm-fpmove
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit sse.movsd src dest code*))))

  (define asm-fpcast
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit sse.movd src dest code*))))

  (define asm-fptrunc
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit sse.cvttsd2si src dest code*))))

  (define asm-load
    (lambda (type)
      (lambda (code* dest base index offset)
        (Trivit (dest)
          (let ([src (build-mem-opnd base index offset)])
            (case type
              [(integer-64 unsigned-64) (emit mov src dest code*)]
              [(integer-32) (emit movsl src dest code*)]
              [(unsigned-32) (emit movl src dest code*)] ; clears upper 32 bits of destination register
              [(integer-16) (emit movsw src dest code*)]
              [(unsigned-16) (emit movzw src dest code*)]
              [(integer-8) (emit movsb src dest code*)]
              [(unsigned-8) (emit movzb src dest code*)]
              [else (sorry! 'asm-load "unexpected mref type ~s" type)]))))))

  (define asm-store
    (lambda (type)
      (lambda (code* base index offset src)
        (define imm8 (lambda (n) `(imm ,(modulo n #x100))))
        (define imm16 (lambda (n) `(imm ,(modulo n #x10000))))
        (Trivit (src)
          (let ([dest (build-mem-opnd base index offset)])
            (record-case src
              [(imm) (n)
               (case type
                 [(integer-64 unsigned-64) (emit movi src dest code*)]
                 [(integer-32 unsigned-32) (emit movil src dest code*)]
                 [(integer-16 unsigned-16) (emit moviw (imm16 n) dest code*)]
                 [(integer-8 unsigned-8) (emit movib (imm8 n) dest code*)]
                 [else (sorry! 'asm-store "unexpected mset! type ~s" type)])]
              [(literal) stuff
               (case type
                 [(integer-64 unsigned-64) (emit movi src dest code*)]
                 [(integer-32 unsigned-32) (emit movil src dest code*)]
                 [(integer-16 unsigned-16) (emit moviw src dest code*)]
                 [(integer-8 unsigned-8) (emit movib src dest code*)]
                 [else (sorry! 'asm-store "unexpected mset! type ~s" type)])]
              [else
                (case type
                  [(integer-64 unsigned-64) (emit mov src dest code*)]
                  [(integer-32 unsigned-32) (emit movl src dest code*)]
                  [(integer-16 unsigned-16) (emit movw src dest code*)]
                  [(integer-8 unsigned-8) (emit movb src dest code*)]
                  [else (sorry! 'asm-store "unexpected mset! type ~s" type)])]))))))

  (define asm-swap
    (lambda (type)
      (lambda (code* dest src)
        (Trivit (dest)
          (safe-assert (equal? (Triv->rand src) dest))
          (emit bswap dest
            (case type
              [(integer-16) (emit asri '(imm 48) dest code*)]
              [(unsigned-16) (emit lsri '(imm 48) dest code*)]
              [(integer-32) (emit asri '(imm 32) dest code*)]
              [(unsigned-32) (emit lsri '(imm 32) dest code*)]
              [(integer-64 unsigned-64) code*]
              [else ($oops 'assembler-internal "unexpected asm-swap type argument ~s" type)]))))))

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (emit muls src1 dest code*))))

  (define asm-div
    (lambda (code* dest-rax src-rax src-rdx src2)
      (Trivit (src2)
        (safe-assert (and (eq? dest-rax %rax) (eq? src-rax %rax) (eq? src-rdx %rdx)))
        (emit divsax src2 code*))))

  (define asm-sext-rax->rdx
    (lambda (code* dest-rdx src-rax)
      (safe-assert (and (eq? dest-rdx %rdx) (eq? src-rax %rax)))
      (emit extad code*)))

  (define asm-muli
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit mulsi src1 src0 dest code*))))

  (define asm-popcount
    (lambda (inline?)
      (lambda (code* dest src)
        (Trivit (dest src)
          (emit popcount dest src inline? code*)))))

  (define-who asm-addop
    (lambda (op)
      (case op
        [(+) asm-add]
        [(logand) asm-logand]
        [(logor) asm-logor]
        [(logxor) asm-logxor]
        [else ($oops who "unsupported op ~s" op)])))

  (define asm-add
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit addi src1 dest code*)]
          [else (emit add src1 dest code*)]))))

  (define asm-read-performance-monitoring-counter
    (lambda (code* dest src)
      ; rdx is an implied dest and included in info's kill list
      (safe-assert (eq? dest %rax))
      (safe-assert (eq? src %rcx))
      (emit rdpmc code*)))

  (define asm-read-time-stamp-counter
    (lambda (code* dest)
      ; rdx is an implied dest and included in info's kill list
      (safe-assert (eq? dest %rax))
      (emit rdtsc code*)))

  (define asm-cpuid
    (lambda (code* dest)
      ; rbx/rcx/rdx is an implied dest and included in info's kill list
      (safe-assert (eq? dest %rax))
      (emit movi '(imm 1) (cons 'reg %rax)
        (emit cpuid
          (emit mov (cons 'reg %rcx) (cons 'reg %rax)
             code*)))))

  (define asm-inc-profile-counter
    (lambda (code* dest src)
      (Trivit (dest src)
        (record-case src
          [(imm) (n) (if (eqv? n 1) (emit inc dest code*) (emit addi src dest code*))]
          [else (emit add src dest code*)]))))

  (define-who asm-inc-cc-counter
    (lambda (code* base offset val)
      (let ([dest (nanopass-case (L16 Triv) offset
                    [(immediate ,imm) `(disp ,imm ,base)]
                    [,x `(index 0 ,x ,base)]
                    [else ($oops who "unexpected increment offset ~s" offset)])])
        (nanopass-case (L16 Triv) val
          [(immediate ,imm)
           (if (fx= imm 1)
               (emit inc dest code*)
               (emit addi `(imm ,imm) dest code*))]
          [,x (emit add (cons 'reg x) dest code*)]
          [else ($oops who "unsupported increment ~s" val)]))))

  (define asm-sub
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit subi src1 dest code*)]
          [else (emit sub src1 dest code*)]))))

  (define asm-negate
    (lambda (code* dest src)
      (Trivit (dest)
        (safe-assert (equal? (Triv->rand src) dest))
        (emit neg dest code*))))

  (define asm-sub-negate
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (let ([code* (emit neg dest code*)])
          (record-case src1
            [(imm literal) stuff (emit subi src1 dest code*)]
            [else (emit sub src1 dest code*)])))))

  (define asm-pop
    (lambda (code* dest)
      (Trivit (dest)
        (emit pop dest code*))))

  (define asm-return
    (lambda ()
      (emit addi '(imm 8) (cons 'reg %sp)
        (emit ret '()))))

  (define asm-c-return
    (lambda (info)
      (emit ret '())))

  (define asm-locked-incr
    (lambda (code* base index offset)
      (let ([dest (build-mem-opnd base index offset)])
        (emit locked-inc dest code*))))

  (define asm-locked-decr
    (lambda (code* base index offset)
      (let ([dest (build-mem-opnd base index offset)])
        (emit locked-dec dest code*))))

  (define asm-locked-cmpxchg
    (lambda (code* base index offset old-v new-v)
      (let ([dest (build-mem-opnd base index offset)])
        (emit locked-cmpxchg dest (cons 'reg new-v) code*))))

  (define asm-pause
    (lambda (code*)
      (emit pause code*)))

  (define asm-debug
    (lambda (code*)
      (emit int3 code*)))

  (define asm-exchange
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (emit xchg src1 dest code*))))

  (define-who asm-shiftop
    (lambda (op)
      (case op
        [(sll) asm-sll]
        [(srl) asm-srl]
        [(sra) asm-sra]
        [else ($oops who "unsupported op ~s" op)])))

  (define asm-sll
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit asli src1 dest code*)]
          [else
            (safe-assert (ax-register? src1 %rcx))
            (emit asl dest code*)]))))

  (define asm-srl
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit lsri src1 dest code*)]
          [else
            (safe-assert (ax-register? src1 %rcx))
            (emit lsr dest code*)]))))

  (define asm-sra
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit asri src1 dest code*)]
          [else
            (safe-assert (ax-register? src1 %rcx))
            (emit asr dest code*)]))))

  (define asm-logand
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit andi src1 dest code*)]
          [else (emit and src1 dest code*)]))))

  (define asm-logor
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit ori src1 dest code*)]
          [else (emit or src1 dest code*)]))))

  (define asm-logxor
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit xori src1 dest code*)]
          [else (emit xor src1 dest code*)]))))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest)
        (safe-assert (equal? (Triv->rand src) dest))
        (emit not dest code*))))

  (define asm-lea1
    (lambda (offset)
      (rec asm-lea1-internal
        (lambda (code* dest src)
          (if (eq? src dest)
              (Trivit (dest)
                (emit addi `(imm ,offset) dest code*))
              (Trivit (dest)
                (emit lea `(disp ,offset ,src) dest code*)))))))

  (define asm-lea2
    (lambda (offset)
      (rec asm-lea2-internal
        (lambda (code* dest src1 src2)
          (cond
            [(and (eq? src1 dest) (fx= offset 0))
             (Trivit (dest src2)
               (emit add src2 dest code*))]
            [(and (eq? src2 dest) (fx= offset 0))
             (Trivit (dest src1)
               (emit add src1 dest code*))]
            [else
              (Trivit (dest)
                (emit lea `(index ,offset ,src1 ,src2)
                  dest code*))])))))

  (define asm-logtest
    (lambda (i? info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
          (safe-assert
            (record-case x
              [(disp reg index literal@) stuff #t]
              [else #f]))
          (values
            (record-case y
              [(imm) (n)
               (if (and (fixnum? n)
                        (fx= (fxlogand n #xff) n)
                        (record-case x
                          [(reg) r #t]
                          ; counting on little-endian byte order
                          [(disp index) stuff #t]))
                   (emit testib y x '())
                   (emit testi y x '()))]
              [(literal) stuff (emit testi y x '())]
              [else (emit test x y '())])
            (let-values ([(l1 l2) (if i? (values l2 l1) (values l1 l2))])
              (asm-conditional-jump info l2 l1 offset)))))))

  (define asm-fp-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (values
          (Trivit (x y)
            (emit sse.ucomisd x y '()))
          (asm-conditional-jump info l1 l2 offset)))))

  (define asm-relop
    (lambda (info)
      (rec asm-relop-internal
        (lambda (l1 l2 offset x y)
          (Trivit (x y)
            (safe-assert
              (record-case x
                [(reg disp index) ignore #t]
                [else #f]))
            (values
              (record-case y
                [(imm literal) stuff (emit cmpi y x '())]
                [else (emit cmp y x '())])
              (asm-conditional-jump info l1 l2 offset)))))))

  (define asm-condition-code
    (lambda (info)
      (rec asm-check-flag-internal
        (lambda (l1 l2 offset)
          (values '() (asm-conditional-jump info l1 l2 offset))))))

  ; TODO: should this also handle pushil?
  (define asm-push
    (lambda (code* x)
      (Trivit (x)
        (emit push x code*))))

  (define asm-save-flrv
    (lambda (code*)
      ; save float return value in case we're calling into C
      ; from the foreign-procedure return path to allocate a flonum
      ; return value.  need 16 bytes rather than 8 to maintain
      ; 16-byte stack alignment
      (emit subi '(imm 16) (cons 'reg %sp)
        (emit sse.movsd (cons 'reg %Cfpretval) `(disp 0 ,%sp) code*))))

  (define asm-restore-flrv
    (lambda (code*)
      (emit sse.movsd `(disp 0 ,%sp) (cons 'reg %Cfpretval)
        (emit addi '(imm 16) (cons 'reg %sp) code*))))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
        `(x86_64-jump ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec)
      (let ([target `(x86_64-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* jmp-reg . ignore)
            (asm-helper-call code* target jmp-reg))))))

  (define asm-c-simple-call
    (lambda (entry)
      (rec asm-c-simple-call-internal
        (lambda (code* jmp-reg)
          (let ([sp-opnd (cons 'reg %sp)])
            (if (fx= entry (lookup-c-entry Sreturn))
                ; pretend S_generic_invoke called Sreturn directly by wiping out
                ; stack space added by invoke-prelude and jumping rather than calling
                (emit addi '(imm 8) sp-opnd
                  (asm-helper-jump code* `(x86_64-jump 0 (entry ,entry))))
                (let ([target `(x86_64-call 0 (entry ,entry))])
                  (if-feature windows
                    ; must leave room for callee to store argument registers,
                    ; even if there are no arguments
                    (emit subi '(imm 32) sp-opnd
                      (asm-helper-call (emit addi '(imm 32) sp-opnd code*) target jmp-reg))
                    (asm-helper-call code* target jmp-reg)))))))))

  (define asm-get-tc
    (let ([target `(x86_64-call 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* jmp-reg) ; dest is ignored, since it is always the first C argument (rax in this case)
        (asm-helper-call code* target jmp-reg))))

  (define asm-activate-thread
    (let ([target `(x86_64-call 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* jmp-reg)
        (asm-helper-call code* target jmp-reg))))

  (define asm-deactivate-thread
    (let ([target `(x86_64-call 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code*)
        (asm-helper-call code* target %rax))))

  (define asm-unactivate-thread
    (let ([target `(x86_64-call 0 (entry ,(lookup-c-entry unactivate-thread)))])
      (lambda (code* arg-reg)
        (asm-helper-call code* target %rax))))

  (define asm-indirect-call
    (lambda (code* t . ignore)
      ; NB: c-call is already required to be a register or memory operand, so
      ; no need to build a relocation entry.
      (Trivit (t)
        (emit call t code*))))

  (define asm-direct-jump
    (lambda (l offset)
      (let ([offset (adjust-return-point-offset offset l)])
        (asm-helper-jump '() (make-funcrel 'x86_64-jump l offset)))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
        `(x86_64-jump ,(info-literal-offset info)
           (,(info-literal-type info) ,(info-literal-addr info))))))

  (define asm-indirect-jump
    (lambda (t)
      (Trivit (t)
        (safe-assert (record-case t [(imm) (n) (signed-32? n)] [else #t]))
        (emit jmp t '()))))

  (define-who asm-return-address
    (lambda (dest l incr-offset next-addr)
      (make-rachunk dest l incr-offset next-addr
        (or (cond
              [(local-label-offset l) =>
               (lambda (offset)
                 (let ([incr-offset (adjust-return-point-offset incr-offset l)])
                   (let ([disp (fx- next-addr (fx- offset incr-offset))])
                     (and (signed-32? disp)
                          (Trivit (dest)
                            (emit lea `(riprel ,disp) dest '()))))))]
               [else #f])
            (asm-move '() dest (with-output-language (L16 Triv) `(label-ref ,l ,incr-offset)))))))

  (define asm-jump
    (lambda (l next-addr)
      (make-gchunk l next-addr
        (cond
          [(local-label-offset l) =>
           (lambda (offset)
             (let ([disp (fx- next-addr offset)])
               (safe-assert (signed-32? disp))
               (if (fx= disp 0)
                   '()
                   (emit bra `(label ,disp ,l) '()))))]
          [else
           ; label must be somewhere above.  generate something so that a hard loop
           ; doesn't get dropped.  this also has some chance of being the right size
           ; for the final branch instruction.
           (emit bra `(label 0 ,l) '())]))))

  (define-who asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      (define get-disp-opnd
        (lambda (next-addr l)
          (cond
            [(and (local-label? l) (local-label-offset l)) =>
             (lambda (offset)
               (let ([disp (fx- next-addr offset)])
                 (safe-assert (signed-32? disp))
                 (values disp `(label ,disp ,l))))]
            [else (values 0 `(label 0 ,l))])))
      (safe-assert (and (local-label? l1) (local-label? l2)))
      (let ([type (info-condition-code-type info)]
            [reversed? (info-condition-code-reversed? info)])
        (make-cgchunk info l1 l2 next-addr
          (let ()
            (define-syntax pred-case
              (lambda (x)
                (define build-bop-seq
                  (lambda (bop opnd1 opnd2 l2 body)
                    #`(let ([code* (emit #,bop #,opnd1 code*)])
                        (let-values ([(disp #,opnd2) (get-disp-opnd (fx+ next-addr (asm-size* code*)) #,l2)])
                          #,body))))
                (define handle-or
                  (lambda (e opnd l)
                    (syntax-case e (or)
                      [(or bop1 bop2)
                       (build-bop-seq #'bop2 opnd opnd l
                         #`(emit bop1 #,opnd code*))]
                      [bop #`(emit bop #,opnd code*)])))
                (define handle-reverse
                  (lambda (e opnd l)
                    (syntax-case e (r?)
                      [(r? c1 c2) #`(if reversed? #,(handle-or #'c1 opnd l) #,(handle-or #'c2 opnd l))]
                      [_ (handle-or e opnd l)])))
                (define handle-inverse
                  (lambda (e)
                    (syntax-case e (i?)
                      [(i? c1 c2)
                       #`(cond
                           [(fx= disp1 0) #,(handle-reverse #'c1 #'opnd2 #'l2)]
                           [(fx= disp2 0) #,(handle-reverse #'c2 #'opnd1 #'l1)]
                           [else #,(build-bop-seq #'bra #'opnd2 #'opnd1 #'l1
                                     (handle-reverse #'c2 #'opnd1 #'l1))])]
                      [_ #`(cond ; treating e as c1: inverted condition, branching to false label
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
                             [else ($oops who "~s branch type is currently unsupported" type)]))))])))
            (pred-case
              [(eq?) (i? bne beq)]
              [(u<) (i? (r? bls bcc) (r? bhi bcs))]
              [(<) (i? (r? ble bge) (r? bgt blt))]
              [(<=) (i? (r? blt bgt) (r? bge ble))]
              [(>) (i? (r? bge ble) (r? blt bgt))]
              [(>=) (i? (r? bgt blt) (r? ble bge))]
              [(overflow multiply-overflow) (i? bvc bvs)]
              [(positive) (i? ble bgt)]
              [(carry) (i? bcc bcs)]
              ; unordered: zf,pf,cf <- 111; gt: 000; lt: 001; eq: 100
              ; reversed & inverted: !(fl< y x) = !(fl> x y) iff zf = 1 & cf = 1
              [(fp<) bls]
              ; reversed & inverted: !(fl<= y x) = !(fl>= x y) iff cf = 1
              [(fp<=) bcs]
              ; inverted: !(fl= x y) iff zf = 0 or cf (or pf) = 1
              [(fp=) (or bne bcs)]))))))

  (define asm-helper-jump
    (lambda (code* reloc)
      (let ([jmp-reg (cons 'reg %ts)])
        (emit movi '(imm #xffffffff) jmp-reg
          (emit jmp jmp-reg
            (asm-helper-relocation code* reloc))))))

  (define asm-kill
    (lambda (code* dest)
      code*))

  (define asm-helper-call
    (lambda (code* reloc jmp-reg)
      (emit movi '(imm #xffffffff) (cons 'reg jmp-reg)
        (emit call (cons 'reg jmp-reg)
          (asm-helper-relocation code* reloc)))))

  (define asm-helper-relocation
    (lambda (code* reloc)
      (cons* reloc (aop-cons* `(asm "relocation:" ,reloc) code*))))

  (define-syntax asm-enter
    (lambda (x)
      (syntax-case x ()
        [(k e)
         (with-implicit (k %seq %inline)
           #'(%seq
               ; adjust to 16-byte boundary, accounting for 8-byte return address pushed by call
               (set! ,%sp ,(%inline - ,%sp (immediate 8)))
               ,e))])))

  (module (asm-foreign-call asm-foreign-callable)
    (if-feature windows
      (begin
        (define vint (vector %Carg1 %Carg2 %Carg3 %Carg4))
        (define vfp (vector %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4)))
      (begin
        (define vint (vector %Carg1 %Carg2 %Carg3 %Carg4 %Carg5 %Carg6))
        (define vfp (vector %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4 %Cfparg5 %Cfparg6 %Cfparg7 %Cfparg8))))

    (define (align n size)
      (fxlogand (fx+ n (fx- size 1)) (fx- size)))

    (define (classify-type type)
      (nanopass-case (Ltype Type) type
        [(fp-ftd& ,ftd) (classify-eightbytes ftd)]
        [else #f]))

    (define (classified-size type)
      (nanopass-case (Ltype Type) type
        [(fp-ftd& ,ftd) ($ftd-size ftd)]
        [else #f]))

    ;; classify-eightbytes: returns '(memory) or a nonemtpy list of 'integer/'sse
    (if-feature windows
      ;; Windows: either passed in one register or not
      (define (classify-eightbytes ftd)
        (cond
         [($ftd-compound? ftd)
          (if (memv ($ftd-size ftd) '(1 2 4 8))
              '(integer)
              '(memory))]
         [(eq? 'float (caar ($ftd->members ftd)))
          '(sse)]
         [else '(integer)]))
      ;; Non-Windows: SYSV ABI is a more general classification of
      ;; 8-byte segments into 'integer, 'sse, or 'memory modes
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
                           [new-class (merge class (if (eq? kind 'float) 'sse 'integer))])
                      (cond
                       [(eq? new-class 'memory)
                        '(memory)]
                       [else
                        (vector-set! classes pos new-class)
                        (loop (cdr mbrs))]))]))])))])))

    (define (count v l)
      (cond
       [(null? l) 0]
       [(eq? (car l) v) (fx+ 1 (count v (cdr l)))]
       [else (count v (cdr l))]))

    ;; A result is put in registers if it has up to two
    ;; eightbytes, each 'integer or 'sse. On Windows,
    ;; `result-classes` always has only one item.
    (define (result-fits-in-registers? result-classes)
      (and result-classes
           (not (eq? 'memory (car result-classes)))
           (or (null? (cdr result-classes))
               (null? (cddr result-classes)))))

    ;; An argument is put in registers depending on how many
    ;; registers are left
    (define (pass-here-by-stack? classes iint ints ifp fps)
      (or (eq? 'memory (car classes))
          (fx> (fx+ iint ints) 6)
          (fx> (fx+ ifp fps) 8)))

    (module (push-registers pop-registers push-registers-size)
      (define (move-registers regs load?)
        (define (fp-reg? reg) (eq? (reg-type reg) 'fp))
        (with-output-language (L13 Effect)
          (let loop ([regs regs] [offset 0])
            (let* ([reg (car regs)]
                   [e (cond
                       [(fp-reg? reg)
                        (if load?
                            `(set! ,reg ,(%mref ,%sp ,%zero ,offset fp))
                            `(set! ,(%mref ,%sp ,%zero ,offset fp) ,reg))]
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

    (define (as-c-call e)
      (if-feature windows
        (with-output-language (L13 Effect)
          (%seq
           (set! ,%sp ,(%inline - ,%sp (immediate 32)))
           ,e
           (set! ,%sp ,(%inline + ,%sp (immediate 32)))))
        e))

    (define asm-foreign-call
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
                 [load-double-reg2
                  (lambda (fpreg ireg)
                    (lambda (x) ; unboxed
                      (%seq
                        (set! ,fpreg ,x)
                        ;; To support the varargs convention, copy the value into a GP register
                        (set! ,ireg ,(%inline get-double ,fpreg)))))]
                 [load-single-reg
                  (lambda (fpreg)
                    (lambda (x) ; unboxed
                      `(set! ,fpreg ,(%inline double->single ,x))))]
                 [load-int-reg
                  (lambda (type ireg)
                    (lambda (x)
                      (cond
                        [(nanopass-case (Ltype Type) type [(fp-integer ,bits) (fx= bits 32)] [else #f])
                         ; original code generated movil or movl
                         (nanopass-case (L13 Rhs) x
                           [,t `(set! ,ireg ,(%inline zext32 ,t))]
                           ; here we use ireg twice to get around needing a temporary when
                           ; x is a non-triv right-hand-side
                           [else (%seq (set! ,ireg ,x) (set! ,ireg ,(%inline zext32 ,ireg)))])]
                        [else `(set! ,ireg ,x)])))]
                 [load-content-stack
                  (lambda (offset len)
                    (lambda (x) ; requires var
                      (let loop ([offset offset] [x-offset 0] [len len])
                        (cond
                         [(= len 0) `(nop)]
                         [(>= len 8)
                          `(seq
                            (set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-64 #f)
                                                                ,%load ,x ,%zero (immediate ,x-offset)))
                            ,(loop (fx+ offset 8) (fx+ x-offset 8) (fx- len 8)))]
                         [(>= len 4)
                          `(seq
                            (set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-32 #f)
                                                                ,%load ,x ,%zero (immediate ,x-offset)))
                            ,(loop (fx+ offset 4) (fx+ x-offset 4) (fx- len 4)))]
                         [(>= len 2)
                          `(seq
                            (set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-16 #f)
                                                                ,%load ,x ,%zero (immediate ,x-offset)))
                            ,(loop (fx+ offset 2) (fx+ x-offset 2) (fx- len 2)))]
                         [else
                          `(set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-8 #f)
                                                               ,%load ,x ,%zero (immediate ,x-offset)))]))))]
                 [load-content-regs
                  (lambda (classes size unsigned? iint ifp)
                    (lambda (x) ; requires var
                      (let loop ([size size] [iint iint] [ifp ifp] [classes classes] [x-offset 0])
                        (cond
                         [(null? classes) `(nop)]
                         [(eq? 'sse (car classes))
                          (cond
                           [(fx= size 4)
                            ;; Must be the last element
                            `(set! ,(vector-ref vfp ifp) ,(%inline load-single ,(%mref ,x ,%zero ,x-offset fp)))]
                           [else
                            `(seq
                              (set! ,(vector-ref vfp ifp) ,(%mref ,x ,%zero ,x-offset fp))
                              ,(loop (fx- size 8) iint (fx+ ifp 1) (cdr classes) (fx+ x-offset 8)))])]
                         ;; Remaining cases are integers:
                         [(>= size 8)
                          `(seq
                            (set! ,(vector-ref vint iint) (inline ,(make-info-load 'integer-64 #f)
                                                                  ,%load ,x ,%zero (immediate ,x-offset)))
                            ,(loop (fx- size 8) (fx+ iint 1) ifp (cdr classes) (fx+ x-offset 8)))]
                         ;; Remaining cases must be the last element
                         [else
                          (let loop ([reg (vector-ref vint iint)] [size size] [x-offset x-offset])
                            (cond
                             [(= size 4)
                              `(set! ,reg (inline ,(make-info-load (if unsigned? 'unsigned-32 'integer-32) #f)
                                                  ,%load ,x ,%zero (immediate ,x-offset)))]
                             [(= size 2)
                              `(set! ,reg (inline ,(make-info-load (if unsigned? 'unsigned-16 'integer-16) #f)
                                                  ,%load ,x ,%zero (immediate ,x-offset)))]
                             [(= size 1)
                              `(set! ,reg (inline ,(make-info-load (if unsigned? 'unsigned-8 'integer-8) #f)
                                                  ,%load ,x ,%zero (immediate ,x-offset)))]
                             [(> size 4)
                              ;; 5, 6, or 7: multiple steps to avoid reading too many bytes
                              (let ([tmp %rax]) ;; ?? ok to use %rax?
                                (%seq
                                 ,(loop reg (fx- size 4) (fx+ x-offset 4))
                                 (set! ,reg ,(%inline sll ,reg (immediate 32)))
                                 ,(loop tmp 4 x-offset)
                                 (set! ,reg ,(%inline + ,reg ,tmp))))]
                             [else
                              ;; 3: multiple steps to avoid reading too many bytes
                              (let ([tmp %rax]) ;; ?? ok to use %rax?
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
                  (lambda (types)
                    (if-feature windows
                      (let loop ([types types] [locs '()] [regs '()] [fp-regs '()] [i 0] [isp 0])
                        (if (null? types)
                            (values isp 0 locs regs fp-regs)
                            (nanopass-case (Ltype Type) (car types)
                              [(fp-double-float)
                               (if (< i 4)
                                   (let ([reg (vector-ref vint i)])
                                     (loop (cdr types)
                                       (cons (load-double-reg2 (vector-ref vfp i) reg) locs)
                                       (cons reg regs) (cons (vector-ref vfp i) fp-regs) (fx+ i 1) isp))
                                   (loop (cdr types)
                                     (cons (load-double-stack isp) locs)
                                     regs fp-regs i (fx+ isp 8)))]
                              [(fp-single-float)
                               (if (< i 4)
                                   (loop (cdr types)
                                     (cons (load-single-reg (vector-ref vfp i)) locs)
                                     regs (cons (vector-ref vfp i) fp-regs) (fx+ i 1) isp)
                                   (loop (cdr types)
                                     (cons (load-single-stack isp) locs)
                                     regs fp-regs i (fx+ isp 8)))]
                              [(fp-ftd& ,ftd)
                               (cond
                                [(memv ($ftd-size ftd) '(1 2 4 8))
                                 ;; pass as value in register or as value on the stack
                                 (cond
                                  [(< i 4)
                                   ;; pass as value in register
                                   (cond
                                    [(and (not ($ftd-compound? ftd))
                                          (eq? 'float (caar ($ftd->members ftd))))
                                     ;; float or double
                                     (loop (cdr types)
                                       (cons (load-content-regs '(sse) ($ftd-size ftd) #t i i) locs)
                                       (add-regs 1 i vint regs) (add-regs 1 i vfp fp-regs) (fx+ i 1) isp)]
                                    [else
                                     ;; integer
                                     (loop (cdr types)
                                       (cons (load-content-regs '(integer) ($ftd-size ftd) ($ftd-unsigned? ftd) i i) locs)
                                       (add-regs 1 i vint regs) fp-regs(fx+ i 1) isp)])]
                                  [else
                                   ;; pass as value on the stack
                                   (loop (cdr types)
                                     (cons (load-content-stack isp ($ftd-size ftd)) locs)
                                     regs fp-regs i (fx+ isp (align ($ftd-size ftd) 8)))])]
                                [else
                                 ;; pass by reference in register or by reference on the stack
                                 (cond
                                  [(< i 4)
                                   ;; pass by reference in a register
                                   (let ([reg (vector-ref vint i)])
                                     (loop (cdr types)
                                       (cons (load-int-reg (car types) reg) locs)
                                       (cons reg regs) fp-regs (fx+ i 1) isp))]
                                  [else
                                   ;; pass by reference on the stack
                                   (loop (cdr types)
                                     (cons (load-int-stack isp) locs)
                                     regs fp-regs i (fx+ isp 8))])])]
                              [else
                               (if (< i 4)
                                   (let ([reg (vector-ref vint i)])
                                     (loop (cdr types)
                                       (cons (load-int-reg (car types) reg) locs)
                                       (cons reg regs) fp-regs
                                       (fx+ i 1) isp))
                                   (loop (cdr types)
                                     (cons (load-int-stack isp) locs)
                                     regs fp-regs i (fx+ isp 8)))])))
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
                                      [fps (count 'sse classes)])
                                 (cond
                                  [(pass-here-by-stack? classes iint ints ifp fps)
                                   ;; pass on the stack
                                   (loop (cdr types)
                                         (cons (load-content-stack isp ($ftd-size ftd)) locs)
                                         regs fp-regs iint ifp (fx+ isp (align ($ftd-size ftd) 8)))]
                                  [else
                                   ;; pass in registers
                                   (loop (cdr types)
                                         (cons (load-content-regs classes ($ftd-size ftd) ($ftd-unsigned? ftd) iint ifp) locs)
                                         (add-regs ints iint vint regs) (add-regs fps ifp vfp fp-regs)
                                         (fx+ iint ints) (fx+ ifp fps) isp)]))]
                              [else
                               (if (< iint 6)
                                   (let ([reg (vector-ref vint iint)])
                                     (loop (cdr types)
                                       (cons (load-int-reg (car types) reg) locs)
                                       (cons reg regs) fp-regs
                                       (fx+ iint 1) ifp isp))
                                   (loop (cdr types)
                                     (cons (load-int-stack isp) locs)
                                     regs fp-regs iint ifp (fx+ isp 8)))])))))])
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
                 ,(save-and-restore (cons %deact live*) (as-c-call (%inline deactivate-thread)))
                 ,e
                 ,(save-and-restore result-live* (as-c-call `(set! ,%rax ,(%inline activate-thread))))))]
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
            (let loop ([classes classes] [offset 0] [iregs (reg-list %rax %rdx)] [fpregs (reg-list %Cfparg1 %Cfparg2)] [size size])
              (cond
               [(null? classes)
                `(seq
                  ,c-call
                  (set! ,%rcx ,(%mref ,%sp ,saved-offset)))]
               [(eq? 'sse (car classes))
                `(seq
                  ,(loop (cdr classes) (fx+ offset 8) iregs (cdr fpregs) (fx- size 8))
                  ,(case size
                     [(4) (%inline store-single ,(%mref ,%rcx ,%zero ,offset fp) ,(car fpregs))]
                     [else `(set! ,(%mref ,%rcx ,%zero ,offset fp) ,(car fpregs))]))]
               [else
                `(seq
                  ,(loop (cdr classes) (fx+ offset 8) (cdr iregs) fpregs (fx- size 8))
                  ,(let ([ireg (car iregs)])
                     (case size
                       [(1) `(inline ,(make-info-load 'integer-8 #f) ,%store
                                     ,%rcx ,%zero (immediate ,offset) ,ireg)]
                       [(2) `(inline ,(make-info-load 'integer-16 #f) ,%store
                                     ,%rcx ,%zero (immediate ,offset) ,ireg)]
                       [(3) (%seq
                             (inline ,(make-info-load 'integer-16 #f) ,%store
                                     ,%rcx ,%zero (immediate ,offset) ,ireg)
                             (set! ,ireg ,(%inline srl ,ireg (immediate 16)))
                             (inline ,(make-info-load 'integer-8 #f) ,%store
                                     ,%rcx ,%zero (immediate ,(fx+ 2 offset)) ,ireg))]
                       [(4) `(inline ,(make-info-load 'integer-32 #f) ,%store
                                     ,%rcx ,%zero (immediate ,offset) ,ireg)]
                       [(5 6 7) (%seq
                                 (inline ,(make-info-load 'integer-32 #f) ,%store
                                         ,%rcx ,%zero (immediate ,offset) ,ireg)
                                 (set! ,ireg ,(%inline srl ,ireg (immediate 32)))
                                 ,(case size
                                    [(5)
                                     `(inline ,(make-info-load 'integer-8 #f) ,%store
                                              ,%rcx ,%zero (immediate ,(fx+ 4 offset)) ,ireg)]
                                    [(6)
                                     `(inline ,(make-info-load 'integer-16 #f) ,%store
                                              ,%rcx ,%zero (immediate ,(fx+ 4 offset)) ,ireg)]
                                    [(7)
                                     (%seq
                                      (inline ,(make-info-load 'integer-16 #f) ,%store
                                              ,%rcx ,%zero (immediate ,(fx+ 4 offset)) ,ireg)
                                      (set! ,ireg ,(%inline srl ,ireg (immediate 16)))
                                      (inline ,(make-info-load 'integer-8 #f) ,%store
                                              ,%rcx ,%zero (immediate ,(fx+ 6 offset)) ,ireg))]))]
                       [else `(set! ,(%mref ,%rcx ,offset) ,ireg)])))])))
          (define (get-result-regs fill-result-here? result-type result-classes)
            (if fill-result-here?
                (let loop ([classes result-classes] [iregs (reg-list %rax %rdx)] [fpregs (reg-list %Cfparg1 %Cfparg2)])
                  (cond
                   [(null? classes) '()]
                   [(eq? 'sse (car classes))
                    (cons (car fpregs) (loop (cdr classes) iregs (cdr fpregs)))]
                   [else
                    (cons (car iregs) (loop (cdr classes) (cdr iregs) fpregs))]))
                (nanopass-case (Ltype Type) result-type
                  [(fp-double-float) (list %Cfpretval)]
                  [(fp-single-float) (list %Cfpretval)]
                  [(fp-void) '()]
                  [else (list %rax)])))
          (define returnem
            (lambda (frame-size locs ccall r-loc)
             ; need to maintain 16-byte alignment, ignoring the return address
             ; pushed by call instruction, which counts as part of callee's frame
             ; tc is callee-save; no need to save
              (let ([frame-size (logand (+ frame-size 15) -16)])
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
                   [result-reg* (get-result-regs fill-result-here? result-type result-classes)]
                   [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)])
              (with-values (do-args (if fill-result-here? (cdr arg-type*) arg-type*))
                (lambda (frame-size nfp locs live* fp-live*)
                  (with-values (add-save-fill-target fill-result-here? frame-size locs)
                    (lambda (frame-size locs)
                      (returnem frame-size locs
                        (lambda (t0 not-varargs?)
                          (let* ([t (if adjust-active? %deact t0)] ; need a register if `adjust-active?`
                                 [kill* (add-caller-save-registers result-reg*)]
                                 [c-call
                                  (add-deactivate adjust-active? t0 (append fp-live* live*)
                                   result-reg*
                                   (if-feature windows
                                     (%seq
                                       (set! ,%sp ,(%inline - ,%sp (immediate 32)))
                                       (inline ,(make-info-kill*-live* kill* (append fp-live* live*)) ,%c-call ,t)
                                       (set! ,%sp ,(%inline + ,%sp (immediate 32))))
                                     (%seq
                                      ,(if not-varargs?
                                           `(nop)
                                           ;; System V ABI varargs functions require count of fp regs used in %al register.
                                           ;; since we don't know if the callee is a varargs function, we always set it.
                                           `(set! ,%rax (immediate ,nfp)))
                                      ,(let ([live* (append fp-live* live*)])
                                         `(inline ,(make-info-kill*-live* kill* (if not-varargs? live* (cons %rax live*))) ,%c-call ,t)))))])
                            (cond
                             [fill-result-here?
                              (add-fill-result c-call (fx- frame-size (constant ptr-bytes)) result-classes result-size)]
                             [else c-call])))
                        (nanopass-case (Ltype Type) result-type
                          [(fp-double-float)
                           (lambda (lvalue) ; unboxed
                             `(set! ,lvalue ,%Cfpretval))]
                          [(fp-single-float)
                           (lambda (lvalue) ; unboxed
                             `(set! ,lvalue ,(%inline single->double ,%Cfpretval)))]
                          [(fp-integer ,bits)
                           (case bits
                             [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline sext8 ,%rax)))]
                             [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline sext16 ,%rax)))]
                             [(32) (lambda (lvalue) `(set! ,lvalue ,(%inline sext32 ,%rax)))]
                             [(64) (lambda (lvalue) `(set! ,lvalue ,%rax))]
                             [else ($oops 'assembler-internal
                                     "unexpected asm-foreign-procedures fp-integer size ~s"
                                     bits)])]
                          [(fp-unsigned ,bits)
                           (case bits
                             [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline zext8 ,%rax)))]
                             [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline zext16 ,%rax)))]
                             [(32) (lambda (lvalue) `(set! ,lvalue ,(%inline zext32 ,%rax)))]
                             [(64) (lambda (lvalue) `(set! ,lvalue ,%rax))]
                             [else ($oops 'assembler-internal
                                     "unexpected asm-foreign-procedures fp-unsigned size ~s"
                                     bits)])]
                          [else (lambda (lvalue) `(set! ,lvalue ,%rax))])))))))))))

    (define asm-foreign-callable
      #|
      Windows:
                   Frame Layout
                   +---------------------------+
                   |                           |
                   |    incoming stack args    |
                   |                           |
           sp+176: +---------------------------+ <- 16-byte boundary
                   |                           | 
                   |  space for register args  | four quads
                   |                           | 
           sp+144: +---------------------------+ <- 16-byte boundary
incoming           |   incoming return address | one quad
      sp-> sp+120: +---------------------------+
                   |                           |
                   |   callee-save registers   | RBX, RBP, RDI, RSI, R12, R13, R14, R15 (8 quads)
                   |                           |                            and XMM6-11 (6 quads)
            sp+24: +---------------------------+
                   |        active state       | two quads
                   +---------------------------+
                   | pad word / indirect space | one quad
             sp+0: +---------------------------+<- 16-byte boundary


      Standard:
                   Frame Layout
                   +---------------------------+
                   |                           |
                   |    incoming stack args    |
           sp+192: |                           |
                   +---------------------------+ <- 16-byte boundary
                   |   incoming return address | one quad
                   +---------------------------+
           sp+176: |  pad word / active state  | one quad
                   +---------------------------+
                   |   indirect result space   | two quads
           sp+160: |  (for & results via regs) |
                   +---------------------------+<- 16-byte boundary
                   |                           | 
                   |    saved register args    | space for Carg*, Cfparg* (14 quads)
            sp+48: |                           |
                   +---------------------------+<- 16-byte boundary
                   |                           |
                   |   callee-save registers   | RBX, RBP, R12, R13, R14, R15 (6 quads)
                   |                           | 
             sp+0: +---------------------------+<- 16-byte boundary
      |#
      (with-output-language (L13 Effect)
        (let ()
          (define saved-register-arg-offset (if-feature windows 144 48))
          (define active-state-offset (if-feature windows 24 176))
          (define stack-args-offset (if-feature windows 176 192))
          (define load-double-stack
            (lambda (offset)
              (lambda (x) ; boxed (always a var)
                `(set! ,(%mref ,x ,%zero ,(constant flonum-data-disp) fp)
                       ,(%mref ,%sp ,%zero ,offset fp)))))
          (define load-single-stack
            (lambda (offset)
              (lambda (x) ; boxed (always a var)
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
                     [(32) `(set! ,lvalue (inline ,(make-info-load 'integer-32 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                     [(64) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                     [else ($oops 'assembler-internal
                             "unexpected load-int-stack fp-integer size ~s"
                             bits)])]
                  [(fp-unsigned ,bits)
                   (case bits
                     [(8) `(set! ,lvalue (inline ,(make-info-load 'unsigned-8 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                     [(16) `(set! ,lvalue (inline ,(make-info-load 'unsigned-16 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
                     [(32) `(set! ,lvalue (inline ,(make-info-load 'unsigned-32 #f) ,%load ,%sp ,%zero (immediate ,offset)))]
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
              (if-feature windows
                (let f ([types types] [i 0] [isp saved-register-arg-offset])
                  (if (or (null? types) (fx= i 4))
                      `(nop)
                      (nanopass-case (Ltype Type) (car types)
                        [(fp-double-float)
                         (if (< i 4)
                             (%seq
                               (set! ,(%mref ,%sp ,%zero ,isp fp) ,(vector-ref vfp i))
                               ,(f (cdr types) (fx+ i 1) (fx+ isp 8)))
                             (f (cdr types) i isp))]
                        [(fp-single-float)
                         (if (< i 4)
                             (%seq
                               ,(%inline store-single ,(%mref ,%sp ,%zero ,isp fp) ,(vector-ref vfp i))
                               ,(f (cdr types) (fx+ i 1) (fx+ isp 8)))
                             (f (cdr types) i isp))]
                        [(fp-ftd& ,ftd)
                         (cond
                          [(memv ($ftd-size ftd) '(1 2 4 8))
                           ;; receive as value in register or on the stack
                           (cond
                            [(< i 4)
                             ;; receive in register
                             (cond
                              [(and (not ($ftd-compound? ftd))
                                    (eq? 'float (caar ($ftd->members ftd))))
                               ;; float or double
                               `(seq
                                 (set! ,(%mref ,%sp ,%zero ,isp fp) ,(vector-ref vfp i))
                                 ,(f (cdr types) (fx+ i 1) (fx+ isp 8)))]
                              [else
                               ;; integer
                               `(seq
                                 (set! ,(%mref ,%sp ,isp) ,(vector-ref vint i))
                                 ,(f (cdr types) (fx+ i 1) (fx+ isp 8)))])]
                            [else
                             ;; receive by value on the stack
                             (f (cdr types) i isp)])]
                          [else
                           ;; receive by reference in register or on the stack
                           (cond
                            [(< i 4)
                             ;; receive by reference in register
                             `(seq
                               (set! ,(%mref ,%sp ,isp) ,(vector-ref vint i))
                               ,(f (cdr types) (fx+ i 1) (fx+ isp 8)))]
                            [else
                             ;; receive by reference on the stack
                             (f (cdr types) i isp)])])]
                        [else
                         (if (< i 4)
                             (%seq
                               (set! ,(%mref ,%sp ,isp) ,(vector-ref vint i))
                               ,(f (cdr types) (fx+ i 1) (fx+ isp 8)))
                             (f (cdr types) i isp))])))
                (let f ([types types] [iint 0] [ifp 0] [isp saved-register-arg-offset])
                  (if (or (null? types) (and (fx>= iint 6) (fx>= ifp 8)))
                      `(nop)
                      (nanopass-case (Ltype Type) (car types)
                        [(fp-double-float)
                         (if (< ifp 8)
                             (%seq
                               (set! ,(%mref ,%sp ,%zero ,isp fp) ,(vector-ref vfp ifp))
                               ,(f (cdr types) iint (fx+ ifp 1) (fx+ isp 8)))
                             (f (cdr types) iint ifp isp))]
                        [(fp-single-float)
                         (if (< ifp 8)
                             (%seq
                               ,(%inline store-single ,(%mref ,%sp ,%zero ,isp fp) ,(vector-ref vfp ifp))
                               ,(f (cdr types) iint (fx+ ifp 1) (fx+ isp 8)))
                             (f (cdr types) iint ifp isp))]
                        [(fp-ftd& ,ftd)
                         (let* ([classes (classify-eightbytes ftd)]
                                [ints (count 'integer classes)]
                                [fps (count 'sse classes)])
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
                                [(eq? (car classes) 'sse)
                                 `(seq
                                   (set! ,(%mref ,%sp ,%zero ,isp fp) ,(vector-ref vfp ifp))
                                   ,(reg-loop (cdr classes) iint (fx+ ifp 1) (+ isp 8)))]
                                [else
                                 `(seq
                                   (set! ,(%mref ,%sp ,isp) ,(vector-ref vint iint))
                                   ,(reg-loop (cdr classes) (fx+ iint 1) ifp (+ isp 8)))]))]))]
                        [else
                         (if (< iint 6)
                             (%seq
                               (set! ,(%mref ,%sp ,isp) ,(vector-ref vint iint))
                               ,(f (cdr types) (fx+ iint 1) ifp (fx+ isp 8)))
                             (f (cdr types) iint ifp isp))]))))))
          (define do-stack
            (lambda (types adjust-active?)
             ; risp is where incoming register args are stored
             ; sisp is where incoming stack args are stored
              (if-feature windows
                (let f ([types types] [locs '()] [isp saved-register-arg-offset]) ; stack args follow saved registers
                  (if (null? types)
                      locs
                      (f (cdr types)
                         (cons
                           (nanopass-case (Ltype Type) (car types)
                             [(fp-double-float) (load-double-stack isp)]
                             [(fp-single-float) (load-single-stack isp)]
                             [(fp-ftd& ,ftd)
                              (cond
                               [(memq ($ftd-size ftd) '(1 2 4 8))
                                ;; passed by value
                                (load-stack-address isp)]
                               [else
                                ;; passed by reference
                                (load-int-stack (car types) isp)])]
                             [else (load-int-stack (car types) isp)])
                           locs)
                         (fx+ isp 8))))
                (let f ([types types]
                        [locs '()]
                        [iint 0]
                        [ifp 0]
                        [risp saved-register-arg-offset]
                        [sisp stack-args-offset])
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
                                [fps (count 'sse classes)])
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
                         (if (= iint 6)
                             (f (cdr types)
                               (cons (load-int-stack (car types) sisp) locs)
                               iint ifp risp (fx+ sisp 8))
                             (f (cdr types)
                               (cons (load-int-stack (car types) risp) locs)
                               (fx+ iint 1) ifp (fx+ risp 8) sisp))]))))))
          (define (do-result result-type result-classes adjust-active?)
            (nanopass-case (Ltype Type) result-type
              [(fp-ftd& ,ftd)
               (cond
                [(result-fits-in-registers? result-classes)
                 ;; Copy content of result area on stack into
                 ;; the integer and floating-point registers
                 (let loop ([result-classes result-classes]
                            [offset (if-feature windows 0 160)]
                            [int* (list %rax %rdx)]
                            [fp* (list %Cfpretval %Cfparg2)]
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
                    [(eq? (car result-classes) 'sse)
                     (loop (cdr result-classes)
                           (fx+ offset 8)
                           int*
                           (cdr fp*)
                           (cons `(set! ,(car fp*) ,(%mref ,%sp ,%zero ,offset fp))
                                 accum)
                           live*
                           (cons (car fp*) fp-live*))]))]
                [else
                 (values (lambda ()
                           ;; Return pointer that was filled; destination was the first argument
                           `(set! ,%Cretval ,(%mref ,%sp ,saved-register-arg-offset)))
                         (list %Cretval)
                         '())])]
              [(fp-double-float)
               (values
                (lambda (x) ; boxed (always a var)
                  `(set! ,%Cfpretval ,(%mref ,x ,%zero ,(constant flonum-data-disp) fp)))
                '()
                (list %Cfpretval))]
              [(fp-single-float)
               (values
                (lambda (x) ; boxed (always a var)
                  `(set! ,%Cfpretval ,(%inline double->single ,(%mref ,x ,%zero ,(constant flonum-data-disp) fp))))
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
                       (set! ,%Carg1 ,(%mref ,%sp ,(+ (push-registers-size result-regs) active-state-offset)))
                       ,(as-c-call (%inline unactivate-thread ,%Carg1)))])
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
                      ,(if-feature windows
                         (%seq
                           ,(%inline push ,%rbx)
                           ,(%inline push ,%rbp)
                           ,(%inline push ,%rdi)
                           ,(%inline push ,%rsi)
                           ,(%inline push ,%r12)
                           ,(%inline push ,%r13)
                           ,(%inline push ,%r14)
                           ,(%inline push ,%r15)
                           ;; 16 bytes of this space is needed needed only for `adjust-active?`, but
                           ;; always add the space so that unwind info can be consistent:
                           (set! ,%sp ,(%inline - ,%sp (immediate 64)))
                           (set! ,(%mref ,%sp ,%zero 0 fp) ,%fp3)
                           (set! ,(%mref ,%sp ,%zero 8 fp) ,%fp4)
                           (set! ,(%mref ,%sp ,%zero 16 fp) ,%fp5)
                           (set! ,(%mref ,%sp ,%zero 24 fp) ,%fp6)
                           (set! ,(%mref ,%sp ,%zero 32 fp) ,%fp7)
                           (set! ,(%mref ,%sp ,%zero 40 fp) ,%fp8)
                           (set! ,%sp ,(%inline - ,%sp (immediate 8)))
                           ,(save-arg-regs arg-type*))
                         (%seq
                           (set! ,%sp ,(%inline - ,%sp (immediate 136)))
                           ,(%inline push ,%rbx)
                           ,(%inline push ,%rbp)
                           ,(%inline push ,%r12)
                           ,(%inline push ,%r13)
                           ,(%inline push ,%r14)
                           ,(%inline push ,%r15)
                           ,(save-arg-regs arg-type*)))
                      ,(if-feature pthreads
                         ((lambda (e)
                            (if adjust-active?
                                (%seq
                                 ,(as-c-call `(set! ,%rax ,(%inline activate-thread)))
                                 (set! ,(%mref ,%sp ,active-state-offset) ,%rax)
                                 ,e)
                                e))
                          (%seq
                           (set! ,%rax ,(%inline get-tc))
                           (set! ,%tc ,%rax)))
                         `(set! ,%tc (literal ,(make-info-literal #f 'entry (lookup-c-entry thread-context) 0))))))
                   (let ([locs (reverse locs)])
                     (if synthesize-first?
                         (cons (load-stack-address (if-feature windows 0 160)) ; space on stack for results to be returned via registers
                               locs)
                         locs))
                   get-result
                   (lambda ()
                     (define callee-save-regs
                       (if-feature windows
                         (list %rbx %rbp %rdi %rsi %r12 %r13 %r14 %r15
                               %fp3 %fp4 %fp5 %fp6 %fp7 %fp8)
                         (list %rbx %rbp %r12 %r13 %r14 %r15)))
                     (in-context Tail
                      ((lambda (e)
                         (if adjust-active?
                             (%seq
                              ,(unactivate (append result-fp-regs result-regs))
                              ,e)
                             e))
                       (%seq
                        ,(if-feature windows
                           (%seq
                             (set! ,%sp ,(%inline + ,%sp (immediate 8)))
                             (set! ,%fp3 ,(%mref ,%sp ,%zero 0 fp))
                             (set! ,%fp4 ,(%mref ,%sp ,%zero 8 fp))
                             (set! ,%fp5 ,(%mref ,%sp ,%zero 16 fp))
                             (set! ,%fp6 ,(%mref ,%sp ,%zero 24 fp))
                             (set! ,%fp7 ,(%mref ,%sp ,%zero 32 fp))
                             (set! ,%fp8 ,(%mref ,%sp ,%zero 40 fp))
                             (set! ,%sp ,(%inline + ,%sp (immediate 64)))
                             (set! ,%r15 ,(%inline pop))
                             (set! ,%r14 ,(%inline pop))
                             (set! ,%r13 ,(%inline pop))
                             (set! ,%r12 ,(%inline pop))
                             (set! ,%rsi ,(%inline pop))
                             (set! ,%rdi ,(%inline pop))
                             (set! ,%rbp ,(%inline pop))
                             (set! ,%rbx ,(%inline pop)))
                           (%seq
                             (set! ,%r15 ,(%inline pop))
                             (set! ,%r14 ,(%inline pop))
                             (set! ,%r13 ,(%inline pop))
                             (set! ,%r12 ,(%inline pop))
                             (set! ,%rbp ,(%inline pop))
                             (set! ,%rbx ,(%inline pop))
                             (set! ,%sp ,(%inline + ,%sp (immediate 136)))))
                        (asm-c-return ,null-info ,callee-save-regs ... ,result-regs ... ,result-fp-regs ...))))))))))))))
  )
