;;; ppc32.ss
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
;;;  Registers:
;;;    r0-r31: general purpose registers, 32 bits each
;;;    f0-f31: floating point registers, 64 bits each
;;;    cr0-cr7: condition register fields, 4 bits each
;;;    lr: link register
;;;    ctr: count register
;;;    xer: fixed-point exception register
;;;    fpscr: floating-point status and control register
;;;  Register usage:
;;;    r1: stack frame pointer -- callee-saved.
;;;        16-byte aligned, pointing at lowest allocated, valid 
;;;        stack frame, growing toward low addresses. contents 
;;;        of address point to the previously allocated stack frame.
;;;    r2: system-reserved -- do not use
;;;    r3, r4: return values -- caller-saved
;;;    r3-r10: parameter passing -- caller-saved
;;;    r13: small data area pointer -- shared objects shall not alter
;;;    r0, r11, r12: unused -- caller-saved
;;;    r14-r30: unused -- callee-saved
;;;    r31: used for local variables or "environment pointers" -- callee-saved
;;;    f1: floating point return value
;;;    f1-f8: floating point parameter passing
;;;    f0, f9-f13: unused -- callee-saved
;;;    f14-f31: unused -- caller-saved
;;;    cr2-cr4: callee-saved
;;;    cr1, cr5-cr7: caller-saved

(define-registers
  (reserved
    [%tc        %r29              #t 29]
    [%sfp       %r23              #t 23]
    [%ap        %r31              #t 31]
    [%esp       %r21              #t 21]
    [%eap       %r26              #t 26]
    [%trap      %r22              #t 22]
    [%real-zero %r0               #f  0])
  (allocable
    #;[%zero                      #f  0]
    [%ac0   %r11                  #f 11]
    [%xp    %r20                  #t 20]
    [%ts    %r14                  #t 14]
    [%td    %r15                  #t 15]
    [%ac1   %r12 %deact           #f 12]
    [%ret   %r17                  #t 17]
    [%cp    %r24                  #t 24]
    [%yp    %r27                  #t 27]
    [%tp    %r28                  #t 28]
    [       %r3  %Carg1  %Cretval  %Cretval-high  #f  3]
    [       %r4  %Carg2            %Cretval-low   #f  4]
    [       %r5  %Carg3                           #f  5]
    [       %r6  %Carg4                           #f  6]
    [       %r7  %Carg5                           #f  7]
    [       %r8  %Carg6                           #f  8]
    [       %r9  %Carg7                           #f  9]
    [       %r10 %Carg8                           #f 10]
    [       %r16                                  #t 16]
    [       %r18                                  #t 18]
    [       %r19                                  #t 19]
    [       %r25                                  #t 25]
    [       %r30                                  #t 30]
  )
  (machine-dependent
    [%sp       %Csp        #t  1]
    [%Ctoc                 #f  2] ;; operating system reserved
    [%Csda                 #f 13] ;; might point to small data area, if used
    [%flreg1               #f  0]
    [%Cfparg1  %Cfpretval  #f  1]
    [%Cfparg2              #f  2]
    [%Cfparg3              #f  3]
    [%Cfparg4              #f  4]
    [%Cfparg5              #f  5]
    [%Cfparg6              #f  6]
    [%Cfparg7              #f  7]
    [%Cfparg8              #f  8]
    [%flreg2               #f  9]
    [%flreg3               #f 10]
    [%flreg4               #f 11]
    [%flreg5               #f 12]
    [%flreg6               #f 13]
    [%flreg7               #t 14]
    [%flreg8               #t 15]
    [%flreg9               #t 16]
    [%flreg10              #t 17]
    [%flreg11              #t 18]
    [%flreg12              #t 19]
    [%flreg13              #t 20]
    [%flreg14              #t 21]
    [%flreg15              #t 22]
    [%flreg16              #t 23]
    [%flreg17              #t 24]
    [%flreg18              #t 25]
    [%flreg19              #t 26]
    [%flreg20              #t 27]
    [%flreg21              #t 28]
    [%flreg22              #t 29]
    [%flreg23              #t 30]
    [%flreg24              #t 31]
    ))

;;; SECTION 2: instructions
(module (md-handle-jump) ; also sets primitive handlers
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

  ; don't bother with literal@? check since lvalues can't be literals
  (define lmem? mref?)

  (define mem?
    (lambda (x)
      (or (lmem? x) (literal@? x))))

  (define-syntax define-imm-pred
    (lambda (x)
      (syntax-case x ()
        [(_ pred?)
         (with-syntax ([imm-pred? (construct-name #'pred? "imm-" #'pred?)])
           #'(define imm-pred?
               (lambda (x)
                 (nanopass-case (L15c Triv) x
                   [(immediate ,imm) (pred? imm)]
                   [else #f]))))])))

  (define-imm-pred integer16?)
  (define-imm-pred shifted-integer16?)
  (define-imm-pred negatable-integer16?)
  (define-imm-pred negatable-shifted-integer16?)
  (define-imm-pred unsigned16?)
  (define-imm-pred shifted-unsigned16?)
  (define-imm-pred shift-count?)

  (define imm-constant?
    (lambda (x)
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) #t]
        [else #f])))

  (define lvalue->ur
    (lambda (x k)
      (if (mref? x)
          (let ([u (make-tmp 'u)])
            (seq
              (set-ur=mref u x)
              (k u)))
          (k x))))

  (define mref->mref
    (lambda (a k)
      (define return
        (lambda (x0 x1 imm)
          ; ppc load & store instructions support index or offset but not both
          (safe-assert (or (eq? x1 %zero) (eqv? imm 0)))
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm)))))
      (nanopass-case (L15c Triv) a
        [(mref ,lvalue0 ,lvalue1 ,imm)
         (lvalue->ur lvalue0
           (lambda (x0)
             (lvalue->ur lvalue1
               (lambda (x1)
                 (cond
                   [(and (eq? x1 %zero) (integer16? imm)) (return x0 %zero imm)]
                   [else
                    (let ([u (make-tmp 'u)])
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
         (let ([u (make-tmp 'u)])
           (seq
             (build-set! ,u ,(literal@->literal a))
             (k (with-output-language (L15d Lvalue) `(mref ,u ,%zero 0)))))]
        [else (mref->mref a k)])))

  (define-pass imm->negative-imm : (L15c Triv) (ir) -> (L15d Triv) ()
    (Lvalue : Lvalue (ir) -> Lvalue ()
      [(mref ,lvalue1 ,lvalue2 ,imm) (sorry! who "unexpected mref ~s" ir)])
    (Triv : Triv (ir) -> Triv ()
      [(immediate ,imm) `(immediate ,(- imm))]))

  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (memq 'ur aty*)
             (and (memq 'shift-count aty*) (imm-shift-count? a))
             (and (memq 'unsigned16 aty*) (imm-unsigned16? a))
             (and (memq 'shifted-unsigned16 aty*) (imm-shifted-unsigned16? a))
             (and (memq 'integer16 aty*) (imm-integer16? a))
             (and (memq 'shifted-integer16 aty*) (imm-shifted-integer16? a))
             (and (memq 'negated-integer16 aty*) (imm-negatable-integer16? a))
             (and (memq 'negated-shifted-integer16 aty*) (imm-negatable-shifted-integer16? a))
             (and (memq 'imm-constant aty*) (imm-constant? a))
             (and (memq 'mem aty*) (mem? a))))]))

  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
           [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
           [(or (and (memq 'shift-count aty*) (imm-shift-count? a))
                (and (memq 'unsigned16 aty*) (imm-unsigned16? a))
                (and (memq 'shifted-unsigned16 aty*) (imm-shifted-unsigned16? a))
                (and (memq 'integer16 aty*) (imm-integer16? a))
                (and (memq 'shifted-integer16 aty*) (imm-shifted-integer16? a))
                (and (memq 'imm-constant aty*) (imm-constant? a)))
             (k (imm->imm a))]
           [(or (and (memq 'negated-integer16 aty*) (imm-negatable-integer16? a))
                (and (memq 'negated-shifted-integer16 aty*) (imm-negatable-shifted-integer16? a)))
            (k (imm->negative-imm a))]
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
           [else (sorry! 'coerce-opnd "cannot coerce ~s to ~s" a aty*)]))]))

  (define set-ur=mref
    (lambda (ur mref)
      (mref->mref mref
        (lambda (mref)
          (build-set! ,ur ,mref)))))

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
           ; NB: really need to use unspillable or mark %real-zero killed here but can't without extending jump syntax
           (values '() `(jump (literal ,info)))]
          [(label-ref ,l ,offset)
           ; NB: really need to use unspillable or mark %real-zero killed here but can't without extending jump syntax
           (values '() `(jump (label-ref ,l ,offset)))]
          [else (long-form t)]))))

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
                                   (let ([u (make-tmp 'u)])
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

  ; x is not the same as z in any clause that follows a clause where (x z)
  ; and y is coercible to one of its types, however:
  ; WARNING: do not assume that if x isn't the same as z then x is independent
  ; of z, since x might be an mref with z as it's base or index

  (define-instruction value (-)
    [(op (z ur) (x ur) (y negated-integer16 negated-shifted-integer16))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,y))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub-from ,y ,x))])

  (define-instruction value (-/ovfl)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub-from/ovfl ,y ,x))])

  (define-instruction value (-/eq)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-sub-from/eq ,y ,x))])

  (define-instruction value (+)
    [(op (z ur) (x ur) (y integer16 shifted-integer16))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,y))]
    [(op (z ur) (x integer16 shifted-integer16) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,y))])

  (define-instruction value (+/ovfl)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add/ovfl ,x ,y))])

  (define-instruction value (+/carry)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-add/carry ,x ,y))])

  (define-instruction value (*)
    [(op (z ur) (x ur) (y integer16))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,x ,y))]
    [(op (z ur) (x integer16) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-mul ,x ,y))])

  (define-instruction value (*/ovfl) ; ov flag set iff non-immediate
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
          [else (sorry! #f "unexpected logical operator ~s" op)])))
    (define-instruction value (logand logor logxor)
      [(op (z ur) (x unsigned16 shifted-unsigned16) (y ur))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,y ,x))]
      [(op (z ur) (x ur) (y unsigned16 shifted-unsigned16))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,x ,y))]
      [(op (z ur) (x ur) (y ur))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,x ,y))]))

  (define-instruction value (lognot)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))])

  (define-instruction value (srl)
    [(op (z ur) (x ur) (y shift-count))
     (if (nanopass-case (L15d Triv) y [(immediate ,imm) (zero? imm)])
         `(set! ,(make-live-info) ,z ,x)
         `(set! ,(make-live-info) ,z (asm ,info ,asm-srl ,x ,y)))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-srl ,x ,y))])

  (let ()
    (define select-op
      (lambda (op)
        (case op
          [(sra) asm-sra]
          [(sll) asm-sll])))
    (define-instruction value (sra sll)
      [(op (z ur) (x ur) (y shift-count ur))
       `(set! ,(make-live-info) ,z (asm ,info ,(select-op op) ,x ,y))]))

  (define-instruction value (move)
    [(op (z mem) (x ur))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem imm-constant))
     `(set! ,(make-live-info) ,z ,x)])

  (define-instruction value lea1
    ; NB: would be simpler if offset were explicit operand
    ; NB: why not one version of lea with %zero for y in lea1 case?
    [(op (z ur) (x ur))
     (let ([offset (info-lea-offset info)])
       (if (or (integer16? offset) (shifted-integer16? offset))
           `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x (immediate ,offset)))
           (let ([u (make-tmp 'u)])
             (seq
               `(set! ,(make-live-info) ,u (immediate ,offset))
               `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,u))))))])

  (define-instruction value lea2
    ; NB: would be simpler if offset were explicit operand
    [(op (z ur) (x ur) (y ur))
     (let ([offset (info-lea-offset info)] [u (make-tmp 'u)])
       (seq
         (if (or (integer16? offset) (shifted-integer16? offset))
             `(set! ,(make-live-info) ,u (asm ,info ,asm-add ,y (immediate ,offset)))
             (seq
               `(set! ,(make-live-info) ,u (immediate ,offset))
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,u ,y))))
         `(set! ,(make-live-info) ,z (asm ,info ,asm-add ,x ,u))))])

  (define-instruction value (sext8 sext16 zext8 zext16)
    [(op (z ur) (x mem ur)) `(set! ,(make-live-info) ,z (asm ,info ,(asm-move/extend op) ,x))])

  ;; load formats:
  ;; unsigned-16
  ;; lhzx rD <- [rA + rB]
  ;; lhz  rD <- [rA + d]
  ;; integer-16
  ;; lhax rD <- [rA + rB]
  ;; lha  rD <- [rA + d]
  ;; unsigned-8
  ;; lbzx rD <- [rA + rB]
  ;; lbz  rD <- [rA + rB]
  ;; signed-8 (no instructions for, have to load unsigned-8 and sign-extend)
  ;; lwz  rD <- [rA + d]
  ;; lwzx rD <- [rA + rB]
  (let ()
    (define imm-zero (with-output-language (L15d Triv) `(immediate 0)))
    (define load/store
      (lambda (info x y w k) ; x ur, y ur, w ur or imm
        (safe-assert (not (eq? w %zero)))
        (safe-assert (not (eq? x %zero)))
        (with-output-language (L15d Effect)
          (if (ur? w)
              (if (eq? y %zero)
                  (k x w imm-zero)
                  (let ([u (make-tmp 'u)])
                    (seq
                      `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w))
                      (k x u imm-zero))))
              (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
                (cond
                  [(fx= n 0) (k x y w)]
                  [(and (integer16? n) (not (info-load-swapped? info)))
                   (if (eq? y %zero)
                       (k x y w)
                       (let ([u (make-tmp 'u)])
                         (seq
                           `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,y))
                           (k u %zero w))))]
                  [(and (integer16? n) (not (eq? y %zero))) ; and swapped
                   (let ([u (make-tmp 'u)])
                     (seq
                       `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,w))
                       (k u y imm-zero)))]
                  [else (let ([u (make-tmp 'u)])
                          (seq
                            `(set! ,(make-live-info) ,u ,w)
                            (if (eq? y %zero)
                                (k x u imm-zero)
                                (seq
                                  `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,x ,u))
                                  (k u y imm-zero)))))]))))))
    (define-instruction value (load)
      [(op (z ur) (x ur) (y ur) (w ur imm-constant))
       (load/store info x y w
         (lambda (x y w)
           `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-load info) ,x ,y ,w))))])
    (define-instruction effect (store)
      [(op (x ur) (y ur) (w ur imm-constant) (z ur))
       (load/store info x y w
         (lambda (x y w)
           `(asm ,null-info ,(asm-store info) ,x ,y ,w ,z)))]))

  (define-instruction effect (store-with-update)
    [(op (x ur) (y ur) (z ur integer16))
     `(asm ,info ,asm-store-with-update ,x ,y ,z)])

  (define-instruction effect (load-single load-single->double load-double load-double->single
                              store-single store-single->double store-double)
    [(op (x ur) (y ur) (z integer16 ur))
     (if (eq? y %zero)
         (if (ur? z)
             `(asm ,info ,(asm-fl-load/store op (info-loadfl-flreg info)) ,x ,z (immediate 0))
             `(asm ,info ,(asm-fl-load/store op (info-loadfl-flreg info)) ,x ,y ,z))
         (if (and (not (ur? z)) (fx= (nanopass-case (L15d Triv) z [(immediate ,imm) imm]) 0))
             `(asm ,info ,(asm-fl-load/store op (info-loadfl-flreg info)) ,x ,y ,z)
             (let ([u (make-tmp 'u)])
               (seq
                 `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,z))
                 `(asm ,info ,(asm-fl-load/store op (info-loadfl-flreg info)) ,x ,u (immediate 0))))))])

  (define-instruction effect (flt)
    [(op (x ur) (y ur))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-flt ,x ,y ,u)))])

  (define-instruction effect (fl+ fl- fl/ fl*)
    [(op (x ur) (y ur) (z ur))
     `(asm ,info ,(asm-flop-2 op) ,x ,y ,z)])

  (define-instruction value (trunc)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-trunc ,x))])

  (define-instruction pred (fl= fl< fl<=)
    [(op (x ur) (y ur))
     (let ([info (make-info-condition-code op #f #f)])
        (values '() `(asm ,info ,(asm-fl-relop info) ,x ,y)))])

  (define-instruction effect (inc-cc-counter)
    [(op (x ur) (w shifted-integer16 integer16 ur) (z ur))
     (let ([u1 (make-tmp 'u1)] [u2 (make-tmp 'u2)])
       (seq
         `(set! ,(make-live-info) ,u1 (asm ,null-info ,asm-add ,x ,w))
         `(set! ,(make-live-info) ,u2 (asm ,null-info ,asm-kill))
         `(asm ,null-info ,asm-inc-cc-counter ,u1 ,z ,u2)))])

  (define-instruction effect (inc-profile-counter)
    [(op (x mem) (y ur shifted-integer16 integer16))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u ,x)
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,u ,y))
         `(set! ,(make-live-info) ,x ,u)))])

  (define-instruction value (read-time-stamp-counter)
    [(op (z ur))
     (safe-assert (and (info-kill*? info) (memq %real-zero (info-kill*-kill* info))))
      `(set! ,(make-live-info) ,z (asm ,info ,asm-read-time-base))])

  ;; always returns 0 on PPC
  (define-instruction value (read-performance-monitoring-counter)
    [(op (z ur) (x imm-constant ur)) `(set! ,(make-live-info) ,z (asm ,null-info ,asm-read-counter))])

  ;; no kills since we expect to be called when all necessary state has already been saved
  (define-instruction value (get-tc)
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,info ,asm-get-tc ,u))))])

  ;; like get-tc
  (define-instruction value (activate-thread)
    [(op (z ur))
     (safe-assert (eq? z %Cretval))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z (asm ,info ,asm-activate-thread ,u))))])

  (define-instruction effect (deactivate-thread)
    [(op)
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-deactivate-thread ,u)))])

  (define-instruction effect (unactivate-thread)
    [(op (z ur))
     (safe-assert (eq? z %Carg1))
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(asm ,info ,asm-unactivate-thread ,u)))])

  (define-instruction value (asmlibcall)
    [(op (z ur)) 
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(set! ,(make-live-info) ,z 
            (asm ,info ,(asm-library-call (info-asmlib-libspec info) #t) ,u ,(info-kill*-live*-live* info) ...))))])

  (define-instruction effect (asmlibcall!)
    [(op)
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(asm ,info ,(asm-library-call! (info-asmlib-libspec info) #t) ,u ,(info-kill*-live*-live* info) ...)))])

  (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
  (define-instruction effect (c-simple-call)
    [(op)
     (let ([u (make-tmp 'u)])
       (seq
         `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
         `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info) #t) ,u)))])

  (define-instruction pred (eq? < > <= >=)
    [(op (y integer16) (x ur))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #t #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))]
    [(op (x ur) (y ur integer16))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))])

  (define-instruction pred (u<)
    [(op (y unsigned16) (x ur))
     (let ([info (make-info-condition-code op #t #t)])
       (values '() `(asm ,info ,(asm-logrelop info) ,x ,y)))]
    [(op (x ur) (y ur unsigned16))
     (let ([info (make-info-condition-code op #f #t)])
       (values '() `(asm ,info ,(asm-logrelop info) ,x ,y)))])

  (define-instruction pred (condition-code)
    [(op) (values '() `(asm ,info ,(asm-condition-code info)))])

  (define-instruction pred (type-check?)
    [(op (x ur) (mask shifted-unsigned16 unsigned16 ur) (type unsigned16 ur))
     (let ([u (make-tmp 'u)])
       (values
         (with-output-language (L15d Effect)
           `(set! ,(make-live-info) ,u (asm ,null-info ,asm-logand ,x ,mask)))
         `(asm ,info-cc-eq ,(asm-logrelop info-cc-eq) ,u ,type)))])

  (define-instruction pred (logtest log!test)
    [(op (x shifted-unsigned16 unsigned16) (y ur))
     (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,y ,x))]
    [(op (x ur) (y ur integer16))
     (values '() `(asm ,info-cc-eq ,(asm-logtest (eq? op 'log!test) info-cc-eq) ,x ,y))])

  (let ()
    (define lea->reg
      (lambda (x y w k)
        (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
          (with-output-language (L15d Effect)
            (cond
              [(eqv? n 0)
                (if (eq? y %zero)
                    (k %real-zero x)
                    (k x y))]
              [(or (shifted-integer16? n) (integer16? n))
                (let ([u (make-tmp 'u)])
                  (seq
                    (if (eq? y %zero)
                        `(set! ,(make-live-info) ,u ,w)
                        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,w)))
                    (k x u)))]
              [else
                (let ([u (make-tmp 'u)])
                  (seq
                    `(set! ,(make-live-info) ,u ,w)
                    `(set! ,(make-live-info) ,u (asm ,null-info ,asm-add ,y ,u))
                    (k x u)))])))))
    ;; compiler implements init-lock! and unlock! as 32-bit store of zero 
    (define-instruction pred (lock!)
      [(op (x ur) (y ur) (w shifted-integer16 integer16))
       (lea->reg x y w
         (lambda (base index)
           (values
             '()
             `(asm ,info-cc-eq ,(asm-lock info-cc-eq) ,base ,index))))])
    (define-instruction effect (locked-incr! locked-decr!)
      [(op (x ur) (y ur) (w shifted-integer16 integer16))
       (lea->reg x y w
         (lambda (base index)
           (let ([u (make-tmp 'u)])
             (seq
               `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
               `(asm ,null-info ,(asm-lock+/- op) ,base ,index ,u)))))]))

  (define-instruction effect (pause)
    [(op) `(asm ,info ,asm-isync)])

  (define-instruction effect (c-call)
    [(op (x ur))
     `(asm ,info ,asm-indirect-call ,x ,(info-kill*-live*-live* info) ...)])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect (invoke-prelude)
    [(op) `(set! ,(make-live-info) ,%tc ,%Carg1)])

  (define-instruction effect (save-lr)
    [(op (z integer16))
     (let ([n (nanopass-case (L15d Triv) z [(immediate ,imm) imm])])
       (seq
         `(set! ,(make-live-info) ,%real-zero (asm ,info ,(asm-get-lr)))
         `(set! ,(make-live-info) (mref ,%Csp ,%zero ,n) ,%real-zero)))])

  (define-instruction effect (restore-lr)
    [(op (z integer16))
     (let ([n (nanopass-case (L15d Triv) z [(immediate ,imm) imm])])
       (seq
         `(set! ,(make-live-info) ,%real-zero (mref ,%Csp ,%zero ,n))
         `(asm ,info ,(asm-set-lr) ,%real-zero)))])
)

;;; SECTION 3: assembler
(module asm-module ( ; required exports
                     asm-move asm-move/extend asm-load asm-store asm-library-call asm-library-call! asm-library-jump
                     asm-div asm-mul asm-mul/ovfl asm-add asm-add/ovfl asm-sub-from asm-sub-from/ovfl
                     asm-add/carry asm-sub-from/eq
                     asm-logand asm-logor asm-logxor asm-sra asm-srl asm-sll
                     asm-logand asm-lognot
                     asm-logtest asm-fl-relop asm-relop asm-logrelop
                     asm-indirect-jump asm-literal-jump
                     asm-direct-jump asm-return-address asm-jump asm-conditional-jump asm-data-label asm-rp-header
                     asm-indirect-call asm-condition-code
                     asm-trunc asm-flt
                     asm-lock asm-lock+/-
                     asm-fl-load/store
                     asm-flop-2 asm-c-simple-call
                     asm-save-flrv asm-restore-flrv asm-return asm-c-return asm-size
                     asm-enter asm-foreign-call asm-foreign-callable
                     asm-read-counter
                     asm-read-time-base
                     asm-inc-cc-counter
                     asm-store-with-update
                     asm-get-lr asm-set-lr
                     unsigned16? shifted-unsigned16?
                     integer16? shifted-integer16?
                     negatable-integer16? negatable-shifted-integer16?
                     shift-count?
                     asm-isync
                     ; threaded version specific
                     asm-get-tc asm-activate-thread asm-deactivate-thread asm-unactivate-thread
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

  (define ax-condition-helper
    (lambda (code)
      (define build-code
        (lambda (code opnd-code)
          (let ([opnd (case opnd-code
                        [(true)   #b01100]
                        [(false)  #b00100]
                        [(always) #b10100])]
                [index (case code
                         [(lt) 0]
                         [(gt) 1]
                         [(eq) 2]
                         [(so) 3])])
            ;; always assuming cr = 0, if not calculation is (fx+ (fx* cr 4) index)
            (fxlogor (fxsll opnd 5) index))))
      (case code
        [(al) (build-code 'lt 'always)]
        [(lt) (build-code 'lt 'true)]
        [(le) (build-code 'gt 'false)]
        [(eq) (build-code 'eq 'true)]
        [(ge) (build-code 'lt 'false)]
        [(gt) (build-code 'gt 'true)]
        [(nl) (build-code 'lt 'false)]
        [(ne) (build-code 'eq 'false)]
        [(ng) (build-code 'gt 'false)]
        [(so) (build-code 'so 'true)]
        [(ns) (build-code 'so 'false)])))

  (define ax-spr-code
    (lambda (spr)
      (case spr
        [(lr)  #b0100000000]
        [(ctr) #b0100100000]
        [(xer) #b0000100000])))

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

  (define-op add     arithmetic-op   #b100001010 #b0 #b0)
  (define-op add.    arithmetic-op   #b100001010 #b1 #b0)
  (define-op addo.   arithmetic-op   #b100001010 #b1 #b1)
  (define-op addi    reg-reg-simm-op #b001110)
  (define-op addis   reg-reg-simm-op #b001111)
  (define-op divw    arithmetic-op   #b111101011 #b0 #b0)
  (define-op mulli   reg-reg-simm-op #b000111)
  (define-op mullw   arithmetic-op   #b011101011 #b0 #b0)
  (define-op mullwo. arithmetic-op   #b011101011 #b1 #b1)
  (define-op subf    arithmetic-op   #b000101000 #b0 #b0)
  (define-op subf.   arithmetic-op   #b000101000 #b1 #b0)
  (define-op subfo.  arithmetic-op   #b000101000 #b1 #b1)

  (define-op and    logical-op      #b0000011100 #b0)
  (define-op and.   logical-op      #b0000011100 #b1)
  (define-op andi.  reg-reg-uimm-op #b011100)
  (define-op andis. reg-reg-uimm-op #b011101)
  (define-op or     logical-op      #b0110111100 #b0)
  (define-op nor    logical-op      #b0001111100 #b0)
  (define-op ori    reg-reg-uimm-op #b011000)
  (define-op oris   reg-reg-uimm-op #b011001)
  (define-op xor    logical-op      #b0100111100 #b0)
  (define-op xori   reg-reg-uimm-op #b011010)
  (define-op xoris  reg-reg-uimm-op #b011011)

  ; Note: actually a reg-reg-uimm-op ori r0, r0,0, written this way to get the
  ; nop to show up in asm output.
  (define-op nop    nop-op)

  (define-op rlwinm  rotate-imm-op  #b0)
  (define-op slw     logical-op     #b0000011000 #b0)
  (define-op sraw    logical-op     #b1100011000 #b0)
  (define-op srawi   logical-imm-op #b1100111000 #b0)
  (define-op srw     logical-op     #b1000011000 #b0)

  (define-op lbz   reg-reg-simm-op #b100010)
  (define-op lbzx  indexed-op      #b0001010111)
  (define-op lha   reg-reg-simm-op #b101010)
  (define-op lhax  indexed-op      #b0101010111)
  (define-op lhbrx indexed-op      #b1100010110)
  (define-op lhz   reg-reg-simm-op #b101000)
  (define-op lhzx  indexed-op      #b0100010111)
  (define-op lwarx reserved-op     #b010100     #b0)
  (define-op lwbrx indexed-op      #b1000010110)
  (define-op lwz   reg-reg-simm-op #b0000100000)
  (define-op lwzx  indexed-op      #b0000010111)

  (define-op lfd  reg-reg-simm-op #b110010)
  (define-op lfdx indexed-op      #b1001010111)
  (define-op lfs  reg-reg-simm-op #b110000)
  (define-op lfsx indexed-op      #b1000010111)

  (define-op stfd   reg-reg-simm-op #b110110)
  (define-op stfdu  reg-reg-simm-op #b110111)
  (define-op stfdx  indexed-op      #b1011010111)
  (define-op stfs   reg-reg-simm-op #b110100)
  (define-op stfsx  indexed-op      #b1010010111)
  (define-op stwcx. reserved-op     #b10010110 #b1)

  (define-op fctiwz flsingle-op #b001111 #b0)
  (define-op frsp   flsingle-op #b001100 #b0)

  (define-op fadd flreg-op #b010101 #b0)
  (define-op fdiv flreg-op #b010010 #b0)
  (define-op fmul flmul-op #b0)
  (define-op fsub flreg-op #b010100 #b0)

  (define-op cror cror-op)

  (define-op fcmpu compare-op     #b111111 #b0000000000)
  (define-op cmp   compare-op     #b011111 #b0000000000)
  (define-op cmpl  compare-op     #b011111 #b0000100000)
  (define-op cmpi  compare-imm-op #b001011)
  (define-op cmpli compare-imm-op #b001010)

  (define-op stb    reg-reg-simm-op #b100110)
  (define-op stbx   indexed-op      #b0011010111)
  (define-op sth    reg-reg-simm-op #b101100)
  (define-op sthbrx indexed-op      #b1110010110)
  (define-op sthx   indexed-op      #b0110010111)
  (define-op stw    reg-reg-simm-op #b100100)
  (define-op stwbrx indexed-op      #b1010010110)
  (define-op stwu   reg-reg-simm-op #b100101)
  (define-op stwux  indexed-op      #b0010110111)
  (define-op stwx   indexed-op      #b0010010111)

  (define-op extsb ext-op #b1110111010 #b0)
  (define-op extsh ext-op #b1110011010 #b0)

  (define-op b     unconditional-branch-op      #b0 #b0)
  (define-op blt   conditional-branch-op        #b0 #b0 (ax-condition-helper 'lt))
  (define-op ble   conditional-branch-op        #b0 #b0 (ax-condition-helper 'le))
  (define-op beq   conditional-branch-op        #b0 #b0 (ax-condition-helper 'eq))
  (define-op bge   conditional-branch-op        #b0 #b0 (ax-condition-helper 'ge))
  (define-op bgt   conditional-branch-op        #b0 #b0 (ax-condition-helper 'gt))
  (define-op bnl   conditional-branch-op        #b0 #b0 (ax-condition-helper 'nl))
  (define-op bne   conditional-branch-op        #b0 #b0 (ax-condition-helper 'ne))
  (define-op bng   conditional-branch-op        #b0 #b0 (ax-condition-helper 'ng))
  (define-op bso   conditional-branch-op        #b0 #b0 (ax-condition-helper 'so))
  (define-op bns   conditional-branch-op        #b0 #b0 (ax-condition-helper 'ns))
  (define-op bctr  conditional-branch-to-spr-op #b1000010000 #b0 (ax-condition-helper 'al))
  (define-op bctrl conditional-branch-to-spr-op #b1000010000 #b1 (ax-condition-helper 'al))
  (define-op blr   conditional-branch-to-spr-op #b0000010000 #b0 (ax-condition-helper 'al))

  (define-op mfcr  move-from-cr-op)
  (define-op mflr  move-from-special-reg-op #b0101010011 (ax-spr-code 'lr))
  (define-op mftb  move-from-special-reg-op #b0101110011 #b0110001000)
  (define-op mftbu move-from-special-reg-op #b0101110011 #b0110101000)
  (define-op mtcrf move-to-cr-op)
  (define-op mtlr  move-to-special-reg-op (ax-spr-code 'lr))
  (define-op mtctr move-to-special-reg-op (ax-spr-code 'ctr))
  (define-op mtxer move-to-special-reg-op (ax-spr-code 'xer))

  (define-op isync isync-op)

  (define arithmetic-op
    (lambda (op opcode set-cr? set-oe? dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op dest-ea opnd0-ea opnd1-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code dest-ea)]
        [16 (ax-ea-reg-code opnd0-ea)]
        [11 (ax-ea-reg-code opnd1-ea)]
        [10 set-oe?]
        [1  opcode]
        [0  set-cr?])))

  (define reg-reg-simm-op
    (lambda (op opcode dest-ea opnd0-ea imm code*)
      (emit-code (op dest-ea opnd0-ea imm code*)
        [26 opcode]
        [21 (ax-ea-reg-code dest-ea)]
        [16 (ax-ea-reg-code opnd0-ea)]
        [0  (fxlogand (ax-imm-data imm) #xFFFF)])))

  (define reg-reg-uimm-op
    (lambda (op opcode dest-ea opnd0-ea imm code*)
      (emit-code (op dest-ea opnd0-ea imm code*)
        [26 opcode]
        [21 (ax-ea-reg-code opnd0-ea)]
        [16 (ax-ea-reg-code dest-ea)]
        [0  (fxlogand (ax-imm-data imm) #xFFFF)])))

  ;; same as reg-reg-uimm-op, if we fixed the operation to ori and provided
  ;; r0, r0, 0 as the operands
  (define nop-op
    (lambda (op code*)
      (emit-code (op code*)
        [26 #b011000]
        [21 #b00000]
        [16 #b00000]
        [0  #b00000])))
 
  (define logical-op
    (lambda (op opcode set-cr? dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op dest-ea opnd0-ea opnd1-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code opnd0-ea)]
        [16 (ax-ea-reg-code dest-ea)]
        [11 (ax-ea-reg-code opnd1-ea)]
        [1  opcode]
        [0  set-cr?])))
  
  (define logical-imm-op
    (lambda (op opcode set-cr? dest-ea opnd0-ea imm code*)
      (emit-code (op dest-ea opnd0-ea imm code*)
        [26 #b011111]
        [21 (ax-ea-reg-code opnd0-ea)]
        [16 (ax-ea-reg-code dest-ea)]
        [11 (fxlogand (ax-imm-data imm) #xFFFF)]
        [1  opcode]
        [0  set-cr?])))
  
  (define indexed-op
    (lambda (op opcode dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op dest-ea opnd0-ea opnd1-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code dest-ea)]
        [16 (ax-ea-reg-code opnd0-ea)]
        [11 (ax-ea-reg-code opnd1-ea)]
        [1  opcode]
        [0  #b0])))

  (define ext-op
    (lambda (op opcode set-cr? dest-ea opnd0-ea code*)
      (emit-code (op dest-ea opnd0-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code opnd0-ea)]
        [16 (ax-ea-reg-code dest-ea)]
        [11 #b00000]
        [1  opcode]
        [0  set-cr?])))

  (define rotate-imm-op
    (lambda (op set-cr? dest-ea opnd0-ea imm-shift imm-mb imm-me code*)
      ;; imm-shift, imm-mb, and imm-me checked to be between 0 <= imm-{shift,mb,me} <= 31 before calling rotate-imm-op
      (emit-code (op dest-ea opnd0-ea imm-shift imm-mb imm-me code*)
        [26 #b010101]
        [21 (ax-ea-reg-code opnd0-ea)]
        [16 (ax-ea-reg-code dest-ea)]
        [11 (ax-imm-data imm-shift)]
        [6  (ax-imm-data imm-mb)]
        [1  (ax-imm-data imm-me)]
        [0  set-cr?])))

  (define flsingle-op
    (lambda (op opcode set-cr? dest-ea opnd0-ea code*)
      (emit-code (op dest-ea opnd0-ea code*)
        [26 #b111111]
        [21 (ax-ea-reg-code dest-ea)]
        [11 (ax-ea-reg-code opnd0-ea)]
        [1  opcode]
        [0  set-cr?])))

  (define flreg-op
    (lambda (op opcode set-cr? dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op dest-ea opnd0-ea opnd1-ea code*)
        [26 #b111111]
        [21 (ax-ea-reg-code dest-ea)]
        [16 (ax-ea-reg-code opnd0-ea)]
        [11 (ax-ea-reg-code opnd1-ea)]
        [6  #b00000]
        [1  opcode]
        [0  set-cr?])))

  (define flmul-op
    (lambda (op set-cr? dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op dest-ea opnd0-ea opnd1-ea code*)
        [26 #b111111]
        [21 (ax-ea-reg-code dest-ea)]
        [16 (ax-ea-reg-code opnd0-ea)]
        [11 #b00000]
        [6  (ax-ea-reg-code opnd1-ea)]
        [1  #b011001]
        [0  set-cr?])))

  (define compare-op
    (lambda (op opcode0 opcode1 opnd0-ea opnd1-ea code*)
      (emit-code (op opnd0-ea opnd1-ea code*)
        [26 opcode0]
        [23 #b000] ; crfD
        [22 #b0]
        [21 #b0] ; L-bit (long?), must be 0 on 32-bit
        [16 (ax-ea-reg-code opnd0-ea)]
        [11 (ax-ea-reg-code opnd1-ea)]
        [1  opcode1]
        [0  #b0])))

  (define compare-imm-op
    (lambda (op opcode opnd-ea imm code*)
      (emit-code (op opnd-ea imm code*)
        [26 opcode]
        [23 #b000] ; crfD
        [22 #b0]
        [21 #b0]   ; L bit
        [16 (ax-ea-reg-code opnd-ea)]
        [0  (fxlogand (ax-imm-data imm) #xFFFF)])))

  (define reserved-op
    (lambda (op opcode1 opcode2 dest-ea opnd0-ea opnd1-ea code*)
      (emit-code (op dest-ea opnd0-ea opnd1-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code dest-ea)]
        [16 (ax-ea-reg-code opnd0-ea)]
        [11 (ax-ea-reg-code opnd1-ea)]
        [1  opcode1]
        [0  opcode2])))

  (define-who conditional-branch-op
    (lambda (op absolute-address link condition-bits branch-dest code*)
      (emit-code (op branch-dest code*) 
        [26 #b010000]
        [16 condition-bits]
        [2  (if (pair? branch-dest)
                (record-case branch-dest
                  [(label) (offset l)
                   (fxlogand (fxsrl (fx+ offset 4) 2) #x3FFF)]
                  [else (sorry! who "unexpected dest ~s" branch-dest)])
                (fxlogand branch-dest #x3FFF))]
        [1  absolute-address]
        [0  link])))

  (define conditional-branch-to-spr-op
    (lambda (op opcode link condition-bits code*)
      (emit-code (op code*) 
        [26 #b010011]
        [16 condition-bits]
        [11 #b00000]
        [1  opcode]
        [0  link])))

  (define-who unconditional-branch-op
    (lambda (op absolute-address link dest code*)
      (record-case dest
        [(label) (offset l)
         (emit-code (op dest code*)
           [26 #b010010]
           [2  (fxlogand (fxsrl (fx+ offset 4) 2) #xFFFFFF)]
           [1  absolute-address]
           [0  link])]
        [else (sorry! who "unexpected dest ~s" dest)])))

  (define move-from-cr-op
    (lambda (op dest-ea code*)
      (emit-code (op dest-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code dest-ea)]
        [16 #b00000]
        [11 #b00000]
        [1  #b0000010011]
        [0  #b0])))

  (define move-from-special-reg-op
    (lambda (op opcode spr dest-ea code*)
      (emit-code (op dest-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code dest-ea)]
        [11 spr]
        [1  opcode]
        [0  #b0])))

  (define move-to-cr-op
    (lambda (op mask opnd-ea code*)
      (emit-code (op mask opnd-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code opnd-ea)]
        [20 #b0]
        [12 mask]
        [11 #b0]
        [1  #b0010010000]
        [0  #b0])))

  (define move-to-special-reg-op
    (lambda (op spr opnd-ea code*)
      (emit-code (op opnd-ea code*)
        [26 #b011111]
        [21 (ax-ea-reg-code opnd-ea)]
        [11 spr]
        [1  #b0111010011]
        [0  #b0])))

  (define cror-op
    (lambda (op dest-fld opnd0-fld opnd1-fld code*)
      (emit-code (op dest-fld opnd0-fld opnd1-fld code*)
        [26 #b010011]
        [21 dest-fld]
        [16 opnd0-fld]
        [11 opnd1-fld]
        [1  #b0111000001]
        [0  #b0])))

  (define isync-op
    (lambda (op code*)
      (emit-code (op code*)
        [26 #b010011]
        [21 #b00000]
        [16 #b00000]
        [11 #b00000]
        [1  #b0010010110]
        [0  #b0])))

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
  
  (define-syntax build
    (syntax-rules ()
      [(_ x e)
       (and (memq (datum x) '(byte word long)) (integer? (datum e)))
       (quote (x . e))]
      [(_ x e)
       (memq (datum x) '(byte word long))
       (cons 'x e)]))

  (define-syntax byte-fields
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

  (define unsigned16?
    (lambda (imm)
      (and (fixnum? imm) ($fxu< imm (expt 2 16)))))

  (define shifted-unsigned16?
    (lambda (imm)
      (and (<= (- (expt 2 31)) imm (- (expt 2 31) 1))
           (not (logtest imm #xFFFF)))))

  (define integer16?
    (lambda (imm)
      (and (fixnum? imm) (fx< (fx- (expt 2 15)) imm (fx- (expt 2 15) 1)))))

  (define shifted-integer16?
    (lambda (imm)
      (and (<= (- (expt 2 31)) imm (- (expt 2 31) 1))
           (not (logtest imm #xFFFF)))))

  (define negatable-integer16?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- 1 (expt 2 15)) imm (expt 2 15)))))

  (define negatable-shifted-integer16?
    (lambda (imm)
      (and (<= (- 1 (expt 2 31)) imm (expt 2 31))
           (not (logtest imm #xFFFF)))))

  (define shift-count?
    (lambda (imm)
      (and (fixnum? imm) ($fxu< imm (expt 2 5)))))

  (define branch-disp?
    (lambda (x)
      (and (fixnum? x) 
           (fx<= (- (expt 2 25)) x (- (expt 2 25) 1))
           (not (fxlogtest x #b11)))))

  (define conditional-branch-disp?
    (lambda (x)
      (and (fixnum? x) 
           (fx<= (- (expt 2 15)) x (- (expt 2 15) 1))
           (not (fxlogtest x #b11)))))

  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm ppc32-abs ppc32-jump ppc32-call) 0]
        [else 4])))

  (define ax-mov32
    (lambda (dest n code*)
      (let* ([n (if (< n 0) (+ n (expt 2 32)) n)] ;; signed -> unsigned conversion for 32-bit value
             [high (ash n -16)]                   ;; shift high bits into lower 16
             [low (- n (ash high 16))]            ;; subtract upper 16 bits off x
             [high (cond
                     [(fx< low #x8000) high]          ;; if high bit of low is set, use high
                     [(fx< high #xFFFF) (fx+ high 1)] ;; else, if high is less than (- (expt 2 16) 1), add 1 to adjust for signed-ness of lower addi
                     [else 0])])                      ;; otherwise high is (- (expt 2 16) 1), we still need to add 1, so we wrap to 0, giving the right final result.
        (emit addis dest `(reg . ,%real-zero) `(imm ,high)
          (emit addi dest dest `(imm ,low) code*)))))

  (define-who ax-move-literal
    (lambda (dest src code*)
      (record-case src
        [(literal) stuff
         (ax-mov32 dest 0
           (asm-helper-relocation (cons 'ppc32-abs stuff) code*))]
        [else (sorry! who "unexpected source ~s" src)])))

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
             [(reg) ignore (emit or dest src src code*)]
             [(imm) (n)
              (cond
                [(integer16? n) (emit addi dest `(reg . ,%real-zero) `(imm ,n) code*)]
                [(shifted-integer16? n)
                 (emit addis dest `(reg . ,%real-zero)
                   `(imm ,(bitwise-arithmetic-shift-right n 16))
                   code*)]
                [else (ax-mov32 dest n code*)])]
             [(literal) stuff (ax-move-literal dest src code*)]
             [(disp) (n breg)
              (safe-assert (integer16? n))
              (emit lwz dest `(reg . ,breg) `(imm ,n) code*)]
             [(index) (n ireg breg)
              (safe-assert (eqv? n 0))
              (emit lwzx dest `(reg . ,breg) `(reg . ,ireg) code*)]
             [else (bad!)])]
          [(ax-reg? src)
           (record-case dest
             [(disp) (n breg)
              (safe-assert (or (unsigned16? n) (unsigned16? (- n))))
              (emit stw src `(reg . ,breg) `(imm ,n) code*)]
             [(index) (n ireg breg)
              (safe-assert (eqv? n 0))
              (emit stwx src `(reg . ,breg) `(reg . ,ireg) code*)]
             [else (bad!)])]
          [else (bad!)]))))

  (define-who asm-move/extend
    (lambda (op)
      (lambda (code* dest src)
        (Trivit (dest src)
          (record-case src
            [(reg) ignore 
             (case op
               [(sext8) (emit extsb dest src code*)]
               [(sext16) (emit extsh dest src code*)]
               [(zext8) (emit andi. dest src `(imm #xff) code*)]
               [(zext16) (emit andi. dest src `(imm #xffff) code*)]
               [else (sorry! who "unexpected op ~s" op)])]
            [(disp) (n breg)
             (safe-assert (integer16? n))
             (case op
               [(sext8) (emit lbz dest breg `(imm ,n)
                          (emit extsb dest dest code*))]
               [(sext16) (emit lha dest breg `(imm ,n) code*)]
               [(zext8) (emit lbz dest breg `(imm ,n) code*)]
               [(zext16) (emit lhz dest breg `(imm ,n) code*)]
               [else (sorry! who "unexpected op ~s" op)])]
            [(index) (n ireg breg)
             (safe-assert (eqv? n 0))
             (case op
               [(sext8) (emit lbzx dest breg ireg
                          (emit extsb dest dest code*))]
               [(sext16) (emit lhax dest breg ireg code*)]
               [(zext8) (emit lbzx dest breg ireg code*)]
               [(zext16) (emit lhzx dest breg ireg code*)]
               [else (sorry! who "unexpected op ~s" op)])]
            [else (sorry! who "unexpected src ~s" src)])))))

  (define asm-add
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (record-case src1
          [(imm) (n)
           (if (shifted-integer16? n)
               (emit addis dest src0
                 `(imm ,(bitwise-arithmetic-shift-right n 16))
                 code*)
               (emit addi dest src0 `(imm ,n) code*))]
          [else (emit add dest src0 src1 code*)]))))

  (define asm-add/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (let ([zed `(reg . ,%real-zero)])
          (emit addi zed zed `(imm 0)
            (emit mtxer zed
              (emit addo. dest src0 src1 code*)))))))

  (define asm-add/carry 
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit add. dest src0 src1 code*))))

  (define asm-sub-from
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit subf dest src0 src1 code*))))

  (define asm-sub-from/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (let ([zed `(reg . ,%real-zero)])
          (emit addi zed zed `(imm 0)
            (emit mtxer zed
              (emit subfo. dest src0 src1 code*)))))))

  (define asm-sub-from/eq 
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit subf. dest src0 src1 code*))))

  (module (asm-logand asm-logor asm-logxor)
    (define-syntax asm-logicalop
      (syntax-rules ()
        [(_ opi opis op)
         (lambda (code* dest src0 src1)
           (Trivit (dest src0 src1)
             (record-case src1
               [(imm) (n)
                (if (unsigned16? n)
                    (emit opi dest src0 `(imm ,n) code*)
                    (begin
                      (safe-assert (shifted-unsigned16? n))
                      (emit opis dest src0 `(imm ,(ash n -16)) code*)))]
               [else (emit op dest src0 src1 code*)])))]))

    (define asm-logand (asm-logicalop andi. andis. and))
    (define asm-logor (asm-logicalop ori oris or))
    (define asm-logxor (asm-logicalop xori xoris xor)))

  (define asm-sra
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (record-case src1
          [(imm) (n) (emit srawi dest src0 `(imm ,n) code*)]
          [else (emit sraw dest src0 src1 code*)]))))

  (define asm-srl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (record-case src1
          [(imm) (n) (emit rlwinm dest src0 `(imm ,(fx- 32 n)) `(imm ,n) `(imm 31) code*)]
          [else (emit srw dest src0 src1 code*)]))))

  (define asm-sll
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (record-case src1
          [(imm) (n) (emit rlwinm dest src0 `(imm ,n) `(imm 0) `(imm ,(fx- 31 n)) code*)]
          [else (emit slw dest src0 src1 code*)]))))

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (record-case src1
          [(imm) (n) (emit mulli dest src0 `(imm ,n) code*)]
          [else (emit mullw dest src0 src1 code*)]))))

  (define asm-mul/ovfl
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (let ([zed `(reg . ,%real-zero)])
          (emit addi zed zed `(imm 0)
            (emit mtxer zed
              (emit mullwo. dest src0 src1 code*)))))))

  (define asm-div
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit divw dest src0 src1 code*))))

  (define-who asm-load
    (lambda (info)
      (lambda (code* dest base index offset)
        (let ([type (info-load-type info)] [swapped? (info-load-swapped? info)])
          (let ([n (nanopass-case (L16 Triv) offset
                     [(immediate ,imm) imm]
                     [else (sorry! who "unexpected non-immediate offset ~s" offset)])])
            (Trivit (dest base)
              (cond
                [(eqv? n 0)
                 (let ([index (if (eq? index %zero) %real-zero index)])
                   (Trivit (index)
                     (case type
                       [(integer-32 unsigned-32)
                        (if swapped?
                            (emit lwbrx dest index base code*)
                            (emit lwzx dest index base code*))]
                       [(integer-16)
                        (if swapped?
                            (emit lhbrx dest index base
                              (emit extsh dest dest code*))
                            (emit lhax dest index base code*))]
                       [(unsigned-16)
                        (if swapped?
                            (emit lhbrx dest index base code*)
                            (emit lhzx dest index base code*))]
                       [(integer-8) (emit lbzx dest index base
                                      (emit extsb dest dest code*))]
                       [(unsigned-8) (emit lbzx dest index base code*)]
                       [else (sorry! who "unexpected mref type ~s" type)])))]
                [(eq? index %zero)
                 (case type
                   [(integer-32 unsigned-32) (emit lwz dest base `(imm ,n) code*)]
                   [(integer-16) (emit lha dest base `(imm ,n) code*)]
                   [(unsigned-16) (emit lhz dest base `(imm ,n) code*)]
                   [(integer-8) (emit lbz dest base `(imm ,n) 
                                  (emit extsb dest dest code*))]
                   [(unsigned-8) (emit lbz dest base `(imm ,n) code*)]
                   [else (sorry! who "unexpected mref type ~s" type)])]
                [else (sorry! who "expected %zero base or 0 offset, got ~s and ~s" base offset)])))))))

  (define-who asm-store
    (lambda (info)
      (lambda (code* base index offset src)
        (let ([type (info-load-type info)] [swapped? (info-load-swapped? info)])
          (let ([n (nanopass-case (L16 Triv) offset
                     [(immediate ,imm) imm]
                     [else (sorry! who "unexpected non-immediate offset ~s" offset)])])
            (Trivit (src base)
              (cond
                [(eqv? n 0)
                 (let ([index (if (eq? index %zero) %real-zero index)])
                   (Trivit (index)
                     (case type
                       [(integer-32 unsigned-32)
                         (if swapped?
                             (emit stwbrx src index base code*)
                             (emit stwx src index base code*))]
                       [(integer-16 unsigned-16)
                         (if swapped?
                             (emit sthbrx src index base code*)
                             (emit sthx src index base code*))]
                       [(integer-8 unsigned-8) (emit stbx src index base code*)]
                       [else (sorry! who "unexpected mref type ~s" type)])))]
                [(eq? index %zero)
                 (case type
                   [(integer-32 unsigned-32) (emit stw src base `(imm ,n) code*)]
                   [(integer-16 unsigned-16) (emit sth src base `(imm ,n) code*)]
                   [(integer-8 unsigned-8) (emit stb src base `(imm ,n) code*)]
                   [else (sorry! who "unexpected mref type ~s" type)])]
                [else (sorry! who "expected %zero base or 0 offset, got ~s and ~s" base offset)])))))))

  ;; load single->double
  ;; lfs  frD <- [rA + d]
  ;; lfsx frD <- [rA + rB]
  ;; load double
  ;; lfd  frD <- [rA + d]
  ;; lfdx frD <- [rA + rB]
  ;; store double
  ;; stfd  [rA + d] <- frS
  ;; stfdx [rA + rB] <- frS
  ;; store double->single
  ;; stfs  [rA + d] <- frS
  ;; stfsx [rA + rB] <- frS
  (define asm-fl-load/store
    (lambda (op flreg)
      (lambda (code* base index offset)
        (Trivit (flreg base)
          (define-syntax finish
            (syntax-rules ()
              [(_ op opx code*)
               (if (eq? index %zero)
                   (Trivit (offset)
                     (emit op flreg base offset code*))
                   (Trivit (index)
                     (emit opx flreg base index code*)))]))
          (case op
            [(load-single load-single->double) (finish lfs lfsx code*)]
            [(load-double) (finish lfd lfdx code*)]
            [(load-double->single) 
             (finish lfd lfdx (emit frsp flreg flreg code*))]
            [(store-single) (finish stfs stfsx code*)]
            [(store-double) (finish stfd stfdx code*)]
            [(store-single->double) 
             (emit frsp flreg flreg
               (finish stfd stfdx code*))])))))

  (define-who asm-flop-2
    (lambda (op)
      (lambda (code* src1 src2 dest)
        (let ([flreg1 `(reg . ,%flreg1)] [flreg2 `(reg . ,%flreg2)])
          (Trivit (src1 src2 dest)
            (emit lfd flreg1 src1 `(imm ,(constant flonum-data-disp))
              (emit lfd flreg2 src2 `(imm ,(constant flonum-data-disp))
                (let ([code* (emit stfd flreg1 dest `(imm ,(constant flonum-data-disp)) code*)])
                  (case op
                    [(fl+) (emit fadd flreg1 flreg1 flreg2 code*)]
                    [(fl-) (emit fsub flreg1 flreg1 flreg2 code*)]
                    [(fl*) (emit fmul flreg1 flreg1 flreg2 code*)]
                    [(fl/) (emit fdiv flreg1 flreg1 flreg2 code*)]
                    [else (sorry! who "unrecognized op ~s" op)])))))))))

  (define asm-trunc
    (lambda (code* dest src)
      (let ([flreg1 `(reg . ,%flreg1)] [Csp `(reg . ,%Csp)])
        (Trivit (dest src)
          (emit lfd flreg1 src `(imm ,(constant flonum-data-disp))
            (emit fctiwz flreg1 flreg1
              (emit stfd flreg1 Csp `(imm -8)
                (emit lwz dest Csp `(imm -4) code*))))))))

  (define asm-flt
    (lambda (code* src dest tmp)
      (Trivit (src dest tmp)
        (let ([flreg1 `(reg . ,%flreg1)]
              [flreg2 `(reg . ,%flreg2)]
              [flodat-disp `(imm ,(constant flonum-data-disp))])
          (emit xoris tmp src `(imm #x8000)
            (emit stw tmp dest `(imm ,(+ (constant flonum-data-disp) 4))
              (emit addis tmp `(reg . ,%real-zero) `(imm #x4330)
                (emit stw tmp dest flodat-disp
                  (emit lfd flreg1 dest flodat-disp
                    (ax-move-literal tmp `(literal 0 (object 4503601774854144.0))
                      (emit lfd flreg2 tmp flodat-disp
                        (emit fsub flreg1 flreg1 flreg2 
                          (emit stfd flreg1 dest flodat-disp
                            code*)))))))))))))

  (define asm-lock
    (lambda (info)
      ;  r0 = lwarx [base, index]
      ;  cmpi r0, 0
      ;  bc (ne) L1 (+3)
      ;  r0 = 1
      ;  strex r0, [base, index]
      ;L1:
      (lambda (l1 l2 offset base index)
        (values
          (Trivit (base index)
            (let ([zed `(reg . ,%real-zero)])
              (emit lwarx zed base index
                (emit cmpi zed `(imm 0)
                  (emit bne 3  ;; jumping past 3 instructions: bne, ori, and stwcx.
                    (emit addi zed zed `(imm 1)
                      (emit stwcx. zed base index '())))))))
          (asm-conditional-jump info l1 l2 offset)))))

  (define-who asm-lock+/-
    ; L:
    ;   tmp = lwarx [base,index] 
    ;   tmp = tmp +/- 1
    ;   stwcx. tmp [base,index] -- sets condition code
    ;   bc (ne) L (-3)
    ;   cmpi tmp, 0
    (lambda (op)
      (lambda (code* base index tmp)
        (let ([inc `(imm ,(case op
                            [(locked-incr!) 1]
                            [(locked-decr!) -1]
                            [else (sorry! who "unexpected op ~s" op)]))])
          (assert (not (eq? tmp %real-zero)))
          (Trivit (base index tmp)
            (emit lwarx tmp base index
              (emit addi tmp tmp inc
                (emit stwcx. tmp base index
                  ;; jumping back to the lwarx
                  (emit bne -3 
                    (emit cmpi tmp `(imm 0) code*))))))))))

  (define asm-fl-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (let ([flreg1 `(reg . ,%flreg1)] [flreg2 `(reg . ,%flreg2)])
          (Trivit (x y)
            (values
              (emit lfd flreg1 x `(imm ,(constant flonum-data-disp))
                (emit lfd flreg2 y `(imm ,(constant flonum-data-disp))
                  (emit fcmpu flreg1 flreg2
                    (if (eq? (info-condition-code-type info) 'fl<=)
                        (emit cror 1 1 3 '())
                        '()))))
              (asm-conditional-jump info l1 l2 offset)))))))

  (module (asm-relop asm-logrelop)
    (define-syntax define-asm-relop
      (syntax-rules ()
        [(_ name opi op)
         (define name
           (lambda (info)
             (lambda (l1 l2 offset x y)
               (Trivit (x y)
                 (safe-assert (ax-reg? x))
                 (values
                   (record-case y
                     [(imm) (n) (emit opi x `(imm ,n) '())]
                     [(reg) ignore (emit op x y '())]
                     [else (sorry! 'name "unexpected second operand ~s" y)])
                   (asm-conditional-jump info l1 l2 offset))))))]))
    (define-asm-relop asm-relop cmpi cmp)
    (define-asm-relop asm-logrelop cmpli cmpl))

  ;; ASM INSTRUCTIONS DONE ABOVE HERE

  (define asm-condition-code
    (lambda (info)
      (rec asm-check-flag-internal
        (lambda (l1 l2 offset)
          (values '() (asm-conditional-jump info l1 l2 offset))))))

  (define asm-save-flrv
    (lambda (code*)
      ; could instead stash flrv either in callee-save fl reg (one that we preserve ourselves in invoke-prelude) or in thread context
      (let ([Csp `(reg . ,%Csp)])
        (emit stfdu `(reg . ,%Cfpretval) Csp `(imm -8) 
          (emit stwu Csp Csp `(imm -8) code*)))))

  (define asm-restore-flrv
    (lambda (code*)
      (let ([Csp `(reg . ,%Csp)])
        (emit lfd `(reg . ,%Cfpretval) Csp `(imm 8)
          (emit addi Csp Csp `(imm 16) code*)))))

  (define asm-read-time-base
    (lambda (code* dest)
      (Trivit (dest)
        ; NB: not atomic => value will be way off on average once every 4 billion times, but we
        ; NB: don't care since consumers have to deal with incorrect values for other reasons.
        (emit mftbu dest
          (emit mftb `(reg . ,%real-zero) code*)))))

  (define asm-read-counter
    (lambda (code* dest)
      (Trivit (dest)
        ;; return zero
        (emit ori dest `(reg . ,%real-zero) `(imm 0) code*))))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
        `(ppc32-jump ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec save-ra?)
      (let ([target `(ppc32-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* dest tmp . ignore) ; ignore arguments, which must be in fixed locations
            (asm-helper-call code* target save-ra? tmp))))))

  (define asm-library-call!
    (lambda (libspec save-ra?)
      (let ([target `(ppc32-call ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* tmp . ignore) ; ignore arguments, which must be in fixed locations
            (asm-helper-call code* target save-ra? tmp))))))

  (define asm-c-simple-call
    (lambda (entry save-ra?)
      (let ([target `(ppc32-call 0 (entry ,entry))])
        (rec asm-c-simple-call-internal
          (lambda (code* tmp . ignore)
            (asm-helper-call code* target save-ra? tmp))))))

  (define-who asm-indirect-call
    (lambda (code* dest . ignore)
      (Trivit (dest)
        (unless (ax-reg? dest) (sorry! who "unexpected dest ~s" dest))
        (emit mtctr dest
          (emit bctrl code*)))))

  (define asm-direct-jump
    (lambda (l offset)
      (asm-helper-jump '() (make-funcrel 'ppc32-jump l offset))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
        `(ppc32-jump ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info))))))

  ;; NB: cleanup asm-indirect-jump call in cpnanopass so that a real tmp is
  ;; NB: assigned to this when we are jumping to a mref.
  ;; NB: (currently using %real-zero for a temporary)
  (define-who asm-indirect-jump
    (lambda (src)
      (let ([real-zero-reg `(reg . ,%real-zero)])
        (Trivit (src)
          (record-case src
            [(reg) ignore 
             (emit mtctr src
               (emit bctr '()))]
            [(disp) (n breg)
             (safe-assert (integer16? n))
             (emit lwz real-zero-reg `(reg . ,breg) `(imm ,n)
               (emit mtctr real-zero-reg
                 (emit bctr '())))]
            [(index) (n ireg breg)
             (safe-assert (eqv? n 0))
             (emit lwzx real-zero-reg `(reg . ,breg) `(reg . ,ireg) 
               (emit mtctr real-zero-reg
                 (emit bctr '())))]
            [else (sorry! who "unexpected src ~s" src)])))))

  ;; NB: kills real-zero, since it is used as a temporary here 
  (define asm-logtest
    (lambda (i? info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
          (values
            (record-case y
              [(imm) (n)
               (if (shifted-unsigned16? n)
                   (emit andis. `(reg . ,%real-zero) x
                     `(imm ,(bitwise-arithmetic-shift-right n 16)) '())
                   (emit andi. `(reg . ,%real-zero) x `(imm ,n) '()))]
              [else (emit and. `(reg . ,%real-zero) x y '())])
            (let-values ([(l1 l2) (if i? (values l2 l1) (values l1 l2))])
              (asm-conditional-jump info l2 l1 offset)))))))

  (define asm-get-tc
    (let ([target `(ppc32-call 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* dest tmp . ignore) ; dest is ignored, since it is always Cretval
        (asm-helper-call code* target #f tmp))))

  (define asm-activate-thread
    (let ([target `(ppc32-call 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* dest tmp . ignore) ; dest is ignored, since it is always Cretval
        (asm-helper-call code* target #f tmp))))

  (define asm-deactivate-thread
    (let ([target `(ppc32-call 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code* tmp . ignore)
        (asm-helper-call code* target #f tmp))))

  (define asm-unactivate-thread
    (let ([target `(ppc32-call 0 (entry ,(lookup-c-entry unactivate-thread)))])
      (lambda (code* tmp . ignore)
        (asm-helper-call code* target #f tmp))))

  (define-who asm-return-address
    (lambda (dest l incr-offset next-addr)
      (make-rachunk dest l incr-offset next-addr
        (asm-move '() dest
          (with-output-language (L16 Triv) `(label-ref ,l ,incr-offset))))))

  (define-who asm-jump
    (lambda (l next-addr)
      (make-gchunk l next-addr
        (cond
          [(local-label-offset l) =>
           (lambda (offset)
             (let ([disp (fx- next-addr offset)])
               (cond
                 [(eqv? disp 0) '()]
                 [(branch-disp? disp) (emit b `(label ,disp ,l) '())]
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
            (emit b `(label 0 ,l) '())]))))

  (define-who asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      (define get-disp
        (lambda (next-addr l)
          (if (local-label? l)
              (cond
                [(local-label-offset l) =>
                 (lambda (offset)
                   (let ([disp (fx- next-addr offset)])
                     (unless (branch-disp? disp) (sorry! who "no support for code objects > 32MB in length"))
                     disp))]
                [else 0])
              (sorry! who "unexpected label ~s" l))))
      (define-syntax define-pred-emitter
        (lambda (x)
          (define build-emit
            (lambda (op)
              (with-syntax ([op op])
                #'(emit op disp/opnd code*))))
          (define process-r
            (lambda (r*)
              (if (null? r*)
                  '()
                  (syntax-case (car r*) (r?)
                    [(r? op1 op2)
                     (with-syntax ([op1 (build-emit #'op1)] [op2 (build-emit #'op2)])
                       (cons #'(if r? op1 op2) (process-r (cdr r*))))]
                    [op (identifier? #'op) (cons (build-emit #'op) (process-r (cdr r*)))]))))
          (syntax-case x (i?)
            [(_ name [(ops ...) (i? r1 r2)] ...)
             (with-syntax ([(r1 ...) (process-r #'(r1 ...))]
                           [(r2 ...) (process-r #'(r2 ...))])
               #'(define name
                   (lambda (op i? r? disp/opnd code*)
                     (case op
                       [(ops ...) (if i? r1 r2)] ...))))])))
      (define-pred-emitter emit-branch
        [(fl= eq?) (i? bne beq)]
        [(fl< < u<) (i? (r? ble bge) (r? bgt blt))]
        [(fl<= <=) (i? (r? blt bgt) (r? bge ble))]
        [(>) (i? (r? bge ble) (r? blt bgt))]
        [(>=) (i? (r? bgt blt) (r? ble bge))]
        [(carry multiply-overflow overflow) (i? bns bso)])
      (let ([type (info-condition-code-type info)]
            [reversed? (info-condition-code-reversed? info)])
        (make-cgchunk info l1 l2 next-addr
          (let ([disp1 (get-disp next-addr l1)] [disp2 (get-disp next-addr l2)])
            (cond
              ;; 1 conditional jump
              [(and (fx= disp1 0) (conditional-branch-disp? disp2))
               (emit-branch type #t reversed? `(label ,disp2 ,l2) '())]
              [(and (fx= disp2 0) (conditional-branch-disp? disp1))
               (emit-branch type #f reversed? `(label ,disp1 ,l1) '())]
              ;; 1 conditional jump, 1 unconditional jump
              [(conditional-branch-disp? (fx+ disp2 4))
               (emit-branch type #t reversed? `(label ,(fx+ disp2 4) ,l2)
                 (emit b `(label ,disp1 ,l1) '()))]
              [(conditional-branch-disp? (fx+ disp1 4))
               (emit-branch type #f reversed? `(label ,(fx+ disp1 4) ,l1)
                 (emit b `(label ,disp2 ,l2) '()))]
              ;;     jmp<condition> L1
              ;;     jmp dest1
              ;; L1: jmp dest2
              [else
               ;; jumping past 2 instructions, the branch and b opnd1
               (emit-branch type #t reversed? 2
                 (emit b `(label ,(fx+ disp1 4) ,l1)
                   (emit b `(label ,disp2 ,l2) '())))]))))))

  (define asm-data-label
    (lambda (code* l offset func code-size)
      (let ([rel (make-funcrel 'abs l offset)])
        (cons* rel (aop-cons* `(asm "mrv point:" ,rel) code*)))))

  (define asm-helper-jump
    (lambda (code* reloc)
      (emit nop
        (emit nop
          (emit nop
            (emit nop 
              (asm-helper-relocation reloc code*)))))))

  (define asm-kill
    (lambda (code* dest)
      code*))

  (define asm-helper-call
    (lambda (code* reloc save-ra? tmp)
      (Trivit (tmp)
        ;; NB. saves lr into the local variable space for the frame we are
        ;; NB. creating.
        (define ax-save/restore
          (lambda (code* tmp p)
            (let ([Csp `(reg . ,%Csp)])
              (emit mflr tmp
                (emit stwu Csp Csp `(imm -16)
                  (emit stw tmp Csp `(imm 12)
                    (p (emit lwz tmp Csp `(imm 12)
                         (emit mtlr tmp
                           (emit addi Csp Csp `(imm 16) code*))))))))))
        (define maybe-save-ra
          (lambda (code* p)
            (if save-ra?
                (ax-save/restore code* tmp p)
                (p code*))))
        (maybe-save-ra code*
          (lambda (code*)
            (emit nop
              (emit nop
                (emit nop
                  (emit nop 
                    (asm-helper-relocation reloc code*))))))))))

  (define asm-helper-relocation
    (lambda (reloc code*)
      (cons* reloc (aop-cons* `(asm "relocation:" ,reloc) code*))))

  (define asm-rp-header
    (let ([mrv-error `(abs ,(constant code-data-disp)
                        (library-code ,(lookup-libspec values-error)))])
      (lambda (code* mrvl fs lpm func code-size)
        (cons*
          (if (target-fixnum? lpm)
              `(long . ,(fix lpm))
              `(abs 0 (object ,lpm)))
          (aop-cons* `(asm livemask: ,(format "~b" lpm))
            '(code-top-link)
            (aop-cons* `(asm code-top-link)
              `(long . ,fs)
              (aop-cons* `(asm "frame size:" ,fs)
                (if mrvl
                    (asm-data-label code* mrvl 0 func code-size)
                    (cons*
                      mrv-error
                      (aop-cons* `(asm "mrv point:" ,mrv-error)
                        code*))))))))))

  (define asm-return 
    (lambda () 
      (emit blr '())))

  (define asm-c-return 
    (lambda (info) 
      (emit blr '())))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit nor dest src src code*))))

  (define asm-enter values)
  
  (define-who asm-inc-cc-counter
    (lambda (code* addr val tmp)
      (assert (not (eq? tmp %zero)))
      (Trivit (addr val tmp)
        (define do-ldr
          (lambda (offset k code*)
            (emit lwz tmp addr `(imm ,offset) (k (emit stw tmp addr `(imm ,offset) code*)))))
        (define do-add/carry
          (lambda (code*)
            (emit addo. tmp tmp val code*)))
        (do-ldr 4
          do-add/carry
          (emit bns 4
            (do-ldr 0
              (lambda (code*)
                (emit addi tmp tmp `(imm 1) code*))
              code*))))))

  (define asm-store-with-update
    (lambda (code* src base idx/off)
      (Trivit (src base idx/off)
        (record-case idx/off
          [(imm) (n) (emit stwu src base `(imm ,n) code*)]
          [else (emit stwux src base idx/off code*)])))) 

  (define asm-get-lr
    (lambda ()
      (lambda (code* dest)
        (Trivit (dest)
          (emit mflr dest code*)))))
  
  (define asm-set-lr
    (lambda ()
      (lambda (code* src)
        (Trivit (src)
          (emit mtlr src code*)))))

  (define asm-isync
    (lambda (code*)
      (emit isync code*)))
  
  (module (asm-foreign-call asm-foreign-callable)
    (define align (lambda (b x) (let ([k (- b 1)]) (fxlogand (fx+ x k) (fxlognot k)))))
    (define gp-parameter-regs (lambda () (list %Carg1 %Carg2 %Carg3 %Carg4 %Carg5 %Carg6 %Carg7 %Carg8)))
    (define fp-parameter-regs (lambda () (list %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4 %Cfparg5 %Cfparg6 %Cfparg7 %Cfparg8)))
    (define fp-result-regs (lambda () (list %Cfpretval)))
    (define (indirect-result-that-fits-in-registers? result-type)
      (nanopass-case (Ltype Type) result-type
        [(fp-ftd& ,ftd) (not ($ftd-compound? ftd))]
	[else #f]))
    (define (indirect-result-to-pointer? result-type)
      (nanopass-case (Ltype Type) result-type
        [(fp-ftd& ,ftd) ($ftd-compound? ftd)]
	[else #f]))

    (module (push-registers pop-registers)
      ;; stack offset must be 8-byte aligned if fp-reg-count is non-zero
      (define (move-registers regs fp-reg-count fp-regs load? offset e)
	(with-output-language (L13 Effect)
          (cond
	   [(fx> fp-reg-count 0)
	    ;; Push floating-point first to get correct alignment
	    (let ([offset (align 8 offset)])
	      (move-registers regs (fx- fp-reg-count 1) (cdr fp-regs) load? (fx+ offset 8)
			      (cond
			       [load? `(seq ,e (inline ,(make-info-loadfl (car fp-regs)) ,%load-double ,%sp ,%zero (immediate ,offset)))]
			       [else  `(seq (inline ,(make-info-loadfl (car fp-regs)) ,%store-double ,%sp ,%zero (immediate ,offset)) ,e)])))]
	   [(pair? regs)
	    (move-registers (cdr regs) 0 '() load? (fx+ offset 4)
			    (cond
			     [load? `(seq ,e (set! ,(car regs) ,(%mref ,%sp ,offset)))]
			     [else  `(seq (set! ,(%mref ,%sp ,offset) ,(car regs)) ,e)]))]
	   [else e])))
      ;; Add "pushes" before e
      (define (push-registers regs fp-reg-count fp-regs offset e)
	(move-registers regs fp-reg-count fp-regs #f offset e))
      ;; Add "pops" after e
      (define (pop-registers regs fp-reg-count fp-regs offset e)
        (move-registers regs fp-reg-count fp-regs #t offset e)))

    (define-who asm-foreign-call
      (with-output-language (L13 Effect)
        (define load-double-stack
          (lambda (offset fp-disp)
            (lambda (x) ; requires var
              (%seq
                (inline ,(make-info-loadfl %flreg1) ,%load-double ,x ,%zero (immediate ,fp-disp))
                (inline ,(make-info-loadfl %flreg1) ,%store-double ,%sp ,%zero (immediate ,offset))))))
        (define load-single-stack
          (lambda (offset fp-disp single?)
            (lambda (x) ; requires var
              (%seq
                (inline ,(make-info-loadfl %flreg1) ,(if single? %load-single %load-double->single) ,x ,%zero (immediate ,fp-disp))
                (inline ,(make-info-loadfl %flreg1) ,%store-single ,%sp ,%zero (immediate ,offset))))))
        (define load-int-stack
          (lambda (offset)
            (lambda (rhs) ; requires rhs
              `(set! ,(%mref ,%sp ,offset) ,rhs))))
        (define load-int64-stack
          (lambda (offset)
            (lambda (lorhs hirhs) ; requires rhs
              (%seq
                (set! ,(%mref ,%sp ,(fx+ offset 4)) ,lorhs)
                (set! ,(%mref ,%sp ,offset) ,hirhs)))))
        (define load-indirect-int-stack
          (lambda (offset size)
            (lambda (rhs) ; requires rhs
	      (let ([int-type (case size
				[(1) 'integer-8]
				[(2) 'integer-16]
				[else 'integer-32])])
              `(set! ,(%mref ,%sp ,offset) (inline ,(make-info-load int-type #f) ,%load ,rhs ,%zero (immediate ,0)))))))
        (define load-indirect-int64-stack
          (lambda (offset)
            (lambda (x) ; requires var
              `(seq
		(set! ,(%mref ,%sp ,offset) ,(%mref ,x 0))
		(set! ,(%mref ,%sp ,(fx+ offset 4)) ,(%mref ,x 4))))))
        (define load-double-reg
          (lambda (fpreg fp-disp)
            (lambda (x) ; requires var
              `(inline ,(make-info-loadfl fpreg) ,%load-double ,x ,%zero (immediate ,fp-disp)))))
        (define load-soft-double-reg
          (lambda (loreg hireg fp-disp)
            (lambda (x)
               (%seq
                 (set! ,loreg ,(%mref ,x ,(fx+ fp-disp 4)))
                 (set! ,hireg ,(%mref ,x ,fp-disp))))))
        (define load-single-reg
          (lambda (fpreg fp-disp single?)
            (lambda (x) ; requires var
              `(inline ,(make-info-loadfl fpreg) ,(if single? %load-single %load-double->single) ,x ,%zero (immediate ,fp-disp)))))
        (define load-soft-single-reg
          (lambda (ireg fp-disp single?)
            (lambda (x)
              (%seq
                (inline ,(make-info-loadfl %flreg1) ,(if single? %load-single %load-double->single) ,x ,%zero (immediate ,fp-disp))
                (inline ,(make-info-loadfl %flreg1) ,%store-single ,%tc ,%zero (immediate ,(constant tc-ac0-disp)))
                (set! ,ireg ,(%tc-ref ac0))))))
        (define load-int-reg
          (lambda (ireg)
            (lambda (x) ; requires rhs
              `(set! ,ireg ,x))))
        (define load-int64-reg
          (lambda (loreg hireg)
            (lambda (lo hi) ; requires two rhss
              (%seq
                (set! ,loreg ,lo)
                (set! ,hireg ,hi)))))
        (define load-indirect-int-reg
          (lambda (ireg size category)
            (lambda (rhs) ; requires var
	      (let ([int-type (case category
				[(unsigned) (case size
					      [(1) 'unsigned-8]
					      [(2) 'unsigned-16]
					      [else 'unsigned-32])]
				[else (case size
					[(1) 'integer-8]
					[(2) 'integer-16]
					[else 'integer-32])])])
		`(set! ,ireg (inline ,(make-info-load int-type #f) ,%load ,rhs ,%zero (immediate ,0)))))))
        (define load-indirect-int64-reg
          (lambda (loreg hireg)
            (lambda (x) ; requires var
              `(seq
		(set! ,hireg ,(%mref ,x 0))
		(set! ,loreg ,(%mref ,x 4))))))
        (define do-args
          (lambda (types)
            ;; NB: start stack pointer at 8 to put arguments above the linkage area
            (let loop ([types types] [locs '()] [live* '()] [int* (gp-parameter-regs)] [flt* (fp-parameter-regs)] [isp 8]
		       ;; needed when adjusting active:
		       [fp-live-count 0]
		       ;; configured for `ftd-fp&` unpacking of floats:
		       [fp-disp (constant flonum-data-disp)] [single? #f])
              (if (null? types)
                  (values isp locs live* fp-live-count)
                  (nanopass-case (Ltype Type) (car types)
                    [(fp-double-float)
                     (if (constant software-floating-point)
                         (let ([int* (if (even? (length int*)) int* (cdr int*))])
                           (if (null? int*)
                               (let ([isp (align 8 isp)])
                                 (loop (cdr types)
                                   (cons (load-double-stack isp fp-disp) locs)
                                   live* '() flt* (fx+ isp 8) fp-live-count
				   (constant flonum-data-disp) #f))
                               (loop (cdr types)
                                 (cons (load-soft-double-reg (cadr int*) (car int*) fp-disp) locs)
                                 (cons* (car int*) (cadr int*) live*) (cddr int*) flt* isp fp-live-count
				 (constant flonum-data-disp) #f)))
                         (if (null? flt*)
                             (let ([isp (align 8 isp)])
                               (loop (cdr types)
                                 (cons (load-double-stack isp fp-disp) locs)
                                 live* int* '() (fx+ isp 8) fp-live-count
				 (constant flonum-data-disp) #f))
                             (loop (cdr types)
                               (cons (load-double-reg (car flt*) fp-disp) locs)
                               live* int* (cdr flt*) isp (fx+ fp-live-count 1)
			       (constant flonum-data-disp) #f)))]
                    [(fp-single-float)
                     (if (constant software-floating-point)
                         (if (null? int*)
                             ; NB: ABI says singles are passed as doubles on the stack, but gcc/linux doesn't
                             (loop (cdr types)
                               (cons (load-single-stack isp fp-disp single?) locs)
                               live* '() flt* (fx+ isp 4) fp-live-count
			       (constant flonum-data-disp) #f)
                             (loop (cdr types)
                               (cons (load-soft-single-reg (car int*) fp-disp single?) locs)
                               (cons (car int*) live*) (cdr int*) flt* isp fp-live-count
			       (constant flonum-data-disp) #f))
                         (if (null? flt*)
                             ; NB: ABI says singles are passed as doubles on the stack, but gcc/linux doesn't
                             (let ([isp (align 4 isp)])
                               (loop (cdr types)
                                 (cons (load-single-stack isp fp-disp single?) locs)
                                 live* int* '() (fx+ isp 4) fp-live-count
				 (constant flonum-data-disp) #f))
                             (loop (cdr types)
                               (cons (load-single-reg (car flt*) fp-disp single?) locs)
                               live* int* (cdr flt*) isp (fx+ fp-live-count 1)
			       (constant flonum-data-disp) #f)))]
                    [(fp-ftd& ,ftd)
                     (cond
                      [($ftd-compound? ftd)
                       ;; pass as pointer
		       (let ([pointer-type (with-output-language (Ltype Type) `(fp-integer 32))])
			 (loop (cons pointer-type (cdr types)) locs live* int* flt* isp fp-live-count
			       (constant flonum-data-disp) #f))]
                      [else
                       ;; extract content and pass that content
		       (let ([category ($ftd-atomic-category ftd)])
			 (cond
			  [(eq? category 'float)
			   ;; piggy-back on unboxed handler
			   (let ([unpacked-type (with-output-language (Ltype Type)
						  (case ($ftd-size ftd)
						    [(4) `(fp-single-float)]
						    [else `(fp-double-float)]))])
			     (loop (cons unpacked-type (cdr types)) locs live* int* flt* isp fp-live-count
				   ;; no floating displacement within pointer:
				   0
				   ;; in case of float, load as single-float:
				   (= ($ftd-size ftd) 4)))]
			  [(and (memq category '(integer unsigned))
				(fx= 8 ($ftd-size ftd)))
			   (let ([int* (if (even? (length int*)) int* (cdr int*))])
			     (if (null? int*)
                               (let ([isp (align 8 isp)])
                                 (loop (cdr types)
                                   (cons (load-indirect-int64-stack isp) locs)
                                   live* '() flt* (fx+ isp 8) fp-live-count
				   (constant flonum-data-disp) #f))
                               (loop (cdr types)
                                 (cons (load-indirect-int64-reg (cadr int*) (car int*)) locs)
                                 (cons* (car int*) (cadr int*) live*) (cddr int*) flt* isp fp-live-count
				 (constant flonum-data-disp) #f)))]
			  [else
			   (if (null? int*)
			       (loop (cdr types)
                                 (cons (load-indirect-int-stack isp ($ftd-size ftd)) locs)
                                 live* '() flt* (fx+ isp 4) fp-live-count
  			         (constant flonum-data-disp) #f)
			       (loop (cdr types)
                                 (cons (load-indirect-int-reg (car int*) ($ftd-size ftd) category) locs)
                                 (cons (car int*) live*) (cdr int*) flt* isp fp-live-count
				 (constant flonum-data-disp) #f))]))])]
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
                                   live* '() flt* (fx+ isp 8) fp-live-count
				   (constant flonum-data-disp) #f))
                               (loop (cdr types)
                                 (cons (load-int64-reg (cadr int*) (car int*)) locs)
                                 (cons* (car int*) (cadr int*) live*) (cddr int*) flt* isp fp-live-count
				 (constant flonum-data-disp) #f)))
                         (if (null? int*)
                             (loop (cdr types)
                               (cons (load-int-stack isp) locs)
                               live* '() flt* (fx+ isp 4) fp-live-count
			       (constant flonum-data-disp) #f)
                             (loop (cdr types)
                               (cons (load-int-reg (car int*)) locs)
                               (cons (car int*) live*) (cdr int*) flt* isp fp-live-count
			       (constant flonum-data-disp) #f)))])))))
	(define do-indirect-result-from-registers
	  (lambda (ftd offset)
	    (let ([tmp %Carg8])
	      (%seq
	       (set! ,tmp ,(%mref ,%sp ,offset))
	       ,(cond
		 [(and (not (constant software-floating-point))
		       (eq? 'float ($ftd-atomic-category ftd)))
		  `(inline ,(make-info-loadfl %Cfpretval) ,(if (= 4 ($ftd-size ftd)) %store-single %store-double)
			   ,tmp ,%zero (immediate 0))]
		 [else
		  (case ($ftd-size ftd)
		    [(1) `(inline ,(make-info-load 'integer-8 #f) ,%store ,tmp ,%zero (immediate 0) ,%Cretval)]
		    [(2) `(inline ,(make-info-load 'integer-16 #f) ,%store ,tmp ,%zero (immediate 0) ,%Cretval)]
		    [(4) `(inline ,(make-info-load 'integer-32 #f) ,%store ,tmp ,%zero (immediate 0) ,%Cretval)]
		    [(8)
		     (%seq
		      (inline ,(make-info-load 'integer-32 #f) ,%store ,tmp ,%zero (immediate 0) ,%Cretval-high)
		      (inline ,(make-info-load 'integer-32 #f) ,%store ,tmp ,%zero (immediate 4) ,%Cretval-low))]
		    [else (sorry! who "unexpected result size")])])))))
	(define (add-deactivate t0 offset live* fp-live-count result-live* result-fp-live-count e)
	  (let ([save-and-restore
		 (lambda (regs fp-count fp-regs e)
		   (cond
		    [(and (null? regs) (fx= 0 fp-count)) e]
		    [else
		     (pop-registers regs fp-count fp-regs offset
				    (push-registers regs fp-count fp-regs offset
						    e))]))])
	    (%seq
	     (set! ,%deact ,t0)
	     ,(save-and-restore (cons %deact live*) fp-live-count (fp-parameter-regs) (%inline deactivate-thread))
	     ,e
	     ,(save-and-restore result-live* result-fp-live-count (fp-result-regs) `(set! ,%Cretval ,(%inline activate-thread))))))
        (lambda (info)
          (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
          (let* ([arg-type* (info-foreign-arg-type* info)]
		 [result-type (info-foreign-result-type info)]
		 [fill-result-here? (indirect-result-that-fits-in-registers? result-type)]
		 [adjust-active? (memq 'adjust-active (info-foreign-conv info))])
            (with-values (do-args (if fill-result-here? (cdr arg-type*) arg-type*))
              (lambda (orig-frame-size locs live* fp-live-count)
                ;; NB: add 4 to frame size for CR save word
                (let* ([fill-stash-offset orig-frame-size]
		       [base-frame-size (fx+ orig-frame-size (if fill-result-here? 4 0))]
		       [deactivate-save-offset (if (and adjust-active? (fx> fp-live-count 0))
						   (align 8 base-frame-size) ; for `double` save
						   base-frame-size)]
		       [frame-size (align 16 (fx+ 4 ; for CR save
						  (if adjust-active?
						      (fx+ deactivate-save-offset
							   (fx* fp-live-count 8)
							   (fx* (length live*) 4))
						      deactivate-save-offset)))])
                  (values
                    (lambda () (%inline store-with-update ,%Csp ,%Csp (immediate ,(fx- frame-size))))
                    (let ([locs (reverse locs)])
		      (cond
		       [fill-result-here?
			;; stash extra argument on the stack to be retrieved after call and filled with the result:
			(cons (load-int-stack fill-stash-offset) locs)]
		       [else locs]))
                    (lambda (t0)
		      (define (make-call result-live* result-fp-live-count)
			(cond
			 [adjust-active?
			  (add-deactivate t0 deactivate-save-offset live* fp-live-count result-live* result-fp-live-count
					  `(inline ,(make-info-kill*-live* result-live* live*) ,%c-call ,%deact))]
			 [else `(inline ,(make-info-kill*-live* result-live* live*) ,%c-call ,t0)]))
		      (if (constant software-floating-point)
                          (let ()
                            (define handle-64-bit
                              (lambda ()
				(make-call (reg-list %Cretval-high %Cretval-low) 0)))
                            (define handle-32-bit
                              (lambda ()
                                (make-call (reg-list %Cretval) 0)))
                            (define handle-integer-cases
                              (lambda (bits)
                                (case bits
                                  [(8 16 32) (handle-32-bit)]
                                  [(64) (handle-64-bit)]
                                  [else (sorry! who "unexpected asm-foreign-procedures fp-integer size ~s" bits)])))
			    (define (handle-ftd&-case ftd)
			      (cond
			       [fill-result-here?
				(%seq
				 ,(if (> ($ftd-size ftd) 4)
				      (handle-64-bit)
				      (handle-32-bit))
				 ,(do-indirect-result-from-registers ftd fill-stash-offset))]
			       [else (make-call (reg-list) 0)]))
                            (nanopass-case (Ltype Type) result-type
                              [(fp-double-float) (handle-64-bit)]
                              [(fp-single-float) (handle-32-bit)]
                              [(fp-integer ,bits) (handle-integer-cases bits)]
                              [(fp-integer ,bits) (handle-integer-cases bits)]
			      [(fp-ftd& ,ftd) (handle-ftd&-case ftd)]
                              [else (make-call (reg-list %Cretval) 0)]))
                          (let ()
                            (define handle-integer-cases
                              (lambda (bits)
                                (case bits
                                  [(8 16 32) (make-call (reg-list %Cretval) 0)]
                                  [(64) (make-call (reg-list %Cretval-high %Cretval-low) 0)]
                                  [else (sorry! who "unexpected asm-foreign-procedures fp-integer size ~s" bits)])))
			    (define (handle-ftd&-case ftd)
			      (cond
			       [fill-result-here?
				(%seq
				 ,(if (not (eq? 'float ($ftd-atomic-category ftd)))
				      (handle-integer-cases (* 8 ($ftd-size ftd)))
				      (make-call (reg-list) 1))
				 ,(do-indirect-result-from-registers ftd fill-stash-offset))]
			       [else `(inline ,(make-info-kill*-live* (reg-list) live*) ,%c-call ,t0)]))
                            (nanopass-case (Ltype Type) result-type
                              [(fp-double-float) (make-call (reg-list) 1)]
                              [(fp-single-float) (make-call (reg-list) 1)]
                              [(fp-integer ,bits) (handle-integer-cases bits)]
                              [(fp-unsigned ,bits) (handle-integer-cases bits)]
			      [(fp-ftd& ,ftd) (handle-ftd&-case ftd)]
                              [else (make-call (reg-list %Cretval) 0)]))))
                    (nanopass-case (Ltype Type) result-type
                      [(fp-double-float)
                       (lambda (lvalue)
                         (if (constant software-floating-point)
                             (%seq
                               (set! ,(%mref ,lvalue ,(constant flonum-data-disp)) ,%Cretval-high)
                               (set! ,(%mref ,lvalue ,(fx+ (constant flonum-data-disp) 4)) ,%Cretval-low))
                             `(inline ,(make-info-loadfl %Cfpretval) ,%store-double ,lvalue ,%zero
                                ,(%constant flonum-data-disp))))]
                      [(fp-single-float)
                       (lambda (lvalue)
                         (if (constant software-floating-point)
                             (%seq
                               (set! ,(%tc-ref ac0) ,%Cretval)
                               (inline ,(make-info-loadfl %flreg1) ,%load-single->double ,%tc ,%zero (immediate ,(constant tc-ac0-disp)))
                               (inline ,(make-info-loadfl %flreg1) ,%store-double ,lvalue ,%zero ,(%constant flonum-data-disp)))
                             `(inline ,(make-info-loadfl %Cfpretval) ,%store-single->double
                                ,lvalue ,%zero ,(%constant flonum-data-disp))))]
                      [(fp-integer ,bits)
                       (case bits
                         [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline sext8 ,%Cretval)))]
                         [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline sext16 ,%Cretval)))]
                         [(32) (lambda (lvalue) `(set! ,lvalue ,%Cretval))]
                         [(64) (lambda (lvlow lvhigh)
                                 `(seq
                                    (set! ,lvhigh ,%Cretval-high)
                                    (set! ,lvlow ,%Cretval-low)))]
                         [else (sorry! who "unexpected asm-foreign-procedures fp-integer size ~s" bits)])]
                      [(fp-unsigned ,bits)
                       (case bits
                         [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline zext8 ,%Cretval)))]
                         [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline zext16 ,%Cretval)))]
                         [(32) (lambda (lvalue) `(set! ,lvalue ,%Cretval))]
                         [(64) (lambda (lvlow lvhigh)
                                 `(seq
                                    (set! ,lvhigh ,%Cretval-high)
                                    (set! ,lvlow ,%Cretval-low)))]
                         [else (sorry! who "unexpected asm-foreign-procedures fp-unsigned size ~s" bits)])]
                      [else (lambda (lvalue) `(set! ,lvalue ,%Cretval))])
                    (lambda () `(set! ,%sp ,(%inline + ,%sp (immediate ,frame-size))))))))))))

    (define-who asm-foreign-callable
      #|
       PPC general frame layout (f calls g calls h)
                   +---------------------------+
                   |                           |
                   |      parameter list       | 0-? words (g's stack arguments from f)
           sp+n+8: |                           |
                   +---------------------------+
                   |                           |
                   |            lr             | 1 word (place for g to store lr)
           sp+n+4: |                           |
                   +---------------------------+
                   |                           |
   f's frame       |        back chain         | 1 word                                    
             sp+n: |                           |         <--------------------------------+
                   +---------------------------+                                          |
                   +---------------------------+                                          |
                   |                           |                                          |
                   |    floating-point regs    | 0-18 double-words (g's callee-save fprs) |
     sp+8+X+Y+1+Z: |                           |                                          |
                   +---------------------------+                                          |
                   |                           |                                          |
                   |       integer regs        | 0-18 words (g's callee-saved gprs)       |
       sp+8+X+Y+1: |                           |                                          |
                   +---------------------------+                                          |
                   |                           |                                          |
                   |     control register      | 1 word (g's saved cr)                    |
         sp+8+X+Y: |                           |                                          |
                   +---------------------------+                                          |
                   |                           |                                          |
                   |   local variable space    | 0-? words                                |
           sp+8+X: |       (and padding)       |                                          |
                   +---------------------------+                                          |
                   |                           |                                          |
                   |      parameter list       | 0-? words (h's stack arguments from g)   |
             sp+8: |                           |                                          |
                   +---------------------------+                                          |
                   |                           |                                          |
                   |            lr             | 1 word (place for h to store lr)         |
             sp+4: |                           |                                          |
                   +---------------------------+                                          |
                   |                           |                                          |
   g's frame       |        back chain         | 1 word  ---------------------------------+
             sp+0: |          [sp+n]           |
                   +---------------------------+
                   +---------------------------+
                   |                           |
   h's frame       |                           |
                   |                           |

         X = number of bytes for parameters
         Y = number of bytes for local variables
         Z = number of bytes for callee-save gp regs
         n = 8 + X + Y + 1 + Z + [number of bytes for callee-save fp regs]

       PPC foreign-callable Frame Layout
           sp+188:
                   +---------------------------+
                   |                           |
                   |            lr             | 1 word
           sp+X+4: |                           |
                   +---------------------------+
                   |                           |
                   |        back chain         | 1 word
             sp+X: |                           |
                   +---------------------------+ <- 16-byte aligned
                   +---------------------------+
                   +---------------------------+ <- 16-byte aligned
                   |                           |
                   |       &-return space      | 2 words, if needed
                   |                           |
                   +---------------------------+ <- 8-byte aligned
                   |      unactivate mode      | 1 word, if needed
                   +---------------------------+
                   |                           |
                   |      callee-save regs     |
                   |                           |
                   +---------------------------+
                   |                           |
                   |  floating-point arg regs  | 
                   |                           |
                   +---------------------------+ <- 8-byte aligned
                   |                           |
                   |   integer argument regs   |   Also used to stash results during unactivate
                   |                           |
             sp+8: +---------------------------+ <- 8-byte aligned
                   |                           |
                   |            lr             | 1 word (place for get-thread-context to store lr)
                   |                           |
                   +---------------------------+
                   |                           |
                   |        back chain         | 1 word
             sp+0: |         [sp+X-4]          |
                   +---------------------------+

       FOR foreign callable (nb: assuming flreg1 & flreg2 are caller-save):
                 decrement sp by 176 & set up back chain (atomically)
                 save gp arg regs (based on number declared by foreign-callable form) at sp+8
                 save fp arg regs (based on number declared by foreign-callable form) at sp+40
                 don't bother saving cr
                 save callee-save gp registers at sp+108 (could avoid those we don't use during argument conversion, if we knew what they were)
                 save lr at sp[188] (actually sp 4, before sp is moved)
                 if threaded:
                   call get-thread-context
                 else
                   tc <- thread-context
                 endif
                 ...
                 restore lr from sp[188]

       INVARIANTS
         stack grows down
         each frame 16-byte aligned
      |#
      (with-output-language (L13 Effect)
        (let ()
          (define load-double-stack
            (lambda (offset)
              (lambda (x) ; requires var
                (%seq
                  (inline ,(make-info-loadfl %flreg1) ,%load-double ,%sp ,%zero (immediate ,offset))
                  (inline ,(make-info-loadfl %flreg1) ,%store-double ,x ,%zero ,(%constant flonum-data-disp))))))
          (define load-soft-single-stack
            (lambda (offset)
              (lambda (x) ; requires var
                (%seq
                  (inline ,(make-info-loadfl %flreg1) ,%load-single->double ,%sp ,%zero (immediate ,offset))
                  (inline ,(make-info-loadfl %flreg1) ,%store-double ,x ,%zero ,(%constant flonum-data-disp))))))
          (define load-int-stack
            (lambda (type offset)
              (lambda (lvalue)
                (nanopass-case (Ltype Type) type
                  [(fp-integer ,bits)
                   (case bits
                     [(8) `(set! ,lvalue (inline ,(make-info-load 'integer-8 #f) ,%load ,%sp ,%zero (immediate ,(fx+ offset 3))))]
                     [(16) `(set! ,lvalue (inline ,(make-info-load 'integer-16 #f) ,%load ,%sp ,%zero (immediate ,(fx+ offset 2))))]
                     [(32) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                     [else (sorry! who "unexpected load-int-stack fp-integer size ~s" bits)])]
                  [(fp-unsigned ,bits)
                   (case bits
                     [(8) `(set! ,lvalue (inline ,(make-info-load 'unsigned-8 #f) ,%load ,%sp ,%zero (immediate ,(fx+ offset 3))))]
                     [(16) `(set! ,lvalue (inline ,(make-info-load 'unsigned-16 #f) ,%load ,%sp ,%zero (immediate ,(fx+ offset 2))))]
                     [(32) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                     [else (sorry! who "unexpected load-int-stack fp-unsigned size ~s" bits)])]
                  [else `(set! ,lvalue ,(%mref ,%sp ,offset))]))))
          (define load-int64-stack
            (lambda (offset)
              (lambda (lolvalue hilvalue)
                (%seq
                  (set! ,lolvalue ,(%mref ,%sp ,(fx+ offset 4)))
                  (set! ,hilvalue ,(%mref ,%sp ,offset))))))
          (define load-stack-address
            (lambda (offset)
              (lambda (lvalue)
		`(set! ,lvalue ,(%inline + ,%sp (immediate ,offset))))))
          (define load-stack-address/convert-float
            (lambda (offset)
              (lambda (lvalue)
		(%seq
		 ;; Overwrite argument on stack with single-precision version
		 ;; FIXME: is the callee allowed to do this if the argument is passed on the stack?
		 (inline ,(make-info-loadfl %flreg1) ,%load-double->single ,%sp ,%zero (immediate ,offset))
		 (inline ,(make-info-loadfl %flreg1) ,%store-single ,%sp ,%zero (immediate ,offset))
		 (set! ,lvalue ,(%inline + ,%sp (immediate ,offset)))))))
          (define count-reg-args
            (lambda (types gp-reg-count fp-reg-count synthesize-first-argument?)
              (let f ([types types] [iint (if synthesize-first-argument? -1 0)] [iflt 0])
                (if (null? types)
                    (values iint iflt)
                    (cond
                      [(and (not (constant software-floating-point))
                            (nanopass-case (Ltype Type) (car types)
                              [(fp-double-float) #t]
                              [(fp-single-float) #t]
			      [(fp-ftd& ,ftd) (eq? 'float ($ftd-atomic-category ftd))]
                              [else #f]))
                       (f (cdr types) iint (if (fx< iflt fp-reg-count) (fx+ iflt 1) iflt))]
                      [(or (nanopass-case (Ltype Type) (car types)
                             [(fp-integer ,bits) (fx= bits 64)]
                             [(fp-unsigned ,bits) (fx= bits 64)]
			     [(fp-ftd& ,ftd) (and (not ($ftd-compound? ftd))
						  (fx= 8 ($ftd-size ftd)))]
                             [else #f])
                           (and (constant software-floating-point)
                                (nanopass-case (Ltype Type) (car types)
                                  [(fp-double-float) #t]
                                  [else #f])))
                       (let ([iint (align 2 iint)])
                         (f (cdr types) (if (fx< iint gp-reg-count) (fx+ iint 2) iint) iflt))]
                      [else (f (cdr types) (if (fx< iint gp-reg-count) (fx+ iint 1) iint) iflt)])))))
          (define do-stack
            ; all of the args are on the stack at this point, though not contiguous since
            ; we push all of the int reg args with one push instruction and all of the
            ; float reg args with another (v)push instruction
            (lambda (types gp-reg-count fp-reg-count int-reg-offset float-reg-offset stack-arg-offset
			   synthesize-first-argument? return-space-offset)
              (let loop ([types (if synthesize-first-argument? (cdr types) types)]
                         [locs '()]
                         [iint 0]
                         [iflt 0]
                         [int-reg-offset int-reg-offset]
                         [float-reg-offset float-reg-offset]
                         [stack-arg-offset stack-arg-offset])
                (if (null? types)
                    (let ([locs (reverse locs)])
                      (if synthesize-first-argument?
                          (cons (load-stack-address return-space-offset)
                                locs)
                          locs))
                    (cond
                      [(and (not (constant software-floating-point))
                            (nanopass-case (Ltype Type) (car types)
                              [(fp-double-float) #t]
                              [(fp-single-float) #t]
                              [else #f]))
                       (if (fx< iflt fp-reg-count)
                           (loop (cdr types)
                             (cons (load-double-stack float-reg-offset) locs)
                             iint (fx+ iflt 1) int-reg-offset (fx+ float-reg-offset 8) stack-arg-offset)
                           (let ([stack-arg-offset (align 8 stack-arg-offset)])
                             (loop (cdr types)
                               (cons (load-double-stack stack-arg-offset) locs)
                               iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 8))))]
                      [(and (constant software-floating-point)
                            (nanopass-case (Ltype Type) (car types)
                              [(fp-double-float) #t]
                              [else #f]))
                       (let ([iint (align 2 iint)])
                         (if (fx< iint gp-reg-count)
                             (let ([int-reg-offset (align 8 int-reg-offset)])
                               (loop (cdr types)
                                 (cons (load-double-stack int-reg-offset) locs)
                                 (fx+ iint 2) iflt (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset))
                             (let ([stack-arg-offset (align 8 stack-arg-offset)])
                               (loop (cdr types)
                                 (cons (load-double-stack stack-arg-offset) locs)
                                 iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 8)))))]
                      [(and (constant software-floating-point)
                            (nanopass-case (Ltype Type) (car types)
                              [(fp-single-float) #t]
                              [else #f]))
                       (if (fx< iint gp-reg-count)
                           (loop (cdr types)
                             (cons (load-soft-single-stack int-reg-offset) locs)
                             (fx+ iint 1) iflt (fx+ int-reg-offset 4) float-reg-offset stack-arg-offset)
                           (loop (cdr types)
                             (cons (load-soft-single-stack stack-arg-offset) locs)
                             iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 4)))]
		      [(nanopass-case (Ltype Type) (car types)
			 [(fp-ftd& ,ftd) (not ($ftd-compound? ftd))]
			 [else #f])
		       ;; load pointer to address on the stack
		       (let ([ftd (nanopass-case (Ltype Type) (car types)
				    [(fp-ftd& ,ftd) ftd])])
			 (case (and (not (constant software-floating-point))
				    ($ftd-atomic-category ftd))
			   [(float)
			    (let ([load-address (case ($ftd-size ftd)
						  [(4) load-stack-address/convert-float]
						  [else load-stack-address])])
			      (if (fx< iflt fp-reg-count)
				  (loop (cdr types)
				    (cons (load-address float-reg-offset) locs)
				    iint (fx+ iflt 1) int-reg-offset (fx+ float-reg-offset 8) stack-arg-offset)
				  (let ([stack-arg-offset (align 8 stack-arg-offset)])
				    (loop (cdr types)
				      (cons (load-address stack-arg-offset) locs)
				      iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 8)))))]
			   [else
			    (case ($ftd-size ftd)
			      [(8)
			       (let ([iint (align 2 iint)])
				 (if (fx< iint gp-reg-count)
				     (let ([int-reg-offset (align 8 int-reg-offset)])
				       (loop (cdr types)
					 (cons (load-stack-address int-reg-offset) locs)
					 (fx+ iint 2) iflt (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset))
				     (let ([stack-arg-offset (align 8 stack-arg-offset)])
				       (loop (cdr types)
					 (cons (load-stack-address stack-arg-offset) locs)
					 iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 8)))))]
			      [else
			       (let ([byte-offset (- 4 ($ftd-size ftd))])
				 (if (fx< iint gp-reg-count)
				     (loop (cdr types)
				       (cons (load-stack-address (+ int-reg-offset byte-offset)) locs)
				       (fx+ iint 1) iflt (fx+ int-reg-offset 4) float-reg-offset stack-arg-offset)
				     (loop (cdr types)
                                       (cons (load-stack-address (+ stack-arg-offset byte-offset)) locs)
				       iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 4))))])]))]
		      [(nanopass-case (Ltype Type) (car types)
                         [(fp-integer ,bits) (fx= bits 64)]
                         [(fp-unsigned ,bits) (fx= bits 64)]
                         [else #f])
                       (let ([iint (align 2 iint)])
                         (if (fx< iint gp-reg-count)
                             (let ([int-reg-offset (align 8 int-reg-offset)])
                               (loop (cdr types)
                                 (cons (load-int64-stack int-reg-offset) locs)
                                 (fx+ iint 2) iflt (fx+ int-reg-offset 8) float-reg-offset stack-arg-offset))
                             (let ([stack-arg-offset (align 8 stack-arg-offset)])
                               (loop (cdr types)
                                 (cons (load-int64-stack stack-arg-offset) locs)
                                 iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 8)))))]
                      [else
                       (if (fx< iint gp-reg-count)
                           (loop (cdr types)
                             (cons (load-int-stack (car types) int-reg-offset) locs)
                             (fx+ iint 1) iflt (fx+ int-reg-offset 4) float-reg-offset stack-arg-offset)
                           (loop (cdr types)
                             (cons (load-int-stack (car types) stack-arg-offset) locs)
                             iint iflt int-reg-offset float-reg-offset (fx+ stack-arg-offset 4)))])))))
          (define save-regs
            (lambda (regs offset)
              (if (null? regs)
                  `(nop)
                  (let f ([regs regs] [offset offset])
                    (let ([inline `(inline ,(make-info-load 'integer-32 #f) ,%store ,%Csp ,%zero (immediate ,offset) ,(car regs))])
                      (let ([regs (cdr regs)])
                        (if (null? regs)
                            inline
                            (%seq ,inline ,(f regs (fx+ offset 4))))))))))
          (define save-fp-regs
            (lambda (regs offset)
              (if (null? regs)
                  `(nop)
                  (let f ([regs regs] [offset offset])
                    (let ([inline `(inline ,(make-info-loadfl (car regs)) ,%store-double ,%Csp ,%zero (immediate ,offset))])
                      (let ([regs (cdr regs)])
                        (if (null? regs)
                            inline
                            (%seq ,inline ,(f regs (fx+ offset 8))))))))))
          (define restore-regs
            (lambda (regs offset)
              (if (null? regs)
                  `(nop)
                  (let f ([regs regs] [offset offset])
                    (let ([inline `(set! ,(car regs) (inline ,(make-info-load 'integer-32 #f) ,%load  ,%Csp ,%zero (immediate ,offset)))])
                      (let ([regs (cdr regs)])
                        (if (null? regs)
                            inline
                            (%seq ,inline ,(f regs (fx+ offset 4))))))))))
          (define do-result
            (lambda (result-type return-space-offset int-reg-offset)
              (nanopass-case (Ltype Type) result-type
		[(fp-ftd& ,ftd)
		 (case ($ftd-atomic-category ftd)
		   [(float)
		    (values
		     (lambda ()
		       (case ($ftd-size ftd)
			 [(4) `(inline ,(make-info-loadfl %Cfpretval) ,%load-single ,%sp ,%zero (immediate ,return-space-offset))]
			 [else `(inline ,(make-info-loadfl %Cfpretval) ,%load-double ,%sp ,%zero (immediate ,return-space-offset))]))
		     '()
		     1)]
		   [else
		    (cond
		     [($ftd-compound? ftd)
		      ;; return pointer
		      (values
		       (lambda () `(set! ,%Cretval ,(%mref ,%sp ,int-reg-offset)))
		       (list %Cretval)
		       0)]
		     [(fx= 8 ($ftd-size ftd))
		      (values (lambda ()
				(%seq
				 (set! ,%Cretval-high ,(%mref ,%sp ,return-space-offset))
				 (set! ,%Cretval-low ,(%mref ,%sp ,(fx+ return-space-offset 4)))))
			      (list %Cretval-high %Cretval-low)
			      0)]
		     [else
		      (values
		       (lambda ()
			 (case ($ftd-size ftd)
			   [(1) `(set! ,%Cretval (inline ,(make-info-load 'integer-8 #f) ,%load ,%sp ,%zero (immediate ,return-space-offset)))]
			   [(2) `(set! ,%Cretval (inline ,(make-info-load 'integer-16 #f) ,%load ,%sp ,%zero (immediate ,return-space-offset)))]
			   [else `(set! ,%Cretval ,(%mref ,%sp ,return-space-offset))]))
		       (list %Cretval)
		       0)])])]
                [(fp-double-float)
                 (values (lambda (x)
			   `(inline ,(make-info-loadfl %Cfpretval) ,%load-double ,x ,%zero ,(%constant flonum-data-disp)))
			 '()
			 1)]
                [(fp-single-float)
                 (values (lambda (x)
			   `(inline ,(make-info-loadfl %Cfpretval) ,%load-double->single ,x ,%zero ,(%constant flonum-data-disp)))
			 '()
			 1)]
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
		   (values (lambda (lo-rhs hi-rhs)
			     (%seq
			      (set! ,%Cretval-low ,lo-rhs)
			      (set! ,%Cretval-high ,hi-rhs)))
			   (list %Cretval-high %Cretval-low)
			   0)]
		  [else
		   (values (lambda (rhs)
			     `(set! ,%Cretval ,rhs))
			   (list %Cretval)
			   0)])])))
          (define (unactivate unactivate-mode-offset result-regs result-num-fp-regs stash-offset)
            (let ([e (%seq
                       (set! ,%Carg1 ,(%mref ,%sp ,unactivate-mode-offset))
                       ,(%inline unactivate-thread ,%Carg1))])
	      (pop-registers result-regs result-num-fp-regs (fp-result-regs) stash-offset
			     (push-registers result-regs result-num-fp-regs (fp-result-regs) stash-offset
					     e))))
          (lambda (info)
            (define callee-save-regs (list %r14 %r15 %r16 %r17 %r18 %r19 %r20 %r21 %r22 %r23 %r24 %r25 %r26 %r27 %r28 %r29 %r30 %r31))
            (define isaved (length callee-save-regs))
            (let ([arg-type* (info-foreign-arg-type* info)]
		  [result-type (info-foreign-result-type info)]
                  [gp-reg-count (length (gp-parameter-regs))]
                  [fp-reg-count (length (fp-parameter-regs))])
              (let-values ([(iint iflt) (count-reg-args arg-type* gp-reg-count fp-reg-count (indirect-result-that-fits-in-registers? result-type))])
                (let* ([int-reg-offset 8]       ; initial offset for calling conventions
                       [float-reg-offset (align 8 (fx+ (fx* gp-reg-count 4) int-reg-offset))]
                       [callee-save-offset (if (constant software-floating-point)
                                               float-reg-offset
                                               (fx+ (fx* fp-reg-count 8) float-reg-offset))]
		       [synthesize-first-argument? (indirect-result-that-fits-in-registers? result-type)]
		       [adjust-active? (memq 'adjust-active (info-foreign-conv info))]
                       [unactivate-mode-offset (fx+ (fx* isaved 4) callee-save-offset)]
                       [return-space-offset (align 8 (fx+ unactivate-mode-offset (if adjust-active? 4 0)))]
                       [stack-size (align 16 (fx+ return-space-offset (if synthesize-first-argument? 8 0)))]
                       [stack-arg-offset (fx+ stack-size 8)])
		  (let-values ([(get-result result-regs result-num-fp-regs) (do-result result-type return-space-offset int-reg-offset)])
		    (values
		     (lambda ()
		       (%seq
                         ,(%inline save-lr (immediate 4))
			 ,(%inline store-with-update ,%Csp ,%Csp (immediate ,(fx- stack-size)))
			 ,(save-regs (list-head (gp-parameter-regs) iint) int-reg-offset)
			 ,(save-fp-regs (list-head (fp-parameter-regs) iflt) float-reg-offset)
			 ; not bothering with callee-save floating point regs right now
                         ; not bothering with cr, because we don't update nonvolatile fields
			 ,(save-regs callee-save-regs callee-save-offset)
			 ,(if-feature pthreads
			    ((lambda (e)
                               (if adjust-active?
                                   (%seq
                                     (set! ,%Cretval ,(%inline activate-thread))
                                     (set! ,(%mref ,%sp ,unactivate-mode-offset) ,%Cretval)
                                     ,e)
                                   e))
			     (%seq 
                               (set! ,%Cretval ,(%inline get-tc))
                               (set! ,%tc ,%Cretval)))
                            `(set! ,%tc (literal ,(make-info-literal #f 'entry (lookup-c-entry thread-context) 0))))))
		     ; list of procedures that marshal arguments from their C stack locations
                     ; to the Scheme argument locations
                     (do-stack arg-type* gp-reg-count fp-reg-count int-reg-offset float-reg-offset stack-arg-offset
			       synthesize-first-argument? return-space-offset)
		     get-result
		     (lambda ()
		       (in-context Tail
                        ((lambda (e)
                           (if adjust-active?
                               (%seq
                                ,(unactivate unactivate-mode-offset result-regs result-num-fp-regs int-reg-offset)
                                ,e)
                               e))
                         (%seq
                           ; restore the lr
                           (inline ,null-info ,%restore-lr (immediate ,(fx+ stack-size 4)))
                           ; restore the callee save registers
                           ,(restore-regs callee-save-regs callee-save-offset)
                           ; deallocate space for pad & arg reg values
                           (set! ,%Csp ,(%inline + ,%Csp (immediate ,stack-size)))
                           ; done
                           (asm-c-return ,null-info ,callee-save-regs ... ,result-regs ...)))))))))))))))
)
