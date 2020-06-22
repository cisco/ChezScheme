;;; x86.ss
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
(define-registers
  (reserved
    [%tc  %edi #t 7]
    [%sfp %ebp #t 5]
    #;[%ap]
    #;[%esp]
    #;[%eap]
    #;[%trap])
  (allocable ; keep in sync with all-but-byte-registers below
    [%ac0 %edx #f 2]
    [%xp  %ecx #f 1]
    [%ts  %eax #f 0]
    [%td  %ebx #t 3]
    #;[%ret]
    #;[%cp]
    #;[%ac1]
    #;[%yp]
    [%esi      #t 6])
  (machine-dependent
    [%flreg1   #f 0]
    [%flreg2   #f 1]
    [%sp       #t 4]
    #;[%esi      #f 6]))

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

  (define all-but-byte-registers
    ; include only allocable registers that aren't byte registers
    ; keep in sync with define-registers above
    (lambda ()
      (list %esi)))

  ; don't bother with literal@? check since lvalues can't be literals
  (define lmem? mref?)

  (define mem?
    (lambda (x)
      (or (lmem? x) (literal@? x))))

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

  (define lvalue->ur
    (lambda (x k)
      (if (mref? x)
          (let ([u (make-tmp 'u)])
            (seq
              (set-ur=mref u x)
              (k u)))
          (k x))))

  (define literal@->mem
    (lambda (a k)
      (nanopass-case (L15c Triv) a
        ; NOTE: x86_64 and risc arch's will need to deal with this differently
        [(literal ,info) (k (with-output-language (L15d Triv) `(literal ,info)))])))

  (define mref->mref
    (lambda (a k)
      (nanopass-case (L15c Triv) a
        ; NOTE: x86_64 and risc arch's will need to deal with limitations on the offset
        [(mref ,lvalue0 ,lvalue1 ,imm)
         (lvalue->ur lvalue0
           (lambda (x0)
             (lvalue->ur lvalue1
               (lambda (x1)
                 (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm)))))))])))

  (define mem->mem
    (lambda (a k)
      (cond
        [(literal@? a) (literal@->mem a k)]
        [else (mref->mref a k)])))

  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (memq 'ur aty*)
             (or (and (memq 'imm32 aty*) (imm32? a))
                 (and (memq 'imm aty*) (imm? a))
                 (and (memq 'zero aty*) (imm0? a))
                 (and (memq 'real-imm32 aty*) (real-imm32? a))
                 (and (memq 'negatable-real-imm32 aty*) (negatable-real-imm32? a))
                 (and (memq 'mem aty*) (mem? a)))))]))

  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
           [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
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
           [else (sorry! 'coerce-opnd "cannot coerce ~s to ~s" a aty*)]))]))

  (define set-ur=mref
    (lambda (ur mref)
      (mref->mref mref
        (lambda (mref)
          (build-set! ,ur ,mref)))))

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

  (define-syntax define-instruction
    (lambda (x)
      (define acsame-mem
        (lambda (c a b bty* k)
          #`(lambda (c a b)
              (if (and (lmem? c) (same? a c) (coercible? b '#,bty*))
                  (coerce-opnd b '#,bty*
                    (lambda (b)
                      (mem->mem c
                        (lambda (c)
                          (#,k c b)))))
                  (next c a b)))))

      (define-who acsame-ur
        (lambda (c a b bty* k)
          #`(lambda (c a b)
              (if (and (same? a c) (coercible? b '#,bty*))
                  (coerce-opnd b '#,bty*
                    (lambda (b)
                      (cond
                        [(ur? c) (#,k c b)]
                        [(mref? c)
                         (nanopass-case (L15c Triv) c
                           ; NOTE: x86_64 and risc arch's will need to deal with limitations on the offset
                           [(mref ,lvalue0 ,lvalue1 ,imm)
                            (lvalue->ur lvalue0
                              (lambda (x0)
                                (lvalue->ur lvalue1
                                  (lambda (x1)
                                    (let ([u (make-tmp 'u)])
                                      (seq
                                        (build-set! ,u (mref ,x0 ,x1 ,imm))
                                        (#,k u b)
                                        (build-set! (mref ,x0 ,x1 ,imm) ,u)))))))])]
                        [else (sorry! '#,(datum->syntax #'* who) "unexpected operand ~s" c)])))
                  (next c a b)))))

      (define make-value-clause
        (lambda (fmt)
          (syntax-case fmt (mem ur xp)
            [(op (c mem) (a ?c) (b bty* ...))
             (bound-identifier=? #'?c #'c)
             (acsame-mem #'c #'a #'b #'(bty* ...) #'(lambda (c b) (rhs c c b)))]
            [(op (c ur) (a ?c) (b bty* ...))
             (bound-identifier=? #'?c #'c)
             (acsame-ur #'c #'a #'b #'(bty* ...) #'(lambda (c b) (rhs c c b)))]
            [(op (c mem) (a aty* ...) (b ?c))
             (bound-identifier=? #'?c #'c)
             (acsame-mem #'c #'b #'a #'(aty* ...) #'(lambda (c a) (rhs c a c)))]
            [(op (c ur) (a aty* ...) (b ?c))
             (bound-identifier=? #'?c #'c)
             (acsame-ur #'c #'b #'a #'(aty* ...) #'(lambda (c a) (rhs c a c)))]
            [(op (c mem) (a aty ...) (b bty ...))
             #`(lambda (c a b)
                 (if (and (lmem? c) (coercible? a '(aty ...)) (coercible? b '(bty ...)))
                     (coerce-opnd b '(bty ...)
                       (lambda (b)
                         (coerce-opnd a '(aty ...)
                           (lambda (a)
                             (mref->mref c (lambda (c) (rhs c a b)))))))
                     (next c a b)))]
            [(op (c ur) (a aty ...) (b bty ...))
             #`(lambda (c a b)
                 (if (and (coercible? a '(aty ...)) (coercible? b '(bty ...)))
                     (coerce-opnd b '(bty ...)
                       (lambda (b)
                         (coerce-opnd a '(aty ...)
                           (lambda (a)
                             (if (ur? c)
                                 (rhs c a b)
                                 (let ([u (make-tmp 'u)])
                                   (seq
                                     (rhs u a b)
                                     (mref->mref c
                                       (lambda (c)
                                         (build-set! ,c ,u))))))))))
                     (next c a b)))]
            ; four-operand case below can require four unspillables
            [(op (c ur) (a ur) (b ur) (d dty ...))
             (not (memq 'mem (datum (dty ...))))
             #`(lambda (c a b d)
                 (if (coercible? d '(dty ...))
                     (coerce-opnd d '(dty ...)
                       (lambda (d)
                         (coerce-opnd a '(ur)
                           (lambda (a)
                             (coerce-opnd b '(ur)
                               (lambda (b)
                                 (if (ur? c)
                                     (rhs c a b d)
                                     (let ([u (make-tmp 'u)])
                                       (seq
                                         (rhs u a b d)
                                         (mref->mref c
                                           (lambda (c)
                                             (build-set! ,c ,u))))))))))))
                     (next c a b d)))]
            [(op (c mem) (a ?c))
             (bound-identifier=? #'?c #'c)
             #`(lambda (c a)
                 (if (and (lmem? c) (same? c a))
                     (mem->mem c
                       (lambda (c)
                         (rhs c c)))
                     (next c a)))]
            [(op (c ur) (a ?c))
             (bound-identifier=? #'?c #'c)
             #`(lambda (c a)
                 (if (same? a c)
                     (if (ur? c)
                         (rhs c c)
                         (mem->mem c
                           (lambda (c)
                             (let ([u (make-tmp 'u)])
                               (seq
                                 (build-set! ,u ,c)
                                 (rhs u u)
                                 (build-set! ,c ,u))))))
                     (next c a)))]
            [(op (c mem) (a aty ...))
             #`(lambda (c a)
                 (if (and (lmem? c) (coercible? a '(aty ...)))
                     (coerce-opnd a '(aty ...)
                       (lambda (a)
                         (mem->mem c
                           (lambda (c)
                             (rhs c a)))))
                     (next c a)))]
            [(op (c ur) (a aty ...))
             #`(lambda (c a)
                 (if (coercible? a '(aty ...))
                     (coerce-opnd a '(aty ...)
                       (lambda (a)
                         (if (ur? c)
                             (rhs c a)
                             (mem->mem c
                               (lambda (c)
                                 (let ([u (make-tmp 'u)])
                                   (seq
                                     (rhs u a)
                                     (build-set! ,c ,u))))))))
                     (next c a)))]
            [(op (c ur))
             #`(lambda (c)
                 (if (ur? c)
                     (rhs c)
                     (mem->mem c
                       (lambda (c)
                         (let ([u (make-tmp 'u)])
                           (seq
                             (rhs u)
                             (build-set! ,c ,u)))))))]
            [(op (c mem))
             #`(lambda (c)
                 (if (lmem? c)
                     (mem->mem c
                       (lambda (c)
                         (rhs c)))
                     (next c)))])))

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

  (define-instruction value (-/ovfl -/eq) ; must set condition codes, so can't use lea or sub-negate
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

  (define-instruction value (* */ovfl) ; */ovfl must set mulitply-overflow flag on overflow
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
          (let ([ueax (make-precolored-unspillable 'ueax %eax)]
                [uedx (make-precolored-unspillable 'uedx %edx)])
            (with-output-language (L15d Effect)
              (seq
                `(set! ,(make-live-info) ,ueax ,x)
                `(set! ,(make-live-info) ,uedx (asm ,null-info ,asm-sext-eax->edx ,ueax))
                `(set! ,(make-live-info) ,ueax (asm ,null-info ,asm-div ,ueax ,uedx ,y))
                `(set! ,(make-live-info) ,z ,ueax)))))))
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
          (let ([uecx (make-precolored-unspillable 'uecx %ecx)])
            (with-output-language (L15d Effect)
              (seq
                `(set! ,(make-live-info) ,uecx ,y)
                `(set! ,(make-live-info) ,z (asm ,info ,(asm-shiftop op) ,x ,uecx))))))))
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

  (define-instruction value move
    [(op (z mem) (x ur imm32))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem imm))
     ; NOTE: risc arch's will need to deal with limitations on imm
     `(set! ,(make-live-info) ,z ,x)])

  (define-instruction value lea1
    [(op (z ur) (x ur))
     ; TODO: risc arch, x86_64 must handle cases where offset is too lage
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea1 (info-lea-offset info)) ,x))])

  (define-instruction value lea2
    [(op (z ur) (x ur) (y ur))
     ; TODO: risc arch, x86_64 must handle cases where offset is too lage
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-lea2 (info-lea-offset info)) ,x ,y))])

  (define-instruction value (sext8 sext16 zext8 zext16)
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
        (lambda (swapped? w k)
          (with-output-language (L15d Effect)
            (if swapped?
                (let ([u (make-tmp 'u)])
                  (seq
                    `(set! ,(make-live-info) ,u ,w)
                    `(set! ,(make-live-info) ,u (asm ,info ,(asm-swap (info-load-type info)) ,u))
                    (k u)))
                (k w)))))
      (define select-value-register
        (lambda (type w k)
          (if (and (ur? w) (memq type '(integer-8 unsigned-8)))
              (let ([u (make-restricted-unspillable 'ubyte (all-but-byte-registers))])
                (with-output-language (L15d Effect)
                  (seq
                    `(set! ,(make-live-info) ,u ,w)
                    (k u))))
              (k w)))))
    [(op (x ur) (y ur) (z imm32) (w ur real-imm32))
     (let ([type (info-load-type info)])
       (select-value-register type w
         (lambda (w)
           (maybe-swap (info-load-swapped? info) w
             (lambda (w)
               `(asm ,info ,(asm-store type) ,x ,y ,z ,w))))))]
    [(op (x ur) (y ur) (z ur) (w ur real-imm32))
     (let ([type (info-load-type info)])
       (select-value-register type w
         (lambda (w)
           (maybe-swap (info-load-swapped? info) w
             (lambda (w)
               (if (eq? y %zero)
                   `(asm ,info ,(asm-store type) ,x ,z (immediate 0) ,w)
                   (let ([u (make-tmp 'u)])
                     (seq
                       `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-lea2 0) ,y ,z))
                       `(asm ,info ,(asm-store type) ,x ,u (immediate 0) ,w)))))))))])

  (define-instruction value (fstpl)
    [(op (z mem)) `(set! ,(make-live-info) ,z (asm ,info ,asm-fstpl))])

  (define-instruction value (fstps)
    [(op (z mem)) `(set! ,(make-live-info) ,z (asm ,info ,asm-fstps))])

  (define-instruction effect (fldl)
    [(op (z mem)) `(asm ,info ,asm-fldl ,z)])

  (define-instruction effect (flds)
    [(op (z mem)) `(asm ,info ,asm-flds ,z)])

  (define-instruction effect (load-single->double load-double->single)
    [(op (x ur) (y ur) (z imm32))
     `(asm ,info ,(asm-fl-cvt op (info-loadfl-flreg info)) ,x ,y ,z)])

  (define-instruction effect (store-single store-double)
    [(op (x ur) (y ur) (z imm32))
     `(asm ,info ,(asm-fl-store op (info-loadfl-flreg info)) ,x ,y ,z)])

  (define-instruction effect (load-double load-single)
    [(op (x ur) (y ur) (z imm32))
     `(asm ,info ,(asm-fl-load op (info-loadfl-flreg info)) ,x ,y ,z)])

  (define-instruction effect (flt)
    [(op (x mem ur) (y ur)) `(asm ,info ,asm-flt ,x ,y)])

  (define-instruction effect (fl+ fl- fl/ fl*)
    [(op (x ur) (y ur) (z ur)) `(asm ,info ,(asm-flop-2 op) ,x ,y ,z)])

  (define-instruction effect (flsqrt)
    [(op (x ur) (y ur)) `(asm ,info ,asm-flsqrt ,x ,y)])

  (define-instruction effect inc-cc-counter
    [(op (x ur) (y imm32 ur) (z imm32 ur)) `(asm ,info ,asm-inc-cc-counter ,x ,y ,z)])

  (define-instruction effect inc-profile-counter 
    [(op (x ur mem) (y imm32 ur)) `(asm ,info ,asm-inc-profile-counter ,x ,y)])

  (define-instruction value (trunc)
    [(op (z ur) (x ur)) `(set! ,(make-live-info) ,z (asm ,info ,asm-trunc ,x))])

  ;; no kills since we expect to be called when all necessary state has already been saved
  (define-instruction value get-tc
    [(op (z ur))
     (safe-assert (eq? z %eax))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-get-tc))])

  (define-instruction value activate-thread
    [(op (z ur))
     (safe-assert (eq? z %eax)) ; see get-tc
     `(set! ,(make-live-info) ,z (asm ,info ,asm-activate-thread))])

  (define-instruction effect deactivate-thread
    [(op)
     `(asm ,info ,asm-deactivate-thread)])

  (define-instruction effect unactivate-thread
    [(op)
     `(asm ,info ,asm-unactivate-thread)])

  ; TODO: should we insist that asm-library-call preserve %ts and %td?
  ; TODO: risc architectures will have to take info-asmlib-save-ra? into account
  (define-instruction value asmlibcall
    [(op (z ur)) 
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-library-call (info-asmlib-libspec info)) ,(info-kill*-live*-live* info) ...))])

  (define-instruction effect asmlibcall!
    [(op) `(asm ,info ,(asm-library-call (info-asmlib-libspec info)) ,(info-kill*-live*-live* info) ...)])

  (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
  (define-instruction effect (c-simple-call)
    [(op) `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info)))])

  (define-instruction value pop
    [(op (z ur)) `(set! ,(make-live-info) ,z (asm ,info ,asm-pop))])

  (define-instruction pred (fl= fl< fl<=)
    [(op (x ur) (y ur))
     (let ([info (make-info-condition-code op #t #f)]) ; NB: reversed? flag is assumed to be #t
       (values '() `(asm ,info ,(asm-fl-relop info) ,x ,y)))])

  (define-instruction pred (eq? u< < > <= >=)
    ; the idea (following from the intel x86/x86_64 documentation)
    ; is that we want to squeeze this into a CMP that allows one of
    ; the following formats:
    ; CMP r/m, imm
    ; CMP r/m, r
    ; CMP r, r/m
    ; the last format we may want to drop, since it uses a different
    ; format from the one above it, but is interchangable with it,
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
      [(op (x ur) (y ur) (w imm32))
       (let ([uts (make-precolored-unspillable 'uts %ts)])
         (values
           (nanopass-case (L15d Triv) w
             [(immediate ,imm)
              (with-output-language (L15d Effect)
                (seq
                  `(set! ,(make-live-info) ,uts (immediate 1))
                  `(set! ,(make-live-info) ,uts
                     (asm ,info ,asm-exchange ,uts
                       (mref ,x ,y ,imm)))))])
           `(asm ,info-cc-eq ,asm-eq ,uts (immediate 0))))]))

  (define-instruction effect (locked-incr!)
    [(op (x ur) (y ur) (w imm32))
     `(asm ,info ,asm-locked-incr ,x ,y ,w)])

  (define-instruction effect (locked-decr!)
    [(op (x ur) (y ur) (w imm32))
     `(asm ,info ,asm-locked-decr ,x ,y ,w)])

  (define-instruction effect (cas)
    [(op (x ur) (y ur) (w imm32) (old ur) (new ur))
     (let ([ueax (make-precolored-unspillable 'ueax %eax)])
       (with-output-language (L15d Effect)
         (seq
           `(set! ,(make-live-info) ,ueax ,old)
           ;; NB: may modify %eax:
           `(asm ,info ,asm-locked-cmpxchg ,x ,y ,w ,ueax ,new))))])

  (define-instruction effect (pause)
    [(op) `(asm ,info ,asm-pause)])

  (define-instruction value read-performance-monitoring-counter
    [(op (z ur) (x ur mem imm))
     (safe-assert (eq? z %eax))
     (safe-assert (and (info-kill*? info) (memq %edx (info-kill*-kill* info))))
     (let ([uecx (make-precolored-unspillable 'uecx %ecx)])
       (seq
         `(set! ,(make-live-info) ,uecx ,x)
         `(set! ,(make-live-info) ,z (asm ,info ,asm-read-performance-monitoring-counter ,uecx))))])

  (define-instruction value read-time-stamp-counter
    [(op (z ur))
     (safe-assert (eq? z %eax))
     (safe-assert (and (info-kill*? info) (memq %edx (info-kill*-kill* info))))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-read-time-stamp-counter))])

  (define-instruction effect (c-call)
    [(op (x ur mem)) `(asm ,info ,asm-indirect-call ,x)])

  (define-instruction effect (push)
    [(op (x ur)) `(asm ,info ,asm-push ,x)])

  (define-instruction effect save-flrv
    [(op) `(asm ,info ,asm-save-flrv)])

  (define-instruction effect restore-flrv
    [(op) `(asm ,info ,asm-restore-flrv)])

  (define-instruction effect invoke-prelude
    [(op)
     (constant-case machine-type-name
       [(i3nt ti3nt) `(set! ,(make-live-info) ,%tc (mref ,%sp ,%zero 4))]
       [else
        (seq
          `(set! ,(make-live-info) ,%tc (mref ,%sp ,%zero 4))
          `(set! ,(make-live-info) ,%sp (asm ,info ,asm-sub ,%sp (immediate 12))))])])
  )

;;; SECTION 3: assembler
(module asm-module (; required exports
                     asm-move asm-move/extend asm-load asm-store asm-swap asm-library-call asm-library-jump
                     asm-mul asm-muli asm-addop asm-add asm-sub asm-negate asm-sub-negate
                     asm-pop asm-shiftop asm-sll asm-logand asm-lognot
                     asm-logtest asm-fl-relop asm-relop asm-push asm-indirect-jump asm-literal-jump
                     asm-direct-jump asm-return-address asm-jump asm-conditional-jump asm-data-label asm-rp-header
                     asm-lea1 asm-lea2 asm-indirect-call asm-fstpl asm-fstps asm-fldl asm-flds asm-condition-code
                     asm-fl-cvt asm-fl-store asm-fl-load asm-flt asm-trunc asm-div
                     asm-exchange asm-pause asm-locked-incr asm-locked-decr asm-locked-cmpxchg
                     asm-flop-2 asm-flsqrt asm-c-simple-call
                     asm-save-flrv asm-restore-flrv asm-return asm-c-return asm-size
                     asm-enter asm-foreign-call asm-foreign-callable
                     asm-inc-profile-counter
                     asm-inc-cc-counter asm-read-time-stamp-counter asm-read-performance-monitoring-counter
                     ; threaded version specific
                     asm-get-tc asm-activate-thread asm-deactivate-thread asm-unactivate-thread
                     ; machine dependent exports
                     asm-sext-eax->edx)

  (define byte-register?
    (lambda (x)
      (or (eq? x %eax) (eq? x %ebx) (eq? x %ecx) (eq? x %edx))))

  (define ax-register?
    (case-lambda
      [(x) (record-case x [(reg) r #t] [else #f])]
      [(x reg) (record-case x [(reg) r (eq? r reg)] [else #f])]))

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
  ; suffixes are a sub-list of (b w l)--
  ; the opcode, the size (byte word or long), and all other expressions
  ; are passed to the specified handler--
  ; for prefix 'p' and each suffix 's' a macro of the form 'ps' is set up--
  ; if no suffix is specified the prefix is defined as a macro
  (define-syntax define-op
    (lambda (x)
      (syntax-case x ()
        [(k prefix (suffix ...) handler e ...)
         (let ([suffix* (datum (suffix ...))])
           (unless (andmap (lambda (x) (memq x '(b w *))) suffix*)
             (syntax-error x (format "invalid suffix list ~s" suffix*)))
           (with-syntax ([(op ...) (map (lambda (x)
                                          (if (eq? x '*)
                                              (construct-name #'k "386op-" #'prefix)
                                              (construct-name #'k "386op-" #'prefix x)))
                                     suffix*)]
                         [(size ...) (map (lambda (x)
                                            (case x [(b) #'byte] [(w) #'word] [(*) #'long]))
                                       suffix*)])
             #'(begin
                 (define-syntax op
                   (syntax-rules ()
                     [(_ mneu arg (... ...))
                      (handler 'mneu 'size e ... arg (... ...))]))
                 ...)))]
        [(k op handler e ...)
         (with-syntax ([op (construct-name #'k "386op-" #'op)])
           #'(define-syntax op
               (syntax-rules ()
                 [(_ mneu arg (... ...))
                  (handler 'mneu e ... arg (... ...))])))])))

  (define-syntax emit
    (lambda (x)
      (syntax-case x ()
        [(k op x ...)
         (with-syntax ([emit-op (construct-name #'k "386op-" #'op)])
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

  (define-op addi (b *) addi-op   #b100000 #b000)
  (define-op subi (b *) addi-op   #b100000 #b101)
  (define-op cmpi (b *) addi-op   #b100000 #b111)

  (define-op adci (b *) addi-op   #b100000 #b010)

  (define-op ori  (b *) logi-op #b001)
  (define-op andi (b *) logi-op #b100)
  (define-op xori (b *) logi-op #b110)
  (define-op testi (b *) testi-op #b1111011 #b000)

  (define-op movi (b w *) movi-op #b1100011 #b000)

  (define-op mov    (b w *) binary-op #b100010)
  (define-op movsb     mul-op #b00001111 #b10111110)
  (define-op movsw     mul-op #b00001111 #b10111111)
  (define-op movzb     mul-op #b00001111 #b10110110)
  (define-op movzw     mul-op #b00001111 #b10110111)

  (define-op add  (b *) binary-op #b000000)
  (define-op or   (b *) binary-op #b000010)
  (define-op and  (b *) binary-op #b001000)
  (define-op sub  (b *) binary-op #b001010)
  (define-op xor  (b *) binary-op #b001100)
  (define-op test (b *) test-op   #b1000010)
  (define-op cmp  (b *) binary-op #b001110)
  (define-op xchg (b *) xchg-op   #b1000011)
  (define-op bswap      byte-reg-op2 #b00001111 #b11001)

  (define-op divsax (*) unary-op   #b1111011  #b111)
  (define-op mulsax (*) unary-op   #b1111011  #b100)
  (define-op muls      mul-op     #b00001111 #b10101111)
  (define-op mulsi     muli-op    #b01101001)

  (define-op lea       lea-op     #b10001101)

  (define-op pop       byte-reg-op1 #b01011)
  (define-op push      byte-reg-op1 #b01010)
  (define-op pushi     pushil-op)
  (define-op pushall   byte-op     #b01100000)
  (define-op popall    byte-op     #b01100001)
  (define-op pushf     byte-op     #b10011100)
  (define-op popf      byte-op     #b10011101)
  (define-op nop       byte-op     #b10010000)
  (define-op ret       byte-op     #b11000011)
  (define-op retl      byte+short-op #b11000010)
  (define-op sahf      byte-op     #b10011110)
  (define-op extad     byte-op     #b10011001)  ; extend eax to edx

  (define-op rdtsc     two-byte-op     #b1111 #b00110001) ; read time-stamp counter
  (define-op rdpmc     two-byte-op     #b1111 #b00110011) ; read performance monitoring counter
  (define-op pause     two-byte-op #b11110011 #b10010000) ; equivalent to rep nop

  (define-op dec (b *) unary-op  #b1111111 #b001)
  (define-op inc (b *) unary-op  #b1111111 #b000)
  (define-op neg (b *) unary-op  #b1111011 #b011)
  (define-op not (b *) unary-op  #b1111011 #b010)

  (define-op locked-dec (b *) locked-unary-op #b1111111 #b001)
  (define-op locked-inc (b *) locked-unary-op #b1111111 #b000)

  (define-op locked-cmpxchg (*) locked-cmpxchg-op)

  ; also do inc-reg dec-reg

  (define-op call      jump-op #b010)
  (define-op jmp       jump-op #b100)    ; ow - was #b011 (looks like lcal*)
  (define-op bra       bra-op)
  (define-op bsr       bsr-op)

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

  ; coprocessor ops required to handle calling conventions
  (define-op fldl  float-op2 #b101 #b000) ; double memory push => ST[0]
  (define-op flds  float-op2 #b001 #b000) ; single memory push => ST[0]
  (define-op fstpl float-op2 #b101 #b011) ; ST[0] => double memory, pop
  (define-op fstps float-op2 #b001 #b011) ; ST[0] => single memory, pop

  ; SSE2 instructions (pulled from x86_64macros.ss)
  (define-op sse.addsd     sse-op1 #xF2 #x58)
  (define-op sse.andpd     sse-op1 #x66 #x54)
  (define-op sse.cvtss2sd  sse-op1 #xF3 #x5A)
  (define-op sse.cvtsd2ss  sse-op1 #xF2 #x5A)
  (define-op sse.cvttsd2si sse-op1 #xF2 #x2C)
  (define-op sse.cvtsi2sd  sse-op1 #xF2 #x2A)
  (define-op sse.divsd     sse-op1 #xF2 #x5E)
  (define-op sse.movd      sse-op2 #x66 #x6E #x7E)
  (define-op sse.movsd     sse-op2 #xF2 #x10 #x11)
  (define-op sse.movss     sse-op2 #xF3 #x10 #x11)
  (define-op sse.mulsd     sse-op1 #xF2 #x59)
  (define-op sse.sqrtsd    sse-op1 #xF2 #x51)
  (define-op sse.subsd     sse-op1 #xF2 #x5C)
  (define-op sse.ucomisd   sse-op1 #x66 #x2E)
  (define-op sse.xorpd     sse-op1 #x66 #x57)

  (define sse-op1
    (lambda (op prefix-code op-code source dest-reg code*)
      (emit-code (op source dest-reg code*)
        (build byte prefix-code)
        (build byte #x0F)
        (build byte op-code)
        (ax-ea-modrm-reg source dest-reg)
        (ax-ea-sib source)
        (ax-ea-addr-disp source))))

  (define sse-op2
    (lambda (op prefix-code dstreg-op-code srcreg-op-code source dest code*)
      (cond
        [(ax-register? source)
         (emit-code (op source dest code*)
           (build byte prefix-code)
           (build byte #x0F)
           (build byte srcreg-op-code)
           (ax-ea-modrm-reg dest source)
           (ax-ea-sib dest)
           (ax-ea-addr-disp dest))]
        [(ax-register? dest)
         (emit-code (op source dest code*)
           (build byte prefix-code)
           (build byte #x0F)
           (build byte dstreg-op-code)
           (ax-ea-modrm-reg source dest)
           (ax-ea-sib source)
           (ax-ea-addr-disp source))]
        [else
          ($oops 'assembler-internal "sse-op2 source=~s dest=~s" source dest)])))

  (define float-op2
    (lambda (op op-code1 op-code2 source-ea code*)
      (emit-code (op source-ea code*)
        (build byte
          (byte-fields
            [3 #b11011]
            [0 op-code1]))
        (ax-ea-modrm-ttt source-ea op-code2)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea))))

  (define mul-op
    ; used for movzbl as well as mulsl
    (lambda (op op-code1 op-code2 source-ea dest-reg code*)
      (emit-code (op source-ea dest-reg code*)
        (build byte op-code1)
        (build byte op-code2)
        (ax-ea-modrm-reg source-ea dest-reg)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea))))

  (define muli-op
    (lambda (op op-code imm-data source-ea dest-reg code*)
      (emit-code (op imm-data source-ea dest-reg code*)
        (build byte op-code)
        (ax-ea-modrm-reg source-ea dest-reg)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea)
        (build long (ax-imm-data imm-data)))))

  (define lea-op
    (lambda (op op-code source-ea reg code*)
      (emit-code (op source-ea reg code*)
        (build byte op-code)
        (ax-ea-modrm-reg source-ea reg)
        (ax-ea-sib source-ea)
        (ax-ea-addr-disp source-ea))))

  (define test-op
    (lambda (op size op-code source-ea reg code*)
      (emit-code (op source-ea reg code*)
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
          (build byte #x0f)
          (build byte
            (byte-fields
              [1 #b1011000]
              [0 (ax-size-code size)]))
          (ax-ea-modrm-reg dest-ea new-reg)
          (ax-ea-sib dest-ea)
          (ax-ea-addr-disp dest-ea)))))

  (define pushil-op
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
      (if (and (eq? size 'long)
               (record-case imm-ea
                 [(imm) (n) (<= -128 n 127)]
                 [else #f]))
          (emit-code (op imm-ea dest-ea code*)
            (build byte
              (byte-fields
                [1 #b1000001]
                [0 (ax-size-code size)]))
            (ax-ea-modrm-ttt dest-ea ttt-code)
            (ax-ea-sib dest-ea)
            (ax-ea-addr-disp dest-ea)
            (ax-ea-imm-data 'byte imm-ea))
          (emit-code (op imm-ea dest-ea code*)
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
      (if (and (eq? size 'long)
               (record-case imm-ea
                 [(imm) (n) (<= -128 n 127)]
                 [else #f]))
          (emit-code (op imm-ea dest-ea code*)
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
    (lambda (op size op-code ttt-code imm-ea dest-ea code*)
      (cond
        [(ax-register? dest-ea)
         (emit-code (op imm-ea dest-ea code*)
           (and (eq? size 'word) (build byte 102))
           (build byte
             (byte-fields
               [4 11]
               [3 (ax-size-code size)]
               [0 (ax-ea-reg-code dest-ea)]))
           (ax-ea-imm-data size imm-ea))]
        [else
          (emit-code (op imm-ea dest-ea code*)
            (and (eq? size 'word) (build byte 102))
            (build byte
              (byte-fields
                [1 99]
                [0 (ax-size-code size)]))
            (ax-ea-modrm-ttt dest-ea ttt-code)
            (ax-ea-sib dest-ea)
            (ax-ea-addr-disp dest-ea)
            (ax-ea-imm-data size imm-ea))])))

  ;;; always need byte immediate data for shift ops
  (define shifti-op
    (lambda (op size op-code ttt-code imm-ea dest-ea code*)
      (emit-code (op imm-ea dest-ea code*)
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
           (build byte
             (byte-fields
               [1 op-code]
               [0 (ax-size-code size)]))
           (ax-ea-modrm-reg dest source)
           (ax-ea-sib dest)
           (ax-ea-addr-disp dest))]
        [(ax-register? dest)
         (emit-code (op source dest code*)
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

  (define byte+short-op
    (lambda (op op-code1 t code*)
      (emit-code (op code*)
        (build byte op-code1)
        (build byte (fxand (cadr t) #xFF))
        (build byte (fxsrl (cadr t) 16)))))

  (define byte-reg-op1
    (lambda (op op-code1 reg code*)
      (begin
        (unless (ax-register? reg)
          ($oops 'assembler-internal "(byte-reg-op) ~s is not a real register" reg))
        (emit-code (op reg code*)
          (build byte
            (byte-fields
              [3 op-code1]
              [0 (ax-ea-reg-code reg)]))))))

  (define byte-reg-op2
    (lambda (op op-code1 op-code2 reg code*)
      (begin
        (unless (ax-register? reg)
          ($oops 'assembler-internal "(byte-reg-op) ~s is not a real register" reg))
        (emit-code (op reg code*)
          (build byte op-code1)
          (build byte
            (byte-fields
              [3 op-code2]
              [0 (ax-ea-reg-code reg)]))))))

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

  (define-who ax-size-code
    (lambda (x)
      (case x
        [(byte) 0]
        [(word) 1]
        [(long) 1]
        [else (sorry! who "invalid size ~s" x)])))

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
       (fx+ (fxsll e n) ...)]))

  (define ax-ea-addr-disp
    (lambda (dest-ea)
      (record-case dest-ea
        [(index) (size index-reg base-reg)
         (cond
           [(and (eqv? 0 size) (not (eq? base-reg %ebp))) #f]
           [(ax-byte-size? size) (build byte size)]
           [else (build long size)])]
        [(literal@) stuff (cons 'abs stuff)]
        [(disp) (size reg)
         (cond
           [(and (eqv? 0 size) (not (eq? reg %ebp))) #f] ; indirect
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
           (ax-ss-index-base (reg-mdinfo index-reg) (reg-mdinfo base-reg))]
          [(literal@) (size addr) #f]
          [(disp) (size reg)
           (and (eq? reg %sp) (ax-ss-index-base #b100 #b100))]
          [(reg) r #f]
          [else ($oops 'assembler-internal "ax-ea-sib dest-ea=~s" dest-ea)]))))

  (define ax-ea-modrm-reg
    (lambda (dest-ea reg)
      (ax-ea-modrm-ttt dest-ea (ax-ea-reg-code reg))))

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
            [(literal@) (size addr) #b101]
            [(disp) (size reg) (reg-mdinfo reg)]
            [(reg) r (reg-mdinfo r)]
            [else ($oops 'assembler-internal "ax-r/m dest-ea=~s" dest-ea)]))]
       [ax-mod ; 2 bits
        (lambda (dest-ea)
          (record-case dest-ea
            [(index) (size index-reg base-reg)
             (cond
               [(and (eqv? 0 size) (not (eq? base-reg %ebp))) #b00]
               [(ax-byte-size? size) #b01]
               [else #b10])]
            [(literal@) stuff #b00]   
            [(disp) (size reg)
             (cond
               [(and (eqv? 0 size) (not (eq? reg %ebp))) #b00] ; indirect
               [(ax-byte-size? size) #b01]
               [else #b10])]
            [(reg) r #b11]
            [else ($oops 'assembler-internal "ax-mod dest-ea ~s" dest-ea)]))])
      (lambda (dest-ea ttt)
        (ax-mod-ttt-r/m (ax-mod dest-ea) ttt (ax-r/m dest-ea)))))

  (define ax-ea-imm-data
    (lambda (size imm-data)
      (record-case imm-data
        [(literal) stuff (cons 'abs stuff)]
        [(funcrel) stuff (cons 'funcrel (ax-ea-imm-data 'long stuff))]
        [(imm) (n) (cons size n)]
        [else ($oops 'assembler-internal
                "ax-ea-imm-data imm-data=~s" imm-data)])))

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
        [(asm) 0]
        [(byte) 1]
        [(word) 2]
        [else 4])))

  (define asm-move
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
            [(zext8) (emit movzb src dest code*)]
            [(zext16) (emit movzw src dest code*)]
            [else (sorry! who "unexpected op ~s" op)])))))

  (define asm-fstpl
    (lambda (code* dest)
      (Trivit (dest)
        (emit fstpl dest code*))))

  (define asm-fstps
    (lambda (code* dest)
      (Trivit (dest)
        (emit fstps dest code*))))

  (define asm-fldl
    (lambda (code* src)
      (Trivit (src)
        (emit fldl src code*))))

  (define asm-flds
    (lambda (code* src)
      (Trivit (src)
        (emit flds src code*))))

  (define asm-fl-cvt
    (lambda (op flreg)
      (lambda (code* base index offset)
        (let ([src (build-mem-opnd base index offset)])
          (case op
            [(load-single->double) (emit sse.cvtss2sd src (cons 'reg flreg) code*)]
            [(load-double->single) (emit sse.cvtsd2ss src (cons 'reg flreg) code*)])))))

  (define asm-fl-store
    (lambda (op flreg)
      (lambda (code* base index offset)
        (let ([dest (build-mem-opnd base index offset)])
          (case op
            [(store-single) (emit sse.movss (cons 'reg flreg) dest code*)]
            [(store-double) (emit sse.movsd (cons 'reg flreg) dest code*)])))))

  (define asm-fl-load
    (lambda (op flreg)
      (lambda (code* base index offset)
        (let ([src (build-mem-opnd base index offset)])
          (case op
            [(load-single) (emit sse.movss src (cons 'reg flreg) code*)]
            [(load-double) (emit sse.movsd src (cons 'reg flreg) code*)])))))

  (define asm-flt
    (lambda (code* src flonumreg)
      (Trivit (src)
        (let ([dest `(disp ,(constant flonum-data-disp) ,flonumreg)]
              [flreg (cons 'reg %flreg1)])
          (emit sse.cvtsi2sd src flreg
            (emit sse.movsd flreg dest code*))))))

  (define asm-flop-2
    (lambda (op)
      (lambda (code* src1 src2 dest)
        (let ([src1 `(disp ,(constant flonum-data-disp) ,src1)]
              [src2 `(disp ,(constant flonum-data-disp) ,src2)]
              [dest `(disp ,(constant flonum-data-disp) ,dest)])
          (let ([code* (emit sse.movsd (cons 'reg %flreg1) dest code*)])
            (let ([code* (case op
                           [(fl+) (emit sse.addsd src2 (cons 'reg %flreg1) code*)]
                           [(fl-) (emit sse.subsd src2 (cons 'reg %flreg1) code*)]
                           [(fl*) (emit sse.mulsd src2 (cons 'reg %flreg1) code*)]
                           [(fl/) (emit sse.divsd src2 (cons 'reg %flreg1) code*)])])
              (emit sse.movsd src1 (cons 'reg %flreg1) code*)))))))

  (define asm-flsqrt
    (lambda (code* src dest)
      (let ([src `(disp ,(constant flonum-data-disp) ,src)]
            [dest `(disp ,(constant flonum-data-disp) ,dest)])
        (emit sse.sqrtsd src (cons 'reg %flreg1)
          (emit sse.movsd (cons 'reg %flreg1) dest code*)))))

  (define asm-trunc
    (lambda (code* dest flonumreg)
      (Trivit (dest)
        (let ([src `(disp ,(constant flonum-data-disp) ,flonumreg)])
          (emit sse.cvttsd2si src dest code*)))))

  (define asm-load
    (lambda (type)
      (lambda (code* dest base index offset)
        (Trivit (dest)
          (let ([src (build-mem-opnd base index offset)])
            (case type
              [(integer-32 unsigned-32) (emit mov src dest code*)]
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
                 [(integer-32 unsigned-32) (emit movi src dest code*)]
                 [(integer-16 unsigned-16) (emit moviw (imm16 n) dest code*)]
                 [(integer-8 unsigned-8) (emit movib (imm8 n) dest code*)]
                 [else (sorry! 'asm-store "unexpected mset! type ~s" type)])]
              [(literal) stuff
               (case type
                 [(integer-32 unsigned-32) (emit movi src dest code*)]
                 [(integer-16 unsigned-16) (emit moviw src dest code*)]
                 [(integer-8 unsigned-8) (emit movib src dest code*)]
                 [else (sorry! 'asm-store "unexpected mset! type ~s" type)])]
              [else
                (case type
                  [(integer-32 unsigned-32) (emit mov src dest code*)]
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
              [(integer-16) (emit asri '(imm 16) dest code*)]
              [(unsigned-16) (emit lsri '(imm 16) dest code*)]
              [(integer-32 unsigned-32) code*]
              [else ($oops 'assembler-internal "unexpected asm-swap type argument ~s" type)]))))))

  (define asm-mul
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (emit muls src1 dest code*))))

  (define asm-div
    (lambda (code* dest-eax src-eax src-edx src2)
      (Trivit (src2)
        (safe-assert (and (eq? dest-eax %eax) (eq? src-eax %eax) (eq? src-edx %edx)))
        (emit divsax src2 code*))))

  (define asm-sext-eax->edx
    (lambda (code* dest-edx src-eax)
      (safe-assert (and (eq? dest-edx %edx) (eq? src-eax %eax)))
      (emit extad code*)))

  (define asm-muli
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit mulsi src1 src0 dest code*))))

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
      ; edx is an implied dest and included in info's kill list
      (safe-assert (eq? dest %eax))
      (safe-assert (eq? src %ecx))
      (emit rdpmc code*)))

  (define asm-read-time-stamp-counter
    (lambda (code* dest)
      ; edx is an implied dest and included in info's kill list
      (safe-assert (eq? dest %eax))
      (emit rdtsc code*)))

  (define asm-inc-profile-counter
    (lambda (code* dest src)
      (Trivit (dest src)
        (record-case src
          [(imm) (n) (if (eqv? n 1) (emit inc dest code*) (emit addi src dest code*))]
          [(literal) stuff (emit addi src dest code*)]
          [else (emit add src dest code*)]))))

  (define-who asm-inc-cc-counter
    (lambda (code* base offset val)
      (let-values ([(lo-dest hi-dest)
                    (nanopass-case (L16 Triv) offset
                      [(immediate ,imm)
                       (values `(disp ,imm ,base) `(disp ,(+ imm (constant ptr-bytes)) ,base))]
                      [,x (values `(index 0 ,x ,base) `(index ,(constant ptr-bytes) ,x ,base))]
                      [else ($oops who "unexpected increment offset ~s" offset)])])
        (let ([code* (emit adci '(imm 0) hi-dest code*)])
          (nanopass-case (L16 Triv) val
            [(immediate ,imm) (emit addi `(imm ,imm) lo-dest code*)]
            [,x (emit add (cons 'reg x) lo-dest code*)]
            [else ($oops who "unsupported increment ~s" val)])))))

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
      (constant-case machine-type-name
        ; remove padding added by asm-enter
        [(i3nt ti3nt) (emit ret '())]
        [else (emit addi '(imm 12) (cons 'reg %sp) (emit ret '()))])))

  (define asm-c-return
    (lambda (info)
      (if (info-c-return? info)
          (let ([offset (info-c-return-offset info)])
            (safe-assert (<= 0 offset #xFFFF))
            (emit retl `(imm ,offset) '()))
          (emit ret '()))))

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
            (safe-assert (ax-register? src1 %ecx))
            (emit asl dest code*)]))))

  (define asm-srl
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit lsri src1 dest code*)]
          [else
            (safe-assert (ax-register? src1 %ecx))
            (emit lsr dest code*)]))))

  (define asm-sra
    (lambda (code* dest src0 src1)
      (Trivit (dest src1)
        (safe-assert (equal? (Triv->rand src0) dest))
        (record-case src1
          [(imm literal) stuff (emit asri src1 dest code*)]
          [else
            (safe-assert (ax-register? src1 %ecx))
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
                          [(reg) r (byte-register? r)]
                          ; counting on little-endian byte order
                          [(disp index literal@) stuff #t]))
                   (emit testib y x '())
                   (emit testi y x '()))]
              [(literal) stuff (emit testi y x '())]
              [else (emit test x y '())])
            (let-values ([(l1 l2) (if i? (values l2 l1) (values l1 l2))])
              (asm-conditional-jump info l2 l1 offset)))))))

  (define asm-fl-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (values
          (let ([x `(disp ,(constant flonum-data-disp) ,x)]
                [y `(disp ,(constant flonum-data-disp) ,y)])
            (emit sse.movsd y (cons 'reg %flreg1)
              (emit sse.ucomisd x (cons 'reg %flreg1) '())))
          (asm-conditional-jump info l1 l2 offset)))))

  (define asm-relop
    (lambda (info)
      (rec asm-relop-internal
        (lambda (l1 l2 offset x y)
          (Trivit (x y)
            (safe-assert
              (record-case x
                [(reg disp index literal@) ignore #t]
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
      ; we normally need 8 to store the floating point return variable, but
      ; on some OS's we need 16 in order to get the required 16-byte alignment
      (emit subi `(imm ,(constant-case machine-type-name [(i3nt ti3nt) 8] [else 16]))
        (cons 'reg %sp)
        (emit fstpl `(disp 0 ,%sp) code*))))

  (define asm-restore-flrv
    (lambda (code*)
      ; we normally need 8 to store the floating point return variable, but
      ; on some OS's we need 16 in order to get the required 16-byte alignment
      (emit fldl `(disp 0 ,%sp)
        (emit addi `(imm ,(constant-case machine-type-name [(i3nt ti3nt) 8] [else 16]))
          (cons 'reg %sp) code*))))

  (define asm-library-jump
    (lambda (l)
      (emit bra
        `(literal ,(constant code-data-disp) (library-code ,(libspec-label-libspec l)))
        '())))

  (define asm-library-call
    (lambda (libspec)
      (let ([target `(literal ,(constant code-data-disp) (library-code ,libspec))])
        (rec asm-asm-call-internal
          (lambda (code* . ignore) ; ignore arguments, which must be in fixed locations
            (emit bsr target code*))))))

  (define asm-c-simple-call
    (lambda (entry)
      (let ([target `(literal 0 (entry ,entry))])
        (rec asm-c-simple-call-internal
          (lambda (code*)
            (emit bsr target code*))))))

  (define asm-get-tc
    (let ([target `(literal 0 (entry ,(lookup-c-entry get-thread-context)))])
      (lambda (code* dest) ; dest is ignored, since it is always the first C result (eax in this case)
        (emit bsr target code*))))

  (define asm-activate-thread
    (let ([target `(literal 0 (entry ,(lookup-c-entry activate-thread)))])
      (lambda (code* dest) ; dest is ignored, as in asm-get-tc
        (emit bsr target code*))))

  (define asm-deactivate-thread
    (let ([target `(literal 0 (entry ,(lookup-c-entry deactivate-thread)))])
      (lambda (code*)
        (emit bsr target code*))))

  (define asm-unactivate-thread
    (let ([target `(literal 0 (entry ,(lookup-c-entry unactivate-thread)))])
      (lambda (code*)
        (emit bsr target code*))))

  (define asm-indirect-call
    (lambda (code* t)
      (Trivit (t)
        (emit call t code*))))

  (define asm-direct-jump
    (lambda (l offset)
      (emit bra (make-funcrel 'literal l offset) '())))

  (define asm-literal-jump
    (lambda (info)
      (emit bra
        `(literal ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info)))
        '())))

  (define asm-indirect-jump
    (lambda (t)
      (Trivit (t)
        (emit jmp t '()))))

  (define-who asm-return-address
    (lambda (dest l incr-offset next-addr)
      ; no pc-relative addressing on x86 (except via call/pop),
      ; so just use move and let the linker hook it up
      (make-rachunk dest l incr-offset next-addr
        (asm-move '() dest (with-output-language (L16 Triv) `(label-ref ,l ,incr-offset))))))

  (define asm-jump
    (lambda (l next-addr)
      (make-gchunk l next-addr
        (cond
          [(local-label-offset l) =>
           (lambda (offset)
             (let ([disp (fx- next-addr offset)])
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
                 (values disp `(label ,disp ,l))))]
            [(libspec-label? l)
             (values 0 `(literal ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))]
            [else (values 0 `(label 0 ,l))])))
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
              [(carry) (i? bcc bcs)]
              ; unordered: zf,pf,cf <- 111; gt: 000; lt: 001; eq: 100
              ; reversed & inverted: !(fl< y x) = !(fl> x y) iff zf = 1 & cf = 1
              [(fl<) bls]
              ; reversed & inverted: !(fl<= y x) = !(fl>= x y) iff cf = 1
              [(fl<=) bcs]
              ; inverted: !(fl= x y) iff zf = 0 or cf (or pf) = 1
              [(fl=) (or bne bcs)]))))))

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

  (constant-case machine-type-name
    [(i3nt ti3nt) (define asm-enter values)]
    [else
     (define-syntax asm-enter
       (lambda (x)
         (syntax-case x ()
           [(k e)
            (with-implicit (k %seq %inline)
              #'(%seq
                 ; adjust to 16-byte boundary, accounting for 4-byte return address pushed by call
                 (set! ,%sp ,(%inline - ,%sp (immediate 12)))
                 ,e))])))])

  (define callee-expects-result-pointer?
    (lambda (result-type)
      (nanopass-case (Ltype Type) result-type
        [(fp-ftd& ,ftd) (constant-case machine-type-name
                          [(i3osx ti3osx i3nt ti3nt)
                           (case ($ftd-size ftd)
                             [(1 2 4 8) #f]
                             [else #t])]
                          [else ($ftd-compound? ftd)])]
        [else #f])))
  (define callee-pops-result-pointer?
    (lambda (result-type)
      (callee-expects-result-pointer? result-type)))
  (define fill-result-pointer-from-registers?
    (lambda (result-type)
      (nanopass-case (Ltype Type) result-type
        [(fp-ftd& ,ftd) (not (callee-expects-result-pointer? result-type))]
        [else #f])))

  (module (push-registers pop-registers push-registers-size)
    (define (move-registers regs fp-reg-count load? offset e)
      (with-output-language (L13 Effect)
        (cond
         [(fx> fp-reg-count 0)
          (let ([offset (fx- offset 8)])
            (move-registers regs (fx- fp-reg-count 1) load? offset
                            (cond
                             [load? `(seq ,(%inline fldl ,(%mref ,%sp ,offset)) ,e)]
                             [else  `(seq ,e ,(%inline fstpl ,(%mref ,%sp ,offset)))])))]
         [(pair? regs)
          (let ([offset (fx- offset 4)])
            (move-registers (cdr regs) 0 load? offset
                            (cond
                             [load? `(seq (set! ,(car regs) ,(%mref ,%sp ,offset)) ,e)]
                             [else  `(seq ,e (set! ,(%mref ,%sp ,offset) ,(car regs)))])))]
         [else e])))
    (define (push-registers-size regs fp-reg-count arg-count)
      ;; Align with the expectation that `arg-count` arguments
      ;; will be pushed later, before a function call
      (let ([offset (fx+ (fx* 4 (length regs)) (fx* 8 fp-reg-count))])
        (constant-case machine-type-name
          [(i3nt ti3nt) offset]
          [else
           (fx- (fxlogand (fx+ offset (fx* 4 arg-count) 15) -16)
                 (fx* 4 arg-count))])))
    (define (push-registers regs fp-reg-count arg-count)
      (let ([offset (push-registers-size regs fp-reg-count arg-count)])
        (move-registers regs fp-reg-count #f offset
                        (with-output-language (L13 Effect)
                          `(set! ,%sp ,(%inline - ,%sp (immediate ,offset)))))))
    (define (pop-registers regs fp-reg-count arg-count)
      (let ([offset (push-registers-size regs fp-reg-count arg-count)])
        (move-registers regs fp-reg-count #t offset
                        (with-output-language (L13 Effect)
                                              `(set! ,%sp ,(%inline + ,%sp (immediate ,offset))))))))

  (define asm-foreign-call
    (with-output-language (L13 Effect)
      (letrec ([load-double-stack
                (lambda (offset)
                  (lambda (x) ; requires var
                    (%seq
                      (inline ,(make-info-loadfl %flreg1) ,%load-double ,x ,%zero ,(%constant flonum-data-disp))
                      (inline ,(make-info-loadfl %flreg1) ,%store-double ,%sp ,%zero (immediate ,offset)))))]
               [load-single-stack
                (lambda (offset)
                  (lambda (x) ; requires var
                    (%seq
                      (inline ,(make-info-loadfl %flreg1) ,%load-double->single ,x ,%zero ,(%constant flonum-data-disp))
                      (inline ,(make-info-loadfl %flreg1) ,%store-single ,%sp ,%zero (immediate ,offset)))))]
               [load-stack
                 (lambda (offset)
                   (lambda (rhs) ; requires rhs
                     `(set! ,(%mref ,%sp ,offset) ,rhs)))]
               [load-stack64
                 (lambda (offset)
                   (lambda (lorhs hirhs) ; requires rhs
                     (%seq
                       (set! ,(%mref ,%sp ,offset) ,lorhs)
                       (set! ,(%mref ,%sp ,(fx+ offset 4)) ,hirhs))))]
               [load-content
                (lambda (offset len)
                  (lambda (x) ; requires var
                    (let loop ([offset offset] [x-offset 0] [len len])
                      (cond
                       [(= len 0) `(nop)]
                       [(>= len 4)
                        `(seq
                          (set! ,(%mref ,%sp ,offset) (inline ,(make-info-load 'integer-32 #f)
                                                              ,%load ,x ,%zero (immediate ,x-offset)))
                          ,(loop (fx+ offset 4) (fx+ x-offset 4) (fx- len 4)))]
                       [(>= len 2)
                        (%seq
                          (set! ,%eax (inline ,(make-info-load 'integer-16 #f)
                                              ,%load ,x ,%zero (immediate ,x-offset)))
                          (inline ,(make-info-load 'integer-16 #f)
                                  ,%store ,%sp ,%zero (immediate ,offset)
                                  ,%eax)
                          ,(loop (fx+ offset 2) (fx+ x-offset 2) (fx- len 2)))]
                       [else
                        (%seq
                         (set! ,%eax (inline ,(make-info-load 'integer-8 #f)
                                             ,%load ,x ,%zero (immediate ,x-offset)))
                         (inline ,(make-info-load 'integer-8 #f)
                                 ,%store ,%sp ,%zero (immediate ,offset)
                                 ,%eax))]))))]
               [do-stack
                (lambda (types locs n result-type)
                  (if (null? types)
                      (values n locs)
                      (nanopass-case (Ltype Type) (car types)
                        [(fp-double-float)
                         (do-stack (cdr types)
                           (cons (load-double-stack n) locs)
                           (fx+ n 8)
                           #f)]
                        [(fp-single-float)
                         (do-stack (cdr types)
                           (cons (load-single-stack n) locs)
                           (fx+ n 4)
                           #f)]
                        [(fp-ftd& ,ftd)
                         (do-stack (cdr types)
                           (cons (load-content n ($ftd-size ftd)) locs)
                           (fx+ n (fxlogand (fx+ ($ftd-size ftd) 3) -4))
                           #f)]
                        [(fp-ftd ,ftd)
                         (cond
                          [(and result-type
                                (fill-result-pointer-from-registers? result-type))
                           ;; Callee doesn't expect this argument; move
                           ;; it to the end just to save it for filling
                           ;; when the callee returns
                           (let ([end-n 0])
                             (with-values (do-stack (cdr types)
                                                    (cons (lambda (rhs)
                                                            ((load-stack end-n) rhs))
                                                          locs)
                                                    n
                                                    #f)
                               (lambda (frame-size locs)
                                 (set! end-n frame-size)
                                 (values (fx+ frame-size 4) locs))))]
                          [else
                           (do-stack (cdr types)
                               (cons (load-stack n) locs)
                               (fx+ n 4)
                               #f)])]
                        [else
                         (if (nanopass-case (Ltype Type) (car types)
                               [(fp-integer ,bits) (fx= bits 64)]
                               [(fp-unsigned ,bits) (fx= bits 64)]
                               [else #f])
                             (do-stack (cdr types)
                               (cons (load-stack64 n) locs)
                               (fx+ n 8)
                               #f)
                             (do-stack (cdr types)
                               (cons (load-stack n) locs)
                               (fx+ n 4)
                               #f))])))])
        (define (get-result-registers fill-result-here? result-type)
          (cond
           [fill-result-here?
            (let* ([ftd (nanopass-case (Ltype Type) result-type
                          [(fp-ftd& ,ftd) ftd])]
                   [size ($ftd-size ftd)])
              (case size
                [(4)
                 (cond
                  [(and (if-feature windows (not ($ftd-compound? ftd)) #t)
                        (equal? '((float 4 0)) ($ftd->members ftd)))
                   (values '() 1)]
                  [else (values (reg-list %eax) 0)])]
                [(8)
                 (cond
                  [(and (if-feature windows (not ($ftd-compound? ftd)) #t)
                        (equal? '((float 8 0)) ($ftd->members ftd)))
                   (values '() 1)]
                  [else (values (reg-list %eax %edx) 0)])]
                [else (values (reg-list %eax) 0)]))]
           [else
            (nanopass-case (Ltype Type) result-type
              [(fp-double-float) (values '() 1)]
              [(fp-single-float) (values '() 1)]
              [(fp-integer ,bits)
               (case bits
                 [(64) (values (reg-list %eax %edx) 0)]
                 [else (values (reg-list %eax) 0)])]
              [(fp-unsigned ,bits)
               (case bits
                 [(64) (values (reg-list %eax %edx) 0)]
                 [else (values (reg-list %eax) 0)])]
              [(fp-void) (values '() 0)]
              [else (values (reg-list %eax) 0)])]))
        (define (add-deactivate adjust-active? fill-result-here? t0 result-type e)
          (cond
           [adjust-active?
            (let-values ([(result-regs result-fp-count) (get-result-registers fill-result-here? result-type)])
              (let ([save-and-restore
                     (lambda (regs fp-count e)
                       (cond
                        [(and (null? regs) (fx= 0 fp-count)) e]
                        [else (%seq
                               ,(push-registers regs fp-count 0)
                               ,e
                               ,(pop-registers regs fp-count 0))]))])
                (%seq
                 (set! ,%edx ,t0)
                 ,(save-and-restore (list %edx) 0 (%inline deactivate-thread))
                 ,e
                 ,(save-and-restore result-regs result-fp-count `(set! ,%eax ,(%inline activate-thread))))))]
           [else e]))
        (define returnem
          (lambda (conv* orig-frame-size locs result-type ccall r-loc)
            (let ([frame-size (constant-case machine-type-name
                                ; maintain 16-byte alignment not including the return address pushed
                                ; by the call instruction, which counts as part of callee's frame
                                [(i3nt ti3nt) orig-frame-size]
                                [else (fxlogand (fx+ orig-frame-size 15) -16)])])
              (values (lambda ()
                        (if (fx= frame-size 0)
                            `(nop)
                            `(set! ,%sp ,(%inline - ,%sp (immediate ,frame-size)))))
                (reverse locs)
                ccall
                r-loc
                ; Windows __stdcall convention requires callee to clean up
                (lambda ()
                  (if (or (fx= frame-size 0) (memq 'i3nt-stdcall conv*) (memq 'i3nt-com conv*))
                      `(nop)
                      (let ([frame-size (if (callee-pops-result-pointer? result-type)
                                            (fx- frame-size (constant ptr-bytes))
                                            frame-size)])
                        `(set! ,%sp ,(%inline + ,%sp (immediate ,frame-size))))))))))
        (lambda (info)
          (safe-assert (reg-callee-save? %tc)) ; no need to save-restore
          (let ([conv* (info-foreign-conv* info)]
                [arg-type* (info-foreign-arg-type* info)]
                [result-type (info-foreign-result-type info)])
            (with-values (do-stack arg-type* '() 0 result-type)
              (lambda (frame-size locs)
                (returnem conv* frame-size locs result-type
                  (lambda (t0)
                    (let* ([fill-result-here? (fill-result-pointer-from-registers? result-type)]
                           [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)]
                           [t (if adjust-active? %edx t0)] ; need a register if `adjust-active?`
                           [call
                            (add-deactivate adjust-active? fill-result-here? t0 result-type
                              (cond
                                [(memq 'i3nt-com conv*)
                                 (when (null? arg-type*)
                                   ($oops 'foreign-procedure
                                          "__com convention requires instance argument"))
                                 ; jump indirect
                                 (%seq
                                  (set! ,%eax ,(%mref ,%sp 0))
                                  (set! ,%eax ,(%mref ,%eax 0))
                                  (set! ,%eax ,(%inline + ,%eax ,t))
                                  (inline ,(make-info-kill*-live* (reg-list %eax %edx) '()) ,%c-call ,(%mref ,%eax 0)))]
                                [else `(inline ,(make-info-kill*-live* (reg-list %eax %edx) '()) ,%c-call ,t)]))])
                      (cond
                       [fill-result-here?
                        (let* ([ftd (nanopass-case (Ltype Type) result-type
                                      [(fp-ftd& ,ftd) ftd])]
                               [size ($ftd-size ftd)])
                          (%seq
                           ,call
                           (set! ,%ecx ,(%mref ,%sp ,(fx- frame-size (constant ptr-bytes))))
                           ,(case size
                              [(1)
                               `(inline ,(make-info-load 'integer-8 #f) ,%store
                                        ,%ecx ,%zero (immediate ,0) ,%eax)]
                              [(2)
                               `(inline ,(make-info-load 'integer-16 #f) ,%store
                                        ,%ecx ,%zero (immediate ,0) ,%eax)]
                              [(4)
                               (cond
                                [(and (if-feature windows (not ($ftd-compound? ftd)) #t)
				      (equal? '((float 4 0)) ($ftd->members ftd)))
                                 `(set! ,(%mref ,%ecx 0) ,(%inline fstps))]
                                [else
                                 `(set! ,(%mref ,%ecx 0) ,%eax)])]
                              [(8)
                               (cond
                                [(and (if-feature windows (not ($ftd-compound? ftd)) #t)
				      (equal? '((float 8 0)) ($ftd->members ftd)))
                                 `(set! ,(%mref ,%ecx 0) ,(%inline fstpl))]
                                [else
                                 `(seq
                                   (set! ,(%mref ,%ecx 0) ,%eax)
                                   (set! ,(%mref ,%ecx 4) ,%edx))])])))]
                       [else call])))
                  (nanopass-case (Ltype Type) result-type
                    [(fp-double-float)
                     (lambda (x)
                       `(set! ,(%mref ,x ,(constant flonum-data-disp))
                          ,(%inline fstpl)))]
                    [(fp-single-float)
                     (lambda (x)
                       `(set! ,(%mref ,x ,(constant flonum-data-disp))
                          ,(%inline fstpl)))]
                    [(fp-integer ,bits)
                     (case bits
                       [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline sext8 ,%eax)))]
                       [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline sext16 ,%eax)))]
                       [(32) (lambda (lvalue) `(set! ,lvalue ,%eax))]
                       [(64) (lambda (lvlow lvhigh)
                               ; right now we are using ac0 (edx) for our low value and ac1 (pseudo-reg)
                               ; for the high value.  As a result we need to be careful to clear edx (ac0)
                               ; before we set ac0 (edx)
                               `(seq
                                  (set! ,lvhigh ,%edx)
                                  (set! ,lvlow ,%eax)))]
                       [else ($oops 'assembler-internal
                               "unexpected asm-foreign-procedures fp-integer size ~s"
                               bits)])]
                    [(fp-unsigned ,bits)
                     (case bits
                       [(8) (lambda (lvalue) `(set! ,lvalue ,(%inline zext8 ,%eax)))]
                       [(16) (lambda (lvalue) `(set! ,lvalue ,(%inline zext16 ,%eax)))]
                       [(32) (lambda (lvalue) `(set! ,lvalue ,%eax))]
                       [(64) (lambda (lvlow lvhigh)
                               ; right now we are using ac0 (edx) for our low value and ac1 (pseudo-reg)
                               ; for the high value.  As a result we need to be careful to clear edx (ac0)
                               ; before we set ac0 (edx)
                               `(seq
                                  (set! ,lvhigh ,%edx)
                                  (set! ,lvlow ,%eax)))]
                       [else ($oops 'assembler-internal
                               "unexpected asm-foreign-procedures fp-integer size ~s"
                               bits)])]
                    [else (lambda (lvalue) `(set! ,lvalue ,%eax))])))))))))

  (define asm-foreign-callable
    #|
                   Frame Layout
                   +---------------------------+
                   |                           |
                   |    incoming stack args    |
         sp+X+Y+Z: |                           |
                   +---------------------------+ <- i3nt/ti3nt: 4-byte boundary. other: 16-byte boundary
                   |   incoming return address | one word
                   +---------------------------+
                   |                           | 
                   |   callee-save registers   | EBP, ESI, EDI, EBX (4 words)
           sp+X+Y: |                           |
                   +---------------------------+
             sp+X: |      unactivate mode      | 0 words or 1 word
                   +---------------------------+
                   |   indirect result space   | i3nt/ti3nt: 2 words
                   |  (for & results via regs) | other: 3 words
             sp+0: +---------------------------+<- i3nt/ti3nt: 4-byte boundary. other: 16-byte boundary
      |#


    (with-output-language (L13 Effect)
      (let ()
        (define load-double-stack
          (lambda (offset)
            (lambda (x) ; requires var
              (%seq
                (inline ,(make-info-loadfl %flreg1) ,%load-double ,%sp ,%zero (immediate ,offset))
                (inline ,(make-info-loadfl %flreg1) ,%store-double ,x ,%zero ,(%constant flonum-data-disp))))))
        (define load-single-stack
          (lambda (offset)
            (lambda (x) ; requires var
              (%seq
                (inline ,(make-info-loadfl %flreg1) ,%load-single->double ,%sp ,%zero (immediate ,offset))
                (inline ,(make-info-loadfl %flreg1) ,%store-double ,x ,%zero ,(%constant flonum-data-disp))))))
        (define load-stack
          (lambda (type offset)
            (lambda (lvalue) ; requires lvalue
              (nanopass-case (Ltype Type) type
                [(fp-integer ,bits)
                 (case bits
                   [(8) `(set! ,lvalue (inline ,(make-info-load 'integer-8 #f) ,%load
                                         ,%sp ,%zero (immediate ,offset)))]
                   [(16) `(set! ,lvalue (inline ,(make-info-load 'integer-16 #f) ,%load
                                          ,%sp ,%zero (immediate ,offset)))]
                   [(32) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                   [else ($oops 'assembler-internal
                           "unexpected load-int-stack fp-integer size ~s"
                           bits)])]
                [(fp-unsigned ,bits)
                 (case bits
                   [(8) `(set! ,lvalue (inline ,(make-info-load 'unsigned-8 #f) ,%load
                                         ,%sp ,%zero (immediate ,offset)))]
                   [(16) `(set! ,lvalue (inline ,(make-info-load 'unsigned-16 #f) ,%load
                                          ,%sp ,%zero (immediate ,offset)))]
                   [(32) `(set! ,lvalue ,(%mref ,%sp ,offset))]
                   [else ($oops 'assembler-internal
                           "unexpected load-int-stack fp-unsigned size ~s"
                           bits)])]
                [else `(set! ,lvalue ,(%mref ,%sp ,offset))]))))
        (define load-stack-address
          (lambda (offset)
            (lambda (lvalue) ; requires lvalue
              `(set! ,lvalue ,(%inline + ,%sp (immediate ,offset))))))
        (define load-stack64
          (lambda (type offset)
            (lambda (lolvalue hilvalue) ; requires lvalue
              (%seq
                (set! ,lolvalue ,(%mref ,%sp ,offset))
                (set! ,hilvalue ,(%mref ,%sp ,(fx+ offset 4)))))))
        (define do-stack
          (lambda (types locs n)
            (if (null? types)
                (values n locs)
                (nanopass-case (Ltype Type) (car types)
                  [(fp-double-float)
                   (do-stack (cdr types)
                     (cons (load-double-stack n) locs)
                     (fx+ n 8))]
                  [(fp-single-float)
                   (do-stack (cdr types)
                     (cons (load-single-stack n) locs)
                     (fx+ n 4))]
                  [(fp-ftd& ,ftd)
                   (do-stack (cdr types)
                     (cons (load-stack-address n) locs)
                     (fx+ n (fxlogand (fx+ ($ftd-size ftd) 3) -4)))]
                  [else
                   (if (nanopass-case (Ltype Type) (car types)
                         [(fp-integer ,bits) (fx= bits 64)]
                         [(fp-unsigned ,bits) (fx= bits 64)]
                         [else #f])
                       (do-stack (cdr types)
                         (cons (load-stack64 (car types) n) locs)
                         (fx+ n 8))
                       (do-stack (cdr types)
                         (cons (load-stack (car types) n) locs)
                         (fx+ n 4)))]))))
          (define (do-result result-type init-stack-offset indirect-result-to-registers?)
            (nanopass-case (Ltype Type) result-type
              [(fp-ftd& ,ftd)
               (cond
                [indirect-result-to-registers?
                 (cond
                  [(and (if-feature windows (not ($ftd-compound? ftd)) #t)
                        (equal? '((float 4 0)) ($ftd->members ftd)))
                   (values (lambda ()
                             (%inline flds ,(%mref ,%sp 0)))
                           '()
                           1)]
                  [(and (if-feature windows (not ($ftd-compound? ftd)) #t)
                        (equal? '((float 8 0)) ($ftd->members ftd)))
                   (values (lambda ()
                             (%inline fldl ,(%mref ,%sp 0)))
                           '()
                           1)]
                  [(fx= ($ftd-size ftd) 8)
                   (values (lambda ()
                             `(seq
                               (set! ,%eax ,(%mref ,%sp 0))
                               (set! ,%edx ,(%mref ,%sp 4))))
                           (list %eax %edx)
                           0)]
                  [else
                   (values (lambda ()
                             `(set! ,%eax ,(%mref ,%sp 0)))
                           (list %eax)
                           0)])]
                [else
                 (values (lambda ()
                           ;; Return pointer that was filled; destination was the first argument
                           `(set! ,%eax ,(%mref ,%sp ,init-stack-offset)))
                         (list %eax)
                         0)])]
              [(fp-double-float)
               (values (lambda (x)
                         (%inline fldl ,(%mref ,x ,(constant flonum-data-disp))))
                       '()
                       1)]
              [(fp-single-float)
               (values (lambda (x)
                         (%inline fldl ,(%mref ,x ,(constant flonum-data-disp))))
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
                 (values (lambda (lorhs hirhs) ; requires rhs
                           (%seq
                            (set! ,%eax ,lorhs)
                            (set! ,%edx ,hirhs)))
                         (list %eax %edx)
                         0)]
                [else
                 (values (lambda (x)
                           `(set! ,%eax ,x))
                         (list %eax)
                         0)])]))
          (define (unactivate result-regs result-num-fp-regs)
            (let ([e (%seq
                       (set! ,%eax ,(%mref ,%sp ,(+ 8 (push-registers-size result-regs result-num-fp-regs 1))))
                       ,(%inline push ,%eax)
                       ,(%inline unactivate-thread)
                       ,(%inline pop ,%eax))])
              (if (and (null? result-regs) (fx= 0 result-num-fp-regs))
                  e
                  (%seq
                   ,(push-registers result-regs result-num-fp-regs 1)
                   ,e
                   ,(pop-registers result-regs result-num-fp-regs 1)))))
        (lambda (info)
          (let* ([conv* (info-foreign-conv* info)]
                 [adjust-active? (if-feature pthreads (memq 'adjust-active conv*) #f)]
                 [arg-type* (info-foreign-arg-type* info)]
                 [result-type (info-foreign-result-type info)]
                 [indirect-result-space (constant-case machine-type-name
                                          [(i3nt ti3nt) (if adjust-active? 12 8)]
                                          [else
                                           ;; maintain 16-bit alignment, taking into account
                                           ;; 16 bytes pushed above + 4 for RA pushed by asmCcall;
                                           ;; 8 of these bytes are used for &-return space, if needed;
                                           ;; the extra 4 bytes may be used for the unactivate mode
                                           12])]
                 [init-stack-offset (fx+ 20 indirect-result-space)]
		 [indirect-result-to-registers? (fill-result-pointer-from-registers? result-type)])
              (let-values ([(get-result result-regs result-num-fp-regs)
                            (do-result result-type init-stack-offset indirect-result-to-registers?)])
                (with-values (do-stack (if indirect-result-to-registers?
                                           (cdr arg-type*)
                                           arg-type*)
                                       '()
                                       init-stack-offset)
                  (lambda (frame-size locs)
                    (values
                     (lambda ()
                       (%seq
                         ,(%inline push ,%ebp)
                         ,(%inline push ,%esi)
                         ,(%inline push ,%edi)
                         ,(%inline push ,%ebx)
                         (set! ,%sp ,(%inline - ,%sp (immediate ,indirect-result-space)))
                         ,(if-feature pthreads
                            ((lambda (e)
                               (if adjust-active?
                                   (%seq
                                    (set! ,%eax ,(%inline activate-thread))
                                    (set! ,(%mref ,%sp ,8) ,%eax)
                                    ,e)
                                   e))
                             `(seq
                                (set! ,%eax ,(%inline get-tc))
                                (set! ,%tc ,%eax)))
                            `(set! ,%tc (literal ,(make-info-literal #f 'entry (lookup-c-entry thread-context) 0))))))
                     (let ([locs (reverse locs)])
                       (if indirect-result-to-registers?
                           (cons (load-stack-address 0) ; use the &-return space
                                 locs)
                           locs))
                     get-result
                     (lambda ()
                       (define callee-save-regs (list %ebx %edi %esi %ebp))
                       (in-context Tail
                        ((lambda (e)
                           (if adjust-active?
                               (%seq
                                ,(unactivate result-regs result-num-fp-regs)
                                ,e)
                               e))
                         (%seq
                           (set! ,%sp ,(%inline + ,%sp (immediate ,indirect-result-space)))
                           (set! ,%ebx ,(%inline pop))
                           (set! ,%edi ,(%inline pop))
                           (set! ,%esi ,(%inline pop))
                           (set! ,%ebp ,(%inline pop))
                           ; Windows __stdcall convention requires callee to clean up
                           ,((lambda (e)
                               (if (or (memq 'i3nt-stdcall conv*) (memq 'i3nt-com conv*))
                                 (let ([arg-size (fx- frame-size init-stack-offset)])
                                   (if (fx> arg-size 0)
                                       (%seq
                                        (set!
                                         ,(%mref ,%sp ,arg-size)
                                         ,(%mref ,%sp 0))
                                        (set! ,%sp ,(%inline + ,%sp (immediate ,arg-size)))
                                        ,e)
                                       e))
                                 e))
                             `(asm-c-return ,(if (callee-pops-result-pointer? result-type)
                                                 ;; remove the pointer argument provided by the caller
                                                 ;; after popping the return address
                                                 (make-info-c-return 4)
                                                 null-info)
                                            ,callee-save-regs ...
                                            ,result-regs ...)))))))))))))))
  )
