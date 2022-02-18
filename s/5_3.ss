;;; 5_3.ss
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

;;; Care must be take with floating point constants to permit cross
;;; compilation between machines with differing floating point styles.
;;; Negative zero, infinities, large or small numbers, non-binary
;;; fractions, and precise numbers are dangerous and should be calculated.
;;; positive zero, NAN, small integers, and binary fractions with only a few
;;; significant bits are safe on all current machines.
;;; examples:
;;; dangerous: -0.0, +inf.0, -inf.0, 1e100, 1e-100, 0.1
;;; safe: 0.0, +nan.0, 1.0, 2.0, 0.5

(begin
(eval-when (compile)

   (define-constant max-float-exponent
      (float-type-case
         [(ieee) 1023]))

   (define-constant min-float-exponent
      (float-type-case
         [(ieee) -1023]))

   (define-constant float-mantissa-bits
      (float-type-case
         [(ieee) 53]))

)

(let ()
; could use foreign-entry? primitive if foreign.ss were loaded first
(define op-if-entry?
   (let ()
      (define lookup
         (foreign-procedure "(cs)lookup_foreign_entry" (string)
            void*))
      (lambda (op name)
         (and (not (eqv? (lookup name) 0))
              (op name)))))

(let ()

(define cflop1
   (lambda (x)
      (foreign-procedure x (double-float) double-float)))

(define cflop2
   (lambda (x)
      (foreign-procedure x (double-float double-float) double-float)))

(define schemeop1
   (lambda (x)
      (foreign-procedure x (scheme-object) scheme-object)))

(define schemeop2
   (lambda (x)
      (foreign-procedure x (scheme-object scheme-object) scheme-object)))

(let ()

(define biglength (schemeop1 "(cs)s_integer_length"))
(define bigodd? (schemeop1 "(cs)s_bigoddp"))
(define float (schemeop1 "(cs)s_float"))

(define big=
  (foreign-procedure "(cs)s_big_eq" (scheme-object scheme-object)
    boolean))
(define big<
  (foreign-procedure "(cs)s_big_lt" (scheme-object scheme-object)
    boolean))
(define big-negate (schemeop1 "(cs)s_big_negate"))
(define integer-ash (schemeop2 "(cs)s_ash"))
(define integer+ (schemeop2 "(cs)add"))
(define integer* (schemeop2 "(cs)mul"))
(define integer- (schemeop2 "(cs)sub"))
(define integer/ (schemeop2 "(cs)s_div"))
(define intquotient (schemeop2 "(cs)ss_trunc"))
(define intquotient-remainder (schemeop2 "(cs)ss_trunc_rem"))
(define intremainder (schemeop2 "(cs)rem"))

(define $flsin (cflop1 "(cs)sin"))

(define $flcos (cflop1 "(cs)cos"))

(define $flasin (cflop1 "(cs)asin"))

(define $flacos (cflop1 "(cs)acos"))
(define $flfloor (cflop1 "(cs)floor"))
(define $flceiling (cflop1 "(cs)ceil"))

(let ()

(define omega
   (float-type-case
      [(ieee) (float #e1.7976931348623157e308)]))

(define $flexpt
   (machine-case
      [(i3nt ti3nt a6s2 ta6s2 i3s2 ti3s2 i3nb ti3nb a6nb ta6nb)
       ; pow(nan,+0.0) => nan instead of +1.0
       (let ([cexpt (cflop2 "(cs)pow")])
         (lambda (x y)
           (cond
            [(fl= y 0.0) 1.0]
            [else (cexpt x y)])))]
      [else (cflop2 "(cs)pow")]))

(define $fltan (cflop1 "(cs)tan"))

(define flcosh (cflop1 "(cs)cosh"))

(define fltanh
   (machine-case
      [(i3fb ti3fb)
       ; broken for -0.0, +/-inf
       (let ([ctanh (cflop1 "(cs)tanh")])
        (lambda (x)
          (cond
           [(fl= x 0.0) x]
           [(infinity? x) (if (negated-flonum? x) -1.0 1.0)]
           [else (ctanh x)])))]
      [(i3nb ti3nb a6nb ta6nb)
       ; broken for -0.0
       (let ([ctanh (cflop1 "(cs)tanh")])
        (lambda (x)
          (cond
           [(fl= x 0.0) x]
           [else (ctanh x)])))]
      [else (cflop1 "(cs)tanh")]))

(define $flexp (cflop1 "(cs)exp"))

(define $fllog
  (machine-case
    [(a6s2 ta6s2 i3s2 ti3s2 i3ob ti3ob i3nb ti3nb a6nb ta6nb a6ob ta6ob)
     ; broken for -inf.0
     (let ([clog (cflop1 "(cs)log")])
       (lambda (x) (if (and (infinity? x) (negated-flonum? x)) +nan.0 (clog x))))]
    [else (cflop1 "(cs)log")]))

(define $flsqrt (cflop1 "(cs)sqrt"))

(define flatan2
   (machine-case
      [(i3nt ti3nt)
       ; atan2(+inf.0,+inf.0) => pi/2 instead of pi/4
       ; atan2(-inf.0,-inf.0) => -pi/2 instead of -3pi/4
       ; atan2(+inf.0,-inf.0) => NAN instead of 3pi/4
       ; atan2(-inf.0,+inf.0) => NAN instead of -pi/4
       ; atan2(+0.0,-0.0) => +0.0 instead of +pi
       ; atan2(-0.0,-0.0) => -0.0 instead of -pi
       ; atan2(-0.0,-1.0) => pi instead of -pi
       (let ([catan2 (cflop2 "(cs)atan2")])
          (let ([pi (catan2 0.0 -1.0)])
             (lambda (y x)
                (cond
                   [(and (infinity? y) (infinity? x))
                    (let ([y (if (negated-flonum? y) -1.0 1.0)]
                          [x (if (negated-flonum? x) -1.0 1.0)])
                       (catan2 y x))]
                   [(and (fl= y 0.0) (not ($nan? x)))
                    (if (negated-flonum? y)
                        (if (negated-flonum? x) (fl- pi) (fl- 0.0))
                        (if (negated-flonum? x) pi 0.0))]
                   [else (catan2 y x)]))))]
      [(i3ob ti3ob a6ob ta6ob a6s2 ta6s2 i3s2 ti3s2 i3nb ti3nb a6nb ta6nb)
       ; atan2(-0.0,+0.0) => +0.0 instead of -0.0
       ; atan2(+0.0,-0.0) => +0.0 instead of +pi
       ; atan2(-0.0,-0.0) => +0.0 instead of -pi
       (let ([catan2 (cflop2 "(cs)atan2")])
         (let ([pi (catan2 0.0 -1.0)])
           (lambda (y x)
             (cond
              [(and (fl= y 0.0) (not ($nan? x)))
               (if (negated-flonum? y)
                   (if (negated-flonum? x) (fl- pi) (fl- 0.0))
                   (if (negated-flonum? x) pi 0.0))]
              [else (catan2 y x)]))))]
       [else (cflop2 "(cs)atan2")]))

(define $flatan (cflop1 "(cs)atan"))

(define flsinh (cflop1 "(cs)sinh"))

(define flatanh
   (or (op-if-entry? cflop1 "(cs)atanh")
       ; |x| <= 1
       ; principle expression:
       ; (log(1+x)-log(1-x))/2
       ; should use "log1p" but it doesn't exist on the 88k
       (let ([f (lambda (x)
                   (fl* 0.5 (fl- ($fllog (fl+ 1.0 x)) ($fllog (fl- 1.0 x)))))])
          (lambda (x)
             (if (negated-flonum? x) (fl- (f (fl- x))) (f x))))))

(define fllog1+
   (or (op-if-entry? cflop1 "(cs)log1p")
       (lambda (x) ($fllog (fl+ 1.0 x)))))

(let ()

(define log2 ($fllog 2.0))

(define flhypot (cflop2 "(cs)hypot"))

(define flasinh
   ; scheme-coded version needs "log2"
   (or (op-if-entry? cflop1 "(cs)asinh")
       ; prinicple expression:
       ; log(x + sqrt(xx + 1))
       ; avoids spurious overflows
       ; avoids underflow problems from negative x by using identity
       ; asinh(-x) = -asinh(x)
       ; should use "log1p" for small x but it doesn't exist on the 88k
       (let ([f (lambda (x)
                   (if (fl= (fl+ x 1.0) x)
                       (fl+ ($fllog x) log2)
                       ($fllog (fl+ x ($flsqrt (fl+ (fl* x x) 1.0))))))])
          (lambda (x)
             (if (negated-flonum? x) (fl- (f (fl- x))) (f x))))))

(define flacosh
   ; scheme-coded version needs "log2"
   (or (op-if-entry? cflop1 "(cs)acosh")
       ; x >= 1
       ; prinicple expression:
       ; log(x + sqrt(xx - 1))
       ; avoids spurious overflows
       (lambda (x)
          (if (fl= (fl- x 1.0) x)
              (fl+ ($fllog x) log2)
              ($fllog (fl+ x ($flsqrt (fl- (fl* x x) 1.0))))))))

(let ()

(define pi (flatan2 0.0 -1.0))
(define sqrt-omega ($flsqrt omega))
(define log-omega ($fllog omega))
(define acosh-omega (flacosh omega))

(let ()

(define-syntax define-trig-op
  (syntax-rules ()
    [(_ who flop cflop zero-value)
     (set! who
       (lambda (x)
         (type-case x
           [(flonum?) (flop x)]
           [($inexactnum?) (cflop x)]
           [(fixnum?) (if (fx= x 0) zero-value (who (fixnum->flonum x)))]
           [(bignum? ratnum? $exactnum?) (who (inexact x))]
           [else (nonnumber-error 'who x)])))]))

(define $flinteger-or-inf?
  (lambda (x)
    (fl= ($flfloor x) x)))

(define $flinteger?
  (lambda (x)
    (and ($flinteger-or-inf? x)
         (not (exceptional-flonum? x)))))

(define nonnumber-error
   (lambda (who what)
      ($oops who "~s is not a number" what)))

(define noncomplex-error
   (lambda (who what)
      ($oops who "~s is not a complex number" what)))

(define nonreal-error
   (lambda (who what)
      ($oops who "~s is not a real number" what)))

(define nonrational-error
   (lambda (who what)
      ($oops who "~s is not a rational number" what)))

(define noninteger-error
   (lambda (who what)
      ($oops who "~s is not an integer" what)))

(define nonexact-integer-error
   (lambda (who what)
      ($oops who "~s is not an exact integer" what)))

(define noncflonum-error
   (lambda (who what)
      ($oops who "~s is not a cflonum" what)))

(define domain-error
   (lambda (who what)
      ($oops who "undefined for ~s" what)))

(define domain-error2
   (lambda (who x y)
      ($oops who "undefined for values ~s and ~s" x y)))

; note: (cfl*i z) =/= (* +i z) if RP(z) == -0.0
(define cfl*i
   (lambda (z)
      (fl-make-rectangular (fl- (cfl-imag-part z)) (cfl-real-part z))))

; note: (cfl/i z) =/= (/ z +i) or (* -i z) if IP(z) == -0.0
(define cfl/i
   (lambda (z)
      (fl-make-rectangular (cfl-imag-part z) (fl- (cfl-real-part z)))))

; Some of the following is based on
; W. Kahan's "Branch Cuts for Complex Elementary Functions"
; in "The State of the Art of Numerical Analysis"
; (IMA/SIAM proceedings, 1986, pp 165-211)
; ed. by A. Iserles and M.J.D. Powell

; Kahan gives principal expressions and algorithms for several
; complex functions.  The principal expressions are mathematically
; correct, but not necessarily good computationally.  They
; do, however, make good test expressions for ordinary inputs.

; Steele's "Common Lisp: the Language" (second edition) was used
; to determine valid domains for some of the functions.

(define cflmagnitude
   (lambda (z)
      (flhypot (cfl-real-part z) (cfl-imag-part z))))

(define cflangle
   (lambda (z)
      (flatan2 (cfl-imag-part z) (cfl-real-part z))))

(define cfllog
   ; principal expression from Kahan:
   ; log(z) = log(|z|) + angle(z)i
   ; Kahan uses a different algorithm to calculate the real part.
   (let ([f (lambda (x y)
               ; x >= y
               (let ([r (fl/ y x)])
                  (fl+ ($fllog x) (fl* .5 (fllog1+ (fl* r r))))))]
         [k (fl* .5 log2)])
      (lambda (z)
         (let ([x (cfl-real-part z)] [y (cfl-imag-part z)])
            (fl-make-rectangular
               (let ([x (flabs x)] [y (flabs y)])
                  (cond
                     [(fl> x y) (f x y)]
                     [(fl< x y) (f y x)]
                     [(fl= x y) (fl+ ($fllog x) k)]
                     [(infinity? x) x]
                     [(infinity? y) y]
                     [($nan? x) x]
                     [else y]))
               (flatan2 y x))))))

(define cflsqrt
   ; principal expression from Kahan:
   ; sqrt(z) = expt(z,1/2)
   ; Kahan's algorithm except for the calculation of "a"
   (let ([f (let ([k ($flsqrt (fl* .5 (fl+ ($flsqrt 2.0) 1.0)))])
               (lambda (x y)
                  ; sqrt(|x+yi| + |x|)/2
                  (cond
                     [(fl> x y)
                      (let ([r (fl/ y x)])
                         (fl* ($flsqrt x)
                              ($flsqrt (fl* .5 (fl+ ($flsqrt (fl+ 1.0 (fl* r r)))
                                                   1.0)))))]
                     [(fl< x y)
                      (let ([r (fl/ x y)])
                         (fl* ($flsqrt y)
                              ($flsqrt (fl* .5 (fl+ ($flsqrt (fl+ (fl* r r) 1.0))
                                                   r)))))]
                     [(fl= x y) (fl* ($flsqrt x) k)]
                     [(infinity? x) x]
                     [(infinity? y) y]
                     [($nan? x) x]
                     [else y])))])
      (lambda (z)
         (let ([x (cfl-real-part z)] [y (cfl-imag-part z)])
            (let ([a (f (flabs x) (flabs y))])
               (if (fl= a 0.0)
                   (fl-make-rectangular a y)
                   (let ([b (if (infinity? y) y (fl* (fl/ y a) .5))])
                      (if (fl< x 0.0)
                          (fl-make-rectangular
                             (flabs b)
                             (if (negated-flonum? y) (fl- a) a))
                          (fl-make-rectangular a b)))))))))

(define cflexp
   ; exp(a+bi) = exp(a)cos(b) + exp(a)sin(b)i
   (lambda (z)
      (let ([a (cfl-real-part z)] [b (cfl-imag-part z)])
         (cond
           ; perhaps misguidedly treat x+0.0i the same as x
            [(fl= b 0.0) (fl-make-rectangular ($flexp a) b)]
            [(fl<= a log-omega)
             (let ([e^a ($flexp a)])
                (fl-make-rectangular (fl* e^a ($flcos b)) (fl* e^a ($flsin b))))]
            [else (fl-make-rectangular
                    (let ([cosb ($flcos b)])
                      (if (fl< cosb 0.0)
                          (fl- ($flexp (fl+ a ($fllog (fl- cosb)))))
                          ($flexp (fl+ a ($fllog cosb)))))
                    (let ([sinb ($flsin b)])
                      (if (fl< sinb 0.0)
                          (fl- ($flexp (fl+ a ($fllog (fl- sinb)))))
                          ($flexp (fl+ a ($fllog sinb))))))]))))

(define cflslowsinh
   ; probably not the best way to handle this
   (let ([f (lambda (z -z)
               (cfl- (cflexp (cfl- z log2)) (cfl* .5 (cflexp -z))))])
      (lambda (z)
         (if (fl< (cfl-real-part z) 0.0)
             (cfl- (f (cfl- z) z))
             (f z (cfl- z))))))

(define cflslowcosh
   ; probably not the best way to handle this
   (let ([f (lambda (z -z)
               (cfl+ (cflexp (cfl- z log2)) (cfl* .5 (cflexp -z))))])
      (lambda (z)
         (if (fl< (cfl-real-part z) 0.0)
             (f (cfl- z) z)
             (f z (cfl- z))))))

(define cflsin
   ; sin(a+bi) = sin(a)cosh(b)+cos(a)sinh(b)i
   (lambda (z)
      (let ([a (cfl-real-part z)] [b (cfl-imag-part z)])
         (if (fl<= (flabs b) acosh-omega)
             (fl-make-rectangular (fl* ($flsin a) (flcosh b))
                                  (fl* ($flcos a) (flsinh b)))
             (cfl/i (cflslowsinh (cfl*i z)))))))

(define cflcos
   ; cos(a+bi) = cos(a)cosh(b)-sin(a)sinh(b)i
   (lambda (z)
      (let ([a (cfl-real-part z)] [b (cfl-imag-part z)])
         (if (fl<= (flabs b) acosh-omega)
             (fl-make-rectangular (fl* ($flcos a) (flcosh b))
                                  (fl- (fl* ($flsin a) (flsinh b))))
             (cflslowcosh (cfl*i z))))))

(define cfltan
   ; from Kahan
   (lambda (z)
      (cfl/i (cfltanh (cfl*i z)))))

(define cflacos
   ; from Kahan
   ; principal expression:
   ; 2log(sqrt((1+z)/2) + sqrt((1-z)/2)i)/i = pi/2 - asin(z)
   ; returns a+bi where
   ; a = 2atan(RP(sqrt(1-z))/RP(sqrt(1+z)))
   ; b = asinh(IP(conjugate(sqrt(1+z)))sqrt(1-z))
   (lambda (z)
      (let ([z- (cflsqrt (cfl- 1.0 z))]
            [z+ (cflsqrt (cfl+ 1.0 z))])
         (let ([a (cfl-real-part z-)] [b (cfl-imag-part z-)]
               [c (cfl-real-part z+)] [d (cfl-imag-part z+)])
            (fl-make-rectangular (fl* 2.0 ($flatan (fl/ a c)))
                                 (flasinh (fl- (fl* b c) (fl* a d))))))))

(define cflasin
   ; from Kahan
   ; principal expression:
   ; asinh(iz)/i
   ; returns a+bi where
   ; a = atan(RP(z)/RP(sqrt(1-z)sqrt(1+z)))
   ; b = asinh(IP(conjugate(sqrt(1-z))sqrt(1+z)))
   (lambda (z)
      (let ([z- (cflsqrt (cfl- 1.0 z))]
            [z+ (cflsqrt (cfl+ 1.0 z))])
         (let ([a (cfl-real-part z-)] [b (cfl-imag-part z-)]
               [c (cfl-real-part z+)] [d (cfl-imag-part z+)])
            (fl-make-rectangular
               ($flatan (fl/ (cfl-real-part z) (fl- (fl* a c) (fl* b d))))
               (flasinh (fl- (fl* a d) (fl* b c))))))))

(define cflasinh
   ; from Kahan
   ; principal expression:
   ; log(z + sqrt(1 + zz))
   (lambda (z)
      (cfl/i (cflasin (cfl*i z)))))

(define cflsinh
   ; sinh(a+bi) = sinh(a)cos(b)+cosh(a)sin(b)i
   (lambda (z)
      (let ([a (cfl-real-part z)] [b (cfl-imag-part z)])
         (if (fl<= a acosh-omega)
             (fl-make-rectangular (fl* (flsinh a) ($flcos b))
                                  (fl* (flcosh a) ($flsin b)))
             (cflslowsinh z)))))

(define cflcosh
   ; cosh(a+bi) = cosh(a)cos(b)+sinh(a)sin(b)i
   (lambda (z)
      (let ([a (cfl-real-part z)] [b (cfl-imag-part z)])
         (if (fl<= a acosh-omega)
             (fl-make-rectangular (fl* (flcosh a) ($flcos b))
                                  (fl* (flsinh a) ($flsin b)))
             (cflslowcosh z)))))

(define cfltanh
   ; from Kahan
   (let ([theta (fl/ acosh-omega 4.0)])
      (lambda (z)
         (let ([x (cfl-real-part z)] [y (cfl-imag-part z)])
            (let ([ax (flabs x)])
               (if (fl> ax theta)
                   (fl-make-rectangular
                      (if (negated-flonum? x) -1.0 1.0)
                      (if (negated-flonum? y) (fl- 0.0) 0.0))
                   (let ([t ($fltan y)]
                         [s (flsinh x)])
                      (let ([beta (fl+ 1.0 (fl* t t))]
                            [ss (fl* s s)])
                         (let ([rho ($flsqrt (fl+ 1.0 ss))])
                            (if (infinity? t)
                                (fl-make-rectangular (fl/ rho s) (/ t))
                                (let ([k (/ (fl+ 1.0 (fl* beta ss)))])
                                   (fl-make-rectangular (fl* beta rho s k)
                                                        (fl* t k)))))))))))))

(define cflacosh
   ; from Kahan
   ; principal expression:
   ; 2log(sqrt((z+1)/2) + sqrt((z-1)/2))
   ; returns a+bi where
   ; a = (asinh (real-part (* (conjugate (sqrt (- z 1))) (sqrt (+ z 1)))))
   ; b = (* 2 (atan (/ (imag-part (sqrt (- z 1))) (real-part (sqrt (+ z 1))))))
   (lambda (z)
      (let ([z- (cflsqrt (cfl- z 1.0))]
            [z+ (cflsqrt (cfl+ z 1.0))])
         (let ([a (cfl-real-part z-)] [b (cfl-imag-part z-)]
               [c (cfl-real-part z+)] [d (cfl-imag-part z+)])
            (fl-make-rectangular (flasinh (fl+ (fl* a c) (fl* b d)))
                                 (fl* 2.0 ($flatan (fl/ b c))))))))

(define cflatanh
   ; principal expression from Kahan:
   ; (log(1+z) - log(1-z))/2
   (let ([f (let ([theta (fl/ sqrt-omega 4.0)] [pi/2 (flatan2 1.0 0.0)])
               (let ([rho (fl/ theta)] [-pi/2 (fl- pi/2)])
                  (lambda (x y)
                     ; x is positive
                     (let ([ay (abs y)])
                        (cond
                           [(or (fl> x theta) (fl> ay theta))
                            ; RP(1/z) +/- (pi/2)i
                            (fl-make-rectangular
                               (cond
                                  [(fl> x ay) (fl/ (fl+ x (fl* (fl/ y x) y)))]
                                  [(fl< x ay) (let ([r (fl/ y x)])
                                                 (fl/ r (fl+ (fl* x r) y)))]
                                  [else (fl/ (fl+ x ay))])
                               (if (negated-flonum? y) pi/2 -pi/2))]
                           [(fl= x 1.0)
                            (let ([k (fl+ ay rho)])
                               (fl-make-rectangular
                                  ($fllog (fl/ ($flsqrt ($flsqrt (fl+ 4.0
                                                                   (* y y))))
                                                      ($flsqrt k)))
                                  (fl/ (fl+ pi/2 ($flatan (fl/ k 2.0)))
                                       (if (negated-flonum? y) 2.0 -2.0))))]
                           [else
                            (let ([1-x (fl- 1.0 x)]
                                  [k (let ([k (fl+ ay rho)]) (fl* k k))])
                               (fl-make-rectangular
                                  (fl/ (fllog1+ (fl/ (fl* 4.0 x)
                                                     (fl+ (fl* 1-x 1-x) k)))
                                       4.0)
                                  (fl/ (flatan2 (fl* 2.0 y)
                                                (fl- (fl* 1-x (fl+ 1.0 x)) k))
                                       -2.0)))])))))])
      (lambda (z)
         (let ([x (cfl-real-part z)] [y (cfl-imag-part z)])
            (if (negated-flonum? x)
                (cfl- (f (fl- x) y))
                (f x (fl- y)))))))

(define cflatan
   ; from Kahan
   ; principal expression:
   ; arctanh(zi)/i
   (lambda (z)
      (cfl/i (cflatanh (cfl*i z)))))

(define exact-inexact+
   (lambda (x y)
      (cond
         [(fixnum? x) (if (fx= x 0) y (fl+ (fixnum->flonum x) y))]
         [(or (floatable? x) (fl= y 0.0)) (fl+ (inexact x) y)]
         [(exceptional-flonum? y) y]
         [else (inexact (+ x (exact y)))])))

(define exact-inexact-
   (lambda (x y)
      (cond
         [(fixnum? x) (if (fx= x 0) (fl- y) (fl- (fixnum->flonum x) y))]
         [(or (floatable? x) (fl= y 0.0)) (fl- (inexact x) y)]
         [(exceptional-flonum? y) (fl- y)]
         [else (inexact (- x (exact y)))])))

(define inexact-exact-
   (lambda (x y)
      (cond
         [(fixnum? y) (fl- x (fixnum->flonum y))]
         [(or (floatable? y) (fl= x 0.0)) (fl- x (inexact y))]
         [(exceptional-flonum? x) x]
         [else (inexact (- (exact x) y))])))

(define exact-inexact*
   (lambda (x y)
      (cond
         [(fixnum? x) (if (fx= x 0) 0 (fl* (fixnum->flonum x) y))]
         [(floatable? x) (fl* (inexact x) y)]
         [(or (fl= y 0.0) (exceptional-flonum? y)) (if (< x 0) (fl- y) y)]
         [else (inexact (* x (exact y)))])))

(define exact-inexact/
   (lambda (x y)
      (cond
         [(fixnum? x) (fl/ (fixnum->flonum x) y)]
         [(floatable? x) (fl/ (inexact x) y)]
         [(or (fl= y 0.0) (exceptional-flonum? y))
          (if (< x 0) (fl/ -1.0 y) (fl/ y))]
         [else (inexact (/ x (exact y)))])))

(define inexact-exact/
   (lambda (x y)
      (cond
         [(fixnum? y) (fl/ x (fixnum->flonum y))]
         [(floatable? y) (fl/ x (inexact y))]
         [(or (fl= x 0.0) (exceptional-flonum? x)) (if (< y 0) (fl- x) x)]
         [else (inexact (/ (exact x) y))])))

(define floatable?
   ; x is "floatable" if it can be made inexact without overflow or underflow
   (lambda (x)
      (type-case x
         [(fixnum?) #t]
         [(bignum?) (fx<= (integer-length x) (constant max-float-exponent))]
         [(ratnum?) (fx<= (constant min-float-exponent)
                          (fx- (integer-length (numerator x))
                               (integer-length (denominator x)))
                          (constant max-float-exponent))]
         [($exactnum?) (and (floatable? (real-part x))
                             (floatable? (imag-part x)))]
         [else #t])))

(define fixnum-floatable-wlop?
  ;; floatable without loss of precision
  (lambda (x)
    (if (<= (- (fixnum-width) 1) (constant float-mantissa-bits))
        #t
        (let ([hi (expt 2 (constant float-mantissa-bits))])
          (fx<= (- hi) x hi)))))

(define exact-inexact-compare?
   ; e is an exact number, i is a flonum
   ; Preserve transitivity by making i exact,
   ; unless i is +/-infinity or a NAN, in which case any normal flonum
   ; is a safe representation of e for comparative purposes.
   (lambda (pred? e i)
      (float-type-case
         [(ieee)
          (if (exceptional-flonum? i)
              (pred? 0.0 i)
              (pred? e (exact i)))]
         [else (pred? e (exact i))])))

(define exact-sqrt
   ; x must be exact
   ; returns the exact square root if it exists, otherwise an approximation
   (lambda (x)
      (type-case x
         [(fixnum? bignum?)
          (if (< x 0) (make-rectangular 0 (isqrt (abs x))) (isqrt x))]
         [(ratnum?)
          (/ (exact-sqrt (numerator x)) (exact-sqrt (denominator x)))]
         [else
          (let ([rp (exact-sqrt (/ (+ (exact-sqrt (magnitude-squared x))
                                      (real-part x))
                                   2))])
             (make-rectangular rp (/ (imag-part x) (* 2 rp))))])))

(define ($fldiv-and-mod x y)
  (if (negated-flonum? y)
      (let ([q ($flfloor (fl/ x (fl- y)))])
        (values (fl- q) (fl+ x (fl* y q))))
      (let ([q ($flfloor (fl/ x y))])
        (values q (fl- x (fl* y q))))))

(define ($fldiv x y)
  (if (negated-flonum? y)
      (fl- ($flfloor (fl/ x (fl- y))))
      ($flfloor (fl/ x y))))

(define ($flmod x y)
  (if (negated-flonum? y)
      (fl+ x (fl* y ($flfloor (fl/ x (fl- y)))))
      (fl- x (fl* y ($flfloor (fl/ x y))))))

(define ($fldiv0-and-mod0 x y)
 ; there doesn't seem to be an easy way to do this...
  (let-values ([(d m) ($fldiv-and-mod x y)])
    (if (fl> y 0.0)
        (if (fl< m (fl/ y 2.0))
            (values d m)
            (values (fl+ d 1.0) (fl- m y)))
        (if (fl< m (fl/ y -2.0))
            (values d m)
            (values (fl- d 1.0) (fl+ m y))))))

(define ($fldiv0 x y)
  (let-values ([(d m) ($fldiv-and-mod x y)])
    (if (fl> y 0.0)
        (if (fl< m (fl/ y 2.0)) d (fl+ d 1.0))
        (if (fl< m (fl/ y -2.0)) d (fl- d 1.0)))))

(define ($flmod0 x y)
  (let ([m ($flmod x y)])
    (if (fl> y 0.0)
        (if (fl< m (fl/ y 2.0)) m (fl- m y))
        (if (fl< m (fl/ y -2.0)) m (fl+ m y)))))

(define ($fxdiv-and-mod x y who) ; signal error on overflow if who != #f, otherwise return bignum
  (if (fx< x 0)
      (if (fx< y 0)
          (if (fx> x y) ; |x| < |y| => q = 0, r = x != 0
              (values 1 (fx- x y))
              (if (and (fx= y -1) (fx= x (most-negative-fixnum)))
                  (if who
                      ($impoops who "fixnum overflow with arguments ~s and ~s" x y)
                      (values (- (most-negative-fixnum)) 0))
                  (let* ([q (fxquotient x y)] [r (fx- x (fx* y q))])
                    (if (fx= r 0) (values q 0) (values (fx+ q 1) (fx- r y))))))
          (if (fx> x (fx- y)) ; |x| < |y| => q = 0, r = x != 0
              (values -1 (fx+ x y))
              (let* ([q (fxquotient x y)] [r (fx- x (fx* y q))])
                (if (fx= r 0) (values q 0) (values (fx- q 1) (fx+ r y))))))
      (if (or (fx< x y) (fx> (fx- x) y)) ; |x| < |y| => q = 0, r = x
          (values 0 x)
          (let ([q (fxquotient x y)])
            (values q (fx- x (fx* y q)))))))

(define ($fxdiv x y who) ; signal error on overflow if who != #f, otherwise return bignum
  (if (fx< x 0)
      (if (fx< y 0)
          (if (fx> x y) ; |x| < |y| => q = 0, r = x != 0
              1
              (if (and (fx= y -1) (fx= x (most-negative-fixnum)))
                  (if who
                      ($impoops who "fixnum overflow with arguments ~s and ~s" x y)
                      (- (most-negative-fixnum)))
                  (let ([q (fxquotient x y)])
                    (if (fx= x (fx* y q)) q (fx+ q 1)))))
          (if (fx> x (fx- y)) ; |x| < |y| => q = 0, r = x != 0
              -1
              (let ([q (fxquotient x y)])
                (if (fx= x (fx* y q)) q (fx- q 1)))))
      (if (or (fx< x y) (fx> (fx- x) y)) ; |x| < |y| => q = 0, r = x
          0
          (fxquotient x y))))

(define ($fxmod x y) ; no overflow possible
  (if (fx< x 0)
      (if (fx< y 0)
          (if (fx> x y) ; |x| < |y| => q = 0, r = x != 0
              (fx- x y)
              (if (and (fx= y -1) (fx= x (most-negative-fixnum)))
                  0
                  (let* ([q (fxquotient x y)] [r (fx- x (fx* y q))])
                    (if (fx= r 0) 0 (fx- r y)))))
          (if (fx> x (fx- y)) ; |x| < |y| => q = 0, r = x != 0
              (fx+ x y)
              (let* ([q (fxquotient x y)] [r (fx- x (fx* y q))])
                (if (fx= r 0) 0 (fx+ r y)))))
      (if (or (fx< x y) (fx> (fx- x) y)) ; |x| < |y| => q = 0, r = x
          x
          (fx- x (fx* y (fxquotient x y))))))

(define ($fxdiv0-and-mod0 x y who)
  (let-values ([(d m) ($fxdiv-and-mod x y who)])
    (if (fx> y 0)
        (if (fx< m (if (fx= y (most-positive-fixnum))
                       (ash (+ (most-positive-fixnum) 1) -1)
                       (fxsrl (fx+ y 1) 1)))
            (values d m)
            (values (fx+ d 1) (fx- m y)))
        (if (fx< m (if (fx= y (most-negative-fixnum))
                       (ash (- 1 (most-negative-fixnum)) -1)
                       (fxsrl (fx- 1 y) 1)))
            (values d m)
            (values (fx- d 1) (fx+ m y))))))

(define ($fxdiv0 x y who)
  (let-values ([(d m) ($fxdiv-and-mod x y who)])
    (if (fx> y 0)
        (if (fx< m (if (fx= y (most-positive-fixnum))
                       (ash (+ (most-positive-fixnum) 1) -1)
                       (fxsrl (fx+ y 1) 1)))
            d
            (fx+ d 1))
        (if (fx< m (if (fx= y (most-negative-fixnum))
                       (ash (- 1 (most-negative-fixnum)) -1)
                       (fxsrl (fx- 1 y) 1)))
            d
            (fx- d 1)))))

(define ($fxmod0 x y)
  (let ([m ($fxmod x y)])
    (if (fx> y 0)
        (if (fx< m (if (fx= y (most-positive-fixnum))
                       (ash (+ (most-positive-fixnum) 1) -1)
                       (fxsrl (fx+ y 1) 1)))
            m
            (fx- m y))
        (if (fx< m (if (fx= y (most-negative-fixnum))
                       (ash (- 1 (most-negative-fixnum)) -1)
                       (fxsrl (fx- 1 y) 1)))
            m
            (fx+ m y)))))

(define ($exdiv-and-mod x y) ; like $fldiv-and-mod
  (if (< y 0)
      (let ([q (floor (/ x (- y)))])
        (values (- q) (+ x (* y q))))
      (let ([q (floor (/ x y))])
        (values q (- x (* y q))))))

(define ($exdiv0-and-mod0 x y)
  (let-values ([(d m) ($exdiv-and-mod x y)])
    (if (> y 0)
        (if (< m (/ y 2))
            (values d m)
            (values (+ d 1) (- m y)))
        (if (< m (/ y -2))
            (values d m)
            (values (- d 1) (+ m y))))))

(define ($exdiv x y) ; like $fldiv
  (if (< y 0)
      (- (floor (/ x (- y))))
      (floor (/ x y))))

(define ($exmod x y) ; like $flmod
  (if (< y 0)
      (+ x (* y (floor (/ x (- y)))))
      (- x (* y (floor (/ x y))))))

(define $sll
  (lambda (who x n)
    (type-case n
      [(fixnum?)
       (unless (fx>= n 0) ($oops who "~s is not a nonnegative exact integer" n))
       (type-case x
         [(fixnum?)
          (let ([max-fx-shift (- (constant fixnum-bits) 1)])
            (if (fx> n max-fx-shift)
                (integer-ash x n)
                (let ([m (#3%fxsll x n)])
                  (if (fx= (fxsra m n) x)
                      m
                      (integer-ash x n)))))]
         [(bignum?) (integer-ash x n)]
         [else (nonexact-integer-error who x)])]
      [(bignum?)
       (unless ($bigpositive? n) ($oops who "~s is not a nonnegative exact integer" n))
       (type-case x
         [(fixnum? bignum?)
          (let ([k (most-positive-fixnum)])
            ($sll who ($sll who x k) (- n k)))]
         [else (nonexact-integer-error who x)])]
      [else (nonexact-integer-error who n)])))

(define $sra
  (lambda (who x n)
    (type-case n
      [(fixnum?)
       (unless (fx>= n 0) ($oops who "~s is not a nonnegative exact integer" n))
       (type-case x
         [(fixnum?)
          (let ([max-fx-shift (- (constant fixnum-bits) 1)])
            (fxsra x (if (fx> n max-fx-shift) max-fx-shift n)))]
         [(bignum?) (integer-ash x (- n))]
         [else (nonexact-integer-error who x)])]
      [(bignum?)
       (unless ($bigpositive? n) ($oops who "~s is not a nonnegative exact integer" n))
       (type-case x
         [(fixnum? bignum?)
          (let ([k (most-positive-fixnum)])
            ($sra who ($sra who x k) (- n k)))]
         [else (nonexact-integer-error who x)])]
      [else (nonexact-integer-error who n)])))

(define $negate
  (lambda (who x)
    (type-case x
      [(fixnum?)
       (if (fx= x (most-negative-fixnum))
           (let-syntax ([a (lambda (x) (- (constant most-negative-fixnum)))]) a)
           (fx- x))]
      [(bignum?) (big-negate x)]
      [(flonum?) (fl- x)]
      [(ratnum?) (integer/ (- ($ratio-numerator x)) ($ratio-denominator x))]
      [($exactnum? $inexactnum?) (make-rectangular (- (real-part x)) (- (imag-part x)))]
      [else (nonnumber-error who x)])))

(set! integer?
  (lambda (x)
    (type-case x
      [(fixnum? bignum?) #t]
      [(flonum?) ($flinteger? x)]
      [else #f])))

(set! integer-valued?
  (lambda (x)
    (type-case x
      [(fixnum? bignum?) #t]
      [(flonum?) ($flinteger? x)]
      [($inexactnum?)
       (and (fl= ($inexactnum-imag-part x) 0.0)
            ($flinteger? ($inexactnum-real-part x)))]
      [else #f])))

(set! rational?
  (lambda (x)
    (type-case x
      [(fixnum? bignum? ratnum?) #t]
      [(flonum?) (not (exceptional-flonum? x))]
      [else #f])))

(set! rational-valued?
  (lambda (x)
    (type-case x
      [(fixnum? bignum? ratnum?) #t]
      [(flonum?) (not (exceptional-flonum? x))]
      [($inexactnum?)
       (and (fl= ($inexactnum-imag-part x) 0.0)
            (not (exceptional-flonum? ($inexactnum-real-part x))))]
      [else #f])))

(set! real?
  (lambda (x)
    (type-case x
      [(fixnum? flonum? bignum? ratnum?) #t]
      [else #f])))

(set! real-valued?
  (lambda (x)
    (type-case x
      [(fixnum? flonum? bignum? ratnum?) #t]
      [($inexactnum?) (fl= ($inexactnum-imag-part x) 0.0)]
      [else #f])))

(set! complex?
 ; same as number?
  (lambda (x)
    (type-case x
      [(fixnum? cflonum? bignum? ratnum? $exactnum?) #t]
      [else #f])))

(set! number?
 ; same as complex?
  (lambda (x)
    (type-case x
      [(fixnum? cflonum? bignum? ratnum? $exactnum?) #t]
      [else #f])))

(set! exact?
   (lambda (x)
      (type-case x
         [(fixnum?) #t]
         [(cflonum?) #f]
         [(bignum? ratnum? $exactnum?) #t]
         [else (nonnumber-error 'exact? x)])))

(set! inexact?
   (lambda (x)
      (type-case x
         [(cflonum?) #t]
         [(fixnum? bignum? ratnum? $exactnum?) #f]
         [else (nonnumber-error 'inexact? x)])))

(set-who! numerator
  (lambda (x)
    (type-case x
      [(ratnum?) ($ratio-numerator x)]
      [(fixnum? bignum?) x]
      [(flonum?)
       (cond
         [(exceptional-flonum? x) (nonrational-error who x)]
         [($flinteger-or-inf? x) x]
         [else (inexact (numerator (exact x)))])]
      [else (nonrational-error who x)])))

(set-who! denominator
  (lambda (x)
    (type-case x
      [(ratnum?) ($ratio-denominator x)]
      [(fixnum? bignum?) 1]
      [(flonum?)
       (cond
         [(exceptional-flonum? x) (nonrational-error who x)]
         [($flinteger-or-inf? x) 1.0]
         [else (inexact (denominator (exact x)))])]
      [else (nonrational-error who x)])))

(set! real-part
   (lambda (z)
      (type-case z
         [($inexactnum?) ($inexactnum-real-part z)]
         [($exactnum?) ($exactnum-real-part z)]
         [(flonum? fixnum? bignum? ratnum?) z]
         [else (noncomplex-error 'real-part z)])))

(set! imag-part
   (lambda (z)
      (type-case z
         [($inexactnum?) ($inexactnum-imag-part z)]
         [($exactnum?) ($exactnum-imag-part z)]
         [(flonum?) 0]
         [(fixnum? bignum? ratnum?) 0]
         [else (noncomplex-error 'imag-part z)])))

(set! modulo
   (lambda (x y)
      (unless (integer? x) (noninteger-error 'modulo x))
      (unless (integer? y) (noninteger-error 'modulo y))
      (when (= y 0) (domain-error 'modulo y))
      (let ([r (remainder x y)])
         (if (if (negative? y) (positive? r) (negative? r))
             (+ r y)
             r))))

(set! expt-mod
   ; (modulo (expt x y) z)
   (lambda (x y z)
      (unless (integer? x)
         ($oops 'expt-mod "~s is not an integer" x))
      (unless (and (integer? y) (not (negative? y)))
         ($oops 'expt-mod "~s is not a nonnegative integer" y))
      (unless (and (integer? z) (not (zero? z)))
         ($oops 'expt-mod "~s is not a nonzero integer" z))
      (if (= y 0)
          (modulo 1 z)
          (do ([w 1 (if (even? y) w (remainder (* w b) z))]
               [y y (quotient y 2)]
               [b (remainder x z) (remainder (* b b) z)])
              ((= y 1) (modulo (* w b) z))))))

(set-who! negative?
  (lambda (x)
    (type-case x
      [(fixnum?) (fx< x 0)]
      [(flonum?) (fl< x 0.0)]
      [(bignum?) (not ($bigpositive? x))]
      [(ratnum?) (< ($ratio-numerator x) 0)]
      [else (nonreal-error who x)])))

(set-who! nonnegative?
  (lambda (x)
    (type-case x
      [(fixnum?) (fx>= x 0)]
      [(flonum?) (fl>= x 0.0)]
      [(bignum?) ($bigpositive? x)]
      [(ratnum?) (>= ($ratio-numerator x) 0)]
      [else (nonreal-error who x)])))

(set-who! positive?
  (lambda (x)
    (type-case x
      [(fixnum?) (fx> x 0)]
      [(flonum?) (fl> x 0.0)]
      [(bignum?) ($bigpositive? x)]
      [(ratnum?) (> ($ratio-numerator x) 0)]
      [else (nonreal-error who x)])))

(set-who! nonpositive?
  (lambda (x)
    (type-case x
      [(fixnum?) (fx<= x 0)]
      [(flonum?) (fl<= x 0.0)]
      [(bignum?) (not ($bigpositive? x))]
      [(ratnum?) (<= ($ratio-numerator x) 0)]
      [else (nonreal-error who x)])))

(set-who! min
  (rec min
    (case-lambda
      [(x y)
       (type-case x
          [(flonum?)
           (type-case y
              [(flonum?) (if (or (fl< x y) ($nan? x)) x y)]
              [(fixnum? bignum? ratnum?) (min x (inexact y))]
              [else (nonreal-error who y)])]
          [(fixnum?)
           (type-case y
              [(fixnum?) (if (fx< x y) x y)]
              [(bignum? ratnum?) (if (< x y) x y)]
              [(flonum?) (min (inexact x) y)]
              [else (nonreal-error who y)])]
          [(bignum? ratnum?)
           (type-case y
              [(fixnum? bignum? ratnum?) (if (< x y) x y)]
              [(flonum?) (min (inexact x) y)]
              [else (nonreal-error who y)])]
          [else (nonreal-error who x)])]
      [(x) (if (real? x) x (nonreal-error who x))]
      [(x y . z)
       (let loop ([x (min x y)] [z z])
          (if (null? z)
              x
              (loop (min x (car z)) (cdr z))))])))

(set-who! max
  (rec max
    (case-lambda
      [(x y)
       (type-case x
          [(flonum?)
           (type-case y
              [(flonum?) (if (or (fl> x y) ($nan? x)) x y)]
              [(fixnum? bignum? ratnum?) (max x (inexact y))]
              [else (nonreal-error who y)])]
          [(fixnum?)
           (type-case y
              [(fixnum?) (if (fx> x y) x y)]
              [(bignum? ratnum?) (if (> x y) x y)]
              [(flonum?) (max (inexact x) y)]
              [else (nonreal-error who y)])]
          [(bignum? ratnum?)
           (type-case y
              [(fixnum? bignum? ratnum?) (if (> x y) x y)]
              [(flonum?) (max (inexact x) y)]
              [else (nonreal-error who y)])]
          [else (nonreal-error who x)])]
      [(x) (if (real? x) x (nonreal-error who x))]
      [(x y . z)
       (let loop ([x (max x y)] [z z])
          (if (null? z)
              x
              (loop (max x (car z)) (cdr z))))])))

(let ()
  (define exgcd
    (foreign-procedure "(cs)gcd"
      (scheme-object scheme-object)
      scheme-object))

  (define (exlcm x1 x2)
    (if (or (eqv? x1 0) (eqv? x2 0))
        0
        (* (abs x1) (/ (abs x2) (exgcd x1 x2)))))

  (set-who! gcd
    (rec gcd
      (case-lambda
        [() 0]
        [(x1) (gcd x1 x1)]
        [(x1 x2)
         (if (and (or (fixnum? x1) (bignum? x1))
                  (or (fixnum? x2) (bignum? x2)))
             (exgcd x1 x2)
             (begin
               (unless (integer? x1) (noninteger-error who x1))
               (unless (integer? x2) (noninteger-error who x2))
               (inexact (exgcd (exact x1) (exact x2)))))]
        [(x1 x2 . xr)
         (let f ([x1 x1] [x2 x2] [xr xr])
           (let ([x1 (gcd x1 x2)])
             (if (null? xr) x1 (f x1 (car xr) (cdr xr)))))])))

  (set-who! lcm
    (rec lcm
      (case-lambda
        [() 1]
        [(x) (lcm x x)]
        [(x1 x2)
         (if (and (or (fixnum? x1) (bignum? x1))
                  (or (fixnum? x2) (bignum? x2)))
             (exlcm x1 x2)
             (begin
               (unless (integer? x1) (noninteger-error who x1))
               (unless (integer? x2) (noninteger-error who x2))
               (inexact (exlcm (exact x1) (exact x2)))))]
        [(x1 x2 . xr)
         (let f ([x1 x1] [x2 x2] [xr xr])
           (let ([x1 (lcm x1 x2)])
             (if (null? xr) x1 (f x1 (car xr) (cdr xr)))))]))))

(let ()
  (define convert-to-inexact
    (lambda (z who)
      (type-case z
        [(fixnum?) (fixnum->flonum z)]
        [(bignum? ratnum?) (float z)]
        [($exactnum?)
         (fl-make-rectangular (inexact ($exactnum-real-part z))
                              (inexact ($exactnum-imag-part z)))]
        [(cflonum?) z]
        [else (nonnumber-error who z)])))
  (set-who! inexact (lambda (z) (convert-to-inexact z who)))
  (set-who! exact->inexact (lambda (z) (convert-to-inexact z who))))

(let ()
  (define convert-to-exact
    (lambda (z who)
      (type-case z
        [(flonum?)
         (when (exceptional-flonum? z)
           ($oops 'exact "no exact representation for ~s" z))
         (let ([dx (decode-float z)])
           (let ([mantissa (* (vector-ref dx 0) (vector-ref dx 2))]
                 [exponent (vector-ref dx 1)])
             (if (fx< exponent 0)
                 (/ mantissa (ash 1 (fx- exponent)))
                 (* mantissa (ash 1 exponent)))))]
        [($inexactnum?)
         (make-rectangular
           (exact ($inexactnum-real-part z))
           (exact ($inexactnum-imag-part z)))]
        [(fixnum? bignum? ratnum? $exactnum?) z]
        [else (nonnumber-error who z)])))
  (set-who! exact (lambda (z) (convert-to-exact z who)))
  (set-who! inexact->exact (lambda (z) (convert-to-exact z who))))

(set! rationalize
   ; Alan Bawden's algorithm
   (letrec
      ([rat1 ; x < y
        (lambda (x y)
           (cond
              [(> x 0) (rat2 x y)]
              [(< y 0) (- (rat2 (- y) (- x)))]
              [else (if (and (exact? x) (exact? y)) 0 0.0)]))]
       [rat2 ; 0 < x < y
        (lambda (x y)
           (let ([fx (floor x)] [fy (floor y)])
              (cond
                 [(= fx x) fx]
                 [(= fx fy) (+ fx (/ (rat2 (/ (- y fy)) (/ (- x fx)))))]
                 [else (+ fx 1)])))])
      (lambda (x e)
         (unless (real? x) (nonreal-error 'rationalize x))
         (unless (real? e) (nonreal-error 'rationalize e))
         (let ([x (- x e)] [y (+ x e)])
           (cond
              [(< x y) (rat1 x y)]
              [(< y x) (rat1 y x)]
              [else x])))))

(set! abs
   (lambda (z)
      (type-case z
         [(fixnum?) (if (fx< z 0) (if (fx= z (most-negative-fixnum)) (- (most-negative-fixnum)) (fx- z)) z)]
         [(flonum?) (flabs z)]
         [(bignum?) (if ($bigpositive? z) z (- z))]
         [(ratnum?) (if (< z 0) (- z) z)]
         [else (nonreal-error 'abs z)])))

(set! magnitude
   (lambda (z)
      (type-case z
         [(flonum?) (flabs z)]
         [(fixnum?) (if (fx< z 0) (- z) z)]
         [($inexactnum?) (cflmagnitude z)]
         [($exactnum?)
          (let ([x ($exactnum-real-part z)] [y ($exactnum-imag-part z)])
             (sqrt (+ (* x x) (* y y))))]
         [(bignum?) (if ($bigpositive? z) z (- z))]
         [(ratnum?) (if (< z 0) (- z) z)]
         [else (noncomplex-error 'magnitude z)])))

(set! angle
   (lambda (z)
      (type-case z
         [(flonum?) (if (negated-flonum? z) pi 0.0)]
         [($inexactnum?) (cflangle z)]
         [(fixnum? bignum? ratnum?)
          (cond
             [(< z 0) pi]
             [(> z 0) 0]
             [else (domain-error 'angle z)])]
         [($exactnum?)
          ; use single argument atan to avoid precision loss
          ; cases from Kahan
          (let ([x ($exactnum-real-part z)] [y ($exactnum-imag-part z)])
             (cond
                [(> (abs y) (abs x))
                 (- (fl* pi (if (< y 0) -.5 .5)) (atan (/ x y)))]
                [(< x 0)
                 (if (< y 0)
                     (- (atan (/ y x)) pi)
                     (+ (atan (/ y x)) pi))]
                [else (atan (/ y x))]))]
         [else (noncomplex-error 'angle z)])))

(set-who! make-rectangular
  (lambda (x y)
    (type-case y
      [(flonum?)
       (fl-make-rectangular
          (type-case x
            [(flonum?) x]
            [(fixnum? bignum? ratnum?) (inexact x)]
            [else (nonreal-error who x)])
          y)]
      [(fixnum? bignum? ratnum?)
       (if (eq? y 0)
           (if (real? x) x (nonreal-error who x))
           (type-case x
             [(fixnum? bignum? ratnum?) ($make-exactnum x y)]
             [(flonum?) (fl-make-rectangular x (inexact y))]
             [else (nonreal-error who x)]))]
      [else (nonreal-error who y)])))

(set-who! make-polar
  (lambda (x y)
    (unless (real? x) (nonreal-error 'make-polar x))
    (unless (real? y) (nonreal-error 'make-polar y))
    (cond
      [(eq? y 0) x]
      [(eq? x 0) 0]
      [else (make-rectangular (* x (cos y)) (* x (sin y)))])))

(set! log
  (rec log
    (case-lambda
      [(x)
       (type-case x
         [(flonum?)
          (if (negated-flonum? x)
              (fl-make-rectangular ($fllog (fl- x)) pi)
              ($fllog x))]
         [($inexactnum?) (cfllog x)]
         [(fixnum?)
          (cond
             [(fx> x 1) ($fllog (fixnum->flonum x))]
             [(fx= x 1) 0]
             [(fx< x 0) (make-rectangular (log (- x)) pi)]
             [else (domain-error 'log x)])]
         [(bignum?)
          (let ([len (integer-length x)])
             (if (fx<= len (constant max-float-exponent))
                 (log (inexact x))
                 (+ (* len log2) (log (inexact (/ x (ash 1 len)))))))]
         [(ratnum?)
          (if (floatable? x)
              (log (inexact x))
              (- (log (numerator x)) (log (denominator x))))]
         [($exactnum?)
          (make-rectangular
            (/ (log (magnitude-squared x)) 2)
            (angle x))]
         [else (nonnumber-error 'log x)])]
      [(x y) (/ (log x) (log y))])))

(define-trig-op exp $flexp cflexp 1)
(define-trig-op sin $flsin cflsin 0)
(define-trig-op cos $flcos cflcos 1)
(define-trig-op tan $fltan cfltan 0)

(set! asin
   (lambda (x)
      (type-case x
         [(flonum?)
          ; make sure NANs go the "$flasin" route
          (if (or (fl< x -1.0) (fl> x 1.0))
              (cflasin x)
              ($flasin x))]
         [($inexactnum?) (cflasin x)]
         [(fixnum?) (if (fx= x 0) 0 (asin (fixnum->flonum x)))]
         [(bignum? ratnum? $exactnum?) (asin (inexact x))]
         [else (nonnumber-error 'asin x)])))

(set! acos
   (lambda (x)
      (type-case x
         [(flonum?)
          ; make sure NANs go the "$flacos" route
          (if (or (fl< x -1.0) (fl> x 1.0))
              (cflacos x)
              ($flacos x))]
         [($inexactnum?) (cflacos x)]
         [(fixnum?) (if (fx= x 1) 0 (acos (fixnum->flonum x)))]
         [(bignum? ratnum? $exactnum?) (acos (inexact x))]
         [else (nonnumber-error 'acos x)])))

(set! atan
   (case-lambda
      [(x)
       (type-case x
          [(flonum?) ($flatan x)]
          [($inexactnum?) (cflatan x)]
          [(fixnum?) (if (fx= x 0) 0 (atan (fixnum->flonum x)))]
          [(bignum? ratnum?) (atan (inexact x))]
          [($exactnum?)
           (when (or (= x +i) (= x -i)) (domain-error 'atan x))
           (atan (inexact x))]
          [else (nonnumber-error 'atan x)])]
      [(y x)
       (cond
          [(and (flonum? y) (flonum? x))
           (flatan2 y x)]
          [(and (fixnum? y) (fixnum? x))
           (if (fx= y 0)
               (cond
                  [(fx> x 0) 0]
                  [(fx< x 0) pi]
                  [else (domain-error2 'atan2 y x)])
               (flatan2 (fixnum->flonum y) (fixnum->flonum x)))]
          [else
           (unless (real? y) (nonreal-error 'atan y))
           (unless (real? x) (nonreal-error 'atan x))
           (angle (make-rectangular x y))])]))

(define-trig-op sinh flsinh cflsinh 0)
(define-trig-op cosh flcosh cflcosh 1)
(define-trig-op tanh fltanh cfltanh 0)
(define-trig-op asinh flasinh cflasinh 0)

(set! acosh
   (lambda (x)
      (type-case x
         [(flonum?)
          ; make sure NANs go the "flacosh" route
          (if (fl< x 1.0) (cflacosh x) (flacosh x))]
         [($inexactnum?) (cflacosh x)]
         [(fixnum?) (if (fx= x 1) 0 (acosh (fixnum->flonum x)))]
         [(bignum? ratnum? $exactnum?) (acosh (inexact x))]
         [else (nonnumber-error 'acosh x)])))

(set! atanh
   (lambda (x)
      (type-case x
         [(flonum?)
          ; make sure NANs go the "flatanh" route
          (if (or (fl< x -1.0) (fl> x 1.0))
              (cflatanh x)
              (flatanh x))]
         [($inexactnum?) (cflatanh x)]
         [(fixnum?)
          (cond
             [(or (fx> x 1) (fx< x -1)) (atanh (fixnum->flonum x))]
             [(fx= x 0) 0]
             [else (domain-error 'atan x)])]
         [(bignum? ratnum? $exactnum?) (atanh (inexact x))]
         [else (nonnumber-error 'atanh x)])))

; exceptional cases from Steele(CLtL), page 311
(set! expt
   (lambda (x y)
      (type-case y
         [(fixnum? bignum?)
          (cond
            [(and (eq? y 0) (number? x)) 1]
            [(eq? x 0)
             (if (< y 0)
                 ($impoops 'expt "undefined for values ~s and ~s" x y)
                 0)]
            [(eq? x 1) 1]
            [(eq? x 2) (if (< y 0) (/ (ash 1 (- y))) (ash 1 y))]
            [(and (flonum? x) (floatable? y)) ($flexpt x (inexact y))]
            [(and ($inexactnum? x) (floatable? y)) (exp (* y (log x)))]
            [(not (number? x)) (nonnumber-error 'expt x)]
            [(ratnum? x)
             (if (< y 0)
                 (let ([y (- y)])
                   (/ (expt (denominator x) y) (expt (numerator x) y)))
                 (/ (expt (numerator x) y) (expt (denominator x) y)))]
            [else
             (let ()
               (define (f x n)
                 (if (eq? n 1)
                     x
                     (let ([s (f x (ash n -1))])
                        (if (even? n) (* s s) (* (* s s) x)))))
               (if (< y 0)
                   (if (or (fixnum? x) (bignum? x))
                       (/ (f x (- y)))
                       (f (/ x) (- y)))
                   (f x y)))])]
         [(flonum?)
          (type-case x
             [(flonum?)
              (if (and (fl< x 0.0) (not ($flinteger-or-inf? y)))
                  (exp (* y (log x)))
                  ($flexpt x y))]
             [($inexactnum? $exactnum?) (exp (* y (log x)))]
             [(fixnum? bignum? ratnum?)
              (if (floatable? x)
                  (expt (inexact x) y)
                  (exp (* y (log x))))]
             [else (nonnumber-error 'expt x)])]
         [($inexactnum?)
          (if (or (eq? x 0) (and (flonum? x) (= x 0.0)))
              0.0 
              (begin
                (unless (number? x) (nonnumber-error 'expt x))
                (exp (* y (log x)))))]
         [(ratnum? $exactnum?)
          (unless (number? x) (nonnumber-error 'expt x))
          (cond
             [(eq? x 0)
              (if (> (real-part y) 0)
                  0
                  ($impoops 'expt "undefined for values ~s and ~s" x y))]
             [(floatable? y) (expt x (inexact y))]
             [else (exp (* y (log x)))])]
         [else (nonnumber-error 'expt y)])))

(set! sqrt
   (lambda (x)
      (type-case x
         [(flonum?)
          (if (and (negated-flonum? x) (not ($nan? x)))
              (fl-make-rectangular 0.0 ($flsqrt (flabs x)))
              ($flsqrt x))]
         [($inexactnum?) (cflsqrt x)]
         [(fixnum? bignum? ratnum? $exactnum?)
          (let ([y (exact-sqrt x)])
             (let ([yy (* y y)])
                (cond
                   [(= yy x) y]
                   [(floatable? x) (sqrt (inexact x))]
                   [else (* y (sqrt (inexact (/ x yy))))])))]
         [else (nonnumber-error 'sqrt x)])))

(set! isqrt
   ; Based on code credited to "boyland@aspen.Berkeley.EDU" by
   ; Akira Kurihara (d34676@tansei.cc.u-tokyo.ac.jp)
   (lambda (n)
      (cond
         [(and (or (fixnum? n) (bignum? n)) (>= n 0))
          (let isqrt ([n n])
             (cond
                [(>= n 16) ; ensures k > 0
                 (let ([a1 (let ([k (ash (- (integer-length n) 1) -2)])
                              (ash (isqrt (ash n (- (ash k 1)))) k))])
                    (let ([q&r ($quotient-remainder n a1)])
                       (let ([a2 (car q&r)])
                          (let ([a3 (ash (+ a1 a2) -1)])
                             (if (odd? a2)
                                 a3
                                 (let ([d (- a3 a1)])
                                    (if (> (* d d) (cdr q&r))
                                        (- a3 1)
                                        a3)))))))]
                [(>= n  9) 3]
                [(>= n  4) 2]
                [(>= n  1) 1]
                [else 0]))]
         [(and (integer? n) (>= n 0)) (floor (sqrt n))]
         [else ($oops 'isqrt "~s is not a nonnegative integer" n)])))

(set-who! floor
  (lambda (x)
    (type-case x
      [(fixnum? bignum?) x]
      [(flonum?) ($flfloor x)]
      [(ratnum?)
       (let ([y (quotient ($ratio-numerator x) ($ratio-denominator x))])
         (if (< x 0) (- y 1) y))]
      [else (nonreal-error who x)])))

(set-who! ceiling
  (lambda (x)
    (type-case x
      [(fixnum? bignum?) x]
      [(flonum?) ($flceiling x)]
      [(ratnum?)
       (let ([y (quotient ($ratio-numerator x) ($ratio-denominator x))])
         (if (< x 0) y (+ y 1)))]
      [else (nonreal-error who x)])))

(set-who! truncate
  (lambda (x)
    (type-case x
       [(fixnum? bignum?) x]
       [(flonum?) (if (negated-flonum? x) (fl- ($flfloor (flabs x))) ($flfloor x))]
       [(ratnum?) (quotient ($ratio-numerator x) ($ratio-denominator x))]
       [else (nonreal-error who x)])))

(set-who! quotient
  (let ([f (lambda (x y) (truncate (/ x y)))])
    (lambda (x y)
      (type-case y
        [(fixnum?)
         (when (fx= y 0) (domain-error who y))
         (cond
           [(fx= y 1) (unless (integer? x) (noninteger-error who x)) x]
           [(fx= y -1) (unless (integer? x) (noninteger-error who x)) ($negate who x)]
           [else
             (type-case x
               [(fixnum?) (if (and (fx= y -1) (fx= x (most-negative-fixnum)))
                              (- (most-negative-fixnum))
                              (fxquotient x y))]
               [(bignum?) (intquotient x y)]
               [else
                 (unless (integer? x) (noninteger-error who x))
                 (f x y)])])]
        [(bignum?)
         (type-case x
           [(fixnum? bignum?) (intquotient x y)]
           [else
             (unless (integer? x) (noninteger-error who x))
             (f x y)])]
        [else
          (unless (integer? y) (noninteger-error who y))
          (unless (integer? x) (noninteger-error who x))
          (when (= y 0) (domain-error who y))
          (f x y)]))))

(set-who! div-and-mod
  (lambda (x y)
    (type-case y
      [(fixnum?)
       (type-case x
         [(fixnum?)
          (when (fx= y 0) (domain-error who y))
          ($fxdiv-and-mod x y #f)]
         [(flonum?) ($fldiv-and-mod x (fixnum->flonum y))]
         [(bignum?)
          (cond
            [(fx= y 1) (values x 0)]
            [(fx= y -1) (values (big-negate x) 0)]
            [else
              (when (fx= y 0) (domain-error who y))
              (let ([q.r (intquotient-remainder x y)])
                (if ($bigpositive? x)
                    (values (car q.r) (cdr q.r))
                    (if (eq? (cdr q.r) 0)
                        (values (car q.r) 0)
                        (if (fx< y 0)
                            (values (+ (car q.r) 1) (fx- (cdr q.r) y))
                            (values (- (car q.r) 1) (fx+ (cdr q.r) y))))))])]
         [(ratnum?)
          (when (fx= y 0) (domain-error who y))
          ($exdiv-and-mod x y)]
         [else (domain-error who x)])]
      [(flonum?)
       (type-case x
         [(fixnum?) ($fldiv-and-mod (fixnum->flonum x) y)]
         [(flonum?) ($fldiv-and-mod x y)]
         [(bignum? ratnum?) ($fldiv-and-mod (real->flonum x) y)]
         [else (domain-error who x)])]
      [(bignum?)
       (type-case x
         [(fixnum?) ; know |x| < |y| => q = 0, r = x
          (if (fx< x 0)
              (if ($bigpositive? y) (values -1 (+ x y)) (values 1 (- x y)))
              (values 0 x))]
         [(flonum?) ($fldiv-and-mod x (real->flonum y))]
         [(bignum?)
          (let ([q.r (intquotient-remainder x y)])
            (if ($bigpositive? x)
                (values (car q.r) (cdr q.r))
                (if (eq? (cdr q.r) 0)
                    (values (car q.r) 0)
                    (if ($bigpositive? y)
                        (values (- (car q.r) 1) (+ (cdr q.r) y))
                        (values (+ (car q.r) 1) (- (cdr q.r) y))))))]
         [(ratnum?) ($exdiv-and-mod x y)]
         [else (domain-error who x)])]
      [(ratnum?)
       (type-case x
         [(fixnum? bignum? ratnum?) ($exdiv-and-mod x y)]
         [(flonum?) ($fldiv-and-mod x (real->flonum y))]
         [else (domain-error who x)])]
      [else (domain-error who y)])))

(set-who! div
  (lambda (x y)
    (type-case y
      [(fixnum?)
       (type-case x
         [(fixnum?)
          (when (fx= y 0) (domain-error who y))
          ($fxdiv x y #f)]
         [(flonum?) ($fldiv x (fixnum->flonum y))]
         [(bignum?)
          (when (fx= y 0) (domain-error who y))
          (cond
            [(fx= y 1) x]
            [(fx= y -1) (big-negate x)]
            [else
             (if ($bigpositive? x)
                 (intquotient x y)
                 (let ([q.r (intquotient-remainder x y)])
                   (if (eq? (cdr q.r) 0)
                       (car q.r)
                       (if (fx< y 0)
                           (+ (car q.r) 1)
                           (- (car q.r) 1)))))])]
         [(ratnum?)
          (when (fx= y 0) (domain-error who y))
          ($exdiv x y)]
         [else (domain-error who x)])]
      [(flonum?)
       (type-case x
         [(fixnum?) ($fldiv (fixnum->flonum x) y)]
         [(flonum?) ($fldiv x y)]
         [(bignum? ratnum?) ($fldiv (real->flonum x) y)]
         [else (domain-error who x)])]
      [(bignum?)
       (type-case x
         [(fixnum?) ; know |x| < |y| => q = 0, r = x
          (if (fx< x 0) (if ($bigpositive? y) -1 1) 0)]
         [(flonum?) ($fldiv x (real->flonum y))]
         [(bignum?)
          (if ($bigpositive? x)
              (intquotient x y)
              (let ([q.r (intquotient-remainder x y)])
                (if (eq? (cdr q.r) 0)
                    (car q.r)
                    (if ($bigpositive? y)
                        (- (car q.r) 1)
                        (+ (car q.r) 1)))))]
         [(ratnum?) ($exdiv x y)]
         [else (domain-error who x)])]
      [(ratnum?)
       (type-case x
         [(fixnum? bignum? ratnum?) ($exdiv x y)]
         [(flonum?) ($fldiv x (real->flonum y))]
         [else (domain-error who x)])]
      [else (domain-error who y)])))

(set-who! mod
  (lambda (x y)
    (type-case y
      [(fixnum?)
       (type-case x
         [(fixnum?)
          (when (fx= y 0) (domain-error who y))
          ($fxmod x y)]
         [(flonum?) ($flmod x (fixnum->flonum y))]
         [(bignum?)
          (when (fx= y 0) (domain-error who y))
          (cond
            [(or (fx= y 1) (fx= y -1)) 0]
            [else
             (if ($bigpositive? x)
                 (intremainder x y)
                 (let ([q.r (intquotient-remainder x y)])
                   (if (eq? (cdr q.r) 0)
                       0
                       (if (fx< y 0)
                           (fx- (cdr q.r) y)
                           (fx+ (cdr q.r) y)))))])]
         [(ratnum?)
          (when (fx= y 0) (domain-error who y))
          ($exmod x y)]
         [else (domain-error who x)])]
      [(flonum?)
       (type-case x
         [(fixnum?) ($flmod (fixnum->flonum x) y)]
         [(flonum?) ($flmod x y)]
         [(bignum? ratnum?) ($flmod (real->flonum x) y)]
         [else (domain-error who x)])]
      [(bignum?)
       (type-case x
         [(fixnum?) ; know |x| < |y| => q = 0, r = x
          (if (fx< x 0) (if ($bigpositive? y) (+ x y) (- x y)) x)]
         [(flonum?) ($flmod x (real->flonum y))]
         [(bignum?)
          (if ($bigpositive? x)
              (intremainder x y)
              (let ([q.r (intquotient-remainder x y)])
                (if (eq? (cdr q.r) 0)
                    0
                    (if ($bigpositive? y)
                        (+ (cdr q.r) y)
                        (- (cdr q.r) y)))))]
         [(ratnum?) ($exmod x y)]
         [else (domain-error who x)])]
      [(ratnum?)
       (type-case x
         [(fixnum? bignum? ratnum?) ($exmod x y)]
         [(flonum?) ($flmod x (real->flonum y))]
         [else (domain-error who x)])]
      [else (domain-error who y)])))

(set-who! div0-and-mod0
  (lambda (x y)
    (type-case y
      [(fixnum?)
       (type-case x
         [(fixnum?)
          (when (fx= y 0) (domain-error who y))
          ($fxdiv0-and-mod0 x y #f)]
         [(flonum?) ($fldiv0-and-mod0 x (fixnum->flonum y))]
         [(bignum?)
          (cond
            [(fx= y 1) (values x 0)]
            [(fx= y -1) (values (big-negate x) 0)]
            [else
             (when (fx= y 0) (domain-error who y))
             ($exdiv0-and-mod0 x y)])]
         [(ratnum?)
          (when (fx= y 0) (domain-error who y))
          ($exdiv0-and-mod0 x y)]
         [else (domain-error who x)])]
      [(flonum?)
       (type-case x
         [(fixnum?) ($fldiv0-and-mod0 (fixnum->flonum x) y)]
         [(flonum?) ($fldiv0-and-mod0 x y)]
         [(bignum? ratnum?) ($fldiv0-and-mod0 (real->flonum x) y)]
         [else (domain-error who x)])]
      [(bignum?)
       (type-case x
         [(fixnum? bignum? ratnum?) ($exdiv0-and-mod0 x y)]
         [(flonum?) ($fldiv0-and-mod0 x (real->flonum y))]
         [else (domain-error who x)])]
      [(ratnum?)
       (type-case x
         [(fixnum? bignum? ratnum?) ($exdiv0-and-mod0 x y)]
         [(flonum?) ($fldiv0-and-mod0 x (real->flonum y))]
         [else (domain-error who x)])]
      [else (domain-error who y)])))

(set-who! div0
  (lambda (x y)
    (define (exdiv0 x y)
      (let-values ([(d m) ($exdiv-and-mod x y)])
        (if (> y 0)
            (if (< m (/ y 2)) d (+ d 1))
            (if (< m (/ y -2)) d (- d 1)))))
    (type-case y
      [(fixnum?)
       (type-case x
         [(fixnum?)
          (when (fx= y 0) (domain-error who y))
          ($fxdiv0 x y #f)]
         [(flonum?) ($fldiv0 x (fixnum->flonum y))]
         [(bignum?)
          (cond
            [(fx= y 1) x]
            [(fx= y -1) (big-negate x)]
            [else
             (when (fx= y 0) (domain-error who y))
             (exdiv0 x y)])]
         [(ratnum?)
          (when (fx= y 0) (domain-error who y))
          (exdiv0 x y)]
         [else (domain-error who x)])]
      [(flonum?)
       (type-case x
         [(fixnum?) ($fldiv0 (fixnum->flonum x) y)]
         [(flonum?) ($fldiv0 x y)]
         [(bignum? ratnum?) ($fldiv0 (real->flonum x) y)]
         [else (domain-error who x)])]
      [(bignum?)
       (type-case x
         [(fixnum? bignum? ratnum?) (exdiv0 x y)]
         [(flonum?) ($fldiv0 x (real->flonum y))]
         [else (domain-error who x)])]
      [(ratnum?)
       (type-case x
         [(fixnum? bignum? ratnum?) (exdiv0 x y)]
         [(flonum?) ($fldiv0 x (real->flonum y))]
         [else (domain-error who x)])]
      [else (domain-error who y)])))

(set-who! mod0
  (lambda (x y)
    (define (exmod0 x y)
      (let ([m ($exmod x y)])
        (if (> y 0)
            (if (< m (/ y 2)) m (- m y))
            (if (< m (/ y -2)) m (+ m y)))))
    (type-case y
      [(fixnum?)
       (type-case x
         [(fixnum?)
          (when (fx= y 0) (domain-error who y))
          ($fxmod0 x y)]
         [(flonum?) ($flmod0 x (fixnum->flonum y))]
         [(bignum?)
          (cond
            [(or (fx= y 1) (fx= y -1)) 0]
            [else
             (when (fx= y 0) (domain-error who y))
             (exmod0 x y)])]
         [(ratnum?)
          (when (fx= y 0) (domain-error who y))
          (exmod0 x y)]
         [else (domain-error who x)])]
      [(flonum?)
       (type-case x
         [(fixnum?) ($flmod0 (fixnum->flonum x) y)]
         [(flonum?) ($flmod0 x y)]
         [(bignum? ratnum?) ($flmod0 (real->flonum x) y)]
         [else (domain-error who x)])]
      [(bignum?)
       (type-case x
         [(fixnum? bignum? ratnum?) (exmod0 x y)]
         [(flonum?) ($flmod0 x (real->flonum y))]
         [else (domain-error who x)])]
      [(ratnum?)
       (type-case x
         [(fixnum? bignum? ratnum?) (exmod0 x y)]
         [(flonum?) ($flmod0 x (real->flonum y))]
         [else (domain-error who x)])]
      [else (domain-error who y)])))

(set-who! remainder
  (let* ([fmod (cflop2 "(cs)mod")]
         [f (lambda (x y)
              (cond
                [(eqv? x 0) 0]
                [else
                 (let ([r (fmod (real->flonum x) (real->flonum y))])
                   (if (fl= r 0.0)
                       ;; Always return positive 0.0 --- not sure why,
                       ;; but Racket and other Schemes seem to agree
                       0.0
                       r))]))])
    (lambda (x y)
      (type-case y
        [(fixnum?)
         (when (fx= y 0) (domain-error who y))
         (cond
           [(or (fx= y 1) (fx= y -1)) (unless (integer? x) (noninteger-error who x)) 0]
           [else
             (type-case x
               [(fixnum?) (fxremainder x y)]
               [(bignum?) (intremainder x y)]
               [else
                 (unless (integer? x) (noninteger-error who x))
                 (f x y)])])]
        [(bignum?)
         (type-case x
           [(fixnum? bignum?) (intremainder x y)]
           [else
             (unless (integer? x) (noninteger-error who x))
             (f x y)])]
        [else
          (unless (integer? y) (noninteger-error who y))
          (unless (integer? x) (noninteger-error who x))
          (when (= y 0) (domain-error who y))
          (f x y)]))))

(set-who! even?
   (lambda (x)
      (type-case x
         [(fixnum?) (fxeven? x)]
         [(bignum?) (not (bigodd? x))]
         [(flonum?)
          (when (exceptional-flonum? x) (noninteger-error who x))
          (let ([y (fl* ($flfloor (fl/ x 2.0)) 2.0)])
             (cond
                [(fl= x y) #t]
                [(fl= (fl+ y 1.0) x) #f]
                [else (noninteger-error who x)]))]
         [else
          (unless (integer? x) (noninteger-error who x))
          (even? (real-part x))])))

(set-who! odd?
   (lambda (x)
      (type-case x
         [(fixnum?) (fxodd? x)]
         [(bignum?) (bigodd? x)]
         [(flonum?)
          (when (exceptional-flonum? x) (noninteger-error who x))
          (let ([y (fl* ($flfloor (fl/ x 2.0)) 2.0)])
             (cond
                [(fl= x y) #f]
                [(fl= (fl+ y 1.0) x) #t]
                [else (noninteger-error who x)]))]
         [else
          (unless (integer? x) (noninteger-error who x))
          (odd? (real-part x))])))

(set-who! round
  (lambda (x)
    (type-case x
      [(flonum?) (flround x)]
      [(fixnum? bignum?) x]
      [(ratnum?)
       (let ([x1 (+ x 1/2)])
         (let ([x2 (floor x1)])
           (if (and (= x1 x2) (odd? x2))
               (- x2 1)
               x2)))]
      [else (nonreal-error who x)])))

;;; help routines used by library entries
;;; they are fully generic, but the cases are organized to catch those
;;; the library routines don't check first

(set! $=
   (lambda (who x y)
      (type-case x
         [(fixnum?)
          (type-case y
             [(fixnum?) (fx= x y)]
             [(bignum? ratnum? $exactnum?) #f]
             [(cflonum?) (if (fixnum-floatable-wlop? x) (cfl= (fixnum->flonum x) y) (exact-inexact-compare? = x y))]
             [else (nonnumber-error who y)])]
         [(bignum?)
          (type-case y
             [(fixnum?) #f]
             [(bignum?) (big= x y)]
             [(ratnum? $exactnum?) #f]
             [(flonum?) (exact-inexact-compare? = x y)]
             [($inexactnum?) (and (fl= ($inexactnum-imag-part y) 0.0) (= x ($inexactnum-real-part y)))]
             [else (nonnumber-error who y)])]
         [(ratnum?)
          (type-case y
             [(fixnum? bignum? $exactnum?) #f]
             [(ratnum?)
              (and (= ($ratio-numerator x) ($ratio-numerator y))
                   (= ($ratio-denominator x) ($ratio-denominator y)))]
             [(flonum?) (exact-inexact-compare? = x y)]
             [($inexactnum?) (and (fl= ($inexactnum-imag-part y) 0.0) (= x ($inexactnum-real-part y)))]
             [else (nonnumber-error who y)])]
         [($exactnum? $inexactnum?)
          (unless (number? y) (nonnumber-error who y))
          (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))]
         [(flonum?)
          (type-case y
             [(cflonum?) (cfl= x y)]
             [(fixnum?) (if (fixnum-floatable-wlop? y) (fl= x (fixnum->flonum y)) (exact-inexact-compare? = y x))]
             [(bignum? ratnum?) (exact-inexact-compare? = y x)]
             [($exactnum?) #f]
             [else (nonnumber-error who y)])]
         [else (nonnumber-error who x)])))

(set! $<
   (lambda (who x y)
      (type-case x
         [(fixnum?)
          (type-case y
             [(fixnum?) (fx< x y)]
             [(bignum?) ($bigpositive? y)]
             [(ratnum?) (< (* ($ratio-denominator y) x) ($ratio-numerator y))]
             [(flonum?) (if (fixnum-floatable-wlop? x) (< (fixnum->flonum x) y) (exact-inexact-compare? < x y))]
             [else (nonreal-error who y)])]
         [(bignum?)
          (type-case y
             [(bignum?) (big< x y)]
             [(fixnum?) (not ($bigpositive? x))]
             [(ratnum?) (< (* ($ratio-denominator y) x) ($ratio-numerator y))]
             [(flonum?) (exact-inexact-compare? < x y)]
             [else (nonreal-error who y)])]
         [(ratnum?)
          (type-case y
             [(fixnum? bignum?)
              (< ($ratio-numerator x) (* ($ratio-denominator x) y))]
             [(ratnum?)
              (< (* ($ratio-numerator x) ($ratio-denominator y))
                 (* ($ratio-numerator y) ($ratio-denominator x)))]
             [(flonum?) (exact-inexact-compare? < x y)]
             [else (nonreal-error who y)])]
         [(flonum?)
          (type-case y
             [(flonum?) (fl< x y)]
             [(fixnum?) (if (fixnum-floatable-wlop? y) (fl< x (fixnum->flonum y)) (exact-inexact-compare? > y x))]
             [(bignum? ratnum?) (exact-inexact-compare? > y x)]
             [else (nonreal-error who y)])]
         [else (nonreal-error who x)])))

(set! $<=
   (lambda (who x y)
      (type-case x
         [(fixnum?)
          (type-case y
             [(fixnum?) (fx<= x y)]
             [(bignum?) ($bigpositive? y)]
             [(ratnum?)
              (<= (* ($ratio-denominator y) x) ($ratio-numerator y))]
             [(flonum?) (if (fixnum-floatable-wlop? x) (<= (fixnum->flonum x) y) (exact-inexact-compare? <= x y))]
             [else (nonreal-error who y)])]
         [(bignum?)
          (type-case y
             [(bignum?) (not (big< y x))]
             [(fixnum?) (not ($bigpositive? x))]
             [(ratnum?)
              (<= (* ($ratio-denominator y) x) ($ratio-numerator y))]
             [(flonum?) (exact-inexact-compare? <= x y)]
             [else (nonreal-error who y)])]
         [(ratnum?)
          (type-case y
             [(fixnum? bignum?)
              (<= ($ratio-numerator x) (* ($ratio-denominator x) y))]
             [(ratnum?)
              (<= (* ($ratio-numerator x) ($ratio-denominator y))
                 (* ($ratio-numerator y) ($ratio-denominator x)))]
             [(flonum?) (exact-inexact-compare? <= x y)]
             [else (nonreal-error who y)])]
         [(flonum?)
          (type-case y
             [(flonum?) (fl<= x y)]
             [(fixnum?) (if (fixnum-floatable-wlop? y) (fl<= x (fixnum->flonum y)) (exact-inexact-compare? >= y x))]
             [(bignum? ratnum?) (exact-inexact-compare? >= y x)]
             [else (nonreal-error who y)])]
         [else (nonreal-error who x)])))

(set! $+
  (lambda (who x y)
    (define (exint-unknown+ who x y)
      (type-case y
        [(fixnum? bignum?) (integer+ x y)]
        [(ratnum?)
         (let ([d ($ratio-denominator y)])
           (integer/ (+ (* x d) ($ratio-numerator y)) d))]
        [(flonum?) (exact-inexact+ x y)]
        [($exactnum? $inexactnum?)
         (make-rectangular (+ x (real-part y)) (imag-part y))]
        [else (nonnumber-error who y)]))
    (cond
      [(eqv? y 0) (unless (number? x) (nonnumber-error who x)) x]
      [else
        (type-case x
          [(fixnum?)
           (cond
             [(fx= x 0) (unless (number? y) (nonnumber-error who y)) y]
             [else (exint-unknown+ who x y)])]
          [(bignum?) (exint-unknown+ who x y)]
          [(ratnum?)
           (type-case y
             [(fixnum? bignum?)
              (let ([d ($ratio-denominator x)])
                (integer/ (+ (* y d) ($ratio-numerator x)) d))]
             [(ratnum?)
              (let ([xd ($ratio-denominator x)] [yd ($ratio-denominator y)])
                (integer/
                  (+ (* ($ratio-numerator x) yd) (* ($ratio-numerator y) xd))
                  (* xd yd)))]
             [($exactnum? $inexactnum?)
              (make-rectangular (+ x (real-part y)) (imag-part y))]
             [(flonum?) (exact-inexact+ x y)]
             [else (nonnumber-error who y)])]
          [(flonum?)
           (type-case y
             [(cflonum?) (cfl+ x y)]
             [(fixnum? bignum? ratnum?) (exact-inexact+ y x)]
             [($exactnum?)
              (make-rectangular (+ x (real-part y)) (imag-part y))]
             [else (nonnumber-error who y)])]
          [($exactnum? $inexactnum?)
           (type-case y
             [(fixnum? bignum? ratnum? flonum?)
              (make-rectangular (+ (real-part x) y) (imag-part x))]
             [($exactnum? $inexactnum?)
              (make-rectangular (+ (real-part x) (real-part y))
                (+ (imag-part x) (imag-part y)))]
             [else (nonnumber-error who y)])]
          [else (nonnumber-error who x)])])))

(set! $*
  (lambda (who x y)
    (define (exint-unknown* who x y)
      (type-case y
        [(fixnum? bignum?) (integer* x y)]
        [(ratnum?) (integer/ (* x ($ratio-numerator y)) ($ratio-denominator y))]
        [($exactnum? $inexactnum?)
         (make-rectangular (* x (real-part y)) (* x (imag-part y)))]
        [(flonum?) (exact-inexact* x y)]
        [else (nonnumber-error who y)]))
    (cond
      [(and (fixnum? y) ($fxu< (#3%fx+ y 1) 3))
       (cond
         [(fx= y 0) (unless (number? x) (nonnumber-error who x)) 0]
         [(fx= y 1) (unless (number? x) (nonnumber-error who x)) x]
         [else ($negate who x)])]
      [else
        (type-case x
          [(fixnum?)
           (cond
             [($fxu< (#3%fx+ x 1) 3)
              (cond
                [(fx= x 0) (unless (number? y) (nonnumber-error who y)) 0]
                [(fx= x 1) (unless (number? y) (nonnumber-error who y)) y]
                [else ($negate who y)])]
             [else (exint-unknown* who x y)])]
          [(bignum?) (exint-unknown* who x y)]
          [(ratnum?)
           (type-case y
             [(fixnum? bignum?)
              (integer/ (* y ($ratio-numerator x)) ($ratio-denominator x))]
             [(ratnum?)
              (integer/
                (* ($ratio-numerator x) ($ratio-numerator y))
                (* ($ratio-denominator x) ($ratio-denominator y)))]
             [($exactnum? $inexactnum?)
              (make-rectangular (* x (real-part y)) (* x (imag-part y)))]
             [(flonum?) (exact-inexact* x y)]
             [else (nonnumber-error who y)])]
          [(flonum?)
           (type-case y
             [(cflonum?) (cfl* x y)]
             [(fixnum? bignum? ratnum?) (exact-inexact* y x)]
             [($exactnum?)
              (make-rectangular (* x (real-part y)) (* x (imag-part y)))]
             [else (nonnumber-error who y)])]
          [($exactnum? $inexactnum?)
           (type-case y
             [(fixnum? bignum? ratnum? flonum?)
              (make-rectangular (* (real-part x) y) (* (imag-part x) y))]
             [($exactnum? $inexactnum?)
              (let ([a (real-part x)] [b (imag-part x)]
                    [c (real-part y)] [d (imag-part y)])
                (make-rectangular (- (* a c) (* b d)) (+ (* a d) (* b c))))]
             [else (nonnumber-error who y)])]
          [else (nonnumber-error who x)])])))

(set! $-
  (lambda (who x y)
    (define (exint-unknown- who x y)
      (type-case y
        [(fixnum? bignum?) (integer- x y)]
        [(ratnum?)
         (let ([d ($ratio-denominator y)])
           (integer/ (- (* x d) ($ratio-numerator y)) d))]
        [($exactnum? $inexactnum?)
         (make-rectangular (- x (real-part y)) (- (imag-part y)))]
        [(flonum?) (exact-inexact- x y)]
        [else (nonnumber-error who y)]))
    (cond
      [(eqv? y 0) (unless (number? x) (nonnumber-error who x)) x]
      [else
        (type-case x
          [(fixnum?)
           (cond
             [(eqv? x 0) ($negate who y)]
             [else (exint-unknown- who x y)])]
          [(bignum?) (exint-unknown- who x y)]
          [(ratnum?)
           (type-case y
             [(fixnum? bignum?)
              (let ([d ($ratio-denominator x)])
                (integer/ (- ($ratio-numerator x) (* y d)) d))]
             [(ratnum?)
              (let ([xd ($ratio-denominator x)] [yd ($ratio-denominator y)])
                (integer/
                  (- (* ($ratio-numerator x) yd) (* ($ratio-numerator y) xd))
                  (* xd yd)))]
             [($exactnum? $inexactnum?)
              (make-rectangular (- x (real-part y)) (- (imag-part y)))]
             [(flonum?) (exact-inexact- x y)]
             [else (nonnumber-error who y)])]
          [(flonum?)
           (type-case y
             [(cflonum?) (cfl- x y)]
             [(fixnum? bignum? ratnum?) (inexact-exact- x y)]
             [($exactnum?)
              (make-rectangular (- x (real-part y)) (- (imag-part y)))]
             [else (nonnumber-error who y)])]
          [($exactnum? $inexactnum?)
           (type-case y
             [(fixnum? bignum? ratnum? flonum?)
              (make-rectangular (- (real-part x) y) (imag-part x))]
             [($exactnum? $inexactnum?)
              (make-rectangular (- (real-part x) (real-part y))
                (- (imag-part x) (imag-part y)))]
             [else (nonnumber-error who y)])]
          [else (nonnumber-error who x)])])))

(set! $/
  (lambda (who x y)
    (define (unknown-exint/ who x y)
      (type-case x
        [(fixnum?)
         (when (eqv? y 0) (domain-error who y))
         (if (eqv? x 0) 0 (integer/ x y))]
        [(bignum?)
         (when (eqv? y 0) (domain-error who y))
         (integer/ x y)]
        [(ratnum?)
         (when (eqv? y 0) (domain-error who y))
         (integer/ ($ratio-numerator x) (* y ($ratio-denominator x)))]
        [($exactnum?)
         (when (eqv? y 0) (domain-error who y))
         (make-rectangular (/ (real-part x) y) (/ (imag-part x) y))]
        [($inexactnum?)
         (make-rectangular (/ (real-part x) y) (/ (imag-part x) y))]
        [(flonum?) (inexact-exact/ x y)]
        [else (nonnumber-error who x)]))
    (type-case y
      [(fixnum?)
       (cond
         [(fx= y 1) (unless (number? x) (nonnumber-error who x)) x]
         [(fx= y -1) (unless (number? x) (nonnumber-error who x)) ($negate who x)]
         [else (unknown-exint/ who x y)])]
       [(bignum?) (unknown-exint/ who x y)]
       [(ratnum?)
        (type-case x
          [(fixnum? bignum?)
           (integer/ (* x ($ratio-denominator y)) ($ratio-numerator y))]
          [(ratnum?)
           (integer/ (* ($ratio-numerator x) ($ratio-denominator y))
                     (* ($ratio-denominator x) ($ratio-numerator y)))]
          [($exactnum? $inexactnum?)
           (make-rectangular (/ (real-part x) y) (/ (imag-part x) y))]
          [(flonum?) (inexact-exact/ x y)]
          [else (nonnumber-error who x)])]
       [(flonum?)
        (type-case x
          [(cflonum?) (cfl/ x y)]
          [(fixnum? bignum? ratnum?) (exact-inexact/ x y)]
          [($exactnum?)
           (make-rectangular (/ (real-part x) y) (/ (imag-part x) y))]
          [else (nonnumber-error who x)])]
       [($exactnum? $inexactnum?)
        (type-case x
          [(fixnum? bignum? ratnum? flonum?)
           ;; a / c+di => c(a/(cc+dd)) + (-d(a/cc+dd))i
           (let ([c (real-part y)] [d (imag-part y)])
             (let ([t (/ x (+ (* c c) (* d d)))])
               (make-rectangular (* c t) (- (* d t)))))]
          [($exactnum? $inexactnum?)
           ;; a+bi / c+di => (ac+bd)/(cc+dd) + ((bc-ad)/(cc+dd))i
           (let ([a (real-part x)] [b (imag-part x)]
                                   [c (real-part y)] [d (imag-part y)])
             (let ([t (+ (* c c) (* d d))])
               (make-rectangular (/ (+ (* a c) (* b d)) t)
                 (/ (- (* b c) (* a d)) t))))]
          [else (nonnumber-error who x)])]
       [else (nonnumber-error who y)])))

(set! conjugate
   (lambda (x)
      (type-case x
         [(flonum? fixnum? ratnum? bignum?) x]
         [($inexactnum?)
          (fl-make-rectangular ($inexactnum-real-part x)
                               (fl- ($inexactnum-imag-part x)))]
         [($exactnum?)
          ($make-exactnum ($exactnum-real-part x)
                           (- ($exactnum-imag-part x)))]
         [else (nonnumber-error 'conjugate x)])))

(set! magnitude-squared
   (lambda (x)
      (type-case x
         [(flonum?) (fl* x x)]
         [($inexactnum?)
          (let ([a ($inexactnum-real-part x)] [b ($inexactnum-imag-part x)])
             (fl+ (fl* a a) (fl* b b)))]
         [(fixnum? ratnum? bignum?) (* x x)]
         [($exactnum?)
          (let ([a ($exactnum-real-part x)] [b ($exactnum-imag-part x)])
             (+ (* a a) (* b b)))]
         [else (nonnumber-error 'magnitude-squared x)])))

(set! cfl-magnitude-squared
   (lambda (x)
      (type-case x
         [(flonum?) (fl* x x)]
         [($inexactnum?)
          (let ([a ($inexactnum-real-part x)] [b ($inexactnum-imag-part x)])
             (fl+ (fl* a a) (fl* b b)))]
         [else (noncflonum-error 'cfl-magnitude-squared x)])))

(set! zero?
   (lambda (z)
      (type-case z
         [(fixnum?) (fx= z 0)]
         [(flonum?) (fl= z 0.0)]
         [($inexactnum?) (cfl= z 0.0)]
         [(bignum? ratnum? $exactnum?) #f]
         [else (nonnumber-error 'zero? z)])))

(set-who! nan?
  (lambda (x)
    (type-case x
      [(flonum?) ($nan? x)]
      [(fixnum? bignum? ratnum?) #f]
      [else (nonreal-error who x)])))

(set-who! infinite?
  (lambda (x)
    (type-case x
      [(flonum?) (infinity? x)]
      [(fixnum? bignum? ratnum?) #f]
      [else (nonreal-error who x)])))

(set-who! finite?
  (lambda (x)
    (type-case x
      [(flonum?) (not (exceptional-flonum? x))]
      [(fixnum? bignum? ratnum?) #t]
      [else (nonreal-error who x)])))

(let ()
  (define $ash
    (lambda (who x n)
      (type-case n
        [(fixnum?)
         (type-case x
           [(fixnum?)
            (let ([max-fx-shift (- (constant fixnum-bits) 1)])
              (if (fx< n 0)
                 ; can't just go for it since (- n) may not be representable
                  (if (fx< n (- max-fx-shift))
                      (fxsra x max-fx-shift)
                      (fxsra x (fx- n)))
                  (if (fx> n max-fx-shift)
                      (integer-ash x n)
                      (let ([m (#3%fxsll x n)])
                        (if (fx= (fxsra m n) x)
                            m
                            (integer-ash x n))))))]
           [(bignum?) (integer-ash x n)]
           [else (nonexact-integer-error who x)])]
        [(bignum?)
         (type-case x
           [(fixnum? bignum?)
            (let ([k (if (negative? n)
                         (most-negative-fixnum)
                         (most-positive-fixnum))])
              (ash (ash x k) (- n k)))]
           [else (nonexact-integer-error who x)])]
        [else (nonexact-integer-error who n)])))

  (set-who! ash (lambda (x n) ($ash who x n)))

  (set-who! bitwise-arithmetic-shift (lambda (x n) ($ash who x n))))

(set-who! bitwise-arithmetic-shift-left (lambda (x n) ($sll who x n)))

(set-who! bitwise-arithmetic-shift-right (lambda (x n) ($sra who x n)))

(set-who! integer-length
  (lambda (x)
    (type-case x
      [(fixnum?) (fxlength x)]
      [(bignum?) (biglength x)]
      [else (nonexact-integer-error who x)])))

(set-who! bitwise-length ; same as integer-length
  (lambda (x)
    (type-case x
      [(fixnum?) (fxlength x)]
      [(bignum?) (biglength x)]
      [else (nonexact-integer-error who x)])))

(set-who! bitwise-if
  (lambda (x y z)
    (define big-if
      (lambda (ei1 ei2 ei3)
        (bitwise-ior (bitwise-and ei1 ei2)
                     (bitwise-and (bitwise-not ei1) ei3))))
    (type-case x
      [(fixnum?)
       (type-case y
         [(fixnum?)
          (type-case z
            [(fixnum?) (fxif x y z)]
            [(bignum?) (big-if x y z)]
            [else (nonexact-integer-error who z)])]
         [(bignum?)
          (type-case z
            [(fixnum? bignum?) (big-if x y z)]
            [else (nonexact-integer-error who z)])]
         [else (nonexact-integer-error who y)])]
      [(bignum?)
       (type-case y
         [(fixnum? bignum?)
          (type-case z
            [(fixnum? bignum?) (big-if x y z)]
            [else (nonexact-integer-error who z)])]
         [else (nonexact-integer-error who y)])]
      [else (nonexact-integer-error who x)])))

(set-who! bitwise-copy-bit
  (lambda (x y b)
    (unless (and (integer? x) (exact? x))
      ($oops who "~s is not an exact integer" x))
    (unless (or (and (fixnum? y) (fxnonnegative? y))
                (and (bignum? y) ($bigpositive? y)))
      ($oops who "~s is not a nonnegative exact integer" y))
    (cond
      [(eq? b 0) (logbit0 y x)]
      [(eq? b 1) (logbit1 y x)]
      [else ($oops who "bit argument ~s is not 0 or 1" b)])))

(let ()
  (define count-table
    (let ()
      (define-syntax make-count-table
        (lambda (x)
          #`'#,(let ([bv (make-bytevector 256)])
                 (define slow-bit-count
                   (lambda (x)
                     (do ([x x (fxsrl x 1)] [cnt 0 (if (fxodd? x) (fx+ cnt 1) cnt)])
                         ((fx= x 0) cnt))))
                 (do ([i 0 (fx+ i 1)])
                     ((fx= i 256))
                   (bytevector-u8-set! bv i (slow-bit-count i)))
                 bv)))
      (make-count-table)))
  (define $fxbit-count
    (lambda (n)
      (if (fx= n 0)
          0
          (constant-case ptr-bits
            [(64)
             (fx+ (bytevector-u8-ref count-table (fxlogand n #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 8) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 16) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 24) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 32) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 40) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 48) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 56) #xff)))]
            [(32)
             (fx+ (bytevector-u8-ref count-table (fxlogand n #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 8) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 16) #xff))
                  (bytevector-u8-ref count-table (fxlogand (fxsrl n 24) #xff)))]))))
  (define $big-bit-count
    (lambda (n)
      (let ([end (fx+ (fx* ($bignum-length n) (constant bigit-bytes)) (constant bignum-data-disp))])
        (do ([i (constant bignum-data-disp) (fx+ i 1)]
             [cnt 0 (+ cnt (bytevector-u8-ref count-table ($object-ref 'unsigned-8 n i)))])
            ((fx= i end) cnt)))))
  (set-who! fxbit-count
    (lambda (n)
      (unless (fixnum? n) ($oops who "~s is not a fixnum" n))
      (if (fx< n 0)
          (fxnot ($fxbit-count (fxnot n)))
          ($fxbit-count n))))
  (set-who! bitwise-bit-count
    (lambda (n)
      (cond
        [(fixnum? n)
         (if (fx< n 0)
             (fxnot ($fxbit-count (fxnot n)))
             ($fxbit-count n))]
        [(bignum? n)
         (if ($bigpositive? n)
             ($big-bit-count n)
             (fxnot ($big-bit-count (bitwise-not n))))]
        [else ($oops who "~s is not an exact integer" n)]))))

(set-who! bitwise-first-bit-set
  (let ()
    (define $big-first-bit-set
      (foreign-procedure "(cs)s_big_first_bit_set" (ptr) ptr))
    (lambda (n)
      (cond
        [(fixnum? n) (fxfirst-bit-set n)]
        [(bignum? n) ($big-first-bit-set n)]
        [else ($oops who "~s is not an exact integer" n)]))))

(set-who! bitwise-bit-field
  (let ()
   ; big-positive-bit-field assumes n is a positive bignum, start and
   ; end are nonnegative fixnums, and end > start
    (define big-positive-bit-field
      (foreign-procedure "(cs)s_big_positive_bit_field" (ptr ptr ptr) ptr))
    (define (generic-bit-field n start end)
      (bitwise-and
        ($sra who n start)
        (- ($sll who 1 (- end start)) 1)))
    (lambda (n start end)
      (unless (or (fixnum? n) (bignum? n))
        ($oops who "~s is not an exact integer" n))
      (cond
        [(and (fixnum? start) (fixnum? end))
         (unless (fx>= start 0) ($oops who "invalid start index ~s" start))
         (unless (fx>= end start) ($oops who "invalid end index ~s" end))
         (cond
           [(fx= end start) 0]
           [(and (fixnum? n) (fx< end (fx- (fixnum-width) 1)))
            (fxsra (fxand n (fxnot (fxsll -1 end))) start)]
           [(and (bignum? n) ($bigpositive? n))
            (big-positive-bit-field n start end)]
           [else (generic-bit-field n start end)])]
        [else
         (unless (or (and (fixnum? start) (fx>= start 0))
                     (and (bignum? start) ($bigpositive? start)))
           ($oops who "invalid start index ~s" start))
         (unless (or (and (fixnum? end) (>= end start))
                     (and (bignum? end) (>= end start)))
           ($oops who "invalid end index ~s" end))
         (generic-bit-field n start end)]))))

(set-who! exact-integer-sqrt
  (lambda (n)
    (define (big-integer-sqrt n)
     ; adapted from SRFI 77 mail-archive posting by Brad Lucier, who derived
     ; it from "Karatsuba Square Root" by Paul Zimmermann, INRIA technical report
     ; RR-3805, 1999.
      (if (and (fixnum? n) (or (not (fixnum? (expt 2 52))) (< n (expt 2 52))))
          (let ([q (flonum->fixnum (flsqrt (fixnum->flonum n)))])
            (values q (fx- n (fx* q q))))
          (let ([b ($sra who (+ (integer-length n) 1) 2)])
            (let-values ([(s^ r^) (big-integer-sqrt ($sra who n (+ b b)))])
              (let* ([q&u (intquotient-remainder
                            (+ ($sll who r^ b)
                               (bitwise-bit-field n b (+ b b)))
                            ($sll who s^ 1))]
                     [q (car q&u)]
                     [u (cdr q&u)])
                (let ([s (+ ($sll who s^ b) q)]
                      [r (- (+ ($sll who u b)
                               (bitwise-bit-field n 0 b))
                            (* q q))])
                  (if (negative? r)
                      (values
                        (- s 1)
                        (+ r (- ($sll who s 1) 1)))
                      (values s r))))))))
    (cond
      [(and (fixnum? n) (fx>= n 0))
       (if (or (not (fixnum? (expt 2 52)))
               (fx< n (expt 2 52)))
           (let ([q (flonum->fixnum (flsqrt (fixnum->flonum n)))])
             (values q (fx- n (fx* q q))))
           (big-integer-sqrt n))]
      [(and (bignum? n) (#%$bigpositive? n)) (big-integer-sqrt n)]
      [else ($oops who "~s is not a nonnegative exact integer" n)])))

(set-who! $quotient-remainder
  (lambda (x y)
    (type-case y
      [(fixnum? bignum?)
       (when (eq? y 0) (domain-error who y))
       (type-case x
         [(fixnum? bignum?) (intquotient-remainder x y)]
         [else (nonexact-integer-error who x)])]
      [else (nonexact-integer-error who y)])))

(set! random
   (let ([fxrandom (foreign-procedure "(cs)s_fxrandom"
                      (scheme-object) scheme-object)]
         [flrandom (foreign-procedure "(cs)s_flrandom"
                      (scheme-object) scheme-object)])
      (lambda (x)
         (cond
            [(and (fixnum? x) (fx> x 0)) (fxrandom x)]
            [(and (flonum? x) (fl> x 0.0)) (flrandom x)]
            [(and (bignum? x) (> x 0))
             (let ([radix (most-positive-fixnum)])
                (do ([i x (quotient i radix)]
                     [a (fxrandom radix) (+ (* a radix) (fxrandom radix))])
                    ((<= i radix) (remainder a x))))]
            [else ($oops 'random "invalid argument ~s" x)]))))

(set! random-seed ; must follow \#-
   (let ([limit #xFFFFFFFF]
         [get-seed (foreign-procedure "(cs)s_random_seed"
                      () unsigned-32)]
         [set-seed (foreign-procedure "(cs)s_set_random_seed"
                      (unsigned-32) void)])
      (case-lambda
         [() (get-seed)]
         [(n)
          (unless (and (or (fixnum? n) (bignum? n)) (<= 1 n limit))
             ($oops 'random-seed "invalid argument ~s" n))
          (set-seed n)])))

(let ()
  (define-syntax fl-op
    (syntax-rules ()
      [(_ name $name x ...)
       (set-who! name
         (lambda (x ...)
           (unless (flonum? x) ($oops who "~s is not a flonum" x))
           ...
           ($name x ...)))]))
  (fl-op flexp $flexp x)
  (fl-op flsin $flsin x)
  (fl-op flcos $flcos x)
  (fl-op fltan $fltan x)
  (fl-op flasin $flasin x)
  (fl-op flacos $flacos x)
  (fl-op flsqrt $flsqrt x)
  (fl-op flexpt $flexpt x y)
  (fl-op flfloor $flfloor x)
  (fl-op flceiling $flceiling x))

(set-who! flinteger?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    ($flinteger? x)))

(set-who! fllog
  (rec fllog
    (case-lambda
      [(x)
       (unless (flonum? x) ($oops who "~s is not a flonum" x))
       ($fllog x)]
      [(x y)
       (unless (flonum? x) ($oops who "~s is not a flonum" x))
       (unless (flonum? y) ($oops who "~s is not a flonum" y))
       (/ ($fllog x) ($fllog y))])))

(set-who! flatan
  (rec flatan
    (case-lambda
      [(x)
       (unless (flonum? x) ($oops who "~s is not a flonum" x))
       ($flatan x)]
      [(x y)
       (unless (flonum? x) ($oops who "~s is not a flonum" x))
       (unless (flonum? y) ($oops who "~s is not a flonum" y))
       (flatan2 x y)])))

(set-who! fltruncate
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (if (negated-flonum? x) (fl- ($flfloor (flabs x))) ($flfloor x))))

(set-who! flnan?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    ($nan? x)))

(set-who! flinfinite?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (infinity? x)))

(set-who! flfinite?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (not (exceptional-flonum? x))))

(set-who! flzero?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (fl= x 0.0)))

(set-who! flpositive?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (fl> x 0.0)))

(set-who! flnegative?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (fl< x 0.0)))

(set-who! flnonpositive?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (fl<= x 0.0)))

(set-who! flnonnegative?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (fl>= x 0.0)))

(set-who! fleven?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (when (exceptional-flonum? x) (noninteger-error who x))
    (let ([y (fl* ($flfloor (fl/ x 2.0)) 2.0)])
      (cond
        [(fl= x y) #t]
        [(fl= (fl+ y 1.0) x) #f]
        [else (noninteger-error who x)]))))

(set-who! flodd?
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (when (exceptional-flonum? x) (noninteger-error who x))
    (let ([y (fl* ($flfloor (fl/ x 2.0)) 2.0)])
      (cond
        [(fl= x y) #f]
        [(fl= (fl+ y 1.0) x) #t]
        [else (noninteger-error who x)]))))

(set-who! flmin
  (let ([$flmin (lambda (x y) (if (or (fl< x y) ($nan? x)) x y))])
    (case-lambda
       [(x y)
        (unless (flonum? x) ($oops who "~s is not a flonum" x))
        (unless (flonum? y) ($oops who "~s is not a flonum" y))
        ($flmin x y)]
       [(x)
        (unless (flonum? x) ($oops who "~s is not a flonum" x))
        x]
       [(x y . r)
        (unless (flonum? x) ($oops who "~s is not a flonum" x))
        (unless (flonum? y) ($oops who "~s is not a flonum" y))
        (let loop ([x ($flmin x y)] [r r])
          (if (null? r)
              x
              (let ([y (car r)])
                (unless (flonum? y) ($oops who "~s is not a flonum" y))
                (loop ($flmin x y) (cdr r)))))])))

(set-who! flmax
  (let ([$flmax (lambda (x y) (if (or (fl> x y) ($nan? x)) x y))])
    (case-lambda
       [(x y)
        (unless (flonum? x) ($oops who "~s is not a flonum" x))
        (unless (flonum? y) ($oops who "~s is not a flonum" y))
        ($flmax x y)]
       [(x)
        (unless (flonum? x) ($oops who "~s is not a flonum" x))
        x]
       [(x y . r)
        (unless (flonum? x) ($oops who "~s is not a flonum" x))
        (unless (flonum? y) ($oops who "~s is not a flonum" y))
        (let loop ([x ($flmax x y)] [r r])
          (if (null? r)
              x
              (let ([y (car r)])
                (unless (flonum? y) ($oops who "~s is not a flonum" y))
                (loop ($flmax x y) (cdr r)))))])))

(set-who! flnumerator
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (cond
      [($flinteger-or-inf? x) x]
      [($nan? x) x]
      [else (inexact (numerator (exact x)))])))

(set-who! fldenominator
  (lambda (x)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (cond
      [($flinteger-or-inf? x) 1.0]
      [($nan? x) x]
      [else (inexact (denominator (exact x)))])))

(set-who! fldiv-and-mod
  (lambda (x y)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (unless (flonum? y) ($oops who "~s is not a flonum" y))
    ($fldiv-and-mod x y)))

(set-who! fldiv
  (lambda (x y)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (unless (flonum? y) ($oops who "~s is not a flonum" y))
    ($fldiv x y)))

(set-who! flmod
  (lambda (x y)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (unless (flonum? y) ($oops who "~s is not a flonum" y))
    ($flmod x y)))

(set-who! fldiv0-and-mod0
  (lambda (x y)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (unless (flonum? y) ($oops who "~s is not a flonum" y))
    ($fldiv0-and-mod0 x y)))

(set-who! fldiv0
  (lambda (x y)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (unless (flonum? y) ($oops who "~s is not a flonum" y))
    ($fldiv0 x y)))

(set-who! flmod0
  (lambda (x y)
    (unless (flonum? x) ($oops who "~s is not a flonum" x))
    (unless (flonum? y) ($oops who "~s is not a flonum" y))
    ($flmod0 x y)))

(set-who! fxdiv-and-mod
  (lambda (x y)
    (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
    (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
    (when (fx= y 0) (domain-error who y))
    ($fxdiv-and-mod x y who)))

(set-who! fxdiv
  (lambda (x y)
    (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
    (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
    (when (fx= y 0) (domain-error who y))
    ($fxdiv x y who)))

(set-who! fxmod
  (lambda (x y)
    (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
    (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
    (when (fx= y 0) (domain-error who y))
    ($fxmod x y)))

(set-who! fxdiv0-and-mod0
  (lambda (x y)
    (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
    (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
    (when (fx= y 0) (domain-error who y))
    ($fxdiv0-and-mod0 x y who)))

(set-who! fxdiv0
  (lambda (x y)
    (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
    (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
    (when (fx= y 0) (domain-error who y))
    ($fxdiv0 x y who)))

(set-who! fxmod0
  (lambda (x y)
    (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
    (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
    (when (fx= y 0) (domain-error who y))
    ($fxmod0 x y)))

(let ()
  (define (return n)
    (if (fixnum? n)
        (values n 0)
        (if ($bigpositive? n)
            (values (- n (expt 2 (fixnum-width))) 1)
            (values (+ n (expt 2 (fixnum-width))) -1))))

  (set-who! fx+/carry
    (lambda (x y z)
      (cond
        [($fx+? ($fx+? x y) z) => (lambda (n) (values n 0))]
        [else
         (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
         (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
         (unless (fixnum? z) ($oops who "~s is not a fixnum" z))
         (return (+ x y z))])))

  (set-who! fx-/carry
    (lambda (x y z)
      (cond
        [($fx-? ($fx-? x y) z) => (lambda (n) (values n 0))]
        [else
         (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
         (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
         (unless (fixnum? z) ($oops who "~s is not a fixnum" z))
         (return (- x y z))]))))

(set-who! fx*/carry
  (lambda (x y z)
    (unless (fixnum? x) ($oops who "~s is not a fixnum" x))
    (unless (fixnum? y) ($oops who "~s is not a fixnum" y))
    (let ([t (* x y)])
      (cond
        [($fx+? t z) => (lambda (n) (values n 0))]
        [else
         (unless (fixnum? z) ($oops who "~s is not a fixnum" z))
         (let-values ([(q r) ($exdiv0-and-mod0 (+ (* x y) z) (expt 2 (fixnum-width)))])
           (values r q))]))))

(set-who! bitwise-copy-bit-field
  (lambda (n start end m)
    (unless (or (fixnum? n) (bignum? n))
      ($oops who "~s is not an exact integer" n))
    (unless (or (and (fixnum? start) (fx>= start 0))
                (and (bignum? start) ($bigpositive? start)))
      ($oops who "invalid start index ~s" start))
    (unless (or (and (fixnum? end) (fixnum? start) (fx>= end start))
                (and (bignum? end) (>= end start)))
      ($oops who "invalid end index ~s" end))
    (unless (or (fixnum? m) (bignum? m))
      ($oops who "~s is not an exact integer" m))
    (let ([mask (- ($sll who 1 (- end start)) 1)])
      (logor
        (logand n (lognot ($sll who mask start)))
        ($sll who (logand m mask) start)))))

(set-who! bitwise-rotate-bit-field
  (lambda (n start end count)
    (unless (or (fixnum? n) (bignum? n))
      ($oops who "~s is not an exact integer" n))
    (unless (or (and (fixnum? start) (fx>= start 0))
                (and (bignum? start) ($bigpositive? start)))
      ($oops who "invalid start index ~s" start))
    (unless (or (and (fixnum? end) (fixnum? start) (fx>= end start))
                (and (bignum? end) (>= end start)))
      ($oops who "invalid end index ~s" end))
    (unless (or (and (fixnum? count) (fx>= count 0))
                (and (bignum? count) ($bigpositive? count)))
      ($oops who "invalid count ~s" count))
    (let ([width (- end start)])
      (if (positive? width)
          (let ([count (mod count width)]
                [mask ($sll who (- ($sll who 1 width) 1) start)])
            (let ([field (logand n mask)])
              (logxor
                (logxor
                  (logand
                    (logor ($sll who field count)
                           ($sra who field (- width count)))
                    mask)
                  field)
                n)))
          n))))

(set-who! fxrotate-bit-field
  (lambda (n start end count)
    (unless (fixnum? n) ($oops who "~s is not a fixnum" n))
    (unless (and (fixnum? end) ($fxu< end (fixnum-width)))
      ($oops who "invalid end index ~s" end))
    (unless (and (fixnum? start) (not ($fxu< end start)))
      (if (and (fixnum? start) ($fxu< start (fixnum-width)))
          ($oops who "start index ~s is greater than end index ~s" start end)
          ($oops who "invalid start index ~s" start)))
    (let ([width (fx- end start)])
      (unless (and (fixnum? count) (not ($fxu< width count)))
        (if (and (fixnum? count) ($fxu< count (fixnum-width)))
            ($oops who "count ~s is greater than difference between end index ~s and start index ~s" count end start)
            ($oops who "invalid count ~s" count)))
      (let ([mask (fxsll (fxsrl -1 (fx- (fixnum-width) width)) start)])
        (let ([field (fxlogand n mask)])
          (fxlogor
            (fxlogxor n field)
            (fxlogand 
              (fxlogor
                (fxsll (fxlogand field (fxsrl mask count)) count)
                (fxsrl field (fx- width count)))
              mask)))))))

(let ()
  (define rev-table
    (let ()
      (define-syntax make-rev-table
        (lambda (x)
          #`'#,(let ([bv (make-bytevector 256)])
                 (for-each
                   (lambda (m)
                     (bytevector-u8-set! bv m
                       (do ([m m (fxsrl m 1)]
                            [m^ 0 (fxior (fxsll m^ 1) (fxand m 1))]
                            [k 8 (fx- k 1)])
                           ((fx= k 0) m^))))
                   (iota 256))
                 bv)))
      (make-rev-table)))

  (define $fxreverse
    (lambda (m k)
      (do ([m m (fxsrl m 8)]
           [m^ 0 (fxior (fxsll m^ 8) (bytevector-u8-ref rev-table (fxand m #xff)))]
           [k k (fx- k 8)])
          ((fx< k 8)
           (fxior
             (fxsll m^ k)
             (fxsrl (bytevector-u8-ref rev-table m) (fx- 8 k)))))))

  (set-who! fxreverse-bit-field
    (lambda (n start end)
      (unless (fixnum? n) ($oops who "~s is not a fixnum" n))
      (unless (and (fixnum? start) ($fxu< start (fixnum-width)))
        ($oops who "invalid start index ~s" start))
      (unless (and (fixnum? end) ($fxu< end (fixnum-width)))
        ($oops who "invalid end index ~s" end))
      (unless (fx<= start end)
        ($oops who "start index ~s is greater than end index ~s" start end))
      (fxcopy-bit-field n start end
        ($fxreverse (fxbit-field n start end) (fx- end start)))))

  (set-who! bitwise-reverse-bit-field
    (lambda (n start end)
      (define sra bitwise-arithmetic-shift-right)
      (define sll bitwise-arithmetic-shift-left)
      (define w-1 (fx- (fixnum-width) 1))
      (define mask (- (sll 1 w-1) 1))
      (unless (or (fixnum? n) (bignum? n))
        ($oops who "~s is not an exact integer" n))
      (unless (or (and (fixnum? start) (fx>= start 0))
                  (and (bignum? start) ($bigpositive? start)))
        ($oops who "invalid start index ~s" start))
      (unless (or (and (fixnum? end) (fx>= end 0))
                  (and (bignum? end) ($bigpositive? end)))
        ($oops who "invalid end index ~s" end))
      (unless (<= start end)
        ($oops who "start index ~s is greater than end index ~s" start end))
      (bitwise-copy-bit-field n start end
        (do ([m (bitwise-bit-field n start end) (sra m w-1)]
             [m^ 0 (logor (sll m^ w-1) ($fxreverse (logand m mask) w-1))]
             [k (- end start) (- k w-1)])
            ((<= k w-1) (logor (sll m^ k) ($fxreverse m k))))))))
)))))))
)
