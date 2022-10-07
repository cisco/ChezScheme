;;; 5_3.ms
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

(define <<
   (case-lambda
      [(x y)
       (and (flonum? x)
            (flonum? y)
            (if (and (fl= x 0.0) (fl= y 0.0))
                (fl< (fl/ 1.0 x) (fl/ 1.0 y))
                (fl< x y)))]
      [(x y z)
       (and (<< x y) (<< y z))]))

(mat inexact
   (== (inexact 0) +0.0)
   (== (inexact #e+1e-400) +0.0)
   (== (inexact #e-1e-400) -0.0)
   (== (inexact #e+1e+400) +inf.0)
   (== (inexact #e-1e+400) -inf.0)
   (== (inexact #e+1e-5000) +0.0)
   (== (inexact #e-1e-5000) -0.0)
   (== (inexact #e+1e+5000) +inf.0)
   (== (inexact #e-1e+5000) -inf.0)

  ; make sure inexact rounds to even whenever exactly half way to next
  ; (assuming 52-bit mantissa + hidden bit)
  ; ratios
   (fl= (inexact (+ (ash 1 52) 0/2)) #x10000000000000.0)
   (fl= (inexact (+ (ash 1 52) 1/2)) #x10000000000000.0)
   (fl= (inexact (+ (ash 1 52) 2/2)) #x10000000000001.0)
   (fl= (inexact (+ (ash 1 52) 3/2)) #x10000000000002.0)
   (fl= (inexact (+ (ash 1 52) 4/2)) #x10000000000002.0)
   (fl= (inexact (+ (ash 1 52) 5/2)) #x10000000000002.0)
  ; integers
   (fl= (inexact (* (+ (ash 1 52) 0/2) 2)) #x20000000000000.0)
   (fl= (inexact (* (+ (ash 1 52) 1/2) 2)) #x20000000000000.0)
   (fl= (inexact (* (+ (ash 1 52) 2/2) 2)) #x20000000000002.0)
   (fl= (inexact (* (+ (ash 1 52) 3/2) 2)) #x20000000000004.0)
   (fl= (inexact (* (+ (ash 1 52) 4/2) 2)) #x20000000000004.0)
   (fl= (inexact (* (+ (ash 1 52) 5/2) 2)) #x20000000000004.0)
   (fl= (inexact (ash (* (+ (ash 1 52) 0/2) 2) 40)) #x200000000000000000000000.0)
   (fl= (inexact (ash (* (+ (ash 1 52) 1/2) 2) 40)) #x200000000000000000000000.0)
   (fl= (inexact (ash (* (+ (ash 1 52) 2/2) 2) 40)) #x200000000000020000000000.0)
   (fl= (inexact (ash (* (+ (ash 1 52) 3/2) 2) 40)) #x200000000000040000000000.0)
   (fl= (inexact (ash (* (+ (ash 1 52) 4/2) 2) 40)) #x200000000000040000000000.0)
   (fl= (inexact (ash (* (+ (ash 1 52) 5/2) 2) 40)) #x200000000000040000000000.0)
  ; make sure inexact rounds up when more than half way to next
  ; (assuming 52-bit mantissa + hidden bit)
  ; ratios
   (fl= (inexact (+ (ash 1 52) 0/2 1/4)) #x10000000000000.0)
   (fl= (inexact (+ (ash 1 52) 1/2 1/4)) #x10000000000001.0)
   (fl= (inexact (+ (ash 1 52) 2/2 1/4)) #x10000000000001.0)
   (fl= (inexact (+ (ash 1 52) 3/2 1/4)) #x10000000000002.0)
   (fl= (inexact (+ (ash 1 52) 4/2 1/4)) #x10000000000002.0)
   (fl= (inexact (+ (ash 1 52) 5/2 1/4)) #x10000000000003.0)
   (fl= (inexact (+ (ash 1 52) 1/2 1/8)) #x10000000000001.0)
   (fl= (inexact (+ (ash 1 52) 3/2 1/8)) #x10000000000002.0)
   (fl= (inexact (+ (ash 1 52) 1/2 (expt 2 -80))) #x10000000000001.0)
   (fl= (inexact (+ (ash 1 52) 3/2 (expt 2 -80))) #x10000000000002.0)
  ; integers
   (fl= (inexact (* (+ (ash 1 52) 0/2 1/4) 4)) #x40000000000000.0)
   (fl= (inexact (* (+ (ash 1 52) 1/2 1/4) 4)) #x40000000000004.0)
   (fl= (inexact (* (+ (ash 1 52) 2/2 1/4) 4)) #x40000000000004.0)
   (fl= (inexact (* (+ (ash 1 52) 3/2 1/4) 4)) #x40000000000008.0)
   (fl= (inexact (* (+ (ash 1 52) 4/2 1/4) 4)) #x40000000000008.0)
   (fl= (inexact (* (+ (ash 1 52) 5/2 1/4) 4)) #x4000000000000C.0)
   (fl= (inexact (* (+ (ash 1 52) 1/2 1/8) 8)) #x80000000000008.0)
   (fl= (inexact (* (+ (ash 1 52) 3/2 1/8) 8)) #x80000000000010.0)
   (fl= (inexact (* (+ (ash 1 52) 1/2 (expt 2 -80)) (expt 2 80)))
        #x1000000000000100000000000000000000.0)
   (fl= (inexact (* (+ (ash 1 52) 3/2 (expt 2 -80)) (expt 2 80)))
        #x1000000000000200000000000000000000.0)
   ; verify fix for incorrect input of 2.2250738585072011e-308 reported by leppie
   ; 2.2250738585072011e-308 falls right on the edge between normalized and denormalized numbers,
   ; and should not be rounded up to a normalized number
   (equal?
     (number->string (string->number "2.2250738585072011e-308"))
     "2.225073858507201e-308|52")
   (equal?
     (decode-float (string->number "2.2250738585072011e-308"))
     '#(#b1111111111111111111111111111111111111111111111111111 -1074 1))
   ; similar case in binary...
   (equal?
     (decode-float (string->number "#b1.111111111111111111111111111111111111111111111111111011e-1111111111"))
     '#(#b1111111111111111111111111111111111111111111111111111 -1074 1))
   (equal?
     (number->string (string->number "#b1.111111111111111111111111111111111111111111111111111011e-1111111111"))
     "2.225073858507201e-308|52")
   ; slightly higher number should be rounded up
   (equal?
     (number->string (string->number "2.2250738585072012e-308"))
     "2.2250738585072014e-308")
   (equal?
     (number->string (string->number "#b1.111111111111111111111111111111111111111111111111111100e-1111111111"))
     "2.2250738585072014e-308")
)

(mat exact
   (error? (exact (nan)))
   (error? (exact +inf.0))
   (error? (exact -inf.0))
   (eq? (exact +0.0) 0)
   (eq? (exact -0.0) 0)
)

(mat ==
   (== 1.0 1.0)
   (== -1.0 -1.0)
   (not (== -1.0 +1.0))
   (not (== +1.0 -1.0))
   (== 0.0 0.0)
   (== -0.0 -0.0)
   (not (== -0.0 +0.0))
   (not (== +0.0 -0.0))
   (== +inf.0 +inf.0)
   (== -inf.0 -inf.0)
   (not (== -inf.0 +inf.0))
   (not (== +inf.0 -inf.0))
   (== (nan) (nan))
   (not (== +inf.0 (nan)))
   (not (== (nan) -inf.0))
   (not (== 0.0 0.0-0.0i))
   (== +e +e)
   (== -e -e)
   (not (== +e +0.0))
   (not (== -e -0.0))
 )

(mat <<
   (<< -1.0 1.0)
   (not (<< +1.0 -1.0))
   (not (<< 0.0 0.0))
   (<< -0.0 +0.0)
   (not (<< +0.0 -0.0))
   (<< -inf.0 +inf.0)
   (not (<< +inf.0 -inf.0))
   (not (<< (nan) (nan)))
   (not (<< (nan) +0.0))
   (not (<< +0.0 (nan)))
   (<< -e +0.0 +e)
   (<< -e -0.0 +e)
   (not (<< +e +e))
   (not (<< -e -e))
 )

(mat fl=
   (let ((n (read (open-input-string "+nan.0"))))
     (not (fl= n n)))
   (fl= (nan))
   (not (fl= (nan) +inf.0))
   (not (fl= (nan) -inf.0))
   (not (fl= (nan) (nan)))
   (not (fl= (nan) 0.0))
   (fl= +inf.0 +inf.0)
   (fl= -inf.0 -inf.0)
   (not (fl= -inf.0 +inf.0))
   (fl= +0.0 -0.0)
 )

(mat fl<
   (fl< (nan))
   (not (fl< (nan) (nan)))
   (not (fl< (nan) 0.0))
   (not (fl< 0.0 (nan)))
   (fl< -inf.0 0.0)
 )

(mat fl>
   (fl> (nan))
   (not (fl> (nan) (nan)))
   (not (fl> (nan) 0.0))
   (not (fl> 0.0 (nan)))
   (fl> +inf.0 -inf.0)
   (fl> +inf.0 0.0)
   (not (fl> +0.0 -0.0))
 )

(mat fl<=
   (fl<= (nan))
   (not (fl<= (nan) (nan)))
   (not (fl<= (nan) 0.0))
   (not (fl<= 0.0 (nan)))
 )

(mat fl>=
   (fl>= (nan))
   (not (fl>= (nan) (nan)))
   (not (fl>= (nan) 0.0))
   (not (fl>= 0.0 (nan)))
 )

(mat fl-
   (== (fl- +0.0) -0.0)
   (== (fl- -0.0) +0.0)
   (== (fl- +inf.0) -inf.0)
   (== (fl- -inf.0) +inf.0)
   (== (fl- (nan)) (nan))
   (== (fl- -0.0 -0.0) +0.0)
   (== (fl- +0.0 -0.0) +0.0)
   (== (fl- -0.0 +0.0) -0.0)
   (== (fl- +0.0 +0.0) +0.0)
   (andmap
     (lambda (a)
       (andmap
         (lambda (b)
           (andmap
             (lambda (c) (== (fl- a b c) (fl- (fl- a b) c)))
             '(0.0 -0.0)))
         '(0.0 -0.0)))
     '(0.0 -0.0))
   (let ()
     (define-syntax ff
       (syntax-rules ()
         [(_ k1 k2) (lambda (x) (eqv? (fl- k1 x k2) (fl- (fl- k1 x) k2)))]))
     (andmap
       (lambda (p) (and (p +0.0) (p -0.0)))
       (list (ff +0.0 +0.0) (ff +0.0 -0.0) (ff -0.0 +0.0) (ff -0.0 -0.0))))
   (error? (fl- 3.0 5.4 'a))
   (error? (fl- 'a 3.0 5.4))
   (error? (fl- 3.0 'a 5.4))
   (== (fl- 5.0 4.0 3.0 2.0) -4.0)
   (== (fl- 5.0 4.0 3.0 2.0 1.0 0.0 -1.0 -2.0) -2.0)
   (begin
     (define ($fl-f x y) (fl- -0.0 x y))
     (procedure? $fl-f))
   (== ($fl-f 3.0 4.0) -7.0)
   (== (fl- 1e30 1e30 7.0) -7.0)
 )

(mat +
  ; just in case we're ever tempted to combine nested generic arithmetic operators...
  (begin
    (define f1a (lambda (x) (= (+ x 2) (+ (+ x 1) 1))))
    (define f1b (lambda (x) (= (+ (+ x 1) 1) x)))
    (define f2 (lambda (x) (= (- (+ x 1e308) 1e308) +inf.0)))
    #t)
  (f1a 0)
  (not (f1a (inexact (expt 2 53))))
  (not (f1b 0))
  (f1b (inexact (expt 2 53)))
  (not (f2 (inexact 0)))
  (f2 +inf.0)
  (not (f2 +nan.0))
  (f2 1e308)
 )

(mat -
   (== (- +0.0) -0.0)
   (== (- -0.0) +0.0)
   (== (- +inf.0) -inf.0)
   (== (- -inf.0) +inf.0)
   (== (- (nan)) (nan))
   (== (- -0.0 -0.0) +0.0)
   (== (- +0.0 -0.0) +0.0)
   (== (- -0.0 +0.0) -0.0)
   (== (- +0.0 +0.0) +0.0)
   (andmap
     (lambda (a)
       (andmap
         (lambda (b)
           (andmap
             (lambda (c) (== (- a b c) (- (- a b) c)))
             '(0.0 -0.0)))
         '(0.0 -0.0)))
     '(0.0 -0.0))
   (error? (- 3.0 5.4 'a))
   (error? (- 'a 3.0 5.4))
   (error? (- 3.0 'a 5.4))
   (== (- 1e30 1e30 7.0) -7.0)
   (begin
     (define $ieee-foo
       (lambda (x)
         (- x 1e30 7.0)))
     #t)
   (== ($ieee-foo 1e30) -7.0)
 )

(mat fl+
   (== (fl+ -0.0 -0.0) -0.0)
   (== (fl+ +0.0 -0.0) +0.0)
   (== (fl+ -0.0 +0.0) +0.0)
   (== (fl+ +0.0 +0.0) +0.0)
 )

(mat fl*
   (== (fl* -1.0 +0.0) -0.0)
   (== (fl* -1.0 -0.0) +0.0)
   (== (fl* +1.0 +0.0) +0.0)
   (== (fl* +1.0 -0.0) -0.0)
 )

(mat fl/
   (== (fl/ +0.0) +inf.0)
   (== (fl/ -0.0) -inf.0)
   (== (fl/ +inf.0) +0.0)
   (== (fl/ -inf.0) -0.0)
   (== (fl/ (nan)) (nan))
   (== (fl/ +1.0 +0.0) +inf.0)
   (== (fl/ +1.0 -0.0) -inf.0)
   (== (fl/ -1.0 +0.0) -inf.0)
   (== (fl/ -1.0 -0.0) +inf.0)
   (== (fl/ +0.0 +0.0) (nan))
   (== (fl/ +0.0 -0.0) (nan))
   (== (fl/ -0.0 +0.0) (nan))
   (== (fl/ -0.0 -0.0) (nan))
   (andmap
     (lambda (a)
       (andmap
         (lambda (b)
           (andmap
             (lambda (c) (== (fl/ a b c) (fl/ (fl/ a b) c)))
             '(1e300 1e250)))
         '(1e300 1e250)))
     '(1e300 1e250))
   (error? (fl/ 3.0 5.4 'a))
   (error? (fl/ 'a 3.0 5.4))
   (error? (fl/ 3.0 'a 5.4))
   (== (fl/ 16.0 2.0 -2.0 2.0) -2.0)
   (== (fl/ 16.0 2.0 -2.0 2.0 4.0 1.0 -1.0) 0.5)
   (== (fl/ 1e300 1e300 1e300) 1e-300)
 )

(mat /
   (== (/ +0.0) +inf.0)
   (== (/ -0.0) -inf.0)
   (== (/ +inf.0) +0.0)
   (== (/ -inf.0) -0.0)
   (== (/ (nan)) (nan))
   (== (/ +1.0 +0.0) +inf.0)
   (== (/ +1.0 -0.0) -inf.0)
   (== (/ -1.0 +0.0) -inf.0)
   (== (/ -1.0 -0.0) +inf.0)
   (== (/ +0.0 +0.0) (nan))
   (== (/ +0.0 -0.0) (nan))
   (== (/ -0.0 +0.0) (nan))
   (== (/ -0.0 -0.0) (nan))
   (andmap
     (lambda (a)
       (andmap
         (lambda (b)
           (andmap
             (lambda (c) (== (/ a b c) (/ (/ a b) c)))
             '(1e300 1e250)))
         '(1e300 1e250)))
     '(1e300 1e250))
   (error? (/ 3.0 5.4 'a))
   (error? (/ 'a 3.0 5.4))
   (error? (/ 3.0 'a 5.4))
   (== (fl/ 1e300 1e300 1e300) 1e-300)
 )

(mat expt
   (== (expt +0.0 +0.0) +1.0)
   (== (expt -0.0 +0.0) +1.0)
   (== (expt +0.0 -0.0) +1.0)
   (== (expt -0.0 -0.0) +1.0)
   (== (expt +1.0 +0.0) +1.0)
   (== (expt -1.0 +0.0) +1.0)
   (== (expt +0.0 +1.0) +0.0)
   (== (expt -0.0 +1.0) -0.0)
   (== (expt -0.0 +2.0) +0.0)
   (== (expt -0.0 +3.0) -0.0)
   (== (expt +inf.0 +0.0) +1.0)
   (== (expt +inf.0 +1.0) +inf.0)
   (== (expt -inf.0 +0.0) +1.0)
   (== (expt -inf.0 +1.0) -inf.0)
   (== (expt +inf.0 +inf.0) +inf.0)
   (== (expt +inf.0 -inf.0) +0.0)
   (== (expt -inf.0 +inf.0) +nan.0+nan.0i)
   (== (expt -inf.0 -inf.0) +nan.0+nan.0i)
   (== (expt +inf.0 +.5) +inf.0)
   (== (expt (nan) +.5) (nan))
   (== (expt +.5 (nan)) (nan))
   (== (expt (nan) (nan)) (nan))
   (== (expt (nan) +0.0) +1.0)
   (== (expt +0.0 (nan)) (nan))
   (== (expt +0.0 (nan)) (nan))
   (== (expt +inf.0+2i 2) +inf.0+0.0i)
   (== (let ([n (expt 2 32)]) (expt 2 (make-rectangular n n))) -inf.0+inf.0i)
 )

(mat magnitude
   (== (magnitude -0.0) 0.0)
   (== (magnitude 0.0) 0.0)
   (== (magnitude 0.0-0.0i) 0.0)
   (== (magnitude -1.0) 1.0)
   (== (magnitude 1.0) 1.0)
   (== (magnitude 0.0+1.0i) 1.0)
   (== (magnitude +inf.0) +inf.0)
   (== (magnitude -inf.0) +inf.0)
   (== (magnitude +inf.0+inf.0i) +inf.0)
   (== (magnitude +inf.0+2.0i) +inf.0)
   (== (magnitude +2.0+inf.0i) +inf.0)
   (== (magnitude (nan)) (nan))
   (== (magnitude (make-rectangular (nan) (nan))) (nan))
   (== (magnitude (make-rectangular +0.0 (nan))) (nan))
   (<< +0.0 (magnitude (make-rectangular +e +e)))
   (<< +0.0 (magnitude (make-rectangular -e -e)))
 )

(mat sqrt
   ; from Kahan
   (== (sqrt -0.0) -0.0) ; the grounds that (negative? -0.0) => #f
   (== (sqrt -4.0) +0.0+2.0i)
   (== (sqrt -inf.0) +0.0+inf.0i)
   (== (sqrt 0.0+inf.0i) +inf.0+inf.0i)
   (== (sqrt 4.0+inf.0i) +inf.0+inf.0i)
   (== (sqrt +inf.0+inf.0i) +inf.0+inf.0i)
   (== (sqrt -0.0+inf.0i) +inf.0+inf.0i)
   (== (sqrt -4.0+inf.0i) +inf.0+inf.0i)
   (== (sqrt -inf.0+inf.0i) +inf.0+inf.0i)
   (== (sqrt 0.0-inf.0i) +inf.0-inf.0i)
   (== (sqrt 4.0-inf.0i) +inf.0-inf.0i)
   (== (sqrt +inf.0-inf.0i) +inf.0-inf.0i)
   (== (sqrt -0.0-inf.0i) +inf.0-inf.0i)
   (== (sqrt -4.0-inf.0i) +inf.0-inf.0i)
   (== (sqrt -inf.0-inf.0i) +inf.0-inf.0i)
   (== (sqrt (make-rectangular (nan) +0.0)) (make-rectangular (nan)(nan)))
   (== (sqrt (make-rectangular 0.0 (nan))) (make-rectangular (nan) (nan)))
   (== (sqrt (make-rectangular (nan) (nan))) (make-rectangular (nan) (nan)))
   (== (sqrt +inf.0+0.0i) +inf.0+0.0i)
   (== (sqrt +inf.0+4.0i) +inf.0+0.0i)
   (== (sqrt +inf.0-0.0i) +inf.0-0.0i)
   (== (sqrt +inf.0-4.0i) +inf.0-0.0i)
   (== (sqrt (make-rectangular +inf.0 (nan))) (make-rectangular +inf.0 (nan)))
   (== (sqrt -inf.0+0.0i) +0.0+inf.0i)
   (== (sqrt -inf.0+4.0i) +0.0+inf.0i)
   (== (sqrt -inf.0-0.0i) +0.0-inf.0i)
   (== (sqrt -inf.0-4.0i) +0.0-inf.0i)
   (let ([z (sqrt (make-rectangular -inf.0 (nan)))])
      (and (== (real-part z) (nan)) (== (abs (imag-part z)) +inf.0)))
   ; others
   (== (sqrt +0.0) +0.0)
   (== (sqrt +1.0) +1.0)
   (== (sqrt +4.0) +2.0)
   (== (sqrt +inf.0) +inf.0)
   (== (sqrt +0.0+0.0i) +0.0+0.0i)
   (== (sqrt +1.0+0.0i) +1.0+0.0i)
   (== (sqrt +4.0+0.0i) +2.0+0.0i)
   (== (sqrt +inf.0+0.0i) +inf.0+0.0i)
   (== (sqrt -0.0+0.0i) +0.0+0.0i)
   (== (sqrt -1.0+0.0i) +0.0+1.0i)
   (== (sqrt -4.0+0.0i) +0.0+2.0i)
   (== (sqrt -inf.0+0.0i) +0.0+inf.0i)
   (== (sqrt -0.0-0.0i) +0.0-0.0i)
   (== (sqrt -1.0-0.0i) +0.0-1.0i)
   (== (sqrt -inf.0-0.0i) +0.0-inf.0i)
   (== (sqrt +0.0-0.0i) +0.0-0.0i)
   (== (sqrt +1.0-0.0i) +1.0-0.0i)
   (== (sqrt +inf.0-0.0i) +inf.0-0.0i)
   (== (sqrt (nan)) (nan))
 )

(mat exp
   (== (exp +0.0) +1.0)
   (== (exp -0.0) +1.0)
   (== (exp +inf.0) +inf.0)
   (== (exp -inf.0) +0.0)
   (== (exp (nan)) (nan))
   (== (exp +0.0+0.0i) +1.0+0.0i)
   (== (exp -0.0-0.0i) +1.0-0.0i)
  ; if exp treats x+0.0i the same as x:
   (== (exp +inf.0+0.0i) +inf.0+0.0i)
  ; otherwise:
   #;(== (exp +inf.0+0.0i) +inf.0+nan.0i)
   (== (exp +inf.0-0.0i) +inf.0-0.0i)
   (== (exp -inf.0+0.0i) 0.0+0.0i)
   (== (exp -inf.0-0.0i) 0.0-0.0i)
  ; if exp treats x+0.0i the same as x:
   (== (exp (make-rectangular (nan) +0.0)) (make-rectangular (nan) +0.0))
  ; otherwise:
   #;(== (exp (make-rectangular (nan) +0.0)) (make-rectangular (nan) (nan)))
  ; if exp treats x+0.0i the same as x:
   (== (exp (make-rectangular (nan) -0.0)) (make-rectangular (nan) -0.0))
  ; otherwise:
   #;(== (exp (make-rectangular (nan) -0.0)) (make-rectangular (nan) (nan)))
   (~= (exp 700.0+.75i) 7.421023049046266e303+6.913398801654868e303i)
   (~= (exp 700.0-.75i) 7.421023049046266e303-6.913398801654868e303i)
   (== (exp 800.0+.75i) +inf.0+inf.0i)
   (== (exp 800.0-.75i) +inf.0-inf.0i)
   (== (exp 800.0+1e-200i) +inf.0+2.7263745721125063e147i)
   (== (exp 800.0-1e-200i) +inf.0-2.7263745721125063e147i)
   (== (exp +inf.0+1.0i) +inf.0+inf.0i)
   (== (exp +inf.0+2.0i) -inf.0+inf.0i)
   (== (exp +inf.0+3.0i) -inf.0+inf.0i)
   (== (exp +inf.0+4.0i) -inf.0-inf.0i)
   (== (exp +inf.0+123.0i) -inf.0-inf.0i)
 )

(mat log
   (== (log 0.0) -inf.0)
   (== (log 1.0) 0.0)
   (== (log +inf.0) +inf.0)

   (== (log -0.0) -inf.0) ; on the grounds that (negative? -0.0) => #f
   (== (log -1.0) (make-rectangular 0.0 +pi))
   (== (log -inf.0) (make-rectangular +inf.0 +pi))

   (== (log +1.0i) (make-rectangular 0.0 +pi/2))
   (== (log -1.0i) (make-rectangular 0.0 -pi/2))

   (== (log -0.0+0.0i) (make-rectangular -inf.0 +pi))
   (== (log -0.0-0.0i) (make-rectangular -inf.0 -pi))
   (== (log +0.0+0.0i) -inf.0+0.0i)
   (== (log +0.0-0.0i) -inf.0-0.0i)

   (== (log +1.0+0.0i) 0.0+0.0i)
   (== (log -1.0+0.0i) (make-rectangular 0.0 +pi))
   (== (log +1.0-0.0i) 0.0-0.0i)
   (== (log -1.0-0.0i) (make-rectangular 0.0 -pi))
 )

(mat fllog
   (== (log 0.0) -inf.0)
   (== (log 1.0) 0.0)
   (== (log +inf.0) +inf.0)

   (== (log -0.0) -inf.0)  ; on the grounds that (negative? -0.0) => #f
   (== (log -1.0) (make-rectangular 0.0 +pi))
   (== (log -inf.0) (make-rectangular +inf.0 +pi))

   (== (log +1.0i) (make-rectangular 0.0 +pi/2))
   (== (log -1.0i) (make-rectangular 0.0 -pi/2))

   (== (log -0.0+0.0i) (make-rectangular -inf.0 +pi))
   (== (log -0.0-0.0i) (make-rectangular -inf.0 -pi))
   (== (log +0.0+0.0i) -inf.0+0.0i)
   (== (log +0.0-0.0i) -inf.0-0.0i)

   (== (log +1.0+0.0i) 0.0+0.0i)
   (== (log -1.0+0.0i) (make-rectangular 0.0 +pi))
   (== (log +1.0-0.0i) 0.0-0.0i)
   (== (log -1.0-0.0i) (make-rectangular 0.0 -pi))
)

(mat sin
   (== (sin +0.0) +0.0)
   (== (sin -0.0) -0.0)
   (== (sin +inf.0) (nan))
   (== (sin -inf.0) (nan))
   (== (sin (nan)) (nan))
 )

(mat cos
   (== (cos +0.0) +1.0)
   (== (cos -0.0) +1.0)
   (== (cos +inf.0) (nan))
   (== (cos -inf.0) (nan))
   (== (cos (nan)) (nan))
 )

(mat tan
   (== (tan +0.0) +0.0)
   (== (tan -0.0) -0.0)
   (== (tan +inf.0) (nan))
   (== (tan -inf.0) (nan))
   (== (tan (nan)) (nan))
   (== (tan -0.0+0.0i) -0.0+0.0i)
 )

(mat asin
   (== (asin +0.0) +0.0)
   (== (asin -0.0) -0.0)
   (== (asin +1.0) +pi/2)
   (== (asin -1.0) -pi/2)
   (== (asin (nan)) (nan))
   (== (asin -0.0+0.0i) -0.0+0.0i)
 )

(mat acos
   (== (acos +1.0) +0.0)
   (== (acos -1.0) +pi)
   (== (acos +0.0) +pi/2)
   (== (acos -0.0) +pi/2)
   (== (acos (nan)) (nan))
 )

(mat atan
   ; cases from Steele (CLtL)
   (== (atan +0.0 +e) +0.0)
   (== (atan +0.0 +inf.0) +0.0)
   (<< +0.0 (atan +e +e) +pi/2)
   (<< +0.0 (atan +inf.0 +inf.0) +pi/2)
   (== (atan +e +0.0) +pi/2)
   (== (atan +inf.0 +0.0) +pi/2)
   (== (atan +e -0.0) +pi/2)
   (== (atan +inf.0 -0.0) +pi/2)
   (<< +pi/2 (atan +e -e) +pi)
   (<< +pi/2 (atan +inf.0 -inf.0) +pi)
   (== (atan +0.0 -e) +pi)
   (== (atan +0.0 -inf.0) +pi)
   (== (atan -0.0 -e) -pi)             ; Steele erroneously says +pi
   (== (atan -0.0 -inf.0) -pi)         ; Steele erroneously says +pi
   (<< -pi (atan -e -e) -pi/2)
   (<< -pi (atan -inf.0 -inf.0) -pi/2)
   (== (atan -e +0.0) -pi/2)
   (== (atan -e -0.0) -pi/2)
   (== (atan -inf.0 +0.0) -pi/2)
   (== (atan -inf.0 -0.0) -pi/2)
   (<< -pi/2 (atan -e +e) -0.0)
   (<< -pi/2 (atan -inf.0 +inf.0) -0.0)
   (== (atan -0.0 +e) -0.0)
   (== (atan -0.0 +inf.0) -0.0)
   (== (atan +0.0 +0.0) +0.0)
   (== (atan -0.0 +0.0) -0.0)
   (== (atan +0.0 -0.0) +pi)
   (== (atan -0.0 -0.0) -pi)

   (== (atan -inf.0) -pi/2)
   (== (atan +inf.0) +pi/2)
   (== (atan +0.0) +0.0)
   (== (atan -0.0) -0.0)
   (if (memq (machine-type) '(i3qnx ti3qnx))
       (~= (atan +1.0) +pi/4)
       (== (atan +1.0) +pi/4))
   (== (atan -1.0) -pi/4)
   (== (atan (nan)) (nan))
   (== (atan -0.0+0.0i) -0.0+0.0i)
)

(mat sinh
   (== (sinh 0.0) 0.0)
   (== (sinh -0.0) -0.0)
   (== (sinh +inf.0) +inf.0)
   (== (sinh -inf.0) -inf.0)
   (== (sinh (nan)) (nan))
   (== (sinh -0.0+0.0i) -0.0+0.0i)
 )

(mat cosh
   (== (cosh 0.0) 1.0)
   (== (cosh -0.0) 1.0)
   (== (cosh +inf.0) +inf.0)
   (== (cosh -inf.0) +inf.0)
   (== (cosh (nan)) (nan))
 )

(mat tanh
   (== (tanh 0.0) 0.0)
   (== (tanh -0.0) -0.0)
   (== (tanh +inf.0) +1.0)
   (== (tanh -inf.0) -1.0)
   (== (tanh (nan)) (nan))
   (== (tanh -0.0+0.0i) -0.0+0.0i)
 )

(mat asinh
   (== (asinh 0.0) 0.0)
   (== (asinh -0.0) -0.0)
   (== (asinh +inf.0) +inf.0)
   (== (asinh -inf.0) -inf.0)
   (== (asinh (nan)) (nan))
   (== (asinh -0.0+0.0i) -0.0+0.0i)
 )

(mat acosh
   (== (acosh 1.0) 0.0)
   (== (acosh +inf.0) +inf.0)
   (== (acosh (nan)) (nan))
 )

(mat atanh
   (== (atanh 0.0) 0.0)
   (== (atanh -0.0) -0.0)
   (== (atanh +1.0) +inf.0)
   (== (atanh -1.0) -inf.0)
   (== (atanh (nan)) (nan))
   (== (atanh -0.0+0.0i) -0.0+0.0i)
   (== (atanh -0.0+0.0i) -0.0+0.0i)
 )

(mat flonum->fixnum
   (error? (flonum->fixnum +inf.0))
   (error? (flonum->fixnum -inf.0))
   (error? (flonum->fixnum (nan)))
   (eq? (flonum->fixnum -0.0) 0)
 )

(mat fllp
  (error? (fllp 3))
  (eqv? (fllp 0.0) 0)
  (eqv? (fllp 1.0) 2046)
  (eqv? (fllp -1.0) 2046)
  (eqv? (fllp 1.5) 2047)
  (eqv? (fllp -1.5) 2047)
  (and (memv (fllp +nan.0) '(4094 4095)) #t)
  (eqv? (fllp +inf.0) 4094)
  (eqv? (fllp -inf.0) 4094)
  (eqv?
    (fllp #b1.1111111111111111111111111111111111111111111111111111e1111111111)
    4093)
  (eqv? (fllp #b1.0e-1111111110) 2)
  (or (eqv? #b.1e-1111111110 0.0)
      (eqv? (fllp #b.1e-1111111110) 1))
  (eqv? (fllp #b.01e-1111111110) 0)
 )

(mat fp-output
  (equal? (number->string 1e23) "1e23")
  (equal? (number->string 4.450147717014403e-308) "4.450147717014403e-308")
  (equal? (number->string 1.1665795231290236e-302) "1.1665795231290236e-302")
 ; fp printing algorithm always rounds up on ties
  (equal? (number->string 3.6954879760742188e-6) "3.6954879760742188e-6")
  (equal? (number->string 5.629499534213123e14) "5.629499534213123e14")
 )

(mat string->number
  (equal? (string->number "+1e-400") +0.0)
  (equal? (string->number "-1e-400") -0.0)
  (equal? (string->number "+1e+400") +inf.0)
  (equal? (string->number "-1e+400") -inf.0)
  (equal? (string->number "+1e-5000") +0.0)
  (equal? (string->number "-1e-5000") -0.0)
  (equal? (string->number "+1e+5000") +inf.0)
  (equal? (string->number "-1e+5000") -inf.0)
  (equal? (string->number "+1e-50000") +0.0)
  (equal? (string->number "-1e-50000") -0.0)
  (equal? (string->number "+1e+50000") +inf.0)
  (equal? (string->number "-1e+50000") -inf.0)
  (equal? (string->number "+1e-500000") +0.0)
  (equal? (string->number "-1e-500000") -0.0)
  (equal? (string->number "+1e+500000") +inf.0)
  (equal? (string->number "-1e+500000") -inf.0)

  (equal? (string->number "#b1e-10000110010") 5e-324)
  (equal? (string->number "5e-324") 5e-324)
  (equal? (string->number "#b-1e-10000110010") -5e-324)
  (equal? (string->number "-5e-324") -5e-324)
  (equal? (string->number "#b1e-10000110010") (inexact (* 5 (expt 10 -324))))
  (equal? (string->number "5e-324") (inexact (* 5 (expt 10 -324))))
  (equal? (string->number "#b-1e-10000110010") (inexact (* -5 (expt 10 -324))))
  (equal? (string->number "-5e-324") (inexact (* -5 (expt 10 -324))))

  (if (memq (machine-type) '(a6nt ta6nt)) ; tolerably inaccurate
      (equal? (string->number "#b1e-10000110100") 0.0)
      (equal? (string->number "#b1e-10000110011") 0.0))
  (if (memq (machine-type) '(a6nt ta6nt)) ; tolerably inaccurate
      (equal? (string->number "#b-1e-10000110100") -0.0)
      (equal? (string->number "#b-1e-10000110011") -0.0))
  (equal? (string->number "5e-325") 0.0)
  (equal? (string->number "-5e-325") -0.0)

  (equal? (string->number "1.7976931348623157e308") 1.7976931348623157e308)
  (equal? (string->number "-1.7976931348623157e308") -1.7976931348623157e308)
  (equal? (string->number "#b1.1111111111111111111111111111111111111111111111111111e1111111111") 1.7976931348623157e308)
  (equal? (string->number "#b-1.1111111111111111111111111111111111111111111111111111e1111111111") -1.7976931348623157e308)
  (equal? (string->number "1.7976931348623157e308") (inexact (* 9007199254740991 (expt 2 971))))
  (equal? (string->number "-1.7976931348623157e308") (inexact (* -9007199254740991 (expt 2 971))))
  (equal? (string->number "#b1.1111111111111111111111111111111111111111111111111111e1111111111") (inexact (* 9007199254740991 (expt 2 971))))
  (equal? (string->number "#b-1.1111111111111111111111111111111111111111111111111111e1111111111") (inexact (* -9007199254740991 (expt 2 971))))

  (equal? (string->number "#b1.11111111111111111111111111111111111111111111111111111e1111111111") +inf.0)
  (equal? (string->number "#b-1.11111111111111111111111111111111111111111111111111111e1111111111") -inf.0)
  (equal? (string->number "1.7976931348623159e308") +inf.0)
  (equal? (string->number "-1.7976931348623159e308") -inf.0)

  (equal? (string->number "1e100000000000000000000") +inf.0)
  (equal? (string->number "-1e100000000000000000000") -inf.0)
  (equal? (string->number "1e-100000000000000000000") 0.0)
  (equal? (string->number "-1e-100000000000000000000") -0.0)
)
