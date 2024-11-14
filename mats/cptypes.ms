;;; cptypes.ms
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

(define-syntax cptypes-equivalent-expansion?
  (syntax-rules ()
    [(_ x y)
     (equivalent-expansion?
      (parameterize ([enable-cp0 #t]
                     [#%$suppress-primitive-inlining #f]
                     #;[optimize-level (max (optimize-level) 2)])
        (expand/optimize x))
      (parameterize ([enable-cp0 #t]
                     [#%$suppress-primitive-inlining #f]
                     #;[optimize-level (max (optimize-level) 2)])
        (expand/optimize y)))]))

(define-syntax cptypes/once-equivalent-expansion?
  ; Replace the default value of run-cp0 with a version that calls
  ; cp0 only once instead of twice.
  ; This is useful to test some reductions that are shared with cp0
  ; or that should be executed in a single pass.
  (syntax-rules ()
    [(_ x y)
     (equivalent-expansion?
      (parameterize ([run-cp0 (lambda (cp0 c) (cp0 c))]
                     [#%$suppress-primitive-inlining #f]
                     #;[optimize-level (max (optimize-level) 2)])
        (expand/optimize x))
      (parameterize ([run-cp0 (lambda (cp0 c) (#3%$cptypes c))]
                     [#%$suppress-primitive-inlining #f]
                     #;[optimize-level (max (optimize-level) 2)])
        (expand/optimize y)))]))

(define-syntax cptypes/nocp0-equivalent-expansion?
  ; When run-cp0 is call, use #3%$cptypes insted of the cp0 function provided.
  ; This disables the reductions in cp0.ss, so it's posible to see
  ; the isolated effect of the reduction in cptypes.ss. 
  (syntax-rules ()
    [(_ x y)
     (equivalent-expansion?
      (parameterize ([run-cp0 (lambda (cp0 c) (#3%$cptypes c))]
                     [#%$suppress-primitive-inlining #f]
                     #;[optimize-level (max (optimize-level) 2)])
        (expand/optimize x))
      (parameterize ([run-cp0 (lambda (cp0 c) (#3%$cptypes c))]
                     [#%$suppress-primitive-inlining #f]
                     #;[optimize-level (max (optimize-level) 2)])
        (expand/optimize y)))]))

(mat cptypes-handcoded
  (cptypes-equivalent-expansion?
    '(vector? (vector))  ;actually reduced by folding, not cptypes 
    #t)
  (cptypes-equivalent-expansion?
    '(vector? (vector 1 2 3)) 
    #t)
  (cptypes-equivalent-expansion?
    '(vector? (box 1)) 
    #f)
  (cptypes-equivalent-expansion?
    '(box? (vector 1 2 3)) 
    #f)
  (cptypes-equivalent-expansion?
    '(box? (box 1)) 
    #t)
  (cptypes-equivalent-expansion?
    '(pair? (cons 1 2)) 
    #t)
  (cptypes-equivalent-expansion?
    '(pair? (list 1 2)) 
    #t)
  (cptypes-equivalent-expansion?
    '(pair? (list)) 
    #f)
  (cptypes-equivalent-expansion?
    '(eq? (newline) (void))
    '(begin (newline) #t))
  (cptypes-equivalent-expansion?
    '(eq? (newline) 0)
    '(begin (newline) #f))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (vector? x))
    '(lambda (x) (vector-set! x 0 0) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (box? x))
    '(lambda (x) (vector-set! x 0 0) #f))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (vector-set! x 0 0) (set! y (vector? x)))
    '(lambda (x y) (vector-set! x 0 0) (set! y #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (set! y (vector-ref x 0)) (list (vector? x) y))
    '(lambda (x y) (set! y (vector-ref x 0)) (list #t y)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (let ([y (random 7)]) (list (vector? x) y y)))
    '(lambda (x) (vector-set! x 0 0) (let ([y (random 7)]) (list #t y y))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (let ([y (vector? x)]) (list (random 7) y y)))
    '(lambda (x) (vector-set! x 0 0) (let ([y #t]) (list (random 7) y y))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (let ([y (vector-ref x 0)]) (list (vector? x) y y)))
    '(lambda (x) (let ([y (vector-ref x 0)]) (list #t y y))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (let ([y (vector-ref x 0)])
                   (let ([z (vector? x)])
                     (list y y z z))))
    '(lambda (x) (let ([y (vector-ref x 0)])
                   (let ([z #t])
                     (list y y z z)))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (let ([y (vector-ref x 0)]) (display (list y y))) (vector? x))
    '(lambda (x) (let ([y (vector-ref x 0)]) (display (list y y))) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (let ([y (random 7)]) (display (list (vector-ref x 0) y y))) (vector? x))
    '(lambda (x) (let ([y (random 7)]) (display (list (vector-ref x 0) y y))) #t))
  (cptypes-equivalent-expansion?
    '(let ([y (vector 1 2 3)]) (display (list (vector? y) y y)))
    '(let ([y (vector 1 2 3)]) (display (list #t y y))))
  (cptypes-equivalent-expansion?
    '(let ([y (vector 1 2 3)]) (display (list y y)) (vector? y))
    '(let ([y (vector 1 2 3)]) (display (list y y)) #t))
  (cptypes-equivalent-expansion?
    '(vector? (let ([y (vector 1 2 3)]) (display (list y y)) y))
    '(let ([y (vector 1 2 3)]) (display (list y y)) #t))
  (cptypes-equivalent-expansion?
    '(vector? (let ([y (vector 1 2 3)]) (display (list y y)) (vector 4 5 6)))
    '(let ([y (vector 1 2 3)]) (display (list y y)) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (null? x) (display x)))
    '(lambda (x) (when (null? x) (display '()))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (vector? x) (eq? x 'vector?)))
    '(lambda (x) (when (vector? x) #f)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (vector? x) (pair? x)))
    '(lambda (x) (when (vector? x) #f)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (vector? x) (vector? x)))
    '(lambda (x) (when (vector? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (procedure? x) (procedure? x)))
    '(lambda (x) (when (procedure? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (f) (f) (procedure? f))
    '(lambda (f) (f) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x)
       (vector-set! x 0 0)
       (let loop ([n 1000])
         (unless (zero? n)
           (display (vector? x))
           (loop (- n 1)))))
    '(lambda (x)
       (vector-set! x 0 0)
       (let loop ([n 1000])
         (unless (zero? n)
           (display #t)
           (loop (- n 1))))))
  (cptypes-equivalent-expansion?
    '(lambda (x)
       (let loop ([n 1000])
         (unless (zero? n)
           (vector-set! x 0 n)
           (loop (- n 1))))
       (vector? x))
    '(lambda (x)
       (let loop ([n 1000])
         (unless (zero? n)
           (vector-set! x 0 n)
           (loop (- n 1))))
       (vector? x)))
  (cptypes-equivalent-expansion?
    '(begin (error 'who "msg") 1) ;could be reduced in cp0
    '(begin (error 'who "msg") 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x) 1)
    '(lambda (x) (vector-set! x) 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (#2%-) 1)
    '(lambda (x) (#2%-) 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (#2%make-vector x 0 7) 1)
    '(lambda (x) (#2%make-vector x 0 7) 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (set-box! x 0) 1)
    '(lambda (x) (vector-set! x 0 0) (set-box! x 0) 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x x x) 1)
    '(lambda (x) (vector-set! x x x) 2))
  ;This test may fail because the subexpressions may be reordered
  (cptypes-equivalent-expansion?
    '(lambda (x) (+ (unbox x) (car x)))
    '(lambda (x) (- (unbox x) (car x))))
  ;Regresion test for primitives with 0 arguments after an error
  (cptypes-equivalent-expansion?
    '(lambda () (box (let ([x (error 'who "msg")]) (cons x (random)))))
    '(lambda () (error 'who "msg")))
  (parameterize ([debug-level 2])
    (cptypes-equivalent-expansion?
     '(lambda () (box (let ([x (error 'who "msg")]) (cons x (random)))))
     '(lambda () (#%$value (error 'who "msg")))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! (box 5) 0 0) 1)
    '(lambda (x) (vector-set! (box 5) 0 0) 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (#2%odd? x) (real? x))
    '(lambda (x) (#2%odd? x) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (#2%odd? x) 1)
    '(lambda (x) (vector-set! x 0 0) (#2%odd? x) 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) (zero? x)))
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) (#3%eq? x 0))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (or (fixnum? x) (bignum? x)) (zero? x)) x))
    '(lambda (x) (when (and (or (fixnum? x) (bignum? x)) (zero? x)) 0)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (zero? x)))
    '(lambda (x) (when (fixnum? x) (#3%fxzero? x))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (fixnum? x) (zero? x)) x))
    '(lambda (x) (when (and (fixnum? x) (zero? x)) 0)))
  (cptypes/once-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (zero? x) 7))
    '(lambda (x) (when (fixnum? x) 7)))
)

(mat cptypes-type-if
  (cptypes-equivalent-expansion?
    '(lambda (x) (if (vector-ref x 0) (newline) (void)) (vector? x))
    '(lambda (x) (if (vector-ref x 0) (newline) (void)) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (if (vector-ref x 0) (vector? x) (void)))
    '(lambda (x) (if (vector-ref x 0) #t (void))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (if (vector-ref x 0) (void) (vector? x)))
    '(lambda (x) (if (vector-ref x 0) (void) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (if (zero? (vector-ref x 0)) (newline) (void)) (vector? x))
    '(lambda (x) (if (zero? (vector-ref x 0)) (newline) (void)) #t))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (if (zero? (random 2)) (vector-set! x 0 0) (void)) (vector? x))
         '(lambda (x) (if (zero? (random 2)) (vector-set! x 0 0) (void)) #t)))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (if (zero? (random 2)) (void) (vector-set! x 0 0)) (vector? x))
         '(lambda (x) (if (zero? (random 2)) (void) (vector-set! x 0 0)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (if x (newline) (void)))
    '(lambda (x) (vector-set! x 0 0) (if #t (newline) (void))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (if (vector? x) (newline) (void)))
    '(lambda (x) (vector-set! x 0 0) (if #t (newline) (void))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (vector? x) (if x (newline) (void))))
    '(lambda (x) (when (vector? x) (if #t (newline) (void)))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (boolean? x) (if x (newline) (void))))
         '(lambda (x) (when (boolean? x) (if #t (newline) (void))))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (if (zero? (random 2)) (vector? x) (void)))
    '(lambda (x) (vector-set! x 0 0) (if (zero? (random 2)) #t (void))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (if (zero? (random 2)) (void) (vector? x)))
    '(lambda (x) (vector-set! x 0 0) (if (zero? (random 2)) (void) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (if (vector? x) (vector? x) (void)))
    '(lambda (x) (if (vector? x) #t (void))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (if (vector? x) (void) (vector? x)))
         '(lambda (x) (if (vector? x) (void) #t))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (if (vector? x) (if (vector? y) (list (vector? x) (vector? y)) (void)) (void)))
    '(lambda (x y) (if (vector? x) (if (vector? y) (list #t #t) (void)) (void))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (if (and (vector? x) (vector? y)) (list (vector? x) (vector? y)) (void)))
    '(lambda (x y) (if (and (vector? x) (vector? y)) (list #t #t) (void))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x y) (if (or (vector? x) (vector? y)) (vector? x) (void)))
         '(lambda (x y) (if (or (vector? x) (vector? y)) #t (void)))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x y) (if (or (vector? x) (vector? y)) (vector? y) (void)))
         '(lambda (x y) (if (or (vector? x) (vector? y)) #t (void)))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (if (if (vector? x) (vector? y) #f) (list (vector? x) (vector? y)) (void)))
    '(lambda (x y) (if (if (vector? x) (vector? y) #f) (list #t #t) (void))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (if (if (vector? x) (vector? y) #t) (void) (vector? x)))
    '(lambda (x y) (if (if (vector? x) (vector? y) #t) (void) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (t) (let ([x (if t (begin (newline) #f) #f)]) (display x) (number? x)))
    '(lambda (t) (let ([x (if t (begin (newline) #f) #f)]) (display x) #f)))
  (cptypes-equivalent-expansion?
    '(lambda (t) (let ([x (if t 1 2)]) (fixnum? x)))
    '(lambda (t) (let ([x (if t 1 2)]) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (t) (let ([x (if t 1 2.0)]) (number? x)))
    '(lambda (t) (let ([x (if t 1 2.0)]) #t)))
  (cptypes-equivalent-expansion?
    '(if (error 'who "msg") (display 1) (display 2))
    '(if (error 'who "msg") (display -1) (display -2)))
  (cptypes-equivalent-expansion?
    '(begin (if (error 'who "msg") (display 1) (display 2)) (display 3))
    '(begin (if (error 'who "msg") (display 1) (display 2)) (display -3)))
  (cptypes-equivalent-expansion?
    '(begin (if (box? (box 0)) (error 'who "msg") (void)) (display 1))
    '(begin (if (box? (box 0)) (error 'who "msg") (void)) (display -1)))
  (not (cptypes-equivalent-expansion?
         '(begin (if (box? (box 0)) (void) (error 'who "msg")) (display 1))
         '(begin (if (box? (box 0)) (void) (error 'who "msg")) (display -1))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (if (zero? (random 2)) (vector-set! x 0 0) (error 'who "msg")) (vector? x))
    '(lambda (x) (if (zero? (random 2)) (vector-set! x 0 0) (error 'who "msg")) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (if (zero? (random 2)) (error 'who "msg") (vector-set! x 0 0)) (vector? x))
    '(lambda (x) (if (zero? (random 2)) (error 'who "msg") (vector-set! x 0 0)) #t))
  (cptypes-equivalent-expansion?
    '(begin (if (zero? (random 2)) (error 'who "msg") (error 'who "other")) (display 1))
    '(begin (if (zero? (random 2)) (error 'who "msg") (error 'who "other")) (display -1)))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (if y (vector-set! x 0 0) (vector-set! x 0 1)) (vector? x))
    '(lambda (x y) (if y (vector-set! x 0 0) (vector-set! x 0 1)) #t))
  (not (cptypes-equivalent-expansion?
         '(lambda (x y) (if y (void) (vector-set! x 0 0)) (vector? x))
         '(lambda (x y) (if y (void) (vector-set! x 0 0)) #t)))
  (not (cptypes-equivalent-expansion?
         '(lambda (x y) (if y (vector-set! x 0 0) (void)) (vector? x))
         '(lambda (x y) (if y (vector-set! x 0 0) (void)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (if (if y (vector? x) (error 'who "msg")) (vector? x) (void)))
    '(lambda (x y) (if (if y (vector? x) (error 'who "msg")) #t (void))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (if (if y (error 'who "msg") (vector? x)) (vector? x) (void)))
    '(lambda (x y) (if (if y (error 'who "msg") (vector? x)) #t (void))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x y) (if (if y (vector? x) (error 'who "msg")) (void) (vector? x)))
         '(lambda (x y) (if (if y (vector? x) (error 'who "msg")) (void) #t))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x y) (if (if y (error 'who "msg") (vector? x)) (void) (vector? x)))
         '(lambda (x y) (if (if y (error 'who "msg") (vector? x)) (void) #t))))
  (cptypes-equivalent-expansion?
    '(lambda (t) (vector? (if t (vector 1) (vector 2))))
    '(lambda (t) (if t (vector 1) (vector 2)) #t))
  (cptypes-equivalent-expansion?
    '(number? (if t 1 2.0))
    '(begin (if t 1 2.0) #t))
  (cptypes-equivalent-expansion?
    '(lambda (t) (fixnum? (if t 1 2)))
    '(lambda (t) (if t 1 2.0) #t))
  (cptypes-equivalent-expansion?
    '(lambda (t) (boolean? (if t #t #f)))
    '(lambda (t) (if t #t #f) #t))
  (cptypes-equivalent-expansion?
    '(lambda (t) ((lambda (x) (if x #t #f)) (if t (vector 1) (box 1))))
    '(lambda (t) (if t (vector 1) (box 1)) #t))
  (cptypes-equivalent-expansion?
    '(lambda (t)(not (if t (vector 1) (box 1))))
    '(lambda (t) (if t (vector 1) (box 1)) #f))
  (cptypes-equivalent-expansion?
     '(lambda (x y z f)
       (let ([t (if x (vector 1) (box 1))])
         (if (if y t z) (f t 1) (f t 2))))
     '(lambda (x y z f)
       (let ([t (if x (vector 1) (box 1))])
         (if (if y #t z) (f t 1) (f t 2)))))
  (not (cptypes-equivalent-expansion?
          '(lambda (x y z f)
            (let ([t (vector? x)])
              (if (if y t z) (f t 1) (f t 2))))
          '(lambda (x y z f)
            (let ([t (vector? x)])
              (if (if y #t z) (f t 1) (f t 2))))))
  (not (cptypes-equivalent-expansion?
          '(lambda (x y z f)
            (let ([t (vector? x)])
              (if (if y t z) (f t 1) (f t 2))))
          '(lambda (x y z f)
            (let ([t (vector? x)])
              (if (if y #f z) (f t 1) (f t 2))))))
  (cptypes-equivalent-expansion?
     '(lambda (t b)
       (if (if t (newline) (unbox b)) (vector? b) (box? b)))
     '(lambda (t b)
       (if (if t (newline) (unbox b)) (vector? b) #t)))
  (cptypes-equivalent-expansion?
     '(lambda (t b)
       (if (if t (unbox b) (newline)) (vector? b) (box? b)))
     '(lambda (t b)
       (if (if t (unbox b) (newline)) (vector? b) #t)))
  (cptypes-equivalent-expansion?
     '(lambda (t b)
       (if (if t #f (unbox b)) (vector? b) (box? b)))
     '(lambda (t b)
       (if (if t #f (unbox b)) #f (box? b))))
  (cptypes-equivalent-expansion?
     '(lambda (t b)
       (if (if t (unbox b) #f) (vector? b) (box? b)))
     '(lambda (t b)
       (if (if t (unbox b) #f) #f (box? b))))
)

(mat cptypes-type-specialize-numbers
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (= x y)))
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (#3%fx= x y))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (= x y)))
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (#3%fl= x y))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (> x y)))
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (#3%fx> x y))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (> x y)))
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (#3%fl> x y))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (max x y)))
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (#3%fxmax x y))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (max x y)))
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (#3%flmax x y))))
  (cptypes/once-equivalent-expansion?
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (fixnum? (max x y))))
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (#3%fxmax x y) #t)))
  (cptypes/once-equivalent-expansion?
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (flonum? (max x y))))
    '(lambda (x y) (when (and (flonum? x) (flonum? y)) (#3%flmax x y) #t)))
)

(mat cptype-directly-applied-case-lambda
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval
        '(let ([t ((lambda (x y) (cons y x)) 'a 'b)])
           (list t t))))
    '((b . a) (b . a)))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval
        '(let ([t ((lambda (x . y) (cons y x)) 'a 'b 'c 'd)])
           (list t t))))
    '(((b c d) . a) ((b c d) . a)))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval
        '(let ([t ((case-lambda
                     [(x) (cons 'first x)]
                     [(x y) (cons* 'second y x)]
                     [(x . y) (cons* 'third y x)]) 'a 'b)])
           (list t t))))
    '((second b . a) (second b . a)))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval
        '(let ([t ((case-lambda
                     [(x) (cons 'first x)]
                     [(x y) (cons* 'second y x)]
                     [(x . y) (cons* 'third y x)]) 'a 'b 'c)])
           (list t t))))
    '((third (b c) . a) (third (b c) . a)))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval 
        '(let ([t 'z])
           ((lambda args (set! t (cons args t))) 'a 'b 'c)
           t)))
    '((a b c) . z))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval 
        '(let ([t 'z])
           ((lambda args (set! t (cons args t))) 'a 'b 'c)
           t)))
    '((a b c) . z))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval 
        '(let ([t 'z])
           ((lambda (x . y) (set! t (cons* y x t))) 'a 'b 'c)
           t)))
    '((b c) a . z))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval 
        '(let ([t 'z])
           ((case-lambda 
              [(x) (set! t (cons* 'first x t))]
              [(x y) (set! t (cons* 'second y x t))]
              [(x . y) (set! t (cons* 'third y x t))]) 'a 'b)
           t)))
    '(second b a . z))
  (equal?
    (parameterize ([enable-type-recovery #t]
                   [run-cp0 (lambda (cp0 x) x)])
      (eval 
        '(let ([t 'z])
           ((case-lambda 
              [(x) (set! t (cons* 'first x t))]
              [(x y) (set! t (cons* 'second y x t))]
              [(x . y) (set! t (cons* 'third y x t))]) 'a 'b 'c 'd)
           t)))
    '(third (b c d) a . z))
)

(define (test-chain/preamble/self preamble check-self? l)
  (let loop ([l l])
    (if (null? l)
        #t
        (and (or (not check-self?)
                 (cptypes-equivalent-expansion?
                   `(let ()
                      ,preamble
                      (lambda (x) (when (,(car l) x) (,(car l) x))))
                   `(let ()
                      ,preamble
                      (lambda (x) (when (,(car l) x) #t)))))
             (let loop ([t (cdr l)])
               (if (null? t)
                   #t
                   (and (cptypes-equivalent-expansion?
                          `(let ()
                             ,preamble
                             (lambda (x) (when (,(car l) x) (,(car t) x))))
                          `(let ()
                             ,preamble
                             (lambda (x) (when (,(car l) x) #t))))
                        (not (cptypes-equivalent-expansion?
                               `(let ()
                                  ,preamble
                                  (lambda (x) (when (,(car t) x) (,(car l) x))))
                               `(let ()
                                  ,preamble
                                  (lambda (x) (when (,(car t) x) #t)))))
                        (or (not check-self?)
                            (cptypes-equivalent-expansion?
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (and (,(car l) x) (,(car t) x)) (,(car l) x))))
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (and (,(car l) x) (,(car t) x)) #t))))
                            (cptypes-equivalent-expansion?
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (and (,(car t) x) (,(car l) x)) (,(car l) x))))
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (and (,(car t) x) (,(car l) x)) #t))))
                            (cptypes-equivalent-expansion?
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (or (,(car l) x) (,(car t) x)) (,(car t) x))))
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (or (,(car l) x) (,(car t) x)) #t))))
                            (cptypes-equivalent-expansion?
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (or (,(car l) x) (,(car t) x)) (,(car t) x))))
                              `(let ()
                                 ,preamble
                                 (lambda (x) (when (or (,(car t) x) (,(car l) x)) #t))))
                            )
                        (loop (cdr t)))))
             (loop (cdr l))))))

(define (test-chain l)
  (test-chain/preamble/self '(void) #t l))

(define (test-chain* l)
  (test-chain/preamble/self '(void) #f l))

(define (test-chain/preamble preamble l)
  (test-chain/preamble/self preamble #t l))

(define (test-chain*/preamble l)
  (test-chain/preamble/self preamble #f l))

(define (test-disjoint/preamble/self preamble check-self? l)
  (let loop ([l l])
    (if (null? l)
        #t
       (and (or (not check-self?)
                (cptypes-equivalent-expansion?
                  `(let ()
                     ,preamble
                     (lambda (x) (when (,(car l) x) (,(car l) x))))
                  `(let ()
                     ,preamble
                     (lambda (x) (when (,(car l) x) #t)))))
            (let loop ([t (cdr l)])
              (if (null? t)
                  #t
                  (and (cptypes-equivalent-expansion?
                         `(let ()
                            ,preamble
                            (lambda (x) (when (,(car l) x) (,(car t) x))))
                         `(let ()
                            ,preamble
                            (lambda (x) (when (,(car l) x) #f))))
                       (cptypes-equivalent-expansion?
                         `(let ()
                           ,preamble
                           (lambda (x) (when (,(car t) x) (,(car l) x))))
                         `(let ()
                            ,preamble
                            (lambda (x) (when (,(car t) x) #f))))
                       (loop (cdr t)))))
            (loop (cdr l))))))

(define (test-disjoint l)
  (test-disjoint/preamble/self '(void) #t l))

(define (test-disjoint* l)
  (test-disjoint/preamble/self '(void) #f l))

(define (test-disjoint/preamble preamble l)
  (test-disjoint/preamble/self preamble #t l))

(define (test-disjoint*/preamble preamble l)
  (test-disjoint/preamble/self preamble #f l))

(mat cptypes-type-implies?
  (test-chain '((lambda (x) (eq? x 0)) fixnum? (lambda (x) (and (integer? x) (exact? x))) real? number?))
  (test-chain* '((lambda (x) (or (eq? x 0) (eq? x 10))) fixnum? (lambda (x) (and (integer? x) (exact? x))) real? number?))
  (test-chain* '(fixnum? integer? real?))
  (test-chain* '(fixnum? exact? number?)) ; exact? may raise an error
  (test-chain* '(bignum? exact? number?)) ; exact? may raise an error
  (test-chain '((lambda (x) (eqv? x (expt 256 100))) bignum? integer? real? number?))
  (test-chain '((lambda (x) (eqv? 0.0 x)) flonum? real? number?))
  (test-chain* '((lambda (x) (or (eqv? x 0.0) (eqv? x 3.14))) flonum? real? number?))
  (test-chain* '((lambda (x) (or (eq? x 0) (eqv? x 3.14))) real? number?))
  (test-chain '(gensym? symbol?))
  (test-chain '((lambda (x) (eq? x 'banana)) symbol?))
  (test-chain '(not boolean?))
  (test-chain '((lambda (x) (eq? x #t)) boolean?))
  (test-chain* '(record? #3%$record?))
  (test-chain* '((lambda (x) (eq? x car)) procedure?))
  (test-chain* '(record-type-descriptor? #3%$record?))
  (test-disjoint '(pair? box? #3%$record? number?
                   vector? string? bytevector? fxvector? symbol?
                   char? boolean? null? (lambda (x) (eq? x (void)))
                   eof-object? bwp-object? procedure?))
  (test-disjoint '(pair? box? real? gensym? not))
  (test-disjoint '(pair? box? fixnum? flonum? (lambda (x) (eq? x #t))))
  (test-disjoint '(pair? box? fixnum? flonum? (lambda (x) (eq? #t x))))
  (test-disjoint '((lambda (x) (eq? x 0)) (lambda (x) (eq? x 1)) flonum?))
  (test-disjoint '((lambda (x) (eqv? x 0.0)) (lambda (x) (eqv? x 1.0)) fixnum?))
  (test-disjoint '(exact? inexact?))
  (test-disjoint '(integer? ratnum?))
  (test-disjoint '((lambda (x) (eq? x 'banana)) (lambda (x) (eq? x 'apple))))
  (test-disjoint* '(list? record? vector?))
)

; use a gensym to make expansions equivalent
(define my-rec (gensym "my-rec"))
(define my-sub-rec (gensym "my-sub-rec"))
(mat cptypes-type-record?
  ; define-record
  (parameterize ([optimize-level 2])
    (cptypes-equivalent-expansion?
      `(let () (define-record ,my-rec (a)) (lambda (x) (display (my-rec-a x)) (my-rec? x)))
      `(let () (define-record ,my-rec (a)) (lambda (x) (display (my-rec-a x)) #t))))
  (parameterize ([optimize-level 2])
    (cptypes-equivalent-expansion?
      `(let () (define-record ,my-rec (a)) (lambda (x) (set-my-rec-a! x 0) (my-rec? x)))
      `(let () (define-record ,my-rec (a)) (lambda (x) (set-my-rec-a! x 0) #t))))
  (cptypes-equivalent-expansion?
    `(let () (define-record ,my-rec (a)) (let ([x (make-my-rec 0)]) (display (list x x)) (my-rec? x)))
    `(let () (define-record ,my-rec (a)) (let ([x (make-my-rec 0)]) (display (list x x)) #t)))
  (cptypes-equivalent-expansion?
    `(let () (define-record ,my-rec (a)) (let ([x (make-my-rec 0)]) (display (list x x)) (if x 1 2)))
    `(let () (define-record ,my-rec (a)) (let ([x (make-my-rec 0)]) (display (list x x)) (if #t 1 2))))

  (test-chain/preamble `(define-record ,my-rec (a)) '(my-rec? #3%$record?))
  (test-chain/preamble `(begin
                          (define-record ,my-rec (a))
                          (define-record ,(gensym "sub-rec") ,my-rec (b)))
                       '(sub-rec? my-rec? #3%$record?))
  (test-disjoint/preamble `(define-record ,my-rec (a)) '(my-rec? pair? null? not number?))
  (test-disjoint/preamble `(begin
                            (define-record ,my-rec (a))
                            (define-record ,(gensym "other-rec") (a)))
                       '(my-rec? other-rec?))

  ; define-record-type
  (parameterize ([optimize-level 2])
    (cptypes-equivalent-expansion?
      `(let () (define-record-type ,my-rec (fields a)) (lambda (x) (display (my-rec-a x)) (my-rec? x)))
      `(let () (define-record-type ,my-rec (fields a)) (lambda (x) (display (my-rec-a x)) #t))))
  (parameterize ([optimize-level 2])
    (cptypes-equivalent-expansion?
      `(let () (define-record-type ,my-rec (fields (mutable a))) (lambda (x) (my-rec-a-set! x 0) (my-rec? x)))
      `(let () (define-record-type ,my-rec (fields (mutable a))) (lambda (x) (my-rec-a-set! x 0) #t))))
  (cptypes-equivalent-expansion?
    `(let () (define-record-type ,my-rec (fields a)) (let ([x (make-my-rec 0)]) (display (list x x)) (my-rec? x)))
    `(let () (define-record-type ,my-rec (fields a)) (let ([x (make-my-rec 0)]) (display (list x x)) #t)))
  (cptypes-equivalent-expansion?
    `(let () (define-record-type ,my-rec (fields a)) (let ([x (make-my-rec 0)]) (display (list x x)) (if x 1 2)))
    `(let () (define-record-type ,my-rec (fields a)) (let ([x (make-my-rec 0)]) (display (list x x)) (if #t 1 2))))

  (test-chain/preamble `(define-record-type ,my-rec (fields a)) '(my-rec? #3%$record?))
  #;(test-chain/preamble `(begin
                            (define-record-type ,my-rec (fields a))
                            (define-record-type ,(gensym "sub-rec") (parent ,my-rec) (fields b)))
                         '(sub-rec? my-rec? #3%$record?))
  (test-disjoint/preamble `(define-record-type ,my-rec (fields a)) '(my-rec? pair? null? not number?))
  #;(test-disjoint/preamble `(begin
                              (define-record-type ,my-rec (fields a))
                              (define-record-type ,(gensym "other-rec") (fields a)))
                            '(my-rec? other-rec?))

  ; define-record-type (sealed #t)
  (parameterize ([optimize-level 2])
    (cptypes-equivalent-expansion?
      `(let () (define-record-type ,my-rec (fields a) (sealed #t)) (lambda (x) (display (my-rec-a x)) (my-rec? x)))
      `(let () (define-record-type ,my-rec (fields a) (sealed #t)) (lambda (x) (display (my-rec-a x)) #t))))
  (parameterize ([optimize-level 2])
    (cptypes-equivalent-expansion?
      `(let () (define-record-type ,my-rec (fields (mutable a)) (sealed #t)) (lambda (x) (my-rec-a-set! x 0) (my-rec? x)))
      `(let () (define-record-type ,my-rec (fields (mutable a)) (sealed #t)) (lambda (x) (my-rec-a-set! x 0) #t))))
  (cptypes-equivalent-expansion?
    `(let () (define-record-type ,my-rec (fields a) (sealed #t)) (let ([x (make-my-rec 0)]) (display (list x x)) (my-rec? x)))
    `(let () (define-record-type ,my-rec (fields a) (sealed #t)) (let ([x (make-my-rec 0)]) (display (list x x)) #t)))
  (cptypes-equivalent-expansion?
    `(let () (define-record-type ,my-rec (fields a) (sealed #t)) (let ([x (make-my-rec 0)]) (display (list x x)) (if x 1 2)))
    `(let () (define-record-type ,my-rec (fields a) (sealed #t)) (let ([x (make-my-rec 0)]) (display (list x x)) (if #t 1 2))))

  (test-chain/preamble `(define-record-type ,my-rec (fields a) (sealed #t)) '(my-rec? #3%$record?))
  #;(test-chain/preamble `(begin
                            (define-record-type ,my-rec (fields a))
                            (define-record-type ,(gensym "sub-rec") (parent ,my-rec) (fields b) (sealed #t)))
                         '(sub-rec? my-rec? #3%$record?))
  (test-disjoint/preamble `(define-record-type ,my-rec (fields a) (sealed #t)) '(my-rec? pair? null? not number?))
  #;(test-disjoint/preamble `(begin
                              (define-record-type ,my-rec (fields a) (sealed #t))
                              (define-record-type ,(gensym "other-rec") (fields a) (sealed #t)))
                            '(my-rec? other-rec?))
  #;(test-disjoint/preamble `(begin
                              (define-record-type ,my-rec (fields a) (sealed #t))
                              (define-record-type ,(gensym "other-rec") (fields a)))
                            '(my-rec? other-rec?))

  ;; substituting `record-instance?`
  (cptypes-equivalent-expansion?
   `(let ()
      (define-record-type ,my-rec (fields a))
      (define-record-type ,my-sub-rec (fields a) (parent ,my-rec))
      (lambda (x) (and (my-rec? x) (list 'ok (my-sub-rec? x)))))
   `(let ()
      (define-record-type ,my-rec (fields a))
      (define-record-type ,my-sub-rec (fields a) (parent ,my-rec))
      (lambda (x) (and (my-rec? x) (list 'ok (#3%record-instance? x (record-type-descriptor ,my-sub-rec)))))))

  ;; substituting `sealed-record-instance?`
  (cptypes-equivalent-expansion?
   `(let ()
      (define-record-type ,my-rec (fields a))
      (define-record-type ,my-sub-rec (fields a) (parent ,my-rec) (sealed #t))
      (lambda (x) (and (my-rec? x) (list 'ok (my-sub-rec? x)))))
   `(let ()
      (define-record-type ,my-rec (fields a))
      (define-record-type ,my-sub-rec (fields a) (parent ,my-rec) (sealed #t))
      (lambda (x) (and (my-rec? x) (list 'ok (#3%$sealed-record-instance? x (record-type-descriptor ,my-sub-rec)))))))

  ;; obviously incompatible rtds
  ;; the third pass is needed to eliminate #3%$value
  (parameterize ([run-cp0 (lambda (cp0 x) (cp0 (cp0 (cp0 x))))])
    (cptypes-equivalent-expansion?
     `(let ()
        (define-record I (a))
        (define A (make-record-type-descriptor 'a #f #f #f #f '(1 . 0)))
        (lambda (x) (and ((record-predicate A) x) (I? x))))
     `(begin
        (make-record-type-descriptor 'a #f #f #f #f '(1 . 0))
        (lambda (x) #f))))
)

(mat cptypes-lists
  (test-chain '(null? list-assuming-immutable? (lambda (x) (or (null? x) (pair? x)))))
  (test-chain* '(null? list? (lambda (x) (or (null? x) (pair? x)))))
  (cptypes-equivalent-expansion?
    '(lambda (x f) (when (list-assuming-immutable? x) (f) (list-assuming-immutable? x)))
    '(lambda (x f) (when (list-assuming-immutable? x) (f) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x f) (unless (list-assuming-immutable? x) (f) (list-assuming-immutable? x)))
    '(lambda (x f) (unless (list-assuming-immutable? x) (f) #f)))
  (not (cptypes-equivalent-expansion?
         '(lambda (x f) (when (list? x) (f) (list? x)))
         '(lambda (x f) (when (list? x) (f) #t))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x f) (unless (list? x) (f) (list? x)))
         '(lambda (x f) (unless (list? x) (f) #f))))
  (test-disjoint '(null? pair?))
  (not (test-disjoint* '(list? null?)))
  (not (test-disjoint* '(list? pair?)))
  (not (test-disjoint* '(list-assuming-immutable? null?)))
  (not (test-disjoint* '(list-assuming-immutable? pair?)))
  (not (test-disjoint* '(list-assuming-immutable? list?)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (list-assuming-immutable? x) (list-assuming-immutable? (cdr x))))
    '(lambda (x) (when (list-assuming-immutable? x) (cdr x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (list-assuming-immutable? x) (pair? x)) (list-assuming-immutable? (cdr x))))
    '(lambda (x) (when (and (list-assuming-immutable? x) (pair? x)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (list-assuming-immutable? x) (list-assuming-immutable? (cdr (error 'e "")))))
    '(lambda (x) (when (list-assuming-immutable? x) (error 'e ""))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (vector? x) (list-assuming-immutable? (#2%cdr x)) 1))
    '(lambda (x) (when (vector? x) (#2%cdr x))))
  (cptypes-equivalent-expansion?
    '(lambda (f) (define x '(1 2 3)) (f x) (list-assuming-immutable? x))
    '(lambda (f) (define x '(1 2 3)) (f x) #t))
  (cptypes-equivalent-expansion?
    '(lambda () (define x '(1 2 3)) (pair? x))
    '(lambda () (define x '(1 2 3)) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (vector? x) (#2%list->vector x) 1))
    '(lambda (x) (when (vector? x) (#2%list->vector x) 2)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (unless (or (null? x) (pair? x)) (#2%list->vector x) 1))
    '(lambda (x) (unless (or (null? x) (pair? x)) (#2%list->vector x) 2)))
  (cptypes-equivalent-expansion?
    '(lambda (x f) (#2%list->vector x) (f) (or (null? x) (pair? x)))
    '(lambda (x f) (#2%list->vector x) (f) #t))
  (not (cptypes-equivalent-expansion?
         '(lambda (x f) (list->vector x) (f) (list? x))
         '(lambda (x f) (list->vector x) (f) #t)))
)

(define (test-closed1 f* p?*)
  (let loop ([f* f*])
    (or (null? f*)
        (let ([f (car f*)])
          (and (let loop ([p?* p?*])
                 (or (null? p?*)
                     (let ([p? (car p?*)])
                       (and (cptypes-equivalent-expansion?
                              `(lambda (x) (when (,p? x) (,p? (,f x))))
                              `(lambda (x) (when (,p? x) (,f x) #t)))
                           (loop (cdr p?*))))))
               (loop (cdr f*)))))))

(mat cptypes-unsafe
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (pair? x) (car x)))
    '(lambda (x) (when (pair? x) (#3%car x))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (pair? x) (cdr x)))
    '(lambda (x) (when (pair? x) (#3%cdr x))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (pair? x) (#2%cadr x)))
         '(lambda (x) (when (pair? x) (#3%cadr x)))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (fxmax x y)))
    '(lambda (x y) (when (and (fixnum? x) (fixnum? y)) (#3%fxmax x y))))
  (cptypes-equivalent-expansion?
    '(lambda (x y) (when (and (fixnum? x) (eq? y 5)) (fxmax x y)))
    '(lambda (x y) (when (and (fixnum? x) (eq? y 5)) (#3%fxmax x y))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (fxmax x 5)))
    '(lambda (x) (when (fixnum? x) (#3%fxmax x 5))))
  (cptypes-equivalent-expansion?
    '(lambda (x y z) (when (and (fixnum? x) (fixnum? y) (fixnum? z)) (fxmax x y z)))
    '(lambda (x y z) (when (and (fixnum? x) (fixnum? y) (fixnum? z)) (#3%fxmax x y z))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (fxzero? x)))
    '(lambda (x) (when (fixnum? x) (#3%fxzero? x))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (number? x) (#2%odd? x)))
         '(lambda (x) (when (number? x) (#3%odd? x)))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (number? x) (#2%exact? x)))
    '(lambda (x) (when (number? x) (#3%exact? x))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (#2%exact? x))
         '(lambda (x) (#3%exact? x))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (fixnum? x)
                        (fixnum? (add1 x))))
         '(lambda (x) (when (fixnum? x)
                        #t))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (bignum? x)
                        (bignum? (add1 x))))
         '(lambda (x) (when (bignum? x)
                        #t))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (flonum? x)
                   (add1 x)))
    '(lambda (x) (when (flonum? x)
                   (#3%fl+ x 1.0))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (flonum? x)
                   (1+ x)))
    '(lambda (x) (when (flonum? x)
                   (#3%fl+ x 1.0))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (fixnum? x)
                        (fixnum? (sub1 x))))
         '(lambda (x) (when (fixnum? x)
                        #t))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (bignum? x)
                        (bignum? (sub1 x))))
         '(lambda (x) (when (bignum? x)
                        #t))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (flonum? x)
                   (sub1 x)))
    '(lambda (x) (when (flonum? x)
                   (#3%fl- x 1.0))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (flonum? x)
                   (1- x)))
    '(lambda (x) (when (flonum? x)
                   (#3%fl- x 1.0))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (flonum? x)
                   (-1+ x)))
    '(lambda (x) (when (flonum? x)
                   (#3%fl- x 1.0))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (fixnum? x)
                        (fixnum? (abs x))))
         '(lambda (x) (when (fixnum? x)
                        #t))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x)
                   (abs x)))
    '(lambda (x) (when (fixnum? x)
                   (let ([t x])
                     (if (#3%fx= t (most-negative-fixnum))
                         (pariah (- (most-negative-fixnum)))
                         (#3%fxabs t))))))
  (cptypes-equivalent-expansion? ; unexpected, but correct
    '(lambda (x) (when (bignum? x)
                   (bignum? (abs x))))
    '(lambda (x) (when (bignum? x)
                   #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (flonum? x)
                   (abs x)))
    '(lambda (x) (when (flonum? x)
                   (#3%flabs x))))
  (test-closed1 '(add1 1+ sub1 1- -1+ abs)
                '(flonum? real? (lambda (x) (and (integer? x) (exact? x)))))
)

(mat cptypes-rest-argument
  (cptypes/nocp0-equivalent-expansion?
    '((lambda (x . r) (pair? r)) 1)
    '((lambda (x . r) #f) 1))
  (cptypes/nocp0-equivalent-expansion?
    '((lambda (x . r) (null? r)) 1)
    '((lambda (x . r) #t) 1))
  (cptypes/nocp0-equivalent-expansion?
    '((lambda (x . r) (pair? r)) 1 2)
    '((lambda (x . r) #t) 1 2))
  (cptypes/nocp0-equivalent-expansion?
    '((lambda (x . r) (null? r)) 1 2)
    '((lambda (x . r) #f) 1 2))
)

(mat cptypes-delay
  (cptypes-equivalent-expansion?
    '(lambda (b) (map (lambda (x) (box? b)) (unbox b)))
    '(lambda (b) (map (lambda (x) #t) (unbox b))))
  (cptypes-equivalent-expansion?
    '(lambda (b) (list (lambda (x) (box? b)) (unbox b)))
    '(lambda (b) (list (lambda (x) #t) (unbox b))))
  (cptypes-equivalent-expansion?
    '(lambda (b) (list (unbox b) (lambda (x) (box? b))))
    '(lambda (b) (list (unbox b) (lambda (x) #t))))
)

(mat cptypes-call-with-values
  ; The single value case is handled by cp0
  (cptypes-equivalent-expansion?
    '(lambda (v)
       (call-with-values
        (lambda () (vector-ref v 0))
        (lambda (y) (list (vector? v) (vector-ref v 1) y))))
    '(lambda (v)
       (call-with-values
        (lambda () (vector-ref v 0))
        (lambda (y) (list #t (vector-ref v 1) y)))))
  (cptypes-equivalent-expansion?
    '(lambda (t)
       (call-with-values
        (lambda () (if t (box 2) (box 3)))
        (lambda (y) (list y (box? y)))))
    '(lambda (t)
       (call-with-values
        (lambda () (if t (box 2) (box 3)))
        (lambda (y) (list y #t)))))
  (cptypes-equivalent-expansion?
    '(lambda (t b)
       (call-with-values
        (lambda () (if t 1 2))
        (lambda (y) (display (unbox b))))
       (box? b))
    '(lambda (t b)
       (call-with-values
        (lambda () (if t 1 2))
        (lambda (y) (display (unbox b))))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (b)
       (call-with-values
        (lambda () (if (unbox b) 1 2))
        (lambda (y) (display y)))
       (box? b))
    '(lambda (b)
       (call-with-values
        (lambda () (if (unbox b) 1 2))
        (lambda (y) (display y)))
       #t))

  (cptypes-equivalent-expansion?
    '(lambda (b)
       (call-with-values
        (lambda () (if (unbox b) 1 (values 2 3)))
        (lambda (x y) (list x y (box? b)))))
    '(lambda (b)
       (call-with-values
        (lambda () (if (unbox b) 1 (values 2 3)))
        (lambda (x y) (list x y #t)))))
  (cptypes-equivalent-expansion?
    '(lambda (t b)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
        (lambda (x y) (display (list x y (unbox b)))))
       (box? b))
    '(lambda (t b)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
        (lambda (x y) (display (list x y (unbox b)))))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (b)
       (call-with-values
        (lambda () (if (unbox b) 1 (values 2 3)))
        (lambda (x y) (display (list x y))))
       (box? b))
    '(lambda (b)
       (call-with-values
        (lambda () (if (unbox b) 1 (values 2 3)))
        (lambda (x y) (display (list x y))))
       #t))

  (cptypes-equivalent-expansion?
    '(lambda (b)
       (call-with-values
        (case-lambda 
          [() (if (unbox b) 1 (values 2 3))]
          [(x) (error 'e "")])
        (lambda (x y) (list x y (box? b)))))
    '(lambda (b)
       (call-with-values
        (case-lambda 
          [() (if (unbox b) 1 (values 2 3))]
          [(x) (error 'e "")])
        (lambda (x y) (list x y #t)))))
  (cptypes-equivalent-expansion?
    '(lambda (t b)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
        (case-lambda
         [(x y) (display (list x y (unbox b)))]
         [(x) (error 'e "")]))
       (box? b))
    '(lambda (t b)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
        (case-lambda
         [(x y) (display (list x y (unbox b)))]
         [(x) (error 'e "")]))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (b)
       (call-with-values
        (case-lambda 
          [() (if (unbox b) 1 (values 2 3))]
          [(x) (error 'e "")])
        (lambda (x y) (display (list x y))))
       (box? b))
    '(lambda (b)
       (call-with-values
        (case-lambda 
          [() (if (unbox b) 1 (values 2 3))]
          [(x) (error 'e "")])
        (lambda (x y) (display (list x y))))
       #t))

  (cptypes-equivalent-expansion?
    '(lambda (t b)
       (call-with-values
        (begin (display (unbox b)) (lambda () (if t 1 (values b 2))))
        (lambda (x y) (list x y (box? b)))))
    '(lambda (t b)
       (call-with-values
        (begin (display (unbox b)) (lambda () (if t 1 (values b 2))))
        (lambda (x y) (list x y #t)))))
  ; This is difficult to handle in cptypes, so I ignored it.
  ; But it is anyway handled by cp0.
  #;(cptypes-equivalent-expansion?
      '(lambda (t b)
         (call-with-values
          (lambda () (if t 1 (values b (box? b))))
          (begin (display (unbox b)) (lambda (x y) (list x y b)))))
      '(lambda (t b)
         (call-with-values
          (lambda () (if t 1 (values b #t)))
          (begin (display (unbox b)) (lambda (x y) (list x y b))))))

  (cptypes-equivalent-expansion?
    '(lambda (t)
       (number?
        (call-with-values
         (lambda () (if t 1 (values 2 3)))
         (case-lambda [(x y) 2] [(x) 1]))))
    '(lambda (t)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
        (case-lambda [(x y) 2] [(x) 1]))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (t)
       (number?
        (call-with-values
         (lambda () (if t 1 (values 2 3)))
         (case-lambda [(x y) 2] [(x) (error 'e "")]))))
    '(lambda (t)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
        (case-lambda [(x y) 2] [(x) (error 'e "")]))
       #t))

  (cptypes-equivalent-expansion?
    '(lambda (t f)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
         f)
       (procedure? f))
    '(lambda (t f)
       (call-with-values
        (lambda () (if t 1 (values 2 3)))
         f)
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (t f)
       (call-with-values
        f
        (lambda (x y) (+ x y)))
       (procedure? f))
    '(lambda (t f)
       (call-with-values
        f
        (lambda (x y) (+ x y)))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (t f)
       (when (box? f)
         (call-with-values
          (lambda () (if t 1 (values 2 3)))
          f)
         111))
    '(lambda (t f)
       (when (box? f)
         (call-with-values
          (lambda () (if t 1 (values 2 3)))
          f)
         222)))
  (cptypes-equivalent-expansion?
    '(lambda (t f)
       (when (box? f)
         (call-with-values
          f
          (lambda (x y) (+ x y)))
         111))
    '(lambda (t f)
       (when (box? f)
         (call-with-values
          f
          (lambda (x y) (+ x y)))
         222)))
)

(mat cptypes-apply
  (cptypes-equivalent-expansion?
    '(lambda (l b)
       (apply (lambda (x) (display (list (unbox b) x))) l)
       (box? b))
    '(lambda (l b)
       (apply (lambda (x) (display (list (unbox b) x))) l)
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (l b)
       (apply (lambda (x y) (display (list (unbox b) x))) 7 l)
       (box? b))
    '(lambda (l b)
       (apply (lambda (x y) (display (list (unbox b) x))) 7 l)
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (l b)
       (apply (lambda (x) (display (list b x))) (unbox b))
       (box? b))
    '(lambda (l b)
       (apply (lambda (x) (display (list b x))) (unbox b))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (l b)
       (apply (lambda (x y) (display (list b x y))) 7 (unbox b))
       (box? b))
    '(lambda (l b)
       (apply (lambda (x y) (display (list b x y))) 7 (unbox b))
       #t))

  (cptypes-equivalent-expansion?
    ; with #3% the argument may be inlined and then executed in reverse order
    '(lambda (l b)
       (#2%apply (lambda (x y) (list (box? b) x y)) 7 (unbox b)))
    '(lambda (l b)
       (#2%apply (lambda (x y) (list #t x y)) 7 (unbox b))))

  (cptypes-equivalent-expansion?
    '(lambda (l b)
       (apply
        (case-lambda 
          [(x) (list (unbox b) x)]
          [(x y) (error 'e "")])
        l)
       (box? b)) 
    '(lambda (l b)
       (apply
        (case-lambda 
          [(x) (list (unbox b) x)]
          [(x y) (error 'e "")])
        l)
       #t)) 

  (cptypes-equivalent-expansion?
    '(lambda (l)
       (number?
        (apply (lambda (x y) (+ x y)) l)))
    '(lambda (l)
       (apply (lambda (x y) (+ x y)) l)
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (l)
       (number?
        (apply
         (case-lambda
          [(x y) (+ x y)]
          [()  (error 'e "")])
         l)))
    '(lambda (l)
       (apply
        (case-lambda
         [(x y) (+ x y)]
         [()  (error 'e "")])
        l)
       #t))

  (cptypes-equivalent-expansion?
    '(lambda (f l)
       (apply f l)
       (procedure? f))
    '(lambda (f l)
       (apply f l)
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (t f)
       (when (box? f)
         (apply f l)
         111))
    '(lambda (t f)
       (when (box? f)
         (apply f l)
         222)))
)

(mat cptypes-dynamic-wind
  (cptypes-equivalent-expansion?
    '(lambda (f)
       (box? (dynamic-wind (lambda (x) #f) (lambda () (box (f))) (lambda () #f))))
    '(lambda (f)
       (begin
         (dynamic-wind (lambda (x) #f) (lambda () (box (f))) (lambda () #f))
         #t)))

  (cptypes-equivalent-expansion?
    '(lambda (b)
       (dynamic-wind (lambda (x) (unbox b)) (lambda () #f) (lambda () #f))
       (box? b))
    '(lambda (b)
       (dynamic-wind (lambda (x) (unbox b)) (lambda () #f) (lambda () #f))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (b)
       (dynamic-wind (lambda (x) #f) (lambda () (unbox b)) (lambda () #f))
       (box? b))
    '(lambda (b)
       (dynamic-wind (lambda (x) #f) (lambda () (unbox b)) (lambda () #f))
       #t))
  (cptypes-equivalent-expansion?
    '(lambda (b)
       (dynamic-wind (lambda (x) #f) (lambda () #f) (lambda () (unbox b)))
       (box? b))
    '(lambda (b)
       (dynamic-wind (lambda (x) #f) (lambda () #f) (lambda () (unbox b)))
       #t))

  (cptypes-equivalent-expansion?
    '(lambda (b)
       (dynamic-wind (lambda (x) (unbox b)) (lambda () (box? b)) (lambda () #f)))
    '(lambda (b)
       (dynamic-wind (lambda (x) (unbox b)) (lambda () #t) (lambda () #f))))
  (cptypes-equivalent-expansion?
    '(lambda (b)
       (dynamic-wind (lambda (x) (unbox b)) (lambda () #f) (lambda () (box? b))))
    '(lambda (b)
       (dynamic-wind (lambda (x) (unbox b)) (lambda () #f) (lambda () #t) )))
  (not (cptypes-equivalent-expansion?
         '(lambda (b)
            (dynamic-wind (lambda () #f) (lambda (x) (unbox b)) (lambda () (box? b))))
         '(lambda (b)
            (dynamic-wind (lambda () #f) (lambda (x) (unbox b)) (lambda () #t)))))
)

(mat cptypes-result-type
  ; test the special case for predicates
  (cptypes-equivalent-expansion?
    '(number? (optimize-level))
    '(begin (optimize-level) #t))
  (cptypes-equivalent-expansion?
      '(eq? (optimize-level 0) (void))
      '(begin (optimize-level 0) #t))
  (cptypes-equivalent-expansion?
    '(number? (optimize-level 0))
    '(begin (optimize-level 0) #f))
  (parameterize ([optimize-level 0])
    (eq? (optimize-level 0) (void)))
)

(mat cptypes-drop
  (cptypes/once-equivalent-expansion?
    '(pair? (list 1 (display 2) 3))
    '(begin (display 2) #t))
  (cptypes/once-equivalent-expansion?
    '(vector? (list 1 (display 2) 3))
    '(begin (display 2) #f))
  (cptypes/once-equivalent-expansion?
    '(pair? (list 1 (vector 2 (display 3) 4)))
    '(begin (display 3) #t))
  (cptypes/once-equivalent-expansion?
    '(vector? (list 1 (vector 2 (display 3) 4)))
    '(begin (display 3) #f))
  ; regression test: check that the compiler doesn't loop forever
  ; when the return arity is unknown
  (cptypes-equivalent-expansion?
    '(lambda (f) (box? (box (f))))
    '(lambda (f) (#3%$value (f)) #t))
)

(mat cptypes-store-immediate
  (cptypes-equivalent-expansion?
   '(lambda (v)
      (let loop ([i 0])
        (when (fx< i (vector-length v))
          (vector-set! v i i)
          (loop (fx+ i 1)))))
   '(lambda (v)
      (let loop ([i 0])
        (when (fx< i (vector-length v))
          (vector-set! v i (#3%$fixmediate i))
          (loop (fx+ i 1))))))
  (cptypes-equivalent-expansion?
   '(lambda (x y) (set-box! x (if (vector? y) #t (error 't))))
   '(lambda (x y) (set-box! x (#3%$fixmediate (if (vector? y) #t (error 't))))))
)

(mat cptypes-maybe
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (not x) (vector? x)) (box? x)))
    '(lambda (x) (when (or (not x) (vector? x)) #f)))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (or (not x) (vector? x)) (vector? x)))
         '(lambda (x) (when (or (not x) (vector? x)) #t))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (not x) (vector? x)) (when x (vector? x))))
    '(lambda (x) (when (or (not x) (vector? x)) (when x #t))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (not x) (char? x)) (when x (char? x))))
    '(lambda (x) (when (or (not x) (char? x)) (when x #t))))
  (cptypes-equivalent-expansion?
    '(lambda (s) (define x (string->number s)) (when x (number? x)))
    '(lambda (s) (define x (string->number s)) (when x #t)))
  (cptypes-equivalent-expansion?
    '(lambda (s) (define x (string->number s)) (unless (number? x) x))
    '(lambda (s) (define x (string->number s)) (unless (number? x) #f)))
  (cptypes-equivalent-expansion?
    '(lambda (s) (define x (string->number s)) (unless (eq? x #f) (number? x)))
    '(lambda (s) (define x (string->number s)) (unless (eq? x #f) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (s) (define x (string->number s)) (unless (number? x) (eq? x #f)))
    '(lambda (s) (define x (string->number s)) (unless (number? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-char p)) (not x))
    '(lambda (p) (define x (get-char p)) #f))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-char p)) (box? x))
    '(lambda (p) (define x (get-char p)) #f))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-char p)) (unless (eof-object? x) (char? x)))
    '(lambda (p) (define x (get-char p)) (unless (eof-object? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-char p)) (unless (char? x) (eof-object? x)))
    '(lambda (p) (define x (get-char p)) (unless (char? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-u8 p)) (when (number? x) (fixnum? x)))
    '(lambda (p) (define x (get-u8 p)) (when (number? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-u8 p)) (unless (eof-object? x) (fixnum? x)))
    '(lambda (p) (define x (get-u8 p)) (unless (eof-object? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-u8 p)) (unless (fixnum? x) (eof-object? x)))
    '(lambda (p) (define x (get-u8 p)) (unless (fixnum? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-u8 p)) (unless (eq? x (eof-object)) (fixnum? x)))
    '(lambda (p) (define x (get-u8 p)) (unless (eq? x (eof-object)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (p) (define x (get-u8 p)) (unless (fixnum? x) (eq? x (eof-object))))
    '(lambda (p) (define x (get-u8 p)) (unless (fixnum? x) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (unless (char? x) (char? x)))
    '(lambda (x) (unless (char? x) #f)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (char? x) (eq? x #\A)) (char? x)))
    '(lambda (x) (when (and (char? x) (eq? x #\A)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (char? x) (eq? x #\A)) (eq? x #\A)))
    '(lambda (x) (when (and (char? x) (eq? x #\A)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (char? x) (eq? x #\A)) (char? x)))
    '(lambda (x) (when (or (char? x) (eq? x #\A)) #t)))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (or (char? x) (eq? x #\A)) (eq? x #\A)))
         '(lambda (x) (when (or (char? x) (eq? x #\A)) #t))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (char? x) (unless (eq? x #\A) (char? x))))
    '(lambda (x) (when (char? x) (unless (eq? x #\A) #t))))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (when (char? x) (unless (eq? x #\A) (eq? x #\A))))
         '(lambda (x) (when (char? x) (unless (eq? x #\A) #t)))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (eq? x #\A) (unless (char? x) 1)))
    '(lambda (x) (when (eq? x #\A) (unless (char? x) 2))))
)

(mat exact-integer
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) (or (fixnum? x) (bignum? x))))
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) (or (bignum? x) (fixnum? x))))
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) (and (integer? x) (exact? x))))
    '(lambda (x) (when (or (fixnum? x) (bignum? x)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (integer? x) (exact? x)) (or (fixnum? x) (bignum? x))))
    '(lambda (x) (when (and (integer? x) (exact? x)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (fixnum? x) (eq? x 7)) (fixnum? x)))
    '(lambda (x) (when (or (fixnum? x) (eq? x 7)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (or (eq? x 7) (fixnum? x)) (fixnum? x)))
    '(lambda (x) (when (or (eq? x 7) (fixnum? x)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (fixnum? x) (eq? x 7)) (eq? x 7)))
    '(lambda (x) (when (and (fixnum? x) (eq? x 7)) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (and (eq? x 7) (fixnum? x)) (eq? x 7)))
    '(lambda (x) (when (and (eq? x 7) (fixnum? x)) #t)))
  ; check that non-fixnum? is not lost
  (cptypes-equivalent-expansion?
    '(lambda (x) (unless (fixnum? x) (unless (fixnum? x) x)))
    '(lambda (x) (unless (fixnum? x) x)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (real? x) (unless (fixnum? x) (unless (fixnum? x) x))))
    '(lambda (x) (when (real? x) (unless (fixnum? x) x))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (unless (fixnum? x) (when (real? x) (unless (fixnum? x) x))))
    '(lambda (x) (unless (fixnum? x) (when (real? x) x))))
)

(mat bitwise
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (bitwise-and x 7)))
    '(lambda (x) (when (fixnum? x) (fxand x 7))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (bitwise-ior x 7)))
    '(lambda (x) (when (fixnum? x) (fxior x 7))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (bitwise-xor x 7)))
    '(lambda (x) (when (fixnum? x) (fxxor x 7))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (when (fixnum? x) (bitwise-not x)))
    '(lambda (x) (when (fixnum? x) (fxnot x))))
  (cptypes-equivalent-expansion?
    '(lambda (x) (fixnum? (bitwise-and x 7)))
    '(lambda (x) (bitwise-and x 7) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (fixnum? (bitwise-and x (most-positive-fixnum))))
    '(lambda (x) (bitwise-and x (most-positive-fixnum)) #t))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (fixnum? (bitwise-and x -7)))
         '(lambda (x) (bitwise-and x -7) #t)))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (fixnum? (bitwise-and x (add1 (most-positive-fixnum)))))
         '(lambda (x) (bitwise-and x (add1 (most-positive-fixnum))) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (x) (fixnum? (bitwise-ior x -7)))
    '(lambda (x) (bitwise-ior x -7) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (fixnum? (bitwise-ior x (most-negative-fixnum))))
    '(lambda (x) (bitwise-ior x (most-negative-fixnum)) #t))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (fixnum? (bitwise-ior x 7)))
         '(lambda (x) (bitwise-ior x 7) #t)))
  (not (cptypes-equivalent-expansion?
         '(lambda (x) (fixnum? (bitwise-ior x (sub1 (most-negative-fixnum)))))
         '(lambda (x) (bitwise-ior x (sub1 (most-negative-fixnum))) #t)))
  (cptypes-equivalent-expansion?
    '(lambda (f x) (when (fixnum? x) (bitwise-and (f) x)))
    '(lambda (f x) (when (fixnum? x)
                     (let ([y (f)])
                       (if (#2%fixnum? y) ;the specialization uses #2%fixnum?
                           (fxand y x)
                           (bitwise-and y x))))))
  (cptypes-equivalent-expansion?
    '(lambda (f x) (when (fixnum? x) (bitwise-and x (f))))
    '(lambda (f x) (when (fixnum? x)
                     (let ([y (f)])
                       (if (#2%fixnum? y) ;the specialization uses #2%fixnum?
                           (fxand x y)
                           (bitwise-and x y))))))
)

(mat cptypes-unreachable
  (cptypes-equivalent-expansion?
   '(lambda (x) (if (pair? x) (car x) (#3%assert-unreachable)))
   '(lambda (x) (#3%car x)))
  (not
   (cptypes-equivalent-expansion?
    '(lambda (x) (if (pair? x) (car x) (#2%assert-unreachable)))
    '(lambda (x) (#3%car x))))
)

(mat cptypes-bottom
  (cptypes-equivalent-expansion?
   '(lambda (x) (error 'x "no") (add1 x))
   '(lambda (x) (error 'x "no")))
  (parameterize ([debug-level 2])
    (cptypes-equivalent-expansion?
     '(lambda (x) (error 'x "no") (add1 x))
     '(lambda (x) (error 'x "no") (void))))
  (parameterize ([debug-level 2])
    (not
     (cptypes-equivalent-expansion?
      '(lambda (x) (error 'x "no") (add1 x))
      '(lambda (x) (error 'x "no")))))
  (parameterize ([debug-level 2])
    (not
     (cptypes-equivalent-expansion?
      '(lambda (x) (rationalize "no") (add1 x))
      '(lambda (x) (rationalize "no")))))
  (parameterize ([debug-level 2])
    (not
     (cptypes-equivalent-expansion?
      '(lambda (x) (+ 1 "no") (add1 x))
      '(lambda (x) (+ 1 "no")))))
  (cptypes-equivalent-expansion?
   '(lambda (f) (f (error 'x "no") f))
   '(lambda (f) (error 'x "no")))
  (cptypes-equivalent-expansion?
   '(lambda (f) ((error 'x "no") f f))
   '(lambda (f) (error 'x "no")))
  (cptypes-equivalent-expansion?
   '(lambda (x) (if (error 'x "no") (add1 x) (sub1 x)))
   '(lambda (x) (error 'x "no")))
  (parameterize ([debug-level 2])
    (not
     (cptypes-equivalent-expansion?
      '(lambda (x) (if (error 'x "no") (add1 x) (sub1 x)))
      '(lambda (x) (error 'x "no")))))
  (cptypes-equivalent-expansion?
   '(lambda (x) (+ (error 'x "no") x))
   '(lambda (x) (error 'x "no")))
  (cptypes-equivalent-expansion?
   '(lambda (x) (list x (add1 x) (error 'x "no") (sub1 x)))
   '(lambda (x) (error 'x "no")))
  (cptypes-equivalent-expansion?
   '(lambda (x) (apply x (add1 x) (error 'x "no") (sub1 x)))
   '(lambda (x) (error 'x "no")))
  (cptypes-equivalent-expansion?
   '(lambda (x) (apply (error 'x "no") (add1 x) (sub1 x)))
   '(lambda (x) (error 'x "no")))
  (cptypes-equivalent-expansion?
   '(lambda (x) (let* ([x (add1 x)] [y (error 'x "no")]) (+ x y)))
   '(lambda (x) (add1 x) (error 'x "no")))
  (cptypes-equivalent-expansion?
   '(lambda (x) (list (if (odd? x) (error 'x "no") (error 'x "nah")) 17))
   '(lambda (x) (if (odd? x) (error 'x "no") (error 'x "nah"))))
  (cptypes-equivalent-expansion?
   '(let ([x #f]) (case-lambda [() x] [(y) (set! x (error 'x "no"))]))
   '(let ([x #f]) (case-lambda [() x] [(y) (error 'x "no")])))
  (parameterize ([debug-level 2])
    (cptypes-equivalent-expansion?
     '(let ([x #f]) (case-lambda [() x] [(y) (set! x (error 'x "no"))]))
     '(let ([x #f]) (case-lambda [() x] [(y) (error 'x "no") (void)]))))
  (parameterize ([debug-level 2])
    (not
     (cptypes-equivalent-expansion?
      '(let ([x #f]) (case-lambda [() x] [(y) (set! x (error 'x "no"))]))
      '(let ([x #f]) (case-lambda [() x] [(y) (error 'x "no")])))))

  (cptypes-equivalent-expansion?
   '(lambda (x) (if x (x) (error 'x "no")))
   '(lambda (x) (if x (void) (error 'x "no")) (x)))
  (parameterize ([debug-level 2])
    (not
     (cptypes-equivalent-expansion?
      '(lambda (x) (if x (x) (error 'x "no")))
      '(lambda (x) (if x (void) (error 'x "no")) (x)))))

  (cptypes-equivalent-expansion?
   '(lambda (x) (+ (#%$call-setting-continuation-attachment 'a (lambda () (error 'x "no ~s" a))) 1))
   '(lambda (x) (#%$value (#%$call-setting-continuation-attachment 'a (lambda () (error 'x "no ~s" a))))))
  (cptypes-equivalent-expansion?
   '(lambda (x) (if (#%$call-setting-continuation-attachment 'a (lambda () (error 'x "no ~s" a))) 1))
   '(lambda (x) (#%$value (#%$call-setting-continuation-attachment 'a (lambda () (error 'x "no ~s" a))))))
  (not
   (cptypes-equivalent-expansion?
    '(lambda (x) (+ (#%$call-setting-continuation-attachment 'a (lambda () (error 'x "no ~s" a))) 1))
    '(lambda (x) (#%$call-setting-continuation-attachment 'a (lambda () (error 'x "no ~s" a))))))
  (cptypes-equivalent-expansion?
   '(lambda (x) (+ (#%$call-getting-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))) 1))
   '(lambda (x) (#%$value (#%$call-getting-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))))))
  (not
   (cptypes-equivalent-expansion?
    '(lambda (x) (+ (#%$call-getting-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))) 1))
    '(lambda (x) (#%$call-getting-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))))))
  (cptypes-equivalent-expansion?
   '(lambda (x) (+ (#%$call-consuming-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))) 1))
   '(lambda (x) (#%$value (#%$call-consuming-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))))))
  (not
   (cptypes-equivalent-expansion?
    '(lambda (x) (+ (#%$call-consuming-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))) 1))
    '(lambda (x) (#%$call-consuming-continuation-attachment 'a (lambda (a) (error 'x "no ~s" a))))))
  (parameterize ([optimize-level 2])
    (cptypes-equivalent-expansion?
     '(lambda (p) (car p) (vector-ref p 0) (oops))
     '(lambda (p) (car p) (vector-ref p 0))))
)

(mat cptypes-boxes
  (test-chain* '((lambda (x) (eq? x '#&7)) box?))
  (cptypes-equivalent-expansion?
   '(lambda (x) (when (or (eq? x '#&7) (eq? x '#&8)) (box? x)))
   '(lambda (x) (when (or (eq? x '#&7) (eq? x '#&8)) #t)))
  (cptypes-equivalent-expansion?
   '(lambda (x) (set-box! x 7) (box? x))
   '(lambda (x) (set-box! x 7) #t))
)

(define (test-null-vector vec? null-vec other-vec vec-set! val)
  (and (test-chain `((lambda (x) (eq? x ',null-vec)) ,vec?))
       (test-chain* `((lambda (x) (eq? x ',other-vec)) ,vec?))
       (test-disjoint* `((lambda (x) (eq? x ',null-vec)) (lambda (x) (eq? x ',other-vec))))
       (cptypes-equivalent-expansion?
        `(lambda (x) (when (or (eq? x ',null-vec) (eq? x ',other-vec)) (,vec? x)))
        `(lambda (x) (when (or (eq? x ',null-vec) (eq? x ',other-vec)) #t)))
       (cptypes-equivalent-expansion?
        `(lambda (x) (when (or (eq? x ',other-vec) (eq? x ',null-vec)) (,vec? x)))
        `(lambda (x) (when (or (eq? x ',other-vec) (eq? x ',null-vec)) #t)))
       (cptypes-equivalent-expansion?
        `(lambda (x) (,vec-set! x 0 ,val) (,vec? x))
        `(lambda (x) (,vec-set! x 0 ,val) #t))
       (cptypes-equivalent-expansion?
        `(lambda (x) (,vec-set! x 0 ,val) (eq? x ',null-vec))
        `(lambda (x) (,vec-set! x 0 ,val) #f))))

(mat cptypes-null-vectors
  (test-null-vector 'vector? '#() '#(7) 'vector-set! 7)
  (test-null-vector 'vector? (vector->immutable-vector '#()) (vector->immutable-vector '#(7)) 'vector-set! 7)
  (test-null-vector 'string? "" "7" 'string-set! #\7)
  (test-null-vector 'string? (string->immutable-string "") (string->immutable-string "7") 'string-set! #\7)
  (test-null-vector 'bytevector? '#vu8() '#vu8(7) 'bytevector-u8-set! 7)
  (test-null-vector 'bytevector? (bytevector->immutable-bytevector '#vu8()) (bytevector->immutable-bytevector '#vu8(7)) 'bytevector-u8-set! 7)
  (test-null-vector 'bytevector? '#vu8() '#vu8(249) 'bytevector-s8-set! -7)
  (test-null-vector 'bytevector? (bytevector->immutable-bytevector '#vu8()) (bytevector->immutable-bytevector '#vu8(249)) 'bytevector-s8-set! -7)
  (test-null-vector 'fxvector? '#vfx() '#vfx(7) 'fxvector-set! 7)
  (test-null-vector 'flvector? '#vfl() '#vfl(7.0) 'flvector-set! 7.0)

  (test-disjoint '((lambda (x) (eq? x '#()))
                   (lambda (x) (eq? x (vector->immutable-vector '#())))
                   (lambda (x) (eq? x ""))
                   (lambda (x) (eq? x (string->immutable-string "")))
                   (lambda (x) (eq? x #vu8()))
                   (lambda (x) (eq? x (bytevector->immutable-bytevector #vu8())))
                   (lambda (x) (eq? x #vfx()))
                   (lambda (x) (eq? x #vfl()))))

  ; check that equivalent-expansion? is comparing flvectors correctly
  (equivalent-expansion? #vfl(7.0) #vfl(7.0))
  (equivalent-expansion? #vfl(+nan.0) #vfl(+nan.0))
  (not (equivalent-expansion? #vfl(0.0) #vfl(-0.0)))
)
