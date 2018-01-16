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
    '(begin (let ([y (vector 1 2 3)]) (display (list y y)) y) #t))
  (cptypes-equivalent-expansion?
    '(vector? (let ([y (vector 1 2 3)]) (display (list y y)) (vector 4 5 6)))
    '(begin (let ([y (vector 1 2 3)]) (display (list y y)) (vector 4 5 6)) #t))
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
    '(lambda (x) (vector-set! (box 5) 0 0) 1)
    '(lambda (x) (vector-set! (box 5) 0 0) 2))
  (cptypes-equivalent-expansion?
    '(lambda (x) (#2%odd? x) (real? x))
    '(lambda (x) (#2%odd? x) #t))
  (cptypes-equivalent-expansion?
    '(lambda (x) (vector-set! x 0 0) (#2%odd? x) 1)
    '(lambda (x) (vector-set! x 0 0) (#2%odd? x) 2))
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
    '(lambda (t) (let ([x (if t (begin (newline) #f) #f)]) (number? x)))
    '(lambda (t) (let ([x (if t (begin (newline) #f) #f)]) #f)))
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
  (test-chain '((lambda (x) (eq? x 0)) fixnum? #;exact-integer? real? number?))
  (test-chain* '(fixnum? integer? real?))
  (test-chain* '(fixnum? exact? number?)) ; exact? may raise an error
  (test-chain* '((lambda (x) (eq? x (expt 256 100))) real? number?)) ; bignum?
  (test-chain '((lambda (x) (eqv? 0.0 x)) flonum? real? number?))
  (test-chain '(gensym? symbol?))
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
  (test-disjoint* '(list? record? vector?))
  (not (test-disjoint* '(list? null?)))
  (not (test-disjoint* '(list? pair?)))
)

; use a gensym to make expansions equivalent
(define my-rec (gensym "my-rec"))
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
)

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
)
