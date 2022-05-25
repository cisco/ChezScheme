;;; 5-1.ms
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

(mat eq?
    (eq? 'a 'a)
    (let ((x 203840238409238402384)) (eq? x x))
    (let ((x (cons 3 4))) (eq? x x))
    (not (eq? "hi there" (string-append "hi " "there")))
    (not (eq? (cons '() '()) (cons '() '())))
 )

(mat eqv?
    (eqv? 'a 'a)
    (not (eqv? '(a b (c)) "hi"))
    (not (eqv? '(a b (c)) (list 'a 'b '(c))))
    (not (eqv? 3.4 3.5))
    (eqv? 3.4 3.4)
    (eqv? 3/4 3/4)
    (not (eqv? 3/4 4/5))
    (not (eqv? 2.0 2))
    (not (eqv? 4.5 9/2))
    (eqv? 123124211123 123124211123)
    (not (eqv? 123124211123 123124211124))
    (not (eqv? "hi there" (string-append "hi " "there")))
    (not (eqv? (vector 1 2 (vector 3 4) 5) '#(1 2 #(3 4) 5)))
    (eqv? +nan.0 +nan.0)
    (eqv? +inf.0 +inf.0)
    (eqv? -inf.0 -inf.0)
    (not (eqv? -inf.0 +inf.0))
    (eqv? +0.0 +0.0)
    (eqv? -0.0 -0.0)
    (not (eqv? +0.0 -0.0))
    (eqv? 3.0+0.0i 3.0+0.0i)
    (eqv? 3.0-0.0i 3.0-0.0i)
    (not (eqv? 3.0+0.0i 3.0-0.0i))
    (not (eqv? 3.0+0.0i 3.0))
    (not (eqv? 3.0 3))
    (not (eqv? 3.0+4.0i 3+4i))
    (not (eqv? 3 3.0))
    (not (eqv? 3+4i 3.0+4.0i))
 )

(mat equal?
    (equal? 'a 'a)
    (not (equal? '(a b (c)) "hi"))
    (equal? '(a b (c)) (list 'a 'b '(c)))
    (not (equal? '(a b (c)) '(a b (d))))
    (equal? 123124211123 123124211123)
    (not (equal? 123124211123 123124211124))
    (equal? "hi there" (string-append "hi " "there"))
    (not (equal? "hi there " "hi there"))
    (equal? (vector 1 2 (vector 3 4) 5) '#(1 2 #(3 4) 5))
    (not (equal? (vector 1 2 (vector 3 4) 5) '#(1 2 3 4 5)))
    (equal? +nan.0 +nan.0)
    (equal? +inf.0 +inf.0)
    (equal? -inf.0 -inf.0)
    (not (equal? -inf.0 +inf.0))
    (equal? +0.0 +0.0)
    (equal? -0.0 -0.0)
    (not (equal? +0.0 -0.0))
    (equal? 3.0+0.0i 3.0+0.0i)
    (equal? 3.0-0.0i 3.0-0.0i)
    (not (equal? 3.0+0.0i 3.0-0.0i))
    (not (equal? 3.0+0.0i 3.0))
    (not (equal? 3.0 3))
    (not (equal? 3.0+4.0i 3+4i))
    (not (equal? 3 3.0))
    (not (equal? 3+4i 3.0+4.0i))
 )

(mat new-equal? ; includes dag and cycle checks
  (time (equal? '(a b c) '(a b c)))
  (equal? '#1=(a b c . #1#) '#2=(a b c . #2#))
  (not (equal? '#3=(a b c . #3#) '#4=(a b . #4#)))
  (equal? '#5=(a b c . #5#) '#6=(a b c a b c . #6#))
  (equal? '#7=(a b c . #7#) '(a b c a b c . #7#))
  (not (equal? '#8=(a b c . #8#) '#9=(a b c a c . #9#)))
  (andmap eq?
    (let ([ls1 '#10=(a #10# c #10# d #11# f)]
          [ls2 '#11=(a (a #11# c #10# d #11# f) c #10# d #11# f)])
      (list (equal? ls1 ls1)
            (equal? ls2 ls2)
            (equal? ls1 ls2)
            (equal? ls2 ls1)
            (equal? (cadr ls1) ls2)
            (equal? (cons 'g ls1) ls1)
            (equal? (append ls1 '(g)) ls1)
            (equal? (cdr ls1) (cdddr ls1))
            (equal? (cdr ls1) (cdr (cadr ls2)))))
    '(#t #t #t #t #t #f #f #f #t))
  (andmap eq?
    (let ([leaf1 (list "As a tree, I am huge.")]
          [leaf2 (list "As a dag, I am small.")])
      (let ([tr1 (let f ([n 100])
                   (if (= n 0)
                       leaf1
                       (let ([tr (f (- n 1))])
                         (cons tr tr))))]
            [tr2 (let f ([n 100])
                   (if (= n 0)
                       leaf2
                       (let ([tr (f (- n 1))])
                         (cons tr tr))))])
        (let ([ls (list (equal? tr1 tr1)
                        (equal? tr2 tr2)
                        (equal? tr1 tr2)
                        (equal? tr1 (car tr1)))])
          (set-car! leaf1 (car leaf2))
          (cons* (equal? tr1 tr1)
                 (equal? tr2 tr2)
                 (equal? tr1 tr2)
                 (equal? tr1 (cdr tr1))
                 ls))))
    '(#t #t #t #f #t #t #f #f))

  (time (equal? '#(a b c) '#(a b c)))
  (equal? '#101=#(a b c #1#) '#102=#(a b c #2#))
  (not (equal? '#103=#(a b c #103#) '#104=#(a b #104#)))
  (equal? '#105=#(a b c #105#) '#106=#(a b c #(a b c #106#)))
  (equal? '#107=#(a b c #107#) '#(a b c #(a b c #107#)))
  (not (equal? '#108=#(a b c #108#) '#109=#(a b c #(a c #109#))))
  (andmap eq?
    (let ([v1 '#110=#(a #110# c #110# d #111# f)]
          [v2 '#111=#(a #(a #111# c #110# d #111# f) c #110# d #111# f)]
          [v3 '#112=#(a #(a #112# c #110# d #112# f) c #110# d #112# g)])
      (list (equal? v1 v1)
            (equal? v2 v2)
            (equal? v3 v3)
            (equal? v1 v2)
            (equal? v2 v1)
            (equal? v1 v3)
            (equal? v2 v3)
            (equal? v3 v1)
            (equal? v3 v2)
            (equal? (vector-ref v1 1) v2)))
    '(#t #t #t #t #t #f #f #f #f #t))
  (andmap eq?
    (let ([leaf1 (vector "As a tree, I am huge.")]
          [leaf2 (vector "As a dag, I am small.")])
      (let ([tr1 (let f ([n 100])
                   (if (= n 0)
                       leaf1
                       (let ([tr (f (- n 1))])
                         (vector tr tr))))]
            [tr2 (let f ([n 100])
                   (if (= n 0)
                       leaf2
                       (let ([tr (f (- n 1))])
                         (vector tr tr))))])
        (let ([ls (list (equal? tr1 tr1)
                        (equal? tr2 tr2)
                        (equal? tr1 tr2)
                        (equal? tr1 (vector-ref tr1 0)))])
          (vector-set! leaf1 0 (vector-ref leaf2 0))
          (cons* (equal? tr1 tr1)
                 (equal? tr2 tr2)
                 (equal? tr1 tr2)
                 (equal? tr1 (vector-ref tr1 1))
                 ls))))
    '(#t #t #t #f #t #t #f #f))

  (let ([ls1 (make-list 100000 'a)]
        [ls2 (make-list 100000 'a)])
    (time
      (let f ([n 1000])
        (or (fx= n 0) (and (equal? ls1 ls2) (f (fx- n 1)))))))

  (let ([v1 (make-vector 10000 (make-vector 100 'a))]
        [v2 (make-vector 10000 (make-vector 100 'a))])
    (time 
      (let f ([n 100])
        (or (fx= n 0) (and (equal? v1 v2) (f (fx- n 1)))))))

  (time
    (let () ; w/sharing
      (define (consup1 n)
        (case n
          [(0) '()]
          [(1) 'a]
          [(2) 3/4]
          [(3) 3.416]
          [else
           (case (logand n 7)
             [(0) (let ([x (consup1 (ash n -3))]) (cons x x))]
             [(1) (make-vector 10 (consup1 (ash n -3)))]
             [(2) (let ([x (cons #f (consup1 (ash n -3)))]) (set-car! x x) x)]
             [(3) (let ([x (consup1 (ash n -3))]) (vector x 'a x))]
             [(4) (cons (consup1 (ash n -3)) (consup1 (ash n -3)))]
             [(5) (cons (string-copy "hello") (consup1 (ash n -3)))]
             [(6) (list (consup1 (ash n -3)))]
             [(7) (box (consup2 (ash n -3)))])]))
      (define (consup2 n)
        (case n
          [(0) '()]
          [(1) 'a]
          [(2) 3/4]
          [(3) 3.416]
          [else
           (case (logand n 7)
             [(0) (cons (consup2 (ash n -3)) (consup2 (ash n -3)))]
             [(1) (let ([x (make-vector 10 (consup1 (ash n -3)))])
                    (vector-set! x 5 (consup1 (ash n -3)))
                    x)]
             [(2) (let ([x (cons #f (consup2 (ash n -3)))]) (set-car! x x) x)]
             [(3) (let ([x (consup2 (ash n -3))]) (vector x 'a x))]
             [(4) (let ([x (consup2 (ash n -3))]) (cons x x))]
             [(5) (cons (string-copy "hello") (consup2 (ash n -3)))]
             [(6) (list (consup2 (ash n -3)))]
             [(7) (box (consup1 (ash n -3)))])]))
      (define (consup3 n)
        (case n
          [(0) 'a]
          [(1) '()]
          [(2) 3.416]
          [(3) 3/4]
          [else
           (case (logand n 7)
             [(0) (cons (consup3 (ash n -3)) (consup3 (ash n -3)))]
             [(1) (let ([x (make-vector 10 (consup3 (ash n -3)))])
                    (vector-set! x 5 (consup3 (ash n -3)))
                    x)]
             [(2) (let ([x (cons #f (consup3 (ash n -3)))]) (set-car! x x) x)]
             [(3) (let ([x (consup3 (ash n -3))]) (vector x 'a x))]
             [(4) (let ([x (consup3 (ash n -3))]) (cons x x))]
             [(5) (cons (string-copy "hello") (consup3 (ash n -3)))]
             [(6) (list (consup3 (ash n -3)))]
             [(7) (box (consup3 (ash n -3)))])]))
      (let loop ([n 10000])
        (unless (fx= n 0)
          (let ([rn (random (ash 1 50))])
            (let ([x1 (consup1 rn)] [x2 (consup2 rn)] [x3 (consup3 rn)])
              (define-syntax test
                (syntax-rules ()
                  [(_ e) (unless e (errorf #f "~s failed for rn = ~s" 'e rn))]))
              (test (equal? x1 x1))
              (test (equal? x2 x2))
              (test (equal? x3 x3))
              (test (equal? x1 x2))
              (test (equal? x2 x1))
              (test (not (equal? x1 x3)))
              (test (not (equal? x3 x1)))
              (test (not (equal? x2 x3)))
              (test (not (equal? x3 x2)))))
          (loop (fx- n 1))))
      #t))

  (time
    (let () ; w/o sharing
      (define (consup1 n)
        (case n
          [(0) '(#() 1389222281905413113340958870929048921229855260389703462234642106526635063669)]
          [(1) '#(a #vfx(3 4 5))]
          [(2) '(3/4 . #e3e100+4i)]
          [(3) '(3.416 . -7.5+.05i)]
          [else
           (case (logand n 3)
             [(0) (cons (consup1 (ash n -2)) (consup1 (ash n -3)))]
             [(1) (vector (consup1 (ash n -2)) (consup1 (ash n -3)))]
             [(2) (cons "hello" (consup1 (ash n -2)))]
             [(3) (box (consup2 (ash n -2)))])]))
      (define (consup2 n)
        (case n
          [(0) '(#() 1389222281905413113340958870929048921229855260389703462234642106526635063669)]
          [(1) '#(a #vfx(3 4 5))]
          [(2) '(3/4 . #e3e100+4i)]
          [(3) '(3.416 . -7.5+.05i)]
          [else
           (case (logand n 3)
             [(0) (cons (consup2 (ash n -2)) (consup2 (ash n -3)))]
             [(1) (vector (consup2 (ash n -2)) (consup2 (ash n -3)))]
             [(2) (cons "hello" (consup2 (ash n -2)))]
             [(3) (box (consup1 (ash n -2)))])]))
      (define (consup3 n)
        (case n
          [(0) '(#() 1389222281905413113340958870929048921229855260289703462234642106526635063669)]
          [(1) '#(a #vfx(3 4 6))]
          [(2) '(3/4 . #e3e100+5i)]
          [(3) '(3.417 . -7.5+.05i)]
          [else
           (case (logand n 3)
             [(0) (cons (consup3 (ash n -2)) (consup3 (ash n -3)))]
             [(1) (vector (consup3 (ash n -2)) (consup3 (ash n -3)))]
             [(2) (cons "hello" (consup3 (ash n -2)))]
             [(3) (box (consup3 (ash n -2)))])]))
      (let loop ([n 10000])
        (unless (fx= n 0)
          (let ([rn (random (ash 1 25))])
            (let ([x1 (consup1 rn)] [x2 (consup2 rn)] [x3 (consup3 rn)])
              (define-syntax test
                (syntax-rules ()
                  [(_ e) (unless e (errorf #f "~s failed for rn = ~s" 'e rn))]))
              (test (equal? x1 x1))
              (test (equal? x2 x2))
              (test (equal? x3 x3))
              (test (equal? x1 x2))
              (test (equal? x2 x1))
              (test (not (equal? x1 x3)))
              (test (not (equal? x3 x1)))
              (test (not (equal? x2 x3)))
              (test (not (equal? x3 x2)))))
          (loop (fx- n 1))))
      #t))

  (time
    (let () ; w/sharing
      (define (consup n)
        (define cache
          (let ([ls '()] [n 0] [vk 1000])
            (case-lambda
              [()
               (and (> n 0)
                 (let f ([i (random n)] [ls ls])
                   (if (fx< i vk)
                       (vector-ref (car ls) i)
                       (f (fx- i vk) (cdr ls)))))]
              [(x)
               (let ([i (fxmodulo n vk)])
                 (if (fx= i 0)
                     (set! ls (append ls (list (make-vector vk x))))
                     (vector-set! (list-ref ls (fxquotient n vk)) i x)))
               (set! n (fx+ n 1))])))
        (let f ([n n])
          (if (= n 0)
              (or (cache) (cons '() '()))
              (case (logand n 3)
                [(0) (let ([p1 (cons #f #f)] [p2 (cons #f #f)])
                       (let ([p (cons p1 p2)])
                         (cache p)
                         (let ([p (f (ash n -2))])
                           (set-car! p1 (car p))
                           (set-car! p2 (cdr p)))
                         (let ([p (f (ash n -2))])
                           (set-cdr! p1 (car p))
                           (set-cdr! p2 (cdr p)))
                         p))]
                [(1) (let ([m (random 10)])
                       (let ([v1 (make-vector m #f)] [v2 (make-vector m #f)])
                         (let ([p (cons v1 v2)])
                           (cache p)
                           (do ([i 0 (fx+ i 1)])
                               ((fx= i m))
                             (let ([p (f (ash n -2))])
                               (vector-set! v1 i (car p))
                               (vector-set! v2 i (cdr p))))
                           p)))]
                [(2) (let ([p1 (f (ash n -2))]
                           [p2 (f (ash n -2))])
                       (cons (cons (cdr p1) (cdr p2))
                             (cons (car p1) (car p2))))]
                [(3) (or (cache) (f (ash n -2)))]))))
      (let loop ([n 5000])
        (unless (fx= n 0)
          (let ([rn (* (random 1000) (expt 2 (random 10)))])
            (let ([p (consup rn)])
              (let ([x1 (car p)] [x2 (cdr p)])
                (define-syntax test
                  (syntax-rules ()
                    [(_ e) (unless e (errorf #f "~s failed for rn = ~s" 'e rn))]))
                (test (equal? x1 x1))
                (test (equal? x2 x2))
                (test (equal? x1 x2))
                (test (equal? x2 x1)))))
          (loop (fx- n 1))))
      #t))

 ; srfi 85 examples
  (equal? '() '())
  (equal? (vector 34.5 34.5) '#(34.5 34.5))
  (andmap eq?
    (let* ([x (list 'a)] [y (list 'a)] [z (list x y)])
      (list (equal? z (list y x)) (equal? z (list x x))))
    '(#t #t))
  (andmap eq?
    (let ([x (list 'a 'b 'c 'a)]
          [y (list 'a 'b 'c 'a 'b 'c 'a)])
      (set-cdr! (list-tail x 2) x)
      (set-cdr! (list-tail y 5) y)
      (list
        (equal? x x)
        (equal? x y)
        (equal? (list x y 'a) (list y x 'b))))
    '(#t #t #f))

 ; tests that break original SRFI 85 implementation
  (let ()
    (define x
      (let ([x1 (vector 'h)]
            [x2 (let ([x (list #f)]) (set-car! x x) x)])
        (vector x1 (vector 'h) x1 (vector 'h) x1 x2)))
    (define y
      (let ([y1 (vector 'h)]
            [y2 (vector 'h)]
            [y3 (let ([x (list #f)]) (set-car! x x) x)])
        (vector (vector 'h) y1 y1 y2 y2 y3)))
    (equal? x y))
  (let ()
    (define x
      (let ([x0 (vector #f #f #f)]
            [x1 (vector #f #f #f)]
            [x2 (vector #f #f #f)])
         (vector-fill! x0 x0)
         (vector-fill! x1 x1)
         (vector-fill! x2 x2)
         (vector x0 x1 x0 x2 x0)))
    (define y
      (let ([y0 (vector #f #f #f)]
            [y1 (vector #f #f #f)]
            [y2 (vector #f #f #f)])
         (vector-fill! y0 y0)
         (vector-fill! y1 y1)
         (vector-fill! y2 y2)
         (vector y0 y1 y1 y2 y2)))
    (equal? x y))
  (let ()
    (define x
      (let ([x (cons (cons #f 'a) 'a)])
        (set-car! (car x) x)
        x))
    (define y
      (let ([y (cons (cons #f 'a) 'a)])
        (set-car! (car y) (car y))
        y))
    (equal? x y))
  (let ()
    (define x
      (let* ([x3 (cons 'x3 'x3)]
             [x2 (cons 'x2 x3)]
             [x1 (cons x2 'x1)])
        (set-car! x3 x3)
        (set-cdr! x3 x3)
        (set-car! x2 x2)
        (set-cdr! x1 x1)
        x1))
    (define y
      (let* ([y2 (cons 'y1 'y1)]
             [y1 (cons y2 y2)])
        (set-car! y2 y1)
        (set-cdr! y2 y1)
        y1))
    (equal? x y))
  (let ()
    (define x
      (let* ([x3 (cons 'x3 'x3)]
             [x2 (cons 'x2 x3)]
             [x1 (cons x2 'x1)])
        (set-car! x3 x3)
        (set-cdr! x3 x3)
        (set-car! x2 x2)
        (set-cdr! x1 x1)
        x1))
    (define y
      (let* ([y2 (cons 'y1 'y1)]
             [y1 (cons y2 y2)])
        (set-car! y2 y1)
        (set-cdr! y2 y1)
        y1))
    (equal? x y))
  (let ()
    (define (make-x k)
      (let ([x1 (cons 
                  (let f ([n k])
                    (if (= n 0)
                        (let ([x0 (cons #f #f)])
                          (set-car! x0 x0)
                          (set-cdr! x0 x0)
                          x0)
                        (let ([xi (cons #f (f (- n 1)))])
                          (set-car! xi xi)
                          xi)))
                  #f)])
        (set-cdr! x1 x1)
        x1))
    (define y
      (let* ([y2 (cons #f #f)] [y1 (cons y2 y2)])
        (set-car! y2 y1)
        (set-cdr! y2 y1)
        y1))
    (time (equal? (make-x 100) y)))

 ; tests that stress corrected SRFI 85 implementation
 (or (equal?
       (let ([v1 '#200=(#200#)] [v2 '#201=(#201#)])
         (let ([t0 (current-time 'time-process)])
           (let ([ans (let f ([i 1000] [x #t])
                        (if (fx= i 0)
                            x
                            (f (fx- i 1) (and x (equal? v1 v2)))))])
             (list
               ans
               (let ([t (current-time 'time-process)])
                 (< (+ (* (- (time-second t) (time-second t0)) 1000000000)
                       (- (time-nanosecond t) (time-nanosecond t0)))
                    30000000))))))
       '(#t #t))
      (#%$enable-check-heap))

  (or (equal?
        (let ([v1 (make-vector 95000 (make-vector 95000 0))]
              [v2 (make-vector 95000 (make-vector 95000 0))])
          (let ([t0 (current-time 'time-process)])
            (let ([ans (equal? v1 v2)])
              (list
                ans
                (let ([t (current-time 'time-process)])
                  (> (+ (* (- (time-second t) (time-second t0)) 1000000000)
                        (- (time-nanosecond t) (time-nanosecond t0)))
                     100000000))))))
        '(#t #f))
      (#%$enable-check-heap))

  (or (equal?
        (let ([n 100000])
          (let ([f (lambda (n)
                     (let ([ls (make-list n 0)])
                       (set-cdr! (last-pair ls) ls)
                       ls))])
            (let ([v1 (f n)] [v2 (f (- n 1))])
              (let ([t0 (current-time 'time-process)])
                (let ([ans (equal? v1 v2)])
                  (let ([t (current-time 'time-process)])
                    (list
                      ans
                      (< (+ (* (- (time-second t) (time-second t0)) 1000000000)
                            (- (time-nanosecond t) (time-nanosecond t0)))
                         200000000))))))))
        '(#t #t))
      (#%$enable-check-heap))
)

(mat boolean?
    (boolean? #t)
    (boolean? #f)
    (not (boolean? 't))
    (not (boolean? 'f))
    (not (boolean? 'nil))
    (not (boolean? '(a b c)))
    (not (boolean? #\a))
)

(mat null?
    (null? '())
    (not (null? #f))
    (not (null? #t))
    (not (null? 3))
    (not (null? 'a))
 )

(mat pair?
    (pair? '(a b c))
    (pair? '(a . b))
    (pair? (cons 3 4))
    (not (pair? '()))
    (not (pair? 3))
    (not (pair? 'a))
    (not (pair? "hi"))
 )

(mat list?
    (list? '(a b c))
    (not (list? '(a . b)))
    (not (list? (cons 3 4)))
    (list? '())
    (not (list? 3))
    (not (list? 'a))
    (not (list? "hi"))
    (let ([a (make-list 100)])
       (set-cdr! (last-pair a) a)
       (not (list? a)))
 )

(mat atom?
    (not (atom? '(a b c)))
    (not (atom? '(a . b)))
    (not (atom? (cons 3 4)))
    (atom? '())
    (atom? 3)
    (atom? 'a)
    (atom? "hi")
 )
   
(mat number?
    (number? 3)
    (number? 23048230482304)
    (number? 203480234802384/23049821)
    (number? -3/4)
    (number? -1)
    (number? 0)
    (number? -12083)
    (number? 3.5)
    (number? 1.8e-10)
    (number? -3e5)
    (number? -1231.2344)
    (not (number? 'a))
    (not (number? "hi"))
    (not (number? (cons 3 4)))
    (number? 5.0-0.0i)
    (number? 5.0+0.0i)
    (number? 5.0+4.0i)
    (number? +inf.0)
    (number? -inf.0)
    (number? +nan.0)
 )

(mat complex?
    (complex? 3)
    (complex? 23048230482304)
    (complex? 203480234802384/23049821)
    (complex? -3/4)
    (complex? -1)
    (complex? 0)
    (complex? -12083)
    (complex? 3.5)
    (complex? 1.8e-10)
    (complex? -3e5)
    (complex? -1231.2344)
    (not (complex? 'a))
    (not (complex? "hi"))
    (not (complex? (cons 3 4)))
    (complex? 5.0-0.0i)
    (complex? 5.0+0.0i)
    (complex? 5.0+4.0i)
    (complex? +inf.0)
    (complex? -inf.0)
    (complex? +nan.0)
 )

(mat real?
    (real? 3)
    (real? 23048230482304)
    (real? 203480234802384/23049821)
    (real? -3/4)
    (real? -1)
    (real? 0)
    (real? -12083)
    (real? 3.5)
    (real? 1.8e-10)
    (real? -3e5)
    (real? -1231.2344)
    (not (real? 'a))
    (not (real? "hi"))
    (not (real? (cons 3 4)))
    (not (real? 5.0-0.0i))
    (not (real? 5.0+0.0i))
    (not (real? 5.0+4.0i))
    (real? +inf.0)
    (real? -inf.0)
    (real? +nan.0)
 )

(mat real-valued?
    (real-valued? 3)
    (real-valued? 23048230482304)
    (real-valued? 203480234802384/23049821)
    (real-valued? -3/4)
    (real-valued? -1)
    (real-valued? 0)
    (real-valued? -12083)
    (real-valued? 3.5)
    (real-valued? 1.8e-10)
    (real-valued? -3e5)
    (real-valued? -1231.2344)
    (not (real-valued? 'a))
    (not (real-valued? "hi"))
    (not (real-valued? (cons 3 4)))
    (real-valued? 5.0-0.0i)
    (real-valued? 5.0+0.0i)
    (not (real-valued? 8.0+3.0i))
    (real-valued? +inf.0)
    (real-valued? -inf.0)
    (real-valued? +nan.0)
 )

(mat rational?
    (rational? 3)
    (rational? 23048230482304)
    (rational? 203480234802384/23049821)
    (rational? -3/4)
    (rational? -1)
    (rational? 0)
    (rational? -12083)
    (rational? 3.5)
    (rational? 1.8e-10)
    (rational? -3e5)
    (rational? -1231.2344)
    (not (rational? 'a))
    (not (rational? "hi"))
    (not (rational? (cons 3 4)))
    (not (rational? 5.0-0.0i))
    (not (rational? 5.0+0.0i))
    (not (rational? 8.0+3.0i))
    (not (rational? +inf.0))
    (not (rational? -inf.0))
    (not (rational? +nan.0))
 )

(mat rational-valued?
    (rational-valued? 3)
    (rational-valued? 23048230482304)
    (rational-valued? 203480234802384/23049821)
    (rational-valued? -3/4)
    (rational-valued? -1)
    (rational-valued? 0)
    (rational-valued? -12083)
    (rational-valued? 3.5)
    (rational-valued? 1.8e-10)
    (rational-valued? -3e5)
    (rational-valued? -1231.2344)
    (not (rational-valued? 'a))
    (not (rational-valued? "hi"))
    (not (rational-valued? (cons 3 4)))
    (rational-valued? 5.0-0.0i)
    (rational-valued? 5.0+0.0i)
    (not (rational-valued? 8.0+3.0i))
    (not (rational-valued? +inf.0))
    (not (rational-valued? -inf.0))
    (not (rational-valued? +nan.0))
    (not (rational-valued? +inf.0+0.0i))
    (not (rational-valued? +inf.0-0.0i))
    (not (rational-valued? -inf.0+0.0i))
    (not (rational-valued? -inf.0-0.0i))
    (not (rational-valued? +nan.0+0.0i))
    (not (rational-valued? +nan.0-0.0i))
 )

(mat integer?
    (integer? 3)
    (integer? 23048230482304)
    (not (integer? 203480234802384/23049821))
    (not (integer? -3/4))
    (integer? -1)
    (integer? 0)
    (integer? -12083)
    (integer? 4.0)
    (not (integer? 3.5))
    (not (integer? 1.8e-10))
    (integer? 1.8e10)
    (integer? -3e5)
    (not (integer? -1231.2344))
    (not (integer? 'a))
    (not (integer? "hi"))
    (not (integer? (cons 3 4)))
    (not (integer? 3.0-0.0i))
    (not (integer? 3.0+0.0i))
    (not (integer? 3.0+1.0i))
    (integer? #i1)
    (not (integer? +inf.0))
    (not (integer? -inf.0))
    (not (integer? +nan.0))
 )

(mat integer-valued?
    (integer-valued? 3)
    (integer-valued? 23048230482304)
    (not (integer-valued? 203480234802384/23049821))
    (not (integer-valued? -3/4))
    (integer-valued? -1)
    (integer-valued? 0)
    (integer-valued? -12083)
    (integer-valued? 4.0)
    (not (integer-valued? 3.5))
    (not (integer-valued? 1.8e-10))
    (integer-valued? 1.8e10)
    (integer-valued? -3e5)
    (not (integer-valued? -1231.2344))
    (not (integer-valued? 'a))
    (not (integer-valued? "hi"))
    (not (integer-valued? (cons 3 4)))
    (integer-valued? 3.0-0.0i)
    (integer-valued? 3.0+0.0i)
    (not (integer-valued? 3.0+1.0i))
    (integer-valued? #i1)
    (not (integer-valued? +inf.0))
    (not (integer-valued? -inf.0))
    (not (integer-valued? +nan.0))
 )

(mat char?
    (char? #\a)
    (char? #\3)
    (char? (string-ref "hi" 0))
    (not (char? "a"))
    (not (char? 'a))
    (not (char? '(a b c)))
 )

(mat string?
    (string? "hi")
    (string? (string-append "hi " "there"))
    (string? (string #\a #\b #\c #\c))
    (not (string? #\a))
    (not (string? 'a))
    (not (string? '(a b c)))
    (not (string? 3))
 )

(mat vector?
    (vector? '#(a b c))
    (vector? (vector 1 2 3 4))
    (not (vector? '(a b c)))
    (not (vector? "hi there"))
    (not (vector? 234234))
 )

(mat fxvector?
    (fxvector? #vfx(1 2 3))
    (fxvector? (fxvector 1 2 3 4))
    (not (fxvector? '(1 2 3)))
    (not (fxvector? '#(1 2 3)))
    (not (fxvector? '#vu8(1 2 3)))
    (not (fxvector? "hi there"))
    (not (fxvector? 234234))
 )

(mat bytevector?
    (bytevector? '#vu8(1 2 3))
    (bytevector? (bytevector 1 2 3 4))
    (not (bytevector? '(1 2 3)))
    (not (bytevector? '#(1 2 3)))
    (not (bytevector? '#vfx(1 2 3)))
    (not (bytevector? "hi there"))
    (not (bytevector? 234234))
 )

(mat symbol?
    (symbol? 'a)
    (symbol? '|(a b c)|)
    (symbol? (string->symbol "hi there"))
    (symbol? (gensym "hi there"))
    (not (symbol? "hi there"))
    (not (symbol? 3))
 )

(mat box?
    (box? '#&(a b c))
    (box? (box 3))
    (not (box? '()))
    (not (box? 3))
    (not (box? '(a b c)))
    (not (box? 'a))
    (not (box? "hi"))
 )

(mat input-port?
    (input-port? (current-input-port))
    (not (input-port? (open-output-string)))
 )

(mat output-port?
    (output-port? (current-output-port))
    (not (output-port? (open-input-string "hello")))
    (output-port? (trace-output-port))
 )

(mat procedure?
    (procedure? car)
    (procedure? (lambda (x) x))
    (not (procedure? 3))
    (not (procedure? '#(1 b c)))
    (not (procedure? '(a b c)))
 )

(mat boolean=?
  (error? (boolean=?))
  (error? (boolean=? #f))
  (error? (boolean=? 3 #t))
  (error? (boolean=? #t 3))
  (error? (boolean=? 3 #f #t))
  (error? (boolean=? #t 3 #t))
  (error? (boolean=? #t #f 3))
  (error? (boolean=? 3 #t #f #t))
  (error? (boolean=? #f 3 #f #t))
  (error? (boolean=? #t #t 3 #t))
  (error? (boolean=? #f #t #f 3))
  (eqv? (boolean=? #t #t) #t)
  (eqv? (boolean=? #f #t) #f)
  (eqv? (boolean=? #t #f) #f)
  (eqv? (boolean=? #f #f) #t)
  (eqv? (boolean=? #f #f #t) #f)
  (eqv? (boolean=? #f #f #f #f #f #t) #f)
  (eqv? (boolean=? #t #t #t #t #t #f) #f)
  (eqv? (boolean=? #t #t #t #t #t #t) #t)
  (eqv? (boolean=? #f #f #f #f #f #f) #t)
)

(mat symbol=?
  (error? (symbol=?))
  (error? (symbol=? 'f))
  (error? (symbol=? 3 't))
  (error? (symbol=? 't 3))
  (error? (symbol=? 3 'f 't))
  (error? (symbol=? 't 3 't))
  (error? (symbol=? 't 'f 3))
  (error? (symbol=? 3 't 'f 't))
  (error? (symbol=? 'f 3 'f 't))
  (error? (symbol=? 't 't 3 't))
  (error? (symbol=? 'f 't 'f 3))
  (eqv? (symbol=? 't 't) #t)
  (eqv? (symbol=? 'f 't) #f)
  (eqv? (symbol=? 't 'f) #f)
  (eqv? (symbol=? 'f 'f) #t)
  (eqv? (symbol=? 'f 'f 't) #f)
  (eqv? (symbol=? 'f 'f 'f 'f 'f 't) #f)
  (eqv? (symbol=? 't 't 't 't 't 'f) #f)
  (eqv? (symbol=? 't 't 't 't 't 't) #t)
  (eqv? (symbol=? 'f 'f 'f 'f 'f 'f) #t)
)
