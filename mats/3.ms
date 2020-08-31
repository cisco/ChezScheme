;;; 3.ms
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

(define-syntax matrest
  (lambda (x)
    (define matrest-argerr-test
      (lambda (ls)
        (if (null? ls)
            '()
            (cons (with-syntax (((n ...) (make-list (length (cdr ls)) 1)))
                    (syntax (error? (matrestf n ...))))
                  (matrest-argerr-test (cdr ls))))))
    (define iota
      (lambda (i n)
        (if (= i n)
            '()
            (cons i (iota (+ i 1) n)))))
    (define matrest-test
      (lambda (n ls)
        (let* ((m (length ls)) (n (+ n m)))
          (let f ((n n))
            (if (< n m)
                '()
                (cons (with-syntax (((x ...) (iota 0 m))
                                    ((y ...) (iota m n)))
                        (syntax (equal? (matrestf x ... y ...)
                                        '(y ...))))
                      (f (- n 1))))))))
    (syntax-case x ()
      ((k n)
       (let ((n (datum n)))
         (with-syntax (((g ...) (generate-temporaries (make-list n)))
                       (name (datum->syntax (syntax k)
                               (string->symbol (format "matrest~s" n)))))
           (with-syntax (((at ...) (matrest-argerr-test (syntax (g ...))))
                         ((t ...) (matrest-test 10 (syntax (g ...)))))
             (syntax
                (mat name
                  (begin (define (matrestf g ... . r) r) #t)
                  at ...
                  t ...
                  )))))))))

(matrest 0)
(matrest 1)
(matrest 2)
(matrest 3)
(matrest 4)
(matrest 5)
(matrest 6)
(matrest 7)
(matrest 8)
(matrest 9)
(matrest 10)

(mat application
   (error? ((list '(a b c))))
 )

(mat lambda
    (let ((f (lambda () 'a))) (eq? (f) 'a))
    (let ((f (lambda (x) x))) (eq? (f 'a) 'a))
    (let ((f (lambda x x)))
        (and (equal? (f) '())
             (equal? (f 1) '(1))
             (equal? (f 1 2) '(1 2))
             (equal? (f 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7))))
    (let ((f (lambda (x y) (cons x y)))) (equal? (f 1 2) '(1 . 2)))
    (let ((f (lambda (x . y) (cons x y))))
        (and (equal? (f 1) '(1))
             (equal? (f 1 2) '(1 2))
             (equal? (f 1 2 3) '(1 2 3))
             (equal? (f 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7))))
    (let ((f (lambda (x y z) (list x y z)))) (equal? (f 1 2 3) '(1 2 3)))
    (let ((f (lambda (x y . z) (cons x (cons y z)))))
        (and (equal? (f 1 2) '(1 2))
             (equal? (f 1 2 3) '(1 2 3))
             (equal? (f 1 2 3 4) '(1 2 3 4))
             (equal? (f 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7))))
    (let ((f (lambda (x y) (set! x 3) (cons x y))))
        ;see if there is an implicit "begin"
        (equal? (f 1 2) '(3 . 2)))
    (eqv?
      (let ((f (case-lambda
                 ((x) (+ x 1))
                 ((x . xs) (cons (+ x 2) xs))
                 (xs 0))))
         (f))
      0)
 )

(mat case-lambda
    (procedure? (case-lambda))
    (error? ((case-lambda)))
    (error? (let ((f (case-lambda))) (f 3 4 5)))
    (begin
      (define foo (case-lambda [() 0] [(a b c) 3]))
      (eq? (foo 1 2 3) 3))
    (eq? (foo) 0)
    (error? (foo 1))
    (error? (foo 1 2))
    (error? (foo 1 2 3 4))
    (begin
      (define foo (case-lambda [(a b c) 3] [() 0]))
      (eq? (foo 1 2 3) 3))
    (eq? (foo) 0)
    (error? (foo 1))
    (error? (foo 1 2))
    (error? (foo 1 2 3 4))
    (begin
      (define foo (case-lambda [() 0] [(a) 1] [args 2]))
      (eq? (foo 1 2 3) 2))
    (eq? (foo) 0)
    (eq? (foo 1) 1)
    (begin
      (define foo (case-lambda [() 0] [(a) 1] [(a b c . args) 3]))
      (eq? (foo 1 2 3) 3))
    (eq? (foo) 0)
    (eq? (foo 1) 1)
    (error? (foo 1 2))
    (begin
      (define foo (case-lambda [() 0] [args 1] [(a b c . args) 3]))
      (and (eq? (foo 1 2 3) 1)
           (eq? (foo 1 2) 1)
           (eq? (foo 1) 1)
           (eq? (foo) 0)))
 )

(mat let
    (let ((x 'a)) (eq? x 'a))
    (let ((x 'a)) (let ((x 'b)) (eq? x 'b)))
    (let ((x 'a) (y 'b)) (equal? (cons x y) '(a . b)))
    (let ((x 'a))
        ;test for implicit "begin"
        (let ((y 'b)) #f (set! x y))
        (eq? x 'b))
    ((lambda (x) (eq? x 'a)) 'a)
    ((lambda (x . r) (eq? x 'a)) 'a)
    ((lambda r (eq? (car r) 'a)) 'a)
    (error? ((lambda (x . r) (eq? x 'a))))
 )

(mat let*
    (let* ((x 'a)) (eq? x 'a))
    (let* ((x (cons 1 2)) (y x)) (eq? x y))
    (let ((x 1) (y 2)) (let* ((x 10) (y 12)) (equal? (cons x y) '(10 . 12))))
    (let* ((x 'a))
        ;test for implicit "begin"
        (let* ((y 'b)) #f (set! x y))
        (eq? x 'b))
 )

(mat letrec
    (letrec ((f (lambda () x)) (x (cons 1 2))) (eq? (f) x))
    (letrec ((f (lambda () g)) (g (lambda () f)))
        (and (eq? (f) g) (eq? (g) f)))
    (letrec ((f (lambda (x) (if (zero? x) 'odd (g (1- x)))))
             (g (lambda (x) (if (zero? x) 'even (f (1- x))))))
      (and
        (eq? (g 10) 'even)
        (eq? (g 13) 'odd)
        (eq? (f 13) 'even)))
    (letrec ((x 'a))
        ;test for implicit "begin"
        (letrec ((y 'b)) #f (set! x y))
        (eq? x 'b))
    #;(eqv? (letrec ((v 0) (k (call/cc (lambda (x) x)))) ; invalid in r6rs
             ; david carlton's bug
             (set! v (+ v 1))
             (k (lambda (x) v)))
          1)
    #;(eqv? (letrec ((k (call/cc (lambda (x) x))) (v 0)) ; invalid in r6rs
             ; david carlton's bug
             (set! v (+ v 1))
             (k (lambda (x) v)))
          1)
    #;(eqv? (letrec* ((v 0) (k (call/cc (lambda (x) x)))) ; invalid in r6rs
             ; variation on david carlton's "bug"
             (set! v (+ v 1))
             (k (lambda (x) v)))
          2)
    #;(eqv? (letrec* ((k (call/cc (lambda (x) x))) (v 0)) ; invalid in r6rs
             ; another variation on david carlton's "bug"
             (set! v (+ v 1))
             (k (lambda (x) v)))
          1)
   ; testing for named-let equivalents
   (eqv? (letrec ((f (lambda (x) (if (zero? x) 1 (* x (f (1- x))))))) (f 5))
         120)
   (letrec ((f (lambda (x) (if (zero? x) #t (f (1- x)))))) (f 10000))
   (letrec ((f (lambda (x y) (or (and (= x 0) (= y 10)) (f (- x 1) (+ y 1))))))
     (f 10 0))
   (eqv? (letrec ((f (lambda (x) (if (= x 0) 1 (+ (f (- x 1)) 1))))) (f 10)) 11)
   (eqv? (let ([base 20])
           (letrec ((f (lambda (x)
                         (if (= x 0) base
                             (+ (f (- x 1)) 1)))))
             (f 10)))
         30)
   (error? (letrec ((x (lambda (x) x))) (f 3 4)))
   (eq? (letrec ((f (lambda (x) (if x (list (f x)) 0)))) (f #f)) 0)
   (equal? (letrec ((f (lambda (x) (if x (list (f (not x))) 0)))) (f #t)) '(0))
   (equal? (letrec ((f (lambda (x) (if x (list (g x)) 0)))
                    (g (lambda (x) (f #f))))
             (f #t))
     '(0))
   (equal? (letrec ((f (lambda (x) (if x (list (g (not x))) 0)))
                    (g (lambda (x) (f x))))
             (g #t))
     '(0))
  (error? (letrec ([a 3] [b a]) (+ a b)))
 ; shouldn't get warnings for these if valid-check algorithm is working
 ; properly
  (procedure? (letrec ([bar (letrec ([f (lambda (x) f)]) f)]) bar))
  (eqv?
    (letrec ([fllog 3] [flacosh (or values (lambda (x) fllog))]) (flacosh 4))
    4)
  (eqv?
    (let ()
      (define $b #t)
      (letrec ([fllog 3] [flacosh (if $b (lambda (x) fllog) values)])
        (flacosh 4)))
    3)
  (equal?
    (letrec ([a 3] [b (#2%cons (lambda () a) (lambda (x) (set! a x)))])
      ((cdr b) 17)
      (list a ((car b))))
    '(17 17))
  #;(pair?
    (member
      (letrec ([k (call/cc (lambda (k) k))] ; invalid in r6rs
               [f (let ([n 0]) (lambda (a) (set! n (+ n 1)) n))])
        (f (void))
        (let ([m (k f)])
          (list (eq? k f) m (f (void)))))
      '((#f 2 2) (#t 3 4))))
  (error? (letrec ([a (set! b 0)] [b 3]) 17))
 ; test strongly connected components algorithm used by cpletrec
  (equal?
    (letrec ([f0 (lambda (x) (f4 (cons 0 x)))]
             [f1 (lambda (x)
                   (if (fx> (length x) 10)
                       x
                       (f3 (f4 (cons 1 x)))))]
             [f2 (lambda (x) (f3 (cons 2 x)))]
             [f3 (lambda (x) (f1 (cons 3 x)))]
             [f4 (lambda (x) (f1 (f2 (cons 4 x))))])
      (apply
        (lambda (t0 t1 t2 t3 t4)
          (set! f0 (values t0))
          (set! f1 (values t1))
          (set! f2 (values t2))
          (set! f3 (values t3))
          (set! f4 (values t4)))
        (list f0 f1 f2 f3 f4))
      (f0 '()))
    '(3 3 3 2 4 1 3 2 4 1 3 2 4 0))
  (equal?
    (letrec ([f0 (list (lambda (x) ((car f4) (cons 0 x))))]
             [f1 (list (lambda (x)
                         (if (fx> (length x) 10)
                             x
                             ((car f3) ((car f4) (cons 1 x))))))]
             [f2 (list (lambda (x) ((car f3) (cons 2 x))))]
             [f3 (list (lambda (x) ((car f1) (cons 3 x))))]
             [f4 (list (lambda (x) ((car f1) ((car f2) (cons 4 x)))))])
      ((car f0) '()))
    '(3 3 3 2 4 1 3 2 4 1 3 2 4 0))
 )

(mat letrec*
    (letrec* ((f (lambda () x)) (x (cons 1 2))) (eq? (f) x))
    (letrec* ((f (lambda () g)) (g (lambda () f)))
        (and (eq? (f) g) (eq? (g) f)))
    (letrec* ((f (lambda (x) (if (zero? x) 'odd (g (1- x)))))
              (g (lambda (x) (if (zero? x) 'even (f (1- x))))))
      (and
        (eq? (g 10) 'even)
        (eq? (g 13) 'odd)
        (eq? (f 13) 'even)))
    (letrec* ((x 'a))
        ;test for implicit "begin"
        (letrec ((y 'b)) #f (set! x y))
        (eq? x 'b))
    #;(eqv? (letrec* ((v 0) (k (call/cc (lambda (x) x)))) ; invalid in r6rs
             ; variation on david carlton's "bug"
             (set! v (+ v 1))
             (k (lambda (x) v)))
          2)
    #;(eqv? (letrec* ((k (call/cc (lambda (x) x))) (v 0)) ; invalid in r6rs
             ; another variation on david carlton's "bug"
             (set! v (+ v 1))
             (k (lambda (x) v)))
          1)
   ; testing for named-let equivalents
   (eqv? (letrec* ((f (lambda (x) (if (zero? x) 1 (* x (f (1- x))))))) (f 5))
         120)
   (letrec* ((f (lambda (x) (if (zero? x) #t (f (1- x)))))) (f 10000))
   (letrec* ((f (lambda (x y) (or (and (= x 0) (= y 10)) (f (- x 1) (+ y 1))))))
     (f 10 0))
   (eqv? (letrec* ((f (lambda (x) (if (= x 0) 1 (+ (f (- x 1)) 1))))) (f 10)) 11)
   (eqv? (let ([base 20])
           (letrec* ((f (lambda (x)
                         (if (= x 0) base
                             (+ (f (- x 1)) 1)))))
             (f 10)))
         30)
   (error? (letrec* ((x (lambda (x) x))) (f 3 4)))
   (eq? (letrec* ((f (lambda (x) (if x (list (f x)) 0)))) (f #f)) 0)
   (equal? (letrec* ((f (lambda (x) (if x (list (f (not x))) 0)))) (f #t)) '(0))
   (equal? (letrec* ((f (lambda (x) (if x (list (g x)) 0)))
                    (g (lambda (x) (f #f))))
             (f #t))
     '(0))
   (equal? (letrec* ((f (lambda (x) (if x (list (g (not x))) 0)))
                    (g (lambda (x) (f x))))
             (g #t))
     '(0))
  (equal? (letrec* ((x 3) (y x)) (+ x y)) 6)
  (equal?
    (parameterize ([internal-defines-as-letrec* #t])
      (eval '(let ()
               (define x 3)
               (define y x)
               (+ x y))))
    6)
  (error? (letrec* ((y x) (x 3)) (+ x y)))
  (error? (letrec* ((x x)) x))
 ; shouldn't get warnings for these if valid-check algorithm is working
 ; properly
  (procedure? (letrec* ([bar (letrec* ([f (lambda (x) f)]) f)]) bar))
  (eqv?
    (letrec* ([fllog 3] [flacosh (or values (lambda (x) fllog))]) (flacosh 4))
    4)
  (eqv?
    (let ()
      (define $b #t)
      (letrec* ([fllog 3] [flacosh (if $b (lambda (x) fllog) values)])
        (flacosh 4)))
    3)
  (equal?
    (letrec* ([a 3] [b (#2%cons (lambda () a) (lambda (x) (set! a x)))])
      ((cdr b) 17)
      (list a ((car b))))
    '(17 17))
  (equal?
    (letrec* ([f (let ([n 0]) (lambda () (set! n (+ n 1)) n))])
      (letrec* ([x (f)] [y (f)])
        (list x y)))
    '(1 2))
  (error? (letrec* ([a (set! b 0)] [b 3]) 17))
  (eqv? (letrec* ([b 3] [a (set! b 0)]) 17) 17)
  #;(equal?
    (letrec* ([k (call/cc (lambda (k) k))] ; invalid in r6rs
              [f (let ([n 0]) (lambda (a) (set! n (+ n 1)) n))])
      (f (void))
      (let ([m (k f)])
        (list (eq? k f) m (f (void)))))
    '(#f 2 2))
  #;(equal?
    (letrec* ([f (let ([n 0]) (lambda (a) (set! n (+ n 1)) n))]
              [k (call/cc (lambda (k) k))]) ; invalid in r6rs
      (f (void))
      (let ([m (k f)])
        (list (eq? k f) m (f (void)))))
    '(#t 3 4))

 ; make sure letrec* doesn't treat global or local assignable
 ; variables simple
  (begin
    (define $frodo)
    (letrec* ([merry 'merry]
              [ignore (set! $frodo (lambda () pippin))]
              [pippin (#3%cons $frodo $frodo)])
      (void))
    (eq? (car ($frodo)) $frodo))
  (begin
    (define $frodo)
    (letrec* ([merry 'merry]
              [ignore (set! $frodo (lambda () pippin))]
              [pippin $frodo])
      (void))
    (eq? ($frodo) $frodo))
  (let ([$frodo #f])
    (letrec* ([merry 'merry]
              [ignore (set! $frodo (lambda () pippin))]
              [pippin $frodo])
      (void))
    (eq? ($frodo) $frodo))

 ; similarly, make sure letrec* doesn't reorder primitives that can
 ; observe effects of other expressions
  (equal?
    (letrec* ([t (cons 'a 'b)]
              [f (lambda () y)]
              [x (begin (set-car! t 'c) (car t))]
              [p (car t)]
              [g (lambda () x)]
              [y (begin (set-car! t 'd) (car t))]
              [q (car t)])
      (list t p q x y (f) (g)))
    `((d . b) c d c d d c))

  (equal?
    (letrec* ([t (gensym)]
              [f (lambda () y)]
              [x (list (putprop t 'ham f))]
              [p (property-list t)]
              [g (lambda () x)]
              [y (list (putprop t 'spam g))]
              [q (property-list t)])
      (list
        (equal? p (list 'ham f))
        (or (equal? q (list 'ham f 'spam g))
            (equal? q (list 'spam g 'ham f)))
        (procedure? f)
        (procedure? g)
        x
        y))
    `(#t #t #t #t (,(void)) (,(void))))
 ; test strongly connected components algorithm used by cpletrec
  (equal?
    (letrec* ([f0 (lambda (x) (f4 (cons 0 x)))]
              [f1 (lambda (x)
                    (if (fx> (length x) 10)
                        x
                        (f3 (f4 (cons 1 x)))))]
              [f2 (lambda (x) (f3 (cons 2 x)))]
              [f3 (lambda (x) (f1 (cons 3 x)))]
              [f4 (lambda (x) (f1 (f2 (cons 4 x))))])
      (apply
        (lambda (t0 t1 t2 t3 t4)
          (set! f0 (values t0))
          (set! f1 (values t1))
          (set! f2 (values t2))
          (set! f3 (values t3))
          (set! f4 (values t4)))
        (list f0 f1 f2 f3 f4))
      (f0 '()))
    '(3 3 3 2 4 1 3 2 4 1 3 2 4 0))
  (equal?
    (letrec* ([f0 (list (lambda (x) ((car f4) (cons 0 x))))]
              [f1 (list (lambda (x)
                          (if (fx> (length x) 10)
                              x
                              ((car f3) ((car f4) (cons 1 x))))))]
              [f2 (list (lambda (x) ((car f3) (cons 2 x))))]
              [f3 (list (lambda (x) ((car f1) (cons 3 x))))]
              [f4 (list (lambda (x) ((car f1) ((car f2) (cons 4 x)))))])
      ((car f0) '()))
    '(3 3 3 2 4 1 3 2 4 1 3 2 4 0))
 )

(mat dipa-letrec ; from Dipa Sarkar
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)] [b (a)] [c (cons 1 2)]) b))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)]
              [b (let ([d a]) (d))]
              [c (cons 1 2)])
      b))
  
  (error? ; undefined variable c
    (letrec* ([a (let ([d (lambda () c)]) (d))]
              [b a]
              [c (cons 1 2)])
      b))
  
  (error? ; undefined variable c
    (letrec* ([a
               (letrec* ([b (lambda () c)] [d (b)] [c (cons 1 2)]) d)])
      a))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)]
              [b (lambda (f) (f))]
              [d (b a)]
              [c (cons 1 2)])
      d))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)]
              [b (lambda () a)]
              [d ((b))]
              [c (cons 1 2)])
      d))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () (set! c d))]
              [b (a)]
              [c (cons 1 2)])
      b))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () (set! d c))]
              [b (a)]
              [c (cons 1 2)])
      d))
  
  (equal?
    (letrec* ([a (lambda () c)]
              [b (if #t a (a))]
              [c (cons 1 2)])
      (b))
    '(1 . 2))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)]
              [b (if #t (a) a)]
              [c (cons 1 2)])
      b))
  
  (error? ; undefined variable a
    (letrec ([a (letrec* ([b (lambda () a)]) (b))]
             [c (cons 1 2)])
      (cons a c)))
  
  (error? ; undefined variable a
    (letrec ([a (lambda () c)]
             [b (lambda () a)]
             [c ((b))]
             [d (cons 1 2)])
      d))
  
  (error? ; undefined variable a
    (letrec ([a (lambda () b)][b (lambda () c)][c (a)]) c))
  
  (error? ; undefined variable a
    (letrec ([a
              (letrec* ([b (lambda () c)] [c (cons 1 2)]) (b))]
             [d a])
      d))
  
  (error? ; undefined variable a
    (letrec ([a (let ([x 0])(lambda () x))][b (let ([y 2]) (* y (a)))]) b))
  
  (error? ; undefined variable c
    (letrec ([a (letrec* ([b (lambda () c)] [d c]) (b))]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons 3 4)]) (cons (m) n))))
  
  (error? ; undefined variable c
    (letrec ([a (letrec* ([b (lambda () c)] [d c]) b)]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons 3 4)]) (cons (m) n))))
  
  (equal?
    '((3 . 4) 3 . 4)
    (letrec ([a (letrec* ([b (lambda () c)] [d 0]) b)]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons 3 4)]) (cons (m) n))))
  
  (equal?
    '((1 . 2) (3 . 4) 3 . 4)
    (letrec ([a (letrec* ([b (lambda () (lambda () c))] [d (b)]) d)]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons 3 4)])
        (cons c (cons (m) n)))))
  
  (error? ; undefined variable b
    (letrec ([a
              (letrec ([b (lambda () (lambda () c))] [d (b)]) d)]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons 3 4)])
        (cons c (cons (m) n)))))
  
  (error? ; undefined variable b
    (letrec ([a (letrec ([b (lambda () (lambda () c))] [d ((b))]) d)]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons 3 4)])
        (cons c (cons (m) n)))))
  
  (error? ; undefined variable c
    (letrec ([a (letrec* ([b (lambda () (lambda () c))] [d ((b))]) d)]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons 3 4)])
        (cons c (cons (m) n)))))
  
  (equal?
    '((1 . 2) ((1 . 2) . 4) (1 . 2) . 4)
    (letrec ([a (letrec* ([b (lambda () (lambda () c))] [d (b)]) d)]
             [c (cons 1 2)])
      (letrec* ([m (lambda () n)] [n (cons c 4)])
        (cons (a) (cons (m) n)))))
  
  (equal? '(1 . 2)
    (letrec* ([m (let ([f (lambda () n)]) f)][n (cons 1 2)]) n))
  
  (error? ; undefined variable n
    (letrec* ([m (let ([f (lambda () n)]) (f))]
              [n (cons 1 2)])
      n))
  
  (eqv? #f
   (letrec* ([a (lambda (n) (n 0))]
             [b (a (lambda (x) (if (zero? x) #f c)))]
             [c #t])
     b))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda (n) (n 0))]
              [b (a (lambda (x) (if (zero? x) c #f)))]
              [c #t])
      b))
  
  (error? ; undefined variable a
    (letrec ([a (letrec ([b (letrec ([c (lambda () a)]) (c))])
                  (lambda () b))])
      ((lambda () c))))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda (f g) (f g))][b (lambda (x) c)][c (b b)]) (list a b c)))
  
  (error? ; undefined variable m
    (letrec ([m (lambda (x) (cons n x))] [n ((lambda () m))])
      (m '())))
  
  (error? ; undefined variable a
    (letrec ([a (lambda () 0)]
             [b (zero? (a))]
             [c (if b (a) a)])
      c))
  
  (error? ; undefined variable y
    (letrec ([x (lambda () y)] [y (lambda (f) (f))] [z (y (lambda () (x)))])
      (z (lambda () 3))))
  
  (eq? 3
    (letrec* ([x (lambda (f) (f))]
              [y (lambda () x)]
              [z (x y)])
      (z (lambda () 3))))
  
  (eq? 3
    (letrec ([x (lambda (f) (f))]
             [y (lambda () x)]
             [z (lambda () (x y))])
      ((z) (lambda () 3))))
  
  #;(error? ; undefined variable y
    (letrec ([x (lambda () y)]
             [y (lambda (f) (f))]
             [z (call/cc (lambda (k) (y (lambda () (x)))))]) ; invalid in r6rs
      ((z) (lambda () 3))))
  
  #;(eq? 3
       (letrec ([x (lambda (f) (f))]
                [y (lambda () x)]
                [z (call/cc (lambda (k) (lambda () (x y))))]) ; invalid in r6rs
         ((z) (lambda () 3))))
  
  (error? ; undefined variable a
    (letrec ([a 3]
             [b (letrec* ([c (lambda () a)] [d (c)]) (* d d))])
      (* a b)))
  
  (error? ; undefined variable a
    (letrec ([a 3]
             [b (letrec* ([c (lambda () (lambda () a))] [d (c)])
                  (* (d) (d)))])
      (* a b)))
  
  (eq? 9
       (letrec ([a 3] [b (letrec* ([c (lambda () (lambda () a))] [d (c)]) d)])
         (* a (b))))
  
  (eq? 27
       (letrec ([a 3]
          [b (lambda () (letrec* ([c (lambda () (lambda () a))]
                [d (c)])
               (* (d) (d))))])
         (* a (b))))
  
  
  #;(error? ; undefined variable b
    (letrec* ([a (call/cc (lambda (k) (lambda (n) (if (zero? n) k b))))]
              [b ((a 0) (a 10))]) ; invalid in r6rs
      b))
)

(mat cpvalid
  (error? (letrec ([a (lambda () c)] [b (a)] [c 4]) b))
  (error? (letrec* ([a (lambda () c)] [b (a)] [c 4]) b))
  (error? (letrec ([f (lambda () (g))] [h (f)] [g (lambda () 3)]) h))
  (error? (letrec* ([f (lambda () (g))] [h (f)] [g (lambda () 3)]) h))
  (error? (letrec ([a (set! b 0)] [b 3]) b))
  (error? (letrec ([b 3] [a (set! b 723)]) b))
  (error? (letrec* ([a (set! b 0)] [b 3]) b))
  (eqv? (letrec* ([b 3] [a (set! b 723)]) b) 723)
  (error? (letrec ([a (lambda () c)]
                   [b (let ((f (lambda () (a)))) (f))]
                   [c 44])
            (list (a) b c)))
  (error? (letrec* ([a (lambda () c)]
                    [b (let ((f (lambda () (a)))) (f))]
                    [c 44])
            (list (a) b c)))
  (error? (letrec ([a (lambda () c)]
                   [b (let ((f (lambda () a))) (f))]
                   [c 44])
            (list (a) (b) c)))
  (equal? (letrec* ([a (lambda () c)]
                    [b (let ((f (lambda () a))) (f))]
                    [c 44])
            (list (a) (b) c))
          '(44 44 44))
  (equal? (letrec ([a (cons (lambda () b) (lambda () c))]
                   [b (cons (lambda () a) (lambda () c))]
                   [c (cons (lambda () a) (lambda () b))]
                   [d (list (lambda () d))])
            (map pair? (list ((car a)) ((cdr b)) c ((car d)))))
          '(#t #t #t #t))
  (equal? (letrec* ([a (cons (lambda () b) (lambda () c))]
                    [b (cons (lambda () a) (lambda () c))]
                    [c (cons (lambda () a) (lambda () b))]
                    [d (list (lambda () d))])
            (map pair? (list ((car a)) ((cdr b)) c ((car d)))))
          '(#t #t #t #t))
  (error? (letrec ([a (letrec ([b (lambda () (c))]
                               [c (lambda () a)]
                               [d (lambda () (b))])
                        (d))])
            (a 55)))
  (error? (letrec ([a (letrec* ([b (lambda () (c))]
                                [c (lambda () a)]
                                [d (lambda () (b))])
                        (d))])
            (a 55)))
  (error? (letrec ([a (letrec ([b (lambda () (c))]
                               [c (lambda () a)]
                               [d (b)])
                        (d))])
            (a 55)))
  (error? (letrec ([a (letrec* ([b (lambda () (c))]
                                [c (lambda () a)]
                                [d (b)])
                        (d))])
            (a 55)))
  (eqv? (letrec* ([b (lambda () (c))]
                  [c (lambda () 73)]
                  [d (b)])
          d)
        73)
  (procedure?
    (let ()
        (define f (rec f* (lambda () (g))))
        (define g (rec g* (lambda () (f))))
        g))
  (equal?
    (let ([q #f])
      (letrec ((a (letrec ((f (lambda () a)) (g (lambda () (set! q "hi\n"))))
                    (g)
                    (lambda () (f)))))
        (list (eq? a (a)) q)))
    '(#t "hi\n"))
  (error? ; should complain about g
    (let ()
        (define f (letrec ((f* (lambda () (g)))) (f*)))
        (define g (letrec ((g* (lambda () (f)))) (g*)))
        g))
  (internal-defines-as-letrec*)
  (begin (internal-defines-as-letrec* #f) (not (internal-defines-as-letrec*)))
  (error? ; might complain about f or g
    (let ()
        (define f (letrec ((f* (lambda () (g)))) (f*)))
        (define g (letrec ((g* (lambda () (f)))) (g*)))
        g))
  (begin (internal-defines-as-letrec* #t) (internal-defines-as-letrec*))
  (error?
    (letrec* ((a (lambda () (c)))
              (b (lambda () (d)))
              (c (lambda () (f)))
              (d (lambda () (f)))
              (e (cons (a) (lambda () (b))))
              (f ((cdr e))))
      7))
  (error?
    (letrec* ((a (lambda () (b)))
              (b (lambda () (c)))
              (c (a)))
      7))
 ; verify that cpletrec output is straight rec-binding:
  (equal? (letrec* ((e (lambda (x) (or (= x 0) (o (- x 1)))))
                    (o (lambda (x) (and (not (= x 0)) (e (- x 1))))))
            (list (e 7) (o 7) (e 6) (o 6)))
          '(#f #t #t #f))
 ; verify that cpletrec output is straight rec-binding:
  (letrec ([a (letrec* ([b (lambda () (c))]
                        [c (lambda () a)]
                        [d (lambda () (b))])
                (lambda () (d)))])
    (eq? a (a)))
 ; check for warnings when requested
  (eq?
    (parameterize ([undefined-variable-warnings "yes please!"])
      (undefined-variable-warnings))
    #t)
  (warning? ; possible undefined variable
    (parameterize ([undefined-variable-warnings #t] [optimize-level 2])
      (eval '(let () (define x x) x))))
  (error? ; undefined variable
    (parameterize ([undefined-variable-warnings #f] [optimize-level 2])
      (eval '(let () (define x x) x))))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda () (pretty-print '(let () (define x x) (x y))))
      'replace)
    #t)
  (warning? ; possible undefined variable, with source info
    (parameterize ([undefined-variable-warnings #t] [optimize-level 2])
      (compile-file "testfile")))
  (error? ; undefined variable, with source info
    (parameterize ([undefined-variable-warnings #f] [optimize-level 2])
      (compile-file "testfile")
      (load "testfile.so")))
)

(mat cpvalid2 ; from Dipa
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)] [b (a)] [c (cons 1 2)]) b))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)][b (let ((d a)) (d))][c (cons 1 2)]) b))
  
  (error? ; undefined variable c
    (letrec* ([a (let ([d (lambda () c)]) (d))][b a][c (cons 1 2)]) b))
  
  (error? ; undefined variable c
    (letrec* ([a (letrec* ([b (lambda () c)][d (b)][c (cons 1 2)]) d)]) a))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)][b (lambda (f) (f))][d (b a)][c (cons 1 2)]) d))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)][b (lambda () a)][d ((b))][c (cons 1 2)]) d))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () (set! c d))][b (a)][c (cons 1 2)]) b))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () (set! d c))][b (a)][c (cons 1 2)]) d))
  
  (equal?
    (letrec* ([a (lambda () c)]
              [b (if #t a (a))]
              [c (cons 1 2)])
      (b))
    '(1 . 2))
  
  (error? ; undefined variable c
    (letrec* ([a (lambda () c)][b (if #t (a) a)][c (cons 1 2)]) b))
  
  (error? ; undefined variable a
    (letrec ([a (letrec* ([b (lambda () a)]) (b))][c (cons 1 2)]) (cons a c)))
)

(mat rec
    (let ((f (rec g (lambda () g)))) (eq? f (f)))
    (let ((f (rec g (lambda (x) (if (zero? x) 1 (* x (g (1- x))))))))
        (= (f 4) 24))
 )

(mat define
    (begin (define xxxx 'xxxxval) #t)
    (and (top-level-bound? 'xxxx) (eqv? (top-level-value 'xxxx) 'xxxxval))
    (begin (define (ffff x) (+ x x)) #t)
    (and (top-level-bound? 'ffff) (eqv? ((top-level-value 'ffff) 17) 34))
    (begin (define (eeee . l) l) #t)
    (equal? (eeee 1 2 3) '(1 2 3))
    (begin (define (dddd x . l) (cons x l)) #t)
    (equal? (dddd 1 2 3 4) '(1 2 3 4))
    ((lambda (x)
        (define yyyy x)
        (define (gggg y) (+ yyyy y))
        (and (not (top-level-bound? 'yyyy))
             (not (top-level-bound? 'gggg))
             (eqv? (gggg 22) 25)))
     3)
    (let ((x 3))
       (define yyyy x)
       (define (gggg y) (+ yyyy y))
       (and (not (top-level-bound? 'yyyy))
            (not (top-level-bound? 'gggg))
            (eqv? (gggg 22) 25)))
    (let* ((x 3))
       (define yyyy x)
       (define (gggg y) (+ yyyy y))
       (and (not (top-level-bound? 'yyyy))
            (not (top-level-bound? 'gggg))
            (eqv? (gggg 22) 25)))
    (letrec ((x 3))
       (define yyyy x)
       (define (gggg y) (+ yyyy y))
       (and (not (top-level-bound? 'yyyy))
            (not (top-level-bound? 'gggg))
            (eqv? (gggg 22) 25)))
    (let ()
       (begin (define x 3) (define y 4))
       (begin)
       (begin (define z 5))
       (= (+ (* x x) (* y y)) (* z z)))
    (error? (lambda () 0 (define x 3) x))
    (error? (lambda () 0 (begin (define x 3)) x))
    (error? (lambda () 0 (begin) x))
    (error? (case-lambda [() 0 (define x 3) x]))
    (error? (let () 0 (define x 3) x))
    (error? (let* () 0 (define x 3) x))
    (error? (letrec () 0 (define x 3) x))
    (error? (if (define x 3) x x))
 )

(mat define-values
  (begin (define-values ($dv-x $dv-y) (values 'a 'b)) #t)
  (eq? $dv-x 'a)
  (eq? $dv-y 'b)
  (begin (define-values $dv-r (values)) #t)
  (equal? $dv-r '())
  (begin (define-values $dv-r (values 1)) #t)
  (equal? $dv-r '(1))
  (begin (define-values $dv-r (values 1 2 3 4 5)) #t)
  (equal? $dv-r '(1 2 3 4 5))
  (begin (define-values ($dv-x $dv-y . $dv-r) (values 1 2 3 4 5)) #t)
  (eqv? $dv-x 1)
  (eqv? $dv-y 2)
  (equal? $dv-r '(3 4 5))
  (begin (define-values ($dv-x $dv-y) (div-and-mod 19 4)) #t)
  (eqv? $dv-x 4)
  (eqv? $dv-y 3)
  (begin (define-values ($dv-x $dv-y . $dv-z) (div-and-mod 19 4)) #t)
  (eqv? $dv-x 4)
  (eqv? $dv-y 3)
  (equal? $dv-z '())
  (error? ; invalid number of arguments
    (define-values ($dv-x . $dv-r) (values)))
  (error? ; invalid number of arguments
    (define-values ($dv-x $dv-y . $dv-r) (values)))
  (error? ; invalid number of arguments
    (define-values ($dv-x $dv-y . $dv-r) (values 1)))
  (error? ; invalid number of arguments
    (define-values ($dv-x $dv-y $dv-z . $dv-r) (div-and-mod 19 4)))
  (error? ; invalid number of arguments
    (define-values ($dv-x) (div-and-mod 19 4)))
  (error? ; invalid number of arguments
    (define-values () (div-and-mod 19 4)))
  (error? ; duplicate variable name
    (define-values ($dv-x $dv-x) (div-and-mod 19 4)))
  (error? ; duplicate variable name
    (define-values ($dv-x . $dv-x) (div-and-mod 19 4)))
  (equal?
    (let ()
      (define-values (x y) (values 'a 'b))
      (list x y))
    '(a b))
  (equal?
    (let ()
      (define-values r (values))
      r)
    '())
  (equal?
    (let ()
      (module (r)
        (define-values r (values 1)))
      r)
    '(1))
  (equal?
    (let ()
      (define-values r (values 1 2 3 4 5))
      r)
    '(1 2 3 4 5))
  (equal?
    (let ()
      (define-values (x y . r) (values 1 2 3 4 5))
      (vector x y r))
    '#(1 2 (3 4 5)))
  (equal?
    (let ()
      (define-values (x y) (div-and-mod 19 4))
      (list y x))
    '(3 4))
  (equal?
    (let ()
      (define-values (x y . z) (div-and-mod 19 4))
      (vector z x y))
    '#(() 4 3))
  (error? ; invalid number of arguments
    (let ()
      (define-values (x . r) (values))
      r))
  (error? ; no expressions in body
    (let ()
      (define-values (x y . r) (values))))
  (error? ; invalid number of arguments
    (let ()
      (define-values (x y . r) (values 1))
      x))
  (error? ; invalid number of arguments
    (let ()
      (define-values (x y z . r) (div-and-mod 19 4))
      x))
  (error? ; invalid number of arguments
    (let ()
      (define-values (x) (div-and-mod 19 4))
      x))
  (error? ; invalid number of arguments
    (let ()
      (define-values () (div-and-mod 19 4))
      #t))
  (error? ; duplicate variable name
    (let ()
      (define-values (x x) (div-and-mod 19 4))
      x))
  (error? ; duplicate variable name
    (let ()
      (define-values (x . x) (div-and-mod 19 4))
      x))
  (begin
    (library ($dv-foo) (export $dv-foo-x) (import (chezscheme))
      (define-values $dv-foo-x (div-and-mod 19 4)))
    #t)
  (equal?
    (let () (import ($dv-foo)) $dv-foo-x)
    '(4 3))
  (begin (import ($dv-foo)) #t)
  (equal? $dv-foo-x '(4 3))
  (begin
    (library ($dv-foo1) (export $dv-foo1-x) (import (chezscheme))
      (define-values ($dv-foo1-x . r) (values)))
    #t)
  (error? ; invalid number of arguments
    (let () (import ($dv-foo1)) $dv-foo1-x))
  (error? ; duplicate variable name
    (library ($dv-foo2) (export $dv-foo2-x) (import (chezscheme))
      (define-values ($dv-foo2-x . $dv-foo2-x) (values))
      $dv-foo2-x))
 ; make sure pattern variables and ellipses on RHS don't screw us up
  (eqv?
    (let ()
      (define-syntax q
        (lambda (x)
          (syntax-case x ()
            [(_ dots) (free-identifier=? #'dots #'(... ...)) 3])))
      (define-values (a) (q ...))
      a)
    3)
  (equal?
    (let ()
      (define-syntax q
        (lambda (x)
          (syntax-case x ()
            [(_ dots) (free-identifier=? #'dots #'(... ...)) 3])))
      (define-values a (q ...))
      a)
    '(3))
  (equal?
    (let ()
      (define-syntax q
        (lambda (x)
          (syntax-case x ()
            [(_ dots) (free-identifier=? #'dots #'(... ...)) 3])))
      (define-values (a . b) (q ...))
      (list a b))
    '(3 ()))
  (equal?
    (syntax-case '(a b c) ()
      [(x ...)
       (let ()
         (define-values (args) #'(x ...))
         args)])
    '(a b c))
  (equal?
    (syntax-case '(a b c) ()
      [(x ...)
       (let ()
         (define-values (args . rot) (values #'(x ...) #'(x ...) 3))
         (list args rot))])
    '((a b c) ((a b c) 3)))
  (equal?
   (let ()
     (define x 1)
     (define-values ()
       (begin
         "don't interrupt definitions"
         (values)))
     (define y 2)
     (list x y))
   '(1 2))
)

(mat assimilation
  (syntax-case (parameterize ([run-cp0 (lambda (cp0 x) (cp0 x))]
                              [#%$suppress-primitive-inlining #f]
                              [optimize-level 2])
                 (expand/optimize
                   '(letrec* ([x (let ([y 0])
                                   (lambda ()
                                     (set! y (- y 1))
                                     y))]
                              [z (lambda () (x))])
                      (z)
                      (x))))
               (lambda set! - $primitive)
    [(let ([y1 0])
       (set! y2 (#2%- y3 1))
       (set! y4 (#2%- y5 1))
       y6)
     #t]
    [_ #f])
  (syntax-case (parameterize ([run-cp0 (lambda (cp0 x) (cp0 x))]
                              [#%$suppress-primitive-inlining #f]
                              [optimize-level 2])
                 (expand/optimize
                   '(letrec ([x (let ([y 0])
                                  (lambda ()
                                    (set! y (- y 1))
                                    y))]
                             [z (lambda () (x))])
                      (z)
                      (x))))
               (lambda set! - $primitive)
    [(let ([y1 0])
       (set! y2 (#2%- y3 1))
       (set! y4 (#2%- y5 1))
       y6)
     #t]
    [_ #f])
  (syntax-case (parameterize ([run-cp0 (lambda (cp0 x) (cp0 x))]
                              [#%$suppress-primitive-inlining #f]
                              [optimize-level 2])
                 (expand/optimize
                   '(letrec* ([w 15]
                              [x (let ([y w])
                                   (lambda ()
                                     (set! y (- y 1))
                                     y))]
                              [z (lambda () (x))])
                      (z)
                      (x))))
               (lambda set! - $primitive)
    [(let ([y1 15])
      (set! y2 (#2%- y3 1))
      (set! y4 (#2%- y5 1))
      y6)
     #t]
    [_ #f])
  (equal?
    (let ([f (letrec ([e? (lambda (x) (or (zero? x) (o? (- x 1))))]
                      [o? (lambda (x) (not (e? x)))])
               (lambda (a b) (vector (e? a) (e? b) (o? a) (o? b))))])
      (f 3 0))
    '#(#f #t #t #f))
  (equal?
    (let ([f (letrec ([q? (lambda (x) (not (p? x)))]
                      [p? (lambda (x) (> x 0))])
               (lambda (a b) (vector (p? a) (p? b) (q? a) (q? b))))])
      (f 3 -3))
    '#(#t #f #f #t))
  (equal?
    (let ([f (letrec* ([x 5] [y (+ x x)])
               (lambda ()
                 (set! x (+ x y))
                 (set! y (+ y x))
                 (cons x y)))])
      (let ([t (f)]) (list t (f))))
    '((15 . 25) (40 . 65)))
  (equal?
    (letrec ([f (letrec* ([g (lambda (x)
                               (lambda (y)
                                 (if (= x y) 0 (+ 2 (h (- y 1))))))]
                          [x0 17]
                          [h (g x0)])
                  (lambda (y1 y2) (cons (h y1) (h y2))))])
      (list (f 20 25) (f 28 31)))
    '((6 . 16) (22 . 28)))
  (equal?
    (letrec ([f (letrec* ([g (lambda (n f)
                               (if (= n 0)
                                   f
                                   (g (- n 1) (lambda (m) (f (+ m 1))))))]
                          [q 7]
                          [h (g q (lambda (x) (* x 2)))])
                  (lambda (y1 y2 y3) (list (h y1) (h y2) ((g 5 values) 7))))])
      (vector (f 1 2 3) (f 4 5 6)))
    '#((16 18 12) (22 24 12)))
  (equal?
    (letrec ([f (letrec* ([g (values
                               (lambda (n f)
                                 (if (= n 0)
                                     f
                                     (g (- n 1) (lambda (m) (f (+ m 1)))))))]
                          [q 7]
                          [h (g q (lambda (x) (* x 2)))])
                  (lambda (y1 y2 y3) (list (h y1) (h y2) ((g 5 values) 7))))])
      (vector (f 1 2 3) (f 4 5 6)))
    '#((16 18 12) (22 24 12)))
  (equal?
    (letrec ([f (letrec* ([g (lambda (n f)
                               (if (= n 0)
                                   f
                                   (g (- n 1) (lambda (m) (f (+ m 1))))))]
                          [g^ g]
                          [g^^ g^])
                  (lambda (y1 y2 y3)
                    (when #f (set! g 0) (set! g^ 1) (set! g^^ 2))
                    (list ((g y1 values) y2)
                          ((g^ y2 (lambda (x) (* x x))) y3)
                          ((g^^ y3 (lambda (x) (- x))) y1))))])
      (vector (f 1 2 3) (f 4 5 6)))
    '#((3 25 -4) (9 121 -10)))
)

(mat set!
    (begin (set! foo 'hello) (eq? foo 'hello))
    (let ([x 'a]) (set! x 'b) (eq? x 'b))
    (let ([x 'a])
       (let ([f (lambda () (set! x 'b))])
          (and (eq? x 'a) (begin (f) (eq? x 'b)))))
   ; test gensym set!/reference
    (equal? (begin (set! #0=#{a |pig|} '#0#) (set! #1=#{b |sty|} #0#) #1#) '#0#)
 )

(mat fluid-let
    (fluid-let () #t)
    (eq? (fluid-let () (define x 4) x) 4)
    (let* ((x 'a) (f (lambda () x)))
        (and
            (fluid-let ((x 'b))
                (and (eq? x 'b) (eq? (f) 'b)))
            (eq? x 'a)
            (eq? (f) 'a)))
    (let* ((x 'a) (f (lambda () x)))
        (and
            (call/cc
                (lambda (return)
                    (fluid-let ((x 'b))
                        (return (and (eq? x 'b) (eq? (f) 'b))))))
            (eq? x 'a)
            (eq? (f) 'a)))
    (equal?
      (let* ((x 'a) (f (lambda () x)))
          ((call/cc
              (lambda (return)
                  (fluid-let ((x 'b))
                      (call/cc
                          (lambda (back)
                               (return back)))
                      (let ((ans (f))) (lambda (y) (list ans x))))))
            '()))
      '(b a))
    (eqv?
      (let ([x 75])
        (fluid-let ([x 23] [x 23]) 0)
        x)
      75)
 )

;(mat variable
;   (eq? (fluid-let ([car 3])
;           ((parameterize ([optimize-level 2])
;               (eval '(lambda () car)))))
;        car)
;   (eq? (fluid-let ([car 3])
;           ((parameterize ([$compiling-system-code #t])
;               (eval '(lambda () car)))))
;        car)
;   (eq? ((parameterize ([$compiling-system-code #t])   
;            (eval '(lambda () $oblist))))
;        (parameterize ([$compiling-system-code #t])
;            (eval '$oblist)))
;   (error? ((parameterize ([optimize-level 2])
;               (eval '(lambda () (set! car 3))))))
; )

(mat mrvs
  (error?
    (values))
  (error?
    (if (values 1 2 3) 4 5))
  (error?
    (values 1 2 3))
  (eq?
    (values 2)
    2)
  (eq?
    (let ((f (lambda () (values)))) 
      (+ 2 (call-with-values f (lambda () 5))))
    7)
  (error?
    (let ((f (lambda () (values)))) (+ 2 (f))))
  (eq?
    (call-with-values
      (lambda () (begin 5 (values 2 3)))
      (lambda (x y) (+ x y)))
    5)
  (error?
    (call-with-values
      (lambda () (begin 5 (values 2)))
      (lambda (x y) (+ x y))))
  (eq?
    (call-with-values
      (lambda () (begin 5 (values 1 2)))
      (lambda (x y) (+ x y)))
    3)
  (eq?
    (call-with-values
      (lambda () (values 2 3))
      (lambda (x y) (+ x y)))
    5)
  (equal?
    (let ((f (lambda () (values 2 3)))
          (g (lambda (x y) (cons x y))))
      (call-with-values f g))
    '(2 . 3))
  (eq?
    (let ((f (lambda () (lambda () (values 2 3))))
          (g (lambda (x) x)))
      (call-with-values (call-with-values f g) +))
    5)
  (eq?
    (let ((f (lambda () (lambda () (values 2 3)))))
      (call-with-values (car (call-with-values f list)) +))
    5)
  (equal?
    (cons 1 (let ((f (lambda () (values 2 3)))) (call-with-values f list)))
    '(1 2 3))
  (eq?
    (let ((f (lambda (g h) (+ 1 (call-with-values g h)))))
      (f (lambda () (values 1 2))
         (lambda (x y) (+ x y))))
    4)
  (eq?
    (let ((f (lambda (f g) (call-with-values f g))))
      (f (lambda () (call/cc (lambda (k) (values 5 k))))
         (lambda (x k) (if (= x 5) (k 0 k) 1))))
    1)
  (eq?
    (+ 2 (call/cc
           (lambda (k)
             (let ((f (lambda () (k 5))))
               (call-with-values f list)))))
    7)
  (eq?
    (let ((f (lambda () 
               (let ((f (lambda (f g) (call-with-values f g))))
                 (f (lambda ()
                      (call/cc
                        (lambda (k)
                          (values 0 k))))
                    (lambda (x k)
                      (call/cc
                        (lambda (k1)
                          (k 1 k1))))))))
          (g (lambda (x y) x)))
      (call-with-values f g))
    1)
  (bignum?
    (letrec ((f (lambda (x)
                  (if (= x 0)
                      (values 1 0 0)
                      (let ((g (lambda (u v w)
                                 (values (* x u) (+ v 1) (+ w 2)))))
                        (call-with-values
                          (lambda () (f (- x 1)))
                          g))))))
      (let ((h (lambda (x y z) x)))
        (call-with-values
          (lambda () (f 2000))
          h))))
  (equal?
    (let ((h (lambda (x) (lambda (y z) (list x y z))))
          (g (lambda (x) (lambda () (values x 3)))))
      (cons 0 (call-with-values (g 2) (h 1))))
    '(0 1 2 3))
  (eqv? (call-with-values (lambda () (apply values (make-list 1000 1))) +)
        1000)
  (equal? (call-with-values (lambda () (if (random 10) 2 3)) list)
          '(2))
  (equal? (call-with-values (case-lambda (x x) (() 3)) list) '(()))
  (eqv? (let ([f (lambda () (values 1 2 3))])
          (+ 2 (call-with-values f (lambda x (length x)))))
      5)
  (equal? (let ((x list)) (call-with-values (lambda () (set! x +) 3) x))
          '(3))
  (error? (call-with-values values (lambda (x) x)))
  (error? (call-with-values values (lambda (x y) x)))
  (error? (let ((f values)) (call-with-values f (lambda (x y) x))))
  (equal?
    (let ()
      (define f
        (lambda (a b c)
          (call-with-values
          (let ((x values)) (lambda () (x 1 2)))
            (lambda (d e)
              (list a b c d e)))))
      (f 3 4 5))
    '(3 4 5 1 2))
  (eqv?
    (let ()
        (define f1
          (lambda (x) (values 1 0)))
        (define f2
          (lambda (a)
            (vector-ref a 0)
            (call-with-values
              (lambda () (f1 a))
              (lambda (d e) d)))) 
        (f2 '#(a)))
    1)
  (equal?
    (let ()
        (define f1 (lambda (x) (lambda () (values 1 2))))
        (define f2
          (lambda (a)
            (random 10)
            (call-with-values
              (f1 a)
              (lambda (x y)
                (random 20)
                (list a x y)))))
        (f2 0))
    '(0 1 2))
  (null? (call-with-values
           (lambda () (call/cc (lambda (k) (values))))
           (lambda args args)))
  (null? (call-with-values
           (lambda () (call/cc (lambda (k) (k))))
           (lambda args args)))
  (equal?
    (call-with-values
      (lambda () (call/cc (lambda (k) (k 'a 'b 'c))))
      (lambda args args))
    '(a b c))
  (equal?
    (call-with-values
      (lambda () (call/cc (lambda (k) (values 'a 'b 'c))))
      (lambda args args))
    '(a b c))
  (null? (call-with-values
           (lambda () (dynamic-wind values values values))
           list))
  (equal?
    (call-with-values
      (lambda () (call/cc (lambda (k) (values 1 2 3 4 5 6 7 8 9 10))))
      list)
    '(1 2 3 4 5 6 7 8 9 10))
  (eqv?
    (letrec ((z 2)
             (f (lambda () (values 1 z)))
             (g (lambda (x y) (values x y z))))
      (call-with-values
        (lambda ()
          (call-with-values
            f
            (lambda (z b) (g z b))))
        (lambda (c d e)
          (+ c d e z))))
    7)
  (or (= (optimize-level) 3)
      (guard (c [(not (warning? c)) (collect) #t])
        (if (call-with-values
              current-output-port
              (lambda (v out) (current-output-port)))
          1
          2)))
  (equal?
    (let ()
      (define split
        (lambda (ls)
          (if (or (null? ls) (null? (cdr ls)))
              (values ls '())
              (call-with-values
                (lambda () (split (cddr ls)))
                (lambda (odds evens)
                  (values (cons (car ls) odds)
                          (cons (cadr ls) evens)))))))
       (call-with-values
         (lambda () (split '(a b c d e f)))
         vector))
    '#((a c e) (b d f)))

  ; test chains of consumers
  (begin
    (define-syntax $mrvs-a
      (syntax-rules ()
        [(_) ($mrvs-f0)]
        [(_ f1 f2 ...)
         (let ([f1 (lambda (a b c d) (values d a b c))])
           (call-with-values (lambda () ($mrvs-a f2 ...)) f1))]))
    (define $mrvs-f0 (lambda () (values 1 2 3 4)))
    (define $mrvs-list (lambda args args))
    #t)

  ; test chains of consumers ending in a non-tail call
  (equal?
    (call-with-values (lambda () ($mrvs-a)) $mrvs-list)
    '(1 2 3 4))

  (equal?
    (call-with-values (lambda () ($mrvs-a f1)) $mrvs-list)
    '(4 1 2 3))

  (equal?
    (call-with-values (lambda () ($mrvs-a f1 f2 f3 f4)) $mrvs-list)
    '(1 2 3 4))

  ; test chains of consumers ending in a tail call
  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () ($mrvs-a)) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () ($mrvs-a f1)) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(4 1 2 3))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () ($mrvs-a f1 f2 f3 f4)) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (call-with-values (lambda () ($mrvs-a f1 f2 f3)) foo)))
    #t)
  (equal? ($mrvs-q $mrvs-list) '(2 3 4 1))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (lambda ()
          (call-with-values (lambda () ($mrvs-a f1 f2 f3)) foo))))
    #t)
  (equal? (($mrvs-q $mrvs-list)) '(2 3 4 1))

  ; test chains of consumers ending in a let-values
  (equal?
    (let-values ([(a . r) ($mrvs-a)]) (cons r a))
    '((2 3 4) . 1))

  (equal?
    (let-values ([(a . r) ($mrvs-a f1)]) (cons r a))
    '((1 2 3) . 4))

  (equal?
    (let-values ([(a . r) ($mrvs-a f1 f2 f3 f4)]) (cons r a))
    '((2 3 4) . 1))

  ; test chains of consumers ending in a let-values-like call-with-values
  (equal?
    (call-with-values
      (lambda () ($mrvs-a))
      (lambda (a b . r) (cons* r b a)))
    '((3 4) 2 . 1))

  (equal?
    (call-with-values
      (lambda () ($mrvs-a f1))
      (lambda (a b . r) (cons* r b a)))
    '((2 3) 1 . 4))

  (equal?
    (call-with-values
      (lambda () ($mrvs-a f1 f2 f3 f4))
      (lambda (a b . r) (cons* r b a)))
    '((3 4) 2 . 1))

  ; test chains of consumers w/fi as free variables
  (begin
    (define-syntax $mrvs-a
      (syntax-rules ()
        [(_ f ...)
         (let ([x 17])
           (let ([f (lambda (y a b c d) (values x d a b c))] ...)
             (set! x (* x 4))
             (lambda () ($mrvs-b f ...))))]))
    (define-syntax $mrvs-b
      (syntax-rules ()
        [(_) ($mrvs-f0)]
        [(_ f1 f2 ...) (call-with-values (lambda () ($mrvs-b f2 ...)) f1)]))
    (define $mrvs-f0 (lambda () (values 0 1 2 3 4)))
    (define $mrvs-list (lambda args args))
    #t)

  ; test chains of consumers ending in a non-tail call
  (equal?
    (call-with-values (lambda () (($mrvs-a))) $mrvs-list)
    '(0 1 2 3 4))

  (equal?
    (call-with-values (lambda () (($mrvs-a f1))) $mrvs-list)
    '(68 4 1 2 3))

  (equal?
    (call-with-values (lambda () (($mrvs-a f1 f2 f3 f4))) $mrvs-list)
    '(68 1 2 3 4))

  ; test chains of consumers ending in a tail call
  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () (($mrvs-a))) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(0 1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () (($mrvs-a f1))) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(68 4 1 2 3))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () (($mrvs-a f1 f2 f3 f4))) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(68 1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (call-with-values (lambda () (($mrvs-a f1 f2 f3))) foo)))
    #t)
  (equal? ($mrvs-q $mrvs-list) '(68 2 3 4 1))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (lambda ()
          (call-with-values (lambda () (($mrvs-a f1 f2 f3))) foo))))
    #t)
  (equal? (($mrvs-q $mrvs-list)) '(68 2 3 4 1))

  ; test chains of consumers ending in a let-values
  (equal?
    (let-values ([(x a . r) (($mrvs-a))]) (cons* x r a))
    '(0 (2 3 4) . 1))

  (equal?
    (let-values ([(x a . r) (($mrvs-a f1))]) (cons* x r a))
    '(68 (1 2 3) . 4))

  (equal?
    (let-values ([(x a . r) (($mrvs-a f1 f2 f3 f4))]) (cons* x r a))
    '(68 (2 3 4) . 1))

  ; test chains of consumers ending in a let-values-like call-with-values
  (equal?
    (call-with-values
      (lambda () (($mrvs-a)))
      (lambda (x a b . r) (cons* x r b a)))
    '(0 (3 4) 2 . 1))

  (equal?
    (call-with-values
      (lambda () (($mrvs-a f1)))
      (lambda (x a b . r) (cons* x r b a)))
    '(68 (2 3) 1 . 4))

  (equal?
    (call-with-values
      (lambda () (($mrvs-a f1 f2 f3 f4)))
      (lambda (x a b . r) (cons* x r b a)))
    '(68 (3 4) 2 . 1))

  (begin
    (define-syntax $mrvs-qcons (lambda (x) #`'#,cons))
    (define-syntax $mrvs-qvalues (lambda (x) #`'#,(lambda args (apply values args))))
    (define $mrvs-f (lambda () (values 1 2)))
    #t)

  (equal?
    (call-with-values (lambda () (values 1 2)) $mrvs-qcons)
    '(1 . 2))

  (equal?
    (let ([f (lambda () (values 1 2))])
      (call-with-values f $mrvs-qcons))
    '(1 . 2))

  (equal?
    (call-with-values $mrvs-f $mrvs-qcons)
    '(1 . 2))

  (equal?
    (call-with-values (lambda () (call-with-values $mrvs-f $mrvs-qvalues)) $mrvs-qcons)
    '(1 . 2))

  (equal?
    (let ([f (lambda () (call-with-values (lambda () (values 1 2)) $mrvs-qcons))])
      (f))
    '(1 . 2))

  (equal?
    (let ([f (lambda ()
               (let ([f (lambda () (values 1 2))])
                 (call-with-values f $mrvs-qcons)))])
      (f))
    '(1 . 2))

  (equal?
    (let ([f (lambda () (call-with-values $mrvs-f $mrvs-qcons))])
      (f))
    '(1 . 2))

  (equal?
    (let ([f (lambda ()
               (call-with-values
                 (lambda () (call-with-values $mrvs-f $mrvs-qvalues))
                 $mrvs-qcons))])
      (f))
    '(1 . 2))

  (equal?
    (letrec ((f (lambda (x) (values 7 8 9))))
      (let ((h list))
        (call-with-values
          (lambda () (f 0))
          h)))
    '(7 8 9))

  (equal?
    (let-values ([(a . b) (values 1 2 3)]) (cons b a))
    '((2 3) . 1))

  (equal?
    (let ([f (lambda (x) (values x (+ x 1)))])
      (let-values ([(a b) (f 3)]) (cons b a)))
    '(4 . 3))

  ; let-values inserts an "else" (effectively) clause---the following doesn't
  (equal?
    (let ([f (lambda (x) (values x (+ x 1)))])
      (call-with-values
        (lambda () (f 3))
        (lambda (a b) (cons b a))))
    '(4 . 3))

  (equal?
    (let ([f (lambda (x) (values x (+ x 1)))]) (begin (f 3) 7))
    7)

  (equal?
    ((lambda (a . b) (cons b a)) 7 8 9)
    '((8 9) . 7))

  (equal?
    (call-with-values
      (lambda ()
        (let ([f (lambda (x) (values x (+ x 1) (+ x 2)))]
              [g (lambda () 7)])
          (call-with-values g f)))
      list*)
    '(7 8 . 9))

  (equal?
    (let ([q (lambda () (let ([f (lambda (x) (values x (+ x 1) (+ x 2)))]
                              [g (lambda () 7)])
                          (call-with-values g f)))])
      (call-with-values q (lambda (a b c) (list c b a))))
    '(9 8 7))

  (equal?
    (let ([q (lambda () (let ([f (lambda (x y) (values x (+ x 1) (+ y 2)))]
                              [g (lambda () (values 7 8))])
                          (call-with-values g f)))])
      (call-with-values q (lambda (a b c) (list c b a))))
    '(10 8 7))
  (error? ; unbound variable $mrvs-foo
    (call-with-values (lambda () (set! $mrvs-foo list) (values 3 2 1)) $mrvs-foo))
  (begin
    (define $mrvs-foo 17)
    #t)
  (error? ; attempt to call nonprocedure 17
    (call-with-values (lambda () (set! $mrvs-foo list) (values 3 2 1)) $mrvs-foo))
  (begin
    (define $mrvs-foo vector)
    #t)
  (equal?
    (call-with-values (lambda () (set! $mrvs-foo list) (values 3 2 1)) $mrvs-foo)
    '#(3 2 1))
  (or (= (optimize-level) 3)
      (eqv?
        (let ([x 0] [f (lambda (x) (values 1 2))])
          (guard (c [#t x])
            (call-with-values
              (begin (set! x (+ x 3)) f)
              (begin (set! x (+ x 7)) 'oops))))
        10))
  (or (= (optimize-level) 3)
      (eqv?
        (let ([x 0] [f (lambda (x y z) (list z y x))])
          (guard (c [#t x])
            (#2%call-with-values
              (begin (set! x (+ x 3)) 'oops)
              (begin (set! x (+ x 7)) f))))
        10))

  ; testing of chains that do not get washed away into direct calls with mvlet
  (begin
    (define-syntax $mrvs-c
      (lambda (x)
        (define help
          (lambda (f* k)
            (if (null? f*)
                (k #'($mrvs-f0))
                (with-syntax ([f1 (car f*)])
                  #`(let ([f1 (lambda (a b c d) (values d a b c))])
                      ; using random to confuse cp0 until it gets smart enough to defeat this
                      (let ([f1 (if (eqv? (random 5) 10) #f f1)])
                        #,(help (cdr f*)
                            (lambda (body)
                              (k #`(call-with-values (lambda () #,body) f1))))))))))
        (syntax-case x ()
          [(_) #'($mrvs-f0)]
          [(_ f1 f2 ...) (help #'(f1 f2 ...) values)])))
    (define $mrvs-f0 (lambda () (values 1 2 3 4)))
    (define $mrvs-list (lambda args args))
    #t)

  ; test chains of consumers ending in a non-tail call
  (equal?
    (call-with-values (lambda () ($mrvs-c)) $mrvs-list)
    '(1 2 3 4))

  (equal?
    (call-with-values (lambda () ($mrvs-c f1)) $mrvs-list)
    '(4 1 2 3))

  (equal?
    (call-with-values (lambda () ($mrvs-c f1 f2 f3 f4)) $mrvs-list)
    '(1 2 3 4))

  ; test chains of consumers ending in a tail call
  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () ($mrvs-c)) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () ($mrvs-c f1)) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(4 1 2 3))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () ($mrvs-c f1 f2 f3 f4)) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (call-with-values (lambda () ($mrvs-c f1 f2 f3)) foo)))
    #t)
  (equal? ($mrvs-q $mrvs-list) '(2 3 4 1))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (lambda ()
          (call-with-values (lambda () ($mrvs-c f1 f2 f3)) foo))))
    #t)
  (equal? (($mrvs-q $mrvs-list)) '(2 3 4 1))

  ; test chains of consumers ending in a let-values
  (equal?
    (let-values ([(a . r) ($mrvs-c)]) (cons r a))
    '((2 3 4) . 1))

  (equal?
    (let-values ([(a . r) ($mrvs-c f1)]) (cons r a))
    '((1 2 3) . 4))

  (equal?
    (let-values ([(a . r) ($mrvs-c f1 f2 f3 f4)]) (cons r a))
    '((2 3 4) . 1))

  ; test chains of consumers ending in a let-values-like call-with-values
  (equal?
    (call-with-values
      (lambda () ($mrvs-c))
      (lambda (a b . r) (cons* r b a)))
    '((3 4) 2 . 1))

  (equal?
    (call-with-values
      (lambda () ($mrvs-c f1))
      (lambda (a b . r) (cons* r b a)))
    '((2 3) 1 . 4))

  (equal?
    (call-with-values
      (lambda () ($mrvs-c f1 f2 f3 f4))
      (lambda (a b . r) (cons* r b a)))
    '((3 4) 2 . 1))

  ; test chains of consumers w/fi as free variables
  (begin
    (define-syntax $mrvs-c
      (syntax-rules ()
        [(_ f ...)
         (let ([x 17])
           (let ([f (lambda (y a b c d) (values x d a b c))] ...)
             (let ([f (if (eqv? (random 5) 10) #f f)] ...)
               (set! x (* x 4))
               (lambda () ($mrvs-d f ...)))))]))
    (define-syntax $mrvs-d
      (syntax-rules ()
        [(_) ($mrvs-f0)]
        [(_ f1 f2 ...) (call-with-values (lambda () ($mrvs-d f2 ...)) f1)]))
    (define $mrvs-f0 (lambda () (values 0 1 2 3 4)))
    (define $mrvs-list (lambda args args))
    #t)

  ; test chains of consumers ending in a non-tail call
  (equal?
    (call-with-values (lambda () (($mrvs-c))) $mrvs-list)
    '(0 1 2 3 4))

  (equal?
    (call-with-values (lambda () (($mrvs-c f1))) $mrvs-list)
    '(68 4 1 2 3))

  (equal?
    (call-with-values (lambda () (($mrvs-c f1 f2 f3 f4))) $mrvs-list)
    '(68 1 2 3 4))

  ; test chains of consumers ending in a tail call
  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () (($mrvs-c))) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(0 1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () (($mrvs-c f1))) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(68 4 1 2 3))

  (begin
    (define $mrvs-q
      (lambda ()
        (call-with-values (lambda () (($mrvs-c f1 f2 f3 f4))) $mrvs-list)))
    #t)
  (equal? ($mrvs-q) '(68 1 2 3 4))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (call-with-values (lambda () (($mrvs-c f1 f2 f3))) foo)))
    #t)
  (equal? ($mrvs-q $mrvs-list) '(68 2 3 4 1))

  (begin
    (define $mrvs-q
      (lambda (foo)
        (lambda ()
          (call-with-values (lambda () (($mrvs-c f1 f2 f3))) foo))))
    #t)
  (equal? (($mrvs-q $mrvs-list)) '(68 2 3 4 1))

  ; test chains of consumers ending in a let-values
  (equal?
    (let-values ([(x a . r) (($mrvs-c))]) (cons* x r a))
    '(0 (2 3 4) . 1))

  (equal?
    (let-values ([(x a . r) (($mrvs-c f1))]) (cons* x r a))
    '(68 (1 2 3) . 4))

  (equal?
    (let-values ([(x a . r) (($mrvs-c f1 f2 f3 f4))]) (cons* x r a))
    '(68 (2 3 4) . 1))

  ; test chains of consumers ending in a let-values-like call-with-values
  (equal?
    (call-with-values
      (lambda () (($mrvs-c)))
      (lambda (x a b . r) (cons* x r b a)))
    '(0 (3 4) 2 . 1))

  (equal?
    (call-with-values
      (lambda () (($mrvs-c f1)))
      (lambda (x a b . r) (cons* x r b a)))
    '(68 (2 3) 1 . 4))

  (equal?
    (call-with-values
      (lambda () (($mrvs-c f1 f2 f3 f4)))
      (lambda (x a b . r) (cons* x r b a)))
    '(68 (3 4) 2 . 1))

  ; regression tests to make sure a bug in the compiler's handling of
  ; values in a single value context is properly handled in all cases
  (begin
    (module $mrvs-double-call (double-call)
      (define split
        (lambda (ls)
          (if (null? ls)
              (values #f '())
              (values #t (cdr ls)))))
      (define double-call
        (lambda (x)
          (let-values ([(x y) (split (split x))])
            (list y x)))))
    #t)

  (error? ; returned two values to single value return context
    (let ()
      (import $mrvs-double-call)
      (double-call '(a b))))

  (error? ; returned two values to single value return context
    (let ()
      (import $mrvs-double-call)
      (double-call '())))

  (error? ; a is not a pair
    (let ()
      (import $mrvs-double-call)
      (double-call 'a)))

  ; regression testing for handling mvset in tail context
  (call-with-values
    (lambda ()
      (call-with-values
        (lambda ()
          (+ (random 1) 7))
        list))
    (lambda l (equal? l '((7)))))

  ; regression testing for handling mvset in predicate context
  (if (call-with-values
        (lambda ()
          (call-with-values
            (lambda ()
              (+ (random 1) 7))
            list))
        (lambda l (equal? l '((7)))))
      #t
      #f)

  ; regression test for handling mvcall with inline form
  (equal?
   '(result x)
   (let ([bx (box #f)])
     (define-record-type thing
       (fields pos)
       (nongenerative #{thing hlg584lmg5htbdauw7dkid2sh-0}))
     (set-box! bx (make-thing 'x))
     (let ([posx (unbox bx)])
       (cons 'result
             (call-with-values
                 (lambda ()
                   (if (thing? posx)
                       ;; compiled as inline load:
                       (thing-pos posx)
                       (do-something-else)))
               list)))))

  ;; regression test to make sure the continuation is well formed when
  ;; an exception handler is call for a wrong number of values are
  ;; returned to a multi-value context
  (begin
    (define ($go-fail-to-get-two-values)
      (call-with-values (lambda () ($get-one-value))
        (lambda (a b) (list a b))))
    (define ($get-one-value)
      (call/cc ; copies return address off stack
       (lambda (k)
         (collect) ; do something non-trivial
         k)))
    (#%$continuation?
     (call/cc
      (lambda (esc)
        (car
          (with-exception-handler
           (lambda (exn)
             (call/cc
              (lambda (k) ; this continuation used to be broken, and
                (collect) ; a GC was the simplest way of detecting it
                (esc k))))
           $go-fail-to-get-two-values))))))

)

(mat let-values
  (error? (let-values))
  (error? (let-values ((x))))
  (error? (let-values ()))
  (error? (let-values (((x) 3))))
  (error? (let-values (((3) 4)) 5))
  (error? (let-values (((3 4) (values 1 2))) 5))
  (error? (let-values (((x . 3) (values 1 2 3))) x))
  (error? (let-values ((() (values 1 2))) 7))
  (error? (let-values (((x) (values 1 2))) x))
  (error? (let-values (((x y z) (values 1 2))) x))
  (error? (let-values (((x y z . w) (values 1 2))) x))
  (error? (let-values ((() 1)) 7))
  (error? (let-values (((x y) 1)) x))
  (error? (let-values (((x y z) 1)) x))
  (error? (let-values (((x y . w) 1)) x))
  (error? (let-values (((x x . w) (values 1 2 3))) (list x w)))
  (error? (let-values (((x y . w) (values 1 2 3)) [(x q) (values 4 5)]) (list x w q)))
  (equal?
    (let-values (((x) 3)) x)
    3)
  (equal?
    (let-values (((x y) (values 3 4))) (list x y))
    '(3 4))
  (equal?
    (let-values (((x . y) (values 3 4))) (list x y))
    '(3 (4)))
  (equal?
    (let-values ((x (values 3 4))) x)
    '(3 4))
  (equal?
    (let-values ((x 3)) x)
    '(3))
  (equal?
    (let-values (((x . y) (values 1 2 3)) ((z) (values 4))) (list x y z))
    '(1 (2 3) 4))
  (equal?
    (let ()
      (define split
        (lambda (ls)
          (if (or (null? ls) (null? (cdr ls)))
              (values ls '())
              (let-values (((odds evens) (split (cddr ls))))
                (values (cons (car ls) odds)
                        (cons (cadr ls) evens))))))
       (call-with-values
         (lambda () (split '(a b c d e f)))
         vector))
    '#((a c e) (b d f)))
  (equal?
    (let ()
      (define f
        (lambda (a b c)
          (let-values (((d e) (let ((x values)) (x 1 2))))
            (list a b c d e))))
      (f 3 4 5))
    '(3 4 5 1 2))
  (equal?
    (let ()
      (define f1
        (lambda (x) (apply values (vector->list x))))
      (define f2
        (lambda (a b)
          (let-values ([(d) (f1 a)]
                       [(e . f) (f1 b)]
                       [(g h i) (f1 b)]
                       [j (f1 b)])
            (list d e f g h i j))))
      (f2 '#(a) '#(b c d)))
    '(a b (c d) b c d (b c d)))
  (eqv?
    (letrec ((z 2)
             (f (lambda () (values 1 z)))
             (g (lambda (x y) (values x y z))))
      (let-values ([(c d e) (let-values ([(z b) (f)]) (g z b))])
        (+ c d e z)))
    7)
  (equal?
    (let ([a 3])
      (let-values ([(a b) (values (+ a 1) (+ a 2))]
                   [(c) (values (+ a 3))])
        (list a b c)))
    '(4 5 6))
 ; make sure pattern variables and ellipses on RHS don't screw us up
  (eqv?
    (let ()
      (define-syntax q
        (lambda (x)
          (syntax-case x ()
            [(_ dots) (free-identifier=? #'dots #'(... ...)) 3])))
      (let-values ([(a) (q ...)]) a))
    3)
  (equal?
    (syntax-case '(a b c) ()
      [(x ...) (let-values ([(args) #'(x ...)]) args)])
    '(a b c))
)

(mat let*-values
  (error? (let*-values))
  (error? (let*-values ((x))))
  (error? (let*-values ()))
  (error? (let*-values (((x) 3))))
  (error? (let*-values (((3) 4)) 5))
  (error? (let*-values (((3 4) (values 1 2))) 5))
  (error? (let*-values (((x . 3) (values 1 2 3))) x))
  (error? (let*-values ((() (values 1 2))) 7))
  (error? (let*-values (((x) (values 1 2))) x))
  (error? (let*-values (((x y z) (values 1 2))) x))
  (error? (let*-values (((x y z . w) (values 1 2))) x))
  (error? (let*-values ((() 1)) 7))
  (error? (let*-values (((x y) 1)) x))
  (error? (let*-values (((x y z) 1)) x))
  (error? (let*-values (((x y . w) 1)) x))
  (error? (let*-values (((x x . w) (values 1 2 3))) (list x w)))
  (equal?
    (let*-values (((x) 3)) x)
    3)
  (equal?
    (let*-values (((x y) (values 3 4))) (list x y))
    '(3 4))
  (equal?
    (let*-values (((x . y) (values 3 4))) (list x y))
    '(3 (4)))
  (equal?
    (let*-values ((x (values 3 4))) x)
    '(3 4))
  (equal?
    (let*-values ((x 3)) x)
    '(3))
  (equal?
    (let*-values (((x . y) (values 1 2 3)) ((z) (values 4))) (list x y z))
    '(1 (2 3) 4))
  (equal?
    (let ()
      (define split
        (lambda (ls)
          (if (or (null? ls) (null? (cdr ls)))
              (values ls '())
              (let*-values (((odds evens) (split (cddr ls))))
                (values (cons (car ls) odds)
                        (cons (cadr ls) evens))))))
       (call-with-values
         (lambda () (split '(a b c d e f)))
         vector))
    '#((a c e) (b d f)))
  (equal?
    (let ()
      (define f
        (lambda (a b c)
          (let*-values (((d e) (let ((x values)) (x 1 2))))
            (list a b c d e))))
      (f 3 4 5))
    '(3 4 5 1 2))
  (equal?
    (let ()
      (define f1
        (lambda (x) (apply values (vector->list x))))
      (define f2
        (lambda (a b)
          (let*-values ([(d) (f1 a)]
                       [(e . f) (f1 b)]
                       [(g h i) (f1 b)]
                       [j (f1 b)])
            (list d e f g h i j))))
      (f2 '#(a) '#(b c d)))
    '(a b (c d) b c d (b c d)))
  (eqv?
    (letrec ((z 2)
             (f (lambda () (values 1 z)))
             (g (lambda (x y) (values x y z))))
      (let*-values ([(c d e) (let*-values ([(z b) (f)]) (g z b))])
        (+ c d e z)))
    7)
  (equal?
    (let ([a 3])
      (let*-values ([(a b) (values (+ a 1) (+ a 2))]
                    [(c) (values (+ a 3))])
        (list a b c)))
    '(4 5 7))
 ; make sure pattern variables and ellipses on RHS don't screw us up
  (eqv?
    (let ()
      (define-syntax q
        (lambda (x)
          (syntax-case x ()
            [(_ dots) (free-identifier=? #'dots #'(... ...)) 3])))
      (let*-values ([(a) (q ...)]) a))
    3)
  (equal?
    (syntax-case '(a b c) ()
      [(x ...) (let*-values ([(args) #'(x ...)]) args)])
    '(a b c))
)
