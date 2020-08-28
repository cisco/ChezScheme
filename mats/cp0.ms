;;; cp0.ms
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

(define-syntax cp0-mat
  (syntax-rules ()
    [(_ name form ...)
     (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (mat name form ...))]))

(cp0-mat cp0-regression
 ; test to keep cp0 honest about letrec's implicit assignment
  #;(letrec ((x (call/cc (lambda (k) k)))) ; invalid in r6rs
    (let ((y x))
      (y (lambda (z) (not (eq? x y))))))
 ; make sure compiler doesn't loop...
  (begin
    (define omega
      (lambda ()
        ((lambda (x) (x x)) (lambda (x) (x x)))))
    (procedure? omega))
 ; make sure cp0 doesn't assume read returns #t
  (not (read (open-input-string "#f")))
 ; test proper visiting of assigned variables
  (letrec ((x (lambda () x)) (y (lambda () x)))
    (set! y (y))
    (eq? y (y)))
 ; test proper quote propagation from seq w/side effect
  (equal?
    (let ((x 0))
      (let ((y (begin (set! x (+ x 1)) 0)))
        (let ((z (+ y 1)))
          (list x z))))
    '(1 1))
 ; test that we reset integrated? flags for outer calls when we bug out of
 ; an inner call in cases where operator of call is itself a call
  (begin
    (define whack! (lambda () (set! whack! 'okay)))
    (define ignore list)
    (letrec ([g
              (lambda x
                  ((lambda (x)
                     (ignore)
                     (when (null? x) (g #f))
                     (lambda (y) (ignore x y y y)))
                   (ignore (ignore ignore))))])
      ((g) (whack!)))
    (eq? whack! 'okay))
 ; make sure cp0 does not go to lala land
  (error? (letrec ((x x)) x))
 ; make sure residual assignments to unref'd vars don's blow
  (eq? (let ((x (void)))
         (set! x 0)
         (letrec ((f (lambda () (set! x (+ x 1)) x)) (g (lambda (x) x)))
           (g 3)))
       3)
  (eq? (let ()
         (define kons-proc
           (lambda (a) (lambda (b) (lambda (g) ((g a) b)))))
         (define-syntax kons
           (syntax-rules () [(_ x y) ((kons-proc x) y)]))
         (define kar (lambda (pr) (pr (lambda (a) (lambda (b) a)))))
         (define kdr (lambda (pr) (pr (lambda (a) (lambda (b) b)))))
         ((kar (kons (lambda (x y) (kar (kons x y)))
                 (kons (lambda (x y) (kdr (kons x y)))
                       (lambda (x y) (kdr (kar (kons (kons x y) 'nil)))))))
          3 4))
       3)
 ; test for various bugs fixed in 5.9i, all relating to resetting an
 ; outer context when we abort from an inner one
  (begin
    (define **a 1)
    (define-syntax **huge
      (identifier-syntax
        (set! **output
          (cons
            (list (list **a **a **a **a **a **a **a **a **a **a)
                  (list **a **a **a **a **a **a **a **a **a **a)
                  (list **a **a **a **a **a **a **a **a **a **a)
                  (list **a **a **a **a **a **a **a **a **a **a)
                  (list **a **a **a **a **a **a **a **a **a **a))
            **output))))
    (define **test-output
      (case-lambda
        [(th) (**test-output 1 th)]
        [(n th)
         (set! **output '())
         (and (th)
           (equal? **output
             (make-list n
               '((1 1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1 1)))))]))
    (**test-output (lambda () **huge #t)))
  (**test-output
    (lambda ()
      (equal?
        (let ((f (lambda ()
                   (let ((x **huge))
                     (let ((g (lambda () x)))
                       (g) memq)))))
          ((f) (+ 1 2) '(1 2 3 4 5)))
        '(3 4 5))))
  (**test-output
    (lambda ()
      (equal?
        (let ((f (lambda ()
                   (let ((x **huge))
                     (let ((g (begin 0 (lambda () x)))) (g) memq)))))
          ((f) (+ 1 2) '(1 2 3 4 5)))
        '(3 4 5))))
  (**test-output
    (lambda ()
      (equal?
        (let ((f (lambda ()
                   (let ((x **huge))
                     (let ((g (lambda () x))) (g) (g) memq)))))
          ((f) (+ 1 2) '(1 2 3 4 5)))
        '(3 4 5))))
  (**test-output
    (lambda ()
      (eq?
        (let ((f (lambda () (let ((x **huge)) (lambda (y z) (or (= y 3) x))))))
          ((f) (+ 1 2) 4))
        #t)))
  (**test-output 2
    (lambda ()
      (eq? (let ((f (lambda ()
                      (let ((x **huge)) (lambda (y z) (or (= y 3) x))))))
            ((f) (+ 1 2) 4)
            ((f) (+ 1 2) 4))
        #t)))
  (**test-output 2
    (lambda ()
      (eq?
        (let ((f (lambda ()
                   (let ((x **huge)) (lambda (y z) (if (y z) 'ok x))))))
          ((f) + 3)
          ((f) + 3))
        'ok)))
  (eq?
    (let ((f (lambda () (let ((x 0)) (lambda (y z) (if (y z) 'ok x))))))
      ((f) + 3))
    'ok)
  (not (let ((f (lambda (x)
             (eq? (begin (set! x 4) x)
                  (begin (set! x 5) x)))))
         (f 'a)))
  (not (let ((f #f) (g #f))
         (let ((x 0))
           (set! g (lambda () (eq? (begin (f) x) (begin (f) x))))
           (set! f (lambda () (set! x (+ x 1))))
           (g))))
  (eq? (let ([g% (lambda (cp)
                   (let ([t1 0])
                     (set! t1 (car cp))
                     (let ([t2 t1]) 4)))])
         g%
         (g% '(0)))
       4)
  (error? (let ((f (lambda (x) x))) (let ((g f)) (g))))
  (begin
    (define $foo$
      (letrec ((func1
                (lambda (cont0)
                  (cont0 'x)))) ; incorrect # args to cont0 (func3)
         (lambda ()
           (letrec ((func3
                     (lambda (cont2 x)
                       (cont2 x))))
              (lambda ()
                (func1 func3))))))
    #t)
  (error? (($foo$)))
  (begin
    (define $foo$
      (letrec ((func1
                (lambda (cont0)
                  (cont0 list 'x)))) ; correct # args to cont0 (func3)
         (lambda ()
           (letrec ((func3
                     (lambda (cont2 x)
                       (cont2 x))))
              (lambda ()
                (func1 func3))))))
    #t)
  (equal? (($foo$)) '(x))
 ; make sure cpletrec doesn't toss bindings for assigned variables
  (equal?
    (let ()
      (define *root* '())
      (define (init-traverse) (set! *root* 0))
      (define (run-traverse) (traverse *root*))
      (init-traverse))
    (void))
 ; make sure nested cp0 doesn't assimilate letrec bindings when
 ; body is simple but not pure
  ((lambda (x ls) (and (member x ls) #t))
    (let ([x 0])
      (letrec ([a (letrec ([b (set! x 1)]) x)]
               [c (letrec ([d (set! x 2)]) x)])
        (list a c)))
    '((1 2) (2 1)))
  ((lambda (x ls) (and (member x ls) #t))
    (let ([x 0])
      (letrec ([a (letrec ([b x]) (set! x 1) b)]
               [c (letrec ([d x]) (set! x 2) d)])
        (list a c x)))
    '((2 0 1) (0 1 2)))
 ; make sure (r6rs:fx+ x 0) isn't folded to (r6rs:fx+ x), since
 ; r6rs:fx+ doesn't accept just one argument.
  (begin
    (define $cp0-f (let ([z 0]) (lambda (x) (r6rs:fx+ x z))))
    (define $cp0-g (let ([z 0]) (lambda (x) (r6rs:fx* x 1))))
    #t)
  (eqv? ($cp0-f 17) 17)
  (eqv? ($cp0-g 17) 17)
  (error? ($cp0-f 'a))
  (error? ($cp0-g 'a))
 ; make sure cp0 isn't overeager about moving discardable but
 ; not pure primitive calls
  (and
    (member
      (let ([p (cons 1 2)])
        (list
          (let ([x (car p)]) (set-car! p 3) x)
          (let ([x (car p)]) (set-car! p 4) x)))
      '((4 1) (1 3)))
    #t)
  ; make sure cp0 doesn't screw up on an "almost" or pattern
  (error? ; #f is not a number
    (if (let ([x (eqv? (random 2) 2)]) (if x x (+ x 1))) 4 5))
   (begin
     (define f
       (lambda (x)
         (letrec ([foo (lambda (ls)
                         (let loop ([ls ls] [rls '()])
                           (if (null? ls)
                               rls
                               (loop (cdr ls) (cons (car ls) rls)))))])
           (apply foo (list x)))))
     #t)
   (equal?
     (f (list 1 2))
     '(2 1))
   (begin
     (define f
       (lambda (x)
         (letrec ([foo (lambda (x ls)
                         (let loop ([ls ls] [rls '()])
                           (if (null? ls)
                               (cons x rls)
                               (loop (cdr ls) (cons (car ls) rls)))))])
           (apply (begin (write 'a) foo) (begin (write 'b) 'bar) (begin (write 'c) (list x))))))
     #t)
   (equal?
     (f (list 1 2))
     '(bar 2 1))
   ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string (lambda () (f (list 1 2))))
    '("abc" "acb" "bac" "bca" "cab" "cba"))
   (begin
     (define $x 17)
     #t)
   ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (apply
          (begin (write 'a) member)
          (begin (write 'b) $x)
          (begin (write 'c) (list (begin (write 'd) '()))))))
    '("abcd" "acdb" "bacd" "bcda" "cdab" "cdba"))
   ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (apply
          (begin (write 'a) ash)
          (begin (write 'b) $x)
          (begin (write 'c) (list (begin (write 'd) 0))))))
    '("abcd" "acdb" "bacd" "bcda" "cdab" "cdba"))
   ; check to see if this turns up a missing referenced flag due to an extra
   ; binding for p.  (missing referenced flags are presently detected only when
   ; cpletrec is compiled with d=k, k > 0.)
   (equal?
     (apply (let ([p (box 0)]) (lambda () p)) '())
     '#&0)
   ; check for some corrected flags
   (not (and (record-type-parent #!base-rtd) #t))
   (error? ; invalid report specifier
     (begin
       (null-environment #f)
       #t))
   (error? ; not a source object
     (begin
       (source-object-bfp #f)
       #t))
   (error? ; not a source object
     (begin
       (source-object-efp #f)
       #t))
   (error? ; not a source object
     (begin
       (source-object-sfd #f)
       #t))
   (error? ; not a condition
     (begin
       (condition #f)
       #t))
   ; nested if optimization
   (begin
     (define $cp0-f
       (lambda (x y a b c)
         (if (if (if (if (#3%zero? (a)) #f #t) (begin (b) #t) #f)
                 (c)
                 #f)
             (x)
             (y))))
     #t)
   (equal?
     (with-output-to-string
       (lambda ()
         ($cp0-f
           (lambda () (printf "x\n"))
           (lambda () (printf "y\n"))
           (lambda () (printf "a\n") 0)
           (lambda () (printf "b\n"))
           (lambda () (printf "c\n") #t))))
     "a\ny\n")
   (equivalent-expansion?
     (expand/optimize
       '(lambda (x y a b c)
          (if (if (if (if (#3%zero? (a)) #f #t) (begin (b) #t) #f)
                  (c)
                  #f)
              (x)
              (y))))
     '(lambda (x y a b c)
        (if (if (#3%zero? (a))
                #f
                (begin (b) (c)))
            (x)
            (y))))
   (equivalent-expansion?
     (expand/optimize
       '(lambda (x y a b c)
          (if (if (if (not (#3%zero? (a))) (begin (b) #t) #f)
                  (c)
                  #f)
              (x)
              (y))))
     '(lambda (x y a b c)
        (if (if (#3%zero? (a))
                #f
                (begin (b) (c)))
            (x)
            (y))))
   (error? (apply zero? 0))
   (error? (if (apply eof-object 1 2) 3 4))
  ; test for folding of multiple-value primitives
   (equivalent-expansion?
     (expand/optimize '(lambda () (div-and-mod 7 3)))
     '(lambda () (#3%values 2 1)))
   (equivalent-expansion?
     (expand/optimize '(lambda () (exact-integer-sqrt 19)))
     '(lambda () (#3%values 4 3)))
   (equivalent-expansion?
     (expand/optimize
       '(call-with-values
          (lambda () (div-and-mod 7 3))
          (lambda (x y) (#2%cons (* x 10) (/ y 10)))))
     '(#2%cons 20 1/10))
)

(cp0-mat cp0-mrvs
  (eqv? (call-with-values (lambda () (values 1 2 3)) +) 6)
  (begin
    (define **cwv-test
      (lambda (out p)
        (define x '())
        (define pp (lambda (a) (set! x (cons a x))))
        (and (p pp)
             (if (procedure? out)
                 (out (reverse x))
                 (equal? (reverse x) out)))))
    (**cwv-test '(1 2 2 3)
      (lambda (pretty-print)
        (pretty-print 1)
        (pretty-print 2)
        (pretty-print 2)
        (pretty-print 3)
        #t)))
  (**cwv-test '(1 1 2 3)
    (lambda (pretty-print)
      (equal?
        (call-with-values
          (begin
            (pretty-print 1)
            (lambda () (pretty-print 2) (+ 1 2 3)))
          (begin
            (pretty-print 1)
            (lambda (n) (pretty-print 3) (list n n n))))
        '(6 6 6))))
  (**cwv-test '(1 1 2 3)
    (lambda (pretty-print)
      (eqv?
        (call-with-values
          (begin
            (pretty-print '1)
            (lambda () (pretty-print '2) (values 1 2 3)))
          (begin
            (pretty-print '1)
            (lambda (a b c) (pretty-print '3) (+ c b a))))
        6)))
  (**cwv-test '(1 1 2 3 4)
    (lambda (pretty-print)
      (eqv?
        (call-with-values
          (begin
            (pretty-print '1)
            (lambda ()
              (pretty-print '2)
              (values 1 (begin (pretty-print '3) 2) 3)))
          (begin
            (pretty-print '1)
            (lambda (a b c) (pretty-print '4) (+ c b a))))
        6)))
  (begin
    (define **foo (lambda () (values 'a 'b 'c)))
    (define **bar vector)
    (equal? (call-with-values **foo **bar) '#(a b c)))
  (equal?
    (call-with-values
      (lambda () (values 1 2 3))
      (case-lambda [(x) (list x x x)] [(a b c) (list c b a)]))
    '(3 2 1))
  (equal? (call-with-values (lambda () (values 1 2 3)) **bar) '#(1 2 3))
  (**cwv-test '(1 2)
    (lambda (pretty-print)
      (equal?
        (call-with-values
          (lambda () (pretty-print '2) (values 1 2 3))
          (begin (pretty-print '1) **bar))
        '#(1 2 3))))
  (**cwv-test '(1 1 2)
    (lambda (pretty-print)
      (equal?
        (call-with-values
          (begin
            (pretty-print '1)
            (lambda () (pretty-print '2) (values 1 2 3)))
          (begin (pretty-print '1) **bar))
        '#(1 2 3))))
  (equal? (call-with-values **foo (lambda (a b c) (list c b a))) '(c b a))
  (equal? (let ((f (lambda (a b c) (list c b a))))
            (call-with-values **foo f))
    '(c b a))
  (**cwv-test '(1)
    (lambda (pretty-print)
      (equal? (call-with-values
                **foo
                (begin
                  (pretty-print '1)
                  (lambda (a b c) (vector c b a))))
        '#(c b a))))
  (**cwv-test (lambda (x) (or (equal? x '(1 2 3)) (equal? x '(2 3 4))))
    (lambda (pretty-print)
      (define n 1)
      (define boof
        (lambda ()
          (pretty-print 3)
          (lambda (a b c) (list c b a))))
      (equal?
        (call-with-values
          (begin (pretty-print n) **foo)
          (begin (set! n 4) (pretty-print 2) (boof)))
        '(c b a))))
  (**cwv-test '(1 2 3)
    (lambda (pretty-print)
      (define n 1)
      (define boof
        (lambda ()
          (pretty-print 3)
          (lambda (a b c) (list c b a))))
      (equal?
        (let* ((prod (begin (pretty-print n) **foo))
               (csmr (begin (set! n 4) (pretty-print 2) (boof))))
          (call-with-values prod csmr))
        '(c b a))))
  (**cwv-test '(2 3 4)
    (lambda (pretty-print)
      (define n 1)
      (define boof
        (lambda ()
          (pretty-print 3)
          (lambda (a b c) (list c b a))))
      (equal?
        (let* ((csmr (begin (set! n 4) (pretty-print 2) (boof)))
               (prod (begin (pretty-print n) **foo)))
          (call-with-values prod csmr))
        '(c b a))))
  (**cwv-test '(1 1)
    (lambda (pretty-print)
      (equal?
        (call-with-values
          (begin
            (pretty-print '1)
            **foo)
          (begin
            (pretty-print '1)
            (lambda (a b c) (list c b a))))
        '(c b a))))
  (begin
    (set! **a #t)
    (equal?
      (call-with-values
        (lambda () (if **a (values 1) (values 1 2 3)))
        (case-lambda [(x) (list x x x)] [(a b c) (list c b a)]))
      '(1 1 1)))
  (begin
    (set! **a #f)
    (equal?
      (call-with-values
        (lambda () (if **a (values 1) (values 1 2 3)))
        (case-lambda [(x) (list x x x)] [(a b c) (list c b a)]))
      '(3 2 1)))
  (begin
    (set! **a #t)
    (equal?
      (let ((f (lambda (a) (if **a (values 1) (values 1 2 3)))))
        (call-with-values
          (lambda () (f #t))
          (case-lambda [(x) (list x x x)] [(a b c) (list c b a)])))
      '(1 1 1)))
  (begin
    (set! **a #f)
    (equal?
      (let ((f (lambda (a) (if **a (values 1) (values 1 2 3)))))
        (call-with-values
          (lambda () (f #t))
          (case-lambda [(x) (list x x x)] [(a b c) (list c b a)])))
      '(3 2 1)))
  (equal?
    (call-with-values
      (lambda ()
        (define foo
          (lambda (x)
            (if (zero? x)
                (values 1 2 3)
                (call-with-values
                  (lambda () (foo (- x 1)))
                  (lambda (a b c)
                    (values (+ a 1) (+ b a) (+ c 2)))))))
        (call-with-values
          (lambda () (foo 0))
          (lambda (a b c)
            (foo (+ a b c)))))
      list)
    '(7 23 15))
  (equal?
    (let ((f (lambda ()
               (let loop ((n 10))
                 (if (zero? n)
                     call-with-values
                     (loop (fx- n 1)))))))
      ((f) (lambda () (values 1 2)) cons))
    '(1 . 2))
  (equal?
    (let ()
      (define (go n)
        (let ((f (lambda ()
                   (let loop ((n n))
                     (if (zero? n)
                         call-with-values
                         (loop (fx- n 1)))))))
          ((f) (lambda () (values 1 2)) cons)))
      (go 1000))
    '(1 . 2))
  (begin
    (define **bozo
      (lambda (pretty-print)
        (pretty-print '3)
        (lambda x
          (pretty-print 6)
          x)))
    (define **clown (lambda () (values 1 2 3)))
    (**cwv-test '(3 6)
      (lambda (pretty-print)
        (equal?
          (call-with-values **clown (**bozo pretty-print))
          '(1 2 3)))))
  (**cwv-test '(1 2)
    (lambda (pretty-print)
      (equal?
        (let ((f (lambda () (pretty-print '2) (values 1 2 3))))
          (call-with-values
            (begin (pretty-print '1) f)
            (lambda x x)))
        '(1 2 3))))
  (**cwv-test '(1 2)
    (lambda (pretty-print)
      (equal?
        (let ((f (lambda () (pretty-print '2) (**foo))))
          (call-with-values
            (begin (pretty-print '1) f)
            (lambda x x)))
        '(a b c))))
  (**cwv-test '(1 2 3 4)
    (lambda (pretty-print)
      (equal?
        (let ([f
               (lambda ()
                 (pretty-print '2)
                 (lambda () (pretty-print '3) (**foo)))])
          (call-with-values
            (begin (pretty-print '1) (f))
            (lambda x (pretty-print 4) x)))
        '(a b c))))
  (**cwv-test '(1)
    (lambda (pretty-print)
      (equal?
        (call-with-values
          (begin (pretty-print '1) (lambda () (**foo)))
          (lambda (x y z) (list y z x)))
        '(b c a))))
  (procedure?
    (lambda ()
      (define test1 (lambda () void))
      (define test2
        (lambda ()
          (call-with-values test1 (lambda (tester) (tester)))))
      (test2)))
  (eqv?
    (let ()
      (define test1 (lambda (x) (values (lambda () (+ x 1)))))
      (define test2
        (lambda (x)
          (let-values ([(tester) (test1 x)])
            (tester))))
      (test2 10))
    11)
)

(cp0-mat apply-partial-folding
  (test-cp0-expansion
    '(apply fx+ '(1 2 3 4 5))
    15)
  (test-cp0-expansion
    '(apply fx+ 3 x 4 '(5 7 9))
    (if (eqv? (optimize-level) 3)
        '(#3%fx+ 28 x)
        '(#2%fx+ 28 x)))
  (test-cp0-expansion
    '(apply fx+ 3 x 4 (begin (write 'hi) '(5 7 9)))
    (if (eqv? (optimize-level) 3)
        '(let ([g x]) (#3%write 'hi) (#3%fx+ 28 g))
        '(let ([g x]) (#2%write 'hi) (#2%fx+ 28 g))))
  (test-cp0-expansion
    '(apply fx+ 3 x 4 '(5 7 9.0))
    (if (eqv? (optimize-level) 3)
        '(#3%fx+ 19 x 9.0)
        '(#2%fx+ 19 x 9.0)))
  (test-cp0-expansion
    `(apply apply '(,list 2 3 (4 5 6)))
    `(',list 2 3 4 5 6))
  (test-cp0-expansion
    `(#3%apply #3%apply #3%+ '(1 (2 3 4)))
    10)
  (test-cp0-expansion
    `(apply apply apply + 1 '(2 3 (4 5 (6 7))))
    28)
  (test-cp0-expansion
    `(let ([f apply]) (f f f * 1 '(2 3 (4 5 (6)))))
    720)
  (test-cp0-expansion
   `(lambda (x) (apply (lambda (prim ls) (apply prim ls)) zero? (list x)))
   (if (eqv? (optimize-level) 3)
       '(lambda (x) (#3%apply #3%zero? x))
       '(lambda (x) (#2%apply #2%zero? x))))
  (test-cp0-expansion
   `(apply (lambda (prim ls) (apply prim ls)) zero? (list (cons 0 '())))
   #t)
  (test-cp0-expansion
   `(apply (lambda (prim ls) (apply prim ls)) zero? (cons 0 '()))
   (if (eqv? (optimize-level) 3)
       '(#3%apply #3%zero? 0)
       '(#2%apply #2%zero? 0)))
)

(mat expand/optimize
  (error? (expand/optimize))
  (error? (expand/optimize 'a 'b))
  (error? (expand/optimize 'a 'b 'c))
  (eqv? (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))])
          (expand/optimize 3))
        3)
  (equal? (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))])
            (expand/optimize '(#2%cdr '(3 4))))
          ''(4))
  (eqv? (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
          (expand/optimize ; from cp0 talk
            '(let ([n (expt 2 10)])
               (define even?
                 (lambda (x) (or (zero? x) (not (odd? x)))))
               (define odd?
                 (lambda (x) (not (even? (- x 1)))))
               (define f
                 (lambda (x)
                   (lambda (y)
                     (lambda (z)
                       (if (= z 0) (omega) (+ x y z))))))
               (define omega
                 (lambda ()
                   ((lambda (x) (x x)) (lambda (x) (x x)))))
               (let ([g (f 1)] [m (f n)])
                 (let ([h
                        (if (> ((g 2) 3) 5)
                            (lambda (x) (+ x 1))
                            odd?)])
                   (h n))))))
        1025)
  (let ([x (parameterize ([#%$suppress-primitive-inlining #f]
                          [run-cp0 (lambda (cp0 x) (cp0 x))]
                          [optimize-level 3])
             (expand/optimize ; from mwbor talk
               '(let ()
                  (import scheme)
                  (define opcode-pos 27)
                  (define src1-pos 22)
                  (define src2-pos 0)
                  (define dst-pos 17)
                  (define imm-bit (ash 1 16))
                  (define regops '((ld . 22) (add . 28)))
                  (define immops '((addi . 28)))
                  (define regcodes
                    '((r0 . 0) (r1 . 1) (r2 . 2) (r3 . 3)))
                  (define-syntax reg
                    (syntax-rules ()
                      [(_ r) (cdr (assq 'r regcodes))]))
                  (define imm
                    (lambda (n)
                      (unless (< -32768 n 32767)
                        (errorf 'imm "invalid immediate ~s" n))
                      n))
                  (define $emit!
                    (lambda (op a1 a2 a3)
                      (emit-word!
                        (+ (cond
                             [(assq op regops) =>
                              (lambda (a)
                                (ash (cdr a) opcode-pos))]
                             [(assq op immops) =>
                              (lambda (a)
                                (+ (ash (cdr a) opcode-pos)
                                   imm-bit))]
                             [else
                              (errorf 'emit
                                "unrecognized operator ~s"
                                op)])
                           (ash a1 src1-pos)
                           (ash a2 src2-pos)
                           (ash a3 dst-pos)))))
                  (define-syntax emit
                    (syntax-rules ()
                      [(_ op a1 a2 a3) ($emit! 'op a1 a2 a3)]))
                  (set! test
                    (lambda (r)
                      (emit ld (reg r0) (reg r1) (reg r2))
                      (emit addi (reg r2) 320 (reg r2))
                      (emit add (reg r2) r (reg r2)))))))])
    (and
      (equivalent-expansion? x
        '(set! test
           (lambda (r)
             (emit-word! 2953052161)
             (emit-word! 3766812992)
             (emit-word! (#3%+ 3766747136 r)))))
      (syntax-case x ()
        [(set! test
           (lambda (r1)
             (ew1! 2953052161)
             (ew2! 3766812992)
             (ew3! (#3%+ 3766747136 r2))))
         (eq? #'r1 #'r2)])))
  (let ([x (parameterize ([#%$suppress-primitive-inlining #f]
                          [run-cp0 (lambda (cp0 x) (cp0 x))]
                          [optimize-level 2])
             (expand/optimize ; from mwbor talk
               '(let ()
                  (import scheme)
                  (define opcode-pos 27)
                  (define src1-pos 22)
                  (define src2-pos 0)
                  (define dst-pos 17)
                  (define imm-bit (ash 1 16))
                  (define regops '((ld . 22) (add . 28)))
                  (define immops '((addi . 28)))
                  (define regcodes
                    '((r0 . 0) (r1 . 1) (r2 . 2) (r3 . 3)))
                  (define-syntax reg
                    (syntax-rules ()
                      [(_ r) (cdr (assq 'r regcodes))]))
                  (define imm
                    (lambda (n)
                      (unless (< -32768 n 32767)
                        (errorf 'imm "invalid immediate ~s" n))
                      n))
                  (define $emit!
                    (lambda (op a1 a2 a3)
                      (emit-word!
                        (+ (cond
                             [(assq op regops) =>
                              (lambda (a)
                                (ash (cdr a) opcode-pos))]
                             [(assq op immops) =>
                              (lambda (a)
                                (+ (ash (cdr a) opcode-pos)
                                   imm-bit))]
                             [else
                              (errorf 'emit
                                "unrecognized operator ~s"
                                op)])
                           (ash a1 src1-pos)
                           (ash a2 src2-pos)
                           (ash a3 dst-pos)))))
                  (define-syntax emit
                    (syntax-rules ()
                      [(_ op a1 a2 a3) ($emit! 'op a1 a2 a3)]))
                  (set! test
                    (lambda (r)
                      (emit ld (reg r0) (reg r1) (reg r2))
                      (emit addi (reg r2) 320 (reg r2))
                      (emit add (reg r2) r (reg r2)))))))])
    (and
      (equivalent-expansion? x
        '(set! test
           (lambda (r)
             (emit-word! 2953052161)
             (emit-word! 3766812992)
             (emit-word! (#2%+ 3766747136 (#2%ash r 0))))))
      (syntax-case x ($primitive)
        [(set! test
           (lambda (r1)
             (ew1! 2953052161)
             (ew2! 3766812992)
             (ew3! (#2%+ 3766747136 (#2%ash r2 0)))))
         (eq? #'r1 #'r2)])))
 ; verify optimization of (if e s s) => (begin e s)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2] [compile-profile #f])
      (expand/optimize
        '(lambda (x) (if e x x))))
    '(lambda (x) e x))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2] [compile-profile #f])
      (expand/optimize
        '(lambda (y x) (if y x x))))
    '(lambda (y x) x))
 ; verify optimization of (if s s #f) => s
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2] [compile-profile #f])
      (expand/optimize
        '(lambda (x) (if x x #f))))
    '(lambda (x) x))
 ; verify optimization of (if s s #f) => s
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2] [compile-profile #f])
      (expand/optimize
        '(let ()
           (define-syntax broken-or
             (syntax-rules ()
               [(_) #f]
               [(_ x y ...)
                (let ([t x])
                  (if t t (broken-or y ...)))]))
           (broken-or a))))
    'a)
 ; verify optimization of or pattern
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2] [compile-profile #f])
      (expand/optimize
        '(lambda (x y) (if (not (or (fx< x y) (fx> y x))) x y))))
    '(lambda (x.0 y.1)
       (if (if (#2%fx< x.0 y.1) #t (#2%fx> y.1 x.0))
           y.1
           x.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2] [compile-profile #f])
      (expand/optimize
        '(lambda (x y) (if (or (fx< x y) (fx> y x)) x y))))
    '(lambda (x y) (if (if (#2%fx< x y) #t (#2%fx> y x)) x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(let ([q #f])
           (lambda (x y) (if (or q (fx> x y)) x y)))))
    '(lambda (x y) (if (#2%fx> x y) x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(let ([q #t])
           (lambda (x y) (if (or q (fx> x y)) x y)))))
    '(lambda (x y) x))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(begin 3 4)))
    4)
  ; verify expansion of not pattern
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        `(not #t)))
    #f)
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        `(not #f)))
    #t)
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        `(not '(a b c))))
    #f)
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        `(let ([x 2] [y 3])
           (not (begin (set! x (* x y)) (set! y (* x y)) 10)))))
    `(let ([x 2] [y 3])
       (set! x (#2%* x y))
       (set! y (#2%* x y))
       #f))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        `(not (let ([x 2] [y 3]) (set! x (* x y)) (set! y (* x y)) 10))))
    `(let ([x 2])
       (let ([y 3])
         (set! x (#2%* x y))
         (set! y (#2%* x y))
         #f)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        `(if (not (or #t (futz))) 17 32)))
    32)
)

(mat expand-output
  (error? ; not a textual output port or #f
    (expand-output #t))
  (error?  ; not a textual output port or #f
    (let-values ([(bop get) (open-bytevector-output-port)])
      (expand-output bop)))
  (begin
    (define $eospam 17)
    #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (parameterize ([expand-output (current-output-port)] [#%$suppress-primitive-inlining #f])
          (pretty-print (compile '(let () (import (chezscheme)) (+ 3 4 $eospam)))))))
    (if (eqv? (optimize-level) 3)
        "(#3%+ 3 4 $eospam)\n24\n"
        "(#2%+ 3 4 $eospam)\n24\n"))
  (equal?
    (with-output-to-string
      (lambda ()
        (parameterize ([expand-output (current-output-port)] [#%$suppress-primitive-inlining #f])
          (pretty-print (interpret '(let () (import (chezscheme)) (+ 3 4 $eospam)))))))
    (if (eqv? (optimize-level) 3)
        "(#3%+ 3 4 $eospam)\n24\n"
        "(#2%+ 3 4 $eospam)\n24\n"))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print '(eval-when (visit revisit) (define $eo-q (* 2 2))))
        (pretty-print '(define $eo-x 3))
        (pretty-print '(define-syntax $eo-a (identifier-syntax 5)))
        (pretty-print '(pretty-print (vector $eo-x $eo-q (+ $eo-a 1)))))
      'replace)
    #t)
  (begin
    (define $eo-sop
      (let ()
        (define syntax-record-writer
          (case-lambda
            [() (record-writer (record-rtd #'a))]
            [(x) (record-writer (record-rtd #'a) x)]))
        (open-input-string
          (with-output-to-string
            (lambda ()
              (parameterize ([expand-output (current-output-port)]
                             [print-gensym #t]
                             [optimize-level 2]
                             [compile-file-message #f]
                             [enable-cp0 #t]
                             [#%$suppress-primitive-inlining #f]
                             [syntax-record-writer (lambda (x p wr) (display "syntax-object" p))])
                (compile-file "testfile")))))))
    #t)
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (begin
         (set! $eo-q (#2%* 2 2))
         (#3%$sc-put-cte
           'syntax-object
           '(global . ,gensym?)
           '*top*))))
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (eval-when (revisit)
         (set! $eo-x 3))
       (eval-when (visit)
         (#3%$sc-put-cte
           'syntax-object
           '(global . ,gensym?)
           '*top*))))
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (eval-when (visit)
         (#3%$sc-put-cte
           'syntax-object
           ,list?
           '*top*))))
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (eval-when (revisit)
         (#2%pretty-print (#2%vector $eo-x $eo-q (#2%+ 5 1))))))
  (begin (set! $eo-sop #f) #t)
)

(mat expand/optimize-output
  (error? ; not a textual output port or #f
    (expand/optimize-output #t))
  (error?  ; not a textual output port or #f
    (let-values ([(bop get) (open-bytevector-output-port)])
      (expand/optimize-output bop)))
  (equal?
    (with-output-to-string
      (lambda ()
        (parameterize ([expand/optimize-output (current-output-port)]
                       [enable-cp0 #t]
                       [#%$suppress-primitive-inlining #f])
          (pretty-print (compile '(let () (import (chezscheme)) (+ 3 4 $eospam)))))))
    (if (eqv? (optimize-level) 3)
        "(#3%+ 7 $eospam)\n24\n"
        "(#2%+ 7 $eospam)\n24\n"))
  (equal?
    (with-output-to-string
      (lambda ()
        (parameterize ([expand/optimize-output (current-output-port)]
                       [enable-cp0 #t]
                       [#%$suppress-primitive-inlining #f])
          (pretty-print (interpret '(let () (import (chezscheme)) (+ 3 4 $eospam)))))))
    (if (eqv? (optimize-level) 3)
        "(#3%+ 7 $eospam)\n24\n"
        "(#2%+ 7 $eospam)\n24\n"))
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print '(eval-when (visit revisit) (define $eo-q (* 2 2))))
        (pretty-print '(define $eo-x 3))
        (pretty-print '(define-syntax $eo-a (identifier-syntax 5)))
        (pretty-print '(pretty-print (vector $eo-x $eo-q (+ $eo-a 1)))))
      'replace)
    #t)
  (begin
    (define $eo-sop
      (let ()
        (define syntax-record-writer
          (case-lambda
            [() (record-writer (record-rtd #'a))]
            [(x) (record-writer (record-rtd #'a) x)]))
        (open-input-string
          (with-output-to-string
            (lambda ()
              (parameterize ([expand/optimize-output (current-output-port)]
                             [print-gensym #t]
                             [optimize-level 2]
                             [compile-file-message #f]
                             [enable-cp0 #t]
                             [#%$suppress-primitive-inlining #f]
                             [syntax-record-writer (lambda (x p wr) (display "syntax-object" p))])
                (compile-file "testfile")))))))
    #t)
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (begin
         (set! $eo-q 4)
         (#3%$sc-put-cte
           'syntax-object
           '(global . ,gensym?)
           '*top*))))
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (eval-when (revisit)
         (set! $eo-x 3))
       (eval-when (visit)
         (#3%$sc-put-cte
           'syntax-object
           '(global . ,gensym?)
           '*top*))))
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (eval-when (visit)
         (#3%$sc-put-cte
           'syntax-object
           ,list?
           '*top*))))
  (equivalent-expansion?
    (read $eo-sop)
    `(begin
       (recompile-requirements () ())
       (eval-when (revisit)
         (#2%pretty-print (#2%vector $eo-x $eo-q 6)))))
  (begin (set! $eo-sop #f) #t)
)

(mat cp0-partial-folding
 ; check partial folding of +, fx+, fl+, and cfl+
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] 
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (+) (+ 3) (+ 3 4) (+ x) (+ x 0) (+ 0 x) (+ x 3)
           (+ x 3 4) (+ 3 x 4) (+ 3 x -3) (+ 3 x 4 y 5)
           (+ +nan.0 x 4 y 5))))
    '(#2%list 0 3 7 (#2%+ x) (#2%+ x) (#2%+ x) (#2%+ 3 x)
       (#2%+ 7 x) (#2%+ 7 x) (#2%+ x) (#2%+ 12 x y)
       (begin (#2%+ x y) +nan.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (+) (+ 3) (+ 3 4) (+ x) (+ x 0) (+ 0 x) (+ x 3)
           (+ x 3 4) (+ 3 x 4) (+ 3 x -3) (+ 3 x 4 y 5)
           (+ +nan.0 x 4 y 5))))
    '(#3%list 0 3 7 x x x (#3%+ 3 x)
       (#3%+ 7 x) (#3%+ 7 x) x (#3%+ 12 x y)
       +nan.0))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fx+) (fx+ 3) (fx+ 3 4) (fx+ x) (fx+ x 0) (fx+ 0 x) (fx+ x 3)
           (fx+ x 3 4) (fx+ 3 x 4) (fx+ 3 x -3) (fx+ 3 x 4 y 5))))
    '(#2%list 0 3 7 (#2%fx+ x) (#2%fx+ x) (#2%fx+ x) (#2%fx+ 3 x)
       (#2%fx+ 7 x) (#2%fx+ 7 x) (#2%fx+ x) (#2%fx+ 12 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fx+) (fx+ 3) (fx+ 3 4) (fx+ x) (fx+ x 0) (fx+ 0 x) (fx+ x 3)
           (fx+ x 3 4) (fx+ 3 x 4) (fx+ 3 x -3) (fx+ 3 x 4 y 5))))
    '(#3%list 0 3 7 x x x (#3%fx+ 3 x)
       (#3%fx+ 7 x) (#3%fx+ 7 x) x (#3%fx+ 12 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fl+) (fl+ 3.0) (fl+ 3.0 4.0) (fl+ x) (fl+ x 0.0) (fl+ x -0.0) (fl+ 0.0 x) (fl+ -0.0 x) (fl+ x 3.0)
           (fl+ x 3.0 4.0) (fl+ 3.0 x 4.0) (fl+ 3.0 x -3.0) (fl+ (truncate -.5) x (/ -inf.0)) (fl+ 3.0 x 4.0 y 5.0)
           (fl+ 3.0 x +nan.0 y 5.0))))
    '(#2%list 0.0 3.0 7.0 (#2%fl+ x) (#2%fl+ 0.0 x) (#2%fl+ x) (#2%fl+ 0.0 x) (#2%fl+ x) (#2%fl+ 3.0 x)
       (#2%fl+ 7.0 x) (#2%fl+ 7.0 x) (#2%fl+ 0.0 x) (#2%fl+ x) (#2%fl+ 12.0 x y)
       (begin (#2%fl+ x y) +nan.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fl+) (fl+ 3.0) (fl+ 3.0 4.0) (fl+ x) (fl+ x 0.0) (fl+ x -0.0) (fl+ 0.0 x) (fl+ -0.0 x) (fl+ x 3.0)
           (fl+ x 3.0 4.0) (fl+ 3.0 x 4.0) (fl+ 3.0 x -3.0) (fl+ (truncate -.5) x (/ -inf.0)) (fl+ 3.0 x 4.0 y 5.0)
           (fl+ 3.0 x +nan.0 y 5.0))))
    '(#3%list 0.0 3.0 7.0 x (#3%fl+ 0.0 x) x (#3%fl+ 0.0 x) x (#3%fl+ 3.0 x)
       (#3%fl+ 7.0 x) (#3%fl+ 7.0 x) (#3%fl+ 0.0 x) x (#3%fl+ 12.0 x y)
       +nan.0))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (cfl+) (cfl+ 3.0) (cfl+ 3.0 4.0) (cfl+ x) (cfl+ x 0.0) (cfl+ x -0.0) (cfl+ 0.0 x) (cfl+ -0.0 x) (cfl+ x 3.0)
           (cfl+ x 3.0 4.0) (cfl+ 3.0 x 4.0) (cfl+ 3.0 x -3.0) (cfl+ (truncate -.5) x (/ -inf.0)) (cfl+ 3.0 x 4.0 y 5.0)
           (cfl+ 3.0 x +nan.0+nan.0i y 5.0))))
    '(#2%list 0.0 3.0 7.0 (#2%cfl+ x) (#2%cfl+ 0.0 x) (#2%cfl+ x) (#2%cfl+ 0.0 x) (#2%cfl+ x) (#2%cfl+ 3.0 x)
       (#2%cfl+ 7.0 x) (#2%cfl+ 7.0 x) (#2%cfl+ 0.0 x) (#2%cfl+ x) (#2%cfl+ 12.0 x y)
       (begin (#2%cfl+ x y) +nan.0+nan.0i)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (cfl+) (cfl+ 3.0) (cfl+ 3.0 4.0) (cfl+ x) (cfl+ x 0.0) (cfl+ x -0.0) (cfl+ 0.0 x) (cfl+ -0.0 x) (cfl+ x 3.0)
           (cfl+ x 3.0 4.0) (cfl+ 3.0 x 4.0) (cfl+ 3.0 x -3.0) (cfl+ (truncate -.5) x (/ -inf.0)) (cfl+ 3.0 x 4.0 y 5.0)
           (cfl+ 3.0 x +nan.0+nan.0i y 5.0))))
    '(#3%list 0.0 3.0 7.0 x (#3%cfl+ 0.0 x) x (#3%cfl+ 0.0 x) x (#3%cfl+ 3.0 x)
       (#3%cfl+ 7.0 x) (#3%cfl+ 7.0 x) (#3%cfl+ 0.0 x) x (#3%cfl+ 12.0 x y)
       +nan.0+nan.0i))

 ; check partial folding of *, fx*, fl*, and cfl*
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (*) (* 3) (* 3 4) (* x) (* x 1) (* 1 x) (* x 3)
           (* x 3 4) (* 3 x 4) (* 3 x 1/3) (* 3 x 4 y 5)
           (* 3 x 0 y 5))))
    '(#2%list 1 3 12 (#2%* x) (#2%* x) (#2%* x) (#2%* 3 x)
       (#2%* 12 x) (#2%* 12 x) (#2%* x) (#2%* 60 x y)
       (begin (#2%* x y) 0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (*) (* 3) (* 3 4) (* x) (* x 1) (* 1 x) (* x 3)
           (* x 3 4) (* 3 x 4) (* 3 x 1/3) (* 3 x 4 y 5)
           (* 3 x 0 y 5))))
    '(#3%list 1 3 12 x x x (#3%* 3 x)
       (#3%* 12 x) (#3%* 12 x) x (#3%* 60 x y)
       0))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fx*) (fx* 3) (fx* 3 4) (fx* x) (fx* x 1) (fx* 1 x) (fx* x 3)
           (fx* x 3 4) (fx* 3 x 4) (fx* 1 x 1) (fx* 3 x 4 y 5)
           (fx* 3 x 0 y 5))))
    '(#2%list 1 3 12 (#2%fx* x) (#2%fx* x) (#2%fx* x) (#2%fx* 3 x)
       (#2%fx* 12 x) (#2%fx* 12 x) (#2%fx* x) (#2%fx* 60 x y)
       (begin (#2%fx* x y) 0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fx*) (fx* 3) (fx* 3 4) (fx* x) (fx* x 1) (fx* 1 x) (fx* x 3)
           (fx* x 3 4) (fx* 3 x 4) (fx* 1 x 1) (fx* 3 x 4 y 5)
           (fx* 3 x 0 y 5))))
    '(#3%list 1 3 12 x x x (#3%fx* 3 x)
       (#3%fx* 12 x) (#3%fx* 12 x) x (#3%fx* 60 x y)
       0))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fl*) (fl* 3.0) (fl* 3.0 4.0) (fl* x) (fl* x 1.0) (fl* 1.0 x) (fl* x 3.0)
           (fl* x 3.0 4.0) (fl* 3.0 x 4.0) (fl* 3.0 x #i1/3) (fl* 3.0 x 4.0 y 5.0)
           (fl* 3.0 x 4.0 y +nan.0))))
    '(#2%list 1.0 3.0 12.0 (#2%fl* x) (#2%fl* x) (#2%fl* x) (#2%fl* 3.0 x)
       (#2%fl* 12.0 x) (#2%fl* 12.0 x) (#2%fl* x) (#2%fl* 60.0 x y)
       (begin (#2%fl* x y) +nan.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fl*) (fl* 3.0) (fl* 3.0 4.0) (fl* x) (fl* x 1.0) (fl* 1.0 x) (fl* x 3.0)
           (fl* x 3.0 4.0) (fl* 3.0 x 4.0) (fl* 3.0 x #i1/3) (fl* 3.0 x 4.0 y 5.0)
           (fl* 3.0 x 4.0 y +nan.0))))
    '(#3%list 1.0 3.0 12.0 x x x (#3%fl* 3.0 x)
       (#3%fl* 12.0 x) (#3%fl* 12.0 x) x (#3%fl* 60.0 x y)
       +nan.0))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (cfl*) (cfl* 3.0) (cfl* 3.0 4.0) (cfl* x) (cfl* x 1.0) (cfl* 1.0 x) (cfl* x 3.0)
           (cfl* x 3.0 4.0) (cfl* 3.0 x 4.0) (cfl* 3.0 x #i1/3) (cfl* 3.0 x 4.0 y 5.0)
           (cfl* 3.0 x 4.0 y +nan.0+nan.0i))))
    '(#2%list 1.0 3.0 12.0 (#2%cfl* x) (#2%cfl* x) (#2%cfl* x) (#2%cfl* 3.0 x)
       (#2%cfl* 12.0 x) (#2%cfl* 12.0 x) (#2%cfl* x) (#2%cfl* 60.0 x y)
       (begin (#2%cfl* x y) +nan.0+nan.0i)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (cfl*) (cfl* 3.0) (cfl* 3.0 4.0) (cfl* x) (cfl* x 1.0) (cfl* 1.0 x) (cfl* x 3.0)
           (cfl* x 3.0 4.0) (cfl* 3.0 x 4.0) (cfl* 3.0 x #i1/3) (cfl* 3.0 x 4.0 y 5.0)
           (cfl* 3.0 x 4.0 y +nan.0+nan.0i))))
    '(#3%list 1.0 3.0 12.0 x x x (#3%cfl* 3.0 x)
       (#3%cfl* 12.0 x) (#3%cfl* 12.0 x) x (#3%cfl* 60.0 x y)
       +nan.0+nan.0i))

 ; check partial folding of -, fx-, fl-, and cfl-
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (- 3) (- 3 4) (- x) (- x 0) (- 0 x) (- x 3) (- x 3 4)
           (- 3 x 4) (- 3 x 3) (- x 3 -3) (- 4 x 3 -3) (- 3 x 4 y 5))))
    '(#2%list -3 -1 (#2%- x) (#2%- x 0) (#2%- x) (#2%- x 3) (#2%- x 3 4) (#2%- 3 x 4)
       (#2%- 3 x 3) (#2%- x 3 -3) (#2%- 4 x 3 -3) (#2%- 3 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (- 3) (- 3 4) (- x) (- x 0) (- 0 x) (- x 3) (- x 3 4)
           (- 3 x 4) (- 3 x 3) (- x 3 -3) (- 4 x 3 -3) (- 3 x 4 y 5))))
    '(#3%list -3 -1 (#3%- x) (#3%- x 0) (#3%- x) (#3%- x 3) (#3%- x 3 4) (#3%- 3 x 4)
       (#3%- 3 x 3) (#3%- x 3 -3) (#3%- 4 x 3 -3) (#3%- 3 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fx- 3) (fx- 3 4) (fx- x) (fx- x 0) (fx- 0 x) (fx- x 3) (fx- x 3 4)
           (fx- 3 x 4) (fx- 3 x 3) (fx- x 3 -3) (fx- 4 x 3 -3) (fx- 3 x 4 y 5))))
    '(#2%list -3 -1 (#2%fx- x) (#2%fx- x 0) (#2%fx- x) (#2%fx- x 3) (#2%fx- x 3 4) (#2%fx- 3 x 4)
       (#2%fx- 3 x 3) (#2%fx- x 3 -3) (#2%fx- 4 x 3 -3) (#2%fx- 3 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fx- 3) (fx- 3 4) (fx- x) (fx- x 0) (fx- 0 x) (fx- x 3) (fx- x 3 4)
           (fx- 3 x 4) (fx- 3 x 3) (fx- x 3 -3) (fx- 4 x 3 -3) (fx- 3 x 4 y 5))))
    '(#3%list -3 -1 (#3%fx- x) (#3%fx- x 0) (#3%fx- x) (#3%fx- x 3) (#3%fx- x 3 4) (#3%fx- 3 x 4)
       (#3%fx- 3 x 3) (#3%fx- x 3 -3) (#3%fx- 4 x 3 -3) (#3%fx- 3 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fl- 3.0) (fl- 3.0 4.0) (fl- x) (fl- x 0.0) (fl- x -0.0) (fl- 0.0 x) (fl- -0.0 x) (fl- x 3.0) (fl- x 3.0 4.0)
           (fl- 3.0 x 4.0) (fl- 3.0 x 3.0) (fl- -0.0 x 0.0) (fl- x 3.0 -3.0) (fl- x 0.0 y) (fl- x -0.0 3.0) (fl- 4.0 x 3.0 -3.0)
           (fl- 3.0 x 4.0 y 5.0))))
    '(#2%list -3.0 -1.0 (#2%fl- x) (#2%fl- x 0.0) (#2%fl- x -0.0) (#2%fl- 0.0 x) (#2%fl- x) (#2%fl- x 3.0)
       (#2%fl- x 3.0 4.0) (#2%fl- 3.0 x 4.0) (#2%fl- 3.0 x 3.0) (#2%fl- -0.0 x 0.0) (#2%fl- x 3.0 -3.0)
       (#2%fl- x 0.0 y) (#2%fl- x -0.0 3.0) (#2%fl- 4.0 x 3.0 -3.0) (#2%fl- 3.0 x 4.0 y 5.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fl- 3.0) (fl- 3.0 4.0) (fl- x) (fl- x 0.0) (fl- x -0.0) (fl- 0.0 x) (fl- -0.0 x) (fl- x 3.0) (fl- x 3.0 4.0)
           (fl- 3.0 x 4.0) (fl- 3.0 x 3.0) (fl- -0.0 x 0.0) (fl- x 3.0 -3.0) (fl- x 0.0 y) (fl- x -0.0 3.0) (fl- 4.0 x 3.0 -3.0)
           (fl- 3.0 x 4.0 y 5.0))))
    '(#3%list -3.0 -1.0 (#3%fl- x) (#3%fl- x 0.0) (#3%fl- x -0.0) (#3%fl- 0.0 x) (#3%fl- x) (#3%fl- x 3.0)
       (#3%fl- x 3.0 4.0) (#3%fl- 3.0 x 4.0) (#3%fl- 3.0 x 3.0) (#3%fl- -0.0 x 0.0) (#3%fl- x 3.0 -3.0)
       (#3%fl- x 0.0 y) (#3%fl- x -0.0 3.0) (#3%fl- 4.0 x 3.0 -3.0) (#3%fl- 3.0 x 4.0 y 5.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (cfl- 3.0) (cfl- 3.0 4.0) (cfl- x) (cfl- x 0.0) (cfl- x -0.0) (cfl- 0.0 x) (cfl- -0.0 x) (cfl- x 3.0) (cfl- x 3.0 4.0)
           (cfl- 3.0 x 4.0) (cfl- 3.0 x 3.0) (cfl- -0.0 x 0.0) (cfl- x 3.0 -3.0) (cfl- x 0.0 y) (cfl- x -0.0 3.0) (cfl- 4.0 x 3.0 -3.0)
           (cfl- 3.0 x 4.0 y 5.0))))
    '(#2%list
       -3.0 -1.0 (#2%cfl- x) (#2%cfl- x 0.0) (#2%cfl- x -0.0) (#2%cfl- 0.0 x) (#2%cfl- x) (#2%cfl- x 3.0) (#2%cfl- x 3.0 4.0)
       (#2%cfl- 3.0 x 4.0) (#2%cfl- 3.0 x 3.0) (#2%cfl- -0.0 x 0.0) (#2%cfl- x 3.0 -3.0) (#2%cfl- x 0.0 y) (#2%cfl- x -0.0 3.0) (#2%cfl- 4.0 x 3.0 -3.0)
       (#2%cfl- 3.0 x 4.0 y 5.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (cfl- 3.0) (cfl- 3.0 4.0) (cfl- x) (cfl- x 0.0) (cfl- x -0.0) (cfl- 0.0 x) (cfl- -0.0 x) (cfl- x 3.0) (cfl- x 3.0 4.0)
           (cfl- 3.0 x 4.0) (cfl- 3.0 x 3.0) (cfl- -0.0 x 0.0) (cfl- x 3.0 -3.0) (cfl- x 0.0 y) (cfl- x -0.0 3.0) (cfl- 4.0 x 3.0 -3.0)
           (cfl- 3.0 x 4.0 y 5.0))))
    '(#3%list
       -3.0 -1.0 (#3%cfl- x) (#3%cfl- x 0.0) (#3%cfl- x -0.0) (#3%cfl- 0.0 x) (#3%cfl- x) (#3%cfl- x 3.0) (#3%cfl- x 3.0 4.0)
       (#3%cfl- 3.0 x 4.0) (#3%cfl- 3.0 x 3.0) (#3%cfl- -0.0 x 0.0) (#3%cfl- x 3.0 -3.0) (#3%cfl- x 0.0 y) (#3%cfl- x -0.0 3.0) (#3%cfl- 4.0 x 3.0 -3.0)
       (#3%cfl- 3.0 x 4.0 y 5.0)))

 ; check partial folding of /, fx/, fl/, and cfl/
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (/ 3) (/ 9 4) (/ x) (/ x 1) (/ 1 x) (/ x 3) (/ x 3 4)
           (/ 9 x 4) (/ 3 x 3) (/ x 3 1/3) (/ 4 x 3 1/3) (/ 50 x 4 y 5))))
    '(#2%list
       1/3 9/4 (#2%/ x) (#2%/ x 1) (#2%/ x) (#2%/ x 3) (#2%/ x 3 4)
       (#2%/ 9 x 4) (#2%/ 3 x 3) (#2%/ x 3 1/3) (#2%/ 4 x 3 1/3) (#2%/ 50 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (/ 3) (/ 9 4) (/ x) (/ x 1) (/ 1 x) (/ x 3) (/ x 3 4)
           (/ 9 x 4) (/ 3 x 3) (/ x 3 1/3) (/ 4 x 3 1/3) (/ 50 x 4 y 5))))
    '(#3%list
       1/3 9/4 (#3%/ x) (#3%/ x 1) (#3%/ x) (#3%/ x 3) (#3%/ x 3 4)
       (#3%/ 9 x 4) (#3%/ 3 x 3) (#3%/ x 3 1/3) (#3%/ 4 x 3 1/3) (#3%/ 50 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fx/ 3) (fx/ 9 4) (fx/ x) (fx/ x 1) (fx/ 1 x) (fx/ x 3) (fx/ x 3 4)
           (fx/ 9 x 4) (fx/ 1 x 1) (fx/ x 1 1) (fx/ 4 x 1 1) (fx/ 50 x 4 y 5))))
    '(#2%list
       0 2 (#2%fx/ x) (#2%fx/ x 1) (#2%fx/ x) (#2%fx/ x 3) (#2%fx/ x 3 4)
       (#2%fx/ 9 x 4) (#2%fx/ 1 x 1) (#2%fx/ x 1 1) (#2%fx/ 4 x 1 1) (#2%fx/ 50 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fx/ 3) (fx/ 9 4) (fx/ x) (fx/ x 1) (fx/ 1 x) (fx/ x 3) (fx/ x 3 4)
           (fx/ 9 x 4) (fx/ 1 x 1) (fx/ x 1 1) (fx/ 4 x 1 1) (fx/ 50 x 4 y 5))))
    '(#3%list
       0 2 (#3%fx/ x) (#3%fx/ x 1) (#3%fx/ x) (#3%fx/ x 3) (#3%fx/ x 3 4)
       (#3%fx/ 9 x 4) (#3%fx/ 1 x 1) (#3%fx/ x 1 1) (#3%fx/ 4 x 1 1) (#3%fx/ 50 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fxquotient 3) (fxquotient 9 4) (fxquotient x) (fxquotient x 1) (fxquotient 1 x) (fxquotient x 3) (fxquotient x 3 4)
           (fxquotient 9 x 4) (fxquotient 1 x 1) (fxquotient x 1 1) (fxquotient 4 x 1 1) (fxquotient 50 x 4 y 5))))
    '(#2%list
       0 2 (#2%fxquotient x) (#2%fxquotient x 1) (#2%fxquotient x) (#2%fxquotient x 3) (#2%fxquotient x 3 4)
       (#2%fxquotient 9 x 4) (#2%fxquotient 1 x 1) (#2%fxquotient x 1 1) (#2%fxquotient 4 x 1 1) (#2%fxquotient 50 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fxquotient 3) (fxquotient 9 4) (fxquotient x) (fxquotient x 1) (fxquotient 1 x) (fxquotient x 3) (fxquotient x 3 4)
           (fxquotient 9 x 4) (fxquotient 1 x 1) (fxquotient x 1 1) (fxquotient 4 x 1 1) (fxquotient 50 x 4 y 5))))
    '(#3%list
       0 2 (#3%fxquotient x) (#3%fxquotient x 1) (#3%fxquotient x) (#3%fxquotient x 3) (#3%fxquotient x 3 4)
       (#3%fxquotient 9 x 4) (#3%fxquotient 1 x 1) (#3%fxquotient x 1 1) (#3%fxquotient 4 x 1 1) (#3%fxquotient 50 x 4 y 5)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fl/ 2.0) (fl/ 9.0 4.0) (fl/ x) (fl/ x 1.0) (fl/ 1.0 x) (fl/ x 3.0) (fl/ x 3.0 4.0)
           (fl/ 9.0 x 4.0) (fl/ 3.0 x 3.0) (fl/ x 2.0 .5) (fl/ 4.0 x 2.0 .5)
           (fl/ 50.0 x 4.0 y 5.0))))
    '(#2%list
       .5 2.25 (#2%fl/ x) (#2%fl/ x 1.0) (#2%fl/ x) (#2%fl/ x 3.0) (#2%fl/ x 3.0 4.0)
       (#2%fl/ 9.0 x 4.0) (#2%fl/ 3.0 x 3.0) (#2%fl/ x 2.0 .5) (#2%fl/ 4.0 x 2.0 .5)
       (#2%fl/ 50.0 x 4.0 y 5.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fl/ 2.0) (fl/ 9.0 4.0) (fl/ x) (fl/ x 1.0) (fl/ 1.0 x) (fl/ x 3.0) (fl/ x 3.0 4.0)
           (fl/ 9.0 x 4.0) (fl/ 3.0 x 3.0) (fl/ x 2.0 .5) (fl/ 4.0 x 2.0 .5)
           (fl/ 50.0 x 4.0 y 5.0))))
    '(#3%list
       .5 2.25 (#3%fl/ x) (#3%fl/ x 1.0) (#3%fl/ x) (#3%fl/ x 3.0) (#3%fl/ x 3.0 4.0)
       (#3%fl/ 9.0 x 4.0) (#3%fl/ 3.0 x 3.0) (#3%fl/ x 2.0 .5) (#3%fl/ 4.0 x 2.0 .5)
       (#3%fl/ 50.0 x 4.0 y 5.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (cfl/ 2.0) (cfl/ 9.0 4.0) (cfl/ x) (cfl/ x 1.0) (cfl/ 1.0 x) (cfl/ x 3.0) (cfl/ x 3.0 4.0)
           (cfl/ 9.0 x 4.0) (cfl/ 3.0 x 3.0) (cfl/ x 2.0 .5) (cfl/ 4.0 x 2.0 .5)
           (cfl/ 50.0 x 4.0 y 5.0))))
    '(#2%list
       .5 2.25 (#2%cfl/ x) (#2%cfl/ x 1.0) (#2%cfl/ x) (#2%cfl/ x 3.0) (#2%cfl/ x 3.0 4.0)
       (#2%cfl/ 9.0 x 4.0) (#2%cfl/ 3.0 x 3.0) (#2%cfl/ x 2.0 .5) (#2%cfl/ 4.0 x 2.0 .5)
       (#2%cfl/ 50.0 x 4.0 y 5.0)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (cfl/ 2.0) (cfl/ 9.0 4.0) (cfl/ x) (cfl/ x 1.0) (cfl/ 1.0 x) (cfl/ x 3.0) (cfl/ x 3.0 4.0)
           (cfl/ 9.0 x 4.0) (cfl/ 3.0 x 3.0) (cfl/ x 2.0 .5) (cfl/ 4.0 x 2.0 .5)
           (cfl/ 50.0 x 4.0 y 5.0))))
    '(#3%list
       .5 2.25 (#3%cfl/ x) (#3%cfl/ x 1.0) (#3%cfl/ x) (#3%cfl/ x 3.0) (#3%cfl/ x 3.0 4.0)
       (#3%cfl/ 9.0 x 4.0) (#3%cfl/ 3.0 x 3.0) (#3%cfl/ x 2.0 .5) (#3%cfl/ 4.0 x 2.0 .5)
       (#3%cfl/ 50.0 x 4.0 y 5.0)))

 ; check partial folding of #{2,3}%{fx,}log{and,or,xor}
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (logand)
           (logand -1) (logand 0) (logand 7)
           (logand 5 0) (logand 0 5) (logand 5 -1) (logand -1 x) (logand x 0) (logand 5 3) (logand 5 x) (logand x y)
           (logand 5 0 3) (logand 5 7 -1 6) (logand x -1 y) (logand 13 x 7 y) (logand 13 x 7 0 y) (logand 13 x 7 -1 y))))
    '(#2%list
       -1
       -1 0 7
       0 0 5 (#2%logand x) (begin (#2%logand x) 0) 1 (#2%logand 5 x) (#2%logand x y)
       0 4 (#2%logand x y) (#2%logand 5 x y) (begin (#2%logand x y) 0) (#2%logand 5 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (logand)
           (logand -1) (logand 0) (logand 7)
           (logand 5 0) (logand 0 5) (logand 5 -1) (logand -1 x) (logand x 0) (logand 5 3) (logand 5 x) (logand x y)
           (logand 5 0 3) (logand 5 7 -1 6) (logand x -1 y) (logand 13 x 7 y) (logand 13 x 7 0 y) (logand 13 x 7 -1 y))))
    '(#3%list
       -1
       -1 0 7
       0 0 5 x 0 1 (#3%logand 5 x) (#3%logand x y)
       0 4 (#3%logand x y) (#3%logand 5 x y) 0 (#3%logand 5 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fxlogand)
           (fxlogand -1) (fxlogand 0) (fxlogand 7)
           (fxlogand 5 0) (fxlogand 0 5) (fxlogand 5 -1) (fxlogand -1 x) (fxlogand x 0) (fxlogand 5 3) (fxlogand 5 x) (fxlogand x y)
           (fxlogand 5 0 3) (fxlogand 5 7 -1 6) (fxlogand x -1 y) (fxlogand 13 x 7 y) (fxlogand 13 x 7 0 y) (fxlogand 13 x 7 -1 y))))
    '(#2%list
       -1
       -1 0 7
       0 0 5 (#2%fxlogand x) (begin (#2%fxlogand x) 0) 1 (#2%fxlogand 5 x) (#2%fxlogand x y)
       0 4 (#2%fxlogand x y) (#2%fxlogand 5 x y) (begin (#2%fxlogand x y) 0) (#2%fxlogand 5 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fxlogand)
           (fxlogand -1) (fxlogand 0) (fxlogand 7)
           (fxlogand 5 0) (fxlogand 0 5) (fxlogand 5 -1) (fxlogand -1 x) (fxlogand x 0) (fxlogand 5 3) (fxlogand 5 x) (fxlogand x y)
           (fxlogand 5 0 3) (fxlogand 5 7 -1 6) (fxlogand x -1 y) (fxlogand 13 x 7 y) (fxlogand 13 x 7 0 y) (fxlogand 13 x 7 -1 y))))
    '(#3%list
       -1
       -1 0 7
       0 0 5 x 0 1 (#3%fxlogand 5 x) (#3%fxlogand x y)
       0 4 (#3%fxlogand x y) (#3%fxlogand 5 x y) 0 (#3%fxlogand 5 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fxlogor)
           (fxlogor -1) (fxlogor 0) (fxlogor 7)
           (fxlogor 5 0) (fxlogor 0 5) (fxlogor 5 -1) (fxlogor -1 x) (fxlogor x 0) (fxlogor 5 3) (fxlogor 5 x) (fxlogor x y)
           (fxlogor 5 0 3) (fxlogor 5 7 -1 6) (fxlogor x 0 y) (fxlogor 13 x 7 y) (fxlogor 13 x 7 -1 y) (fxlogor 13 x 7 0 y))))
    '(#2%list
       0
       -1 0 7
       5 5 -1 (begin (#2%fxlogor x) -1) (#2%fxlogor x) 7 (#2%fxlogor 5 x) (#2%fxlogor x y)
       7 -1 (#2%fxlogor x y) (#2%fxlogor 15 x y) (begin (#2%fxlogor x y) -1) (#2%fxlogor 15 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fxlogor)
           (fxlogor -1) (fxlogor 0) (fxlogor 7)
           (fxlogor 5 0) (fxlogor 0 5) (fxlogor 5 -1) (fxlogor -1 x) (fxlogor x 0) (fxlogor 5 3) (fxlogor 5 x) (fxlogor x y)
           (fxlogor 5 0 3) (fxlogor 5 7 -1 6) (fxlogor x 0 y) (fxlogor 13 x 7 y) (fxlogor 13 x 7 -1 y) (fxlogor 13 x 7 0 y))))
    '(#3%list
       0
       -1 0 7
       5 5 -1 -1 x 7 (#3%fxlogor 5 x) (#3%fxlogor x y)
       7 -1 (#3%fxlogor x y) (#3%fxlogor 15 x y) -1 (#3%fxlogor 15 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (logor)
           (logor -1) (logor 0) (logor 7)
           (logor 5 0) (logor 0 5) (logor 5 -1) (logor -1 x) (logor x 0) (logor 5 3) (logor 5 x) (logor x y)
           (logor 5 0 3) (logor 5 7 -1 6) (logor x 0 y) (logor 13 x 7 y) (logor 13 x 7 -1 y) (logor 13 x 7 0 y))))
    '(#2%list
       0
       -1 0 7
       5 5 -1 (begin (#2%logor x) -1) (#2%logor x) 7 (#2%logor 5 x) (#2%logor x y)
       7 -1 (#2%logor x y) (#2%logor 15 x y) (begin (#2%logor x y) -1) (#2%logor 15 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (logor)
           (logor -1) (logor 0) (logor 7)
           (logor 5 0) (logor 0 5) (logor 5 -1) (logor -1 x) (logor x 0) (logor 5 3) (logor 5 x) (logor x y)
           (logor 5 0 3) (logor 5 7 -1 6) (logor x 0 y) (logor 13 x 7 y) (logor 13 x 7 -1 y) (logor 13 x 7 0 y))))
    '(#3%list
       0
       -1 0 7
       5 5 -1 -1 x 7 (#3%logor 5 x) (#3%logor x y)
       7 -1 (#3%logor x y) (#3%logor 15 x y) -1 (#3%logor 15 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (logxor)
           (logxor -1) (logxor 0) (logxor 7)
           (logxor 5 0) (logxor 0 5) (logxor 5 -1) (logxor -1 x) (logxor x 0) (logxor 5 3) (logxor 5 x) (logxor x y)
           (logxor 5 0 3) (logxor 5 7 -1 6) (logxor x 0 y) (logxor 13 x 7 y) (logxor 13 x 7 -1 y) (logxor 13 x 7 0 y))))
    '(#2%list
       0
       -1 0 7
       5 5 -6 (#2%logxor -1 x) (#2%logxor x) 6 (#2%logxor 5 x) (#2%logxor x y)
       6 -5 (#2%logxor x y) (#2%logxor 10 x y) (#2%logxor -11 x y) (#2%logxor 10 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (logxor)
           (logxor -1) (logxor 0) (logxor 7)
           (logxor 5 0) (logxor 0 5) (logxor 5 -1) (logxor -1 x) (logxor x 0) (logxor 5 3) (logxor 5 x) (logxor x y)
           (logxor 5 0 3) (logxor 5 7 -1 6) (logxor x 0 y) (logxor 13 x 7 y) (logxor 13 x 7 -1 y) (logxor 13 x 7 0 y))))
    '(#3%list
       0
       -1 0 7
       5 5 -6 (#3%logxor -1 x) x 6 (#3%logxor 5 x) (#3%logxor x y)
       6 -5 (#3%logxor x y) (#3%logxor 10 x y) (#3%logxor -11 x y) (#3%logxor 10 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(list
           (fxlogxor)
           (fxlogxor -1) (fxlogxor 0) (fxlogxor 7)
           (fxlogxor 5 0) (fxlogxor 0 5) (fxlogxor 5 -1) (fxlogxor -1 x) (fxlogxor x 0) (fxlogxor 5 3) (fxlogxor 5 x) (fxlogxor x y)
           (fxlogxor 5 0 3) (fxlogxor 5 7 -1 6) (fxlogxor x 0 y) (fxlogxor 13 x 7 y) (fxlogxor 13 x 7 -1 y) (fxlogxor 13 x 7 0 y))))
    '(#2%list
       0
       -1 0 7
       5 5 -6 (#2%fxlogxor -1 x) (#2%fxlogxor x) 6 (#2%fxlogxor 5 x) (#2%fxlogxor x y)
       6 -5 (#2%fxlogxor x y) (#2%fxlogxor 10 x y) (#2%fxlogxor -11 x y) (#2%fxlogxor 10 x y)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f]
                   [run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 3])
      (expand/optimize
        '(list
           (fxlogxor)
           (fxlogxor -1) (fxlogxor 0) (fxlogxor 7)
           (fxlogxor 5 0) (fxlogxor 0 5) (fxlogxor 5 -1) (fxlogxor -1 x) (fxlogxor x 0) (fxlogxor 5 3) (fxlogxor 5 x) (fxlogxor x y)
           (fxlogxor 5 0 3) (fxlogxor 5 7 -1 6) (fxlogxor x 0 y) (fxlogxor 13 x 7 y) (fxlogxor 13 x 7 -1 y) (fxlogxor 13 x 7 0 y))))
    '(#3%list
       0
       -1 0 7
       5 5 -6 (#3%fxlogxor -1 x) x 6 (#3%fxlogxor 5 x) (#3%fxlogxor x y)
       6 -5 (#3%fxlogxor x y) (#3%fxlogxor 10 x y) (#3%fxlogxor -11 x y) (#3%fxlogxor 10 x y)))
)

(mat cp0-apply
  (begin
    (define $permutations
      (rec permutations
        (lambda (x*)
          (if (null? x*)
              '()
              (if (null? (cdr x*))
                  (list x*)
                  (let f ([x* x*] [rx* '()])
                    (if (null? x*)
                        '()
                        (append
                          (map (lambda (ls) (cons (car x*) ls)) (permutations (append (cdr x*) rx*)))
                          (f (cdr x*) (cons (car x*) rx*))))))))))
    #t)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply (lambda () 7) '())))
    '7)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (lambda (x y z) (#%+ x y z)) '(3 4 5))))
    '12)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (lambda (x y z) (#%+ x y z)) (#%list 3 4 5))))
    '12)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply
           (lambda (x y z) (#%+ (begin (#%write 'a) x) y z))
           (#%list e1 e2 e3))))
    (if (= (optimize-level) 3)
        '(let ([x e1] [y e2] [z e3])
           (#3%+ (begin (#3%write 'a) x) y z))
        '(let ([x e1] [y e2] [z e3])
           (#2%+ (begin (#2%write 'a) x) y z))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply #%+ '(1 2 3 4))))
    '10)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply #%+ (#%list 1 2 3 4))))
    '10)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(lambda (x) (#%apply #%+ (#%list 1 2 x 4)))))
    (if (= (optimize-level) 3)
        '(lambda (x) (#3%+ 7 x))
        '(lambda (x) (#2%+ 7 x))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (lambda (x y z) (#%+ x y z)) (#%list e1 e2 e3))))
    (if (= (optimize-level) 3)
        '(#3%+ e1 e2 e3)
        '(#2%+ e1 e2 e3)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply #%+ (#%list 1 (begin (#%write 'a) 2) 3))))
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) 6)
        '(begin (#2%write 'a) 6)))
  (let ([expr (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
                (expand/optimize
                  '(#%apply (begin (#%write 'a) #%+)
                     (begin (#%write 'b) 4)
                     (begin
                       (#%write 'c)
                       (#%list
                         1
                         (begin (#%write 'd) 2)
                         (begin (#%write 'e) 3))))))])
    (ormap
      (lambda (groups) (equivalent-expansion? expr `(begin ,@(apply append groups) 10)))
      ($permutations
        (if (= (optimize-level) 3)
            '(((#3%write 'a)) ((#3%write 'b)) ((#3%write 'c) (#3%write 'd) (#3%write 'e)))
            '(((#2%write 'a)) ((#2%write 'b)) ((#2%write 'c) (#2%write 'd) (#2%write 'e)))))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (lambda (x y z) (#%vector x y)) (#%list e1 2 e3))))
    (if (= (optimize-level) 3)
        '(#3%vector e1 2)
        '(begin e3 (#2%vector e1 2))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(lambda (x) (#%apply x '(1 2 3)))))
    '(lambda (x) (x 1 2 3)))
  (let ([q (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
             (expand/optimize '(#%apply e0 (#%list e1 e2 e3))))])
    (or (equivalent-expansion?  q '(let ([t1 e1] [t2 e2] [t3 e3]) (e0 t1 t2 t3)))
        (equivalent-expansion?  q '(let ([t0 e0]) (t0 e1 e2 e3)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply
           (case-lambda [(x y) x] [(a b c d e) c])
           (#%list 1 2 3 4 5))))
    '3)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (case-lambda [(x y) x] [r r]) (#%list 1 2 3 4 5))))
    '(#3%list 1 2 3 4 5))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (case-lambda [(x y) x] [r r]) (#%list 1 2 q 4 5))))
    '(#3%list 1 2 q 4 5))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply #%apply #%+ (#%list 1 2 3 (#%list 4 5)))))
    15)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply #%apply #%apply #%apply #%apply #%+ (#%list 1 2 3 (#%list 4 5 (#%list 6 7 (#%list* 8 9 (#%list (#%list 10)))))))))
    55)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply #%apply #%apply #%apply #%+ (#%cons 1 (#%list 2 3 (#%cons* 4 (#%list 5 (#%cons 6 (#%list* 7 (#%list 8 (#%cons 9 '(10))))))))))))
    55)
  (begin
    (define $check-writes
      (lambda (eepat x)
        (define ordered?
          (lambda (ls)
            (define same-prefix?
              (lambda (ls1 ls2)
                (or (null? ls2)
                    (and (eqv? (car ls1) (car ls2))
                         (same-prefix? (cdr ls1) (cdr ls2))))))
            (null?
              (let f ([ls ls] [q '()] [qlen 0])
                (if (null? ls)
                    '()
                    (let ([x (car ls)])
                      (let ([xlen (length x)])
                        (cond
                          [(fx= xlen qlen) (f (cdr ls) x xlen)]
                          [(fx< xlen qlen) ls]
                          [else (and (fx= xlen (fx+ qlen 1))
                                     (same-prefix? x q)
                                     (let ([ls (f (cdr ls) x xlen)])
                                       (and ls (f ls q qlen))))]))))))))
        (syntax-case x (begin $primitive quote)
          [(begin
             (($primitive level write) (quote (d ...)))
             ...
             ans)
           (begin
             (unless (equivalent-expansion? #'ans eepat) (errorf #f "~s is not equivalent to ~s" #'ans eepat))
             (unless (ordered? #'((d ...) ...)) (errorf #f "writes are out-of-order in ~s" x))
             #t)]
          [_ (errorf #f "unexpected output pattern for ~s" x)])))
    #t)
  ($check-writes 55
   (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
     (expand/optimize '(let ()
                         (import (chezscheme))
                         (let ([list (begin (write '()) list)] [list* (if #t list* list)])
                           (write '(1))
                           ((begin (write '(1 1)) apply)
                            (begin (write '(1 2)) apply)
                            (begin (write '(1 3)) apply)
                            (let ([waste (write '(1 4))]) apply)
                            (begin (write '(1 5)) apply)
                            (begin (write '(1 6)) +)
                            (begin (write '(1 7))
                              ((begin (write '(1 7 1)) list)
                               (begin (write '(1 7 2)) 1)
                               (begin (write '(1 7 3)) 2)
                               (begin (write '(1 7 4)) 3)
                               (begin (write '(1 7 5))
                                 ((begin (write '(1 7 5 1)) list)
                                  (begin (write '(1 7 5 2)) 4)
                                  (begin (write '(1 7 5 3)) 5)
                                  (begin (write '(1 7 5 4))
                                    ((begin (write '(1 7 5 4 1)) list)
                                     (begin (write '(1 7 5 4 2)) 6)
                                     (begin (write '(1 7 5 4 3)) 7)
                                     (begin (write '(1 7 5 4 4))
                                       ((begin (write '(1 7 5 4 4 1)) list*)
                                        (begin (write '(1 7 5 4 4 2)) 8)
                                        (begin (write '(1 7 5 4 4 3)) 9)
                                        (begin (write '(1 7 5 4 4 4))
                                          ((begin (write '(1 7 5 4 4 1)) list)
                                           (begin (write '(1 7 5 4 4 2))
                                             ((begin (write '(1 7 5 4 4 2 1)) list)
                                              (begin (write '(1 7 5 4 4 2 2)) 10)))))))))))))))))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 (cp0 (cp0 x))))])
      (expand/optimize '(#%apply #%apply #%+ (#%list 1 2 3 (#%list 4 5)))))
    '15)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply (lambda () 7) (#%list* '()))))
    '7)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (lambda (x y z) (#%+ x y z)) (#%list* 3 4 '(5)))))
    '12)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply #%+ (#%list* e '(2 3)))))
    (if (= (optimize-level) 3)
        '(#3%+ 5 e)
        '(#2%+ 5 e)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply
           (lambda (x y z) (#%+ (begin (#%write 'a) x) y z))
           (#%list* e1 e2 e3 '()))))
    (if (= (optimize-level) 3)
        '(let ([x e1] [y e2] [z e3])
           (#3%+ (begin (#3%write 'a) x) y z))
        '(let ([x e1] [y e2] [z e3])
           (#2%+ (begin (#2%write 'a) x) y z))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (lambda (x y z) (#%+ x y z)) (#%list* e1 e2 e3 '()))))
    (if (= (optimize-level) 3)
        '(#3%+ e1 e2 e3)
        '(#2%+ e1 e2 e3)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply #%+ (#%list* 1 (begin (#%write 'a) 2) '(3)))))
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) 6)
        '(begin (#2%write 'a) 6)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(#%apply (lambda (x y z) (#%vector x y)) (#%list* e1 2 e3 '()))))
    (if (= (optimize-level) 3)
        '(#3%vector e1 2)
        '(begin e3 (#2%vector e1 2))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(#%apply (lambda (x y z) (#%vector x y z)) (#%list* 1 '(2 3)))))
    (if (= (optimize-level) 3)
        '(#3%vector 1 2 3)
        '(#2%vector 1 2 3)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(lambda (r) (#%apply (lambda (x y z) (#%vector x y z)) (#%list* 1 r)))))
    (if (= (optimize-level) 3)
        '(lambda (r) (let ([y (#3%car r)]) (#3%vector 1 y (#3%car (#3%cdr r)))))
        '(lambda (r) (#2%apply (lambda (x y z) (#2%vector x y z)) 1 r))))
  (let ([expr (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
                (expand/optimize
                  '(#%apply (begin (#%write 'a) #%+)
                     (begin (#%write 'b) 4)
                     (begin
                       (#%write 'c)
                       (#%list*
                         1
                         (begin (#%write 'd) 2)
                         (begin (#%write 'e) '(3)))))))])
    (ormap
      (lambda (groups) (equivalent-expansion? expr `(begin ,@(apply append groups) 10)))
      ($permutations
        (if (= (optimize-level) 3)
            '(((#3%write 'a)) ((#3%write 'b)) ((#3%write 'c) (#3%write 'd) (#3%write 'e)))
            '(((#2%write 'a)) ((#2%write 'b)) ((#2%write 'c) (#2%write 'd) (#2%write 'e)))))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(let ([x (cons 0 (list))]) (#%apply #%zero? x))))
    #t)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      ;; don't fold primitive in value context with bad apply convention
      (expand/optimize '(#%apply #%zero? 0)))
    (if (= (optimize-level) 3)
        '(#3%apply #3%zero? 0)
        '(#2%apply #2%zero? 0)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      ;; don't fold primitive in test context with bad apply convention
      (expand/optimize '(if (#%apply #%eof-object 1 2 3) 4 5)))
    (if (= (optimize-level) 3)
        '(if (#3%apply #3%eof-object 1 2 3) 4 5)
        '(if (#2%apply #2%eof-object 1 2 3) 4 5)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      ;; don't fold primitive in effect context with bad apply convention
      (expand/optimize '(begin (#%apply #%box? 'step) 3)))
    (if (= (optimize-level) 3)
        '(begin (#3%apply #3%box? 'step) 3)
        '(begin (#2%apply #2%box? 'step) 3)))
 )

(mat cp0-car/cdr
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%car)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%cons)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'f) ($yyy) (#3%write 'e) ($xxx))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'f) ($yyy) (#2%write 'e) ($xxx))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%car)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%list)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))
               (begin (#%write 'g) ($zzz))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'f) ($yyy) (#3%write 'g) ($zzz) (#3%write 'e) ($xxx))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'f) ($yyy) (#2%write 'g) ($zzz) (#2%write 'e) ($xxx))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%car)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%list*)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))
               (begin (#%write 'g) ($zzz))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'f) ($yyy) (#3%write 'g) ($zzz) (#3%write 'e) ($xxx))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'f) ($yyy) (#2%write 'g) ($zzz) (#2%write 'e) ($xxx))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%car)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%cons*)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))
               (begin (#%write 'g) ($zzz))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'f) ($yyy) (#3%write 'g) ($zzz) (#3%write 'e) ($xxx))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'f) ($yyy) (#2%write 'g) ($zzz) (#2%write 'e) ($xxx))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%cons)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'f) ($yyy))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'f) ($yyy))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%list)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))
               (begin (#%write 'g) ($zzz))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%list (begin (#3%write 'f) ($yyy)) (begin (#3%write 'g) ($zzz))))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%list (begin (#2%write 'f) ($yyy)) (begin (#2%write 'g) ($zzz))))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%list*)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))
               (begin (#%write 'g) ($zzz))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%list* (begin (#3%write 'f) ($yyy)) (begin (#3%write 'g) ($zzz))))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%list* (begin (#2%write 'f) ($yyy)) (begin (#2%write 'g) ($zzz))))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%cons*)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))
               (begin (#%write 'g) ($zzz))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%cons* (begin (#3%write 'f) ($yyy)) (begin (#3%write 'g) ($zzz))))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%cons* (begin (#2%write 'f) ($yyy)) (begin (#2%write 'g) ($zzz))))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%list*)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'f) ($yyy))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'f) ($yyy))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%cons*)
               (begin (#%write 'e) ($xxx))
               (begin (#%write 'f) ($yyy))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'f) ($yyy))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'f) ($yyy))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%list*)
               (begin (#%write 'e) ($xxx))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%cdr (begin (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx))))
        '(begin (#2%write 'a) (#2%write 'b) (#2%cdr (begin (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx))))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin (#%write 'a)
           ((begin (#%write 'b) #%cdr)
            (begin (#%write 'c)
              ((begin (#%write 'd) #%cons*)
               (begin (#%write 'e) ($xxx))))))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%cdr (begin (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx))))
        '(begin (#2%write 'a) (#2%write 'b) (#2%cdr (begin (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx))))))
 )

(mat cp0-seq-ref
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(vector-ref (vector 1 2 3) 1)))
    2)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(list-ref (list 1 2 3) 1)))
    2)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(list-ref (list* 1 2 3) 1)))
    2)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(list-ref (cons* 1 2 3) 1)))
    2)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(fxvector-ref (fxvector 1 2 3) 1)))
    2)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(string-ref (string #\1 #\2 #\3) 1)))
    #\2)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) vector-ref)
            (begin (write 'c)
              ((begin (write 'd) vector)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) ($yyy))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'h) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'g) ($zzz) (#2%write 'f) ($yyy))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) vector-ref)
            (begin (write 'c)
              ((begin (write 'd) vector)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 3)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin
           (#3%write 'a)
           (#3%write 'b)
           (#3%vector-ref
             (begin
               (#3%write 'c)
               (#3%write 'd)
               (#3%vector
                 (begin (#3%write 'e) ($xxx))
                 (begin (#3%write 'f) ($yyy))
                 (begin (#3%write 'g) ($zzz))))
             (begin (#3%write 'h) 3)))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%vector-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%vector
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 3)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) list-ref)
            (begin (write 'c)
              ((begin (write 'd) list)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) ($yyy))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'h) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'g) ($zzz) (#2%write 'f) ($yyy))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) list-ref)
            (begin (write 'c)
              ((begin (write 'd) list*)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) ($yyy))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'h) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'g) ($zzz) (#2%write 'f) ($yyy))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) list-ref)
            (begin (write 'c)
              ((begin (write 'd) cons*)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) ($yyy))
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'h) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'g) ($zzz) (#2%write 'f) ($yyy))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) list-ref)
            (begin (write 'c)
              ((begin (write 'd) cons*)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 2)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin
           (#3%write 'a)
           (#3%write 'b)
           (#3%list-ref
             (begin
               (#3%write 'c)
               (#3%write 'd)
               (#3%cons*
                 (begin (#3%write 'e) ($xxx))
                 (begin (#3%write 'f) ($yyy))
                 (begin (#3%write 'g) ($zzz))))
             (begin (#3%write 'h) 2)))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%list-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%cons*
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 2)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) string-ref)
            (begin (write 'c)
              ((begin (write 'd) string)
               (begin (write 'e) ($xxx))
               (begin (write 'f) #\y)
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) #\y)
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'h) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'g) ($zzz) (#2%write 'f) #\y)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) string-ref)
            (begin (write 'c)
              ((begin (write 'd) string)
               (begin (write 'e) ($xxx))
               (begin (write 'f) 'oops)
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) 'oops)
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%string-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%string
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) 'oops)
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 1)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) string-ref)
            (begin (write 'c)
              ((begin (write 'd) string)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) ($yyy))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%string-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%string
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 1)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) string-ref)
            (begin (write 'c)
              ((begin (write 'd) #2%string)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin
           (#3%write 'a)
           (#3%write 'b)
           (#3%string-ref
             (begin
               (#3%write 'c)
               (#3%write 'd)
               (#2%string
                 (begin (#3%write 'e) ($xxx))
                 (begin (#3%write 'f) ($yyy))
                 (begin (#3%write 'g) ($zzz))))
             (begin (#3%write 'h) 1)))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%string-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%string
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 1)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) string-ref)
            (begin (write 'c)
              ((begin (write 'd) string)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 3)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin
           (#3%write 'a)
           (#3%write 'b)
           (#3%string-ref
             (begin
               (#3%write 'c)
               (#3%write 'd)
               (#3%string
                 (begin (#3%write 'e) ($xxx))
                 (begin (#3%write 'f) ($yyy))
                 (begin (#3%write 'g) ($zzz))))
             (begin (#3%write 'h) 3)))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%string-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%string
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 3)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) fxvector-ref)
            (begin (write 'c)
              ((begin (write 'd) fxvector)
               (begin (write 'e) ($xxx))
               (begin (write 'f) 121)
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) 121)
        '(begin (#2%write 'a) (#2%write 'b) (#2%write 'h) (#2%write 'c) (#2%write 'd) (#2%write 'e) ($xxx) (#2%write 'g) ($zzz) (#2%write 'f) 121)))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) fxvector-ref)
            (begin (write 'c)
              ((begin (write 'd) fxvector)
               (begin (write 'e) ($xxx))
               (begin (write 'f) 'oops)
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) 'oops)
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%fxvector-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%fxvector
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) 'oops)
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 1)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) fxvector-ref)
            (begin (write 'c)
              ((begin (write 'd) fxvector)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin (#3%write 'a) (#3%write 'b) (#3%write 'h) (#3%write 'c) (#3%write 'd) (#3%write 'e) ($xxx) (#3%write 'g) ($zzz) (#3%write 'f) ($yyy))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%fxvector-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%fxvector
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 1)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) fxvector-ref)
            (begin (write 'c)
              ((begin (write 'd) #2%fxvector)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 1)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin
           (#3%write 'a)
           (#3%write 'b)
           (#3%fxvector-ref
             (begin
               (#3%write 'c)
               (#3%write 'd)
               (#2%fxvector
                 (begin (#3%write 'e) ($xxx))
                 (begin (#3%write 'f) ($yyy))
                 (begin (#3%write 'g) ($zzz))))
             (begin (#3%write 'h) 1)))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%fxvector-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%fxvector
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 1)))))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level (max (optimize-level) 2)])
      (expand/optimize
        '(begin (write 'a)
           ((begin (write 'b) fxvector-ref)
            (begin (write 'c)
              ((begin (write 'd) fxvector)
               (begin (write 'e) ($xxx))
               (begin (write 'f) ($yyy))
               (begin (write 'g) ($zzz))))
            (begin (write 'h) 3)))))
    ; other possibilities exist but are too many to list and too difficult to construct with $permutations.
    ; if you see a problem, convert to use $check-writes (defined above)
    (if (= (optimize-level) 3)
        '(begin
           (#3%write 'a)
           (#3%write 'b)
           (#3%fxvector-ref
             (begin
               (#3%write 'c)
               (#3%write 'd)
               (#3%fxvector
                 (begin (#3%write 'e) ($xxx))
                 (begin (#3%write 'f) ($yyy))
                 (begin (#3%write 'g) ($zzz))))
             (begin (#3%write 'h) 3)))
        '(begin
           (#2%write 'a)
           (#2%write 'b)
           (#2%fxvector-ref
             (begin
               (#2%write 'c)
               (#2%write 'd)
               (#2%fxvector
                 (begin (#2%write 'e) ($xxx))
                 (begin (#2%write 'f) ($yyy))
                 (begin (#2%write 'g) ($zzz))))
             (begin (#2%write 'h) 3)))))
 )

(mat let-pushing
  ; make sure letify doesn't drop the let binding for x into the call to cons which would
  ; cause the allocation of z's location not to be in the continuation of the rhs of x.
  (equal?
    (let ([ls '()])
      (let ([th.k (let ([x (call/cc (lambda (k) k))] [z 0])
                    (cons (lambda () (set! z (+ z 1)) z) x))])
        (and (set! ls (cons ((car th.k)) ls))
             (set! ls (cons ((car th.k)) ls))
             ((cdr th.k) (lambda (x) (set! ls (cons 17 ls))))))
      ls)
    '(17 2 1 2 1))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(lambda (x) (letrec ([y (if (pair? x) (#3%car x) x)]) 4))))
    '(lambda (x) 4))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(let ([x e]) (list (list x)))))
    '(#2%list (#2%list e)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(let ([x (lambda (x) x)]) (list (list x) (list 3)))))
    '(#2%list (#2%list (lambda (x) x)) (#2%list 3)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(lambda (y) (let ([x (+ y y)] [z #f]) (list (lambda () (set! z 15) z) x)))))
    '(lambda (y) (let ([x (#2%+ y y)] [z #f]) (#2%list (lambda () (set! z 15) z) x))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 3])
      (expand/optimize
        '(lambda (y) (let ([x (+ y y)] [z #f]) (list (lambda () (set! z 15) z) x)))))
    ; doesn't push (+ y y) because it's not pure and one of the vars (z) is assigned
    '(lambda (y) (let ([x (#3%+ y y)] [z #f]) (#3%list (lambda () (set! z 15) z) x))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 3])
      (expand/optimize
        '(lambda (y) (let ([x (make-message-condition y)] [z #f]) (list (lambda () (set! z 15) z) x)))))
    ; does push (make-message-condition y) because it is pure, even though one of the vars (z) is assigned
    '(lambda (y) (let ([z #f]) (#3%list (lambda () (set! z 15) z) (#3%make-message-condition y)))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2] [compile-profile #f])
      (expand/optimize
        '(let ()
           (define-record foo ((immutable boolean x)))
           (or (foo-x e1) e2))))
    `(if (let ([g0 e1])
           (if (#3%record? g0 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'foo-x g0 ',record-type-descriptor?))
           (#3%$object-ref 'boolean g0 ,fixnum?))
         #t
         e2))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 3] [compile-profile #f])
      (expand/optimize
        '(let ()
           (define-record foo ((immutable boolean x)))
           (or (foo-x e1) e2))))
    `(if (#3%$object-ref 'boolean e1 ,fixnum?) #t e2))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(lambda (v)
           (let ([v2 (if (vector? v) v (error))])
             (let ([q (vector-sort v2)] [n (#3%vector-length v)])
               (display "1")
               (list q n))))))
    '(lambda (v)
       (let ([v2 (if (#2%vector? v) v (#2%error))])
         (let ([q (#2%vector-sort v2)] [n (#3%vector-length v)])
           (#2%display "1")
           (#2%list q n)))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))] [optimize-level 2])
      (expand/optimize
        '(lambda (v)
           (let ([v2 (if (vector? v) v (error))])
             (let ([q (vector-sort v2)] [n (or v 72)])
               (display "1")
               (list q n))))))
    '(lambda (v)
       (let ([q (#2%vector-sort (if (#2%vector? v) v (#2%error)))]
             [n (if v v 72)])
         (#2%display "1")
         (#2%list q n))))
)

(mat equality-of-refs
  (begin
    (define-syntax eqtest
      (syntax-rules ()
        [(_ eqprim) (eqtest eqprim #f)]
        [(_ eqprim generic?)
         (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
           (let ([arity-mask (procedure-arity-mask eqprim)] [primref `($primitive ,(if (= (optimize-level) 3) 3 2) eqprim)])
             (define-syntax ifsafe
               (syntax-rules ()
                 [(_ n e1 e2)
                  (if (and (fxbit-set? arity-mask n) (or generic? (= (optimize-level) 3))) e1 e2)]))
             (and
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim x)))
                 (ifsafe 1
                   `(lambda (x) #t)
                   `(lambda (x) (,primref x))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim (begin (x) x))))
                 (ifsafe 1
                   `(lambda (x) (x) #t)
                   `(lambda (x) (,primref (begin (x) x)))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (set! x (x x)) (x (eqprim x))))
                 (ifsafe 1
                   `(lambda (x) (set! x (x x)) (x #t))
                   `(lambda (x) (set! x (x x)) (x (,primref x)))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim (x x))))
                 (ifsafe 1
                   `(lambda (x) (x x) #t)
                   `(lambda (x) (,primref (x x)))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim x x)))
                 (ifsafe 2
                   `(lambda (x) #t)
                   `(lambda (x) (,primref x x))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim (begin (x) x) x)))
                 (ifsafe 2
                   `(lambda (x) (x) #t)
                   `(lambda (x) (,primref (begin (x) x) x))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim x (begin (x) x))))
                 (ifsafe 2
                   `(lambda (x) (x) #t)
                   `(lambda (x) (,primref x (begin (x) x)))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim (begin (x) x) (begin (x x) x))))
                 (ifsafe 2
                   `(lambda (x) (x) (x x) #t)
                   `(lambda (x) (,primref (begin (x) x) (begin (x x) x)))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x y) (eqprim x y)))
                 `(lambda (x y) (,primref x y)))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim x x x x x)))
                 (ifsafe 5
                   `(lambda (x) #t)
                   `(lambda (x) (,primref x x x x x))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x y) (eqprim x x x x y)))
                 `(lambda (x y) (,primref x x x x y)))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim x x (begin (x) x) x x)))
                 (ifsafe 5
                   `(lambda (x) (x) #t)
                   `(lambda (x) (,primref x x (begin (x) x) x x))))
               (equivalent-expansion?
                 (expand/optimize
                   `(lambda (x) (eqprim x x (begin (set! x 15) x) x x)))
                 `(lambda (x) (,primref x x (begin (set! x 15) x) x x)))
               )))]))
    #t)
  (eqtest eq? #t)
  (eqtest eqv? #t)
  (eqtest equal? #t)
  (eqtest bytevector=?)
  (eqtest enum-set=?)
  (eqtest bound-identifier=?)
  (eqtest free-identifier=?)
  (eqtest ftype-pointer=?)
  (eqtest literal-identifier=?)
  (eqtest time=?)
  (eqtest boolean=?)
  (eqtest symbol=?)
  (eqtest char=?)
  (eqtest char-ci=?)
  (eqtest string=?)
  (eqtest string-ci=?)
  (eqtest r6rs:char=?)
  (eqtest r6rs:char-ci=?)
  (eqtest r6rs:string=?)
  (eqtest r6rs:string-ci=?)
  (eqtest fx=)
  (eqtest fx=?)
  (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (equivalent-expansion?
      (expand/optimize
        `(lambda (x) (fl= x x))) ; x could be +nan.0
      `(lambda (x) (($primitive ,(if (fx= (optimize-level) 3) 3 2) fl=) x x))))
  (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (equivalent-expansion?
      (expand/optimize
        `(lambda (x) (= x x))) ; x could be +nan.0
      `(lambda (x) (($primitive ,(if (fx= (optimize-level) 3) 3 2) =) x x))))
)
