;;; misc.ms
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

;;; regression and other tests that don't fit somewhere more logical

(define-syntax biglet
  (lambda (x)
    (syntax-case x ()
      ((_ n bindings e)
       (let ((nv (datum n)))
         (if (= nv 0)
             (syntax (let bindings e))
             (with-syntax ((m (- nv 1)))
               (syntax (biglet m ((g n) . bindings) (+ g e))))))))))

(define-syntax biglambda
  (lambda (x)
    (syntax-case x ()
      ((_ n vars e)
       (let ((nv (datum n)))
         (if (= nv 0)
             (syntax (lambda vars e))
             (with-syntax ((m (- nv 1)))
               (syntax (biglambda m (g . vars) (+ g e))))))))))

(mat version
  (string? (scheme-version))
  (string? (scheme-version #f))
  (string? (scheme-version #t))
  (equal? (scheme-version) (scheme-version #f))
  (= 3 (length (call-with-values scheme-version-number list)))
  (let* ([alpha-s (format "-pre-release.~s" (scheme-pre-release))]
         [ends? (lambda (s)
                  (let ([len (string-length s)])
                    (and (> len (string-length alpha-s))
                         (equal? alpha-s (substring s (- len (string-length alpha-s)) len)))))])
    (and (if (scheme-pre-release)
             (ends? (scheme-version #t))
             (not (ends? (scheme-version #t))))
         (not (ends? (scheme-version))))))

(mat cycle
  (let ((x '#1=(a b . #1#)))
    (eqv? x x))
  (let-syntax ((a (lambda (y)
                    (let ((x (list 'quote '*)))
                      (set-car! (cdr x) x)
                      (datum->syntax (syntax a) x)))))
    (let ((a (a))) (and (pair? a) (eq? (cadr a) a))))
  (let-syntax ((a (lambda (y)
                      (let ((x (list 1 '*)))
                        (set-car! (cdr x) x)
                        (with-syntax ((l (datum->syntax (syntax a) x)))
                          (syntax (quote l)))))))
     (let ((a (a))) (and (pair? a) (eq? (car a) 1) (eq? (cadr a) a))))
;  (let ((x '(#2=(#2#) . #2#)))
;    (and (eq? (car x) (caar x)) (eq? (car x) (cdr x))))
)

(mat overflow ; attempt to force dooverflow, dooverflood, apply_dooverflood
    ;; this should test dooverflow
    (eqv? (let f ((n 100000))
             (if (= n 0)
                 0
                 (+ (f (- n 1)) 1)))
          100000)
    ;; this should test dooverflow
    (eqv? (let f ((n 10000) (m 0))
             (if (= n 0)
                 m
                 (f (call/cc (lambda (k) (- n 1)))
                    (call/cc (lambda (k) (+ (k (+ m 1)) 1))))))
          10000)
    ;; this should test dooverflood
    (eqv? (let f ((n 10000))
             (if (= n 0)
                 0
                 (let ((m (biglet 100 () 0)))
                    (+ m (f (- n 1))))))
          (* 10000 (let f ((n 100) (m 0)) (if (= n 0) m (f (- n 1) (+ m n))))))
    ;; this should test apply_dooverflood
    (= (length (apply list (make-list 100000))) 100000)
    ;; this should test apply_dooverflood
    (eqv? (let ((a (biglambda 100 () 0))
                (ls (make-list 100 1)))
             (let f ((n 10000))
                (if (= n 0)
                    0
                    (let ((m (apply a ls)))
                       (+ m (f (- n 1)))))))
          (* 100 10000))
    ; this should test overflow w/mrvs
    (let-syntax ((first (syntax-rules ()
                          ((_ e)
                           (call-with-values
                             (lambda () e)
                             (lambda (x . args) x))))))
      (eqv? (first (let f ((n 100000))
                     (if (fx= n 0)
                         (values 1 1)
                         (values (fx+ (first (f (fx- n 1))) 1) 1))))
            100001))
    ; test overflow w/lots of values to large frame
    (eqv? (let-syntax ((first (syntax-rules ()
                                ((_ e1 e2 ...)
                                 (call-with-values
                                   (lambda () e1 e2 ...)
                                   (lambda (x . args) x))))))
            (biglet 100 () (first (apply values (make-list 10000 0)))))
          5050)
    (eq?
      (let ()
        (define foo
          (lambda ()
            (define-syntax a
              (lambda (x)
                (syntax-case x ()
                  [(_ n)
                   (with-syntax ([(g ...) (generate-temporaries (make-list (datum n)))])
                     #'(let ([g 3] ...) (list g ...)))])))
            (a 1000)))
        (define (q n)
          (call/1cc
            (lambda (k0)
              ((call/1cc
                 (lambda (k1)
                   (call/1cc
                     (lambda (k2)
                       (k1 (lambda () (let f ([n n]) (foo) (unless (fx= n 0) (f (- n 1)))) (k2)))))
                   (k0 'done)))))))
        (q 1000))
      'done)
    ; regression test for np-place-overflow-and-trap treating test part of
    ; if-expr as tail when if-expr is tail
    (begin
      (define $poat-if-bug
        (lambda (x)
          (if (or (#3%fx= x 0) ($poat-if-bug (#3%fx- x 1)))
              'yes
              'no)))
      #t)
    (eq? ($poat-if-bug 20000) 'yes)
)

(begin
  (define ls0 '())
  (define ls1 '(a))
  (define ls2 '(a b))
  (define ls3 '(a b c))
  (define-syntax relop-length-test
    (lambda (x)
      (syntax-case x ()
        [(_ op)
         (with-syntax (((exp ...)
                        (map (lambda (ls)
                               (with-syntax ((ls ls)
                                             ((n ...) '(0 1 2 3 4 5)))
                                 #'(list (op (length ls) n) ...)))
                             (list #'ls0 #'ls1 #'ls2 #'ls3))))
           (with-syntax ((exp #'(list exp ...)))
             (with-syntax ((ans (datum->syntax #'* (interpret (datum exp)))))
               #'(equal? exp 'ans))))]))))

(mat relop-length ; test (relop (length e) n)
  (eqv? (pretty-print (expand (relop-length-test =))) (void))
  (relop-length-test <)
  (relop-length-test >)
  (relop-length-test <=)
  (relop-length-test >=)

  (relop-length-test fx=)
  (relop-length-test fx<)
  (relop-length-test fx>)
  (relop-length-test fx<=)
  (relop-length-test fx>=)
)

(mat compiler1
   (error? ; unbound variable
     (i-am-not-bound))
   (begin
     (define i-am-bound-but-not-to-a-procedure 'oops)
     #t)
   (error? ; non-procedure
     (i-am-bound-but-not-to-a-procedure))
   ;; test cpr1 code to avoid loading closer pointer for direct rec calls
   ;; make sure closure is loaded for value ref of g
   (letrec ((g (lambda (x)
                  (if (eq? x 'b)
                      (let ((h g)) (h 'c))
                      (if (eq? x 'a)
                          (g 'b)
                          'okay)))))
      (eq? (g 'a) 'okay))
   ;; make sure closure is loaded for closure containing g
   (letrec ((g (lambda (x)
                  (if (eq? x 'b)
                      (let ((h (lambda (x) (g x)))) (h 'c))
                      (if (eq? x 'a)
                          (g 'b)
                          'okay)))))
      (eq? (g 'a) 'okay))
   ;; test for incorrect call screwing up nocp code
   (error? (letrec ((g (lambda () (g (list))))) (g)))
   ;; test for rest list avoidance code being fooled by assignment conversion
   (begin
     (define (rest-test x . y)
       (set! y y)
       y)
     (equal?
       (rest-test 1 2)
       '(2)))
   ;; test for bogus conversion of direct lambda calls with rest arguments
   (equal? ((lambda x x) 1 2 3 4) '(1 2 3 4))
   ;; test for register allocator bug
   (let ()
     (define (foo return) (return 'foo))
     (define (goo return)
       (foo (lambda (y)
              (let ((x 'goo))
                (return x y '() '())))))
     (equal? (goo list) '(goo foo () ())))
   (let ()
     (define (foo return) (return 'foo))
     (define (goo return)
       (foo (lambda (y)
              (let ((x 'goo))
                (return x y 'hoo '() '())))))
     (equal? (goo list) '(goo foo hoo () ())))
   (eq? (let ((f (lambda x x))) ((begin 'a f))) '())
   (error? (letrec ((a (lambda (v) v))) ((begin 'foo a))))
   (equal? (let ((f (case-lambda ((x) 'a) ((x y) 'b) (z z))))
             ((begin 'c f) 3 4 5 6))
           '(3 4 5 6))
   (equal? (let ((f (lambda x x)))
             (call-with-values (lambda () ((begin 'a f))) list))
           '(()))
   (equal? (let ((f (lambda x x)))
             (call-with-values (lambda () ((begin 'a f)))
               (lambda args args)))
           '(()))
   (eqv?
     (let () ; mvlet in 5.0c & before were branching to domvleterr call
       (define id-var-name
         (lambda ()
           (define-syntax first
             (syntax-rules ()
               ((_ e) (#2%call-with-values
                        (lambda () e)
                        (lambda (x . ignore) x)))))
           (let ((f (lambda () (or (first (values #f 2)) 3))))
             (f))))
       (id-var-name))
     3)
   (begin (define string->color (lambda (x) (values 1 2))) (procedure? string->color))
   (eqv? (call-with-values
           (lambda () (string->color #f))
           (lambda (x y) x))
         1)
  ; test for cp2-store handling of binary dest with singleton next
   (procedure?
     (lambda (s end)
       (let ([end (or (if s end #f) end)])
         (if end s #f))))
  ; make sure case-lambda clause ordering is observed
   (equal?
     (let ((f (case-lambda
                [(x) (* x x)]
                [(x y) (+ x x)]
                [(x . r) (- x x)])))
       (list (f 5) (f 5 4) (f 5 4 3)))
     '(25 10 0))
   ; make sure irreducible flow graph doesn't choke the compiler
   (procedure?
     (rec q
       (case-lambda
         [() (q 0)]
         [(x) (q)])))
   ; regression tests for non-tail-call mref lvalue destination
   (begin
     (define (c1-f a)
       (let ([x (fxvector 0)])
         (lambda (v) (fxvector-set! x 0 (modulo v a)) x)))
     #t)
   (equal? ((c1-f 7) 10) #vfx(3))
   (begin
     (define (c1-id x) x)
     (define (c1-g x) (vector-set-fixnum! x 0 (c1-id 17)))
     #t)
   (equal? (let ([v (vector 3)]) (c1-g v) v) '#(17))
)

(mat compiler2 ; random tests
  (eqv? (((lambda (x) (lambda (y) (- x y))) 3) 4) -1)
  (equal? (let ((f (lambda (x) (lambda (y) (- x y)))))
            (cons ((f 3) 4) ((f 4) 3)))
    '(-1 . 1))
  (eqv? (letrec ((f (lambda (a) a))
                 (g (lambda (b) (if b (begin (f b) (g (not b))) 17))))
          (g #f))
    17)
  (eqv? (letrec ((f (lambda (a) a))
                 (g (lambda (b) (if b (begin (f b) (g (not b))) 13))))
          (g #t))
    13)
  (eqv? (letrec ((f (lambda (a) a))
                 (g (lambda (b) (if b (begin (f b) (g #f)) 11))))
          (g #f))
    11)
  (eqv? (letrec ((f (lambda (a) a))
                 (g (lambda (b) (if b (begin (f b) (g #f)) 9))))
          (g #t))
    9)
  (eqv? (let ((f (lambda (x) (+ x x))))
          (let ((g (lambda () f f)))
            (g) ((g) 3)))
    6)

  (eqv? (letrec ((f (lambda (x) (+ x x))))
          (letrec ((g (lambda () f f)))
            (g) ((g) 3)))
    6)
  (equal? (apply (lambda (x y) (list y x)) 'a 'b '()) '(b a))
  (equal? (apply (lambda (x . r) (list r x)) '(a b c)) '((b c) a))
  (equal? (apply list '(1 2 3)) '(1 2 3))
  (eqv? (apply + '(1 2 3)) 6)
  (let ([f (lambda x x)]) (equal? (f) '()))
  (eq? (let ()
         (define *current-gensym* 0)
         (define (generate-symbol)
           (set! *current-gensym* (+ *current-gensym* 1))
           (string->symbol (number->string *current-gensym*)))
         (define f (lambda (x) x))
         (f 3))
    3)
  (eqv? (let f ((x 0)) (if (= x 0) 1 (* x (f (- x 1))))) 1)
  (error? (let ((f (lambda () (let ((x 3)) (lambda (y z) (or (= y 3) x))))))
            (begin ((f) 3 (+ 'a 3))) 0))
  (eqv? (let ((f (lambda () (let ((x 3)) (lambda (y z) (or (= y 3) x))))))
          (begin ((f) 3 (+ 3 4)) 0))
    0)
  (let ((f (lambda () (lambda (y z) (or (= y 3) z))))) ((f) 3 (+ 3 4)))
  (let ((f (lambda () (lambda (y z) (or (= z 7) z))))) ((f) 3 (+ 3 4)))
  (let ((f (lambda (y z) (or (= y 3) z)))) (f 3 (+ 3 4)))
  (error? (let ((f (lambda (x) (+ x x)))) (f 3 4)))
  (error? ; invalid argument count in call to car
    (cons (car 1 2)))
  (error? ; invalid argument count in call to cons
    (let loop () (loop (cons 1 2 3))))
  (equal?
    (call/cc
      (lambda (k)
        (cons (k '(a b c)))))
    '(a b c))
  (equal?
    (call/cc
      (lambda (k)
        (let loop () (loop (k '(a b c))))))
    '(a b c))
  (equal?
    (call/cc
      (lambda (k)
        (letrec ([sum (lambda (n) (if (= n 0) 1 (+ n (sum (- n 1)))))])
          (cons (sum (k '(a . b)) 15)))))
    '(a . b))
  (equal?
    (call/cc
      (lambda (k)
        (letrec ([sum (lambda (n) (if (= n 0) 1 (+ n (sum (k '(a . b)) (- n 1)))))])
          (cons (sum 15)))))
    '(a . b))
  (equal?
    (call/cc
      (lambda (k)
        (letrec* ([a (lambda () c)]
                  [b (k "hi")]
                  [c (pair? k 1)])
          (errorf 'oops "shouldn't reach here ~s" (list a b)))))
    "hi")
  ; make sure we set up the stack properly before call-error
  (or (= (optimize-level) 3)
      (call/cc
        (lambda (k)
          (with-exception-handler
            (lambda (c) (collect) (k #t))
            (rec p (lambda () (('spam 1 2))))))))
  ; make sure return-address is set properly and stack is otherwise
  ; well-formed when we go through call-error for invalid consumer
  (begin
    (define ($foo$ x y z w p) w)
    #t)
  (or (= (optimize-level) 3)
      (call/cc
        (lambda (k)
          (with-exception-handler (lambda (c) (collect) (k #t))
            (lambda ()
              (let ([x (list (lambda () (sort < '(3 2 5 7 9)) (values 1 2 3)))])
                ($foo$ 1 2 3 4 5)
                (call-with-values (car x) x)))))))
  ; make sure return-address is set properly and stack is otherwise
  ; well-formed when we go through values-error
  (begin
    (define $values (lambda () (printf "hello!\n") (values 1 2 3 4 5 6 7 8)))
    #t)
  (or (= (optimize-level) 3)
      (eqv?
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (c) (collect) (k 'okay))
              (lambda () (if ($values) 3 4)))))
        'okay))
  (or (= (optimize-level) 3)
      (eqv?
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (c) (collect) (k 'okay))
              (lambda ()
                (let ([x (random 10)])
                  (if ($values) x 4))))))
        'okay))
  ; make sure return-address is set properly and stack is otherwise
  ; well-formed when we go through mvlet-error
  (or (= (optimize-level) 3)
      (eqv?
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (c) (collect) (k 'okay))
              (lambda ()
                (let ([x (random 10)])
                  (call-with-values $values
                    (lambda (x y) 'oops)))))))
        'okay))
  (or (= (optimize-level) 3)
      (eqv?
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (c) (collect) (k 'okay))
              (lambda ()
                (define f (case-lambda))
                (let ([x (random 10)])
                  (call-with-values $values f))))))
        'okay))
  (or (= (optimize-level) 3)
      (eqv?
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (c) (collect) (k 'okay))
              (lambda ()
                (let ([x (random 10)])
                  (call-with-values
                    (lambda () ($values) (values 1 2 3))
                    (lambda (x y) 'oops)))))))
        'okay))
  ; make sure compiler doesn't bomb trying to borrow a closure
  ; whose name isn't already free
  (equal?
    (let ([ls '()])
      (let ([v (((parameterize ([run-cp0 (lambda (cp0 x) x)])

                   (eval '(lambda (x y)
                            (let ((av (lambda () (x y))))
                              (av)
                              (lambda ()
                                (let ((tt (lambda () (x y))))
                                  (begin (tt) 3)))))))
                 (lambda (z) (set! ls (cons z ls)))
                 17))])
        (cons v ls)))
    '(3 17 17))
  ; for good measure, some where borrowing can occur
  ; tt borrow av
  (equal?
    (let ([ls '()])
      (let ([v (((parameterize ([run-cp0 (lambda (cp0 x) x)])
                   (eval '(lambda (x y)
                            (let ((av (lambda () (x y))))
                              (lambda ()
                                (av)
                                (let ((tt (lambda () (x y))))
                                  (begin (tt) 3)))))))
                 (lambda (z) (set! ls (cons z ls)))
                 17))])
        (cons v ls)))
    '(3 17 17))
  ; tt borrow av (which happens to be free in tt)
  (equal?
    (let ([ls '()])
      (let ([v (((parameterize ([run-cp0 (lambda (cp0 x) x)])

                   (eval '(lambda (x y)
                            (let ((av (lambda () (x y))))
                              (lambda ()
                                (let ((tt (lambda () (av) (x y))))
                                  (begin (tt) 3)))))))
                 (lambda (z) (set! ls (cons z ls)))
                 17))])
        (cons v ls)))
    '(3 17 17))
  ; tt borrow av, zz borrow av
  (equal?
    (let ([ls '()])
      (let ([v ((((parameterize ([run-cp0 (lambda (cp0 x) x)])

                    (eval '(lambda (x y)
                             (let ((av (lambda () (x y))))
                               (lambda ()
                                 (av)
                                 (let ((tt (lambda () (av) (x y))))
                                   (lambda ()
                                     (tt)
                                     (let ([zz (lambda () (x y))])
                                       (begin (zz) 3)))))))))
                  (lambda (z) (set! ls (cons z ls)))
                  17)))])
        (cons v ls)))
    '(3 17 17 17 17))
  ; tt borrow av, zz borrow av
  (equal?
    (let ([ls '()])
      (let ([v ((((parameterize ([run-cp0 (lambda (cp0 x) x)])
                    (eval '(lambda (x y)
                             (let ((av (lambda () (x y))))
                               (lambda ()
                                 (av)
                                 (let ((tt (lambda () (av) (x y))))
                                   (lambda ()
                                     (tt)
                                     (let ([zz (lambda () (x y))])
                                       (begin (zz) 3)))))))))
                  (lambda (z) (set! ls (cons z ls)))
                  17)))])
        (cons v ls)))
    '(3 17 17 17 17))
  ; zz borrow av (tt goes away)
  (equal?
    (let ([ls '()])
      (let ([v ((((parameterize ([run-cp0 (lambda (cp0 x) x)])
                    (eval '(lambda (x y)
                             (let ((av (lambda () (x y))))
                               (lambda ()
                                 (av)
                                 (let ((tt (lambda () (av) (x y))))
                                   (lambda ()
                                     (av)
                                     (let ([zz (lambda () (x y))])
                                       (begin (zz) 3)))))))))
                  (lambda (z) (set! ls (cons z ls)))
                  17)))])
        (cons v ls)))
    '(3 17 17 17))
  ; tt borrow av, zz borrow av
  (equal?
    (let ([ls '()])
      (let ([v ((((parameterize ([run-cp0 (lambda (cp0 x) x)])
                    (eval '(lambda (x y)
                             (let ((av (lambda () (x y))))
                               (lambda ()
                                 (av)
                                 (let ((tt (lambda () (av) (x y))))
                                   (lambda ()
                                     (tt)
                                     (av)
                                     (let ([zz (lambda () (x y))])
                                       (begin (zz) 3)))))))))
                  (lambda (z) (set! ls (cons z ls)))
                  17)))])
        (cons v ls)))
    '(3 17 17 17 17 17))
  ; tt borrow av, zz borrow av
  (equal?
    (let ([ls '()])
      (let ([v ((((parameterize ([run-cp0 (lambda (cp0 x) x)])
                    (eval '(lambda (x y)
                             (let ((av (lambda () (x y))))
                               (lambda ()
                                 (av)
                                 (let ((tt (lambda () (av) (x y))))
                                   (lambda ()
                                     (let ([zz (lambda () (tt) (x y))])
                                       (begin (zz) 3)))))))))
                  (lambda (z) (set! ls (cons z ls)))
                  17)))])
        (cons v ls)))
    '(3 17 17 17 17))
  ; tt borrow av, zz can't borrow
  (equal?
    (let ([ls '()])
      (let ([v ((((parameterize ([run-cp0 (lambda (cp0 x) x)])
                    (eval '(lambda (x y)
                             (let ((av (lambda () (x y))))
                               (lambda ()
                                 (av)
                                 (let ((tt (lambda () (av) (x y))))
                                   (tt)
                                   (lambda ()
                                     (let ([zz (lambda () (x y))])
                                       (begin (zz) 3)))))))))
                  (lambda (z) (set! ls (cons z ls)))
                  17)))])
        (cons v ls)))
    '(3 17 17 17 17))
  ; tt goes away, zz can't borrow
  (equal?
    (let ([ls '()])
      (let ([v ((((parameterize ([run-cp0 (lambda (cp0 x) x)])
                    (eval '(lambda (x y)
                             (let ((av (lambda () (x y))))
                               (lambda ()
                                 (av)
                                 (let ((tt (lambda () (av) (x y))))
                                   (lambda ()
                                     (let ([zz (lambda () (x y))])
                                       (begin (zz) 3)))))))))
                  (lambda (z) (set! ls (cons z ls)))
                  17)))])
        (cons v ls)))
    '(3 17 17))
  ; regression test for bug in which $flonum-exponent read past mapped memory
  (eq?
    (do ([n 2000 (- n 1)] [ls (iota 2000)])
        ((= n 0) 'fini)
      (map (lambda (x) (let ([x (exact (sqrt -2.0))]) x)) ls))
    'fini)
)

(mat compiler3
  ;; test cpr0 code to avoid bombing with compile-time error for apparent
  ;; arg count mismatch in direct call
  ;; need to add tests for mvcall and mvlet as well.
  (equal?
    (let ((ip (open-input-string "#f")))
      (let ((consumer (lambda (x) (list x))))
        (if (read ip) (consumer 1 2) (consumer 4))))
    '(4))
  ;; error message should come at run time, warning at compile time.
  (guard (c [(warning? c) #t])
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(let ([ip (open-input-string "#t")])
             (let ([consumer (lambda (x) (list x))])
               (if (read ip) (consumer 1 2) (consumer 4))))))
      'replace)
    (load "testfile.ss")
    #f)
  (error? ; incorrect argument count
    (load "testfile.ss"))
  (error?
    (let ((ip (open-input-string "#t")))
      (let ((consumer (lambda (x) (list x))))
        (if (read ip) (consumer 1 2) (consumer 4)))))
 ; test proper nonprocedure-procedure handling; goto is used as a symbol
 ; but not given a value in compiler boot file.  we had been failing to
 ; run retrofit_nonprocedure_procedure after loading the second (compiler)
 ; boot file.
  (begin
    (define $goto (lambda () (goto)))
    #t)
  (error? ($goto))
 ; check for nonprocedure-procedure handling when procedure is bound
 ; to something other than a procedure
  (error? (3 4))
  (error? ((cons 'a 'b) 4))
 ; check to make sure rest list is created after arguments are evaluated
  (begin
    (define non-eq-spines?
      (lambda (x)
        (let f ([ls1 (car x)] [ls2 (cdr x)])
          (if (null? ls1)
              (null? ls2)
              (and (not (eq? ls1 ls2))
                   (eq? (car ls1) (car ls2))
                   (f (cdr ls1) (cdr ls2)))))))
    #t)
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (define (f . args) args)
        (let ([ls (f (call/cc values) 1 2 3)]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((car ls1) (car ls1)))))
      (cons ls1 ls2)))
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (define (f a . args) (cons a args))
        (let ([ls (f (call/cc values) 1 2 3)]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((car ls1) (car ls1)))))
      (cons ls1 ls2)))
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (define (f . args) args)
        (let ([ls (f 1 (call/cc values) 2 3)]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((cadr ls1) (cadr ls1)))))
      (cons ls1 ls2)))
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (define (f a . args) (cons a args))
        (let ([ls (f 1 (call/cc values) 2 3)]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((cadr ls1) (cadr ls1)))))
      (cons ls1 ls2)))
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (define (f a . args) (cons a args))
        (let ([ls (f 1 2 (call/cc values) 3)]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((caddr ls1) (caddr ls1)))))
      (cons ls1 ls2)))
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (define (f . args) args)
        (let ([ls (f 1 2 3 (call/cc values))]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((cadddr ls1) (cadddr ls1)))))
      (cons ls1 ls2)))
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (define (f a . args) (cons a args))
        (let ([ls (f 1 2 3 (call/cc values))]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((cadddr ls1) (cadddr ls1)))))
      (cons ls1 ls2)))
  ; same thing, with direct lambda applications (should complete the set)
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (let ([ls ((lambda (a . args) (cons a args)) (call/cc values) 1 2 3)]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((car ls1) (car ls1)))))
      (cons ls1 ls2)))
  ; same thing, with let-values (should complete the set)
  (non-eq-spines?
    (let ()
      (define *k*)
      (define (f)
        (let ([ls (let-values ([(a . args) (values (call/cc values) 1 2 3)]) (cons a args))]) (*k* ls)))
      (define ls1 (call/cc (lambda (k) (set! *k* k) (f))))
      (define ls2 (call/cc (lambda (k) (set! *k* k) ((car ls1) (car ls1)))))
      (cons ls1 ls2)))
  ; make sure trivial cwv produces same code as let
  ((lambda (s1 s2)
     (call-with-port
       (open-string-input-port s1)
       (lambda (p1)
         (call-with-port
           (open-string-input-port s2)
           (lambda (p2)
             (let loop ()
               (if (eof-object? (get-line p1))
                   (eof-object? (get-line p2))
                   (and (not (eof-object? (get-line p2)))
                        (loop)))))))))
   (with-output-to-string
     (lambda ()
       (parameterize ([gensym-count 0] [print-gensym #f] [#%$assembly-output #t] [#%$suppress-primitive-inlining #f])
         (eval '(lambda (x) 
                  (let ()
                    (import scheme)
                    (call-with-values (lambda () (x)) (lambda (y) (x y)))))))))
   (with-output-to-string
     (lambda ()
       (parameterize ([gensym-count 0] [print-gensym #f] [#%$assembly-output #t])
         (eval '(lambda (x) (let ([y (x)]) (x y))))))))
 )

(mat compiler4
 ; check for overly loose loop recognition
  (eq? (let ([f (lambda (t)
                  ((letrec ([merge
                             (case-lambda [(t) (merge t t)] [(i t) 'yes])])
                     merge)
                   t))])
         (f 3))
       'yes)
  (eq? (let ([f (lambda (t)
                  (define merge (case-lambda [(t) (merge t t)] [(i t) 'yes]))
                  (merge t))])
         (f 3))
       'yes)
 ; original program from Bob Burger for overly loose loop recognition
  (equal?
    (let ()
      (define (consolidate T)
        (define merge
          (case-lambda
            [(T) (if (null? T) '() (merge (car T) (cdr T)))]
            [(I T)
             (if (null? T) (cons I '()) (merge I (car T) (cdr T)))]
            [(I J T)
             (let ([I-hi (cdr I)])
               (if (<= (car J) I-hi)
                   (let ([J-hi (cdr J)])
                     (if (<= J-hi I-hi)
                         (merge I T)
                         (merge (cons (car I) J-hi) T)))
                   (cons I (merge J T))))]))
        (merge T))
      (consolidate '((1 . 2) (2 . 5))))
    '((1 . 5)))
 )

(mat argcnt-check
   (eqv? (let ((f (lambda (x) #t))) (set! f (lambda (x y) x)) (f 1 2)) 1)
   (error? (let ((f (lambda (x) x))) (f 1 2)))
   (let ((f (case-lambda ((x) x) ((x y) #t)))) (f 1 2))
   (error? (let ((f (case-lambda ((x) x) ((x y) x)))) (f 1 2 3)))
   (let ((f (case-lambda ((x) x) ((x . y) #t)))) (f 1 2 3))
   (error? (let ((f (lambda (x y z . r) x))) (f)))
   (error? (let ((f (lambda (x y z . r) x))) (f 1)))
   (error? (let ((f (lambda (x y z . r) x))) (f 1 2)))
   (eqv? (let ((f (lambda (x y z . r) x))) (f 1 2 3)) 1)
   (eqv? (let ((f (lambda (x y z . r) x))) (f 1 2 3 4)) 1)
   (eqv? (let ((f (lambda (x y z . r) x))) (f 1 2 3 4 5)) 1)
   (let ((f (case-lambda ((x . r) x) ((x y . r) y)))) (f #t))
   (let ((f (case-lambda ((x y . r) y) ((x . r) x)))) (f #t))
   (error? (let f ((x 3)) (f)))
   (let f ((x #f)) (or x (f #t)))
   (let f ((x #f) (y #t)) (or x (f y x)))
   (error? (let f ((x #f) (y #t)) (or x (f #t))))
   (let ((f (or (lambda (x) x) (lambda (x y) x)))) (f #t))
   (error? (let ((f (or 3 (lambda (x) x)))) (f #t)))
   (guard (c [(equal? (condition-message c) "incorrect argument count in call ~a") #t]
             [else (raise c)])
     (let loop ([x 1])
       (if (fx= x 0)
           x
           (loop)))
     #f)
   (begin
     (with-output-to-file "testfile-argcnt-check-loop.ss"
       (lambda ()
         (pretty-print
           '(let loop ([x 1])
              (if (fx= x 0)
                  x
                  (loop)))))
       'replace)
     #t)
   (guard (c [(equal? (condition-message c) "possible incorrect argument count in call ~a") #t]
             [else #f])
     (load "testfile-argcnt-check-loop.ss")
     #f)
   (guard (c [(equal? (condition-message c) "possible incorrect argument count in call ~a") #t]
             [else #f])
     (compile-library "testfile-argcnt-check-loop.ss")
     #f)
   (begin
     (define foo
       (lambda ()
         (let loop ([x 1])
           (if (fx= x 0)
               x
               (loop)))))
     #t)
   (guard (c [(equal? (condition-message c) "incorrect argument count in call ~a") #t]
             [else (raise c)])
     (foo)
     #f)
   (begin
     (with-output-to-file "testfile-argcnt-check-foo.ss"
       (lambda ()
         (pretty-print
           '(define foo
              (lambda ()
                (let loop ([x 1])
                  (if (fx= x 0)
                      x
                      (loop)))))))
       'replace)
     #t)
   (guard (c [(equal? (condition-message c) "possible incorrect argument count in call ~a") #t]
             [else #f])
     (load "testfile-argcnt-check-foo.ss"))
   (guard (c [(equal? (condition-message c) "possible incorrect argument count in call ~a") #t]
             [else #f])
     (compile-library "testfile-argcnt-check-foo.ss"))
   (begin
     (library (argcnt-check-r)
       (export foo)
       (import (chezscheme))
       (define foo
         (lambda ()
           (let f ([x 1])
             (if (fx= x 0)
                 x
                 (list (f)))))))
     #t)
   (guard (c [(equal? (condition-message c) "incorrect argument count in call ~a") #t]
             [else (raise c)])
     (let ()
       (import (argcnt-check-r))
       (foo)
       #f))
   (begin
     (library (argcnt-check-s)
       (export foo foo1 foo2)
       (import (chezscheme))
       (define foo
         (lambda ()
           (let loop ([x 1])
             (if (fx= x 0)
                 x
                 (loop)))))
       (define foo1 (lambda () (foo) (foo) (foo) (foo) (foo)))
       (define foo2 (lambda () (foo))))
     #t)
   (guard (c [(equal? (condition-message c) "incorrect argument count in call ~a") #t]
             [else (raise c)])
     (let ()
       (import (argcnt-check-s))
       (foo)
       #f))
   (guard (c [(equal? (condition-message c) "incorrect argument count in call ~a") #t]
             [else (raise c)])
     (let ()
       (import (argcnt-check-s))
       (foo1)
       #f))
   (guard (c [(equal? (condition-message c) "incorrect argument count in call ~a") #t]
             [else (raise c)])
     (let ()
       (import (argcnt-check-s))
       (foo2)
       #f))
   (begin
     (with-output-to-file "testfile-argcnt-check-s.ss"
       (lambda ()
         (pretty-print
           '(library (testfile-argcnt-check-s)
              (export foo)
              (import (chezscheme))
              (define foo
                (lambda ()
                  (let loop ([x 1])
                    (if (fx= x 0)
                        x
                        (loop))))))))
       'replace)
     #t)
   (guard (c [(equal? (condition-message c) "possible incorrect argument count in call ~a") #t]
             [else (raise c)])
     (eval '(import (testfile-argcnt-check-s)))
     #f)
   (guard (c [(equal? (condition-message c) "possible incorrect argument count in call ~a") #t]
             [else (raise c)])
     (load "testfile-argcnt-check-s.ss")
     #f)
   (guard (c [(equal? (condition-message c) "possible incorrect argument count in call ~a") #t]
             [else (raise c)])
     (compile-library "testfile-argcnt-check-s.ss")
     #f)
)

(mat direct-call
   (let ()
      (define f (let ((x 3)) (lambda (y) (+ x y))))
      (define g (lambda () (f 4)))
      (eq? (g) 7))
)

(mat inspect ; need lots more
  (eq? ((call/cc inspect/object) 'type) 'continuation)
  (eq? ((call/1cc inspect/object) 'type) 'continuation)
  (integer? ((call/cc inspect/object) 'depth))
  (integer? ((call/1cc inspect/object) 'depth))
  (error? ((inspect/object '#(1)) 'ref))
  (or (equal? (current-eval) interpret)
      (let ()
        (define $f (lambda (x) (let ([o (call/cc inspect/object)]) (cons x o))))
        (let ([q ($f (cons 'a 'b))])
          (eq? ((cdr q) 'eval 'x) (car q)))))
  (error? ; invalid message
    ((inspect/object (cons 'car 'cdr)) 'creep))
  (error? ; incorrect number of arguments
    ((inspect/object (cons 'car 'cdr)) 'size))
  (error? ; invalid generation
    ((inspect/object (cons 'car 'cdr)) 'size 'oops))
  (<= ((inspect/object (cons 'car 'cdr)) 'size 0) (fx* (ftype-sizeof uptr) 2))
  (eqv? ((inspect/object (cons 0 0)) 'size 'static) (fx* (ftype-sizeof uptr) 2))
  (equal?
    (let ([ls (list 0 0)])
      (set-cdr! (cdr ls) ls)
      (let ([x (inspect/object ls)])
        (let* ([size1 (x 'size 'static)] [size2 ((x 'cdr) 'size 'static)])
          (cons size1 size2))))
    (cons
      (fx* (ftype-sizeof uptr) 4)
      (fx* (ftype-sizeof uptr) 2)))
)

(mat compute-size
  (error? (compute-size 0 -1))
  (error? (compute-size 0 'dynamic))
  (eqv? (compute-size 0) 0)
  (eqv? (compute-size (cons 0 0)) (fx* (ftype-sizeof uptr) 2))
  (eqv? (compute-size 'cons) 0)
  ; from the user's guide
  (eqv?
    (compute-size 0)
    0)
  (eqv?
    (compute-size (cons 0 0))
    (* (ftype-sizeof uptr) 2))
  (eqv?
    (compute-size (cons (vector #t #f) 0))
    (* (ftype-sizeof uptr) 6))
  (eqv?
    (compute-size
      (let ([x (cons 0 0)])
        (set-car! x x)
        (set-cdr! x x)
        x))
    (* (ftype-sizeof uptr) 2))
  (>=
    (let ()
      (define-record-type frob (fields x))
      (compute-size
        (let ([x (make-frob 0)])
          (cons x x))))
    (* (ftype-sizeof uptr) 16))
  (eqv?
    (parameterize ([collect-request-handler void])
      (let ()
        (define-record-type frob (fields x))
        (collect 1 1)
        (compute-size
          (let ([x (make-frob 0)])
            (cons x x))
          0)))
    (* (ftype-sizeof uptr) 4))
  ; make sure we don't venture into the undefined fields of a shot 1-shot continuation
  (fixnum? (let ([k (call/1cc (lambda (k) k))]) (collect) (compute-size k)))
)

(mat compute-size-increments
  (error? (compute-size-increments 'not-a-list))
  (error? (compute-size-increments 0))
  (error? (compute-size-increments (list 0) -1))
  (error? (compute-size-increments (list 0) "static"))
  (error? (compute-size-increments (list 0) '()))
  (begin
    (define pair-size (compute-size (cons 1 2)))
    (define ephemeron-size (compute-size (ephemeron-cons 1 2)))
    #t)
  (equal? (list pair-size pair-size)
          (compute-size-increments (list (cons 1 2) (cons 3 4))))
  (equal? (list pair-size 0)
          (compute-size-increments (let ([p (cons 1 2)])
                                     (list p p))))
  (equal? (list (* 3 pair-size) pair-size)
          (let ([l (list 1 2)])
            (compute-size-increments (list (cons 3 l) (cons 4 l)))))
  (equal? (list pair-size)
          (compute-size-increments (list (weak-cons (make-bytevector 100) #f))))
  (let* ([x (make-bytevector 100)]
         [ls (list (lambda () x) x)])
    (equal? (compute-size-increments ls)
            (reverse (compute-size-increments (reverse ls)))))
  ;; Ephemeron(s) found before key:
  (equal? (list ephemeron-size (* 2 pair-size))
          (compute-size-increments (let* ([p (cons 0 0)]
                                          [e (ephemeron-cons p (cons 0 0))])
                                     (list e p))))
  (equal? (list ephemeron-size (* 3 pair-size))
          (let* ([v (cons 1 2)]
                 [e (ephemeron-cons v (cons 3 4))])
            (compute-size-increments (list e (cons v #f)))))
  (equal? (list (* 2 (+ ephemeron-size pair-size)) (* 4 pair-size))
          (let* ([v (cons 1 2)]
                 [e* (list (ephemeron-cons v (cons 3 4))
                           (ephemeron-cons v (cons 5 6)))])
            (compute-size-increments (list e* (cons v #f)))))
  ;; Key found before ephemeron(s):
  (equal? (list (* 2 pair-size) (+ ephemeron-size pair-size))
          (let* ([v (cons 1 2)]
                 [e (ephemeron-cons v (cons 3 4))])
            (compute-size-increments (list (cons v #f) e))))
  (equal? (list (* 2 pair-size) (+ (* 4 pair-size) (* 2 ephemeron-size)))
          (let* ([v (cons 1 2)]
                 [e* (list (ephemeron-cons v (cons 3 4))
                           (ephemeron-cons v (cons 5 6)))])
            (compute-size-increments (list (cons v #f) e*))))
  ;; This call will encounter many kinds of objects, just to make
  ;; sure it doesn't fail:
  (list? (compute-size-increments (list (call/cc values)) 'static))
  ;; Check that a deactivated thread's continuation can be traversed
  ;; for `compute-size-increments`:
  (or (not (threaded?))
      (let* ([ready (box #f)]
             [saved (box #f)]
             [m (make-mutex)]
             [N 1000000]
             [pause-until (lambda (check)
                            (let loop ()
                              (unless (check)
                                (sleep (make-time 'time-duration 10000 0))
                                (loop))))]
             ;; to make sure a thread parameter's content is not counted:
             [new-param (make-thread-parameter (make-bytevector (* 3 N)))])
        (mutex-acquire m)
        (let* ([th-code
                ;; we'll measure the difference between data reachable by this
                ;; code versus data reachable from the thread
                (lambda ()
                  (let ([bstr (make-bytevector N)])
                    (set-box! ready 'go)
                    ;; Block so that thread becomes deactivated
                    (mutex-acquire m)
                    (mutex-release m)
                    ;; bstr is retained in the thread's continuation until here
                    (set-box! saved (bytevector-u8-ref bstr 0))
                    (pause-until (lambda () (box-cas! ready 'finish 'done)))
                    ;; Block so that thread becomes deactivated, again
                    (mutex-acquire m)
                    (mutex-release m)))]
               [th (fork-thread th-code)])
          ;; Wait for thread to start
          (pause-until (lambda () (eq? 'go (unbox ready))))
          ;; Wait for thread to become inactive, blocked on the mutex
          (pause-until (lambda () (= 1 (#%$top-level-value '$active-threads))))
          ;; Get thread's size, which should include bstr
          (let ([pre-sizes (compute-size-increments (list th-code
                                                          th
                                                          ;; don't count parameters towards the thread's
                                                          ;; size; in particular, the `$tt-fat` parameter
                                                          ;; from a "thread.ms" test can have random symbols,
                                                          ;; some of which may be bound to large values
                                                          (#%$tc-field 'parameters (#%$thread-tc th))))])
            (mutex-release m)
            ;; Wait for bytevector to be discarded in the thread
            (pause-until (lambda () (unbox saved)))
            (mutex-acquire m)
            (set-box! ready 'finish)
            ;; Wait for thread to become inactive again
            (pause-until (lambda () (eq? 'done (unbox ready))))
            (pause-until (lambda () (= 1 (#%$top-level-value '$active-threads))))
            ;; Get thread's size, which shouldn't include bstr
            (let ([post-sizes (compute-size-increments (list th-code
                                                             th
                                                             (#%$tc-field 'parameters (#%$thread-tc th))))])
              (printf "SIZES ~s\n" (list pre-sizes N post-sizes))
              (mutex-release m)
              ;; Wait for thread to exit
              (thread-join th)
              ;; Make sure `compute-size-increments` doesn't crash on a
              ;; terminated thread:
              (compute-size-increments (list th))
              ;; drop extra bytevector from the current thread:
              (new-param #f)
              ;; Main result: detected size of `bstr` in the thread
              ;; while it was part of the continuation
              (or (eq? (current-eval) interpret) ; interpreter continuation is not precise enough
                  (and (> (cadr pre-sizes) N)
                       (< (cadr post-sizes) N))
                  (errorf 'compute-size-increments "failed ~s ~s ~s" pre-sizes N post-sizes)))))))
  )

(mat collect+compute-size-increments
  (eq? (void) (collect 0 0 0 #f))
  (eq? '() (collect 0 0 0 '()))

  (error? (collect 0 0 0 'not-a-list))
  (error? (collect 0 0 0 0))
  (error? (collect 'not-a-generation 0 0 '()))
  (error? (collect 0 'not-a-generation 0 '()))
  (error? (collect 0 0 'not-a-generation '()))
  (error? (collect 1 0 0 '()))
  
  (begin
    (define-record-type count-wrap (fields val))
    (collect 0 0 0 (list (make-count-wrap 0))) ; take care of one-time initialization costs
    (define wrap-size (car (collect 0 0 0 (list (make-count-wrap 0))))) ; includes rtd
    (define just-wrap-size (cadr (collect 0 0 0 (list (make-count-wrap 0) (make-count-wrap 1)))))
    (define pair-size (compute-size (cons 1 2)))
    (define ephemeron-size (compute-size (ephemeron-cons 1 2)))
    #t)
  (equal? (list pair-size pair-size)
          (collect 0 0 0 (list (cons 1 2) (cons 3 4))))
  (equal? (list pair-size 0)
          (collect 0 0 0 (let ([p (cons 1 1)])
                           (list p p))))
  (equal? (list (* 3 pair-size) pair-size)
          (let ([l (list 1 2)])
            (collect 0 0 0 (list (cons 3 l) (cons 4 l)))))
  (equal? (list pair-size)
          (collect 0 0 0 (list (weak-cons (make-bytevector 100) #f))))
  ;; Ephemeron(s) found before key:
  (equal? (list ephemeron-size (+ (* 2 pair-size) wrap-size))
          (collect 0 0 0 (let* ([p (make-count-wrap (cons 0 0))]
                               [e (ephemeron-cons p (cons 0 0))])
                           (list e p))))
  (equal? (list ephemeron-size (+ (* 3 pair-size) wrap-size))
          (let* ([v (make-count-wrap (cons 1 2))]
                 [e (ephemeron-cons v (cons 3 4))])
            (collect 0 0 0 (list e (cons v #f)))))
  (equal? (list (* 2 (+ ephemeron-size pair-size)) (+ (* 4 pair-size) wrap-size))
          (let* ([v (make-count-wrap (cons 1 2))]
                 [e* (list (ephemeron-cons v (cons 3 4))
                           (ephemeron-cons v (cons 5 6)))])
            (collect 0 0 0 (list e* (cons v #f)))))
  ;; Key found before ephemeron(s):
  (equal? (list (+ (* 2 pair-size) wrap-size) (+ ephemeron-size pair-size))
          (let* ([v (make-count-wrap (cons 1 2))]
                 [e (ephemeron-cons v (cons 3 4))])
            (collect 0 0 0 (list (cons v #f) e))))
  (equal? (list (* 2 pair-size) (+ (* 4 pair-size) (* 2 ephemeron-size)))
          (let* ([v (cons 1 2)]
                 [e* (list (ephemeron-cons v (cons 3 4))
                           (ephemeron-cons v (cons 5 6)))])
            (collect 0 0 0 (list (cons v #f) e*))))
  ;; Weakly held objects:
  (equal? '(0)
          (let* ([v (make-count-wrap (cons 1 2))]
                 [ls (weak-cons v '())])
            (collect 0 0 0 ls)))
  (equal? (list wrap-size pair-size (+ just-wrap-size pair-size))
          (let* ([v (make-count-wrap (cons 1 2))]
                 [ls (cons* (make-count-wrap 0) (cons v 1) (weak-cons v '()))])
            (collect 0 0 0 ls)))
  (equal? (list 0 (+ wrap-size (* 2 pair-size)))
          (let* ([v (make-count-wrap (cons 1 2))]
                 [ls (weak-cons v (cons (cons v 1) '()))])
            (collect 0 0 0 ls)))
  (equal? #!bwp
          (let* ([v (make-count-wrap (cons 1 2))]
                 [ls (weak-cons v '())])
            (collect 0 0 0 ls)
            (car ls)))
  ;; These calls will encounter many kinds of objects, just to make
  ;; sure they don't fail:
  (list? (collect 0 0 0 (list (call/cc values))))
  (list? (collect (collect-maximum-generation) (collect-maximum-generation) (collect-maximum-generation) (list (call/cc values))))

  (let ()
    (define e (ephemeron-cons #t (gensym)))
    (collect 0 1)
    (let ([g (gensym)])
      (set-car! e g)
      (set! g #f)
      ;; For this collection, `e` is both on the dirty list
      ;; and involved in measuring; make sure those roles
      ;; don't conflict
      (collect 1 1 1 (list e))
      (equal? e (cons #!bwp #!bwp))))

  (let ()
    (define e (ephemeron-cons #t 'other))
    (collect 0 1)
    (let ([g (gensym)])
      (set-car! e g)
      (collect 1 1 1 (list e))
      (equal? e (cons g 'other))))
)

(mat compute-composition
  (error? (compute-composition 0 -1))
  (error? (compute-composition 0 "static"))
  (equal? (compute-composition 0) '())
  (equal?
    (sort (lambda (x y) (fx> (cadr x) (cadr y)))
      (compute-composition (cons (fxvector 1) (vector (fxvector 2) (fxvector 3) (list (fxvector 4))))))
    `((fxvector . (4 . ,(fx* 4 (ftype-sizeof uptr) 2))) (pair . (2 . ,(fx* 2 (ftype-sizeof uptr) 2))) (vector . (1 . ,(fx* 4 (ftype-sizeof uptr))))))
  (equal? (compute-composition 'cons) '())
  ; from the user's guide
  (begin
    (define $same-elements?
      (lambda (ls1 ls2)
        (and (equal? (length ls1) (length ls2))
             (let f ([ls1 ls1])
               (or (null? ls1)
                   (and (member (car ls1) ls2)
                        (f (cdr ls1))))))))
    #t)
  (equal?
    (compute-composition 0)
    '())
  ($same-elements?
    (compute-composition (cons 0 0))
    `((pair 1 . ,(* (ftype-sizeof uptr) 2))))
  (equal?
    (compute-composition (cons (vector #t #f) 0))
    `((pair 1 . ,(* (ftype-sizeof uptr) 2))
      (vector 1 . ,(* (ftype-sizeof uptr) 4))))
  (equal?
    (compute-composition
      (let ([x (cons 0 0)])
        (set-car! x x)
        (set-cdr! x x)
        x))
    `((pair 1 . ,(* (ftype-sizeof uptr) 2))))
  (>=
    (let ()
      (define-record-type frob (fields x))
      (length
        (compute-composition
          (let ([x (make-frob 0)])
            (cons x x)))))
    4) ; pair, rtd, record, fields vector, name
  (let ()
    (define-record-type frob (fields x))
    ($same-elements?
      (parameterize ([collect-request-handler void])
        (let ()
          (collect 1 1)
          (compute-composition
            (let ([x (make-frob 0)])
              (cons x x))
            0)))
      `((pair 1 . ,(* (ftype-sizeof uptr) 2))
        (,(record-type-descriptor frob) 1 . ,(* (ftype-sizeof uptr) 2)))))
  ; make sure we don't venture into the undefined fields of a shot 1-shot continuation
  (list? (let ([k (call/1cc (lambda (k) k))]) (collect) (compute-composition k)))
)

(mat make-object-finder
  (begin
    (define $fo
      (lambda args
        (let ([find-next (apply make-object-finder args)])
          (cond
            [(find-next) =>
             (lambda (path)
               (unless (list? path)
                 (errorf '$fo-all "~s is not a list" path))
               path)]
            [else #f]))))
    (define $fo-all
      (lambda args
        (let ([find-next (apply make-object-finder args)])
          (let f ()
            (cond
              [(find-next) =>
               (lambda (path)
                 (unless (list? path)
                   (errorf '$fo-all "~s is not a list" path))
                 (cons path (f)))]
              [else '()])))))
    (define set-equal?
      (lambda (s1 s2)
        (and (= (length s1) (length s2))
             (andmap (lambda (x) (member x s2)) s1)
             #t)))
    #t)
  (error? ; not a procedure
    (make-object-finder 17))
  (error? ; invalid generation
    (make-object-finder not 'q (+ (collect-maximum-generation) 1)))
  (error? ; invalid generation
    (make-object-finder not 'q 'oldgen))
  (error? ; invalid generation
    (make-object-finder not 'q -1))
  (error? ; invalid number of arguments
    ((make-object-finder fixnum? 1) 'a))
  (not ($fo (let ([ctr 0]) (lambda (x) (set! ctr (+ ctr 1)) (when (= (mod ctr 4000) 0) (pretty-print ctr)) #f))))
  (pair? ($fo symbol?))
  (not ($fo symbol? (list 1 2 3)))
  (equal?
    ($fo symbol? (list 1 'a-symbol-probably-not-static 3))
    '(a-symbol-probably-not-static (a-symbol-probably-not-static 3) (1 a-symbol-probably-not-static 3)))
  (equal?
    ($fo symbol? (list 1 'a 3))
    '(a (a 3) (1 a 3)))
  (equal?
    ($fo symbol? (list 'a-symbol-probably-not-static 2 3))
    '(a-symbol-probably-not-static (a-symbol-probably-not-static 2 3)))
  (equal?
    ($fo symbol? (list 'a 2 3))
    '(a (a 2 3)))
  (equal?
    ($fo flonum? (list 1 3.14 3))
    '(3.14 (3.14 3) (1 3.14 3)))
  (not ($fo symbol? (vector 1 2 3)))
  (equal?
    ($fo symbol? (vector 1 'a-symbol-probably-not-static 3))
    '(a-symbol-probably-not-static #(1 a-symbol-probably-not-static 3)))
  (equal?
    ($fo flonum? (vector 1 3.14 3))
    '(3.14 #(1 3.14 3)))
  (equal?
    ($fo fixnum? (vector 1 'a-symbol-probably-not-static 3))
    '(1 #(1 a-symbol-probably-not-static 3)))
  (equal?
    ($fo-all fixnum? 1)
    '((1)))
  (set-equal?
    ($fo-all fixnum? (vector 1 'a-symbol-probably-not-static 3))
    '((1 #(1 a-symbol-probably-not-static 3)) (3 #(1 a-symbol-probably-not-static 3))))
  (set-equal?
    ($fo-all fixnum? (list 1 'a-symbol-probably-not-static 3))
    '((1 (1 a-symbol-probably-not-static 3)) (3 (3) (a-symbol-probably-not-static 3) (1 a-symbol-probably-not-static 3))))
  (let-values ([(g path*) (parameterize ([generate-inspector-information #f]
                                         [compile-profile #f]
                                         [current-eval compile]
                                         [enable-cp0 #f])
                            (eval `(let ()
                                     (define f (lambda (x) (lambda (y) (cons x '#(4 5)))))
                                     (define g (f '#(a b)))
                                     (values g ($fo-all vector? g)))))])
    (set-equal?
      path*
      `((#(4 5) ,(#%$closure-code g) ,g)
        (#(a b) ,g))))
  (not ($fo (lambda (x) (and (string? x) (string=? x "cons"))) 'cons 0))
  (list? ($fo (lambda (x) (and (string? x) (string=? x "cons"))) 'cons 'static))
  ; make sure we don't venture into the undefined fields of a shot 1-shot continuation
  (not (let ([k (call/1cc (lambda (k) k))]) (collect) ($fo (lambda (x) #f) k)))
)

(mat print-vector-length
    (not (print-vector-length))
    (let ([p (open-output-string)])
       (write '#(1 2 3) p)
       (string=? (get-output-string p) "#(1 2 3)"))
    (let ([p (open-output-string)])
       (parameterize ([print-vector-length #t])
          (write '#(1 2 3) p))
       (string=? (get-output-string p) "#3(1 2 3)"))
    )

(mat print-brackets
    (print-brackets)
    (let ([p (open-output-string)])
       (pretty-print '(let ([x x]) x) p)
       (string=? (get-output-string p) (format "(let ([x x]) x)~%")))
    (let ([p (open-output-string)])
       (parameterize ([print-brackets #f])
          (pretty-print '(let ([x x]) x) p))
       (string=? (get-output-string p) (format "(let ((x x)) x)~%")))
    )

(mat subset
  (not (subset-mode))
  (error? (subset-mode 'ieee))
  (error? (subset-mode 'r4rs))
  (error? (subset-mode 'r5rs))
  (error? (subset-mode #t))
  (begin (subset-mode #f) (not (subset-mode)))
)

(mat eval
  (eq? (eval '(let ((x 3)) x)) 3)
  (eq? (eval '(let ((x 3)) x) (interaction-environment)) 3)
  (eq? (eval '(let ((x 3)) x) (scheme-report-environment 5)) 3)
  (eq? (eval '(let ((x 3)) x) (ieee-environment)) 3)
  (eq? (eval '(let ((x 3)) x) (null-environment 5)) 3)

  (eq? (eval '(< 3 5) (scheme-report-environment 5)) #true)
  (eq? (eval '(< 3 5) (ieee-environment)) #true)

  (eq? (eval '(let ((p (delay 3))) (force p))) 3)
  (eq? (eval '(let ((p (delay 3))) (force p)) (interaction-environment)) 3)
  (eq? (eval '(let ((p (delay 3))) (force p)) (scheme-report-environment 5)) 3)
  (error? (eval '(let ((p (delay 3))) (force p)) (null-environment 5)))
  (error? (eval '(let ((p (delay 3))) (force p)) (ieee-environment)))

  (error? (eval '(cons 1 2) (null-environment 5)))
  (error? (eval '(sort < '(3 2 4)) (scheme-report-environment 5)))
  (error? (eval '(sort < '(3 2 4)) (ieee-environment)))
  (error? (eval '(sort < '(3 2 4)) (null-environment 5)))

  (error? (eval '(< 3) (scheme-report-environment 5)))
  (error? (eval '(< 3) (ieee-environment)))

)

(mat eval2
  (eq? (eval '(let ((x 3)) x)) 3)
  (eq? (eval '(let ((x 3)) x) (interaction-environment)) 3)
  (eq? (eval '(let ((x 3)) x) (scheme-report-environment 5)) 3)
  (eq? (eval '(let ((x 3)) x) (null-environment 5)) 3)
  (eq? (eval '(let ((x 3)) x) (ieee-environment)) 3)

  (eq? (eval 'list) list)
  (eq? (eval 'list (interaction-environment)) list)
  (eq? (eval 'list (scheme-report-environment 5)) list)
  (error? (eval 'list (null-environment 5)))
  (eq? (eval 'list (ieee-environment)) list)

  (eq? (eval 'force) force)
  (eq? (eval 'force (interaction-environment)) force)
  (eq? (eval 'force (scheme-report-environment 5)) force)
  (error? (eval 'force (null-environment 5)))
  (error? (eval 'force (ieee-environment)))

  (eq? (force (eval '(delay 17))) 17)
  (eq? (force (eval '(delay 17) (interaction-environment))) 17)
  (eq? (force (eval '(delay 17) (scheme-report-environment 5))) 17)
  (eq? (force (eval '(delay 17) (null-environment 5))) 17)
  (error? (eval '(delay 17) (ieee-environment)))

  (error? (eval '(set! + -) (scheme-report-environment 5)))
  (error? (eval '(set! + -) (null-environment 5)))
  (error? (eval '(set! + -) (ieee-environment)))

  (error? (eval '(define x -) (scheme-report-environment 5)))
  (error? (eval '(define x -) (null-environment 5)))
  (error? (eval '(define x -) (ieee-environment)))

  (error? (eval '(define-syntax x list) (scheme-report-environment 5)))
  (error? (eval '(define-syntax x list) (null-environment 5)))
  (error? (eval '(define-syntax x list) (ieee-environment)))
  (error? (eval '(define-syntax x (syntax-rules () ((_) 4)))
                (ieee-environment)))

  (eq? (eval '(syntax-case 3 () (_ 4))) 4)
  (eq? (eval '(syntax-case 3 () (_ 4)) (interaction-environment)) 4)
  (error? (eval '(syntax-case 3 () (_ 4)) (scheme-report-environment 5)))
  (error? (eval '(syntax-case 3 () (_ 4)) (null-environment 5)))
  (error? (eval '(syntax-case 3 () (_ 4)) (ieee-environment)))
)

(mat getenv/putenv
  (procedure? getenv)
  (procedure? putenv)
  (or (embedded?)
      (string? (or (getenv "HOME") (getenv "HOMEPATH"))))
  (not (getenv "FUBULYFRATZ"))
  (eq? (putenv "FUBULY" "FRATZ") (void))
  (not (getenv "FUBULYFRATZ"))
  (equal? (getenv "FUBULY") "FRATZ")
  (eq? (putenv "FUBULY" "fratz") (void))
  (equal? (getenv "FUBULY") "fratz")
  (error? (getenv 'hello))
  (error? (putenv 'hello "goodbye"))
  (error? (putenv "hello" 'goodbye))
 )

(mat source-directories
  (equal? (separate-eval '(source-directories)) "(\".\")\n")
  (equal? (parameterize ((source-directories (list "/a" ".")))
            (source-directories))
          '("/a" "."))
  (error? (source-directories 'a))
  (error? (source-directories "a"))
  (error? (source-directories '("a" . "b")))
  (error? (source-directories '(3)))
  (error? ; invalid exports list---not "testfile.ss not found in source directories"
    (begin
      (with-output-to-file "testfile.ss"
        (lambda () (pretty-print '(module (a 3) (define a 3))))
        'replace)
      (parameterize ([source-directories '("." "probably not there")])
        (load "testfile.ss"))))
)

(mat queries
  (boolean? (threaded?))
  (boolean? (petite?))
  (let ([pid (get-process-id)])
    (and (integer? pid) (exact? pid)))
  (eqv? (get-thread-id) 0)
  (eqv? (get-process-id) (get-process-id))
  (eqv? (get-thread-id) (get-thread-id))
)

(mat cpletrec
  (eq? (letrec ((x 3)) x) 3)
  (eq? (letrec ((x 3)) 4) 4)
  (eq? (letrec ((x (let ((y 4)) (lambda (x) (+ x y))))) (x 7)) 11)
  (eq? (letrec ((x (letrec ((y 4)) (lambda (x) (+ x y))))) (x 7)) 11)
  (eq? (letrec ((x 4)) (set! x 3)) (void))
  (eq? (letrec ((x 4)) (set! x (begin (write 'hi) 3))) (void))
  (eq? (letrec ((x (letrec ((y (lambda (z) (+ z z))))
                     (lambda (x) (y x)))))
         (x 3))
       6)
  (equal? (letrec ((foo (rec f (lambda (x ls) (list x ls))))) (foo 1 2))
    '(1 2))
  (eq? (letrec ((x (let ((a (+ 3 4))) (let ((b (+ a a))) b)))) x) 14)
  (eq? (letrec ((x (let ((a (lambda (x) (+ x 1))))
                     (let ((b (lambda (y) (+ (a y) y))))
                       (lambda (z) (* (b z) z))))))
         (x 3))
       21)
  (equal?
    (let ()
      (define next
        (let ((cnt 0))
          (lambda () (set! cnt (+ cnt 1)) cnt)))
      (define list-next
        (lambda ()
          (list (next) (next))))
      (sort < (cons (next) (list-next))))
    '(1 2 3))
  (record?
    ((let ()
       (define-record foo (a b c))
       make-foo)
     1 2 3))
  (record?
    ((let ()
       (define-record foo (a b c) (((mutable d) (+ a b))))
       make-foo)
     1 2 3))
  (record?
    ((let ()
       (define-record foo (a b c))
       make-foo)
     1 2 3))
  (error? (letrec ((x (foreign-procedure "foo" () void))) (x 17)))
  (equal?
    (letrec ((x (let ((a 3)
                      (b (letrec ((e (lambda (y) (eq? y x))))
                           (lambda () (e x))))
                      (d (let ((c 4)) (lambda () (+ 5 c)))))
                  (lambda ()
                    (list a (b) (d))))))
      (x))
    '(3 #t 9))
  (equal?
    (letrec ((x (let ((a 3)
                      (b (letrec ((e (lambda (y) (eq? y x))))
                           (lambda () (e x))))
                      (d (let ((c 4)) (lambda () (+ 5 c)))))
                  (lambda ()
                    (set! a (+ a 1))
                    (list a (b) (d))))))
      (x))
    '(4 #t 9))
  (equal?
    (letrec ((x (let ((a 3))
                  (letrec ((b (lambda (x) (+ x 2)))
                           (d (lambda (y) (* y y))))
                    (lambda ()
                      (set! a (+ a 1))
                      (list a (b a) (d a)))))))
      (x))
    '(4 6 16))
  (equal?
    (letrec ((x (let ((a 3))
                  (let ((b (letrec ((e (lambda (y) (eq? y x))))
                             (lambda () (e x))))
                        (d (let ((c 4)) (lambda () (+ a c)))))
                    (lambda ()
                      (set! a (+ a 1))
                      (list a (b) (d)))))))
     (x))
   '(4 #t 8))
  #;(warning?
    (begin
      (define unknown (lambda (x) x))
      (letrec ([foo (unknown (lambda () bar))]
               [bar (lambda () foo)])
        foo)))
  #;(warning?
    (mat/cf
      (begin
        (define unknown (lambda (x) x))
        (letrec ([foo (unknown (lambda () bar))]
                 [bar (unknown (lambda () foo))])
          foo))))
  (error?
    (eval '(letrec* ([f (lambda () q)] [g (f)] [q 17]) g)))
  (error?
    (eval '(begin
             (define unknown (lambda (x) (x)))
             (letrec ([foo (unknown (lambda () bar))]
                      [bar (lambda () foo)])
               foo))))
  (error?
    (eval '(mat/cf
             (begin
               (define unknown (lambda (x) (x)))
               (letrec ([foo (unknown (lambda () bar))]
                        [bar (unknown (lambda () foo))])
                 foo)))))
 ; test cpvalid/undefer interaction
  (error? ; attempt to reference undefined variable b
    (letrec* ([d (letrec ([a (lambda () c)] [b 1] [c b]) 2)]) 3))
  (error? ; attempt to reference undefined variable b
    (letrec* ([d (letrec ([a (lambda () 0)] [b 1] [c b]) 2)]) 3))
  (error? ; attempt to reference undefined variable a
    (letrec* ([d (letrec ([a (lambda () 1)] [c a]) 2)]) 3))
  (error? ; attempt to reference undefined variable b
    (letrec* ([d (letrec* ([a (lambda () 1)] [c b] [b 4]) 2)]) 3))
  (error? ; attempt to reference undefined variable b
    (letrec* ([d (letrec ([a (set! b (lambda () 0))] [b 1]) 2)]) 3))
  (eqv?
    (letrec* ([d (letrec ([a (lambda () 1)] [c (if #f a)]) 2)]) 3)
    3)
  (eqv?
    (letrec* ([d (letrec* ([a (lambda () 1)] [c (if #f b)] [b 4]) 2)]) 3)
    3)
  (eqv?
    (letrec* ([d (letrec ([a (if #f (set! b (lambda () 0)))] [b 1]) 2)]) 3)
    3)
  (eqv?
    (letrec* ([d (letrec ([a (lambda () 0)] [b 1] [c 2]) 2)]) 3)
    3)
  (procedure? (letrec* ([bar (letrec* ([f (lambda (x) f)]) f)]) bar))
  (eqv?
    (letrec* ([d (letrec* ([a 0] [b (set! a (lambda () 1))]) 2)]) 3)
    3)
 ; make sure we don't get valid check(s)
  (equivalent-expansion?
    (parameterize ([run-cp0 (lambda (cp0 x) (cp0 x))]
                   [optimize-level 2])
      (expand/optimize
        '(let ()
           (define f (lambda () (g)))
           (define g (lambda () 17))
           (define x (f))
           x)))
    '17)
  ; check for regression: cpvalid leaving behind a cpvalid-defer form
  (equivalent-expansion?
    (parameterize ([run-cp0 (lambda (cp0 x) x)]
                   [optimize-level 2])
      (expand/optimize '(letrec* ([f (letrec ([x x]) (lambda () x))]) 0)))
    '(let ([f (let ([valid? #f])
                (let ([x (#2%void)])
                  (set! x
                    (begin
                      (if valid?
                          (#2%void)
                          (#2%$source-violation #f #f #t
                            "attempt to reference undefined variable ~s" 'x))
                      x))
                  (set! valid? #t)
                  (lambda () x)))])
       0))
)

(mat generate-procedure-source-information
  (begin
    (define the-source
      (let ([sfd (make-source-file-descriptor "the-source.ss" (open-bytevector-input-port '#vu8()))])
        (make-source-object sfd 10 20)))
    (define (make-proc full-inspect?)
      (parameterize ([generate-inspector-information full-inspect?]
                     [generate-procedure-source-information #t])
        (let ([e '(lambda (x) x)])
          (compile (make-annotation e the-source e)))))
    (define proc-i (make-proc #t))
    (define proc-n (make-proc #f))
    (and (procedure? proc-i)
         (procedure? proc-n)))
  (equal? (((inspect/object proc-i) 'code) 'source-object)
          the-source)
  (equal? (((inspect/object proc-n) 'code) 'source-object)
          the-source)
  (equal? ((((inspect/object proc-i) 'code) 'source) 'value)
          '(lambda (x) x))
  (equal? (((inspect/object proc-n) 'code) 'source)
          #f)
)

(mat strip-fasl-file
  (error?
    (fasl-strip-options ratfink profile-source))
  (error? ; not a string
    (strip-fasl-file (fasl-strip-options profile-source) "testfile.so" (fasl-strip-options profile-source)))
  (error? ; not a string
    (strip-fasl-file "testfile.so" (fasl-strip-options profile-source) (fasl-strip-options profile-source)))
  (error? ; not a fasl-strip-options object
    (strip-fasl-file "testfile.so" "testfile.so" "testfile.so"))
  (enum-set? (fasl-strip-options))
  (enum-set? (fasl-strip-options inspector-source))
  (enum-set? (fasl-strip-options inspector-source compile-time-information))
  (begin
    (define object-file-size
      (lambda (path)
        (bytevector-length (call-with-port (open-file-input-port path (file-options compressed)) get-bytevector-all))))
    (define strip-and-check
      (lambda (in out options)
        (let ([n (object-file-size in)])
          (strip-fasl-file in out options)
          (< (object-file-size out) n))))
    #t)

  ; plain libraries
  (begin
    (with-output-to-file "testfile-sff-1a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-sff-1a)
             (export a x)
             (import (chezscheme))
             (define-syntax a (identifier-syntax (x 5)))
             (define x (lambda (n) (if (= n 0) 1 (* n (x (- n 1)))))))))
      'replace)
    (with-output-to-file "testfile-sff-1b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-sff-1b)
             (export b y)
             (import (chezscheme) (testfile-sff-1a))
             (define-syntax b (syntax-rules () [(_ k) (k y)]))
             (define y (x 4)))))
      'replace)
    (with-output-to-file "testfile-sff-1c.ss"
      (lambda ()
        (pretty-print '(define preexisting-entries (length (profile-dump))))
        (pretty-print '(eval-when (compile) (import (add-prefix (testfile-sff-1a) sff-1a-))))
        (pretty-print '(eval-when (compile) (import (add-prefix (testfile-sff-1b) sff-1b-))))
        (pretty-print '(pretty-print (list (sff-1a-x 3) sff-1b-y)))
        (pretty-print '(pretty-print (not (((inspect/object sff-1a-x) 'code) 'source))))
        (pretty-print '(pretty-print (= (length (profile-dump)) preexisting-entries))))
      'replace)
    (delete-file "testfile-sff-1a.so")
    (delete-file "testfile-sff-1b.so")
    (delete-file "testfile-sff-1c.so")
    (separate-compile
      '(lambda (x)
         (parameterize ([generate-inspector-information #t]
                        [compile-profile #t]
                        [compile-imported-libraries #t])
           (compile-file x)))
      'sff-1c)
    #t)
  (begin
    (define (go)
      (separate-eval
        '(define preexisting-entries
           (with-exception-handler
             (lambda (c) (unless (warning? c) (raise-continuable c)))
             (lambda () (length (profile-dump-list)))))
        '(import (testfile-sff-1a))
        '(import (testfile-sff-1b))
        '(define-syntax so?
           (lambda (x)
             (syntax-case x ()
               [(_ q) (and (syntax->annotation #'q) #t)])))
        '(list a (b so?) (x 3) y)
        '(not (((inspect/object x) 'code) 'source))
        '(define all-entries
           (with-exception-handler
             (lambda (c) (unless (warning? c) (raise-continuable c)))
             (lambda () (length (profile-dump-list)))))
        '(= all-entries preexisting-entries)))
    #t)
  (equal?
    (go)
    "(120 #t 6 24)\n#f\n#f\n")
  (strip-and-check "testfile-sff-1a.so" "testfile-sff-1a.so"
    (fasl-strip-options inspector-source))
  (strip-and-check "testfile-sff-1b.so" "testfile-sff-1b.so"
    (fasl-strip-options inspector-source))
  (equal?
    (go)
    "(120 #t 6 24)\n#t\n#f\n")
  (strip-and-check "testfile-sff-1a.so" "testfile-sff-1a.so"
    (fasl-strip-options profile-source))
  (strip-and-check "testfile-sff-1b.so" "testfile-sff-1b.so"
    (fasl-strip-options profile-source))
  (equal?
    (go)
    "(120 #t 6 24)\n#t\n#t\n")
  (strip-and-check "testfile-sff-1a.so" "testfile-sff-1a.so"
    (fasl-strip-options source-annotations))
  (strip-and-check "testfile-sff-1b.so" "testfile-sff-1b.so"
    (fasl-strip-options source-annotations))
  (equal?
    (go)
    "(120 #f 6 24)\n#t\n#t\n")
  (strip-and-check "testfile-sff-1a.so" "testfile-sff-1a.so"
    (fasl-strip-options compile-time-information))
  (strip-and-check "testfile-sff-1b.so" "testfile-sff-1b.so"
    (fasl-strip-options compile-time-information))
  (strip-and-check "testfile-sff-1c.so" "testfile-sff-1c.so"
    (fasl-strip-options profile-source))
  (equal?
    (separate-eval
      '(guard (c [else (display-condition c) (newline) #t]) (eval '(import (testfile-sff-1b))))
      '(guard (c [else (display-condition c) (newline) #t]) (eval '(import (testfile-sff-1a))))
      '(expand 'a)
      '(expand 'b)
      '(load "testfile-sff-1c.so")
      '(guard (c [else (display-condition c) (newline) #t]) (eval '(import (testfile-sff-1b)))))
    "Exception: loading testfile-sff-1b.so did not define library (testfile-sff-1b)\n#t\n\
     Exception: loading testfile-sff-1a.so did not define library (testfile-sff-1a)\n#t\n\
     a\nb\n\
     (6 24)\n#t\n#t\n\
     Exception: loading testfile-sff-1b.so did not define compile-time information for library (testfile-sff-1b)\n#t\n\
     ")

  ; scripts
  (begin
    (with-output-to-file "testfile-sff.ss"
      (lambda ()
        (printf "#! ~a --script\n" *scheme*)
        (pretty-print '(define (hello) (import (chezscheme)) (printf "hello\n")))
        (pretty-print '(hello)))
      'replace)
    (parameterize ([generate-inspector-information #t])
      (compile-script "testfile-sff"))
    #t)
  (strip-and-check "testfile-sff.so" "testfile-sff-stripped.so"
    (fasl-strip-options inspector-source))
  (equal?
    (separate-eval
      '(load "testfile-sff.so")
      '(and (((inspect/object hello) 'code) 'source) #t))
    "hello\n#t\n")
  (equal?
    (separate-eval
      '(load "testfile-sff-stripped.so")
      '(and (((inspect/object hello) 'code) 'source) #t))
    "hello\n#f\n")
  (equal?
    (run-script "./testfile-sff.so")
    "hello\n")
  (equal?
    (run-script "./testfile-sff-stripped.so")
    "hello\n")

  ; non-library compile-time-information
  (begin
    (with-output-to-file "testfile-sff-3.ss"
      (lambda ()
        (pretty-print '(define cons vector))
        (pretty-print '(define-syntax + (identifier-syntax -))))
      'replace)
    (separate-compile 'sff-3)
    (define $orig-size (object-file-size "testfile-sff-3.so"))
    #t)
  (equal?
    (separate-eval
      '(load "testfile-sff-3.so")
      '(cons 3 4)
      '(+ 3 4))
    "#(3 4)\n-1\n")
  (strip-and-check "testfile-sff-3.so" "testfile-sff-3.so"
    (fasl-strip-options compile-time-information))
  (< (object-file-size "testfile-sff-3.so") $orig-size)
  (equal?
    (separate-eval
      '(load "testfile-sff-3.so")
      '(cons 3 4)
      '(+ 3 4))
    "(3 . 4)\n7\n")
  (let ([n (object-file-size "testfile-sff-3.so")])
    (strip-fasl-file "testfile-sff-3.so" "testfile-sff-3.so"
      (fasl-strip-options compile-time-information))
    (= (object-file-size "testfile-sff-3.so") n))
  (begin
    (mkfile "testfile-sff-4.ss"
      '(library (testfile-sff-4) (export a b c) (import (chezscheme))
         (define-syntax a (identifier-syntax 12))
         (define b 13)
         (meta define c 14)))
    (mkfile "testfile-sff-4p.ss"
      '(import (chezscheme) (testfile-sff-4))
      '(write b))
    (separate-compile
      '(lambda (x) (parameterize ([compile-imported-libraries #t]) (compile-program x)))
      'sff-4p)
    #t)
  (equal?
    (separate-eval
      '(let ()
         (import (testfile-sff-4))
         (define-syntax cc (lambda (x) c))
         (printf "a = ~s, b = ~s, c = ~s\n" a b cc)))
    "a = 12, b = 13, c = 14\n")
  (equal?
    (separate-eval
      '(let ([x (with-output-to-string (lambda () (load-program "testfile-sff-4p.so")))])
         (printf "b = ~a, a = ~s\n" x (eval 'a (environment '(testfile-sff-4))))))
    "b = 13, a = 12\n")
  (begin
    (strip-fasl-file "testfile-sff-4.so" "testfile-sff-4.so"
      (fasl-strip-options compile-time-information))
    #t)
  (error? ; no compile-time info
    (separate-eval
      '(let ()
         (import (testfile-sff-4))
         (list a b))))
  (error? ; no compile-time info
    (separate-eval
      '(let ([x (with-output-to-string (lambda () (load-program "testfile-sff-4p.so")))])
         (printf "b = ~a, a = ~s\n" x (eval 'a (environment '(testfile-sff-4)))))))
  (error? ; no compile-time info
    (separate-eval
      '(let ([x (with-output-to-string (lambda () (load-program "testfile-sff-4p.so")))])
         (printf "b = ~a, a = ~s\n" x (eval '(let () (import (testfile-sff-4)) a))))))
  (error? ; no compile-time info
    (separate-eval
      '(parameterize ([import-notify #t])
         (let ([x (with-output-to-string (lambda () (load-program "testfile-sff-4p.so")))])
           (printf "b = ~a, a = ~s\n" x (eval '(let () (import (testfile-sff-4)) a)))))))
)

(mat $fasl-file-equal?
  (let ([fn (find-source "fatfib.ss")])
    (parameterize ([generate-inspector-information #t])
      (compile-file fn "testfile-fatfib1.so"))
    (parameterize ([generate-inspector-information #t])
      (compile-file fn "testfile-fatfib2.so"))
    (parameterize ([generate-inspector-information #f])
      (compile-file fn "testfile-fatfib3.so"))
    #t)
  (error? ; not a string
    (#%$fasl-file-equal? 'testfile-fatfib1.so "testfile-fatfib2.so"))
  (error? ; not a string
    (#%$fasl-file-equal? 'testfile-fatfib1.so "testfile-fatfib2.so" #t))
  (error? ; not a string
    (#%$fasl-file-equal? "testfile-fatfib1.so" 13.4))
  (error? ; not a string
    (#%$fasl-file-equal? "testfile-fatfib1.so" 13.4 #f))
  (error? ; file doesn't exist
    (#%$fasl-file-equal? "testfile-fatfib1.so" "probably-does-not-exist"))
  (error? ; file doesn't exist
    (#%$fasl-file-equal? "testfile-fatfib1.so" "probably-does-not-exist" #f))
  (error? ; file doesn't exist
    (#%$fasl-file-equal? "probably-does-not-exist" "testfile-fatfib2.so"))
  (error? ; file doesn't exist
    (#%$fasl-file-equal? "probably-does-not-exist" "testfile-fatfib2.so" #t))
  (#%$fasl-file-equal? "testfile-fatfib1.so" "testfile-fatfib2.so")
  (not (#%$fasl-file-equal? "testfile-fatfib1.so" "testfile-fatfib3.so"))
  (error? (#%$fasl-file-equal? "testfile-fatfib1.so" "testfile-fatfib3.so" #t))
)

(mat vfasl
  (begin
    (define-record-type vfasl-demo
      (fields x y)
      (nongenerative #{vfasl-demo pfwhk286n2j894o33awcq9er4-0}))
    (define vfasl-content (list 1 1/2 3.0 4+5i 6.0+7.0i
                                "apple" 'banana (string->uninterned-symbol "coconut")
                                (make-vfasl-demo 10 "11")
                                (vector 1 'two "three")
                                (stencil-vector 30 'one 2.0 0+3i "four")
                                (box 88)
                                "" '#() '#vu8() (make-fxvector 0) (make-flvector 0)
                                (string->immutable-string "") (vector->immutable-vector '#())
                                (bytevector->immutable-bytevector '#vu8())))
    (define (same-vfasl-content? v)
      (andmap (lambda (a b)
                (or (eqv? a b)
                    (and (uninterned-symbol? a)
                         (uninterned-symbol? b)
                         (equal? (symbol->string a) (symbol->string b)))
                    (and (or (and (string? a)
                                  (positive? (string-length a)))
                             (and (vector? a)
                                  (positive? (vector-length a)))
                             (box? a)
                             (stencil-vector? a))
                         (equal? a b))
                    (and (vfasl-demo? a)
                         (vfasl-demo? b)
                         (equal? (vfasl-demo-x a)
                                 (vfasl-demo-x b))
                         (equal? (vfasl-demo-y a)
                                 (vfasl-demo-y b)))
                    (begin
                      (printf "~s ~s\n" a b)
                      #f)))
              vfasl-content
              v))
    (compile-to-file (list `(define (vfasled) ',vfasl-content)
                           `(define (get-vfasled) vfasled)
                           `(define (call-vfasled) (vfasled)))
                     "testfile-fasl.so")
    (case (machine-type)
      [(pb tpb)
       ;; vfasl not supported with endianness not statically known
       (load "testfile-fasl.so")]
      [else
       (vfasl-convert-file "testfile-fasl.so" "testfile-vfasl.so" #f)
       (load "testfile-vfasl.so")])
    #t)
  
  (same-vfasl-content? (vfasled))
  (eq? vfasled (get-vfasled))
  (eq? (vfasled) (call-vfasled)))

(mat cost-center
  (error? ; wrong number of arguments
    (make-cost-center 'foo))

  (error? ; foo is not a cost center
    (with-cost-center 'foo (lambda () 5)))

  (error? ; bar is not a procedure
    (with-cost-center (make-cost-center) 'bar))

  (error? ; 5 is not a cost center
    (cost-center-instruction-count 5))

  (error? ; "test" is not a cost center
    (cost-center-allocation-count "test"))

  (error? ; 4.7 is not a cost center
    (cost-center-time 4.7))

  (error? ; #\c is not a cost center
    (reset-cost-center! #\c))

  (let ([cc (make-cost-center)])
    (cost-center? cc))

  ;;; instruction cost center tests
  ((lambda (x)
     (<= 5 x 50))
   (let ([cc (make-cost-center)])
     (with-cost-center cc
       (lambda ()
         (parameterize ([generate-instruction-counts #t]
                        [compile-interpret-simple #f]
                        [enable-cp0 #f])
           (compile '(let ([p (cons 'a 'b)]) (car p))))))
     (cost-center-instruction-count cc)))

  (begin
    (define $cc-sum-1
      (parameterize ([generate-instruction-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls])
               (if (null? ls)
                   0
                   (+ (car ls) (f (cdr ls)))))))))
    #t)

  ((lambda (x)
     (<= 100 x 1000))
   (let ([cc (make-cost-center)])
     (with-cost-center cc (lambda () ($cc-sum-1 (iota 10))))
     (cost-center-instruction-count cc)))

  ((lambda (x)
     (<= 1000 x 10000))
   (let ([cc (make-cost-center)])
     (with-cost-center cc (lambda () ($cc-sum-1 (iota 100))))
     (cost-center-instruction-count cc)))

  (begin
    (define $cc-1 (make-cost-center))
    (define $cc-sum-2
      (parameterize ([generate-instruction-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls])
               (with-cost-center $cc-1
                 (lambda ()
                   (if (null? ls)
                       0
                       (+ (car ls) (f (cdr ls)))))))))))
    #t)

  ((lambda (x)
     (<= 100 x 1500))
   (begin
     ($cc-sum-2 (iota 10))
     (cost-center-instruction-count $cc-1)))

  (begin
    (reset-cost-center! $cc-1)
    #t)

  ((lambda (x)
     (<= 1000 x 15000))
   (begin
     ($cc-sum-2 (iota 100))
     (cost-center-instruction-count $cc-1)))

  (begin
    (reset-cost-center! $cc-1)
    #t)

  (let ([cc (make-cost-center)])
    (with-cost-center cc (lambda () ($cc-sum-2 (iota 10))))
    (<= (cost-center-instruction-count $cc-1) (cost-center-instruction-count cc)))

  (begin
    (define-syntax when-threaded
      (lambda (x)
        (syntax-case x ()
          [(_ e0 e1 ...)
           (if (threaded?)
               #'(begin e0 e1 ...)
               #'(begin #t))])))
    #t)

  (when-threaded
    (begin
      (include "thread-check.ss")
      ($thread-check)))

  (when-threaded
    ((lambda (x)
       (<= 200 x 2000))
     (let ([cc (make-cost-center)])
       (define sum-th
         (lambda ()
           (with-cost-center cc (lambda () ($cc-sum-1 (iota 10))))))
       (define t1 (fork-thread sum-th))
       (define t2 (fork-thread sum-th))
       (thread-join t1)
       (thread-join t2)
       (cost-center-instruction-count cc))))

  (when-threaded ($thread-check))

  (when-threaded
    (reset-cost-center! $cc-1)
    ((lambda (x)
       (<= 200 x 3000))
     (let ()
       (define sum-th
         (lambda ()
           ($cc-sum-2 (iota 10))))
       (define t1 (fork-thread sum-th))
       (define t2 (fork-thread sum-th))
       (thread-join t1)
       (thread-join t2)
       (cost-center-instruction-count $cc-1))))

  (when-threaded ($thread-check))

  (when-threaded
    (reset-cost-center! $cc-1)
    (let ([cc (make-cost-center)])
      (define sum-th
        (lambda ()
          (with-cost-center cc (lambda () ($cc-sum-2 (iota 10))))))
       (define t1 (fork-thread sum-th))
       (define t2 (fork-thread sum-th))
       (thread-join t1)
       (thread-join t2)
       (<= (cost-center-instruction-count $cc-1)
           (cost-center-instruction-count cc))))

  (when-threaded ($thread-check))

  (begin
    (define $cc-fibonacci
      (let ([fib
              (parameterize ([generate-instruction-counts #t])
                (compile
                  '(rec fib
                     (lambda (i)
                       (cond
                         [(= i 0) 0]
                         [(= i 1) 1]
                         [else (+ (fib (- i 1))
                                  (fib (- i 2)))])))))])
        (lambda (n) (with-cost-center $cc-1 (lambda () (fib n))))))
    #t)

  (let ([normal-count (begin
                        (reset-cost-center! $cc-1)
                        ($cc-fibonacci 10)
                        (cost-center-instruction-count $cc-1))]
        [eng-count (begin
                     (reset-cost-center! $cc-1)
                     (let f ([eng (make-engine (lambda () ($cc-fibonacci 10)))])
                       (eng 50 (lambda args (cost-center-instruction-count $cc-1)) f)))])
    ; range because when running in an engine the trap check might
    ; be taken, and it will slightly increase the instruction count
    (<= normal-count eng-count (+ normal-count 100)))

  ;;; allocation cost center tests
  (eqv?
    (case (fixnum-width)
      [(30) 24]
      [(61) 48])
    (let ([cc (make-cost-center)])
      (with-cost-center cc
        (lambda ()
          (parameterize ([generate-allocation-counts #t]
                         [compile-interpret-simple #f])
            (compile '(#%list 'a 'b 'c)))))
      (cost-center-allocation-count cc)))

  ((lambda (count) ; range for rand call done to test variable alloc case and 64-bit words
     (<= 16 count 120))
   (let ([cc (make-cost-center)])
     (with-cost-center cc
       (lambda ()
         (parameterize ([generate-allocation-counts #t] [compile-interpret-simple #f])
           (compile `(let ([x (fx+ 3 (random 10))])
                       (#3%make-vector x))))))
     (cost-center-allocation-count cc)))

  (begin
    (define $cc-reverse-1
      (parameterize ([generate-allocation-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls] [rls '()])
               (if (null? ls)
                   rls
                   (f (cdr ls) (#%cons (car ls) rls))))))))
    #t)

  (eqv?
    (case (fixnum-width)
      [(30) 80]
      [(61) 160])
    (let ([cc (make-cost-center)])
      (with-cost-center cc (lambda () ($cc-reverse-1 (make-list 10))))
      (cost-center-allocation-count cc)))

  (eqv?
    (case (fixnum-width)
      [(30) 800]
      [(61) 1600])
    (let ([cc (make-cost-center)])
      (with-cost-center cc (lambda () ($cc-reverse-1 (make-list 100))))
      (cost-center-allocation-count cc)))

  (begin
    (define $cc-2 (make-cost-center))
    (define $cc-reverse-2
      (parameterize ([generate-allocation-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls] [rls '()])
               (with-cost-center $cc-2
                 (lambda () 
                   (if (null? ls)
                       rls
                       (f (cdr ls) (#%cons (car ls) rls))))))))))
    #t)

  ((lambda (x) ; wide range to accommodate both 4-byte and 8-byte words
     (<= 80 x 480))
   (begin
     ($cc-reverse-2 (make-list 10))
     (cost-center-allocation-count $cc-2)))

  (begin
    (reset-cost-center! $cc-2)
    #t)

  ((lambda (x) ; wide range to accommodate both 4-byte and 8-byte words
     (<= 800 x 4800))
   (begin
     ($cc-reverse-2 (make-list 100))
     (cost-center-allocation-count $cc-2)))

  (begin
    (reset-cost-center! $cc-2)
    #t)

  (let ([cc (make-cost-center)])
    (with-cost-center cc (lambda () ($cc-reverse-2 (make-list 10))))
    (<= (cost-center-allocation-count $cc-2) (cost-center-allocation-count cc)))

  (begin
    (define $cc-reverse-3
      (let ([rev (parameterize ([generate-allocation-counts #t])
                   (compile
                     '(rec rev
                        (lambda (ls rls)
                          (if (null? ls)
                              rls
                              (rev (cdr ls) (#%cons (car ls) rls)))))))])
        (lambda (ls)
          (with-cost-center $cc-2 (lambda () (rev ls '()))))))
    #t)

  (eqv?
    (begin
      (reset-cost-center! $cc-2)
      ($cc-reverse-3 (iota 10))
      (cost-center-allocation-count $cc-2))
    (begin
      (reset-cost-center! $cc-2)
      (let f ([eng (make-engine (lambda () ($cc-reverse-3 (iota 10))))])
        (eng 10 (lambda args (cost-center-allocation-count $cc-2)) f))))

  (when-threaded
    (eqv?
      (case (fixnum-width)
        [(30) 160]
        [(61) 320])
      (let ([cc (make-cost-center)])
        (define reverse-th
          (lambda ()
            (with-cost-center cc (lambda () ($cc-reverse-1 (iota 10))))))
        (define t1 (fork-thread reverse-th))
        (define t2 (fork-thread reverse-th))
        (thread-join t1)
        (thread-join t2)
        (cost-center-allocation-count cc))))

  (when-threaded ($thread-check))

  (when-threaded
    (reset-cost-center! $cc-2)
    ((lambda (x) ; wide range to accommodate both 4-byte and 8-byte words
       (<= 160 x 960))
     (let ()
       (define reverse-th
         (lambda ()
           ($cc-reverse-2 (iota 10))))
        (define t1 (fork-thread reverse-th))
        (define t2 (fork-thread reverse-th))
        (thread-join t1)
        (thread-join t2)
       (cost-center-allocation-count $cc-2))))

  (when-threaded ($thread-check))

  (when-threaded
    (reset-cost-center! $cc-2)
    (let ([cc (make-cost-center)])
      (define reverse-th
        (lambda ()
          (with-cost-center cc (lambda () ($cc-reverse-2 (iota 10))))))
       (define t1 (fork-thread reverse-th))
       (define t2 (fork-thread reverse-th))
       (thread-join t1)
       (thread-join t2)
       (<= (cost-center-instruction-count $cc-2)
           (cost-center-instruction-count cc))))

  (when-threaded ($thread-check))

  ;;; instruction with allocation cost center tests
  ((lambda (x)
     (<= 10 x 50))
   (let ([cc (make-cost-center)])
     (with-cost-center cc
       (lambda () 
         (parameterize ([generate-allocation-counts #t]
                        [generate-instruction-counts #t]
                        [compile-interpret-simple #f]
                        [enable-cp0 #f])
           (compile '(let ([p (cons 'a 'b)]) (car p))))))
     (cost-center-instruction-count cc)))

  (begin
    (define $cc-sum-1
      (parameterize ([generate-allocation-counts #t]
                     [generate-instruction-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls])
               (if (null? ls)
                   0
                   (+ (car ls) (f (cdr ls)))))))))
    #t)

  ((lambda (x)
     (<= 100 x 1000))
   (let ([cc (make-cost-center)])
     (with-cost-center cc (lambda () ($cc-sum-1 (iota 10))))
     (cost-center-instruction-count cc)))

  ((lambda (x)
     (<= 1000 x 10000))
   (let ([cc (make-cost-center)])
     (with-cost-center cc (lambda () ($cc-sum-1 (iota 100))))
     (cost-center-instruction-count cc)))

  (begin
    (define $cc-1 (make-cost-center))
    (define $cc-sum-2
      (parameterize ([generate-allocation-counts #t]
                     [generate-instruction-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls])
               (with-cost-center $cc-1
                 (lambda () 
                   (if (null? ls)
                       0
                       (+ (car ls) (f (cdr ls)))))))))))
    #t)

  ((lambda (x)
     (<= 100 x 1500))
   (begin
     ($cc-sum-2 (iota 10))
     (cost-center-instruction-count $cc-1)))

  (begin
    (reset-cost-center! $cc-1)
    #t)

  ((lambda (x)
     (<= 1000 x 15000))
   (begin
     ($cc-sum-2 (iota 100))
     (cost-center-instruction-count $cc-1)))

  (begin
    (reset-cost-center! $cc-1)
    #t)

  (let ([cc (make-cost-center)])
    (with-cost-center cc (lambda () ($cc-sum-2 (iota 10))))
    (<= (cost-center-instruction-count $cc-1) (cost-center-instruction-count cc)))

  ;; allocation with instruction counts
  (eqv?
    (case (fixnum-width)
      [(30) 24]
      [(61) 48])
    (let ([cc (make-cost-center)])
      (with-cost-center cc
        (lambda ()
          (parameterize ([generate-allocation-counts #t]
                         [generate-instruction-counts #t]
                         [compile-interpret-simple #f])
            (compile '(#%list 'a 'b 'c)))))
      (cost-center-allocation-count cc)))

  (let ([x (fx+ 3 (random 10))])
    ((lambda (count) ; range for padding on 32-bit and to accommodate 64-bit words
       (<= (fxsll (fx+ x 1) 2) count (fxsll (fx+ x 2) 3)))
     (let ([cc (make-cost-center)])
       (with-cost-center cc
         (lambda ()
           (parameterize ([generate-allocation-counts #t]
                          [generate-instruction-counts #t]
                          [compile-interpret-simple #f])
             (compile `(#%make-vector ,x)))))
       (cost-center-allocation-count cc))))

  (begin
    (define $cc-reverse-1
      (parameterize ([generate-allocation-counts #t]
                     [generate-instruction-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls] [rls '()])
               (if (null? ls)
                   rls
                   (f (cdr ls) (#%cons (car ls) rls))))))))
    #t)

  (eqv?
    (case (fixnum-width)
      [(30) 80]
      [(61) 160])
    (let ([cc (make-cost-center)])
      (with-cost-center cc (lambda () ($cc-reverse-1 (make-list 10))))
      (cost-center-allocation-count cc)))

  (eqv?
    (case (fixnum-width)
      [(30) 800]
      [(61) 1600])
    (let ([cc (make-cost-center)])
      (with-cost-center cc (lambda () ($cc-reverse-1 (make-list 100))))
      (cost-center-allocation-count cc)))

  (begin
    (define $cc-2 (make-cost-center))
    (define $cc-reverse-2
      (parameterize ([generate-allocation-counts #t]
                     [generate-instruction-counts #t])
        (compile
          '(lambda (ls)
             (let f ([ls ls] [rls '()])
               (with-cost-center $cc-2
                 (lambda ()
                   (if (null? ls)
                       rls
                       (f (cdr ls) (#%cons (car ls) rls))))))))))
    #t)

  ((lambda (x) ; wide range to accommodate both 4-byte and 8-byte words
     (<= 80 x 480))
   (begin
     ($cc-reverse-2 (make-list 10))
     (cost-center-allocation-count $cc-2)))

  (begin
    (reset-cost-center! $cc-2)
    #t)

  ((lambda (x) ; wide range to accommodate both 4-byte and 8-byte words
     (<= 800 x 4800))
   (begin
     ($cc-reverse-2 (make-list 100))
     (cost-center-allocation-count $cc-2)))

  (> (cost-center-allocation-count $cc-2) 0)
  (> (cost-center-instruction-count $cc-2) 0)

  (begin
    (reset-cost-center! $cc-2)
    #t)

  (fx= (cost-center-allocation-count $cc-2) 0)
  (fx= (cost-center-instruction-count $cc-2) 0)

  (let ([cc (make-cost-center)])
    (with-cost-center cc (lambda () ($cc-reverse-2 (make-list 10))))
    (<= (cost-center-allocation-count $cc-2) (cost-center-allocation-count cc)))

  (begin
    (define $fib (lambda (x) (if (< x 2) 1 (+ ($fib (- x 1)) ($fib (- x 2))))))
    #t)

  ;; timing information (no instrumentation needed)
  ((lambda (x)
     (and (time<? (make-time 'time-duration 0 0) x)
          (time<? x (make-time 'time-duration 0 10))))
   (let ([cc (make-cost-center)])
     (with-cost-center #t cc
       (lambda ()
         (let ([t0 (current-time 'time-thread)])
           (let f ()
             (when (time=? (current-time 'time-thread) t0)
               ($fib 10)
               (f))))))
     (cost-center-time cc)))

  (let ([cc1 (make-cost-center)] [cc2 (make-cost-center)])
    (with-cost-center #t cc1
      (lambda ()
        (let f ([n 10])
          (with-cost-center #t cc2
            (lambda ()
              (cond
                [(= n 0) 1]
                [(= n 1) 1]
                [else (+ (f (- n 1)) (f (- n 2)))]))))))
    (time<=? (cost-center-time cc2) (cost-center-time cc1)))

  (begin
    (define $cc-3 (make-cost-center))
    (define $cc-fib
      (parameterize ([generate-allocation-counts #t]
                     [generate-instruction-counts #t])
        (compile
          '(let ()
             (define (n->peano n)
               (if (zero? n)
                   '()
                   (cons 'succ (n->peano (- n 1)))))
             (define peano->n length)
             (define (peano-sub1 n)
               (if (null? n)
                   (errorf 'peano-sub "cannot subtract 1 from 0")
                   (cdr n)))
             (define peano-zero '())
             (define (peano-add1 n) (#%cons 'succ n))
             (define (peano+ n1 n2)
               (if (eq? n1 peano-zero)
                   n2
                   (peano-add1 (peano+ (peano-sub1 n1) n2))))
             (lambda (n)
               (with-cost-center #t $cc-3
                 (lambda ()
                   (peano->n
                     (let f ([n (n->peano n)])
                       (cond
                         [(equal? n peano-zero) (peano-add1 peano-zero)]
                         [(equal? n (peano-add1 peano-zero)) (peano-add1 peano-zero)]
                         [else
                           (let ([n (peano-sub1 n)])
                             (peano+ (f n) (f (peano-sub1 n))))]))))))))))
    #t)

  (fx= (cost-center-instruction-count $cc-3) 0)
  (fx= (cost-center-allocation-count $cc-3) 0)
  (time=? (cost-center-time $cc-3) (make-time 'time-duration 0 0))

  ((lambda (x)
     (and (time<? (make-time 'time-duration 0 0) x)
          ;; on a modern machine, we expect a time on the
          ;; order of 1s, but allow a lof ot variation or
          ;; a slow machine with up to 50s:
          (or (time<? x (make-time 'time-duration 0 50))
              (#%$enable-check-heap))))
   (begin
     ($cc-fib 30)
     (cost-center-time $cc-3)))

  (> (cost-center-instruction-count $cc-3) 0)
  (> (cost-center-allocation-count $cc-3) 0)
  (time>? (cost-center-time $cc-3) (make-time 'time-duration 0 0))

  (begin
    (reset-cost-center! $cc-3)
    #t)

  (fx= (cost-center-instruction-count $cc-3) 0)
  (fx= (cost-center-allocation-count $cc-3) 0)
  (time=? (cost-center-time $cc-3) (make-time 'time-duration 0 0))
)



(mat lock-object
  (begin
    (define $locked-objects (foreign-procedure "(cs)locked_objects" () ptr))
    #t)
  (let ([ls ($locked-objects)])
    (unless (null? ls) (errorf #f "found locked objects ~s" ls))
    #t)
  (let ()
    (define-record user-event (x))
    (do ([n 20 (- n 1)])
        ((= n 0))
      (for-each unlock-object
        (map (lambda (x) (lock-object x) x)
          (map make-user-event
            (make-list 10000)))))
    #t)
  (let ([ls ($locked-objects)])
    (unless (null? ls) (errorf #f "found locked objects ~s" ls))
    #t)
  (let ()
    (define-record user-event (x))
    (do ([n 20 (- n 1)])
        ((= n 0))
      (for-each unlock-object
        (map (lambda (x)
               (let ([x (case x
                          [(0) (lambda () x)]
                          [(1) (cons x x)]
                          [(2) (vector x)]
                          [(3) (vector x x)]
                          [(4) (string #\a #\b)]
                          [(5) (box (cons 3 4))]
                          [(6) (/ 8 17)]
                          [(7) (exact (sin 3.0))]
                          [(8) (exact (sqrt -73.0))]
                          [(9) (call/cc values)]
                          [(10) (make-user-event x)])])
                 (lock-object x)
                 x))
             (map random (make-list 2000 11)))))
    #t)
  (let ([ls ($locked-objects)])
    (unless (null? ls) (errorf #f "found locked objects ~s" ls))
    #t)
  (eqv?
    (let ()
      (define (pick ls) (list-ref ls (random (length ls))))
     ; we don't pick then remq-first because the picked element may be
     ; an unlocked flonum and may be cloned into two copies by the
     ; collector between the pick and the remq-first
      (define (pick-rem ls)
        (let f ([ls ls] [i (random (length ls))])
          (if (fx= i 0)
              (values (car ls) (cdr ls))
              (let-values ([(x d) (f (cdr ls) (fx- i 1))])
                (values x (cons (car ls) d))))))
      (module (random-tree)
        (define leaves
          `(,(lambda () '())
            ,(lambda () 0)
            ,(lambda () #f)
            ,(lambda () #t)
            ,(lambda () #\q)
            ,(lambda () (* 3.4 5))
            ,(lambda () (* 15/16 5))
            ,(lambda () (* 1+2i 5))
            ,(lambda () (* 3.0-2.5i 5))
            ,(lambda () (pick (oblist)))
            ,gensym
            ,(lambda () (make-string (random 10) (pick '(#\$ #\! #\*))))
            ))
        (define nodes
          `(,(lambda (th) (cons (th) (th)))
            ,(lambda (th) (weak-cons (th) (th)))
            ,(lambda (th) (list->vector (map (lambda (x) (th)) (make-list (+ 1 (random 4))))))
            ,(lambda (th)
               (define-record frob ((immutable x) (immutable y)))
               (record-reader 'frob1 (type-descriptor frob))
               (make-frob (th) (th)))
            ,(lambda (th)
               (define-record frob ((immutable x) (mutable y)))
               (record-reader 'frob2 (type-descriptor frob))
               (make-frob (th) (th)))
            ,(lambda (th)
               (define-record frob ((immutable x) (immutable integer-32 y)))
               (record-reader 'frob3 (type-descriptor frob))
               (make-frob (th) (random 200000)))
            ,(lambda (th)
               (define-record frob ((immutable x) (mutable integer-32 y)))
               (record-reader 'frob4 (type-descriptor frob))
               (make-frob (th) (random 200000)))
            ,(lambda (th)
               (let ([x (th)] [y (th)])
                 (let ([f (lambda () (cons x y))])
                   (values f (#%$closure-code f)))))
            ,(lambda (th)
               (let ([x (th)] [y (th)])
                 (call/cc
                   (lambda (k)
                     (call/cc (lambda (k1) (k k1)))
                     (cons x y)))))
            ))
        (define random-tree
          (lambda (n)
            (let ([objects '()])
              (let ([t (let f ([n n])
                         (let-values ([t* (if (= n 0)
                                              ((pick leaves))
                                              ((pick nodes) (lambda () (f (- n 1)))))])
                           (set! objects (append t* objects))
                           (car t*)))])
                objects)))))
      (define (chew n)
        (let f ([ls (make-list n)])
          (if (< (length ls) 2)
              (random-tree 2)
              (append (f (cddr ls)) (f (cdr ls))))))
      (define (randomize ls)
        (if (null? ls)
            '()
            (let-values ([(a d) (pick-rem ls)])
              (cons a (randomize d)))))
      (define (split ls)
        (if (null? ls)
            (values '() '())
            (let-values ([(a ls) (pick-rem ls)])
              (let-values ([(ls1 ls2) (split ls)])
                (if (= (random 2) 0)
                    (values (cons a ls1) ls2)
                    (values ls1 (cons a ls2)))))))
      (define (locktest)
        (define m 5)
        (let f ([n 100] [l0 '()] [l1 '()] [l2 '()])
          (let ([l1addr (map #%$fxaddress l1)] [l2addr (map #%$fxaddress l2)])
            (chew 15)
            (let ([bad (remq f
                         (map (lambda (x a) (if (fx= (#%$fxaddress x) a) f x))
                              (append l1 l2)
                              (append l1addr l2addr)))])
              (unless (andmap flonum? bad)
                (errorf 'locktest "locked object address(es) changed for ~s" bad))))
          (if (= n 0)
              (begin
                (for-each unlock-object l1)
                (for-each unlock-object l2)
                (for-each unlock-object l2)
                'yippee!)
              (let-values ([(l0drop l0keep) (split l0)]
                           [(l1drop l1keep) (split l1)]
                           [(l2drop l2keep) (split l2)])
                (for-each unlock-object l1drop)
                (for-each unlock-object l2drop)
                (for-each unlock-object l2drop)
                (let-values ([(l0stay l0up) (split l0keep)]
                             [(l1down l1up) (split l1keep)]
                             [(l2down l2stay) (split l2keep)])
                  (for-each lock-object l0up)
                  (for-each lock-object l1up)
                  (for-each unlock-object l1down)
                  (for-each unlock-object l2down)
                  (f (- n 1)
                     (randomize (append l0stay l1down))
                     (let ([l1new (random-tree m)])
                       (for-each lock-object l1new)
                       (randomize (append l0up l2down l1new)))
                     (randomize (append l1up l2stay))))))))
      (locktest))
    'yippee!)
  (let ([ls ($locked-objects)])
    (unless (null? ls) (errorf #f "found locked objects ~s" ls))
    #t)
  (eqv?
    (let ()
      (define-record frob ((immutable x) (immutable y))
        ([(immutable hash) (hash-frob x y)]))
      (define leaves
        `(,(lambda () '())
          ,(lambda () 0)
          ,(lambda () #f)
          ,(lambda () #t)
          ,(lambda () #\q)
          ,(lambda () (* 3.4 5))
          ,(lambda () (* 15/16 5))
          ,(lambda () (* 1+2i 5))
          ,(lambda () (* 3.0-2.5i 5))
          ,(lambda () (pick (oblist)))
          ,gensym
          ,(lambda () (make-string (random 10) (pick '(#\$ #\! #\*))))
          ))
      (define (hash-frob x y) (+ 13 (ash (hash x) 4) (* (hash y) 7)))
      (define (hash x)
        (case x
          [(()) 1]
          [(0) 2]
          [(#f) 3]
          [(#t) 4]
          [(#\q) 5]
          [(17.0) 6]
          [(75/16) 7]
          [(5+10i) 8]
          [(15.0-12.5i) 9]
          [else
           (cond
             [(gensym? x) (+ 10 (ash (hash-string (symbol->string x)) 4))]
             [(symbol? x) (+ 11 (ash (hash-string (symbol->string x)) 4))]
             [(string? x) (+ 12 (ash (hash-string x) 4))]
             [(frob? x) (hash-frob (frob-x x) (frob-y x))]
             [else (errorf 'hash "unexpected object ~s" x)])]))
      (define (hash-string s)
        (apply logxor (map char->integer (string->list s))))
      (define (check-hash x)
        (let ([h (hash x)]) ; run regardless for error check
          (when (frob? x)
            (unless (= (hash x) (frob-hash x))
              (errorf 'check-hash "hash mismatch for ~s" x)))))
      (define (pick ls) (list-ref ls (random (length ls))))
     ; we don't pick then remq-first because the picked element may be
     ; an unlocked flonum and may be cloned into two copies by the
     ; collector between the pick and the remq-first
      (define (pick-rem ls)
        (let f ([ls ls] [i (random (length ls))])
          (if (fx= i 0)
              (values (car ls) (cdr ls))
              (let-values ([(x d) (f (cdr ls) (fx- i 1))])
                (values x (cons (car ls) d))))))
      (define random-tree
        (lambda (n)
          (let ([objects '()])
            (let ([t (let f ([n n])
                       (let-values ([t* (if (= n 0)
                                            ((pick leaves))
                                            (make-frob (f (- n 1)) (f (- n 1))))])
                         (set! objects (append t* objects))
                         (car t*)))])
                objects))))
      (define (chew n)
        (let f ([ls (make-list n)])
          (if (< (length ls) 2)
              (random-tree 2)
              (append (f (cddr ls)) (f (cdr ls))))))
      (define (randomize ls)
        (if (null? ls)
            '()
            (let-values ([(a d) (pick-rem ls)])
              (cons a (randomize d)))))
      (define (split ls)
        (if (null? ls)
            (values '() '())
            (let-values ([(a ls) (pick-rem ls)])
              (let-values ([(ls1 ls2) (split ls)])
                (if (= (random 2) 0)
                    (values (cons a ls1) ls2)
                    (values ls1 (cons a ls2)))))))
      (define (locktest)
        (define m 5)
        (let f ([n 100] [l0 '()] [l1 '()] [l2 '()])
          (let ([l1addr (map #%$fxaddress l1)] [l2addr (map #%$fxaddress l2)])
            (chew 15)
            (let ([bad (remq f
                         (map (lambda (x a) (if (fx= (#%$fxaddress x) a) f x))
                              (append l1 l2)
                              (append l1addr l2addr)))])
              (unless (andmap flonum? bad)
                (errorf 'locktest "locked object address(es) changed for ~s" bad))))
          (for-each check-hash l0)
          (for-each check-hash l1)
          (for-each check-hash l2)
          (if (= n 0)
              (begin
                (for-each unlock-object l1)
                (for-each unlock-object l2)
                (for-each unlock-object l2)
                'yippee!)
              (let-values ([(l0drop l0keep) (split l0)]
                           [(l1drop l1keep) (split l1)]
                           [(l2drop l2keep) (split l2)])
                (for-each unlock-object l1drop)
                (for-each unlock-object l2drop)
                (for-each unlock-object l2drop)
                (let-values ([(l0stay l0up) (split l0keep)]
                             [(l1down l1up) (split l1keep)]
                             [(l2down l2stay) (split l2keep)])
                  (for-each lock-object l0up)
                  (for-each lock-object l1up)
                  (for-each unlock-object l1down)
                  (for-each unlock-object l2down)
                  (f (- n 1)
                     (randomize (append l0stay l1down))
                     (let ([l1new (random-tree m)])
                       (for-each lock-object l1new)
                       (randomize (append l0up l2down l1new)))
                     (randomize (append l1up l2stay))))))))
      (locktest))
    'yippee!)
  (let ([ls ($locked-objects)])
    (unless (null? ls) (errorf #f "found locked objects ~s" ls))
    #t)
  (parameterize ([collect-request-handler void])
    (define x (cons 3 4))
    (lock-object x)
    (collect 1 1) ; should leave segment containing x with locked bit
    (set-cdr! x (cons 0 0)) ; should mark the card containing x in the segment dirty
    (collect 0 0) ; should crash if sweep_dirty doesn't ignore locked objects
    (unlock-object x)
    #t)
  (let ([ls ($locked-objects)])
    (unless (null? ls) (errorf #f "found locked objects ~s" ls))
    #t)
  ; shouldn't include immediates in locked-object lists
  (begin
    (lock-object -17)
    (lock-object #f)
    (lock-object #!eof)
    (lock-object #\newline)
    (let ([ls ($locked-objects)])
      (unless (null? ls) (errorf #f "found locked objects ~s" ls))
      #t))
  ; cons should be static, and shouldn't include static objects in locked-object lists
  (begin
    (lock-object 'cons)
    (let ([ls ($locked-objects)])
      (unless (null? ls) (errorf #f "found locked objects ~s" ls))
      #t))
  ; locked objects promoted to static generation are listed in the static-generation locked list
  ; so mutated locked objects are properly swept (and the cards they're in, which might contain
  ; random stuff, aren't)
  #;(parameterize ([collect-request-handler void])
    (define x (cons 3 4))
    (lock-object x)
    (collect (collect-maximum-generation) 'static)
    (let ([ls ($locked-objects)])
      (unless (null? ls) (errorf #f "found locked objects ~s" ls))
      #t))

  ;; Make sure a locked object that spans segments is appropriately
  ;; swept when it's modified to ceate a backpointer
  (let* ([N 100000]
         [v (make-vector N)])
    (lock-object v)
    (collect 0)
    (let ([p (cons 1 2)])
      (vector-set! v (sub1 N) p)
      (collect 0)
      (set-car! p 'yes)
      (unlock-object v)
      (equal? '(yes . 2) (vector-ref v (sub1 N)))))
  )

(mat eval-order
  (eqv? (call/cc (lambda (k) (0 (k 1)))) 1)
  (eqv? (let ([zero 0]) (call/cc (lambda (k) (zero (k 1))))) 1)
  (begin
    (define $notproc (cons 'not 'proc))
    (not (procedure? $notproc)))
  (eqv? (call/cc (lambda (k) ($notproc (k 1)))) 1)
)


(define eval-test
  (lambda (s)
    (with-output-to-file "testfile.ss"
      (lambda () (display s))
      'replace)
    (parameterize ([#%$suppress-primitive-inlining #f])
      (load "testfile.ss" (lambda (x) (eval x))))
    #t))
(define load-test
  (lambda (s)
    (with-output-to-file "testfile.ss"
      (lambda () (display s))
      'replace)
    (parameterize ([#%$suppress-primitive-inlining #f])
      (load "testfile.ss"))
    #t))
(define compile-test
  (lambda (s)
    (with-output-to-file "testfile.ss"
      (lambda () (display s))
      'replace)
    (parameterize ([#%$suppress-primitive-inlining #f])
      (compile-file "testfile.ss"))
    (load "testfile.so")
    #t))

(define-syntax error/warning-mat
  (syntax-rules ()
    [(_ what string ...)
     (begin
      ; removed primitive argcnt warnings when no source is available
      ; to avoid warnings followed immediately by errors in the repl
      ; and warnings in run-time calls to eval
       #;(mat (what eval-warning) (warning? (eval-test string)) ...)
       (mat (what eval-error) (error? (eval-test string)) ...)
       (mat (what load-warning) (warning? (load-test string)) ...)
       (mat (what load-error) (error? (load-test string)) ...)
       (mat (what compile-warning) (warning? (compile-test string)) ...)
       (mat (what compile-error) (error? (compile-test string)) ...))]))

(define-syntax error-mat
  (syntax-rules ()
    [(_ what string ...)
     (begin
       (mat (what eval-error) (error? (eval-test string)) ...)
       (mat (what load-error) (error? (load-test string)) ...)
       (mat (what compile-error) (error? (compile-test string)) ...))]))

(error/warning-mat argcnt
  "; cp1in argument-count error\n\n(define f (lambda () (import scheme) (car)))\n(f)\n"
  "; cp1in argument-count error\n\n(define f (lambda () (import scheme) (car '(a b) '(c d))))\n(f)\n"
  "; cp1in argument-count error\n\n(define f (lambda () (let ([g (lambda () 0)]) (g 7))))\n(f)\n"
  "; cp1in argument-count error\n\n(define f (lambda () (let ([g (lambda (x) 0)]) (g))))\n(f)\n"
)

(error-mat syntax
  "; eval-when syntax error\n\n(eval-when (compile load eval))"
  "; eval-when syntax error\n\n(eval-when (never) 3)"
  "; begin syntax error\n\n(begin 3 . 4)"
  "; application syntax error\n\n(f 1 2 . 3)"
  "; define syntax error\n\n(define foo 3 4)"
  "; define-syntax syntax error\n\n(define-syntax (foo x y) z)"
  "; cond syntax error\n\n(cond . 17)"
  "; lambda syntax error\n\n(lambda (x 3 y) 3)"
)

(mat sci-bug
  (fl~= (expt 10.0 (- 21)) 1e-21)
  (fl~= (flexpt 10.0 (- 21.0)) 1e-21)
)

(mat apropos
  (error? (apropos 3))
  (error? (apropos '(hit me)))
  (error? (apropos 'a 'b))
  (error? (apropos 'a 'b 'c))
  (error? (apropos))
  (let ([ls (apropos-list 'str)])
    (and (memq 'string=? ls)
         (memq 'display-string ls)
         (memq 'record-constructor ls)
         (not (memq 'cons ls))
         (not (memq 'straightjacket ls))))
  (let ([ls (apropos-list "str")])
    (and (memq 'string=? ls)
         (memq 'display-string ls)
         (memq 'record-constructor ls)
         (not (memq 'cons ls))
         (not (memq 'straightjacket ls))))
  (equal?
    (with-output-to-string (lambda () (apropos 'substring)))
    "interaction environment:\n  substring, substring-fill!\n(chezscheme):\n  substring, substring-fill!\n(rnrs):\n  substring\n(rnrs base):\n  substring\n(scheme):\n  substring, substring-fill!\n")
  (equal?
    (with-output-to-string (lambda () (apropos "substring")))
    "interaction environment:\n  substring, substring-fill!\n(chezscheme):\n  substring, substring-fill!\n(rnrs):\n  substring\n(rnrs base):\n  substring\n(scheme):\n  substring, substring-fill!\n")
  (equal?
    (with-output-to-string (lambda () (apropos 'substring (copy-environment (scheme-environment) #t '(substring-fill!)))))
    "supplied environment:\n  substring-fill!\n(chezscheme):\n  substring, substring-fill!\n(rnrs):\n  substring\n(rnrs base):\n  substring\n(scheme):\n  substring, substring-fill!\n")
  (null? (apropos-list 'thisshouldntbefound))
  (equal?
    (apropos-list 'apropos)
    '(apropos apropos-list
      ((chezscheme) apropos apropos-list)
      ((scheme) apropos apropos-list)))
  (equal? (apropos-list '$apropos-unbound1) '())
  (error? (eval '$apropos-unbound1))
  (equal? (apropos-list '$apropos-unbound1) '())
  (equal? (apropos-list '$apropos-bound1) '())
  (eq? (eval '(set! $apropos-bound1 17)) (void))
  (equal? (apropos-list '$apropos-bound1) '($apropos-bound1))
  (begin (define $apropos-env (copy-environment (scheme-environment)))
         (environment? $apropos-env))
  (equal? (apropos-list '$apropos-unbound2 $apropos-env) '())
  (error? (eval '$apropos-unbound2 $apropos-env))
  (equal? (apropos-list '$apropos-unbound2 $apropos-env) '())
  (equal? (apropos-list '$apropos-bound2 $apropos-env) '())
  (eq? (eval '(set! $apropos-bound2 17) $apropos-env) (void))
  (equal? (apropos-list '$apropos-bound2 $apropos-env) '($apropos-bound2))
)

(mat p423 ; tests for p423 compiler
  (equal?
    (list
      '()
      75
      (- 2 4)
      (* -6 7)
      (cons 0 '())
      (cons (cons 0 '()) (cons 1 '()))
      (cdr (cons 16 32))
      (void)
      (if #f 3)
      (let () 3)
      (let ((x 0)) x)
      (let ([x 0]) x x)
      (let ([x 17]) (+ x x))
      (let ([q (add1 (add1 2))]) q)
      (+ 20 (if #t 122))
      (let ((x 16)
            (y 128))
        (* x y))
      (if #t
         (+ 20
           (if #t 122))
         10000)
      (let ([x 3])
        (let ([y (+ x (quote 4))])
          (+ x y)))
      (let ((x '(#(1 2 (3 #(4))) #() 3 #t))) x)
      (not (if #f #t (not #f)))
      (let ([x 0] [y 4000]) x)
      (let ((x (cons 16 32))) (pair? x))
      (begin (if #f 7) 3)
      (begin (< 1 2) 3)
      (begin '(1 . 2) 3)
      (begin (if (zero? 4) 7) 3)
      (let ([x 0]) (begin (if (zero? x) 7) x))
      (let ([x 0]) (begin (if (zero? x) (begin x 7)) x))
      (let ([x 0] [z 9000])
         (begin (if (zero? x) (begin x 7)) z))
      (let ([x 0] [z 9000])
         (begin (if (zero? x) (begin (set! x x) 7))
           (+ x z)))
      (let ([x 4]) (begin (+ (begin (set! x 17) 3) 4) x))
      (let ([x (cons 0 '())])
         (begin (if x (set-car! x (car x))) x))
      (let ([x (cons 0 '())])
         (begin (if x (set-car! x (+ (car x) (car x)))) x))
      (let ([x (cons 0 '())])
         (if (zero? (car x)) (begin (set-car! x x) 7) x))
      (let ([x (cons 0 '())])
         (let ([q x]) (if (zero? (car x)) (begin (set-car! q x) 7) x)))
      (let ([x 0]) (if (zero? x) (begin (set! x (+ x 5000)) x) 20))
      (let ([y 0]) (begin (if #t (set! y y)) y))
      (begin (if #t #t #t) #f)
      (begin (if (if #t #t #f) (if #t #t #f) (if #t #t #f)) #f)
      (let
         ([x 0]
          [y 4000]
          [z 9000])
         (let ((q (+ x z)))
           (begin
             (if (zero? x) (begin (set! q (+ x x)) 7))
             (+ y y)
             (+ x z))))
      (let ([x (let ([y 2]) y)]
             [y 5])
         (add1 x))
      (let ([y 4000]) (+ y y))
      ((lambda (y) y) 4000)
      (let ([f (lambda (x) x)])
         (add1 (f 0)))
      (let ([f (lambda (y) y)]) (f (f 4)))
      ((lambda (f) (f (f 4))) (lambda (y) y))
      ((let ([a 4000])
          (lambda (b) (+ a b)))
        5000)
      (((lambda (a)
           (lambda (b)
             (+ a b)))
         4000)
        5000)
      (let ([f (lambda (x) (add1 x))]) (f (f 0)))
      ((lambda (f) (f (f 0))) (lambda (x) (add1 x)))
      (let ([x 0] [f (lambda (x) x)])
         (let ([a (f x)] [b (f x)] [c (f x)]) (+ (+ a b) c)))
      (let ([x 0] [y 1] [z 2] [f (lambda (x) x)])
         (let ([a (f x)] [b (f y)] [c (f z)])
           (+ (+ a b) c)))
      (let ([f (lambda (x y) x)])
         (f 0 1))
      (let ([f (lambda (x y) x)])
         (let ([a (f 0 1)]) (f a a)))
      (let ([x 0] [y 1] [z 2] [f (lambda (x y z) x)])
         (let ([a (f x y z)]) (f a a a)))
      (let ([x 0] [y 1] [z 2] [f (lambda (x y z) x)])
         (let ([a (f x y z)] [b y] [c z]) (f a b c)))
      (let ([f (lambda (a b c d)
                  (+ a d))])
         (f 0 1 2 3))
      (let ([f (lambda (x) x)])
         (+ (f 0)
           (let ([a 0] [b 1] [c 2])
             (+ (f a) (+ (f b) (f c))))))
      (let ([f (lambda (x) x)])
         (+ (f 0)
           (let ([a 0] [b 1] [c 2])
             (add1 (f a)))))
      (let ([f (lambda (x) x)])
        (let ([a 1])
          (* (+ (f a) a) a)))

      (let ([k (lambda (x y) x)])
        (let ([b 17])
          ((k (k k 37) 37) b (* b b))))

      (let ([f (lambda ()
                 (let ([n 256])
                    (let ([v (make-vector n)])
                     (vector-set! v 32 n)
                     (vector-ref v 32))))])
        (pair? (f)))
      (let ((w 4) (x 8) (y 16) (z 32))
        (let ((f (lambda ()
                   (+ w (+ x (+ y z))))))
          (f)))
      (let ([f (lambda (x) x)])
         (+ (f 0) (let ([a 0] [b 1] [c 2] [d 3])
                    (+ (f a)
                      (+ (f b)
                        (+ (f c)
                          (f d)))))))
     ; test use of keywords/primitives as variables
      (let ([quote (lambda (x) x)]
            [let (lambda (x y) (- y x))]
            [if (lambda (x y z) (cons x z))]
            [cons (lambda (x y) (cons y x))]
            [+ 16])
        (set! + (* 16 2))
        (cons (let ((quote (lambda () 0))) +)
              (if (quote (not #f))
                  720000
                  -1)))
      (letrec () 3)
      (let ([a 0]) (letrec ([a (lambda () 0)] [b (lambda () 11)]) (set! a 11)))
      (let ([a 0]) (letrec ([a (lambda () (set! a 0))] [b 11]) (a)))
      (let ([a 0]) (let ([a (set! a 0)] [b 11]) a))
      (let ([a 5]) (let ([a 0] [b (set! a (+ a 11))]) a))
      (let ([x (lambda () 4)])
        (letrec ([y (lambda () (z))] [z x]) (y)))
      (letrec ([a (lambda () 0)]) (a))
      (letrec ([a (lambda () 0)] [b (lambda () 11)]) (a))
      (let ([z 4])
        (letrec ([f (lambda (x)
                      (letrec ([g (lambda (y)
                                    (if (= y 0) 0
                                        (f (- y 1))))])
                        (g x)))])
          (f z)))
      (let ([x 0]) (letrec ([a (lambda () 0)] [b (lambda () 11)]) (set! x 11)))
      (let ([a 0]) (let ([b (set! a 0)]) a))
      (let ([a 0]) (let ([a (set! a 0)]) (let ([b 11]) a)))
      (let ([a 0]) (let ([a 0]) (let ([b (set! a 11)]) a)))
      (let ([a 0]) (let ([a 0]) (let ([b 11]) (set! a 11))))
      (let ([f (let ([x 1]) (lambda (y) (+ x y)))])
         (let ([x 0]) (f (f x))))
      ((let ([t (lambda (x) (+ x 50))])
          (lambda (f) (t (f 1000))))
        (lambda (y) (+ y 2000)))
      (let ([x 0])
         (let ([f (let ([x 1]
                        [z x])
                    (lambda (y)
                      (+ x (+ z y))))])
           (f (f x))))
      (((lambda (t)
           (lambda (f) (t (f 1000))))
         (lambda (x) (+ x 50)))
        (lambda (y) (+ y 2000)))
      ((let ([t 50])
          (lambda (f)
            (+ t (f))))
        (lambda () 2000))
      (((lambda (t)
           (lambda (f)
             (+ t (f))))
         50)
        (lambda () 2000))
      ((let ([x 300])
          (lambda (y) (+ x y)))
        400)
      (let ([x 3] [f (lambda (x y) x)])
         (f (f 0 0) x))
      (let ([x 3] [f (lambda (x y) x)])
         (if (f 0 0) (f (f 0 0) x) 0))
      (let ([x02 3] [f01 (lambda (x04 y03) x04)])
         (if (not x02) (f01 (f01 0 0) x02) 0))
      (let ((f (lambda (x) (if (if (pair? x) (not (eq? (car x) 0)) #f) x #f))))
        (f (cons 0 0)))
      (let ((f (lambda (x)
                 (if (if x (not (if (pair? x) (not (eq? (car x) 0)) #f)) #f)
                     x #f))))
        (f 0))
      (let ((f (lambda (x) (if (if (pair? x) #t (null? x)) x '()))))
        (f 0))
      (let ([y 4])
         (let ([f (lambda (y) y)])
           (f (f y))))
      (let ([y 4])
         (let ([f (lambda (x y) 0)])
           (f (f y y) (f y y))))
      (let ([y 4])
         (let ([f (lambda (x y) 0)])
           (f (f y y) (f y (f y y)))))
      (let ([y 4])
         (let ([f (lambda (x y) 0)])
           (f (f y (f y y)) (f y (f y y)))))
      ((lambda (y) ((lambda (f) (f (f y))) (lambda (y) y))) 4)
      (let ([f (lambda (x) (+ x x))]) (f 4000))
      (let ((x (if 1000 2000 3000)))
         x)
      (let ([f (lambda (x) x)])
         (add1 (if #f 1 (f 22))))
      (let ([f (lambda (x) x)])
         (if (f (zero? 23)) 1 22))
      (let ([f (lambda (x) (if x (not x) x))]
             [f2 (lambda (x) (* 10 x))]
             [x 23])
         (add1 (if (f (zero? x)) 1 (* x (f2 (sub1 x))))))
      (let ([f (lambda () 0)])
         (let ([x (f)])
           1))
      (let ([f (lambda () 0)])
         (begin (f) 1))
      (let ([f (lambda (x) x)])
         (if #t (begin (f 3) 4) 5))
      (let ([f (lambda (x) x)])
         (begin (if #t (f 4) 5) 6))
      (let ([f (lambda (x) x)])
         (begin
           (if (f #t)
             (begin
               (f 3)
               (f 4))
             (f 5))
           (f 6)))
      (let ([f (lambda (x) (add1 x))])
         (f (let ([f 3]) (+ f 1))))
      (let ((x 15)
             (f (lambda (h v) (* h v)))
             (k (lambda (x) (+ x 5)))
             (g (lambda (x) (add1 x))))
         (k (g (let ((g 3)) (f g x)))))
      (let ([x 4])
         (let ([f (lambda () x)])
           (set! x 5)
           (f)))
      (let ([x (let ([y 2])
                  y)])
         x)
      (let ([x (if #t (let ([y 2])
                         y)
                  1)])
         x)
      (let ([x (let ([y (let ([z 3])
                           z)])
                  y)])
         x)
      (let ([x (if #t (let ([y (if #t (let ([z 3])
                                         z)
                                  2)])
                         y)
                  1)])
         x)
      (+ (let ([x 3])
            (add1 x))
         4)
      (+ (let ([x 3] [y 4])
            (* x y))
         4)
      (let ([x (add1 (let ([y 4]) y))]) x)
      (let ([x (add1 (letrec ([y (lambda () 4)]) (y)))]) x)
      (let ([x (+ (let ([y 4]) y)  (let ([y 4]) y))]) (add1 x))
      (let ([z 0])
         (let ([x z])
           z
           x))
      (let ([z 0])
         (let ([x (begin (let ([y 2]) (set! z y)) z)])
           x))
      (let ([x (begin (let ([y 2]) (set! y y)) (let ([z 3]) z))])
         x)
      (letrec ([one (lambda (n) (if (zero? n) 1 (one (sub1 n))))])
         (one 13))
      (letrec
         ((even (lambda (x) (if (zero? x) #t (odd (sub1 x)))))
          (odd (lambda (x) (if (zero? x) #f (even (sub1 x))))))
         (odd 13))
      (let ([t #t]
             [f #f])
         (letrec
           ((even (lambda (x) (if (zero? x) t (odd (sub1 x)))))
            (odd (lambda (x) (if (zero? x) f (even (sub1 x))))))
           (odd 13)))
      (let ((even (lambda (x) x)))
         (even
           (letrec
             ((even (lambda (x) (if (zero? x) #t (odd (sub1 x)))))
              (odd (lambda (x) (if (zero? x) #f (even (sub1 x))))))
             (odd 13))))
      (letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n)))))))
         (fact 5))
      (letrec ([remq (lambda (x ls)
                       (if (null? ls)
                           '()
                           (if (eq? (car ls) x)
                               (remq x (cdr ls))
                               (cons (car ls) (remq x (cdr ls))))))])
        (remq 3 '(3 1 3)))
      (let ([x 5])
         (letrec
           ([a
              (lambda (u v w) (if (zero? u) (b v w) (a (- u 1) v w)))]
            [b
              (lambda (q r)
                (let ([p (* q r)])
                  (letrec
                    ([e (lambda (n) (if (zero? n) (c p) (o (- n 1))))]
                     [o (lambda (n) (if (zero? n) (c x) (e (- n 1))))])
                    (e (* q r)))))]
            [c (lambda (x) (* 5 x))])
           (a 3 2 1)))
      (let ([f (lambda () 80)])
         (let ([a (f)] [b (f)])
           0))
      (let ([f (lambda () 80)])
         (let ([a (f)] [b (f)])
           (* a b)))
      (let ([f (lambda () 80)]
             [g (lambda () 80)])
         (let ([a (f)] [b (g)])
           (* a b)))
      (let ((f (lambda (x) (add1 x)))
             (g (lambda (x) (sub1 x)))
             (t (lambda (x) (add1 x)))
             (j (lambda (x) (add1 x)))
             (i (lambda (x) (add1 x)))
             (h (lambda (x) (add1 x)))
             (x 80))
         (let ((a (f x)) (b (g x)) (c (h (i (j (t x))))))
           (* a (* b (+ c 0)))))
      (let ((x 3000))
         (if (integer? x)
           (let ((y (cons x '())))
             (if (if (pair? y) (null? (cdr y)) #f)
               (+ x 5000)
               (- x 3000)))))
      (let ((x (cons 1000 2000)))
         (if (pair? x)
           (let ((temp (car x)))
             (set-car! x (cdr x))
             (set-cdr! x temp)
             (+ (car x) (cdr x)))
           10000000))
      (let ((v (make-vector 3)))
         (vector-set! v 0 10)
         (vector-set! v 1 20)
         (vector-set! v 2 30)
         (if (vector? v)
           (+ (+ (vector-length v) (vector-ref v 0))
             (+ (vector-ref v 1) (vector-ref v 2)))
           10000))
      (let ([fact
               (lambda (fact n)
                 (if (zero? n) 1 (* (fact fact (sub1 n)) n)))])
         (fact fact 5))
      (let ([f (lambda (x) (+ x 1000))])
         (if (zero? (f -2)) (f 6000) (f (f 8000))))
      (let ([f (lambda (x) (+ x 1000))])
         (if (zero? (f -1)) (f 6000) (f (f 8000))))
      (let ((f (lambda (x y) (+ x 1000))))
         (+ (if (f 3000 (begin 0 0 0)) (f (f 4000 0) 0) 8000) 2000))
      ((((lambda (x)
            (lambda (y)
              (lambda (z)
                (+ x (+ y (+ z y))))))
          5) 6) 7)
      ((((((lambda (x)
              (lambda (y)
                (lambda (z)
                  (lambda (w)
                    (lambda (u)
                      (+ x (+ y (+ z (+ w u)))))))))
            5) 6) 7) 8) 9)
      (let ((f (lambda (x) x)))
         (if (procedure? f)
           #t
           #f))
      (let ((sum (lambda (sum ls)
                    (if (null? ls)
                      0
                      (+ (car ls) (sum sum (cdr ls)))))))
         (sum sum (cons 1 (cons 2 (cons 3 '())))))
      (let ((v (make-vector 5))
             (w (make-vector 7)))
         (vector-set! v 0 #t)
         (vector-set! w 3 #t)
         (if (boolean? (vector-ref v 0))
           (vector-ref w 3)
           #f))
      (let ((a 5) (b 4))
         (if (< b 3)
           (eq? a (+ b 1))
           (if (<= b 3)
             (eq? (- a 1) b)
             (= a (+ b 2)))))
      (let ((a 5) (b 4))
         (if #f
           (eq? a (+ b 1))
           (if #f
             (eq? (- a 1) b)
             (= a (+ b 2)))))
      (((lambda (a)
           (lambda ()
             (+ a (if #t 200))
             1500))
         1000))
      (((lambda (b)
           (lambda (a) (set! a (if 1 2)) (+ a b)))
         100)
        200)
      ((((lambda (a)
            (lambda (b)
              (set! a (if b 200))
              (lambda (c)
                (set! c (if 300 400))
                (+ a (+ b c)))))
          1000)
         2000)
        3000)
      ((((lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))) 10) 20) 30)
      (+ 2 3)
      ((lambda (a) (+ 2 a)) 3)
      (((lambda (b) (lambda (a) (+ b a))) 3) 2)
      ((lambda (b) ((lambda (a) (+ b a)) 2)) 3)
      ((lambda (f) (f (f 5))) (lambda (x) x))
      ((let ((f (lambda (x) (+ x 3000))))
          (lambda (y) (f (f y))))
        2000)
      (let ((n 17) (s 18) (t 19))
         (let ((st (make-vector 5)))
           (vector-set! st 0 n)
           (vector-set! st 1 s)
           (vector-set! st 2 t)
           (if (not (vector? st))
             10000
             (vector-length st))))
      (let ((s (make-vector 1)))
         (vector-set! s 0 82)
         (if (eq? (vector-ref s 0) 82) 1000 2000))
      (not 17)
      (not #f)
      (let ([fact
               (lambda (fact n acc)
                 (if (zero? n) acc (fact fact (sub1 n) (* n acc))))])
         (fact fact 5 1))
      ((lambda (b c a)
          (let ((b (+ b a))
                (a (+ a (let ((a (+ b b))
                              (c (+ c c)))
                          (+ a a)))))
            (* a a)))
        2 3 4)
      (let ((f (lambda (x) (lambda () (x))))) ((f (lambda () 3))))
      (letrec ((f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))))
         (let ([q 17])
           (let ((g (lambda (a) (set! q 10) (lambda () (a q)))))
             ((g f)))))
      (letrec ((f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))))
         (let ((g (lambda (a) (lambda (b) (a b)))))
           ((g f) 10)))
      (letrec ((f (lambda () (+ a b)))
               (g (lambda (y) (set! g (lambda (y) y)) (+ y y)))
               (a 17)
               (b 35)
               (h (cons (lambda () a) (lambda (v) (set! a v)))))
         (let ((x1 (f)) (x2 (g 22)) (x3 ((car h))))
           (let ((x4 (g 22)))
             ((cdr h) 3)
             (let ((x5 (f)) (x6 ((car h))))
               (cons x1 (cons x2 (cons x3 (cons x4 (cons x5 x6)))))))))
      (letrec ((f (lambda () (+ a b)))
               (a 17)
               (b 35)
               (h (cons (lambda () a) (lambda () b))))
         (cons (f) (cons a (cons b (cons ((car h)) ((cdr h)))))))
      (letrec ((f (lambda (x)
                     (letrec ((x 3)) 3))))
         (letrec ((g (lambda (x) (letrec ((y 14)) (set! y 7) y))))
           (set! g (cons g 3))
           (letrec ((h (lambda (x) x)) (z 42))
             (cons (cdr g) (h z)))))
      (let ([t #t] [f #f])
         (let ([bools (cons t f)] [id (lambda (x) (if (not x) f t))])
           (letrec
             ([even (lambda (x) (if (zero? x) (id (car bools)) (odd (- x 1))))]
              [odd (lambda (y) (if (zero? y) (id (cdr bools)) (even (- y 1))))])
             (odd 5))))
      (letrec ([fib (lambda (x)
                      (let ([decrx (lambda () (set! x (- x 1)))])
                        (if (< x 2)
                            1
                            (+ (begin (decrx) (fib x))
                               (begin (decrx) (fib x))))))])
        (fib 10))
      (letrec ([fib (lambda (x)
                      (let ([decrx (lambda () (lambda (i) (set! x (- x i))))])
                        (if (< x 2)
                            1
                            (+ (begin ((decrx) 1) (fib x))
                               (begin ((decrx) 1) (fib x))))))])
        (fib 10))
      (let ((f (lambda (g u) (g (if u (g 37) u)))))
        (f (lambda (x) x) 75))

      (let ((f (lambda (h u) (h (if u (h (+ u 37)) u))))
            (w 62))
        (f (lambda (x) (- w x)) (* 75 w)))

      (let ([t #t] [f #f])
        (let ([bools (cons t f)] [id (lambda (x) (if (not x) f t))])
          (letrec
            ([even (lambda (x) (if (id (zero? x)) (car bools) (odd (- x 1))))]
             [odd (lambda (y) (if (zero? y) (id (cdr bools)) (even (- y 1))))])
            (odd 5))))

      ((lambda (x y z)
         (let  ((f (lambda (u v) (begin (set! x u) (+ x v))))
                (g (lambda (r s) (begin (set! y (+ z s)) y))))
           (* (f '1 '2) (g '3 '4))))
       '10 '11 '12)

      ((lambda (x y z)
         (let ((f '#f)
               (g (lambda (r s) (begin (set! y (+ z s)) y))))
           (begin
             (set! f
               (lambda (u v) (begin (set! v u) (+ x v))))
             (* (f '1 '2) (g '3 '4)))))
       '10 '11 '12)

      (letrec ((f (lambda (x) (+ x 1)))
               (g (lambda (y) (f (f y)))))
        (+ (f 1) (g 1)))

      (let ((y 3))
        (letrec
          ((f (lambda (x) (if (zero? x) (g (+ x 1)) (f (- x y)))))
           (g (lambda (x) (h (* x x))))
           (h (lambda (x) x)))
          (g 39)))

      (letrec ((f (lambda (x) (+ x 1)))
               (g (lambda (y) (f (f y)))))
        (set! f (lambda (x) (- x 1)))
        (+ (f 1) (g 1)))

      (letrec ([f (lambda () (+ a b))]
               [a 17]
               [b 35]
               [h (cons (lambda () a) (lambda () b))])
        (cons (f) (cons a (cons b (cons ((car h)) ((cdr h)))))))

      (let ((v (make-vector 8)))
        (vector-set! v 0 '())
        (vector-set! v 1 (void))
        (vector-set! v 2 #f)
        (vector-set! v 3 (cons 3 4))
        (vector-set! v 4 (make-vector 3))
        (vector-set! v 5 #t)
        (vector-set! v 6 2)
        (vector-set! v 7 5)
        (vector-ref v (vector-ref v 6)))

      (let ([x 5] [th (let ((a 1)) (lambda () a))])
        (letrec ([fact (lambda (n th)
                         (if (zero? n)
                             (th)
                             (* n (fact (- n 1) th))))])
          (fact x th)))

      (let ([negative? (lambda (n) (< n 0))])
        (letrec
          ([fact
             (lambda (n)
               (if (zero? n)
                   1
                   (* n (fact (- n 1)))))]
           [call-fact
             (lambda (n)
               (if (not (negative? n))
                   (fact n)
                   (- 0 (fact (- 0 n)))))])
          (cons (call-fact 5) (call-fact -5))))

      (letrec ([iota-fill!
                (lambda (v i n)
                  (if (not (= i n))
                      (begin
                        (vector-set! v i i)
                        (iota-fill! v (+ i 1) n))))])
        (let ([n 4])
          (let ([v (make-vector n)])
            (iota-fill! v 0 n)
            v)))

    ; try with operand-constraints reg/int? returning false for ints
    ; to make sure that nested operands are being pulled out properly
      (let ((f (lambda (x) x)))
        (let ((g (lambda (x) (let ((y (+ x x))) (f x) (cons x y)))))
          (g 3)))

    ; nested test examples
      (+ (let ((x 7) (y 2)) (if (if (= x 7) (< y 0) (<= 0 y)) 77 88)) 99)
      (+ (let ((x 7) (y -22)) (if (if (= x 7) (< y 0) (<= 0 y)) 77 88)) 99)
      (+ (let ((x 8) (y 2)) (if (if (= x 7) (< y 0) (<= 0 y)) 77 88)) 99)
      (+ (let ((x 8) (y -22)) (if (if (= x 7) (< y 0) (<= 0 y)) 77 88)) 99)

    ; make-vector with non-constant operand and improper alignment
      (let ([x 6])
        (let ([v (make-vector x)])
          (vector-set! v 0 3)
          (vector-set! v 1 (cons (vector-ref v 0) 2))
          (vector-set! v 2 (cons (vector-ref v 1) 2))
          (vector-set! v 3 (cons (vector-ref v 2) 2))
          (vector-set! v 4 (cons (vector-ref v 3) 2))
          (vector-set! v 5 (cons (vector-ref v 4) 2))
          (cons (pair? (vector-ref v 5)) (car (vector-ref v 4)))))

    ; nest some lambdas
      (((((lambda (a)
            (lambda (b)
              (lambda (c)
                (lambda (d)
                  (cons (cons a b) (cons c d))))))
           33) 55) 77) 99)

    ; test set! on letrec rhs
     (letrec ([b 4])
       (letrec ([a (lambda (x) (set! a x) 5)])
         (a (lambda (x) x))
         (set! b 8)
         (a 7)))

    ; test optimize-letrec---contributed by Jeremiah Penery
     (letrec ([q (cons (lambda (x)
                         (letrec ([b r])
                           b))
                       '())]
              [r 10])
       ((car q) 5))

    ; normalize-context test a bit---contributed by Andy Keep
     (let ((x 5)) (if (set! x 6) 1 0) x)

    ; stress the register allocator
      (let ((a 17))
        (let ((f (lambda (x)
                   (let ((x1 (+ x 1)) (x2 (+ x 2)))
                     (let ((y1 (* x1 7)) (y2 (* x2 7)))
                       (let ((z1 (- y1 x1)) (z2 (- y2 x2)))
                         (let ((w1 (* z1 a)) (w2 (* z2 a)))
                           (let ([g (lambda (b)
                                      (if (= b a)
                                          (cons x1 (cons y1 (cons z1 '())))
                                          (cons x2 (cons y2 (cons z2 '())))))]
                                 [h (lambda (c)
                                      (if (= c x) w1 w2))])
                             (if (if (= (* x x) (+ x x))
                                     #t
                                     (< x 0))
                                 (cons (g 17) (g 16))
                                 (cons (h x) (h (- x 0))))))))))))
          (cons (f 2) (cons (f -1) (cons (f 3) '())))))

      (let ([x (cons #f #t)] [y 17])
        (if (if (car x) #t (< y 20))
            (* y (* y 2))
            (void)))
      (let ((v (make-vector (add1 37))))
        (vector-set! v 0 (boolean? v))
        (vector-set! v (* 3 11) (vector-length v))
        ((let ((w (cons 33 '())))
          (lambda ()
            (if (not (eq? w (cons 33 '())))
                (begin
                  (set-cdr! w (vector? v))
                  w))))))
      (let ((v (make-vector (add1 37))))
        (vector-set! v 0 (boolean? v))
        (vector-set! v (* 3 11) #t)
        ((let ((w (cons (sub1 34) #f)))
          (lambda ()
            (set-cdr! w v)
            (if (not (eq? w (cons (- (vector-length v) 5) v)))
                (begin
                  (set-car! w (vector-ref (cdr w) (car w)))
                  w))))))

     ; make sure uncover-live passes don't leave behind unassigned
     ; or unlisted variables as a result of dead code.
      (letrec ([a (lambda () 1)])
        (let ([b 2])
          (if #t
              3
              (begin (a) b))))

     ; stress test introduce-unspillables by generating
     ; (mset fp i (+ (mref fp j) (mref fp k)))
      (let ((f (lambda (x) x)))
        (let ((x 1) (y 2))
          (let ((z (f x)))
            (let ((w (+ x y)))
              (let ((q (f w)))
                w)))))

     ; stress test introduce-unspillables by generating
     ; (mset (mref fp i) tmp (mref fp k))---can't actually get
     ; (mset (mref fp i) (mref fp j) (mref fp k)), 'cause we
     ; have to add in the vector-data offset
      (let ((f (lambda (x) x)))
        (let ((x (make-vector 4)) (y 2) (z 17))
          (vector-set! x y z)
          (let ((w (f x)))
            (cons (+ y z) x))))
      (letrec ([s0 (lambda (a b c d e)
                     (if (null? a)
                         (cons b (cons c (cons d e)))
                         (if (eq? (car a) #t)
                             (s1 (cdr a) (+ b 1) c d e)
                             (s2 (cdr a) b (+ c 1) d e))))]
               [s1 (lambda (a b c d e)
                     (if (eq? (car a) #t)
                         (s0 (cdr a) b c (+ d 1) e)
                         (s1 (cdr a) b c d (+ e 1))))]
               [s2 (lambda (a b c d e)
                     (if (eq? (car a) #t)
                         (s0 (cdr a) (+ b 1) d c e)
                         (s2 (cdr a) e d b c)))])
        (s0 '(#t #f #t #f #t #f #f #f #f #t) 10 20 30 40))

     ; stress optimize-letrec.  in the outer letrec, q should be treated as
     ; 'lambda'.  in the inner letrec, f should be treated as simple,
     ; d as 'lambda', and a, b, c, and e as complex.
     ; should evaluate to ((40 #f 105 15 . #t) #t 252 36 #t 9841 . 18)
      (letrec ((q (lambda (x) (if (< x 1) 13 (+ (* (q (- x 2)) 3) 1)))))
        (letrec ((a (lambda (x) x))
                 (b (cons (lambda () (* c 7)) (lambda (v) (set! c v))))
                 (c 15)
                 (d (lambda (x) (set! a x) (a x)))
                 (e (q 12))
                 (f 18))
          (let ([a0 (a #f)] [b0 ((car b))] [c0 c])
            (let ([d0 (d (lambda (z) #t))])
              ((cdr b) (* f 2))
              (cons (cons (q 1) (cons a0 (cons b0 (cons c0 d0))))
                    (cons (a #f)
                          (cons ((car b))
                                (cons c (cons (procedure? d) (cons e f))))))))))

      ;; Jie Li
      (let ((a 5))
        (let ((b (cons a 6)))
          (let ((f (lambda(x) (* x a))))
           (begin (if (- (f a) (car b))
                      (begin (set-car! b
                                       (if (not a) (* 2 a) (+ 2 a)))
                             (f a))
                      (if (not (not (< (f a) b)))
                          (f a)))
                  (not 3)
                  (void)
                  (f (car b))))))
      (letrec ([f (lambda (x y) (if (not x) (g (add1 x) (add1 y)) (h (+ x y))))]
               [g (lambda (u v)
                    (let ([a (+ u v)]
                          [b (* u v)])
                      (letrec ([e (lambda (d)
                                    (letrec ([p (cons a b)]
                                             [q (lambda (m)
                                                  (if (< m u)
                                                      (f m d)
                                                      (h (car p))))])
                                      (q (f a b))))])
                        (e u))))]
               [h (lambda (w) w)])
        (f 4 5))
      (letrec ((f (lambda (x)
                    (+ x (((lambda (y)
                             (lambda (z)
                               (+ y z)))
                           6)7))))
               (g (+ 5 ((lambda (w u) (+ w u)) 8 9))))
        g)
      ;; Jordan Johnson
      (let ((test (if (not (not 10)) #f 5)))
        (letrec ([num 5]
                 [length
                  (lambda (ls)
                    (let ((len (if ((lambda (ck) (begin ck (set! num test) ck))
                                    (null? ls))
                                   (begin num (set! num 0) num)
                                   (begin (length '())
                                          (set! num 5)
                                          (+ 1 (length (cdr ls)))))))
                      (if len len)))])
          (length (cons 5 (cons (if (set! num 50) (length (cons test '())) 1)
                                '())))))
      (letrec ([quotient (lambda (x y)
                           (if (< x 0)
                               (- 0 (quotient (- 0 x) y))
                               (if (< y 0)
                                   (- 0 (quotient x (- 0 y)))
                                   (letrec ([f (lambda (x a)
                                                 (if (< x y)
                                                     a
                                                     (f (- x y) (+ a 1))))])
                                     (f x 0)))))])
        (letrec ([sub-interval 1]
                 [sub-and-continue
                  (lambda (n acc k) (k (- n sub-interval) (* n acc)))]
                 [strange-fact
                  (lambda (n acc)
                    (if (zero? n)
                        (lambda (proc) (proc acc))
                        (sub-and-continue n acc strange-fact)))])
          (let ([x 20]
                [fact (let ((seed 1)) (lambda (n) (strange-fact n seed)))])
            (let ([give-fact5-answer (fact 5)]
                  [give-fact6-answer (fact 6)]
                  [answer-user (lambda (ans) (quotient ans x))])
              (set! x (give-fact5-answer answer-user))
              (begin (set! x (give-fact6-answer answer-user))
                     x)))))
      (let ((y '())
            (z 10))
        (let ((test-ls (cons 5 y)))
          (set! y (lambda (f)
                    ((lambda (g) (f (lambda (x) ((g g) x))))
                     (lambda (g) (f (lambda (x) ((g g) x)))))))
          (set! test-ls (cons z test-ls))
          (letrec ((length (lambda (ls)
                              (if (null? ls) 0 (+ 1 (length (cdr ls)))))))
            (let ((len (length test-ls)))
              (eq? (begin
                    (set! length (y (lambda (len)
                                      (lambda (ls)
                                        (if (null? ls)
                                            0
                                            (+ 1 (len (cdr ls))))))))
                    (length test-ls))
                   len)))))
      ;; Ryan Newton
      (letrec
        ((loop
           (lambda ()
             (lambda ()
               (loop)))))
        (loop)
        0)
      (letrec ([f (lambda ()
                    (letrec ([loop
                               (lambda (link)
                                 (lambda ()
                                   (link)))])
                      (loop (lambda () 668))))])
        ((f)))
      (if (lambda () 1)
          (let ((a 2))
            (if (if ((lambda (x)
                       (let ((x (set! a (set! a 1))))
                         x)) 1)
                    (if (eq? a (void))
                        #t
                        #f)
                    #f)
                #36rgood        ; dyb: cannot use symbols, so use radix 36
                #36rbad)))      ; syntax to make all letters digits

     ; contributed by Ryan Newton
      (letrec
         (
           [dropsearch
             (lambda (cell tree)
               (letrec
                 ([create-link
                    (lambda (node f)
                      (lambda (g)
                        (if (not (pair? node))
                            (f g)
                            (if (eq? node cell)
                                #f
                                (f (create-link (car node)
                                                (create-link (cdr node) g)))))))]
                  [loop
                    (lambda (link)
                      (lambda ()
                        (if link
                            (loop (link (lambda (v) v)))
                            #f)))])
                 (loop (create-link tree (lambda (x) x)))
                 ))]

           [racethunks
             (lambda (thunkx thunky)
               (if (if thunkx thunky #f)
                   (racethunks (thunkx) (thunky))
                   (if thunky
                       #t
                       (if thunkx
                           #f
                           '()))))]

           [higher?
             (lambda (x y tree)
               (racethunks (dropsearch x tree)
                           (dropsearch y tree)))]

           [under?
             (lambda (x y tree)
               (racethunks (dropsearch x y)
                           (dropsearch x tree)))]

           [explore
             (lambda (x y tree)
               (if (not (pair? y))
                   #t
                   (if (eq? x y)
                       #f    ;This will take out anything that points to itself
                       (let ((result (higher? x y tree)))
                         (if (eq? result #t)
                             (if (explore y (car y) tree)
                                 (explore y (cdr y) tree)
                                 #f)
                             (if (eq? result #f)
                                 (process-vertical-jump x y tree)
                                 (if (eq? result '())
                                     (process-horizontal-jump x y tree)
                                     )))))))]

           [process-vertical-jump
             (lambda (jumpedfrom jumpedto tree)
               (if
                 (under? jumpedfrom jumpedto tree)
                 #f
                 (fullfinite? jumpedto)))]

           [process-horizontal-jump
             (lambda (jumpedfrom jumpedto tree)
               (fullfinite? jumpedto))]

           [fullfinite?
             (lambda (pair)
               (if (not (pair? pair))
                   #t
                   (if (explore pair (car pair) pair)
                       (explore pair (cdr pair) pair)
                       #f)))])
         (cons
           (fullfinite? (cons 1 2))
           (cons
             (fullfinite? (let ((x (cons 1 2))) (set-car! x x) x))
             (cons
               (fullfinite? (let ([a (cons 0 0)] [b (cons 0 0)] [c (cons 0 0)])
                              (set-car! a b) (set-cdr! a c) (set-cdr! b c)
                              (set-car! b c) (set-car! c b) (set-cdr! c b) a))
               '())))))
    `(() 75 -2 -42 (0) ((0) 1) 32 ,(void) ,(void) 3 0 0 34 4
      142 2048 142 10 (#3(1 2 (3 #1(4))) #0() 3 #t) #f 0 #t 3
      3 3 3 0 0 9000 9000 17 (0) (0) 7 7 5000 0 #f #f 9000 3
      8000 4000 1 4 4 9000 9000 2 2 0 3 0 0 0 0 3 3 1 2 17 #f
      60 6 ((#t . -1) . 32) 3 ,(void) ,(void) ,(void) 0 4 0 0
      0 ,(void) 0 ,(void) 11 ,(void) 2 3050 2 3050 2050 2050
      700 0 0 0 #f 0 () 4 0 0 0 4 8000 2000 23 22 5061 1 1 4
      6 6 5 51 5 2 2 3 3 8 16 5 5 9 0 2 3 1 #t #t #t 120 (1)
      10 0 6400 6400 537516 8000 3000 63 120 10000 10000 8000
      24 35 #t 6 #t #f #f 1500 102 2600 60 5 5 5 5 5 8000 5
      1000 #f #t 120 144 3 3628800 3628800
      (52 44 17 22 38 . 3) (52 17 35 17 . 35) (3 . 42) #t 89
      89 37 4687 #t 48 176 5 1521 -1 (52 17 35 17 . 35) #f
      120 (120 . -120) #4(0 1 2 3) (3 . 6) 187 176 176 187
      (#t ((3 . 2) . 2) . 2) ((33 . 55) 77 . 99) 7 10 6
      (((3 21 18) 4 28 24) ((0 0 0) 1 7 6) (408 . 408)) 578
      (33 . #t)
      (#t .  #38(#f 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #t 0))
      3 3 (19 . #4(0 0 17 0)) (22 32 41 . 12)
      ((40 #f 105 15 . #t) #t 252 36 #t 9841 . 18) 35 9 22 2
      120 #t 0 668 778477 (#t #f #f)))
  (equal?
    (list
      ;;; Abdulaziz Ghuloum
      ;;; this is a vanilla insertion sort routine, not really interesting but used to
      ;;; derive the Y-Combinator version below.
      (letrec ([sort
                 (lambda (p? ls)
                   (if (null? ls)
                       '()
                       (insert p? (car ls) (sort p? (cdr ls)))))]
               [insert
                 (lambda (p? x ls)
                   (if (null? ls)
                       (cons x '())
                       (if (p? x (car ls))
                           (cons x ls)
                           (cons (car ls) (insert p? x (cdr ls))))))])
        (sort (lambda (x y) (< x y)) '(4 3 2 5 6 3 6 9)))

      ;;; and this is a more exotic insertion sort using double-Y-Combinator in order
      ;;; to stretch anonymous lambda expressions to their limit.  Does it hurt yet?
      (((lambda (le) ; this is sort
         ((lambda (f) (f f))
             (lambda (f)
               (le (lambda (p? ls)
                 ((f f) p? ls))))))
        (lambda (sort)
          (lambda (p? ls)
            (if (null? ls)
                '()
                (((lambda (le)  ; this is insert
                   ((lambda (f) (f f))
                       (lambda (f)
                         (le (lambda (x ls) ((f f) x ls))))))
                    (lambda (insert)
                   (lambda (x ls)
                     (if (null? ls)
                         (cons x '())
                         (if (p? x (car ls))
                             (cons x ls)
                                (cons (car ls) (insert x (cdr ls))))))))
                    (car ls) (sort p? (cdr ls)))))))
       (lambda (x y) (< x y)) ; this is the sorting criterion
       '(4 3 2 5 6 3 6 9)) ; and the list to be sorted

      ;;; this is a definition of a rotate procedure that rotates the elements of a
      ;;; list n times.  It rotates the pair cells themselves and not the contents.
      ;;; It tests proper closure implementations in (set! x (cdr x)) as well as
      ;;; set-cdr! as it does not appear that frequently in tests.ss
      ;;;
      ;;;  before
      ;;;      +--+--+    +--+--+    +--+--+         +--+--+    +--+--+    +--+--+
      ;;;      | 1|------>| 2|------>| 3|------> ... | 6|------>| 7|------>| 8|#f|
      ;;;      +--+--+    +--+--+    +--+--+         +--+--+    +--+--+    +--+--+
      ;;;       ^^
      ;;;       yx
      ;;;
      ;;;  after
      ;;;      +--+--+    +--+--+         +--+--+    +--+--+    +--+--+    +--+--+
      ;;;      | 4|------>| 5|------> ... | 8|------>| 1|------>| 2|------>| 3|#f|
      ;;;      +--+--+    +--+--+         +--+--+    +--+--+    +--+--+    +--+--+
      ;;;       ^                                     ^
      ;;;       x                                     y
      (let ([x (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 '()))))))))])
        (letrec ([rotate
                   (lambda (n)
                     (if (not (<= n 0))
                       (let ([s x])
                         (set! x (cdr x))
                         (insert s x)
                         (rotate (- n 1)))))]
                 [insert
                   (lambda (s x)
                     (if (null? (cdr x))
                         (begin
                           (set-cdr! x s)
                           (set-cdr! s '()))
                         (insert s (cdr x))))])
          (let ([y x])
            (rotate 3) ; rotate x and chop y as a side effect
            (cons x (cons y '()))))) ; cons for comparison

      ;;; Albert Hartono
      (letrec [(length 6)
               (start-value 6)]
        ((lambda (v lst)
           (letrec [(length (lambda (x)
                              (if (null? x)
                                  0
                                  (add1 (length (cdr x))))))]
             (let [(ls-lg (length lst))
                   (v-lg (vector-length v))]
               (let [(new-vec (make-vector (+ ls-lg v-lg)))]
                 (letrec [(loop-vec
                           (lambda (index)
                             (if (= index v-lg)
                                 (loop-ls lst index)
                                 (begin
                                   (vector-set! new-vec index (vector-ref v index))
                                   (loop-vec (add1 index))))))
                          (loop-ls
                           (lambda (lst index)
                             (if (not (null? lst))
                                 (begin
                                   (vector-set! new-vec index (car lst))
                                   (loop-ls (cdr lst) (add1 index))))))]
                   (loop-vec 0)
                   new-vec)))))
         (let [(vec (letrec ([tmp-vec (lambda () (make-vector length))]
                             [fill-vector
                              (lambda (v lg val)
                                (if (zero? lg)
                                    v
                                    (begin
                                      (vector-set! v (sub1 lg) val)
                                      (fill-vector v (sub1 lg) (add1 val)))))])
                      (fill-vector (tmp-vec) (vector-length (tmp-vec))
                                   (- 0 start-value))))]
           vec)
         (letrec [(make-list (lambda (lg val)
                               (if (not (zero? lg))
                                   (cons val (make-list (sub1 lg) (sub1 val)))
                                   '())))]
           (make-list length start-value))))

      ;;; Brooke Chenoweth
      ;;; a little Ackermann, just for fun
      ;;; if you uncomment this, you should probably make most of the passes
      ;;; trusted, unless you want to wait a long time for it to complete. - rkd
      #;(let ([x 3] [y 6])
        (letrec ([A (lambda (x y)
                      (if (= x 0)
                          (add1 y)
                          (if (= y 0)
                              (A (sub1 x) 1)
                              (A (sub1 x) (A x (sub1 y))))))])
          (A x y)))

      ;;; let's try out a more substantial program
      ;;; the N queens problem, for several values of n
      ;;; solve-n-queens gives a list of the row indices for a valid queen placement, or #f if no solution
      (let ([n-vals '(1 2 3 4 5 6 7 8)])
        (letrec ([solve-n-queens
                   (lambda (n)
                     (letrec ([extend-board
                                (lambda (i b)
                                  (if (= i n)
                                      (let ([b (adjust b)])
                                        (if b (extend-board 0 b) #f))
                                      (if (valid? i b)
                                          (cons i b)
                                          (extend-board (+ i 1) b))))]
                              [valid?
                                (lambda (i b)
                                    (no-threat? (sub1 i) i (add1 i) b))]
                              [no-threat?
                                (lambda (u s d others)
                                  (if (null? others)
                                      #t
                                      (if (not (let ([neighbor (car others)])
                                                  (if (= neighbor u)
                                                      #t
                                                      (if (= neighbor s)
                                                          #t
                                                          (= neighbor d)))))
                                          (no-threat? (- u 1) s (+ d 1) (cdr others))
                                          #f)))]
                              [adjust
                                (lambda (b)
                                  (if b
                                      (if (not (null? b))
                                          (extend-board (add1 (car b)) (cdr b))
                                          #f)
                                      #f))]
                              [solve
                                (lambda (len b)
                                  (if (= n len)
                                      b
                                      (solve (add1 len) (extend-board 0 b))))])
                       (solve 0 '())))])
          (letrec ([test
                     (lambda (ls)
                       (if (null? ls)
                           '()
                           (let ([n (car ls)])
                             (cons (solve-n-queens n)
                               (test (cdr ls))))))])
            (test n-vals))))

      ;;; Ronald Garcia
      (let ([re-apply
             (lambda (high)
               (letrec ([gen
                         (lambda (iter cont)
                           (let ([cont1 (lambda (f val) (cont f (f val)))]
                                 [cont2 (lambda (f val) (cont f val))])
                             (if (= iter 0)
                                 cont2
                                 (gen (- iter 1) cont1))))])
                 (gen high (lambda (f val) val))))])
        ((re-apply 10) (lambda (x) (+ x 1)) 5 ))

      (let ([make-list
             (lambda (count)
               (letrec ([loop
                         (lambda (val counter max)
                           (if (= counter max)
                               val
                               (loop (cons counter val) (+ counter 1) max)))])
                 (loop '() 0 count)))])
        (make-list 12))

      ;;; Jeremiah Willcock
      ;;; This test stresses two parts of the compiler: variable renaming and
      ;;; register allocation.  It stresses the variable renaming mechanism by
      ;;; using locally-bound names that match special forms in the compiler.  It
      ;;; stresses register allocation by having a large number of variables (and
      ;;; most of them are referenced).  The actual code of the program is mostly a
      ;;; factorial function, but with many helper lambdas to deal with the lack of
      ;;; if.  The list of set! statements had formerly set all variables up to z,
      ;;; but the list was trimmed so that it would compile using the compiler on
      ;;; the course Web page.  The list of cons expressions at the bottom could
      ;;; also be extended to z.  This program also has deeply nested expressions
      ;;; that will be simplified by remove-complex-opera*.  It also contains a not
      ;;; expression in order to test the compiler's handling of this expression
      ;;; type, as well as a one-armed if expression and an implicit begin.
      (let ([ef (lambda (x y z)
                  (let ([result z]) (if x (set! result y)) result))]
            [a 1] [b 2] [c 3] [d 4] [e 5] [f 6] [g 7] [h 8] [i 9]
            [j 10] [k 11] [l 12] [m 13] [n 14] [o 15] [p 16] [q 17] [r 18]
            [s 19] [t 20] [u 21] [v 22] [w 23] [x 24] [y 25] [z 26])
        (set! a 0)
        (set! b 0)
        (set! c 0)
        (set! d 0)
        (set! e 0)
        (set! f 0)
        (set! g 0)
        (set! h 0)
        (set! i 0)
        (set! j 0)
        (set! k 0)
        (set! l 0)
        (set! m 0)
        (set! n 0)
        (set! o 0)
        (set! p 0)
        (letrec ([let 5]
                 [letrec (lambda (x y) (set! let x) y)]
                 [fac (lambda (n) ((ef (not (zero? n)) (f2 n) f1)))]
                 [f1 (lambda () 1)]
                 [f2
                  ((lambda (f3) (lambda (n) (lambda () (* n (f3 n)))))
                   (lambda (n) (fac (- n 1))))]
                 [f3 (lambda (x) -1)]
                 [if (lambda (x) (lambda () (+ 1 x)))])
          ((lambda (lambda)
             (cons lambda
                   (cons (fac let)
                         (cons a (cons b (cons c (cons d (cons e (cons f
                           (cons g (cons h (cons i (cons j (cons k (cons l
                             (cons m (cons n (cons o '()))))))))))))))))))
           (letrec ([if 7]) ((if let))))))

      ;; This test uses streams of integers (similar to those studied in CSCI B521
      ;; and B621) to produce a list of integers that are not multiples of two and
      ;; five.  It also has a heavy use of lambdas within the streams.  This test
      ;; case will test closure conversion, most of its lambdas have references to
      ;; free variables.  This program is purely functional, so it is much less of
      ;; a test of assignment conversion and begin handling than the last program.
      (letrec ([integers (lambda (n) (cons n (lambda () (integers (+ n 1)))))]
               [stream-times (lambda (s n)
                 (cons (* (car s) n)
                  (lambda () (stream-times ((cdr s)) n))))]
               [difference (lambda (s1 s2)
                 (if (if (null? s1) #t (null? s2)) '()
                  (if (< (car s1) (car s2))
                   (cons (car s1) (lambda () (difference ((cdr s1)) s2)))
                   (if (= (car s1) (car s2))
                    (difference ((cdr s1)) ((cdr s2)))
                    (difference s1 ((cdr s2)))))))]
               [stream-head (lambda (s n)
                 (if (if (null? s) #t (zero? n)) '()
                  (cons (car s)
                   (if (= n 1) '() (stream-head ((cdr s)) (- n 1))))))])
        (stream-head
         (difference
          (difference (integers 0) (stream-times (integers 0) 2))
          (stream-times (integers 0) 5))
         20))

      ;;; Mark Meiss
      ;;; Test out identifier defintions, scope of letrec, the poor man's
      ;;; Y-combinator, and higher-order procedures.
      (letrec ([odd  (lambda (lambda odd)
                        ((odd (lambda))))]
                [even (lambda (letrec lambda)
                        (((((lambda letrec))))))])
         (letrec ([uf (lambda (x y z) (if (x) y z))]
                  [af (lambda (x y z) ((if x y z)))])
           (letrec ([make-sub (lambda (sub)
                                (lambda (n) (- n sub)))]
                    [odd (lambda (odd even)
                           (lambda (n)
                             ((uf (lambda () (zero? n))
                                  (lambda () #f)
                                  (lambda () ((even even odd) ((make-sub 1) n)))))))]
                    [even (lambda (even odd)
                            (lambda (n)
                              (af (zero? n)
                                  (lambda () #t)
                                  (lambda () ((odd odd even) ((make-sub 1) n))))))])
             ((even even odd) 12))))


      ;;; Test out higher-order procedures and a mixture of tail and non-tail
      ;;; calls by playing around with a representation of Church numerals.
      (letrec ([zero (lambda (f)
                        (lambda (x) x))]
                [succ (lambda (n)
                        (lambda (f)
                          (lambda (x) (f ((n f) x)))))]
                [zero? (lambda (n)
                         ((n (lambda (x) #f)) #t))])
         (letrec ([to-int (lambda (n)
                            ((n (lambda (a) (+ a 1))) 0))]
                  [from-int (lambda (n)
                              (if (= n 0) zero (succ (from-int (- n 1)))))])
           (letrec ([add (lambda (n)
                           (lambda (m) ((n succ) m)))])
             (- (+ 5 4)
                (to-int ((add (from-int 5)) (from-int 4)))))))

      ;;; Matthew Garrett
      ;;; Bubble Sort on a list of numbers
      ;;; A recursive function defined inside a recursive function, both with the
      ;;; same name.
      (letrec ([list-length   (lambda (ls)
                                (letrec ([loop (lambda (ls n)
                                                 (if (null? ls)
                                                     n
                                                     (loop (cdr ls) (+ n 1))))])
                                  (loop ls 0)))]
               [sorted?       (lambda (lon)
                                (if (<= (list-length lon) 1)
                                    #t
                                    (if (< (car lon) (car (cdr lon)))
                                        (sorted? (cdr lon))
                                        #f)))]
               [bubble-sort   (lambda (lon)
                                  (if (sorted? lon)
                                      lon
                                      (bubble-sort (cdr
        ; cdr is necessary because of the "hold" place keeper, in this inner
        ; bubble-sort, which is guaranteed to get first place in this lesser to
        ; greater sorting.
        (letrec ([bubble-sort (lambda (hold list-of-numbers)
                                (if (null? list-of-numbers)
                                    (cons hold '())
                                    (if (< hold (car list-of-numbers))
                                        (cons hold
                                          (bubble-sort
                                            (car list-of-numbers)
                                            (cdr list-of-numbers)))
                                        (cons (car list-of-numbers)
                                          (bubble-sort hold
                                            (cdr list-of-numbers))))))])
          (bubble-sort 0 lon))))))])
        (bubble-sort '(5 6 4 3 8 7))))
    '((2 3 3 4 5 6 6 9) (2 3 3 4 5 6 6 9)
       ((4 5 6 7 8 1 2 3) (1 2 3))
       #12(-1 -2 -3 -4 -5 -6 6 5 4 3 2 1)
       ((0) #f #f (2 0 3 1) (3 1 4 2 0) (4 2 0 5 3 1)
            (5 3 1 6 4 2 0) (3 1 6 2 5 7 4 0))
       15 (11 10 9 8 7 6 5 4 3 2 1 0)
       (6 40320 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (1 3 7 9 11 13 17 19 21 23 27 29 31 33 37 39 41 43 47 49)
       #t 0 (3 4 5 6 7 8)))
)

(mat constant-closures
  ; make sure that closure optimization doesn't replicate closures
  (let ([f (rec f (lambda (q) f))])
    (and
      (eq? f (f 3))
      (eq? ((f 3) 4) (f 3))))
  (begin
    (with-output-to-file "testfile-cc.ss"
      (lambda ()
        (pretty-print
          '(define $cc-foo (rec f (lambda (q) f)))))
      'replace)
    (compile-file "testfile-cc")
    (load "testfile-cc.so")
    #t)
  (eq? ($cc-foo 3) $cc-foo)
  (eq? (($cc-foo 3) 4) $cc-foo)
)

(mat simplify-if
  (eqv?
    (let ([x 'a] [y 'b])
      (and (fixnum? x) (fixnum? (car y))))
    #f)
  (eqv?
    (let ([x 'a] [y 'b])
      (and (fixnum? x) (fixnum? (car y)) 75))
    #f)
  (error? ; not a port
    (let ([x 'a])
      (and (textual-port? x) (input-port? x))))
  (not
    (let ([x 'a])
      (and (input-port? x) (textual-port? x))))
  (let ([x (current-input-port)])
    (and (input-port? x) (textual-port? x)))
  (equal?
    (let ()
      (define (? x) (and (input-port? x) (if (textual-port? x) #t (binary-port? x))))
      (define-syntax first-value
        (syntax-rules ()
          [(_ e) (let-values ([(x . r) e]) x)]))
      (list
        (? 'a)
        (? (open-string-input-port ""))
        (? (first-value (open-string-output-port)))
        (? (open-bytevector-input-port #vu8()))
        (? (first-value (open-bytevector-output-port)))))
    '(#f #t #f #t #f))
)

(mat virtual-registers
  (fixnum? (virtual-register-count))
  (fx>= (virtual-register-count) 0)
  (error? ; invalid index
    (virtual-register 'one))
  (error? ; invalid index
    (virtual-register -1))
  (error? ; invalid index
    (virtual-register (+ (most-positive-fixnum) 1)))
  (error? ; invalid index
    (virtual-register 0.0))
  (error? ; invalid index
    (set-virtual-register! 'one 19))
  (error? ; invalid index
    (set-virtual-register! -1 19))
  (error? ; invalid index
    (set-virtual-register! (+ (most-positive-fixnum) 1) 19))
  (error? ; invalid index
    (set-virtual-register! 0.0 19))
  (fx>= (virtual-register-count) 4)
  (eqv? (set-virtual-register! 3 'hello) (void))
  (eqv? (virtual-register 3) 'hello)
  (eqv?
    (let ([x 3]) (virtual-register x))
    'hello)
  (eqv?
    (let ([x 3] [y (cons 1 2)])
      (set-virtual-register! x (list y)))
    (void))
  (equal? (virtual-register 3) '((1 . 2)))
  (equal?
    (let ()
      (define g (make-guardian))
      (g (virtual-register 3))
      (collect)
      (list (virtual-register 3) (g)))
    '(((1 . 2)) #f))
)

(mat pariah
  (error? ; invalid syntax
    (pariah))
  (error? ; invalid syntax
    (pariah . 17))
  (equal?
    (list (pariah 17))
    '(17))
  (equal?
    (let f ([n 10])
      (if (fx= n 0)
          (pariah 1)
          (* n (f (fx- n 1)))))
    3628800)
  ; make sure that cp0 doesn't remove the pariah form
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(if (zero? (random 1000))
             (pariah (display 0))
             (display 1))))
    (if (= (optimize-level) 3)
        '(if (#3%zero? (#3%random 1000))
             (begin (pariah (void)) (#3%display 0))
             (#3%display 1))
        '(if (#3%zero? (#2%random 1000))
             (begin (pariah (void)) (#2%display 0))
             (#2%display 1))))
)

(unless (memq (machine-type) '(arm32le tarm32le arm64le tarm64le arm64osx tarm64osx ; timestamp counter tends to be priviledged on Arm
                                       arm64ios tarm64ios arm64nt tarm64nt
                                       pb pb32l pb32b pb64l pb64b tpb tpb32l tpb32b tpb64l tpb64b)) ; doesn't increment for pb
  (mat $read-time-stamp-counter

    (let ([t (#%$read-time-stamp-counter)])
      (and (integer? t) (exact? t)))

    (let ()
      ;; NB: pulled from thread.ms, to use as a delay
      (define fat+
        (lambda (x y)
          (if (zero? y)
              x
              (fat+ (1+ x) (1- y)))))
      (define fatfib
        (lambda (x)
          (if (< x 2)
              1
              (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
      (let loop ([count 10] [success 0])
        (if (fx= count 0)
            (>= success 9)
            (let ([t0 (#%$read-time-stamp-counter)])
              (fatfib 26)
              (let ([t1 (#%$read-time-stamp-counter)])
                (loop (fx- count 1)
                      (if (< t0 t1)
                          (fx+ success 1)
                          success)))))))
    ))

(mat procedure-arity-mask
  (equal? (procedure-arity-mask (lambda () #f)) 1)
  (equal? (procedure-arity-mask (lambda (x) x)) 2)
  (equal? (procedure-arity-mask (lambda (x y z w) x)) 16)
  (equal? (procedure-arity-mask (interpret '(lambda (x y z w) x))) 16)
  (or (eq? (current-eval) interpret)
      (equal? (procedure-arity-mask (lambda (x y z w a b c d e f g h i j) x)) (ash 1 14)))
  (or (eq? (current-eval) interpret)
      (equal? (procedure-arity-mask (interpret '(lambda (x y z w a b c d e f g h i j) x))) (ash 1 14)))
  (or (eq? (current-eval) interpret)
      (and
        (equal? (procedure-arity-mask (case-lambda)) 0)
        (equal? (procedure-arity-mask (case-lambda [(x) x] [(x y) y])) 6)
        (equal? (procedure-arity-mask (case-lambda [() x] [(x . y) y])) -1)
        (equal? (procedure-arity-mask (case-lambda [() x] [(x y . z) y])) (bitwise-not 2))
        (equal? (procedure-arity-mask (case-lambda [(x y . z) y] [() x])) (bitwise-not 2))
        (equal? (procedure-arity-mask (case-lambda [(x) x] [(x y) y] [(x y z) z])) 14)))
  (equal? (procedure-arity-mask list) -1)
  (equal? (procedure-arity-mask cons) 4)
  (equal? (procedure-arity-mask list*) (bitwise-not 1))

  (equal? (procedure-arity-mask +) -1)
  (equal? (procedure-arity-mask -) -2)
  (equal? (procedure-arity-mask max) -2)

  (equal? (call/cc procedure-arity-mask) -1)
  (equal? (call/1cc procedure-arity-mask) -1)
  (equal? (procedure-arity-mask #%$null-continuation) 0)
  (equal?
    (parameterize ([enable-cp0 #t]) (compile '(procedure-arity-mask
                                                (case-lambda [a a] [(b) b]))))
    -1)
  (equal?
    (parameterize ([enable-cp0 #f]) (compile '(procedure-arity-mask
                                                (case-lambda [a a] [(b) b]))))
    -1)

  (error? ; invalid argument
    (procedure-arity-mask 17))
  )


(mat procedure-name
  (begin
    (define (procedure-name f)
      (((inspect/object f) 'code) 'name))
    (define (ok-name? name expect)
      (or (equal? name expect)
          ;; interpreter currently doesn't keep names
          (eq? (current-eval) interpret)))
    (define should-be-named-f (let ([f (lambda (x) x)]) f))
    (define should-be-named-g (letrec ([g (lambda (x) x)]) g))
    (define should-be-named-h (let ([f (let ([h (lambda (x) x)]) h)]) f))
    (define should-be-named-i (letrec ([f (let ([i (lambda (x) x)]) i)]) f))
    (define should-be-named-j (let ([f (letrec ([j (lambda (x) x)]) j)]) f))
    (define (result-should-be-named-mk-CP)
      (let ([struct:CP (make-record-type-descriptor 'CP #f #f #f #f '(1 . 1))])
        (let ([mk-CP (record-constructor (make-record-constructor-descriptor
                                          struct:CP #f #f))])
          mk-CP)))
    #t)
  (ok-name? (procedure-name procedure-name) "procedure-name")
  (ok-name? (procedure-name should-be-named-f) "f")
  (ok-name? (procedure-name should-be-named-g) "g")
  (ok-name? (procedure-name should-be-named-h) "h")
  (ok-name? (procedure-name should-be-named-i) "i")
  (ok-name? (procedure-name should-be-named-j) "j")

  (or (not (enable-cp0))
      (#%$suppress-primitive-inlining)
      (let ([gx (make-guardian)])
        (ok-name? (procedure-name gx) "gx")))
  (or (not (enable-cp0))
      (#%$suppress-primitive-inlining)
      (ok-name? (procedure-name (result-should-be-named-mk-CP)) "mk-CP"))

  (or (not (enable-cp0))
      (andmap ok-name?
              (map
               procedure-name
               (let ([f (lambda (g)
                          (g (lambda (x) x)))])
                 (list (f (lambda (a) a))
                       (f (lambda (b) b)))))
              '("a" "b")))
  )


(mat wrapper-procedure
  (error? (make-wrapper-procedure))
  (error? (make-wrapper-procedure (lambda args args)))
  (error? (make-wrapper-procedure (lambda args args) 1))
  (error? (make-wrapper-procedure 1 1 #f))
  (error? (make-wrapper-procedure 'not-a-procedure 1 #f))
  (error? (make-wrapper-procedure (lambda args args) 'not-an-exact-integer #f))
  (error? (make-wrapper-procedure (lambda args args) 1.0 #f))

  (error? (make-arity-wrapper-procedure))
  (error? (make-arity-wrapper-procedure (lambda args args)))
  (error? (make-arity-wrapper-procedure (lambda args args) 1))
  (error? (make-arity-wrapper-procedure 1 1 #f))
  (error? (make-arity-wrapper-procedure 'not-a-procedure 1 #f))
  (error? (make-arity-wrapper-procedure (lambda args args) 'not-an-exact-integer #f))
  (error? (make-arity-wrapper-procedure (lambda args args) 1.0 #f))

  (equal? ((make-wrapper-procedure (lambda args args) 8 #f) 1 2 3)
          '(1 2 3))
  (equal? ((make-wrapper-procedure (lambda args args) 1 #f) 1 2 3) ; arity not checked!
          '(1 2 3))
  (equal? ((make-wrapper-procedure (lambda args args) (expt 2 100) #f) 1 2 3) ; arity not checked!
          '(1 2 3))

  (equal? ((make-arity-wrapper-procedure (lambda args args) 8 #f) 1 2 3)
          '(1 2 3))
  (equal? ((make-arity-wrapper-procedure (lambda args args) (+ (expt 2 100) 8) #f) 1 2 3)
          '(1 2 3))
  (error? ((make-arity-wrapper-procedure (lambda args args) 1 #f) 1 2 3))
  (error? ((make-arity-wrapper-procedure (lambda args args) (expt 2 100) #f) 1 2 3))
  (equal? (make-list 100 'ok) (apply (make-arity-wrapper-procedure (lambda args args) -1 #f) (make-list 100 'ok)))

  (equal? (procedure-arity-mask (make-wrapper-procedure (lambda args args) 1 #f))
          1)
  (equal? (procedure-arity-mask (make-wrapper-procedure (lambda args args) -12345 #f))
          -12345)
  (equal? (procedure-arity-mask (make-wrapper-procedure (lambda args args) (expt 2 100) #f))
          (expt 2 100))
          
  (equal? (procedure-arity-mask (make-arity-wrapper-procedure (lambda args args) 1 #f))
          1)
  (equal? (procedure-arity-mask (make-arity-wrapper-procedure (lambda args args) -12345 #f))
          -12345)
  (equal? (procedure-arity-mask (make-arity-wrapper-procedure (lambda args args) (expt 2 100) #f))
          (expt 2 100))
          
  (not (wrapper-procedure? 10))
  (not (wrapper-procedure? (lambda args args)))
  (not (wrapper-procedure? (interpret '(lambda args args))))
  (wrapper-procedure? (make-wrapper-procedure (lambda args args) 1 #f))
  (wrapper-procedure? (make-arity-wrapper-procedure (lambda args args) 1 #f))

  (error? (wrapper-procedure-data 1))
  (error? (wrapper-procedure-data (lambda args args)))
  (error? (wrapper-procedure-data (interpret '(lambda args args))))
  (equal? (wrapper-procedure-data (make-wrapper-procedure (lambda args args) 1 'data))
          'data)
  (equal? (wrapper-procedure-data (make-arity-wrapper-procedure (lambda args args) 1 'data))
          'data)

  (error? (set-wrapper-procedure-procedure!))
  (error? (set-wrapper-procedure-procedure! (make-arity-wrapper-procedure (lambda args args) 1 #f)))
  (error? (set-wrapper-procedure-procedure! 1 void))
  (error? (set-wrapper-procedure-procedure! (lambda args args) void))
  (error? (set-wrapper-procedure-procedure! (interpret '(lambda args args)) void))
  (let ([p (make-wrapper-procedure (lambda args args) 8 #f)])
    (set-wrapper-procedure-procedure! p vector)
    (equal? (p 1 2 3)
            '#(1 2 3)))
  (let ([p (make-arity-wrapper-procedure (lambda args args) 8 #f)])
    (set-wrapper-procedure-procedure! p vector)
    (equal? (p 1 2 3)
            '#(1 2 3)))

  (error? (set-wrapper-procedure-data!))
  (error? (set-wrapper-procedure-data! (make-arity-wrapper-procedure (lambda args args) 1 #f)))
  (error? (set-wrapper-procedure-data! 1 #t))
  (error? (set-wrapper-procedure-data! (lambda args args) #t))
  (error? (set-wrapper-procedure-data! (interpret '(lambda args args)) #t))
  (let ([p (make-wrapper-procedure (lambda args args) 8 'data)])
    (set-wrapper-procedure-data! p 'other-data)
    (equal? (wrapper-procedure-data p)
            'other-data))
  (let ([p (make-arity-wrapper-procedure (lambda args args) 8 'data)])
    (set-wrapper-procedure-data! p 'other-data)
    (equal? (wrapper-procedure-data p)
            'other-data))

  (let ([a (make-wrapper-procedure (lambda args args) 8 #f)])
    (lock-object a)
    (collect)
    (let ([g (gensym)])
      (set-wrapper-procedure-data! a g)
      (collect)
      (and
       (equal? (wrapper-procedure-data a) g)
       (begin (unlock-object a) #t))))
  (let ([a (make-arity-wrapper-procedure (lambda args args) 8 #f)])
    (lock-object a)
    (collect)
    (let ([g (gensym)])
      (set-wrapper-procedure-data! a g)
      (collect)
      (and
       (equal? (wrapper-procedure-data a) g)
       (begin (unlock-object a) #t))))
  )

(mat fasl-immutable
  (begin
    (define immutable-objs (list (vector->immutable-vector '#(1 2 3))
                                 (string->immutable-string "abc")
                                 (bytevector->immutable-bytevector #vu8(1 2 3))
                                 (box-immutable 1)
                                 ;; Not immutable, but we want to test strip:
                                 (fxvector 1 2 3)
                                 (flvector 1.5 2.5 3.5)
                                 (stencil-vector 6 'a 'b)))
    (define immutable-zero-objs (list (vector->immutable-vector '#())
                                      (string->immutable-string "")
                                      (bytevector->immutable-bytevector #vu8())
                                      (box-immutable 1)))
    (define (immutable? l)
      (and (immutable-vector? (list-ref l 0))
           (immutable-string? (list-ref l 1))
           (immutable-bytevector? (list-ref l 2))
           (immutable-box? (list-ref l 3))))
    (define (round-trip l)
      (let-values ([(o get) (open-bytevector-output-port)])
        (fasl-write l o)
        (immutable? (fasl-read (open-bytevector-input-port (get))))))
    (define (round-trip-via-strip l)
      (compile-to-file (list `(set! fasl-immutable-round-trip ',l)) "testfile-immut-sff.so")
      (strip-fasl-file "testfile-immut-sff.so" "testfile-immut-sff.so" (fasl-strip-options))
      (load "testfile-immut-sff.so")
      (let ([l2 (eval 'fasl-immutable-round-trip)])
        (and (equal? l l2)
             (immutable? l2))))
    (define (round-trip-symbol sym)
      (let-values ([(o get) (open-bytevector-output-port)])
        (fasl-write sym o)
        (let ([s (fasl-read (open-bytevector-input-port (get)))])
          (and (symbol? s)
               (immutable-string? (symbol->string s))
               (or (not (gensym? s))
                   (immutable-string? (gensym->unique-string s)))))))
    #t)

  (immutable? immutable-objs)
  (immutable? immutable-zero-objs)
  (round-trip immutable-objs)
  (round-trip immutable-zero-objs)
  (round-trip-via-strip immutable-objs)
  (round-trip-via-strip immutable-zero-objs)

  (round-trip-symbol 'hello)
  (round-trip-symbol (string->symbol "hola"))
  (round-trip-symbol (gensym "bonjour"))

  ;; Make sure `fasl-read` didn't mark "mutable" null values
  ;; as immutable:
  (mutable-vector? '#())
  (mutable-string? "")
  (mutable-bytevector? '#vu8())
  
 )

(mat show-allocation
  (begin
    (#%$show-allocation #t)
    #t)
)

; regression test for a bug in arm32 targets that improperly handled
; add-with-immediate instructions when the immediate operand didn't fit
; into 8 bits
; h/t @weinholt on Github
(mat arm32-immediate-operand
  (begin
    (with-output-to-file "testfile-ai-1.ss"
      (lambda ()
        (pretty-print
         '(library (arm-immediate-1)
            (export x y)
            (import (rnrs))
            (define (x) '(a))
            (define (y . _) '(a)))))
      'replace)
    (compile-library "testfile-ai-1.ss")
    (with-output-to-file "testfile-ai-2.ss"
      (lambda ()
        (pretty-print
         '(library (arm-immediate-2)
            (export)
            (import (rnrs) (arm-immediate-1))
            (define a
              (y (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)))
            (define b
              (y (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x)
                 (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x) (x))))))
      'replace)
    (compile-library "testfile-ai-2.ss")
    #t))

(mat self-evaluating-vectors
  ;; Do not assume the initial state of self-evaluating-vectors. For the tests below however, set it
  ;; to #t
  (begin
    (define default-sev (self-evaluating-vectors))
    (self-evaluating-vectors #t)
    #t)

  (error? (parameterize ([self-evaluating-vectors #f])
            (eval '#(1 a b))))

  (equal? (eval '#(2 c d)) '#(2 c d))

  (equal? (eval '(let ()
                   (define-syntax qv
                     (syntax-rules ()
                       [(_ a ...) #(a ...)]))
                   (qv 3 e f)))
          '#(3 e f))

  ;; There were bugs in handling of annotations in vectors (which only manifests when loading from files)
  (begin
    (define-syntax exptest
      (lambda (stx)
        (define (testfile . forms)
          (with-output-to-file "testfile-sev.ss"
            (lambda ()
              (for-each pretty-print forms))
            'replace)
          (load "testfile-sev.ss"))

        (syntax-case stx ()
          [(_ (lib name . libbody) b b* ...)
           (let ()
             (testfile (datum (lib name . libbody)))
             #'(let ()
                 (import name)
                 b b* ...))])))
    #t)

  (equal? (exptest (library (test-self-eval-vector)
                     (export v)
                     (import (chezscheme))
                     (define v #(a b c)))
                   v)
          '#(a b c))
  (equal? (exptest (library (test-self-eval-vector)
                     (export v)
                     (import (chezscheme))
                     (define v #(a (b) #(c (d) e))))
                   v)
          '#(a (b) #(c (d) e)))
  (equal? (exptest (library (test-self-eval-vector)
                     (export qv)
                     (import (chezscheme))
                     (define-syntax qv
                       (syntax-rules ()
                         [(_ a ...)
                          #(a ...)])))
                   (qv a (b c #(d) e) f))
          '#(a (b c #(d) e) f))
  (equal? (exptest (library (test-self-eval-vector)
                     (export mqv v)
                     (import (chezscheme))
                     (define-syntax mqv
                       (syntax-rules ()
                         [(_ mm init)
                          (define-syntax mm
                            (syntax-rules ()
                              [(_ a (... ...))
                               #(init a (... ...) #(init a) (... ...) (#(init a)) (... ...))]))]))
                     (mqv M zz)
                     (define v (M a- b-)))
                   (mqv mm XX)
                   (list (mm aa bb cc) v))
          '(#(XX aa bb cc #(XX aa) #(XX bb) #(XX cc) (#(XX aa)) (#(XX bb)) (#(XX cc)))
            #(zz a- b- #(zz a-) #(zz b-) (#(zz a-)) (#(zz b-)))))
  ;; Restore the flag in order not to disturb the other tests
  (begin
    (self-evaluating-vectors default-sev)
    #t))

(mat current-generate-id
  (begin
    (define (make-x-generator)
      (let ([x-uid "gf91a5b83ujz3mogjdaij7-x"]
            [counter-ht (make-eq-hashtable)])
        (lambda (sym)
          (let* ([n (eq-hashtable-ref counter-ht sym 0)]
                 [str (if (gensym? sym) (gensym->unique-string sym) (symbol->string sym))]
                 [g (gensym (symbol->string sym) (format "~a-~a-~a" x-uid str n))])
            (eq-hashtable-set! counter-ht sym (+ n 1))
            g))))
    (and (parameterize ([current-generate-id (make-x-generator)])
           (eval `(module consistent-x (x make-pt pt-r)
                    ;; Note: `module` doesn't currently enable `x` to be inlined
                    (define x 1)
                    (define-record-type pt (fields r i)))))
         #t))
  (begin
    (define return-x (let ()
                       (import consistent-x)
                       (lambda () x)))
    (define a-pt (let ()
                   (import consistent-x)
                   (make-pt -1 -2)))
    (define get-r (let ()
                    (import consistent-x)
                    (lambda (p) (pt-r p))))
    (equal? 1 (return-x)))
  (equal? -1 (get-r a-pt))
  (begin
    (parameterize ([current-generate-id (make-x-generator)])
      (eval `(module consistent-x (x make-pt pt-x)
               (define x 2)
               (define-record-type pt (fields x y)))))
    (equal? 2 (return-x)))
  (equal? -1 (get-r a-pt))
  (begin
    (parameterize ([current-generate-id (make-x-generator)])
      (eval `(module consistent-x (x)
               (define x 3)
               (define-syntax def (syntax-rules () [(_) (define x 'other)]))
               ;; `(def)` after above definition => expect that
               ;; its `x` is generated second
               (def))))
    (equal? 3 (return-x)))
)

(mat expand-omit-library-invocations
  (not (expand-omit-library-invocations))
  (begin
    (library (define-m-as-one) (export m) (import (chezscheme)) (define m 1))
    (define (find-define-m-as-one s)
      (or (eq? s 'define-m-as-one)
          (and (pair? s)
                (or (find-define-m-as-one (car s))
                        (find-define-m-as-one (cdr s))))))
    #t)
  (find-define-m-as-one (expand '(let () (import (define-m-as-one)) m)))
  (begin
    (expand-omit-library-invocations 'yes)
    (eq? #t (expand-omit-library-invocations)))
  (not (find-define-m-as-one (expand '(let () (import (define-m-as-one)) m))))
  (begin
    (expand-omit-library-invocations #f)
    (not (expand-omit-library-invocations)))
  (find-define-m-as-one (expand '(let () (import (define-m-as-one)) m)))
)

(mat enable-unsafe-application
  (begin
   (define (get-uncprep-form e)
     (let ([r #f])
       (parameterize ([run-cp0 (lambda (cp0 e)
                                 (parameterize ([enable-unsafe-application #f])
                                   (set! r (#%$uncprep e)))
                                 e)])
         (expand/optimize e))
       r))
   #t)
  (equivalent-expansion? (get-uncprep-form '(lambda (x) (x)))
                         '(lambda (x) (x)))
  (equivalent-expansion? (parameterize ([enable-unsafe-application #t])
                           (get-uncprep-form '(lambda (x) (x))))
                         (if (= 3 (optimize-level))
                             '(lambda (x) (x))
                             '(lambda (x) (#3%$app x))))
  )

(mat enable-unsafe-variable-reference
  (begin
   (define (get-uncprep-form e)
     (let ([r #f])
       (parameterize ([run-cp0 (lambda (cp0 e)
                                 (set! r (#%$uncprep e))
                                 e)])
         (expand/optimize e))
       r))
   #t)
  (equivalent-expansion? (parameterize ([#%$suppress-primitive-inlining #f])
                           (get-uncprep-form '(lambda (x) (letrec ([y y]) (+ y x)))))
                         (if (= 3 (optimize-level))
                             '(lambda (x)
                                (letrec ([y y])
                                  (#3%+ y x)))
                             '(lambda (x)
                                (let ([valid? #f])
                                  (letrec ([y (begin
                                                (if valid?
                                                    (#2%void)
                                                    (#2%$source-violation #f #f #t "attempt to reference undefined variable ~s" 'y))
                                                y)])
                                    (set! valid?  #t)
                                    (#2%+ y x))))))
  (equivalent-expansion? (parameterize ([enable-unsafe-variable-reference #t]
                                        [#%$suppress-primitive-inlining #f])
                           (get-uncprep-form '(lambda (x) (letrec ([y y]) (+ y x)))))
                         (if (= 3 (optimize-level))
                             '(lambda (x)
                                (letrec ([y y])
                                  (#3%+ y x)))
                             '(lambda (x)
                                (letrec ([y y])
                                  (#2%+ y x)))))
  )

(mat phantom-bytevector
  (phantom-bytevector? (make-phantom-bytevector 0))
  (not (phantom-bytevector? 10))
  (not (phantom-bytevector? (vector 1 2 3)))

  (error? (make-phantom-bytevector -1))
  (error? (make-phantom-bytevector (expt 2 100)))
  (error? (make-phantom-bytevector 'x))

  (begin
    (define $ph (make-phantom-bytevector 0))
    (phantom-bytevector? $ph))
  (eqv? 0 (phantom-bytevector-length $ph))      
  (eqv? (void) (set-phantom-bytevector-length! $ph 1))
  (eqv? 1 (phantom-bytevector-length $ph))
  (eqv? (void) (set-phantom-bytevector-length! $ph 100))
  (eqv? 100 (phantom-bytevector-length $ph))

  (begin
    (collect (collect-maximum-generation))
    (define $pre-allocated (bytes-allocated))
    (define $pre-memory (current-memory-bytes))
    (set-phantom-bytevector-length! $ph $pre-allocated)
    #t)

  ;; Big change to `(bytes-allocated)`
  (< (* 1.75 $pre-allocated)
     (bytes-allocated)
     (* 2.25 $pre-allocated))

  ;; Big change to `(current-memory-bytes)`
  (< (+ (* 0.75 $pre-allocated)
        $pre-memory)
     (current-memory-bytes)
     (+ (* 1.25 $pre-memory)
        $pre-memory))

  ;; Same change after GC
  (begin
    (collect (collect-maximum-generation))
    (< (* 1.75 $pre-allocated)
       (bytes-allocated)
       (* 2.25 $pre-allocated)))

  ;; fasl => another jump by `$pre-allocated` bytes
  (begin
    (define $ph2
      (let-values ([(o get) (open-bytevector-output-port)])
        (fasl-write $ph o)
        (fasl-read (open-bytevector-input-port (get)))))
    (phantom-bytevector? $ph2))

  (< (* 2.75 $pre-allocated)
     (bytes-allocated)
     (* 3.25 $pre-allocated))

  ;; Try GC again
  (begin
    (collect (collect-maximum-generation))
    (< (* 2.75 $pre-allocated)
       (bytes-allocated)
       (* 3.25 $pre-allocated)))

  ;; Let GC reclaim $ph2, and `(byte-allocated)` should go down
  (begin
    (set! $ph2 #f)
    (collect (collect-maximum-generation))
    (< (* 1.75 $pre-allocated)
       (bytes-allocated)
       (* 2.25 $pre-allocated)))

  (> (compute-size $ph) (phantom-bytevector-length $ph))

  ;; Change length of `$ph`, and `(byte-allocated)` should go down
  (begin
    (set-phantom-bytevector-length! $ph 0)
    (< (* 0.75 $pre-allocated)
       (bytes-allocated)
       (* 1.25 $pre-allocated)))
  )

(mat immobile
  (error? (box-immobile))
  (error? (box-immobile 1 2))

  (error? (make-immobile-vector))
  (error? (make-immobile-vector 'a))
  (error? (make-immobile-vector -10))
  (error? (make-immobile-vector (expt 2 100)))
  (error? (make-immobile-vector 10 1 2))

  (error? (make-immobile-bytevector))
  (error? (make-immobile-bytevector 'a))
  (error? (make-immobile-byte-vector -10))
  (error? (make-immobile-bytevector (expt 2 100)))
  (error? (make-immobile-bytevector 10 1024))
  (error? (make-immobile-bytevector 10 1 2))

  (box? (box-immobile 10))
  (vector? (make-immobile-vector 10))
  (eqv? 0 (vector-ref (make-immobile-vector 10) 9))
  (bytevector? (make-immobile-bytevector 10))
  (eqv? 0 (bytevector-u8-ref (make-immobile-bytevector 10 0) 9))

  (begin
    (define (make-objects)
      (let loop ([i 16])
        (cond
          [(zero? i) '()]
          [else
           (let* ([b (box-immobile (format "box ~a" i))]
                  [b-addr (#%$fxaddress b)]
                  [v (make-immobile-vector (expt 2 i) b)]
                  [v-addr (#%$fxaddress v)]
                  [s (make-immobile-bytevector (expt 2 i) i)]
                  [s-addr (#%$fxaddress s)])
             (cons (list i
                         b b-addr
                         v v-addr
                         s s-addr)
                   (loop (sub1 i))))])))
    (define (check-objects l)
      (let loop ([l l])
        (or (null? l)
            (let-values ([(i b b-addr v v-addr s s-addr) (apply values (car l))])
              (and (equal? (format "box ~a" i) (unbox b))
                   (equal? (format "box ~a" i) (unbox (vector-ref v (sub1 (vector-length v)))))
                   (eqv? i (bytevector-u8-ref s (sub1 (bytevector-length s))))
                   (eqv? b-addr (#%$fxaddress b))
                   (eqv? v-addr (#%$fxaddress v))
                   (eqv? s-addr (#%$fxaddress s))
                   (loop (cdr l)))))))
    (define (mutate-objects l)
      (let loop ([l l])
        (or (null? l)
            (let-values ([(i b b-addr v v-addr s s-addr) (apply values (car l))])
              (set-box! b (format "box ~a" i))
              (vector-set! v (sub1 (vector-length v)) (box (unbox b)))
              (loop (cdr l))))))
    #t)

  (with-interrupts-disabled
   (let ([objs (make-objects)])
     (and (check-objects objs)
          (begin
            (collect 0 1)
            (and
             (check-objects objs)
             (begin
               (mutate-objects objs)
               (collect 0 0)
               (and
                (check-objects objs)
                (begin
                  (collect (collect-maximum-generation))
                  (check-objects objs)))))))))

  (or
   (not (threaded?))
   (let ([m (make-mutex)]
         [c (make-condition)]
         [running 4])
     (let thread-loop ([t running])
       (unless (= t 0)
         (fork-thread
          (lambda ()
            (let loop ([i 1000] [objs '()] [addrs '()])
              (cond
                [(= i 0)
                 (mutex-acquire m)
                 (set! running (sub1 running))
                 (condition-signal c)
                 (mutex-release m)]
                [else
                 (let ([v (case (modulo i 3)
                            [(0) (box-immobile objs)]
                            [(1) (make-immobile-vector i objs)]
                            [(2) (make-immobile-bytevector i)])])
                   (let ([objs (cons v objs)]
                         [addrs (cons (#%$fxaddress v) addrs)])
                     (collect-rendezvous)
                     (let check ([objs objs] [addrs addrs])
                       (unless (null? objs)
                         (let ([v (car objs)])
                           (unless (= (#%$fxaddress v) (car addrs))
                             (errorf 'immobile "address changed: ~s" v))
                           (cond
                             [(box? v)
                              (unless (eq? (unbox v) (cdr objs))
                                (errorf 'immobile "bad box content"))]
                             [(vector? v)
                              (let loop ([j 0])
                                (unless (= j (vector-length v))
                                  (unless (eq? (cdr objs) (vector-ref v j))
                                    (errorf 'immobile "bad vector content"))
                                  (loop (add1 j))))]
                             [(bytevector? v)
                              (void)]
                             [else
                              (errorf 'immobile "bad object: ~s" v)]))
                         (check (cdr objs) (cdr addrs))))
                     (loop (sub1 i) objs addrs)))]))))
         (thread-loop (sub1 t))))
     (mutex-acquire m)
     (let loop ()
       (unless (= running 0)
         (condition-wait c m)
         (loop)))
     (mutex-release m)
     ;; Wait for threads to exit
     (let ()
       (define $threads (foreign-procedure "(cs)threads" () scheme-object))
       (let loop ()
         (unless (= 1 (length ($threads)))
           (sleep (make-time 'time-duration 10000 0))
           (loop))))
     #t))

  )

(mat compacting
  ;; try to provoke the GC into putting a record into marked
  ;; (instead of copied) space and check the write barrier there
  (let loop ([N 2])
    (or (= N 8192)
        (let sel-loop ([sels (list car cadr)])
          (cond
            [(null? sels) (loop (* N 2))]
            [else
             (let ()
               (define rtd (make-record-type
                            "r"
                            (let loop ([i N])
                              (if (zero? i)
                                  (list '[ptr y])
                                  (cons `[uptr ,(string->symbol (format "x~a" i))]
                                        (loop (sub1 i)))))))
               
               (define (make-r)
                 (apply (record-constructor rtd)
                        (let loop ([i N])
                          (if (zero? i)
                              '(the-y-value)
                              (cons 0 (loop (sub1 i)))))))
               
               (define r-y (record-accessor rtd N))
               (define set-r-y! (record-mutator rtd N))
               
               (define rs (list (make-r)
                                (make-r)
                                (make-r)))
               (collect (collect-maximum-generation))
               (set! rs (list (car rs) (caddr rs)))
               (collect (collect-maximum-generation))
               (set-r-y! ((car sels) rs) (string-copy "new-string-to-go"))
               (collect)
               (and (equal? (r-y ((car sels) rs))
                            "new-string-to-go")
                    (sel-loop (cdr sels))))]))))
  )

(mat $begin-unsafe
  (equivalent-expansion?
   (parameterize ([#%$suppress-primitive-inlining #f])
     (expand/optimize '(lambda (x)
                         (import (only $system $begin-unsafe))
                         (let () (fx+ 2 ($begin-unsafe (fx+ 1 x)))))))
   (if (fx= (optimize-level) 3)
       '(lambda (x) (#3%fx+ 2 (#3%fx+ 1 x)))
       '(lambda (x) (#2%fx+ 2 (#3%fx+ 1 x)))))
  )
