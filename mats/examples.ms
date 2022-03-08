;;; examples.ms
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

(define-syntax examples-mat
  (syntax-rules ()
    [(_ name (file ...) expr ...)
     (begin
       (mat name
         (begin
           (load (find-source (format "~a.ss" file)))
           ...
           #t)
         expr ...)
       (mat name
         (begin
           (load (find-source (format "~a.so" file)))
           ...
           #t)
         expr ...))]))

(define load-example
   (case-lambda
      [(str)
       (load (find-source (format "~a.ss" str)))
       #t]
      [(str eval)
       (load (find-source (format "~a/~a.ss" str)) eval)
       #t]))

(define (example-file file) (find-source file))

(define file=?
   (lambda (fn1 fn2)
      (let ([p1 (open-input-file fn1)] [p2 (open-input-file fn2)])
         (let loop ()
            (let cloop ([c1 (read-char p1)] [c2 (read-char p2)])
              ;; discard CR characters to avoid Unix-vs.-Windows issues
              (cond
                [(eqv? c1 #\return) (cloop (read-char p1) c2)]
                [(eqv? c2 #\return) (cloop c1 (read-char p2))]
                [(eof-object? c1)
                 (close-port p1)
                 (close-port p2)
                 (eof-object? c2)]
                [else (and (not (eof-object? c2))
                           (char=? c1 c2)
                           (loop))]))))))

(examples-mat def-edit ("def" "edit")
   (begin (def fact (lambda (x) (if (zero? x) 1 (* x (fact ( x 1))))))
          (procedure? fact))
   (equal? (ls-def) '(fact))
   (let ([in (open-input-string "3 3 4 3 2 (ib 1 -) t")]
         [out (open-output-string)])
      (and (eqv? (parameterize ([current-input-port in]
                                [current-output-port out])
                    (ed-def fact))
                 'fact)
           (equal? (get-output-string out)
"(def fact (lambda (...) (...)))
edit> (lambda (x) (if (...) 1 (...)))
edit> (if (zero? x) 1 (* x (...)))
edit> (* x (fact (...)))
edit> (fact (x 1))
edit> (x 1)
edit> (- x 1)
edit> (def fact (lambda (...) (...)))
edit> 
")))
   (eqv? (fact 30) 265252859812191058636308480000000)
 )

(examples-mat fact ("fact")
   (eqv? (fact 30) 265252859812191058636308480000000)
 )

(examples-mat fatfib ("fatfib")
   (eqv? (fatfib 10) 89)
 )

(examples-mat fib ("fib")
   (begin (printf "***** expect trace of (fib 4):~%")
          (eqv? (fib 4) 5))
 )

(examples-mat freq ("freq")
   ;; freq.in and freq.out come from example in TSPL
   (begin (delete-file "testfile.freq" #f) #t)
   (begin (frequency (example-file "freq.in") "testfile.freq")
          (file=? "testfile.freq" (example-file "freq.out")))
 )

;-------- freq.in: --------
;Peter Piper picked a peck of pickled peppers;
;A peck of pickled peppers Peter Piper picked.
;If Peter Piper picked a peck of pickled peppers,
;Where's the peck of pickled peppers Peter Piper picked?

;-------- freq.out: --------
;1       A
;1       If
;4       Peter
;4       Piper
;1       Where
;2       a
;4       of
;4       peck
;4       peppers
;4       picked
;4       pickled
;1       s
;1       the

; "interpret" can't handle all Chez core forms
;(mat interpret
;   (and (eq? (getprop 'interpret '*type*) 'primitive)
;        (begin (remprop 'interpret '*type*) #t))
;   (load-example "interpret")
;   (load-example "interpret" interpret)
;   (load-example "fatfib" interpret)
;   (eqv? (fatfib 4) 5)
;   (begin (putprop 'interpret '*type* 'primitive) #t)
; )

(examples-mat m4 ("m4")
   (begin (m4 "testfile.m4" (example-file "m4test.in"))
          (file=? (example-file "m4test.out") "testfile.m4"))
 )

(examples-mat macro ("macro")
   (begin (macro xxxxxx (lambda (x) `',x)) #t)
   (equal? (xxxxxx 3) '(xxxxxx 3))
 )

(examples-mat matrix ("matrix")
  ;; examples from TSPL2:
  (equal? (mul 3 4) 12)
  (equal? (mul 1/2 '#(#(1 2 3))) '#(#(1/2 1 3/2)))
  (equal? (mul -2
               '#(#(3 -2 -1)
                  #(-3 0 -5)
                  #(7 -1 -1))) '#(#(-6 4 2)
                                  #(6 0 10)
                                  #(-14 2 2)))
  (equal? (mul '#(#(1 2 3))
               '#(#(2 3)
                  #(3 4)
                  #(4 5))) '#(#(20 26)))
  (equal? (mul '#(#(2 3 4)
                  #(3 4 5))
               '#(#(1) #(2) #(3))) '#(#(20) #(26)))
  (equal? (mul '#(#(1 2 3)
                  #(4 5 6))
               '#(#(1 2 3 4)
                  #(2 3 4 5)
                  #(3 4 5 6))) '#(#(14 20 26 32)
                                  #(32 47 62 77)))
)

(examples-mat object ("object")
   (begin (define-object (summit x)
             ([y 3])
             ([getx (lambda () x)]
              [sumxy (lambda () (+ x y))]
              [setx (lambda (v) (set! x v))]))
          (procedure? summit))
   (begin (define a (summit 1)) (procedure? a))
   (eq? (send-message a getx) 1)
   (eq? (send-message a sumxy) 4)
   (begin (send-message a setx 13)
          (eq? (send-message a sumxy) 16))
   ;; examples from TSPL:
   (begin (define-object (kons kar kdr)
             ([get-car (lambda () kar)]
              [get-cdr (lambda () kdr)]
              [set-car! (lambda (x) (set! kar x))]
              [set-cdr! (lambda (x) (set! kdr x))]))
          (procedure? kons))
   (begin (define p (kons 'a 'b)) (procedure? p))
   (eq? (send-message p get-car) 'a)
   (eq? (send-message p get-cdr) 'b)
   (begin (send-message p set-cdr! 'c)
          (eq? (send-message p get-cdr) 'c))
   (begin (define-object (kons kar kdr pwd)
             ([get-car (lambda () kar)]
              [get-cdr (lambda () kar)]
              [set-car!
                 (lambda (x p)
                    (when (string=? p pwd)
                        (set! kar x)))]
              [set-cdr!
                 (lambda (x p)
                    (when (string=? p pwd)
                        (set! kar x)))]))
          (procedure? kons))
   (begin (define p1 (kons 'a 'b "magnificent")) (procedure? p1))
   (begin (send-message p1 set-car! 'c "magnificent")
          (eq? (send-message p1 get-car) 'c))
   (begin (send-message p1 set-car! 'd "please")
          (eq? (send-message p1 get-car) 'c))
   (begin (define p2 (kons 'x 'y "please")) (procedure? p2))
   (begin (send-message p2 set-car! 'z "please")
          (eq? (send-message p2 get-car) 'z))
   (begin (define-object (kons kar kdr)
             ([count 0])
             ([get-car
               (lambda ()
                  (set! count (+ count 1))
                  kar)]
              [get-cdr
               (lambda ()
                  (set! count (+ count 1))
                  kdr)]
              [accesses
               (lambda () count)]))
          (procedure? kons))
   (begin (define p (kons 'a 'b)) (procedure? p))
   (eq? (send-message p get-car) 'a)
   (eq? (send-message p get-cdr) 'b)
   (eq? (send-message p accesses) '2)
   (eq? (send-message p get-cdr) 'b)
   (eq? (send-message p accesses) '3)
 )

(examples-mat power ("power")
  (eqv? (power 1/2 3) 1/8)
)

(examples-mat rabbit ("rabbit")
   (begin (printf "***** expect rabbit output:~%")
          (rabbit 3)
          (dispatch)
          #t)
 )

(examples-mat rsa ("rsa")
   (begin (printf "***** expect rsa output:~%")
          (make-user bonzo)
          (make-user bobo)
          (make-user tiger)
          (show-center)
          #t)
   (equal? (send "hi there" bonzo bobo) "hi there")
   (equal? (send "hi there to you" bobo bonzo) "hi there to you")
   (not (equal? (decrypt (encrypt "hi there" bonzo bobo) tiger)
                "hi there"))
 )

(define stream->list
   (lambda (s)
      (if (procedure? s)
          '()
          (cons (car s) (stream->list (cdr s))))))

(examples-mat scons ("scons")
   (eqv? (stream-ref factlist 3) 6)
   (equal? (stream->list factlist) '(1 1 2 6))
   (eqv? (stream-ref factlist 10) 3628800)
   (equal? (stream->list factlist)
           '(1 1 2 6 24 120 720 5040 40320 362880 3628800))
   (eqv? (stream-ref fiblist 3) 3)
   (equal? (stream->list fiblist) '(1 1 2 3))
   (eqv? (stream-ref fiblist 5) 8)
   (equal? (stream->list fiblist) '(1 1 2 3 5 8))
 )

(examples-mat setof ("setof")
   (equal? (set-of x (x in '(a b c))) '(a b c))
   (equal? (set-of x (x in '(1 2 3 4)) (even? x)) '(2 4))
   (equal? (set-of (cons x y) (x in '(1 2 3)) (y is (* x x)))
           '((1 . 1) (2 . 4) (3 . 9)))
   (equal? (set-of (cons x y) (x in '(a b)) (y in '(1 2)))
           '((a . 1) (a . 2) (b . 1) (b . 2)))
 )

(examples-mat unify ("unify")
   ;; examples from TSPL:
   (eq? (unify 'x 'y) 'y)
   (equal? (unify '(f x y) '(g x y)) "clash")
   (equal? (unify '(f x (h)) '(f (h) y)) '(f (h) (h)))
   (equal? (unify '(f (g x) y) '(f y x)) "cycle")
   (equal? (unify '(f (g x) y) '(f y (g x))) '(f (g x) (g x)))
 )

(examples-mat fft ("fft")
  (equal? (dft '(0 0 0 0)) '(0 0 0 0))
  (equal? (dft '(2.0 2.0 2.0 2.0)) '(8.0 0.0-0.0i 0.0 0.0+0.0i))
  (equal? (dft '(+2.i +2.i +2.i +2.i)) '(+0.0+8.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))
)

(examples-mat compat ("compat")
   (eqv? (define! defined-with-define! (lambda () defined-with-define!))
         'defined-with-define!)
   (let ((p defined-with-define!))
     (set! defined-with-define! 0)
     (eqv? (p) 0))

   (eqv? (defrec! defined-with-defrec! (lambda () defined-with-defrec!))
         'defined-with-defrec!)
   (let ((p defined-with-defrec!))
     (set! defined-with-defrec! 0)
     (eqv? (p) p))

   (eqv? (begin0 1 2 3 4) 1)

   (equal? (recur f ((ls '(a b c)) (new '()))
             (if (null? ls) new (f (cdr ls) (cons (car ls) new))))
           '(c b a))

   (equal? (tree-copy '()) '())
   (equal? (tree-copy 'a) 'a)
   (equal? (tree-copy '(a)) '(a))
   (equal? (tree-copy '(a (b c) . d)) '(a (b c) . d))
   (let* ((p1 '((a . b) c)) (p2 (car p1)) (p3 (cdr p1)))
       (let ((c1 (tree-copy p1)))
           (not
               (or (memq c1 (list p1 p2 p3))
                   (memq (car c1) (list p1 p2 p3))
                   (memq (cdr c1) (list p1 p2 p3))))))

   (= *most-positive-short-integer*
      *most-positive-fixnum*
      (most-positive-fixnum))

   (= *most-negative-short-integer*
      *most-negative-fixnum*
      (most-negative-fixnum))

   (eof-object? *eof*)

   (eq? short-integer? fixnum?)
   (eq? big-integer? bignum?)
   (eq? ratio? ratnum?)
   (eq? float? flonum?)

   (eq? bound? top-level-bound?)
   (eq? global-value top-level-value)
   (eq? set-global-value! set-top-level-value!)
   (eq? define-global-value define-top-level-value)
   (eq? symbol-value top-level-value)
   (eq? set-symbol-value! set-top-level-value!)

   (eq? put putprop)
   (eq? get getprop)

   (eq? copy-list list-copy)
   (eq? copy-tree tree-copy)
   (eq? copy-string string-copy)
   (eq? copy-vector vector-copy)

   (eq? intern string->symbol)
   (eq? symbol-name symbol->string)
   (eq? make-temp-symbol gensym)
   (eq? temp-symbol? gensym?)
   (eq? string->uninterned-symbol gensym)
   (eq? uninterned-symbol? gensym?)

   (eq? compile-eval compile)

   (eq? closure? procedure?)

   (eq? =? =)
   (eq? <? <)
   (eq? >? >)
   (eq? <=? <=)
   (eq? >=? >=)

   (eq? float exact->inexact)
   (eq? rational inexact->exact)

   (eq? char-equal? char=?)
   (eq? char-less? char<?)
   (eq? string-equal? string=?)
   (eq? string-less? string<?)

   (eq? flush-output flush-output-port)
   (eq? clear-output clear-output-port)
   (eq? clear-input clear-input-port)

   (eq? mapcar map)
   (eq? mapc for-each)
   (eq? true #t)
   (eq? false #f)
   (eq? t #t)
   (eq? nil '())

   (eq? macro-expand expand)

    (eq? (cull negative? '()) '())
    (let ((x (list -1 2 -3 -3 1 -5 2 6)))
        (and
            (equal? (cull pair? x) '())
            (equal? (cull negative? x) '(-1 -3 -3 -5))
            (equal? x '(-1 2 -3 -3 1 -5 2 6))))

    (eq? (cull! negative? '()) '())
    (let ((x (list -1 2 -3 -3 1 -5 2 6)))
        (and
            (equal? (cull! pair? x) '())
            (equal? (cull! negative? x) '(-1 -3 -3 -5))))

    (eq? (mem (lambda (x) #t) '()) #f)
    (let ((x '(a b c)))
        (and
            (equal? (mem (lambda (x) (eq? x 'a)) x) x)
            (equal? (mem (lambda (x) (eq? x 'b)) x) (cdr x))
            (equal? (mem (lambda (x) (eq? x 'c)) x) (cddr x))
            (equal? (mem (lambda (x) (eq? x 'd)) x) #f)))
    (let ((x '(1 -2 3)))
        (and
            (equal? (mem negative? x) (cdr x))
            (equal? (mem positive? x) x)
            (equal? (mem pair? x) #f)))

    (eq? (rem (lambda (x) #t) '()) '())
    (let ((x (list 1 -2 3)))
        (and
            (equal? (rem negative? x) '(1 3))
            (equal? x '(1 -2 3))))
    (let ((x (list 1 -2 3)))
        (and
            (equal? (rem positive? x) '(-2))
            (equal? x '(1 -2 3))))

    (eq? (rem! (lambda (x) #t) '()) '())
    (let ((x (list 1 -2 3))) (equal? (rem! negative? x) '(1 3)))
    (let ((x (list 1 -2 3))) (equal? (rem! positive? x) '(-2)))

    (eq? (ass (lambda (x) #t) '()) #f)
    (let ((a (list -1)) (b (list 2)) (c (list 3)))
        (let ((l (list a b c)))
            (and
                (equal? (ass negative? l) a)
                (equal? (ass positive? l) b)
                (equal? (ass (lambda (x) (= x 3)) l) c)
                (equal? (ass pair? l) #f))))

    (equal? (decode-float 0.0) '#(0 0 1))
    (let ((x (decode-float (inexact 2/3))))
        (define ~=
          (let ([*fuzz* .0001])
             (lambda (x y)
                (and (flonum? x)
                     (flonum? y)
                     (<= (abs (- x y)) *fuzz*)))))
        (~= (inexact (* (vector-ref x 2)
                               (vector-ref x 0)
                               (expt 2 (vector-ref x 1))))
            (inexact 2/3)))

    (let ((x (box 3)))
        (and (equal? (swap-box! x 4) 3) (equal? (unbox x) 4)))

  (begin (define-macro! fudge (a (b . c) d) `(quote (,a ,b ,c ,d)))
         (equal? (fudge + (- . *) /) '(+ - * /)))

 ; tests from MichaelL@frogware.com, testing the changes he suggested
  (let ()
    (define-macro test-1 (val)
      `',val)
    (equal? 'x (test-1 x)))
  (let ()
    (define-macro (test-1 val)
      `',val)
    (equal? 'x (test-1 x)))
  (let ()
    (define-macro test-2 (val)
      `'(,val))
    (equal? '(x) (test-2 x)))
  (let ()
    (define-macro (test-2 val)
      `'(,val))
    (equal? '(x) (test-2 x)))
  (let ([xyz '(x y z)])
    (define-macro test-3 (val)
      `(,@val))
    (equal? '(x y z) (test-3 xyz)))
  (let ([xyz '(x y z)])
    (define-macro (test-3 val)
      `(,@val))
    (equal? '(x y z) (test-3 xyz)))
  (let ()
    (define-macro test-4 (val)
      (let ([test-function (lambda (x)
                             (string->symbol
                              (string-append
                               (symbol->string x)
                               "!!!")))])
        `'(,(test-function val))))
    (equal? '(xyz!!!) (test-4 xyz)))
  (let ()
    (define-macro (test-4 val)
      (let ([test-function (lambda (x)
                             (string->symbol
                              (string-append
                               (symbol->string x)
                               "!!!")))])
        `'(,(test-function val))))
    (equal? '(xyz!!!) (test-4 xyz))) 
  (let ()
    (define-macro test-5 (this . that)
      `'(,this ,that))
    (equal? '(x (y z)) (test-5 x y z)))
  (let ()
    (define-macro (test-5 this . that)
      `'(,this ,that))
    (equal? '(x (y z)) (test-5 x y z)))
  (let ()
    (define-macro test-6 (this . that)
      `'(,this ,@that))
    (equal? '(x y z) (test-6 x y z)))
  (let ()
    (define-macro (test-6 this . that)
      `'(,this ,@that))
    (equal? '(x y z) (test-6 x y z)))
  (let ()
    (defmacro test-1 (val)
      `',val)
    (equal? 'x (test-1 x)))
  (let ()
    (defmacro (test-1 val)
      `',val)
    (equal? 'x (test-1 x)))
  (let ()
    (defmacro test-2 (val)
      `'(,val))
    (equal? '(x) (test-2 x)))
  (let ()
    (defmacro (test-2 val)
      `'(,val))
    (equal? '(x) (test-2 x)))
  (let ([xyz '(x y z)])
    (defmacro test-3 (val)
      `(,@val))
    (equal? '(x y z) (test-3 xyz)))
  (let ([xyz '(x y z)])
    (defmacro (test-3 val)
      `(,@val))
    (equal? '(x y z) (test-3 xyz)))
  (let ()
    (defmacro test-4 (val)
      (let ([test-function (lambda (x)
                             (string->symbol
                              (string-append
                               (symbol->string x)
                               "!!!")))])
        `'(,(test-function val))))
    (equal? '(xyz!!!) (test-4 xyz)))
  (let ()
    (defmacro (test-4 val)
      (let ([test-function (lambda (x)
                             (string->symbol
                              (string-append
                               (symbol->string x)
                               "!!!")))])
        `'(,(test-function val))))
    (equal? '(xyz!!!) (test-4 xyz)))
  (let ()
    (defmacro test-5 (this . that)
      `'(,this ,that))
    (equal? '(x (y z)) (test-5 x y z)))
  (let ()
    (defmacro (test-5 this . that)
      `'(,this ,that))
    (equal? '(x (y z)) (test-5 x y z)))
  (let ()
    (defmacro test-6 (this . that)
      `'(,this ,@that))
    (equal? '(x y z) (test-6 x y z)))
  (let ()
    (defmacro (test-6 this . that)
      `'(,this ,@that))
    (equal? '(x y z) (test-6 x y z)))

  (begin (define-struct! caramel x y z) (eqv? (caramel-x (caramel 1 2 3)) 1))
)

(examples-mat ez-grammar-test ("ez-grammar-test")
  (equal?
    (with-output-to-string ez-grammar-test)
    "8 tests ran\n")
)
