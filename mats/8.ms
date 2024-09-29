;;; 8.ms
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

(mat define-syntax
   (begin (define-syntax foo
            (syntax-rules ()
              [(foo a b) (list a b)]))
          #t)
   (error? (expand '(foo)))
   (error? (expand '(foo . a)))
   (error? (expand '(foo a)))
   (error? (expand '(foo a . b)))
   (equal? (foo 3 4) '(3 4))
;;   (equal? (expand-once '(foo 3 4)) '(list 3 4))
   (equal? (foo 3 4) '(3 4))
   (error? (expand '(foo a b . c)))
   (error? (expand '(foo a b c)))
   (begin (define-syntax foo
            (syntax-rules (bar)
              [(foo) '()]
              [(foo (bar x)) x]
              [(foo x) (cons x '())]
              [(foo x y ...) (cons x (foo y ...))]))
          #t)
   (equivalent-expansion? (expand '(foo)) ''())
   (equivalent-expansion? (expand '(foo (bar a))) 'a)
   (equal? (foo 'a) '(a))
;;   (equal? (expand-once '(foo a b c)) '(cons a (foo b c)))
   (equal? (foo 'a 'b 'c) '(a b c))
   (equal? (foo 'a 'b (bar 'c)) '(a b . c))
   (equal? (foo 'a 'b 'c 'd) '(a b c d))
   (equal? (foo 'a 'b 'c (bar 'd)) '(a b c . d))
   (begin (define-syntax foo
            (lambda (x)
              (syntax-case x ()
                [(_ ((x v) ...) e1 e2 ...)
                 (andmap symbol? '(x ...))
                 (syntax ((lambda (x ...) e1 e2 ...) v ...))]
                [(_ ((lambda (x ...) e1 e2 ...) v ...))
                 (= (length '(x ...)) (length '(v ...)))
                 (syntax (foo ((x v) ...) e1 e2 ...))])))
          #t)
   (equal? (foo ((a 3) (b 4)) (cons a b)) '(3 . 4))
   (error? (expand '(foo ((1 b) (c d)) e f g)))
   (error? (expand '(foo ((lambda (a c) e f g) b))))
   (error? (define-syntax foo (syntax-rules (...) [(foo ...) 0])))
  ; no longer an error:
   #;(error? (define-syntax foo (syntax-rules () [(foo x ... y) 0])))
   (error? (define-syntax foo (syntax-rules () [(foo x . ...) 0])))
   (error? (define-syntax foo (syntax-rules () [(foo (...)) 0])))
   (error? (define-syntax foo (syntax-rules () [(foo x x) 0])))
   (begin (define-syntax foo (syntax-rules () [(foo foo) 0])) #t)
   (begin (define-syntax foo
            (lambda (x)
              (syntax-case x ()
                [(_ keys)
                 (with-syntax ([x `,(syntax keys)]) (syntax x))])))
          (equivalent-expansion? (expand '(foo (a b c))) '(a b c)))
   (begin (define-syntax foo ; test exponential "with" time problem
            (lambda (x)
              (syntax-case x ()
                [(_)
                 (with-syntax
                   ([a1 1] [b1 2] [c1 3] [d1 4] [e1 5] [f1 6] [g1 7] [h1 8]
                    [a2 1] [b2 2] [c2 3] [d2 4] [e2 5] [f2 6] [g2 7] [h2 8]
                    [a3 1] [b3 2] [c3 3] [d3 4] [e3 5] [f3 6] [g3 7] [h3 8]
                    [a4 1] [b4 2] [c4 3] [d4 4] [e4 5] [f4 6] [g4 7] [h4 8]
                    [a5 1] [b5 2] [c5 3] [d5 4] [e5 5] [f5 6] [g5 7] [h5 8]
                    [a6 1] [b6 2] [c6 3] [d6 4] [e6 5] [f6 6] [g6 7] [h6 8]
                    [a7 1] [b7 2] [c7 3] [d7 4] [e7 5] [f7 6] [g7 7] [h7 8]
                    [a8 1] [b8 2] [c8 3] [d8 4] [e8 5] [f8 6] [g8 7] [h8 8])
                   (syntax (list a1 b2 c3 d4 e5 f6 g7 h8)))])))
          (equal? (foo) '(1 2 3 4 5 6 7 8)))
   (eqv? (let ()
           (let-syntax () (define x 3) (define y 4))
           (define z (lambda () (+ x y)))
           (z))
         7)
   (eqv? (let ()
           (let-syntax ((a (syntax-rules ()
                             ((_ x v) (define x v))))
                        (b (syntax-rules ()
                             ((_ x v) (define-syntax x
                                        (syntax-rules ()
                                          ((_) v)))))))
             (a x 3)
             (b y 4))
           (define z (lambda () (+ x (y))))
           (z))
         7)
  (eqv?
    (let-syntax ((a (eval '(lambda (x) (let ((x x)) (syntax 3))))))
      (a))
    3)
  (error?
    (begin
      (define-syntax x (let ((a 3)) (identifier-syntax (define a 4))))
      x))
  (error?
    (begin
      (define-syntax x (let ((a 3)) (identifier-syntax (set! a 4))))
      x))
  (error?
    (begin
      (define-syntax x
        (let ((a 3))
          (identifier-syntax
            (fluid-let-syntax ((a (identifier-syntax 4)))
              3))))
      x))
  ;; transformers expressions can reference local keywords
  (eqv?
    (let-syntax ((a (lambda (x) (syntax (lambda (y) (syntax 3))))))
      (let-syntax ((b a))
        b))
    3)
  (eqv?
    (let-syntax ((a (lambda (x) (syntax (lambda (y) (syntax 3))))))
      (letrec-syntax ((b a))
        b))
    3)
  (eqv?
    (let-syntax ((a (lambda (x) (syntax (lambda (y) (syntax 3))))))
      (fluid-let-syntax ((b a))
        b))
    3)
  (eqv?
    (let-syntax ((a (lambda (x) (syntax (lambda (y) (syntax 3))))))
      (let ()
        (define-syntax b a)
        b))
    3)
  (let-syntax ((a (lambda (x) #'(lambda (x) #'3))))
    (define-syntax top-level-b a)
    (eqv? top-level-b 3))
  ;; transformers expressions cannot reference local variables
  (error?
    (let ((a (lambda (x) x)))
      (let-syntax ((b a))
        b)))
  (error?
    (let ((a (lambda (x) x)))
      (letrec-syntax ((b a))
        b)))
  (error?
    (let ((a (lambda (x) x)))
      (fluid-let-syntax ((b a))
        b)))
  (error?
    (let ((a (lambda (x) x)))
      (let ()
        (define-syntax b a)
        b)))
  ;; transformers expressions cannot reference pattern variables
  (error?
    (let-syntax ([foo
                  (lambda (x)
                    (syntax-case x ()
                      [(_ z ...)
                       (let-syntax ([bar (lambda (y) #'(z ...))])
                         (bar))]))])
      (foo + 8 9 10)))
  ;; but can expand into syntax forms containing pattern variable references
  (equal?
    (let-syntax ([foo
                  (lambda (x)
                    (syntax-case x ()
                      [(_ z ...)
                       (let-syntax ([bar (lambda (y) #'#'(z (... ...)))])
                         (bar))]))])
      (foo + 8 9 10))
    27)

  (procedure? (eval (expand '(rec f (lambda (x) x)))))
 ; make sure we're using the right environment for evaluating transformers
  (eq? (let ()
         (define x 3)
         (let-syntax ((x (identifier-syntax (identifier-syntax 4))))
           (define-syntax a x))
         a)
       4)
 ; make sure local-syntax bindings aren't visible outside their scope
  (equal?
    (let ([a 14])
      (module (x y)
        (let-syntax ((a (identifier-syntax 3)))
          (define x a))
        (define y a))
      (cons x y))
    '(3 . 14))
  (begin
    (define $ds-a 14)
    (module ($ds-x $ds-y)
      (letrec-syntax ((a (identifier-syntax 3)))
        (define $ds-x a))
      (define $ds-y $ds-a))
    (equal? (cons $ds-x $ds-y) '(3 . 14)))
 ; make sure both introduced references and defines are scoped the same
  (eq? (let ()
         (define-syntax a (identifier-syntax (begin (define x 3) x)))
         (let () a))
       3)

  (begin
    (define $a 'aaa)
    (define $x 'xxx)
    (define-syntax $introduce-module
      (identifier-syntax
        (begin (module $a ($x) (define $x 73))
               (import $a)
               (eq? $x 73))))
    $introduce-module)
  (eq? $a 'aaa) ; make sure introduced module binding isn't visible
  (eq? $x 'xxx) ; make sure introduced and imported variable isn't visible
  (eq? (top-level-value '$a) 'aaa)
  (eq? (top-level-value '$x) 'xxx)
  (begin
    (define-syntax $dsmat-foo1
      (lambda (x)
        (syntax-case x ()
          ((_ name arg ...)
           (with-syntax (($... (syntax (... ...))))
             (syntax
               (begin
                 (define $dsmat-y 10)
                 (define-syntax name
                   (lambda (z)
                     (syntax-case z ()
                       ((_ a $...)
                        (syntax (list
                                  $dsmat-y
                                  a $...)))))))))))))
    #t)
  (begin ($dsmat-foo1 $dsmat-bar) #t)
  (error? ($dsmat-bar $dsmat-y))
  (begin (define $dsmat-y 77) #t)
  (equal? ($dsmat-bar $dsmat-y) '(10 77))
  (error? ; misplaced ellipsis
    (with-syntax ([x 3]) #'#(... (x))))
  (error? ; missing ellipsis
    (syntax-case '((1 2) (3 4)) () [((x y) ...) #'(quote (x y ...))]))
  (error? ; missing ellipsis
    (syntax-case '((1 2) (3 4)) () [((v w) ...) #'(quote (v ... w))]))
  (equal?
    (let ()
      (define b)
      (define d)
      (define-syntax a
        (lambda (x)
          (syntax-case x (b c)
            [(_ b) "b"]
            [(_ c) "c"]
            [(_ bar) (free-identifier=? #'bar #'d) "d"]
            [(_ bar) (free-identifier=? #'bar #'e) "e"]
            [(_ bar bee)
             (bound-identifier=? #'bar #'bee)
             (symbol->string (datum bar))]
            [_ "nope"])))
      (list (a b) (a c) (a d) (a e) (a b b) (a c c) (a f)))
    '("b" "c" "d" "e" "b" "c" "nope"))
  (equal?
    (let ()
      (define-syntax letrec
        (lambda (x)
          (syntax-case x ()
            [(_ ((i v) ...) e1 e2 ...)
             (with-syntax ([(t ...) (generate-temporaries #'(i ...))])
               #'(let ([i #f] ...)
                   (let ([t v] ...)
                     (set! i t)
                     ...
                     (let () e1 e2 ...))))])))
      (list
        (letrec ([f (lambda (x)
                      (if (zero? x) 'odd (g (- x 1))))]
                 [g (lambda (x) (if (zero? x) 'even (f (- x 1))))])
          (and (eq? (g 10) 'even)
               (eq? (g 13) 'odd)
               (eq? (f 13) 'even)))
        (letrec ([v 0] [k (call/cc (lambda (x) x))])
          (set! v (+ v 1))
          (k (lambda (x) v)))))
    '(#t 1))
  (equal?
    (let ()
      (define-syntax main ; Anton's example
        (lambda (stx)
          (let ((make-swap
                 (lambda (x y)
                   (with-syntax ((x x) (y y) ((t) (generate-temporaries '(*))))
                     (syntax
                       (let ((t1 x))
                         (set! x y)
                         (set! y t1)))))))
            (syntax-case stx ()
              ((_)
               (with-syntax ((swap (make-swap (syntax s) (syntax t))))
                 (syntax
                   (let ((s 1) (t 2))
                     swap
                     (list s t)))))))))
      (main))
    '(2 1))
 ; make sure second definition of marked id works like set!
  (begin
    (define $ds-b '())
    (define-syntax $ds-a
      (lambda (x)
        #'(begin
            (define q 33)
            (define (f) q)
            (set! $ds-b (cons (f) $ds-b))
            (define q 55)
            (set! $ds-b (cons (f) $ds-b))
            (set! $ds-b (cons q $ds-b))
            #t)))
    #t)
  $ds-a
  (equal? $ds-b '(55 55 33))

 ; check underscore as wildcard
  (equal?
    (let ()
      (define-syntax a
        (lambda (x)
          (syntax-case x ()
            [(_ id e)
             #'(let ()
                 (define-syntax id
                   (lambda (x)
                     (syntax-case x ()
                       [(_ q _) #'(list q '_)])))
                 e)])))
      (a xxx (xxx (cons (xxx 3 (/ 1 0)) 4) (/ 1 0))))
    '(((3 _) . 4) _))

  (equal?
    (let ([b 1] [c 2] [d 3] [e 4] [f 5] [g 6])
      (define-syntax a
        (syntax-rules ()
          [(_ x _ y _ z _)
           (list x y 'z '_)]))
      (a b c d e f g))
    '(1 3 f _))
 ; test syntax-rules fender
  (eqv?
    (let ()
      (define-syntax k
        (syntax-rules ()
          [(_ a b) (identifier? #'a) (let ((a (+ b 1))) (* a b))]))
      (let ([x 4]) (k x (+ x 3))))
    88)
  ; test for mishandling of underscore introduced by syntax-rules
  (equal?
    (let ([_ 3])
      (define-syntax a (lambda (x) (syntax-case x (_) [(k _) 4] [(k x) #'(* x x)])))
      (list (a _)))
    '(4))
  (equal?
    (let ([_ 3])
      (define-syntax a (syntax-rules (_) [(k _) 4] [(k x) (* x x)]))
      (list (a _)))
    '(4))
)

(mat r6rs:syntax-rules
  (equal?
    (let ([b 1] [c 2] [d 3] [e 4] [f 5] [g 6])
      (import (rnrs))
      (define-syntax a
        (syntax-rules ()
          [(_ x _ y _ z _)
           (list x y 'z '_)]))
      (a b c d e f g))
    '(1 3 f _))
  (equal?
    (let ()
      (import (rnrs))
      (define-syntax a
        (syntax-rules (b)
          [(_ b) "yup"]
          [(_ c) (list c)]))
      (list (a b) (a 3)))
    '("yup" (3)))
 ; test syntax-rules fender
  (error?
    (let ()
      (import (rnrs))
      (define-syntax k
        (syntax-rules ()
          [(_ a b) (identifier? #'a) (let ((a (+ b 1))) (* a b))]))
      (let ([x 4]) (k x (+ x 3)))))
  (error?
    (let ()
      (import (rnrs))
      (syntax-rules (_))))
  (error? (syntax-rules (_)))
  (error?
    (let ()
      (import (rnrs))
      (syntax-rules (...))))
  (error? (syntax-rules (...)))
  ; test for mishandling of underscore introduced by syntax-rules
  (equal?
    (let ()
      (import (rnrs))
      (let ([_ 3])
        (define-syntax a (syntax-rules (_) [(k _) 4] [(k x) (* x x)]))
        (list (a _))))
    '(4))
)

(mat definition-not-permitted
 ; top level
  (error? ; definition not permitted
    (let-syntax ((frob (lambda (x) #'(void))))
      (define frob 15)))
  (error? ; definition not permitted
    (let-syntax ((frob (lambda (x) #'(void))))
      (define-syntax frob (identifier-syntax 15))))
  (error? ; definition not permitted
    (let-syntax ((frob (lambda (x) #'(void))))
      (module frob (x) (define x 15))))
  (error? ; definition not permitted
    (let-syntax ((frob (lambda (x) #'(void))))
      (alias frob cons)))
 ; top level module body
  (error? ; definition not permitted
    (module (frob)
      (let-syntax ((frob (lambda (x) #'(void))))
        (define frob -15))))
  (error? ; definition not permitted
    (module (frob)
      (let-syntax ((frob (lambda (x) #'(void))))
        (define-syntax frob (identifier-syntax -15)))))
  (error? ; definition not permitted
    (module (frob)
      (let-syntax ((frob (lambda (x) #'(void))))
        (module frob (x) (define x -15)))))
  (error? ; definition not permitted
    (module (frob)
      (let-syntax ((frob (lambda (x) #'(void))))
        (alias frob cons))))
 ; body
  (error? ; definition not permitted
    (let ()
      (let-syntax ((frob (lambda (x) #'(void))))
        (define frob 'xxx))
      frob))
  (error? ; definition not permitted
    (let ()
      (let-syntax ((frob (lambda (x) #'(void))))
        (define-syntax frob (identifier-syntax 'xxx)))
      frob))
  (error? ; definition not permitted
    (let ()
      (let-syntax ((frob (lambda (x) #'(void))))
        (module frob (x) (define x 'xxx)))
      (import frob)
      x))
  (error? ; definition not permitted
    (let ()
      (let-syntax ((frob (lambda (x) #'(void))))
        (alias frob cons))
      (cons 3 4)))
)

(mat invalid-bindings
  (error? (let-syntax ([x '(global)]) x))
  (error? (letrec-syntax ([x '(global)]) x))
  (error? (fluid-let-syntax ([x '(global)]) x))
  (error? (begin (define-syntax x '(global)) x))
  (error? (let () (define-syntax x '(global)) x))
  (error? (let () (let-syntax ([x '(global)]) x)))
  (error? (let () (letrec-syntax ([x '(global)]) x)))
  (error? (let-syntax ([x '(lexical . #\a)]) x))
  (error? (letrec-syntax ([x '(lexical . #\a)]) x))
  (error? (fluid-let-syntax ([x '(lexical . #\a)]) x))
  (error? (begin (define-syntax x '(lexical . #\a)) x))
  (error? (let () (define-syntax x '(lexical . #\a)) x))
  (error? (let () (let-syntax ([x '(lexical . #\a)]) x)))
  (error? (let () (letrec-syntax ([x '(lexical . #\a)]) x)))
  (error? (let-syntax ([x '(macro . cond)]) x))
  (error? (letrec-syntax ([x '(macro . cond)]) x))
  (error? (fluid-let-syntax ([x '(macro . cond)]) x))
  (error? (begin (define-syntax x '(macro . cond)) x))
  (error? (let () (define-syntax x '(macro . cond)) x))
  (error? (let () (let-syntax ([x '(macro . cond)]) x)))
  (error? (let () (letrec-syntax ([x '(macro . cond)]) x)))
)

(mat generalized-pattern
  (begin
    (define-syntax gp$a (syntax-rules () [(_ x ... y) (list y x ...)]))
    #t)
  (error? gp$a)
  (error? (gp$a))
  (error? (gp$a . b))
  (equal? (gp$a 1 2 3 4 5) '(5 1 2 3 4))
  (equal? (gp$a 1) '(1))
  (equal? (gp$a 1 2) '(2 1))
  (begin
    (define-syntax gp$b
      (lambda (x)
        (syntax-case x ()
          [(_ x ... y) #'(list y x ...)])))
    #t)
  (error? gp$b)
  (error? (gp$b))
  (error? (gp$b . b))
  (equal? (gp$b 1 2 3 4 5) '(5 1 2 3 4))
  (equal? (gp$b 1) '(1))
  (equal? (gp$b 1 2) '(2 1))
  (begin
    (define-syntax gp$c
      (syntax-rules ()
        [(_ x ... y z . w) '((x ...) y z w)]))
    #t)
  (error? (gp$c))
  (error? (gp$c 1))
  (equal? (gp$c 1 2) '(() 1 2 ()))
  (equal? (gp$c 1 2 3 4 5) '((1 2 3) 4 5 ()))
  (equal? (gp$c 1 2 . 3) '(() 1 2 3))
  (equal? (gp$c 1 2 3 4 5 . 6) '((1 2 3) 4 5 6))
  (begin
    (define-syntax gp$d
      (syntax-rules (foo)
        [(_ x ... (y z) . #(foo w1 w2)) '((x ...) y z w1 w2)]))
    #t)
  (error? (gp$d 1 2 . #(foo 6 7)))
  (error? (gp$d 1 2))
  (error? (gp$d 1 2 (3 4)))
  (equal? (gp$d (4 5) . #(foo 6 7)) '(() 4 5 6 7))
  (equal? (gp$d 1 (4 5) . #(foo 6 7)) '((1) 4 5 6 7))
  (equal? (gp$d 1 2 3 (4 5) . #(foo 6 7)) '((1 2 3) 4 5 6 7))
  (begin
    (define-syntax gp$e
      (syntax-rules (rats)
        [(_ x ... . rats) '(x ...)]))
    #t)
  (error? (gp$e))
  (error? (gp$e 1))
  (error? (gp$e 1 2))
  (error? (gp$e rats))
  (equal? (gp$e . rats) '())
  (equal? (gp$e 1 . rats) '(1))
  (equal? (gp$e 1 2 3 4 5 . rats) '(1 2 3 4 5))
  (begin
    (define-syntax gp$f
      (syntax-rules (rats)
        [(_ (x ... y) ...) '(x ... ... y ...)]))
    #t)
  (equal? (gp$f) '())
  (equal? (gp$f (1 2 3 4 5) (6 7 8)) '(1 2 3 4 6 7 5 8))
  (error?
    (define-syntax gp$g
      (syntax-rules ()
        [(_ x ... y ...) '(x ... y ...)])))
  (begin
    (define-syntax gp$h
      (syntax-rules (rats)
        [(_ #(x ... y) ...) '(x ... ... y ...)]))
    #t)
  (error? (gp$h (1 2 3)))
  (error? (gp$h . 4))
  (equal? (gp$h) '())
  (equal? (gp$h #(1 2 3 4 5) #(6 7 8)) '(1 2 3 4 6 7 5 8))
)

(mat define-integrable
  (begin
    (define-syntax define-integrable
      (lambda (x)
        (define make-residual-name
          (lambda (name)
            (datum->syntax name
              (string->symbol
                (string-append "residual-"
                  (symbol->string (syntax->datum name)))))))
        (syntax-case x (lambda)
          ((_ name (lambda formals form1 form2 ...))
           (identifier? (syntax name))
           (with-syntax ((xname (make-residual-name (syntax name))))
             (syntax
               (begin
                 (define-syntax name
                   (lambda (x)
                     (syntax-case x ()
                       (_ (identifier? x) (syntax xname))
                       ((_ arg (... ...))
                        (syntax
                          ((fluid-let-syntax
                             ((name (identifier-syntax xname)))
                             (lambda formals form1 form2 ...))
                           arg (... ...)))))))
                 (define xname
                   (fluid-let-syntax ((name (identifier-syntax xname)))
                     (lambda formals form1 form2 ...))))))))))
    #t)
  (let ()
    (define-integrable even? (lambda (x) (if (= x 0) #t (odd? (- x 1)))))
    (define-integrable odd? (lambda (x) (if (= x 0) #f (even? (- x 1)))))
    (and (even? 20) (not (odd? 20))))
  (begin
    (define-syntax define-integrable
      (lambda (x)
        (syntax-case x (lambda)
          [(_ name (lambda formals form1 form2 ...))
           (identifier? #'name)
           #'(begin
               (define-syntax name
                 (lambda (x)
                   (syntax-case x ()
                     [_ (identifier? x) #'xname]
                     [(_ arg (... ...))
                      #'((fluid-let-syntax ([name (identifier-syntax xname)])
                           (lambda formals form1 form2 ...))
                          arg
                          (... ...))])))
               (define xname
                 (fluid-let-syntax ([name (identifier-syntax xname)])
                   (lambda formals form1 form2 ...))))])))
    #t)
  (let ()
    (define-integrable even? (lambda (x) (if (= x 0) #t (odd? (- x 1)))))
    (define-integrable odd? (lambda (x) (if (= x 0) #f (even? (- x 1)))))
    (and (even? 20) (not (odd? 20))))
  (begin
    (define-integrable $di-foo
      (lambda (x) (if (list? x) (map $di-foo x) (list x))))
    (define-integrable $di-bar
      (lambda (x) (if (vector? x) (vector-map $di-bar x) (vector ($di-foo x)))))
    (equal?
      (list ($di-bar '#(a b c)) ($di-bar '(1 2 3)))
      '(#(#((a)) #((b)) #((c))) #(((1) (2) (3))))))
)

(mat identifier-syntax
  (eqv?
    (let ([x 0])
      (define-syntax frob
        (identifier-syntax
          [id (begin (set! x (+ x 1)) x)]
          [(set! id v) (set! x v)]))
      (let ([n (+ frob frob frob)])
        (set! frob 15)
        (+ n frob)))
    22)
  (begin
    (module (($is-frob x))
      (define x 'initial-x)
      (define-syntax $is-frob
        (make-variable-transformer
          (lambda (z)
            (syntax-case z (set!)
              [(set! id e)
               (identifier? #'id)
               #'(set! x e)]
              [id (identifier? #'id) #'(vector x)]
              [(_ a b c ...) #'(set! x (list (cons a b) c ...))])))))
    (equal? $is-frob '#(initial-x)))
  (error? ; invalid syntax
    ($is-frob))
  (error? ; invalid syntax
    ($is-frob 3))
  (error? ; invalid syntax
    (set! $is-frob))
  (error? ; invalid syntax
    (set! $is-frob 3 4))
  (equal?
    (begin
      ($is-frob 3 4)
      $is-frob)
    '#(((3 . 4))))
  (equal?
    (begin
      ($is-frob 3 4 5 6 7)
      $is-frob)
    '#(((3 . 4) 5 6 7)))
  (equal?
    (let ()
      (set! $is-frob 55)
      $is-frob)
    '#(55))
  (equal?
    (let ()
      ($is-frob 'q 'p 'doll)
      $is-frob)
    '#(((q . p) doll)))
  (equal?
    (let ([z (void)])
      (set! $is-frob 44)
      (let ([set! (lambda args (set! z args))])
        (set! $is-frob 15)
        (list z $is-frob)))
    '((#(44) 15) #(44)))
)

(mat with-syntax
   (begin (define-syntax foo
            (lambda (x)
              (syntax-case x ()
                [(_ x ...)
                 (with-syntax ([n (length (syntax (x ...)))])
                   (syntax (list n 'x ...)))])))
          #t)
   (equal? (foo 3 2 1) '(3 3 2 1))
   (equal? (foo 3 2 1) '(3 3 2 1))
   (begin (define-syntax foo
            (lambda (x)
              (syntax-case x ()
                [(_ (x ...) ...)
                 (with-syntax
                   (((len ...) (map length (syntax ((x ...) ...))))
                    (((z ...) ...) (map reverse (syntax ((x ...) ...)))))
                   (syntax '((len z ...) ...)))])))
          #t)
   (equal? (foo) '())
   (equal? (foo (a b) (c d e)) '((2 b a) (3 e d c)))
   (error? (expand '(foo . a)))
   (error? (expand '(foo a)))
   (error? (expand '(foo (a b . c) (d e f))))
   (error? (expand '(foo (a b c) . d)))
   (begin (define-syntax foo
            (lambda (x)
              (syntax-case x ()
                [(_ x ...)
                 (with-syntax ([(y1 y2 ...) (syntax (x ...))])
                   (with-syntax ([(z1 z2) (syntax y1)])
                     (syntax '(z2 z1))))])))
          #t)
   (equal? (foo (a b) (c d) (e f)) '(b a))
   (error? (expand '(foo)))       ;oops: "car: incorrect list structure"
   (error? (expand '(foo a b c))) ;oops: "cadr: incorrect list structure"
   (error? (define-syntax foo
             (lambda (x)
               (syntax-case x ()
                 [(_) (with-syntax ([(x x) '(1 2)]) 0)]))))
   (error? (define-syntax foo
             (lambda (x)
               (syntax-case x ()
                 [(_) (with-syntax ([x 1] [x 2]) 0)]))))
   (equal? (with-syntax ((x 3)) #'#&x) '#&3)
   (equal? (with-syntax ((x 3)) #'#(x)) '#(3))
   (equal? (list (with-syntax () (define x 3) x) 4) '(3 4))
   (equal? (list (with-syntax ([q 3]) (define x #'q) x) 4) '(3 4))
   (equal? (list (with-syntax ([q 3] [r 5]) (define x #'q) (cons x #'r)) 4) '((3 . 5) 4))
 )

(mat generate-temporaries
  (error? (generate-temporaries))
  (error? (generate-temporaries '(a b c) '(d e f)))
  (error? (generate-temporaries '(a b . c)))
  (error? (generate-temporaries (let ([x (list 'a 'b 'c)]) (set-cdr! (cddr x) (cdr x)) x)))
  (andmap identifier? (generate-temporaries '(a b c)))
  (= (length (generate-temporaries '(a b c))) 3)
  (andmap identifier? (generate-temporaries #'(a b c)))
  (= (length (generate-temporaries #'(a b c))) 3)
  (andmap identifier? (generate-temporaries (cons 'q #'(1 2 3))))
  (= (length (generate-temporaries (cons 'q #'(1 2 3)))) 4)
 ; make sure generate-temporaries isn't confused by annotations
  (begin
    (let ((op (open-output-file "testfile.ss" 'replace)))
      (pretty-print
        '(begin
           (define-syntax $gt-a
             (lambda (x)
               (syntax-case x ()
                 [(_ x)
                  (with-syntax ([(t1 t2 t3) (generate-temporaries #'(1 1 1))])
                    #'(define x (let ([t1 17] [t2 53] [t3 -10]) (cons* t2 t3 t1))))])))
           ($gt-a $gt-x))
        op)
        (close-output-port op)
        (compile-file "testfile.ss"))
    #t)
  (begin
    (load "testfile.so")
    #t)
  (equal? $gt-x '(53 -10 . 17))
)

(mat syntax->list
  (error? (syntax->list #'a))
  (error? (syntax->list #'(a b . e)))
  (eq? (syntax->list #'()) '())
  (andmap bound-identifier=? (syntax->list #'(a b c)) (list #'a #'b #'c))
  (not (pair? (car (syntax->list #'((a . b))))))
 ; just for comparison
  (pair? (car (syntax->datum #'((a . b)))))
)

(mat syntax->vector
  (error? (syntax->vector #'a))
  (error? (syntax->vector #'(a b . e)))
  (eq? (syntax->vector #'#()) '#())
  (andmap bound-identifier=? (vector->list (syntax->vector #'#(a b c))) (list #'a #'b #'c))
  (not (pair? (vector-ref (syntax->vector #'#((a . b))) 0)))
 ; just for comparison
  (pair? (vector-ref (syntax->datum #'#((a . b))) 0))
)

(mat syntax-errors
  (begin
    (define $do-one
      (lambda (x)
        (collect (collect-maximum-generation)) ; close ports left open by load/compile-file w/mat's error handler
        (let ((op (open-output-file "testfile.ss" 'replace)))
          (fprintf op "   ~%     ")
          (if (string? x)
              (fprintf op "~a~%" x)
              (parameterize ((pretty-initial-indent 5))
                (pretty-print x op)))
          (close-output-port op))
        (load "testfile.ss")))
    #t)

 ; fix "missing definition for exports" error to be like duplicate-id-error
 ; as is, no character position information is given
  (error? ($do-one '(module (y) (define x 3))))

 ; get no character position information for this
  (error? ($do-one '(let () (define x 3) (define-syntax x (identifier-syntax 4)) x)))

 ; these should possibly give position of invalid/duplicate id, not whole form
  (error? ($do-one '(let () (module (x) (define x 3) (define-syntax x (identifier-syntax 4))) x)))

  (error? ($do-one '(module (x) (define x 3) (define-syntax x (identifier-syntax 4)))))

  (error? ($do-one '(letrec ((3 4)) 5)))

  (error? ($do-one '(letrec-syntax ((3 4)) 5)))

 ; these should be okay:
  (error? ($do-one
            '(module (x)
               (module (x) (define a 1) (define a 2) (define x 3) (define x 4)))))

  (error? ($do-one '(a . b)))

  (error? ($do-one '(module (x) (define x 3) (define x 4))))

  (error? ($do-one '(module (x) (module (x) (define x 3) (define x 4)))))

  (error? ($do-one '(letrec ((x 3) (x 4)) x)))

  (error? ($do-one '(letrec-syntax ((x 3) (x 4)) x)))

  (error? ($do-one '(let () (module (x) (define x 3) (define x 4)) x)))

  (error? ($do-one '(let () (define x 3) (define x 4) x)))

  (error? ($do-one '(cond (a . b))))

  (error? ($do-one '(syntax-case (list 'a 'b) (a) ((_ b b) (syntax b)))))

  (error? ($do-one '(syntax-case (list 'a 'b) (a) ((_ a ...) 3 4 5))))

  (error? ($do-one '(syntax-case (list 'a 'b) (a ...) ((_ a ...) 3))))

  (error? ($do-one '(syntax a b)))

  (error? ($do-one '(if a b c d)))

  (error? ($do-one '(letrec ((x 3) (y 4) . (z 5)) (list x y z))))

  (error? ($do-one '(let () ($primitive 4 car))))

  (error? ($do-one '(syntax-case x)))

  (error? ($do-one '(quote a b)))

  (error? ($do-one '(fluid-let-syntax)))

  (error? ($do-one '(letrec-syntax () . 3)))

  (error? ($do-one '(lambda (x x) x)))

  (error? ($do-one '(lambda (x y) . z)))

  (error? ($do-one '(lambda (3) 3)))

  (error? ($do-one '(let ((x 4)) (set! x 3 5) x)))

  (error? ($do-one '(set! x 3 5)))

  (error? ($do-one '(let () (import . x) 3)))

  (error? ($do-one '(import . x)))

  (error? ($do-one '(let () (import (just scheme cons)))))

  (error? ($do-one '(import (just scheme cons))))

  (error? ($do-one '(module ((a . b)) c)))

  (error? ($do-one '(module (a . b) c)))

  (error? ($do-one '(define x y z)))

  (error? ($do-one '(define-syntax x y z)))

  (error? ($do-one '(case-lambda (()))))

  (error? ($do-one '(import m-not-defined)))

  (error? ($do-one '(let () (import m-not-defined) 3)))

  (error? ($do-one '(module () (import m-not-defined))))

  (error? ($do-one '(lambda (x) (define x 3))))

  (begin
    (define-syntax muck (lambda (x) 'x))
    #t)

  (error? ($do-one '(muck)))

  (error? ($do-one '(eval-when (compile load foo) bar)))

  (error? ($do-one '(let ((x 3) (y . 4)) (+ x y))))

  (error? ($do-one '(begin
                        (define-syntax $a
                          (lambda (x)
                            (syntax-case x ()
                              ((_ a b c)
                               (syntax-case #'(a b c) ()
                                 [(_ x y z) (quote (x y z))])))))
                        ($a 1 2 3))))
 ; [
  (error? ($do-one "'(a b (c d])")) ; )

  (error? ($do-one '(let ()
                      (define-syntax a
                        (lambda (x)
                          (syntax-case x ()
                            [a (datum->syntax #'a '(if 1))])))
                      a)))

  (error? ($do-one '(let ()
                      (define-syntax a
                        (syntax-rules ()
                          [(_ m i)
                           (module m (i)
                             (import m1))]))
                      (module m1 (xxx) (define xxx 155))
                      (a m2 xxx)
                      (let () (import m2) xxx))))

  (error? ($do-one '(let ()
                      (define-syntax a
                        (lambda (q)
                          #'(let ()
                              (define x 5)
                              (define-syntax x
                                (identifier-syntax 5))
                              x)))
                      a)))

  (error? ; attempt to assign immutable variable cons
    ($do-one '(begin
                (set! cons list)
                (cons 1 2 3))))

  (error? ; attempt to assign immutable variable x
    ($do-one
       '(begin
          (library ($selib1) (export (rename (a $selib1-a)))
            (import (rnrs))
            (define x 0)
            (define-syntax a
              (syntax-rules ()
                [(_ n) (begin (set! x (+ x n)) x)])))
          (import ($selib1))
          ($selib1-a 17))))

  (error? ; attempt to assign immutable variable x
    ($do-one
       '(begin
          (library ($selib1) (export (rename (a $selib1-a)))
            (import (rnrs))
            (define x 0)
            (define-syntax a
              (syntax-rules ()
                [(_) (begin (set! x (+ x 1)) x)])))
          (import ($selib1))
          ($selib1-a))))

  (error?
    (mat/cf
      (begin
        (define-syntax err-test
          (syntax-rules ()
            [(_ a b c) (list 'a 'b 'c)]))
        (err-test "wrong # args"))))

  (error? ($do-one '(let () 3 (module foo ()) 4)))
  (error? ($do-one '(let () 3 (module ()) 4)))
  (error? ($do-one '(let () 3 (import scheme) 4)))
  (error? ($do-one '(let () 3 (import-only scheme) 4)))
  (error? ($do-one '(let () 3 (module . foo) 4)))
  (error? ($do-one '(let () 3 (module) 4)))
  (error? ($do-one '(let () 3 (import . scheme) 4)))
  (error? ($do-one '(let () 3 (import-only . scheme) 4)))

  (error? ($do-one '(let () (define-syntax foo (syntax-rules () [(_ e) (if e)])) (foo 17))))

  (error? ($do-one
            `(let ()
               (define-syntax spam
                 (lambda (x)
                   #`(assert (let-syntax ([q '#,(lambda (x) #f)]) q))))
               spam)))
  (error? ($do-one
            '(let ()
               (define-syntax spam
                 (lambda (x)
                   #`(let-values ([(a b) (let-syntax ([q '#,(lambda (x) 3)]) q)])
                       (list a b))))
               spam)))
  (error? ($do-one
            '(let ()
               (define-syntax spam
                 (lambda (x)
                   #'(let ()
                       (define x 0)
                       (define y 1)
                       (define-property x y sort)
                       (let-values ([(a b c) (values x y)])
                         (list a b)))))
               spam)))
 )

; this is identical to the preceding except that $do-one calls compile-file instead
; of load.
(mat syntax-errors2
  (begin
    (define $do-one
      (lambda (x)
        (collect (collect-maximum-generation)) ; close ports left open by load/compile-file w/mat's error handler
        (let ((op (open-output-file "testfile.ss" 'replace)))
          (fprintf op "   ~%     ")
          (if (string? x)
              (fprintf op "~a~%" x)
              (parameterize ((pretty-initial-indent 5))
                (pretty-print x op)))
          (close-output-port op))
        (compile-file "testfile.ss")
        (load "testfile.so")))
    #t)

 ; fix "missing definition for exports" error to be like duplicate-id-error
 ; as is, no character position information is given
  (error? ($do-one '(module (y) (define x 3))))

 ; get no character position information for this
  (error? ($do-one '(let () (define x 3) (define-syntax x (identifier-syntax 4)) x)))

 ; these should possibly give position of invalid/duplicate id, not whole form
  (error? ($do-one '(let () (module (x) (define x 3) (define-syntax x (identifier-syntax 4))) x)))

  (error? ($do-one '(module (x) (define x 3) (define-syntax x (identifier-syntax 4)))))

  (error? ($do-one '(letrec ((3 4)) 5)))

  (error? ($do-one '(letrec-syntax ((3 4)) 5)))

 ; these should be okay:
  (error? ($do-one
            '(module (x)
               (module (x) (define a 1) (define a 2) (define x 3) (define x 4)))))

  (error? ($do-one '(a . b)))

  (error? ($do-one '(module (x) (define x 3) (define x 4))))

  (error? ($do-one '(module (x) (module (x) (define x 3) (define x 4)))))

  (error? ($do-one '(letrec ((x 3) (x 4)) x)))

  (error? ($do-one '(letrec-syntax ((x 3) (x 4)) x)))

  (error? ($do-one '(let () (module (x) (define x 3) (define x 4)) x)))

  (error? ($do-one '(let () (define x 3) (define x 4) x)))

  (error? ($do-one '(cond (a . b))))

  (error? ($do-one '(syntax-case (list 'a 'b) (a) ((_ b b) (syntax b)))))

  (error? ($do-one '(syntax-case (list 'a 'b) (a) ((_ a ...) 3 4 5))))

  (error? ($do-one '(syntax-case (list 'a 'b) (a ...) ((_ a ...) 3))))

  (error? ($do-one '(syntax a b)))

  (error? ($do-one '(if a b c d)))

  (error? ($do-one '(letrec ((x 3) (y 4) . (z 5)) (list x y z))))

  (error? ($do-one '(let () ($primitive 4 car))))

  (error? ($do-one '(syntax-case x)))

  (error? ($do-one '(quote a b)))

  (error? ($do-one '(fluid-let-syntax)))

  (error? ($do-one '(letrec-syntax () . 3)))

  (error? ($do-one '(lambda (x x) x)))

  (error? ($do-one '(lambda (x y) . z)))

  (error? ($do-one '(lambda (3) 3)))

  (error? ($do-one '(let ((x 4)) (set! x 3 5) x)))

  (error? ($do-one '(set! x 3 5)))

  (error? ($do-one '(let () (import . x) 3)))

  (error? ($do-one '(import . x)))

  (error? ($do-one '(let () (import (just scheme cons)))))

  (error? ($do-one '(import (just scheme cons))))

  (error? ($do-one '(module ((a . b)) c)))

  (error? ($do-one '(module (a . b) c)))

  (error? ($do-one '(define x y z)))

  (error? ($do-one '(define-syntax x y z)))

  (error? ($do-one '(case-lambda (()))))

  (error? ($do-one '(import m-not-defined)))

  (error? ($do-one '(let () (import m-not-defined) 3)))

  (error? ($do-one '(module () (import m-not-defined))))

  (error? ($do-one '(lambda (x) (define x 3))))

  (begin
    (define-syntax muck (lambda (x) 'x))
    #t)

  (error? ($do-one '(muck)))

  (error? ($do-one '(eval-when (compile load foo) bar)))

  (error? ($do-one '(let ((x 3) (y . 4)) (+ x y))))

  (error? ($do-one '(begin
                        (define-syntax $a
                          (lambda (x)
                            (syntax-case x ()
                              ((_ a b c)
                               (syntax-case #'(a b c) ()
                                 [(_ x y z) (quote (x y z))])))))
                        ($a 1 2 3))))
 ; [
  (error? ($do-one "'(a b (c d])")) ; )

  (error? ($do-one '(let ()
                      (define-syntax a
                        (lambda (x)
                          (syntax-case x ()
                            [a (datum->syntax #'a '(if 1))])))
                      a)))

  (error? ($do-one '(let ()
                      (define-syntax a
                        (syntax-rules ()
                          [(_ m i)
                           (module m (i)
                             (import m1))]))
                      (module m1 (xxx) (define xxx 155))
                      (a m2 xxx)
                      (let () (import m2) xxx))))

  (error? ($do-one '(let ()
                      (define-syntax a
                        (lambda (q)
                          #'(let ()
                              (define x 5)
                              (define-syntax x
                                (identifier-syntax 5))
                              x)))
                      a)))

  (error? ; ris #f: attempt to assign immutable variable cons
          ; ris #t: incorrect number of arguments to cons
    ($do-one '(begin
                (set! cons list)
                (set! cons #%cons)
                (cons 1 2 3))))

  (error? ; attempt to assign immutable variable x
    ($do-one
       '(begin
          (library ($selib1) (export (rename (a $selib1-a)))
            (import (rnrs))
            (define x 0)
            (define-syntax a
              (syntax-rules ()
                [(_ n) (begin (set! x (+ x n)) x)])))
          (import ($selib1))
          ($selib1-a 17))))

  (error? ; attempt to assign immutable variable x
    ($do-one
       '(begin
          (library ($selib1) (export (rename (a $selib1-a)))
            (import (rnrs))
            (define x 0)
            (define-syntax a
              (syntax-rules ()
                [(_) (begin (set! x (+ x 1)) x)])))
          (import ($selib1))
          ($selib1-a))))

  (error?
    (mat/cf
      (begin
        (define-syntax err-test
          (syntax-rules ()
            [(_ a b c) (list 'a 'b 'c)]))
        (err-test "wrong # args"))))

  (error? ($do-one '(let () 3 (module foo ()) 4)))
  (error? ($do-one '(let () 3 (module ()) 4)))
  (error? ($do-one '(let () 3 (import scheme) 4)))
  (error? ($do-one '(let () 3 (import-only scheme) 4)))
  (error? ($do-one '(let () 3 (module . foo) 4)))
  (error? ($do-one '(let () 3 (module) 4)))
  (error? ($do-one '(let () 3 (import . scheme) 4)))
  (error? ($do-one '(let () 3 (import-only . scheme) 4)))

  (error? ($do-one '(let () (define-syntax foo (syntax-rules () [(_ e) (if e)])) (foo 17))))

  ; make sure we don't get complaints from fasl writer due to procedures in the source
  ; information residualzied for the production of errors
  (error? ($do-one
            `(let ()
               (define-syntax spam
                 (lambda (x)
                   #`(assert (let-syntax ([q '#,(lambda (x) #f)]) q))))
               spam)))
  (error? ($do-one
            '(let ()
               (define-syntax spam
                 (lambda (x)
                   #`(let-values ([(a b) (let-syntax ([q '#,(lambda (x) 3)]) q)])
                       (list a b))))
               spam)))
  (error? ($do-one
            '(let ()
               (define-syntax spam
                 (lambda (x)
                   #'(let ()
                       (define x 0)
                       (define y 1)
                       (define-property x y sort)
                       (let-values ([(a b c) (values x y)])
                         (list a b)))))
               spam)))
 )

(mat define-structure
   (begin
     (define-structure ($tree left node right))
     #t)
   ($tree? (make-$tree 3 4 5))
   (let ((tr (make-$tree 'a 'b 'c)))
      (and (eq? ($tree-left tr) 'a)
           (eq? ($tree-node tr) 'b)
           (eq? ($tree-right tr) 'c)))
   (begin
     (define-structure (pare kar kdr)
        ((original-kar kar) (original-kdr kdr)))
     #t)
   (andmap procedure?
           (list make-pare
                 pare?
                 pare-kar
                 pare-kdr
                 pare-original-kar
                 pare-original-kdr
                 set-pare-kar!
                 set-pare-kdr!
                 set-pare-original-kar!
                 set-pare-original-kdr!))
   (pare? (make-pare 3 4))
   (eq? (pare-kar (make-pare 'a 'b)) 'a)
   (eq? (pare-kdr (make-pare 'a 'b)) 'b)
   (eq? (pare-original-kar (make-pare 'a 'b)) 'a)
   (eq? (pare-original-kdr (make-pare 'a 'b)) 'b)
   (let ((p (make-pare 'a 'b)))
      (set-pare-kar! p 'c)
      (set-pare-kdr! p 'd)
      (and (eq? (pare-kar p) 'c)
           (eq? (pare-kdr p) 'd)
           (eq? (pare-original-kar p) 'a)
           (eq? (pare-original-kdr p) 'b)))
 )

(mat module1
  (begin
    (module $foo ($a) (define $a 4) (define $b 5))
    (import $foo)
    (eq? $a 4))
  (error?
    (begin
      (module $foo ($a) (define $a 4) (define $b 5))
      (import $foo)
      $b))
  (eq? (let ()
         (module $foo ($a) (define $a 4) (define $b 5))
         (import $foo)
         $a)
       4)
  (error?
    (let ()
      (module $foo ($a) (define $a 4) (define $b 5))
      (import $foo)
      $b))
  (begin
    (module $foo ($a)
      (define-syntax $a (identifier-syntax 4))
      (define-syntax $b (identifier-syntax 5)))
    (import $foo)
    (eq? $a 4))
  (error?
    (begin
      (module $foo ($a)
        (define-syntax $a (identifier-syntax 4))
        (define-syntax $b (identifier-syntax 5)))
      (import $foo)
      $b))
  (eq? (let ()
         (module $foo ($a)
           (define-syntax $a (identifier-syntax 4))
           (define-syntax $b (identifier-syntax 5)))
         (import $foo)
         $a)
       4)
  (error?
    (let ()
      (module $foo ($a)
        (define-syntax $a (identifier-syntax 4))
        (define-syntax $b (identifier-syntax 5)))
      (import $foo)
      $b))
  (begin
    (module $foo (($a $b))
      (define-syntax $a (identifier-syntax $b))
      (define $b 400))
    (import $foo)
    (eq? $a 400))
  (error?
    (begin
      (module $foo ($a)
        (define-syntax $a (identifier-syntax $b))
        (define $b 400))
      (import $foo)
      $a))
  (eq? (let ()
         (module $foo (($a $b))
           (define-syntax $a (identifier-syntax $b))
           (define $b 400))
         (import $foo)
         $a)
       400)
  (eq? (let ()
         (module $foo ($a)
           (define-syntax $a (identifier-syntax $b))
           (define $b 400))
         (import $foo)
         $a)
       400)
  (begin
    (define-syntax anonymous-module
      (syntax-rules ()
        ((_ (exp ...) def ...)
         (begin
           (module $tmp (exp ...) def ...)
           (import $tmp)))))
    (anonymous-module ($x) (define $x 3))
    (eq? $x 3))
  (eq? (let () (anonymous-module ($x) (define $x 3)) $x) 3)
  (begin
    (define $y (lambda () $x))
    (anonymous-module ($x) (define $x 3))
    (eq? ($y) 3))
  (eq? (let ()
         (define $y (lambda () $x))
         (anonymous-module ($x) (define $x 3))
         ($y))
       3)
  (begin
    (anonymous-module (ok)
      (define $y 4)
      (define ok (lambda () $y)))
    (define $y (lambda () (ok)))
    (eq? ($y) 4))
 ; was an error before change to treat top-level begin like a <body>
  (begin
    (define $y (lambda () (rats)))
    (anonymous-module (rats)
      (define $y 4)
      (define rats (lambda () $y)))
    (eqv? ($y) 4))
  (eq? (let ()
         (define $y (lambda () ($x)))
         (anonymous-module ($x)
           (define $y 4)
           (define $x (lambda () $y)))
         ($y))
       4)
  (begin
    (anonymous-module ($a)
      (anonymous-module ($a)
        (define $a 3)))
    (eq? $a 3))
  (begin
    (anonymous-module ($a)
      (anonymous-module (($a $b))
        (define-syntax $a (identifier-syntax $b))
        (define $b 77)))
    (eq? $a 77))
  (begin
    (define-syntax defconst
      (syntax-rules ()
        ((_ $x e)
         (anonymous-module (($x t))
           (define-syntax $x (identifier-syntax t))
           (define t e)))))
    (defconst $a 3)
    (eq? $a 3))
  (error? (set! $a 4))
  (begin
    (module $qq ($q) (defconst $q 53))
    (eq? (let () (import $qq) $q) 53))
  (error? (let () (import $qq) (set! $q 4)))
  (begin (import $qq) (eq? $q 53))
  (error? (set! $q 4))
 ; repeat last set of tests for built-in anonymous modules
  (begin
    (module ($x) (define $x 3))
    (eq? $x 3))
  (eq? (let () (module ($x) (define $x 3)) $x) 3)
  (begin
    (define $y (lambda () $x))
    (module ($x) (define $x 3))
    (eq? ($y) 3))
  (eq? (let ()
         (define $y (lambda () $x))
         (module ($x) (define $x 3))
         ($y))
       3)
  (begin
    (module (ok)
      (define $y 4)
      (define ok (lambda () $y)))
    (define $y (lambda () (ok)))
    (eq? ($y) 4))
 ; was an error before change to treat top-level begin like a <body>
  (begin
    (define $y (lambda () (mice)))
    (module (mice)
      (define $y 4)
      (define mice (lambda () $y)))
    (eqv? ($y) 4))
  (eq? (let ()
         (define $y (lambda () ($x)))
         (module ($x)
           (define $y 4)
           (define $x (lambda () $y)))
         ($y))
       4)
  (begin
    (module ($a)
      (module ($a)
        (define $a 3)))
    (eq? $a 3))
  (begin
    (module ($a)
      (module (($a $b))
        (define-syntax $a (identifier-syntax $b))
        (define $b 77)))
    (eq? $a 77))
  (begin
    (define-syntax defconst
      (syntax-rules ()
        ((_ $x e)
         (module (($x t))
           (define-syntax $x (identifier-syntax t))
           (define t e)))))
    (defconst $a 3)
    (eq? $a 3))
  (error? (set! $a 4))
  (begin
    (module $qq ($q) (defconst $q 53))
    (eq? (let () (import $qq) $q) 53))
  (error? (let () (import $qq) (set! $q 4)))
  (begin (import $qq) (eq? $q 53))
  (error? (set! $q 4))
  (begin
    (module $prom ((del make-$prom) frc)
      (define-syntax del
        (syntax-rules ()
          ((_ exp) (make-$prom (lambda () exp)))))
      (define frc (lambda ($prom) ($prom)))
      (define make-$prom
        (lambda (th)
          (let ([val #f] [forced? #f])
            (lambda ()
              (if forced?
                  val
                  (let ([e (th)]) (set! forced? #t) (set! val e) e)))))))
    (module $tofu ($lazy-let)
      (import $prom)
      (define-syntax $lazy-let
        (lambda (form)
          (syntax-case form ()
            [(_ ((v e) ...) e1 e2 ...)
             #'(let ([v (del e)] ...)
                 (let-syntax ((v (identifier-syntax (frc v))) ...)
                   e1 e2 ...))]))))
    (module $test ($a)
      (import $tofu)
      (define-syntax push!
        (syntax-rules ()
          ((_ $x ls) (set! ls (cons $x ls)))))
      (define $a
        (lambda ()
          (let ((ls '()))
            (let ((w ($lazy-let (($x (begin (push! '$x ls) '$x))
                                 ($y (begin (push! '$y ls) '$y))
                                 ($z (begin (push! '$z ls) '$z)))
                       (if $x (list $x $y) $z))))
              (append w ls))))))
    (equal? (let () (import $test) ($a)) '($x $y $y $x)))
  (begin (import $test) (equal? ($a) '($x $y $y $x)))
  (error? (let () (module () (define $a 3) (define-syntax $a list)) 5))
  (eqv?
    (let ()
      (module $a ($x) (define $x 3) (set! $x (+ $x 1)))
      (import $a)
      $x)
    4)
  (eq? (let ()
         (module $foo ($a)
           (module $a ($b)
             (define-syntax $a (identifier-syntax $b))
             (define-syntax $b (identifier-syntax $c))
             (define $c 7)))
         (import $foo)
         (import $a)
         $b)
       7)
  (eq? (let ()
         (module $foo ($a) (module $a ($x) (define $x 3)))
         (import $foo)
         (import $a)
         $x)
       3)
  (begin
    (module $foo ($a) (module $a ($x) (define $x 3)))
    (import $foo)
    (import $a)
    (eq? $x 3))
  (error?
    (begin
      (module $foo ($a)
        (module $a ($b)
          (define-syntax $a (identifier-syntax $b))
          (define-syntax $b (identifier-syntax $c))
          (define $c 7)))
      (import $foo)
      (import $a)
      $b))
  (begin
    (module $foo ($a)
      (module $a (($b $c))
        (define-syntax $a (identifier-syntax $b))
        (define-syntax $b (identifier-syntax $c))
        (define $c 7)))
    (import $foo)
    (import $a)
    (eq? $b 7))
  (error?
    (begin
      (module $foo ($a)
        (module $a (($b $c))
          (define-syntax $a (identifier-syntax $c))
          (define-syntax $b (identifier-syntax $a))
          (define $c 7)))
      (import $foo)
      (import $a)
      (eq? $b 7)))
  (error?
   (begin
    (module $foo ($a)
      (module $a (($b $a))
        (define-syntax $a (identifier-syntax $c))
        (define-syntax $b (identifier-syntax $a))
        (define $c 7)))
    (import $foo)
    (import $a)
    (eq? $b 7)))
  (begin
    (module $foo ($a)
      (module $a (($b ($a $c)))
        (define-syntax $a (identifier-syntax $c))
        (define-syntax $b (identifier-syntax $a))
        (define $c 7)))
    (import $foo)
    (import $a)
    (eq? $b 7))
  (begin
    (module $foo ($a)
      (module $a (($b $a $c))
        (define-syntax $a (identifier-syntax $c))
        (define-syntax $b (identifier-syntax $a))
        (define $c 7)))
    (import $foo)
    (import $a)
    (eq? $b 7))
  (begin
    (module $foo ($a)
      (module $a (($b $a))
        (module (($a $c))
          (define-syntax $a (identifier-syntax $c))
          (define $c 7))
        (define-syntax $b (identifier-syntax $a))))
    (import $foo)
    (import $a)
    (eq? $b 7))
  (error?
    (begin
      (module $foo ($a)
        (define-syntax $a (identifier-syntax $b))
        (define-syntax $b (identifier-syntax 4)))
      (import $foo)
      $a))
  (eq? (let ()
         (module $foo ($a)
           (define-syntax $a (identifier-syntax $b))
           (define-syntax $b (identifier-syntax $c))
           (define $c 7))
         (import $foo)
         $a)
       7)
  (eq? (let ()
         (module $foo ($y)
           (module $x ($y)
             (define-syntax $y (identifier-syntax $z))
             (define $z 4))
           (import $x))
         (import $foo)
         $y)
       4)
  (eq? (let ()
         (module $foo ($y)
           (module $x (($y $z))
             (define-syntax $y (identifier-syntax $z))
             (define $z 4))
           (import $x))
         (import $foo)
         $y)
       4)
  (error?
    (begin
      (module $foo ($y)
        (module $x ($y)
          (define-syntax $y (identifier-syntax $z))
          (define $z 4))
        (import $x))
      (import $foo)
      $y))
  (begin
    (module $foo ($y)
      (module $x (($y $z))
        (define-syntax $y (identifier-syntax $z))
        (define $z 4))
      (import $x))
    (import $foo)
    (eq? $y 4))
  (eq? (let ()
         (module $foo ($y)
           (module $x ($y $z)
             (define-syntax $y (identifier-syntax $z))
             (define $z 4))
           (import $x))
         (import $foo)
         $y)
       4)
  (error?
    (begin
      (module $foo ($y)
        (module $x ($y $z)
          (define-syntax $y (identifier-syntax $z))
          (define $z 44))
        (import $x))
      (import $foo)
      (eq? $y 44)))
  (begin
    (module $foo ($y)
      (module $x (($y $z) $z)
        (define-syntax $y (identifier-syntax $z))
        (define $z 44))
      (import $x))
    (import $foo)
    (eq? $y 44))
  (begin
    (module $foo (($y $z))
      (module $x ($y $z)
        (define-syntax $y (identifier-syntax $z))
        (define $z 44))
      (import $x))
    (import $foo)
    (eq? $y 44))
  (error?
    (let ()
      (module $foo (($y $z))
        (module (($y $z))
          (define-syntax $y (identifier-syntax $z))
          (define $z 4)))
      (import $foo)
      $y))
  (error? ; undefined export $y
    (let ()
      (module $foo (($y $z))
        (define-syntax $y (identifier-syntax $z))
        (module ($y))
        (define $z 4))
      (import $foo)
      $y))
  (error? ; undefined export $z
    (let ()
      (module $foo ($y)
        (module (($y $z))
          (define-syntax $y (identifier-syntax $z)))
        (define $z 4))
      (import $foo)
      $y))
 ; following demonstrates "recursive" modules
  (equal?
    (let ()
      (module $one ($e)
        (define $e (lambda ($x) (or (zero? $x) ($o (- $x 1))))))
      (module $two ($o)
        (define $o (lambda ($x) (not ($e $x)))))
      (import $one)
      (import $two)
      (map (lambda ($x) (list ($o $x) ($e $x))) '(0 1 2 3 4 5)))
    '((#f #t) (#t #f) (#f #t) (#t #f) (#f #t) (#t #f)))
 ; "recursive" modules don't work at top level ...
  (error?
    (begin
      (module $one ($e)
        (define $e (lambda ($x) (or (zero? $x) ($o (- $x 1))))))
      (module $two ($o)
        (define $o (lambda ($x) (not ($e $x)))))
      (import $one)
      (import $two)
      (map (lambda ($x) ($o $x)) '(0 1 2 3 4 5))))
 ; ... unless encapsulated within a top-level module
  (begin
    (module ($e $o)
      (module $one ($e)
        (define $e (lambda ($x) (or (zero? $x) ($o (- $x 1))))))
      (module $two ($o)
        (define $o (lambda ($x) (not ($e $x)))))
      (import $one)
      (import $two))
    (equal?
      (map (lambda ($x) (list ($o $x) ($e $x))) '(0 1 2 3 4 5))
      '((#f #t) (#t #f) (#f #t) (#t #f) (#f #t) (#t #f))))
 ; the following set of tests, as with many others above, highlights the
 ; difference between the flexibility of local and rigidness of global
 ; export rules.  for the global, we need to explicitly list the implicit
 ; exports; for the global, we do not.
  (eq? (let ()
         (module $a ($alpha)
           (define-syntax $alpha (identifier-syntax $x))
           (module $b ($x) (define $x 3))
           (import $b))
         (import $a)
         $alpha)
       3)
  (error?
    (begin
      (module $a ($alpha)
        (define-syntax $alpha (identifier-syntax $x))
        (module $b ($x) (define $x 3))
        (import $b))
      (import $a)
      $alpha))
  (begin
    (module $a (($alpha $x))
      (define-syntax $alpha (identifier-syntax $x))
      (module $b ($x) (define $x 3))
      (import $b))
    (import $a)
    (eq? $alpha 3))
  (equal?
    (let ()
      (define $x "current outer value of $x")
      (let ()
        (module $a ($alpha)
          (define-syntax $alpha (identifier-syntax $x))
          (module $b ($y) (define $y 445) (define $x 3))
          (import $b))
        (import $a)
        $alpha))
    "current outer value of $x")
  (begin
    (define $x "current outer value of $x")
    (module $a ($alpha)
      (define-syntax $alpha (identifier-syntax $x))
      (module $b ($y) (define $y 445) (define $x 3))
      (import $b))
    (import $a)
    (equal? $alpha "current outer value of $x"))
  (begin
    (define-syntax $beta
      (syntax-rules ()
        ((_ x y)
         (begin
           (module x ($beta-a) (define $beta-a 666))
           (import x)
           (define-syntax y (identifier-syntax $beta-a))))))
    (eqv? (let () ($beta q t) t) 666))
  (error? (let () ($beta q t) $beta-a))
  (begin
    (define-syntax $gamma
      (syntax-rules ()
        ((_ x y)
         (begin
           (module x ($aaa) (define $aaa 666))
           (define y (lambda () (import x) $aaa))))))
    (eq? (let () ($gamma q t) (t)) 666))
  (error? (let () ($gamma q t) (import q) $aaa))
  (begin ($gamma $q $t) #t)
  (eqv? ($t) 666)
  (error? (let () (import $q) $aaa))
  (error? (begin (import $q) (eq? $aaa 666)))
  (error?
    (begin
      (define-syntax a
        (lambda (x)
          (syntax-case x ()
            ((_ e) #'(define x e)))))
      (a 3)))
  (error?
    (begin
      (define-syntax a
        (lambda (x)
          (syntax-case x ()
            ((_ e) #'(define-syntax x e)))))
      (a (identifier-syntax 4))))
  (error?
    (begin
      (define-syntax a
          (lambda (x)
            (syntax-case x ()
              ((_ i e) #'(module x (i) (define i e))))))
      (a b 'c)))
  (error? ; defnie not defined
    (module (y) (import-only (rnrs)) (defnie x 3) (define y 4)))
)

(mat module2
  (begin
    (define-syntax $define-structure
      (lambda (x)
        (define construct-name
          (lambda (template-identifier . args)
            (datum->syntax
              template-identifier
              (string->symbol
                (apply string-append
                       (map (lambda (x)
                              (if (string? x)
                                  x
                                  (symbol->string (syntax->datum x))))
                            args))))))
        (syntax-case x ()
          ((_ (name id1 ...))
           (andmap identifier? (syntax (name id1 ...)))
           (with-syntax
             ((constructor (construct-name (syntax name) "make-" (syntax name)))
              (predicate (construct-name (syntax name) (syntax name) "?"))
              ((access ...)
               (map (lambda (x) (construct-name x (syntax name) "-" x))
                    (syntax (id1 ...))))
              ((assign ...)
               (map (lambda (x)
                      (construct-name x "set-" (syntax name) "-" x "!"))
                    (syntax (id1 ...))))
              (structure-length
               (+ (length (syntax (id1 ...))) 1))
              ((index ...)
               (let f ((i 1) (ids (syntax (id1 ...))))
                  (if (null? ids)
                      '()
                      (cons i (f (+ i 1) (cdr ids)))))))
             (syntax (begin
                       (module name (constructor access ...)
                         (define constructor
                           (lambda (id1 ...)
                             (vector 'name id1 ... )))
                         (define access
                           (lambda (x)
                             (vector-ref x index)))
                         ...)
                       (import name))))))))
    (module $foo ($foos build-$foos)
      ($define-structure ($foos x))
      (define (build-$foos) (make-$foos 3)))
    (let ()
      (import $foo)
      (import $foos)
      (define x (build-$foos))
      (define y (make-$foos 4))
      (equal? (list ($foos-x x) ($foos-x y)) '(3 4))))
  (begin
    (import $foo)
    (import $foos)
    (define $x (build-$foos))
    (define $y (make-$foos 4))
    (equal? (list ($foos-x $x) ($foos-x $y)) '(3 4)))
  (let ()
    (define-syntax $define-structure
      (lambda (x)
        (define construct-name
          (lambda (template-identifier . args)
            (datum->syntax
              template-identifier
              (string->symbol
                (apply string-append
                       (map (lambda (x)
                              (if (string? x)
                                  x
                                  (symbol->string (syntax->datum x))))
                            args))))))
        (syntax-case x ()
          ((_ (name id1 ...))
           (andmap identifier? (syntax (name id1 ...)))
           (with-syntax
             ((constructor (construct-name (syntax name) "make-" (syntax name)))
              (predicate (construct-name (syntax name) (syntax name) "?"))
              ((access ...)
               (map (lambda (x) (construct-name x (syntax name) "-" x))
                    (syntax (id1 ...))))
              ((assign ...)
               (map (lambda (x)
                      (construct-name x "set-" (syntax name) "-" x "!"))
                    (syntax (id1 ...))))
              (structure-length
               (+ (length (syntax (id1 ...))) 1))
              ((index ...)
               (let f ((i 1) (ids (syntax (id1 ...))))
                  (if (null? ids)
                      '()
                      (cons i (f (+ i 1) (cdr ids)))))))
             (syntax (begin
                       (module name (constructor access ...)
                         (define constructor
                           (lambda (id1 ...)
                             (vector 'name id1 ... )))
                         (define access
                           (lambda (x)
                             (vector-ref x index)))
                         ...)
                       (import name))))))))
    (module $foo ($foos build-$foos)
      ($define-structure ($foos x))
      (define (build-$foos) (make-$foos 3)))
    (import $foo)
    (import $foos)
    (let ()
      (define x (build-$foos))
      (define y (make-$foos 4))
      (equal? (list ($foos-x x) ($foos-x y)) '(3 4))))
 )

(mat module3
  (equal? (let ()
            (module foo (thing) (define thing #f))
            (define set (lambda (x) (import foo) (set! thing x)))
            (define get (lambda () (import foo) thing))
            (let ([before (get)])
              (set 37)
              (list before (get))))
          '(#f 37))
  (eqv? (let ()
          (module foo (thing) (define thing #f))
          (define get (lambda () (import foo) thing))
          (import foo)
          (set! thing 37)
          (get))
        37)
  (eqv? (let ()
          (define x 45)
          (define-syntax def (identifier-syntax (define x 123)))
          (define-syntax fof (identifier-syntax (let () def x)))
          fof)
        45)
  (eqv? (let ()
          (define x 45)
          (define-syntax def (identifier-syntax (define x 123)))
          (define-syntax fof (identifier-syntax (let () def x)))
          (let () fof))
        45)
  (eqv? (let ()
          (define x 45)
          (define-syntax fof (identifier-syntax (let () (define x 123) x)))
          (let () fof))
        123)
  (eqv? (let ()
          (define x 45)
          (define-syntax def
            (identifier-syntax
              (begin (define x 123) (set! x (+ x x)))))
          (define-syntax fof (identifier-syntax (let () def x)))
          (let () fof))
        45)
  (eqv? (let ()
          (define x 45)
          (define-syntax def
            (syntax-rules ()
              ((_ id) (define id 123))))
          (define-syntax fof (identifier-syntax (let () (def x) x)))
          (let () fof))
        123)
  (eqv? (let ()
          (define x 45)
          (define-syntax fof
            (identifier-syntax
              (let ()
                (define-syntax def (identifier-syntax (define x 123)))
                def
                x)))
          (let () fof))
        45)
  (eqv? (let ()
          (define x 45)
          (define-syntax def (identifier-syntax (define x 123)))
          (define-syntax ref (identifier-syntax x))
          (let () def ref))
        45)
  (eqv? (let ()
          (define x 45)
          (define-syntax fof
            (identifier-syntax
              (let ()
                (define-syntax def
                  (lambda (x)
                    (syntax-case x ()
                      [id
                       (identifier? #'id)
                       (with-syntax ([var (datum->syntax #'id 'x)])
                         #'(define var 123))])))
                def
                x)))
          (let () fof))
        123)
  (eqv? (let ()
          (define x 45)
          (define-syntax zorpon (identifier-syntax define))
          (define-syntax fof (identifier-syntax (let () (zorpon x 123) x)))
          (let () fof))
        123)
  (eqv? (let ()
          (define x 45)
          (define-syntax def (identifier-syntax (zorpon x 123)))
          (define-syntax fof (identifier-syntax (let () def x)))
          (let () (fluid-let-syntax ((zorpon (identifier-syntax define))) fof)))
        45)
  (equal? (let ()
            (module foo (x) (define x 3))
            (define-syntax blah
              (lambda (x)
                (syntax-case x ()
                  [id
                   (identifier? #'id)
                   (with-syntax ([output
                                  (datum->syntax #'id
                                    '(let () (import foo) x))])
                     #'output)])))
            (cons blah (let () blah)))
        '(3 . 3))
  (equal? (let ()
            (module foo (x) (define x 3))
            (module bar (x) (define x 5))
            (define-syntax get
              (lambda (x)
                (syntax-case x ()
                  [(_ mod)
                   (identifier? #'mod)
                   (with-syntax ([var (datum->syntax #'mod 'x)])
                     #'(let () (import mod) var))])))
            (cons (get bar) (let () (get foo))))
          '(5 . 3))
  (equal? (let ()
            (module foo (x) (define x 3))
            (module bar (x) (define x 5))
            (define-syntax get
              (syntax-rules ()
                ((_ mod id) (let () (import mod) id))))
            (cons (get bar x) (let () (get foo x))))
          '(5 . 3))
  (equal? (let ((x 1))
            (module foo (x) (define x 3))
            (module bar (x) (define x 5))
            (define-syntax get-x
              (syntax-rules ()
                ((_ mod) (let () (import mod) x))))
            (cons (get-x bar) (let () (get-x foo))))
          '(1 . 1))
)

(mat module4
  (equal?
    (let ()
      (define-syntax import*
        (lambda (x)
          (syntax-case x ()
            [(_ mid) #'(import mid)]
            [(_ mid s1 s2 ...)
             (with-syntax ((((id ...) d ...)
                            (let f ((ls #'(s1 s2 ...)))
                              (if (null? ls)
                                  '(())
                                  (let ((rest (f (cdr ls))))
                                    (syntax-case (car ls) (as)
                                      [(as id1 id2)
                                       (cons (cons #'id2 (car rest))
                                             (cons #'(define-syntax id2
                                                       (identifier-syntax id1))
                                                   (cdr rest)))]
                                      [id (identifier? #'id)
                                       (cons (cons #'id (car rest))
                                             (cdr rest))]))))))
               #'(module (id ...) (import mid) d ...))])))
      (module m1 (x y) (define x 'x) (define y 'y))
      (list (let () (import* m1) (cons x y))
            (let () (import* m1 x y) (cons x y))
            (let () (import* m1 x) (define y 'yy) (cons x y))
            (let ((x 'outer)) (import* m1 (as x xx) y) (list* x xx y))))
    '((x . y) (x . y) (x . yy) (outer x . y)))
)

(mat module5
  (begin
    (module $zip (a b c)
      (define a 1)
      (define b 123)
      (define-syntax c (identifier-syntax (list a b))))
    (equal? (let () (import $zip) (list a b c))
            '(1 123 (1 123))))
  (eq? (let () (import-only $zip) a) 1)
  (error? (let () (import-only $zip) (list a b c)))
  (error? (let ((z list)) (import-only $zip) (z a b c)))
  (equal?
    (let ()
      (module bar (q r s)
        (import $zip)
        (define q (lambda () a))
        (define-syntax r (identifier-syntax b))
        (define s (lambda () c)))
      (list
        (let () (import bar) (q))
        (let () (import bar) r)
        (let () (import bar) (s))
        (let () (module (r) (import bar)) r)))
    '(1 123 (1 123) 123))
  (error?
    (let ()
      (module bar (q r s)
        (import $zip)
        (define q (lambda () a))
        (define-syntax r (identifier-syntax b))
        (define s (lambda () c)))
      (let ((q "outer")) (module (r) (import bar)) (q))))
  (begin
    (module $zoom (m1 x)
      (define x "this is x")
      (module m1 (x (z y))
        (define x "this is m1's x")
        (define y "this is m1's y")
        (define-syntax z (identifier-syntax y))))
    (equal? (let () (import $zoom) (let ((q x)) (import m1) (list q x z)))
            '("this is x" "this is m1's x" "this is m1's y")))
  (error? (let () (import $zoom) (define q x) (import m1) (list q x z)))
 ; check that we get the right x even though x (et al.) have
 ; multiple properties in the implementation.
  (begin
    (module $foo (x a b c)
      (define x "this is foo's X")
      (define a "this is foo's A")
      (define b "this is foo's B")
      (define c "this is foo's C"))
    (equal?
      (list (let () (import $foo) (list x a))
            (let () (import $foo) (list b c)))
      '(("this is foo's X" "this is foo's A")
        ("this is foo's B" "this is foo's C"))))
  (error? (let () (import $foo) (import $zip) #t))
)

(mat module6
  (begin
    (define-syntax $from1
      (syntax-rules ()
        ((_ m id)
         (let () (import-only m) id))))
    (define-syntax $from2
      (syntax-rules ()
        ((_ m id)
         (let () (module (id) (import m)) id))))
    (define-syntax $from3
      (syntax-rules ()
        [(_ m id)
         (let ([z (cons 1 2)])
           (let ([id z])
             (import m)
             (let ([t id])
               (if (eq? t z) (errorf 'from "~s undefined" 'id) t))))]))
    (module $frappe (wire (whip egg))
      (define wire 3)
      (define-syntax whip (identifier-syntax egg))
      (define egg 'whites))
    (equal?
      (list (cons ($from1 $frappe wire) ($from1 $frappe whip))
            (cons ($from2 $frappe wire) ($from2 $frappe whip))
            (cons ($from3 $frappe wire) ($from3 $frappe whip)))
      '((3 . whites) (3 . whites) (3 . whites))))
  (equal?
    (let ()
      (module q (m from)
        (module m (f) (define f "this is f"))
        (define-syntax from
          (syntax-rules () [(_ m id) (let () (import-only m) id)])))
      (let () (import-only q) (from m f)))
    "this is f")
  (begin
    (module $q (m from)
      (module m (f) (define f "this is f"))
      (define-syntax from
        (syntax-rules () [(_ m id) (let () (import-only m) id)])))
    (equal? (let () (import-only $q) (from m f)) "this is f"))
  (eqv? (let ()
          (module p ((d m) f)
            (define-syntax d
              (syntax-rules ()
                ((_ e) (m (lambda () e)))))
            (define m (lambda (x) x))
            (define f (lambda (th) (th))))
          (let () (import-only p) (f (d 2))))
        2)
  (begin
    (module $p ((d m) f)
      (define-syntax d
        (syntax-rules ()
          ((_ e) (m (lambda () e)))))
      (define m (lambda (x) x))
      (define f (lambda (th) (th))))
    (eqv? (let () (import-only $p) (f (d 2))) 2))
  (error? (let () (import-only $p) (f (d cons))))
)

(mat module7
  (begin (module ($x) (define $x 3) (set! $x (+ $x $x)))
    (eq? $x 6))
  (eq? (let () (module ($x) (define $x 3) (set! $x (+ $x $x))) $x) 6)
)

(mat module8
  (begin
    (module $m ($a $b)
      (define-syntax $a (identifier-syntax 3))
      (define-syntax $b (identifier-syntax $a)))
    (eq? (let ()
           (import $m)
           (fluid-let-syntax (($a (identifier-syntax 4))) $b))
         4))
  (eq? (let ()
         (import $m)
         (fluid-let-syntax (($a (identifier-syntax 4))) $a))
       4)
  (begin
    (import $m)
    (eq? (fluid-let-syntax (($a (identifier-syntax 4))) $b) 4))
  (begin
    (define-syntax $a
      (syntax-rules ()
        ((_ m y z)
         (begin
           (module m ($crazy-x) (define $crazy-x 3731))
           (import m)
           (define y (lambda () $crazy-x))
           (define-syntax z (identifier-syntax $crazy-x))))))
    #t)
  (begin
    ($a $crazy-p $crazy-q $crazy-r)
    (eq? $crazy-r 3731))
  (error? $crazy-x)
  (eq? ($crazy-q) 3731)
  (eq? $crazy-r 3731)
  (begin
    (define-syntax $a1
      (syntax-rules ()
        ((_ m y)
         (module m
           ($flash-x y)
           (define $flash-x "flash")
           (define y (lambda () $flash-x))))))
    #t)
  (begin ($a1 $flash-p $flash-q) #t)
  (begin (import $flash-p) (procedure? $flash-q))
  (error? $flash-x)
  (equal? ($flash-q) "flash")
  (begin
    (define-syntax $c
      (syntax-rules ()
        ((_ y)
         (begin
           (define-syntax $blast-x (identifier-syntax "blast"))
           (define-syntax y (identifier-syntax $blast-x))))))
    #t)
  (begin ($c $blast-y) (equal? $blast-y "blast"))
  (equal? $blast-y "blast")
  (error? $blast-x)
  (begin
    (define-syntax $b
      (syntax-rules ()
        ((_ y) (begin
                 (define $crud-x "crud")
                 (define y (lambda () $crud-x))))))
    #t)
  (begin ($b $crud-y) (procedure? $crud-y))
  (equal? ($crud-y) "crud")
  (error? $crud-x)
  (begin
    (define-syntax $b2
      (syntax-rules ()
        ((_ x y)
         (begin
           (define-syntax x
             (identifier-syntax
               (begin
                 (define $idiot-x "idiot")
                 $idiot-x)))
           (define y (lambda () $idiot-x))))))
    #t)
  (begin ($b2 $idiot-q $idiot-p) (procedure? $idiot-p))
  (equal? (let () $idiot-q) "idiot")
  (begin $idiot-q #t)
  (error? ($idiot-p))
 ; the following should probably generate an error, but doesn't due to
 ; our change in wraps (we apply only the most recent substitution)
 ; (error?
 ;   (begin
 ;     (define-syntax a
 ;       (lambda (?)
 ;         (with-syntax ((xx ((lambda (x) #'x) 4)))
 ;           #'(module (x) (define xx 3)))))
 ;     a))
  (eq? (let ((junk #f))
         (module (a) (import scheme)
            (define-syntax a
              (lambda (x)
                (syntax-case x (foo car)
                  ((_ foo car bar-lit cons-lit)
                   (and (free-identifier=? #'bar-lit #'bar)
                        (free-identifier=? #'cons-lit #'cons))
                   #''yup)))))
         (module () (import scheme)
           (set! junk (a foo car bar cons)))
         junk)
       'yup)
  (error? (let ((junk #f))
            (module (a) (import scheme)
               (define-syntax a
                 (lambda (x)
                   (syntax-case x (foo car)
                     ((_ foo car bar-lit cons-lit)
                      (and (free-identifier=? #'bar-lit #'bar)
                           (free-identifier=? #'cons-lit #'cons))
                      #''yup)))))
            (module () (import scheme)
              (define car 3)
              (set! junk (a foo car bar cons)))
            junk))
)

(mat module9
  (eq? (let () (import-only r5rs) (cond (else 0))) 0)
  (eq? (let () (import-only r5rs-syntax) (cond (else 0))) 0)
  (eq? (let () (import-only ieee) (cond (else 0))) 0)
  (eq? (let () (import-only scheme) (cond (else 0))) 0)
  (eq? (let () (import-only $system) (cond (else 0))) 0)
  (eq? (eval '(cond (else 0)) (scheme-report-environment 5)) 0)
  (eq? (eval '(cond (else 0)) (null-environment 5)) 0)
  (eq? (eval '(cond (else 0)) (interaction-environment)) 0)
  (eq? (eval '(cond (else 0)) (ieee-environment)) 0)
  (equal?
    (let ()
      (import-only scheme)
      (define-record foo ((immutable a)))
      (foo-a (make-foo 3)))
    3)
  (equal? (let ()
            (module foo (a b)
              (define-syntax a
                (syntax-rules (b)
                  ((_ b) "yup")
                  ((_ c) (list c))))
              (define-syntax b
                (lambda (x)
                  (syntax-error x "misplaced aux keyword"))))
            (let ()
              (import-only foo)
              (a (a b))))
          '("yup"))
  (equal? (let ()
            (import-only scheme)
            `(a b ,(+ 3 4) ,@(list 'd 'e)))
          '(a b 7 d e))
 ; assuming internal-defines-as-letrec* defaults to #t
  (internal-defines-as-letrec*)
 ; following tests assume it's set to #f
  (begin (internal-defines-as-letrec* #f) (not (internal-defines-as-letrec*)))
  (error? ; cookie undefined
    (begin
      (module ($b)
        (module (($b getvar))
          (define getvar (lambda () "it worked"))
          (module (($b cookie tmp))
            (define cookie "secret")
            (define tmp cookie)
            (define-syntax $b
              (identifier-syntax
                (if (eq? tmp cookie) (begin (set! tmp (getvar)) tmp) tmp))))))
      (string=? $b "it worked")))
  (begin (internal-defines-as-letrec* #t) (internal-defines-as-letrec*))
  (begin
    (module ($b)
      (module (($b getvar))
        (define getvar (lambda () "it worked"))
        (module (($b cookie tmp))
          (define tmp)
          (define cookie "secret")
          (define-syntax $b
            (identifier-syntax
              (if (eq? tmp cookie) (begin (set! tmp (getvar)) tmp) tmp)))
          (set! tmp cookie))))
    (string=? $b "it worked"))
  (begin
    (module $foo ($b)
      (module bar (($b getvar))
        (module baz (($b cookie tmp))
          (define cookie "secret")
          (define tmp)
          (define-syntax $b
            (identifier-syntax
              (if (eq? tmp cookie) (begin (set! tmp (getvar)) tmp) tmp)))
          (set! tmp cookie))
        (define getvar (lambda () "this also worked"))
        (import baz))
      (import bar))
    (import $foo)
    (string=? $b "this also worked"))
)

(mat module10
  (begin ; make sure we the right binding is exported
    (module ($module10-foo)
      (define $module10-foo "okay")
      (module () (define $module10-foo 'oh-oh)))
    #t)
  (equal? $module10-foo "okay")
  (begin
    (module ($module10-bar)
      (module () (define $module10-bar 'oh-oh))
      (define $module10-bar "fine"))
    #t)
  (equal? $module10-bar "fine")
  (begin
    (module ($module10-qwerty)
      (module ($module10-qwerty)
        (define $module10-qwerty "dandy")))
    #t)
  (equal? $module10-qwerty "dandy")
  (let ()
    (module (foo)
      (define foo "okay")
      (module () (define foo 'oh-oh)))
    (equal? foo "okay"))
  (let ()
    (module (bar)
      (module () (define bar 'oh-oh))
      (define bar "fine"))
    (equal? bar "fine"))
  (let ()
    (module (qwerty)
      (module (qwerty)
        (define qwerty "dandy")))
    (equal? qwerty "dandy"))
)

(mat module11
  (error? ; identifier out of context
    (module (x y)
      (define x 3)
      (define-syntax y (lambda (z) x))))
  (error? ; identifier out of context
    (let ()
      (module (x y)
        (define x 3)
        (define-syntax y (lambda (z) x)))
      y))
)

(mat with-implicit
  (error? ; invalid syntax
    (with-implicit))
  (error? ; invalid syntax
    (with-implicit foo (bar ...) e1 e2))
  (error? ; invalid syntax
    (with-implicit (a b c)))
  (error? ; invalid syntax
    (with-implicit (a b c) . d))
  (error? ; invalid syntax
    (with-implicit (a b c) d . e))
  (error? ; invalid syntax
    (with-implicit (1 2 3) d e))
  (error? ; invalid syntax
    (with-implicit (a 2 c) d e))
  (error? ; 15 is not an identifier
    (with-syntax ([a 15])
      (with-implicit (a b c) d e)))
  (eqv?
    (let ((borf 'borf-outer))
      (define-syntax frob
        (lambda (x)
          (syntax-case x ()
            [k (with-implicit (k borf) #'borf)])))
      frob)
    'borf-outer)
  (equal?
    (let ([borf 'borf-outer])
      (define-syntax frob
        (lambda (x)
          (syntax-case x ()
            [(k e)
             (with-implicit (k borf)
               #'(let () (define borf 'borf-inner) e))])))
      (list borf (frob (list borf))))
    '(borf-outer (borf-inner)))
  (equal?
    (let ()
      (define-syntax for
        (lambda (x)
          (syntax-case x ()
            [(k (e0 e1 e2) b1 b2 ...)
             (with-implicit (k break continue)
               #'(call/cc
                   (lambda (break)
                     e0
                     (let f ()
                       (when e1
                         (call/cc (lambda (continue) b1 b2 ...))
                         e2
                         (f))))))])))
      (define ls-in)
      (define ls-out)
      (for ((begin (set! ls-in '(a b c d e f g h i j)) (set! ls-out '()))
            (not (null? ls-in))
            (set! ls-in (cdr ls-in)))
        (when (memq (car ls-in) '(c e)) (continue))
        (set! ls-out (cons (car ls-in) ls-out))
        (when (memq (car ls-in) '(g j)) (break)))
      ls-out)
    '(g f d b a))
)

(mat datum
  (error? (datum))
  (error? (datum a b c))
  (error? (datum . b))
  (equal? (datum (a b c)) '(a b c))
  (equal?
    (let ()
      (define-syntax ralph
        (lambda (x)
          (syntax-case x ()
            [(k a b)
             (fixnum? (datum a))
             (with-syntax ([q (datum->syntax #'k (make-list (datum a) 15))])
               #'(cons b 'q))]
            [(_ a b) #'(cons 'a 'b)])))
      (list (ralph 3 4) (ralph 3.0 4.0)))
    '((4 15 15 15) (3.0 . 4.0)))
)

(mat alias
  (error? ; invalid syntax
    (alias x "y"))
  (error? ; invalid syntax
    (alias 3 x))
  (eq? (let ((x 2)) (alias y x) y) 2)
  (equal?
    (let ((x "x"))
      (define-syntax fool
        (let ()
          (alias y x)
          (lambda (z) #'y)))
      fool)
    "x")
  (equal?
    (let ()
      (define x "x")
      (alias y x)
      y)
    "x")
  (begin
    (module (($alias-blue blue))
      (define blue "bleu")
      (alias $alias-blue blue))
    (equal? $alias-blue "bleu"))
  (begin
    (define $alias-blot "blot")
    (equal? (let () (alias y $alias-blot) y) "blot"))
  (begin
    (define $alias-f (let () (alias x $alias-blarg) (lambda () x)))
    (procedure? $alias-f))
  (error? ; $alias-blarg not bound
    ($alias-f))
  (begin
    (define $alias-blarg "blarg")
    (equal? ($alias-f) "blarg"))
  (begin
    (define-syntax $alias-blarg (lambda (x) "bloog"))
    (equal? ($alias-f) "blarg"))
  (begin
    (define $alias-g (let () (alias x lambda) (x () "g")))
    (equal? ($alias-g) "g"))
  (begin
    (define $alias-x 3)
    (alias $alias-y $alias-x)
    (eq? $alias-y 3))
  (eq? (let ()
         (define $alias-x 4)
         (alias $alias-y $alias-x)
         $alias-y)
       4)
 ; the following is no longer an error: binding for label is exported
 ; if the alias's identifier is exported
  (begin
    (module ($alias-y)
      (define $alias-x 5)
      (alias $alias-y $alias-x))
    (eq? $alias-y 5))
  (begin
    (module ($alias-y55)
      (define $alias-x55 5)
      (alias $alias-y55 $alias-x55)
      (alias $alias-z55 $alias-x55))
    (eq? $alias-y 5))
  (error? $alias-x55)
  (error? $alias-z55)
  (begin
    (module (($alias-y $alias-x))
      (define $alias-x 6)
      (alias $alias-y $alias-x))
    (eq? $alias-y 6))
  (begin
    (module ($alias-y)
      (module (($alias-y $alias-x))
        (define $alias-x 66)
        (alias $alias-y $alias-x)))
    (eq? $alias-y 66))
  (eq? (let ()
         (module (($alias-y $alias-x))
           (define $alias-x 7)
           (alias $alias-y $alias-x))
         $alias-y)
       7)
  (eq? (let ((x 8))
         (module (y) (alias y x))
         y)
       8)
  (error? ; read-only environment
    (eval '(alias x cons) (scheme-environment)))
  (error? ; read-only environment
    (eval
      '(begin
         (import scheme)
         (alias $alias-cons cons)
         (set! $alias-cons 3))
      (copy-environment (interaction-environment))))
  (error? ; read-only environment
    (eval
      '(begin
         (import scheme)
         (set! cons 3))
      (copy-environment (interaction-environment))))
  (begin
    (module (($i-foo foo))
      (define-record foo ())
      (alias $i-foo foo))
    (define-record $i-bar $i-foo (x))
    ($i-bar? (make-$i-bar 3)))
  (begin
    (module ($i-foo)
      (module m (foo) (define-record foo ()))
      (module g2 (($i-foo g3))
        (module g2 ((g3 foo))
          (import m)
          (alias g3 foo))
        (import g2)
        (alias $i-foo g3))
      (import g2))
    (define-record $i-bar $i-foo (x))
    ($i-bar? (make-$i-bar 3)))
  (begin
    (module $alias-m ($alias:car) (import scheme) (alias $alias:car car))
    (import $alias-m)
    (eqv? ($alias:car '(2.3 4.5 6.7)) 2.3))
  (begin
    (library ($alias-a)
      (export x)
      (import (chezscheme))
      (define y 17)
      (alias x y))
    #t)
  (eqv? (let () (import ($alias-a)) x) 17)
  (error? ; attempt to create an alias to unbound identifier y
    (library ($alias-b)
      (export x)
      (import (chezscheme))
      (alias x y)))
  (error? ; attempt to create an alias to unbound identifier y
    (library ($alias-c)
      (export y)
      (import (chezscheme))
      (alias x y)
      (define y 17)))
  (begin
    (with-output-to-file "testfile-alias-d.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-alias-d)
             (export x)
             (import (chezscheme))
             (alias x y)
             (define y 17))))
      'replace)
    #t)
  (error? ; attempt to create an alias to unbound identifier y
    (compile-file "testfile-alias-d"))
  (error? ; attempt to create an alias to unbound identifier y
    (load "testfile-alias-d.ss"))
  (error? ; attempt to create an alias to unbound identifier y
    (library ($alias-b)
      (export x)
      (import (chezscheme))
      (let () (alias x y) 'hello)))
  (eqv?
    (let ()
      (import-only (chezscheme))
      (define y 17)
      (alias x y)
      x)
    17)
  (error? ; attempt to create an alias to unbound identifier y
    (let ()
      (import-only (chezscheme))
      (alias x y)
      7))
  (error? ; attempt to create an alias to unbound identifier y
    (let ()
      (import-only (chezscheme))
      (alias x y)
      (define y 3)
      7))
  (begin
    (with-output-to-file "testfile-alias-e.ss"
      (lambda ()
        (pretty-print
          '(let ()
             (import-only (chezscheme))
             (alias x y)
             (define y 3)
             7)))
      'replace)
    #t)
  (error? ; attempt to create an alias to unbound identifier y
    (compile-file "testfile-alias-e"))
  (error? ; attempt to create an alias to unbound identifier y
    (load "testfile-alias-e.ss"))
)

(mat extended-import
  (begin
    (module $notscheme (cons car cdr)
      (define cons)
      (define car)
      (define-syntax cdr (identifier-syntax $cdr)))
    #t)
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import scheme)
            (cons car cdr)))))
    (if (= (optimize-level) 3)
        '(#3%cons #3%car #3%cdr)
        '(#2%cons #2%car #2%cdr)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import (only scheme car cdr))
            (cons car cdr)))))
    (if (= (optimize-level) 3)
        '((#3%$top-level-value 'cons) #3%car #3%cdr)
        '((#2%$top-level-value 'cons) #2%car #2%cdr)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import (except scheme car cdr))
            (cons car cdr)))))
    (if (= (optimize-level) 3)
        '(#3%cons (#3%$top-level-value 'car) $cdr)
        '(#2%cons (#2%$top-level-value 'car) $cdr)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import (only (except scheme cdr) car))
            (cons car cdr)))))
    (if (= (optimize-level) 3)
        '((#3%$top-level-value 'cons) #3%car $cdr)
        '((#2%$top-level-value 'cons) #2%car $cdr)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import (add-prefix (only scheme car cdr) scheme:))
            (cons scheme:car cdr)))))
    (if (= (optimize-level) 3)
        '((#3%$top-level-value 'cons) #3%car $cdr)
        '((#2%$top-level-value 'cons) #2%car $cdr)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import (drop-prefix (only scheme car cdr cons) c))
            (ons ar dr)))))
    (if (= (optimize-level) 3)
        '(#3%cons #3%car #3%cdr)
        '(#2%cons #2%car #2%cdr)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import (rename scheme [car xar] [cdr xdr]))
            (cons xar cdr)))))
    (if (= (optimize-level) 3)
        '(#3%cons #3%car $cdr)
        '(#2%cons #2%car $cdr)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '
        (let ()
          (import $notscheme)
          (let ()
            (import (alias scheme [car xar] [cdr xdr]))
            (cons xar cdr)))))
    (if (= (optimize-level) 3)
        '(#3%cons #3%car #3%cdr)
        '(#2%cons #2%car #2%cdr)))
  ; no glob support yet
  #;(equivalent-expansion?
      (parameterize ([#%$suppress-primitive-inlining #f])
        (expand '
          (let ()
            (import $notscheme)
            (let ()
              (import (glob scheme c*r))
              (cons car cdr)))))
    '(cons #2%car #2%cdr))
  (begin
    (module ($i-foo)
      (module m (foo) (define foo 45))
      (import (add-prefix m $i-)))
    (eq? $i-foo 45))
  (begin
    (library ($s) (export $spam)
      (import (scheme))
      (module m (spam) (define spam 3))
      (import (prefix m $)))
    (import ($s))
    (eqv? $spam 3))
  (begin
    (module ($i-foo)
      (module m (m:$i-foo) (define m:$i-foo 57))
      (import (drop-prefix m m:)))
    (eq? $i-foo 57))
  (begin
    (module ($i-foo)
      (module m (bar) (define bar 63))
      (import (rename m (bar $i-foo))))
    (eq? $i-foo 63))
  (begin
    (module ($i-foo)
      (module m (bar) (define bar 75))
      (import (alias m (bar $i-foo))))
    (eq? $i-foo 75))
  (begin
    (module ($i-x $i-y)
      (module m ($i-x $i-y) (define $i-x "x") (define $i-y "y"))
      (import (rename m ($i-y $i-x) ($i-x $i-y))))
    (equal? (list $i-x $i-y) '("y" "x")))
  (error? ; duplicate identifiers $i-x and $i-y
    (begin
      (module ($i-x $i-y)
        (module m ($i-x $i-y) (define $i-x "x") (define $i-y "y"))
        (import (alias m ($i-x $i-y) ($i-y $i-x))))
      (equal? (list $i-x $i-y) '("y" "x"))))
  (error? ; duplicate identifiers $i-x and $i-y
    (let ()
      (module ($i-x $i-y)
        (module m ($i-x $i-y) (define $i-x "x") (define $i-y "y"))
        (import (alias m ($i-x $i-y) ($i-y $i-x))))
      (equal? (list $i-x $i-y) '("y" "x"))))
  (begin
    (module ($i-foo)
      (module m (foo) (define-record foo ()))
      (import (rename m (foo $i-foo))))
    (define-record $i-bar $i-foo (x))
    ($i-bar? (make-$i-bar 3)))
  (let ()
    (module ($i-foo)
      (module m (foo) (define-record foo ()))
      (import (rename m (foo $i-foo))))
    (define-record $i-bar $i-foo (x))
    ($i-bar? (make-$i-bar 3)))
  (begin
    (module ($i-foo)
      (module m (foo) (module foo ($i-x) (define $i-x 14)))
      (import (rename m (foo $i-foo))))
    (import $i-foo)
    (eq? $i-x 14))
  (let ()
    (module ($i-foo)
      (module m (foo) (module foo ($i-x) (define $i-x 14)))
      (import (rename m (foo $i-foo))))
    (import $i-foo)
    (eq? $i-x 14))
  (error? ; y not visible
    (begin
      (module m (x y) (define x 3) (define y 4))
      (let ((x 5) (y 6)) (import-only (only m x)) y)))
  (error? ; y not visible
    (begin
      (module m (x y) (define x 3) (define y 4))
      (let ((x 5) (y 6))
       ; equivalent of (import-only (only m x)):
        (begin
          (module g0 (x) (import-only m))
          (import-only g0))
        y)))
  (begin ; keep with next
    (define $i-grotto-x 7)
    (define $i-grotto-y 8)
    (define $i-grotto-z 9)
    (equal? (list $i-grotto-x $i-grotto-y $i-grotto-z) '(7 8 9)))
  (begin ; keep with preceding
    (module $i-grotto ($i-grotto-x $i-grotto-y $i-grotto-z)
      (define $i-grotto-x 3)
      (define $i-grotto-y 4)
      (define $i-grotto-z 5))
    (import (only $i-grotto $i-grotto-x))
    (equal? (list $i-grotto-x $i-grotto-y $i-grotto-z) '(3 8 9)))
  (begin
    (import (rename (only scheme car) [car $i-car-from-scheme]))
    (eq? ($i-car-from-scheme '(a b c)) 'a))
  (begin
    (import (only (add-prefix scheme $i-scheme:) $i-scheme:list))
    (equal? ($i-scheme:list 3 4 5) '(3 4 5)))
  (begin
    (import (add-prefix (only scheme list) $i-scheme:))
    (equal? ($i-scheme:list 3 4 5) '(3 4 5)))
)

(mat import ; check import semantics changes May 05
  (begin
    (define $imp-x 0)
    (module $imp-m ($imp-x) (define $imp-x 3))
    (define-syntax $imp-from (syntax-rules () [(_ $imp-m $imp-x) (let () (import $imp-m) $imp-x)]))
    (define-syntax $imp-from-m (syntax-rules () [(_ $imp-x) (let () (import $imp-m) $imp-x)]))
    (define-syntax $imp-x-from (syntax-rules () [(_ $imp-m) (let () (import $imp-m) $imp-x)]))
    (define-syntax $imp-x-from-m (syntax-rules () [(_) (let () (import $imp-m) $imp-x)]))
    (define-syntax $imp-module*
      (syntax-rules ()
        [(_ (x ...) d ...)
         (begin (module t (x ...) d ...) (import t))]))
    (define-syntax $imp-import*
      (syntax-rules () [(_ m) (import m)]))
    #t)
  (eqv? ($imp-from $imp-m $imp-x) 3)
  (eqv? ($imp-from-m $imp-x) 0)
  (eqv? ($imp-x-from $imp-m) 0)
  (eqv? ($imp-x-from-m) 3)
  (eqv? (let () ($imp-from $imp-m $imp-x)) 3)
  (eqv? (let () ($imp-from-m $imp-x)) 0)
  (eqv? (let () ($imp-x-from $imp-m)) 0)
  (eqv? (let () ($imp-x-from-m)) 3)
  (eqv? (let () (module $imp-m ($imp-x) (define $imp-x 4)) ($imp-from $imp-m $imp-x)) 4)
  (eqv? (let () (module $imp-m ($imp-x) (define $imp-x 4)) ($imp-from-m $imp-x)) 0)
  (eqv? (let () (module $imp-m ($imp-x) (define $imp-x 4)) ($imp-x-from $imp-m)) 0)
  (eqv? (let () (module $imp-m ($imp-x) (define $imp-x 4)) ($imp-x-from-m)) 3)
  (eqv? (let () (module m (x) (define x 4)) ($imp-import* m) x) 4)
  (eqv? (let () ($imp-module* (x) (define y 5) (define x (lambda () y))) (x)) 5)
  (equal?
    (let ()
      (define-syntax module*
        (syntax-rules ()
          [(_ (x ...) d ...)
           (begin (module t (x ...) d ...) (import t))]))
      (define-syntax import* (syntax-rules () [(_ m) (import m)]))
      (define x 0)
      (module m (x) (define x 3))
      (define-syntax from (syntax-rules () [(_ m x) (let () (import m) x)]))
      (define-syntax from-m (syntax-rules () [(_ x) (let () (import m) x)]))
      (define-syntax x-from (syntax-rules () [(_ m) (let () (import m) x)]))
      (define-syntax x-from-m (syntax-rules () [(_) (let () (import m) x)]))
      (module* (a) (define b 'bee) (define a (lambda () b)))
      (list
        (let () (module m (x) (define x 4)) (from m x))
        (let () (module m (x) (define x 4)) (from-m x))
        (let () (module m (x) (define x 4)) (x-from m))
        (let () (module m (x) (define x 4)) (x-from-m))
        (let () (import* m) x)
        (a)))
    '(4 0 0 3 3 bee))
  (equal?
    (let ()
      (define-syntax alpha
        (syntax-rules ()
          [(_ m v e)
           (let ()
             (module m (v x)
               (define x 'introduced)
               (define v 'supplied))
             (list e (let () (import m) (list v x))))]))
      (let () (alpha q x (let () (import q) x))))
    '(supplied (supplied introduced)))
  (begin
    (module $imp-list ($imp-null? $imp-car $imp-cdr $imp-cons)
      (import (add-prefix (only scheme null? car cdr cons) $imp-)))
    (define-syntax $imp-a
      (syntax-rules ()
        ((_ x) (define-syntax x
                 (lambda (q)
                   (import (only $imp-list $imp-car))
                   #'$imp-car)))))
    ($imp-a $imp-foo)
    (eqv? $imp-foo #%car))
  (eqv?
    (let ()
      (module rat (fink dog) (define fink 'lestein) (define dog 'cat))
      (define-syntax a
        (syntax-rules ()
          ((_ x) (define-syntax x
                   (lambda (q)
                     (import (only rat fink))
                     #'fink)))))
      (a foo)
      foo)
    'lestein)
  (eqv?
    (let ()
      (module rat (fink dog) (define fink 'lestein) (define dog 'cat))
      (define-syntax a
        (syntax-rules ()
          ((_ x) (define-syntax x
                   (lambda (q)
                     (import (add-prefix rat r:))
                     #'r:fink)))))
      (a foo)
      foo)
    'lestein)
  (eqv?
    (let ()
      (module rat (fink dog) (define fink 'lestein) (define dog 'cat))
      (define-syntax a
        (syntax-rules ()
          ((_ x) (define-syntax x
                   (lambda (q)
                     (import (except rat dog))
                     #'fink)))))
      (a foo)
      foo)
    'lestein)
  (eqv?
    (let ()
      (module m (x) (define x 'x1))
      (define-syntax a
        (lambda (q)
          #'(let ([x 'x2])
              (module n (x) (import m))
              (let () (import n) x))))
      a)
    'x1)
  (eqv?
    (let ()
      (module m (x) (define x 'x1))
      (define-syntax a
        (lambda (q)
          #'(let ([x 'x2])
              (import m)
              x)))
      a)
    'x1)
  (error? ; duplicate definition for x
    (let ()
      (module m (x) (define x 'x1))
      (define-syntax a
        (lambda (q)
          #'(let ()
              (define x 'x2)
              (import m)
              x)))
      a))
  (error? ; duplicate definition for x
    (let ()
      (module m (x) (define x 'x1))
      (define-syntax a
        (lambda (q)
          #'(let ()
              (import m)
              (define x 'x2)
              x)))
      a))
  (equal?
    (let ()
      (import scheme)
      (import scheme)
      car)
    car)
  (error? ; "duplicate definition for car
    (let ()
      (import scheme)
      (import (rename scheme (cdr car)))
      car))
  (error? ; duplicate definition for car
    (let ()
      (module (car) (define car 'edsel))
      (import scheme)
      car))
  (error? ; duplicate definition for car
    (let ()
      (define-syntax a
        (lambda (q)
          #'(let ()
              (module (car) (define car 'edsel))
              (import scheme)
              car)))
      a))
  (equal?
    (let ()
      (define-syntax a
        (lambda (q)
          #'(let ()
              (import scheme)
              (import scheme)
              car)))
      a)
    car)
  (error? ; duplicate definition for x
    (let ()
      (define-syntax a
        (lambda (q)
          #'(let ()
              (define x 5)
              (define-syntax x (identifier-syntax 5))
              x)))
      a))
  (error? ; missing definition for export(s) (xxx).
    (let ()
      (define-syntax a
        (syntax-rules ()
          [(_ m i) (module m (i) (import m1))]))
      (module m1 (xxx) (define xxx 155))
      (a m2 xxx)
      (let () (import m2) xxx)))
  (equal?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))])
      (expand/optimize
        '(let-syntax ([a (lambda (x) #'(let () (import scheme) car))])
           a)))
    (if (= (optimize-level) 3) '#3%car '#2%car))
  (equal?
    (parameterize ([#%$suppress-primitive-inlining #f] [run-cp0 (lambda (cp0 x) (cp0 x))])
      (expand/optimize
        '(let-syntax ([a (syntax-rules ()
                           [(_ x)
                            (define-syntax x
                              (lambda (q)
                                (import scheme)
                                #'car))])])
           (a foo)
           foo)))
    (if (= (optimize-level) 3) '#3%car '#2%car))
  (error? ; read-only environment
    (eval '(import (rnrs)) (scheme-environment)))
  (error? ; invalid context for import
    (let ([x (import)]) x))
 ; check 10/27/2010 change to make sense of multiple modules/libraries
 ; within the same import-only form
  (equal?
    (let ()
      (module m1 (x) (define x box))
      (module m2 (y) (define y 772))
      (let ()
        (import-only m1 m2)
        (x y)))
    '#&772)
  (equal?
    (let ()
      (module m1 (x) (define x box))
      (module m2 (y) (define y 772))
      (let ()
        (import m1 m2)
        (x y)))
    '#&772)
  (error? ; unbound identifier list
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import-only m1 m2)
        (list x y))))
  (equal?
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import m1 m2)
        (list x y)))
    '(29 772))
  (equal?
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import-only scheme m1 m2)
        (list x y)))
    '(29 772))
  (equal?
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import scheme m1 m2)
        (list x y)))
    '(29 772))
  (equal?
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import-only (scheme) m1 m2)
        (list x y)))
    '(29 772))
  (equal?
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import (scheme) m1 m2)
        (list x y)))
    '(29 772))
  (equal?
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import-only m1 m2 (scheme))
        (list x y)))
    '(29 772))
  (equal?
    (let ()
      (module m1 (x) (define x 29))
      (module m2 (y) (define y 772))
      (let ()
        (import m1 m2 (scheme))
        (list x y)))
    '(29 772))
  (begin
    (library ($io A) (export p) (import (rnrs)) (define p 17))
    (library ($io B) (export q) (import (rnrs)) (define q (lambda (x) (cons 'q x))))
    (library ($io C) (export r) (import (chezscheme) ($io B))
      (import-only ($io A) (only (rnrs) define *))
      (define r (* p 2)))
    #t)
  (equal?
    (let ()
      (import-only ($io B) ($io C))
      (q r))
    '(q . 34))
  (error? ; unbound identifier p
    (let ()
      (import ($io A))
      (import-only ($io B) ($io C))
      (q p)))
  (begin
    (library ($io A) (export p) (import (rnrs)) (define p 17))
    (library ($io B) (export q) (import (rnrs)) (define q (lambda (x) (cons 'q x))))
    (library ($io C) (export r) (import (chezscheme) ($io B))
      (import ($io A) (only (rnrs) define *))
      (define r (* p 2)))
    #t)
  (equal?
    (let ()
      (import ($io B) ($io C))
      (q r))
    '(q . 34))
  (equal?
    (let ()
      (import ($io A))
      (import ($io B) ($io C))
      (q p))
    '(q . 17))
  (error? ; unbound identifier p
    (begin
      (library ($io A) (export p) (import (rnrs)) (define p 17))
      (library ($io B) (export q) (import (rnrs)) (define q (lambda (x) (cons 'q x))))
      (library ($io C) (export r) (import (chezscheme) ($io A))
        (import-only ($io B) (only (rnrs) define *))
        (define r (* p 2)))))
  (begin
    (library ($io A) (export p) (import (rnrs)) (define p 17))
    (library ($io B) (export q) (import (rnrs)) (define q (lambda (x) (cons 'q x))))
    (library ($io C) (export r) (import (chezscheme) ($io A))
      (import ($io B) (only (rnrs) define *))
      (define r (* p 2)))
    #t)
  (error? ; unbound identifier *
    (begin
      (library ($io A) (export p) (import (rnrs)) (define p 17))
      (library ($io B) (export q) (import (rnrs)) (define q (lambda (x) (cons 'q x))))
      (library ($io C) (export r) (import (chezscheme) ($io A))
        (import-only ($io B) (only (rnrs) define))
        (define r (* p 2)))))
  (begin
    (library ($io A) (export p) (import (rnrs)) (define p 17))
    (library ($io B) (export q) (import (rnrs)) (define q (lambda (x) (cons 'q x))))
    (library ($io C) (export r) (import (chezscheme) ($io A))
      (import ($io B) (only (rnrs) define))
      (define r (* p 2)))
    #t)
 ; check for let-like semantics for import w/multiple subforms
  (eq?
    (let ()
      (module A (B) (module B (x) (define x 'a-b)))
      (module B (x) (define x 'b))
      (let ()
        (import A B)
        x))
    'b)
  (eq?
    (let ()
      (module A (B) (module B (x) (define x 'a-b)))
      (module B (x) (define x 'b))
      (let ()
        (import-only A B)
        x))
    'b)
  (begin
    (library (for mat) (export x) (import (scheme)) (define x 123))
    (library (a b c) (export d) (import (scheme)) (define d 27))
    (library (xd)
      (export ls)
      (import (scheme) (library (for mat)) (library (a b c)))
      (define ls (list x d)))
    #t)
  (equal? 123
    (let ([x 'port])
      (import (library (for mat)))
      x))
  (equal? 27
    (let ([d 'nile])
      (import (library (a b c)))
      d))
  (equal? 27
    (let ([d 'nile])
      (import (a b c))
      d))
  (equal? '(123 27)
    (let ([x 'treme] [d 'nile])
      (import (xd))
      ls))
)

(mat export ; test stand-alone export form
  (error? ; export outside module or library
    (export))
  (error? ; export outside module or library
    (export cons))
  (error? ; export outside module or library
    (top-level-program
      (import (chezscheme))
      (export)))
  (let ()
    (export)
    #t)
  (error? ; nonempty export outside module or library
    (let ()
      (export cons)
      #t))
  (begin
    (module ()
      (define $ex-x 3)
      (export (rename ($ex-x $ex-y) ($ex-y $ex-x)))
      (define $ex-y 4))
    #t)
  (equal?
    (cons $ex-x $ex-y)
    '(4 . 3))
  (begin
    (library ($ex-A) (export) (import (chezscheme))
      (define $ex-x 7)
      (export (rename ($ex-x $ex-y) ($ex-y $ex-x)))
      (define $ex-y 9))
    #t)
  (equal?
    (let ()
      (import ($ex-A))
      (cons $ex-x $ex-y))
    '(9 . 7))
  (begin
    (import ($ex-A))
    #t)
  (equal?
    (cons $ex-x $ex-y)
    '(9 . 7))
  (equal?
    (let ()
      (module ()
        (define $ex-x 3)
        (export (rename ($ex-x $ex-y) ($ex-y $ex-x)))
        (define $ex-y 4))
      (cons $ex-x $ex-y))
    '(4 . 3))
  (begin
    (module $ex-m (x x)
      (define x 5)
      (export x))
    #t)
  (eqv? (let () (import $ex-m) x) 5)
  (eqv?
    (let ()
      (module (x x)
        (define x 5)
        (export x))
      x)
    5)
  (eqv?
    (let ()
      (module (x)
        (define x 5)
        (export x))
      x)
    5)
  (error? ; duplicate export
    (module (x)
      (define x 15)
      (define y 117)
      (export (rename (y x)))))
  (begin
   ; okay to export id twice as long as it has the same binding
    (library ($ex-B) (export x x) (import (chezscheme))
      (define x 25)
      (export x))
    #t)
  (eqv? (let () (import ($ex-B)) x) 25)
  (begin
   ; okay to export id twice as long as it has the same binding
    (library ($ex-B) (export x (rename (x x))) (import (chezscheme))
      (define x 25)
      (export x))
    #t)
  (eqv? (let () (import ($ex-B)) x) 25)
  (begin
   ; okay to export id twice as long as it has the same binding
    (library ($ex-B) (export x (rename (y x))) (import (chezscheme))
      (define x 25)
      (alias y x)
      (export x))
    #t)
  (eqv? (let () (import ($ex-B)) x) 25)
  (begin
    (library ($ex-B) (export x) (import (chezscheme))
      (define x 35)
      (export x))
    #t)
  (eqv? (let () (import ($ex-B)) x) 35)
  (begin
    (import ($ex-B))
    (eqv? x 35))
  (error? ; duplicate export
    (library ($ex-C) (export x) (import (chezscheme))
      (define x 5)
      (define y 17)
      (export (rename (y x)))))
  (equal?
    (let ()
      (module f ((a x y))
        (import (chezscheme))
        (define x 3)
        (define y 4)
        (define-syntax a (identifier-syntax (cons x y)))
        (export a))
      (import f)
      a)
    '(3 . 4))
  (equal?
    (let ()
      (module m ()
        (define x 3)
        (module m1 (x y)
          (define x 4)
          (define-syntax y (identifier-syntax x))
          (indirect-export y x))
        (export (import m1)))
      (let ()
        (import m)
        (list x y)))
    '(4 4))
  (equal?
    (let ()
      (module m ()
        (define x 3)
        (module m1 (x y)
          (define x 4)
          (define-syntax y (identifier-syntax x))
          (indirect-export y x))
        (export (import (only m1 y)) x))
      (let ()
        (import m)
        (list x y)))
    '(3 4))
  (begin
    (define-syntax $ex-export1
      (syntax-rules ()
        [(_ (m id ...)) (export (import (only m id ...)))]
        [(_ id) (export id)]))
    (define-syntax $ex-export
      (syntax-rules ()
        [(_ frob ...) (begin ($ex-export1 frob) ...)]))
    #t)
  (begin
    (module $ex-mm ()
      ($ex-export)
      (define x 3)
      (module m1 ()
        ($ex-export x y)
        (define x 4)
        (define-syntax y (identifier-syntax x))
        (indirect-export y x))
      ($ex-export (m1 y) x))
    #t)
  (equal?
    (let ()
      (import $ex-mm)
      (list x y))
    '(3 4))
  (equal?
    (let ()
      (module m ()
        ($ex-export)
        (define x 3)
        (module m1 ()
          ($ex-export x y)
          (define x 4)
          (define-syntax y (identifier-syntax x))
          (indirect-export y x))
        ($ex-export (m1 y) x))
      (let ()
        (import m)
        (list x y)))
    '(3 4))
  (begin
    (with-output-to-file "testfile-ex1a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex1a)
             (export q)
             (import (chezscheme))
             (define-syntax q (identifier-syntax 17)))))
      'replace)
    (with-output-to-file "testfile-ex1b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex1b)
             (export)
             (import (chezscheme))
             (define x 22)
             (export x (import (testfile-ex1a))))))
      'replace)
    (for-each separate-compile '(ex1a ex1b))
    #t)
  (equal?
    (let () (import (testfile-ex1b)) (list x q))
    '(22 17))
  (begin
    (with-output-to-file "testfile-ex2a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex2a)
             (export q)
             (import (chezscheme))
             (define-syntax q (identifier-syntax 17)))))
      'replace)
    (with-output-to-file "testfile-ex2b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex2b)
             (export)
             (import (chezscheme))
             (define x 22)
             (export (rename (x q)) (import (prefix (rename (testfile-ex2a) (q que)) pi))))))
      'replace)
    (for-each separate-compile '(ex2a ex2b))
    #t)
  (equal?
    (let () (import (testfile-ex2b)) (list q pique))
    '(22 17))
  (begin
    (with-output-to-file "testfile-ex3a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex3a)
             (export q)
             (import (chezscheme))
             (implicit-exports #f)
             (indirect-export a x)
             (define x 17)
             (define-syntax a (identifier-syntax (* x 2)))
             (indirect-export q a)
             (define-syntax q (identifier-syntax (+ a 1))))))
      'replace)
    (with-output-to-file "testfile-ex3b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex3b)
             (export)
             (import (chezscheme))
             (define x 22)
             (export (rename (x q)) (import (prefix (rename (testfile-ex3a) (q que)) pi))))))
      'replace)
    (for-each separate-compile '(ex3a ex3b))
    #t)
  (equal?
    (let () (import (testfile-ex3b)) (list q pique))
    '(22 35))
  (begin
    (with-output-to-file "testfile-ex4a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex4a)
             (export q)
             (import (chezscheme))
             (implicit-exports #f)
             (define x 17)
             (define-syntax a (identifier-syntax (* x 2)))
             (define-syntax q (identifier-syntax (+ a 1))))))
      'replace)
    (with-output-to-file "testfile-ex4b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-ex4b)
             (export)
             (import (chezscheme))
             (define x 22)
             (export (rename (x q)) (import (prefix (rename (testfile-ex4a) (q que)) pi))))))
      'replace)
    (for-each separate-compile '(ex4a ex4b))
    #t)
  (error? ; attempt to reference unexported identifier a
    (let () (import (testfile-ex4b)) (list q pique)))
)

(define eval-test
  (lambda (expr)
    (eval expr)
    #t))
(define load-test
  (lambda (expr)
    (with-output-to-file "testfile.ss"
      (lambda () (pretty-print expr))
      'replace)
    (load "testfile.ss")
    #t))
(define compile-test
  (lambda (expr)
    (with-output-to-file "testfile.ss"
      (lambda () (pretty-print expr))
      'replace)
    (compile-file "testfile.ss")
    (load "testfile.so")
    #t))

(define-syntax errmat
  (lambda (x)
    (syntax-case x ()
      [(_ name expr ...)
       (let ([make-name (lambda (x) (datum->syntax #'name (string->symbol (format "~s-~s" x (datum name)))))])
         #`(begin
             (mat #,(make-name 'eval) (error? (eval-test 'expr)) ...)
             (mat #,(make-name 'load) (error? (load-test 'expr)) ...)
             (mat #,(make-name 'compile) (error? (compile-test 'expr)) ...)))])))

(errmat export-errors
 ; attempt to export multiple bindings for x
  (module A ()
    (define x 5)
    (define y 6)
    (export (rename (y x)) x))
 ; attempt to export multiple bindings for x
  (module ()
    (module A ()
      (define x 5)
      (define y 6)
      (export (rename (y x)) x)))
 ; attempt to export multiple bindings for x
  (let ()
    (module A ()
      (define x 5)
      (define y 6)
      (export (rename (y x)) x))
    0)
 ; attempt to export multiple bindings for x
  (library (A) (export) (import (chezscheme))
    (define x 5)
    (define y 6)
    (export (rename (y x)) x))
 ; attempt to export multiple bindings for x
  (module A ()
    (define x 5)
    (define y 6)
    (export x (rename (y x))))
 ; attempt to export multiple bindings for x
  (module ()
    (module A ()
      (define x 5)
      (define y 6)
      (export x (rename (y x)))))
 ; attempt to export multiple bindings for x
  (let ()
    (module A ()
      (define x 5)
      (define y 6)
      (export x (rename (y x))))
    0)
 ; attempt to export multiple bindings for x
  (library (A) (export) (import (chezscheme))
    (define x 5)
    (define y 6)
    (export x (rename (y x))))
 ; attempt to export multiple bindings for x
  (module A ()
    (define x 5)
    (module B (x) (define x 6))
    (export x (import B)))
 ; attempt to export multiple bindings for x
  (module ()
    (module A ()
      (define x 5)
      (module B (x) (define x 6))
      (export x (import B))))
 ; attempt to export multiple bindings for x
  (let ()
    (module A ()
      (define x 5)
      (module B (x) (define x 6))
      (export x (import B)))
    0)
 ; attempt to export multiple bindings for x
  (library (A) (export) (import (chezscheme))
    (define x 5)
    (module B (x) (define x 6))
    (export x (import B)))
 ; attempt to export multiple bindings for x
  (module A ()
    (define x 5)
    (module B (x) (define x 6))
    (export (import B) x))
 ; attempt to export multiple bindings for x
  (module ()
    (module A ()
      (define x 5)
      (module B (x) (define x 6))
      (export (import B) x)))
 ; attempt to export multiple bindings for x
  (let ()
    (module A ()
      (define x 5)
      (module B (x) (define x 6))
      (export (import B) x))
    0)
 ; attempt to export multiple bindings for x
  (library (A) (export) (import (chezscheme))
    (define x 5)
    (module B (x) (define x 6))
    (export (import B) x))
 ; attempt to export multiple bindings for x
  (module A ()
    (module B (x) (define x 6))
    (module C (x) (define x 7))
    (export (import C) (import B)))
 ; attempt to export multiple bindings for x
  (module ()
    (module A ()
      (module B (x) (define x 6))
      (module C (x) (define x 7))
      (export (import C) (import B))))
 ; attempt to export multiple bindings for x
  (let ()
    (module A ()
      (module B (x) (define x 6))
      (module C (x) (define x 7))
      (export (import C) (import B)))
    0)
 ; attempt to export multiple bindings for x
  (library (A) (export) (import (chezscheme))
    (module B (x) (define x 6))
    (module C (x) (define x 7))
    (export (import C) (import B)))
 ; missing import y
  (module A ()
    (module B (x) (define x 6))
    (export (import (only B y))))
 ; missing import y
  (module ()
    (module A ()
      (module B (x) (define x 6))
      (export (import (only B y)))))
 ; missing import y
  (let ()
    (module A ()
      (module B (x) (define x 6))
      (export (import (only B y))))
    0)
 ; missing import y
  (library (A) (export) (import (chezscheme))
    (module B (x) (define x 6))
    (export (import (only B y))))
 ; missing import y
  (module A ()
    (module B (x) (define x 6))
    (export (import (rename B (y z)))))
 ; missing import y
  (module ()
    (module A ()
      (module B (x) (define x 6))
      (export (import (rename B (y z))))))
 ; missing import y
  (let ()
    (module A ()
      (module B (x) (define x 6))
      (export (import (rename B (y z)))))
    0)
 ; missing import y
  (library (A) (export) (import (chezscheme))
    (module B (x) (define x 6))
    (export (import (rename B (y z)))))
 ; library (rename B y z) not found
  (module A ()
    (module B (x) (define x 6))
    (export (import (rename B y z))))
 ; library (rename B y z) not found
  (module ()
    (module A ()
      (module B (x) (define x 6))
      (export (import (rename B y z)))))
 ; library (rename B y z) not found
  (let ()
    (module A ()
      (module B (x) (define x 6))
      (export (import (rename B y z))))
    0)
 ; library (rename B y z) not found
  (library (A) (export) (import (chezscheme))
    (module B (x) (define x 6))
    (export (import (rename B y z))))
 ; missing expected prefix foo: x
  (module A ()
    (module B (x) (define foo:y 5) (define x 6))
    (export (import (drop-prefix B foo:))))
 ; missing expected prefix foo: x
  (module ()
    (module A ()
      (module B (x) (define foo:y 5) (define x 6))
      (export (import (drop-prefix B foo:)))))
 ; missing expected prefix foo: x
  (let ()
    (module A ()
      (module B (x) (define foo:y 5) (define x 6))
      (export (import (drop-prefix B foo:))))
    0)
 ; missing expected prefix foo: x
  (library (A) (export) (import (chezscheme))
    (module B (x) (define foo:y 5) (define x 6))
    (export (import (drop-prefix B foo:))))
)

(mat indirect-export ; test stand-alone indirect-export form
  (error? ; invalid indirect-export syntax
    (module $ie-f (($ie-a x))
      (import (chezscheme))
      (define x '$ie-x)
      (indirect-export ($ie-a y z))
      (define y '$ie-y)
      (define-syntax $ie-a (identifier-syntax (list x y z)))
      (define z '$ie-z)))
  (error? ; export z undefined
    (module $ie-f (($ie-a x))
      (import (chezscheme))
      (define x '$ie-x)
      (indirect-export $ie-a y z)
      (define y '$ie-y)
      (define-syntax $ie-a (identifier-syntax (list x y z)))))
  (begin
    (module $ie-f ($ie-a)
      (import (chezscheme))
      (define-syntax $ie-a (identifier-syntax (list z)))
      (define z '$ie-z))
    #t)
  (error? ; attempt to reference unexported identifier z
    (let () (import $ie-f) $ie-a))
  (begin
    (module $ie-f (($ie-a z))
      (import (chezscheme))
      (define-syntax $ie-a (identifier-syntax (list z)))
      (define z '$ie-z))
    #t)
  (equal?
    (let () (import $ie-f) $ie-a)
    '($ie-z))
  (begin
    (module $ie-f ($ie-a)
      (import (chezscheme))
      (indirect-export $ie-a z)
      (define-syntax $ie-a (identifier-syntax (list z)))
      (define z '$ie-z))
    #t)
  (equal?
    (let () (import $ie-f) $ie-a)
    '($ie-z))
  (begin
    (module $ie-f ()
      (import (chezscheme))
      (export $ie-a)
      (indirect-export $ie-a z)
      (define-syntax $ie-a (identifier-syntax (list z)))
      (define z '$ie-z))
    #t)
  (equal?
    (let () (import $ie-f) $ie-a)
    '($ie-z))
  (begin
    (module $ie-f ()
      (import (chezscheme))
      (indirect-export $ie-a z)
      (export $ie-a)
      (define-syntax $ie-a (identifier-syntax (list z)))
      (define z '$ie-z))
    #t)
  (equal?
    (let () (import $ie-f) $ie-a)
    '($ie-z))
  (begin
    (module $ie-f (($ie-a x))
      (import (chezscheme))
      (define x '$ie-x)
      (indirect-export $ie-a z)
      (define y '$ie-y)
      (define-syntax $ie-a (identifier-syntax (list x y z)))
      (define z '$ie-z)
      (indirect-export $ie-a y))
    #t)
  (equal?
    (let () (import $ie-f) $ie-a)
    '($ie-x $ie-y $ie-z))
  (begin
    (module $ie-g ()
      (define x 3)
      (define y 4)
      (define-syntax a (identifier-syntax (list x y)))
      (alias b a)
      (export a b)
      (indirect-export a x)
      (indirect-export b y))
    #t)
  (equal?
    (let () (import $ie-g) a)
    '(3 4))
  (begin
    (module $ie-h ((cons x))
      (define-property cons car #'x)
      (define x 3))
    #t)
  (eqv?
    (let ()
      (define-syntax ref-prop
        (lambda (x)
          (lambda (r)
            (syntax-case x ()
              [(_ id key) (r #'id #'key)]))))
      (import $ie-h)
      (ref-prop cons car))
    3)
  (begin
    (module $ie-h (cons)
      (define-property cons car #'x)
      (define x 3))
    #t)
  (error? ; unexported identifier x
    (let ()
      (define-syntax ref-prop
        (lambda (x)
          (lambda (r)
            (syntax-case x ()
              [(_ id key) (r #'id #'key)]))))
      (import $ie-h)
      (ref-prop cons car)))
  (begin
    (module $ie-h (cons)
      (implicit-exports #t)
      (define-property cons car #'x)
      (define x 3))
    #t)
  (eqv?
    (let ()
      (define-syntax ref-prop
        (lambda (x)
          (lambda (r)
            (syntax-case x ()
              [(_ id key) (r #'id #'key)]))))
      (import $ie-h)
      (ref-prop cons car))
    3)
  (error? ; undefine export x
    (library ($ie-i)
      (export a)
      (import (chezscheme))
      (define-syntax a (identifier-syntax x))
      (indirect-export a x)))
)

(mat implicit-exports ; test stand-alone implicit-exports form
  (error? ; invalid syntax
    (implicit-exports))
  (error? ; invalid syntax
    (+ (implicit-exports) 3))
  (error? ; invalid syntax
    (+ (implicit-exports yes!) 3))
  (error? ; invalid syntax
    (+ (implicit-exports no way!) 3))
  (error? ; outside of module or library
    (implicit-exports #t))
  (error? ; invalid context for definition
    (+ (implicit-exports #f) 3))
  (begin
    (module $ie-A (a) (import (chezscheme))
      (define-syntax a (identifier-syntax x))
      (define x 3))
    #t)
  (error? ; unexported identifier x
    (let () (import $ie-A) a))
  (begin
    (module $ie-A (a) (import (chezscheme))
      (implicit-exports #t)
      (define-syntax a (identifier-syntax x))
      (define x 3))
    #t)
  (eqv?
    (let () (import $ie-A) a)
    3)
  (begin
    (module $ie-A (a) (import (chezscheme))
      (implicit-exports #f)
      (define-syntax a (identifier-syntax x))
      (define x 3))
    #t)
  (error? ; unexported identifier x
    (let () (import $ie-A) a))
  (begin
    (library ($ie-A) (export a) (import (chezscheme))
      (define-syntax a (identifier-syntax x))
      (define x 3))
    #t)
  (eqv?
    (let () (import ($ie-A)) a)
    3)
  (begin
    (library ($ie-A) (export a) (import (chezscheme))
      (implicit-exports #f)
      (define-syntax a (identifier-syntax x))
      (define x 3))
    #t)
  (error? ; unexported identifier x
    (let () (import ($ie-A)) a))
  (begin
    (library ($ie-A) (export a) (import (chezscheme))
      (implicit-exports #t)
      (define-syntax a (identifier-syntax x))
      (define x 3))
    #t)
  (eqv?
    (let () (import ($ie-A)) a)
    3)
  (begin
    (module $ie-A (a) (import (chezscheme))
      (module (a)
        (define-syntax a (identifier-syntax x))
        (define x 3)))
    #t)
  (error? ; unexported identifier x
    (let () (import $ie-A) a))
  (begin
    (module $ie-A (a) (import (chezscheme))
      (module ((a x))
        (define-syntax a (identifier-syntax x))
        (define x 3)))
    #t)
  (eqv?
    (let () (import $ie-A) a)
    3)
  (begin
    (module $ie-A (a) (import (chezscheme))
      (module (a)
        (implicit-exports #f)
        (define-syntax a (identifier-syntax x))
        (define x 3)))
    #t)
  (error? ; unexported identifier x
    (let () (import $ie-A) a))
  (begin
    (module $ie-A (a) (import (chezscheme))
      (module (a)
        (implicit-exports #t)
        (define-syntax a (identifier-syntax x))
        (define x 3)))
    #t)
  (eqv?
    (let () (import $ie-A) a)
    3)
  (begin
    (module $ie-B (a) (import (chezscheme))
      (define-syntax a (identifier-syntax x))
      (module (x) (module (x (a x)) (define a 4) (define x 3))))
    #t)
  (error? ; unexported identifier x
    (let () (import $ie-B) a))
  (begin
    (module $ie-B (a) (import (chezscheme))
      (define-syntax a (identifier-syntax x))
      (indirect-export a x)
      (module (x) (module (x (a x)) (define a 4) (define x 3))))
    #t)
  (eqv?
    (let () (import $ie-B) a)
    3)
  (begin
    (module $ie-C (a) (import (chezscheme))
      (module ((b x))
        (define-syntax b (identifier-syntax x))
        (define x 3))
      (alias a b))
    #t)
  (eqv?
    (let () (import $ie-C) a)
    3)
  (begin
    (module $ie-C (a) (import (chezscheme))
      (module (b)
        (define-syntax b (identifier-syntax x))
        (define x 3))
      (alias a b))
    #t)
  (error? ; unexported identifier x
    (let () (import $ie-C) a))
  (begin
    (module $ie-C (a) (import (chezscheme))
      (module (b)
        (indirect-export b x)
        (define-syntax b (identifier-syntax x))
        (define x 3))
      (alias a b))
    #t)
  (eqv?
    (let () (import $ie-C) a)
    3)
  (begin
    (module $ie-D (a)
      (module (a (b x))
        (define-syntax b (identifier-syntax (list x)))
        (module (a x)
          (module (b x)
            (define-syntax b (identifier-syntax x))
            (define x 3))
          (alias a b))))
    #t)
  (error? ; unexported identifier x
    (let () (import $ie-D) a))
  (begin
    (module $ie-E (a)
      (import (chezscheme))
      (define-syntax a (identifier-syntax x))
      (alias b a)
      (indirect-export b x)
      (define x 77))
    #t)
 ; this works because the indirect export of x for b
 ; counts as an indirect export of x for a.  perhaps it
 ; shouldn't work.
  (eqv?
    (let () (import $ie-E) a)
    77)
 ; perhaps this shouldn't work either:
  (eqv?
    (let ()
      (define b 3)
      (alias a b)
      (fluid-let-syntax ([b (identifier-syntax 4)])
        a))
    4)
  (begin
    (module $ie-F (a)
      (import (chezscheme))
      (module (a)
        (implicit-exports #f)
        (define-syntax a (identifier-syntax x)))
      (implicit-exports #t)
      (define x 77))
    #t)
  (eqv?
    (let () (import $ie-F) a)
    77)
  (begin
    (module $ie-G (a)
      (implicit-exports #t)
      (module M1 (x)
        (define x 5))
      (module M2 ((a x))
        (implicit-exports #t)
        (import M1)
        (define-syntax a (identifier-syntax x)))
      (import M2))
    #t)
  (eqv?
    (let () (import $ie-G) a)
    5)
  (begin
    (module $ie-H (a)
      (implicit-exports #t)
      (module M1 (x)
        (define x 5))
      (module M2 (a)
        (implicit-exports #t)
        (define-syntax a (let () (import M1) (identifier-syntax x))))
      (import M2))
    #t)
  (eqv?
    (let () (import $ie-H) a)
    5)
  (begin
    (module $ie-I (a)
      (define x 5)
      (indirect-export a x)
      (module M2 (a)
        (define-syntax a (identifier-syntax x)))
      (import M2))
    #t)
  (eqv?
    (let () (import $ie-I) a)
    5)
  (begin
    (module $ie-J (m)
      (implicit-exports #t)
      (module m (e)
        (define f 44)
        (define-syntax e (identifier-syntax f))))
    #t)
  (error? ; unexported identifier f
    (let ()
      (import $ie-J)
      (import m)
      e))
)

(mat marked-top-level-ids
  (begin
    (define-syntax $a
      (syntax-rules ()
        ((_ x e)
         (begin
           (module ($y-marked) (define $y-marked e))
           (define x (lambda () $y-marked))))))
    ($a $one 1)
    ($a $two 2)
    (equal? (list ($one) ($two)) '(1 2)))
  (not (top-level-bound? '$y-marked))
  (begin
    (define-syntax $a
      (syntax-rules ()
        ((_ x e)
         (begin
           (define $y-marked e)
           (define x (lambda () $y-marked))))))
    ($a $one 1)
    ($a $two 2)
    ($a $three 3)
    (equal? (list ($one) ($two) ($three)) '(1 2 3)))
  (not (top-level-bound? '$y-marked))
  (not (top-level-bound? '$y-marked))
  (begin
    (define-syntax $a
      (syntax-rules ()
        ((_ x e)
         (begin
           (define $y-marked e)
           (define-syntax x (identifier-syntax $y-marked))))))
    ($a $one 1)
    ($a $two 2)
    ($a $three 3)
    ($a $four 4)
    (equal? (list $one $two $three $four) '(1 2 3 4)))
  (begin ; once more, with feeling
    (define-syntax $a
      (syntax-rules ()
        ((_ x e)
         (begin
           (define $y-marked e)
           (define-syntax x (identifier-syntax $y-marked))))))
    ($a $one 1)
    ($a $two 2)
    ($a $three 3)
    ($a $four 4)
    (equal? (list $one $two $three $four) '(1 2 3 4)))
  (begin
    (module $foo ($a) (define-syntax $a (identifier-syntax 3)))
    (import $foo)
    (eq? $a 3))
  (begin ; keep with preceding mat
    (define-syntax $a (identifier-syntax 4))
    (eq? $a 4))
 )

(mat top-level-begin
 ; mats to test change to body-like semantics for begin
  (begin
    (define ($foofrah expr ans)
      (with-output-to-file "testfile.ss"
        (lambda () (pretty-print expr))
        'replace)
      (let* ([ss.out (with-output-to-string (lambda () (load "testfile.ss")))]
             [cf.out (with-output-to-string (lambda () (compile-file "testfile.ss")))]
             [so.out (with-output-to-string (lambda () (load "testfile.so")))])
        (let ([actual
               (list
                 ss.out
                 (substring cf.out
                   (string-length "compiling testfile.ss with output to testfile.so\n")
                   (string-length cf.out))
                 so.out)])
          (unless (equal? actual ans)
            (pretty-print actual)
            (errorf #f "unexpected actual value ~s instead of ~s" actual ans))))
      #t)
    #t)
  ($foofrah
    '(begin
       (define-record-type (a make-a a?) (fields type mapper))
       (define-syntax define-descendant
         (lambda (x)
           (syntax-case x ()
             [(_ parent-id maker type name pred arg ...)
              (with-syntax ([(getter ...) (generate-temporaries #'(arg ...))])
                #'(define-record-type (name maker pred)
                    (parent parent-id)
                    (fields (immutable arg getter) ...)
                    (protocol
                      (lambda (n)
                        (lambda (arg ...)
                          (letrec ([rec ((n 'type (lambda (receiver) (receiver (getter rec) ...))) arg ...)])
                            rec))))))])))
       (define-descendant a make-a subname x x? y z)
       (write ((a-mapper (make-a 3 4)) list)))
    '("(3 4)" "" "(3 4)"))
  ($foofrah
    '(begin
       (eval-when (compile load eval) (write 1))
       (eval-when (compile load eval) (write 2) (write 3))
       (newline))
    '("123\n" "123" "123\n"))
  ($foofrah
    '(begin
       (define (f) (import foo) x1)
       (module foo (x1) (define x1 'x1))
       (pretty-print (f)))
    '("x1\n" "" "x1\n"))
  ($foofrah
    '(begin
       (define x2 'x2)
       (module (y2) (define y2 x2))
       (pretty-print y2)) ;=> x2
    '("x2\n" "" "x2\n"))
  ($foofrah
    '(begin
       (define x3 'x3)
       (module foo (y2) (define y2 x3))
       (import foo)
       (pretty-print y2)) ;=> x3
    '("x3\n" "" "x3\n"))
  ($foofrah
    '(eval-when (compile load)
       (eval-when (compile load eval) (define x4 "x4"))
       (define-syntax a4 (lambda (q) x4))
       (display a4))
    '("" "x4" "x4"))
  ($foofrah
    '(eval-when (compile load eval)
       (define x5 "x5")
       (display x5))
    '("x5" "x5" "x5"))
  (begin
    (define x5 "x5")
    ($foofrah ; keep with preceding test
      '(begin
         (define x5 "x5new")
         (define-syntax a5 (lambda (q) x5))
         (printf "~a ~a\n" a5 x5))
      '("x5 x5new\n" "" "x5new x5new\n")))
  ($foofrah
    '(begin
       (define x6 a6)
       (define-syntax a6 (identifier-syntax 'cool))
       (pretty-print x6))
    '("cool\n" "" "cool\n"))
  (error? ; variable a7 is not bound
    (eval '(begin
             (define x7 a7)
             (define-syntax a7 (identifier-syntax 'cool))
             (define a7 'the-real-deal))))
  ($foofrah
    '(begin
       (define x8 'not-cool)
       (define (f8) x8)
       (define x8 'just-right)
       (pretty-print (f8))) ;=> just-right
    '("just-right\n" "" "just-right\n"))
  ($foofrah
    '(begin
       (define x9 'not-cool)
       (define-syntax a9 (identifier-syntax x9))
       (define x9 'just-right)
       (pretty-print a9)) ;=> just-right
    '("just-right\n" "" "just-right\n"))
  ($foofrah
    '(begin
       (define x10 a10)
       (module m10 (x y)
         (define-syntax x (identifier-syntax 'm10-x))
         (define y a10)
         (define-syntax a10 (identifier-syntax 'm10-y)))
       (library (l10) (export x y) (import (rnrs))
         (define-syntax x (identifier-syntax 'l10-x))
         (define y a10)
         (define-syntax a10 (identifier-syntax 'l10-y)))
       (define-syntax a10 (identifier-syntax 'outer-x10))
       (import (rename m10 (y yy)) (rename (l10) (x xx)))
       (pretty-print (list x y xx yy)))
    '("(m10-x l10-y l10-x m10-y)\n" "" "(m10-x l10-y l10-x m10-y)\n"))
  ($foofrah
    '(begin
       (define-syntax a
         (syntax-rules ()
           [(a q) (begin (define (q) x) (define x 4))]))
       (a zz)
       (pretty-print (zz)))
    '("4\n" "" "4\n"))
  ($foofrah
    '(begin
       (eval-when (compile load eval)
         (module const (get put)
           (define ht (make-eq-hashtable))
           (define get (lambda (name) (hashtable-ref ht name 0)))
           (define put (lambda (name value) (hashtable-set! ht name value)))))
       (define-syntax dc
         (syntax-rules ()
           [(_ id e) (let () (import const) (put 'id e))]))
       (define-syntax con
         (syntax-rules ()
           [(_ id) (let () (import const) (get 'id))]))
       (dc spam 13)
       (dc b (list (con spam) 's))
       (pretty-print (list (con spam) (con b) (con c))))
    '("(13 (13 s) 0)\n" "" "(13 (13 s) 0)\n"))
  (begin (define const) (define dc) (define con) #t)
  ($foofrah
    '(begin
       (eval-when (compile load eval)
         (module const (get put)
           (define ht (make-eq-hashtable))
           (define get (lambda (name) (hashtable-ref ht name 0)))
           (define put (lambda (name value) (hashtable-set! ht name value)))))
       (define-syntax dc
         (syntax-rules ()
           [(_ id e) (let () (import const) (put 'id e))]))
       (define-syntax con
         (syntax-rules ()
           [(_ id) (let () (import const) (get 'id))]))
       (eval-when (compile load eval)
         (dc spam 13)
         (dc b (list (con spam) 's)))
       (eval-when (compile load eval)
         (pretty-print (list (con spam) (con b) (con c)))))
    '("(13 (13 s) 0)\n" "(13 (13 s) 0)\n" "(13 (13 s) 0)\n"))
  (begin (define const) (define dc) (define con) #t)
  ($foofrah
    '(begin
       (eval-when (compile load eval)
         (module const (get put)
           (define ht (make-eq-hashtable))
           (define get (lambda (name) (hashtable-ref ht name 0)))
           (define put (lambda (name value) (hashtable-set! ht name value)))))
       (define-syntax dc
         (syntax-rules ()
           [(_ id e) (eval-when (compile load eval) (let () (import const) (put 'id e)))]))
       (define-syntax con
         (syntax-rules ()
           [(_ id) (eval-when (compile load eval) (let () (import const) (get 'id)))]))
       (dc spam 13)
       (dc b (list (con spam) 's))
       (eval-when (compile load eval)
         (pretty-print (list (con spam) (con b) (con c)))))
    '("(13 (13 s) 0)\n" "(13 (13 s) 0)\n" "(13 (13 s) 0)\n"))
  (begin (define const) (define dc) (define con) #t)
  ($foofrah
    '(begin
       (eval-when (compile eval)
         (module const (get put)
           (define ht (make-eq-hashtable))
           (define get (lambda (name) (hashtable-ref ht name 0)))
           (define put (lambda (name value) (hashtable-set! ht name value)))))
       (define-syntax dc
         (syntax-rules ()
           [(_ id e) (eval-when (compile eval) (let () (import const) (put 'id e)))]))
       (define-syntax con
         (syntax-rules ()
           [(_ id) (eval-when (compile eval) (let () (import const) (get 'id)))]))
       (dc spam 13)
       (dc b (list (con spam) 's))
       (eval-when (compile eval)
         (pretty-print (list (con spam) (con b) (con c)))))
    '("(13 (13 s) 0)\n" "(13 (13 s) 0)\n" ""))
  (begin (define const) (define dc) (define con) #t)
  ($foofrah
    '(begin
       (define-syntax a
         (identifier-syntax
           (begin
             (eval-when (compile eval)
               (module const (get put)
                 (define ht (make-eq-hashtable))
                 (define get (lambda (name) (hashtable-ref ht name 0)))
                 (define put (lambda (name value) (hashtable-set! ht name value)))))
             (define-syntax dc
               (syntax-rules ()
                 [(_ id e) (eval-when (compile eval) (let () (import const) (put 'id e)))]))
             (define-syntax con
               (syntax-rules ()
                 [(_ id) (eval-when (compile eval) (let () (import const) (get 'id)))]))
             (dc spam 13)
             (dc b (list (con spam) 's))
             (eval-when (compile eval)
               (pretty-print (list (con spam) (con b) (con c)))))))
       a)
    '("(13 (13 s) 0)\n" "(13 (13 s) 0)\n" ""))
  (begin (define const) (define dc) (define con) #t)
  (begin
    (with-output-to-file "testfile-lib-c.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-lib-c)
             (export y)
             (import (chezscheme) (testfile-lib-a))
             (define y (lambda () x))
             (printf "invoke c\n"))))
      'replace)
    (with-output-to-file "testfile-test-ac.ss"
      (lambda ()
        (pretty-print
          '(begin
             (library (testfile-lib-a)
               (export x)
               (import (chezscheme))
               (define x (lambda () 1))
               (printf "invoke a\n"))
             (import (testfile-lib-c) (chezscheme))
             (pretty-print (eq? (y) y)))))
      'replace)
    #t)
  (let ([cf '(lambda (x)
               (parameterize ([compile-imported-libraries #t])
                 (compile-file x)))])
    (separate-compile cf 'test-ac)
    #t)
  (equal?
    (separate-eval '(load "testfile-test-ac.so"))
    "invoke a\ninvoke c\n#f\n")
 ; make sure no local-label bindings make it into compiled wraps
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(let-syntax ([a (lambda (x) 0)])
             (define-syntax $foo (lambda (x) #'cons)))))
      'replace)
    (compile-file "testfile")
    (load "testfile.so")
    #t)
  (equal? $foo cons)
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(begin
             (define-syntax $foo-a (lambda (x) 0))
             (define-syntax $foo (lambda (x) #'cons)))))
      'replace)
    (compile-file "testfile")
    (load "testfile.so")
    #t)
  (equal? $foo cons)
)

#;
(mat top-level-begin-NOT
 ; these mats test a behavior we have at this point decided against,
 ; in which a syntax object for an identifier imported from a library
 ; via an import is inserted outside the scope of the local import
 ; in a compiled file, thus forcing an implicit import of the library
 ; when the compiled file is loaded.  possibly, the library should be
 ; imported when a reference is actually attempted, but we shouldn't
 ; import eagerly on the off chance that a syntax object will be used
 ; in this manner, because the import will usually be unnecessary.
  (begin
    (with-output-to-file "testfile-tlb-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-tlb-a1)
             (export tlb-a1-rats)
             (import (rnrs))
             (define-syntax tlb-a1-rats (identifier-syntax 17)))))
      'replace)
    (with-output-to-file "testfile-tlb-a2.ss"
      (lambda ()
        (pretty-print
          '(define-syntax tlb-a2-foo
             (let ()
               (import (testfile-tlb-a1))
               (lambda (x) #'(cons tlb-a1-rats 2))))))
      'replace)
    (with-output-to-file "testfile-tlb-a3.ss"
      (lambda ()
        (pretty-print
          '(let-syntax ([silly (lambda (x)
                                 (import (testfile-tlb-a1))
                                 (syntax-case x ()
                                   [(_ id) #'(define-syntax id (identifier-syntax (cons tlb-a1-rats 3)))]))])
             (silly tlb-a3-fluffy))))
      'replace)
    (with-output-to-file "testfile-tlb-a4.ss"
      (lambda ()
        (pretty-print
          '(module (tlb-a4-pie)
             (import (testfile-tlb-a1))
             (define-syntax tlb-a4-pie
               (lambda (x) #'(cons tlb-a1-rats 4))))))
      'replace)
    (with-output-to-file "testfile-tlb-a5.ss"
      (lambda ()
        (pretty-print
          '(meta define tlb-a5-spam
             (let () (import (testfile-tlb-a1)) #'(cons tlb-a1-rats 5)))))
      'replace)
    (with-output-to-file "testfile-tlb-a6a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-tlb-a6a)
             (export tlb-a6-fop)
             (import (rnrs) (testfile-tlb-a1))
             (define tlb-a6-fop #'(cons tlb-a1-rats 6)))))
      'replace)
    (with-output-to-file "testfile-tlb-a6b.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-tlb-a6b)
             (export tlb-a6-alpha)
             (import (rnrs) (testfile-tlb-a6a))
             (define-syntax tlb-a6-alpha (lambda (x) tlb-a6-fop)))))
      'replace)
    (with-output-to-file "testfile-tlb-a6c.ss"
      (lambda ()
        (pretty-print '(import (rnrs) (testfile-tlb-a6b)))
        (pretty-print '(write tlb-a6-alpha)))
      'replace)
    (with-output-to-file "testfile-tlb-a7.ss"
      (lambda ()
        (pretty-print
          '(define-property spam spam (let () (import (testfile-tlb-a1)) #'(cons tlb-a1-rats 7)))))
      'replace)
    (with-output-to-file "testfile-tlb-a8.ss"
      (lambda ()
        (pretty-print
          '(define tlb-a8-spam (let () (import (testfile-tlb-a1)) #'(cons tlb-a1-rats 8)))))
      'replace)
    (with-output-to-file "testfile-tlb-a9.ss"
      (lambda ()
        (pretty-print
          '(let ()
             (import (testfile-tlb-a1))
             (set! tlb-a9-spam #'(cons tlb-a1-rats 9)))))
      'replace)
    (with-output-to-file "testfile-tlb-a10.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-tlb-a1)))
        (pretty-print '(define-top-level-value 'tlb-a10-spam #'(cons tlb-a1-rats 10))))
      'replace)
    (let ([cf (lambda (what)
                `(lambda (x)
                   (parameterize ([compile-imported-libraries #t])
                     (,what x))))])
      (separate-compile (cf 'compile-file) 'tlb-a2)
      (separate-compile (cf 'compile-file) 'tlb-a3)
      (separate-compile (cf 'compile-file) 'tlb-a4)
      (separate-compile (cf 'compile-file) 'tlb-a5)
      (separate-compile (cf 'compile-library) 'tlb-a6b)
      (separate-compile (cf 'compile-program) 'tlb-a6c)
      (separate-compile (cf 'compile-file) 'tlb-a7)
      (separate-compile (cf 'compile-file) 'tlb-a8)
      (separate-compile (cf 'compile-file) 'tlb-a9)
      (separate-compile (cf 'compile-program) 'tlb-a10))
    #t)
  (equal?
    (separate-eval '(visit "testfile-tlb-a2.so") '(pretty-print tlb-a2-foo))
    "(17 . 2)\n")
  (equal?
    (separate-eval '(visit "testfile-tlb-a3.so") '(pretty-print tlb-a3-fluffy))
    "(17 . 3)\n")
  (equal?
    (separate-eval '(visit "testfile-tlb-a4.so") '(pretty-print tlb-a4-pie))
    "(17 . 4)\n")
  (equal?
    (separate-eval '(visit "testfile-tlb-a5.so") '(pretty-print (let-syntax ([a (lambda (x) tlb-a5-spam)]) a)))
    "(17 . 5)\n")
  (equal?
    (separate-eval '(revisit "testfile-tlb-a6c.so"))
    "(17 . 6)")
  (equal?
    (separate-eval '(visit "testfile-tlb-a7.so") '(pretty-print (let-syntax ([a (lambda (x) (lambda (r) (r #'spam #'spam)))]) a)))
    "(17 . 7)\n")
  (equal?
    (separate-eval '(revisit "testfile-tlb-a8.so") '(pretty-print (let-syntax ([a (lambda (x) tlb-a8-spam)]) a)))
    "(17 . 8)\n")
  (equal?
    (separate-eval '(revisit "testfile-tlb-a9.so") '(pretty-print (let-syntax ([a (lambda (x) tlb-a9-spam)]) a)))
    "(17 . 9)\n")
 ; don't really want to fix this one:
  (equal?
    (separate-eval '(load-program "testfile-tlb-a10.so") '(pretty-print (let-syntax ([a (lambda (x) tlb-a10-spam)]) a)))
    "(17 . 10)\n")
  (begin
    (with-output-to-file "testfile-tlb-bQ.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-tlb-bQ)
             (export tlb-bq)
             (import (rnrs))
             (define-syntax tlb-bq (identifier-syntax 17)))))
      'replace)
    (with-output-to-file "testfile-tlb-bA.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-tlb-bA)
             (export tlb-bset-a! tlb-bget-a)
             (import (rnrs))
             (define a #f)
             (define tlb-bset-a! (lambda (x) (set! a x)))
             (define tlb-bget-a (lambda () a)))))
      'replace)
    (with-output-to-file "testfile-tlb-bP.ss"
      (lambda ()
        (pretty-print '(import (rnrs) (rnrs eval) (testfile-tlb-bQ) (testfile-tlb-bA)))
        (pretty-print '(tlb-bset-a! #'tlb-bq))
        (pretty-print
          '(eval
             '(let ()
                (define-syntax alpha (lambda (x) (tlb-bget-a)))
                (write (cons alpha 'B)))
             (environment '(rnrs) '(testfile-tlb-bA) '(testfile-tlb-bQ)))))
      'replace)
    (let ([cf (lambda (what)
                `(lambda (x)
                   (parameterize ([compile-imported-libraries #t])
                     (,what x))))])
      (separate-compile (cf 'compile-program) 'tlb-bP))
    #t)
  (equal?
    (separate-eval '(load-program "testfile-tlb-bP.so"))
    "(17 . B)")
)

(mat deferred-transformer
 ; don't get caught being lazy on transformer evaluation
  (begin
    (define $ratfink
      (let ([state 0])
        (lambda () (set! state (+ state 1)) (lambda (x) state))))
    (procedure? $ratfink))
  (eqv? (let-syntax ((f ($ratfink)))
          (let-syntax ((g ($ratfink))) g))
        2)
 )

(mat copy-environment
 ; dummy test to set up nondescript record-writer for environments
 ; so that error messages involving environments don't include generated
 ; names that may change from run to run.  the record-writer is reset at
 ; end of this mat.
  (equal?
    (let ([env-rtd (record-rtd (scheme-environment))])
      (set! *saved-record-writer* (record-writer env-rtd))
      (record-writer env-rtd (lambda (x p wr) (display "#<environment>" p)))
      (format "~s" (scheme-environment)))
    "#<environment>")
  (equal?
    (let ([e (copy-environment (scheme-environment))])
      (eval '(define x 17) e)
      (eval '(define-syntax a
               (syntax-rules ()
                 [(_ b c)
                  (begin
                    (define x c)
                    (define-syntax b (identifier-syntax x)))]))
            e)
      (eval '(a foo 33) e)
      (list (eval 'foo e)
            (eval 'x e)
            (top-level-value 'x e)))
    '(33 17 17))
  (equal?
    (let ([e (copy-environment (scheme-environment) #t)])
      (eval '(define x 17) e)
      (eval '(define-syntax a
               (syntax-rules ()
                 [(_ b c)
                  (begin
                    (define x c)
                    (define-syntax b (identifier-syntax x)))]))
            e)
      (eval '(a foo 33) e)
      (list (eval 'foo e)
            (eval 'x e)
            (top-level-value 'x e)))
    '(33 17 17))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (eval '(define x 17) e)
      (eval '(define-syntax a
               (syntax-rules ()
                 [(_ b c)
                  (begin
                    (define x c)
                    (define-syntax b (identifier-syntax x)))]))
            e)
      (eval '(a foo 33) e)
      (list (eval 'foo e)
            (eval 'x e)
            (top-level-value 'x e))))
  (equal?
    (let* ([e1 (copy-environment (scheme-environment))]
           [e2 (copy-environment e1)])
      (define-top-level-value 'list list* e1)
      (list
        (parameterize ([optimize-level 0]) (eval '(list 1 2 3) e1))
        (parameterize ([optimize-level 0]) (eval '(list 1 2 3) e2))))
    '((1 2 . 3) (1 2 3)))
  (equal?
    (let* ([e1 (copy-environment (scheme-environment))]
           [e2 (copy-environment e1)])
      (define-top-level-value 'list list* e1)
      (list
        (parameterize ([optimize-level 0]) (eval '(list 1 2 3) e1))
        (parameterize ([optimize-level 0]) (eval '(list 1 2 3) e2))))
    '((1 2 . 3) (1 2 3)))
  (error?
    (let* ([e1 (copy-environment (scheme-environment))]
           [e2 (copy-environment e1)])
      (set-top-level-value! 'list list* e1)
      (list
        (parameterize ([optimize-level 0]) (eval '(list 1 2 3) e1))
        (parameterize ([optimize-level 0]) (eval '(list 1 2 3) e2)))))
  (equal?
    (let ([e1 (copy-environment (scheme-environment))])
      (define-top-level-value 'curly (lambda (x) (+ x 15)) e1)
      (let ([e2 (copy-environment e1)])
        (define-top-level-value 'curly (lambda (x) (- x 15)) e2)
        (list (eval '(curly 5) e1) (eval '(curly 5) e2))))
    '(20 -10))
  (equal?
    (let ([e1 (copy-environment (scheme-environment))])
      (set-top-level-value! 'curly (lambda (x) (+ x 15)) e1)
      (let ([e2 (copy-environment e1)])
        (set-top-level-value! 'curly (lambda (x) (- x 15)) e2)
        (list (eval '(curly 5) e1) (eval '(curly 5) e2))))
    '(20 -10))
  (equal?
    (let ([e1 (copy-environment (scheme-environment))])
      (define-top-level-value 'curly (lambda (x) (+ x 15)) e1)
      (let ([e2 (copy-environment e1)])
        (define-top-level-value 'curly (lambda (x) (- x 15)) e1)
        (list (eval '(curly 5) e1) (eval '(curly 5) e2))))
    '(-10 20))
  (equal?
    (let ([e1 (copy-environment (scheme-environment))])
      (set-top-level-value! 'curly (lambda (x) (+ x 15)) e1)
      (let ([e2 (copy-environment e1)])
        (set-top-level-value! 'curly (lambda (x) (- x 15)) e1)
        (list (eval '(curly 5) e1) (eval '(curly 5) e2))))
    '(-10 20))
  (equal?
    (let ([e (copy-environment (scheme-environment))])
      (eval '(define let 4) e)
      (define-top-level-value 'let* 6 e)
      (list (top-level-value 'let e)
            (eval '(list let*) e)))
    '(4 (6)))
  (error?
    (let ([e (copy-environment (scheme-environment))])
      (set-top-level-value! letrec 3 e)))
  (error?
    (let ([e (copy-environment (scheme-environment))])
      (set-top-level-value! 'letrec 3 e)))
  (error?
    (let ([e (copy-environment (scheme-environment))])
      (eval '(set! lambda 55) e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (eval '(define cons 55) e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (eval '(set! cons 55) e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (define-top-level-value 'cons 3 e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (set-top-level-value! 'cons 3 e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (define-top-level-value 'frappule 3 e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (set-top-level-value! 'irascible 3 e)))
  (error?
    (let ([e (copy-environment (scheme-environment))])
      (eval 'nonstandard-identifier e)))
  (equal?
    (let ([env-rtd (record-rtd (scheme-environment))])
      (record-writer env-rtd *saved-record-writer*)
      (format "~s" (scheme-environment)))
    "#<environment *scheme*>")
  (equal?
    (let ([e (copy-environment (scheme-environment) #t '())])
      (define-top-level-value 'cons list e)
      (list (eval '(cons 3 4) e) (top-level-bound? 'list e)))
    '((3 4) #f))
  (error?
    (let ([e (copy-environment (scheme-environment) #t '())])
      (eval '(quote 3) e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #t '(scheme))])
      (eval '(import scheme) e)
      (eval '(let ((x 3)) x) e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #t '(import))])
      (eval '(import scheme) e)
      (eval '(let ((x 3)) x) e)))
  (eqv?
    (let ([e (copy-environment (scheme-environment) #t '(import scheme))])
      (eval '(import scheme) e)
      (eval '(let ((x 3)) x) e))
    3)
  (error?
    (let ([e (copy-environment (scheme-environment) #t '(import scheme))])
      (eval '(import scheme) e)
      (set-top-level-value! 'cons 72 e)))
  (begin
    (define $copy-env-tmp1 723)
    (define $copy-env-tmp2 -327)
    (define $copy-env-env
      (copy-environment
        (interaction-environment)
        #t
        (remq 'let*
              (remq 'cons
                    (remq '$copy-env-tmp1
                          (environment-symbols (interaction-environment)))))))
    (environment? $copy-env-env))
  (equal?
    (eval '(let ((x (list 1 2))) (list x x $copy-env-tmp2)) $copy-env-env)
    '(#0=(1 2) #0# -327))
  (error? (eval 'cons $copy-env-env))
  (error? (eval 'let* $copy-env-env))
  (error? (eval '$copy-env-tmp1 $copy-env-env))
  (begin
    (eval '(define + -) $copy-env-env)
    (begin
      (equal? (top-level-value '+ $copy-env-env) -)
      (equal? (eval '+ $copy-env-env) -)
      (equal? (eval '#2%+ $copy-env-env) +)))
  (equal?
    (begin
      (eval '(set! cons 52) $copy-env-env)
      (top-level-value 'cons $copy-env-env))
    52)

 ; verify new (as of csv7.5) copy-environment semantics
  (begin
    (define $ce-e1 (copy-environment (scheme-environment) #t))
    (eval '(module foo (eek) (define eek -7)) $ce-e1)
    (eval '(import foo) $ce-e1)
    (eval '(define-syntax ez (identifier-syntax 'tuary)) $ce-e1)
    (define-top-level-value 'whence 'now $ce-e1)
    #t)
  (equal?
    (eval '(list cons eek whence ez) $ce-e1)
    `(,cons -7 now tuary))
  (begin
    (define $ce-e2 (copy-environment $ce-e1 #t))
    #t)
  (equal?
    (eval '(list cons eek whence ez) $ce-e2)
    `(,cons -7 now tuary))
  (equal?
    (begin
      (eval '(set! eek (* eek 3)) $ce-e1)
      (list (eval '(let () (import foo) eek) $ce-e1)
            (eval '(let () (import foo) eek) $ce-e2)
            (eval 'eek $ce-e1)
            (top-level-value 'eek $ce-e2)))
    '(-21 -21 -21 -21))
  (equal?
    (begin
      (eval '(set! eek (* eek 3)) $ce-e2)
      (list (eval '(let () (import foo) eek) $ce-e1)
            (eval '(let () (import foo) eek) $ce-e2)
            (eval 'eek $ce-e1)
            (top-level-value 'eek $ce-e2)))
    '(-63 -63 -63 -63))
  (equal?
    (begin
      (set-top-level-value! 'eek 99 $ce-e1)
      (list (eval '(let () (import foo) eek) $ce-e1)
            (eval '(let () (import foo) eek) $ce-e2)
            (eval 'eek $ce-e1)
            (top-level-value 'eek $ce-e2)))
    '(99 99 99 99))
  (equal?
    (begin
      (set-top-level-value! 'eek 'ack $ce-e2)
      (list (eval '(let () (import foo) eek) $ce-e1)
            (eval '(let () (import foo) eek) $ce-e2)
            (eval 'eek $ce-e1)
            (top-level-value 'eek $ce-e2)))
    '(ack ack ack ack))
  (equal?
    (begin
      (eval '(set! whence 'later) $ce-e1)
      (list (eval 'whence $ce-e1)
            (top-level-value 'whence $ce-e2)))
    '(later now))
  (equal?
    (begin
      (set-top-level-value! 'whence 'never $ce-e2)
      (list (eval 'whence $ce-e1)
            (top-level-value 'whence $ce-e2)))
    '(later never))
  (error? ; cannot assign immutable variable
    (eval '(set! cons 4) $ce-e1))
  (error? ; cannot assign immutable variable
    (eval '(set! cons 4) $ce-e2))
  (error? ; cannot assign immutable variable
    (set-top-level-value! 'cons 4 $ce-e1))
  (error? ; cannot assign immutable variable
    (set-top-level-value! 'cons 4 $ce-e2))
  (error? ; invalid syntax
    (eval '(set! foo 4) $ce-e1))
  (error? ; invalid syntax
    (eval '(set! foo 4) $ce-e2))
  (error? ; not a variable
    (set-top-level-value! 'foo 4 $ce-e1))
  (error? ; not a variable
    (set-top-level-value! 'foo 4 $ce-e2))
  (error? ; invalid syntax
    (eval '(set! ez 4) $ce-e1))
  (error? ; invalid syntax
    (eval '(set! ez 4) $ce-e2))
  (error? ; not a variable
    (set-top-level-value! 'ez 4 $ce-e1))
  (error? ; not a variable
    (set-top-level-value! 'ez 4 $ce-e2))
  (error? ; invalid syntax
    (eval '(begin (alias ard ez) (set! ard 45)) $ce-e1))
  (equal?
    (let ()
      (define $ce-f1 (eval '(lambda () (list cons eek whence ez)) $ce-e1))
      (define $ce-f2 (eval '(lambda () (list cons eek whence ez)) $ce-e2))
      (define $ce-f3 (eval '(lambda () (list cons (let () (import foo) eek))) $ce-e1))
      (define $ce-f4 (eval '(lambda () (list cons (let () (import foo) eek))) $ce-e2))
      (eval '(define cons 3) $ce-e1)
      (define-top-level-value 'eek 4 $ce-e1)
      (eval '(define whence 5) $ce-e1)
      (define-top-level-value 'ez 6 $ce-e1)
      (define-top-level-value 'cons 'a $ce-e2)
      (eval '(define eek 'b) $ce-e2)
      (define-top-level-value 'whence 'c $ce-e2)
      (eval '(define ez 'd) $ce-e2)
      (list
        ($ce-f1)
        ($ce-f2)
        ($ce-f3)
        ($ce-f4)
        (eval '(list cons eek whence ez) $ce-e1)
        (eval '(list cons eek whence ez) $ce-e2)
        (list cons (eval '(let () (import foo) eek) $ce-e1))
        (list cons (eval '(let () (import foo) eek) $ce-e2))))
    `((,cons ack 5 tuary)
      (,cons ack c tuary)
      (,cons ack)
      (,cons ack)
      (3 4 5 6)
      (a b c d)
      (,cons ack)
      (,cons ack)))
  (equal?
    (let ()
      (eval '(define foo 'not-a-module) $ce-e1)
      (list (eval 'foo $ce-e1)
            (eval '(let () (import foo) eek) $ce-e2)))
    '(not-a-module ack))
  (equal?
    (let ([e (copy-environment (interaction-environment) #f '(cons $ce-e1))])
      (list (eval 'cons e) (eval '$ce-e1 e)))
    (list cons $ce-e1))
  (let ([e1 (copy-environment (scheme-environment) #t '())])
    (define-top-level-value 'darth 'vader e1)
    (let ([e2 (copy-environment e1 #f)])
      (let ([e3 (copy-environment e2 #t)])
        (define (f) (map (lambda (e) (top-level-value 'darth e)) (list e1 e2 e3)))
        (and (equal? (environment-symbols e1) '(darth))
             (equal? (environment-symbols e2) '(darth))
             (equal? (environment-symbols e3) '(darth))
             (equal? (f) '(vader vader vader))
             (eq? (set-top-level-value! 'darth 'maul e1) (void))
             (equal? (f) '(maul vader vader))
             (eq? (set-top-level-value! 'darth 'poodle e3) (void))
             (equal? (f) '(maul vader poodle))))))
 )

(mat environment-mutable?
  (not (environment-mutable? (scheme-environment)))
  (environment-mutable? (interaction-environment))
  (environment-mutable? (copy-environment (scheme-environment)))
 )

(mat trace-define-syntax
  (equivalent-expansion?
    (parameterize ([trace-output-port (open-output-string)]
                   [print-gensym #f])
      (let ([x (expand
                 '(let ()
                    (trace-define-syntax frob
                      (syntax-rules ()
                       [(_ rot gut) (gut rot)]))
                    (frob 17 $tds-foo)))])
        (list x (get-output-string (trace-output-port)))))
    '(($tds-foo 17) "|(frob (frob 17 $tds-foo))\n|($tds-foo 17)\n"))
)

(mat meta
  (error? ; x out of context
    (let () (meta define x 3) x))
  (error? ; x out of context
    (module () (meta define x 3) x))
  (begin
    (module ($meta-z)
      (meta define x #'"jolly")
      (define-syntax y (lambda (z) x))
      (define $meta-z y))
    (equal? $meta-z "jolly"))
  (begin
    (module (mat-meta-bar)
      (module foo (macro-helper a b)
        (meta define table
         ; pretend this is a "big computation":
          (map cons '(#\a #\b #\c) '(1 2 3)))
        (meta define lookup
          (lambda (c)
            (cond [(assq c table) => cdr] [else #f])))
        (meta define macro-helper
          (lambda (x)
            (syntax-case x ()
              [(k c)
               (with-syntax ([n (lookup (datum c))])
                 #'(list '(k c) a n))])))
        (define a 'is)
        (define-syntax b
          (lambda (x) (macro-helper x))))
      (define mat-meta-bar
        (lambda ()
          (import foo)
          (define-syntax d
            (lambda (x) (macro-helper x)))
          (list a (b #\b) (d #\c)))))
      (equal? (mat-meta-bar) '(is ((b #\b) is 2) ((d #\c) is 3))))
  (error? ; lookup out-of-context (in definition of c)
    (begin
      (module (mat-meta-bar)
        (module foo (macro-helper a b c)
          (meta define table
           ; pretend this is a "big computation":
            (map cons '(#\a #\b #\c) '(1 2 3)))
          (meta define lookup
            (lambda (c)
              (cond [(assq c table) => cdr] [else #f])))
          (meta define macro-helper
            (lambda (x)
              (syntax-case x ()
                [(k c)
                 (with-syntax ([n (lookup (datum c))])
                   #'(list '(k c) a n))])))
          (define a 'is)
          (define-syntax b
            (lambda (x) (macro-helper x)))
          (define c
            (lambda (s)
              (map lookup (string->list s)))))
        (define mat-meta-bar
          (lambda ()
            (import foo)
            (define-syntax d
              (lambda (x) (macro-helper x)))
            (list a (b #\b) (c "aq") (d #\c)))))
        (equal? (mat-meta-bar) '(is ((b #\b) is 2) (1 #f) ((d #\c) is 3)))))
  (begin
    (module mat-meta-foo (macro-helper a b)
      (meta define table
       ; pretend this is a "big computation":
        (map cons '(#\a #\b #\c) '(1 2 3)))
      (meta define lookup
        (lambda (c)
          (cond [(assq c table) => cdr] [else #f])))
      (meta define macro-helper
        (lambda (x)
          (syntax-case x ()
            [(k c)
             (with-syntax ([n (lookup (datum c))])
               #'(list '(k c) a n))])))
      (define a 'is)
      (define-syntax b
        (lambda (x) (macro-helper x))))
    #t)
  (equal?
    (let ()
      (define mat-meta-bar1
        (lambda ()
          (import mat-meta-foo)
          (define-syntax d
            (lambda (x) (macro-helper x)))
          (list a (b #\b) (d #\c))))
      (mat-meta-bar1))
    '(is ((b #\b) is 2) ((d #\c) is 3)))
  (begin
    (define mat-meta-bar2
      (lambda ()
        (import mat-meta-foo)
        (define-syntax d
          (lambda (x) (macro-helper x)))
        (list a (b #\b) (d #\c))))
    (procedure? mat-meta-bar2))
  (equal? (mat-meta-bar2) '(is ((b #\b) is 2) ((d #\c) is 3)))
  (error? ; out-of-context (run-time reference to meta variable)
    (let ()
      (module foo (macro-helper a b c)
        (meta define table
         ; pretend this is a "big computation":
          (map cons '(#\a #\b #\c) '(1 2 3)))
        (meta define lookup
          (lambda (c)
            (cond [(assq c table) => cdr] [else #f])))
        (meta define macro-helper
          (lambda (x)
            (syntax-case x ()
              [(k c)
               (with-syntax ([n (lookup (datum c))])
                 #'(list '(k c) a n))])))
        (define a 'is)
        (define-syntax b
          (lambda (x) (macro-helper x)))
        (define c
          (lambda (s)
            (map lookup (string->list s)))))
      (define bar
        (lambda ()
          (import foo)
          (define-syntax d
            (lambda (x) (macro-helper x)))
          (list a (b #\b) (c "aq") (d #\c))))
      (bar)))
  (begin
    (module (mat-meta-q mat-meta-a)
      (meta define mat-meta-q 13)
      (define-syntax mat-meta-a
        (lambda (x)
          (set! mat-meta-q (* mat-meta-q 2))
          (with-syntax ((n mat-meta-q))
            #'(list n (- mat-meta-q 6))))))
    (meta define mat-meta-x (begin (set! mat-meta-q (+ mat-meta-q 4)) mat-meta-q))
    (meta module () (set! mat-meta-q (+ mat-meta-q 10)))
    (define-syntax ans
      (lambda (x)
        (with-syntax ([d (cons (quotient (+ mat-meta-q mat-meta-x) 2) mat-meta-a)])
          #''d)))
    (equal? ans '(35 54 48)))
  (equal?
    (let ()
      (module (mat-meta-q mat-meta-a)
        (meta define mat-meta-q 13)
        (define-syntax mat-meta-a
          (lambda (x)
            (set! mat-meta-q (* mat-meta-q 2))
            (with-syntax ((n mat-meta-q))
              #'(list n (- mat-meta-q 6))))))
      (meta define mat-meta-x (begin (set! mat-meta-q (+ mat-meta-q 4)) mat-meta-q))
      (meta module () (set! mat-meta-q (+ mat-meta-q 10)))
      (define-syntax ans
        (lambda (x)
          (with-syntax ([d (cons (quotient (+ mat-meta-q mat-meta-x) 2) mat-meta-a)])
            #''d)))
      ans)
    '(35 54 48))
  (begin
    (module (mat-meta-zeta)
      (meta module frobrat (boz) (define boz 3))
      (define-syntax rot (lambda (x) (import frobrat) boz))
      (define mat-meta-zeta rot))
    (eq? mat-meta-zeta 3))
  (begin
    (module (mat-meta-gorp)
      (meta define f (lambda (x) (if (= x 0) '() (cons x (f (- x 1))))))
      (define-syntax mat-meta-gorp
        (lambda (x)
          (syntax-case x ()
            [(_ n)
             (with-syntax ([(num ...) (f (datum n))])
               #'(list num ...))]))))
    (equal? (mat-meta-gorp 5) '(5 4 3 2 1)))
  (error? ; f not bound (referenced in alpha before definition complete)
    (module (mat-meta-gorp)
      (meta define f
        (lambda (x)
          (define-syntax alpha
            (lambda (x)
              (f x) ; f not bound (yet)
              #'()))
          (if (= x 0)
              alpha
              (cons x (f (- x 1))))))
      (define-syntax mat-meta-gorp
        (lambda (x)
          (syntax-case x ()
            [(_ n)
             (with-syntax ([(num ...) (f (datum n))])
               #'(list num ...))])))))
  (begin
    (define-syntax $cftest
      (syntax-rules ()
        [(_ e0 e1 e2)
         (begin
           (collect (collect-maximum-generation)) ; close ports left open by load/compile-file w/mat's error handler
           (let ((op (open-output-file "testfile.ss" 'replace)))
             (pretty-print 'e0 op)
             (close-output-port op))
           (compile-file "testfile.ss")
           (and e1 (begin (load "testfile.ss") e2)))]))
    #t)
  ($cftest
    (begin
      (meta define meta-$bun 3)
      (define meta-$burger 4))
    (equal? meta-$bun 3)
    (equal? meta-$burger 4))
  (error?
    ($cftest
      (meta define meta-$lettuce 3)
      (equal? meta-$bun 3)
      (equal? meta-$burger 4)))
 ; check to make sure meta still works if we change interaction environment
  (eqv?
    (parameterize ([interaction-environment (copy-environment (interaction-environment))])
      (eval '
        (let ()
          (meta define foo 3)
          (meta define bar (* 3 7))
          (define-syntax a (lambda (x) (+ foo bar)))
          a)))
    24)
)

(mat meta2
  (error? ; x out-of-context
    (begin
      (meta define x 3)
      x))
  (begin
    (meta define x 3)
    (define-syntax y (lambda (z) x))
    (eq? y 3))

 ; top-level module tests
  (error? ; x out-of-context
    (module m (x) (meta define x 3) (pretty-print x)))
  (error? ; x out-of-context
    (begin
      (module m (x) (meta define x 3))
      (let () (import m) x)))
  (begin
    (module m (x) (meta define x 3))
    (eq? (let () (import m) (define-syntax y (lambda (z) x)) y) 3))
  (error? ; x out-of-context
    (begin
      (module m (x) (meta define x 3))
      (import m)
      x))
  (begin
    (module mm-m (mm-x) (meta define mm-x 3))
    (import mm-m)
    (define-syntax mm-y (lambda (z) mm-x))
    (eq? mm-y 3))
  (begin
    (module ($meta-z)
      (meta define x #'"jolly")
      (define-syntax y (lambda (z) x))
      (define $meta-z y))
    (equal? $meta-z "jolly"))

 ; local tests
  (error? ;=> out-of-context or unbound error
    (let ()
      (module m (x) (meta define x 3) (pretty-print x))
      4))
  (error? ;=> out-of-context or unbound error
    (let ()
      (module m (x) (meta define x 3))
      (let () (import m) x)))
  (eq?
    (let ()
      (module m (x) (meta define x 3))
      (let () (import m) (define-syntax y (lambda (z) x)) y))
    3)
  (let ()
    (module ($meta-z)
      (meta define x #'"jolly")
      (define-syntax y (lambda (z) x))
      (define $meta-z y))
    (equal? $meta-z "jolly"))
  (error? ;=> q out-of-context
    (let ()
      (meta define p 3)
      (define-syntax a
        (lambda (x)
          (meta define q 4)
          `(,#'quote (,p ,q))))
      a))
  (equal?
    (let ()
      (meta define p 3)
      (define-syntax a
        (lambda (x)
          (meta define q 4)
          (define-syntax b (lambda (x) q))
          `(,#'quote (,p ,b))))
      a)
    '(3 4))

  (begin
    (define $mm-p "p")
    (define $mm-q "q")
    (define $mm-r "r")
    (meta module
      ($mm-a $mm-b $mm-c)
      (define t '())
      (define $mm-a (lambda (k v) (set! t (cons (cons k v) t)) #'(void)))
      (define $mm-b (lambda (k) (cdr (assq k t))))
      (define-syntax $mm-c
        (lambda (x)
          (syntax-case x (get put)
            [(_ get n) ($mm-b (datum n))]
            [(_ put n v) ($mm-a (datum n) #'v)])))
      (set! t `((1 . ,#'$mm-q) (2 . ,#'$mm-r))))
    ($mm-c put 7 $mm-p)
    (equal?
      (list ($mm-c get 1) ($mm-c get 2) ($mm-c get 7))
      '("q" "r" "p")))
  (equal?
    (let ([p "p!"] [q "q!"] [r "r!"])
      (meta module (a b c)
        (define t '())
        (define a (lambda (k v) (set! t (cons (cons k v) t)) #'(void)))
        (define b (lambda (k) (cdr (assq k t))))
        (define-syntax c
          (lambda (x)
            (syntax-case x (get put)
              [(_ get n) (b (datum n))]
              [(_ put n v) (a (datum n) #'v)])))
        (set! t `((1 . ,#'q) (2 . ,#'r))))
      (c put 7 p)
      (list (c get 1) (c get 2) (c get 7)))
    '("q!" "r!" "p!"))

 ; assuming internal-defines-as-letrec* defaults to #t
  (internal-defines-as-letrec*)
 ; following tests assume it's set to #f
  (begin (internal-defines-as-letrec* #f) (not (internal-defines-as-letrec*)))
 ; top-level module tests
  (error? ; undefined variable merry
    (module sam (frodo)
      (define merry 'merry)
      (define frodo (cons merry merry))))
  (error? ; undefined variable frodo
    (module sam (frodo)
      (define merry 'merry)
      (define frodo 'frodo)
      (define pippin (cons frodo frodo))))
  (begin (internal-defines-as-letrec* #t) (internal-defines-as-letrec*))
  (eq? (let ()
         (module (x !y ?y) (define x (call/cc values))
           (define y 0)
           (define !y (lambda (v) (set! y v)))
           (define ?y (lambda () y)))
         (!y (+ (?y) 1))
         (x values)
         (?y))
       1)
  (begin
    (module (x !y ?y)
      (define x (call/cc values))
      (define y 0)
      (define !y (lambda (v) (set! y v)))
      (define ?y (lambda () y)))
    (!y (+ (?y) 1))
    (x values)
    (eq? (?y) 1))
  (begin
    (meta define hobbits '())
    (module ()
      (meta module ()
        (set! hobbits (cons 'merry hobbits)))
      (meta module ()
        (set! hobbits (cons 'lobelia hobbits))
        (set! hobbits (cons 'frodo hobbits))
        (set! hobbits (cons 'bilbo hobbits)))
      (meta begin
        (set! hobbits (cons 'pippin hobbits))))
    (define-syntax hobbit-report
      (lambda (x) `(,#'quote ,(datum->syntax #'* hobbits))))
    (equal? hobbit-report '(pippin bilbo frodo lobelia merry)))
  (let ()
    (meta define hobbits '())
    (module ()
      (meta module ()
        (set! hobbits (cons 'merry hobbits)))
      (meta module ()
        (set! hobbits (cons 'lobelia hobbits))
        (set! hobbits (cons 'frodo hobbits))
        (set! hobbits (cons 'bilbo hobbits)))
      (meta begin
        (set! hobbits (cons 'pippin hobbits))))
    (define-syntax hobbit-report
      (lambda (x) `(,#'quote ,(datum->syntax #'* hobbits))))
    (equal? hobbit-report '(pippin bilbo frodo lobelia merry)))
  (begin
    (meta define $whatsit)
    (meta begin (set! $whatsit #xc7c7c7c7))
    (define-syntax $mm-a (lambda (x) $whatsit))
    (eqv? $mm-a #xc7c7c7c7))
  (error? ; no expr in body
    (let () (meta begin (void))))
  (error? ; invalid meta definition ((void))
    (meta (void)))
  (error? ; invalid meta definition ((void))
    (module () (meta (void))))
  (error? ; invalid meta definition ((void))
    (let () (meta (void))))
  (begin
    (define hobbits '())
    (module ()
      (module ()
        (set! hobbits (cons 'merry hobbits)))
      (module ()
        (set! hobbits (cons 'lobelia hobbits))
        (set! hobbits (cons 'frodo hobbits))
        (set! hobbits (cons 'bilbo hobbits)))
      (set! hobbits (cons 'pippin hobbits)))
    (equal? hobbits '(pippin bilbo frodo lobelia merry)))
  (let ()
    (define hobbits '())
    (module ()
      (module ()
        (set! hobbits (cons 'merry hobbits)))
      (module ()
        (set! hobbits (cons 'lobelia hobbits))
        (set! hobbits (cons 'frodo hobbits))
        (set! hobbits (cons 'bilbo hobbits)))
      (set! hobbits (cons 'pippin hobbits)))
    (equal? hobbits '(pippin bilbo frodo lobelia merry)))

 ; assuming internal-defines-as-letrec* true
  (internal-defines-as-letrec*)
  (begin
    (define hobbits '())
    (module sam (frodo)
      (define merry (set! hobbits (cons 'merry hobbits)))
      (define frodo (set! hobbits (cons 'frodo hobbits)))
      (define pippin (set! hobbits (cons 'pippin hobbits))))
    (equal? hobbits '(pippin frodo merry)))
  (let ()
    (define hobbits '())
    (module sam (frodo)
      (define merry (set! hobbits (cons 'merry hobbits)))
      (define frodo (set! hobbits (cons 'frodo hobbits)))
      (define pippin (set! hobbits (cons 'pippin hobbits))))
    (equal? hobbits '(pippin frodo merry)))
  (begin
    (define hobbits '())
    (module sam (frodo)
      (define merry (set! hobbits (cons 'merry hobbits)))
      (module (frodo)
        (define lobelia (set! hobbits (cons 'lobelia hobbits)))
        (define frodo (set! hobbits (cons 'frodo hobbits)))
        (define bilbo (set! hobbits (cons 'bilbo hobbits))))
      (define pippin (set! hobbits (cons 'pippin hobbits))))
    (equal? hobbits '(pippin bilbo frodo lobelia merry)))
  (let ()
    (define hobbits '())
    (module sam (frodo)
      (define merry (set! hobbits (cons 'merry hobbits)))
      (module (frodo)
        (define lobelia (set! hobbits (cons 'lobelia hobbits)))
        (define frodo (set! hobbits (cons 'frodo hobbits)))
        (define bilbo (set! hobbits (cons 'bilbo hobbits))))
      (define pippin (set! hobbits (cons 'pippin hobbits))))
    (equal? hobbits '(pippin bilbo frodo lobelia merry)))
  (begin
    (module sam (frodo)
      (define merry 'merry)
      (define frodo (cons merry merry)))
    (equal? (let () (import sam) frodo) '(merry . merry)))
  (error? ; undefined variable merry
    (module sam (frodo)
      (define frodo (cons merry merry))
      (define merry 'merry)))
  (error? ; undefined variable frodo
    (module sam (frodo)
      (define merry 'merry)
      (define pippin (cons frodo frodo))
      (define frodo 'frodo)))
  (begin
    (module sam (frodo)
      (define merry 'merry)
      (define frodo (lambda () pippin))
      (define pippin (cons frodo frodo)))
    (let () (import sam) (eq? (car (frodo)) frodo)))
  (let ()
    (module (x !y ?y)
      (define x (call/cc values))
      (define y 0)
      (define !y (lambda (v) (set! y v)))
      (define ?y (lambda () y)))
    (!y (+ (?y) 1))
    (x values)
    (eq? (?y) 1))
  (begin
    (module (x !y ?y)
      (define x (call/cc values))
      (define y 0)
      (define !y (lambda (v) (set! y v)))
      (define ?y (lambda () y)))
    (!y (+ (?y) 1))
    (x values)
    (eq? (?y) 1))

 ; test for proper evaluation of meta defines and inits at compile-file time,
 ; visit time, revisit time, and load time
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(meta module $mm-m (a)
             (define q 3)
             (define-syntax qinc! (identifier-syntax (set! q (+ q 1))))
             (define-syntax (a x) qinc! q)
             qinc!
             (set! q (* q q)))))
      'replace)
    (compile-file "testfile")
    #t)
  (eq? (let () (import $mm-m) a) 17)
  (eq? (let () (import $mm-m) a) 18)
  (begin (visit "testfile.so") #t)
  (eq? (let () (import $mm-m) a) 17)
  (eq? (let () (import $mm-m) a) 18)
  (begin (load "testfile.so") #t)
  (eq? (let () (import $mm-m) a) 17)
  (eq? (let () (import $mm-m) a) 18)
  (begin (revisit "testfile.so") #t)
  (eq? (let () (import $mm-m) a) 19)
)

(mat quasisyntax
  (error? ; invalid syntax
    quasisyntax)
  (error? ; invalid syntax
    (quasisyntax))
  (error? ; invalid syntax
    (quasisyntax . a))
  (error? ; invalid syntax
    (quasisyntax a b c))
  (error? ; misplaced
    (unsyntax x))
  (error? ; misplaced
    (unsyntax-splicing x))
  (error? ; misplaced
    (unsyntax x y))
  (error? ; misplaced
    (unsyntax-splicing x y))
  (error? ; misplaced
    (unsyntax))
  (error? ; misplaced
    (unsyntax-splicing))
  (error? ; misplaced
    unsyntax)
  (error? ; misplaced
    unsyntax-splicing)
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`(list #,(length #'(x ...)) 'x ...)])))
         #t)
  (equal? (qs-foo 3 2 1) '(3 3 2 1))
  (equal? (qs-foo 3 2 1) '(3 3 2 1))
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                (quasisyntax (list (unsyntax (length #'(x ...))) 'x ...))])))
         #t)
  (equal? (qs-foo 3 2 1) '(3 3 2 1))
  (equal? (qs-foo 3 2 1) '(3 3 2 1))
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'#`(a #,a b #,@b #,#(#,@#'(x ...)) #,@#(#,#'(x ...)))])))
         #t)
  (equal?
    (qs-foo 3 2 1)
    '(quasisyntax
       (a (unsyntax a) b (unsyntax-splicing b)
          (unsyntax #3(3 2 1)) (unsyntax-splicing #1((3 2 1))))))
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'(a #(#,#'(x ...) #,@#'(x ...) unsyntax unsyntax-splicing ,a ,@b) (a . #,#'(x ...)) . c)])))
         #t)
  (equal?
    (qs-foo 3 2 1)
    '(a #8((3 2 1) 3 2 1 unsyntax unsyntax-splicing ,a ,@b)
        (a 3 2 1)
        .
        c))
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'#(a (#,#'(x ...) #,@#'(x ...) unsyntax unsyntax-splicing ,a ,@b) (a . #,#'(x ...)))])))
         #t)
  (equal?
    (qs-foo 3 2 1)
    '#3(a ((3 2 1) 3 2 1 unsyntax unsyntax-splicing ,a ,@b)
        (a 3 2 1)))
 ; test zero and two+ unsyntax-splicing subforms
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'((unsyntax) 0 (unsyntax #'(a x ... b) #'(x ...)) c)])))
         #t)
  (equal? (qs-foo 3 2 1) '(0 (a 3 2 1 b) (3 2 1) c))
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'#((unsyntax) 0 (unsyntax #'(a x ... b) #'(x ...)) c)])))
         #t)
  (equal? (qs-foo 3 2 1) '#(0 (a 3 2 1 b) (3 2 1) c))
 ; test zero and two+ unsyntax-splicing subforms
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'((unsyntax-splicing) 0 (unsyntax-splicing #'(a x ... b) #'(x ...)) c)])))
         #t)
  (equal? (qs-foo 3 2 1) '(0 a 3 2 1 b 3 2 1 c))
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'#((unsyntax-splicing) 0 (unsyntax-splicing #'(a x ... b) #'(x ...)) c)])))
         #t)
  (equal? (qs-foo 3 2 1) '#(0 a 3 2 1 b 3 2 1 c))
 ; make sure out-of-place unsyntax/unsyntax-splicing keywords are left alone
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...) #`'unsyntax])))
         #t)
  (equal? (qs-foo 3 2 1) 'unsyntax)
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...) #`'unsyntax-splicing])))
         #t)
  (equal? (qs-foo 3 2 1) 'unsyntax-splicing)
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'(a . (unsyntax #'(x ...) #'(x ...)))])))
         #t)
  (equal? (qs-foo 3 2 1) '(a . (unsyntax (syntax (3 2 1)) (syntax (3 2 1)))))
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x ...)
                #`'(a . (unsyntax-splicing #'(x ...)))])))
         #t)
  (equal? (qs-foo 3 2 1) '(a . (unsyntax-splicing (syntax (3 2 1)))))
 ; test noninterference with quasiquote
  (begin (define-syntax qs-foo
           (lambda (x)
             (syntax-case x ()
               [(_ x1 x2 ...)
                #``(a ,@(reverse (list #,@#'(x2 ...))) ,#,#'x1)])))
         #t)
  (equal?
    (qs-foo 3 2 1)
    '(a 1 2 3))
 ; tests adapted from Andre van Tonder posts to srfi 93 discussion
  (equal?
    (let ()
      (define-syntax swap!
        (lambda (e)
          (syntax-case e ()
            [(_ a b)
             (let ([a #'a] [b #'b])
               (quasisyntax
                 (let ([temp (unsyntax a)])
                   (set! (unsyntax a) (unsyntax b))
                   (set! (unsyntax b) temp))))])))
      (let ([temp 1] [set! 2])
        (swap! set! temp)
        (cons temp set!)))
    '(2 . 1))
  (eq?
    (let ()
      (define-syntax case
        (lambda (x)
          (syntax-case x ()
            [(_ e c1 c2 ...)
             (quasisyntax
               (let ([t e])
                 (unsyntax
                   (let f ([c1 #'c1] [cmore #'(c2 ...)])
                     (if (null? cmore)
                         (syntax-case c1 (else)
                           [(else e1 e2 ...) #'(begin e1 e2 ...)]
                           [((k ...) e1 e2 ...)
                            #'(if (memv t '(k ...))
                                  (begin e1 e2 ...))])
                         (syntax-case c1 ()
                           [((k ...) e1 e2 ...)
                            (quasisyntax
                              (if (memv t '(k ...))
                                  (begin e1 e2 ...)
                                  (unsyntax
                                    (f (car cmore)
                                       (cdr cmore)))))]))))))])))
      (case 'a [(b c) 'no] [(d a) 'yes]))
    'yes)
  (eqv?
    (let ()
      (define-syntax let-in-order
        (lambda (form)
          (syntax-case form ()
            [(_ ((i e) ...) e0 e1 ...)
             (let f ([ies #'((i e) ...)] [its #'()])
               (syntax-case ies ()
                 [() (quasisyntax (let (unsyntax its) e0 e1 ...))]
                 [((i e) . ies)
                  (with-syntax ([t (car (generate-temporaries '(t)))])
                    (quasisyntax
                      (let ([t e])
                        (unsyntax
                          (f #'ies
                             (quasisyntax
                               ((i t)
                                 (unsyntax-splicing its))))))))]))])))
      (let-in-order ((x 1) (y 2)) (+ x y)))
    3)
  (equal?
    (let-syntax ([test-ellipses-over-unsyntax
                  (lambda (e)
                    (let ([a #'a])
                      (with-syntax ([(b ...) #'(1 2 3)])
                        (quasisyntax '((b #,a) ...)))))])
      (test-ellipses-over-unsyntax))
    '((1 a) (2 a) (3 a)))
  (equal?
    (let-syntax ([test (lambda (_)
                         (quasisyntax '(list #,(+ 1 2) 4)))])
      (test))
  '(list 3 4))
  (equal?
    (let-syntax ([test (lambda (_)
                         (let ([name #'a])
                           (quasisyntax '(list #,name '#,name))))])
      (test))
    '(list a 'a))
  (equal?
    (let-syntax ([test (lambda (_)
                         (quasisyntax
                           '(a #,(+ 1 2) #,@(map abs '(4 -5 6)) b)))])
      (test))
    '(a 3 4 5 6 b))
  (equal?
    (let-syntax ([test (lambda (_)
                         (quasisyntax
                           '((foo #,(- 10 3))
                              #,@(cdr '(5))
                              .
                              #,(car '(7)))))])
      (test))
    '((foo 7) . 7))
  (equal?
    (let-syntax ([test (lambda (_)
                         (quasisyntax
                           '#(10 5 #,(sqrt 4) #,@(map sqrt '(16 9)) 8)))])
      (test))
    '#(10 5 2 4 3 8))
  (eqv?
    (let-syntax ([test (lambda (_) (quasisyntax #,(+ 2 3)))])
      (test))
    5)
  (equal?
    (let-syntax ([test (lambda (_)
                         (quasisyntax
                           '(a (quasisyntax
                                 (b #,(+ 1 2) #,(foo #,(+ 1 3) d) e))
                               f)))])
      (test))
    '(a (quasisyntax (b #,(+ 1 2) #,(foo 4 d) e)) f))

  (equal?
    (let-syntax ([test (lambda (_)
                         (let ([name1 #'x] [name2 #'y])
                           (quasisyntax
                             '(a (quasisyntax (b #,#,name1 #,#'#,name2 d))
                                 e))))])
      (test))
    '(a (quasisyntax (b #,x #,#'y d)) e))
 ; Bawden's extensions:
  (equal?
    (let-syntax ([test (lambda (_)
                         (quasisyntax '(a (unsyntax 1 2) b)))])
      (test))
    '(a 1 2 b))
  (equal?
    (let-syntax ([test (lambda (_)
                         (quasisyntax
                           '(a (unsyntax-splicing '(1 2) '(3 4)) b)))])
      (test))
    '(a 1 2 3 4 b))
  (equal?
    (let-syntax ([test (lambda (_)
                         (let ([x #'(a b c)])
                           (quasisyntax
                             '(quasisyntax (#,#,x #,@#,x #,#,@x #,@#,@x)))))])
      (test))
    '(quasisyntax
       (#,(a b c)
         #,@(a b c)
         (unsyntax a b c)
         (unsyntax-splicing a b c))))
)

(mat meta-cond
  (begin
    (define $meta-cond-expr
      '(meta-cond
         [(= (optimize-level) 3) $mc-a $mc-b $mc-c]
         [(= (optimize-level) 2) $mc-d]
         [else $mc-e $mc-f]))
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 3]) (expand $meta-cond-expr))
    '(begin $mc-a $mc-b $mc-c))
  (equivalent-expansion?
    (parameterize ([optimize-level 2]) (expand $meta-cond-expr))
    '$mc-d)
  (equivalent-expansion?
    (parameterize ([optimize-level 0]) (expand $meta-cond-expr))
    '(begin $mc-e $mc-f))
  (equal?
    (parameterize ([optimize-level 0]) ; should have no effect
      (with-output-to-string
        (lambda ()
          (meta-cond
            [(= (optimize-level) 3) (pretty-print 'level3)]
            [(= (optimize-level) 2) (pretty-print 'level2)]))))
    (case (optimize-level)
      [(2) "level2\n"]
      [(3) "level3\n"]
      [else ""]))
)

(mat make-compile-time-value
  (error? ; incorrect number of arguments
    (let ()
      (define-syntax a
        (lambda (x)
          (lambda (r)
            (r))))
      a))
  (error? ; not an identifier
    (let ()
      (define-syntax a
        (lambda (x)
          (lambda (r)
            (r #'(a)))))
      a))
  (error? ; not an identifier
    (let ()
      (define-syntax a
        (lambda (x)
          (lambda (r)
            (r #'(a) #'frip))))
      a))
  (error? ; not an identifier
    (let ()
      (define-syntax a
        (lambda (x)
          (lambda (r)
            (r #'a "frip"))))
      a))
  (error? ; incorrect number of arguments
    (let ()
      (define-syntax a
        (lambda (x)
          (lambda (r)
            (r #'a #'frip "extra stuff"))))
      a))
  (error? ; not a compile-time value
    (compile-time-value-value 17))
  (begin
    (with-output-to-file "testfile-mctv0.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mctv0) (export get-ctv get-property) (import (chezscheme))
             (define-syntax get-ctv
               (lambda (x)
                 (lambda (r)
                   (syntax-case x ()
                     [(_ q) #`'#,(datum->syntax #'* (r #'q))]))))
             (define-syntax get-property
               (lambda (x)
                 (lambda (r)
                   (syntax-case x ()
                     [(_ q prop) #`'#,(datum->syntax #'* (r #'q #'prop))])))))))
      'replace)
    (for-each separate-compile '(mctv0))
    #t)
  (begin
    (import (testfile-mctv0))
    #t)
  (compile-time-value? (make-compile-time-value 'fred))
  (begin
    (define-syntax frob (make-compile-time-value 'rabf))
    #t)
  (eq? (get-ctv frob) 'rabf)
  (error? ; invalid syntax
    frob)
  (error? ; invalid syntax
    (frob kupe))
  (eq?
    (let ()
      (define-syntax frob (make-compile-time-value 'shuddle))
      (get-ctv frob))
    'shuddle)
  (eq?
    (let-syntax ([frob (make-compile-time-value 'skupo)])
      (get-ctv frob))
    'skupo)
  (equal?
    (let ([frob "not the global frob ..."])
      (list frob (get-ctv frob)))
    '("not the global frob ..." #f))
  (eq? (get-ctv frob) 'rabf)
  (error? ; invalid syntax
    (let ()
      (define-syntax frob (make-compile-time-value 'shuddle))
      frob))
  (error? ; invalid syntax
    (let ()
      (define-syntax frob (make-compile-time-value 'shuddle))
      (frob)))
  (error? ; duplicate definition
    (module mctv-m1 (x)
      (define x 3)
      (define-syntax x (make-compile-time-value 'xxx))))
  (error? ; duplicate definition
    (module mctv-m1 (x)
      (define-syntax x (make-compile-time-value 'xxx))
      (define-syntax x (make-compile-time-value 'xxx))))
  (begin
    (module mctv-m1 (x)
      (define-syntax x (make-compile-time-value 'xxx)))
    #t)
  (eq? (let () (import mctv-m1) (get-ctv x)) 'xxx)
  (begin
    (library (mctv l1) (export x) (import (chezscheme) (testfile-mctv0))
      (define-syntax x (make-compile-time-value 'xow)))
    #t)
  (eq? (let () (import (mctv l1)) (get-ctv x)) 'xow)
  (eq? (compile-time-value-value (top-level-syntax 'x (environment '(mctv l1)))) 'xow)
  (begin
    (with-output-to-file "testfile-mctv1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mctv1) (export x) (import (chezscheme))
             (define-syntax x (make-compile-time-value 'xuko1)))))
      'replace)
    (for-each separate-compile '(mctv1))
    #t)
  (eq? (let () (import (testfile-mctv1)) (get-ctv x)) 'xuko1)
  (compile-time-value? (top-level-syntax 'x (environment '(testfile-mctv1))))
  (eq? (compile-time-value-value (top-level-syntax 'x (environment '(testfile-mctv1)))) 'xuko1)
  (begin
    (with-output-to-file "testfile-mctv1a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-mctv1a) (export x) (import (chezscheme))
             (define-syntax x (make-compile-time-value 'xuko1)))))
      'replace)
    (for-each separate-compile '(mctv1a))
    #t)
  (eq? (compile-time-value-value (top-level-syntax 'x (environment '(testfile-mctv1a)))) 'xuko1)
  (eq? (let () (import (testfile-mctv1a)) (get-ctv x)) 'xuko1)
  (begin
    (with-output-to-file "testfile-mctv2.ss"
      (lambda ()
        (pretty-print
          '(module mctv-m2 (x)
             (define-syntax x (make-compile-time-value 'xuko2)))))
      'replace)
    (for-each separate-compile '(mctv2))
    (load "testfile-mctv2.so")
    #t)
  (eq? (let () (import mctv-m2) (get-ctv x)) 'xuko2)
  (begin
    (with-output-to-file "testfile-mctv3.ss"
      (lambda ()
        (pretty-print
          '(define-syntax mctv3-x (make-compile-time-value 'xuko3))))
      'replace)
    (for-each separate-compile '(mctv3))
    (load "testfile-mctv3.so")
    #t)
  (eq? (get-ctv mctv3-x) 'xuko3)
  (begin
    (with-output-to-file "testfile-mctv4.ss"
      (lambda ()
        (printf "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (chezscheme) (testfile-mctv0)))
        (pretty-print '(define spod))
        (pretty-print '(define qrtz))
        (pretty-print '(define xptz))
        (pretty-print '(define-syntax x (make-compile-time-value 'xuko4)))
        (pretty-print '(define-property x spod "shuff"))
        (pretty-print '(define-property x qrtz "dmnd"))
        (pretty-print '(printf "~s ~s ~s ~s ~s\n"
                         (get-property get-property spod)
                         (get-property x spod)
                         (get-property x qrtz)
                         (get-property x xptz)
                         (get-ctv x))))
      'replace)
    (for-each (lambda (x) (separate-compile 'compile-program x)) '(mctv4))
    #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (load-program "testfile-mctv4.ss")))
    "#f \"shuff\" \"dmnd\" #f xuko4\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (load-program "testfile-mctv4.so")))
    "#f \"shuff\" \"dmnd\" #f xuko4\n")
  (eqv?
    (let ()
      (define foo 3)
      (define-syntax alpha (make-compile-time-value #'foo))
      (define-syntax beta
        (lambda (x)
          (lambda (r)
            (r #'alpha))))
      (let ()
        (define foo 4)
        beta))
    3)
  (eqv?
    (let ()
      (define foo 3)
      (define-syntax alpha
        (lambda (x)
          (syntax-case x ()
            [(_ id) #'(define-syntax id (make-compile-time-value #'foo))])))
      (let ()
        (define foo 4)
        (alpha beta)
        (define-syntax gamma
          (lambda (x)
            (lambda (r)
              (r #'beta))))
        gamma)) ;=> 3
    3)
  #; ; decided not to have rebuild-macro-output delve into records...
  (eqv?
    (let ()
      (meta define-record-type rats (fields cheese))
      (define foo 3)
      (define-syntax alpha
        (lambda (x)
          (syntax-case x ()
            [(_ id)
             #`(define-syntax id
                 (make-compile-time-value '#,(make-rats #'foo)))])))
      (let ()
        (define foo 4)
        (alpha beta)
        (define-syntax gamma
          (lambda (x)
            (lambda (r)
              #`(let ()
                  (define foo 5)
                  #,(rats-cheese (r #'beta))))))
        gamma))
    3)
  #; ; decided not to have rebuild-macro-output delve into records...
  (eqv?
    (let ()
      (meta define-record-type rats (fields cheese))
      (define foo 3)
      (define-syntax alpha
        (lambda (x)
          (syntax-case x ()
            [(_ id)
             #`(module (id)
                 (define foo 3.5)
                 (define-syntax id
                   (make-compile-time-value '#,(make-rats #'foo))))])))
      (let ()
        (define foo 4)
        (alpha beta)
        (define-syntax gamma
          (lambda (x)
            (lambda (r)
              #`(let ()
                  (define foo 5)
                  #,(rats-cheese (r #'beta))))))
        gamma))
    3.5)
  (eqv?
    (let ()
      (meta define make-rats list)
      (meta define rats-cheese car)
      (define foo 3)
      (define-syntax alpha
        (lambda (x)
          (syntax-case x ()
            [(_ id)
             #`(module (id)
                 (define foo 3.5)
                 (define-syntax id
                   (make-compile-time-value #'#,(make-rats #'foo))))])))
      (let ()
        (define foo 4)
        (alpha beta)
        (define-syntax gamma
          (lambda (x)
            (lambda (r)
              #`(let ()
                  (define foo 5)
                  #,(syntax-case (r #'beta) ()
                      [(foo) #'foo])))))
        gamma))
    3.5)
)

(mat define-property
  (begin
    (library (dp get-property) (export get-property) (import (scheme))
      (define-syntax get-property
        (lambda (x)
          (lambda (r)
            (syntax-case x ()
              [(_ q prop) #`'#,(datum->syntax #'* (r #'q #'prop))])))))
    (import (dp get-property))
    #t)
  (begin
    (define-property cons frotz 'spamgle)
    (equal?
      (cons (get-property cons frotz) (get-property cons fratz))
      '(spamgle . #f)))
  (equal?
    (cons (get-property cons frotz) (get-property cons fratz))
    '(spamgle . #f))
  (equal?
    (let ()
      (import scheme)
      (cons (get-property cons frotz) (get-property cons fratz)))
    (if (free-identifier=? #'cons (let () (import scheme) #'cons))
        '(spamgle . #f)
        '(#f . #f)))
  (equal?
    (let ()
      (define-property cons fratz 'yubah)
      (cons (get-property cons frotz) (get-property cons fratz)))
    '(spamgle . yubah))
  (equal?
    (cons (get-property cons frotz) (get-property cons fratz))
    '(spamgle . #f))
 ; restore
  (begin
    (meta-cond
      [(free-identifier=? #'cons (let () (import scheme) #'cons))
       (import (only scheme cons))]
      [else (define cons (let () (import scheme) cons))])
    #t)
  (equal?
    (cons (get-property cons frotz) (get-property cons fratz))
    '(#f . #f))
  (equal?
    (let ()
      (import scheme)
      (cons (get-property cons frotz) (get-property cons fratz)))
    '(#f . #f))
  (equal?
    (let ()
      (import scheme)
      (define-property list type "procedure")
      (list (get-property list type) (get-property car type)))
    '("procedure" #f))
  (equal?
    (let ()
      (define list (lambda x x))
      (define-property list type "procedure")
      (list (get-property list type) (get-property car type)))
    '("procedure" #f))
  (error? ; multiple definitions for list
    (let ()
      (define-property list type "procedure")
      (define list (lambda x x))
      (list (get-property list type) (get-property car type))))
  (error? ; multiple definitions for list
    (module m (list)
      (define-property list type "procedure")
      (define list (lambda x x))
      (list (get-property list type) (get-property car type))))
  (error? ; immutable environment
    (eval '(define-property frot rat 3) (scheme-environment)))
  (error? ; immutable environment
    (eval '(define-property cons rat 3) (scheme-environment)))
  (error? ; no visible binding
    (eval '(let () (define-property frot cons 3) 3) (scheme-environment)))
  (error? ; no visible binding
    (eval '(let () (define-property cons rat 3) 3) (scheme-environment)))
  (error? ; no visible binding
    (library (dp err1) (export x) (import (scheme))
      (define-property x cons "frap")))
  (error? ; no visible binding
    (library (dp err1) (export x) (import (scheme))
      (define-property cons frip "frap")))
  (error? ; no visible binding
    (module (x) (import-only (scheme))
      (define-property x cons "frap")))
  (error? ; no visible binding
    (module (x) (import-only (scheme))
      (define-property cons frip "frap")))
  (not (get-property list type))
  (equal?
    (let ()
      (define type)
      (define-property list type "proc")
      (list
        (get-property list type)
        (let () (define type) (get-property list type))))
    '("proc" #f))
  (equal?
    (let ()
      (module (type iface list)
        (define type)
        (define iface)
        (define-property list type "a proc")
        (define-property list iface -1))
      (list
        (get-property list type)
        (get-property list iface)))
    '("a proc" -1))
  (equal?
    (let ()
      (module (type list)
        (define type)
        (define iface)
        (define-property list type "a proc")
        (define-property list iface -1))
      (list
        (get-property list type)
        (get-property list iface)))
    '("a proc" #f))
  (equal?
    (let ()
      (module (iface list)
        (define type)
        (define iface)
        (define-property list type "a proc")
        (define-property list iface -1))
      (list
        (get-property list type)
        (get-property list iface)))
    '(#f -1))
  (equal?
    (let ()
      (module (list)
        (define type)
        (define iface)
        (define-property list type "a proc")
        (define-property list iface -1))
      (list
        (get-property list type)
        (get-property list iface)))
    '(#f #f))
  (equal?
    (let ()
      (module (type iface)
        (define type)
        (define iface)
        (define-property list type "a proc")
        (define-property list iface -1))
      (list
        (get-property list type)
        (get-property list iface)))
    '(#f #f))
  (begin
    (define dp-out (open-output-string))
    (module dp-m1 (x)
      (import (scheme) (dp get-property))
      (define x 444)
      (define-property x frob "x-frob")
      (define-property x spam "x-spam")
      (fprintf dp-out "~s ~s ~s ~s\n"
        (get-property x spam)
        (get-property x frob)
        (get-property x rats)
        x))
    (equal?
      (get-output-string dp-out)
      "\"x-spam\" \"x-frob\" #f 444\n"))
  (equal?
    (let ()
      (import dp-m1)
      (list
        (get-property x spam)
        (get-property x frob)
        (get-property x rats)
        x))
    '("x-spam" "x-frob" #f 444))
  (begin
    (define dp-out (open-output-string))
    (module dp-m1 ()
      (import (scheme) (dp get-property))
      (define-property dp-out spam "dp-out-spam")
      (define-property dp-out frob "dp-out-frob")
      (fprintf dp-out "~s ~s ~s\n"
        (get-property dp-out spam)
        (get-property dp-out frob)
        (get-property dp-out rats)))
    (and
      (equal?
        (get-output-string dp-out)
        "\"dp-out-spam\" \"dp-out-frob\" #f\n")
      (not (get-property dp-out spam))
      (not (get-property dp-out frob))))
  (equal?
    (let ()
      (import dp-m1)
      (list
        (get-property x spam)
        (get-property x frob)
        (get-property x rats)))
    '(#f #f #f))
  (begin
    (module dp-m1 (m2 (f x y))
      (import (scheme) (dp get-property))
      (define y "yval")
      (define-property y a "y-a")
      (module m2 (x)
        (define x "xval")
        (define-property x a "x-a")
        (define-property y b "y-b"))
      (import m2)
      (define-property x b "x-b")
      (define-syntax f
        (identifier-syntax
          (list (list x (get-property x a) (get-property x b))
                (list y (get-property y a) (get-property y b))))))
    #t)
  (equal?
    (let () (import dp-m1) f)
    '(("xval" "x-a" "x-b") ("yval" "y-a" #f)))
  (equal?
    (let ()
      (import dp-m1)
      (import m2)
      (list
        (get-property x a)
        (get-property x b)
        (get-property x c)
        x))
    '("x-a" #f #f "xval"))
  (begin
    (library (dp l1) (export x spam frob rats) (import (scheme) (dp get-property))
      (define spam)
      (define frob)
      (define rats)
      (define x (make-parameter 444))
      (define-property x spam "x-spam")
      (define-property x frob "x-frob")
      (printf "~s ~s ~s ~s\n"
        (get-property x spam)
        (get-property x frob)
        (get-property x rats)
        (x)))
    #t)
  (begin (define dp-f) #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (set! dp-f
          (eval
            '(lambda ()
               (import (dp l1))
               (printf "~s ~s ~s ~s\n"
                 (get-property x spam)
                 (get-property x frob)
                 (get-property x rats)
                 (x)))))))
    "\"x-spam\" \"x-frob\" #f 444\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (dp-f)))
    "\"x-spam\" \"x-frob\" #f 444\n")
  (begin
    (library (dp l1) (export x spam frob rats) (import (scheme) (dp get-property))
      (define spam)
      (define frob)
      (define rats)
      (define-syntax x
        (identifier-syntax
          (list
            (get-property x spam)
            (get-property x frob)
            (get-property x rats))))
      (define-property x spam "x-spam")
      (define-property x frob "x-frob")
      (printf "~s ~s ~s ~s\n"
        (get-property x spam)
        (get-property x frob)
        (get-property x rats)
        x))
    #t)
  (begin (define dp-f) #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (set! dp-f
          (eval
            '(lambda ()
               (import (dp l1))
               (printf "~s ~s ~s ~s\n"
                 (get-property x spam)
                 (get-property x frob)
                 (get-property x rats)
                 x))))))
    "")
  (equal?
    (with-output-to-string
      (lambda ()
        (dp-f)))
    "\"x-spam\" \"x-frob\" #f (\"x-spam\" \"x-frob\" #f)\n")
  (begin
    (library (dp l1) (export x qq spam frob rats) (import (scheme) (dp get-property))
      (define spam)
      (define frob)
      (define rats)
      (define qq (make-parameter 33))
      (define-syntax x
        (identifier-syntax
          (list
            (get-property x spam)
            (get-property x frob)
            (get-property x rats))))
      (define-property x spam "x-spam")
      (define-property x frob "x-frob")
      (printf "~s ~s ~s ~s\n"
        (get-property x spam)
        (get-property x frob)
        (get-property x rats)
        x))
    #t)
  (begin (define dp-f) #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (set! dp-f
          (eval
            '(lambda ()
               (import (dp l1))
               (printf "~s ~s ~s ~s ~s\n"
                 (get-property x spam)
                 (get-property x frob)
                 (get-property x rats)
                 x (qq)))))))
    "\"x-spam\" \"x-frob\" #f (\"x-spam\" \"x-frob\" #f)\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (dp-f)))
    "\"x-spam\" \"x-frob\" #f (\"x-spam\" \"x-frob\" #f) 33\n")
  (begin
    (library (dp l1) (export qq spam frob rats) (import (scheme) (dp get-property))
      (define spam)
      (define frob)
      (define rats)
      (define qq (make-parameter 77))
      (define x (make-parameter 444))
      (define-property x spam "x-spam")
      (define-property x frob "x-frob")
      (printf "~s ~s ~s ~s\n"
        (get-property x spam)
        (get-property x frob)
        (get-property x rats)
        (x)))
    #t)
  (begin (define dp-f) #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (set! dp-f
          (eval
            '(lambda (x)
               (import (dp l1))
               (printf "~s ~s ~s ~s\n"
                 (get-property x spam)
                 (get-property x frob)
                 (get-property x rats)
                 (qq)))))))
    "\"x-spam\" \"x-frob\" #f 444\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (dp-f 0)))
    "#f #f #f 77\n")
  (begin
    (module (dp-a)
      (module (dp-a)
        (define-syntax dp-a (identifier-syntax 3)))
      (define-property dp-a spam 55))
    (and (eqv? dp-a 3)
         (eqv? (get-property dp-a spam) 55)))
  (begin
    (module (dp-b)
      (module ((dp-b q))
        (define q 3)
        (define-syntax dp-b (identifier-syntax q)))
      (define-property dp-b spam 55))
    (and (eqv? dp-b 3)
         (eqv? (get-property dp-b spam) 55)))
  (let ()
    (module (dp-c)
      (module (dp-c)
        (define-syntax dp-c (identifier-syntax 3)))
      (define-property dp-c spam 55))
    (and (eqv? dp-c 3)
         (eqv? (get-property dp-c spam) 55)))
  (let ()
    (module (dp-c)
      (module ((dp-c q))
        (define q 3)
        (define-syntax dp-c (identifier-syntax q)))
      (define-property dp-c spam 55))
    (and (eqv? dp-c 3)
         (eqv? (get-property dp-c spam) 55)))
  (begin
    (library (dp l2) (export dp-d dp-e spam) (import (scheme))
      (define spam)
      (module (dp-d)
        (module (dp-d)
          (define-syntax dp-d (identifier-syntax 3)))
        (define-property dp-d spam 55))
      (module (dp-e)
        (module ((dp-e q))
          (define q 13)
          (define-syntax dp-e (identifier-syntax q)))
        (define-property dp-e spam 155)))
    (let ()
      (import (dp l2))
      (and (eqv? dp-d 3)
           (eqv? (get-property dp-d spam) 55)
           (eqv? dp-e 13)
           (eqv? (get-property dp-e spam) 155))))
  (begin
    (import (dp l2))
    (and (eqv? dp-d 3)
         (eqv? (get-property dp-d spam) 55)
         (eqv? dp-e 13)
         (eqv? (get-property dp-e spam) 155)))
  (begin
    (with-output-to-file "testfile-dp0.ss"
      (lambda ()
        (pretty-print '(define $dp0-x "dp0-x"))
        (pretty-print '(define-property $dp0-x dp0 17)))
      'replace)
    (with-output-to-file "testfile-dp1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp1)
             (export cons a b spud)
             (import (scheme))
             (define spud)
             (define a "a")
             (define b "b")
             (define-property cons spud "spud-cons")
             (define-property a spud "spud-a")
             (define-property b spud "spud-b"))))
      'replace)
    (with-output-to-file "testfile-dp2.ss"
      (lambda ()
        (pretty-print
          '(module dp2 (cons a b putz)
             (import (scheme))
             (define putz)
             (define a "a")
             (define b "b")
             (define-property cons putz "putz-cons")
             (define-property a putz "putz-a")
             (define-property b putz "putz-b"))))
      'replace)
    (for-each separate-compile '(dp0 dp1 dp2))
    #t)
  (begin (load "testfile-dp0.so") #t)
  (equal? $dp0-x "dp0-x")
  (equal? (get-property $dp0-x dp0) 17)
  (equal?
    (let ()
      (import (testfile-dp1))
      (list (cons a b) (get-property cons spud) (get-property a spud) (get-property b spud)))
    '(("a" . "b") "spud-cons" "spud-a" "spud-b"))
  (begin (load "testfile-dp2.so") #t)
  (equal?
    (let ()
      (import dp2)
      (list (cons a b) (get-property cons putz) (get-property a putz) (get-property b putz)))
    '(("a" . "b") "putz-cons" "putz-a" "putz-b"))
 ; illustrate use of define-property for storing parent record info,
 ; while still allowing the record name to be a variable whose value
 ; is the record type descriptor
  (equal?
    (let ()
      (module (drt)
        (define drt-key)
        (define-syntax drt
          (lambda (x)
            (define construct-name
              (lambda (template-identifier . args)
                (datum->syntax template-identifier
                  (string->symbol
                    (apply string-append
                      (map (lambda (x)
                             (if (string? x)
                                 x
                                 (symbol->string (syntax->datum x))))
                           args))))))
            (define do-drt
              (lambda (rname fname* prtd)
                (with-syntax ([rname rname]
                              [rtd (make-record-type-descriptor
                                     (syntax->datum rname) prtd #f #f #f
                                     (list->vector (map (lambda (fname) `(immutable ,(syntax->datum fname))) fname*)))]
                              [make-rname (construct-name rname "make-" rname)]
                              [rname? (construct-name rname rname "?")]
                              [(rname-fname ...)
                               (map (lambda (fname) (construct-name fname rname "-" fname))
                                    fname*)]
                              [(i ...) (enumerate fname*)])
                  #'(begin
                      (define rname 'rtd)
                      (define rcd (make-record-constructor-descriptor 'rtd #f #f))
                      (define-property rname drt-key 'rtd)
                      (define make-rname (record-constructor rcd))
                      (define rname? (record-predicate 'rtd))
                      (define rname-fname (record-accessor 'rtd i))
                      ...))))
            (syntax-case x (parent)
              [(_ rname fname ...)
               (for-all identifier? #'(rname fname ...))
               (do-drt #'rname #'(fname ...) #f)]
              [(_ rname (parent pname) fname ...)
               (for-all identifier? #'(rname pname fname ...))
               (lambda (r)
                 (let ([prtd (r #'pname #'drt-key)])
                   (unless prtd (syntax-error #'pname "unrecognized parent record typd"))
                   (do-drt #'rname #'(fname ...) prtd)))]))))
      (drt foo x y)
      (drt bar (parent foo) z)
      (let ([b (make-bar 1 2 3)])
        (list
          (record-type-descriptor? foo)
          (record-type-descriptor? bar)
          (foo? b) (bar? b)
          (foo-x b)
          (foo-y b)
          (bar-z b))))
    '(#t #t #t #t 1 2 3))
 ; on no!
  (equal?
    (let ()
      (define type-key)
      (define-syntax declare
        (syntax-rules ()
          [(_ type id)
           (identifier? #'id)
           (define-property id type-key #'type)]))
      (define-syntax type-of
        (lambda (x)
          (syntax-case x ()
            [(_ id)
             (identifier? #'id)
             (lambda (r)
               #`'#,(r #'id #'type-key))])))
      (let ([x 3])
        (define p (lambda (x) x))
        (declare fixnum? x)
        (declare procedure? p)
        (list (type-of x) (type-of p))))
    '(fixnum? procedure?))
 ; make sure library is visited and invoked when needed by
 ; top-level-xxx procedures, even when properties are defined
  (begin
    (with-output-to-file "testfile-dp3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp3) (export dp3-x frop) (import (chezscheme))
             (define frop)
             (define dp3-x 3)
             (define-property dp3-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp3))
    #t)
  (begin (import (testfile-dp3)) #t)
  (top-level-bound? 'dp3-x)
  (equal? (get-property dp3-x frop) "blob")
  (begin
    (with-output-to-file "testfile-dp4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp4) (export dp4-x frop) (import (chezscheme))
             (define frop)
             (define dp4-x 3)
             (define-property dp4-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp4))
    #t)
  (begin (import (testfile-dp4)) #t)
  (eqv? (top-level-value 'dp4-x) 3)
  (equal? (get-property dp4-x frop) "blob")
  (begin
    (with-output-to-file "testfile-dp5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp5) (export dp5-x frop) (import (chezscheme))
             (define frop)
             (define dp5-x 3)
             (define-property dp5-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp5))
    #t)
  (begin (import (testfile-dp5)) #t)
 ; same as last, but reverse order of checks
  (equal? (get-property dp5-x frop) "blob")
  (eqv? (top-level-value 'dp5-x) 3)
  (begin
    (with-output-to-file "testfile-dp6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp6) (export dp6-x frop) (import (chezscheme))
             (define frop)
             (define-syntax dp6-x (identifier-syntax 3))
             (define-property dp6-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp6))
    #t)
  (begin (import (testfile-dp6)) #t)
  (top-level-syntax? 'dp6-x)
  (equal? (get-property dp6-x frop) "blob")
  (begin
    (with-output-to-file "testfile-dp7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp7) (export dp7-x frop) (import (chezscheme))
             (define frop)
             (define-syntax dp7-x (identifier-syntax 3))
             (define-property dp7-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp7))
    #t)
  (begin (import (testfile-dp7)) #t)
 ; same as last, but reverse order of checks
  (equal? (get-property dp7-x frop) "blob")
  (top-level-syntax? 'dp7-x)
  (begin
    (with-output-to-file "testfile-dp8.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp8) (export dp8-x frop) (import (chezscheme))
             (define frop)
             (define-syntax dp8-x (identifier-syntax 3))
             (define-property dp8-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp8))
    #t)
  (begin (import (testfile-dp8)) #t)
 ; same as last, but reverse order of checks
  (procedure? (top-level-syntax 'dp8-x))
  (equal? (get-property dp8-x frop) "blob")
  (begin
    (with-output-to-file "testfile-dp9.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp9) (export dp9-x frop) (import (chezscheme))
             (define frop)
             (define-syntax dp9-x (identifier-syntax 3))
             (define-property dp9-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp9))
    #t)
  (begin (import (testfile-dp9)) #t)
  (error? ; not a variable
    (set-top-level-value! 'dp9-x 11))
  (equal? (get-property dp9-x frop) "blob")
  (begin
    (with-output-to-file "testfile-dp10.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp10) (export dp10-x frop) (import (chezscheme))
             (define frop)
             (define dp10-x 3)
             (define-property dp10-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp10))
    #t)
  (begin (import (testfile-dp10)) #t)
  (error? ; immutable
    (set-top-level-value! 'dp10-x 11))
  (equal? (get-property dp10-x frop) "blob")
  (begin
    (with-output-to-file "testfile-dp11.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp11) (export dp11-x frop) (import (chezscheme))
             (define frop)
             (define dp11-x 3)
             (define-property dp11-x frop "blob"))))
      'replace)
    (for-each separate-compile '(dp11))
    #t)
  (begin (import (testfile-dp11)) #t)
  (not (top-level-mutable? 'dp11-x))
  (equal? (get-property dp11-x frop) "blob")
  (equal?
    (syntax-case '(a b c) ()
      [(_ . x)
       (let ()
         (define-property x goofy 'stuff)
         (define-property x amazingly 'unlikely)
         (list (get-property x goofy)
               (get-property x amazingly)
               #'x))])
    '(stuff unlikely (b c)))
 (begin
   (library (docstring)
     (export define-docstring get-docstring)
     (import (chezscheme))
     (define check-docstring
       (lambda (x s)
         (unless (string? s)
           (syntax-error x "invalid docstring definition"))
         s))
     (define-syntax define-docstring
       (lambda (x)
         (syntax-case x ()
           [(_ id expr)
            #`(define-property id check-docstring
                (check-docstring #'#,x expr))])))
     (define-syntax get-docstring
       (lambda (x)
         (lambda (r)
           (syntax-case x ()
             [(_ id)
              (or (r #'id #'check-docstring) "no documentation available")])))))
   #t)
  (equal?
    (let ()
      (import (docstring))
      (define-docstring cons "cons takes three arguments")
      (get-docstring cons))
    "cons takes three arguments")
  (equal?
    (let ()
      (import (docstring))
      (define-docstring else "else is cool")
      (cond [else (get-docstring else)]))
    "else is cool")
  ((lambda (x ls) (and (member x ls) #t))
   (parameterize ([#%$suppress-primitive-inlining #f])
     (expand
       '(let ()
          (import scheme)
          (define-property cons car 3)
          cons)))
   `(#%cons #2%cons #3%cons))
  (begin
    (define dp-x #f)
    (define dp-y #f)
    (define-property dp-x dp-y "xy")
    (define-syntax a
      (lambda (z)
        (define-property dp-x z "xz")
        #'(get-property dp-x dp-y)))
    (equal? a "xy"))
  (begin
    (define dp-x #f)
    (define dp-y #f)
    (define-property dp-x dp-y "outer")
    (define-syntax a
      (lambda (z)
        (define-property dp-x dp-y "inner")
        #'(get-property dp-x dp-y)))
    (not a))
  (equal?
    (let ([x #f] [y #f])
      (define-property x y "xy")
      (define-syntax a
        (lambda (z)
          (define-property x z "xz")
          #'(get-property x y)))
      a)
    "xy")
  (eq?
    (let ([x #f] [y #f])
      (define-property x y "outer")
      (define-syntax a
        (lambda (z)
          (define-property x y "inner")
          #'(get-property x y)))
      a)
    #f)
  (eq?
    (let ([x #f])
      (define-syntax a
        (syntax-rules (x)
          [(_ x) 'yes]
          [(_ y) 'no]))
      (let ()
        (define-property x q 0)
        (a x)))
    'yes)
  (begin
    (library (dp l3) (export x)
      (import (chezscheme))
      (define x 5)
      (define-property x car 17))
    (import (dp l3))
    (and (eqv? x 5) (eqv? (let () (import (chezscheme)) (get-property x car)) 17)))
  (begin
    (library (dp l4) (export sort)
      (import (chezscheme))
      (define-property sort car 53))
    (library (dp l5) (export sort)
      (import (chezscheme))
      (define-property sort cdr 87))
    (import (dp l4))
    (import (dp l5))
    (and (procedure? sort)
         (eq? sort #%sort)
         (eqv? (let () (import (only (chezscheme) car)) (get-property sort car)) 53)
         (eqv? (let () (import (only (chezscheme) cdr)) (get-property sort cdr)) 87)))
  (begin
    (with-output-to-file "testfile-dp12.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-dp12) (export dp12-dq) (import (chezscheme))
             (define-syntax dp12-dq (identifier-syntax "dq"))
             (define-property dp12-dq car "dqp"))))
      'replace)
    (for-each separate-compile '(dp12))
    #t)
  (begin (import (testfile-dp12)) #t)
  (equal? (list dp12-dq (let () (import (chezscheme)) (get-property dp12-dq car))) '("dq" "dqp"))
  (equal?
    (let ()
      (define x 0)
      (module m1 (x) (define-property x car "xcar"))
      (module m2 (x) (define-property x cdr "xcdr"))
      (let ([q1 (let () (import m1) (list x (get-property x car) (get-property x cdr)))]
            [q2 (let () (import m2) (list x (get-property x car) (get-property x cdr)))]
            [q3 (let () (import m1) (import m2) (list x (get-property x car) (get-property x cdr)))]
            [q4 (let () (import m2) (import m1) (list x (get-property x car) (get-property x cdr)))])
        (list x q1 q2 q3 q4 (get-property x car) (get-property x cdr))))
    '(0 (0 "xcar" #f) (0 #f "xcdr") (0 "xcar" "xcdr") (0 "xcar" "xcdr") #f #f))
  (equal?
    (let ()
      (define x 0)
      (module m1 (x) (define-property x car "xcar"))
      (import m1)
      (module m2 (x) (define-property x cdr "xcdr"))
      (import m2)
      (list x (get-property x car) (get-property x cdr)))
    '(0 "xcar" "xcdr"))
  (begin
    (module $dp13 (foo)
      (define foo 17)
      (module ((foo bar))
        (define-property foo cons #'bar)
        (define bar 35)))
    #t)
  (eqv?
    (let ()
      (import $dp13)
      (define-syntax a
        (lambda (x)
          (lambda (r)
            (syntax-case x ()
              [(_ id) (r #'id #'cons)]))))
      (a foo))
    35)
  (eqv?
    (let ()
      (module m (x) (define x 3) (define-property x x 4))
      (import m)
      (get-property x x))
    4)
  (eqv?
    (let ()
      (module m (x) (define x 3) (define-property x x 4))
      (import (alias m (x y)))
      (get-property x x))
    4)
  (eqv?
    (let ()
      (module m (x) (define x 3) (define-property x x 4))
      (import (alias m (x y)))
      (get-property x y))
    4)
  (eqv?
    (let ()
      (module m (x) (define x 3) (define-property x x 4))
      (import (alias m (x y)))
      (get-property y x))
    4)
  (eqv?
    (let ()
      (module m (x) (define x 3) (define-property x x 4))
      (import (alias m (x y)))
      (get-property y y))
    4)
  (eqv?
    (let ()
      (module m (x) (define x 3) (define-property x x 4))
      (import (rename m (x y)))
      (get-property y y))
    4)
  (begin
    (module $dp14 (x) (define x 3) (define-property x x 4))
    #t)
  (eqv?
    (let ()
      (import $dp14)
      (get-property x x))
    4)
  (eqv?
    (let ()
      (import (alias $dp14 (x y)))
      (get-property x x))
    4)
  (eqv?
    (let ()
      (import (alias $dp14 (x y)))
      (get-property x y))
    4)
  (eqv?
    (let ()
      (import (alias $dp14 (x y)))
      (get-property y x))
    4)
  (eqv?
    (let ()
      (import (alias $dp14 (x y)))
      (get-property y y))
    4)
  (eqv?
    (let ()
      (import (rename $dp14 (x y)))
      (get-property y y))
    4)
  (equal?
    (let ([y 14])
      (define k1)
      (define k2)
      (module ()
        (export x (rename (y x)))
        (define x 3)
        (define-property x k1 4)
        (define-property x k2 5)
        (alias y x))
      (list x y (get-property x k1) (get-property x k2) (get-property y k1) (get-property y k2)))
    '(3 14 4 5 #f #f))
  (error? ; attempt to export different bindings for x
    (let ([y 14])
      (define k1)
      (define k2)
      (module ()
        (export x (rename (y x)))
        (define x 3)
        (define-property x k1 4)
        (alias y x)
        (define-property x k2 5))
      (list x y (get-property x k1) (get-property y k2))))
  (begin
    (with-output-to-file "testfile-A.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-A)
             (export $testfile-A-x $testfile-A-prop-id)
             (import (scheme))
             (define $testfile-A-x (cons 'a 'b))
             (define $testfile-A-prop-id)
             (define-property $testfile-A-x $testfile-A-prop-id (cons 'c 'd)))))
      'replace)
    (with-output-to-file "testfile-B.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-B)
             (export)
             (import (scheme) (testfile-A))
             (export (import (testfile-A))))))
      'replace)
    (with-output-to-file "testfile-C.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-C)
             (export)
             (import (scheme) (testfile-A) (testfile-B))
             (export (import (testfile-A)) (import (testfile-B))))))
      'replace)
    (for-each separate-compile '(A B C))
    #t)
  (equal?
    (let ()
      (import (testfile-C))
      (list $testfile-A-x (get-property $testfile-A-x $testfile-A-prop-id)))
    '((a . b) (c . d)))
)

(mat library1
  (error? (compile-library "/file/not/there"))
  (error? (load-library "/file/not/there"))
  (error? ; abc is not a string
    (load-library 'abc))
  (error? ; xxx is not a procedure
    (load-library "/file/not/there" 'xxx))
  (error? ; 3 is not a string
    (parameterize ([source-directories '("/tmp" ".")]) (load-library 3)))
  (error? ; 3 is not a string
    (parameterize ([source-directories '("/tmp" ".")]) (load-library 3 values)))
  (begin
    (library ($l1-a) (export $l1-x) (import (scheme))
      (module $l1-x (($l1-a $l1-b) $l1-c $l1-e)
        (define $l1-d 4)
        (define-syntax $l1-a (identifier-syntax (cons $l1-b $l1-y)))
        (define $l1-b 55)
        (define $l1-c (lambda () (* $l1-d $l1-y)))
        (define $l1-f 44)
        (define-syntax $l1-e (identifier-syntax $l1-f)))
      (define $l1-y 14))
    #t)
  (equal?
    (let () (import ($l1-a)) (import $l1-x) (list $l1-a ($l1-c)))
    '((55 . 14) 56))
  (begin
    (import ($l1-a))
    #t)
  (begin
    (import $l1-x)
    #t)
  (equal? $l1-a '(55 . 14))
  (equal? ($l1-c) 56)
  (error? ; unbound variable $l1-b
    $l1-b)
  (error? ; unbound variable $l1-d
    $l1-d)
  (error? ; unbound variable $l1-y
    $l1-y)
  (error? ; unexported identifier $l1-f
    $l1-e)
  (error? ; unbound variable $l1-f
    $l1-f)
  (equal?
    (let () (import ($l1-a)) (import $l1-x) (list $l1-a ($l1-c)))
    '((55 . 14) 56))
  (begin
    (library ($l1-b) (export $l1-x) (import (scheme))
      (module $l1-x ($l1-a $l1-c $l1-e)
        (define $l1-d 4)
        (define $l1-a (lambda () (cons $l1-b $l1-y)))
        (define $l1-b 55)
        (define $l1-c (lambda () (* $l1-d $l1-y)))
        (define $l1-f 44)
        (define $l1-e (lambda () $l1-f)))
      (define $l1-y 14))
    #t)
  (equal?
    (let () (import ($l1-b)) (import $l1-x) (vector ($l1-a) ($l1-c) ($l1-e)))
    '#((55 . 14) 56 44))
  (begin
    (import ($l1-b))
    #t)
  (begin
    (import $l1-x)
    #t)
  (equal? ($l1-a) '(55 . 14))
  (equal? ($l1-c) 56)
  (equal? ($l1-e) 44)
  (error? ; unbound variable $l1-b
    $l1-b)
  (error? ; unbound variable $l1-d
    $l1-d)
  (error? ; unbound variable $l1-y
    $l1-y)
  (error? ; unbound variable $l1-f
    $l1-f)
  (equal?
    (let () (import ($l1-b)) (import $l1-x) (vector ($l1-a) ($l1-c) ($l1-e)))
    '#((55 . 14) 56 44))
  (begin
    (library ($l1-c) (export (rename (q $l1-q) (a:x $l1-x)) $l1-p)
      (import (scheme) (rename ($l1-a) ($l1-x a:x)) (rename ($l1-b) ($l1-x b:x)))
      (import (drop-prefix a:x $l1-) (prefix (drop-prefix b:x $l1-) b:))
      (define-syntax q (identifier-syntax (list a (c) (b:a) (b:c) ($l1-p) (r))))
      (define $l1-p (lambda () (vector a (c) (b:a) (b:c))))
      (define r (lambda () (cons* a (c) (b:a) (b:c)))))
    #t)
  (equal?
    (let () (import ($l1-c)) $l1-q)
    '((55 . 14) 56 (55 . 14) 56
      #4((55 . 14) 56 (55 . 14) 56)
      ((55 . 14) 56 (55 . 14) . 56)))
  (equal?
    (let () (import ($l1-c) ($l1-a)) (import $l1-x) (list $l1-a $l1-q))
    '((55 . 14)
      ((55 . 14) 56 (55 . 14) 56
       #4((55 . 14) 56 (55 . 14) 56)
       ((55 . 14) 56 (55 . 14) . 56))))

  (begin
    (library ($l1-d) (export $l1-x $l1-getx $l1-setx!) (import (scheme))
      (define x 0)
      (define-syntax $l1-x (identifier-syntax x))
      (define $l1-getx (lambda () x))
      (define $l1-setx! (lambda (v) (set! x v))))
    #t)
  (eqv?
    (let () (import ($l1-d)) ($l1-setx! 'hello) ($l1-getx))
    'hello)
  (error? ; unexported identifier x
    (let () (import ($l1-d)) $l1-x))
  (error? ; unexported identifier x
    (expand '(let () (import ($l1-d)) $l1-x)))
  (error? ; immutable variable $l1-x
    (let () (import ($l1-d)) (set! $l1-getx void)))
  (error? ; immutable variable $l1-x
    (expand '(let () (import ($l1-d)) (set! $l1-getx void))))
  (begin
    (import ($l1-d))
    #t)
  (eqv?
    (begin ($l1-setx! 'hello) ($l1-getx))
    'hello)
  (error? ; unexported identifier x
    $l1-x)
  (error? ; unexported identifier x
    (expand '$l1-x))
  (error? ; immutable variable $l1-x
    (set! $l1-getx void))
  (error? ; immutable variable $l1-x
    (expand '(set! $l1-getx void)))

  (error?
    (library ($l1-e) (export $l1-x) (import (scheme))
      (define $l1-x 0)
      (set! $l1-x 1)))
  (error?
    (expand
      '(library ($l1-e) (export $l1-x) (import (scheme))
         (define $l1-x 0)
         (set! $l1-x 1))))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(library ($l1-f) (export $l1-x $l1-y) (import (scheme))
             (define-syntax $l1-x (identifier-syntax q))
             (define-syntax q
               (begin
                 (printf "An expand-time greeting from $l1-f\n")
                 (lambda (x) 77)))
             (define $l1-y (lambda () (* q 2)))
             (printf "A run-time greeting from $l1-f\n")))
        (pretty-print
          '(library ($l1-g) (export $l1-x $l1-z $l1-w) (import (scheme) ($l1-f))
             (define-syntax $l1-z
               (begin
                 (printf "An expand-time greeting from $l1-g\n")
                 (lambda (x) ($l1-y))))
             (define $l1-w
               (begin
                 (printf "A run-time greeting from $l1-g\n")
                 (lambda (x) (cons* x $l1-x ($l1-y)))))))
        (pretty-print
          '(library ($l1-h) (export $l1-x $l1-y $l1-v) (import (scheme) ($l1-f) ($l1-g))
             (define $l1-v (list $l1-x ($l1-y) $l1-z ($l1-w 13)))
             (printf "A run-time greeting from $l1-h\n"))))
      'replace)
    (compile-file "testfile")
    #t)
 ; look, ma, no need to load...
  (equal?
    (let () (import ($l1-h)) $l1-v)
    '(77 154 154 (13 77 . 154)))
  (begin
    (library ($l1-h) (export $l1-x $l1-y $l1-v) (import (scheme))
      (define $l1-x "these aren't")
      (define $l1-y "the exports")
      (define $l1-v "you're looking for"))
    #t)
  (begin (load "testfile.so") #t)
  (equal?
    (let () (import ($l1-h)) $l1-v)
    '(77 154 154 (13 77 . 154)))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(library ($l1-f) (export $l1-x $l1-y) (import (scheme))
             (define-syntax $l1-x (identifier-syntax q))
             (define-syntax q
               (begin
                 (printf "An expand-time greeting from $l1-f\n")
                 (lambda (x) 77)))
             (define $l1-y (lambda () (* q 2)))
             (printf "A run-time greeting from $l1-f\n")))
        (pretty-print
          '(library ($l1-g) (export $l1-x $l1-z $l1-w) (import (scheme) ($l1-f))
             (define-syntax $l1-z
               (begin
                 (printf "An expand-time greeting from $l1-g\n")
                 (lambda (x) ($l1-y))))
             (define $l1-w
               (begin
                 (printf "A run-time greeting from $l1-g\n")
                 (lambda (x) (cons* x $l1-z $l1-x ($l1-y)))))))
        (pretty-print
          '(library ($l1-h) (export $l1-x $l1-y $l1-v) (import (scheme) ($l1-f) ($l1-g))
             (define $l1-v (list $l1-x ($l1-y) $l1-z ($l1-w 13)))
             (printf "A run-time greeting from $l1-h\n"))))
      'replace)
    (compile-file "testfile")
    #t)
 ; look, ma, no need to load...
  (equal?
    (let () (import ($l1-h)) $l1-v)
    '(77 154 154 (13 154 77 . 154)))
  (begin
    (library ($l1-h) (export $l1-x $l1-y $l1-v) (import (scheme))
      (define $l1-x "these aren't")
      (define $l1-y "the exports")
      (define $l1-v "you're looking for"))
    #t)
  (begin (load "testfile.so") #t)
  (equal?
    (let () (import ($l1-h)) $l1-v)
    '(77 154 154 (13 154 77 . 154)))

  (error? ; unknown library ($l1-ham)
    (begin
      (library ($l1-spam) (export) (import ($l1-ham)))
      (library ($l1-ham) (export) (import ($l1-spam)))))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(library ($l1-i) (export $l1-x $l1-y) (import (scheme))
             (define $l1-x 'i-am-x)
             (define-syntax $l1-y (identifier-syntax 'i-am-y))))
        (pretty-print
          '(library ($l1-j) (export $l1-x $l1-y)
             (import ($l1-i) (only (scheme) errorf))
             (errorf #f "this error shouldn't happen")))
        (pretty-print
          '(library ($l1-k) (export $l1-z) (import (scheme) ($l1-j))
             (define $l1-z (list 'i-am-z $l1-x $l1-y)))))
      'replace)
    (compile-file "testfile")
    #t)
  (equal?
    (let () (import ($l1-k)) $l1-z)
    '(i-am-z i-am-x i-am-y))
  (begin (load "testfile.so") #t)
  (equal?
    (let () (import ($l1-k)) $l1-z)
    '(i-am-z i-am-x i-am-y))

  (begin
    (library ($l1-l) (export $l1-x) (import (scheme))
      (define $l1-x 'i-am-$l1-l.$l1-x))
    #t)
  (eq?
    (let ()
      (import ($l1-l))
      (define-syntax a (lambda (x) #`'#,(datum->syntax #'* $l1-x)))
      a)
    'i-am-$l1-l.$l1-x)

  (begin
    (with-output-to-file "testfile-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a1)
             (export $l1-a)
             (import (scheme))
             (define $l1-a 'a1))))
      'replace)
    (with-output-to-file "testfile-b1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b1)
             (export $l1-a $l1-b)
             (import (scheme) (testfile-a1))
             (define $l1-b 'b1))))
      'replace)
    (with-output-to-file "testfile-c1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-c1)
             (export $l1-a $l1-b $l1-c)
             (import (scheme) (testfile-b1))
             (define ($l1-c) (list $l1-a $l1-b 'c1)))))
      'replace)
    (with-output-to-file "testfile-d1.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-b1)))
        (pretty-print '(define ($l1-d) (list $l1-a $l1-b 'd1))))
      'replace)
    (with-output-to-file "testfile-e1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-e1)
             (export $l1-e)
             (import (scheme) (testfile-b1))
             (alias $l1-e $l1-a))))
      'replace)
    (with-output-to-file "testfile-f1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-f1)
             (export $l1-f)
             (import (scheme))
             (define-syntax $l1-f (identifier-syntax "macro-f")))))
      'replace)
    (with-output-to-file "testfile-g1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-g1)
             (export $l1-f)
             (import (scheme) (testfile-f1)))))
      'replace)
    (with-output-to-file "testfile-h1.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-g1)))
        (pretty-print '(define ($l1-h) (list $l1-f))))
      'replace)
    (for-each separate-compile '(a1 b1 c1 d1 e1 f1 g1 h1))
    #t)
  (equal? (begin (load "testfile-d1.so") ($l1-d)) '(a1 b1 d1))
  (begin (import (testfile-c1)) #t)
  (equal? ($l1-c) '(a1 b1 c1))
  (begin (import (testfile-e1)) #t)
  (equal? $l1-e 'a1)
  (equal? (begin (load "testfile-h1.so") ($l1-h)) '("macro-f"))

  (begin
    (with-output-to-file "testfile-a2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a2)
             (export $l1-a)
             (import (scheme))
             (define $l1-a 'a2))))
      'replace)
    (with-output-to-file "testfile-b2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b2)
             (export $l1-a $l1-b)
             (import (scheme) (testfile-a2))
             (define $l1-b 'b2))))
      'replace)
    (with-output-to-file "testfile-c2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-c2)
             (export $l1-a $l1-b $l1-c)
             (import (scheme) (testfile-b2))
             (define ($l1-c) (list $l1-a $l1-b 'c2)))))
      'replace)
    (with-output-to-file "testfile-d2.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-b2)))
        (pretty-print '(define ($l1-d) (list $l1-a $l1-b 'd2))))
      'replace)
    (for-each separate-compile '(a2 b2 c2 d2 a2))
    #t)
  (error? ; expected different compilation instance
          ; program complains about b2 rather than b2 about a2
          ;   now that load-library reloads source when dependency changes
          ;   would be nice if program were reloaded from source as well
    (load "testfile-d2.so"))
  ; no longer fails now that load-library reloads source when dependency changes
  #;(error? ; expected different compilation instance
    (import (testfile-c2)))
  (begin
    (library ($l1-m) (export $l1-x) (import (scheme)) (define $l1-x 333))
    (library ($l1-n) (export $l1-x) (import (scheme)) (import ($l1-m)))
    #t)
  (eqv?
    (let () (import ($l1-n)) $l1-x)
    333)
  (begin
    (define-syntax $from1
      (syntax-rules ()
        ((_ m id)
         (let () (import-only m) id))))
    (define-syntax $from2
      (syntax-rules ()
        ((_ m id)
         (let () (module (id) (import m)) id))))
    (define-syntax $from3
      (syntax-rules ()
        [(_ m id)
         (let ([z (cons 1 2)])
           (let ([id z])
             (import m)
             (let ([t id])
               (if (eq? t z) (errorf 'from "~s undefined" 'id) t))))]))
    (library ($frappe) (export wire whip) (import (scheme))
      (define wire 3)
      (define-syntax whip (identifier-syntax egg))
      (define egg 'whites))
    (equal?
      (list (cons ($from1 ($frappe) wire) ($from1 ($frappe) whip))
            (cons ($from2 ($frappe) wire) ($from2 ($frappe) whip))
            (cons ($from3 ($frappe) wire) ($from3 ($frappe) whip)))
      '((3 . whites) (3 . whites) (3 . whites))))
  (begin
    (library ($q) (export m from) (import (scheme))
      (module m (f) (define f "this is f"))
      (define-syntax from
        (syntax-rules () [(_ m id) (let () (import-only m) id)])))
    (equal? (let () (import-only ($q)) (from m f)) "this is f"))
  (begin
    (library ($p) (export d f) (import (scheme))
      (define-syntax d
        (syntax-rules ()
          ((_ e) (m (lambda () e)))))
      (define m (lambda (x) x))
      (define f (lambda (th) (th))))
    (eqv? (let () (import-only ($p)) (f (d 2))) 2))
 ; this works for libraries because m is implicitly exported
  (eqv? (let () (import-only ($p)) (f (d 1/3))) 1/3)
  (error? ; cons undefined
    (let () (import-only ($p)) (f (d cons))))
  (error? ; invalid syntax
    (library (a) (export x:eval) (import (add-prefix (rnrs eval) x))))
  (error? ; invalid syntax
    (library (a) (export val) (import (drop-prefix (rnrs eval) x))))
  (error? ; invalid syntax
    (library (a) (export meaning) (import (alias (rnrs eval) [eval meaning]))))
  (begin
    (define $l1-q1)
    (define $l1-q2)
    (define-syntax $l1-qlib
      (syntax-rules ()
        [(_ name (export ex ...) (import im ...) body ...)
         (begin
           (library name (export ex ... q)
             (import im ... (rename (only (rnrs) cons) (cons list)))
             (define q list) body ...)
           (let () (import name) (set! $l1-q1 q)))]))
    ($l1-qlib ($l1-libfoo) (export q) (import (rnrs)) (define q list))
    (let () (import ($l1-libfoo)) (set! $l1-q2 q))
    (equal? (list $l1-q1 $l1-q2) (list cons list)))
 ; check for existence of chezscheme library
  (begin
    (library ($l1-r1) (export $l1-x) (import (chezscheme))
      (define $l1-x (sort < '(1 3 2 0 5))))
    (library ($l1-r2) (export $l1-y) (import (chezscheme) ($l1-r1))
      (define $l1-y (cons $l1-x (void))))
    (equal? (let () (import ($l1-r2)) $l1-y) `((0 1 2 3 5) . ,(void))))
  (error? ; invalid context for library form
    (module (a) (library (a) (export) (import))))
  (error? ; invalid syntax for library form
    (module (a) (library a (import) (export x) (define x 3)) (import a) x))
  (error? ; invalid context for top-level-program form
    (module (a) (top-level-program (import))))
  (error? ; invalid syntax for top-level-program form
    (module (a) (top-level-program (display "hello"))))
  (error? ; invalid context for library form
    (lambda () (library (a) (export) (import))))
  (error? ; invalid syntax for library form
    (lambda () (library a (import) (export x) (define x 3)) (import a) x))
  (error? ; invalid context for top-level-program form
    (lambda () (top-level-program (import))))
  (error? ; invalid syntax for top-level-program form
    (lambda () (top-level-program (display "hello"))))
  (error? ; defnie not defined
    (library ($l1-s) (export y) (import (rnrs)) (defnie x 3) (define y 4)))

  (begin
    (library ($l1-s)
      (export m)
      (import (chezscheme))
      (module m (x set-x!)
        (define x 0)
        (define set-x! (lambda () (set! x 1)))))
    #t)
  (error? ; attempt to reference assigned hence unexported
    (let () (import ($l1-s)) (import m) x))
  (error? ; attempt to reference assigned hence unexported
    (let () (import ($l1-s)) (import m) (set! x 2)))
  (error? ; invalid version
    (let () (import-only (chezscheme csv7 (6))) record-field-mutator))
  (equal?
    (let () (import-only (chezscheme csv7)) record-field-mutator)
    csv7:record-field-mutator)

 ; test macros generating libraries
  (begin
    (let-syntax ([make-A (syntax-rules ()
                           [(_) (library (A)
                                  (export $library-x)
                                  (import (chezscheme))
                                  (define $library-x 3))])])
      (make-A))
    #t)
  (error? ; out-of-context library reference (A)
    (equal? (let () (import (A)) $library-x) 3))
  (begin
    (let-syntax ([make-A (lambda (x)
                           (syntax-case x ()
                             [(k) (with-implicit (k A)
                                    #'(library (A)
                                        (export $library-x)
                                        (import (chezscheme))
                                        (define $library-x 3)))]))])
      (make-A))
    #t)
  (error? ; unbound $library-x
    (equal? (let () (import (A)) $library-x) 3))
  (begin
    (let-syntax ([make-A (lambda (x)
                           (syntax-case x ()
                             [(k id ...)
                              (with-implicit (k A)
                                #'(library (A)
                                    (export id ...)
                                    (import (chezscheme))
                                    (define id 3)
                                    ...))]))])
      (make-A $library-x))
    #t)
  (eqv? (let () (import (A)) $library-x) 3)
  (let-syntax ([make-A (syntax-rules ()
                         [(_) (begin
                                (library (A)
                                  (export x)
                                  (import (chezscheme))
                                  (define x 3))
                                (let () (import (A))
                                  (eqv? x 3)))])])
    (make-A))
  (let-syntax ([make-A (syntax-rules ()
                         [(_) (begin
                                (library (A)
                                  (export x)
                                  (import (chezscheme))
                                  (define x 3))
                                (define-syntax q
                                  (syntax-rules ()
                                    [(_) (let ()
                                           (import (A))
                                           x)]))
                                (eqv? (q) 3))])])
    (make-A))

  (begin
    (with-output-to-file "testfile-a14.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a14) (export f) (import (chezscheme))
             (define f (lambda (n) (if (fx= n 0) 1 (fx* n (f (fx- n 1))))))
             (printf "invoked a\n"))))
      'replace)
    (with-output-to-file "testfile-b14.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b14) (export g) (import (chezscheme) (testfile-a14))
             (define g (lambda (n) (f n)))
             (printf "invoked b\n"))))
      'replace)
    (with-output-to-file "testfile-c14.ss"
      (lambda ()
        (pretty-print '(import (chezscheme) (testfile-b14)))
        (pretty-print '(pretty-print (g 10))))
      'replace)
    #t)
  (equal?
    (with-output-to-string
      (lambda () (load "testfile-c14.ss")))
    "invoked a\ninvoked b\n3628800\n")
  ; test for proper propagation and non-propagation of constants across library boundaries
  (begin
    (with-output-to-file "testfile-a15.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a15) (export a b c d e f g fa fb fc fd fe ff fg)
             (import (chezscheme))
             (define-record-type foo (nongenerative) (fields x))
             (define a '())
             (define b 'sym)
             (define c 3/4)
             (define d '(x . y))
             (define e (record-type-descriptor foo))
             (define f (make-foo 3))
             (define g "hello!")
             (define fa (lambda () a))
             (define fb (lambda () b))
             (define fc (lambda () c))
             (define fd (lambda () d))
             (define fe (lambda () e))
             (define ff (lambda () f))
             (define fg (lambda () g)))))
      'replace)
    (with-output-to-file "testfile-b15.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b15) (export a b c d e f g fa fb fc fd fe ff fg)
             (import (chezscheme) (prefix (testfile-a15) %))
             (define a %a)
             (define b %b)
             (define c %c)
             (define d %d)
             (define e %e)
             (define f %f)
             (define g %g)
             (define fa (lambda () (%fa)))
             (define fb (lambda () (%fb)))
             (define fc (lambda () (%fc)))
             (define fd (lambda () (%fd)))
             (define fe (lambda () (%fe)))
             (define ff (lambda () (%ff)))
             (define fg (lambda () (%fg))))))
      'replace)
    (with-output-to-file "testfile-c15.ss"
      (lambda ()
        (pretty-print '(define $c15-ls1
                         (let ()
                           (import (testfile-a15))
                           (list a b c d e f g (fa) (fb) (fc) (fd) (fe) (ff) (fg)))))
        (pretty-print '(define $c15-ls2
                         (let ()
                           (import (testfile-b15))
                           (list a b c d e f g (fa) (fb) (fc) (fd) (fe) (ff) (fg)))))
        (pretty-print '(pretty-print (map eq? $c15-ls1 $c15-ls2)))
        (pretty-print '(pretty-print (map eqv? $c15-ls1 $c15-ls2)))
        (pretty-print '(pretty-print (map equal? $c15-ls1 $c15-ls2))))
      'replace)
    (for-each separate-compile '(a15 b15 c15))
    #t)
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda () (load "testfile-c15.so")))
    '("(#t #t #f #t #t #t #t #t #t #f #t #t #t #t)\n(#t #t #t #t #t #t #t #t #t #t #t #t #t #t)\n(#t #t #t #t #t #t #t #t #t #t #t #t #t #t)\n"
      "(#t #t #t #t #t #t #t #t #t #t #t #t #t #t)\n(#t #t #t #t #t #t #t #t #t #t #t #t #t #t)\n(#t #t #t #t #t #t #t #t #t #t #t #t #t #t)\n"))
  (begin
    (library ($l3) (export f) (import (chezscheme)) (define (f x) x))
    #t)
  (equal?
    (let () (import ($l3)) (f (f 3)))
    3)
  (begin
    ;; (export import-spec ...) empty case
    (library ($empty) (export) (import (chezscheme)) (export (import)))
    #t)
  (begin
    (library ($l4-A) (export a) (import (chezscheme)) (define a 1))
    (library ($l4-B) (export b) (import (chezscheme)) (define b 2))
    #t)
  (equal? '(1 2) (let () (import ($l4-A) ($l4-B)) (list a b)))
  (begin
    ;; (export import-spec ...) multiple imports case
    (library ($l4-C) (export) (import (chezscheme)) (export (import ($l4-A) ($l4-B))))
    (equal? '(1 2) (let () (import ($l4-C)) (list a b))))
 )

(mat library2
  ; test to make sure that libraries needed by the transformers of local
  ; macros are invoked immediately and not required as run-time requirements.
  (begin
    (with-output-to-file "testfile-a3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a3) (export q) (import (rnrs) (only (scheme) putprop)) (define q 3) (putprop 'testfile-a3 'invoke #t))))
      'replace)
    (with-output-to-file "testfile-b3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b3) (export x) (import (testfile-a3) (rnrs) (only (scheme) putprop))
             (define x (let () (define-syntax p (lambda (x) (putprop 'testfile-b3 'visit #t) q)) p)))))
      'replace)
    (for-each separate-compile '(a3 b3))
    #t)
  (equal?
    (let ()
      (import (testfile-b3))
      (list x (getprop 'testfile-a3 'invoke #f) (getprop 'testfile-b3 'visit #f)))
    '(3 #f #f))
  (begin
    (with-output-to-file "testfile-a4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a4) (export q) (import (rnrs) (only (scheme) putprop))
             (define q (lambda (x) (if (= x 0) 1 (* x (q (- x 1))))))
             (putprop 'testfile-a4 'invoke #t))))
      'replace)
    (with-output-to-file "testfile-b4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b4) (export x) (import (testfile-a4) (rnrs) (only (scheme) putprop))
             (define x (let () (define-syntax p (lambda (x) (putprop 'testfile-b4 'visit #t) (q 3))) (list p (q 4)))))))
      'replace)
    (for-each separate-compile '(a4 b4))
    #t)
  (equal?
    (let ()
      (import (testfile-b4))
      (list x (getprop 'testfile-a4 'invoke #f) (getprop 'testfile-b4 'visit #f)))
    '((6 24) #t #f))
  (begin
    (with-output-to-file "testfile-a5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a5) (export q) (import (rnrs) (only (scheme) putprop)) (define q 3) (putprop 'testfile-a5 'invoke #t))))
      'replace)
    (with-output-to-file "testfile-b5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b5) (export x) (import (testfile-a5) (rnrs) (only (scheme) putprop))
             (define x (let-syntax ([p (lambda (x) (putprop 'testfile-b5 'visit #t) q)]) p)))))
      'replace)
    (for-each separate-compile '(a5 b5))
    #t)
  (equal?
    (let ()
      (import (testfile-b5))
      (list x (getprop 'testfile-a5 'invoke #f) (getprop 'testfile-b5 'visit #f)))
    '(3 #f #f))
  (begin
    (with-output-to-file "testfile-a6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a6) (export q) (import (rnrs) (only (scheme) putprop)) (define q 3) (putprop 'testfile-a6 'invoke #t))))
      'replace)
    (with-output-to-file "testfile-b6.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b6) (export x) (import (testfile-a6) (rnrs) (only (scheme) putprop))
             (let-syntax ([p (lambda (x) (putprop 'testfile-b6 'visit #t) q)]) (define x p)))))
      'replace)
    (for-each separate-compile '(a6 b6))
    #t)
  (equal?
    (let ()
      (import (testfile-b6))
      (list x (getprop 'testfile-a6 'invoke #f) (getprop 'testfile-b6 'visit #f)))
    '(3 #f #f))

 ; test cyclic dependency check
 ; this mat and next four are connected
  (begin
    (with-output-to-file "testfile-a7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a7) (export x) (import (rnrs) (testfile-b7)) (define x y))))
      'replace)
    (with-output-to-file "testfile-b7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b7) (export y) (import (rnrs) (testfile-a7)) (define y x))))
      'replace)
    #t)
  (error? ; possible cyclic dependency
    (let () (import (testfile-a7) (testfile-b7)) (list x y)))
  (error? ; possible cyclic dependency
    (let () (import (testfile-b7) (testfile-a7)) (list x y)))
 ; make sure errors didn't leave libraries in a state where they can't be redefined
  (begin
    (with-output-to-file "testfile-a7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a7) (export x) (import (rnrs) (testfile-b7)) (define x y))))
      'replace)
    (with-output-to-file "testfile-b7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b7) (export y) (import (rnrs)) (define y 17))))
      'replace)
    #t)
  (equal?
    (let () (import (testfile-a7) (testfile-b7)) (list x y))
    '(17 17))

 ; import cycles
  (error? ; cyclic dependency on import
    (library ($l2-lib1) (export) (import ($l2-lib1))))
  (begin ; make sure we can redefine after cyclic import error
    (library ($l2-lib1) (export a) (import (rnrs)) (define a "a"))
    #t)
  (equal? (let () (import ($l2-lib1)) a) "a")

  (begin
    (delete-file "testfile-a8.so")
    (with-output-to-file "testfile-a8.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a8) (export a) (import (testfile-a8)))))
      'replace)
     #t)
  (error? ; cyclic dependency on import
    (import (testfile-a8)))
  (begin ; make sure we can redefine after cyclic import error
    (with-output-to-file "testfile-a8.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a8) (export cons) (import (rnrs)))))
      'replace)
    #t)
  (equal? (let () (import (testfile-a8)) cons) (let () (import (rnrs)) cons))

  (begin
    (delete-file "testfile.a9.so")
    (with-output-to-file "testfile-a9.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a9) (export a) (import (testfile-a9)))))
      'replace)
    #t)
  (error? ; cyclic dependency on import
    (compile-file "testfile-a9"))
  (begin ; make sure we can redefine after cyclic import error
    (with-output-to-file "testfile-a9.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a9) (export cons) (import (rnrs)))))
      'replace)
    (compile-file "testfile-a9")
    (load "testfile-a9.so")
    #t)
  (equal? (let () (import (testfile-a9)) cons) (let () (import (rnrs)) cons))

  (begin
    (delete-file "testfile-a10.so")
    (delete-file "testfile-b10.so")
    (with-output-to-file "testfile-a10.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a10) (export a) (import (testfile-b10)))))
      'replace)
    (with-output-to-file "testfile-b10.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b10) (export a) (import (testfile-a10)))))
      'replace)
    #t)
  (error? ; cyclic dependency on import (indirect)
    (import (testfile-a10)))
  (begin ; make sure we can redefine after cyclic import error
    (with-output-to-file "testfile-a10.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a10) (export a) (import (testfile-b10)))))
      'replace)
    (with-output-to-file "testfile-b10.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b10) (export a) (import (rnrs)) (define a "eh?"))))
      'replace)
    #t)
  (equal? (let () (import (testfile-a10)) a) "eh?")

 ; invoke cycles
  (begin
    (library ($l2-lib2) (export a)
      (import (rnrs) (rnrs eval))
      (define a (eval 'a (environment '($l2-lib2)))))
    #t)
  (error? ; cyclic dependency on invoke
    (let () (import ($l2-lib2)) a))

  (begin
    (delete-file "testfile-a11.so")
    (delete-file "testfile-b11.so")
    (with-output-to-file "testfile-a11.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a11) (export a) (import (testfile-b11)))))
      'replace)
    (with-output-to-file "testfile-b11.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-b11) (export a)
             (import (rnrs) (rnrs eval))
             (define a (eval 'a (environment '(testfile-a11)))))))
      'replace)
    #t)
  (error? ; cyclic dependency on invoke (indirect)
    (let () (import (testfile-a11)) a))

 ; visit cycles
  (begin
    (delete-file "testfile-a12.so")
    (remprop 'chewie 'ratface)
    (with-output-to-file "testfile-a12.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a12) (export a)
             (import (rnrs) (rnrs eval) (only (scheme) getprop))
             (define-syntax a
               (if (getprop 'chewie 'ratface #f)
                   (eval 'a (environment '(testfile-a12)))
                   (lambda (x) 3))))))
      'replace)
    (separate-compile 'a12)
    (putprop 'chewie 'ratface #t)
    #t)
  (error? ; cyclic dependency on visit
    (let () (import (testfile-a12)) a))
  (begin
    (with-output-to-file "testfile-a13.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a13) (export a)
             (import (rename (rnrs) (cons a))))))
      'replace)
    (separate-compile 'a13)
    #t)
  (equal? (let () (import (testfile-a13)) (a 3 4)) '(3 . 4))
  (error? (library (foo) (export a (rename b a)) (import (rnrs)) (define a 3) (define b 4)))
  (error? (library (foo) (export a (rename (b a))) (import (rnrs)) (define a 3) (define b 4)))
  (error? (library (foo) (exports a) (import (rnrs)) (define a 3)))
  (error? (library (foo) (export a) (imports (rnrs)) (define a 3)))

  (error? ; misplaced library form
    (let ()
      (library (foo)
        (export)
        (import (scheme))
        (library (bar) (export) (import)))))
  (error? ; misplaced library form
    (let () (library (foo) (export) (import))))
  (error? ; misplaced library form
    (+ (library (bar) (export) (import)) 3))

 ; make sure library is visited when needed
  (begin
    (with-output-to-file "testfile-f2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-f2) (export f2-x) (import (rnrs) (rnrs mutable-pairs))
             (define-syntax define-mutable
               (syntax-rules ()
                 [(_ x e)
                  (begin
                    (define t (list e))
                    (define-syntax x
                      (identifier-syntax
                        [_ (car t)]
                        [(set! _ new) (set-car! t new)])))]))
             (define-mutable f2-x 772))))
      'replace)
    (for-each separate-compile '(f2))
    #t)
  (begin
    (define (f2-x-whack! v)
      (import (testfile-f2))
      (set! f2-x v))
    (f2-x-whack! 29)
    #t)
  (eqv? (let () (import (testfile-f2)) f2-x) 29)
  (not (top-level-bound? 'f2-x))
 ; make sure #'x doesn't force library to be visited if x is an exported
 ; keyword or invoked if x is an exported variable
  (begin
    (with-output-to-file "testfile-g2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-g2) (export hit-a hit-x) (import (chezscheme))
             (define hit-a (make-parameter #f))
             (define hit-x (make-parameter #f)))))
      'replace)
    (with-output-to-file "testfile-h2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-h2) (export x a) (import (rnrs) (testfile-g2))
             (define-syntax a (begin (hit-a #t) (lambda (x) 73)))
             (define x (begin (hit-x #t) (list (hit-x) 97))))))
      'replace)
    (for-each separate-compile '(g2 h2))
    #t)
  (let () (import (testfile-g2)) (and (not (hit-a)) (not (hit-x))))
  (let () (import (testfile-g2) (testfile-h2)) (let ([q #'a]) (and (identifier? q) (not (hit-a)) (not (hit-x)))))
  (let () (import (testfile-g2) (testfile-h2)) (let ([q #'x]) (and (identifier? q) (not (hit-a)) (not (hit-x)))))
  (let () (import (testfile-g2) (testfile-h2)) (and (eqv? a 73) (hit-a) (not (hit-x))))
  (let () (import (testfile-g2) (testfile-h2)) (and (equal? x '(#t 97)) (hit-a) (hit-x)))
)

(mat library3
 ; test several-deep invoke-dependency chain
  (begin
    (with-output-to-file "testfile-a3-0.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a3-0)
             (export x0)
             (import (rnrs))
             (define x0 7))))
      'replace)
    (with-output-to-file "testfile-a3-1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a3-1)
             (export x1)
             (import (rnrs) (testfile-a3-0))
             (define x1 (+ x0 1)))))
      'replace)
    (with-output-to-file "testfile-a3-2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a3-2)
             (export x2)
             (import (rnrs) (testfile-a3-1))
             (define x2 (+ x1 2)))))
      'replace)
    (with-output-to-file "testfile-a3-3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a3-3)
             (export x3)
             (import (rnrs) (testfile-a3-2))
             (define x3 (+ x2 3)))))
      'replace)
    (with-output-to-file "testfile-a3-4.ss"
      (lambda ()
        (pretty-print '(import (rnrs) (testfile-a3-3)))
        (pretty-print '(write (+ x3 4))))
      'replace)
    (separate-compile 'compile-library 'a3-0)
    (separate-compile 'compile-library 'a3-1)
    (separate-compile 'compile-library 'a3-2)
    (separate-compile 'compile-library 'a3-3)
    (separate-compile 'compile-program 'a3-4)
    #t)
  (equal?
    (with-output-to-string
      (lambda () (load-program "testfile-a3-4.so")))
    "17")
  (eqv? (let () (import (testfile-a3-3)) x3) 13)
 ; try begin containing library and top-level program
  (begin
    (with-output-to-file "testfile-a3-5.ss"
      (lambda ()
        (pretty-print
          '(begin
             (library (a3-5 foo)
               (export x)
               (import (rnrs))
               (define x "hello"))
             (top-level-program
               (import (rnrs) (a3-5 foo))
               (display x)))))
      'replace)
    (separate-compile 'a3-5)
    #t)
  (equal?
    (with-output-to-string
      (lambda () (load "testfile-a3-5.so")))
    "hello")
  (equal?
    (with-output-to-string
      (lambda () (load "testfile-a3-5.ss")))
    "hello")
 ; try begin containing two libraries
  (begin
    (with-output-to-file "testfile-a3-6.ss"
      (lambda ()
        (pretty-print
          '(begin
             (library (a3-6 foo)
               (export a x)
               (import (rnrs))
               (define-syntax a (identifier-syntax "boo"))
               (define x "hello"))
             (library (a3-6 bar)
               (export y)
               (import (rnrs) (a3-6 foo))
               (define y (cons a x)))
             (let () (import (a3-6 bar)) (write y)))))
      'replace)
    (separate-compile 'a3-6)
    #t)
  (equal?
    (with-output-to-string
      (lambda () (load "testfile-a3-6.so")))
    "(\"boo\" . \"hello\")")
  (equal?
    (let ()
      (import (a3-6 bar))
      y)
    '("boo" . "hello"))
  (equal?
    (let ()
      (import (a3-6 foo))
      (cons x a))
    '("hello" . "boo"))
 ; import a library in subset-mode system, then outside of subset-mode system
  (begin
    (with-output-to-file "testfile-a3-7.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a3-7)
             (export x)
             (import (rnrs))
             (define x "hello"))))
      'replace)
    #t)
  (equal?
    (parameterize ([subset-mode 'system]) (eval '(let () (import (testfile-a3-7)) x)))
    "hello")
  (equal?
    (let () (import (testfile-a3-7)) x)
    "hello")

  (begin
    (with-output-to-file "testfile-a3-8.ss"
      (lambda ()
        (pretty-print '(printf "outside (testfile-a3-8)\n"))
        (pretty-print
          '(library (testfile-a3-8)
             (export a3-8-x)
             (import (rnrs))
             (define a3-8-x 5)
             (error #f "library should not be invoked"))))
      'replace)
    (with-output-to-file "testfile-a3-9.ss"
      (lambda ()
        (pretty-print
          '(let ()
             (import (scheme) (testfile-a3-8))
             (printf "inside testfile-a3-9\n"))))
      'replace)
    (with-output-to-file "testfile-a3-10.ss"
      (lambda ()
        (pretty-print '(import (scheme) (testfile-a3-8)))
        (pretty-print '(printf "inside testfile-a3-10\n")))
      'replace)
    (separate-compile 'a3-8)
    (separate-compile 'a3-9)
    (separate-compile 'a3-10)
    #t)
  (equal?
    (with-output-to-string (lambda () (load "testfile-a3-9.so")))
    "inside testfile-a3-9\n")
  (equal?
    (with-output-to-string (lambda () (load "testfile-a3-10.so")))
    "inside testfile-a3-10\n")
)

(mat library4
  ; test reloading of libraries if dependencies have changed
  ; when compile-imported-libraries is true.
  ; first test with compile-imported-libraries true:
  (begin
    (define ($reset-l4-1)
      (for-each delete-file '("testfile-l4-a1.so" "testfile-l4-b1.so" "testfile-l4-c1.so"))
      (with-output-to-file "testfile-l4-a1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-a1) (export x) (import (chezscheme) (testfile-l4-b1) (testfile-l4-c1))
               (include "testfile-l4-d1.ss")
               (define a 'a-object)
               (define x (list a b c d)))))
        'replace)
      (with-output-to-file "testfile-l4-b1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-b1) (export b) (import (chezscheme))
               (define b (list 'b-object)))))
        'replace)
      (with-output-to-file "testfile-l4-c1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-c1) (export c) (import (chezscheme))
               (define-syntax c (lambda (x) #''c-object)))))
        'replace)
      (with-output-to-file "testfile-l4-d1.ss"
        (lambda ()
          (pretty-print
            '(define-syntax d (lambda (x) #''d-object))))
        'replace)
      (with-output-to-file "testfile-l4-p1.ss"
        (lambda ()
          (pretty-print
            '(import (testfile-l4-a1) (chezscheme)))
          (pretty-print
            '(pretty-print x)))
        'replace)
      (let ([s (separate-eval
                 '(compile-imported-libraries #t)
                 '(compile-file-message #f)
                 '(load-program "testfile-l4-p1.ss"))])
        (unless (equal? s "(a-object (b-object) c-object d-object)\n")
          (errorf #f "unexpected separate-eval return value ~s" s)))
      ; ensure different file times for followup updates
      (sleep (make-time 'time-duration 0 (if (embedded?) 3 1)))
      #t)
    #t)
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-a1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-a1) (export x) (import (chezscheme) (testfile-l4-b1) (testfile-l4-c1))
               (include "testfile-l4-d1.ss")
               (define a 'newa-object)
               (define x (list a b c d)))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #t)
        '(compile-file-message #f)
        '(load-program "testfile-l4-p1.ss")))
    "(newa-object (b-object) c-object d-object)\n")
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-b1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-b1) (export b) (import (chezscheme))
               (define b (list 'newb-object)))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #t)
        '(compile-file-message #f)
        '(load-program "testfile-l4-p1.ss")))
    "(a-object (newb-object) c-object d-object)\n")
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-c1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-c1) (export c) (import (chezscheme))
               (define-syntax c (lambda (x) #''newc-object)))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #t)
        '(compile-file-message #f)
        '(load-program "testfile-l4-p1.ss")))
    "(a-object (b-object) newc-object d-object)\n")
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-d1.ss"
        (lambda ()
          (pretty-print
            '(define-syntax d (lambda (x) #''newd-object))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #t)
        '(compile-file-message #f)
        '(load-program "testfile-l4-p1.ss")))
    "(a-object (b-object) c-object newd-object)\n")
  ; now with compile-imported-libraries false
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-a1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-a1) (export x) (import (chezscheme) (testfile-l4-b1) (testfile-l4-c1))
               (include "testfile-l4-d1.ss")
               (define a 'newera-object)
               (define x (list a b c d)))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #f)
        '(compile-file-message #t)
        '(load-program "testfile-l4-p1.ss")))
    "(newera-object (b-object) c-object d-object)\n")
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-b1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-b1) (export b) (import (chezscheme))
               (define b (list 'newerb-object)))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #f)
        '(compile-file-message #t)
        '(load-program "testfile-l4-p1.ss")))
    "(a-object (newerb-object) c-object d-object)\n")
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-c1.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-l4-c1) (export c) (import (chezscheme))
               (define-syntax c (lambda (x) #''newerc-object)))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #f)
        '(compile-file-message #t)
        '(load-program "testfile-l4-p1.ss")))
    "(a-object (b-object) newerc-object d-object)\n")
  ($reset-l4-1)
  (equal?
    (begin
      (with-output-to-file "testfile-l4-d1.ss"
        (lambda ()
          (pretty-print
            '(define-syntax d (lambda (x) #''newerd-object))))
        'replace)
      (separate-eval
        '(compile-imported-libraries #f)
        '(compile-file-message #t)
        '(load-program "testfile-l4-p1.ss")))
    "(a-object (b-object) c-object newerd-object)\n")
)

(mat library5
  ; test for proper runtime library dependencies
  (begin
    (with-output-to-file "testfile-l5-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-l5-a1) (export a) (import (chezscheme))
             (define a (cons 3 4)))))
      'replace)
    (with-output-to-file "testfile-l5-b1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-l5-b1) (export a b c) (import (chezscheme) (testfile-l5-a1))
             (define-syntax b (identifier-syntax (vector a)))
             (define c (cons 5 6)))))
      'replace)
    (with-output-to-file "testfile-l5-c1.ss"
      (lambda ()
        (for-each pretty-print
          `((import (chezscheme) (testfile-l5-b1))
            (set-car! a 55)
            (pretty-print (list a b)))))
      'replace)
    (equal?
      (parameterize ([compile-imported-libraries #t])
        (compile-program "testfile-l5-c1"))
      '((testfile-l5-a1))))
  ; delete testfile-l5-b1.{ss,so} to make sure they aren't surreptitiously loaded
  (begin
    (delete-file "testfile-l5-b1.ss")
    (delete-file "testfile-l5-b1.so")
    (and (not (file-exists? "testfile-l5-b1.ss"))
         (not (file-exists? "testfile-l5-b1.so"))))
  (equal?
    (separate-eval '(load-program "testfile-l5-c1.so"))
    "((55 . 4) #((55 . 4)))\n")
)

(mat library6
  ; test for proper handling of visit library dependencies
  (begin
    (with-output-to-file "testfile-l6-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-l6-a1) (export a) (import (chezscheme))
             (define a (cons 3 4)))))
      'replace)
    (with-output-to-file "testfile-l6-b1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-l6-b1) (export b-x b-y) (import (chezscheme) (testfile-l6-a1))
             (define-syntax b-x (lambda (x) (car a)))
             (define b-y (cons 5 6)))))
      'replace)
    (with-output-to-file "testfile-l6-c1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-l6-c1) (export c) (import (chezscheme) (testfile-l6-b1))
             (meta define c
               (lambda (x)
                 #`(cons (* #,x #,(car b-y)) (* #,x #,(cdr b-y))))))))
      'replace)
    (with-output-to-file "testfile-l6-prog1.ss"
      (lambda ()
        (pretty-print '(eval-when (visit) (printf "visiting testfile-l6-prog1\n")))
        (pretty-print '(define-syntax M
                         (lambda (x)
                           (import (testfile-l6-c1))
                           (syntax-case x ()
                             [(_ f d) #`(f #,(c (datum d)))]))))
        (pretty-print '(eval-when (revisit) (printf "revisiting testfile-l6-prog1\n")))
        (pretty-print '(pretty-print (M vector 2))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t])
           (compile-file x)))
      "testfile-l6-prog1")
    #t)

  (begin
    (delete-file "testfile-l6-a1.so")
    (delete-file "testfile-l6-a1.ss")
    (and (not (file-exists? "testfile-l6-a1.so"))
         (not (file-exists? "testfile-l6-a1.ss"))))

  (equal?
    (separate-eval '(revisit "testfile-l6-prog1.so"))
    "revisiting testfile-l6-prog1\n#((10 . 12))\n")
)

(mat library7
  (begin
    (mkfile "testfile-l7-a1.ss"
      '(library (testfile-l7-a1) (export a-macro a) (import (chezscheme)) (define-syntax a-macro (identifier-syntax 'aaa)) (define (a x) (+ x (* x x)))))
    (mkfile "testfile-l7-b1.ss"
      '(library (testfile-l7-b1) (export b) (import (chezscheme) (testfile-l7-a1)) (define (b x) (cons 'b a-macro))))
    (mkfile "testfile-l7-c1.ss"
      '(library (testfile-l7-c1) (export c) (import (chezscheme) (testfile-l7-a1)) (define (c x) (cons 'c (a x)))))
    (mkfile "testfile-l7-d1.ss"
      '(library (testfile-l7-d1) (export d) (import (chezscheme) (testfile-l7-a1)) (define (d x) (list 'd a-macro (a x)))))
    (separate-compile
      '(lambda (x) (for-each compile-library x))
      '(list "testfile-l7-a1" "testfile-l7-b1" "testfile-l7-c1" "testfile-l7-d1"))
    #t)
  (equal?
    (separate-eval
      '(let () (import (testfile-l7-b1)) (b 7))
      '(let () (import (testfile-l7-c1)) (c 7))
      '(let () (import (testfile-l7-d1)) (d 7)))
    "(b . aaa)\n(c . 56)\n(d aaa 56)\n")
  (begin
    (separate-compile
      '(lambda (x) (for-each compile-library x))
      '(list "testfile-l7-a1" "testfile-l7-b1" "testfile-l7-c1"))
    #t)
  (equal?
    (separate-eval
      '(let () (import (testfile-l7-b1)) (b 7))
      '(let () (import (testfile-l7-c1)) (c 7))
      ; this should reload from source, since dependency is out-of-date
      '(let () (import (testfile-l7-d1)) (d 7)))
    "(b . aaa)\n(c . 56)\n(d aaa 56)\n")
  (equal?
    (separate-eval
      ; this should reload from source, since dependency is out-of-date
      '(let () (import (testfile-l7-d1)) (d 7))
      '(let () (import (testfile-l7-c1)) (c 7))
      '(let () (import (testfile-l7-b1)) (b 7)))
    "(d aaa 56)\n(c . 56)\n(b . aaa)\n")
  (error? ; expected different compilation instance
    (separate-eval
      '(let () (import (testfile-l7-b1)) (b 7))
      '(let () (import (testfile-l7-c1)) (c 7))
      '(load-library "testfile-l7-d1.so")
      '(let () (import (testfile-l7-d1)) (d 7))))
  (error? ; expected different compilation instance
    (separate-eval
      '(load-library "testfile-l7-d1.so")
      '(let () (import (testfile-l7-d1)) (d 7))))
  (equal?
    (separate-eval
      '(load-library "testfile-l7-b1.ss")
      '(let () (import (testfile-l7-b1)) (b 7))
      ; this should reload from source, since dependency is out-of-date
      '(let () (import (testfile-l7-c1)) (c 7))
      ; this should reload from source, since dependency is out-of-date
      '(let () (import (testfile-l7-d1)) (d 7)))
    "(b . aaa)\n(c . 56)\n(d aaa 56)\n")
  (error? ; expected different compilation instance
    (separate-eval
      '(load-library "testfile-l7-b1.ss")
      '(load-library "testfile-l7-c1.ss")
      '(load-library "testfile-l7-d1.so")
      '(let () (import (testfile-l7-d1)) (d 7))))
  (begin
    (delete-file "testfile-l7-a1.so")
    #t)
  (equal?
    (separate-eval
      '(parameterize ([compile-imported-libraries #t]) (compile-library "testfile-l7-b1.ss"))
      '(let () (import (testfile-l7-b1)) (b 7))
      ; this should reload from source, since dependency is out-of-date
      '(let () (import (testfile-l7-c1)) (c 7))
      '(let () (import (testfile-l7-d1)) (d 7)))
    "compiling testfile-l7-b1.ss with output to testfile-l7-b1.so\ncompiling testfile-l7-a1.ss with output to testfile-l7-a1.so\n(b . aaa)\n(c . 56)\n(d aaa 56)\n")
  (begin
    (delete-file "testfile-l7-a1.so")
    #t)
  (error? ; expected different compilation instance
    (separate-eval
      '(parameterize ([compile-imported-libraries #t]) (compile-library "testfile-l7-b1.ss"))
      '(load-library "testfile-l7-c1.so")
      '(let () (import (testfile-l7-c1)) (c 7))))
  (equal?
    (separate-eval
      '(library (testfile-l7-a1) (export a-macro a) (import (chezscheme)) (define-syntax a-macro (identifier-syntax 'aaa2)) (define (a x) (* x 11)))
      '(let () (import (testfile-l7-b1)) (b 7))
      '(let () (import (testfile-l7-c1)) (c 7))
      '(let () (import (testfile-l7-d1)) (d 7)))
    "(b . aaa2)\n(c . 77)\n(d aaa2 77)\n")
  (error? ; expected different compilation instance
    (separate-eval
      '(library (testfile-l7-a1) (export a-macro a) (import (chezscheme)) (define-syntax a-macro (identifier-syntax 'aaa2)) (define (a x) (* x 11)))
      '(let () (import (testfile-l7-b1)) (b 7))
      '(let () (import (testfile-l7-c1)) (c 7))
      '(load-library "testfile-l7-d1.so")
      '(let () (import (testfile-l7-d1)) (d 7))))
)

(mat library-regression
  ; test that failing invoke code does not result in cyclic dependency problem on re-run
  (equal?
    (separate-eval
      '(begin
         (library (invoke-fail)
           (export x)
           (import (chezscheme))
           (define x #f)
           (error #f "failed to load library (invoke-fail)"))
         (guard (e [else
                     (guard (e2 [else
                                  (display-condition e) (newline)
                                  (display-condition e2) (newline)])
                       (eval 'x (environment '(chezscheme) '(invoke-fail))))])
           (eval 'x (environment '(chezscheme) '(invoke-fail))))))
    "Exception: failed to load library (invoke-fail)\nException: failed to load library (invoke-fail)\n")

  ; test that true cyclic dependency will always report the same thing
  (equal?
    (separate-eval
      '(begin
         (library (invoke-cyclic)
           (export x y)
           (import (chezscheme))
           (define x #f)
           (define y (eval '(if x 5 10) (environment '(chezscheme) '(invoke-cyclic)))))
         (guard (e [else
                     (guard (e2 [else
                                  (display-condition e) (newline)
                                  (display-condition e2) (newline)])
                       (eval 'x (environment '(chezscheme) '(invoke-cyclic))))])
           (eval 'x  (environment '(chezscheme) '(invoke-cyclic))))))
    "Exception: cyclic dependency involving invocation of library (invoke-cyclic)\nException: cyclic dependency involving invocation of library (invoke-cyclic)\n")

  (begin
    ; library to help make it easier to cause a failure in the visit-code that
    ; does not lead to failure during compilation of the file.
    (with-output-to-file "testfile-lr-l1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-lr-l1)
             (export make-it-fail)
             (import (chezscheme))
             (define make-it-fail (make-parameter #f (lambda (x) (and x #t)))))))
      'replace)
    ; simple test to define one macro and potentially to raise an error when
    ; defining the second one.
    (with-output-to-file "testfile-lr-l2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-lr-l2)
             (export M1 M2)
             (import (chezscheme) (testfile-lr-l1))
             (define-syntax M1
               (identifier-syntax #f))

             (define-syntax M2
               (if (make-it-fail)
                   (error 'M2 "user requested failure with (make-it-fail) parameter")
                   (lambda (x)
                     (syntax-case x ()
                       [(_ expr) #'expr])))))))
      'replace)
    ; more complete test that attempts to create the various types of things
    ; that can be defined in visit code so that we can verify things are being
    ; properly reset.
    (with-output-to-file "testfile-lr-l3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-lr-l3)
             (export a b c d e f g h)
             (import (chezscheme) (testfile-lr-l1))

             (module a (x) (define x 5))
             (alias b cons)
             (define-syntax c (make-compile-time-value 5))
             (define d 5)
             (meta define e 5)
             (define-syntax f (identifier-syntax #f))
             (define $g (make-parameter #f))
             (define-syntax g
               (make-variable-transformer
                 (lambda (x)
                   (syntax-case x ()
                     [(set! _ v) #'($g v)]
                     [_ #'($g)]
                     [(_ e* ...) #'(($g) e* ...)]))))
             (define-property f g 10)
             (define-syntax h
               (if (make-it-fail)
                   (error 'h "user requested failure with (make-it-fail) parameter")
                   (lambda (x)
                     (syntax-case x ()
                       [(_ expr) #'expr])))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t])
           (for-each compile-library x)))
      '(list "testfile-lr-l1" "testfile-lr-l2" "testfile-lr-l3"))
    #t)

  (equal?
    (separate-eval
      '(begin
         (import (testfile-lr-l2) (testfile-lr-l1))
         (make-it-fail #t)
         (guard (e [else
                     (guard (e2
                              [else
                                (display-condition e) (newline)
                                (display-condition e2) (newline)])
                       (eval 'M1 (environment '(testfile-lr-l2))))])
           (eval 'M1 (environment '(testfile-lr-l2))))))
    "Exception in M2: user requested failure with (make-it-fail) parameter\nException in M2: user requested failure with (make-it-fail) parameter\n")

  ; module is defined as part of import code, run time bindings are setup as part of invoke code
  (equal?
    (separate-eval
      '(begin
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         (import a)
         x))
    "5\n")

  ; alias is part of module binding ribcage, set up by import code
  (equal?
    (separate-eval
      '(begin
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         (b 'a 'b)))
    "(a . b)\n")

  ; compile-time-value is set in visit code, should show same error each time it is referenced
  (equal?
    (separate-eval
      '(begin
         (library (lookup)
           (export lookup)
           (import (chezscheme))
           (define-syntax lookup
             (lambda (x)
               (syntax-case x ()
                 [(_ id) (lambda (rho) #`'#,(rho #'id))]
                 [(_ id key) (lambda (rho) #`'#,(rho #'id #'key))]))))
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         (guard (e [else
                     (guard (e2
                              [else
                                (display-condition e) (newline)
                                (display-condition e2) (newline)])
                       (eval '(lookup c) (environment '(testfile-lr-l3) '(lookup))))])
           (eval '(lookup c) (environment '(testfile-lr-l3) '(lookup))))))
    "Exception in h: user requested failure with (make-it-fail) parameter\nException in h: user requested failure with (make-it-fail) parameter\n")

  ; defines are set up as part of invoke code
  (equal?
    (separate-eval
      '(begin
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         d))
    "5\n")

  ; meta defines are set up as part of visit code
  (equal?
    (separate-eval
      '(begin
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         (guard (e [else
                     (guard (e2
                              [else
                                (display-condition e) (newline)
                                (display-condition e2) (newline)])
                       (eval '(let ()
                                (define-syntax get-e
                                  (lambda (x)
                                    (syntax-case x ()
                                      [(_) #`'#,e])))
                                (get-e))
                         (environment '(chezscheme) '(testfile-lr-l3))))])
           (eval '(let ()
                    (define-syntax get-e
                      (lambda (x)
                        (syntax-case x ()
                          [(_) #`'#,e])))
                    (get-e))
             (environment '(chezscheme) '(testfile-lr-l3))))))
    "Exception in h: user requested failure with (make-it-fail) parameter\nException in h: user requested failure with (make-it-fail) parameter\n")

  ; macros are set up as part of visit code
  (equal?
    (separate-eval
      '(begin
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         (guard (e [else
                     (guard (e2
                              [else
                                (display-condition e) (newline)
                                (display-condition e2) (newline)])
                       (eval 'f (environment '(testfile-lr-l3))))])
           (eval 'f (environment '(testfile-lr-l3))))))
    "Exception in h: user requested failure with (make-it-fail) parameter\nException in h: user requested failure with (make-it-fail) parameter\n")

  ; variable transformer macros are set up as part of visit code
  (equal?
    (separate-eval
      '(begin
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         (guard (e [else
                     (guard (e2
                              [else
                                (display-condition e) (newline)
                                (display-condition e2) (newline)])
                       (eval 'g (environment '(testfile-lr-l3))))])
           (eval 'g (environment '(testfile-lr-l3))))))
    "Exception in h: user requested failure with (make-it-fail) parameter\nException in h: user requested failure with (make-it-fail) parameter\n")

  ; properties are setup as part of visit code.
  (equal?
    (separate-eval
      '(begin
         (library (lookup)
           (export lookup)
           (import (chezscheme))
           (define-syntax lookup
             (lambda (x)
               (syntax-case x ()
                 [(_ id) (lambda (rho) #`'#,(rho #'id))]
                 [(_ id key) (lambda (rho) #`'#,(rho #'id #'key))]))))
         (import (testfile-lr-l3) (testfile-lr-l1))
         (make-it-fail #t)
         (guard (e [else
                     (guard (e2
                              [else
                                (display-condition e) (newline)
                                (display-condition e2) (newline)])
                       (eval '(lookup f g) (environment '(testfile-lr-l3) '(lookup))))])
           (eval '(lookup f g) (environment '(testfile-lr-l3) '(lookup))))))
    "Exception in h: user requested failure with (make-it-fail) parameter\nException in h: user requested failure with (make-it-fail) parameter\n")

  ;; re-arm import code if it complains about a library that is not visible
  (begin
    (with-output-to-file "testfile-lr-l4.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-lr-l4)
            (export x)
            (import (chezscheme))
            (define x 123))))
      'replace)
    (with-output-to-file "testfile-lr-p4.ss"
      (lambda ()
        (for-each pretty-print
         '((import (testfile-lr-l4) (scheme))
           (define (run args)
             (guard (c [#t (display-condition c) (newline)])
               (pretty-print (top-level-value (car args) (environment (cdr args))))))
           (when (> x 0) ;; reference export
             (let ([args (map string->symbol (command-line-arguments))])
               (if (= (length args) 2)
                   (begin
                     (run args)
                     (run args))
                   (error #f "expected 2 args")))))))
      'replace)
    (separate-eval
     '(parameterize ([compile-imported-libraries #t] [generate-wpo-files #t])
        (compile-program "testfile-lr-p4.ss")
        (compile-whole-program "testfile-lr-p4.wpo" "testfile-lr-p4-visible" #t)
        (compile-whole-program "testfile-lr-p4.wpo" "testfile-lr-p4-not-visible" #f)))
    (equal?
     (separate-eval
      '(parameterize ([command-line-arguments '("x" "testfile-lr-l4")])
         (load-program "testfile-lr-p4-visible")
         (load-program "testfile-lr-p4-not-visible")))
     (string-append
      "123\n"
      "123\n"
      "Exception in environment: attempt to import invisible library (testfile-lr-l4)\n"
      "Exception in environment: attempt to import invisible library (testfile-lr-l4)\n"))))

(mat invoke-library
  (error? ; invalid library reference
     (invoke-library '(testfile-il1 (<= 3))))
  (error? ; invalid library reference
     (invoke-library '(testfile-il1 (what?))))
  (error? ; invalid library reference
     (invoke-library '()))
  (error? ; invalid library reference
     (invoke-library 'hello))
  (error? ; invalid library reference
     (invoke-library '(3 2 1)))
  (begin
    (mkfile "testfile-il1.ss"
      '(library (testfile-il1 (2)) (export a) (import (chezscheme)) (define a 3) (printf "invoked (testfile-il1)\n")))
    #t)
  (equal?
    (separate-eval
      '(let () (import (testfile-il1)) a))
    "invoked (testfile-il1)\n3\n")
  (equal?
    (separate-eval
      '(invoke-library '(testfile-il1)))
    "invoked (testfile-il1)\n")
  (equal?
    (separate-eval
      '(invoke-library '(testfile-il1))
      '(printf "hello\n")
      '(let () (import (testfile-il1)) a))
    "invoked (testfile-il1)\nhello\n3\n")
  (equal?
    (separate-eval
      '(let () (import (testfile-il1)) a)
      '(printf "hello\n")
      '(invoke-library '(testfile-il1)))
    "invoked (testfile-il1)\n3\nhello\n")
  (begin
    (separate-eval '(compile-library "testfile-il1"))
    #t)
  (delete-file "testfile-il1.ss")
  (equal?
    (separate-eval
      '(let () (import (testfile-il1)) a))
    "invoked (testfile-il1)\n3\n")
  (equal?
    (separate-eval
      '(invoke-library '(testfile-il1)))
    "invoked (testfile-il1)\n")
  (equal?
    (separate-eval
      '(invoke-library '(testfile-il1))
      '(printf "hello\n")
      '(let () (import (testfile-il1)) a))
    "invoked (testfile-il1)\nhello\n3\n")
  (equal?
    (separate-eval
      '(let () (import (testfile-il1)) a)
      '(printf "hello\n")
      '(invoke-library '(testfile-il1)))
    "invoked (testfile-il1)\n3\nhello\n")
  (error? ; version mismatch
    (separate-eval '(invoke-library '(testfile-il1 (3)))))
  (error? ; version mismatch
    (separate-eval
      '(invoke-library '(testfile-il1 ((>= 3))))))
  (equal?
    (separate-eval
      '(invoke-library '(testfile-il1 ((>= 2)))))
    "invoked (testfile-il1)\n")
  (equal?
    (separate-eval
      '(invoke-library '(testfile-il1 (2))))
    "invoked (testfile-il1)\n")
)

(mat cross-library-optimization
  (begin
    (with-output-to-file "testfile-clo-1a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-clo-1a)
             (export f)
             (import (chezscheme))
             (define f (lambda (s) (format "~s!\n" s))))))
      'replace)
    (with-output-to-file "testfile-clo-1b.ss"
      (lambda ()
        (pretty-print
          '(import (chezscheme) (testfile-clo-1a)))
        (pretty-print
          '(display-string (f 'hello))))
      'replace)
    #t)
  (eqv? (compile-library "testfile-clo-1a") (void))
  ; in this case, can't propage f because of the embedded string constant,
  ; so program depends on library at run time
  (equal? (compile-program "testfile-clo-1b") '((testfile-clo-1a)))
  (equal?
    (with-output-to-string
      (lambda () (load-program "testfile-clo-1b.so")))
    "hello!\n")
  (begin
    (with-output-to-file "testfile-clo-2a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-clo-2a)
             (export f)
             (import (chezscheme))
             (define f (lambda (s) (symbol->string s))))))
      'replace)
    (with-output-to-file "testfile-clo-2b.ss"
      (lambda ()
        (pretty-print
          '(import (chezscheme) (testfile-clo-2a)))
        (pretty-print
          '(display-string (f 'hello))))
      'replace)
    #t)
  (eqv? (compile-library "testfile-clo-2a") (void))
  ; in this case, nothing stopping propagation of f,
  ; so program doesn't necessarily depend on library at run time
  (and (member
         (compile-program "testfile-clo-2b")
         '(() ((testfile-clo-2a))))
       #t)
  (equal?
    (with-output-to-string
      (lambda () (load-program "testfile-clo-2b.so")))
    "hello")
  ; testing internal consistency for library w/externally visible side effect, which we don't guarantee
  ; will happen if all runtime references are optimized away
  (begin
    (with-output-to-file "testfile-clo-3a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-clo-3a)
             (export g h)
             (import (chezscheme))
             (define (f) (putprop 'spam 'canned #t))
             (define (g) (getprop 'spam 'canned #f))
             (define (h) (remprop 'spam 'canned))
             (f))))
      'replace)
    (with-output-to-file "testfile-clo-3b.ss"
      (lambda ()
        (pretty-print
          '(import (chezscheme) (testfile-clo-3a)))
        (pretty-print
          '(write (g))))
      'replace)
    #t)
  (equal?
    (let ([libs (parameterize ([compile-imported-libraries #t]) (compile-program "testfile-clo-3b"))])
      (cond
        ; if compiled program depends on the library, the externally visible side effect (putprop) will be done
        [(equal? libs '((testfile-clo-3a)))
         (cons
           (equal? (with-output-to-string (lambda () (load-program "testfile-clo-3b.so"))) "#t")
           (let () (import (testfile-clo-3a)) (g)))]
        ; otherwise not
        [(equal? libs '())
         (cons
           (equal? (with-output-to-string (lambda () (load-program "testfile-clo-3b.so"))) "#f")
           (not (let () (import (testfile-clo-3a)) (g))))]
        [else 'oops]))
    '(#t . #t))
  (equal? (let () (import (testfile-clo-3a)) (h)) (void))
  (not (let () (import (testfile-clo-3a)) (g)))

  ; testing support of procedures with improper formals
  (begin
    (with-output-to-file "testfile-clo-4a.ss"
      (lambda ()
        (pretty-print
         '(library (testfile-clo-4a)
            (export f g)
            (import (chezscheme))
            (define (f a . rest)
              (apply list a rest))
            (define g
              (case-lambda
                [(a) "foo"]
                [(a . rest) (apply list a rest)])))))
      'replace)
    #t)
  (begin
    (load-library "testfile-clo-4a.ss"
                  (lambda (x) (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f] [current-eval compile])
                                (eval x))))
    #t)
  (or
   (and (compile-profile) #t) ; => testfile-clo-4a was compiled with profiling, so not quite the same as below
   (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
       '(lambda (x y z)
          (import (testfile-clo-4a))
          (list
           (f x y z)
           (g x y z)))))
    '(begin
       (#3%$invoke-library '(testfile-clo-4a) '() 'testfile-clo-4a)
       (lambda (x y z)
         (#2%list (#2%list x y z)
                  ((#3%$top-level-value 'g) x y z))))))
)

(mat lots-of-libraries
  (begin
    (define (lol-mklibname n) (string->symbol (format "testfile-lol-~d" n)))
    (define (lol-mkvarname n) (string->symbol (format "n~d" n)))
    (define lol-fiblib
      (lambda (n)
        (let fiblib ([n n])
          (if (fx= n 1)
              `((library (testfile-lol-1) (export n1) (import (chezscheme)) (define n1 1))
                (library (testfile-lol-0) (export n0) (import (chezscheme)) (define n0 0)))
              (cons
                `(library (,(lol-mklibname n))
                   (export ,(lol-mkvarname n))
                   (import (chezscheme) (,(lol-mklibname (fx- n 1))) (,(lol-mklibname (fx- n 2))))
                   (define ,(lol-mkvarname n) (+ ,(lol-mkvarname (fx- n 1)) ,(lol-mkvarname (fx- n 2)))))
                (fiblib (fx- n 1)))))))
    #t)
  (eqv?
    (let ([n 10])
      (eval `(begin ,@(reverse (lol-fiblib n)) (let () (import (,(lol-mklibname n))) ,(lol-mkvarname n)))))
    55)
  (begin
    (define lol-n 100)
    (do ([lib* (lol-fiblib lol-n) (cdr lib*)] [n lol-n (fx- n 1)])
        ((null? lib*))
      (with-output-to-file (format "~s.ss" (lol-mklibname n))
        (lambda () (pretty-print (car lib*)))
        'replace))
    (with-output-to-file "testfile-lol-prog.ss"
      (lambda ()
        (for-each pretty-print
          `((import (chezscheme) (,(lol-mklibname lol-n)))
            (pretty-print ,(lol-mkvarname lol-n)))))
      'replace)
    (define $lol-watchdog
      (let ([t (current-time 'time-utc)])
        (let ([time-n 3])
          (separate-eval
            `(parameterize ([compile-imported-libraries #t])
               (compile-library ,(format "~a.ss" (lol-mklibname time-n)))))
          (do ([n 0 (+ n 1)]) ((> n time-n)) (delete-file (format "~a.so" (lol-mklibname n)))))
        (let ([t (time-difference (current-time 'time-utc) t)])
          (let ([t-reasonable
                  (let ([ns (* (+ (* (time-second t) (expt 10 9)) (time-nanosecond t)) lol-n)])
                    (make-time 'time-duration (remainder ns (expt 10 9)) (quotient ns (expt 10 9))))])
            `(let ([t (current-time 'time-utc)])
               (timer-interrupt-handler
                 (let ([t-reasonable (make-time 'time-duration ,(time-nanosecond t-reasonable) ,(time-second t-reasonable))])
                   (lambda ()
                     (unless (time<=? (time-difference (current-time 'time-utc) t) t-reasonable)
                       (errorf #f "unreasonable time elapsed"))
                     (set-timer 10000))))
               ((timer-interrupt-handler)))))))
    #t)
  (string?
    (separate-compile
      `(lambda (x)
         ,$lol-watchdog
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      'lol-prog))
  (equal?
    (separate-eval `(begin ,$lol-watchdog (load-program "testfile-lol-prog.so")))
    (format "~d\n"
      (let fib ([i 1] [n1 1] [n0 0])
        (if (fx= i lol-n)
            n1
            (fib (+ i 1) (+ n1 n0) n1)))))
  ; test rebuild
  (string?
    (separate-compile
      `(lambda (x)
         ,$lol-watchdog
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      'lol-prog))
  ; test maybe rebuild
  (string?
    (separate-compile
      `(lambda (x)
         ,$lol-watchdog
         (parameterize ([compile-imported-libraries #t])
           (maybe-compile-program x)))
      'lol-prog))
)

(mat import-dependencies
  (begin
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 3)))
             (define x (begin (printf "rt\n") 4)))))
      'replace)
    (separate-compile 'compile-library 'a)
    #t)
  (begin
    (with-output-to-file "testfile-m1.ss"
      (lambda ()
        (pretty-print
          '(module (q1)
             (import (testfile-a))
             (define-syntax q1 (identifier-syntax a)))))
      'replace)
    (separate-compile 'compile-file 'm1)
    #t)
  (equal?
    (separate-eval '(load "testfile-m1.so") 'q1)
    "ct\n3\n")
  (begin
    (with-output-to-file "testfile-m2.ss"
      (lambda ()
        (pretty-print
          '(module (q2)
             (import (testfile-a))
             (define-syntax q2 (identifier-syntax x)))))
      'replace)
    (separate-compile 'compile-file 'm2)
    #t)
  (equal?
    (separate-eval '(load "testfile-m2.so") 'q2)
    "rt\n4\n")
  (begin
    (sleep (make-time 'time-duration 1000000 1))
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 33)))
             (define x (begin (printf "rt\n") 44)))))
      'replace)
    (separate-compile 'compile-library 'a)
    (separate-compile 'maybe-compile-file 'm1)
    (separate-compile 'maybe-compile-file 'm2)
    #t)
  (equal?
    (separate-eval '(load "testfile-m1.so") 'q1)
    "ct\n33\n")
  (equal?
    (separate-eval '(load "testfile-m2.so") 'q2)
    "rt\n44\n")
  ; --------
  (begin
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 3)))
             (define x (begin (printf "rt\n") 4)))))
      'replace)
    (separate-compile 'compile-library 'a)
    #t)
  (begin
    (with-output-to-file "testfile-m3.ss"
      (lambda ()
        (pretty-print
          '(define-syntax q3 (let () (import (testfile-a)) (identifier-syntax a)))))
      'replace)
    (separate-compile 'compile-file 'm3)
    #t)
  (equal?
    (separate-eval '(load "testfile-m3.so") 'q3)
    "ct\n3\n")
  (begin
    (with-output-to-file "testfile-m4.ss"
      (lambda ()
        (pretty-print
          '(define-syntax q4 (let () (import (testfile-a)) (identifier-syntax x)))))
      'replace)
    (separate-compile 'compile-file 'm4)
    #t)
  (equal?
    (separate-eval '(load "testfile-m4.so") 'q4)
    "rt\n4\n")
  (begin
    (sleep (make-time 'time-duration 1000000 1))
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 33)))
             (define x (begin (printf "rt\n") 44)))))
      'replace)
    (separate-compile 'compile-library 'a)
    (separate-compile 'maybe-compile-file 'm3)
    (separate-compile 'maybe-compile-file 'm4)
    #t)
  (equal?
    (separate-eval '(load "testfile-m3.so") 'q3)
    "ct\n33\n")
  (equal?
    (separate-eval '(load "testfile-m4.so") 'q4)
    "rt\n44\n")
  ; --------
  (begin
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 3)))
             (define x (begin (printf "rt\n") 4)))))
      'replace)
    (separate-compile 'compile-library 'a)
    #t)
  (begin
    (with-output-to-file "testfile-m5.ss"
      (lambda ()
        (pretty-print
          '(define-property q5 q5 (let () (import (testfile-a)) #'a))))
      'replace)
    (separate-compile 'compile-file 'm5)
    #t)
  (equal?
    (separate-eval
      '(load "testfile-m5.so")
      '(let ()
         (define-syntax ref-prop
           (lambda (x)
             (lambda (r)
               (syntax-case x ()
                 [(_ id key) (r #'id #'key)]))))
         (ref-prop q5 q5)))
    "ct\n3\n")
  (begin
    (with-output-to-file "testfile-m6.ss"
      (lambda ()
        (pretty-print
          '(define-property q6 q6 (let () (import (testfile-a)) #'x))))
      'replace)
    (separate-compile 'compile-file 'm6)
    #t)
  (equal?
    (separate-eval '(load "testfile-m6.so")
      '(let ()
         (define-syntax ref-prop
           (lambda (x)
             (lambda (r)
               (syntax-case x ()
                 [(_ id key) (r #'id #'key)]))))
         (ref-prop q6 q6)))
    "rt\n4\n")
  (begin
    (sleep (make-time 'time-duration 1000000 1))
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 33)))
             (define x (begin (printf "rt\n") 44)))))
      'replace)
    (separate-compile 'compile-library 'a)
    (separate-compile 'maybe-compile-file 'm5)
    (separate-compile 'maybe-compile-file 'm6)
    #t)
  (equal?
    (separate-eval
      '(load "testfile-m5.so")
      '(let ()
         (define-syntax ref-prop
           (lambda (x)
             (lambda (r)
               (syntax-case x ()
                 [(_ id key) (r #'id #'key)]))))
         (ref-prop q5 q5)))
    "ct\n33\n")
  (equal?
    (separate-eval '(load "testfile-m6.so")
      '(let ()
         (define-syntax ref-prop
           (lambda (x)
             (lambda (r)
               (syntax-case x ()
                 [(_ id key) (r #'id #'key)]))))
         (ref-prop q6 q6)))
    "rt\n44\n")
  ; --------
  (begin
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 3)))
             (define x (begin (printf "rt\n") 4)))))
      'replace)
    (separate-compile 'compile-library 'a)
    #t)
  (begin
    (with-output-to-file "testfile-m7.ss"
      (lambda ()
        (pretty-print
          '(meta define q7 (let () (import (testfile-a)) #'a))))
      'replace)
    (separate-compile 'compile-file 'm7)
    #t)
  (equal?
    (separate-eval
      '(load "testfile-m7.so")
      '(let ()
         (define-syntax qq (lambda (x) q7))
         qq))
    "ct\n3\n")
  (begin
    (with-output-to-file "testfile-m8.ss"
      (lambda ()
        (pretty-print
          '(meta define q8 (let () (import (testfile-a)) #'x))))
      'replace)
    (separate-compile 'compile-file 'm8)
    #t)
  (equal?
    (separate-eval
      '(load "testfile-m8.so")
      '(let ()
         (define-syntax qq (lambda (x) q8))
         qq))
    "rt\n4\n")
  (begin
    (sleep (make-time 'time-duration 1000000 1))
    (with-output-to-file "testfile-a.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-a) (export a x) (import (chezscheme))
             (define-syntax a (begin (printf "ct\n") (identifier-syntax 33)))
             (define x (begin (printf "rt\n") 44)))))
      'replace)
    (separate-compile 'compile-library 'a)
    (separate-compile 'maybe-compile-file 'm7)
    (separate-compile 'maybe-compile-file 'm8)
    #t)
  (equal?
    (separate-eval
      '(load "testfile-m7.so")
      '(let ()
         (define-syntax qq (lambda (x) q7))
         qq))
    "ct\n33\n")
  (equal?
    (separate-eval
      '(load "testfile-m8.so")
      '(let ()
         (define-syntax qq (lambda (x) q8))
         qq))
    "rt\n44\n")
)

(mat eval-when-library
  (begin
    (with-output-to-file "testfile-ewl1.ss"
      (lambda ()
        (pretty-print
          '(eval-when ()
             (library (testfile-ewl1)
               (export x)
               (import (rnrs))
               (define-syntax x (identifier-syntax 23))))))
      'replace)
    (with-output-to-file "testfile-ewl2.ss"
      (lambda ()
        (pretty-print
          '(eval-when (eval)
             (library (testfile-ewl2)
               (export x)
               (import (rnrs))
               (define-syntax x (identifier-syntax 23))))))
      'replace)
    (with-output-to-file "testfile-ewl3.ss"
      (lambda ()
        (pretty-print
          '(eval-when (load)
             (library (testfile-ewl3)
               (export x)
               (import (rnrs))
               (define-syntax x (identifier-syntax 23))))))
      'replace)
    (with-output-to-file "testfile-ewl4.ss"
      (lambda ()
        (pretty-print
          '(eval-when (visit)
             (library (testfile-ewl4)
               (export x)
               (import (rnrs))
               (define-syntax x (identifier-syntax 23))))))
      'replace)
    (with-output-to-file "testfile-ewl5.ss"
      (lambda ()
        (pretty-print
          '(eval-when (revisit)
             (library (testfile-ewl5)
               (export x)
               (import (rnrs))
               (define-syntax x (identifier-syntax 23))))))
      'replace)
    (with-output-to-file "testfile-ewl6.ss"
      (lambda ()
        (pretty-print
          '(eval-when (compile)
             (library (testfile-ewl6)
               (export x)
               (import (rnrs))
               (define-syntax x (identifier-syntax 23))))))
      'replace)
    (for-each
      delete-file
      '("testfile-ewl1.so" "testfile-ewl2.so" "testfile-ewl3.so" "testfile-ewl4.so"
         "testfile-ewl5.so" "testfile-ewl6.so"))
    #t)
 ; loading testfile-ewlx.ss did not define library (testfile-ewlx)
  (error? (let ([x 55]) (import (testfile-ewl1)) x))
  (error? (let ([x 55]) (import (testfile-ewl3)) x))
  (error? (let ([x 55]) (import (testfile-ewl4)) x))
  (error? (let ([x 55]) (import (testfile-ewl5)) x))
  (error? (let ([x 55]) (import (testfile-ewl6)) x))
  (begin
    (for-each separate-compile '(ewl1 ewl2 ewl3 ewl4 ewl5 ewl6))
    (for-each load-library
      '("testfile-ewl1.so" "testfile-ewl2.so" "testfile-ewl3.so" "testfile-ewl4.so"
         "testfile-ewl5.so" "testfile-ewl6.so"))
    #t)
 ; loading testfile-ewlx.so did not define library (testfile-ewlx)
 ; actually "testfile-ewlx.ss did not ..." (ss rather than so)
 ;   now that load-library reloads source when dependency changes
  (error? (let ([x 55]) (import (testfile-ewl1)) x))
  (error? (let ([x 55]) (import (testfile-ewl2)) x))
  (error? (let ([x 55]) (import (testfile-ewl6)) x))
  (begin
    (load-library "testfile-ewl2.ss")
    (compile-library "testfile-ewl6")
    #t)
  (eqv? (let ([x 55]) (import (testfile-ewl2)) x) 23)
  (eqv? (let ([x 55]) (import (testfile-ewl3)) x) 23)
  (eqv? (let ([x 55]) (import (testfile-ewl4)) x) 23)
  (eqv? (let ([x 55]) (import (testfile-ewl5)) x) 23)
  (eqv? (let ([x 55]) (import (testfile-ewl6)) x) 23)
)

(mat library-directories
  (error? ; invalid argument
    (library-directories '("a" . hello)))
  (error? ; invalid argument
    (library-directories '("a" . ("src" . "obj"))))
  (error? ; invalid argument
    (library-directories '("a" . (("src")))))
  (error? ; invalid argument
    (library-directories '("a" . (("src" "obj")))))
  (error? ; invalid argument
    (library-directories '("a" . ((("src" "obj"))))))
  (let ([x (library-directories)])
    (and (list? x)
         (andmap (lambda (x) (and (pair? x) (string? (car x)) (string? (cdr x)))) x)))
  (if (windows?)
      (parameterize ([library-directories "a1;boo;c:/;dxxy"])
        (equal? (library-directories) '(("a1" . "a1") ("boo" . "boo") ("c:/" . "c:/") ("dxxy" . "dxxy"))))
      (parameterize ([library-directories "a1:boo:c;/:dxxy"])
        (equal? (library-directories) '(("a1" . "a1") ("boo" . "boo") ("c;/" . "c;/") ("dxxy" . "dxxy")))))
  (if (windows?)
      (parameterize ([library-directories "a1;boo;;boo-obj;c:/;;dxxy"])
        (equal? (library-directories) '(("a1" . "a1") ("boo" . "boo-obj") ("c:/" . "dxxy"))))
      (parameterize ([library-directories "a1:boo::boo-obj:c;/::dxxy"])
        (equal? (library-directories) '(("a1" . "a1") ("boo" . "boo-obj") ("c;/" . "dxxy")))))
  (let ([default (library-directories)])
    (if (windows?)
        (parameterize ([library-directories "a1;boo;c:/;dxxy;"])
          (equal? (library-directories) `(,@'(("a1" . "a1") ("boo" . "boo") ("c:/" . "c:/") ("dxxy" . "dxxy")) ,@default)))
        (parameterize ([library-directories "a1:boo:c;/:dxxy:"])
          (equal? (library-directories) `(,@'(("a1" . "a1") ("boo" . "boo") ("c;/" . "c;/") ("dxxy" . "dxxy")) ,@default)))))
  (begin
    (with-output-to-file "testfile-ld1.ss"
      (lambda ()
        (pretty-print
          `(library (,(string->symbol (cd)) testfile-ld1)
             (export x)
             (import (rnrs))
             (define-syntax x (identifier-syntax 23)))))
      'replace)
    #t)
  (error? ; library not found
    (parameterize ([library-directories '()])
      (eval `(lambda () (import (testfile-ld1)) x))))
  (eqv?
    ((parameterize ([library-directories '()])
       (eval `(lambda () (import (,(string->symbol (cd)) testfile-ld1)) x))))
    23)
)

(mat library-extensions
  (error? ; invalid argument
    (library-extensions '.a1.sls))
  (error? ; invalid argument
    (library-extensions '((".foo"))))
  (error? ; invalid argument
    (library-extensions '((".foo" ".bar"))))
  (error? ; invalid argument
    (library-extensions '(((".junk")))))
  (let ([x (library-extensions)])
    (and (list? x)
         (andmap (lambda (x) (and (pair? x) (string? (car x)) (string? (cdr x)))) x)))
  (if (windows?)
      (parameterize ([library-extensions ".a1.sls;.boo;.crud;.junk"])
        (equal? (library-extensions) '((".a1.sls" . ".a1.so") (".boo" . ".so") (".crud" . ".so") (".junk" . ".so"))))
      (parameterize ([library-extensions ".a1.sls:.boo:.crud:.junk"])
        (equal? (library-extensions) '((".a1.sls" . ".a1.so") (".boo" . ".so") (".crud" . ".so") (".junk" . ".so")))))
  (let ([default (library-extensions)])
    (if (windows?)
        (parameterize ([library-extensions ".a1.sls;.boo;.crud;.junk;"])
          (equal? (library-extensions) `(,@'((".a1.sls" . ".a1.so") (".boo" . ".so") (".crud" . ".so") (".junk" . ".so")) ,@default)))
        (parameterize ([library-extensions ".a1.sls:.boo:.crud:.junk:"])
          (equal? (library-extensions) `(,@'((".a1.sls" . ".a1.so") (".boo" . ".so") (".crud" . ".so") (".junk" . ".so")) ,@default)))))
  (let ([default (library-extensions)])
    (if (windows?)
        (parameterize ([library-extensions ".a1.sls;.boo;;.booso;.crud;;.junk;"])
          (equal? (library-extensions) `(,@'((".a1.sls" . ".a1.so") (".boo" . ".booso") (".crud" . ".junk")) ,@default)))
        (parameterize ([library-extensions ".a1.sls:.boo::.booso:.crud::.junk:"])
          (equal? (library-extensions) `(,@'((".a1.sls" . ".a1.so") (".boo" . ".booso") (".crud" . ".junk")) ,@default)))))
)

(mat library-search-handler
  (procedure? (library-search-handler))
  (eq? (library-search-handler) default-library-search-handler)
  (error? (default-library-search-handler "not-symbol" '(lib) '() '()))
  (error? (default-library-search-handler 'import 'bad-library-name '() '()))
  (error? (default-library-search-handler 'import '(lib) '(("invalid" "path" "list")) '()))
  (error? (default-library-search-handler 'import '(lib) '(("foo" . "bar")) '(("bad") ("extensions"))))
  (error?
   (parameterize ([library-search-handler
                   (lambda (who path dir* all-ext*)
                     (values '(bad source path) #f #f))])
     (eval '(import (foo)))))
  (error?
   (parameterize ([library-search-handler
                   (lambda (who path dir* all-ext*)
                     (values #f '(bad object path) #f))])
     (eval '(import (foo)))))
  (error?
   (parameterize ([library-search-handler
                   (lambda (who path dir* all-ext*)
                     (values #f #f #t))])
     (eval '(import (foo)))))
  (begin
    (mkdir "lsh-testdir")
    (mkdir "lsh-testdir/src1")
    (mkdir "lsh-testdir/src2")
    (mkdir "lsh-testdir/obj")
    #t)
  (begin
    (with-output-to-file "lsh-testdir/src1/lib.ss"
      (lambda ()
        (pretty-print
         '(library (lib) (export a) (import (scheme))
            (define a "src1 provided this a"))))
      'replace)
    (with-output-to-file "lsh-testdir/src2/lib.ss"
      (lambda ()
        (pretty-print
         '(library (lib) (export a) (import (scheme))
            (define a "a from src2"))))
      'replace)
    (with-output-to-file "lsh-testdir/src2/foo.ss"
      (lambda ()
        (pretty-print
         '(library (foo) (export a) (import (scheme) (lib)))))
      'replace)
    (parameterize ([generate-wpo-files #t]
                   [compile-imported-libraries #t]
                   [library-directories '(("src2" . "obj"))])
      (compile-file "lsh-testdir/src2/lib.ss" "lsh-testdir/obj/lib.so")
      (compile-file "lsh-testdir/src2/foo.ss" "lsh-testdir/obj/foo.so"))
    #t)
  (equal?
   "a from src2\n"
   (separate-eval
    '(cd "lsh-testdir")
    '(library-extensions '((".ss" . ".so")))
    '(library-directories '(("src2" . "obj") ("src1" . "obj")))
    '(library-search-handler
      (lambda (who path dir* all-ext*)
        (let-values ([(src-path obj-path obj-exists?)
                      (default-library-search-handler who path dir* all-ext*)])
          (assert (equal? src-path (path-build "src2" "lib.ss")))
          (assert (equal? obj-path (path-build "obj" "lib.so")))
          (assert obj-exists?)
          (values src-path obj-path obj-exists?))))
    '(printf "~a\n" (let () (import (lib)) a))))
  (equal?
   "src1 provided this a\n"
   (separate-eval
    '(cd "lsh-testdir")
    '(library-extensions '((".ss" . ".so")))
    '(library-directories '(("src2" . "obj") ("src1" . "obj")))
    '(library-search-handler
      (lambda (who path dir* all-ext*)
        (assert (eq? who 'import))
        (assert (equal? path '(lib)))
        (assert (equal? dir* (library-directories)))
        (assert (equal? all-ext* (library-extensions)))
        ;; switcheroo
        (values "src1/lib.ss" #f #f)))
    '(printf "~a\n" (let () (import (lib)) a))))
  (equal?
   (string-append
    "compiling src1/lib.ss with output to obj/lib-compiled.so\n"
    "src1 provided this a\n")
   (separate-eval
    '(cd "lsh-testdir")
    '(compile-imported-libraries #t)
    '(library-search-handler
      (lambda (who path dir* all-ext*)
        (values "src1/lib.ss" "obj/lib-compiled.so" #f)))
    '(printf "~a\n" (let () (import (lib)) a))))
  ;; the default library-search-handler finds obj/lib.wpo
  ;; so no libraries are needed at run time
  (equal?
    "()\n"
   (separate-eval
    '(cd "lsh-testdir")
    '(library-extensions '((".ss" . ".so")))
    '(library-directories '(("src1" . "obj") ("src2" . "obj")))
    '(compile-whole-library "obj/foo.wpo" "foo.library")))
  (equal?
   "((lib))\n"
   (separate-eval
    '(cd "lsh-testdir")
    '(library-extensions '((".ss" . ".so")))
    '(library-directories '(("src1" . "obj") ("src2" . "obj")))
    '(define (check who path dir*)
       (assert (eq? who 'compile-whole-library))
       (assert (equal? path '(lib)))
       (assert (equal? dir* (library-directories))))
    '(library-search-handler
      (lambda (who path dir* all-ext*)
        (check who path dir*)
        (assert (equal? all-ext* '((".ss" . ".wpo"))))
        ;; default search finds the wpo file, but ...
        (let-values ([(src-path obj-path obj-exists?)
                      (default-library-search-handler who path dir* all-ext*)])
          ;; user reordered library-directories since compiling the wpo file
          (assert (equal? src-path (path-build "src1" "lib.ss")))
          (assert (equal? obj-path (path-build "obj" "lib.wpo")))
          (assert obj-exists?))
        ;; ... we install a new handler that returns the object file instead
        (library-search-handler
         (lambda (who path dir* all-ext*)
           (check who path dir*)
           (assert (equal? all-ext* (library-extensions)))
           (values #f "obj/lib.so" #t)))
        ;; ... and report no .wpo file found so we fall back to the
        ;; library-search-handler just installed
        (values #f #f #f)))
    '(compile-whole-library "obj/foo.wpo" "foo.library")))
  (begin
    (rm-rf "lsh-testdir")
    #t)
)

(mat compile-imported-libraries
  (not (compile-imported-libraries))
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (define $cil '())
    (with-output-to-file "testdir/cil1.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil1 $cil))))
        (pretty-print
          '(library (testdir cil1) (export a) (import (rnrs))
             (define x 57388321)
             (define-syntax a (lambda (q) #'x)))))
      'replace)
    (with-output-to-file "testdir/cil2.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil2 $cil))))
        (pretty-print
          '(library (testdir cil2) (export a b f get-y) (import (rnrs) (testdir cil1))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil2)))
        (pretty-print '(f (cons (b) a))))
      'replace)
    #t)
  (equal?
    (parameterize ([compile-imported-libraries #t]
                   [compile-file-message #f]
                   [compile-library-handler
                    (lambda args
                      (printf "hello!\n")
                      (flush-output-port)
                      (apply compile-library args)
                      (printf "goodbye.\n")
                      (flush-output-port))])
      (with-output-to-string
        (lambda ()
          (load-program "testdir/cil"))))
    "hello!\nhello!\ngoodbye.\ngoodbye.\n")
  (file-exists? "testdir/cil1.so")
  (file-exists? "testdir/cil2.so")
  (equal? $cil '(cil1 cil2))
  (equal? (let () (import (testdir cil2)) (get-y)) '((57388321) . 57388321))
  (equal? (let () (import (testdir cil2)) (f 772) (get-y)) 772)
  (eq?
    (parameterize ([compile-imported-libraries #t])
      (load-program "testdir/cil"))
    (void))
  (equal? (let () (import (testdir cil2)) (get-y)) '((57388321) . 57388321))
  (equal? $cil '(cil1 cil2))
  (begin
    (rm-rf "testdir")
    #t)
 ; once again with extension .ss, to see if position in library-extensions list matters
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (define $cil '())
    (with-output-to-file "testdir/cil3.ss"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil3 $cil))))
        (pretty-print
          '(library (testdir cil3) (export a) (import (rnrs))
             (define x 57388321)
             (define-syntax a (lambda (q) #'x)))))
      'replace)
    (with-output-to-file "testdir/cil4.ss"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil4 $cil))))
        (pretty-print
          '(library (testdir cil4) (export a b f get-y) (import (rnrs) (testdir cil3))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil4)))
        (pretty-print '(f (cons (b) a))))
      'replace)
    #t)
  (eq?
    (parameterize ([compile-imported-libraries #t])
      (load-program "testdir/cil"))
    (void))
  (file-exists? "testdir/cil3.so")
  (file-exists? "testdir/cil4.so")
  (equal? $cil '(cil3 cil4))
  (equal? (let () (import (testdir cil4)) (get-y)) '((57388321) . 57388321))
  (equal? (let () (import (testdir cil4)) (f 772) (get-y)) 772)
  (eq?
    (parameterize ([compile-imported-libraries #t])
      (load-program "testdir/cil"))
    (void))
  (equal? (let () (import (testdir cil4)) (get-y)) '((57388321) . 57388321))
  (equal? $cil '(cil3 cil4))
  (begin
    (rm-rf "testdir")
    (rm-rf "objdir")
    #t)
 ; try again with different library-directories and library-extensions
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (define $cil '())
    (with-output-to-file "testdir/cil5.ss"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil5 $cil))))
        (pretty-print
          '(library (testdir cil5) (export a) (import (rnrs))
             (define x 57388321)
             (define-syntax a (lambda (q) #'x)))))
      'replace)
    (with-output-to-file "testdir/cil6.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil6 $cil))))
        (pretty-print
          '(library (testdir cil6) (export a b f get-y) (import (rnrs) (testdir cil5))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil6)))
        (pretty-print '(f (cons (b) a))))
      'replace)
    #t)
  (eq?
    (parameterize ([compile-imported-libraries #t]
                   [library-directories '(("." . "objdir"))]
                   [library-extensions '((".sls" . ".bar") (".ss" . ".foo"))])
      (load-program "testdir/cil"))
    (void))
  (file-exists? "objdir/testdir/cil5.foo")
  (file-exists? "objdir/testdir/cil6.bar")
  (equal? $cil '(cil5 cil6))
  (equal? (let () (import (testdir cil6)) (get-y)) '((57388321) . 57388321))
  (equal? (let () (import (testdir cil6)) (f 772) (get-y)) 772)
  (eq?
    (parameterize ([compile-imported-libraries #t]
                   [library-directories '(("." . "objdir"))]
                   [library-extensions '((".sls" . ".bar") (".ss" . ".foo"))])
      (load-program "testdir/cil"))
    (void))
  (equal? (let () (import (testdir cil6)) (get-y)) '((57388321) . 57388321))
  (equal? $cil '(cil5 cil6))
  (begin
    (rm-rf "testdir")
    (rm-rf "objdir")
    #t)
 ; what if we compile explicitly first?
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (define $cil '())
    (with-output-to-file "testdir/cil7.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil7 $cil))))
        (pretty-print
          '(library (testdir cil7) (export a) (import (rnrs))
             (define x 57388321)
             (define-syntax a (lambda (q) #'x)))))
      'replace)
    (with-output-to-file "testdir/cil8.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil8 $cil))))
        (pretty-print
          '(library (testdir cil8) (export a b f get-y) (import (rnrs) (testdir cil7))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil8)))
        (pretty-print '(f (cons (b) a))))
      'replace)
    (compile-library "testdir/cil7.sls")
    (compile-library "testdir/cil8.sls")
    #t)
  (file-exists? "testdir/cil7.so")
  (file-exists? "testdir/cil8.so")
  (equal? $cil '(cil8 cil7))
  (eq?
    (parameterize ([compile-imported-libraries #t])
      (load-program "testdir/cil"))
    (void))
  (equal? $cil '(cil8 cil7))
  (equal? (let () (import (testdir cil8)) (get-y)) '((57388321) . 57388321))
  (begin
    (rm-rf "testdir")
    #t)
 ; what if we compile ahead of time, and put .so in library extensions?
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (define $cil '())
    (with-output-to-file "testdir/cil9.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil9 $cil))))
        (pretty-print
          '(library (testdir cil9) (export a) (import (rnrs))
             (define x 57388321)
             (define-syntax a (lambda (q) #'x)))))
      'replace)
    (with-output-to-file "testdir/cil10.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil10 $cil))))
        (pretty-print
          '(library (testdir cil10) (export a b f get-y) (import (rnrs) (testdir cil9))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil10)))
        (pretty-print '(f (cons (b) a))))
      'replace)
    (compile-library "testdir/cil9.sls")
    (compile-library "testdir/cil10.sls")
    #t)
  (file-exists? "testdir/cil9.so")
  (file-exists? "testdir/cil10.so")
  (equal? $cil '(cil10 cil9))
  (eq?
    (parameterize ([compile-imported-libraries #t]
                   [library-extensions (cons ".so" (library-extensions))])
      (load-program "testdir/cil"))
    (void))
  (equal? $cil '(cil10 cil9))
  (equal? (let () (import (testdir cil10)) (get-y)) '((57388321) . 57388321))
  (begin
    (rm-rf "testdir")
    #t)
 ; separate compilation
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (define $cil '())
    (with-output-to-file "testdir/cil11.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil11 $cil))))
        (pretty-print
          '(library (testdir cil11) (export a) (import (rnrs))
             (define x 57388321)
             (define-syntax a (lambda (q) #'x)))))
      'replace)
    (with-output-to-file "testdir/cil12.sls"
      (lambda ()
        (pretty-print '(eval-when (compile) (set! $cil (cons 'cil12 $cil))))
        (pretty-print
          '(library (testdir cil12) (export a b f get-y) (import (rnrs) (testdir cil11))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil.ss"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil12)))
        (pretty-print '(f (cons (b) a))))
      'replace)
    #t)
  (begin
    (separate-compile
      '(lambda (x)
         (set! $cil '())
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      "testdir/cil")
    #t)
  (file-exists? "testdir/cil.so")
  (file-exists? "testdir/cil11.so")
  (file-exists? "testdir/cil12.so")
  (equal? $cil '())
  (equal? (let () (import (testdir cil11)) a) 57388321)
  (eq?
    (parameterize ([compile-imported-libraries #t])
      (load-program "testdir/cil.so"))
    (void))
  (equal? (let () (import (testdir cil12)) (get-y)) '((57388321) . 57388321))
  (equal? $cil '())
  (begin
    (rm-rf "testdir")
    #t)
 ; test auto recompilation if dependency is recompiled
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (with-output-to-file "testdir/cil13.sls"
      (lambda ()
        (pretty-print
          '(library (testdir cil13) (export a x) (import (rnrs))
             (define x 73)
             (define-syntax a (lambda (q) #'(+ x 6))))))
      'replace)
    (with-output-to-file "testdir/cil14.sls"
      (lambda ()
        (pretty-print
          '(library (testdir cil14) (export a b f get-y) (import (rnrs) (testdir cil13))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a x)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil-a.ss"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil14)))
        (pretty-print '(f (cons (b) a)))
        (pretty-print '(display (get-y))))
      'replace)
    (with-output-to-file "testdir/cil15.sls"
      (lambda ()
        (pretty-print
          '(library (testdir cil15) (export a x) (import (rnrs))
             (define x 73)
             (define-syntax a (lambda (q) #'(+ x 6))))))
      'replace)
    (with-output-to-file "testdir/cil16.sls"
      (lambda ()
        (pretty-print
          '(library (testdir cil16) (export a b f get-y) (import (rnrs) (testdir cil15))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a x)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil-b.ss"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil16)))
        (pretty-print '(f (cons (b) a)))
        (pretty-print '(display (get-y))))
      'replace)
    (with-output-to-file "testdir/cil17.sls"
      (lambda ()
        (pretty-print
          '(library (testdir cil17) (export a x) (import (rnrs))
             (define x 73)
             (define-syntax a (lambda (q) #'(+ x 6))))))
      'replace)
    (with-output-to-file "testdir/cil18.sls"
      (lambda ()
        (pretty-print
          '(library (testdir cil18) (export a b f get-y) (import (rnrs) (testdir cil17))
             (define y #f)
             (define get-y (lambda () y))
             (define b (lambda () (list a x)))
             (define f (lambda (v) (set! y v))))))
      'replace)
    (with-output-to-file "testdir/cil-c.ss"
      (lambda ()
        (display "#! /usr/bin/env scheme-script\n")
        (pretty-print '(import (rnrs) (testdir cil18)))
        (pretty-print '(f (cons (b) a)))
        (pretty-print '(display (get-y))))
      'replace)
    #t)
 ; compile 'em all in a separate process
  (begin
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      "testdir/cil-a")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      "testdir/cil-b")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      "testdir/cil-c")
    #t)
  (file-exists? "testdir/cil-a.so")
  (file-exists? "testdir/cil13.so")
  (file-exists? "testdir/cil14.so")
  (file-exists? "testdir/cil-b.so")
  (file-exists? "testdir/cil15.so")
  (file-exists? "testdir/cil16.so")
  (file-exists? "testdir/cil-c.so")
  (file-exists? "testdir/cil13.so")
  (file-exists? "testdir/cil14.so")
 ; can't test programs' output here, since we don't want
 ; to load the libraries until after the next step
 ; now delete object file or modify source file and recompile
  (begin
   ; ensure a different time stamp
    (delete-file "testdir/cil13.so")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      "testdir/cil-a")
    (sleep (make-time 'time-duration 0 1))
    (with-output-to-file "testdir/cil15.sls"
      (lambda ()
        (pretty-print
          '(library (testdir cil15) (export a x) (import (rnrs))
             (define x -73)
             (define-syntax a (lambda (q) #'(+ x 6))))))
      'replace)
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #t])
           (compile-program x)))
      "testdir/cil-b")
    (delete-file "testdir/cil17.so")
    (separate-compile
      '(lambda (x)
         (parameterize ([compile-imported-libraries #f]) ; #f here rather than #t should cause failure
           (compile-program x)))
      "testdir/cil-c")
    #t)
  (file-exists? "testdir/cil-a.so")
  (file-exists? "testdir/cil13.so")
  (file-exists? "testdir/cil14.so")
  (file-exists? "testdir/cil-b.so")
  (file-exists? "testdir/cil15.so")
  (file-exists? "testdir/cil16.so")
  ; testdir/cil-c.so exists now that load-library reloads source when dependency changes
  (file-exists? "testdir/cil-c.so")
  (file-exists? "testdir/cil13.so")
  (file-exists? "testdir/cil14.so")
  (file-exists? "testdir/cil-a.so")
  (file-exists? "testdir/cil13.so")
  (file-exists? "testdir/cil14.so")
 ; now test programs' output
  (equal?
    (with-output-to-string
      (lambda () (load-program "testdir/cil-a.so")))
    "((79 73) . 79)")
  (equal?
    (with-output-to-string
      (lambda () (load-program "testdir/cil-b.so")))
    "((-67 -73) . -67)")
  (begin
    (rm-rf "testdir")
    #t)
  ; ---------------------------------------------------------------
  (begin
    (mkdir "testdir")
    #t)
  (begin
    (with-output-to-file "testdir/cil19A.ss"
      (lambda ()
        (pretty-print
          '(library (testdir cil19A)
             (export x)
             (import (chezscheme))
             (define x (make-parameter 13)))))
      'replace)
    (with-output-to-file "testdir/cil19B.ss"
      (lambda ()
        (pretty-print
          '(library (testdir cil19B)
             (export y)
             (import (chezscheme))
            ; importing from within RHS to make sure RHS imports are tracked
             (define y (make-parameter (let () (import (testdir cil19A)) (+ (x) 5)))))))
      'replace)
    (with-output-to-file "testdir/cil19C.ss"
      (lambda ()
        (pretty-print
          '(import (chezscheme) (testdir cil19B)))
        (pretty-print
          '(pretty-print (y))))
      'replace)
    #t)
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    (format
     "compiling testdir~acil19B.ss with output to testdir~acil19B.so\ncompiling testdir~acil19A.ss with output to testdir~acil19A.so\n18\n"
     (directory-separator) (directory-separator) (directory-separator) (directory-separator)))
  (file-exists? "testdir/cil19A.so")
  (file-exists? "testdir/cil19B.so")
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    "18\n")
 ; now add an include file
  (begin
    (sleep (make-time 'time-duration 0 1))
    (with-output-to-file "testdir/cil19A1.ss"
      (lambda ()
        (pretty-print
          '(define x (make-parameter 19))))
      'replace)
    (with-output-to-file "testdir/cil19A.ss"
      (lambda ()
        (pretty-print
          '(library (testdir cil19A)
             (export x)
             (import (chezscheme))
             (include "cil19A1.ss"))))
      'replace)
    #t)
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    (format
     "compiling testdir~acil19A.ss with output to testdir~acil19A.so\ncompiling testdir~acil19B.ss with output to testdir~acil19B.so\n24\n"
     (directory-separator) (directory-separator) (directory-separator) (directory-separator)))
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    "24\n")
 ; now change first include file to include a second
  (begin
    (sleep (make-time 'time-duration 0 1))
    (with-output-to-file "testdir/cil19A2.ss"
      (lambda ()
        (pretty-print
          '(define x (make-parameter 23))))
      'replace)
    (with-output-to-file "testdir/cil19A1.ss"
      (lambda ()
        (pretty-print
          '(include "cil19A2.ss")))
      'replace)
    #t)
  ; load w/compile-imported-libraries #f---should get old result
  ; not longer now that load-library reloads source when dependency changes
  (equal?
    (separate-eval
      '(compile-imported-libraries #f)
      '(load-program "testdir/cil19C.ss"))
    "28\n"
    #;"24\n")
  ; should get new result with compile-imported-libraries #t
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    (format
     "compiling testdir~acil19A.ss with output to testdir~acil19A.so\ncompiling testdir~acil19B.ss with output to testdir~acil19B.so\n28\n"
    (directory-separator) (directory-separator) (directory-separator) (directory-separator)))
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    "28\n")
 ; now change second include file
  (begin
    (sleep (make-time 'time-duration 0 1))
    (with-output-to-file "testdir/cil19A2.ss"
      (lambda ()
        (pretty-print
          '(define x (make-parameter 31))))
      'replace)
    #t)
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    (format
     "compiling testdir~acil19A.ss with output to testdir~acil19A.so\ncompiling testdir~acil19B.ss with output to testdir~acil19B.so\n36\n"
     (directory-separator) (directory-separator) (directory-separator) (directory-separator)))
  (equal?
    (separate-eval
      '(compile-imported-libraries #t)
      '(load-program "testdir/cil19C.ss"))
    "36\n")
  (begin
    (rm-rf "testdir")
    #t)
)

(mat import-notify
  (eq? (import-notify 'yes) (void))
  (eq? (import-notify) #t)
  (begin
    (with-output-to-file "testfile-imno1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-imno1) (export x) (import (rnrs))
             (define x -73))))
      'replace)
    (with-output-to-file "testfile-imno2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-imno2) (export y) (import (rnrs) (testfile-imno1))
             (define y (+ x x)))))
      'replace)
    (separate-compile 'imno1)
    #t)
  (equal?
    (parameterize ([source-directories '(".")]
                   [library-directories '(".")]
                   [console-output-port (open-output-string)])
      (eval '(lambda () (import (testfile-imno2)) y))
      (get-output-string (console-output-port)))
    "import: did not find source file \"testfile-imno2.chezscheme.sls\"\nimport: found source file \"testfile-imno2.ss\"\nimport: did not find corresponding object file \"testfile-imno2.so\"\nimport: loading source file \"testfile-imno2.ss\"\nimport: did not find source file \"testfile-imno1.chezscheme.sls\"\nimport: found source file \"testfile-imno1.ss\"\nimport: found corresponding object file \"testfile-imno1.so\"\nimport: object file is not older\nimport: visiting object file \"testfile-imno1.so\"\nattempting to 'revisit' previously 'visited' \"testfile-imno1.so\" for library (testfile-imno1) run-time info\n")
  (eq? (import-notify #f) (void))
)

(mat rnrs-libraries
  (equal?
    (let ([cons void])
      (let () (import (rnrs base)) (cons 3 4)))
    '(3 . 4))
)

(mat top-level-program
  (equal?
    (with-output-to-string
      (lambda ()
        (eval '(top-level-program (import (scheme))
                 (define-syntax a (identifier-syntax (cons x y)))
                 (define x 55)
                 (printf "x = ~s\n" x)
                 (define y 'yyy)
                 (printf "(a x y) = ~s\n" (list a x y))))))
    "x = 55\n(a x y) = ((55 . yyy) 55 yyy)\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (with-output-to-file "testfile-tlp1.ss"
          (lambda ()
            (pretty-print
              '(library (testfile-tlp1) (export $tlp-x $tlp-y) (import (scheme))
                 (define-syntax $tlp-y
                   (begin
                     (printf "visiting tlp1\n")
                     (identifier-syntax (cons ($tlp-x) (z)))))
                 (define z (make-parameter 'zzz))
                 (define $tlp-x (make-parameter 'xxx))
                 (printf "invoking tlp1\n"))))
          'replace)
        (with-output-to-file "testfile-tlp.ss"
          (lambda ()
            (pretty-print
              '(top-level-program (import (testfile-tlp1) (rnrs) (only (scheme) list printf))
                 (define-syntax a (identifier-syntax (cons x y)))
                 (define x ($tlp-x))
                 (printf "x = ~s\n" x)
                 (define y $tlp-y)
                 (printf "(a x y) = ~s\n" (list a x y)))))
          'replace)
       ; compile in same Scheme process
        (compile-file "testfile-tlp1")
        (compile-file "testfile-tlp")))
    "compiling testfile-tlp1.ss with output to testfile-tlp1.so\nvisiting tlp1\ncompiling testfile-tlp.ss with output to testfile-tlp.so\n")
  (equal?
    (with-output-to-string
      (lambda () (load "testfile-tlp.so")))
    "invoking tlp1\nx = xxx\n(a x y) = ((xxx xxx . zzz) xxx (xxx . zzz))\n")
  (begin
    (with-output-to-file "testfile-tlp2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-tlp2) (export $tlp-x $tlp-y) (import (scheme))
             (define-syntax $tlp-y
               (begin
                 (printf "visiting tlp2\n")
                 (identifier-syntax (cons ($tlp-x) z))))
             (define z 'zzz)
             (define $tlp-x (make-parameter 'xxx))
             (printf "invoking tlp2\n"))))
      'replace)
    (with-output-to-file "testfile-tlp.ss"
      (lambda ()
        (pretty-print
          '(top-level-program (import (testfile-tlp2) (rnrs) (only (scheme) list printf))
             (define-syntax a (identifier-syntax (cons x y)))
             (define x ($tlp-x))
             (printf "x = ~s\n" x)
             (define y $tlp-y)
             (printf "(a x y) = ~s\n" (list a x y)))))
      'replace)
    (for-each separate-compile '(tlp2 tlp))
    #t)
  (equal?
    (with-output-to-string
      (lambda () (load "testfile-tlp.so")))
    "invoking tlp2\nx = xxx\n(a x y) = ((xxx xxx . zzz) xxx (xxx . zzz))\n")
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((import (rnrs))
            (define x 0)
            (define (inc v) (set! x (+ x v)) x)
            (if (inc 3)))))
      'replace)
    #t)
  (error? ; invalid syntax (if (inc 3)) at [not near] line 4, char 1
    (load-program "testfile.ss"))
  (equal?
    (with-output-to-string
      (lambda ()
        (with-output-to-file "testfile-tlp1.ss"
          (lambda ()
            (pretty-print
              '(library (testfile-tlp1) (export $tlp-x $tlp-y) (import (scheme))
                 (define-syntax $tlp-y
                   (begin
                     (printf "visiting tlp1\n")
                     (identifier-syntax (cons ($tlp-x) (z)))))
                 (define z (make-parameter 'zzz))
                 (define $tlp-x (make-parameter 'xxx))
                 (printf "invoking tlp1\n"))))
          'replace)
        (with-output-to-file "testfile-tlp.ss"
          (lambda ()
            (for-each pretty-print
              '((import (testfile-tlp1) (rnrs) (only (scheme) list printf))
                (define-syntax a (identifier-syntax (cons x y)))
                (define x ($tlp-x))
                (printf "x = ~s\n" x)
                (define y $tlp-y)
                (printf "(a x y) = ~s\n" (list a x y)))))
          'replace)
       ; compile in same Scheme process
        (compile-library "testfile-tlp1")
        (compile-program "testfile-tlp")))
    "compiling testfile-tlp1.ss with output to testfile-tlp1.so\nvisiting tlp1\ncompiling testfile-tlp.ss with output to testfile-tlp.so\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (load-library "testfile-tlp1.so")))
    "")
  (equal?
    (with-output-to-string
      (lambda ()
        (load-program "testfile-tlp.so")))
    "invoking tlp1\nx = xxx\n(a x y) = ((xxx xxx . zzz) xxx (xxx . zzz))\n")
 ; load again from source
  (equal?
    (with-output-to-string
      (lambda ()
        (load-library "testfile-tlp1.ss")))
    "visiting tlp1\n")
  (error? ; wrong version of testfile-tlp1
    (load-program "testfile-tlp.so"))
  (equal?
    (with-output-to-string
      (lambda ()
        (load-program "testfile-tlp.ss")))
    "invoking tlp1\nx = xxx\n(a x y) = ((xxx xxx . zzz) xxx (xxx . zzz))\n")
  (begin
    (delete-file "testfile-tlp1.so")
    (delete-file "testfile-tlp.so")
    #t)
  (begin
    (with-output-to-file "testfile-tlp1.ss"
      (lambda ()
        (parameterize ([print-vector-length #t])
          (pretty-print
            '(library (testfile-tlp1) (export $tlp-z) (import (chezscheme))
               (define $tlp-z '#3(1 2))))))
      'replace)
    (with-output-to-file "testfile-tlp.ss"
      (lambda ()
        (parameterize ([print-vector-length #t])
          (for-each pretty-print
            '((import (testfile-tlp1) (chezscheme))
              (pretty-print (equal? $tlp-z '#3(1 2)))))))
      'replace)
    #t)
  (error? ; nonstandard vector-length syntax
    (compile-library "testfile-tlp1"))
  (error? ; nonstandard vector-length syntax
    (compile-program "testfile-tlp"))
  (error? ; nonstandard vector-length syntax
    (load-library "testfile-tlp1.ss"))
  (error? ; nonstandard vector-length syntax
    (load-program "testfile-tlp.ss"))
  (begin
    (with-output-to-file "testfile-tlp1.ss"
      (lambda ()
        (display "#!chezscheme\n")
        (parameterize ([print-vector-length #t])
          (pretty-print
            '(library (testfile-tlp1) (export $tlp-z) (import (chezscheme))
               (define $tlp-z '#3(1 2))))))
      'replace)
    (with-output-to-file "testfile-tlp.ss"
      (lambda ()
        (display "#!chezscheme\n")
        (parameterize ([print-vector-length #t])
          (for-each pretty-print
            '((import (testfile-tlp1) (chezscheme))
              (pretty-print (equal? $tlp-z '#3(1 2)))))))
      'replace)
    #t)
  (equal?
    (begin
      (compile-library "testfile-tlp1")
      (compile-program "testfile-tlp")
      (with-output-to-string
        (lambda ()
          (load-library "testfile-tlp1.so")
          (load-program "testfile-tlp.so"))))
    "#t\n")
  (equal?
    (with-output-to-string
      (lambda ()
        (load-library "testfile-tlp1.ss")
        (load-program "testfile-tlp.ss")))
    "#t\n")
 ; test to make sure compiled top-level-program doesn't try to
 ; load libraries upon which it should not depend
  (equal?
    (begin
      (with-output-to-file "testfile-tlp3.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-tlp3)
               (export t1-x)
               (import (chezscheme))
               (define t1-x 332211))))
        'replace)
      (with-output-to-file "testfile-tlp4.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-tlp4)
               (export t2-q)
               (import (chezscheme) (testfile-tlp3))
               (define-syntax t2-q (lambda (x) t1-x)))))
        'replace)
      (with-output-to-file "testfile-tlp5.ss"
        (lambda ()
          (pretty-print '(import (chezscheme) (testfile-tlp4)))
          (pretty-print '(pretty-print t2-q)))
        'replace)
      (separate-compile 'compile-library 'tlp3)
      (separate-compile 'compile-library 'tlp4)
      (separate-compile 'compile-program 'tlp5)
      (delete-file "testfile-tlp3.ss")
      (delete-file "testfile-tlp4.ss")
      (delete-file "testfile-tlp3.so")
      (delete-file "testfile-tlp4.so")
      (printf "loading testfile-tlp5.so\n")
      (with-output-to-string
        (lambda ()
          (load-program "testfile-tlp5.so"))))
    "332211\n")
 ; check dependencies returned by compile-program
  (equal?
    (let ()
      (define dep8)
      (with-output-to-file "testfile-tlp6.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-tlp6)
               (export t1-x)
               (import (chezscheme))
               (define t1-x 332211))))
        'replace)
      (with-output-to-file "testfile-tlp7.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-tlp7)
               (export t2-q)
               (import (chezscheme) (testfile-tlp6))
               (define-syntax t2-q (lambda (x) t1-x)))))
        'replace)
      (with-output-to-file "testfile-tlp8.ss"
        (lambda ()
          (pretty-print '(import (chezscheme) (testfile-tlp7)))
          (pretty-print '(pretty-print t2-q)))
        'replace)
      (compile-library "testfile-tlp6")
      (compile-library "testfile-tlp7")
      (set! dep8 (compile-program "testfile-tlp8"))
      (printf "loading testfile-tlp8.so\n")
      (list
        (with-output-to-string
          (lambda ()
            (load-program "testfile-tlp8.so")))
        dep8))
    '("332211\n" ()))
 ; version of the above where program does depend on something
  (equal?
    (let ()
      (define dep8)
      (with-output-to-file "testfile-tlp9.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-tlp9)
               (export t1-x)
               (import (chezscheme))
               (define t1-x (make-parameter 332211)))))
        'replace)
      (with-output-to-file "testfile-tlp10.ss"
        (lambda ()
          (pretty-print
            '(library (testfile-tlp10)
               (export t2-q)
               (import (chezscheme) (testfile-tlp9))
               (define-syntax t2-q (identifier-syntax (t1-x))))))
        'replace)
      (with-output-to-file "testfile-tlp11.ss"
        (lambda ()
          (pretty-print '(import (chezscheme) (testfile-tlp10)))
          (pretty-print '(pretty-print t2-q)))
        'replace)
     ; if we don't let the compilation happen implicitly, the filename
     ; for (testfile-tlp9) doesn't get set
      (parameterize ([compile-imported-libraries #t])
        (set! dep8 (compile-program "testfile-tlp11")))
      (printf "loading testfile-tlp11.so\n")
      (list
        (with-output-to-string
          (lambda ()
            (load-program "testfile-tlp11.so")))
        dep8))
    '("332211\n" ((testfile-tlp9))))
  (path-equal? (library-object-filename '(testfile-tlp9)) "testfile-tlp9.so")
 ; make sure internal module bindings are properly set up before
 ; the body forms are processed
  (begin
    (top-level-program
      (import (chezscheme))
      (module ((a x))
        (define x 3)
        (define-syntax a (identifier-syntax x))
        (putprop 'tlp-spam 'tlp 7))
      a
      (putprop 'tlp-spam 'spam a))
    (and (eqv? (getprop 'tlp-spam 'spam) 3)
         (eqv? (getprop 'tlp-spam 'tlp) 7)
         (remprop 'tlp-spam 'spam)
         (remprop 'tlp-spam 'tlp)
         #t))
  ; make sure we ignore return value(s) of interleaved init expressions
  (equal?
   (with-output-to-string
     (lambda ()
       ; prevent cp0 from fixing the problem
       (parameterize ([run-cp0 (lambda (f x) x)])
         (eval '(top-level-program (import (scheme))
                  (define (f) (printf "hello\n") (values 1 2 3))
                  (f)
                  (define x 'world)
                  (pretty-print x))))))
   "hello\nworld\n")
)

(mat library-meta
  (begin
    (with-output-to-file "testfile-lm-a1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-lm-a1)
             (export a)
             (import (chezscheme))
             (meta define a #'17))))
      'replace)
    (with-output-to-file "testfile-lm-a2.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-lm-a2)
             (export b)
             (import (chezscheme) (testfile-lm-a1))
             (define-syntax b (lambda (q) a)))))
      'replace)
    (for-each separate-compile '(lm-a1 lm-a2))
    #t)
  (equal?
    (let ()
      (import (testfile-lm-a2))
      b)
    17)
  (error? ; attempt to assign unbound variable
    (let ()
      (import (testfile-lm-a1))
      (define-syntax b (lambda (q) (set! a (+ a 1)) a))))
  ; test $visit-library
  (begin
    (with-output-to-file "testfile-lm-b1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-lm-b1)
             (export a)
             (import (chezscheme))
             (meta define a #'17))))
      'replace)
    (with-output-to-file "testfile-lm-b2.ss"
      (lambda ()
        (pretty-print '(import (testfile-lm-b1)))
        (pretty-print '(define-syntax b (lambda (q) a))))
      'replace)
    (for-each separate-compile '(lm-b1 lm-b2))
    #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (parameterize ([trace-output-port (current-output-port)])
          (load "testfile-lm-b2.so"))))
    "")
  (eqv? b 17)
)

(mat library-introspection
  (error? (library-exports 'foo))
  (error? (library-exports '(1 2 3)))
  (error? (library-exports '(probably not a valid loaded library)))
  (error? (library-exports '(probably not a valid loaded library (2 3))))
  (error? (library-exports '(rnrs (six))))
  (error? (library-exports '(rnrs (1))))
  (error? (library-version 'foo))
  (error? (library-version '(1 2 3)))
  (error? (library-version '(probably not a valid loaded library)))
  (error? (library-version '(probably not a valid loaded library ((>= 0)))))
  (error? (library-version '(rnrs (3 . 4))))
  (error? (library-version '(rnrs (1))))
  (error? (library-requirements 'foo))
  (error? (library-requirements '(1 2 3)))
  (error? (library-requirements '(probably not a valid loaded library)))
  (error? (library-requirements '(probably not a valid loaded library (1))))
  (error? (library-requirements '(rnrs (3.0))))
  (error? (library-requirements '(rnrs (1))))
  (error? (library-object-filename 'foo))
  (error? (library-object-filename '(1 2 3)))
  (error? (library-object-filename '(probably not a valid loaded library)))
  (error? (library-object-filename '(probably not a valid loaded library (2 3))))
  (error? (library-object-filename '(rnrs (six))))
  (error? (library-object-filename '(rnrs (1))))

  (error? (library-requirements 'foo (library-requirements-options)))
  (error? (library-requirements '(1 2 3) (library-requirements-options)))
  (error? (library-requirements '(probably not a valid loaded library) (library-requirements-options)))
  (error? (library-requirements '(probably not a valid loaded library (1)) (library-requirements-options)))
  (error? (library-requirements '(rnrs (3.0)) (library-requirements-options)))
  (error? (library-requirements '(rnrs (1)) (library-requirements-options)))

  (enum-set? (library-requirements-options))
  (error? (library-requirements-options . a))
  (error? (library-requirements-options spam))
  (error? (library-requirements-options import spam))

  (error? (library-requirements '(chezscheme) 'import))
  (error? (library-requirements '(chezscheme) '(import)))
  (error? (library-requirements '(chezscheme) '()))

  (begin
    (define set-equal?
      (lambda (s1 s2)
        (and (= (length s1) (length s2))
             (andmap (lambda (x) (member x s2)) s1)
             #t)))
    #t)
  (list? (library-list))
  (andmap list? (library-list))
  (andmap (lambda (x) (andmap symbol? x)) (library-list))
  (begin
    (library (null) (export) (import))
    #t)
  (let ([ls (library-list)])
    (and
      (member '(rnrs) ls)
      (member '(rnrs strings) ls)
      (member '(rnrs io ports) ls)
      (member '(chezscheme) ls)
      (member '(scheme) ls)
      (member '(null) ls))
      #t)
  (null? (library-exports '(null)))
  (set-equal?
    (library-exports '(rnrs mutable-pairs))
    '(set-car! set-cdr!))
  (equal? (sort string<? (map symbol->string (library-exports '(scheme))))
          (sort string<? (map symbol->string (library-exports '(chezscheme)))))
  (equal? (library-version '(rnrs)) '(6))
  (equal? (library-version '(rnrs (6))) '(6))
  (equal? (library-version '(rnrs (or (6) (7)))) '(6))
  (equal? (library-version '(rnrs (or (6) (7)))) '(6))
  (equal? (library-version '(scheme)) '())
  (equal? (library-requirements '(scheme)) '())
  (equal? (library-requirements '(scheme) (library-requirements-options)) '())
  (equal? (library-requirements '(scheme) (library-requirements-options import)) '())
  (equal? (library-requirements '(scheme ())) '())
  (equal? (library-requirements '(rnrs)) '())
  (equal? (library-requirements '(null)) '())
  (not (library-object-filename '(rnrs)))
  (not (library-object-filename '(rnrs (6))))
  (not (library-object-filename '(rnrs (or (6) (7)))))
  (not (library-object-filename '(rnrs (or (6) (7)))))
  (not (library-object-filename '(scheme)))
  (begin
    (library (li1 (3 4 5))
      (export x y)
      (import (chezscheme))
      (define-syntax x (lambda (x) 3))
      (define y (+ x 1)))
    (library (li2 (7 2))
      (export x z w)
      (import (rnrs) (li1 (3)))
      (define z (+ x y))
      (define-syntax w (lambda (q) (* y 2))))
    (library (li2a (7 2))
      (export x z w)
      (import (rnrs) (li1 (3)))
      (define z (+ x x))
      (define-syntax w (lambda (q) (* y 2))))
    #t)
  (and (member '(li1) (library-list))
       (member '(li2) (library-list))
       (member '(li2a) (library-list))
       #t)
  (equal? (library-version '(li1)) '(3 4 5))
  (equal? (library-version '(li2)) '(7 2))
  (equal? (library-version '(li2 ((>= 5)))) '(7 2))
  (equal? (library-version '(li2 (7 (>= 1)))) '(7 2))
  (error? (library-version '(li2 (6))))
  (set-equal? (library-exports '(li1)) '(x y))
  (set-equal? (library-exports '(li2)) '(x z w))
  (set-equal? (library-exports '(li2 ((>= 5)))) '(x z w))
  (set-equal? (library-exports '(li2 (7 (>= 1)))) '(x z w))
  (error? (library-exports '(li2 (6))))
  (not (library-object-filename '(li1)))
  (not (library-object-filename '(li2)))
  (not (library-object-filename '(li2 ((>= 5)))))
  (not (library-object-filename '(li2 (7 (>= 1)))))
  (error? (library-exports '(li2 (6))))
  (set-equal?
    (library-requirements '(li1))
    '((chezscheme)))
  (set-equal?
    (library-requirements '(li2 ((>= 7))))
    '((rnrs (6)) (li1 (3 4 5))))
  (set-equal?
    (library-requirements '(li2))
    '((rnrs (6)) (li1 (3 4 5))))
  (set-equal?
    (library-requirements '(li2) (library-requirements-options import))
    '((rnrs (6)) (li1 (3 4 5))))
  (set-equal?
    (library-requirements '(li2) (library-requirements-options visit@visit))
    '())
  (set-equal?
    (library-requirements '(li2) (library-requirements-options invoke@visit))
    '((li1 (3 4 5))))
  (set-equal?
    (library-requirements '(li2) (library-requirements-options invoke))
    '((li1 (3 4 5))))
  (error? (library-requirements '(li2 (6))))
  (set-equal?
    (library-requirements '(li2a))
    '((rnrs (6)) (li1 (3 4 5))))
  (set-equal?
    (library-requirements '(li2a) (library-requirements-options import))
    '((rnrs (6)) (li1 (3 4 5))))
  (set-equal?
    (library-requirements '(li2a) (library-requirements-options visit@visit))
    '())
  (set-equal?
    (library-requirements '(li2a) (library-requirements-options invoke@visit))
    '((li1 (3 4 5))))
  (set-equal?
    (library-requirements '(li2a) (library-requirements-options invoke))
    '())
  (equal?
    (let ()
      (import (li1) (li2))
      (list x y z w))
    '(3 4 7 8))
 ; make sure requirements haven't changed just because we used the exports
  (set-equal?
    (library-requirements '(li1))
    '((chezscheme)))
  (set-equal?
    (library-requirements '(li2))
    '((rnrs (6)) (li1 (3 4 5))))
  (begin
    (define-syntax $li-a
      (syntax-rules ()
        [(_ name a p)
         (begin
           (library name (export a y) (import (rnrs))
             (define-syntax a (identifier-syntax (cons y 1)))
             (define y 'hello))
           (define p (lambda () (import name) y)))]))
    ($li-a ($li-spam) q $li-get-y)
    #t)
  (eq? ($li-get-y) 'hello)
  (equal? (let () (import ($li-spam)) q) '(hello . 1))
  (eqv? (let ([y 75]) (import ($li-spam)) y) 75)
  (begin
    (with-output-to-file "testfile-li3.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-li3)
             (export x)
             (import (rnrs))
             (define x 3))))
      'replace)
    (with-output-to-file "testfile-li4.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-li4)
             (export x)
             (import (rnrs))
             (define x 3))))
      'replace)
    (with-output-to-file "testfile-li5.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-li5)
             (export x)
             (import (rnrs))
             (define x 3))))
      'replace)
    (separate-compile 'li5)
    #t)
  (path-equal?
    (parameterize ([compile-imported-libraries #t])
      (eval '(let () (import (testfile-li3)) x))
      (library-object-filename '(testfile-li3)))
     "testfile-li3.so")
  (equal?
    (parameterize ([compile-imported-libraries #f])
      (eval '(let () (import (testfile-li4)) x))
      (library-object-filename '(testfile-li4)))
    #f)
  (path-equal?
    (begin
      (eval '(let () (import (testfile-li5)) x))
      (library-object-filename '(testfile-li5)))
    "testfile-li5.so")
  (equal?
    (begin
      (load-library "testfile-li3.ss")
      (library-object-filename '(testfile-li3)))
    #f)
  (equal?
    (begin
      (load-library "testfile-li3.so")
      (library-object-filename '(testfile-li3)))
    "testfile-li3.so")
  (begin
    (define (test-library-info do-load file)
      (separate-eval
       `(,do-load ,file)
       ;; no import here
       '(begin
          (define (show f)
            (printf "~s: " f)
            (guard (e [else (display-condition e) (newline)])
              (printf "~s\n" ((eval f) '(testfile-li3)))))
          (show 'library-object-filename)
          (show 'library-requirements)
          (show 'library-version)
          (show 'library-exports))
       ;; now import
       '(import (testfile-li3))
       '(show 'library-exports)))
    #t)
  (equal?
   (test-library-info 'load-library "testfile-li3.ss")
   (string-append
    "library-object-filename: #f\n"
    "library-requirements: ((rnrs (6)))\n"
    "library-version: ()\n"
    "library-exports: (x)\n"
    "library-exports: (x)\n"))
  (equal?
   (test-library-info 'load-library "testfile-li3.so")
   (string-append
    "library-object-filename: \"testfile-li3.so\"\n"
    "library-requirements: ((rnrs (6)))\n"
    "library-version: ()\n"
    "library-exports: (x)\n"
    "library-exports: (x)\n"))
  (equal?
   (test-library-info 'visit "testfile-li3.so")
   (string-append
    "library-object-filename: \"testfile-li3.so\"\n"
    "library-requirements: Exception: run-time information for library (testfile-li3) has not been loaded\n"
    "library-version: ()\n"
    "library-exports: (x)\n"
    "library-exports: (x)\n"))
  (equal?
   (test-library-info 'revisit "testfile-li3.so")
   (string-append
    "library-object-filename: \"testfile-li3.so\"\n"
    "library-requirements: Exception: compile-time information for library (testfile-li3) has not been loaded\n"
    "library-version: ()\n"
    "library-exports: Exception: compile-time information for library (testfile-li3) has not been loaded\n"
    "library-exports: (x)\n"))
)

(mat rnrs-eval
  (begin
    (define $eval-e1 (environment '(rnrs)))
    (environment? $eval-e1))
  (error? ; variable environment not bound
    (r6rs:eval 'environment $eval-e1))
  (error? ; variable eval not bound
    (r6rs:eval 'eval $eval-e1))
  (eq? (r6rs:eval 'cons $eval-e1) cons)
  (error? ; invalid context for definition
    (r6rs:eval '(define x 4) $eval-e1))
  (error? ; invalid context for definition
    (r6rs:eval '(define foo 4) $eval-e1))
  (error? ; cannot assign cons
    (r6rs:eval '(set! cons 4) $eval-e1))
  (error? ; cannot assign foo
    (r6rs:eval '(set! foo 4) $eval-e1))
  (begin
    (with-output-to-file "testfile-eval1.ss"
      (lambda ()
        (pretty-print
          '(library (testfile-eval1)
             (export canned spam list define quote set!)
             (import (rnrs))
             (define-syntax canned
               (begin
                 (display "testfile-eval1 visit")
                 (newline)
                 (identifier-syntax tuna)))
             (define spam (lambda () (cons 'not canned)))
             (define tuna 'yummy)
             (display "testfile-eval1 invoke")
             (newline))))
      'replace)
    #t)
  (equal?
    (r6rs:eval
      '(list canned (spam))
      (environment '(testfile-eval1)))
    '(yummy (not . yummy)))
  (error? ; cons is not bound
    (r6rs:eval
      '(cons canned (spam))
      (environment '(testfile-eval1))))
  (error? ; invalid context for definition
    (r6rs:eval
      '(define foo 3)
      (environment '(testfile-eval1))))
  (error? ; cannot assign
    (r6rs:eval
      '(set! spam 3)
      (environment '(testfile-eval1))))
  (error? ; cannot assign
    (r6rs:eval
      '(set! foo 3)
      (environment '(testfile-eval1))))
  (error? ; invalid definition in immutable environment
    (let ([env (environment '(testfile-eval1))])
      (eval `(define cons ',vector) env)))
  (equal?
    (let ([env (copy-environment (environment '(testfile-eval1)))])
      (eval `(define cons ',vector) env)
      (r6rs:eval '(cons canned (spam)) env))
    '#(yummy (not . yummy)))
  (eq?
    (r6rs:eval '(let () (import (scheme)) compile)
      (environment '(only (scheme) let import)))
    compile)
)

(mat top-level-syntax-functions
  (error? (top-level-syntax "hello"))
  (error? (top-level-syntax))
  (error? (top-level-syntax 'hello 'hello))
  (error? (top-level-syntax (scheme-environment) (scheme-environment)))
  (error? (top-level-syntax? "hello"))
  (error? (top-level-syntax?))
  (error? (top-level-syntax? 'hello 'hello))
  (error? (top-level-syntax? (scheme-environment) (scheme-environment)))
  (error? (define-top-level-syntax "hello" "hello"))
  (error? (define-top-level-syntax))
  (error? (define-top-level-syntax 15))
  (error? (define-top-level-syntax 'hello 'hello 'hello))
  (error? (define-top-level-syntax (scheme-environment) (scheme-environment) (scheme-environment)))
  (error?
    (let ([e (scheme-environment)])
      (define-top-level-syntax 'p (lambda (x) "hello") e)))
  (error?
    (let ([e (copy-environment (scheme-environment) #f)])
      (define-top-level-syntax 'p void e)))
  (error?
    (let ([e (scheme-environment)])
      (top-level-syntax 'p e)))
  (and (top-level-syntax 'hopenotdefined) #t)
  (and (top-level-syntax 'cons) #t)
  (and (top-level-syntax 'scheme) #t)
  (error? (top-level-syntax 'cond (environment)))
  (top-level-syntax? 'hopenotdefined)
  (top-level-syntax? 'cons)
  (top-level-syntax? 'scheme)
  (not (top-level-syntax? 'cond (environment)))

  (top-level-syntax? 'cond)
  (procedure? (top-level-syntax 'cond))

  (begin
    (define-top-level-syntax '$tls-foo (syntax-rules () [(_ x) (x x)]))
    #t)
  (equal? ($tls-foo list) `(,list))

  (equal?
    (parameterize ([interaction-environment
                    (copy-environment (scheme-environment) #t)])
      (let ([t (syntax-rules () [(_ x y) (* x y)])])
        (eval `(define-syntax cons ',t))
        (eval '(cons 3 4))))
    12)
  (equal?
    (let ([e (environment '(only (scheme) cond))])
      (list
        (top-level-syntax? 'cond e)
        (eq? (top-level-syntax 'cond e) (top-level-syntax 'cond (scheme-environment)))
        (top-level-syntax? 'cdr e)))
    '(#t #t #f))
  (equal?
    (let ([e (copy-environment (environment) #t)])
      (let ([t1 (lambda (x) 17)] [t2 (syntax-rules () [(_ x y) (list y x)])])
        (define-top-level-syntax 'p t1 e)
        (define-top-level-syntax 'q t2 e)
        (list
          (top-level-syntax? 'p e)
          (top-level-syntax? 'q e)
          (top-level-syntax? 'r e)
          (eq? (top-level-syntax 'p e) t1)
          (eq? (top-level-syntax 'q e) t2)
          ((top-level-syntax 'p e) 'p)
          (eval '(q 3 4) e)
          (eval 'p e))))
    '(#t #t #t #t #t 17 (4 3) 17))
 )

(mat annotations
  (source-file-descriptor?
    (make-source-file-descriptor #f ; anything is allowed as a path
      (open-bytevector-input-port (string->utf8 "hello"))))
  (error? ; 17 is not a binary-input port
    (make-source-file-descriptor "foo" 17))
  (error? ; #<input port stdin> is not a binary-input port
    (make-source-file-descriptor "foo" (open-string-input-port "oops")))
  (error? ; #<binary input port> does not support port-position and set-port-position!
    (make-source-file-descriptor "foo"
      (make-custom-binary-input-port "foo" (lambda (bv s c) 0) #f #f #f)
      #t))
  (begin
    (define str "(ugh (if \x3b2;))")
    (define bv (string->utf8 str))
    (define ip (open-bytevector-input-port bv))
    (define sfd (make-source-file-descriptor "foo" ip #t))
    #t)
  (not (= (bytevector-length bv) (string-length str)))
  (error? ; sfd is not an sfd
    (make-source-object 'sfd 2 3))
  (error? ; two is not an exact integer
    (make-source-object sfd 'two 3))
  (error? ; three is not an exact integer
    (make-source-object sfd 2 'three))
  (error? ; bfp 3 is not between 0 and efp 2
    (make-source-object sfd 3 2))
  (error? ; bfp -7 not between 0 and efp -3
    (make-source-object sfd -7 -3))
  (error? ; bfp -7 is not between 0 and efp 3
    (make-source-object sfd -7 3))
  (error? ; bfp -7 is not between 0 and efp 3
    (make-source-object sfd -7 3 2 1))
  (error? ; one is not an exact integer
    (make-source-object sfd 1 2 'one 1))
  (error? ; one is not an exact integer
    (make-source-object sfd 1 2 1 'one))
  (error? ; zero is not an exact positive integer
    (make-source-object sfd 1 2 0 1))
  (error? ; zero is not an exact positive integer
    (make-source-object sfd 1 2 1 0))
  (error? ; bfp 3 is not between 0 and efp 2
    (make-source-object sfd 3 2 1 1))
  (begin
    (define source (make-source-object sfd 2 3))
    (define source-at-line-two (make-source-object sfd 3 5 2 1))
    #t)
  (error? ; source is not a source object
    (make-annotation #f 'source #f))
  (begin
    (define a (make-annotation '(if 3) source '(if I were a rich man)))
    (define a-at-line-two (make-annotation '(if 3) source-at-line-two '(if I were a rich man)))
    (define x (datum->syntax #'* a))
    #t)
  (source-file-descriptor? sfd)
  (not (source-file-descriptor? source))
  (source-object? source)
  (source-object? source-at-line-two)
  (not (source-object? sfd))
  (not (source-object? a))
  (annotation? a)
  (not (annotation? source))
  (error? ; #<source> is not an sfd
    (source-file-descriptor-path source))
  (error? ; #<annotation> is not an sfd
    (source-file-descriptor-checksum a))
  (error? ; #<sfd> is not a source object
    (source-object-sfd sfd))
  (error? ; #<annotation> is not a source object
    (source-object-bfp a))
  (error? ; 3 is not a source object
    (source-object-efp 3))
  (error? ; 3 is not a source object
    (source-object-line 3))
  (error? ; 3 is not a source object
    (source-object-column 3))
  (error? ; 3 is not an annotation
    (annotation-expression 3))
  (error? ; #<source> is not an annotation
    (annotation-stripped source))
  (error? ; #<sfd> is not an annotation
    (annotation-source sfd))
  (error? ; #<source> is not an annotation
    (annotation-option-set source))
  (error? ; invalid syntax
    (annotation-options . debug))
  (error? ; invalid syntax
    (annotation-options 3 profile))
  (error? ; invalid option
    (annotation-options fig))
  (error? ; invalid option
    (annotation-options debug fig))
  (error? ; invalid option
    (annotation-options fig profile))
  (equal?
    (source-file-descriptor-path sfd)
    "foo")
  (number? (source-file-descriptor-checksum sfd))
  (eq? (source-object-sfd source) sfd)
  (eq? (source-object-bfp source) 2)
  (eq? (source-object-efp source) 3)
  (eq? (source-object-line source) #f)
  (eq? (source-object-column source) #f)
  (eq? (source-object-sfd source) sfd)
  (eq? (source-object-bfp source-at-line-two) 3)
  (eq? (source-object-efp source-at-line-two) 5)
  (eq? (source-object-line source-at-line-two) 2)
  (eq? (source-object-column source-at-line-two) 1)
  (equal? (annotation-expression a) '(if 3))
  (eq? (annotation-source a) source)
  (equal? (annotation-stripped a) '(if I were a rich man))
  (enum-set=? (annotation-option-set a) (annotation-options debug profile))
  (enum-set=?
    (annotation-option-set (make-annotation '(if 3) source '(if I were a rich man) (annotation-options)))
    (annotation-options))
  (enum-set=?
    (annotation-option-set (make-annotation '(if 3) source '(if I were a rich man) (annotation-options debug)))
    (annotation-options debug))
  (enum-set=?
    (annotation-option-set (make-annotation '(if 3) source '(if I were a rich man) (annotation-options profile)))
    (annotation-options profile))
  (enum-set=?
    (annotation-option-set (make-annotation '(if 3) source '(if I were a rich man) (annotation-options debug profile)))
    (annotation-options debug profile))
  (enum-set=?
    (annotation-option-set (make-annotation '(if 3) source '(if I were a rich man) (annotation-options profile debug)))
    (annotation-options debug profile))
  (eq? (syntax->annotation x) a)
  (not (syntax->annotation #'(a b c)))
  (not (syntax->annotation '(a b c)))
  (not (syntax->annotation #f))
  (error? ; invalid syntax (if I were a rich man) at char 2 of foo
    (expand a))
  (error? ; invalid syntax (if I were a rich man) at line 2, char 1 of foo
    (expand a-at-line-two))
  (error? ; invalid syntax (if I were a rich man) at char 2 of foo
    (eval a))
  (error? ; invalid syntax (if I were a rich man) at char 2, char 1 of foo
    (eval a-at-line-two))
  (error? ; invalid syntax (if I were a rich man) at char 2 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* a))) foo)))
  (error? ; invalid syntax (if I were a rich man) at line 2, char 1 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* a-at-line-two))) foo)))
  (error? ; invalid syntax (if I were a rich man) at char 2 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* (make-annotation '(if 3) source '(if I were a rich man) (annotation-options debug profile))))) foo)))
  (error? ; invalid syntax (if I were a rich man) at line 2, char 1 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* (make-annotation '(if 3) source-at-line-two '(if I were a rich man) (annotation-options debug profile))))) foo)))
  (error? ; invalid syntax (if I were a rich man) at char 2 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* (make-annotation '(if 3) source '(if I were a rich man) (annotation-options debug))))) foo)))
  (error? ; invalid syntax (if I were a rich man)
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* (make-annotation '(if 3) source '(if I were a rich man) (annotation-options profile))))) foo)))
  (error? ; invalid syntax (if I were a rich man)
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* (make-annotation '(if 3) source '(if I were a rich man) (annotation-options))))) foo)))
  (error? ; invalid argument count in call (f) at char 2 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* `(let ([f (lambda (x) x)]) ,(make-annotation '(f) source '(f) (annotation-options debug profile)))))) foo)))
  (error? ; invalid argument count in call (f) at line 2, char 1 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* `(let ([f (lambda (x) x)]) ,(make-annotation '(f) source-at-line-two '(f) (annotation-options debug profile)))))) foo)))
  (error? ; invalid argument count in call (f) at char 2 of foo
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* `(let ([f (lambda (x) x)]) ,(make-annotation '(f) source '(f) (annotation-options debug)))))) foo)))
  (error? ; invalid argument count in call (f)
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* `(let ([f (lambda (x) x)]) ,(make-annotation '(f) source '(f) (annotation-options profile)))))) foo)))
  (error? ; invalid argument count in call (f)
    (eval '(let () (define-syntax foo (lambda (z) (datum->syntax #'* `(let ([f (lambda (x) x)]) ,(make-annotation '(f) source '(f) (annotation-options)))))) foo)))
  (begin
    (profile-clear)
    #t)
  (begin
    (define foo
      (parameterize ([compile-profile #t] [current-eval compile])
        (eval '(lambda ()
                 (define-syntax foo
                   (lambda (z)
                     (datum->syntax #'*
                       (make-annotation
                         `(,(make-annotation '+ (make-source-object sfd 2 3) '+ (annotation-options debug profile))
                            ,(make-annotation '3 (make-source-object sfd 4 5) '3 (annotation-options))
                            ,(make-annotation '44 (make-source-object sfd 8 10) '44 (annotation-options debug)))
                         (make-source-object sfd 1 11)
                         '(+ 3 44)
                         (annotation-options profile)))))
                 foo))))
    #t)
  (equal? (foo) 47)
  (equal?
    (let ([ls (profile-dump-list)])
      (vector
        (find (lambda (x) (equal? (list-head (cdr x) 3) '("foo" 1 11))) ls)
        (find (lambda (x) (equal? (list-head (cdr x) 3) '("foo" 2 3))) ls)
        (find (lambda (x) (equal? (list-head (cdr x) 3) '("foo" 4 5))) ls)
        (find (lambda (x) (equal? (list-head (cdr x) 3) '("foo" 8 10))) ls)))
    '#((1 "foo" 1 11 #f #f)
       (1 "foo" 2 3 #f #f)
       #f
       #f))
  (begin
    (profile-clear)
    #t)
  (begin
    (define ip (transcoded-port ip (native-transcoder)))
    (define-values (x fp) (get-datum/annotations ip sfd 0))
    #t)
  (error? ; #<sfd> is not a textual input port
    (get-datum/annotations sfd sfd 0))
  (error? ; #<input port> is not an sfd
    (get-datum/annotations ip ip 0))
  (error? ; #<sfd> is not a valid file position
    (get-datum/annotations ip sfd sfd))
  (error? ; -5 is not a valid file position
    (get-datum/annotations ip sfd -5))
  (error? ; 5.0 is not a valid file position
    (get-datum/annotations ip sfd 5.0))
  (eqv? fp (string-length str))
  (annotation? x)
  (equal? (annotation-stripped x) (with-input-from-string str read))
  (equal?
    (let f ([x x])
      (and (annotation? x)
           (let ([x (annotation-expression x)])
             (if (list? x)
                 (map f x)
                 x))))
    (with-input-from-string str read))
  (begin
    (define source (annotation-source x))
    #t)
  (source-object? source)
  (eq? (source-object-sfd source) sfd)
  (eqv? (source-object-bfp source) 0)
  (eqv? (source-object-efp source) (string-length str))
  (source-file-descriptor?
    (source-file-descriptor 'spam 0))
  (error? ; not an exact nonnegative integer
    (source-file-descriptor "spam" -1))
  (error? ; not an exact nonnegative integer
    (source-file-descriptor "spam" 1.0))
  (source-file-descriptor? (source-file-descriptor "spam" #x34534a5))
  (source-file-descriptor? (source-file-descriptor "spam" #x20333333333339999999997834443333337))
  (equal?
    (source-file-descriptor-path (source-file-descriptor "spam" #x20333333333339999999997834443333337))
    "spam")
  (equal?
    (source-file-descriptor-checksum (source-file-descriptor "spam" #x20333333333339999999997834443333337))
    #x20333333333339999999997834443333337)
  (error? ; not an sfd
    (locate-source "spam" 17))
  (error? ; not an exact nonnegative integer
    (locate-source sfd -1))
  (error? ; not an exact nonnegative integer
    (locate-source sfd 'a))
  (let-values ([() (locate-source sfd 7)]) #t)
  (let-values ([() (locate-source (source-file-descriptor 'something-else 0) 7)]) #t)
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (printf "; bogus exports\n")
        (printf "(module (a 3)\n")
        (printf "  (define a 3))\n"))
      'replace)
    #t)
  (equal?
    (guard (c [(syntax-violation? c)
               (let* ([form (syntax-violation-form c)]
                      [annotation (syntax->annotation form)]
                      [source (annotation-source annotation)])
                 (cons
                   (call-with-values (lambda () (locate-source (source-object-sfd source) (source-object-bfp source))) vector)
                   (call-with-values (lambda () (locate-source (source-object-sfd source) (source-object-efp source))) vector)))])
      (load "testfile.ss"))
    (cons (vector (find-source "testfile.ss") 2 12) (vector (find-source "testfile.ss") 2 13)))
  (equal?
    (let ([sfd (source-file-descriptor (source-file-descriptor-path sfd) (source-file-descriptor-checksum sfd) )])
      (let ([source (make-source-object sfd 2 3)])
        (guard (c [(syntax-violation? c)
                   (let* ([form (syntax-violation-form c)]
                          [annotation (syntax->annotation form)]
                          [source (annotation-source annotation)])
                     (cons
                       (call-with-values (lambda () (locate-source (source-object-sfd source) (source-object-bfp source))) vector)
                       (call-with-values (lambda () (locate-source (source-object-sfd source) (source-object-efp source))) vector)))])
          (load "testfile.ss"))))
    (cons (vector (find-source "testfile.ss") 2 12) (vector (find-source "testfile.ss") 2 13)))

  (error? ; not a source object
   (locate-source-object-source "spam" #t #t))
  (error?
   (current-locate-source-object-source 7))
  (error?
   (current-locate-source-object-source "string"))
  (error? ; not a source object
   ((current-locate-source-object-source) "spam" #t #t))
  (error? ; invalid syntax (if I were a rich man) at line 200, char 17 of foo
   (parameterize ([current-locate-source-object-source
                   (lambda (src start? cache?)
                     (values (source-file-descriptor-path (source-object-sfd src)) 200 17))])
     (expand a)))
 )

(mat annotations-via-recorded-lines
  (error?
   (current-make-source-object 7))
  (error?
   (current-make-source-object "string"))
  (begin
    (define sfd-with-lines
      (let ((op (open-output-file "testfile.ss" 'replace)))
        (display "apple\n  banana\ncoconut" op)
        (close-port op)
        (let* ([ip (open-file-input-port "testfile.ss")]
               [sfd (make-source-file-descriptor "testfile.ss" ip)])
          (close-port ip)
          sfd)))
    (define input-string-with-lines "Apple\n  Banana\nCoconut\nMore")
    (define input-port-with-lines (open-string-input-port input-string-with-lines))
    (define input-port-with-line-pos 0)
    (define (make-make-source-object/get-lines expected-sfd)
      (lambda (sfd bfp efp)
        (if (eq? sfd expected-sfd)
            ;; Gather line and column now:
            (let-values ([(path line col) (locate-source sfd bfp #t)])
              (make-source-object sfd bfp efp line col))
            (error 'recording-make-source-object "reading some other file?"))))
    (define (read-with-lines)
      (parameterize ([current-make-source-object (make-make-source-object/get-lines sfd-with-lines)])
        (let-values ([(v pos) (get-datum/annotations input-port-with-lines sfd-with-lines input-port-with-line-pos)])
          (set! input-port-with-line-pos pos)
          v)))
    #t)
  (begin
    (define line-one (read-with-lines))
    (annotation? line-one))
  (equal? (annotation-stripped line-one) 'Apple)
  (equal? (source-object-bfp (annotation-source line-one)) 0)
  (equal? (source-object-line (annotation-source line-one)) 1)
  (equal? (source-object-column (annotation-source line-one)) 1)
  (begin
    (define line-two (read-with-lines))
    (annotation? line-two))
  (equal? (source-object-bfp (annotation-source line-two)) 8)
  (equal? (source-object-line (annotation-source line-two)) 2)
  (equal? (source-object-column (annotation-source line-two)) 3)
  (begin
    (define line-three (read-with-lines))
    (annotation? line-three))
  (equal? (source-object-bfp (annotation-source line-three)) 15)
  (equal? (source-object-line (annotation-source line-three)) 3)
  (equal? (source-object-column (annotation-source line-three)) 1)
  (annotation? (read-with-lines)) ; 'More
  (eof-object? (read-with-lines))

  ;; Make sure lines are calculated right with input that is longer than
  ;; the file buffer size:
  (begin
    (define input-string-with-lines (string-append
                                     "\""
                                     (make-string (* 2 (file-buffer-size)) #\a)
                                     "\""
                                     "\nend"))

    (define input-port-with-lines (open-string-input-port input-string-with-lines))
    (define sfd-with-lines
      (let ((op (open-output-file "testfile.ss" 'replace)))
        (display input-string-with-lines op)
        (close-port op)
        (let* ([ip (open-file-input-port "testfile.ss")]
               [sfd (make-source-file-descriptor "testfile.ss" ip)])
          (close-port ip)
          sfd)))
    (define input-port-with-line-pos 0)
    (define (read-with-lines)
      (parameterize ([current-make-source-object (make-make-source-object/get-lines sfd-with-lines)])
        (let-values ([(v pos) (get-datum/annotations input-port-with-lines sfd-with-lines input-port-with-line-pos)])
          (set! input-port-with-line-pos pos)
          v)))
    (define line-one (read-with-lines))
    (annotation? line-one))
  (string? (annotation-stripped line-one))
  (begin
    (define line-two (read-with-lines))
    (annotation? line-two))
  (equal? (source-object-line (annotation-source line-two)) 2)
  (equal? (source-object-column (annotation-source line-two)) 1)
  )

(mat locate-source-caching
  (begin
   (define (make-expr n)
     `(let ()
        ,@(let loop ([i n])
            (if (zero? i)
                '(#t)
                (cons
                 `(let-values ([(x y z) (values 1 2 3)]) x)
                 (loop (sub1 i)))))))

   (define (time-expr n)
     (with-output-to-file "testfile.ss"
       (lambda ()
         (pretty-print (make-expr n)))
       'truncate)
     (collect)
     (parameterize ([collect-request-handler void])
       (let ([start (current-time 'time-process)])
         (let loop ([iters 1])
           (load "testfile.ss" expand)
           (let ([delta (time-difference (current-time 'time-process) start)])
             (let ([n (/ (+ (* #e1e9 (time-second delta))
                            (time-nanosecond delta))
                         iters)])
               (if (zero? n)
                   ;; more iterations to get a non-0 time
                   (loop (add1 iters))
                   n)))))))

   (let loop ([tries 3])
     (when (zero? tries)
       (error 'source-cache-test "loading lots of `let-values` forms seems to take too long"))
     (let ([t1000 (time-expr 1000)] [t10000 (time-expr 10000)])
       (or (> (* 20 t1000) t10000)
         (begin
           (printf "t1000 = ~s, t10000 = ~s, t10000 / t1000 = ~s\n" t1000 t10000 (inexact (/ t10000 t1000)))
           (loop (sub1 tries)))))))

  (begin
    (define sfd-to-cache
      (let ((op (open-output-file "testfile.ss" 'replace)))
        (display "apple\n  banana\ncoconut" op)
        (close-port op)
        (let* ([ip (open-file-input-port "testfile.ss")]
               [sfd (make-source-file-descriptor "testfile.ss" ip)])
          (close-port ip)
          sfd)))

    (equal? (call-with-values
             (lambda () (locate-source sfd-to-cache 8 #t))
             (case-lambda
              [(name line col) (list line col)]))
            '(2 3)))

  (begin
    (let ((op (open-output-file "testfile.ss" 'replace)))
      (display "1\n2\n3\n4\n5\n6789" op)
      (close-port op))
    ;; Cache may report the old source line,
    ;; or uncached should report no line:
    (equal? (call-with-values
             (lambda () (locate-source sfd-to-cache 8 #t))
             (case-lambda
              [() '(2 3)] ; report no line same as expected cache
              [(name line col) (list line col)]))
            '(2 3)))

  ;; An uncached lookup defniitely reports no line:
  (equal? (call-with-values
            (lambda () (locate-source sfd-to-cache 8 #f))
            (lambda () 'none))
          'none)

  (begin
    (collect (collect-maximum-generation))
    ;; After collecting the maximum generation, the
    ;; cached information should definitely be gone:
    (equal? (call-with-values
             (lambda () (locate-source sfd-to-cache 8 #t))
             (lambda () 'gone))
            'gone))
  )

(mat include
  (error? ; invalid syntax
    (expand '(include spam)))
  (error? ; invalid syntax
    (parameterize ([source-directories '("../s" "../c")])
      (expand '(include spam))))
 )

(mat extend-syntax
   (begin (extend-syntax (foo)
             [(foo a b) (list a b)])
          #t)
   (equal? (foo 3 4) '(3 4))
   (begin (extend-syntax (foo bar)
             [(foo) '()]
             [(foo (bar x)) x]
             [(foo x) (cons x '())]
             [(foo x y ...) (cons x (foo y ...))])
          #t)
   (equal? (foo 'a 'b 'c 'd) '(a b c d))
   (equal? (foo 'a 'b 'c (bar 'd)) '(a b c . d))
   (begin (extend-syntax (foo)
             [(foo ((x v) ...) e1 e2 ...)
              (andmap symbol? '(x ...))
              ((lambda (x ...) e1 e2 ...) v ...)]
             [(foo ((lambda (x ...) e1 e2 ...) v ...))
              (= (length '(x ...)) (length '(v ...)))
               (foo ((x v) ...) e1 e2 ...)])
          #t)
   (equal? (foo ((a 3) (b 4)) (cons a b)) '(3 . 4))
   (error? (extend-syntax (foo ...) [(foo ...) 0]))
   (error? (extend-syntax (foo) [(foo x ... y) 0]))
   (error? (extend-syntax (foo) [(foo x . ...) 0]))
   (error? (extend-syntax (foo) [(foo (...)) 0]))
   (error? (extend-syntax (foo) [(foo x x) 0]))
   (begin (extend-syntax (foo) [(foo foo) 0]) #t)
   (begin (extend-syntax (foo) [(foo keys) (with ([x `,'keys]) 'x)])
          (equal? (foo (a b c)) '(a b c)))
   (begin (extend-syntax (foo) [(foo x y) '`(x ,@y)])
          (equal? (foo a b) '`(a ,@b)))
   (begin (extend-syntax (foo) ; test exponential "with" time problem
             [(foo)
              (with ([a1 1] [b1 2] [c1 3] [d1 4] [e1 5] [f1 6] [g1 7] [h1 8]
                     [a2 1] [b2 2] [c2 3] [d2 4] [e2 5] [f2 6] [g2 7] [h2 8]
                     [a3 1] [b3 2] [c3 3] [d3 4] [e3 5] [f3 6] [g3 7] [h3 8]
                     [a4 1] [b4 2] [c4 3] [d4 4] [e4 5] [f4 6] [g4 7] [h4 8]
                     [a5 1] [b5 2] [c5 3] [d5 4] [e5 5] [f5 6] [g5 7] [h5 8]
                     [a6 1] [b6 2] [c6 3] [d6 4] [e6 5] [f6 6] [g6 7] [h6 8]
                     [a7 1] [b7 2] [c7 3] [d7 4] [e7 5] [f7 6] [g7 7] [h7 8]
                     [a8 1] [b8 2] [c8 3] [d8 4] [e8 5] [f8 6] [g8 7] [h8 8])
                 '(a1 b2 c3 d4 e5 f6 g7 h8))])
      (equal? (foo) '(1 2 3 4 5 6 7 8)))
   (equal? (letrec* ((x 3) (y (+ x 2))) (list x y)) '(3 5))
 )

(mat with
   (begin (extend-syntax (foo)
             [(foo x ...)
              (with ([n (length '(x ...))])
                 (list n 'x ...))])
          #t)
   (equal? (foo 3 2 1) '(3 3 2 1))
   (begin (extend-syntax (foo)
             [(foo (x ...) ...)
              (list (with ([(y ...)
                            '(x ... (with ([n (length '(x ...))]) n))])
                       (with ([(z ...) (reverse '(y ...))])
                          (list 'z ...)))
                    ...)])
          #t)
   (equal? (foo) '())
   (equal? (foo (a b) (c d e)) '((2 b a) (3 e d c)))
   (begin (extend-syntax (foo)
             [(foo x ...)
              (with ([(y1 y2 ...) '(x ...)])
                 (with ([(z1 z2) 'y1])
                    '(z2 z1)))])
          #t)
   (equal? (foo (a b) (c d) (e f)) '(b a)))

(mat quote-syntax
   (equal? (syntax->datum (quote-syntax (a b c))) '(a b c))
   (equal? (syntax->datum (quote-syntax ...)) '...)
   (identifier? (quote-syntax if))
   (let* ([sfd (make-source-file-descriptor "src" (open-bytevector-input-port '#vu8()))]
          [src (make-source-object sfd 1 2)])
     (identifier? (eval
                   (make-annotation `(quote-syntax ,(make-annotation 'if src 'if))
                                    src
                                    '(quote-syntax if)))))
   (bound-identifier=? (quote-syntax ...) (syntax (... ...)))
   (free-identifier=? (quote-syntax ...) (syntax (... ...)))
   (equal? (with-syntax ([x #'y]) (syntax->datum (quote-syntax x))) 'x)
   (let-syntax ([m (lambda (stx)
                     (syntax-case stx ()
                       [(_ x) #'(and (equal? (syntax->datum (quote-syntax x)) '+)
                                     (free-identifier=? (syntax x) #'-))]))])
     (with-syntax ([+ #'-]) (m +)))
   (error? ; invalid syntax
     (quote-syntax))
   (error? ; invalid syntax
     (quote-syntax a b)))
