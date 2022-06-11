;;; oop
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

(mat oop
  (begin
    (import (oop))
    (define-syntax seq-list
      (lambda (x)
        (import (only scheme list))
        (syntax-case x ()
          [(_ e ...)
           (with-syntax ([(t ...) (generate-temporaries #'(e ...))])
             #'(let* ([t e] ...) (list t ...)))])))
    (define true? (lambda (x) (eq? x #t)))
    #t)
  (begin
    (define-class (<a> a1) (<root>)
      (ivars [x1 a1])
      (methods
        [m1 (q) (list self x1 q)]
        [m2 () x1]))
     #t)
  (error? ; incorrect argument count
    (make-<a>))
  (error? ; incorrect argument count
    (make-<a> 1 2))
  (begin
    (define i1 (make-<a> 3))
    #t)
  (equal? (cdr (m1 i1 4)) '(3 4))
  (eq? (car (m1 i1 4)) i1)
  (error? ; incorrect argument count
    (m1 i1))
  (error? ; incorrect argument count
    (m1 i1 4 5))
  (error? ; m1 not applicable to 17
    (m1 17 4))
  (error? ; not bound
    (<a>-x1 i1))
  (error? ; not bound
    (<a>-x1-set! i1 17))

  ; no longer an error to duplicate x1
  (begin
    (define x1 'outer-x1)
    (define x3 'outer-x3)
    (define-class (<b> b1 b2) (<a> (+ b1 b2))
      (ivars [x1 b1] [x2 b2])
      (methods
        [m1 (q) (vector self x1 q)]
        [m3 (s t) (list s t x1 x2)]
        [m4 () x3]))
    (define i2 (make-<b> 10 4))
    #t
  )
  (equal? (m2 i2) 14)
  (equal? (m3 i2 'kurds 'weigh) '(kurds weigh 10 4))
  (eq? (m4 i2) 'outer-x3)

  (begin
    (define-class (<b> b1 b2) (<a> (+ b1 b2))
      (ivars [x2 b1] [x3 b2])
      (methods
        [m1 (q) (vector self x2 q)]
        [m4 () x1]
        [m3 (s t) (list s t x1 x2 x3)]))
    (define i2 (make-<b> 4 5))
    #t)
  (eq? (m4 i2) 'outer-x1)
  (eq? (vector-ref (m1 i2 6) 0) i2)
  (equal? (vector-ref (m1 i2 6) 1) 4)
  (equal? (vector-ref (m1 i2 6) 2) 6)

  (begin
    (define-class (<c> x) (<root>)
      (ivars [x x])
      (methods [c1 (a) (make-<c> a)]))
    #t)
  ((lambda (x) (<c>? x)) (c1 (make-<c> 4) 5))

  (eq?
    (let ()
      (define-class (<c> x) (<root>)
        (ivars [x x])
        (methods
          [c1 (a) (make-<c> a)]
          [c2 () x]))
      (c2 (c1 (make-<c> 44) 87)))
    87)

  (begin
    (define-class (foo x) (<root>)
      (ivars [x x])
      (methods
        [hit () x]
        [hit (y) (set! x (+ x y))]))
    #t)

  (equal?
    (let ([a (make-foo 1)])
      (let ((b (hit a)))
        (hit a 17)
        (list b (hit a))))
    '(1 18))

  (error? ; invalid arity for hit
    (define-class (bar) (foo 1) (methods [hit (y z) (list y z)])))

  ; test variable arity methods

  (equal?
    (let ()
      (define-class (foo) (<root>) (methods [test (a . b) (list 'test a b)]))
      (test (make-foo) 1 2 3 4 5))
    '(test 1 (2 3 4 5)))

  (equal?
    (let ()
      (define-class (foo) (<root>) (methods [test (a . b) (list 'test a b)]))
      (define-class (bar) (foo) (methods [test (x . y) (list 'bar x y (super 'p 'd 'q 'r 's 't 'u))]))
      (test (make-bar) 1 2 3 4 5 6 7))
    '(bar 1 (2 3 4 5 6 7) (test p (d q r s t u))))

  (equal?
    (let ()
      (define-class (foo x) (<root>)
        (ivars [x x])
        (methods
          [ping () x]
          [ping (v) (set! x v)]))
      (define-class (bar x) (foo x)
        (methods
          [ping () super]                             ; return super method
          [ping (v) (super (+ (super) v))]))
      (let ([x (make-foo 1)] [y (make-bar 10)])
        (let ([before-x (ping x)] [before-y ((ping y))])
          (ping x 100)
          (ping y 100)
          (let ([after-x (ping x)] [after-y ((ping y))])
            ((ping y) 76)
            (list before-x before-y after-x after-y ((ping y)))))))
    '(1 10 100 110 76))

  (equal?
    (let ()
      (define-class (foo) (<root>) (methods [chow x (cons 'foo x)]))
      (define-class (bar) (foo) (methods [chow x (apply super 'bar x)]))
      (list (chow (make-foo) 1 2 3)
            (chow (make-bar) 4 5 6)))
    '((foo 1 2 3) (foo bar 4 5 6)))

  ; Verify that first-class super knows all arities of corresponding method.
  (equal?
    (let ()
      (define-class (foo) (<root>)
        (methods
          [chow (mein) (list 'foo 'chow-1 mein)]
          [chow (a b) (list 'foo 'chow-2 a b)]))
      (define-class (bar) (foo) (methods [chow (a b) super]))
      (let ([sup (chow (make-bar) 'ignore1 'ignore2)])
        (list (sup 'mane) (sup "ay" "bee"))))
    '((foo chow-1 mane) (foo chow-2 "ay" "bee")))

  ; Verify that we don't override method unless its generic is visible,
  ; i.e., we get a new method of the same name
  (equal?
    (let ()
      (module (foo (alpha bar))
        (define-class (foo) (<root>) (methods [bar () 'foobar]))
        (define-syntax alpha (identifier-syntax bar)))
      (define-class (baz) (foo) (methods [bar () 'bazbar]))
      (let ([x (make-baz)]) (list (alpha x) (bar x))))
    '(foobar bazbar))

  ; Verify that we can't send super unless method's generic is visible.
  (error? ; no inherited bar method (super)
    (let ()
      (module (foo (alpha bar))
        (define-class (foo) (<root>) (methods [bar () 'foobar]))
        (define-syntax alpha (identifier-syntax bar)))
      (define-class (baz) (foo) (methods [bar () (super)]))
      (make-baz)))

  ; Verify that we can't define a generic for a method with the same name
  ; as an interface method, i.e., supply an implementation of an
  ; interface-inherited method with the wrong arity
  (begin
    (define-interface bonk [whack (a mole)])
    #t)
  (error? ; invalid arity for whack
    (define-class (pewter) (<root>) (implements bonk)
      (methods
        [whack (e) "method w/ same name as interface method, but diff arity"]
        [whack (o no) "method matches interface method"])))

  ; more elaborate verification that we can't define a generic for a method
  ; with the same name as an interface method, i.e., supply an implementation
  ; of an interface-inherited method with the wrong arity
  (begin
    (define-interface bark [ham ()] [spam (y)])
    #t)
  (error? ; invalid arity for whack (or spam)
    (define-class (platinum) (<root>) (implements bark bonk)
      (methods
        [ham () "and cheese"]
        [spam () "spam"]
        [spam (y) "spam"]
        [xspam (x) "xspam"]
        [whack (e) "method w/ same name as interface method, but diff arity"]
        [whack (o no) "method matches interface method"])))
  (error? ; invalid arity for whack (or spam)
    (define-class (platinum) (<root>) (implements bonk bark)
      (methods
        [ham () "and cheese"]
        [spam () "spam"]
        [spam (y) "spam"]
        [xspam (x) "xspam"]
        [whack (e) "method w/ same name as interface method, but diff arity"]
        [whack (o no) "method matches interface method"])))
  (error? ; invalid arity for spam (or whack)
    (define-class (platinum) (<root>) (implements bark bonk)
      (methods
        [whack (e) "method w/ same name as interface method, but diff arity"]
        [whack (o no) "method matches interface method"]
        [ham () "and cheese"]
        [spam () "spam"]
        [spam (y) "spam"]
        [xspam (x) "xspam"])))
  (error? ; invalid arity for spam (or whack)
    (define-class (platinum) (<root>) (implements bonk bark)
      (methods
        [whack (e) "method w/ same name as interface method, but diff arity"]
        [whack (o no) "method matches interface method"]
        [ham () "and cheese"]
        [spam () "spam"]
        [spam (y) "spam"]
        [xspam (x) "xspam"])))


  (begin
    (define-interface i1 [fish (fry)])
    (define-interface i2 [rats (around)])
    #t)

  (error? ; fish not applicable to 3
    (fish 3 4))
  (error? ; rats not applicable to 3
    (rats 3 4))
  (error? ; fish not applicable to #<frob>
    (let ()
      (define-record frob ())
      (record-writer (type-descriptor frob)
        (lambda (x p wr)
          (display "#<frob>" p)))
      (fish (make-frob) 4)))

  (error? ; no implementation of interface method rats
    (define-class (<d> x) (<root>) (implements i1 i2)
      (ivars [x (* x x)])
      (methods
        [fish (fry) (list fry x)]
        [run (around) (cons around x)]
        [x! (v) (set! x (* v v))])))

  (equal?
    (let ()
      (define-class (<d> x) (<root>) (implements i1 i2)
        (ivars [x (* x x)])
        (methods
          [fish (fry) (list fry x)]
          [rats (around) (cons around x)]
          [x? () x]
          [x! (v) (set! x (* v v))]))
      (define d (make-<d> 3))
      (x! d 7)
      (list (x? d) (fish d "hi") (rats d "ih")))
    '(49 ("hi" 49) ("ih" . 49)))

  (begin
    (define-class (<e>) (<root>)
      (methods
        [m1 () (define-class (<f>) (<e>) (methods [m2 () 14])) (* (m2 (make-<f>)) 2)]))
    #t)
  (eqv? (m1 (make-<e>)) 28)

  (equal?
    (let ()
      (define (m2 x) "undefined")
      (module (c1 make-c1 m1 c1-friends)
        (module all (c1 make-c1 m1 m2)
          (define-class (c1) (<root>)
            (methods
              [m1 () "public"]
              [m2 () "protected"])))
        (module c1-friends (m2) (import all))
        (import all))
      (module (make-c2 m3)
        (import c1-friends)
        (define-class (c2) (c1)
          (methods [m3 () (m2 self)])))
      (module (make-c3 m4)
        (import c1-friends)
        (define-class (c3) (<root>)
          (methods [m4 (x) (m2 x)])))
      (let ([x (make-c2)] [y (make-c3)])
        (list (m1 x) (m2 x) (m3 x) (m4 y x))))
    '("public" "undefined" "protected" "protected"))

  (equal?
    (let ()
      (define (m2 x) "undefined")
      (module (c1 make-c1 m1 c2 make-c2 m3 make-c3 m4)
        (define-class (c1) (<root>)
          (methods
            [m1 () "public"]
            [m2 () "protected"]))
        (define-class (c2) (c1)
          (methods [m3 () (m2 self)]))
        (define-class (c3) (<root>)
          (methods [m4 (x) (m2 x)])))
      (let ([x (make-c2)] [y (make-c3)])
        (list (m1 x) (m2 x) (m3 x) (m4 y x))))
    '("public" "undefined" "protected" "protected"))

  (true?
    (let ([f (lambda ()
               (define-class (frap) (<root>))
               (cons make-frap frap?))])
      ((cdr (f)) ((car (f))))))

  (true?
    (let ([f (lambda ()
               (define-class (frap) (<root>) (methods [m () 5]))
               (cons make-frap frap?))])
      (not ((cdr (f)) ((car (f)))))))

  (true?
    (let ([f (lambda ()
               (define-class (frap) (<root>))
               (cons make-frap frap?))]
          [g (lambda ()
               (define-class (frap) (<root>))
               (cons make-frap frap?))])
      (and (not ((cdr (f)) ((car (g)))))
           (not ((cdr (g)) ((car (f))))))))

  (true?
    (let ([f (lambda ()
               (define-class (#{frap |.O7*%gC?Sxs~2\\%|}) (<root>))
               (cons make-frap frap?))]
          [g (lambda ()
               (define-class (#{frap |.O7*%gC?Sxs~2\\%|}) (<root>))
               (cons make-frap frap?))])
      (and ((cdr (f)) ((car (g))))
           ((cdr (g)) ((car (f)))))))

  (true?
    (let ([f (lambda ()
               (define-class (#{frap |.TfvA+Ml1*u&?\\%|}) (<root>) (ivars [x 0]))
               (cons make-frap frap?))]
          [g (lambda ()
               (define-class (#{frap |.TfvA+Ml1*u&?\\%|}) (<root>) (ivars [x 0]))
               (cons make-frap frap?))])
      (and ((cdr (f)) ((car (g))))
           ((cdr (g)) ((car (f)))))))

  (error? ; incompatible record type
    (let ([f (lambda ()
               (define-class (#{frap |.Uo3>P+Wu9o=u\\%|}) (<root>) (ivars [x 0]))
               (cons make-frap frap?))]
          [g (lambda ()
               (define-class (#{frap |.Uo3>P+Wu9o=u\\%|}) (<root>) (ivars [y 0]))
               (cons make-frap frap?))])
      (and ((cdr (f)) ((car (g))))
           ((cdr (g)) ((car (f)))))))

  (error? ; cannot specify gensym class-name with methods or interfaces
    (let ([f (lambda ()
               (define-class (frap) (<root>) (methods [m1 () 5]))
               (cons make-frap frap?))]
          [g (lambda ()
               (define-class (#{frap |.R@iB9FE~OXVz\\%|}) (<root>) (methods [m1 () 5]))
               (cons make-frap frap?))])
      (and ((cdr (f)) ((car (g))))
           ((cdr (g)) ((car (f)))))))

  (equal?
    (let ()
      (define-class (<frozwell> x) (<root>)
        (constructor frozwell-make)
        (predicate is-frozwell?))
        (let ([frzwl (frozwell-make 3)])
          (list (is-frozwell? frzwl)
                (is-frozwell? 17))))
    '(#t #f))

  (begin
    (define-class (<frozwell> x) (<root>)
      (constructor frozwell-make)
      (predicate is-frozwell?))
    #t)
  (equal?
    (let ([frzwl (frozwell-make 3)])
      (list (is-frozwell? frzwl)
            (is-frozwell? 17)))
    '(#t #f))

  (begin
    (library (L1)
      (export <frozwell> frozwell-make is-frozwell?)
      (import (chezscheme) (oop))
      (define-class (<frozwell> x) (<root>)
        (constructor frozwell-make)
        (predicate is-frozwell?)))
    #t)

  (equal?
    (let ()
      (import (L1))
      (let ([frzwl (frozwell-make 3)])
        (list (is-frozwell? frzwl)
              (is-frozwell? 17))))
    '(#t #f))

  (error? ; invalid syntax <frozwell>
    (let ()
      (import (L1))
      <frozwell>))

  (error? ; extra ivars clause
    (define-class (foo) (<root>)
      (ivars [x 0])
      (ivars [y 1])
      (methods [show () (values x y)])))

  (error? ; extra methods clause
    (define-class (foo) (<root>)
      (ivars [x 0] [y 1])
      (methods [show () (values x y)])
      (methods [get-x () x])))

  (begin
    (define-interface istud [cram (z)])
    (define-class (fritz q) (<root>)
      (methods [fritz-x+ (y) (+ x y)] [cram (n) (set! x (+ x n))])
      (predicate ?fritzy)
      (ivars [x (* q q)])
      (constructor fritzit)
      (implements istud))
    #t)
  (equal?
    (let ([w (fritzit 10)])
      (cram w 50)
      (list (?fritzy w)
            (?fritzy 'fritzy)
            (fritz-x+ w 7)))
    '(#t #f 157))


  (error? ; invalid assignment of immutable ivar x
    (define-class (blast x) (<root>) (ivars [immutable x x] [mutable y x])
      (init (set! x (* x x)))))

  (error? ; invalid assignment of immutable ivar x
    (define-class (blast x) (<root>) (ivars [immutable x x] [mutable y x])
      (methods
        [m (v) (set! x v)])))

  (error? ; blast-x-set! not bound
    (let ()
      (define-class (blast x) (<root>) (ivars [public immutable x x] [public mutable y x]))
      (define b (make-blast 17))
      (blast-x-set! b (* (blast-x b) (blast-x b)))
      (blast-x b)))

  (equal?
    (let ()
      (define-class (blast x) (<root>) (ivars [public immutable x x] [public mutable y x])
        (init (set! y (* y y))))
      (define b (make-blast 9))
      (list (blast-x b) (blast-y b)))
    '(9 81))

  (equal?
    (let ()
      (define-class (blast x) (<root>)
        (ivars [public immutable x x] [public mutable y x])
        (methods
          [m (v) (set! y v)]))
      (define b (make-blast 9))
      (m b 35)
      (list (blast-x b) (blast-y b)))
    '(9 35))

  (equal?
    (let ()
      (define-class (blast x) (<root>)
        (ivars [public immutable x x] [public mutable y x]))
      (define b (make-blast 17))
      (blast-y-set! b (* (blast-x b) (blast-x b)))
      (list (blast-x b) (blast-y b)))
    '(17 289))

  (begin
    (define-class (<q> a1) (<root>)
      (ivars [public mupu1 (+ a1 1)]
             [public mutable mupu2 (+ a1 2)]
             [public immutable impu3 (+ a1 3)]
  
             [private mupr4 (+ a1 4)]
             [private mutable mupr5 (+ a1 5)]
             [private immutable impr6 (+ a1 6)]
  
             [private mupr7 (+ a1 7)]
             [private mutable mupr8 (+ a1 8)]
             [private immutable impr9 (+ a1 9)]))
    (define i1 (make-<q> 10))
    #t)
  (equal?
    (list (<q>-mupu1 i1) (<q>-mupu2 i1) (<q>-impu3 i1))
    '(11 12 13))
  (equal?
    (begin
      (<q>-mupu1-set! i1 'a)
      (<q>-mupu2-set! i1 'b)
      (list (<q>-mupu1 i1) (<q>-mupu2 i1) (<q>-impu3 i1)))
    '(a b 13))
  (error? ; not bound
    <q>-mupr4)
  (error? ; not bound
    <q>-mupr5)
  (error? ; not bound
    <q>-impr6)
  (error? ; not bound
    <q>-mupr7)
  (error? ; not bound
    <q>-mupr8)
  (error? ; not bound
    <q>-impr9)
  (error? ; not bound
    <q>-impu3-set!)
  (error? ; not bound
    <q>-mupr4-set!)
  (error? ; not bound
    <q>-mupr5-set!)
  (error? ; not bound
    <q>-impr6-set!)
  (error? ; not bound
    <q>-mupr7-set!)
  (error? ; not bound
    <q>-mupr8-set!)
  (error? ; not bound
    <q>-impr9-set!)

  (begin
    (define-class (<r> a1) (<q> (+ a1 10))
      (ivars [public mupu1 (+ a1 1)]
             [mutable public mupu2 (+ a1 2)]
             [immutable public impu3 (+ a1 3)]))
    (define i2 (make-<r> 10))
    #t)
  (equal?
    (list (<q>-mupu1 i2) (<q>-mupu2 i2) (<q>-impu3 i2)
          (<r>-mupu1 i2) (<r>-mupu2 i2) (<r>-impu3 i2))
    '(21 22 23 11 12 13))
  (equal?
    (begin
      (<q>-mupu1-set! i2 "hi")
      (<q>-mupu2-set! i2 "there")
      (<r>-mupu1-set! i2 "ye")
      (<r>-mupu2-set! i2 "matey")
      (list (<q>-mupu1 i2) (<q>-mupu2 i2) (<q>-impu3 i2)
            (<r>-mupu1 i2) (<r>-mupu2 i2) (<r>-impu3 i2)))
    '("hi" "there" 23 "ye" "matey" 13))
  (error? ; not bound
    <r>-impu3-set!)
  (error? ; not applicable
    (<r>-mupu1 i1))
  (error? ; not applicable
    (<r>-mupu1-set! i1 55))

  (begin
    (define-class (<s> a1) (<r> (+ a1 10))
      (ivars [public mupu1 (+ a1 1)]
             [public mutable mupu2 (+ a1 2)]
             [public immutable impu3 (+ a1 3)])
      (prefix "s$"))
    (define i3 (make-<s> 10))
    #t)
  (equal?
    (list (<q>-mupu1 i3) (<q>-mupu2 i3) (<q>-impu3 i3)
          (<r>-mupu1 i3) (<r>-mupu2 i3) (<r>-impu3 i3)
          (s$mupu1 i3) (s$mupu2 i3) (s$impu3 i3))
    '(31 32 33 21 22 23 11 12 13))
  (equal?
    (begin
      (<q>-mupu1-set! i3 'hi)
      (<q>-mupu2-set! i3 'there)
      (<r>-mupu1-set! i3 'ye)
      (<r>-mupu2-set! i3 'matey)
      (s$mupu1-set! i3 'scaliwag)
      (s$mupu2-set! i3 'pirate)
      (list (<q>-mupu1 i3) (<q>-mupu2 i3) (<q>-impu3 i3)
            (<r>-mupu1 i3) (<r>-mupu2 i3) (<r>-impu3 i3)
            (s$mupu1 i3) (s$mupu2 i3) (s$impu3 i3)))
    '(hi there 33 ye matey 23 scaliwag pirate 13))
  (error? ; not bound
    <s>-impu1)
  (error? ; not bound
    s$impu3-set!)
  (error? ; not applicable
    (s$mupu1 i1))
  (error? ; not applicable
    (s$mupu1-set! i1 55))
  (error? ; not applicable
    (s$mupu1 i2))
  (error? ; not applicable
    (s$mupu1-set! i2 55))

  ;;; tests from Michael Lenaghan of frogware, Inc.

  (begin
    ;; simple init expression
    (define-class (<test-1> x y) (<root>)
      (ivars [x x] [y y] [z (* x y)])
      (methods [method-1 () z]))
  
    ;; simple init expressions that depend
    ;; on previously computed values
    (define-class (<test-2> x y) (<root>)
      (ivars [x x] [y y] [z1 (* x y)] [z2 (* 2 z1)])
      (methods [method-2 () z2]))
  
    ;; simple init proc
    (define-class (<test-3> x y) (<root>)
      (ivars [x x] [y y] [z 0])
      (init
       (set! z (* x y)))
      (methods [method-3 () z]))
  
    ;; class and base class initialization can have
    ;; different arity
    (define-class (<test-4> x) (<test-1> x x))
  
    ;; class and base class initialization can have
    ;; different arity and base class can use expressions
    (define-class (<test-5> x) (<test-1> x (* 2 x)))
    #t)

  (eqv?
    (let ([test (make-<test-1> 5 10)])
      (method-1 test))
    50)

  (eqv?
    (let ([test (make-<test-2> 5 10)])
      (method-2 test))
    100)

  (eqv?
    (let ([test (make-<test-3> 5 10)])
      (method-3 test))
    50)

  (eqv?
    (let ([test (make-<test-4> 5)])
      (method-1 test))
    25)

  (eqv?
    (let ([test (make-<test-5> 5)])
      (method-1 test))
    50)

  (begin
    ;; base class
    (define-class (<test-1> init-1) (<root>)
      (ivars [fld-1 init-1])
      (methods
        [whoami () self]
        [method-1 () fld-1]
        [method-2 (x) (set! fld-1 x)]))
  
    ;; sub-class
    (define-class (<test-2> init-1 init-2) (<test-1> init-1)
      (ivars [fld-2 init-2])
      (methods
        [method-3 () fld-2]
        [method-4 (x) (set! fld-2 x)]))
  
    ;; Note: The class <test-3> can't use the method names
    ;; "method-3" and "method-4" because they're used by
    ;; <test-2>. Chez OOP produces a "generic function"
    ;; for each method, and it looks like those functions
    ;; all have to belong to one line of the class inheritance
    ;; tree.
  
    ;; sub-class w/ overload
    (define-class (<test-3> init-1 init-2) (<test-1> init-1)
      (ivars [fld-2 init-2])
      (methods
        [method-1 () (method-3a self)]
        [method-2 (x) (method-4a self x)]
        [method-3a () fld-2]
        [method-4a (x) (set! fld-2 x)]))
  
    ;; sub-class w/ overload & fields
    ;; if this is uncommented, uncomment <test-4> test below
    #;
      (define-class (<test-4> init-1) (<test-1> init-1)
      (methods
        [method-3b ()
                   ;; this provides access to super-class fields
                   (open-instance <test-1> "" self)
                   fld-1]
        [method-4b (x)
                   ;; this provides access to super-class fields
                   (open-instance <test-1> "" self)
                   (set! fld-1 x)]))
  
    ;; sub-class w/ overload & super
    (define-class (<test-5> init-1) (<test-1> init-1)
      (methods
        [method-1 () (string->symbol
                       (string-append (symbol->string (super)) "!!!"))]
        [method-2 (x) (super
                        (string->symbol
                          (string-append (symbol->string x) "!!!")))]))
  
    ;; sub-class w/ variable arity
    (define-class (<test-6> init-1) (<test-1> init-1)
      (methods
        [method-5 () (method-1 self)]
        [method-5 (x) (method-2 self x)]))
    #t)

  (equal?
    (let ((test (make-<test-1> 'hello)))
      (seq-list
        (eq? test (whoami test))
        (method-1 test)
        (method-2 test 'goodbye)
        (method-1 test)))
    `(#t hello ,(void) goodbye))

  (equal?
    (let ((test (make-<test-2> 'hello 'hello-again)))
      (seq-list
        (method-1 test)
        (method-2 test 'goodbye)
        (method-1 test)
        (method-3 test)
        (method-4 test 'goodbye-again)
        (method-3 test)))
    `(hello ,(void) goodbye hello-again ,(void) goodbye-again))

  (equal?
    (let ((test (make-<test-3> 'hello 'hello-again)))
      (seq-list
        (method-1 test)
        (method-2 test 'goodbye-again)
        (method-1 test)
        (method-3a test)
        (method-4a test 'hello-again)
        (method-3a test)))
    `(hello-again ,(void) goodbye-again goodbye-again ,(void) hello-again))

  #;
  (equal?
    (let ((test (make-<test-4> 'hello)))
      (seq-list
        (method-1 test)
        (method-2 test 'goodbye)
        (method-1 test)

        (method-3b test)
        (method-4b test 'hello)
        (method-3b test)))
    `(hello ,(void) goodbye goodbye ,(void) hello))

  (equal?
    (let ((test (make-<test-5> 'hello)))
      (seq-list
        (method-1 test)
        (method-2 test 'goodbye)
        (method-1 test)))
    `(hello!!! ,(void) goodbye!!!!!!))

  (equal?
    (let ((test (make-<test-6> 'hello)))
      (seq-list
        (method-5 test)
        (method-5 test 'goodbye)
        (method-5 test)))
    `(hello ,(void) goodbye))

  (begin
    ;; use class exported from module
    (module test-1 (<test-1> make-<test-1> method-1)
      (define-class (<test-1> x) (<root>)
        (ivars [x x])
        (methods [method-1 () x])))
    #t)

  (eqv? (let () (import test-1) (method-1 (make-<test-1> 3))) 3)
  (eqv?
    (let ()
      (import test-1)
      (define-class (<frob> x) (<test-1> x))
      (method-1 (make-<test-1> 3)))
    3)

  (begin
    ;; use sub-class exported from module
    (module test-2 (<test-2> make-<test-2> method-1)
      (import test-1)
      (define-class (<test-2> x) (<test-1> x)))
    #t)

  (eqv? (let () (import test-2) (method-1 (make-<test-2> 3))) 3)

  (begin
    ;; use sub-class w/ overload exported from module
    (module test-3 (make-<test-3> method-1)
      (import test-2)
      (define-class (<test-3> x) (<test-2> x)
        (ivars [x x])
        (methods [method-1 () (* x x)])))
    #t)

  (eqv?
    (let ()
      (import test-1)
      (let ([test (make-<test-1> 10)])
        (method-1 test)))
    10)

  (eqv?
    (let ()
      (import test-2)
      (let ([test (make-<test-2> 10)])
        (method-1 test)))
    10)

  (eqv?
    (let ()
      (import test-3)
      (let ([test (make-<test-3> 10)])
        (method-1 test)))
    100)

  (begin
    ;; base interface
    (define-interface <<interface-1>>
      [imethod-1 ()]
      [imethod-2 (x)])
  
    ;; sub-interface
    (define-interface <<interface-2>> <<interface-1>>
      [imethod-3 ()]
      [imethod-4 (x)])
  
    ;; base interface
    (define-interface <<interface-3>>
      [imethod-5 ()]
      [imethod-6 (x)])
  
    ;; base class w/ base interface
    (define-class (<itest-1> init-1) (<root>)
      (implements <<interface-1>>)
      (ivars [fld-1 init-1])
      (methods
        [method-1 () 'method-1]
        [imethod-1 () fld-1]
        [imethod-2 (x) (set! fld-1 x)]))
  
    ;; sub-class w/ sub-interface
    (define-class (<itest-2> init-1 init-2) (<itest-1> init-1)
      (implements <<interface-2>>)
      (ivars [fld-2 init-2])
      (methods
        [method-2 () 'method-2]
        [imethod-3 () fld-2]
        [imethod-4 (x) (set! fld-2 x)]))
  
    ;; sub-class w/ new method
    (define-class (<itest-3> init-1 init-2) (<itest-2> init-1 init-2)
      (ivars [fld-3 (+ init-1 init-2)])
      (methods
        [method-3 () fld-3]))
  
    ;; base class w/ interfaces & new method
    (define-class (<itest-4> init-1) (<root>)
      (implements <<interface-1>> <<interface-3>>)
      (ivars [fld-1 init-1])
      (methods
        [imethod-1 () (* 2 fld-1)]
        [imethod-2 (x) (set! fld-1 x)]
        [imethod-5 () (* 4 fld-1)]
        [imethod-6 (x) (set! fld-1 x)]
        [method-4 () fld-1]
        [method-4! (x) (set! fld-1 x)]))
    #t)

  (equal?
    (let ((itest (make-<itest-1> 'hello)))
      (seq-list
        (method-1 itest)
        (imethod-1 itest)
        (imethod-2 itest 'goodbye)
        (imethod-1 itest)))
    `(method-1 hello ,(void) goodbye))

  (eqv?
    (let ((itest (make-<itest-2> 'hello 'hello-again)))
      (method-2 itest))
    'method-2)

  (equal?
    (let ((itest (make-<itest-2> 'hello 'hello-again)))
      (seq-list
        (imethod-1 itest)
        (imethod-2 itest 'goodbye)
        (imethod-1 itest)

        (imethod-3 itest)
        (imethod-4 itest 'goodbye-again)
        (imethod-3 itest)))
    `(hello ,(void) goodbye hello-again ,(void) goodbye-again))

  (eqv?
    (let ((itest (make-<itest-3> 5 10)))
      (method-3 itest))
    15)

  (equal?
    (let ((itest (make-<itest-4> 10)))
      (seq-list
        (imethod-1 itest)
        (imethod-5 itest)
        (method-4 itest)
        (method-4! itest 20)
        (imethod-1 itest)
        (imethod-5 itest)
        (method-4 itest)))
    `(20 40 10 ,(void) 40 80 20))

  (begin
    ;; export interface from module
    (module test-1 (<<interface-1>> imethod-1 imethod-2)
      (define-interface <<interface-1>>
        [imethod-1 ()]
        [imethod-2 (v)]))
  
    ;; export sub-interface from module
    (module test-2 (<<interface-2>> imethod-3 imethod-4)
      (import test-1)
      (define-interface <<interface-2>> <<interface-1>>
        [imethod-3 ()]
        [imethod-4 (v)]))
  
    ;; use class w/ interface exported from module
    (module test-3 (<itest-3> make-<itest-3> imethod-1 imethod-2)
      (import test-1)
      (define-class (<itest-3> x) (<root>)
        (implements <<interface-1>>)
        (ivars [x x])
        (methods
          [imethod-1 () x]
          [imethod-2 (v) (set! x v)])))
  
    ;; use sub-class w/ interface exported from module
    (module test-4 (<itest-4> make-<itest-4> imethod-1 imethod-2 imethod-3 imethod-4)
      (import test-2)
      (import test-3)
      (define-class (<itest-4> x) (<itest-3> x)
        (implements <<interface-2>>)
        (methods
          [imethod-3 () (* 2 (imethod-1 self))]
          [imethod-4 (v) (imethod-2 self (* 2 v))])))
  
    ;; use sub-class w/ overload of interface methods exported from module
    (module test-5 (make-<itest-5> imethod-1 imethod-2 imethod-3 imethod-4)
      (import test-4)
      (define-class (<itest-5> x) (<itest-4> x)
        (methods
          [imethod-1 () (* 2 (super))]
          [imethod-3 () (* 2 (super))])))
  
    ;; use sub-class w/ new methods exported from module
    (module test-6 (make-<itest-6> method-1)
      (import test-4)
      (define-class (<itest-6> x) (<itest-4> x)
        (ivars [x x])
        (methods
          [method-1 () (* x x)])))
    #t)

  (equal?
    (let ()
      (import test-3)
      (let ([test (make-<itest-3> 10)])
        (seq-list
          (imethod-1 test)
          (imethod-2 test 20)
          (imethod-1 test))))
    `(10 ,(void) 20))

  (equal?
    (let ()
      (import test-4)
      (let ([test (make-<itest-4> 10)])
        (seq-list
          (imethod-1 test)
          (imethod-2 test 20)
          (imethod-1 test)
          (imethod-3 test)
          (imethod-4 test 20)
          (imethod-3 test))))
    `(10 ,(void) 20 40 ,(void) 80))

  (equal?
    (let ()
      (import test-5)
      (let ([test (make-<itest-5> 10)])
        (seq-list
          (imethod-1 test)
          (imethod-2 test 20)
          (imethod-1 test)
          (imethod-3 test)
          (imethod-4 test 20)
          (imethod-3 test))))
    `(20 ,(void) 40 160 ,(void) 320))

  (eqv?
    (let ()
      (import test-6)
      (let ([test (make-<itest-6> 10)])
        (method-1 test)))
    100)

  ;;; end of tests from Michael Lenaghan of frogware, Inc.

  ;;; letrec-classes tests from seminar

  (begin
    (define-syntax letrec-classes
      (syntax-rules ()
        [(_ ([class-name (class-formal ...) (base-name base-arg ...)
               ([ivar ivar-init] ...)
               [method-name (method-formal ...) method-b1 method-b2 ...] ...]
             ...)
            b1 b2 ...)
         (let ()
           (define-class (class-name class-formal ...) (base-name base-arg ...)
             (ivars [ivar ivar-init] ...)
             (methods [method-name (method-formal ...) method-b1 method-b2 ...] ...))
           ...
           b1 b2 ...)]))
    #t)

  (error? ; wrong number of base-class arguments
    (letrec-classes ([<a> (x) (<root>) ()])
      (letrec-classes ([<b> () (<a>) ()])
        (make-<b>))))

  (error? ; no inherited foo method for (super)
    (letrec-classes ([<a> () (<root>) () [foo () (super)]])
      (foo (make-<a>))))

  (eq?
    (let ()
      (letrec-classes ([<c> (x) (<root>)
                        ([x x])
                        [c1 (a) (make-<c> a)]
                        [c2 () x]])
        (c2 (c1 (make-<c> 44) 87))))
    87)

  (eq?
    (letrec-classes ((A () (<root>) ()))
      (letrec-classes
        ((<root> () (A) ()
          (foo (<root>) 77)))
        (foo (make-<root>) 88)))
    77)

  ; Ronald Garcia

  ; Here are some INVALID test cases that I use to exercise what errors my
  ; compiler will catch.  A few might not fail given the proper compiler
  ; extension (i.e. do classes and variables share the same namespace...)

  (error? ; duplicate definition repeat, repeat?, and make-repeat
    (letrec-classes ([Repeat () (<root>) ()]
                     [Repeat () (<root>) ()])
      0))

  (error? ; duplicate ivar i
    (letrec-classes ([Vars () (<root>) ((i 1) (i 1))])
      0))

  (error? ; unrecognized base class aaaaa
    (letrec-classes ([Empty () (aaaaa) ()])
      (let ([mt (make-Empty)])
        0)))

  ;;; Chez Scheme allows this:
  (eqv?
    (letrec-classes ([One () (<root>) ()]
                     [Two  () (One) ()])
      0)
    0)

  (error? ; unrecognized base class aaaaa
    (letrec-classes ([One () (<root>) ()])
      (letrec-classes ([Two  () (aaaaa) ()])
      0)))

  (error? ; duplicate same-arity method definition
    (letrec-classes ([Vars () (<root>) ()
                           (M1 () 0)
                           (M1 () 1)])
      0))

  (error? ; incorrect base argument count
    (letrec-classes ([Class () (<root> unbound) ()])
      0))

  (error? ; unbound is not bound
    (letrec-classes ([c1 (x) (<root>) ()])
      (letrec-classes ([c2 () (c1 unbound) ()])
        (make-c2))))

  (error? ; unbound is not bound
    (letrec-classes ([c () (<root>) ((i unbound))])
      (make-c)))

  (error? ; j is unbound
    (letrec-classes ([c () (<root>) ((i j) (j 0))])
      (make-c)
      0))

  (eqv?
    (letrec-classes ([c () (<root>) ((i 1) (j (+ i 2))) (m () j)])
      (m (make-c)))
    3)

  (error? ; unbound is not bound
    (letrec-classes ([c (i j) (<root>) ()])
      (make-c 1 unbound)))

  (error? ; unbound is not bound
    (letrec-classes ([c (i j) (<root>) ()])
      (c? unbound)))

  (error? ; unbound is not bound
    (letrec-classes ([Class () (<root>) () (M1 (i) unbound)])
      (M1 (make-Class) 6)))

  (error? ; duplicate definition of M1
    (letrec-classes ([One () (<root>) () (M1 () 0)]
                     [Two () (<root>) () (M1 () 0)])
      0))

  (eqv?
    (letrec-classes ([Pop () (<root>) () (M1 () 0)])
      (letrec-classes ([One () (Pop) () (M1 () 1)]
                       [Two () (<root>) () (M1 () 2)])
        (M1 (make-Two))))
    2)

  (error? ; duplicate definition of M2
    (letrec-classes ([Pop () (<root>)  () (M1 () 0)])
      (letrec-classes ([One () (Pop) () (M1 () 1) (M2 () 2)]
                       [Two () (Pop) () (M2 () 2)])
          0)))

  (equal?
    (letrec-classes ([Pop () (<root>)  () (M1 () 0)])
      (letrec-classes ([One () (Pop) () (M1 () 1)]
                       [Two () (Pop) () (M2 () 2)])
        (let ([M2* M2])
          (letrec-classes ([Three () (One) () (M1 () 3) (M2 () 4)])
            (list (M1 (make-Pop))
                  (M1 (make-One))
                  (M1 (make-Two))
                  (M2* (make-Two))
                  (M1 (make-Three))
                  (M2 (make-Three)))))))
    '(0 1 0 2 3 4))

  (error? ; variable ingnacious is unbound
    (letrec-classes ([Pop () (<root>) ([ingnacious 1])])
      (letrec-classes ([One () (<root>) () (M1 () ingnacious)])
        (M1 (make-One)))))

  (equal?
    (letrec-classes ([Pop () (<root>) ([i 1]) [get () i]])
      (letrec-classes ([One () (Pop) ([i 2]) [get () (list (super) i)]])
        (get (make-One))))
    '(1 2))

  (error? ; invalid syntax class
    (let ([Class #f])
      (letrec-classes ([Class () (<root>) ()])
        (let ([Class Class])
          0))))

  (eqv?
    (letrec-classes ([Class () (<root>) ()])
      (let ([Class #f]
            [foo (make-Class)])
        (Class? foo)))
    #t)

  ; Here are some pretty trivial (i.e. relatively easy to follow by hand) test cases.
  ; They cover some pretty basic functionality (specifying classes without making them, etc.)

  (eq?
  ;; simplest example...
  (letrec-classes ([Empty () (<root>)
                          ()])
    0)
  0)

  (eq?
  ;; It's okay for ivars in separate classes to have the same name.
  (letrec-classes ([One () (<root>) ((var 0))]
                   [Two  () (<root>) ((var 0))])
    0)
  0)

  (eq?
  ;; naive inheritance example
  (letrec-classes ([One () (<root>) ()])
    (letrec-classes ([Two  () (One) ()])
    0))
  0)

  (eq?
  ;; Actually make a class
  (letrec-classes ([Empty () (<root>)
                          ()])
    (let ([mt (make-Empty)])
      0))
  0)

  (eq?
  ;; simple example of using class formals in base-init
  (letrec-classes ([One (i) (<root>) ()])
    (letrec-classes ([Two (j) (One j) ()])
    0))
  0)

  (eq?
  ;; simple example of using class formal in ivar-init.
  (letrec-classes ([Class (i) (<root>)
                          ((var i))])
    0)
  0)

  (eq?
  ;; ivar-init's can see the previous ivar.
  (letrec-classes ([Class () (<root>)
                          ((var1 0)
                           (var2 var1))])
    0)
  0)


  (eq?
  ;; parameters to methods are visible in methods
  (letrec-classes ([Class () (<root>) ()
                          (M1 (i) i)])
    0)
  0)

  (eq?
  ;; "self" is implicitly added to method environments.
  (letrec-classes ([Class () (<root>) ()
                          (M1 () self)])
    0)
  0)

  (eq?
  ;; inheritance hierarchy can share methods
  (letrec-classes ([Pop () (<root>) ()
                        (M1 () 0)])
    (letrec-classes ([One () (Pop) ()
                          (M1 () 1)]
                     [Two () (Pop) ()
                          (M1 () 2)])
      0))
  0)

  (eq?
  ;; more windy inheritance hierarchy
  (letrec-classes ([Pop () (<root>) () (M1 () 0)])
    (letrec-classes ([One () (Pop) () (M1 () 1)]
                     [Two () (Pop) () (M2 () 2)])
      (letrec-classes ([Three () (One) ()
                              (M1 () 1)
                              (M3 () 2)]
                       [Four () (Two) ()
                              (M2 () 1)
                              (M4 () 2)])
        0)))
  0)

  (eq?
  ;; Skip a generation before overloading...
  (letrec-classes ([Pop () (<root>) () (M1 () 0)])
    (letrec-classes ([One () (Pop) ()])
      (letrec-classes ([Three () (One) ()
                              (M1 () 1)
                              (M3 () 2)])
        0)))
  0)

  (eq?
  ;; classes in the same block can see each other.
  (letrec-classes ([One () (<root>) () (M1 () (make-Two))]
                   [Two () (<root>) ()])
    0)
  0)

  (eq?
  ;; classes in the same block can call each other's methods.
  (letrec-classes ([One () (<root>) () (M1 (obj) (M2 obj))]
                   [Two () (<root>) () (M2 () 3)])
    0)
  0)

  (eq?
  ;; class methods in the same block can be seen in base inits
  (letrec-classes ([Pop (i j) (<root>) ()])
    (letrec-classes ([One () (<root>) () (M1 (obj) (M2 obj))]
                     [Two () (Pop (M1 (make-One)) 5) () (M2 () 3)])
      0))
  0)

  (eq?
  ;; subclass methods can see superclass instance vars
  (letrec-classes ([Pop () (<root>) ([i 1])])
    (letrec-classes ([One () (Pop) () (M1 () i)])
      0))
  0)


  (eq?
  ;; class names should become unique
  (letrec-classes ([Class () (<root>) ()])
    (letrec-classes ([Class () (<root>) ()])
      0))
  0)

  (eq?
  ;; class names should not clash with variables either
  (let ([Class #f])
    (letrec-classes ([Class () (<root>) ()])
      (letrec-classes ([Class () (<root>) ()])
        0)))
  0)


  (eq?
  ;; Variables bound outside letrec-classes should be visible
  (let ([bound-var #f])
    (letrec-classes ([Super (i) (<root>) ()])
      (letrec-classes ([Class () (Super bound-var) ([i bound-var])
                            (M1 () bound-var)])
        0)))
  0)

  (eq?
  ;; Number has one instance variable that holds a number and one method
  ;; that returns the number.
  (letrec-classes ([Number (num^) (<root>)
                           ((num num^))
                           (Val () num)])
    (let ([nb1 (make-Number 1)]
          [nb2 (make-Number 2)])
      (+ (Val nb1) (Val nb2))))
  ;; result: 3
  3)

  (eq?
  ;; test out method binding
  (letrec-classes ([Pop () (<root>) () (MP1 () 0) (MP2 (i) 0)])
    (letrec-classes ([One () (Pop) () (M1 () 1)])
      (letrec-classes ([Two () (One) () (M2 (i j) 2) (MP1 () 2)])
        (letrec-classes ([Three () (Two) () (MP2 (i) 3) (M3 () 3)])
          0))))
  0)

  ; Mark Meiss

  (eq?
  (let ([object (letrec-classes
                  ([duo (n) (<root>)
                     ([n n])
                     (plus  () (+ n 2))
                     (times () (* n 2))
                     (expt  () (* n n))
                     (export ()
                       (let ([vec (make-vector 4)])
                         (vector-set! vec 0 self)
                         (vector-set! vec 1 plus)
                         (vector-set! vec 2 times)
                         (vector-set! vec 3 expt)
                         vec))])
                  (export (make-duo 6)))])
    (* ((vector-ref object 1) (vector-ref object 0))
       (+ ((vector-ref object 2) (vector-ref object 0))
          ((vector-ref object 3) (vector-ref object 0)))))

  ; should evaluate to 384
  384)

  ;------------------------------------------------------------------------

  (eq?
  (letrec ([class-maker (lambda (n)
                          (if (zero? n)
                              (letrec-classes
                                ([zero () (<root>)
                                   ()
                                   (get-n () 0)])
                                (cons (make-zero) get-n))
                              (letrec-classes
                                ([succ () (<root>)
                                   ()
                                   (get-n () (let ([prev (class-maker (sub1 n))])
                                               (add1 ((cdr prev) (car prev)))))])
                                (cons (make-succ) get-n))))]
           [fib (lambda (n)
                  (if (< ((cdr n) (car n)) 2)
                      ((cdr n) (car n))
                      (+ (fib (class-maker (sub1 ((cdr n) (car n)))))
                         (fib (class-maker (sub1 (sub1 ((cdr n) (car n)))))))))])
    (fib (class-maker 7)))

  ; should evaluate to 13
  13)

  ;------------------------------------------------------------------------

  (eq?
  (letrec-classes ([<route> (a b c) (<root>)
                     ([a (+ a a)]
                      [b (+ a b)]
                      [c (+ b c)])
                     (get-b () b)
                     (sum (a) (+ a (+ (get-b self) c)))])
    (sum (make-<route> 1 2 3) 4))

  ; should evaluate to 15
  15)

  ;------------------------------------------------------------------------

  (equal?
  (letrec-classes ([A (x y) (<root>)
                      ([x x] [y y])
                      (get-x () x)
                      (get-y () y)
                      (test (object)
                        (if (A? object)
                            (+ (- (get-x self) (get-x object))
                               (- (get-y self) (get-y object)))
                            (* (get-x self) (get-y self))))])
    (letrec-classes ([B () (A 2 3)
                      ()
                        (become-if-not-A (object)
                          (if (A? object) self object))]
                     [C (x y) (A x y)
                        (#;
                         [x x]
                         #;
                         [y y])])
      (let ([a-var (make-A 3 4)]
            [b-var (make-B)]
            [c-var (make-C 2 1)]
            [vec (make-vector 4)])
        (vector-set! vec 0 (test a-var b-var))
        (vector-set! vec 1 (test a-var c-var))
        (set! b-var (become-if-not-A b-var b-var))
        (vector-set! vec 2 (test a-var b-var))
        (set! b-var (become-if-not-A b-var c-var))
        (vector-set! vec 3 (test a-var b-var))
        vec)))

  ; should evaluate to #(2 4 2 2)
  '#(2 4 2 2))

  ;------------------------------------------------------------------------

  #;
  (equal?
    (letrec-classes ([fish (head tail) (<root>)
                       ([head head] [tail tail])
                       (behead () (set! head tail))
                       (betail () (set! tail head))
                       (get-head () head)
                       (get-tail () tail)])
      (letrec-classes ([guppy (head tail) (fish head tail)
                         ()
                         (behead () (open-instance fish "" self) (set! head (cons tail tail)))
                         (betail () (open-instance fish "" self) (set! tail (cons head head)))])
        (letrec-classes ([minnow (head tail) (guppy head tail)
                           ()
                           (behead () (begin (super) (set! betail behead)))
                           (betail () (begin (super) (set! behead betail)))])
          (let ([fishy-1 (make-fish 4 8)]
                [fishy-2 (make-guppy 5 9)])
            (let ([fishy-red (make-minnow fishy-1 fishy-2)])
              (behead fishy-1)
              (betail fishy-2)
              (behead fishy-red)
              (betail fishy-red)
              (get-tail (cdr (get-head fishy-red))))))))

  ; should evaluate to (5 . 5)
  '(5 . 5))

  ; Brooke Chenoweth

  (equal?
  ;; objects shouldn't be identifiable as vectors or procedures
  (letrec-classes ([foo () (<root>) ()])
    (let ([obj (make-foo)])
      (cons (foo? obj)
        (cons (procedure? obj)
          (cons (vector? obj) '())))))
  ; should return '(#t #f #f)
  '(#t #f #f))

  (eq?
  ;; We should be able to package up methods for outside use
  (let ([foo-package
          (letrec-classes ([foo (x) (<root>)
                             ((x x))
                             (get-x () x)])
            (let ([v (make-vector 3)])
              (vector-set! v 0
                (lambda (x) (make-foo x))) ; foo-maker
              (vector-set! v 1
                (lambda (x) (foo? x))) ; foo?
              (vector-set! v 2
                (lambda (inst) (get-x inst))) ; get-x
              v))])
    (let ([make-foo (vector-ref foo-package 0)]
          [foo? (vector-ref foo-package 1)]
          [foo-get-x (vector-ref foo-package 2)])
      (let ([r (letrec-classes ([R () (<root>) ()]) (make-R))]
            [f (make-foo 4)])
        (if (foo? r)
            (foo-get-x r)
            (if (foo? f)
                (foo-get-x f)
                -100)))))
  ; should return 4
  4)

  (equal?
  (letrec-classes ([A (x y) (<root>)
                     ((s (+ x y))
                      (d (- x y)))
                     (m1 () (- s d))
                     (m2 () (+ s d))]
                   [R () (<root>) ()])
    (letrec-classes ([B (x y z) (A y z)
                       ((p (* x y)))
                       (m1 () (+ (super) p))
                       (m3 () (- (m2 self) p))])
      (let ([robj (make-R)]
            [aobj (make-A 1 2)] ; s = 3, d = -1
            [bobj (make-B 3 4 5)] ; s = 9, d = -1, p = 12
            [gather-results
              (lambda (obj)
                (and (A? obj)
                     (cons (m1 obj)
                       (cons (m2 obj)
                         (cons (if (B? obj)
                                   (m3 obj)
                                   #f)
                           '())))))]
            [v (make-vector 3)])
        (vector-set! v 0 (gather-results robj))
        (vector-set! v 1 (gather-results aobj))
        (vector-set! v 2 (gather-results bobj))
        v)))
  ; should return #( #f (4 2 #f) (22 8 -4))
  '#(#f (4 2 #f) (22 8 -4)))


  ;; Allen Lee

  (equal?
  (letrec-classes ([superguy (x y) (<root>)
                     ([x (* x x)]
                      [y (let ([x 3])
                           (+ x (- y y)))]
                      [z (lambda (x) (+ x x))])
                      (getX () x)
                      (getY () y)])
    (letrec-classes ([subguy (x y) (superguy (+ x x) (+ y y))
                       ([new-x x])
                       (plus (y) (+ new-x y))])
      (letrec-classes ([subsubguy (x y z) (subguy
                                            (+ (+ x y) z)
                                            (getY (make-subguy x (* y z))))
                         ()
                         (minus (y) (- new-x y))])
        (let ([supe (make-superguy 2 1)]
              [sub (make-subguy 3 4)]
              [subsub (make-subsubguy 1 2 3)])
          (letrec ([map (lambda (p ls)
                          (if (null? ls)
                              '()
                              (cons (p (car ls))
                                (map p (cdr ls)))))])
            (let ([true (if (superguy? supe)
                            (if (superguy? sub)
                                (if (superguy? subsub)
                                    (if (subguy? sub)
                                        (if (subguy? subsub)
                                            (if (subsubguy? subsub)
                                                (if (not (subsubguy? sub))
                                                    (if (not (subsubguy? supe))
                                                        (not (subguy? supe))
                                                        #f)
                                                    #f)
                                                #f)
                                            #f)
                                        #f)
                                    #f)
                                #f)
                            #f)]
                  [x-es (map (lambda (obj)
                               (getX obj))
                          (cons supe (cons sub (cons subsub '()))))]
                  [y-es (map (lambda (obj)
                               (getY obj))
                          (cons supe (cons sub (cons subsub '()))))])
              (cons true (cons x-es y-es))))))))
  '(#t (4 36 144) 3 3 3))

  (equal?
    (letrec-classes ([NullEntity () (<root>)
                       ()
                       (notifyme (evt) (if #f #f))])
      (letrec-classes ([SchmentityEntity (int) (NullEntity)
                         ([value int])
                         (notifyme (evt)
                           (evt value))]
                       [Pool (size) (<root>)
                         ([numElements 0]
                          [pool (make-vector size)]
                          [observers (cons (make-NullEntity) '())])
                         (add (item)
                           (begin
                             (incrementElements self)
                             (if (not (< (getCurrentIndex self) (vector-length pool)))
                                 ;; need to re-expand the pool
                                 (let ([newPool (make-vector
                                                  (* (getSize self)
                                                    (getLoadFactor self)))])
                                   (letrec ([loop
                                              (lambda (n)
                                                (if (= n (getCurrentIndex self))
                                                    (begin
                                                      (vector-set! newPool n item)
                                                      newPool)
                                                    (begin
                                                      (vector-set! newPool n
                                                        (vector-ref pool n))
                                                      (loop (add1 n)))))])
                                     (setPool self (loop 0))))
                                 (vector-set! pool (getCurrentIndex self) item))))
                         (remove (item)
                           (letrec ([loop
                                      (lambda (n)
                                        (if (not (= n (getNumElements self)))
                                            (if (= (vector-ref pool n) item)
                                                (letrec
                                                    ([shift
                                                       (lambda (start)
                                                         (if (= start (getCurrentIndex self))
                                                             (vector-set! pool
                                                               start (void))
                                                             (begin
                                                               (vector-set! pool start
                                                                 (vector-ref pool (+ start 1)))
                                                               (shift (+ n 1)))))])
                                                  (shift n))
                                                (decrementElements self))
                                            (loop (+ n 1))))])
                             (loop 0)))
                         (isEmpty () (= (getNumElements self) 0))
                         (getCurrentIndex () (- (getNumElements self) 1))
                         (getSize () (vector-length pool))
                         (getNumElements () numElements)
                         (incrementElements ()
                           (set! numElements (+ numElements 1)))
                         (decrementElements ()
                           (if (not (= (getNumElements self) 0))
                               (set! numElements (- numElements 1))))
                         (getPool () pool)
                         (setPool (newPool)
                           (set! pool newPool))
                         (notify (evt)
                           (letrec ([loop
                                      (lambda (ls)
                                        (if (null? ls)
                                            '()
                                            (cons (notifyme (car ls) evt)
                                              (loop (cdr ls)))))])
                             (loop observers)))
                         (subscribe (obj)
                           (set! observers (cons obj observers)))
                         (purgeObservers ()
                           (set! observers (cons (make-NullEntity) '())))
                         (getLoadFactor () 2)
                         (contains (item)
                           (letrec ([loop
                                      (lambda (n)
                                        (if (< n (getNumElements self))
                                            (if (= item (vector-ref pool n))
                                                #t
                                                (loop (+ n 1)))
                                            #f))])
                             (loop 0)))])
        (let ([pool (make-Pool 37)])
          (letrec ([addToPool (lambda (n)
                                (if (= n 0)
                                    (isEmpty pool)
                                    (begin
                                      (add pool n)
                                      (addToPool (sub1 n)))))])
            (addToPool 42)
            (addToPool 23)
            (remove pool 14)
            (subscribe pool (make-SchmentityEntity 23))
            (subscribe pool (make-SchmentityEntity 14))
            (let ([notified (notify pool (lambda (x) (* x 3)))])
              (cons (isEmpty pool)
                (cons (getCurrentIndex pool)
                  (cons (getPool pool)
                    (cons (getNumElements pool)
                      (cons (contains pool 23)
                        (cons (contains pool 15)
                          notified)))))))))))
  ; should evaluate to (#f 63 #74(some-huge-vector-with-65-elements) 64 #t #t 42 69 #<void>)
    `(#f 63 #74(42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0) 64 #t #t 42 69 ,(void)))

  (equal?
    (letrec-classes ([broken () (<root>)
                       ([vec (make-vector 5)])
                       (object-or-vector (n)
                         (if (< n 7)
                             (begin
                               (vector-set! vec 0 14)
                               (vector-set! vec 1 15)
                               (vector-set! vec 2 16)
                               vec)
                             self))])
      (letrec-classes ([fixed-broken (num) (broken)
                         ()
                         (object-or-vector (n)
                           (* n n))])
        (let ([all-k (make-broken)])
          (let ([still-valid1 (if (vector? (object-or-vector all-k 4))
                                  (vector-ref (object-or-vector all-k 4) 0)
                                  #f)]
                [still-valid2 (if (vector? (object-or-vector all-k 5))
                                  (vector-ref (object-or-vector all-k 5) 1)
                                  #f)]
                [still-valid3 (if (vector? (object-or-vector all-k 6))
                                  (vector-ref (object-or-vector all-k 6) 2)
                                  #f)]
                [is-vector? (vector? (object-or-vector all-k 7))]
                [fixed (make-fixed-broken 37)])
            (cons (object-or-vector fixed 37)
              (cons still-valid1
                (cons still-valid2
                  (cons still-valid3
                    (cons is-vector? '())))))))))
    ;; should return the list (14 15 16 #t)
    '(1369 14 15 16 #f))


  ;; Matthew Garrett

  ;;; eopl-tests.ss

  ;;; These test cases are translated as directly as possible from "Essentials
  ;;; of Programming Languages", 2nd Ed. by Friedman, Wand, and Haynes, Chapter
  ;;; 5, Objects and Classes.

  (equal?
    ;;; Figure 5.1, A simple object-oriented program
    (letrec-classes
      ([c1 (x) (<root>)
        ([i x] [j (- 0 x)])
        (countup (d)
          (set! i (+ i d))
          (set! j (- j d)))
        (getstate () (list i j))])
      (let ([t1 0] [t2 0] [o1 (make-c1 3)])
        (set! t1 (getstate o1))
        (countup o1 2)
        (set! t2 (getstate o1))
        (list t1 t2)))
  '((3 -3) (5 -5)))

  (eq?
    ;;; page 172, odd-even
    (letrec-classes
      ([oddeven () (<root>)
        ()
        (even (n)
          (if (zero? n)
              1
              (odd self (sub1 n))))
        (odd (n)
          (if (zero? n)
              0
              (even self (sub1 n))))])
      (let ([o1 (make-oddeven)])
        (odd o1 13)))
  1)

  (eq?
    ;;; Figure 5.2 Object-oriented program for summing the leaves of a tree
    (letrec-classes ([<newroot> () (<root>) () [sum () (void)]])
      (letrec-classes
        ([interior_node (l r) (<newroot>)
          ([left l] [right r])
          (sum ()
            (+ (sum left) (sum right)))]
         [leaf_node (v) (<newroot>)
          ([value v])
          (sum () value)])
        (let ([o1 (make-interior_node
                    (make-interior_node
                      (make-leaf_node 3)
                      (make-leaf_node 4))
                    (make-leaf_node 5))])
          (sum o1))))
  12)

  (equal?
    ;;; Figure 5.3 Classic example of inheritance: colorpoint
    (letrec-classes
      ([point (initx inity) (<root>)
        ([x initx] [y inity])
        (move (dx dy)
          (set! x (+ x dx))
          (set! y (+ y dy)))
        (get_location () (list x y))]
       [colorpoint (initx inity) (point initx inity)
        ([color 0])
        (set_color (c) (set! color c))
        (get_color () color)])
      (let ([p  (make-point       3  4)]
            [cp (make-colorpoint 10 20)])
        (move p 3 4)
        (set_color cp 87)
        (move cp 10 20)
        (list (get_location p) (get_location cp) (get_color cp))))
        ;;; should return '((6 8) (20 40) 87)
  '((6 8) (20 40) 87))

  #;
    (equal?
      ;;; page 175, shadowing
      (letrec-classes
        ([c1 () (<root>)
          ([x 0] [y 0])
          (setx1 (v) (set! x v))
          (sety1 (v) (set! y v))
          (getx1 () x)
          (gety1 () y)])
        (letrec-classes
          ([c2 () (c1)
            ([y2 0])
            (sety2 (v) (set! y2 v))
            (getx2 () (open-instance c1 "" self) x)
            (gety2 () y2)])
          (let ([o2 (make-c2)])
            (setx1 o2 101)
            (sety1 o2 102)
            (sety2 o2 999)
            (list (getx1 o2) (gety1 o2) (getx2 o2) (gety2 o2)))))
          ;;; should return '(101 102 101 999)
  '(101 102 101 999))

  (equal?
    ;;; page 176, redefining methods
    (letrec-classes
      ([c1 () (<root>)
        ()
        (m1 () 1)
        (m2 () (m1 self))]
       [c2 () (c1)
        ()
        (m1 () 2)])
      (let ([o1 (make-c1)] [o2 (make-c2)])
        (list (m1 o1) (m1 o2) (m2 o2))))
  '(1 2 2))

  (equal?
    ;;; Figure 5.4 Example illustrating interaction of self and inheritance
    (letrec-classes
      ([c1 () (<root>)
        ()
        (m1 () 1)
        (m2 () 100)
        (m3 () (m2 self))]
       [c2 () (c1)
        ()
        (m2 () 2)])
      (let ([o1 (make-c1)] [o2 (make-c2)])
        (list (m1 o1)     ; 1
              (m2 o1)     ; 100
              (m3 o1)     ; 100
              (m1 o2)     ; 1 (from c1)
              (m2 o2)     ; 2 (from c2)
              (m3 o2))))  ; 2 (c1's m3 calls c2's m2)
  '(1 100 100 1 2 2))

  (eq?
    ;;; Figure 5.5 Example demonstrating a need for static method dispatch
    (letrec-classes
      ([point (initx inity) (<root>)
        ([x initx] [y initx])
        (move (dx dy)
          (set! x (+ x dx))
          (set! y (+ y dy)))
        (getlocation ()
          (list x y))]
       [colorpoint (initx inity initcolor) (point 0 0)
        ([color initcolor])
        (set_color (c) (set! color c))
        (get_color () color)])
      (let ([o1 (make-colorpoint 3 4 172)])
        (get_color o1)))
  172)

  (eq?
    ;;; Figure 5.6 Example illustrating interaction of super call with self
    (letrec-classes
      ([c1 () (<root>)
        ()
        (m1 () (m2 self))
        (m2 () 13)])
      (letrec-classes
        ([c2 () (c1)
          ()
          (m1 () (super))
          (m2 () 23)
          (m3 () (m1 self))])
        (letrec-classes
          ([c3 () (c2)
            ()
            (m1 () (super))
            (m2 () 33)])
          (let ([o3 (make-c3)])
            (m3 o3)))))
  33)

  ; Jeremiah Willcock

  (eq?
  (let ()
    (define-class (A n) (<root>)
      (ivars [next (foo n)])
      (methods
        [get-next () next]
        [get-length ()
          (if (null? next) 0 (+ 1 (get-length (get-next self))))]))
    (define (foo n) (if (zero? n) '() (make-A (- n 1))))
    (let ((a (make-A 10)))
      (get-length a)))
  10)

  (eq?
  (letrec-classes ((A (n) (<root>) ((next
                                        (if (zero? n) '()
                                          (make-A (- n 1)))))
                                       (get-next () next)
                                       (get-length ()
                                        (if (null? next) 0
                                          (+ 1 (get-length (get-next self)))))))
        (let ((a (make-A 10)))
          (get-length a)))
  10)

  ; should this really be an error?  It's not clear how to make base ivars
  ; visible in ivar inits efficiently if we want to do so.
  (error? ; variable oop-x1 is not bound
    (let ()
      (define-class (<a> oop-x) (<root>) (ivars [oop-x1 oop-x]))
      (define-class (<b> oop-x) (<a> oop-x) (ivars [oop-x2 (+ oop-x1 oop-x1)]))
      (define-class (<c> oop-x) (<b> oop-x) (ivars [oop-x3 (+ oop-x2 oop-x2)]))
      (define-class (<d> oop-x) (<c> oop-x) (ivars [oop-x4 (+ oop-x3 oop-x3)]))
      (define-class (<e> oop-x) (<d> oop-x) (ivars [oop-x5 (+ oop-x4 oop-x4)]))
      (define-class (<f> oop-x) (<e> oop-x) (ivars [oop-x6 (+ oop-x5 oop-x5)]))
      (define-class (<g> oop-x) (<f> oop-x) (ivars [oop-x7 (+ oop-x6 oop-x6)]))
      (define-class (<h> oop-x) (<g> oop-x) (ivars [oop-x8 (+ oop-x7 oop-x7)]))
      (define-class (<i> oop-x) (<h> oop-x) (ivars [oop-x9 (+ oop-x8 oop-x8)]) (methods [m () oop-x9]))
      (m (make-<i> 1))))

  (eq?
    (let ()
      (define-class (<a> x0) (<root>)
        (ivars [x1 (+ x0 x0)]
               [x2 (+ x1 x1)]
               [x3 (+ x2 x2)]
               [x4 (+ x3 x3)]
               [x5 (+ x4 x4)]
               [x6 (+ x5 x5)]
               [x7 (+ x6 x6)]
               [x8 (+ x7 x7)]
               [x9 (+ x8 x8)])
        (methods [m () x9]))
      (m (make-<a> 1)))
  512)


  ; Abdulaziz Ghuloum

  (begin
    (define-class (R) (<root>))
    #t)
  (eq?  (R? 0) #f)

  (eq?  (R? (cons 1 2)) #f)

  (eq?  (R? (make-vector 2)) #f)

  (eq?  (R? (lambda () 4)) #f)

  (eq?  (R? #f) #f)

  (eq?  (R? #t) #f)

  (eq?  (R? '()) #f)

  (equal?
    (letrec-classes
      ([AA () (<root>)
        ([x 0][y 0])
            (get-x () x)
            (get-y () y)
            (set-x (a)
              (letrec-classes
                ([AA () (<root>)
                      ()
                      (set-x (a) (set! x a))])
                    (set-x (make-AA) a)))
            (set-y (a)
              (letrec-classes
                ([AA () (<root>)
                      ([y 0])
                      (set-y (a) (set! y a))])
                    (set-y (make-AA) a)))])
      (let ([a (make-AA)])
        (set-x a 5)
            (set-y a 0)
            (cons (get-x a) (get-y a))))
    '(5 . 0))

  (eq?
    (letrec-classes
      ([<pair> (a b) (<root>)
        ([a a][b b])
            (car () a)
            (cdr () b)
            (set-car! (x) (set! a x))
            (set-cdr! (x) (set! b x))])
      (let ([cons (lambda (a b) (make-<pair> a b))]
            [pair? (lambda (x) (<pair>? x))])
            (let ([x (cons 4 5)])
              (let ([y (cons 3 4)])
            (set-car! y 12)
                (set-cdr! x y)
                (let ([cdr (cdr x)])
                  (if (pair? cdr)
                    (let ([car (car cdr)])
                          (if (pair? car) #f car))))))))
    12)

  ; this doesn't test the oop system at all:
  #;
  (equal?
    (let
      ([letrec-classes
        (lambda (x y) (cons x y))]
       [<root> (lambda () 7)]
       [y (lambda () 3)]
       [x (lambda (y) y)]
       [self (cons 12 (cons 34 45))]
       [A (lambda (a b c d)
            (lambda ()
                      (let ([v (make-vector 4)])
                        (vector-set! v 0 a)
                        (vector-set! v 1 b)
                        (vector-set! v 2 c)
                        (vector-set! v 3 d)
                            v)))]
       [make (lambda (a b) b)]
       [inc-x (lambda (a b) (cons a b))]
       [s (lambda () 87)])
      (letrec-classes
        ([A (y) (<root>)
              ([x y])
              (inc-x (s) (begin self))])
            (inc-x (make-A 3) 3)))
  '(#4(3 7 3 (87 12 34 . 45)) 3 . 3))

  (eq?
    (let ([let 0][lambda 1][letrec 2][if 5])
      (letrec-classes
        ([A (x) (<root>)
              ([x x])
              (inc-x (s) (begin (set! x (+ x s)) self))
              (get-x () x)])
            (get-x (inc-x (make-A 4) 3))))
    7)

  ; Jeremiah Willcock

  (eq?
    (letrec-classes () #f)
    #f)

  (eq?
    (letrec-classes ((A () (<root>) ()))
      (letrec-classes ((B () (A) ()))
        (make-B)
        #f))
    #f)

  (eq?
    (letrec-classes ((A () (<root>) () (foo () 5)))
      (letrec-classes ((B () (A) () (foo () 7) (bar () 8)))
        (foo (make-B))))
    7)

  (equal?
    (letrec-classes
      ((A (x y) (<root>) ((x x) (y y))
        (get-x () x)
        (get-y () y)
        (set-x (value) (set! x value))))
      (let ((A (make-A 1 2)))
        (cons (get-x A) (get-y A))))
    '(1 . 2))

  (eq?
    (letrec-classes ((A () (<root>) ()))
      (letrec-classes ((<root> () (A) () (foo () 5)))
        (foo (make-<root>))))
    5)

  #;
  (equal?
    (letrec-classes
      ((A (x y) (<root>) ((x (* 2 x)) (y (+ 7 y)))
        (get-x () (- x 3))
        (get-y () (* 2 y))))
      (letrec-classes
        ((<xroot> (z w) (A (* w z) (- w z)) ()
          (get-x () (open-instance A "" self) x)
          (set-x! (<yroot>) (open-instance A "" self) (set! x <yroot>))
          (call-get-x (set-x!) (get-x set-x!))
          (call-set-x! (A) (set-x! self A))))
        (let ((<zroot> (make-<xroot> 4 9)))
          (let ((x (get-x <zroot>))
                (y (get-y <zroot>))
                (x2 (call-get-x <zroot> <zroot>)))
            (cons x
              (cons y
                (cons x2
                  (let ((foo (set-x! <zroot> 7)))
                    (cons (get-x <zroot>) '())))))))))
    '(72 24 72 7))

  #;
  (equal?
    (letrec-classes
      ((A (x y) (<root>) ((x (* 2 x)) (y (+ 7 y)))
        (get-x () (- x 3))
        (get-y () (* 2 y))))
      (letrec-classes
        ((<xroot> (z w) (A (* w z) (- w z)) ()
          (get-x () (open-instance A "" self) x)
          (set-x! (<yroot>) (open-instance A "" self) (set! x <yroot>))
          (call-get-x (set-x!) (get-x set-x!))
          (call-set-x! (A) (set-x! self A))))
        (let ((<root> (make-<xroot> 4 9)))
          (let ((x (get-x <root>))
                (y (get-y <root>))
                (x2 (call-get-x <root> <root>)))
            (cons x
              (cons y
                (cons x2
                  (let ((foo (set-x! <root> 7)))
                    (cons (get-x <root>) '())))))))))
    '(72 24 72 7))

  #;
  (equal?
    (letrec-classes
      ((A (x y) (<root>) ((x (* 2 x)) (y (+ 7 y)))
        (get-x () (- x 3))
        (get-y () (* 2 y))))
      (letrec-classes
        ((<xroot> (z w) (A (* w z) (- w z)) ()
          (get-x () (open-instance A "" self) x)
          (set-x! (<root>) (open-instance A "" self) (set! x <root>))
          (call-get-x (set-x!) (get-x set-x!))
          (call-set-x! (A) (set-x! self A))))
        (let ((<root> (make-<xroot> 4 9)))
          (let ((x (get-x <root>))
                (y (get-y <root>))
                (x2 (call-get-x <root> <root>)))
            (cons x
              (cons y
                (cons x2
                  (let ((foo (set-x! <root> 7)))
                    (cons (get-x <root>) '())))))))))
    '(72 24 72 7))

  (eq?
    (letrec-classes ((A (x y) (<root>) ((x (* 2 x)) (y (+ 7 y)))
                      (get-x () x)))
      (letrec-classes
        ((<root> (z w) (A (* w z) (- w z)) ()
          (get-x () 7)
          (call-get-x (set-x!) (get-x set-x!))))
        (let ((<root> (make-<root> 4 9)))
          (call-get-x <root> <root>))))
    7)

  (eq?
    (letrec-classes ((A () (<root>) ()))
      (letrec-classes
        ((<root> () (A) ()
          (set-x! (<root>) #f)))
        #f))
    #f)

  #;
  (equal?
    (letrec-classes
      ((A (x y) (<root>) ((x (* 2 x)) (y (+ 7 y)))
        (get-x () (- x 3))
        (get-y () (* 2 y))))
      (letrec-classes
        ((<root> (z w) (A (* w z) (- w z)) ()
          (get-x () (open-instance A "" self) x)
          (set-x! (<root>) (open-instance A "" self) (set! x <root>))
          (call-get-x (set-x!) (get-x set-x!))
          (call-set-x! (A) (set-x! self A))))
        (let ((<root> (make-<root> 4 9)))
          (let ((x (get-x <root>))
                (y (get-y <root>))
                (x2 (call-get-x <root> <root>)))
            (cons x
              (cons y
                (cons x2
                  (let ((foo (set-x! <root> 7)))
                    (cons (get-x <root>) '())))))))))
    '(72 24 72 7))

  (equal?
    (letrec-classes
      ((xself () (<root>) ((x 7))
        (xisa-vtable? () (let ((self 5)) (+ x self)))))
      (cons
        (xisa-vtable? (make-xself))
        (xself? (make-xself))))
    '(12 . #t))

  (equal?
    (letrec-classes
      ((xself () (<root>) ((x 7))
        (isa-vtable? () (let ((self 5)) (+ x self)))))
      (cons
        (isa-vtable? (make-xself))
        (xself? (make-xself))))
    '(12 . #t))

  (equal?
    (letrec-classes ; Cannot have class named "self"
      ((self () (<root>) ((x 7))
        (isa-vtable? () (let ((self 5)) (+ x self)))))
      (cons
        (isa-vtable? (make-self))
        (self? (make-self))))
    '(12 . #t))

  (eq?
    (let ((self 5))
      (letrec-classes ((A () (<root>) ()
                        (foo () self)
                        (bar () 5)))
        (let ((self 7))
          (bar (foo (make-A))))))
    5)

  (equal?
    (letrec-classes ((A () (<root>) ())
                     (B () (<root>) ()))
      (letrec-classes ((C () (A) ()))
        (letrec-classes ((D () (C) ()))
          (let ((isa-grid-entry (lambda (obj)
                    (cons (A? obj)
                    (cons (B? obj)
                    (cons (C? obj)
                    (cons (D? obj) '())))))))
            (letrec ((map (lambda (f l)
                      (if (null? l) '()
                        (cons (f (car l))
                              (map f (cdr l)))))))
              (map isa-grid-entry
                (cons 5
                (cons (make-A)
                (cons (make-B)
                (cons (make-C)
                (cons (make-D) '())))))))))))
    '((#f #f #f #f)
      (#t #f #f #f)
      (#f #t #f #f)
      (#t #f #t #f)
      (#t #f #t #t)))

  (equal?
    (letrec-classes ((A () (<root>) ()))
      (let ((z (make-A)))
        (cons (pair? z)
        (cons (vector? z)
        (cons (null? z)
        (cons (procedure? z)
        (cons (boolean? z)
        '())))))))
    '(#f #f #f #f #f))

  #;
  (equal?
    (let ((x 7))
      (letrec-classes ((A () (<root>) ((y x))))
        (letrec-classes ((B () (A) () (set-x (value) (set! x value))
                                      (get-y () (open-instance A "" self) y)))
          (let ((w (make-B)))
            (set-x w 9)
            (cons (get-y w) x)))))
    '(7 . 9))

  (equal?
    (letrec-classes ((A () (<root>) () (x () 1) (y () 2)))
      (letrec-classes ((B () (A) () (x () (- 0 (super))) (z () 3)))
        (letrec-classes ((C () (B) () (y () (+ 10 (super)))
                                      (z () (- 0 (super)))))
          (let ((a (make-A)) (b (make-B)) (c (make-C)))
            (cons
              (cons (x a) (y a))
            (cons
              (cons (x b) (cons (y b) (z b)))
            (cons
              (cons (x c) (cons (y c) (z c))) '())))))))
    '((1 . 2) (-1 2 . 3) (-1 12 . -3)))

  (eq?
    ; Based on suggestion in class about constructors making the same class
    (letrec-classes ((A (n) (<root>) ((next
                                      (if (zero? n) '()
                                        (make-A (- n 1)))))
                                     (get-next () next)
                                     (get-length ()
                                      (if (null? next) 0
                                        (+ 1 (get-length (get-next self)))))))
      (let ((a (make-A 10)))
        (get-length a)))
    10)

  (equal?
    ; Automatic differentiator -- expressions of one variable w/ int constants
    (letrec-classes ((Differentiable () (<root>) () (compute () #f)
                                                    (diff () #f)))
      (letrec-classes (
        (sum (a b) (Differentiable) ((a a) (b b))
                                    (compute ()
                                      (lambda (x)
                                        (+ ((compute a) x)
                                           ((compute b) x))))
                                    (diff () (make-sum (diff a) (diff b))))
        (prod (a b) (Differentiable) ((a a) (b b))
                                     (compute ()
                                      (lambda (x)
                                        (* ((compute a) x)
                                           ((compute b) x))))
                                     (diff () (make-sum
                                      (make-prod a (diff b))
                                      (make-prod b (diff a)))))
        (pow (a b) (Differentiable) ((a a) (b b)) ; Constant exponent
                                    (compute ()
                                      (letrec ((real-pow
                                        (lambda (base power)
                                          (if (zero? power)
                                            1
                                            (* base
                                               (real-pow base (- power 1)))))))
                                        (lambda (x)
                                          (real-pow ((compute a) x) b))))
                                    (diff ()
                                      (if (zero? b)
                                        (make-constant 0)
                                        (make-prod (make-constant b)
                                          (make-prod
                                           (make-pow a (- b 1))
                                           (diff a))))))
        (constant (x) (Differentiable) ((x x))
                                       (compute ()
                                        (lambda (z) x))
                                       (diff () (make-constant 0)))
        (variable () (Differentiable) ()
                                      (compute ()
                                        (lambda (x) x))
                                      (diff () (make-constant 1))))
      (let ((+ (lambda (a b) (make-sum a b)))
            (- (lambda (a b) (make-sum a (make-prod b (make-constant -1)))))
            (* (lambda (a b) (make-prod a b)))
            (^ (lambda (a b) (make-pow a b)))
            (! (lambda (x) (make-constant x)))
            (x (make-variable)))
        (let ((fun (+ (^ (- x (! 1)) 9) (* x (! 7)))))
          (letrec ((diff-at-values (lambda (fun ndiffs vals)
                    (if (zero? ndiffs)
                      '()
                      (cons
                        (letrec ((map (lambda (f l)
                                  (if (null? l) '()
                                    (cons (f (car l))
                                          (map f (cdr l)))))))
                          (map (compute fun) vals))
                        (diff-at-values (diff fun) (sub1 ndiffs) vals))))))
            (diff-at-values fun 4 '(-5 -4 -3 -2 -1 0 1 2 3 4 5)))))))
    '((-10077731 -1953153 -262165 -19697 -519 -1 7 15 533 19711 262179)
      (15116551 3515632 589831 59056 2311 16 7 16 2311 59056 589831)
      (-20155392 -5625000 -1179648 -157464 -9216 -72 0 72 9216 157464 1179648)
      (23514624 7875000 2064384 367416 32256 504 0 504 32256 367416 2064384)))

  ; Robert George

  (eq?
    (letrec-classes ([A () (<root>)
                       ([x 1]
                        [y (letrec-classes ([B () (<root>)
                                              ([x2 2])
                                              (get-x () (if x2 x2 (letrec-classes ([C () (<root>)
                                                                                     ([x3 3])
                                                                                     (get-x () x3)])
                                                                    (let ([obj (make-C)])
                                                                      (get-x obj)))))])
                             (let ([obj (make-B)])
                               (get-x obj)))])
                       (get-x () x)])
      (get-x (make-A)))
    1)

  (equal?
    (letrec ([map (lambda (proc ls)
                    (if (null? ls)
                        '()
                        (cons (proc (car ls)) (map proc (cdr ls)))))])
      (letrec-classes ([A () (<root>)
                         ([x 0])
                         (square-and-set (y) (let ([val (* y y)])
                                               (set! x (+ x val))
                                               val))
                         (get-x () x)])
        (let ([obj (make-A)])
          (let ([ls (map (lambda (x) (square-and-set obj x)) '(1 2 3 4 5))])
            (cons ls (get-x obj))))))
    '((1 4 9 16 25) . 55))

  #;
  (eq?
    (let ([x 5])
      (letrec-classes ([A () (<root>)
                         ([x 3])
                         (get-x () x)])
        (letrec-classes ([B () (A)
                          ([y 4])
                          (get-x () (open-instance A "" self) x)])
          (+ x (get-x (make-B)) (get-x (make-A))))))
    11)

  (eq?
    (letrec-classes ([A () (<root>)
                       ([x 1] [y (+ x x)])
                       (get-x () x)
                       (get-y () y)])
      (letrec-classes ([B () (A)
                        ([z 3])
                        (add-em () (+ (get-x (make-A)) (get-x (make-B)) z))])
        (add-em (make-B))))
    5)

  (eq?
    (letrec-classes ([A () (<root>)
                       ([x 1])
                       (get-x () x)])
      (letrec-classes ([B () (A)
                        ([y 3])
                        (get-x () (super))])
        (get-x (make-B))))
    1)

  (eq?
    (letrec-classes ([A () (<root>)
                       ([x 1])
                       (create-A () (make-A))])
      (letrec-classes ([B () (A)
                        ([y 2])
                        (do-it () (A? (create-A (make-B))))])
        (do-it (make-B))))
    #t)

  (eq?
    (let ()
      (define-syntax albatross
        (syntax-rules ()
          [(_ f m)
           (begin
             (define-class (fowl) (<root>) (ivars [x 77]) (methods [m () x]))
             (define f (lambda () (make-fowl))))]))
      (albatross alcatraz pelican)
      (pelican (alcatraz)))
    77)

  (error? ; variable make-fowl is not bound
    (make-fowl))

  ; Jeremy Siek

  (eq?
    (letrec-classes ((shape () (<root>) () (foo (s) s)))
      (letrec-classes ((rect () (shape) () (get-h () 0) (foo (s) s)))
        (let ([r (make-rect)]) (get-h r))))
    0)
)
