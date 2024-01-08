;;; 5_6.ms
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

(mat vector
    (equal? (vector 1 2 3 4) '#(1 2 3 4))
    (eq? (vector) '#())
    )

(mat immutable-vector
  (equal? (immutable-vector 1 2 3 4) (vector->immutable-vector '#(1 2 3 4)))
  (eq? (immutable-vector) (vector->immutable-vector '#()))
  (begin
    (define (f x)
      (let ([v (immutable-vector (begin (write 'a) (+ x 1)) (begin (write 'a) 2) (begin (write 'a) (cons 3 x)))])
        (for-each write
          (list
           (begin (write 'b) (vector-ref v 0))
           (begin (write 'b) (vector-ref v 1))
           (begin (write 'b) (vector-ref v 2))))))
    #t)
  (equal?
   (with-output-to-string (lambda () (f 7)))
   "aaabbb82(3 . 7)")
  )

(mat make-vector
    (eqv? (vector-length (make-vector 10)) 10)
    (eqv? (vector-length (make-vector 100)) 100)
    (eqv? (vector-length (make-vector (+ 100 17))) 117)
    (equal? (make-vector 0) '#())
    (equal? (make-vector 3 'a) '#(a a a))
    (equal? (make-vector 10 '#t) (vector #t #t #t #t #t #t #t #t #t #t))
    (equal? (make-vector (- 4 2) (+ 1 1)) (vector 2 2))
    (eqv? (make-vector (- 4 4) (+ 1 1)) (vector))
    (error? (make-vector 'a 23))
 )

(mat vector-length
    (eqv? (vector-length '#(a b c)) 3)
    (eqv? (vector-length '#100(a b c)) 100)
    (eqv? (vector-length '#()) 0)
    (error? (vector-length '(a b c)))
 )

(mat $vector-ref-check?
  (let ([v (make-vector 3)] [imm-v (vector->immutable-vector (make-vector 3))] [not-v (make-fxvector 3)])
    (let ([i-1 -1] [i0 0] [i1 1] [i2 2] [i3 3] [ifalse #f] [ibig (+ (most-positive-fixnum) 1)])
      (and
        (not (#%$vector-ref-check? not-v i0))
        (not (#%$vector-ref-check? v ifalse))
        (not (#%$vector-ref-check? imm-v ifalse))
        (not (#%$vector-ref-check? v i-1))
        (not (#%$vector-ref-check? imm-v i-1))
        (#%$vector-ref-check? v 0)
        (#%$vector-ref-check? v 1)
        (#%$vector-ref-check? v 2) 
        (#%$vector-ref-check? imm-v 0)
        (#%$vector-ref-check? imm-v 1)
        (#%$vector-ref-check? imm-v 2)
        (#%$vector-ref-check? v i0)
        (#%$vector-ref-check? v i1)
        (#%$vector-ref-check? v i2) 
        (#%$vector-ref-check? imm-v i0)
        (#%$vector-ref-check? imm-v i1)
        (#%$vector-ref-check? imm-v i2)
        (not (#%$vector-ref-check? v 3))
        (not (#%$vector-ref-check? v i3))
        (not (#%$vector-ref-check? v ibig))
        (not (#%$vector-ref-check? imm-v 3))
        (not (#%$vector-ref-check? imm-v i3))
        (not (#%$vector-ref-check? imm-v ibig)))))
 )


(mat vector-ref
    (eqv? (vector-ref '#(a b c) 0) 'a)
    (eqv? (vector-ref '#(a b c) 1) 'b)
    (eqv? (vector-ref '#(a b c) 2) 'c)
    (error? (vector-ref '#(a b c) 3))
    (error? (vector-ref '#(a b c) -1))
    (error? (vector-ref '#(a b c) 'a))
    (error? (vector-ref '(a b c) 2))
 )

(mat vector-set!
    (let ((v (vector 'a 'b 'c)))
        (and
            (begin (vector-set! v 0 'x) (equal? v '#(x b c)))
            (begin (vector-set! v 1 'y) (equal? v '#(x y c)))
            (begin (vector-set! v 2 'z) (equal? v '#(x y z)))))
    (error? (vector-set! (vector 'a 'b 'c) 3 'd))
    (error? (vector-set! (vector 'a 'b 'c) -1 'd))
    (error? (vector-set! (vector 'a 'b 'c) 'a 'd))
    (error? (vector-set! (list 'a 'b 'c) 2 'd))
 )

(mat vector-set-fixnum!
    (let ((v (vector 'a 'b 'c)))
        (and
            (begin (vector-set-fixnum! v 0 5) (equal? v '#(5 b c)))
            (begin (vector-set-fixnum! v 1 6) (equal? v '#(5 6 c)))
            (begin (vector-set-fixnum! v 2 7) (equal? v '#(5 6 7)))))
    (let ((v (vector 'a 'b 'c)) (n -1))
        (and
            (begin (set! n (+ n 1)) (vector-set-fixnum! v n (+ (* n n) 1)) (equal? v '#(1 b c)))
            (begin (set! n (+ n 1)) (vector-set-fixnum! v n (+ (* n n) 1)) (equal? v '#(1 2 c)))
            (begin (set! n (+ n 1)) (vector-set-fixnum! v n (+ (* n n) 1)) (equal? v '#(1 2 5)))))
    (error? (vector-set-fixnum! (vector 'a 'b 'c) 3 0))
    (error? (vector-set-fixnum! (vector 'a 'b 'c) -1 3))
    (error? (vector-set-fixnum! (vector 'a 'b 'c) 'a 4))
    (error? (vector-set-fixnum! (list 'a 'b 'c) 2 5))
    (error? (vector-set-fixnum! (vector 'a 'b 'c) 2 'd))
    (error? (vector-set-fixnum! (vector 'a 'b 'c) 2 #\d))
    (error? (let ([v (vector 'a 'b 'c)] [n -1] [x '(a b c)])
              (set! n (+ n 2))
              (vector-set-fixnum! v n x)))
 )

(mat vector-copy
    (eq? (vector-copy '#()) '#())
    (equal? (vector-copy '#(a b c)) '#(a b c))
    (equal? (vector-copy '#(a b c) 0 1) '#(a))
    (equal? (vector-copy '#(a b c) 2 1) '#(c))
    (equal? (vector-copy '#(a b c d) 1 2) '#(b c))
    (eq? (vector-copy '#(a b c d) 1 0) '#())
    (mutable-vector? (vector-copy '#(a b c)))
    (mutable-vector? (vector-copy '#(a b c) 0 1))
    (mutable-vector? (vector-copy '#(a b c) 2 1))
    (mutable-vector? (vector-copy '#(a b c d) 1 2))
    (let* ((x1 (vector 1 2 3)) (x2 (vector-copy x1)))
        (and (equal? x2 x1) (not (eq? x2 x1))))
    (andmap
      (lambda (n)
        (let ([v (vector-map random (make-vector n 1000))])
          (equal? (vector-copy v) v)))
      (map random (make-list 500 2500)))
    (error? (vector-copy '(a b c)))
    (error? (vector-copy '#(a b c) 'x 2))
    (error? (vector-copy '#(a b c) 1 'x))
    (error? (vector-copy '#(a b c) -1 2))
    (error? (vector-copy '#(a b c) 1 3))
    (error? (vector-copy '#(a b c) 2 -1))
    )

(mat immutable-vector-copy
    (eq? (immutable-vector-copy '#()) (immutable-vector))
    (equal? (immutable-vector-copy '#(a b c)) '#(a b c))
    (equal? (immutable-vector-copy '#(a b c) 0 1) '#(a))
    (equal? (immutable-vector-copy '#(a b c) 2 1) '#(c))
    (equal? (immutable-vector-copy '#(a b c d) 1 2) '#(b c))
    (immutable-vector? (immutable-vector-copy '#(a b c)))
    (immutable-vector? (immutable-vector-copy '#(a b c) 0 1))
    (immutable-vector? (immutable-vector-copy '#(a b c) 2 1))
    (immutable-vector? (immutable-vector-copy '#(a b c d) 1 2))
    (eq? (immutable-vector-copy '#(a b c d) 1 0) (immutable-vector))
    (let* ((x1 (vector 1 2 3)) (x2 (immutable-vector-copy x1)))
        (and (equal? x2 x1) (not (eq? x2 x1))))
    (andmap
      (lambda (n)
        (let ([v (vector-map random (make-vector n 1000))])
          (equal? (immutable-vector-copy v) v)))
      (map random (make-list 500 2500)))
    (error? (immutable-vector-copy '(a b c)))
    (error? (immutable-vector-copy '#(a b c) 'x 2))
    (error? (immutable-vector-copy '#(a b c) 1 'x))
    (error? (immutable-vector-copy '#(a b c) -1 2))
    (error? (immutable-vector-copy '#(a b c) 1 3))
    (error? (immutable-vector-copy '#(a b c) 2 -1))
    )

(mat vector-set/copy
    (equal? (vector-set/copy '#(a b c) 0 'x) '#(x b c))
    (equal? (vector-set/copy '#(a b c) 1 'x) '#(a x c))
    (equal? (vector-set/copy '#(a b c) 2 'x) '#(a b x))
    (equal? (vector-set/copy '#(a b c d e f g h i) 2 'x) '#(a b x d e f g h i))
    (mutable-vector? (vector-set/copy '#(a b c) 0 'x))
    (mutable-vector? (vector-set/copy '#(a b c) 1 'x))
    (mutable-vector? (vector-set/copy '#(a b c) 2 'x))
    (mutable-vector? (vector-set/copy '#(a b c d e f g h i) 2 'x))
    (error? (vector-set/copy 1))
    (error? (vector-set/copy '#(a b c)))
    (error? (vector-set/copy '#(a b c) 1))
    (error? (vector-set/copy '#(a b c) 'y 'x))
    (error? (vector-set/copy '#(a b c) -1 'x))
    (error? (vector-set/copy '#(a b c) 3 'x))
 )

(mat immutable-vector-set/copy
    (equal? (immutable-vector-set/copy '#(a b c) 0 'x) '#(x b c))
    (equal? (immutable-vector-set/copy '#(a b c) 1 'x) '#(a x c))
    (equal? (immutable-vector-set/copy '#(a b c) 2 'x) '#(a b x))
    (equal? (immutable-vector-set/copy '#(a b c d e f g h i) 2 'x) '#(a b x d e f g h i))
    (immutable-vector? (immutable-vector-set/copy '#(a b c) 0 'x))
    (immutable-vector? (immutable-vector-set/copy '#(a b c) 1 'x))
    (immutable-vector? (immutable-vector-set/copy '#(a b c) 2 'x))
    (immutable-vector? (immutable-vector-set/copy '#(a b c d e f g h i) 2 'x))
    (error? (immutable-vector-set/copy 1))
    (error? (immutable-vector-set/copy '#(a b c)))
    (error? (immutable-vector-set/copy '#(a b c) 1))
    (error? (immutable-vector-set/copy '#(a b c) 'y 'x))
    (error? (immutable-vector-set/copy '#(a b c) -1 'x))
    (error? (immutable-vector-set/copy '#(a b c) 3 'x))
 )

(mat vector-append
    (eq? (vector-append) '#())
    (eq? (vector-append '#()) '#())
    (eq? (vector-append '#() '#()) '#())
    (eq? (vector-append '#() '#() '#()) '#())
    (eq? (vector-append '#() '#() '#() '#()) '#())
    (equal? (vector-append '#(a b c)) '#(a b c))
    (mutable-vector? (vector-append '#(a b c)))
    (equal? (vector-append '#(a b c) '#(d e)) '#(a b c d e))
    (mutable-vector? (vector-append '#(a b c) '#(d e)))
    (equal? (vector-append '#(a b c) '#(d e) '#(f) '#(g h i)) '#(a b c d e f g h i))
    (mutable-vector? (vector-append '#(a b c) '#(d e) '#(f) '#(g h i)))
    (equal? (vector-append (vector 'p) '#()) '#(p))
    (mutable-vector? (vector-append (vector 'p) '#()))
    (equal? (vector-append '#() (vector 'p)) '#(p))
    (mutable-vector? (vector-append '#() (vector 'p)))
    (equal? (vector-append (vector 'p) '#(a b c)) '#(p a b c))
    (equal? (vector-append '#(a b c) (vector 'p)) '#(a b c p))
    (mutable-vector? (vector-append (vector 'p) '#(a b c)))
    (mutable-vector? (vector-append '#(a b c) (vector 'p)))
    (equal? (vector-append (vector 'p) (make-vector 3 'ok)) '#(p ok ok ok))
    (mutable-vector? (vector-append (vector 'p) (make-vector 3 'ok)))
    (let* ([ip (open-input-string "4 0")]
           [c 0]
           [v0 (vector-append (make-vector (read ip) (begin (set! c (+ c 1)) c)) (vector 'x 2 3 4))]
           [c0 c]
           [v1 (vector-append (make-vector (read ip) (begin (set! c (+ c 1)) c)) (vector 'y 'not))])
      (and (equal? v0 '#(1 1 1 1 x 2 3 4))
           (mutable-vector? v0)
           (= c0 1)
           (equal? v1 '#(y not))
           (mutable-vector? v1)
           (= c 2)))
    (equal? (vector-append (make-vector 1 'p) '#(d q)) '#(p d q))
    (mutable-vector? (vector-append (make-vector 1 'p) '#(d q)))
    (equal? (vector-append (vector 'p) (make-vector 0 'ok)) '#(p))
    (mutable-vector? (vector-append (vector 'p) (make-vector 0 'ok)))
    (error? (vector-append 1))
    (error? (vector-append '#(a b c) 'x))
    (error? (vector-append '#(a b c) '#(d) 'x))
    (error? (vector-append '#(a b c) '#(d) '#(e)'x))
    (error? (vector-append (vector 'p) (make-vector -1 'ok)))

    ;; ensure that `vector-append` optimization doesn't misorder
    ;; evaluation and potential collection with respect to filling
    ;; the vector
    (with-interrupts-disabled
      (letrec ([f (lambda (m)
                    (collect 0 1)
                    (number->string m))]
               [N 1000])
        (let ([v (vector-append (make-vector N)
                                (vector (f 8)))])
          (and (eqv? 0 (#%$generation (vector-ref v N)))
               (eqv? 0 (#%$generation v))
               (begin
                 (collect 0 1)
                 (and (eqv? 1 (#%$generation (vector-ref v N)))
                      (equal? (vector-ref v N) "8")))))))
 )

(mat immutable-vector-append
    (eq? (immutable-vector-append) (immutable-vector))
    (eq? (immutable-vector-append '#()) (immutable-vector))
    (eq? (immutable-vector-append '#() '#()) (immutable-vector))
    (eq? (immutable-vector-append '#() '#() '#()) (immutable-vector))
    (eq? (immutable-vector-append '#() '#() '#() '#()) (immutable-vector))
    (equal? (immutable-vector-append '#(a b c)) '#(a b c))
    (immutable-vector? (immutable-vector-append '#(a b c)))
    (equal? (immutable-vector-append '#(a b c) '#(d e)) '#(a b c d e))
    (immutable-vector? (immutable-vector-append '#(a b c) '#(d e)))
    (equal? (immutable-vector-append '#(a b c) '#(d e) '#(f) '#(g h i)) '#(a b c d e f g h i))
    (immutable-vector? (immutable-vector-append '#(a b c) '#(d e) '#(f) '#(g h i)))
    (equal? (immutable-vector-append (vector 'p) '#()) '#(p))
    (immutable-vector? (immutable-vector-append (vector 'p) '#()))
    (equal? (immutable-vector-append '#() (vector 'p)) '#(p))
    (immutable-vector? (immutable-vector-append '#() (vector 'p)))
    (equal? (immutable-vector-append (vector 'p) '#(a b c)) '#(p a b c))
    (equal? (immutable-vector-append '#(a b c) (vector 'p)) '#(a b c p))
    (immutable-vector? (immutable-vector-append (vector 'p) '#(a b c)))
    (immutable-vector? (immutable-vector-append '#(a b c) (vector 'p)))
    (equal? (immutable-vector-append (vector 'p) (make-vector 3 'ok)) '#(p ok ok ok))
    (immutable-vector? (immutable-vector-append (vector 'p) (make-vector 3 'ok)))
    (equal? (immutable-vector-append (vector 'p) (make-vector 0 'ok)) '#(p))
    (immutable-vector? (immutable-vector-append (vector 'p) (make-vector 0 'ok)))
    (equal? (immutable-vector-append (make-vector 1 'p) '#(d q)) '#(p d q))
    (immutable-vector? (immutable-vector-append (make-vector 1 'p) '#(d q)))
    (let* ([ip (open-input-string "4 0")]
           [c 0]
           [v0 (immutable-vector-append (make-vector (read ip) (begin (set! c (+ c 1)) c)) (vector 'x 2 3 4))]
           [c0 c]
           [v1 (immutable-vector-append (make-vector (read ip) (begin (set! c (+ c 1)) c)) (vector 'y 'not))])
      (and (equal? v0 '#(1 1 1 1 x 2 3 4))
           (immutable-vector? v0)
           (= c0 1)
           (equal? v1 '#(y not))
           (immutable-vector? v1)
           (= c 2)))
    (error? (immutable-vector-append 1))
    (error? (immutable-vector-append '#(a b c) 'x))
    (error? (immutable-vector-append '#(a b c) '#(d) 'x))
    (error? (immutable-vector-append '#(a b c) '#(d) '#(e)'x))
    (error? (immutable-vector-append (vector 'p) (make-vector -1 'ok)))

    ;; same as mutable-vector test above
    (with-interrupts-disabled
      (letrec ([f (lambda (m)
                    (collect 0 1)
                    (number->string m))]
               [N 1000])
        (let ([v (immutable-vector-append (make-vector N)
                                          (vector (f 8)))])
          (and (eqv? 0 (#%$generation (vector-ref v N)))
               (eqv? 0 (#%$generation v))
               (begin
                 (collect 0 1)
                 (and (eqv? 1 (#%$generation (vector-ref v N)))
                      (equal? (vector-ref v N) "8")))))))
    )

(mat vector-cbuild-cp0 (parameters [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
     (equivalent-expansion?
      (expand/optimize '(vector->immutable-vector (vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize '(vector->immutable-vector (immutable-vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (not
      (equivalent-expansion?
       (expand/optimize `(vector->immutable-vector '#(1 2)))
       `(quote ,(immutable-vector 1 2))))
     (equivalent-expansion?
      (expand/optimize `(vector->immutable-vector '#()))
      `(quote ,(immutable-vector)))
     (equivalent-expansion?
      (expand/optimize `(vector->immutable-vector ',(immutable-vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector->immutable-vector (vector x y))))
      `(lambda (x y) (#3%immutable-vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector->immutable-vector (immutable-vector x y))))
      `(lambda (x y) (#3%immutable-vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector->immutable-vector (begin (x) (vector x y)))))
      `(lambda (x y) (x) (#3%immutable-vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector->immutable-vector (begin (x) (immutable-vector x y)))))
      `(lambda (x y) (x) (#3%immutable-vector x y)))

     (equivalent-expansion?
      (expand/optimize '(immutable-vector-copy (vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize '(immutable-vector-copy (immutable-vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize `(immutable-vector-copy ',(immutable-vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (immutable-vector-copy (vector x y))))
      `(lambda (x y) (#3%immutable-vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (immutable-vector-copy (immutable-vector x y))))
      `(lambda (x y) (#3%immutable-vector x y)))

     (equivalent-expansion?
      (expand/optimize '(vector-copy (vector 1 2)))
      `(#3%vector 1 2))
     (equivalent-expansion?
      (expand/optimize '(vector-copy (immutable-vector 1 2)))
      `(#3%vector 1 2))
     (equivalent-expansion?
      (expand/optimize `(vector-copy ',(immutable-vector 1 2)))
      `(#3%vector 1 2))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector-copy (vector x y))))
      `(lambda (x y) (#3%vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector-copy (immutable-vector x y))))
      `(lambda (x y) (#3%vector x y)))

     (equivalent-expansion?
      (expand/optimize '(immutable-vector-append (vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize '(immutable-vector-append (vector 1 2) (vector 3 4)))
      `(quote ,(immutable-vector 1 2 3 4)))
     (equivalent-expansion?
      (expand/optimize '(immutable-vector-append (immutable-vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize '(immutable-vector-append (immutable-vector 1 2) (vector 3 4)))
      `(quote ,(immutable-vector 1 2 3 4)))
     (equivalent-expansion?
      (expand/optimize `(immutable-vector-append ',(immutable-vector 1 2)))
      `(quote ,(immutable-vector 1 2)))
     (equivalent-expansion?
      (expand/optimize `(immutable-vector-append ',(immutable-vector 1 2) (vector 3 4)))
      `(quote ,(immutable-vector 1 2 3 4)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (immutable-vector-append (vector x y))))
      `(lambda (x y) (#3%immutable-vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (immutable-vector-append (vector x y) (vector y x))))
      `(lambda (x y) (#3%immutable-vector x y y x)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (immutable-vector-append (immutable-vector x y))))
      `(lambda (x y) (#3%immutable-vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (immutable-vector-append (immutable-vector x y) (immutable-vector y x))))
      `(lambda (x y) (#3%immutable-vector x y y x)))
     (equivalent-expansion?
      (expand/optimize `(lambda (x y) (immutable-vector-append (immutable-vector 1 x)
                                                               (vector 2 y)
                                                               (vector-copy (vector 6 7 x))
                                                               ',(immutable-vector 8))))
      `(lambda (x y) (#3%immutable-vector 1 x 2 y 6 7 x 8)))

     (equivalent-expansion?
      (expand/optimize '(vector-append (vector 1 2)))
      `(#3%vector 1 2))
     (equivalent-expansion?
      (expand/optimize '(vector-append (vector 1 2) (vector 3 4)))
      `(#3%vector 1 2 3 4))
     (equivalent-expansion?
      (expand/optimize '(vector-append (immutable-vector 1 2)))
      `(#3%vector 1 2))
     (equivalent-expansion?
      (expand/optimize '(vector-append (immutable-vector 1 2) (vector 3 4)))
      `(#3%vector 1 2 3 4))
     (equivalent-expansion?
      (expand/optimize `(vector-append ',(immutable-vector 1 2)))
      `(#3%vector 1 2))
     (equivalent-expansion?
      (expand/optimize `(vector-append ',(immutable-vector 1 2) (vector 3 4)))
      `(#3%vector 1 2 3 4))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector-append (vector x y))))
      `(lambda (x y) (#3%vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector-append (vector x y) (vector y x))))
      `(lambda (x y) (#3%vector x y y x)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector-append (immutable-vector x y))))
      `(lambda (x y) (#3%vector x y)))
     (equivalent-expansion?
      (expand/optimize '(lambda (x y) (vector-append (immutable-vector x y) (immutable-vector y x))))
      `(lambda (x y) (#3%vector x y y x)))
     (equivalent-expansion?
      (expand/optimize `(lambda (x y) (vector-append (immutable-vector 1 x)
                                                     (vector 2 y)
                                                     (vector-copy (vector 6 7 x))
                                                     ',(immutable-vector 8))))
      `(lambda (x y) (#3%vector 1 x 2 y 6 7 x 8)))

     
     (equivalent-expansion?
      (expand/optimize `(lambda (x y)
                          (vector-ref (vector-copy (immutable-vector 1 x)) 1)))
      `(lambda (x y) x))
     (equivalent-expansion?
      (expand/optimize `(lambda (x y)
                          (vector->immutable-vector
                           (vector (vector-ref (vector-copy (immutable-vector 1 x)) 1)
                                   (vector-ref (immutable-vector-append (immutable-vector 2 3)
                                                                        (vector y 0))
                                               2)))))
      `(lambda (x y) (#3%immutable-vector x y)))

     (equivalent-expansion?
      (expand/optimize `(lambda (x)
                          (vector-length (immutable-vector x x x))))
      `(lambda (x) 3))
     (equivalent-expansion?
      (expand/optimize `(lambda (x)
                          (vector-length (immutable-vector x (#2%printf "hi\n") x))))
      `(lambda (x)
         (#2%printf "hi\n")
         3))

     )

(mat vector-fill!
    (let ([v (vector-copy '#5(a b c d e))])
       (and (equal? v '#5(a b c d e))
            (begin
               (vector-fill! v 9)
               (equal? v '#5(9)))))
    (let ([v (vector-copy '#5(a b c d e))])
       (and (equal? v '#5(a b c d e))
            (begin
               (vector-fill! v (cons 'a 'b))
               (equal? v '#5((a . b))))))
    (error? (let ([v (fxvector)]) (vector-fill! v 3)))
    (let ([v (make-vector 1000)])
      (collect 0 1)
      (let ([x (cons 'a 'b)])
        (vector-fill! v x)
        (collect 0 0)
        (andmap (lambda (y) (eq? y x)) (vector->list v))))
 )

(mat list->vector
    (equal? (list->vector '(a b c)) '#(a b c))
    (equal? (list->vector '()) '#())
    (error? (list->vector '#(a b c)))
    (error? (list->vector '(#\a #\b . #\c)))
    (error? (list->vector (let ([ls (list #\a #\b #\c)]) (set-cdr! (cddr ls) (cdr ls)) ls)))
 )

(mat vector->list
    (equal? (vector->list '#(a b c)) '(a b c))
    (equal? (vector->list '#()) '())
    (error? (vector->list '(a b c)))
 )

(mat fxvector
    (equal? (fxvector 1 2 3 4) '#vfx(1 2 3 4))
    (eq? (fxvector) '#vfx())
    (fxvector? (fxvector (most-positive-fixnum)))
    (fxvector? (fxvector (most-negative-fixnum)))
    (error? (fxvector (+ (most-positive-fixnum) 1)))
    (error? (fxvector (- (most-negative-fixnum) 1)))
    (error? (fxvector 1 2 'a 4))
 )

(mat make-fxvector
    (eqv? (fxvector-length (make-fxvector 10)) 10)
    (eqv? (fxvector-length (make-fxvector 100)) 100)
    (eqv? (fxvector-length (make-fxvector (+ 100 17))) 117)
    (eq? (make-fxvector 0) '#vfx())
    (let ([x (make-fxvector 10)])
      (and (= (fxvector-length x) 10)
           (andmap fixnum? (fxvector->list x))))
    (error? (make-fxvector 3 'a))
    (error? (make-fxvector 10 (+ (most-positive-fixnum) 1)))
    (error? (make-fxvector 10 (- (most-negative-fixnum) 1)))
    (equal? (make-fxvector 10 7) (fxvector 7 7 7 7 7 7 7 7 7 7))
    (equal? (make-fxvector (- 4 2) (+ 1 1)) (fxvector 2 2))
    (eqv? (make-fxvector (- 4 4) (+ 1 1)) (fxvector))
 )

(mat fxvector-syntax
  (eq? '#vfx() '#vfx())
  (eq? '#0vfx() #vfx())
  (equal? 
    '(#vfx(1 2 3) #3vfx(1 2 3) #6vfx(1 2 3))
    (list (fxvector 1 2 3) (fxvector 1 2 3) (fxvector 1 2 3 3 3 3)))
  (let ([x #10vfx()])
    (and (= (fxvector-length x) 10)
         (andmap fixnum? (fxvector->list x))))
 ; the following is invalid because the reader doesn't allow graph marks
 ; and references within an fxvector
 ; (equal? '(#0=#vfx(#1=33 #2# #1# #2=44 #3#) #2# #3=55)
 ;         '(#vfx(33 44 33 44 55) 44 55))
)

(mat fxvector-length
    (eqv? (fxvector-length '#vfx(3 4 5)) 3)
    (eqv? (fxvector-length '#100vfx(5 4 3)) 100)
    (eqv? (fxvector-length '#vfx()) 0)
    (error? (fxvector-length '(a b c)))
 )

(mat $fxvector-ref-check?
  (let ([fv (make-fxvector 3)] [not-fv (make-vector 3)])
    (let ([i-1 -1] [i0 0] [i1 1] [i2 2] [i3 3] [ifalse #f] [ibig (+ (most-positive-fixnum) 1)])
      (and
        (not (#%$fxvector-ref-check? not-fv i0))
        (not (#%$fxvector-ref-check? fv ifalse))
        (not (#%$fxvector-ref-check? fv i-1))
        (#%$fxvector-ref-check? fv 0)
        (#%$fxvector-ref-check? fv 1)
        (#%$fxvector-ref-check? fv 2)
        (#%$fxvector-ref-check? fv i0)
        (#%$fxvector-ref-check? fv i1)
        (#%$fxvector-ref-check? fv i2)
        (not (#%$fxvector-ref-check? fv 3))
        (not (#%$fxvector-ref-check? fv i3))
        (not (#%$fxvector-ref-check? fv ibig)))))
 )

(mat fxvector-ref
    (eqv? (fxvector-ref '#vfx(3 4 5) 0) '3)
    (eqv? (fxvector-ref '#vfx(3 4 5) 1) '4)
    (eqv? (fxvector-ref '#vfx(3 4 5) 2) '5)
    (eqv? (fxvector-ref (fxvector (most-positive-fixnum)) 0) (most-positive-fixnum))
    (eqv? (fxvector-ref (fxvector (most-negative-fixnum)) 0) (most-negative-fixnum))
    (error? (fxvector-ref '#vfx(3 4 5) 3))
    (error? (fxvector-ref '#vfx(3 4 5) -1))
    (error? (fxvector-ref '#vfx(3 4 5) 'a))
    (error? (fxvector-ref '#(3 4 5) 2))
    (error? (fxvector-ref '(3 4 5) 2))
 )

(mat fxvector-set!
    (let ((v (fxvector '3 '4 '5)))
        (and
            (begin (fxvector-set! v 0 '33) (equal? v '#vfx(33 4 5)))
            (begin (fxvector-set! v 1 '44) (equal? v '#vfx(33 44 5)))
            (begin (fxvector-set! v 2 '55) (equal? v '#vfx(33 44 55)))))
    (error? (fxvector-set! (fxvector '3 '4 '5) 3 'd))
    (error? (fxvector-set! (fxvector '3 '4 '5) -1 'd))
    (error? (fxvector-set! (fxvector '3 '4 '5) 'a 'd))
    (error? (fxvector-set! (fxvector '3 '4 '5) 2 'd))
    (error? (fxvector-set! (list '3 '4 '5) 2 'd))
    (error? (fxvector-set! (fxvector 3 4 5) 1 (- (most-negative-fixnum) 1)))
    (error? (fxvector-set! (fxvector 3 4 5) 0 (+ (most-positive-fixnum) 1)))
    (begin
      (define test-fxvector-set!
        (lambda (v i x)
          (fxvector-set! v i x)))
      #t)
    (equal?
      (let ([v (fxvector 3 4 5)])
        (test-fxvector-set! v 0 -3)
        (test-fxvector-set! v 1 -4)
        (test-fxvector-set! v 2 17)
        v)
      #vfx(-3 -4 17))
    (error? (test-fxvector-set! (list 3 4 5) 0 9))
    (error? (test-fxvector-set! (vector 3 4) 0 9))
    (error? (test-fxvector-set! (fxvector 3 4 5) 3 9))
    (error? (test-fxvector-set! (fxvector 3 4 5) -3 9))
    (error? (test-fxvector-set! (fxvector 3 4 5) (+ (most-positive-fixnum) 1) 9))
    (error? (test-fxvector-set! (fxvector 3 4 5) (- (most-negative-fixnum) 1) 9))
    (error? (test-fxvector-set! (fxvector 3 4 5) 'a 9))
    (error? (test-fxvector-set! (fxvector 3 4 5) 2 (+ (most-positive-fixnum) 1)))
    (error? (test-fxvector-set! (fxvector 3 4 5) 2 (- (most-negative-fixnum) 1)))
    (error? (test-fxvector-set! (fxvector 3 4 5) 2 'a))
 )

(mat fxvector-copy
    (equal? (fxvector-copy '#vfx()) '#vfx())
    (equal? (fxvector-copy '#vfx(3 4 5)) '#vfx(3 4 5))
    (let* ((x1 (fxvector 1 2 3)) (x2 (fxvector-copy x1)))
        (and (equal? x2 x1) (not (eq? x2 x1))))
    (andmap
      (lambda (n)
        (let ([v (list->fxvector (map random (make-list n 1000)))])
          (equal? (fxvector-copy v) v)))
      (map random (make-list 500 2500)))
    (error? (fxvector-copy '(a b c)))
 )

(mat fxvector-fill!
    (let ([v (fxvector-copy '#5vfx(1 2 3 4 5))])
       (and (equal? v '#5vfx(1 2 3 4 5))
            (begin
               (fxvector-fill! v 9)
               (equal? v '#5vfx(9)))))
    (let ([v (fxvector-copy '#5vfx(1 2 3 4 5))])
       (and (equal? v '#5vfx(1 2 3 4 5))
            (begin
               (fxvector-fill! v -17)
               (equal? v '#5vfx(-17)))))
    (error? (let ([v (fxvector 1)]) (fxvector-fill! v 'a)))
    (error? (let ([v (vector 1)]) (fxvector-fill! v 3)))
 )

(mat list->fxvector
    (equal? (list->fxvector '(1 2 3)) '#vfx(1 2 3))
    (equal? (list->fxvector '()) '#vfx())
    (error? (list->fxvector '#(a b c)))
    (error? (list->fxvector '(1 2 . 3)))
    (error? (list->fxvector (let ([ls (list 1 2 3)]) (set-cdr! (cddr ls) (cdr ls)) ls)))
 )

(mat fxvector->list
    (equal? (fxvector->list '#vfx(1 2 3)) '(1 2 3))
    (equal? (fxvector->list '#vfx()) '())
    (error? (fxvector->list '(a b c)))
 )

(mat flvector
    (equal? (flvector 1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0))
    (eq? (flvector) '#vfl())
    (flvector? (flvector 1.0))
    (flvector? (flvector -1.0))
    (error? (flvector 1))
    (error? (flvector 'a))
    (error? (flvector 1.0 2.0 'a 4.0))
 )

(mat make-flvector
    (eqv? (flvector-length (make-flvector 10)) 10)
    (eqv? (flvector-length (make-flvector 100)) 100)
    (eqv? (flvector-length (make-flvector (+ 100 17))) 117)
    (eq? (make-flvector 0) '#vfl())
    (let ([x (make-flvector 10)])
      (and (= (flvector-length x) 10)
           (andmap flonum? (flvector->list x))))
    (error? (make-flvector 3 'a))
    (error? (make-flvector 10 1))
    (error? (make-flvector 10 'a))
    (equal? (make-flvector 10 7.0) (flvector 7.0 7.0 7.0 7.0 7.0 7.0 7.0 7.0 7.0 7.0))
    (equal? (make-flvector (- 4 2) (+ 1.0 1.0)) (flvector 2.0 2.0))
    (eqv? (make-flvector (- 4 4) (+ 1.0 1.0)) (flvector))
 )

(mat flvector-syntax
  (eq? '#vfl() '#vfl())
  (eq? '#0vfl() #vfl())
  (equal? 
    '(#vfl(1.0 2.0 3.0) #3vfl(1.0 2.0 3.0) #6vfl(1.0 2.0 3.0))
    (list (flvector 1.0 2.0 3.0) (flvector 1.0 2.0 3.0) (flvector 1.0 2.0 3.0 3.0 3.0 3.0)))
  (let ([x #10vfl()])
    (and (= (flvector-length x) 10)
         (andmap flonum? (flvector->list x))))
 ; the following is invalid because the reader doesn't allow graph marks
 ; and references within an flvector
 ; (equal? '(#0=#vfl(#1=33 #2# #1# #2=44 #3#) #2# #3=55)
 ;         '(#vfl(33 44 33 44 55) 44 55))
)

(mat flvector-length
    (eqv? (flvector-length '#vfl(3.0 4.0 5.0)) 3)
    (eqv? (flvector-length '#100vfl(5.0 4.0 3.0)) 100)
    (eqv? (flvector-length '#vfl()) 0)
    (error? (flvector-length '(a b c)))
 )

(mat $flvector-ref-check?
  (let ([fv (make-flvector 3)] [not-fv (make-vector 3)])
    (let ([i-1 -1] [i0 0] [i1 1] [i2 2] [i3 3] [ifalse #f] [ibig (+ (most-positive-fixnum) 1)])
      (and
        (not (#%$flvector-ref-check? not-fv i0))
        (not (#%$flvector-ref-check? fv ifalse))
        (not (#%$flvector-ref-check? fv i-1))
        (#%$flvector-ref-check? fv 0)
        (#%$flvector-ref-check? fv 1)
        (#%$flvector-ref-check? fv 2)
        (#%$flvector-ref-check? fv i0)
        (#%$flvector-ref-check? fv i1)
        (#%$flvector-ref-check? fv i2)
        (not (#%$flvector-ref-check? fv 3))
        (not (#%$flvector-ref-check? fv i3))
        (not (#%$flvector-ref-check? fv ibig)))))
 )

(mat flvector-ref
    (eqv? (flvector-ref '#vfl(3.0 4.0 5.0) 0) '3.0)
    (eqv? (flvector-ref '#vfl(3.0 4.0 5.0) 1) '4.0)
    (eqv? (flvector-ref '#vfl(3.0 4.0 5.0) 2) '5.0)
    (error? (flvector-ref '#vfl(3.0 4.0 5.0) 3))
    (error? (flvector-ref '#vfl(3.0 4.0 5.0) -1))
    (error? (flvector-ref '#vfl(3.0 4.0 5.0) 'a))
    (error? (flvector-ref '#(3.0 4.0 5.0) 2))
    (error? (flvector-ref '(3.0 4.0 5.0) 2))
 )

(mat flvector-set!
    (let ((v (flvector '3.0 '4.0 '5.0)))
        (and
            (begin (flvector-set! v 0 '33.0) (equal? v '#vfl(33.0 4.0 5.0)))
            (begin (flvector-set! v 1 '44.0) (equal? v '#vfl(33.0 44.0 5.0)))
            (begin (flvector-set! v 2 '55.0) (equal? v '#vfl(33.0 44.0 55.0)))))
    (error? (flvector-set! (flvector '3.0 '4.0 '5.0) 3 'd))
    (error? (flvector-set! (flvector '3.0 '4.0 '5.0) -1 'd))
    (error? (flvector-set! (flvector '3.0 '4.0 '5.0) 'a 'd))
    (error? (flvector-set! (flvector '3.0 '4.0 '5.0) 2 'd))
    (error? (flvector-set! (list '3.0 '4.0 '5.0) 2 'd))
    (error? (flvector-set! (flvector 3.0 4.0 5.0) 1 1))
    (error? (flvector-set! (flvector 3.0 4.0 5.0) 0 'a))
    (begin
      (define test-flvector-set!
        (lambda (v i x)
          (flvector-set! v i x)))
      #t)
    (equal?
      (let ([v (flvector 3.0 4.0 5.0)])
        (test-flvector-set! v 0 -3.0)
        (test-flvector-set! v 1 -4.0)
        (test-flvector-set! v 2 17.0)
        v)
      #vfl(-3.0 -4.0 17.0))
    (error? (test-flvector-set! (list 3.0 4.0 5.0) 0 9.0))
    (error? (test-flvector-set! (vector 3.0 4.0) 0 9.0))
    (error? (test-flvector-set! (flvector 3.0 4.0 5.0) 3 9.0))
    (error? (test-flvector-set! (flvector 3.0 4.0 5.0) -3 9.0))
    (error? (test-flvector-set! (flvector 3.0 4.0 5.0) (+ (most-positive-fixnum) 1) 9.0))
    (error? (test-flvector-set! (flvector 3.0 4.0 5.0) (- (most-negative-fixnum) 1) 9.0))
    (error? (test-flvector-set! (flvector 3.0 4.0 5.0) 'a 9.0))
    (error? (test-flvector-set! (flvector 3.0 4.0 5.0) 2 1))
    (error? (test-flvector-set! (flvector 3.0 4.0 5.0) 2 'a))
 )

(mat flvector-copy
    (equal? (flvector-copy '#vfl()) '#vfl())
    (equal? (flvector-copy '#vfl(3.0 4.0 5.0)) '#vfl(3.0 4.0 5.0))
    (let* ((x1 (flvector 1.0 2.0 3.0)) (x2 (flvector-copy x1)))
        (and (equal? x2 x1) (not (eq? x2 x1))))
    (andmap
      (lambda (n)
        (let ([v (list->flvector (map (lambda (x) (random (inexact x))) (make-list n 1000)))])
          (equal? (flvector-copy v) v)))
      (map random (make-list 500 2500)))
    (error? (flvector-copy '(a b c)))
 )

(mat flvector-fill!
    (let ([v (flvector-copy '#5vfl(1.0 2.0 3.0 4.0 5.0))])
       (and (equal? v '#5vfl(1.0 2.0 3.0 4.0 5.0))
            (begin
               (flvector-fill! v 9.0)
               (equal? v '#5vfl(9.0)))))
    (let ([v (flvector-copy '#5vfl(1.0 2.0 3.0 4.0 5.0))])
       (and (equal? v '#5vfl(1.0 2.0 3.0 4.0 5.0))
            (begin
               (flvector-fill! v -17.0)
               (equal? v '#5vfl(-17.0)))))
    (error? (let ([v (flvector 1.0)]) (flvector-fill! v 'a)))
    (error? (let ([v (vector 1.0)]) (flvector-fill! v 3.0)))
 )

(mat list->flvector
    (equal? (list->flvector '(1.0 2.0 3.0)) '#vfl(1.0 2.0 3.0))
    (equal? (list->flvector '()) '#vfl())
    (error? (list->flvector '#(a b c)))
    (error? (list->flvector '(1.0 2.0 . 3.0)))
    (error? (list->flvector (let ([ls (list 1.0 2.0 3.0)]) (set-cdr! (cddr ls) (cdr ls)) ls)))
 )

(mat flvector->list
    (equal? (flvector->list '#vfl(1.0 2.0 3.0)) '(1.0 2.0 3.0))
    (equal? (flvector->list '#vfl()) '())
    (error? (flvector->list '(a b c)))
 )

(mat vector-map
  (error? ; invalid number of arguments
    (vector-map))
  (error? ; invalid number of arguments
    (vector-map '#()))
  (error? ; invalid number of arguments
    (vector-map +))
  (error? ; non procedure '#()
    (vector-map '#() '#()))
  (error? ; non procedure '#()
    (vector-map '#() '#() '#()))
  (error? ; non procedure '#()
    (vector-map '#() '#() '#() '()))
  (error? ; non procedure '#()
    (vector-map '#() '#() '#() '#() '#()))
  (error? ; non vector 3
    (vector-map + 3))
  (error? ; non vector (3)
    (vector-map + '#() '(3)))
  (error? ; non vector (3)
    (vector-map + '#() '#() '(3)))
  (error? ; non vector (3)
    (vector-map + '#() '#() '(3) '#()))
  (error? ; non vector 7
    (vector-map + 7 '#() '#() '#() '#()))
  (error? ; lengths differ
    (vector-map + '#() '#(x)))
  (error? ; lengths differ
    (vector-map + '#() '#() '#(x)))
  (error? ; lengths differ
    (vector-map + '#() '#() '#(x) '#()))
  (error? ; lengths differ
    (vector-map + '#(y) '#() '#(x) '#()))
  (error? ; lengths differ
    (vector-map + '#(y) '#() '#() '#() '#()))

  (equal? (vector-map + '#()) '#())
  (equal? (vector-map + '#(1)) '#(1))
  (equal? (vector-map + '#(1 2)) '#(1 2))
  (equal? (vector-map + '#(1 2 3)) '#(1 2 3))
  (equal? (vector-map + '#(1 2 3 4)) '#(1 2 3 4))
  (equal? (vector-map + (make-vector 100 7)) '#100(7))

  (equal? (vector-map list '#() '#()) '#())
  (equal? (vector-map list '#(1) '#(5)) '#((1 5)))
  (equal? (vector-map list '#(1 2) '#(5 7)) '#((1 5) (2 7)))
  (equal? (vector-map list '#(1 2 3) '#(a b c)) '#((1 a) (2 b) (3 c)))
  (equal? (vector-map list '#(1 2 3 4) '#(a b c d)) '#((1 a) (2 b) (3 c) (4 d)))

  (equal? (vector-map list '#() '#() '#()) '#())
  (equal? (vector-map list '#(1) '#(5) '#(a)) '#((1 5 a)))
  (equal? (vector-map list '#(1 2) '#(5 7) '#(a b)) '#((1 5 a) (2 7 b)))
  (equal?
    (vector-map list '#(1 2 3) '#(5 7 9) '#(a b c))
    '#((1 5 a) (2 7 b) (3 9 c)))
  (equal?
    (vector-map list '#(1 2 3 4) '#(5 7 9 11) '#(a b c d))
    '#((1 5 a) (2 7 b) (3 9 c) (4 11 d)))

  (equal? (vector-map list '#() '#() '#() '#()) '#())
  (equal? (vector-map list '#(#\a) '#(1) '#(5) '#(a)) '#((#\a 1 5 a)))
  (equal?
    (vector-map list '#(#\a #\b) '#(1 2) '#(5 7) '#(a b))
    '#((#\a 1 5 a) (#\b 2 7 b)))
  (equal?
    (vector-map list '#(#\a #\b #\c) '#(1 2 3) '#(5 7 9) '#(a b c))
    '#((#\a 1 5 a) (#\b 2 7 b) (#\c 3 9 c)))
  (equal?
    (vector-map list '#(#\a #\b #\c #\d) '#(1 2 3 4) '#(5 7 9 11) '#(a b c d))
    '#((#\a 1 5 a) (#\b 2 7 b) (#\c 3 9 c) (#\d 4 11 d)))
  (let ([orig-v #f] [orig-elts #f] [next #f])
    (let ([n 100])
      (let ([v (vector-map
                 (lambda (x) (cons (call/cc values) x))
                 (list->vector (iota n)))])
        (if orig-v
            (unless (andmap eq? (vector->list orig-v) orig-elts)
              (errorf #f "original vector-map elts mutated"))
            (begin
              (set! orig-v v)
              (set! orig-elts (vector->list v))
              (set! next 0)))
        (let ([m next])
          (unless (= m n)
            (set! next (fx+ next 1)) 
            (let ([p (vector-ref orig-v m)])
              (unless (eqv? (cdr p) m)
                (errorf #f "unexpected cdr value (~s instead of ~s)" (cdr p) m))
              ((car p) n)))))
      (eqv? next n)))
   (begin
     (define ($vector-map-f1 p x1 x2 x3 x4 x5)
       (vector
         (vector-map p '#())
         (vector-map p '#() x1)
         (vector-map p '#() x1 x2)
         (vector-map p '#() x1 x2 x3)
         (vector-map p '#() x1 x2 x3 x4)
         (vector-map p '#() x1 x2 x3 x4 x5)
         (vector-map p x1 '#())
         (vector-map p x1 '#() x2)
         (vector-map p x1 '#() x2 x3)
         (vector-map p x1 '#() x2 x3 x4)
         (vector-map p x1 '#() x2 x3 x4 x5)
         (vector-map p x1 x2 '#())
         (vector-map p x1 x2 '#() x3)
         (vector-map p x1 x2 '#() x3 x4)
         (vector-map p x1 x2 '#() x3 x4 x5)
         (vector-map p x1 x2 x3 '#())
         (vector-map p x1 x2 x3 '#() x4)
         (vector-map p x1 x2 x3 '#() x4 x5)
         (vector-map p x1 x2 x3 x4 '#())
         (vector-map p x1 x2 x3 x4 '#() x5)
         (vector-map p x1 x2 x3 x4 x5 '#())))
     (procedure? $vector-map-f1))
   (equal?
     ($vector-map-f1 vector '#() '#() '#() '#() '#())
     '#(#() #() #() #() #() #() #() #() #() #() #() #() #() #() #() #() #() #()
       #() #() #()))
   (begin
     (define ($vector-map-f1 p x1 x2 x3 x4 x5)
       (vector
         (vector-map p '#(a))
         (vector-map p '#(a) x1)
         (vector-map p '#(a) x1 x2)
         (vector-map p '#(a) x1 x2 x3)
         (vector-map p '#(a) x1 x2 x3 x4)
         (vector-map p '#(a) x1 x2 x3 x4 x5)
         (vector-map p x1 '#(a))
         (vector-map p x1 '#(a) x2)
         (vector-map p x1 '#(a) x2 x3)
         (vector-map p x1 '#(a) x2 x3 x4)
         (vector-map p x1 '#(a) x2 x3 x4 x5)
         (vector-map p x1 x2 '#(a))
         (vector-map p x1 x2 '#(a) x3)
         (vector-map p x1 x2 '#(a) x3 x4)
         (vector-map p x1 x2 '#(a) x3 x4 x5)
         (vector-map p x1 x2 x3 '#(a))
         (vector-map p x1 x2 x3 '#(a) x4)
         (vector-map p x1 x2 x3 '#(a) x4 x5)
         (vector-map p x1 x2 x3 x4 '#(a))
         (vector-map p x1 x2 x3 x4 '#(a) x5)
         (vector-map p x1 x2 x3 x4 x5 '#(a))))
     (procedure? $vector-map-f1))
   (equal?
     ($vector-map-f1 vector '#(1) '#(4) '#(d) '#(g) '#(7))
     '#(#(#(a))
       #(#(a 1))
       #(#(a 1 4))
       #(#(a 1 4 d))
       #(#(a 1 4 d g))
       #(#(a 1 4 d g 7))
       #(#(1 a))
       #(#(1 a 4))
       #(#(1 a 4 d))
       #(#(1 a 4 d g))
       #(#(1 a 4 d g 7))
       #(#(1 4 a))
       #(#(1 4 a d))
       #(#(1 4 a d g))
       #(#(1 4 a d g 7))
       #(#(1 4 d a))
       #(#(1 4 d a g))
       #(#(1 4 d a g 7))
       #(#(1 4 d g a))
       #(#(1 4 d g a 7))
       #(#(1 4 d g 7 a))))
   (begin
     (define ($vector-map-f1 p x1 x2 x3 x4 x5)
       (vector
         (vector-map p '#(a b))
         (vector-map p '#(a b) x1)
         (vector-map p '#(a b) x1 x2)
         (vector-map p '#(a b) x1 x2 x3)
         (vector-map p '#(a b) x1 x2 x3 x4)
         (vector-map p '#(a b) x1 x2 x3 x4 x5)
         (vector-map p x1 '#(a b))
         (vector-map p x1 '#(a b) x2)
         (vector-map p x1 '#(a b) x2 x3)
         (vector-map p x1 '#(a b) x2 x3 x4)
         (vector-map p x1 '#(a b) x2 x3 x4 x5)
         (vector-map p x1 x2 '#(a b))
         (vector-map p x1 x2 '#(a b) x3)
         (vector-map p x1 x2 '#(a b) x3 x4)
         (vector-map p x1 x2 '#(a b) x3 x4 x5)
         (vector-map p x1 x2 x3 '#(a b))
         (vector-map p x1 x2 x3 '#(a b) x4)
         (vector-map p x1 x2 x3 '#(a b) x4 x5)
         (vector-map p x1 x2 x3 x4 '#(a b))
         (vector-map p x1 x2 x3 x4 '#(a b) x5)
         (vector-map p x1 x2 x3 x4 x5 '#(a b))))
     (procedure? $vector-map-f1))
   (equal?
     ($vector-map-f1 vector '#(1 2) '#(4 5) '#(d e) '#(g h) '#(7 j))
     '#(#(#(a) #(b))
       #(#(a 1) #(b 2))
       #(#(a 1 4) #(b 2 5))
       #(#(a 1 4 d) #(b 2 5 e))
       #(#(a 1 4 d g) #(b 2 5 e h))
       #(#(a 1 4 d g 7) #(b 2 5 e h j))
       #(#(1 a) #(2 b))
       #(#(1 a 4) #(2 b 5))
       #(#(1 a 4 d) #(2 b 5 e))
       #(#(1 a 4 d g) #(2 b 5 e h))
       #(#(1 a 4 d g 7) #(2 b 5 e h j))
       #(#(1 4 a) #(2 5 b))
       #(#(1 4 a d) #(2 5 b e))
       #(#(1 4 a d g) #(2 5 b e h))
       #(#(1 4 a d g 7) #(2 5 b e h j))
       #(#(1 4 d a) #(2 5 e b))
       #(#(1 4 d a g) #(2 5 e b h))
       #(#(1 4 d a g 7) #(2 5 e b h j))
       #(#(1 4 d g a) #(2 5 e h b))
       #(#(1 4 d g a 7) #(2 5 e h b j))
       #(#(1 4 d g 7 a) #(2 5 e h j b))))
   (begin
     (define ($vector-map-f1 p x1 x2 x3 x4 x5)
       (vector
         (vector-map p '#(a b c))
         (vector-map p '#(a b c) x1)
         (vector-map p '#(a b c) x1 x2)
         (vector-map p '#(a b c) x1 x2 x3)
         (vector-map p '#(a b c) x1 x2 x3 x4)
         (vector-map p '#(a b c) x1 x2 x3 x4 x5)
         (vector-map p x1 '#(a b c))
         (vector-map p x1 '#(a b c) x2)
         (vector-map p x1 '#(a b c) x2 x3)
         (vector-map p x1 '#(a b c) x2 x3 x4)
         (vector-map p x1 '#(a b c) x2 x3 x4 x5)
         (vector-map p x1 x2 '#(a b c))
         (vector-map p x1 x2 '#(a b c) x3)
         (vector-map p x1 x2 '#(a b c) x3 x4)
         (vector-map p x1 x2 '#(a b c) x3 x4 x5)
         (vector-map p x1 x2 x3 '#(a b c))
         (vector-map p x1 x2 x3 '#(a b c) x4)
         (vector-map p x1 x2 x3 '#(a b c) x4 x5)
         (vector-map p x1 x2 x3 x4 '#(a b c))
         (vector-map p x1 x2 x3 x4 '#(a b c) x5)
         (vector-map p x1 x2 x3 x4 x5 '#(a b c))))
     (procedure? $vector-map-f1))
   (equal?
     ($vector-map-f1 vector '#(1 2 3) '#(4 5 6) '#(d e f) '#(g h i) '#(7 j 9))
     '#(#(#(a) #(b) #(c))
       #(#(a 1) #(b 2) #(c 3))
       #(#(a 1 4) #(b 2 5) #(c 3 6))
       #(#(a 1 4 d) #(b 2 5 e) #(c 3 6 f))
       #(#(a 1 4 d g) #(b 2 5 e h) #(c 3 6 f i))
       #(#(a 1 4 d g 7) #(b 2 5 e h j) #(c 3 6 f i 9))
       #(#(1 a) #(2 b) #(3 c))
       #(#(1 a 4) #(2 b 5) #(3 c 6))
       #(#(1 a 4 d) #(2 b 5 e) #(3 c 6 f))
       #(#(1 a 4 d g) #(2 b 5 e h) #(3 c 6 f i))
       #(#(1 a 4 d g 7) #(2 b 5 e h j) #(3 c 6 f i 9))
       #(#(1 4 a) #(2 5 b) #(3 6 c))
       #(#(1 4 a d) #(2 5 b e) #(3 6 c f))
       #(#(1 4 a d g) #(2 5 b e h) #(3 6 c f i))
       #(#(1 4 a d g 7) #(2 5 b e h j) #(3 6 c f i 9))
       #(#(1 4 d a) #(2 5 e b) #(3 6 f c))
       #(#(1 4 d a g) #(2 5 e b h) #(3 6 f c i))
       #(#(1 4 d a g 7) #(2 5 e b h j) #(3 6 f c i 9))
       #(#(1 4 d g a) #(2 5 e h b) #(3 6 f i c))
       #(#(1 4 d g a 7) #(2 5 e h b j) #(3 6 f i c 9))
       #(#(1 4 d g 7 a) #(2 5 e h j b) #(3 6 f i 9 c))))
   (begin
     (define ($vector-map-f1 p x1 x2 x3 x4 x5)
       (vector
         (vector-map p '#(a b c d))
         (vector-map p '#(a b c d) x1)
         (vector-map p '#(a b c d) x1 x2)
         (vector-map p '#(a b c d) x1 x2 x3)
         (vector-map p '#(a b c d) x1 x2 x3 x4)
         (vector-map p '#(a b c d) x1 x2 x3 x4 x5)
         (vector-map p x1 '#(a b c d))
         (vector-map p x1 '#(a b c d) x2)
         (vector-map p x1 '#(a b c d) x2 x3)
         (vector-map p x1 '#(a b c d) x2 x3 x4)
         (vector-map p x1 '#(a b c d) x2 x3 x4 x5)
         (vector-map p x1 x2 '#(a b c d))
         (vector-map p x1 x2 '#(a b c d) x3)
         (vector-map p x1 x2 '#(a b c d) x3 x4)
         (vector-map p x1 x2 '#(a b c d) x3 x4 x5)
         (vector-map p x1 x2 x3 '#(a b c d))
         (vector-map p x1 x2 x3 '#(a b c d) x4)
         (vector-map p x1 x2 x3 '#(a b c d) x4 x5)
         (vector-map p x1 x2 x3 x4 '#(a b c d))
         (vector-map p x1 x2 x3 x4 '#(a b c d) x5)
         (vector-map p x1 x2 x3 x4 x5 '#(a b c d))))
     (procedure? $vector-map-f1))
   (equal?
     ($vector-map-f1 vector '#(1 2 3 4) '#(f g h i) '#(k l m n) '#(p q r s) '#(u v w x))
     '#(#(#(a) #(b) #(c) #(d)) #(#(a 1) #(b 2) #(c 3) #(d 4))
       #(#(a 1 f) #(b 2 g) #(c 3 h) #(d 4 i))
       #(#(a 1 f k) #(b 2 g l) #(c 3 h m) #(d 4 i n))
       #(#(a 1 f k p) #(b 2 g l q) #(c 3 h m r) #(d 4 i n s))
       #(#(a 1 f k p u) #(b 2 g l q v) #(c 3 h m r w) #(d 4 i n s x))
       #(#(1 a) #(2 b) #(3 c) #(4 d))
       #(#(1 a f) #(2 b g) #(3 c h) #(4 d i))
       #(#(1 a f k) #(2 b g l) #(3 c h m) #(4 d i n))
       #(#(1 a f k p) #(2 b g l q) #(3 c h m r) #(4 d i n s))
       #(#(1 a f k p u) #(2 b g l q v) #(3 c h m r w) #(4 d i n s x))
       #(#(1 f a) #(2 g b) #(3 h c) #(4 i d))
       #(#(1 f a k) #(2 g b l) #(3 h c m) #(4 i d n))
       #(#(1 f a k p) #(2 g b l q) #(3 h c m r) #(4 i d n s))
       #(#(1 f a k p u) #(2 g b l q v) #(3 h c m r w) #(4 i d n s x))
       #(#(1 f k a) #(2 g l b) #(3 h m c) #(4 i n d))
       #(#(1 f k a p) #(2 g l b q) #(3 h m c r) #(4 i n d s))
       #(#(1 f k a p u) #(2 g l b q v) #(3 h m c r w) #(4 i n d s x))
       #(#(1 f k p a) #(2 g l q b) #(3 h m r c) #(4 i n s d))
       #(#(1 f k p a u) #(2 g l q b v) #(3 h m r c w) #(4 i n s d x))
       #(#(1 f k p u a) #(2 g l q v b) #(3 h m r w c) #(4 i n s x d))))
   (begin
     (define ($vector-map-f1 p x1 x2 x3 x4 x5)
       (vector
         (vector-map p '#(a b c d e))
         (vector-map p '#(a b c d e) x1)
         (vector-map p '#(a b c d e) x1 x2)
         (vector-map p '#(a b c d e) x1 x2 x3)
         (vector-map p '#(a b c d e) x1 x2 x3 x4)
         (vector-map p '#(a b c d e) x1 x2 x3 x4 x5)
         (vector-map p x1 '#(a b c d e))
         (vector-map p x1 '#(a b c d e) x2)
         (vector-map p x1 '#(a b c d e) x2 x3)
         (vector-map p x1 '#(a b c d e) x2 x3 x4)
         (vector-map p x1 '#(a b c d e) x2 x3 x4 x5)
         (vector-map p x1 x2 '#(a b c d e))
         (vector-map p x1 x2 '#(a b c d e) x3)
         (vector-map p x1 x2 '#(a b c d e) x3 x4)
         (vector-map p x1 x2 '#(a b c d e) x3 x4 x5)
         (vector-map p x1 x2 x3 '#(a b c d e))
         (vector-map p x1 x2 x3 '#(a b c d e) x4)
         (vector-map p x1 x2 x3 '#(a b c d e) x4 x5)
         (vector-map p x1 x2 x3 x4 '#(a b c d e))
         (vector-map p x1 x2 x3 x4 '#(a b c d e) x5)
         (vector-map p x1 x2 x3 x4 x5 '#(a b c d e))))
     (procedure? $vector-map-f1))
   (equal?
     ($vector-map-f1 vector '#(1 2 3 4 5) '#(f g h i j) '#(k l m n o) '#(p q r s t) '#(u v w x y))
     '#(#(#(a) #(b) #(c) #(d) #(e)) #(#(a 1) #(b 2) #(c 3) #(d 4) #(e 5))
       #(#(a 1 f) #(b 2 g) #(c 3 h) #(d 4 i) #(e 5 j))
       #(#(a 1 f k) #(b 2 g l) #(c 3 h m) #(d 4 i n) #(e 5 j o))
       #(#(a 1 f k p) #(b 2 g l q) #(c 3 h m r) #(d 4 i n s) #(e 5 j o t))
       #(#(a 1 f k p u) #(b 2 g l q v) #(c 3 h m r w) #(d 4 i n s x) #(e 5 j o t y))
       #(#(1 a) #(2 b) #(3 c) #(4 d) #(5 e))
       #(#(1 a f) #(2 b g) #(3 c h) #(4 d i) #(5 e j))
       #(#(1 a f k) #(2 b g l) #(3 c h m) #(4 d i n) #(5 e j o))
       #(#(1 a f k p) #(2 b g l q) #(3 c h m r) #(4 d i n s) #(5 e j o t))
       #(#(1 a f k p u) #(2 b g l q v) #(3 c h m r w) #(4 d i n s x) #(5 e j o t y))
       #(#(1 f a) #(2 g b) #(3 h c) #(4 i d) #(5 j e))
       #(#(1 f a k) #(2 g b l) #(3 h c m) #(4 i d n) #(5 j e o))
       #(#(1 f a k p) #(2 g b l q) #(3 h c m r) #(4 i d n s) #(5 j e o t))
       #(#(1 f a k p u) #(2 g b l q v) #(3 h c m r w) #(4 i d n s x) #(5 j e o t y))
       #(#(1 f k a) #(2 g l b) #(3 h m c) #(4 i n d) #(5 j o e))
       #(#(1 f k a p) #(2 g l b q) #(3 h m c r) #(4 i n d s) #(5 j o e t))
       #(#(1 f k a p u) #(2 g l b q v) #(3 h m c r w) #(4 i n d s x) #(5 j o e t y))
       #(#(1 f k p a) #(2 g l q b) #(3 h m r c) #(4 i n s d) #(5 j o t e))
       #(#(1 f k p a u) #(2 g l q b v) #(3 h m r c w) #(4 i n s d x) #(5 j o t e y))
       #(#(1 f k p u a) #(2 g l q v b) #(3 h m r w c) #(4 i n s x d) #(5 j o t y e))))
 )

(mat vector-for-each
  (error? ; invalid number of arguments
    (vector-for-each))
  (error? ; invalid number of arguments
    (vector-for-each '#()))
  (error? ; invalid number of arguments
    (vector-for-each +))
  (error? ; non procedure '#()
    (vector-for-each '#() '#()))
  (error? ; non procedure '#()
    (vector-for-each '#() '#() '#()))
  (error? ; non procedure '#()
    (vector-for-each '#() '#() '#() '()))
  (error? ; non procedure '#()
    (vector-for-each '#() '#() '#() '#() '#()))
  (error? ; non vector 3
    (vector-for-each + 3))
  (error? ; non vector (3)
    (vector-for-each + '#() '(3)))
  (error? ; non vector (3)
    (vector-for-each + '#() '#() '(3)))
  (error? ; non vector (3)
    (vector-for-each + '#() '#() '(3) '#()))
  (error? ; non vector 7
    (vector-for-each + 7 '#() '#() '#() '#()))
  (error? ; lengths differ
    (vector-for-each + '#() '#(x)))
  (error? ; lengths differ
    (vector-for-each + '#() '#() '#(x)))
  (error? ; lengths differ
    (vector-for-each + '#() '#() '#(x) '#()))
  (error? ; lengths differ
    (vector-for-each + '#(y) '#() '#(x) '#()))
  (error? ; lengths differ
    (vector-for-each + '#(y) '#() '#() '#() '#()))
  (equal? (vector-for-each + '#()) (void))
  (equal? (vector-for-each + '#() '#()) (void))
  (equal? (vector-for-each + '#() '#() '#()) (void))
  (equal? (vector-for-each + '#() '#() '#() '#() '#()) (void))
  (equal?
    (let ([ls '()])
      (vector-for-each (lambda (x) (set! ls (cons x ls))) '#(a b c d e f))
      ls)
    '(f e d c b a))
  (equal?
    (let ([ls '()])
      (vector-for-each
        (lambda (x y) (set! ls (cons (cons x y) ls)))
        '#(a b c d e f)
        '#(3 2 7 6 5 4))
      ls)
    '((f . 4) (e . 5) (d . 6) (c . 7) (b . 2) (a . 3)))
  (equal?
    (let ([ls '()])
      (vector-for-each
        (lambda r (set! ls (cons r ls)))
        '#(a b c d e f)
        '#(3 2 7 6 5 4)
        '#(-1 -2 -3 -4 -5 -6))
      ls)
    '((f 4 -6) (e 5 -5) (d 6 -4) (c 7 -3) (b 2 -2) (a 3 -1)))
  (equal?
    (let ([ls '()])
      (vector-for-each
        (lambda r (set! ls (cons r ls)))
        '#(a b c d e f)
        '#(3 2 7 6 5 4)
        '#(-1 -2 -3 -4 -5 -6)
        '#(m n o p q r)
        '#(z y x w v u))
      ls)
    '((f 4 -6 r u) (e 5 -5 q v) (d 6 -4 p w) (c 7 -3 o x)
       (b 2 -2 n y) (a 3 -1 m z)))
  (begin
    (define ($vector-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (vector-for-each p '#())
        (vector-for-each p '#() x1)
        (vector-for-each p '#() x1 x2)
        (vector-for-each p '#() x1 x2 x3)
        (vector-for-each p '#() x1 x2 x3 x4)
        (vector-for-each p '#() x1 x2 x3 x4 x5)
        (vector-for-each p x1 '#())
        (vector-for-each p x1 '#() x2)
        (vector-for-each p x1 '#() x2 x3)
        (vector-for-each p x1 '#() x2 x3 x4)
        (vector-for-each p x1 '#() x2 x3 x4 x5)
        (vector-for-each p x1 x2 '#())
        (vector-for-each p x1 x2 '#() x3)
        (vector-for-each p x1 x2 '#() x3 x4)
        (vector-for-each p x1 x2 '#() x3 x4 x5)
        (vector-for-each p x1 x2 x3 '#())
        (vector-for-each p x1 x2 x3 '#() x4)
        (vector-for-each p x1 x2 x3 '#() x4 x5)
        (vector-for-each p x1 x2 x3 x4 '#())
        (vector-for-each p x1 x2 x3 x4 '#() x5)
        (vector-for-each p x1 x2 x3 x4 x5 '#())))
    (procedure? $vector-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($vector-for-each-f1 q '#() '#() '#() '#() '#())
      (reverse ls))
    '())
  (begin
    (define ($vector-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (vector-for-each p '#(a))
        (vector-for-each p '#(a) x1)
        (vector-for-each p '#(a) x1 x2)
        (vector-for-each p '#(a) x1 x2 x3)
        (vector-for-each p '#(a) x1 x2 x3 x4)
        (vector-for-each p '#(a) x1 x2 x3 x4 x5)
        (vector-for-each p x1 '#(a))
        (vector-for-each p x1 '#(a) x2)
        (vector-for-each p x1 '#(a) x2 x3)
        (vector-for-each p x1 '#(a) x2 x3 x4)
        (vector-for-each p x1 '#(a) x2 x3 x4 x5)
        (vector-for-each p x1 x2 '#(a))
        (vector-for-each p x1 x2 '#(a) x3)
        (vector-for-each p x1 x2 '#(a) x3 x4)
        (vector-for-each p x1 x2 '#(a) x3 x4 x5)
        (vector-for-each p x1 x2 x3 '#(a))
        (vector-for-each p x1 x2 x3 '#(a) x4)
        (vector-for-each p x1 x2 x3 '#(a) x4 x5)
        (vector-for-each p x1 x2 x3 x4 '#(a))
        (vector-for-each p x1 x2 x3 x4 '#(a) x5)
        (vector-for-each p x1 x2 x3 x4 x5 '#(a))))
    (procedure? $vector-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($vector-for-each-f1 q '#(1) '#(f) '#(k) '#(p) '#(u))
      (reverse ls))
    '((a) (1 a) (f 1 a) (k f 1 a) (p k f 1 a) (u p k f 1 a)
      (a 1) (f a 1) (k f a 1) (p k f a 1) (u p k f a 1)
      (a f 1) (k a f 1) (p k a f 1) (u p k a f 1) (a k f 1)
      (p a k f 1) (u p a k f 1) (a p k f 1) (u a p k f 1)
      (a u p k f 1)))
  (begin
    (define ($vector-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (vector-for-each p '#(a b))
        (vector-for-each p '#(a b) x1)
        (vector-for-each p '#(a b) x1 x2)
        (vector-for-each p '#(a b) x1 x2 x3)
        (vector-for-each p '#(a b) x1 x2 x3 x4)
        (vector-for-each p '#(a b) x1 x2 x3 x4 x5)
        (vector-for-each p x1 '#(a b))
        (vector-for-each p x1 '#(a b) x2)
        (vector-for-each p x1 '#(a b) x2 x3)
        (vector-for-each p x1 '#(a b) x2 x3 x4)
        (vector-for-each p x1 '#(a b) x2 x3 x4 x5)
        (vector-for-each p x1 x2 '#(a b))
        (vector-for-each p x1 x2 '#(a b) x3)
        (vector-for-each p x1 x2 '#(a b) x3 x4)
        (vector-for-each p x1 x2 '#(a b) x3 x4 x5)
        (vector-for-each p x1 x2 x3 '#(a b))
        (vector-for-each p x1 x2 x3 '#(a b) x4)
        (vector-for-each p x1 x2 x3 '#(a b) x4 x5)
        (vector-for-each p x1 x2 x3 x4 '#(a b))
        (vector-for-each p x1 x2 x3 x4 '#(a b) x5)
        (vector-for-each p x1 x2 x3 x4 x5 '#(a b))))
    (procedure? $vector-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($vector-for-each-f1 q '#(1 2) '#(f g) '#(k l) '#(p q) '#(u v))
      (reverse ls))
    '((a) (b) (1 a) (2 b) (f 1 a) (g 2 b) (k f 1 a)
      (l g 2 b) (p k f 1 a) (q l g 2 b) (u p k f 1 a)
      (v q l g 2 b) (a 1) (b 2) (f a 1) (g b 2) (k f a 1)
      (l g b 2) (p k f a 1) (q l g b 2) (u p k f a 1)
      (v q l g b 2) (a f 1) (b g 2) (k a f 1) (l b g 2)
      (p k a f 1) (q l b g 2) (u p k a f 1) (v q l b g 2)
      (a k f 1) (b l g 2) (p a k f 1) (q b l g 2)
      (u p a k f 1) (v q b l g 2) (a p k f 1) (b q l g 2)
      (u a p k f 1) (v b q l g 2) (a u p k f 1)
      (b v q l g 2)))
  (begin
    (define ($vector-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (vector-for-each p '#(a b c))
        (vector-for-each p '#(a b c) x1)
        (vector-for-each p '#(a b c) x1 x2)
        (vector-for-each p '#(a b c) x1 x2 x3)
        (vector-for-each p '#(a b c) x1 x2 x3 x4)
        (vector-for-each p '#(a b c) x1 x2 x3 x4 x5)
        (vector-for-each p x1 '#(a b c))
        (vector-for-each p x1 '#(a b c) x2)
        (vector-for-each p x1 '#(a b c) x2 x3)
        (vector-for-each p x1 '#(a b c) x2 x3 x4)
        (vector-for-each p x1 '#(a b c) x2 x3 x4 x5)
        (vector-for-each p x1 x2 '#(a b c))
        (vector-for-each p x1 x2 '#(a b c) x3)
        (vector-for-each p x1 x2 '#(a b c) x3 x4)
        (vector-for-each p x1 x2 '#(a b c) x3 x4 x5)
        (vector-for-each p x1 x2 x3 '#(a b c))
        (vector-for-each p x1 x2 x3 '#(a b c) x4)
        (vector-for-each p x1 x2 x3 '#(a b c) x4 x5)
        (vector-for-each p x1 x2 x3 x4 '#(a b c))
        (vector-for-each p x1 x2 x3 x4 '#(a b c) x5)
        (vector-for-each p x1 x2 x3 x4 x5 '#(a b c))))
    (procedure? $vector-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($vector-for-each-f1 q '#(1 2 3) '#(f g h) '#(k l m) '#(p q r) '#(u v w))
      (reverse ls))
    '((a) (b) (c) (1 a) (2 b) (3 c) (f 1 a) (g 2 b) (h 3 c)
      (k f 1 a) (l g 2 b) (m h 3 c) (p k f 1 a) (q l g 2 b)
      (r m h 3 c) (u p k f 1 a) (v q l g 2 b) (w r m h 3 c)
      (a 1) (b 2) (c 3) (f a 1) (g b 2) (h c 3) (k f a 1)
      (l g b 2) (m h c 3) (p k f a 1) (q l g b 2) (r m h c 3)
      (u p k f a 1) (v q l g b 2) (w r m h c 3) (a f 1)
      (b g 2) (c h 3) (k a f 1) (l b g 2) (m c h 3)
      (p k a f 1) (q l b g 2) (r m c h 3) (u p k a f 1)
      (v q l b g 2) (w r m c h 3) (a k f 1) (b l g 2)
      (c m h 3) (p a k f 1) (q b l g 2) (r c m h 3)
      (u p a k f 1) (v q b l g 2) (w r c m h 3) (a p k f 1)
      (b q l g 2) (c r m h 3) (u a p k f 1) (v b q l g 2)
      (w c r m h 3) (a u p k f 1) (b v q l g 2)
      (c w r m h 3)))
  (begin
    (define ($vector-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (vector-for-each p '#(a b c d))
        (vector-for-each p '#(a b c d) x1)
        (vector-for-each p '#(a b c d) x1 x2)
        (vector-for-each p '#(a b c d) x1 x2 x3)
        (vector-for-each p '#(a b c d) x1 x2 x3 x4)
        (vector-for-each p '#(a b c d) x1 x2 x3 x4 x5)
        (vector-for-each p x1 '#(a b c d))
        (vector-for-each p x1 '#(a b c d) x2)
        (vector-for-each p x1 '#(a b c d) x2 x3)
        (vector-for-each p x1 '#(a b c d) x2 x3 x4)
        (vector-for-each p x1 '#(a b c d) x2 x3 x4 x5)
        (vector-for-each p x1 x2 '#(a b c d))
        (vector-for-each p x1 x2 '#(a b c d) x3)
        (vector-for-each p x1 x2 '#(a b c d) x3 x4)
        (vector-for-each p x1 x2 '#(a b c d) x3 x4 x5)
        (vector-for-each p x1 x2 x3 '#(a b c d))
        (vector-for-each p x1 x2 x3 '#(a b c d) x4)
        (vector-for-each p x1 x2 x3 '#(a b c d) x4 x5)
        (vector-for-each p x1 x2 x3 x4 '#(a b c d))
        (vector-for-each p x1 x2 x3 x4 '#(a b c d) x5)
        (vector-for-each p x1 x2 x3 x4 x5 '#(a b c d))))
    (procedure? $vector-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($vector-for-each-f1 q '#(1 2 3 4) '#(f g h i) '#(k l m n) '#(p q r s) '#(u v w x))
      (reverse ls))
    '((a) (b) (c) (d) (1 a) (2 b) (3 c) (4 d) (f 1 a)
      (g 2 b) (h 3 c) (i 4 d) (k f 1 a) (l g 2 b) (m h 3 c)
      (n i 4 d) (p k f 1 a) (q l g 2 b) (r m h 3 c)
      (s n i 4 d) (u p k f 1 a) (v q l g 2 b) (w r m h 3 c)
      (x s n i 4 d) (a 1) (b 2) (c 3) (d 4) (f a 1) (g b 2)
      (h c 3) (i d 4) (k f a 1) (l g b 2) (m h c 3) (n i d 4)
      (p k f a 1) (q l g b 2) (r m h c 3) (s n i d 4)
      (u p k f a 1) (v q l g b 2) (w r m h c 3) (x s n i d 4)
      (a f 1) (b g 2) (c h 3) (d i 4) (k a f 1) (l b g 2)
      (m c h 3) (n d i 4) (p k a f 1) (q l b g 2) (r m c h 3)
      (s n d i 4) (u p k a f 1) (v q l b g 2) (w r m c h 3)
      (x s n d i 4) (a k f 1) (b l g 2) (c m h 3) (d n i 4)
      (p a k f 1) (q b l g 2) (r c m h 3) (s d n i 4)
      (u p a k f 1) (v q b l g 2) (w r c m h 3) (x s d n i 4)
      (a p k f 1) (b q l g 2) (c r m h 3) (d s n i 4)
      (u a p k f 1) (v b q l g 2) (w c r m h 3) (x d s n i 4)
      (a u p k f 1) (b v q l g 2) (c w r m h 3)
      (d x s n i 4)))
  (begin
    (define ($vector-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (vector-for-each p '#(a b c d e))
        (vector-for-each p '#(a b c d e) x1)
        (vector-for-each p '#(a b c d e) x1 x2)
        (vector-for-each p '#(a b c d e) x1 x2 x3)
        (vector-for-each p '#(a b c d e) x1 x2 x3 x4)
        (vector-for-each p '#(a b c d e) x1 x2 x3 x4 x5)
        (vector-for-each p x1 '#(a b c d e))
        (vector-for-each p x1 '#(a b c d e) x2)
        (vector-for-each p x1 '#(a b c d e) x2 x3)
        (vector-for-each p x1 '#(a b c d e) x2 x3 x4)
        (vector-for-each p x1 '#(a b c d e) x2 x3 x4 x5)
        (vector-for-each p x1 x2 '#(a b c d e))
        (vector-for-each p x1 x2 '#(a b c d e) x3)
        (vector-for-each p x1 x2 '#(a b c d e) x3 x4)
        (vector-for-each p x1 x2 '#(a b c d e) x3 x4 x5)
        (vector-for-each p x1 x2 x3 '#(a b c d e))
        (vector-for-each p x1 x2 x3 '#(a b c d e) x4)
        (vector-for-each p x1 x2 x3 '#(a b c d e) x4 x5)
        (vector-for-each p x1 x2 x3 x4 '#(a b c d e))
        (vector-for-each p x1 x2 x3 x4 '#(a b c d e) x5)
        (vector-for-each p x1 x2 x3 x4 x5 '#(a b c d e))))
    (procedure? $vector-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($vector-for-each-f1 q '#(1 2 3 4 5) '#(f g h i j) '#(k l m n o) '#(p q r s t) '#(u v w x y))
      (reverse ls))
    '((a) (b) (c) (d) (e) (1 a) (2 b) (3 c) (4 d) (5 e)
      (f 1 a) (g 2 b) (h 3 c) (i 4 d) (j 5 e) (k f 1 a)
      (l g 2 b) (m h 3 c) (n i 4 d) (o j 5 e) (p k f 1 a)
      (q l g 2 b) (r m h 3 c) (s n i 4 d) (t o j 5 e)
      (u p k f 1 a) (v q l g 2 b) (w r m h 3 c) (x s n i 4 d)
      (y t o j 5 e) (a 1) (b 2) (c 3) (d 4) (e 5) (f a 1)
      (g b 2) (h c 3) (i d 4) (j e 5) (k f a 1) (l g b 2)
      (m h c 3) (n i d 4) (o j e 5) (p k f a 1) (q l g b 2)
      (r m h c 3) (s n i d 4) (t o j e 5) (u p k f a 1)
      (v q l g b 2) (w r m h c 3) (x s n i d 4) (y t o j e 5)
      (a f 1) (b g 2) (c h 3) (d i 4) (e j 5) (k a f 1)
      (l b g 2) (m c h 3) (n d i 4) (o e j 5) (p k a f 1)
      (q l b g 2) (r m c h 3) (s n d i 4) (t o e j 5)
      (u p k a f 1) (v q l b g 2) (w r m c h 3) (x s n d i 4)
      (y t o e j 5) (a k f 1) (b l g 2) (c m h 3) (d n i 4)
      (e o j 5) (p a k f 1) (q b l g 2) (r c m h 3)
      (s d n i 4) (t e o j 5) (u p a k f 1) (v q b l g 2)
      (w r c m h 3) (x s d n i 4) (y t e o j 5) (a p k f 1)
      (b q l g 2) (c r m h 3) (d s n i 4) (e t o j 5)
      (u a p k f 1) (v b q l g 2) (w c r m h 3) (x d s n i 4)
      (y e t o j 5) (a u p k f 1) (b v q l g 2) (c w r m h 3)
      (d x s n i 4) (e y t o j 5)))
 ; check for proper tail recursion
   (equal?
     (list
       (let ([s (statistics)])
         (let ([k 100000] [v '#(a b c)])
           (let ([n k] [m 0])
             (define (f) (unless (fx= n 0) (vector-for-each foo v)))
             (define (foo x)
               (set! m (+ m 1))
               (when (eq? x (vector-ref v (fx- (vector-length v) 1)))
                 (set! n (- n 1))
                 (f)
                 17)) ; blow tail recursion here
             (f)
             (list (> (sstats-bytes (sstats-difference (statistics) s))
                      10000)
                   (eqv? n 0)
                   (eqv? m (* k (vector-length v)))))))
       (let ([s (statistics)])
         (let ([k 100000] [v '#(a b c)])
           (let ([n k] [m 0])
             (define (f) (unless (fx= n 0) (vector-for-each foo v)))
             (define (foo x)
               (set! m (+ m 1))
               (when (eq? x (vector-ref v (fx- (vector-length v) 1)))
                 (set! n (- n 1))
                 (f)))
             (f)
             (list (<= 0
                      (sstats-bytes (sstats-difference (statistics) s))
                      1000)
                   (eqv? n 0)
                   (eqv? m (* k (vector-length v))))))))
     '((#t #t #t) (#t #t #t)))
 )

(define $merge-sort
  (lambda (lt? ls)
    (define merge
      (lambda (ls1 ls2)
        (if (null? ls1)
            ls2
            (if (null? ls2)
                ls1
                (if (lt? (car ls1) (car ls2))
                    (cons (car ls1) (merge (cdr ls1) ls2))
                    (cons (car ls2) (merge ls1 (cdr ls2))))))))
    (define sort
      (lambda (ls n)
        (if (fx<= n 1)
            ls
            (let ([mid (quotient n 2)])
              (merge
                (sort (list-head ls mid) mid)
                (sort (list-tail ls mid) (fx- n mid)))))))
    (sort ls (length ls))))

(mat vector-sort
  (error? ; invalid number of arguments
    (vector-sort))
  (error? ; invalid number of arguments
    (vector-sort >))
  (error? ; invalid number of arguments
    (vector-sort '#(a b c)))
  (error? ; invalid number of arguments
    (vector-sort > '#(1 2 3) #t))
  (error? ; 3 is not a proper list
    (vector-sort > 3))
  (error? ; (1 2 3) is not a vector
    (vector-sort > '(1 2 3)))
  (error? ; #(a b c) is not a procedure
    (vector-sort '#(a b c) '#(a b c)))
  (error? ; b is not a real number
    (vector-sort > '#(1 b 3)))
  (equal? (vector-sort > '#()) '#())
  (let ([v (vector 3 2 1)])
    (and
      (equal? (vector-sort > v) '#(3 2 1))
      (equal? v '#(3 2 1))))
  (let ([v (vector 1 2 3)])
    (and
      (equal? (vector-sort > v) '#(3 2 1))
      (equal? v '#(1 2 3))))
  (let ([v (vector 2 3 1)])
    (and
      (equal? (vector-sort > v) '#(3 2 1))
      (equal? v '#(2 3 1))))
  (let ([v (vector -2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5)])
    (and
      (equal?
        (vector-sort < v)
        '#(-43 -11 -7 -5 -2 0 3.0 3 5 8 8.0 9 9))
      (equal? v '#(-2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5))))
  (let ([v (vector 2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9)])
    (and
      (equal?
        (vector-sort (lambda (x y) (< (abs x) (abs y))) v)
        '#(1 -1 2 2 -3 3 4 5 -5 8 8 -8 9 -9 10))
      (equal? v '#(2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9))))
  (let ([v (vector 1 3 2 4)])
    (and
      (equal? (vector-sort < v) '#(1 2 3 4))
      (equal? v '#(1 3 2 4))))
  (equal?
    (with-output-to-string
      (lambda ()
        (do ([n 1000 (fx- n 5)])
            ((fx= n 0))
          (write-char #\.)
          (flush-output-port)
          (do ([k 25 (fx- k 1)])
              ((fx= k 0))
            (let ([ls (map (lambda (x) (random k)) (make-list n))])
              (unless (let ([v (list->vector ls)])
                        (and
                          (equal?
                            (vector-sort < v)
                            (list->vector ($merge-sort < ls)))
                          (equal? v (list->vector ls))))
                (fprintf (console-output-port) "\n~s\n" ls)
                (errorf #f "failed")))))))
    (make-string 200 #\.))
)

(mat vector-sort!
  (error? ; invalid number of arguments
    (vector-sort!))
  (error? ; invalid number of arguments
    (vector-sort! >))
  (error? ; invalid number of arguments
    (vector-sort! '#(a b c)))
  (error? ; invalid number of arguments
    (vector-sort! > '#(1 2 3) #t))
  (error? ; 3 is not a proper list
    (vector-sort! > 3))
  (error? ; (1 2 3) is not a vector
    (vector-sort! > '(1 2 3)))
  (error? ; #(a b c) is not a procedure
    (vector-sort! '#(a b c) '#(a b c)))
  (error? ; b is not a real number
    (vector-sort! > '#(1 b 3)))
  (equal? (vector-sort! > '#()) (void))
  (let ([v (vector 3 2 1)])
    (and
      (equal? (vector-sort! > v) (void))
      (equal? v '#(3 2 1))))
  (let ([v (vector 1 2 3)])
    (and
      (equal? (vector-sort! > v) (void))
      (equal? v '#(3 2 1))))
  (let ([v (vector 2 3 1)])
    (and
      (equal? (vector-sort! > v) (void))
      (equal? v '#(3 2 1))))
  (let ([v (vector -2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5)])
    (and
      (equal?
        (vector-sort! < v)
        (void))
      (equal? v '#(-43 -11 -7 -5 -2 0 3.0 3 5 8 8.0 9 9))))
  (let ([v (vector 2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9)])
    (and
      (equal?
        (vector-sort! (lambda (x y) (< (abs x) (abs y))) v)
        (void))
      (equal? v '#(1 -1 2 2 -3 3 4 5 -5 8 8 -8 9 -9 10))))
  (let ([v (vector 1 3 2 4)])
    (and
      (equal? (vector-sort! < v) (void))
      (equal? v '#(1 2 3 4))))
  (equal?
    (with-output-to-string
      (lambda ()
        (do ([n 1000 (fx- n 5)])
            ((fx= n 0))
          (write-char #\.)
          (flush-output-port)
          (do ([k 25 (fx- k 1)])
              ((fx= k 0))
            (let ([ls (map (lambda (x) (random k)) (make-list n))])
              (unless (let ([v (list->vector ls)])
                        (and
                          (equal? (vector-sort! < v) (void))
                          (equal? v (list->vector ($merge-sort < ls)))))
                (fprintf (console-output-port) "\n~s\n" ls)
                (errorf #f "failed")))))))
    (make-string 200 #\.))

  (let ([Ns '(10 100 1024 2048 4096)])
    (with-interrupts-disabled
     (andmap
      (lambda (N)
        (define vec (make-vector (* 2 N)))
        ;; make the vector gen-1:
        (collect 0 1)
        ;; fill first half with immediates and second half with gen-0 objects,
        ;; where both halves are already in order
        (let loop ([i 0])
          (unless (fx= i N)
            (vector-set! vec i i)
            (vector-set! vec (fx+ i N) (number->string i))
            (loop (fx+ i 1))))
        ;; sort, moving objects in second half to first
        (vector-sort! (lambda (a b)
                        (cond
                          [(string? a)
                           (if (string? b)
                               (< (string->number a) (string->number b))
                               #t)]
                          [(string? b) #f]
                          [else (< a b)]))
                      vec)
        ;; check whether dirty words got recorded correctly...
        ;; collect all gen-0 into gen-1
        (collect 0 1)
        ;; make sure the right values are in the still in vector,
        ;; which suggests that gen-0 references were updated correctly
        (let loop ([i 0])
          (unless (fx= i N)
            (unless (equal? (vector-ref vec (fx+ i N)) i)
              (errorf 'oops "at ~s ~s" i (vector-ref vec (fx+ i N))))
            (unless (equal? (vector-ref vec i) (number->string i))
              (errorf 'oops "at ~s ~s" i (vector-ref vec i)))
            (loop (fx+ i 1))))
        ;; passes
        #t)
      Ns)))
)

(mat vector->immutable-vector
    (begin
      (define immutable-123-vector
        (vector->immutable-vector (vector 1 2 3)))
      #t)
    
    (immutable-vector? immutable-123-vector)
    (not (mutable-vector? immutable-123-vector))
    
    (equal? '#(1 2 3) immutable-123-vector)
    (eq? immutable-123-vector
         (vector->immutable-vector immutable-123-vector))

    ;; these also turn out to be conversions that should
    ;; leave an immutable vector alone:
    (eq? immutable-123-vector
         (immutable-vector-copy immutable-123-vector))
    (eq? immutable-123-vector
         (immutable-vector-append immutable-123-vector))

    (mutable-vector? (make-vector 5))
    (not (immutable-vector? (make-vector 5)))
    
    (immutable-vector? (vector->immutable-vector (vector)))
    (not (mutable-vector? (vector->immutable-vector (vector))))
    (not (immutable-vector? (vector)))
    (mutable-vector? (vector))
    
    (not (immutable-vector? (vector-copy immutable-123-vector)))

    (error? (vector-set! immutable-123-vector 0 1))
    (error? (vector-set-fixnum! immutable-123-vector 0 1))
    (error? (vector-fill! immutable-123-vector 0))
    (error? (vector-sort! < immutable-123-vector))
)

(mat vector-cas!
  (begin
    (define vec1 (vector 1 2 3))
    (define vec2 (vector 'apple 'banana 'coconut))
    (eq? 1 (vector-ref vec1 0)))
  (not (vector-cas! vec1 0 0 1))
  (eq? 1 (vector-ref vec1 0))
  (retry-for-spurious (vector-cas! vec1 0 1 4))
  (eq? 4 (vector-ref vec1 0))
  (not (vector-cas! vec1 0 1 5))

  (not (vector-cas! vec1 1 0 1))
  (eq? 2 (vector-ref vec1 1))
  (retry-for-spurious (vector-cas! vec1 1 2 5))
  (eq? 5 (vector-ref vec1 1))

  (not (vector-cas! vec2 0 'banana 'donut))
  (retry-for-spurious (vector-cas! vec2 0 'apple 'donut))
  (not (vector-cas! vec2 0 'apple 'eclair))
  (eq? 'donut (vector-ref vec2 0))

  (not (vector-cas! vec2 1 'apple 'fig))
  (retry-for-spurious (vector-cas! vec2 1 'banana 'fig))
  (not (vector-cas! vec2 1 'banana 'grape))
  (eq? 'fig (vector-ref vec2 1))

  (error? (vector-cas! vec1)) ; arity
  (error? (vector-cas! vec1 1)) ; arity
  (error? (vector-cas! vec1 1 2)) ; arity
  (error? (vector-cas! 1 vec1 2 3)) ; not a vector
  (error? (vector-cas! (vector->immutable-vector vec1) 1 2 3)) ; not a mutable vector
  (error? (vector-cas! vec1 vec1 2 3)) ; not a fixnum
  (error? (vector-cas! vec1 (expt 2 100) 2 3)) ; not a fixnum
  (error? (vector-cas! vec1 -1 2 3)) ; out of range
  (error? (vector-cas! vec1 -2 2 3)) ; out of range
  (error? (vector-cas! vec1 5 2 3)) ; out of range
  (error? (vector-cas! vec1 (expt 2 26) 2 3)) ; out of range
  (error? (vector-cas! vec1 (expt 2 40) 2 3)) ; out of range

  ;; make sure `vector-cas!` works with GC generations:
  (begin
    (collect 0)
    (let ([g1 (gensym)])
      (and (retry-for-spurious (vector-cas! vec2 2 'coconut g1))
           (begin
             (collect 0)
             (eq? g1 (vector-ref vec2 2))))))
)

(mat stencil-vector
  (>= (stencil-vector-mask-width) 24)
  (stencil-vector? (stencil-vector 1 0))
  (not (stencil-vector? 10))

  (begin
    (define alphabet-sv (stencil-vector #b10101010101010101010 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))
    (define mod-sv (stencil-vector #b1001 'a 'b))
    (and (stencil-vector? alphabet-sv)
         (stencil-vector? mod-sv)))

  (error? (stencil-vector 'x))
  (error? (stencil-vector -1))
  (error? (stencil-vector 0 'apple))
  (error? (stencil-vector 3 'apple))
  (error? (stencil-vector 3 'apple))

  (<= (stencil-vector-mask-width) 58) ; so the next test reports the same error on all platforms
  (error? (stencil-vector (expt 2 58) 'a))

  (error? (stencil-vector-length 3))
  (error? (stencil-vector-mask 3))
  (error? (stencil-vector-ref 3 0))

  (eqv? 2 (stencil-vector-length (stencil-vector 3 'apple 'banana)))
  (eqv? 3 (stencil-vector-mask (stencil-vector 3 'apple 'banana)))
  (eqv? 2 (stencil-vector-length (stencil-vector 514 'apple 'banana)))
  (eqv? 514 (stencil-vector-mask (stencil-vector 514 'apple 'banana)))
  (eqv? 10 (stencil-vector-length (stencil-vector 1023 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
  (eqv? 1 (stencil-vector-length (stencil-vector (expt 2 (sub1 (stencil-vector-mask-width))) 'a)))
  (eqv? 1 (stencil-vector-length (stencil-vector (expt 2 (- (stencil-vector-mask-width) 2)) 'a)))

  (error? (stencil-vector-ref (stencil-vector 0) 0))
  (error? (stencil-vector-ref (stencil-vector 3 'apple 'banana) 2))
  (error? (stencil-vector-ref (stencil-vector 3 'apple 'banana) -1))
  (error? (stencil-vector-ref (stencil-vector 3 'apple 'banana) (expt 2 100)))
  (error? (stencil-vector-ref (stencil-vector 3 'apple 'banana) 1.0))

  (eq? 'a (stencil-vector-ref alphabet-sv 0))
  (eq? 'f (stencil-vector-ref alphabet-sv 5))
  (eq? 'j (stencil-vector-ref alphabet-sv 9))

  (error? (stencil-vector-set! (stencil-vector 0) 0 'x))
  (error? (stencil-vector-set! (stencil-vector 3 'apple 'banana) 2 'x))
  (error? (stencil-vector-set! (stencil-vector 3 'apple 'banana) -1 'x))
  (error? (stencil-vector-set! (stencil-vector 3 'apple 'banana) (expt 2 100) 'x))
  (error? (stencil-vector-set! (stencil-vector 3 'apple 'banana) 1.0 'x))

  (equal? '(ok b) (begin
                    (stencil-vector-set! mod-sv 0 'ok)
                    (list (stencil-vector-ref mod-sv 0)
                          (stencil-vector-ref mod-sv 1))))

  (error? (stencil-vector-update 1 0 1 'z))
  (error? (stencil-vector-update alphabet-sv 0 2 'z))
  (error? (stencil-vector-update alphabet-sv 1 0))
  (error? (stencil-vector-update alphabet-sv 0 1))
  (error? (stencil-vector-update alphabet-sv 0 1 'a 'b))

  ;; add 1
  (let ([v (stencil-vector-update alphabet-sv 0 1 'z)])
    (and (eqv? 11 (stencil-vector-length v))
         (eq? 'z (stencil-vector-ref v 0))
         (eq? 'a (stencil-vector-ref v 1))
         (eq? 'b (stencil-vector-ref v 2))
         (eq? 'j (stencil-vector-ref v 10))))
  ;; add 2
  (let ([v (stencil-vector-update alphabet-sv 0 #b10001 'z 'w)])
    (and (eqv? 12 (stencil-vector-length v))
         (eq? 'z (stencil-vector-ref v 0))
         (eq? 'a (stencil-vector-ref v 1))
         (eq? 'b (stencil-vector-ref v 2))
         (eq? 'w (stencil-vector-ref v 3))
         (eq? 'c (stencil-vector-ref v 4))
         (eq? 'j (stencil-vector-ref v 11))))
  ;; remove 1
  (let ([v (stencil-vector-update alphabet-sv 2 0)])
    (and (eqv? 9 (stencil-vector-length v))
         (eq? 'b (stencil-vector-ref v 0))
         (eq? 'c (stencil-vector-ref v 1))
         (eq? 'j (stencil-vector-ref v 8))))
  ;; remove 2
  (let ([v (stencil-vector-update alphabet-sv #b10000010 0)])
    (and (eqv? 8 (stencil-vector-length v))
         (eq? 'b (stencil-vector-ref v 0))
         (eq? 'c (stencil-vector-ref v 1))
         (eq? 'e (stencil-vector-ref v 2))
         (eq? 'j (stencil-vector-ref v 7))))
  ;; add 2 and remove 2
  (let ([v (stencil-vector-update alphabet-sv #b10000010 #b10001 'z 'w)])
    (and (eqv? 10 (stencil-vector-length v))
         (eq? 'z (stencil-vector-ref v 0))
         (eq? 'b (stencil-vector-ref v 1))
         (eq? 'w (stencil-vector-ref v 2))
         (eq? 'c (stencil-vector-ref v 3))
         (eq? 'e (stencil-vector-ref v 4))
         (eq? 'j (stencil-vector-ref v 9))))
  ;; remove all
  (eqv? 0 (stencil-vector-length (stencil-vector-update alphabet-sv #b10101010101010101010 0)))

  (error? (stencil-vector-truncate! 1 0))
  (error? (stencil-vector-truncate! (stencil-vector 0) 1))
  (error? (stencil-vector-truncate! (stencil-vector 7) 15))
  (error? (stencil-vector-truncate! (stencil-vector 6) 7))
  (let ([sv (stencil-vector 7 1 2 3)])
    (stencil-vector-truncate! sv 3)
    (and (stencil-vector? sv)
         (eqv? (stencil-vector-mask sv) 3)
         (eqv? (stencil-vector-ref sv 0) 1)
         (eqv? (stencil-vector-ref sv 1) 2)))
  (let ([sv (stencil-vector 7 1 2 3)])
    (stencil-vector-truncate! sv 6)
    (and (stencil-vector? sv)
         (eqv? (stencil-vector-mask sv) 6)
         (eqv? (stencil-vector-ref sv 0) 1)
         (eqv? (stencil-vector-ref sv 1) 2)))

  ;; check fasl:
  (let ([s (stencil-vector 42 'a 'b 'c)])
    (define-values (o get) (open-bytevector-output-port))
    (fasl-write s o)
    (equal? s (fasl-read (open-bytevector-input-port (get)))))
   
  ;; try to check GC interaction
  (let loop ([n 100000]
             [v1 (stencil-vector 7 'a 'b 'c)]
             [v2 (stencil-vector 28 "a" "b" "c")]
             [v3 (stencil-vector 15 (vector 'x) (vector 'y) (vector 'z)  (vector 'w))])
    (or (fx= n 0)
        (and
         (fx= (stencil-vector-mask v1) 7)
         (eqv? (stencil-vector-ref v1 1) 'b)
         (fx= (stencil-vector-mask v2) 28)
         (equal? (stencil-vector-ref v2 0) "a")
         (equal? (stencil-vector-ref v2 1) "b")
         (equal? (stencil-vector-ref v2 2) "c")
         (fx= (stencil-vector-mask v3) 15)
         (equal? (stencil-vector-ref v3 0) '#(x))
         (equal? (stencil-vector-ref v3 1) '#(y))
         (equal? (stencil-vector-ref v3 2) '#(z))
         (equal? (stencil-vector-ref v3 3) '#(w))
         (loop (fx- n 1)
               (stencil-vector-update
                (stencil-vector-update v1 3 1 'a)
                0 2 'b)
               (stencil-vector-update v2 12 12 (string #\a) (string #\b))
               (stencil-vector-update
                (stencil-vector-update v3 6 16 (vector 'extra))
                16 6 (vector 'y) (vector 'z))))))

  )

(mat system-stencil-vector
  (#%$stencil-vector? (stencil-vector 1 0))
  (not (#%$stencil-vector? 10))
  (#%$system-stencil-vector? (#%$system-stencil-vector 1 0))
  (not (#%$system-stencil-vector? (stencil-vector 1 0)))
  (not (#%$system-stencil-vector? 10))
  (not (stencil-vector? (#%$system-stencil-vector 1 0)))

  (begin
    (define alphabet-sv (#%$system-stencil-vector #b10101010101010101010 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))
    (define mod-sv (#%$system-stencil-vector #b1001 'a 'b))
    (and (#%$stencil-vector? alphabet-sv)
         (#%$stencil-vector? mod-sv)
         (#%$system-stencil-vector? alphabet-sv)
         (#%$system-stencil-vector? mod-sv)))

  (error? (#%$system-stencil-vector 'x))
  (error? (#%$system-stencil-vector -1))
  (error? (#%$system-stencil-vector 0 'apple))
  (error? (#%$system-stencil-vector 3 'apple))
  (error? (#%$system-stencil-vector 3 'apple))

  (<= (stencil-vector-mask-width) 58) ; so the next test reports the same error on all platforms
  (error? (#%$system-stencil-vector (expt 2 58) 'a))

  (error? (#%$stencil-vector-length 3))
  (error? (#%$stencil-vector-mask 3))
  (error? (#%$stencil-vector-ref 3 0))

  (eqv? 2 (#%$stencil-vector-length (stencil-vector 3 'apple 'banana)))
  (eqv? 2 (#%$stencil-vector-length (#%$system-stencil-vector 3 'apple 'banana)))
  (eqv? 3 (#%$stencil-vector-mask (stencil-vector 3 'apple 'banana)))
  (eqv? 3 (#%$stencil-vector-mask (#%$system-stencil-vector 3 'apple 'banana)))
  (eqv? 514 (#%$stencil-vector-mask (#%$system-stencil-vector 514 'apple 'banana)))
  (eqv? 10 (#%$stencil-vector-length (#%$system-stencil-vector 1023 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
  (eqv? 1 (#%$stencil-vector-length (#%$system-stencil-vector (expt 2 (sub1 (stencil-vector-mask-width))) 'a)))
  (eqv? 1 (#%$stencil-vector-length (#%$system-stencil-vector (expt 2 (- (stencil-vector-mask-width) 2)) 'a)))

  (error? (#%$stencil-vector-ref (stencil-vector 0) 0))
  (error? (#%$stencil-vector-ref (#%$system-stencil-vector 0) 0))
  (error? (#%$stencil-vector-ref (stencil-vector 3 'apple 'banana) 2))
  (error? (#%$stencil-vector-ref (#%$system-stencil-vector 3 'apple 'banana) 2))

  (error? (#%$stencil-vector-set! (stencil-vector 0) 0 'x))
  (error? (#%$stencil-vector-set! (#%$system-stencil-vector 0) 0 'x))

  (equal? '(ok b) (begin
                    (#%$stencil-vector-set! mod-sv 0 'ok)
                    (list (#%$stencil-vector-ref mod-sv 0)
                          (#%$stencil-vector-ref mod-sv 1))))

  (error? (#%$system-stencil-vector-update 1 0 1 'z))

  ;; add 1
  (let ([v (#%$system-stencil-vector-update alphabet-sv 0 1 'z)])
    (and (eqv? 11 (#%$stencil-vector-length v))
         (eq? 'z (#%$stencil-vector-ref v 0))
         (eq? 'a (#%$stencil-vector-ref v 1))
         (eq? 'b (#%$stencil-vector-ref v 2))
         (eq? 'j (#%$stencil-vector-ref v 10))))
  ;; add 2
  (let ([v (#%$system-stencil-vector-update alphabet-sv 0 #b10001 'z 'w)])
    (and (eqv? 12 (#%$stencil-vector-length v))
         (eq? 'z (#%$stencil-vector-ref v 0))
         (eq? 'a (#%$stencil-vector-ref v 1))
         (eq? 'b (#%$stencil-vector-ref v 2))
         (eq? 'w (#%$stencil-vector-ref v 3))
         (eq? 'c (#%$stencil-vector-ref v 4))
         (eq? 'j (#%$stencil-vector-ref v 11))))
  ;; remove 1
  (let ([v (#%$system-stencil-vector-update alphabet-sv 2 0)])
    (and (eqv? 9 (#%$stencil-vector-length v))
         (eq? 'b (#%$stencil-vector-ref v 0))
         (eq? 'c (#%$stencil-vector-ref v 1))
         (eq? 'j (#%$stencil-vector-ref v 8))))
  ;; remove 2
  (let ([v (#%$system-stencil-vector-update alphabet-sv #b10000010 0)])
    (and (eqv? 8 (#%$stencil-vector-length v))
         (eq? 'b (#%$stencil-vector-ref v 0))
         (eq? 'c (#%$stencil-vector-ref v 1))
         (eq? 'e (#%$stencil-vector-ref v 2))
         (eq? 'j (#%$stencil-vector-ref v 7))))
  ;; add 2 and remove 2
  (let ([v (#%$system-stencil-vector-update alphabet-sv #b10000010 #b10001 'z 'w)])
    (and (eqv? 10 (#%$stencil-vector-length v))
         (eq? 'z (#%$stencil-vector-ref v 0))
         (eq? 'b (#%$stencil-vector-ref v 1))
         (eq? 'w (#%$stencil-vector-ref v 2))
         (eq? 'c (#%$stencil-vector-ref v 3))
         (eq? 'e (#%$stencil-vector-ref v 4))
         (eq? 'j (#%$stencil-vector-ref v 9))))
  ;; remove all
  (eqv? 0 (#%$stencil-vector-length (#%$system-stencil-vector-update alphabet-sv #b10101010101010101010 0)))

  (error? (#%$system-stencil-vector-truncate! 1 0))
  (error? (#%$system-stencil-vector-truncate! (#%$system-stencil-vector 0) 1))
  (error? (#%$system-stencil-vector-truncate! (#%$system-stencil-vector 7) 15))
  (error? (#%$system-stencil-vector-truncate! (#%$system-stencil-vector 6) 7))
  (let ([sv (#%$system-stencil-vector 7 1 2 3)])
    (#%$system-stencil-vector-truncate! sv 3)
    (and (#%$system-stencil-vector? sv)
         (eqv? (#%$stencil-vector-mask sv) 3)
         (eqv? (#%$stencil-vector-ref sv 0) 1)
         (eqv? (#%$stencil-vector-ref sv 1) 2)))
  (let ([sv (#%$system-stencil-vector 7 1 2 3)])
    (#%$system-stencil-vector-truncate! sv 6)
    (and (#%$system-stencil-vector? sv)
         (eqv? (#%$stencil-vector-mask sv) 6)
         (eqv? (#%$stencil-vector-ref sv 0) 1)
         (eqv? (#%$stencil-vector-ref sv 1) 2)))

  ;; check fasl:
  (let ([s (#%$system-stencil-vector 13 'a 'b 'c)])
    (define-values (o get) (open-bytevector-output-port))
    (fasl-write s o)
    (equal? s (fasl-read (open-bytevector-input-port (get)))))

  )
