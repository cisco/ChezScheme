;;; 5-2.ms
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

(define (make-cyclic-list)
  (let ((ls (list 'a 'b)))
    (set-cdr! (last-pair ls) ls)
    ls))
(define cyclic-list (make-cyclic-list))
(define cyclic-alist
  (let ((ls (list '(a . 1) '(b . 2) '(3.2 . 3) '("a" . 4))))
    (set-cdr! (last-pair ls) ls)
    ls))

(mat cons
    (equal? (cons 3 4) '(3 . 4))
    (equal? (cons 3 '(a)) '(3 a))
    (not (equal? (cons 2 3) (cons 3 2)))
 )

(mat car
    (eq? (car '(a b c)) 'a)
    (eq? (car (cons 'a 'b)) 'a)
    (error? (car "hi"))
 )

(mat cdr
    (equal? (cdr '(a b c)) '(b c))
    (eq? (cdr (cons 'a 'b)) 'b)
    (null? (cdr (cons 'a '())))
    (error? (cdr 3))
 )   

(mat set-car!
    (let ((x (list 'a 'b))) (set-car! x 3) (equal? x '(3 b)))
    (error? (set-car! 'a 'b))
 )

(mat set-cdr!
    (let ((x (list 'a 'b))) (set-cdr! x 3) (equal? x '(a . 3)))
    (error? (set-cdr! 'a 'b))
 )

(set! bush
   (lambda (n)
      (let f ((n n) (x '()))
          (if (zero? n)
              x
              (cons (f (1- n) (cons 'a x)) (f (1- n) (cons 'd x)))))))
(set! b2 (bush 2))
(set! b3 (bush 3))
(set! b4 (bush 4))
(mat c....r
    ;first, get some confidence in bush
    (equal? b2 '(((a a) d a) (a d) d d))
    (equal? (caar b2) '(a a))
    (equal? (cadr b2) '(a d))
    (equal? (cdar b2) '(d a))
    (equal? (cddr b2) '(d d))
    (equal? (caaar b3) '(a a a))
    (equal? (caadr b3) '(a a d))
    (equal? (cadar b3) '(a d a))
    (equal? (caddr b3) '(a d d))
    (equal? (cdaar b3) '(d a a))
    (equal? (cdadr b3) '(d a d))
    (equal? (cddar b3) '(d d a))
    (equal? (cdddr b3) '(d d d))
    (equal? (caaaar b4) '(a a a a))
    (equal? (caaadr b4) '(a a a d))
    (equal? (caadar b4) '(a a d a))
    (equal? (caaddr b4) '(a a d d))
    (equal? (cadaar b4) '(a d a a))
    (equal? (cadadr b4) '(a d a d))
    (equal? (caddar b4) '(a d d a))
    (equal? (cadddr b4) '(a d d d))
    (equal? (cdaaar b4) '(d a a a))
    (equal? (cdaadr b4) '(d a a d))
    (equal? (cdadar b4) '(d a d a))
    (equal? (cdaddr b4) '(d a d d))
    (equal? (cddaar b4) '(d d a a))
    (equal? (cddadr b4) '(d d a d))
    (equal? (cdddar b4) '(d d d a))
    (equal? (cddddr b4) '(d d d d))
)

(define a.b '(a . b))

(mat c....r-errors
    (error? (caar a.b))
    (error? (cadr a.b))
    (error? (cdar a.b))
    (error? (cddr a.b))
    (error? (caaar a.b))
    (error? (caadr a.b))
    (error? (cadar a.b))
    (error? (caddr a.b))
    (error? (cdaar a.b))
    (error? (cdadr a.b))
    (error? (cddar a.b))
    (error? (cdddr a.b))
    (error? (caaaar a.b))
    (error? (caaadr a.b))
    (error? (caadar a.b))
    (error? (caaddr a.b))
    (error? (cadaar a.b))
    (error? (cadadr a.b))
    (error? (caddar a.b))
    (error? (cadddr a.b))
    (error? (cdaaar a.b))
    (error? (cdaadr a.b))
    (error? (cdadar a.b))
    (error? (cdaddr a.b))
    (error? (cddaar a.b))
    (error? (cddadr a.b))
    (error? (cdddar a.b))
    (error? (cddddr a.b))
)

(mat make-list
    (equal? (length (make-list 15)) 15)
    (equal? (make-list 3 'a) '(a a a))
    (null? (make-list 0 0))
 )

(mat list
    (equal? (list 1 2 3 4) '(1 2 3 4))
    (null? (list))
 )

(mat list*
    (error? (list*))
    (equal? (list* 1) 1)
    (equal? (list* (list 1 2 3)) '(1 2 3))
    (equal? (list* 1 2 3 4) '(1 2 3 . 4))
    (equal?
      (list* 1 2 (list* 3 4 5) (list* 6 7 8))
      '(1 2 (3 4 . 5) 6 7 . 8))
    (not (list* #f))
    (eq? (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2]) (expand/optimize '(not (list* #f)))) #t)
 )

(mat cons*
    (error? (cons*))
    (equal? (cons* 1) 1)
    (equal? (cons* (list 1 2 3)) '(1 2 3))
    (equal? (cons* 1 2 3 4) '(1 2 3 . 4))
    (equal?
      (cons* 1 2 (list* 3 4 5) (list* 6 7 8))
      '(1 2 (3 4 . 5) 6 7 . 8))
    (not (cons* #f))
    (eq? (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2]) (expand/optimize '(not (cons* #f)))) #t)
 )

(mat length
    (= (length '(1 2 3 4 5)) 5)
    (= (length '()) 0)
    ; check that expand-primitives doesn't generate incorrect code.
    ; we don't check that it optimizes, however.
    (let ([ls* (map make-list '(0 1 2 3 4 5 8 9 10 99 100 101 1000))])
      (define-syntax test1
        (syntax-rules ()
          [(_ prim)
           (let ()
             (define (f x)
               (and
                 (prim (#3%length x))
                 (prim (#3%length x))))
             (andmap
               (lambda (x)
                 (let ([n (length x)])
                   (equal?
                     (f x)
                     (prim n))))
               ls*))]))
      (define-syntax test2
        (syntax-rules ()
          [(_ prim)
           (let ()
             (define (f x)
               (list
                 (prim (#3%length x) 0)
                 (prim 0 (#3%length x))
                 (prim (#3%length x) 1)
                 (prim 1 (#3%length x))
                 (prim (#3%length x) 4)
                 (prim 4 (#3%length x))
                 (prim (#3%length x) 9)
                 (prim 9 (#3%length x))
                 (prim (#3%length x) 100)
                 (prim 100 (#3%length x))))
             (andmap
               (lambda (x)
                 (let ([n (length x)])
                   (equal?
                     (f x)
                     (list
                       (prim n 0)
                       (prim 0 n)
                       (prim n 1)
                       (prim 1 n)
                       (prim n 4)
                       (prim 4 n)
                       (prim n 9)
                       (prim 9 n)
                       (prim n 100)
                       (prim 100 n)))))
               ls*))]))
      (and
        (test1 zero?)
        (test1 positive?)
        (test1 nonnegative?)
        (test1 negative?)
        (test1 nonpositive?)
        (test1 fxzero?)
        (test1 fxpositive?)
        (test1 fxnonnegative?)
        (test1 fxnegative?)
        (test1 fxnonpositive?)
        (test2 eq?)
        (test2 eqv?)
        (test2 equal?)
        (test2 <)
        (test2 <=)
        (test2 =)
        (test2 >=)
        (test2 >)
        (test2 r6rs:<)
        (test2 r6rs:<=)
        (test2 r6rs:=)
        (test2 r6rs:>=)
        (test2 r6rs:>)
        (test2 r6rs:<)
        (test2 r6rs:<=)
        (test2 r6rs:=)
        (test2 r6rs:>=)
        (test2 r6rs:>)
        (test2 fx<)
        (test2 fx<=)
        (test2 fx=)
        (test2 fx>=)
        (test2 fx>)
        (test2 fx<?)
        (test2 fx<=?)
        (test2 fx=?)
        (test2 fx>=?)
        (test2 fx>?)
        (test2 #%$fxu<)))
 )

(mat list-ref
    (eq? (list-ref '(a b c d e) 3) 'd)
    (eq? (list-ref '(a b c d e) 4) 'e)
    (eq? (list-ref '(a b) 0) 'a)
    (eq? (list-ref '(a b . c) 1) 'b)
    (eq? (list-ref cyclic-list 20) 'a)
    (eq? (list-ref cyclic-list 21) 'b)
    (eq? (list-ref cyclic-list 10000) 'a)
    (eq? (list-ref cyclic-list 10001) 'b)
    (eq? (list-ref cyclic-list (expt 2 1000)) 'a)
    (eq? (list-ref cyclic-list (+ (expt 2 1000) 1)) 'b)
    (eq? (list-ref `(1 2 . ,cyclic-list) 20) 'a)
    (eq? (list-ref `(1 2 . ,cyclic-list) 21) 'b)
    (eq? (list-ref `(1 2 . ,cyclic-list) 10000) 'a)
    (eq? (list-ref `(1 2 . ,cyclic-list) 10001) 'b)
    (eq? (list-ref `(1 2 . ,cyclic-list) (expt 2 1000)) 'a)
    (eq? (list-ref `(1 2 . ,cyclic-list) (+ (expt 2 1000) 1)) 'b)
    (error? (list-ref 'a 0))
    (error? (list-ref '(a b . c) 4))
    (error? (list-ref '(a b) 4))
    (error? (list-ref '(a b c) 4))
    (error? (list-ref '(a b c d) 4))
    (error? (list-ref '(a b c . e) 4))
    (error? (list-ref '(a b c d . e) 4))
    (error? (list-ref '(a b c d) 5))
    (error? (list-ref '(a b c d e) 5))
    (error? (list-ref '(a b c d e . f) 5))
    (error? (list-ref '(a b . c) 10000))
    (error? (list-ref '(a b c) 10000))
    (error? (list-ref '(a b . c) 444444444444444444444444444444444444444444))
    (error? (list-ref '(a b c) 444444444444444444444444444444444444444444))
    (error? (list-ref '(a b c) -1))
    (error? (list-ref '(a b c) -4444444444444444444444))
    (error? (list-ref '(a b c) 'a))
 )

(mat list-tail
    (let ((x '(d e f))) (eq? (list-tail (list* 'a 'b x) 2) x))
    (let ((x '(d e f))) (eq? (list-tail (list* 'a 'b x) 3) (cdr x)))
    (let ((x '(a b c))) (eq? (list-tail x 0) x))
    (null? (list-tail '(a b c) 3))
    (eq? (list-tail '(a b . c) 2) 'c)
    (eq? (list-tail cyclic-list 20) cyclic-list)
    (eq? (list-tail cyclic-list 21) (cdr cyclic-list))
    (eq? (list-tail cyclic-list 10000) cyclic-list)
    (eq? (list-tail cyclic-list 10001) (cdr cyclic-list))
    (eq? (list-tail cyclic-list (expt 2 1000)) cyclic-list)
    (eq? (list-tail cyclic-list (+ (expt 2 1000) 1)) (cdr cyclic-list))
    (eq? (list-tail `(1 2 . ,cyclic-list) 20) cyclic-list)
    (eq? (list-tail `(1 2 . ,cyclic-list) 21) (cdr cyclic-list))
    (eq? (list-tail `(1 2 . ,cyclic-list) 10000) cyclic-list)
    (eq? (list-tail `(1 2 . ,cyclic-list) 10001) (cdr cyclic-list))
    (eq? (list-tail `(1 2 . ,cyclic-list) (expt 2 1000)) cyclic-list)
    (eq? (list-tail `(1 2 . ,cyclic-list) (+ (expt 2 1000) 1))
         (cdr cyclic-list))
    (eq? (list-tail 'a 0) 'a)
    (error? (list-tail '(a b . c) 4))
    (error? (list-tail '(a b c . d) 4))
    (error? (list-tail '(a b . c) 5))
    (error? (list-tail '(a b c . d) 5))
    (error? (list-tail '(a b c d . e) 5))
    (error? (list-tail '(a) 4))
    (error? (list-tail '(a b) 4))
    (error? (list-tail '(a b c) 4))
    (error? (list-tail '(a b) 5))
    (error? (list-tail '(a b c) 5))
    (error? (list-tail '(a b c d) 5))
    (error? (list-tail '(a b . c) 10000))
    (error? (list-tail '(a b c) 10000))
    (error? (list-tail '(a b . c) 444444444444444444444444444444444444444444))
    (error? (list-tail '(a b c) 444444444444444444444444444444444444444444))
    (error? (list-tail '(a b c) -1))
    (error? (list-tail '(a b c) -4444444444444444444444))
    (error? (list-tail '(a b c) 'a))
 )

(mat list-head
    (equal? (list-head '(a b c) 3) '(a b c))
    (equal? (list-head '(a b . c) 2) '(a b))
    (equal? (list-head cyclic-list 0) '())
    (equal? (list-head cyclic-list 1) '(a))
    (equal? (list-head cyclic-list 2) '(a b))
    (equal? (list-head cyclic-list 20) '(a b a b a b a b a b a b a b a b a b a b))
    (equal? (list-head cyclic-list 21) '(a b a b a b a b a b a b a b a b a b a b a))
    (equal?
      (let ([ls (list-head cyclic-list 10000)])
        (list (length ls)
              (length (remq 'a ls))
              (length (remq 'b ls))
              (last-pair ls)))
      '(10000 5000 5000 (b)))
    (equal?
      (let ([ls (list-head cyclic-list 10001)])
        (list (length ls)
              (length (remq 'a ls))
              (length (remq 'b ls))
              (last-pair ls)))
      '(10001 5000 5001 (a)))
    (error? (list-head '(a . b) 3))
    (error? (list-head '(a b . c) 3))
    (equal? (list-head '(a b c . d) 3) '(a b c))
    (error? (list-head '(a b . c) 4))
    (error? (list-head '(a b c . d) 4))
    (equal? (list-head '(a b c d . e) 4) '(a b c d))
    (error? (list-head '(a b . c) 10000))
    (error? (list-head '(a b c) 4))
    (error? (list-head '(a b c) 5))
    (error? (list-head '(a b c d) 5))
    (error? (list-head '(a b c d) 6))
    (error? (list-head '(a b c) 10000))
    (error? (list-head '(a b c) 10001))
    (error? (list-head '(a b c d) 10000))
    (error? (list-head '(a b c d) 10001))
    (error? (list-head '(a b c) -1))
    (error? (list-head '(a b c) -2))
    (error? (list-head '(a b c) 4444444444444444444444))
    (error? (list-head '(a b c) -4444444444444444444445))
    (error? (list-head '(a b c) 'a))
    (error? (list-head '(a b c) 2.0))
 )

(mat last-pair
   (let ([x '(d e f)])
      (eq? (last-pair x) (cddr x)))
   (let ([x (cons 'c 'd)])
      (let ([y (list* 'a 'b x)])
         (eq? (last-pair y) x)))
   (error? (last-pair cyclic-list))
   (error? (last-pair (cdr cyclic-list)))
   (error? (last-pair `(a b c . ,cyclic-list)))
)

(mat list-copy
    (eq? (list-copy '()) '())
    (equal? (list-copy '(a b c)) '(a b c))
    (let* ((p1 '(a b c)) (p2 (cdr p1)) (p3 (cdr p2)))
        (let ((c1 (list-copy p1)))
            (not
                (or (memq c1 (list p1 p2 p3))
                    (memq (cdr c1) (list p1 p2 p3))
                    (memq (cddr c1) (list p1 p2 p3))))))
   (error? (list-copy '#(a b c)))
   (error? (list-copy '(a b . c)))
   (error? (list-copy cyclic-list))
   (error? (list-copy (cdr cyclic-list)))
   (error? (list-copy `(a b c . ,cyclic-list)))
 )

(mat append
    (null? (append))
    (equal? (append '(a b c)) '(a b c))
    (let ((x '(a b c)) (y '(d e f)))
        (let ((z (append x y)))
            (and (equal? x '(a b c))
                 (equal? y '(d e f))
                 (equal? z '(a b c d e f)))))
    (let ((x '(a b c)))
       (equal? (append '(a b c) '()) '(a b c)))
    (let ((x '(d e f)))
       (eq? (list-tail (append '(a b c) x) 3) x))
    (equal? (append '(a b) '(c d) '(e f)) '(a b c d e f))
    (error? (append cyclic-list '()))
    (error? (append (cdr cyclic-list) '()))
    (error? (append '(a b . c) '()))
    (error? (append `(c d . ,cyclic-list) '()))
    (error? (append '(a b) cyclic-list '()))
    (error? (append '(a b) (cdr cyclic-list) '()))
    (error? (append '(a b) `(c d . ,cyclic-list) '()))
    (error? (append '(a b) '(a b . c) '()))
    (error? (append '(1) '(a b) cyclic-list '()))
    (error? (append '(1) '(a b) (cdr cyclic-list) '()))
    (error? (append '() '(a b) `(c d . ,cyclic-list) '()))
    (error? (append '(1) '(a b) '(a b . c) '()))
    (not (append #f))
    (eq? (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2]) (expand/optimize '(not (append #f)))) #t)
 )

(mat append!
    (null? (append!))
    (equal? (append! (list 'a 'b 'c)) '(a b c))
    (let ((x (list 'a 'b 'c)) (y (list 'd 'e 'f)))
        (and (eq? x (append! x y)) (eq? y (list-tail x 3))))
    (equal? (append! (list 'a 'b 'c) '()) '(a b c))
    (equal? (append! '() '(a b c)) '(a b c))
    (equal? (append! (list 'a 'b) (list 'c 'd) '(e f)) '(a b c d e f))
    (error? (append! cyclic-list '()))
    (error? (append! (cdr (make-cyclic-list)) '()))
    (error? (append! (cons* 'c 'd (make-cyclic-list)) '()))
    (error? (append! (cons* 'a 'b 'c) '()))
    (error? (append! (list 'a 'b) (make-cyclic-list) '()))
    (error? (append! (list 'a 'b) (cdr (make-cyclic-list)) '()))
    (error? (append! (list 'a 'b) (cons* 'c 'd (make-cyclic-list)) '()))
    (error? (append! (list 'a 'b) (cons* 'a 'b 'c) '()))
    (error? (append! (list 1) (list 'a 'b) (make-cyclic-list) '()))
    (error? (append! (list 1) (list 'a 'b) (cdr (make-cyclic-list)) '()))
    (error? (append! (list 1) (list 'a 'b) (cons* 'c 'd (make-cyclic-list)) '()))
    (error? (append! (list 1) (list 'a 'b) (cons* 'a 'b 'c) '()))
    (not (append! #f))
    (eq? (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2]) (expand/optimize '(not (append! #f)))) #t)
 )

(mat reverse
    (let* ((x '(a b c d)) (y (reverse x)))
        (and (equal? x '(a b c d)) (equal? y '(d c b a))))
    (eq? (reverse '()) '())
    (equal? (reverse '(a)) '(a))
    (error? (reverse))
    (error? (reverse 'a))
    (error? (reverse '(a b . c)))
    (error? (reverse cyclic-list))
    (error? (reverse (cdr cyclic-list)))
    (error? (reverse `(a b . ,cyclic-list)))
 )

(mat reverse!
    (let* ((x (list 'a 'b 'c 'd)) (y (reverse! x)))
        (and (not (equal? x '(a b c d))) (equal? y '(d c b a))))
    (let ([memq? (lambda (x ls) (and (memq x ls) #t))])
      (let* ((p1 (list 'a 'b 'c)) (p2 (cdr p1)) (p3 (cdr p2)))
          (let* ((r1 (reverse! p1)) (r2 (cdr r1)) (r3 (cdr r2)))
              (let ((p-pairs (list p1 p2 p3)))
                  (and (memq? r1 p-pairs) (memq? r2 p-pairs) (memq? r3 p-pairs))))))
    (eq? (reverse! '()) '())
    (let ((x '(a))) (eq? (reverse! x) x))
    (error? (reverse!))
    (error? (reverse! 'a))
    (error? (reverse! (cons* 'a 'b 'c)))
    (error? (reverse! (make-cyclic-list)))
    (error? (reverse! (cdr (make-cyclic-list))))
    (error? (reverse! (cons* 'a 'b (make-cyclic-list))))
 )

(mat memp
  (not (memp (lambda (x) #t) '()))
  (let ([x '(a b c)])
    (and (equal? (memp (lambda (x) (eq? x 'a)) x) x)
         (equal? (memp (lambda (x) (eq? x 'b)) x) (cdr x))
         (equal? (memp (lambda (x) (eq? x 'c)) x) (cddr x))
         (not (memp (lambda (x) (eq? x 'd)) x))))
  (let ([x '(1 -2 3)])
    (and (equal? (memp negative? x) (cdr x))
         (equal? (memp positive? x) x)
         (not (memp pair? x))))
  (equal? (memp (lambda (x) (eq? x 'a)) (cdr cyclic-list)) cyclic-list)
  (error? ; cyclic list
    (memp (lambda (x) #f) cyclic-list))
  (error? ; improper list
    (memp (lambda (x) #f) '(a b . c)))
  (error? ; not a procedure
    (memp 'a '(a b c)))
 )

(mat find
  (not (find (lambda (x) #t) '()))
  (let ([x '(a b c)])
    (and (equal? (find (lambda (x) (eq? x 'a)) x) 'a)
         (equal? (find (lambda (x) (eq? x 'b)) x) 'b)
         (equal? (find (lambda (x) (eq? x 'c)) x) 'c)
         (not (find (lambda (x) (eq? x 'd)) x))))
  (let ([x '(1 -2 3)])
    (and (equal? (find negative? x) -2)
         (equal? (find positive? x) 1)
         (not (find pair? x))))
  (equal? (find (lambda (x) (eq? x 'a)) (cdr cyclic-list)) 'a)
  (error? ; cyclic list
    (find (lambda (x) #f) cyclic-list))
  (error? ; improper list
    (find (lambda (x) #f) '(a b . c)))
  (error? ; improper list
    (find (lambda (x) #f) '(a b c . d)))
  (error? ; not a procedure
    (find 'a '(a b c)))
 )

(mat memq
    (eq? (memq 'a '()) #f)
    (let ((x '(a b c c b a)))
        (and (eq? (memq 'a x) x)
             (eq? (memq 'b x) (cdr x))
             (eq? (memq 'c x) (cddr x))
             (eq? (memq 'd x) #f)))
    (let ((x '(1 1/2 .5 (a . b))))
        (and (eq? (memq 1 x) x)
             (eq? (memq 1/2 x) #f)
             (eq? (memq .5 x) #f)
             (eq? (memq (cons 'a 'b) x) #f)
             (eq? (memq .7 x) #f)))
    (let* ((x (list 'a)) (y (list '(a) x 'b x)))
        (eq? (memq x y) (cdr y)))
    (let ((x (list (string #\h #\i) (string #\i #\h))))
        (and (eq? (memq "hi" x) #f) (eq? (memq "ih" x) #f)))
    (let ((x (list (list 'a) (list 'b) (list 'c) (list 'a))))
        (and (eq? (memq '(a) x) #f)
             (eq? (memq '(b) x) #f)
             (eq? (memq '(c) x) #f)
             (eq? (memq '(d) x) #f)))
    (eq? (memq 'a cyclic-list) cyclic-list)
    (eq? (memq 'b cyclic-list) (cdr cyclic-list))
    (let ([x `(c d . ,cyclic-list)])
      (eq? (memq 'd x) (cdr x)))
    (let ([x '(a b . c)])
      (and (eq? (memq 'a x) x) (eq? (memq 'b x) (cdr x))))
    (error? (memq))
    (error? (memq 'c))
    (error? (memq 'c 'a))
    (error? (memq 'c cyclic-list))
    (error? (memq 'c '(a b . c)))
 )

(mat memv
    (eq? (memv 'a '()) #f)
    (let ((x '(a b c c b a)))
        (and (eq? (memv 'a x) x)
             (eq? (memv 'b x) (cdr x))
             (eq? (memv 'c x) (cddr x))
             (eq? (memv 'd x) #f)))
    (let ((x '(1 1/2 .5 12314122441)))
        (and (eq? (memv 1 x) x)
             (eq? (memv 1/2 x) (cdr x))
             (eq? (memv .5 x) (cddr x))
             (eq? (memv 12314122441 x) (cdddr x))
             (eq? (memv .7 x) #f)))
    (let* ((x (list 'a)) (y (list '(a) x 'b x)))
        (eq? (memv x y) (cdr y)))
    (let ((x (list (string #\h #\i) (string #\i #\h))))
        (and (eq? (memv "hi" x) #f) (eq? (memv "ih" x) #f)))
    (let ((x (list (list 'a) (list 'b) (list 'c) (list 'a))))
        (and (eq? (memv '(a) x) #f)
             (eq? (memv '(b) x) #f)
             (eq? (memv '(c) x) #f)
             (eq? (memv '(d) x) #f)))
    (eq? (memv 'a cyclic-list) cyclic-list)
    (eq? (memv 'b cyclic-list) (cdr cyclic-list))
    (let ([x `(c d . ,cyclic-list)])
      (eq? (memv 'd x) (cdr x)))
    (let ([x '(a b . c)])
      (and (eq? (memv 'a x) x) (eq? (member 'b x) (cdr x))))
    (error? (memv))
    (error? (memv 'c))
    (error? (memv 'c 'a))
    (error? (memv 'c cyclic-list))
    (error? (memv 'c '(a b . c)))
    (eq?
      (memv 2 '#0=(1 2 3 . #0#))
      (cdr '#0#))
 )

(mat member
    (eq? (member 'a '()) #f)
    (let ((x '(a b c c b a)))
        (and (eq? (member 'a x) x)
             (eq? (member 'b x) (cdr x))
             (eq? (member 'c x) (cddr x))
             (eq? (member 'd x) #f)))
    (let ((x '(1 1/2 .5 12314122441)))
        (and (eq? (member 1 x) x)
             (eq? (member 1/2 x) (cdr x))
             (eq? (member .5 x) (cddr x))
             (eq? (member 12314122441 x) (cdddr x))
             (eq? (member .7 x) #f)))
    (let* ((x (list 'a)) (y (list '(a) x 'b x)))
        (eq? (member x y) y))
    (let ((x (list 'hi (string #\h #\i) (string #\i #\h))))
        (and (eq? (member "hi" x) (cdr x))
             (eq? (member "ih" x) (cddr x))))
    (let ((x '("hi" "ih" "hi")))
        (and (eq? (member "hi" x) x) (eq? (member "ih" x) (cdr x))))
    (let ((x (list (list 'a) (list 'b) (list 'c) (list 'a))))
        (and (eq? (member '(a) x) x)
             (eq? (member '(b) x) (cdr x))
             (eq? (member '(c) x) (cddr x))
             (eq? (member '(d) x) #f)))
    (eq? (member 'a cyclic-list) cyclic-list)
    (eq? (member 'b cyclic-list) (cdr cyclic-list))
    (let ([x `(c d . ,cyclic-list)])
      (eq? (member 'd x) (cdr x)))
    (let ([x '(a b . c)])
      (and (eq? (member 'a x) x) (eq? (member 'b x) (cdr x))))
    (error? (member))
    (error? (member 'c))
    (error? (member 'c 'a))
    (error? (member 'c cyclic-list))
    (error? (member 'c '(a b . c)))
 )

(mat partition
  (equal?
    (let-values ([x (partition negative? '())]) x)
    '(() ()))
  (let ((x (list -1 2 -3 -3 1 -5 2 6)))
    (define-syntax valequal?
      (syntax-rules ()
        [(_ e v ...)
         (let-values ([ls e]) (equal? ls (list v ...)))]))
    (and (valequal? (partition pair? x) '() '(-1 2 -3 -3 1 -5 2 6))
         (valequal? (partition negative? x) '(-1 -3 -3 -5) '(2 1 2 6))
         (equal? x '(-1 2 -3 -3 1 -5 2 6))))
  (error? ; improper list
    (partition values cyclic-list))
  (error? ; improper list
    (partition values (cons 'x cyclic-list)))
  (error? ; improper list
    (partition values 'q))
  (error? ; not a procedure
    (partition 'q '()))
  (eqv?
    (let loop ([n 100])
      (when (>= n 0)
        (let ([ls (map (lambda (x) (random 20)) (make-list n))])
          (let-values ([(odds evens) (partition odd? ls)])
            (let-values ([(evens1 odds1) (partition even? ls)])
              (unless (and (equal? odds1 odds) (equal? evens1 evens))
                (printf "partition error 1: ~s\n" ls)
                (errorf #f "partition test 1 failed")))
            (let ([odds2 (filter odd? ls)] [evens2 (remp odd? ls)])
              (unless (and (equal? odds2 odds) (equal? evens2 evens))
                (printf "partition error 2: ~s\n" ls)
                (errorf #f "partition test 2 failed")))
            (let ([odds3 (remp even? ls)] [evens3 (filter even? ls)])
              (unless (and (equal? odds3 odds) (equal? evens3 evens))
                (printf "partition error 3: ~s\n" ls)
                (errorf #f "partition test 3 failed")))
            (let ([odds4 (fold-right (lambda (x ls) (if (odd? x) (cons x ls) ls)) '() ls)]
                  [evens4 (fold-right (lambda (x ls) (if (odd? x) ls (cons x ls))) '() ls)])
              (unless (and (equal? odds4 odds) (equal? evens4 evens))
                (printf "partition error 4: ~s\n" ls)
                (errorf #f "partition test 4 failed")))
            (let ([odds5 (reverse (fold-left (lambda (ls x) (if (odd? x) (cons x ls) ls)) '() ls))]
                  [evens5 (reverse (fold-left (lambda (ls x) (if (odd? x) ls (cons x ls))) '() ls))])
              (unless (and (equal? odds5 odds) (equal? evens5 evens))
                (printf "partition error 5: ~s\n" ls)
                (errorf #f "partition test 5 failed")))))
        (loop (- n 1))))
    (void))
)

(mat filter
  (equal?
    (let ([x 3])
      (let ([y (filter (begin (set! x 55) (lambda (x) #f)) '())])
        (list x y)))
    '(55 ()))
  (equal?
    (let ([x 3])
      (let ([y (filter (begin (set! x (+ x 35)) (lambda (x) #f))
                       (begin (set! x (+ x 7)) '()))])
        (list x y)))
    '(45 ()))
  (eq? (filter negative? '()) '())
  (let ((x (list -1 2 -3 -3 1 -5 2 6)))
    (and (equal? (filter pair? x) '())
         (equal? (filter negative? x) '(-1 -3 -3 -5))
         (equal? x '(-1 2 -3 -3 1 -5 2 6))))
  (error? ; improper list
    (filter values cyclic-list))
  (error? ; improper list
    (filter values (cons 'x cyclic-list)))
  (error? ; improper list
    (filter values 'q))
  (error? ; not a procedure
    (filter 'q '()))
)

(mat remp
  (eqv? (remp (lambda (x) #t) '()) '())
  (equal?
    (let ([x 3])
      (let ([y (remp (begin (set! x 55) (lambda (x) #t)) '())])
        (list x y)))
    '(55 ()))
  (equal?
    (let ([x 3])
      (let ([y (remp (begin (set! x (+ x 35)) (lambda (x) #t))
                     (begin (set! x (+ x 7)) '()))])
        (list x y)))
    '(45 ()))
  (let ([x (list 1 -2 3)])
    (and (equal? (remp negative? x) '(1 3))
         (equal? x '(1 -2 3))))
  (let ([x (list 1 -2 3)])
    (and (equal? (remp positive? x) '(-2))
         (equal? x '(1 -2 3))))
  (error? ; improper list
    (remp values cyclic-list))
  (error? ; improper list
    (remp values (cons 'x cyclic-list)))
  (error? ; improper list
    (remp values 'q))
  (error? ; not a procedure
    (remp 'q '()))
 )

(mat remq
    (eq? (remq 'a '()) '())
    (eq? (remq 'a '(a)) '())
    (equal? (remq 'a '(b c)) '(b c))
    (let* ((x (string #\a #\b)) (l (list 'a x "ab" 'c x "ab")))
        (equal? (remq x l) '(a "ab" c "ab")))
    (let* ((x (list 3)) (l (list 'a 'b x 'c '(3) 'd x 'e x)))
        (equal? (remq x l) '(a b c (3) d e)))
    (error? (remq "c" cyclic-list))
    (error? (let ((s "c")) (remq "c" (cons s cyclic-list))))
    (error? (remq "c" 'a))
 )

(mat remq!
    (eq? (remq! 'a '()) '())
    (eq? (remq! 'a '(a)) '())
    (equal? (remq! 'a '(b c)) '(b c))
    (let* ((x (string #\a #\b)) (l (list 'a x "ab" 'c x "ab")))
        (equal? (remq! x l) '(a "ab" c "ab")))
    (let* ((x (list 3)) (l (list 'a 'b x 'c '(3) 'd x 'e x)))
        (equal? (remq! x l) '(a b c (3) d e)))
    (let* ((p1 (list 'a 'b 'c)) (p3 (cddr p1)))
        (let* ((r1 (remq! 'b p1)) (r2 (cdr r1)))
            (and (eq? p1 r1) (eq? r2 p3) (equal? p1 '(a c)))))
    (error? (remq! "c" 'a))
    (error? (remq! "c" (make-cyclic-list)))
 )

(mat remv
    (eq? (remv 'a '()) '())
    (eq? (remv 'a '(a)) '())
    (equal? (remv 'a '(b c)) '(b c))
    (let* ((x (string #\a #\b)) (l (list 'a x "ab" 'c x "ab")))
        (equal? (remv x l) '(a "ab" c "ab")))
    (let* ((x (list 3)) (l (list 'a 'b x 'c '(3) 'd x 'e x)))
        (equal? (remv x l) '(a b c (3) d e)))
    (error? (remv "c" cyclic-list))
    (error? (let ((s "c")) (remv "c" (cons s cyclic-list))))
    (error? (remv "c" 'a))
 )

(mat remv!
    (eq? (remv! 'a '()) '())
    (eq? (remv! 'a '(a)) '())
    (equal? (remv! 'a '(b c)) '(b c))
    (let* ((x (string #\a #\b)) (l (list 'a x "ab" 'c x "ab")))
        (equal? (remv! x l) '(a "ab" c "ab")))
    (let* ((x (list 3)) (l (list 'a 'b x 'c '(3) 'd x 'e x)))
        (equal? (remv! x l) '(a b c (3) d e)))
    (let* ((p1 (list 'a 'b 'c)) (p3 (cddr p1)))
        (let* ((r1 (remv! 'b p1)) (r2 (cdr r1)))
            (and (eq? r1 p1) (eq? r2 p3) (equal? p1 '(a c)))))
    (error? (remv! "c" (make-cyclic-list)))
    (error? (remv! "c" 'a))
 )

(mat remove
    (eq? (remove 'a '()) '())
    (eq? (remove 'a '(a)) '())
    (equal? (remove 'a '(b c)) '(b c))
    (let* ((x (string #\a #\b)) (l (list 'a x "ab" 'c x "ab")))
        (equal? (remove x l) '(a c)))
    (let* ((x (list 3)) (l (list 'a 'b x 'c '(3) 'd x 'e x)))
        (equal? (remove x l) '(a b c d e)))
    (error? (remove "c" cyclic-list))
    (error? (let ((s "c")) (remove "c" (cons s cyclic-list))))
    (error? (remove "c" 'a))
 )

(mat remove!
    (eq? (remove! 'a '()) '())
    (eq? (remove! 'a '(a)) '())
    (equal? (remove! 'a '(b c)) '(b c))
    (let* ((x (string #\a #\b)) (l (list 'a x "ab" 'c x "ab")))
        (equal? (remove! x l) '(a c)))
    (let* ((x (list 3)) (l (list 'a 'b x 'c '(3) 'd x 'e x)))
        (equal? (remove! x l) '(a b c d e)))
    (let* ((p1 (list 'a 'b 'c)) (p3 (cddr p1)))
        (let* ((r1 (remove! 'b p1)) (r2 (cdr r1)))
            (and (eq? r1 p1) (eq? r2 p3) (equal? p1 '(a c)))))
    (error? (remove! "c" (make-cyclic-list)))
 )

(mat substq
    (equal? (substq 1 'b '((b c) b a)) '((1 c) 1 a))
    (equal? (substq 1 'd '((b c) b a)) '((b c) b a))
    (equal? (substq 1/3 1/2 '((1/2 c) 1/2 a)) '((1/2 c) 1/2 a))
    (let* ([x (cons 'a 'b)] [y (cons (cons 'a 'b) x)])
       (equal? (substq 'boo x y) '((a . b) . boo)))
    (let ((x '((b c) b a)))
        (eq? x (substq 1 'd x)))
 )

(mat substq!
    (equal? (substq! 1 'b '((b c) b a)) '((1 c) 1 a))
    (equal? (substq! 1 'd '((b c) b a)) '((b c) b a))
    (equal? (substq! 1/3 1/2 '((1/2 c) 1/2 a)) '((1/2 c) 1/2 a))
    (let* ([x (cons 'a 'b)] [y (cons (cons 'a 'b) x)])
       (equal? (substq! 'boo x y) '((a . b) . boo)))
    (let ((x '((b c) b a)))
        (eq? x (substq! 1 'd x)))
    (let ((x '((b c) b a)))
        (eq? x (substq! 1 'b x)))
 )

(mat substv
    (equal? (substv 1 'b '((b c) b a)) '((1 c) 1 a))
    (equal? (substv 1 'd '((b c) b a)) '((b c) b a))
    (equal? (substv 1/3 1/2 '((1/2 c) 1/2 a)) '((1/3 c) 1/3 a))
    (let* ([x (cons 'a 'b)] [y (cons (cons 'a 'b) x)])
       (equal? (substv 'boo x y) '((a . b) . boo)))
    (let ((x '((b c) b a)))
       (eq? x (substv 1 'd x)))
 )

(mat substv!
    (equal? (substv! 1 'b '((b c) b a)) '((1 c) 1 a))
    (equal? (substv! 1 'd '((b c) b a)) '((b c) b a))
    (equal? (substv! 1/3 1/2 '((1/2 c) 1/2 a)) '((1/3 c) 1/3 a))
    (let* ([x (cons 'a 'b)] [y (cons (cons 'a 'b) x)])
       (equal? (substv! 'boo x y) '((a . b) . boo)))
    (let ((x '((b c) b a)))
        (eq? x (substv! 1 'd x)))
    (let ((x '((b c) b a)))
        (eq? x (substv! 1 'b x)))
 )

(mat subst
    (equal? (subst 1 'b '((b c) b a)) '((1 c) 1 a))
    (equal? (subst 1 'd '((b c) b a)) '((b c) b a))
    (equal? (subst 1/3 1/2 '((1/2 c) 1/2 a)) '((1/3 c) 1/3 a))
    (let* ([x (cons 'a 'b)] [y (cons (cons 'a 'b) x)])
       (equal? (subst 'boo x y) '(boo . boo)))
    (let ((x '((b c) b a)))
        (eq? x (subst 1 'd x)))
 )

(mat subst!
    (equal? (subst! 1 'b '((b c) b a)) '((1 c) 1 a))
    (equal? (subst! 1 'd '((b c) b a)) '((b c) b a))
    (equal? (subst! 1/3 1/2 '((1/2 c) 1/2 a)) '((1/3 c) 1/3 a))
    (let* ([x (cons 'a 'b)] [y (cons (cons 'a 'b) x)])
       (equal? (subst! 'boo x y) '(boo . boo)))
    (let ((x '((b c) b a)))
        (eq? x (subst! 1 'd x)))
    (let ((x '((b c) b a)))
        (eq? x (subst! 1 'b x)))
 )

(mat assp
  (not (assp (lambda (x) #t) '()))
  (let ([a (list -1)] [b (list 2)] [c (list 3)])
    (let ([l (list a b c)])
      (and (equal? (assp negative? l) a)
           (equal? (assp positive? l) b)
           (equal? (assp (lambda (x) (= x 3)) l) c)
           (not (assp pair? l)))))
  (eq? (cdr (assp (lambda (x) (eq? x 'a)) cyclic-alist)) 1)
  (eq? (cdr (assp (lambda (x) (eq? x 'a)) (cdr cyclic-alist))) 1)
  (eq? (cdr (assp (lambda (x) (eqv? x 3.2)) cyclic-alist)) 3)
  (eq? (cdr (assp (lambda (x) (equal? x "a")) cyclic-alist)) 4)
  (error? ; cyclic alist
    (assp (lambda (x) #f) cyclic-alist))
  (error? ; improper alist
    (assp (lambda (x) #f) '((a . 1) . c)))
  (error? ; improper alist
    (assp (lambda (x) #f) 17))
  (error? ; not a procedure
    (assp 'a '((a . 1) (b . 2))))
 )

(mat assq
    (eq? (assq 'a '()) #f)
    (let ((a (list 'a)) (b (list 'b)) (c (list 'c)))
        (let ((l (list a b '(c) c b a)))
            (and (eq? (assq 'a l) a)
                 (eq? (assq 'b l) b)
                 (not (eq? (assq 'c l) c))
                 (eq? (assq 'd l) #f))))
    (let* ((x (list 'a)) (l `(((a)) ((b)) (,x) ((c)))))
        (eq? (assq x l) (caddr l)))
    (eq? (cdr (assq 'a cyclic-alist)) 1)
    (error? (assq 3.2 cyclic-alist))
    (error? (assq "a" cyclic-alist))
    (error? (assq 'c cyclic-alist))
    (error? (assq "s" cyclic-alist))
 )

(mat assv
    (eq? (assv 'a '()) #f)
    (let ((a (list 'a)) (b (list 'b)) (c (list 'c)))
        (let ((l (list a b '(c) c b a)))
            (and (eq? (assv 'a l) a)
                 (eq? (assv 'b l) b)
                 (not (eq? (assv 'c l) c))
                 (eq? (assv 'd l) #f))))
    (let* ((x (list 'a)) (l `(((a)) ((b)) (,x) ((c)))))
        (eq? (assv x l) (caddr l)))
    (eq? (cdr (assv 'a cyclic-alist)) 1)
    (eq? (cdr (assv 3.2 cyclic-alist)) 3)
    (error? (assv "a" cyclic-alist))
    (error? (assv 1/2 cyclic-alist))
    (error? (assv "s" cyclic-alist))
 )

(mat assoc
    (eq? (assoc 'a '()) #f)
    (let ((a (list 'a)) (b (list 'b)) (c (list 'c)))
        (let ((l (list a b '(c) c b a)))
            (and (eq? (assoc 'a l) a)
                 (eq? (assoc 'b l) b)
                 (not (eq? (assoc 'c l) c))
                 (eq? (assoc 'd l) #f))))
    (let* ((x (list 'a)) (l `(((a)) ((b)) (,x) ((c)))))
        (eq? (assoc x l) (car l)))
    (eq? (cdr (assoc 'a cyclic-alist)) 1)
    (eq? (cdr (assoc 3.2 cyclic-alist)) 3)
    (eq? (cdr (assoc "a" cyclic-alist)) 4)
    (error? (assoc 1/2 cyclic-alist))
    (error? (assoc "s" cyclic-alist))
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

(mat sort
  (error? ; invalid number of arguments
    (sort))
  (error? ; invalid number of arguments
    (sort >))
  (error? ; invalid number of arguments
    (sort '(a b c)))
  (error? ; invalid number of arguments
    (sort > '(1 2 3) #t))
  (error? ; 3 is not a proper list
    (sort > 3))
  (error? ; #(1 2 3) is not a proper list
    (sort > '#(1 2 3)))
  (error? ; (1 2 . 3) is not a proper list
    (sort > '(1 2 . 3)))
  (error? ; cyclic list
    (sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
          cyclic-list))
  (error? ; cyclic list
    (sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
          (cdr cyclic-list)))
  (error? ; cyclic list
    (sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
          `(q p . ,cyclic-list)))
  (error? ; (a b c) is not a procedure
    (sort '(a b c) '(a b c)))
  (error? ; b is not a real number
    (sort > '(1 b 3)))
  (equal? (sort > '()) '())
  (let ([v (list 3 2 1)])
    (and
      (equal? (sort > v) '(3 2 1))
      (equal? v '(3 2 1))))
  (let ([v (list 1 2 3)])
    (and
      (equal? (sort > v) '(3 2 1))
      (equal? v '(1 2 3))))
  (let ([v (list 2 3 1)])
    (and
      (equal? (sort > v) '(3 2 1))
      (equal? v '(2 3 1))))
  (let ([v (list -2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5)])
    (and
      (equal?
        (sort < v)
        '(-43 -11 -7 -5 -2 0 3.0 3 5 8 8.0 9 9))
      (equal? v '(-2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5))))
  (let ([v (list 2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9)])
    (and
      (equal?
        (sort (lambda (x y) (< (abs x) (abs y))) v)
        '(1 -1 2 2 -3 3 4 5 -5 8 8 -8 9 -9 10))
      (equal? v '(2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9))))
  (let ([v (list 1 3 2 4)])
    (and
      (equal? (sort < v) '(1 2 3 4))
      (equal? v '(1 3 2 4))))
  (equal?
    (with-output-to-string
      (lambda ()
        (do ([n 1000 (fx- n 5)])
            ((fx= n 0))
          (write-char #\.)
          (flush-output-port)
          (do ([k 25 (fx- k 1)])
              ((fx= k 0))
            (let* ([ls (map (lambda (x) (random k)) (make-list n))]
                   [copy (map values ls)])
              (unless (and
                        (equal? (sort < ls) ($merge-sort < copy))
                        (equal? ls copy))
                (fprintf (console-output-port) "\n~s\n" ls)
                (errorf #f "failed")))))))
    (make-string 200 #\.))
)

(mat list-sort
  (error? ; invalid number of arguments
    (list-sort))
  (error? ; invalid number of arguments
    (list-sort >))
  (error? ; invalid number of arguments
    (list-sort '(a b c)))
  (error? ; invalid number of arguments
    (list-sort > '(1 2 3) #t))
  (error? ; 3 is not a proper list
    (list-sort > 3))
  (error? ; #(1 2 3) is not a proper list
    (list-sort > '#(1 2 3)))
  (error? ; (1 2 . 3) is not a proper list
    (list-sort > '(1 2 . 3)))
  (error? ; cyclic list
    (list-sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
          cyclic-list))
  (error? ; cyclic list
    (list-sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
          (cdr cyclic-list)))
  (error? ; cyclic list
    (list-sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
          `(q p . ,cyclic-list)))
  (error? ; (a b c) is not a procedure
    (list-sort '(a b c) '(a b c)))
  (error? ; b is not a real number
    (list-sort > '(1 b 3)))
  (equal? (list-sort > '()) '())
  (let ([v (list 3 2 1)])
    (and
      (equal? (list-sort > v) '(3 2 1))
      (equal? v '(3 2 1))))
  (let ([v (list 1 2 3)])
    (and
      (equal? (list-sort > v) '(3 2 1))
      (equal? v '(1 2 3))))
  (let ([v (list 2 3 1)])
    (and
      (equal? (list-sort > v) '(3 2 1))
      (equal? v '(2 3 1))))
  (let ([v (list -2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5)])
    (and
      (equal?
        (list-sort < v)
        '(-43 -11 -7 -5 -2 0 3.0 3 5 8 8.0 9 9))
      (equal? v '(-2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5))))
  (let ([v (list 2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9)])
    (and
      (equal?
        (list-sort (lambda (x y) (< (abs x) (abs y))) v)
        '(1 -1 2 2 -3 3 4 5 -5 8 8 -8 9 -9 10))
      (equal? v '(2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9))))
  (let ([v (list 1 3 2 4)])
    (and
      (equal? (list-sort < v) '(1 2 3 4))
      (equal? v '(1 3 2 4))))
  (equal?
    (with-output-to-string
      (lambda ()
        (do ([n 1000 (fx- n 5)])
            ((fx= n 0))
          (write-char #\.)
          (flush-output-port)
          (do ([k 25 (fx- k 1)])
              ((fx= k 0))
            (let* ([ls (map (lambda (x) (random k)) (make-list n))]
                   [copy (map values ls)])
              (unless (and
                        (equal? (list-sort < ls) ($merge-sort < copy))
                        (equal? ls copy))
                (fprintf (console-output-port) "\n~s\n" ls)
                (errorf #f "failed")))))))
    (make-string 200 #\.))
)

(mat sort!
  (error? ; invalid number of arguments
    (sort!))
  (error? ; invalid number of arguments
    (sort! >))
  (error? ; invalid number of arguments
    (sort! '(a b c)))
  (error? ; invalid number of arguments
    (sort! > '(1 2 3) #t))
  (error? ; 3 is not a proper list
    (sort! > 3))
  (error? ; #(1 2 3) is not a proper list
    (sort! > '#(1 2 3)))
  (error? ; (1 2 . 3) is not a proper list
    (sort! > (cons* 1 2 3)))
  (error? ; cyclic list
    (sort! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (make-cyclic-list)))
  (error? ; cyclic list
    (sort! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (cdr (make-cyclic-list))))
  (error? ; cyclic list
    (sort! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           `(q p . ,(make-cyclic-list))))
  (error? ; (a b c) is not a procedure
    (sort! '(a b c) '(a b c)))
  (error? ; b is not a real number
    (sort! > '(1 b 3)))
  (equal? (sort! > '()) '())
  (let ([v (list 3 2 1)])
    (equal? (sort! > v) '(3 2 1)))
  (let ([v (list 1 2 3)])
    (equal? (sort! > v) '(3 2 1)))
  (let ([v (list 2 3 1)])
    (equal? (sort! > v) '(3 2 1)))
  (let ([v (list -2 3.0 9 8 3 -11 0 9 -5 -7 -43 8.0 5)])
    (equal?
      (sort! < v)
      '(-43 -11 -7 -5 -2 0 3.0 3 5 8 8.0 9 9)))
  (let ([v (list 2 5 8 -3 9 10 -5 8 -8 4 2 3 1 -1 -9)])
    (equal?
      (sort! (lambda (x y) (< (abs x) (abs y))) v)
      '(1 -1 2 2 -3 3 4 5 -5 8 8 -8 9 -9 10)))
  (let ([v (list 1 3 2 4)])
    (equal? (sort! < v) '(1 2 3 4)))
  (equal?
    (with-output-to-string
      (lambda ()
        (do ([n 1000 (fx- n 5)])
            ((fx= n 0))
          (write-char #\.)
          (flush-output-port)
          (do ([k 25 (fx- k 1)])
              ((fx= k 0))
            (let* ([ls (map (lambda (x) (random k)) (make-list n))]
                   [copy (map values ls)])
              (unless (equal? (sort! < ls) ($merge-sort < copy))
                (fprintf (console-output-port) "\n~s\n" copy)
                (errorf #f "failed")))))))
    (make-string 200 #\.))
)

(mat merge
   (equal? (merge < '() '()) '())
   (equal? (merge < '(1) '()) '(1))
   (equal? (merge < '() '(2)) '(2))
   (equal? (merge < '(1) '(2)) '(1 2))
   (equal? (merge < '(2) '(1)) '(1 2))
   (equal? (merge < '(1 3 5 7 9) '(2 4 6 8 10)) '(1 2 3 4 5 6 7 8 9 10))
   (equal? (merge < '(1 2 5 7 8) '(3 4 6 9 10)) '(1 2 3 4 5 6 7 8 9 10))
   (equal?
       (merge (lambda (x y) (< (abs x) (abs y)))
           '(-1 1 4 -4)
           '(1 -3 3 8 9 -9))
       '(-1 1 1 -3 3 4 -4 8 9 -9))
   (let ((l1 (list 1 3 5 7 9)) (l2 (list 2 4 6 8 10)))
       (and (equal? (merge < l1 l2) '(1 2 3 4 5 6 7 8 9 10))
           (equal? l1 '(1 3 5 7 9))
           (equal? l2 '(2 4 6 8 10))))
  (error? ; (1 2 . 3) is not a proper list
    (merge > '(5 -1 2) '(1 2 . 3)))
  (error? ; (1 2 . 3) is not a proper list
    (merge > '(1 2 . 3) '(5 -1 2)))
  (error? ; cyclic list
    (merge (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           '(p b q) cyclic-list))
  (error? ; cyclic list
    (merge (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           cyclic-list '(p b q)))
  (error? ; cyclic list
    (merge (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           '(p b c) (cdr cyclic-list)))
  (error? ; cyclic list
    (merge (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (cdr cyclic-list) '(p b c)))
  (error? ; cyclic list
    (merge (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           '(p b c) `(q p . ,cyclic-list)))
  (error? ; cyclic list
    (merge (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           `(q p . ,cyclic-list) '(p b c)))
 )

(mat merge!
   (equal? (merge! < '() '()) '())
   (equal? (merge! < '(1) '()) '(1))
   (equal? (merge! < '() '(2)) '(2))
   (equal? (merge! < '(1) '(2)) '(1 2))
   (equal? (merge! < '(2) '(1)) '(1 2))
   (equal? (merge! < '(1 3 5 7 9) '(2 4 6 8 10)) '(1 2 3 4 5 6 7 8 9 10))
   (equal? (merge! < '(1 2 5 7 8) '(3 4 6 9 10)) '(1 2 3 4 5 6 7 8 9 10))
   (equal?
       (merge! (lambda (x y) (< (abs x) (abs y)))
           '(-1 1 4 -4)
           '(1 -3 3 8 9 -9))
       '(-1 1 1 -3 3 4 -4 8 9 -9))
  (error? ; (1 2 . 3) is not a proper list
    (merge! > (list 5 -1 2) (cons* 1 2 3)))
  (error? ; (1 2 . 3) is not a proper list
    (merge! > (cons* 1 2 3) (list 5 -1 2)))
  (error? ; cyclic list
    (merge! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (list 'p 'b 'q) (make-cyclic-list)))
  (error? ; cyclic list
    (merge! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (make-cyclic-list) (list 'p 'b 'q)))
  (error? ; cyclic list
    (merge! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (list 'p 'b 'q) (cdr (make-cyclic-list))))
  (error? ; cyclic list
    (merge! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (cdr (make-cyclic-list)) (list 'p 'b 'q)))
  (error? ; cyclic list
    (merge! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (list 'p 'b 'q) (cons* 'q 'p (make-cyclic-list))))
  (error? ; cyclic list
    (merge! (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
           (cons* 'q 'p (make-cyclic-list)) (list 'p 'b 'q)))
 )

(mat iota
  (error? ; not a nonnegative fixnum
    (iota 'a))
  (error? ; not a nonnegative fixnum
    (iota -1))
  (error? ; not a nonnegative fixnum
    (iota #e1e30))
  (error? ; not a nonnegative fixnum
    (iota 3/4))
  (error? ; wrong number of arguments
    (iota))
  (error? ; wrong number of arguments
    (iota 3 17))
  (equal? (iota 7) '(0 1 2 3 4 5 6))
  (equal? (iota 6) '(0 1 2 3 4 5))
  (equal? (iota 0) '())
  (equal? (iota 1) '(0))
  (equal? (iota 2) '(0 1))
  (equal? (iota 3) '(0 1 2))
  (equal? (iota 4) '(0 1 2 3))
  (let ([ls (iota 100)])
    (and
      (= (length ls) 100)
      (equal? ls (sort < ls))
      (eqv? (car ls) 0)
      (eqv? (apply + ls) 4950)))
)

(mat enumerate
  (error? ; not a proper list
    (enumerate 'a))
  (error? ; not a proper list
    (enumerate '(a . b)))
  (error? ; not a proper list
    (enumerate (let ([ls (list 'a 'b 'c)]) (set-cdr! (cdr ls) ls) ls)))
  (error? ; wrong number of arguments
    (enumerate))
  (error? ; wrong number of arguments
    (enumerate '(a b c) '(d e f)))
  (equal? (enumerate '(a b c d e f g)) '(0 1 2 3 4 5 6))
  (equal? (enumerate '(a b c d e f)) '(0 1 2 3 4 5))
  (equal? (enumerate '()) '())
  (equal? (enumerate '(1)) '(0))
  (equal? (enumerate '(3 2)) '(0 1))
  (equal? (enumerate '(5 4 3)) '(0 1 2))
  (equal? (enumerate '(q p (d o l) l)) '(0 1 2 3))
  (let ([ls (enumerate (make-list 100))])
    (and
      (= (length ls) 100)
      (equal? ls (sort < ls))
      (eqv? (car ls) 0)
      (eqv? (apply + ls) 4950)))
)

(mat list-assuming-immutable?
  (list-assuming-immutable? '(1 2 3))
  (not (list-assuming-immutable? '(1 2 . 3)))
  (not (list-assuming-immutable? #t))
  (not (list-assuming-immutable? 3))
  (list-assuming-immutable? '())

  ;; Check concurrent use of thread bits
  (or (not (threaded?))
      (let ([m (make-mutex)]
            [c (make-condition)]
            [running 4])
        (define (fail msg) (printf "~a\n" msg) (exit))
        (let thread-loop ([n-thread running])
          (unless (zero? n-thread)
            (fork-thread
             (lambda ()
               (let repeat-loop ([n 30] [l '()] [nl 0] [locked '()])
                 (cond
                   [(zero? n)
                    (for-each unlock-object locked)
                    (mutex-acquire m)
                    (set! running (sub1 running))
                    (condition-signal c)
                    (mutex-release m)]
                   [else
                    (mutex-acquire m)
                    (printf "trying ~a\n" n)
                    (mutex-release m)
                    (let ([N 10000])
                      (let loop ([i N] [l l] [nl nl] [locked locked] [bvs '()])
                        (cond
                          [(zero? i)
                           (let ([locked (let ([p (cons 1 2)])
                                           (lock-object p)
                                           (cons p locked))])
                             (collect-rendezvous)
                             (let ([check
                                    (lambda ()
                                      (let loop ([l l])
                                        (when (pair? l)
                                          (unless (list-assuming-immutable? l)
                                            (fail "not a list?!"))
                                          (loop (cdr l))))
                                      (let loop ([nl nl])
                                        (when (pair? nl)
                                          (when (list-assuming-immutable? nl)
                                            (fail "a list?!"))
                                          (loop (cdr nl)))))])
                               (check)
                               (let ([locked (repeat-loop (sub1 n)
                                                          (if (odd? n) l '())
                                                          (if (even? n) nl 0)
                                                          locked)])
                                 (check)
                                 locked)))]
                          [else
                           (let ([l (if (= 0 (modulo i 17))
                                        l
                                        (cons i l))]
                                 [nl (if (= 0 (modulo i 3))
                                         nl
                                         (cons i nl))]
                                 [locked (if #f ; (= i (/ N 2))
                                             (let ([p (cons 1 2)])
                                               (lock-object p)
                                               (when (list-assuming-immutable? p)
                                                 (fail "locked object is a list!?"))
                                               (cons p locked))
                                             locked)])
                             (when (zero? (bitwise-and i (sub1 i)))
                               (let inner-repeat-loop ([j 4])
                                 (unless (zero? j)
                                   (unless (list-assuming-immutable? l)
                                     (fail "not a list?!"))
                                   (when (list-assuming-immutable? nl)
                                     (fail "a list?!"))
                                   (inner-repeat-loop (sub1 j)))))
                             (when (zero? (modulo i 100))
                               (collect-rendezvous))
                             (let ([bv (make-bytevector 12)]) ;; maybe same segment as list bits
                               (bytevector-u8-set! bv 0 255)
                               (bytevector-u8-set! bv 1 255)
                               (bytevector-u8-set! bv 4 255)
                               (loop (sub1 i) l nl locked (cons bv bvs))))])))]))))
            (thread-loop (sub1 n-thread))))
        (mutex-acquire m)
        (let wait ()
          (unless (= 0 running)
            (condition-wait c m)
            (wait)))
        (mutex-release m)
        #t))
  )

(mat tortoise-hare
  (begin
    (define (check-tortoise-hare the-function)
      (cond
        [(eqv? (optimize-level) 3)
         ;; this test is intended to check safe mode, and it can fail on
         ;; operations that have inlined unsafe variants
         #t]
        [else
         ;; check that a function like `list?` doesn't crash if the input is
         ;; mutated to go from a list to non-list
         (let ()
           (define N 100000)
           (define Q 100)
           (define ml (let loop ([i N])
                        (if (zero? i)
                            '()
                            (cons (cons i i) (loop (sub1 i))))))
           (define mid (list-tail ml (floor (* N 1/4))))
           (define mid-rest (cdr mid))
           (boolean?
            (let find-crash ([j 0])
              (define e (make-engine (lambda ()
                                       (guard (x [(who-condition? x)
                                                  #t])
                                              (and (the-function ml)
                                                   #t)))))
              (printf "trying fuel ~s for ~s\n" j the-function)
              (set-cdr! mid mid-rest)
              (e (floor (* (/ j Q) N))
                 (lambda (t v) v)
                 (lambda (e2)
                   (set-cdr! mid 0)
                   (e2 N (lambda (t v) v) (lambda (e2) "still not done"))
                   (or (= j Q)
                       (find-crash (add1 j))))))))]))
    #t)
  (check-tortoise-hare list?)
  (check-tortoise-hare list-assuming-immutable?)
  (check-tortoise-hare last-pair)
  (check-tortoise-hare length)
  (check-tortoise-hare (lambda (l) (list-tail l 100000)))
  (check-tortoise-hare (lambda (l) (assoc "no" l)))
  (check-tortoise-hare (lambda (l) (assv "no" l)))
  (check-tortoise-hare (lambda (l) (assq "no" l)))
  (check-tortoise-hare (lambda (l) (assp string? l)))
  (check-tortoise-hare (lambda (l) (member "no" l)))
  (check-tortoise-hare (lambda (l) (memv "no" l)))
  (check-tortoise-hare (lambda (l) (memq "no" l)))
  (check-tortoise-hare (lambda (l) (memp string? l)))
  (check-tortoise-hare (lambda (l) (find string? l)))
  (check-tortoise-hare (lambda (l) (remove "no" l)))
  (check-tortoise-hare (lambda (l) (remv "no" l)))
  (check-tortoise-hare (lambda (l) (remq "no" l)))
  (check-tortoise-hare (lambda (l) (remp string? l)))
  (check-tortoise-hare (lambda (l) (filter integer? l)))
  (check-tortoise-hare (lambda (l) (partition integer? l) 'ok))
  (check-tortoise-hare generate-temporaries)
  )
