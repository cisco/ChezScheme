;;; 4.ms
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

;;; section 4-1:

(mat apply
   (equal? (apply cons '(1 2)) '(1 . 2))
   (equal? (apply list '(1 2 3 4 5)) '(1 2 3 4 5))
   (equal? (apply (lambda (x . y) (list x y)) '(1 2 3 4 5)) '(1 (2 3 4 5)))
   (equal? (apply list '(1 2 3)) '(1 2 3))
   (equal? (apply list 1 '(2 3)) '(1 2 3))
   (equal? (apply list 1 2 '(3)) '(1 2 3))
   (equal? (apply list 1 2 3 '()) '(1 2 3))
   (error? (apply))
   (error? (apply list))
   (error? (apply list 3))
   (error? (apply list 3 4))
   (error? (apply list 3 4 5 6 7 8 9))
   (error? (apply list 3 '(4 . 5)))
   (error? (apply list 3 4 5 6 7 8 9 '(10 . 11)))
   (error? (apply + '#1=(1 2 . #1#)))
   (equivalent-expansion?
     (parameterize ([run-cp0 (lambda (cp0 x) (cp0 x))]
                    [#%$suppress-primitive-inlining #f]
                    [optimize-level 2])
       (expand/optimize
         `(let ()
            (import scheme)
            (apply + ',(make-list 1000 3)))))
     3000)
 )

;;; section 4-2:

(mat quote
   (equal? '() (cdr '(a)))
   (equal? '(a b c) (list 'a 'b 'c))
   (equal? '#(a b c) (vector 'a 'b 'c))
   (equal? 'a (string->symbol "a")))

(mat quasiquote ; adapted from The Scheme Programming Language
   (equal? `(+ 2 3) '(+ 2 3))
   (equal? `(+ 2 ,(* 3 4)) '(+ 2 12))
   (equal? `(a b (,(+ 2 3) c) d) '(a b (5 c) d))
   (equal? `(a b ,(reverse '(c d e)) f g) '(a b (e d c) f g))
   (equal? `(+ ,@(cdr '(* 2 3))) '(+ 2 3))
   (equal? `(a b ,@(reverse '(c d e)) f g) '(a b e d c f g))
   (equal? '`,(cons 'a 'b) (list 'quasiquote (list 'unquote '(cons 'a 'b))))
   (equal? `',(cons 'a 'b) ''(a . b))
   (equal? `#(+ 2 3) '#(+ 2 3))
   (equal? `#(+ 2 ,(* 3 4)) '#(+ 2 12))
   (equal? `#(a b (,(+ 2 3) c) d) '#(a b (5 c) d))
   (equal? `#(a b ,(reverse '(c d e)) f g) '#(a b (e d c) f g))
   (equal? `#(+ ,@(cdr '(* 2 3))) '#(+ 2 3))
   (equal? `#(a b ,@(reverse '(c d e)) f g) '#(a b e d c f g))
   (equal? `#(10 5 ,@'(4 3)) '#(10 5 4 3))
   (equal? (let ((x 1) (y 2))
             `(foo (,x ,y)
                   `(bar ,@(baz ,y))))
           '(foo (1 2) `(bar ,@(baz 2))))
   (equal? `#&(10 5 ,@'(4 3)) '#&(10 5 4 3))
   (equal? `#&,cons (box cons))
  ; test Bawden's extensions to quasiquote
   (equal? `(a (unquote-splicing '(b) '(c)) d) '(a b c d))
   (equal? `(a (unquote '(b) '(c)) d) '(a (b) (c) d))
   (begin
     (begin (define x '(m n)) (define m '(b c)) (define n '(d e)))
     (equal?
       (list (eval ``(a ,@,@x f) (interaction-environment))
             (eval ``(a ,@,@x) (interaction-environment)))
       '((a b c d e f) (a b c d e))))
  ; test to make sure we leave bare unquote alone in vectors
   (equal? `#((+ 1 2) unquote)
           '#((+ 1 2) unquote))
   (equal? `#((+ 1 2) unquote (+ 3 4))
           '#((+ 1 2) unquote (+ 3 4)))
   (equal? `#((+ 1 2) unquote (list 3 4))
           '#((+ 1 2) unquote (list 3 4)))
   (equal? `#((+ 1 2) unquote (+ 2 3) (+ 3 4))
           '#((+ 1 2) unquote (+ 2 3) (+ 3 4)))
   (equal? `#(unquote)
           '#(unquote))
   (equal? `#(unquote (+ 3 4))
           '#(unquote (+ 3 4)))
   (equal? `#(unquote (list 3 4))
           '#(unquote (list 3 4)))
   (equal? `#(unquote (+ 2 3) (+ 3 4))
           '#(unquote (+ 2 3) (+ 3 4)))
  ; new tests to exercise reimplementation
   (let ([f (lambda () (import scheme) `(,'a . ,'b))])
     (not (eq? (f) (f))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(,a . ,b)))
    (if (= (optimize-level) 3)
        '(#3%cons a b)
        '(#2%cons a b)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(,a ,c . ,b)))
    (if (= (optimize-level) 3)
        '(#3%list* a c b)
        '(#2%list* a c b)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a ,@b ,c d ,e f)))
    (if (= (optimize-level) 3)
        '(#3%cons 'a (#3%append b (#3%list* c 'd e '(f))))
        '(#2%cons 'a (#2%append b (#2%list* c 'd e '(f))))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(,'a ,'c . ,'b)))
    (if (= (optimize-level) 3)
        '(#3%list* 'a 'c 'b)
        '(#2%list* 'a 'c 'b)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b c)))
    ''(a b c))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b ,c)))
    (if (= (optimize-level) 3)
        '(#3%list 'a 'b c)
        '(#2%list 'a 'b c)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(,'a ,@c ,'b)))
    (if (= (optimize-level) 3)
        '(#3%cons 'a (#3%append c (#3%list 'b)))
        '(#2%cons 'a (#2%append c (#2%list 'b)))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a ,@'() c)))
    (if (= (optimize-level) 3)
        '(#3%cons 'a (#3%append '() '(c)))
        '(#2%cons 'a (#2%append '() '(c)))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote) d)))
    ''(a b d))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote c1) d)))
    (if (= (optimize-level) 3)
        '(#3%list* 'a 'b c1 '(d))
        '(#2%list* 'a 'b c1 '(d))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote c1 c2) d)))
    (if (= (optimize-level) 3)
        '(#3%list* 'a 'b c1 c2 '(d))
        '(#2%list* 'a 'b c1 c2 '(d))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote c1) ,d)))
    (if (= (optimize-level) 3)
        '(#3%list 'a 'b c1 d)
        '(#2%list 'a 'b c1 d)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote c1 c2) ,d)))
    (if (= (optimize-level) 3)
        '(#3%list 'a 'b c1 c2 d)
        '(#2%list 'a 'b c1 c2 d)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote-splicing) d)))
    ''(a b d))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote-splicing c1) d)))
    (if (= (optimize-level) 3)
        '(#3%list* 'a 'b (#3%append c1 '(d)))
        '(#2%list* 'a 'b (#2%append c1 '(d)))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a b (unquote-splicing c1 c2) d)))
    (if (= (optimize-level) 3)
        '(#3%list* 'a 'b (#3%append c1 c2 '(d)))
        '(#2%list* 'a 'b (#2%append c1 c2 '(d)))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b c)))
    ''#(a b c))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(,c d)))
    (if (= (optimize-level) 3)
        '(#3%vector c 'd)
        '(#2%vector c 'd)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b ,c)))
    (if (= (optimize-level) 3)
        '(#3%vector 'a 'b c)
        '(#2%vector 'a 'b c)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b ,c d)))
    (if (= (optimize-level) 3)
        '(#3%vector 'a 'b c 'd)
        '(#2%vector 'a 'b c 'd)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b ,@c d)))
    (if (= (optimize-level) 3)
        '(#3%list->vector (#3%list* 'a 'b (#3%append c '(d))))
        '(#2%list->vector (#2%list* 'a 'b (#2%append c '(d))))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b (unquote) d)))
    ''#(a b d))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b (unquote c1) d)))
    (if (= (optimize-level) 3)
        '(#3%vector 'a 'b c1 'd)
        '(#2%vector 'a 'b c1 'd)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b (unquote c1 c2) d)))
    (if (= (optimize-level) 3)
        '(#3%vector 'a 'b c1 c2 'd)
        '(#2%vector 'a 'b c1 c2 'd)))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b (unquote-splicing) d)))
    ''#(a b d))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b (unquote-splicing c1) d)))
    (if (= (optimize-level) 3)
        '(#3%list->vector (#3%list* 'a 'b (#3%append c1 '(d))))
        '(#2%list->vector (#2%list* 'a 'b (#2%append c1 '(d))))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`#(a b (unquote-splicing c1 c2) d)))
    (if (= (optimize-level) 3)
        '(#3%list->vector (#3%list* 'a 'b (#3%append c1 c2 '(d))))
        '(#2%list->vector (#2%list* 'a 'b (#2%append c1 c2 '(d))))))
  (equivalent-expansion?
    (parameterize ([#%$suppress-primitive-inlining #f])
      (expand '`(a `(b0 ,(b1 ,@b2 ,@b3)) (unquote c1 c2) ,d)))
    (if (= (optimize-level) 3)
        '(#3%list 'a
           (#3%list 'quasiquote
             (#3%list 'b0
               (#3%list 'unquote (#3%cons 'b1 (#3%append b2 b3)))))
           c1 c2 d)
        '(#2%list 'a
           (#2%list 'quasiquote
             (#2%list 'b0
               (#2%list 'unquote (#2%cons 'b1 (#2%append b2 b3)))))
           c1 c2 d)))
 )

;;; section 4-3:

(mat begin
   (error? (or (begin) #t)) ;just see if (begin) is allowed
   (begin (eq? 'a 'a))
   (let ([x 'a]) (begin (set! x 'b) (eq? x 'b)))
   (let ([x 'a])
      (begin
         (set! x 'b)
         (set! x (cons x x))
         (equal? x '(b . b))))
 )

;;; section 4-4:

(mat if
   (let ([x 'a])
      (set! x 'b)
      (and
         (eq? (if (eq? x 'a) 'a 'b) 'b)
         (eq? (if (eq? x 'b) 'a 'b) 'a)))
   (let ([x 'a])
      (if (eq? x 'a) (set! x 'b))
      (if (eq? x 'a) (set! x 'c))
      (eq? x 'b))

   (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (not (not (f x))) e1 e2)))
     '(if (f x) e1 e2))

   (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) #f #t)) e1 e2)))
     '(begin (set! x y) (if (begin (set! z y) (#2%zero? h)) e2 e1)))

  (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) #f #f)) e1 e2)))
     '(begin (set! x y) (set! z y) (#2%zero? h) e2))

  (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) #t #t)) e1 e2)))
     '(begin (set! x y) (set! z y) (#2%zero? h) e1))
  
  (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) #t #f)) e1 e2)))
     '(begin (set! x y) (if (begin (set! z y) (#2%zero? h)) e1 e2)))

  (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) (begin (set! l z) #t) (begin (set! l y) #f))) e1 e2)))
     '(begin (set! x y) (if (begin (set! z y) (#2%zero? h)) (begin (set! l z) e1) (begin (set! l y) e2))))

  (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) (begin (set! l z) #t) (begin (set! l y) #t))) e1 e2)))
     '(begin (set! x y) (if (begin (set! z y) (#2%zero? h)) (set! l z) (set! l y)) e1))

  (equivalent-expansion?
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) (begin (set! l z) #f) (begin (set! l y) #f))) e1 e2)))
     '(begin (set! x y) (if (begin (set! z y) (#2%zero? h)) (set! l z) (set! l y)) e2))

  (equivalent-expansion? 
     (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
       (expand/optimize '(if (begin (set! x y) (if (begin (set! z y) (zero? h)) (begin (set! l z) #f) (begin (set! l y) #t))) e1 e2)))
     '(begin (set! x y) (if (begin (set! z y) (#2%zero? h)) (begin (set! l z) e2) (begin (set! l y) e1))))

 )

(mat when
   (= (let ((x 12)) (when (= x 12) (set! x 11) (set! x 1)) x) 1)
   (= (let ((x 12)) (when (= x 11) (set! x 11) (set! x 1)) x) 12)
 )

(mat unless
   (eq? (let ((y 'a)) (unless (eq? y 'b) (set! y 'c)) y) 'c)
   (eq? (let ((y 'a)) (unless (eq? y 'a) (set! y 'c)) y) 'a)
 )

(mat not
   (not #f)
   (not (not #t))
   (let ((x 3)) (set! x 4) (not (= x 3)))
 )

(mat and
   (not (let ((x 'x)) (set! x #f) (and x #t #t)))
   (eq? (let ((x 'x)) (and x (begin (set! x 'c) x) x)) 'c)
 )

(mat or
   (equal? (let ((x 'x)) (set! x '()) (or (eq? x 'x) (cons x x) 3)) '(()))
   (equal? (let ((x 'x)) (set! x '()) (or (eq? x 'x) (not (null? x)) 3)) 3)
   (not (let ((x 'x)) (set! x '()) (or (eq? x 'x) (not (null? x)))))
   ; make sure the following isn't incorrectly recognized as an or
   (equal? (let ((x #f)) (if x x (cons x x))) '(#f . #f))
 )

(mat cond
  (error? ; invalid syntax
    (cond))
   (let ((a 'a))
      (and (begin (set! a 3)
                  (cond ((= a 4) #f) ((= a 3) #t) (else #f)))
           (begin (set! a 4)
                  (cond ((= a 4) #t) ((= a 3) #f) (else #f)))
           (begin (set! a 2)
                  (cond ((= a 4) #f) ((= a 3) #f) (else #t)))
           (begin (set! a 4)
                  (cond ((= a 4)) ((= a 3) #f) (else #f)))
           (begin (set! a 3)
                  (cond ((= a 4) #f) ((= a 3) (= a 4) #t) (else #f)))))
   (eq? 'b (cond ((assq 'a '((a . b))) => cdr) (else #f)))
   (equal? '(b c) (cond ((memq 'b '(a b c))) (else #f)))
  ; make sure cond requires procedure on RHS of =>
   (error?
     (let () ; aziz's strange example
       (define-syntax x
         (syntax-rules ()
           ((_ t) (lambda (t) t))))
       ((cond (#t => x)) 18)))
 )

(mat exclusive-cond
   (error? ; invalid syntax
     (exclusive-cond [a . b]))
   (error? ; invalid syntax
     (exclusive-cond))
   (let ((a 'a))
      (and (begin (set! a 3)
                  (exclusive-cond ((= a 4) #f) ((= a 3) #t) (else #f)))
           (begin (set! a 4)
                  (exclusive-cond ((= a 4) #t) ((= a 3) #f) (else #f)))
           (begin (set! a 2)
                  (exclusive-cond ((= a 4) #f) ((= a 3) #f) (else #t)))
           (begin (set! a 4)
                  (exclusive-cond ((= a 4) => (lambda (x) x)) ((= a 3) #f) (else #f)))
           (begin (set! a 3)
                  (exclusive-cond ((= a 4) #f) ((= a 3) (= a 4) #t) (else #f)))))
   (eq? 'b (exclusive-cond ((assq 'a '((a . b))) => cdr) (else #f)))
   (equal? '(b c) (exclusive-cond ((memq 'b '(a b c)) => (lambda (x) x)) (else #f)))
  ; make sure exclusive-cond requires procedure on RHS of =>
   (error?
     (let () ; aziz's strange example
       (define-syntax x
         (syntax-rules ()
           ((_ t) (lambda (t) t))))
       ((exclusive-cond (#t => x)) 18)))
  ; verify that exclusive cond actually reorders with profile information available
   (begin
     (with-output-to-file "testfile.ss"
       (lambda ()
         (pretty-print
           '(let ()
              (define count1 0)
              (define count2 0)
              (define count3 0)
              (define count4 0)
              (define count5 0)
              (define foo
                (lambda (n)
                  (exclusive-cond
                    [(begin (set! count1 (+ count1 1)) (< n 5))
                     (set! count3 (+ count3 1))]
                    [(begin (set! count2 (+ count2 1)) (> n 5))
                     (set! count4 (+ count4 1))]
                    [else (set! count5 (+ count5 1))])))
              (do ([i 10 (fx- i 1)])
                ((fx= i 0))
                (foo 10))
              (foo 3)
              (pretty-print (list count1 count2 count3 count4 count5)))))
       'replace)
     (profile-clear-database)
     #t)
   (equal?
     (with-output-to-string
       (lambda ()
         ; make sure no collection occurs before profile data is dumped
         (parameterize ([compile-profile #t] [collect-request-handler void])
           (load "testfile.ss" compile)
           (profile-dump-data "testfile.pd" (remp preexisting-profile-dump-entry? (profile-dump))))
         ; make sure collections are restarted
         (collect)))
     "(11 10 1 10 0)\n")
   (begin
     (profile-load-data "testfile.pd")
     #t)
   (equal?
     (with-output-to-string
       (lambda ()
         (load "testfile.ss" compile)))
     "(1 11 1 10 0)\n")
   (begin
     (profile-clear-database)
     #t)
   (begin
     (profile-load-data "testfile.pd" "testfile.pd")
     #t)
   (equal?
     (with-output-to-string
       (lambda ()
         (load "testfile.ss" compile)))
     "(1 11 1 10 0)\n")
   (begin
     (profile-clear-database)
     #t)
 )

(mat case
  (error? ; invalid syntax
    (case 3 [a . b]))
  (eq? (case 'a [a 'yes] [b 'no]) 'yes)
  (let ((a 'a))
    (and
      (begin (set! a 'a)
        (case a (a #t) ((b c) #f))
        (case a (a #t) ((b c) #f) (else #f)))
      (begin (set! a 'b)
        (case a (a #f) ((b c) #t))
        (case a (a #f) ((b c) #t) (else #f)))
      (begin (set! a 'c)
        (case a (a #f) ((b c) #t))
        (case a (a #f) ((b c) #t) (else #f)))
      (begin (set! a 'd)
        (case a (a #f) ((b c) #f) (else #t)))))
  (let ([f (lambda (x)
             (case x
               (#\a 'case1)
               (1/2 'case2)
               (999999999999999 'case3)
               (3.4 'case4)
               (else 'oops)))])
    (and (eq? (f (string-ref "abc" 0)) 'case1)
         (eq? (f (exact 0.5)) 'case2)
         (eq? (f (- 1000000000000000 1)) 'case3)
         (eq? (f (+ 3.0 4/10)) 'case4)
         (eq? (f 'b) 'oops)))
  (case '() [() #f] [else #t])
  (case '() [(()) #t] [else #f])
  (case "meow" ["meow" #t] [else #f])
  (case '(1 2 3) [((1 2 3) (3 2 1)) #t] [else #f])
  (case 'a [1 6] ["meow" #f] [(a b c) #t])
  (case #\: [1 6] ["meow" #f] [(a b c) #f] [(#\; #\9 #\: #\4) #t])
  (case (/ 12.0 3.0) [(4 5 6) #f] [("bla") #f] [(a b c) #f] [(1 5.8 4.9 4.0) #t] [else #f])
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define foo
             (lambda (x)
               (case x
                 [("three" 4) 'B]
                 [("three" 5) 'A]
                 [else #f]))))
        (pretty-print
          '(begin
             (do ([i 10 (fx- i 1)]) ((fx= i 0)) (write (foo 5)))
             (write (foo "three")))))
      'replace)
    (profile-clear-database)
    #t)
  ; verify no reordering w/no profile information
  (let ([x (let* ([ip (open-file-input-port "testfile.ss")]
                  [sfd (make-source-file-descriptor "testfile.ss" ip #t)]
                  [ip (transcoded-port ip (native-transcoder))])
             (let-values ([(x efp) (get-datum/annotations ip sfd 0)])
               (close-port ip)
               (parameterize ([optimize-level 2] [enable-cp0 #f] [#%$suppress-primitive-inlining #f]) (expand/optimize x))))])
    ; redundant keys might or might not be pruned, so allow it both ways
    (or (equivalent-expansion?
          x
          '(begin
             (set! foo
               (lambda (x)
                 (let ([t x])
                   (if (#2%member t '("three" 4))
                       'B
                       (if (#2%member t '("three" 5))
                           'A
                           #f)))))
             (#2%void)))
        (equivalent-expansion?
          x
          '(begin
             (set! foo
               (lambda (x)
                 (let ([t x])
                   (if (#2%member t '("three" 4))
                       'B
                       (if (#2%member t '(5))
                           'A
                           #f)))))
             (#2%void)))))
  (equal?
    (with-output-to-string
      (lambda ()
        (parameterize ([compile-profile #t]) (load "testfile.ss" compile))))
    "AAAAAAAAAAB")
  (begin
    (profile-dump-data "testfile.pd" (remp preexisting-profile-dump-entry? (profile-dump)))
    (profile-load-data "testfile.pd")
    #t)
  (equal?
    (with-output-to-string
      (lambda ()
        (load "testfile.ss" compile)))
    "AAAAAAAAAAB")
  ; verify reordering based on profile information
  (equivalent-expansion?
    (let* ([ip (open-file-input-port "testfile.ss")]
           [sfd (make-source-file-descriptor "testfile.ss" ip #t)]
           [ip (transcoded-port ip (native-transcoder))])
      (let-values ([(x efp) (get-datum/annotations ip sfd 0)])
        (close-port ip)
        (parameterize ([optimize-level 2] [enable-cp0 #f] [#%$suppress-primitive-inlining #f]) (expand/optimize x))))
    '(begin
       (set! foo
         (lambda (x)
           (let ([t x])
             (if (#2%member t '(5))
                 'A
                 (if (#2%member t '("three" 4))
                     'B
                     #f)))))
       (#2%void)))
  (begin
    (profile-clear-database)
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #f] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(lambda (x) (case x [(a b a 7) 'one] [(c a 7 9) 'two] [else 'three]))))
    '(lambda (x)
       (let ([t x])
         (if (#2%member t '(a b 7))
             'one
             (if (#2%member t '(c 9))
                 'two
                 'three)))))
  ; ensure we don't miss syntax errors through case discarding unreachable clause bodies
  (error? ; invalid syntax (if)
    (lambda (x)
      (case x
        [(a) 'one]
        [(b c) 'two]
        [(a b c) (if)]
        [else #f])))
  ; ensure expansion into cond doesn't cause => to "work" for case
  (error? ; invalid syntax =>
    (lambda (x)
      (case x
        [(a b c) => values])))
  (error? ; invalid syntax =>
    (lambda (x)
      (case x
        [(a b c) #f]
        [(d e f) => values])))
  (error? ; invalid syntax =>
    (lambda (x)
      (case x
        [(a b c) #f]
        [(a b c) => values])))
  (error? ; invalid syntax =>
    (lambda (x)
      (case x
        [(a b c) => values]
        [else #f])))
  (error? ; invalid syntax =>
    (lambda (x)
      (case x
        [(a b c) #f]
        [(d e f) => values]
        [else #f])))
  (error? ; invalid syntax =>
    (lambda (x)
      (case x
        [(a b c) #f]
        [(a b c) => values]
        [else #f])))
  (error? ; invalid syntax (case)
    (case))
)

(mat r6rs:case
  (error? ; invalid syntax
    (let ()
      (import (only (rnrs) case))
      (case 'a [a 'yes] [b 'no])))
  (error? ; invalid syntax
    (let ()
      (import (only (rnrs) case))
      (case 'a [a 'yes] [b 'no])))
  (let ((a 'a))
    (import (only (rnrs) case))
    (and
      (begin (set! a 'a)
        (case a ((a) #t) ((b c) #f))
        (case a ((a) #t) ((b c) #f) (else #f)))
      (begin (set! a 'b)
        (case a ((a) #f) ((b c) #t))
        (case a ((a) #f) ((b c) #t) (else #f)))
      (begin (set! a 'c)
        (case a ((a) #f) ((b c) #t))
        (case a ((a) #f) ((b c) #t) (else #f)))
      (begin (set! a 'd)
        (case a ((a) #f) ((b c) #f) (else #t)))))
  (let ([f (lambda (x)
             (import (only (rnrs) case))
             (case x
               ((#\a) 'case1)
               ((1/2) 'case2)
               ((999999999999999) 'case3)
               ((3.4) 'case4)
               (else 'oops)))])
    (and (eq? (f (string-ref "abc" 0)) 'case1)
         (eq? (f (exact 0.5)) 'case2)
         (eq? (f (- 1000000000000000 1)) 'case3)
         (eq? (f (+ 3.0 4/10)) 'case4)
         (eq? (f 'b) 'oops)))
  (let ()
    (import (only (rnrs) case))
    (case '() [() #f] [else #t]))
  (let ()
    (import (only (rnrs) case))
    (case '() [(()) #t] [else #f]))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #f] [#%$suppress-primitive-inlining #f])
      (expand/optimize '(lambda (x)
                          (import (only (rnrs) case))
                          (case x [(a b a 7) 'one] [(c a 7 9) 'two] [else 'three]))))
    '(lambda (x)
       (let ([t x])
         (if (#2%memv t '(a b 7))
             'one
             (if (#2%memv t '(c 9))
                 'two
                 'three)))))
  ; ensure we don't miss syntax errors through case discarding unreachable clause bodies
  (error? ; invalid syntax (if)
    (lambda (x)
      (import (only (rnrs) case))
      (case x
        [(a) 'one]
        [(b c) 'two]
        [(a b c) (if)]
        [else #f])))
  ; ensure expansion into cond doesn't cause => to "work" for case
  (error? ; invalid syntax =>
    (lambda (x)
      (import (only (rnrs) case))
      (case x
        [(a b c) => values])))
  (error? ; invalid syntax =>
    (lambda (x)
      (import (only (rnrs) case))
      (case x
        [(a b c) #f]
        [(d e f) => values])))
  (error? ; invalid syntax =>
    (lambda (x)
      (import (only (rnrs) case))
      (case x
        [(a b c) #f]
        [(a b c) => values])))
  (error? ; invalid syntax =>
    (lambda (x)
      (import (only (rnrs) case))
      (case x
        [(a b c) => values]
        [else #f])))
  (error? ; invalid syntax =>
    (lambda (x)
      (import (only (rnrs) case))
      (case x
        [(a b c) #f]
        [(d e f) => values]
        [else #f])))
  (error? ; invalid syntax =>
    (lambda (x)
      (import (only (rnrs) case))
      (case x
        [(a b c) #f]
        [(a b c) => values]
        [else #f])))
  (error? ; invalid syntax (case)
    (let ()
      (import (only (rnrs) case))
      (case)))
)

(mat record-case
   (record-case '(a b c)
      [a (b c) (and (eq? b 'b) (eq? c 'c))]
      [b x #f]
      [c x #f]
      [else #f])
   (record-case (list #\a #\b #\c)
      [#\a (b c) (and (eq? b #\b) (eq? c #\c))]
      [#\b x #f]
      [#\c x #f])
   (record-case (list (/ 3 4) 'b 'c)
      [1/2 x #f]
      [3/4 x (equal? x '(b c))]
      [5/8 x #f]
      [else #f])
   (record-case '(d a b c)
      [a x (equal? x '(b c))]
      [b x #f]
      [c x #f]
      [else #t])
   (record-case '(a b c d e)
      [a (x1 x2 x3 . x4) (equal? (list x1 x2 x3 x4) '(b c d (e)))]
      [else #f])
 )

;;; section 4-5:

(mat named-let
   (eqv? (let f ((x 5)) (if (zero? x) 1 (* x (f (1- x))))) 120)
   (let f ((x 10000)) (if (zero? x) #t (f (1- x))))
   (let f ([x 10] [y 0]) (or (and (= x 0) (= y 10)) (f (- x 1) (+ y 1))))
   (eqv? (let f ([x 10]) (if (= x 0) 1 (+ (f (- x 1)) 1))) 11)
   (eqv? (let ([base 20])
            (let f ([x 10])
               (if (= x 0) base
                   (+ (f (- x 1)) 1))))
         30)
   ; this looks almost like a named let, but isn't, and is treated as
   ; if the 4 were not present by some earlier verisons
   (error? ((letrec ((x (lambda (x) x))) x) 3 4))
 )

(define ($destroy ls x)
  (when (pair? ls)
    ($destroy (cdr ls) x)
    (set-cdr! ls x)))

(mat map
   (eqv? (map car '()) '())
   (equal? (map 1+ '(1 2 3 4 5 6)) '(2 3 4 5 6 7))
   (equal? (map 1+ '()) '())
   (equal? (map cons '(1 2 3) '(4 5 6)) '((1 . 4) (2 . 5) (3 . 6)))
   (let ((x 3))
      (equal? (apply + (map (lambda (y) (set! x (1+ x)) x) '(a b c d)))
              22))
   (equal? (map (lambda (x y z) (+ x (+ y z)))
                '(1 2 3 4 5)
                '(11 12 13 14 15)
                '(21 22 23 24 25))
           '(33 36 39 42 45))
   (begin
     (define ($map-f1 p x1 x2 x3 x4 x5)
       (list
         (map p '())
         (map p '() x1)
         (map p '() x1 x2)
         (map p '() x1 x2 x3)
         (map p '() x1 x2 x3 x4)
         (map p '() x1 x2 x3 x4 x5)
         (map p x1 '())
         (map p x1 '() x2)
         (map p x1 '() x2 x3)
         (map p x1 '() x2 x3 x4)
         (map p x1 '() x2 x3 x4 x5)
         (map p x1 x2 '())
         (map p x1 x2 '() x3)
         (map p x1 x2 '() x3 x4)
         (map p x1 x2 '() x3 x4 x5)
         (map p x1 x2 x3 '())
         (map p x1 x2 x3 '() x4)
         (map p x1 x2 x3 '() x4 x5)
         (map p x1 x2 x3 x4 '())
         (map p x1 x2 x3 x4 '() x5)
         (map p x1 x2 x3 x4 x5 '())))
     (procedure? $map-f1))
   (equal?
     ($map-f1 list '() '() '() '() '())
     '(() () () () () () () () () () () () () () () () () ()
       () () ()))
   (begin
     (define ($map-f1 p x1 x2 x3 x4 x5)
       (list
         (map p '(a))
         (map p '(a) x1)
         (map p '(a) x1 x2)
         (map p '(a) x1 x2 x3)
         (map p '(a) x1 x2 x3 x4)
         (map p '(a) x1 x2 x3 x4 x5)
         (map p x1 '(a))
         (map p x1 '(a) x2)
         (map p x1 '(a) x2 x3)
         (map p x1 '(a) x2 x3 x4)
         (map p x1 '(a) x2 x3 x4 x5)
         (map p x1 x2 '(a))
         (map p x1 x2 '(a) x3)
         (map p x1 x2 '(a) x3 x4)
         (map p x1 x2 '(a) x3 x4 x5)
         (map p x1 x2 x3 '(a))
         (map p x1 x2 x3 '(a) x4)
         (map p x1 x2 x3 '(a) x4 x5)
         (map p x1 x2 x3 x4 '(a))
         (map p x1 x2 x3 x4 '(a) x5)
         (map p x1 x2 x3 x4 x5 '(a))))
     (procedure? $map-f1))
   (equal?
     ($map-f1 list '(1) '(4) '(d) '(g) '(7))
     '(((a))
       ((a 1))
       ((a 1 4))
       ((a 1 4 d))
       ((a 1 4 d g))
       ((a 1 4 d g 7))
       ((1 a))
       ((1 a 4))
       ((1 a 4 d))
       ((1 a 4 d g))
       ((1 a 4 d g 7))
       ((1 4 a))
       ((1 4 a d))
       ((1 4 a d g))
       ((1 4 a d g 7))
       ((1 4 d a))
       ((1 4 d a g))
       ((1 4 d a g 7))
       ((1 4 d g a))
       ((1 4 d g a 7))
       ((1 4 d g 7 a))))
   (begin
     (define ($map-f1 p x1 x2 x3 x4 x5)
       (list
         (map p '(a b))
         (map p '(a b) x1)
         (map p '(a b) x1 x2)
         (map p '(a b) x1 x2 x3)
         (map p '(a b) x1 x2 x3 x4)
         (map p '(a b) x1 x2 x3 x4 x5)
         (map p x1 '(a b))
         (map p x1 '(a b) x2)
         (map p x1 '(a b) x2 x3)
         (map p x1 '(a b) x2 x3 x4)
         (map p x1 '(a b) x2 x3 x4 x5)
         (map p x1 x2 '(a b))
         (map p x1 x2 '(a b) x3)
         (map p x1 x2 '(a b) x3 x4)
         (map p x1 x2 '(a b) x3 x4 x5)
         (map p x1 x2 x3 '(a b))
         (map p x1 x2 x3 '(a b) x4)
         (map p x1 x2 x3 '(a b) x4 x5)
         (map p x1 x2 x3 x4 '(a b))
         (map p x1 x2 x3 x4 '(a b) x5)
         (map p x1 x2 x3 x4 x5 '(a b))))
     (procedure? $map-f1))
   (equal?
     ($map-f1 list '(1 2) '(4 5) '(d e) '(g h) '(7 j))
     '(((a) (b))
       ((a 1) (b 2))
       ((a 1 4) (b 2 5))
       ((a 1 4 d) (b 2 5 e))
       ((a 1 4 d g) (b 2 5 e h))
       ((a 1 4 d g 7) (b 2 5 e h j))
       ((1 a) (2 b))
       ((1 a 4) (2 b 5))
       ((1 a 4 d) (2 b 5 e))
       ((1 a 4 d g) (2 b 5 e h))
       ((1 a 4 d g 7) (2 b 5 e h j))
       ((1 4 a) (2 5 b))
       ((1 4 a d) (2 5 b e))
       ((1 4 a d g) (2 5 b e h))
       ((1 4 a d g 7) (2 5 b e h j))
       ((1 4 d a) (2 5 e b))
       ((1 4 d a g) (2 5 e b h))
       ((1 4 d a g 7) (2 5 e b h j))
       ((1 4 d g a) (2 5 e h b))
       ((1 4 d g a 7) (2 5 e h b j))
       ((1 4 d g 7 a) (2 5 e h j b))))
   (begin
     (define ($map-f1 p x1 x2 x3 x4 x5)
       (list
         (map p '(a b c))
         (map p '(a b c) x1)
         (map p '(a b c) x1 x2)
         (map p '(a b c) x1 x2 x3)
         (map p '(a b c) x1 x2 x3 x4)
         (map p '(a b c) x1 x2 x3 x4 x5)
         (map p x1 '(a b c))
         (map p x1 '(a b c) x2)
         (map p x1 '(a b c) x2 x3)
         (map p x1 '(a b c) x2 x3 x4)
         (map p x1 '(a b c) x2 x3 x4 x5)
         (map p x1 x2 '(a b c))
         (map p x1 x2 '(a b c) x3)
         (map p x1 x2 '(a b c) x3 x4)
         (map p x1 x2 '(a b c) x3 x4 x5)
         (map p x1 x2 x3 '(a b c))
         (map p x1 x2 x3 '(a b c) x4)
         (map p x1 x2 x3 '(a b c) x4 x5)
         (map p x1 x2 x3 x4 '(a b c))
         (map p x1 x2 x3 x4 '(a b c) x5)
         (map p x1 x2 x3 x4 x5 '(a b c))))
     (procedure? $map-f1))
   (equal?
     ($map-f1 list '(1 2 3) '(4 5 6) '(d e f) '(g h i) '(7 j 9))
     '(((a) (b) (c))
       ((a 1) (b 2) (c 3))
       ((a 1 4) (b 2 5) (c 3 6))
       ((a 1 4 d) (b 2 5 e) (c 3 6 f))
       ((a 1 4 d g) (b 2 5 e h) (c 3 6 f i))
       ((a 1 4 d g 7) (b 2 5 e h j) (c 3 6 f i 9))
       ((1 a) (2 b) (3 c))
       ((1 a 4) (2 b 5) (3 c 6))
       ((1 a 4 d) (2 b 5 e) (3 c 6 f))
       ((1 a 4 d g) (2 b 5 e h) (3 c 6 f i))
       ((1 a 4 d g 7) (2 b 5 e h j) (3 c 6 f i 9))
       ((1 4 a) (2 5 b) (3 6 c))
       ((1 4 a d) (2 5 b e) (3 6 c f))
       ((1 4 a d g) (2 5 b e h) (3 6 c f i))
       ((1 4 a d g 7) (2 5 b e h j) (3 6 c f i 9))
       ((1 4 d a) (2 5 e b) (3 6 f c))
       ((1 4 d a g) (2 5 e b h) (3 6 f c i))
       ((1 4 d a g 7) (2 5 e b h j) (3 6 f c i 9))
       ((1 4 d g a) (2 5 e h b) (3 6 f i c))
       ((1 4 d g a 7) (2 5 e h b j) (3 6 f i c 9))
       ((1 4 d g 7 a) (2 5 e h j b) (3 6 f i 9 c))))
   (begin
     (define ($map-f1 p x1 x2 x3 x4 x5)
       (list
         (map p '(a b c d))
         (map p '(a b c d) x1)
         (map p '(a b c d) x1 x2)
         (map p '(a b c d) x1 x2 x3)
         (map p '(a b c d) x1 x2 x3 x4)
         (map p '(a b c d) x1 x2 x3 x4 x5)
         (map p x1 '(a b c d))
         (map p x1 '(a b c d) x2)
         (map p x1 '(a b c d) x2 x3)
         (map p x1 '(a b c d) x2 x3 x4)
         (map p x1 '(a b c d) x2 x3 x4 x5)
         (map p x1 x2 '(a b c d))
         (map p x1 x2 '(a b c d) x3)
         (map p x1 x2 '(a b c d) x3 x4)
         (map p x1 x2 '(a b c d) x3 x4 x5)
         (map p x1 x2 x3 '(a b c d))
         (map p x1 x2 x3 '(a b c d) x4)
         (map p x1 x2 x3 '(a b c d) x4 x5)
         (map p x1 x2 x3 x4 '(a b c d))
         (map p x1 x2 x3 x4 '(a b c d) x5)
         (map p x1 x2 x3 x4 x5 '(a b c d))))
     (procedure? $map-f1))
   (equal?
     ($map-f1 list '(1 2 3 4) '(f g h i) '(k l m n) '(p q r s) '(u v w x))
     '(((a) (b) (c) (d)) ((a 1) (b 2) (c 3) (d 4))
       ((a 1 f) (b 2 g) (c 3 h) (d 4 i))
       ((a 1 f k) (b 2 g l) (c 3 h m) (d 4 i n))
       ((a 1 f k p) (b 2 g l q) (c 3 h m r) (d 4 i n s))
       ((a 1 f k p u) (b 2 g l q v) (c 3 h m r w) (d 4 i n s x))
       ((1 a) (2 b) (3 c) (4 d))
       ((1 a f) (2 b g) (3 c h) (4 d i))
       ((1 a f k) (2 b g l) (3 c h m) (4 d i n))
       ((1 a f k p) (2 b g l q) (3 c h m r) (4 d i n s))
       ((1 a f k p u) (2 b g l q v) (3 c h m r w) (4 d i n s x))
       ((1 f a) (2 g b) (3 h c) (4 i d))
       ((1 f a k) (2 g b l) (3 h c m) (4 i d n))
       ((1 f a k p) (2 g b l q) (3 h c m r) (4 i d n s))
       ((1 f a k p u) (2 g b l q v) (3 h c m r w) (4 i d n s x))
       ((1 f k a) (2 g l b) (3 h m c) (4 i n d))
       ((1 f k a p) (2 g l b q) (3 h m c r) (4 i n d s))
       ((1 f k a p u) (2 g l b q v) (3 h m c r w) (4 i n d s x))
       ((1 f k p a) (2 g l q b) (3 h m r c) (4 i n s d))
       ((1 f k p a u) (2 g l q b v) (3 h m r c w) (4 i n s d x))
       ((1 f k p u a) (2 g l q v b) (3 h m r w c) (4 i n s x d))))
   (begin
     (define ($map-f1 p x1 x2 x3 x4 x5)
       (list
         (map p '(a b c d e))
         (map p '(a b c d e) x1)
         (map p '(a b c d e) x1 x2)
         (map p '(a b c d e) x1 x2 x3)
         (map p '(a b c d e) x1 x2 x3 x4)
         (map p '(a b c d e) x1 x2 x3 x4 x5)
         (map p x1 '(a b c d e))
         (map p x1 '(a b c d e) x2)
         (map p x1 '(a b c d e) x2 x3)
         (map p x1 '(a b c d e) x2 x3 x4)
         (map p x1 '(a b c d e) x2 x3 x4 x5)
         (map p x1 x2 '(a b c d e))
         (map p x1 x2 '(a b c d e) x3)
         (map p x1 x2 '(a b c d e) x3 x4)
         (map p x1 x2 '(a b c d e) x3 x4 x5)
         (map p x1 x2 x3 '(a b c d e))
         (map p x1 x2 x3 '(a b c d e) x4)
         (map p x1 x2 x3 '(a b c d e) x4 x5)
         (map p x1 x2 x3 x4 '(a b c d e))
         (map p x1 x2 x3 x4 '(a b c d e) x5)
         (map p x1 x2 x3 x4 x5 '(a b c d e))))
     (procedure? $map-f1))
   (equal?
     ($map-f1 list '(1 2 3 4 5) '(f g h i j) '(k l m n o) '(p q r s t) '(u v w x y))
     '(((a) (b) (c) (d) (e)) ((a 1) (b 2) (c 3) (d 4) (e 5))
       ((a 1 f) (b 2 g) (c 3 h) (d 4 i) (e 5 j))
       ((a 1 f k) (b 2 g l) (c 3 h m) (d 4 i n) (e 5 j o))
       ((a 1 f k p) (b 2 g l q) (c 3 h m r) (d 4 i n s) (e 5 j o t))
       ((a 1 f k p u) (b 2 g l q v) (c 3 h m r w) (d 4 i n s x) (e 5 j o t y))
       ((1 a) (2 b) (3 c) (4 d) (5 e))
       ((1 a f) (2 b g) (3 c h) (4 d i) (5 e j))
       ((1 a f k) (2 b g l) (3 c h m) (4 d i n) (5 e j o))
       ((1 a f k p) (2 b g l q) (3 c h m r) (4 d i n s) (5 e j o t))
       ((1 a f k p u) (2 b g l q v) (3 c h m r w) (4 d i n s x) (5 e j o t y))
       ((1 f a) (2 g b) (3 h c) (4 i d) (5 j e))
       ((1 f a k) (2 g b l) (3 h c m) (4 i d n) (5 j e o))
       ((1 f a k p) (2 g b l q) (3 h c m r) (4 i d n s) (5 j e o t))
       ((1 f a k p u) (2 g b l q v) (3 h c m r w) (4 i d n s x) (5 j e o t y))
       ((1 f k a) (2 g l b) (3 h m c) (4 i n d) (5 j o e))
       ((1 f k a p) (2 g l b q) (3 h m c r) (4 i n d s) (5 j o e t))
       ((1 f k a p u) (2 g l b q v) (3 h m c r w) (4 i n d s x) (5 j o e t y))
       ((1 f k p a) (2 g l q b) (3 h m r c) (4 i n s d) (5 j o t e))
       ((1 f k p a u) (2 g l q b v) (3 h m r c w) (4 i n s d x) (5 j o t e y))
       ((1 f k p u a) (2 g l q v b) (3 h m r w c) (4 i n s x d) (5 j o t y e))))
 ; make sure compiler doesn't bomb w/two few args
  (procedure? (lambda (x) (map x)))
  (error? ; nonprocedure
    (map 3 '()))
  (error? ; nonprocedure
    (map 3 '() '()))
  (error? ; nonprocedure
    (map 3 '(a b c)))
  (error? ; nonprocedure
    (parameterize ([optimize-level 3])
      (eval '(#2%map 3 '(a b c)))))
  (error? ; nonprocedure
    (parameterize ([optimize-level 3])
      (eval
        '(let ()
           (define (f p b)
             (unbox b)
             (#2%map p (if (box? b) '() '(1 2 3)))
             (list p (procedure? p)))
           (f 7 (box 0))))))
  (error? ; improper list
    (map pretty-print 'a))
  (error? ; improper list
    (map pretty-print '(a . b)))
  (error? ; cyclic list
    (map pretty-print '#1=(a . #1#)))
  (error? ; length mismatch
    (map list '(a b) '(p q r)))
  (error? ; length mismatch
    (map list '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (map list 'a '(a b)))
  (error? ; improper list
    (map list '(a b) 'a))
  (error? ; improper list
    (map list '(a . b) '(a b)))
  (error? ; improper list
    (map list '(a b) '(a . b)))
  (error? ; cyclic list
    (map list '#1# '(a b c)))
  (error? ; cyclic list
    (map list '(a b c) '#1#))
  (error? ; improper list
    (map list 'a '(a b) '(1 2)))
  (error? ; improper list
    (map list '(a b) 'a '(1 2)))
  (error? ; improper list
    (map list '(a b) '(1 2) 'a))
  (error? ; improper list
    (map list '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (map list '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (map list '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (map list '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (map list '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (map list '(a b c) '(1 2 3) '#1#))
  (equal?
    (let ((l (list 1 2 3 4)))
      (map (lambda (x) ($destroy l 1) (* x x)) l))
    '(1 4 9 16))
  (equal?
    (let ((l (list 1 2 3 4)))
      (map (lambda (x y) ($destroy l y) (cons x y)) l '(a b c d)))
    '((1 . a) (2 . b) (3 . c) (4 . d)))
  (equal?
    (let ((l (list 1 2 3 4)))
      (map (lambda (x y) ($destroy l '()) (cons x y)) l '(a b c d)))
    '((1 . a) (2 . b) (3 . c) (4 . d)))
  (equal?
    (let ((l (list 1 2 3 4)))
      (map (lambda (x y) ($destroy l y) (cons x y)) '(a b c d) l))
    '((a . 1) (b . 2) (c . 3) (d . 4)))
  (equal?
    (let ((l (list 1 2 3 4 5 6 7)))
      (map (lambda (x y z) ($destroy l '()) (list z x y))
           l
           '(a b c d e f g)
           '(p q r s t u v)))
    '((p 1 a) (q 2 b) (r 3 c) (s 4 d) (t 5 e) (u 6 f) (v 7 g)))
  (equal?
    (let ((l (list 1 2 3 4 5 6 7)))
      (map (lambda (x y z) ($destroy l '()) (list z x y))
           '(a b c d e f g)
           l
           '(p q r s t u v)))
    '((p a 1) (q b 2) (r c 3) (s d 4) (t e 5) (u f 6) (v g 7)))
  (equal?
    (let ((l (list 1 2 3 4 5 6 7)))
      (map (lambda (x y z) ($destroy l '()) (list z x y))
           '(a b c d e f g)
           '(p q r s t u v)
           l))
    '((1 a p) (2 b q) (3 c r) (4 d s) (5 e t) (6 f u) (7 g v)))
  (let ([orig-ls #f] [orig-cars #f] [orig-cdrs #f] [next #f])
    (define (copy-spine ls)
      (if (null? ls)
          '()
          (cons ls (copy-spine (cdr ls)))))
    (let ([n 100])
      (let ([ls (map (lambda (x) (cons (call/cc values) x)) (iota n))])
        (if orig-ls
            (begin
              (unless (andmap eq? orig-ls orig-cars)
                (errorf #f "original map cars mutated"))
              (unless (andmap eq? (copy-spine orig-ls) orig-cdrs)
                (errorf #f "original map cdrs mutated")))
            (begin
              (set! orig-ls ls)
              (set! orig-cars (list-copy ls))
              (set! orig-cdrs (copy-spine ls))
              (set! next 0)))
        (let ([m next])
          (unless (= m n)
            (set! next (fx+ next 1)) 
            (let ([p (list-ref orig-ls m)])
              (unless (eqv? (cdr p) m)
                (errorf #f "unexpected cdr value (~s instead of ~s)" (cdr p) m))
              ((car p) n)))))
      (eqv? next n)))
  (equal?
    (let ([x 3])
      (let ([y (map (begin (set! x 14) cons) '())])
        (list x y)))
    '(14 ()))
  (equal?
    (let ([x 3])
      (let ([y (map (begin (set! x 14) list) '() '() '())])
        (list x y)))
    '(14 ()))
  ;; cp0 optimizations for map
  ;; mapping over empty list(s) always returns '()
  (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(map (lambda (x) x) '()) ''()))
  (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(map add1 '() '() '() '()) ''()))
  (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(map (lambda (x) x) '()) ''()))
  (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(map add1 '() '() '() '()) ''()))
  ;; if map is called only for effects, remove the expression only if the procedure
  ;; has the correct arity and can't raise an error
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
      (expand/optimize
        '(begin (#3%map list '(5 4 3 2 1 0)) 7)))
    7)
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
      (expand/optimize
        '(begin (#3%map box? '(5 4 3 2 1 0)) 7)))
    7)
  (not (equivalent-expansion?
         (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
           (expand/optimize
             '(begin (#3%map unbox '(5 4 3 2 1 0)) 7)))
           7))
  (not (equivalent-expansion?
         (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
           (expand/optimize
             '(begin (#3%map cons '(5 4 3 2 1 0)) 7)))
           7))
  ;; map with lambda exp as procedure and lists in the form (list e0 e1 ... en) or '(e0 e1 ... en)
  ;; avoid creating each list and doing the actual map
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(map (lambda (x y z) (apply + x y z)) 
           (list 1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(#2%list 12 15 18))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(map (lambda (x y z) (apply + x y z)) 
           (list 1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(#3%list 12 15 18))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(map (lambda (x y z) (apply + x y z)) 
           '(1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(#2%list 12 15 18))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(map (lambda (x y z) (apply + x y z)) 
           '(1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(#3%list 12 15 18))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(map (lambda (x y z) (apply + x y z)) 
           '(1 2 3)
           '(4 5 6)
           '((7) (8) (9)))))
    '(#2%list 12 15 18))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(map (lambda (x y z) (apply + x y z)) 
           '(1 2 3)
           '(4 5 6)
           '((7) (8) (9)))))
    '(#3%list 12 15 18))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(map (lambda (x y z)
                (string->symbol
                  (apply
                    string-append
                    (map symbol->string (list x y z)))))
              (list 'a 't 'x)
              (list 'b 'u 'y)
              (list 'c 'v 'z))))
    '(#2%list
       (#2%string->symbol (#2%string-append "a" "b" "c"))
       (#2%string->symbol (#2%string-append "t" "u" "v"))
       (#2%string->symbol (#2%string-append "x" "y" "z"))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(map (lambda (x y z)
                (string->symbol
                  (apply
                    string-append
                    (map symbol->string (list x y z)))))
              (list 'a 't 'x)
              (list 'b 'u 'y)
              (list 'c 'v 'z))))
    '(#3%list
       (#3%string->symbol (#3%string-append "a" "b" "c"))
       (#3%string->symbol (#3%string-append "t" "u" "v"))
       (#3%string->symbol (#3%string-append "x" "y" "z"))))
  (equal?
    (with-output-to-string
      (lambda ()
        (pretty-print (map (begin (write 'ab) (lambda (x y) (cons x y)))
                        (begin (write 'a) (list (begin (write 'b) 'c)))
                        (begin (write 'a) (list (begin (write 'b) 'd)))))))
    "ababab((c . d))\n")
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (pretty-print (map (lambda (x y) (cons x y))
                        (list (begin (write 'a) 'c) (begin (write 'b) 'd))
                        (list (begin (write 'x) 'e) (begin (write 'y) 'f))))))
    ; lots of valid possibilities, but make sure we don't interleave and get, e.g., axby
    '("abxy((c . e) (d . f))\n"
      "abyx((c . e) (d . f))\n"
      "baxy((c . e) (d . f))\n"
      "bayx((c . e) (d . f))\n"
      "xyab((c . e) (d . f))\n"
      "yxab((c . e) (d . f))\n"
      "xyba((c . e) (d . f))\n"
      "yxba((c . e) (d . f))\n"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (pretty-print (map (lambda (x y z) (cons* x y z))
                        (begin (write 'a) (list (begin (write 'b) 'g) 'j))
                        (begin (write 'c) (list (begin (write 'd) 'h) 'k))
                        (begin (write 'e) (list (begin (write 'f) 'i) 'l))))))
    '("abcdef((g h . i) (j k . l))\n"
      "abefcd((g h . i) (j k . l))\n"
      "cdabef((g h . i) (j k . l))\n"
      "cdefab((g h . i) (j k . l))\n"
      "efabcd((g h . i) (j k . l))\n"
      "efcdab((g h . i) (j k . l))\n"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (pretty-print (map (lambda (x y z) (cons* x y z))
                        (begin (write 'ab) '(g j))
                        (begin (write 'c) (list (begin (write 'd) 'h) 'k))
                        (begin (write 'e) (list (begin (write 'f) 'i) 'l))))))
    '("abcdef((g h . i) (j k . l))\n"
      "abefcd((g h . i) (j k . l))\n"
      "cdabef((g h . i) (j k . l))\n"
      "cdefab((g h . i) (j k . l))\n"
      "efabcd((g h . i) (j k . l))\n"
      "efcdab((g h . i) (j k . l))\n"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (pretty-print (map (lambda (x y z) (cons* x y z))
                        (begin (write 'a) (list (begin (write 'b) 'g) 'j))
                        (begin (write 'cd) '(h k))
                        (begin (write 'e) (list (begin (write 'f) 'i) 'l))))))
    '("abcdef((g h . i) (j k . l))\n"
      "abefcd((g h . i) (j k . l))\n"
      "cdabef((g h . i) (j k . l))\n"
      "cdefab((g h . i) (j k . l))\n"
      "efabcd((g h . i) (j k . l))\n"
      "efcdab((g h . i) (j k . l))\n"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (pretty-print (map (lambda (x y z) (cons* x y z))
                        (begin (write 'a) (list (begin (write 'b) 'g) 'j))
                        (begin (write 'c) (list (begin (write 'd) 'h) 'k))
                        (begin (write 'ef) '(i l))))))
    '("abcdef((g h . i) (j k . l))\n"
      "abefcd((g h . i) (j k . l))\n"
      "cdabef((g h . i) (j k . l))\n"
      "cdefab((g h . i) (j k . l))\n"
      "efabcd((g h . i) (j k . l))\n"
      "efcdab((g h . i) (j k . l))\n"))
 )

(mat fold-left
 ; next several are from r6rs
  (eqv? (fold-left + 0 '(1 2 3 4 5)) 15)
  (equal?
    (fold-left (lambda (a e) (cons e a)) '() '(1 2 3 4 5))
    '(5 4 3 2 1))
  (eqv?
    (fold-left
      (lambda (count x) (if (odd? x) (+ count 1) count))
      0
      '(3 1 4 1 5 9 2 6 5 3))
    7)
  (eqv?
    (fold-left
      (lambda (max-len s) (max max-len (string-length s)))
      0
      '("longest" "long" "longer"))
    7)
  (equal?
    (fold-left cons '(q) '(a b c))
    '((((q) . a) . b) . c))
  (eqv? 
    (fold-left + 0 '(1 2 3) '(4 5 6))
    21)
  (procedure? (lambda (x) (fold-left x)))
  (procedure? (lambda (x) (fold-left x y)))
  (error? ; nonprocedure
    (fold-left 3 0 '()))
  (error? ; nonprocedure
    (fold-left 3 0 '() '()))
  (error? ; nonprocedure
    (fold-left 3 0 '(a b c)))
  (error? ; improper list
    (fold-left cons 0 'a))
  (error? ; improper list
    (fold-left cons 0 '(a . b)))
  (error? ; cyclic list
    (fold-left cons 0 '#1=(a . #1#)))
  (error? ; length mismatch
    (fold-left list 0 '(a b) '(p q r)))
  (error? ; length mismatch
    (fold-left list 0 '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (fold-left list 0 'a '(a b)))
  (error? ; improper list
    (fold-left list 0 '(a b) 'a))
  (error? ; improper list
    (fold-left list 0 '(a . b) '(a b)))
  (error? ; improper list
    (fold-left list 0 '(a b) '(a . b)))
  (error? ; cyclic list
    (fold-left list 0 '#1# '(a b c)))
  (error? ; cyclic list
    (fold-left list 0 '(a b c) '#1#))
  (error? ; improper list
    (fold-left list 0 'a '(a b) '(1 2)))
  (error? ; improper list
    (fold-left list 0 '(a b) 'a '(1 2)))
  (error? ; improper list
    (fold-left list 0 '(a b) '(1 2) 'a))
  (error? ; improper list
    (fold-left list 0 '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (fold-left list 0 '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (fold-left list 0 '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (fold-left list 0 '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (fold-left list 0 '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (fold-left list 0 '(a b c) '(1 2 3) '#1#))
  (error? ; list altered
    (let ((l (list 1 2 3 4)))
      (fold-left (lambda (a x) ($destroy l 1) (+ x a)) 0 l)))
  (error? ; list altered
    (let ((l (list 1 2 3 4)))
      (fold-left (lambda (a x y) ($destroy l 'q) (list* a x y)) 0 l '(a b c d))))
  (error? ; list altered
    (let ((l (list 1 2 3 4)))
      (fold-left (lambda (a x y) ($destroy l 'q) (cons x y)) 0 l '(a b c d))))
  (error? ; list altered
    (let ((l (list 1 2 3 4)))
      (fold-left (lambda (a x y) ($destroy l 'q) (cons x y)) 0 '(a b c d) l)))
  (error? ; list altered
    (let ((l (list 1 2 3 4 5 6 7)))
      (fold-left (lambda (a x y z) ($destroy l 'q) (list z x y))
        0
        l
        '(a b c d e f g)
        '(p q r s t u v))))
  (error? ; list altered
    (let ((l (list 1 2 3 4 5 6 7)))
      (fold-left (lambda (a x y z) ($destroy l 'q) (list z x y))
        0
        '(a b c d e f g)
        l
        '(p q r s t u v))))
  (error? ; list altered
    (let ((l (list 1 2 3 4 5 6 7)))
      (fold-left (lambda (a x y z) ($destroy l 'q) (list z x y))
        0
        '(a b c d e f g)
        '(p q r s t u v)
        l)))
 )

(mat fold-right
 ; next several are from r6rs
  (eqv? (fold-right + 0 '(1 2 3 4 5)) 15)
  (equal?
    (fold-right cons '() '(1 2 3 4 5))
    '(1 2 3 4 5))
  (equal?
    (fold-right
      (lambda (x l) (if (odd? x) (cons x l) l))
      '()
      '(3 1 4 1 5 9 2 6 5))
    '(3 1 1 5 9 5))
  (equal?
    (fold-right cons '(q) '(a b c))
    '(a b c q))
  (eqv? (fold-right + 0 '(1 2 3) '(4 5 6)) 21)
  (eqv? (fold-right list 75 '()) 75)
  (equal?
    (let ([x 3])
      (let ([y (fold-right (begin (set! x 14) cons) 75 '())])
        (list x y)))
    '(14 75))
  (equal?
    (let ([x 3])
      (let ([y (fold-right (begin (set! x 14) list) 75 '() '() '())])
        (list x y)))
    '(14 75))
  (equal?
    (fold-right
      (lambda (a b) (cons (1+ a) b))
      'q
      '(1 2 3 4 5 6))
    '(2 3 4 5 6 7 . q))
  (equal?
    (fold-right list* 'q '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
    '(1 5 9 2 6 10 3 7 11 4 8 12 . q))
  (equal?
    (let ((x 3))
      (fold-right (lambda (y a) (set! x (1+ x)) (+ x a)) '5 '(a b c d)))
    27)
  (equal?
    (fold-right (lambda (x y z a) (cons (+ x (+ y z)) a)) 'q
      '(1 2 3 4 5) '(11 12 13 14 15) '(21 22 23 24 25))
    '(33 36 39 42 45 . q))
 ; make sure compiler doesn't bomb w/two few args
  (procedure? (lambda (x) (fold-right x)))
  (procedure? (lambda (x) (fold-right x y)))
  (error? ; nonprocedure
    (fold-right 3 0 '()))
  (error? ; nonprocedure
    (fold-right 3 0 '() '()))
  (error? ; nonprocedure
    (fold-right 3 0 '(a b c)))
  (error? ; improper list
    (fold-right list 0 'a))
  (error? ; improper list
    (fold-right list 0 '(a . b)))
  (error? ; cyclic list
    (fold-right list 0 '#1=(a . #1#)))
  (error? ; length mismatch
    (fold-right list 0 '(a b) '(p q r)))
  (error? ; length mismatch
    (fold-right list 0 '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (fold-right list 0 'a '(a b)))
  (error? ; improper list
    (fold-right list 0 '(a b) 'a))
  (error? ; improper list
    (fold-right list 0 '(a . b) '(a b)))
  (error? ; improper list
    (fold-right list 0 '(a b) '(a . b)))
  (error? ; cyclic list
    (fold-right list 0 '#1# '(a b c)))
  (error? ; cyclic list
    (fold-right list 0 '(a b c) '#1#))
  (error? ; improper list
    (fold-right list 0 'a '(a b) '(1 2)))
  (error? ; improper list
    (fold-right list 0 '(a b) 'a '(1 2)))
  (error? ; improper list
    (fold-right list 0 '(a b) '(1 2) 'a))
  (error? ; improper list
    (fold-right list 0 '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (fold-right list 0 '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (fold-right list 0 '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (fold-right list 0 '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (fold-right list 0 '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (fold-right list 0 '(a b c) '(1 2 3) '#1#))
  (equal?
    (let ((l (list 1 2 3 4)))
      (fold-right (lambda (x a) ($destroy l 1) (cons (* x x) a)) 'q l))
    '(1 4 9 16 . q))
  (equal?
    (let ((l (list 1 2 3 4)))
      (fold-right
        (lambda (x y a) ($destroy l y) (cons (cons x y) a))
        'q
        l
        '(a b c d)))
    '((1 . a) (2 . b) (3 . c) (4 . d) . q))
  (equal?
    (let ((l (list 1 2 3 4)))
      (fold-right
        (lambda (x y a) ($destroy l '()) (cons (cons x y) a))
        'q
        l
        '(a b c d)))
    '((1 . a) (2 . b) (3 . c) (4 . d) . q))
  (equal?
    (let ((l (list 1 2 3 4)))
      (fold-right
        (lambda (x y a) ($destroy l y) (cons (cons x y) a))
        'q
        '(a b c d)
        l))
    '((a . 1) (b . 2) (c . 3) (d . 4) . q))
  (equal?
    (let ((l (list 1 2 3 4 5 6 7)))
      (fold-right (lambda (x y z a) ($destroy l '()) (cons (list z x y) a))
           'q
           l
           '(a b c d e f g)
           '(p q r s t u v)))
    '((p 1 a) (q 2 b) (r 3 c) (s 4 d) (t 5 e) (u 6 f) (v 7 g) . q))
  (equal?
    (let ((l (list 1 2 3 4 5 6 7)))
      (fold-right (lambda (x y z a) ($destroy l '()) (cons (list z x y) a))
           'q
           '(a b c d e f g)
           l
           '(p q r s t u v)))
    '((p a 1) (q b 2) (r c 3) (s d 4) (t e 5) (u f 6) (v g 7) . q))
  (equal?
    (let ((l (list 1 2 3 4 5 6 7)))
      (fold-right (lambda (x y z a) ($destroy l '()) (cons (list z x y) a))
           'q
           '(a b c d e f g)
           '(p q r s t u v)
           l))
    '((1 a p) (2 b q) (3 c r) (4 d s) (5 e t) (6 f u) (7 g v) . q))
 )

(mat for-each
   (let ((x 0))
      (for-each (lambda (y) (set! x (1- x))) '(1 2 3 4 5 6 7))
      (= x -7))
   (let ((x 0))
      (for-each (lambda (y) (set! x (1- x))) '())
      (= x 0))
   (let ((x '()))
      (for-each (lambda (y) (set! x (cons y x))) '(a b c d))
      (equal? x '(d c b a)))
   (let ((x 0))
      (for-each
         (lambda (y z) (set! x (+ x (- y z))))
         '(4 5 6)
         '(3 2 1))
      (= x 9))
   (let ((x 0))
      (for-each
         (lambda (y z w) (set! x (+ x (+ y (- z w)))))
         '(-1 -2 -3)
         '(4 5 6)
         '(3 2 1))
      (= x 3))
   (let ((x 0))
      (for-each
         (lambda (y z w) (set! x (+ x (+ y (- z w)))))
         '()
         '()
         '())
      (= x 0))
 ; check for proper tail recursion
   (equal?
     (list
       (let ([s (statistics)])
         (let ([k 100000] [ls '(a b c)])
           (let ([n k] [m 0])
             (define (f) (unless (fx= n 0) (for-each foo ls)))
             (define (foo x)
               (set! m (+ m 1))
               (when (eq? x (car (last-pair ls)))
                 (set! n (- n 1))
                 (f)
                 17)) ; blow tail recursion here
             (f)
             (list (> (sstats-bytes (sstats-difference (statistics) s))
                      10000)
                   (eqv? n 0)
                   (eqv? m (* k (length ls)))))))
       (let ([s (statistics)])
         (let ([k 100000] [ls '(a b c)])
           (let ([n k] [m 0])
             (define (f) (unless (fx= n 0) (for-each foo ls)))
             (define (foo x)
               (set! m (+ m 1))
               (when (eq? x (car (last-pair ls)))
                 (set! n (- n 1))
                 (f)))
             (f)
             (list (<= 0
                      (sstats-bytes (sstats-difference (statistics) s))
                      1000)
                   (eqv? n 0)
                   (eqv? m (* k (length ls))))))))
     '((#t #t #t) (#t #t #t)))
   (eqv?
     (for-each (lambda (x y) (+ x y)) '(1 2 3) '(4 5 6))
     9)
   (let-values ([() (for-each
                      (lambda (x y) (if (eqv? x 3) (values) (+ x y)))
                      '(1 2 3)
                      '(4 5 6))])
     #t)
   (equal?
     (let-values ([(a b) (for-each
                           (lambda (x y) (if (eqv? x 3) (values x y) (+ x y)))
                           '(1 2 3)
                           '(4 5 6))])
       (list a b))
     '(3 6))

 ; make sure compiler doesn't bomb w/two few args
  (procedure? (lambda (x) (for-each x)))
  (error? ; nonprocedure
    (for-each 3 '()))
  (error? ; nonprocedure
    (for-each 3 '() '()))
  (error? ; nonprocedure
    (for-each 3 '(a b c)))
  (error? ; nonprocedure
    (parameterize ([optimize-level 3])
      (eval '(#2%for-each 3 '(a b c)))))
  (error? ; nonprocedure
    (parameterize ([optimize-level 3])
      (eval
        '(let ()
           (define (f p b)
             (unbox b)
             (#2%for-each p (if (box? b) '() '(1 2 3)))
             (list p (procedure? p)))
           (f 7 (box 0))))))
  (error? ; improper list
    (for-each pretty-print 'a))
  (error? ; improper list
    (for-each pretty-print '(a . b)))
  (error? ; cyclic list
    (for-each pretty-print '#1=(a . #1#)))
  (error? ; length mismatch
    (for-each (lambda (x y) (write (list x y))) '(a b) '(p q r)))
  (error? ; length mismatch
    (for-each (lambda (x y z) (write (list x y z))) '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (for-each values 'a '(a b)))
  (error? ; improper list
    (for-each values '(a b) 'a))
  (error? ; improper list
    (for-each values '(a . b) '(a b)))
  (error? ; improper list
    (for-each values '(a b) '(a . b)))
  (error? ; cyclic list
    (for-each values '#1# '(a b c)))
  (error? ; cyclic list
    (for-each values '(a b c) '#1#))
  (error? ; improper list
    (for-each values 'a '(a b) '(1 2)))
  (error? ; improper list
    (for-each values '(a b) 'a '(1 2)))
  (error? ; improper list
    (for-each values '(a b) '(1 2) 'a))
  (error? ; improper list
    (for-each values '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (for-each values '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (for-each values '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (for-each values '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (for-each values '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (for-each values '(a b c) '(1 2 3) '#1#))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x) (set-cdr! (cdr l) 1)) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x) (set-cdr! (cddr l) 1)) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y) (set-cdr! (cdr l) y)) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y) (set-cdr! (cddr l) y)) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y) (set-cdr! (cdr l) y)) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y) (set-cdr! (cddr l) y)) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y z) (set-cdr! (cdr l) '())) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y z) (set-cdr! (cddr l) '())) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y z) (set-cdr! (cdr l) '())) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y z) (set-cdr! (cddr l) '())) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y z) (set-cdr! (cdr l) '())) '(a b c d) '(p q r s) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-each (lambda (x y z) (set-cdr! (cddr l) '())) '(a b c d) '(p q r s) l)))
  (begin
    (define ($for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (for-each p '())
        (for-each p '() x1)
        (for-each p '() x1 x2)
        (for-each p '() x1 x2 x3)
        (for-each p '() x1 x2 x3 x4)
        (for-each p '() x1 x2 x3 x4 x5)
        (for-each p x1 '())
        (for-each p x1 '() x2)
        (for-each p x1 '() x2 x3)
        (for-each p x1 '() x2 x3 x4)
        (for-each p x1 '() x2 x3 x4 x5)
        (for-each p x1 x2 '())
        (for-each p x1 x2 '() x3)
        (for-each p x1 x2 '() x3 x4)
        (for-each p x1 x2 '() x3 x4 x5)
        (for-each p x1 x2 x3 '())
        (for-each p x1 x2 x3 '() x4)
        (for-each p x1 x2 x3 '() x4 x5)
        (for-each p x1 x2 x3 x4 '())
        (for-each p x1 x2 x3 x4 '() x5)
        (for-each p x1 x2 x3 x4 x5 '())))
    (procedure? $for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($for-each-f1 q '() '() '() '() '())
      (reverse ls))
    '())
  (begin
    (define ($for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (for-each p '(a))
        (for-each p '(a) x1)
        (for-each p '(a) x1 x2)
        (for-each p '(a) x1 x2 x3)
        (for-each p '(a) x1 x2 x3 x4)
        (for-each p '(a) x1 x2 x3 x4 x5)
        (for-each p x1 '(a))
        (for-each p x1 '(a) x2)
        (for-each p x1 '(a) x2 x3)
        (for-each p x1 '(a) x2 x3 x4)
        (for-each p x1 '(a) x2 x3 x4 x5)
        (for-each p x1 x2 '(a))
        (for-each p x1 x2 '(a) x3)
        (for-each p x1 x2 '(a) x3 x4)
        (for-each p x1 x2 '(a) x3 x4 x5)
        (for-each p x1 x2 x3 '(a))
        (for-each p x1 x2 x3 '(a) x4)
        (for-each p x1 x2 x3 '(a) x4 x5)
        (for-each p x1 x2 x3 x4 '(a))
        (for-each p x1 x2 x3 x4 '(a) x5)
        (for-each p x1 x2 x3 x4 x5 '(a))))
    (procedure? $for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($for-each-f1 q '(1) '(f) '(k) '(p) '(u))
      (reverse ls))
    '((a) (1 a) (f 1 a) (k f 1 a) (p k f 1 a) (u p k f 1 a)
      (a 1) (f a 1) (k f a 1) (p k f a 1) (u p k f a 1)
      (a f 1) (k a f 1) (p k a f 1) (u p k a f 1) (a k f 1)
      (p a k f 1) (u p a k f 1) (a p k f 1) (u a p k f 1)
      (a u p k f 1)))
  (begin
    (define ($for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (for-each p '(a b))
        (for-each p '(a b) x1)
        (for-each p '(a b) x1 x2)
        (for-each p '(a b) x1 x2 x3)
        (for-each p '(a b) x1 x2 x3 x4)
        (for-each p '(a b) x1 x2 x3 x4 x5)
        (for-each p x1 '(a b))
        (for-each p x1 '(a b) x2)
        (for-each p x1 '(a b) x2 x3)
        (for-each p x1 '(a b) x2 x3 x4)
        (for-each p x1 '(a b) x2 x3 x4 x5)
        (for-each p x1 x2 '(a b))
        (for-each p x1 x2 '(a b) x3)
        (for-each p x1 x2 '(a b) x3 x4)
        (for-each p x1 x2 '(a b) x3 x4 x5)
        (for-each p x1 x2 x3 '(a b))
        (for-each p x1 x2 x3 '(a b) x4)
        (for-each p x1 x2 x3 '(a b) x4 x5)
        (for-each p x1 x2 x3 x4 '(a b))
        (for-each p x1 x2 x3 x4 '(a b) x5)
        (for-each p x1 x2 x3 x4 x5 '(a b))))
    (procedure? $for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($for-each-f1 q '(1 2) '(f g) '(k l) '(p q) '(u v))
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
    (define ($for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (for-each p '(a b c))
        (for-each p '(a b c) x1)
        (for-each p '(a b c) x1 x2)
        (for-each p '(a b c) x1 x2 x3)
        (for-each p '(a b c) x1 x2 x3 x4)
        (for-each p '(a b c) x1 x2 x3 x4 x5)
        (for-each p x1 '(a b c))
        (for-each p x1 '(a b c) x2)
        (for-each p x1 '(a b c) x2 x3)
        (for-each p x1 '(a b c) x2 x3 x4)
        (for-each p x1 '(a b c) x2 x3 x4 x5)
        (for-each p x1 x2 '(a b c))
        (for-each p x1 x2 '(a b c) x3)
        (for-each p x1 x2 '(a b c) x3 x4)
        (for-each p x1 x2 '(a b c) x3 x4 x5)
        (for-each p x1 x2 x3 '(a b c))
        (for-each p x1 x2 x3 '(a b c) x4)
        (for-each p x1 x2 x3 '(a b c) x4 x5)
        (for-each p x1 x2 x3 x4 '(a b c))
        (for-each p x1 x2 x3 x4 '(a b c) x5)
        (for-each p x1 x2 x3 x4 x5 '(a b c))))
    (procedure? $for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($for-each-f1 q '(1 2 3) '(f g h) '(k l m) '(p q r) '(u v w))
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
    (define ($for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (for-each p '(a b c d))
        (for-each p '(a b c d) x1)
        (for-each p '(a b c d) x1 x2)
        (for-each p '(a b c d) x1 x2 x3)
        (for-each p '(a b c d) x1 x2 x3 x4)
        (for-each p '(a b c d) x1 x2 x3 x4 x5)
        (for-each p x1 '(a b c d))
        (for-each p x1 '(a b c d) x2)
        (for-each p x1 '(a b c d) x2 x3)
        (for-each p x1 '(a b c d) x2 x3 x4)
        (for-each p x1 '(a b c d) x2 x3 x4 x5)
        (for-each p x1 x2 '(a b c d))
        (for-each p x1 x2 '(a b c d) x3)
        (for-each p x1 x2 '(a b c d) x3 x4)
        (for-each p x1 x2 '(a b c d) x3 x4 x5)
        (for-each p x1 x2 x3 '(a b c d))
        (for-each p x1 x2 x3 '(a b c d) x4)
        (for-each p x1 x2 x3 '(a b c d) x4 x5)
        (for-each p x1 x2 x3 x4 '(a b c d))
        (for-each p x1 x2 x3 x4 '(a b c d) x5)
        (for-each p x1 x2 x3 x4 x5 '(a b c d))))
    (procedure? $for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($for-each-f1 q '(1 2 3 4) '(f g h i) '(k l m n) '(p q r s) '(u v w x))
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
    (define ($for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (for-each p '(a b c d e))
        (for-each p '(a b c d e) x1)
        (for-each p '(a b c d e) x1 x2)
        (for-each p '(a b c d e) x1 x2 x3)
        (for-each p '(a b c d e) x1 x2 x3 x4)
        (for-each p '(a b c d e) x1 x2 x3 x4 x5)
        (for-each p x1 '(a b c d e))
        (for-each p x1 '(a b c d e) x2)
        (for-each p x1 '(a b c d e) x2 x3)
        (for-each p x1 '(a b c d e) x2 x3 x4)
        (for-each p x1 '(a b c d e) x2 x3 x4 x5)
        (for-each p x1 x2 '(a b c d e))
        (for-each p x1 x2 '(a b c d e) x3)
        (for-each p x1 x2 '(a b c d e) x3 x4)
        (for-each p x1 x2 '(a b c d e) x3 x4 x5)
        (for-each p x1 x2 x3 '(a b c d e))
        (for-each p x1 x2 x3 '(a b c d e) x4)
        (for-each p x1 x2 x3 '(a b c d e) x4 x5)
        (for-each p x1 x2 x3 x4 '(a b c d e))
        (for-each p x1 x2 x3 x4 '(a b c d e) x5)
        (for-each p x1 x2 x3 x4 x5 '(a b c d e))))
    (procedure? $for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($for-each-f1 q '(1 2 3 4 5) '(f g h i j) '(k l m n o) '(p q r s t) '(u v w x y))
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
  ;; cp0 optimizations for for-each
  ;; for-each with an empty list(s) always (void)
  (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(for-each (lambda (x) x) '()) '(#2%void)))
  (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(for-each add1 '() '() '() '()) '(#2%void)))
  (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(for-each (lambda (x) x) '()) '(#2%void)))
  (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
    (test-cp0-expansion equal? '(for-each add1 '() '() '() '()) '(#2%void)))
  ;; remove for-each the expression only if the procedure
  ;; has the correct arity and can't raise an error
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
      (expand/optimize
        '(#3%for-each list '(5 4 3 2 1 0))))
    '(#2%void))
  (equivalent-expansion?
    (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
      (expand/optimize
        '(#3%for-each box? '(5 4 3 2 1 0))))
    '(#2%void))
  (not (equivalent-expansion?
         (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
           (expand/optimize
             '(#3%for-each unbox '(5 4 3 2 1 0))))
           '(#2%void)))
  (not (equivalent-expansion?
         (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f] [optimize-level 2])
           (expand/optimize
             '(#3%for-each cons '(5 4 3 2 1 0))))
           '(#2%void)))
  ;; for-each with lambda exp as procedure and lists in the form (list e0 e1 ... en) or '(e0 e1 ... en)
  ;; avoid creating each list and doing the actual for-each
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(for-each (lambda (x y z) (display (apply + x y z))) 
           (list 1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(begin (#2%display 12) (#2%display 15) (#2%display 18)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(for-each (lambda (x y z) (display (apply + x y z))) 
           (list 1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(begin (#3%display 12) (#3%display 15) (#3%display 18)))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(for-each (lambda (x y z) (display (apply + x y z))) 
           '(1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(begin (#2%display 12) (#2%display 15) (#2%display 18)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(for-each (lambda (x y z) (display (apply + x y z))) 
           '(1 2 3)
           (list 4 5 6)
           (list '(7) '(8) '(9)))))
    '(begin (#3%display 12) (#3%display 15) (#3%display 18)))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(for-each (lambda (x y z) (display (apply + x y z))) 
           '(1 2 3)
           '(4 5 6)
           '((7) (8) (9)))))
    '(begin (#2%display 12) (#2%display 15) (#2%display 18)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize 
        '(for-each (lambda (x y z) (display (apply + x y z))) 
           '(1 2 3)
           '(4 5 6)
           '((7) (8) (9)))))
    '(begin (#3%display 12) (#3%display 15) (#3%display 18)))
  (equal?
    (with-output-to-string
      (lambda ()
        (for-each (begin (write 'ab) (lambda (x y) (write (cons x y))))
          (begin (write 'a) (list (begin (write 'b) 'c)))
          (begin (write 'a) (list (begin (write 'b) 'd))))))
    "ababab(c . d)")
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (for-each (lambda (x y) (write (cons x y)))
          (list (begin (write 'a) 'c) (begin (write 'b) 'd))
          (list (begin (write 'x) 'e) (begin (write 'y) 'f)))))
    ; lots of valid possibilities, but make sure we don't interleave and get, e.g., axby
    '("abxy(c . e)(d . f)"
      "abyx(c . e)(d . f)"
      "baxy(c . e)(d . f)"
      "bayx(c . e)(d . f)"
      "xyab(c . e)(d . f)"
      "yxab(c . e)(d . f)"
      "xyba(c . e)(d . f)"
      "yxba(c . e)(d . f)"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (for-each (lambda (x y z) (write (cons* x y z)))
          (begin (write 'a) (list (begin (write 'b) 'g) 'j))
          (begin (write 'c) (list (begin (write 'd) 'h) 'k))
          (begin (write 'e) (list (begin (write 'f) 'i) 'l)))))
    '("abcdef(g h . i)(j k . l)"
      "abefcd(g h . i)(j k . l)"
      "cdabef(g h . i)(j k . l)"
      "cdefab(g h . i)(j k . l)"
      "efabcd(g h . i)(j k . l)"
      "efcdab(g h . i)(j k . l)"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (for-each (lambda (x y z) (write (cons* x y z)))
          (begin (write 'ab) '(g j))
          (begin (write 'c) (list (begin (write 'd) 'h) 'k))
          (begin (write 'e) (list (begin (write 'f) 'i) 'l)))))
    '("abcdef(g h . i)(j k . l)"
      "abefcd(g h . i)(j k . l)"
      "cdabef(g h . i)(j k . l)"
      "cdefab(g h . i)(j k . l)"
      "efabcd(g h . i)(j k . l)"
      "efcdab(g h . i)(j k . l)"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (for-each (lambda (x y z) (write (cons* x y z)))
          (begin (write 'a) (list (begin (write 'b) 'g) 'j))
          (begin (write 'cd) '(h k))
          (begin (write 'e) (list (begin (write 'f) 'i) 'l)))))
    '("abcdef(g h . i)(j k . l)"
      "abefcd(g h . i)(j k . l)"
      "cdabef(g h . i)(j k . l)"
      "cdefab(g h . i)(j k . l)"
      "efabcd(g h . i)(j k . l)"
      "efcdab(g h . i)(j k . l)"))
  ((lambda (x ls) (and (member x ls) #t))
    (with-output-to-string
      (lambda ()
        (for-each (lambda (x y z) (write (cons* x y z)))
          (begin (write 'a) (list (begin (write 'b) 'g) 'j))
          (begin (write 'c) (list (begin (write 'd) 'h) 'k))
          (begin (write 'ef) '(i l)))))
    '("abcdef(g h . i)(j k . l)"
      "abefcd(g h . i)(j k . l)"
      "cdabef(g h . i)(j k . l)"
      "cdefab(g h . i)(j k . l)"
      "efabcd(g h . i)(j k . l)"
      "efcdab(g h . i)(j k . l)"))
 )

(mat ormap
   (ormap symbol? '(a b c d))
   (ormap symbol? '(a 1 2 3))
   (ormap symbol? '(1 2 3 a))
   (not (ormap symbol? '()))
   (not (ormap symbol? '(1 2 3 4)))
   (ormap = '(1 2 3 4) '(1.1 2.0 3.1 4.1))
   (not (ormap = '(1 2 3 4) '(1.1 2.2 3.3 4.4)))
   (eqv? (ormap 1+ '(1 2 3 4)) 2)
   (eqv? (ormap + '(1 2 3) '(3 4 5)) 4)
   (ormap (lambda (x y z) (= (+ x y) z))
          '(1 2 3 4)
          '(1.2 2.3 3.4 4.5)
          '(2.3 4.4 6.4 8.6))
   (not (ormap (lambda (x y z) (= (+ x y) z))
               '(1 2 3 4)
               '(1.2 2.3 3.4 4.5)
               '(2.3 4.4 6.5 8.6)))
   (not (ormap (lambda (x y z) #t) '() '() '()))
 ; make sure compiler doesn't bomb w/two few args
  (procedure? (lambda (x) (ormap x)))
  (error? ; nonprocedure
    (ormap 3 '()))
  (error? ; nonprocedure
    (ormap 3 '() '()))
  (error? ; nonprocedure
    (ormap 3 '(a b c)))
  (error? ; improper list
    (ormap not 'a))
  (error? ; improper list
    (ormap not '(a . b)))
  (error? ; cyclic list
    (ormap not '#1=(a . #1#)))
  (error? ; length mismatch
    (ormap (lambda (x y) #f) '(a b) '(p q r)))
  (error? ; length mismatch
    (ormap (lambda (x y z) #f) '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (ormap (lambda (x y) #f) 'a '(a b)))
  (error? ; improper list
    (ormap (lambda (x y) #f) '(a b) 'a))
  (error? ; improper list
    (ormap (lambda (x y) #f) '(a . b) '(a b)))
  (error? ; improper list
    (ormap (lambda (x y) #f) '(a b) '(a . b)))
  (error? ; cyclic list
    (ormap (lambda (x y) #f) '#1# '(a b c)))
  (error? ; cyclic list
    (ormap (lambda (x y) #f) '(a b c) '#1#))
  (error? ; improper list
    (ormap (lambda (x y z) #f) 'a '(a b) '(1 2)))
  (error? ; improper list
    (ormap (lambda (x y z) #f) '(a b) 'a '(1 2)))
  (error? ; improper list
    (ormap (lambda (x y z) #f) '(a b) '(1 2) 'a))
  (error? ; improper list
    (ormap (lambda (x y z) #f) '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (ormap (lambda (x y z) #f) '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (ormap (lambda (x y z) #f) '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (ormap (lambda (x y z) #f) '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (ormap (lambda (x y z) #f) '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (ormap (lambda (x y z) #f) '(a b c) '(1 2 3) '#1#))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x) (set-cdr! (cdr l) 1) #f) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x) (set-cdr! (cddr l) 1) #f) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y) (set-cdr! (cdr l) y) #f) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y) (set-cdr! (cddr l) y) #f) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y) (set-cdr! (cdr l) y) #f) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y) (set-cdr! (cddr l) y) #f) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y z) (set-cdr! (cdr l) '()) #f) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y z) (set-cdr! (cddr l) '()) #f) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y z) (set-cdr! (cdr l) '()) #f) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y z) (set-cdr! (cddr l) '()) #f) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y z) (set-cdr! (cdr l) '()) #f) '(a b c d) '(p q r s) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (ormap (lambda (x y z) (set-cdr! (cddr l) '()) #f) '(a b c d) '(p q r s) l)))
 )

(mat andmap
   (andmap symbol? '(a b c d))
   (not (andmap symbol? '(a 1 2 3)))
   (not (andmap symbol? '(1 2 3 a)))
   (andmap symbol? '())
   (not (andmap symbol? '(1 2 3 4)))
   (andmap = '(1 2 3 4) '(1.0 2.0 3.0 4.0))
   (not (andmap = '(1 2 3 4) '(1.0 2.0 3.3 4.0)))
   (eqv? (andmap 1+ '(1 2 3 4)) 5)
   (eqv? (andmap + '(1 2 3) '(3 4 5)) 8)
   (andmap (lambda (x y z) (= (+ x y) z))
          '(1 2 3 4)
          '(1.2 2.3 3.4 4.5)
          '(2.2 4.3 6.4 8.5))
   (not (andmap (lambda (x y z) (= (+ x y) z))
               '(1 2 3 4)
               '(1.2 2.3 3.4 4.5)
               '(2.2 4.3 6.5 8.5)))
   (eq? (andmap (lambda (x y z) #t) '() '() '()) #t)
 ; make sure compiler doesn't bomb w/two few args
  (procedure? (lambda (x) (andmap x)))
  (error? ; nonprocedure
    (andmap 3 '()))
  (error? ; nonprocedure
    (andmap 3 '() '()))
  (error? ; nonprocedure
    (andmap 3 '(a b c)))
  (error? ; improper list
    (andmap values 'a))
  (error? ; improper list
    (andmap values '(a . b)))
  (error? ; cyclic list
    (andmap values '#1=(a . #1#)))
  (error? ; length mismatch
    (andmap (lambda (x y) #t) '(a b) '(p q r)))
  (error? ; length mismatch
    (andmap (lambda (x y z) #t) '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (andmap (lambda (x y) #t) 'a '(a b)))
  (error? ; improper list
    (andmap (lambda (x y) #t) '(a b) 'a))
  (error? ; improper list
    (andmap (lambda (x y) #t) '(a . b) '(a b)))
  (error? ; improper list
    (andmap (lambda (x y) #t) '(a b) '(a . b)))
  (error? ; cyclic list
    (andmap (lambda (x y) #t) '#1# '(a b c)))
  (error? ; cyclic list
    (andmap (lambda (x y) #t) '(a b c) '#1#))
  (error? ; improper list
    (andmap (lambda (x y z) #t) 'a '(a b) '(1 2)))
  (error? ; improper list
    (andmap (lambda (x y z) #t) '(a b) 'a '(1 2)))
  (error? ; improper list
    (andmap (lambda (x y z) #t) '(a b) '(1 2) 'a))
  (error? ; improper list
    (andmap (lambda (x y z) #t) '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (andmap (lambda (x y z) #t) '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (andmap (lambda (x y z) #t) '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (andmap (lambda (x y z) #t) '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (andmap (lambda (x y z) #t) '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (andmap (lambda (x y z) #t) '(a b c) '(1 2 3) '#1#))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x) (set-cdr! (cdr l) 1) #t) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x) (set-cdr! (cddr l) 1) #t) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y) (set-cdr! (cdr l) y) #t) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y) (set-cdr! (cddr l) y) #t) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y) (set-cdr! (cdr l) y) #t) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y) (set-cdr! (cddr l) y) #t) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y z) (set-cdr! (cdr l) '()) #t) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y z) (set-cdr! (cddr l) '()) #t) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y z) (set-cdr! (cdr l) '()) #t) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y z) (set-cdr! (cddr l) '()) #t) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y z) (set-cdr! (cdr l) '()) #t) '(a b c d) '(p q r s) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (andmap (lambda (x y z) (set-cdr! (cddr l) '()) #t) '(a b c d) '(p q r s) l)))
 )

(mat exists
   (exists symbol? '(a b c d))
   (exists symbol? '(a 1 2 3))
   (exists symbol? '(1 2 3 a))
   (not (exists symbol? '()))
   (not (exists symbol? '(1 2 3 4)))
   (exists = '(1 2 3 4) '(1.1 2.0 3.1 4.1))
   (not (exists = '(1 2 3 4) '(1.1 2.2 3.3 4.4)))
   (eqv? (exists 1+ '(1 2 3 4)) 2)
   (eqv? (exists + '(1 2 3) '(3 4 5)) 4)
   (exists (lambda (x y z) (= (+ x y) z))
          '(1 2 3 4)
          '(1.2 2.3 3.4 4.5)
          '(2.3 4.4 6.4 8.6))
   (not (exists (lambda (x y z) (= (+ x y) z))
               '(1 2 3 4)
               '(1.2 2.3 3.4 4.5)
               '(2.3 4.4 6.5 8.6)))
   (not (exists (lambda (x y z) #t) '() '() '()))
 ; make sure compiler doesn't bomb w/two few args
  (procedure? (lambda (x) (exists x)))
  (error? ; nonprocedure
    (exists 3 '()))
  (error? ; nonprocedure
    (exists 3 '() '()))
  (error? ; nonprocedure
    (exists 3 '(a b c)))
  (error? ; improper list
    (exists not 'a))
  (error? ; improper list
    (exists not '(a . b)))
  (error? ; cyclic list
    (exists not '#1=(a . #1#)))
  (error? ; length mismatch
    (exists (lambda (x y) #f) '(a b) '(p q r)))
  (error? ; length mismatch
    (exists (lambda (x y z) #f) '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (exists (lambda (x y) #f) 'a '(a b)))
  (error? ; improper list
    (exists (lambda (x y) #f) '(a b) 'a))
  (error? ; improper list
    (exists (lambda (x y) #f) '(a . b) '(a b)))
  (error? ; improper list
    (exists (lambda (x y) #f) '(a b) '(a . b)))
  (error? ; cyclic list
    (exists (lambda (x y) #f) '#1# '(a b c)))
  (error? ; cyclic list
    (exists (lambda (x y) #f) '(a b c) '#1#))
  (error? ; improper list
    (exists (lambda (x y z) #f) 'a '(a b) '(1 2)))
  (error? ; improper list
    (exists (lambda (x y z) #f) '(a b) 'a '(1 2)))
  (error? ; improper list
    (exists (lambda (x y z) #f) '(a b) '(1 2) 'a))
  (error? ; improper list
    (exists (lambda (x y z) #f) '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (exists (lambda (x y z) #f) '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (exists (lambda (x y z) #f) '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (exists (lambda (x y z) #f) '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (exists (lambda (x y z) #f) '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (exists (lambda (x y z) #f) '(a b c) '(1 2 3) '#1#))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x) (set-cdr! (cdr l) 1) #f) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x) (set-cdr! (cddr l) 1) #f) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y) (set-cdr! (cdr l) y) #f) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y) (set-cdr! (cddr l) y) #f) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y) (set-cdr! (cdr l) y) #f) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y) (set-cdr! (cddr l) y) #f) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y z) (set-cdr! (cdr l) '()) #f) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y z) (set-cdr! (cddr l) '()) #f) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y z) (set-cdr! (cdr l) '()) #f) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y z) (set-cdr! (cddr l) '()) #f) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y z) (set-cdr! (cdr l) '()) #f) '(a b c d) '(p q r s) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (exists (lambda (x y z) (set-cdr! (cddr l) '()) #f) '(a b c d) '(p q r s) l)))
 )

(mat for-all
   (for-all symbol? '(a b c d))
   (not (for-all symbol? '(a 1 2 3)))
   (not (for-all symbol? '(1 2 3 a)))
   (for-all symbol? '())
   (not (for-all symbol? '(1 2 3 4)))
   (for-all = '(1 2 3 4) '(1.0 2.0 3.0 4.0))
   (not (for-all = '(1 2 3 4) '(1.0 2.0 3.3 4.0)))
   (eqv? (for-all 1+ '(1 2 3 4)) 5)
   (eqv? (for-all + '(1 2 3) '(3 4 5)) 8)
   (for-all (lambda (x y z) (= (+ x y) z))
          '(1 2 3 4)
          '(1.2 2.3 3.4 4.5)
          '(2.2 4.3 6.4 8.5))
   (not (for-all (lambda (x y z) (= (+ x y) z))
               '(1 2 3 4)
               '(1.2 2.3 3.4 4.5)
               '(2.2 4.3 6.5 8.5)))
   (eq? (for-all (lambda (x y z) #t) '() '() '()) #t)
 ; make sure compiler doesn't bomb w/two few args
  (procedure? (lambda (x) (for-all x)))
  (error? ; nonprocedure
    (for-all 3 '()))
  (error? ; nonprocedure
    (for-all 3 '() '()))
  (error? ; nonprocedure
    (for-all 3 '(a b c)))
  (error? ; improper list
    (for-all values 'a))
  (error? ; improper list
    (for-all values '(a . b)))
  (error? ; cyclic list
    (for-all values '#1=(a . #1#)))
  (error? ; length mismatch
    (for-all (lambda (x y) #t) '(a b) '(p q r)))
  (error? ; length mismatch
    (for-all (lambda (x y z) #t) '(1 2) '(a b) '(p q r)))
  (error? ; improper list
    (for-all (lambda (x y) #t) 'a '(a b)))
  (error? ; improper list
    (for-all (lambda (x y) #t) '(a b) 'a))
  (error? ; improper list
    (for-all (lambda (x y) #t) '(a . b) '(a b)))
  (error? ; improper list
    (for-all (lambda (x y) #t) '(a b) '(a . b)))
  (error? ; cyclic list
    (for-all (lambda (x y) #t) '#1# '(a b c)))
  (error? ; cyclic list
    (for-all (lambda (x y) #t) '(a b c) '#1#))
  (error? ; improper list
    (for-all (lambda (x y z) #t) 'a '(a b) '(1 2)))
  (error? ; improper list
    (for-all (lambda (x y z) #t) '(a b) 'a '(1 2)))
  (error? ; improper list
    (for-all (lambda (x y z) #t) '(a b) '(1 2) 'a))
  (error? ; improper list
    (for-all (lambda (x y z) #t) '(a . b) '(a b) '(1 2)))
  (error? ; improper list
    (for-all (lambda (x y z) #t) '(a b) '(a . b) '(1 2)))
  (error? ; improper list
    (for-all (lambda (x y z) #t) '(a b) '(1 2) '(a . b)))
  (error? ; cyclic list
    (for-all (lambda (x y z) #t) '#1# '(a b c) '(1 2 3)))
  (error? ; cyclic list
    (for-all (lambda (x y z) #t) '(a b c) '#1# '(1 2 3)))
  (error? ; cyclic list
    (for-all (lambda (x y z) #t) '(a b c) '(1 2 3) '#1#))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x) (set-cdr! (cdr l) 1) #t) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x) (set-cdr! (cddr l) 1) #t) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y) (set-cdr! (cdr l) y) #t) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y) (set-cdr! (cddr l) y) #t) l '(a b c d))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y) (set-cdr! (cdr l) y) #t) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y) (set-cdr! (cddr l) y) #t) '(a b c d) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y z) (set-cdr! (cdr l) '()) #t) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y z) (set-cdr! (cddr l) '()) #t) l '(a b c d) '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y z) (set-cdr! (cdr l) '()) #t) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y z) (set-cdr! (cddr l) '()) #t) '(a b c d) l '(p q r s))))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y z) (set-cdr! (cdr l) '()) #t) '(a b c d) '(p q r s) l)))
  (error? ; input list mutated
    (let ((l (list 1 2 3 4)))
      (for-all (lambda (x y z) (set-cdr! (cddr l) '()) #t) '(a b c d) '(p q r s) l)))
 )

(mat do
   (do ((i 5 (1- i)) (j 1 (* i j))) ((zero? i) (= j 120)))
   (do ((a 3) (i 20 (1- i))) ((zero? i) (= a 23)) (set! a (1+ a)))
 )

;;; section 4-6:

(mat call/cc
   (call/cc procedure?)
   (equal? (call/cc (lambda (x) (+ 3 (x "hi")))) "hi")
   (eq? (let ([l (call/cc
                    (lambda (ret)
                        (call/cc (lambda (l) (ret l)))
                        (lambda (x) 'hi)))])
            (l #f))
        'hi)
   (((call/cc call/cc) (lambda (x) x)) #t)
   (let () 
     (define f
       (lambda (n)
         (let f ((n n))
           (or (fx= n 0) 
               (and (call/cc (lambda (k) k))
                    (f (fx- n 1)))))))
     (f 100000))
   (let () 
     (define f
       (lambda (n)
         (let f ((n n))
           (or (fx= n 0) 
               (and (call/cc (lambda (k) (k k)))
                    (f (fx- n 1)))))))
     (f 100000))
   (let f ((n 100000))
     (or (= n 0)
         (call/cc (lambda (k) (f (- n 1))))))
   (eqv? (let f ((n 1000) (ks '()))
           (if (= n 0)
               ((list-ref (reverse ks) 317) 0)
               (call/cc (lambda (k) (- (f (- n 1) (cons k ks)) 1)))))
         -317)
   (call/cc (lambda (k) (call/cc (lambda (k1) (eq? k1 k)))))
   (let f ((n 1000) (k #f))
     (or (= n 0)
         (call/cc
           (lambda (k1)
             (and (eq? k1 (or k k1))
                  (f (- n 1) k1))))))
   (eqv? (let ()
           (define (ctak-aux k x y z)
             (cond ((not (< y x))  ;xy
                    (k z))
                   (else (call-with-current-continuation
                          (ctak-aux
                           k
                           (call-with-current-continuation
                            (lambda (k)
                              (ctak-aux k (- x 1) y z)))
                           (call-with-current-continuation
                            (lambda (k)
                              (ctak-aux k (- y 1) z x)))
                           (call-with-current-continuation
                            (lambda (k)
                              (ctak-aux k (- z 1) x y))))))))
           (define (ctak x y z)
             (call-with-current-continuation
              (lambda (k)
                (ctak-aux k x y z))))
           (ctak 18 12 6))
         7)
  (eqv? (call-with-current-continuation
          (lambda (exit)
            (for-each
              (lambda (x) (if (negative? x) (exit x)))
              '(54 0 37 -3 245 19))
            #t))
        -3)
  (equal?
    (let ()
      (define list-length
        (lambda (obj)
          (call-with-current-continuation
            (lambda (return)
              (letrec ([r
                        (lambda (obj)
                          (cond
                            [(null? obj) 0]
                            [(pair? obj) (+ (r (cdr obj)) 1)]
                            [else (return #f)]))])
                (r obj))))))
      (list (list-length '(1 2 3 4)) (list-length '(a b . c))))
    '(4 #f))
  (let ()
    (define (next-leaf-generator obj eot)
      (letrec ([return #f]
               [cont
                (lambda (x)
                  (recur obj)
                  (set! cont (lambda (x) (return eot)))
                  (cont #f))]
               [recur
                (lambda (obj)
                  (if (pair? obj)
                      (for-each recur obj)
                      (call-with-current-continuation
                        (lambda (c) (set! cont c) (return obj)))))])
        (lambda ()
          (call-with-current-continuation
            (lambda (ret) (set! return ret) (cont #f))))))
    (define (leaf-eq? x y)
      (let* ([eot (list 'eot)]
             [xf (next-leaf-generator x eot)]
             [yf (next-leaf-generator y eot)])
        (letrec ([loop
                  (lambda (x y)
                    (cond
                      [(not (eq? x y)) #f]
                      [(eq? eot x) #t]
                      [else (loop (xf) (yf))]))])
          (loop (xf) (yf)))))
    (and (eq? (leaf-eq? '(a (b (c))) '((a) b c)) #t)
         (eq? (leaf-eq? '(a (b (c))) '((a) b c d)) #f)))
 )
 
(mat dynamic-wind
   (let ([x 3])
      (and (eqv? x 3)
           (eqv? (dynamic-wind
                    (lambda () (set! x 4))
                    (lambda () x)
                    (lambda () (set! x 10)))
                 4)
           (eqv? x 10)))
   (let ([x 3])
      (and (eqv? x 3)
           (eqv? (call/cc
                    (lambda (l)
                       (dynamic-wind
                          (lambda () (set! x 4))
                          (lambda () (l x))
                          (lambda () (set! x 10)))
                       (set! x 20)))
                 4)
           (eqv? x 10)))
    (equal? (let* ([x 3]
                   [l (call/cc
                         (lambda (ret)
                            (dynamic-wind
                               (lambda () (set! x (1+ x)))
                               (lambda ()
                                  (call/cc (lambda (l) (ret l)))
                                  (let ([y x]) (lambda (n) (list n y))))
                               (lambda () (set! x (1- x))))))])
               (l x))
            '(3 4))
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (dynamic-wind
                           (lambda () #f)
                           (lambda () (k2 0))
                           (lambda () (k1 0)))))
                  1)))
         0)
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (dynamic-wind
                           (lambda () #f)
                           (lambda () (k1 0))
                           (lambda () (k2 0)))))
                  1)))
         1)
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (dynamic-wind
                           (lambda () (k2 0))
                           (lambda () (k2 10))
                           (lambda () (k2 20)))))
                  1)))
         1)
   (equal?
     (let ((p (open-output-string)))
       (if (call/cc
             (lambda (k)
               (dynamic-wind
                 (lambda () (display "E" p))
                 (lambda () (call/cc (lambda (k1) (set! *k1 k1) (k #t))))
                 (lambda () (display "I" p)))))
           (*k1 #f)
           (display "O" p))
       (get-output-string p))
     "EIEIO")

 ; once again for critical dynamic wind
   (let ([x 3])
      (and (eqv? x 3)
           (eqv? (dynamic-wind #t
                    (lambda () (set! x 4))
                    (lambda () x)
                    (lambda () (set! x 10)))
                 4)
           (eqv? x 10)))
   (let ([x 3])
      (and (eqv? x 3)
           (eqv? (call/cc
                    (lambda (l)
                       (dynamic-wind #t
                          (lambda () (set! x 4))
                          (lambda () (l x))
                          (lambda () (set! x 10)))
                       (set! x 20)))
                 4)
           (eqv? x 10)))
    (equal? (let* ([x 3]
                   [l (call/cc
                         (lambda (ret)
                            (dynamic-wind #t
                               (lambda () (set! x (1+ x)))
                               (lambda ()
                                  (call/cc (lambda (l) (ret l)))
                                  (let ([y x]) (lambda (n) (list n y))))
                               (lambda () (set! x (1- x))))))])
               (l x))
            '(3 4))
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (dynamic-wind #t
                           (lambda () #f)
                           (lambda () (k2 0))
                           (lambda () (k1 0)))))
                  1)))
         0)
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (dynamic-wind #t
                           (lambda () #f)
                           (lambda () (k1 0))
                           (lambda () (k2 0)))))
                  1)))
         1)
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (dynamic-wind #t
                           (lambda () (k2 0))
                           (lambda () (k2 10))
                           (lambda () (k2 20)))))
                  1)))
         1)
   (equal?
     (let ((p (open-output-string)))
       (if (call/cc
             (lambda (k)
               (dynamic-wind #t
                 (lambda () (display "E" p))
                 (lambda () (call/cc (lambda (k1) (set! *k1 k1) (k #t))))
                 (lambda () (display "I" p)))))
           (*k1 #f)
           (display "O" p))
       (get-output-string p))
     "EIEIO")

 ; make sure interrupts are enabled with error in critical dynamic wind
  (error? (dynamic-wind #t (lambda () gook) void void))
  (and (= (disable-interrupts) 1)
       (= (enable-interrupts) 0))
  (error? (dynamic-wind #t void void (lambda () gook)))
  (and (= (disable-interrupts) 1)
       (= (enable-interrupts) 0))
  (error? ((call/cc
             (lambda (k)
               (let ([first? #t])
                 (dynamic-wind #t
                   (lambda () (if first? (set! first? #f) gook))
                   (lambda () (call/cc k))
                   void))))))
  (and (= (disable-interrupts) 1)
       (= (enable-interrupts) 0))
  (error? (call/cc
            (lambda (k)
              (let ([first? #t])
                (dynamic-wind #t
                  void
                  k
                  (lambda () gook))))))
  (and (= (disable-interrupts) 1)
       (= (enable-interrupts) 0))
 )

(mat r6rs:dynamic-wind
   (let ([x 3])
      (and (eqv? x 3)
           (eqv? (r6rs:dynamic-wind
                    (lambda () (set! x 4))
                    (lambda () x)
                    (lambda () (set! x 10)))
                 4)
           (eqv? x 10)))
   (let ([x 3])
      (and (eqv? x 3)
           (eqv? (call/cc
                    (lambda (l)
                       (r6rs:dynamic-wind
                          (lambda () (set! x 4))
                          (lambda () (l x))
                          (lambda () (set! x 10)))
                       (set! x 20)))
                 4)
           (eqv? x 10)))
    (equal? (let* ([x 3]
                   [l (call/cc
                         (lambda (ret)
                            (r6rs:dynamic-wind
                               (lambda () (set! x (1+ x)))
                               (lambda ()
                                  (call/cc (lambda (l) (ret l)))
                                  (let ([y x]) (lambda (n) (list n y))))
                               (lambda () (set! x (1- x))))))])
               (l x))
            '(3 4))
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (r6rs:dynamic-wind
                           (lambda () #f)
                           (lambda () (k2 0))
                           (lambda () (k1 0)))))
                  1)))
         0)
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (r6rs:dynamic-wind
                           (lambda () #f)
                           (lambda () (k1 0))
                           (lambda () (k2 0)))))
                  1)))
         1)
   (eqv? (call/cc
            (lambda (k1)
               (+ (call/cc
                     (lambda (k2)
                        (r6rs:dynamic-wind
                           (lambda () (k2 0))
                           (lambda () (k2 10))
                           (lambda () (k2 20)))))
                  1)))
         1)
   (equal?
     (let ((p (open-output-string)))
       (if (call/cc
             (lambda (k)
               (r6rs:dynamic-wind
                 (lambda () (display "E" p))
                 (lambda () (call/cc (lambda (k1) (set! *k1 k1) (k #t))))
                 (lambda () (display "I" p)))))
           (*k1 #f)
           (display "O" p))
       (get-output-string p))
     "EIEIO")
 )

(mat call/1cc
   (call/1cc procedure?)
   (equal? (call/cc (lambda (x) (+ 3 (x "hi")))) "hi")
   (((call/1cc call/cc) (lambda (x) x)) #t)
   (((call/cc call/1cc) (lambda (x) x)) #t)
   (error?
     (parameterize ((collect-request-handler void))
       ((let f ((n 100))
          (if (= n 0)
              (call/1cc
                (lambda (k)
                  (rec me
                    (case-lambda
                      [() me]
                      [(x) (k x)]))))
              ((call/1cc (lambda (k) (f (- n 1)))))))
        (rec me
          (case-lambda
            [() me]
            [(x) #t])))))
   (parameterize ((collect-request-handler void))
     ((let f ((n 100))
        (if (= n 0)
            (call/cc
              (lambda (k)
                (rec me
                  (case-lambda
                    [() me]
                    [(x) (k x)]))))
            ((call/1cc (lambda (k) (f (- n 1)))))))
      (rec me
        (case-lambda
          [() me]
          [(x) #t]))))
   (let () 
     (define f
       (lambda (n)
         (let f ((n n))
           (or (fx= n 0) 
               (and (call/cc (lambda (k) (k k)))
                    (f (fx- n 1)))))))
     (f 100000))
   (let f ((n 100000))
     (or (= n 0)
         (call/1cc (lambda (k) (f (- n 1))))))
   (eqv? (let f ((n 1000) (ks '()))
           (if (= n 0)
               ((list-ref (reverse ks) 317) 0)
               (call/1cc (lambda (k) (- (f (- n 1) (cons k ks)) 1)))))
         -317)
   (call/1cc (lambda (k) (call/1cc (lambda (k1) (eq? k1 k)))))
   (call/1cc (lambda (k) (call/cc (lambda (k1) (eq? k1 k)))))
   (call/cc (lambda (k) (call/1cc (lambda (k1) (eq? k1 k)))))
   (let f ((n 1000) (k #f))
     (or (= n 0)
         (call/1cc
           (lambda (k1)
             (and (eq? k1 (or k k1))
                  (f (- n 1) k1))))))
   (eqv? (let ()
           (define (ctak-aux k x y z)
             (cond ((not (< y x))  ;xy
                    (k z))
                   (else (call/1cc
                          (ctak-aux
                           k
                           (call/1cc
                            (lambda (k)
                              (ctak-aux k (- x 1) y z)))
                           (call/1cc
                            (lambda (k)
                              (ctak-aux k (- y 1) z x)))
                           (call/1cc
                            (lambda (k)
                              (ctak-aux k (- z 1) x y))))))))
           (define (ctak x y z)
             (call/1cc
              (lambda (k)
                (ctak-aux k x y z))))
           (ctak 18 12 6))
         7)
   (let ([x 3])
      (and (eqv? x 3)
           (eqv? (call/1cc
                    (lambda (l)
                       (dynamic-wind
                          (lambda () (set! x 4))
                          (lambda () (l x))
                          (lambda () (set! x 10)))
                       (set! x 20)))
                 4)
           (eqv? x 10)))
    (equal? (let* ([x 3]
                   [l (call/cc
                         (lambda (ret)
                            (dynamic-wind
                               (lambda () (set! x (1+ x)))
                               (lambda ()
                                  (call/1cc (lambda (l) (ret l)))
                                  (let ([y x]) (lambda (n) (list n y))))
                               (lambda () (set! x (1- x))))))])
               (l x))
            '(3 4))
   (eqv? (call/1cc
            (lambda (k1)
               (+ (call/1cc
                     (lambda (k2)
                        (dynamic-wind
                           (lambda () #f)
                           (lambda () (k2 0))
                           (lambda () (k1 0)))))
                  1)))
         0)
   (eqv? (call/1cc
            (lambda (k1)
               (+ (call/1cc
                     (lambda (k2)
                        (dynamic-wind
                           (lambda () #f)
                           (lambda () (k1 0))
                           (lambda () (k2 0)))))
                  1)))
         1)
   (eqv? (call/1cc
            (lambda (k1)
               (+ (call/1cc
                     (lambda (k2)
                        (dynamic-wind
                           (lambda () (k2 0))
                           (lambda () (k2 10))
                           (lambda () (k2 20)))))
                  1)))
         1)
   (equal?
     (let ((p (open-output-string)))
       (if (call/cc
             (lambda (k)
               (dynamic-wind
                 (lambda () (display "E" p))
                 (lambda () (call/1cc (lambda (k1) (set! *k1 k1) (k #t))))
                 (lambda () (display "I" p)))))
           (*k1 #f)
           (display "O" p))
       (get-output-string p))
     "EIEIO")
 )
 
;;; section 4-7:

(mat engine
   (letrec ([ee (make-engine
                   (lambda ()
                      (map 1+ '(1 2 3 4 5 6 7 8 9))))]
            [foo (lambda (n e)
                    (if (zero? n)
                        '()
                        (e n
                           (lambda (x y) (foo (1- n) ee))
                           (lambda (e) (foo n e)))))]
            [goo (lambda (n)
                    (if (zero? n)
                        'okay
                        (begin (foo n ee) (goo (1- n)))))])
        (eq? (goo 20) 'okay))
    (let ([e (make-engine (lambda () (engine-block) (engine-return 'hi)))])
       (e 10000
          (lambda (x y) #f)
          (lambda (e1)
             (e1 10000
                 (lambda (t x) (eq? x 'hi))
                 (lambda (e) #f)))))
    (equal? (let ([e (make-engine (lambda () (engine-block) (values 1 2 3)))])
              (e 10000
                 (lambda (x . y) #f)
                 (lambda (e1)
                    (e1 10000
                        (lambda (t . x) x)
                        (lambda (e) #f)))))
            '(1 2 3))
  (eqv?
    (let ([e (make-engine (lambda () (raise 'hello)))])
      (guard (c [else c])
        (e 1000 list values)))
    'hello)
  (eqv?
    (let ([e (make-engine (lambda () (raise-continuable 'hello)))])
      (with-exception-handler
        (lambda (c) 17)
        (lambda () (e 1000 (lambda (x y) y) values))))
    17)
  (eqv?
    (let ([e (make-engine
               (lambda ()
                 (let ([x (raise-continuable 'hello)])
                   (define fib
                     (lambda (x)
                       (if (<= x 1)
                           1
                           (+ (fib (- x 1)) (fib (- x 2))))))
                   (cons x (fib 20)))))])
      (with-exception-handler
        (lambda (c) (and (eq? c 'hello) 17))
        (lambda ()
          (e 1000 (lambda (x y) y) (lambda (x) 'stalled)))))
    'stalled)
  (equal?
    (let ([e (make-engine
               (lambda ()
                 (let ([x (raise-continuable 'hello)])
                   (define fib
                     (lambda (x)
                       (if (<= x 1)
                           1
                           (+ (fib (- x 1)) (fib (- x 2))))))
                   (cons x (fib 20)))))])
      (with-exception-handler
        (lambda (c) (and (eq? c 'hello) 17))
        (lambda ()
          (e 1000
             (lambda (x y) 'oops1)
             (lambda (e)
               (e 1000
                 (lambda (x y) 'oops2)
                 (lambda (e)
                   (e 1000000
                     (lambda (x y) y)
                     values))))))))
    '(17 . 10946))
  (equal?
    (let* ([e0 (make-engine
                 (lambda ()
                   (define fib
                     (lambda (x)
                       (if (<= x 1)
                           1
                           (+ (fib (- x 1)) (fib (- x 2))))))
                   (let ([n (fib 20)])
                     (cons n (raise-continuable 'hello)))))]
           [e1 (with-exception-handler
                 (lambda (c) 'stuff1)
                 (lambda ()
                   (e0 1000
                     (lambda (x y) 'oops1)
                     (lambda (e) e))))]
           [e2 (with-exception-handler
                 (lambda (c) 'stuff2)
                 (lambda ()
                   (e1 1000
                     (lambda (x y) 'oops2)
                     (lambda (e) e))))])
      (with-exception-handler
        (lambda (c) 'stuff3)
        (lambda ()
          (e2 1000000
            (lambda (x y) y)
            (lambda (e) e)))))
    '(10946 . stuff3))
  (let ()
    (define spin
      (letrec ((spin
                (lambda (n m)
                  (cond
                    ((= n 0) m)
                    (else (spin (- n 1) (+ m 1)))))))
        (lambda (n)
          (spin n 0))))
    (define test6B/counter
      (lambda (ticks th)
        (define bytes (bytes-allocated))
        (define counter 0)
        (let loop ([e (make-engine th)])
          (call-with-values
            (lambda () (e ticks values values))
            (case-lambda
              [(left v) v]
              [(e)
               (set! counter (add1 counter))
               (when (zero? (remainder counter 100000))
                 (collect (collect-maximum-generation))
                 (let ([% 20] [new-bytes (bytes-allocated)])
                   (when (> new-bytes (* bytes (+ 1 (/ % 100))))
                     (errorf 'test6B/counter "bytes allocated has grown by more than ~s% from ~s to ~s"
                       % bytes new-bytes))))
               (loop e)])))))
    (let ([n 100000000])
      (eqv?
        (test6B/counter 125 (lambda () (spin n)))
        n)))
)

;;; section 4-8:

(mat delay-force ;;; from The Scheme Programming Language
   (letrec ([stream-car
             (lambda (s)
                (car (force s)))]
            [stream-cdr
             (lambda (s)
                (cdr (force s)))]
            [stream-add
             (lambda (s1 s2)
                (delay
                   (cons (+ (stream-car s1) (stream-car s2))
                         (stream-add (stream-cdr s1) (stream-cdr s2)))))])
      (let ([counters
             (let next ([n 1])
                (delay (cons n (next (+ n 1)))))])
         (and (eqv? (stream-car counters) 1)
              (eqv? (stream-car (stream-cdr counters)) 2)
              (let ([even-counters (stream-add counters counters)])
                 (and (eqv? (stream-car even-counters) 2)
                      (eqv? (stream-car (stream-cdr even-counters)) 4))))))
   (equal? (let ([x 0])
             (let ([y (delay (begin (set! x 1) (values)))])
               (let ([z x])
                 (force y)
                 (list x z))))
           '(1 0))
  ; test for common delay/force bug posted to comp.lang.scheme; we had
  ; this for a short while after delay/force were extended to handle
  ; multiple values
   (eq? (letrec ((p (delay (if c 3 (begin (set! c #t) (+ (force p) 1)))))
                 (c #f))
          (force p))
        3)
 )

;;; no section ...

(mat make-guardian
   (procedure? make-guardian)
   (with-interrupts-disabled
      (let ([x (make-guardian)])
         (and (not (x))
              (begin (x (cons 'a 'b)) (not (x)))
              (begin (collect) (equal? (x) '(a . b)))
              (not (x)))))
   (with-interrupts-disabled
      (let ([x1 (make-guardian)])
         ; counting on a little compiler cleanliness here...
         (let ([x2 (make-guardian)])
            (x1 x2)
            (x2 x2))
         (collect)
         (let ([x2 (x1)])
            (and (equal? (x2) x2)
                 (not (x1))
                 (not (x2))))))
   (parameterize ([collect-trip-bytes (expt 2 24)])
     (let ([k 1000000])
       (let ([g (make-guardian)])
         (let f ([n k])
           (unless (= n 0)
             (g (cons 3 4))
             (let f () (cond [(g) => (lambda (x) (g x) (f))]))
             (f (- n 1))))
         (let f ([n k])
           (unless (= n 0)
             (cond
               [(g) => (lambda (x) (f (- n 1)))]
               [else (collect) (f n)])))
         #t)))
   (with-interrupts-disabled
      (let ([x (make-guardian)])
         (and (not (x))
              (begin (x (cons 'a 'b) 'calvin) (not (x)))
              (begin (collect) (equal? (x) 'calvin))
              (not (x)))))
   (with-interrupts-disabled
      (let ([x (make-guardian)])
         (and (not (x))
              (begin (x (cons 'a 'b) (cons 'calvin 'hobbes)) (not (x)))
              (begin (collect) (equal? (x) '(calvin . hobbes)))
              (not (x)))))
   (with-interrupts-disabled
      (let ([x (make-guardian)])
         (and (not (x))
              (begin (x (cons 'a 'b) 17) (not (x)))
              (begin (collect) (equal? (x) '17))
              (not (x)))))
   (equal?
     (with-interrupts-disabled
       (let ([g1 (make-guardian)] [g2 (make-guardian)])
         (let ([p (list 'a 'b)])
           (g1 p g2)
           (g2 (list 'c 'd))
           (collect 0 0)
           (let ([p (cdr p)])
             (collect 0 0)
             (list ((g1)) p)))))
     '((c d) (b)))

   (eq? (with-interrupts-disabled
          (let* ([g (make-guardian)] [x (list 'a 'b)])
            (g x)
            (collect 0 0)
            (#%$keep-live x)
            (g)))
        #f)

   (or (not (threaded?))
       (equal?
         (parameterize ([collect-request-handler void])
           (let ([g (make-guardian)])
             (fork-thread (lambda () (g (list 'a 'b))))
             (let f () (when (> (length (#%$thread-list)) 1) (f)))
             (collect)
             (g)))
         '(a b)))

   (parameterize ([collect-request-handler void] [enable-object-counts #t])
     (define-record-type fraz (fields zle))
     (define g (make-guardian))
     (define x (make-fraz 17))
     (g x)
     (collect 0 0)
     (unless (let ([a (assq 'guardian (object-counts))])
               (and a (assq 0 (cdr a))))
       (error #f "no generation 0 guardian in object-counts list"))
     (unless (let ([a (assq (record-type-descriptor fraz) (object-counts))])
               (and a (assq 0 (cdr a))))
       (error #f "no generation 0 fraz in object-counts list"))
     (collect (collect-maximum-generation))
     (unless (let ([a (assq 'guardian (object-counts))])
               (and a (assq (collect-maximum-generation) (cdr a))))
       (error #f "no maximum-generation guardian in object-counts list"))
     (unless (let ([a (assq (record-type-descriptor fraz) (object-counts))])
               (and a (assq (collect-maximum-generation) (cdr a))))
       (error #f "no maximum-generation fraz in object-counts list"))
     (collect (collect-maximum-generation) 'static)
     (when (let ([a (assq 'guardian (object-counts))])
               (and a (assq 'static (cdr a))))
       (error #f "static-generation guardian in object-counts list"))
     (unless (let ([a (assq (record-type-descriptor fraz) (object-counts))])
               (and a (assq 'static (cdr a))))
       (error #f "no static-generation fraz in object-counts list"))
     (pretty-print (cons g x)) ; keep 'em live
     #t)

   (parameterize ([collect-request-handler void])
     (define (get-all g) (let ([q (g)]) (if q (cons q (get-all g)) '())))
     (module (insist)
       (define ($insist e? expr expected got)
         (unless (e? got expected)
           (errorf #f "expected ~s to return ~s, got ~s" expr expected got)))
       (define-syntax insist
         (syntax-rules ()
           [(_ ?e? ?expr ?expected)
            ($insist ?e? '?expr ?expected ?expr)])))
     (let ([g1 (make-guardian)] [g2 (make-guardian)])
       (let ([x (box (cons 'a 'b))] [y (box (cons 'c 'd))])
         (insist eq? (unregister-guardian g1) '())
         (insist eq? (unregister-guardian g2) '())
         (g1 (unbox x))
         (g1 (unbox y))
         (g2 (unbox x))
         (g1 (unbox y))
         (g1 (unbox x))
         (collect 0 0)
         (g2 (unbox x))
         (g1 (cons 'e 'f))
         (g2 (unbox x))
         (g1 (unbox x))
         (g2 (cons 'g 'h))
         (insist eq? (get-all g1) '())
         (insist eq? (get-all g2) '())
         (let ([q (unregister-guardian g2)])
           (unless (and (= (length q) 4) (equal? (remove '(g . h) q) (list (unbox x) (unbox x) (unbox x))))
             (errorf #f "expected (unregister-guardian g2) to contain x = (a . b), x = (a . b), and (g . h), got ~s" q)))
         (insist eq? (unregister-guardian g2) '())
         (insist eq? (get-all g1) '())
         (insist eq? (get-all g2) '())
         (collect 0 0)
         (insist equal? (get-all g1) '((e . f)))
         (insist eq? (get-all g2) '())
         (g2 (unbox x))
         (set-box! x #f)
         (collect 0 0)
         (insist equal? (get-all g1) '((a . b) (a . b) (a . b)))
         (insist equal? (get-all g2) '((a . b)))
         (insist equal? (unregister-guardian g1) '((c . d) (c . d)))
         (insist eq? (unregister-guardian g2) '())
         (pretty-print (list g1 g2 x y)))) ; keep 'em live
     #t)
 )

(mat refcount-guardians
  (error? ; unrecognized ftype
    (ftype-guardian NO!))
  (error? ; first element must be a word-sized integer with native endianness
    (let ()
      (define-ftype A (struct))
      (ftype-guardian A)))
  (error? ; first element must be a word-sized integer with native endianness
    (let ()
      (define-ftype A (union [u1 (struct (refcount char))] [u2 (struct (foo (* A)))]))
      (ftype-guardian A)))
  (error? ; invalid ftype-guardian argument
    (let ()
      (define-ftype A (struct (refcount iptr) (x int)))
      (define g (ftype-guardian A))
      (g (cons 'ka 'blooie))))
  (error? ; invalid ftype-guardian argument
    (let ()
      (define-ftype A (struct (refcount iptr) (x int)))
      (define g (ftype-guardian A))
      (g (make-ftype-pointer iptr 0))))
  (eq?
    (let ()
      (define-ftype A (struct (refcount iptr) (x int)))
      (define g (ftype-guardian iptr))
      (g (make-ftype-pointer A 0)))
    (void))
  (with-interrupts-disabled
    (let ()
      (define-ftype A (struct (refcount iptr) (x int)))
      (define g (ftype-guardian A))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (ftype-set! A (refcount) a 0)
      (assert (not (ftype-locked-incr! A (refcount) a)))
      (assert (eqv? (ftype-ref A (refcount) a) 1))
      (g a)
      (set! a #f)
      (collect 0 0)
      (assert (eqv? (ftype-ref A (refcount) (g)) 0))
      (assert (not (g)))
      #t))
  (with-interrupts-disabled
    (let ()
      (define-ftype A (struct (refcount uptr) (x int)))
      (define g (ftype-guardian A))
      (define addr (foreign-alloc (ftype-sizeof A)))
      (define a1 (make-ftype-pointer A addr))
      (define a2 (make-ftype-pointer A addr))
      (define wpa1 (weak-cons a1 '()))
      (define wpa2 (weak-cons a2 '()))
      (ftype-set! A (refcount) a1 0)
      (ftype-set! A (x) a1 17)
      (assert (eqv? (ftype-ref A (x) a1) 17))
      (assert (eqv? (ftype-ref A (x) a2) 17))
      (assert (eqv? (ftype-ref A (refcount) a1) 0))
      (assert (eqv? (ftype-ref A (refcount) a2) 0))
      (assert (not (ftype-locked-incr! A (refcount) a1)))
      (assert (not (ftype-locked-incr! A (refcount) a2)))
      (assert (eqv? (ftype-ref A (refcount) a1) 2))
      (assert (eqv? (ftype-ref A (refcount) a2) 2))
      (g a1)
      (g a2)
      (collect 0 0)
      (assert (not (g)))
      (set! a1 #f)
      (collect 0 0)
      (assert (not (g)))
      (set! a2 #f)
      (collect 0 0)
      (set! a2 (g))
      (assert (eq? (car wpa2) a2))
      (assert (not (g)))
      (assert (eqv? (ftype-ref A (refcount) a2) 0))
      #t))
  (with-interrupts-disabled
    (let ()
      (define-ftype A (struct (refcount iptr) (x int)))
      (define g (ftype-guardian A))
      (define regular-g (make-guardian))
      (define addr (foreign-alloc (ftype-sizeof A)))
      (define a (make-ftype-pointer A addr))
      (ftype-set! A (refcount) a 0)
      (assert (not (ftype-locked-incr! A (refcount) a)))
      (assert (eqv? (ftype-ref A (refcount) a) 1))
      (regular-g a)
      (g a)
      (regular-g a)
      (set! a #f)
      (collect 0 0)
      (assert (eqv? (ftype-ref A (refcount) (regular-g)) 0))
      (assert (eqv? (ftype-ref A (refcount) (regular-g)) 0))
      (assert (eqv? (ftype-ref A (refcount) (g)) 0))
      (assert (not (regular-g)))
      (assert (not (g)))
      #t))
  (with-interrupts-disabled
    (let ()
      (define-ftype A (struct (refcount uptr) (x int)))
      (define g (ftype-guardian A))
      (define regular-g (make-guardian))
      (define addr (foreign-alloc (ftype-sizeof A)))
      (define a (make-ftype-pointer A addr))
      (ftype-set! A (refcount) a 0)
      (assert (not (ftype-locked-incr! A (refcount) a)))
      (assert (not (ftype-locked-incr! A (refcount) a)))
      (assert (eqv? (ftype-ref A (refcount) a) 2))
      (regular-g a)
      (g a)
      (regular-g a)
      (set! a #f)
      (collect 0 0)
      (assert (eqv? (ftype-ref A (refcount) (regular-g)) 1))
      (assert (eqv? (ftype-ref A (refcount) (regular-g)) 1))
      (assert (not (regular-g)))
      (assert (not (g)))
      #t))
  (with-interrupts-disabled
    (let ()
      (define-ftype A (struct (refcount iptr) (x int)))
      (define g (ftype-guardian A))
      (define regular-g (make-guardian))
      (define addr (foreign-alloc (ftype-sizeof A)))
      (define a (make-ftype-pointer A addr))
      (ftype-set! A (refcount) a 0)
      (assert (not (ftype-locked-incr! A (refcount) a)))
      (assert (not (ftype-locked-incr! A (refcount) a)))
      (assert (eqv? (ftype-ref A (refcount) a) 2))
      (regular-g a)
      (g a)
      (g a)
      (regular-g a)
      (set! a #f)
      (collect 0 0)
      (assert (eqv? (ftype-ref A (refcount) (regular-g)) 0))
      (assert (eqv? (ftype-ref A (refcount) (regular-g)) 0))
      (assert (eqv? (ftype-ref A (refcount) (g)) 0))
      (assert (not (regular-g)))
      (assert (not (g)))
      #t))

   (parameterize ([collect-request-handler void])
     (define-ftype A (struct (refcount iptr) (uid int)))
     (define (get-all g)
       (let ([a (g)])
         (if a
             (begin
               (unless (eqv? (ftype-ref A (refcount) a) 0)
                 (errorf 'get-all "nonzero refcount ~s, uid ~s" (ftype-ref A (refcount) a) (ftype-ref A (uid) a)))
               (cons a (get-all g)))
             '())))
     (module (insist)
       (define ($insist e? expr expected got)
         (unless (e? got expected)
           (errorf #f "expected ~s to return ~s, got ~s" expr expected got)))
       (define-syntax insist
         (syntax-rules ()
           [(_ ?e? ?expr ?expected)
            ($insist ?e? '?expr ?expected ?expr)])))
     (define (get-uid a) (ftype-ref A (uid) a))
     (define (fritter addr refcount uid)
       (let ([a (make-ftype-pointer A addr)])
         (ftype-set! A (refcount) a refcount)
         (ftype-set! A (uid) a uid)
         (box a)))
     (let ([x-addr (foreign-alloc (ftype-sizeof A))] [y-addr (foreign-alloc (ftype-sizeof A))] [z-addr (foreign-alloc (ftype-sizeof A))])
       (let ([x1 (fritter x-addr 6 73)] [x2 (box (make-ftype-pointer A x-addr))] [y (fritter y-addr 2 57)] [z (fritter z-addr 2 91)])
         (let ([g1 (ftype-guardian A)] [g2 (ftype-guardian A)])
           (insist eq? (unregister-guardian g1) '())
           (insist eq? (unregister-guardian g2) '())
           (g1 (unbox x1))
           (g2 (unbox x1))
           (g1 (unbox x1))
           (g1 (unbox x2))
           (g2 (unbox x1))
           (g1 (unbox y))
           (g1 (unbox y))
           (g2 (unbox z))
           (g1 (unbox z))
           (insist eq? (get-all g1) '())
           (insist eq? (get-all g2) '())
           (let ([q (unregister-guardian g2)])
             (define (decr-refcount! a) (ftype-locked-decr! A (refcount) a))
             (unless (and (= (length q) 3) (memq (unbox x1) (memq (unbox x1) q)) (memq (unbox z) q))
               (errorf #f "expected (unregister-guardian g2) to contain x/uid 73, x/uid 73, and z/uid 91, got ~s" (map get-uid q)))
             (map decr-refcount! q))
           (insist eq? (unregister-guardian g2) '())
           (insist eq? (get-all g1) '())
           (insist eq? (get-all g2) '())
           (pretty-print z) ; keep it live
           (set-box! z #f)
           (collect 0 0)
           (insist equal? (map get-uid (get-all g1)) '(91))
           (insist eq? (get-all g2) '())
           (g2 (unbox x1))
           (pretty-print x1) ; keep it live
           (set-box! x1 #f)
           (collect 0 0)
           (insist eq? (get-all g1) '())
           (insist eq? (get-all g2) '())
           (insist eq? (unregister-guardian g2) '())
           (insist eqv? (ftype-ref A (refcount) (unbox x2)) 1)
           (pretty-print x2) ; keep it live
           (set-box! x2 #f)
           (collect 0 0)
           (insist equal? (map get-uid (get-all g1)) '(73))
           (insist equal? (map get-uid (get-all g2)) '())
           (insist eq? (unregister-guardian g2) '())
           (pretty-print y) ; keep it live
           (set-box! y #f)
           (collect 0 0)
           (insist equal? (map get-uid (get-all g1)) '(57))
           (insist equal? (map get-uid (get-all g2)) '())
           (insist eq? (unregister-guardian g1) '())
           (insist eq? (unregister-guardian g2) '())
           (pretty-print (list g1 g2 x y)))) ; keep 'em live
       (foreign-free x-addr)
       (foreign-free y-addr)
       (foreign-free z-addr))
     #t)
  )

(mat weak-cons
   (procedure? weak-cons)
   (procedure? weak-pair?)
   (with-interrupts-disabled
      (let ([x (weak-cons (cons 'a 'b) 'c)])
         (and (equal? (car x) '(a . b))
              (begin (collect) (bwp-object? (car x)))
              (begin (set-car! x (cons 'd 'e)) (equal? (car x) '(d . e)))
              (begin (collect (collect-maximum-generation))
                     (bwp-object? (car x))))))
 )

(mat ephemeron
   (begin
     (define ephemeron-key car)
     (define ephemeron-value cdr)
     
     (define gdn (make-guardian))
     #t)

   (ephemeron-pair? (ephemeron-cons 1 2))

   (begin
     ;; ----------------------------------------
     ;; Check that the ephemeron value doesn't retain
     ;; itself as an epehemeron key
     (define-values (es wps saved)
       (let loop ([n 1000] [es '()] [wps '()] [saved '()])
         (cond
          [(zero? n)
           (values es wps saved)]
          [else
           (let ([k1 (gensym)]
                 [k2 (gensym)])
             (gdn k2)
             (loop (sub1 n)
                   (cons (ephemeron-cons k1 (box k1))
                         (cons (ephemeron-cons k2 (box k2))
                               es))
                   (weak-cons k1 (weak-cons k2 wps))
                   (cons k1 saved)))])))
     
     (collect (collect-maximum-generation))

     ;; All now waiting to be reported by the guardian
     (let loop ([es es] [wps wps] [saved saved])
       (cond
        [(null? saved) #t]
        [else
         (and
          (eq? (car saved) (car wps))
          (eq? (car saved) (ephemeron-key (car es)))
          (eq? (car saved) (unbox (ephemeron-value (car es))))
          (eq? (cadr wps) (ephemeron-key (cadr es)))
          (eq? (cadr wps) (unbox (ephemeron-value (cadr es))))
          (loop (cddr es) (cddr wps) (cdr saved)))])))

   (begin
     ;; Report each from the guardian:
     (let loop ([saved saved])
       (unless (null? saved)
         (gdn)
         (loop (cdr saved))))

     (collect (collect-maximum-generation))

     (let loop ([es es] [wps wps] [saved saved])
       (cond
        [(null? saved) #t]
        [else
         (and
          (eq? (car saved) (car wps))
          (eq? (car saved) (ephemeron-key (car es)))
          (eq? (car saved) (unbox (ephemeron-value (car es))))
          (eq? #!bwp (cadr wps))
          (eq? #!bwp (ephemeron-key (cadr es)))
          (eq? #!bwp (ephemeron-value (cadr es)))
          (loop (cddr es) (cddr wps) (cdr saved)))])))

   ;; ----------------------------------------
   ;; Stress test to check that the GC doesn't suffer from quadratic
   ;; behavior
   (let ()
     (define (wrapper v) (list 1 2 3 4 5 v))
     
     ;; Create a chain of ephemerons where we have all
     ;; the the ephemerons immediately in a list,
     ;; but we discover the keys one at a time
     (define (mk n prev-key es)
       (cond
        [(zero? n)
         (values prev-key es)]
        [else
         (let ([key (gensym)])
           (mk (sub1 n)
               key
               (cons (ephemeron-cons key (wrapper prev-key))
                     es)))]))

     ;; Create a chain of ephemerons where we have all
     ;; of the keys immediately in a list,
     ;; but we discover the ephemerons one at a time
     (define (mk* n prev-e keys)
       (cond
        [(zero? n)
         (values prev-e keys)]
        [else
         (let ([key (gensym)])
           (mk* (sub1 n)
                (ephemeron-cons key (wrapper prev-e))
                (cons key
                      keys)))]))

     (define (measure-time n keep-alive)
       ;; Hang the discover-keys-one-at-a-time chain
       ;; off the end of the discover-ephemerons-one-at-a-time
       ;; chain, which is the most complex case for avoiding
       ;; quadratic GC times
       (parameterize ([collect-request-handler void] [collect-maximum-generation (max (collect-maximum-generation) 2)])
         (collect 2)
         (let*-values ([(key es) (mk n (gensym) '())]
                       [(root holds) (mk* n key es)])
           (let ([start (current-time)])
             (collect 0 1)
             (collect 1 2)
             (collect 2 2)
             (let ([delta (time-difference (current-time) start)])
               ;; Sanity check on ephemerons
               (for-each (lambda (e)
                           (when (eq? #!bwp (ephemeron-key e))
                             (error 'check "oops")))
                         es)
               ;; Keep `root` and `holds` live:
               (keep-alive (cons root holds))
               ;; Return duration:
               delta)))))

     (define N 10000)

     ;; The first time should be roughy x10 the second (not x100)
     (let loop ([tries 3])
       (define dummy #f)
       (define (keep-alive v) (set! dummy (cons dummy v)))
       (define t1 (measure-time (* 10 N) keep-alive))
       (define dummy2 (set! dummy #f))
       (define t2 (measure-time N keep-alive))
       (define (duration->inexact t) (+ (* (time-second t) 1e9)
                                        (inexact (time-nanosecond t))))
       (set! dummy #f)
       (let ([t1 (duration->inexact t1)] [t2 (duration->inexact t2)])
         (or (< (/ t1 t2) 20)
           (begin
             (printf "t1 = ~s, t2 = ~s, t1/t2 = ~s\n" t1 t2 (/ t1 t2))
             (and (positive? tries)
               (loop (sub1 tries))))))))

   ;; ----------------------------------------
   ;; Check interaction of mutation and generations
   
   ;; This check disables interrups so that a garbage collection
   ;; happens only for the explicit `collect` request.
   (with-interrupts-disabled
     (let ([e (ephemeron-cons (gensym) 'ok)])
       (collect 0) ; => `e` is moved to generation 1
       (and
         (eq? #!bwp (ephemeron-key e))
         (eq? #!bwp (ephemeron-value e))
         (let ([s (gensym)])
           (set-car! e s)
           (set-cdr! e 'ok-again)
           (collect 0) ; => `s` is moved to generation 1
           (and
             (eq? s (ephemeron-key e))
             (eq? 'ok-again (ephemeron-value e))
             (begin
               (set! s #f)
               (collect 1) ; collect former `s`
               (and
                 (eq? #!bwp (ephemeron-key e))
                 (eq? #!bwp (ephemeron-value e)))))))))

   ;; ----------------------------------------
   ;; Check interaction of mutation and incremental generation promotion

   (parameterize ([collect-request-handler void] [collect-maximum-generation (max (collect-maximum-generation) 2)])
    (let ([key "key"])
      (let ([e (ephemeron-cons key #f)])
        (collect 0 1 1)
        (let ([key2 (gensym key)])
          ;; e is gen 1, key2 is gen 0:
          (set-car! e key2)
          (collect 1 1 2)
          ;; Now, e is gen 1, key2 is gen 0
          (and (eq? (car e) key2)
               (begin
                 (collect 1 2 2)
                 ;; Check that the GC update the reference to `key2` in `e`:
                 (eq? (car e) key2)))))))

   ;; ----------------------------------------
   ;; Check fasl:
   (let ([s (gensym)])
     (define-values (o get) (open-bytevector-output-port))
     (fasl-write (list s
                       (ephemeron-cons s 'ok))
                 o)
     (let* ([l (fasl-read (open-bytevector-input-port (get)))]
            [e (cadr l)])
       (and
        (eq? (car l) (ephemeron-key e))
        (eq? 'ok (ephemeron-value e))
        (begin
          (set! s #f)
          (set! l #f)
          (collect (collect-maximum-generation))
          (and
           (eq? #!bwp (ephemeron-key e))
           (eq? #!bwp (ephemeron-value e))))))))

(mat $primitive
   (procedure? #%car)
   (procedure? #2%car)
   (procedure? #3%car)
   (equal? '#%car '($primitive car))
   (equal? '#2%car '($primitive 2 car))
   (equal? '#3%car '($primitive 3 car))
   (equal? (#%list 1 2 3) '(1 2 3))
   (eqv? (#2%+ 1 2 3) 6)
   (error? (#2%fx+ 'a))
   (error? #3%fubar)
   (error? (#2%car 'a 'b))
   (error? (#2%car 3)))
