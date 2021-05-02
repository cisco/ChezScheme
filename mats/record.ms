;;; record.ms
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

(define $mpf32 (min (most-positive-fixnum) (- (ash 1 29) 1)))
(define $mnf32 (max (most-negative-fixnum) (- (ash 1 29))))

(define $mpf64 (min (most-positive-fixnum) (- (ash 1 60) 1)))
(define $mnf64 (max (most-negative-fixnum) (- (ash 1 60))))

(mat record1
  (begin
    (define-record fudge ((immutable double-float a)))
    (andmap procedure? (list make-fudge fudge? fudge-a)))
  (error? (make-fudge 3))
  (error? (fudge-a 3))
)

(mat record2
  (begin
    (define fudge (make-record-type "fudge" '((immutable double-float a))))
    (record-type-descriptor? fudge))
  (begin
    (define make-fudge (record-constructor fudge))
    (procedure? make-fudge))
  (error? (make-fudge 3))
  (error? ((csv7:record-field-accessor fudge 'a) 3))
  (error? (make-record-type "fudge" '((immutable double-float a) . b)))
  (error? (make-record-type "fudge"
            (let ([x (list '(immutable a) '(immutable b) '(immutable c))])
              (set-cdr! (cddr x) (cdr x))
              x)))
)

(mat type-descriptor
  (let ()
    (define-record foo ())
    (record-type-descriptor? (type-descriptor foo)))
  (error? (type-descriptor 3))
  (error? (type-descriptor car))
)

(mat record3
  (begin
    (define-record fudge ((immutable a)))
    (andmap procedure? (list make-fudge fudge? fudge-a)))
  (begin
    (define x (make-fudge 3))
    (fudge? x))
  (eqv? (fudge-a x) 3)
  (error? (set-fudge-a! x x))
  (eqv? (fudge-a x) 3)
  (let ()
    (define-record fudge ((immutable a)))
    (and (andmap procedure? (list make-fudge fudge? fudge-a))
         (let ((x (make-fudge 3)))
           (and (fudge? x)
                (eqv? (fudge-a x) 3)
                (eqv? (fudge-a x) 3)))))
)

(mat record4
  (begin
    (define-record fudge ((a)))
    (andmap procedure? (list make-fudge fudge? fudge-a set-fudge-a!)))
  (begin
    (define x (make-fudge 3))
    (fudge? x))
  (eqv? (fudge-a x) 3)
  (error? (set-fudge-a! 3 x))
  (begin (set-fudge-a! x x) (eqv? (fudge-a x) x))
  #;(equal? (format "~s" x) "#0=#[fudge #0#]")
  (begin
    (define-record fudge ((mutable a)))
    (andmap procedure? (list make-fudge fudge? fudge-a set-fudge-a!)))
  (begin
    (define x (make-fudge 3))
    (fudge? x))
  (eqv? (fudge-a x) 3)
  (begin (set-fudge-a! x x) (eqv? (fudge-a x) x))
  #;(equal? (format "~s" x) "#0=#[fudge #0#]")
)

(mat record5
  (begin
    (define-record fudge ((mutable a) (mutable double-float b)))
    (andmap procedure?
      (list make-fudge fudge? fudge-a set-fudge-a! fudge-b set-fudge-b!)))
  (begin
    (define x (make-fudge 'a 3.4))
    (fudge? x))
  (eqv? (begin (set-fudge-b! x 4.4) (fudge-b x)) 4.4)
  #;(equal? (format "~s" x) "#[fudge a 4.4]")
  (begin
    (collect (collect-maximum-generation))
    (set-fudge-a! x (cons 3 4))
    (let ((p (weak-cons (fudge-a x) #f)))
      (collect)
      (and (eq? (car p) (fudge-a x))
           (begin (collect)
                  (eq? (car p) (fudge-a x))
                  (equal? (car p) '(3 . 4))))))
  (error? (set-fudge-b! x 4))
  (begin
    (define-record fudge ((a) (double-float b)))
    (andmap procedure?
      (list make-fudge fudge? fudge-a set-fudge-a! fudge-b set-fudge-b!)))
  (begin
    (define x (make-fudge 'a 3.4))
    (fudge? x))
  (eqv? (begin (set-fudge-b! x 4.4) (fudge-b x)) 4.4)
  #;(equal? (format "~s" x) "#[fudge a 4.4]")
  (begin
    (collect (collect-maximum-generation))
    (set-fudge-a! x (cons 3 4))
    (let ((p (weak-cons (fudge-a x) #f)))
      (collect)
      (and (eq? (car p) (fudge-a x))
           (begin (collect)
                  (eq? (car p) (fudge-a x))
                  (equal? (car p) '(3 . 4))))))
  (error? (set-fudge-b! x 4))
)

(mat record6
  (begin
    (define-record bar ((immutable a) (immutable integer-32 b))
      (((immutable c) (+ a b)) ((immutable double-float d) (+ a b c))))
    (andmap procedure? (list make-bar bar? bar-a bar-b bar-c bar-d)))
  (begin
    (define x (make-bar 9.0 23))
    (and (bar? x)
         #;(equal? (format "~s" x) "#[bar 9.0 23 32.0 64.0]")))
  (eqv? (bar-d x) 64.0)
  (eqv? (bar-b x) 23)
  (let ((y (make-bar 9.0 $mpf32)))
    (eqv? (bar-b y) $mpf32))
  (let ((y (make-bar 9.0 (+ $mpf32 1))))
    (eqv? (bar-b y) (+ $mpf32 1)))
  (let ((y (make-bar 9.0 $mnf32)))
    (eqv? (bar-b y) $mnf32))
  (let ((y (make-bar 9.0 (- $mnf32 1))))
    (eqv? (bar-b y) (- $mnf32 1)))
  (let ((y (make-bar 9.0 #x7fffffff)))
    (eqv? (bar-b y) #x7fffffff))
  (let ((y (make-bar 9.0 #x-80000000)))
    (eqv? (bar-b y) #x-80000000))
  (error? (make-bar 9.0 #x100000000))
  (error? (make-bar 9.0 #x-80000001))
  (error? (make-bar 9.0 23.0))
 ; now that we allow 2^(b-1)..2^b-1
  (let ((y (make-bar 9.0 #x80000000)))
    (eqv? (bar-b y) #x-80000000))
  (let ((y (make-bar 9.0 #xffffffff)))
    (eqv? (bar-b y) #x-1))

 ; make sure we can use modifiers and types as field names
  (equal?
    (let ()
      (define-record foo ((mutable mutable) (immutable int) (immutable integer-32) integer-8))
      (let ([x (make-foo 3 4 5 6)])
        (set-foo-mutable! x 75)
        (list ($record->vector x) (foo-mutable x) (foo-int x) (foo-integer-32 x) (foo-integer-8 x))))
    '(#(foo 75 4 5 6) 75 4 5 6))
  (equal?
    (let ()
      (define foo (make-record-type "hello" '((mutable mutable) (immutable int) (immutable integer-32) integer-8)))
      (let ([x ((record-constructor foo) 3 4 5 6)])
        ((csv7:record-field-mutator foo 'mutable) x 75)
        (list ($record->vector x)
              ((csv7:record-field-accessor foo 'mutable) x)
              ((csv7:record-field-accessor foo 'int) x)
              ((csv7:record-field-accessor foo 'integer-32) x)
              ((csv7:record-field-accessor foo 'integer-8) x))))
    '(#(hello 75 4 5 6) 75 4 5 6))
)

(mat record7
  (begin
    (define-record bar ((immutable a) (immutable unsigned-32 b))
      ((c (+ a b)) ((double-float d) (+ a b c))))
    (andmap procedure? (list make-bar bar? bar-a bar-b bar-c bar-d)))
  (begin
    (define x (make-bar 9.0 23))
    (and (bar? x)
         #;(equal? (format "~s" x) "#[bar 9.0 23 32.0 64.0]")))
  (eqv? (bar-d x) 64.0)
  (eqv? (bar-b x) 23)
  (let ((y (make-bar 9.0 $mpf32)))
    (eqv? (bar-b y) $mpf32))
  (let ((y (make-bar 9.0 (+ $mpf32 1))))
    (eqv? (bar-b y) (+ $mpf32 1)))
  (let ((y (make-bar 9.0 #x7fffffff)))
    (eqv? (bar-b y) #x7fffffff))
  (let ((y (make-bar 9.0 #x80000000)))
    (eqv? (bar-b y) #x80000000))
  (let ((y (make-bar 9.0 #xffffffff)))
    (eqv? (bar-b y) #xffffffff))
  (error? (make-bar 9.0 #x100000000))
  (error? (make-bar 9.0 #x-ffffffff))
  (error? (make-bar 9.0 23.0))
 ; now that we allow 2^(b-1)..2^b-1
  (let ([y (make-bar 9.0 $mnf32)])
    (eqv? (bar-b y) (+ #x100000000 $mnf32)))
  (let ([y (make-bar 9.0 (- $mnf32 1))])
    (eqv? (bar-b y) (+ #x100000000 (- $mnf32 1))))
  (let ([y (make-bar 9.0 -1)])
    (eqv? (bar-b y) #xffffffff))
  (let ([y (make-bar 9.0 #x-80000000)])
    (eqv? (bar-b y) #x80000000))
)

(mat record8
  (let ()
    (define small
      (make-record-type "small"
        (append '((immutable double-float x))
                (map (lambda (x) (gensym)) (make-list 3))
                '((mutable y)))))
    (let ()
      (define make-small (record-constructor small))
      (define small-x (csv7:record-field-accessor small 'x))
      (define small-y (csv7:record-field-accessor small 'y))
      (define set-small-y! (csv7:record-field-mutator small 'y))
      (record-reader 'small small)
      (let ((x (apply make-small (cons 3.4 (make-list 4 'odyssey)))))
        (and (eqv? (string-length (format "~s" x)) 44)
             (begin
               (collect (collect-maximum-generation))
               (set-small-y! x (cons 3 4))
               (let ((p (weak-cons (small-y x) #f)))
                 (collect)
                 (and (eq? (car p) (small-y x))
                      (begin
                        (collect)
                        (eq? (car p) (small-y x))))))))))
  (let ()
    (define huge
      (make-record-type "huge"
        (append '((immutable double-float x))
                (map (lambda (x) (gensym)) (make-list 2000))
                '(y))))
    (let ()
      (define make-huge (record-constructor huge))
      (define huge-x (csv7:record-field-accessor huge 'x))
      (define huge-y (csv7:record-field-accessor huge 'y))
      (define set-huge-y! (csv7:record-field-mutator huge 'y))
      (record-reader 'huge huge)
      (let ((x (apply make-huge (cons 3.4 (make-list 2001 'odyssey)))))
        (and (eqv? (string-length (format "~s" x)) 16019)
             (begin
               (collect (collect-maximum-generation))
               (set-huge-y! x (cons 3 4))
               (let ((p (weak-cons (huge-y x) #f)))
                 (collect)
                 (and (eq? (car p) (huge-y x))
                      (begin
                        (collect)
                        (eq? (car p) (huge-y x))))))))))
)

(mat record9
  (record-type-descriptor? (make-record-type "fudge" '()))
  (begin
    (define fudge (make-record-type "fudge" '((mutable a))))
    (define make-fudge (record-constructor fudge))
    (define fudge? (record-predicate fudge))
    (define fudge.a (csv7:record-field-accessor fudge 'a))
    (define x (make-fudge 3))
    (and (record-type-descriptor? fudge) (fudge? x)))
  (eqv? (fudge.a x) 3)
  (begin
    (define set-fudge.a! (csv7:record-field-mutator fudge 'a))
    (set-fudge.a! x x)
    (eqv? (fudge.a x) x))
  (begin (record-reader 'fudge fudge) #t)
  (begin
    (define y (read (open-input-string "#[fudge 77]")))
    (and (fudge? y)
         (eqv? (fudge.a y) 77)))
  (eq? (record-reader 'fudge) fudge)
  (eq? (record-reader fudge) 'fudge)
  (begin (record-reader 'fudge #f) #t) ; pass name
  (not (record-reader fudge))
  (not (record-reader 'fudge))
  (begin (record-reader 'fudge fudge) #t)
  (eq? (record-reader 'fudge) fudge)
  (eq? (record-reader fudge) 'fudge)
  (error? (record-reader #f))
  (error? (record-reader #f 'fudge))
  (error? (record-reader fudge 'fudge))
  (error? (record-reader #f #f))
  (error? (record-reader 'fudge 'candy))
  (error? (record-reader fudge fudge))
  (begin (record-reader fudge #f) #t) ; pass rtd
  (not (record-reader fudge))
  (not (record-reader 'fudge))
  (begin
    (define fudge (make-record-type "fudge" '((a))))
    (define make-fudge (record-constructor fudge))
    (define fudge? (record-predicate fudge))
    (define fudge.a (csv7:record-field-accessor fudge 'a))
    (define x (make-fudge 3))
    (and (record-type-descriptor? fudge) (fudge? x)))
  (eqv? (fudge.a x) 3)
  (begin
    (define set-fudge.a! (csv7:record-field-mutator fudge 'a))
    (set-fudge.a! x x)
    (eqv? (fudge.a x) x))
  (begin (record-reader 'fudge fudge) #t)
  (begin
    (define y (read (open-input-string "#[fudge 77]")))
    (and (fudge? y)
         (eqv? (fudge.a y) 77)))
)

(mat record10
  (begin
    (define bar (make-record-type "bar"
                  '((immutable a) (mutable b) (immutable c))))
    (define make-bar (record-constructor bar))
    (define bar? (record-predicate bar))
    (define bar.a (csv7:record-field-accessor bar 'a))
    (define bar.b (csv7:record-field-accessor bar 'b))
    (define bar.c (csv7:record-field-accessor bar 'c))
    (define x (make-bar 3 4 5))
    (bar? x))
  (eqv? (bar.b x) 4)
  (begin
    (define set-bar.b! (csv7:record-field-mutator bar 'b))
    (procedure? set-bar.b!))
  (error? (define set-bar.a! (csv7:record-field-mutator bar 'a)))
  (error? (define set-bar.c! (csv7:record-field-mutator bar 'c)))
  (begin (record-reader 'bar bar) #t)
  (let ((x (read (open-input-string "#1=#[bar a #1# c]"))))
    (and (bar? x) (eq? (bar.b x) x)))
  (let ((x (read (open-input-string "#[bar #1=a b #1#]"))))
    (and (bar? x)
         (eq? (bar.a x) 'a)
         (eq? (bar.a x) (bar.c x))
         (eq? (bar.b x) 'b)))
  (error? (read (open-input-string "#1=#[bar a b #1#]")))
  (error? (read (open-input-string "#1=#[bar #1# b c]")))
  (bar? (read (open-input-string "#[bar #1# b #1=a]")))
  (equal?
    (with-output-to-string
      (lambda ()
        (let ([pred (begin
                      (display "one\n")
                      (record-predicate
                        (begin
                          (display "two\n")
                          (make-record-type '#{foo bje68fdhbe06wod3-a} '(x)))))])
          (printf "~s\n" (pred 17))
          (printf "~s\n" (pred ((record-constructor (make-record-type '#{foo bje68fdhbe06wod3-a} '(x))) 55))))))
    "one\ntwo\n#f\n#t\n")
)

#;(mat record11
  (let ()
    (define froz
      (rec froz
        (make-record-type "froz" '((immutable a) (immutable b))
          (lambda (x p wr)
            (define froz.a (csv7:record-field-accessor froz 'a))
            (wr `(* hi john ,(froz.a x) *) p)))))
    (equal? (format "~s" ((record-constructor froz) 1 2))
            "(* hi john 1 *)"))
)

(mat record12
   (begin
     (define-record $tree ((immutable left) (immutable node) (immutable right)))
     (record-type-descriptor? (type-descriptor $tree)))
   ($tree? (make-$tree 3 4 5))
   (let ((tr (make-$tree 'a 'b 'c)))
      (and (eq? ($tree-left tr) 'a)
           (eq? ($tree-node tr) 'b)
           (eq? ($tree-right tr) 'c)))
   (begin
     (define-record $tree ((left) (node) (right)))
     (record-type-descriptor? (type-descriptor $tree)))
   ($tree? (make-$tree 3 4 5))
   (let ((tr (make-$tree 'a 'b 'c)))
      (and (eq? ($tree-left tr) 'a)
           (eq? ($tree-node tr) 'b)
           (eq? ($tree-right tr) 'c)))
   (begin
     (define-record $tree (left node right))
     (record-type-descriptor? (type-descriptor $tree)))
   ($tree? (make-$tree 3 4 5))
   (let ((tr (make-$tree 'a 'b 'c)))
      (and (eq? ($tree-left tr) 'a)
           (eq? ($tree-node tr) 'b)
           (eq? ($tree-right tr) 'c)))
   (begin
     (define-record $tree ((left) (immutable node) (right)))
     (record-type-descriptor? (type-descriptor $tree)))
   ($tree? (make-$tree 3 4 5))
   (let ((tr (make-$tree 'a 'b 'c)))
      (and (eq? ($tree-left tr) 'a)
           (eq? ($tree-node tr) 'b)
           (eq? ($tree-right tr) 'c)))
   (begin
     (define-record pare ((mutable kar) kdr)
       (((scheme-object original-kar) kar) ((mutable original-kdr) kdr)))
     (record-type-descriptor? (type-descriptor pare)))
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

(mat record13
  (begin
    (define-record stretch-string ((integer-32 length) (fill))
      ([(string) (make-string length fill)]))
    (define stretch-string-ref
      (lambda (s i)
        (let ([n (stretch-string-length s)])
          (when (>= i n) (stretch-stretch-string! s (+ i 1) n))
          (string-ref (stretch-string-string s) i))))
    (define stretch-string-set!
      (lambda (s i c)
        (let ([n (stretch-string-length s)])
          (when (>= i n) (stretch-stretch-string! s (+ i 1) n))
          (string-set! (stretch-string-string s) i c))))
    (define stretch-string-fill!
      (lambda (s c)
        (string-fill! (stretch-string-string s) c)
        (set-stretch-string-fill! s c)))
    (define stretch-stretch-string!
      (lambda (s i n)
        (set-stretch-string-length! s i)
        (let ([str (stretch-string-string s)]
              [fill (stretch-string-fill s)])
          (let ([xtra (make-string (- i n) fill)])
            (set-stretch-string-string! s
              (string-append str xtra))))))
    (define ss (make-stretch-string 2 #\X))
    (stretch-string? ss))
  
  (equal? (stretch-string-string ss) "XX")
  (eqv? (stretch-string-ref ss 3) #\X)
  (eqv? (stretch-string-length ss) 4)
  (equal? (stretch-string-string ss) "XXXX")
  
  (begin
    (stretch-string-fill! ss #\@)
    (equal? (stretch-string-string ss) "@@@@"))
  (eqv? (stretch-string-ref ss 5) #\@)
  (equal? (stretch-string-string ss) "@@@@@@")
  
  (begin
    (stretch-string-set! ss 7 #\=)
    (eqv? (stretch-string-length ss) 8))
  (equal? (stretch-string-string ss) "@@@@@@@=")
)

(mat record14
  (begin
    (define-record froz
      ((immutable a) (immutable b))
      (((immutable c) (+ a b)))
      (#;(print-method
         (lambda (x p wr)
           (wr `(* hi john ,(froz-c x) *) p)))))
    (froz? (make-froz 17 23)))
  #;(equal? (format "~s" (make-froz 17 23)) "(* hi john 40 *)")
  (eqv? (froz-a (make-froz 17 23)) 17)
  (let ()
    (define-record pair ((mutable car) (immutable cdr))
      ()
      (#;(print-method
         (lambda (x p wr)
           (display "(" p) ; )
           (wr (car x) p)
           (display " . " p)
           (wr (cdr x) p) ; (
           (display ")" p)))
       (constructor cons)
       (prefix "")))
    (and (pair? (cons 3 4))
         (not (pair? '(3 . 4)))
         (eq? (car (cons 3 4)) 3)
         (eq? (cdr (cons 3 4)) 4)
         #;(equal? (format "~s" (cons 3 (cons 4 '()))) "(3 . (4 . ()))")
         #;(let ((x (cons 3 4)))
           (set-car! x x)
           (equal? (format "~s" x) "#0=(#0# . 4)"))))
)

(mat record15
  (equal? (let ()
            (define-record foo ((mutable a)))
            (let ((x (make-foo '*)))
              (record-reader 'foo (record-rtd x))
              (set-foo-a! x x)
              (parameterize ((print-graph #t))
                (let ((p (open-output-string)))
                  (pretty-print x p)
                  (get-output-string p)))))
          (format "#0=#[foo #0#]~%"))
  (equal? (let ((* "*"))
            (define-record foo (a))
            (let ((x (make-foo *)) (y (make-foo *)))
              (record-reader 'foo (record-rtd x))
              (parameterize ((print-graph #t))
                (format "~s" (list x y)))))
          "(#[foo #0=\"*\"] #[foo #0#])")
)

(mat record16
  (begin
    (define-record bazar ((immutable a) (mutable b) (immutable c))
      ()
      ((prefix "bazar.") #;(reader-name "bazar")))
    (define x (make-bazar 3 4 5))
    (bazar? x))
  (eqv? (bazar.b x) 4)
  (procedure? set-bazar.b!)
  (eqv? (record-reader 'bazar (record-rtd x)) (void))
  (let ((x (read (open-input-string "#1=#[bazar a #1# c]"))))
    (and (bazar? x) (eq? (bazar.b x) x)))
  (let ((x (read (open-input-string "#[bazar #1=a b #1#]"))))
    (and (bazar? x)
         (eq? (bazar.a x) 'a)
         (eq? (bazar.a x) (bazar.c x))
         (eq? (bazar.b x) 'b)))
  (error? (read (open-input-string "#1=#[bazar a b #1#]")))
  (error? (read (open-input-string "#1=#[bazar #1# b c]")))
  (bazar? (read (open-input-string "#[bazar #1# b #1=a]")))
)

(mat record17
  (let ()
    (define-record f ((integer-8 x) (integer-8 y) (integer-32 z)))
    (let ()
      (define r (make-f 1 2 3))
      (and (f? r) (equal? '(3 2 1) (list (f-z r) (f-y r) (f-x r))))))
  (let ()
    (define-record f ((integer-8 x) (integer-8 y) (integer-32 z)))
    (let ()
      (define r (make-f 1 2 3))
      (set-f-x! r 72)
      (set-f-y! r 73)
      (set-f-z! r 74)
      (and (f? r) (equal? '(74 73 72) (list (f-z r) (f-y r) (f-x r))))))
  (let ()
    (define-record f ((integer-8 x) (integer-8 y) (integer-32 z)))
    (let ()
      (define r (make-f 1 2 3))
      (set-f-x! r -72)
      (set-f-y! r -73)
      (set-f-z! r -74)
      (and (f? r) (equal? '(-74 -73 -72) (list (f-z r) (f-y r) (f-x r))))))
  (begin
    (define-record $froz
      ((unsigned-8 x) (double-float y) (single-float z) (unsigned-16 w)))
    (procedure? make-$froz))
  (error? (make-$froz 256 2.5 3.5 0))
  (let ([y (make-$froz -1 2.5 3.5 0)])
    (eqv? ($froz-x y) (+ #x100 -1)))
  (error? (make-$froz -129 2.5 3.5 0))
  (error? (make-$froz 0 2.5 3.5 #x10000))
  (let ([y (make-$froz 0 2.5 3.5 -1)])
    (eqv? ($froz-w y) (+ #x10000 -1)))
  (error? (make-$froz 0 2.5 3.5 #x-8001))
  (error? (make-$froz 0 2 3.5 0))
  (error? (make-$froz 0 2.5 3 0))
  (begin (define $rfroz (make-$froz 1 2.5 3.5 4)) ($froz? $rfroz))
  (eqv? ($froz-x $rfroz) 1)
  (eqv? ($froz-y $rfroz) 2.5)
  (eqv? ($froz-z $rfroz) 3.5)
  (eqv? ($froz-w $rfroz) 4)
  (eqv? (set-$froz-x! $rfroz 2) (void))
  (eqv? (set-$froz-y! $rfroz 2.75) (void))
  (eqv? (set-$froz-z! $rfroz 3.75) (void))
  (eqv? (set-$froz-w! $rfroz 5) (void))
  (eqv? ($froz-x $rfroz) 2)
  (eqv? ($froz-y $rfroz) 2.75)
  (eqv? ($froz-z $rfroz) 3.75)
  (eqv? ($froz-w $rfroz) 5)
  (eqv? (set-$froz-z! $rfroz #b11e111111111) (void))
  (eqv? ($froz-z $rfroz) +inf.0)
  (eqv? (set-$froz-z! $rfroz #b11e-111111111) (void))
  (eqv? ($froz-z $rfroz) 0.0)
  (begin
    (set-$froz-x! $rfroz -1)
    (eqv? ($froz-x $rfroz) (+ #x100 -1)))
  (error? (set-$froz-x! $rfroz 256))
  (error? (set-$froz-x! $rfroz #x-81))
  (error? (set-$froz-y! $rfroz 2))
  (error? (set-$froz-z! $rfroz 2))
  (error? (set-$froz-w! $rfroz #x-8001))
  (begin
    (set-$froz-w! $rfroz -1)
    (eqv? ($froz-w $rfroz) (+ #x10000 -1)))
  (error? (set-$froz-w! $rfroz #x10000))
  (begin
    (define-record $froz ((integer-8 x) (integer-16 w)))
    (procedure? make-$froz))
  (error? (make-$froz 256 0))
  (let ([y (make-$froz #x80 #x8000)])
    (and (eqv? ($froz-x y) #x-80)
         (eqv? ($froz-w y) #x-8000)))
  (error? (make-$froz -129 0))
  (error? (make-$froz 0 #x10000))
  (error? (make-$froz 0 #x-8001))
  (begin (define $rfroz (make-$froz 1 4)) ($froz? $rfroz))
  (eqv? ($froz-x $rfroz) 1)
  (eqv? ($froz-w $rfroz) 4)
  (eqv? (set-$froz-x! $rfroz 2) (void))
  (eqv? (set-$froz-w! $rfroz 5) (void))
  (eqv? ($froz-x $rfroz) 2)
  (eqv? ($froz-w $rfroz) 5)
  (begin (set-$froz-x! $rfroz #xff)
         (set-$froz-w! $rfroz #xffff)
         (eqv? ($froz-x $rfroz) -1)
         (eqv? ($froz-w $rfroz) -1))
  (error? (set-$froz-x! $rfroz 256))
  (error? (set-$froz-x! $rfroz -129))
  (error? (set-$froz-w! $rfroz #x10000))
  (error? (set-$froz-w! $rfroz #x-8001))
)

(mat record18
  (let* ([size 200]
         [ls (map (lambda (x)
                    (let ([name (gensym)])
                      (case (random 6)
                        [(0) `(immutable ,name)]
                        [(1) `(mutable ,name)]
                        [(2) `(integer-32 ,name)]
                        [(3) `(double-float ,name)]
                        [(4) `(single-float ,name)]
                        [(5) `(immutable unsigned-16 ,name)])))
                  (make-list size))])
    (define another
      (lambda (type)
        (case type
          [(scheme-object) (substring "xxlovelyxx" 2 8)]
          [(integer-32)
           (case (random 10)
             [(0) 0]
             [(1) 1]
             [(2) -1]
             [(3) $mpf32]
             [(4) $mnf32]
             [(5) (+ $mpf32 1)]
             [(6) (- $mnf32 1)]
             [(7) #x7fffffff]
             [(8) #x-80000000]
             [(9) (- (random #x100000000) #x80000000)])]
          [(unsigned-16)
           (case (random 6)
             [(0) 0]
             [(1) 1]
             [(2) #x7fff]
             [(3) #x8000]
             [(4) #xffff]
             [(5) (random #x10000)])]
          [(double-float) (if (zero? (random 1)) (random 1e15) (- (random 1e15)))]
          [(single-float) (inexact (random #e1e7))]
          [else (errorf #f "unexpected type ~s" type)])))
    (let ([rtd (make-record-type "big" ls)])
      (let ([accessors (map (lambda (x) (csv7:record-field-accessor rtd x))
                            (csv7:record-type-field-names rtd))]
            [mutators (map (lambda (x)
                             (and (csv7:record-field-mutable? rtd x)
                                  (csv7:record-field-mutator rtd x)))
                            (csv7:record-type-field-names rtd))]
            [vals (map another (map cadr (csv7:record-type-field-decls rtd)))])
        (let ([inst (apply (record-constructor rtd) vals)])
          (let f ((n 2000) (vals vals))
            (unless (= n 0)
              (if (= (modulo n 20) 0) (collect))
              (f (- n 1)
                 (map (lambda (acc mut! val type)
                        (let ([ival (acc inst)])
                          (unless (and (eqv? ival val)
                                       (or (not (string? ival))
                                           (string=? ival "lovely")))
                            (errorf #f "unexpected value ~s; should have been ~s"
                              ival val)))
                        (if (and mut! (= (random 10) 3))
                            (let ([nval (another type)])
                              (mut! inst nval)
                              nval)
                            val))
                       accessors
                       mutators
                       vals
                       (map cadr (csv7:record-type-field-decls rtd)))))))))
    #t)
)

(mat foreign-data
  (begin
    (module ($fd-unaligned-integers $fd-unaligned-floats)
      (define-syntax define-constant
        (syntax-rules (machine-type)
          [(_ machine-type y) (begin)]
          [(_ x y) (define x y)]))
      (define-syntax features
        (syntax-rules ()
          [(_ x ...) (begin)]))
      (define-syntax constant
        (syntax-rules ()
          [(_ x) x]))
      (define-syntax constant-case
        (syntax-rules (else)
          [(_ const [(k ...) e1 e2 ...] ... [else ee1 ee2 ...])
           (meta-cond
             [(memv (constant const) '(k ...)) e1 e2 ...]
             ...
             [else ee1 ee2 ...])]
          [(_ const [(k ...) e1 e2 ...] ...)
           (meta-cond
             [(memv (constant const) '(k ...)) e1 e2 ...]
             ...
             [else (syntax-error const
                     (format "unhandled value ~s" (constant const)))])]))
      (define-syntax include-from-s
        (lambda (x)
          (syntax-case x ()
            [(k ?path)
             (string? (datum ?path))
             (let ([s-path (format "~a/../s/~a" *mats-dir* (datum ?path))])
               (datum->syntax #'k `(include ,s-path)))])))
      (include-from-s "machine.def")
     ; all this work for two constants:
      (define $fd-unaligned-integers (constant unaligned-integers))
      (define $fd-unaligned-floats (constant unaligned-floats)))
    (define ($fd-make-min bytes) (- (ash (expt 256 bytes) -1)))
    (define ($fd-make-max bytes) (- (expt 256 bytes) 1))
    (define $fd-addr-min ($fd-make-min (foreign-sizeof 'void*)))
    (define $fd-addr-max ($fd-make-max (foreign-sizeof 'void*)))
    (define $fd-int-min ($fd-make-min (foreign-sizeof 'int)))
    (define $fd-int-max ($fd-make-max (foreign-sizeof 'int)))
    (define $fd-short-min ($fd-make-min (foreign-sizeof 'short)))
    (define $fd-short-max ($fd-make-max (foreign-sizeof 'short)))
    (define $fd-long-min ($fd-make-min (foreign-sizeof 'long)))
    (define $fd-long-max ($fd-make-max (foreign-sizeof 'long)))
    (define $fd-long-long-min ($fd-make-min (foreign-sizeof 'long-long)))
    (define $fd-long-long-max ($fd-make-max (foreign-sizeof 'long-long)))
    (define $fd-char-max ($fd-make-max 1))
    (define $fd-wchar-max (min ($fd-make-max (foreign-sizeof 'wchar)) #x10ffff))
    (define $fd-i8-min ($fd-make-min 1))
    (define $fd-i8-max ($fd-make-max 1))
    (define $fd-i16-min ($fd-make-min 2))
    (define $fd-i16-max ($fd-make-max 2))
    (define $fd-i32-min ($fd-make-min 4))
    (define $fd-i32-max ($fd-make-max 4))
    (define $fd-i64-min ($fd-make-min 8))
    (define $fd-i64-max ($fd-make-max 8))
    #t)
 ; foreign-alloc
  (error? ; not a positive fixnum
    (foreign-alloc 0))
  (error? ; not a positive fixnum
    (foreign-alloc (+ (most-positive-fixnum) 1)))
  (error? ; not a positive fixnum
    (foreign-alloc -5))
  (error? ; not a positive fixnum
    (foreign-alloc 17.0))
 ; foreign-free
  (error? ; invalid address
    (foreign-free 17.0))
  (error? ; invalid address
    (foreign-free (- $fd-addr-min 1)))
  (error? ; invalid address
    (foreign-free (+ $fd-addr-max 1)))
  (equal?
    (let ([x (foreign-alloc 16)])
      (list
        (<= 0 x $fd-addr-max)
        (foreign-free x)))
   (list #t (void)))
  ; foreign-ref
  (begin
    (define $max-uptr+1
      (cond
        [(fx= (fixnum-width) 30) #x100000000]
        [(fx= (fixnum-width) 61) #x10000000000000000]
        [else ($oops '$raw-fd-a "unexpected fixnum-width ~s" (fixnum-width))]))
    #t)
  (error? ; invalid address
    (foreign-ref 'integer-32 $max-uptr+1 0))
  (error? ; invalid address
    (foreign-ref 'integer-32 (- $max-uptr+1) 100))
  (error? ; invalid offset
    (foreign-ref 'integer-32 0 (+ (most-positive-fixnum) 1)))
  (error? ; invalid addr + offset
    (foreign-ref 'integer-32 (- $max-uptr+1 4) 4))
  (error? ; invalid addr + offset for 4-byte type
    (foreign-ref 'integer-32 (- $max-uptr+1 8) 6))
  (error? ; invalid address
    (foreign-set! 'integer-32 $max-uptr+1 0 7))
  (error? ; invalid address
    (foreign-set! 'integer-32 (- $max-uptr+1) 100 7))
  (error? ; invalid offset
    (foreign-set! 'integer-32 0 (+ (most-positive-fixnum) 1) 7))
  (error? ; invalid addr + offset
    (foreign-set! 'integer-32 (- $max-uptr+1 4) 4 7))
  (error? ; invalid addr + offset for 4-byte type
    (foreign-set! 'integer-32 (- $max-uptr+1 8) 6 7))
  (meta-cond
    [(fx= (fixnum-width) 30)
     (define $real-fd-a (foreign-alloc (+ 40 7)))
     (define $fd-a (logand (+ $real-fd-a 7) -8))
     (define $raw-fd-a (ash (if (>= $fd-a (expt 2 31)) (- $fd-a (expt 2 32)) $fd-a) -2))
     (and
       (<= 0 $fd-a $fd-addr-max)
       (fixnum? $raw-fd-a))]
    [(fx= (fixnum-width) 61)
     (define $real-fd-a (foreign-alloc (+ 40 7)))
     (define $fd-a (logand (+ $real-fd-a 7) -8))
     (define $raw-fd-a (ash (if (>= $fd-a (expt 2 63)) (- $fd-a (expt 2 64)) $fd-a) -3))
     (and
       (<= 0 $fd-a $fd-addr-max)
       (fixnum? $raw-fd-a))]
    [else ($oops '$raw-fd-a "unexpected fixnum-width ~s" (fixnum-width))])
  (error? ; invalid type
    (foreign-ref 'aint $fd-a 0))
  (error? ; invalid type
    (foreign-ref 'ptr $fd-a 0))
  (error? ; invalid type
    (foreign-ref 'scheme-object $fd-a 0))
  (begin
    (define $fd-f (lambda () (foreign-ref 'ptr $fd-a 0)))
    (procedure? $fd-f))
  (error? ; invalid type
    ($fd-f))
  (begin
    (define $fd-f (lambda (x) (foreign-ref x $fd-a 0)))
    (procedure? $fd-f))
  (error? ; invalid type
    ($fd-f 'ptr))
  (error? ; invalid address
    (foreign-ref 'int 7.5 0))
  (error? ; invalid address
    (foreign-ref 'int (- $fd-addr-min 1) 0))
  (error? ; invalid address
    (foreign-ref 'int (+ $fd-addr-max 1) 0))
  (error? ; not a fixnum
    (foreign-ref 'int $fd-a 0.0))
  (error? ; not a fixnum
    (foreign-ref 'int $fd-a (+ (most-positive-fixnum) 1)))
  (error? ; not a fixnum
    (foreign-ref 'int $fd-a (- (most-negative-fixnum) 1)))
 ; foreign-set!
  (error? ; invalid type
    (foreign-set! 'aint $fd-a 0 17))
  (error? ; invalid type
    (foreign-set! 'ptr $fd-a 0 17))
  (error? ; invalid type
    (foreign-set! 'scheme-object $fd-a 0 17))
  (begin
    (define $fd-f (lambda () (foreign-set! 'ptr $fd-a 0 17)))
    (procedure? $fd-f))
  (error? ; invalid type
    ($fd-f))
  (begin
    (define $fd-f (lambda (x) (foreign-set! x $fd-a 0 17)))
    (procedure? $fd-f))
  (error? ; invalid type
    ($fd-f 'ptr))
  (error? ; invalid address
    (foreign-set! 'int 7.5 0 17))
  (error? ; invalid address
    (foreign-set! 'int (- $fd-addr-min 1) 0 17))
  (error? ; invalid address
    (foreign-set! 'int (+ $fd-addr-max 1) 0 17))
  (error? ; not a fixnum
    (foreign-set! 'int $fd-a 0.0 17))
  (error? ; not a fixnum
    (foreign-set! 'int $fd-a (+ (most-positive-fixnum) 1) 17))
  (error? ; not a fixnum
    (foreign-set! 'int $fd-a (- (most-negative-fixnum) 1) 17))
 ; integer-8/unsigned-8
  (error? ; invalid value for type
    (foreign-set! 'integer-8 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'integer-8 $fd-a 0 (- $fd-i8-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'integer-8 $fd-a 0 (+ $fd-i8-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-8 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-8 $fd-a 0 (- $fd-i8-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-8 $fd-a 0 (+ $fd-i8-max 1)))
  (equal?
    (begin
      (foreign-set! 'integer-8 $fd-a 3 255)
      (list (foreign-ref 'integer-8 $fd-a 3)
            (foreign-ref 'unsigned-8 $fd-a 3)))
    '(-1 255))
  (equal?
    (begin
      (foreign-set! 'unsigned-8 $fd-a 5 -5)
      (list (foreign-ref 'integer-8 $fd-a 5)
            (foreign-ref 'unsigned-8 $fd-a 5)))
    '(-5 251))
  (equal?
    (begin
      (foreign-set! 'integer-8 $fd-a 0 #x-80)
      (foreign-set! 'integer-8 $fd-a 1 0)
      (foreign-set! 'integer-8 $fd-a 2 #x7f)
      (foreign-set! 'integer-8 $fd-a 3 #x80)
      (foreign-set! 'integer-8 $fd-a 4 #xff)
      (list (foreign-ref 'integer-8 $fd-a 0)
            (foreign-ref 'integer-8 $fd-a 1)
            (foreign-ref 'integer-8 $fd-a 2)
            (foreign-ref 'integer-8 $fd-a 3)
            (foreign-ref 'integer-8 $fd-a 4)
            (foreign-ref 'unsigned-8 $fd-a 0)
            (foreign-ref 'unsigned-8 $fd-a 1)
            (foreign-ref 'unsigned-8 $fd-a 2)
            (foreign-ref 'unsigned-8 $fd-a 3)
            (foreign-ref 'unsigned-8 $fd-a 4)))
    `(#x-80 0 #x7f #x-80 -1
      #x80 0 #x7f #x80 #xff))
  (equal?
    (begin
      (foreign-set! 'unsigned-8 $fd-a 0 #x-80)
      (foreign-set! 'unsigned-8 $fd-a 1 0)
      (foreign-set! 'unsigned-8 $fd-a 2 #x7f)
      (foreign-set! 'unsigned-8 $fd-a 3 #x80)
      (foreign-set! 'unsigned-8 $fd-a 4 #xff)
      (list (foreign-ref 'integer-8 $fd-a 0)
            (foreign-ref 'integer-8 $fd-a 1)
            (foreign-ref 'integer-8 $fd-a 2)
            (foreign-ref 'integer-8 $fd-a 3)
            (foreign-ref 'integer-8 $fd-a 4)
            (foreign-ref 'unsigned-8 $fd-a 0)
            (foreign-ref 'unsigned-8 $fd-a 1)
            (foreign-ref 'unsigned-8 $fd-a 2)
            (foreign-ref 'unsigned-8 $fd-a 3)
            (foreign-ref 'unsigned-8 $fd-a 4)))
    `(#x-80 0 #x7f #x-80 -1
      #x80 0 #x7f #x80 #xff))
 ; integer-16/unsigned-16
  (error? ; invalid value for type
    (foreign-set! 'integer-16 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'integer-16 $fd-a 0 (- $fd-i16-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'integer-16 $fd-a 0 (+ $fd-i16-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-16 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-16 $fd-a 0 (- $fd-i16-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-16 $fd-a 0 (+ $fd-i16-max 1)))
  (equal?
    (begin
      (foreign-set! 'integer-16 $fd-a 2 #xabcd)
      (list (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 2)))
    `(,(- #xabcd #x10000) #xabcd))
  (equal?
    (begin
      (foreign-set! 'unsigned-16 $fd-a 2 -5321)
      (list (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 2)))
    `(-5321 ,(+ -5321 #x10000)))
  (equal?
    (begin
      (foreign-set! 'integer-16 $fd-a 0 #x-8000)
      (foreign-set! 'integer-16 $fd-a 2 0)
      (foreign-set! 'integer-16 $fd-a 4 #x7fff)
      (foreign-set! 'integer-16 $fd-a 6 #x8000)
      (foreign-set! 'integer-16 $fd-a 8 #xffff)
      (list (foreign-ref 'integer-16 $fd-a 0)
            (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'integer-16 $fd-a 4)
            (foreign-ref 'integer-16 $fd-a 6)
            (foreign-ref 'integer-16 $fd-a 8)
            (foreign-ref 'unsigned-16 $fd-a 0)
            (foreign-ref 'unsigned-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 4)
            (foreign-ref 'unsigned-16 $fd-a 6)
            (foreign-ref 'unsigned-16 $fd-a 8)))
    `(#x-8000 0 #x7fff #x-8000 -1
      #x8000 0 #x7fff #x8000 #xffff))
  (equal?
    (begin
      (foreign-set! 'unsigned-16 $fd-a 0 #x-8000)
      (foreign-set! 'unsigned-16 $fd-a 2 0)
      (foreign-set! 'unsigned-16 $fd-a 4 #x7fff)
      (foreign-set! 'unsigned-16 $fd-a 6 #x8000)
      (foreign-set! 'unsigned-16 $fd-a 8 #xffff)
      (list (foreign-ref 'integer-16 $fd-a 0)
            (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'integer-16 $fd-a 4)
            (foreign-ref 'integer-16 $fd-a 6)
            (foreign-ref 'integer-16 $fd-a 8)
            (foreign-ref 'unsigned-16 $fd-a 0)
            (foreign-ref 'unsigned-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 4)
            (foreign-ref 'unsigned-16 $fd-a 6)
            (foreign-ref 'unsigned-16 $fd-a 8)))
    `(#x-8000 0 #x7fff #x-8000 -1
      #x8000 0 #x7fff #x8000 #xffff))
 ; integer-32/unsigned-32
  (error? ; invalid value for type
    (foreign-set! 'integer-32 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'integer-32 $fd-a 0 (- $fd-i32-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'integer-32 $fd-a 0 (+ $fd-i32-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-32 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-32 $fd-a 0 (- $fd-i32-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-32 $fd-a 0 (+ $fd-i32-max 1)))
  (equal?
    (begin
      (foreign-set! 'integer-32 $fd-a 0 #x-80000000)
      (foreign-set! 'integer-32 $fd-a 4 0)
      (foreign-set! 'integer-32 $fd-a 8 #x7fffffff)
      (foreign-set! 'integer-32 $fd-a 12 #x80000000)
      (foreign-set! 'integer-32 $fd-a 16 #xffffffff)
      (list (foreign-ref 'integer-32 $fd-a 0)
            (foreign-ref 'integer-32 $fd-a 4)
            (foreign-ref 'integer-32 $fd-a 8)
            (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'integer-32 $fd-a 16)
            (foreign-ref 'unsigned-32 $fd-a 0)
            (foreign-ref 'unsigned-32 $fd-a 4)
            (foreign-ref 'unsigned-32 $fd-a 8)
            (foreign-ref 'unsigned-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 16)))
    `(#x-80000000 0 #x7fffffff #x-80000000 -1
      #x80000000 0 #x7fffffff #x80000000 #xffffffff))
  (equal?
    (begin
      (foreign-set! 'unsigned-32 $fd-a 0 #x-80000000)
      (foreign-set! 'unsigned-32 $fd-a 4 0)
      (foreign-set! 'unsigned-32 $fd-a 8 #x7fffffff)
      (foreign-set! 'unsigned-32 $fd-a 12 #x80000000)
      (foreign-set! 'unsigned-32 $fd-a 16 #xffffffff)
      (list (foreign-ref 'integer-32 $fd-a 0)
            (foreign-ref 'integer-32 $fd-a 4)
            (foreign-ref 'integer-32 $fd-a 8)
            (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'integer-32 $fd-a 16)
            (foreign-ref 'unsigned-32 $fd-a 0)
            (foreign-ref 'unsigned-32 $fd-a 4)
            (foreign-ref 'unsigned-32 $fd-a 8)
            (foreign-ref 'unsigned-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 16)))
    `(#x-80000000 0 #x7fffffff #x-80000000 -1
      #x80000000 0 #x7fffffff #x80000000 #xffffffff))
  (equal?
    (begin
      (foreign-set! 'integer-32 $fd-a 12 #xabcd1234)
      (list (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 12)))
    `(,(- #xabcd1234 #x100000000) #xabcd1234))
  (equal?
    (begin
      (foreign-set! 'unsigned-32 $fd-a 12 #x-765321ab)
      (list (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 12)))
    `(#x-765321ab ,(+ #x-765321ab #x100000000)))
 ; integer-64/unsigned-64
  (error? ; invalid value for type
    (foreign-set! 'integer-64 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'integer-64 $fd-a 0 (- $fd-i64-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'integer-64 $fd-a 0 (+ $fd-i64-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-64 $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-64 $fd-a 0 (- $fd-i64-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-64 $fd-a 0 (+ $fd-i64-max 1)))
  (equal?
    (begin
      (foreign-set! 'integer-64 $fd-a 16 #xabcd1234ffee8765)
      (list (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
    `(,(- #xabcd1234ffee8765 #x10000000000000000)
      #xabcd1234ffee8765
      ,(- #xabcd1234ffee8765 #x10000000000000000)
      #xabcd1234ffee8765))
  (equal?
    (begin
      (foreign-set! 'unsigned-64 $fd-a 16 #x-765321ab4c8e9de1)
      (list (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
    `(#x-765321ab4c8e9de1
      ,(+ #x-765321ab4c8e9de1 #x10000000000000000)
      #x-765321ab4c8e9de1
      ,(+ #x-765321ab4c8e9de1 #x10000000000000000)))
  (equal?
    (begin
      (foreign-set! 'integer-64 $fd-a 0 #x-8000000000000000)
      (foreign-set! 'integer-64 $fd-a 8 0)
      (foreign-set! 'integer-64 $fd-a 16 #x7fffffffffffffff)
      (foreign-set! 'integer-64 $fd-a 24 #x8000000000000000)
      (foreign-set! 'integer-64 $fd-a 32 #xffffffffffffffff)
      (list (foreign-ref 'integer-64 $fd-a 0)
            (foreign-ref 'integer-64 $fd-a 8)
            (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'integer-64 $fd-a 24)
            (foreign-ref 'integer-64 $fd-a 32)
            (foreign-ref 'unsigned-64 $fd-a 0)
            (foreign-ref 'unsigned-64 $fd-a 8)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 24)
            (foreign-ref 'unsigned-64 $fd-a 32)))
    `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
      #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
  (equal?
    (begin
      (foreign-set! 'unsigned-64 $fd-a 0 #x-8000000000000000)
      (foreign-set! 'unsigned-64 $fd-a 8 0)
      (foreign-set! 'unsigned-64 $fd-a 16 #x7fffffffffffffff)
      (foreign-set! 'unsigned-64 $fd-a 24 #x8000000000000000)
      (foreign-set! 'unsigned-64 $fd-a 32 #xffffffffffffffff)
      (list (foreign-ref 'integer-64 $fd-a 0)
            (foreign-ref 'integer-64 $fd-a 8)
            (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'integer-64 $fd-a 24)
            (foreign-ref 'integer-64 $fd-a 32)
            (foreign-ref 'unsigned-64 $fd-a 0)
            (foreign-ref 'unsigned-64 $fd-a 8)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 24)
            (foreign-ref 'unsigned-64 $fd-a 32)))
    `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
      #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
 ; iptr/uptr
  (error? ; invalid value for type
    (foreign-set! 'iptr $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'iptr $fd-a 0 (- $fd-addr-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'iptr $fd-a 0 (+ $fd-addr-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'uptr $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'uptr $fd-a 0 (- $fd-addr-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'uptr $fd-a 0 (+ $fd-addr-max 1)))
  (case $fd-addr-max
    [(#xffffffff)
     (and
       (equal?
         (begin
           (foreign-set! 'iptr $fd-a 12 #xabcd1234)
           (list (foreign-ref 'iptr $fd-a 12)
                 (foreign-ref 'uptr $fd-a 12)
                 (foreign-ref 'integer-32 $fd-a 12)
                 (foreign-ref 'unsigned-32 $fd-a 12)))
         `(,(- #xabcd1234 #x100000000)
           #xabcd1234
           ,(- #xabcd1234 #x100000000)
           #xabcd1234))
       (equal?
         (begin
           (foreign-set! 'uptr $fd-a 12 #x-765321ab)
           (list (foreign-ref 'iptr $fd-a 12)
                 (foreign-ref 'uptr $fd-a 12)
                 (foreign-ref 'integer-32 $fd-a 12)
                 (foreign-ref 'unsigned-32 $fd-a 12)))
         `(#x-765321ab
          ,(+ #x-765321ab #x100000000)
          #x-765321ab
          ,(+ #x-765321ab #x100000000))))]
    [(#xffffffffffffffff)
     (and
       (equal?
         (begin
           (foreign-set! 'iptr $fd-a 16 #xabcd1234ffee8765)
           (list (foreign-ref 'iptr $fd-a 16)
                 (foreign-ref 'uptr $fd-a 16)
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
         `(,(- #xabcd1234ffee8765 #x10000000000000000)
           #xabcd1234ffee8765
           ,(- #xabcd1234ffee8765 #x10000000000000000)
           #xabcd1234ffee8765))
       (equal?
         (begin
           (foreign-set! 'uptr $fd-a 16 #x-765321ab4c8e9de1)
           (list (foreign-ref 'iptr $fd-a 16)
                 (foreign-ref 'uptr $fd-a 16)
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
         `(#x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000)
           #x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000)))
       (equal?
         (begin
           (foreign-set! 'void* $fd-a 16 #x-765321ab4c8e9de1)
           (list (foreign-ref 'iptr $fd-a 16)
                 (foreign-ref 'void* $fd-a 16)
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
         `(#x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000)
           #x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000))))]
    [else (error 'foreign-data-mat "unexpected $fd-addr-max ~s" $fd-addr-max)])
 ; int/unsigned
  (error? ; invalid value for type
    (foreign-set! 'int $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'int $fd-a 0 (- $fd-int-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'int $fd-a 0 (+ $fd-int-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned $fd-a 0 (- $fd-int-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned $fd-a 0 (+ $fd-int-max 1)))
  (case $fd-int-max
    [(#xffffffff)
     (and
       (equal?
         (begin
           (foreign-set! 'int $fd-a 0 #x-80000000)
           (foreign-set! 'int $fd-a 4 0)
           (foreign-set! 'int $fd-a 8 #x7fffffff)
           (foreign-set! 'int $fd-a 12 #x80000000)
           (foreign-set! 'int $fd-a 16 #xffffffff)
           (list (foreign-ref 'int $fd-a 0)
                 (foreign-ref 'int $fd-a 4)
                 (foreign-ref 'int $fd-a 8)
                 (foreign-ref 'int $fd-a 12)
                 (foreign-ref 'int $fd-a 16)
                 (foreign-ref 'unsigned $fd-a 0)
                 (foreign-ref 'unsigned $fd-a 4)
                 (foreign-ref 'unsigned $fd-a 8)
                 (foreign-ref 'unsigned $fd-a 12)
                 (foreign-ref 'unsigned $fd-a 16)))
         `(#x-80000000 0 #x7fffffff #x-80000000 -1
           #x80000000 0 #x7fffffff #x80000000 #xffffffff))
       (equal?
         (begin
           (foreign-set! 'unsigned $fd-a 0 #x-80000000)
           (foreign-set! 'unsigned $fd-a 4 0)
           (foreign-set! 'unsigned $fd-a 8 #x7fffffff)
           (foreign-set! 'unsigned $fd-a 12 #x80000000)
           (foreign-set! 'unsigned $fd-a 16 #xffffffff)
           (list (foreign-ref 'int $fd-a 0)
                 (foreign-ref 'int $fd-a 4)
                 (foreign-ref 'int $fd-a 8)
                 (foreign-ref 'int $fd-a 12)
                 (foreign-ref 'int $fd-a 16)
                 (foreign-ref 'unsigned $fd-a 0)
                 (foreign-ref 'unsigned $fd-a 4)
                 (foreign-ref 'unsigned $fd-a 8)
                 (foreign-ref 'unsigned $fd-a 12)
                 (foreign-ref 'unsigned $fd-a 16)))
         `(#x-80000000 0 #x7fffffff #x-80000000 -1
           #x80000000 0 #x7fffffff #x80000000 #xffffffff))
       (equal?
         (begin
           (foreign-set! 'unsigned-int $fd-a 0 #x-80000000)
           (foreign-set! 'unsigned-int $fd-a 4 0)
           (foreign-set! 'unsigned-int $fd-a 8 #x7fffffff)
           (foreign-set! 'unsigned-int $fd-a 12 #x80000000)
           (foreign-set! 'unsigned-int $fd-a 16 #xffffffff)
           (list (foreign-ref 'int $fd-a 0)
                 (foreign-ref 'int $fd-a 4)
                 (foreign-ref 'int $fd-a 8)
                 (foreign-ref 'int $fd-a 12)
                 (foreign-ref 'int $fd-a 16)
                 (foreign-ref 'unsigned-int $fd-a 0)
                 (foreign-ref 'unsigned-int $fd-a 4)
                 (foreign-ref 'unsigned-int $fd-a 8)
                 (foreign-ref 'unsigned-int $fd-a 12)
                 (foreign-ref 'unsigned-int $fd-a 16)))
         `(#x-80000000 0 #x7fffffff #x-80000000 -1
           #x80000000 0 #x7fffffff #x80000000 #xffffffff))
       (equal?
         (begin
           (foreign-set! 'int $fd-a 12 #xabcd1234)
           (list (foreign-ref 'int $fd-a 12)
                 (foreign-ref 'unsigned $fd-a 12)
                 (foreign-ref 'integer-32 $fd-a 12)
                 (foreign-ref 'unsigned-32 $fd-a 12)))
         `(,(- #xabcd1234 #x100000000)
           #xabcd1234
           ,(- #xabcd1234 #x100000000)
           #xabcd1234))
       (equal?
         (begin
           (foreign-set! 'unsigned $fd-a 12 #x-765321ab)
           (list (foreign-ref 'int $fd-a 12)
                 (foreign-ref 'unsigned $fd-a 12)
                 (foreign-ref 'integer-32 $fd-a 12)
                 (foreign-ref 'unsigned-32 $fd-a 12)))
         `(#x-765321ab
          ,(+ #x-765321ab #x100000000)
          #x-765321ab
          ,(+ #x-765321ab #x100000000))))]
    [else (error 'foreign-data-mat "unexpected $fd-int-max ~s" $fd-int-max)])
 ; short/unsigned-short
  (error? ; invalid value for type
    (foreign-set! 'short $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'short $fd-a 0 (- $fd-short-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'short $fd-a 0 (+ $fd-short-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-short $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-short $fd-a 0 (- $fd-short-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-short $fd-a 0 (+ $fd-short-max 1)))
  (case $fd-short-max
    [(#xffff)
     (and
       (equal?
         (begin
           (foreign-set! 'short $fd-a 0 #x-8000)
           (foreign-set! 'short $fd-a 2 0)
           (foreign-set! 'short $fd-a 4 #x7fff)
           (foreign-set! 'short $fd-a 6 #x8000)
           (foreign-set! 'short $fd-a 8 #xffff)
           (list (foreign-ref 'short $fd-a 0)
                 (foreign-ref 'short $fd-a 2)
                 (foreign-ref 'short $fd-a 4)
                 (foreign-ref 'short $fd-a 6)
                 (foreign-ref 'short $fd-a 8)
                 (foreign-ref 'unsigned-short $fd-a 0)
                 (foreign-ref 'unsigned-short $fd-a 2)
                 (foreign-ref 'unsigned-short $fd-a 4)
                 (foreign-ref 'unsigned-short $fd-a 6)
                 (foreign-ref 'unsigned-short $fd-a 8)))
         `(#x-8000 0 #x7fff #x-8000 -1
           #x8000 0 #x7fff #x8000 #xffff))
       (equal?
         (begin
           (foreign-set! 'unsigned-short $fd-a 0 #x-8000)
           (foreign-set! 'unsigned-short $fd-a 2 0)
           (foreign-set! 'unsigned-short $fd-a 4 #x7fff)
           (foreign-set! 'unsigned-short $fd-a 6 #x8000)
           (foreign-set! 'unsigned-short $fd-a 8 #xffff)
           (list (foreign-ref 'short $fd-a 0)
                 (foreign-ref 'short $fd-a 2)
                 (foreign-ref 'short $fd-a 4)
                 (foreign-ref 'short $fd-a 6)
                 (foreign-ref 'short $fd-a 8)
                 (foreign-ref 'unsigned-short $fd-a 0)
                 (foreign-ref 'unsigned-short $fd-a 2)
                 (foreign-ref 'unsigned-short $fd-a 4)
                 (foreign-ref 'unsigned-short $fd-a 6)
                 (foreign-ref 'unsigned-short $fd-a 8)))
         `(#x-8000 0 #x7fff #x-8000 -1
           #x8000 0 #x7fff #x8000 #xffff))
       (equal?
         (begin
           (foreign-set! 'short $fd-a 2 #xabcd)
           (list (foreign-ref 'short $fd-a 2)
                 (foreign-ref 'unsigned-short $fd-a 2)
                 (foreign-ref 'integer-16 $fd-a 2)
                 (foreign-ref 'unsigned-16 $fd-a 2)))
         `(,(- #xabcd #x10000) #xabcd ,(- #xabcd #x10000) #xabcd))
       (equal?
         (begin
           (foreign-set! 'unsigned-short $fd-a 2 -5321)
           (list (foreign-ref 'short $fd-a 2)
                 (foreign-ref 'unsigned-short $fd-a 2)
                 (foreign-ref 'integer-16 $fd-a 2)
                 (foreign-ref 'unsigned-16 $fd-a 2)))
         `(-5321 ,(+ -5321 #x10000) -5321 ,(+ -5321 #x10000))))]
    [else (error 'foreign-data-mat "unexpected $fd-short-max ~s" $fd-short-max)])
 ; long/unsigned-long
  (error? ; invalid value for type
    (foreign-set! 'long $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'long $fd-a 0 (- $fd-long-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'long $fd-a 0 (+ $fd-long-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-long $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-long $fd-a 0 (- $fd-long-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-long $fd-a 0 (+ $fd-long-max 1)))
  (case $fd-long-max
    [(#xffffffff)
     (and
       (equal?
         (begin
           (foreign-set! 'long $fd-a 0 #x-80000000)
           (foreign-set! 'long $fd-a 4 0)
           (foreign-set! 'long $fd-a 8 #x7fffffff)
           (foreign-set! 'long $fd-a 12 #x80000000)
           (foreign-set! 'long $fd-a 16 #xffffffff)
           (list (foreign-ref 'long $fd-a 0)
                 (foreign-ref 'long $fd-a 4)
                 (foreign-ref 'long $fd-a 8)
                 (foreign-ref 'long $fd-a 12)
                 (foreign-ref 'long $fd-a 16)
                 (foreign-ref 'unsigned-long $fd-a 0)
                 (foreign-ref 'unsigned-long $fd-a 4)
                 (foreign-ref 'unsigned-long $fd-a 8)
                 (foreign-ref 'unsigned-long $fd-a 12)
                 (foreign-ref 'unsigned-long $fd-a 16)))
         `(#x-80000000 0 #x7fffffff #x-80000000 -1
           #x80000000 0 #x7fffffff #x80000000 #xffffffff))
       (equal?
         (begin
           (foreign-set! 'unsigned-long $fd-a 0 #x-80000000)
           (foreign-set! 'unsigned-long $fd-a 4 0)
           (foreign-set! 'unsigned-long $fd-a 8 #x7fffffff)
           (foreign-set! 'unsigned-long $fd-a 12 #x80000000)
           (foreign-set! 'unsigned-long $fd-a 16 #xffffffff)
           (list (foreign-ref 'long $fd-a 0)
                 (foreign-ref 'long $fd-a 4)
                 (foreign-ref 'long $fd-a 8)
                 (foreign-ref 'long $fd-a 12)
                 (foreign-ref 'long $fd-a 16)
                 (foreign-ref 'unsigned-long $fd-a 0)
                 (foreign-ref 'unsigned-long $fd-a 4)
                 (foreign-ref 'unsigned-long $fd-a 8)
                 (foreign-ref 'unsigned-long $fd-a 12)
                 (foreign-ref 'unsigned-long $fd-a 16)))
         `(#x-80000000 0 #x7fffffff #x-80000000 -1
           #x80000000 0 #x7fffffff #x80000000 #xffffffff))
       (equal?
         (begin
           (foreign-set! 'long $fd-a 12 #xabcd1234)
           (list (foreign-ref 'long $fd-a 12)
                 (foreign-ref 'unsigned-long $fd-a 12)
                 (foreign-ref 'integer-32 $fd-a 12)
                 (foreign-ref 'unsigned-32 $fd-a 12)))
         `(,(- #xabcd1234 #x100000000)
           #xabcd1234
           ,(- #xabcd1234 #x100000000)
           #xabcd1234))
       (equal?
         (begin
           (foreign-set! 'unsigned-long $fd-a 12 #x-765321ab)
           (list (foreign-ref 'long $fd-a 12)
                 (foreign-ref 'unsigned-long $fd-a 12)
                 (foreign-ref 'integer-32 $fd-a 12)
                 (foreign-ref 'unsigned-32 $fd-a 12)))
         `(#x-765321ab
          ,(+ #x-765321ab #x100000000)
          #x-765321ab
          ,(+ #x-765321ab #x100000000))))]
    [(#xffffffffffffffff)
     (and
       (equal?
         (begin
           (foreign-set! 'long $fd-a 0 #x-8000000000000000)
           (foreign-set! 'long $fd-a 8 0)
           (foreign-set! 'long $fd-a 16 #x7fffffffffffffff)
           (foreign-set! 'long $fd-a 24 #x8000000000000000)
           (foreign-set! 'long $fd-a 32 #xffffffffffffffff)
           (list (foreign-ref 'long $fd-a 0)
                 (foreign-ref 'long $fd-a 8)
                 (foreign-ref 'long $fd-a 16)
                 (foreign-ref 'long $fd-a 24)
                 (foreign-ref 'long $fd-a 32)
                 (foreign-ref 'unsigned-long $fd-a 0)
                 (foreign-ref 'unsigned-long $fd-a 8)
                 (foreign-ref 'unsigned-long $fd-a 16)
                 (foreign-ref 'unsigned-long $fd-a 24)
                 (foreign-ref 'unsigned-long $fd-a 32)))
         `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
           #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
       (equal?
         (begin
           (foreign-set! 'unsigned-long $fd-a 0 #x-8000000000000000)
           (foreign-set! 'unsigned-long $fd-a 8 0)
           (foreign-set! 'unsigned-long $fd-a 16 #x7fffffffffffffff)
           (foreign-set! 'unsigned-long $fd-a 24 #x8000000000000000)
           (foreign-set! 'unsigned-long $fd-a 32 #xffffffffffffffff)
           (list (foreign-ref 'long $fd-a 0)
                 (foreign-ref 'long $fd-a 8)
                 (foreign-ref 'long $fd-a 16)
                 (foreign-ref 'long $fd-a 24)
                 (foreign-ref 'long $fd-a 32)
                 (foreign-ref 'unsigned-long $fd-a 0)
                 (foreign-ref 'unsigned-long $fd-a 8)
                 (foreign-ref 'unsigned-long $fd-a 16)
                 (foreign-ref 'unsigned-long $fd-a 24)
                 (foreign-ref 'unsigned-long $fd-a 32)))
         `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
           #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
       (equal?
         (begin
           (foreign-set! 'long $fd-a 16 #xabcd1234ffee8765)
           (list (foreign-ref 'long $fd-a 16)
                 (foreign-ref 'unsigned-long $fd-a 16)
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
         `(,(- #xabcd1234ffee8765 #x10000000000000000)
           #xabcd1234ffee8765
           ,(- #xabcd1234ffee8765 #x10000000000000000)
           #xabcd1234ffee8765))
       (equal?
         (begin
           (foreign-set! 'unsigned-long $fd-a 16 #x-765321ab4c8e9de1)
           (list (foreign-ref 'long $fd-a 16)
                 (foreign-ref 'unsigned-long $fd-a 16)
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
         `(#x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000)
           #x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000))))]
    [else (error 'foreign-data-mat "unexpected $fd-long-max ~s" $fd-long-max)])
 ; long-long/unsigned-long-long
  (error? ; invalid value for type
    (foreign-set! 'long-long $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'long-long $fd-a 0 (- $fd-long-long-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'long-long $fd-a 0 (+ $fd-long-long-max 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-long-long $fd-a 0 17.0))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-long-long $fd-a 0 (- $fd-long-long-min 1)))
  (error? ; invalid value for type
    (foreign-set! 'unsigned-long-long $fd-a 0 (+ $fd-long-long-max 1)))
  (case $fd-long-long-max
    [(#xffffffffffffffff)
     (and
       (equal?
         (begin
           (foreign-set! 'long-long $fd-a 0 #x-8000000000000000)
           (foreign-set! 'long-long $fd-a 8 0)
           (foreign-set! 'long-long $fd-a 16 #x7fffffffffffffff)
           (foreign-set! 'long-long $fd-a 24 #x8000000000000000)
           (foreign-set! 'long-long $fd-a 32 #xffffffffffffffff)
           (list (foreign-ref 'long-long $fd-a 0)
                 (foreign-ref 'long-long $fd-a 8)
                 (foreign-ref 'long-long $fd-a 16)
                 (foreign-ref 'long-long $fd-a 24)
                 (foreign-ref 'long-long $fd-a 32)
                 (foreign-ref 'unsigned-long-long $fd-a 0)
                 (foreign-ref 'unsigned-long-long $fd-a 8)
                 (foreign-ref 'unsigned-long-long $fd-a 16)
                 (foreign-ref 'unsigned-long-long $fd-a 24)
                 (foreign-ref 'unsigned-long-long $fd-a 32)))
         `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
           #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
       (equal?
         (begin
           (foreign-set! 'unsigned-long-long $fd-a 0 #x-8000000000000000)
           (foreign-set! 'unsigned-long-long $fd-a 8 0)
           (foreign-set! 'unsigned-long-long $fd-a 16 #x7fffffffffffffff)
           (foreign-set! 'unsigned-long-long $fd-a 24 #x8000000000000000)
           (foreign-set! 'unsigned-long-long $fd-a 32 #xffffffffffffffff)
           (list (foreign-ref 'long-long $fd-a 0)
                 (foreign-ref 'long-long $fd-a 8)
                 (foreign-ref 'long-long $fd-a 16)
                 (foreign-ref 'long-long $fd-a 24)
                 (foreign-ref 'long-long $fd-a 32)
                 (foreign-ref 'unsigned-long-long $fd-a 0)
                 (foreign-ref 'unsigned-long-long $fd-a 8)
                 (foreign-ref 'unsigned-long-long $fd-a 16)
                 (foreign-ref 'unsigned-long-long $fd-a 24)
                 (foreign-ref 'unsigned-long-long $fd-a 32)))
         `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
           #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
       (equal?
         (begin
           (foreign-set! 'long-long $fd-a 16 #xabcd1234ffee8765)
           (list (foreign-ref 'long-long $fd-a 16)
                 (foreign-ref 'unsigned-long-long $fd-a 16)
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
         `(,(- #xabcd1234ffee8765 #x10000000000000000)
           #xabcd1234ffee8765
           ,(- #xabcd1234ffee8765 #x10000000000000000)
           #xabcd1234ffee8765))
       (equal?
         (begin
           (foreign-set! 'unsigned-long-long $fd-a 16 #x-765321ab4c8e9de1)
           (list (foreign-ref 'long-long $fd-a 16)
                 (foreign-ref 'unsigned-long-long $fd-a 16)
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
                 (if (eq? (native-endianness) 'little)
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 16)
                       (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                     (logor
                       (foreign-ref 'unsigned-32 $fd-a 20)
                       (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
         `(#x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000)
           #x-765321ab4c8e9de1
           ,(+ #x-765321ab4c8e9de1 #x10000000000000000))))]
    [else (error 'foreign-data-mat "unexpected $fd-long-long-max ~s" $fd-long-long-max)])
 ; char
  (error? ; invalid value for type
    (foreign-set! 'char $fd-a 0 17))
  (error? ; invalid value for type
    (foreign-set! 'char $fd-a 0 (integer->char (+ $fd-char-max 1))))
  (case $fd-char-max
    [(#xff)
     (and
       (equal?
         (begin
           (foreign-set! 'char $fd-a 2 #\xed)
           (list (foreign-ref 'char $fd-a 2)
                 (foreign-ref 'unsigned-8 $fd-a 2)))
         `(#\xed #xed))
       (equal?
         (begin
           (foreign-set! 'char $fd-a 3 (integer->char 0))
           (list (foreign-ref 'char $fd-a 3)
                 (foreign-ref 'unsigned-8 $fd-a 3)))
         `(#\nul 0))
       (equal?
         (begin
           (foreign-set! 'char $fd-a 3 (integer->char $fd-char-max))
           (list (foreign-ref 'char $fd-a 3)
                 (foreign-ref 'unsigned-8 $fd-a 3)))
         `(,(integer->char $fd-char-max) ,$fd-char-max)))]
    [else (error 'foreign-data-mat "unexpected $fd-char-max ~s" $fd-char-max)])
 ; wchar
  (error? ; invalid value for type
    (foreign-set! 'wchar $fd-a 0 17))
  (or (= $fd-wchar-max #x10ffff)
      (guard (c [#t])
        (foreign-set! 'wchar $fd-a 0 (integer->char (+ $fd-wchar-max 1)))
        #f))
  (case $fd-wchar-max
    [(#xffff)
     (and
       (equal?
         (begin
           (foreign-set! 'wchar $fd-a 2 #\xedac)
           (list (foreign-ref 'wchar $fd-a 2)
                 (foreign-ref 'unsigned-16 $fd-a 2)))
         `(#\xedac #xedac))
       (equal?
         (begin
           (foreign-set! 'wchar $fd-a 2 (integer->char 0))
           (list (foreign-ref 'wchar $fd-a 2)
                 (foreign-ref 'unsigned-16 $fd-a 2)))
         `(#\nul 0))
       (equal?
         (begin
           (foreign-set! 'wchar $fd-a 2 (integer->char $fd-wchar-max))
           (list (foreign-ref 'wchar $fd-a 2)
                 (foreign-ref 'unsigned-16 $fd-a 2)))
         `(,(integer->char $fd-wchar-max) ,$fd-wchar-max)))]
    [(#x10ffff)
     (and
       (equal?
         (begin
           (foreign-set! 'wchar $fd-a 4 #\x10edac)
           (list (foreign-ref 'wchar $fd-a 4)
                 (foreign-ref 'unsigned-32 $fd-a 4)))
         `(#\x10edac #x10edac))
       (equal?
         (begin
           (foreign-set! 'wchar $fd-a 4 (integer->char 0))
           (list (foreign-ref 'wchar $fd-a 4)
                 (foreign-ref 'unsigned-32 $fd-a 4)))
         `(#\nul 0))
       (equal?
         (begin
           (foreign-set! 'wchar $fd-a 4 (integer->char $fd-wchar-max))
           (list (foreign-ref 'wchar $fd-a 4)
                 (foreign-ref 'unsigned-32 $fd-a 4)))
         `(,(integer->char $fd-wchar-max) ,$fd-wchar-max)))]
    [else (error 'foreign-data-mat "unexpected $fd-wchar-max ~s" $fd-wchar-max)])
 ; boolean
  (equal?
    (begin
      (foreign-set! 'boolean $fd-a 0 #t)
      (foreign-set! 'boolean $fd-a 8 #f)
      (foreign-set! 'boolean $fd-a 16 0)
      (foreign-set! 'int $fd-a 24 64)
      (list
        (foreign-ref 'boolean $fd-a 0)
        (foreign-ref 'boolean $fd-a 8)
        (foreign-ref 'boolean $fd-a 16)
        (foreign-ref 'boolean $fd-a 24)
        (foreign-ref 'int $fd-a 0)
        (foreign-ref 'int $fd-a 8)
        (foreign-ref 'int $fd-a 16)
        (foreign-ref 'int $fd-a 24)))
    '(#t #f #t #t 1 0 1 64))
 ; fixnum
  (error? ; invalid value for type
    (foreign-set! 'fixnum $fd-a 0 2/3))
  (error? ; invalid value for type
    (foreign-set! 'fixnum $fd-a 0 (+ (greatest-fixnum) 1)))
  (error? ; invalid value for type
    (foreign-set! 'fixnum $fd-a 0 (- (least-fixnum) 1)))
  (equal?
    (begin
      (foreign-set! 'fixnum $fd-a 0 (greatest-fixnum))
      (foreign-set! 'fixnum $fd-a 8 (least-fixnum))
      (foreign-set! 'fixnum $fd-a 16 0)
      (foreign-set! 'fixnum $fd-a 24 (quotient (greatest-fixnum) 2))
      (list
        (foreign-ref 'fixnum $fd-a 0)
        (foreign-ref 'fixnum $fd-a 8)
        (foreign-ref 'fixnum $fd-a 16)
        (foreign-ref 'fixnum $fd-a 24)))
    `(,(greatest-fixnum) ,(least-fixnum) 0 ,(quotient (greatest-fixnum) 2)))
 ; float / single-float
  (error? ; invalid value for type
    (foreign-set! 'float $fd-a 0 17))
  (error? ; invalid value for type
    (foreign-set! 'single-float $fd-a 0 17))
  (equal?
    (begin
      (foreign-set! 'float $fd-a 12 7.5)
      (list (foreign-ref 'float $fd-a 12)
            (foreign-ref 'single-float $fd-a 12)))
    '(7.5 7.5))
  (equal?
    (begin
      (foreign-set! 'single-float $fd-a 12 7.5)
      (list (foreign-ref 'float $fd-a 12)
            (foreign-ref 'single-float $fd-a 12)))
    '(7.5 7.5))
 ; double / double-float
  (error? ; invalid value for type
    (foreign-set! 'double $fd-a 0 17))
  (error? ; invalid value for type
    (foreign-set! 'double-float $fd-a 0 17))
  (equal?
    (begin
      (foreign-set! 'double $fd-a 8 -5.4)
      (list (foreign-ref 'double $fd-a 8)
            (foreign-ref 'double-float $fd-a 8)))
    '(-5.4 -5.4))
  (equal?
    (begin
      (foreign-set! 'double-float $fd-a 8 -5.4)
      (list (foreign-ref 'double $fd-a 8)
            (foreign-ref 'double-float $fd-a 8)))
    '(-5.4 -5.4))
 ; spot check unaligned ref/set
  (or (not $fd-unaligned-integers)
      (equal?
        (begin
          (foreign-set! 'unsigned-32 $fd-a 13 #x-765321ab)
          (list (foreign-ref 'integer-32 $fd-a 13)
                (foreign-ref 'unsigned-32 $fd-a 13)))
        `(#x-765321ab ,(+ #x-765321ab #x100000000))))
  (or (not $fd-unaligned-integers)
      (equal?
        (begin
          (foreign-set! 'integer-64 $fd-a 17 #xabcd1234ffee8765)
          (list (foreign-ref 'integer-64 $fd-a 17)
                (foreign-ref 'unsigned-64 $fd-a 17)
                (if (eq? (native-endianness) 'little)
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 17)
                      (ash (foreign-ref 'integer-32 $fd-a 21) 32))
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 21)
                      (ash (foreign-ref 'integer-32 $fd-a 17) 32)))
                (if (eq? (native-endianness) 'little)
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 17)
                      (ash (foreign-ref 'unsigned-32 $fd-a 21) 32))
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 21)
                      (ash (foreign-ref 'unsigned-32 $fd-a 17) 32)))))
        `(,(- #xabcd1234ffee8765 #x10000000000000000)
          #xabcd1234ffee8765
          ,(- #xabcd1234ffee8765 #x10000000000000000)
          #xabcd1234ffee8765)))
  (or (not $fd-unaligned-integers)
      (case $fd-short-max
        [(#xffff)
         (and
           (equal?
             (begin
               (foreign-set! 'short $fd-a 3 #xabcd)
               (list (foreign-ref 'short $fd-a 3)
                     (foreign-ref 'unsigned-short $fd-a 3)
                     (foreign-ref 'integer-16 $fd-a 3)
                     (foreign-ref 'unsigned-16 $fd-a 3)))
             `(,(- #xabcd #x10000) #xabcd ,(- #xabcd #x10000) #xabcd))
           (equal?
             (begin
               (foreign-set! 'unsigned-short $fd-a 3 -5321)
               (list (foreign-ref 'short $fd-a 3)
                     (foreign-ref 'unsigned-short $fd-a 3)
                     (foreign-ref 'integer-16 $fd-a 3)
                     (foreign-ref 'unsigned-16 $fd-a 3)))
             `(-5321 ,(+ -5321 #x10000) -5321 ,(+ -5321 #x10000))))]
        [else (error 'foreign-data-mat "unexpected $fd-short-max ~s" $fd-short-max)]))
  (or (not $fd-unaligned-floats)
      (equal?
        (begin
          (foreign-set! 'float $fd-a 6 7.5)
          (list (foreign-ref 'float $fd-a 6)
                (foreign-ref 'single-float $fd-a 6)))
        '(7.5 7.5)))
  (or (not $fd-unaligned-floats)
      (equal?
        (begin
          (foreign-set! 'double-float $fd-a 5 -5.4)
          (list (foreign-ref 'double $fd-a 5)
                (foreign-ref 'double-float $fd-a 5)))
        '(-5.4 -5.4)))
 ; $object-ref
  (equal?
    (begin
      (foreign-set! 'integer-8 $fd-a 3 255)
      (list (#%$object-ref 'integer-8 $raw-fd-a 3)
            (#%$object-ref 'unsigned-8 $raw-fd-a 3)))
    '(-1 255))
  (equal?
    (begin
      (foreign-set! 'unsigned-8 $fd-a 5 -5)
      (list (#%$object-ref 'integer-8 $raw-fd-a 5)
            (#%$object-ref 'unsigned-8 $raw-fd-a 5)))
    '(-5 251))
  (equal?
    (begin
      (foreign-set! 'integer-8 $fd-a 0 #x-80)
      (foreign-set! 'integer-8 $fd-a 1 0)
      (foreign-set! 'integer-8 $fd-a 2 #x7f)
      (foreign-set! 'integer-8 $fd-a 3 #x80)
      (foreign-set! 'integer-8 $fd-a 4 #xff)
      (list (#%$object-ref 'integer-8 $raw-fd-a 0)
            (#%$object-ref 'integer-8 $raw-fd-a 1)
            (#%$object-ref 'integer-8 $raw-fd-a 2)
            (#%$object-ref 'integer-8 $raw-fd-a 3)
            (#%$object-ref 'integer-8 $raw-fd-a 4)
            (#%$object-ref 'unsigned-8 $raw-fd-a 0)
            (#%$object-ref 'unsigned-8 $raw-fd-a 1)
            (#%$object-ref 'unsigned-8 $raw-fd-a 2)
            (#%$object-ref 'unsigned-8 $raw-fd-a 3)
            (#%$object-ref 'unsigned-8 $raw-fd-a 4)))
    `(#x-80 0 #x7f #x-80 -1
      #x80 0 #x7f #x80 #xff))
  (equal?
    (begin
      (foreign-set! 'unsigned-8 $fd-a 0 #x-80)
      (foreign-set! 'unsigned-8 $fd-a 1 0)
      (foreign-set! 'unsigned-8 $fd-a 2 #x7f)
      (foreign-set! 'unsigned-8 $fd-a 3 #x80)
      (foreign-set! 'unsigned-8 $fd-a 4 #xff)
      (list (#%$object-ref 'integer-8 $raw-fd-a 0)
            (#%$object-ref 'integer-8 $raw-fd-a 1)
            (#%$object-ref 'integer-8 $raw-fd-a 2)
            (#%$object-ref 'integer-8 $raw-fd-a 3)
            (#%$object-ref 'integer-8 $raw-fd-a 4)
            (#%$object-ref 'unsigned-8 $raw-fd-a 0)
            (#%$object-ref 'unsigned-8 $raw-fd-a 1)
            (#%$object-ref 'unsigned-8 $raw-fd-a 2)
            (#%$object-ref 'unsigned-8 $raw-fd-a 3)
            (#%$object-ref 'unsigned-8 $raw-fd-a 4)))
    `(#x-80 0 #x7f #x-80 -1
      #x80 0 #x7f #x80 #xff))
 ; integer-16/unsigned-16
  (equal?
    (begin
      (foreign-set! 'integer-16 $fd-a 2 #xabcd)
      (list (#%$object-ref 'integer-16 $raw-fd-a 2)
            (#%$object-ref 'unsigned-16 $raw-fd-a 2)))
    `(,(- #xabcd #x10000) #xabcd))
  (equal?
    (begin
      (foreign-set! 'unsigned-16 $fd-a 2 -5321)
      (list (#%$object-ref 'integer-16 $raw-fd-a 2)
            (#%$object-ref 'unsigned-16 $raw-fd-a 2)))
    `(-5321 ,(+ -5321 #x10000)))
  (equal?
    (begin
      (foreign-set! 'integer-16 $fd-a 0 #x-8000)
      (foreign-set! 'integer-16 $fd-a 2 0)
      (foreign-set! 'integer-16 $fd-a 4 #x7fff)
      (foreign-set! 'integer-16 $fd-a 6 #x8000)
      (foreign-set! 'integer-16 $fd-a 8 #xffff)
      (list (#%$object-ref 'integer-16 $raw-fd-a 0)
            (#%$object-ref 'integer-16 $raw-fd-a 2)
            (#%$object-ref 'integer-16 $raw-fd-a 4)
            (#%$object-ref 'integer-16 $raw-fd-a 6)
            (#%$object-ref 'integer-16 $raw-fd-a 8)
            (#%$object-ref 'unsigned-16 $raw-fd-a 0)
            (#%$object-ref 'unsigned-16 $raw-fd-a 2)
            (#%$object-ref 'unsigned-16 $raw-fd-a 4)
            (#%$object-ref 'unsigned-16 $raw-fd-a 6)
            (#%$object-ref 'unsigned-16 $raw-fd-a 8)))
    `(#x-8000 0 #x7fff #x-8000 -1
      #x8000 0 #x7fff #x8000 #xffff))
  (equal?
    (begin
      (foreign-set! 'unsigned-16 $fd-a 0 #x-8000)
      (foreign-set! 'unsigned-16 $fd-a 2 0)
      (foreign-set! 'unsigned-16 $fd-a 4 #x7fff)
      (foreign-set! 'unsigned-16 $fd-a 6 #x8000)
      (foreign-set! 'unsigned-16 $fd-a 8 #xffff)
      (list (#%$object-ref 'integer-16 $raw-fd-a 0)
            (#%$object-ref 'integer-16 $raw-fd-a 2)
            (#%$object-ref 'integer-16 $raw-fd-a 4)
            (#%$object-ref 'integer-16 $raw-fd-a 6)
            (#%$object-ref 'integer-16 $raw-fd-a 8)
            (#%$object-ref 'unsigned-16 $raw-fd-a 0)
            (#%$object-ref 'unsigned-16 $raw-fd-a 2)
            (#%$object-ref 'unsigned-16 $raw-fd-a 4)
            (#%$object-ref 'unsigned-16 $raw-fd-a 6)
            (#%$object-ref 'unsigned-16 $raw-fd-a 8)))
    `(#x-8000 0 #x7fff #x-8000 -1
      #x8000 0 #x7fff #x8000 #xffff))
 ; integer-32/unsigned-32
  (equal?
    (begin
      (foreign-set! 'integer-32 $fd-a 0 #x-80000000)
      (foreign-set! 'integer-32 $fd-a 4 0)
      (foreign-set! 'integer-32 $fd-a 8 #x7fffffff)
      (foreign-set! 'integer-32 $fd-a 12 #x80000000)
      (foreign-set! 'integer-32 $fd-a 16 #xffffffff)
      (list (#%$object-ref 'integer-32 $raw-fd-a 0)
            (#%$object-ref 'integer-32 $raw-fd-a 4)
            (#%$object-ref 'integer-32 $raw-fd-a 8)
            (#%$object-ref 'integer-32 $raw-fd-a 12)
            (#%$object-ref 'integer-32 $raw-fd-a 16)
            (#%$object-ref 'unsigned-32 $raw-fd-a 0)
            (#%$object-ref 'unsigned-32 $raw-fd-a 4)
            (#%$object-ref 'unsigned-32 $raw-fd-a 8)
            (#%$object-ref 'unsigned-32 $raw-fd-a 12)
            (#%$object-ref 'unsigned-32 $raw-fd-a 16)))
    `(#x-80000000 0 #x7fffffff #x-80000000 -1
      #x80000000 0 #x7fffffff #x80000000 #xffffffff))
  (equal?
    (begin
      (foreign-set! 'unsigned-32 $fd-a 0 #x-80000000)
      (foreign-set! 'unsigned-32 $fd-a 4 0)
      (foreign-set! 'unsigned-32 $fd-a 8 #x7fffffff)
      (foreign-set! 'unsigned-32 $fd-a 12 #x80000000)
      (foreign-set! 'unsigned-32 $fd-a 16 #xffffffff)
      (list (#%$object-ref 'integer-32 $raw-fd-a 0)
            (#%$object-ref 'integer-32 $raw-fd-a 4)
            (#%$object-ref 'integer-32 $raw-fd-a 8)
            (#%$object-ref 'integer-32 $raw-fd-a 12)
            (#%$object-ref 'integer-32 $raw-fd-a 16)
            (#%$object-ref 'unsigned-32 $raw-fd-a 0)
            (#%$object-ref 'unsigned-32 $raw-fd-a 4)
            (#%$object-ref 'unsigned-32 $raw-fd-a 8)
            (#%$object-ref 'unsigned-32 $raw-fd-a 12)
            (#%$object-ref 'unsigned-32 $raw-fd-a 16)))
    `(#x-80000000 0 #x7fffffff #x-80000000 -1
      #x80000000 0 #x7fffffff #x80000000 #xffffffff))
  (equal?
    (begin
      (foreign-set! 'integer-32 $fd-a 12 #xabcd1234)
      (list (#%$object-ref 'integer-32 $raw-fd-a 12)
            (#%$object-ref 'unsigned-32 $raw-fd-a 12)))
    `(,(- #xabcd1234 #x100000000) #xabcd1234))
  (equal?
    (begin
      (foreign-set! 'unsigned-32 $fd-a 12 #x-765321ab)
      (list (#%$object-ref 'integer-32 $raw-fd-a 12)
            (#%$object-ref 'unsigned-32 $raw-fd-a 12)))
    `(#x-765321ab ,(+ #x-765321ab #x100000000)))
 ; integer-64/unsigned-64
  (equal?
    (begin
      (foreign-set! 'integer-64 $fd-a 16 #xabcd1234ffee8765)
      (list (#%$object-ref 'integer-64 $raw-fd-a 16)
            (#%$object-ref 'unsigned-64 $raw-fd-a 16)
            (if (eq? (native-endianness) 'little)
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 16)
                  (ash (#%$object-ref 'integer-32 $raw-fd-a 20) 32))
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 20)
                  (ash (#%$object-ref 'integer-32 $raw-fd-a 16) 32)))
            (if (eq? (native-endianness) 'little)
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 16)
                  (ash (#%$object-ref 'unsigned-32 $raw-fd-a 20) 32))
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 20)
                  (ash (#%$object-ref 'unsigned-32 $raw-fd-a 16) 32)))))
    `(,(- #xabcd1234ffee8765 #x10000000000000000)
      #xabcd1234ffee8765
      ,(- #xabcd1234ffee8765 #x10000000000000000)
      #xabcd1234ffee8765))
  (equal?
    (begin
      (foreign-set! 'unsigned-64 $fd-a 16 #x-765321ab4c8e9de1)
      (list (#%$object-ref 'integer-64 $raw-fd-a 16)
            (#%$object-ref 'unsigned-64 $raw-fd-a 16)
            (if (eq? (native-endianness) 'little)
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 16)
                  (ash (#%$object-ref 'integer-32 $raw-fd-a 20) 32))
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 20)
                  (ash (#%$object-ref 'integer-32 $raw-fd-a 16) 32)))
            (if (eq? (native-endianness) 'little)
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 16)
                  (ash (#%$object-ref 'unsigned-32 $raw-fd-a 20) 32))
                (logor
                  (#%$object-ref 'unsigned-32 $raw-fd-a 20)
                  (ash (#%$object-ref 'unsigned-32 $raw-fd-a 16) 32)))))
    `(#x-765321ab4c8e9de1
      ,(+ #x-765321ab4c8e9de1 #x10000000000000000)
      #x-765321ab4c8e9de1
      ,(+ #x-765321ab4c8e9de1 #x10000000000000000)))
  (equal?
    (begin
      (foreign-set! 'integer-64 $fd-a 0 #x-8000000000000000)
      (foreign-set! 'integer-64 $fd-a 8 0)
      (foreign-set! 'integer-64 $fd-a 16 #x7fffffffffffffff)
      (foreign-set! 'integer-64 $fd-a 24 #x8000000000000000)
      (foreign-set! 'integer-64 $fd-a 32 #xffffffffffffffff)
      (list (#%$object-ref 'integer-64 $raw-fd-a 0)
            (#%$object-ref 'integer-64 $raw-fd-a 8)
            (#%$object-ref 'integer-64 $raw-fd-a 16)
            (#%$object-ref 'integer-64 $raw-fd-a 24)
            (#%$object-ref 'integer-64 $raw-fd-a 32)
            (#%$object-ref 'unsigned-64 $raw-fd-a 0)
            (#%$object-ref 'unsigned-64 $raw-fd-a 8)
            (#%$object-ref 'unsigned-64 $raw-fd-a 16)
            (#%$object-ref 'unsigned-64 $raw-fd-a 24)
            (#%$object-ref 'unsigned-64 $raw-fd-a 32)))
    `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
      #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
  (equal?
    (begin
      (foreign-set! 'unsigned-64 $fd-a 0 #x-8000000000000000)
      (foreign-set! 'unsigned-64 $fd-a 8 0)
      (foreign-set! 'unsigned-64 $fd-a 16 #x7fffffffffffffff)
      (foreign-set! 'unsigned-64 $fd-a 24 #x8000000000000000)
      (foreign-set! 'unsigned-64 $fd-a 32 #xffffffffffffffff)
      (list (#%$object-ref 'integer-64 $raw-fd-a 0)
            (#%$object-ref 'integer-64 $raw-fd-a 8)
            (#%$object-ref 'integer-64 $raw-fd-a 16)
            (#%$object-ref 'integer-64 $raw-fd-a 24)
            (#%$object-ref 'integer-64 $raw-fd-a 32)
            (#%$object-ref 'unsigned-64 $raw-fd-a 0)
            (#%$object-ref 'unsigned-64 $raw-fd-a 8)
            (#%$object-ref 'unsigned-64 $raw-fd-a 16)
            (#%$object-ref 'unsigned-64 $raw-fd-a 24)
            (#%$object-ref 'unsigned-64 $raw-fd-a 32)))
    `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
      #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
 ; fixnum
  (equal?
    (begin
      (foreign-set! 'fixnum $fd-a 0 (greatest-fixnum))
      (foreign-set! 'fixnum $fd-a 8 (least-fixnum))
      (foreign-set! 'fixnum $fd-a 16 0)
      (foreign-set! 'fixnum $fd-a 24 (quotient (greatest-fixnum) 2))
      (list
        (#%$object-ref 'fixnum $raw-fd-a 0)
        (#%$object-ref 'fixnum $raw-fd-a 8)
        (#%$object-ref 'fixnum $raw-fd-a 16)
        (#%$object-ref 'fixnum $raw-fd-a 24)))
    `(,(greatest-fixnum) ,(least-fixnum) 0 ,(quotient (greatest-fixnum) 2)))
 ; single-float
  (equal?
    (begin
      (foreign-set! 'single-float $fd-a 12 7.5)
      (#%$object-ref 'single-float $raw-fd-a 12))
    7.5)
 ; double-float
  (equal?
    (begin
      (foreign-set! 'double-float $fd-a 8 -5.4)
      (#%$object-ref 'double-float $raw-fd-a 8))
    -5.4)
 ; spot check unaligned ref/set
  (or (not $fd-unaligned-integers)
      (equal?
        (begin
          (foreign-set! 'unsigned-32 $fd-a 13 #x-765321ab)
          (list (#%$object-ref 'integer-32 $raw-fd-a 13)
                (#%$object-ref 'unsigned-32 $raw-fd-a 13)))
        `(#x-765321ab ,(+ #x-765321ab #x100000000))))
  (or (not $fd-unaligned-integers)
      (equal?
        (begin
          (foreign-set! 'integer-64 $fd-a 17 #xabcd1234ffee8765)
          (list (#%$object-ref 'integer-64 $raw-fd-a 17)
                (#%$object-ref 'unsigned-64 $raw-fd-a 17)
                (if (eq? (native-endianness) 'little)
                    (logor
                      (#%$object-ref 'unsigned-32 $raw-fd-a 17)
                      (ash (#%$object-ref 'integer-32 $raw-fd-a 21) 32))
                    (logor
                      (#%$object-ref 'unsigned-32 $raw-fd-a 21)
                      (ash (#%$object-ref 'integer-32 $raw-fd-a 17) 32)))
                (if (eq? (native-endianness) 'little)
                    (logor
                      (#%$object-ref 'unsigned-32 $raw-fd-a 17)
                      (ash (#%$object-ref 'unsigned-32 $raw-fd-a 21) 32))
                    (logor
                      (#%$object-ref 'unsigned-32 $raw-fd-a 21)
                      (ash (#%$object-ref 'unsigned-32 $raw-fd-a 17) 32)))))
        `(,(- #xabcd1234ffee8765 #x10000000000000000)
          #xabcd1234ffee8765
          ,(- #xabcd1234ffee8765 #x10000000000000000)
          #xabcd1234ffee8765)))
  (or (not $fd-unaligned-integers)
      (case $fd-short-max
        [(#xffff)
         (and
           (equal?
             (begin
               (foreign-set! 'short $fd-a 3 #xabcd)
               (list (#%$object-ref 'short $raw-fd-a 3)
                     (#%$object-ref 'unsigned-short $raw-fd-a 3)
                     (#%$object-ref 'integer-16 $raw-fd-a 3)
                     (#%$object-ref 'unsigned-16 $raw-fd-a 3)))
             `(,(- #xabcd #x10000) #xabcd ,(- #xabcd #x10000) #xabcd))
           (equal?
             (begin
               (foreign-set! 'unsigned-short $fd-a 3 -5321)
               (list (#%$object-ref 'short $raw-fd-a 3)
                     (#%$object-ref 'unsigned-short $raw-fd-a 3)
                     (#%$object-ref 'integer-16 $raw-fd-a 3)
                     (#%$object-ref 'unsigned-16 $raw-fd-a 3)))
             `(-5321 ,(+ -5321 #x10000) -5321 ,(+ -5321 #x10000))))]
        [else (error 'foreign-data-mat "unexpected $fd-short-max ~s" $fd-short-max)]))
  (or (not $fd-unaligned-floats)
      (equal?
        (begin
          (foreign-set! 'single-float $fd-a 6 7.5)
          (#%$object-ref 'single-float $raw-fd-a 6))
        7.5))
  (or (not $fd-unaligned-floats)
      (equal?
        (begin
          (foreign-set! 'double-float $fd-a 5 -5.4)
          (#%$object-ref 'double-float $raw-fd-a 5))
        -5.4))

 ; $object-set!
  (equal?
    (begin
      (#%$object-set! 'integer-8 $raw-fd-a 3 255)
      (list (foreign-ref 'integer-8 $fd-a 3)
            (foreign-ref 'unsigned-8 $fd-a 3)))
    '(-1 255))
  (equal?
    (begin
      (#%$object-set! 'unsigned-8 $raw-fd-a 5 -5)
      (list (foreign-ref 'integer-8 $fd-a 5)
            (foreign-ref 'unsigned-8 $fd-a 5)))
    '(-5 251))
  (equal?
    (begin
      (#%$object-set! 'integer-8 $raw-fd-a 0 #x-80)
      (#%$object-set! 'integer-8 $raw-fd-a 1 0)
      (#%$object-set! 'integer-8 $raw-fd-a 2 #x7f)
      (#%$object-set! 'integer-8 $raw-fd-a 3 #x80)
      (#%$object-set! 'integer-8 $raw-fd-a 4 #xff)
      (list (foreign-ref 'integer-8 $fd-a 0)
            (foreign-ref 'integer-8 $fd-a 1)
            (foreign-ref 'integer-8 $fd-a 2)
            (foreign-ref 'integer-8 $fd-a 3)
            (foreign-ref 'integer-8 $fd-a 4)
            (foreign-ref 'unsigned-8 $fd-a 0)
            (foreign-ref 'unsigned-8 $fd-a 1)
            (foreign-ref 'unsigned-8 $fd-a 2)
            (foreign-ref 'unsigned-8 $fd-a 3)
            (foreign-ref 'unsigned-8 $fd-a 4)))
    `(#x-80 0 #x7f #x-80 -1
      #x80 0 #x7f #x80 #xff))
  (equal?
    (begin
      (#%$object-set! 'unsigned-8 $raw-fd-a 0 #x-80)
      (#%$object-set! 'unsigned-8 $raw-fd-a 1 0)
      (#%$object-set! 'unsigned-8 $raw-fd-a 2 #x7f)
      (#%$object-set! 'unsigned-8 $raw-fd-a 3 #x80)
      (#%$object-set! 'unsigned-8 $raw-fd-a 4 #xff)
      (list (foreign-ref 'integer-8 $fd-a 0)
            (foreign-ref 'integer-8 $fd-a 1)
            (foreign-ref 'integer-8 $fd-a 2)
            (foreign-ref 'integer-8 $fd-a 3)
            (foreign-ref 'integer-8 $fd-a 4)
            (foreign-ref 'unsigned-8 $fd-a 0)
            (foreign-ref 'unsigned-8 $fd-a 1)
            (foreign-ref 'unsigned-8 $fd-a 2)
            (foreign-ref 'unsigned-8 $fd-a 3)
            (foreign-ref 'unsigned-8 $fd-a 4)))
    `(#x-80 0 #x7f #x-80 -1
      #x80 0 #x7f #x80 #xff))
 ; integer-16/unsigned-16
  (equal?
    (begin
      (#%$object-set! 'integer-16 $raw-fd-a 2 #xabcd)
      (list (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 2)))
    `(,(- #xabcd #x10000) #xabcd))
  (equal?
    (begin
      (#%$object-set! 'unsigned-16 $raw-fd-a 2 -5321)
      (list (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 2)))
    `(-5321 ,(+ -5321 #x10000)))
  (equal?
    (begin
      (#%$object-set! 'integer-16 $raw-fd-a 0 #x-8000)
      (#%$object-set! 'integer-16 $raw-fd-a 2 0)
      (#%$object-set! 'integer-16 $raw-fd-a 4 #x7fff)
      (#%$object-set! 'integer-16 $raw-fd-a 6 #x8000)
      (#%$object-set! 'integer-16 $raw-fd-a 8 #xffff)
      (list (foreign-ref 'integer-16 $fd-a 0)
            (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'integer-16 $fd-a 4)
            (foreign-ref 'integer-16 $fd-a 6)
            (foreign-ref 'integer-16 $fd-a 8)
            (foreign-ref 'unsigned-16 $fd-a 0)
            (foreign-ref 'unsigned-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 4)
            (foreign-ref 'unsigned-16 $fd-a 6)
            (foreign-ref 'unsigned-16 $fd-a 8)))
    `(#x-8000 0 #x7fff #x-8000 -1
      #x8000 0 #x7fff #x8000 #xffff))
  (equal?
    (begin
      (#%$object-set! 'unsigned-16 $raw-fd-a 0 #x-8000)
      (#%$object-set! 'unsigned-16 $raw-fd-a 2 0)
      (#%$object-set! 'unsigned-16 $raw-fd-a 4 #x7fff)
      (#%$object-set! 'unsigned-16 $raw-fd-a 6 #x8000)
      (#%$object-set! 'unsigned-16 $raw-fd-a 8 #xffff)
      (list (foreign-ref 'integer-16 $fd-a 0)
            (foreign-ref 'integer-16 $fd-a 2)
            (foreign-ref 'integer-16 $fd-a 4)
            (foreign-ref 'integer-16 $fd-a 6)
            (foreign-ref 'integer-16 $fd-a 8)
            (foreign-ref 'unsigned-16 $fd-a 0)
            (foreign-ref 'unsigned-16 $fd-a 2)
            (foreign-ref 'unsigned-16 $fd-a 4)
            (foreign-ref 'unsigned-16 $fd-a 6)
            (foreign-ref 'unsigned-16 $fd-a 8)))
    `(#x-8000 0 #x7fff #x-8000 -1
      #x8000 0 #x7fff #x8000 #xffff))
 ; integer-32/unsigned-32
  (equal?
    (begin
      (#%$object-set! 'integer-32 $raw-fd-a 0 #x-80000000)
      (#%$object-set! 'integer-32 $raw-fd-a 4 0)
      (#%$object-set! 'integer-32 $raw-fd-a 8 #x7fffffff)
      (#%$object-set! 'integer-32 $raw-fd-a 12 #x80000000)
      (#%$object-set! 'integer-32 $raw-fd-a 16 #xffffffff)
      (list (foreign-ref 'integer-32 $fd-a 0)
            (foreign-ref 'integer-32 $fd-a 4)
            (foreign-ref 'integer-32 $fd-a 8)
            (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'integer-32 $fd-a 16)
            (foreign-ref 'unsigned-32 $fd-a 0)
            (foreign-ref 'unsigned-32 $fd-a 4)
            (foreign-ref 'unsigned-32 $fd-a 8)
            (foreign-ref 'unsigned-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 16)))
    `(#x-80000000 0 #x7fffffff #x-80000000 -1
      #x80000000 0 #x7fffffff #x80000000 #xffffffff))
  (equal?
    (begin
      (#%$object-set! 'unsigned-32 $raw-fd-a 0 #x-80000000)
      (#%$object-set! 'unsigned-32 $raw-fd-a 4 0)
      (#%$object-set! 'unsigned-32 $raw-fd-a 8 #x7fffffff)
      (#%$object-set! 'unsigned-32 $raw-fd-a 12 #x80000000)
      (#%$object-set! 'unsigned-32 $raw-fd-a 16 #xffffffff)
      (list (foreign-ref 'integer-32 $fd-a 0)
            (foreign-ref 'integer-32 $fd-a 4)
            (foreign-ref 'integer-32 $fd-a 8)
            (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'integer-32 $fd-a 16)
            (foreign-ref 'unsigned-32 $fd-a 0)
            (foreign-ref 'unsigned-32 $fd-a 4)
            (foreign-ref 'unsigned-32 $fd-a 8)
            (foreign-ref 'unsigned-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 16)))
    `(#x-80000000 0 #x7fffffff #x-80000000 -1
      #x80000000 0 #x7fffffff #x80000000 #xffffffff))
  (equal?
    (begin
      (#%$object-set! 'integer-32 $raw-fd-a 12 #xabcd1234)
      (list (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 12)))
    `(,(- #xabcd1234 #x100000000) #xabcd1234))
  (equal?
    (begin
      (#%$object-set! 'unsigned-32 $raw-fd-a 12 #x-765321ab)
      (list (foreign-ref 'integer-32 $fd-a 12)
            (foreign-ref 'unsigned-32 $fd-a 12)))
    `(#x-765321ab ,(+ #x-765321ab #x100000000)))
 ; integer-64/unsigned-64
  (equal?
    (begin
      (#%$object-set! 'integer-64 $raw-fd-a 16 #xabcd1234ffee8765)
      (list (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
    `(,(- #xabcd1234ffee8765 #x10000000000000000)
      #xabcd1234ffee8765
      ,(- #xabcd1234ffee8765 #x10000000000000000)
      #xabcd1234ffee8765))
  (equal?
    (begin
      (#%$object-set! 'unsigned-64 $raw-fd-a 16 #x-765321ab4c8e9de1)
      (list (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'integer-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'integer-32 $fd-a 16) 32)))
            (if (eq? (native-endianness) 'little)
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 16)
                  (ash (foreign-ref 'unsigned-32 $fd-a 20) 32))
                (logor
                  (foreign-ref 'unsigned-32 $fd-a 20)
                  (ash (foreign-ref 'unsigned-32 $fd-a 16) 32)))))
    `(#x-765321ab4c8e9de1
      ,(+ #x-765321ab4c8e9de1 #x10000000000000000)
      #x-765321ab4c8e9de1
      ,(+ #x-765321ab4c8e9de1 #x10000000000000000)))
  (equal?
    (begin
      (#%$object-set! 'integer-64 $raw-fd-a 0 #x-8000000000000000)
      (#%$object-set! 'integer-64 $raw-fd-a 8 0)
      (#%$object-set! 'integer-64 $raw-fd-a 16 #x7fffffffffffffff)
      (#%$object-set! 'integer-64 $raw-fd-a 24 #x8000000000000000)
      (#%$object-set! 'integer-64 $raw-fd-a 32 #xffffffffffffffff)
      (list (foreign-ref 'integer-64 $fd-a 0)
            (foreign-ref 'integer-64 $fd-a 8)
            (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'integer-64 $fd-a 24)
            (foreign-ref 'integer-64 $fd-a 32)
            (foreign-ref 'unsigned-64 $fd-a 0)
            (foreign-ref 'unsigned-64 $fd-a 8)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 24)
            (foreign-ref 'unsigned-64 $fd-a 32)))
    `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
      #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
  (equal?
    (begin
      (#%$object-set! 'unsigned-64 $raw-fd-a 0 #x-8000000000000000)
      (#%$object-set! 'unsigned-64 $raw-fd-a 8 0)
      (#%$object-set! 'unsigned-64 $raw-fd-a 16 #x7fffffffffffffff)
      (#%$object-set! 'unsigned-64 $raw-fd-a 24 #x8000000000000000)
      (#%$object-set! 'unsigned-64 $raw-fd-a 32 #xffffffffffffffff)
      (list (foreign-ref 'integer-64 $fd-a 0)
            (foreign-ref 'integer-64 $fd-a 8)
            (foreign-ref 'integer-64 $fd-a 16)
            (foreign-ref 'integer-64 $fd-a 24)
            (foreign-ref 'integer-64 $fd-a 32)
            (foreign-ref 'unsigned-64 $fd-a 0)
            (foreign-ref 'unsigned-64 $fd-a 8)
            (foreign-ref 'unsigned-64 $fd-a 16)
            (foreign-ref 'unsigned-64 $fd-a 24)
            (foreign-ref 'unsigned-64 $fd-a 32)))
    `(#x-8000000000000000 0 #x7fffffffffffffff #x-8000000000000000 -1
      #x8000000000000000 0 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff))
 ; fixnum
  (equal?
    (begin
      (#%$object-set! 'fixnum $raw-fd-a 0 (greatest-fixnum))
      (#%$object-set! 'fixnum $raw-fd-a 8 (least-fixnum))
      (#%$object-set! 'fixnum $raw-fd-a 16 0)
      (#%$object-set! 'fixnum $raw-fd-a 24 (quotient (greatest-fixnum) 2))
      (list
        (foreign-ref 'fixnum $fd-a 0)
        (foreign-ref 'fixnum $fd-a 8)
        (foreign-ref 'fixnum $fd-a 16)
        (foreign-ref 'fixnum $fd-a 24)))
    `(,(greatest-fixnum) ,(least-fixnum) 0 ,(quotient (greatest-fixnum) 2)))
 ; single-float
  (equal?
    (begin
      (#%$object-set! 'single-float $raw-fd-a 12 7.5)
      (foreign-ref 'single-float $fd-a 12))
    7.5)
 ; double-float
  (equal?
    (begin
      (#%$object-set! 'double-float $raw-fd-a 8 -5.4)
      (foreign-ref 'double-float $fd-a 8))
    -5.4)
 ; spot check unaligned ref/set
  (or (not $fd-unaligned-integers)
      (equal?
        (begin
          (#%$object-set! 'unsigned-32 $raw-fd-a 13 #x-765321ab)
          (list (foreign-ref 'integer-32 $fd-a 13)
                (foreign-ref 'unsigned-32 $fd-a 13)))
        `(#x-765321ab ,(+ #x-765321ab #x100000000))))
  (or (not $fd-unaligned-integers)
      (equal?
        (begin
          (#%$object-set! 'integer-64 $raw-fd-a 17 #xabcd1234ffee8765)
          (list (foreign-ref 'integer-64 $fd-a 17)
                (foreign-ref 'unsigned-64 $fd-a 17)
                (if (eq? (native-endianness) 'little)
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 17)
                      (ash (foreign-ref 'integer-32 $fd-a 21) 32))
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 21)
                      (ash (foreign-ref 'integer-32 $fd-a 17) 32)))
                (if (eq? (native-endianness) 'little)
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 17)
                      (ash (foreign-ref 'unsigned-32 $fd-a 21) 32))
                    (logor
                      (foreign-ref 'unsigned-32 $fd-a 21)
                      (ash (foreign-ref 'unsigned-32 $fd-a 17) 32)))))
        `(,(- #xabcd1234ffee8765 #x10000000000000000)
          #xabcd1234ffee8765
          ,(- #xabcd1234ffee8765 #x10000000000000000)
          #xabcd1234ffee8765)))
  (or (not $fd-unaligned-integers)
      (case $fd-short-max
        [(#xffff)
         (and
           (equal?
             (begin
               (#%$object-set! 'short $raw-fd-a 3 #xabcd)
               (list (foreign-ref 'short $fd-a 3)
                     (foreign-ref 'unsigned-short $fd-a 3)
                     (foreign-ref 'integer-16 $fd-a 3)
                     (foreign-ref 'unsigned-16 $fd-a 3)))
             `(,(- #xabcd #x10000) #xabcd ,(- #xabcd #x10000) #xabcd))
           (equal?
             (begin
               (#%$object-set! 'unsigned-short $raw-fd-a 3 -5321)
               (list (foreign-ref 'short $fd-a 3)
                     (foreign-ref 'unsigned-short $fd-a 3)
                     (foreign-ref 'integer-16 $fd-a 3)
                     (foreign-ref 'unsigned-16 $fd-a 3)))
             `(-5321 ,(+ -5321 #x10000) -5321 ,(+ -5321 #x10000))))]
        [else (error 'foreign-data-mat "unexpected $fd-short-max ~s" $fd-short-max)]))
  (or (not $fd-unaligned-floats)
      (equal?
        (begin
          (#%$object-set! 'single-float $raw-fd-a 6 7.5)
          (foreign-ref 'single-float $fd-a 6))
        7.5))
  (or (not $fd-unaligned-floats)
      (equal?
        (begin
          (#%$object-set! 'double-float $raw-fd-a 5 -5.4)
          (foreign-ref 'double-float $fd-a 5))
        -5.4))

 ; this needs to be done last
  (begin
    (set! $raw-fd-a #f)
    (set! $fd-a #f)
    (foreign-free $real-fd-a)
    (set! $real-fd-a #f)
    #t)
 )

(mat $integer-xxx?
  (not (#%$integer-8? 'a))
  (not (#%$integer-16? '3.4))
  (not (#%$integer-32? '3/4))
  (not (#%$integer-64? '4+3i))
  (not (#%$integer-8? #x-10000000000000000000000000000000000000000000000000000000000000000))
  (not (#%$integer-8? #x-81))
  (#%$integer-8? #x-80)
  (#%$integer-8? #x-1)
  (#%$integer-8? #x7f)
  (#%$integer-8? #x80)
  (#%$integer-8? #xff)
  (not (#%$integer-8? #x100))
  (not (#%$integer-8? #x+10000000000000000000000000000000000000000000000000000000000000000))
  (not (#%$integer-16? #x-10000000000000000000000000000000000000000000000000000000000000000))
  (not (#%$integer-16? #x-8001))
  (#%$integer-16? #x-8000)
  (#%$integer-16? #x-1)
  (#%$integer-16? #x7fff)
  (#%$integer-16? #x8000)
  (#%$integer-16? #xffff)
  (not (#%$integer-16? #x10000))
  (not (#%$integer-16? #x+10000000000000000000000000000000000000000000000000000000000000000))
  (not (#%$integer-32? #x-10000000000000000000000000000000000000000000000000000000000000000))
  (not (#%$integer-32? #x-80000001))
  (#%$integer-32? #x-80000000)
  (#%$integer-32? #x-1)
  (#%$integer-32? #x7fffffff)
  (#%$integer-32? #x80000000)
  (#%$integer-32? #xffffffff)
  (not (#%$integer-32? #x100000000))
  (not (#%$integer-32? #x+10000000000000000000000000000000000000000000000000000000000000000))
  (not (#%$integer-64? #x-10000000000000000000000000000000000000000000000000000000000000000))
  (not (#%$integer-64? #x-8000000000000001))
  (#%$integer-64? #x-8000000000000000)
  (#%$integer-64? #x-1)
  (#%$integer-64? #x7fffffffffffffff)
  (#%$integer-64? #x8000000000000000)
  (#%$integer-64? #xffffffffffffffff)
  (not (#%$integer-64? #x10000000000000000))
  (not (#%$integer-64? #x+10000000000000000000000000000000000000000000000000000000000000000))
)

(mat object-address
  (equal?
    (with-interrupts-disabled ; or lock r
      (let ()
        (import $system)
        (define-syntax record-field-address
          (lambda (x)
            (define-syntax datum
              (syntax-rules ()
                [(_ x) (syntax-object->datum #'x)]))
            (define rtd-flds
              (csv7:record-field-accessor
                (record-rtd (make-record-type "foo" '()))
                'flds))
            ; fld structure is vector:  #5(fld name mutable type offset)
            (define fld-check
              (lambda (who x)
                (unless (and (vector? x)
                             (= (vector-length x) 5)
                             (eq? (vector-ref x 0) 'fld))
                  (errorf who "~s is not a fld" x))))
            (define fld-name
              (lambda (x) (fld-check 'fld-name x) (vector-ref x 1)))
            (define fld-mutable?
              (lambda (x) (fld-check 'fld-mutable? x) (vector-ref x 2)))
            (define fld-type
              (lambda (x) (fld-check 'fld-type x) (vector-ref x 3)))
            (define fld-byte
              (lambda (x) (fld-check 'fld-byte x) (vector-ref x 4)))
            (syntax-case x ()
              [(_ recid record field-name)
               (and (identifier? #'recid) (identifier? #'field-name))
               (lambda (r)
                 (let ([rinfo (r #'recid)])
                   (unless (and (pair? rinfo)
                                (eq? (car rinfo) '#{record val9xfsq6oa12q4-a})
                                (record-type-descriptor? (cadr rinfo)))
                     (syntax-error #'recid "unrecognized record"))
                   (let ([rtd (cadr rinfo)])
                     (with-syntax ([offset
                                    (or (let ([field-name (datum field-name)])
                                          (ormap
                                            (lambda (fld)
                                              (and (eq? (fld-name fld) field-name)
                                                   (fld-byte fld)))
                                            (rtd-flds rtd)))
                                        (syntax-error
                                          "unrecognized field name"
                                          #'field-name))])
                       #'($object-address record offset)))))])))
        (define-record foo ((integer-32 x) (double-float y)))
        (let* ([r (make-foo 666 66.6)]
               [x (record-field-address foo r x)]
               [y (record-field-address foo r y)])
          (let ([t1 (foreign-ref 'integer-32 x 0)]
                [t2 (foreign-ref 'double-float y 0)])
            (foreign-set! 'integer-32 x 0 -1)
            (foreign-set! 'double-float y 0 .25)
            (list t1 t2 (foo-x r) (foo-y r))))))
    '(666 66.6 -1 .25))
  (#%$address-in-heap? (#%$object-address cons 0))
  (not (#%$address-in-heap? 0))
)

(mat record-inheritance
  (equal?
    (let ()
      (define-record soy ([double-float milk]))
      (define-record toast soy (y))
      (let ([x (make-toast #0=3.4 #1="hello")])
        (list (soy-milk x) (toast-y x))))
    '(#0# #1#))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record soy ([double-float milk]))
           (define-record toast soy (y))
           (let ([x (make-toast 3.4 "hello")])
             (list (soy-milk x) (toast-y x))))))
    `(let ([x (let ([y (#3%$record ',record-type-descriptor? . ,list?)])
                (#3%$object-set! 'double-float y ,fixnum? 3.4)
                y)])
       (#2%list
           (#3%$object-ref 'double-float x ,fixnum?)
           (#3%$object-ref 'scheme-object x ,fixnum?))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record soy ([double-float milk]))
           (define-record toast soy (y))
           (let ([x (make-toast 3.4 "hello")])
             (list (soy-milk x) (toast-y x))))))
    `(let ([x (let ([y (#3%$record ',record-type-descriptor? . ,list?)])
                (#3%$object-set! 'double-float y ,fixnum? 3.4)
                y)])
       (#3%list
         (#3%$object-ref 'double-float x ,fixnum?)
         (#3%$object-ref 'scheme-object x ,fixnum?))))
  (let ()
    (define-record p (x))
    (define-record c p (x))
    (let ()
      (define prtd (record-rtd (make-p 1)))
      (define crtd (record-rtd (make-c 1 2)))
      (let ()
        (define px1a (csv7:record-field-accessor prtd 'x))
        (define px1b (csv7:record-field-accessor prtd 0))
        (define cx1b (csv7:record-field-accessor crtd 0))
        (define cx2a (csv7:record-field-accessor crtd 'x))
        (define cx2b (csv7:record-field-accessor crtd 1))
        (define d1 (cons 1 2))
        (define d2 (cons 3 4))
        (let ()
          (define r (make-c d1 d2))
          (and (eq? (p-x r) d1)
               (eq? (px1a r) (p-x r))
               (eq? (px1b r) (p-x r))
               (eq? (cx1b r) (p-x r))
               (eq? (c-x r) d2)
               (eq? (cx2a r) (c-x r))
               (eq? (cx2b r) (c-x r)))))))
  (let ()
    (define-record p (x))
    (define-record c p (x))
    (record-reader 'c (record-rtd (make-c 1 2)))
    (let ([r1 (read (open-input-string "#[c #0=(a b) #0#]"))]
          [r2 (read (open-input-string "#0=#[c #0# 0]"))]
          [r3 (read (open-input-string "#0=#[c 0 #0#]"))]
          [r4 (read (open-input-string "#0=#[c #0# #0#]"))])
      (and (eq? (p-x r1) (c-x r1))
           (eq? (p-x r2) r2)
           (eq? (c-x r2) 0)
           (eq? (p-x r3) 0)
           (eq? (c-x r3) r3)
           (eq? (p-x r4) r4)
           (eq? (c-x r4) r4))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record p (x))
           (define-record c p (x))
           (record-reader 'c (record-rtd (make-c 1 2)))
           (let ([r1 (read (open-input-string "#[c #0=(a b) #0#]"))]
                 [r2 (read (open-input-string "#0=#[c #0# 0]"))]
                 [r3 (read (open-input-string "#0=#[c 0 #0#]"))]
                 [r4 (read (open-input-string "#0=#[c #0# #0#]"))])
             (and (eq? (p-x r1) (c-x r1))
                  (eq? (p-x r2) r2)
                  (eq? (c-x r2) 0)
                  (eq? (p-x r3) 0)
                  (eq? (c-x r3) r3)
                  (eq? (p-x r4) r4)
                  (eq? (c-x r4) r4))))))
    `(begin
       (#3%record-reader 'c ',record-type-descriptor?)
       (let ([r1 (#3%read (#3%open-input-string "#[c #0=(a b) #0#]"))]
             [r2 (#3%read (#3%open-input-string "#0=#[c #0# 0]"))]
             [r3 (#3%read (#3%open-input-string "#0=#[c 0 #0#]"))]
             [r4 (#3%read (#3%open-input-string "#0=#[c #0# #0#]"))])
         (if (#3%eq?
               (#3%$object-ref 'scheme-object r1 ,fixnum?)
               (#3%$object-ref 'scheme-object r1 ,fixnum?))
             (if (#3%eq? (#3%$object-ref 'scheme-object r2 ,fixnum?) r2)
                 (if (#3%eq? (#3%$object-ref 'scheme-object r2 ,fixnum?) 0)
                     (if (#3%eq? (#3%$object-ref 'scheme-object r3 ,fixnum?) 0)
                         (if (#3%eq? (#3%$object-ref 'scheme-object r3 ,fixnum?) r3)
                             (if (#3%eq? (#3%$object-ref 'scheme-object r4 ,fixnum?) r4)
                                 (#3%eq? (#3%$object-ref 'scheme-object r4 ,fixnum?) r4)
                                 #f)
                             #f)
                         #f)
                     #f)
                 #f)
             #f))))
)

(mat record-writer
  (equal?
    (with-output-to-string
      (lambda ()
        (define-record-type sp (fields lat))
        (record-writer (type-descriptor sp)
          (lambda (x p w) (w (sp-lat x) p)))
        (pretty-print (list (make-sp 'ugh)))))
    "(ugh)\n")
  (error? ; 'sp is not an rtd
    (with-output-to-string
      (lambda ()
        (define-record-type sp (fields lat))
        (record-writer 'sp
          (lambda (x p w) (w (sp-lat x) p))))))
  (error? ; "oops" is not a procedure
    (with-output-to-string
      (lambda ()
        (define-record-type sp (fields lat))
        (record-writer (type-descriptor sp) "oops"))))
  (error? ; ugh is not a textual output port
    (with-output-to-string
      (lambda ()
        (define-record-type sp (fields lat))
        (record-writer (type-descriptor sp)
          (lambda (x p w) (w p (sp-lat x))))
        (pretty-print (list (make-sp 'ugh))))))
  (error? ; procedure not a textual output port
    (with-output-to-string
      (lambda ()
        (define-record-type sp (fields lat))
        (record-writer (type-descriptor sp)
          (lambda (x p w) (w (sp-lat x) w)))
        (pretty-print (list (make-sp 'ugh))))))
  (begin
    (define-record $froz (a b) ([c (+ a b)]))
    (define-record $fruz $froz (d))
    (define-record $friz $fruz ())
    (define-record $fraz $friz ())
    (record-writer (type-descriptor $fraz)
      (lambda (x p wr)
        (display "<fraz>" p)))
    (record-writer (type-descriptor $froz)
      (lambda (x p wr)
        (wr `(* hi john ,($froz-c x) *) p)))
    (and (equal? (format "~s" (make-$froz 17 23)) "(* hi john 40 *)")
         (equal? (format "~s" (make-$fruz 17 24 37)) "(* hi john 41 *)")
         (equal? (format "~s" (make-$friz 17 25 38)) "(* hi john 42 *)")
         (equal? (format "~s" (make-$fraz 17 26 39)) "<fraz>")))
  (begin
    (record-writer (type-descriptor $froz)
      (lambda (x p wr)
        (fprintf p "<$froz c=~s>" ($froz-c x))))
    (and (equal? (format "~s" (make-$froz 18 23)) "<$froz c=41>")
         (equal? (format "~s" (make-$fruz 18 24 37)) "<$froz c=42>")
         (equal? (format "~s" (make-$friz 18 25 38)) "<$froz c=43>")
         (equal? (format "~s" (make-$fraz 18 26 39)) "<fraz>")))
  (begin
    (record-writer (type-descriptor $fruz)
      (lambda (x p wr)
        (fprintf p "<$fruz d=~s>" ($fruz-d x))))
    (and (equal? (format "~s" (make-$froz 19 23)) "<$froz c=42>")
         (equal? (format "~s" (make-$fruz 19 24 37)) "<$fruz d=37>")
         (equal? (format "~s" (make-$friz 19 25 38)) "<$fruz d=38>")
         (equal? (format "~s" (make-$fraz 18 26 39)) "<fraz>")))
  (let ()
    (define-record pair ((mutable car) (immutable cdr))
      ()
      ((constructor cons) (prefix "")))
    (record-writer (type-descriptor pair)
      (lambda (x p wr)
        (display "(" p) ; )
        (wr (car x) p)
        (display " . " p)
        (wr (cdr x) p) ; (
        (display ")" p)))
    (and (pair? (cons 3 4))
         (not (pair? '(3 . 4)))
         (eq? (car (cons 3 4)) 3)
         (eq? (cdr (cons 3 4)) 4)
         (equal? (format "~s" (cons 3 (cons 4 '()))) "(3 . (4 . ()))")
         (let ((x (cons 3 4)))
           (set-car! x x)
           (equal? (format "~s" x) "#0=(#0# . 4)"))))
)

(mat record-equal/hash
  (begin
    (define (equiv? v1 v2)
      (and (equal? v1 v2)
           (= (equal-hash v1) (equal-hash v2))
           (let ([ht (make-hashtable equal-hash equal?)])
             (hashtable-set! ht v1 "yes")
             (equal? "yes" (hashtable-ref ht v2 "no")))))
    
    (define (not-equiv? v1 v2)
      (and (not (equal? v1 v2))
           (let ([ht (make-hashtable equal-hash equal?)])
             (hashtable-set! ht v1 "yes")
             (equal? "no" (hashtable-ref ht v2 "no")))))

    (define-record-type E+H$a
      (fields (mutable x)
              (immutable y)))

    (define-record-type E+H$a+
      (parent E+H$a)
      (fields (mutable z)))

    (define-record-type E+H$b
      (fields (immutable x)
              (mutable y))
      (opaque #t))

    (define-record-type E+H$b+
      (parent E+H$b)
      (fields (mutable z))
      (opaque #t))

    (define (E+H$a-equal? a1 a2 eql?)
      (eql? (E+H$a-x a1) (E+H$a-x a2)))
    (define (E+H$a-hash a hc)
      (hc (E+H$a-x a)))

    (define (E+H$b-equal? b1 b2 eql?)
      (eql? (E+H$b-y b1) (E+H$b-y b2)))
    (define (E+H$b-hash b hc)
      (hc (E+H$b-y b)))

    (define cyclic-E+H$a1 (make-E+H$a 1 2))
    (E+H$a-x-set! cyclic-E+H$a1 cyclic-E+H$a1)
    (define cyclic-E+H$a2 (make-E+H$a 1 2))
    (E+H$a-x-set! cyclic-E+H$a2 cyclic-E+H$a2)

    (define cyclic-E+H$b+1 (make-E+H$b+ 1 2 3))
    (define cyclic-E+H$b+2 (make-E+H$b+ 1 2 3))
    (E+H$b-y-set! cyclic-E+H$b+1 (list 1 2 3 (box cyclic-E+H$b+2)))
    (E+H$b-y-set! cyclic-E+H$b+2 (list 1 2 3 (box cyclic-E+H$b+1)))

    #t)

  (not-equiv? (make-E+H$a 1 2) (make-E+H$a 1 2))
  (not-equiv? (make-E+H$b 1 2) (make-E+H$b 1 2))

  (not (record-type-equal-procedure (record-type-descriptor E+H$a)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$a)))
  (not (record-type-equal-procedure (record-type-descriptor E+H$a+)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$a+)))
  (not (record-type-equal-procedure (record-type-descriptor E+H$b+)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$b+)))
  (not (record-type-equal-procedure (record-type-descriptor E+H$b)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$b)))

  (begin
    (record-type-equal-procedure (record-type-descriptor E+H$a) E+H$a-equal?)
    (record-type-hash-procedure (record-type-descriptor E+H$a) E+H$a-hash)
    #t)

  (eq? (record-type-equal-procedure (record-type-descriptor E+H$a)) E+H$a-equal?)
  (eq? (record-type-hash-procedure (record-type-descriptor E+H$a)) E+H$a-hash)
  (not (record-type-equal-procedure (record-type-descriptor E+H$a+)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$a+)))
  (eq? (record-equal-procedure (make-E+H$a 1 2) (make-E+H$a 1 2)) E+H$a-equal?)
  (eq? (record-equal-procedure (make-E+H$a+ 1 3 5) (make-E+H$a 1 2)) E+H$a-equal?)
  (eq? (record-equal-procedure (make-E+H$a 1 2) (make-E+H$a+ 1 3 5)) E+H$a-equal?)
  (eq? (record-equal-procedure (make-E+H$a+ 1 3 5) (make-E+H$a+ 1 3 5)) E+H$a-equal?)
  (eq? (record-hash-procedure (make-E+H$a 1 2)) E+H$a-hash)
  (eq? (record-hash-procedure (make-E+H$a+ 1 3 5)) E+H$a-hash)
  (not (record-type-equal-procedure (record-type-descriptor E+H$a+)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$a+)))
  
  (equiv? (make-E+H$a 1 2) (make-E+H$a 1 2))
  (equiv? (make-E+H$a 1 2) (make-E+H$a 1 3))
  
  (equiv? (make-E+H$a 1 2) (make-E+H$a+ 1 3 5))
  (equiv? (make-E+H$a+ 1 3 5) (make-E+H$a 1 2))
  
  (not-equiv? (make-E+H$a+ 2 3 5) (make-E+H$a 1 2))
  (not-equiv? (make-E+H$a+ 2 3 5) (make-E+H$a+ 1 2 4))

  (not (equiv? (make-E+H$a 1 2) (make-E+H$b 1 2)))

  (not-equiv? (make-E+H$b 1 2) (make-E+H$b 1 2))
  (not-equiv? (make-E+H$b+ 1 2 3) (make-E+H$b+ 1 2 3))

  (not (record-type-equal-procedure (record-type-descriptor E+H$b+)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$b+)))
  (not (record-type-equal-procedure (record-type-descriptor E+H$b)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$b)))

  (begin
    (record-type-equal-procedure (record-type-descriptor E+H$b+) E+H$b-equal?)
    (record-type-hash-procedure (record-type-descriptor E+H$b+) E+H$b-hash)
    #t)

  (not-equiv? (make-E+H$b 1 2) (make-E+H$b 1 2))
  (equiv? (make-E+H$b+ 0 2 4) (make-E+H$b+ 1 2 3))

  (equiv? cyclic-E+H$a1 cyclic-E+H$a2)
  (equiv? cyclic-E+H$a1 (make-E+H$a cyclic-E+H$a2 3))

  (equiv? cyclic-E+H$b+1 cyclic-E+H$b+2)

  (begin
    (record-type-equal-procedure (record-type-descriptor E+H$a+) E+H$a-equal?)
    (record-type-hash-procedure (record-type-descriptor E+H$a+) E+H$a-hash)
    #t)

  (eq? (record-type-equal-procedure (record-type-descriptor E+H$a)) E+H$a-equal?)
  (eq? (record-type-hash-procedure (record-type-descriptor E+H$a)) E+H$a-hash)

  (equiv? (make-E+H$a+ 1 2 4) (make-E+H$a+ 1 3 5))
  (not-equiv? (make-E+H$a+ 1 3 5) (make-E+H$a 1 2))
  (not-equiv? (make-E+H$a 1 2) (make-E+H$a+ 1 3 5))

  (begin
    (record-type-equal-procedure (record-type-descriptor E+H$a) E+H$a-equal?)
    (record-type-hash-procedure (record-type-descriptor E+H$a) E+H$a-hash)
    #t)

  (not (record-equal-procedure (make-E+H$a+ 1 3 5) (make-E+H$a 1 2)))

  (equiv? (make-E+H$a+ 1 2 4) (make-E+H$a+ 1 3 5))
  (not-equiv? (make-E+H$a+ 1 3 5) (make-E+H$a 1 2))
  (not-equiv? (make-E+H$a 1 2) (make-E+H$a+ 1 3 5))

  (begin
    (record-type-equal-procedure (record-type-descriptor E+H$a+) #f)
    (record-type-hash-procedure (record-type-descriptor E+H$a+) #f)
    #t)

  (not (record-type-equal-procedure (record-type-descriptor E+H$a+)))
  (not (record-type-hash-procedure (record-type-descriptor E+H$a+)))

  (eq? (record-equal-procedure (make-E+H$a+ 1 3 5) (make-E+H$a 1 2)) E+H$a-equal?)
  (eq? (record-hash-procedure (make-E+H$a+ 1 3 5)) E+H$a-hash)

  (equiv? (make-E+H$a+ 1 3 5) (make-E+H$a 1 2))
  (equiv? (make-E+H$a 1 2) (make-E+H$a+ 1 3 5))
  (equiv? (make-E+H$a+ 1 2 4) (make-E+H$a+ 1 3 5))

  (error? ; not an rtd
    (record-type-equal-procedure 7))
  (error? ; not an rtd
    (record-type-equal-procedure 7 (lambda (x y e?) #f)))
  (error? ; not a procedure or #f
    (record-type-equal-procedure (record-type-descriptor E+H$a+) 7))
  (error? ; not an rtd
    (record-type-hash-procedure 7))
  (error? ; not an rtd
    (record-type-hash-procedure 7 (lambda (x y e?) #f)))
  (error? ; not a procedure or #f
    (record-type-hash-procedure (record-type-descriptor E+H$a+) 7))
  (error? ; not a record
    (record-equal-procedure 7 (make-E+H$a 1 2)))
  (error? ; not a record
    (record-equal-procedure (make-E+H$a 1 2) 7))
  (error? ; not a record
    (record-hash-procedure 7))

  ; csug examples
  (begin
    (define-record marble (color quality))
    #t)

  (not (record-type-equal-procedure (record-type-descriptor marble)))
  (not (equal? (make-marble 'blue 'medium) (make-marble 'blue 'medium)))
  (not (equal? (make-marble 'blue 'medium) (make-marble 'blue 'high)))

  ; Treat marbles as equal when they have the same color
  (begin
    (record-type-equal-procedure (record-type-descriptor marble)
      (lambda (m1 m2 eql?)
        (eql? (marble-color m1) (marble-color m2))))
    (record-type-hash-procedure (record-type-descriptor marble)
      (lambda (m hash)
        (hash (marble-color m))))
    #t)

  (equal? (make-marble 'blue 'medium) (make-marble 'blue 'high))
  (not (equal? (make-marble 'red 'high) (make-marble 'blue 'high)))

  (begin
    (define ht (make-hashtable equal-hash equal?))
    (hashtable-set! ht (make-marble 'blue 'medium) "glass")
    #t)

  (equal? (hashtable-ref ht (make-marble 'blue 'high) #f) "glass")

  (begin
    (define-record shooter marble (size))
    #t)

  (equal? (make-marble 'blue 'medium) (make-shooter 'blue 'large 17)) ;=> #t
  (equal? (make-shooter 'blue 'large 17) (make-marble 'blue 'medium)) ;=> #t
  (equal? (hashtable-ref ht (make-shooter 'blue 'high 17) #f) "glass")

  (begin
    (define-record-type node
      (nongenerative)
      (fields (mutable left) (mutable right)))
    (record-type-equal-procedure (record-type-descriptor node)
      (lambda (x y e?)
        (and
          (e? (node-left x) (node-left y))
          (e? (node-right x) (node-right y)))))
    (record-type-hash-procedure (record-type-descriptor marble)
      (lambda (x hash)
        (+ (hash (node-left x)) (hash (node-right x)) 23)))
    (define graph1
      (let ([x (make-node "a" (make-node #f "b"))])
        (node-left-set! (node-right x) x)
        x))
    (define graph2
      (let ([x (make-node "a" (make-node (make-node "a" #f) "b"))])
        (node-right-set! (node-left (node-right x)) (node-right x))
        x))
    (define graph3
      (let ([x (make-node "a" (make-node #f "c"))])
        (node-left-set! (node-right x) x)
        x))
    #t)

  (equal? graph1 graph2)
  (not (equal? graph1 graph3))
  (not (equal? graph2 graph3))

  (begin
    (define h (make-hashtable equal-hash equal?))
    (hashtable-set! h graph1 #t)
    #t)

  (hashtable-ref h graph1 #f)
  (hashtable-ref h graph2 #f)
  (not (hashtable-ref h graph3 #f))

  (begin
    (define record-hash
      (lambda (x hash)
        (let ([rtd (record-rtd x)])
          (do ([field-name* (csv7:record-type-field-names rtd) (cdr field-name*)]
               [i 0 (fx+ i 1)]
               [h 0 (+ h (hash ((csv7:record-field-accessor rtd i) x)))])
            ((null? field-name*) h)))))
    (define record-equal?
      (lambda (x y e?)
        (let ([rtd (record-rtd x)])
          (and (eq? (record-rtd y) rtd)
               (let f ([field-name* (csv7:record-type-field-names rtd)] [i 0])
                 (or (null? field-name*)
                     (and (let ([accessor (csv7:record-field-accessor rtd i)])
                            (e? (accessor x) (accessor y)))
                          (f (cdr field-name*) (fx+ i 1)))))))))
    (define equiv?
      (lambda (x y)
        (parameterize ([default-record-equal-procedure record-equal?])
          (equal? x y))))
    (define equiv-hash
      (lambda (x)
        (parameterize ([default-record-hash-procedure record-hash])
          (equal-hash x))))
    (define-record-type frob (fields (mutable q)))
    (define-record-type frub (fields (mutable x) y z))
    (define frob-hash
      (lambda (x hash)
        (raise 'frob-hash)))
    (define frob-equal?
      (lambda (x y e?)
        #f))
    (define rthp
      (lambda (rtd)
        (case-lambda
          [() (record-type-hash-procedure rtd)]
          [(x) (record-type-hash-procedure rtd x)])))
    (define rtep
      (lambda (rtd)
        (case-lambda
          [() (record-type-equal-procedure rtd)]
          [(x) (record-type-equal-procedure rtd x)])))
    #t)
  (not (record-type-equal-procedure (record-type-descriptor frob)))
  (not (record-type-hash-procedure (record-type-descriptor frob)))
  (not (record-type-equal-procedure (record-type-descriptor frub)))
  (not (record-type-hash-procedure (record-type-descriptor frub)))
  (equal?
    (parameterize ([(rthp (record-type-descriptor frob)) record-hash])
      (list
        (record-hash-procedure (make-frob #\q))
        (record-hash-procedure (make-frub 1 2 3))))
    (list record-hash #f))
  (equal?
    (parameterize ([(rtep (record-type-descriptor frob)) record-equal?])
      (list
        (record-equal-procedure (make-frub 1 2 3) (make-frub 1 2 3))
        (record-equal-procedure (make-frub 1 2 3) (make-frob #\q))
        (record-equal-procedure (make-frob #\q) (make-frub 1 2 3))
        (record-equal-procedure (make-frob #\q) (make-frob #\q))))
    (list #f #f #f record-equal?))
  (equal?
    (parameterize ([default-record-hash-procedure record-hash])
      (list
        (record-hash-procedure (make-frob #\q))
        (record-hash-procedure (make-frub 1 2 3))))
    (list record-hash record-hash))
  (equal?
    (parameterize ([default-record-equal-procedure record-equal?])
      (list
        (record-equal-procedure (make-frub 1 2 3) (make-frub 1 2 3))
        (record-equal-procedure (make-frub 1 2 3) (make-frob #\q))
        (record-equal-procedure (make-frob #\q) (make-frub 1 2 3))
        (record-equal-procedure (make-frob #\q) (make-frob #\q))))
    (list record-equal? record-equal? record-equal? record-equal?))
  (equal?
    (parameterize ([default-record-hash-procedure record-hash]
                   [(rthp (record-type-descriptor frob)) frob-hash])
      (list
        (record-hash-procedure (make-frob #\q))
        (record-hash-procedure (make-frub 1 2 3))))
    (list frob-hash record-hash))
  (equal?
    (parameterize ([default-record-equal-procedure record-equal?]
                   [(rtep (record-type-descriptor frob)) frob-equal?])
      (list
        (record-equal-procedure (make-frub 1 2 3) (make-frub 1 2 3))
        (record-equal-procedure (make-frub 1 2 3) (make-frob #\q))
        (record-equal-procedure (make-frob #\q) (make-frub 1 2 3))
        (record-equal-procedure (make-frob #\q) (make-frob #\q))))
    (list record-equal? #f #f frob-equal?))
  ((lambda (x) (and (integer? x) (exact? x) (nonnegative? x)))
    (parameterize ([default-record-hash-procedure record-hash])
      (equal-hash (vector 1 2 (make-frub 1 2 3) 5 (make-frob #\q) 7))))
  (eq?
    (guard (c [(eq? c 'frob-hash) 'yup] [else (raise c)])
      (parameterize ([default-record-hash-procedure record-hash]
                     [(rthp (record-type-descriptor frob)) frob-hash])
        (equal-hash (list "hello" (make-frob #\q)))))
    'yup)
  ((lambda (x) (and (integer? x) (exact? x) (nonnegative? x)))
    (parameterize ([default-record-hash-procedure record-hash]
                   [(rthp (record-type-descriptor frob)) frob-hash])
      (equal-hash (vector 1 2 (make-frub 1 2 3) 5 6))))
  (equiv? (make-frob #\q) (make-frob #\q))
  (equiv? (make-frub 1 2 3) (make-frub 1 2 3))
  (not (parameterize ([(rtep (record-type-descriptor frob)) frob-equal?])
         (equiv? (make-frob #\q) (make-frob #\q))))
  (parameterize ([(rtep (record-type-descriptor frob)) frob-equal?])
    (equiv? (make-frub 1 2 3) (make-frub 1 2 3)))
  (equal?
    (let ([ht (make-hashtable equiv-hash equiv?)])
      (hashtable-set! ht (make-frob #\q) 'one)
      (hashtable-set! ht (make-frub 1 2 3) 'two)
      (hashtable-set! ht (make-frub 'a 'b 'c) 'three)
      (list
        (hashtable-ref ht (make-frob #\q) #f)
        (hashtable-ref ht (make-frub 1 2 3) #f)
        (hashtable-ref ht (make-frub 'a 'b 'c) #f)
        (hashtable-ref ht (make-frub 'x 'y 'z) #f)))
    '(one two three #f))
)

(mat record19
 ; test ellipses in init expressions
  (equal?
    (let ()
      (define-record foo ()
        ([a (let ()
              (define-syntax f
                (syntax-rules ()
                  [(_ b ...) (list 'b ...)]))
              (f 1 2 3))]))
      (foo-a (make-foo)))
    '(1 2 3))
)

(mat record20
 ; test argument-name handing in generated record constructors
  (equal?
    (let ()
      (define foo
        (make-record-type "foo"
          '((integer-32 fixnum?)
          (double-float flonum?)
          unless
          unless)))
      (let ()
        (define make-foo (record-constructor foo))
        (define foo? (record-predicate foo))
        (define foo.0 (csv7:record-field-accessor foo 'fixnum?))
        (define foo.1 (csv7:record-field-accessor foo 'flonum?))
        (define foo.2 (csv7:record-field-accessor foo 2))
        (define foo.3 (csv7:record-field-accessor foo 3))
        (let ([x (make-foo 1 3.0 'a 'b)])
          (list (foo? x)
                (foo.0 x)
                (foo.1 x)
                (foo.2 x)
                (foo.3 x)))))
    '(#t 1 3.0 a b))
  (equal?
    (let ([foo (make-record-type "foo" '(a a a))])
      (define make-foo (record-constructor foo))
      (define foo? (record-predicate foo))
      (define foo.0 (csv7:record-field-accessor foo 0))
      (define foo.1 (csv7:record-field-accessor foo 1))
      (define foo.2 (csv7:record-field-accessor foo 2))
      (let ([x (make-foo 'a 'b 'c)])
        (list (foo? x)
              (foo.0 x)
              (foo.1 x)
              (foo.2 x))))
    '(#t a b c))
  (equal?
    (let* ([names '(a a a a a a a a a a a a)]
           [foo (make-record-type "foo" names)])
      (define make-foo (record-constructor foo))
      (define foo? (record-predicate foo))
      (define foos (let ([n (length names)])
                     (let f ([i 0])
                       (if (= i n)
                           '()
                           (cons (csv7:record-field-accessor foo i)
                                 (f (+ i 1)))))))
      (let ([x (make-foo 1 2 3 4 5 6 7 8 9 10 11 12)])
        (cons (foo? x) (map (lambda (p) (p x)) foos))))
    '(#t 1 2 3 4 5 6 7 8 9 10 11 12))
  (equal?
    (let* ([foo (make-record-type "foo" '((integer-32 a)))]
           [bar (make-record-type foo "bar" '((double-float a)))])
      (define make-bar (record-constructor bar))
      (define bar? (record-predicate bar))
      (define bar.0 (csv7:record-field-accessor bar 0))
      (define bar.1 (csv7:record-field-accessor bar 1))
      (let ([x (make-bar 17 23.5)])
        (list (bar? x) (bar.0 x) (bar.1 x))))
    '(#t 17 23.5))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let* ([foo (make-record-type "foo" '((integer-32 a)))]
                [bar (make-record-type foo "bar" '((double-float a)))])
           (define make-bar (record-constructor bar))
           (define bar? (record-predicate bar))
           (define bar.0 (csv7:record-field-accessor bar 0))
           (define bar.1 (csv7:record-field-accessor bar 1))
           (let ([x (make-bar 17 23.5)])
             (list (bar? x) (bar.0 x) (bar.1 x))))))
    `(let ([x (let ([y (#3%$record (#2%make-record-type (#2%make-record-type "foo" '((integer-32 a))) "bar" '((double-float a))) . ,list?)])
                (#3%$object-set! 'double-float y ,fixnum? 23.5)
                (#3%$object-set! 'integer-32 y ,fixnum? 17)
                y)])
       (#2%list
         #t
         (#3%$object-ref 'integer-32 x ,fixnum?)
         (#3%$object-ref 'double-float x ,fixnum?))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let* ([foo (make-record-type "foo" '((integer-32 a)))]
                [bar (make-record-type foo "bar" '((double-float a)))])
           (define make-bar (record-constructor bar))
           (define bar? (record-predicate bar))
           (define bar.0 (csv7:record-field-accessor bar 0))
           (define bar.1 (csv7:record-field-accessor bar 1))
           (let ([x (make-bar 17 23.5)])
             (list (bar? x) (bar.0 x) (bar.1 x))))))
    `(let ([x (let ([y (#3%$record (#3%make-record-type (#3%make-record-type "foo" '((integer-32 a))) "bar" '((double-float a))) . ,list?)])
                (#3%$object-set! 'double-float y ,fixnum? 23.5)
                (#3%$object-set! 'integer-32 y ,fixnum? 17)
                y)])
       (#3%list
         #t
         (#3%$object-ref 'integer-32 x ,fixnum?)
         (#3%$object-ref 'double-float x ,fixnum?))))
)

(mat record21 ; duplicate field names and invalid field syntax
  (error? ; duplicate field name
    (define-record foo (x x)))
  (error? ; duplicate field name
    (define-record foo (x (mutable x))))
  (error? ; duplicate field name
    (define-record foo (x) ([x 3])))
  (error? ; duplicate field name
    (define-record foo (x) ([(immutable x) 3])))
  (error? ; duplicate field name
    (define-record foo () ([x 4] [x 3])))
  (error? ; duplicate field name
    (define-record foo () ([x 4] [(immutable x) 3])))
  (error? ; invalid field syntax
    (define-record foo ([x 4])))
  (error? ; invalid field syntax
    (define-record foo ([(mutable foo) 3])))
  (error? ; duplicate field name
   ; would be okay if we used field name rather than record name as template
   ; for generated accessor and mutator identifiers
    (equal?
      (let ()
        (define-syntax a
          (syntax-rules ()
            [(_ name fld get)
             (begin
               (define-record name (fld x) () ([prefix ""]))
               (define get x))]))
        (a rt x g)
        (let ([r (make-rt 3 4)])
          (list (x r) (g r))))
      '(3 4)))
)

(mat record22 ; make sure inlined record routines make proper checks
  (eqv?
    (let ()
      (define ty (make-record-type "bar" '((mutable q))))
      (define q! (csv7:record-field-mutator ty 'q))
      (let ([x ((record-constructor ty) 3)])
        (q! x 'hello)
        ((csv7:record-field-accessor ty 0) x)))
    'hello)
  (error?
    (let ()
      (define ty (make-record-type "bar" '((immutable q))))
      (define q! (csv7:record-field-mutator ty 'q))
      (let ([x ((record-constructor ty) 3)])
        (q! x 'hello)
        ((csv7:record-field-accessor ty 0) x))))
  (procedure?
    (lambda ()
      (define ty (make-record-type "bar" '((immutable q))))
      (define q! (csv7:record-field-mutator ty 'q))
      (let ([x ((record-constructor ty) 3)])
        (q! x 'hello)
        ((csv7:record-field-accessor ty 0) x))))
  (error?
    (let ()
      (define ty (make-record-type "bar" '((mutable q))))
      (define q! (csv7:record-field-mutator ty 'q))
      (let ([x ((record-constructor ty) 3)])
        (q! x 'hello)
        ((csv7:record-field-accessor ty 'z) x))))
  (procedure?
    (lambda ()
      (define ty (make-record-type "bar" '((mutable q))))
      (define q! (csv7:record-field-mutator ty 'q))
      (let ([x ((record-constructor ty) 3)])
        (q! x 'hello)
        ((csv7:record-field-accessor ty 'z) x))))
  (error?
    (let ()
      (define ty (make-record-type "bar" '((mutable q))))
      (define q! (csv7:record-field-mutator ty 'z))
      (let ([x ((record-constructor ty) 3)])
        (q! x 'hello)
        ((csv7:record-field-accessor ty 0) x))))
  (procedure?
    (lambda ()
      (define ty (make-record-type "bar" '((mutable q))))
      (define q! (csv7:record-field-mutator ty 'z))
      (let ([x ((record-constructor ty) 3)])
        (q! x 'hello)
        ((csv7:record-field-accessor ty 0) x))))
  (error?
    (let ()
      (define ty (make-record-type "bar" '((mutable q))))
      (csv7:record-field-accessible? ty 3)))
  (procedure?
    (lambda ()
      (define ty (make-record-type "bar" '((mutable q))))
      (csv7:record-field-accessible? ty 3)))
  (equal?
    (let ([n 0])
      (define ty (make-record-type "bar" '((mutable q))))
      (let ([b (csv7:record-field-accessible? (begin (set! n (+ n 5)) ty) (begin (set! n (+ n 12)) 0))])
        (cons b n)))
    '(#t . 17))
  (error?
    (let ()
      (define ty (make-record-type "bar" '((mutable q))))
      (csv7:record-field-mutable? ty 'notq)))
  (procedure?
    (lambda ()
      (define ty (make-record-type "bar" '((mutable q))))
      (csv7:record-field-mutable? ty 'notq)))
  (error?
    (let ()
      (define ty (make-record-type "bar" '((mutable creepy q))))
      (csv7:record-field-mutable? ty 'notq)))
  (procedure?
    (lambda ()
      (define ty (make-record-type "bar" '((mutable creepy q))))
      (csv7:record-field-mutable? ty 'notq)))
  (error?
    (let ()
      (define-record bar ((immutable creepy q)))
      (make-bar 3)))
  (error?
    (lambda ()
      (define-record bar ((immutable creepy q)))
      (make-bar 3)))
)

(mat record23 ; test general make-record-type interface
  (equal?
    (let ()
      (define enum-base-rtd
        (make-record-type ; not sealed, not opaque
          #!base-rtd                    ; undocumented $base-rtd
          '#{enum b9s78zmm79qs7j22-a}   ; make enum-base-rtd type nongenerative
          '((immutable sym->index) (immutable index->sym))))
      (define get-sym->index
        (csv7:record-field-accessor enum-base-rtd 'sym->index))
      (define get-index->sym
        (csv7:record-field-accessor enum-base-rtd 'index->sym))
      (define enum-parent-rtd ; not sealed, not opaque
        (make-record-type "enum-parent" '((immutable members))))
      (define get-members (csv7:record-field-accessor enum-parent-rtd 'members))
    
      (let ([this-enum-rtd
             (#%$make-record-type enum-base-rtd enum-parent-rtd "enum"
               '()               ; no fields to add
               #t                ; sealed
               #f                ; not opaque
               '*sym->index*     ; extras (tacked onto end of rtd)
               '*index->sym*)])  ; i.e., static (per enumeration type) fields
        (let ([make-this-enum (record-constructor this-enum-rtd)])
    
          (let ([enum (make-this-enum '*members*)])
            (let ([rtd (record-rtd enum)])
              (list
                (get-members enum)
                (get-sym->index rtd)
                (get-index->sym rtd)))))))
    '(*members* *sym->index* *index->sym*))
  (error? ; cannot extend sealed record type
    (let ([rtd1 (#%$make-record-type #!base-rtd #f "foo" '() #t #f '())])
      (#%$make-record-type #!base-rtd rtd1 "bar" '() #f #f '())))
)

(mat record25
 ; test generic C aliases for specific types
  (begin
    (define-record r25-bar ((int a) (unsigned b) (unsigned-int c)
                            (short d) (unsigned-short e)
                            (long f) (unsigned-long g)
                            (iptr h) (uptr i)
                            (float j) (double k)
                            (ptr l) (char m) (wchar n) (fixnum o)
                            (void* p) (boolean q)
                            (long-long r) (unsigned-long-long s)))
    #t)
  (error? (make-r25-bar 1.0 2 3 4 5 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2.0 3 4 5 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 'three 4 5 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 1/4 5 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 "five" 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 '(6) 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 6 '#(a b c d e f g) 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 6 7 'ate 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 6 7 8 #\9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 6 7 8 9 10 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 6 7 8 9 10.0 11.0+0.0i 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 6 7 8 9 10.0 11.0 #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
  (error? (make-r25-bar 1 2 3 4 5 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12.0 13))
  (error? (make-r25-bar 1 2 3 4 5 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13.0))
  (begin
    (define r25-x (make-r25-bar 1 2 3 4 5 6 7 8 9 10.0 11.0 'blue #\a #\x3bb 75 #xc7c7c7c7 'a 12 13))
    (and (r25-bar? r25-x) (not (r25-bar? '(foo)))))
  (error? (set-r25-bar-a! r25-x 3.0))
  (eq? (set-r25-bar-a! r25-x (+ (r25-bar-a r25-x) 73)) (void))
  (error? (set-r25-bar-b! r25-x 3.0))
  (eq? (set-r25-bar-b! r25-x (+ (r25-bar-b r25-x) 73)) (void))
  (error? (set-r25-bar-c! r25-x 3.0))
  (eq? (set-r25-bar-c! r25-x (+ (r25-bar-c r25-x) 73)) (void))
  (error? (set-r25-bar-d! r25-x 3.0))
  (eq? (set-r25-bar-d! r25-x (+ (r25-bar-d r25-x) 73)) (void))
  (error? (set-r25-bar-e! r25-x 3.0))
  (eq? (set-r25-bar-e! r25-x (+ (r25-bar-e r25-x) 73)) (void))
  (error? (set-r25-bar-f! r25-x 3.0))
  (eq? (set-r25-bar-f! r25-x (- (r25-bar-f r25-x) 73)) (void))
  (error? (set-r25-bar-g! r25-x 3.0))
  (eq? (set-r25-bar-g! r25-x (+ (r25-bar-g r25-x) 73)) (void))
  (error? (set-r25-bar-h! r25-x 3.0))
  (eq? (set-r25-bar-h! r25-x (+ (r25-bar-h r25-x) 73)) (void))
  (error? (set-r25-bar-i! r25-x 3.0))
  (eq? (set-r25-bar-i! r25-x (+ (r25-bar-i r25-x) 73)) (void))
  (error? (set-r25-bar-j! r25-x 3))
  (eq? (set-r25-bar-j! r25-x (+ (r25-bar-j r25-x) 73)) (void))
  (error? (set-r25-bar-k! r25-x 3))
  (eq? (set-r25-bar-k! r25-x (+ (r25-bar-k r25-x) 73)) (void))
  (eq? (set-r25-bar-l! r25-x (cons (r25-bar-l r25-x) 73)) (void))
  (error? (set-r25-bar-m! r25-x 3.0))
  (eq? (set-r25-bar-m! r25-x (integer->char (+ (char->integer (r25-bar-m r25-x)) 1))) (void))
  (error? (set-r25-bar-n! r25-x 3.0))
  (eq? (set-r25-bar-n! r25-x (integer->char (+ (char->integer (r25-bar-n r25-x)) 1))) (void))
  (error? (set-r25-bar-o! r25-x 3.0))
  (eq? (set-r25-bar-o! r25-x (+ (r25-bar-o r25-x) 73)) (void))
  (error? (set-r25-bar-p! r25-x 3.0))
  (eq? (set-r25-bar-p! r25-x (+ (r25-bar-p r25-x) 73)) (void))
  (eq? (set-r25-bar-q! r25-x (not (r25-bar-q r25-x))) (void))
  (error? (set-r25-bar-r! r25-x 3.0))
  (eq? (set-r25-bar-r! r25-x (- (r25-bar-r r25-x) 73)) (void))
  (error? (set-r25-bar-s! r25-x 3.0))
  (eq? (set-r25-bar-s! r25-x (+ (r25-bar-s r25-x) 73)) (void))
  (equal?
    (list
      (r25-bar-a r25-x)
      (r25-bar-b r25-x)
      (r25-bar-c r25-x)
      (r25-bar-d r25-x)
      (r25-bar-e r25-x)
      (r25-bar-f r25-x)
      (r25-bar-g r25-x)
      (r25-bar-h r25-x)
      (r25-bar-i r25-x)
      (r25-bar-j r25-x)
      (r25-bar-k r25-x)
      (r25-bar-l r25-x)
      (r25-bar-m r25-x)
      (r25-bar-n r25-x)
      (r25-bar-o r25-x)
      (r25-bar-p r25-x)
      (r25-bar-q r25-x)
      (r25-bar-r r25-x)
      (r25-bar-s r25-x))
    '(74 75 76 77 78 -67 80 81 82 83.0 84.0 (blue . 73) #\b #\x3bc 148 #xc7c7c810 #f -61 86))
  (error? (set-r25-bar-a! r25-x (expt 256 (foreign-sizeof 'int))))
  (error? (set-r25-bar-a! r25-x (- -1 (ash (expt 256 (foreign-sizeof 'int)) -1))))
  (begin
    (define ($test-int x size get put)
      (let* ([n10000 (expt 256 size)]
             [nffff (- n10000 1)]
             [n8000 (ash n10000 -1)]
             [n7fff (- n8000 1)]
             [n-8000 (- n8000)]
             [n-8001 (- n-8000 1)])
        (and
          (or (= (optimize-level) 3) (guard (c [#t]) (put x n10000) #f))
          (eq? (put x nffff) (void))
          (eqv? (get x) -1)
          (eq? (put x n8000) (void))
          (eqv? (get x) n-8000)
          (eq? (put x n7fff) (void))
          (eqv? (get x) n7fff)
          (eq? (put x 0) (void))
          (eqv? (get x) 0)
          (eq? (put x -1) (void))
          (eqv? (get x) -1)
          (eq? (put x n-8000) (void))
          (eqv? (get x) n-8000)
          (or (= (optimize-level) 3) (guard (c [#t]) (put x n-8001) #f))
          (eqv? (get x) n-8000))))
    (define ($test-uint x size get put)
      (let* ([n10000 (expt 256 size)]
             [nffff (- n10000 1)]
             [n8000 (ash n10000 -1)]
             [n7fff (- n8000 1)]
             [n-8000 (- n8000)]
             [n-8001 (- n-8000 1)])
        (and
          (or (= (optimize-level) 3) (guard (c [#t]) (put x n10000) #f))
          (eq? (put x nffff) (void))
          (eqv? (get x) nffff)
          (eq? (put x n8000) (void))
          (eqv? (get x) n8000)
          (eq? (put x n7fff) (void))
          (eqv? (get x) n7fff)
          (eq? (put x 0) (void))
          (eqv? (get x) 0)
          (eq? (put x -1) (void))
          (eqv? (get x) nffff)
          (eq? (put x n-8000) (void))
          (eqv? (get x) n8000)
          (or (= (optimize-level) 3) (guard (c [#t]) (put x n-8001) #f))
          (eqv? (get x) n8000))))
    (define ($test-fixnum x get put)
      (let ([n8000 (+ (greatest-fixnum) 1)]
            [n7fff (greatest-fixnum)]
            [n-8000 (least-fixnum)]
            [n-8001 (- (least-fixnum) 1)])
        (and
          (or (= (optimize-level) 3) (guard (c [#t]) (put x n8000) #f))
          (eq? (put x n7fff) (void))
          (eqv? (get x) n7fff)
          (eq? (put x 0) (void))
          (eqv? (get x) 0)
          (eq? (put x -1) (void))
          (eqv? (get x) -1)
          (eq? (put x n-8000) (void))
          (eqv? (get x) n-8000)
          (or (= (optimize-level) 3) (guard (c [#t]) (put x n-8001) #f))
          (eqv? (get x) n-8000))))
    #t)
  ($test-int r25-x (foreign-sizeof 'int) r25-bar-a set-r25-bar-a!)
  ($test-uint r25-x (foreign-sizeof 'unsigned) r25-bar-b set-r25-bar-b!)
  ($test-uint r25-x (foreign-sizeof 'unsigned-int) r25-bar-c set-r25-bar-c!)
  ($test-int r25-x (foreign-sizeof 'short) r25-bar-d set-r25-bar-d!)
  ($test-uint r25-x (foreign-sizeof 'unsigned-short) r25-bar-e set-r25-bar-e!)
  ($test-int r25-x (foreign-sizeof 'long) r25-bar-f set-r25-bar-f!)
  ($test-uint r25-x (foreign-sizeof 'unsigned-long) r25-bar-g set-r25-bar-g!)
  ($test-int r25-x (foreign-sizeof 'long-long) r25-bar-r set-r25-bar-r!)
  ($test-uint r25-x (foreign-sizeof 'unsigned-long-long) r25-bar-s set-r25-bar-s!)
  ($test-int r25-x (foreign-sizeof 'iptr) r25-bar-h set-r25-bar-h!)
  ($test-uint r25-x (foreign-sizeof 'uptr) r25-bar-i set-r25-bar-i!)
  ($test-fixnum r25-x r25-bar-o set-r25-bar-o!)
  ($test-uint r25-x (foreign-sizeof 'void*) r25-bar-p set-r25-bar-p!)
)

(mat fasl-records
  ; make sure we can fasl out cyclic record type descriptors
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define $fsr-a
             (let ()
               (define-syntax a
                 (lambda (x)
                   (let* ([rtd1 (#%$make-record-type #!base-rtd #!base-rtd
                                  "rtd1" '((mutable q)) #f #f)]
                          [rtd2 (#%$make-record-type rtd1 #!base-rtd
                                  "rtd2" '() #f #f #f)])
                     ((record-mutator rtd1 0) rtd2 rtd2)
                     #`(quote #,rtd2))))
               a))))
      'replace)
    (load "testfile.ss")
    #t)
  (eq?
    ((record-accessor (record-rtd $fsr-a) 0) $fsr-a)
    $fsr-a)
  (begin
    (separate-compile "testfile")
    (load "testfile.so")
    #t)
  (eq?
    ((record-accessor (record-rtd $fsr-a) 0) $fsr-a)
    $fsr-a)
  ; ... even when cycle involves the record's base rtd
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define $fsr-b
             (let ()
               (define-syntax a
                 (lambda (x)
                   (let* ([rtd1 (#%$make-record-type #!base-rtd #!base-rtd
                                  "rtd1" '((mutable q)) #f #f)]
                          [rtd2 (#%$make-record-type rtd1 #!base-rtd
                                  "rtd2" '() #f #f #f)]
                          [rtd3 (#%$make-record-type rtd2 #!base-rtd
                                  "rtd3" '() #f #f)])
                     ((record-mutator rtd1 0) rtd2 rtd3)
                     #`(quote #,rtd3))))
               a))))
      'replace)
    (load "testfile.ss")
    #t)
  (eq?
    ((record-accessor (record-rtd (record-rtd $fsr-b)) 0) (record-rtd $fsr-b))
    $fsr-b)
  (begin
    (separate-compile "testfile")
    (load "testfile.so")
    #t)
  (eq?
    ((record-accessor (record-rtd (record-rtd $fsr-b)) 0) (record-rtd $fsr-b))
    $fsr-b)
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define $fsr-c
             (let ()
               (define-syntax a
                 (lambda (x)
                   (let* ([rtd1 (#%$make-record-type #!base-rtd #!base-rtd
                                  "rtd1" '((mutable q)) #f #f)]
                          [rtd2 (#%$make-record-type rtd1 #!base-rtd
                                  "rtd2" '() #f #f #f)]
                          [rtd3 (#%$make-record-type rtd2 #f
                                  "rtd3" '((immutable a)) #f #f)])
                     ((record-mutator rtd1 0) rtd2 ((record-constructor rtd3) 23))
                     #`(quote #,rtd3))))
               a))))
      'replace)
    (load "testfile.ss")
    #t)
  (record?
    ((record-accessor (record-rtd (record-rtd $fsr-c)) 0) (record-rtd $fsr-c))
    $fsr-c)
  (begin
    (separate-compile "testfile")
    (load "testfile.so")
    #t)
  (record?
    ((record-accessor (record-rtd (record-rtd $fsr-c)) 0) (record-rtd $fsr-c))
    $fsr-c)
  ; fasl out typed fields
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define $fsr-d-inst
             (let ()
               (define-syntax a
                 (lambda (x)
                   (define-record $fsr-d ((immutable integer-32 a) (mutable unsigned-40 b)))
                   #`(quote #,(make-$fsr-d #x1234abcd #xfedcba6543))))
               a))))
      'replace)
    (load "testfile.ss")
    #t)
  (eqv?
    ((record-accessor (record-rtd $fsr-d-inst) 0) $fsr-d-inst)
    #x1234abcd)
  (eqv?
    ((record-accessor (record-rtd $fsr-d-inst) 1) $fsr-d-inst)
    #xfedcba6543)
  (begin
    (separate-compile "testfile")
    (load "testfile.so")
    #t)
  (eqv?
    ((record-accessor (record-rtd $fsr-d-inst) 0) $fsr-d-inst)
    #x1234abcd)
  (eqv?
    ((record-accessor (record-rtd $fsr-d-inst) 1) $fsr-d-inst)
    #xfedcba6543)
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(eval-when (compile load)
             (define-record $fsr-e
               ((immutable integer-8 i8)
                (immutable integer-16 i16)
                (immutable integer-24 i24)
                (immutable integer-32 i32)
                (immutable integer-40 i40)
                (immutable integer-48 i48)
                (immutable integer-56 i56)
                (immutable integer-64 i64)
                (immutable unsigned-8 u8)
                (immutable unsigned-16 u16)
                (immutable unsigned-24 u24)
                (immutable unsigned-32 u32)
                (immutable unsigned-40 u40)
                (immutable unsigned-48 u48)
                (immutable unsigned-56 u56)
                (immutable unsigned-64 u64)
                (immutable char c)
                (immutable single-float sf)
                (immutable wchar wc)
                (immutable double-float df)
                (immutable fixnum f)))))
        (pretty-print
          '(define $fsr-e-inst1
             (let-syntax ([a (lambda (x)
                               #`'#,(make-$fsr-e 0 -1 0 -1 0 -1 0 -1
                                      0 #xffff 0 #xffffffff 0 #xffffffffffff
                                      0 #xffffffffffffffff
                                      #\nul 3.14 #\x3bc -3.14 0))])
               a)))
        (pretty-print
          '(define $fsr-e-inst2
             (let-syntax ([a (lambda (x)
                               #`'#,(make-$fsr-e -1 0 -1 0 -1 0 -1 0
                                      #xff 0 #xffffff 0 #xffffffffff 0
                                      #xffffffffffffff 0
                                      #\a -3.14 #\nul 3.14 -1))])
               a)))
        (pretty-print
          '(define $fsr-e-inst3
             (let-syntax ([a (lambda (x)
                               #`'#,(make-$fsr-e
                                      #x7f #x-8000 #x7fffff #x-80000000
                                      #x7fffffffff #x-800000000000
                                      #x7fffffffffffff #x-8000000000000000
                                      #x7f #x8000 #x7fffff #x80000000
                                      #x7fffffffff #x800000000000
                                      #x7fffffffffffff #x8000000000000000
                                      #\a +inf.0 #\nul -0.0 -1))])
               a)))
        (pretty-print
          '(define $fsr-e-inst4
             (let-syntax ([a (lambda (x)
                               #`'#,(make-$fsr-e
                                      #x-80 #x7fff #x-800000 #x7fffffff
                                      #x-8000000000 #x7fffffffffff
                                      #x-80000000000000 #x7fffffffffffffff
                                      #x80 #x7fff #x800000 #x7fffffff
                                      #x8000000000 #x7fffffffffff
                                      #x80000000000000 #x7fffffffffffffff
                                      #\a +0.0 #\nul +inf.0 -1))])
               a))))
      'replace)
    #t)
  (begin
    (separate-compile "testfile")
    (load "testfile.so")
    #t)
  ($fsr-e? $fsr-e-inst1)
  ($fsr-e? $fsr-e-inst2)
  ($fsr-e? $fsr-e-inst3)
  ($fsr-e? $fsr-e-inst4)
  (equal?
    ($record->vector $fsr-e-inst1)
    ($record->vector
      (make-$fsr-e 0 -1 0 -1 0 -1 0 -1
        0 #xffff 0 #xffffffff 0 #xffffffffffff
        0 #xffffffffffffffff
        #\nul 3.14 #\x3bc -3.14 0)))
  (equal?
    ($record->vector $fsr-e-inst2)
    ($record->vector
      (make-$fsr-e -1 0 -1 0 -1 0 -1 0
        #xff 0 #xffffff 0 #xffffffffff 0
        #xffffffffffffff 0
        #\a -3.14 #\nul 3.14 -1)))
  (equal?
    ($record->vector $fsr-e-inst3)
    ($record->vector
      (make-$fsr-e
        #x7f #x-8000 #x7fffff #x-80000000
        #x7fffffffff #x-800000000000
        #x7fffffffffffff #x-8000000000000000
        #x7f #x8000 #x7fffff #x80000000
        #x7fffffffff #x800000000000
        #x7fffffffffffff #x8000000000000000
        #\a +inf.0 #\nul -0.0 -1)))
  (equal?
    ($record->vector $fsr-e-inst4)
    ($record->vector
      (make-$fsr-e
        #x-80 #x7fff #x-800000 #x7fffffff
        #x-8000000000 #x7fffffffffff
        #x-80000000000000 #x7fffffffffffffff
        #x80 #x7fff #x800000 #x7fffffff
        #x8000000000 #x7fffffffffff
        #x80000000000000 #x7fffffffffffffff
        #\a +0.0 #\nul +inf.0 -1)))
  )

(mat record?
  (eq? (record? 3) #f)
  (eq? (record? 'a) #f)
  (eq? (record? '#(1 2 3)) #f)
  (eq? (record? (make-record-type "foo" '())) #t)
  (eq? (record? ((record-constructor (make-record-type "foo" '())))) #t)
  (equal?
    (let ([rtd1 (make-record-type "foo" '())]
          [rtd2 (make-record-type "bar" '())])
      (let ([rtd3 (make-record-type rtd1 "xfoo" '())])
        (list (record? ((record-constructor rtd1)) rtd1)
              (record? ((record-constructor rtd1)) rtd2)
              (record? ((record-constructor rtd1)) rtd3)
              (record? ((record-constructor rtd3)) rtd1)
              (record? ((record-constructor rtd3)) rtd2)
              (record? ((record-constructor rtd3)) rtd3))))
    '(#t #f #f #t #f #t))
  (error? (record? 3 4))
  (error? (record? ((record-constructor (make-record-type "foo" '()))) 'a))
  (error? (record? ((record-constructor (make-record-type "foo" '()))) '#(1)))
  (let ()
    (define-record-type A)
    (define-record-type B (parent A))
    (define-record-type C (parent B))
    (define-record-type D (parent C) (sealed #t))
    (define-record-type E (parent C) (opaque #t))
    (define a (make-A))
    (define b (make-B))
    (define c (make-C))
    (define d (make-D))
    (define e (make-E))
    (define Atd (record-type-descriptor A))
    (define Btd (record-type-descriptor B))
    (define Ctd (record-type-descriptor C))
    (define Dtd (record-type-descriptor D))
    (define Etd (record-type-descriptor E))
    (and
      (equal?
        (list (record? 3) (record? a) (record? b) (record? c) (record? d) (record? e))
        '(#f #t #t #t #t #f))
      (equal?
        (let ()
          (import (rnrs))
          (list (record? 3) (record? a) (record? b) (record? c) (record? d) (record? e)))
        '(#f #t #t #t #t #f))
      (equal?
        (list (record? 3 Atd) (record? a Atd) (record? b Atd) (record? c Atd) (record? d Atd) (record? e Atd))
        '(#f #t #t #t #t #t))
      (equal?
        (list (record? 3 Btd) (record? a Btd) (record? b Btd) (record? c Btd) (record? d Btd) (record? e Btd))
        '(#f #f #t #t #t #t))
      (equal?
        (list (record? 3 Ctd) (record? a Ctd) (record? b Ctd) (record? c Ctd) (record? d Ctd) (record? e Ctd))
        '(#f #f #f #t #t #t))
      (equal?
        (list (record? 3 Dtd) (record? a Dtd) (record? b Dtd) (record? c Dtd) (record? d Dtd) (record? e Dtd))
        '(#f #f #f #f #t #f))
      (equal?
        (list (record? 3 Etd) (record? a Etd) (record? b Etd) (record? c Etd) (record? d Etd) (record? e Etd))
        '(#f #f #f #f #f #t))
      (equal?
        (let ([record? #%$sealed-record?])
          (list (record? 3 Dtd) (record? a Dtd) (record? b Dtd) (record? c Dtd) (record? d Dtd) (record? e Dtd)))
        '(#f #f #f #f #t #f))))
)

(mat record-type-mismatch
  (begin
    (define-record-type flotsam
      (nongenerative #{flotsam flotsam})
      (fields x y))
    #t)
  (record-type-descriptor?
    (make-record-type '#{flotsam flotsam} '((immutable x) (immutable y))))
  (error? ; different parent
    (begin
      (define-record-type pflotsam (nongenerative pflotsam))
      (define-record-type flotsam
        (nongenerative #{flotsam flotsam})
        (parent pflotsam)
        (fields x y))))
  (error? ; different fields
    (define-record-type flotsam
      (nongenerative #{flotsam flotsam})
      (fields x y z)))
  (error? ; different fields
    (make-record-type '#{flotsam flotsam} '((int x) y)))
  (error? ; different mutability
    (define-record-type flotsam
      (nongenerative #{flotsam flotsam})
      (fields (mutable x) y)))
  (error? ; different flags
    (define-record-type flotsam
      (nongenerative #{flotsam flotsam})
      (sealed #t)
      (fields x y)))
  (error? ; different flags
    (define-record-type flotsam
      (nongenerative #{flotsam flotsam})
      (opaque #t)
      (fields x y)))
)

(mat r6rs-records-procedural
  ((lambda (x)
     (and (list? x)
          (record? (car x))
          (equal?
            (cdr x)
            '(765 45 25 #t #t #f #f #t #t #f foo bar #1(x) #2(y z) #f #t
                  (#t #f) (#f #f) (#f #t) #t pluto))))
    (let ()
      (define prtd
        (make-record-type-descriptor 'foo #f #f #f #f
          '#((mutable x))))
      (define rtd
        (make-record-type-descriptor 'bar prtd 'pluto #t #f
          '#((mutable y) (immutable z))))
      (define rcd (make-record-constructor-descriptor rtd #f #f))
      (define rc (r6rs:record-constructor rcd))
      (define foo-x (record-accessor prtd 0))
      (define foo-x! (record-mutator prtd 0))
      (define bar-y (record-accessor rtd 0))
      (define bar-y! (record-mutator rtd 0))
      (define bar-z (record-accessor rtd 1))
      (define x (rc 17 20 25))
      (bar-y! x (+ (bar-y x) (bar-z x)))
      (foo-x! x (* (bar-y x) (foo-x x)))
      (list x (foo-x x) (bar-y x) (bar-z x)
        (record-type-descriptor? rtd)
        (record-constructor-descriptor? rcd)
        (record-type-descriptor? rcd)
        (record-constructor-descriptor? rtd)
        (record-field-mutable? prtd 0)
        (record-field-mutable? rtd 0)
        (record-field-mutable? rtd 1)
        (record-type-name prtd)
        (record-type-name rtd)
        (record-type-field-names prtd)
        (record-type-field-names rtd)
        (eq? (record-rtd x) prtd)
        (eq? (record-rtd x) rtd)
        (list (record-type-generative? prtd) (record-type-generative? rtd))
        (list (record-type-opaque? prtd) (record-type-opaque? rtd))
        (list (record-type-sealed? prtd) (record-type-sealed? rtd))
        (gensym? (record-type-uid prtd))
        (record-type-uid rtd))))

  (equal?
    (parameterize ([current-output-port (open-output-string)])
      (define a-rtd (make-record-type-descriptor 'a #f #f #f #f
                      '#((mutable x))))
      (define b-rtd (make-record-type-descriptor 'b a-rtd #f #f #f
                      '#((immutable x) (mutable y))))
      (define a? (record-predicate a-rtd))
      (define b? (record-predicate b-rtd))
      (define a-x (record-accessor a-rtd 0))
      (define a-x! (record-mutator a-rtd 0))
      (define b-x (record-accessor b-rtd 0))
      (define b-y (record-accessor b-rtd 1))
      (define b-y! (record-mutator b-rtd 1))
      (define (a->list b)
        (if (b? b)
            (list (a-x b) (b-x b) (b-y b))
            (list (a-x b))))
      (define a-rcd0 (make-record-constructor-descriptor a-rtd #f #f))
      (define b-rcd0 (make-record-constructor-descriptor b-rtd #f #f))
      #;(define make-a0 (r6rs:record-constructor a-rcd0))
      #;(define make-b0 (r6rs:record-constructor b-rcd0))
      (define make-a0 (record-constructor a-rcd0)) ; should handle rcd too
      (define make-b0 (record-constructor b-rcd0)) ; should handle rcd too
      (define b-rcd1 (make-record-constructor-descriptor b-rtd a-rcd0 #f))
      (define make-b1 (r6rs:record-constructor b-rcd1))
      (define a-rcd2
        (make-record-constructor-descriptor a-rtd #f
          (lambda (p)
            (lambda (x y)
              (let ([r (p (- x y))])
                (printf "make-a2: ~s\n" (a->list r))
                (a-x r)
                r)))))
      (define make-a2 (r6rs:record-constructor a-rcd2))
      (let ([ls (map a->list (list
                               (make-a0 3)
                               (make-b0 4 5 6)
                               (make-b1 7 8 9)
                               (make-a2 10 11)))])
        (cons (get-output-string (current-output-port)) ls)))
    '("make-a2: (-1)\n" (3) (4 5 6) (7 8 9) (-1)))

  (equal?
    (parameterize ([current-output-port (open-output-string)])
      (define a-rtd (make-record-type-descriptor 'a #f #f #f #f
                      '#((mutable x))))
      (define a? (record-predicate a-rtd))
      (define a-x (record-accessor a-rtd 0))
      (define (a->list b) (list (a-x b)))
      (define-syntax echo
        (syntax-rules ()
          [(_ s e) (begin (printf "~a in\n" s)
                     (let ([x e])
                       (printf "~a out: ~s\n" s (record? x))
                       x))]))
      (define a-rcd
        (make-record-constructor-descriptor a-rtd #f
          (lambda (m) (lambda (q t) (echo "A" (m (* q t)))))))
      (define make-a (r6rs:record-constructor a-rcd))
      (let ([ls (map a->list (list (make-a 3 4)))])
        (cons (get-output-string (current-output-port)) ls)))
    '("A in\nA out: #t\n" (12)))

  (equal?
    (parameterize ([current-output-port (open-output-string)])
      (define a-rtd (make-record-type-descriptor 'a #f #f #f #f
                      '#((mutable x))))
      (define b-rtd (make-record-type-descriptor 'b a-rtd #f #f #f
                      '#((immutable x) (mutable y))))
      (define a? (record-predicate a-rtd))
      (define b? (record-predicate b-rtd))
      (define a-x (record-accessor a-rtd 0))
      (define a-x! (record-mutator a-rtd 0))
      (define b-x (record-accessor b-rtd 0))
      (define b-y (record-accessor b-rtd 1))
      (define b-y! (record-mutator b-rtd 1))
      (define (a->list b)
        (if (b? b)
            (list (a-x b) (b-x b) (b-y b))
            (list (a-x b))))
      (define-syntax echo
        (syntax-rules ()
          [(_ s e) (begin (printf "~a in\n" s)
                     (let ([x e])
                       (printf "~a out: ~s\n" s (record? x))
                       x))]))
      (define a-rcd
        (make-record-constructor-descriptor a-rtd #f
          (lambda (m) (lambda (q) (echo "A" (m (* q q)))))))
      (define b-rcd
        (make-record-constructor-descriptor b-rtd a-rcd
          (lambda (m) (lambda (q) (echo "B" ((m q) (- q) (/ q)))))))
      (define make-b (r6rs:record-constructor b-rcd))
      (let ([ls (map a->list (list (make-b 3)))])
        (cons (get-output-string (current-output-port)) ls)))
    '("B in\nA in\nA out: #t\nB out: #t\n" (9 -3 1/3)))

  (equal?
    (parameterize ([current-output-port (open-output-string)])
      (define a-rtd (make-record-type-descriptor 'a #f #f #f #f
                      '#((mutable x))))
      (define b-rtd (make-record-type-descriptor 'b a-rtd #f #f #f
                      '#((immutable x) (mutable y))))
      (define c-rtd (make-record-type-descriptor 'c b-rtd #f #f #f
                      '#((immutable z) (mutable w))))
      (define a? (record-predicate a-rtd))
      (define b? (record-predicate b-rtd))
      (define c? (record-predicate c-rtd))
      (define a-x (record-accessor a-rtd 0))
      (define a-x! (record-mutator a-rtd 0))
      (define b-x (record-accessor b-rtd 0))
      (define b-y (record-accessor b-rtd 1))
      (define b-y! (record-mutator b-rtd 1))
      (define c-z (record-accessor c-rtd 0))
      (define c-w (record-accessor c-rtd 1))
      (define c-w! (record-mutator c-rtd 1))
      (define (a->list b)
        (if (c? b)
            (list (a-x b) (b-x b) (b-y b) (c-z b) (c-w b))
            (if (b? b)
                (list (a-x b) (b-x b) (b-y b))
                (list (a-x b)))))
      (define-syntax echo
        (syntax-rules ()
          [(_ s e) (begin (printf "~a in\n" s)
                     (let ([x e])
                       (printf "~a out: ~s\n" s (record? x))
                       x))]))
      (define a-rcd
        (make-record-constructor-descriptor a-rtd #f
          (lambda (m) (lambda (q) (echo "A" (m (* q q)))))))
      (define b-rcd
        (make-record-constructor-descriptor b-rtd a-rcd
          (lambda (m) (lambda (q) (echo "B" ((m q) (- q) (/ q)))))))
      (define c-rcd
        (make-record-constructor-descriptor c-rtd b-rcd
          (lambda (m)
            (lambda (q t)
              (echo "C" ((m (+ q t)) (* q t) (cons q t)))))))
      (define make-c (r6rs:record-constructor c-rcd))
      (let ([ls (map a->list (list (make-c 3 4)))])
        (cons (get-output-string (current-output-port)) ls)))
    '("C in\nB in\nA in\nA out: #t\nB out: #t\nC out: #t\n"
       (49 -7 1/7 12 (3 . 4))))
  (error? ; rtd/rcd mismatch
    (let ()
      (define-syntax rtd1 (lambda (x) #`'#,(make-record-type "foo" '(x))))
      (define-syntax rtd2 (lambda (x) #`'#,(make-record-type rtd1 "bar" '(y))))
      (define-syntax rtd3 (lambda (x) #`'#,(make-record-type "foo2" '(a b))))
      (define-syntax rtd4 (lambda (x) #`'#,(make-record-type rtd3 "bar2" '(c d))))
      (define rcd1
        (make-record-constructor-descriptor rtd1 #f
          (lambda (n) (lambda (q) (n (* q q))))))
      (define rcd3
        (make-record-constructor-descriptor rtd3 rcd1
          (lambda (p) (lambda (t u v) ((p t u) v 0)))))
      (define cons3 (r6rs:record-constructor rcd3))
      (cons3 1 2 3)))

 ; make sure appropriate error checking is done for protocols
  (error? ; not a procedure (parent protocol)
    (let ([pprot (cons 'ugly 'ducking)]
          [cprot (lambda (p) (lambda (x y) ((p x 0 17) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; not a procedure (child protocol)
    (let ([pprot (lambda (n) n)]
          [cprot 'flimflam])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; not a procedure (returned from parent protocol)
    (let ([pprot (lambda (n) 'not-a-procedure)]
          [cprot (lambda (p) (lambda (x y) ((p x 17) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; not a procedure (returned from child protocol)
    (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
          [cprot (lambda (p) 'spam)])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; wrong number of arguments (to parent protocol)
    (let ([pprot (lambda (n) n)]
          [cprot (lambda (p) (lambda (x y) ((p x 0 17) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; wrong number of arguments (to parent protocol)
    (let ([pprot (lambda (n) n)]
          [cprot (lambda (p) (lambda (x y) ((p x) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; wrong number of arguments (to parent protocol)
    (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
          [cprot (lambda (p) (lambda (x y) ((p x 17 'xtra) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; wrong number of arguments (to parent protocol)
    (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
          [cprot (lambda (p) (lambda (x y) ((p x) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; wrong number of arguments (to child constructor)
    (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
          [cprot (lambda (p) (lambda (x y) ((p x 17) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1))))
  (error? ; wrong number of arguments (to child constructor)
    (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
          [cprot (lambda (p) (lambda (x y) ((p x 17) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2 3))))
  (error? ; wrong number of arguments (to parent "new" procedure)
    (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w "what?")))]
          [cprot (lambda (p) (lambda (x y) ((p x 17) y)))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))
  (error? ; wrong number of arguments (to child "new" procedure)
    (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
          [cprot (lambda (p) (lambda (x y) ((p x 17) y '#(oops))))])
      (define prtd (make-record-type "parent" '(x y)))
      (define crtd (make-record-type prtd "child" '(z)))
      (define prcd (make-record-constructor-descriptor prtd #f pprot))
      (define crcd (make-record-constructor-descriptor crtd prcd cprot))
      (define pcons (r6rs:record-constructor prcd))
      (define ccons (r6rs:record-constructor crcd))
      (list (pcons 1 2) (ccons 1 2))))

 ; make sure we can use modifiers and types as field names
  (equal?
    (let ()
      (define foo (make-record-type-descriptor 'umph #f #f #f #f '#((mutable mutable) (immutable int) (immutable integer-32))))
      (let ([x ((r6rs:record-constructor (make-record-constructor-descriptor foo #f #f)) 3 4 5)])
        ((record-mutator foo 0) x 75)
        (list ($record->vector x)
              ((record-accessor foo 0) x)
              ((record-accessor foo 1) x)
              ((record-accessor foo 2) x))))
    '(#(umph 75 4 5) 75 4 5))

 ; optimization tests---observe with expand/optimize
  (equal?
    (map $record->vector
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd (make-record-constructor-descriptor prtd #f #f))
        (define crcd (make-record-constructor-descriptor crtd #f #f))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons (r6rs:record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2 3))))
    '(#(parent 1 2) #(child 1 2 3)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd (make-record-constructor-descriptor prtd #f #f))
            (define crcd (make-record-constructor-descriptor crtd #f #f))
            (define pcons (r6rs:record-constructor prcd))
            (define ccons (r6rs:record-constructor crcd))
            (list (pcons 1 2) (ccons 1 2 3)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 2 3)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd (make-record-constructor-descriptor prtd #f #f))
            (define crcd (make-record-constructor-descriptor crtd #f #f))
            (define pcons (r6rs:record-constructor prcd))
            (define ccons (r6rs:record-constructor crcd))
            (list (pcons 1 2) (ccons 1 2 3)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 2 3)))))
 ; same as set above except with r6rs:record-constructor
 ; replaced by record:constructor, which should handle rcds
  (equal?
    (map $record->vector
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd (make-record-constructor-descriptor prtd #f #f))
        (define crcd (make-record-constructor-descriptor crtd #f #f))
        (define pcons (record-constructor prcd))
        (define ccons (record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2 3))))
    '(#(parent 1 2) #(child 1 2 3)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd (make-record-constructor-descriptor prtd #f #f))
            (define crcd (make-record-constructor-descriptor crtd #f #f))
            (define pcons (record-constructor prcd))
            (define ccons (record-constructor crcd))
            (list (pcons 1 2) (ccons 1 2 3)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 2 3)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd (make-record-constructor-descriptor prtd #f #f))
            (define crcd (make-record-constructor-descriptor crtd #f #f))
            (define pcons (record-constructor prcd))
            (define ccons (record-constructor crcd))
            (list (pcons 1 2) (ccons 1 2 3)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 2 3)))))
  (equal?
    (map $record->vector
      ; same thing except supplying prcd in place of #f, which should
      ; result in the same residual code
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd (make-record-constructor-descriptor prtd #f #f))
        (define crcd (make-record-constructor-descriptor crtd prcd #f))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons (r6rs:record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2 3))))
    '(#(parent 1 2) #(child 1 2 3)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f #f))
           (define crcd
             (make-record-constructor-descriptor crtd prcd #f))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2 3)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 2 3)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f #f))
           (define crcd
             (make-record-constructor-descriptor crtd prcd #f))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2 3)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 2 3)))))
  (equal?
    (map $record->vector
     ; test with variables bound to protocol lambda expressions
      (let ([pprot (lambda (n) n)]
            [cprot (lambda (p) (lambda (x y) ((p x 0) y)))])
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd (make-record-constructor-descriptor prtd #f pprot))
        (define crcd (make-record-constructor-descriptor crtd prcd cprot))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons (r6rs:record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 1 2) #(child 1 0 2)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([pprot (lambda (n) n)]
               [cprot (lambda (p) (lambda (x y) ((p x 0) y)))])
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f pprot))
           (define crcd
             (make-record-constructor-descriptor crtd prcd cprot))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 0 2)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([pprot (lambda (n) n)]
               [cprot (lambda (p) (lambda (x y) ((p x 0) y)))])
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f pprot))
           (define crcd
             (make-record-constructor-descriptor crtd prcd cprot))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 0 2)))))
  (begin (define $global 'worldwide) #t)
  (equal?
    (map $record->vector
     ; same but with a global variable in place of the constant 0
      (let ([pprot (lambda (n) n)]
            [cprot (lambda (p) (lambda (x y) ((p x $global) y)))])
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd (make-record-constructor-descriptor prtd #f pprot))
        (define crcd (make-record-constructor-descriptor crtd prcd cprot))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons (r6rs:record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 1 2) #(child 1 worldwide 2)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([pprot (lambda (n) n)]
               [cprot (lambda (p) (lambda (x y) ((p x $global) y)))])
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f pprot))
           (define crcd
             (make-record-constructor-descriptor crtd prcd cprot))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 $global 2)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([pprot (lambda (n) n)]
               [cprot (lambda (p) (lambda (x y) ((p x $global) y)))])
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f pprot))
           (define crcd
             (make-record-constructor-descriptor crtd prcd cprot))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
           (#3%list
             (#3%$record prtd 1 2)
             (#3%$record crtd 1 $global 2)))))
  (equal?
    (map $record->vector
     ; same but with a outer lexical variable in place of the constant 0
      (let ([lex $global])
        (let ([pprot (lambda (n) n)]
              [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
          (define prtd (make-record-type "parent" '(x y)))
          (define crtd (make-record-type prtd "child" '(z)))
          (define prcd (make-record-constructor-descriptor prtd #f pprot))
          (define crcd (make-record-constructor-descriptor crtd prcd cprot))
          (define pcons (r6rs:record-constructor prcd))
          (define ccons (r6rs:record-constructor crcd))
          (list (pcons 1 2) (ccons 1 2)))))
    '(#(parent 1 2) #(child 1 worldwide 2)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([lex $global])
           (let ([pprot (lambda (n) n)]
                 [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
             (define prtd (make-record-type "parent" '(x y)))
             (define crtd (make-record-type prtd "child" '(z)))
             (define prcd
               (make-record-constructor-descriptor prtd #f pprot))
             (define crcd
               (make-record-constructor-descriptor crtd prcd cprot))
             (define pcons (r6rs:record-constructor prcd))
             (define ccons (r6rs:record-constructor crcd))
             (list (pcons 1 2) (ccons 1 2))))))
    '(let ([lex $global])
       (let ([prtd (#2%make-record-type "parent" '(x y))])
         (let ([crtd (#2%make-record-type prtd "child" '(z))])
           (#2%list
             (#3%$record prtd 1 2)
             (#3%$record crtd 1 lex 2))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([lex $global])
           (let ([pprot (lambda (n) n)]
                 [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
             (define prtd (make-record-type "parent" '(x y)))
             (define crtd (make-record-type prtd "child" '(z)))
             (define prcd
               (make-record-constructor-descriptor prtd #f pprot))
             (define crcd
               (make-record-constructor-descriptor crtd prcd cprot))
             (define pcons (r6rs:record-constructor prcd))
             (define ccons (r6rs:record-constructor crcd))
             (list (pcons 1 2) (ccons 1 2))))))
    '(let ([lex $global])
       (let ([prtd (#3%make-record-type "parent" '(x y))])
         (let ([crtd (#3%make-record-type prtd "child" '(z))])
           (#3%list
             (#3%$record prtd 1 2)
             (#3%$record crtd 1 lex 2))))))
  (equal?
    (map $record->vector
     ; same but slightly more complicated parent protocol
      (let ([lex $global])
        (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
              [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
          (define prtd (make-record-type "parent" '(x y)))
          (define crtd (make-record-type prtd "child" '(z)))
          (define prcd (make-record-constructor-descriptor prtd #f pprot))
          (define crcd (make-record-constructor-descriptor crtd prcd cprot))
          (define pcons (r6rs:record-constructor prcd))
          (define ccons (r6rs:record-constructor crcd))
          (list (pcons 1 2) (ccons 1 2)))))
    '(#(parent 8 2) #(child 8 worldwide 2)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([lex $global])
           (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
                 [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
             (define prtd (make-record-type "parent" '(x y)))
             (define crtd (make-record-type prtd "child" '(z)))
             (define prcd
               (make-record-constructor-descriptor prtd #f pprot))
             (define crcd
               (make-record-constructor-descriptor crtd prcd cprot))
             (define pcons (r6rs:record-constructor prcd))
             (define ccons (r6rs:record-constructor crcd))
             (list (pcons 1 2) (ccons 1 2))))))
    '(let ([lex $global])
       (let ([prtd (#2%make-record-type "parent" '(x y))])
         (let ([crtd (#2%make-record-type prtd "child" '(z))])
           (#2%list
             (#3%$record prtd 8 2)
             (#3%$record crtd 8 lex 2))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([lex $global])
           (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) w)))]
                 [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
             (define prtd (make-record-type "parent" '(x y)))
             (define crtd (make-record-type prtd "child" '(z)))
             (define prcd
               (make-record-constructor-descriptor prtd #f pprot))
             (define crcd
               (make-record-constructor-descriptor crtd prcd cprot))
             (define pcons (r6rs:record-constructor prcd))
             (define ccons (r6rs:record-constructor crcd))
             (list (pcons 1 2) (ccons 1 2))))))
    '(let ([lex $global])
       (let ([prtd (#3%make-record-type "parent" '(x y))])
         (let ([crtd (#3%make-record-type prtd "child" '(z))])
           (#3%list
             (#3%$record prtd 8 2)
             (#3%$record crtd 8 lex 2))))))
  (equal?
    (map $record->vector
     ; same but ignore one of the parent args
      (let ([lex $global])
        (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) 53)))]
              [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
          (define prtd (make-record-type "parent" '(x y)))
          (define crtd (make-record-type prtd "child" '(z)))
          (define prcd (make-record-constructor-descriptor prtd #f pprot))
          (define crcd (make-record-constructor-descriptor crtd prcd cprot))
          (define pcons (r6rs:record-constructor prcd))
          (define ccons (r6rs:record-constructor crcd))
          (list (pcons 1 2) (ccons 1 2)))))
    '(#(parent 8 53) #(child 8 53 2)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([lex $global])
           (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) 53)))]
                 [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
             (define prtd (make-record-type "parent" '(x y)))
             (define crtd (make-record-type prtd "child" '(z)))
             (define prcd
               (make-record-constructor-descriptor prtd #f pprot))
             (define crcd
               (make-record-constructor-descriptor crtd prcd cprot))
             (define pcons (r6rs:record-constructor prcd))
             (define ccons (r6rs:record-constructor crcd))
             (list (pcons 1 2) (ccons 1 2))))))
    '(begin $global
       (let ([prtd (#2%make-record-type "parent" '(x y))])
         (let ([crtd (#2%make-record-type prtd "child" '(z))])
           (#2%list
             (#3%$record prtd 8 53)
             (#3%$record crtd 8 53 2))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ([lex $global])
           (let ([pprot (lambda (n) (lambda (z w) (n (+ z 7) 53)))]
                 [cprot (lambda (p) (lambda (x y) ((p x lex) y)))])
             (define prtd (make-record-type "parent" '(x y)))
             (define crtd (make-record-type prtd "child" '(z)))
             (define prcd
               (make-record-constructor-descriptor prtd #f pprot))
             (define crcd
               (make-record-constructor-descriptor crtd prcd cprot))
             (define pcons (r6rs:record-constructor prcd))
             (define ccons (r6rs:record-constructor crcd))
             (list (pcons 1 2) (ccons 1 2))))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 8 53)
           (#3%$record crtd 8 53 2)))))
  (equal?
    (map $record->vector
     ; same thing except pprot and cprot lambda expressions
     ; appear directly in the calls to m-r-c-d
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd
          (make-record-constructor-descriptor prtd #f
            (lambda (n) n)))
        (define crcd
          (make-record-constructor-descriptor crtd prcd
            (lambda (p) (lambda (x y) ((p x 0) y)))))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons (r6rs:record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 1 2) #(child 1 0 2)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f (lambda (n) n)))
           (define crcd
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x 0) y)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 0 2)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f (lambda (n) n)))
           (define crcd
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x 0) y)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 1 2)
           (#3%$record crtd 1 0 2)))))
  (equal?
    (map $record->vector
     ; same thing except with slightly more complicated parent protocol
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd
          (make-record-constructor-descriptor prtd #f
            (lambda (n) (lambda (z w) (n (+ z 7) w)))))
        (define crcd
          (make-record-constructor-descriptor crtd prcd
            (lambda (p) (lambda (x y) ((p x y) 0)))))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons (r6rs:record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 2) #(child 8 2 0)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor
               prtd
               #f
               (lambda (n) (lambda (z w) (n (+ z 7) w)))))
           (define crcd
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x y) 0)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor
               prtd
               #f
               (lambda (n) (lambda (z w) (n (+ z 7) w)))))
           (define crcd
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x y) 0)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equal?
    (map $record->vector
     ; same thing but ignore one of the parent args
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd
          (make-record-constructor-descriptor prtd #f
            (lambda (n) (lambda (z w) (n (+ z 7) 53)))))
        (define crcd
          (make-record-constructor-descriptor crtd prcd
            (lambda (p) (lambda (x y) ((p x y) 0)))))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons (r6rs:record-constructor crcd))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 53) #(child 8 53 0)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor
               prtd
               #f
               (lambda (n) (lambda (z w) (n (+ z 7) 53)))))
           (define crcd
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x y) 0)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 8 53)
           (#3%$record crtd 8 53 0)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor
               prtd
               #f
               (lambda (n) (lambda (z w) (n (+ z 7) 53)))))
           (define crcd
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x y) 0)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 8 53)
           (#3%$record crtd 8 53 0)))))
  (equal?
    (map $record->vector
     ; same thing except don't give a name to the child rcd
     ; surprisingly, this folds up because the call to r6rs:record-constructor
     ; (as with any primitive call) gets pushed into the letrec produced by
     ; make-record-constructor-descriptor
     ; > (print-gensym #f)
     ; > (new-cafe expand/optimize)
     ; >> (#%r6rs:record-constructor (letrec ((x (lambda (n) n))) (foo x)))
     ; (letrec ([x (lambda (n) n)]) (#2%r6rs:record-constructor (foo x)))
     ; >> (#%car (letrec ((x (lambda (n) n))) (foo x)))
     ; (letrec ([x (lambda (n) n)]) (#2%car (foo x)))
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define prcd
          (make-record-constructor-descriptor prtd #f
            (lambda (n) (lambda (z w) (n (+ z 7) w)))))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons
          (r6rs:record-constructor
            (make-record-constructor-descriptor crtd prcd
              (lambda (p) (lambda (x y) ((p x y) 0))))))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 2) #(child 8 2 0)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor
               prtd
               #f
               (lambda (n) (lambda (z w) (n (+ z 7) w)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons
             (r6rs:record-constructor
               (make-record-constructor-descriptor
                 crtd
                 prcd
                 (lambda (p) (lambda (x y) ((p x y) 0))))))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor
               prtd
               #f
               (lambda (n) (lambda (z w) (n (+ z 7) w)))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons
             (r6rs:record-constructor
               (make-record-constructor-descriptor
                 crtd
                 prcd
                 (lambda (p) (lambda (x y) ((p x y) 0))))))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equal?
    (map $record->vector
     ; same thing except give pprot a name
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define pprot (lambda (n) (lambda (z w) (n (+ z 7) w))))
        (define prcd (make-record-constructor-descriptor prtd #f pprot))
        (define pcons (r6rs:record-constructor prcd))
        (define ccons
          (r6rs:record-constructor
            (make-record-constructor-descriptor crtd prcd
              (lambda (p) (lambda (x y) ((p x y) 0))))))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 2) #(child 8 2 0)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define pprot (lambda (n) (lambda (z w) (n (+ z 7) w))))
           (define prcd
             (make-record-constructor-descriptor prtd #f pprot))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons
             (r6rs:record-constructor
               (make-record-constructor-descriptor
                 crtd
                 prcd
                 (lambda (p) (lambda (x y) ((p x y) 0))))))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define pprot (lambda (n) (lambda (z w) (n (+ z 7) w))))
           (define prcd
             (make-record-constructor-descriptor prtd #f pprot))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons
             (r6rs:record-constructor
               (make-record-constructor-descriptor
                 crtd
                 prcd
                 (lambda (p) (lambda (x y) ((p x y) 0))))))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equal?
    (map $record->vector
     ; push our luck: don't give a name to parent rcd either.
     ; this one doesn't fold up.  to fix it, we'd need to (a)
     ; pull the inner m-r-c-d call and outer protocol expr into a
     ; let or letrec wrapping the outer m-r-c-d call, and (b)
     ; pull the bindings for both outside of the r6rs:r-c call ...
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define pcons
          (r6rs:record-constructor
            (make-record-constructor-descriptor prtd #f
              (lambda (n) (lambda (z w) (n (+ z 7) w))))))
        (define ccons
          (r6rs:record-constructor
            (make-record-constructor-descriptor crtd
              (make-record-constructor-descriptor prtd #f
                (lambda (n) (lambda (z w) (n (+ z 7) w))))
              (lambda (p) (lambda (x y) ((p x y) 0))))))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 2) #(child 8 2 0)))
  (equal?
    (map $record->vector
     ; ... like this (at optimize-level 3, anyway)
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define pcons
          (r6rs:record-constructor
            (make-record-constructor-descriptor prtd #f
              (lambda (n) (lambda (z w) (n (+ z 7) w))))))
        (define ccons
          (let ([prcd (make-record-constructor-descriptor prtd #f
                        (lambda (n) (lambda (z w) (n (+ z 7) w))))]
                [cprot (lambda (p) (lambda (x y) ((p x y) 0)))])
            (r6rs:record-constructor
              (make-record-constructor-descriptor crtd prcd cprot))))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 2) #(child 8 2 0)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define pcons
             (r6rs:record-constructor
               (make-record-constructor-descriptor
                 prtd
                 #f
                 (lambda (n) (lambda (z w) (n (+ z 7) w))))))
           (define ccons
             (let ([prcd (make-record-constructor-descriptor
                           prtd
                           #f
                           (lambda (n) (lambda (z w) (n (+ z 7) w))))]
                   [cprot (lambda (p) (lambda (x y) ((p x y) 0)))])
               (r6rs:record-constructor
                 (make-record-constructor-descriptor crtd prcd cprot))))
           (list (pcons 1 2) (ccons 1 2)))))
   ; this is now as good as it gets at optimize-level 2
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define pcons
             (r6rs:record-constructor
               (make-record-constructor-descriptor
                 prtd
                 #f
                 (lambda (n) (lambda (z w) (n (+ z 7) w))))))
           (define ccons
             (let ([prcd (make-record-constructor-descriptor
                           prtd
                           #f
                           (lambda (n) (lambda (z w) (n (+ z 7) w))))]
                   [cprot (lambda (p) (lambda (x y) ((p x y) 0)))])
               (r6rs:record-constructor
                 (make-record-constructor-descriptor crtd prcd cprot))))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 0)))))
  (equal?
    (map $record->vector
     ; ... this isn't good enough
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define pcons
          (r6rs:record-constructor
            (make-record-constructor-descriptor prtd #f
              (lambda (n) (lambda (z w) (n (+ z 7) w))))))
        (define ccons
          (let ([tmp (make-record-constructor-descriptor crtd
                       (make-record-constructor-descriptor prtd #f
                         (lambda (n) (lambda (z w) (n (+ z 7) w))))
                       (lambda (p) (lambda (x y) ((p x y) 0))))])
            (r6rs:record-constructor tmp)))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 2) #(child 8 2 0)))
  (equal?
    (map $record->vector
     ; try some with inlining
      (let ()
        (define prtd (make-record-type "parent" '(x y)))
        (define crtd (make-record-type prtd "child" '(z)))
        (define (make-prcd f) (make-record-constructor-descriptor prtd #f f))
        (define prcd (make-prcd (lambda (n) (lambda (z w) (n (+ z 7) w)))))
        (define (make-crcd z)
          (make-record-constructor-descriptor crtd prcd
            (lambda (p) (lambda (x y) ((p x y) z)))))
        (define crcd (make-crcd -17))
        (define (make-pcons) (r6rs:record-constructor prcd))
        (define pcons (make-pcons))
        (define (make-ccons x) (r6rs:record-constructor x))
        (define ccons (make-ccons crcd))
        (list (pcons 1 2) (ccons 1 2))))
    '(#(parent 8 2) #(child 8 2 -17)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define (make-prcd f)
             (make-record-constructor-descriptor prtd #f f))
           (define prcd
             (make-prcd (lambda (n) (lambda (z w) (n (+ z 7) w)))))
           (define (make-crcd z)
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x y) z)))))
           (define crcd (make-crcd -17))
           (define (make-pcons) (r6rs:record-constructor prcd))
           (define pcons (make-pcons))
           (define (make-ccons x) (r6rs:record-constructor x))
           (define ccons (make-ccons crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#2%make-record-type "parent" '(x y))])
       (let ([crtd (#2%make-record-type prtd "child" '(z))])
         (#2%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 -17)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define (make-prcd f)
             (make-record-constructor-descriptor prtd #f f))
           (define prcd
             (make-prcd (lambda (n) (lambda (z w) (n (+ z 7) w)))))
           (define (make-crcd z)
             (make-record-constructor-descriptor
               crtd
               prcd
               (lambda (p) (lambda (x y) ((p x y) z)))))
           (define crcd (make-crcd -17))
           (define (make-pcons) (r6rs:record-constructor prcd))
           (define pcons (make-pcons))
           (define (make-ccons x) (r6rs:record-constructor x))
           (define ccons (make-ccons crcd))
           (list (pcons 1 2) (ccons 1 2)))))
    '(let ([prtd (#3%make-record-type "parent" '(x y))])
       (let ([crtd (#3%make-record-type prtd "child" '(z))])
         (#3%list
           (#3%$record prtd 8 2)
           (#3%$record crtd 8 2 -17)))))
  (equal?
    (parameterize ([print-vector-length #f])
      (with-output-to-string
       ; more elaborate test with side effects
        (lambda ()
          (define prtd (make-record-type "parent" '(x y)))
          (define crtd (make-record-type prtd "child" '(z)))
          (define prcd
            (make-record-constructor-descriptor prtd #f
              (rec pprot
                (lambda (new)
                  (lambda (x n m)
                    (let ([r (new x (+ n m))])
                      (pretty-print `(parent ,($record->vector r)))
                      r))))))
          (define crcd
            (make-record-constructor-descriptor crtd prcd
              (rec cprot
                (lambda (p)
                  (lambda (z x n m)
                    (let ([r ((p x n m) z)])
                      (pretty-print `(child ,($record->vector r)))
                      r))))))
          (define pcons (r6rs:record-constructor prcd))
          (define ccons (r6rs:record-constructor crcd))
          (pretty-print ($record->vector (pcons 1 2 3)))
          (pretty-print ($record->vector (ccons 1 2 3 4))))))
    "(parent #(parent 1 5))\n#(parent 1 5)\n(parent #(child 2 7 1))\n(child #(child 2 7 1))\n#(child 2 7 1)\n")
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f
               (rec pprot
                 (lambda (new)
                   (lambda (x n m)
                     (let ([r (new x (+ n m))])
                       (pretty-print `(parent ,($record->vector r)))
                       r))))))
           (define crcd
             (make-record-constructor-descriptor crtd prcd
               (rec cprot
                 (lambda (p)
                   (lambda (z x n m)
                     (let ([r ((p x n m) z)])
                       (pretty-print `(child ,($record->vector r)))
                       r))))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (pretty-print ($record->vector (pcons 1 2 3)))
           (pretty-print ($record->vector (ccons 1 2 3 4))))))
    '(lambda ()
       (let ([prtd (#2%make-record-type "parent" '(x y))])
         (let ([crtd (#2%make-record-type prtd "child" '(z))])
           (#2%pretty-print
             ($record->vector
               (let ([r (#3%$record prtd 1 5)])
                 (#2%pretty-print (#2%list 'parent ($record->vector r)))
                 r)))
           (#2%pretty-print
             ($record->vector
               (let ([r (let ([r (#3%$record crtd 2 7 1)])
                          (#2%pretty-print (#2%list 'parent ($record->vector r)))
                          r)])
                 (#2%pretty-print (#2%list 'child ($record->vector r)))
                 r)))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define prcd
             (make-record-constructor-descriptor prtd #f
               (rec pprot
                 (lambda (new)
                   (lambda (x n m)
                     (let ([r (new x (+ n m))])
                       (pretty-print `(parent ,($record->vector r)))
                       r))))))
           (define crcd
             (make-record-constructor-descriptor crtd prcd
               (rec cprot
                 (lambda (p)
                   (lambda (z x n m)
                     (let ([r ((p x n m) z)])
                       (pretty-print `(child ,($record->vector r)))
                       r))))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (pretty-print ($record->vector (pcons 1 2 3)))
           (pretty-print ($record->vector (ccons 1 2 3 4))))))
    '(lambda ()
       (let ([prtd (#3%make-record-type "parent" '(x y))])
         (let ([crtd (#3%make-record-type prtd "child" '(z))])
           (#3%pretty-print
             ($record->vector
               (let ([r (#3%$record prtd 1 5)])
                 (#3%pretty-print (#3%list 'parent ($record->vector r)))
                 r)))
           (#3%pretty-print
             ($record->vector
               (let ([r (let ([r (#3%$record crtd 2 7 1)])
                          (#3%pretty-print (#3%list 'parent ($record->vector r)))
                          r)])
                 (#3%pretty-print (#3%list 'child ($record->vector r)))
                 r)))))))
  (equal?
    (parameterize ([print-vector-length #f])
      (with-output-to-string
       ; adding a grandchild
        (lambda ()
          (define prtd (make-record-type "parent" '(x y)))
          (define crtd (make-record-type prtd "child" '(z)))
          (define gcrtd (make-record-type crtd "grand-child" '(w)))
          (define prcd
            (make-record-constructor-descriptor prtd #f
              (rec pprot
                (lambda (new)
                  (lambda (x n m)
                    (let ([r (new x (+ n m))])
                      (pretty-print `(parent ,($record->vector r)))
                      r))))))
          (define crcd
            (make-record-constructor-descriptor crtd prcd
              (rec cprot
                (lambda (p)
                  (lambda (z x n m)
                    (let ([r ((p x n m) z)])
                      (pretty-print `(child ,($record->vector r)))
                      r))))))
          (define gcrcd
            (make-record-constructor-descriptor gcrtd crcd
              (rec gcprot
                (lambda (p)
                  (lambda (w x q z)
                    (let ([r ((p z x q 7) (* w 3))])
                      (pretty-print `(grand-child ,($record->vector r)))
                      r))))))
          (define pcons (r6rs:record-constructor prcd))
          (define ccons (r6rs:record-constructor crcd))
          (define gccons (r6rs:record-constructor gcrcd))
          (pretty-print ($record->vector (pcons 1 2 3)))
          (pretty-print ($record->vector (ccons 1 2 3 4)))
          (pretty-print ($record->vector (gccons 1 2 3 4))))))
    (format "~
      (parent #(parent 1 5))\n~
      #(parent 1 5)\n~
      (parent #(child 2 7 1))\n~
      (child #(child 2 7 1))\n~
      #(child 2 7 1)\n~
      (parent #(grand-child 2 10 4 3))\n~
      (child #(grand-child 2 10 4 3))\n~
      (grand-child #(grand-child 2 10 4 3))\n~
      #(grand-child 2 10 4 3)\n"))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define gcrtd (make-record-type crtd "grand-child" '(w)))
           (define prcd
             (make-record-constructor-descriptor prtd #f
               (rec pprot
                 (lambda (new)
                   (lambda (x n m)
                     (let ([r (new x (+ n m))])
                       (pretty-print `(parent ,($record->vector r)))
                       r))))))
           (define crcd
             (make-record-constructor-descriptor crtd prcd
               (rec cprot
                 (lambda (p)
                   (lambda (z x n m)
                     (let ([r ((p x n m) z)])
                       (pretty-print `(child ,($record->vector r)))
                       r))))))
           (define gcrcd
             (make-record-constructor-descriptor gcrtd crcd
               (rec gcprot
                 (lambda (p)
                   (lambda (w x q z)
                     (let ([r ((p z x q 7) (* w 3))])
                       (pretty-print `(grand-child ,($record->vector r)))
                       r))))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (define gccons (r6rs:record-constructor gcrcd))
           (pretty-print ($record->vector (pcons 1 2 3)))
           (pretty-print ($record->vector (ccons 1 2 3 4)))
           (pretty-print ($record->vector (gccons 1 2 3 4))))))
    '(lambda ()
       (let ([prtd (#2%make-record-type "parent" '(x y))])
         (let ([crtd (#2%make-record-type prtd "child" '(z))])
           (let ([gcrtd (#2%make-record-type crtd "grand-child" '(w))])
             (#2%pretty-print
               ($record->vector
                 (let ([r (#3%$record prtd 1 5)])
                   (#2%pretty-print (#2%list 'parent ($record->vector r)))
                   r)))
             (#2%pretty-print
               ($record->vector
                 (let ([r (let ([r (#3%$record crtd 2 7 1)])
                            (#2%pretty-print (#2%list 'parent ($record->vector r)))
                            r)])
                   (#2%pretty-print (#2%list 'child ($record->vector r)))
                   r)))
             (#2%pretty-print
               ($record->vector
                 (let ([r (let ([r (let ([r (#3%$record gcrtd 2 10 4 3)])
                                     (#2%pretty-print (#2%list 'parent ($record->vector r)))
                                     r)])
                            (#2%pretty-print (#2%list 'child ($record->vector r)))
                            r)])
                   (#2%pretty-print (#2%list 'grand-child ($record->vector r)))
                   r))))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define prtd (make-record-type "parent" '(x y)))
           (define crtd (make-record-type prtd "child" '(z)))
           (define gcrtd (make-record-type crtd "grand-child" '(w)))
           (define prcd
             (make-record-constructor-descriptor prtd #f
               (rec pprot
                 (lambda (new)
                   (lambda (x n m)
                     (let ([r (new x (+ n m))])
                       (pretty-print `(parent ,($record->vector r)))
                       r))))))
           (define crcd
             (make-record-constructor-descriptor crtd prcd
               (rec cprot
                 (lambda (p)
                   (lambda (z x n m)
                     (let ([r ((p x n m) z)])
                       (pretty-print `(child ,($record->vector r)))
                       r))))))
           (define gcrcd
             (make-record-constructor-descriptor gcrtd crcd
               (rec gcprot
                 (lambda (p)
                   (lambda (w x q z)
                     (let ([r ((p z x q 7) (* w 3))])
                       (pretty-print `(grand-child ,($record->vector r)))
                       r))))))
           (define pcons (r6rs:record-constructor prcd))
           (define ccons (r6rs:record-constructor crcd))
           (define gccons (r6rs:record-constructor gcrcd))
           (pretty-print ($record->vector (pcons 1 2 3)))
           (pretty-print ($record->vector (ccons 1 2 3 4)))
           (pretty-print ($record->vector (gccons 1 2 3 4))))))
    '(lambda ()
       (let ([prtd (#3%make-record-type "parent" '(x y))])
         (let ([crtd (#3%make-record-type prtd "child" '(z))])
           (let ([gcrtd (#3%make-record-type crtd "grand-child" '(w))])
             (#3%pretty-print
               ($record->vector
                 (let ([r (#3%$record prtd 1 5)])
                   (#3%pretty-print (#3%list 'parent ($record->vector r)))
                   r)))
             (#3%pretty-print
               ($record->vector
                 (let ([r (let ([r (#3%$record crtd 2 7 1)])
                            (#3%pretty-print (#3%list 'parent ($record->vector r)))
                            r)])
                   (#3%pretty-print (#3%list 'child ($record->vector r)))
                   r)))
             (#3%pretty-print
               ($record->vector
                 (let ([r (let ([r (let ([r (#3%$record gcrtd 2 10 4 3)])
                                     (#3%pretty-print (#3%list 'parent ($record->vector r)))
                                     r)])
                            (#3%pretty-print (#3%list 'child ($record->vector r)))
                            r)])
                   (#3%pretty-print (#3%list 'grand-child ($record->vector r)))
                   r))))))))
  (error? ; given prcd is not for parent rtd
    (parameterize ([print-vector-length #f])
      (with-output-to-string
       ; adding a grandchild
        (lambda ()
          (define prtd (make-record-type "parent" '(x y)))
          (define crtd (make-record-type prtd "child" '(z)))
          (define gcrtd (make-record-type prtd "grand-child" '(w)))
          (define prcd
            (make-record-constructor-descriptor prtd #f
              (rec pprot
                (lambda (new)
                  (lambda (x n m)
                    (let ([r (new x (+ n m))])
                      (pretty-print `(parent ,($record->vector r)))
                      r))))))
          (define crcd
            (make-record-constructor-descriptor crtd prcd
              (rec cprot
                (lambda (p)
                  (lambda (z x n m)
                    (let ([r ((p x n m) z)])
                      (pretty-print `(child ,($record->vector r)))
                      r))))))
          (define gcrcd
            (make-record-constructor-descriptor gcrtd crcd
              (rec gcprot
                (lambda (p)
                  (lambda (w x q z)
                    (let ([r ((p z x q 7) (* w 3))])
                      (pretty-print `(grand-child ,($record->vector r)))
                      r))))))
          (define pcons (r6rs:record-constructor prcd))
          (define ccons (r6rs:record-constructor crcd))
          (define gccons (r6rs:record-constructor gcrcd))
          (pretty-print ($record->vector (pcons 1 2 3)))
          (pretty-print ($record->vector (ccons 1 2 3 4)))
          (pretty-print ($record->vector (gccons 1 2 3 4)))))))
  (eqv?
    (make-record-type-descriptor 'foo #f '#{rats c7ajhty66y4x1og-a} #f #f '#())
    (make-record-type-descriptor 'bar #f '#{rats c7ajhty66y4x1og-a} #f #f '#()))
  (eqv?
    (let ()
      (define rtd (make-record-type-descriptor 'bar #f #f #f #f '#()))
      (record-type-sealed? rtd))
    #f)
  (eqv?
    (let ()
      (define rtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
      (record-type-sealed? rtd))
    #t)
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define rtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
           (record-type-sealed? rtd))))
    '(begin
       (#2%make-record-type-descriptor 'bar #f #f #t #f '#0())
       #t))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define rtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
           (record-type-sealed? rtd))))
    '#t)
  (eqv?
    (let ()
      (define rtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
      (record? ((record-constructor rtd))))
    #t)
  (eqv?
    (let ()
      (define rtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
      (r6rs:record? ((record-constructor rtd))))
    #t)
  (eqv?
    (let ()
      (define rtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
      (record? ((record-constructor rtd)) rtd))
    #t)
  (eqv?
    (let ()
      (define prtd (make-record-type-descriptor 'bar #f #f #f #f '#()))
      (define crtd (make-record-type-descriptor 'foo prtd #f #f #f '#()))
      (record? ((record-constructor crtd)) prtd))
    #t)
  (error? ; parent sealed
    (let ()
      (define prtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
      (define crtd (make-record-type-descriptor 'foo prtd #f #f #f '#()))
      (record? ((record-constructor crtd)) prtd)))
  (eqv?
    (let ()
      (define prtd (make-record-type-descriptor 'bar #f #f #f #f '#()))
      (define crtd (make-record-type-descriptor 'foo prtd #f #f #f '#()))
      (define xrtd (make-record-type-descriptor 'poo #f #f #f #f '#()))
      (record? ((record-constructor xrtd)) prtd))
    #f)
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (define rtd (make-record-type-descriptor 'bar #f #f #f #f '#()))
           (record? x rtd))))
    '(lambda (x)
       (#3%record? x (#2%make-record-type-descriptor 'bar #f #f #f #f '#()))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (define rtd (make-record-type-descriptor 'bar #f #f #t #f '#()))
           (record? x rtd))))
    '(lambda (x)
       (#3%$sealed-record? x (#2%make-record-type-descriptor 'bar #f #f #t #f '#0()))))
)

(mat r6rs-records-procedural2
  (equal?
    (with-output-to-string
      (lambda ()
        (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
        (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
        (define a (begin (write 'b) (record-accessor (begin (write 'c) rtd) 0)))
        (write (a ((begin (write 'd) (record-constructor (begin (write 'e) rtd))) 17)))))
    "abcde17")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define a (begin (write 'b) (record-accessor (begin (write 'c) rtd) 0)))
           (write (a ((begin (write 'd) (record-constructor (begin (write 'e) rtd))) 17))))))
    '(lambda ()
       (#2%write 'a)
       (#2%write 'b)
       (#2%write 'c)
       (#2%write (begin (#2%write 'd) (#2%write 'e) 17))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define a (begin (write 'b) (record-accessor (begin (write 'c) rtd) 0)))
           (write (a ((begin (write 'd) (record-constructor (begin (write 'e) rtd))) 17))))))
    '(lambda ()
       (#3%write 'a)
       (#3%write 'b)
       (#3%write 'c)
       (#3%write (begin (#3%write 'd) (#3%write 'e) 17))))

  (equal?
    (with-output-to-string
      (lambda ()
        (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
        (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
        (write ((begin (write 'b) (record-accessor (begin (write 'c) rtd) 0))
                ((begin (write 'b) (record-constructor (begin (write 'c) rtd))) 17)))))
    "abcbc17")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (write ((begin (write 'b) (record-accessor (begin (write 'c) rtd) 0))
                   ((begin (write 'b) (record-constructor (begin (write 'c) rtd))) 17))))))
    '(lambda ()
       (#2%write 'a)
       (#2%write
         (begin
           (#2%write 'b)
           (#2%write 'c)
           (#2%write 'b)
           (#2%write 'c)
           17))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (write ((begin (write 'b) (record-accessor (begin (write 'c) rtd) 0))
                   ((begin (write 'b) (record-constructor (begin (write 'c) rtd))) 17))))))
    '(lambda ()
       (#3%write 'a)
       (#3%write
         (begin
           (#3%write 'b)
           (#3%write 'c)
           (#3%write 'b)
           (#3%write 'c)
           17))))

  ((lambda (x y) (and (member x y) #t))
    (with-output-to-string
      (lambda ()
        (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
        (define-syntax rtd (lambda (x) #`(quote #,(make-record-type-descriptor 'foo #f uid #f #f '#((mutable x))))))
        (define-syntax qr (lambda (x) #`(quote #,((record-constructor rtd) 17))))
        (write
          (let ([r qr])
            ((begin (write 'b) (record-mutator (begin (write 'c) rtd) 0)) r 23)
            ((begin (write 'b) (record-accessor (begin (write 'c) (record-rtd r)) 0)) r)))))
    '("bcbc17" "bcbc23"))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define-syntax rtd (lambda (x) #`(quote #,(make-record-type-descriptor 'foo #f uid #f #f '#((mutable x))))))
           (define-syntax qr (lambda (x) #`(quote #,((record-constructor rtd) 17))))
           (write
             (let ([r qr])
               ((begin (write 'b) (record-mutator (begin (write 'c) rtd) 0)) r 23)
               ((begin (write 'b) (record-accessor (begin (write 'c) (record-rtd r)) 0)) r))))))
    `(lambda ()
       (#2%write
         (begin
           (#2%write 'b)
           (#2%write 'c)
           (#3%$object-set! 'scheme-object ',record? ,fixnum? 23)
           (#2%write 'b)
           (#2%write 'c)
           17))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define-syntax rtd (lambda (x) #`(quote #,(make-record-type-descriptor 'foo #f uid #f #f '#((mutable x))))))
           (define-syntax qr (lambda (x) #`(quote #,((record-constructor rtd) 17))))
           (write
             (let ([r qr])
               ((begin (write 'b) (record-mutator (begin (write 'c) rtd) 0)) r 23)
               ((begin (write 'b) (record-accessor (begin (write 'c) (record-rtd r)) 0)) r))))))
    `(lambda ()
       (#3%write
         (begin
           (#3%write 'b)
           (#3%write 'c)
           (#3%$object-set! 'scheme-object ',record? ,fixnum? 23)
           (#3%write 'b)
           (#3%write 'c)
           17))))

  (equal?
    (with-output-to-string
      (lambda ()
        (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
        (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
        (write ((begin (write 'b) (record-predicate (begin (write 'c) rtd)))
                ((begin (write 'b) (record-constructor (begin (write 'c) rtd))) 17)))))
    "abcbc#t")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (write ((begin (write 'b) (record-predicate (begin (write 'c) rtd)))
                   ((begin (write 'b) (record-constructor (begin (write 'c) rtd))) 17))))))
    '(lambda ()
       (#2%write 'a)
       (#2%write
         (begin
           (#2%write 'b)
           (#2%write 'c)
           (#2%write 'b)
           (#2%write 'c)
           #t))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (write ((begin (write 'b) (record-predicate (begin (write 'c) rtd)))
                   ((begin (write 'b) (record-constructor (begin (write 'c) rtd))) 17))))))
    '(lambda ()
       (#3%write 'a)
       (#3%write
         (begin
           (#3%write 'b)
           (#3%write 'c)
           (#3%write 'b)
           (#3%write 'c)
           #t))))

  (equal?
    (with-output-to-string
      (lambda ()
        (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
        (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
        (define make (begin (write 'b) (record-constructor (begin (write 'c) rtd))))
        (define a (begin (write 'd) (record-accessor (begin (write 'e) rtd) 0)))
        (define x (make (let ((f (begin (write 'f) (lambda (x) x)))) (let ([g (begin (write 'g) (lambda (x) (or x f)))]) (g 3) (g 17)))))
        (write (a x))))
    "abcdefg17")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define make (begin (write 'b) (record-constructor (begin (write 'c) rtd))))
           (define a (begin (write 'd) (record-accessor (begin (write 'e) rtd) 0)))
           (define x (make (let ((f (begin (write 'f) (lambda (x) x)))) (let ([g (begin (write 'g) (lambda (x) (or x f)))]) (g 3) (g 17)))))
           (write (a x)))))
    '(lambda ()
       (#2%write 'a)
       (#2%write 'b)
       (#2%write 'c)
       (#2%write 'd)
       (#2%write 'e)
       (#2%write 'f)
       (#2%write 'g)
       (#2%write 17)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define make (begin (write 'b) (record-constructor (begin (write 'c) rtd))))
           (define a (begin (write 'd) (record-accessor (begin (write 'e) rtd) 0)))
           (define x (make (let ((f (begin (write 'f) (lambda (x) x)))) (let ([g (begin (write 'g) (lambda (x) (or x f)))]) (g 3) (g 17)))))
           (write (a x)))))
    '(lambda ()
       (#3%write 'a)
       (#3%write 'b)
       (#3%write 'c)
       (#3%write 'd)
       (#3%write 'e)
       (#3%write 'f)
       (#3%write 'g)
       (#3%write 17)))

  (equal?
    (with-output-to-string
      (lambda ()
        (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
        (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
        (define rcd (begin (write 'b) (make-record-constructor-descriptor rtd #f #f)))
        (define a (begin (write 'c) (record-accessor (begin (write 'd) rtd) 0)))
        (write (a ((begin (write 'e) (record-constructor (begin (write 'f) rcd))) 17)))))
    "abcdef17")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define rcd (begin (write 'b) (make-record-constructor-descriptor rtd #f #f)))
           (define a (begin (write 'c) (record-accessor (begin (write 'd) rtd) 0)))
           (write (a ((begin (write 'e) (record-constructor (begin (write 'f) rcd))) 17))))))
    '(lambda ()
       (#2%write 'a)
       (#2%write 'b)
       (#2%write 'c)
       (#2%write 'd)
       (#2%write (begin (#2%write 'e) (#2%write 'f) 17))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define rcd (begin (write 'b) (make-record-constructor-descriptor rtd #f #f)))
           (define a (begin (write 'c) (record-accessor (begin (write 'd) rtd) 0)))
           (write (a ((begin (write 'e) (record-constructor (begin (write 'f) rcd))) 17))))))
    '(lambda ()
       (#3%write 'a)
       (#3%write 'b)
       (#3%write 'c)
       (#3%write 'd)
       (#3%write (begin (#3%write 'e) (#3%write 'f) 17))))

  (equal?
    (with-output-to-string
      (lambda ()
        (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
        (define rtd1 (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
        (define rcd1 (begin (write 'b) (make-record-constructor-descriptor rtd1 #f #f)))
        (define rtd2 (begin (write 'a) (make-record-type-descriptor 'foo rtd1 uid #f #f '#((immutable x)))))
        (define rcd2 (begin (write 'b) (make-record-constructor-descriptor rtd2 rcd1 #f)))
        (write (list rcd1 rcd2))))
    "abab(#<record constructor descriptor> #<record constructor descriptor>)")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd1 (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define rcd1 (begin (write 'b) (make-record-constructor-descriptor rtd1 #f #f)))
           (define rtd2 (begin (write 'a) (make-record-type-descriptor 'foo rtd1 uid #f #f '#((immutable x)))))
           (define rcd2 (begin (write 'b) (make-record-constructor-descriptor rtd2 rcd1 #f)))
           (write (list rcd1 rcd2)))))
    `(lambda ()
       (#2%write 'a)
       (#2%write 'b)
       (#2%write 'a)
       (#2%write 'b)
       (#2%write
         (#2%list
           ',record-constructor-descriptor?
           ',record-constructor-descriptor?))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define-syntax uid (lambda (x) #`(quote #,(datum->syntax #'* (gensym)))))
           (define rtd1 (begin (write 'a) (make-record-type-descriptor 'foo #f uid #f #f '#((immutable x)))))
           (define rcd1 (begin (write 'b) (make-record-constructor-descriptor rtd1 #f #f)))
           (define rtd2 (begin (write 'a) (make-record-type-descriptor 'foo rtd1 uid #f #f '#((immutable x)))))
           (define rcd2 (begin (write 'b) (make-record-constructor-descriptor rtd2 rcd1 #f)))
           (write (list rcd1 rcd2)))))
    `(lambda ()
       (#3%write 'a)
       (#3%write 'b)
       (#3%write 'a)
       (#3%write 'b)
       (#3%write
         (#3%list
           ',record-constructor-descriptor?
           ',record-constructor-descriptor?))))

  ; test cross-library optimization of record definitions
  (begin
    (with-output-to-file "testfile-rrp1.ss"
      (lambda ()
        (pretty-print 
          '(library (testfile-rrp1)
             (export 
               make-bar bar? bar-x
               make-foo foo? foo-x foo-y foo-x-set!
               bar-inst foo-inst)
             (import (chezscheme))
             (define-record-type bar (fields x))
             (define-record-type foo (parent bar) (fields (mutable x) y)
               (protocol (lambda (pargs->new) (lambda (y z) ((pargs->new z) 17 y)))))
             (define bar-inst (make-bar 7))
             (define foo-inst (make-foo 13 11)))))
      'replace)
    #t)
  ; first, the control, with cp0 disabled
  (begin
    (load-library "testfile-rrp1.ss" (lambda (x) (parameterize ([enable-cp0 #f]) (eval x))))
    #t)
  (equal?
    (let ()
      (define ugh
        (lambda (x)
          (import (testfile-rrp1))
          (let ([b (make-bar 23)] [f (make-foo 31 41)])
            (foo-x-set! f 37) 
            (list
              (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
              (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
              (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
              (foo-x f) (foo-x foo-inst)
              (foo-y f) (foo-y foo-inst)))))
      (ugh 19))
    '(#f #f #t #f #t #f #t #t #t #t 23 41 11 7 37 17 31 13))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (import (testfile-rrp1))
           (let ([b (make-bar 23)] [f (make-foo 31 41)])
             (foo-x-set! f 37) 
             (list
               (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
               (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
               (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
               (foo-x f) (foo-x foo-inst)
               (foo-y f) (foo-y foo-inst))))))
    '(begin
       (#3%$invoke-library '(testfile-rrp1) '() 'testfile-rrp1)
       (lambda (x)
         (let ([b ((#3%$top-level-value 'make-bar) 23)]
               [f ((#3%$top-level-value 'make-foo) 31 41)])
           ((#3%$top-level-value 'foo-x-set!) f 37)
           (#2%list
             ((#3%$top-level-value 'foo?) x)
             ((#3%$top-level-value 'foo?) b)
             ((#3%$top-level-value 'foo?) f)
             ((#3%$top-level-value 'foo?) (#3%$top-level-value 'bar-inst))
             ((#3%$top-level-value 'foo?) (#3%$top-level-value 'foo-inst))
             ((#3%$top-level-value 'bar?) x)
             ((#3%$top-level-value 'bar?) b)
             ((#3%$top-level-value 'bar?) f)
             ((#3%$top-level-value 'bar?) (#3%$top-level-value 'bar-inst))
             ((#3%$top-level-value 'bar?) (#3%$top-level-value 'foo-inst))
             ((#3%$top-level-value 'bar-x) b)
             ((#3%$top-level-value 'bar-x) f)
             ((#3%$top-level-value 'bar-x) (#3%$top-level-value 'foo-inst))
             ((#3%$top-level-value 'bar-x) (#3%$top-level-value 'bar-inst))
             ((#3%$top-level-value 'foo-x) f)
             ((#3%$top-level-value 'foo-x) (#3%$top-level-value 'foo-inst))
             ((#3%$top-level-value 'foo-y) f)
             ((#3%$top-level-value 'foo-y) (#3%$top-level-value 'foo-inst)))))))
  ; now with cp0 enabled and optimize-level 2...also need compiler or cross-library optimization won't occur
  (begin
    (load-library "testfile-rrp1.ss" (lambda (x) (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f] [current-eval compile]) (eval x))))
    #t)
  (equal?
    (let ()
      (define ugh
        (lambda (x)
          (import (testfile-rrp1))
          (let ([b (make-bar 23)] [f (make-foo 31 41)])
            (foo-x-set! f 37) 
            (list
              (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
              (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
              (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
              (foo-x f) (foo-x foo-inst)
              (foo-y f) (foo-y foo-inst)))))
      (ugh 19))
    '(#f #f #t #f #t #f #t #t #t #t 23 41 11 7 37 17 31 13))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (import (testfile-rrp1))
           (let ([b (make-bar 23)] [f (make-foo 31 41)])
             (foo-x-set! f 37) 
             (list
               (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
               (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
               (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
               (foo-x f) (foo-x foo-inst)
               (foo-y f) (foo-y foo-inst))))))
    `(begin
       (#3%$invoke-library '(testfile-rrp1) '() 'testfile-rrp1)
       (lambda (x)
         (let ([f (#3%$record ',record-type-descriptor? 41 17 31)])
           (#3%$object-set! 'scheme-object f ,fixnum? 37)
           (#2%list (#3%record? x ',record-type-descriptor?) #f
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?)
             (#3%record? x ',record-type-descriptor?) #t
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?) 23
             41
             (let ([g4 (#3%$top-level-value 'foo-inst)])
               (if (#3%record? g4 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'bar-x g4 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g4 ,fixnum?))
             (let ([g4 (#3%$top-level-value 'bar-inst)])
               (if (#3%record? g4 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'bar-x g4 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g4 ,fixnum?))
             (#3%$object-ref 'scheme-object f ,fixnum?)
             (let ([g3 (#3%$top-level-value 'foo-inst)])
               (if (#3%record? g3 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'foo-x g3 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g3 ,fixnum?))
             31
             (let ([g2 (#3%$top-level-value 'foo-inst)])
               (if (#3%record? g2 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'foo-y g2 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g2 ,fixnum?)))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (import (testfile-rrp1))
           (let ([b (make-bar 23)] [f (make-foo 31 41)])
             (foo-x-set! f 37) 
             (list
               (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
               (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
               (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
               (foo-x f) (foo-x foo-inst)
               (foo-y f) (foo-y foo-inst))))))
    `(begin
       (#3%$invoke-library '(testfile-rrp1) '() 'testfile-rrp1)
       (lambda (x)
         (let ([f (#3%$record ',record-type-descriptor? 41 17 31)])
           (#3%$object-set! 'scheme-object f ,fixnum? 37)
           (#3%list (#3%record? x ',record-type-descriptor?) #f
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?)
             (#3%record? x ',record-type-descriptor?) #t
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?) 23
             41
             (let ([g4 (#3%$top-level-value 'foo-inst)])
               (if (#3%record? g4 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'bar-x g4 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g4 ,fixnum?))
             (let ([g4 (#3%$top-level-value 'bar-inst)])
               (if (#3%record? g4 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'bar-x g4 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g4 ,fixnum?))
             (#3%$object-ref 'scheme-object f ,fixnum?)
             (let ([g3 (#3%$top-level-value 'foo-inst)])
               (if (#3%record? g3 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'foo-x g3 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g3 ,fixnum?))
             31
             (let ([g2 (#3%$top-level-value 'foo-inst)])
               (if (#3%record? g2 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'foo-y g2 ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g2 ,fixnum?)))))))
  ; now with cp0 enabled and optimize-level 3...also need compiler or cross-library optimization won't occur
  (begin
    (load-library "testfile-rrp1.ss" (lambda (x) (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f] [current-eval compile]) (eval x))))
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (import (testfile-rrp1))
           (let ([b (make-bar 23)] [f (make-foo 31 41)])
             (foo-x-set! f 37) 
             (list
               (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
               (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
               (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
               (foo-x f) (foo-x foo-inst)
               (foo-y f) (foo-y foo-inst))))))
    `(begin
       (#3%$invoke-library '(testfile-rrp1) '() 'testfile-rrp1)
       (lambda (x)
         (let ([f (#3%$record ',record-type-descriptor? 41 17 31)])
           (#3%$object-set! 'scheme-object f ,fixnum? 37)
           (#2%list (#3%record? x ',record-type-descriptor?) #f
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?)
             (#3%record? x ',record-type-descriptor?) #t
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?) 23
             41
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'bar-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object f ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             31
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (import (testfile-rrp1))
           (let ([b (make-bar 23)] [f (make-foo 31 41)])
             (foo-x-set! f 37) 
             (list
               (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
               (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
               (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
               (foo-x f) (foo-x foo-inst)
               (foo-y f) (foo-y foo-inst))))))
    `(begin
       (#3%$invoke-library '(testfile-rrp1) '() 'testfile-rrp1)
       (lambda (x)
         (let ([f (#3%$record ',record-type-descriptor? 41 17 31)])
           (#3%$object-set! 'scheme-object f ,fixnum? 37)
           (#3%list (#3%record? x ',record-type-descriptor?) #f
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?)
             (#3%record? x ',record-type-descriptor?) #t
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?) 23
             41
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'bar-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object f ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             31
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?))))))
  ; now compiling to / loading from a file with cp0 enabled and optimize-level 3
  (begin
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (compile-library "testfile-rrp1.ss"))
    (load-library "testfile-rrp1.so")
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (import (testfile-rrp1))
           (let ([b (make-bar 23)] [f (make-foo 31 41)])
             (foo-x-set! f 37) 
             (list
               (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
               (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
               (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
               (foo-x f) (foo-x foo-inst)
               (foo-y f) (foo-y foo-inst))))))
    `(begin
       (#3%$invoke-library '(testfile-rrp1) '() 'testfile-rrp1)
       (lambda (x)
         (let ([f (#3%$record ',record-type-descriptor? 41 17 31)])
           (#3%$object-set! 'scheme-object f ,fixnum? 37)
           (#2%list (#3%record? x ',record-type-descriptor?) #f
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?)
             (#3%record? x ',record-type-descriptor?) #t
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?) 23
             41
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'bar-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object f ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             31
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (import (testfile-rrp1))
           (let ([b (make-bar 23)] [f (make-foo 31 41)])
             (foo-x-set! f 37) 
             (list
               (foo? x) (foo? b) (foo? f) (foo? bar-inst) (foo? foo-inst)
               (bar? x) (bar? b) (bar? f) (bar? bar-inst) (bar? foo-inst)
               (bar-x b) (bar-x f) (bar-x foo-inst) (bar-x bar-inst)
               (foo-x f) (foo-x foo-inst)
               (foo-y f) (foo-y foo-inst))))))
    `(begin
       (#3%$invoke-library '(testfile-rrp1) '() 'testfile-rrp1)
       (lambda (x)
         (let ([f (#3%$record ',record-type-descriptor? 41 17 31)])
           (#3%$object-set! 'scheme-object f ,fixnum? 37)
           (#3%list (#3%record? x ',record-type-descriptor?) #f
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?)
             (#3%record? x ',record-type-descriptor?) #t
             #t
             (#3%record? (#3%$top-level-value 'bar-inst) ',record-type-descriptor?)
             (#3%record? (#3%$top-level-value 'foo-inst) ',record-type-descriptor?) 23
             41
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'bar-inst) ,fixnum?)
             (#3%$object-ref 'scheme-object f ,fixnum?)
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?)
             31
             (#3%$object-ref 'scheme-object (#3%$top-level-value 'foo-inst) ,fixnum?))))))
  ;; regression tests for cp0 handling of record-mutator when handed a
  ;; (record-rtd rtd expr) directly.
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define build-box
             (lambda (name k)
               (let ([gs (gensym (symbol->string name))])
                 (define-syntax mrtd
                   (identifier-syntax
                     (make-record-type-descriptor
                       name #f gs #f #f '#((mutable x)))))
                 (k (record-constructor 
                      (make-record-constructor-descriptor mrtd #f #f))
                   (record-predicate mrtd)
                   (record-accessor mrtd 0)
                   (record-mutator mrtd 0)))))
           (build-box 'record-box
             (lambda (box box? unbox set-box!)
               (let ([b (box 4)])
                 (set-box! b (* 3 (unbox b)))
                 (list (box? b) (unbox b))))))))
    `(let ([gs (#2%gensym "record-box")])
       (let ([g5 (#2%make-record-type-descriptor 'record-box #f gs #f #f '#((mutable x)))]
             [g6 (#2%make-record-type-descriptor 'record-box #f gs #f #f '#((mutable x)))]
             [g4 (#2%make-record-type-descriptor 'record-box #f gs #f #f '#((mutable x)))])
         (let ([b ((#2%record-constructor
                     (#2%make-record-constructor-descriptor
                       (#2%make-record-type-descriptor 'record-box #f gs #f #f '#((mutable x)))
                       #f #f))
                   4)])
           (let ([g7 (#2%* 3
                       (begin
                         (if (#3%record? b g6) (#2%void) (#3%$record-oops 'unbox b g6))
                         (#3%$object-ref 'scheme-object b ,fixnum?)))])
             (if (#3%record? b g4) (#2%void) (#3%$record-oops 'set-box! b g4))
             (#3%$object-set! 'scheme-object b ,fixnum? g7))
           (#2%list
             (#3%record? b g5)
             (begin
               (if (#3%record? b g6) (#2%void) (#3%$record-oops 'unbox b g6))
               (#3%$object-ref 'scheme-object b ,fixnum?)))))))
    (equal?
      (let ()
        (define build-box
          (lambda (name k)
            (let ([gs (gensym (symbol->string name))])
              (define-syntax mrtd
                (identifier-syntax
                  (make-record-type-descriptor
                    name #f gs #f #f '#((mutable x)))))
              (k (record-constructor 
                   (make-record-constructor-descriptor mrtd #f #f))
                (record-predicate mrtd)
                (record-accessor mrtd 0)
                (record-mutator mrtd 0)))))
        (build-box 'record-box
          (lambda (box box? unbox set-box!)
            (let ([b (box 4)])
              (set-box! b (* 3 (unbox b)))
              (list (box? b) (unbox b))))))
      '(#t 12))
    (equivalent-expansion?
      (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
        (expand/optimize
          '(let ()
             (define useless
               (lambda (name)
                 (record-mutator (make-record-type-descriptor
                                   name #f #f #f #f '#((mutable x))) 0)))
             (procedure? (useless 'useless-box-setter)))))
      `(#2%procedure?
         (let ([g0 (#2%make-record-type-descriptor 'useless-box-setter #f #f #f #f '#((mutable x)))])
           (lambda (g1 g2)
             (if (#3%record? g1 g0)
                 (#2%void)
                 (#3%$record-oops 'moi g1 g0))
             (#3%$object-set! 'scheme-object g1 ,fixnum? g2)))))
    (let ()
      (define useless
        (lambda (name)
          (record-mutator (make-record-type-descriptor
                            name #f #f #f #f '#((mutable x))) 0)))
      (procedure? (useless 'useless-box-setter)))
)

(mat r6rs-records-syntactic
 ; adapted from r6rs
  (begin
    (define-record-type point (fields x y))
    #t)
  (error? ; invalid syntax
    point)
  (error? ; wrong number of arguments
    (make-point))
  (error? ; wrong number of arguments
    (make-point 3))
  (error? ; wrong number of arguments
    (make-point 3 4 5))
  (begin
    (define p (make-point 3 4))
    #t)
  (error? ; wrong number of arguments
    (point?))
  (error? ; wrong number of arguments
    (point? p p))
  (point? p)
  (not (point? '(3 . 4)))
  (not (point? (let () (define-record-type point (fields x y)) (make-point 3 4))))
  (error? ; unbound
    (point-x-set! p 17))
  (error? ; unbound
    (point-y-set! p 17))
  (eqv? (point-x p) 3)
  (eqv? (point-y p) 4)
  (error? ; wrong number of arguments
    (point-x))
  (error? ; wrong number of arguments
    (point-y p p))
  (not (eq? p (make-point 3 4)))
  (not (record-field-mutable? (record-type-descriptor point) 0))
  (not (record-field-mutable? (record-type-descriptor point) 1))
  (error? (record-mutator (record-type-descriptor point) 0))
  (error? (record-mutator (record-type-descriptor point) 1))

  (let ()
    (define-record-type point (fields x y))
    (define p (make-point 3 4))
    (and
      (point? p)
      (not (point? '(3 . 4)))
      (not (point? (let () (define-record-type point (fields x y)) (make-point 3 4))))
      (eqv? (point-x p) 3)
      (eqv? (point-y p) 4)
      (not (eq? p (make-point 3 4)))))

  (begin (set! make-point values) #t)
  (begin
    (define-record-type (point make-point point?)
      (fields 
        (immutable x point-x)
        (immutable y point-y)))
    #t)
  (error? ; invalid syntax
    point)
  (error? ; wrong number of arguments
    (make-point))
  (error? ; wrong number of arguments
    (make-point 3))
  (error? ; wrong number of arguments
    (make-point 3 4 5))
  (begin
    (define p (make-point 3 4))
    #t)
  (error? ; wrong number of arguments
    (point?))
  (error? ; wrong number of arguments
    (point? p p))
  (point? p)
  (not (point? '(3 . 4)))
  (not (point? (let () (define-record-type point (fields x y)) (make-point 3 4))))
  (error? ; unbound
    (point-x-set! p 17))
  (error? ; unbound
    (point-y-set! p 17))
  (eqv? (point-x p) 3)
  (eqv? (point-y p) 4)
  (error? ; wrong number of arguments
    (point-x))
  (error? ; wrong number of arguments
    (point-y p p))
  (not (eq? p (make-point 3 4)))

  (begin
    (define-record-type widget (fields x))
    #t)
  (begin
    (define-record-type frob
      (fields (mutable widget))
      (protocol
        (lambda (p)
          (lambda (n) (p (make-widget n))))))
    #t)
  (begin
    (define f (make-frob 17))
    #t)
  (frob? f)
  (widget? (frob-widget f))
  (error? ; wrong number of arguments
    (frob-widget-set!))
  (error? ; wrong number of arguments
    (frob-widget-set! f))
  (error? ; wrong number of arguments
    (frob-widget-set! f f f))
  (eqv? (frob-widget-set! f (list (frob-widget f))) (void))
  (pair? (frob-widget f))
  (not (widget? (frob-widget f)))

  (begin (set! make-frob values) #t)
  (begin
    (define-record-type (frob make-frob frob?)
      (fields (mutable widget
                       frob-widget
                       frob-widget-set!))
      (protocol
        (lambda (p)
          (lambda (n) (p (make-widget n))))))
    #t)
  (begin
    (define f (make-frob 17))
    #t)
  (frob? f)
  (widget? (frob-widget f))
  (error? ; wrong number of arguments
    (frob-widget-set!))
  (error? ; wrong number of arguments
    (frob-widget-set! f))
  (error? ; wrong number of arguments
    (frob-widget-set! f f f))
  (eqv? (frob-widget-set! f (list (frob-widget f))) (void))
  (pair? (frob-widget f))
  (not (widget? (frob-widget f)))

  (begin (set! make-frob values) #t)
  (begin
    (define-record-type frob
      (fields (mutable widget getwid setwid!))
      (protocol
        (lambda (p)
          (lambda (n) (p (make-widget n))))))
    #t)
  (begin
    (define f (make-frob 17))
    #t)
  (frob? f)
  (widget? (getwid f))
  (error? ; wrong number of arguments
    (setwid!))
  (error? ; wrong number of arguments
    (setwid! f))
  (error? ; wrong number of arguments
    (setwid! f f f))
  (eqv? (setwid! f (list (getwid f))) (void))
  (pair? (getwid f))
  (not (widget? (getwid f)))

  (begin
    (define-record-type (point make-point point?)
      (fields (immutable x point-x)
              (mutable y point-y set-point-y!))
      (nongenerative
        point-4893d957-e00b-11d9-817f-00111175eb9e))
    (define-record-type (cpoint make-cpoint cpoint?)
      (parent point)
      (protocol
       (lambda (n)
         (lambda (x y c) 
           ((n x y) (color->rgb c)))))
      (fields
        (mutable rgb cpoint-rgb cpoint-rgb-set!)))
    (define (color->rgb c)
      (cons 'rgb c))
    (define p1 (make-point 1 2))
    (define p2 (make-cpoint 3 4 'red))
    #t)
  (point? p1)
  (point? p2)
  (not (point? (vector)))
  (not (point? (cons 'a 'b)))
  (not (cpoint? p1))
  (cpoint? p2)
  (eqv? (point-x p1) 1)
  (eqv? (point-y p1) 2)
  (eqv? (point-x p2) 3)
  (eqv? (point-y p2) 4)
  (equal? (cpoint-rgb p2) '(rgb . red))
  (eqv? (set-point-y! p1 17) (void))
  (eqv? (point-y p1) 17)
  (record-type-descriptor? (record-rtd p1))

  (begin
    (define-record-type (ex1 make-ex1 ex1?)
      (protocol (lambda (p) (lambda a (p a))))
      (fields (immutable f ex1-f)))
    (define ex1-i1 (make-ex1 1 2 3))
    #t)
  (equal? (ex1-f ex1-i1) '(1 2 3))

  (begin
    (define-record-type (ex2 make-ex2 ex2?)
      (protocol
        (lambda (p) (lambda (a . b) (p a b))))
      (fields (immutable a ex2-a)
              (immutable b ex2-b)))
    (define ex2-i1 (make-ex2 1 2 3))
    #t)

  (eqv? (ex2-a ex2-i1) 1)
  (equal? (ex2-b ex2-i1) '(2 3))

  (not (record-type-opaque? (record-type-descriptor ex2)))
  (not (record-type-sealed? (record-type-descriptor ex2)))
  (record? ex2-i1)
  (r6rs:record? ex2-i1)

  (begin
    (define *ex3-instance* #f)
    (define-record-type ex3
      (parent cpoint)
      (protocol
       (lambda (n)
         (lambda (x y t)
           (let ((r ((n x y 'red) t)))
             (set! *ex3-instance* r)
             r))))
      (fields 
       (mutable thickness))
      (sealed #t) (opaque #t))
    (define ex3-i1 (make-ex3 1 2 17))
    #t)
  (ex3? ex3-i1)
  (equal? (cpoint-rgb ex3-i1) '(rgb . red))
  (eqv? (ex3-thickness ex3-i1) 17)
  (begin
    (ex3-thickness-set! ex3-i1 18)
    #t)
  (eqv? (ex3-thickness ex3-i1) 18)
  (eqv? *ex3-instance* ex3-i1)

  (record-type-opaque? (record-type-descriptor ex3))
  (record-type-sealed? (record-type-descriptor ex3))
  (not (r6rs:record? ex3-i1))
  (not (record? ex3-i1))
  (error? ; not a record
    (record-rtd ex3-i1))
  (error? ; not a record
    (record-rtd ex3-i1))
  (error? ; parent record type is sealed
    (define-record-type ex3xxx (parent ex3)))
  (record-type-descriptor? (record-type-descriptor ex3))
  (record-constructor-descriptor? (record-constructor-descriptor ex3))
  (equal?
    (parameterize ([print-gensym 'pretty])
      (with-output-to-string
        (lambda ()
          (define-record-type f (fields x))
          (define-record-type g (fields y) (parent f) (opaque #t))
          (define-record-type h (fields z) (parent g) (opaque #t))
          (let ([fx (make-f 'a)] [gx (make-g 'a 'b)] [hx (make-h 'a 'b 'c)])
            (write fx)
            (write gx)
            (write hx)
            (record-writer (record-type-descriptor f)
              (lambda (x p wr)
                (display "#<an f>" p)))
            (record-writer (record-type-descriptor g)
              (lambda (x p wr)
                (display "#<a g>" p)))
            (record-writer (record-type-descriptor h)
              (lambda (x p wr)
                (display "#<an h x=" p)
                (wr (f-x x) p)
                (display " y=" p)
                (wr (g-y x) p)
                (display " z=" p)
                (wr (h-z x) p)
                (display ">" p)))
            (write fx)
            (write gx)
            (write hx)))))
    "#[#:f a]#<g>#<h>#<an f>#<a g>#<an h x=a y=b z=c>")
  (equal?
    (let ()
      (define-record-type f (fields x))
      (define-record-type g (fields y) (parent f) (opaque #t))
      (define-record-type h (fields z) (parent g) (opaque #t))
      (list
        ($record->vector
          (with-input-from-string
            (with-output-to-string
              (lambda () (write (make-f "hello"))))
            read))
        ($record->vector
          (with-input-from-string
            (format "#[~s k]"
              (record-type-uid (record-type-descriptor f)))
            read))
        ($record->vector
          (with-input-from-string
            (format "#[~s k 9]"
              (record-type-uid (record-type-descriptor g)))
            read))
        ($record->vector
          (with-input-from-string
            (format "#[~s opaque? no problem]"
              (record-type-uid (record-type-descriptor h)))
            read))))
    '(#(f "hello")
      #(f k)
      #(g k 9)
      #(h opaque? no problem)))

  (begin
    (define-record-type (unit-vector
                         make-unit-vector
                         unit-vector?)
      (protocol
       (lambda (p)
         (lambda (x y z)
           (let ((length 
                   (sqrt (+ (* x x)
                            (* y y)
                            (* z z)))))
             (p (/ x length)
                (/ y length)
                (/ z length))))))
      (fields (immutable x unit-vector-x)
              (immutable y unit-vector-y)
              (immutable z unit-vector-z)))
    (define uv (make-unit-vector 3 4 0))
    #t)
  (unit-vector? uv)
  (eqv? (unit-vector-x uv) 3/5)
  (eqv? (unit-vector-y uv) 4/5)
  (eqv? (unit-vector-z uv) 0)

 ; to avoid gensyms in error messages, hence problems diffing mat output
  (begin (print-record #f) #t)

 ; test generativity
  (error? ; not a point
    (let f ([x #f])
      (define-record-type point (fields x y))
      (if x 
          (point-x x)
          (f (make-point 3 4)))))

  (not (let f ([x #f])
         (define-record-type point (fields x y))
         (if x 
             (point? x)
             (f (make-point 3 4)))))

  (begin
    (define ($f p)
      (define-record-type point (fields x y))
      (if (eq? p 'make) (make-point 3 4) (point? p)))
    (not ($f ($f 'make))))

  (eqv?
    (let f ([x #f])
      (define-record-type point (fields x y) (nongenerative))
      (if x 
          (point-x x)
          (f (make-point 3 4))))
    3)

  (let f ([x #f])
    (define-record-type point (fields x y) (nongenerative))
    (if x 
        (point? x)
        (f (make-point 3 4))))

  (begin
    (define ($f p)
      (define-record-type point (fields x y) (nongenerative))
      (if (eq? p 'make) (make-point 3 4) (point? p)))
    ($f ($f 'make)))

  (eqv?
    (let f ([x #f])
      (define-record-type point (fields x y) (nongenerative spam))
      (if x 
          (point-x x)
          (f (make-point 3 4))))
    3)

  (error? ; not a point
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y))
      (define-record-type cpoint
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (point-x x)
          (f (make-cpoint 3 4 'red)))))

  (error? ; not a cpoint
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y))
      (define-record-type cpoint
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (cpoint-rgb x)
          (f (make-cpoint 3 4 'red)))))

  (eqv?
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative))
      (define-record-type cpoint
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (point-x x)
          (f (make-cpoint 3 4 'red))))
    3)

  (error? ; not a cpoint
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative))
      (define-record-type cpoint
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (cpoint-rgb x)
          (f (make-cpoint 3 4 'red)))))

  (error? ; incompatible record type
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y))
      (define-record-type cpoint (nongenerative)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (point-x x)
          (f (make-cpoint 3 4 'red)))))

  (error? ; incompatible record type
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y))
      (define-record-type cpoint (nongenerative)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (cpoint-rgb x)
          (f (make-cpoint 3 4 'red)))))

  (eqv?
    (let ()
      (define-record-type point (fields x y))
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type cpoint (nongenerative)
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (point-x x)
            (f (make-cpoint 3 4 'red)))))
    3)

  (equal?
    (let ()
      (define-record-type point (fields x y))
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type cpoint (nongenerative)
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (cpoint-rgb x)
            (f (make-cpoint 3 4 'red)))))
    '(rgb . red))

  (eqv?
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative))
      (define-record-type cpoint (nongenerative)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (point-x x)
          (f (make-cpoint 3 4 'red))))
    3)

  (equal?
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative))
      (define-record-type cpoint (nongenerative)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (cpoint-rgb x)
          (f (make-cpoint 3 4 'red))))
    '(rgb . red))

  (eqv?
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative point0001))
      (define-record-type cpoint
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (point-x x)
          (f (make-cpoint 3 4 'red))))
    3)

  (error? ; not a cpoint
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative point0002))
      (define-record-type cpoint
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (cpoint-rgb x)
          (f (make-cpoint 3 4 'red)))))

  (error? ; incompatible record type
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y))
      (define-record-type cpoint (nongenerative cpoint0003)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (point-x x)
          (f (make-cpoint 3 4 'red)))))

  (error? ; incompatible record type
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y))
      (define-record-type cpoint (nongenerative cpoint0004)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (cpoint-rgb x)
          (f (make-cpoint 3 4 'red)))))

  (eqv?
    (let ()
      (define-record-type point (fields x y))
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type cpoint (nongenerative cpoint0005)
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (point-x x)
            (f (make-cpoint 3 4 'red)))))
    3)

  (equal?
    (let ()
      (define-record-type point (fields x y))
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type cpoint (nongenerative cpoint0006)
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (cpoint-rgb x)
            (f (make-cpoint 3 4 'red)))))
    '(rgb . red))

  (eqv?
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative point0007))
      (define-record-type cpoint (nongenerative cpoint0008)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (point-x x)
          (f (make-cpoint 3 4 'red))))
    3)

  (equal?
    (let f ([x #f])
      (define (color->rgb c) (cons 'rgb c))
      (define-record-type point (fields x y) (nongenerative point0009))
      (define-record-type cpoint (nongenerative cpoint0010)
        (parent point)
        (fields (mutable rgb))
        (protocol
          (lambda (n)
            (lambda (x y c) 
              ((n x y) (color->rgb c))))))
      (if x 
          (cpoint-rgb x)
          (f (make-cpoint 3 4 'red))))
    '(rgb . red))

 ; make sure we can use arbitrary symbols as uids w/o destroying bindings
  (equal?
    ($record->vector
      (let ()
        (define-record-type foo (fields x) (nongenerative cons))
        (make-foo (cons 17 3))))
    '#(foo (17 . 3)))
  (equal? (cons 17 3) '(17 . 3))

 ; make sure we can use modifiers and types as field names
  (equal?
    (let ()
      (define-record-type foo (fields (mutable mutable) (immutable int) (immutable char) (mutable integer-32)))
      (let ([x (make-foo 3 4 5 6)])
        (foo-mutable-set! x 75)
        (list ($record->vector x) (foo-mutable x) (foo-int x) (foo-char x) (foo-integer-32 x))))
    '(#(foo 75 4 5 6) 75 4 5 6))

  (begin (print-record #t) (print-record))

 ; optimization tests---observe with expand/optimize
  (equal?
    (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
     ; try define-record-type
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type point (fields x y))
        (define-record-type cpoint
          (parent point)
          (fields (mutable rgb)))
        (if x 
            (list x (cpoint? x) (make-point -8 -15))
            (f (make-cpoint 3 4 (color->rgb 'red))))))
    '(#(cpoint 3 4 (rgb . red)) #f #(point -8 -15)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y))
           (define-record-type cpoint
             (parent point)
             (fields (mutable rgb)))
           (if x 
               (list x (cpoint? x) (make-point -8 -15))
               (f (make-cpoint 3 4 (color->rgb 'red)))))))
    '(lambda ()
       (let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'point #f
                    #f #f #f '#2((immutable x) (immutable y))
                    'define-record-type)])
         (let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'cpoint rtd
                      #f #f #f '#1((mutable rgb))
                      'define-record-type)])
           (if x
               (#2%list x (#3%record? x rtd) (#3%$record rtd -8 -15))
               (f (#3%$record rtd 3 4 (#2%cons 'rgb 'red))))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y))
           (define-record-type cpoint
             (parent point)
             (fields (mutable rgb)))
           (if x 
               (list x (cpoint? x) (make-point -8 -15))
               (f (make-cpoint 3 4 (color->rgb 'red)))))))
      '(lambda ()
         (let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'point #f
                      #f #f #f '#2((immutable x) (immutable y))
                      'define-record-type)])
           (let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'cpoint rtd
                        #f #f #f '#1((mutable rgb))
                        'define-record-type)])
             (if x
                 (#3%list x (#3%record? x rtd) (#3%$record rtd -8 -15))
                 (f (#3%$record rtd 3 4 (#3%cons 'rgb 'red))))))))
  (equal?
    (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
     ; same but nongenerative w/accessor call
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type point (fields x y) (nongenerative))
        (define-record-type cpoint
          (nongenerative)
          (parent point)
          (fields (mutable rgb)))
        (if x 
            (list x (cpoint-rgb x) (make-point -8 -15))
            (f (make-cpoint 3 4 (color->rgb 'red))))))
    '(#(cpoint 3 4 (rgb . red)) (rgb . red) #(point -8 -15)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y) (nongenerative))
           (define-record-type cpoint
             (nongenerative)
             (parent point)
             (fields (mutable rgb)))
           (if x 
               (list x (cpoint-rgb x) (make-point -8 -15))
               (f (make-cpoint 3 4 (color->rgb 'red)))))))
    `(lambda ()
       (if x
           (#2%list
             x
             (let ([g12 x])
               (if (#3%record? g12 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'cpoint-rgb g12
                     ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g12 ,fixnum?))
             ',record?)
           (f (#3%$record ',record-type-descriptor? 3 4 (#2%cons 'rgb 'red))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y) (nongenerative))
           (define-record-type cpoint
             (nongenerative)
             (parent point)
             (fields (mutable rgb)))
           (if x 
               (list x (cpoint-rgb x) (make-point -8 -15))
               (f (make-cpoint 3 4 (color->rgb 'red)))))))
    `(lambda ()
       (if x
           (#3%list x (#3%$object-ref 'scheme-object x ,fixnum?) ',record?)
           (f (#3%$record ',record-type-descriptor? 3 4 (#3%cons 'rgb 'red))))))
  (equal?
    (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
     ; same but with child protocol
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type point (fields x y))
        (define-record-type cpoint
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (list x (cpoint? x) (make-point -8 -15))
            (f (make-cpoint 3 4 'red)))))
    '(#(cpoint 3 4 (rgb . red)) #f #(point -8 -15)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y))
           (define-record-type cpoint
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint? x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    '(lambda ()
       (let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'point #f
                    #f #f #f '#2((immutable x) (immutable y))
                    'define-record-type)])
         (let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'cpoint rtd
                      #f #f #f '#1((mutable rgb))
                      'define-record-type)])
           (if x
               (#2%list x
                 (#3%record? x rtd)
                 (#3%$record rtd -8 -15))
               (f (#3%$record rtd 3 4 (#2%cons 'rgb 'red))))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y))
           (define-record-type cpoint
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint? x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    '(lambda ()
       (let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'point #f
                    #f #f #f '#2((immutable x) (immutable y))
                    'define-record-type)])
         (let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'cpoint rtd
                      #f #f #f '#1((mutable rgb))
                      'define-record-type)])
           (if x
               (#3%list x
                 (#3%record? x rtd)
                 (#3%$record rtd -8 -15))
               (f (#3%$record rtd 3 4 (#3%cons 'rgb 'red))))))))
  (equal?
    (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
     ; same but nongenerative w/accessor call
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type point (fields x y) (nongenerative point0009))
        (define-record-type cpoint (nongenerative cpoint0010)
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (list x (cpoint-rgb x) (make-point -8 -15))
            (f (make-cpoint 3 4 'red)))))
    '(#(cpoint 3 4 (rgb . red)) (rgb . red) #(point -8 -15)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y) (nongenerative point0009))
           (define-record-type cpoint (nongenerative cpoint0010)
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint-rgb x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    `(lambda ()
       (if x
           (#2%list
             x
             (let ([g35 x])
               (if (#3%record? g35 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'cpoint-rgb g35
                     ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g35 ,fixnum?))
             ',record?)
           (f (#3%$record ',record-type-descriptor? 3 4 (#2%cons 'rgb 'red))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point (fields x y) (nongenerative point0009))
           (define-record-type cpoint (nongenerative cpoint0010)
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint-rgb x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    `(lambda ()
       (if x
           (#3%list
             x
             (#3%$object-ref 'scheme-object x ,fixnum?)
             ',record?)
           (f (#3%$record ',record-type-descriptor? 3 4 (#3%cons 'rgb 'red))))))
  (equal?
    (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
     ; same as two above but with trivial parent protocol
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type point
          (fields x y)
          (protocol (lambda (n) n)))
        (define-record-type cpoint
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (list x (cpoint? x) (make-point -8 -15))
            (f (make-cpoint 3 4 'red)))))
    '(#(cpoint 3 4 (rgb . red)) #f #(point -8 -15)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point
             (fields x y)
             (protocol (lambda (n) n)))
           (define-record-type cpoint
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                    (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint? x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    '(lambda ()
       (let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'point #f
                    #f #f #f '#2((immutable x) (immutable y))
                    'define-record-type)])
         (let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'cpoint rtd
                      #f #f #f '#1((mutable rgb))
                      'define-record-type)])
           (if x
               (#2%list
                 x
                 (#3%record? x rtd)
                 (#3%$record rtd -8 -15))
               (f (#3%$record rtd 3 4 (#2%cons 'rgb 'red))))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point
             (fields x y)
             (protocol (lambda (n) n)))
           (define-record-type cpoint
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                    (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint? x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    '(lambda ()
       (let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'point #f
                    #f #f #f '#2((immutable x) (immutable y))
                    'define-record-type)])
         (let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'cpoint rtd
                      #f #f #f '#1((mutable rgb))
                      'define-record-type)])
           (if x
               (#3%list x
                 (#3%record? x rtd)
                 (#3%$record rtd -8 -15))
               (f (#3%$record rtd 3 4 (#3%cons 'rgb 'red))))))))
  (equal?
    (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
     ; same but nongenerative w/accessor call
      (let f ([x #f])
        (define (color->rgb c) (cons 'rgb c))
        (define-record-type point
          (fields x y)
          (nongenerative point0009)
          (protocol (lambda (n) n)))
        (define-record-type cpoint (nongenerative cpoint0010)
          (parent point)
          (fields (mutable rgb))
          (protocol
            (lambda (n)
              (lambda (x y c) 
                ((n x y) (color->rgb c))))))
        (if x 
            (list x (cpoint-rgb x) (make-point -8 -15))
            (f (make-cpoint 3 4 'red)))))
    '(#(cpoint 3 4 (rgb . red)) (rgb . red) #(point -8 -15)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point
             (fields x y)
             (nongenerative point0009)
             (protocol (lambda (n) n)))
           (define-record-type cpoint (nongenerative cpoint0010)
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint-rgb x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    `(lambda ()
       (if x
           (#2%list
             x
             (let ([g57 x])
               (if (#3%record? g57 ',record-type-descriptor?)
                   (#2%void)
                   (#3%$record-oops 'cpoint-rgb g57
                     ',record-type-descriptor?))
               (#3%$object-ref 'scheme-object g57 ,fixnum?))
             ',record?)
           (f (#3%$record ',record-type-descriptor? 3 4 (#2%cons 'rgb 'red))))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda ()
           (define (color->rgb c) (cons 'rgb c))
           (define-record-type point
             (fields x y)
             (nongenerative point0009)
             (protocol (lambda (n) n)))
           (define-record-type cpoint (nongenerative cpoint0010)
             (parent point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) (color->rgb c))))))
           (if x 
               (list x (cpoint-rgb x) (make-point -8 -15))
               (f (make-cpoint 3 4 'red))))))
    `(lambda ()
       (if x
           (#3%list x
             (#3%$object-ref 'scheme-object x ,fixnum?)
             ',record?)
           (f (#3%$record ',record-type-descriptor? 3 4 (#3%cons 'rgb 'red))))))
  (begin
   ; test global define-record-type
    (define ($color->rgb c) (cons 'rgb c))
    (define-record-type ($point $make-point $point?)
      (fields x y))
    (define-record-type ($cpoint $make-cpoint $cpoint?)
      (parent $point)
      (fields (mutable rgb)))
    (equal?
      (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
        (let ([x ($make-cpoint 3 4 ($color->rgb 'red))])
          (list x ($cpoint-rgb x) ($make-point -8 -15))))
      '(#($cpoint 3 4 (rgb . red)) (rgb . red) #($point -8 -15))))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin
          ; test global define-record-type
           (define ($color->rgb c) (cons 'rgb c))
           (define-record-type ($point $make-point $point?)
             (fields x y))
           (define-record-type ($cpoint $make-cpoint $cpoint?)
             (parent $point)
             (fields (mutable rgb)))
           (equal?
             (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
               (let ([x ($make-cpoint 3 4 ($color->rgb 'red))])
                 (list x ($cpoint-rgb x) ($make-point -8 -15))))
             '(#($cpoint 3 4 (rgb . red)) (rgb . red) #($point -8 -15))))))
    `(begin
       (set! $color->rgb (lambda (c) (#2%cons 'rgb c)))
       (set! $make-point
         (lambda (g73 g74)
           (#3%$record ',record-type-descriptor? g73 g74)))
       (set! $point?
         (lambda (g72)
           (#3%record? g72 ',record-type-descriptor?)))
       (set! $point-x
         (lambda (g71)
           (if (#3%record? g71 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g71
                 ',record-type-descriptor?))
           (#3%$object-ref 'scheme-object g71 ,fixnum?)))
       (set! $point-y
         (lambda (g70)
           (if (#3%record? g70 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g70
                 ',record-type-descriptor?))
           (#3%$object-ref 'scheme-object g70 ,fixnum?)))
       (set! $make-cpoint
         (lambda (g67 g68 g69)
           (#3%$record ',record-type-descriptor? g67 g68 g69)))
       (set! $cpoint?
         (lambda (g66)
           (#3%record? g66 ',record-type-descriptor?)))
       (set! $cpoint-rgb
         (lambda (g65)
           (if (#3%record? g65 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g65
                 ',record-type-descriptor?))
           (#3%$object-ref 'scheme-object g65 ,fixnum?)))
       (set! $cpoint-rgb-set!
         (lambda (g63 g64)
           (if (#3%record? g63 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g63
                 ',record-type-descriptor?))
           (#3%$object-set! 'scheme-object g63 ,fixnum? g64)))
       (#2%equal?
         (#2%map
           (lambda (x) (if (#2%$record? x) ($record->vector x) x))
           (let ([x ($make-cpoint 3 4 ($color->rgb 'red))])
             (#2%list x ($cpoint-rgb x) ($make-point -8 -15))))
         '(#4($cpoint 3 4 (rgb . red)) (rgb . red) #3($point -8 -15)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin
          ; test global define-record-type
           (define ($color->rgb c) (cons 'rgb c))
           (define-record-type ($point $make-point $point?)
             (fields x y))
           (define-record-type ($cpoint $make-cpoint $cpoint?)
             (parent $point)
             (fields (mutable rgb)))
           (equal?
             (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
               (let ([x ($make-cpoint 3 4 ($color->rgb 'red))])
                 (list x ($cpoint-rgb x) ($make-point -8 -15))))
             '(#($cpoint 3 4 (rgb . red)) (rgb . red) #($point -8 -15))))))
    `(begin
       (set! $color->rgb (lambda (c) (#3%cons 'rgb c)))
       (set! $make-point
         (lambda (g109 g110)
           (#3%$record ',record-type-descriptor? g109 g110)))
       (set! $point?
         (lambda (g108)
           (#3%record? g108 ',record-type-descriptor?)))
       (set! $point-x
         (lambda (g107) (#3%$object-ref 'scheme-object g107 ,fixnum?)))
       (set! $point-y
         (lambda (g106) (#3%$object-ref 'scheme-object g106 ,fixnum?)))
       (set! $make-cpoint
         (lambda (g103 g104 g105)
           (#3%$record ',record-type-descriptor? g103 g104 g105)))
       (set! $cpoint?
         (lambda (g102)
           (#3%record? g102 ',record-type-descriptor?)))
       (set! $cpoint-rgb
         (lambda (g101)
           (#3%$object-ref 'scheme-object g101 ,fixnum?)))
       (set! $cpoint-rgb-set!
         (lambda (g99 g100)
           (#3%$object-set! 'scheme-object g99 ,fixnum? g100)))
       (#3%equal?
         (#3%map
           (lambda (x) (if (#3%$record? x) ($record->vector x) x))
           (let ([x ($make-cpoint 3 4 ($color->rgb 'red))])
             (#3%list x ($cpoint-rgb x) ($make-point -8 -15))))
         '(#4($cpoint 3 4 (rgb . red)) (rgb . red) #3($point -8 -15)))))
  (begin
   ; test global define-record-type
    (define ($color->rgb c) (cons 'rgb c))
    (define-record-type ($point $make-point $point?)
      (fields x y)
      (protocol (lambda (n) n)))
    (define-record-type ($cpoint $make-cpoint $cpoint?)
      (parent $point)
      (fields (mutable rgb))
      (protocol
        (lambda (n)
          (lambda (x y c) 
            ((n x y) ($color->rgb c))))))
    (equal?
      (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
        (let ([x ($make-cpoint 3 4 'red)])
          (list x ($cpoint-rgb x) ($make-point -8 -15))))
      '(#($cpoint 3 4 (rgb . red)) (rgb . red) #($point -8 -15))))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin
          ; test global define-record-type
           (define ($color->rgb c) (cons 'rgb c))
           (define-record-type ($point $make-point $point?)
             (fields x y)
             (protocol (lambda (n) n)))
           (define-record-type ($cpoint $make-cpoint $cpoint?)
             (parent $point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) ($color->rgb c))))))
           (equal?
             (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
               (let ([x ($make-cpoint 3 4 'red)])
                 (list x ($cpoint-rgb x) ($make-point -8 -15))))
             '(#($cpoint 3 4 (rgb . red)) (rgb . red) #($point -8 -15))))))
    `(begin
       (set! $color->rgb (lambda (c) (#2%cons 'rgb c)))
       (letrec ([g7 (lambda (n) n)])
         (#3%$set-top-level-value! 'rcd1
           (#3%$make-record-constructor-descriptor
             ',record-type-descriptor? #f g7 'define-record-type)))
       (set! $make-point (#2%r6rs:record-constructor (#2%$top-level-value 'rcd1)))
       (set! $point?
         (lambda (g153)
           (#3%record? g153 ',record-type-descriptor?)))
       (set! $point-x
         (lambda (g152)
           (if (#3%record? g152 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g152
                 ',record-type-descriptor?))
           (#3%$object-ref 'scheme-object g152 ,fixnum?)))
       (set! $point-y
         (lambda (g151)
           (if (#3%record? g151 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g151
                 ',record-type-descriptor?))
           (#3%$object-ref 'scheme-object g151 ,fixnum?)))
       (#3%$set-top-level-value! 'rcd2
         (#2%$make-record-constructor-descriptor
           ',record-type-descriptor? (#2%$top-level-value 'rcd1)
           (lambda (n) (lambda (x y c) ((n x y) ($color->rgb c))))
           'define-record-type))
       (set! $make-cpoint (#2%r6rs:record-constructor (#2%$top-level-value 'rcd2)))
       (set! $cpoint?
         (lambda (g150)
           (#3%record? g150 ',record-type-descriptor?)))
       (set! $cpoint-rgb
         (lambda (g149)
           (if (#3%record? g149 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g149
                 ',record-type-descriptor?))
           (#3%$object-ref 'scheme-object g149 ,fixnum?)))
       (set! $cpoint-rgb-set!
         (lambda (g147 g148)
           (if (#3%record? g147 ',record-type-descriptor?)
               (#2%void)
               (#3%$record-oops 'moi g147
                 ',record-type-descriptor?))
           (#3%$object-set! 'scheme-object g147 ,fixnum? g148)))
       (#2%equal?
         (#2%map
           (lambda (x) (if (#2%$record? x) ($record->vector x) x))
           (let ([x ($make-cpoint 3 4 'red)])
             (#2%list x ($cpoint-rgb x) ($make-point -8 -15))))
         '(#4($cpoint 3 4 (rgb . red)) (rgb . red) #3($point -8 -15)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(begin
          ; test global define-record-type
           (define ($color->rgb c) (cons 'rgb c))
           (define-record-type ($point $make-point $point?)
             (fields x y)
             (protocol (lambda (n) n)))
           (define-record-type ($cpoint $make-cpoint $cpoint?)
             (parent $point)
             (fields (mutable rgb))
             (protocol
               (lambda (n)
                 (lambda (x y c) 
                   ((n x y) ($color->rgb c))))))
           (equal?
             (map (lambda (x) (if (#%$record? x) ($record->vector x) x))
               (let ([x ($make-cpoint 3 4 'red)])
                 (list x ($cpoint-rgb x) ($make-point -8 -15))))
             '(#($cpoint 3 4 (rgb . red)) (rgb . red) #($point -8 -15))))))
    `(begin
       (set! $color->rgb (lambda (c) (#3%cons 'rgb c)))
       (letrec ([g7 (lambda (n) n)])
         (#3%$set-top-level-value! 'rcd1
           (#3%$make-record-constructor-descriptor
             ',record-type-descriptor? #f g7 'define-record-type)))
       (set! $make-point (#3%r6rs:record-constructor (#3%$top-level-value 'rcd1)))
       (set! $point?
         (lambda (g129)
           (#3%record? g129 ',record-type-descriptor?)))
       (set! $point-x
         (lambda (g128) (#3%$object-ref 'scheme-object g128 ,fixnum?)))
       (set! $point-y
         (lambda (g127) (#3%$object-ref 'scheme-object g127 ,fixnum?)))
       (#3%$set-top-level-value! 'rcd2
         (#3%$make-record-constructor-descriptor ',record-type-descriptor?
           (#3%$top-level-value 'rcd1)
           (lambda (n) (lambda (x y c) ((n x y) ($color->rgb c))))
           'define-record-type))
       (set! $make-cpoint (#3%r6rs:record-constructor (#3%$top-level-value 'rcd2)))
       (set! $cpoint?
         (lambda (g126)
           (#3%record? g126 ',record-type-descriptor?)))
       (set! $cpoint-rgb
         (lambda (g125)
           (#3%$object-ref 'scheme-object g125 ,fixnum?)))
       (set! $cpoint-rgb-set!
         (lambda (g123 g124)
           (#3%$object-set! 'scheme-object g123 ,fixnum? g124)))
       (#3%equal?
         (#3%map
           (lambda (x) (if (#3%$record? x) ($record->vector x) x))
           (let ([x ($make-cpoint 3 4 'red)])
             (#3%list x ($cpoint-rgb x) ($make-point -8 -15))))
         '(#4($cpoint 3 4 (rgb . red)) (rgb . red) #3($point -8 -15)))))
  (error? ; can't handle define-record-type parent
    (let ()
      (define-record-type fratrat)
      (define-record dormy fratrat ())))
  (error? ; can't handle define-record parent
    (let ()
      (define-record fratrat ())
      (define-record-type dormy (parent fratrat))))
  (equal?
    (let ()
      (define-record fratrat ())
      (define-record-type dormy
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f)))
      (let ([x (make-fratrat)] [y (make-dormy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y))))
    '(#t #f #t #t))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat ())
           (define-record-type dormy
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f)))
           (let ([x (make-fratrat)] [y (make-dormy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y))))))
    `(begin
       (#2%$make-record-type-descriptor #!base-rtd 'dormy
         ',record-type-descriptor? #f #f #f '#0()
         'define-record-type)
       (#2%list #t #f #t #t)))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat ())
           (define-record-type dormy
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f)))
           (let ([x (make-fratrat)] [y (make-dormy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y))))))
    `(#3%list #t #f #t #t))
  (equal?
    (let ()
      (define-record fratrat (x))
      (define-record-type dormy
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y))))
    '(#t #f #t #t 17 23 creepy))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat (x))
           (define-record-type dormy
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
             (fields (immutable y)))
           (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'dormy
                  ',record-type-descriptor? #f #f #f
                  '#1((immutable y)) 'define-record-type)])
       (let ([x (#3%$record ',record-type-descriptor? 17)]
             [y (#3%$record rtd 23 'creepy)])
         (#2%list #t #f #t #t
           (#3%$object-ref 'scheme-object x ,fixnum?)
           (#3%$object-ref 'scheme-object y ,fixnum?)
           'creepy))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat (x))
           (define-record-type dormy
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
             (fields (immutable y)))
           (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'dormy
                  ',record-type-descriptor? #f #f #f
                  '#1((immutable y)) 'define-record-type)])
       (let ([x (#3%$record ',record-type-descriptor? 17)]
             [y (#3%$record rtd 23 'creepy)])
         (#3%list #t #f #t #t
           (#3%$object-ref 'scheme-object x ,fixnum?)
           (#3%$object-ref 'scheme-object y ,fixnum?)
           'creepy))))
  (equal?
    (let () ; add a protocol
      (define-record fratrat (x))
      (define-record-type dormy
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y))
        (protocol (lambda (p) (lambda (q) ((p (car q)) q)))))
      (let ([x (make-fratrat 17)] [y (make-dormy '(23 creepy))])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y))))
    '(#t #f #t #t 17 23 (23 creepy)))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat (x))
           (define-record-type dormy
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
             (fields (immutable y))
             (protocol (lambda (p) (lambda (q) ((p (car q)) q)))))
           (let ([x (make-fratrat 17)] [y (make-dormy '(23 creepy))])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'dormy
                  ',record-type-descriptor? #f #f #f
                  '#1((immutable y)) 'define-record-type)])
       (let ([x (#3%$record ',record-type-descriptor? 17)]
             [y (#3%$record rtd 23 '(23 creepy))])
         (#2%list #t #f #t #t
           (#3%$object-ref 'scheme-object x ,fixnum?)
           (#3%$object-ref 'scheme-object y ,fixnum?)
           '(23 creepy)))))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat (x))
           (define-record-type dormy
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
             (fields (immutable y))
             (protocol (lambda (p) (lambda (q) ((p (car q)) q)))))
           (let ([x (make-fratrat 17)] [y (make-dormy '(23 creepy))])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'dormy
                  ',record-type-descriptor? #f #f #f
                  '#1((immutable y)) 'define-record-type)])
       (let ([x (#3%$record ',record-type-descriptor? 17)]
             [y (#3%$record rtd 23 '(23 creepy))])
         (#3%list #t #f #t #t
           (#3%$object-ref 'scheme-object x ,fixnum?)
           (#3%$object-ref 'scheme-object y ,fixnum?)
           '(23 creepy)))))
  (error? ; m-r-c-d can't handle non-scheme-object fields
    (let ()
      (define-record fratrat ((immutable integer-32 x)))
      (define-record-type dormy
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (equal?
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type dormy
        (nongenerative)
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y))))
    '(#t #f #t #t 17 23 creepy))
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat ((immutable x)))
           (define-record-type dormy
             (nongenerative)
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
             (fields (immutable y)))
           (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(#2%list #t #f #t #t 17 23 'creepy))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat ((immutable x)))
           (define-record-type dormy
             (nongenerative)
             (parent-rtd
               (type-descriptor fratrat)
               (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
             (fields (immutable y)))
           (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(#3%list #t #f #t #t 17 23 'creepy))
  (equal?
    (let ()
      (define-record fratrat ((immutable x)))
      (define dormy (make-record-type (type-descriptor fratrat) '#{dormy a3utgl1aoz8jzrg1-0} '((immutable y))))
      (define make-dormy (record-constructor dormy))
      (define dormy? (record-predicate dormy))
      (define dormy-y (record-accessor dormy 0))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y))))
    '(#t #f #t #t 17 23 creepy))
  (equivalent-expansion? ; optimize-level 2 expansion of above (note dormy gensym must be different)
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat ((immutable x)))
           (define dormy (make-record-type (type-descriptor fratrat) '#{dormy a3utgl1aoz8jzrg1-1} '((immutable y))))
           (define make-dormy (record-constructor dormy))
           (define dormy? (record-predicate dormy))
           (define dormy-y (record-accessor dormy 0))
           (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(#2%list #t #f #t #t 17 23 'creepy))
  (equivalent-expansion? ; optimize-level 2 expansion of above (note dormy gensym must be different)
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record fratrat ((immutable x)))
           (define dormy (make-record-type (type-descriptor fratrat) '#{dormy a3utgl1aoz8jzrg1-2} '((immutable y))))
           (define make-dormy (record-constructor dormy))
           (define dormy? (record-predicate dormy))
           (define dormy-y (record-accessor dormy 0))
           (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
             (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
               (fratrat-x x) (fratrat-x y) (dormy-y y))))))
    `(#3%list #t #f #t #t 17 23 'creepy))
  (error? ; can't have both parent and parent-rtd
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have both parent and parent-rtd
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (parent fratrat2)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have two parent-rtd clauses
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type dormy
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (parent-rtd
          (type-descriptor fratrat)
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have two parent clauses
    (let ()
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (parent fratrat2)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have two fields clauses
    (let ()
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (fields z)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have two nongenerative clauses
    (let ()
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (nongenerative)
        (nongenerative spam-for-dinner)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have two sealed? clauses
    (let ()
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (sealed #t)
        (sealed #t)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have two opaque? clauses
    (let ()
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (opaque #t)
        (opaque #t)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; can't have two protocol clauses
    (let ()
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (protocol values)
        (protocol (lambda (x) x))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; protocol expression doesn't evaluate to a procedure
    (let ()
      (define-record-type fratrat2 (fields (immutable x)))
      (define-record-type dormy
        (parent fratrat2)
        (protocol 'whoops!)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; not an rcd
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type dormy
        (parent-rtd
          (type-descriptor fratrat)
          'rats)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (error? ; not an rtd
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type dormy
        (parent-rtd 'rats
          (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (fratrat-x y) (dormy-y y)))))
  (equal?
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type dormy
        (parent-rtd #f #f)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (dormy-y y))))
    '(#t #f #f #t 17 creepy))
  (equal?
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type dormy
        (parent-rtd (record-type-descriptor fratrat) #f)
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 23 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (dormy-y y))))
    '(#t #f #t #t 17 creepy))
  (error? ; "can't specify rcd w/o rtd"
    (let ()
      (define-record fratrat ((immutable x)))
      (define-record-type dormy
        (parent-rtd #f (make-record-constructor-descriptor (type-descriptor fratrat) #f #f))
        (fields (immutable y)))
      (let ([x (make-fratrat 17)] [y (make-dormy 'creepy)])
        (list (fratrat? x) (dormy? x) (fratrat? y) (dormy? y)
          (fratrat-x x) (dormy-y y)))))
  (error? ; invalid syntax
    (define-record-type (fields x)))
  (error? ; invalid clause
    (define-record-type foo (x)))
  (error? ; invalid clause
    (define-record-type foo (fields . x)))
  (error? ; invalid field
    (define-record-type foo (fields (mutable flyboy flyboy))))
  (error? ; invalid field
    (define-record-type foo (fields (immutable flyboy flyboy flyboy!))))
  (error? ; invalid field
    (define-record-type foo (fields (ugly flyboy))))
  (error? ; invalid clause
    (define-record-type foo (nongenerative 'spam)))
  (error? ; cannot handle record name defined by define-record
    (let ()
      (define-record frob ())
      (record-constructor-descriptor frob)))
  (error? ; invalid protocol value
    (define-record-type frob (protocol 'oops)))
  (let ()
    (define-record-type foo (nongenerative #{rats c9zu8koxo8gppgp-a}))
    (define-record-type bar (nongenerative #{rats c9zu8koxo8gppgp-a}))
    (and
      (eqv? (type-descriptor foo) (type-descriptor bar))
      (foo? (make-bar))
      (bar? (make-foo))))

 ; test for appropriate choice of pretty names for uids
  ((lambda (x y) (and (gensym? x) (equal? (symbol->string x) y)))
    (let ()
      (define-record-type foo)
      (record-type-uid (record-type-descriptor foo)))
    "foo")

 ; test for appropriate choice of pretty names for uids
  ((lambda (x y) (and (gensym? x) (equal? (symbol->string x) y)))
    (let ()
      (define-record-type (foo xfoo yfoo))
      (record-type-uid (record-type-descriptor foo)))
    "foo")

 ; test for appropriate choice of pretty names for uids
  ((lambda (x y) (and (gensym? x) (equal? (symbol->string x) y)))
    (let ()
      (define-record-type foo (nongenerative))
      (record-type-uid (record-type-descriptor foo)))
    "foo")

 ; test for appropriate choice of pretty names for uids
  ((lambda (x y) (and (gensym? x) (equal? (symbol->string x) y)))
    (let ()
      (define-record-type (foo xfoo yfoo) (nongenerative))
      (record-type-uid (record-type-descriptor foo)))
    "foo")

  (eqv?
    (let ()
      (define-record-type bar)
      (record-type-sealed? (record-type-descriptor bar)))
    #f)
  (eqv?
    (let ()
      (define-record-type bar (sealed #t))
      (record-type-sealed? (record-type-descriptor bar)))
    #t)
  (equivalent-expansion? ; optimize-level 2 expansion of above
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type bar (sealed #t))
           (record-type-sealed? (record-type-descriptor bar)))))
    '(begin
       (#2%$make-record-type-descriptor #!base-rtd 'bar #f #f #t #f '#() 'define-record-type)
       #t))
  (equivalent-expansion? ; optimize-level 3 expansion of above
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type bar (sealed #t))
           (record-type-sealed? (record-type-descriptor bar)))))
    '#t)
  (eqv?
    (let ()
      (define-record-type bar (sealed #t))
      (record? (make-bar)))
    #t)
  (eqv?
    (let ()
      (define-record-type bar (sealed #t))
      (r6rs:record? (make-bar)))
    #t)
  (eqv?
    (let ()
      (define-record-type bar (sealed #t))
      (record? (make-bar) (record-type-descriptor bar)))
    #t)
  (eqv?
    (let ()
      (define-record-type prnt)
      (define-record-type chld (parent prnt))
      (record? (make-chld) (record-type-descriptor prnt)))
    #t)
  (error? ; parent sealed
    (let ()
      (define-record-type prnt (sealed #t))
      (define-record-type chld (parent prnt))
      (record? (make-chld) (record-type-descriptor prnt))))
  (eqv?
    (let ()
      (define-record-type prnt)
      (define-record-type chld (parent prnt))
      (define-record-type xftr)
      (record? (make-xftr) (record-type-descriptor prnt)))
    #f)
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f] [compile-profile #f])
      (expand/optimize
        '(lambda (x)
           (define-record-type bar)
           (record? x (record-type-descriptor bar)))))
    '(lambda (x)
       (#3%record? x (#2%$make-record-type-descriptor #!base-rtd 'bar #f #f #f #f '#() 'define-record-type))))

  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f] [compile-profile #f])
      (expand/optimize
        '(lambda (x)
           (define-record-type bar (sealed #t))
           (record? x (record-type-descriptor bar)))))
    '(lambda (x)
       (#3%$sealed-record? x (#2%$make-record-type-descriptor #!base-rtd 'bar #f #f #t #f '#() 'define-record-type))))

  (equal?
    ($record->vector
      (let ()
        (define-record-type A
          (nongenerative)
          (fields))
        (define-record-type B
          (nongenerative)
          (parent A)
          (fields z)
          (protocol (lambda (make) (lambda (z) ((make) z)))))
        (define-record-type C
          (nongenerative)
          (parent B)
          (fields)
          (protocol (lambda (make) (lambda (z) ((make z))))))
        (make-C 4)))
    '#(C 4))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type A
             (nongenerative)
             (fields))
           (define-record-type B
             (nongenerative)
             (parent A)
             (fields z)
             (protocol (lambda (make) (lambda (z) ((make) z)))))
           (define-record-type C
             (nongenerative)
             (parent B)
             (fields)
             (protocol (lambda (make) (lambda (z) ((make z))))))
           (make-C 4))))
    `',record?)

  (equal?
    ($record->vector
      (let ()
        (define-record-type A
          (nongenerative)
          (fields))
        (define-record-type B
          (nongenerative)
          (parent A)
          (fields z)
          (protocol (lambda (make) (lambda () ((make) 0)))))
        (define-record-type C
          (nongenerative)
          (parent B)
          (fields)
          (protocol (lambda (make) (lambda () ((make))))))
        (make-C)))
    '#(C 0))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type A
             (nongenerative)
             (fields))
           (define-record-type B
             (nongenerative)
             (parent A)
             (fields z)
             (protocol (lambda (make) (lambda () ((make) 0)))))
           (define-record-type C
             (nongenerative)
             (parent B)
             (fields)
             (protocol (lambda (make) (lambda () ((make))))))
           (make-C))))
    `',record?)

  (equal?
    ($record->vector
      (let ()
        (define-record-type A
          (nongenerative)
          (fields))
        (define-record-type B
          (nongenerative)
          (parent A)
          (fields z)
          (protocol (lambda (make) (lambda (z) ((make) z)))))
        (define-record-type C
          (nongenerative)
          (parent B)
          (fields w)
          (protocol (lambda (make) (lambda (z) ((make z) 0)))))
        (make-C 4)))
    '#(C 4 0))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type A
             (nongenerative)
             (fields))
           (define-record-type B
             (nongenerative)
             (parent A)
             (fields z)
             (protocol (lambda (make) (lambda (z) ((make) z)))))
           (define-record-type C
             (nongenerative)
             (parent B)
             (fields w)
             (protocol (lambda (make) (lambda (z) ((make z) 0)))))
           (make-C 4))))
    `',record?)

  (equal?
    ($record->vector
      (let ()
        (define-record-type A
          (nongenerative)
          (fields))
        (define-record-type B
          (nongenerative)
          (parent A)
          (fields z)
          (protocol (lambda (make) (lambda (z) ((make) z)))))
        (define-record-type C
          (nongenerative)
          (parent B)
          (fields w q1 q2 q3)
          (protocol (lambda (make) (lambda (z) ((make z) 0 1 2 3)))))
        (make-C 4)))
    '#(C 4 0 1 2 3))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type A
             (nongenerative)
             (fields))
           (define-record-type B
             (nongenerative)
             (parent A)
             (fields z)
             (protocol (lambda (make) (lambda (z) ((make) z)))))
           (define-record-type C
             (nongenerative)
             (parent B)
             (fields w q1 q2 q3)
             (protocol (lambda (make) (lambda (z) ((make z) 0 1 2 3)))))
           (make-C 4))))
    `',record?)

 ; try hierarchy of five levels
  (equal?
    ($record->vector
      (let ()
        (define-record-type A
          (nongenerative)
          (fields))
        (define-record-type B
          (nongenerative)
          (parent A)
          (fields z)
          (protocol (lambda (make) (lambda (z) ((make) z)))))
        (define-record-type C
          (nongenerative)
          (parent B)
          (fields w q1 q2 q3)
          (protocol (lambda (make) (lambda (z) ((make z) 0 1 2 3)))))
        (define-record-type D
          (nongenerative)
          (parent C)
          (fields w)
          (protocol (lambda (make) (lambda (z w/2) ((make z) (* w/2 2))))))
        (define-record-type E
          (nongenerative)
          (parent D)
          (fields w)
          (protocol (lambda (make) (lambda (z a b) ((make z (/ a 5)) (+ a b))))))
        (make-E 3 7 11)))
    '#(E 3 0 1 2 3 14/5 18))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type A
             (nongenerative)
             (fields))
           (define-record-type B
             (nongenerative)
             (parent A)
             (fields z)
             (protocol (lambda (make) (lambda (z) ((make) z)))))
           (define-record-type C
             (nongenerative)
             (parent B)
             (fields w q1 q2 q3)
             (protocol (lambda (make) (lambda (z) ((make z) 0 1 2 3)))))
           (define-record-type D
             (nongenerative)
             (parent C)
             (fields w)
             (protocol (lambda (make) (lambda (z w/2) ((make z) (* w/2 2))))))
           (define-record-type E
             (nongenerative)
             (parent D)
             (fields w)
             (protocol (lambda (make) (lambda (z a b) ((make z (/ a 5)) (+ a b))))))
           (make-E 3 7 11))))
    `',record?)

  (begin
    (module ($drt-foo1)
      (define-record-type $drt-foo1
        (protocol (lambda (new) (lambda () (new))))))
    (define-record-type $drt-bar1
      (parent $drt-foo1)
      (protocol (lambda (make-new) (lambda () ((make-new))))))
    ($drt-bar1? (make-$drt-bar1)))
  ($drt-bar1? (make-$drt-bar1))

  (begin
    (define $drt-false #f)
    (module ($drt-foo2)
      (define-record-type $drt-foo2
        (parent-rtd $drt-false $drt-false)
        (protocol (lambda (new) (lambda () (new))))))
    (define-record-type $drt-bar2
      (parent $drt-foo2)
      (protocol (lambda (make-new) (lambda () ((make-new))))))
    ($drt-bar2? (make-$drt-bar2)))
  ($drt-bar2? (make-$drt-bar2))

  ; make sure record accessor isn't folded when applied to
  ; the wrong type of constant argument
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (b)
           (let ([x 'x])
             (define-record-type frob (nongenerative) (fields x))
             (if b (frob-x x) 72)))))
    `(lambda (b)
       (if b
           (begin
             (#3%$record-oops 'frob-x 'x ',record-type-descriptor?)
             (#3%$object-ref 'scheme-object 'x ,fixnum?))
           72)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (b)
           (let ([x 'x])
             (define-record-type frob (nongenerative) (fields x))
             (if b (frob-x x) 72)))))
    `(lambda (b)
       (if b
           (#3%$object-ref 'scheme-object 'x ,fixnum?)
           72)))
  ; ensure we're checking to make sure field names, accessors, and
  ; mutators are identifiers
  (error? ; invalid field spec
    (define-record-type foo (fields 876)))
  (error? ; invalid field spec
    (define-record-type foo (fields (mutable (x)))))
  (error? ; invalid field spec
    (define-record-type foo (fields (immutable "spam"))))
  (error? ; invalid field spec
    (define-record-type foo (fields (immutable (x) foo-x))))
  (error? ; invalid accessor name
    (define-record-type foo (fields (immutable x (foo-x)))))
  (error? ; invalid field spec
    (define-record-type foo (fields (mutable (x) foo-x foo-x!))))
  (error? ; invalid accessor name
    (define-record-type foo (fields (mutable x (foo-x) foo-x!))))
  (error? ; invalid accessor name
    (define-record-type foo (fields (mutable x foo-x (foo-x!)))))
)

(mat define-record-type-extensions
  (error? ; nongenerative clause missing
    (parameterize ([require-nongenerative-clause #t])
      (eval '
        (let ()
          (define-record-type foo)
          make-foo))))
  (procedure?
    (parameterize ([require-nongenerative-clause #t])
      (eval '
        (let ()
          (define-record-type foo (nongenerative #f))
          make-foo))))
  (procedure?
    (parameterize ([require-nongenerative-clause #t])
      (eval '
        (let ()
          (define-record-type foo (nongenerative))
          make-foo))))
  (procedure?
    (parameterize ([require-nongenerative-clause #t])
      (eval '
        (let ()
          (define-record-type foo (nongenerative #{foo e7akngbfn4x0395fvq3uor-0}))
          make-foo))))
  ((lambda (ls) (not (apply eq? ls)))
   (let ()
     (define f
       (lambda () 
         (define-record-type foo (nongenerative #f))
         (record-type-descriptor foo)))
     (list (f) (f))))
  ((lambda (ls) (apply eq? ls))
   (let ()
     (define f
       (lambda () 
         (define-record-type foo (nongenerative))
         (record-type-descriptor foo)))
     (list (f) (f))))
  )

(mat cp0-record-ref-optimizations
  (eqv?
    (let ()
      (define-record-type foo (fields x))
      (let ([x 17])
        (let ([q (make-foo x)])
          (set! x 43)
          (foo-x q))))
    17)
  (eqv?
    (let ()
      (define-record-type foo (fields x))
      (let ([x 17])
        (let ([q (make-foo x)])
          #;(set! x 43)
          (foo-x q))))
    17)
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type foo (fields x))
           (let ([x 17])
             (let ([q (make-foo x)])
               #;(set! x 43)
               (foo-x q))))))
    17)
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record-type foo (fields x))
           (let ([x 17])
             (let ([q (make-foo x)])
               (set! x 43)
               (foo-x q))))))
    `(let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'foo #f #f #f #f '#((immutable x))
                  'define-record-type)])
       (let ([x 17])
         (let ([q (#3%$record rtd x)])
           (set! x 43)
           (#3%$object-ref 'scheme-object q ,fixnum?)))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (fields x y))
           (let ([q (make-foo a 3)])
             (list (foo-x q) (foo-y q))))))
    '(lambda (a)
       (#2%$make-record-type-descriptor #!base-rtd 'foo #f #f #f #f '#((immutable x) (immutable y))
         'define-record-type)
       (#2%list a 3)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (fields x y))
           (let ([q (make-foo a 3)])
             (list (foo-x q) (foo-y q))))))
    '(lambda (a) (#3%list a 3)))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (nongenerative) (fields x y))
           (let ([q (make-foo a 3)])
             (list (foo-x q) (foo-y q))))))
    '(lambda (a) (#2%list a 3)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (nongenerative) (fields x y))
           (let ([q (make-foo a 3)])
             (list (foo-x q) (foo-y q))))))
    '(lambda (a) (#3%list a 3)))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (fields x y))
           (let ([q (make-foo (cons a a) (lambda () a))])
             (list (foo-x q) ((foo-y q)))))))
    '(lambda (a)
       (#2%$make-record-type-descriptor #!base-rtd 'foo #f #f #f #f '#((immutable x) (immutable y))
         'define-record-type)
       (#2%list (#2%cons a a) a)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (fields x y))
           (let ([q (make-foo (cons a a) (lambda () a))])
             (list (foo-x q) ((foo-y q)))))))
    '(lambda (a) (#3%list (#3%cons a a) a)))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (nongenerative) (fields x y))
           (let ([q (make-foo (cons a a) (lambda () a))])
             (list (foo-x q) ((foo-y q)))))))
    '(lambda (a) (#2%list (#2%cons a a) a)))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (a)
           (define-record-type foo (nongenerative) (fields x y))
           (let ([q (make-foo (cons a a) (lambda () a))])
             (list (foo-x q) ((foo-y q)))))))
    '(lambda (a) (#3%list (#3%cons a a) a)))
  ; oscar's example
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f] [compile-profile #f])
      (expand/optimize
        '(let ()
           (import scheme)
           (define-record foo ([immutable ptr a] [immutable ptr b]))
           (define (inc r) (make-foo (foo-a r) (+ (foo-b r) 1)))
           (lambda (x)
             (let* ([r (make-foo 37 x)]
                    [r (inc r)]
                    [r (inc r)])
               r)))))
    `(lambda (x) (#3%$record ',record-type-descriptor? 37 (#2%+ 1 (#2%+ 1 x)))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f] [compile-profile #f])
      (expand/optimize
        '(let ()
           (import scheme)
           (define-record foo ([immutable ptr a] [immutable ptr b]))
           (define (inc r) (make-foo (foo-a r) (+ (foo-b r) 1)))
           (lambda (x)
             (let* ([r (make-foo 37 x)]
                    [r (inc r)]
                    [r (inc r)])
               r)))))
    `(lambda (x) (#3%$record ',record-type-descriptor? 37 (#3%+ 1 (#3%+ 1 x)))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (import scheme)
           (define-record-type foo (fields a b))
           (define (inc r) (make-foo (foo-a r) (+ (foo-b r) 1)))
           (lambda (x)
             (let* ([r (make-foo 37 x)]
                    [r (inc r)]
                    [r (inc r)])
               r)))))
    '(let ([rtd (#2%$make-record-type-descriptor #!base-rtd 'foo #f #f #f #f '#((immutable a) (immutable b)) 'define-record-type)])
       (lambda (x) (#3%$record rtd 37 (#2%+ 1 (#2%+ 1 x))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (import scheme)
           (define-record-type foo (fields a b))
           (define (inc r) (make-foo (foo-a r) (+ (foo-b r) 1)))
           (lambda (x)
             (let* ([r (make-foo 37 x)]
                    [r (inc r)]
                    [r (inc r)])
               r)))))
    '(let ([rtd (#3%$make-record-type-descriptor #!base-rtd 'foo #f #f #f #f '#((immutable a) (immutable b))
                  'define-record-type)])
       (lambda (x) (#3%$record rtd 37 (#3%+ 1 (#3%+ 1 x))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (import scheme)
           (define-record-type foo
             (nongenerative)
             (fields a b)
             (protocol
               (let ([ctr 0])
                 (lambda (new)
                   (lambda (q)
                     (let ([x (begin (set! ctr (+ xtr 1)) ctr)])
                       (new q x)))))))
           (make-foo 3))))
    `(let ([ctr 0])
       (letrec ([g0 (lambda (new) (lambda (q) (new q (begin (set! ctr (#2%+ 1 xtr)) ctr))))])
         (#3%$make-record-constructor-descriptor ',record-type-descriptor?  #f g0 'define-record-type)
         (#3%$record ',record-type-descriptor? 3 (begin (set! ctr (#2%+ 1 xtr)) ctr)))))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (import scheme)
           (define-record-type foo
             (nongenerative)
             (fields a b)
             (protocol
               (let ([ctr 0])
                 (lambda (new)
                   (lambda (q)
                     (let ([x (begin (set! ctr (+ xtr 1)) ctr)])
                       (new q x)))))))
           (make-foo 3))))
    `(let ([ctr 0])
       (letrec ([g0 (lambda (new) (lambda (q) (new q (begin (set! ctr (#3%+ 1 xtr)) ctr))))])
         (#3%$make-record-constructor-descriptor ',record-type-descriptor?  #f g0 'define-record-type)
         (#3%$record ',record-type-descriptor? 3 (begin (set! ctr (#3%+ 1 xtr)) ctr)))))
  (error? ; invalid uid
    (let ()
      (define useless
        (lambda (name)
          (record-mutator (make-record-type-descriptor
                            name #f 5 #f #f '#((mutable x))) 0)))
      (procedure? (useless 'useless-box-setter))))
  (equal?
    (let ()
      (define-record foo ((immutable double x)))
      (foo-x (make-foo 3.0)))
    3.0)
  (begin
    (define $foo
      (lambda (y)
        (define-record foo ((immutable double x) (immutable int y)))
        (foo-x (make-foo 3.0 y))))
    #t)
  (equal? ($foo 17) 3.0)
 )

(mat cp0-rtd-inspection-optimizations
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd
             (make-record-type-descriptor 'foo #f #f #f #f
               '#((mutable x))))
           (define rtd
             (make-record-type-descriptor 'bar prtd 'pluto #t #f
               '#((mutable y) (immutable z))))
           (define rcd (make-record-constructor-descriptor rtd #f #f))
           (list 
             (record-type-descriptor? rtd)
             (record-constructor-descriptor? rcd)
             (record-type-descriptor? rcd)
             (record-constructor-descriptor? rtd)
             (record-field-mutable? prtd 0)
             (record-field-mutable? rtd 0)
             (record-field-mutable? rtd 1)
             (record-type-field-names prtd)
             (record-type-field-names rtd)
             (list (record-type-generative? prtd) (record-type-generative? rtd))
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(let ([prtd (#2%make-record-type-descriptor 'foo #f #f #f #f '#((mutable x)))])
       (let ([rtd (#2%make-record-type-descriptor 'bar prtd 'pluto #t #f '#((mutable y) (immutable z)))])
         (let ([rcd (#3%make-record-constructor-descriptor rtd #f #f)])
           (#2%list
             #t
             #t
             (#2%record-type-descriptor? rcd)
             (#2%record-constructor-descriptor? rtd)
             #t
             #t
             #f
             '#(x)
             '#(y z)
             (#2%list (#2%record-type-generative? prtd) (#2%record-type-generative? rtd))
             (#2%list #f #f)
             (#2%list #f #t))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type-descriptor 'foo #f #f #f #f '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f #f #f '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(let ([prtd (#2%make-record-type-descriptor 'foo #f #f #f #f '#())])
       (#2%make-record-type-descriptor 'bar prtd #f #f #f '#())
       (#2%list
         (#2%list #f #f)
         (#2%list #f #f))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type-descriptor 'foo #f #f #f #t '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f #f #f '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(let ([prtd (#2%make-record-type-descriptor 'foo #f #f #f #t '#())])
       (#2%make-record-type-descriptor 'bar prtd #f #f #f '#())
       (#2%list
         (#2%list #t #t)
         (#2%list #f #f))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type-descriptor 'foo #f #f #f #f '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f #t #t '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(let ([prtd (#2%make-record-type-descriptor 'foo #f #f #f #f '#())])
       (#2%make-record-type-descriptor 'bar prtd #f #t #t '#())
       (#2%list
         (#2%list #f #t)
         (#2%list #f #t))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define prtd (make-record-type-descriptor 'foo #f #f #f #t '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f #t #t '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(let ([prtd (#2%make-record-type-descriptor 'foo #f #f #f #t '#())])
       (#2%make-record-type-descriptor 'bar prtd #f #t #t '#())
       (#2%list
         (#2%list #t #t)
         (#2%list #f #t))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (sealed? opaque?)
           (define prtd (make-record-type-descriptor 'foo #f #f sealed? opaque? '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f #f #f '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(lambda (sealed? opaque?)
       (let ([prtd (#2%make-record-type-descriptor 'foo #f #f sealed? opaque? '#())])
         (let ([rtd (#2%make-record-type-descriptor 'bar prtd #f #f #f '#())])
           (#2%list
             (#2%list (#2%record-type-opaque? prtd) (#2%record-type-opaque? rtd))
             (#2%list (#2%record-type-sealed? prtd) #f))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (sealed? opaque?)
           (define prtd (make-record-type-descriptor 'foo #f #f sealed? opaque? '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f #t #t '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(lambda (sealed? opaque?)
       (let ([prtd (#2%make-record-type-descriptor 'foo #f #f sealed? opaque? '#())])
         (#2%make-record-type-descriptor 'bar prtd #f #t #t '#())
         (#2%list
           (#2%list (#2%record-type-opaque? prtd) #t)
           (#2%list (#2%record-type-sealed? prtd) #t)))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (sealed? opaque?)
           (define prtd (make-record-type-descriptor 'foo #f #f #f #f '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f sealed? opaque? '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(lambda (sealed? opaque?)
       (let ([rtd (#2%make-record-type-descriptor 'bar (#2%make-record-type-descriptor 'foo #f #f #f #f '#()) #f sealed? opaque? '#())])
         (#2%list
           (#2%list #f (#2%record-type-opaque? rtd))
           (#2%list #f (#2%record-type-sealed? rtd))))))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (sealed? opaque?)
           (define prtd (make-record-type-descriptor 'foo #f #f #f #t '#()))
           (define rtd (make-record-type-descriptor 'bar prtd #f sealed? opaque? '#()))
           (list 
             (list (record-type-opaque? prtd) (record-type-opaque? rtd))
             (list (record-type-sealed? prtd) (record-type-sealed? rtd))))))
    '(lambda (sealed? opaque?)
       (let ([rtd (#2%make-record-type-descriptor 'bar (#2%make-record-type-descriptor 'foo #f #f #f #t '#()) #f sealed? opaque? '#())])
         (#2%list
           (#2%list #t #t)
           (#2%list #f (#2%record-type-sealed? rtd))))))
 )

(define (cp0x3 cp0 x)
  (cp0 (cp0 (cp0 x))))

(define (member? o l)
  (and (member o l) #t))
 
(mat cp0-kar-kons-optimizations
  ; for now, it's necesary to run cp0 three times to complete the reduction
  (equal?
    (with-output-to-string
      (lambda ()
        (define-record mybox (val))
        (display (mybox-val (begin (display 1) (make-mybox 2))))))
    "12")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [run-cp0 cp0x3] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record mybox (val))
           (display (mybox-val (begin (display 1) (make-mybox 2)))))))
    '(#2%display
      (begin
        (#2%display 1)
        2)))
  (eq? (let ()
         (define-record kons (kar kdr))
         (kons-kar (make-kons 'a 'b)))
       'a)
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [run-cp0 cp0x3] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record kons (kar kdr))
           (kons-kar (make-kons 'a 'b)))))
    ''a)
  (eq? (let ()
         (define-record kons (kar kdr))
         (kons-kdr (make-kons 'a 'b)))
       'b)
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [run-cp0 cp0x3] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
           (define-record kons (kar kdr))
           (kons-kdr (make-kons 'a 'b)))))
    ''b)
  (member?
    (with-output-to-string
      (lambda ()
        (define-record kons (kar kdr))
        (display (kons-kar (make-kons (begin (display 1) (display 2) 3)
                                      (begin (display 4) (display 5) 6))))))
    '("45123" "12453"))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [run-cp0 cp0x3] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
          (define-record kons (kar kdr))
          (display (kons-kar (make-kons (begin (display 1) (display 2) 3)
                                        (begin (display 4) (display 5) 6)))))))
    '(#2%display
      (begin
        (#2%display 4)
        (#2%display 5)
        (#2%display 1)
        (#2%display 2)
        3)))
  (member?
    (with-output-to-string
      (lambda ()
        (define-record kons (kar kdr))
        (display (kons-kdr (make-kons (begin (display 1) (display 2) 3)
                                      (begin (display 4) (display 5) 6))))))
    '("45126" "12456"))
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [run-cp0 cp0x3] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
          (define-record kons (kar kdr))
          (display (kons-kdr (make-kons (begin (display 1) (display 2) 3)
                                        (begin (display 4) (display 5) 6)))))))
    '(#2%display
      (begin
        (#2%display 4)
        (#2%display 5)
        (#2%display 1)
        (#2%display 2)
        6)))
  (equal?
    (with-output-to-string
      (lambda ()
        (define-record ktail (kar (immutable kdr)))
        (define x (make-ktail 1 2))
        (display 3)
        (display (ktail-kdr (begin (display 4) x)))))
    "342")
  (equivalent-expansion?
    (parameterize ([optimize-level 2] [enable-cp0 #t] [run-cp0 cp0x3] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(let ()
          (define-record ktail (kar (immutable kdr)))
          (define x (make-ktail 1 2))
          (display 3)
          (display (ktail-kdr (begin (display 4) x))))))
    '(begin
       (#2%display 3)
       (#2%display
        (begin
          (#2%display 4)
          2))))
  (equal?
    (with-output-to-string
      (lambda ()
        (define-record ktail (kar (immutable kdr)))
        (define x (make-ktail 1 2))
        (display 3)
        (display (ktail-kar (begin (display 4) x)))))
    "341")
  (not (equivalent-expansion?
         (parameterize ([optimize-level 2] [enable-cp0 #t] [run-cp0 cp0x3] [#%$suppress-primitive-inlining #f])
           (expand/optimize
             '(let ()
               (define-record ktail (kar (immutable kdr)))
               (define x (make-ktail 1 2))
               (display 3)
               (display (ktail-kar (begin (display 4) x))))))
         '(begin
            (#2%display 3)
            (#2%display
             (begin
               (#2%display 4)
               1)))))
)
