;;; enum.ms
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

(mat enumeration
  (equal? '(a b c) (enum-set->list (make-enumeration '(a b c))))
  (equal? '(a b c) (enum-set->list (make-enumeration '(a b a c))))
  (equal? '(a b c)
          (enum-set->list
           ((enum-set-constructor (make-enumeration '(a a b b c d)))
            '(a b c))))
  (equal? 
   '(a b c d e f g h i j k l m n o p q r s t u v w x y z
       aa bb cc dd ee ff gg hh ii jj kk ll mm
       nn oo pp qq rr ss tt uu vv ww xx yy zz)
   (enum-set->list
    (make-enumeration
     '(a b c d e f g h i j k l m n o p q r s t u v w x y z
         aa bb cc dd ee ff gg hh ii jj kk ll mm
         nn oo pp qq rr ss tt uu vv ww xx yy zz))))
  (equal? '(d)
          (let ([c (enum-set-constructor (make-enumeration '(a b c d e f)))])
            (enum-set->list (enum-set-intersection (c '(a c d e))
                                                   (c '(b d f))))))
  (equal? '(a b c d e f)
          (let ([c (enum-set-constructor (make-enumeration '(a b c d e f)))])
            (enum-set->list (enum-set-union (c '(a c d e))
                                            (c '(b d f))))))
  (equal? '(a c e)
          (let ([c (enum-set-constructor (make-enumeration '(a b c d e f)))])
            (enum-set->list (enum-set-difference (c '(a c d e))
                                                 (c '(b d f))))))
  (equal? '(b f)
          (let ([c (enum-set-constructor (make-enumeration '(a b c d e f)))])
            (enum-set->list (enum-set-complement (c '(a c d e))))))
  (equal? '(a b c d e f)
          (let ([c (enum-set-constructor (make-enumeration '(a b c d e f)))])
            (enum-set->list (enum-set-universe (c '(a c d e))))))
  (equal? '(a c d e)
          (let ([c (enum-set-constructor (make-enumeration '(a b c d e f)))])
            (enum-set->list (enum-set-projection (c '(a c d e))
                                                 (c '(b d f))))))
  (equal? '(0 1 #f 5 #f)
          (let ([e (make-enumeration '(a b c d e f))])
            (map (enum-set-indexer e) '(a b g f h))))
  (error? (enum-set-intersection (make-enumeration '(a b c d e f g))
                                 (make-enumeration '(a b c d e f g))))
  (error? (enum-set-intersection 1 1))
  (equal? '(#f #t #f #t #t #t #f #f #f #f)
          (let ([x ((enum-set-constructor (make-enumeration '(a b c d e f g)))
                    '(b d e f))])
            (map (lambda (y) (enum-set-member? y x)) '(a b c d e f g h i j))))
  (equal? '(#t #f #t #f)
          (let ([e1 (make-enumeration '(a b c d))]
                [e2 (make-enumeration '(c d e f))])
            (list (enum-set-subset? e1 e1)
                  (enum-set-subset? e1 e2)
                  (enum-set-subset? e2 e2)
                  (enum-set-subset? e2 e1))))
  (equal? '(#f #f #f #f #f)
          (let ([c1 (enum-set-constructor (make-enumeration '(a b c d)))]
                [c2 (enum-set-constructor (make-enumeration '(c d e f)))])
            (list (enum-set-subset? (c1 '(c)) (c2 '(c d)))
                  (enum-set-subset? (c1 '(a c)) (c2 '(c d)))
                  (enum-set-subset? (c1 '(c d)) (c2 '(c d)))
                  (enum-set=? (c1 '(c d)) (c2 '(c d)))
                  (enum-set=? (c1 '(c)) (c2 '(c d))))))
  (equal? '(#t #f #t #t #f)
          (let ([c1 (enum-set-constructor (make-enumeration '(a b c d e f)))]
                [c2 (enum-set-constructor (make-enumeration '(f e d c b a)))])
            (list (enum-set-subset? (c1 '(c)) (c2 '(c d)))
                  (enum-set-subset? (c1 '(a c)) (c2 '(c d)))
                  (enum-set-subset? (c1 '(c d)) (c2 '(c d)))
                  (enum-set=? (c1 '(c d)) (c2 '(c d)))
                  (enum-set=? (c1 '(c)) (c2 '(c d))))))
  (equal? 'a
          (let ()
            (define-enumeration foo (a b c) make-foo)
            (foo a)))
  (error? (let ()
            (define-enumeration foo (a b c) make-foo)
            (foo d)))
  (equal? '(a b)
          (let ()
            (define-enumeration foo (a b c) make-foo)
            (enum-set->list (make-foo a b))))
  (error? (let ()
            (define-enumeration foo (a b c) make-foo)
            (make-foo a d)))
  (error? (make-enumeration 3))
  (error? (enum-set-universe 3))
  (error? (enum-set-indexer 3))
  (error? (let ([e (make-enumeration '(a b c))])
            ((enum-set-indexer e) 1)))
  (error? (enum-set->list 3))
  (equal? '(a b)
          (let ()
            (define-enumeration foo (a b c) f)
            (enum-set->list (enum-set-union (f a) (f b)))))
  (error? (let ()
            (define-enumeration foo (a b c) f)
            (enum-set->list (enum-set-union (f a) 3))))
  (error? (enum-set-union 4 (make-enumeration '(a b c))))
  (error? (let ()
            (define-enumeration foo (a b c) f)
            (define-enumeration bar (a b c) g)
            (enum-set-union (f a) (g b))))
  (error? (enum-set-complement 3))
  (error? (enum-set-projection 3 (make-enumeration '(a b))))
  (error? (enum-set-projection (make-enumeration '(a b)) 4))
  (equal? '(a b)
          (enum-set->list
           (enum-set-projection (make-enumeration '(a b))
                                (make-enumeration '(a b)))))
  (equal? '(a b)
          (enum-set->list
           (enum-set-projection (make-enumeration '(a b c))
                                (make-enumeration '(a b)))))
  (equal? '(a b)
          (enum-set->list
           (enum-set-projection (make-enumeration '(a b))
                                (make-enumeration '(a b c)))))
  (equal? #t (let () (define-enumeration foo () bar) #t))
  (error? (let () (define-enumeration 3 () bar) #t))
  (error? (let () (define-enumeration foo baz bar) #t))
  (error? (let () (define-enumeration foo () 3) #t))
  (error? (let () (define-enumeration foo (a 3) bar) #t))
  (error? (let ()
            (define-enumeration foo (a b) bar)
            (foo 3)))
  (error? (let ()
            (define-enumeration foo (a b) bar)
            (bar 3)))
  (error? ; cannot extend sealed record
    (make-record-type
      (record-rtd (make-enumeration '(a b c)))
      "foo" '()))

  (equal? #t (enum-set? (make-enumeration '(a b c))))
  (equal? #f (enum-set? 1))
 )
