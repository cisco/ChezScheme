;;; 5_1.ss
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

;;; type and generic predicates

(begin
(define boolean?
   (lambda (x)
      (or (eq? x #t) (eq? x #f))))

(define not
   (lambda (x)
      (if x #f #t)))

(define eqv?
  (lambda (x y)
    (eqv? x y)))

(define (equal? x y)
  (define k0 200)
  (define kb -20)

  #;(define (union-find ht x y) ; hashtable-ref/set! version
    (define (find b) ; splitting
      (let ([n (car b)]) ; next or census
        (if (pair? n)
            (let loop ([b b] [n n])
              (let ([nn (car n)])
                (if (pair? nn)
                    (begin (set-car! b nn) (loop n nn))
                    n)))
            b)))
    (let ([bx (eq-hashtable-ref ht x #f)]
          [by (eq-hashtable-ref ht y #f)])
      (if (not bx)
          (if (not by)
              (let ([b (list 1)])
                (eq-hashtable-set! ht x b)
                (eq-hashtable-set! ht y b)
                #f)
              (begin
                (eq-hashtable-set! ht x (find by))
                #f))
          (if (not by)
              (begin
                (eq-hashtable-set! ht y (find bx))
                #f)
              (let ([rx (find bx)] [ry (find by)])
                (or (eq? rx ry)
                    (let ([nx (car rx)] [ny (car ry)])
                      (if (fx> nx ny)
                          (begin
                            (set-car! ry rx)
                            (set-car! rx (fx+ nx ny))
                            #f)
                          (begin
                            (set-car! rx ry)
                            (set-car! ry (fx+ ny nx))
                            #f)))))))))

  (define (union-find ht x y) ; htcell version
    (define (find p n) ; splitting
      (if (pair? n)
          (let loop ([p p] [n n])
            (let ([nn (cdr n)])
              (if (pair? nn)
                  (begin (set-cdr! p nn) (loop n nn))
                  n)))
          p))
    (let ([ax (eq-hashtable-cell ht x 0)]
          [ay (eq-hashtable-cell ht y 0)])
      (let ([nx (cdr ax)] [ny (cdr ay)])
        (if (eq? nx 0)
            (if (eq? ny 0)
                (begin
                  (set-cdr! ax ay)
                  (set-cdr! ay 1)
                  #f)
                (begin
                  (set-cdr! ax (find ay ny))
                  #f))
            (if (eq? ny 0)
                (begin
                  (set-cdr! ay (find ax nx))
                  #f)
                (let ([rx (find ax nx)] [ry (find ay ny)])
                  (or (eq? rx ry)
                      (let ([nx (cdr rx)] [ny (cdr ry)])
                        (if (fx> nx ny)
                            (begin
                              (set-cdr! ry rx)
                              (set-cdr! rx (fx+ nx ny))
                              #f)
                            (begin
                              (set-cdr! rx ry)
                              (set-cdr! ry (fx+ ny nx))
                              #f))))))))))

  (define (interleave? x y k)
    (let ([ht (make-eq-hashtable)])
      (define (e? x y k)
        (if (fx<= k 0)
            (if (fx= k kb)
                (fast? x y (random (* 2 k0)))
                (slow? x y k))
            (fast? x y k)))
      (define (slow? x y k)
        (cond
          [(eq? x y) k]
          [(pair? x)
           (and (pair? y)
             (if (union-find ht x y)
                 0
                 (let ([k (e? (car x) (car y) (fx- k 1))])
                   (and k (e? (cdr x) (cdr y) k)))))]
          [(vector? x)
           (and (vector? y)
             (let ([n (vector-length x)])
               (and (fx= (vector-length y) n)
                 (if (union-find ht x y)
                     0
                     (let f ([i 0] [k (fx- k 1)])
                       (if (fx= i n)
                           k
                           (let ([k (e? (vector-ref x i) (vector-ref y i) k)])
                              (and k (f (fx+ i 1) k)))))))))]
          [(string? x) (and (string? y) (string=? x y) k)]
          [(flonum? x) (and (flonum? y) ($fleqv? x y) k)]
          [($inexactnum? x)
           (and ($inexactnum? y)
                ($fleqv? ($inexactnum-real-part x) ($inexactnum-real-part y))
                ($fleqv? ($inexactnum-imag-part x) ($inexactnum-imag-part y))
                k)]
          [(bignum? x) (and (bignum? y) (= x y) k)]
          [(ratnum? x) (and (ratnum? y) (= x y) k)]
          [($exactnum? x) (and ($exactnum? y) (= x y) k)]
          [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
          [(fxvector? x)
           (and (fxvector? y)
                (fx= (fxvector-length x) (fxvector-length y))
                (let f ([i (fx- (fxvector-length x) 1)])
                  (if (fx< i 0)
                      k
                      (and (fx= (fxvector-ref x i) (fxvector-ref y i))
                           (f (fx1- i))))))]
          [(box? x)
           (and (box? y)
             (if (union-find ht x y)
                 0
                 (e? (unbox x) (unbox y) (fx- k 1))))]
          [($record? x)
           (and ($record? y)
                (let ([rec-equal? ($record-equal-procedure x y)])
                  (and rec-equal?
                       (if (union-find ht x y)
                           0
                           (let ([next-k k] [decr 1])
                             (and (rec-equal? x y
                                    (lambda (x1 y1)
                                      ; decrementing only on first subfield, if any, like vectors and pairs
                                      (let ([k (e? x1 y1 (fx- next-k decr))])
                                        (and k
                                             (begin
                                               (set! next-k k)
                                               (set! decr 0)
                                               #t)))))
                                  next-k))))))]
          [else (and (eqv? x y) k)]))
      (define (fast? x y k)
        (let ([k (fx- k 1)])
          (cond
            [(eq? x y) k]
            [(pair? x)
             (and (pair? y)
               (let ([k (e? (car x) (car y) k)])
                 (and k (e? (cdr x) (cdr y) k))))]
            [(vector? x)
             (and (vector? y)
               (let ([n (vector-length x)])
                 (and (fx= (vector-length y) n)
                   (let f ([i 0] [k k])
                     (if (fx= i n)
                         k
                         (let ([k (e? (vector-ref x i) (vector-ref y i) k)])
                            (and k (f (fx+ i 1) k))))))))]
            [(string? x) (and (string? y) (string=? x y) k)]
            [(flonum? x) (and (flonum? y) ($fleqv? x y) k)]
            [($inexactnum? x)
             (and ($inexactnum? y)
                  ($fleqv? ($inexactnum-real-part x) ($inexactnum-real-part y))
                  ($fleqv? ($inexactnum-imag-part x) ($inexactnum-imag-part y))
                  k)]
            [(bignum? x) (and (bignum? y) (= x y) k)]
            [(ratnum? x) (and (ratnum? y) (= x y) k)]
            [($exactnum? x) (and ($exactnum? y) (= x y) k)]
            [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
            [(fxvector? x)
             (and (fxvector? y)
                  (fx= (fxvector-length x) (fxvector-length y))
                  (let f ([i (fx- (fxvector-length x) 1)])
                    (if (fx< i 0)
                        k
                        (and (fx= (fxvector-ref x i) (fxvector-ref y i))
                             (f (fx1- i))))))]
            [(box? x) (and (box? y) (e? (unbox x) (unbox y) k))]
            [($record? x)
             (and ($record? y)
                  (let ([rec-equal? ($record-equal-procedure x y)])
                    (and rec-equal?
                         (let ([next-k k])
                           (and (rec-equal? x y
                                  (lambda (x1 y1)
                                    (let ([k (e? x1 y1 next-k)])
                                      (and k
                                           (begin
                                             (set! next-k k)
                                             #t)))))
                                next-k)))))]
            [else (and (eqv? x y) k)])))
      (and (e? x y k) #t)))

  (define (precheck? x y k)
    (cond
      [(eq? x y) k]
      [(pair? x)
       (and (pair? y)
         (if (fx<= k 0)
             k
             (let ([k (precheck? (car x) (car y) (fx- k 1))])
               (and k (precheck? (cdr x) (cdr y) k)))))]
      [(vector? x)
       (and (vector? y)
         (let ([n (vector-length x)])
           (and (fx= (vector-length y) n)
             (let f ([i 0] [k k])
               (if (or (fx= i n) (fx<= k 0))
                   k
                   (let ([k (precheck?
                              (vector-ref x i)
                              (vector-ref y i)
                              (fx- k 1))])
                     (and k (f (fx+ i 1) k))))))))]
      [(string? x) (and (string? y) (string=? x y) k)]
      [(flonum? x) (and (flonum? y) ($fleqv? x y) k)]
      [($inexactnum? x)
       (and ($inexactnum? y)
            ($fleqv? ($inexactnum-real-part x) ($inexactnum-real-part y))
            ($fleqv? ($inexactnum-imag-part x) ($inexactnum-imag-part y))
            k)]
      [(bignum? x) (and (bignum? y) (= x y) k)]
      [(ratnum? x) (and (ratnum? y) (= x y) k)]
      [($exactnum? x) (and ($exactnum? y) (= x y) k)]
      [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
      [(fxvector? x)
       (and (fxvector? y)
            (fx= (fxvector-length x) (fxvector-length y))
            (let f ([i (fx- (fxvector-length x) 1)])
              (if (fx< i 0)
                  k
                  (and (fx= (fxvector-ref x i) (fxvector-ref y i))
                       (f (fx1- i))))))]
      [(box? x)
       (and (box? y)
         (if (fx<= k 0)
             k
             (precheck? (unbox x) (unbox y) (fx- k 1))))]
      [($record? x)
       (and ($record? y)
            (let ([rec-equal? ($record-equal-procedure x y)])
              (and rec-equal?
                   (if (fx<= k 0)
                       k
                       (let ([next-k k])
                         (and (rec-equal? x y
                                (lambda (x1 y1)
                                  ; decrementing k for each field, like vectors but unlike pairs
                                  (let ([k (precheck? x1 y1 (fx- next-k 1))])
                                    (and k
                                         (begin
                                           (set! next-k k)
                                           #t)))))
                              next-k))))))]
      [else (and (eqv? x y) k)]))

  (let ([k (precheck? x y k0)])
    (and k (or (fx> k 0) (interleave? x y 0)))))

(define boolean=?
  (case-lambda
    [(b1 b2)
     (unless (boolean? b1) ($oops 'boolean=? "~s is not a boolean" b1))
     (unless (boolean? b2) ($oops 'boolean=? "~s is not a boolean" b2))
     (#3%boolean=? b1 b2)]
    [(b1 b2 . b*)
     (unless (boolean? b1) ($oops 'boolean=? "~s is not a boolean" b1))
     (unless (boolean? b2) ($oops 'boolean=? "~s is not a boolean" b2))
     (for-each
       (lambda (b) (unless (boolean? b) ($oops 'boolean=? "~s is not a boolean" b)))
       b*)
     (and (#3%boolean=? b1 b2)
          (let f ([b* b*])
            (or (null? b*)
                (and (#3%boolean=? (car b*) b1)
                     (f (cdr b*))))))]))

(define symbol=?
  (case-lambda
    [(s1 s2)
     (unless (symbol? s1) ($oops 'symbol=? "~s is not a symbol" s1))
     (unless (symbol? s2) ($oops 'symbol=? "~s is not a symbol" s2))
     (#3%symbol=? s1 s2)]
    [(s1 s2 . s*)
     (unless (symbol? s1) ($oops 'symbol=? "~s is not a symbol" s1))
     (unless (symbol? s2) ($oops 'symbol=? "~s is not a symbol" s2))
     (for-each
       (lambda (s) (unless (symbol? s) ($oops 'symbol=? "~s is not a symbol" s)))
       s*)
     (and (#3%symbol=? s1 s2)
          (let f ([s* s*])
            (or (null? s*)
                (and (#3%symbol=? (car s*) s1)
                     (f (cdr s*))))))]))
)
