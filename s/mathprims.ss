;;; mathprims.ss
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

(begin
(eval-when (compile)

  (define-syntax define-relop
    (syntax-rules ()
      [(_ name pred? err not-nan?)
       (set! name
          (case-lambda
             [(x1 x2) (#2%name x1 x2)]
             [(x1 x2 x3) (if (#2%name x1 x2) (#2%name x2 x3) (begin (#2%name x2 x3) #f))]
             [(x1 x2 . rest)
              (let loop ([x1 x1] [x2 x2] [rest rest])
                 (if (#2%name x1 x2)
                     (or (null? rest) (loop x2 (car rest) (cdr rest)))
                     (let loop ([rest rest])
                        (cond
                           [(null? rest) #f]
                           [(pred? (car rest)) (loop (cdr rest))]
                           [else (err 'name (car rest))]))))]
             [(x1)
              (unless (pred? x1) (err 'name x1))
              (#3%not-nan? x1)]))]))

  (define-syntax define-r6rs-relop ; requires 2+ arguments
    (syntax-rules ()
      [(_ name pred? err)
       (set! name
          (case-lambda
             [(x1 x2) (#2%name x1 x2)]
             [(x1 x2 x3) (if (#2%name x1 x2) (#2%name x2 x3) (begin (#2%name x2 x3) #f))]
             [(x1 x2 . rest)
              (let loop ([x1 x1] [x2 x2] [rest rest])
                 (if (#2%name x1 x2)
                     (or (null? rest) (loop x2 (car rest) (cdr rest)))
                     (let loop ([rest rest])
                        (cond
                           [(null? rest) #f]
                           [(pred? (car rest)) (loop (cdr rest))]
                           [else (err 'name (car rest))]))))]))]))

  (define-syntax define-addop
    (syntax-rules ()
      [(_ name)
       (set! name
          (case-lambda
             [(x1 x2) (#2%name x1 x2)]
             [(x1 x2 x3) (#2%name (#2%name x1 x2) x3)]
             [(x1 x2 . rest)
              (let loop ([x1 x1] [x2 x2] [rest rest])
                 (let ([x (#2%name x1 x2)])
                    (if (null? rest) x (loop x (car rest) (cdr rest)))))]
             [(x1) (#2%name x1)]
             [() (#2%name)]))]))

  (define-syntax define-subop
    (syntax-rules ()
      [(_ name pred? err)
       (set! name
          (case-lambda
             [(x1 x2) (#2%name x1 x2)]
             [(x1 x2 x3) (#2%name (#2%name x1 x2) x3)]
             [(x1) (#2%name x1)]
             [(x0 x1 . rest)
              (unless (pred? x0) (err 'name x0))
              (let loop ([x0 x0] [x1 x1] [rest rest])
                (unless (pred? x1) (err 'name x1))
                (if (null? rest)
                    (#3%name x0 x1)
                    (loop (#3%name x0 x1) (car rest) (cdr rest))))]))]))

  (define-syntax define-generic-subop
    (syntax-rules ()
      [(_ name)
       (set! name
          (case-lambda
             [(x1 x2) (#2%name x1 x2)]
             [(x1 x2 x3) (#2%name (#2%name x1 x2) x3)]
             [(x1) (#2%name x1)]
             [(x0 x1 . rest)
              (let loop ([x0 x0] [x1 x1] [rest rest])
                (if (null? rest)
                    (#2%name x0 x1)
                    (loop (#2%name x0 x1) (car rest) (cdr rest))))]))]))

  (define-syntax define-cfl-relop
    (syntax-rules ()
      [(_ name pred? err not-nan?)
       (set! name
          (case-lambda
             [(x1 x2)
              (unless (pred? x1) (err 'name x1))
              (unless (pred? x2) (err 'name x2))
              (#3%name x1 x2)]
             [(x1 x2 x3)
              (unless (pred? x1) (err 'name x1))
              (unless (pred? x2) (err 'name x2))
              (unless (pred? x3) (err 'name x3))
              (and (#3%name x1 x2) (#3%name x2 x3))]
             [(x1 x2 . rest)
              (unless (pred? x1) (err 'name x1))
              (let loop ([x1 x1] [x2 x2] [rest rest])
                 (unless (pred? x2) (err 'name x2))
                 (if (#3%name x1 x2)
                     (or (null? rest) (loop x2 (car rest) (cdr rest)))
                     (let loop ([rest rest])
                        (cond
                           [(null? rest) #f]
                           [(pred? (car rest)) (loop (cdr rest))]
                           [else (err 'name (car rest))]))))]
             [(x1)
              (unless (pred? x1) (err 'name x1))
              (not-nan? x1)]))]))

  (define-syntax define-cfl-addop
    (syntax-rules ()
      [(_ name pred? err)
       (set! name
          (case-lambda
             [(x1 x2)
              (unless (pred? x1) (err 'name x1))
              (unless (pred? x2) (err 'name x2))
              (#3%name x1 x2)]
             [(x1 x2 x3)
              (unless (pred? x1) (err 'name x1))
              (unless (pred? x2) (err 'name x2))
              (unless (pred? x3) (err 'name x3))
              (#3%name (#3%name x1 x2) x3)]
             [(x1 x2 . rest)
              (unless (pred? x1) (err 'name x1))
              (let loop ([x1 x1] [x2 x2] [rest rest])
                 (unless (pred? x2) (err 'name x2))
                 (let ([x (#3%name x1 x2)])
                    (if (null? rest) x (loop x (car rest) (cdr rest)))))]
             [(x1)
              (unless (pred? x1) (err 'name x1))
              (#3%name x1)]
             [() (name)]))]))

  (define-syntax define-cfl-subop
    (syntax-rules ()
      [(_ name pred? err)
       (set! name
          (case-lambda
             [(x1 x2)
              (unless (pred? x1) (err 'name x1))
              (unless (pred? x2) (err 'name x2))
              (#3%name x1 x2)]
             [(x1 x2 x3)
              (unless (pred? x1) (err 'name x1))
              (unless (pred? x2) (err 'name x2))
              (unless (pred? x3) (err 'name x3))
              (#3%name (#3%name x1 x2) x3)]
             [(x1)
              (unless (pred? x1) (err 'name x1))
              (#3%name x1)]
             [(x0 x1 . rest)
              (unless (pred? x0) (err 'name x0))
              (let loop ([x0 x0] [x1 x1] [rest rest])
                (unless (pred? x1) (err 'name x1))
                (if (null? rest)
                    (#3%name x0 x1)
                    (loop (#3%name x0 x1) (car rest) (cdr rest))))]))]))
)

(define 1- (lambda (x) (#2%1- x)))

(define 1+ (lambda (x) (#2%1+ x)))

(define sub1 (lambda (x) (#2%sub1 x)))

(define -1+ (lambda (x) (#2%-1+ x)))

(define add1 (lambda (x) (#2%add1 x)))

(define-addop +)
(define-generic-subop -)
(define-addop *)
(define-generic-subop /)

(define-addop logand)
(define-addop bitwise-and)
(define-addop logior)
(define-addop bitwise-ior)
(define-addop logor)
(define-addop logxor)
(define-addop bitwise-xor)

(define (lognot x) (#2%lognot x))
(define (bitwise-not x) (#2%bitwise-not x))

(define (logbit? x y) (#2%logbit? x y))
(define (bitwise-bit-set? x y) (#2%bitwise-bit-set? x y))
(define (logbit0 x y) (#2%logbit0 x y))
(define (logbit1 x y) (#2%logbit1 x y))
(define (logtest x y) (#2%logtest x y))

(eval-when (compile)
  (define-syntax define-number-relop
    (syntax-rules ()
      [(_ name)
       (define name
          (case-lambda
             [(x1 x2) (#2%name x1 x2)]
             [(x1 x2 x3) (if (#2%name x1 x2) (#2%name x2 x3) (begin (#2%name x2 x3) #f))]
             [(x1) (begin (#2%name x1 0) #t)]
             [(x1 x2 . rest)
              (let loop ([x1 x1] [x2 x2] [ls rest])
                 (if (or (null? ls) (loop x2 (car ls) (cdr ls)))
                     (#2%name x1 x2)
                     (begin (#2%name x1 x2) #f)))]))])))

(define-number-relop =)
(define-number-relop <)
(define-number-relop >)
(define-number-relop <=)
(define-number-relop >=)

(eval-when (compile)
  (define-syntax define-r6rs-number-relop ; requires 2+ argument
    (syntax-rules ()
      [(_ r6rs:name name)
       (define-who #(r6rs: name)
          (case-lambda
             [(x1 x2) (#2%r6rs:name x1 x2)]
             [(x1 x2 x3) (if (#2%r6rs:name x1 x2)
                             (#2%r6rs:name x2 x3)
                             (begin (#2%r6rs:name x2 x3) #f))]
             [(x1 x2 . rest)
              (let loop ([x1 x1] [x2 x2] [ls rest])
                 (if (or (null? ls) (loop x2 (car ls) (cdr ls)))
                     (#2%r6rs:name x1 x2)
                     (begin (#2%r6rs:name x1 x2) #f)))]))])))

(define-r6rs-number-relop r6rs:= =)
(define-r6rs-number-relop r6rs:< <)
(define-r6rs-number-relop r6rs:> >)
(define-r6rs-number-relop r6rs:<= <=)
(define-r6rs-number-relop r6rs:>= >=)

(eval-when (compile) (optimize-level 3))

(let ()
   (define flargerr
      (lambda (who x)
         ($oops who "~s is not a flonum" x)))

   (set! fl-make-rectangular
      (lambda (x y)
         (unless (flonum? x) (flargerr 'fl-make-rectangular x))
         (unless (flonum? y) (flargerr 'fl-make-rectangular y))
         (#3%fl-make-rectangular x y)))

   (define-addop fl+)
   (define-subop fl- flonum? flargerr)
   (define-addop fl*)
   (define-subop fl/ flonum? flargerr)

   (set! flabs
      (lambda (x)
         (unless (flonum? x) (flargerr 'flabs x))
         (#3%flabs x)))

   (set! flround
      (lambda (x)
         (unless (flonum? x) (flargerr 'flround x))
         (#3%flround x)))

   (set! fllp
      (lambda (x)
         (unless (flonum? x) (flargerr 'fllp x))
         (#3%fllp x)))

   (define-relop fl= flonum? flargerr fl=)
   (define-relop fl< flonum? flargerr fl=)
   (define-relop fl> flonum? flargerr fl=)
   (define-relop fl<= flonum? flargerr fl=)
   (define-relop fl>= flonum? flargerr fl=)
   (define-r6rs-relop fl=? flonum? flargerr)
   (define-r6rs-relop fl<? flonum? flargerr)
   (define-r6rs-relop fl>? flonum? flargerr)
   (define-r6rs-relop fl<=? flonum? flargerr)
   (define-r6rs-relop fl>=? flonum? flargerr)

   (set-who! $fleqv?
     (lambda (x y)
       (unless (flonum? x) (flargerr who x))
       (unless (flonum? y) (flargerr who y))
       (#3%$fleqv? x y)))

   (set-who! $flhash
     (lambda (x)
       (unless (flonum? x) (flargerr who x))
       (#3%$flhash x)))

   (set-who! $flonum-exponent ; requires optimize-level 3
      (lambda (x)
         (unless (flonum? x) (flargerr who x))
         ($flonum-exponent x)))

   (set-who! $flonum-sign ; requires optimize-level 3
      (lambda (x)
         (unless (flonum? x) (flargerr who x))
         ($flonum-sign x)))

   (set-who! flonum->fixnum
      (let ([flmnf (fixnum->flonum (most-negative-fixnum))]
            [flmpf (fixnum->flonum (most-positive-fixnum))])
         (lambda (x)
            (unless (flonum? x) (flargerr who x))
            (unless (fl<= flmnf x flmpf)
               ($oops who "result for ~s would be outside of fixnum range" x))
            (#3%flonum->fixnum x))))
)

(let ()
   (define fxargerr
      (lambda (who x)
         ($oops who "~s is not a fixnum" x)))

   (define /zeroerr
      (lambda (who)
         ($oops who "attempt to divide by zero")))

   (define fxanserr
      (lambda (who . args)
         ($impoops who "fixnum overflow computing ~s" (cons who args))))

   (define-addop fx+)
   (define-subop fx- fixnum? fxargerr)

   (set-who! #(r6rs: fx+) (lambda (x y) (#2%r6rs:fx+ x y)))
   (set-who! #(r6rs: fx-)
     (case-lambda
       [(x) (#2%r6rs:fx- x)]
       [(x y) (#2%r6rs:fx- x y)]))

   (set! fx1-
      (lambda (x)
         (#2%fx1- x)))

   (set! fx1+
      (lambda (x)
         (#2%fx1+ x)))

   (set! fxzero?
      (lambda (x)
         (#2%fxzero? x)))

   (set! fx*
     (rec fx*
       (case-lambda
         [(x1 x2)
          (if (fixnum? x1)
              (if (fixnum? x2)
                  ; should handle fixnums (avoiding overflow)
                  (let ([n (* x1 x2)])
                     (if (fixnum? n) n (fxanserr 'fx* x1 x2)))
                  (fxargerr 'fx* x2))
              (fxargerr 'fx* x1))]
         [(x1 x2 x3)
          (if (fixnum? x1)
              (if (fixnum? x2)
                  (if (fixnum? x3)
                      ; should handle fixnums (avoiding overflow)
                      (let ([n (* x1 x2)])
                        (if (fixnum? n)
                            ; should handle fixnums (avoiding overflow)
                            (let ([n (* n x3)])
                              (if (fixnum? n) n (fxanserr 'fx* x1 x2 x3)))
                            (fxanserr 'fx* x1 x2 x3)))
                      (fxargerr 'fx* x3))
                  (fxargerr 'fx* x2))
              (fxargerr 'fx* x1))]
         [(x1) (if (fixnum? x1) x1 (fxargerr 'fx* x1))]
         [() 1]
         [(x1 . rest)
          (let loop ([a x1] [ls rest])
             (if (null? ls)
                 a
                 (loop (fx* a (car ls)) (cdr ls))))])))

   (set-who! #(r6rs: fx*)
     (lambda (x1 x2)
       (if (fixnum? x1)
           (if (fixnum? x2)
               ; should handle fixnums (avoiding overflow)
               (let ([n (* x1 x2)])
                 (if (fixnum? n) n (fxanserr who x1 x2)))
               (fxargerr who x2))
           (fxargerr who x1))))

   (set! fxquotient
     (rec fxquotient
       (case-lambda
         [(x1 x2)
          (if (fixnum? x1)
              (if (fixnum? x2)
                  (begin
                    (when (fx= x2 0) (/zeroerr 'fxquotient))
                    (if (and (fx= x2 -1) (fx= x1 (most-negative-fixnum)))
                        (fxanserr 'fxquotient x1 x2)
                        (#3%fxquotient x1 x2)))
                  (fxargerr 'fxquotient x2))
              (fxargerr 'fxquotient x1))]
         [(x1 x2 x3)
          (if (fixnum? x1)
              (if (fixnum? x2)
                  (if (fixnum? x3)
                      (begin
                        (when (fx= x2 0) (/zeroerr 'fxquotient))
                        (if (and (fx= x2 -1) (fx= x1 (most-negative-fixnum)))
                            (fxanserr 'fxquotient x1 x2 x3)
                            (let ([n (#3%fxquotient x1 x2)])
                              (when (fx= x3 0) (/zeroerr 'fxquotient))
                              (if (and (fx= x3 -1) (fx= n (most-negative-fixnum)))
                                  (fxanserr 'fxquotient x1 x2 x3)
                                  (#3%fxquotient n x3)))))
                      (fxargerr 'fxquotient x3))
                  (fxargerr 'fxquotient x2))
              (fxargerr 'fxquotient x1))]
         [(x1)
          (if (fixnum? x1)
              (if (fx= x1 0)
                  (/zeroerr 'fxquotient)
                  (#3%fxquotient 1 x1))
              (fxargerr 'fxquotient x1))]
         [(x1 . rest)
          (let loop ([a x1] [ls rest])
            (if (null? ls)
                a
                (loop (fxquotient a (car ls)) (cdr ls))))])))

   (set! fx/
     (rec fx/ ;; same as fxquotient---should it be?
       (case-lambda
         [(x1 x2)
          (if (fixnum? x1)
              (if (fixnum? x2)
                  (begin
                    (when (fx= x2 0) (/zeroerr 'fx/))
                    (if (and (fx= x2 -1) (fx= x1 (most-negative-fixnum)))
                        (fxanserr 'fx/ x1 x2)
                        (#3%fx/ x1 x2)))
                  (fxargerr 'fx/ x2))
              (fxargerr 'fx/ x1))]
         [(x1 x2 x3)
          (if (fixnum? x1)
              (if (fixnum? x2)
                  (if (fixnum? x3)
                      (begin
                        (when (fx= x2 0) (/zeroerr 'fx/))
                        (if (and (fx= x2 -1) (fx= x1 (most-negative-fixnum)))
                            (fxanserr 'fx/ x1 x2 x3)
                            (let ([n (#3%fx/ x1 x2)])
                              (when (fx= x3 0) (/zeroerr 'fx/))
                              (if (and (fx= x3 -1) (fx= n (most-negative-fixnum)))
                                  (fxanserr 'fx/ x1 x2 x3)
                                  (#3%fx/ n x3)))))
                      (fxargerr 'fx/ x3))
                  (fxargerr 'fx/ x2))
              (fxargerr 'fx/ x1))]
         [(x1)
          (if (fixnum? x1)
              (if (fx= x1 0)
                  (/zeroerr 'fx/)
                  (#3%fx/ 1 x1))
              (fxargerr 'fx/ x1))]
         [(x1 . rest)
          (let loop ([a x1] [ls rest])
            (if (null? ls)
                a
                (loop (fx/ a (car ls)) (cdr ls))))])))

   (set! fxabs
      (lambda (x)
         (unless (fixnum? x) (fxargerr 'fxabs x))
         (when (fx= x (most-negative-fixnum)) (fxanserr 'fxabs x))
         (#3%fxabs x)))

   (define-relop fx= fixnum? fxargerr fx=)
   (define-relop fx< fixnum? fxargerr fx=)
   (define-relop fx> fixnum? fxargerr fx=)
   (define-relop fx<= fixnum? fxargerr fx=)
   (define-relop fx>= fixnum? fxargerr fx=)
   (define-r6rs-relop fx=? fixnum? fxargerr)
   (define-r6rs-relop fx<? fixnum? fxargerr)
   (define-r6rs-relop fx>? fixnum? fxargerr)
   (define-r6rs-relop fx<=? fixnum? fxargerr)
   (define-r6rs-relop fx>=? fixnum? fxargerr)

   (set! $fxu<
     (lambda (x y)
       (unless (fixnum? x) (fxargerr '$fxu< x))
       (unless (fixnum? y) (fxargerr '$fxu< y))
       (#3%$fxu< x y)))

   (define-addop fxlogand)
   (define-addop fxlogior)
   (define-addop fxlogor)
   (define-addop fxlogxor)
   (define-addop fxand)
   (define-addop fxior)
   (define-addop fxxor)

   (set! fxsll
      (lambda (x y)
         (#2%fxsll x y)))

   (set! fxarithmetic-shift-left
     (lambda (x y)
       (#2%fxarithmetic-shift-left x y)))

   (set! fxsrl
      (lambda (x y)
         (#2%fxsrl x y)))

   (set! fxsra
      (lambda (x y)
         (#2%fxsra x y)))

   (set! fxarithmetic-shift-right
     (lambda (x y)
       (#2%fxarithmetic-shift-right x y)))

   (set! fxarithmetic-shift
     (lambda (x y)
       (#2%fxarithmetic-shift x y)))

   (set! fxlognot
      (lambda (x)
         (#2%fxlognot x)))

   (set! fxnot
      (lambda (x)
         (#2%fxnot x)))

   (set! fxlogtest
      (lambda (x y)
         (#2%fxlogtest x y)))

   (set! fxlogbit?
      (lambda (x y)
         (#2%fxlogbit? x y)))

   (set! fxbit-set?
      (lambda (x y)
         (#2%fxbit-set? x y)))

   (set! fxlogbit0
      (lambda (x y)
         (#2%fxlogbit0 x y)))

   (set! fxlogbit1
      (lambda (x y)
         (#2%fxlogbit1 x y)))

   (set-who! fxcopy-bit
     (lambda (n k b)
      ; optimize-level 2 handler doesn't kick in unless b=0 or b=1
       (unless (fixnum? n) (fxargerr who n))
       (unless (fixnum? k) (fxargerr who k))
       (unless ($fxu< k (fx- (fixnum-width) 1))
         ($oops who "invalid bit index ~s" k))
       (case b
         [(0) (#3%fxlogbit0 k n)]
         [(1) (#3%fxlogbit1 k n)]
         [else ($oops who "invalid bit value ~s" b)])))

   (set! fxeven?
      (lambda (x)
         (#2%fxeven? x)))

   (set! fxodd?
      (lambda (x)
         (#2%fxodd? x)))

   (set! fxremainder
      (lambda (x y)
         (unless (fixnum? x) (fxargerr 'fxremainder x))
         (unless (fixnum? y) (fxargerr 'fxremainder y))
         (when (fx= y 0) (/zeroerr 'fxremainder))
         (#3%fxremainder x y)))

   (set! fxmodulo
      (lambda (x y)
         (unless (fixnum? x) (fxargerr 'fxmodulo x))
         (unless (fixnum? y) (fxargerr 'fxmodulo y))
         (when (fx= y 0) (/zeroerr 'fxmodulo))
         (let ([r (fxremainder x y)])
            (if (if (fxnegative? y) (fxpositive? r) (fxnegative? r))
                (fx+ r y)
                r))))

   (set! fxmin
      (case-lambda
         [(x y)
          (unless (fixnum? x) (fxargerr 'fxmin x))
          (unless (fixnum? y) (fxargerr 'fxmin y))
          (if (fx< y x) y x)]
         [(x y z)
          (unless (fixnum? x) (fxargerr 'fxmin x))
          (unless (fixnum? y) (fxargerr 'fxmin y))
          (unless (fixnum? z) (fxargerr 'fxmin z))
          (if (fx< y x)
              (if (fx< z y) z y)
              (if (fx< z x) z x))]
         [(x . y)
          (unless (fixnum? x) (fxargerr 'fxmin x))
          (let f ([x x] [y y])
             (if (null? y)
                 x
                 (f (let ([z (car y)])
                       (unless (fixnum? z) (fxargerr 'fxmin z))
                       (if (fx< z x) z x))
                    (cdr y))))]))

   (set! fxmax
      (case-lambda
         [(x y)
          (unless (fixnum? x) (fxargerr 'fxmax x))
          (unless (fixnum? y) (fxargerr 'fxmax y))
          (if (fx> y x) y x)]
         [(x y z)
          (unless (fixnum? x) (fxargerr 'fxmax x))
          (unless (fixnum? y) (fxargerr 'fxmax y))
          (unless (fixnum? z) (fxargerr 'fxmax z))
          (if (fx> y x)
              (if (fx> z y) z y)
              (if (fx> z x) z x))]
         [(x . y)
          (unless (fixnum? x) (fxargerr 'fxmax x))
          (let f ([x x] [y y])
             (if (null? y)
                 x
                 (f (let ([z (car y)])
                       (unless (fixnum? z) (fxargerr 'fxmax z))
                       (if (fx> z x) z x))
                    (cdr y))))]))

   (set! fxnegative?
      (lambda (x)
         (#2%fxnegative? x)))

   (set! fxpositive?
      (lambda (x)
         (#2%fxpositive? x)))

   (set! fxnonnegative?
      (lambda (x)
         (#2%fxnonnegative? x)))

   (set! fxnonpositive?
      (lambda (x)
         (#2%fxnonpositive? x)))

   (set! fixnum->flonum
      (lambda (x)
         (unless (fixnum? x) (fxargerr 'fixnum->flonum x))
         (#3%fixnum->flonum x)))

   (set-who! fxlength
     (lambda (x)
       (if (fixnum? x)
           (#3%fxlength x)
           (fxargerr who x))))

   (set-who! fxfirst-bit-set
     (lambda (x)
       (if (fixnum? x)
           (#3%fxfirst-bit-set x)
           (fxargerr who x))))

   (set-who! fxif
     (lambda (x y z)
       (if (fixnum? x)
           (if (fixnum? y)
               (if (fixnum? z)
                   (#3%fxif x y z)
                   (fxargerr who z))
               (fxargerr who y))
           (fxargerr who x))))

   (set-who! fxbit-field
     (lambda (n start end)
       (if (fixnum? n)
           (if (and (fixnum? start) ($fxu< start (fixnum-width)))
               (if (and (fixnum? end) ($fxu< end (fixnum-width)))
                   (if (fx<= start end)
                       (fxsra (fxand n (fxnot (fxsll -1 end))) start)
                       ($oops who "start index ~s is greater than end index ~s" start end))
                   ($oops who "~s is not a valid end index" end))
               ($oops who "~s is not a valid start index" start))
           (fxargerr who n))))

   (set-who! fxcopy-bit-field
     (lambda (n start end m)
       (if (fixnum? n)
           (if (and (fixnum? start) ($fxu< start (fixnum-width)))
               (if (and (fixnum? end) ($fxu< end (fixnum-width)))
                   (if (fx<= start end)
                       (if (fixnum? m)
                           (let ([mask (fx- (fxsll 1 (fx- end start)) 1)])
                             (fxior
                               (fxand n (fxnot (fxsll mask start)))
                               (fxsll (fxand m mask) start)))
                           (fxargerr who m))
                       ($oops who "start index ~s is greater than end index ~s" start end))
                   ($oops who "~s is not a valid end index" end))
               ($oops who "~s is not a valid start index" start))
           (fxargerr who n))))
)

;;; The "cfl" operations could be done at level 0 by expanding them out.
;;; They might be more efficient that way since they wouldn't have to
;;; do double flonum checking.

(define cflonum?
   (lambda (x)
      (cflonum? x)))

(let ()

(define noncflonum-error
   (lambda (who x)
      ($oops who "~s is not a cflonum" x)))

(set! cfl-real-part
   (lambda (z)
      (type-case z
          [($inexactnum?) ($inexactnum-real-part z)]
          [(flonum?) z]
          [else (noncflonum-error 'cfl-real-part z)])))

(set! cfl-imag-part
   (lambda (z)
      (type-case z
          [($inexactnum?) ($inexactnum-imag-part z)]
          [(flonum?) 0.0]
          [else (noncflonum-error 'cfl-imag-part z)])))

(define-cfl-addop cfl+ cflonum? noncflonum-error)
(define-cfl-addop cfl* cflonum? noncflonum-error)
(define-cfl-subop cfl- cflonum? noncflonum-error)
(define-cfl-subop cfl/ cflonum? noncflonum-error)
(define-cfl-relop cfl= cflonum? noncflonum-error cfl=)

(set! cfl-conjugate
   (lambda (x)
      (type-case x
         [(cflonum?) (#3%cfl-conjugate x)]
         [else (noncflonum-error 'cfl-conjugate x)])))

)
)
