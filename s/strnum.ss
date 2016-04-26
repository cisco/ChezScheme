"strnum.ss"
;;; strnum.ss
;;; Copyright 1984-2016 Cisco Systems, Inc.
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

(define $str->num)

(eval-when (compile)

   (define-constant max-float-exponent
      (float-type-case
         [(ieee) 1023]))

   (define-constant min-float-exponent
      (float-type-case
         [(ieee) -1023]))
)

#|
R5RS Section 6.2.4 (Syntax of numerical constants) says

  A numerical constant may be specified to be either exact or inexact
  by a prefix.  The prefixes are #e for exact, and #i for inexact.
  An exactness prefix may appear before or after any radix prefix that
  is used.  If the written representation of a number has no exactness
  prefix, the constant may be either inexact or exact.  It is inexact
  if it contains a decimal point, an exponent, or a ``#'' character in
  the place of a digit, otherwise it is exact.

This specifies the exactness of the result.  It doesn't specify precisely
the number produced when there is a mix of exact and inexact subparts
and what happens if an apparently exact subpart of an inexact number
cannot be represented.

Possible options include:

(A) Treat each subpart as inexact if the #i prefix is specified or the
    #e prefix is not specified and any subpart is inexact, i.e.,
    contains a decimal point, exponent, or # character.  Treat each
    subpart as exact if the #e prefix is specified or if the #i prefix
    is not specified and each subpart is exact.

(B) Treat each subpart as exact or inexact in isolation and use the
    usual rules for preserving inexactness when combining the subparts.
    Apply inexact to the result if #i is present and exact
    to the result if #e is present.

(C) If #e and #i are not present, treat each subpart as exact or inexact
    in isolation and use the usual rules for preserving inexactness when
    combining the subparts.  If #e is present, treat each subpart as
    exact, with # digits treated as zeros.  If #i is present, treat each
    subpart as inexact.

Also, the R5RS description of string->number says:

  Returns a number of the maximally precise representation expressed
  by the given string.  Radix must be an exact integer, either 2,
  8, 10, or 16.  If supplied, radix is a default radix that may be
  overridden by an explicit radix prefix in string (e.g. "#o177").
  If radix is not supplied, then the default radix is 10.  If string is
  not a syntactically valid notation for a number, then string->number
  returns #f.

This raises an additional question, which is whether string->number
should signal an error or return #f whenever a "syntactically valid"
number (or subpart thereof, with option B), such as 1/0 or #e1/0#
(or 1/0+1.0i) cannot be represented.

                       A                B                C

0/0                    #f               #f               #f
0/0#                  nan                0                0
0#/0                  nan               #f               #f
0#/0#                 nan              nan              nan
#i0/0                 nan               #f              nan
#i0/0#                nan              0.0              nan
#i0#/0                nan               #f              nan
#i0#/0#               nan              nan              nan
#e0/0                  #f               #f               #f
#e0/0#                 #f                0               #f
#e0#/0                 #f               #f               #f
#e0#/0#                #f               #f               #f

1/0                    #f               #f               #f
1/0#                  inf              inf              inf
1#/0                  inf               #f               #f
1#/0#                 inf              inf              inf
#i1/0                 inf               #f              inf
#i1/0#                inf              inf              inf
#i1#/0                inf               #f              inf
#i1#/0#               inf              inf              inf
#e1/0                  #f               #f               #f
#e1/0#                 #f               #f               #f
#e1#/0                 #f               #f               #f
#e1#/0#                #f               #f               #f

1/0+1.0i          +nan.0+1.0i           #f               #f
1.0+1/0i           1.0+nan.0i           #f               #f

#e1e1000         (expt 10 1000)         #f         (expt 10 1000)
#e1#e1000        (expt 10 1001)         #f         (expt 10 1001)

This code implements Option C and returns #f instead of signaling an
error whenever a syntactically valid number cannot be represented.
It computes inexact components with exact arithmetic where possible,
however, before converting them into inexact numbers, to insure the
greatest possible accuracy.

Rationale for Option C: B and C adhere most closely to the semantics of
the individual / and make-rectangular operators, and neither requires that
we scan the entire number first (as with A) to determine the (in)exactness
of the result.  C takes into account the known (in)exactness of the
result to represent some useful values that B cannot, such as #e1e1000.
|#


(let ()
;;   (mknum-state <state name>
;;                <expression if end of string found>
;;                [<transition key> <state transition>]
;;                ...)
(define-syntax mknum-state
  (lambda (e)
    (syntax-case e ()
      ((_key name (id ...) exp clause ...)
       (with-implicit (_key z x k i r6rs? c d)
         (let ()
           (define mknum-state-test
             (lambda (key)
               (syntax-case key (-)
                 (char
                  (char? (datum char))
                  #'(char=? c char))
                 ((char1 - char2)
                  #'(char<=? char1 c char2))
                 ((key ...)
                  `(,#'or ,@(map mknum-state-test #'(key ...)))))))
           (define mknum-call
             (lambda (incr? call)
               (syntax-case call (let)
                 [(let ([x e] ...) call)
                  (with-syntax ([call (mknum-call incr? #'call)])
                    #'(let ([x e] ...) call))]
                 [(x1 x2 ...)
                  (if incr?
                      #'(x1 z x k (fx+ i 1) r6rs? x2 ...)
                      #'(x1 z x k i r6rs? x2 ...))])))
           (define mknum-state-help
             (lambda (ls)
               (syntax-case ls (else)
                 (() #''bogus)
                 (((else call)) (mknum-call #f #'call))
                 (stuff
                  (with-syntax ((rest (mknum-state-help (cdr ls))))
                    (syntax-case (car ls) (digit)
                      (((digit r) call)
                       (with-syntax ([call (mknum-call #t #'call)])
                         #'(let ((d (ascii-digit-value c r)))
                             (if d call rest))))
                      (((digit r) fender call)
                       (with-syntax ([call (mknum-call #t #'call)])
                         #'(let ((d (ascii-digit-value c r)))
                             (if (and d fender) call rest))))
                      ((key call)
                       (with-syntax ([test (mknum-state-test #'key)]
                                     [call (mknum-call #t #'call)])
                         #'(if test call rest)))
                      ((key fender call)
                       (with-syntax ([test (mknum-state-test #'key)]
                                     [call (mknum-call #t #'call)])
                         #'(if (and test fender) call rest)))))))))
           (with-syntax ((rest (mknum-state-help #'(clause ...))))
             #'(define name
                 (lambda (z x k i r6rs? id ...)
                   (if (= i k)
                       exp
                       (let ([c (char-downcase (string-ref x i))])
                          rest)))))))))))

(define ascii-digit-value
   (lambda (c r)
      (let ([v (cond
                  [(char<=? #\0 c #\9) (char- c #\0)]
                  [(char<=? #\a c #\z) (char- c #\W)]
                  [else 36])])
         (and (fx< v r) v))))

; variables automatically maintained and passed by the mknum macro:
;     z: if #f, return number or #f else return z or #f
;     x: string
;     k: string length
;     i: index into string, 0 <= i < k
; r6rs?: if #t, reject non-r6rs features
; variables automatically created by the mknum macro:
;     c: holds current character
;     d: holds digit value of c in a digit clause
; other "interesting" variables:
;     r: radix, 0 < r < 37
;    ex: exactness: 'i, 'e, or #f (from prefix)
;     e: strict exactness: 'i or 'e
;     s: function to add sign to number 
;    ms: meta-state: ureal, real, real@
;   n,m: number or z

; The sign of the mantissa cannot be put on until a number has
; been made inexact (if necessary) to make sure zero gets the right sign.

(let ()

(define plus (lambda (x) x))
(define minus -)

(define-record-type state
  (fields (immutable type) (immutable part))
  (nongenerative)
  (sealed #t))

(define make-part ; never turns inexact number exact
  (case-lambda
    [(e s m) (s (if (eq? e 'i) (inexact m) m))]
    [(e s m r n)
    ; get out quick for really large/small exponents, like 1e1000000000
    ; no need for great precision here; using 2x the min/max base two
    ; exponent, which should be conservative for all bases.  1x should
    ; actually work for positive n, but for negative n we need something
    ; smaller than 1x to allow denormalized numbers.
    ; s must be the actual sign of the result, with m >= 0 
     (s (if (eq? e 'i)
            (if (or (> n (* (constant max-float-exponent) 2))
                    (< n (* (constant min-float-exponent) 2)))
                (if (< n 0) 0.0 +inf.0)
                (inexact (* m (expt r n))))
            (* m (expt r n))))]))

(define finish-number
  (lambda (z ms n)
    (if (or (eq? ms 'ureal) (eq? ms 'real))
        (or z n)
        (and (eq? (state-type ms) 'real@)
             (or z (make-polar (state-part ms) n))))))

(define finish-rectangular-number
  (lambda (z ms n)
    (if (or (eq? ms 'ureal) (eq? ms 'real))
        (or z (make-rectangular 0 n))
        (and (eq? (state-type ms) 'real)
             (or z (make-rectangular (state-part ms) n))))))

(mknum-state prefix0 (r ex)           ; start state
   #f
   [#\# (prefix1 r ex)]
   [else (num0 r ex)])

(mknum-state prefix1 (r ex)           ; saw leading #
   #f
   [(digit 10) (not r6rs?) (prefix2 d ex)]
   [#\e (prefix3 r 'e)]
   [#\i (prefix3 r 'i)]
   [#\b (prefix6 2 ex)]
   [#\o (prefix6 8 ex)]
   [#\d (prefix6 10 ex)]
   [#\x (prefix6 16 ex)])

(mknum-state prefix2 (r ex)           ; saw digit after #
   #f
   [(digit 10) (fx< r 37) (prefix2 (+ (* r 10) d) ex)]
   [#\r (fx< 1 r 37) (prefix6 r ex)])

(mknum-state prefix3 (r ex)           ; saw exactness prefix
   #f
   [#\# (prefix4 ex)]
   [else (num0 r ex)])

(mknum-state prefix4 (ex)             ; saw # after exactness
   #f
   [(digit 10) (not r6rs?) (prefix5 d ex)]
   [#\b (num0 2 ex)]
   [#\o (num0 8 ex)]
   [#\d (num0 10 ex)]
   [#\x (num0 16 ex)])

(mknum-state prefix5 (r ex)           ; saw # digit after exactness
   #f
   [(digit 10) (fx< r 37) (prefix5 (+ (* r 10) d) ex)]
   [#\r (fx< 1 r 37) (num0 r ex)])

(mknum-state prefix6 (r ex)           ; saw radix prefix
   #f
   [#\# (prefix7 r)]
   [else (num0 r ex)])

(mknum-state prefix7 (r)              ; saw # after radix
   #f
   [#\e (num0 r 'e)]
   [#\i (num0 r 'i)])

(mknum-state num0 (r ex)              ; saw prefix, if any
   #f
   [(digit r) (num2 'ureal r ex plus d)]
   [#\. (or (not r6rs?) (fx= r 10)) (float0 'ureal r ex plus)]
   [#\+ (num1 'real r ex plus)]
   [#\- (num1 'real r ex minus)])

(mknum-state num1 (ms r ex s)            ; saw sign
   #f
   [(digit r) (num2 ms r ex s d)]
   [#\. (or (not r6rs?) (fx= r 10)) (float0 ms r ex s)]
   [#\i (num3 ms r ex s)]
   [#\n (let ([z (if (eq? ex 'e) 'norep z)]) (nan0 ms r ex s))])

(mknum-state num2 (ms r ex s n)          ; saw digit
   (finish-number z ms (or z (make-part (or ex 'e) s n)))
   [(digit r) (num2 ms r ex s (or z (+ (* n r) d)))]
   [#\/ (rat0 ms r ex s (or z (make-part (or ex 'e) plus n)))]
   [#\| (mwidth0 ms r ex (or z (make-part (or ex 'i) s n)))]
   [#\. (or (not r6rs?) (fx= r 10)) (float1 ms r ex s n (fx+ i 1) 0)]
   [#\# (not r6rs?) (numhash ms r ex s (or z (* n r)))]
   [(#\e #\s #\f #\d #\l) (or (not r6rs?) (fx= r 10)) (exp0 ms r ex s n)]
   [else (complex0 ms r ex (or z (make-part (or ex 'e) s n)))])

(mknum-state num3 (ms r ex s)            ; saw "i" after sign
   (finish-rectangular-number z ms (or z (make-part (or ex 'e) s 1)))
   [#\n (let ([z (if (eq? ex 'e) 'norep z)]) (inf0 ms r ex s))])

(mknum-state inf0 (ms r ex s)            ; saw "in" after sign
   #f
   [#\f (inf1 ms r ex s)])

(mknum-state inf1 (ms r ex s)            ; saw "inf" after sign
   #f
   [#\. (inf2 ms r ex s)])

(mknum-state inf2 (ms r ex s)            ; saw "inf." after sign
   #f
   [#\0 (inf3 ms r ex s)])

(mknum-state inf3 (ms r ex s)            ; saw "inf.0" after sign
   (finish-number z ms (or z (s +inf.0)))
   [else (complex0 ms r ex (or z (s +inf.0)))])

(mknum-state nan0 (ms r ex s)            ; saw "n" after sign
   #f
   [#\a (nan1 ms r ex s)])

(mknum-state nan1 (ms r ex s)            ; saw "na" after sign
   #f
   [#\n (nan2 ms r ex s)])

(mknum-state nan2 (ms r ex s)            ; saw "nan" after sign
   #f
   [#\. (nan3 ms r ex s)])

(mknum-state nan3 (ms r ex s)            ; saw "nan." after sign
   #f
   [#\0 (nan4 ms r ex s)])

(mknum-state nan4 (ms r ex s)            ; saw "nan.0" after sign
   (finish-number z ms +nan.0)
   [else (complex0 ms r ex +nan.0)])

(mknum-state numhash (ms r ex s n)       ; saw # after integer
   (finish-number z ms (or z (make-part (or ex 'i) s n)))
   [#\/ (rat0 ms r ex s (or z (make-part (or ex 'i) plus n)))]
   [#\. (floathash ms r ex s n (fx+ i 1) 0)]
   [#\# (numhash ms r ex s (or z (* n r)))]
   [(#\e #\s #\f #\d #\l) (exp0 ms r ex s n)]
   [else (complex0 ms r ex (or z (make-part (or ex 'i) s n)))])

; can't embed sign in m since we might end up in exp0 and then on
; to make-part, which counts on sign being separate
(mknum-state rat0 (ms r ex s m)          ; saw slash
   #f
   [#\0 (not (eq? ex 'i))
        (rat1a ms r ex s m)]
   [(digit r) (rat1 ms r ex s m d)])

(mknum-state rat1a (ms r ex s m)         ; exact zero denominator so far
   'norep
   [#\0 (not (eq? ex 'i))
        (rat1a ms r ex s m)]
   [(digit r) (rat1 ms r ex s m d)]
   [#\# (not r6rs?) (let ([z (if (eq? ex 'e) 'norep z)]) (rathash ms r ex s m 0))]
   [(#\e #\s #\f #\d #\l) (or (not r6rs?) (fx= r 10)) (let ([z 'norep]) (exp0 ms r ex s z))]
   [else (let ([z 'norep]) (complex0 ms r ex z))])

(mknum-state rat1 (ms r ex s m n)        ; saw denominator digit
   (finish-number z ms (or z (/ m (make-part (or ex 'e) s n))))
   [(digit r) (rat1 ms r ex s m (or z (+ (* n r) d)))]
   [#\# (not r6rs?) (rathash ms r ex s m (or z (* n r)))]
   [(#\e #\s #\f #\d #\l) (or (not r6rs?) (fx= r 10)) (exp0 ms r ex s (or z (/ m (make-part (or ex 'e) plus n))))]
   [else (complex0 ms r ex (or z (/ m (make-part (or ex 'e) s n))))])

(mknum-state rathash (ms r ex s m n)     ; saw # after denominator
   (finish-number z ms (or z (/ m (make-part (or ex 'i) s n))))
   [#\# (rathash ms r ex s m (or z (* n r)))]
   [(#\e #\s #\f #\d #\l) (exp0 ms r ex s (or z (/ m (make-part (or ex 'i) plus n))))]
   [else (complex0 ms r ex (or z (/ m (make-part (or ex 'i) s n))))])

(mknum-state float0 (ms r ex s)           ; saw leading decimal point
   #f
   [(digit r) (float1 ms r ex s 0 i d)])

(mknum-state float1 (ms r ex s m j n)     ; saw fraction digit at j
   (finish-number z ms (or z (make-part (or ex 'i) s (+ m (* n (expt r (- j i)))))))
   [(digit r) (float1 ms r ex s m j (or z (+ (* n r) d)))]
   [#\| (mwidth0 ms r ex (or z (make-part (or ex 'i) s (+ m (* n (expt r (- j i)))))))]
   [#\# (not r6rs?) (floathash ms r ex s m j (or z (* n r)))]
   [(#\e #\s #\f #\d #\l) (exp0 ms r ex s (or z (+ m (* n (expt r (- j i))))))]
   [else (complex0 ms r ex (or z (make-part (or ex 'i) s (+ m (* n (expt r (- j i)))))))])

(mknum-state floathash (ms r ex s m j n)  ; seen hash(es), now in fraction
   (finish-number z ms (or z (make-part (or ex 'i) s (+ m (* n (expt r (- j i)))))))
   [#\# (floathash ms r ex s m j (or z (* n r)))]
   [(#\e #\s #\f #\d #\l) (exp0 ms r ex s (or z (+ m (* n (expt r (- j i))))))]
   [else (complex0 ms r ex (or z (make-part (or ex 'i) s (+ m (* n (expt r (- j i)))))))])

(mknum-state exp0 (ms r ex s m)              ; saw exponent flag
   #f
   [(digit r) (exp2 ms r ex s m plus d)]
   [#\+ (exp1 ms r ex s m plus)]
   [#\- (exp1 ms r ex s m minus)])

(mknum-state exp1 (ms r ex sm m s)           ; saw exponent sign
   #f
   [(digit r) (exp2 ms r ex sm m s d)])

(mknum-state exp2 (ms r ex sm m s n)         ; saw exponent digit
   (finish-number z ms (or z (make-part (or ex 'i) sm m r (s n))))
   [(digit r) (exp2 ms r ex sm m s (or z (+ (* n r) d)))]
   [#\| (mwidth0 ms r ex (or z (make-part (or ex 'i) sm m r (s n))))]
   [else (complex0 ms r ex (or z (make-part (or ex 'i) sm m r (s n))))])

(mknum-state mwidth0 (ms r ex n)             ; saw vertical bar
   #f
   [(digit 10) (mwidth1 ms r ex n)])
  
(mknum-state mwidth1 (ms r ex n)             ; saw digit after vertical bar
   (finish-number z ms n)
   [(digit 10) (mwidth1 ms r ex n)]
   [else (complex0 ms r ex n)])

(mknum-state complex0 (ms r ex n)
   #f
   [#\@ (or (eq? ms 'real) (eq? ms 'ureal))
        (complex1 (make-state 'real@ n) r ex)]
   [#\+ (or (eq? ms 'real) (eq? ms 'ureal))
        (num1 (make-state 'real n) r ex plus)]
   [#\- (or (eq? ms 'real) (eq? ms 'ureal))
        (num1 (make-state 'real n) r ex minus)]
   [#\i (or (eq? ms 'real) (and (state? ms) (eq? (state-type ms) 'real)))
        (complex2 ms n)])

(mknum-state complex1 (ms r ex) ; like num0 but knows ms already
   #f
   [(digit r) (num2 ms r ex plus d)]
   [#\. (float0 ms r ex plus)]
   [#\+ (num1 ms r ex plus)]
   [#\- (num1 ms r ex minus)])

(mknum-state complex2 (ms n)
   (finish-rectangular-number z ms n))

; str->num returns
;   (or z <number>)    valid number
;   norep              syntactically valid but cannot represent
;   #f                 valid prefix (eof/end-of-string)
;   bogus              invalid prefix
(set! $str->num
  (lambda (z x k r ex r6rs?)
    (prefix0 z x k 0 r6rs? r ex)))

)) ; let

(define string->number
   (case-lambda
      [(x) (string->number x 10)]
      [(x r)
       (unless (string? x)
          ($oops 'string->number "~s is not a string" x))
       (unless (and (fixnum? r) (fx< 1 r 37))
          ($oops 'string->number "~s is not a valid radix" r))
       (and (eq? ($str->num 'cool x (string-length x) r #f #f) 'cool)
            ($str->num #f x (string-length x) r #f #f))]))

(define-who #(r6rs: string->number)
  (case-lambda
    [(x) (string->number x 10)]
    [(x r)
     (unless (string? x) ($oops who "~s is not a string" x))
     (unless (memq r '(2 8 10 16)) ($oops who "~s is not a valid radix" r))
     (and (eq? ($str->num 'cool x (string-length x) r #f #t) 'cool)
          ($str->num #f x (string-length x) r #f #t))]))

(define-who number->string
  (case-lambda
    [(x)
     (unless (number? x) ($oops who "~s is not a number" x))
     (format "~d" x)]
    [(x r)
     (unless (number? x) ($oops who "~s is not a number" x))
     (unless (and (fixnum? r) (fx< 1 r 37))
       ($oops who "~s is not a valid radix" r))
     (parameterize ([print-radix r]) (format "~a" x))]
    [(x r m)
     (unless (or (and (fixnum? m) (fx> m 0))
                 (and (bignum? m) (> m 0)))
       ($oops who "~s is not a valid precision" m))
     (unless (inexact? x)
       ($oops who "a precision is specified and ~s is not inexact" x))
     (parameterize ([print-radix r] [print-precision m]) (format "~a" x))]))

(define-who #(r6rs: number->string)
  (case-lambda
    [(x)
     (unless (number? x) ($oops who "~s is not a number" x))
     (format "~d" x)]
    [(x r)
     (unless (number? x) ($oops who "~s is not a number" x))
     (unless (memq r '(2 8 10 16))
       ($oops who "~s is not a valid radix" r))
     (parameterize ([print-radix r]) (format "~a" x))]
    [(x r m)
     (unless (number? x) ($oops who "~s is not a number" x))
     (unless (eq? r 10)
       (if (memq r '(2 8 16))
           ($oops who "a precision is specified and radix ~s is not 10" r)
           ($oops who "~s is not a valid radix" r)))
     (unless (or (and (fixnum? m) (fx> m 0))
                 (and (bignum? m) ($bigpositive? m)))
       ($oops who "~s is not a valid precision" m))
     (unless (inexact? x)
       ($oops who "a precision is specified and ~s is not inexact" x))
     (parameterize ([print-radix r] [print-precision m]) (format "~a" x))]))
