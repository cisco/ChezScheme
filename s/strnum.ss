;;; strnum.ss
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


#|
R6RS Section 4.2.8 (Numbers) says:

  A representation of a number object may be specified to be either
  exact or inexact by a prefix. The prefixes are #e for exact, and #i
  for inexact. An exactness prefix may appear before or after any radix
  prefix that is used.  If the representation of a number object has
  no exactness prefix, the constant is inexact if it contains a decimal
  point, an exponent, or a nonempty mantissa width; otherwise it is exact.

This specifies the exactness of the result.  It doesn't specify precisely
the number produced when there is a mix of exact and inexact subparts
and what happens if an apparently exact subpart of an inexact number
cannot be represented.

Possible options include:

(A) Treat each subpart as inexact if the #i prefix is specified or the
    #e prefix is not specified and any subpart is inexact, i.e.,
    contains a decimal point, exponent, or mantissa width.  Treat each
    subpart as exact if the #e prefix is specified or if the #i prefix
    is not specified and each subpart is exact.

(B) Treat each subpart as exact or inexact in isolation and use the
    usual rules for preserving inexactness when combining the subparts.
    Apply inexact to the result if #i is present and exact to the
    result if #e is present.

(C) If #e and #i are not present, treat each subpart as exact or inexact
    in isolation and use the usual rules for preserving inexactness when
    combining the subparts.  If #e is present, treat each subpart
    as exact.  If #i is present, treat each subpart as inexact.

Also, the R6RS description of string->number says:

  If string is not a syntactically valid notation for a number object
  or a notation for a rational number object with a zero denominator,
  then string->number returns #f.

We take "zero denomintor" here to mean "exact zero denominator", and
treat, e.g., #i1/0, as +inf.0.

                       A                B                C
0/0                    #f               #f               #f
1/0                    #f               #f               #f
#e1/0                  #f               #f               #f
#i1/0                +inf.0             #f             +inf.0
1/0+1.0i          +nan.0+1.0i           #f               #f
1.0+1/0i           1.0+nan.0i           #f               #f
#e1e1000         (expt 10 1000)         #f         (expt 10 1000)
0@1.0               0.0+0.0i             0                0
1.0+0i              1.0+0.0i            1.0              1.0

This code implements Option C.  It computes inexact components with
exact arithmetic where possible, however, before converting them into
inexact numbers, to insure the greatest possible accuracy.

Rationale for Option C: B and C adhere most closely to the semantics of
the individual / and make-rectangular operators, sometimes produce exact
results when A would produce inexact results, and do not require a scan
of the entire number first (as with A) to determine the (in)exactness of
the result.  C takes into account the known (in)exactness of the result
to represent some useful values that B cannot, such as #i1/0 and #e1e1000.

R6RS doesn't say is what string->number should return for syntactically
valid numbers (other than exact numbers with a zero denominator) for
which the implementation has no representation, such as exact 1@1 in an
implementation such as Chez Scheme that represents all complex numbers in
rectangular form.  Options include returning an approximation represented
as an inexact number (so that the result, which should be exact, isn't
exact), returning an approximation represented as an exact number (so
that the approximation misleadingly represents itself as exact), or to
admit an implementation restriction.  We choose the to return an inexact
result for 1@1 (extending the set of situations where numeric constants
are implicitly inexact) and treat #e1@1 as violating an implementation
restriction, with string->number returning #f and the reader raising
an exception.
|#

(begin
(let ()
;;   (mknum-state <state name>
;;                <expression if end of string found>
;;                [<transition key> <state transition>]
;;                ...)
(define-syntax mknum-state
  (lambda (e)
    (syntax-case e ()
      [(_key name (id ...) efinal clause ...)
       (with-implicit (_key str k i r6rs? !r6rs x1 c d)
         (let ()
           (define mknum-state-test
             (lambda (key)
               (syntax-case key (-)
                 [char
                   (char? (datum char))
                   #'(char=? c char)]
                 [(char1 - char2)
                  #'(char<=? char1 c char2)]
                 [(key ...)
                  `(,#'or ,@(map mknum-state-test #'(key ...)))])))
           (define mknum-call
             (lambda (incr? call)
               (syntax-case call (let)
                 [(let ([x e] ...) call)
                  (with-syntax ([call (mknum-call incr? #'call)])
                    #'(let ([x e] ...) call))]
                 [(e1 e2 ...)
                  (if incr?
                      #'(e1 str k (fx+ i 1) r6rs? !r6rs x1 e2 ...)
                      #'(e1 str k i r6rs? !r6rs x1 e2 ...))])))
           (define mknum-state-help
             (lambda (ls)
               (syntax-case ls (else)
                 [() #''bogus]
                 [((else call)) (mknum-call #f #'call)]
                 [_ (with-syntax ((rest (mknum-state-help (cdr ls))))
                      (syntax-case (car ls) (digit)
                        [((digit r) call)
                         (with-syntax ([call (mknum-call #t #'call)])
                           #'(let ((d (ascii-digit-value c r)))
                               (if d call rest)))]
                        [((digit r) fender call)
                         (with-syntax ([call (mknum-call #t #'call)])
                           #'(let ((d (ascii-digit-value c r)))
                               (if (and d fender) call rest)))]
                        [(key call)
                         (with-syntax ([test (mknum-state-test #'key)]
                                       [call (mknum-call #t #'call)])
                           #'(if test call rest))]
                        [(key fender call)
                         (with-syntax ([test (mknum-state-test #'key)]
                                       [call (mknum-call #t #'call)])
                           #'(if (and test fender) call rest))]))])))
           (with-syntax ([rest (mknum-state-help #'(clause ...))]
                         [efinal (syntax-case #'efinal ()
                                   [#f #'efinal]
                                   [_ #'(if (and r6rs? !r6rs) '!r6rs efinal)])])
             #'(define name
                 (lambda (str k i r6rs? !r6rs x1 id ...)
                   (if (= i k)
                       efinal
                       (let ([c (char-downcase (string-ref str i))])
                         rest)))))))])))

(define ascii-digit-value
  (lambda (c r)
    (let ([v (cond
               [(char<=? #\0 c #\9) (char- c #\0)]
               [(char<=? #\a c #\z) (char- c #\W)]
               [else 36])])
      (and (fx< v r) v))))

; variables automatically maintained and passed by the mknum macro:
;   str: string
;     k: string length
;     i: index into string, 0 <= i < k
; r6rs?: if #t, return !r6rs for well-formed non-r6rs features
; !r6rs: if #t, seen non-r6rs feature
;    x1: first part of complex number when ms = imag or angle: number, thunk, or norep
; variables automatically created by the mknum macro:
;     c: holds current character
;     d: holds digit value of c in a digit clause
; other "interesting" variables:
;     r: radix, 2 <= r <= 36 (can be outside this range while constructing #<r>r prefix)
;    ex: exactness: 'i, 'e, or #f
;     s: function to add sign to number
;    ms: meta-state: real, imag, angle
;     n: exact integer
;     m: exact or inexact integer
;     w: exact or inexact integer or norep
;    wi: exact integer or norep or 'inf or 'nan
;     x: number, thunk, procedure taking rounding procedure, or norep
;     e: exact integer exponent
;    mw: exact integer mantissa width
;    i?: #t if number should be made inexact
; invariant: (thunk) != exact 0.

; The sign of the mantissa cannot be put on until a number has
; been made inexact (if necessary) to make sure zero gets the right sign.

(let ()

(define plus (lambda (x) x))
(define minus -)

(define (implied-i ex) (if (not ex) 'i ex))

(define noround (lambda (x) x))
(define rounder
  (lambda (p ex)
    (if (zero? p)
        (lambda (n) 0)
        (lambda (n)
          (let ([a (numerator n)]
                [b (denominator n)])
	    (let* ([a-bits (bitwise-length a)]
                   [b-bits (bitwise-length b)]
                   [d (- a-bits b-bits)]
                   ;; If `p` is large, we might run out of memory by
                   ;; shifting by it directly, but in some cases, the right
                   ;; result should fit into memory no matter how big `p` is.
                   [p (cond
                        [(not (eq? ex 'e))
                         ;; end result will have at most 53 bits, anyway, so
                         ;; bound p; we don't need a tight bound, and adding 2
                         ;; extra bits over `double` precision to make sure
                         ;; rounding will be right
                         (min p (+ a-bits b-bits 53 2))]
                        [(= b (bitwise-arithmetic-shift-left 1 (- b-bits 1)))
                         ;; no need for extra precision if the
                         ;; denominator is a power of 2
                         (min p a-bits)]
                        [else p])])
	      (let*-values
		  ([(a b)
		    (if (positive? d)
			(values a (bitwise-arithmetic-shift-left b d))
			(values (bitwise-arithmetic-shift-left a (- d)) b))]
		   [(b d)
		    (if (>= a b)
			(values (bitwise-arithmetic-shift-left b 1) (+ d 1))
			(values b d))]
		   [(q r)
		    (div-and-mod (bitwise-arithmetic-shift-left a (+ p 1))
				 b)])
		(* (+ q
		      (cond [(not (bitwise-bit-set? q 0)) 0]
			    [(or (not (zero? r))
				 (bitwise-bit-set? q 1)) 1]
			    [else -1]))
		   (expt 2 (- d p 1))))))))))

(define make-part
  (lambda (i? s n)
    (s (if i? (inexact n) n))))

(define make-part/exponent
  (lambda (i? s t wi r e)
    ; get out quick for really large/small exponents, like 1e1000000000
    ; no need for great precision here; using 2x the min/max base two
    ; exponent, which should be conservative for all bases.  1x should
    ; actually work for positive n, but for negative e we need something
    ; smaller than 1x to allow denormalized numbers.
    ; s must be the actual sign of the result, with w >= 0
    (define max-float-exponent
      (float-type-case
        [(ieee) 1023]))
    (define min-float-exponent
      (float-type-case
       [(ieee) -1023]))
    (cond
      [(eq? wi 'norep) 'norep]
      [(eq? wi 'inf) (if i? (s +inf.0) 'norep)]
      [(eq? wi 'nan) (if i? +nan.0 'norep)]
      [i? (s (if (eqv? wi 0)
                 0.0
                 (if (<= (* min-float-exponent 2)
                         (+ e (/ (- (integer-length (numerator wi))
                                    (integer-length (denominator wi)))
                                 (log r 2)))
                         (* max-float-exponent 2))
                     (inexact (t (* wi (expt r e))))
                     (if (< e 0) 0.0 +inf.0))))]
      [(eqv? wi 0) 0]
      [else (lambda () (s (t (* wi (expt r e)))))])))

(define (thaw x) (if (procedure? x) (x) x))

(define finish-number
  (lambda (ms ex x1 x)
    (case ms
      [(real ureal) (if (procedure? x) (x) x)]
      [(angle)
       (cond
         [(or (eq? x1 'norep) (eq? x 'norep)) 'norep]
         [(eqv? x1 0) 0]
         [(eqv? x 0) (thaw x1)]
         [(eq? ex 'e) 'norep]
         [else (make-polar (thaw x1) (thaw x))])]
      [else #f])))

(define finish-rectangular-number
  (lambda (ms x1 x)
    (case ms
      [(real ureal)
       (if (eq? x 'norep)
           'norep
           (make-rectangular 0 (thaw x)))]
      [(imag)
       (if (or (eq? x1 'norep) (eq? x 'norep))
           'norep
           (make-rectangular (thaw x1) (thaw x)))]
      [else #f])))

(mknum-state prefix0 (r ex)              ; start state
  #f
  [#\# (prefix1 r ex)]
  [else (num0 r ex)])

(mknum-state prefix1 (r ex)              ; saw leading #
  #f
  [(digit 10) (let ([!r6rs #t]) (prefix2 d ex))]
  [#\e (prefix3 r 'e)]
  [#\i (prefix3 r 'i)]
  [#\b (prefix6 2 ex)]
  [#\o (prefix6 8 ex)]
  [#\d (prefix6 10 ex)]
  [#\x (prefix6 16 ex)])

(mknum-state prefix2 (r ex)              ; saw digit after #
  #f
  [(digit 10) (fx< r 37) (prefix2 (+ (* r 10) d) ex)]
  [#\r (fx< 1 r 37) (prefix6 r ex)])

(mknum-state prefix3 (r ex)              ; saw exactness prefix
  #f
  [#\# (prefix4 ex)]
  [else (num0 r ex)])

(mknum-state prefix4 (ex)                ; saw # after exactness
  #f
  [(digit 10) (let ([!r6rs #t]) (prefix5 d ex))]
  [#\b (num0 2 ex)]
  [#\o (num0 8 ex)]
  [#\d (num0 10 ex)]
  [#\x (num0 16 ex)])

(mknum-state prefix5 (r ex)              ; saw # digit after exactness
  #f
  [(digit 10) (fx< r 37) (prefix5 (+ (* r 10) d) ex)]
  [#\r (fx< 1 r 37) (num0 r ex)])

(mknum-state prefix6 (r ex)              ; saw radix prefix
  #f
  [#\# (prefix7 r)]
  [else (num0 r ex)])

(mknum-state prefix7 (r)                 ; saw # after radix
  #f
  [#\e (num0 r 'e)]
  [#\i (num0 r 'i)])

(mknum-state num0 (r ex)                 ; saw prefix, if any
  #f
  [(digit r) (num2 r ex 'ureal plus d)]
  [#\. (let ([!r6rs (or !r6rs (not (fx= r 10)))]) (float0 r ex 'ureal plus))]
  [#\+ (num1 r ex 'real plus)]
  [#\- (num1 r ex 'real minus)])

(mknum-state num1 (r ex ms s)            ; saw sign
  #f
  [(digit r) (num2 r ex ms s d)]
  [#\. (let ([!r6rs (or !r6rs (not (fx= r 10)))]) (float0 r ex ms s))]
  [#\i (num3 r ex ms s)]
  [#\n (nan0 r ex ms s)])

(mknum-state num2 (r ex ms s n)          ; saw digit
  (finish-number ms ex x1 (make-part (eq? ex 'i) s n))
  [(digit r) (num2 r ex ms s (+ (* n r) d))]
  [#\/ (rat0 r ex ex ms s (make-part #f plus n))]
  [#\| (mwidth0 r ex ms (lambda (t) (make-part (not (eq? ex 'e)) s (t n))))]
  [#\. (let ([!r6rs (or !r6rs (not (fx= r 10)))]) (float1 r ex ms s n (fx+ i 1) 0))]
  [#\# (let ([!r6rs #t]) (numhash r ex ms s (* n r)))]
  [(#\e #\s #\f #\d #\l) (let ([!r6rs (or !r6rs (not (fx= r 10)))]) (exp0 r ex ms s n))]
  [else (complex0 r ex ms (make-part (eq? ex 'i) s n))])

(mknum-state num3 (r ex ms s)            ; saw "i" after sign
  (finish-rectangular-number ms x1 (make-part (eq? ex 'i) s 1))
  [#\n (inf0 r ex ms s)])

(mknum-state inf0 (r ex ms s)            ; saw "in" after sign
  #f
  [#\f (inf1 r ex ms s)])

(mknum-state inf1 (r ex ms s)            ; saw "inf" after sign
  #f
  [#\. (inf2 r ex ms s)])

(mknum-state inf2 (r ex ms s)            ; saw "inf." after sign
  #f
  [#\0 (inf3 r ex ms s)])

(mknum-state inf3 (r ex ms s)            ; saw "inf.0" after sign
  (finish-number ms ex x1 (if (eq? ex 'e) 'norep (s +inf.0)))
  [else (complex0 r ex ms (if (eq? ex 'e) 'norep (s +inf.0)))])

(mknum-state nan0 (r ex ms s)            ; saw "n" after sign
  #f
  [#\a (nan1 r ex ms s)])

(mknum-state nan1 (r ex ms s)            ; saw "na" after sign
  #f
  [#\n (nan2 r ex ms s)])

(mknum-state nan2 (r ex ms s)            ; saw "nan" after sign
  #f
  [#\. (nan3 r ex ms s)])

(mknum-state nan3 (r ex ms s)            ; saw "nan." after sign
  #f
  [#\0 (nan4 r ex ms s)])

(mknum-state nan4 (r ex ms s)            ; saw "nan.0" after sign
  (finish-number ms ex x1 (if (eq? ex 'e) 'norep +nan.0))
  [else (complex0 r ex ms +nan.0)])

(mknum-state numhash (r ex ms s n)       ; saw # after integer
  (finish-number ms ex x1 (make-part (not (eq? ex 'e)) s n))
  [#\/ (rat0 r (implied-i ex) ex ms s (make-part #f plus n))]
  [#\. (floathash r ex ms s n (fx+ i 1) 0)]
  [#\# (numhash r ex ms s (* n r))]
  [(#\e #\s #\f #\d #\l) (exp0 r ex ms s n)]
  [else (complex0 r ex ms (make-part (not (eq? ex 'e)) s n))])

; can't embed sign in m since we might end up in exp0 and then on
; to make-part, which counts on sign being separate
(mknum-state rat0 (r ex d-ex ms s m)          ; saw slash
  #f
  [(digit r) (rat1 r ex d-ex ms s m d)])

(define (mkrat i? d-i? s nan inf p q)
  (if (eqv? q 0)
      (if d-i? (s (/ (inexact p) 0.0)) (if (eqv? p 0) nan inf))
      (let ([r (/ p q)])
        (s (if (or i? d-i?) (inexact r) r)))))

(mknum-state rat1 (r ex d-ex ms s m n)        ; saw denominator digit
  (finish-number ms ex x1 (mkrat (eq? ex 'i) (eq? d-ex 'i) s 'norep 'norep m (make-part #f plus n)))
  [(digit r) (rat1 r ex d-ex ms s m (+ (* n r) d))]
  [#\# (let ([!r6rs #t]) (rathash r ex ms s m (* n r)))]
  [(#\e #\s #\f #\d #\l) (let ([!r6rs #t]) (exp0 r ex ms s (mkrat #f #f plus 'nan 'inf m (make-part #f plus n))))]
  [else (complex0 r ex ms (mkrat #f (eq? d-ex 'i) s 'norep 'norep m (make-part #f plus n)))])

(mknum-state rathash (r ex ms s m n)    ; saw # after denominator
  (finish-number ms ex x1 (mkrat #f (not (eq? ex 'e)) s 'norep 'norep m (make-part #f plus n)))
  [#\# (rathash r ex ms s m (* n r))]
  [(#\e #\s #\f #\d #\l) (exp0 r ex ms s (mkrat #f #f plus 'nan 'inf m (make-part #f plus n)))]
  [else (complex0 r ex ms (mkrat #f (not (eq? ex 'e)) s 'norep 'norep m (make-part #f plus n)))])

(mknum-state float0 (r ex ms s)          ; saw leading decimal point
  #f
  [(digit r) (float1 r ex ms s 0 i d)])

(mknum-state float1 (r ex ms s m j n)    ; saw fraction digit at j
  (finish-number ms ex x1 (make-part (not (eq? ex 'e)) s (+ m (* n (expt r (- j i))))))
  [(digit r) (float1 r ex ms s m j (+ (* n r) d))]
  [#\| (mwidth0 r ex ms (lambda (t) (make-part (not (eq? ex 'e)) s (t (+ m (* n (expt r (- j i))))))))]
  [#\# (let ([!r6rs #t]) (floathash r ex ms s m j (* n r)))]
  [(#\e #\s #\f #\d #\l) (exp0 r ex ms s (+ m (* n (expt r (- j i)))))]
  [else (complex0 r ex ms (make-part (not (eq? ex 'e)) s (+ m (* n (expt r (- j i))))))])

(mknum-state floathash (r ex ms s m j n) ; seen hash(es), now in fraction
  (finish-number ms ex x1 (make-part (not (eq? ex 'e)) s (+ m (* n (expt r (- j i))))))
  [#\# (floathash r ex ms s m j (* n r))]
  [(#\e #\s #\f #\d #\l) (exp0 r ex ms s (+ m (* n (expt r (- j i)))))]
  [else (complex0 r ex ms (make-part (not (eq? ex 'e)) s (+ m (* n (expt r (- j i))))))])

(mknum-state exp0 (r ex ms s wi)          ; saw exponent flag
  #f
  [(digit r) (exp2 r ex ms s wi plus d)]
  [#\+ (exp1 r ex ms s wi plus)]
  [#\- (exp1 r ex ms s wi minus)])

(mknum-state exp1 (r ex ms sm wi s)       ; saw exponent sign
  #f
  [(digit r) (exp2 r ex ms sm wi s d)])

(mknum-state exp2 (r ex ms sm wi s e)     ; saw exponent digit(s)
  (finish-number ms ex x1 (make-part/exponent (not (eq? ex 'e)) sm noround wi r (s e)))
  [(digit r) (exp2 r ex ms sm wi s (+ (* e r) d))]
  [#\| (mwidth0 r ex ms (lambda (t) (make-part/exponent (not (eq? ex 'e)) sm t wi r (s e))))]
  [else (complex0 r ex ms (make-part/exponent (not (eq? ex 'e)) sm noround wi r (s e)))])

(mknum-state mwidth0 (r ex ms x)        ; saw vertical bar
  #f
  [(digit 10) (mwidth1 r ex ms d x)])

(mknum-state mwidth1 (r ex ms mw x)     ; saw digit after vertical bar
  (finish-number ms ex x1 (x (rounder mw ex)))
  [(digit 10) (mwidth1 r ex ms (+ (* 10 mw) d) x)]
  [else (complex0 r ex ms (x (rounder mw ex)))])

(mknum-state complex0 (r ex ms x)        ; saw end of real part before end of string
  (assert #f) ; should arrive here only from else clauses, thus not at the end of the string
  [#\@ (memq ms '(real ureal)) (let ([x1 x]) (complex1 r ex 'angle))]
  [#\+ (memq ms '(real ureal)) (let ([x1 x]) (num1 r ex 'imag plus))]
  [#\- (memq ms '(real ureal)) (let ([x1 x]) (num1 r ex 'imag minus))]
  [#\i (memq ms '(real imag)) (complex2 ms x)])

(mknum-state complex1 (r ex ms)          ; seen @.  like num0 but knows ms already
  #f
  [(digit r) (num2 r ex ms plus d)]
  [#\. (let ([!r6rs (or !r6rs (not (fx= r 10)))]) (float0 r ex ms plus))]
  [#\+ (num1 r ex ms plus)]
  [#\- (num1 r ex ms minus)])

(mknum-state complex2 (ms x)             ; saw i after real or imag
  (finish-rectangular-number ms x1 x))

; str->num returns
;   <number>    syntactically valid, representable number
;   !r6rs       syntactically valid non-r6rs syntax in #!r6rs mode
;   norep       syntactically valid but cannot represent
;   #f          syntactically valid prefix (eof/end-of-string)
;   bogus       syntactically invalid prefix
(set! $str->num
  (lambda (str k r ex r6rs?)
    (prefix0 str k 0 r6rs? #f #f r ex)))
)) ; let

(define string->number
  (case-lambda
    [(x) (string->number x 10)]
    [(x r)
     (unless (string? x)
       ($oops 'string->number "~s is not a string" x))
     (unless (and (fixnum? r) (fx< 1 r 37))
       ($oops 'string->number "~s is not a valid radix" r))
     (let ([z ($str->num x (string-length x) r #f #f)])
       (and (number? z) z))]))

(define-who #(r6rs: string->number)
  (case-lambda
    [(x) (string->number x 10)]
    [(x r)
     (unless (string? x) ($oops who "~s is not a string" x))
     (unless (memq r '(2 8 10 16)) ($oops who "~s is not a valid radix" r))
     (let ([z ($str->num x (string-length x) r #f #t)])
       (and (number? z) z))]))

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
     (unless (number? x) ($oops who "~s is not a number" x))
     (unless (and (fixnum? r) (fx< 1 r 37))
       ($oops who "~s is not a valid radix" r))
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
     (unless (eqv? r 10)
       (if (memq r '(2 8 16))
           ($oops who "a precision is specified and radix ~s is not 10" r)
           ($oops who "~s is not a valid radix" r)))
     (unless (or (and (fixnum? m) (fx> m 0))
                 (and (bignum? m) ($bigpositive? m)))
       ($oops who "~s is not a valid precision" m))
     (unless (inexact? x)
       ($oops who "a precision is specified and ~s is not inexact" x))
     (parameterize ([print-radix r] [print-precision m]) (format "~a" x))]))
)
