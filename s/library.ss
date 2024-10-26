;;; library.ss
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

;;; Library entries should not contain references that could themselves
;;; compile into library entries.  (Actually it will work as long as the
;;; use follows the definition, but...)  Consequently they should be
;;; kept simple.

(eval-when (compile)
   (optimize-level 3)
   (generate-inspector-information #f)
   ($compile-profile #f)
   ($optimize-closures #t)
   (run-cp0 (default-run-cp0))
   (generate-interrupt-trap #f)
   ($track-dynamic-closure-counts #f))

(eval-when (compile)
(define-syntax define-library-entry
  (lambda (x)
    (define name->libspec
      (lambda (name)
        (or ($sgetprop name '*libspec* #f)
            ($oops 'define-library-entry "~s is undefined" name))))
    (define name->does-not-expect-headroom-libspec
      (lambda (name)
        (or ($sgetprop name '*does-not-expect-headroom-libspec* #f)
            ($oops 'define-library-entry "~s is missing no headroom libspec" name))))
    (syntax-case x ()
      [(_ (name . args) e1 e2 ...)
       (identifier? #'name)
       (let ([libspec (name->libspec (datum name))]
             [does-not-expect-headroom-libspec (name->does-not-expect-headroom-libspec (datum name))])
         (with-syntax ([index (libspec-index libspec)]
                       [does-not-expect-headroom-index (libspec-index does-not-expect-headroom-libspec)]
                       [libspec (datum->syntax #'name libspec)]
                       [does-not-expect-headroom-libspec (datum->syntax #'name does-not-expect-headroom-libspec)])
           ; NB: we are duplicating code here, because looking up the library entry fails on startup.
           #'(begin
               ($install-library-entry
                 'index
                 (case-lambda libspec (args e1 e2 ...)))
               ($install-library-entry
                 'does-not-expect-headroom-index
                 (case-lambda does-not-expect-headroom-libspec (args e1 e2 ...))))))])))
)

; we can't evaluate any dirty writes (eg. defines) until scan-remembered-set
; is ready, so install it up front.
(let ([install-library-entry ($hand-coded '$install-library-entry-procedure)])
  (install-library-entry
    (libspec-index (lookup-libspec scan-remembered-set))
    ($hand-coded 'scan-remembered-set)))

(let ([install-library-entry ($hand-coded '$install-library-entry-procedure)])
 ; no top-level defines before this point, or the linker won't have
 ; nonprocedure-code to insert in pvalue slot
  (install-library-entry
    (libspec-index (lookup-libspec nonprocedure-code))
    ($hand-coded 'nonprocedure-code)))

(define $foreign-entry ($hand-coded '$foreign-entry-procedure))
;; The name `$install-library-entry` is special to `vfasl-can-combine?`
(define $install-library-entry
  ($hand-coded '$install-library-entry-procedure))

(eval-when (compile)
(define-syntax define-hand-coded-library-entry
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       (identifier? #'name)
       #'($install-library-entry (libspec-index (lookup-libspec name))
           ($hand-coded 'name))))))
)

(define-hand-coded-library-entry get-room)
(define-hand-coded-library-entry call-error)
(define-hand-coded-library-entry dooverflood)
(define-hand-coded-library-entry dooverflow)
(define-hand-coded-library-entry dorest0)
(define-hand-coded-library-entry dorest1)
(define-hand-coded-library-entry dorest2)
(define-hand-coded-library-entry dorest3)
(define-hand-coded-library-entry dorest4)
(define-hand-coded-library-entry dorest5)
;;; doargerr must come before dounderflow*
(define-hand-coded-library-entry doargerr)

;;; dounderflow* must come before dounderflow
(define-library-entry (dounderflow* k args)
  ($do-wind ($current-winders) ($continuation-winders k))
  (cond
    ((null? args) (k))
    ((null? (cdr args)) (k (car args)))
    (else (#2%apply k args)))) ; library apply not available yet

;; before anything that returns multiple values
(define-hand-coded-library-entry values-error)

;;; dounderflow & nuate must come before callcc
(define-hand-coded-library-entry dounderflow)
(define-hand-coded-library-entry nuate)
(define-hand-coded-library-entry reify-1cc)
(define-hand-coded-library-entry maybe-reify-cc)
(define-hand-coded-library-entry callcc)
(define-hand-coded-library-entry call1cc)
(define-hand-coded-library-entry dofargint32)
(define-hand-coded-library-entry dofretint32)
(define-hand-coded-library-entry dofretuns32)
(define-hand-coded-library-entry dofargint64)
(define-hand-coded-library-entry dofretint64)
(define-hand-coded-library-entry dofretuns64)
(define-hand-coded-library-entry dofretu8*)
(define-hand-coded-library-entry dofretu16*)
(define-hand-coded-library-entry dofretu32*)
(define-hand-coded-library-entry domvleterr)
(define-hand-coded-library-entry bytevector=?)
(define-hand-coded-library-entry $wrapper-apply)
(define-hand-coded-library-entry wrapper-apply)
(define-hand-coded-library-entry arity-wrapper-apply)
(define-hand-coded-library-entry event-detour)
(define-hand-coded-library-entry popcount-slow) ; before fxpopcount use
(define-hand-coded-library-entry cpu-features)  ; before fxpopcount use

(define $instantiate-code-object ($hand-coded '$instantiate-code-object))

;;; set up $nuate for overflow
(define $nuate ($closure-code (call/1cc (lambda (k) k))))

(set! #{raw-ref-count bhowt6w0coxl0s2y-1} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{raw-create-count bhowt6w0coxl0s2y-2} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{raw-alloc-count bhowt6w0coxl0s2y-3} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{ref-count bhowt6w0coxl0s2y-4} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{pair-create-count bhowt6w0coxl0s2y-5} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{vector-create-count bhowt6w0coxl0s2y-6} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{vector-alloc-count bhowt6w0coxl0s2y-8} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{padded-vector-alloc-count bhowt6w0coxl0s2y-11} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{closure-create-count bhowt6w0coxl0s2y-7} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{closure-alloc-count bhowt6w0coxl0s2y-9} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])
(set! #{padded-closure-alloc-count bhowt6w0coxl0s2y-10} '#[#{profile-counter b5vnnom9h4o4uny0-2} 0])

(let ()
  (include "hashtable-types.ss")
  (set! $eq-ht-rtd (record-type-descriptor eq-ht))
  (set! $symbol-ht-rtd (record-type-descriptor symbol-ht)))

(define-library-entry (cfl* x y)
   ;; a+bi * c+di => ac-bd + (ad+bc)i
   ;; spurious overflows
   (cond
      [(flonum? x)
       (if (flonum? y)
           (fl* x y)
           (fl-make-rectangular
              (fl* x ($inexactnum-real-part y))
              (fl* x ($inexactnum-imag-part y))))]
      [(flonum? y)
       (fl-make-rectangular
          (fl* ($inexactnum-real-part x) y)
          (fl* ($inexactnum-imag-part x) y))]
      [else
       (let ([a ($inexactnum-real-part x)] [b ($inexactnum-imag-part x)]
             [c ($inexactnum-real-part y)] [d ($inexactnum-imag-part y)])
          (fl-make-rectangular
             (fl- (fl* a c) (fl* b d))
             (fl+ (fl* a d) (fl* b c))))]))

(define-library-entry (cfl+ x y)
   ;; a+bi + c+di => (a+c) + (b+d)i
   (cond
      [(flonum? x)
       (if (flonum? y)
           (fl+ x y)
           (fl-make-rectangular
              (fl+ x ($inexactnum-real-part y))
              ($inexactnum-imag-part y)))]
      [(flonum? y)
       (fl-make-rectangular
          (fl+ ($inexactnum-real-part x) y)
          ($inexactnum-imag-part x))]
      [else
       (fl-make-rectangular
          (fl+ ($inexactnum-real-part x) ($inexactnum-real-part y))
          (fl+ ($inexactnum-imag-part x) ($inexactnum-imag-part y)))]))

(define-library-entry (cfl- x y)
   ;; a+bi - c+di => (a-c) + (b-d)i
   (cond
      [(flonum? x)
       (if (flonum? y)
           (fl- x y)
           (fl-make-rectangular
              (fl- x ($inexactnum-real-part y))
              (fl- ($inexactnum-imag-part y))))]
      [(flonum? y)
       (fl-make-rectangular
          (fl- ($inexactnum-real-part x) y)
          ($inexactnum-imag-part x))]
      [else
       (fl-make-rectangular
          (fl- ($inexactnum-real-part x) ($inexactnum-real-part y))
          (fl- ($inexactnum-imag-part x) ($inexactnum-imag-part y)))]))

(define-library-entry (cfl/ x y)
   ;; See "Algorithm 116: Complex Division" by Robert L. Smith,
   ;; Communications of the ACM, Volume 5, Issue 8, Aug. 1962
   (cond
      [(flonum? y)
       ;; a+bi / c => a/c + (b/c)i
       (if (flonum? x)
           (fl/ x y)
           (fl-make-rectangular
              (fl/ ($inexactnum-real-part x) y)
              (fl/ ($inexactnum-imag-part x) y)))]
      [(flonum? x)
       ;; a / c+di => a/(c+d(d/c)) + (-a(d/c)/(c+d(d/c)))i if |c| >= |d|
       ;; a / c+di => a(c/d)/(d+c(c/d)) + (a/(d+c(c/d)))i if |c| < |d|
       (let ([c ($inexactnum-real-part y)] [d ($inexactnum-imag-part y)])
         (if (fl>= (flabs c) (flabs d))
             (let* ([r (fl/ d c)]
                    [den (fl+ c (fl* r d))])
               (fl-make-rectangular
                  (fl/ x den)
                  (fl/ (fl- (fl* x r)) den)))
             (let* ([r (fl/ c d)]
                    [den (fl+ d (fl* r c))])
               (fl-make-rectangular
                  (fl/ (fl* x r) den)
                  (fl/ (fl- x) den)))))]
      [else
       ;; a+bi / c+di => (a+b(d/c))/(c+d(d/c)) + ((b-a(d/c))/(c+d(d/c)))i if |c| >= |d|
       ;; a+bi / c+di => (b+a(c/d))/(d+c(c/d)) + ((a-b(c/d))/(d+c(c/d)))i if |c| < |d|
       (let ([a ($inexactnum-real-part x)] [b ($inexactnum-imag-part x)]
             [c ($inexactnum-real-part y)] [d ($inexactnum-imag-part y)])
         (if (fl>= (flabs c) (flabs d))
             (let* ([r (fl/ d c)]
                    [den (fl+ c (fl* r d))])
               (fl-make-rectangular
                  (fl/ (fl+ a (fl* b r)) den)
                  (fl/ (fl- b (fl* a r)) den)))
             (let* ([r (fl/ c d)]
                    [den (fl+ d (fl* r c))])
               (fl-make-rectangular
                  (fl/ (fl+ (fl* a r) b) den)
                  (fl/ (fl- (fl* b r) a) den)))))]))

(let ()
  (define char-oops
    (lambda (who x)
      ($oops who "~s is not a character" x)))
  (define fixnum-oops
    (lambda (who x)
      ($oops who "~s is not a fixnum" x)))
  (define string-oops
    (lambda (who x)
      ($oops who "~s is not a string" x)))
  (define mutable-string-oops
    (lambda (who x)
      ($oops who "~s is not a mutable string" x)))
  (define vector-oops
    (lambda (who x)
      ($oops who "~s is not a vector" x)))
  (define mutable-vector-oops
    (lambda (who x)
      ($oops who "~s is not a mutable vector" x)))
  (define fxvector-oops
    (lambda (who x)
      ($oops who "~s is not an fxvector" x)))
  (define flvector-oops
    (lambda (who x)
      ($oops who "~s is not an flvector" x)))
  (define bytevector-oops
    (lambda (who x)
      ($oops who "~s is not a bytevector" x)))
  (define mutable-bytevector-oops
    (lambda (who x)
      ($oops who "~s is not a mutable bytevector" x)))
  (define index-oops
    (lambda (who x i)
      ($oops who "~s is not a valid index for ~s" i x)))
  (define bytevector-index-oops
    ;; for consistency with error before library entry was introduced:
    (lambda (who x i)
      ($oops who "invalid index ~s for bytevector ~s" i x)))

  (define stencil-vector-oops
    (lambda (who x)
      ($oops who "~s is not a stencil vector" x)))

  (define-library-entry (char->integer x) (char-oops 'char->integer x))

  (define-library-entry (string-ref s i)
    (if (string? s)
        (index-oops 'string-ref s i)
        (string-oops 'string-ref s)))

  (define-library-entry (string-set! s i c)
    (if ($string-set!-check? s i)
        (if (char? c)
            (string-set! s i c)
            (char-oops 'string-set! c))
        (if (mutable-string? s)
            (index-oops 'string-set! s i)
            (mutable-string-oops 'string-set! s))))

  (define-library-entry (string-length s)
    (string-oops 'string-length s))

  (define-library-entry (vector-ref v i)
    (if (vector? v)
        (index-oops 'vector-ref v i)
        (vector-oops 'vector-ref v)))

  (define-library-entry (vector-set! v i x)
    (if (mutable-vector? v)
        (index-oops 'vector-set! v i)
        (mutable-vector-oops 'vector-set! v)))

  (define-library-entry (vector-set-fixnum! v i x)
    (if (fixnum? x)
        (if (mutable-vector? v)
            (index-oops 'vector-set-fixnum! v i)
            (mutable-vector-oops 'vector-set-fixnum! v))
        ($oops 'vector-set-fixnum! "~s is not a fixnum" x)))

  (define-library-entry (vector-length v)
    (vector-oops 'vector-length v))

  (define-library-entry (vector-cas! v i old-x new-x)
    (if (mutable-vector? v)
        (index-oops 'vector-cas! v i)
        (mutable-vector-oops 'vector-cas! v)))

  (define-library-entry (fxvector-ref v i)
    (if (fxvector? v)
        (index-oops 'fxvector-ref v i)
        (fxvector-oops 'fxvector-ref v)))

  (define-library-entry (fxvector-set! v i x)
    (if (fxvector? v)
        (if (and (fixnum? i) ($fxu< i (fxvector-length v)))
            (fixnum-oops 'fxvector-set! x)
            (index-oops 'fxvector-set! v i))
        (fxvector-oops 'fxvector-set! v)))

  (define-library-entry (fxvector-length v)
    (fxvector-oops 'fxvector-length v))

  (define-library-entry (flvector-ref v i)
    (if (flvector? v)
        (index-oops 'flvector-ref v i)
        (flvector-oops 'flvector-ref v)))

  (define-library-entry (flvector-set! v i x)
    (if (flvector? v)
        (if (and (fixnum? i) ($fxu< i (flvector-length v)))
            ($oops 'flvector-set! "~s is not a flonum" x)
            (index-oops 'flvector-set! v i))
        (flvector-oops 'flvector-set! v)))

  (define-library-entry (flvector-length v)
    (flvector-oops 'flvector-length v))

  (define-library-entry (bytevector-s8-ref v i)
    (if (bytevector? v)
        (index-oops 'bytevector-s8-ref v i)
        (bytevector-oops 'bytevector-s8-ref v)))

  (define-library-entry (bytevector-u8-ref v i)
    (if (bytevector? v)
        (index-oops 'bytevector-u8-ref v i)
        (bytevector-oops 'bytevector-u8-ref v)))

  (define-library-entry (bytevector-s8-set! v i k)
    (if ($bytevector-set!-check? 8 v i)
        (if (and (fixnum? k) (fx<= -128 k 127))
            (bytevector-s8-set! v i k)
            ($oops 'bytevector-s8-set! "invalid value ~s" k))
        (if (mutable-bytevector? v)
            (index-oops 'bytevector-s8-set! v i)
            (mutable-bytevector-oops 'bytevector-s8-set! v))))

  (define-library-entry (bytevector-u8-set! v i k)
    (if ($bytevector-set!-check? 8 v i)
        (if (and (fixnum? k) (fx<= 0 k 255))
            (bytevector-u8-set! v i k)
            ($oops 'bytevector-u8-set! "invalid value ~s" k))
        (if (mutable-bytevector? v)
            (index-oops 'bytevector-u8-set! v i)
            (mutable-bytevector-oops 'bytevector-u8-set! v))))

  (define-library-entry (bytevector-length v)
    (bytevector-oops 'bytevector-length v))

  (define-library-entry ($stencil-vector-mask v)
    (stencil-vector-oops '$stencil-vector-mask v))

  (define-library-entry (stencil-vector-mask v)
    (stencil-vector-oops 'stencil-vector-mask v))

  (define-library-entry (bytevector-ieee-double-native-ref v i)
    (if (bytevector? v)
        (bytevector-index-oops 'bytevector-ieee-double-native-ref v i)
        (bytevector-oops 'bytevector-ieee-double-native-ref v)))

  (define-library-entry (bytevector-ieee-double-native-set! v i)
    (if (mutable-bytevector? v)
        (bytevector-index-oops 'bytevector-ieee-double-native-set! v i)
        (mutable-bytevector-oops 'bytevector-ieee-double-native-set! v)))

  (define-library-entry (char=? x y) (char-oops 'char=? (if (char? x) y x)))
  (define-library-entry (char<? x y) (char-oops 'char<? (if (char? x) y x)))
  (define-library-entry (char>? x y) (char-oops 'char>? (if (char? x) y x)))
  (define-library-entry (char<=? x y) (char-oops 'char<=? (if (char? x) y x)))
  (define-library-entry (char>=? x y) (char-oops 'char>=? (if (char? x) y x)))
)

(define-library-entry (real->flonum x who)
  (cond
    [(fixnum? x) (fixnum->flonum x)]
    [(or (bignum? x) (ratnum? x)) (inexact x)]
    [(flonum? x) x]
    [else ($oops who "~s is not a real number" x)]))

(let ()
  (define pair-oops
    (lambda (who x)
      ($oops who "~s is not a pair" x)))

  (define-library-entry (car x) (pair-oops 'car x))
  (define-library-entry (cdr x) (pair-oops 'cdr x))
  (define-library-entry (set-car! x y) (pair-oops 'set-car! x))
  (define-library-entry (set-cdr! x y) (pair-oops 'set-cdr! x))
)

(let ()
  (define c..r-oops
    (lambda (who obj)
      ($oops who "incorrect list structure ~s" obj)))

  (define-library-entry (caar x) (c..r-oops 'caar x))
  (define-library-entry (cadr x) (c..r-oops 'cadr x))
  (define-library-entry (cdar x) (c..r-oops 'cdar x))
  (define-library-entry (cddr x) (c..r-oops 'cddr x))
  (define-library-entry (caaar x) (c..r-oops 'caaar x))
  (define-library-entry (caadr x) (c..r-oops 'caadr x))
  (define-library-entry (cadar x) (c..r-oops 'cadar x))
  (define-library-entry (caddr x) (c..r-oops 'caddr x))
  (define-library-entry (cdaar x) (c..r-oops 'cdaar x))
  (define-library-entry (cdadr x) (c..r-oops 'cdadr x))
  (define-library-entry (cddar x) (c..r-oops 'cddar x))
  (define-library-entry (cdddr x) (c..r-oops 'cdddr x))
  (define-library-entry (caaaar x) (c..r-oops 'caaaar x))
  (define-library-entry (caaadr x) (c..r-oops 'caaadr x))
  (define-library-entry (caadar x) (c..r-oops 'caadar x))
  (define-library-entry (caaddr x) (c..r-oops 'caaddr x))
  (define-library-entry (cadaar x) (c..r-oops 'cadaar x))
  (define-library-entry (cadadr x) (c..r-oops 'cadadr x))
  (define-library-entry (caddar x) (c..r-oops 'caddar x))
  (define-library-entry (cadddr x) (c..r-oops 'cadddr x))
  (define-library-entry (cdaaar x) (c..r-oops 'cdaaar x))
  (define-library-entry (cdaadr x) (c..r-oops 'cdaadr x))
  (define-library-entry (cdadar x) (c..r-oops 'cdadar x))
  (define-library-entry (cdaddr x) (c..r-oops 'cdaddr x))
  (define-library-entry (cddaar x) (c..r-oops 'cddaar x))
  (define-library-entry (cddadr x) (c..r-oops 'cddadr x))
  (define-library-entry (cdddar x) (c..r-oops 'cdddar x))
  (define-library-entry (cddddr x) (c..r-oops 'cddddr x))
)

(define-library-entry (unbox x)
  ($oops 'unbox "~s is not a box" x))

(define-library-entry (set-box! b v)
  ($oops 'set-box! "~s is not a mutable box" b))

(define-library-entry (box-cas! b old-v new-v)
  ($oops 'box-cas! "~s is not a mutable box" b))

(let ()
(define (fxnonfixnum1 who x)
  ($oops who "~s is not a fixnum" x))

(define (fxnonfixnum2 who x y)
  ($oops who "~s is not a fixnum" (if (fixnum? x) y x)))

(define (fxoops1 who x)
   (if (fixnum? x)
       ($impoops who "fixnum overflow with argument ~s" x)
       (fxnonfixnum1 who x)))

(define (fxoops2 who x y)
   (if (fixnum? x)
       (if (fixnum? y)
           ($impoops who "fixnum overflow with arguments ~s and ~s" x y)
           (fxnonfixnum1 who y))
       (fxnonfixnum1 who x)))

(define (shift-count-oops who x)
  ($oops who "invalid shift count ~s" x))

(define-library-entry (fx+ x y) (fxoops2 'fx+ x y))
(define-library-entry (fx- x y) (fxoops2 'fx- x y))
(define-library-entry (fx* x y) (fxoops2 'fx* x y))
(define-library-entry (fx1+ x) (fxoops1 'fx1+ x))
(define-library-entry (fx1- x) (fxoops1 'fx1- x))

(define-library-entry (fx+/wraparound x y) (fxoops2 'fx+/wraparound x y))
(define-library-entry (fx-/wraparound x y) (fxoops2 'fx-/wraparound x y))
(define-library-entry (fx*/wraparound x y) (fxoops2 'fx*/wraparound x y))
(define-library-entry (fxsll/wraparound x y)
  (if (and (fixnum? x) (fixnum? y))
      (shift-count-oops 'fxsll/wraparound y)
      (fxoops2 'fxsll/wraparound x y)))

(define-library-entry (fx= x y) (fxnonfixnum2 'fx= x y))
(define-library-entry (fx< x y) (fxnonfixnum2 'fx< x y))
(define-library-entry (fx> x y) (fxnonfixnum2 'fx> x y))
(define-library-entry (fx<= x y) (fxnonfixnum2 'fx<= x y))
(define-library-entry (fx>= x y) (fxnonfixnum2 'fx>= x y))
(define-library-entry (fx=? x y) (fxnonfixnum2 'fx=? x y))
(define-library-entry (fx<? x y) (fxnonfixnum2 'fx<? x y))
(define-library-entry (fx>? x y) (fxnonfixnum2 'fx>? x y))
(define-library-entry (fx<=? x y) (fxnonfixnum2 'fx<=? x y))
(define-library-entry (fx>=? x y) (fxnonfixnum2 'fx>=? x y))
(define-library-entry (fxzero? x) (fxnonfixnum1 'fxzero? x))
(define-library-entry (fxpositive? x) (fxnonfixnum1 'fxpositive? x))
(define-library-entry (fxnonpositive? x) (fxnonfixnum1 'fxnonpositive? x))
(define-library-entry (fxnegative? x) (fxnonfixnum1 'fxnegative? x))
(define-library-entry (fxnonnegative? x) (fxnonfixnum1 'fxnonnegative? x))
(define-library-entry (fxeven? x) (fxnonfixnum1 'fxeven? x))
(define-library-entry (fxodd? x) (fxnonfixnum1 'fxodd? x))
(define-library-entry (fxlogior x y) (fxnonfixnum2 'fxlogior x y))
(define-library-entry (fxlogor x y) (fxnonfixnum2 'fxlogor x y))
(define-library-entry (fxlogxor x y) (fxnonfixnum2 'fxlogxor x y))
(define-library-entry (fxlogand x y) (fxnonfixnum2 'fxlogand x y))
(define-library-entry (fxlognot x) (fxnonfixnum1 'fxlognot x))
(define-library-entry (fxior x y) (fxnonfixnum2 'fxior x y))
(define-library-entry (fxxor x y) (fxnonfixnum2 'fxxor x y))
(define-library-entry (fxand x y) (fxnonfixnum2 'fxand x y))
(define-library-entry (fxnot x) (fxnonfixnum1 'fxnot x))
(define-library-entry (fixnum->flonum x) (fxnonfixnum1 'fixnum->flonum x))
(define-library-entry (fxpopcount x) ($oops 'fxpopcount "~s is not a non-negative fixnum" x))
(define-library-entry (fxpopcount32 x) ($oops 'fxpopcount32 "~s is not a 32-bit fixnum" x))
(define-library-entry (fxpopcount16 x) ($oops 'fxpopcount16 "~s is not a 16-bit fixnum" x))

(define-library-entry (fxsll x y)
  (cond
    [(not (fixnum? x)) (fxnonfixnum1 'fxsll x)]
    [(not (fixnum? y)) (fxnonfixnum1 'fxsll y)]
    [(fx= 0 y) x]
    [($fxu< y (constant fixnum-bits))
     (if (fx>= x 0)
         (if (fx< x (fxsll 1 (fx- (- (constant fixnum-bits) 1) y)))
             (fxsll x y)
             (fxoops2 'fxsll x y))
         (if (fx>= x (fxsll -1 (fx- (- (constant fixnum-bits) 1) y)))
             (fxsll x y)
             (fxoops2 'fxsll x y)))]
    [(fx= y (constant fixnum-bits)) (if (fx= x 0) x (fxoops2 'fxsll x y))]
    [else (shift-count-oops 'fxsll y)]))

(define-library-entry (fxarithmetic-shift-left x y)
  (cond
    [(not (fixnum? x)) (fxnonfixnum1 'fxarithmetic-shift-left x)]
    [(not (fixnum? y)) (fxnonfixnum1 'fxarithmetic-shift-left y)]
    [(fx= 0 y) x]
    [($fxu< y (constant fixnum-bits))
     (if (fx>= x 0)
         (if (fx< x (fxsll 1 (fx- (- (constant fixnum-bits) 1) y)))
             (fxsll x y)
             (fxoops2 'fxarithmetic-shift-left x y))
         (if (fx>= x (fxsll -1 (fx- (- (constant fixnum-bits) 1) y)))
             (fxsll x y)
             (fxoops2 'fxarithmetic-shift-left x y)))]
    [else (shift-count-oops 'fxarithmetic-shift-left y)]))

(define-library-entry (fxsrl x y)
  (cond
    [(not (fixnum? x)) (fxnonfixnum1 'fxsrl x)]
    [(not (fixnum? y)) (fxnonfixnum1 'fxsrl y)]
    [else (shift-count-oops 'fxsrl y)]))

(define-library-entry (fxsra x y)
  (cond
    [(not (fixnum? x)) (fxnonfixnum1 'fxsra x)]
    [(not (fixnum? y)) (fxnonfixnum1 'fxsra y)]
    [else (shift-count-oops 'fxsra y)]))

(define-library-entry (fxarithmetic-shift-right x y)
  (cond
    [(not (fixnum? x)) (fxnonfixnum1 'fxarithmetic-shift-right x)]
    [(not (fixnum? y)) (fxnonfixnum1 'fxarithmetic-shift-right y)]
    [else (shift-count-oops 'fxarithmetic-shift-right y)]))

(define-library-entry (fxarithmetic-shift x y)
  (cond
    [(not (fixnum? x)) (fxnonfixnum1 'fxarithmetic-shift x)]
    [(not (fixnum? y)) (fxnonfixnum1 'fxarithmetic-shift y)]
    [(fx= 0 y) x]
    [($fxu< y (constant fixnum-bits))
     (if (fx>= x 0)
         (if (fx< x (fxsll 1 (fx- (- (constant fixnum-bits) 1) y)))
             (fxsll x y)
             (fxoops2 'fxarithmetic-shift x y))
         (if (fx>= x (fxsll -1 (fx- (- (constant fixnum-bits) 1) y)))
             (fxsll x y)
             (fxoops2 'fxarithmetic-shift x y)))]
    [(fx< (fx- (constant fixnum-bits)) y 0) (fxsra x (fx- y))]
    [else (shift-count-oops 'fxarithmetic-shift y)]))

(define-library-entry (fxlogbit? k n)
  (if (fixnum? n)
      (if (fixnum? k)
          (if (fx< k 0)
              ($oops 'fxlogbit? "invalid bit index ~s" k)
             ; this case left to us by cp1in fxlogbit? handler
              (fx< n 0))
          (fxnonfixnum1 'fxlogbit? k))
      (fxnonfixnum1 'fxlogbit? n)))

(define-library-entry (fxbit-set? n k)
  (if (fixnum? n)
      (if (fixnum? k)
          (if (fx< k 0)
              ($oops 'fxbit-set? "invalid bit index ~s" k)
             ; this case left to us by cp1in fxbit-set? handler
              (fx< n 0))
          (fxnonfixnum1 'fxbit-set? k))
      (fxnonfixnum1 'fxbit-set? n)))

(define-library-entry (fxlogbit0 k n)
  (if (fixnum? n)
      (if (fixnum? k)
          ($oops 'fxlogbit0 "invalid bit index ~s" k)
          (fxnonfixnum1 'fxlogbit0 k))
      (fxnonfixnum1 'fxlogbit0 n)))

(define-library-entry (fxlogbit1 k n)
  (if (fixnum? n)
      (if (fixnum? k)
          ($oops 'fxlogbit1 "invalid bit index ~s" k)
          (fxnonfixnum1 'fxlogbit1 k))
      (fxnonfixnum1 'fxlogbit1 n)))

(define-library-entry (fxcopy-bit n k)
 ; get here only if third argument is 0 or 1
  (if (fixnum? n)
      (if (fixnum? k)
          ($oops 'fxcopy-bit "invalid bit index ~s" k)
          (fxnonfixnum1 'fxcopy-bit k))
      (fxnonfixnum1 'fxcopy-bit n)))

(define-library-entry (fxlogtest x y) (fxnonfixnum2 'fxlogtest x y))
)

(let ()
  (define flonum-oops
    (lambda (who x)
      ($oops who "~s is not a flonum" x)))

  (define-library-entry (fl= x y) (flonum-oops 'fl= (if (flonum? x) y x)))
  (define-library-entry (fl< x y) (flonum-oops 'fl< (if (flonum? x) y x)))
  (define-library-entry (fl> x y) (flonum-oops 'fl> (if (flonum? x) y x)))
  (define-library-entry (fl<= x y) (flonum-oops 'fl<= (if (flonum? x) y x)))
  (define-library-entry (fl>= x y) (flonum-oops 'fl>= (if (flonum? x) y x)))
  (define-library-entry (fl=? x y) (flonum-oops 'fl=? (if (flonum? x) y x)))
  (define-library-entry (fl<? x y) (flonum-oops 'fl<? (if (flonum? x) y x)))
  (define-library-entry (fl>? x y) (flonum-oops 'fl>? (if (flonum? x) y x)))
  (define-library-entry (fl<=? x y) (flonum-oops 'fl<=? (if (flonum? x) y x)))
  (define-library-entry (fl>=? x y) (flonum-oops 'fl>=? (if (flonum? x) y x)))

  (define-library-entry (fl+ x y) (flonum-oops 'fl+ (if (flonum? x) y x)))
  (define-library-entry (fl- x y) (flonum-oops 'fl- (if (flonum? x) y x)))
  (define-library-entry (fl* x y) (flonum-oops 'fl* (if (flonum? x) y x)))
  (define-library-entry (fl/ x y) (flonum-oops 'fl/ (if (flonum? x) y x)))
  (define-library-entry (flnegate x) (flonum-oops 'fl- x))
  (define-library-entry (flabs x) (flonum-oops 'flabs x))
  (define-library-entry (flmin x y) (flonum-oops 'flmin (if (flonum? x) y x)))
  (define-library-entry (flmax x y) (flonum-oops 'flmax (if (flonum? x) y x)))

  (define-library-entry (flsqrt x) (flonum-oops 'flsqrt x))
  (define-library-entry (flround x) (flonum-oops 'flround x))
  (define-library-entry (flfloor x) (flonum-oops 'flfloor x))
  (define-library-entry (flceiling x) (flonum-oops 'flceiling x))
  (define-library-entry (fltruncate x) (flonum-oops 'fltruncate x))
  (define-library-entry (flsingle x) (flonum-oops 'flsingle x))
  (define-library-entry (flsin x) (flonum-oops 'flsin x))
  (define-library-entry (flcos x) (flonum-oops 'flcos x))
  (define-library-entry (fltan x) (flonum-oops 'fltan x))
  (define-library-entry (flasin x) (flonum-oops 'flasin x))
  (define-library-entry (flacos x) (flonum-oops 'flacos x))
  (define-library-entry (flatan x) (flonum-oops 'flatan x))
  (define-library-entry (flatan2 x y) (flonum-oops 'flatan (if (flonum? x) y x)))
  (define-library-entry (flexp x) (flonum-oops 'flexp x))
  (define-library-entry (fllog x) (flonum-oops 'fllog x))
  (define-library-entry (fllog2 x y) (flonum-oops 'fllog (if (flonum? x) y x)))
  (define-library-entry (flexpt x y) (flonum-oops 'flexpt (if (flonum? x) y x)))
  (define-library-entry (flbit-field x y z) (flonum-oops 'flbit-field x))

  (define-library-entry (flonum->fixnum x) (if (flonum? x)
                                               ($oops 'flonum->fixnum "result for ~s would be outside of fixnum range" x)
                                               (flonum-oops 'flonum->fixnum x)))
)

;; Now using `rint` via a C entry
#;
(define-library-entry (flround x)
 ; assumes round-to-nearest-or-even
  (float-type-case
    [(ieee)
     (define threshold+ #i#x10000000000000)
     (define threshold- #i#x-10000000000000)])
  (if (fl= x 0.0)
      x ; don't change sign
      (if (fl>= x 0.0)
          (if (fl< x threshold+)
              (fl- (fl+ x threshold+) threshold+)
              x)
          (if (fl>= x -0.5)
              -0.0 ; keep negative
              (if (fl> x threshold-)
                  (fl- (fl+ x threshold-) threshold-)
                  x)))))

;;; The generic comparison entries assume the fixnum case is inlined.

(define-library-entry (= x y)
   (cond
      [(flonum? x)
       (cond 
          [(flonum? y) (fl= x y)]
          [($inexactnum? y) (and (fl= ($inexactnum-imag-part y) 0.0)
                                  (fl= ($inexactnum-real-part y) x))]
          [else ($= '= x y)])]
      [($inexactnum? x)
       (cond 
          [(flonum? y) (and (fl= ($inexactnum-imag-part x) 0.0)
                            (fl= ($inexactnum-real-part x) y))]
          [($inexactnum? y)
           (and (fl= ($inexactnum-imag-part x) ($inexactnum-imag-part y))
                (fl= ($inexactnum-real-part x) ($inexactnum-real-part y)))]
          [else ($= '= x y)])]
      [else ($= '= x y)]))

(define-library-entry (zero? x)
   (cond
      [(cflonum? x) (cfl= x 0.0)]
      [(or (bignum? x) (ratnum? x) ($exactnum? x)) #f]
      [else ($= 'zero? x 0)]))

(define-library-entry (< x y)
   (cond
      [(and (flonum? x) (flonum? y)) (fl< x y)]
      [else ($< '< x y)]))

(define-library-entry (> x y)
   (cond
      [(and (flonum? x) (flonum? y)) (fl> x y)]
      [else ($< '> y x)]))

(define-library-entry (<= x y)
   (cond
      [(and (flonum? x) (flonum? y)) (fl<= x y)]
      [else ($<= '<= x y)]))

(define-library-entry (>= x y)
   (cond
      [(and (flonum? x) (flonum? y)) (fl>= x y)]
      [else ($<= '>= y x)]))

(define-library-entry (+ x y)
   (cond
      [(flonum? x)
       (cond
          [(flonum? y) (fl+ x y)]
          [($inexactnum? y) (cfl+ x y)]
          [else ($+ '+ x y)])]
      [(and ($inexactnum? x) (cflonum? y)) (cfl+ x y)]
      [else ($+ '+ x y)]))

(define-library-entry (1+ x)
   (cond
      [(flonum? x) (fl+ x 1.0)]
      [($inexactnum? x) (cfl+ x 1.0)]
      [else ($+ '1+ x 1)]))

(define-library-entry (add1 x)
   (cond
      [(flonum? x) (fl+ x 1.0)]
      [($inexactnum? x) (cfl+ x 1.0)]
      [else ($+ 'add1 x 1)]))

(define-library-entry (negate x)
   (cond
      [(flonum? x) (fl- x)]
      [($inexactnum? x) (cfl- x)]
      [else ($- '- 0 x)]))

(define-library-entry (- x y)
   (cond
      [(flonum? x)
       (cond
          [(flonum? y) (fl- x y)]
          [($inexactnum? y) (cfl- x y)]
          [else ($- '- x y)])]
      [(and ($inexactnum? x) (cflonum? y)) (cfl- x y)]
      [else ($- '- x y)]))

(define-library-entry (1- x)
   (cond
      [(flonum? x) (fl- x 1.0)]
      [($inexactnum? x) (cfl- x 1.0)]
      [else ($- '1- x 1)]))

(define-library-entry (-1+ x)
   (cond
      [(flonum? x) (fl- x 1.0)]
      [($inexactnum? x) (cfl- x 1.0)]
      [else ($- '-1+ x 1)]))

(define-library-entry (sub1 x)
   (cond
      [(flonum? x) (fl- x 1.0)]
      [($inexactnum? x) (cfl- x 1.0)]
      [else ($- 'sub1 x 1)]))

(define-library-entry (* x y)
   (cond
      [(flonum? x)
       (cond
          [(flonum? y) (fl* x y)]
          [($inexactnum? y) (cfl* x y)]
          [else ($* '* x y)])]
      [(and ($inexactnum? x) (cflonum? y)) (cfl* x y)]
      [else ($* '* x y)]))

(define-library-entry (/ x y)
   (cond
      [(flonum? x)
       (cond
          [(flonum? y) (fl/ x y)]
          [($inexactnum? y) (cfl/ x y)]
          [else ($/ '/ x y)])]
      [(and ($inexactnum? x) (cflonum? y)) (cfl/ x y)]
      [else ($/ '/  x y)]))

;;; The logical operators assume the fixnum case is inlined.
(let ()
  (define exactintoops1
    (lambda (who x)
      ($oops who "~s is not an exact integer" x)))
  (define exactintoops2
    (lambda (who x y)
      (exactintoops1 who (if (or (fixnum? x) (bignum? x)) y x))))
  (define invalidindexoops
    (lambda (who k)
      ($oops who "invalid bit index ~s" k)))

  (define-library-entry (logand x y)
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logand x y)
        (exactintoops2 'logand x y)))

  (define-library-entry (bitwise-and x y)
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logand x y)
        (exactintoops2 'bitwise-and x y)))

  (define-library-entry (logior x y) ; same as logor
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logor x y)
        (exactintoops2 'logior x y)))

  (define-library-entry (logor x y)
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logor x y)
        (exactintoops2 'logor x y)))

  (define-library-entry (bitwise-ior x y)
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logor x y)
        (exactintoops2 'bitwise-ior x y)))

  (define-library-entry (logxor x y)
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logxor x y)
        (exactintoops2 'logxor x y)))

  (define-library-entry (bitwise-xor x y)
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logxor x y)
        (exactintoops2 'bitwise-xor x y)))

  (define-library-entry (lognot x)
    (if (bignum? x)
        ($lognot x)
        (exactintoops1 'lognot x)))

  (define-library-entry (bitwise-not x)
    (if (bignum? x)
        ($lognot x)
        (exactintoops1 'bitwise-not x)))

  (let ()
    (define (do-logbit? who k n)
      (cond
        [(fixnum? n)
         (cond
           [(fixnum? k)
            (if (fx< k 0)
                (invalidindexoops who k)
               ; this case left to us by cp1in logbit? handler
                (fx< n 0))]
           [(bignum? k)
            (if (< k 0)
                (invalidindexoops who k)
               ; this case left to us by cp1in logbit? handler
                (fx< n 0))]
           [else (invalidindexoops who k)])]
        [(bignum? n)
         (cond
           [(fixnum? k)
            (if (fx< k 0)
                (invalidindexoops who k)
                ($logbit? k n))]
           [(bignum? k)
            (if (< k 0)
                (invalidindexoops who k)
               ; $logbit? requires k to be a fixnum
                (fxlogtest (ash n (- k)) 1))]
           [else (invalidindexoops who k)])]
        [else (exactintoops1 who n)]))
    (define-library-entry (logbit? k n) (do-logbit? 'logbit? k n))
    (define-library-entry (bitwise-bit-set? n k) (do-logbit? 'bitwise-bit-set? k n)))

  (define-library-entry (logbit0 k n)
    (if (or (fixnum? n) (bignum? n))
        (cond
          [(fixnum? k)
           (if (fx< k 0)
               (invalidindexoops 'logbit0 k)
               ($logbit0 k n))]
          [(bignum? k)
           (if (< k 0)
               (invalidindexoops 'logbit0 k)
              ; $logbit0 requires k to be a fixnum
               ($logand n ($lognot (ash 1 k))))]
          [else (invalidindexoops 'logbit0 k)])
        (exactintoops1 'logbit0 n)))

  (define-library-entry (logbit1 k n)
    (if (or (fixnum? n) (bignum? n))
        (cond
          [(fixnum? k)
           (if (fx< k 0)
               (invalidindexoops 'logbit1 k)
               ($logbit1 k n))]
          [(bignum? k)
           (if (< k 0)
               (invalidindexoops 'logbit1 k)
              ; $logbit1 requires k to be a fixnum
               ($logor n (ash 1 k)))]
          [else (invalidindexoops 'logbit1 k)])
        (exactintoops1 'logbit1 n)))

  (define-library-entry (logtest x y)
    (if (if (fixnum? x)
            (bignum? y)
            (and (bignum? x)
                 (or (fixnum? y) (bignum? y))))
        ($logtest x y)
        (exactintoops2 'logtest x y)))
)

(let ()
  (include "io-types.ss")
  (define-syntax define-safe/unsafe
    (lambda (x)
      (syntax-case x ()
        [(k (name arg ...) e ...)
         (with-syntax ([safe-name (construct-name #'k "safe-" #'name)]
                       [unsafe-name (construct-name #'k "unsafe-" #'name)]
                       [who (datum->syntax #'k 'who)]
                       [check (datum->syntax #'k 'check)])
           #'(let ()
               (define who 'name)
               (let ()
                 (define-syntax check (identifier-syntax if))
                 (define-library-entry (safe-name arg ...) e ...))
               (let ()
                 (define-syntax check (syntax-rules () [(_ e1 e2 e3) e2]))
                 (define-library-entry (unsafe-name arg ...) e ...))))])))
  (define-safe/unsafe (get-u8 p)
    (check (and (input-port? p) (binary-port? p))
      ((port-handler-get ($port-handler p)) 'get-u8 p)
      ($oops who "~s is not a binary input port" p)))
  (define-safe/unsafe (get-char p)
    (check (and (input-port? p) (textual-port? p))
      ((port-handler-get ($port-handler p)) who p)
      ($oops who "~s is not a textual input port" p)))
  (define-safe/unsafe (read-char p)
    (check (and (input-port? p) (textual-port? p))
      ((port-handler-get ($port-handler p)) who p)
      ($oops who "~s is not a textual input port" p)))
  (define-safe/unsafe (lookahead-u8 p)
    (check (and (input-port? p) (binary-port? p))
      ((port-handler-lookahead ($port-handler p)) 'lookahead-u8 p)
      ($oops who "~s is not a binary input port" p)))
  (define-safe/unsafe (lookahead-char p)
    (check (and (input-port? p) (textual-port? p))
      ((port-handler-lookahead ($port-handler p)) who p)
      ($oops who "~s is not a textual input port" p)))
  (define-safe/unsafe (peek-char p)
    (check (and (input-port? p) (textual-port? p))
      ((port-handler-lookahead ($port-handler p)) who p)
      ($oops who "~s is not a textual input port" p)))
  (define-safe/unsafe (unget-u8 p x)
    (check (and (input-port? p) (binary-port? p))
      (check (or (and (fixnum? x) (fx<= 0 x 255)) (eof-object? x))
        ((port-handler-unget ($port-handler p)) who p x)
        ($oops who "~s is not an octet or the eof object" x))
      ($oops who "~s is not a binary input port" p)))
  (define-safe/unsafe (unget-char p x)
    (check (and (input-port? p) (textual-port? p))
      (check (or (char? x) (eof-object? x))
        ((port-handler-unget ($port-handler p)) who p x)
        ($oops who "~s is not an character or the eof object" x))
      ($oops who "~s is not a textual input port" p)))
  (define-safe/unsafe (unread-char x p)
    (check (and (input-port? p) (textual-port? p))
      (check (or (char? x) (eof-object? x))
        ((port-handler-unget ($port-handler p)) who p x)
        ($oops who "~s is not an character or the eof object" x))
      ($oops who "~s is not a textual input port" p)))
  (define-safe/unsafe (put-u8 p x)
    (check (and (output-port? p) (binary-port? p))
      (check (and (fixnum? x) (fx<= 0 x 255))
        ((port-handler-put ($port-handler p)) who p x)
        ($oops who "~s is not an octet" x))
      ($oops who "~s is not a binary output port" p)))
  (define-safe/unsafe (put-char p x)
    (check (and (output-port? p) (textual-port? p))
      (check (char? x)
        ((port-handler-put ($port-handler p)) who p x)
        ($oops who "~s is not a character" x))
      ($oops who "~s is not a textual output port" p)))
  (define-safe/unsafe (write-char x p)
    (check (and (output-port? p) (textual-port? p))
      (check (char? x)
        ((port-handler-put ($port-handler p)) who p x)
        ($oops who "~s is not a character" x))
      ($oops who "~s is not a textual output port" p)))
  (define-safe/unsafe (newline p)
    (check (and (output-port? p) (textual-port? p))
      ((port-handler-put ($port-handler p)) who p #\newline)
      ($oops who "~s is not a textual output port" p)))
  (define-safe/unsafe (port-eof? p)
    (check (input-port? p)
      (eof-object? ((port-handler-lookahead ($port-handler p)) who p))
      ($oops who "~s is not an input port" p)))
  (define-library-entry (put-bytevector bop bv start count)
    (define who 'put-bytevector)
    (if (or (fx> count max-put-copy) (fx> count (binary-port-output-count bop)))
        (let ([put-some (port-handler-put-some ($port-handler bop))])
          (let loop ([start start] [count count])
            (unless (eq? 0 count)
              (let ([n (put-some who bop bv start count)])
                (loop (fx+ start n) (fx- count n))))))
        (let ([i (binary-port-output-index bop)])
         ; counting on cp1in generating call to $byte-copy here and
         ; $byte-copy foreign procedure to be compiled w/o interrupt
         ; trap check in prims.ss.  otherwise this won't be safe for
         ; multitasking.
          (bytevector-copy! bv start (binary-port-output-buffer bop) i count)
          (set-binary-port-output-index! bop (fx+ i count)))))
  (define-library-entry (put-bytevector-some bop bv start count)
    (define who 'put-bytevector-some)
    (if (or (fx> count max-put-copy) (fx> count (binary-port-output-count bop)))
        (let ([put-some (port-handler-put-some ($port-handler bop))])
          (put-some who bop bv start count))
        (let ([i (binary-port-output-index bop)])
         ; counting on cp1in generating call to $byte-copy here and
         ; $byte-copy foreign procedure to be compiled w/o interrupt
         ; trap check in prims.ss.  otherwise this won't be safe for
         ; multitasking.
          (bytevector-copy! bv start (binary-port-output-buffer bop) i count)
          (set-binary-port-output-index! bop (fx+ i count))
          count)))
  (define-library-entry (put-string top st start count)
    (define who 'put-string)
    (if (or (fx> count max-put-copy) (fx> count (textual-port-output-count top)))
        (let ([put-some (port-handler-put-some ($port-handler top))])
          (let loop ([start start] [count count])
            (unless (eq? 0 count)
              (let ([n (put-some who top st start count)])
                (loop (fx+ start n) (fx- count n))))))
        (let ([i (textual-port-output-index top)])
         ; counting on cp1in generating call to $byte-copy here and
         ; $byte-copy foreign procedure to be compiled w/o interrupt
         ; trap check in prims.ss.  otherwise this won't be safe for
         ; multitasking.
          (string-copy! st start (textual-port-output-buffer top) i count)
          (set-textual-port-output-index! top (fx+ i count)))))
  (define-library-entry (put-string-some top st start count)
    (define who 'put-string-some)
    (if (or (fx> count max-put-copy) (fx> count (textual-port-output-count top)))
        (let ([put-some (port-handler-put-some ($port-handler top))])
          (put-some who top st start count))
        (let ([i (textual-port-output-index top)])
         ; counting on cp1in generating call to $byte-copy here and
         ; $byte-copy foreign procedure to be compiled w/o interrupt
         ; trap check in prims.ss.  otherwise this won't be safe for
         ; multitasking.
          (string-copy! st start (textual-port-output-buffer top) i count)
          (set-textual-port-output-index! top (fx+ i count))
          count)))
  (define-library-entry (display-string st top)
    (define who 'display-string)
    (let ([start 0] [count (string-length st)])
      (if (or (fx> count max-put-copy) (fx> count (textual-port-output-count top)))
          (let ([put-some (port-handler-put-some ($port-handler top))])
            (let loop ([start start] [count count])
              (unless (eq? 0 count)
                (let ([n (put-some who top st start count)])
                  (loop (fx+ start n) (fx- count n))))))
          (let ([i (textual-port-output-index top)])
           ; counting on cp1in generating call to $byte-copy here and
           ; $byte-copy foreign procedure to be compiled w/o interrupt
           ; trap check in prims.ss.  otherwise this won't be safe for
           ; multitasking.
            (string-copy! st start (textual-port-output-buffer top) i count)
            (set-textual-port-output-index! top (fx+ i count))))))
)

(define-library-entry ($top-level-value x)
  (unless (symbol? x)
    ($oops '$top-level-value "~s is not a symbol" x))
  (unless ($top-level-bound? x)
    ($oops #f "variable ~:s is not bound" x))
  (#3%$top-level-value x))

(define-library-entry (event)
  (define (timer)
    (if (eq? ($tc-field 'timer-ticks ($tc)) 0)
        (let ([handler (timer-interrupt-handler)])
          ($tc-field 'timer-ticks ($tc) #f)
          (signal)
          (handler))
        (signal)))
  (define (signal)
    (let ([x ($tc-field 'signal-interrupt-pending ($tc))])
      (if x
          (let ([handler $signal-interrupt-handler])
            ($tc-field 'signal-interrupt-pending ($tc) #f)
            (keyboard)
            (for-each handler ($dequeue-scheme-signals ($tc))))
          (keyboard))))
  (define (keyboard)
    (if ($tc-field 'keyboard-interrupt-pending ($tc))
        (let ([handler (keyboard-interrupt-handler)])
          ($tc-field 'keyboard-interrupt-pending ($tc) #f)
          (collector)
          (handler))
        (collector)))
  (define (collector)
    (if $collect-request-pending
        (let ([handler $collect-rendezvous])
          (restart-timer)
          (handler))
        (restart-timer)))
  (define (restart-timer)
    (cond
      [($tc-field 'timer-ticks ($tc)) =>
       (lambda (t)
         (let ([ticks (fxmin t (constant default-timer-ticks))])
           ($tc-field 'timer-ticks ($tc) (fx- t ticks))
           ($tc-field 'something-pending ($tc) #t)
           ($set-timer ticks)))]
      [else
       ($set-timer (constant default-timer-ticks))]))
  (if (and (fx= ($tc-field 'disable-count ($tc)) 0) ($tc-field 'something-pending ($tc)))
      (begin
        ($set-timer (most-positive-fixnum))
        ($tc-field 'something-pending ($tc) #f)
        (timer))
      ($set-timer (constant default-timer-ticks))))

(define-library-entry (virtual-register idx)
  ($oops 'virtual-register "invalid index ~s" idx))

(define-library-entry (set-virtual-register! idx)
  ($oops 'set-virtual-register! "invalid index ~s" idx))

(define-library-entry (map1 f ls)
  (let map ([f f] [ls ls])
    (if (null? ls)
        '()
        (let ((r (cdr ls)))
          (if (null? r)
              (list (f (car ls)))
              ; cdr first to avoid getting sick if f mutates input
              (let ([tail (map f (cdr r))])
                (list* (f (car ls)) (f (car r)) tail)))))))

(define-library-entry (map2 f ls1 ls2)
  (let map ([f f] [ls1 ls1] [ls2 ls2])
    (if (null? ls1)
        '()
        (let ((r1 (cdr ls1)))
          (if (null? r1)
              (list (f (car ls1) (car ls2)))
              (let ((r2 (cdr ls2)))
                ; cdr first to avoid getting sick if f mutates input
                (let ([tail (map f (cdr r1) (cdr r2))])
                  (list* (f (car ls1) (car ls2))
                    (f (car r1) (car r2))
                    tail))))))))

(define-library-entry (map-car ls)
  (let map ([ls ls])
    (if (null? ls)
        '()
        (let ((r (cdr ls)))
          (if (null? r)
              (list (car (car ls)))
              (list* (car (car ls)) (car (car r)) (map (cdr r))))))))

(define-library-entry (map-cdr ls)
  (let map ([ls ls])
    (if (null? ls)
        '()
        (let ((r (cdr ls)))
          (if (null? r)
              (list (cdr (car ls)))
              (list* (cdr (car ls)) (cdr (car r)) (map (cdr r))))))))

(define-library-entry (map-cons ls1 ls2)
  (let map ([ls1 ls1] [ls2 ls2])
    (if (null? ls1)
        '()
        (let ((r1 (cdr ls1)))
          (if (null? r1)
              (list (cons (car ls1) (car ls2)))
              (let ((r2 (cdr ls2)))
                (list* (cons (car ls1) (car ls2))
                  (cons (car r1) (car r2))
                  (map (cdr r1) (cdr r2)))))))))

(define-library-entry (for-each1 f ls)
  (unless (null? ls)
    (let for-each ([x (car ls)] [ls (cdr ls)])
      (if (null? ls)
          (f x)
          (begin
            (f x)
            (for-each (car ls) (cdr ls)))))))

(define-library-entry (for-each2 f ls1 ls2)
  (unless (null? ls1)
    (let for-each ([x (car ls1)] [ls1 (cdr ls1)] [ls2 ls2])
      (if (null? ls1)
          (f x (car ls2))
          (begin
            (f x (car ls2))
            (for-each (car ls1) (cdr ls1) (cdr ls2)))))))

(define-library-entry (andmap1 f ls)
  (or (null? ls)
      (let andmap ([ls ls])
        (let ([x (car ls)] [ls (cdr ls)])
          (if (null? ls)
              (f x)
              (and (f x) (andmap ls)))))))

(define-library-entry (ormap1 f ls)
  (and (not (null? ls))
       (let ormap ([ls ls])
         (let ([x (car ls)] [ls (cdr ls)])
           (if (null? ls)
               (f x)
               (or (f x) (ormap ls)))))))

(define-library-entry (vector-for-each1 p v)
  (let ([n (vector-length v)])
    (unless (fx= n 0)
      (let loop ([i 0])
        (let ([j (fx+ i 1)])
          (if (fx= j n)
              (p (vector-ref v i))
              (begin
                (p (vector-ref v i))
                (loop j))))))))

(define-library-entry (vector-for-each2 p u v)
  (let ([n (vector-length u)])
    (unless (fx= n 0)
      (let loop ([i 0])
        (let ([j (fx+ i 1)])
          (if (fx= j n)
              (p (vector-ref u i) (vector-ref v i))
              (begin
                (p (vector-ref u i) (vector-ref v i))
                (loop j))))))))

(define-library-entry (vector-map1 p v)
  (let ([n (vector-length v)])
    (let f ([i (fx- n 1)])
      (if (fx> i 0)
          (let ([x1 (p (vector-ref v i))] [x2 (p (vector-ref v (fx- i 1)))])
            (let ([vout (f (fx- i 2))])
              (vector-set! vout i x1)
              (vector-set! vout (fx- i 1) x2)
              vout))
          (make-vector n (if (fx= i 0) (p (vector-ref v 0)) 0))))))

(define-library-entry (vector-map2 p u v)
  (let ([n (vector-length u)])
    (let f ([i (fx- n 1)])
      (if (fx> i 0)
          (let ([x1 (p (vector-ref u i) (vector-ref v i))]
                [x2 (let ([j (fx- i 1)])
                      (p (vector-ref u j) (vector-ref v j)))])
            (let ([vout (f (fx- i 2))])
              (vector-set! vout i x1)
              (vector-set! vout (fx- i 1) x2)
              vout))
          (make-vector n
            (if (fx= i 0)
                (p (vector-ref u 0) (vector-ref v 0))
                0))))))

(define-library-entry (string-for-each1 p s)
  (let ([n (string-length s)])
    (unless (fx= n 0)
      (let loop ([i 0])
        (let ([j (fx+ i 1)])
          (if (fx= j n)
              (p (string-ref s i))
              (begin
                (p (string-ref s i))
                (loop j))))))))

(define-library-entry (string-for-each2 p s t)
  (let ([n (string-length s)])
    (unless (fx= n 0)
      (let loop ([i 0])
        (let ([j (fx+ i 1)])
          (if (fx= j n)
              (p (string-ref s i) (string-ref t i))
              (begin
                (p (string-ref s i) (string-ref t i))
                (loop j))))))))

(define-library-entry (fold-left1 combine nil ls)
  (if (null? ls)
      nil
      (let fold-left ([ls ls] [acc nil])
        (let ([cdrls (cdr ls)])
          (if (null? cdrls)
              (combine acc (car ls))
              (fold-left cdrls (combine acc (car ls))))))))

(define-library-entry (fold-left2 combine nil ls1 ls2)
  (if (null? ls1)
      nil
      (let fold-left ([ls1 ls1] [ls2 ls2] [acc nil])
        (let ([cdrls1 (cdr ls1)])
          (if (null? cdrls1)
              (combine acc (car ls1) (car ls2))
              (fold-left cdrls1 (cdr ls2)
                (combine acc (car ls1) (car ls2))))))))

(define-library-entry (fold-right1 combine nil ls)
  (let fold-right1 ([combine combine] [nil nil] [ls ls])
    (if (null? ls)
        nil
        ; naturally does cdrs first to avoid mutation sickness
        (combine (car ls) (fold-right1 combine nil (cdr ls))))))

(define-library-entry (fold-right2 combine nil ls1 ls2)
  (let fold-right2 ([combine combine] [nil nil] [ls1 ls1] [ls2 ls2])
    (if (null? ls1)
        nil
        ; naturally does cdrs first to avoid mutation sickness
        (combine (car ls1) (car ls2)
          (fold-right2 combine nil (cdr ls1) (cdr ls2))))))

(eval-when (compile)
(define-syntax doapply
  (syntax-rules ()
    [(_ p (x ...) ls) (if (null? ls) (p x ...) (doapply p (x ...) ls (ls)))]
    [(_ p (x ...) ls (ls1 ... lsn))
     (= (length #'(ls1 ...)) 4)
     ($apply p (fx+ (length '(x ...)) (length '(ls1 ...)) (length lsn))
       (list* x ... ls))]
    [(_ p (x ...) ls (ls1 ... lsn-1))
     (let ([lsn (cdr lsn-1)])
       (if (null? lsn)
           (p x ... (car ls1) ... (car lsn-1))
           (doapply p (x ...) ls (ls1 ... lsn-1 lsn))))]))
)

(define-library-entry (apply0 p ls)
  (doapply p () ls))

(define-library-entry (apply1 p x1 ls)
  (doapply p (x1) ls))

(define-library-entry (apply2 p x1 x2 ls)
  (doapply p (x1 x2) ls))

(define-library-entry (apply3 p x1 x2 x3 ls)
  (doapply p (x1 x2 x3) ls))

(define-library-entry ($check-continuation c check-as? as)
  (let ([who 'call-in-other-continuation])
    (unless ($continuation? c)
      ($oops who "~s is not a continuation" c))
    (when check-as?
      (unless (let ([c-as ($continuation-attachments c)])
                (or (eq? as c-as)
                    (and (pair? as)
                         (eq? (cdr as) c-as))))
        ($oops who "~s is not an extension of of the attachments of ~s" as c)))
    ($do-wind ($current-winders) ($continuation-winders c))))

(define-library-entry (eqv? x y)
  (if (eq? x y) 
      #t
      (exclusive-cond
        [(flonum? x) (and (flonum? y) ($fleqv? x y))]
        [($inexactnum? x)
         (and ($inexactnum? y)
              ($fleqv? ($inexactnum-real-part x) ($inexactnum-real-part y))
              ($fleqv? ($inexactnum-imag-part x) ($inexactnum-imag-part y)))]
        [(bignum? x) (and (bignum? y) (= x y))]
        [(ratnum? x) (and (ratnum? y) (= x y))]
        [($exactnum? x) (and ($exactnum? y) (= x y))]
        [else #f])))

(define-library-entry (memv x ls)
  (if (or (symbol? x) (fixmediate? x))
      (memq x ls)
      (let memv ([ls ls])
        (and (not (null? ls))
             (if (eqv? (car ls) x)
                 ls
                 (let ([ls (cdr ls)])
                   (and (not (null? ls))
                        (if (eqv? (car ls) x)
                            ls
                            (memv (cdr ls))))))))))

(define-library-entry (reverse ls)
  (let loop ([ls ls] [a '()])
    (if (null? ls)
        a
        (let ([ls2 (cdr ls)])
          (if (null? ls2)
              (cons (car ls) a)
              (loop (cdr ls2) (cons* (car ls2) (car ls) a)))))))

(let ()
  (include "hashtable-types.ss")

  (define (ht-size-cas! ht old new)
    (let-syntax ([size-field-pos
                  (lambda (stx)
                    (include "hashtable-types.ss")
                    (let loop ([names (csv7:record-type-field-names (record-type-descriptor ht))])
                      (if (eq? (car names) 'size)
                          0
                          (fx+ 1 (loop (cdr names))))))])
      ($record-cas! ht (size-field-pos) old new)))

  ;;; eq hashtable operations must be compiled with
  ;;; generate-interrupt-trap #f and optimize-level 3
  ;;; so they can't be interrupted by a collection
  (let ()
    (define-syntax lookup-keyval
      (syntax-rules ()
        [(_ ?x ?b succ fail)
         (let ([x ?x])
           (let loop ([b ?b])
             (if (fixnum? b)
                 fail
                 (let ([keyval ($tlc-keyval b)])
                   (if (eq? (car keyval) x)
                       (succ keyval)
                       (loop ($tlc-next b)))))))]))
  
    (define-syntax incr-size!
      (syntax-rules ()
        [(_ h vec)
         (let ([size (fx+ (ht-size h) 1)] [n (vector-length vec)])
           (ht-size-set! h size)
           (when (and (fx> size n) (fx< n (fxsrl (most-positive-fixnum) 1)))
             (adjust! h vec n (fxsll n 1))))]))
  
    (define-syntax decr-size!
      (syntax-rules ()
        [(_ h vec)
         (let ([size (fx- (ht-size h) 1)] [n (vector-length vec)])
           (ht-size-set! h size)
           (when (and (fx< size (fxsrl n 2)) (fx> n (ht-minlen h)))
             (let ([target (fxmax (fxsll size 2) (ht-minlen h))])
               (let loop ([n2 n])
                 (let ([n2 (fxsrl n2 1)])
                   (if (fx<= n2 target)
                       (adjust! h vec n n2)
                       (loop n2)))))))]))

    ;; Must be consistent with `eq_hash` in "../c/segment.h"
    (define-syntax eq-hash
      (syntax-rules ()
        [(_ v-expr) (fixmix ($fxaddress v-expr))]))
  
    (define adjust!
      (lambda (h vec1 n1 n2)
        (let ([vec2 ($make-eqhash-vector n2)] [mask2 (fx- n2 1)])
          (do ([i1 0 (fx+ i1 1)])
            ((fx= i1 n1))
            (let loop ([b (vector-ref vec1 i1)])
              (unless (fixnum? b)
                (let ([next ($tlc-next b)] [keyval ($tlc-keyval b)])
                  (let ([i2 (fxlogand (eq-hash (car keyval)) mask2)])
                    ($set-tlc-next! b (vector-ref vec2 i2))
                    (vector-set! vec2 i2 b))
                  (loop next)))))
          (ht-vec-set! h vec2))))
  
    (define-library-entry (eq-hashtable-ref h x v)
      (lookup-keyval x
        (let ([vec (ht-vec h)])
          (vector-ref vec (fxlogand (eq-hash x) (fx- (vector-length vec) 1))))
        cdr v))
  
    (define-library-entry (eq-hashtable-ref-cell h x)
      (lookup-keyval x
        (let ([vec (ht-vec h)])
          (vector-ref vec (fxlogand (eq-hash x) (fx- (vector-length vec) 1))))
        (lambda (x) x)
        #f))

    (define-library-entry (eq-hashtable-contains? h x)
      (lookup-keyval x
        (let ([vec (ht-vec h)])
          (vector-ref vec (fxlogand (eq-hash x) (fx- (vector-length vec) 1))))
        (lambda (x) #t)
        #f))
  
    (define-library-entry (eq-hashtable-cell h x v)
      (let* ([vec (ht-vec h)]
             [idx (fxlogand (eq-hash x) (fx- (vector-length vec) 1))]
             [b (vector-ref vec idx)])
        (lookup-keyval x b
          values
          (let ([keyval (let ([subtype (eq-ht-subtype h)])
                          (cond
                           [(eq? subtype (constant eq-hashtable-subtype-normal)) (cons x v)]
                           [(eq? subtype (constant eq-hashtable-subtype-weak)) (weak-cons x v)]
                           [else (ephemeron-cons x v)]))])
            (vector-set! vec idx ($make-tlc h keyval b))
            (incr-size! h vec)
            keyval))))

    ;; Note: never adjusts the vector size. Use `eq-hashtable-set!`
    ;; with exclusive access (perhaps in a GC callback) to enable
    ;; resizing.
    (define-library-entry (eq-hashtable-try-atomic-cell h x v)
      (let* ([vec (ht-vec h)]
             [idx (fxlogand (eq-hash x) (fx- (vector-length vec) 1))]
             [b (vector-ref vec idx)])
        (lookup-keyval x b
          values
          (let ([keyval (let ([subtype (eq-ht-subtype h)])
                          (cond
                           [(eq? subtype (constant eq-hashtable-subtype-normal)) (cons x v)]
                           [(eq? subtype (constant eq-hashtable-subtype-weak)) (weak-cons x v)]
                           [else (ephemeron-cons x v)]))])
            (and (vector-cas! vec idx b ($make-tlc h keyval b))
                 (let loop ()
                   (let* ([old-size (ht-size h)]
                          [size (fx+ old-size 1)])
                     (or (ht-size-cas! h old-size size)
                         (loop))))
                 keyval)))))
  
    (let ()
      (define do-set!
        (lambda (h x v)
          (let* ([vec (ht-vec h)]
                 [idx (fxlogand (eq-hash x) (fx- (vector-length vec) 1))]
                 [b (vector-ref vec idx)])
            (lookup-keyval x b
              (lambda (keyval) (set-cdr! keyval v))
              (begin
                (vector-set! vec idx
                  ($make-tlc h
                    (let ([subtype (eq-ht-subtype h)])
                      (cond
                       [(eq? subtype (constant eq-hashtable-subtype-normal)) (cons x v)]
                       [(eq? subtype (constant eq-hashtable-subtype-weak)) (weak-cons x v)]
                       [else (ephemeron-cons x v)]))
                    b))
                (incr-size! h vec))))))
  
      (define-library-entry (eq-hashtable-set! h x v)
        (do-set! h x v))
  
      (define-library-entry (eq-hashtable-update! h x p v)
        (let* ([vec (ht-vec h)]
               [idx (fxlogand (eq-hash x) (fx- (vector-length vec) 1))]
               [b (vector-ref vec idx)])
          (lookup-keyval x b
            (lambda (a) (set-cdr! a (p (cdr a))))
            (do-set! h x (p v))))))
  
    (define-library-entry (eq-hashtable-delete! h x)
      (let* ([vec (ht-vec h)]
             [idx (fxlogand (eq-hash x) (fx- (vector-length vec) 1))]
             [b (vector-ref vec idx)])
        (unless (fixnum? b)
          (if (eq? (car ($tlc-keyval b)) x)
              (begin
                (vector-set! vec idx ($tlc-next b))
                ($set-tlc-next! b #f)
                (decr-size! h vec))
              (let loop ([b b])
                (let ([n ($tlc-next b)])
                  (unless (fixnum? n)
                    (if (eq? (car ($tlc-keyval n)) x)
                        (begin
                          ($set-tlc-next! b ($tlc-next n))
                          ($set-tlc-next! n #f)
                          (decr-size! h vec))
                        (loop n)))))))))
  )

  ; symbol hashtable operations
  (let ()
    (define-syntax incr-size!
      (syntax-rules ()
        [(_ h vec)
         (let ([size (fx+ (ht-size h) 1)] [n (vector-length vec)])
           (ht-size-set! h size)
           (when (and (fx> size n) (fx< n (fxsrl (most-positive-fixnum) 1)))
             (adjust! h vec (fxsll n 1))))]))

    (define-syntax decr-size!
      (syntax-rules ()
        [(_ h vec)
         (let ([size (fx- (ht-size h) 1)] [n (vector-length vec)])
           (ht-size-set! h size)
           (when (and (fx< size (fxsrl n 2)) (fx> n (ht-minlen h)))
             (adjust! h vec (fxsrl n 1))))]))

    (define adjust!
      (lambda (h vec1 n2)
        (let ([vec2 (make-vector n2 '())]
              [mask2 (fx- n2 1)])
          (vector-for-each
            (lambda (b)
              (for-each
                (lambda (a)
                  (let ([hc (fxlogand ($symbol-hash (car a)) mask2)])
                    (vector-set! vec2 hc (cons a (vector-ref vec2 hc)))))
                b))
            vec1)
          (ht-vec-set! h vec2))))

    (define-library-entry (symbol-hashtable-ref h x v)
      (let ([hc ($symbol-hash x)])
        (if hc
            (let ([vec (ht-vec h)])
              (let loop ([b (vector-ref vec (fxlogand hc (fx- (vector-length vec) 1)))])
                (if (null? b)
                    v
                    (let ([a (car b)])
                      (if (eq? (car a) x) (cdr a) (loop (cdr b)))))))
            (pariah v))))

    (define-library-entry (symbol-hashtable-ref-cell h x)
      (let ([hc ($symbol-hash x)])
        (if hc
            (let ([vec (ht-vec h)])
              (let loop ([b (vector-ref vec (fxlogand hc (fx- (vector-length vec) 1)))])
                (if (null? b)
                    #f
                    (let ([a (car b)])
                      (if (eq? (car a) x) a (loop (cdr b)))))))
            (pariah #f))))

    (define-library-entry (symbol-hashtable-contains? h x)
      (let ([hc ($symbol-hash x)])
        (and hc
             (let ([vec (ht-vec h)])
               (let loop ([b (vector-ref vec (fxlogand hc (fx- (vector-length vec) 1)))])
                 (and (not (null? b))
                      (or (eq? (caar b) x)
                          (loop (cdr b)))))))))

    (define-library-entry (symbol-hashtable-cell h x v)
      (let ([vec (ht-vec h)] [hc ($symbol-hash x)])
        (if hc
            (let ([idx (fxlogand hc (fx- (vector-length vec) 1))])
              (let ([bucket (vector-ref vec idx)])
                (let loop ([b bucket])
                  (if (null? b)
                      (let ([a (cons x v)])
                        (vector-set! vec idx (cons a bucket))
                        (incr-size! h vec)
                        a)
                      (let ([a (car b)])
                        (if (eq? (car a) x)
                            a
                            (loop (cdr b))))))))
            (let ([idx (fxlogand (symbol-hash x) (fx- (vector-length vec) 1))])
              (let ([a (cons x v)])
                (vector-set! vec idx (cons a (vector-ref vec idx)))
                (incr-size! h vec)
                a)))))

    (define-library-entry (symbol-hashtable-set! h x v)
      (let ([vec (ht-vec h)] [hc ($symbol-hash x)])
        (if hc
            (let ([idx (fxlogand hc (fx- (vector-length vec) 1))])
              (let ([bucket (vector-ref vec idx)])
                (let loop ([b bucket])
                  (if (null? b)
                      (begin
                        (vector-set! vec idx (cons (cons x v) bucket))
                        (incr-size! h vec))
                      (let ([a (car b)])
                        (if (eq? (car a) x) (set-cdr! a v) (loop (cdr b))))))))
            (let ([idx (fxlogand (symbol-hash x) (fx- (vector-length vec) 1))])
              (vector-set! vec idx (cons (cons x v) (vector-ref vec idx)))
              (incr-size! h vec)))))

    (define-library-entry (symbol-hashtable-update! h x p v)
      (let ([vec (ht-vec h)] [hc ($symbol-hash x)])
        (if hc
            (let ([idx (fxlogand hc (fx- (vector-length vec) 1))])
              (let ([bucket (vector-ref vec idx)])
                (let loop ([b bucket])
                  (if (null? b)
                      (begin
                        (vector-set! vec idx (cons (cons x (p v)) bucket))
                        (incr-size! h vec))
                      (let ([a (car b)])
                        (if (eq? (car a) x)
                            (set-cdr! a (p (cdr a)))
                            (loop (cdr b))))))))
            (let ([idx (fxlogand (symbol-hash x) (fx- (vector-length vec) 1))])
              (vector-set! vec idx (cons (cons x (p v)) (vector-ref vec idx)))
              (incr-size! h vec)))))

    (define-library-entry (symbol-hashtable-delete! h x)
      (let ([hc ($symbol-hash x)])
        (when hc
          (let ([vec (ht-vec h)])
            (let ([idx (fxlogand hc (fx- (vector-length vec) 1))])
              (let loop ([b (vector-ref vec idx)] [p #f])
                (unless (null? b)
                  (let ([a (car b)])
                    (if (eq? (car a) x)
                        (begin
                          (if p (set-cdr! p (cdr b)) (vector-set! vec idx (cdr b)))
                          (decr-size! h vec))
                        (loop (cdr b) b))))))))))
  )
)

;;; the routines below may cause significant allocation without any
;;; embedded calls to other trap-checking routines, so we enable
;;; generation-interrupt-trap for them.
(eval-when (compile) (generate-interrupt-trap #t))

(define-library-entry (append ls1 ls2)
  (let append ([ls1 ls1] [ls2 ls2])
    (if (null? ls1)
        ls2
        (let ((cdr-ls1 (cdr ls1)))
          (if (null? cdr-ls1)
              (cons (car ls1) ls2)
              (list* (car ls1) (car cdr-ls1) (append (cdr cdr-ls1) ls2)))))))
