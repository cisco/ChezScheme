;;; print.ss
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
(define-constant cycle-node-max 1000)

(define-syntax decr
  (lambda (x)
    (syntax-case x ()
      ((_ x) (identifier? #'x) #'(and x (fx- x 1))))))

(define-syntax limit?
  (syntax-rules ()
    ((_ x) (eq? x 0))))
) ;eval-when

;;; $make-graph-env is shared with pretty.ss
(define $make-graph-env)

(define record-writer
  (let ([rw-ht #f] [cache '()])
    (case-lambda
      [(rtd)
       (define default-record-writer
         (lambda (x p wr)
           (let ([rtd ($record-type-descriptor x)])
             (cond ; keep in sync with wrhelp
               [(record-type-opaque? rtd)
                (fprintf p "#<~a>" (csv7:record-type-name rtd))]
               [(record-type-descriptor? x)
                (fprintf p "#<record type ~:s>" (record-type-uid x))]
               [(record-constructor-descriptor? x)
                (fprintf p "#<record constructor descriptor>")]
               [else
                (display "#[" p)
               ; we use write instead of wr here so that the field doesn't get
               ; a reference (#n#) when print-graph is true.
                (write (or (record-reader rtd) (record-type-uid rtd)) p)
                (do ([flds (csv7:record-type-field-names rtd) (cdr flds)]
                     [i 0 (+ i 1)])
                    ((null? flds))
                  (write-char #\space p)
                  (wr ((csv7:record-field-accessor rtd i) x) p))
                (write-char #\] p)]))))
       (unless (record-type-descriptor? rtd)
         ($oops 'record-writer "~s is not a record-type descriptor" rtd))
       (if rw-ht
           (or (eq-hashtable-ref rw-ht rtd #f)
               (let ([proc (or (let f ([rtd rtd])
                                 (let ([rtd (record-type-parent rtd)])
                                   (and rtd
                                     (or (eq-hashtable-ref rw-ht rtd #f)
                                         (f rtd)))))
                               default-record-writer)])
                ; cache the derived entry
                 (eq-hashtable-cell rw-ht rtd proc)
                 (set! cache (weak-cons rtd cache))
                 proc))
           default-record-writer)]
      [(rtd proc)
       (unless (record-type-descriptor? rtd)
         ($oops 'record-writer "~s is not a record-type descriptor" rtd))
       (unless (or (procedure? proc) (eq? proc #f))
         ($oops 'record-writer "~s is not a procedure" proc))
       (unless rw-ht (set! rw-ht (make-weak-eq-hashtable)))
      ; remove derived entries for rtd and any of its children
       (set! cache
         (let f ([cache cache])
           (cond
             [(null? cache) '()]
             [(eq? (car cache) #!bwp) (f (cdr cache))]
             [(let g ([x (car cache)])
                (and x (or (eq? x rtd) (g (record-type-parent x)))))
              (eq-hashtable-delete! rw-ht (car cache))
              (f (cdr cache))]
             [else (weak-cons (car cache) (f (cdr cache)))])))
       (let ([a (eq-hashtable-cell rw-ht rtd proc)])
         (unless (eq? (cdr a) proc) (set-cdr! a proc)))])))

(let ()
(define black-hole '#0=#0#)

(define hashable?
  (lambda (x)
    (if ($immediate? x)
        (eq? x black-hole)
        (and
          ($object-in-heap? x)
          (or (pair? x)
              (vector? x)
              (box? x)
              (and ($record? x) (not (eq? x #!base-rtd)))
              (fxvector? x)
              (string? x)
              (bytevector? x)
              (gensym? x))))))

(define bit-sink
  (let ([bsp #f])
    (define make-bit-sink-port
      (lambda ()
        (define handler
          (message-lambda
            (lambda (msg . args) ($oops 'bit-sink-port "operation ~s not handled" msg))
            [(block-write p s n) (void)]
            [(clear-output-port p) (set-textual-port-output-index! p 0)]
            [(close-port p)
             (set-textual-port-output-size! p 0)
             (mark-port-closed! p)]
            [(flush-output-port p) (set-textual-port-output-index! p 0)]
            [(port-name p) "bit-sink port"]
            [(write-char c p) (set-textual-port-output-index! p 0)]))
        (make-output-port handler (make-string 1024))))
    (lambda ()
      (or bsp (let ([p (make-bit-sink-port)]) (set! bsp p) p)))))

(define (graph-env x lev len)
   ;; NOTE: if used as is in fasl.ss, fasl.ss will have to play
   ;; $last-new-vector-element game.
   ;; $make-graph-env takes an object, a print level, and a print
   ;; length, and returns a procedure whose first argument is a
   ;; message, one of 'tag, 'tag?, or 'count.  If the message is
   ;; 'tag, one of #f, (mark . <n>), or (ref . <n>) is returned if
   ;; the second argument needs no tag, a mark tag, or a reference
   ;; tag.  If the message is 'tag?, #f is returned iff the second
   ;; argument needs no tag.  If the message is 'count, the number
   ;; of items needing tags is returned.
   (let ([ht (make-eq-hashtable)] [count 0])
      (let find-dupls ([x x] [lev lev] [lslen len])
        (when (and (hashable? x) (not (limit? lev)))
          (let ([a (eq-hashtable-cell ht x 'first)])
            (case (cdr a)
              [(first)
               (set-cdr! a #f)
               (cond
                 [(pair? x)
                  (unless (limit? lslen)
                    (find-dupls (car x) (decr lev) len)
                    (find-dupls (cdr x) lev (decr lslen)))]
                 [(vector? x)
                  (unless (fx= (vector-length x) 0)
                    (let ([m (if (print-vector-length)
                                 ($last-new-vector-element vector-length vector-ref x)
                                 (fx- (vector-length x) 1))]
                          [lev (decr lev)])
                      (let f ([i 0] [veclen len])
                        (unless (or (fx> i m) (limit? veclen))
                          (find-dupls (vector-ref x i) lev len)
                          (f (fx+ i 1) (decr veclen))))))]
                 [(and ($record? x) (not (eq? x #!base-rtd)))
                  (when (print-record)
                    ((record-writer ($record-type-descriptor x)) x (bit-sink)
                     (lambda (x p) ; could check for p == (bit-sink)
                       (find-dupls x (decr lev) len))))]
                 [(box? x) (find-dupls (unbox x) (decr lev) len)]
                 [(eq? x black-hole) (find-dupls x (decr lev) len)])]
              [(#f)
               (set! count (fx+ count 1))
               (set-cdr! a #t)]))))
      (and (not (fx= count 0))
           (let ([next -1])
              (case-lambda
                 [(msg x)
                  (case msg
                     [(tag)
                      (and (hashable? x)
                           (let ([a (eq-hashtable-cell ht x #f)])
                              (case (cdr a)
                                 [(#f) #f]
                                 [(#t) (set! next (fx+ next 1))
                                       (set-cdr! a `(ref . ,next))
                                       `(mark . ,next)]
                                 [else (cdr a)])))]
                     [(tag?) (eq-hashtable-ref ht x #f)])]
                 [(msg) (case msg [(count) count])])))))

(define (really-cyclic? x lev len)

  (define cyclic?
    (lambda (x curlev lstlen)
      (if ($immediate? x)
          (if (eq? x black-hole) (not lev) #f)
          (and ($object-in-heap? x)
               (cond
                 [(pair? x) (cyclic-structure? x curlev lstlen cyclic-pair?)]
                 [(vector? x) (cyclic-structure? x curlev 0 cyclic-vector?)]
                 [(and ($record? x) (not (eq? x #!base-rtd)))
                  (and (print-record)
                       (cyclic-structure? x curlev lstlen
                         (lambda (x curlev lstlen)
                           (call/cc
                             (lambda (k)
                               ((record-writer ($record-type-descriptor x)) x (bit-sink)
                                (lambda (x p) ; could check for p == (bit-sink)
                                  (if (cyclic? x (fx+ curlev 1) 0)
                                      (k #t))))
                               #f)))))]
                 [(box? x) (cyclic-structure? x curlev 0 cyclic-box?)]
                 [else #f])))))

   (define cyclic-structure?
      (let ([ht (make-eq-hashtable)])
         (lambda (x curlev lstlen sub-cyclic?)
            (and (not (eq? curlev lev))
                 (let ([a (eq-hashtable-cell ht x #f)])
                    (let ([oldlev (cdr a)])
                       (if oldlev
                           (or (not (if (= oldlev curlev) len lev))
                               (sub-cyclic? x curlev lstlen))
                           (begin (set-cdr! a curlev)
                                  (or (sub-cyclic? x curlev lstlen)
                                      (begin (set-cdr! a #f) #f))))))))))

   (define cyclic-pair?
      (lambda (x curlev lstlen)
         (and (not (eq? lstlen len))
              (or (cyclic? (car x) (fx+ curlev 1) 0)
                  (cyclic? (cdr x) curlev (fx+ lstlen 1))))))

   (define cyclic-vector?
      (lambda (x curlev lstlen)
         (let ([n (vector-length x)] [curlev (fx+ curlev 1)])
            (let across ([i (fx- (if len (fxmin len n) n) 1)])
               (and (fx>= i 0)
                    (or (cyclic? (vector-ref x i) curlev 0)
                        (across (fx- i 1))))))))

   (define cyclic-box?
      (lambda (x curlev lstlen)
         (cyclic? (unbox x) (fx+ curlev 1) 0)))

  (cyclic? x 0 0)

)

(define maybe-cyclic?
  ;; brain damaged---can go essentially forever on very large trees
  ;; should keep separate count, lev, and len variables
  (lambda (x lev len)
    (let down ([x x]
               [xlev (if lev
                         (fxmin lev (constant cycle-node-max))
                         (constant cycle-node-max))])
      (cond
        [(fx= xlev 0) (or (not lev) (fx> lev (constant cycle-node-max)))]
        [($immediate? x) (if (eq? x black-hole) (not lev) #f)]
        [else
         (and ($object-in-heap? x)
              (cond
                [(pair? x)
                 (let across ([x x]
                              [xlen (if len
                                        (fxmin len (constant cycle-node-max))
                                        (constant cycle-node-max))])
                   (cond
                     [(fx= xlen 0)
                      (or (not len) (fx> len (constant cycle-node-max)))]
                     [(pair? x)
                      (or (down (car x) (fx- xlev 1))
                          (across (cdr x) (fx- xlen 1)))]
                     [else (down x (fx- xlev 1))]))]
                [(vector? x)
                 (let ([n (vector-length x)])
                   (let across ([i (fx- (if len (fxmin len n) n) 1)])
                     (and (fx>= i 0)
                          (or (down (vector-ref x i) (fx- xlev 1))
                              (across (fx- i 1))))))]
                [(and ($record? x) (not (eq? x #!base-rtd)))
                 (and (print-record)
                      (call/cc
                        (lambda (k)
                          ((record-writer ($record-type-descriptor x)) x (bit-sink)
                           (lambda (x p) ; could check for p == (bit-sink)
                             (if (down x (fx- xlev 1)) (k #t))))
                          #f)))]
                [(box? x) (down (unbox x) (fx- xlev 1))]
                [else #f]))]))))

(set! $make-graph-env
  (lambda (who x lev len)
    (and (if ($immediate? x)
             (eq? x black-hole)
             (and ($object-in-heap? x)
                  (or (pair? x) (vector? x) (box? x) (and ($record? x) (not (eq? x #!base-rtd))))))
         (or (print-graph)
             (and (not (and lev len))
                  (maybe-cyclic? x lev len)
                  (really-cyclic? x lev len)
                  (begin
                    (warningf who
                      "cycle detected; proceeding with (print-graph #t)")
                    #t)))
         (graph-env x lev len))))
)

;;; $last-new-vector-element is shared with read.ss and pretty.ss

(define $last-new-vector-element
   ; assumes vector has at least one element
   (lambda (vlen vref x)
      (let ([n (vlen x)])
         (let ([y (vref x (fx- n 1))])
            (do ([i (fx- n 2) (fx- i 1)])
                ((or (fx< i 0) (not (eqv? y (vref x i))))
                 (fx+ i 1)))))))

;;; $flonum->digits is exported to allow us to play around with formatted IO
#|
(flonum->digits <flonum> <output radix> <mode> <position>)

<flonum> is the number you want to convert to digits.
<output radix> is the radix (usually 10) that you want the output
    digits to be in
<mode> is one of:
    'normal:  this is used for "free format" printing.  In normal
        mode, the digits produced are the minumum number needed
        to reproduce the internal representation.  In this case,
        <position> is ignored (pass in zero).

    'absolute: this is for fixed-format floating point notation.
        In this case, digit production is cut off (with
        appropriate rounding) at position <position>.  Negative
        values of <position> specify positions to the right of
        the radix point; positive values specify positions to the
        left.

    'relative: this is for fixed-format exponential notation.
        In this case, digit production is cut off (with
        appropriate rounding) at position <position> relative
        to the first digit.  <position> must be negative.

Return Value:

In any of the modes, you receive an infinite list consisting of

    * the sign represented by 1 for + and -1 for -
    * the exponent
    * the significant digits w/o trailing zeros
    * a (posibly empty) sequence of -1's, and
    * a (posibly empty) sequence of -2's.

The -1's should be printed as zeros if you need them; a -1 digit is
equivalent to zero except that it is not necessary to print it to
fully specify the number.

The -2's should be printed as hash marks (#) or something similar;
a -2 digit carries no information whatsoever, i.e., once you start
getting -2 digits, all precision is gone.  For normalized IEEE
64-bit floats, -2 digits won't appear until after 15 or so other
digits.  For denormalized floats, -2 digits may appear much sooner
(perhaps even starting with the second digit).

Positive floating point zero returns with (1 0 -1 ...).  Negative
floating point returns with (1 0 -1 ...).
|#

(define $flonum->digits)
(let ()
(define terminator-1 '#1=(-1 . #1#))
(define terminator-2 '#2=(-2 . #2#))

(define invlog2of
  ;; must be accurate to several decimal places
  (let ([v (let ([ln2 (log 2)])
             (let f ([n 36] [ls '()])
               (if (fx= n 1)
                   (list->vector ls)
                   (f (fx- n 1) (cons (/ ln2 (log n)) ls)))))])
    (lambda (x)
      (vector-ref v (fx- x 2)))))

(define exptt
  (let ([v (let f ([k 325] [ls '()])
             (if (fx< k 0)
                 (list->vector ls)
                 (f (fx- k 1) (cons (expt 10 k) ls))))])
    (lambda (ob k)
      (if (and (fx= ob 10) (fx<= 0 k 320))
          (vector-ref v k)
          (expt ob k)))))

(define dragon4
   ;; ob is the base of the output representation
   ;; e is the exponent of the floating point number
   ;; f is the significand of the floating point number
   ;; p is the precision of the floating point number
   ;; cutoffmode is one of normal (no cutoff), absolute (for fixed
   ;;    formats), or relative (for fixed exponential formats)
   ;; initialcutoffplace is the digit position where cutoff should occur
   ;;    (in radix-ob digits)
   ;; when cutoffmode is relative, initialcutoffplace must be negative
   ;; internally, ruf, cop, r, s, m+, m- are roundupflag, cutoffplace,
   ;;    R, S, M+, M- from original algorithm
   (lambda (e f p ob cutoffmode initialcutoffplace)
     (define estimate
       (lambda (r s m- m+ ruf)
         (let ([k (fx1+
                   (exact
                    (floor (- (* (fx+ (integer-length f) e -1)
                                 (invlog2of ob))
                              1e-10))))])
           (cond
            [(> k 0)
             (let ([scale (exptt ob k)])
               (fixup k r (* s scale) m- m+ ruf))]
            [(< k 0)
             (let* ([scale (exptt ob (fx- k))]
                    [m- (* m- scale)])
               (fixup k (* r scale) s m- (and m+ (ash m- 1)) ruf))]
            [else
             (fixup k r s m- m+ ruf)]))))

     (define fixup
       (lambda (k r s m- m+ ruf)
         (if ((if ruf >= >) (+ r (or m+ m-)) s)
             (cutoffadjust0 (fx+ k 1) r (* s ob) m- m+ ruf)
             (cutoffadjust0 k r s m- m+ ruf))))

     (define cutoffadjust0
       (lambda (k r s m- m+ ruf)
         (case cutoffmode
           [(normal) (generate0 k r s m- m+ ruf #f)]
           [(absolute)
            (cutoffadjust k r s m- m+ ruf (fx- initialcutoffplace k))]
           [(relative)
            (when (fx>= initialcutoffplace 0)
              ($oops '$flonum->digits "nonnegative relative cutoffplace ~s"
                     initialcutoffplace))
            (cutoffadjust k r s m- m+ ruf initialcutoffplace)]
           [else
            ($oops '$flonum->digits "invalid cutoffmode ~s"
                   cutoffmode)])))

     (define cutoffadjust
       (lambda (k r s m- m+ ruf a)
         (let ([y (/ (if (fx>= a 0)
                         (* s (exptt ob a))
                         (/ s (exptt ob (fx- a)))) 2)])
           (if (> y m-)
               (if (ratnum? y)
                   (let* ([d (denominator y)]
                          [r (* r d)]
                          [s (* s d)]
                          [m- (numerator y)]
                          [m+ (and m+
                                   (let ([new-m+ (* m+ d)])
                                     (if (> new-m+ m-) new-m+ #f)))]
                          [ruf (or ruf (not m+))])
                     (fixup2 k r s m- m+ ruf (fx+ k a)))
                   (let* ([m- y]
                          [m+ (and m+ (if (> m+ y) m+ #f))]
                          [ruf (or ruf (not m+))])
                     (fixup2 k r s m- m+ ruf (fx+ k a))))
               (generate0 k r s m- m+ ruf (fx+ k a))))))

     (define fixup2
       (lambda (k r s m- m+ ruf cop)
         (if ((if ruf >= >) (+ r (or m+ m-)) s)
             (cutoffadjust0 (fx+ k 1) r (* s ob) m- m+ ruf)
             (generate0 k r s m- m+ ruf cop))))

     (define generate0
       (lambda (k r s m- m+ ruf cop)
         (let ([k (fx- k 1)])
           (cons k (generate k r s m- m+ ruf cop)))))

     (define generate
       (lambda (k r s m- m+ ruf cop)
         (let* ([rob (* r ob)]
                [q-r ($quotient-remainder rob s)]
                [u (car q-r)]
                [r (cdr q-r)]
                [m- (* m- ob)]
                [m+ (and m+ (ash m- 1))]
                [low ((if ruf <= <) r m-)]
                [high ((if ruf >= >) (+ r (or m+ m-)) s)])
           (if (and (not low) (not high))
               (cons u (generate (fx- k 1) r s m- m+ ruf cop))
               (let ([d (cond [(and low (not high)) u]
                              [(and high (not low)) (fx+ u 1)]
                              [(< (ash r 1) s) u]
                              [else (fx+ u 1)])])
                 (cond
                   [(not cop) (cons d terminator-1)]
                   [(fx< k cop) terminator-1]
                   [(fx= k cop) (cons d terminator-1)]
                   [else (cons d
                           (if (fx= d u)
                               (generate1 k (+ r (or m+ m-)) s cop)
                               (generate1 k (- (+ r (or m+ m-)) s) s cop)))]))))))

    ; delta may be zero, in which case all digits are significant,
    ; even if we've been asked for 1,000,000 of them.  This is due to
    ; our definition of (in)significant: "a digit is insignificant when
    ; it and all digits after it can be replaced by any base-B digits
    ; without altering the value of the number when input.  In other
    ; words, a digit is insignificant if incrementing the preceding
    ; digit does not cause the number to fall outside the rounding
    ; range of v."  For 1e23, which falls exactly midway between two
    ; fp numbers and reads as the next one down due to "unbiased rounding",
    ; if we add even a single 1 digit way down, we're pushed to the next
    ; higher (when read).  For example:
    ; 100000000000000000000000.000000000000000000000000000000000000001
    ; reads as 1.0000000000000001e23.  We probably don't get into this
    ; situation unless ruf is allowed to be true for absolute and relative
    ; modes below, since with ruf false, we won't try to print a number that
    ; is exactly half way between two floating-point numbers.
     (define generate1
       (lambda (k delta s cop)
         (if (>= delta s)
             terminator-2
             (let ([rest (let ([k (fx- k 1)])
                           (if (fx= k cop)
                               terminator-1
                               (generate1 k (* delta ob) s cop)))])
               (if (eq? rest terminator-1)
                   rest
                   (cons -1 rest))))))

    ; we restrict ruf = true to cutoffmode normal so that we don't end
    ; up in the situation described above where delta is zero and we
    ; show an indefinite number of digits as significant.
     (let ([b (and (fx> e -1074) (= f 4503599627370496))]
           [ruf (and (eq? cutoffmode 'normal) (even? f))])
       (if (fx>= e 0)
           (let* ([r (ash f (if b (fx+ e 2) (fx+ e 1)))]
                  [m- (ash 1 e)]
                  [m+ (and b (ash m- 1))]
                  [s (if b 4 2)])
             (estimate r s m- m+ ruf))
           (let* ([r (ash f (if b 2 1))]
                  [s (ash 1 (if b (fx- 2 e) (fx- 1 e)))]
                  [m+ (and b 2)])
             (estimate r s 1 m+ ruf))))))

(set! $flonum->digits
  (lambda (x ob cutoffmode initialcutoffplace)
    (let ([dx (if (flonum? x) (decode-float x) x)])
      (let ([f (vector-ref dx 0)]
            [e (vector-ref dx 1)]
            [s (vector-ref dx 2)])
        (cons s (if (= f 0)
                    (cons 0 terminator-1)
                    (dragon4 e f 53 ob cutoffmode initialcutoffplace)))))))
)

(define $write-pretty-quick)
(define write)
(define display)

(let ()
(define digit-char-table "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define-syntax digit->char
  (syntax-rules ()
    ((_ d) (string-ref digit-char-table d))))

(define wr
   (lambda (x r lev len d? env p)
      (let ([a (and env (env 'tag x))])
         (if (not a)
             (wrhelp x r lev len d? env p)
             (record-case a
                [(mark) n
                 (write-char #\# p)
                 (wrfixits n 10 p)
                 (write-char #\= p)
                 (wrhelp x r lev len d? env p)]
                [(ref) n
                 (write-char #\# p)
                 (wrfixits n 10 p)
                 (write-char #\# p)])))))

(define wrhelp
   (lambda (x r lev len d? env p)
     (define void? (lambda (x) (eq? x (void))))
     (define black-hole? (lambda (x) (eq? x '#3=#3#)))
     (define base-rtd? (lambda (x) (eq? x #!base-rtd)))
     (if-feature pthreads
       (begin
         (define $condition? thread-condition?)
         (define $condition-name condition-name)
         (define $mutex? mutex?)
         (define $mutex-name mutex-name))
       (begin
         (define $condition? (lambda (x) #f))
         (define $condition-name (lambda (x) #f))
         (define $mutex? (lambda (x) #f))
         (define $mutex-name (lambda (x) #f))))
     (cond
       [($immediate? x)
        (type-case x
          [(fixnum?) (wrfixnum x r d? p)]
          [(null?) (display-string "()" p)]
          [(boolean?) (display-string (if x "#t" "#f") p)]
          [(char?) (if d? (write-char x p) (wrchar x p))]
          [(eof-object?) (display-string "#!eof" p)]
          [(bwp-object?) (display-string "#!bwp" p)]
          [($unbound-object?) (display-string "#<unbound object>" p)]
          [(void?) (display-string "#<void>" p)]
          [(black-hole?) (wrblack-hole x r lev len d? env p)]
          [else (display-string "#<garbage>" p)])]
       [($object-in-heap? x)
        (type-case x
          [(symbol?)
           (cond
             [d? (display-string (symbol->string x) p)]
             [(gensym? x)
              (case (print-gensym)
                [(#f)
                 (wrsymbol (symbol->string x) p)]
                [(pretty)
                 (display-string "#:" p)
                 (wrsymbol (symbol->string x) p)]
                [(pretty/suffix)
                 (let ((uname (gensym->unique-string x)))
                   (define string-prefix?
                     (lambda (x y)
                       (let ([n (string-length x)])
                         (and (fx<= n (string-length y))
                              (let prefix? ([i 0])
                                (or (fx= i n)
                                    (and (char=? (string-ref x i) (string-ref y i))
                                         (prefix? (fx+ i 1)))))))))
                   (wrsymbol (symbol->string x) p)
                   (display-string "." p)
                   (if (and $session-key (string-prefix? $session-key uname))
                       (display-string (substring uname (string-length $session-key) (string-length uname)) p)
                       (wrsymbol uname p #t)))]
                [else
                  (let ((uname (gensym->unique-string x)))
                    (display-string "#{" p)
                    (wrsymbol (symbol->string x) p)
                    (display-string " " p)
                    (wrsymbol uname p)
                    (display-string "}" p))])]
             [else (wrsymbol (symbol->string x) p)])]
          [(pair?) (wrpair x r lev len d? env p)]
          [(string?) (if d? (display-string x p) (wrstring x p))]
          [(vector?) (wrvector vector-length vector-ref #f x r lev len d? env p)]
          [(fxvector?) (wrvector fxvector-length fxvector-ref "vfx" x r lev len d? env p)]
          [(bytevector?) (wrvector bytevector-length bytevector-u8-ref "vu8" x r lev len d? env p)]
          [(flonum?) (wrflonum #f x r d? p)]
          ; catch before record? case
          [($condition?)
           (cond
            (($condition-name x) =>
             (lambda (name)
               (display-string "#<condition " p)
               (wrsymbol (symbol->string name) p)
               (write-char #\> p)))
            (else (display-string "#<condition>" p)))]
          [($mutex?)
           (cond
            (($mutex-name x) =>
             (lambda (name)
               (display-string "#<mutex " p)
               (wrsymbol (symbol->string name) p)
               (write-char #\> p)))
            (else (display-string "#<mutex>" p)))]
          [(base-rtd?) (display-string "#!base-rtd" p)]
          [($record?)
           (if (print-record)
               (if (limit? lev)
                   (display-string "#[...]" p)
                   ((record-writer ($record-type-descriptor x)) x p
                    (lambda (x p) ; could check for p == old p
                      (wr x r (decr lev) len d? env p))))
               (let ([rtd ($record-type-descriptor x)])
                 (cond ; keep in sync with default-record-writer
                   [(record-type-opaque? rtd)
                    (display-string "#<" p)
                    (display-string (csv7:record-type-name rtd) p)
                    (display-string ">" p)]
                   [(record-type-descriptor? x)
                    (display-string "#<record type " p)
                    (wrsymbol (symbol->string (record-type-uid x)) p)
                    (display-string ">" p)]
                   [(record-constructor-descriptor? x)
                    (display-string "#<record constructor descriptor>" p)]
                   [else
                     (display-string "#<record of type " p)
                     (display-string (csv7:record-type-name rtd) p)
                     (display-string ">" p)])))]
          [(bignum?) (wrbignum x r d? p)]
          [(ratnum?) (wrratnum x r d? p)]
          [($inexactnum?) (wrinexactnum x r d? p)]
          [($exactnum?) (wrexactnum x r d? p)]
          [(box?) (wrbox x r lev len d? env p)]
          [(procedure?)
           (if ($continuation? x)
               (wrcontinuation x p)
               (wrprocedure x p))]
          [(port?) (wrport x p)]
          [($code?) (wrcode x p)]
          [($tlc?) (display-string "#<tlc>" p)]
          [(thread?) (wrthread x p)]
          [($rtd-counts?) (display-string "#<rtd-counts>" p)]
          [else (display-string "#<garbage>" p)])]
        [else (display-string "#<foreign>" p)])))

(module (wrprocedure wrcode)
  (include "types.ss")

  (define wrcodename
    (lambda (x p)
      (cond
        [(let ([s ($code-name x)])
           (and (string? s) s)) =>
         (lambda (s)
           (write-char #\space p)
           (display-string s p))])
      (cond
        [(let ([info ($code-info x)])
           (and (code-info? info) (code-info-src info))) =>
         (lambda (src)
           (fprintf p " at ~a:~a"
             (path-last (source-file-descriptor-name (source-sfd src)))
             (if (source-2d? src)
                 (format "~a.~a" (source-2d-line src) (source-2d-column src))
                 (source-bfp src))))])))

  (define wrprocedure
    (lambda (x p)
      (display-string "#<procedure" p)
      (wrcodename ($closure-code x) p)
      (write-char #\> p)))

  (define wrcode
    (lambda (x p)
      (display-string "#<code" p)
      (wrcodename x p)
      (write-char #\> p))))

(define wrthread
  (lambda (x p)
    (display-string "#<thread " p)
    (let ([n (with-tc-mutex
               (let ([tc ($thread-tc x)])
                 (and (not (eq? tc 0)) ($tc-field 'threadno tc))))])
      (if n
          (if (fixnum? n) (wrfixnum n 10 #t p) (wrbignum n 10 #t p))
          (display "destroyed" p)))
    (write-char #\> p)))

(define wrcontinuation
  (lambda (x p)
    (cond
      [(eq? x $null-continuation)
       (display-string "#<null continuation>" p)]
      [(= ($continuation-stack-length x) (constant unscaled-shot-1-shot-flag))
       (display-string "#<shot one-shot continuation>" p)]
      [else
       (display-string "#<" p)
       (unless (= ($continuation-stack-length x)
                  ($continuation-stack-clength x))
         (display-string "one-shot " p))
       (let ([code ($continuation-return-code x)])
         (if ($system-code? code)
             (display-string "system continuation" p)
             (display-string "continuation" p))
         (let ([s ($code-name code)])
           (when (string? s)
             (display-string " in " p)
             (display-string s p)))
         (write-char #\> p))])))

(define wrblack-hole
   (lambda (x r lev len d? env p)
      (if (limit? lev)
          (display-string "..." p)
          (wr x r (decr lev) len d? env p))))

(define wrbox
   (lambda (x r lev len d? env p)
      (display-string "#&" p)
      (if (limit? lev)
          (display-string "..." p)
          (wr (unbox x) r (decr lev) len d? env p))))

(define wrpair
  (lambda (x r lev len d? env p)
    (define inheap-pair?
      (lambda (x)
        ; safe to do pair? check first here since pair is a primary type,
        ; hence pair? won't deference the possibly bogus foreign pointer
        (and (pair? x) ($object-in-heap? x))))
    (define abbreviations
      '((quote . "'") (quasiquote . "`") (unquote . ",") (unquote-splicing . ",@")
        (syntax . "#'") (quasisyntax . "#`") (unsyntax . "#,") (unsyntax-splicing . "#,@")))
    (cond
      [(and d? (inheap-pair? (cdr x)) (null? (cddr x)) (assq (car x) abbreviations)) =>
       (lambda (a)
         (display-string (cdr a) p)
         (wr (cadr x) r lev len d? env p))]
      [else
       (write-char #\( p)
       (if (limit? lev)
           (display-string "..." p)
           (let loop ([x x] [n len])
             (if (limit? n)
                 (display-string "..." p)
                 (begin
                   (wr (car x) r (decr lev) len d? env p)
                   (cond
                     [(and (inheap-pair? (cdr x))
                           (or d? (not env) (not (env 'tag? (cdr x)))))
                      (write-char #\space p)
                      (loop (cdr x) (decr n))]
                     [(null? (cdr x))]
                     [else
                      (display-string " . " p)
                      (wr (cdr x) r (decr lev) len d? env p)])))))
       (write-char #\) p)])))

(define wrvector
   (lambda (vlen vref prefix x r lev len d? env p)
      (let ([size (vlen x)] [pvl (and (not d?) (print-vector-length))])
         (write-char #\# p)
         (when pvl (wrfixits size 10 p))
         (when prefix (display-string prefix p))
         (write-char #\( p)   ;)
         (if (and (limit? lev) (not (fx= size 0)))
             (display-string "..." p)
             (unless (fx= size 0)
                (let ([last (if pvl
                                ($last-new-vector-element vlen vref x)
                                (fx- size 1))])
                   (let loop ([i 0] [n len])
                      (if (limit? n)
                          (display-string "..." p)
                          (begin (wr (vref x i) r (decr lev) len d? env p)
                                 (unless (fx= i last)
                                    (write-char #\space p)
                                    (loop (fx+ i 1) (decr n))))))))) ;(
         (write-char #\) p))))

(define wrport
   (lambda (x p)
      (let ([name (port-name x)])
         (display-string "#<" p)
         (when (binary-port? x) (display-string "binary " p))
         (if (input-port? x)
             (if (output-port? x)
                 (display-string "input/output port" p)
                 (display-string "input port" p))
             (if (output-port? x)
                 (display-string "output port" p)
                 (display-string "port" p)))
         (when (and (string? name) (not (eqv? name "")))
             (write-char #\space p)
             (display-string name p))
         (write-char #\> p))))

(define wrfixits
  (case-lambda
    [(n r p)
     (cond
       [(fx< n r) (write-char (digit->char n) p)]
       [else
        (wrfixits (fx/ n r) r p)
        (write-char (digit->char (fxremainder n r)) p)])]
    [(n r d p)
     (cond
       [(fx< n r)
        (do ([d d (fx- d 1)]) ((fx<= d 1)) (write-char #\0 p))
        (write-char (digit->char n) p)]
       [else
        (wrfixits (fx/ n r) r (fx- d 1) p)
        (write-char (digit->char (fxremainder n r)) p)])]))

(define wrfixits-negative
   ;; would be (wrfixits (fx- -n) r p) except that (- -n) may not be a
   ;; fixnum, e.g., for n = (most-negative-fixnum) in two's complement
   (lambda (-n r p)
      (cond
         [(fx> -n (fx- r)) (write-char (digit->char (fx- -n)) p)]
         [else
          (wrfixits-negative (fx/ -n r) r p)
          (write-char (digit->char (fx- (remainder -n r))) p)])))

(define wrbigits
  (let ()
   ; divide-and-conquer, treating bignum as two ``big base'' bigits
   ; first base must be >= sqrt(n); base i+1 must be >= sqrt(base i)
   ; last base must be <= most-positive-fixnum
    (define largest-fixnum-big-base
      (let ([v (make-vector 37)])
        (do ([b 2 (fx+ b 1)])
            ((fx= b 37) v)
          (vector-set! v b
            (let f ([bb b] [d 1])
              (let ([bb^2 (* bb bb)])
                (if (fixnum? bb^2)
                    (f bb^2 (* d 2))
                    (cons (cons bb d) '()))))))))
    (define (big-bases n r)
      (let ([iln/2 (bitwise-arithmetic-shift-right (+ (bitwise-length n) 1) 1)])
        (let f ([bb* (vector-ref largest-fixnum-big-base r)])
          (let ([bb (caar bb*)])
            (if (> (bitwise-length bb) iln/2)
                bb*
                (f (cons (cons (* bb bb) (* (cdar bb*) 2)) bb*)))))))
    (lambda (n r p)
      (let f ([n n] [d 0] [bb* (big-bases n r)])
        (cond
          [(fixnum? n) (wrfixits n r d p)]
          [(> (caar bb*) n) (f n d (cdr bb*))]
          [else
           (let ([hi.lo ($quotient-remainder n (caar bb*))])
             (f (car hi.lo) (- d (cdar bb*)) (cdr bb*))
             (f (cdr hi.lo) (cdar bb*) (cdr bb*)))])))))

(define wrradix
   (lambda (r p)
      (case r
         [(16) (display-string "#x" p)]
         [(8)  (display-string "#o" p)]
         [(2)  (display-string "#b" p)]
         [else
          (write-char #\# p)
          (wrfixits r 10 p)
          (write-char #\r p)])))

(define wrinexactnum
   (lambda (x r d? p)
      (let ([x1 ($inexactnum-real-part x)] [x2 ($inexactnum-imag-part x)])
         (wrflonum #f x1 r d? p)
         (wrflonum #t x2 r #t p)
         (write-char #\i p))))

(define wrexactnum
   (lambda (x r d? p)
      (let ([x1 ($exactnum-real-part x)] [x2 ($exactnum-imag-part x)])
         (wrhelp x1 r #f #f d? #f p)
         (unless (< x2 0) (write-char #\+ p))
         (wrhelp x2 r #f #f #t #f p)
         (write-char #\i p))))

(define wrratnum
   (lambda (x r d? p)
      (let ([n (numerator x)] [d (denominator x)])
         (if (fixnum? n) (wrfixnum n r d? p) (wrbignum n r d? p))
         (write-char #\/ p)
         (if (fixnum? d) (wrfixits d r p) (wrbigits d r p)))))

(define wrbignum
   (lambda (x r d? p)
      (unless (or (fx= r 10) d?) (wrradix r p))
      (wrbigits (if (< x 0) (begin (write-char #\- p) (- x)) x) r p)))

(define wrfixnum
   (lambda (x r d? p)
      (unless (or (fx= r 10) d?) (wrradix r p))
      (if (fx< x 0)
          (begin (write-char #\- p) (wrfixits-negative x r p))
          (wrfixits x r p))))

(define wrflonum
   (let ()
      (define flonum-digit->char
         (lambda (n)
            (string-ref "#00123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (fx+ n 2))))

      (define free-format
         (lambda (e s p)
            (when (fx< e 0)
               (write-char #\0 p)
               (write-char #\. p)
               (let loop ([e e])
                  (when (fx< e -1)
                     (write-char #\0 p)
                     (loop (fx+ e 1)))))
            (let loop ([u (car s)] [s (cdr s)] [e e])
               (write-char (flonum-digit->char u) p)
               (when (fx= e 0) (write-char #\. p))
               (let ([u (car s)])
                  (when (or (fx>= u 0) (fx>= e 0))
                     (loop u (cdr s) (fx- e 1)))))))

      (define free-format-exponential
         (lambda (e s r p)
            (write-char (flonum-digit->char (car s)) p)
            (let ([s (cdr s)])
               (unless (fx< (car s) 0) (write-char #\. p))
               (let loop ([s s])
                  (let ([u (car s)])
                     (unless (fx< u 0)
                        (write-char (flonum-digit->char u) p)
                        (loop (cdr s))))))
            (write-char #\e p)
            (wrfixnum e r #t p)))

      (define display-precision
        (lambda (m p)
          (write-char #\| p)
          (if (fixnum? m) (wrfixits m 10 p) (wrbigits m 10 p))))

      (lambda (force-sign x r d? p)
         ;;; R4RS & IEEE-1178 technically demand radix 10, but screw 'em
        (if (exceptional-flonum? x)
            (if ($nan? x)
                (display-string "+nan.0" p)
                (if (fl> x 0.0)
                    (display-string "+inf.0" p)
                    (display-string "-inf.0" p)))
            (let ([dx (decode-float x)])
              (unless (or (fx= r 10) d?) (wrradix r p))
              (let ([ls ($flonum->digits dx r 'normal 0)])
                (let ([s (car ls)] [e (cadr ls)] [ls (cddr ls)])
                   (if (fx< s 0)
                       (write-char #\- p)
                       (when force-sign (write-char #\+ p)))
                   (if (or (fx> r 10) (fx< -4 e 10))
                       (free-format e ls p)
                       (free-format-exponential e ls r p))))
              (cond
                [(print-precision) =>
                 (lambda (m)
                   (if (and (fixnum? m) (fx< m 53))
                       (display-precision (fxmax m (integer-length (vector-ref dx 0))) p)
                       (display-precision m p)))]
                [else
                 (let ([m (integer-length (vector-ref dx 0))])
                   (when (fx< 0 m 53) (display-precision m p)))]))))))

(define wrchar
  (lambda (x p)
    (define (r6rs-char-name x)
      (cond
        [(assq x '((#\nul . nul) (#\alarm . alarm) (#\backspace . backspace) (#\tab . tab)
                   (#\newline . newline) (#\vtab . vtab) (#\page . page) (#\return . return)
                   (#\esc . esc) (#\space . space) (#\delete . delete))) => cdr]
        [else #f]))
    (display-string "#\\" p)
    (cond
      [(if (print-char-name) (char-name x) (r6rs-char-name x)) =>
       (lambda (name)
         (display-string (symbol->string name) p))]
      [(or (char<=? #\x21 x #\x7e)
           (and (print-unicode)
                (char>=? x #\x80)
                (not (char=? x #\nel))
                (not (char=? x #\ls))))
       (write-char x p)]
      [else
       (write-char #\x p)
       (wrfixits (char->integer x) 16 p)])))

(define wrstring
  (lambda (x p)
    (write-char #\" p)
    (let ([n (string-length x)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i n))
        (let ([c (string-ref x i)])
          (cond
            [(memv c '(#\" #\\)) (write-char #\\ p) (write-char c p)]
            [(or (char<=? #\x20 c #\x7e)
                 (and (print-unicode)
                      (char>=? c #\x80)
                      (not (char=? c #\nel))
                      (not (char=? c #\ls))))
             (write-char c p)]
            [else
             (case c
               [(#\bel) (write-char #\\ p) (write-char #\a p)]
               [(#\backspace) (write-char #\\ p) (write-char #\b p)]
               [(#\newline) (write-char #\\ p) (write-char #\n p)]
               [(#\page) (write-char #\\ p) (write-char #\f p)]
               [(#\return) (write-char #\\ p) (write-char #\r p)]
               [(#\tab) (write-char #\\ p) (write-char #\t p)]
               [(#\vt) (write-char #\\ p) (write-char #\v p)]
               [else
                (write-char #\\ p)
                (write-char #\x p)
                (wrfixits (char->integer c) 16 p)
                (write-char #\; p)])]))))
    (write-char #\" p)))

(module (wrsymbol)
 ; this is like the one used in read, but has no eof clause
  (define-syntax state-case
    (lambda (x)
      (define state-case-test
        (lambda (cvar k)
          (with-syntax ((cvar cvar))
            (syntax-case k (-)
              (char
               (char? (datum char))
               #'(char=? cvar char))
              ((char1 - char2)
               (and (char? (datum char1)) (char? (datum char2)))
               #'(char<=? char1 cvar char2))
              (predicate
               (identifier? #'predicate)
               #'(predicate cvar))))))
      (define state-case-help
        (lambda (cvar clauses)
          (syntax-case clauses (else)
            (((else exp1 exp2 ...))
             #'(begin exp1 exp2 ...))
            ((((k ...) exp1 exp2 ...) . more)
             (with-syntax (((test ...)
                            (map (lambda (k) (state-case-test cvar k))
                                 #'(k ...)))
                           (rest (state-case-help cvar #'more)))
               #'(if (or test ...) (begin exp1 exp2 ...) rest)))
            (((k exp1 exp2 ...) . more)
             (with-syntax ((test (state-case-test cvar #'k))
                           (rest (state-case-help cvar #'more)))
               #'(if test (begin exp1 exp2 ...) rest))))))
      (syntax-case x ()
        ((_ cvar more ...)
         (identifier? #'cvar)
         (state-case-help #'cvar #'(more ...))))))

  (define (print-hex-char c p)
    (write-char #\\ p)
    (write-char #\x p)
    (wrfixits (char->integer c) 16 p)
    (write-char #\; p))

  (define (s0 s p n)
    (let ([c (string-ref s 0)])
      (state-case c
        [((#\a - #\z) (#\A - #\Z) #\* #\= #\< #\> #\/ #\! #\$ #\% #\& #\: #\? #\^ #\_ #\~)
         (write-char c p)]
        [(#\.)
         (if (and (fx= n 3) (char=? (string-ref s 1) #\.) (char=? (string-ref s 2) #\.))
             (write-char #\. p)
             (print-hex-char c p))]
        [(#\-)
         (if (or (fx= n 1) (char=? (string-ref s 1) #\>))
             (write-char #\- p)
             (print-hex-char c p))]
        [(#\+) (if (fx= n 1) (write-char #\+ p) (print-hex-char c p))]
        [$constituent? (if (print-unicode) (write-char c p) (print-hex-char c p))]
        [else (print-hex-char c p)]))
    (s1 s p n 1))

  (define (s1 s p n i)
    (unless (fx= i n)
      (let ([c (string-ref s i)])
        (state-case c
          [((#\a - #\z) (#\A - #\Z) #\- #\? (#\0 - #\9) #\* #\! #\= #\> #\< #\$ #\% #\& #\/ #\: #\^ #\_ #\~ #\+ #\. #\@)
           (write-char c p)]
          [$constituent? (if (print-unicode) (write-char c p) (print-hex-char c p))]
          [$subsequent? (if (print-unicode) (write-char c p) (print-hex-char c p))]
          [else (print-hex-char c p)]))
      (s1 s p n (fx+ i 1))))

  (define extended-identifier?
    (let ()
      (define-syntax state-machine
        (lambda (x)
          (syntax-case x ()
            ((_k start-state (name def (test e) ...) ...)
             (with-implicit (_k s i n) ; support num4 kludge
               #'(let ()
                   (define name
                     (lambda (s i n)
                       (if (= i n)
                           def
                           (let ([g (string-ref s i)])
                             (state-machine-help (s i n) g (test e) ... ))))) ...
                   (lambda (string)
                     (start-state string 0 (string-length string)))))))))
      (define-syntax state-machine-help
        (syntax-rules (else to skip)
          [(_ (s i n) c [test (skip to x)] more ...)
           (state-machine-help (s i n) c [test (x s i n)] more ...)]
          [(_ (s i n) c [test (to x)] more ...)
           (state-machine-help (s i n) c [test (x s (fx+ i 1) n)] more ...)]
          [(_ (s i n) c [else e]) e]
          [(_ (s i n) c [test e] more ...)
           (if (state-machine-test c test)
               e
               (state-machine-help (s i n)c more ...))]))
      (define-syntax state-machine-test
        (syntax-rules (-)
          [(_ c (char1 - char2))
           (char<=? char1 c char2)]
          [(_ c (e1 e2 ...))
           (or (state-machine-test c e1) (state-machine-test c e2) ...)]
          [(_ c char)
           (char=? c char)]))
      (state-machine start
        (start #f          ; start state
          [((#\a - #\z) (#\A - #\Z)) (to sym)]
          [(#\- #\+) (to num1)]
          [(#\* #\= #\> #\<) (to sym)]
          [(#\0 - #\9) (to num4)]
          [#\. (to num2)]
          [(#\{ #\}) (to brace)]
          [else (skip to sym)])
        (num1 #t           ; seen + or -
          [(#\0 - #\9) (to num4)]
          [#\. (to num3)]
          [(#\i #\I) (to num5)]
          [else (skip to sym)])
        (num2 #f           ; seen .
          [(#\0 - #\9) (to num4)]
          [else (skip to sym)])
        (num3 #f           ; seen +. or -.
          [(#\0 - #\9) (to num4)]
          [else (skip to sym)])
        (num4 #f           ; seen digit, +digit, -digit, or .digit
          [else ; kludge
            (if (number? ($str->num s n 10 #f #f)) ; grabbing private s and n
                #f
                (sym s i n))])             ; really (skip to sym)
        (num5 #f           ; bars: seen +i, -i, +I, or -I
          [else (skip to sym)])
        (sym #t            ; safe symbol
          [((#\a - #\z) (#\A - #\Z) #\- #\? (#\0 - #\9) #\* #\! #\= #\> #\< #\+ #\/)
           (to sym)]
          [((#\nul - #\space) #\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\; #\" #\\ #\|)
           #f]
          [else (to sym)])
        (brace #t          ; { or }
          [else #f]))))

  (define wrsymbol
    (case-lambda
      [(s p) (wrsymbol s p #f)]
      [(s p tail?)
       (let ([n (string-length s)])
         (if tail?
             (s1 s p n 0)
             (if (fx= n 0)
                 (display-string "||" p)
                 (if (and (print-extended-identifiers) (extended-identifier? s))
                     (display-string s p)
                     (s0 s p n)))))])))

(set! $write-pretty-quick
   (lambda (x lev len env p)
      (wrhelp x (print-radix) lev len #f env p)))

(let ()
  (define (do-write x p who)
    (when (port-closed? p)
      ($oops who "not permitted on closed port ~s" p))
    (let ([lev (print-level)] [len (print-length)])
       (let ([env ($make-graph-env who x lev len)])
          (wr x (print-radix) lev len #f env p))))

  (set-who! write
    (case-lambda
      [(x p)
       (unless (and (output-port? p) (textual-port? p))
         ($oops 'write "~s is not a textual output port" p))
       (do-write x p who)]
      [(x) (do-write x (current-output-port) who)]))

  (set-who! put-datum
    (lambda (p x)
      (unless (and (output-port? p) (textual-port? p))
        ($oops who "~s is not a textual output port" p))
      (do-write x p who))))

(let ()
  (define (do-display x p who)
    (when (port-closed? p)
      ($oops who "not permitted on closed port ~s" p))
    (wr x (print-radix) #f #f #t #f p))

  (set-who! display
    (case-lambda
      [(x p)
       (unless (and (output-port? p) (textual-port? p))
         ($oops 'display "~s is not a textual output port" p))
       (do-display x p who)]
      [(x) (do-display x (current-output-port) who)])))

) ;let

(define print-gensym
  ($make-thread-parameter
    #t
    (lambda (x) (if (memq x '(pretty pretty/suffix)) x (and x #t)))))

(define print-record ($make-thread-parameter #t (lambda (x) (and x #t))))

(define print-graph ($make-thread-parameter #f (lambda (x) (and x #t))))

(define print-level
   ($make-thread-parameter
      #f
      (lambda (x)
         (unless (or (not x) (and (fixnum? x) (fx>= x 0)))
            ($oops 'print-level "~s is not a nonnegative fixnum or #f" x))
         x)))

(define print-length
   ($make-thread-parameter
      #f
      (lambda (x)
         (unless (or (not x) (and (fixnum? x) (fx>= x 0)))
            ($oops 'print-length "~s is not a nonnegative fixnum or #f" x))
         x)))

(define print-radix
   ($make-thread-parameter
      10
      (lambda (x)
         (unless (and (fixnum? x) (fx<= 2 x 36))
            ($oops 'print-radix "~s is not between 2 and 36" x))
         x)))

(define print-brackets ($make-thread-parameter #t (lambda (x) (and x #t))))

(define print-unicode ($make-thread-parameter #t (lambda (x) (and x #t))))

(define print-char-name ($make-thread-parameter #f (lambda (x) (and x #t))))

(define print-vector-length ($make-thread-parameter #f (lambda (x) (and x #t))))

(define print-extended-identifiers ($make-thread-parameter #f (lambda (x) (and x #t))))

(define print-precision
  ($make-thread-parameter
    #f
    (lambda (x)
      (unless (or (not x) (and (fixnum? x) (fx> x 0)) (and (bignum? x) ($bigpositive? x)))
        ($oops 'print-precision "~s is not a positive exact integer or #f" x))
      x)))
)
