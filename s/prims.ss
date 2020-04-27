;;; prims.ss
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

(eval-when (compile)
   (optimize-level 3)
   (run-cp0 (default-run-cp0))
   (generate-interrupt-trap #f))

(begin
;;; hand-coded primitives

(define-who $hand-coded
  (lambda (name)
    ($oops who
      "must invoke with quoted name and compile at optimize level 2 or 3")))

(define list ($hand-coded 'list-procedure))
(define $record ($hand-coded '$record-procedure))
(define vector ($hand-coded 'vector-procedure))
(define cons* ($hand-coded 'cons*-procedure))
(define list* ($hand-coded 'list*-procedure))
(define $apply ($hand-coded '$apply-procedure))

;;; basic C-coded primitives

(define $oops ; boot-time error procedure
  (foreign-procedure "(cs)boot-error"
    (scheme-object scheme-object scheme-object)
    void))
(define errorf $oops)
(define assertion-violationf $oops)

(define $lookup-library-entry
  (foreign-procedure "(cs)lookup_library_entry"
    (fixnum boolean)
    scheme-object))

(define $flonum->fixnum
  (foreign-procedure "(cs)s_fltofx"
    (scheme-object)
    scheme-object))

; must be compiled w/generate-interrupt-trap #f for library eq-hashtable operations
(define weak-cons
  (foreign-procedure "(cs)s_weak_cons"
    (scheme-object scheme-object)
    scheme-object))

(define weak-pair?
  (foreign-procedure "(cs)s_weak_pairp"
    (scheme-object)
    scheme-object))

(define ephemeron-cons
  (foreign-procedure "(cs)s_ephemeron_cons"
    (scheme-object scheme-object)
    scheme-object))

(define ephemeron-pair?
  (foreign-procedure "(cs)s_ephemeron_pairp"
    (scheme-object)
    scheme-object))

(define $split-continuation
  (foreign-procedure "(cs)single_continuation"
    (scheme-object iptr)
    scheme-object))

(define $set-collect-trip-bytes
  (foreign-procedure "(cs)s_set_collect_trip_bytes"
    (scheme-object)
    scheme-object))

(define oblist
  (foreign-procedure "(cs)s_oblist"
    ()
    scheme-object))

(define $dequeue-scheme-signals
  (foreign-procedure "(cs)dequeue_scheme_signals"
    (ptr)
    ptr))

(define-who $show-allocation
  (let ([fp (foreign-procedure "(cs)s_showalloc" (boolean string) void)])
    (case-lambda
      [() (fp #f #f)]
      [(x) (fp x #f)]
      [(x s)
       (unless (string? s) ($oops who "~s is not a string" s))
       (fp x s)])))

(define-who $enable-check-heap
  (let ([get (foreign-procedure "(cs)check_heap_enabledp" () boolean)]
        [set (foreign-procedure "(cs)enable_check_heap" (boolean) void)])
    (case-lambda
      [() (get)]
      [(x) (set x)])))

(define-who $check-heap-errors
  (foreign-procedure "(cs)check_heap_errors"
    ()
    uptr))

(define-who system
  (let ([p (foreign-procedure "(cs)s_system" (string) scheme-object)])
    (lambda (x)
      (unless (string? x) ($oops who "~s is not a string" x))
      (p x))))

(define $set-code-byte!
  (foreign-procedure "(cs)s_set_code_byte"
    (scheme-object scheme-object scheme-object)
    scheme-object))

(define $set-code-word!
  (foreign-procedure "(cs)s_set_code_word"
    (scheme-object scheme-object scheme-object)
    scheme-object))

(define $set-code-long!
  (foreign-procedure "(cs)s_set_code_long"
    (scheme-object scheme-object scheme-object)
    scheme-object))

(define $set-code-quad!
  (foreign-procedure "(cs)s_set_code_quad"
    (scheme-object scheme-object scheme-object)
    scheme-object))

(define $flush-instruction-cache
  (foreign-procedure "(cs)s_flush_instruction_cache"
    ()
    scheme-object))

(define $make-closure
  (foreign-procedure "(cs)s_make_closure"
    (scheme-object scheme-object)
    scheme-object))

(define string->symbol
  (foreign-procedure "(cs)s_intern"
    (scheme-object)
    scheme-object))

(define $intern2
  (foreign-procedure "(cs)s_intern2"
    (scheme-object scheme-object)
    scheme-object))

(define $intern3
  (foreign-procedure "(cs)s_intern3"
    (scheme-object scheme-object scheme-object)
    scheme-object))

(define $intern-gensym
  (foreign-procedure "(cs)s_intern_gensym"
    (scheme-object)
    void))

(define cpu-time
  (foreign-procedure "(cs)cputime"
    ()
    scheme-object))

(define real-time
  (foreign-procedure "(cs)realtime"
    ()
    scheme-object))

(define $fixed-path?
  (foreign-procedure "(cs)fixedpathp"
    (string)
    boolean))

(define getenv
  (let ([getenv (foreign-procedure "(cs)getenv" (string) scheme-object)])
    (lambda (s)
      (unless (string? s) ($oops 'getenv "~s is not a string" s))
      (getenv s))))

(define putenv
  (let ([putenv (foreign-procedure "(cs)putenv" (string string) void)])
    (lambda (s1 s2)
      (unless (string? s1) ($oops 'putenv "~s is not a string" s1))
      (unless (string? s2) ($oops 'putenv "~s is not a string" s2))
      (putenv s1 s2))))

(define decode-float
  (foreign-procedure "(cs)s_decode_float"
    (scheme-object)
    scheme-object))

(define $fx*
  (foreign-procedure "(cs)fxmul" (fixnum fixnum)
    fixnum))

(define $fx/
  (foreign-procedure "(cs)fxdiv" (fixnum fixnum)
    fixnum))

(define $procedure-name
  (lambda (x)
    (unless (procedure? x)
      ($oops '$procedure-name "~s is not a procedure" x))
    ($code-name ($closure-code x))))

(define-who procedure-arity-mask
  (lambda (x)
    (unless (procedure? x) ($oops who "~s is not a procedure" x))
    ($code-arity-mask ($closure-code x))))

(let ()
  (define-syntax frob-proc
    (syntax-rules ()
      [(_ frob make-frob frob-set! elt? elt-msg)
       (let ([elt-error
              (lambda args
                 (do ([args args (cdr args)])
                     ((null? args))
                   (unless (elt? (car args))
                     ($oops 'frob elt-msg (car args)))))])
          (define-syntax frob-clause
            (syntax-rules ()
              ((_ c (... ...))
               (if (and (elt? c) (... ...))
                   (frob c (... ...))
                   (elt-error c (... ...))))))
          (case-lambda
             [() (frob-clause)]
             [(c1) (frob-clause c1)]
             [(c1 c2)
              (frob-clause c1 c2)]
             [(c1 c2 c3)
              (frob-clause c1 c2 c3)]
             [(c1 c2 c3 c4)
              (frob-clause c1 c2 c3 c4)]
             [(c1 c2 c3 c4 c5)
              (frob-clause c1 c2 c3 c4 c5)]
             [(c1 c2 c3 c4 c5 c6)
              (frob-clause c1 c2 c3 c4 c5 c6)]
             [l (let ([s (make-frob (length l))])
                   (do ([ls l (cdr ls)] [i 0 (fx+ i 1)])
                       ((null? ls) s)
                       (let ([c (car ls)])
                          (unless (elt? c) (elt-error c))
                          (frob-set! s i c))))]))]))


  (set! string (frob-proc string make-string string-set! char?
                  "~s is not a character"))
  (set! fxvector (frob-proc fxvector make-fxvector fxvector-set! fixnum?
                  "~s is not a fixnum"))
  (set! bytevector
    (let ([fill? (lambda (k) (and (fixnum? k) (fx<= -128 k 255)))])
      (frob-proc bytevector make-bytevector $bytevector-set! fill?
                    "invalid value ~s")))
)

(define-who (bytevector-truncate! bv n)
  (unless (mutable-bytevector? bv) ($oops who "~s is not a mutable bytevector" bv))
  (unless (and (fixnum? n) (not ($fxu< (bytevector-length bv) n)))
    ($oops who "invalid new length ~s for ~s" n bv))
  (bytevector-truncate! bv n))

(define-who (string-truncate! st n)
  (unless (mutable-string? st) ($oops who "~s is not a mutable string" st))
  (unless (and (fixnum? n) (not ($fxu< (string-length st) n)))
    ($oops who "invalid new length ~s for ~s" n st))
  (string-truncate! st n))

(define-who make-string
  (case-lambda
    [(n c)
     (unless (and (fixnum? n) (not ($fxu< (constant maximum-string-length) n)))
       ($oops who "~s is not a valid string length" n))
     (unless (char? c)
       ($oops who "~s is not a character" c))
     (make-string n c)]
    [(n)
     (unless (and (fixnum? n) (not ($fxu< (constant maximum-string-length) n)))
       ($oops who "~s is not a valid string length" n))
     (make-string n)]))

(define-who make-vector
   (case-lambda
      [(n x)
       (unless (and (fixnum? n) (not ($fxu< (constant maximum-vector-length) n)))
         ($oops who "~s is not a valid vector length" n))
       (make-vector n x)]
      [(n)
       (unless (and (fixnum? n) (not ($fxu< (constant maximum-vector-length) n)))
         ($oops who "~s is not a valid vector length" n))
       (make-vector n)]))

(define $make-eqhash-vector
  (case-lambda
    [(n)
     (unless (and (fixnum? n) (not ($fxu< (constant maximum-vector-length) n)))
       ($oops '$make-eqhash-vector "~s is not a nonnegative fixnum" n))
     ($make-eqhash-vector n)]))

(define-who make-fxvector
  (case-lambda
    [(n x)
     (unless (and (fixnum? n) (not ($fxu< (constant maximum-fxvector-length) n)))
       ($oops who "~s is not a valid fxvector length" n))
     (unless (fixnum? x)
       ($oops who "~s is not a fixnum" x))
     (make-fxvector n x)]
    [(n)
     (unless (and (fixnum? n) (not ($fxu< (constant maximum-fxvector-length) n)))
       ($oops who "~s is not a valid fxvector length" n))
     (make-fxvector n)]))

(define string-fill!
   (lambda (s c)
      (unless (mutable-string? s)
         ($oops 'string-fill! "~s is not a mutable string" s))
      (unless (char? c)
         ($oops 'string-fill! "~s is not a character" c))
      (string-fill! s c)))

(define fxvector-fill!
   (lambda (v n)
      (unless (mutable-fxvector? v)
         ($oops 'fxvector-fill! "~s is not a mutable fxvector" v))
      (unless (fixnum? n)
         ($oops 'fxvector-fill! "~s is not a fixnum" n))
      (fxvector-fill! v n)))

;;; multiple return values stuff

(define values ($hand-coded 'values-procedure))

(define call-with-values
  (lambda (producer consumer)
    (unless (procedure? producer)
      ($oops 'call-with-values "~s is not a procedure" producer))
    (unless (procedure? consumer)
      ($oops 'call-with-values "~s is not a procedure" consumer))
    (call-with-values producer consumer)))

(define call/1cc
  (lambda (p)
    (unless (procedure? p)
      ($oops 'call/1cc "~s is not a procedure" p))
    (#3%call/1cc p)))

(define call/cc
  (lambda (p)
    (unless (procedure? p)
      ($oops 'call/cc "~s is not a procedure" p))
    (#3%call/cc p)))

(define-who call-with-current-continuation
  (lambda (p)
    (unless (procedure? p)
      ($oops who "~s is not a procedure" p))
    (#3%call/cc p)))

(define $code? (lambda (x) ($code? x)))

(define $system-code? (lambda (x) ($system-code? x)))

(define $system-procedure?
   (lambda (x)
      (and (procedure? x) ($system-code? ($closure-code x)))))

(define-who $make-code-object
  (foreign-procedure "(cs)s_make_code"
     (iptr iptr ptr ptr iptr ptr ptr)
     ptr))

(define-who $code-name
  (lambda (x)
    (unless ($code? x) ($oops who "~s is not code" x))
    ($code-name x)))

(define-who $code-arity-mask
  (lambda (x)
    (unless ($code? x) ($oops who "~s is not code" x))
    ($code-arity-mask x)))

(define-who $code-free-count
  (lambda (x)
    (unless ($code? x) ($oops who "~s is not code" x))
    ($code-free-count x)))

(define-who $code-info
  (lambda (x)
    (unless ($code? x) ($oops who "~s is not code" x))
    ($code-info x)))

(define-who $code-pinfo*
  (lambda (x)
    (unless ($code? x) ($oops who "~s is not code" x))
    ($code-pinfo* x)))

(define $object-address ; not safe and can't be
  (lambda (x offset)
    ($object-address x offset)))

(define $address->object ; also not safe and can't be
  (lambda (x offset)
    ;; could do address-in-heap? checking here if we passed a who.
    ($address->object x offset)))

(define foreign-callable-entry-point
  (lambda (x)
    (unless ($code? x)
      ($oops 'foreign-callable-entry-point "~s is not a code object" x))
    ($object-address x (constant code-data-disp))))

(define-who foreign-callable-code-object
  (lambda (x)
    (unless (and (integer? x) (exact? x) ($address-in-heap? x))
      ($oops who "~s is not an entry point" x))
    ($address->object x (constant code-data-disp))))

(define $closure-code
   (lambda (x)
      (unless (procedure? x)
         ($oops '$closure-code "~s is not a closure" x))
      ($closure-code x)))

(define $closure-length
   (lambda (x)
      (unless (procedure? x)
         ($oops '$closure-length "~s is not a closure" x))
      ($code-free-count ($closure-code x))))

(define $closure-ref
   (lambda (x i)
      (unless (procedure? x)
         ($oops '$closure-ref "~s is not a closure" x))
      (unless (and (fixnum? i) (fx< -1 i ($closure-length x)))
         ($oops '$closure-ref "invalid index ~s" i))
      ($closure-ref x i)))

(define $continuation? (lambda (x) ($continuation? x)))

(define $continuation-stack-length
   (lambda (x)
      (unless ($continuation? x)
         ($oops '$continuation-stack-length "~s is not a continuation" x))
      ($continuation-stack-length x)))

(define $continuation-stack-clength
   (lambda (x)
      (unless ($continuation? x)
         ($oops '$continuation-stack-clength "~s is not a continuation" x))
      ($continuation-stack-clength x)))

(define $continuation-stack-ref
   (lambda (x i)
      (unless ($continuation? x)
         ($oops '$continuation-stack-ref "~s is not a continuation" x))
      (unless (and (fixnum? i) (fx< 0 i ($continuation-stack-clength x)))
         ($oops '$continuation-stack-ref "invalid index ~s" i))
      ($continuation-stack-ref x i)))

(define $continuation-link
   (lambda (x)
      (unless ($continuation? x)
         ($oops '$continuation-link "~s is not a continuation" x))
      ($continuation-link x)))

(define $continuation-winders
   (lambda (x)
      (unless ($continuation? x)
         ($oops '$continuation-winders "~s is not a continuation" x))
      ($continuation-winders x)))

(define $continuation-return-code
   (lambda (x)
      (unless ($continuation? x)
         ($oops '$continuation-return-code "~s is not a continuation" x))
      ($continuation-return-code x)))

(define $continuation-return-livemask
   (lambda (x)
      (unless ($continuation? x)
         ($oops '$continuation-return-livemask "~s is not a continuation" x))
      ($continuation-return-livemask x)))

(define $continuation-return-offset
   (lambda (x)
      (unless ($continuation? x)
         ($oops '$continuation-return-offset "~s is not a continuation" x))
      ($continuation-return-offset x)))

(define void
   (lambda ()
      (void)))

(define eof-object (lambda () (eof-object)))

(define $unbound-object
   (lambda ()
      ($unbound-object)))

(define $unbound-object?
   (lambda (x)
      ($unbound-object? x)))

(define $set-timer
   (lambda (ticks)
      (unless (and (fixnum? ticks) (fx> ticks 0))
         ($oops '$set-timer "~s is not a positive fixnum" ticks))
      ($set-timer ticks)))

(define $fx+?
   (lambda (x y)
      ($fx+? x y)))

(define $fx-?
   (lambda (x y)
      ($fx-? x y)))

(define $bigpositive?
   (lambda (x)
      (unless (bignum? x)
         ($oops '$bigpositive "~s is not a bignum" x))
      ($bigpositive? x)))

(define $string-ref-check? (lambda (s i) ($string-ref-check? s i)))
(define $string-set!-check? (lambda (s i) ($string-set!-check? s i)))

(define $vector-ref-check? (lambda (v i) ($vector-ref-check? v i)))
(define $vector-set!-check? (lambda (v i) ($vector-set!-check? v i)))

(define $fxvector-ref-check? (lambda (v i) ($fxvector-ref-check? v i)))
(define $fxvector-set!-check? (lambda (v i) ($fxvector-set!-check? v i)))

(define $ratio-numerator
   (lambda (q)
      (if (ratnum? q)
          ($ratio-numerator q)
          ($oops '$ratio-numerator "~s is not a ratnum" q))))

(define $ratio-denominator
   (lambda (q)
      (if (ratnum? q)
          ($ratio-denominator q)
          ($oops '$ratio-denominator "~s is not a ratnum" q))))

(let ()
  (include "io-types.ss")
  (set-who! $make-binary-output-port
    (rec $make-binary-output-port
      (case-lambda
        [(name handler buffer) ($make-binary-output-port name handler buffer #f)]
        [(name handler buffer info)
         (unless (string? name) ($oops who "invalid name ~s" name))
         (unless (port-handler? handler) ($oops who "invalid handler ~s" handler))
         (unless (bytevector? buffer) ($oops who "invalid buffer ~s" buffer))
         (#3%$make-binary-output-port name handler buffer info)])))

  (set-who! $make-binary-input-port
    (rec $make-binary-input-port
      (case-lambda
        [(name handler buffer) ($make-binary-input-port name handler buffer #f)]
        [(name handler buffer info)
         (unless (string? name) ($oops who "invalid name ~s" name))
         (unless (port-handler? handler) ($oops who "invalid handler ~s" handler))
         (unless (bytevector? buffer) ($oops who "invalid buffer ~s" buffer))
         (#3%$make-binary-input-port name handler buffer info)])))

  (set-who! $make-binary-input/output-port
    (rec $make-binary-input/output-port
      (case-lambda
        [(name handler ibuffer obuffer) ($make-binary-input/output-port name handler ibuffer obuffer #f)]
        [(name handler ibuffer obuffer info)
         (unless (string? name) ($oops who "invalid name ~s" name))
         (unless (port-handler? handler) ($oops who "invalid handler ~s" handler))
         (unless (bytevector? ibuffer) ($oops who "invalid input buffer ~s" ibuffer))
         (unless (bytevector? obuffer) ($oops who "invalid output buffer ~s" obuffer))
         (#3%$make-binary-input/output-port name handler ibuffer obuffer info)])))

  (set-who! $make-textual-output-port
    (rec $make-textual-output-port
      (case-lambda
        [(name handler buffer) ($make-textual-output-port name handler buffer #f)]
        [(name handler buffer info)
         (unless (string? name) ($oops who "invalid name ~s" name))
         (unless (port-handler? handler) ($oops who "invalid handler ~s" handler))
         (unless (string? buffer)
           ($oops who "invalid buffer ~s" buffer))
         (#3%$make-textual-output-port name handler buffer info)])))

  (set-who! $make-textual-input-port
    (rec $make-textual-input-port
      (case-lambda
        [(name handler buffer) ($make-textual-input-port name handler buffer #f)]
        [(name handler buffer info)
         (unless (string? name) ($oops who "invalid name ~s" name))
         (unless (port-handler? handler) ($oops who "invalid handler ~s" handler))
         (unless (string? buffer) ($oops who "invalid buffer ~s" buffer))
         (#3%$make-textual-input-port name handler buffer info)])))

  (set-who! $make-textual-input/output-port
    (rec $make-textual-input/output-port
      (case-lambda
        [(name handler ibuffer obuffer) ($make-textual-input/output-port name handler ibuffer obuffer #f)]
        [(name handler ibuffer obuffer info)
         (unless (string? name) ($oops who "invalid name ~s" name))
         (unless (port-handler? handler) ($oops who "invalid handler ~s" handler))
         (unless (string? ibuffer) ($oops who "invalid input buffer ~s" ibuffer))
         (unless (string? obuffer) ($oops who "invalid output buffer ~s" obuffer))
         (#3%$make-textual-input/output-port name handler ibuffer obuffer info)]))))

(define-who $port-flags-set?
  (lambda (p mask)
    (unless (port? p) ($oops who "~s is not a port" p))
    (unless (fixnum? mask) ($oops who "invalid mask ~s" mask))
    ($port-flags-set? p mask)))

(define-who $set-port-flags!
  (lambda (p mask)
    (unless (port? p) ($oops who "~s is not a port" p))
    (unless (fixnum? mask) ($oops who "invalid mask ~s" mask))
    ($set-port-flags! p mask)))

(define-who $reset-port-flags!
  (lambda (p mask)
    (unless (port? p) ($oops who "~s is not a port" p))
    (unless (fixnum? mask) ($oops who "invalid mask ~s" mask))
    ($reset-port-flags! p mask)))

(define port-closed?
   (lambda (p)
      (if (port? p)
          (port-closed? p)
          ($oops 'port-closed? "~s is not a port" p))))

(define mark-port-closed!
   (lambda (p)
      (if (port? p)
          (mark-port-closed! p)
          ($oops 'mark-port-closed! "~s is not a port" p))))

(define-who $port-handler
  (lambda (p)
    (unless (port? p)
      ($oops who "~s is not a port" p))
    ($port-handler p)))

(define-who set-port-bol!
  (lambda (p x)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (set-port-bol! p x)))

(define-who set-port-eof!
  (lambda (p x)
    (unless (input-port? p) ($oops who "~s is not an input port" p))
    (set-port-eof! p x)))

(define-who $port-info
  (lambda (p)
    (unless (port? p) ($oops who "~s is not a port" p))
    ($port-info p)))

(define-who $set-port-info!
  (lambda (p x)
    (unless (port? p) ($oops who "~s is not a port" p))
    ($set-port-info! p x)))

(define-who port-name
  (lambda (p)
    (unless (port? p) ($oops who "~s is not a port" p))
    (port-name p)))

(define-who set-port-name!
  (lambda (p x)
    (unless (port? p) ($oops who "~s is not a port" p))
    (set-port-name! p x)))

(let ()
 ; defines port-input-index, port-output-index, port-input-size, port-output-size, port-input-buffer, and port-output-buffer
  (define-syntax get/set
    (lambda (x)
      (syntax-case x ()
        [(_ direction what bpred? tpred?)
         (with-syntax ([port-dir-x (construct-name #'what "port-" #'direction "-" #'what)]
                       [set-port-dir-x! (construct-name #'what "set-port-" #'direction "-" #'what "!")]
                       [textual-port-dir-x (construct-name #'what "textual-port-" #'direction "-" #'what)]
                       [set-textual-port-dir-x! (construct-name #'what "set-textual-port-" #'direction "-" #'what "!")]
                       [binary-port-dir-x (construct-name #'what "binary-port-" #'direction "-" #'what)]
                       [set-binary-port-dir-x! (construct-name #'what "set-binary-port-" #'direction "-" #'what "!")]
                       [dir-port? (construct-name #'direction #'direction "-port?")])
           #`(begin
               (set-who! port-dir-x
                 (lambda (p)
                   (cond
                     [(and (dir-port? p) (textual-port? p)) (textual-port-dir-x p)]
                     [(and (dir-port? p) (binary-port? p)) (binary-port-dir-x p)]
                     [else ($oops who #,(format "~~s is not an ~a port" (datum direction)) p)])))
               (set-who! set-port-dir-x!
                 (lambda (p x)
                   (cond
                     [(and (dir-port? p) (textual-port? p))
                      (unless (tpred? p x) ($oops who #,(format "invalid ~a ~~s" (datum what)) x))
                      (set-textual-port-dir-x! p x)]
                     [(and (dir-port? p) (binary-port? p))
                      (unless (bpred? p x) ($oops who #,(format "invalid ~a ~~s" (datum what)) x))
                      (set-binary-port-dir-x! p x)]
                     [else ($oops who #,(format "~~s is not an ~a port" (datum direction)) p)])))))])))

  (get/set input buffer (lambda (p x) (bytevector? x)) (lambda (p x) (string? x)))
  (get/set input index
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (binary-port-input-size p))))
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (textual-port-input-size p)))))
  (get/set input size
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (bytevector-length (binary-port-input-buffer p)))))
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (string-length (textual-port-input-buffer p))))))
  (get/set output buffer (lambda (p x) (bytevector? x)) (lambda (p x) (string? x)))
  (get/set output index
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (binary-port-output-size p))))
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (textual-port-output-size p)))))
  (get/set output size
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (bytevector-length (binary-port-output-buffer p)))))
    (lambda (p n) (and (fixnum? n) (fx<= 0 n (string-length (textual-port-output-buffer p)))))))

(define-who port-input-count
  (lambda (p)
    (cond
      [(and (input-port? p) (textual-port? p))
       (textual-port-input-count p)]
      [(and (input-port? p) (binary-port? p))
       (binary-port-input-count p)]
      [else ($oops who "~s is not an input port" p)])))

(define-who port-output-count
  (lambda (p)
    (cond
      [(and (output-port? p) (textual-port? p))
       (textual-port-output-count p)]
      [(and (output-port? p) (binary-port? p))
       (binary-port-output-count p)]
      [else ($oops who "~s is not an output port" p)])))

(define-who textual-port-input-buffer
  (lambda (p)
    (unless (and (input-port? p) (textual-port? p))
      ($oops who "~s is not a textual input port" p))
    (textual-port-input-buffer p)))

(define-who set-textual-port-input-buffer!
  (lambda (p s)
    (unless (and (input-port? p) (textual-port? p))
      ($oops who "~s is not a textual input port" p))
    (unless (string? s) ($oops who "~s is not a string" s))
    (set-textual-port-input-buffer! p s)))

(define-who textual-port-input-index
  (lambda (p)
    (unless (and (input-port? p) (textual-port? p))
      ($oops who "~s is not a textual input port" p))
    (textual-port-input-index p)))

(define-who set-textual-port-input-index!
  (lambda (p n)
    (unless (and (input-port? p) (textual-port? p))
      ($oops who "~s is not a textual input port" p))
    (unless (and (fixnum? n) (fx<= 0 n (textual-port-input-size p)))
      ($oops who "~s is not a valid index for ~s" n p))
    (set-textual-port-input-index! p n)))

(define-who textual-port-input-size
  (lambda (p)
    (unless (and (input-port? p) (textual-port? p))
      ($oops who "~s is not a textual input port" p))
    (textual-port-input-size p)))

(define-who set-textual-port-input-size!
  (lambda (p n)
    (unless (and (input-port? p) (textual-port? p))
      ($oops who "~s is not a textual input port" p))
    (unless (and (fixnum? n) (fx<= 0 n (string-length (textual-port-input-buffer p))))
       ($oops who "~s is not a valid size for ~s" n p))
    (set-textual-port-input-size! p n)))

(define-who textual-port-output-buffer
  (lambda (p)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (textual-port-output-buffer p)))

(define-who set-textual-port-output-buffer!
  (lambda (p s)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (unless (string? s) ($oops who "~s is not a string" s))
    (set-textual-port-output-buffer! p s)))

(define-who textual-port-output-index
  (lambda (p)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (textual-port-output-index p)))

(define-who set-textual-port-output-index!
  (lambda (p n)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (unless (and (fixnum? n) (fx<= 0 n (textual-port-output-size p)))
       ($oops who "~s is not a valid index for ~s" n p))
    (set-textual-port-output-index! p n)))

(define-who textual-port-output-size
  (lambda (p)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (textual-port-output-size p)))

(define-who set-textual-port-output-size!
  (lambda (p n)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (unless (and (fixnum? n) (fx<= 0 n (string-length (textual-port-output-buffer p))))
       ($oops who "~s is not a valid size for ~s" n p))
    (set-textual-port-output-size! p n)))

(define-who binary-port-input-buffer
  (lambda (p)
    (unless (and (input-port? p) (binary-port? p))
      ($oops who "~s is not a binary input port" p))
    (binary-port-input-buffer p)))

(define-who set-binary-port-input-buffer!
  (lambda (p bv)
    (unless (and (input-port? p) (binary-port? p))
      ($oops who "~s is not a binary input port" p))
    (unless (bytevector? bv) ($oops who "~s is not a bytevector" bv))
    (set-binary-port-input-buffer! p bv)))

(define-who binary-port-input-index
  (lambda (p)
    (unless (and (input-port? p) (binary-port? p))
      ($oops who "~s is not a binary input port" p))
    (binary-port-input-index p)))

(define-who set-binary-port-input-index!
  (lambda (p n)
    (unless (and (input-port? p) (binary-port? p))
      ($oops who "~s is not a binary input port" p))
    (unless (and (fixnum? n) (fx<= 0 n (binary-port-input-size p)))
      ($oops who "~s is not a valid index for ~s" n p))
    (set-binary-port-input-index! p n)))

(define-who binary-port-input-size
  (lambda (p)
    (unless (and (input-port? p) (binary-port? p))
      ($oops who "~s is not a binary input port" p))
    (binary-port-input-size p)))

(define-who set-binary-port-input-size!
  (lambda (p n)
    (unless (and (input-port? p) (binary-port? p))
      ($oops who "~s is not a binary input port" p))
    (unless (and (fixnum? n) (fx<= 0 n (bytevector-length (binary-port-input-buffer p))))
      ($oops who "~s is not a valid size for ~s" n p))
    (set-binary-port-input-size! p n)))

(define-who binary-port-input-count
  (lambda (p)
    (unless (and (input-port? p) (binary-port? p))
      ($oops who "~s is not a binary input port" p))
    (binary-port-input-count p)))

(define-who binary-port-output-count
  (lambda (p)
    (unless (and (output-port? p) (binary-port? p))
      ($oops who "~s is not a binary output port" p))
    (binary-port-output-count p)))

(define-who textual-port-input-count
  (lambda (p)
    (unless (and (input-port? p) (textual-port? p))
      ($oops who "~s is not a textual input port" p))
    (textual-port-input-count p)))

(define-who textual-port-output-count
  (lambda (p)
    (unless (and (output-port? p) (textual-port? p))
      ($oops who "~s is not a textual output port" p))
    (textual-port-output-count p)))

(define-who port-input-empty?
  (lambda (p)
    (unless (input-port? p) ($oops who "~s is not an input port" p))
    (#3%port-input-empty? p)))

(define-who port-output-full?
  (lambda (p)
    (unless (output-port? p) ($oops who "~s is not an output port" p))
    (#3%port-output-full? p)))

(define-who binary-port-output-buffer
  (lambda (p)
    (unless (and (output-port? p) (binary-port? p))
      ($oops who "~s is not a binary output port" p))
    (binary-port-output-buffer p)))

(define-who set-binary-port-output-buffer!
  (lambda (p bv)
    (unless (and (output-port? p) (binary-port? p))
      ($oops who "~s is not a binary output port" p))
    (unless (bytevector? bv) ($oops who "~s is not a bytevector" bv))
    (set-binary-port-output-buffer! p bv)))

(define-who binary-port-output-index
  (lambda (p)
    (unless (and (output-port? p) (binary-port? p))
      ($oops who "~s is not a binary output port" p))
    (binary-port-output-index p)))

(define-who set-binary-port-output-index!
  (lambda (p n)
    (unless (and (output-port? p) (binary-port? p))
      ($oops who "~s is not a binary output port" p))
    (unless (and (fixnum? n) (fx<= 0 n (binary-port-output-size p)))
      ($oops who "~s is not a valid index for ~s" n p))
    (set-binary-port-output-index! p n)))

(define-who binary-port-output-size
  (lambda (p)
    (unless (and (output-port? p) (binary-port? p))
      ($oops who "~s is not a binary output port" p))
    (binary-port-output-size p)))

(define-who set-binary-port-output-size!
  (lambda (p n)
    (unless (and (output-port? p) (binary-port? p))
      ($oops who "~s is not a binary output port" p))
    (unless (and (fixnum? n) (fx<= 0 n (bytevector-length (binary-port-output-buffer p))))
      ($oops who "~s is not a valid size for ~s" n p))
    (set-binary-port-output-size! p n)))

(define $symbol-name
   (lambda (s)
      (if (symbol? s)
          ($symbol-name s)
          ($oops '$symbol-name "~s is not a symbol" s))))

(define $set-symbol-name!
   (lambda (s l)
      (if (symbol? s)
          ($set-symbol-name! s l)
          ($oops '$set-symbol-name! "~s is not a symbol" s))))

(define $symbol-property-list
   (lambda (s)
      (if (symbol? s)
          ($symbol-property-list s)
          ($oops '$symbol-property-list "~s is not a symbol" s))))

(define $set-symbol-property-list!
   (lambda (s l)
      (if (symbol? s)
          ($set-symbol-property-list! s l)
          ($oops '$set-symbol-property-list! "~s is not a symbol" s))))

(define $system-property-list
   (lambda (s)
      (if (symbol? s)
          ($system-property-list s)
          ($oops '$system-property-list "~s is not a symbol" s))))

(define $set-system-property-list!
   (lambda (s l)
      (if (symbol? s)
          ($set-system-property-list! s l)
          ($oops '$set-system-property-list! "~s is not a symbol" s))))

(define-who $symbol-hash
  (lambda (x)
    (unless (symbol? x) ($oops who "~s is not a symbol" x))
    (#3%$symbol-hash x)))

(define-who $set-symbol-hash!
  (lambda (x y)
    (unless (symbol? x) ($oops who "~s is not a symbol" x))
    (#3%$set-symbol-hash! x y)))

(define symbol->string
  (lambda (s)
    (unless (symbol? s)
      ($oops 'symbol->string "~s is not a symbol" s))
    (#3%symbol->string s)))

(define $top-level-value
  (lambda (s)
    (#2%$top-level-value s)))

(define $set-top-level-value!
  (lambda (s v)
    (if (symbol? s)
        ($set-top-level-value! s v)
        ($oops '$set-top-level-value! "~s is not a symbol" s))))

(define $top-level-bound?
  (lambda (s)
    (if (symbol? s)
        ($top-level-bound? s)
        ($oops '$top-level-bound? "~s is not a symbol" s))))

(define-who $bignum-length
  (lambda (n)
    (unless (bignum? n) ($oops who "~s is not a bignum" n))
    (#3%$bignum-length n)))

(define string-length
   (lambda (s)
      (#2%string-length s)))

(define string-ref
   (lambda (s i)
      (#2%string-ref s i)))

(define string-set!
   ; could use #2%string-set!
   (lambda (v i x)
      (#2%string-set! v i x)))

(define-who $string-set-immutable!
   (lambda (s)
     (unless (string? s)
       ($oops who "~s is not a string" s))
     (#3%$string-set-immutable! s)))

(define-who mutable-string?
  (lambda (v)
    (#3%mutable-string? v)))

(define-who immutable-string?
  (lambda (v)
    (#3%immutable-string? v)))

(define char->integer
   (lambda (x)
      (#2%char->integer x)))

(define-who integer->char
  (lambda (n)
    (if (and (fixnum? n)
             (or ($fxu< n #xD800)
                 (fx<= #xE000 n #x10FFFF)))
        (#3%integer->char n)
        ($oops 'integer->char "~s is not a valid unicode scalar value" n))))

(define vector-length
   (lambda (v)
      (#2%vector-length v)))

(define vector-ref
   (lambda (v i)
      (#2%vector-ref v i)))

(define vector-set!
   (lambda (v i x)
      (#2%vector-set! v i x)))

(define vector-cas!
   (lambda (v i old-x new-x)
      (#2%vector-cas! v i old-x new-x)))

(define vector-set-fixnum!
  (lambda (v i x)
    (#2%vector-set-fixnum! v i x)))

(define-who $vector-set-immutable!
   (lambda (v)
     (unless (vector? v)
       ($oops who "~s is not a vector" v))
     (#3%$vector-set-immutable! v)))

(define mutable-vector?
   (lambda (v)
     (#3%mutable-vector? v)))

(define immutable-vector?
   (lambda (v)
     (#3%immutable-vector? v)))

(define fxvector-length
   (lambda (v)
      (#2%fxvector-length v)))

(define fxvector-ref
   (lambda (v i)
      (#2%fxvector-ref v i)))

(define fxvector-set!
   (lambda (v i x)
      (#2%fxvector-set! v i x)))

(define-who $fxvector-set-immutable!
   (lambda (s)
     (unless (fxvector? s)
       ($oops who "~s is not a fxvector" s))
     (#3%$fxvector-set-immutable! s)))

(define mutable-fxvector?
  (lambda (s)
    (#3%mutable-fxvector? s)))

(define immutable-fxvector?
  (lambda (s)
    (#3%immutable-fxvector? s)))

(define cons (lambda (x y) (cons x y)))

(define car
   (lambda (p)
      (#2%car p)))

(define cdr
   (lambda (p)
      (#2%cdr p)))

(define set-car!
   (lambda (p v)
      (#2%set-car! p v)))

(define set-cdr!
   (lambda (p v)
      (#2%set-cdr! p v)))

(define box (lambda (x) (box x)))

(define box-immutable (lambda (x) (box-immutable x)))

(define unbox
   (lambda (b)
      (if (box? b)
          (unbox b)
          ($oops 'unbox "~s is not a box" b))))

(define set-box!
   (lambda (b v)
      (if (mutable-box? b)
          (set-box! b v)
          ($oops 'set-box! "~s is not a mutable box" b))))

(define-who box-cas!
   (lambda (b old-v new-v)
      (if (mutable-box? b)
          (box-cas! b old-v new-v)
          ($oops who "~s is not a mutable box" b))))

(define mutable-box?
  (lambda (b)
    (#3%mutable-box? b)))

(define immutable-box?
  (lambda (b) 
    (#3%immutable-box? b)))

(define pair? (lambda (x) (pair? x)))

(define box? (lambda (x) (box? x)))

(define symbol? (lambda (x) (symbol? x)))

(define gensym? (lambda (x) (gensym? x)))

(define fixnum? (lambda (x) (fixnum? x)))

(define bignum? (lambda (x) (bignum? x)))

(define ratnum? (lambda (x) (ratnum? x)))

(define string? (lambda (x) (string? x)))

(define vector? (lambda (x) (vector? x)))

(define fxvector? (lambda (x) (fxvector? x)))

(define procedure? (lambda (x) (procedure? x)))

(define flonum? (lambda (x) (flonum? x)))

(define char? (lambda (x) (char? x)))

(define eof-object? (lambda (x) (eof-object? x)))

(define bwp-object? (lambda (x) (bwp-object? x)))

(define port? (lambda (p) (port? p)))

(define input-port? (lambda (p) (input-port? p)))

(define output-port? (lambda (p) (output-port? p)))

(define-who port-eof?
  (lambda (input-port)
    (unless (input-port? input-port)
      ($oops who "~s is not an input port" input-port))
    (port-eof? input-port)))

(define-who binary-port?
  (lambda (p)
    (unless (port? p) ($oops who "~s is not a port" p))
    (#3%binary-port? p)))

(define-who textual-port?
  (lambda (p)
    (unless (port? p) ($oops who "~s is not a port" p))
    (#3%textual-port? p)))

(define eq?
   (lambda (x y)
      (eq? x y)))

(define char-
   (lambda (x y)
      (unless (char? x)
         ($oops 'char- "~s is not a character" x))
      (unless (char? y)
         ($oops 'char- "~s is not a character" y))
      (char- x y)))

(define most-positive-fixnum (lambda () (#2%most-positive-fixnum)))
(define most-negative-fixnum (lambda () (#2%most-negative-fixnum)))
(define greatest-fixnum (lambda () (#2%greatest-fixnum)))
(define least-fixnum (lambda () (#2%least-fixnum)))
(define fixnum-width (lambda () (#2%fixnum-width)))

(define-who (get-u8 p)
  (#2%get-u8 p))

(define-who (get-char p)
  (#2%get-char p))

(define-who (lookahead-u8 p)
  (#2%lookahead-u8 p))

(define-who (lookahead-char p)
  (#2%lookahead-char p))

(define-who unget-u8
  (lambda (binary-input-port octet)
    (unless (and (input-port? binary-input-port) (binary-port? binary-input-port))
      ($oops who "~s is not a binary input port" binary-input-port))
    (unless (or (and (fixnum? octet) (fx<= 0 octet 255)) (eof-object? octet))
      ($oops who "~s is not an octet or the eof object" octet))
    (#3%unget-u8 binary-input-port octet)))

(define-who unget-char
  (lambda (textual-input-port char)
    (unless (and (input-port? textual-input-port) (textual-port? textual-input-port))
      ($oops who "~s is not a textual input port" textual-input-port))
    (unless (or (char? char) (eof-object? char))
      ($oops who "~s is not a character or the eof object" char))
    (#3%unget-char textual-input-port char)))

(define-who (put-u8 binary-output-port octet)
  (unless (and (output-port? binary-output-port) (binary-port? binary-output-port))
    ($oops who "~s is not a binary output port" binary-output-port))
  (unless (and (fixnum? octet) (fx<= 0 octet 255))
    ($oops who "~s is not an octet" octet))
  (#3%put-u8 binary-output-port octet))

(define-who (put-char textual-output-port char)
  (unless (and (output-port? textual-output-port) (textual-port? textual-output-port))
    ($oops who "~s is not a textual output port" textual-output-port))
  (unless (char? char) ($oops who "~s is not a char" char))
  (#3%put-char textual-output-port char))

(define peek-char
  (case-lambda
    [(p) (#2%peek-char p)]
    [() (#3%peek-char)]))

(define read-char
  (case-lambda
    [(p) (#2%read-char p)]
    [() (#3%read-char)]))

(define unread-char
  (case-lambda
    [(x p)
     (unless (and (input-port? p) (textual-port? p))
       ($oops 'unread-char "~s is not a textual input port" p))
     (unless (char? x)
       ($oops 'unread-char "~s is not a character" x))
     (unread-char x p)]
    [(x)
     (unless (char? x)
       ($oops 'unread-char "~s is not a character" x))
     (unread-char x)]))

(define write-char
  (case-lambda
    [(x p)
     (unless (and (output-port? p) (textual-port? p))
       ($oops 'write-char "~s is not a textual output port" p))
     (unless (char? x)
       ($oops 'write-char "~s is not a character" x))
     (write-char x p)]
    [(x)
     (unless (char? x)
       ($oops 'write-char "~s is not a character" x))
     (write-char x)]))

(define newline
  (case-lambda
    [(p)
     (unless (and (output-port? p) (textual-port? p))
       ($oops 'newline "~s is not a textual output port" p))
     (newline p)]
    [() (newline)]))

(define display-string
  (case-lambda
    [(s p)
     (unless (and (output-port? p) (textual-port? p))
       ($oops 'display-string "~s is not a textual output port" p))
     (unless (string? s)
       ($oops 'display-string "~s is not a character" s))
     (display-string s p)]
    [(s)
     (unless (string? s)
       ($oops 'display-string "~s is not a character" s))
     (display-string s)]))

(define $immediate? (lambda (x) ($immediate? x)))
(define $inexactnum? (lambda (x) ($inexactnum? x)))

(define $inexactnum-real-part
   (lambda (x)
      (unless ($inexactnum? x)
         ($oops '$inexactnum-real-part "~s is not an inexactnum" x))
      ($inexactnum-real-part x)))

(define $inexactnum-imag-part
   (lambda (x)
      (unless ($inexactnum? x)
         ($oops '$inexactnum-imag-part "~s is not an inexactnum" x))
      ($inexactnum-imag-part x)))

(define $exactnum? (lambda (x) ($exactnum? x)))

(define $exactnum-real-part
   (lambda (x)
      (unless ($exactnum? x)
         ($oops '$exactnum-real-part "~s is not an exactnum" x))
      ($exactnum-real-part x)))

(define $exactnum-imag-part
   (lambda (x)
      (unless ($exactnum? x)
         ($oops '$exactnum-imag-part "~s is not an exactnum" x))
      ($exactnum-imag-part x)))

(define $make-exactnum
   (lambda (x y)
      (unless (or (fixnum? x) (bignum? x) (ratnum? x))
         ($oops '$make-exactnum "~s is not an exact real number" x))
      (unless (or (and (fixnum? y) (not (fx= y 0))) (bignum? y) (ratnum? y))
         ($oops '$make-exactnum "~s is not an nonzero exact real number" y))
      ($make-exactnum x y)))

(define $rtd-counts? (lambda (x) ($rtd-counts? x)))

(define getprop
   (case-lambda
      [(s p)
       (if (symbol? s)
           (getprop s p)
           ($oops 'getprop "~s is not a symbol" s))]
      [(s p d)
       (if (symbol? s)
           (getprop s p d)
           ($oops 'getprop "~s is not a symbol" s))]))

(define $current-stack-link
  (case-lambda
    [() ($current-stack-link)]
    [(k)
     (unless (or ($continuation? k) (zero? k))
       ($oops '$current-stack-link "invalid argument ~s" k))
     ($current-stack-link k)]))

(define $current-winders
  (case-lambda
    [() ($current-winders)]
    [(w)
     (unless (and (list? w) (andmap (lambda (x) (winder? x)) w))
       ($oops '$current-winders "malformed winders ~s" w))
     ($current-winders w)]))

(define lock-object
  (foreign-procedure "(cs)lock_object" (scheme-object) void))
(define unlock-object
  (foreign-procedure "(cs)unlock_object" (scheme-object) void))
(define locked-object?
  (foreign-procedure "(cs)locked_objectp" (scheme-object) boolean))

(define-who $install-guardian
  (lambda (obj rep tconc)
    ; tconc is assumed to be valid at all call sites
    (#3%$install-guardian obj rep tconc)))

(define-who $install-ftype-guardian
  (lambda (obj tconc)
    ; tconc is assumed to be valid at all call sites
    (#3%$install-ftype-guardian obj tconc)))

(define guardian?
  (lambda (g)
    (#3%guardian? g)))

(define-who unregister-guardian
  (let ([fp (foreign-procedure "(cs)unregister_guardian" (scheme-object) scheme-object)])
    (define probable-tconc? ; full tconc? could be expensive ...
      (lambda (x)
        (and (pair? x) (pair? (car x)) (pair? (cdr x)))))
    (lambda (g)
      (unless (guardian? g) ($oops who "~s is not a guardian" g))
      ; at present, guardians should have either one free variable (the tcond) or two(the tconc and an ftd)
      ; but we just look for a probable tconc among whatever free variables it has
      (fp (let ([n ($code-free-count ($closure-code g))])
            (let loop ([i 0])
              (if (fx= i n)
                  ($oops #f "failed to find a tconc among the free variables of guardian ~s" g)
                  (let ([x ($closure-ref g i)])
                    (if (probable-tconc? x)
                        x
                        (loop (fx+ i 1)))))))))))

(define-who $ftype-guardian-oops
  (lambda (ftd obj)
    ($oops 'ftype-guardian "~s is not an ftype pointer of the expected type ~s" obj ftd)))

(define make-guardian (lambda () (#2%make-guardian)))

(define $make-ftype-guardian (lambda (ftd) (#2%$make-ftype-guardian ftd)))

(define $address-in-heap?
  (foreign-procedure "(cs)s_addr_in_heap" (uptr) boolean))

(define $object-in-heap?
  (foreign-procedure "(cs)s_ptr_in_heap" (ptr) boolean))

(define $event (lambda () ($event)))

(define $tc (lambda () ($tc)))
(define $thread-list (lambda () ($thread-list)))

(define $tc-field
  (let ()
    (define bogus
      (lambda (fld)
        ($oops '$tc-field "invalid tc field ~s" fld)))
    (define-syntax alpha
      (with-syntax ([(param ...)
                     (fold-left
                       (lambda (ls field)
                         (apply
                           (lambda (name type disp len)
                             (if (eq? type 'ptr)
                                 (cons (datum->syntax #'* name) ls)
                                 ls))
                           field))
                       '() (getprop 'tc '*fields* '()))])
        (lambda (x)
          #'(case-lambda
              [(fld tc)
               (case fld
                 [(param) ($tc-field 'param tc)]
                 ...
                 [else (bogus fld)])]
              [(fld tc v)
               (case fld
                 [(param) ($tc-field 'param tc v)]
                 ...
                 [else (bogus fld)])]))))
    alpha))

(define virtual-register-count (lambda () (#2%virtual-register-count)))
(define virtual-register (lambda (idx) (#2%virtual-register idx)))
(define set-virtual-register! (lambda (idx val) (#2%set-virtual-register! idx val)))

(define thread? (lambda (x) (thread? x)))
(define $thread-tc
  (lambda (thread)
    (unless (thread? thread)
      ($oops '$thread-tc "~s is not a thread" thread))
    ($thread-tc thread)))

(when-feature pthreads

(define $raw-collect-cond (lambda () ($raw-collect-cond)))
(define $raw-tc-mutex (lambda () ($raw-tc-mutex)))
(define fork-thread)
(define make-mutex)
(define mutex?)
(define mutex-name)
(define mutex-acquire)
(define mutex-release)
(define make-condition)
(define thread-condition?)
(define condition-name)
(define condition-wait)
(define condition-signal)
(define condition-broadcast)
(define $close-resurrected-mutexes&conditions)
(define $tc-mutex)
(define $collect-cond)
(let ()
; scheme-object's below are mutex and condition addresses, which are
; assumed to be at least ptr aligned and therefore look like fixnums
(define ft (foreign-procedure "(cs)fork_thread" (scheme-object)
              scheme-object))
(define mm (foreign-procedure "(cs)make_mutex" () scheme-object))
(define mf (foreign-procedure "(cs)mutex_free" (scheme-object) void))
(define ma (foreign-procedure "(cs)mutex_acquire" (scheme-object) void))
(define ma-nb (foreign-procedure "(cs)mutex_acquire_noblock" (scheme-object)
                  scheme-object))
(define mr (foreign-procedure "(cs)mutex_release" (scheme-object) void))
(define mc (foreign-procedure "(cs)make_condition" () scheme-object))
(define cf (foreign-procedure "(cs)condition_free" (scheme-object) void))
(define cw (foreign-procedure "(cs)condition_wait" (scheme-object scheme-object scheme-object) boolean))
(define cb (foreign-procedure "(cs)condition_broadcast" (scheme-object) void))
(define cs (foreign-procedure "(cs)condition_signal" (scheme-object) void))

(define-record-type (condition $make-condition $condition?)
  (fields (mutable addr $condition-addr $condition-addr-set!)
          (immutable name $condition-name))
  (nongenerative)
  (sealed #t))

(define-record-type (mutex $make-mutex $mutex?)
  (fields (mutable addr $mutex-addr $mutex-addr-set!)
          (immutable name $mutex-name))
  (nongenerative)
  (sealed #t))

(define make-mutex-no-check
  (lambda (name)
    (let ([m ($make-mutex (mm) name)])
      (mutex-guardian m)
      m)))

(define make-condition-no-check
  (lambda (name)
    (let ([c ($make-condition (mc) name)])
      (condition-guardian c)
      c)))

(define mutex-guardian (make-guardian))
(define condition-guardian (make-guardian))

(set! fork-thread
  (lambda (t)
    (unless (procedure? t)
      ($oops 'fork-thread "~s is not a procedure" t))
    (ft (lambda ()
          (call/cc
            (lambda (k)
              (parameterize ([abort-handler
                              (case-lambda [() (k -1)] [(x) (k x)])]
                             [exit-handler
                              (case-lambda [() (k (void))] [(x . args) (k x)])]
                             [reset-handler (lambda () (k (void)))])
                (t)
                (void))))))))

(set-who! make-mutex
  (case-lambda
    [() (make-mutex-no-check #f)]
    [(name)
     (unless (or (not name) (symbol? name)) ($oops who "~s is not a symbol or #f" name))
     (make-mutex-no-check name)]))

(set! mutex?
  (lambda (x)
    ($mutex? x)))

(set-who! mutex-name
  (lambda (m)
    (unless (mutex? m) ($oops who "~s is not a mutex" m))
    ($mutex-name m)))

(set! mutex-acquire
  (case-lambda
    [(m) (mutex-acquire m #t)]
    [(m block?)
     (unless (mutex? m)
       ($oops 'mutex-acquire "~s is not a mutex" m))
     (let ([addr ($mutex-addr m)])
       (when (eq? addr 0)
         ($oops 'mutex-acquire "mutex is defunct"))
       (let ([r ((if block? ma ma-nb) addr)])
         ($keep-live m)
         r))]))

(set! mutex-release
  (lambda (m)
    (unless (mutex? m)
      ($oops 'mutex-release "~s is not a mutex" m))
    (let ([addr ($mutex-addr m)])
      (when (eq? addr 0)
        ($oops 'mutex-release "mutex is defunct"))
      (mr addr))))

(set-who! make-condition
  (case-lambda
    [() (make-condition-no-check #f)]
    [(name)
     (unless (or (not name) (symbol? name)) ($oops who "~s is not a symbol or #f" name))
     (make-condition-no-check name)]))

(set! thread-condition?
  (lambda (x)
    ($condition? x)))

(set-who! condition-name
  (lambda (c)
    (unless (thread-condition? c) ($oops who "~s is not a condition" c))
    ($condition-name c)))

(set! condition-wait
  (case-lambda
   [(c m) (condition-wait c m #f)]
   [(c m t)
    (unless (thread-condition? c)
      ($oops 'condition-wait "~s is not a condition" c))
    (unless (mutex? m)
      ($oops 'condition-wait "~s is not a mutex" m))
    (unless (or (not t)
		(and (time? t) (memq (time-type t) '(time-duration time-utc))))
      ($oops 'condition-wait "~s is not a time record of type time-duration or time-utc" t))
    (let ([caddr ($condition-addr c)] [maddr ($mutex-addr m)])
      (when (eq? caddr 0)
        ($oops 'condition-wait "condition is defunct"))
      (when (eq? maddr 0)
        ($oops 'condition-wait "mutex is defunct"))
      (let ([r (cw caddr maddr t)])
        ($keep-live c)
        ($keep-live m)
        r))]))

(set! condition-broadcast
  (lambda (c)
    (unless (thread-condition? c)
      ($oops 'condition-broadcast "~s is not a condition" c))
    (let ([addr ($condition-addr c)])
      (when (eq? addr 0)
        ($oops 'condition-broadcast "condition is defunct"))
      (cb addr))))

(set! condition-signal
  (lambda (c)
    (unless (thread-condition? c)
      ($oops 'condition-signal "~s is not a condition" c))
    (let ([addr ($condition-addr c)])
      (when (eq? addr 0)
        ($oops 'condition-signal "condition is defunct"))
      (cs addr))))

(set! $close-resurrected-mutexes&conditions
  ; called from single-threaded docollect
  (lambda ()
    (let f ()
      (let mg ([m (mutex-guardian)])
        (when m
          (let ([addr ($mutex-addr m)])
	    (unless (eq? addr 0)
	      (mf addr)
              ($mutex-addr-set! m 0)))
          (f))))
    (let f ()
      (let cg ([c (condition-guardian)])
        (when c
	  (let ([addr ($condition-addr c)])
	    (unless (eq? addr 0)
	      (cf addr)
              ($condition-addr-set! c 0)))
          (f))))))

(set! $tc-mutex ($make-mutex ($raw-tc-mutex) '$tc-mutex))
(set! $collect-cond ($make-condition ($raw-collect-cond) '$collect-cond))
))

(let ()
  (define-syntax define-tc-parameter
    (lambda (x)
      (syntax-case x ()
        [(_ name pred type init)
         #'(begin (define-tc-parameter name pred type) (name init))]
        [(_ name pred type)
         (with-syntax ([msg (format "~~s is not ~a" (datum type))])
           #'(set! name
               (case-lambda
                 [() (name)]
                 [(x)
                  (unless (pred x) ($oops 'name msg x))
                  (name x)])))])))
  (define-tc-parameter current-input-port (lambda (x) (and (input-port? x) (textual-port? x))) "a textual input port")
  (define-tc-parameter current-output-port (lambda (x) (and (output-port? x) (textual-port? x))) "a textual output port")
  (define-tc-parameter current-error-port (lambda (x) (and (output-port? x) (textual-port? x))) "a textual output port")
  (define-tc-parameter $block-counter (lambda (x) (and (fixnum? x) (fx<= x 0))) "a nonpositive fixnum" 0)
  (define-tc-parameter $sfd (lambda (x) (or (eq? x #f) (source-file-descriptor? x))) "a source-file descriptor or #f" #f)
  (define-tc-parameter $current-mso (lambda (x) (or (eq? x #f) (procedure? x))) "a procedure or #f" #f)
  (define-tc-parameter $target-machine symbol? "a symbol")
  (define-tc-parameter optimize-level (lambda (x) (and (fixnum? x) (fx<= 0 x 3))) "a valid optimize level" 0)
  (define-tc-parameter $compile-profile (lambda (x) (memq x '(#f source block))) "a valid compile-profile flag" #f)
  (define-tc-parameter subset-mode (lambda (mode) (memq mode '(#f system))) "a valid subset mode" #f)
  (define-tc-parameter default-record-equal-procedure (lambda (x) (or (eq? x #f) (procedure? x))) "a procedure or #f" #f)
  (define-tc-parameter default-record-hash-procedure (lambda (x) (or (eq? x #f) (procedure? x))) "a procedure or #f" #f)
)

(define-who compile-profile
  ; this wrapper is used to filter #t => source
  (case-lambda
    [() ($compile-profile)]
    [(x)
     ($compile-profile
       (case x
         [(#f source block) x]
         [(#t) 'source]
         [else ($oops who "invalid mode ~s [must be #f, #t, source, or block]" x)]))]))

(let ()
  (define-syntax define-boolean-tc-parameter
    (lambda (x)
      (syntax-case x ()
        [(_ name init)
         #'(begin
             (set! name
               (case-lambda
                 [() (name)]
                 [(x) (name (and x #t))]))
             (name init))])))
  (define-boolean-tc-parameter generate-inspector-information #t)
  (define-boolean-tc-parameter generate-procedure-source-information #f)
  (define-boolean-tc-parameter generate-profile-forms #t)
  (define-boolean-tc-parameter $suppress-primitive-inlining #f)
)

(define $make-tlc
  (lambda (ht keyval next)
    (unless (eq-hashtable? ht)
      ($oops '$make-tlc "~s is not an eq hashtable" ht))
    (unless (or ($tlc? next) (fixnum? next))
      ($oops '$make-tlc "invalid next argument ~s" next))
    (unless (pair? keyval)
      ($oops '$make-tlc "invalid keyval argument ~s" keyval))
    (#3%$make-tlc ht keyval next)))

(define $tlc? (lambda (x) (#3%$tlc? x)))

(let ()
  (define-syntax define-tlc-parameter
    (syntax-rules ()
      [(_ name)
       (set! name
         (lambda (tlc)
           (unless ($tlc? tlc) ($oops 'name "~s is not a tlc" tlc))
           (#3%name tlc)))]
      [(_ name name!)
       (begin
         (define-tlc-parameter name)
         (set! name!
           (lambda (tlc x)
             (unless ($tlc? tlc) ($oops 'name "~s is not a tlc" tlc))
             (#3%name! tlc x))))]))
  (define-tlc-parameter $tlc-keyval)
  (define-tlc-parameter $tlc-ht)
  (define-tlc-parameter $tlc-next $set-tlc-next!)
)

(define ($fxaddress x) (#3%$fxaddress x))

(define $logand
  (foreign-procedure "(cs)logand"
    (scheme-object scheme-object)
    scheme-object))

(define $logor
  (foreign-procedure "(cs)logor"
    (scheme-object scheme-object)
    scheme-object))

(define $logxor
  (foreign-procedure "(cs)logxor"
    (scheme-object scheme-object)
    scheme-object))

(define $lognot
  (foreign-procedure "(cs)lognot"
    (scheme-object)
    scheme-object))

(define $logbit?
  (foreign-procedure "(cs)logbitp"
    (scheme-object scheme-object)
    scheme-object))

(define $logbit0
  (foreign-procedure "(cs)logbit0"
    (scheme-object scheme-object)
    scheme-object))

(define $logbit1
  (foreign-procedure "(cs)logbit1"
    (scheme-object scheme-object)
    scheme-object))

(define $logtest
  (foreign-procedure "(cs)logtest"
    (scheme-object scheme-object)
    scheme-object))

(when-feature windows
(define get-registry
  (let ([fp (foreign-procedure "(windows)GetRegistry"
              (wstring)
              scheme-object)])
    (lambda (s)
      (unless (string? s) ($oops 'get-registry "~s is not a string" s))
      (let ([x (fp s)])
        (and x (utf16->string x (constant native-endianness)))))))

(define put-registry!
  (let ([fp (foreign-procedure "(windows)PutRegistry"
              (wstring wstring)
              void)])
    (lambda (s1 s2)
      (unless (string? s1) ($oops 'put-registry! "~s is not a string" s1))
      (unless (string? s2) ($oops 'put-registry! "~s is not a string" s2))
      (fp s1 s2))))

(define remove-registry!
  (let ([fp (foreign-procedure "(windows)RemoveRegistry"
              (wstring)
              void)])
    (lambda (s)
      (unless (string? s) ($oops 'remove-registry! "~s is not a string" s))
      (fp s))))
)

(define ($real->flonum x who)
  (unless (or (not who) (symbol? who) (string? who))
    ($oops '$real->flonum "invalid who argument ~s" who))
  (#3%$real->flonum x who))

(define (real->flonum x) (#2%real->flonum x))

(define $integer-8? (lambda (x) (#3%$integer-8? x)))
(define $integer-16? (lambda (x) (#3%$integer-16? x)))
(define $integer-24? (lambda (x) (#3%$integer-24? x)))
(define $integer-32? (lambda (x) (#3%$integer-32? x)))
(define $integer-40? (lambda (x) (#3%$integer-40? x)))
(define $integer-48? (lambda (x) (#3%$integer-48? x)))
(define $integer-56? (lambda (x) (#3%$integer-56? x)))
(define $integer-64? (lambda (x) (#3%$integer-64? x)))
(define $foreign-char? (lambda (x) (#3%$foreign-char? x)))
(define $foreign-wchar? (lambda (x) (#3%$foreign-wchar? x)))

(define $byte-copy!
  (foreign-procedure "(cs)byte-copy"
    (scheme-object fixnum scheme-object fixnum fixnum)
    void))

(define $ptr-copy!
  (foreign-procedure "(cs)ptr-copy"
    (scheme-object fixnum scheme-object fixnum fixnum)
    void))

(define-who ($sealed-record? x rtd)
  (unless (record-type-descriptor? rtd)
    ($oops who "~s is not a record type descriptor" rtd))
  (#3%$sealed-record? x rtd))

(define ($record? x) (#3%$record? x))

(define-who ($record-type-descriptor r)
  (unless ($record? r) ($oops who "~s is not a record" r))
  (#3%$record-type-descriptor r))

(define-who utf8->string
  (let ()
    (define slurp
      (lambda (bv start)
        (let ([n (bytevector-length bv)])
          (let ([s (make-string (fx- n start))])
            (define (fini j)
              (if (fx= j (string-length s))
                  s
                  (string-truncate! s j)))
            (let loop ([i start] [j 0])
              (if (fx= i n)
                  (fini j)
                  (let ([b1 (bytevector-u8-ref bv i)])
                    (cond
                      [(fx<= b1 #x7f) ; one-byte encoding
                       (string-set! s j (integer->char b1))
                       (loop (fx+ i 1) (fx+ j 1))]
                      [(fx<= #xc0 b1 #xdf) ; two-byte encoding
                       (if (fx< i (fx- n 1)) ; have at least two bytes?
                           (let ([b2 (bytevector-u8-ref bv (fx+ i 1))])
                             (if (fx= (fxsrl b2 6) #b10) ; second byte a continuation byte?
                                 (begin
                                   (string-set! s j
                                     (let ([x (fxlogor (fxsll (fxlogand b1 #b11111) 6) (fxlogand b2 #b111111))])
                                       (if (fx<= x #x7f) #\xfffd (integer->char x))))
                                   (loop (fx+ i 2) (fx+ j 1)))
                                ; second byte is not a continuation byte
                                 (begin
                                   (string-set! s j #\xfffd)
                                   (loop (fx+ i 1) (fx+ j 1)))))
                          ; have only one byte
                           (begin
                             (string-set! s j #\xfffd)
                             (fini (fx+ j 1))))]
                      [(fx<= #xe0 b1 #xef) ; three-byte encoding
                       (if (fx< i (fx- n 1)) ; have at least two bytes?
                           (let ([b2 (bytevector-u8-ref bv (fx+ i 1))])
                             (if (fx= (fxsrl b2 6) #b10) ; second byte a continuation byte?
                                 (if (fx< i (fx- n 2)) ; have at least three bytes?
                                     (let ([b3 (bytevector-u8-ref bv (fx+ i 2))])
                                       (if (fx= (fxsrl b3 6) #b10) ; third byte a continuation byte?
                                           (begin
                                             (string-set! s j
                                               (let ([x (fxlogor
                                                          (fxsll (fxlogand b1 #b1111) 12)
                                                          (fxsll (fxlogand b2 #b111111) 6)
                                                          (fxlogand b3 #b111111))])
                                                 (if (and (fx>= x #x800) (not (fx<= #xd800 x #xdfff)))
                                                     (integer->char x)
                                                     #\xfffd)))
                                             (loop (fx+ i 3) (fx+ j 1)))
                                          ; third byte is not a continuation byte
                                           (begin
                                             (string-set! s j #\xfffd)
                                             (loop (fx+ i 2) (fx+ j 1)))))
                                    ; have only two bytes
                                     (begin
                                       (string-set! s j #\xfffd)
                                       (fini (fx+ j 1))))
                                ; second byte is not a continuation byte
                                 (begin
                                   (string-set! s j #\xfffd)
                                   (loop (fx+ i 1) (fx+ j 1)))))
                          ; have only one byte
                           (begin
                             (string-set! s j #\xfffd)
                             (fini (fx+ j 1))))]
                      [(fx<= #xf0 b1 #xf4) ; four-byte encoding
                       (if (fx< i (fx- n 1)) ; have at least two bytes?
                           (let ([b2 (bytevector-u8-ref bv (fx+ i 1))])
                             (if (fx= (fxsrl b2 6) #b10) ; second byte a continuation byte?
                                 (if (fx< i (fx- n 2)) ; have at least three bytes?
                                     (let ([b3 (bytevector-u8-ref bv (fx+ i 2))])
                                       (if (fx= (fxsrl b3 6) #b10) ; third byte a continuation byte?
                                           (if (fx< i (fx- n 3)) ; have at least four bytes?
                                               (let ([b4 (bytevector-u8-ref bv (fx+ i 3))])
                                                 (if (fx= (fxsrl b4 6) #b10) ; fourth byte a continuation byte?
                                                     (begin
                                                       (string-set! s j
                                                         (let ([x (fxlogor
                                                                    (fxsll (fxlogand b1 #b111) 18)
                                                                    (fxsll (fxlogand b2 #b111111) 12)
                                                                    (fxsll (fxlogand b3 #b111111) 6)
                                                                    (fxlogand b4 #b111111))])
                                                           (if (fx<= #x10000 x #x10ffff)
                                                               (integer->char x)
                                                               #\xfffd)))
                                                       (loop (fx+ i 4) (fx+ j 1)))
                                                    ; fourth byte is not a continuation byte
                                                     (begin
                                                       (string-set! s j #\xfffd)
                                                       (loop (fx+ i 3) (fx+ j 1)))))
                                              ; have only three bytes
                                               (begin
                                                 (string-set! s j #\xfffd)
                                                 (fini (fx+ j 1))))
                                          ; third byte is not a continuation byte
                                           (begin
                                             (string-set! s j #\xfffd)
                                             (loop (fx+ i 2) (fx+ j 1)))))
                                    ; have only two bytes
                                     (begin
                                       (string-set! s j #\xfffd)
                                       (fini (fx+ j 1))))
                                ; second byte is not a continuation byte
                                 (begin
                                   (string-set! s j #\xfffd)
                                   (loop (fx+ i 1) (fx+ j 1)))))
                          ; have only one byte
                           (begin
                             (string-set! s j #\xfffd)
                             (fini (fx+ j 1))))]
                      [else
                       (string-set! s j #\xfffd)
                       (loop (fx+ i 1) (fx+ j 1))]))))))))
    (lambda (bv)
      (unless (bytevector? bv) ($oops who "~s is not a bytevector" bv))
      (slurp bv
        (if (and (fx>= (bytevector-length bv) 3)
                 (fx= (bytevector-u8-ref bv 0) #xef)
                 (fx= (bytevector-u8-ref bv 1) #xbb)
                 (fx= (bytevector-u8-ref bv 2) #xbf))
            3 0)))))

(let ()
  (define ($string->utf8 s nul?)
    (let ([sn (string-length s)])
      (let ([bv (do ([si 0 (fx+ si 1)]
                     [bvn 0 (+ bvn (let ([k (char->integer (string-ref s si))])
                                     (if (fx<= k #x7ff)
                                         (if (fx<= k #x7f) 1 2)
                                         (if (fx<= k #xffff) 3 4))))])
                  ((fx= si sn)
                   (if nul?
                       (let ([real-bvn (fx+ bvn 1)])
                         (unless (fixnum? real-bvn)
                           ($oops 'string->utf8 "result would be too large"))
                         (let ([bv (make-bytevector real-bvn)])
                           (bytevector-u8-set! bv bvn 0)
                           bv))
                       (begin
                         (unless (fixnum? bvn)
                           ($oops 'string->utf8 "result would be too large"))
                         (make-bytevector bvn)))))])
        (let f ([si 0] [bvi 0])
          (if (fx= si sn)
              bv
              (let ([k (char->integer (string-ref s si))])
                (if (fx<= k #x7ff)
                    (if (fx<= k #x7f)
                        (begin
                          (bytevector-u8-set! bv bvi k)
                          (f (fx+ si 1) (fx+ bvi 1)))
                        (begin
                          (bytevector-u8-set! bv bvi (fxlogor #b11000000 (fxsrl k 6)))
                          (bytevector-u8-set! bv (fx+ bvi 1) (fxlogor #b10000000 (fxlogand k #b111111)))
                          (f (fx+ si 1) (fx+ bvi 2))))
                    (if (fx<= k #xffff)
                        (begin
                          (bytevector-u8-set! bv bvi (fxlogor #b11100000 (fxsrl k 12)))
                          (bytevector-u8-set! bv (fx+ bvi 1) (fxlogor #b10000000 (fxlogand (fxsrl k 6) #b111111)))
                          (bytevector-u8-set! bv (fx+ bvi 2) (fxlogor #b10000000 (fxlogand k #b111111)))
                          (f (fx+ si 1) (fx+ bvi 3)))
                        (begin
                          (bytevector-u8-set! bv bvi (fxlogor #b11110000 (fxsrl k 18)))
                          (bytevector-u8-set! bv (fx+ bvi 1) (fxlogor #b10000000 (fxlogand (fxsrl k 12) #b111111)))
                          (bytevector-u8-set! bv (fx+ bvi 2) (fxlogor #b10000000 (fxlogand (fxsrl k 6) #b111111)))
                          (bytevector-u8-set! bv (fx+ bvi 3) (fxlogor #b10000000 (fxlogand k #b111111)))
                          (f (fx+ si 1) (fx+ bvi 4)))))))))))
  (set! $fp-string->utf8
    (lambda (s)
      ($string->utf8 s #t)))
  (set-who! string->utf8
    (lambda (s)
      (unless (string? s) ($oops who "~s is not a string" s))
      ($string->utf8 s #f))))

(define-who utf16->string
  (let ()
    (define slurp
      (lambda (bv eness start)
        (let ([n (bytevector-length bv)])
          (let ([s (make-string (fxsrl (fx+ (fx- n start) 1) 1))])
            (define (fini j)
              (if (fx= j (string-length s))
                  s
                  (string-truncate! s j)))
            (define-syntax go
              (syntax-rules ()
                [(_ bv-u16-ref)
                 (let loop ([i start] [j 0])
                   (cond
                     [(fx= i n) (fini j)]
                     [(fx= i (fx- n 1))
                      (string-set! s j #\xfffd)
                      (fini (fx+ j 1))]
                     [else
                      (let ([w1 (bv-u16-ref bv i)])
                        (cond
                          [(fx<= #xD800 w1 #xDBFF) ; two-word encoding
                           (cond
                             [(fx>= i (fx- n 3))
                              (string-set! s j #\xfffd)
                              (fini (fx+ j 1))]
                             [else
                              (let ([w2 (bv-u16-ref bv (fx+ i 2))])
                                (string-set! s j
                                  (if (fx<= #xDC00 w2 #xDFFF)
                                      (integer->char
                                        (fx+ (fxlogor
                                               (fxsll (fx- w1 #xD800) 10)
                                               (fx- w2 #xDC00))
                                             #x10000))
                                      #\xfffd))
                                (loop (fx+ i 4) (fx+ j 1)))])]
                          [(fx<= #xDC00 w1 #xDFFF) ; misplaced continuation word
                           (string-set! s j #\xfffd)
                           (loop (fx+ i 2) (fx+ j 1))]
                          [else
                           (string-set! s j (integer->char w1))
                           (loop (fx+ i 2) (fx+ j 1))]))]))]))
            (if (eq? eness (constant native-endianness))
                (go bytevector-u16-native-ref)
                (go (lambda (bv i) (bytevector-u16-ref bv i eness))))))))
    (rec utf16->string
      (case-lambda
        [(bv eness) (utf16->string bv eness #f)]
        [(bv eness mandatory?)
         (unless (bytevector? bv) ($oops who "~s is not a bytevector" bv))
         (unless (memq eness '(big little)) ($oops who "invalid endianness ~s" eness))
         (if (or mandatory? (< (bytevector-length bv) 2))
             (slurp bv eness 0)
             (let ([BOM (bytevector-u16-native-ref bv 0)])
               (if (fx= BOM #xfeff)
                   (slurp bv (constant native-endianness) 2)
                   (if (fx= BOM #xfffe)
                       (slurp bv (constant-case native-endianness [(big) 'little] [(little) 'big]) 2)
                       (slurp bv eness 0)))))]))))

(let ()
  (define ($string->utf16 s eness nul?)
    (let ([sn (string-length s)])
      (let ([bv (do ([si 0 (fx+ si 1)]
                     [bvn 0 (+ bvn (if (char<=? (string-ref s si) #\xffff) 2 4))])
                  ((fx= si sn)
                   (if nul?
                       (let ([real-bvn (+ bvn 2)])
                         (unless (fixnum? real-bvn)
                           ($oops 'string->utf16 "result would be too large"))
                         (let ([bv (make-bytevector real-bvn)])
                           (bytevector-u16-native-set! bv bvn 0)
                           bv))
                       (begin
                         (unless (fixnum? bvn)
                           ($oops 'string->utf16 "result would be too large"))
                         (make-bytevector bvn)))))])
        (define-syntax go
          (syntax-rules ()
            [(_ bv-u16-set!)
             (let f ([si 0] [bvi 0])
               (unless (fx= si sn)
                 (let ([x (char->integer (string-ref s si))])
                   (if (fx<= x #xffff)
                       (begin
                         (bv-u16-set! bv bvi x)
                         (f (fx+ si 1) (fx+ bvi 2)))
                       (let ([x (fx- x #x10000)])
                         (bv-u16-set! bv bvi (fxior #xD800 (fxsrl x 10)))
                         (bv-u16-set! bv (fx+ bvi 2) (fxior #xDC00 (fxand x #x3ff)))
                         (f (fx+ si 1) (fx+ bvi 4)))))))]))
        (if (eq? eness (constant native-endianness))
            (go bytevector-u16-native-set!)
            (go (lambda (bv i n) (bytevector-u16-set! bv i n eness))))
        bv)))
  (set! $fp-string->utf16
    (lambda (s eness)
      ($string->utf16 s eness #t)))
  (set-who! string->utf16
    (rec string->utf16
      (case-lambda
        [(s)
         (unless (string? s) ($oops who "~s is not a string" s))
         ($string->utf16 s 'big #f)]
        [(s eness)
         (unless (string? s) ($oops who "~s is not a string" s))
         (unless (memq eness '(big little)) ($oops who "invalid endianness ~s" eness))
         ($string->utf16 s eness #f)]))))

(define-who utf32->string
  (let ()
    (define slurp
      (lambda (bv eness start)
        (let ([n (bytevector-length bv)])
          (let ([s (make-string (fxsrl (fx+ (fx- n start) 3) 2))])
            (define-syntax go
              (syntax-rules ()
                [(_ bv-u32-ref)
                 (let loop ([i start] [j 0])
                   (cond
                     [(fx= i n) s]
                     [(fx>= i (fx- n 3))
                      (string-set! s j #\xfffd)
                      s]
                     [else
                      (let ([x (bv-u32-ref bv i)])
                        (string-set! s j
                          (if (and (fixnum? x) (fx<= x #x10ffff) (not (fx<= #xd800 x #xdfff)))
                              (integer->char x)
                              #\xfffd))
                        (loop (fx+ i 4) (fx+ j 1)))]))]))
            (if (eq? eness (constant native-endianness))
                (go bytevector-u32-native-ref)
                (go (lambda (bv i) (bytevector-u32-ref bv i eness))))))))
    (rec utf32->string
      (case-lambda
        [(bv eness) (utf32->string bv eness #f)]
        [(bv eness mandatory?)
         (unless (bytevector? bv) ($oops who "~s is not a bytevector" bv))
         (unless (memq eness '(big little)) ($oops who "invalid endianness ~s" eness))
         (if (or mandatory? (< (bytevector-length bv) 4))
             (slurp bv eness 0)
             (let ([BOM (bytevector-u32-native-ref bv 0)])
               (if (and (fixnum? BOM) (fx= BOM #xfeff))
                   (slurp bv (constant native-endianness) 4)
                   (if (= BOM #xfffe0000)
                       (slurp bv (constant-case native-endianness [(big) 'little] [(little) 'big]) 4)
                       (slurp bv eness 0)))))]))))

(let ()
  (define ($string->utf32 s eness nul?)
    (let ([sn (string-length s)])
      (unless (fx<= (if nul? sn (fx+ sn 1)) (fxsrl (greatest-fixnum) 2))
        ($oops 'string->utf32 "result would be too large"))
      (let ([bv (if nul?
                    (let* ([bvn (fxsll sn 2)] [bv (make-bytevector (fx+ bvn 4))])
                      (bytevector-u32-native-set! bv bvn 0)
                      bv)
                    (make-bytevector (fxsll sn 2)))])
        (define-syntax go
          (syntax-rules ()
            [(_ bv-u32-set!)
             (do ([si 0 (fx+ si 1)])
                 ((fx= si sn))
               (bv-u32-set! bv (fxsll si 2) (char->integer (string-ref s si))))]))
        (if (eq? eness (constant native-endianness))
            (go bytevector-u32-native-set!)
            (go (lambda (bv i n) (bytevector-u32-set! bv i n eness))))
        bv)))
  (set! $fp-string->utf32
    (lambda (s eness)
      ($string->utf32 s eness #t)))
  (set-who! string->utf32
    (rec string->utf32
      (case-lambda
        [(s)
         (type-check who string s)
         ($string->utf32 s 'big #f)]
        [(s eness)
         (type-check who string s)
         (unless (memq eness '(big little)) ($oops who "invalid endianness ~s" eness))
         ($string->utf32 s eness #f)]))))

(define $breakhere (foreign-procedure "(cs)s_breakhere" (ptr) void))

(define $errno->string (foreign-procedure "(cs)s_strerror" (int) scheme-object))

(define $errno (foreign-procedure "(cs)s_errno" () int))

(define interactive? (foreign-procedure "(cs)s_interactivep" () boolean))

(define-who $read-performance-monitoring-counter
  (lambda (x)
   ; might should check valid range (which is fixed by the hardware we are running on)
    (type-check who fixnum x)
    (#3%$read-performance-monitoring-counter x)))

(define $read-time-stamp-counter
  (lambda ()
    (#3%$read-time-stamp-counter)))

(define $keep-live
  (lambda (x)
    (#2%$keep-live x)))

(when-feature windows
(let ()
  (define mbtwc
    (foreign-procedure "(cs)s_multibytetowidechar"
      (unsigned ptr)
      ptr))
  (define wctmb
    (foreign-procedure "(cs)s_widechartomultibyte"
      (unsigned ptr)
      ptr))
  (define (cp->unsigned who cp)
    (case cp
      [(cp-acp) 0]
      [(cp-maccp) 2]
      [(cp-oemcp) 1]
      [(cp-symbol) 42]
      [(cp-thread-acp) 3]
      [(cp-utf7) 65000]
      [(cp-utf8) 65001]
      [else
       (if (and (fixnum? cp) (fx>= cp 0))
           cp
           ($oops who "invalid code page ~s" cp))]))
  (set-who! multibyte->string
    (lambda (cp bv)
      (let ([cp (cp->unsigned who cp)])
        (unless (bytevector? bv) ($oops who "~s is not a bytevector" bv))
        (utf16->string (mbtwc cp bv) 'little #t))))
  (set-who! string->multibyte
    (lambda (cp str)
      (let ([cp (cp->unsigned who cp)])
        (unless (string? str) ($oops who "~s is not a string" str))
        (wctmb cp (string->utf16 str 'little))))))
)
)
