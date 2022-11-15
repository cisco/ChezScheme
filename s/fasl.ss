;;; fasl.ss
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

;; The fasl reader is "fasl.c", which includes an overview of the fasl
;; format.

(let ()
(define-record-type target
  (nongenerative #{target dchg2hp5v3cck8ge283luo-1})
  (sealed #t)
  (fields
    fasl-bld-graph
    fasl-enter
    fasl-out
    fasl-start
    fasl-table
    fasl-wrf-graph
    fasl-base-rtd
    fasl-write
    fasl-file))

(let ()
(include "types.ss")

; don't use rtd-* as defined in record.ss in case we're building a patch
; file for cross compilation, because the offsets may be incorrect
(define rtd-size (csv7:record-field-accessor #!base-rtd 'size))
(define rtd-flds (csv7:record-field-accessor #!base-rtd 'flds))
(define rtd-ancestry (csv7:record-field-accessor #!base-rtd 'ancestry))
(define rtd-name (csv7:record-field-accessor #!base-rtd 'name))
(define rtd-uid (csv7:record-field-accessor #!base-rtd 'uid))
(define rtd-flags (csv7:record-field-accessor #!base-rtd 'flags))

(define-record-type table
  (fields (mutable count) (immutable hash)
          (immutable external?-pred) (mutable external-count) (mutable externals)
          (mutable bignums))
  (nongenerative)
  (sealed #t)
  (protocol
   (lambda (new)
     (case-lambda
      [() (new 0 (make-eq-hashtable) #f 0 '() #f)]
      [(external?-pred) (new 0 (make-eq-hashtable) external?-pred 0 '() #f)]))))

(define maybe-remake-rtd
  (lambda (rtd t)
    (if (eq? (machine-type) ($target-machine))
        rtd
        ($remake-rtd rtd (let () (include "layout.ss") compute-field-offsets)))))

(include "fasl-helpers.ss")
(include "target-fixnum.ss")

(define maybe-intern-bignum
  ;; When we cross-compile on a 32-bit machine for a 64-bit machine or
  ;; vice versa, constant folding or reading separate instances of an
  ;; integer can produce different sharing. To avoid structural
  ;; differences in fasled boot files, intern bignums in 'system mode.
  (lambda (x t)
    (cond
      [(and (bignum? x)
            (eq? (subset-mode) 'system))
       (when (not (table-bignums t))
         (table-bignums-set! t (make-hashtable equal-hash equal?)))
       (cdr (hashtable-cell (table-bignums t) x x))]
      [else x])))

(define bld-pair
   (lambda (x t a? d)
      (bld (car x) t a? d)
      (bld (cdr x) t a? d)))

(define bld-vector
   (lambda (x t a? d)
      (let ([len (vector-length x)])
         (let bldvec ([i 0])
            (unless (fx= i len)
               (bld (vector-ref x i) t a? d)
               (bldvec (fx+ i 1)))))))

(define bld-stencil-vector
   (lambda (x t a? d)
      (let ([len ($stencil-vector-length x)])
         (let bldvec ([i 0])
            (unless (fx= i len)
               (bld ($stencil-vector-ref x i) t a? d)
               (bldvec (fx+ i 1)))))))

(define bld-record
  (lambda (x t a? d)
    (unless (eq? x #!base-rtd)
      (cond
        [(record-type-descriptor? x)
         ;; fasl representation for record-type-descriptor includes uid separately and as part of the record
         (bld (record-type-uid x) t a? d)
         (really-bld-record (maybe-remake-rtd x t) t a? d)]
        [else
         (really-bld-record x t a? d)]))))

(define really-bld-record
  (lambda (x t a? d)
    (let ([rtd ($record-type-descriptor x)])
      (bld rtd t a? d)
      (let ([flds (rtd-flds rtd)])
        (if (fixnum? flds)
            (let loop ([i 0])
              (unless (fx= i flds)
                (bld ($record-ref x i) t a? d)
                (loop (fx+ i 1))))
            (do ([flds flds (cdr flds)] [i 0 (+ i 1)])
              ((null? flds))
              (when (memq (fld-type (car flds)) '(scheme-object ptr))
                (bld ((csv7:record-field-accessor rtd i) x) t a? d))))))))

(define bld-ht
  (lambda (x t a? d)
    (let-values ([(keyvec valvec) (hashtable-entries x)])
      (vector-for-each
        (lambda (key val)
          (bld key t a? d)
          (bld val t a? d))
        keyvec valvec))))

(define bld-box
   (lambda (x t a? d)
      (bld (unbox x) t a? d)))

(define bld-simple
   (lambda (x t a? d)
      (void)))

(module (bld-graph dump-graph reset-dump-graph shift-externals!)
  (define enable-dump-graph? #f)
  (define vcat (if enable-dump-graph?
                   `#((code . ,(lambda (x) (and (pair? x) (eq? (car x) 'code))))
                      (pair . ,pair?)
                      (string . ,string?)
                      (symbol . ,symbol?)
                      (vector . ,vector?)
                      (record . ,record?)
                      (other . ,(lambda (x) #t)))))
  (define ventry)
  (define vdup)
  (define record!
    (lambda (v x)
      (when enable-dump-graph?
        (let f ([i 0])
          (let ([cat (vector-ref vcat i)])
            (if ((cdr cat) x)
                (vector-set! v i (fx+ (vector-ref v i) 1))
                (f (fx+ i 1))))))))
  (define reset-dump-graph
    (lambda ()
      (when enable-dump-graph?
        (set! ventry (make-vector (vector-length vcat) 0))
        (set! vdup (make-vector (vector-length vcat) 0)))))
  (define dump-graph
    (lambda ()
      (when enable-dump-graph?
        (vector-for-each
          (lambda (cat entry dup)
            (printf "~10s ~10s ~s\n" entry dup (car cat)))
          vcat ventry vdup))))
  (define bld-graph
    (lambda (x t a? d inner? handler)
      (let ([a (eq-hashtable-cell (table-hash t) x 'first)])
        (let ([p (cdr a)])
          (cond
            [(eq? p 'first)
             #;(let ([n (hashtable-size (table-hash t))])
                 (when (fx= (modulo n 10000) 0)
                   (printf "entries = ~s, ba = ~s, count = ~s\n" n (bytes-allocated) (table-count t))))
             (cond
               [(let ([pred (table-external?-pred t)])
                  (and pred
                       (pred x)))
                ;; Don't traverse; just record as external. We'll
                ;; assign positions to externals after the graph
                ;; has been fully traversed.
                (let ([p (cons (table-external-count t) #f)])
                  (set-cdr! a p)
                  (table-external-count-set! t (fx+ (table-external-count t) 1))
                  (table-externals-set! t (cons p (table-externals t))))]
               [else
                (record! ventry x)
                (cond
                  [(fx>= d 500)
                   ;; Limit depth of recursion by lifting to a `fasl-begin` graph:
                   (let ([n (table-count t)])
                     (set-cdr! a (cons n (if inner? 'inner-begin 'begin)))
                     (table-count-set! t (fx+ n 1)))
                   (handler x t a? 0)]
                  [else
                   (set-cdr! a #f)
                   (handler x t a? (fx+ d 1))])])]
            [(not p)
             (record! vdup x)
             (let ([n (table-count t)])
               (set-cdr! a (cons n #t))
               (table-count-set! t (fx+ n 1)))])))))
  (define (shift-externals! t)
    (unless (null? (table-externals t))
      (let ([c (table-count t)])
        (table-count-set! t (fx+ c (table-external-count t)))
        (for-each (lambda (p)
                    (set-car! p (fx+ (car p) c)))
                  (table-externals t)))))
  (reset-dump-graph))

(define bld
   (lambda (x t a? d)
      (cond
        [(fixnum? x) (if (target-bignum? x)
                         (bld-graph x t a? d #t bld-simple)
                         (bld-simple x t a? d))]
        [($immediate? x) (bld-simple x t a? d)]
        [(pair? x) (bld-graph x t a? d #t bld-pair)]
        [(vector? x) (bld-graph x t a? d #t bld-vector)]
        [($stencil-vector? x) (bld-graph x t a? d #t bld-stencil-vector)]
        [(or (symbol? x) (string? x)) (bld-graph x t a? d #t bld-simple)]
        ; this check must go before $record? check
        [(and (annotation? x) (not a?))
         (bld (annotation-stripped x) t a? d)]
        ; this check must go before $record? check
        [(eq-hashtable? x) (bld-graph x t a? d #t bld-ht)]
        ; this check must go before $record? check
        [(symbol-hashtable? x) (bld-graph x t a? d #t bld-ht)]
        [($record? x) (bld-graph x t a? d #t bld-record)]
        [(box? x) (bld-graph x t a? d #t bld-box)]
        [(bignum? x) (if (target-fixnum? x)
                         (bld-simple x t a? d)
                         (bld-graph (maybe-intern-bignum x t) t a? d #t bld-simple))]
        [else (bld-graph x t a? d #t bld-simple)])))

(module (small-integer? large-integer?)
  (define least-small-integer (- (expt 2 31)))
  (define greatest-small-integer (- (expt 2 31) 1))
  (define small-integer?
    (lambda (x)
      (if (fixnum? greatest-small-integer)
          (and (fixnum? x) (fx<= least-small-integer x greatest-small-integer))
          (or (fixnum? x) (and (bignum? x) (<= least-small-integer x greatest-small-integer))))))
  (define large-integer?
    (lambda (x)
      (if (fixnum? greatest-small-integer)
          (if (fixnum? x) (not (fx<= least-small-integer x greatest-small-integer)) (bignum? x))
          (and (bignum? x) (not (<= least-small-integer x greatest-small-integer)))))))

(define wrf-small-integer
  (lambda (x p t a?)
    (put-u8 p (constant fasl-type-small-integer))
    (put-iptr p x)))

(define wrf-large-integer
  (lambda (x p t a?)
    (put-u8 p (constant fasl-type-large-integer))
    (put-u8 p (if (positive? x) 0 1))
    (let* ([x (abs x)] [il (integer-length x)])
      (let* ([n (bitwise-arithmetic-shift-right il
                  (log2 (constant bigit-bits)))]
             [m (bitwise-arithmetic-shift-left n
                  (log2 (constant bigit-bits)))])
        (if (fx= m il)
            (put-uptr p n)
            (begin
              (put-uptr p (+ n 1))
              (put-uptr p (bitwise-arithmetic-shift-right x m))))
        (let f ([end m])
          (unless (= end 0)
            (let ([start (- end (constant bigit-bits))])
              (put-uptr p (bitwise-bit-field x start end))
              (f start))))))))

(define wrf-pair
  (lambda (x p t a?)
    (cond
      [(weak-pair? x)
       (put-u8 p (constant fasl-type-weak-pair))
       (wrf (car x) p t a?)
       (wrf (cdr x) p t a?)]
      [(ephemeron-pair? x)
       (put-u8 p (constant fasl-type-ephemeron))
       (wrf (car x) p t a?)
       (wrf (cdr x) p t a?)]
      [else
       ; more like list*
       (put-u8 p (constant fasl-type-pair))
       (let ([n (let wrf-pair-loop0 ([n 1] [x (cdr x)])
                  ; cut off at end or at shared structure
                  (if (and (pair? x)
                           (not (weak-pair? x))
                           (not (ephemeron-pair? x))
                           (not (eq-hashtable-ref (table-hash t) x #f)))
                      (wrf-pair-loop0 (fx+ n 1) (cdr x))
                      n))])
         (put-uptr p n)
         (let wrf-pair-loop1 ([x x] [n n])
           (wrf (car x) p t a?)
           (if (fx= n 1)
               (wrf (cdr x) p t a?)
               (wrf-pair-loop1 (cdr x) (fx- n 1)))))])))

(define wrf-symbol
  (lambda (x p t a?)
    (cond
      [(gensym? x)
       (let ((uname (gensym->unique-string x)))
         (put-u8 p (constant fasl-type-gensym))
         (wrf-string-help (symbol->string x) p)
         (wrf-string-help uname p))]
      [(uninterned-symbol? x)
       (put-u8 p (constant fasl-type-uninterned-symbol))
       (wrf-string-help (symbol->string x) p)]
      [else
       (put-u8 p (constant fasl-type-symbol))
       (wrf-string-help (symbol->string x) p)])))

(define wrf-string-help
  (lambda (x p)
    (put-uptr p (string-length x))
    (let ([n (string-length x)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i n))
        (put-uptr p (char->integer (string-ref x i)))))))

(define wrf-string
   (lambda (x p t a?)
      (put-u8 p (if (immutable-string? x)
                    (constant fasl-type-immutable-string)
                    (constant fasl-type-string)))
      (wrf-string-help x p)))

(define wrf-vector
   (lambda (x p t a?)
      (put-u8 p (if (immutable-vector? x)
                    (constant fasl-type-immutable-vector)
                    (constant fasl-type-vector)))
      (let ([n (vector-length x)]) 
         (put-uptr p n)
         (let wrf-vector-loop ([i 0])
            (unless (fx= i n)
               (wrf (vector-ref x i) p t a?)
               (wrf-vector-loop (fx+ i 1)))))))

(define wrf-fxvector
  (lambda (x p t a?)
    (put-u8 p (constant fasl-type-fxvector))
    (let ([n (fxvector-length x)])
      (put-uptr p n)
      (let wrf-fxvector-loop ([i 0])
        (unless (fx= i n)
          (put-iptr p (fxvector-ref x i))
          (wrf-fxvector-loop (fx+ i 1)))))))

(define wrf-flvector
  (lambda (x p t a?)
    (put-u8 p (constant fasl-type-flvector))
    (let ([n (flvector-length x)])
      (put-uptr p n)
      (let wrf-flvector-loop ([i 0])
        (unless (fx= i n)
          (wrf-flonum (flvector-ref x i) p t a?)
          (wrf-flvector-loop (fx+ i 1)))))))

(define wrf-bytevector
  (lambda (x p t a?)
    (put-u8 p (if (immutable-bytevector? x)
                  (constant fasl-type-immutable-bytevector)
                  (constant fasl-type-bytevector)))
    (let ([n (bytevector-length x)])
      (put-uptr p n)
      (let wrf-bytevector-loop ([i 0])
        (unless (fx= i n)
          (let ([x (bytevector-u8-ref x i)])
            (put-u8 p x)
            (wrf-bytevector-loop (fx+ i 1))))))))

(define wrf-stencil-vector
   (lambda (x p t a?)
      (put-u8 p (if ($system-stencil-vector? x)
                    (constant fasl-type-system-stencil-vector)
                    (constant fasl-type-stencil-vector)))
      (put-uptr p ($stencil-vector-mask x))
      (let ([n ($stencil-vector-length x)]) 
         (let wrf-stencil-vector-loop ([i 0])
            (unless (fx= i n)
               (wrf ($stencil-vector-ref x i) p t a?)
               (wrf-stencil-vector-loop (fx+ i 1)))))))

; Written as: fasl-tag rtd field ...
(module (wrf-record really-wrf-record wrf-annotation)
  (define wrf-fields
    (lambda (x p t a?)
      ; extract field values using host field information (byte offset and filtered
      ; type); write using target field information.  to save i/o & space, using iptr
      ; as common rep'n for multibyte integer fields since any small unsigned quantity
      ; is a small signed but a small negative signed quantity is a large unsigned
      ; quantity.  we check 16- and 32-bit integer values and fixnums before writing
      ; them in case the host field is larger than the target field.
      (define get-field
        (lambda (host-fld)
          (let ([type (fld-type host-fld)] [addr (fld-byte host-fld)])
            ; using $filter-foreign-type to get host filtering
            (case ($filter-foreign-type type)
              [(scheme-object) ($object-ref 'ptr x addr)]
              [(integer-8 unsigned-8 char) ($object-ref 'unsigned-8 x addr)]
              [(integer-16 unsigned-16) ($object-ref 'integer-16 x addr)]
              [(integer-24 unsigned-24) ($object-ref 'integer-24 x addr)]
              [(integer-32 unsigned-32) ($object-ref 'integer-32 x addr)]
              [(integer-40 unsigned-40) ($object-ref 'integer-40 x addr)]
              [(integer-48 unsigned-48) ($object-ref 'integer-48 x addr)]
              [(integer-56 unsigned-56) ($object-ref 'integer-56 x addr)]
              [(integer-64 unsigned-64) ($object-ref 'integer-64 x addr)]
              [(single-float) ($object-ref 'unsigned-32 x addr)]
              [(double-float) ($object-ref 'unsigned-64 x addr)]
              [(wchar)
               (constant-case wchar-bits
                 [(16) ($object-ref 'integer-16 x addr)]
                 [(32) ($object-ref 'integer-32 x addr)])]
              [(fixnum) ($object-ref 'fixnum x addr)]
              [else ($oops 'fasl-write "cannot fasl record field of type ~s" type)]))))
      (define check-field
        (lambda (target-fld val)
          (unless (eq? (constant machine-type-name) (machine-type))
            (let* ([type (fld-type target-fld)] [filtered-type (filter-foreign-type type)])
              (unless (case filtered-type
                        [(scheme-object) #t]
                        [(integer-16 unsigned-16) ($integer-16? val)]
                        [(integer-32 unsigned-32) ($integer-32? val)]
                        [(wchar)
                         (constant-case wchar-bits
                           [(16) ($integer-16? val)]
                           [(32) ($integer-32? val)])]
                        [(fixnum) (<= (- (ash 1 (- (constant fixnum-bits) 1))) val (- (ash 1 (- (constant fixnum-bits) 1)) 1))]
                        [(char single-float double-float) #t]
                        [(integer-8 integer-64 integer-24 integer-40 integer-48 integer-56) #t]
                        [(unsigned-8 unsigned-64 unsigned-24 unsigned-40 unsigned-48 unsigned-56) #t]
                        [else ($oops 'fasl-write "unexpected difference in filtered foreign type ~s for unfiltered type ~s" filtered-type type)])
                ($oops 'fasl-write "host value ~s for type ~s is too big for target" val type))))))
      (define put-field
        (lambda (field-type field-addr pad val)
          (define put-i64
            (lambda (p val)
              (constant-case ptr-bits
                [(32) (put-iptr p (bitwise-arithmetic-shift-right val 32)) (put-uptr p (logand val #xffffffff))]
                [(64) (put-iptr p val)])))
          (define-syntax put-padty
            (syntax-rules ()
              [(_ fasl-fld-type)
               (put-u8 p (fxlogor (fxsll pad 4) (constant fasl-fld-type)))]))
          (let ([type field-type] [addr field-addr])
            ; using filter-foreign-type to get target filtering
            (case (filter-foreign-type type)
              [(scheme-object) (put-padty fasl-fld-ptr) (wrf val p t a?) (constant ptr-bytes)]
              [(integer-8 unsigned-8 char) (put-padty fasl-fld-u8) (put-u8 p val) 1]
              [(integer-16 unsigned-16) (put-padty fasl-fld-i16) (put-iptr p val) 2]
              [(integer-24 unsigned-24) (put-padty fasl-fld-i24) (put-iptr p val) 3]
              [(integer-32 unsigned-32) (put-padty fasl-fld-i32) (put-iptr p val) 4]
              [(integer-40 unsigned-40) (put-padty fasl-fld-i40) (put-i64 p val) 5]
              [(integer-48 unsigned-48) (put-padty fasl-fld-i48) (put-i64 p val) 6]
              [(integer-56 unsigned-56) (put-padty fasl-fld-i56) (put-i64 p val) 7]
              [(integer-64 unsigned-64) (put-padty fasl-fld-i64) (put-i64 p val) 8]
              [(single-float)
               (put-padty fasl-fld-single)
               (put-uptr p val)
               4]
              [(double-float)
               (put-padty fasl-fld-double)
               (let ([n val])
                 (put-uptr p (ash n -32))
                 (put-uptr p (logand n #xFFFFFFFF)))
               8]
              [(wchar)
               (constant-case wchar-bits
                 [(16) (put-padty fasl-fld-i16) (put-iptr p val)]
                 [(32) (put-padty fasl-fld-i32) (put-iptr p val)])
               (/ (constant wchar-bits) 8)]
              [(fixnum)
               (constant-case ptr-bits
                 [(32) (put-padty fasl-fld-i32)]
                 [(64) (put-padty fasl-fld-i64)])
               (put-iptr p val)
               (constant ptr-bytes)]
              [else ($oops 'fasl-write "cannot fasl record field of type ~s" type)]))))
      (let* ([host-rtd ($record-type-descriptor x)]
             [target-rtd (maybe-remake-rtd host-rtd t)]
             [target-fld* (rtd-flds target-rtd)])
        (put-uptr p (rtd-size target-rtd))
        (put-uptr p (if (fixnum? target-fld*) target-fld* (length target-fld*)))
        (wrf host-rtd p t a?)
        (if (fixnum? target-fld*)
            (let loop ([i 0] [addr (constant record-data-disp)])
              (unless (fx= i target-fld*)
                (let ([sz (put-field 'scheme-object addr 0 ($record-ref x i))])
                  (loop (fx+ i 1) (fx+ addr sz)))))
            (fold-left
              (lambda (last-target-addr host-fld target-fld)
                (let ([val (get-field host-fld)])
                  (check-field target-fld val)
                  (let ([target-addr (fld-byte target-fld)])
                    (fx+ target-addr (put-field (fld-type host-fld) (fld-byte host-fld)
                                                (fx- target-addr last-target-addr) val)))))
              (constant record-data-disp)
              (rtd-flds host-rtd)
              target-fld*)))))

  (define wrf-record
    (lambda (x p t a?)
      (if (eq? x #!base-rtd)
          (put-u8 p (constant fasl-type-base-rtd))
          (really-wrf-record x p t a?))))

  (define really-wrf-record
    (lambda (x p t a?)
      (cond
        [(record-type-descriptor? x)
         (put-u8 p (constant fasl-type-rtd))
         (wrf (record-type-uid x) p t a?)
         (let ([self (let ([a (rtd-ancestry x)])
                       (vector-ref a (sub1 (vector-length a))))])
           (unless (eq? x self)
             ($oops 'fasl "mismatch ~s ~s" x self)))
         (unless (eq-hashtable-ref (table-hash t) x #f)
           ($oops 'fasl "not in table!?"))
         (if (and a? (fxlogtest a? (constant fasl-omit-rtds)))
             (put-uptr p 0) ; => must be registered already at load time
             (wrf-fields (maybe-remake-rtd x t) p t a?))]
        [else
         (put-u8 p (constant fasl-type-record))
         (wrf-fields x p t a?)])))

  (define wrf-annotation
    (lambda (x p t a?)
      (define maybe-remake-annotation
        (lambda (x a?)
          (let ([a? (fxand a? (constant annotation-all))])
            (if (fx= (annotation-flags x) a?)
                x
                (make-annotation (annotation-expression x) (annotation-source x) (annotation-stripped x) a?)))))
      (put-u8 p (constant fasl-type-record))
      (wrf-fields (maybe-remake-annotation x a?) p t a?)))
)

(define wrf-eqht
  (lambda (x p t a?)
    (put-u8 p (constant fasl-type-eq-hashtable))
    (put-u8 p (if (hashtable-mutable? x) 1 0))
    (put-u8 p (cond
               [(eq-hashtable-weak? x) (constant eq-hashtable-subtype-weak)]
               [(eq-hashtable-ephemeron? x) (constant eq-hashtable-subtype-ephemeron)]
               [else  (constant eq-hashtable-subtype-normal)]))
    (put-uptr p ($ht-minlen x))
    (put-uptr p ($ht-veclen x))
    (let-values ([(keyvec valvec) (hashtable-entries x)])
      (put-uptr p (vector-length keyvec))
      (vector-for-each
        (lambda (key val)
          (wrf key p t a?)
          (unless (<= (constant most-positive-fixnum) (most-positive-fixnum))
            (when (fixnum? key)
              (unless (fx<= (constant most-negative-fixnum) key (constant most-positive-fixnum))
                ($oops 'fasl-write "eq-hashtable fixnum key ~s is out-of-range for target machine" key))))
          (wrf val p t a?))
        keyvec valvec))))

(define wrf-symht
  (lambda (x p t a?)
    (put-u8 p (constant fasl-type-symbol-hashtable))
    (put-u8 p (if (hashtable-mutable? x) 1 0))
    (put-uptr p ($ht-minlen x))
    (put-u8 p
      (let ([equiv? (hashtable-equivalence-function x)])
        (cond
          [(eq? equiv? eq?) 0]
          [(eq? equiv? eqv?) 1]
          [(eq? equiv? equal?) 2]
          [(eq? equiv? symbol=?) 3]
          [else ($oops 'fasl-write "unexpected equivalence function ~s for symbol hashtable ~s" equiv? x)])))
    (put-uptr p ($ht-veclen x))
    (let-values ([(keyvec valvec) (hashtable-entries x)])
      (put-uptr p (vector-length keyvec))
      (vector-for-each
        (lambda (key val)
          (wrf key p t a?)
          (wrf val p t a?))
        keyvec valvec))))

(define wrf-box
   (lambda (x p t a?)
      (put-u8 p (if (immutable-box? x)
                    (constant fasl-type-immutable-box)
                    (constant fasl-type-box)))
      (wrf (unbox x) p t a?)))

(define wrf-ratnum
   (lambda (x p t a?)
      (put-u8 p (constant fasl-type-ratnum))
      (wrf ($ratio-numerator x) p t a?)
      (wrf ($ratio-denominator x) p t a?)))

(define wrf-inexactnum
   (lambda (x p t a?)
      (put-u8 p (constant fasl-type-inexactnum))
      (wrf ($inexactnum-real-part x) p t a?)
      (wrf ($inexactnum-imag-part x) p t a?)))

(define wrf-exactnum
   (lambda (x p t a?)
      (put-u8 p (constant fasl-type-exactnum))
      (wrf ($exactnum-real-part x) p t a?)
      (wrf ($exactnum-imag-part x) p t a?)))

(define wrf-char
   (lambda (x p)
      (wrf-immediate
        (fxlogor (fxsll (char->integer x) (constant char-data-offset))
                 (constant type-char))
        p)))

(define wrf-immediate
   (lambda (x p)
      (put-u8 p (constant fasl-type-immediate))
      (put-uptr p x)))

(define wrf-flonum
   (lambda (x p t a?)
     (put-u8 p (constant fasl-type-flonum))
     (let ([n ($object-ref 'unsigned-64 x (constant flonum-data-disp))])
       (put-uptr p (ash n -32))
       (put-uptr p (logand n #xFFFFFFFF)))))

(define wrf-phantom
  (lambda (x p t a?)
    (put-u8 p (constant fasl-type-phantom))
    (put-uptr p (phantom-bytevector-length x))))

(define wrf-graph
   (lambda (x p t a? handler)
      (let ([a (eq-hashtable-ref (table-hash t) x #f)])
         (cond
            [(not a)
             (handler x p t a?)]
            [(cdr a)
             (put-u8 p (constant fasl-type-graph-def))
             (put-uptr p (car a))
             (set-cdr! a #f)
             (handler x p t a?)]
            [else
             (put-u8 p (constant fasl-type-graph-ref))
             (put-uptr p (car a))]))))

(define (wrf-invalid x p t a?)
  (wrf-graph x p t a?
             (lambda (x p t a?)
               ($oops 'fasl-write "invalid fasl object ~s" x))))

(define wrf
   (lambda (x p t a?)
      (cond
         [(symbol? x) (wrf-graph x p t a? wrf-symbol)]
         [(pair? x) (wrf-graph x p t a? wrf-pair)]
         [(small-integer? x) (if (target-fixnum? x)
                                 (wrf-small-integer x p t a?)
                                 (wrf-graph (maybe-intern-bignum x t) p t a? wrf-small-integer))]
         [(null? x) (wrf-immediate (constant snil) p)]
         [(not x) (wrf-immediate (constant sfalse) p)]
         [(eq? x #t) (wrf-immediate (constant strue) p)]
         [(string? x) (wrf-graph x p t a? wrf-string)]
         [(fxvector? x) (wrf-graph x p t a? wrf-fxvector)]
         [(flvector? x) (wrf-graph x p t a? wrf-flvector)]
         [(bytevector? x) (wrf-graph x p t a? wrf-bytevector)]
         ; this check must go before $record? check
         [(annotation? x)
          (if (and a? (fxlogtest a? (constant annotation-all)))
              (wrf-graph x p t a? wrf-annotation)
              (wrf (annotation-stripped x) p t a?))]
         ; this check must go before $record? check
         [(eq-hashtable? x) (wrf-graph x p t a? wrf-eqht)]
         ; this check must go before $record? check
         [(symbol-hashtable? x) (wrf-graph x p t a? wrf-symht)]
         ; this check must go before $record? check
         [(hashtable? x) (wrf-invalid  x p t a?)]
         [($record? x) (wrf-graph x p t a? wrf-record)]
         [(vector? x) (wrf-graph x p t a? wrf-vector)]
         [($stencil-vector? x) (wrf-graph x p t a? wrf-stencil-vector)]
         [(char? x) (wrf-char x p)]
         [(box? x) (wrf-graph x p t a? wrf-box)]
         [(large-integer? x) (if (target-fixnum? x)
                                 (wrf-large-integer x p t a?)
                                 (wrf-graph (maybe-intern-bignum x t) p t a? wrf-large-integer))]
         [(ratnum? x) (wrf-graph x p t a? wrf-ratnum)]
         [(flonum? x) (wrf-graph x p t a? wrf-flonum)]
         [($inexactnum? x) (wrf-graph x p t a? wrf-inexactnum)]
         [($exactnum? x) (wrf-graph x p t a? wrf-exactnum)]
         [(eof-object? x) (wrf-immediate (constant seof) p)]
         [(bwp-object? x) (wrf-immediate (constant sbwp) p)]
         [($unbound-object? x) (wrf-immediate (constant sunbound) p)]
         [(eq? x (void)) (wrf-immediate (constant svoid) p)]
         [(eq? x '#0=#0#) (wrf-immediate (constant black-hole) p)]
         [($rtd-counts? x) (wrf-immediate (constant sfalse) p)]
         [(phantom-bytevector? x) (wrf-graph x p t a? wrf-phantom)]
         [else (wrf-invalid x p t a?)])))

(module (start)
  (define start
    (lambda (p t situation x a? proc)
      (shift-externals! t)
      (dump-graph)
      (let-values ([(bv* size)
                    (let-values ([(p extractor) ($open-bytevector-list-output-port)])
                      (let ([n (table-count t)])
                        (unless (fx= n 0)
                          (put-u8 p (constant fasl-type-graph))
                          (put-uptr p n)
                          (put-uptr p (table-external-count t))))
                      (let ([begins (extract-begins t)])
                        (unless (null? begins)
                          (put-u8 p (constant fasl-type-begin))
                          (put-uptr p (fx+ (length begins) 1))
                          (for-each (lambda (x)
                                      (if (eq? 'begin (cdr (eq-hashtable-ref (table-hash t) x #f)))
                                          (proc x p)
                                          (wrf x p t a?)))
                                    begins)))
                      (proc x p)
                      (extractor))])
        ($write-fasl-bytevectors p bv* size situation (constant fasl-type-fasl)))))

  (define (extract-begins t)
    (let ([ht (table-hash t)])
      (let-values ([(keys vals) (hashtable-entries ht)])
        (let ([len (vector-length keys)])
          (let loop ([i 0] [begins '()])
            (cond
             [(fx= i len)
              ;; Sort so that higher graph numbers are earlier, which
              ;; achieves the intended effect of limiting recursion.
              (list-sort (lambda (a b)
                           (> (car (eq-hashtable-ref ht a #f))
                              (car (eq-hashtable-ref ht b #f))))
                         begins)]
             [else
              (let ([v (vector-ref vals i)])
                (cond
                 [(not v) (loop (fx+ i 1) begins)]
                 [(or (eq? 'begin (cdr v))
                      (eq? 'inner-begin (cdr v)))
                  (loop (fx+ i 1)
                        (cons (vector-ref keys i) begins))]
                 [else (loop (fx+ i 1) begins)]))])))))))

(module (fasl-write fasl-file)
  ; when called from fasl-write or fasl-file, always preserve annotations;
  ; otherwise use value passed in by the compiler
  (define fasl-one
    (lambda (x p external?-pred omit-rtds?)
      (let ([t (make-table external?-pred)]
            [a? (fxior (constant annotation-all)
                       (if omit-rtds?
                           (constant fasl-omit-rtds)
                           0))])
         (bld x t a? 0)
         (start p t (constant fasl-type-visit-revisit) x a? (lambda (x p) (wrf x p t a?))))))

  (define-who fasl-write
    (case-lambda
     [(x p) (fasl-write x p #f #f)]
     [(x p external?-pred) (fasl-write x p external?-pred #f)]
     [(x p external?-pred omit-rtds?)
      (unless (and (output-port? p) (binary-port? p))
        ($oops who "~s is not a binary output port" p))
      (unless (or (not external?-pred) (procedure? external?-pred))
        ($oops who "~s is not #f or a procedure" external?-pred))
      (when ($port-flags-set? p (constant port-flag-compressed)) ($compressed-warning who p))
      (emit-header p (constant scheme-version) (constant machine-type-any))
      (fasl-one x p external?-pred omit-rtds?)]))

  (define-who fasl-file
    (lambda (in out)
      (unless (string? in) ($oops who "~s is not a string" in))
      (unless (string? out) ($oops who "~s is not a string" out))
      (let ([ip ($open-file-input-port who in (file-options)
                  (buffer-mode block) (current-transcoder))]
            [op ($open-file-output-port who out (file-options replace))])
        (on-reset
          (begin
            (close-input-port ip)
            (delete-file out #f))
          (on-reset
            (close-port op)
            (emit-header op (constant scheme-version) (constant machine-type-any))
            (let fasl-loop ()
              (let ([x (read ip)])
                (unless (eof-object? x)
                  (fasl-one x op #f #f)
                  (fasl-loop)))))
          (close-port op))
        (close-port ip)))))

(define fasl-base-rtd
  (lambda (x p)
    (emit-header p (constant scheme-version) (constant machine-type-any))
    (let ([t (make-table)])
      (bld-graph x t #f 0 #t really-bld-record)
      (start p t (constant fasl-type-visit-revisit) x #f (lambda (x p) (wrf-graph x p t #f really-wrf-record))))))

($fasl-target (make-target bld-graph bld wrf start make-table wrf-graph fasl-base-rtd fasl-write fasl-file))
)

(let ()
  (define fasl-target
    (lambda ()
      (let ([target ($fasl-target)])
        (assert target)
        target)))
  (set! $fasl-bld-graph (lambda (x t a? d inner? handler) ((target-fasl-bld-graph (fasl-target)) x t a? d inner? handler)))
  (set! $fasl-enter (lambda (x t a? d) ((target-fasl-enter (fasl-target)) x t a? d)))
  (set! $fasl-out (lambda (x p t a?) ((target-fasl-out (fasl-target)) x p t a?)))
  (set! $fasl-start (lambda (p t situation x a? proc) ((target-fasl-start (fasl-target)) p t situation x a? proc)))
  (set! $fasl-table (case-lambda
                     [() ((target-fasl-table (fasl-target)))]
                     [(external?-pred) ((target-fasl-table (fasl-target)) external?-pred)]))
  (set! $fasl-wrf-graph (lambda (x p t a? handler) ((target-fasl-wrf-graph (fasl-target)) x p t a? handler)))
  (set! $fasl-base-rtd (lambda (x p) ((target-fasl-base-rtd (fasl-target)) x p)))
  (set! fasl-write (case-lambda
                    [(x p) ((target-fasl-write (fasl-target)) x p)]
                    [(x p externals) ((target-fasl-write (fasl-target)) x p externals)]
                    [(x p externals omit-rtds?) ((target-fasl-write (fasl-target)) x p externals omit-rtds?)]))
  (set! fasl-file (lambda (in out) ((target-fasl-file (fasl-target)) in out))))

(when ($unbound-object? (#%$top-level-value '$capture-fasl-target))
  (let ([ht (make-hashtable values =)])
    (set! $capture-fasl-target
      (lambda (mt)
        (hashtable-set! ht mt ($fasl-target))))
    (set-who! $with-fasl-target
      (lambda (mt th)
        (cond
          [(hashtable-ref ht mt #f) =>
           (lambda (target)
             (parameterize ([$fasl-target target])
               (th)))]
          [else ($oops who "unrecognized machine type ~s" mt)])))))
  
($capture-fasl-target (constant machine-type))
)
