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
(define rtd-parent (csv7:record-field-accessor #!base-rtd 'parent))
(define rtd-name (csv7:record-field-accessor #!base-rtd 'name))
(define rtd-uid (csv7:record-field-accessor #!base-rtd 'uid))
(define rtd-flags (csv7:record-field-accessor #!base-rtd 'flags))

(define-record-type table
  (fields (mutable count) (immutable hash))
  (nongenerative)
  (sealed #t)
  (protocol
    (lambda (new)
      (lambda ()
        (new 0 (make-eq-hashtable))))))

(include "fasl-helpers.ss")

(define bld-pair
   (lambda (x t a?)
      (bld (car x) t a?)
      (bld (cdr x) t a?)))

(define bld-vector
   (lambda (x t a?)
      (let ([len (vector-length x)])
         (let bldvec ([i 0])
            (unless (fx= i len)
               (bld (vector-ref x i) t a?)
               (bldvec (fx+ i 1)))))))

(define bld-record
  (lambda (x t a?)
    (unless (eq? x #!base-rtd)
      (when (record-type-descriptor? x)
        ; fasl representation for record-type-descriptor includes uid separately and as part of the record
        (bld (record-type-uid x) t a?))
      (really-bld-record x t a?))))

(define really-bld-record
  (lambda (x t a?)
    (let ([rtd ($record-type-descriptor x)])
      (bld rtd t a?)
      (do ([flds (rtd-flds rtd) (cdr flds)] [i 0 (+ i 1)])
        ((null? flds))
        (when (memq (fld-type (car flds)) '(scheme-object ptr))
          (bld ((csv7:record-field-accessor rtd i) x) t a?))))))

(define bld-ht
  (lambda (x t a?)
    (let-values ([(keyvec valvec) (hashtable-entries x)])
      (vector-for-each
        (lambda (key val)
          (bld key t a?)
          (bld val t a?))
        keyvec valvec))))

(define bld-box
   (lambda (x t a?)
      (bld (unbox x) t a?)))

(define bld-simple
   (lambda (x t a?)
      (void)))

(module (bld-graph dump-graph reset-dump-graph)
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
    (lambda (x t a? handler)
      (let ([a (eq-hashtable-cell (table-hash t) x 'first)])
        (let ([p (cdr a)])
          (cond
            [(eq? p 'first)
             #;(let ([n (hashtable-size (table-hash t))])
                 (when (fx= (modulo n 10000) 0)
                   (printf "entries = ~s, ba = ~s, count = ~s\n" n (bytes-allocated) (table-count t))))
             (record! ventry x)
             (set-cdr! a #f)
             (handler x t a?)]
            [(not p)
             (record! vdup x)
             (let ([n (table-count t)])
               (set-cdr! a (cons n #t))
               (table-count-set! t (fx+ n 1)))])))))
  (reset-dump-graph))

(define bld
   (lambda (x t a?)
      (cond
        [(pair? x) (bld-graph x t a? bld-pair)]
        [(vector? x) (bld-graph x t a? bld-vector)]
        [(or (symbol? x) (string? x)) (bld-graph x t a? bld-simple)]
        ; this check must go before $record? check
        [(and (annotation? x) (not a?))
         (bld (annotation-stripped x) t a?)]
        ; this check must go before $record? check
        [(eq-hashtable? x) (bld-graph x t a? bld-ht)]
        ; this check must go before $record? check
        [(symbol-hashtable? x) (bld-graph x t a? bld-ht)]
        [($record? x) (bld-graph x t a? bld-record)]
        [(box? x) (bld-graph x t a? bld-box)]
        [(or (large-integer? x) (ratnum? x) ($inexactnum? x) ($exactnum? x)
             (fxvector? x) (bytevector? x))
         (bld-graph x t a? bld-simple)])))

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
    (put-u8 p (if (immutable-fxvector? x)
                  (constant fasl-type-immutable-fxvector)
                  (constant fasl-type-fxvector)))
    (let ([n (fxvector-length x)])
      (put-uptr p n)
      (let wrf-fxvector-loop ([i 0])
        (unless (fx= i n)
          (put-iptr p (fxvector-ref x i))
          (wrf-fxvector-loop (fx+ i 1)))))))

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

; Written as: fasl-tag rtd field ...
(module (wrf-record really-wrf-record wrf-annotation)
  (define maybe-remake-rtd
    (lambda (rtd)
      (if (eq? (machine-type) ($target-machine))
          rtd
          ($remake-rtd rtd (let () (include "layout.ss") compute-field-offsets)))))

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
        (lambda (target-fld pad val)
          (define put-i64
            (lambda (p val)
              (constant-case ptr-bits
                [(32) (put-iptr p (bitwise-arithmetic-shift-right val 32)) (put-uptr p (logand val #xffffffff))]
                [(64) (put-iptr p val)])))
          (define-syntax put-padty
            (syntax-rules ()
              [(_ fasl-fld-type)
               (put-u8 p (fxlogor (fxsll pad 4) (constant fasl-fld-type)))]))
          (let ([type (fld-type target-fld)] [addr (fld-byte target-fld)])
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
             [target-rtd (maybe-remake-rtd host-rtd)]
             [target-fld* (rtd-flds target-rtd)])
        (put-uptr p (rtd-size target-rtd))
        (put-uptr p (length target-fld*))
        (wrf host-rtd p t a?)
        (fold-left
          (lambda (last-target-addr host-fld target-fld)
            (let ([val (get-field host-fld)])
              (check-field target-fld val)
              (let ([target-addr (fld-byte target-fld)])
                (fx+ target-addr (put-field host-fld (fx- target-addr last-target-addr) val)))))
          (constant record-data-disp)
          (rtd-flds host-rtd)
          target-fld*))))

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
         (wrf-fields (maybe-remake-rtd x) p t a?)]
        [else
         (put-u8 p (constant fasl-type-record))
         (wrf-fields x p t a?)])))

  (define wrf-annotation
    (lambda (x p t a?)
      (define maybe-remake-annotation
        (lambda (x a?)
          (if (fx= (annotation-flags x) a?)
              x
              (make-annotation (annotation-expression x) (annotation-source x) (annotation-stripped x) a?))))
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
   (lambda (x p)
     (put-u8 p (constant fasl-type-flonum))
     (let ([n ($object-ref 'unsigned-64 x (constant flonum-data-disp))])
       (put-uptr p (ash n -32))
       (put-uptr p (logand n #xFFFFFFFF)))))

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

(define wrf
   (lambda (x p t a?)
      (cond
         [(symbol? x) (wrf-graph x p t a? wrf-symbol)]
         [(pair? x) (wrf-graph x p t a? wrf-pair)]
         [(small-integer? x) (wrf-small-integer x p t a?)]
         [(null? x) (wrf-immediate (constant snil) p)]
         [(not x) (wrf-immediate (constant sfalse) p)]
         [(eq? x #t) (wrf-immediate (constant strue) p)]
         [(string? x) (wrf-graph x p t a? wrf-string)]
         [(fxvector? x) (wrf-graph x p t a? wrf-fxvector)]
         [(bytevector? x) (wrf-graph x p t a? wrf-bytevector)]
         ; this check must go before $record? check
         [(annotation? x)
          (if a?
              (wrf-graph x p t a? wrf-annotation)
              (wrf (annotation-stripped x) p t a?))]
         ; this check must go before $record? check
         [(eq-hashtable? x) (wrf-graph x p t a? wrf-eqht)]
         ; this check must go before $record? check
         [(symbol-hashtable? x) (wrf-graph x p t a? wrf-symht)]
         ; this check must go before $record? check
         [(hashtable? x) ($oops 'fasl-write "invalid fasl object ~s" x)]
         [($record? x) (wrf-graph x p t a? wrf-record)]
         [(vector? x) (wrf-graph x p t a? wrf-vector)]
         [(char? x) (wrf-char x p)]
         [(box? x) (wrf-graph x p t a? wrf-box)]
         [(large-integer? x) (wrf-graph x p t a? wrf-large-integer)]
         [(ratnum? x) (wrf-graph x p t a? wrf-ratnum)]
         [(flonum? x) (wrf-flonum x p)]
         [($inexactnum? x) (wrf-graph x p t a? wrf-inexactnum)]
         [($exactnum? x) (wrf-graph x p t a? wrf-exactnum)]
         [(eof-object? x) (wrf-immediate (constant seof) p)]
         [(bwp-object? x) (wrf-immediate (constant sbwp) p)]
         [($unbound-object? x) (wrf-immediate (constant sunbound) p)]
         [(eq? x (void)) (wrf-immediate (constant svoid) p)]
         [(eq? x '#0=#0#) (wrf-immediate (constant black-hole) p)]
         [($rtd-counts? x) (wrf-immediate (constant sfalse) p)]
         [else ($oops 'fasl-write "invalid fasl object ~s" x)])))

(define start
  (lambda (p t situation proc)
    (define (append-bvs bv*)
      (let f ([bv* bv*] [n 0])
        (if (null? bv*)
            (if (fixnum? n)
                (make-bytevector n)
                ($oops 'fasl-write "fasl output is too large to compress"))
            (let ([bv1 (car bv*)])
              (let ([m (bytevector-length bv1)])
                (let ([bv2 (f (cdr bv*) (+ n m))])
                  (bytevector-copy! bv1 0 bv2 n m)
                  bv2))))))
    (dump-graph)
    (let-values ([(bv* size)
                  (let-values ([(p extractor) ($open-bytevector-list-output-port)])
                    (let ([n (table-count t)])
                      (unless (fx= n 0)
                        (put-u8 p (constant fasl-type-graph))
                        (put-uptr p n)))
                    (proc p)
                    (extractor))])
      (put-u8 p situation)
      (if (and (>= size 100) (fasl-compressed))
          (let* ([fmt ($tc-field 'compress-format ($tc))]
                 [bv (append-bvs bv*)]
                 [uncompressed-size-bv (call-with-bytevector-output-port (lambda (bvp) (put-uptr bvp (bytevector-length bv))))]
                 [bv ($bytevector-compress bv fmt)])
            (put-uptr p (+ 1 (bytevector-length uncompressed-size-bv) (bytevector-length bv)))
            (put-u8 p 
              (cond
                [(eqv? fmt (constant COMPRESS-GZIP)) (constant fasl-type-gzip)]
                [(eqv? fmt (constant COMPRESS-LZ4)) (constant fasl-type-lz4)]
                [else ($oops 'fasl-write "unexpected $compress-format value ~s" fmt)]))
            (put-bytevector p uncompressed-size-bv)
            (put-bytevector p bv))
          (begin
            (put-uptr p (+ size 1))
            (put-u8 p (constant fasl-type-uncompressed))
            (for-each (lambda (bv) (put-bytevector p bv)) bv*))))))

(module (fasl-write fasl-file)
  ; when called from fasl-write or fasl-file, always preserve annotations;
  ; otherwise use value passed in by the compiler
  (define fasl-one
    (lambda (x p)
      (let ([t (make-table)])
        (bld x t (constant annotation-all))
        (start p t (constant fasl-type-visit-revisit) (lambda (p) (wrf x p t (constant annotation-all)))))))

  (define-who fasl-write
    (lambda (x p)
      (unless (and (output-port? p) (binary-port? p))
        ($oops who "~s is not a binary output port" p))
      (when ($port-flags-set? p (constant port-flag-compressed)) ($compressed-warning who p))
      (emit-header p (constant scheme-version) (constant machine-type-any))
      (fasl-one x p)))

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
                  (fasl-one x op)
                  (fasl-loop)))))
          (close-port op))
        (close-port ip)))))

(define fasl-base-rtd
  (lambda (x p)
    (emit-header p (constant scheme-version) (constant machine-type-any))
    (let ([t (make-table)])
      (bld-graph x t #f really-bld-record)
      (start p t (constant fasl-type-visit-revisit) (lambda (p) (wrf-graph x p t #f really-wrf-record))))))

($fasl-target (make-target bld-graph bld wrf start make-table wrf-graph fasl-base-rtd fasl-write fasl-file))
)

(let ()
  (define fasl-target
    (lambda ()
      (let ([target ($fasl-target)])
        (assert target)
        target)))
  (set! $fasl-bld-graph (lambda (x t a? handler) ((target-fasl-bld-graph (fasl-target)) x t a? handler)))
  (set! $fasl-enter (lambda (x t a?) ((target-fasl-enter (fasl-target)) x t a?)))
  (set! $fasl-out (lambda (x p t a?) ((target-fasl-out (fasl-target)) x p t a?)))
  (set! $fasl-start (lambda (p t situation proc) ((target-fasl-start (fasl-target)) p t situation proc)))
  (set! $fasl-table (lambda () ((target-fasl-table (fasl-target)))))
  (set! $fasl-wrf-graph (lambda (x p t a? handler) ((target-fasl-wrf-graph (fasl-target)) x p t a? handler)))
  (set! $fasl-base-rtd (lambda (x p) ((target-fasl-base-rtd (fasl-target)) x p)))
  (set! fasl-write (lambda (x p) ((target-fasl-write (fasl-target)) x p)))
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
