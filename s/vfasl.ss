;; vfasl conversion uses the fasl parser from "strip.ss"; it creates
;; an image of the memory that fasl_in from "fasl.c" would create

(let ()

(include "strip-types.ss")

;; ************************************************************
;; Encode-time data structures                              */

;; During encoding, we use a bytevector per vspace on first pass,
;; single shared bytevector on the second pass
(define-record-type vfasl-chunk
  (fields (mutable bv)
          (mutable offset) ; offset into bv
          (mutable alloc) ; allocation pointer; implies size
          limit) ; #f or a sanity-check limit
  (nongenerative))

(define-record-type vfasl-info
  (fields (mutable bv)

          (mutable base-addr) ; index within bv to make pointers and relocations relative to

          (mutable sym-count)

          (mutable symref-count)
          (mutable symrefs)  ; offset into bv

          (mutable rtdref-count)
          (mutable rtdrefs)  ; offset into bv

          (mutable singletonref-count)
          (mutable singletonrefs) ; offset into bv

          spaces ; vector of vfasl-chunk

          (mutable ptr-bitmap) ; #f or offset into bv

          (mutable graph)
          (mutable base-rtd)   ; write base-rtd only once

          (mutable symbols)    ; intern symbols (because multiple fasl blocks may be combined)
          (mutable rtds)       ; intern rtds (same reason)
          (mutable strings)    ; intern certain strings (for code names)

          (mutable installs-library-entry?)) ; to determine whether vfasls can be combined
  (nongenerative))

(define (new-vfasl-info)
  (make-vfasl-info #f

                   0
                   0 ; sym-count

                   0 ;symref-count
                   #f

                   0 ; rtdref-count
                   #f

                   0 ; singletonref-count
                   #f

                   (list->vector
                    (let loop ([i 0])
                      (if (fx= i (constant vspaces-count))
                          '()
                          (cons (make-vfasl-chunk '#vu8() 0 0 #f)
                                (loop (fx+ i 1))))))
                   #f ; ptr-bitmap

                   (make-eq-hashtable)
                   #f
                   (make-eq-hashtable)
                   (make-eq-hashtable)
                   (make-hashtable string-hash string=?)

                   #f)) ; installs-library-entry?

;; Creates a vfasl image for the fasl content `v` (as read by "strip.ss").
;; The target endianness must be statically known.
(define (to-vfasl v)
  (constant-case native-endianness
    [(unknown) ($oops 'vfasl "cannot vfasl with unknown endianness")]
    [else (void)])
  (let ([v (ensure-reference v)]
        [vfi (new-vfasl-info)])
    ;; First pass: determine sizes
    (copy v vfi)

    ;; Setup for second pass: allocate to contiguous bytes
    (let* ([data-size (let loop ([i 0])
                        (if (fx= i (constant vspaces-count))
                            0
                            (fx+ (vfasl-chunk-alloc
                                  (vector-ref (vfasl-info-spaces vfi) i))
                                 (loop (fx+ i 1)))))]
           [table-size (fx+ (fx* (vfasl-info-symref-count vfi) (constant ptr-bytes))
                            (fx* (vfasl-info-rtdref-count vfi) (constant ptr-bytes))
                            (fx* (vfasl-info-singletonref-count vfi) (constant ptr-bytes)))]
           [bitmap-size (fxsra (fx+ data-size (fx- (constant byte-bits) 1)) (constant log2-byte-bits))]
           [size (fx+ (constant size-vfasl-header)
                      data-size
                      table-size
                      bitmap-size)]
           [bv (make-bytevector size 0)])
      (vfasl-info-bv-set! vfi bv)

      ;; write header, except for result offset and table size:
      (set-uptr! bv (constant vfasl-header-data-size-disp) data-size)
      (let loop ([i 1] [offset (vfasl-chunk-alloc
                                (vector-ref (vfasl-info-spaces vfi) 0))])
        (unless (fx= i (constant vspaces-count))
          (set-uptr! bv
                     (fx+ (constant vfasl-header-vspace-rel-offsets-disp)
                          (fx* (fx- i 1) (constant ptr-bytes)))
                     offset)
          (loop (fx+ i 1) (fx+ offset (vfasl-chunk-alloc
                                       (vector-ref (vfasl-info-spaces vfi) i))))))
      (set-uptr! bv (constant vfasl-header-symref-count-disp) (vfasl-info-symref-count vfi))
      (set-uptr! bv (constant vfasl-header-rtdref-count-disp) (vfasl-info-rtdref-count vfi))
      (set-uptr! bv (constant vfasl-header-singletonref-count-disp) (vfasl-info-singletonref-count vfi))

      (let ([base-addr (constant size-vfasl-header)])
        (vfasl-info-base-addr-set! vfi base-addr)

        (let* ([p
                ;; Set pointers to vspaces based on sizes from first pass
                (let loop ([i 0] [p base-addr])
                  (if (fx= i (constant vspaces-count))
                      p
                      (let ([len (vfasl-chunk-alloc
                                  (vector-ref (vfasl-info-spaces vfi) i))])
                        (vector-set! (vfasl-info-spaces vfi) i (make-vfasl-chunk bv p 0 len))
                        (loop (fx+ i 1) (fx+ p len)))))]
               [p (begin
                    (vfasl-info-symrefs-set! vfi p)
                    (fx+ p (fx* (vfasl-info-symref-count vfi) (constant ptr-bytes))))]
               [p (begin
                    (vfasl-info-rtdrefs-set! vfi p)
                    (fx+ p (fx* (vfasl-info-rtdref-count vfi) (constant ptr-bytes))))]
               [p (begin
                    (vfasl-info-singletonrefs-set! vfi p)
                    (fx+ p (fx* (vfasl-info-singletonref-count vfi) (constant ptr-bytes))))]
               [bm p])
          (vfasl-info-ptr-bitmap-set! vfi bm)

          (vfasl-info-sym-count-set! vfi 0)
          (vfasl-info-symref-count-set! vfi 0)
          (vfasl-info-rtdref-count-set! vfi 0)
          (vfasl-info-singletonref-count-set! vfi 0)
          (vfasl-info-graph-set! vfi (make-eq-hashtable))
          (vfasl-info-base-rtd-set! vfi #f)

          ;; Write data
          (let ([v (copy v vfi)])
            (let-values ([(bv offset) (vptr->bytevector+offset v vfi)])
              (set-iptr! bv (constant vfasl-header-result-offset-disp) (- offset base-addr)))

            ;; We can ignore trailing zeros in the bitmap:
            (let* ([zeros (let loop ([bmp (fx+ bm bitmap-size)] [zeros 0])
                            (cond
                              [(fx= bmp bm) zeros]
                              [(fx= 0 (bytevector-u8-ref bv (fx- bmp 1)))
                               (loop (fx- bmp 1) (fx+ zeros 1))]
                              [else zeros]))]
                   [table-size (fx+ table-size (fx- bitmap-size zeros))])
              (set-uptr! bv (constant vfasl-header-table-size-disp) table-size)
              ;; Truncate bytevector to match end of bitmaps
              (bytevector-truncate! bv (fx- size zeros)))

            (sort-offsets! bv (vfasl-info-symrefs vfi) (vfasl-info-symref-count vfi))
            (sort-offsets! bv (vfasl-info-rtdrefs vfi) (vfasl-info-rtdref-count vfi))
            (sort-offsets! bv (vfasl-info-singletonrefs vfi) (vfasl-info-singletonref-count vfi))

            bv))))))

;; If compiled code uses `$install-library-entry`, then it can't be
;; combined into a single vfasled object, because the installation
;; needs to be evaluated for laster vfasls. Recognize a non-combinable
;; value as anything that references the C entry or even mentions the
;; symbol `$install-library-entry` (as defined in "library.ss"). If
;; non-boot code mentions the symbol `$install-library-entry`, it just
;; isn't as optimal.
;;
;; This is an expensive test, since we perform half of a vfasl
;; encoding to look for `$install-library-entry`. */
(define (fasl-can-combine? v)
  (constant-case native-endianness
    [(unknown) ($oops 'vfasl "cannot vfasl with unknown endianness")]
    [else (void)])
  (let ([vfi (new-vfasl-info)])
    ;; Run a "first pass"
    (copy v vfi)
    (not (vfasl-info-installs-library-entry? vfi))))

;; Box certain kinds of values (including singletons) where the vfasl
;; process needs a pointer into data
(define (ensure-reference v)
  (define (enbox v)
    (fasl-tuple (constant fasl-type-box) (vector v)))
  (define (enbox-fixnum n)
    (if (<= (constant most-negative-fixnum) n (constant most-positive-fixnum))
        (enbox v)
        v))
  (fasl-case* v
    [(atom ty uptr)
     (constant-case* ty
       [(fasl-type-immediate fasl-type-base-rtd) (enbox v)]
       [else v])]
    [(small-integer iptr) (enbox-fixnum iptr)]
    [(large-integer sign vuptr) (enbox-fixnum (build-exact-integer sign vuptr))]
    [(tuple ty vec)
     (constant-case* ty
       [(fasl-type-box) (enbox v)]
       [else v])]
    [(string ty string)
     (constant-case* ty
       [(fasl-type-symbol) (enbox v)]
       [else
        (if (fx= 0 (string-length string))
            (enbox v)
            v)])]
    [(vector ty vec)
     (if (fx= 0 (vector-length vec))
         (enbox v)
         v)]
    [(fxvector vec)
     (if (fx= 0 (vector-length vec))
         (enbox v)
         v)]
    [(bytevector ty bv)
     (if (fx= 0 (bytevector-length bv))
         (enbox v)
         v)]
    [(record maybe-uid size nflds rtd pad-ty* fld*)
     (enbox v)]
    [else v]))

;; quicksort on uptrs within a bytevector
(define (sort-offsets! bv offset len)
  (define (uref i)
    (ref-uptr bv (fx+ offset (fx* i (constant ptr-bytes)))))
  (define (uset! i v)
    (set-uptr! bv (fx+ offset (fx* i (constant ptr-bytes))) v))
  (when (fx> len 1)
    (let* ([mid (fxsra len 1)]
           [tmp (uref mid)])
      (uset! mid (uref 0))
      (uset! 0 tmp))
    (let ([p-val (uref 0)])
      (let loop ([i 1] [pivot 0])
        (cond
          [(fx= i len)
           (uset! pivot p-val)
           (sort-offsets! bv offset pivot)
           (sort-offsets! bv (fx+ offset (fx* (fx+ pivot 1) (constant ptr-bytes))) (fx- len pivot 1))]
          [(< (uref i) p-val)
           (uset! pivot (uref i))
           (let ([pivot (fx+ pivot 1)])
             (uset! i (uref pivot))
             (loop (fx+ i 1) pivot))]
          [else
           (loop (fx+ i 1) pivot)])))))

;; ----------------------------------------

;; A vptr represents a pointer to an object allocated in a vfasl image.
;; A vsingleton represents a pointer to a single (not in the image).
;; A number a pointer represents a literal pointer, such as a fixnum or immediate.

(define (make-vptr v vspace) (cons v vspace))
(define (make-vsingleton n) (cons n 'singleton))

(define (vptr? v) (and (pair? v) (not (eq? (cdr v) 'singleton))))
(define (vptr-v v) (car v))
(define (vptr-vspace v) (cdr v))
(define (vptr+ v offset) (make-vptr (fx+ (vptr-v v) offset) (vptr-vspace v)))

(define (vsingleton? v) (and (pair? v) (eq? (cdr v) 'singleton)))
(define (vsingleton-index v) (car v))

(define (segment-start? sz)
  (fxzero? (fxand sz (fx- (constant bytes-per-segment) 1))))
(define (segment-truncate sz)
  (fxand sz (fxnot (fx- (constant bytes-per-segment) 1))))

;; Allocate into the given vspace in a vfasl image. The result
;; is just the `v` part of a vptr (because it's easier to do arithmetic
;; with that to initialize the item).
(define (find-room who vfi vspc n type)
  (let ([n (c-alloc-align n)]
        [vc (vector-ref (vfasl-info-spaces vfi) vspc)])
    (constant-case* vspc
      [(vspace-symbol vspace-impure-record)
       ;; For these spaces, in case they will be loaded into the static
       ;; generation, objects must satisfy an extra constraint: an object
       ;; must not span segments unless it's at the start of a
       ;; segment
       (let ([sz (vfasl-chunk-alloc vc)])
         (unless (segment-start? sz)
           ;; Since we're not at the start of a segment, don't let an
           ;; object span a segment
           (when (and (not (fx= (segment-truncate sz) (segment-truncate (fx+ sz n))))
                      (not (segment-start? (fx+ sz n))))
             ;; Skip to next segment
             (vfasl-chunk-alloc-set! vc (segment-truncate (fx+ sz n))))))]
      [else (void)])
    (let* ([sz (vfasl-chunk-alloc vc)]
           [new-sz (fx+ sz n)]
           [limit (vfasl-chunk-limit vc)])
      (when (and limit
                 (fx> new-sz limit))
        ($oops 'vfasl "allocation overrun"))
      (when (fx< (bytevector-length (vfasl-chunk-bv vc)) new-sz)
        (let ([bv (make-bytevector (fx+ (if (fxzero? sz)
                                            (constant bytes-per-segment)
                                            (fx* 2 (bytevector-length (vfasl-chunk-bv vc))))
                                        (segment-truncate n)))])
          (bytevector-copy! (vfasl-chunk-bv vc) 0 bv 0 sz)
          (vfasl-chunk-bv-set! vc bv)))
      (vfasl-chunk-alloc-set! vc new-sz)
      (make-vptr (fx- sz (fx- (constant typemod) type))
                 vspc))))

(define vptr->bytevector+offset
  (case-lambda
   [(p vfi) (vptr->bytevector+offset p 0 vfi)]
   [(p delta vfi)
    (let ([vc (vector-ref (vfasl-info-spaces vfi) (vptr-vspace p))])
      (values (vfasl-chunk-bv vc) (fx+ (vfasl-chunk-offset vc) (vptr-v p) delta)))]))

;; Overloaded to either set in a bytevector or set in a vfasl image:
(define set-uptr!
  (case-lambda
   [(bv i uptr)
    (constant-case ptr-bytes
      [(4) (bytevector-u32-set! bv i uptr (constant native-endianness))]
      [(8) (bytevector-u64-set! bv i uptr (constant native-endianness))])]
   [(p delta uptr vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (set-uptr! bv offset uptr))]))

;; Overloaded in the same way as `set-uptr!`
(define ref-uptr
  (case-lambda
   [(bv i)
    (constant-case ptr-bytes
      [(4) (bytevector-u32-ref bv i (constant native-endianness))]
      [(8) (bytevector-u64-ref bv i (constant native-endianness))])]
   [(p delta vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (ref-uptr bv offset))]))

;; Overloaded in the same way as `set-uptr!`
(define set-iptr!
  (case-lambda
   [(bv i uptr)
    (constant-case ptr-bytes
      [(4) (bytevector-s32-set! bv i uptr (constant native-endianness))]
      [(8) (bytevector-s64-set! bv i uptr (constant native-endianness))])]
   [(p delta uptr vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (set-iptr! bv offset uptr))]))

;; Overloaded in the same way as `set-uptr!`
(define set-double!
  (case-lambda
   [(bv i dbl)
    (bytevector-ieee-double-set! bv i dbl (constant native-endianness))]
   [(p delta dbl vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (set-double! bv offset dbl))]))

;; Overloaded in the same way as `set-uptr!`
(define set-int32!
  (case-lambda
   [(bv i uptr)
    (bytevector-s32-set! bv i uptr (constant native-endianness))]
   [(p delta uptr vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (set-int32! bv offset uptr))]))

;; Overloaded in the same way as `set-uptr!`
(define set-char!
  (case-lambda
   [(bv i char)
    (let ([n (bitwise-ior (bitwise-arithmetic-shift-left (char->integer char) (constant char-data-offset))
                          (constant type-char))])
      (constant-case string-char-bytes
        [(4) (bytevector-u32-set! bv i n (constant native-endianness))]))]
   [(p delta char vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (set-char! bv offset char))]))

(define set-u8!
  (case-lambda
   [(p delta u8 vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (bytevector-u8-set! bv offset u8))]))

(define (copy-u8s! p delta bv bv-off len vfi)
  (let-values ([(dest-bv offset) (vptr->bytevector+offset p delta vfi)])
    (bytevector-copy! bv bv-off dest-bv offset len)))

;; Overloaded in the same way as `set-uptr!`
(define set-bigit!
  (case-lambda
   [(bv i bigit)
    (constant-case bigit-bytes
      [(4) (bytevector-u32-set! bv i bigit (constant native-endianness))])]
   [(p delta bigit vfi)
    (let-values ([(bv offset) (vptr->bytevector+offset p delta vfi)])
      (set-bigit! bv offset bigit))]))

;; Sets a pointer in a vfasl image, and optionally records the reference.
;; The pointer is written as a relative offset, and then it will get
;; adjusted when the vfasl image is loaded.
(define (do-set-ptr! at-p delta p vfi record?)
  (let* ([vc (vector-ref (vfasl-info-spaces vfi) (vptr-vspace at-p))]
         [rel-v (fx- (fx+ (vptr-v at-p) delta (vfasl-chunk-offset vc))
                     (vfasl-info-base-addr vfi))])
    (define (register! vfasl-info-ref-count
                       vfasl-info-ref-count-set!
                       vfasl-info-refs)
      (unless record? ($oops 'vfasl "expected to record ptr"))
      (let ([c (vfasl-info-ref-count vfi)]
            [refs (vfasl-info-refs vfi)])
        (vfasl-info-ref-count-set! vfi (fx+ c 1))
        (when refs
          (set-uptr! (vfasl-info-bv vfi) (fx+ refs (fx* c (constant ptr-bytes))) rel-v))))
    (let ([val (cond
                 [(vptr? p)
                  (let* ([p-vspc (vptr-vspace p)]
                         [p-vc (vector-ref (vfasl-info-spaces vfi) p-vspc)])
                    (constant-case* p-vspc
                      [(vspace-symbol)
                       (when record?
                         (register! vfasl-info-symref-count
                                    vfasl-info-symref-count-set!
                                    vfasl-info-symrefs))
                       ;; symbol reference are not registered in the bitmap,
                       ;; and the reference is as an index instead of address offset
                       (fix (symbol-vptr->index p vfi))]
                      [else
                       (when record?
                         (when (eqv? p-vspc (constant vspace-rtd))
                           (register! vfasl-info-rtdref-count
                                      vfasl-info-rtdref-count-set!
                                      vfasl-info-rtdrefs))
                         (let ([bm (vfasl-info-ptr-bitmap vfi)])
                           (when bm
                             (safe-assert (fxzero? (fxand rel-v (fx- (constant ptr-bytes) 1))))
                             (let* ([w-rel-b (fxsra rel-v (constant log2-ptr-bytes))]
                                    [i (fx+ bm (fxsra w-rel-b (constant log2-byte-bits)))]
                                    [bit (fxsll 1 (fxand w-rel-b (fx- (constant byte-bits) 1)))]
                                    [bv (vfasl-info-bv vfi)])
                               (bytevector-u8-set! bv i (fxior (bytevector-u8-ref bv i) bit))))))
                       (fx- (fx+ (vptr-v p) (vfasl-chunk-offset p-vc))
                            (vfasl-info-base-addr vfi))]))]
                 [(vsingleton? p)
                  (register! vfasl-info-singletonref-count
                             vfasl-info-singletonref-count-set!
                             vfasl-info-singletonrefs)
                  (fix (vsingleton-index p))]
                 [else p])])
      (set-iptr! at-p delta val vfi))))

(define (set-ptr! at-p delta p vfi) (do-set-ptr! at-p delta p vfi #t))
(define (set-ptr!/no-record at-p delta p vfi) (do-set-ptr! at-p delta p vfi #f))

(define (symbol-vptr->index p vfi)
  ;; There may be leftover space at the end of each segment containing symbols,
  ;; we we have to compensate for that
  (let* ([vc (vector-ref (vfasl-info-spaces vfi) (constant vspace-symbol))]
         [offset (fx+ (vptr-v p) (fx- (constant typemod) (constant type-symbol)))]
         [seg (quotient offset (constant bytes-per-segment))])
    (fx+ (fx* seg (quotient (constant bytes-per-segment) (constant size-symbol)))
         (fxquotient (fx- offset (fx* seg (constant bytes-per-segment))) (constant size-symbol)))))

(define (build-exact-integer sign vuptr)
  (let loop ([v 0] [i 0])
    (cond
      [(fx= i (vector-length vuptr))
       (if (eqv? sign 1) (- v) v)]
      [else (loop (bitwise-ior (bitwise-arithmetic-shift v (constant bigit-bits))
                                   (vector-ref vuptr i))
                  (fx+ i 1))])))

(define (build-flonum high low)
  (let ([bv (make-bytevector 8)])
    (bytevector-u64-native-set! bv 0 (bitwise-ior low (bitwise-arithmetic-shift high 32)))
    (bytevector-ieee-double-native-ref bv 0)))

(define (unpack-flonum v)
  (fasl-case* v
    [(flonum high low) (build-flonum high low)]
    [else ($oops 'vfasl "expected a flonum")]))

(define (unpack-symbol v)
  (or (fasl-case* v
        [(string ty string)
         (if (eq? ty (constant fasl-type-symbol))
             (string->symbol string)
             #f)]
        [(gensym pname uname) (gensym pname uname)]
        [else #f])
      (error 'vfasl "expected a symbol: ~s" v)))

;; ----------------------------------------

(define rtd-flds (csv7:record-field-accessor #!base-rtd 'flds))

(define (fix v)
  (bitwise-arithmetic-shift-left v (constant fixnum-offset)))
(define (fixed? v)
  (fxzero? (bitwise-and v (sub1 (fxsll 1 (constant fixnum-offset))))))

(define (graph! v new-p vfi)
  (eq-hashtable-set! (vfasl-info-graph vfi) v new-p))

(define (copy v vfi)
  (or (eq-hashtable-ref (vfasl-info-graph vfi) v #f)
      (do-copy v vfi)))

(define (do-copy v vfi)
  (fasl-case* v
    [(atom ty uptr)
     (constant-case* ty
       [(fasl-type-immediate) uptr]
       [(fasl-type-entry fasl-type-library fasl-type-library-code)
        ($oops 'vfasl "expected only in a relocation: ~s" v)]
       [else ($oops 'vfasl "unknown atom: ~s" v)])]
    [(small-integer iptr) (exact-integer-copy v iptr vfi)]
    [(large-integer sign vuptr)
     (exact-integer-copy v (build-exact-integer sign vuptr) vfi)]
    [(flonum high low)
     (let ([new-p (find-room 'flonum vfi
                             (constant vspace-data)
                             (constant size-flonum)
                             (constant type-flonum))])
       (graph! v new-p vfi)
       (set-double! new-p (constant flonum-data-disp) (build-flonum high low) vfi)
       new-p)]
    [(pair vec)
     (let ([len (vector-length vec)]
           [vspc (constant vspace-impure)])
       (cond
         [(fx= len 1) (copy (vector-ref vec 0) vfi)]
         [else
          ;; can't just use `pair-copy` for initial pair, because we need
          ;; to set up the graph:
          (let ([new-p (find-room 'pair vfi
                                  (constant vspace-impure)
                                  (constant size-pair)
                                  (constant type-pair))])
            (graph! v new-p vfi)
            (set-ptr! new-p (constant pair-car-disp) (copy (vector-ref vec 0) vfi) vfi)
            (let ([d (let loop ([i 1])
                       (let ([e (copy (vector-ref vec i) vfi)]
                             [i (fx+ i 1)])
                         (if (fx= i len)
                             e
                             (pair-copy e (loop i) vfi))))])
              (set-ptr! new-p (constant pair-cdr-disp) d vfi)
              new-p))]))]
    [(tuple ty vec)
     (constant-case* ty
       [(fasl-type-base-rtd) (base-rtd-copy v vfi)]
       [(fasl-type-box fasl-type-immutable-box)
        (let ([new-p (find-room 'box vfi
                                (constant vspace-impure)
                                (constant size-box)
                                (constant type-typed-object))])
          (graph! v new-p vfi)
          (set-uptr! new-p (constant box-type-disp)
                     (if (eqv? ty (constant fasl-type-immutable-box))
                         (constant type-immutable-box)
                         (constant type-box))
                     vfi)
          (set-ptr! new-p (constant box-ref-disp) (copy (vector-ref vec 0) vfi) vfi)
          new-p)]
       [(fasl-type-ratnum)
        (let ([new-p (find-room 'ratnum vfi
                                (constant vspace-impure)
                                (constant size-ratnum)
                                (constant type-typed-object))])
          (graph! v new-p vfi)
          (set-uptr! new-p (constant ratnum-type-disp) (constant type-ratnum) vfi)
          (set-ptr! new-p (constant ratnum-numerator-disp) (copy (vector-ref vec 0) vfi) vfi)
          (set-ptr! new-p (constant ratnum-denominator-disp) (copy (vector-ref vec 1) vfi) vfi)
          new-p)]
       [(fasl-type-exactnum)
        (let ([new-p (find-room 'exactnum vfi
                                (constant vspace-impure)
                                (constant size-exactnum)
                                (constant type-typed-object))])
          (graph! v new-p vfi)
          (set-uptr! new-p (constant exactnum-type-disp) (constant type-exactnum) vfi)
          (set-ptr! new-p (constant exactnum-real-disp) (copy (vector-ref vec 0) vfi) vfi)
          (set-ptr! new-p (constant exactnum-imag-disp) (copy (vector-ref vec 1) vfi) vfi)
          new-p)]
       [(fasl-type-inexactnum)
        (let ([new-p (find-room 'inexactnum vfi
                                (constant vspace-data)
                                (constant size-inexactnum)
                                (constant type-typed-object))])
          (graph! v new-p vfi)
          (set-uptr! new-p (constant inexactnum-type-disp) (constant type-inexactnum) vfi)
          (set-double! new-p (constant inexactnum-real-disp) (unpack-flonum (vector-ref vec 0)) vfi)
          (set-double! new-p (constant inexactnum-imag-disp) (unpack-flonum (vector-ref vec 1)) vfi)
          new-p)]
       [(fasl-type-weak-pair)
        ($oops 'vfasl "weak pair not supported")]
       [(fasl-type-ephemeron)
        ($oops 'vfasl "ephemeron pair not supported")]
       [else
        ($oops 'vfasl "unrecognized tuple type")])]
    [(string ty string)
     (constant-case* ty
       [(fasl-type-symbol)
        (when (string=? string "$install-library-entry")
          (vfasl-info-installs-library-entry?-set! vfi #t))
        (symbol-copy v
                     (string-copy string vfi)
                     (string->symbol string)
                     string
                     vfi)]
       [(fasl-type-uninterned-symbol)
        (symbol-copy v
                     (pair-copy (string-copy string vfi) (constant sfalse) vfi)
                     #f
                     string
                     vfi)]
       [else
        (let ([immutable? (eqv? ty (constant fasl-type-immutable-string))])
          (cond
            [(fx= 0 (string-length string))
             (make-vsingleton (if immutable?
                                  (constant singleton-null-immutable-string)
                                  (constant singleton-null-string)))]
            [else
             (vector-copy v string vfi
                          string-length
                          vspace-data
                          header-size-string string-data-disp
                          string-char-bytes
                          (bitwise-ior (bitwise-arithmetic-shift-left (string-length string) (constant string-length-offset))
                                       (if immutable?
                                           (constant string-immutable-flag)
                                           0)
                                       (constant type-string))
                          string-type-disp
                          set-char!
                          string-ref)]))])]
    [(gensym pname uname)
     (symbol-copy v (pair-copy (string-copy uname vfi) (string-copy pname vfi) vfi) (gensym pname uname) uname vfi)]
    [(vector ty vec)
     (cond
       [(fx= 0 (vector-length vec))
        (make-vsingleton (constant-case* ty
                           [(fasl-type-vector)
                            (constant singleton-null-vector)]
                           [(fasl-type-immutable-vector)
                            (constant singleton-null-immutable-vector)]
                           [(fasl-type-flvector)
                            (constant singleton-null-flvector)]))]
       [else
        (constant-case* ty
          [(fasl-type-vector fasl-type-immutable-vector)
           (vector-copy v vec vfi
                        vector-length
                        vspace-impure
                        header-size-vector vector-data-disp
                        ptr-bytes
                        (bitwise-ior (bitwise-arithmetic-shift-left (vector-length vec) (constant vector-length-offset))
                                     (if (eqv? ty (constant fasl-type-immutable-vector))
                                         (constant vector-immutable-flag)
                                         0)
                                     (constant type-vector))
                        vector-type-disp
                        set-ptr!
                        (lambda (vec i) (copy (vector-ref vec i) vfi)))]
          [(fasl-type-flvector)
           (vector-copy v vec vfi
                        vector-length
                        vspace-data
                        header-size-flvector flvector-data-disp
                        double-bytes
                        (bitwise-ior (bitwise-arithmetic-shift-left (vector-length vec) (constant flvector-length-offset))
                                     (constant type-flvector))
                        flvector-type-disp
                        set-double!
                        (lambda (v i) (unpack-flonum (vector-ref v i))))])])]
    [(fxvector vec)
     (cond
       [(fx= 0 (vector-length vec))
        (make-vsingleton (constant singleton-null-fxvector))]
       [else
        (vector-copy v vec vfi
                     vector-length
                     vspace-data
                     header-size-fxvector fxvector-data-disp
                     ptr-bytes
                     (bitwise-ior (bitwise-arithmetic-shift-left (vector-length v) (constant fxvector-length-offset))
                                  (constant type-fxvector))
                     fxvector-type-disp
                     set-iptr!
                     (lambda (v i) (fix (vector-ref v i))))])]
    [(bytevector ty bv)
     (cond
       [(fx= 0 (bytevector-length bv))
        (make-vsingleton (if (eqv? ty (constant fasl-type-immutable-bytevector))
                             (constant singleton-null-immutable-bytevector)
                             (constant singleton-null-bytevector)))]
       [else
        (vector-copy v bv vfi
                     bytevector-length
                     vspace-data
                     header-size-bytevector bytevector-data-disp
                     byte-bytes
                     (bitwise-ior (bitwise-arithmetic-shift-left (bytevector-length bv) (constant bytevector-length-offset))
                                  (if (eqv? ty (constant fasl-type-immutable-bytevector))
                                      (constant bytevector-immutable-flag)
                                      0)
                                  (constant type-bytevector))
                     bytevector-type-disp
                     set-u8!
                     bytevector-u8-ref)])]
    [(stencil-vector mask vec sys?)
     (vector-copy v vec vfi
                  vector-length
                  vspace-impure
                  header-size-stencil-vector stencil-vector-data-disp
                  ptr-bytes
                  (bitwise-ior (bitwise-arithmetic-shift-left mask (constant stencil-vector-mask-offset))
                               (if sys?
                                   (constant type-sys-stencil-vector)
                                   (constant type-stencil-vector)))
                  stencil-vector-type-disp
                  set-ptr!
                  (lambda (v i) (copy (vector-ref v i) vfi)))]
    [(record maybe-uid size nflds rtd pad-ty* fld*)
     (cond
       [(refers-back-to-self? v rtd)
        (base-rtd-copy v vfi)]
       [(and maybe-uid
             (let ([v2 (eq-hashtable-ref (vfasl-info-rtds vfi) (unpack-symbol maybe-uid) v)])
               (and (not (eq? v2 v))
                    v2)))
        => (lambda (v2)
             (copy v2 vfi))]
       [else
        (let ([rtd-p (copy rtd vfi)])
          (when maybe-uid
            (eq-hashtable-set! (vfasl-info-rtds vfi) (unpack-symbol maybe-uid) v)
            ;; make sure parent type is earlier
            (safe-assert (pair? fld*))
            (let ([ancestry (car fld*)])
              (field-case ancestry
                          [ptr (elem)
                               (let loop ([elem elem])
                                 (fasl-case* elem
                                   [(vector ty vec)
                                    (let ([parent (vector-ref vec (fx- (vector-length vec)
                                                                       (constant ancestry-parent-offset)))])
                                      (copy parent vfi))]
                                   [(indirect g i) (loop (vector-ref g i))]
                                   [else ($oops 'vfasl "parent type not recognized ~s" elem)]))]
                          [else (safe-assert (not 'ptr)) (void)])))
          (let* ([vspc (cond
                         [maybe-uid
                          (constant vspace-rtd)]
                         [(eqv? 0 (let-values ([(bv offset) (vptr->bytevector+offset rtd-p vfi)])
                                    (ref-uptr bv (fx+ offset (constant record-type-mpm-disp)))))
                          (constant vspace-pure-typed)]
                         [else
                          (constant vspace-impure-record)])]
                 [new-p (find-room 'record vfi vspc size (constant type-typed-object))])
            (graph! v new-p vfi)
            (set-ptr! new-p (constant record-type-disp) rtd-p vfi)
            (let loop ([addr (constant record-data-disp)]
                       [pad-ty* pad-ty*]
                       [fld* fld*])
              (unless (null? pad-ty*)
                (let* ([pad-ty (car pad-ty*)]
                       [addr (fx+ addr (fxsrl pad-ty 4))]
                       [addr (field-case (car fld*)
                               [ptr (elem)
                                (safe-assert (eqv? (fxand pad-ty #xF) (constant fasl-fld-ptr)))
                                (set-ptr! new-p addr (copy elem vfi) vfi)
                                (fx+ addr (constant ptr-bytes))]
                               [iptr (elem)
                                (set-iptr! new-p addr elem vfi)
                                (fx+ addr (constant ptr-bytes))]
                               [double (high low)
                                (safe-assert (eqv? (fxand pad-ty #xF) (constant fasl-fld-double)))
                                (set-double! new-p addr
                                             (build-flonum high low)
                                             vfi)
                                (fx+ addr (constant double-bytes))]
                               [else
                                (error 'vfasl "unsupported field: ~s" (car fld*))])])
                  (loop addr (cdr pad-ty*) (cdr fld*)))))
            new-p))])]
    [(closure offset c)
     (let* ([c-v (copy c vfi)]
            [new-p (find-room 'closure vfi
                              (constant vspace-closure)
                              (constant header-size-closure)
                              (constant type-closure))])
       (graph! v new-p vfi)
       (set-ptr!/no-record new-p (constant closure-code-disp) (vptr+ c-v offset) vfi)
       new-p)]
    [(code flags free name arity-mask info pinfo* bytes m vreloc)
     (let* ([len (bytevector-length bytes)]
            [new-p (find-room 'code vfi
                              (constant vspace-code)
                              (fx+ (constant header-size-code) len)
                              (constant type-typed-object))])
       (graph! v new-p vfi)
       (set-uptr! new-p (constant code-type-disp)
                  (bitwise-ior (bitwise-arithmetic-shift-left flags (constant code-flags-offset))
                               (constant type-code))
                  vfi)
       (set-uptr! new-p (constant code-length-disp) len vfi)
       (set-ptr! new-p (constant code-name-disp)
                 (fasl-case* name
                   [(string ty string)
                    ;; imitate string interning that fasl read performs:
                    (if (or (eqv? ty (constant fasl-type-string))
                            (eqv? ty (constant fasl-type-immutable-string)))
                        (string-copy string vfi)
                        (copy name vfi))]
                   [else (copy name vfi)])
                 vfi)
       (set-ptr! new-p (constant code-arity-mask-disp) (copy arity-mask vfi) vfi)
       (set-uptr! new-p (constant code-closure-length-disp) free vfi)
       (set-ptr! new-p (constant code-info-disp) (copy info vfi) vfi)
       (set-ptr! new-p (constant code-pinfo*-disp) (copy pinfo* vfi) vfi)
       (copy-u8s! new-p (constant code-data-disp) bytes 0 len vfi)
       ;; must be after code is copied into place:
       (set-ptr!/no-record new-p (constant code-reloc-disp) (copy-reloc m vreloc new-p vfi) vfi)
       new-p)]
    [(symbol-hashtable mutable? minlen subtype veclen vpfasl)
     (let* ([flds (rtd-flds $symbol-ht-rtd)]
            [len (fx* (length flds) (constant ptr-bytes))]
            [new-p (find-room 'symbol-ht vfi
                              (constant vspace-impure)
                              (fx+ (constant header-size-record) len)
                              (constant type-typed-object))]
            [vec-p (find-room 'symbol-ht-vector vfi
                              (constant vspace-impure)
                              (fx+ (constant header-size-vector) (fx* veclen (constant ptr-bytes)))
                              (constant type-typed-object))]
            [equiv (case subtype
                     [(0) (make-vsingleton (constant singleton-eq))]
                     [(1) (make-vsingleton (constant singleton-eqv))]
                     [(2) (make-vsingleton (constant singleton-equal))]
                     [(3) (make-vsingleton (constant singleton-symbol=?))]
                     [else ($oops 'vfasl "unrecognized symbol table subtype ~s" subtype)])])
       (define (field-offset name)
         (let loop ([flds flds] [addr (constant record-data-disp)])
           (cond
             [(null? flds) ($oops 'vfasl "could not find symbol hash table field ~s" name)]
             [(eq? (fld-name (car flds)) name) addr]
             [else (loop (cdr flds) (fx+ addr (constant ptr-bytes)))])))
       (graph! v new-p vfi)
       (set-ptr! new-p (constant record-type-disp) (make-vsingleton (constant singleton-symbol-ht-rtd)) vfi)
       (set-ptr! new-p (field-offset 'type) (make-vsingleton (constant singleton-symbol-symbol)) vfi)
       (set-ptr! new-p (field-offset 'mutable?) (if mutable? (constant strue) (constant sfalse)) vfi)
       (set-ptr! new-p (field-offset 'vec) vec-p vfi)
       (set-ptr! new-p (field-offset 'minlen) (fix minlen) vfi)
       (set-ptr! new-p (field-offset 'size) (fix (vector-length vpfasl)) vfi)
       (set-ptr! new-p (field-offset 'equiv?) equiv vfi)
       (set-uptr! vec-p (constant vector-type-disp)
                  (bitwise-ior (bitwise-arithmetic-shift-left veclen (constant vector-length-offset))
                               (constant type-vector))
                  vfi)
       (let ([to-vec (make-vector veclen (constant snil))])
         ;; first, determine what goes in each vector slot, building up
         ;; pair copies for the vector slots:
         (vector-for-each (lambda (p)
                            (let* ([a (copy (car p) vfi)]
                                   [b (copy (cdr p) vfi)]
                                   [hc (or (fasl-case* (car p)
                                             [(string ty string)
                                              (and (eqv? ty (constant fasl-type-symbol))
                                                   (target-symbol-hash string))]
                                             [(gensym pname uname)
                                              (target-symbol-hash uname)]
                                             [else #f])
                                           ($oops 'vfasl "symbol table key not a symbol ~s" (car p)))]
                                   [i (fxand hc (fx- veclen 1))])
                              (vector-set! to-vec i (pair-copy (pair-copy a b vfi) (vector-ref to-vec i) vfi))))
                          vpfasl)
         ;; install the vector slots:
         (let loop ([i 0])
           (unless (fx= i veclen)
             (set-ptr! vec-p (fx+ (constant vector-data-disp) (fx* i (constant ptr-bytes)))
                       (vector-ref to-vec i)
                       vfi)
             (loop (fx+ i 1)))))
       new-p)]
    [(indirect g i) (copy (vector-ref g i) vfi)]
    [else
     ($oops 'vfasl "unsupported ~s" v)]))

(define-syntax (vector-copy stx)
  (syntax-case stx ()
    [(_ v vec vfi
        vec-length
        vspace
        header-size-vec data-disp
        elem-bytes
        tag
        vec-type-disp
        set-elem!
        vec-ref)
     #'(let* ([len (vec-length vec)]
              [new-p (find-room 'vec-type-disp vfi
                                (constant vspace)
                                (fx+ (constant header-size-vec) (fx* len (constant elem-bytes)))
                                (constant type-typed-object))])
         (graph! v new-p vfi)
         (set-uptr! new-p (constant vec-type-disp) tag vfi)
         (let loop ([i 0])
           (unless (fx= i len)
             (set-elem! new-p (fx+ (constant data-disp) (fx* i (constant elem-bytes)))
                        (vec-ref vec i)
                        vfi)
             (loop (fx+ i 1))))
         new-p)]))

(define (symbol-copy v name sym hash-name vfi)
  (let ([v2 (eq-hashtable-ref (vfasl-info-symbols vfi) sym v)])
    (cond
      [(not (eq? v v2))
       (copy v2 vfi)]
      [else
       (when sym
         (eq-hashtable-set! (vfasl-info-symbols vfi) sym v))
       (let ([new-p (find-room 'symbol vfi
                               (constant vspace-symbol)
                               (constant size-symbol)
                               (constant type-symbol))])
         (graph! v new-p vfi)
         (set-uptr! new-p (constant symbol-value-disp)
                    ;; use value slot to store symbol index
                    (fix (symbol-vptr->index new-p vfi))
                    vfi)
         (set-uptr! new-p (constant symbol-pvalue-disp)
                    ;; use pvalue slot to record interned or not
                    (if sym (constant strue) (constant sfalse))
                    vfi)
         (set-uptr! new-p (constant symbol-plist-disp) (constant snil) vfi)
         (set-ptr! new-p (constant symbol-name-disp) name vfi)
         (set-uptr! new-p (constant symbol-splist-disp) (constant snil) vfi)
         (set-iptr! new-p (constant symbol-hash-disp) (fix (target-symbol-hash hash-name)) vfi)
         new-p)])))

(define target-symbol-hash
  (let ([symbol-hashX (constant-case ptr-bits
                        [(32) (foreign-procedure "(cs)symbol_hash32" (ptr) integer-32)]
                        [(64) (foreign-procedure "(cs)symbol_hash64" (ptr) integer-64)])])
    (lambda (name)
      (bitwise-and (symbol-hashX (if (pair? name) (car name) name))
                   (constant most-positive-fixnum)))))

(define (string-copy name vfi)
  ;; interns `name` so that symbols and code share
  (let ([s (or (hashtable-ref (vfasl-info-strings vfi) name #f)
               (let ([s (fasl-string (constant fasl-type-immutable-string) name)])
                 (hashtable-set! (vfasl-info-strings vfi) name s)
                 s))])
    (copy s vfi)))

(define (pair-copy a d vfi)
  (let* ([new-p (find-room 'pair vfi
                           (constant vspace-impure)
                           (constant size-pair)
                           (constant type-pair))])
    (set-ptr! new-p (constant pair-car-disp) a vfi)
    (set-ptr! new-p (constant pair-cdr-disp) d vfi)
    new-p))

(define (exact-integer-copy v n vfi)
  (if (<= (constant most-negative-fixnum) n (constant most-positive-fixnum))
      (fix n)
      (let ([len (fxquotient (fx+ (integer-length n) (fx- (constant bigit-bits) 1)) (constant bigit-bits))])
        (vector-copy v n vfi
                     (lambda (n) len)
                     vspace-data
                     header-size-bignum bignum-data-disp
                     bigit-bytes
                     (bitwise-ior (bitwise-arithmetic-shift-left len (constant bignum-length-offset))
                                  (if (negative? n)
                                      (constant type-negative-bignum)
                                      (constant type-positive-bignum)))
                     bignum-type-disp
                     set-bigit!
                     (lambda (n i)
                       (let ([i (- len i 1)])
                         (let ([i (fx* i (constant bigit-bits))])
                           (bitwise-bit-field n i (fx+ i (constant bigit-bits))))))))))

(define (base-rtd-copy v vfi)
  (let ([new-p (or (vfasl-info-base-rtd vfi)
                   (find-room 'base-rtd vfi
                              (constant vspace-rtd)
                              (constant size-record-type)
                              (constant type-typed-object)))])
    ;; this is a placeholder, and there's no need to write any content
    (graph! v new-p vfi)
    (vfasl-info-base-rtd-set! vfi new-p)
    new-p))

(define (refers-back-to-self? v rtd)
  (or (eq? v rtd)
      (fasl-case* rtd
        [(indirect g i) (refers-back-to-self? v (vector-ref g i))]
        [else #f])))

(define (reloc-addr n)
  (fx+ (constant reloc-table-data-disp) (fx* n (constant ptr-bytes))))

(define (make-short-reloc type code-offset item-offset)
  (bitwise-ior (bitwise-arithmetic-shift-left type (constant reloc-type-offset))
               (bitwise-arithmetic-shift-left code-offset (constant reloc-code-offset-offset))
               (bitwise-arithmetic-shift-left item-offset (constant reloc-item-offset-offset))))

(define (build-vfasl-reloc tag pos)
  (fix (bitwise-ior tag (bitwise-arithmetic-shift-left pos (constant vfasl-reloc-tag-bits)))))

(define (copy-reloc m vreloc code-p vfi)
  (let* ([new-p (find-room 'reloc vfi
                           (constant vspace-reloc)
                           (fx+ (constant header-size-reloc-table) (fx* m (constant ptr-bytes)))
                           (constant type-untyped))])
    (set-uptr! new-p (constant reloc-table-size-disp) m vfi)
    (set-ptr!/no-record new-p (constant reloc-table-code-disp) code-p vfi)
    (let loop ([n 0] [a 0] [i 0])
      (unless (fx= n m)
        (fasl-case* (vector-ref vreloc i)
          [(reloc type-etc code-offset item-offset elem)
           (let* ([type (fxsra type-etc 2)]
                  [n (cond
                       [(fxlogtest type-etc 1)
                        (set-uptr! new-p (reloc-addr n)
                                   (bitwise-ior (fxsll type (constant reloc-type-offset))
                                                (constant reloc-extended-format))
                                   vfi)
                        (set-uptr! new-p (reloc-addr (fx+ n 1)) item-offset vfi)
                        (set-uptr! new-p (reloc-addr (fx+ n 2)) code-offset vfi)
                        (fx+ n 3)]
                       [else
                        (set-uptr! new-p (reloc-addr n)
                                   (make-short-reloc type code-offset item-offset)
                                   vfi)
                        (fx+ n 1)])]
                  [a (fx+ a code-offset)]
                  [new-elem (or (fasl-case* elem
                                  [(atom ty uptr)
                                   (constant-case* ty
                                     [(fasl-type-entry)
                                      (when (eqv? uptr (lookup-c-entry install-library-entry))
                                        (vfasl-info-installs-library-entry?-set! vfi #t))
                                      (build-vfasl-reloc (constant vfasl-reloc-c-entry-tag) uptr)]
                                     [(fasl-type-library)
                                      (build-vfasl-reloc (constant vfasl-reloc-library-entry-tag) uptr)]
                                     [(fasl-type-library-code)
                                      (build-vfasl-reloc (constant vfasl-reloc-library-entry-code-tag) uptr)]
                                     [else #f])]
                                  [else #f])
                                (let ([elem-addr (copy elem vfi)])
                                  (cond
                                    [(vsingleton? elem-addr)
                                     (build-vfasl-reloc (constant vfasl-reloc-singleton-tag)
                                                        (vsingleton-index elem-addr))]
                                    [(vptr? elem-addr)
                                     (cond
                                       [(eqv? (vptr-vspace elem-addr) (constant vspace-symbol))
                                        (build-vfasl-reloc (constant vfasl-reloc-symbol-tag)
                                                           (symbol-vptr->index elem-addr vfi))]
                                       [else
                                        (let-values ([(bv offset) (vptr->bytevector+offset elem-addr vfi)])
                                          (safe-assert (not (fixed? offset)))
                                          (fx- offset (vfasl-info-base-addr vfi)))])]
                                    [else
                                     ;; an immediate value; for fixnums, we can only allow 0
                                     (unless (or (eqv? elem-addr 0)
                                                 (not (fixed? elem-addr)))
                                       ($oops 'vfasl "unexpected fixnum in relocation ~s" elem-addr))
                                     elem-addr])))])
             ;; overwrites constant-loading instructions in the code, so the
             ;; linking protocol needs to be able to deal with that, possibly using
             ;; later instructions to infer the right repair:
             (set-int32! code-p a new-elem vfi)
             (loop n a (fx+ i 1)))]
          [else ($oops 'vfasl "expected a relocation")])))
    new-p))

(set! $fasl-to-vfasl to-vfasl)
(set! $fasl-can-combine? fasl-can-combine?))
