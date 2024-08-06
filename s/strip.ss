;; strip.ss
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

;; The `strip-fasl-file` and related functions use a fasl reader and
;; writer that are completely separate from the ones in "fasl.ss" and
;; "fasl.c", so changes made in those places must be duplicated here.
;; The vfasl writer uses this fasl reader.

(let ()
  ; per file
  (define-threaded fasl-who)
  (define-threaded fasl-count)

  (include "strip-types.ss")

  (define follow-indirect
    (lambda (x)
      (fasl-case x
        [indirect (g i) (follow-indirect (vector-ref g i))]
        [else x])))

  (define-syntax bogus
    (lambda (x)
      (syntax-case x ()
        [(_ msg arg ...)
         (string? (datum msg))
         #`($oops fasl-who #,(string-append (datum msg) " within fasl entry ~d") arg ... fasl-count)])))

  (define-syntax sorry!
    (syntax-rules ()
      [(_ str arg ...) ($oops 'fasl-internal str arg ...)]))

  (module (read-entry)
    (define-syntax fasl-type-case
      (syntax-rules (else)
        [(_ e0 [(k ...) e1 e2 ...] ... [else ee1 ee2 ...])
         (let ([x e0])
           (cond
             [(memv x (list (constant k) ...)) e1 e2 ...]
             ...
             [else ee1 ee2 ...]))]))
    (define read-iptr
      (lambda (p)
        (let ([k0 (read-byte p)])
          (let f ([k k0] [n (fxsrl (fxlogand k0 #x7f) 1)])
            (if (fxlogbit? 0 k)
                (let ([k (read-byte p)])
                  (f k (logor (ash n 7) (fxsrl k 1))))
                (if (fxlogbit? 7 k0) (- n) n))))))
    (define read-uptr
      (lambda (p)
        (let ([k (read-byte p)])
          (let f ([k k] [n (fxand k #x7F)])
            (if (fxlogbit? 7 k)
                (let ([k (read-byte p)])
                  (f k (logor (ash n 7) (fxand k #x7F))))
                n)))))
    (define read-uptr/bytes
      (lambda (p)
        (let ([k (read-byte p)])
          (let f ([k k] [n (fxand k #x7F)] [bytes 1])
            (if (fxlogbit? 7 k)
                (let ([k (read-byte p)])
                  (f k (logor (ash n 7) (fxand k #x7F)) (fx+ bytes 1)))
                (values n bytes))))))
    (define read-byte-or-eof
      (lambda (p)
        (get-u8 p)))
    (define read-byte
      (lambda (p)
        (let ([b (get-u8 p)])
          (when (eof-object? b) (bogus "unexpected eof in ~a" (port-name p)))
          b)))
    (define (read-byte! x p)
      (let ([y (read-byte p)])
        (unless (eqv? y x)
          (bogus "expected byte ~s, got ~s from ~a" x y (port-name p)))))
    (define read-bytevector
      (lambda (p n)
        (let ([bv (make-bytevector n)])
          (do ([i 0 (fx+ i 1)])
              ((fx= i n) bv)
            (bytevector-u8-set! bv i (read-byte p))))))
    (define read-string
      (lambda (p)
        (let ([n (read-uptr p)])
          (let ([s (make-string n)])
            (do ([i 0 (+ i 1)])
                ((= i n))
              (string-set! s i (integer->char (read-uptr p))))
            s))))
    (define (read-entry p init-g)
      (let ([ty (read-byte-or-eof p)])
        (if (eof-object? ty)
            ty
            (fasl-type-case ty
              [(fasl-type-header) (read-header p)]
              [(fasl-type-visit fasl-type-revisit fasl-type-visit-revisit)
               (let* ([situation ty]
                      [size (read-uptr p)]
                      [compressed-flag (read-byte p)]
                      [kind (read-byte p)])
                 (unless (eqv? kind (constant fasl-type-fasl))
                   (bogus "unexpected fasl kind ~a" (port-name p)))
                 (fasl-type-case compressed-flag
                   [(fasl-type-gzip fasl-type-lz4)
                    (let-values ([(dest-size dest-size-bytes) (read-uptr/bytes p)])
                      (let* ([src-size (- size 2 dest-size-bytes)]
                             [bv (read-bytevector p src-size)]
                             [bv ($bytevector-uncompress bv 0 src-size dest-size
                                   (if (eqv? compressed-flag (constant fasl-type-gzip))
                                       (constant COMPRESS-GZIP)
                                       (constant COMPRESS-LZ4)))])
                        (fasl-entry situation (read-fasl (open-bytevector-input-port bv) init-g))))]
                   [(fasl-type-uncompressed) (fasl-entry situation (read-fasl p init-g))]
                   [else (bogus "expected compression flag in ~a" (port-name p))]))]
              [else (bogus "expected header or situation in ~a" (port-name p))]))))
    (define (read-header p)
      (let* ([bv (constant fasl-header)] [n (bytevector-length bv)])
        (do ([i 1 (fx+ i 1)])
            ((fx= i n))
          (read-byte! (bytevector-u8-ref bv i) p)))
      (let* ([version (read-uptr p)]
             [machine (read-uptr p)])
        (unless (eqv? version (constant scheme-version))
          (bogus "expected version ~a, but found ~a in ~a"
            ($format-scheme-version (constant scheme-version))
            ($format-scheme-version version)
            (port-name p)))
        (read-byte! (char->integer #\() p) ;)
        (fasl-header version machine
          (let f () ;(
            (let ([c (read-byte p)])
              (if (eqv? c (char->integer #\)))
                  '()
                  (cons c (f))))))))
    (define (read-fld p g ty)
      (define (read-double p)
        (let* ([high (read-uptr p)]
               [low (read-uptr p)])
          (field-double high low)))
      (fasl-type-case ty
        [(fasl-fld-ptr) (field-ptr (read-fasl p g))]
        [(fasl-fld-u8) (field-byte (read-byte p))]
        [(fasl-fld-i16) (field-iptr (read-iptr p))]
        [(fasl-fld-i24) (field-iptr (read-iptr p))]
        [(fasl-fld-i32) (field-iptr (read-iptr p))]
        [(fasl-fld-i40) (field-iptr (read-iptr p))]
        [(fasl-fld-i48) (field-iptr (read-iptr p))]
        [(fasl-fld-i56) (field-iptr (read-iptr p))]
        [(fasl-fld-i64) (field-iptr (read-iptr p))]
        [(fasl-fld-single) (field-single (read-uptr p))]
        [(fasl-fld-double) (read-double p)]
        [else (bogus "unexpected record fld type ~s in ~a" ty (port-name p))]))
    (define (read-vfasl p g n)
      (let ([v (make-vector n)])
        (do ([i 0 (fx+ i 1)])
          ((fx= i n) v)
          (vector-set! v i (read-fasl p g)))))
    (define (read-vpfasl p g)
      (let ([n (read-uptr p)])
        (let ([v (make-vector n)])
          (do ([i 0 (fx+ i 1)])
            ((fx= i n) v)
            (vector-set! v i
              (let ([key (read-fasl p g)])
                (cons key (read-fasl p g))))))))
    (define (read-record p g maybe-uid size)
      (let* ([nflds (read-uptr p)] [rtd (read-fasl p g)])
        (let loop ([n nflds] [rpad-ty* '()] [rfld* '()])
          (if (fx= n 0)
              (fasl-record maybe-uid size nflds rtd (reverse rpad-ty*) (reverse rfld*))
              (let* ([pad-ty (read-byte p)] [fld (read-fld p g (fxlogand pad-ty #x0f))])
                (loop (fx- n 1) (cons pad-ty rpad-ty*) (cons fld rfld*)))))))
    (define (read-fasl p g)
      (let ([ty (read-byte p)])
        (fasl-type-case ty
          [(fasl-type-pair) (fasl-pair (read-vfasl p g (+ (read-uptr p) 1)))]
          [(fasl-type-box fasl-type-immutable-box) (fasl-tuple ty (vector (read-fasl p g)))]
          [(fasl-type-symbol) (fasl-string ty (read-string p))]
          [(fasl-type-gensym)
           (let* ([pname (read-string p)] [uname (read-string p)])
             (fasl-gensym pname uname))]
          [(fasl-type-uninterned-symbol)
           (fasl-string ty (read-string p))]
          [(fasl-type-ratnum fasl-type-exactnum fasl-type-inexactnum
                             fasl-type-weak-pair fasl-type-ephemeron)
           (let ([first (read-fasl p g)])
             (fasl-tuple ty (vector first (read-fasl p g))))]
          [(fasl-type-vector fasl-type-immutable-vector fasl-type-flvector)
           (fasl-vector ty (read-vfasl p g (read-uptr p)))]
          [(fasl-type-fxvector)
           (fasl-fxvector
             (let ([n (read-uptr p)])
               (let ([v (make-vector n)])
                 (do ([i 0 (fx+ i 1)])
                     ((fx= i n) v)
                   (vector-set! v i (read-iptr p))))))]
          [(fasl-type-bytevector fasl-type-immutable-bytevector)
           (fasl-bytevector ty (read-bytevector p (read-uptr p)))]
          [(fasl-type-stencil-vector)
           (let ([mask (read-uptr p)])
             (fasl-stencil-vector mask (read-vfasl p g (bitwise-bit-count mask)) #f))]
          [(fasl-type-system-stencil-vector)
           (let ([mask (read-uptr p)])
             (fasl-stencil-vector mask (read-vfasl p g (bitwise-bit-count mask)) #t))]
          [(fasl-type-base-rtd) (fasl-tuple ty '#())]
          [(fasl-type-rtd) (let* ([uid (read-fasl p g)]
                                  [size (read-uptr p)])
                             (if (eqv? size 0)
                                 (fasl-rtd-ref uid)
                                 (read-record p g uid size)))]
          [(fasl-type-record) (read-record p g #f (read-uptr p))]
          [(fasl-type-closure)
           (let* ([offset (read-uptr p)]
                  [c (read-fasl p g)])
             (fasl-closure offset c))]
          [(fasl-type-flonum)
           (let* ([high (read-uptr p)]
                  [low (read-uptr p)])
             (fasl-flonum high low))]
          [(fasl-type-string fasl-type-immutable-string) (fasl-string ty (read-string p))]
          [(fasl-type-small-integer) (fasl-small-integer (read-iptr p))]
          [(fasl-type-large-integer)
           (let* ([sign (read-byte p)]
                  [n (read-uptr p)])
             (fasl-large-integer sign
               (let ([v (make-vector n)])
                 (do ([i 0 (fx+ i 1)])
                     ((fx= i n) v)
                   (vector-set! v i (read-uptr p))))))]
          [(fasl-type-eq-hashtable)
           (let* ([mutable? (read-byte p)]
                  [subtype (read-byte p)]
                  [minlen (read-uptr p)]
                  [veclen (read-uptr p)]
                  [v (read-vpfasl p g)])
             (fasl-eq-hashtable mutable? subtype minlen veclen v))]
          [(fasl-type-symbol-hashtable)
           (let* ([mutable? (read-byte p)]
                  [minlen (read-uptr p)]
                  [equiv (read-byte p)]
                  [veclen (read-uptr p)]
                  [v (read-vpfasl p g)])
             (fasl-symbol-hashtable mutable? minlen equiv veclen v))]
          [(fasl-type-code)
           (let* ([flags (read-byte p)]
                  [free (read-uptr p)]
                  [nbytes (read-uptr p)]
                  [name (read-fasl p g)]
                  [arity-mask (read-fasl p g)]
                  [info (read-fasl p g)]
                  [pinfo* (read-fasl p g)]
                  [bytes (let ([bv (make-bytevector nbytes)])
                           (do ([i 0 (fx+ i 1)])
                             ((fx= i nbytes) bv)
                             (bytevector-u8-set! bv i (read-byte p))))]
                  [m (read-uptr p)]
                  [vreloc (let loop ([n 0] [rls '()])
                            (if (fx= n m)
                                (list->vector (reverse rls))
                                (let* ([type-etc (read-byte p)]
                                       [code-offset (read-uptr p)]
                                       [item-offset (if (fxlogtest type-etc 2) (read-uptr p) 0)])
                                  (loop
                                    (fx+ n (if (fxlogtest type-etc 1) 3 1))
                                    (cons (fasl-reloc type-etc code-offset item-offset (read-fasl p g)) rls)))))]
                  )
             (fasl-code flags free name arity-mask info pinfo* bytes m vreloc))]
          [(fasl-type-immediate fasl-type-entry fasl-type-library fasl-type-library-code)
           (fasl-atom ty (read-uptr p))]
          [(fasl-type-graph) (read-fasl p (let ([new-g (make-vector (read-uptr p) #f)])
                                            (let ([n (read-uptr p)])
                                              (unless (or (zero? n) (and g (>= (vector-length g) n)))
                                                (bogus "incompatible external vector in ~a" (port-name p))))
                                            (when g
                                              (let ([delta (fx- (vector-length new-g) (vector-length g))])
                                                (let loop ([i 0])
                                                  (unless (fx= i (vector-length g))
                                                    (vector-set! new-g (fx+ i delta) (vector-ref g i))
                                                    (loop (fx+ i 1))))))
                                            new-g))]
          [(fasl-type-graph-def)
           (let ([n (read-uptr p)])
             (let ([x (read-fasl p g)])
               (when (vector-ref g n) (bogus "duplicate definition for graph element ~s in ~a" n (port-name p)))
               (vector-set! g n x)
               x))]
          [(fasl-type-graph-ref)
           (let ([n (read-uptr p)])
             (or (vector-ref g n)
                 (fasl-indirect g n)))]
          [(fasl-type-begin)
           (let loop ([n (read-uptr p)])
             (if (fx= n 1)
                 (read-fasl p g)
                 (begin
                   ;; will set graph definitions:
                   (read-fasl p g)
                   (loop (fx- n 1)))))]
          [else (bogus "unexpected fasl code ~s in ~a" ty (port-name p))]))))

  (define read-script-header
    (lambda (ip)
      (let-values ([(bvop extract) (open-bytevector-output-port)])
        (define get
          (lambda ()
            (let ([b (get-u8 ip)])
              (put-u8 bvop b)
              b)))
        (if (and (eqv? (get) (char->integer #\#))
                 (eqv? (get) (char->integer #\!))
                 (let ([b (get)])
                   (or (eqv? b (char->integer #\/))
                       (eqv? b (char->integer #\space)))))
            (let f ()
              (let ([b (get)])
                (if (eof-object? b)
                    (bogus "unexpected eof reading #! line in ~a" (port-name ip))
                    (if (eqv? b (char->integer #\newline))
                        (extract)
                        (f)))))
            (begin (set-port-position! ip 0) #f)))))

  (let ()
    (define-threaded strip-inspector-information?)
    (define-threaded strip-profile-information?)
    (define-threaded strip-source-annotations?)
    (define-threaded strip-compile-time-information?)

    (module (fasl-record-predicate fasl-record-accessor)
      (define field-index
        (lambda (rtd field-name)
          (let ([v (record-type-field-names rtd)])
            (let loop ([i 0] [index #f])
              (if (fx= i (vector-length v))
                  (or index (sorry! "field ~s not found for ~s" field-name rtd))
                  (if (eq? (vector-ref v i) field-name)
                      (if index
                          (sorry! "duplicate field ~s found for ~s" field-name rtd)
                          (loop (fx+ i 1) i))
                      (loop (fx+ i 1) index)))))))
      (define uid-index (field-index #!base-rtd 'uid))
      (define fasl-record?
        (lambda (uname x)
          (fasl-case (follow-indirect x)
            [record (maybe-uid size nflds rtd pad-ty* fld*)
              (fasl-case (follow-indirect rtd)
                [record (rtd-uid rtd-size rtd-nflds rtd-rtd rtd-pad-ty* rtd-fld*)
                  (and (> (length rtd-fld*) uid-index)
                       (field-case (list-ref rtd-fld* uid-index)
                         [ptr (fasl)
                          (fasl-case (follow-indirect fasl)
                            [gensym (pname2 uname2) (string=? uname2 uname)]
                            [else #f])]
                         [else #f]))]
                [else #f])]
            [else #f])))
      (define fasl-record-predicate
        (lambda (rtd)
          (let ([uname (gensym->unique-string (record-type-uid rtd))])
            (lambda (x)
              (fasl-record? uname x)))))
      (define fasl-record-accessor
        (lambda (rtd field-name)
          (let ([uname (gensym->unique-string (record-type-uid rtd))]
                [index (field-index rtd field-name)])
            (lambda (x)
              (unless (fasl-record? uname x)
                (sorry! "unexpected type of object ~s" x))
              (fasl-case (follow-indirect x)
                [record (maybe-uid size nflds rtd pad-ty* fld*)
                  (unless (> (length fld*) index)
                    (sorry! "fewer fields than expected for ~s" x))
                  (let ([fld (list-ref fld* index)])
                    (field-case fld
                      [ptr (fasl) fasl]
                      [else (sorry! "unexpected type of field ~s" fld)]))]
                [else (sorry! "~s should have been a fasl record" x)]))))))

    (module (fasl-annotation? fasl-annotation-stripped)
      (include "types.ss")
      (define fasl-annotation? (fasl-record-predicate (record-type-descriptor annotation)))
      (define fasl-annotation-stripped (fasl-record-accessor (record-type-descriptor annotation) 'stripped)))

    (define-record-type table
      (nongenerative)
      (sealed #t)
      (fields (mutable count) (immutable ht))
      (protocol
        (lambda (new)
          (lambda ()
            (new 0 (make-eq-hashtable))))))

    (define build-graph!
      (lambda (x t th)
        (let ([a (eq-hashtable-cell (table-ht t) x 'first)])
          (let ([p (cdr a)])
            (cond
              [(eq? p 'first) (set-cdr! a #f) (th)]
              [(not p)
               (let ([n (table-count t)])
                 (set-cdr! a (cons n #t))
                 (table-count-set! t (fx+ n 1)))])))))

    (define build!
      (lambda (x t)
        (define build-vfasl!
          (lambda (vfasl)
            (lambda ()
              (vector-for-each (lambda (fasl) (build! fasl t)) vfasl))))
        (fasl-case x
          [entry (situation fasl) (sorry! "unexpected fasl-record-type entry")]
          [header (version machine dependencies) (sorry! "unexpected fasl-record-type header")]
          [pair (vfasl) (build-graph! x t (build-vfasl! vfasl))]
          [tuple (ty vfasl) (build-graph! x t (build-vfasl! vfasl))]
          [string (ty string) (build-graph! x t void)]
          [gensym (pname uname) (build-graph! x t void)]
          [vector (ty vfasl) (build-graph! x t (build-vfasl! vfasl))]
          [fxvector (viptr) (build-graph! x t void)]
          [bytevector (ty viptr) (build-graph! x t void)]
          [stencil-vector (mask vfasl sys?) (build-graph! x t (build-vfasl! vfasl))]
          [record (maybe-uid size nflds rtd pad-ty* fld*)
           (if (and strip-source-annotations? (fasl-annotation? x))
               (build! (fasl-annotation-stripped x) t)
               (build-graph! x t
                 (lambda ()
                   (when maybe-uid (build! maybe-uid t))
                   (build! rtd t)
                   (for-each (lambda (fld)
                               (field-case fld
                                 [ptr (fasl) (build! fasl t)]
                                 [else (void)]))
                     fld*))))]
          [rtd-ref (uid) (build-graph! x t (lambda () (build! uid #t)))]
          [closure (offset c) (build-graph! x t (lambda () (build! c t)))]
          [flonum (high low) (build-graph! x t void)]
          [small-integer (iptr) (build-graph! x t void)]
          [large-integer (sign vuptr) (build-graph! x t void)]
          [eq-hashtable (mutable? subtype minlen veclen vpfasl)
           (build-graph! x t
             (lambda ()
               (vector-for-each
                 (lambda (pfasl)
                   (build! (car pfasl) t)
                   (build! (cdr pfasl) t))
                 vpfasl)))]
          [symbol-hashtable (mutable? minlen equiv veclen vpfasl)
           (build-graph! x t
             (lambda ()
               (vector-for-each
                 (lambda (pfasl)
                   (build! (car pfasl) t)
                   (build! (cdr pfasl) t))
                 vpfasl)))]
          [code (flags free name arity-mask info pinfo* bytes m vreloc)
           (build-graph! x t
             (lambda ()
               (build! name t)
               (build! arity-mask t)
               (unless strip-inspector-information? (build! info t))
               (unless strip-profile-information? (build! pinfo* t))
               (vector-for-each (lambda (reloc) (build! reloc t)) vreloc)))]
          [atom (ty uptr) (void)]
          [reloc (type-etc code-offset item-offset fasl) (build! fasl t)]
          [indirect (g i) (build! (vector-ref g i) t)])))

    (include "fasl-helpers.ss")

    (define handle-entry
      (lambda (x header-k entry-k)
        (fasl-case x
          [header (version machine dependencies)
           (header-k (lambda (p) (emit-header p version machine dependencies)))]
          [entry (situation fasl)
           (entry-k situation fasl)]
          [else
           (sorry! "unrecognized top-level fasl-record-type ~s" x)])))

    (define (write-one-entry p situation fasl)
      (let ([t (make-table)])
        (build! fasl t)
        (let-values ([(bv* size)
                      (let-values ([(p extractor) ($open-bytevector-list-output-port)])
                        (let ([n (table-count t)])
                          (unless (fx= n 0)
                            (put-u8 p (constant fasl-type-graph))
                            (put-uptr p n)
                            (put-uptr p 0)))
                        (write-fasl p t fasl)
                        (extractor))])
          ($write-fasl-bytevectors p bv* size situation (constant fasl-type-fasl)))))

    (define write-entry
      (lambda (p x)
        (handle-entry
         x
         (lambda (write-k) (write-k p))
         (lambda (situation fasl) (write-one-entry p situation fasl)))))

    (define write-graph
      (lambda (p t x th)
        (let ([a (eq-hashtable-ref (table-ht t) x #f)])
          (cond
            [(not a) (th)]
            [(cdr a)
             (put-u8 p (constant fasl-type-graph-def))
             (put-uptr p (car a))
             (set-cdr! a #f)
             (th)]
            [else
             (put-u8 p (constant fasl-type-graph-ref))
             (put-uptr p (car a))]))))

    (define write-fasl
      (lambda (p t x)
        (fasl-case x
          [entry (situation fasl) (sorry! "unexpected fasl-record-type entry")]
          [header (version machine dependencies) (sorry! "unexpected fasl-record-type header")]
          [pair (vfasl)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-pair))
               (put-uptr p (fx- (vector-length vfasl) 1))
               (vector-for-each (lambda (fasl) (write-fasl p t fasl)) vfasl)))]
          [tuple (ty vfasl)
           (write-graph p t x
             (lambda ()
               (put-u8 p ty)
               (vector-for-each (lambda (fasl) (write-fasl p t fasl)) vfasl)))]
          [string (ty string)
           (write-graph p t x
             (lambda ()
               (put-u8 p ty)
               (write-string p string)))]
          [gensym (pname uname)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-gensym))
               (write-string p pname)
               (write-string p uname)))]
          [vector (ty vfasl)
           (write-graph p t x
             (lambda ()
               (put-u8 p ty)
               (put-uptr p (vector-length vfasl))
               (vector-for-each (lambda (fasl) (write-fasl p t fasl)) vfasl)))]
          [fxvector (viptr)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-fxvector))
               (put-uptr p (vector-length viptr))
               (vector-for-each (lambda (iptr) (put-iptr p iptr)) viptr)))]
          [bytevector (ty bv)
           (write-graph p t x
             (lambda ()
               (put-u8 p ty)
               (put-uptr p (bytevector-length bv))
               (put-bytevector p bv)))]
          [stencil-vector (mask vfasl sys?)
           (write-graph p t x
             (lambda ()
               (put-u8 p (if sys?
                             (constant fasl-type-system-stencil-vector)
                             (constant fasl-type-stencil-vector)))
               (put-uptr p mask)
               (vector-for-each (lambda (fasl) (write-fasl p t fasl)) vfasl)))]
          [record (maybe-uid size nflds rtd pad-ty* fld*)
           (if (and strip-source-annotations? (fasl-annotation? x))
               (write-fasl p t (fasl-annotation-stripped x))
               (write-graph p t x
                 (lambda ()
                   (if maybe-uid
                       (begin
                         (put-u8 p (constant fasl-type-rtd))
                         (write-fasl p t maybe-uid))
                       (put-u8 p (constant fasl-type-record)))
                   (put-uptr p size)
                   (put-uptr p nflds)
                   (write-fasl p t rtd)
                   (for-each (lambda (pad-ty fld)
                               (put-u8 p pad-ty)
                               (field-case fld
                                 [ptr (fasl) (write-fasl p t fasl)]
                                 [byte (n) (put-u8 p n)]
                                 [iptr (n) (put-iptr p n)]
                                 [single (n) (put-uptr p n)]
                                 [double (high low)
                                  (put-uptr p high)
                                  (put-uptr p low)]))
                     pad-ty* fld*))))]
          [rtd-ref (uid)
           (write-graph p t x
             (lambda ()
               (put-uptr p 0)
               (write-fasl p t uid)))]
          [closure (offset c)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-closure))
               (put-uptr p offset)
               (write-fasl p t c)))]
          [flonum (high low)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-flonum))
               (put-uptr p high)
               (put-uptr p low)))]
          [large-integer (sign vuptr)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-large-integer))
               (put-u8 p sign)
               (put-uptr p (vector-length vuptr))
               (vector-for-each (lambda (uptr) (put-uptr p uptr)) vuptr)))]
          [eq-hashtable (mutable? subtype minlen veclen vpfasl)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-eq-hashtable))
               (put-u8 p mutable?)
               (put-u8 p subtype)
               (put-uptr p minlen)
               (put-uptr p veclen)
               (put-uptr p (vector-length vpfasl))
               (vector-for-each
                 (lambda (pfasl)
                   (write-fasl p t (car pfasl))
                   (write-fasl p t (cdr pfasl)))
                 vpfasl)))]
          [symbol-hashtable (mutable? minlen equiv veclen vpfasl)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-symbol-hashtable))
               (put-u8 p mutable?)
               (put-uptr p minlen)
               (put-u8 p equiv)
               (put-uptr p veclen)
               (put-uptr p (vector-length vpfasl))
               (vector-for-each
                 (lambda (pfasl)
                   (write-fasl p t (car pfasl))
                   (write-fasl p t (cdr pfasl)))
                 vpfasl)))]
          [code (flags free name arity-mask info pinfo* bytes m vreloc)
           (write-graph p t x
             (lambda ()
               (put-u8 p (constant fasl-type-code))
               (put-u8 p flags)
               (put-uptr p free)
               (put-uptr p (bytevector-length bytes))
               (write-fasl p t name)
               (write-fasl p t arity-mask)
               (if strip-inspector-information?
                   (write-fasl p t (fasl-atom (constant fasl-type-immediate) (constant sfalse)))
                   (write-fasl p t info))
               (if strip-profile-information?
                   (write-fasl p t (fasl-atom (constant fasl-type-immediate) (constant snil)))
                   (write-fasl p t pinfo*))
               (put-bytevector p bytes)
               (put-uptr p m)
               (vector-for-each (lambda (reloc) (write-fasl p t reloc)) vreloc)))]
          [small-integer (iptr)
           (put-u8 p (constant fasl-type-small-integer))
           (put-iptr p iptr)]
          [atom (ty uptr)
           (put-u8 p ty)
           (put-uptr p uptr)]
          [reloc (type-etc code-offset item-offset fasl)
           (put-u8 p type-etc)
           (put-uptr p code-offset)
           (when (fxlogtest type-etc 2) (put-uptr p item-offset))
           (write-fasl p t fasl)]
          [indirect (g i) (write-fasl p t (vector-ref g i))])))

    (define write-string
      (lambda (p x)
        (let ([n (string-length x)])
          (put-uptr p n)
          (do ([i 0 (fx+ i 1)])
            ((fx= i n))
            (put-uptr p (char->integer (string-ref x i)))))))

    (module (fasl-program-info? fasl-library/rt-info? fasl-recompile-info?)
      (import (nanopass))
      (include "base-lang.ss")
      (include "expand-lang.ss")
      (define fasl-program-info? (fasl-record-predicate (record-type-descriptor program-info)))
      (define fasl-library/rt-info? (fasl-record-predicate (record-type-descriptor library/rt-info)))
      (define fasl-recompile-info? (fasl-record-predicate (record-type-descriptor recompile-info))))

    (define keep-revisit-info
      (lambda (x)
        (fasl-case x
          [entry (situation fasl)
           (and (or (eqv? situation (constant fasl-type-revisit))
                    (eqv? situation (constant fasl-type-visit-revisit)))
                x)]
          [header (version machine dependencies) x]
          [else (sorry! "expected entry or header, got ~s" x)])))

    ;; Almost the same as fasl-read, but in a rawer form that exposes
    ;; more of the encoding's structure
    (define describe
      (lambda (x)
        (define-syntax constant-value-case
          (syntax-rules (else)
            [(_ e0 [(k ...) e1 e2 ...] ... [else ee1 ee2 ...])
             (let ([x e0])
               (cond
                 [(memv x (list (constant k) ...)) e1 e2 ...]
                 ...
                 [else ee1 ee2 ...]))]))
        (let ([ht (make-eq-hashtable)])
          (define (build-flonum high low)
            (let ([bv (make-bytevector 8)])
              (bytevector-u64-native-set! bv 0 (bitwise-ior low (bitwise-arithmetic-shift high 32)))
              (bytevector-ieee-double-native-ref bv 0)))
          (define (describe x)
            (cond
              [(not (fasl? x))
               ;; Preumably from the vector of externals
               x]
              [else
               (let ([p (eq-hashtable-cell ht x #f)])
                 (or (cdr p)
                     (let ([self (vector 'CYCLE #f)])
                       (set-cdr! p self)
                       (let ([v (describe-next x)])
                         (vector-set! self 1 v)
                         (set-cdr! p v)
                         v))))]))
          (define (describe-next x)
            (fasl-case x
              [entry (situation fasl)
               (vector 'ENTRY
                       situation
                       (describe fasl))]
              [header (version machine dependencies)
               (vector 'HEADER
                       version
                       machine
                       dependencies)]
              [pair (vfasl)
               (let ([len (vector-length vfasl)])
                 (let loop ([i 0])
                   (let ([e (describe (vector-ref vfasl i))]
                         [i (fx+ i 1)])
                     (if (fx= i len)
                         e
                         (cons e (loop i))))))]
              [tuple (ty vfasl)
               (constant-value-case ty
                 [(fasl-type-box fasl-type-immutable-box)
                  (box (describe (vector-ref vfasl 0)))]
                 [(fasl-type-ratnum)
                  (/ (describe (vector-ref vfasl 0))
                     (describe (vector-ref vfasl 1)))]
                 [(fasl-type-exactnum)
                  (make-rectangular (describe (vector-ref vfasl 0))
                                    (describe (vector-ref vfasl 1)))]
                 [(fasl-type-inexactnum)
                  (make-rectangular (describe (vector-ref vfasl 0))
                                    (describe (vector-ref vfasl 1)))]
                 [(fasl-type-weak-pair)
                  (weak-cons (describe (vector-ref vfasl 0))
                             (describe (vector-ref vfasl 1)))]
                 [(fasl-type-ephemeron)
                  (ephemeron-cons (describe (vector-ref vfasl 0))
                                  (describe (vector-ref vfasl 1)))]
                 [(fasl-type-base-rtd)
                  #!base-rtd]
                 [else
                  'unknown])]
              [string (ty string)
               (constant-value-case ty
                 [(fasl-type-symbol) (string->symbol string)]
                 [else string])]
              [gensym (pname uname) (gensym pname uname)]
              [vector (ty vfasl) (vector-map describe vfasl)]
              [fxvector (viptr) viptr]
              [bytevector (ty bv) bv]
              [stencil-vector (ty vfasl sys?) (vector-map describe vfasl)]
              [record (maybe-uid size nflds rtd pad-ty* fld*)
               (vector 'RECORD
                       (and maybe-uid (describe maybe-uid))
                       size
                       nflds
                       (describe rtd)
                       (map (lambda (fld)
                               (field-case fld
                                 [ptr (fasl) (describe fasl)]
                                 [byte (n) n]
                                 [iptr (n) n]
                                 [single (n) n]
                                 [double (high low) (build-flonum high low)]))
                            fld*))]
              [rtd-ref (uid) (vector 'RTD (describe uid))]
              [closure (offset c)
                       (vector 'CLOSURE
                               offset
                               (describe c))]
              [flonum (high low) (build-flonum high low)]
              [large-integer (sign vuptr)
               (let loop ([v 0] [i 0])
                 (cond
                   [(fx= i (vector-length vuptr))
                    (if (eqv? sign 1) (- v) v)]
                   [else (loop (bitwise-ior (bitwise-arithmetic-shift v (constant bigit-bits))
                                            (vector-ref vuptr i))
                               (fx+ i 1))]))]
              [eq-hashtable (mutable? subtype minlen veclen vpfasl)
               (let ([ht (make-eq-hashtable)])
                 (vector-for-each
                  (lambda (pfasl)
                    (eq-hashtable-set! ht (car pfasl) (cdr pfasl)))
                  vpfasl)
                 ht)]
              [symbol-hashtable (mutable? minlen equiv veclen vpfasl)
               (let ([ht (make-eq-hashtable)])
                 (vector-for-each
                  (lambda (pfasl)
                    (eq-hashtable-set! ht (car pfasl) (cdr pfasl)))
                  vpfasl)
                 ht)]
              [code (flags free name arity-mask info pinfo* bytes m vreloc)
               (vector 'CODE
                       flags
                       free
                       (describe name)
                       (describe arity-mask)
                       (describe info)
                       (describe pinfo*)
                       bytes
                       m
                       (vector-map describe vreloc))]
              [small-integer (iptr) iptr]
              [atom (ty uptr)
               (constant-value-case ty
                 [(fasl-type-immediate)
                  (constant-value-case uptr
                    [(snil) '()]
                    [(sfalse) #f]
                    [(strue) #f]
                    [(seof) #!eof]
                    [(sbwp) #!bwp]
                    [(svoid) (void)]
                    [else (vector 'IMMEDIATE uptr)])]
                 [(fasl-type-entry) (vector 'ENTRY uptr)]
                 [(fasl-type-library) (vector 'LIBRARY uptr)]
                 [(fasl-type-library-code) (vector 'LIBRARY-CODE uptr)]
                 [else x])]
              [reloc (type-etc code-offset item-offset fasl)
               (vector 'RELOC
                       type-etc
                       code-offset
                       item-offset
                       (describe fasl))]
              [indirect (g i) (describe (vector-ref g i))]
              [else x]))
          (describe x))))

    (set-who! $fasl-strip-options (make-enumeration '(inspector-source profile-source source-annotations compile-time-information)))
    (set-who! $make-fasl-strip-options (enum-set-constructor $fasl-strip-options))

    (let ()
      (define read-and-strip-from-port
        (lambda (ip ifn init-g)
          (let* ([script-header (read-script-header ip)]
                 [mode (and script-header ifn (unless-feature windows (get-mode ifn)))])
            (let loop ([rentry* '()])
              (set! fasl-count (fx+ fasl-count 1))
              (let ([entry (read-entry ip init-g)])
                (if (eof-object? entry)
                    (begin
                      (close-port ip)
                      (values script-header mode (reverse rentry*)))
                    (let ([entry (if strip-compile-time-information? (keep-revisit-info entry) entry)])
                      (loop (if entry (cons entry rentry*) rentry*)))))))))
      (define read-and-strip-file
        (lambda (ifn)
          (let ([ip ($open-file-input-port fasl-who ifn)])
            (on-reset (close-port ip)
                      (read-and-strip-from-port ip ifn #f)))))
      (define convert-fasl-file
        (lambda (who ifn ofn options write)
          (unless (string? ifn) ($oops who "~s is not a string" ifn))
          (unless (string? ofn) ($oops who "~s is not a string" ofn))
          (unless (and (enum-set? options) (enum-set-subset? options $fasl-strip-options))
            ($oops who "~s is not a fasl-strip-options object" options))
          (fluid-let ([strip-inspector-information? (enum-set-subset? (fasl-strip-options inspector-source) options)]
                      [strip-profile-information? (enum-set-subset? (fasl-strip-options profile-source) options)]
                      [strip-source-annotations? (enum-set-subset? (fasl-strip-options source-annotations) options)]
                      [strip-compile-time-information? (enum-set-subset? (fasl-strip-options compile-time-information) options)]
                      [fasl-who who]
                      [fasl-count 0])
            (let-values ([(script-header mode entry*) (read-and-strip-file ifn)])
              (let ([op ($open-file-output-port who ofn (file-options replace))])
                (on-reset (delete-file ofn #f)
                  (on-reset (close-port op)
                    (let ([result (write script-header mode entry* op)])
                      (close-port op)
                      (unless-feature windows (when mode (chmod ofn mode)))
                      result))))))))
      (set-who! $describe-fasl-from-port
        (rec $describe-fasl-from-port
          (case-lambda
           [(ip) ($describe-fasl-from-port ip '#())]
           [(ip externals)
            (unless (input-port? ip) ($oops who "~s is not an input port" ip))
            (fluid-let ([strip-inspector-information? #f]
                        [strip-profile-information? #f]
                        [strip-source-annotations? #f]
                        [strip-compile-time-information? #f]
                        [fasl-who who]
                        [fasl-count 0])
              (let-values ([(script-header mode entry*) (read-and-strip-from-port ip #f externals)])
                (list (and script-header (describe script-header))
                      (map describe entry*))))])))
      (set-who! strip-fasl-file
        (lambda (ifn ofn options)
          (convert-fasl-file who ifn ofn options
                             (lambda (script-header mode entry* op)
                               (when script-header (put-bytevector op script-header))
                               (for-each (lambda (entry) (write-entry op entry)) entry*)))))
      (set-who! pbchunk-convert-file
        (lambda (ifn ofn c-ofns reg-proc-names start-index)
          (unless (string? ifn) ($oops who "~s is not a string" ifn))
          (unless (string? ofn) ($oops who "~s is not a string" ofn))
          (unless (and (pair? c-ofns) (list? c-ofns) (andmap string? c-ofns))
            ($oops who "~s is not a nonempty list of strings" c-ofns))
          (unless (and (pair? reg-proc-names) (list? reg-proc-names) (andmap string? reg-proc-names))
            ($oops who "~s is not a nonempty list of strings" reg-proc-names))
          (unless (and (fixnum? start-index) (fx>= start-index 0))
            ($oops who "~s is not a nonnegative fixnum" start-index))
          (unless (fx= (length c-ofns) (length reg-proc-names))
            ($oops who "length of file-name list ~s does not match the length of function-name list ~s"
                   c-ofns
                   reg-proc-names))
          (convert-fasl-file who ifn ofn (fasl-strip-options)
                             (lambda (script-header mode entry* op)
                               ($fasl-pbchunk!
                                who
                                c-ofns
                                reg-proc-names
                                start-index
                                entry*
                                handle-entry
                                (lambda ()
                                  (when script-header (put-bytevector op script-header))
                                  (for-each (lambda (entry) (write-entry op entry)) entry*)))))))
      (set-who! vfasl-convert-file
        (lambda (ifn ofn bootfile*)
          (convert-fasl-file who ifn ofn (fasl-strip-options)
                             (lambda (script-header mode entry* op)
                               (when bootfile*
                                 ($emit-boot-header op (constant machine-type-name) bootfile*))
                               (let* ([write-out
                                       (lambda (x situation)
                                         (let ([bv ($fasl-to-vfasl x)])
                                           ($write-fasl-bytevectors op (list bv) (bytevector-length bv)
                                                                    ;; see "promoting" below:
                                                                    (constant fasl-type-visit-revisit)
                                                                    (constant fasl-type-vfasl))))]
                                      [write-out-accum (lambda (accum situation)
                                                         (unless (null? accum)
                                                           (if (null? (cdr accum))
                                                               (write-out (car accum) situation)
                                                               (write-out (fasl-vector (constant fasl-type-vector)
                                                                                       (list->vector (reverse accum)))
                                                                          situation))))])
                                 (let loop ([ignore-header? #f] [accum '()] [accum-situation #f] [entry* entry*])
                                   (cond
                                     [(null? entry*)
                                      (write-out-accum accum accum-situation)]
                                     [else
                                      (handle-entry
                                       (car entry*)
                                       (lambda (write-k)
                                         (unless ignore-header?
                                           (write-k op))
                                         (loop #t accum accum-situation (cdr entry*)))
                                       (lambda (situation x)
                                         (cond
                                           [(vector? x)
                                            (loop #t
                                                  (append (reverse (vector->list x)) accum)
                                                  situation
                                                  (cdr entry*))]
                                           [(or (not ($fasl-can-combine? x))
                                                ;; improve sharing by promiting everyting to visit-revisit,
                                                ;; instead of comparing situations
                                                #;
                                                (and accum-situation
                                                     (not (eqv? accum-situation situation))))
                                            (write-out-accum accum accum-situation)
                                            (write-out x situation)
                                            (loop #t '() #f (cdr entry*))]
                                           [else
                                            (loop #t (cons x accum) situation (cdr entry*))])))])))))))))

  (let ()
    ; per file
    (define-threaded fail)
    (define-threaded eq-hashtable-warning-issued?)
    ; per entry
    (define-threaded cmp-ht)
    (define-threaded gensym-table)

    (define-syntax cmp-case
      (lambda (x)
        (define (make-clause t x-case top-e)
          (lambda (variant arg* e)
            (with-syntax ([(arg1 ...) (map (lambda (x) (construct-name x x "1")) arg*)]
                          [(arg2 ...) (map (lambda (x) (construct-name x x "2")) arg*)]
                          [variant variant]
                          [e e]
                          [t t]
                          [x-case x-case]
                          [top-e top-e])
              #'[variant (arg1 ...)
                 (or (x-case t
                       [variant (arg2 ...) e]
                       [else #f])
                     (fail 'variant top-e))])))
        (syntax-case x ()
          [(_ x-case e1 e2 [variant (arg ...) e] ...)
           #`(let ([t2 e2])
               (x-case e1
                 #,@(map (make-clause #'t2 #'x-case #'e1) #'(variant ...) #'((arg ...) ...) #'(e ...))))])))

    (define-who vandmap
      (lambda (p v1 v2)
        (let ([n (vector-length v1)])
          (and (fx= (vector-length v2) n)
               (let f ([i 0])
                 (or (fx= i n)
                     (and (p (vector-ref v1 i) (vector-ref v2 i))
                          (f (fx+ i 1)))))))))

    (define fld=?
      (lambda (fld1 fld2)
        (cmp-case field-case fld1 fld2
          [ptr (fasl) (fasl=? fasl1 fasl2)]
          [byte (n) (eqv? n1 n2)]
          [iptr (n) (eqv? n1 n2)]
          [single (n) (eqv? n1 n2)]
          [double (high low)
           (and (eqv? high1 high2)
                (eqv? low1 low2))])))

    (define (fasl=? entry1 entry2)
      (let ([entry1 (follow-indirect entry1)] [entry2 (follow-indirect entry2)])
        (let ([a (eq-hashtable-cell cmp-ht entry1 #f)]
              [b (eq-hashtable-cell cmp-ht entry2 #f)])
          (or (and (eq? entry2 (cdr a))
                   (eq? entry1 (cdr b)))
              (and (or (not (cdr a)) (fail 'sharing1 entry1))
                   (or (not (cdr b)) (fail 'sharing2 entry2))
                   (begin
                     (set-cdr! a entry2)
                     (set-cdr! b entry1)
                     (cmp-case fasl-case entry1 entry2
                       [entry (situation fasl) (and (= situation1 situation2) (fasl=? fasl1 fasl2))]
                       [header (version machine dependencies)
                        (and (equal? version1 version2)
                             (equal? machine1 machine2)
                             (equal? dependencies1 dependencies2))]
                       [pair (vfasl) (vandmap fasl=? vfasl1 vfasl2)]
                       [tuple (ty vfasl) (and (eqv? ty1 ty2) (vandmap fasl=? vfasl1 vfasl2))]
                       [string (ty string) (and (eqv? ty1 ty2) (string=? string1 string2))]
                       [gensym (pname uname)
                        (and (string=? pname1 pname2)
                             (let ([x (hashtable-ref gensym-table uname1 #f)])
                               (if (not x)
                                   (hashtable-set! gensym-table uname1 uname2)
                                   (string=? x uname2))))]
                       [vector (ty vfasl) (and (eqv? ty1 ty2) (vandmap fasl=? vfasl1 vfasl2))]
                       [fxvector (viptr) (vandmap = viptr1 viptr2)]
                       [bytevector (ty bv) (and (eqv? ty1 ty2) (bytevector=? bv1 bv2))]
                       [stencil-vector (mask vfasl sys?) (and (eqv? mask1 mask2)
                                                              (eqv? sys?1 sys?2)
                                                              (vandmap fasl=? vfasl1 vfasl2))]
                       [record (maybe-uid size nflds rtd pad-ty* fld*)
                        (and (if maybe-uid1
                                 (and maybe-uid2 (fasl=? maybe-uid1 maybe-uid2))
                                 (not maybe-uid2))
                             (eqv? size1 size2)
                             (eqv? nflds1 nflds2)
                             (fasl=? rtd1 rtd2)
                             (andmap eqv? pad-ty*1 pad-ty*2)
                             (andmap fld=? fld*1 fld*2))]
                       [rtd-ref (uid) (eq? uid1 uid2)]
                       [closure (offset c) (and (eqv? offset1 offset2) (fasl=? c1 c2))]
                       [flonum (high low)
                        (and (eqv? high1 high2)
                             (eqv? low1 low2))]
                       [large-integer (sign vuptr) (and (eqv? sign1 sign2) (vandmap = vuptr1 vuptr2))]
                       [eq-hashtable (mutable? subtype minlen veclen vpfasl)
                        (and (eqv? mutable?1 mutable?2)
                             (eqv? subtype1 subtype2)
                             (eqv? minlen1 minlen2)
                             ; don't care if veclens differ
                             #;(eqv? veclen1 veclen2)
                             ; making gross assumption that equal-length hashtables are equal.
                             ; actual eq-hashtable equivalence is hard.
                             (fx= (vector-length vpfasl1) (vector-length vpfasl2))
                             (begin
                               (unless (or (fx= (vector-length vpfasl1) 0) eq-hashtable-warning-issued?)
                                 (set! eq-hashtable-warning-issued? #t)
                                 (warning fasl-who "punting on comparison of eq-hashtable entries"))
                               #t))]
                       [symbol-hashtable (mutable? minlen equiv veclen vpfasl)
                        (let ()
                          (define keyval?
                            (lambda (x1 x2)
                              (fasl-case (car x1)
                                [gensym (pname1 uname1)
                                  (fasl-case (car x2)
                                    [gensym (pname2 uname2) (string<? uname1 uname2)]
                                    [string (ty2 string2) #t]
                                    [else (sorry! "unexpected key ~s" x2)])]
                                [string (ty1 string1)
                                  (fasl-case (car x2)
                                    [gensym (pname2 uname2) #f]
                                    [string (ty2 string2) (string<? string1 string2)]
                                    [else (sorry! "unexpected key ~s" x2)])]
                                [else (sorry! "unexpected key ~s" x1)])))
                          (and (eqv? mutable?1 mutable?2)
                               (eqv? minlen1 minlen2)
                               (eqv? equiv1 equiv2)
                               ; don't care if veclens differ
                               #;(eqv? veclen1 veclen2)
                               (vandmap (lambda (x y) (and (fasl=? (car x) (car y)) (fasl=? (cdr x) (cdr y))))
                                 (vector-sort keyval? vpfasl1)
                                 (vector-sort keyval? vpfasl2))))]
                       [code (flags free name arity-mask info pinfo* bytes m reloc)
                        (and (eqv? flags1 flags2)
                             (eqv? free1 free2)
                             (fasl=? name1 name2)
                             (fasl=? arity-mask1 arity-mask2)
                             (fasl=? info1 info2)
                             (fasl=? pinfo*1 pinfo*2)
                             (bytevector=? bytes1 bytes2)
                             (eqv? m1 m2)
                             (vandmap fasl=? reloc1 reloc2))]
                       [small-integer (iptr) (eqv? iptr1 iptr2)]
                       [atom (ty uptr) (and (eqv? ty1 ty2) (eqv? uptr1 uptr2))]
                       [reloc (type-etc code-offset item-offset fasl)
                        (and (eqv? type-etc1 type-etc2)
                             (eqv? code-offset1 code-offset2)
                             (eqv? item-offset1 item-offset2)
                             (fasl=? fasl1 fasl2))]
                       [indirect (g i) (sorry! "unexpected indirect")])))))))

    (set-who! $fasl-file-equal?
      (rec fasl-file-equal?
        (case-lambda
          [(ifn1 ifn2) (fasl-file-equal? ifn1 ifn2 #f)]
          [(ifn1 ifn2 error?) (fasl-file-equal? ifn1 ifn2 error? #f)]
          [(ifn1 ifn2 error? detail?)
           (unless (string? ifn1) ($oops who "~s is not a string" ifn1))
           (unless (string? ifn2) ($oops who "~s is not a string" ifn2))
           (fluid-let ([fasl-who who]
                       [fasl-count 0]
                       [fail (if error?
                                 (lambda (what where) (bogus "~s comparison failed while comparing ~a and ~a~a" what ifn1 ifn2
                                                             (if detail?
                                                                 (format " at ~s" where)
                                                                 "")))
                                 (lambda (what where) #f))]
                       [eq-hashtable-warning-issued? #f])
             (call-with-port ($open-file-input-port who ifn1)
               (lambda (ip1)
                 (on-reset (close-port ip1)
                   (call-with-port ($open-file-input-port who ifn2)
                     (lambda (ip2)
                       (on-reset (close-port ip2)
                         (let ([script-header1 (read-script-header ip1)]
                               [script-header2 (read-script-header ip2)])
                           (if (equal? script-header1 script-header2)
                               (let loop ()
                                 (set! fasl-count (fx+ fasl-count 1))
                                 (let ([entry1 (read-entry ip1 #f)] [entry2 (read-entry ip2 #f)])
                                   (if (eof-object? entry1)
                                       (or (eof-object? entry2)
                                           (and error? (bogus "~a has fewer fasl entries than ~a" ifn1 ifn2)))
                                       (if (eof-object? entry2)
                                           (and error? (bogus "~a has fewer fasl entries than ~a" ifn2 ifn1))
                                           (and (fluid-let ([cmp-ht (make-eq-hashtable)]
                                                            [gensym-table (make-hashtable string-hash string=?)])
                                                  (fasl=? entry1 entry2))
                                                (loop))))))
                               (and error? (bogus "script headers ~s and ~s differ" script-header1 script-header2)))))))))))])))))
