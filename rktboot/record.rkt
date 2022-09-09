#lang racket/base
(require (for-syntax racket/base)
         racket/unsafe/ops
         racket/vector
         racket/list
         "immediate.rkt"
         "symbol.rkt"
         "gensym.rkt"
         "constant.rkt")

(provide do-$make-record-type
         register-rtd-name!
         register-rtd-fields!
         register-rtd-ancestors!
         s:struct-type?

         $make-record-type
         $make-record-type-descriptor
         $record
         make-record-type
         type-descriptor
         record-predicate
         record-accessor
         record-mutator
         compile-time-record-predicate
         compile-time-record-accessor
         compile-time-record-mutator
         csv7:record-field-accessor
         csv7:record-field-mutator
         csv7:record-field-mutable?
         record-rtd
         record?
         $record?
         record-type-uid
         record-type-name
         record-type-sealed?
         record-type-opaque?
         record-type-parent
         record-type-field-names
         record-type-field-indices
         csv7:record-type-field-names
         $record-type-field-indices
         csv7:record-type-field-decls
         record-writer
         $object-ref)

;; Let there be records: this declaration is the root origin of
;; #!base-rtd. From this description, #!base-rtd gets fasled in a boot
;; file and loaded to define #!base-rtd on startup. The field offsets
;; below don't matter, since they're fixed up for the target plaform.
(define base-rtd-fields
  (map vector-copy
       '(#(fld ancestry #f scheme-object 9)
         #(fld size #f scheme-object 17)
         #(fld pm #f scheme-object 25)
         #(fld mpm #f scheme-object 33)
         #(fld name #f scheme-object 41)
         #(fld flds #f scheme-object 49)
         #(fld flags #f scheme-object 57)
         #(fld uid #f scheme-object 65)
         #(fld counts #f scheme-object 73))))

(define base-rtd-ancestry (vector #f base-rtd))
(define ANCESTRY-PARENT-OFFSET 2)

(define (s:struct-type? v)
  (or (struct-type? v)
      (base-rtd? v)))

;; For rtds based on subtypes of #!base-rtd, the subtype instance
;; that effectively extends the struct type with more fields:
(define rtd-extensions (make-weak-hasheq))

;; For structure types that extend #!base-rtd:
(struct base-rtd-subtype () #:prefab)

(define (subtype-of-base-rtd? rtd)
  (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
    (struct-type-info rtd))
  (and super
       (or (eq? struct:base-rtd-subtype super)
           (and (subtype-of-base-rtd? super)))))


(define (do-$make-record-type in-base-rtd in-super in-name fields sealed? opaque? more
                              #:uid [in-uid #f])
  (define name (cond
                 [(string? in-name) (string->symbol in-name)]
                 [(gensym? in-name) (string->symbol (gensym->pretty-string in-name))]
                 [else in-name]))
  (define uid (or in-uid
                  (cond
                    [(gensym? in-name) in-name]
                    [else #f])))
  (define super
    (cond
      [(base-rtd? in-super) struct:base-rtd-subtype]
      [else in-super]))
  (define num-fields (if (vector? fields) (vector-length fields) (length fields)))
  (define-values (struct:name make-name name? name-ref name-values)
    (make-struct-type (or uid name) super num-fields 0 #f null (and uid 'prefab)))
  (unless (base-rtd? in-base-rtd)
    (hash-set! rtd-extensions struct:name (apply (struct-type-make-constructor in-base-rtd) more)))
  (register-rtd-name! struct:name name)
  (register-rtd-fields! struct:name fields)
  (register-rtd-ancestors! struct:name super)
  (when sealed? (hash-set! rtd-sealed?s struct:name #t))
  (when (or opaque?
            (and super (hash-ref rtd-opaque?s super #f)))
    (hash-set! rtd-opaque?s struct:name #t))
  struct:name)

(define ($make-record-type in-base-rtd super in-name fields sealed? opaque? . more)
  (do-$make-record-type in-base-rtd super in-name fields sealed? opaque? more))

(define ($make-record-type-descriptor base-rtd name parent uid sealed? opaque? fields who . extras)
  (do-$make-record-type base-rtd parent name (vector->list fields) sealed? opaque? extras #:uid uid))

(define ($record rtd . args)
  (cond
    [(base-rtd? rtd)
     (error "here")]
    [(subtype-of-base-rtd? rtd)
     (error "here, too" rtd args)]
    [else
     (apply (struct-type-make-constructor rtd) args)]))

(define make-record-type
  (case-lambda
    [(parent in-name fields)
     ($make-record-type base-rtd parent in-name fields #f #f)]
    [(name fields)
     (make-record-type #f name fields)]))


(define rtd-names (make-weak-hasheq))

(define (register-rtd-name! struct:name name)
  (hash-set! rtd-names struct:name name))

(define rtd-ancestors (make-weak-hasheq))

(define (register-rtd-ancestors! struct:name parent)
  ;; ancestry vector is `(vector #f ... parent self)`
  (unless (hash-ref rtd-ancestors struct:name #f)
    (cond
      [(not parent)
       (hash-set! rtd-ancestors struct:name (vector #f struct:name))]
      [(eq? parent struct:base-rtd-subtype)
       (hash-set! rtd-ancestors struct:name (vector #f base-rtd struct:name))]
      [else
       (define p-vec (hash-ref rtd-ancestors parent))
       (define vec (make-vector (+ 1 (vector-length p-vec)) struct:name))
       (vector-copy! vec 0 p-vec)
       (hash-set! rtd-ancestors struct:name vec)])))

(define rtd-fields (make-weak-hasheq))

;; Must match "cmacro.ss"
(define (fld-name fld) (vector-ref fld 1))
(define (fld-mutable? fld) (vector-ref fld 2))
(define (fld-type fld) (vector-ref fld 3))
(define (fld-byte fld) (vector-ref fld 4))
(define (set-fld-byte! fld v) (vector-set! fld 4 v))
(define fld-byte-value 0) ; doesn't matter; gets replaced in field vectors

(define (register-rtd-fields! struct:name fields)
  (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
    (struct-type-info struct:name))
  (hash-set! rtd-fields struct:name (append
                                     (cond
                                       [(not super) null]
                                       [(or (base-rtd? super)
                                            (eq? super struct:base-rtd-subtype))
                                        ;; fields added in `csv7:record-field-accessor`
                                        null]
                                       [else (hash-ref rtd-fields super)])
                                     (normalize-fields
                                      (if (vector? fields)
                                          (for/list ([e (in-vector fields)])
                                            (cond
                                              [(symbol? e) (list 'immutable e)]
                                              [(pair? (cdr e)) (list (car e) (cadr e))]
                                              [else e]))
                                          fields)))))

(define (normalize-fields fields)
  (unless (list? fields)
    (error 'normalize-fields "not a list: ~s" fields))
  (define (check-type t)
    (case t
      [(scheme-object uptr ptr double) t]
      [else
       (error 'make-struct-type "unsupported type ~s" t)]))
  (define (is-mut? m)
    (case m
      [(mutable) #t]
      [(immutable) #f]
      [else (error 'make-struct-type "unrecognized mutability ~s" m)]))
  (for/list ([field (in-list fields)])
    (cond
      [(and (vector? field)
            (= 3 (vector-length field)))
       (vector 'fld (vector-ref field 2) (is-mut? (vector-ref field 1)) (check-type (vector-ref field 0)) fld-byte-value)]
      [(and (list? field)
            (= 3 (length field)))
       (vector 'fld (list-ref field 2) (is-mut? (list-ref field 0)) (check-type (list-ref field 1)) fld-byte-value)]
      [(symbol? field)
       (vector 'fld field #t 'scheme-object fld-byte-value)]
      [(and (list? field)
            (= 2 (length field)))
       (vector 'fld (cadr field) (is-mut? (car field)) 'scheme-object fld-byte-value)]
      [else
       (error 'normalize-fields "unrecognized field format: ~s" field)])))

(define-syntax (type-descriptor stx)
  (syntax-case stx ()
    [(_ id)
     (car (syntax-local-value #'id))]))

(define (record-predicate rtd)
  (cond
    [(base-rtd? rtd)
     (lambda (v)
       (or (base-rtd? v)
           (base-rtd-subtype? v)))]
    [else
     (define pred (struct-type-make-predicate rtd))
     (lambda (v)
       (if (struct-type? v)
           (pred (hash-ref rtd-extensions v #f))
           (pred v)))]))

(define (compile-time-record-predicate rtd)
  (and (not (base-rtd-subtype-rtd? rtd))
       (struct-type-make-predicate rtd)))

(define (base-rtd-subtype-rtd? rtd)
  (or (eq? struct:base-rtd-subtype rtd)
      (let ()
        (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
          (struct-type-info rtd))
        (if super
            (base-rtd-subtype-rtd? super)
            #f))))

;; `i` does not count parent fields
(define (record-accessor rtd i [name #f])
  (cond
    [(base-rtd? rtd)
     (error 'record-accessor "#!base-rtd not directly supported")]
    [else
     (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
       (struct-type-info rtd))
     (define acc (make-struct-field-accessor ref i (or name (string->symbol (number->string i)))))
     (if (subtype-of-base-rtd? rtd)
         (lambda (rtd/ext)
           (acc (if (struct-type? rtd/ext)
                    (hash-ref rtd-extensions rtd/ext)
                    rtd/ext)))
         acc)]))

(define (compile-time-record-accessor rtd i)
  (and (not (base-rtd-subtype-rtd? rtd))
       (let ()
         (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
           (struct-type-info rtd))
         (make-struct-field-accessor ref i))))

;; `i` does not count parent fields
(define (record-mutator rtd i [name #f])
  (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
    (struct-type-info rtd))
  (make-struct-field-mutator set i name))

(define (compile-time-record-mutator rtd i)
  (and (not (base-rtd-subtype-rtd? rtd))
       (let ()
         (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
           (struct-type-info rtd))
         (make-struct-field-mutator set i))))

;; If `sym/i` is an integer, it *does* count parent fields
(define (csv7:record-field-accessor/mutator rtd sym/i mut?)
  (define (lookup-field-by-name rtd sym)
    (define fields (hash-ref rtd-fields rtd))
    (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
      (struct-type-info rtd))
    (or (for/or ([field (in-list fields)]
                 [i (in-naturals)])
          (define name (fld-name field))
          (and (eq? sym name)
               (lookup-field-by-pos rtd i name)))
        (error 'csv7:record-field-accessor
               "cannot find ~a ~s in ~s"
               (if mut? "mutator" "accessor")
               sym
               fields)))
  ;; returns either a procedure or a number for a count of fields (less than `i`)
  (define (lookup-field-by-pos rtd i [name #f] #:must-proc? [must-proc? #f])
    (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
      (struct-type-info rtd))
    (cond
      [(not super)
       (if (i . >= . init-cnt)
           (if must-proc?
               (error 'csv7:record-field-accessor/mutator "field count too large: ~a" i)
               init-cnt)
           (if mut?
               (make-struct-field-mutator set i name)
               (make-struct-field-accessor ref i name)))]
      [else
       (define s-proc (lookup-field-by-pos super i name))
       (cond
         [(integer? s-proc)
          (if (i . >= . (+ s-proc init-cnt))
              (if must-proc?
                  (error 'csv7:record-field-accessor/mutator "field count too large: ~a" i)
                  (+ s-proc init-cnt))
              (if mut?
                  (make-struct-field-mutator set (- i s-proc) name)
                  (make-struct-field-accessor ref (- i s-proc) name)))]
         [else s-proc])]))
  (define (ptr-type? t)
    (case t
      [(scheme-object ptr) #t]
      [(uptr double) #f]
      [else (error "unrecognized type")]))
  (define (assert-accessor)
    (when mut? (error 'csv7:record-field-mutator "immutable base-rtd field")))
  (cond
    [(or (base-rtd? rtd)
         (subtype-of-base-rtd? rtd))
     (case sym/i
       [(flds)
        (assert-accessor)
        (lambda (rtd)
          (fix-offsets
           (append
            (if (or (base-rtd? rtd)
                    (subtype-of-base-rtd? rtd))
                base-rtd-fields
                null)
            (if (base-rtd? rtd)
                null
                (hash-ref rtd-fields rtd)))))]
       [(ancestry)
        (assert-accessor)
        (lambda (rtd)
          (cond
            [(base-rtd? rtd) base-rtd-ancestry]
            [else
             (define vec (hash-ref rtd-ancestors rtd))
             (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
               (struct-type-info rtd))
             (define parent
               (if (eq? super struct:base-rtd-subtype)
                   base-rtd
                   super))
             (unless (eq? parent (vector-ref vec (- (vector-length vec) ANCESTRY-PARENT-OFFSET)))
               (error "ancestry sanity check failed" rtd vec parent))
             vec]))]
       [(size)
        (assert-accessor)
        (lambda (rtd)
          (let loop ([flds ((csv7:record-field-accessor base-rtd 'flds) rtd)] [x ptr-bytes])
            (cond
              [(null? flds) x]
              [(eq? (fld-type (car flds)) 'double)
               (let ([x (if (zero? (modulo x max-float-alignment))
                            x
                            (+ x (- 8 (modulo x max-float-alignment))))])
                 (loop (cdr flds) (+ x 8)))]
              [else (loop (cdr flds) (+ x ptr-bytes))])))]
       [(pm)
        (assert-accessor)
        (lambda (rtd)
          (define flds ((csv7:record-field-accessor base-rtd 'flds) rtd))
          (cond
            [(for/and ([fld (in-list flds)])
               (ptr-type? (fld-type fld)))
             -1]
            [else
             (for/fold ([m 1]) ([fld (in-list flds)]
                                [i (in-naturals 1)]) ; start after base rtd
               (if (ptr-type? (fld-type fld))
                   (bitwise-ior m (arithmetic-shift 1 i))
                   m))]))]
       [(mpm)
        (assert-accessor)
        (lambda (rtd)
          (for/fold ([m 0]) ([fld (in-list ((csv7:record-field-accessor base-rtd 'flds) rtd))]
                             [i (in-naturals 1)])    ; start after base rtd
            (if (and (fld-mutable? fld)
                     (ptr-type? (fld-type fld)))
                (bitwise-ior m (arithmetic-shift 1 i))
                m)))]
       [(name)
        (assert-accessor)
        record-type-name]
       [(uid)
        (assert-accessor)
        record-type-uid]
       [(flags)
        (assert-accessor)
        (lambda (rtd)
          (bitwise-ior
           (if (hash-ref rtd-opaque?s rtd #f)
               (lookup-constant 'rtd-opaque)
               0)
           (if (hash-ref rtd-sealed?s rtd #f)
               (lookup-constant 'rtd-sealed)
               0)))]
       [(counts)
        (assert-accessor)
        (lambda (rtd) #f)]
       [else
        (cond
          [(and (integer? sym/i)
                (base-rtd? rtd))
           (assert-accessor)
           (csv7:record-field-accessor rtd (fld-name (list-ref base-rtd-fields sym/i)))]
          [(not (base-rtd? rtd))
           (define proc (if (integer? sym/i)
                            (lookup-field-by-pos rtd (- sym/i (length base-rtd-fields)) #:must-proc? #t)
                            (lookup-field-by-name rtd sym/i)))
           (if mut?
               (lambda (rtd/ext v)
                 (proc (if (struct-type? rtd/ext)
                           (hash-ref rtd-extensions rtd/ext)
                           rtd/ext)
                       v))
               (lambda (rtd/ext)
                 (proc (if (struct-type? rtd/ext)
                           (hash-ref rtd-extensions rtd/ext)
                           rtd/ext))))]
          [else
           (error "unknown base-rtd field:" sym/i)])])]
    [(integer? sym/i)
     (lookup-field-by-pos rtd sym/i #:must-proc? #t)]
    [else
     (lookup-field-by-name rtd sym/i)]))

;; If `sym/i` is an integer, it *does* count parent fields
(define (csv7:record-field-accessor rtd sym/i)
  (csv7:record-field-accessor/mutator rtd sym/i #f))

;; If `sym/i` is an integer, it *does* count parent fields
(define (csv7:record-field-mutator rtd sym/i)
  (csv7:record-field-accessor/mutator rtd sym/i #t))

;; `i` *does* count parent fields
(define (csv7:record-field-mutable? rtd i)
  (cond
    [(or (base-rtd? rtd)
         (subtype-of-base-rtd? rtd))
     (error 'csv7:record-field-mutable? "not yet supported")]
    [else
     (define fields (hash-ref rtd-fields rtd))
     (define f (list-ref fields i))
     (fld-mutable? f)]))

(define (record-rtd v)
  (cond
    [(base-rtd? v) base-rtd]
    [(struct? v)
     (define-values (s skipped?) (struct-info v))
     s]
    [(hash-ref rtd-extensions v #f)
     => (lambda (ext)
          (define-values (rtd skipped?) (struct-info ext))
          rtd)]
    [(struct-type? v) base-rtd]
    [else (error 'record-rtd "not a record: ~s" v)]))

(define record?
  (case-lambda
    [(v)
     (and (not (bwp? v))
          (not (black-hole? v))
          (not ($unbound-object? v))
          (or (struct? v)
              (struct-type? v)
              (base-rtd? v)))]
    [(v rtd)
     (and (or (struct? v)
              (struct-type? v)
              (base-rtd? v))
          ((record-predicate rtd) v))]))

(define ($record? v)
  (record? v))

(define (record-type-uid rtd)
  (cond
    [(base-rtd? rtd) '$base-rtd]
    [else
     (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
       (struct-type-info rtd))
     r-name]))

(define (record-type-name rtd)
  (cond
    [(base-rtd? rtd)
     '$base-rtd]
    [else
     (hash-ref rtd-names rtd)]))

(define (record-type-parent rtd)
  (cond
    [(base-rtd? rtd) #f]
    [else
     (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
       (struct-type-info rtd))
     super]))

;; all fields, including from parent
(define (csv7:record-type-field-names rtd)
  (cond
    [(base-rtd? rtd)
     (map fld-name base-rtd-fields)]
    [else
     (map fld-name (hash-ref rtd-fields rtd))]))

;; all fields, including from parent
(define ($record-type-field-indices rtd)
  (cond
    [(base-rtd? rtd)
     (for/list ([f (in-list base-rtd-fields)]
                [i (in-naturals)])
       i)]
    [else
     (for/list ([f (in-list (hash-ref rtd-fields rtd))]
                [i (in-naturals)])
       i)]))

;; does not include parent fields
(define (record-type-field-names rtd)
  (cond
    [(base-rtd? rtd)
     (list->vector (csv7:record-type-field-names rtd))]
    [else
     (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
       (struct-type-info rtd))
     (define all-fields (hash-ref rtd-fields rtd))
     (define fields (reverse (take (reverse all-fields) init-cnt)))
     (list->vector (map fld-name fields))]))

;; does not include parent fields
(define (record-type-field-indices rtd)
  (cond
    [(base-rtd? rtd)
     (list->vector ($record-type-field-indices rtd))]
    [else
     (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
       (struct-type-info rtd))
     (for/vector ([i (in-range init-cnt)])
       i)]))

(define (csv7:record-type-field-decls rtd)
  (map (lambda (v) (list (if (fld-mutable? v) 'mutable 'immutable) (fld-type v) (fld-name v)))
       (hash-ref rtd-fields rtd)))

(define rtd-sealed?s (make-weak-hasheq))
(define (record-type-sealed? rtd)
  (hash-ref rtd-sealed?s rtd #f))

(define rtd-opaque?s (make-weak-hasheq))
(define (record-type-opaque? rtd)
  (hash-ref rtd-opaque?s rtd #f))

(define (record-writer . args)
  (void))

(define (fix-offsets flds)
  (let loop ([flds flds] [offset ptr-bytes])
    (unless (null? flds)
      (cond
        [(eq? (fld-type (car flds)) 'double)
         (let ([offset (if (zero? (modulo offset max-float-alignment))
                           offset
                           (+ offset (- 8 (modulo offset max-float-alignment))))])
           (set-fld-byte! (car flds)  (+ record-ptr-offset offset))
           (loop (cdr flds) (+ offset 8)))]
        [else
         (set-fld-byte! (car flds)  (+ record-ptr-offset offset))
         (loop (cdr flds) (+ offset ptr-bytes))])))
  flds)

;; assumes that `v` has only pointer-sized fields
(define ($object-ref type v offset)
  (cond
    [(flonum? v)
     (case type
       [(unsigned-64)
        (integer-bytes->integer (real->floating-point-bytes v 8) #f)]
       [(integer-64)
        (integer-bytes->integer (real->floating-point-bytes v 8) #t)]
       [(integer-32)
        (define bstr (real->floating-point-bytes v 8))
        (case offset
          [(6) (integer-bytes->integer bstr #t (system-big-endian?) 0 4)]
          [(10) (integer-bytes->integer bstr #t (system-big-endian?) 4 8)]
          [else
           (error "unrecognized floating-point access" type offset)])]
       [else (error "unrecognized floating-point access" type offset)])]
    [else
     (unless (or (eq? type 'scheme-object)
                 (eq? type 'ptr))
       (error '$object-ref "unrecognized type: ~e" type))
     (define i (quotient (- offset (+ record-ptr-offset ptr-bytes)) ptr-bytes))
     (cond
       [(struct-type? v)
        (cond
          [(i . < . (length base-rtd-fields))
           ((csv7:record-field-accessor/mutator base-rtd i #f) v)]
          [else
           (error '$object-ref "not yet supported for base-rtd subtypes")])]
       [(base-rtd? v)
        ((csv7:record-field-accessor/mutator base-rtd i #f) v)]
       [else (unsafe-struct-ref v i)])]))
