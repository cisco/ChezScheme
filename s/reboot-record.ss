;; Needs to implements the same record API as the current Chez Scheme
;; implementation. It doesn't have to be too fast, so it can just use
;; vectors and extra wrappers. Use `define-record-type/orig` here to
;; get the host Scheme's `define-record-type`, and otherwise use `#%`
;; to access host Scheme primitives.

(define-record-type/orig re:rtd
  (fields base-rtd name parent uid fields count sealed? opaque? extras)
  (nongenerative #{re:rtd bxw8uzjdge5u6o5xp0kovyiun-0}))

(define-record-type/orig re:record
  (fields rtd vec)
  (nongenerative #{re:record bxw8uzjdge5u6o5xp0kovyiun-1}))

;; We could dig the `rcd` definition out of "syntax.ss", but currently
;; it also works ok to make out own and provide a small bridge to the
;; one defined in "syntax.ss" as it is discovered later
(define-record-type/orig re:rcd
  (fields rtd parent-rcd protocol)
  (nongenerative #{re:rcd bxw8uzjdge5u6o5xp0kovyiun-3}))
(define rtd-of-rcd #f)

;; bridge enum-set
(define-record-type/orig re:rtd-of-enum
  (fields host-rtd))

(define (make-field name class type)
  (list name class type))
(define field-name car)
(define field-class cadr)
(define field-type caddr)

(define new-base-rtd
  (call-with-input-file
   "s/cmacros.ss"
   (lambda (i)
     (let loop ()
       (define e (read i))
       (cond
         [(eof-object? e)
          (error 'new-base-rtd "`record-type` not found")]
         [(and (pair? e)
               (eq? (car e) 'define-primitive-structure-disps)
               (eq? (cadr e) 'record-type))
          (let ([fields (cdr (list-ref e 3))])
            (make-re:rtd #!base-rtd
                         'base-rtd
                         #f
                         (#%record-type-uid #!base-rtd)
                         (map (lambda (f)
                                (make-field (cadr f) 'immutable 'scheme-object))
                              fields)
                         (length fields)
                         #f
                         #f
                         '()))]
         [else (loop)])))))

(define base-rtd-accessor
  (begin
    (unless (equal? (map field-name (re:rtd-fields new-base-rtd))
                    '(ancestry
                      size
                      pm
                      mpm
                      name
                      flds
                      flags
                      uid
                      counts))
      (error 'base-rtd "reboot base-rtd shape does not match \"cmacros.ss\""))
    (lambda (idx)
      (let-syntax ([handle (syntax-rules ()
                             [(_ x e) (lambda (x)
                                        (if (or (eq? x #!base-rtd)
                                                (re:rtd? x))
                                            e
                                            (vector-ref (re:record-vec x) idx)))])])
        (define (ptr-type? t)
          (case t
            [(scheme-object ptr) #t]
            [(uptr double) #f]
            [else (error 'ptr-type? "unrecognized type")]))
        (define (all-fields rtd)
          (let loop ([rtd rtd] [accum '()])
            (if rtd
                (let ([rtd (subst-base-rtd rtd)])
                  (loop (re:rtd-parent rtd)
                        (append (re:rtd-fields rtd) accum)))
                accum)))
        (case (field-name (list-ref (re:rtd-fields new-base-rtd) idx))
          [(ancestry)
           (handle x (list->vector
                      (let loop ([x x] [accum '()])
                        (cond
                          [(not x) (cons #f accum)]
                          [(eq? x #!base-rtd) (list* #f x accum)]
                          [else (loop (re:rtd-parent x) (cons x accum))]))))]
          [(size)
           (handle x (+ (lookup-constant 'header-size-record)
                        (* (re:rtd-count (subst-base-rtd x)) (lookup-constant 'ptr-bytes))))]
          [(pm)
           (handle x (let ([fields (all-fields (subst-base-rtd x))])
                       (cond
                         [(andmap (lambda (f)
                                    (ptr-type? (field-type f)))
                                  fields)
                          -1]
                         [else
                          (let loop ([pm 1]
                                     [m 2] ; start after base-rtd
                                     [fields fields])
                            (cond
                              [(null? fields) pm]
                              [else
                               (loop (if (ptr-type? (field-type (car fields))) (bitwise-ior pm m) pm)
                                     (bitwise-arithmetic-shift-left m 1)
                                     (cdr fields))]))])))]
          [(mpm)
           (handle x (let ([fields (all-fields (subst-base-rtd x))])
                       (let loop ([pm 0]
                                  [m 2] ; start after base-rtd
                                  [fields fields])
                         (cond
                           [(null? fields) pm]
                           [else
                            (loop (if (and (eq? 'mutable (field-class (car fields)))
                                           (ptr-type? (field-type (car fields))))
                                      (bitwise-ior pm m)
                                      pm)
                                  (bitwise-arithmetic-shift-left m 1)
                                  (cdr fields))]))))]
          [(name)
           (handle x (re:rtd-name (subst-base-rtd x)))]
          [(flds)
           ;; result needs to be consistent with `(macro-define-structure (fld ...))`
           ;; in "cmacros.ss"
           (handle
            x
            (let loop ([fs (all-fields (subst-base-rtd x))] [offset (lookup-constant 'ptr-bytes)])
              (cond
                [(null? fs) '()]
                [else
                 (let ([f (car fs)])
                   (define (make-fld byte)
                     (vector 'fld (field-name f) (eq? (field-class f) 'mutable) (field-type f) byte))
                   (cond
                     [(eq? (field-type f) 'double)
                      (let ([offset (if (zero? (modulo offset (lookup-constant 'max-float-alignment)))
                                        offset
                                        (+ offset (- 8 (modulo offset (lookup-constant 'max-float-alignment)))))])
                        (cons
                         (make-fld (+ (lookup-constant 'record-ptr-offset) offset))
                         (loop (cdr fs) (+ offset 8))))]
                     [else
                      (cons
                       (make-fld (+ (lookup-constant 'record-ptr-offset) offset))
                       (loop (cdr fs) (+ offset (lookup-constant 'ptr-bytes))))]))])))]
          [(flags)
           (handle x (bitwise-ior
                      (if (re:rtd-opaque? (subst-base-rtd x))
                          (lookup-constant 'rtd-opaque)
                          0)
                      (if (re:rtd-sealed? (subst-base-rtd x))
                          (lookup-constant 'rtd-sealed)
                          0)))]
          [(uid)
           (handle x (re:rtd-uid (subst-base-rtd x)))]
          [(counts)
           (handle x #f)]
          [else
           (error 'base-rtd-accessor "unknown index ~s" idx)])))))

(define-primitive $record? #%$record?)

(define-primitive record?
  (case-lambda
   [(v) (and (re:record? v)
             (not (re:rtd-opaque? (re:record-rtd v))))]
   [(v rtd)
    (define (search v-rtd)
      (or (eq? v-rtd rtd)
          (and (not (eq? v-rtd #!base-rtd))
               (let ([p (re:rtd-parent v-rtd)])
                 (and p
                      (search p))))))
    (or (and (re:record? v)
             (search (re:record-rtd v)))
        (and (re:rtd? v)
             (search (re:rtd-base-rtd v))))]))

(define-primitive ($sealed-record? v rtd)
  (and (re:record? v)
       (eq? rtd (re:record-rtd v))))

(define warned-host-record-type (make-eq-hashtable))

(define-primitive ($record-type-descriptor v)
  (cond
    [(eq? v #!base-rtd)
     #!base-rtd]
    [(re:rtd? v)
     (re:rtd-base-rtd v)]
    [(re:record? v)
     (re:record-rtd v)]
    [(re:rcd? v)
     (unless rtd-of-rcd
       (error '$record-type-descriptor "haven't seen the rtd of rcd, so far"))
     rtd-of-rcd]
    [(#%$record? v)
     (let* ([host-rtd (#%$record-type-descriptor v)]
            [name (#%record-type-uid host-rtd)]
            [re-rtd (hashtable-ref all-rtds name #f)])
       (cond
         [re-rtd
          (unless (hashtable-ref warned-host-record-type name #f)
            (hashtable-set! warned-host-record-type name #t)
            (printf "  [warning: allowing host record for ~s]\n"
                    name))
          re-rtd]
         [else
          (error '$record-type-descriptor "not a reboot record: ~s" v)]))]))

(define (check-allowed-host-record v)
  ;; success by `$record-type-descriptor` means we're willing to convert:
  ($record-type-descriptor v))

(define-primitive record-rtd
  (lambda (v)
    (cond
      [(enum-set? v)
       ;; for now, we can keep using the host Scheme's enumerations, but
       ;; we have to be able to treat them as records
       (make-re:rtd-of-enum (#%record-rtd v))]
      [else (re:record-rtd v)])))

(define-primitive record-type-uid
  (lambda (rtd)
    (re:rtd-uid (subst-base-rtd rtd))))
(define-primitive record-type-name
  (lambda (rtd)
    (re:rtd-name (subst-base-rtd rtd))))
(define-primitive record-type-sealed?
  (lambda (rtd)
    (re:rtd-sealed? (subst-base-rtd rtd))))
(define-primitive record-type-opaque?
  (lambda (rtd)
    (re:rtd-opaque? (subst-base-rtd rtd))))
(define-primitive record-type-parent
  (lambda (rtd)
    (re:rtd-parent (subst-base-rtd rtd))))

(define (parent-rtd-count parent)
  (cond
    [(not parent) 0]
    [(eq? parent #!base-rtd) (re:rtd-count new-base-rtd)]
    [else (re:rtd-count parent)]))

(define (subst-base-rtd rtd)
  (if (eq? rtd #!base-rtd)
      new-base-rtd
      rtd))

(define all-rtds (make-eq-hashtable))

(define-primitive ($make-record-type-descriptor base-rtd name parent uid sealed? opaque? fields . extras)
  (unless (or (eq? #!base-rtd base-rtd)
              (re:rtd? base-rtd))
    (error '$make-record-type-descriptor "bad base-rtd ~s" base-rtd))
  (unless (or (not parent) (re:rtd? parent) (eq? parent #!base-rtd))
    (error '$make-record-type-descriptor "bad parent ~s" parent))
  (let ([uid (or uid (gensym))]
        [fields (vector->list fields)])
    (or (hashtable-ref all-rtds uid #f)
        (let ([rtd (make-re:rtd base-rtd name parent uid
                                (map (lambda (f)
                                       (cond
                                         [(symbol? f) (make-field f 'immutable 'scheme-object)]
                                         [(null? (cddr f)) (make-field (cadr f) (car f) 'scheme-object)]
                                         [else (make-field (caddr f) (car f) (cadr f))]))
                                     fields)
                                (+ (length fields) (parent-rtd-count parent))
                                sealed?
                                (or opaque? (and parent
                                                 (re:rtd-opaque? (subst-base-rtd parent))))
                                extras)])
          (hashtable-set! all-rtds uid rtd)
          rtd))))

(define-primitive ($make-record-type base-rtd parent name fields sealed? opaque? . extras)
  (apply $make-record-type-descriptor base-rtd name parent (if (#%gensym? name) name (gensym)) sealed? opaque?
         (list->vector (map (lambda (f)
                              (if (symbol? f)
                                  (list 'mutable f)
                                  f))
                            fields))
         extras))

(define-primitive (make-record-type-descriptor name parent uid sealed? opaque? fields)
  ($make-record-type-descriptor #!base-rtd name parent uid sealed? opaque? fields))

(define-primitive (r6rs:make-record-type-descriptor name parent uid sealed? opaque? fields)
  ($make-record-type-descriptor #!base-rtd name parent uid sealed? opaque? fields))

(define-primitive record-type-descriptor?
  (lambda (v)
    (or (re:rtd? v) (eq? #!base-rtd v))))

(define-primitive make-record-type
  (case-lambda
   [(name fields) (make-record-type #f name fields)]
   [(parent name fields)
    ($make-record-type #!base-rtd parent (if (string? name) (string->symbol name) name)
                       (map (lambda (f)
                              (if (symbol? f)
                                  (make-field 'mutable 'scheme-object f)
                                  f))
                            fields)
                       #f #f)]))

(define-primitive ($remake-rtd rtd compute-field-offsets)
  ;; We don't have to do anything here, because `base-rtd-accessor`
  ;; computes offsets on demand using a fixed calculation that is
  ;; good enough for bootstrapping (for now, at least)
  rtd)

(define-primitive ($record rtd . args)
  (cond
    [(eq? rtd #!base-rtd)
     (error 'base-rtd "fixme")]
    [(re:rtd-of-enum? rtd) (apply #%$record (re:rtd-of-enum-host-rtd rtd) args)]
    [else
     (make-re:record rtd (list->vector args))]))

(define-primitive (make-record-constructor-descriptor rtd parent-rcd protocol)
  (make-re:rcd rtd parent-rcd protocol))

(define-primitive ($make-record-constructor-descriptor rtd parent-rcd protocol who)
  (make-record-constructor-descriptor rtd parent-rcd protocol))

(define-primitive record-constructor-descriptor? re:rcd?)

(define-primitive (record-constructor rcd)
  (cond
    [(re:rtd? rcd)
     (lambda fields
       (make-re:record rcd (list->vector fields)))]
    [else
     (let loop ([rcd rcd]
                [rc (lambda fields
                      (make-re:record (re:rcd-rtd rcd) (list->vector fields)))])
       (let ([protocol (re:rcd-protocol rcd)])
         (if protocol
             (protocol
              (cond
                [(re:rcd-parent-rcd rcd)
                 => (lambda (p-rcd)
                      (loop p-rcd
                            (lambda parent-fields
                              (lambda child-fields
                                (apply rc (append parent-fields child-fields))))))]
                [else rc]))
             rc)))]))

(define-primitive r6rs:record-constructor record-constructor)

(define-primitive (record-predicate rtd)
  (lambda (v) (record? v rtd)))

(define-primitive (record-accessor rtd idx)
  (cond
    [(eq? rtd #!base-rtd)
     (base-rtd-accessor idx)]
    [else
     (let ([idx (+ idx (parent-rtd-count (re:rtd-parent rtd)))])
       (lambda (v)
         (cond
           [(re:rtd? v)
            (list-ref (re:rtd-extras v) (- idx (re:rtd-count new-base-rtd)))]
           [(re:rcd? v)
            (unless (eq? (re:rtd-name rtd) 'rcd) ; sanity check
              (error 'record-accessor "doesn't look like the rcd type"))
            (set! rtd-of-rcd rtd) ; stash for later use
            (case (field-name (list-ref (re:rtd-fields rtd) idx))
              [(rtd) (re:rcd-rtd v)]
              [(prcd) (re:rcd-parent-rcd v)]
              [(protocol) (re:rcd-protocol v)]
              [else
               (error 'record-accessor "not handled for rcd: ~s" idx)])]
           [else
            (unless (re:record? v) (error 'record-accesser "not a record ~s" v))
            (vector-ref (re:record-vec v) idx)])))]))

(define-primitive (record-mutator rtd idx)
  (let ([idx (+ idx (parent-rtd-count (re:rtd-parent rtd)))])
    (lambda (v val)
      (unless (re:record? v) (error 'record-mutator "not a record ~s" v))
      (vector-set! (re:record-vec v) idx val))))

(define-primitive (field-name->index rtd name)
  (+ (let loop ([fs (re:rtd-fields (subst-base-rtd rtd))] [idx 0])
       (cond
         [(null? fs) (error 'csv7-record "field not found ~s" name)]
         [(eq? (caar fs) name) idx]
         [else (loop (cdr fs) (add1 idx))]))
     (parent-rtd-count (re:rtd-parent (subst-base-rtd rtd)))))

(define-primitive (csv7:record-field-accessor rtd name/idx)
  (let ([idx (if (#%symbol? name/idx)
                 (field-name->index (subst-base-rtd rtd) name/idx)
                 name/idx)])
    (cond
      [(eq? rtd #!base-rtd)
       (base-rtd-accessor idx)]
      [else
       (lambda (v)
         (cond
           [(re:rtd? v)
            (error 'csv7:record-field-accessor "need more rtd support ~s ~s" v idx)]
           [(re:record? v)
            (vector-ref (re:record-vec v) idx)]
           [else
            (check-allowed-host-record v)
            ($object-ref 'ptr v (+ (lookup-constant 'record-data-disp)
                                   (* (lookup-constant 'ptr-bytes) idx)))]))])))


(define-primitive (csv7:record-field-mutator rtd name/idx)
  (let ([idx (if (symbol? name/idx)
                 (field-name->index rtd name/idx)
                 name/idx)])
    (lambda (v val)
      (unless (re:record? v) (error 'csv7:record-field-mutator "not a record ~s" v))
      (vector-set! (re:record-vec v) idx val))))

(define-primitive (csv7:record-field-mutable? rtd name/idx)
  (let ([idx (if (symbol? name/idx)
                 (field-name->index rtd name/idx)
                 name/idx)])
    (let loop ([rtd rtd])
      (let ([c (parent-rtd-count (re:rtd-parent (subst-base-rtd rtd)))])
        (if (< idx c)
            (loop (re:rtd-parent (subst-base-rtd rtd)))
            (eq? (field-class (list-ref (re:rtd-fields (subst-base-rtd rtd)) (- idx c))) 'mutable))))))

(define-primitive (csv7:record-field-accessible? rtd name/idx)
  #t)

(define-primitive (record-type-field-names rtd)
  (list->vector (map field-name (re:rtd-fields (subst-base-rtd rtd)))))

(define-primitive (csv7:record-type-field-names rtd)
  (let loop ([rtd rtd] [accum '()])
    (let ([accum (append (map field-name (re:rtd-fields (subst-base-rtd rtd)))
                         accum)]
          [p (re:rtd-parent (subst-base-rtd rtd))])
      (if (not p)
          accum
          (loop p accum)))))

(define-primitive (csv7:record-type-field-decls rtd)
  (let loop ([rtd rtd] [accum '()])
    (let ([accum (append (map (lambda (f)
                                (list (field-class f) (field-type f) (field-name f)))
                              (re:rtd-fields (subst-base-rtd rtd)))
                         accum)]
          [p (re:rtd-parent (subst-base-rtd rtd))])
      (if (not p)
          accum
          (loop p accum)))))

(define-primitive record-writer
  (case-lambda
   [(rtd) #f]
   [(rtd proc) (void)]))

;; assumes that records has only pointer-sized fields
(define-primitive ($object-ref type v offset)
  (cond
    [(flonum? v)
     (let ([bstr (make-bytevector 8)])
       (bytevector-ieee-double-native-set! bstr 0 v)
       (case type
         [(unsigned-64)
          (bytevector-u64-native-ref bstr 0)]
         [(integer-64)
          (bytevector-s64-native-ref bstr 0)]
         [(integer-32)
          (bytevector-s32-native-ref bstr (- offset (lookup-constant 'flonum-data-disp)))]
         [else (error "unrecognized floating-point access" type offset)]))]
    [else
     (unless (or (eq? type 'scheme-object)
                 (eq? type 'ptr)
                 (and (eq? type 'integer-64)
                      (= 8 (ftype-sizeof uptr)))
                 (and (eq? type 'integer-32)
                      (= 4 (ftype-sizeof uptr))))
       (error '$object-ref "unrecognized type: ~s in ~s ~s" type v offset))
     (let ([i (quotient (- offset (+ (lookup-constant 'record-ptr-offset)
                                     (lookup-constant 'ptr-bytes)))
                        (lookup-constant 'ptr-bytes))])
       (cond
         [(record-type-descriptor? v)
          (cond
            [(< i (re:rtd-count new-base-rtd))
             ((base-rtd-accessor i) v)]
            [else
             (error '$object-ref "not yet supported for base-rtd subtypes")])]
         [(re:record? v) (vector-ref (re:record-vec v) i)]
         [else
          (check-allowed-host-record v)
          (meta-cond
           [(#%$top-level-bound? '$record-ref)
            (#%$record-ref v i)]
           [else
            (let ([offset (+ (* (+ i 1) (ftype-sizeof uptr))
                             ;; this turns out to be constant across word
                             ;; sizes, at least for versions before `$record-ref`
                             (lookup-constant 'record-ptr-offset))])
              (#%$object-ref type v offset))])]))]))

(meta define record-type-info list)
(meta define record-type-info-rtd car)
(meta define record-type-info-rcd cadr)

(define-syntax type-descriptor
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ name) (record-type-info-rtd (r #'name))]))))

(define-syntax record-type-descriptor
  (lambda (x)
    (syntax-case x ()
      [(_ name) #'(type-descriptor name)])))

(define-syntax record-constructor-descriptor
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ name)
         (record-type-info-rcd (r #'name))]))))

(define-syntax define-record
  (let ()
    (lambda (x)
      (error 'define-record "please don't use `define-record` to implement the expander"))))

(define-syntax define-record-type
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ name/s spec ...)
         (let ([build (lambda (name . pieces)
                        (datum->syntax name
                                       (string->symbol
                                        (apply string-append
                                               (map (lambda (piece)
                                                      (cond
                                                        [(identifier? piece) (symbol->string (#%syntax->datum piece))]
                                                        [(string? piece) piece]
                                                        [(symbol? piece) (symbol->string piece)]
                                                        [else (error 'build-name "oops")]))
                                                    pieces)))))])
           (let-values ([(name maker pred)
                         (syntax-case #'name/s ()
                           [(name maker pred) (values #'name #'maker #'pred)]
                           [name (values #'name (build #'name "make-" #'name) (build #'name #'name "?"))])])
             (define (find key default)
               (or (ormap (lambda (spec)
                            (syntax-case spec ()
                              [(spec-key . _)
                               (eq? key (#%syntax->datum #'spec-key))
                               spec]
                              [_ #f]))
                          #'(spec ...))
                   default))
             (let ([parent (syntax-case (find 'parent #'(_ #f)) ()
                             [(_ #f) #f]
                             [(_ id) #'id])]
                   [fields (syntax-case (find 'fields #'(fields)) ()
                             [(fields field ...) #'(field ...)])]
                   [uid (syntax-case (find 'nongenerative #'(_ #f)) ()
                          [(_) (datum->syntax name (gensym))]
                          [(_ #f) #f]
                          [(_ id) #'id])]
                   [sealed? (syntax-case (find 'sealed #'(_ #f)) ()
                              [(_ s?) #'s?])]
                   [opaque? (syntax-case (find 'opaque #'(_ #f)) ()
                              [(_ o?) #'o?])]
                   [protocol (syntax-case (find 'protocol #'(_ #f)) ()
                               [(_ p) #'p])]
                   [rtd (build name "RTD:" name)]
                   [rcd (build name "RCD:" name)])
               (let ([field-names (map (lambda (field)
                                         (syntax-case field ()
                                           [(class field-name . _) #'(class field-name)]
                                           [field-name
                                            (identifier? #'field-name)
                                            #'(immutable field-name)]))
                                       fields)])
                 (with-syntax ()
                   (define (show v) #;(pretty-print (syntax->datum v)) v)
                   (show
                    #`(begin
                        (define-syntax #,name
                          (make-compile-time-value (record-type-info (syntax #,rtd) (syntax #,rcd))))
                        (indirect-export #,name #,rtd #,rcd)
                        (define #,rtd
                          (make-record-type-descriptor '#,name
                                                       #,(if parent
                                                             (record-type-info-rtd (r parent))
                                                             #f)
                                                       '#,uid
                                                       '#,sealed?
                                                       '#,opaque?
                                                       '#,(list->vector field-names)))
                        (define #,rcd
                          (make-record-constructor-descriptor #,rtd
                                                              #,(if parent
                                                                    (record-type-info-rcd (r parent))
                                                                    #f)
                                                              #,protocol))
                        (define #,pred (lambda (v) (record? v #,rtd)))
                        (define #,maker (record-constructor #,rcd))
                        #,@(map (lambda (field idx)
                                  #`(define #,(syntax-case field ()
                                                [(_ _ acc . _) #'acc]
                                                [(_ field) (build name name "-" #'field)]
                                                [field
                                                 (identifier? #'field)
                                                 (build name name "-" #'field)])
                                      (record-accessor #,rtd #,idx)))
                                fields
                                (iota (length field-names)))
                        #,@(filter (lambda (v) v)
                                   (map (lambda (field idx)
                                          (syntax-case field (mutable)
                                            [(mutable . _)
                                             #`(define #,(syntax-case field ()
                                                           [(_ _ _ mut) #'mut]
                                                           [(_ field) (build #'field name "-" #'field "-set!")])
                                                 (record-mutator #,rtd #,idx))]
                                            [_ #f]))
                                        fields
                                        (iota (length field-names)))))))))))]))))

