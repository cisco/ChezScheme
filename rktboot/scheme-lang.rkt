#lang racket/base
(require (for-syntax racket/base
                     racket/match)
         (prefix-in r: racket/include)
         racket/fixnum
         racket/flonum
         racket/vector
         racket/splicing
         racket/pretty
         racket/dict
         "config.rkt"
         (for-syntax "config.rkt")
         (for-syntax "constant.rkt")
         "immediate.rkt"
         "define-datatype.rkt"
         "primdata.rkt"
         "gensym.rkt"
         "format.rkt"
         "hand-coded.rkt"
         "scheme-struct.rkt"
         "symbol.rkt"
         "record.rkt"
         (for-syntax "record.rkt")
         "constant.rkt"
         (only-in "r6rs-lang.rkt"
                  make-record-constructor-descriptor
                  set-car!
                  set-cdr!
                  fixnum-width
                  most-positive-fixnum
                  most-negative-fixnum)
         (submod "r6rs-lang.rkt" hash-pair)
         (for-syntax "scheme-struct.rkt"
                     "rcd.rkt"))

(provide (rename-out [s:define define]
                     [s:define define-threaded]
                     [s:define define-who]
                     [gen-let-values let-values]
                     [s:module module]
                     [s:parameterize parameterize])
         set-who!
         import
         include
         when-feature
         fluid-let
         letrec*
         putprop getprop remprop
         $sputprop $sgetprop $sremprop
         define-flags
         $primitive
         $tc $tc-field $thread-tc
         enumerate
         $make-record-type
         $make-record-type-descriptor
         $make-record-type-descriptor*
         $make-record-constructor-descriptor
         $record
         $record?
         $primitive
         $unbound-object?
         $app
         (rename-out [get-$unbound-object $unbound-object])
         meta-cond
         constant
         $target-machine
         $sfd
         $current-mso
         $block-counter
         define-datatype
         datum
         rec
         with-tc-mutex
         with-values
         make-record-type
         type-descriptor
         csv7:record-field-accessor
         csv7:record-field-mutator
         csv7:record-field-mutable?
         record-writer
         record-rtd
         record-type-sealed?
         record-type-opaque?
         record-type-parent
         record-type-field-names
         record-type-field-indices
         csv7:record-type-field-names
         csv7:record-type-field-indices
         csv7:record-type-field-decls
         (rename-out [record-rtd $record-type-descriptor])
         record?
         record-type-uid
         $object-ref
         stencil-vector?
         (rename-out [s:vector-sort vector-sort]
                     [s:vector-sort! vector-sort!])
         vector-for-each
         vector-map
         primvec
         get-priminfo
         $top-level-value
         $set-top-level-value!
         $profile-source-data?
         $compile-profile
         compile-profile
         $optimize-closures
         $lift-closures
         $profile-block-data?
         run-cp0
         generate-interrupt-trap
         $track-dynamic-closure-counts
         $suppress-primitive-inlining
         uninterned-symbol? string->uninterned-symbol
         debug-level
         scheme-version-number
         scheme-fork-version-number
         native-endianness
         (rename-out [make-parameter $make-thread-parameter]
                     [make-parameter make-thread-parameter]
                     [cons make-binding]
                     [car binding-type]
                     [cdr binding-value]
                     [set-car! set-binding-type!]
                     [set-cdr! set-binding-value!]
                     [mpair? binding?]
                     [fx+ r6rs:fx+]
                     [fx- r6rs:fx-]
                     [add1 fx1+]
                     [sub1 fx1-]
                     [add1 1+]
                     [sub1 1-]
                     [fxand fxlogand]
                     [fxior fxlogor]
                     [fxior fxlogior]
                     [fxxor fxlogxor]
                     [fxlshift fxsll]
                     [bitwise-bit-count fxbit-count]
                     [arithmetic-shift ash]
                     [arithmetic-shift bitwise-arithmetic-shift-left]
                     [arithmetic-shift bitwise-arithmetic-shift]
                     [fxrshift fxsra]
                     [bitwise-not lognot]
                     [bitwise-ior logor]
                     [bitwise-xor logxor]
                     [bitwise-ior logior]
                     [bitwise-and logand]
                     [bitwise-bit-set? fxbit-set?]
                     [integer-length bitwise-length]
                     [->fl fixnum->flonum]
                     [+ cfl+]
                     [- cfl-]
                     [* cfl*]
                     [/ cfl/]
                     [= cfl=]
                     [/ fx/]
                     [real-part cfl-real-part]
                     [imag-part cfl-imag-part]
                     [real-part $exactnum-real-part]
                     [imag-part $exactnum-imag-part]
                     [numerator $ratio-numerator]
                     [denominator $ratio-denominator]
                     [= r6rs:=]
                     [char=? r6rs:char=?]
                     [s:error $oops]
                     [error $undefined-violation]
                     [error errorf]
                     [error warningf]
                     [make-bytes make-bytevector]
                     [bytes bytevector]
                     [bytes-length bytevector-length]
                     [bytes? bytevector?]
                     [bytes-set! bytevector-u8-set!]
                     [bytes-ref bytevector-u8-ref]
                     [bytes->immutable-bytes bytevector->immutable-bytevector]
                     [bwp? bwp-object?]
                     [number->string r6rs:number->string]
                     [s:printf printf]
                     [s:fprintf fprintf]
                     [file-position port-position]
                     [file-position set-port-position!]
                     [write-string display-string]
                     [call/ec call/1cc]
                     [s:string->symbol string->symbol])
         fx+/wraparound
         fx-/wraparound
         fx*/wraparound
         fxsll/wraparound
         logbit? logbit1 logbit0 logtest
         (rename-out [logbit? fxlogbit?]
                     [logbit1 fxlogbit1]
                     [logbit0 fxlogbit0]
                     [logtest fxlogtest])
         $fxu<
         fxsrl
         fxbit-field
         fxpopcount
         fxpopcount32
         fxpopcount16
         bitwise-bit-count
         bitwise-arithmetic-shift-right
         bytevector-u16-native-ref
         bytevector-s16-native-ref
         bytevector-u32-native-ref
         bytevector-s32-native-ref
         bytevector-u64-native-ref
         bytevector-s64-native-ref
         bytevector-s16-ref
         bytevector-u16-ref
         bytevector-s32-ref
         bytevector-u32-ref
         bytevector-s64-ref
         bytevector-u64-ref
         $integer-64?
         $integer-32?
         $flonum->digits
         $flonum-sign
         syntax-error
         $source-warning
         all-set?
         any-set?
         iota
         list-head
         subst substq substv
         (rename-out [subst subst!]
                     [substv substv!]
                     [substq substq!])
         nonnegative?
         nonpositive?
         (rename-out [nonnegative? fxnonnegative?]
                     [nonpositive? fxnonpositive?])
         last-pair
         oblist
         make-hashtable
         make-weak-eq-hashtable
         symbol-hash
         hashtable-keys
         hashtable-entries
         eq-hashtable?
         eq-hashtable-weak?
         eq-hashtable-ephemeron?
         symbol-hashtable?
         hashtable-equivalence-function
         hashtable-mutable?
         $ht-minlen
         $ht-veclen
         (rename-out [hash? hashtable?]
                     [hash-ref/pair/dict hashtable-ref]
                     [hash-ref/pair/dict eq-hashtable-ref]
                     [hash-ref-cell eq-hashtable-cell]
                     [hash-set!/pair/dict hashtable-set!]
                     [hash-remove! eq-hashtable-delete!]
                     [equal-hash-code string-hash]
                     [hash-set!/pair/dict symbol-hashtable-set!]
                     [hash-has-key? symbol-hashtable-contains?]
                     [hash-has-key? eq-hashtable-contains?]
                     [hash-ref/pair/dict symbol-hashtable-ref]
                     [hash-ref-cell symbol-hashtable-cell])
         bignum?
         ratnum?
         $inexactnum?
         $exactnum?
         $rtd-counts?
         (rename-out [symbol->string $symbol-name])
         self-evaluating?
         list-sort
         (rename-out [list-sort sort])
         path-absolute?
         subset-mode
         weak-pair?
         ephemeron-pair?
         immutable-string?
         immutable-vector?
         immutable-bytevector?
         immutable-box?
         require-nongenerative-clause
         generate-inspector-information
         generate-procedure-source-information
         enable-cross-library-optimization
         enable-arithmetic-left-associative
         enable-type-recovery
         fasl-compressed
         current-expand
         current-generate-id
         internal-defines-as-letrec*
         eval-syntax-expanders-when
         prelex-assigned set-prelex-assigned!
         prelex-referenced set-prelex-referenced!
         prelex-seen set-prelex-seen!
         prelex-multiply-referenced set-prelex-multiply-referenced!
         safe-assert
         print-gensym $intern3
         print-level
         print-depth
         print-length
         (rename-out [s:pretty-format pretty-format])
         interpret
         who
         with-source-path
         $make-source-oops
         $guard
         $reset-protect
         $map
         $open-file-input-port
         $open-file-output-port
         (rename-out [s:open-output-file open-output-file])
         $open-bytevector-list-output-port
         open-bytevector-output-port
         native-transcoder
         port-file-compressed!
         file-buffer-size
         $source-file-descriptor
         transcoded-port
         current-transcoder
         textual-port?
         binary-port?
         put-bytevector
         put-u8
         get-bytevector-n!
         (rename-out [read-byte get-u8]
                     [peek-byte lookahead-u8]
                     [s:write write])
         console-output-port
         path-root
         path-last
         $make-read
         libspec?
         $hand-coded
         on-reset
         disable-interrupts enable-interrupts
         mutex-acquire mutex-release $tc-mutex $thread-list
         $pass-time
         priminfo-unprefixed
         priminfo-libraries
         $c-bufsiz
         $foreign-procedure
         make-guardian
         $lambda/lift-barrier)

(module+ callback
  (provide set-current-expand-set-callback!))

(define-syntax-rule (import . _)
  (void))

(define-syntax include
  (lambda (stx)
    (syntax-case stx ()
      [(form "machine.def") #`(form ,(string-append target-machine ".def"))]
      [(form p) #'(r:include-at/relative-to form form p)])))

;; If we have to avoid `read-syntax`:
#;
(define-syntax include
  (lambda (stx)
    (syntax-case stx ()
      [(form "machine.def") #`(form #,(string-append target-machine ".def"))]
      [(form p)
       (let ([r (call-with-input-file*
                 (syntax->datum #'p)
                 (lambda (i)
                   (let loop ()
                     (define e (read i))
                     (if (eof-object? e)
                         null
                         (cons e (loop))))))])
         (datum->syntax #'form `(begin ,@r)))])))
 
(define-syntax when-feature
  (syntax-rules ()
    [(_ pthreads . _) (begin)]))

(define-syntax (fluid-let stx)
  (syntax-case stx ()
    [(_ ([id rhs] ...) body ...)
     (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
       #'(let ([tmp-id rhs]
               ...)
           (define (swap)
             (let ([v tmp-id]) (set! tmp-id id) (set! id v)) ...)
           (dynamic-wind
            swap
            (lambda () body ...)
            swap)))]))

;; Help the Racket compiler by lifting immediate record operations out
;; of a `letrec`. Otherwise, the Racket compiler cannot figure out that
;; they won't capture continuations, etc., and will make access slow.
;; We may even be able to substitute a literal procedure, since all record
;; types are prefab structs.
(define-syntax (letrec* stx)
  (syntax-case stx ()
    [(_ (clause ...) . body)
     (let loop ([clauses (syntax->list #'(clause ...))] [lets '()] [letrecs '()] [macros '()] [rcds #hasheq()])
       (cond
         [(null? clauses)
          #`(let #,(reverse lets)
              (letrec-syntaxes+values #,(for/list ([s (in-list macros)])
                                          (syntax-case s ()
                                            [[id rhs]
                                             #'[(id) (lambda (stx) (quote-syntax rhs))]]))
                                      #,(for/list ([s (in-list (reverse letrecs))])
                                          (syntax-case s ()
                                            [[id rhs]
                                             #'[(id) rhs]]))
                . body))]
         [else
          (define (id-eq? a b) (eq? (syntax-e a) (syntax-e b)))
          (syntax-case* (car clauses) ($primitive record-accessor record-predicate
                                                  $make-record-constructor-descriptor
                                                  make-record-constructor-descriptor
                                                  r6rs:record-constructor
                                                  quote) id-eq?
            [[id (($primitive _ record-accessor) 'rtd n)]
             (and (struct-type? (syntax-e #'rtd))
                  (integer? (syntax-e #'n)))
             (let ([a (compile-time-record-accessor (syntax-e #'rtd) (syntax-e #'n))])
               (loop (cdr clauses) (cons (if a
                                             #`[id '#,a]
                                             (car clauses))
                                         lets)
                     letrecs
                     macros
                     rcds))]
            [[id (($primitive _ record-mutator) 'rtd n)]
             (and (struct-type? (syntax-e #'rtd))
                  (integer? (syntax-e #'n)))
             (let ([m (compile-time-record-mutator (syntax-e #'rtd) (syntax-e #'n))])
               (loop (cdr clauses) (cons (if m
                                             #`[id '#,m]
                                             (car clauses))
                                         lets)
                     letrecs
                     macros
                     rcds))]
            [[id (($primitive _ record-predicate) 'rtd)]
             (struct-type? (syntax-e #'rtd))
             (let ([p (compile-time-record-predicate (syntax-e #'rtd))])
               (loop (cdr clauses) (cons (if p
                                             #`[id '#,p]
                                             (car clauses))
                                         lets)
                     letrecs
                     macros
                     rcds))]
            [[id (($primitive _ r6rs:record-constructor) 'rcd)]
             (rec-cons-desc? (syntax-e #'rcd))
             (let ([c (rcd->constructor (syntax-e #'rcd) #f)])
               (cond
                 [c (loop (cdr clauses) (cons #`[id #,c]
                                              lets)
                          letrecs
                          macros
                          rcds)]
                 [else
                  (and (log-warning "couldn't inline ~s" (car clauses)) #f)
                  (loop (cdr clauses) lets (cons (car clauses) letrecs) macros rcds)]))]
            [[id (($primitive _ mrcd)
                  'rtd
                  base
                  proc
                  . maybe-name)]
             (and (or (eq? '$make-record-constructor-descriptor (syntax-e #'mrcd))
                      (eq? 'make-record-constructor-descriptor (syntax-e #'mrcd)))
                  (struct-type? (syntax-e #'rtd))
                  (or (not (syntax-e #'base))
                      (hash-ref rcds (syntax-e #'base) #f))
                  (immediate-procedure-expression? #'proc))
             (let ([rtd (syntax-e #'rtd)]
                   [base-rcdi (and (syntax-e #'base)
                                   (hash-ref rcds (syntax-e #'base) #f))])
               (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
                 (struct-type-info rtd))
               (when (and (not base-rcdi)
                          super)
                 (error "can't handle an rcd without a base rcd and with a parent record type"))
               (define rdci (rcd-info rtd #'proc base-rcdi (+ init-cnt (if base-rcdi
                                                                           (rcd-info-init-cnt base-rcdi)
                                                                           0))))
               (loop (cdr clauses)
                     lets
                     (cons #`[id (mrcd
                                  '#,rtd
                                  base
                                  proc
                                  . maybe-name)]
                           letrecs)
                     macros
                     (hash-set rcds (syntax-e #'id) rdci)))]
            [[id (($primitive _ mrcd)
                  'rtd
                  'base-rcd
                  proc
                  . maybe-name)]
             (and (or (eq? '$make-record-constructor-descriptor (syntax-e #'mrcd))
                      (eq? 'make-record-constructor-descriptor (syntax-e #'mrcd)))
                  (struct-type? (syntax-e #'rtd))
                  (rec-cons-desc? (syntax-e #'base-rcd))
                  (immediate-procedure-expression? #'proc))
             (let ([rtd (syntax-e #'rtd)]
                   [base-rcdi (rcd->rcdi (syntax-e #'base-rcd))])
               (unless base-rcdi
                 (error "can't handle this literal rcd: ~e" (syntax-e #'base-rcd)))
               (define-values (r-name init-cnt auto-cnt ref set immutables super skipped?)
                 (struct-type-info rtd))
               (define rdci (rcd-info rtd #'proc base-rcdi (+ init-cnt (rcd-info-init-cnt base-rcdi))))
               (loop (cdr clauses)
                     lets
                     (cons #`[id (mrcd
                                  '#,rtd
                                  'base-rcd
                                  proc
                                  . maybe-name)]
                           letrecs)
                     macros
                     (hash-set rcds (syntax-e #'id) rdci)))]
            [[id (($primitive _ r6rs:record-constructor) rcd-id)]
             (and (identifier? #'rcd-id)
                  (hash-ref rcds (syntax-e #'rcd-id) #f))
             (let ([rcdi (hash-ref rcds (syntax-e #'rcd-id))])
               (define (rcdi->generator rcdi)
                 (define base-rcdi (rcd-info-base-rcdi rcdi))
                 (cond
                   [(not (rcd-info-proto-expr rcdi))
                    #`(lambda (ctr) ctr)]
                   [(not base-rcdi)
                    (rcd-info-proto-expr rcdi)]
                   [else
                    (with-syntax ([ctr (gensym 'ctr)]
                                  [(p-arg ...) (for/list ([i (in-range (rcd-info-init-cnt base-rcdi))])
                                                 (gensym))]
                                  [(c-arg ...) (for/list ([i (in-range (- (rcd-info-init-cnt rcdi)
                                                                          (rcd-info-init-cnt base-rcdi)))])
                                                 (gensym))])
                    #`(lambda (ctr)
                        (#,(rcd-info-proto-expr rcdi)
                         (#,(rcdi->generator base-rcdi)
                          (lambda (p-arg ...)
                            (lambda (c-arg ...)
                              (ctr p-arg ... c-arg ...)))))))]))
               (define c (struct-type-make-constructor (rcd-info-rtd rcdi)))
               (loop (cdr clauses)
                     lets
                     (cons #`[id (#,(rcdi->generator rcdi) #,c)]
                           letrecs)
                     macros
                     rcds))]
            [[id (($primitive _ r6rs:record-constructor) _)]
             (and (log-warning "couldn't simplify ~s" (car clauses))
                  #f)
             (void)]
            
            [[id (($primitive _ mrcd) . _)]
             (and (or (eq? '$make-record-constructor-descriptor (syntax-e #'mrcd))
                      (eq? 'make-record-constructor-descriptor (syntax-e #'mrcd)))
                  (log-warning "couldn't recognize ~s" (car clauses))
                  #f)
             (void)]
            [else
             (loop (cdr clauses) lets (cons (car clauses) letrecs) macros rcds)])]))]))

(define-for-syntax (immediate-procedure-expression? s)
  (syntax-case s ()
    [(id . _)
     (and (identifier? #'id)
          (or (eq? (syntax-e #'id) 'lambda)
              (eq? (syntax-e #'id) 'case-lambda)))]
    [_ #f]))

(define-syntax (with-inline-cache stx)
  (syntax-case stx ()
    [(_ expr)
     #`(let ([b #,(mcons #f #f)])
         (or (mcar b)
             (let ([r expr])
               (set-mcar! b r)
               r)))]))

(define-syntax (s:parameterize stx)
  (syntax-case stx ()
    [(_ ([id rhs] ...) body ...)
     (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
       #'(let ([tmp-id rhs]
               ...)
           (define (swap)
             (let ([v tmp-id]) (set! tmp-id (id)) (id v)) ...)
           (dynamic-wind
            swap
            (lambda () body ...)
            swap)))]))

(define-syntax s:define
  (syntax-rules ()
    [(_ id) (define id (void))]
    [(_ . rest) (define . rest)]))

(define-syntax (gen-let-values stx)
  (syntax-case stx ()
    [(_ ([lhs rhs] ...) body ...)
     (with-syntax ([([lhs rhs] ...)
                    (for/list ([lhs (in-list (syntax->list #'(lhs ...)))]
                               [rhs (in-list (syntax->list #'(rhs ...)))])
                      (syntax-case lhs ()
                        [(id ...) (list lhs rhs)]
                        [_ (with-syntax ([flat-lhs (let loop ([lhs lhs])
                                                     (syntax-case lhs ()
                                                       [(id . rest)
                                                        (cons #'id (loop #'rest))]
                                                       [_ (list lhs)]))])
                             #'[flat-lhs (call-with-values (lambda () rhs)
                                                           (lambda lhs (values . flat-lhs)))])]))])
       #'(let-values ([lhs rhs] ...) body ...))]))

(define-values (primvec get-priminfo)
  (get-primdata $sputprop scheme-dir))

(begin-for-syntax
  (define (make-flags->bits specs)
    (define bits
      (for/fold ([bits #hasheq()]) ([spec (in-list specs)])
        (define (get-val v)
          (if (number? v) v (hash-ref bits v)))
        (match spec
          [`(,name (or ,vals ...))
           (hash-set bits name (apply bitwise-ior (map get-val vals)))]
          [`(,name ,val)
           (hash-set bits name (get-val val))])))
    (lambda (flags)
      (apply bitwise-ior (for/list ([flag (in-list flags)])
                           (hash-ref bits flag))))))

(define-syntax (define-flags stx)
  (syntax-case stx ()
    [(_ name spec ...)
     #'(define-syntax name
         (let ([flags->bits (make-flags->bits '(spec ...))])
           (lambda (stx)
             (syntax-case stx (or)
               [(_ . flags)
                (flags->bits 'flags)]))))]))

(define-syntax $primitive
  (syntax-rules ()
    [(_ name) name]
    [(_ opt name) name]))

(define ($app proc . args)
  (apply proc args))

(define tc (make-hasheq))
(define ($tc) tc)
(define ($thread-tc tc) tc)

(define $tc-field
  (case-lambda
    [(sym tc) (hash-ref tc sym (case sym
                                 [(parameters) (vector)]
                                 [else 0]))]
    [(sym tc v) (hash-set! tc sym v)]))

(define ($thread-list) (list tc))

(define (enumerate ls)
  (for/list ([v (in-list ls)]
             [i (in-naturals)])
    i))

(define ($make-record-constructor-descriptor rtd prcd protocol who)
  (make-record-constructor-descriptor rtd prcd protocol))

(define ($make-record-type-descriptor* base-rtd name parent uid sealed? opaque? num-fields mutability-mask who . extras)
  (define fields (for ([i (in-range num-fields)])
                   (list (if (bitwise-bit-set? mutability-mask i) 'mutable 'immutable)
                         (string->symbol (format "f~a" i)))))
  (apply $make-record-type-descriptor base-rtd name parent uid sealed? opaque? fields who extras))

(define-syntax-rule (s:module (id ...) body ...)
  (begin
    body ...))

(define-syntax-rule (meta-cond [q r ...] ...)
  (splicing-let-syntax ([go
                         (lambda (stx)
                           (cond
                             [q #'(begin r ...)]
                             ...))])
    (go)))

(define-syntax set-who!
  (syntax-rules ()
    [(_ #(space id) rhs) (void)]
    [(_ id rhs) (set! id rhs)]))

(define-syntax (constant stx)
  (syntax-case stx ()
    [(_ id)
     #`#,(case (syntax-e #'id)
           [(fixnum-bits) fixnum-bits]
           [(most-negative-fixnum) (- (expt 2 (sub1 fixnum-bits)))]
           [(most-positive-fixnum) (sub1 (expt 2 (sub1 fixnum-bits)))]
           [(annotation-debug) annotation-debug]
           [(annotation-profile) annotation-profile]
           [(visit-tag) visit-tag]
           [(revisit-tag) revisit-tag]
           [(prelex-is-flags-offset) prelex-is-flags-offset]
           [(prelex-was-flags-offset) prelex-was-flags-offset]
           [(prelex-sticky-mask) prelex-sticky-mask]
           [(prelex-is-mask) prelex-is-mask]
           [(code-flag-lift-barrier) code-flag-lift-barrier]
           [else (error 'constant "unknown: ~s" #'id)])]))

(define $target-machine (make-parameter (string->symbol target-machine)))
(define $sfd (make-parameter #f))
(define $current-mso (make-parameter #f))
(define $block-counter (make-parameter 0))

(define (any-set? mask x)
  (not (fx= (fxand mask x) 0)))

(define (all-set? mask x)
  (let ((m mask)) (fx= (fxand m x) m)))

(define (iota n)
  (for/list ([i (in-range n)])
    i))

(define (list-head l n)
  (if (zero? n)
      null
      (cons (car l)
            (list-head (cdr l) (sub1 n)))))

(define ((make-subst eql?) new old v)
  (let loop ([v v])
    (cond
      [(eql? v old) new]
      [(pair? v) (cons (loop (car v))
                       (loop (cdr v)))]
      [else v])))

(define subst (make-subst equal?))
(define substv (make-subst eqv?))
(define substq (make-subst eq?))

(define-syntax-rule (datum e)
  (syntax->datum (syntax e)))

(define-syntax-rule (rec id rhs)
  (letrec ([id rhs])
    id))

(define (nonnegative? v)
  (and (real? v)
       (v . >= . 0)))

(define (nonpositive? v)
  (and (real? v)
       (v . <= . 0)))

(define (last-pair p)
  (if (and (pair? p)
           (pair? (cdr p)))
      (last-pair (cdr p))
      p))

(define-syntax-rule (with-tc-mutex body ...)
  (let () body ...))

(define-syntax-rule (with-values prod con)
  (call-with-values (lambda () prod) con))

(define (s:vector-sort proc vec)
  (vector-sort vec proc))

(define (s:vector-sort! proc vec)
  (vector-sort! vec proc))

(define vector-for-each
  (case-lambda
    [(proc vec)
     (for ([e (in-vector vec)])
       (proc e))]
    [(proc vec1 vec2)
     (for ([e1 (in-vector vec1)]
           [e2 (in-vector vec2)])
       (proc e1 e2))]
    [(proc . vecs)
     (apply for-each proc (map vector->list vecs))]))
   
(define vector-map
  (case-lambda
    [(proc vec)
     (for/vector #:length (vector-length vec) ([e (in-vector vec)])
       (proc e))]
    [(proc . vecs)
     (list->vector (apply map proc (map vector->list vecs)))]))

(define (stencil-vector? v) #f)

(define (fxpopcount32 x)
  (let* ([x (- x (bitwise-and (arithmetic-shift x -1) #x55555555))]
         [x (+ (bitwise-and x #x33333333) (bitwise-and (arithmetic-shift x -2) #x33333333))]
         [x (bitwise-and (+ x (arithmetic-shift x -4)) #x0f0f0f0f)]
         [x (+ x (arithmetic-shift x -8) (arithmetic-shift x -16) (arithmetic-shift x -24))])
    (bitwise-and x #x3f)))

(define (fxpopcount x)
  (fx+ (fxpopcount32 (bitwise-and x #xffffffff))
       (fxpopcount32 (arithmetic-shift x -32))))

(define (fxpopcount16 x)
  (fxpopcount32 (bitwise-and x #xffff)))

(define (logbit? m n)
  (bitwise-bit-set? n m))
(define (logbit1 i n)
  (bitwise-ior (arithmetic-shift 1 i) n))
(define (logbit0 i n)
  (bitwise-and (bitwise-not (arithmetic-shift 1 i)) n))
(define (logtest a b)
  (not (eqv? 0 (bitwise-and a b))))

(define ($fxu< a b)
  (if (< a 0)
      #f
      (< a b)))

(define (fxsrl v amt)
  (if (and (v . fx< . 0)
           (amt . fx> . 0))
      (bitwise-and (fxrshift v amt)
                   (- (arithmetic-shift 1 (- (fixnum-width) amt)) 1))
      (fxrshift v amt)))

(define (fxbit-field fx1 fx2 fx3)
  (fxrshift (fxand fx1 (fxnot (fxlshift -1 fx3))) fx2))

(define (wraparound v)
  (cond
    [(fixnum? v)
     v]
    [(zero? (bitwise-and v (add1 (most-positive-fixnum))))
     (bitwise-ior v (- -1 (most-positive-fixnum)))]
    [else
     (bitwise-and v (most-positive-fixnum))]))

;; Re-implement wraparound so we can use Racket v7.9 and earlier:
(define (fx+/wraparound x y)
  (wraparound (+ x y)))
(define (fx-/wraparound x y)
  (wraparound (- x y)))
(define (fx*/wraparound x y)
  (wraparound (* x y)))
(define (fxsll/wraparound x y)
  (wraparound (arithmetic-shift x y)))

(define (bitwise-bit-count fx)
  (cond
    [(eqv? fx 0) 0]
    [(eqv? 0 (bitwise-and fx 1))
     (bitwise-bit-count (arithmetic-shift fx -1))]
    [else
     (add1 (bitwise-bit-count (arithmetic-shift fx -1)))]))

(define (bitwise-arithmetic-shift-right v s)
  (arithmetic-shift v (- s)))

(define (bytevector-u16-native-ref bv i)
  (integer-bytes->integer bv #f (system-big-endian?) i (+ i 2)))

(define (bytevector-s16-native-ref bv i)
  (integer-bytes->integer bv #t (system-big-endian?) i (+ i 2)))

(define (bytevector-u32-native-ref bv i)
  (integer-bytes->integer bv #t (system-big-endian?) i (+ i 4)))

(define (bytevector-s32-native-ref bv i)
  (integer-bytes->integer bv #t (system-big-endian?) i (+ i 4)))

(define (bytevector-u64-native-ref bv i)
  (integer-bytes->integer bv #t (system-big-endian?) i (+ i 8)))

(define (bytevector-s64-native-ref bv i)
  (integer-bytes->integer bv #t (system-big-endian?) i (+ i 8)))

(define (bytevector-s16-ref bv i endness)
  (integer-bytes->integer bv #t (eq? endness 'big) i (+ i 2)))

(define (bytevector-u16-ref bv i endness)
  (integer-bytes->integer bv #f (eq? endness 'big) i (+ i 2)))

(define (bytevector-s32-ref bv i endness)
  (integer-bytes->integer bv #t (eq? endness 'big) i (+ i 4)))

(define (bytevector-u32-ref bv i endness)
  (integer-bytes->integer bv #f (eq? endness 'big) i (+ i 4)))

(define (bytevector-s64-ref bv i endness)
  (integer-bytes->integer bv #t (eq? endness 'big) i (+ i 8)))

(define (bytevector-u64-ref bv i endness)
  (integer-bytes->integer bv #f (eq? endness 'big) i (+ i 8)))

(define ($integer-64? x)
  (<= (- (expt 2 63)) (sub1 (expt 2 64))))

(define ($integer-32? x)
  (<= (- (expt 2 31)) (sub1 (expt 2 32))))

(define ($flonum->digits . args)
  (error '$flonum->digits "not ready"))
   
(define ($flonum-sign fl)
  (if (or (eqv? fl -0.0)
          (negative? fl))
      1
      0))
   
(define ($top-level-value name)
  (case name
    [(apply) apply]
    [($capture-fasl-target)
     (namespace-variable-value name #t (lambda () $unbound-object))]
    [else
     (namespace-variable-value name)]))

(define ($set-top-level-value! name val)
  (namespace-set-variable-value! name val))

(define (get-$unbound-object)
  $unbound-object)

(define ($profile-source-data?)
  #f)

(define $compile-profile (make-parameter #f))
(define compile-profile $compile-profile)
(define $optimize-closures (make-parameter #t))
(define $lift-closures (make-parameter #t))
(define $profile-block-data? (make-parameter #f))
(define run-cp0 (make-parameter error))
(define generate-interrupt-trap (make-parameter #t))
(define $track-dynamic-closure-counts (make-parameter #f))
(define $suppress-primitive-inlining (make-parameter #f))
(define debug-level (make-parameter 0))

(define (scheme-version-number)
  (define v (lookup-constant 'scheme-version))
  (if (zero? (arithmetic-shift v -24))
      (values (arithmetic-shift v -16)
              (bitwise-and 255 (arithmetic-shift v -8))
              (bitwise-and 255 v))
      (values (arithmetic-shift v -24)
              (bitwise-and 255 (arithmetic-shift v -16))
              (bitwise-and 255 (arithmetic-shift v -8)))))

(define (scheme-fork-version-number)
  (define v (lookup-constant 'scheme-version))
  (define-values (maj min sub) (scheme-version-number))
  (if (zero? (arithmetic-shift v -24))
      (values maj min sub 0)
      (values maj min sub (bitwise-and 255 v))))

(define (native-endianness)
  (if (system-big-endian?)
      'big
      'little))

(define (make-hashtable hash eql?)
  (cond
    [(eq? hash symbol-hash)
     (define ht (make-hasheq))
     (hash-set! symbol-hts ht eql?)
     ht]
    [(and (eq? hash equal-hash-code)
          (or (eq? eql? equal?)
              (eq? eql? string=?)))
     (make-hash)]
    [(and (eq? hash values)
          (eq? eql? =))
     (make-hash)]
    [else
     (make-custom-hash eql? hash (lambda (a) 1))]))

(define (make-weak-eq-hashtable)
  (make-weak-hasheq))

(define (hash-ref/pair/dict ht key def-v)
  (if (hash? ht)
      (hash-ref/pair ht key def-v)
      (dict-ref ht key def-v)))

(define (hash-set!/pair/dict ht key v)
  (if (hash? ht)
      (hash-set!/pair ht key v)
      (dict-set! ht key v)))

(define (hashtable-keys ht)
  (list->vector (if (hash? ht)
                    (hash-keys ht)
                    (dict-keys ht))))

(define (hashtable-entries ht)
  (define ps (hash-values ht))
  (values (list->vector (map car ps))
          (list->vector (map cdr ps))))

(define (eq-hashtable? v)
  (and (hash? v) (hash-eq? v) (not (symbol-hashtable? v))))

(define (eq-hashtable-weak? v)
  (hash-weak? v))
(define (eq-hashtable-ephemeron? v)
  #f)

(define symbol-hts (make-weak-hasheq))

(define (symbol-hash x) (eq-hash-code x))

(define (symbol-hashtable? v)
  (and (hash-ref symbol-hts v #f) #t))

(define (hashtable-equivalence-function v)
  (or (hash-ref symbol-hts v #f)
      (error 'hashtable-equivalence-function "only implemented for symbol hashtables")))

(define (hashtable-mutable? ht) #t)

(define ($ht-minlen ht)
  (lookup-constant 'hashtable-default-size))

(define ($ht-veclen ht)
  (arithmetic-shift 1 (integer-length (hash-count ht))))

(define (bignum? x)
  (and (integer? x)
       (exact? x)
       (not (s:fixnum? x))))

(define (ratnum? x)
  (and (real? x)
       (exact? x)
       (not (integer? x))))

(define ($inexactnum? x)
  (and (complex? x)
       (not (real? x))
       (inexact? x)))

(define ($exactnum? x)
  (and (complex? x)
       (not (real? x))
       (exact? x)))

(define ($rtd-counts? x)
  #f)

(define (self-evaluating? v)
  (or (boolean? v)
      (number? v)
      (string? v)
      (bytes? v)
      (char? v)
      (base-rtd? v)
      (bwp? v)))

(define (weak-pair? v)
  #f)
(define (ephemeron-pair? v)
  #f)

;; The Chez Scheme compiler does not itself create
;; any immutable values, but Racket's `eval` coerces
;; to immutable. For fasl purposes, claim all as mutable.
(define any-immutable? #f)

(define (immutable-string? s)
  (and any-immutable?
       (string? s)
       (immutable? s)))

(define (immutable-vector? s)
  (and any-immutable?
       (vector? s)
       (immutable? s)))

(define (immutable-bytevector? s)
  (and any-immutable?
       (bytes? s)
       (immutable? s)))

(define (immutable-box? s)
  (and any-immutable?
       (box? s)
       (immutable? s)))

(define (list-sort pred l)
  (sort l pred))

(define (path-absolute? p)
  (absolute-path? p))

(define current-expand-set-callback void)
(define (set-current-expand-set-callback! cb)
  (set! current-expand-set-callback cb))

(define current-expand
  (let ([v expand])
    (case-lambda
      [() v]
      [(new-v)
       (set! v new-v)
       (current-expand-set-callback)])))

(define subset-mode (make-parameter 'system))
(define internal-defines-as-letrec* (make-parameter #t))
(define (eval-syntax-expanders-when) '(compile eval load))
(define require-nongenerative-clause (make-parameter #f))
(define generate-inspector-information (make-parameter #f))
(define generate-procedure-source-information (make-parameter #f))
(define enable-cross-library-optimization (make-parameter #t))
(define enable-arithmetic-left-associative (make-parameter #f))
(define enable-type-recovery (make-parameter #t))
(define fasl-compressed (make-parameter #f))

(define current-generate-id (make-parameter gensym))

(define (strip-syntax stx)
  (cond
    [(syntax-object? stx) (strip-syntax (syntax-object-e stx))]
    [(pair? stx) (cons (strip-syntax (car stx))
                       (strip-syntax (cdr stx)))]
    [else stx]))

(define (syntax-error stx . strs)
  (error 'syntax-error "~s ~a"
         (strip-syntax stx)
         (apply string-append strs)))

(define ($source-warning . args)
  (void)
  #;
  (printf "WARNING ~s\n" args))

(define-syntax (define-flag-op stx)
  (syntax-case stx ()
    [(_ get-id set-id k)
     #`(begin
         (define-syntax (get-id stx)
           (with-syntax ([prelex-flags (datum->syntax stx 'prelex-flags)])
             (syntax-case stx ()
               [(_ e) #`(positive? (bitwise-and (prelex-flags e) k))])))
         (define-syntax (set-id stx)
           (with-syntax ([prelex-flags-set! (datum->syntax stx 'prelex-flags-set!)]
                         [prelex-flags (datum->syntax stx 'prelex-flags)])
             (syntax-case stx ()
               [(_ e on?) #`(let ([v e])
                              (prelex-flags-set! v (if on?
                                                       (bitwise-ior (prelex-flags v) k)
                                                       (bitwise-and (prelex-flags v) (bitwise-not k)))))]))))]))
(define-flag-op prelex-assigned set-prelex-assigned! #b0000000100000000)
(define-flag-op prelex-referenced set-prelex-referenced! #b0000001000000000)
(define-flag-op prelex-seen set-prelex-seen! #b0000010000000000)
(define-flag-op prelex-multiply-referenced set-prelex-multiply-referenced! #b0000100000000000)

(define-syntax-rule (safe-assert . _) (void))

(define who 'some-who)

(define (with-source-path who name procedure)
  (cond
    [(equal? name "machine.def")
     (procedure (string-append target-machine ".def"))]
    [else
     (procedure name)]))

(define ($make-source-oops . args) #f)

(define ($guard else? handlers body)
  (with-handlers ([(lambda (x) #t) (if else?
                                       (lambda (v) (handlers v void))
                                       handlers)])
    (body)))
(define ($reset-protect body out) (body))

(define ($map who . args) (apply map args))

(define print-level (make-parameter #f))
(define print-depth (make-parameter #f))
(define print-length (make-parameter #f))
(define (s:pretty-format sym [fmt #f]) (void))

(define (interpret e) (eval e))

(define ($open-file-input-port who filename [options #f])
  (open-input-file filename))

(define ($open-file-output-port who filename options)
  (open-output-file filename #:exists (if (eval `(enum-set-subset? (file-options replace) ',options))
                                          'replace
                                          'error)))

(define (s:open-output-file filename [exists 'error])
  (open-output-file filename #:exists exists))

(define ($open-bytevector-list-output-port)
  (define p (open-output-bytes))
  (values p
          (lambda ()
            (define bv (get-output-bytes p))
            (values (list bv) (bytes-length bv)))))

(define (open-bytevector-output-port [transcoder #f])
  (define p (open-output-bytes))
  (values p
          (lambda () (get-output-bytes p))))

(define (native-transcoder)
  #f)

(define (port-file-compressed! p)
  (void))

(define (file-buffer-size)
  4096)

(define ($source-file-descriptor . args)
  #f)

(define (transcoded-port binary-port transcoder)
  binary-port)

(define current-transcoder (make-parameter #f))
(define (textual-port? p) #t)
(define (binary-port? p) #t)

(define (put-bytevector p bv [start 0] [end (bytes-length bv)])
  (write-bytes bv p start end))

(define (put-u8 p b)
  (if (b . < . 0)
      (write-byte (+ 256 b) p)
      (write-byte b p)))

(define (get-bytevector-n! p buf start end)
  (read-bytes! buf p start end))

(define (s:write v [o (current-output-port)])
  (if (and (gensym? v)
           (not (print-gensym)))
      (write-string (gensym->pretty-string v) o)
      (write v o)))

(define (console-output-port) (current-output-port))

(define (path-root p)
  (path->string (path-replace-suffix p #"")))

(define (path-last p)
  (define-values (base name dir?) (split-path p))
  (path->string name))

(define ($make-read p . args)
  (cond
    [(not (current-readtable))
     (lambda () (read p))]
    [else
     (lambda () (read p))]))

;; replaced when "cmacros.ss" is loaded:
(define (libspec? x) (vector? x))

(define-syntax-rule (on-reset oops e1 e2 ...)
  (let () e1 e2 ...))

(define ($pass-time name thunk) (thunk))

(define (disable-interrupts) (void))
(define (enable-interrupts) (void))
(define $tc-mutex 'tc-mutex)
(define (mutex-acquire m) (void))
(define (mutex-release m) (void))

(define $c-bufsiz 4096)

(define-syntax ($foreign-procedure stx)
  (syntax-case stx ()
    [(_ _ name . _) #'name]))

(define (make-guardian)
  (case-lambda
    [() #f]
    [(v) (void)]
    [(v rep) (void)]))

(define-syntax $lambda/lift-barrier
  (syntax-rules ()
    [(_ fmls body ...) (lambda fmls body ...)]))
