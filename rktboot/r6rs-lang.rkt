#lang racket/base
(require (for-syntax racket/base)
         (for-template racket/base)
         racket/fixnum
         racket/flonum
         racket/pretty
         racket/list
         racket/splicing
         racket/unsafe/ops
         "nanopass-patch.rkt"
         "gensym.rkt"
         "format.rkt"
         "syntax-mode.rkt"
         "config.rkt"
         "rcd.rkt"
         (only-in "record.rkt"
                  do-$make-record-type
                  register-rtd-name!
                  register-rtd-fields!
                  register-rtd-ancestors!
                  s:struct-type?
                  record-predicate
                  record-accessor
                  record-mutator)
         (only-in "immediate.rkt"
                  base-rtd)
         (only-in "scheme-struct.rkt"
                  syntax-object syntax-object? syntax-object-e syntax-object-ctx
                  rec-cons-desc rec-cons-desc? rec-cons-desc-rtd rec-cons-desc-parent-rcd rec-cons-desc-protocol
                  top-ribcage))

(provide (except-out (all-from-out racket/base
                                   racket/fixnum
                                   racket/flonum)
                     define
                     syntax
                     syntax-case
                     syntax-rules
                     with-syntax
                     quasisyntax
                     define-syntax
                     syntax->datum
                     module
                     let-syntax
                     letrec-syntax
                     symbol->string
                     format error
                     if
                     sort
                     fixnum?
                     open-output-file
                     dynamic-wind)
         library import export
         (rename-out [patch:define define]
                     [s:syntax syntax]
                     [s:syntax-case syntax-case]
                     [s:syntax-rules syntax-rules]
                     [s:with-syntax with-syntax]
                     [s:quasisyntax quasisyntax]
                     [s:define-syntax define-syntax]
                     [s:syntax->datum syntax->datum]
                     [make-set!-transformer make-variable-transformer]
                     [s:if if]
                     [lambda trace-lambda]
                     [define-syntax trace-define-syntax]
                     [s:splicing-let-syntax let-syntax]
                     [s:splicing-letrec-syntax letrec-syntax]
                     [let trace-let]
                     [define trace-define]
                     [s:dynamic-wind dynamic-wind])
         guard
         identifier-syntax
         (for-syntax datum)
         assert
         (rename-out [zero? fxzero?])
         gensym gensym? gensym->unique-string
         (rename-out [s:symbol->string symbol->string])
         pretty-print
         with-input-from-string with-output-to-string
         define-record-type
         record-type-descriptor
         make-record-type-descriptor
         make-record-type-descriptor*
         make-record-constructor-descriptor
         (rename-out [s:struct-type? record-type-descriptor?])
         record-constructor-descriptor
         record-constructor
         (rename-out [record-constructor r6rs:record-constructor])
         record-predicate
         record-accessor
         record-mutator
         record-constructor-descriptor?
         syntax-violation
         port-position
         close-port
         eof-object
         struct-name struct-ref
         make-list memp partition fold-left fold-right find remp remv
         (rename-out [andmap for-all]
                     [ormap exists]
                     [list* cons*]
                     [s:fixnum? fixnum?]
                     [fx= fx=?]
                     [fx< fx<?]
                     [fx> fx>?]
                     [fx<= fx<=?]
                     [fx>= fx>=?]
                     [fxlshift fxarithmetic-shift-left]
                     [fxnot fxlognot]
                     [odd? fxodd?]
                     [even? fxeven?]
                     [div fxdiv]
                     [mod fxmod]
                     [div-and-mod fxdiv-and-mod]
                     [integer-length fxlength]
                     [exact->inexact inexact]
                     [inexact->exact exact]
                     [bitwise-reverse-bit-field fxreverse-bit-field]
                     [bitwise-copy-bit-field fxcopy-bit-field]
                     [bitwise-copy-bit fxcopy-bit]
                     [make-hasheq make-eq-hashtable]
                     [hash-ref/pair hashtable-ref]
                     [hash-set!/pair hashtable-set!]
                     [hash-set!/pair eq-hashtable-set!]
                     [hash-ref-cell hashtable-cell]
                     [equal-hash-code equal-hash]
                     [s:format format]
                     [s:error error])
         most-positive-fixnum
         most-negative-fixnum
         bitwise-copy-bit-field
         bitwise-copy-bit
         bitwise-first-bit-set
         bitwise-if
         div mod div-and-mod
         fixnum-width
         set-car!
         set-cdr!
         bytevector-copy!
         bytevector-ieee-double-native-set!
         bytevector-ieee-double-native-ref
         bytevector-u64-native-set!
         bytevector-u64-native-ref
         call-with-bytevector-output-port
         make-compile-time-value
         optimize-level
         symbol-value
         set-symbol-value!)

(module+ ikarus
  (provide print-gensym
           annotation? annotation-source
           source-information-type
           source-information-position-line
           source-information-position-column
           source-information-source-file
           source-information-byte-offset-start
           source-information-byte-offset-end
           source-information-char-offset-start
           source-information-char-offset-end
           syntax->source-information
           (rename-out [s:module module])
           indirect-export
           (for-syntax with-implicit)))

(module+ hash-pair
  (provide hash-ref/pair
           hash-set!/pair
           hash-ref-cell
           s:fixnum?))

(begin-for-syntax
  (define here-path
    (let ([p (resolved-module-path-name
              (module-path-index-resolve
               (variable-reference->module-path-index
                (#%variable-reference))))])
      (if (path? p)
          (path->string p)
          `(quote ,p)))))

(define-syntax (library stx)
  (syntax-case stx (nanopass export import)
    [(library (nanopass name)
       (export out ...)
       (import in ...)
       body ...)
     (with-syntax ([here (datum->syntax #'name `(file ,here-path))])
       #'(module name here
           (require (for-syntax here)
                    (except-in (for-template here) datum))
           (export out) ...
           (import in) ...
           body ...))]
    [(library (nanopass) . rest)
     (syntax-case stx ()
       [(_ (np) . _)
        #'(library (np np) . rest)])]))

(define-syntax-rule (export id)
  (provide id))

(define-syntax-rule (indirect-export . _)
  (begin))

(define-syntax (import stx)
  (syntax-case stx (rnrs ikarus nanopass only chezscheme)
    [(import (rnrs _ ...))
     #'(begin)]
    [(import (ikarus))
     (syntax-case stx ()
       [(_ (name))
        (with-syntax ([ref (datum->syntax #'name `(submod (file ,here-path) ikarus))])
          #`(require ref))])]
    [(import (nanopass name))
     (with-syntax ([ref (datum->syntax #'name (list 'quote #'name))])
       #`(require ref (for-syntax ref) (for-template ref)))]
    [(import (only (chezscheme) . _))
     #'(begin)]))

(define-syntax (s:syntax stx)
  (syntax-case stx ()
    [(_ e)
     #`(unwrap-a-bit (syntax #,(mark-original #'e)))]))

(define-syntax (s:syntax-case stx)
  (syntax-case stx ()
    [(_ e lits . rest)
     #'(syntax-case* (strip-outer-struct e) lits s:free-identifier=? . rest)]))

(define-syntax-rule (s:syntax-rules lits [a ... b] ...)
  (lambda (stx)
    (s:syntax-case stx lits
      [a ... (s:syntax b)]
      ...)))

(define-syntax (s:with-syntax stx)
  (syntax-case stx ()
    [(_ ([pat e] ...) . rest)
     #'(with-syntax ([pat (strip-outer-struct e)] ...) . rest)]))

(define-syntax (s:quasisyntax stx)
  (syntax-case stx ()
    [(_ e)
     (with-syntax ([qs #'quasisyntax])
       #`(unwrap-a-bit (qs #,(mark-original #`e))))]))

(define-for-syntax (mark-original e)
  (cond
    [(syntax? e)
     (define v (syntax-e e))
     (cond
       [(pair? v)
        (datum->syntax e
                       (cons (mark-original (car v))
                             (mark-original (cdr v)))
                       e
                       e)]
       [(vector? v)
        (for/vector #:length (vector-length v) ([i (in-vector v)])
          (mark-original i))]
       [(identifier? e) (syntax-property e 'original-in-syntax #t)]
       [else e])]
    [(pair? e)
     (cons (mark-original (car e))
           (mark-original (cdr e)))]
    [else e]))

(define (unwrap-a-bit e)
  (cond
    [fully-unwrap?
     ;; Support use of `syntax-case` in expander implementation
     ;; after the expander itself is expanded.
     (let loop ([e e])
       (cond
         [(syntax? e)
          (cond
            [(and (identifier? e)
                  (syntax-property e 'original-in-syntax))
             (syntax-object (syntax-e e)
                            (cons '(top) (list (top-ribcage '*system* #f))))]
            [else
             (define v (loop (syntax-e e)))
             (define p (syntax-property e 'save-context))
             (if p
                 (syntax-object v p)
                 v)])]
         [(pair? e)
          (cons (loop (car e))
                (loop (cdr e)))]
         [(vector? e)
          (for/vector #:length (vector-length e) ([i (in-vector e)])
            (loop i))]
         [else e]))]
    [else
     ;; Simulate R6RS well enough
     (or (syntax->list e)
         e)]))

;; Also to support use of `syntax-case` in expander implementation
;; after the expander itself is expanded:
(define strip-outer-struct
  (let ()
    (lambda (e)
      (let loop ([e e] [w empty-wraps])
        (cond
          [(syntax-object? e)
           (define v (syntax-object-e e))
           (define new-w (join-wraps w (syntax-object-ctx e)))
           (cond
             [(pair? v)
              (cons (loop (car v) new-w)
                    (loop (cdr v) new-w))]
             [(null? v) v]
             [else
              (syntax-property (datum->syntax #f v) 'save-context new-w)])]
          [(pair? e)
           (cons (loop (car e) w)
                 (loop (cdr e) w))]
          [(vector? e)
           (for/vector #:length (vector-length e) ([i (in-vector e)])
             (loop i w))]
          [(box? e)
           (box (loop (unbox e) w))]
          [(symbol? e)
           (if (equal? w empty-wraps)
               e
               (syntax-property (datum->syntax #f e) 'save-context w))]
          [else e])))))

(define (s:free-identifier=? a b)
  (if fully-unwrap?
      (eq? (syntax-e a) (syntax-e b))
      (free-identifier=? a b)))

(define empty-wraps '(() . ()))

(define (join-wraps w1 w2)
  (define a (join (car w1) (car w2)))
  (define d (join (cdr w1) (cdr w2)))
  (cond
    [(and (eq? a (car w1))
          (eq? d (cdr w1)))
     w1]
    [(and (eq? a (car w2))
          (eq? d (cdr w2)))
     w2]
    [else (cons a d)]))

(define (join l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (append l1 l2)]))

(define (s:syntax->datum s)
  (syntax->datum (datum->syntax #f s)))

(define-syntax-rule (s:define-syntax id rhs)
  (define-syntax id
    (wrap-transformer rhs)))

(define-syntax-rule (s:splicing-let-syntax ([id rhs] ...) body ...)
  (splicing-let-syntax ([id (wrap-transformer rhs)] ...) body ...))

(define-syntax-rule (s:splicing-letrec-syntax ([id rhs] ...) body ...)
  (splicing-letrec-syntax ([id (wrap-transformer rhs)] ...) body ...))

(define-for-syntax (wrap-transformer proc)
  (if (procedure? proc)
      (lambda (stx)
        (let loop ([result (proc stx)])
          (if (procedure? result)
              ;; Chez/Ikarus protocol to get syntax-local-value:
              (loop (result syntax-local-value))
              (datum->syntax #'here result))))
      proc))

(define-syntax s:if
  (syntax-rules ()
    [(_ tst thn els) (if tst thn els)]
    [(_ tst thn) (if tst thn (void))]))

(define-syntax-rule (guard (id [tst rslt ...] ...) body ...)
  (with-handlers ([(lambda (id) (else-to-true tst)) (lambda (id) rslt ...)] ...)
    body ...))

(define-syntax else-to-true
  (syntax-rules (else)
    [(_ else) #t]
    [(_ e) e]))

(define s:dynamic-wind
  (case-lambda
    [(pre thunk post) (dynamic-wind pre thunk post)]
    [(critical? pre thunk post) (dynamic-wind pre thunk post)]))

(begin-for-syntax
  (define-syntax-rule (with-implicit (tid id ...) body ...)
    (with-syntax ([id (datum->syntax (syntax tid) 'id)] ...)
      body ...)))

(begin-for-syntax
  (define-syntax-rule (datum e)
    (syntax->datum (syntax e))))
 
(define-syntax (identifier-syntax stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     #'(make-rename-transformer #'id)]
    [(_ e)
     #'(lambda (stx)
         (if (identifier? stx)
             #'e
             (syntax-case stx ()
               [(_ arg (... ...))
                #'(e arg (... ...))])))]))

(define-syntax-rule (s:module (id ...) body ...)
  (begin
    body ...))

(define-syntax-rule (assert e)
  (unless e
    (error 'assert "failed: ~s" 'e)))

(define (syntax->source-information stx) #f)
(define (source-information-type si) #f)
(define (source-information-position-line si) #f)
(define (source-information-position-column si) #f)
(define (source-information-source-file si) #f)
(define (source-information-byte-offset-start si) #f)
(define (source-information-byte-offset-end si) #f)
(define (source-information-char-offset-start si) #f)
(define (source-information-char-offset-end si) #f)

(define (syntax-violation . args)
  (apply error args))

(define (s:symbol->string s)
  (if (gensym? s)
      (gensym->pretty-string s)
      (symbol->string s)))

(define (with-input-from-string str proc)
  (parameterize ([current-input-port (open-input-string str)])
    (proc)))

(define (with-output-to-string proc)
  (define o (open-output-string))
  (parameterize ([current-output-port o])
    (proc))
  (get-output-string o))

(define protocols (make-hasheq))
(define (install-protocol! rtd protocol)
  (hash-set! protocols rtd protocol))
(define (lookup-protocol rtd)
  (hash-ref protocols rtd))

(define-syntax (define-record-type stx)
  (syntax-case stx ()
    [(_ (name make-name name?) clause ...)
     (let loop ([clauses #'(clause ...)] [fs #'()] [p #f] [super #f] [uid #f] [o? #f] [s? #f])
       (syntax-case clauses (nongenerative sealed fields protocol parent opaque sealed)
         [((nongenerative uid) clause ...)
          (loop #'(clause ...) fs p super #'uid o? s?)]
         [((nongenerative . _) clause ...)
          (loop #'(clause ...) fs p super uid o? s?)]
         [((sealed _) clause ...)
          (loop #'(clause ...) fs p super uid o? s?)]
         [((fields field ...) clause ...)
          (loop #'(clause ...) #'(field ...) p super uid o? s?)]
         [((protocol proc) clause ...)
          (loop #'(clause ...) fs #'proc super uid o? s?)]
         [((parent super) clause ...)
          (loop #'(clause ...) fs p #'super uid o? s?)]
         [((opaque #t) clause ...)
          (loop #'(clause ...) fs p super uid #t s?)]
         [((sealed #t) clause ...)
          (loop #'(clause ...) fs p super uid o? #t)]
         [()
          (let ()
            (define (format-id ctx fmt . args)
              (datum->syntax ctx (string->symbol
                                  (apply format fmt (map syntax-e args)))))
            (define (normalize-fields l)
              (for/list ([f (in-list (syntax->list l))])
                (syntax-case f (mutable immutable)
                  [id
                   (identifier? #'id)
                   (list #'id (format-id #'id  "~a-~a" #'name #'id))]
                  [(mutable id)
                   (list #'id
                         (format-id #'id  "~a-~a" #'name #'id)
                         (format-id #'id  "~a-~a-set!" #'name #'id))]
                  [(immutable id)
                   (list #'id (format-id #'id  "~a-~a" #'name #'id))]
                  [(mutable id ref set)
                   (list #'id #'ref #'set)]
                  [(immutable id ref)
                   (list #'id #'ref)])))
            (define all-fs (normalize-fields fs))
            (define fs-ids (for/list ([f (in-list all-fs)])
                             (syntax-case f ()
                               [(id . _) #'id])))
            (define parent-info (and super (syntax-local-value super)))
            (with-syntax ([num-fields (length all-fs)]
                          [protocol (or p
                                        (if super
                                            #`(lambda (parent-maker)
                                                (lambda (#,@(list-ref parent-info 3) #,@fs-ids)
                                                  ((parent-maker #,@(list-ref parent-info 3)) #,@fs-ids)))
                                            #'(lambda (p) p)))]
                          [maker (if super
                                     #`(let ([parent-protocol (lookup-protocol #,(car parent-info))])
                                         (lambda args
                                           (apply (parent-protocol
                                                   (lambda #,(list-ref parent-info 3)
                                                     (lambda #,fs-ids
                                                       (create-name #,@(list-ref parent-info 3) #,@fs-ids))))
                                                  args)))
                                     #'create-name)]
                          [(getter ...)
                           (for/list ([f (in-list all-fs)]
                                      [pos (in-naturals)])
                             (syntax-case f ()
                               [(id ref . _) (list #'ref
                                                   #`(make-struct-field-accessor name-ref #,pos 'id))]))]
                          [(setter ...)
                           (for/list ([f (in-list all-fs)]
                                      [pos (in-naturals)]
                                      #:when (syntax-case f ()
                                               [(_ _ _) #t]
                                               [_ #f]))
                             (syntax-case f ()
                               [(id _ set) (list #'set
                                                 #`(make-struct-field-mutator name-set! #,pos 'id))]))]
                          [super (if super
                                     (car (syntax-local-value super))
                                     #'#f)]
                          [struct:name (format-id #'name "struct:~a" #'name)]
                          [uid (or uid #'name)]
                          [maybe-prefab (if uid #''prefab #'#f)]
                          [fields-vec (list->vector (syntax-e fs))])
              (with-syntax ([(all-getter-id ...)
                             (append (for/list ([getter (in-list (reverse (syntax->list #'(getter ...))))])
                                       (syntax-case getter ()
                                         [(id . _) #'id]))
                                     (if parent-info
                                         (list-ref parent-info 3)
                                         null))])
                #`(begin
                    (define-syntax name
                      (list (quote-syntax struct:name)
                            (quote-syntax create-name)
                            (quote-syntax name?)
                            (list (quote-syntax all-getter-id) ...)
                            #f
                            #f))
                    (define-values (struct:name create-name name? name-ref name-set!)
                      (make-struct-type 'uid super num-fields 0 #f null maybe-prefab))
                    (define name-protocol protocol)
                    (install-protocol! struct:name name-protocol)
                    (register-rtd-name! struct:name 'name)
                    (register-rtd-fields! struct:name 'fields-vec)
                    (register-rtd-ancestors! struct:name super)
                    (define make-name (name-protocol maker))
                    (define . getter) ...
                    (define . setter) ...))))]))]
    [(_ name clause ...)
     (with-syntax ([make-name (datum->syntax #'name
                                             (string->symbol
                                              (format "make-~a" (syntax-e #'name)))
                                             #'name)]
                   [name? (datum->syntax #'name
                                         (string->symbol
                                          (format "~a?" (syntax-e #'name)))
                                         #'name)])
       #`(define-record-type (name make-name name?) clause ...))]))

(define-syntax (record-type-descriptor stx)
  (syntax-case stx ()
    [(_ id)
     (car (syntax-local-value #'id))]))

(define-syntax (record-constructor-descriptor stx)
  (syntax-case stx ()
    [(_ id)
     #`(rtd->rcd #,(car (syntax-local-value #'id)))]))

(define record-constructor-descriptor? rec-cons-desc?)

(define (rtd->rcd rtd)
  (rec-cons-desc rtd #f (lookup-protocol rtd)))

(define (record-constructor rcd)
  (cond
    [(s:struct-type? rcd)
     ;; For Chez Scheme's legacy procedure
     (struct-type-make-constructor rcd)]
    [(rec-cons-desc? rcd)
     (rcd->constructor rcd lookup-protocol)]))

(define (make-record-type-descriptor name parent uid s? o? fields)
  (do-$make-record-type base-rtd parent name fields s? o? null #:uid uid))

(define (make-record-type-descriptor* name parent uid s? o? num-fields mutability-mask)
  (define fields (for ([i (in-range num-fields)])
                   (list (if (bitwise-bit-set? mutability-mask i) 'mutable 'immutable)
                         (string->symbol (format "f~a" i)))))
  (do-$make-record-type base-rtd parent name fields s? o? null #:uid uid))

(define (make-record-constructor-descriptor rtd parent-rcd protocol)
  (rec-cons-desc rtd parent-rcd protocol))

(define (annotation? a) #f)
(define (annotation-source a) #f)

(define (port-position ip) (file-position ip))

(define (close-port p)
  (if (input-port? p)
      (close-input-port p)
      (close-output-port p)))

(define (eof-object)
  eof)

(define (struct-name a) (substring (symbol->string (vector-ref (struct->vector a) 0))
                                   ;; drop "struct:"
                                   7))
(define (struct-ref s i) (error 'struct-ref "oops"))

(define (make-list n [v #f])
  (vector->list (make-vector n v)))

(define (memp pred l)
  (cond
    [(null? l) #f]
    [(pred (car l)) l]
    [else (memp pred (cdr l))]))

(define (remp pred l)
  (cond
    [(null? l) l]
    [(pred (car l)) (remp pred (cdr l))]
    [else (cons (car l) (remp pred (cdr l)))]))

(define (remv v l)
  (cond
    [(null? l) l]
    [(eqv? v (car l)) (remv v (cdr l))]
    [else (cons (car l) (remv v (cdr l)))]))

(define (partition proc list)
  (let loop ((list list) (yes '()) (no '()))
    (cond ((null? list)
           (values (reverse yes) (reverse no)))
          ((proc (car list))
           (loop (cdr list) (cons (car list) yes) no))
          (else
           (loop (cdr list) yes (cons (car list) no))))))

(define (fold-left combine nil the-list . the-lists)
  (if (null? the-lists)
      (fold-left1 combine nil the-list)
      (let loop ((accum nil) (list the-list) (lists the-lists))
        (if (null? list)
            accum
            (loop (apply combine accum (car list) (map car lists))
                  (cdr list)
                  (map cdr lists))))))

(define (fold-left1 combine nil list)
  (let loop ((accum nil) (list list))
    (if (null? list)
        accum
        (loop (combine accum (car list))
              (cdr list)))))

(define (fold-right combine nil the-list . the-lists)
  (if (null? the-lists)
      (fold-right1 combine nil the-list)
      (let recur ((list the-list) (lists the-lists))
        (if (null? list)
            nil
            (apply combine
                   (car list)
                   (append (map car lists)
                           (cons (recur (cdr list) (map cdr lists))
                                 '())))))))

(define (fold-right1 combine nil list)
  (let recur ((list list))
    (if (null? list)
        nil
        (combine (car list) (recur (cdr list))))))

(define (find proc list)
  (let loop ((list list))
    (cond
      ((null? list) #f)
      ((proc (car list)) (car list))
      (else (loop (cdr list))))))

(define (bitwise-if a b c)
  (bitwise-ior (bitwise-and a b)
               (bitwise-and (bitwise-not a) c)))

(define (bitwise-reverse-bit-field n start end)
  (let ([field (bitwise-bit-field n start end)]
        [width (- end start)])
    (let loop ([old field][new 0][width width])
      (cond
       [(zero? width) (bitwise-copy-bit-field n start end new)]
       [else (loop (arithmetic-shift old -1)
                   (bitwise-ior (arithmetic-shift new 1)
                                (bitwise-and old 1))
                   (sub1 width))]))))

(define (bitwise-copy-bit-field to start end from)
  (let* ([mask1 (arithmetic-shift -1 start)]
         [mask2 (bitwise-not (arithmetic-shift -1 end))]
         [mask (bitwise-and mask1 mask2)])
    (bitwise-if mask
                (arithmetic-shift from start)
                to)))

(define (bitwise-first-bit-set b)
  (if (zero? b)
      -1
      (let loop ([b b][pos 0])
        (if (zero? (bitwise-and b 1))
            (loop (arithmetic-shift b -1) (add1 pos))
            pos))))

(define (bitwise-copy-bit b n bit)
  (if (eq? bit 1)
      (bitwise-ior b (arithmetic-shift 1 n))
      (bitwise-and b (bitwise-not (arithmetic-shift 1 n)))))

(define (div x y)
  (quotient x y))

(define (mod x y)
  (modulo x y))

(define (div-and-mod x y)
  (values (div x y) (mod x y)))

(define (hash-ref/pair ht key def-v)
  (cdr (hash-ref ht key (cons #f def-v))))

(define (hash-set!/pair ht key val)
  (hash-set! ht key (cons (and (not (hash-weak? ht)) key) val)))

(define (hash-ref-cell ht key def-v)
  (or (hash-ref ht key #f)
      (begin
        (hash-set!/pair ht key def-v)
        (hash-ref-cell ht key def-v))))

;; HACK!
(define-syntax (define-mutable-pair-hacks stx)
  (syntax-case stx ()
    [(_ set-car! set-cdr!)
     (cond
       [(eq? 'chez-scheme (system-type 'vm))
        #'(begin
            (require racket/linklet)
            (define chez-eval (instantiate-linklet
                               (compile-linklet '(linklet () () eval))
                               null
                               (make-instance 'scheme)))
            (define set-car! (chez-eval 'set-car!))
            (define set-cdr! (chez-eval 'set-cdr!)))]
       [else
        #'(begin
            (define (set-car! p v) (unsafe-set-mcar! p v))
            (define (set-cdr! p v) (unsafe-set-mcdr! p v)))])]))
(define-mutable-pair-hacks set-car! set-cdr!)

(define (bytevector-copy! src src-start dst dst-start n)
  (bytes-copy! dst dst-start src src-start (+ src-start n)))

(define (bytevector-ieee-double-native-set! bv pos val)
  (real->floating-point-bytes val 8 (system-big-endian?) bv pos))
(define (bytevector-ieee-double-native-ref bv pos)
  (floating-point-bytes->real bv (system-big-endian?) pos (+ pos 8)))

(define (bytevector-u64-native-set! bv pos val)
  (integer->integer-bytes val 8 #f (system-big-endian?) bv pos))
(define (bytevector-u64-native-ref bv pos)
  (integer-bytes->integer bv #f (system-big-endian?) pos (+ pos 8)))

(define (call-with-bytevector-output-port proc)
  (define o (open-output-bytes))
  (proc o)
  (get-output-bytes o))

;; Note: fixnums here are compile-time fixnums, so "config.rkt" is not needed

(define 64-bit? (= (system-type 'word) 64))

(define (fixnum-width) (if (eq? 'racket (system-type 'vm))
                           (if 64-bit? 63 31)
                           (if 64-bit? 61 30)))
(define low-fixnum (- (expt 2 (sub1 (fixnum-width)))))
(define high-fixnum (sub1 (expt 2 (sub1 (fixnum-width)))))

(define s:fixnum? fixnum?)

(define (most-positive-fixnum) high-fixnum)
(define (most-negative-fixnum) low-fixnum)

(define (make-compile-time-value v) v)

(define optimize-level (make-parameter optimize-level-init))

;; For "implementation-helpers.ikarus.ss":
(define (symbol-value s) (namespace-variable-value s #f))
(define (set-symbol-value! s v) (namespace-set-variable-value! s v #f))
