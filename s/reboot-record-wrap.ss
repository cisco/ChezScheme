;; Loads "reboot-record.ss" directly and also wraps it as a
;; `(reboort-records)` module, then defines `library` to interpose on
;; library declarations and substitute the "reboot-record.ss"
;; implementation of records.

(define-syntax define-record-type/orig
  (syntax-rules ()
    [(_ . rest) (define-record-type . rest)]))

(include "reboot-record.ss")

(define-syntax (insert-omits stx)
  ;; omits doesn't include `define-record-type`
  (let* ([rnrs-omits
          '(record?
            record-rtd
            record-type-uid
            record-type-name
            record-type-sealed?
            record-type-opaque?
            record-type-parent
            make-record-type-descriptor
            record-type-descriptor?

            make-record-constructor-descriptor
            record-constructor
            record-predicate
            record-accessor
            record-mutator
            record-type-field-names
            record-type-descriptor
            record-constructor-descriptor)]
         [chezscheme-omits
          (append
           rnrs-omits
           '(define-record
              type-descriptor)
           (filter (lambda (x)
                     (#%$top-level-bound? x))
                   '(record-constructor-descriptor?
                     r6rs:record-constructor
                     r6rs:make-record-type-descriptor
                     csv7:record-field-accessor
                     csv7:record-field-mutator
                     csv7:record-field-mutable?
                     csv7:record-field-accessible?
                     csv7:record-type-field-names
                     csv7:record-type-field-decls
                     record-writer
                     make-record-type)))])
    (syntax-case stx ()
      [(_ e)
       (let loop ([stx #'e])
         (syntax-case stx ()
           [(e ...)
            (apply
             append
             (map (lambda (e)
                    (cond
                      [(eq? (syntax->datum e) 'RNRS-OMITS)
                       (map (lambda (s) (datum->syntax e s))
                            rnrs-omits)]
                      [(eq? (syntax->datum e) 'CHEZSCHEME-OMITS)
                       (map (lambda (s) (datum->syntax e s))
                            chezscheme-omits)]
                      [else
                       (list (loop e))]))
                  #'(e ...)))]
           [_ stx]))])))

(insert-omits
 (library (reboot-records)
   (export record?
           record-rtd
           record-type-uid
           record-type-name
           record-type-sealed?
           record-type-opaque?
           record-type-parent
           make-record-type-descriptor
           r6rs:make-record-type-descriptor
           record-type-descriptor?

           make-record-constructor-descriptor
           record-constructor-descriptor?
           record-constructor
           r6rs:record-constructor
           record-predicate
           record-accessor
           record-mutator
           record-type-field-names
           record-type-descriptor
           record-constructor-descriptor
           define-record
           define-record-type

           $record?
           $sealed-record?
           $record-type-descriptor
           $make-record-type
           $make-record-type-descriptor
           $make-record-constructor-descriptor
           make-record-type
           $remake-rtd
           $record
           type-descriptor
           csv7:record-field-accessor
           csv7:record-field-mutator
           csv7:record-field-mutable?
           csv7:record-field-accessible?
           csv7:record-type-field-names
           csv7:record-type-field-decls
           record-writer
           $object-ref)
   (import (except (rename (chezscheme)
                           [define-record-type define-record-type/orig])
                   CHEZSCHEME-OMITS))
   (define-syntax define-primitive
     (syntax-rules ()
       [(_ . rest) (define . rest)]))
   (define lookup-constant
     (lambda (sym)
       (error sym "should not try to use contant here")))
   (include "reboot-record.ss")))

(insert-omits
 (library (rnrs-no-records)
   (export)
   (import (rnrs)
           (only (chezscheme)
                 export))
   (export (import (except (rnrs)
                           define-record-type
                           RNRS-OMITS)))))

(insert-omits
 (library (chezscheme-no-records)
   (export)
   (import (chezscheme))
   (export (import (except (chezscheme)
                           define-record-type
                           CHEZSCHEME-OMITS)))))

(define-syntax orig-library (top-level-syntax 'library))
(define-syntax (library stx)
  (syntax-case stx ()
    [(_ id outs (import in ...) . rest)
     (with-syntax ([(new-in ...)
                    (apply
                     append
                     (map (lambda (in)
                            (syntax-case in (rnrs chezscheme)
                              [(rnrs)
                               (with-syntax ([(id) in])
                                 (let ([same (lambda (sym) (datum->syntax #'id sym))])
                                   (list #`(#,(same 'rnrs-no-records))
                                         #`(#,(same 'reboot-records)))))]
                              [(chezscheme)
                               (with-syntax ([(id) in])
                                 (let ([same (lambda (sym) (datum->syntax #'id sym))])
                                   (list #`(#,(same 'chezscheme-no-records))
                                         #`(#,(same 'reboot-records)))))]
                              [_ (list in)]))
                          #'(in ...)))])
       #'(orig-library id outs (import new-in ...) . rest))]))

(define compile-time-value? #%compile-time-value?)
(define compile-time-value-value #%compile-time-value-value)

(define (translate-compile-time-value v)
  (if (compile-time-value? v)
      (orig-make-compile-time-value (compile-time-value-value v))
      v))

(define-syntax orig-define-syntax (top-level-syntax 'define-syntax))

(orig-define-syntax
 define-syntax
 (lambda (stx)
   (syntax-case stx ()
     [(_ (id . args) . body)
      #`(orig-define-syntax (id . args) . body)]
     [(_ id rhs)
      #`(orig-define-syntax id (translate-compile-time-value rhs))])))
