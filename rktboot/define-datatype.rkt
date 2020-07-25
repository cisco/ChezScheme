#lang racket/base
(require (for-syntax racket/base))

(provide define-datatype)

(define-syntax define-datatype
  (lambda (stx)
    (syntax-case stx ()
      [(_ name (variant field ...) ...)
       (identifier? #'name)
       #'(define-datatype (name) (variant field ...) ...)]
      [(_ (name base-field ...) (variant field ...) ...)
       (let ([clean (lambda (l)
                      (map (lambda (f)
                             (syntax-case f ()
                               [(_ id) #'id]
                               [id #'id]))
                           (syntax->list l)))])
         (with-syntax ([(base-field ...) (clean #'(base-field ...))]
                       [((field ...) ...) (map clean
                                               (syntax->list #'((field ...) ...)))]
                       [(name-variant ...) (for/list ([variant (in-list (syntax->list #'(variant ...)))])
                                             (format-id variant "~a-~a" #'name variant))]
                       [([set-name-base-field! name-base-field-set!] ...)
                        (for/list ([base-field (in-list (syntax->list #'(base-field ...)))])
                          (define field (syntax-case base-field ()
                                          [(_ id) #'id]
                                          [id #'id]))
                          (list (format-id field "set-~a-~a!" #'name field)
                                (format-id field "~a-~a-set!" #'name field)))]
                       [name-case (format-id #'name "~a-case" #'name)])
           #'(begin
               (define-struct name (base-field ...) #:mutable)
               (define name-base-field-set! set-name-base-field!) ...
               (define-struct (name-variant name) (field ...))
               ...
               (define-syntax (name-case stx)
                 (generate-case stx #'[(name base-field ...)
                                       (variant field ...) ...])))))])))

(define-for-syntax (generate-case stx spec)
  (syntax-case spec ()
    [[(name base-field ...) (variant field ...) ...]
     (let ([variants (syntax->list #'(variant ...))]
           [fieldss (syntax->list #'((field ...) ...))])
       (syntax-case stx ()
         [(_ expr clause ...)
          (with-syntax ([([lhs rhs ...] ...)
                         (for/list ([clause (in-list (syntax->list #'(clause ...)))])
                           (syntax-case clause (else)
                             [[else . _] clause]
                             [[c-variant (c-field ...) rhs ...]
                              (or (for/or ([variant (in-list variants)]
                                           [fields (in-list fieldss)]
                                           #:when (eq? (syntax-e #'c-variant) (syntax-e variant)))
                                    (with-syntax ([variant? (format-id variant "~a-~a?" #'name variant)]
                                                  [(field-ref ...) (for/list ([field (in-list (syntax->list fields))])
                                                                     (format-id field "~a-~a-~a" #'name variant field))])
                                      #`[(variant? v)
                                     (let ([c-field (field-ref v)] ...)
                                       rhs ...)]))
                                  (raise-syntax-error #f
                                                      "no matching variant"
                                                      stx
                                                      clause))]
                             [_ (raise-syntax-error #f
                                                    "unrecognized clause"
                                                    stx
                                                    clause)]))])
            #'(let ([v expr])
                (cond
                  [lhs rhs ...] ...)))]))]))

(define-for-syntax (format-id ctx fmt . args)
  (datum->syntax
   ctx
   (string->symbol
    (apply format fmt (map syntax-e args)))))
