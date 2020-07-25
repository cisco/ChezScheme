#lang racket/base

(provide oblist
         s:string->symbol
         register-symbols

         putprop getprop remprop
         $sputprop $sgetprop $sremprop

         lookup-constant)

(define syms (make-hasheq))

(define (oblist)
  (hash-keys syms))

(define (s:string->symbol str)
  (define s (string->symbol str))
  (hash-set! syms s #t)
  s)

(define (register-symbols v)
  (cond
    [(symbol? v) (hash-set! syms v #t)]
    [(pair? v)
     (register-symbols (car v))
     (register-symbols (cdr v))]
    [(box? v)
     (register-symbols (unbox v))]
    [(vector? v)
     (for ([i (in-vector v)])
       (register-symbols v))]))

     
(define (make-put-get ht)
  (values
   (lambda (sym key val)
     (hash-set! syms sym #t)
     (hash-update! ht sym (lambda (ht) (hash-set ht key val)) #hasheq()))
   (lambda (sym key [def-val #f])
     (hash-ref (hash-ref ht sym #hasheq()) key def-val))
   (lambda (sym key)
     (hash-update! ht sym (lambda (ht) (hash-remove ht key)) #hasheq()))))

(define-values (putprop getprop remprop) (make-put-get (make-hasheq)))
(define-values ($sputprop $sgetprop $sremprop) (make-put-get (make-hasheq)))

(define (lookup-constant key [fail #f])
  (or (getprop key '*constant* #f)
      (if fail
          (fail)
          (error key "cannot find value"))))
