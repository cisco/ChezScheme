#lang racket/base

(provide strip-$primitive
         strip-$app)

(define (strip-$primitive e)
  (cond
    [(and (pair? e)
          (eq? (car e) 'quote))
     e]
    [(and (pair? e)
          (eq? (car e) '$primitive))
     (if (pair? (cddr e))
         (caddr e)
         (cadr e))]
    [(list? e)
     (map strip-$primitive e)]
    [else e]))

(define (strip-$app e)
  (cond
    [(and (pair? e)
          (eq? (car e) 'quote))
     e]
    [(and (pair? e)
          (eq? (car e) '$app))
     (strip-$app (cdr e))]
    [(list? e)
     (map strip-$app e)]
    [else e]))
