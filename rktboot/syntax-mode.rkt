#lang racket/base

(provide fully-unwrap?
         start-fully-unwrapping-syntax!)

(define fully-unwrap? #f)
(define (start-fully-unwrapping-syntax!) (set! fully-unwrap? #t))
