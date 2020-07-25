#lang racket/base
(require "gensym.rkt")

(provide r6rs-readtable)

(define (hash-bang c in src line col pos)
  (make-special-comment (read-syntax/recursive src in)))

(define r6rs-readtable
  (make-readtable
   #f
   #\! 'dispatch-macro hash-bang
   #\{ 'dispatch-macro hash-curly))
