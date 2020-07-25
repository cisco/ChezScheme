#lang racket/base
(require (only-in racket/base
                  [gensym r:gensym]))

;; Represent a gensym as a symbol of the form |{....}| where the
;; "pretty name" must not contain spaces.

(provide print-gensym
         gensym
         $intern3
         gensym?
         gensym->unique-string
         gensym->pretty-string
         hash-curly
         uninterned-symbol?)

(define print-gensym (make-parameter #t))

(define gensym
  (case-lambda
    [() (gensym (r:gensym))]
    [(pretty-name)
     (gensym pretty-name (r:gensym "unique"))]
    [(pretty-name unique-name)
     (string->symbol
      (format "{~a ~a}" pretty-name unique-name))]))

(define ($intern3 gstring pretty-len full-len)
  (gensym (substring gstring 0 pretty-len) gstring))

(define (gensym? s)
  (and (symbol? s)
       (let ([str (symbol->string s)])
         (define len (string-length str))
         (and (positive? len)
              (char=? #\{ (string-ref str 0))
              (char=? #\} (string-ref str (sub1 len)))))))

(define (gensym->unique-string s)
  (cadr (regexp-match #rx"^{[^ ]* (.*)}$" (symbol->string s))))

(define (gensym->pretty-string s)
  (cadr (regexp-match #rx"^{([^ ]*) .*}$" (symbol->string s))))

(define (hash-curly c in src line col pos)
  (define sym
    (string->symbol
     (list->string
      (cons
       #\{
       (let loop ()
         (define ch (read-char in))
         (if (eqv? ch #\})
             '(#\})
             (cons ch (loop))))))))
  (when (regexp-match? #rx"[|]" (symbol->string sym))
    (error "here"))
  sym)

(define (uninterned-symbol? v)
  (and (symbol? v)
       (not (or (symbol-interned? v)
                (symbol-unreadable? v)))))

