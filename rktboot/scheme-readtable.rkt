#lang racket/base
(require racket/fixnum
         racket/flonum
         racket/port
         "immediate.rkt"
         "gensym.rkt")

(provide scheme-readtable)

(define (hash-three c in src line col pos)
  (define got-c (peek-char in))
  (cond
    [(eqv? #\% got-c)
     (read-char in)
     `($primitive 3 ,(read/recursive in))]
    [else
     (hash-graph #\3 in src line col pos)]))

(define (hash-two c in src line col pos)
  (define got-c (peek-char in))
  (cond
    [(eqv? #\% got-c)
     (read-char in)
     `($primitive 2 ,(read/recursive in))]
    [else
     (hash-graph #\2 in src line col pos)]))

(define (hash-one c in src line col pos)
  (define got-c (peek-char in))
  (cond
    [(eqv? #\# got-c)
     ;; "read.ss" has a `#1#` reference before the
     ;; `#1=...` definition; it's going to turn out
     ;; to be `black-hole`
     (define name (object-name in))
     (cond
       [(and (or (string? name) (path? name))
             (regexp-match? #rx"read[.]ss$" name))
        (read-char in)
        black-hole]
       [else
        (hash-graph #\1 in src line col pos)])]
    [else
     (hash-graph #\1 in src line col pos)]))

(define (hash-graph c in src line col pos)
  (cond
    [(and (eqv? (peek-char in) #\=)
          (eqv? (peek-char in 1) #\#)
          (eqv? (peek-char in 2) c)
          (eqv? (peek-char in 3) #\#))
     (read-string 4 in)
     black-hole]
    [else
     (define new-in (input-port-append #f (open-input-string (string #\# c)) in))
     (read/recursive new-in #f #f #t)]))

(define (hash-percent c in src line col pos)
  `($primitive ,(read/recursive in)))

(define (hash-bang c in src line col pos)
  (define sym (read/recursive in))
  (case sym
    [(eof) eof]
    [(base-rtd) base-rtd]
    [(bwp) bwp]
    [(chezscheme) (make-special-comment 'chezscheme)]
    [else (error 'hash-bang "unrecognized ~s" sym)]))

(define ((paren closer) c in src line col pos)
  ;; parse a list, but allow an eof element as produced by #!eof
  (let loop ()
    (define c (peek-char in))
    (cond
      [(eqv? closer c)
       (read-char in)
       null]
      [(char-whitespace? c)
       (read-char in)
       (loop)]
      [(and (eqv? #\. c)
            (char-whitespace? (peek-char in 1)))
       (read-char in)
       (begin0
         (read/recursive in)
         (let loop ()
           (define c (read-char in))
           (cond
             [(char-whitespace? c) (loop)]
             [(eqv? c closer) (void)]
             [else (error 'parens "unexpected: ~s" c)])))]
      [else
       (define v (read/recursive in))
       (if (special-comment? v)
           (loop)
           (cons v (loop)))])))

(define (hash-backslash c in src line col pos)
  (define next-c (peek-char in))
  (cond
    [(or (char-alphabetic? next-c)
         (char-numeric? next-c))
     (define sym (read/recursive in))
     (case sym
       [(newline) #\newline]
       [(return) #\return]
       [(nel) #\u85]
       [(ls) #\u2028]
       [(space) #\space]
       [(nul) #\nul]
       [(tab) #\tab]
       [(vtab vt) #\vtab]
       [(page) #\page]
       [(alarm bel) #\u7]
       [(backspace) #\backspace]
       [(esc) #\u1b]
       [(delete) #\u7F]
       [(rubout) #\rubout]
       [(linefeed) #\linefeed]
       [(0 1 2 3 4 5 6 7 8 9)
        (integer->char (+ sym (char->integer #\0)))]
       [else
        (define str (symbol->string sym))
        (cond
          [(= 1 (string-length str))
           (string-ref str 0)]
          [(eqv? #\x (string-ref str 0))
           (integer->char (string->number (substring str 1) 16))]
          [else
           (error 'hash-backslash "unrecognized ~s" str)])])]
    [else (read-char in)]))

(define (hash-vee c in src line col pos)
  (case (read-char in)
    [(#\u)
     (unless (eqv? #\8 (read-char in)) (error 'hash-vee "not 8"))
     (define l (read/recursive in))
     (list->bytes l)]
    [(#\f)
     (define t (read-char in))
     (unless (or (eqv? #\x t) (eqv? #\l t)) (error 'hash-vee "not x or l"))
     (define l (read/recursive in))
     (if (eqv? #\x t)
         (apply fxvector l)
         (apply flvector l))]
    [else (error 'hash-vee "unexpected")]))

(define (as-symbol c in src line col pos)
  (string->symbol (string c)))

(define scheme-readtable
  (make-readtable
   #f
   #\0 'dispatch-macro hash-graph
   #\1 'dispatch-macro hash-one
   #\2 'dispatch-macro hash-two
   #\3 'dispatch-macro hash-three
   #\4 'dispatch-macro hash-graph
   #\5 'dispatch-macro hash-graph
   #\6 'dispatch-macro hash-graph
   #\7 'dispatch-macro hash-graph
   #\8 'dispatch-macro hash-graph
   #\9 'dispatch-macro hash-graph
   #\% 'dispatch-macro hash-percent
   #\! 'dispatch-macro hash-bang
   #\{ 'dispatch-macro hash-curly
   #\{ 'terminating-macro as-symbol
   #\} 'terminating-macro as-symbol
   #\[ 'terminating-macro (paren #\])
   #\( 'terminating-macro (paren #\))
   #\\ 'dispatch-macro hash-backslash
   #\v 'dispatch-macro hash-vee))
