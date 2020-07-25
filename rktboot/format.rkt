#lang racket/base
(require "gensym.rkt")

(provide s:format
         s:printf
         s:fprintf
         s:error)

(define (s:format fmt . args)
  (define o (open-output-string))
  (do-printf o fmt args)
  (get-output-string o))

(define (s:printf fmt . args)
  (do-printf (current-output-port) fmt args))
  
(define (s:fprintf o fmt . args)
  (do-printf o fmt args))

(define (s:error sym fmt . args)
  (define o (open-output-string))
  (do-printf o fmt args)
  (error sym "~a" (get-output-string o)))
  
(define (do-printf o fmt args)
  (cond
    [(and (equal? fmt "~s")
          (not (print-gensym))
          (and (pair? args)
               (gensym? (car args))))
     (write-string (gensym->pretty-string (car args)) o)]
    [(and (let loop ([i 0])
            (cond
              [(= i (string-length fmt))
               #t]
              [(and (char=? #\~ (string-ref fmt i))
                    (< i (sub1 (string-length fmt))))
               (define c (string-ref fmt (add1 i)))
               (if (or (char=? c #\a)
                       (char=? c #\s)
                       (char=? c #\v)
                       (char=? c #\e))
                   (loop (+ i 2))
                   #f)]
              [else (loop (add1 i))]))
          (or (null? args)
              (not (bytes? (car args)))))
     (apply fprintf o fmt args)]
    [else
     ;; implement additional format functionality
     (let loop ([i 0] [args args] [mode '()])
       (cond
         [(= i (string-length fmt))
          (unless (null? args) (error 'format "leftover args"))]
         [(and (char=? #\~ (string-ref fmt i))
               (< i (sub1 (string-length fmt))))
          (define c (string-ref fmt (add1 i)))
          (case c
            [(#\a #\d)
             (define v (car args))
             (cond
               [(and (gensym? v)
                     (not (print-gensym)))
                (display (gensym->pretty-string v) o)]
               [(bytes? v)
                (begin
                   (write-bytes #"#vu8" o)
                   (display (bytes->list v) o))]
               [else
                (display (if (memq 'upcase mode)
                             (string-upcase v)
                             v)
                         o)])
             (loop (+ i 2) (cdr args) mode)]
            [(#\s #\v #\e)
             (define v (car args))
             (if (bytes? v)
                 (begin
                   (write-bytes #"#vu8" o)
                   (display (bytes->list v) o))
                 (write v o))
             (loop (+ i 2) (cdr args) mode)]
            [(#\x)
             (display (string-upcase (number->string (car args) 16)) o)
             (loop (+ i 2) (cdr args) mode)]
            [(#\: #\@)
             (case (string-ref fmt (+ i 2))
               [(#\[)
                (define (until i char print?)
                  (let loop ([i i])
                    (define c (string-ref fmt i))
                    (cond
                      [(and (char=? c #\~)
                            (char=? char (string-ref fmt (add1 i))))
                       (+ i 2)]
                      [print?
                       (write-char c o)
                       (loop (add1 i))]
                      [else (loop (add1 i))])))
                (define next-i (+ i 3))
                (case c
                  [(#\@)
                   (cond
                     [(car args)
                      (define-values (close-i rest-args) (loop next-i args mode))
                      (loop close-i rest-args mode)]
                     [else
                      (define close-i (until next-i #\] #f))
                      (loop close-i (cdr args) mode)])]
                  [else
                   (define sep-i (until next-i #\; (not (car args))))
                   (define close-i (until sep-i #\] (car args)))
                   (loop close-i (cdr args) mode)])]
               [(#\:)
                (case (string-ref fmt (+ i 3))
                  [(#\()
                   (define-values (close-i rest-args) (loop (+ i 4) args (cons 'upcase mode)))
                   (loop close-i rest-args mode)]
                  [else
                   (error "unexpected after @:" (string-ref fmt (+ i 3)))])]
               [else
                (error "unexpected after : or @" (string-ref fmt (+ i 2)))])]
            [(#\{)
             (define lst (car args))
             (cond
               [(null? lst)
                (let eloop ([i (+ i 2)])
                  (cond
                    [(and (char=? #\~ (string-ref fmt i))
                          (char=? #\} (string-ref fmt (add1 i))))
                     (loop (+ i 2) (cdr args) mode)]
                    [else (eloop (add1 i))]))]
               [else
                (define-values (next-i rest-args)
                  (for/fold ([next-i (+ i 2)] [args (append lst (cdr args))]) ([x (in-list lst)])
                    (loop (+ i 2) args mode)))
                (loop next-i rest-args mode)])]
            [(#\} #\] #\))
             ;; assume we're in a loop via `~{` or `~[` or `~(`
             (values (+ i 2) args)]
            [(#\?)
             (do-printf o (car args) (cadr args))
             (loop (+ i 2) (cddr args) mode)]
            [(#\%)
             (newline o)
             (loop (+ i 2) args mode)]
            [(#\^)
             (if (null? args)
                 (let eloop ([i (+ i 2)])
                   (cond
                     [(= i (string-length fmt))
                      (values i args)]
                     [(and (char=? #\~ (string-ref fmt i))
                           (char=? #\} (string-ref fmt (add1 i))))
                      (values (+ i 2) args)]
                     [else (eloop (add1 i))]))
                 (loop (+ i 2) args mode))]
            [else
             (error "unexpected" fmt)])]
         [else
          (write-char (string-ref fmt i) o)
          (loop (add1 i) args mode)]))]))
