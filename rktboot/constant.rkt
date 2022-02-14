#lang racket/base
(require racket/match
         "scheme-readtable.rkt"
         "config.rkt"
         "machine-def.rkt")

;; Extract constants that we need to get started by reading
;; "cmacros.ss" and the machine ".def" file (without trying to run or
;; expand the files)

(define ht (make-hasheq))

(define (read-constants i)
  (parameterize ([current-readtable scheme-readtable])
    (let loop ()
      (define e (read i))
      (unless (eof-object? e)
        (match e
          [`(define-constant ,id2 (case (constant ,id1)
                                    [(,v1) ,rv1]
                                    [(,v2) ,rv2]
                                    . ,_))
           (define v (hash-ref ht id1))
           (hash-set! ht id2
                      (cond
                        [(eqv? v v1) rv1]
                        [(eqv? v v2) rv2]
                        [else (error "unknown")]))]
          [`(define-constant ,id ,e)
           (let/cc esc
             (hash-set! ht id (constant-eval e esc)))]
          [`(define-constant-default ,id ,e)
           (hash-ref ht id
                     (lambda ()
                       (let/cc esc
                         (hash-set! ht id (constant-eval e esc)))))]
          [`(include ,fn)
           (unless (equal? fn "machine.def")
             (read-constants-from-file fn))]
          [`(define-machine-types . ,ms)
           (hash-set! ht 'machine-type-alist
                      (let ([target (string->symbol target-machine)])
                        (for/list ([m (in-list ms)]
                                   [i (in-naturals)])
                          (when (eq? m target)
                            (hash-set! ht 'machine-type i))
                          (cons i m))))]
          [_ (void)])
        (loop)))))

(define (constant-eval e esc)
  (cond
    [(pair? e)
     (case (car e)
       [(if)
        (if (constant-eval (cadr e) esc)
            (constant-eval (caddr e) esc)
            (constant-eval (cadddr e) esc))]
       [(constant)
        (hash-ref ht (cadr e) esc)]
       [(=)
        (= (constant-eval (cadr e) ht)
           (constant-eval (caddr e) ht))]
       [(fx- -)
        (apply - (map (lambda (e) (constant-eval e esc)) (cdr e)))]
       [(fx+ +)
        (apply + (map (lambda (e) (constant-eval e esc)) (cdr e)))]
       [(quote)
        (cadr e)]
       [(cdr)
        (cdr (constant-eval (cadr e) esc))]
       [(assv)
        (assv (constant-eval (cadr e) esc)
              (constant-eval (caddr e) esc))]
       [else (esc)])]
    [else e]))

(define (read-constants-from-file fn)
  (define i (open-file-with-machine.def-redirect fn target-machine (build-path scheme-dir "s")))
  (begin0
    (read-constants i)
    (close-input-port i)))

(when scheme-dir
  (read-constants-from-file "machine.def")
  (read-constants-from-file "cmacros.ss"))

(define-syntax-rule (define-constant id ...)
  (begin
    (provide id ...)
    (define id (hash-ref ht 'id #f)) ...))

(hash-set! ht 'ptr-bytes (/ (hash-ref ht 'ptr-bits 64) 8))

(define-constant
  ptr-bytes
  fixnum-bits
  max-float-alignment
  annotation-debug
  annotation-profile
  visit-tag
  revisit-tag
  prelex-is-flags-offset
  prelex-was-flags-offset
  prelex-sticky-mask
  prelex-is-mask
  scheme-version
  code-flag-lift-barrier
  record-ptr-offset
  machine-type-name
  architecture)
