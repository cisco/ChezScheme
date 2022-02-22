#lang racket/base
(require racket/match
         "scheme-struct.rkt"
         "scheme-readtable.rkt"
         "symbol.rkt")

(provide get-primdata
         (struct-out priminfo))

(struct priminfo (unprefixed libraries mask signatures arity))

;; Returns flags->bits for prim flags, `primvec` function, and `get-priminfo` function
(define (get-primdata $sputprop scheme-dir)
  (define flags->bits
    (cond
      [scheme-dir
       (call-with-input-file*
        (build-path scheme-dir "s/cmacros.ss")
        (lambda (i)
          (let loop ()
            (define l (parameterize ([current-readtable scheme-readtable])
                        (read i)))
            (match l
              [`(define-flags prim-mask ,specs ...)
               (define bits
                 (for/fold ([bits #hasheq()]) ([spec (in-list specs)])
                   (define (get-val v)
                     (if (number? v) v (hash-ref bits v)))
                   (match spec
                     [`(,name (or ,vals ...))
                      (hash-set bits name (apply bitwise-ior (map get-val vals)))]
                     [`(,name ,val)
                      (hash-set bits name (get-val val))])))
               (lambda (flags)
                 (apply bitwise-ior (for/list ([flag (in-list flags)])
                                      (hash-ref bits flag))))]
              [_ (loop)]))))]
      [else #hasheq()]))
  (define primref-variant
    (call-with-input-file*
     (build-path scheme-dir "s/primref.ss")
     (lambda (i)
       (define decl (parameterize ([current-readtable scheme-readtable])
                      (read i)))
       (match decl
         [`(define-record-type primref
             (nongenerative ,variant)
             . ,_)
          variant]
         [_
          (error "cannot parse content of s/primref.ss")]))))
  (define priminfos (make-hasheq))
  (when scheme-dir
    (call-with-input-file*
     (build-path scheme-dir "s/primdata.ss")
     (lambda (i)
       (let loop ()
         (define l (parameterize ([current-readtable #f])
                     (read i)))
         (unless (eof-object? l)
           (match l
             [`(,def-sym-flags
                ([libraries ,libs ...] [flags ,group-flags ...])
                ,clauses ...)
              (for ([clause (in-list clauses)])
                (match clause
                  [`(,id ,specs ...)
                   (define-values (flags sigs)
                     (for/fold ([flags group-flags] [sigs null]) ([spec (in-list specs)])
                       (match spec
                         [`[sig ,sigs ...] (values flags sigs)]
                         [`[pred ,pred] (values flags sigs)]
                         [`[flags ,flags ...] (values (append flags group-flags) sigs)]
                         [`[feature ,features ...] (values flags sigs)])))
                   (define plain-id (if (pair? id)
                                        (string->symbol (format "~a~a"
                                                                (car id)
                                                                (cadr id)))
                                        id))
                   (define flag-bits (flags->bits flags))
                   (define interface (map sig->interface sigs))
                   (define pr (case primref-variant
                                [(|{primref a0xltlrcpeygsahopkplcn-3}|)
                                 (primref3 plain-id flag-bits interface sigs)]
                                [(|{primref a0xltlrcpeygsahopkplcn-2}|)
                                 (primref2 plain-id flag-bits interface)]
                                [else (error "unrecognized primref variant in s/primref.ss"
                                             primref-variant)]))
                   (register-symbols plain-id)
                   ($sputprop plain-id '*prim2* pr)
                   ($sputprop plain-id '*prim3* pr)
                   ($sputprop plain-id '*flags* flag-bits)
                   (hash-set! priminfos plain-id (priminfo (if (pair? id) (cadr id) id)
                                                           libs
                                                           flag-bits
                                                           sigs
                                                           (map sig->interface sigs)))]))])
           (loop))))))
  (values (lambda () (list->vector (hash-keys priminfos)))
          (lambda (sym) (hash-ref priminfos sym #f))))

(define (sig->interface sig)
  (match sig
    [`((,args ... ,'...) ,ress ...)
     (- -1 (length args))]
    [`((,args ... ,'... ,last-arg) ,ress ...)
     (- -2 (length args))]
    [`((,args ...) ,ress ...)
     (length args)]))
