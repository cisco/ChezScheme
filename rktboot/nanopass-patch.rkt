#lang racket/base
(require (for-syntax racket/base))

;; To load the R6RS nanopass framework into Racket, we need to make
;; an adjustment to the use of `datum->syntax` in `make-in-context-transformer`.
;; This same adjustment appears in the Racket version of nanopass.

(provide patch:define)

(define-syntax (patch:define stx)
  (syntax-case stx (make-in-context-transformer lambda x quote)
    [(...
      (_ id
         (lambda args
           (lambda (x)
             (syntax-case x ()
               [(_ . pat-rest)
                (with-syntax ([qq (datum->syntax _ 'quasiquote)])
                  body)])))))
     (free-identifier=? #'id #'make-in-context-transformer)
     (begin
       (printf "Apply nanopass patch\n")
       #'(...
          (define id
            (lambda args
              (lambda (x)
                (syntax-case x ()
                  [(me . pat-rest)
                   (with-syntax ([qq (datum->syntax #'me 'quasiquote)])
                     body)]))))))]
    [(_ . rest) #'(define . rest)]))
