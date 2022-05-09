#lang racket/base
(require racket/string)

(provide get-sources-from-makefile)

(define (get-sources-from-makefile scheme-dir)
  (call-with-input-file*
   (build-path scheme-dir "s" "build.zuo")
   #:mode 'text
   (lambda (i)
     (unless (equal? (read-line i) "#lang zuo")
       (error "expected `#lang zuo`from build.zuo"))
     (define content (let loop ()
                       (define v (read i))
                       (if (eof-object? v)
                           null
                           (cons v (loop)))))
     (define (extract-list id)
       (let loop ([c content])
         (cond
           [(and (list? c)
                 (= 3 (length c))
                 (eq? (car c) 'define)
                 (eq? (cadr c) id))
            (define v (caddr c))
            (cond
              [(and (list? v) (eq? 'list (car v)))
               (cdr v)]
              [else (error "definition did not have the expected right-hand side" v)])]
           [(list? c)
            (ormap loop c)]
           [else #f])))
     (define bases (extract-list 'base-src-names))
     (define compilers (extract-list 'compiler-names))
     (values bases compilers))))

     
