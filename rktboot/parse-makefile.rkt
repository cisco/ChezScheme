#lang racket/base
(require racket/string)

(provide get-sources-from-makefile)

(define (get-sources-from-makefile scheme-dir)
  (call-with-input-file*
   (build-path scheme-dir "s" "Mf-base")
   #:mode 'text
   (lambda (i)
     (define (extract-files m)
       (string-split (regexp-replace* #rx"\\\\" (bytes->string/utf-8 (cadr m)) "")))
     (define bases (extract-files (regexp-match #rx"basesrc =((?:[^\\\n]*\\\\\n)*[^\\\n]*)\n" i)))
     (define compilers (extract-files (regexp-match #rx"compilersrc =((?:[^\\\n]*\\\\\n)*[^\\\n]*)\n" i)))
     (values bases compilers))))

     
