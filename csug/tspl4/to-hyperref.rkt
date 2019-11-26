#lang racket/base

(for ([f (in-list (directory-list))]
      #:when (regexp-match? #rx"[.]aux" f))
  (printf "Convert ~s\n" f)
  (call-with-input-file*
   f
   (lambda (i)
     (with-output-to-file
       "tmp"
       #:exists 'truncate
       (lambda ()
         (for/fold ([sec #f]) ([line (in-lines i)])
           (cond
             [(regexp-match #rx"\\\\@writefile{toc}{\\\\contentsline {chapter}{(?:\\\\numberline {([^}]*)})?([^}]*)}"
                            line)
              => (lambda (m)
                   (displayln "match")
                   (displayln line)
                   (list (cadr m) (caddr m)))]
             [(regexp-match #rx"\\\\newlabel"
                            line)
              => (lambda (m)
                   (displayln (string-append
                               (substring line 0 (sub1 (string-length line)))
                               "{" (or (car sec) "") "}"
                               "{" (cadr sec) "}"
                               "{}"
                               "}"))
                   sec)]
             [else
              (displayln line)
              sec]))))))
  (rename-file-or-directory "tmp" f #t))

   
                          
                           
