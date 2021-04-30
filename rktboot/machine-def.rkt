#lang racket/base
(require (only-in racket/file file->string))

(provide open-file-with-machine.def-redirect)

(define (open-file-with-machine.def-redirect filename target-machine dir)
  (define (build-path* dir f) (if (eq? dir 'same) f (build-path dir f)))
  (cond 
    [(equal? filename "machine.def")
     (define def (string-append target-machine ".def"))
     (cond
       [(file-exists? (build-path* dir def)) (open-input-file (build-path* dir def))]
       [else
        ;; synthesize a default ".def" file from "[t]unix.def"
        (define def (if (regexp-match? #rx"^t" target-machine) "tunix.def" "unix.def"))
        (let* ([s (file->string (build-path* dir def))]
               [s (regexp-replace* #rx"[$][(]M[)]" s target-machine)]
               [s (regexp-replace* #rx"[$][(]March[)]" s
                                   (cond
                                     [(regexp-match? #rx"^t?a6" target-machine) "a6"]
                                     [(regexp-match? #rx"^t?i3" target-machine) "i3"]
                                     [(regexp-match? #rx"^t?arm32" target-machine) "arm32"]
                                     [(regexp-match? #rx"^t?arm64" target-machine) "arm64"]
                                     [(regexp-match? #rx"^t?ppc32" target-machine) "ppc32"]
                                     [else (error "machine.def: cannto infer architecture")]))])
          (open-input-string s))])]
    [else
     (open-input-file (build-path* dir filename))]))
