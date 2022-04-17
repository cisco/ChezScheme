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
        ;; synthesize a default ".def" file from "[t]unix.def" or "[t]pbarch.def"
        (define def (string-append
                     (if (regexp-match? #rx"^t" target-machine) "t" "")
                     (if (regexp-match? #rx"pb" target-machine) "pbarch" "unix")
                     ".def"))
        (let* ([s (file->string (build-path* dir def))]
               [s (regexp-replace* #rx"[$][(]M[)]" s target-machine)]
               [s (regexp-replace* #rx"[$][(]March[)]" s
                                   (cond
                                     [(regexp-match? #rx"^t?a6" target-machine) "a6"]
                                     [(regexp-match? #rx"^t?i3" target-machine) "i3"]
                                     [(regexp-match? #rx"^t?arm32" target-machine) "arm32"]
                                     [(regexp-match? #rx"^t?arm64" target-machine) "arm64"]
                                     [(regexp-match? #rx"^t?ppc32" target-machine) "ppc32"]
                                     [(regexp-match? #rx"^t?pb" target-machine) "pb"]
                                     [else (error "machine.def: cannot infer architecture")]))]
               [s (regexp-replace* #rx"[$][(]Mend[)]" s
                                   (cond
                                     [(regexp-match? #rx"l$" target-machine) "little"]
                                     [(regexp-match? #rx"b$" target-machine) "big"]
                                     [else "?"]))]
               [s (regexp-replace* #rx"[$][(]Mword[)]" s
                                       (cond
                                         [(regexp-match? #rx"32" target-machine) "32"]
                                         [(regexp-match? #rx"64" target-machine) "64"]
                                         [else "?"]))]
               [s (regexp-replace* #rx"[$][(]Mtimet[)]" s
                                       (cond
                                         [(regexp-match? #rx"nb$" target-machine) "64"]
                                         [(regexp-match? #rx"ob$" target-machine) "64"]
                                         [(regexp-match? #rx"i3" target-machine) "32"]
                                         [(regexp-match? #rx"ppc32" target-machine) "32"]
                                         [(regexp-match? #rx"arm32" target-machine) "32"]
                                         [else "64"]))])
          (open-input-string s))])]
    [else
     (open-input-file (build-path* dir filename))]))
