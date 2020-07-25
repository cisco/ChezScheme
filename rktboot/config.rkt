#lang racket/base
(require ffi/unsafe/global)

(provide scheme-dir
         target-machine
         optimize-level-init)

(define ht (get-place-table))

(define scheme-dir (or (hash-ref ht 'make-boot-scheme-dir #f)
                       (let ([scheme-dir
                              (getenv "SCHEME_SRC")])
                         (and scheme-dir
                              (simplify-path
                               (path->complete-path scheme-dir))))))
(hash-set! ht 'make-boot-scheme-dir scheme-dir)

(define target-machine (or (hash-ref ht 'make-boot-targate-machine #f)
                           (getenv "MACH")
                           (case (system-type)
                             [(macosx) (if (eqv? 64 (system-type 'word))
                                           "ta6osx"
                                           "ti3osx")]
                             [(windows) (if (eqv? 64 (system-type 'word))
                                           "ta6nt"
                                           "ti3nt")]
                             [else
                              (case (path->string (system-library-subpath #f))
                                [("x86_64-linux") "ta6le"]
                                [("i386-linux") "ti3le"]
                                [else #f])])))
(hash-set! ht 'make-boot-targate-machine target-machine)

(define optimize-level-init 3)
