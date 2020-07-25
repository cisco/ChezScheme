#lang racket/base
(require racket/cmdline
         racket/runtime-path)

;; Wrapper around "make-boot.rkt" to make it work in a more normal way
;; with command-line arguments, instead of environment variables.

(define scheme-src #f)
(define mach #f)

(command-line
 #:once-each
 [("--scheme-src") dir "Select the directory (defaults to current directory)"
                   (set! scheme-src dir)]
 [("--machine") machine "Select the machine type (defaults to inferred)"
                (set! mach machine)])

(unless scheme-src
  (printf "Assuming current directory has Chez Scheme sources\n")
  (flush-output))

(void (putenv "SCHEME_SRC" (or scheme-src ".")))
(when mach
  (void (putenv "MACH" mach)))

;; Dynamic, so that environment variables are visible to
;; compile-time instantiation of `make-boot`:
(define-runtime-path make-boot "make-boot.rkt")
(dynamic-require make-boot #f)
