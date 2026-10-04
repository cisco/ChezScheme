(import (chezscheme))

(load "s/executable.ss")

(define launcher
  "/home/matthewmosior/Software/git/ChezScheme/ta6le/bin/ta6le/scheme")

(define petite-boot
  "/home/matthewmosior/Software/git/ChezScheme/ta6le/boot/ta6le/petite.boot")

(compile-executable
  "hello.ss"
  "hello"
  launcher
  petite-boot)

(display "built: hello")
(newline)

(display "intermediate object: hello.so")
(newline)

(display "intermediate base boot: hello.boot")
(newline)
