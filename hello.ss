;;; hello.ss
;;;
;;; Minimal self-contained Chez application.
;;;
;;; This file is compiled and then incorporated into the base boot.
;;; Loading the boot installs this procedure as the value of
;;; `scheme-start`.

(scheme-start
  (lambda args
    (display "Hello from embedded Chez Scheme!")
    (newline)

    (display "machine-type: ")
    (write (machine-type))
    (newline)

    (display "arguments: ")
    (write args)
    (newline)

    0))
