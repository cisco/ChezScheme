;;; executable.ss
;;;
;;; Support for producing self-contained Chez Scheme executables.
;;;
;;; Executable layout:
;;;
;;;   [native Chez launcher]
;;;   [complete base boot]
;;;   [32-byte CHEZBOOT trailer]

(begin
  (let ()
    ;; ------------------------------------------------------------------------
    ;; Trailer constants
    ;; ------------------------------------------------------------------------
    (define embedded-boot-trailer-size 32)
    (define embedded-boot-version 1)
    (define embedded-boot-magic
      #vu8(
        #x43   ; C
        #x48   ; H
        #x45   ; E
        #x5a   ; Z
        #x42   ; B
        #x4f   ; O
        #x4f   ; O
        #x54)) ; T

    ;; ------------------------------------------------------------------------
    ;; Integer encoding
    ;; ------------------------------------------------------------------------

    (define (put-u32le op n)
      (unless
        (and
          (integer? n)
          (exact? n)
          (<= 0 n #xffffffff))
        (error
          'put-u32le
          "value is outside the uint32 range"
          n))
      (put-u8
        op
        (bitwise-and n #xff))
      (put-u8
        op
        (bitwise-and
          (bitwise-arithmetic-shift-right n 8)
          #xff))
      (put-u8
        op
        (bitwise-and
          (bitwise-arithmetic-shift-right n 16)
          #xff))
      (put-u8
        op
        (bitwise-and
          (bitwise-arithmetic-shift-right n 24)
          #xff)))

    (define (put-u64le op n)
      (unless
        (and
          (integer? n)
          (exact? n)
          (<= 0 n #xffffffffffffffff))
        (error
          'put-u64le
          "value is outside the uint64 range"
          n))
      (let loop ([shift 0])
        (unless (= shift 64)
          (put-u8
            op
            (bitwise-and
              (bitwise-arithmetic-shift-right n shift)
              #xff))

          (loop (+ shift 8)))))

    ;; ------------------------------------------------------------------------
    ;; Trailer
    ;; ------------------------------------------------------------------------

    (define (write-embedded-boot-trailer op boot-length)
      (unless
        (and
          (integer? boot-length)
          (exact? boot-length)
          (< 0 boot-length)
          (<= boot-length #xffffffffffffffff))
        (error
          'write-embedded-boot-trailer
          "invalid embedded boot length"
          boot-length))
      ;; 0..7
      (put-bytevector
        op
        embedded-boot-magic)
      ;; 8..11
      (put-u32le
        op
        embedded-boot-version)
      ;; 12..15: flags
      (put-u32le
        op
        0)
      ;; 16..23
      (put-u64le
        op
        boot-length)
      ;; 24..31: reserved
      (put-u64le
        op
        0))

    ;; ------------------------------------------------------------------------
    ;; Streaming copy
    ;; ------------------------------------------------------------------------

    (define binary-copy-buffer-size
      (* 64 1024))

    (define (copy-binary-port ip op)
      (let ([buffer
             (make-bytevector binary-copy-buffer-size)])
        (let loop ([total 0])
          (let ([n
                 (get-bytevector-n!
                   ip
                   buffer
                   0
                   binary-copy-buffer-size)])
            (cond
              [(eof-object? n)
               total]
              [(zero? n)
               (error
                 'copy-binary-port
                 "input port returned zero bytes before end of file")]
              [else
               (put-bytevector
                 op
                 buffer
                 0
                 n)
               (loop (+ total n))])))))

    ;; ------------------------------------------------------------------------
    ;; Temporary output handling
    ;; ------------------------------------------------------------------------

    (define (temporary-output-path output)
      ;; Same directory as OUTPUT so the final rename is within the
      ;; same filesystem.
      (string-append output ".tmp"))

    (define (delete-file-if-exists path)
      (when (file-exists? path)
        (delete-file path)))

    ;; ------------------------------------------------------------------------
    ;; Current executable path
    ;; ------------------------------------------------------------------------

    (define current-executable-path
      (let ([get-process-executable-path
             (foreign-procedure
               "(cs)process_executable_path"
               ()
               scheme-object)])
        (lambda ()
          (let ([path
                 (get-process-executable-path)])
            (unless (string? path)
              (error
                'compile-executable
                "cannot determine current Chez executable pathname"))
            path))))

    ;; ------------------------------------------------------------------------
    ;; Matching petite.boot discovery
    ;; ------------------------------------------------------------------------

    (define current-petite-boot
      (let ([find-petite-boot
             (foreign-procedure
               "(cs)petite_boot_path"
               ()
               scheme-object)])
        (lambda ()
          (let ([path
                 (find-petite-boot)])
            (unless (string? path)
              (error
                'compile-executable
                "cannot find a compatible petite.boot"))
            path))))

    ;; ------------------------------------------------------------------------
    ;; Internal explicit compile-executable implementation
    ;; ------------------------------------------------------------------------

    (define (compile-executable/paths
              source
              output
              launcher
              petite-boot)
      (unless (string? source)
        ($oops
          'compile-executable
          "~s is not a pathname"
          source))
      (unless (string? output)
        ($oops
          'compile-executable
          "~s is not a pathname"
          output))
      (unless (string? launcher)
        ($oops
          'compile-executable
          "~s is not a pathname"
          launcher))
      (unless (string? petite-boot)
        ($oops
          'compile-executable
          "~s is not a pathname"
          petite-boot))
      (unless (file-exists? source)
        (error
          'compile-executable
          "source file does not exist"
          source))
      (unless (file-regular? source)
        (error
          'compile-executable
          "source file is not a regular file"
          source))
      (unless (file-exists? launcher)
        (error
          'compile-executable
          "launcher does not exist"
          launcher))
      (unless (file-regular? launcher)
        (error
          'compile-executable
          "launcher is not a regular file"
          launcher))
      (unless (file-exists? petite-boot)
        (error
          'compile-executable
          "petite boot file does not exist"
          petite-boot))
      (unless (file-regular? petite-boot)
        (error
          'compile-executable
          "petite boot file is not a regular file"
          petite-boot))
      ;; Validate the application contract before generating or compiling
      ;; any intermediate executable files.
      (validate-executable-main source)
      (let* ([bootstrap-source
              (string-append output ".bootstrap.ss")]
             [bootstrap-object
              (string-append output ".bootstrap.so")]
             [bootstrap-wpo
              (string-append output ".bootstrap.wpo")]
             [whole-object
              (string-append output ".whole.so")]
             [boot-file
              (string-append output ".boot")]
             [intermediates
              (list
                bootstrap-source
                bootstrap-object
                bootstrap-wpo
                whole-object
                boot-file)])
        (dynamic-wind
          ;; ----------------------------------------------------------------------
          ;; Before build.
          ;;
          ;; Remove leftovers from a previously interrupted build.
          ;; ----------------------------------------------------------------------
          (lambda ()
            (cleanup-executable-intermediates
              intermediates))
          ;; ----------------------------------------------------------------------
          ;; Build.
          ;; ----------------------------------------------------------------------
          (lambda ()
            ;; Stage 1:
            ;;
            ;; Generate the top-level executable program, compile it, and
            ;; produce its WPO representation.
            (compile-executable-bootstrap
              source
              bootstrap-source
              bootstrap-object
              bootstrap-wpo)
            ;; Stage 2:
            ;;
            ;; Optimize the complete application into one whole-program
            ;; object.
            (compile-executable-whole-program
              bootstrap-wpo
              whole-object)
            ;; Stage 3:
            ;;
            ;; Construct the complete base boot.
            (make-boot-file
              boot-file
              '()
              petite-boot
              whole-object)
            ;; Stage 4:
            ;;
            ;; Atomically package the native launcher and complete base boot.
            ;;
            ;; make-executable returns OUTPUT.
            (make-executable
              launcher
              boot-file
              output))
          ;; ----------------------------------------------------------------------
          ;; After build.
          ;;
          ;; Runs on both normal return and exceptional exit.
          ;; ----------------------------------------------------------------------
          (lambda ()
            (cleanup-executable-intermediates
              intermediates)))))

    ;; ------------------------------------------------------------------------
    ;; Generated application bootstrap
    ;; ------------------------------------------------------------------------

    (define (write-executable-bootstrap path source)
      (let ([op
             (open-file-output-port
               path
               (file-options replace)
               (buffer-mode block)
               (native-transcoder))])
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            ;; This is a real Chez/R6RS top-level program.
            (display
              "(import (chezscheme))\n\n"
              op)
            ;; Put the user's definitions and generated startup code in
            ;; one lexical unit.
            (display
              "(let ()\n"
              op)
            (display
              "  "
              op)
            (write
              `(include ,source)
              op)
            (newline op)
            (newline op)
            (display
              "  (suppress-greeting #t)\n\n"
              op)
            (display
              "  (scheme-start\n"
              op)
            (display
              "    (lambda (program-name . args)\n"
              op)
            (display
              "      (command-line (cons program-name args))\n"
              op)
            (display
              "      (command-line-arguments args)\n"
              op)
            (display
              "      (main args)))\n"
              op)
            (display
              ")\n"
              op)
            (flush-output-port op))
          (lambda ()
            (close-port op)))))

    (define (compile-executable-bootstrap
              source
              bootstrap-source
              bootstrap-object
              bootstrap-wpo)
      (write-executable-bootstrap
        bootstrap-source
        source)
      ;; compile-program is important here: compile-whole-program expects
      ;; the WPO file for a top-level program.
      (parameterize ([generate-wpo-files #t])
        (compile-program
          bootstrap-source
          bootstrap-object))
      ;; generate-wpo-files derives the .wpo filename from the object
      ;; pathname. Verify that the expected artifact was produced.
      (unless (file-exists? bootstrap-wpo)
        (error
          'compile-executable
          "compiler did not produce expected WPO file"
          bootstrap-wpo)))

    ;; ------------------------------------------------------------------------
    ;; Executable entry-point validation
    ;; ------------------------------------------------------------------------

    ;; True for a proper one-element list whose element is a symbol.
    ;;
    ;; This is the shape required for the executable entry point:
    ;;
    ;;   (main args)
    ;;
    (define (single-symbol-formals? formals)
      (and
        (pair? formals)
        (symbol? (car formals))
        (null? (cdr formals))))

    ;; Classify a single top-level form with respect to the executable
    ;; entry-point contract.
    ;;
    ;; Results:
    ;;
    ;;   'none
    ;;     The form does not define `main`.
    ;;
    ;;   'valid
    ;;     The form defines `main` as a procedure accepting exactly
    ;;     one fixed argument.
    ;;
    ;;   'invalid
    ;;     The form attempts to define `main`, but does not satisfy
    ;;     the executable entry-point contract.
    ;;
    (define (main-definition-status form)
      (cond
        [(and
           (pair? form)
           (eq? (car form) 'define)
           (pair? (cdr form)))
         (let ([lhs
                (cadr form)])
           (cond
             ;; Procedure-definition form:
             ;;
             ;;   (define (main args)
             ;;     ...)
             ;;
             [(and
                (pair? lhs)
                (eq? (car lhs) 'main))

              (if (single-symbol-formals? (cdr lhs))
                  'valid
                  'invalid)]
             ;; Variable-definition form:
             ;;
             ;;   (define main
             ;;     (lambda (args)
             ;;       ...))
             ;;
             [(eq? lhs 'main)
              (if
                (and
                  (pair? (cddr form))
                  (null? (cdddr form))
                  (let ([rhs
                         (caddr form)])
                    (and
                      (pair? rhs)
                      (eq? (car rhs) 'lambda)
                      (pair? (cdr rhs))
                      (single-symbol-formals?
                        (cadr rhs)))))
                'valid
                'invalid)]
             [else
              'none]))]
        [else
         'none]))

    ;; Validate one form, recursively looking through top-level BEGIN
    ;; forms.
    ;;
    ;; Returns the number of valid `main` definitions found.
    (define (count-main-definitions form source)
      (cond
        ;; Treat top-level BEGIN as a sequence of top-level forms.
        [(and
           (pair? form)
           (eq? (car form) 'begin))
         (let loop ([forms (cdr form)]
                    [count 0])
           (if (null? forms)
               count
               (loop
                 (cdr forms)
                 (+ count
                    (count-main-definitions
                      (car forms)
                      source)))))]
        [else
         (case (main-definition-status form)
           [(none)
            0]
           [(valid)
            1]
           [(invalid)
            (error
              'compile-executable
              "invalid executable entry point; expected (define (main args) ...)"
              source)]
           [else
            0])]))

    ;; Read SOURCE without evaluating it and verify the executable
    ;; entry-point contract.
    ;;
    ;; Exactly one valid top-level `main` definition is required.
    (define (validate-executable-main source)
      (let ([ip
             (open-file-input-port
               source
               (file-options)
               (buffer-mode block)
               (native-transcoder))])
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (let loop ([count 0])
              (let ([form
                     (get-datum ip)])
                (if (eof-object? form)
                    (cond
                      [(zero? count)
                       (error
                         'compile-executable
                         "executable source does not define main; expected (define (main args) ...)"
                         source)]
                      [(> count 1)
                       (error
                         'compile-executable
                         "executable source defines main more than once"
                         source)]
                      [else
                       (void)])
                    (loop
                      (+ count
                         (count-main-definitions
                           form
                           source)))))))
          (lambda ()
            (close-port ip)))))

    (define (compile-executable-whole-program
              wpo-file
              output-file)
      (let ([external-libraries
             (compile-whole-program
               wpo-file
               output-file
               #f)])
        ;; For compile-executable, silently leaving libraries to be loaded
        ;; from disk would violate the self-contained executable contract.
        (unless (null? external-libraries)
          (error
            'compile-executable
            "whole-program compilation left libraries to be loaded at runtime"
            external-libraries))
        output-file))

    ;; ------------------------------------------------------------------------
    ;; Intermediate artifact cleanup
    ;; ------------------------------------------------------------------------

    (define (cleanup-executable-intermediates paths)
      (for-each
        (lambda (path)
          (when (file-exists? path)
            (guard (condition
                    [else
                     (void)])
              (delete-file path))))
        paths))

    ;; ------------------------------------------------------------------------
    ;; make-executable
    ;; ------------------------------------------------------------------------

    (set-who! make-executable
      (lambda (launcher boot output)
        (unless (string? launcher)
          ($oops
            who
            "~s is not a pathname"
            launcher))
        (unless (string? boot)
          ($oops
            who
            "~s is not a pathname"
            boot))
        (unless (string? output)
          ($oops
            who
            "~s is not a pathname"
            output))
        (unless (file-exists? launcher)
          (error
            who
            "launcher does not exist"
            launcher))
        (unless (file-regular? launcher)
          (error
            who
            "launcher is not a regular file"
            launcher))
        (unless (file-exists? boot)
          (error
            who
            "boot file does not exist"
            boot))
        (unless (file-regular? boot)
          (error
            who
            "boot file is not a regular file"
            boot))
        (when (string=? launcher output)
          (error
            who
            "output pathname must differ from launcher pathname"
            output))
        (when (string=? boot output)
          (error
            who
            "output pathname must differ from boot pathname"
            output))
        (let* ([launcher-mode
                (get-mode launcher)]
               [temporary
                (temporary-output-path output)]
               [launcher-ip #f]
               [boot-ip #f]
               [output-op #f]
               [committed? #f])
          ;; Remove a stale incomplete build, never the real OUTPUT.
          (delete-file-if-exists temporary)
          (guard
            (condition
              [else
               (unless committed?
                 (delete-file-if-exists temporary))
               (raise condition)])
            ;; --------------------------------------------------------------
            ;; Build the entire file into OUTPUT.tmp.
            ;; --------------------------------------------------------------
            (dynamic-wind
              (lambda ()
                (set!
                  launcher-ip
                  (open-file-input-port
                    launcher
                    (file-options)
                    (buffer-mode block)
                    #f))
                (set!
                  boot-ip
                  (open-file-input-port
                    boot
                    (file-options)
                    (buffer-mode block)
                    #f))
                (set!
                  output-op
                  (open-file-output-port
                    temporary
                    (file-options)
                    (buffer-mode block)
                    #f)))
              (lambda ()
                ;; Native launcher.
                (copy-binary-port
                  launcher-ip
                  output-op)
                ;; Base boot.
                (let ([boot-length
                       (copy-binary-port
                         boot-ip
                         output-op)])
                  (when (> boot-length #xffffffffffffffff)
                    (error
                      who
                      "boot image is too large for the v1 trailer"
                      boot-length))
                  ;; Trailer.
                  (write-embedded-boot-trailer
                    output-op
                    boot-length))
                (flush-output-port output-op))
              (lambda ()
                (when launcher-ip
                  (close-port launcher-ip)
                  (set! launcher-ip #f))
                (when boot-ip
                  (close-port boot-ip)
                  (set! boot-ip #f))
                (when output-op
                  (close-port output-op)
                  (set! output-op #f))))
            ;; --------------------------------------------------------------
            ;; Commit only after the complete file has been closed.
            ;; --------------------------------------------------------------
            (chmod
              temporary
              launcher-mode)
            (rename-file
              temporary
              output)
            (set! committed? #t)
            output))))

    ;; ------------------------------------------------------------------------
    ;; Public compile-executable API
    ;; ------------------------------------------------------------------------

    (set-who! compile-executable
      (lambda (source output)
        (unless (string? source)
          ($oops
            who
            "~s is not a pathname"
            source))
        (unless (string? output)
          ($oops
            who
            "~s is not a pathname"
            output))
        (let* ([launcher
                (current-executable-path)]
               [petite-boot
                (current-petite-boot)])
          (compile-executable/paths
            source
            output
            launcher
            petite-boot))))
    ))
