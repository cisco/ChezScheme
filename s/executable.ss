;;; executable.ss
;;;
;;; Prototype support for producing a self-contained Chez Scheme
;;; executable with the layout:
;;;
;;;   [native Chez launcher]
;;;   [complete base boot]
;;;   [32-byte CHEZBOOT trailer]
;;;
;;; The native launcher is expected to contain the embedded-boot
;;; support in c/embedded-boot.c and c/main.c.
;;;
;;; The boot file supplied to make-executable must be a complete
;;; base boot file with no external boot dependencies.

(import (chezscheme))

;;; ==========================================================================
;;; Embedded-boot trailer
;;; ==========================================================================

;;; The trailer is exactly 32 bytes:
;;;
;;;   offset  size  field
;;;   ------  ----  ------------------------------------------
;;;      0      8   ASCII "CHEZBOOT"
;;;      8      4   uint32 little-endian format version
;;;     12      4   uint32 little-endian flags
;;;     16      8   uint64 little-endian boot length
;;;     24      8   uint64 little-endian reserved
;;;
;;; Version 1 requires:
;;;
;;;   version     = 1
;;;   flags       = 0
;;;   boot-length > 0
;;;   reserved    = 0

(define embedded-boot-trailer-size 32)

(define embedded-boot-version 1)

(define embedded-boot-magic
  #vu8(
    #x43                         ; C
    #x48                         ; H
    #x45                         ; E
    #x5a                         ; Z
    #x42                         ; B
    #x4f                         ; O
    #x4f                         ; O
    #x54))                       ; T

;;; ==========================================================================
;;; Little-endian integer encoding
;;; ==========================================================================

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
  (put-u8 op (bitwise-and n #xff))
  (put-u8 op
    (bitwise-and
      (bitwise-arithmetic-shift-right n 8)
      #xff))
  (put-u8 op
    (bitwise-and
      (bitwise-arithmetic-shift-right n 16)
      #xff))
  (put-u8 op
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

;;; ==========================================================================
;;; Little-endian integer decoding
;;;
;;; These are not needed by the native executable at runtime. They are here
;;; so that the Scheme side can independently verify the trailer format that
;;; it writes.
;;; ==========================================================================

(define (get-u32le bv offset)
  (bitwise-ior
    (bytevector-u8-ref bv offset)
    (bitwise-arithmetic-shift-left
      (bytevector-u8-ref bv (+ offset 1))
      8)
    (bitwise-arithmetic-shift-left
      (bytevector-u8-ref bv (+ offset 2))
      16)
    (bitwise-arithmetic-shift-left
      (bytevector-u8-ref bv (+ offset 3))
      24)))

(define (get-u64le bv offset)
  (let loop ([i 0]
             [n 0])
    (if (= i 8)
        n
        (loop
          (+ i 1)
          (bitwise-ior
            n
            (bitwise-arithmetic-shift-left
              (bytevector-u8-ref bv (+ offset i))
              (* i 8)))))))

;;; ==========================================================================
;;; Trailer writing
;;; ==========================================================================

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
  (put-bytevector op embedded-boot-magic)
  (put-u32le op embedded-boot-version)
  (put-u32le op 0)
  (put-u64le op boot-length)
  (put-u64le op 0))

;;; ==========================================================================
;;; Trailer parsing
;;; ==========================================================================

(define (embedded-boot-magic? bv)
  (and
    (= (bytevector-length bv)
       embedded-boot-trailer-size)
    (let loop ([i 0])
      (cond
        [(= i 8)
         #t]
        [(=
           (bytevector-u8-ref bv i)
           (bytevector-u8-ref embedded-boot-magic i))
         (loop (+ i 1))]
        [else
         #f]))))

;;; Returns the encoded boot length.
;;;
;;; Raises an exception if BV is not a valid v1 trailer.

(define (parse-embedded-boot-trailer bv)
  (unless
    (= (bytevector-length bv)
       embedded-boot-trailer-size)
    (error
      'parse-embedded-boot-trailer
      "trailer must contain exactly 32 bytes"
      (bytevector-length bv)))
  (unless
    (embedded-boot-magic? bv)
    (error
      'parse-embedded-boot-trailer
      "invalid embedded-boot magic"))
  (let ([version
         (get-u32le bv 8)]
        [flags
         (get-u32le bv 12)]
        [boot-length
         (get-u64le bv 16)]
        [reserved
         (get-u64le bv 24)])
    (unless (= version embedded-boot-version)
      (error
        'parse-embedded-boot-trailer
        "unsupported embedded-boot trailer version"
        version))
    (unless (= flags 0)
      (error
        'parse-embedded-boot-trailer
        "unsupported embedded-boot flags"
        flags))
    (unless (> boot-length 0)
      (error
        'parse-embedded-boot-trailer
        "embedded boot length must be greater than zero"))
    (unless (= reserved 0)
      (error
        'parse-embedded-boot-trailer
        "reserved embedded-boot trailer field is nonzero"
        reserved))
    boot-length))

;;; ==========================================================================
;;; Binary streaming
;;; ==========================================================================

;;; 64 KiB keeps copying inexpensive without allocating based on the size of
;;; the launcher or boot image.

(define binary-copy-buffer-size
  (* 64 1024))

;;; Copies IP to OP and returns the exact number of bytes copied.
;;;
;;; Both ports must be binary ports.

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
          ;; A regular file should not report a zero-byte read before EOF.
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

;;; ==========================================================================
;;; Packaging
;;; ==========================================================================

;;; make-executable
;;;
;;;   launcher : path to the modified generic Chez native executable
;;;   boot     : path to a COMPLETE BASE BOOT
;;;   output   : output executable pathname
;;;
;;; Produces:
;;;
;;;   launcher ++ boot ++ trailer
;;;
;;; Returns OUTPUT.
;;;
;;; The output inherits the launcher's mode bits so that executable
;;; permissions are retained on Unix-like systems.

(define (make-executable launcher boot output)
  (unless (string? launcher)
    (error
      'make-executable
      "launcher pathname is not a string"
      launcher))
  (unless (string? boot)
    (error
      'make-executable
      "boot pathname is not a string"
      boot))
  (unless (string? output)
    (error
      'make-executable
      "output pathname is not a string"
      output))
  (unless (file-exists? launcher)
    (error
      'make-executable
      "launcher does not exist"
      launcher))
  (unless (file-regular? launcher)
    (error
      'make-executable
      "launcher is not a regular file"
      launcher))
  (unless (file-exists? boot)
    (error
      'make-executable
      "boot file does not exist"
      boot))
  (unless (file-regular? boot)
    (error
      'make-executable
      "boot file is not a regular file"
      boot))

  ;; At least protect against the obvious destructive spellings
  ;;
  ;; A production implementation should canonicalize pathnames before
  ;; comparing them.
  (when (string=? launcher output)
    (error
      'make-executable
      "output pathname must differ from launcher pathname"
      output))
  (when (string=? boot output)
    (error
      'make-executable
      "output pathname must differ from boot pathname"
      output))
  (let ([launcher-mode
         (get-mode launcher)])
    (let ([launcher-ip #f]
          [boot-ip #f]
          [output-op #f]
          [result #f])
      (dynamic-wind
        ;; --------------------------------------------------------------
        ;; Open all three files.
        ;; --------------------------------------------------------------
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
          ;; REPLACE removes and recreates OUTPUT if it already exists.
          (set!
            output-op
            (open-file-output-port
              output
              (file-options replace)
              (buffer-mode block)
              #f)))
        ;; --------------------------------------------------------------
        ;; Write:
        ;;
        ;;   launcher
        ;;   boot
        ;;   trailer
        ;; --------------------------------------------------------------
        (lambda ()
          ;; Native launcher.
          (copy-binary-port
            launcher-ip
            output-op)
          ;; Complete base boot.
          ;;
          ;; We count exactly the bytes written here. That value is what
          ;; the native reader will later use to derive the region's offset.
          (let ([boot-length
                 (copy-binary-port
                   boot-ip
                   output-op)])
            (when (> boot-length #xffffffffffffffff)
              (error
                'make-executable
                "boot image is too large for the v1 trailer"
                boot-length))
            ;; Fixed 32-byte trailer.
            (write-embedded-boot-trailer
              output-op
              boot-length)
            (flush-output-port output-op)
            (set! result output)))
        ;; --------------------------------------------------------------
        ;; Close every port even when copying or trailer generation raises.
        ;; --------------------------------------------------------------
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
      ;; Preserve executable bits and all other launcher permissions.
      ;;
      ;; Chez documents get-mode/chmod in the same numeric format. Under
      ;; Windows, permission bits without a Windows counterpart are ignored.
      (chmod output launcher-mode)
      result)))

;;; ==========================================================================
;;; Compilation + packaging
;;; ==========================================================================

;;; compile-executable
;;;
;;;   source       : application Scheme source
;;;   output       : final native executable
;;;   launcher     : modified generic Chez launcher
;;;   petite-boot  : petite.boot for the launcher's machine type
;;;
;;; SOURCE is expected to install its entry point through `scheme-start`.
;;;
;;; For this initial prototype, two intermediate files are deliberately kept:
;;;
;;;   OUTPUT.so
;;;   OUTPUT.boot
;;;
;;; Keeping them makes it easy to inspect and independently test both stages.

(define (compile-executable source output launcher petite-boot)
  (unless (string? source)
    (error
      'compile-executable
      "source pathname is not a string"
      source))
  (unless (string? output)
    (error
      'compile-executable
      "output pathname is not a string"
      output))
  (unless (string? launcher)
    (error
      'compile-executable
      "launcher pathname is not a string"
      launcher))
  (unless (string? petite-boot)
    (error
      'compile-executable
      "petite boot pathname is not a string"
      petite-boot))
  (unless (file-exists? source)
    (error
      'compile-executable
      "source file does not exist"
      source))
  (unless (file-exists? launcher)
    (error
      'compile-executable
      "launcher does not exist"
      launcher))
  (unless (file-exists? petite-boot)
    (error
      'compile-executable
      "petite boot file does not exist"
      petite-boot))
  (let ([object-file
         (string-append output ".so")]
        [boot-file
         (string-append output ".boot")])
    ;; ---------------------------------------------------------------
    ;; 1. Compile the application's Scheme code.
    ;; ---------------------------------------------------------------
    (compile-file
      source
      object-file)
    ;; ---------------------------------------------------------------
    ;; 2. Construct a COMPLETE BASE BOOT.
    ;;
    ;; The dependency list MUST be empty.
    ;;
    ;; petite.boot MUST be first among the inputs. Chez documents this
    ;; as the mechanism for constructing a base boot file.
    ;; ---------------------------------------------------------------
    (make-boot-file
      boot-file
      '()
      petite-boot
      object-file)
    ;; ---------------------------------------------------------------
    ;; 3. Package launcher + base boot + trailer.
    ;; ---------------------------------------------------------------
    (make-executable
      launcher
      boot-file
      output)))
