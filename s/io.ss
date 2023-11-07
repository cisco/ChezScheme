;;; io.ss
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; possible extensions:
;;;  - mechanism for overriding default #o666 mode
;;;  - user-defined handler records
;;;    - say user-supplied handler procedures "should" return appropriate
;;;      values (e.g., octet/eof for get on binary port), wrap procedures
;;;      in return-value checkers, or allow user to choose whether
;;;      procedures are wrapped in return-value checkers

;;; r6rs custom ports are fubar:
;;;  - binary and textual output ports: no known problems
;;;  - binary input ports: no problem except just after a
;;;    lookahead-u8 returns #!eof or just after unget-u8 of #!eof,
;;;    at which point port position is ill-defined.
;;;  - binary input/output ports: can't work without working
;;;    get-position and set-position! procedures to switch between
;;;    input and output mode
;;;  - textual input ports: no way to implement port-position,
;;;    since get-position returns an arbitrary object, no way to adjust for
;;;    amount we've buffered, and we must buffer at least one character to
;;;    support lookahead-char.  also same problem as custom binary input
;;;    ports with #!eof.
;;;  - textual input/output ports: no way to switch between input
;;;    and output modes, since we cannot implement port-position.
;;;
;;; all problems derive from need to buffer at least one element to
;;; support lookahead-u8 and lookahead-char.
;;;
;;; our workarounds:
;;;  - custom binary and textual output ports:
;;;    - none
;;;  - custom binary input ports:
;;;    - treat eof as zero width
;;;    - assume sequential indices from get-position to compute port-position
;;;      with adjustment for buffered characters
;;;  - custom textual input ports:
;;;    - treat eof as zero width
;;;    - port-position undefined after read
;;;    - no warning for port-position if:
;;;      - no reads (including lookahead and port-eof?) have been done
;;;      - a set-port-position! occurred after last read
;;;      - buffer-mode is none and last read operation was not a lookahead,
;;;        port-eof?, or unget
;;;  - custom binary or textual input/output ports:
;;;    - position for write undefined after read
;;;    - port-position undefined after read
;;;    - no warning for write or port-position if:
;;;      - no reads (including lookahead and port-eof?) have been done
;;;      - a write or set-port-position occurred after last read
;;;      - buffer-mode is none and last read operation was not a lookahead,
;;;        port-eof?, or unget (efficient input can be had with buffer-mode
;;;        none if only get-bytevector operations are used.  sequence of
;;;        gets will relatively slow with buffer-mode none.)
;;;    - exception: we use supplied get-position and
;;;      set-position! on a custom binary input/output port to sync
;;;      position and avoid issuing warnings under assumption that
;;;      get-position indices are sequential

#|
implementation notes:
  - for binary input/output file ports, we can always distinguish input
    mode from output mode by the fact that output-size is zero iff port is
    in input mode.  this does not work for textual ports, because
    output-size can be zero even in output mode for line-buffered ports.
    so we instead use an input-mode flag in the port header.
|#

(let-syntax ([make-positive-fixnum-thread-parameter
              ;; duplicate code so we get the right procedure-name
              (syntax-rules ()
                [(_ who default)
                 ($make-thread-parameter default
                   (lambda (x)
                     (unless (and (fixnum? x) (fx> x 0))
                       ($oops who "~s is not a positive fixnum" x))
                     x))])])
(set-who! file-buffer-size
  (make-positive-fixnum-thread-parameter who $c-bufsiz))

(set-who! custom-port-buffer-size
  (make-positive-fixnum-thread-parameter who 128))

(set-who! transcoded-port-buffer-size
  (make-positive-fixnum-thread-parameter who 1024))

(set-who! make-codec-buffer
  ($make-thread-parameter (lambda (bp) (make-bytevector 1024)) ;; original hardcoded value
    (lambda (x)
      (unless (procedure? x)
        ($oops who "~s is not a procedure" x))
      x)))

(let ()
  (define min-codec-buffer-size
    ;; From the Unicode Standard, Version 15.0 - Core Specification, Section 2.5:
    ;;  "The Unicode Standard provides three distinct encoding forms for Unicode
    ;;   characters, using 8-bit, 16-bit, and 32-bit units. These are named UTF-8,
    ;;   UTF-16, and UTF-32, respectively."
    ;;
    ;; | encoding | code-point representation |
    ;; |----------+---------------------------|
    ;; | UTF-8    | one to four bytes         |
    ;; | UTF-16   | one or two 16-bit values  |
    ;; | UTF-32   | a single 32-bit value     |
    4)

 ; choose whether to issue warnings when custom-port implementation
 ; cannot determine position for port-position or write operation
  #;(define position-warning warning)
  (define (position-warning who msg . args) (void))

  (include "io-types.ss")

  (define-syntax call-port-handler
    (lambda (x)
      (syntax-case x ()
        [(_ msg who ?p arg ...)
         (identifier? #'msg)
         (with-syntax ([port-handler-msg (construct-name #'msg "port-handler-" #'msg)])
           #'(let ([p ?p]) ((port-handler-msg ($port-handler p)) who p arg ...)))])))

  (define-port-handler (codec make-codec codec?) #f
    (name -> string)
    (make-info who tx bp tp-buf-len bv -> codec-info))

 ; ioffsets is an fxvector mapping character positions in a port's input buffer
 ; to byte offsets from the starting byte position for the buffer.  ibytes
 ; is the byte offset of the character just beyond the end of the buffer,
 ; which is also the length in bytes of the data represented by the characters
 ; in the buffer.  ioffsets and ibytes together allow port positions to be
 ; reported in bytes.  ioffsets and ibytes are not consulted when a port's
 ; input buffer is empty, so there is no harm in modifying them when reading
 ; into a different string.  since ioffsets might not have as many elements
 ; as the different string, however, a codec should usually avoid modifying
 ; ioffsets to prevent writes beyond the end of the vector.  a codec's encode
 ; procedure is always called with start = 0 when string to fill is the port's
 ; input buffer, so ibytes should also start at 0.

  (define-record-type codec-info
    (nongenerative)
    (opaque #t)
    (fields
      (immutable tx)           ; transcoder
      (mutable bp)             ; binary port (clone)
      (immutable bv)           ; bytevector buffer (input or output, one at a time)
      (mutable next)           ; next pointer into buffer
      (mutable iend)           ; end of data (input only)
      (immutable ioffsets)     ; byte offset each char in port's buffer, relative to first (input only)
      (mutable ibytes)         ; byte offset of first char beyond port's buffer (input only)
      (mutable icr)            ; #\return seen when eol style is not none (input only)
      (mutable bom)            ; looking for byte-order-mark on input, or ready to write it on output
      (mutable zbom)           ; bom found or placed at position zero
      (mutable big)            ; big endian?
      (immutable decode)       ; input decoder
      (immutable encode)       ; output encoder
      (immutable close)))

 ; keep make-fd in sync with types.h MAKE_FD
  (define (make-fd intfd) intfd)

  (define (port-oops who p msg)
    ($oops/c who
      (make-i/o-port-error p)
      "failed on ~s: ~(~a~)" p msg))

  (define (read-oops who p msg)
    ($oops/c who
      (condition (make-i/o-read-error) (make-i/o-port-error p))
      "failed on ~s: ~(~a~)" p msg))

  (define (write-oops who p msg)
    ($oops/c who
      (condition (make-i/o-write-error) (make-i/o-port-error p))
      "failed on ~s: ~(~a~)" p msg))

  (define (position-oops who p pos msg)
    ($oops/c who
      (condition
        (make-i/o-invalid-position-error pos)
        (make-i/o-port-error p))
      "failed for position ~s on ~s: ~(~a~)" pos p msg))

  (define (open-oops who filename file-options err.msg)
    ($oops/c who
      (let ([err (car err.msg)])
        (cond
          [(eqv? err (constant OPEN-ERROR-PROTECTION))
           (make-i/o-file-protection-error filename)]
          [(eqv? err (constant OPEN-ERROR-EXISTS))
           (make-i/o-file-already-exists-error filename)]
          [(eqv? err (constant OPEN-ERROR-EXISTSNOT))
           (make-i/o-file-does-not-exist-error filename)]
          [else (make-i/o-filename-error filename)]))
      "failed for ~a: ~(~a~)"
      filename
      (cdr err.msg)))

  (define (unget-error who p x)
    ($oops who "cannot unget ~s on ~s" x p))

  (define eol-char?
    (lambda (c)
      (and (memv c '(#\newline #\return #\nel #\ls)) #t)))

  (define-syntax port-gz-mode
    (syntax-rules ()
      [(_ port) ($port-flags-set? port (constant port-flag-compressed))]))
  (define-syntax port-flag-eof-set?
    (syntax-rules ()
      [(_ port) ($port-flags-set? port (constant port-flag-eof))]))
  (define-syntax assert-not-closed
    (syntax-rules ()
      [(_ who port)
       (when (port-closed? port)
         ($oops who "not permitted on closed port ~s" port))]))

  (define-syntax file-options-list
    (syntax-rules ()
      [(_)
       '(no-create no-fail no-truncate compressed replace exclusive append
         perm-set-user-id perm-set-group-id perm-sticky
         perm-no-user-read perm-no-user-write perm-user-execute
         perm-no-group-read perm-no-group-write perm-group-execute
         perm-no-other-read perm-no-other-write perm-other-execute)]))

  (define-syntax eol-style-list
    (syntax-rules ()
      [(_) '(lf cr crlf nel crnel ls none)]))

  (define-syntax error-handling-mode-list
    (syntax-rules ()
      [(_) '(ignore raise replace)]))

  (define ($textual-port-bol? p)
    (let ([index (textual-port-output-index p)])
      (if (fx= index 0)
          ($port-flags-set? p (constant port-flag-bol))
          (eol-char? (string-ref (textual-port-output-buffer p) (fx- index 1))))))

  (define-record-type (transcoder $make-transcoder $transcoder?)
    (nongenerative)
    (opaque #t)
    (sealed #t)
    (fields
      (immutable codec $transcoder-codec)
      (immutable eol-style $transcoder-eol-style)
      (immutable error-handling-mode $transcoder-error-handling-mode)))

  ;; minimum-file-buffer-length is not 0 because of lookahead-u8 and
  ;; unget-u8 and to simplify the logic for setting size and index based
  ;; on length.  the single byte will never be used for output ports.
  (define minimum-file-buffer-length 1)
  (define bytevector-buffer-length 128)
  (define string-buffer-length 16)
  (define unbuffered-transcoded-port-buffer-length 1)

  (define check-option ; for Chez Scheme list-based file open options
    (lambda (who x y)
      (when (and x (not (eq? x y)))
        ($oops who "incompatible options ~s and ~s" x y))))

  ;; Foreign calls to file system
  ;; use critical-section to increment/decrement disable count.
  ;; once we arrive in C code (e.g., bytevector-write) allow deactivation if
  ;; disable-count == 1.  this makes our port operations multitasking
  ;; safe (within a single posix thread if threaded).
  (define $open-input-fd
    (foreign-procedure "(cs)new_open_input_fd"
      (string boolean) scheme-object))
  (define $open-output-fd
    (foreign-procedure "(cs)new_open_output_fd"
      (string int int)
      scheme-object))
  (define $open-input/output-fd
    (foreign-procedure "(cs)new_open_input_output_fd"
      (string int int)
      scheme-object))
  (define $close-fd
    (foreign-procedure "(cs)close_fd"
      (scheme-object boolean) scheme-object))
  (define $bytevector-read
    (foreign-procedure "(cs)bytevector_read"
      (scheme-object scheme-object iptr iptr boolean) scheme-object))
  (define $bytevector-read-nb
    (foreign-procedure "(cs)bytevector_read_nb"
      (scheme-object scheme-object iptr iptr boolean) scheme-object))
  (define $bytevector-write
    (foreign-procedure "(cs)bytevector_write"
      (scheme-object scheme-object iptr iptr boolean) scheme-object))
  (define $put-byte
    (foreign-procedure "(cs)put_byte"
      (scheme-object int boolean) scheme-object))
  (define $set-fd-pos
    (foreign-procedure "(cs)set_fd_pos"
      (scheme-object scheme-object boolean) scheme-object))
  (define $get-fd-pos
    (foreign-procedure "(cs)get_fd_pos"
      (scheme-object boolean) scheme-object))
  (define $get-fd-nonblocking
    (foreign-procedure "(cs)get_fd_non_blocking"
      (scheme-object boolean) scheme-object))
  (define $set-fd-nonblocking
    (foreign-procedure "(cs)set_fd_non_blocking"
      (scheme-object boolean boolean) scheme-object))
  (define $get-fd-length
    (foreign-procedure "(cs)get_fd_length"
      (scheme-object boolean) scheme-object))
  (define $set-fd-length
    (foreign-procedure "(cs)set_fd_length"
      (scheme-object scheme-object boolean) scheme-object))
  (define $fd-regular?
    (foreign-procedure "(cs)fd_regularp" (int) boolean))
  (define $compress-input-fd
    (foreign-procedure "(cs)compress_input_fd" (int integer-64) scheme-object))
  (define $compress-output-fd
    (foreign-procedure "(cs)compress_output_fd" (int) scheme-object))
  (module (clear-open-files register-open-file registered-open-file? unregister-open-file)
    (define open-files #f)
    (define file-guardian)
    (define clear-open-files
     ; called from single-threaded $scheme-init
      (lambda ()
        (set! open-files (make-weak-eq-hashtable))
        (set! file-guardian (make-guardian))))
   ; should register only ports with known system handlers/transcoders
   ; we don't want to get into arbitrary user code when automatically
   ; closing.  when files are closed, we close text ports first, then
   ; binary ports, so it won't generally work to register a text port that
   ; depends on another text port being open or a binary port that
   ; depends on another binary port being open.
    (define register-open-file
      (lambda (p)
        (when open-files
          (with-tc-mutex
            (eq-hashtable-set! open-files p #t)
            (file-guardian p)))))
    (define registered-open-file?
      (lambda (p)
        (and open-files
          (with-tc-mutex
            (eq-hashtable-contains? open-files p)))))
    (define unregister-open-file
      (lambda (p)
        (when open-files
          (with-tc-mutex
            (eq-hashtable-delete! open-files p)))))
    (define silent-close
      (lambda (pvec)
       ; do textual ports first, since they may encapsulate a binary port
        (vector-for-each
          (lambda (x)
            (when (textual-port? x)
              (guard (c [#t (void)]) (close-port x))))
           pvec)
       ; now do binary ports
        (vector-for-each
          (lambda (x)
            (when (binary-port? x)
              (guard (c [#t (void)]) (close-port x))))
           pvec)))
    (set! $close-resurrected-files
     ; called from single-threaded docollect
      (lambda ()
        (when open-files
          (silent-close
            (let f ([i 0])
              (let ([p (file-guardian)])
                (if p
                    (let ([v (f (fx+ i 1))]) (vector-set! v i p) v)
                      (make-vector i))))))))
    (set! $close-files
     ; called from Sscheme_deinit
      (lambda ()
        (with-tc-mutex
          ; don't attempt to close ports if other threads are still running, since the other threads might be
          ; using one or more of the ports up to the bitter end, and port operations are not thread-safe when
          ; two threads operate on the same port.  in particular, trying to close a compressed port here and
          ; in one of the other threads concurrently can result in a double free in gzclose.
          (when (and open-files (if-feature pthreads (= (length ($thread-list)) 1) #t))
            (silent-close (hashtable-keys open-files)))))))

  ;; Helpers for binary-file-ports
  (define (extract-permission-mask options)
    (fxlogor
      (if (enum-set-subset? (file-options perm-set-user-id) options) #o4000 0)
      (if (enum-set-subset? (file-options perm-set-group-id) options) #o2000 0)
      (if (enum-set-subset? (file-options perm-sticky) options) #o1000 0)
      (if (enum-set-subset? (file-options perm-no-user-read) options) 0 #o400)
      (if (enum-set-subset? (file-options perm-no-user-write) options) 0 #o200)
      (if (enum-set-subset? (file-options perm-user-execute) options) #o100 0)
      (if (enum-set-subset? (file-options perm-no-group-read) options) 0 #o40)
      (if (enum-set-subset? (file-options perm-no-group-write) options) 0 #o20)
      (if (enum-set-subset? (file-options perm-group-execute) options) #o10 0)
      (if (enum-set-subset? (file-options perm-no-other-read) options) 0 #o4)
      (if (enum-set-subset? (file-options perm-no-other-write) options) 0 #o2)
      (if (enum-set-subset? (file-options perm-other-execute) options) #o1 0)))

  (define-syntax do-read
    (syntax-rules ()
      [(_ read p_)
       (let ([p p_])
         (do-read read p
                  (binary-port-input-buffer p)
                  0 (bytevector-length (binary-port-input-buffer p))))]
      [(_ read p_ buffer start count)
       (let ([p p_])
         (read ($port-info p) buffer start count (port-gz-mode p)))]))
  (define-syntax bytevector-read
    (syntax-rules ()
      [(_ args ...) (do-read $bytevector-read args ...)]))
  (define-syntax bytevector-read-nb
    (syntax-rules ()
      [(_ args ...) (do-read $bytevector-read-nb args ...)]))

  (define bytevector-write
    (lambda (who p buffer start count)
      (let ([n ($bytevector-write ($port-info p) buffer start count (port-gz-mode p))])
        (unless (fixnum? n) (write-oops who p n))
        n)))

  (define bytevector-flush
    (lambda (who p buffer start count)
      (let ([fd ($port-info p)] [gz (port-gz-mode p)])
        (let loop ([start start] [count count])
          (unless (eq? 0 count)
            (let ([n ($bytevector-write fd buffer start count gz)])
              (unless (fixnum? n) (write-oops who p n))
              (loop (fx+ start n) (fx- count n))))))))

  (define binary-file-port-flush
    (lambda (who p)
      (bytevector-flush who p (binary-port-output-buffer p) 0
        (binary-port-output-index p))
      (set-binary-port-output-index! p 0)))

  (define binary-file-port-ready?
    (lambda (who p)
      (or (not (port-input-empty? p))
          (port-flag-eof-set? p)
          (let ([n (bytevector-read-nb p)])
            (cond
              [(fixnum? n) (set-binary-port-input-size! p n) (not (eq? n 0))]
              [(eof-object? n) (set-port-eof! p #t) #t]
              [(equal? n "interrupt") 'interrupt]
              [else (read-oops who p n)])))))

  (define binary-file-port-lookahead
    (lambda (who p)
      (cond
        [(not (port-input-empty? p))
         (bytevector-u8-ref (binary-port-input-buffer p)
                            (binary-port-input-index p))]
        [(port-flag-eof-set? p) (eof-object)]
        [else (let loop ()
                (let ([n (bytevector-read p)])
                  (cond
                    [(eq? 0 n) (loop)]
                    [(fixnum? n)
                     (set-binary-port-input-size! p n)
                     (bytevector-u8-ref (binary-port-input-buffer p) 0)]
                    [(eof-object? n) (set-port-eof! p #t) n]
                    [(equal? n "interrupt") 'interrupt]
                    [else (read-oops who p n)])))])))

  (define binary-file-port-unget
    (lambda (who p x)
      (when (port-flag-eof-set? p) (unget-error who p x))
      (if (eof-object? x)
          (let ()
            (unless (port-input-empty? p) (unget-error who p x))
            (set-port-eof! p #t))
          (let ([index (binary-port-input-index p)])
            (when (eq? 0 index) (unget-error who p x))
            (set-binary-port-input-index! p (fx1- index))))))

  (define binary-file-port-get
    (lambda (who p)
      (cond
        [(not (port-input-empty? p))
         (let ([index (binary-port-input-index p)])
           (set-binary-port-input-index! p (fx1+ index))
           (bytevector-u8-ref (binary-port-input-buffer p) index))]
        [(port-flag-eof-set? p) (set-port-eof! p #f) (eof-object)]
        [else (let loop ()
                (let ([n (bytevector-read p)])
                  (cond
                    [(eq? 0 n) (loop)]
                    [(fixnum? n)
                     (set-binary-port-input-size! p n)
                     (set-binary-port-input-index! p 1)
                     (bytevector-u8-ref (binary-port-input-buffer p) 0)]
                    [(eof-object? n) n]
                    [(equal? n "interrupt") 'interrupt]
                    [else (read-oops who p n)])))])))

  (define binary-file-port-get-some
    (lambda (who p bv start count)
      (let ([port-count (binary-port-input-count p)])
        (cond
          [(not (eq? 0 port-count))
           (let ([count (fxmin count port-count)]
                 [index (binary-port-input-index p)])
             (bytevector-copy! (binary-port-input-buffer p) index bv start count)
             (set-binary-port-input-index! p (fx+ index count))
             count)]
          [(port-flag-eof-set? p) (set-port-eof! p #f) (eof-object)]
          [(and (fx<= count max-get-copy) (fx<= count (bytevector-length (binary-port-input-buffer p))))
           (let ([n (bytevector-read p)])
             (cond
               [(fixnum? n)
                (let ([count (fxmin n count)])
                  (set-binary-port-input-size! p n)
                  (set-binary-port-input-index! p count)
                  (bytevector-copy! (binary-port-input-buffer p) 0 bv start count)
                  count)]
               [(eof-object? n) n]
               [(equal? n "interrupt") 'interrupt]
               [else (read-oops who p n)]))]
          [else (let ([n (bytevector-read p bv start count)])
                  (cond
                    [(fixnum? n) n]
                    [(eof-object? n) n]
                    [(equal? n "interrupt") 'interrupt]
                    [else (read-oops who p n)]))]))))

  (define binary-file-port-clear-input
    (lambda (who p)
      (set-binary-port-input-size! p 0)))

  (define binary-file-port-put
    (lambda (who p x)
      (let ([index (binary-port-output-index p)]
            [buffer (binary-port-output-buffer p)])
        (cond
          [(not (port-output-full? p))
           (bytevector-u8-set! buffer index x)
           (set-binary-port-output-index! p (fx1+ index))]
          [(fx= index 0) ; since full, => size is 0 => unbuffered
           (let loop ()
             (let ([n ($put-byte ($port-info p) x (port-gz-mode p))])
               (unless (fixnum? n) (write-oops who p n))
               (when (fx= n 0) (loop))))]
          [else
           (bytevector-u8-set! buffer index x)
           (bytevector-flush who p buffer 0 (fx1+ index))
           (set-binary-port-output-index! p 0)]))))

  ;; The following diagram shows the control flow of put-some.
  ;; It is complicated because it must handle nonblocking ports
  ;; while also trying to minimize the number of operating system calls and
  ;; being smart about when to buffer.
  ;;
  ;; Arrows marked with "@" are guarded with a try-fill that
  ;; will try to exit the function early by copying the new bytevector
  ;; into the old bytevector.  Arrows marked with "@@" are the same
  ;; but in future versions might be willing to partially copy
  ;; the old buffer where as the "@" lines will only copy if
  ;; the entire new data fits in the old buffer.
  ;;
  ;; old is the port's buffer
  ;; new is the byte vector being passed in
  ;;
  ;; len(x)=0 tests whether x is empty and returns #t or #f
  ;; write(x) writes the old buffer to the operating system and
  ;;   returns either ALL if all data was written or PARTIAL if
  ;;   one part of the data was written
  ;; shift(old) bytevector copies to the front of old
  ;;   the part of old that wasn't written

  #|
   --@-> len(old)=0 --(#f)--> write(old) --(PARTIAL)--> shift(old) --@@--> DONE
             |                   |
             |                   |
            (#t) <---@---(ALL)---+
             |
             V
         len(new)=0 --(#f)--> write(new) --(PARTIAL)-----------------@@--> DONE
             |                   |
             |                   |
            (#t)               (ALL)
             |                   |
             V                   V
            DONE                DONE
  |#

  (define binary-file-port-put-some
    (lambda (who p bv start count)
      ;; from-start: where to fill from
      ;; from-count: how much to fill from (i.e. how much we want to put)
      ;; to-start: where to fill to
      ;; to-count: how much to fill to (i.e. how much room we have)
      ;; body: what to do if not filling
      (define-syntax try-fill
        (syntax-rules ()
          [(_ from-start from-count to-start to-count body)
           (if (and (fx<= from-count max-put-copy)
                    (fx<= from-count to-count))
               (begin
                 (bytevector-copy! bv from-start
                                   (binary-port-output-buffer p) to-start
                                   from-count)
                 (set-binary-port-output-index! p (fx+ to-start from-count))
                 (fx+ (fx- from-start start) from-count))
               body)]))

      ;; buffer: what to write from
      ;; start: where to write from
      ;; count: how much to write from
      ;; (n): var to bind to how many written
      ;; zero: what to do if count is zero
      ;; normal: what to do if all count written
      ;; interrupted: what to do not all count written
      (define-syntax try-write
        (syntax-rules ()
          [(_ buffer start count (n) zero normal partial)
           (if (eq? 0 count)
               zero
               (let ([n (bytevector-write who p buffer start count)])
                 (if (eq? n count)
                     normal
                     partial)))]))

      ;; On entry: old buffer has been completely written
      ;; and we need to write the new buffer
      (define (write-new)
        (try-write bv start count (n) 0 count
          (try-fill (fx+ start n) (fx- count n) 0 (binary-port-output-size p) n)))

      (let ([port-index (binary-port-output-index p)]
            [port-count (binary-port-output-count p)]
            [port-size (binary-port-output-size p)]
            [port-buffer (binary-port-output-buffer p)])
        (try-fill start count port-index port-count
          (try-write port-buffer 0 port-index (n)
            (write-new)
            (try-fill start count 0 port-size
              (begin
                (set-binary-port-output-index! p 0) ;; may be reset by try-fill
                (write-new)))
            (let ([new-index (fx- port-index n)])
              (bytevector-copy! port-buffer n port-buffer 0 new-index)
              (set-binary-port-output-index! p new-index)
              (try-fill start count new-index (fx- port-size new-index) 0)))))))

  (define binary-file-port-clear-output
    (lambda (who p)
      (set-binary-port-output-index! p 0)))

  (define binary-file-port-close-port
    (lambda (who p)
      (when (input-port? p)
        (set-port-eof! p #f)
        (set-binary-port-input-size! p 0))
      (when (output-port? p) (set-binary-port-output-size! p 0))
      (unregister-open-file p)
      ; mark port closed before closing fd.  if an interrupt occurs, we'd prefer
      ; that the fd's resources never be freed than to have an open port floating
      ; around with fd resources that have already been freed.
      (mark-port-closed! p)
      (let ([msg ($close-fd ($port-info p) (port-gz-mode p))])
        (unless (eq? #t msg) (port-oops who p msg)))))

  (define-syntax binary-file-port-port-position
    (syntax-rules ()
      [(_ mode who ?p)
       (member (datum mode) '(in out in/out))
       (let ([p ?p])
         (let ([n ($get-fd-pos ($port-info p) (port-gz-mode p))])
           (unless (or (fixnum? n) (bignum? n)) (port-oops who p n))
           (- (+ n (if (eq? 'mode 'in) 0 (binary-port-output-index p)))
              (if (eq? 'mode 'out) 0 (binary-port-input-count p)))))]))

  (define binary-file-port-set-port-position!
    (lambda (who p x)
      (unless (and (integer? x) (exact? x) (<= 0 x (- (expt 2 63) 1)))
        ($oops who "~s is not a valid position" x))
      (let ([n ($set-fd-pos ($port-info p) x (port-gz-mode p))])
        (unless (eq? n #t) (position-oops who p x n)))))

  (define binary-file-port-port-nonblocking?
    (lambda (who p)
      (let ([n ($get-fd-nonblocking ($port-info p) (port-gz-mode p))])
        (unless (boolean? n) (port-oops who p n))
        n)))

  (define binary-file-port-set-port-nonblocking!
    (lambda (who p x)
      (let ([n ($set-fd-nonblocking ($port-info p) x (port-gz-mode p))])
        (unless (eq? n #t) (port-oops who p n)))))

  (define binary-file-port-port-length
    (lambda (who p)
      (let ([n ($get-fd-length ($port-info p) (port-gz-mode p))])
        (unless (or (fixnum? n) (bignum? n)) (port-oops who p n))
        n)))

  (define binary-file-port-set-port-length!
    (lambda (who p x)
      (unless (and (integer? x) (exact? x) (<= 0 x (- (expt 2 63) 1)))
        ($oops who "~s is not a valid length" x))
      (let ([n ($set-fd-length ($port-info p) x (port-gz-mode p))])
        (unless (eq? n #t) (port-oops who p n)))))

  ;; Helpers for binary-custom-ports
  (define (bv-read! who p read! bv start count)
    (let ([n (read! bv start count)])
      (unless (and (fixnum? n) (fx<= 0 n count))
        ($oops who "invalid result ~s from read! on ~s" n p))
      n))

  (define (binary-port-read! who p read!)
    (let ([bv (binary-port-input-buffer p)])
      (let ([n (bv-read! who p read! bv 0 (bytevector-length bv))])
        (if (eq? 0 n)
            (eof-object)
            (begin
              (set-binary-port-input-size! p n)
              (bytevector-u8-ref bv 0))))))

  (define bv-write! ;; loops until count written
    (lambda (who p write! bv start count)
      (let loop ([start start]
                 [count count])
        (unless (eq? 0 count)
          (let ([result (write! bv start count)])
            (unless (and (fixnum? result) (fx<= 0 result count))
              ($oops who "invalid result ~s from write! on ~s" result p))
            (loop (fx+ start result) (fx- count result)))))))

  (define binary-custom-port-lookahead
    (lambda (who p read!)
      (cond
        [(not (port-input-empty? p))
         (bytevector-u8-ref (binary-port-input-buffer p)
                            (binary-port-input-index p))]
        [(port-flag-eof-set? p) (eof-object)]
        [else (let ([x (binary-port-read! who p read!)])
                (when (eof-object? x)
                  (set-port-eof! p #t))
                x)])))

  (define binary-custom-port-unget
    (lambda (who p x)
      (when (port-flag-eof-set? p) (unget-error who p x))
      (if (eof-object? x)
          (let ()
            (unless (port-input-empty? p) (unget-error who p x))
            (set-port-eof! p #t))
          (let ([index (binary-port-input-index p)])
            (when (eq? 0 index) (unget-error who p x))
            (set-binary-port-input-index! p (fx1- index))))))

  (define binary-custom-port-get
    (lambda (who p read!)
      (cond
        [(not (port-input-empty? p))
         (let ([index (binary-port-input-index p)])
           (set-binary-port-input-index! p (fx1+ index))
           (bytevector-u8-ref (binary-port-input-buffer p) index))]
        [(port-flag-eof-set? p) (set-port-eof! p #f) (eof-object)]
        [else (let ([x (binary-port-read! who p read!)])
                (unless (eof-object? x)
                  (set-binary-port-input-index! p 1))
                x)])))

  (define binary-custom-port-get-some
    (lambda (who p read! bv start count)
      (let ([port-count (binary-port-input-count p)])
        (cond
          [(not (eq? 0 port-count))
           (let ([count (fxmin count port-count)]
                 [index (binary-port-input-index p)])
             (bytevector-copy! (binary-port-input-buffer p) index bv start count)
             (set-binary-port-input-index! p (fx+ index count))
             count)]
          [(port-flag-eof-set? p) (set-port-eof! p #f) (eof-object)]
          [else (let ([n (bv-read! who p read! bv start count)])
                  (if (eq? 0 n)
                      (eof-object)
                      n))]))))

  (define binary-custom-port-clear-input
    (lambda (who p)
      (set-binary-port-input-size! p 0)))

  (define binary-custom-port-put
    (lambda (who p write! x)
      (let ([buffer (binary-port-output-buffer p)]
            [index (binary-port-output-index p)])
        (bytevector-u8-set! buffer index x)
        (let ([new-index (fx1+ index)])
          (if (port-output-full? p)
              (begin
                (bv-write! who p write! buffer 0 new-index)
                (set-binary-port-output-index! p 0))
              (set-binary-port-output-index! p new-index))))))

  (define binary-custom-port-put-some
    (lambda (who p write! bv start count)
      (if (and (fx<= count max-put-copy) (fx<= count (binary-port-output-count p)))
          (begin
            (let ([index (binary-port-output-index p)])
              (bytevector-copy! bv start
                                (binary-port-output-buffer p) index
                                count)
              (set-binary-port-output-index! p (fx+ index count))
              count))
          (begin
            (bv-write! who p write! (binary-port-output-buffer p)
                       0 (binary-port-output-index p))
            (bv-write! who p write! bv start count)
            (set-binary-port-output-index! p 0)
            count))))

  (define-syntax binary-custom-port-flush
    (syntax-rules ()
      [(_ who p_ write!)
       (let ([p p_])
         (bv-write! who p write! (binary-port-output-buffer p)
                    0 (binary-port-output-index p))
         (set-binary-port-output-index! p 0))]))

  (define binary-custom-port-clear-output
    (lambda (who p)
      (set-binary-port-output-index! p 0)))

  (define binary-custom-port-close-port
    (lambda (who p close)
      (when close (close))
      (mark-port-closed! p)
      (when (input-port? p)
        (set-port-eof! p #f)
        (set-binary-port-input-size! p 0))
      (when (output-port? p) (set-binary-port-output-size! p 0))))

  (define-syntax binary-custom-port-port-position
    (syntax-rules ()
      [(_ mode who ?p get-position)
       (member (datum mode) '(in out in/out))
       (let ([p ?p])
         (let ([n (get-position)])
           (unless (or (and (fixnum? n) (fx>= n 0)) (and (bignum? n) (>= n 0)))
             ($oops who "invalid result ~s from get-position on ~s" n p))
           (- (+ n (if (eq? 'mode 'in) 0 (binary-port-output-index p)))
              (if (eq? 'mode 'out) 0 (binary-port-input-count p)))))]))

  ;; Helpers for textual-custom-ports
  (define (str-read! who p read! str start count)
    (let ([n (read! str start count)])
      (unless (and (fixnum? n) (fx<= 0 n count))
        ($oops who "invalid result ~s from read! on ~s" n p))
      n))

  (define (textual-port-read! who p read!)
    (let ([str (textual-port-input-buffer p)])
      (let ([n (str-read! who p read! str 0 (string-length str))])
        (if (fx= n 0)
            (eof-object)
            (begin
              (set-textual-port-input-size! p n)
              (string-ref str 0))))))

  (define str-write! ;; loops until count written
    (lambda (who p write! str start count)
      (let loop ([start start] [count count])
        (unless (fx= count 0)
          (let ([result (write! str start count)])
            (unless (and (fixnum? result) (fx<= 0 result count))
              ($oops who "invalid result ~s from write! on ~s" result p))
            (loop (fx+ start result) (fx- count result)))))))

  (define textual-custom-port-lookahead
    (lambda (who p read!)
      (cond
        [(not (port-input-empty? p))
         (string-ref
           (textual-port-input-buffer p)
           (textual-port-input-index p))]
        [(port-flag-eof-set? p) (eof-object)]
        [else
         (let ([x (textual-port-read! who p read!)])
           (when (eof-object? x) (set-port-eof! p #t))
           x)])))

  (define textual-custom-port-unget
    (lambda (who p x)
      (when (port-flag-eof-set? p) (unget-error who p x))
      (if (eof-object? x)
          (let ()
            (unless (port-input-empty? p) (unget-error who p x))
            (set-port-eof! p #t))
          (let ([index (textual-port-input-index p)])
            (when (eq? 0 index) (unget-error who p x))
            (set-textual-port-input-index! p (fx1- index))))))

  (define textual-custom-port-get
    (lambda (who p read!)
      (cond
        [(not (port-input-empty? p))
         (let ([index (textual-port-input-index p)])
           (set-textual-port-input-index! p (fx1+ index))
           (string-ref (textual-port-input-buffer p) index))]
        [(port-flag-eof-set? p) (set-port-eof! p #f) (eof-object)]
        [else (let ([x (textual-port-read! who p read!)])
                (unless (eof-object? x)
                  (set-textual-port-input-index! p 1))
                x)])))

  (define textual-custom-port-get-some
    (lambda (who p read! str start count)
      (let ([port-count (textual-port-input-count p)])
        (cond
          [(not (fx= port-count 0))
           (let ([count (fxmin count port-count)]
                 [index (textual-port-input-index p)])
             (string-copy! (textual-port-input-buffer p) index str start count)
             (set-textual-port-input-index! p (fx+ index count))
             count)]
          [(port-flag-eof-set? p) (set-port-eof! p #f) (eof-object)]
          [else (let ([n (str-read! who p read! str start count)])
                  (if (eq? 0 n)
                      (eof-object)
                      n))]))))

  (define textual-custom-port-clear-input
    (lambda (who p)
      (set-textual-port-input-size! p 0)))

  (define textual-custom-port-put
    (lambda (who p write! x)
      (let ([buffer (textual-port-output-buffer p)]
            [index (textual-port-output-index p)])
        (string-set! buffer index x)
        (let ([new-index (fx1+ index)])
          (if (port-output-full? p)
              (begin
                (str-write! who p write! buffer 0 new-index)
                (set-port-bol! p (eol-char? (string-ref buffer index)))
                (set-textual-port-output-index! p 0))
              (set-textual-port-output-index! p new-index))))))

  (define textual-custom-port-put-some
    (lambda (who p write! str start count)
      (if (and (fx<= count max-put-copy) (fx<= count (textual-port-output-count p)))
          (begin
            (let ([index (textual-port-output-index p)])
              (string-copy! str start
                            (textual-port-output-buffer p) index
                            count)
              (set-textual-port-output-index! p (fx+ index count))
              count))
          (begin
            (str-write! who p write! (textual-port-output-buffer p)
                        0 (textual-port-output-index p))
            (str-write! who p write! str start count)
            (set-textual-port-output-index! p 0)
            (set-port-bol! p (eol-char? (string-ref str (fx- (fx+ start count) 1))))
            count))))

  (define textual-custom-port-flush
    (lambda (who p write!)
      (let ([n (textual-port-output-index p)])
        (unless (fx= n 0)
          (let ([buffer (textual-port-output-buffer p)])
            (str-write! who p write! buffer 0 n)
            (set-port-bol! p (eol-char? (string-ref buffer (fx- n 1))))
            (set-textual-port-output-index! p 0))))))

  (define textual-custom-port-clear-output
    (lambda (who p)
      (set-textual-port-output-index! p 0)))

  (define textual-custom-port-close-port
    (lambda (who p close)
      (when close (close))
      (mark-port-closed! p)
      (when (input-port? p)
        (set-port-eof! p #f)
        (set-textual-port-input-size! p 0))
      (when (output-port? p) (set-textual-port-output-size! p 0))))

  (define-syntax check-interrupt
    (syntax-rules ()
      [(_ e)
       (let loop ()
         (let ([x e])
           (if (eq? x 'interrupt)
               (begin ($event) (loop))
               x)))]))

  (module (open-binary-fd-input-port)
    ;; NOTE: port-info stores the file descriptor number or gzFile object
    (define (make-binary-file-input-handler regular?)
      (make-port-handler
        [ready?
         (lambda (who p)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-ready? who p))))]
        [lookahead
         (lambda (who p)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-lookahead who p))))]
        [unget
         (lambda (who p x)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-unget who p x)))]
        [get
         (lambda (who p)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-get who p))))]
        [get-some
         (lambda (who p bv start count)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-get-some who p bv start count))))]
        [clear-input
         (lambda (who p)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-clear-input who p)))]
        [put #f]
        [put-some #f]
        [flush #f]
        [clear-output #f]
        [close-port
         (lambda (who p)
           (critical-section
             (unless (port-closed? p)
               (binary-file-port-close-port who p))))]
        [port-position
         (and regular?
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-port-position in who p))))]
        [set-port-position!
         (and regular?
           (lambda (who p x)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-set-port-position! who p x)
               (set-binary-port-input-size! p 0) ;; junk the buffer data
               (set-port-eof! p #f))))]
        [port-length
         (and regular?
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-port-length who p))))]
        [set-port-length! #f]
        [port-nonblocking?
         (if-feature windows #f
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-port-nonblocking? who p))))]
        [set-port-nonblocking!
         (if-feature windows #f
           (lambda (who p x)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-set-port-nonblocking! who p x))))]))

    (define open-binary-fd-input-port
      (lambda (who name fd regular? mode gzflag)
        (let ([buffer-length (if (eq? mode (buffer-mode none))
                                 minimum-file-buffer-length
                                 (file-buffer-size))])
          (let ([p ($make-binary-input-port
                    name ;; name
                    (make-binary-file-input-handler regular?) ;; handler
                    (make-bytevector buffer-length) ;; buffer
                    fd)]) ;; info
            (if (eq? mode (buffer-mode block))
                ($set-port-flags! p (constant port-flag-block-buffered))
                (when (eq? mode (buffer-mode line))
                  ($set-port-flags! p (constant port-flag-line-buffered))))
            ($set-port-flags! p (constant port-flag-file))
            (when gzflag
              ($set-port-flags! p (constant port-flag-compressed)))
            ;; size is set by $make-binary-input-port, but
            ;; we want it to trip the handler the first time so
            ;; re-set the size to zero
            (set-binary-port-input-size! p 0)
            (register-open-file p)
            p)))))

  (module (open-binary-fd-output-port)
    ;; NOTE: output-size is one less than actual buffer size so
    ;; we always have a place to put data before calling write
    (define (make-binary-file-output-handler regular?)
      (make-port-handler
        [ready? #f]
        [lookahead #f]
        [unget #f]
        [get #f]
        [get-some #f]
        [clear-input #f]
        [put
         (lambda (who p x)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-put who p x)))]
        [put-some
         (lambda (who p bv start count)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-put-some who p bv start count)))]
        [flush
         (lambda (who p)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-flush who p)))]
        [clear-output
         (lambda (who p)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-clear-output who p)))]
        [close-port
         (lambda (who p)
           (critical-section
             (unless (port-closed? p)
               (binary-file-port-flush who p)
               (binary-file-port-close-port who p))))]
        [port-position
         (and regular?
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-port-position out who p))))]
        [set-port-position!
         (and regular?
           (lambda (who p x)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-flush who p)
               (binary-file-port-set-port-position! who p x))))]
        [port-length
         (and regular?
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-flush who p)
               (binary-file-port-port-length who p))))]
        [set-port-length!
         (and regular?
           (lambda (who p x)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-flush who p)
               (binary-file-port-set-port-length! who p x))))]
        [port-nonblocking?
         (if-feature windows #f
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-port-nonblocking? who p))))]
        [set-port-nonblocking!
         (if-feature windows #f
           (lambda (who p x)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-set-port-nonblocking! who p x))))]))

    (define open-binary-fd-output-port
      (lambda (who name fd regular? b-mode lock compressed)
        (let ([buffer-length (if (eq? b-mode (buffer-mode none))
                                 minimum-file-buffer-length
                                 (file-buffer-size))])
          (let ([p ($make-binary-output-port
                    name ;; name
                    (make-binary-file-output-handler regular?) ;; handler
                    (make-bytevector buffer-length) ;; buffer
                    fd)]) ;; info
            (if (eq? b-mode (buffer-mode block))
                ($set-port-flags! p (constant port-flag-block-buffered))
                (when (eq? b-mode (buffer-mode line))
                  ($set-port-flags! p (constant port-flag-line-buffered))))
            ($set-port-flags! p (constant port-flag-file))
            (when compressed
              ($set-port-flags! p (constant port-flag-compressed)))
            (when lock
              ($set-port-flags! p (constant port-flag-exclusive)))
            (set-binary-port-output-size! p (fx1- buffer-length)) ;; leave room for put to work
            (register-open-file p)
            p)))))

  (module (open-binary-fd-input/output-port)
    ;; Two modes: ready-for-input and ready-for-output
    ;;
    ;; ready-for-input: output-size == 0
    ;; ready-for-output: output-size == length-1 and input-size == 0
    ;;
    ;; unbuffered port (ports with length 1 buffers) may be both
    ;; ready-for-input and ready-for-output simultaneously,
    ;; but it is never the case that both
    ;; output-size != 0 and input-size != 0
    ;;
    ;; for our purposes having the eof flag set is the same as input-size != 0

    (define-syntax make-ready-for-input
      (syntax-rules ()
        [(_ who p_)
         (let ([p p_])
           (unless (eq? 0 (binary-port-output-size p))
             (binary-file-port-flush who p)
             ;; don't set input-size; it is set only after a read
             (set-binary-port-output-size! p 0)))]))

    (module ((make-ready-for-output $make-ready-for-output))
      (define $make-ready-for-output
        (lambda (who p)
          (unless (eq? (binary-port-input-size p) 0)
            (unless (port-input-empty? p)
              (binary-file-port-set-port-position! who p
                (binary-file-port-port-position in/out who p)))
            (set-binary-port-input-size! p 0))
          (set-port-eof! p #f)
          (set-binary-port-output-size! p
            (fx1- (bytevector-length (binary-port-output-buffer p))))))

      (define-syntax make-ready-for-output
        (syntax-rules ()
          [(_ ?who ?p)
           (let ([p ?p])
             (when (eq? (binary-port-output-size p) 0)
               ($make-ready-for-output ?who p)))])))

    (define (make-binary-file-input/output-handler regular?)
      (make-port-handler
        [ready?
         (lambda (who p)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (make-ready-for-input who p)
               (binary-file-port-ready? who p))))]
        [lookahead
         (lambda (who p)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (make-ready-for-input who p)
               (binary-file-port-lookahead who p))))]
        [unget
         (lambda (who p x)
           (critical-section
             (assert-not-closed who p)
             (make-ready-for-input who p)
             (binary-file-port-unget who p x)))]
        [get
         (lambda (who p)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (make-ready-for-input who p)
               (binary-file-port-get who p))))]
        [get-some
         (lambda (who p bv start count)
           (check-interrupt
             (critical-section
               (assert-not-closed who p)
               (make-ready-for-input who p)
               (binary-file-port-get-some who p bv start count))))]
        [clear-input
         (lambda (who p)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-clear-input who p)))]
        [put
         (lambda (who p x)
           (critical-section
             (assert-not-closed who p)
             (make-ready-for-output who p)
             (binary-file-port-put who p x)))]
        [put-some
         (lambda (who p bv start count)
           (critical-section
             (assert-not-closed who p)
             (make-ready-for-output who p)
             (binary-file-port-put-some who p bv start count)))]
        [flush
         (lambda (who p)
           (critical-section
             (assert-not-closed who p)
             (make-ready-for-output who p)
             (binary-file-port-flush who p)))]
        [clear-output
         (lambda (who p)
           (critical-section
             (assert-not-closed who p)
             (binary-file-port-clear-output who p)))]
        [close-port
         (lambda (who p)
           (critical-section
             (unless (port-closed? p)
               (binary-file-port-flush who p)
               (binary-file-port-close-port who p))))]
        [port-position
         (and regular?
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-port-position in/out who p))))]
        [set-port-position!
         (and regular?
           (lambda (who p x)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-flush who p)
               (binary-file-port-set-port-position! who p x)
               (set-binary-port-input-size! p 0) ;; junk the buffer data
               (set-port-eof! p #f))))]
        [port-length
         (and regular?
           (lambda (who p)
             (critical-section
               (assert-not-closed who p)
               (binary-file-port-flush who p)
               (binary-file-port-port-length who p))))]
        [set-port-length!
         (and regular?
           (lambda (who p x)
             (critical-section
               (assert-not-closed who p)
               (cond
                 [(and (fx= (binary-port-input-size p) 0) (not (port-flag-eof-set? p)))
                  (binary-file-port-flush who p)
                  (binary-file-port-set-port-length! who p x)]
                 [else
                  (let ([pos (binary-file-port-port-position in/out who p)])
                    (set-binary-port-input-size! p 0) ;; junk the buffer data
                    (set-port-eof! p #f)
                    (binary-file-port-set-port-length! who p x)
                    (binary-file-port-set-port-position! who p pos))]))))]
        [port-nonblocking?
         (if-feature windows #f
           (lambda (who p)
             (critical-section
              (assert-not-closed who p)
              (binary-file-port-port-nonblocking? who p))))]
        [set-port-nonblocking!
         (if-feature windows #f
           (lambda (who p x)
             (critical-section
              (assert-not-closed who p)
              (binary-file-port-set-port-nonblocking! who p x))))]))

    (define open-binary-fd-input/output-port
      (lambda (who name fd regular? b-mode lock compressed)
        (let ([buffer-length (if (eq? b-mode (buffer-mode none))
                                 minimum-file-buffer-length
                                 (file-buffer-size))])
          (let ([p ($make-binary-input/output-port
                    name ;; name
                    (make-binary-file-input/output-handler regular?) ;; handler
                    (make-bytevector buffer-length) ;; input buffer
                    (make-bytevector buffer-length) ;; output buffer
                    fd)]) ;; info
            (if (eq? b-mode (buffer-mode block))
                ($set-port-flags! p (constant port-flag-block-buffered))
                (when (eq? b-mode (buffer-mode line))
                  ($set-port-flags! p (constant port-flag-line-buffered))))
            ($set-port-flags! p (constant port-flag-file))
            (when compressed
              ($set-port-flags! p (constant port-flag-compressed)))
            (when lock
              ($set-port-flags! p (constant port-flag-exclusive)))
            ;; size is set by $make-binary-input/output-port, but
            ;; we want it to trip the handler the first time so
            ;; re-set the size to zero
            (set-binary-port-input-size! p 0)
            (set-binary-port-output-size! p (fx1- buffer-length)) ;; leave room for put to work
            (register-open-file p)
            p)))))

;;;; Public functions
  ;; All section numbers are from ``R6RS -- Standard Libraries''

;;;; 8.1 Condition types (in exceptions.ss)

;;;; 8.2 Port I/O: (rnrs io ports (6))
;;;; 8.2.1 Filenames
;;;; 8.2.2 File options
  ;; file-options in syntax.ss
  (set-who! $file-options (make-enumeration (file-options-list)))
  (set-who! $make-file-options (enum-set-constructor $file-options))

;;;; 8.2.3 Buffer modes
  ;; buffer-mode in syntax.ss
  (set-who! buffer-mode?
    (lambda (mode) (and (memq mode '(none line block)) #t)))

;;;; 8.2.4 Transcoders
  (let ()
    (define (encode-oops who tp c)
      ($oops/c who
        (make-i/o-encoding-error tp c)
        (parameterize ([print-unicode #f])
          (let* ([tx (codec-info-tx ($port-info tp))]
                 [name (codec-name ($transcoder-codec tx))])
            (if (and (eqv? c #\newline) (not (memq ($transcoder-eol-style tx) '(none lf))))
                (format "~a codec cannot encode ~s with eol-style ~s"
                  name c ($transcoder-eol-style tx))
                (format "~a codec cannot encode ~s" name c))))))

    (define (decode-oops who tp msg . args)
      (apply $oops/c who
        (make-i/o-decoding-error tp)
         msg args))

    (define (flush-buffer who bp bv i k)
      (if (fx= k 0)
          0
          (let ([n (call-port-handler put-some who bp bv i k)])
            (if (fx= n 0)
                (begin
                  (unless (fx= i 0) (bytevector-copy! bv i bv 0 k))
                  k)
                (flush-buffer who bp bv (fx+ i n) (fx- k n))))))

    (define get-some-maybe-nb
     ; get some from binary port bp.  if ifready? is true, don't block if port
     ; isn't ready, even if port has not been set nonblocking
      (lambda (who bp bv start ifready?)
        (let ([h ($port-handler bp)])
         ; port-handler-ready? may raise an exception, but that's okay because ifready?
         ; is true only if this is called from transcoded-port's port-handler-ready?.
          (if (or (not ifready?) ((port-handler-ready? h) who bp))
              ((port-handler-get-some h) who bp bv start (fx- (bytevector-length bv) start))
              0))))

    (let ()
      (define latin-1-decode
        (let ()
          (define (return ans i iend cr? bytes info)
            (codec-info-next-set! info i)
            (codec-info-iend-set! info iend)
            (codec-info-ibytes-set! info bytes)
            (codec-info-icr-set! info cr?)
            ans)
          (lambda (who tp str start count ifready?)
            (let ([info ($port-info tp)])
              (let ([bp (codec-info-bp info)]
                    [ioffsets (and (eq? str (textual-port-input-buffer tp)) (codec-info-ioffsets info))]
                    [bv (codec-info-bv info)]
                    [jend (fx+ start count)])
                (let loop ([j start]
                           [i (codec-info-next info)]
                           [iend (codec-info-iend info)]
                           [cr? (codec-info-icr info)]
                           [bytes 0])
                  (cond
                    [(fx= j jend) (return count i iend cr? bytes info)]
                    [(fx= i iend)
                     (if (fx= j start)
                         (let ([n (get-some-maybe-nb who bp bv 0 ifready?)])
                           (cond
                             [(eof-object? n) (return #!eof i iend #f bytes info)]
                             [(fx= n 0) (return 0 i iend cr? bytes info)]
                             [else (loop j 0 n cr? bytes)]))
                        ; don't try to read in this case to avoid dealing with eof
                         (return (fx- j start) i iend cr? bytes info))]
                    [else
                     (let ([b (bytevector-u8-ref bv i)])
                       (cond
                         [(fx= b #x0d)
                          (cond
                            [(eq? ($transcoder-eol-style (codec-info-tx info)) 'none)
                             (string-set! str j #\return)
                             (when ioffsets (fxvector-set! ioffsets j bytes))
                             (loop (fx+ j 1) (fx+ i 1) iend cr? (fx+ bytes 1))]
                            [else
                             (string-set! str j #\newline)
                             (when ioffsets (fxvector-set! ioffsets j bytes))
                             (loop (fx+ j 1) (fx+ i 1) iend #t (fx+ bytes 1))])]
                         [(fx= b #x0a)
                          (cond
                            [cr? (loop j (fx+ i 1) iend #f (fx+ bytes 1))]
                            [else
                             (string-set! str j #\newline)
                             (when ioffsets (fxvector-set! ioffsets j bytes))
                             (loop (fx+ j 1) (fx+ i 1) iend cr? (fx+ bytes 1))])]
                         [(fx= b #x85) ; NEL
                          (cond
                            [cr? (loop j (fx+ i 1) iend #f (fx+ bytes 1))]
                            [else
                             (string-set! str j
                               (if (eq? ($transcoder-eol-style (codec-info-tx info)) 'none)
                                   (integer->char #x85)
                                   #\newline))
                             (when ioffsets (fxvector-set! ioffsets j bytes))
                             (loop (fx+ j 1) (fx+ i 1) iend cr? (fx+ bytes 1))])]
                         [else
                          (string-set! str j (integer->char b))
                          (when ioffsets (fxvector-set! ioffsets j bytes))
                          (loop (fx+ j 1) (fx+ i 1) iend #f (fx+ bytes 1))]))])))))))

      (define latin-1-encode
        (let ()
          (define (return ans o info)
            (codec-info-next-set! info o)
            ans)
          (lambda (who tp str start count)
            (let ([info ($port-info tp)])
              (let ([bp (codec-info-bp info)]
                    [bv (codec-info-bv info)]
                    [jend (fx+ start count)])
                (define codec-buffer-length (bytevector-length bv))
                (let loop ([j start] [o (codec-info-next info)])
                  (cond
                    [(fx= j jend) (return count o info)]
                    [(fx= o codec-buffer-length)
                     (let ([o (flush-buffer who bp bv 0 o)])
                       (if (fx= o codec-buffer-length)
                           (return (fx- j start) o info)
                           (loop j o)))]
                    [else
                     (let ([int (char->integer (string-ref str j))])
                       (cond
                         [(fx= int #x0a)
                          (let ([eol-style ($transcoder-eol-style (codec-info-tx info))])
                            (case eol-style
                              [(lf none)
                               (bytevector-u8-set! bv o #x0a)
                               (loop (fx+ j 1) (fx+ o 1))]
                              [(cr)
                               (bytevector-u8-set! bv o #x0d)
                               (loop (fx+ j 1) (fx+ o 1))]
                              [(nel)
                               (bytevector-u8-set! bv o #x85)
                               (loop (fx+ j 1) (fx+ o 1))]
                              [(crlf crnel)
                               (let f ([o o])
                                 (if (fx< o (fx- codec-buffer-length 1))
                                     (begin
                                       (bytevector-u8-set! bv o #x0d)
                                       (bytevector-u8-set! bv (fx+ o 1) (if (eq? eol-style 'crlf) #x0a #x85))
                                       (loop (fx+ j 1) (fx+ o 2)))
                                     (let ([new-o (flush-buffer who bp bv 0 o)])
                                       (if (fx= new-o o)
                                           (return (fx- j start) o info)
                                           (f new-o)))))]
                              [(ls)
                               (let ([error-mode ($transcoder-error-handling-mode (codec-info-tx info))])
                                 (case error-mode
                                   [(ignore) (loop (fx+ j 1) o)]
                                   [(replace)
                                    (bytevector-u8-set! bv o (char->integer #\?))
                                    (loop (fx+ j 1) (fx+ o 1))]
                                   [(raise) (encode-oops who tp #\newline)]
                                   [else ($oops who "unknown error handling mode ~s" error-mode)]))]
                              [else ($oops who "unrecognized eol style ~s" eol-style)]))]
                         [(fx<= int 255)
                          (bytevector-u8-set! bv o int)
                          (loop (fx+ j 1) (fx+ o 1))]
                         [else
                          (let ([error-mode ($transcoder-error-handling-mode (codec-info-tx info))])
                            (case ($transcoder-error-handling-mode (codec-info-tx info))
                              [(ignore) (loop (fx+ j 1) o)]
                              [(replace)
                               (bytevector-u8-set! bv o (char->integer #\?))
                               (loop (fx+ j 1) (fx+ o 1))]
                              [(raise) (encode-oops who tp (string-ref str j))]
                              [else ($oops who "unknown error handling mode ~s" error-mode)]))]))])))))))

      (set-who! latin-1-codec
        (let ()
          (define codec
            (make-codec
              [name "latin-1"]
              [make-info
               (lambda (who tx bp tp-buf-len bv)
                 (make-codec-info tx bp bv 0 0
                   (and (input-port? bp)
                        (port-has-port-position? bp)
                        (make-fxvector tp-buf-len))
                   0 #f #f #f #f
                   latin-1-decode latin-1-encode (lambda (info) #f)))]))
          (lambda () codec))))

    (let ()
      (define utf-8-decode
        (let ()
          (define (err who tp info i iend bytes b . b*)
            (codec-info-bom-set! info #f)
            (codec-info-next-set! info i)
            (codec-info-iend-set! info iend)
            (codec-info-ibytes-set! info (fx+ bytes 1 (length b*)))
            (codec-info-icr-set! info #f)
            (decode-oops who tp "invalid utf-8 encoding #x~2,'0x~{, ~a~}" b
              (map (lambda (b) (if (eof-object? b) "#!eof" (format "#x~2,'0x" b))) b*)))
          (define (eof-err who tp info i iend bytes)
            (unless (fx= bytes 0) (codec-info-bom-set! info #f))
            (codec-info-next-set! info iend)
            (codec-info-iend-set! info iend)
            (codec-info-ibytes-set! info (fx+ bytes (fx- iend i)))
            (codec-info-icr-set! info #f)
            (decode-oops who tp "unexpected end-of-file reading multibyte utf-8 encoding"))
          (define (return ans i iend cr? bytes info)
            (unless (fx= bytes 0) (codec-info-bom-set! info #f))
            (codec-info-next-set! info i)
            (codec-info-iend-set! info iend)
            (codec-info-ibytes-set! info bytes)
            (codec-info-icr-set! info cr?)
            ans)
          (lambda (who tp str start count ifready?)
            (let ([info ($port-info tp)])
              (let ([bp (codec-info-bp info)]
                    [ioffsets (and (eq? str (textual-port-input-buffer tp)) (codec-info-ioffsets info))]
                    [bv (codec-info-bv info)]
                    [jend (fx+ start count)])
                (define-syntax decode-error
                  (syntax-rules ()
                    [(_ j i iend bytes b1 b2 ...)
                     (case ($transcoder-error-handling-mode (codec-info-tx info))
                       [(ignore) j]
                       [(replace)
                        (string-set! str j #\xfffd)
                        (when ioffsets (fxvector-set! ioffsets j bytes))
                        (fx+ j 1)]
                       [else (err who tp info i iend bytes b1 b2 ...)])]))
                (define-syntax decode-eof-error
                  (syntax-rules ()
                    [(_ j i iend bytes)
                     (case ($transcoder-error-handling-mode (codec-info-tx info))
                       [(ignore) (return #!eof iend iend #f (fx+ bytes (fx- iend i)) info)]
                       [(replace)
                        (string-set! str j #\xfffd)
                        (when ioffsets (fxvector-set! ioffsets j bytes))
                        (return (fx- (fx+ j 1) start) iend iend #f (fx+ bytes (fx- iend i)) info)]
                       [else (eof-err who tp info i iend bytes)])]))
                (let loop ([j start]
                           [i (codec-info-next info)]
                           [iend (codec-info-iend info)]
                           [cr? (codec-info-icr info)]
                           [bytes 0])
                  (cond
                    [(fx= j jend) (pariah (return count i iend cr? bytes info))]
                    [(fx= i iend)
                     (pariah
                       (if (fx= j start)
                           (let ([n (get-some-maybe-nb who bp bv 0 ifready?)])
                             (cond
                               [(eof-object? n) (return #!eof 0 0 #f bytes info)]
                               [(fx= n 0) (return 0 0 0 cr? bytes info)]
                               [else (loop j 0 n cr? bytes)]))
                           ; don't try to read in this case to avoid dealing with eof
                           (return (fx- j start) i iend cr? bytes info)))]
                    [else
                     (let ([b1 (bytevector-u8-ref bv i)])
                       (cond
                         [(fx<= b1 #x7f) ; one-byte encoding
                          (cond
                            [(fx= b1 #x0d)
                             (pariah
                               (cond
                                 [(eq? ($transcoder-eol-style (codec-info-tx info)) 'none)
                                  (string-set! str j #\return)
                                  (when ioffsets (fxvector-set! ioffsets j bytes))
                                  (loop (fx+ j 1) (fx+ i 1) iend cr? (fx+ bytes 1))]
                                 [else
                                  (string-set! str j #\newline)
                                  (when ioffsets (fxvector-set! ioffsets j bytes))
                                  (loop (fx+ j 1) (fx+ i 1) iend #t (fx+ bytes 1))]))]
                            [(fx= b1 #x0a)
                             (pariah
                               (cond
                                 [cr? (loop j (fx+ i 1) iend #f (fx+ bytes 1))]
                                 [else
                                  (string-set! str j #\newline)
                                  (when ioffsets (fxvector-set! ioffsets j bytes))
                                  (loop (fx+ j 1) (fx+ i 1) iend cr? (fx+ bytes 1))]))]
                            [else
                             (string-set! str j (integer->char b1))
                             (when ioffsets (fxvector-set! ioffsets j bytes))
                             (loop (fx+ j 1) (fx+ i 1) iend #f (fx+ bytes 1))])]
                         [else
                          (pariah
                            (cond
                              [(fx<= #xc2 b1 #xdf) ; two-byte encoding
                               (let f ([i i] [iend iend])
                                 (if (fx< (fx+ i 1) iend) ; have at least two bytes?
                                     (let ([b2 (bytevector-u8-ref bv (fx+ i 1))])
                                       (if (fx= (fxsrl b2 6) #b10) ; second byte a continuation byte?
                                           (let ([x (fxlogor (fxsll (fxlogand b1 #b11111) 6) (fxlogand b2 #b111111))]
                                                 [i (fx+ i 2)])
                                             (cond
                                               [(fx= x #x85) ; NEL
                                                (cond
                                                  [cr? (loop j i iend #f (fx+ bytes 2))]
                                                  [else
                                                   (string-set! str j
                                                     (if (eq? ($transcoder-eol-style (codec-info-tx info)) 'none)
                                                         (integer->char #x85)
                                                         #\newline))
                                                   (when ioffsets (fxvector-set! ioffsets j bytes))
                                                   (loop (fx+ j 1) i iend cr? (fx+ bytes 2))])]
                                               [else
                                                (string-set! str j (integer->char x))
                                                (when ioffsets (fxvector-set! ioffsets j bytes))
                                                (loop (fx+ j 1) i iend #f (fx+ bytes 2))]))
                                          ; second byte is not a continuation byte
                                           (let ([j (decode-error j (fx+ i 1) iend bytes b1)])
                                             (loop j (fx+ i 1) iend #f (fx+ bytes 1)))))
                                    ; have only one byte
                                     (begin
                                       (bytevector-u8-set! bv 0 b1)
                                       (let ([i 0] [iend 1])
                                         (let ([n (get-some-maybe-nb who bp bv iend ifready?)])
                                           (cond
                                             [(eof-object? n) (decode-eof-error j i iend bytes)]
                                             [(fx= n 0) (return (fx- j start) i iend cr? bytes info)]
                                             [else (f i (fx+ iend n))]))))))]
                             [(fx<= #xe0 b1 #xef) ; three-byte encoding
                              (let f ([i i] [iend iend])
                                (if (fx< (fx+ i 1) iend) ; have at least two bytes?
                                    (let ([b2 (bytevector-u8-ref bv (fx+ i 1))])
                                      (if (fx= (fxsrl b2 6) #b10) ; second byte a continuation byte?
                                          (if (fx< (fx+ i 2) iend) ; have at least three bytes?
                                              (let ([b3 (bytevector-u8-ref bv (fx+ i 2))])
                                                (if (fx= (fxsrl b3 6) #b10) ; third byte a continuation byte?
                                                    (let ([x (fxlogor
                                                               (fxsll (fxlogand b1 #b1111) 12)
                                                               (fxsll (fxlogand b2 #b111111) 6)
                                                               (fxlogand b3 #b111111))]
                                                          [i (fx+ i 3)])
                                                      (cond
                                                        [(and (fx= x #xfeff) (fx= bytes 0) (codec-info-bom info))
                                                         (loop j i iend #f (fx+ bytes 3))]
                                                        [(and (fx>= x #x800) (not (fx<= #xd800 x #xdfff)))
                                                         (string-set! str j
                                                           (if (and (fx= x #x2028) ; LS
                                                                    (not (eq? ($transcoder-eol-style (codec-info-tx info)) 'none)))
                                                               #\newline
                                                               (integer->char x)))
                                                         (when ioffsets (fxvector-set! ioffsets j bytes))
                                                         (loop (fx+ j 1) i iend #f (fx+ bytes 3))]
                                                        [else
                                                         (let ([j (decode-error j i iend bytes b1 b2 b3)])
                                                           (loop j i iend #f (fx+ bytes 3)))]))
                                                   ; third byte is not a continuation byte
                                                    (let ([j (decode-error j (fx+ i 2) iend bytes b1 b2)])
                                                      (loop j (fx+ i 2) iend #f (fx+ bytes 2)))))
                                             ; have only two bytes
                                              (begin
                                                (bytevector-u8-set! bv 0 b1)
                                                (bytevector-u8-set! bv 1 b2)
                                                (let ([i 0] [iend 2])
                                                  (let ([n (get-some-maybe-nb who bp bv iend ifready?)])
                                                    (cond
                                                      [(eof-object? n) (decode-eof-error j i iend bytes)]
                                                      [(fx= n 0) (return (fx- j start) i iend cr? bytes info)]
                                                      [else (f i (fx+ iend n))])))))
                                         ; second byte is not a continuation byte
                                          (let ([j (decode-error j (fx+ i 1) iend bytes b1)])
                                            (loop j (fx+ i 1) iend #f (fx+ bytes 1)))))
                                   ; have only one byte
                                    (begin
                                      (bytevector-u8-set! bv 0 b1)
                                      (let ([i 0] [iend 1])
                                        (let ([n (get-some-maybe-nb who bp bv iend ifready?)])
                                          (cond
                                            [(eof-object? n) (decode-eof-error j i iend bytes)]
                                            [(fx= n 0) (return (fx- j start) i iend cr? bytes info)]
                                            [else (f i (fx+ iend n))]))))))]
                             [(fx<= #xf0 b1 #xf4) ; four-byte encoding
                              (let f ([i i] [iend iend])
                                (if (fx< (fx+ i 1) iend) ; have at least two bytes?
                                    (let ([b2 (bytevector-u8-ref bv (fx+ i 1))])
                                      (if (fx= (fxsrl b2 6) #b10) ; second byte a continuation byte?
                                          (if (fx< (fx+ i 2) iend) ; have at least three bytes?
                                              (let ([b3 (bytevector-u8-ref bv (fx+ i 2))])
                                                (if (fx= (fxsrl b3 6) #b10) ; third byte a continuation byte?
                                                    (if (fx< (fx+ i 3) iend) ; have at least four bytes?
                                                        (let ([b4 (bytevector-u8-ref bv (fx+ i 3))])
                                                          (if (fx= (fxsrl b4 6) #b10) ; fourth byte a continuation byte?
                                                              (let ([x (fxlogor
                                                                         (fxsll (fxlogand b1 #b111) 18)
                                                                         (fxsll (fxlogand b2 #b111111) 12)
                                                                         (fxsll (fxlogand b3 #b111111) 6)
                                                                         (fxlogand b4 #b111111))]
                                                                    [i (fx+ i 4)])
                                                                (cond
                                                                  [(fx<= #x10000 x #x10ffff)
                                                                   (string-set! str j (integer->char x))
                                                                   (when ioffsets (fxvector-set! ioffsets j bytes))
                                                                   (loop (fx+ j 1) i iend #f (fx+ bytes 4))]
                                                                  [else
                                                                   (let ([j (decode-error j i iend bytes b1 b2 b3)])
                                                                     (loop j i iend #f (fx+ bytes 3)))]))
                                                             ; fourth byte is not a continuation byte
                                                              (let ([j (decode-error j (fx+ i 3) iend bytes b1 b2 b3)])
                                                                (loop j (fx+ i 3) iend #f (fx+ bytes 3)))))
                                                       ; have only three bytes
                                                        (begin
                                                          (bytevector-u8-set! bv 0 b1)
                                                          (bytevector-u8-set! bv 1 b2)
                                                          (bytevector-u8-set! bv 2 b3)
                                                          (let ([i 0] [iend 3])
                                                            (let ([n (get-some-maybe-nb who bp bv iend ifready?)])
                                                              (cond
                                                                [(eof-object? n) (decode-eof-error j i iend bytes)]
                                                                [(fx= n 0) (return (fx- j start) i iend cr? bytes info)]
                                                                [else (f i (fx+ iend n))])))))
                                                   ; third byte is not a continuation byte
                                                    (let ([j (decode-error j (fx+ i 2) iend bytes b1 b2)])
                                                      (loop j (fx+ i 2) iend #f (fx+ bytes 2)))))
                                             ; have only two bytes
                                              (begin
                                                (bytevector-u8-set! bv 0 b1)
                                                (bytevector-u8-set! bv 1 b2)
                                                (let ([i 0] [iend 2])
                                                  (let ([n (get-some-maybe-nb who bp bv iend ifready?)])
                                                    (cond
                                                      [(eof-object? n) (decode-eof-error j i iend bytes)]
                                                      [(fx= n 0) (return (fx- j start) i iend cr? bytes info)]
                                                      [else (f i (fx+ iend n))])))))
                                         ; second byte is not a continuation byte
                                          (let ([j (decode-error j (fx+ i 1) iend bytes b1)])
                                            (loop j (fx+ i 1) iend #f (fx+ bytes 1)))))
                                   ; have only one byte
                                    (begin
                                      (bytevector-u8-set! bv 0 b1)
                                      (let ([i 0] [iend 1])
                                        (let ([n (get-some-maybe-nb who bp bv iend ifready?)])
                                          (cond
                                            [(eof-object? n) (decode-eof-error j i iend bytes)]
                                            [(fx= n 0) (return (fx- j start) i iend cr? bytes info)]
                                            [else (f i (fx+ iend n))]))))))]
                             [else
                              (let ([j (decode-error j (fx+ i 1) iend bytes b1)])
                                (loop j (fx+ i 1) iend #f (fx+ bytes 1)))]))]))])))))))

      (define utf-8-encode
        (let ()
          (define (return ans o info)
            (codec-info-next-set! info o)
            ans)
          (define (write-two-byte bv o x)
            (bytevector-u8-set! bv o (fxlogor #b11000000 (fxsrl x 6)))
            (bytevector-u8-set! bv (fx+ o 1) (fxlogor #b10000000 (fxlogand x #b111111))))
          (define (write-three-byte bv o x)
            (bytevector-u8-set! bv o (fxlogor #b11100000 (fxsrl x 12)))
            (bytevector-u8-set! bv (fx+ o 1) (fxlogor #b10000000 (fxlogand (fxsrl x 6) #b111111)))
            (bytevector-u8-set! bv (fx+ o 2) (fxlogor #b10000000 (fxlogand x #b111111))))
          (lambda (who tp str start count)
            (let ([info ($port-info tp)])
              (codec-info-bom-set! info #f)
              (let ([bp (codec-info-bp info)]
                    [bv (codec-info-bv info)]
                    [jend (fx+ start count)])
                (define codec-buffer-length (bytevector-length bv))
                (let loop ([j start] [o (codec-info-next info)])
                  (cond
                    [(fx= j jend) (return count o info)]
                    [(fx= o codec-buffer-length)
                     (let ([o (flush-buffer who bp bv 0 o)])
                       (if (fx= o codec-buffer-length)
                           (return (fx- j start) o info)
                           (loop j o)))]
                    [else
                     (let ([x (char->integer (string-ref str j))])
                       (cond
                         [(fx= x #x0a)
                          (let ([eol-style ($transcoder-eol-style (codec-info-tx info))])
                            (case eol-style
                              [(lf none)
                               (bytevector-u8-set! bv o #x0a)
                               (loop (fx+ j 1) (fx+ o 1))]
                              [(cr)
                               (bytevector-u8-set! bv o #x0d)
                               (loop (fx+ j 1) (fx+ o 1))]
                              [(crlf nel)
                               (let f ([o o])
                                 (if (fx< o (fx- codec-buffer-length 1))
                                     (begin
                                       (case eol-style
                                         [(crlf)
                                          (bytevector-u8-set! bv o #x0d)
                                          (bytevector-u8-set! bv (fx+ o 1) #x0a)]
                                         [else (write-two-byte bv o #x85)])
                                       (loop (fx+ j 1) (fx+ o 2)))
                                     (let ([new-o (flush-buffer who bp bv 0 o)])
                                       (if (fx= new-o o)
                                           (return (fx- j start) o info)
                                           (f new-o)))))]
                              [(crnel ls)
                               (let f ([o o])
                                 (if (fx< o (fx- codec-buffer-length 2))
                                     (begin
                                       (case eol-style
                                         [(crnel)
                                          (bytevector-u8-set! bv o #x0d)
                                          (write-two-byte bv (fx+ o 1) #x85)]
                                         [else (write-three-byte bv o #x2028)])
                                       (loop (fx+ j 1) (fx+ o 3)))
                                     (let ([new-o (flush-buffer who bp bv 0 o)])
                                       (if (fx= new-o o)
                                           (return (fx- j start) o info)
                                           (f new-o)))))]
                              [else ($oops who "unrecognized eol style ~s" eol-style)]))]
                         [(fx<= x #x7f) ; one-byte encoding
                          (bytevector-u8-set! bv o x)
                          (loop (fx+ j 1) (fx+ o 1))]
                         [(fx<= x #x7ff) ; two-byte encoding
                          (let f ([o o])
                            (if (fx< o (fx- codec-buffer-length 1))
                                (begin
                                  (write-two-byte bv o x)
                                  (loop (fx+ j 1) (fx+ o 2)))
                                (let ([new-o (flush-buffer who bp bv 0 o)])
                                  (if (fx= new-o o)
                                      (return (fx- j start) o info)
                                      (f new-o)))))]
                         [(fx<= x #xffff) ; three-byte encoding
                          (let f ([o o])
                            (if (fx< o (fx- codec-buffer-length 2))
                                (begin
                                  (write-three-byte bv o x)
                                  (loop (fx+ j 1) (fx+ o 3)))
                                (let ([new-o (flush-buffer who bp bv 0 o)])
                                  (if (fx= new-o o)
                                      (return (fx- j start) o info)
                                      (f new-o)))))]
                         [else ; four-byte encoding
                          (let f ([o o])
                            (if (fx< o (fx- codec-buffer-length 3))
                                (begin
                                  (bytevector-u8-set! bv o (fxlogor #b11110000 (fxsrl x 18)))
                                  (bytevector-u8-set! bv (fx+ o 1) (fxlogor #b10000000 (fxlogand (fxsrl x 12) #b111111)))
                                  (bytevector-u8-set! bv (fx+ o 2) (fxlogor #b10000000 (fxlogand (fxsrl x 6) #b111111)))
                                  (bytevector-u8-set! bv (fx+ o 3) (fxlogor #b10000000 (fxlogand x #b111111)))
                                  (loop (fx+ j 1) (fx+ o 4)))
                                (let ([new-o (flush-buffer who bp bv 0 o)])
                                  (if (fx= new-o o)
                                      (return (fx- j start) o info)
                                      (f new-o)))))]))])))))))

      (set-who! utf-8-codec
        (let ()
          (define codec
            (make-codec
              [name "utf-8"]
              [make-info
               (lambda (who tx bp tp-buf-len bv)
                 (make-codec-info tx bp bv 0 0
                   (and (input-port? bp)
                        (port-has-port-position? bp)
                        (make-fxvector tp-buf-len))
                   0 #f #t #f #f
                   utf-8-decode utf-8-encode (lambda (info) #f)))]))
          (lambda () codec))))

    (let ()
      (define utf-16-decode
        (let ()
          (define (err who tp info i iend bytes b . b*)
            (codec-info-bom-set! info #f)
            (codec-info-next-set! info i)
            (codec-info-iend-set! info iend)
            (codec-info-ibytes-set! info (fx+ bytes 1 (length b*)))
            (codec-info-icr-set! info #f)
            (decode-oops who tp "invalid utf-16 encoding #x~2,'0x~{, ~a~}" b
              (map (lambda (b) (if (eof-object? b) "#!eof" (format "#x~2,'0x" b))) b*)))
          (define (eof-err who tp info i iend bytes)
            (unless (fx= bytes 0) (codec-info-bom-set! info #f))
            (codec-info-next-set! info iend)
            (codec-info-iend-set! info iend)
            (codec-info-ibytes-set! info (fx+ bytes (fx- iend i)))
            (codec-info-icr-set! info #f)
            (decode-oops who tp "unexpected end-of-file reading two-word utf-16 encoding"))
          (define (return ans i iend cr? bytes info)
            (unless (fx= bytes 0) (codec-info-bom-set! info #f))
            (codec-info-next-set! info i)
            (codec-info-iend-set! info iend)
            (codec-info-ibytes-set! info bytes)
            (codec-info-icr-set! info cr?)
            ans)
          (lambda (who tp str start count ifready?)
            (let ([info ($port-info tp)])
              (let ([bp (codec-info-bp info)]
                    [ioffsets (and (eq? str (textual-port-input-buffer tp)) (codec-info-ioffsets info))]
                    [bv (codec-info-bv info)]
                    [jend (fx+ start count)])
                (define-syntax decode-error
                  (syntax-rules ()
                    [(_ j i iend bytes b1 b2 ...)
                     (case ($transcoder-error-handling-mode (codec-info-tx info))
                       [(ignore) j]
                       [(replace)
                        (string-set! str j #\xfffd)
                        (when ioffsets (fxvector-set! ioffsets j bytes))
                        (fx+ j 1)]
                       [else (err who tp info i iend bytes b1 b2 ...)])]))
                (define-syntax decode-eof-error
                  (syntax-rules ()
                    [(_ j i iend bytes)
                     (case ($transcoder-error-handling-mode (codec-info-tx info))
                       [(ignore) (return #!eof iend iend #f (fx+ bytes (fx- iend i)) info)]
                       [(replace)
                        (string-set! str j #\xfffd)
                        (when ioffsets (fxvector-set! ioffsets j bytes))
                        (return (fx- (fx+ j 1) start) iend iend #f (fx+ bytes (fx- iend i)) info)]
                       [else (eof-err who tp info i iend bytes)])]))
                (let loop ([j start]
                           [i (codec-info-next info)]
                           [iend (codec-info-iend info)]
                           [cr? (codec-info-icr info)]
                           [bytes 0])
                  (cond
                    [(fx= j jend) (return count i iend cr? bytes info)]
                    [(fx= i iend)
                     (if (fx= j start)
                         (let ([n (get-some-maybe-nb who bp bv 0 ifready?)])
                           (cond
                             [(eof-object? n) (return #!eof 0 0 #f bytes info)]
                             [(fx= n 0) (return 0 0 0 cr? bytes info)]
                             [else (loop j 0 n cr? bytes)]))
                        ; don't try to read in this case to avoid dealing with eof
                         (return (fx- j start) i iend cr? bytes info))]
                    [(fx= i (fx- iend 1))
                     (bytevector-u8-set! bv 0 (bytevector-u8-ref bv i))
                     (let ([n (get-some-maybe-nb who bp bv 1 ifready?)])
                       (cond
                         [(eof-object? n) (decode-eof-error j 0 1 bytes)]
                         [(fx= n 0) (return (fx- j start) 0 1 cr? bytes info)]
                         [else (loop j 0 (fx+ n 1) cr? bytes)]))]
                    [else
                     (let ([b1 (bytevector-u8-ref bv i)]
                           [b2 (bytevector-u8-ref bv (fx+ i 1))])
                       (let ([w1 (if (codec-info-big info)
                                     (fxlogor (fxsll b1 8) b2)
                                     (fxlogor (fxsll b2 8) b1))])
                         (cond
                           [(and (fx= w1 #xfeff) (fx= i 0) (codec-info-bom info))
                            (when (and (port-has-port-position? bp)
                                       (guard (c [#t #f])
                                         (let ([n (port-position bp)])
                                           (eq? (- n iend) 0))))
                               (codec-info-zbom-set! info #t))
                            (loop j (fx+ i 2) iend cr? (fx+ bytes 2))]
                           [(and (fx= w1 #xfffe) (fx= i 0) (codec-info-bom info))
                            (when (and (port-has-port-position? bp)
                                       (guard (c [#t #f])
                                         (let ([n (port-position bp)])
                                           (eq? (- n iend) 0))))
                               (codec-info-zbom-set! info #t))
                            (codec-info-big-set! info (not (codec-info-big info)))
                            (loop j (fx+ i 2) iend cr? (fx+ bytes 2))]
                           [(fx<= #xD800 w1 #xDBFF) ; two-word encoding
                            (cond
                              [(fx<= i (fx- iend 4))
                               (let ([b3 (bytevector-u8-ref bv (fx+ i 2))]
                                     [b4 (bytevector-u8-ref bv (fx+ i 3))])
                                 (let ([w2 (if (codec-info-big info)
                                               (fxlogor (fxsll b3 8) b4)
                                               (fxlogor (fxsll b4 8) b3))])
                                   (cond
                                     [(fx<= #xDC00 w2 #xDFFF) ; valid encoding
                                      (string-set! str j
                                        (integer->char
                                          (fx+ (fxlogor (fxsll (fx- w1 #xD800) 10) (fx- w2 #xDC00))
                                               #x10000)))
                                      (loop (fx+ j 1) (fx+ i 4) iend #f (fx+ bytes 4))]
                                     [else
                                      (let ([i (fx+ i 4)])
                                        (let ([j (decode-error j i iend bytes b1 b2 b3 b4)])
                                          (loop j i iend #f (fx+ bytes 4))))])))]
                              [(fx= i (fx- iend 2))
                               (bytevector-u8-set! bv 0 b1)
                               (bytevector-u8-set! bv 1 b2)
                               (let ([n (get-some-maybe-nb who bp bv 2 ifready?)])
                                 (cond
                                   [(eof-object? n) (decode-eof-error j 0 2 bytes)]
                                   [(fx= n 0) (return (fx- j start) 0 2 cr? bytes info)]
                                   [else (loop j 0 (fx+ n 2) cr? bytes)]))]
                              [else ; must have three bytes of the four we need
                               (bytevector-u8-set! bv 0 b1)
                               (bytevector-u8-set! bv 1 b2)
                               (bytevector-u8-set! bv 2 (bytevector-u8-ref bv (fx+ i 2)))
                               (let ([n (get-some-maybe-nb who bp bv 3 ifready?)])
                                 (cond
                                   [(eof-object? n) (decode-eof-error j 0 3 bytes)]
                                   [(fx= n 0) (return (fx- j start) 0 3 cr? bytes info)]
                                   [else (loop j 0 (fx+ n 3) cr? bytes)]))])]
                           [(fx<= #xDC00 w1 #xDFFF) ; bogus encoding
                            (let ([i (fx+ i 2)])
                              (let ([j (decode-error j i iend bytes b1 b2)])
                                (loop j i iend #f (fx+ bytes 2))))]
                           [(fx= w1 #x0d)
                            (cond
                              [(eq? ($transcoder-eol-style (codec-info-tx info)) 'none)
                               (string-set! str j #\return)
                               (when ioffsets (fxvector-set! ioffsets j bytes))
                               (loop (fx+ j 1) (fx+ i 2) iend cr? (fx+ bytes 2))]
                              [else
                               (string-set! str j #\newline)
                               (when ioffsets (fxvector-set! ioffsets j bytes))
                               (loop (fx+ j 1) (fx+ i 2) iend #t (fx+ bytes 2))])]
                           [(fx= w1 #x0a) ; LF
                            (cond
                              [cr? (loop j (fx+ i 2) iend #f (fx+ bytes 2))]
                              [else
                               (string-set! str j #\newline)
                               (when ioffsets (fxvector-set! ioffsets j bytes))
                               (loop (fx+ j 1) (fx+ i 2) iend cr? (fx+ bytes 2))])]
                           [(fx= w1 #x85) ; NEL
                            (cond
                              [cr? (loop j (fx+ i 2) iend #f (fx+ bytes 2))]
                              [else
                               (string-set! str j
                                 (if (eq? ($transcoder-eol-style (codec-info-tx info)) 'none)
                                     (integer->char w1)
                                     #\newline))
                               (when ioffsets (fxvector-set! ioffsets j bytes))
                               (loop (fx+ j 1) (fx+ i 2) iend cr? (fx+ bytes 2))])]
                           [(fx= w1 #x2028) ; LS
                            (string-set! str j
                              (if (eq? ($transcoder-eol-style (codec-info-tx info)) 'none)
                                  (integer->char w1)
                                  #\newline))
                            (when ioffsets (fxvector-set! ioffsets j bytes))
                            (loop (fx+ j 1) (fx+ i 2) iend #f (fx+ bytes 2))]
                           [else
                            (string-set! str j (integer->char w1))
                            (when ioffsets (fxvector-set! ioffsets j bytes))
                            (loop (fx+ j 1) (fx+ i 2) iend #f (fx+ bytes 2))])))])))))))

      (define utf-16-encode
        (let ()
          (define (return ans o info)
            (codec-info-next-set! info o)
            ans)
          (lambda (who tp str start count)
            (let ([info ($port-info tp)])
              (let ([bp (codec-info-bp info)]
                    [bv (codec-info-bv info)]
                    [jend (fx+ start count)])
                (define codec-buffer-length (bytevector-length bv))
                (when (codec-info-bom info)
                  (codec-info-bom-set! info #f)
                  (when (and (port-has-port-position? bp)
                             (guard (c [#t #f])
                               (eq? (port-position bp) 0)))
                     (codec-info-zbom-set! info #t))
                  (call-port-handler put-some who bp
                    (if (codec-info-big info) #vu8(#xfe #xff) #vu8(#xff #xfe))
                    0 2))
                (let loop ([j start] [o (codec-info-next info)])
                  (cond
                    [(fx= j jend) (return count o info)]
                    [(fx>= o (fx- codec-buffer-length 1))
                     (let ([new-o (flush-buffer who bp bv 0 o)])
                       (if (fx= new-o o)
                           (return (fx- j start) o info)
                           (loop j new-o)))]
                    [else
                     (let ([x (char->integer (string-ref str j))])
                       (cond
                         [(fx= x #x0a)
                          (let ([eol-style ($transcoder-eol-style (codec-info-tx info))])
                            (case eol-style
                              [(lf none)
                               (cond
                                 [(codec-info-big info)
                                  (bytevector-u8-set! bv o #x0)
                                  (bytevector-u8-set! bv (fx+ o 1) #x0a)]
                                 [else
                                  (bytevector-u8-set! bv (fx+ o 1) #x0)
                                  (bytevector-u8-set! bv o #x0a)])
                               (loop (fx+ j 1) (fx+ o 2))]
                              [(cr)
                               (cond
                                 [(codec-info-big info)
                                  (bytevector-u8-set! bv o #x0)
                                  (bytevector-u8-set! bv (fx+ o 1) #x0d)]
                                 [else
                                  (bytevector-u8-set! bv (fx+ o 1) #x0)
                                  (bytevector-u8-set! bv o #x0d)])
                               (loop (fx+ j 1) (fx+ o 2))]
                              [(nel)
                               (cond
                                 [(codec-info-big info)
                                  (bytevector-u8-set! bv o #x0)
                                  (bytevector-u8-set! bv (fx+ o 1) #x85)]
                                 [else
                                  (bytevector-u8-set! bv (fx+ o 1) #x0)
                                  (bytevector-u8-set! bv o #x85)])
                               (loop (fx+ j 1) (fx+ o 2))]
                              [(ls)
                               (cond
                                 [(codec-info-big info)
                                  (bytevector-u8-set! bv o #x20)
                                  (bytevector-u8-set! bv (fx+ o 1) #x28)]
                                 [else
                                  (bytevector-u8-set! bv (fx+ o 1) #x20)
                                  (bytevector-u8-set! bv o #x28)])
                               (loop (fx+ j 1) (fx+ o 2))]
                              [(crlf crnel)
                               (if (fx< o (fx- codec-buffer-length 3))
                                   (begin
                                     (cond
                                       [(codec-info-big info)
                                        (bytevector-u8-set! bv o #x0)
                                        (bytevector-u8-set! bv (fx+ o 1) #x0d)
                                        (bytevector-u8-set! bv (fx+ o 2) #x0)
                                        (bytevector-u8-set! bv (fx+ o 3)
                                          (case eol-style [(crlf) #x0a] [(crnel) #x85]))]
                                       [else
                                        (bytevector-u8-set! bv (fx+ o 1) #x0)
                                        (bytevector-u8-set! bv o #x0d)
                                        (bytevector-u8-set! bv (fx+ o 3) #x0)
                                        (bytevector-u8-set! bv (fx+ o 2)
                                          (case eol-style [(crlf) #x0a] [(crnel) #x85]))])
                                     (loop (fx+ j 1) (fx+ o 4)))
                                   (let ([new-o (flush-buffer who bp bv 0 o)])
                                     (if (fx= new-o o)
                                         (return (fx- j start) o info)
                                         (loop j new-o))))]
                              [else ($oops who "unrecognized eol style ~s" eol-style)]))]
                         [(fx<= x #xffff) ; two-byte encoding
                          (cond
                            [(codec-info-big info)
                             (bytevector-u8-set! bv o (fxsrl x 8))
                             (bytevector-u8-set! bv (fx+ o 1) (fxand x #xff))]
                            [else
                             (bytevector-u8-set! bv (fx+ o 1) (fxsrl x 8))
                             (bytevector-u8-set! bv o (fxand x #xff))])
                          (loop (fx+ j 1) (fx+ o 2))]
                         [else ; four-byte encoding
                          (if (fx< o (fx- codec-buffer-length 3))
                              (let ([x (fx- x #x10000)])
                                (let ([w1 (fxior #xd800 (fxsrl x 10))]
                                      [w2 (fxior #xdc00 (fxand x #x3ff))])
                                  (cond
                                    [(codec-info-big info)
                                     (bytevector-u8-set! bv o (fxsrl w1 8))
                                     (bytevector-u8-set! bv (fx+ o 1) (fxand w1 #xff))
                                     (bytevector-u8-set! bv (fx+ o 2) (fxsrl w2 8))
                                     (bytevector-u8-set! bv (fx+ o 3) (fxand w2 #xff))]
                                    [else
                                     (bytevector-u8-set! bv (fx+ o 1) (fxsrl w1 8))
                                     (bytevector-u8-set! bv o (fxand w1 #xff))
                                     (bytevector-u8-set! bv (fx+ o 3) (fxsrl w2 8))
                                     (bytevector-u8-set! bv (fx+ o 2) (fxand w2 #xff))])
                                  (loop (fx+ j 1) (fx+ o 4))))
                              (let ([new-o (flush-buffer who bp bv 0 o)])
                                (if (fx= new-o o)
                                    (return (fx- j start) o info)
                                    (loop j new-o))))]))])))))))

      (define make-utf-16-codec
        (lambda (bom big)
          (make-codec
            [name "utf-16"]
            [make-info
             (lambda (who tx bp tp-buf-len bv)
               (make-codec-info tx bp bv 0 0
                 (and (input-port? bp)
                      (port-has-port-position? bp)
                      (make-fxvector tp-buf-len))
                 0 #f bom #f big
                 utf-16-decode utf-16-encode (lambda (info) #f)))])))

      (let ([codec-bom-be (make-utf-16-codec #t #t)]
            [codec-bom-le (make-utf-16-codec #t #f)])
        (set-who! #(r6rs: utf-16-codec)
          (lambda () codec-bom-be))
        (set-who! utf-16-codec
          (case-lambda
            [() codec-bom-be]
            [(eness)
             (unless (memq eness '(big little)) ($oops who "invalid endianness ~s" eness))
             (if (eq? eness 'big) codec-bom-be codec-bom-le)])))

      (set-who! utf-16le-codec
        (let ([codec (make-utf-16-codec #f #f)])
          (lambda () codec)))

      (set-who! utf-16be-codec
        (let ([codec (make-utf-16-codec #f #t)])
          (lambda () codec))))

    (when-feature iconv
      (let ()
        (define-record-type iconv-info
          (parent codec-info)
          (nongenerative)
          (opaque #t)
          (fields decode-desc encode-desc))

        (define $iconv-open (foreign-procedure "(cs)s_iconv_open" (string string) ptr))
        (define $iconv-close (foreign-procedure "(cs)s_iconv_close" (uptr) void))
        (define $iconv-from-string (foreign-procedure "(cs)s_iconv_from_string" (uptr ptr uptr uptr ptr uptr uptr) ptr))
        (define $iconv-to-string (foreign-procedure "(cs)s_iconv_to_string" (uptr ptr uptr uptr ptr uptr uptr) ptr))

        (define iconv-decode
          (let ()
            (define (err who tp info i iend bv)
              (codec-info-next-set! info i)
              (codec-info-iend-set! info iend)
              (codec-info-icr-set! info #f)
              (let ([ls (let f ([k 4] [i i])
                          (if (fx= k 0)
                              (list "etc")
                              (if (fx= i iend)
                                  (list "#!eof")
                                  (cons (format "#x~2,'0x" (bytevector-u8-ref bv i))
                                        (f (fx- k 1) (fx+ i 1))))))])
                (decode-oops who tp "decoding failed for byte sequence ~a~{, ~a~}" (car ls) (cdr ls))))
            (define (return-count str start count i iend info)
              (let ([eol-style ($transcoder-eol-style (codec-info-tx info))])
                (if (eq? eol-style 'none)
                    (return count i iend info)
                    (let ([end (fx+ start count)])
                      (let loop ([jold start] [jnew start] [cr? (codec-info-icr info)])
                        (if (fx= jold end)
                            (return/cr (fx- jnew start) i iend cr? info)
                            (let ([c (string-ref str jold)])
                              (case c
                                [(#\nel #\newline)
                                 (if cr?
                                     (loop (fx+ jold 1) jnew #f)
                                     (begin
                                       (string-set! str jnew #\newline)
                                       (loop (fx+ jold 1) (fx+ jnew 1) #f)))]
                                [(#\return)
                                 (string-set! str jnew #\newline)
                                 (loop (fx+ jold 1) (fx+ jnew 1) #t)]
                                [(#\ls)
                                 (string-set! str jnew #\newline)
                                 (loop (fx+ jold 1) (fx+ jnew 1) #f)]
                                [else
                                 (string-set! str jnew c)
                                 (loop (fx+ jold 1) (fx+ jnew 1) #f)]))))))))
            (define (return/cr ans i iend cr? info)
              (codec-info-icr-set! info cr?)
              (return ans i iend info))
            (define (return ans i iend info)
              (codec-info-next-set! info i)
              (codec-info-iend-set! info iend)
              ans)
            (lambda (who tp str start count ifready?)
              (let ([info ($port-info tp)])
                (let ([bp (codec-info-bp info)]
                      [bv (codec-info-bv info)]
                      [jend (fx+ start count)])
                  (let loop ([j start]
                             [i (codec-info-next info)]
                             [iend (codec-info-iend info)])
                    (cond
                      [(fx= j jend) (return-count str start count i iend info)]
                      [(fx= i iend)
                       (if (fx= j start)
                           (let ([n (get-some-maybe-nb who bp bv 0 ifready?)])
                             (cond
                               [(eof-object? n) (return/cr #!eof i iend #f info)]
                               [(fx= n 0) (return 0 i iend info)]
                               [else (loop j 0 n)]))
                          ; don't try to read in this case to avoid dealing with eof
                           (return-count str start (fx- j start) i iend info))]
                      [else
                       (let ([newi.newj ($iconv-to-string (iconv-info-decode-desc info) bv i iend str j jend)])
                         (cond
                           [(pair? newi.newj) (loop (cdr newi.newj) (car newi.newj) iend)]
                          ; one of the following presumably happened:
                          ;  - too few input bytes to make progress
                          ;  - invalid input sequence found
                          ; assuming problem can't have been too little output space since
                          ; j != jend implies enough room for at least one character
                           [(or (eq? newi.newj (constant SICONV-INVALID))
                               ; assuming bv is large enough to hold any valid encoding sequence
                                (and (eq? newi.newj (constant SICONV-DUNNO))
                                     (and (fx= i 0) (fx= iend (bytevector-length bv)))))
                            (case ($transcoder-error-handling-mode (codec-info-tx info))
                              [(ignore) (loop j (fx+ i 1) iend)]
                              [(replace)
                               (string-set! str j #\xfffd)
                               (loop (fx+ j 1) (fx+ i 1) iend)]
                              [else (err who tp info i iend bv)])]
                           [else
                            ; try again with more bytes
                            (unless (fx= i 0) (bytevector-copy! bv i bv 0 (fx- iend i)))
                            (let ([i 0] [iend (fx- iend i)])
                              (let ([n (get-some-maybe-nb who bp bv iend ifready?)])
                                (cond
                                  [(eof-object? n)
                                   (set-port-eof! bp #t)
                                   (case ($transcoder-error-handling-mode (codec-info-tx info))
                                     [(ignore) (loop j (fx+ i 1) iend)]
                                     [(replace)
                                      (string-set! str j #\xfffd)
                                      (loop (fx+ j 1) (fx+ i 1) iend)]
                                     [else (err who tp info i iend bv)])]
                                  [(fx= n 0) (return 0 i iend info)]
                                  [else (loop j 0 (fx+ iend n))])))]))])))))))

        (define iconv-encode
          (let ()
            (define (return ans o info)
              (codec-info-next-set! info o)
              ans)
            (define (do-iconv who info str j jend bv o codec-buffer-length)
              (let ([eol-style ($transcoder-eol-style (codec-info-tx info))]
                    [desc (iconv-info-encode-desc info)])
                (cond
                  [(memq eol-style '(none lf))
                   ($iconv-from-string desc str j jend bv o codec-buffer-length)]
                  [(eqv? (string-ref str j) #\newline)
                   (let ()
                     (define (iconv-newline s k)
                       (let ([newj.newo ($iconv-from-string desc s 0 k bv o codec-buffer-length)])
                         (if (pair? newj.newo)
                             (if (fx= (car newj.newo) k)
                                 (cons (fx+ j 1) (cdr newj.newo))
                                 (constant SICONV-NOROOM))
                             newj.newo)))
                     (case eol-style
                       [(cr) (iconv-newline "\r" 1)]
                       [(nel) (iconv-newline "\x85;" 1)]
                       [(ls) (iconv-newline "\x2028;" 1)]
                       [(crlf) (iconv-newline "\r\n" 2)]
                       [(crnel) (iconv-newline "\r\x85;" 2)]
                       [else ($oops who "unrecognized eol style ~s" eol-style)]))]
                  [else
                   (do ([k (fx+ j 1) (fx+ k 1)])
                       ((or (fx= k jend) (eqv? (string-ref str k) #\newline))
                        ($iconv-from-string desc str j k bv o codec-buffer-length)))])))
            (lambda (who tp str start count)
              (let ([info ($port-info tp)])
                (let ([bp (codec-info-bp info)]
                      [bv (codec-info-bv info)]
                      [jend (fx+ start count)])
                  (define codec-buffer-length (bytevector-length bv))
                  (let loop ([j start] [o (codec-info-next info)])
                    (cond
                      [(fx= j jend) (return count o info)]
                      [(fx= o codec-buffer-length)
                       (let ([o (flush-buffer who bp bv 0 o)])
                         (if (fx= o codec-buffer-length)
                             (return (fx- j start) o info)
                             (loop j o)))]
                      [else
                       (let ([newj.newo (do-iconv who info str j jend bv o codec-buffer-length)])
                         (cond
                           [(pair? newj.newo) (loop (car newj.newo) (cdr newj.newo))]
                          ; one of the following presumably happened:
                          ;  - unencodeable character found
                          ;  - too little output space to make progress
                           [(fx= o 0) ; assuming bv is large enough to hold any valid encoding sequence
                            (case ($transcoder-error-handling-mode (codec-info-tx info))
                              [(ignore) (loop (fx+ j 1) o)]
                              [(replace)
                              ; try to write the Unicode replacement character
                               (let ([newj.newo ($iconv-from-string (iconv-info-encode-desc info) "\xfffd;" 0 1 bv o codec-buffer-length)])
                                 (if (pair? newj.newo)
                                     (loop (fx+ j 1) (cdr newj.newo))
                                    ; if that failed, try to write ?
                                     (let ([newj.newo ($iconv-from-string (iconv-info-encode-desc info) "?" 0 1 bv o codec-buffer-length)])
                                       (if (pair? newj.newo)
                                           (loop (fx+ j 1) (cdr newj.newo))
                                          ; if even that failed, just ignore
                                           (loop (fx+ j 1) o)))))]
                              [else (encode-oops who tp (string-ref str j))])]
                           [else (let ([newo (flush-buffer who bp bv 0 o)])
                                   (if (fx= newo o)
                                       (return (fx- j start) o info)
                                       (loop j newo)))]))])))))))

        (define iconv-close
          (lambda (info)
            (cond [(iconv-info-decode-desc info) => $iconv-close])
            (cond [(iconv-info-encode-desc info) => $iconv-close])))

        (set-who! iconv-codec
          (lambda (code)
            (unless (string? code) ($oops who "~s is not a string" code))
            (make-codec
              [name (format "iconv ~a" code)]
              [make-info
               (lambda (who tx bp tp-buf-len bv)
                 (define UTF-32B/LE
                   (constant-case native-endianness
                     [(little) "UTF-32LE"]
                     [(big) "UTF-32BE"]
                     [(unknown) (case (native-endianness)
                                  [(little) "UTF-32LE"]
                                  [else "UTF-32BE"])]))
                 (define (iconv-open to from)
                   (let ([desc ($iconv-open to from)])
                     (when (string? desc) ($oops who "~a" desc))
                     (unless desc ($oops who "unsupported encoding ~a" code))
                     desc))
                 (let ([decode-desc (and (input-port? bp) (iconv-open UTF-32B/LE code))]
                       [encode-desc (and (output-port? bp) (iconv-open code UTF-32B/LE))])
                   (make-iconv-info tx bp bv 0 0 #f 0 #f #f #f #f
                     (if decode-desc
                         iconv-decode
                         (lambda args ($oops who "unexpected decode from non-input-port ~s" bp)))
                     (if encode-desc
                         iconv-encode
                         (lambda args ($oops who "unexpected encode to non-output-port ~s" bp)))
                     iconv-close decode-desc encode-desc)))]))))))

  ;; eol-style in syntax.ss
  (set-who! $eol-style?
    (lambda (style) (and (memq style (eol-style-list)) #t)))

  (set-who! native-eol-style
    (lambda ()
      (eol-style none)))

  ;; &i/o-decoding in exceptions.ss
  ;; make-i/o-decoding-error in exceptions.ss
  ;; i/o-decoding-error? in exceptions.ss
  ;; &i/o-encoding in exceptions.ss
  ;; make-i/o-encoding-error in exceptions.ss
  ;; i/o-encoding-error? in exceptions.ss
  ;; i/o-encoding-error-char in exceptions.ss

  ;; error-handling-mode in syntax.ss
  (set-who! $error-handling-mode?
    (lambda (mode) (and (memq mode (error-handling-mode-list)) #t)))

  (set-who! make-transcoder
    (rec make-transcoder
      (case-lambda
       [(codec) (make-transcoder codec (native-eol-style) (error-handling-mode replace))]
       [(codec eol-style) (make-transcoder codec eol-style (error-handling-mode replace))]
       [(codec eol-style handling-mode)
        (unless (codec? codec) ($oops who "~s is not a codec" codec))
        (unless ($eol-style? eol-style) ($oops who "~s is not an eol-style" eol-style))
        (unless ($error-handling-mode? handling-mode)
          ($oops who "~s is not an error-handling-mode" handling-mode))
        ($make-transcoder codec eol-style handling-mode)])))

  (set-who! transcoder? (lambda (x) ($transcoder? x)))

  (let ([transcoder (make-transcoder (utf-8-codec))])
    (set-who! native-transcoder (lambda () transcoder))
    (set-who! current-transcoder
      ($make-thread-parameter transcoder
        (lambda (tx)
          (unless ($transcoder? tx) ($oops who "~s is not a transcoder" tx))
          tx))))

  ;; transcoder-codec, transcoder-eol-style, transcoder-error-handling-mode
  (let ()
    (define-syntax define-accessor
      (syntax-rules ()
        [(_ name $name)
         (set-who! name
           (lambda (transcoder)
             (unless ($transcoder? transcoder)
               ($oops who "~s is not a transcoder" transcoder))
             ($name transcoder)))]))

    (define-accessor transcoder-codec $transcoder-codec)
    (define-accessor transcoder-eol-style $transcoder-eol-style)
    (define-accessor transcoder-error-handling-mode $transcoder-error-handling-mode))

;;;; 8.2.5 End-of-file object
  ;; eof-object in prims.ss
  ;; eof-object? in prims.ss

;;;; 8.2.6 Input and output ports
  ;; port? in prims.ss

  (set-who! port-transcoder
    (lambda (port)
      (unless (port? port)
        ($oops who "~s is not a port" port))
      (let ([info ($port-info port)])
        (and (codec-info? info)
             (codec-info-tx info)))))

  ;; textual-port? in prims.ss
  ;; binary-port? in prims.ss

  ;; transcoded-port
  (let ()
    (module (make-transcoded-port-handler)
      (define read-from-codec
        (lambda (who tp str start count ifready?)
          (when (eq? tp $console-input-port)
            (guard (c [else (void)]) (flush-output-port $console-output-port))
            (unless (eq? $console-error-port $console-output-port)
              (guard (c [else (void)]) (flush-output-port $console-error-port))))
          ((codec-info-decode ($port-info tp)) who tp str start count ifready?)))
      (define fill-from-codec
        (lambda (who tp ifready?)
          (let ([buf (textual-port-input-buffer tp)])
            (let ([n (read-from-codec who tp buf 0 (string-length buf) ifready?)])
              (if (eof-object? n)
                  (begin
                    (set-textual-port-input-size! tp 0)
                    (set-port-eof! tp #t))
                  (set-textual-port-input-size! tp n))
              n))))
      (define write-to-codec
        (lambda (who tp str start count)
          (let ([n ((codec-info-encode ($port-info tp)) who tp str start count)])
            (unless (fx= n 0)
              (set-port-bol! tp (eol-char? (string-ref str (fx- (fx+ start n) 1)))))
            n)))
      (define flush-to-codec
        (case-lambda
          [(who tp) (flush-to-codec who tp (textual-port-output-index tp))]
          [(who tp count)
           (unless (fx= count 0)
            ; push the chars from port's buffer into the codec's buffer
             (let loop ([start 0] [count count])
               (let ([n (write-to-codec who tp (textual-port-output-buffer tp) start count)])
                 (unless (fx= n count) (loop (fx+ start n) (fx- count n)))))
             (if ($port-flags-set? tp (constant port-flag-line-buffered))
                 (set-textual-port-output-size! tp 0)
                 (set-textual-port-output-index! tp 0)))]))
      (define try-flush-to-codec
        (lambda (who tp)
          (let ([count (textual-port-output-index tp)])
            (or (fx= count 0)
                (let ([buf (textual-port-output-buffer tp)])
                  (let loop ([start 0] [count count])
                    (let ([n (write-to-codec who tp buf start count)])
                      (cond
                        [(fx= n count)
                         (if ($port-flags-set? tp (constant port-flag-line-buffered))
                             (set-textual-port-output-size! tp 0)
                             (set-textual-port-output-index! tp 0))
                         #t]
                        [(fx= n 0)
                         (unless (fx= start 0)
                           (string-copy! buf start buf 0 count)
                           (when ($port-flags-set? tp (constant port-flag-line-buffered))
                             (set-textual-port-output-size! tp count))
                           (set-textual-port-output-index! tp count))
                         #f]
                        [else (loop (fx+ start n) (fx- count n))]))))))))
      (define flush-from-codec
        (lambda (who tp)
         ; push the bytes from codec's buffer into the binary port
          (let ([info ($port-info tp)])
            (let loop ([start 0] [count (codec-info-next info)])
              (unless (fx= count 0)
                (let ([n (let ([bp (codec-info-bp info)])
                           (call-port-handler put-some who bp (codec-info-bv info) start count))])
                  (loop (fx+ start n) (fx- count n)))))
            (codec-info-next-set! info 0))))
      (define flush-from-bp
        (lambda (who tp)
          (let ([bp (codec-info-bp ($port-info tp))])
            (call-port-handler flush who bp))))
      (module ((make-ready-for-input $make-ready-for-input))
        (define $make-ready-for-input
          (lambda (who tp)
            (flush-to-codec who tp)
            (flush-from-codec who tp)
            (set-textual-port-output-size! tp 0)
            (let ([info ($port-info tp)])
              (codec-info-next-set! info 0)
              (codec-info-iend-set! info 0)
              (codec-info-icr-set! info #f))
            ($set-port-flags! tp (constant port-flag-input-mode))))
        (define-syntax make-ready-for-input
          (syntax-rules ()
            [(_ who ?tp)
             (let ([tp ?tp])
               (unless ($port-flags-set? tp (constant port-flag-input-mode))
                 ($make-ready-for-input who tp)))])))
      (module ((make-ready-for-output $make-ready-for-output))
        (define $make-ready-for-output
          (lambda (who tp)
           ; rewind if textual port or codec has something buffered.
           ; if underlying binary port has something buffered, we'll let
           ; the first write to the binary port take care of it
            (unless (and (fx= (textual-port-input-size tp) 0)
                         (let ([info ($port-info tp)])
                           (fx= (codec-info-next info) (codec-info-iend info))))
              (if (port-handler-port-position ($port-handler tp))
                  (if (port-handler-set-port-position! ($port-handler tp))
                      (let ([bp (codec-info-bp ($port-info tp))])
                        (call-port-handler set-port-position! who bp
                          (call-port-handler port-position who tp)))
                      (position-warning who "cannot set position for write after read on ~s" tp))
                  (position-warning who "cannot determine position for write after read on ~s" tp)))
            (set-textual-port-input-size! tp 0)
            (set-port-eof! tp #f)
            (codec-info-next-set! ($port-info tp) 0)
            (unless ($port-flags-set? tp (constant port-flag-line-buffered))
              (set-textual-port-output-size! tp (fx1- (string-length (textual-port-output-buffer tp)))))
            ($reset-port-flags! tp (constant port-flag-input-mode))))
        (define-syntax make-ready-for-output
          (syntax-rules ()
            [(_ ?who ?tp)
             (let ([tp ?tp])
               (when ($port-flags-set? tp (constant port-flag-input-mode))
                 ($make-ready-for-output ?who tp)))])))
      (define contains-eol-char?
        (lambda (s i end)
          (let f ([i i])
            (and (not (fx= i end))
              (or (eol-char? (string-ref s i))
                  (f (fx+ i 1)))))))
      (define transcoded-port-ready?
        (lambda (who tp)
          (assert-not-closed who tp)
          (make-ready-for-input who tp)
          (or (not (port-input-empty? tp))
              (port-flag-eof-set? tp)
              (not (eq? (fill-from-codec who tp #t) 0)))))
      (define transcoded-port-lookahead
        (lambda (who tp)
          (assert-not-closed who tp)
          (make-ready-for-input who tp)
          (cond
            [(not (port-input-empty? tp))
             (string-ref (textual-port-input-buffer tp)
                         (textual-port-input-index tp))]
            [(port-flag-eof-set? tp) (eof-object)]
            [else (let loop ()
                    (let ([n (fill-from-codec who tp #f)])
                      (cond
                        [(eq? n 0) (loop)]
                        [(eof-object? n) n]
                        [else (string-ref (textual-port-input-buffer tp) 0)])))])))
      (define transcoded-port-unget
        (lambda (who tp x)
          (assert-not-closed who tp)
          (make-ready-for-input who tp)
          (when (port-flag-eof-set? tp) (unget-error who tp x))
          (if (eof-object? x)
              (let ()
                (unless (port-input-empty? tp) (unget-error who tp x))
                (set-port-eof! tp #t))
              (let ([index (textual-port-input-index tp)])
                (when (fx= index 0) (unget-error who tp x))
                (set-textual-port-input-index! tp (fx- index 1))))))
      (define transcoded-port-get
        (lambda (who tp)
          (assert-not-closed who tp)
          (make-ready-for-input who tp)
          (cond
            [(not (port-input-empty? tp))
             (let ([index (textual-port-input-index tp)])
               (set-textual-port-input-index! tp (fx1+ index))
               (string-ref (textual-port-input-buffer tp) index))]
            [(port-flag-eof-set? tp) (set-port-eof! tp #f) (eof-object)]
            [else (let loop ()
                    (let ([n (fill-from-codec who tp #f)])
                      (cond
                        [(eq? 0 n) (loop)]
                        [(eof-object? n) (set-port-eof! tp #f) (eof-object)]
                        [else
                         (set-textual-port-input-index! tp 1)
                         (string-ref (textual-port-input-buffer tp) 0)])))])))
      (define transcoded-port-get-some
        (lambda (who tp str start count)
          (assert-not-closed who tp)
          (make-ready-for-input who tp)
          (let ([port-count (textual-port-input-count tp)])
            (cond
              [(not (fx= port-count 0))
               (let ([count (fxmin count port-count)]
                     [index (textual-port-input-index tp)])
                 (string-copy! (textual-port-input-buffer tp) index str start count)
                 (set-textual-port-input-index! tp (fx+ index count))
                 count)]
              [(port-flag-eof-set? tp) (set-port-eof! tp #f) (eof-object)]
              [else (read-from-codec who tp str start count #f)]))))
      (define transcoded-port-clear-input
        (lambda (who tp)
          (assert-not-closed who tp)
          (when ($port-flags-set? tp (constant port-flag-input-mode))
           ; position will be wrong after this.  c'est la vie.
            (set-textual-port-input-size! tp 0)
            (set-port-eof! tp #f)
            (let ([info ($port-info tp)])
              (codec-info-next-set! info 0)
              (codec-info-iend-set! info 0)
              (codec-info-icr-set! info #f)
              (let ([bp (codec-info-bp info)])
                (call-port-handler clear-input who bp))))))
      (define transcoded-port-put
        (lambda (who tp elt)
          (assert-not-closed who tp)
          (make-ready-for-output who tp)
          (let ([index (textual-port-output-index tp)])
            (string-set! (textual-port-output-buffer tp) index elt)
            (let ([index (fx+ index 1)])
              (cond
                [(not (port-output-full? tp))
                 (set-textual-port-output-index! tp index)]
                [($port-flags-set? tp (constant port-flag-line-buffered))
                 (cond
                   [(eol-char? elt)
                    (flush-to-codec who tp index)
                    (flush-from-codec who tp)
                    (flush-from-bp who tp)]
                   [(fx< (textual-port-output-size tp) (fx- (string-length (textual-port-output-buffer tp)) 1))
                    (set-textual-port-output-size! tp index)
                    (set-textual-port-output-index! tp index)]
                   [else (flush-to-codec who tp index)])]
                [else (flush-to-codec who tp index)])))))
      (define transcoded-port-put-some
        (lambda (who tp str start count)
          (assert-not-closed who tp)
          (make-ready-for-output who tp)
          (cond
            [($port-flags-set? tp (constant port-flag-line-buffered))
             (if (contains-eol-char? str start (fx+ start count))
                 (begin
                  ; line-buffering trumps nonblocking
                   (flush-to-codec who tp)
                   (let loop ([start start] [count count])
                     (unless (fx= count 0)
                       (let ([n (write-to-codec who tp str start count)])
                         (loop (fx+ start n) (fx- count n)))))
                   (flush-from-codec who tp)
                   (flush-from-bp who tp)
                   count)
                 (let ([buf (textual-port-output-buffer tp)]
                       [index (textual-port-output-index tp)])
                   (if (and (fx<= count max-put-copy) (fx< (fx+ index count) (string-length buf)))
                      ; there's room to copy str with one character to spare
                       (begin
                         (string-copy! str start buf index count)
                         (let ([index (fx+ index count)])
                           (set-textual-port-output-size! tp index)
                           (set-textual-port-output-index! tp index))
                         count)
                       (if (try-flush-to-codec who tp) (write-to-codec who tp str start count) 0))))]
            [else (if (try-flush-to-codec who tp) (write-to-codec who tp str start count) 0)])))
      (define transcoded-port-flush
        (lambda (who tp)
          (assert-not-closed who tp)
          (make-ready-for-output who tp)
          (flush-to-codec who tp)
          (flush-from-codec who tp)
          (flush-from-bp who tp)))
      (define transcoded-port-clear-output
        (lambda (who tp)
          (assert-not-closed who tp)
          (unless ($port-flags-set? tp (constant port-flag-input-mode))
           ; position will be wrong after this.  c'est la vie.
            (if ($port-flags-set? tp (constant port-flag-line-buffered))
                (set-textual-port-output-size! tp 0)
                (set-textual-port-output-index! tp 0))
            (let ([info ($port-info tp)])
              (codec-info-next-set! info 0)
              (let ([bp (codec-info-bp info)])
                (call-port-handler clear-output who bp))))))
      (define transcoded-port-close-port
        (lambda (who tp)
          (unless (port-closed? tp)
            (when (output-port? tp)
              (make-ready-for-output who tp)
              (flush-to-codec who tp)
              (flush-from-codec who tp)
              (flush-from-bp who tp))
            (unless (or (eq? tp $console-input-port)  ; refuse to close original console ports
                        (eq? tp $console-output-port)
                        (eq? tp $console-error-port))
              (when (output-port? tp)
                (set-textual-port-output-size! tp 0))
              (when (input-port? tp)
                (set-textual-port-input-size! tp 0)
                (set-port-eof! tp #f))
              (let ([info ($port-info tp)])
                (close-port (codec-info-bp info))
                ((codec-info-close info) info))
              (unregister-open-file tp)
              (mark-port-closed! tp)))))
      (define transcoded-port-port-position
        (lambda (who tp)
          (assert-not-closed who tp)
          (cond
            [($port-flags-set? tp (constant port-flag-input-mode))
            ; (port-position bp) gives us position in bytes after characters and bytes
            ; we haven't yet consumed.  to get position of first unconsumed character or
            ; byte, need to adjust downward by the number of bytes buffered, using
            ; ioffsets to determine the byte position of the first unconsumed character
            ; relative to the start of the port's buffer, ibytes to determine the total
            ; number of bytes represented by the characters in the port's buffer, and
            ; (- iend next) to determine the number of bytes not yet converted
            ; into characters.  if ioffsets is not available, the reported port-position
            ; may not be accurate.
             (let ([info ($port-info tp)])
               (- (call-port-handler port-position who (codec-info-bp info))
                  (let ([buffered-bytes (fx- (codec-info-iend info) (codec-info-next info))])
                    (cond
                      [(port-input-empty? tp) buffered-bytes]
                      [(codec-info-ioffsets info) =>
                       (lambda (ioffsets)
                         (fx- (fx+ (codec-info-ibytes info) buffered-bytes)
                              (fxvector-ref ioffsets (textual-port-input-index tp))))]
                      [else
                       (position-warning who "cannot determine accurate position for operation on ~s" tp)
                       buffered-bytes]))))]
            [else
             (flush-to-codec who tp)
             (flush-from-codec who tp)
             (let ([bp (codec-info-bp ($port-info tp))])
               (call-port-handler port-position who bp))])))
      (define transcoded-port-set-port-position!
        (lambda (who tp pos)
          (assert-not-closed who tp)
          (let ([info ($port-info tp)])
            (if ($port-flags-set? tp (constant port-flag-input-mode))
                (begin
                  (set-textual-port-input-size! tp 0)
                  (set-port-eof! tp #f)
                  (codec-info-next-set! info 0)
                  (codec-info-iend-set! info 0)
                  (codec-info-icr-set! info #f))
                (begin
                  (flush-to-codec who tp)
                  (flush-from-codec who tp)))
            (let ([bp (codec-info-bp info)])
              (call-port-handler set-port-position! who bp
               ; position past bom if known to be present at position 0
               ; if it was found or put elsewhere, all bets are off
                (if (and (eq? pos 0) (codec-info-zbom info)) 2 pos))))))
      (define transcoded-port-port-length
        (lambda (who tp)
          (assert-not-closed who tp)
          (unless ($port-flags-set? tp (constant port-flag-input-mode))
            (flush-to-codec who tp)
            (flush-from-codec who tp))
          (let ([bp (codec-info-bp ($port-info tp))])
            (call-port-handler port-length who bp))))
      (define transcoded-port-set-port-length!
        (lambda (who tp pos)
          (assert-not-closed who tp)
          (unless ($port-flags-set? tp (constant port-flag-input-mode))
            (flush-to-codec who tp)
            (flush-from-codec who tp))
          (let ([bp (codec-info-bp ($port-info tp))])
            (call-port-handler set-port-length! who bp pos))))
      (define transcoded-port-port-nonblocking?
        (lambda (who tp)
          (assert-not-closed who tp)
          (port-nonblocking? (codec-info-bp ($port-info tp)))))
      (define transcoded-port-set-port-nonblocking!
        (lambda (who tp b)
          (assert-not-closed who tp)
          (set-port-nonblocking! (codec-info-bp ($port-info tp)) b)))
      (define (make-transcoded-port-handler bp)
       ; could cache these, but the savings would be minimal
        (make-port-handler
          [ready? (and (input-port? bp) transcoded-port-ready?)]
          [lookahead (and (input-port? bp) transcoded-port-lookahead)]
          [unget (and (input-port? bp) transcoded-port-unget)]
          [get (and (input-port? bp) transcoded-port-get)]
          [get-some (and (input-port? bp) transcoded-port-get-some)]
          [clear-input (and (input-port? bp) transcoded-port-clear-input)]
          [put (and (output-port? bp) transcoded-port-put)]
          [put-some (and (output-port? bp) transcoded-port-put-some)]
          [flush (and (output-port? bp) transcoded-port-flush)]
          [clear-output (and (output-port? bp) transcoded-port-clear-output)]
          [close-port transcoded-port-close-port]
          [port-position
           (and (port-handler-port-position ($port-handler bp))
                transcoded-port-port-position)]
          [set-port-position!
           (and (port-handler-set-port-position! ($port-handler bp))
                transcoded-port-set-port-position!)]
          [port-length
           (and (port-handler-port-length ($port-handler bp))
                transcoded-port-port-length)]
          [set-port-length!
           (and (port-handler-set-port-length! ($port-handler bp))
                transcoded-port-set-port-length!)]
          [port-nonblocking?
           (and (port-handler-port-nonblocking? ($port-handler bp))
                transcoded-port-port-nonblocking?)]
          [set-port-nonblocking!
           (and (port-handler-set-port-nonblocking! ($port-handler bp))
                transcoded-port-set-port-nonblocking!)])))
    (set-who! transcoded-port
      (lambda (bp tx)
        (define-syntax copy-flag!
          (syntax-rules ()
            [(_ from to flag)
             (when ($port-flags-set? from (constant flag))
               ($set-port-flags! to (constant flag)))]))
        (define (clone-port bp)
          (let ([bpc ($make-textual-input/output-port "" ($port-handler bp) "" "" #f)])
            ($byte-copy! bp (constant port-type-disp) bpc (constant port-type-disp) (constant size-port))
            bpc))
        (unless (and (port? bp) (binary-port? bp)) ($oops who "~s is not a binary port" bp))
        (unless ($transcoder? tx) ($oops who "~s is not a transcoder" tx))
        (let* ([buffered? (or ($port-flags-set? bp (constant port-flag-block-buffered))
                              ($port-flags-set? bp (constant port-flag-line-buffered)))]
               [codec-bv
                (if (not buffered?)
                    (make-bytevector min-codec-buffer-size)
                    (let* ([make-buffer (make-codec-buffer)]
                           [bv (make-buffer bp)])
                      (unless (and (mutable-bytevector? bv) (fx>= (bytevector-length bv) min-codec-buffer-size))
                        ($oops who "make-codec-buffer ~s did not return a mutable bytevector of length at least ~r"
                          make-buffer min-codec-buffer-size))
                      bv))]
               [bpc (clone-port bp)]
               [name (port-name bpc)]
               [buffer-length (if buffered?
                                  (transcoded-port-buffer-size)
                                  unbuffered-transcoded-port-buffer-length)]
               [codec ($transcoder-codec tx)]
               [info ((codec-make-info codec) who tx bpc buffer-length codec-bv)]
               [handler (make-transcoded-port-handler bpc)]
               [tp (if (input-port? bpc)
                       (if (output-port? bpc)
                           ($make-textual-input/output-port name handler
                             (make-string buffer-length)
                             (make-string buffer-length)
                             info)
                           ($make-textual-input-port name handler
                             (make-string buffer-length) info))
                       ($make-textual-output-port name handler
                         (make-string buffer-length) info))])
          (copy-flag! bpc tp port-flag-block-buffered)
          (copy-flag! bpc tp port-flag-line-buffered)
          (mark-port-closed! bp)
          (when (input-port? bp)
            (set-binary-port-input-size! bp 0)
            (set-port-eof! bp #f)
            (set-textual-port-input-size! tp 0))
          (when (output-port? bp)
            (set-binary-port-output-size! bp 0)
            (set-textual-port-output-size! tp
              (if ($port-flags-set? tp (constant port-flag-line-buffered))
                  0
                  (fx1- buffer-length)))
            ($set-port-flags! tp (constant port-flag-bol)))
          ($set-port-info! bp tp) ; back-link for bytevector-output-port extractor
          (when (registered-open-file? bp)
            (unregister-open-file bp)
            (register-open-file tp))
          tp))))

  (let ()
    (define-syntax set-who!-port-has
      (lambda (x)
        (syntax-case x ()
          [(_ name)
           (with-syntax ([name (construct-name #'name "port-has-" #'name "?")]
                         [field (construct-name #'name "port-handler-" #'name)])
             #'(set-who! name
                 (lambda (p)
                   (unless (port? p) ($oops who "~s is not a port" p))
                   (and (field ($port-handler p)) #t))))])))

    (define-syntax set-who!-port
      (lambda (x)
        (syntax-case x ()
          [(_ name (args ...))
           (with-syntax ([field (construct-name #'name "port-handler-" #'name)])
             #'(set-who! name
                 (lambda (p args ...)
                   (unless (port? p) ($oops who "~s is not a port" p))
                   (let ([op (field ($port-handler p))])
                     (unless op ($oops who "~s does not support operation" p))
                     (op who p args ...)))))])))

    (set-who!-port-has port-position)
    (set-who!-port     port-position ())
    (set-who!-port-has set-port-position!)
    (set-who!-port     set-port-position! (x))

    ;; The following are not in R6RS
    (set-who!-port-has port-nonblocking?)
    (set-who!-port     port-nonblocking? ())
    (set-who!-port-has set-port-nonblocking!)
    (set-who!-port     set-port-nonblocking! (x))
    (set-who!-port-has port-length)
    (set-who!-port     port-length ())
    (set-who!-port-has set-port-length!)
    (set-who!-port     set-port-length! (x)))

  (set-who! file-position
    (case-lambda
      [(p)
       (unless (port? p) ($oops who "~s is not a port" p))
       (let ([op (port-handler-port-position ($port-handler p))])
         (unless op ($oops who "~s does not support operation" p))
         (op who p))]
      [(p pos)
       (unless (port? p) ($oops who "~s is not a port" p))
       (let ([op (port-handler-set-port-position! ($port-handler p))])
         (unless op ($oops who "~s does not support operation" p))
         (op who p pos))]))

  (set-who! file-length
    (lambda (p)
      (unless (port? p) ($oops who "~s is not a port" p))
      (let ([op (port-handler-port-length ($port-handler p))])
        (unless op ($oops who "~s does not support operation" p))
        (op who p))))

  ;; Not in R6RS
  ;; truncate-file is set-port-length and set-port-position combined
  (let ()
    (define (tp who port pos)
      (unless (output-port? port) ($oops who "~s is not an output port" port))
      (let ([handler ($port-handler port)])
        (let ([set-len! (port-handler-set-port-length! handler)]
              [set-pos! (port-handler-set-port-position! handler)])
          (unless (and set-len! set-pos!)
            ($oops who "~s does not support operation" port))
          (set-len! who port pos)
          (set-pos! who port pos))))

    (set-who! truncate-port
      (case-lambda
        [(port) (tp who port 0)]
        [(port pos) (tp who port pos)]))

    (set-who! truncate-file
      (case-lambda
        [(port) (tp who port 0)]
        [(port pos) (tp who port pos)])))

  (set-who! close-port
    (lambda (port)
      (unless (port? port) ($oops who "~s is not a port" port))
      (call-port-handler close-port who port)))

  (set-who! call-with-port
    (lambda (port proc)
      (unless (port? port) ($oops who "~s is not a port" port))
      (unless (procedure? proc) ($oops who "~s is not a procedure" proc))
      (call-with-values
        (lambda () (proc port))
        (case-lambda
          [(x)
           (call-port-handler close-port who port)
           x]
          [args
           (call-port-handler close-port who port)
           (apply values args)]))))

;;;; 8.2.7 Input ports
  ;; input-port? in prims.ss
  ;; port-eof? in prims.ss

  ;; Not in R6RS
  (set-who! input-port-ready?
    (lambda (input-port)
      (unless (input-port? input-port)
        ($oops who "~s is not an input port" input-port))
      (or (not (port-input-empty? input-port))
          (port-flag-eof-set? input-port)
          (call-port-handler ready? who input-port))))

  (let ()
  ;; open-file-input-port
    (define open-binary-file-input-port
      (lambda (who filename options mode)
        (unless (string? filename)
          ($oops who "~s is not a string" filename))
        (unless (and (enum-set? options)
                     (enum-set-subset? options $file-options))
          ($oops who "~s is not a file-options object" options))
        (unless (buffer-mode? mode)
          ($oops who "~s is not a valid buffer mode" mode))
        (when (enum-set-subset? (file-options exclusive) options)
          ($oops who "exclusive option not supported for file input ports"))
        (let ([fd (critical-section ($open-input-fd filename (enum-set-subset? (file-options compressed) options)))])
          (when (pair? fd) (open-oops who filename options fd))
          (if (box? fd) ; box iff file opened with compressed option is actually gzip'd
              (open-binary-fd-input-port who filename (unbox fd) #t mode #t)
              (open-binary-fd-input-port who filename fd #t mode #f)))))

    (define open-binary-standard-input-port
      (lambda (b-mode)
        (define who 'standard-input-port)
        (unless (buffer-mode? b-mode)
          ($oops who "~s is not a valid buffer mode" b-mode))
        (open-binary-fd-input-port who "stdin" (make-fd 0) ($fd-regular? 0) b-mode #f)))

    (define help-open-file-input-port
      (lambda (who filename options buffer-mode maybe-transcoder)
        (let ([binary-port (open-binary-file-input-port who filename options buffer-mode)])
          (if maybe-transcoder
              (transcoded-port binary-port maybe-transcoder)
              binary-port))))

    (set-who! port-file-compressed!
      (lambda (p)
        (unless (port? p) ($oops who "~s is not a port" p))
        (when (and (input-port? p) (output-port? p)) ($oops who "cannot compress input/output port ~s" p))
        (let ([bp (if (binary-port? p)
                      p
                      (let ([info ($port-info p)])
                        (and (codec-info? info) (codec-info-bp info))))])
          (unless (and bp ($port-flags-set? bp (constant port-flag-file))) ($oops who "~s is not a file port" p))
          (unless ($port-flags-set? bp (constant port-flag-compressed))
            (let ([fd ($port-info bp)])
              (unless ($fd-regular? fd) ($oops who "~s is not a regular file" p))
              ; flush any uncompressed data in the output buffer
              (when (output-port? p) (flush-output-port p))
              (critical-section
                (let ([gzfd (if (input-port? p)
                                (let ([fp (port-position p)])
                                  ; reposition to 'unread' any compressed data in the input buffer
                                  (set-port-position! p fp)
                                  ($compress-input-fd fd fp))
                                ($compress-output-fd fd))])
                  (when (string? gzfd) ($oops who "failed for ~s: ~(~a~)" p gzfd))
                  (unless (eqv? gzfd fd) ; uncompressed input port
                    (assert (box? gzfd))
                    ($set-port-info! bp (unbox gzfd))
                    ($set-port-flags! bp (constant port-flag-compressed))))))))))

    (set-who! open-fd-input-port
      (case-lambda
        [(fd)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (open-binary-fd-input-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) (buffer-mode block) #f)]
        [(fd buffer-mode)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (unless (buffer-mode? buffer-mode)
           ($oops who "~s is not a buffer mode" buffer-mode))
         (open-binary-fd-input-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) buffer-mode #f)]
        [(fd buffer-mode maybe-transcoder)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (unless (buffer-mode? buffer-mode)
           ($oops who "~s is not a buffer mode" buffer-mode))
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (let ([binary-port (open-binary-fd-input-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) buffer-mode #f)])
           (if maybe-transcoder
               (transcoded-port binary-port maybe-transcoder)
               binary-port))]))

    (let ()
      (define s-process (foreign-procedure "(cs)s_process" (string boolean) scheme-object))
      (define (subprocess-port who what fd pid b-mode maybe-transcoder)
        (unless (buffer-mode? b-mode)
          ($oops who "~s is not a valid buffer mode" b-mode))
        (let ([name (format "pid ~s ~a" pid what)])
          (let ([bp (if (eq? what 'stdin)
                        (open-binary-fd-output-port who name (make-fd fd) #f b-mode #f #f)
                        (open-binary-fd-input-port who name (make-fd fd) #f b-mode #f))])
           (if maybe-transcoder (transcoded-port bp maybe-transcoder) bp))))
      (set-who! process
        (lambda (s)
          (unless (string? s) ($oops who "~s is not a string" s))
          (apply (lambda (ifd ofd pid)
                   (list
                     (subprocess-port who 'stdout ifd pid (buffer-mode block) (current-transcoder))
                     (subprocess-port who 'stdin ofd pid (buffer-mode line) (current-transcoder))
                     pid))
            (s-process s #f))))
      (set-who! open-process-ports
        (case-lambda
          [(s)
           (unless (string? s) ($oops who "~s is not a string" s))
           (apply (lambda (ifd efd ofd pid)
                    (values
                      (subprocess-port who 'stdin ofd pid (buffer-mode block) #f)
                      (subprocess-port who 'stdout ifd pid (buffer-mode block) #f)
                      (subprocess-port who 'stderr efd pid (buffer-mode block) #f)
                      pid))
             (s-process s #t))]
          [(s b-mode)
           (unless (string? s) ($oops who "~s is not a string" s))
           (apply (lambda (ifd efd ofd pid)
                    (values
                      (subprocess-port who 'stdin ofd pid b-mode #f)
                      (subprocess-port who 'stdout ifd pid b-mode #f)
                      (subprocess-port who 'stderr efd pid b-mode #f)
                      pid))
             (s-process s #t))]
          [(s b-mode maybe-transcoder)
           (unless (string? s) ($oops who "~s is not a string" s))
           (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
             ($oops who "~s is not #f or a transcoder" maybe-transcoder))
           (apply (lambda (ifd efd ofd pid)
                    (values
                      (subprocess-port who 'stdin ofd pid b-mode maybe-transcoder)
                      (subprocess-port who 'stdout ifd pid b-mode maybe-transcoder)
                      (subprocess-port who 'stderr efd pid b-mode maybe-transcoder)
                      pid))
             (s-process s #t))])))

    (set-who! open-file-input-port
      (case-lambda
        [(filename)
         (open-binary-file-input-port who filename (file-options) (buffer-mode block))]
        [(filename options)
         (open-binary-file-input-port who filename options (buffer-mode block))]
        [(filename options buffer-mode)
         (open-binary-file-input-port who filename options buffer-mode)]
        [(filename options buffer-mode maybe-transcoder)
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (help-open-file-input-port who filename options buffer-mode maybe-transcoder)]))

    (set! $open-file-input-port
      (case-lambda
        [(who filename)
         (open-binary-file-input-port who filename (file-options) (buffer-mode block))]
        [(who filename options)
         (open-binary-file-input-port who filename options (buffer-mode block))]
        [(who filename options buffer-mode)
         (open-binary-file-input-port who filename options buffer-mode)]
        [(who filename options buffer-mode maybe-transcoder)
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (help-open-file-input-port who filename options buffer-mode maybe-transcoder)]))

    (set-who! standard-input-port
      (case-lambda
        [() (open-binary-standard-input-port (buffer-mode block))]
        [(b-mode) (open-binary-standard-input-port b-mode)]
        [(b-mode maybe-transcoder)
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (let ([binary-port (open-binary-standard-input-port b-mode)])
           (if maybe-transcoder
               (transcoded-port binary-port maybe-transcoder)
               binary-port))]))

    (set-who! r6rs:standard-input-port
      (rec standard-input-port
        (lambda ()
          (open-binary-standard-input-port (buffer-mode block)))))

   ; simple i/o routines here to share helpers
    (let ()
      (define (oif who s o)
        (unless (string? s) ($oops who "~s is not a string" s))
        (let ([o (if (list? o) o (list o))])
          (let loop ([o o] [zmode #f] [bmode #f])
            (if (null? o)
                (help-open-file-input-port who s
                  (if (eq? zmode 'compressed) (file-options compressed) (file-options))
                  (if (eq? bmode 'unbuffered) (buffer-mode none) (buffer-mode block))
                  (current-transcoder))
                (case (car o)
                  [(compressed uncompressed)
                   (check-option who zmode (car o))
                   (loop (cdr o) (car o) bmode)]
                  [(buffered unbuffered)
                   (check-option who bmode (car o))
                   (loop (cdr o) zmode (car o))]
                  [else ($oops who "invalid option ~s" (car o))])))))

      (set-who! #(r6rs: open-input-file)
        (lambda (s) (oif who s '())))

      (set-who! open-input-file
        (case-lambda
          [(s) (oif who s '())]
          [(s o) (oif who s o)]))

      (let ()
        (define (cwif who s f o)
          (unless (procedure? f)
            ($oops 'call-with-input-file "~s is not a procedure" f))
          (let ([p (oif 'call-with-input-file s o)])
            (call-with-values
              (lambda () (f p))
              (lambda args (close-input-port p) (apply values args)))))
        (set-who! #(r6rs: call-with-input-file)
          (lambda (s f) (cwif who s f '())))
        (set-who! call-with-input-file
          (case-lambda
            [(s f) (cwif who s f '())]
            [(s f o) (cwif who s f o)])))

      (let ()
        (define (wiff who s f o)
          (unless (procedure? f)
            ($oops 'with-input-from-file "~s is not a procedure" f))
          (let ([p (oif 'with-input-from-file s o)])
            (call-with-values
              (lambda () (parameterize ([current-input-port p]) (f)))
              (lambda v (close-input-port p) (apply values v)))))
        (set-who! #(r6rs: with-input-from-file)
          (lambda (s f) (wiff who s f '())))
        (set-who! with-input-from-file
          (case-lambda
            [(s f) (wiff who s f '())]
            [(s f o) (wiff who s f o)]))))
    )

  ;; open-bytevector-input-port
  (let ()
    ;; port-info stores whether to claim it is nonblocking or not
    (define $bytevector-input-handler
      (make-port-handler
        [ready?
         (lambda (who p)
           (assert-not-closed who p)
           #t)]
        [lookahead
         (lambda (who p)
           (assert-not-closed who p)
           (if (port-input-empty? p)
               (eof-object)
               (bytevector-u8-ref (binary-port-input-buffer p)
                                  (binary-port-input-index p))))]
        [unget
         (lambda (who p x)
           (assert-not-closed who p)
           (if (eof-object? x)
               ;; We don't set port-eof b/c #!eof only comes at end anyway
               (unless (port-input-empty? p) (unget-error who p x))
               (let ([index (binary-port-input-index p)])
                 (when (eq? 0 index) (unget-error who p x))
                 (set-binary-port-input-index! p (fx1- index)))))]
        [get
         (lambda (who p)
           (assert-not-closed who p)
           (if (port-input-empty? p)
               (eof-object)
               (let ([index (binary-port-input-index p)])
                 (set-binary-port-input-index! p (fx1+ index))
                 (bytevector-u8-ref (binary-port-input-buffer p) index))))]
        [get-some
         (lambda (who p bv start count)
           (assert-not-closed who p)
           (let ([port-count (binary-port-input-count p)])
             (if (eq? 0 port-count)
                 (eof-object)
                 (let ([index (binary-port-input-index p)]
                       [count (fxmin count port-count)])
                   (unless (and (fx= index start) (eq? bv (binary-port-input-buffer p)))
                     (bytevector-copy! (binary-port-input-buffer p) index
                       bv start count))
                   (set-binary-port-input-index! p (fx+ index count))
                   count))))]
        [clear-input
         (lambda (who p)
           (assert-not-closed who p))]
        [put #f]
        [put-some #f]
        [flush #f]
        [clear-output #f]
        [close-port
         (lambda (who p)
           (unless (port-closed? p)
             (mark-port-closed! p)
             (set-binary-port-input-size! p 0)))]
        [port-position
         (lambda (who p)
           (assert-not-closed who p)
           (binary-port-input-index p))]
        [set-port-position!
         (lambda (who p x)
           (assert-not-closed who p)
           (unless (and (fixnum? x) (not ($fxu< (binary-port-input-size p) x)))
             (if (or (and (fixnum? x) (fx>= x 0)) (and (bignum? x) (>= x 0)))
                 (position-oops who p x "out of range")
                 ($oops who "~s is not a valid position" x)))
           (set-binary-port-input-index! p x))]
        [port-length
         (lambda (who p)
           (assert-not-closed who p)
           (bytevector-length (binary-port-input-buffer p)))]
        [set-port-length! #f]
        [port-nonblocking?
         (lambda (who p)
           (assert-not-closed who p)
           ($port-info p))]
        [set-port-nonblocking!
         (lambda (who p x)
           (assert-not-closed who p)
           ($set-port-info! p x))]))

    (define open-binary-bytevector-input-port
      (lambda (bv)
        (define who 'open-bytevector-input-port)
        (unless (bytevector? bv)
          ($oops who "~s is not a bytevector" bv))
        (let ([p ($make-binary-input-port "bytevector" $bytevector-input-handler bv #f)])
          ($set-port-flags! p (constant port-flag-block-buffered))
          p)))

    (set-who! open-bytevector-input-port
      (case-lambda
       [(bv) (open-binary-bytevector-input-port bv)]
       [(bv maybe-transcoder)
        (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
          ($oops who "~s is not #f or a transcoder" maybe-transcoder))
        (let ([binary-port (open-binary-bytevector-input-port bv)])
          (if maybe-transcoder
              (transcoded-port binary-port maybe-transcoder)
              binary-port))]))
    )

  ;; open-string-input-port
  (let ()
    ;; port-info stores whether to claim it is nonblocking or not
    (define $string-input-handler
      (make-port-handler
        [ready?
         (lambda (who p)
           (assert-not-closed who p)
           #t)]
        [lookahead
         (lambda (who p)
           (assert-not-closed who p)
           (if (port-input-empty? p)
               (eof-object)
               (string-ref (textual-port-input-buffer p)
                           (textual-port-input-index p))))]
        [unget
         (lambda (who p x)
           (assert-not-closed who p)
           (if (eof-object? x)
               ;; We don't set port-eof b/c #!eof only comes at end anyway
               (unless (port-input-empty? p) (unget-error who p x))
               (let ([index (textual-port-input-index p)])
                 (when (eq? 0 index) (unget-error who p x))
                 (set-textual-port-input-index! p (fx1- index)))))]
        [get
         (lambda (who p)
           (assert-not-closed who p)
           (if (port-input-empty? p)
               (eof-object)
               (let ([index (textual-port-input-index p)])
                 (set-textual-port-input-index! p (fx1+ index))
                 (string-ref (textual-port-input-buffer p) index))))]
        [get-some
         (lambda (who p st start count)
           (assert-not-closed who p)
           (let ([port-count (textual-port-input-count p)])
             (if (eq? 0 port-count)
                 (eof-object)
                 (let ([index (textual-port-input-index p)]
                       [count (fxmin count port-count)])
                   (string-copy! (textual-port-input-buffer p) index
                                 st start count)
                   (set-textual-port-input-index! p (fx+ index count))
                   count))))]
        [clear-input
         (lambda (who p)
           (assert-not-closed who p))]
        [put #f]
        [put-some #f]
        [flush #f]
        [clear-output #f]
        [close-port
         (lambda (who p)
           (unless (port-closed? p)
             (mark-port-closed! p)
             (set-textual-port-input-size! p 0)))]
        [port-position
         (lambda (who p)
           (assert-not-closed who p)
           (textual-port-input-index p))]
        [set-port-position!
         (lambda (who p x)
           (assert-not-closed who p)
           (unless (and (fixnum? x) (not ($fxu< (textual-port-input-size p) x)))
             (if (or (and (fixnum? x) (fx>= x 0)) (and (bignum? x) (>= x 0)))
                 (position-oops who p x "out of range")
                 ($oops who "~s is not a valid position" x)))
           (set-textual-port-input-index! p x))]
        [port-length
         (lambda (who p)
           (assert-not-closed who p)
           (string-length (textual-port-input-buffer p)))]
        [set-port-length! #f]
        [port-nonblocking?
         (lambda (who p)
           (assert-not-closed who p)
           ($port-info p))]
        [set-port-nonblocking!
         (lambda (who p x)
           (assert-not-closed who p)
           ($set-port-info! p x))]))

    (define (osip who str)
      (unless (string? str)
        ($oops who "~s is not a string" str))
      (let ([p ($make-textual-input-port "string" $string-input-handler str #f)])
        ($set-port-flags! p (constant port-flag-block-buffered))
        ($set-port-flags! p (constant port-flag-char-positions))
        p))

    (set-who! open-string-input-port
      (lambda (str)
        (osip who str)))

    (set-who! open-input-string
      (lambda (str)
        (osip who str)))
    )

  ;; standard-input-port in open-binary-file-input-port section
  ;; current-input-port in prims.ss

  (set-who! make-custom-binary-input-port
    (lambda (id read! get-position set-position! close)
      (unless (string? id) ($oops who "~s is not a string" id))
      (unless (procedure? read!) ($oops who "~s is not a procedure" read!))
      (unless (or (not get-position) (procedure? get-position))
        ($oops who "~s is not a procedure or #f" get-position))
      (unless (or (not set-position!) (procedure? set-position!))
        ($oops who "~s is not a procedure or #f" set-position!))
      (unless (or (not close) (procedure? close))
        ($oops who "~s is not a procedure or #f" close))
      (let ([handler
             (make-port-handler
               [ready?
                (lambda (who p)
                  (assert-not-closed who p)
                  (or (not (port-input-empty? p))
                      (port-flag-eof-set? p)
                      (read-oops who p "cannot determine ready status")))]
               [lookahead
                (lambda (who p)
                  (assert-not-closed who p)
                  (binary-custom-port-lookahead who p read!))]
               [unget
                (lambda (who p x)
                  (assert-not-closed who p)
                  (binary-custom-port-unget who p x))]
               [get
                (lambda (who p)
                  (assert-not-closed who p)
                  (binary-custom-port-get who p read!))]
               [get-some
                (lambda (who p bv start count)
                  (assert-not-closed who p)
                  (binary-custom-port-get-some who p read! bv start count))]
               [clear-input
                (lambda (who p)
                  (assert-not-closed who p)
                  (binary-custom-port-clear-input who p))]
               [put #f]
               [put-some #f]
               [flush #f]
               [clear-output #f]
               [close-port
                (lambda (who p)
                  (unless (port-closed? p)
                    (binary-custom-port-close-port who p close)))]
               [port-position
                (and get-position
                  (lambda (who p)
                    (assert-not-closed who p)
                    (binary-custom-port-port-position in who p get-position)))]
               [set-port-position!
                (and set-position!
                  (lambda (who p x)
                    (unless (or (and (fixnum? x) (fx>= x 0)) (and (bignum? x) (>= x 0)))
                      ($oops who "~s is not a valid position" x))
                    (assert-not-closed who p)
                    (set-binary-port-input-size! p 0) ;; junk the buffer data
                    (set-port-eof! p #f)
                    (set-position! x)))]
               [port-length #f]
               [set-port-length! #f]
               [port-nonblocking? #f]
               [set-port-nonblocking! #f])])
        (let ([p ($make-binary-input-port id handler
                   (make-bytevector (custom-port-buffer-size))
                   #f)])
          ($set-port-flags! p (constant port-flag-block-buffered))
          (set-binary-port-input-size! p 0)
          p))))

  (set-who! make-custom-textual-input-port
    (lambda (id read! get-position set-position! close)
      (unless (string? id) ($oops who "~s is not a string" id))
      (unless (procedure? read!) ($oops who "~s is not a procedure" read!))
      (unless (or (not get-position) (procedure? get-position))
        ($oops who "~s is not a procedure or #f" get-position))
      (unless (or (not set-position!) (procedure? set-position!))
        ($oops who "~s is not a procedure or #f" set-position!))
      (unless (or (not close) (procedure? close))
        ($oops who "~s is not a procedure or #f" close))
      (let ([handler
             (make-port-handler
               [ready?
                (lambda (who p)
                  (assert-not-closed who p)
                  (or (not (port-input-empty? p))
                      (port-flag-eof-set? p)
                      (read-oops who p "cannot determine ready status")))]
               [lookahead
                (lambda (who p)
                  (assert-not-closed who p)
                  (textual-custom-port-lookahead who p read!))]
               [unget
                (lambda (who p x)
                  (assert-not-closed who p)
                  (textual-custom-port-unget who p x))]
               [get
                (lambda (who p)
                  (assert-not-closed who p)
                  (textual-custom-port-get who p read!))]
               [get-some
                (lambda (who p str start count)
                  (assert-not-closed who p)
                  (textual-custom-port-get-some who p read! str start count))]
               [clear-input
                (lambda (who p)
                  (assert-not-closed who p)
                  (textual-custom-port-clear-input who p))]
               [put #f]
               [put-some #f]
               [flush #f]
               [clear-output #f]
               [close-port
                (lambda (who p)
                  (unless (port-closed? p)
                    (textual-custom-port-close-port who p close)))]
               [port-position
                (and get-position
                  (lambda (who p)
                    (assert-not-closed who p)
                    (unless (port-input-empty? p)
                      (position-warning who
                        "cannot determine accurate position after read on ~s"
                        p))
                    (get-position)))]
               [set-port-position!
                (and set-position!
                  (lambda (who p x)
                    (assert-not-closed who p)
                    (set-textual-port-input-size! p 0) ;; junk the buffer data
                    (set-port-eof! p #f)
                    (set-position! x)))]
               [port-length #f]
               [set-port-length! #f]
               [port-nonblocking? #f]
               [set-port-nonblocking! #f])])
        (let ([p ($make-textual-input-port id handler
                   (make-string (custom-port-buffer-size))
                   #f)])
          ($set-port-flags! p (constant port-flag-block-buffered))
          (set-textual-port-input-size! p 0)
          p))))


;;;; 8.2.8 Binary input
  ;; get-u8 in prims.ss
  ;; lookahead-u8 in prims.ss
  ;; unget-u8 in prims.ss

  ;; get-bytevector! ::  port * bv * start * max -> count TODO(not R6RS)

  (let ()
    ;; This helper handles all the looping for the following functions
    (define (get-bytevector-min-max who p bv start min max)
      (if (eq? 0 max)
          0
          (let ([get-some (port-handler-get-some ($port-handler p))])
            ;; Loop invariant:
            ;;   next = next spot to fill in the bytevector
            ;;   min = minimum left to read
            ;;   max = maximum left to read
            (let loop ([next start]
                       [min min]
                       [max max])
              (let ([n (get-some who p bv next max)])
                (if (eof-object? n)
                    (if (eq? start next)
                        (eof-object) ;; We couldn't even read one byte
                        (begin ;; Got some but got #!eof before full
                          (call-port-handler unget who p (eof-object)) ;; Put the #!eof back
                          (fx- next start))) ;; Return our count
                    (let ([min (fx- min n)]
                          [next (fx+ next n)])
                      (if (fx<= min 0)
                          (fx- next start) ;; We got enough to stop
                          (loop next min (fx- max n))))))))))

    (define (append-blocks size block-size block blocks)
      (if (null? blocks)
          (bytevector-truncate! block size)
          (let ([buffer (#2%make-bytevector size)])
            (let loop ([block-size block-size] [block block] [blocks blocks] [end size])
              (let ([end (fx- end block-size)])
                (bytevector-copy! block 0 buffer end block-size)
                (if (null? blocks)
                    buffer
                    (loop (caar blocks) (cdar blocks) (cdr blocks) end)))))))

    (set-who! get-bytevector-n
      (lambda (binary-input-port count)
        (unless (and (input-port? binary-input-port) (binary-port? binary-input-port))
          ($oops who "~s is not a binary input port" binary-input-port))
        (unless (and (fixnum? count) (fx>= count 0))
          ($oops who "~s is not a nonnegative fixnum" count))
        (let ([buffer-size (file-buffer-size)])
          (if (not ($fxu< buffer-size count))
              (let ([bv (make-bytevector count)])
                (let ([n (get-bytevector-min-max
                          who binary-input-port bv 0 count count)])
                  (if (eof-object? n) n (bytevector-truncate! bv n))))
              (let ([get-some (port-handler-get-some ($port-handler binary-input-port))])
                (let loop ([count count]
                           [size 0]
                           [next-block-index 0]
                           [next-block (make-bytevector buffer-size)]
                           [blocks '()])
                  (let ([next-size (get-some who binary-input-port
                                     next-block next-block-index
                                     (fxmin count (fx- buffer-size next-block-index)))])
                    (if (or (eof-object? next-size) (eq? next-size 0))
                        (if (eqv? size 0)
                            (if (eof-object? next-size) (eof-object) #vu8())
                            (append-blocks size next-block-index next-block blocks))
                        (let ([count (fx- count next-size)]
                              [size (fx+ size next-size)]
                              [next-block-index (fx+ next-block-index next-size)])
                          (if (eqv? count 0)
                              (append-blocks size next-block-index next-block blocks)
                              (if (fx>= next-block-index (fxquotient buffer-size 2))
                                  (loop count size 0
                                    (make-bytevector buffer-size)
                                    (cons (cons next-block-index next-block) blocks))
                                  (loop count size next-block-index next-block blocks))))))))))))

    (set-who! get-bytevector-n!
      (lambda (binary-input-port bv start count)
        (unless (and (input-port? binary-input-port) (binary-port? binary-input-port))
          ($oops who "~s is not a binary input port" binary-input-port))
        (unless (bytevector? bv)
          ($oops who "~s is not a bytevector" bv))
        (unless (and (fixnum? start) (fx>= start 0))
          ($oops who "invalid start value ~s" start))
        (unless (and (fixnum? count) (fx>= count 0))
          ($oops who "invalid count ~s" count))
        (unless (fx<= count (fx- (bytevector-length bv) start)) ; avoid overflow
          ($oops who "index ~s + count ~s is beyond the end of ~s" start count bv))
        (get-bytevector-min-max who binary-input-port bv start count count)))

    (set-who! get-bytevector-some
      (lambda (binary-input-port)
        (let ([buffer-size (file-buffer-size)])
          (unless (and (input-port? binary-input-port) (binary-port? binary-input-port))
            ($oops who "~s is not a binary input port" binary-input-port))
          (let ([bv (make-bytevector buffer-size)])
            (let ([n (get-bytevector-min-max who binary-input-port bv 0 0 buffer-size)])
              (if (eof-object? n)
                  (eof-object)
                  (bytevector-truncate! bv n)))))))

    (set-who! get-bytevector-some!
      (lambda (binary-input-port bv start count)
        (unless (and (input-port? binary-input-port) (binary-port? binary-input-port))
          ($oops who "~s is not a binary input port" binary-input-port))
        (unless (bytevector? bv)
          ($oops who "~s is not a bytevector" bv))
        (unless (and (fixnum? start) (fx>= start 0))
          ($oops who "invalid start value ~s" start))
        (unless (and (fixnum? count) (fx>= count 0))
          ($oops who "invalid count ~s" count))
        (unless (fx<= count (fx- (bytevector-length bv) start)) ; avoid overflow
          ($oops who "index ~s + count ~s is beyond the end of ~s" start count bv))
        (get-bytevector-min-max who binary-input-port bv start 0 count)))

    (set-who! get-bytevector-all
      (lambda (binary-input-port)
        (unless (and (input-port? binary-input-port) (binary-port? binary-input-port))
          ($oops who "~s is not a binary input port" binary-input-port))
        (let ([buffer-size (file-buffer-size)])
          (let ([get-some (port-handler-get-some ($port-handler binary-input-port))])
            (let loop ([size 0]
                       [next-block-index 0]
                       [next-block (make-bytevector buffer-size)]
                       [blocks '()])
              (let ([next-size (get-some who binary-input-port
                                 next-block next-block-index
                                 (fx- buffer-size next-block-index))])
                (if (eof-object? next-size)
                    (if (eq? size 0)
                        (eof-object)
                        (append-blocks size next-block-index next-block blocks))
                    (let ([size (fx+ size next-size)]
                          [next-block-index (fx+ next-block-index next-size)])
                      (if (fx>= next-block-index (fxquotient buffer-size 2))
                          (loop size 0
                            (make-bytevector buffer-size)
                            (cons (cons next-block-index next-block) blocks))
                          (loop size next-block-index next-block blocks))))))))))
    )


;;;; 8.2.9 Textual input
  ;; get-char in prims.ss
  ;; lookahead-char in prims.ss

  (let ()
    ;; TODO: this code is identical to get-bytevector-min-max
    ;; This helper handles all the looping for the following functions
    (define (get-string-min-max who p bv start min max)
      (if (eq? 0 max)
          0
          (let ([get-some (port-handler-get-some ($port-handler p))])
            ;; Loop invariant:
            ;;   next = next spot to fill in the bytevector
            ;;   min = minimum left to read
            ;;   max = maximum left to read
            (let loop ([next start]
                       [min min]
                       [max max])
              (let ([n (get-some who p bv next max)])
                (if (eof-object? n)
                    (if (eq? start next)
                        (eof-object) ;; We couldn't even read one byte
                        (begin ;; Got some but got #!eof before full
                          (call-port-handler unget who p (eof-object)) ;; Put the #!eof back
                          (fx- next start))) ;; Return our count
                    (let ([min (fx- min n)]
                          [next (fx+ next n)])
                      (if (fx<= min 0)
                          (fx- next start) ;; We got enough to stop
                          (loop next min (fx- max n))))))))))

    (define (append-blocks size block-size block blocks)
      (if (null? blocks)
          (string-truncate! block size)
          (let ([buffer (#2%make-string size)])
            (let loop ([block-size block-size] [block block] [blocks blocks] [end size])
              (let ([end (fx- end block-size)])
                (string-copy! block 0 buffer end block-size)
                (if (null? blocks)
                    buffer
                    (loop (caar blocks) (cdar blocks) (cdr blocks) end)))))))

    (define $get-string-all
      (lambda (who textual-input-port buffer-size one-block?)
        (let ([get-some (port-handler-get-some ($port-handler textual-input-port))])
          (let loop ([size 0]
                     [next-block-index 0]
                     [next-block (make-string buffer-size)]
                     [blocks '()])
            (let ([next-size (get-some who textual-input-port
                               next-block next-block-index
                               (fx- buffer-size next-block-index))])
              (if (eof-object? next-size)
                  (if (eq? size 0)
                      (eof-object)
                      (append-blocks size next-block-index next-block blocks))
                  (let ([size (fx+ size next-size)]
                        [next-block-index (fx+ next-block-index next-size)])
                    (if (and (not one-block?) (fx>= next-block-index (fxquotient buffer-size 2)))
                        (loop size 0
                          (make-string buffer-size)
                          (cons (cons next-block-index next-block) blocks))
                        (loop size next-block-index next-block blocks)))))))))

    (set-who! get-string-n
      (lambda (textual-input-port count)
        (unless (and (input-port? textual-input-port) (textual-port? textual-input-port))
          ($oops who "~s is not a textual input port" textual-input-port))
        (unless (and (fixnum? count) (fx>= count 0))
          ($oops who "~s is not a nonnegative fixnum" count))
        (let ([buffer-size (file-buffer-size)])
          (if (not ($fxu< buffer-size count))
              (let ([st (make-string count)])
                (let ([n (get-string-min-max
                          who textual-input-port st 0 count count)])
                  (if (eof-object? n) n (string-truncate! st n))))
              (let ([get-some (port-handler-get-some ($port-handler textual-input-port))])
                (let loop ([count count]
                           [size 0]
                           [next-block-index 0]
                           [next-block (make-string buffer-size)]
                           [blocks '()])
                  (let ([next-size (get-some who textual-input-port
                                     next-block next-block-index
                                     (fxmin count (fx- buffer-size next-block-index)))])
                    (if (or (eof-object? next-size) (eq? next-size 0))
                        (if (eqv? size 0)
                            (if (eof-object? next-size) (eof-object) "")
                            (append-blocks size next-block-index next-block blocks))
                        (let ([count (fx- count next-size)]
                              [size (fx+ size next-size)]
                              [next-block-index (fx+ next-block-index next-size)])
                          (if (eqv? count 0)
                              (append-blocks size next-block-index next-block blocks)
                              (if (fx>= next-block-index (fxquotient buffer-size 2))
                                  (loop count size 0
                                    (make-string buffer-size)
                                    (cons (cons next-block-index next-block) blocks))
                                  (loop count size next-block-index next-block blocks))))))))))))

    (set-who! get-string-n!
      (lambda (textual-input-port st start count)
        (unless (and (input-port? textual-input-port) (textual-port? textual-input-port))
          ($oops who "~s is not a textual input port" textual-input-port))
        (unless (string? st)
          ($oops who "~s is not a string" st))
        (unless (and (fixnum? start) (fx>= start 0))
          ($oops who "invalid start value ~s" start))
        (unless (and (fixnum? count) (fx>= count 0))
          ($oops who "invalid count ~s" count))
        (unless (fx<= count (fx- (string-length st) start)) ; avoid overflow
          ($oops who "index ~s + count ~s is beyond the end of ~s" start count st))
        (get-string-min-max who textual-input-port st start count count)))

    (set-who! get-string-some
      (lambda (textual-input-port)
        (unless (and (input-port? textual-input-port) (textual-port? textual-input-port))
          ($oops who "~s is not a textual input port" textual-input-port))
        (let ([buffer-size (file-buffer-size)])
          (let ([st (make-string buffer-size)])
            (let ([n (get-string-min-max who textual-input-port st 0 0 buffer-size)])
              (if (eof-object? n)
                  (eof-object)
                  (string-truncate! st n)))))))

    (set-who! get-string-some!
      (lambda (textual-input-port st start count)
        (unless (and (input-port? textual-input-port) (textual-port? textual-input-port))
          ($oops who "~s is not a textual input port" textual-input-port))
        (unless (string? st)
          ($oops who "~s is not a string" st))
        (unless (and (fixnum? start) (fx>= start 0))
          ($oops who "invalid start value ~s" start))
        (unless (and (fixnum? count) (fx>= count 0))
          ($oops who "invalid count ~s" count))
        (unless (fx<= count (fx- (string-length st) start)) ; avoid overflow
          ($oops who "index ~s + count ~s is beyond the end of ~s" start count st))
        (get-string-min-max who textual-input-port st start 0 count)))

    (set-who! get-string-all
      (lambda (textual-input-port)
        (unless (and (input-port? textual-input-port) (textual-port? textual-input-port))
          ($oops who "~s is not a textual input port" textual-input-port))
        ($get-string-all who textual-input-port (file-buffer-size) #f)))

    (set-who! bytevector->string
      (lambda (bv tx)
        (unless (bytevector? bv)
          ($oops who "~s is not a bytevector" bv))
        (unless ($transcoder? tx)
          ($oops who "~s is not a transcoder" tx))
        (let* ([bv-len (bytevector-length bv)]
               [default (file-buffer-size)]
               ;; We only call transcoded-port's get-some handler and always
               ;; with our own buffer, so minimize the port's unused string
               ;; buffer and associated ioffsets fxvector.
               [bp (parameterize ([transcoded-port-buffer-size 1])
                     (open-bytevector-input-port bv tx))]
               ;; With current codecs, bv-len is an upper bound on the length of str.
               ;; We leave room for one exta char so that $get-string-all can loop to
               ;; realize it's at eof.
               [str (if (and (fx<= bv-len default) (fx< bv-len (most-positive-fixnum)))
                        ($get-string-all who bp (fx+ bv-len 1) #t)
                        ($get-string-all who bp default #f))])
          (if (eof-object? str) "" str))))
    )

  (set-who! get-line
    (lambda (tp)
      (unless (and (input-port? tp) (textual-port? tp))
        ($oops who "~s is not a textual input port" tp))
      (let f ([n 0])
        (let ([c (get-char tp)])
          (cond
            [(eof-object? c) (if (fx= n 0) c (begin (unget-char tp c) (make-string n)))]
            [(char=? c #\newline) (make-string n)]
            [else (let ([s (f (fx+ n 1))]) (string-set! s n c) s)])))))

  ;; get-datum in read.ss

;;;; 8.2.10 Output ports
  ;; output-port? in prims.ss
  (let ()
    (define who 'flush-output-port)
    (define flush-help
      (lambda (output-port)
        (call-port-handler flush who output-port)))
    (define flush-check-help
      (lambda (output-port)
        (unless (output-port? output-port)
          ($oops who "~s is not an output port" output-port))
        (flush-help output-port)))
    (set! flush-output-port
      (case-lambda
        [() (flush-help (current-output-port))]
        [(output-port) (flush-check-help output-port)]))
    (set! r6rs:flush-output-port
      (rec flush-output-port
        (lambda (output-port)
          (flush-check-help output-port)))))

 ; input-port-buffer-mode isn't required by r6rs but would be essentially
 ; the same code.  if anything, it would be even more useless.
  (set-who! output-port-buffer-mode
    (lambda (output-port)
      (unless (output-port? output-port)
        ($oops who "~s is not an output port" output-port))
      (cond
        [($port-flags-set? output-port (constant port-flag-block-buffered))
         (buffer-mode block)]
        [($port-flags-set? output-port (constant port-flag-line-buffered))
         (buffer-mode line)]
        [else (buffer-mode none)])))

  ;; open-file-output-port
  (let ()
    (define open-binary-file-output-port
      (lambda (who filename options perms b-mode)
        (let ([no-create (enum-set-subset? (file-options no-create) options)]
              [no-fail (enum-set-subset? (file-options no-fail) options)]
              [no-truncate (enum-set-subset? (file-options no-truncate) options)]
              [append (enum-set-subset? (file-options append) options)]
              [lock (enum-set-subset? (file-options exclusive) options)]
              [replace (enum-set-subset? (file-options replace) options)]
              [compressed (enum-set-subset? (file-options compressed) options)])
          (when (and compressed lock)
            ($oops who "exclusive option is not supported with compress option"))
          (when-feature windows
            (unless-feature pthreads
             ; try to work around windows file open semantics by trying
             ; to close any open ports to the file if we cannot delete it
             ; without doing so.
              (when replace
                (delete-file filename #f)
                (when (file-exists? filename)
                  (collect (collect-maximum-generation))))))
          (let ([fd (critical-section
                     ($open-output-fd filename perms
                        (fxior (if no-create (constant open-fd-no-create) 0)
                               (if no-fail (constant open-fd-no-fail) 0)
                               (if no-truncate (constant open-fd-no-truncate) 0)
                               (if append (constant open-fd-append) 0)
                               (if lock (constant open-fd-lock) 0)
                               (if replace (constant open-fd-replace) 0)
                               (if compressed (constant open-fd-compressed) 0))))])
            (when (pair? fd) (open-oops who filename options fd))
            (open-binary-fd-output-port who filename fd #t b-mode lock compressed)))))

    (define help-open-file-output-port
      (lambda (who filename options perms b-mode maybe-transcoder)
        (let ([bp (open-binary-file-output-port who filename options perms b-mode)])
          (if maybe-transcoder
              (transcoded-port bp maybe-transcoder)
              bp))))

    (define open-binary-standard-output-port
      (lambda (who fd name b-mode)
        (unless (buffer-mode? b-mode)
          ($oops who "~s is not a valid buffer mode" b-mode))
        (open-binary-fd-output-port who name (make-fd fd) ($fd-regular? fd) b-mode #f #f)))

    (set-who! open-file-output-port
      (rec open-file-output-port
        (case-lambda
          [(filename) (open-file-output-port filename (file-options))]
          [(filename options) (open-file-output-port filename options (buffer-mode block))]
          [(filename options b-mode) (open-file-output-port filename options b-mode #f)]
          [(filename options b-mode maybe-transcoder)
           (unless (string? filename) ($oops who "~s is not a string" filename))
           (unless (and (enum-set? options) (enum-set-subset? options $file-options))
             ($oops who "~s is not a file-options object" options))
           (unless (buffer-mode? b-mode)
             ($oops who "~s is not a valid buffer mode" b-mode))
           (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
             ($oops who "~s is not #f or a transcoder" maybe-transcoder))
           (help-open-file-output-port who filename options
             (extract-permission-mask options) b-mode maybe-transcoder)])))

    (set! $open-file-output-port
      (rec $open-file-output-port
        (case-lambda
          [(who filename) ($open-file-output-port who filename (file-options))]
          [(who filename options) ($open-file-output-port who filename options (buffer-mode block))]
          [(who filename options b-mode) ($open-file-output-port who filename options b-mode #f)]
          [(who filename options b-mode maybe-transcoder)
           (unless (string? filename) ($oops who "~s is not a string" filename))
           (unless (and (enum-set? options) (enum-set-subset? options $file-options))
             ($oops who "~s is not a file-options object" options))
           (unless (buffer-mode? b-mode)
             ($oops who "~s is not a valid buffer mode" b-mode))
           (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
             ($oops who "~s is not #f or a transcoder" maybe-transcoder))
           (help-open-file-output-port who filename options
             (extract-permission-mask options) b-mode maybe-transcoder)])))

    (set-who! open-fd-output-port
      (case-lambda
        [(fd)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (open-binary-fd-output-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) (buffer-mode block) #f #f)]
        [(fd buffer-mode)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (unless (buffer-mode? buffer-mode)
           ($oops who "~s is not a buffer mode" buffer-mode))
         (open-binary-fd-output-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) buffer-mode #f #f)]
        [(fd buffer-mode maybe-transcoder)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (unless (buffer-mode? buffer-mode)
           ($oops who "~s is not a buffer mode" buffer-mode))
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (let ([bp (open-binary-fd-output-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) buffer-mode #f #f)])
           (if maybe-transcoder
               (transcoded-port bp maybe-transcoder)
               bp))]))

    (set-who! standard-output-port
      (case-lambda
        [() (open-binary-standard-output-port who 1 "stdout" (buffer-mode line))]
        [(b-mode) (open-binary-standard-output-port who 1 "stdout" b-mode)]
        [(b-mode maybe-transcoder)
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (let ([binary-port (open-binary-standard-output-port who 1 "stdout" b-mode)])
           (if maybe-transcoder
               (transcoded-port binary-port maybe-transcoder)
               binary-port))]))

    (set-who! r6rs:standard-output-port
      (rec standard-output-port
        (lambda ()
          (open-binary-standard-output-port who 1 "stdout" (buffer-mode line)))))

    (set-who! standard-error-port
      (case-lambda
        [() (open-binary-standard-output-port who 2 "stderr" (buffer-mode none))]
        [(b-mode) (open-binary-standard-output-port who 2 "stderr" b-mode)]
        [(b-mode maybe-transcoder)
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (let ([binary-port (open-binary-standard-output-port who 2 "stderr" b-mode)])
           (if maybe-transcoder
               (transcoded-port binary-port maybe-transcoder)
               binary-port))]))

    (set-who! r6rs:standard-error-port
      (rec standard-error-port
        (lambda ()
          (open-binary-standard-output-port who 2 "stderr" (buffer-mode none)))))

   ; simple i/o routines here to share helpers
    (let ()
      (define (oof who s o)
        (unless (string? s) ($oops who "~s is not a string" s))
        (let ([o (if (list? o) o (list o))])
          (let loop ([o o] [ifexists #f] [mode #o666] [zmode #f] [xmode #f] [bmode #f])
            (if (null? o)
                (help-open-file-output-port who s
                  (enum-set-union
                    (enum-set-union
                      (case ifexists
                        [(error) (file-options)]
                        [(truncate) (file-options no-fail)]
                        [(replace) (file-options no-fail no-truncate replace)]
                        [(append) (file-options append no-fail no-truncate)]
                        [else (file-options)])
                      (if (eq? zmode 'compressed) (file-options compressed) (file-options)))
                    (if (eq? xmode 'exclusive) (file-options exclusive) (file-options)))
                  mode
                  (if (eq? bmode 'unbuffered) (buffer-mode none) (buffer-mode block))
                  (current-transcoder))
                (case (car o)
                  [(error truncate replace append)
                   (check-option who ifexists (car o))
                   (loop (cdr o) (car o) mode zmode xmode bmode)]
                  [(compressed uncompressed)
                   (check-option who zmode (car o))
                   (loop (cdr o) ifexists mode (car o) xmode bmode)]
                  [(buffered unbuffered)
                   (check-option who bmode (car o))
                   (loop (cdr o) ifexists mode zmode xmode (car o))]
                  [(exclusive nonexclusive)
                   (check-option who xmode (car o))
                   (loop (cdr o) ifexists mode zmode (car o) bmode)]
                  [(mode)
                   (if (null? (cdr o))
                       ($oops who "mode option requires an argument")
                       (let ([mode (cadr o)])
                         (if (and (fixnum? mode) (fx>= mode 0))
                             (loop (cddr o) ifexists mode zmode xmode bmode)
                             ($oops who "mode argument must be a nonnegative fixnum"))))]
                  [else ($oops who "invalid option ~s" (car o))])))))

      (set-who! #(r6rs: open-output-file)
        (lambda (s) (oof who s '())))

      (set-who! open-output-file
        (case-lambda
          [(s) (oof who s '())]
          [(s o) (oof who s o)]))

      (let ()
        (define (cwof who s f o)
          (unless (procedure? f)
            ($oops who "~s is not a procedure" f))
          (let ([p (oof who s o)])
            (call-with-values
              (lambda () (f p))
              (lambda args
                (close-output-port p)
                (apply values args)))))
        (set-who! #(r6rs: call-with-output-file)
          (lambda (s f) (cwof who s f '())))
        (set-who! call-with-output-file
          (case-lambda
            [(s f) (cwof who s f '())]
            [(s f o) (cwof who s f o)])))

      (let ()
        (define (wotf who s f o)
          (unless (procedure? f)
            ($oops who "~s is not a procedure" f))
          (let ([p (oof who s o)])
            (call-with-values
              (lambda () (parameterize ([current-output-port p]) (f)))
              (lambda v
                (close-output-port p)
                (apply values v)))))
        (set-who! #(r6rs: with-output-to-file)
          (lambda (s f) (wotf who s f '())))
        (set-who! with-output-to-file
          (case-lambda
            [(s f) (wotf who s f '())]
            [(s f o) (wotf who s f o)]))))

    )

  ;; open-bytevector-output-port
  (let ()
    ;; if info-index != index, there was put/put-some after last set-pos
    ;;   and (max info-length index) is true length
    ;; if info-index == index, there was set-pos after last put/put-some
    ;;   and info-length is true length

    ;; Invariant: info-index <= index
    ;; Invariant: size = (max length index)
    ;; Invariant: if no put/put-some after last set-pos/set-length,
    ;;       then info-index = index and true length = info-length
    ;; Invariant: if put/put-some after last set-pos/set-length,
    ;;       then info-index < index and true length = max info-length index

    ;; It is always safe to increment index when count != 0
    ;; It is always safe to write at index when count != 0
    ;; Index always contains the current position
    ;; The only operation that needs to decrement index is set-position
    ;;   which needs to set info-index anyway

    (define-record-type bytevector-output-port-info
      (nongenerative)
      (opaque #t)
      (sealed #t)
      (fields
        (mutable index)
        (mutable length)
        (mutable nonblocking)))

    ;; NOTE: leaves index at 0, callers must reset index if needed
    (define (extend-buffer p count)
      (let ([old-size (binary-port-output-size p)]
            [old-buffer (binary-port-output-buffer p)]
            [old-index (binary-port-output-index p)])
      (let* ([new-length (fxmax bytevector-buffer-length
                                (fx* 2 (fx+ old-size count)))]
             [new-buffer (make-bytevector new-length)])
          (bytevector-copy! old-buffer 0 new-buffer 0
                            (fxmin (bytevector-length old-buffer) old-size))
          (set-binary-port-output-buffer! p new-buffer))))

    (define port-length
      (lambda (who p)
        (let ([info ($port-info p)]
              [index (binary-port-output-index p)])
          (let ([info-index (bytevector-output-port-info-index info)]
                [info-length (bytevector-output-port-info-length info)])
            (if (eq? index info-index)
                info-length ;; last op was set-pos
                (max index info-length)))))) ;; last op was put

    (define $bytevector-output-handler
      (make-port-handler
        [ready? #f]
        [lookahead #f]
        [unget #f]
        [get #f]
        [get-some #f]
        [clear-input #f]
        [put
         (lambda (who p x)
           (assert-not-closed who p)
           (let ([index (binary-port-output-index p)])
             (when (port-output-full? p) (extend-buffer p 0))
             (bytevector-u8-set! (binary-port-output-buffer p) index x)
             (set-binary-port-output-index! p (fx1+ index))))]
        [put-some
         (lambda (who p bv start count)
           (assert-not-closed who p)
           (let ([index (binary-port-output-index p)])
             (when ($fxu< (binary-port-output-count p) count) (extend-buffer p count))
             (bytevector-copy! bv start
               (binary-port-output-buffer p) index count)
             (set-binary-port-output-index! p (fx+ index count)))
           count)]
        [flush ; no-op on bytevector output ports
         (lambda (who p)
           (assert-not-closed who p))]
        [clear-output ; no-op on bytevector output ports
         (lambda (who p)
           (assert-not-closed who p))]
        [close-port
         (lambda (who p)
           (unless (port-closed? p)
            ; sync info-index for possible post-close extraction
             (let ([info ($port-info p)] [index (binary-port-output-index p)])
               (unless (eq? index (bytevector-output-port-info-index info))
                 (bytevector-output-port-info-length-set! info
                   (fxmax index (bytevector-output-port-info-length info)))))
             (mark-port-closed! p)
             (set-binary-port-output-size! p 0)))]
        [port-position
         (lambda (who p)
           (assert-not-closed who p)
           (binary-port-output-index p))]
        [set-port-position!
         (lambda (who p pos)
           (assert-not-closed who p)
           (unless (and (fixnum? pos) (fx>= pos 0))
             (if (and (bignum? pos) (>= pos 0))
                 (position-oops who p pos "out of range")
                 ($oops who "~s is not a valid position" pos)))
           (let ([info ($port-info p)]
                 [index (binary-port-output-index p)])
            ; unless last op was set-pos, save the true length
             (unless (eq? index (bytevector-output-port-info-index info))
               (bytevector-output-port-info-length-set! info
                 (fxmax index (bytevector-output-port-info-length info))))
             (set-binary-port-output-size! p
               (fxmax pos (fx1- (bytevector-length (binary-port-output-buffer p)))))
             (set-binary-port-output-index! p pos)
             (bytevector-output-port-info-index-set! info pos)))]
        [port-length
         (lambda (who p)
           (assert-not-closed who p)
           (port-length who p))]
        [set-port-length!
         (lambda (who p pos)
           (unless (and (fixnum? pos) (fx>= pos 0))
             (if (and (bignum? pos) (>= pos 0))
                 (position-oops who p pos "out of range")
                 ($oops who "~s is not a valid length" pos)))
           (assert-not-closed who p)
           (let ([info ($port-info p)]
                 [index (binary-port-output-index p)]
                 [size (binary-port-output-size p)])
             ;; ensure the bytevector is long enough
             (let ([buflen-1 (fx1- (bytevector-length (binary-port-output-buffer p)))])
               (when ($fxu< buflen-1 pos)
                 (extend-buffer p (fx- pos buflen-1))
                 (set-binary-port-output-index! p index)))
             ;; make it look like a set-pos was done last
             ;; (i.e. index might be beyond true length)
             (bytevector-output-port-info-index-set! info index)
             ;; set the true length
             (bytevector-output-port-info-length-set! info pos)))]
        [port-nonblocking?
         (lambda (who p)
           (assert-not-closed who p)
           (bytevector-output-port-info-nonblocking ($port-info p)))]
        [set-port-nonblocking!
         (lambda (who p x)
           (assert-not-closed who p)
           (bytevector-output-port-info-nonblocking-set! ($port-info p) x))]))

    (define extractor
      (lambda (p)
        (let ([old-buffer
               (bytevector-truncate!
                 (binary-port-output-buffer p)
                 (port-length #f p))])
          (set-binary-port-output-buffer! p #vu8())
          (let ([info ($port-info p)])
            (bytevector-output-port-info-index-set! info 0)
            (bytevector-output-port-info-length-set! info 0))
          old-buffer)))

    (define open-binary-bytevector-output-port
      (lambda ()
        (let ([p ($make-binary-output-port "bytevector"
                   $bytevector-output-handler
                   #vu8()
                   (make-bytevector-output-port-info 0 0 #f))])
          ($set-port-flags! p (constant port-flag-block-buffered))
          (values
           p
           (lambda ()
             (let ([info ($port-info p)])
               (if (bytevector-output-port-info? info)
                   (extractor p)
                  ; the port must have been transcoded
                   (begin
                     (flush-output-port info)
                     (extractor (codec-info-bp ($port-info info)))))))))))

    (set-who! open-bytevector-output-port
      (case-lambda
       [() (open-binary-bytevector-output-port)]
       [(maybe-transcoder)
        (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
          ($oops who "~s is not #f or a transcoder" maybe-transcoder))
        (let-values ([(binary-port extractor)
                      (open-binary-bytevector-output-port)])
          (values
           (if maybe-transcoder
               (transcoded-port binary-port maybe-transcoder)
               binary-port)
           extractor))]))
    )

  ;; open-bytevector-list-output-port
  (let ()
    (define-record-type bv-list-op-info
      (nongenerative)
      (sealed #t)
      (fields
        (mutable nonblocking)
        (mutable bv*)))

    ; allocate in chunk-size chunks
    (define chunk-size 4096)

    (define (extend-buffer p)
      (let ([bv (binary-port-output-buffer p)])
        (unless (eqv? bv #vu8())
          (let ([info ($port-info p)])
            (bv-list-op-info-bv*-set! info
              (cons bv (bv-list-op-info-bv* info))))))
      (set-binary-port-output-buffer! p (make-bytevector chunk-size)))

    (define $bytevector-list-output-handler
      (make-port-handler
        [ready? #f]
        [lookahead #f]
        [unget #f]
        [get #f]
        [get-some #f]
        [clear-input #f]
        [put
          (lambda (who p x)
            (assert-not-closed who p)
            (when (port-output-full? p) (extend-buffer p))
            (let ([index (binary-port-output-index p)])
              (bytevector-u8-set! (binary-port-output-buffer p) index x)
              (set-binary-port-output-index! p (fx1+ index))))]
        [put-some
          (lambda (who p bv start count)
            (assert-not-closed who p)
            (when (port-output-full? p) (extend-buffer p))
            (let ([count (fxmin count (binary-port-output-count p))]
                  [index (binary-port-output-index p)])
              (bytevector-copy! bv start (binary-port-output-buffer p) index count)
              (set-binary-port-output-index! p (fx+ index count))
              count))]
        [flush ; no-op on bytevector output ports
          (lambda (who p)
            (assert-not-closed who p))]
        [clear-output ; no-op on bytevector output ports
          (lambda (who p)
            (assert-not-closed who p))]
        [close-port
          (lambda (who p)
            (unless (port-closed? p)
              ; sync info-index for possible post-close extraction
              #;(let ([old-buffer (bytevector-truncate!
                                    (binary-port-output-buffer p)
                                    (binary-port-output-index p))]
                      [bv* (bv-list-op-info-bv* info)])
                  (bv-list-op-info-size-set! info
                    (fx+ (bytevector-length old-buffer)
                      (fx* (length bv*) chunk-size)))
                  (bv-list-op-info-bv*-set! info
                    (reverse (if (eq? old-buffer #vu8())
                                 bv*
                                 (cons old-buffer bv*)))))
              (mark-port-closed! p)
              (set-binary-port-output-size! p 0)))]
        [port-position
          (lambda (who p)
            (assert-not-closed who p)
            (fx+ (binary-port-output-index p)
              (fx* (length (bv-list-op-info-bv* ($port-info p))) chunk-size)))]
        [set-port-position! #f]
        [port-length
          (lambda (who p)
            (assert-not-closed who p)
            (fx+ (binary-port-output-index p)
              (fx* (length (bv-list-op-info-bv* ($port-info p))) chunk-size)))]
        [set-port-length! #f]
        [port-nonblocking?
          (lambda (who p)
            (assert-not-closed who p)
            (bv-list-op-info-nonblocking ($port-info p)))]
        [set-port-nonblocking!
          (lambda (who p x)
            (assert-not-closed who p)
            (bv-list-op-info-nonblocking-set! ($port-info p) x))]))

    (define extractor
      (lambda (p)
        (let ([info ($port-info p)])
          (let ([bv (bytevector-truncate!
                      (binary-port-output-buffer p)
                      (binary-port-output-index p))]
                [bv* (bv-list-op-info-bv* info)])
            (let ([size (fx+ (bytevector-length bv) (fx* (length bv*) chunk-size))])
              (set-binary-port-output-buffer! p #vu8())
              (bv-list-op-info-bv*-set! info '())
              (values (reverse (if (eqv? bv #vu8()) bv* (cons bv bv*))) size))))))

    (set-who! $open-bytevector-list-output-port
      (lambda ()
        (let ([p ($make-binary-output-port "bytevector-list"
                   $bytevector-list-output-handler
                   #vu8()
                   (make-bv-list-op-info #f '()))])
          ($set-port-flags! p (constant port-flag-block-buffered))
          (values p (lambda () (extractor p)))))))

  (let ()
    (define ($call-with-bytevector-output-port who proc maybe-transcoder)
      (let-values ([(port extractor) (open-bytevector-output-port maybe-transcoder)])
        (proc port)
        (let ([bv (extractor)])
          (call-port-handler close-port who port)
          bv)))

    (set-who! call-with-bytevector-output-port
      (case-lambda
        [(proc)
         (unless (procedure? proc) ($oops who "~s is not a procedure" proc))
         ($call-with-bytevector-output-port who proc #f)]
        [(proc maybe-transcoder)
         (unless (procedure? proc)
           ($oops who "~s is not a procedure" proc))
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not a transcoder" maybe-transcoder))
         ($call-with-bytevector-output-port who proc maybe-transcoder)]))

    (set-who! string->bytevector
      (lambda (str tx)
        (unless (string? str)
          ($oops who "~s is not a string" str))
        (unless ($transcoder? tx)
          ($oops who "~s is not a transcoder" tx))
        ($call-with-bytevector-output-port who
          (lambda (op) (put-string op str))
          tx))))

  ;; open-string-output-port
  (let ()
    ;; see open-bytevector-output-port for explanation of algorithm

    (define-record-type string-output-port-info
      (nongenerative)
      (opaque #t)
      (sealed #t)
      (fields
        (mutable index)
        (mutable length)
        (mutable nonblocking)))

    ;; NOTE: leaves index at 0, callers must reset index if needed
    (define (extend-buffer p count)
      (let ([old-size (textual-port-output-size p)]
            [old-buffer (textual-port-output-buffer p)]
            [old-index (textual-port-output-index p)])
      (let* ([new-length (fxmax string-buffer-length
                                (fx* 2 (fx+ old-size count)))]
             [new-buffer (make-string new-length)])
          (string-copy! old-buffer 0 new-buffer 0
                        (fxmin (string-length old-buffer) old-size))
          (set-textual-port-output-buffer! p new-buffer))))

    (define port-length
       (lambda (who p)
         (let ([info ($port-info p)]
               [index (textual-port-output-index p)])
           (let ([info-index (string-output-port-info-index info)]
                 [info-length (string-output-port-info-length info)])
             (if (eq? index info-index)
                 info-length ;; last op was set-pos
                 (max index info-length)))))) ;; last op was put

    (define $string-output-handler
      (make-port-handler
        [ready? #f]
        [lookahead #f]
        [unget #f]
        [get #f]
        [get-some #f]
        [clear-input #f]
        [put
         (lambda (who p x)
           (assert-not-closed who p)
           (let ([index (textual-port-output-index p)])
             (when (port-output-full? p) (extend-buffer p 0))
             (string-set! (textual-port-output-buffer p) index x)
             (set-textual-port-output-index! p (fx1+ index))))]
        [put-some
         (lambda (who p st start count)
           (assert-not-closed who p)
           (let ([index (textual-port-output-index p)])
             (when ($fxu< (textual-port-output-count p) count) (extend-buffer p count))
             (string-copy! st start
               (textual-port-output-buffer p) index count)
             (set-textual-port-output-index! p (fx+ index count)))
           count)]
        [flush ; no-op on string output ports
         (lambda (who p)
           (assert-not-closed who p))]
        [clear-output ; no-op on string output ports
         (lambda (who p)
           (assert-not-closed who p))]
        [close-port
         (lambda (who p)
           (unless (port-closed? p)
            ; sync info-index for possible post-close extraction
             (let ([info ($port-info p)] [index (textual-port-output-index p)])
               (unless (eq? index (string-output-port-info-index info))
                 (string-output-port-info-length-set! info
                   (fxmax index (string-output-port-info-length info)))))
             (mark-port-closed! p)
             (set-textual-port-output-size! p 0)))]
        [port-position
         (lambda (who p)
           (assert-not-closed who p)
           (textual-port-output-index p))]
        [set-port-position!
         (lambda (who p pos)
           (assert-not-closed who p)
           (unless (and (fixnum? pos) (fx>= pos 0))
             (if (and (bignum? pos) (>= pos 0))
                 (position-oops who p pos "out of range")
                 ($oops who "~s is not a valid position" pos)))
           (let ([info ($port-info p)]
                 [index (textual-port-output-index p)])
            ; unless last op was set-pos, save the true length
             (unless (eq? index (string-output-port-info-index info))
               (string-output-port-info-length-set! info
                 (fxmax index (string-output-port-info-length info))))
             (set-textual-port-output-size! p
               (fxmax pos (fx1- (string-length (textual-port-output-buffer p)))))
             (set-textual-port-output-index! p pos)
             (string-output-port-info-index-set! info pos)))]
        [port-length
         (lambda (who p)
           (assert-not-closed who p)
           (port-length who p))]
        [set-port-length!
         (lambda (who p pos)
           (unless (and (fixnum? pos) (fx>= pos 0))
             (if (and (bignum? pos) (>= pos 0))
                 (position-oops who p pos "out of range")
                 ($oops who "~s is not a valid length" pos)))
           (assert-not-closed who p)
           (let ([info ($port-info p)]
                 [index (textual-port-output-index p)]
                 [size (textual-port-output-size p)])
             ;; ensure the bytevector is long enough
             (let ([buflen-1 (fx1- (string-length (textual-port-output-buffer p)))])
               (when ($fxu< buflen-1 pos)
                 (extend-buffer p (fx- pos buflen-1))
                 (set-textual-port-output-index! p index)))
             ;; make it look like a set-pos was done last
             ;; (i.e. index might be beyond true length)
             (string-output-port-info-index-set! info index)
             ;; set the true length
             (string-output-port-info-length-set! info pos)))]
        [port-nonblocking?
         (lambda (who p)
           (assert-not-closed who p)
           (string-output-port-info-nonblocking ($port-info p)))]
        [set-port-nonblocking!
         (lambda (who p x)
           (assert-not-closed who p)
           (string-output-port-info-nonblocking-set! ($port-info p) x))]))

    (define ($open-string-output-port)
      (let ([p ($make-textual-output-port "string"
                 $string-output-handler
                 ""
                 (make-string-output-port-info 0 0 #f))])
        ($set-port-flags! p (constant port-flag-block-buffered))
        ($set-port-flags! p (constant port-flag-char-positions))
        ($set-port-flags! p (constant port-flag-bol))
        p))

    (define ($get-output-string p)
      (let ([old-buffer
             (string-truncate!
               (textual-port-output-buffer p)
               (port-length #f p))])
        (set-textual-port-output-buffer! p "")
        (let ([info ($port-info p)])
          (string-output-port-info-index-set! info 0)
          (string-output-port-info-length-set! info 0))
        old-buffer))

    (set-who! open-string-output-port
      (lambda ()
        (let ([p ($open-string-output-port)])
          (values p (lambda () ($get-output-string p))))))

    (set-who! open-output-string
      (lambda ()
        ($open-string-output-port)))

    (set-who! get-output-string
      (lambda (p)
        (unless (and (port? p) (eq? ($port-handler p) $string-output-handler))
          ($oops who "~s is not a string output port" p))
        ($get-output-string p)))
    )


  (set-who! call-with-string-output-port
    (lambda (proc)
      (unless (procedure? proc)
        ($oops who "~s is not a procedure" proc))
      (let-values ([(port extractor) (open-string-output-port)])
        (proc port)
        (let ([st (extractor)])
          (call-port-handler close-port who port)
          st))))

  ;; current-output-port and current-error-port are in prims.ss

  (set-who! make-custom-binary-output-port
    (lambda (id write! get-position set-position! close)
      (unless (string? id) ($oops who "~s is not a string" id))
      (unless (procedure? write!) ($oops who "~s is not a procedure" write!))
      (unless (or (not get-position) (procedure? get-position))
        ($oops who "~s is not a procedure or #f" get-position))
      (unless (or (not set-position!) (procedure? set-position!))
        ($oops who "~s is not a procedure or #f" set-position!))
      (unless (or (not close) (procedure? close))
        ($oops who "~s is not a procedure or #f" close))
      (let ([handler
             (make-port-handler
               [ready? #f]
               [lookahead #f]
               [unget #f]
               [get #f]
               [get-some #f]
               [clear-input #f]
               [put
                (lambda (who p x)
                  (assert-not-closed who p)
                  (binary-custom-port-put who p write! x))]
               [put-some
                (lambda (who p bv start count)
                  (assert-not-closed who p)
                  (binary-custom-port-put-some who p write! bv start count))]
               [flush
                (lambda (who p)
                  (assert-not-closed who p)
                  (binary-custom-port-flush who p write!))]
               [clear-output
                (lambda (who p)
                  (assert-not-closed who p)
                  (binary-custom-port-clear-output who p))]
               [close-port
                (lambda (who p)
                  (unless (port-closed? p)
                    (binary-custom-port-flush who p write!)
                    (binary-custom-port-close-port who p close)))]
               [port-position
                (and get-position
                     (lambda (who p)
                       (assert-not-closed who p)
                       (binary-custom-port-port-position out who p get-position)))]
               [set-port-position!
                (and set-position!
                     (lambda (who p x)
                       (unless (or (and (fixnum? x) (fx>= x 0)) (and (bignum? x) (>= x 0)))
                         ($oops who "~s is not a valid position" x))
                       (assert-not-closed who p)
                       (binary-custom-port-flush who p write!)
                       (set-position! x)))]
               [port-length #f]
               [set-port-length! #f]
               [port-nonblocking? #f]
               [set-port-nonblocking! #f])])
        (let ([bufsiz (custom-port-buffer-size)])
          (let ([p ($make-binary-output-port id handler (make-bytevector bufsiz) #f)])
            ($set-port-flags! p (constant port-flag-block-buffered))
            (set-binary-port-output-size! p (fx1- bufsiz)) ;; leave room for put to work
            p)))))

  (set-who! make-custom-textual-output-port
    (lambda (id write! get-position set-position! close)
      (unless (string? id) ($oops who "~s is not a string" id))
      (unless (procedure? write!) ($oops who "~s is not a procedure" write!))
      (unless (or (not get-position) (procedure? get-position))
        ($oops who "~s is not a procedure or #f" get-position))
      (unless (or (not set-position!) (procedure? set-position!))
        ($oops who "~s is not a procedure or #f" set-position!))
      (unless (or (not close) (procedure? close))
        ($oops who "~s is not a procedure or #f" close))
      (let ([handler
             (make-port-handler
               [ready? #f]
               [lookahead #f]
               [unget #f]
               [get #f]
               [get-some #f]
               [clear-input #f]
               [put
                (lambda (who p x)
                  (assert-not-closed who p)
                  (textual-custom-port-put who p write! x))]
               [put-some
                (lambda (who p str start count)
                  (assert-not-closed who p)
                  (textual-custom-port-put-some who p write! str start count))]
               [flush
                (lambda (who p)
                  (assert-not-closed who p)
                  (textual-custom-port-flush who p write!))]
               [clear-output
                (lambda (who p)
                  (assert-not-closed who p)
                  (textual-custom-port-clear-output who p))]
               [close-port
                (lambda (who p)
                  (unless (port-closed? p)
                    (textual-custom-port-flush who p write!)
                    (textual-custom-port-close-port who p close)))]
               [port-position
                (and get-position
                     (lambda (who p)
                       (assert-not-closed who p)
                       (textual-custom-port-flush who p write!)
                       (get-position)))]
               [set-port-position!
                (and set-position!
                     (lambda (who p x)
                       (assert-not-closed who p)
                       (textual-custom-port-flush who p write!)
                       (set-position! x)))]
               [port-length #f]
               [set-port-length! #f]
               [port-nonblocking? #f]
               [set-port-nonblocking! #f])])
        (let ([bufsiz (custom-port-buffer-size)])
          (let ([p ($make-textual-output-port id handler (make-string bufsiz) #f)])
            ($set-port-flags! p (constant port-flag-block-buffered))
            ($set-port-flags! p (constant port-flag-bol))
            (set-textual-port-output-size! p (fx1- bufsiz)) ;; leave room for put to work
            p)))))


;;;; 8.2.11 Binary output
  ;; put-u8 in prims.ss

  (set-who! put-bytevector
    (case-lambda
      [(binary-output-port bv)
       (unless (and (output-port? binary-output-port) (binary-port? binary-output-port))
         ($oops who "~s is not a binary output port" binary-output-port))
       (unless (bytevector? bv)
         ($oops who "~s is not a bytevector" bv))
       (#3%put-bytevector binary-output-port bv)]
      [(binary-output-port bv start)
       (unless (and (output-port? binary-output-port) (binary-port? binary-output-port))
         ($oops who "~s is not a binary output port" binary-output-port))
       (unless (bytevector? bv)
         ($oops who "~s is not a bytevector" bv))
       (unless (and (fixnum? start) (not ($fxu< (bytevector-length bv) start)))
         ($oops who "invalid start value ~s" start))
       (#3%put-bytevector binary-output-port bv start)]
      [(binary-output-port bv start count)
       (unless (and (output-port? binary-output-port) (binary-port? binary-output-port))
         ($oops who "~s is not a binary output port" binary-output-port))
       (unless (bytevector? bv)
         ($oops who "~s is not a bytevector" bv))
       (unless (and (fixnum? start) (fx<= 0 start))
         ($oops who "invalid start value ~s" start))
       (unless (and (fixnum? count) (fx<= 0 count))
         ($oops who "invalid count ~s" count))
       (unless (fx<= count (fx- (bytevector-length bv) start)) ; avoid overflow
         ($oops who "index ~s + count ~s is beyond the end of ~s" start count bv))
       (#3%put-bytevector binary-output-port bv start count)]))

  ;; not in R6RS
  (set-who! put-bytevector-some
    (case-lambda
      [(binary-output-port bv)
       (unless (and (output-port? binary-output-port) (binary-port? binary-output-port))
         ($oops who "~s is not a binary output port" binary-output-port))
       (unless (bytevector? bv)
         ($oops who "~s is not a bytevector" bv))
       (#3%put-bytevector-some binary-output-port bv)]
      [(binary-output-port bv start)
       (unless (and (output-port? binary-output-port) (binary-port? binary-output-port))
         ($oops who "~s is not a binary output port" binary-output-port))
       (unless (bytevector? bv)
         ($oops who "~s is not a bytevector" bv))
       (unless (and (fixnum? start) (not ($fxu< (bytevector-length bv) start)))
         ($oops who "invalid start value ~s" start))
       (#3%put-bytevector-some binary-output-port bv start)]
      [(binary-output-port bv start count)
       (unless (and (output-port? binary-output-port) (binary-port? binary-output-port))
         ($oops who "~s is not a binary output port" binary-output-port))
       (unless (bytevector? bv)
         ($oops who "~s is not a bytevector" bv))
       (unless (and (fixnum? start) (fx<= 0 start))
         ($oops who "invalid start value ~s" start))
       (unless (and (fixnum? count) (fx<= 0 count))
         ($oops who "invalid count ~s" count))
       (unless (fx<= count (fx- (bytevector-length bv) start)) ; avoid overflow
         ($oops who "index ~s + count ~s is beyond the end of ~s" start count bv))
       (#3%put-bytevector-some binary-output-port bv start count)]))

;;;; 8.2.12 Textual output
  ;; put-char in prims.ss

  (set-who! put-string
    (case-lambda
     [(textual-output-port str)
      (unless (and (output-port? textual-output-port) (textual-port? textual-output-port))
        ($oops who "~s is not a textual output port" textual-output-port))
      (unless (string? str)
        ($oops who "~s is not a string" str))
      (#3%put-string textual-output-port str)]
     [(textual-output-port str start)
      (unless (and (output-port? textual-output-port) (textual-port? textual-output-port))
        ($oops who "~s is not a textual output port" textual-output-port))
      (unless (string? str)
        ($oops who "~s is not a string" str))
      (unless (and (fixnum? start) (not ($fxu< (string-length str) start)))
        ($oops who "invalid start value ~s" start))
      (#3%put-string textual-output-port str start)]
     [(textual-output-port str start count)
      (unless (and (output-port? textual-output-port) (textual-port? textual-output-port))
        ($oops who "~s is not a textual output port" textual-output-port))
      (unless (string? str)
        ($oops who "~s is not a string" str))
      (unless (and (fixnum? start) (fx<= 0 start))
        ($oops who "invalid start value ~s" start))
      (unless (and (fixnum? count) (fx<= 0 count))
        ($oops who "invalid count value ~s" count))
      (unless (fx<= count (fx- (string-length str) start)) ; avoid overflow
        ($oops who "index ~s + count ~s is beyond the end of ~s" start count str))
      (#3%put-string textual-output-port str start count)]))

  ;; not in R6RS
  (set-who! put-string-some
    (case-lambda
     [(textual-output-port str)
      (unless (and (output-port? textual-output-port) (textual-port? textual-output-port))
        ($oops who "~s is not a textual output port" textual-output-port))
      (unless (string? str)
        ($oops who "~s is not a string" str))
      (#3%put-string-some textual-output-port str)]
     [(textual-output-port str start)
      (unless (and (output-port? textual-output-port) (textual-port? textual-output-port))
        ($oops who "~s is not a textual output port" textual-output-port))
      (unless (string? str)
        ($oops who "~s is not a string" str))
      (unless (and (fixnum? start) (not ($fxu< (string-length str) start)))
        ($oops who "invalid start value ~s" start))
      (#3%put-string-some textual-output-port str start)]
     [(textual-output-port str start count)
      (unless (and (output-port? textual-output-port) (textual-port? textual-output-port))
        ($oops who "~s is not a textual output port" textual-output-port))
      (unless (string? str)
        ($oops who "~s is not a string" str))
      (unless (and (fixnum? start) (fx<= 0 start))
        ($oops who "invalid start value ~s" start))
      (unless (and (fixnum? count) (fx<= 0 count))
        ($oops who "invalid count value ~s" count))
      (unless (fx<= count (fx- (string-length str) start)) ; avoid overflow
        ($oops who "index ~s + count ~s is beyond the end of ~s" start count str))
      (#3%put-string-some textual-output-port str start count)]))

  ;; put-datum in print.ss

;;;; 8.2.13 Input/output ports
  ;; open-file-input/output-port
  (let ()
    (define open-binary-file-input/output-port
      (lambda (who filename options perms b-mode)
        (let ([no-create (enum-set-subset? (file-options no-create) options)]
              [no-fail (enum-set-subset? (file-options no-fail) options)]
              [no-truncate (enum-set-subset? (file-options no-truncate) options)]
              [append (enum-set-subset? (file-options append) options)]
              [lock (enum-set-subset? (file-options exclusive) options)]
              [replace (enum-set-subset? (file-options replace) options)]
              [compressed (enum-set-subset? (file-options compressed) options)])
          (when (and compressed lock)
            ($oops who "exclusive option is not supported with compress option"))
          (when-feature windows
            (unless-feature pthreads
             ; try to work around windows file open semantics by trying
             ; to close any open ports to the file if we cannot delete it
             ; without doing so.
              (when replace
                (delete-file filename #f)
                (when (file-exists? filename)
                  (collect (collect-maximum-generation))))))
          (let ([fd (critical-section
                      ($open-input/output-fd filename perms
                        (fxior (if no-create (constant open-fd-no-create) 0)
                               (if no-fail (constant open-fd-no-fail) 0)
                               (if no-truncate (constant open-fd-no-truncate) 0)
                               (if append (constant open-fd-append) 0)
                               (if lock (constant open-fd-lock) 0)
                               (if replace (constant open-fd-replace) 0)
                               (if compressed (constant open-fd-compressed) 0))))])
            (when (pair? fd) (open-oops who filename options fd))
            (open-binary-fd-input/output-port who filename fd #t b-mode lock compressed)))))

    (define help-open-file-input/output-port
      (lambda (who filename options perms b-mode maybe-transcoder)
        (let ([bp (open-binary-file-input/output-port who filename options perms b-mode)])
          (if maybe-transcoder
              (transcoded-port bp maybe-transcoder)
              bp))))

    (set-who! open-file-input/output-port
      (rec open-file-input/output-port
        (case-lambda
          [(filename) (open-file-input/output-port filename (file-options))]
          [(filename options) (open-file-input/output-port filename options (buffer-mode block))]
          [(filename options b-mode) (open-file-input/output-port filename options b-mode #f)]
          [(filename options b-mode maybe-transcoder)
           (unless (string? filename) ($oops who "~s is not a string" filename))
           (unless (and (enum-set? options) (enum-set-subset? options $file-options))
             ($oops who "~s is not a file-options object" options))
           (unless (buffer-mode? b-mode) ($oops who "~s is not a valid buffer mode" b-mode))
           (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
             ($oops who "~s is not #f or a transcoder" maybe-transcoder))
           (help-open-file-input/output-port who filename options
             (extract-permission-mask options) b-mode maybe-transcoder)])))

    (set! $open-file-input/output-port
      (rec $open-file-input/output-port
        (case-lambda
          [(who filename) ($open-file-input/output-port who filename (file-options))]
          [(who filename options) ($open-file-input/output-port who filename options (buffer-mode block))]
          [(who filename options b-mode) ($open-file-input/output-port who filename options b-mode #f)]
          [(who filename options b-mode maybe-transcoder)
           (unless (string? filename) ($oops who "~s is not a string" filename))
           (unless (and (enum-set? options) (enum-set-subset? options $file-options))
             ($oops who "~s is not a file-options object" options))
           (unless (buffer-mode? b-mode) ($oops who "~s is not a valid buffer mode" b-mode))
           (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
             ($oops who "~s is not #f or a transcoder" maybe-transcoder))
           (help-open-file-input/output-port who filename options
             (extract-permission-mask options) b-mode maybe-transcoder)])))

    (set-who! open-fd-input/output-port
      (case-lambda
        [(fd)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (open-binary-fd-input/output-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) (buffer-mode block) #f #f)]
        [(fd buffer-mode)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (unless (buffer-mode? buffer-mode)
           ($oops who "~s is not a buffer mode" buffer-mode))
         (open-binary-fd-input/output-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) buffer-mode #f #f)]
        [(fd buffer-mode maybe-transcoder)
         (unless (and (fixnum? fd) (fx>= fd 0))
           ($oops who "~s is not a file descriptor" fd))
         (unless (buffer-mode? buffer-mode)
           ($oops who "~s is not a buffer mode" buffer-mode))
         (unless (or (not maybe-transcoder) ($transcoder? maybe-transcoder))
           ($oops who "~s is not #f or a transcoder" maybe-transcoder))
         (let ([binary-port
                (open-binary-fd-input/output-port who (format "fd ~s" fd) (make-fd fd) ($fd-regular? fd) buffer-mode #f #f)])
           (if maybe-transcoder
               (transcoded-port binary-port maybe-transcoder)
               binary-port))]))

    ; TODO: standard-input/output-port.  requires paired fds

   ; simple i/o routines here to share helpers
    (let ()
      (define (oiof who s o)
        (unless (string? s) ($oops who "~s is not a string" s))
        (let ([o (if (list? o) o (list o))])
          (let loop ([o o] [ifexists #f] [mode #o666] [xmode #f] [bmode #f])
            (if (null? o)
                (help-open-file-input/output-port who s
                  (enum-set-union
                    (case ifexists
                      [(error) (file-options)]
                      [(truncate) (file-options no-fail)]
                      [(replace) (file-options no-fail no-truncate replace)]
                      [(append) (file-options append no-fail no-truncate)]
                      [else (file-options no-fail no-truncate)])
                    (if (eq? xmode 'exclusive) (file-options exclusive) (file-options)))
                  mode
                  (if (eq? bmode 'unbuffered) (buffer-mode none) (buffer-mode block))
                  (current-transcoder))
                (case (car o)
                  [(error truncate replace append)
                   (check-option who ifexists (car o))
                   (loop (cdr o) (car o) mode xmode bmode)]
                  [(buffered unbuffered)
                   (check-option who bmode (car o))
                   (loop (cdr o) ifexists mode xmode (car o))]
                  [(exclusive nonexclusive)
                   (check-option who xmode (car o))
                   (loop (cdr o) ifexists mode (car o) bmode)]
                  [(mode)
                   (if (null? (cdr o))
                       ($oops who "mode option requires an argument")
                       (let ([mode (cadr o)])
                         (if (and (fixnum? mode) (fx>= mode 0))
                             (loop (cddr o) ifexists mode xmode bmode)
                             ($oops who "mode argument must be a nonnegative fixnum"))))]
                  [else ($oops who "invalid option ~s" (car o))])))))
      (set-who! open-input-output-file
        (case-lambda
          [(s) (oiof who s '())]
          [(s o) (oiof who s o)])))
    )

  ;; make-custom-binary-input/output-port
  (let ()
    (define-syntax make-ready-for-input
      (syntax-rules ()
        [(_ who p_ write!)
         (let ([p p_])
           (unless (eq? 0 (binary-port-output-size p))
             (binary-custom-port-flush who p write!)
             ;; don't set input-size; it is set only after a read
             (set-binary-port-output-size! p 0)))]))

    (module ((make-ready-for-output $make-ready-for-output))
      (define $make-ready-for-output
        (lambda (who p get-position set-position!)
          (unless (eq? (binary-port-input-size p) 0)
            (unless (port-input-empty? p)
              (if (not (and get-position set-position!))
                  (position-warning who
                    (if get-position
                        "cannot set position for write after read on ~s"
                        "cannot determine position for write after read on ~s")
                    p)
                  (set-position! (- (get-position) (binary-port-input-count p)))))
            (set-binary-port-input-size! p 0))
          (set-port-eof! p #f)
          (set-binary-port-output-size! p
            (fx1- (bytevector-length (binary-port-output-buffer p))))))

      (define-syntax make-ready-for-output
        (syntax-rules ()
          [(_ ?who ?p ?get-position ?set-position!)
           (let ([p ?p])
             (when (eq? (binary-port-output-size p) 0)
               ($make-ready-for-output ?who p ?get-position ?set-position!)))])))

    ;; Ports start with a non-ill-defined position.
    ;; Unless get-position and set-position! are provided,
    ;; doing a buffered read operation makes the position ill-defined.
    ;;
    ;; A put, put-some or (textual)port-position operation may give
    ;; unexpected results when the position is ill-defined.
    ;;
    ;; A set-port-position is sufficient to make
    ;; the position no longer ill-defined.
    ;;
    ;; Buffered read operations include lookahead, port-eof?, and unget.
    ;; Buffered read operations also include get and get-some if buffer-mode is not none.

    (set-who! make-custom-binary-input/output-port
      (lambda (id read! write! get-position set-position! close)
        (unless (string? id) ($oops who "~s is not a string" id))
        (unless (procedure? read!) ($oops who "~s is not a procedure" read!))
        (unless (procedure? write!) ($oops who "~s is not a procedure" write!))
        (unless (or (not get-position) (procedure? get-position))
          ($oops who "~s is not a procedure or #f" get-position))
        (unless (or (not set-position!) (procedure? set-position!))
          ($oops who "~s is not a procedure or #f" set-position!))
        (unless (or (not close) (procedure? close))
          ($oops who "~s is not a procedure or #f" close))
        (let ([handler
               (make-port-handler
                 [ready?
                  (lambda (who p)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (or (not (port-input-empty? p))
                        (port-flag-eof-set? p)
                        (read-oops who p "cannot determine ready status")))]
                 [lookahead
                  (lambda (who p)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (binary-custom-port-lookahead who p read!))]
                 [unget
                  (lambda (who p x)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (binary-custom-port-unget who p x))]
                 [get
                  (lambda (who p)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (binary-custom-port-get who p read!))]
                 [get-some
                  (lambda (who p bv start count)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (binary-custom-port-get-some who p read! bv start count))]
                 [clear-input
                  (lambda (who p)
                    (assert-not-closed who p)
                    (binary-custom-port-clear-input who p))]
                 [put
                  (lambda (who p x)
                    (assert-not-closed who p)
                    (make-ready-for-output who p get-position set-position!)
                    (binary-custom-port-put who p write! x))]
                 [put-some
                  (lambda (who p bv start count)
                    (assert-not-closed who p)
                    (make-ready-for-output who p get-position set-position!)
                    (binary-custom-port-put-some who p write! bv start count))]
                 [flush
                  (lambda (who p)
                    (assert-not-closed who p)
                   ; binary-custom-port-flush must be a no-op in input mode
                    (binary-custom-port-flush who p write!))]
                 [clear-output
                  (lambda (who p)
                    (assert-not-closed who p)
                    (binary-custom-port-clear-output who p))]
                 [close-port
                  (lambda (who p)
                    (unless (port-closed? p)
                     ; binary-custom-port-flush must be a no-op in input mode
                      (binary-custom-port-flush who p write!)
                      (binary-custom-port-close-port who p close)))]
                 [port-position
                  (and get-position
                    (lambda (who p)
                      (assert-not-closed who p)
                      (binary-custom-port-port-position in/out who p get-position)))]
                 [set-port-position!
                  (and set-position!
                    (lambda (who p x)
                      (unless (or (and (fixnum? x) (fx>= x 0)) (and (bignum? x) (>= x 0)))
                        ($oops who "~s is not a valid position" x))
                      (assert-not-closed who p)
                      (binary-custom-port-flush who p write!)
                      (set-binary-port-input-size! p 0) ;; junk the buffer data
                      (set-port-eof! p #f)
                      (set-position! x)))]
                 [port-length #f]
                 [set-port-length! #f]
                 [port-nonblocking? #f]
                 [set-port-nonblocking! #f])])
          (let ([bufsiz (custom-port-buffer-size)])
            (let ([p ($make-binary-input/output-port id handler
                       (make-bytevector bufsiz)
                       (make-bytevector bufsiz)
                       #f)])
              ($set-port-flags! p (constant port-flag-block-buffered))
              (set-binary-port-input-size! p 0)
              (set-binary-port-output-size! p (fx1- bufsiz)) ;; leave room for put to work
              p))))))

  ;; make-custom-textual-input/output-port
  (let ()
    (define-syntax make-ready-for-input
      (syntax-rules ()
        [(_ who p_ write!)
         (let ([p p_])
           (unless (eq? 0 (textual-port-output-size p))
             (textual-custom-port-flush who p write!)
             ;; don't set input-size; it is set only after a read
             (set-textual-port-output-size! p 0)))]))

    (module ((make-ready-for-output $make-ready-for-output))
      (define $make-ready-for-output
        (lambda (who p get-position set-position!)
          (unless (eq? (textual-port-input-size p) 0)
            (unless (port-input-empty? p)
              (position-warning who "cannot set position for write after read on ~s" p))
            (set-textual-port-input-size! p 0))
          (set-port-eof! p #f)
          (set-textual-port-output-size! p
            (fx1- (string-length (textual-port-output-buffer p))))))

      (define-syntax make-ready-for-output
        (syntax-rules ()
          [(_ ?who ?p ?get-position ?set-position!)
           (let ([p ?p])
             (when (eq? (textual-port-output-size p) 0)
               ($make-ready-for-output ?who p ?get-position ?set-position!)))])))

    ;; Ports start with a non-ill-defined position.
    ;; Unless get-position and set-position! are provided,
    ;; doing a buffered read operation makes the position ill-defined.
    ;;
    ;; A put, put-some or (textual)port-position operation may give
    ;; unexpected results when the position is ill-defined.
    ;;
    ;; A set-port-position is sufficient to make
    ;; the position no longer ill-defined.
    ;;
    ;; Buffered read operations include lookahead, port-eof?, and unget.
    ;; Buffered read operations also include get and get-some if buffer-mode is not none.

    (set-who! make-custom-textual-input/output-port
      (lambda (id read! write! get-position set-position! close)
        (unless (string? id) ($oops who "~s is not a string" id))
        (unless (procedure? read!) ($oops who "~s is not a procedure" read!))
        (unless (procedure? write!) ($oops who "~s is not a procedure" write!))
        (unless (or (not get-position) (procedure? get-position))
          ($oops who "~s is not a procedure or #f" get-position))
        (unless (or (not set-position!) (procedure? set-position!))
          ($oops who "~s is not a procedure or #f" set-position!))
        (unless (or (not close) (procedure? close))
          ($oops who "~s is not a procedure or #f" close))
        (let ([handler
               (make-port-handler
                 [ready?
                  (lambda (who p)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (or (not (port-input-empty? p))
                        (port-flag-eof-set? p)
                        (read-oops who p "cannot determine ready status")))]
                 [lookahead
                  (lambda (who p)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (textual-custom-port-lookahead who p write!))]
                 [unget
                  (lambda (who p x)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (textual-custom-port-unget who p x))]
                 [get
                  (lambda (who p)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (textual-custom-port-get who p read!))]
                 [get-some
                  (lambda (who p str start count)
                    (assert-not-closed who p)
                    (make-ready-for-input who p write!)
                    (textual-custom-port-get-some who p read! str start count))]
                 [clear-input
                  (lambda (who p)
                    (assert-not-closed who p)
                    (textual-custom-port-clear-input who p))]
                 [put
                  (lambda (who p x)
                    (assert-not-closed who p)
                    (make-ready-for-output who p get-position set-position!)
                    (textual-custom-port-put who p write! x))]
                 [put-some
                  (lambda (who p str start count)
                    (assert-not-closed who p)
                    (make-ready-for-output who p get-position set-position!)
                    (textual-custom-port-put-some who p write! str start count))]
                 [flush
                  (lambda (who p)
                    (assert-not-closed who p)
                   ; textual-custom-port-flush must be a no-op in input mode
                    (textual-custom-port-flush who p write!))]
                 [clear-output
                  (lambda (who p)
                    (assert-not-closed who p)
                    (textual-custom-port-clear-output who p))]
                 [close-port
                  (lambda (who p)
                    (unless (port-closed? p)
                     ; textual-custom-port-flush must be a no-op in input mode
                      (textual-custom-port-flush who p write!)
                      (textual-custom-port-close-port who p close)))]
                 [port-position
                  (and get-position
                    (lambda (who p)
                      (assert-not-closed who p)
                      (unless (port-input-empty? p)
                        (position-warning who
                          "cannot determine accurate position after read on ~s"
                          p))
                      (textual-custom-port-flush who p write!)
                      (get-position)))]
                 [set-port-position!
                  (and set-position!
                    (lambda (who p x)
                      (assert-not-closed who p)
                      (textual-custom-port-flush who p write!)
                      (set-textual-port-input-size! p 0) ;; junk the buffer data
                      (set-port-eof! p #f)
                      (set-position! x)))]
                 [port-length #f]
                 [set-port-length! #f]
                 [port-nonblocking? #f]
                 [set-port-nonblocking! #f])])
          (let ([bufsiz (custom-port-buffer-size)])
            (let ([p ($make-textual-input/output-port id handler
                       (make-string bufsiz)
                       (make-string bufsiz)
                       #f)])
              ($set-port-flags! p (constant port-flag-block-buffered))
              ($set-port-flags! p (constant port-flag-bol))
              (set-textual-port-input-size! p 0)
              (set-textual-port-output-size! p (fx1- bufsiz)) ;; leave room for put to work
              p))))))

;;;; 8.3 Simple I/O: (rnrs io simple (6))
  (let ()
    ;; eof-object in 8.2
    ;; eof-object? in 8.2

    ;; call-with-input-file in 8.2 (to share helpers)
    ;; call-with-output-file in 8.2 (to share helpers)

    ;; input-port? in 8.2
    ;; output-port? in 8.2
    ;; current-input-port in 8.2
    ;; current-output-port in 8.2
    ;; current-error-port in 8.2

    ;; with-input-from-file in 8.2 (to share helpers)
    ;; with-output-to-file in 8.2 (to share helpers)

    ;; open-input-file in 8.2 (to share helpers)
    ;; open-output-file in 8.2 (to share helpers)

    (set-who! close-input-port
      (lambda (input-port)
        (unless (input-port? input-port)
          ($oops who "~s is not an input port" input-port))
        (close-port input-port)))

    (set-who! close-output-port
      (lambda (output-port)
        (unless (output-port? output-port)
          ($oops who "~s is not an output port" output-port))
        (close-port output-port)))
    )

  (let ()
    (define ($block-read who p s count)
      (if (fx= count 0)
          (if (port-eof? p) (eof-object) 0)
          (call-port-handler get-some who p s 0 count)))
    (set-who! block-read
      (case-lambda
        [(p s)
         (unless (and (input-port? p) (textual-port? p))
           ($oops who "~s is not a textual input port" p))
         (unless (string? s)
           ($oops who "invalid buffer argument ~s" s))
         ($block-read who p s (string-length s))]
        [(p s n)
         (unless (and (input-port? p) (textual-port? p))
           ($oops who "~s is not a textual input port" p))
         (unless (string? s) ($oops who "invalid buffer argument ~s" s))
         (unless (and (fixnum? n) (fx<= 0 n (string-length s)))
           ($oops who "invalid count argument ~s" n))
         ($block-read who p s n)])))

  (let ()
    (define ($block-write who p s count)
      (let loop ([i 0] [count count])
        (unless (fx= count 0)
          (let ([n (call-port-handler put-some who p s i count)])
            (loop (fx+ i n) (fx- count n)))))
      (call-port-handler flush who p))
    (set-who! block-write
      (case-lambda
        [(p s)
         (unless (and (output-port? p) (textual-port? p))
           ($oops who "~s is not a textual output port" p))
         (unless (string? s) ($oops who "invalid buffer argument ~s" s))
         ($block-write who p s (string-length s))]
        [(p s n)
         (unless (and (output-port? p) (textual-port? p))
           ($oops who "~s is not a textual output port" p))
         (unless (string? s) ($oops who "invalid buffer argument ~s" s))
         (unless (and (fixnum? n) (fx<= 0 n (string-length s)))
           ($oops who "invalid count argument ~s" n))
         ($block-write who p s n)])))

  (let ()
    (define ($char-ready? input-port who)
      (or (not (port-input-empty? input-port))
          (port-flag-eof-set? input-port)
          (call-port-handler ready? who input-port)))
    (set-who! char-ready?
      (case-lambda
        [() ($char-ready? (current-input-port) who)]
        [(input-port)
         (unless (and (input-port? input-port) (textual-port? input-port))
           ($oops who "~s is not a textual input port" input-port))
         ($char-ready? input-port who)])))

  (set-who! clear-input-port
    (rec clear-input-port
      (case-lambda
        [() (let ([p (current-input-port)])
              (call-port-handler clear-input who p))]
        [(p)
         (unless (input-port? p)
           ($oops who "~s is not an input port" p))
         (call-port-handler clear-input who p)])))

  (set-who! clear-output-port
    (rec clear-output-port
      (case-lambda
        [() (let ([p (current-output-port)])
              (call-port-handler clear-output who p))]
        [(p)
         (unless (output-port? p)
           ($oops who "~s is not an output port" p))
         (call-port-handler clear-output who p)])))

  (set-who! fresh-line
    (rec fresh-line
      (case-lambda
        [() (fresh-line (current-output-port))]
        [(p)
         (unless (and (output-port? p) (textual-port? p))
           ($oops who "~s is not a textual output port" p))
         (assert-not-closed who p)
         (unless ($textual-port-bol? p)
           (call-port-handler put who p #\newline))])))

  (set-who! port-bol?
    (lambda (p)
      (unless (and (output-port? p) (textual-port? p))
        ($oops who "~s is not a textual output port" p))
      (assert-not-closed who p)
      ($textual-port-bol? p)))

  (let ()
    (define (binary-fd-port? bp)
      ($port-flags-set? bp (constant port-flag-file)))

    (set-who! file-port?
      (lambda (p)
        (unless (port? p) ($oops who "~s is not a port" p))
        (if (binary-port? p)
            (binary-fd-port? p)
            (let ([info ($port-info p)])
              (and (codec-info? info) (binary-fd-port? (codec-info-bp info)))))))

    (set-who! port-file-descriptor
      (let ()
        (define gzfile-fd (foreign-procedure "(cs)gzxfile_fd" (ptr) int))
        (define (binary-port-fd p bp)
          (unless (binary-fd-port? bp)
            ($oops who "~s is not a file port" p))
          (let ([x ($port-info bp)])
            (if (port-gz-mode bp)
                (gzfile-fd x)
                x)))
        (lambda (p)
          (unless (port? p) ($oops who "~s is not a port" p))
          (if (binary-port? p)
              (binary-port-fd p p)
              (let ([info ($port-info p)])
                (unless (codec-info? info)
                  ($oops who "~s is not a file port" p))
                (binary-port-fd p (codec-info-bp info))))))))

  (let ()
    (define $generic-port-handler
      (make-port-handler
        [ready?
         (lambda (who p)
           (assert-not-closed who p)
           (and (($port-info p) 'char-ready? p) #t))]
        [lookahead
         (lambda (who p)
           (assert-not-closed who p)
           (let ([c (($port-info p) 'peek-char p)])
             (unless (or (char? c) (eof-object? c))
               ($oops 'generic-port-handler "invalid peek-char return value ~s" c))
             c))]
        [unget
         (lambda (who p x)
           (assert-not-closed who p)
           (unless (eof-object? x) (($port-info p) 'unread-char x p))
           (void))]
        [get
         (lambda (who p)
           (assert-not-closed who p)
           (let ([c (($port-info p) 'read-char p)])
             (unless (or (char? c) (eof-object? c))
               ($oops 'generic-port-handler "invalid read-char return value ~s" c))
             c))]
        [get-some
         (lambda (who p st start count)
           (if (= start 0)
               (let ([n (($port-info p) 'block-read p st count)])
                 (unless (or (and (fixnum? n) (not ($fxu< count n)))
                             (eof-object? n))
                   ($oops 'generic-port-handler "invalid block-read return value ~s on ~s" n p))
                 n)
               (let ([tmp (make-string count)])
                 (let ([n (($port-info p) 'block-read p tmp count)])
                   (cond
                     [(and (fixnum? n) (not ($fxu< count n)))
                      (string-copy! tmp 0 st start n)
                      n]
                     [(eof-object? n) n]
                     [else ($oops 'generic-port-handler "invalid block-read return value ~s on ~s" n p)])))))]
        [clear-input
         (lambda (who p)
           (assert-not-closed who p)
           (($port-info p) 'clear-input-port p)
           (void))]
        [put
         (lambda (who p x)
           (assert-not-closed who p)
           (($port-info p) 'write-char x p)
           (void))]
        [put-some
         (lambda (who p st start count)
           (assert-not-closed who p)
           (if (= start 0)
               (($port-info p) 'block-write p st count)
               (let ([tmp (make-string count)])
                 (string-copy! st start tmp 0 count)
                 (($port-info p) 'block-write p tmp count)))
           count)]
        [flush
         (lambda (who p)
           (assert-not-closed who p)
           (($port-info p) 'flush-output-port p)
           (void))]
        [clear-output
         (lambda (who p)
           (assert-not-closed who p)
           (($port-info p) 'clear-output-port p)
           (void))]
        [close-port
         (lambda (who p)
           (unless (port-closed? p)
             (($port-info p) 'close-port p))
           (void))]
        [port-position
         (lambda (who p)
           (assert-not-closed who p)
           (($port-info p) 'file-position p))]
        [set-port-position!
         (lambda (who p x)
           (assert-not-closed who p)
           (($port-info p) 'file-position p x))]
        [port-length
         (lambda (who p)
           (assert-not-closed who p)
           (($port-info p) 'file-length p))]
        [set-port-length!
         (lambda (who p pos)
           (assert-not-closed who p)
           (($port-info p) 'truncate-file p pos))]
        [port-nonblocking? #f]
        [set-port-nonblocking! #f]))

    (define (set-name p)
      (guard (c [#t (void)])
        (let ([name (($port-info p) 'port-name p)])
          (when (string? name) (set-port-name! p name))))
      p)

    (set-who! make-input-port
      (lambda (handler buffer)
        (unless (procedure? handler)
          (if (and (fixnum? handler) (fx>= handler 0))
              ($oops who "fixnum handler no longer supported; use open-fd-input-port")
              ($oops who "~s is not a procedure" handler)))
        (unless (string? buffer) ($oops who "~s is not a string" buffer))
        (set-name
          ($make-textual-input-port "generic"
            $generic-port-handler
            buffer handler))))

    (set-who! make-output-port
      (lambda (handler buffer)
        (unless (procedure? handler)
          (if (and (fixnum? handler) (fx>= handler 0))
              ($oops who "fixnum handler no longer supported; use open-fd-input-port")
              ($oops who "~s is not a procedure" handler)))
        (unless (string? buffer) ($oops who "~s is not a string" buffer))
        (set-name
          ($make-textual-output-port "generic"
            $generic-port-handler
            buffer handler))))

    (set-who! make-input/output-port
      (lambda (handler ibuffer obuffer)
        (unless (procedure? handler)
          (if (and (fixnum? handler) (fx>= handler 0))
              ($oops who "fixnum handler no longer supported; use open-fd-input-port")
              ($oops who "~s is not a procedure" handler)))
        (unless (string? ibuffer) ($oops who "~s is not a string" ibuffer))
        (unless (string? obuffer) ($oops who "~s is not a string" obuffer))
        (set-name
          ($make-textual-input/output-port "generic"
            $generic-port-handler
            ibuffer obuffer handler))))

    (set-who! port-handler
      (let ()
        (define check
          (lambda (msg n)
            (unless (cond
                      [(assq n
                         '((1 char-ready? clear-input-port clear-output-port close-port
                              file-length file-position flush-output-port peek-char
                              port-name read-char)
                            (2 file-position unread-char write-char)
                            (3 block-read block-write))) =>
                       (lambda (ls) (memq msg (cdr ls)))]
                      [else #f])
              ($oops 'non-generic-port-handler
                "cannot handle message ~s with argument count ~s"
                msg n))))
        (define non-generic-port-handler
          (lambda (msg . args)
            (check msg (length args))
            (apply ($top-level-value msg) args)))
        (lambda (p)
          (unless (port? p) ($oops who "~s is not a port" p))
          (if (eq? ($port-handler p) $generic-port-handler)
              ($port-info p)
              non-generic-port-handler))))
  )

  (record-writer (type-descriptor codec)
    (lambda (x p wr)
      (fprintf p "#<codec ~a>" (codec-name x))))

  (record-writer (type-descriptor transcoder)
    (lambda (x p wr)
      (fprintf p "#<transcoder ~a ~s ~s>"
        (codec-name ($transcoder-codec x))
        ($transcoder-eol-style x)
        ($transcoder-error-handling-mode x))))

  (set-who! #(r6rs: current-input-port)
    (lambda ()
      (#2%current-input-port)))

  (set-who! #(r6rs: current-output-port)
    (lambda ()
      (#2%current-output-port)))

  (set-who! #(r6rs: current-error-port)
    (lambda ()
      (#2%current-error-port)))

 ; thread-safe transcript-on, transcript-off, transcript-cafe
  (let ()
    (define-record-type xscript-info
      (nongenerative)
      (opaque #t)
      (sealed #t)
      (fields ip op xp (mutable ungot))
      (protocol
        (lambda (new)
          (lambda (ip op xp)
            (new ip op xp '())))))

    (module (make-xscript-port xscript-port? constituent-ports)
      (define-syntax with-xscript-info
        (syntax-rules ()
          [(_ (p ip op xp ungot) e1 e2 ...)
           (andmap identifier? #'(ip op xp ungot))
           (let ([x ($port-info p)])
             (let ([ip (xscript-info-ip x)]
                   [op (xscript-info-op x)]
                   [xp (xscript-info-xp x)])
               (define-syntax ungot
                 (identifier-syntax
                   [id (xscript-info-ungot x)]
                   [(set! id e) (xscript-info-ungot-set! x e)]))
               e1 e2 ...))]))

      (define-syntax thread-safe
        (syntax-rules ()
          [(_ (p ip op xp ungot) e1 e2 ...)
           (with-xscript-info (p ip op xp ungot)
             (with-tc-mutex e1 e2 ...))]))

      (define-syntax call-xp-handler
        (syntax-rules ()
          [(_ msg who xp arg ...)
           (identifier? #'xp)
           (and (not (port-closed? xp))
                (call-port-handler msg who xp arg ...))]))

      (define slurp-input
        (lambda (who p)
          (with-xscript-info (p ip op xp ungot)
            (let ([tognu (reverse ungot)])
              (guard (c [#t (void)]) ; guard ready? calls
                (let loop ()
                  (when (call-port-handler ready? who ip)
                    (let ([c (call-port-handler get who ip)])
                      (unless (eof-object? c)
                        (call-xp-handler put who xp c)
                        (set! tognu (cons c tognu))
                        (loop))))))
              (set! ungot (reverse tognu))))))

     ; similar in structure to thread-safe console-port handler
      (define xscript-handler
        (make-port-handler
          [ready?
           (lambda (who p)
             (thread-safe (p ip op xp ungot)
               (or (not (null? ungot))
                   (begin
                     (call-port-handler flush who op)
                     (call-port-handler ready? who ip)))))]
          [lookahead
           (lambda (who p)
             (thread-safe (p ip op xp ungot)
               (if (not (null? ungot))
                   (car ungot)
                   (begin
                     (call-port-handler flush who op)
                     (let ([c (call-port-handler get who ip)])
                       (set! ungot (list c))
                       (unless (eof-object? c) (call-xp-handler put who xp c))
                       c)))))]
          [unget
           (lambda (who p x)
             (thread-safe (p ip op xp ungot)
               (set! ungot (cons x ungot))))]
          [get
           (lambda (who p)
             (thread-safe (p ip op xp ungot)
               (if (not (null? ungot))
                   (let ([c (car ungot)])
                     (set! ungot (cdr ungot))
                     c)
                   (begin
                     (call-port-handler flush who op)
                     (let ([c (call-port-handler get who ip)])
                       (unless (eof-object? c) (call-xp-handler put who xp c))
                       c)))))]
          [get-some
           (lambda (who p str start count)
             (thread-safe (p ip op xp ungot)
               (if (and (fx> count 0) (not (null? ungot)))
                   (let ([c (car ungot)])
                     (set! ungot (cdr ungot))
                     (if (eof-object? c)
                         c
                         (begin (string-set! str start c) 1)))
                   (begin
                     (call-port-handler flush who op)
                     (let ([count (call-port-handler get-some who ip str start count)])
                       (unless (or (eof-object? count) (fx= count 0))
                         (call-xp-handler put-some who xp str start count))
                       count)))))]
          [clear-input
           (lambda (who p)
             (thread-safe (p ip op xp ungot)
               (set! ungot '())
               (call-port-handler clear-input who ip)))]
          [put
           (lambda (who p x)
             (thread-safe (p ip op xp ungot)
               (slurp-input who p)
               (call-port-handler put who op x)
               (call-xp-handler put who xp x)
               (if ($textual-port-bol? op)
                   ($set-port-flags! p (constant port-flag-bol))
                   ($reset-port-flags! p (constant port-flag-bol)))))]
          [put-some
           (lambda (who p str start count)
             (thread-safe (p ip op xp ungot)
               (slurp-input who p)
               (let ([count (call-port-handler put-some who op str start count)])
                 (let f ([start start] [count count])
                   (unless (fx= count 0)
                     (let ([n (call-xp-handler put-some who xp str start count)])
                       (and n (f (fx+ start n) (fx- count n))))))
                 (if ($textual-port-bol? op)
                     ($set-port-flags! p (constant port-flag-bol))
                     ($reset-port-flags! p (constant port-flag-bol)))
                 count)))]
          [flush
           (lambda (who p)
             (thread-safe (p ip op xp ungot)
               (call-port-handler flush who op)
               (call-xp-handler flush who xp)))]
          [clear-output
           (lambda (who p)
            ; clearing may put op and xp out of sync, so just flush instead
             (thread-safe (p ip op xp ungot)
               (call-port-handler flush who op)
               (call-xp-handler flush who xp)))]
          [close-port
           (lambda (who p)
            ; refuse to close transcript ports, like console ports---just flush instead
             (thread-safe (p ip op xp ungot)
               (call-port-handler flush who op)
               (call-xp-handler flush who xp)))]
          [port-position #f]
          [set-port-position! #f]
          [port-length #f]
          [set-port-length! #f]
          [port-nonblocking? #f]
          [set-port-nonblocking! #f]))

      (define (make-xscript-port ip op xp)
        (let ([p ($make-textual-input/output-port
                   "transcript" xscript-handler "" ""
                   (make-xscript-info ip op xp))])
          (when ($port-flags-set? ip (constant port-flag-r6rs))
            ($set-port-flags! p (constant port-flag-r6rs)))
          (when ($port-flags-set? ip (constant port-flag-fold-case))
            ($set-port-flags! p (constant port-flag-fold-case)))
          (when ($port-flags-set? ip (constant port-flag-no-fold-case))
            ($set-port-flags! p (constant port-flag-no-fold-case)))
          (when ($textual-port-bol? op)
            ($set-port-flags! p (constant port-flag-bol)))
          p))

      (define xscript-port?
        (lambda (p)
          (eq? ($port-handler p) xscript-handler)))

      (define constituent-ports
        (lambda (p)
          (with-xscript-info (p ip op xp ungot)
            (values ip op xp)))))

    (set-who! $xscript-port? (lambda (p) (xscript-port? p)))

    (set-who! $constituent-ports (lambda (p) (constituent-ports p)))

    (set-who! transcript-on
      (lambda (pathname)
        (unless (string? pathname) ($oops who "~s is not a string" pathname))
        (let ([ip (console-input-port)] [op (console-output-port)])
          (when (and (guard (c [#t #f]) (char-ready? ip))
                     (eqv? (peek-char ip) #\newline))
            (read-char ip))
          (let ([xp ($open-file-output-port who pathname (file-options replace)
                      (buffer-mode block)
                      (current-transcoder))])
            (let ([p (make-xscript-port ip op xp)])
              (when (eq? (console-error-port) op) (console-error-port p))
              (when (eq? (current-input-port) ip) (current-input-port p))
              (when (eq? (current-output-port) op) (current-output-port p))
              (when (eq? (current-error-port) op) (current-error-port p))
              (when (eq? (trace-output-port) op) (trace-output-port p))
              (console-input-port p)
              (console-output-port p)))
          (printf "Chez Scheme Transcript [~a]\n" (date-and-time)))))

    (set-who! transcript-off
      (lambda ()
        (cond
          [(ormap (lambda (p) (and (xscript-port? p) p))
             (list (console-input-port)
                   (console-output-port)
                   (console-error-port)
                   (current-input-port)
                   (current-output-port)
                   (current-error-port)
                   (trace-output-port))) =>
           (lambda (p)
             (let-values ([(ip op xp) (constituent-ports p)])
               (when (eq? (console-input-port) p) (console-input-port ip))
               (when (eq? (console-output-port) p) (console-output-port op))
               (when (eq? (console-error-port) p) (console-error-port op))
               (when (eq? (current-input-port) p) (current-input-port ip))
               (when (eq? (current-output-port) p) (current-output-port op))
               (when (eq? (current-error-port) p) (current-error-port op))
               (when (eq? (trace-output-port) p) (trace-output-port op))
               (flush-output-port p)
               (close-port xp)))])))

    (set-who! transcript-cafe
      (lambda (pathname)
        (unless (string? pathname) ($oops who "~s is not a string" pathname))
        (let ([ip (console-input-port)] [op (console-output-port)])
          (when (and (guard (c [#t #f]) (char-ready? (console-input-port)))
                     (eqv? (peek-char (console-input-port)) #\newline))
            (read-char (console-input-port)))
          (let ([xp ($open-file-output-port who pathname (file-options replace)
                      (buffer-mode block)
                      (current-transcoder))])
            (let ([p (make-xscript-port ip op xp)])
              (with-values
                (dynamic-wind
                  (lambda ()
                    (when (eq? (console-input-port) ip) (console-input-port p))
                    (when (eq? (console-output-port) op) (console-output-port p))
                    (when (eq? (console-error-port) op) (console-error-port p))
                    (when (eq? (current-input-port) ip) (current-input-port p))
                    (when (eq? (current-output-port) op) (current-output-port p))
                    (when (eq? (current-error-port) op) (current-error-port p))
                    (when (eq? (trace-output-port) op) (trace-output-port p)))
                  (lambda ()
                    (printf "Chez Scheme Transcript [~a]\n" (date-and-time))
                    (new-cafe))
                  (lambda ()
                    (when (eq? (console-input-port) p) (console-input-port ip))
                    (when (eq? (console-output-port) p) (console-output-port op))
                    (when (eq? (console-error-port) p) (console-error-port op))
                    (when (eq? (current-input-port) p) (current-input-port ip))
                    (when (eq? (current-output-port) p) (current-output-port op))
                    (when (eq? (current-error-port) p) (current-error-port op))
                    (when (eq? (trace-output-port) p) (trace-output-port op))
                    (flush-output-port p)))
                (lambda vals
                  (close-port xp)
                  (apply values vals)))))))))

  #;(let ()
    (define debug-port-handler
      (make-port-handler
        [ready? (lambda (who p) (input-port-ready? ($port-info p)))]
        [lookahead
         (lambda (who p)
           (let ([b (lookahead-u8 ($port-info p))])
             (if (eof-object? b) b (integer->char b))))]
        [unget
         (lambda (who p x)
           (unget-u8 ($port-info p) (if (eof-object? x) x (char->integer x))))]
        [get
         (lambda (who p)
           (let ([b (get-u8 ($port-info p))])
             (if (eof-object? b) b (integer->char b))))]
        [get-some
         (lambda (who p str start count)
           (if (fx= count 0)
               0
               (let ([b (get-u8 ($port-info p))])
                 (if (eof-object? b)
                     b
                     (begin
                       (string-set! str start (integer->char b))
                       1)))))]
        [clear-input
         (lambda (who p)
           (clear-input-port ($port-info p)))]
        [put
         (lambda (who p x)
           (put-u8 ($port-info p) (char->integer x)))]
        [put-some
         (lambda (who p str start count)
           (if (fx= count 0)
               0
               (begin
                 (put-u8 ($port-info p) (char->integer (string-ref str start)))
                 1)))]
        [flush
         (lambda (who p)
           (flush-output-port ($port-info p)))]
        [clear-output
         (lambda (who p)
           (clear-output-port ($port-info p)))]
        [close-port (lambda (who p) (flush-output-port ($port-info p)) (void))]
        [port-position
         (lambda (who p)
           (port-position ($port-info p)))]
        [set-port-position!
         (lambda (who p x)
           (set-port-position! ($port-info p) x))]
        [port-length
         (lambda (who p)
           (port-length ($port-info p)))]
        [set-port-length!
         (lambda (who p x)
           (set-port-length! ($port-info p) x))]
        [port-nonblocking? #f]
        [set-port-nonblocking! #f]))
    (set! $console-input-port ($make-textual-input-port "debug-stdin" debug-port-handler "" (standard-input-port (buffer-mode block))))
    (set! $console-output-port ($make-textual-output-port "debug-stdout" debug-port-handler "" (standard-output-port (buffer-mode none))))
    (set! $console-output-port ($make-textual-output-port "debug-stderr" debug-port-handler "" (standard-error-port (buffer-mode none)))))

  (let ([ip (standard-input-port (buffer-mode block) (current-transcoder))]
        [op (standard-output-port (buffer-mode line) (current-transcoder))])
    (define same-device? (foreign-procedure "(cs)same_devicep" (int int) boolean))
    (if-feature pthreads
      (let ([$mutex-is-owner (foreign-procedure "(cs)mutex_is_owner" (scheme-object) boolean)])
        (define (make-thread-safe-handler ip op)
          (define port-mutex (make-mutex))
          (define-syntax (with-port-mutex stx)
            (syntax-case stx ()
              [(_ body ...)
               ;; internally, the system might use the standard ports while holding tc mutex,
               ;; in which case it's already atomic enough; for example, a port can be flushed
               ;; and closed on exit; to accomocate those cases, check whether the tc mutex is held
               ;; before grabbing the port mutex, so the lock order (port mutex before tc mutex)
               ;; is preserved
               #'(let ([f (lambda () body ...)])
                   (if ($mutex-is-owner ($raw-tc-mutex))
                       (f)
                       (with-mutex port-mutex (f))))]))
          (make-port-handler
            [ready?
             (and ip
               (lambda (who p)
                 (with-port-mutex
                   (call-port-handler flush who op)
                   (call-port-handler ready? who ip))))]
            [lookahead
             (and ip
               (lambda (who p)
                 (with-port-mutex
                   (call-port-handler flush who op)
                   (call-port-handler lookahead who ip))))]
            [unget
             (and ip
               (lambda (who p x)
                 (with-port-mutex
                   (call-port-handler unget who ip x))))]
            [get
             (and ip
               (lambda (who p)
                 (with-port-mutex
                   (call-port-handler flush who op)
                   (call-port-handler get who ip))))]
            [get-some
             (and ip
               (lambda (who p str start count)
                 (with-port-mutex
                   (call-port-handler flush who op)
                   (call-port-handler get-some who ip str start count))))]
            [clear-input
             (and ip
               (lambda (who p)
                 (with-port-mutex
                   (call-port-handler clear-input who ip))))]
            [put
             (and op
               (lambda (who p x)
                 (with-port-mutex
                   (call-port-handler put who op x)
                   (if ($textual-port-bol? op)
                       ($set-port-flags! p (constant port-flag-bol))
                       ($reset-port-flags! p (constant port-flag-bol))))))]
            [put-some
             (and op
               (lambda (who p str start count)
                 (with-port-mutex
                   (let ([count (call-port-handler put-some who op str start count)])
                     (if ($textual-port-bol? op)
                         ($set-port-flags! p (constant port-flag-bol))
                         ($reset-port-flags! p (constant port-flag-bol)))
                     count))))]
            [flush
             (and op
               (lambda (who p)
                 (with-port-mutex
                   (call-port-handler flush who op))))]
            [clear-output
             (and op
               (lambda (who p)
                 (with-port-mutex
                   (call-port-handler clear-output who op))))]
            [close-port ; refuse to close console ports---just flush instead
             (if op
                 (lambda (who p)
                   (with-port-mutex
                     (call-port-handler flush who op)))
                 (lambda (who p)
                   (void)))]
            [port-position #f]
            [set-port-position! #f]
            [port-length #f]
            [set-port-length! #f]
            [port-nonblocking? #f]
            [set-port-nonblocking! #f]))
        (define thread-safe-console-input/output-port
          (lambda (name ip op)
            (let ([p ($make-textual-input/output-port name (make-thread-safe-handler ip op) "" "" #f)])
              (when ($port-flags-set? ip (constant port-flag-r6rs))
                ($set-port-flags! p (constant port-flag-r6rs)))
              (when ($port-flags-set? ip (constant port-flag-fold-case))
                ($set-port-flags! p (constant port-flag-fold-case)))
              (when ($port-flags-set? ip (constant port-flag-no-fold-case))
                ($set-port-flags! p (constant port-flag-no-fold-case)))
              (when ($textual-port-bol? op)
                ($set-port-flags! p (constant port-flag-bol)))
              p)))
        (define thread-safe-console-output-port
          (lambda (name op)
            (let ([p ($make-textual-output-port name (make-thread-safe-handler #f op) "" #f)])
              (when ($textual-port-bol? op)
                ($set-port-flags! p (constant port-flag-bol)))
              p)))
        (let ([p (thread-safe-console-input/output-port "stdin/out" ip op)])
          (set! $console-input-port p)
          (set! $console-output-port p)
          (set! $console-error-port
            (if (same-device? 1 2)
                p
                (thread-safe-console-output-port "stderr" (standard-error-port (buffer-mode line) (current-transcoder)))))))
      (begin
        (set! $console-input-port ip)
        (set! $console-output-port op)
        (set! $console-error-port
          (if (same-device? 1 2)
              op
              (standard-error-port (buffer-mode line) (current-transcoder)))))))

  (current-input-port $console-input-port)
  (current-output-port $console-output-port)
  (current-error-port $console-error-port)
  (set-who! console-input-port
    (make-parameter
      $console-input-port
      (lambda (ip)
        (unless (and (input-port? ip) (textual-port? ip))
           ($oops who "~s is not a textual input port" ip))
        ip)))
  (set-who! console-output-port
    (make-parameter
      $console-output-port
      (lambda (op)
        (unless (and (output-port? op) (textual-port? op))
           ($oops who "~s is not a textual output port" op))
        op)))
  (set-who! console-error-port
    (make-parameter
      $console-error-port
      (lambda (op)
        (unless (and (output-port? op) (textual-port? op))
           ($oops who "~s is not a textual output port" op))
        op)))
  (set! $io-init
    (lambda ()
      (clear-open-files)
     ; reregister the console ports
      (register-open-file $console-input-port)
      (register-open-file $console-output-port)
      (unless (eq? $console-error-port $console-output-port)
        (register-open-file $console-error-port))))

  (set! $separator-character
    (constant-case architecture
      [(pb) (foreign-procedure "(cs)s_separatorchar" () ptr)]
      [else (if-feature windows (lambda () #\;) (lambda () #\:))]))

  ; utf8->string, etc., are in prims.ss, since they are used by
  ; foreign procedures argument and return values
)
)
