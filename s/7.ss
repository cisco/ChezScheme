;;; 7.ss
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

;;; system operations

(begin
(define scheme-start
  (make-parameter
    (lambda fns (for-each load fns) (new-cafe))
    (lambda (p)
      (unless (procedure? p)
        ($oops 'scheme-start "~s is not a procedure" p))
      p)))

(define scheme-script
  (make-parameter
    (lambda (fn . fns)
      (command-line (cons fn fns))
      (command-line-arguments fns)
      (load fn))
    (lambda (p)
      (unless (procedure? p)
        ($oops 'scheme-script "~s is not a procedure" p))
      p)))

(define scheme-program
  (make-parameter
    (lambda (fn . fns)
      (command-line (cons fn fns))
      (command-line-arguments fns)
      (load-program fn))
    (lambda (p)
      (unless (procedure? p)
        ($oops 'scheme-program "~s is not a procedure" p))
      p)))

(define command-line-arguments
  (make-parameter
    '()
    (lambda (x)
      (unless (and (list? x) (andmap string? x))
        ($oops 'command-line-arguments "~s is not a list of strings" x))
      x)))

(define command-line
  (make-parameter
    '("")
    (lambda (x)
      (unless (and (list? x) (not (null? x)) (andmap string? x))
        ($oops 'command-line "~s is not a nonempty list of strings" x))
      x)))

(define-who #(r6rs: command-line)
  (lambda ()
    (#2%command-line)))

(define-who bytes-allocated
  (let ([ba (foreign-procedure "(cs)bytes_allocated"
              (scheme-object scheme-object)
              scheme-object)])
    (define filter-generation
      (lambda (g)
        (cond
          [(and (fixnum? g) (fx<= 0 g (collect-maximum-generation))) g]
          [(eq? g 'static) (constant static-generation)]
          [else ($oops who "invalid generation ~s" g)])))
    (define filter-space
      (lambda (s)
        (cond
          [(assq s (constant real-space-alist)) => cdr]
          [else ($oops who "invalid space ~s" s)])))
    (case-lambda
      [() (ba -1 -1)]
      [(g) (ba (filter-generation g) -1)]
      [(g s) (ba (if g (filter-generation g) -1) (if s (filter-space s) -1))])))

(define $spaces (lambda () (map car (constant real-space-alist))))

(define current-memory-bytes (foreign-procedure "(cs)curmembytes" () uptr))
(define maximum-memory-bytes (foreign-procedure "(cs)maxmembytes" () uptr))

(define reset-maximum-memory-bytes! (foreign-procedure "(cs)resetmaxmembytes" () void))

(define-who with-source-path
  (lambda (whoarg fn p)
    (unless (or (eq? whoarg #f) (string? whoarg) (symbol? whoarg)) ($oops who "invalid who argument ~s" whoarg))
    (unless (string? fn) ($oops who "~s is not a string" fn))
    (unless (procedure? p) ($oops who "~s is not a procedure" p))
    (let ([dirs (source-directories)])
      (if (or (equal? dirs '("")) (equal? dirs '(".")) ($fixed-path? fn))
          (p fn)
          (let loop ([ls dirs])
            (if (null? ls)
                ($oops whoarg "file ~s not found in source directories" fn)
                (let ([path (let ([dir (car ls)])
                              (if (or (string=? dir "") (string=? dir "."))
                                  fn
                                  (format
                                    (if (directory-separator?
                                          (string-ref dir
                                            (fx- (string-length dir) 1)))
                                        "~a~a"
                                        "~a/~a")
                                    dir fn)))])
                  (if (guard (c [#t #f]) (close-input-port (open-input-file path)) #t)
                      (p path)
                      (loop (cdr ls))))))))))

(set! $compressed-warning
  (let ([warned? #f])
    (lambda (who p)
      (unless warned?
        (set! warned? #t)
        (warningf who "fasl file content is compressed internally; compressing the file (~s) is redundant and can slow fasl writing and reading significantly" p)))))

(set-who! fasl-read
  (let ()
    (define $fasl-read (foreign-procedure "(cs)fasl_read" (int fixnum ptr) ptr))
    (define $bv-fasl-read (foreign-procedure "(cs)bv_fasl_read" (ptr ptr) ptr))
    (define (get-uptr p)
      (let ([k (get-u8 p)])
        (let f ([k k] [n (fxsrl k 1)])
          (if (fxlogbit? 0 k)
              (let ([k (get-u8 p)])
                (f k (logor (ash n 7) (fxsrl k 1))))
              n))))
    (define (get-uptr/bytes p)
      (let ([k (get-u8 p)])
        (let f ([k k] [n (fxsrl k 1)] [bytes 1])
          (if (fxlogbit? 0 k)
              (let ([k (get-u8 p)])
                (f k (logor (ash n 7) (fxsrl k 1)) (fx+ bytes 1)))
              (values n bytes)))))
    (define (malformed p what) ($oops who "malformed fasl-object found in ~s (~a)" p what))
    (define (check-header p)
      (let ([bv (make-bytevector 8 (constant fasl-type-header))])
        (unless (and (eqv? (get-bytevector-n! p bv 1 7) 7)
                     (bytevector=? bv (constant fasl-header)))
          (malformed p "invalid header")))
      (let ([n (get-uptr p)])
        (unless (= n (constant scheme-version))
          ($oops who "incompatible fasl-object version ~a found in ~s"
            ($format-scheme-version n) p)))
      (let ([n (get-uptr p)])
        (unless (or (= n (constant machine-type-any)) (= n (constant machine-type)))
          (cond
            [(assv n (constant machine-type-alist)) =>
             (lambda (a)
               ($oops who "incompatible fasl-object machine-type ~s found in ~s"
                 (cdr a) p))]
            [else (malformed p "unrecognized machine type")])))
      (unless (and (eqv? (get-u8 p) (char->integer #\()) ;)
                   (let f ()
                     (let ([n (get-u8 p)])
                       (and (not (eof-object? n)) ;(
                            (or (eqv? n (char->integer #\))) (f))))))
        (malformed p "invalid list of base boot files")))
    (define (go p situation)
      (define (go1)
        (if (and ($port-flags-set? p (constant port-flag-file))
                 (or (not ($port-flags-set? p (constant port-flag-compressed)))
                     (begin ($compressed-warning who p) #f))
                 (eqv? (binary-port-input-count p) 0))
            ($fasl-read ($port-info p) situation (port-name p))
            (let fasl-entry ()
              (let ([ty (get-u8 p)])
                (cond
                  [(eof-object? ty) ty]
                  [(eqv? ty (constant fasl-type-header))
                   (check-header p)
                   (fasl-entry)]
                  [(eqv? ty (constant fasl-type-visit))
                   (go2 (eqv? situation (constant fasl-type-revisit)))]
                  [(eqv? ty (constant fasl-type-revisit))
                   (go2 (eqv? situation (constant fasl-type-visit)))]
                  [(eqv? ty (constant fasl-type-visit-revisit))
                   (go2 #f)]
                  [else (malformed p "invalid situation")])))))
      (define (go2 skip?)
        (let ([n (get-uptr p)])
          (if skip?
              (begin
                (if (and (port-has-port-position? p) (port-has-set-port-position!? p))
                    (set-port-position! p (+ (port-position p) n))
                    (get-bytevector-n p n))
                (go1))
              (let ([compressed-flag (get-u8 p)])
                (cond
                  [(or (eqv? compressed-flag (constant fasl-type-gzip)) (eqv? compressed-flag (constant fasl-type-lz4)))
                   (let-values ([(dest-size dest-size-bytes) (get-uptr/bytes p)])
                     (let* ([src-size (- n 1 dest-size-bytes)]
                            [bv (get-bytevector-n p src-size)]
                            [bv ($bytevector-uncompress bv dest-size
                                  (if (eqv? compressed-flag (constant fasl-type-gzip))
                                      (constant COMPRESS-GZIP)
                                      (constant COMPRESS-LZ4)))])
                       ($bv-fasl-read bv (port-name p))))]
                  [(eqv? compressed-flag (constant fasl-type-uncompressed))
                   ($bv-fasl-read (get-bytevector-n p (- n 1)) (port-name p))]
                  [else (malformed p "invalid compression")])))))
      (unless (and (input-port? p) (binary-port? p))
        ($oops who "~s is not a binary input port" p))
      (go1))
    (case-lambda
      [(p) (go p (constant fasl-type-visit-revisit))]
      [(p situation)
       (go p
         (case situation
           [(visit) (constant fasl-type-visit)]
           [(revisit) (constant fasl-type-revisit)]
           [(load) (constant fasl-type-visit-revisit)]
           [else ($oops who "invalid situation ~s" situation)]))])))

(define ($compiled-file-header? ip)
  (let ([pos (port-position ip)])
    (let ([cfh? (let* ([bv (constant fasl-header)] [n (bytevector-length bv)])
                  (let f ([i 0])
                    (or (fx= i n)
                        (and (eqv? (get-u8 ip) (bytevector-u8-ref bv i))
                             (f (fx+ i 1))))))])
      (set-port-position! ip pos)
      cfh?)))

(let ()
  (define do-load-binary
    (lambda (who fn ip situation for-import? importer)
      (let ([load-binary (make-load-binary who fn situation for-import? importer)])
        (let ([x (fasl-read ip situation)])
          (unless (eof-object? x)
            (let loop ([x x])
              (let ([next-x (fasl-read ip situation)])
                (if (eof-object? next-x)
                    (load-binary x)
                    (begin (load-binary x) (loop next-x))))))))))

  (define (make-load-binary who fn situation for-import? importer)
    (module (Lexpand? recompile-info? library/ct-info? library/rt-info? program-info?)
      (import (nanopass))
      (include "base-lang.ss")
      (include "expand-lang.ss"))
    (lambda (x)
      (cond
        [(procedure? x) (x)]
        [(library/rt-info? x) ($install-library/rt-desc x for-import? importer fn)]
        [(library/ct-info? x) ($install-library/ct-desc x for-import? importer fn)]
        [(program-info? x) ($install-program-desc x)]
        [(recompile-info? x) (void)]
        [(Lexpand? x) ($interpret-backend x situation for-import? importer fn)]
        ; NB: this is here to support the #t inserted by compile-file-help2 after header information
        [(eq? x #t) (void)]
        [else ($oops who "unexpected value ~s read from ~a" x fn)])))

  (define (do-load who fn situation for-import? importer ksrc)
    (let ([ip ($open-file-input-port who fn)])
      (on-reset (close-port ip)
        (let ([fp (let ([start-pos (port-position ip)])
                    (if (and (eqv? (get-u8 ip) (char->integer #\#))
                             (eqv? (get-u8 ip) (char->integer #\!))
                             (let ([b (get-u8 ip)]) (or (eqv? b (char->integer #\space)) (eqv? b (char->integer #\/)))))
                        (let loop ([fp 3])
                          (let ([b (get-u8 ip)])
                            (if (eof-object? b)
                                fp
                                (let ([fp (+ fp 1)])
                                  (if (eqv? b (char->integer #\newline))
                                      fp
                                      (loop fp))))))
                        (begin (set-port-position! ip start-pos) 0)))])
          (if ($compiled-file-header? ip)
              (begin
                (do-load-binary who fn ip situation for-import? importer)
                (close-port ip))
              (begin
                (unless ksrc
                  (close-port ip)
                  ($oops who "~a is not a compiled file" fn))
                (unless (eqv? fp 0) (set-port-position! ip 0))
                (let ([sfd ($source-file-descriptor fn ip (eqv? fp 0))])
                  (unless (eqv? fp 0) (set-port-position! ip fp))
                  ; whack ip so on-reset close-port call above closes the text port
                  (set! ip (transcoded-port ip (current-transcoder)))
                  (ksrc ip sfd ($make-read ip sfd fp)))))))))

  (set! $make-load-binary
    (lambda (fn)
      (make-load-binary '$make-load-binary fn 'load #f #f)))

  (set-who! load-compiled-from-port
    (lambda (ip)
      (unless (and (input-port? ip) (binary-port? ip))
        ($oops who "~s is not a binary input port" ip))
      (do-load-binary who (port-name ip) ip 'load #f #f)))

  (set-who! visit-compiled-from-port
    (lambda (ip)
      (unless (and (input-port? ip) (binary-port? ip))
        ($oops who "~s is not a binary input port" ip))
      (do-load-binary who (port-name ip) ip 'visit #f #f)))

  (set-who! revisit-compiled-from-port
    (lambda (ip)
      (unless (and (input-port? ip) (binary-port? ip))
        ($oops who "~s is not a binary input port" ip))
      (do-load-binary who (port-name ip) ip 'revisit #f #f)))

  (set-who! load-program
    (rec load-program
      (case-lambda
        [(fn) (load-program fn eval)]
        [(fn ev)
         (unless (string? fn) ($oops who "~s is not a string" fn))
         (unless (procedure? ev) ($oops who "~s is not a procedure" ev))
         (with-source-path who fn
           (lambda (fn)
             (do-load who fn 'load #f #f
               (lambda (ip sfd do-read)
                 ($set-port-flags! ip (constant port-flag-r6rs))
                 (let loop ([x* '()])
                   (let ([x (do-read)])
                     (if (eof-object? x)
                         (begin
                           (close-port ip)
                           (ev `(top-level-program ,@(reverse x*)))
                           (void))
                         (loop (cons x x*)))))))))])))

  (set-who! load-library ; like load, but sets #!r6rs mode
    (rec load-library
      (case-lambda
        [(fn) (load-library fn eval)]
        [(fn ev)
         (unless (string? fn) ($oops who "~s is not a string" fn))
         (unless (procedure? ev) ($oops who "~s is not a procedure" ev))
         (with-source-path who fn
           (lambda (fn)
             (do-load who fn 'load #f #f
               (lambda (ip sfd do-read)
                 ($set-port-flags! ip (constant port-flag-r6rs))
                 (let loop ()
                   (let ([x (do-read)])
                     (unless (eof-object? x)
                       (ev x)
                       (loop))))
                 (close-port ip)))))])))

  (set! $load-library ; for syntax.ss load-library
    ; like load, but sets #!r6rs mode and does not use with-source-path,
    ; since syntax.ss load-library has already determined the path.
    ; adds fn's directory to source-directories
    (lambda (fn situation importer)
      (define who 'import)
      (let ([fn (let ([host-fn (format "~a.~s" (path-root fn) (machine-type))])
                  (if (file-exists? host-fn) host-fn fn))])
        (do-load who fn situation #t importer
          (lambda (ip sfd do-read)
            ($set-port-flags! ip (constant port-flag-r6rs))
            (parameterize ([source-directories (cons (path-parent fn) (source-directories))])
              (let loop ()
                (let ([x (do-read)])
                  (unless (eof-object? x)
                    (eval x)
                    (loop)))))
            (close-port ip))))))

  (set-who! load
    (rec load
      (case-lambda
        [(fn) (load fn eval)]
        [(fn ev)
         (unless (string? fn) ($oops who "~s is not a string" fn))
         (unless (procedure? ev) ($oops who "~s is not a procedure" ev))
         (with-source-path who fn
           (lambda (fn)
             (do-load who fn 'load #f #f
               (lambda (ip sfd do-read)
                 (let loop ()
                   (let ([x (do-read)])
                     (unless (eof-object? x)
                       (ev x)
                       (loop))))
                 (close-port ip)))))])))

  (set! $visit
    (lambda (who fn importer)
      (do-load who fn 'visit #t importer #f)))

  (set! $revisit
    (lambda (who fn importer)
      (do-load who fn 'revisit #t importer #f)))

  (set-who! visit
    (lambda (fn)
      (do-load who fn 'visit #f #f #f)))

  (set-who! revisit
    (lambda (fn)
      (do-load who fn 'revisit #f #f #f))))

(let ()
  (module sstats-record (make-sstats sstats? sstats-cpu sstats-real
                          sstats-bytes sstats-gc-count sstats-gc-cpu
                          sstats-gc-real sstats-gc-bytes
                          set-sstats-cpu! set-sstats-real!
                          set-sstats-bytes! set-sstats-gc-count!
                          set-sstats-gc-cpu! set-sstats-gc-real!
                          set-sstats-gc-bytes!)
    (define-record-type (sstats make-sstats sstats?)
      (nongenerative #{sstats pfwch3jd8ts96giujpitoverj-0})
      (sealed #t)
      (fields 
        (mutable cpu sstats-cpu set-sstats-cpu!)
        (mutable real sstats-real set-sstats-real!)
        (mutable bytes sstats-bytes set-sstats-bytes!)
        (mutable gc-count sstats-gc-count set-sstats-gc-count!)
        (mutable gc-cpu sstats-gc-cpu set-sstats-gc-cpu!)
        (mutable gc-real sstats-gc-real set-sstats-gc-real!)
        (mutable gc-bytes sstats-gc-bytes set-sstats-gc-bytes!))
      (protocol
        (lambda (new)
          (lambda (cpu real bytes gc-count gc-cpu gc-real gc-bytes)
            (new cpu real bytes gc-count gc-cpu gc-real gc-bytes))))))
  (define exact-integer? (lambda (x) (and (integer? x) (exact? x))))
  (set-who! make-sstats 
    (lambda (cpu real bytes gc-count gc-cpu gc-real gc-bytes)
      (define verify-time
        (lambda (name x)
          (unless (time? x)
            ($oops who "~s value ~s is not a time record" name x))))
      (define verify-exact-integer
        (lambda (name x)
          (unless (exact-integer? x)
            ($oops who "~s value ~s is not an exact integer" name x))))
      (import sstats-record)
      (verify-time 'cpu cpu)
      (verify-time 'real real)
      (verify-exact-integer 'bytes bytes)
      (verify-exact-integer 'gc-count gc-count)
      (verify-time 'gc-cpu gc-cpu)
      (verify-time 'gc-real gc-real)
      (verify-exact-integer 'gc-bytes gc-bytes)
      (make-sstats cpu real bytes gc-count gc-cpu gc-real gc-bytes)))
  (set! sstats? (lambda (x) (import sstats-record) (sstats? x)))
  (let ()
    (define verify-sstats
      (lambda (who x)
        (import sstats-record)
        (unless (sstats? x) ($oops who "~s is not an sstats record" x))))
    (define verify-exact-integer
      (lambda (who x)
        (unless (exact-integer? x)
          ($oops who "~s is not an exact integer" x))))
    (define verify-time
      (lambda (who x)
        (unless (time? x)
          ($oops who "~s is not a time record" x))))
    (define-syntax field
      (lambda (x)
        (syntax-case x ()
          [(_ name verify-arg)
            (with-syntax ([sstats-name (construct-name #'sstats-record "sstats-" #'name)]
                          [set-sstats-name! (construct-name #'sstats-record "set-sstats-" #'name "!")])
              #'(begin
                  (set-who! sstats-name
                    (lambda (x)
                      (import sstats-record)
                      (verify-sstats who x)
                      (sstats-name x)))
                  (set-who! set-sstats-name!
                    (lambda (x n)
                      (import sstats-record)
                      (verify-sstats who x)
                      (verify-arg who n)
                      (set-sstats-name! x n)))))])))
    (field cpu verify-time)
    (field real verify-time)
    (field bytes verify-exact-integer)
    (field gc-count verify-exact-integer)
    (field gc-cpu verify-time)
    (field gc-real verify-time)
    (field gc-bytes verify-exact-integer)))

(define-who sstats-print
  (rec sstats-print
   (case-lambda
     [(s) (sstats-print s (current-output-port))]
     [(s port)
      (unless (sstats? s)
        ($oops who "~s is not an sstats record" s))
      (unless (and (output-port? port) (textual-port? port))
        ($oops who "~s is not a textual output port" port))
      (let ([collections (sstats-gc-count s)]
            [time->string
             (lambda (x)
               ;; based on record-writer for ts in date.ss
               (let ([sec (time-second x)] [nsec (time-nanosecond x)])
                 (if (and (< sec 0) (> nsec 0))
                     (format "-~d.~9,'0ds" (- -1 sec) (- 1000000000 nsec))
                     (format "~d.~9,'0ds" sec nsec))))])
         (if (zero? collections)
             (fprintf port
"    no collections
    ~a elapsed cpu time
    ~a elapsed real time
    ~s bytes allocated
"
                      (time->string (sstats-cpu s))
                      (time->string (sstats-real s))
                      (sstats-bytes s))
             (fprintf port
"    ~s collection~:p
    ~a elapsed cpu time, including ~a collecting
    ~a elapsed real time, including ~a collecting
    ~s bytes allocated, including ~s bytes reclaimed
"
                      collections
                      (time->string (sstats-cpu s)) (time->string (sstats-gc-cpu s))
                      (time->string (sstats-real s)) (time->string (sstats-gc-real s))
                      (sstats-bytes s) (sstats-gc-bytes s))))])))

(define display-statistics
   (case-lambda
      [() (display-statistics (current-output-port))]
      [(p)
       (unless (and (output-port? p) (textual-port? p))
          ($oops 'display-statistics "~s is not a textual output port" p))
       (sstats-print (statistics) p)]))

(define-who sstats-difference
   (lambda (a b)
      (unless (sstats? a)
        ($oops who "~s is not an sstats record" a))
      (unless (sstats? b)
        ($oops who "~s is not an sstats record" b))
      (let ([int-diff (lambda (f a b) (- (f a) (f b)))]
            [time-diff (lambda (f a b) (time-difference (f a) (f b)))])
         (make-sstats
           (time-diff sstats-cpu a b)
           (time-diff sstats-real a b)
           (int-diff sstats-bytes a b)
           (int-diff sstats-gc-count a b)
           (time-diff sstats-gc-cpu a b)
           (time-diff sstats-gc-real a b)
           (int-diff sstats-gc-bytes a b)))))

(define collect-generation-radix
   (make-parameter
      4
      (lambda (v)
         (unless (and (fixnum? v) (fx< 0 v))
            ($oops 'collect-generation-radix "~s is not a positive fixnum" v))
         v)))

(define $reset-protect
  (lambda (body out)
    ((call/cc
       (lambda (k)
         (parameterize ([reset-handler
                         (lambda ()
                           (k (lambda ()
                                (out)
                                ((reset-handler)))))])
           (with-exception-handler
             (lambda (c)
              ; would prefer not to burn bridges even for serious condition
              ; if the exception is continuable, but we have no way to know
              ; short of grubbing through the continuation
               (if (serious-condition? c)
                   (k (lambda () (out) (raise c)))
                   (raise-continuable c)))
             (lambda ()
               (call-with-values body
                 (case-lambda
                   [(v) (lambda () v)]
                   [v* (lambda () (apply values v*))]))))))))))

(define exit-handler)
(define reset-handler)
(define abort-handler)
(let ([c-exit (foreign-procedure "(cs)c_exit" (integer-32) void)])
  (define (integer-32? x)
    (and (integer? x)
         (exact? x)
         (<= #x-80000000 x #x7fffffff)))

  (set! exit-handler
    ($make-thread-parameter
      (case-lambda
        [() (c-exit 0)]
        [(x . args) (c-exit (if (eqv? x (void)) 0 (if (integer-32? x) x -1)))])
      (lambda (v)
        (unless (procedure? v)
          ($oops 'exit-handler "~s is not a procedure" v))
        v)))

  (set! reset-handler
    ($make-thread-parameter
      (lambda () (c-exit 0))
      (lambda (v)
        (unless (procedure? v)
          ($oops 'reset-handler "~s is not a procedure" v))
        v)))

  (set! abort-handler
    ($make-thread-parameter
      (case-lambda
        [() (c-exit -1)]
        [(x) (c-exit (if (eqv? x (void)) 0 (if (integer-32? x) x -1)))])
      (lambda (v)
        (unless (procedure? v)
          ($oops 'abort-handler "~s is not a procedure" v))
        v))))

(let ()
  (define (unexpected-return who)
    ($oops who (format "unexpected return from ~s handler" who)))

  (set-who! exit
    (lambda args
      (apply (exit-handler) args)
      (unexpected-return who)))

  (set-who! #(r6rs: exit)
    (case-lambda
      [() ((exit-handler)) (unexpected-return who)]
      [(x) ((exit-handler) x) (unexpected-return who)]))

  (set-who! reset
    (lambda ()
      ((reset-handler))
      (unexpected-return who)))

  (set-who! abort
    (case-lambda
      [() ((abort-handler)) (unexpected-return who)]
      [(x) ((abort-handler) x) (unexpected-return who)])))

(define $interrupt ($make-thread-parameter void))

(define $format-scheme-version
  (lambda (n)
    (if (= (logand n 255) 0)
        (format "~d.~d"
          (ash n -16)
          (logand (ash n -8) 255))
        (format "~d.~d.~d"
          (ash n -16)
          (logand (ash n -8) 255)
          (logand n 255)))))

; set in back.ss
(define $scheme-version)

(define scheme-version-number
  (lambda ()
    (let ([n (constant scheme-version)])
      (values
        (ash n -16)
        (logand (ash n -8) 255)
        (logand n 255)))))

(define scheme-version
  (let ([s #f])
    (lambda ()
      (unless s
        (set! s
          (format "~:[Petite ~;~]Chez Scheme Version ~a"
            $compiler-is-loaded?
            $scheme-version)))
      s)))

(define petite?
  (lambda ()
    (not $compiler-is-loaded?)))

(define threaded?
  (lambda ()
    (if-feature pthreads #t #f)))

(define get-process-id (foreign-procedure "(cs)getpid" () integer-32))

(set! get-thread-id
  (lambda ()
    ($tc-field 'threadno ($tc))))

(define-who sleep
  (let ([fp (foreign-procedure "(cs)nanosleep" (ptr ptr) void)])
    (lambda (t)
      (unless (and (time? t) (eq? (time-type t) 'time-duration))
        ($oops who "~s is not a time record of type time-duration" t))
      (let ([s (time-second t)])
        (when (>= s 0)
          (fp s (time-nanosecond t)))))))

(define $scheme-greeting
  (lambda ()
    (format "~a\nCopyright 1984-2022 Cisco Systems, Inc.\n"
      (scheme-version))))

(define $session-key #f)
(define $scheme-init)
(define $scheme)
(define $script)
(define $as-time-goes-by)
(define collect)
(define break-handler)
(define debug)

(let ()

(define debug-condition* '())

(module (docollect collect-init)
  (define gc-trip 0)
  (define gc-cpu (make-time 'time-collector-cpu 0 0))
  (define gc-real (make-time 'time-collector-real 0 0))
  (define gc-bytes 0)
  (define gc-count 0)
  (define start-bytes 0)
  (define docollect
    (let ([do-gc (foreign-procedure "(cs)do_gc" (int int int) void)])
      (lambda (p)
        (with-tc-mutex
          (unless (= $active-threads 1)
            ($oops 'collect "cannot collect when multiple threads are active"))
          (let-values ([(trip g gmintarget gmaxtarget) (p gc-trip)])
            (set! gc-trip trip)
            (let ([cpu (current-time 'time-thread)] [real (current-time 'time-monotonic)])
              (set! gc-bytes (+ gc-bytes (bytes-allocated)))
              (when (collect-notify)
                (fprintf (console-output-port)
                  "~%[collecting generation ~s into generation ~s..."
                  g gmaxtarget)
                (flush-output-port (console-output-port)))
              (when (eqv? g (collect-maximum-generation))
                ($clear-source-lines-cache))
              (do-gc g gmintarget gmaxtarget)
              ($close-resurrected-files)
              (when-feature pthreads
                ($close-resurrected-mutexes&conditions))
              (when (collect-notify)
                (fprintf (console-output-port) "done]~%")
                (flush-output-port (console-output-port)))
              (set! gc-bytes (- gc-bytes (bytes-allocated)))
              (set! gc-cpu (add-duration gc-cpu (time-difference (current-time 'time-thread) cpu)))
              (set! gc-real (add-duration gc-real (time-difference (current-time 'time-monotonic) real)))
              (set! gc-count (1+ gc-count))))))))
  (define collect-init
    (lambda ()
      (set! gc-trip 0)
      (set! gc-cpu (make-time 'time-collector-cpu 0 0))
      (set! gc-real (make-time 'time-collector-real 0 0))
      (set! gc-count 0)
      (set! gc-bytes 0)
      (set! start-bytes (bytes-allocated))))
  (set! $gc-real-time (lambda () gc-real))
  (set! $gc-cpu-time (lambda () gc-cpu))
  (set! initial-bytes-allocated (lambda () start-bytes))
  (set! bytes-deallocated (lambda () gc-bytes))
  (set! collections (lambda () gc-count))
  (set! statistics
    (lambda ()
      (make-sstats
        (current-time 'time-thread)
        (current-time 'time-monotonic)
        (+ (- (bytes-allocated) start-bytes) gc-bytes)
        gc-count
        gc-cpu
        gc-real
        gc-bytes))))

(set-who! collect
  (let ()
    (define collect0
      (lambda ()
        (docollect
          (lambda (gct)
            (let ([gct (+ gct 1)])
              (let ([cmg (collect-maximum-generation)])
                (let loop ([g cmg])
                  (if (= (modulo gct (expt (collect-generation-radix) g)) 0)
                      (if (fx= g cmg)
                          (values 0 g (fxmin g 1) g)
                          (values gct g 1 (fx+ g 1)))
                      (loop (fx- g 1))))))))))
    (define collect2
      (lambda (g gmintarget gmaxtarget)
        (docollect
          (lambda (gct)
            (values 
             ; make gc-trip to look like we've just collected generation g
             ; w/o also having collected generation g+1
              (if (fx= g (collect-maximum-generation))
                  0
                  (let ([gct (+ gct 1)])
                    (define (trip g)
                      (let ([n (expt (collect-generation-radix) g)])
                        (+ gct (modulo (- n gct) n))))
                    (let ([next (trip g)] [limit (trip (fx+ g 1))])
                      (if (< next limit) next (- limit 1)))))
              g gmintarget gmaxtarget)))))
    (case-lambda
      [() (collect0)]
      [(g)
       (let ([cmg (collect-maximum-generation)])
         (unless (and (fixnum? g) (fx<= 0 g cmg))
           ($oops who "invalid generation ~s" g))
         (let ([gtarget (if (fx= g cmg) g (fx+ g 1))])
           (collect2 g gtarget gtarget)))]
      [(g gtarget)
       (let ([cmg (collect-maximum-generation)])
         (unless (and (fixnum? g) (fx<= 0 g cmg))
           ($oops who "invalid generation ~s" g))
         (unless (if (fx= g cmg)
                     (or (eqv? gtarget g) (eq? gtarget 'static))
                     (or (eqv? gtarget g) (eqv? gtarget (fx+ g 1))))
           ($oops who "invalid target generation ~s for generation ~s" gtarget g)))
       (let ([gtarget (if (eq? gtarget 'static) (constant static-generation) gtarget)])
         (collect2 g gtarget gtarget))]
      [(g gmintarget gmaxtarget)
       (let ([cmg (collect-maximum-generation)])
         (unless (and (fixnum? g) (fx<= 0 g cmg))
           ($oops who "invalid generation ~s" g))
         (unless (if (fx= g cmg)
                     (or (eqv? gmaxtarget g) (eq? gmaxtarget 'static))
                     (or (eqv? gmaxtarget g) (eqv? gmaxtarget (fx+ g 1))))
           ($oops who "invalid maximum target generation ~s for generation ~s" gmaxtarget g))
         (unless (or (eqv? gmintarget gmaxtarget)
                     (and (fixnum? gmintarget)
                          (fx<= 1 gmintarget (if (fixnum? gmaxtarget) gmaxtarget cmg))))
           ($oops who "invalid minimum target generation ~s for generation ~s and maximum target generation ~s" gmintarget g gmaxtarget)))
       (collect2 g
         (if (eq? gmintarget 'static) (constant static-generation) gmintarget)
         (if (eq? gmaxtarget 'static) (constant static-generation) gmaxtarget))])))

(set! collect-rendezvous
  (let ([fire-collector (foreign-procedure "(cs)fire_collector" () void)])
    (lambda ()
      (fire-collector)
      ($collect-rendezvous))))

(set! keyboard-interrupt-handler
   ($make-thread-parameter
      (lambda ()
         (clear-output-port (console-output-port))
         (fresh-line (console-output-port))
         (flush-output-port (console-output-port))
         (($interrupt)))
      (lambda (x)
         (unless (procedure? x)
            ($oops 'keyboard-interrupt-handler "~s is not a procedure" x))
         x)))

(let ()
  (define register-scheme-signal
    (foreign-procedure "(cs)register_scheme_signal" (iptr) void))

  (define signal-alist '())

  (set! register-signal-handler
    (lambda (sig handler)
      (unless (fixnum? sig)
        ($oops 'register-signal-handler "~s is not a fixnum" sig))
      (unless (procedure? handler)
        ($oops 'register-signal-handler "~s is not a procedure" handler))
      (critical-section
        (register-scheme-signal sig)
        (let ((a (assq sig signal-alist)))
          (if a
              (set-cdr! a handler)
              (set! signal-alist (cons (cons sig handler) signal-alist)))))))

  (set! $signal-interrupt-handler
    (lambda (sig)
      (let ((a (assq sig signal-alist)))
        (unless a
          ($oops '$signal-interrupt-handler
                 "unexpected signal number ~d received~%"
                 sig))
        ((cdr a) sig)))))

;;; entry point from C kernel

(set! $scheme-init
  (lambda ()
    (set! debug-condition* '())
    (collect-init)
    ($io-init)
    (set! $session-key #f)
    ($interrupt reset)
    ($clear-pass-stats)
    (enable-interrupts)))

(set! $scheme
  (lambda (fns)
    (define (go)
      (call/cc
        (lambda (k)
          (parameterize ([abort-handler
                          (case-lambda [() (k -1)] [(x) (k x)])]
                         [exit-handler
                          (case-lambda [() (k (void))] [(x . args) (k x)])]
                         [reset-handler (lambda () (k -1))])
            (apply (scheme-start) fns)))))
    (unless (suppress-greeting)
      (display ($scheme-greeting) (console-output-port))
      (newline (console-output-port))
      (flush-output-port (console-output-port)))
    (if-feature expeditor
      (if ($enable-expeditor) ($expeditor go) (go))
      (go))))

(set! $script
  (lambda (program? fn fns)
    (define (go)
      (call/cc
        (lambda (k)
          (parameterize ([abort-handler
                          (case-lambda [() (k -1)] [(x) (k x)])]
                         [exit-handler
                          (case-lambda [() (k (void))] [(x . args) (k x)])]
                         [reset-handler (lambda () (k -1))])
            (apply (if program? (scheme-program) (scheme-script)) fn fns)))))
    (if-feature expeditor
      (if ($enable-expeditor) ($expeditor go) (go))
      (go))))

(set! $as-time-goes-by
  (lambda (e t)
    (define sanitize
      (lambda (s)
        (define sanitize-time
          (lambda (t)
            (if (< (time-second t) 0)
                (make-time 'time-duration 0 0)
                t)))
        (define sanitize-count
          (lambda (n)
            (max n 0)))
        (make-sstats
          (sanitize-time (sstats-cpu s))
          (sanitize-time (sstats-real s))
          (sanitize-count (sstats-bytes s))
          (sanitize-count (sstats-gc-count s))
          (sanitize-time (sstats-gc-cpu s))
          (sanitize-time (sstats-gc-real s))
          (sanitize-count (sstats-gc-bytes s)))))
    (define prstats
      (lambda (b1 b2)
        (let ([a (statistics)])
          (parameterize ([print-level 2] [print-length 2])
            (fprintf (console-output-port) "(time ~s)~%" e))
          (let ([elapsed (sstats-difference a b2)])
            (let ([overhead (sstats-difference b2 b1)])
              (let ([adjusted (sanitize (sstats-difference elapsed overhead))])
                (sstats-print adjusted (console-output-port)))))
          (flush-output-port (console-output-port)))))
    (let ([b1 (statistics)])
      (let ([b2 (statistics)])
        (call-with-values t
          (case-lambda
            [(v) (prstats b1 b2) v]
            [(v1 v2) (prstats b1 b2) (values v1 v2)]
            [(v1 v2 v3) (prstats b1 b2) (values v1 v2 v3)]
            [(v1 v2 v3 v4) (prstats b1 b2) (values v1 v2 v3 v4)]
            [r (prstats b1 b2) (apply values r)]))))))

(set! $report-string
    (lambda (dest what who msg args)
      (let ([what (and (not (equal? what "")) what)]
            [who (and (not (equal? who "")) who)])
        (parameterize ([print-level 3] [print-length 6])
          (format dest "~@[~@(~a~)~]~:[~; in ~]~@[~a~]~:[~;: ~]~@[~?~]"
            what
            (and what who)
            who
            (and (or what who) (not (equal? msg "")))
            msg
            args)))))

(let ()
(define report
  (lambda (what who msg args)
    (fresh-line (console-output-port))
    ($report-string (console-output-port) what who msg args)
    (newline (console-output-port))
    (flush-output-port (console-output-port))))

(set! break-handler
   ($make-thread-parameter
      (case-lambda
         [(who msg . args)
          (unless (string? msg)
             ($oops 'default-break-handler "~s is not a string" msg))
          (report "break" who msg args)
          (($interrupt))]
         [(who)
          (report "break" who "" '())
          (($interrupt))]
         [()
          (($interrupt))])
      (lambda (x)
         (unless (procedure? x)
            ($oops 'break-handler "~s is not a procedure" x))
         x)))
)

(set-who! debug-condition
  (case-lambda
    [() (cond
          [(assv ($tc-field 'threadno ($tc)) debug-condition*) => cdr]
          [else #f])]
    [(c)
     (let ([n ($tc-field 'threadno ($tc))])
       (with-tc-mutex
         (set! debug-condition*
           (let ([ls (remp (lambda (a) (eqv? (car a) n)) debug-condition*)])
             (if c (cons (cons n c) ls) ls)))))]))

(set! debug
  (lambda ()
    (define line-limit 74)
    (define pad
      (lambda (s n p)
        (let ([i (string-length s)])
          (when (> n i) (display (make-string (- n i) #\space) p))
          (display s p)
          (max i n))))
    (define numbered-line-display
      (lambda (point? n c p)
        (display (if point? "*" " "))
        (let ([s (with-output-to-string (lambda () (display-condition c)))])
          (let ([k (- line-limit (+ (pad (number->string n) 4 p) 2))])
            (display ": " p)
            (let ([i (string-length s)])
              (if (> i k)
                  (fprintf p "~a ...~%" (substring s 0 (- k 4)))
                  (fprintf p "~a~%" s)))))))
    (define unnumbered-line-display
      (lambda (c p)
        (let ([s (with-output-to-string (lambda () (display-condition c)))])
          (let ([k (- line-limit 2)])
            (display "  " p)
            (let ([i (string-length s)])
              (if (> i k)
                  (fprintf p "~a ...~%" (substring s 0 (- k 4)))
                  (fprintf p "~a~%" s)))))))
    (define printem
      (lambda (point ls p)
        (if (null? (cdr ls))
            (let ([x (car ls)])
              (unnumbered-line-display (cdr x) p))
            (for-each
              (lambda (x)
                (numbered-line-display (eq? x point) (car x) (cdr x) p))
              ls))))
    (define debug-cafe
      (lambda (point ls)
        (parameterize ([$interrupt void])
          (clear-input-port (console-input-port))
          (let ([waiter (call/cc
                          (lambda (k)
                            (rec f (lambda () (k f)))))])
            (fprintf (console-output-port) "debug> ")
            (flush-output-port (console-output-port))
            (let ([x (let ([x (parameterize ([$interrupt waiter]
                                             [reset-handler waiter])
                                (read (console-input-port)))])
                       (if (eof-object? x)
                           (begin
                             (newline (console-output-port))
                             (flush-output-port (console-output-port))
                             'e)
                           x))])
              (case x
                [(i)
                 (let ([c (cdr point)])
                   (if (continuation-condition? c)
                       (inspect (condition-continuation c))
                       (display "the raise continuation is not available\n")))
                 (waiter)]
                [(c)
                 (inspect (cdr point))
                 (waiter)]
                [(q)
                 (with-tc-mutex
                   (for-each
                     (lambda (x) (set! debug-condition* (remq x debug-condition*)))
                     ls))
                 (void)]
                [(e)
                 (void)]
                [(s)
                 (printem point
                   (sort (lambda (x y) (< (car x) (car y))) ls)
                   (console-output-port))
                 (waiter)]
                [(?)
                 (if (null? (cdr ls))
                     (fprintf (console-output-port)
"Type i  to inspect the raise continuation (if available)
     s  to display the condition
     c  to inspect the condition
     e  or eof to exit the debugger, retaining error continuation
     q  to exit the debugger, discarding error continuation
")
                     (fprintf (console-output-port)
"Type i  to inspect the selected thread's raise continuation (if available)
    <n> to select thread <n>
     s  to display the conditions
     c  to inspect the selected thread's condition
     e  or eof to exit the debugger, retaining error continuations
     q  to exit the debugger, discarding error continuations
"))
                 (flush-output-port (console-output-port))
                 (waiter)]
                [else
                 (cond
                   [(assv x ls) =>
                    (lambda (a)
                      (set! point a)
                      (waiter))]
                   [(and (integer? x) (nonnegative? x))
                    (fprintf (console-output-port)
                       "No saved error continuation for thread ~s.~%"
                       x)
                    (flush-output-port (console-output-port))
                    (waiter)]
                   [else
                    (fprintf (console-output-port)
                      "Invalid command.  Type ? for options.~%")
                    (flush-output-port (console-output-port))
                    (waiter)])]))))))
    (let ([ls debug-condition*])
      (cond
        [(null? ls)
         (fprintf (console-output-port) "Nothing to debug.~%")
         (flush-output-port (console-output-port))]
        [else
         (debug-cafe (car ls) ls)]))))
)

(define $collect-rendezvous
  (lambda ()
    (define once
      (let ([once #f])
        (lambda ()
          (when (eq? once #t)
            ($oops '$collect-rendezvous
              "cannot return to the collect-request-handler"))
          (set! once #t))))
    (if-feature pthreads
      (with-tc-mutex
        (let f ()
          (when $collect-request-pending
            (if (= $active-threads 1) ; last one standing
                (dynamic-wind
                  once
                  (collect-request-handler)
                  (lambda ()
                    (set! $collect-request-pending #f)
                    (condition-broadcast $collect-cond)))
                (begin
                  (condition-wait $collect-cond $tc-mutex)
                  (f))))))
      (critical-section
        (dynamic-wind 
          once
          (collect-request-handler)
          (lambda () (set! $collect-request-pending #f)))))))

(define collect-request-handler
   (make-parameter
      (lambda () (collect))
      (lambda (x)
         (unless (procedure? x)
            ($oops 'collect-request-handler "~s is not a procedure" x))
         x)))

(define collect-notify (make-parameter #f (lambda (x) (and x #t))))

(define $c-error
  (lambda (arg . error-args)
   ; error-args may be present along doargerr path, but we presently
   ; ignore them
    (define-syntax c-error-case
      (lambda (x)
        (syntax-case x ()
          [(_ arg [(key) fmls e1 e2 ...] ...)
           (with-syntax ([(k ...) (map lookup-constant (datum (key ...)))])
             #'(let ([t arg])
                 (record-case t
                   [(k) fmls e1 e2 ...]
                   ...
                   [else ($oops '$c-error "invalid error type ~s" t)])))])))
    (c-error-case arg
      [(ERROR_OTHER) args (apply $oops args)]
      [(ERROR_CALL_UNBOUND) (cnt symbol arg1?)
       ($oops #f "variable ~:s is not bound" symbol)]
      [(ERROR_CALL_NONPROCEDURE_SYMBOL) (cnt symbol arg1?)
       ($oops #f "attempt to apply non-procedure ~s"
                 ($top-level-value symbol))]
      [(ERROR_CALL_NONPROCEDURE) (cnt nonprocedure arg1?)
       ($oops #f "attempt to apply non-procedure ~s" nonprocedure)]
      [(ERROR_CALL_ARGUMENT_COUNT) (cnt procedure arg1?)
       ($oops #f "incorrect number of arguments to ~s" procedure)]
      [(ERROR_RESET) (who msg . args)
       ($oops who "~?.  Some debugging context lost" msg args)]
      [(ERROR_NONCONTINUABLE_INTERRUPT) args
       (let ([noncontinuable-interrupt
              (lambda ()
                 ((keyboard-interrupt-handler))
                 (fprintf (console-output-port)
                          "Noncontinuable interrupt.~%")
                 (reset))])
          ;; ruse to get inspector to print "continuation in
          ;; noncontinuable-interrupt" instead of "#c-error".
          (noncontinuable-interrupt))]
      [(ERROR_VALUES) (cnt)
       ($oops #f
         "returned ~r values to single value return context"
         cnt)]
      [(ERROR_MVLET) (cnt)
       ($oops #f
         "incorrect number of values received in multiple value context")])))

(define break
  (lambda args
    (apply (break-handler) args)))

(define timer-interrupt-handler
   ($make-thread-parameter
      (lambda ()
         ($oops 'timer-interrupt
                "timer interrupt occurred with no handler defined"))
      (lambda (x)
         (unless (procedure? x)
            ($oops 'timer-interrupt-handler "~s is not a procedure" x))
         x)))

(define $symbol-type
  (lambda (name)
    (let ((flags ($sgetprop name '*flags* 0)))
      (cond
        [(any-set? (prim-mask system) flags) 'system]
        [(any-set? (prim-mask primitive) flags) 'primitive]
        [(any-set? (prim-mask keyword) flags)
         (if (any-set? (prim-mask library-uid) flags)
             'library-uid
             'keyword)]
        [(any-set? (prim-mask system-keyword) flags)
         (if (any-set? (prim-mask library-uid) flags)
             'system-library-uid
             'system-keyword)]
        [else 'unknown]))))

(let ()
 ; naive version is good enough for apropos
  (define (substring? s1 s2)
    (let ([n1 (string-length s1)] [n2 (string-length s2)])
      (let loop2 ([i2 0])
        (let loop1 ([i1 0] [j i2])
          (if (fx= i1 n1)
              i2
              (and (not (fx= j n2))
                   (if (char=? (string-ref s1 i1) (string-ref s2 j))
                       (loop1 (fx+ i1 1) (fx+ j 1))
                       (loop2 (fx+ i2 1)))))))))
  (define sym<? (lambda (x y) (string-ci<? (symbol->string x) (symbol->string y))))
  (define apropos-help
    (lambda (s env)
      (let ([s (if (symbol? s) (symbol->string s) s)])
        (sort sym<?
          (let f ([ls (environment-symbols env)])
            (if (null? ls)
                '()
                (if (substring? s (symbol->string (car ls)))
                    (cons (car ls) (f (cdr ls)))
                    (f (cdr ls)))))))))
  (define apropos-library-help
    (lambda (s)
      (define lib<?
        (lambda (lib1 lib2)
          (and (not (null? lib2))
               (or (null? lib1)
                   (if (eq? (car lib1) (car lib2))
                       (lib<? (cdr lib1) (cdr lib2))
                       (sym<? (car lib1) (car lib2)))))))
      (let ([s (if (symbol? s) (symbol->string s) s)])
        (sort (lambda (ls1 ls2) (lib<? (car ls1) (car ls2)))
          (let do-libs ([lib* (library-list)] [match** '()])
            (if (null? lib*)
                match**
                (do-libs (cdr lib*)
                  (let do-exports ([x* (library-exports (car lib*))] [match* '()])
                    (if (null? x*)
                        (if (null? match*)
                            match**
                            (cons (cons (car lib*) (sort sym<? match*)) match**))
                        (do-exports (cdr x*)
                          (if (substring? s (symbol->string (car x*)))
                              (cons (car x*) match*)
                              match*)))))))))))
  (define check-s
    (lambda (who s)
      (unless (or (symbol? s) (string? s))
        ($oops who "~s is not a symbol or string" s))))
  (define check-env
    (lambda (who env)
      (unless (environment? env)
        ($oops 'apropos-list "~s is not an environment" env))))
  (set! apropos-list
    (case-lambda
      [(s)
       (check-s 'apropos-list s)
       (append
         (apropos-help s (interaction-environment))
         (apropos-library-help s))]
      [(s env)
       (check-s 'apropos-list s)
       (check-env 'apropos-list env)
       (append
         (apropos-help s env)
         (apropos-library-help s))]))
  (let ()
    (define do-apropos
      (lambda (who where s env)
        (printf "~a environment:\n ~{~<~%~& ~1:; ~s~>~^,~}~&" where (apropos-help s env))
        (for-each
          (lambda (x) (printf "~s:\n ~{~<~%~& ~1:; ~s~>~^,~}~&" (car x) (cdr x)))
          (apropos-library-help s))))
    (set-who! apropos
      (case-lambda
        [(s)
         (check-s who s)
         (do-apropos who "interaction" s (interaction-environment))]
        [(s env)
         (check-s who s)
         (check-env who env)
         (do-apropos who "supplied" s env)]))))

(let ()
  (define-record-type pass-stats
    (nongenerative)
    (sealed #t)
    (fields 
      (mutable calls)
      (mutable cpu)
      (mutable gc-cpu)
      (mutable bytes))
    (protocol
      (lambda (n)
        (lambda ()
          (let ([t (make-time 'time-duration 0 0)])
            (n 0 t t 0))))))

  (define field-names '(name calls cpu gc-cpu bytes))

  (define stats-ht)

  (define-threaded outer-ps #f)

  (set! $clear-pass-stats
    (lambda ()
      (set! stats-ht (make-eq-hashtable))))

  (set! $enable-pass-timing (make-parameter #f))
  
  (set-who! $pass-time
    (lambda (name thunk)
      (unless (symbol? name) ($oops who "~s is not a symbol" name))
      (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
      (if ($enable-pass-timing)
          (let ([ps (with-tc-mutex
                      (let ([a (hashtable-cell stats-ht name #f)])
                        (let ([ps (or (cdr a) (let ([ps (make-pass-stats)]) (set-cdr! a ps) ps))])
                          (pass-stats-calls-set! ps (+ (pass-stats-calls ps) 1))
                          ps)))])
            (dynamic-wind
              (lambda ()
                (with-tc-mutex
                  (let ([cpu (current-time 'time-thread)]
                        [gc-cpu (current-time 'time-collector-cpu)]
                        [bytes (+ (bytes-deallocated) (bytes-allocated))])
                    (set-time-type! cpu 'time-duration)
                    (set-time-type! gc-cpu 'time-duration)
                    (when outer-ps
                      (pass-stats-cpu-set! outer-ps (add-duration (pass-stats-cpu outer-ps) cpu))
                      (pass-stats-gc-cpu-set! outer-ps (add-duration (pass-stats-gc-cpu outer-ps) gc-cpu))
                      (pass-stats-bytes-set! outer-ps (+ (pass-stats-bytes outer-ps) bytes)))
                    (pass-stats-cpu-set! ps (subtract-duration (pass-stats-cpu ps) cpu))
                    (pass-stats-gc-cpu-set! ps (subtract-duration (pass-stats-gc-cpu ps) gc-cpu))
                    (pass-stats-bytes-set! ps (- (pass-stats-bytes ps) bytes)))))
              (lambda () (fluid-let ([outer-ps ps]) (thunk)))
              (lambda ()
                (with-tc-mutex
                  (let ([cpu (current-time 'time-thread)]
                        [gc-cpu (current-time 'time-collector-cpu)]
                        [bytes (+ (bytes-deallocated) (bytes-allocated))])
                    (set-time-type! cpu 'time-duration)
                    (set-time-type! gc-cpu 'time-duration)
                    (when outer-ps
                      (pass-stats-cpu-set! outer-ps (subtract-duration (pass-stats-cpu outer-ps) cpu))
                      (pass-stats-gc-cpu-set! outer-ps (subtract-duration (pass-stats-gc-cpu outer-ps) gc-cpu))
                      (pass-stats-bytes-set! outer-ps (- (pass-stats-bytes outer-ps) bytes)))
                    (pass-stats-cpu-set! ps (add-duration (pass-stats-cpu ps) cpu))
                    (pass-stats-gc-cpu-set! ps (add-duration (pass-stats-gc-cpu ps) gc-cpu))
                    (pass-stats-bytes-set! ps (+ (pass-stats-bytes ps) bytes)))))))
          (thunk))))

  (set-who! $pass-stats-fields (lambda () field-names))

  (set! $pass-stats
    (lambda ()
      (define (build-result namev psv)
        (vector->list
          (vector-map
            (lambda (name ps) 
              (list name 
                (pass-stats-calls ps)
                (pass-stats-cpu ps)
                (pass-stats-gc-cpu ps)
                (pass-stats-bytes ps)))
            namev
            psv)))
      (with-tc-mutex
        (if outer-ps
            (let ([cpu (current-time 'time-thread)]
                  [gc-cpu (current-time 'time-collector-cpu)]
                  [bytes (+ (bytes-deallocated) (bytes-allocated))])
              (set-time-type! cpu 'time-duration)
              (set-time-type! gc-cpu 'time-duration)
              (pass-stats-cpu-set! outer-ps (add-duration (pass-stats-cpu outer-ps) cpu))
              (pass-stats-gc-cpu-set! outer-ps (add-duration (pass-stats-gc-cpu outer-ps) gc-cpu))
              (pass-stats-bytes-set! outer-ps (+ (pass-stats-bytes outer-ps) bytes))
              (let ([result (call-with-values (lambda () (hashtable-entries stats-ht)) build-result)])
                (pass-stats-cpu-set! outer-ps (subtract-duration (pass-stats-cpu outer-ps) cpu))
                (pass-stats-gc-cpu-set! outer-ps (subtract-duration (pass-stats-gc-cpu outer-ps) gc-cpu))
                (pass-stats-bytes-set! outer-ps (- (pass-stats-bytes outer-ps) bytes))
                result))
              (call-with-values (lambda () (hashtable-entries stats-ht)) build-result)))))

  (let ()
    (define who '$print-pass-stats)
    (define field-name-strings (map symbol->string field-names))
    (define check-psls
      (lambda (psl*)
        (unless (list? psl*) ($oops who "~s is not a list" psl*))
        (for-each
          (lambda (psl)
            (define exact-integer? (lambda (x) (or (fixnum? x) (bignum? x))))
            (unless (and (fx= ($list-length psl who) 5)
                         (apply (lambda (name calls cpu gc-cpu bytes)
                                  (and (exact-integer? calls)
                                       (time? cpu)
                                       (time? gc-cpu)
                                       (exact-integer? bytes)))
                           psl))
              ($oops who "malformed pass-stats entry ~s" psl)))
          psl*)))
    (define val->string
      (lambda (x)
        (cond
          [(time? x)
           (let-values ([(sec nsec)
                         (let ([sec (time-second x)] [nsec (time-nanosecond x)])
                           (if (and (< sec 0) (> nsec 0))
                               (values (+ sec 1) (- 1000000000 nsec))
                               (values sec nsec)))])
             (format "~d.~9,'0d" sec nsec))]
          [else (format "~s" x)])))
    (define (print-pass-stats key psl*)
      (define psl<?
        (lambda (x y)
          (apply (lambda (x-name x-calls x-cpu x-gc-cpu x-bytes)
                   (apply (lambda (y-name y-calls y-cpu y-gc-cpu y-bytes)
                            (case (or key 'non-gc-cpu)
                              [(non-gc-cpu)
                               (time<?
                                 (time-difference x-cpu x-gc-cpu)
                                 (time-difference y-cpu y-gc-cpu))]
                              [(cpu) (time<? x-cpu y-cpu)]
                              [(gc-cpu) (time<? x-gc-cpu y-gc-cpu)]
                              [(bytes) (< x-bytes y-bytes)]
                              [(name) (string<? (symbol->string x-name) (symbol->string y-name))]
                              [(calls) (< x-calls y-calls)]
                              [else ($oops who "unrecognized sort key ~s" key)]))
                     y))
            x)))
      ; run check when passed psl* to check psl*; run when passed
      ; the value of ($pass-stats) to check our assumptions
      (check-psls psl*)
      (let ([psl* (append (sort psl<? psl*)
                    (list (let loop ([psl* psl*] [calls 0] [cpu (make-time 'time-duration 0 0)] [gc-cpu (make-time 'time-duration 0 0)] [bytes 0])
                            (if (null? psl*)
                                (list 'TOTAL calls cpu gc-cpu bytes)
                                (apply (lambda (*name *calls *cpu *gc-cpu *bytes)
                                         (loop (cdr psl*)
                                           (+ calls *calls)
                                           (add-duration cpu *cpu)
                                           (add-duration gc-cpu *gc-cpu)
                                           (+ bytes *bytes)))
                                  (car psl*))))))])
        (let ([s** (map (lambda (psl) (map val->string psl)) psl*)])
          (let ([w* (fold-left (lambda (w* s*)
                                 (map (lambda (s w) (fxmax (string-length s) w)) s* w*))
                      (map string-length field-name-strings)
                      s**)])
            (define print-row
              (lambda (s*)
                (printf "~v<~a~;~>  " (car w*) (car s*))
                (for-each (lambda (s w) (printf "~v:<~a~>  " w s)) (cdr s*) (cdr w*))
                (newline)))
            (print-row field-name-strings)
            (print-row (map (lambda (w) (make-string w #\-)) w*))
            (for-each print-row s**)))))
    (set! $print-pass-stats
      (case-lambda
        [() (print-pass-stats #f ($pass-stats))]
        [(key) (print-pass-stats key ($pass-stats))]
        [(key psl*) (print-pass-stats key psl*)]))))
)
