;;; mat.ss
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

;(eval-when (compile load eval) (current-expand sc-expand))
(eval-when (compile) (optimize-level 2))

(eval-when (load eval)
  (define-syntax mat
    (lambda (x)
      (syntax-case x (parameters)
        [(_ x (parameters [param val ...] ...) e ...)
         #'(let f ([p* (list param ...)] [v** (list (list val ...) ...)])
             (if (null? p*)
                 (mat x e ...)
                 (let ([p (car p*)])
                   (for-each
                     (lambda (v)
                       (parameterize ([p v])
                         (f (cdr p*) (cdr v**))))
                     (car v**)))))]
        [(_ x e ...)
         (with-syntax ([(source ...)
                        (map (lambda (clause)
                               (let ([a (syntax->annotation clause)])
                                 (and (annotation? a) (annotation-source a))))
                             #'(e ...))])
           #'(mat-run 'x '(e source) ...))]))))

(define enable-cp0 (make-parameter #f))

(define-syntax mat/cf
  (syntax-rules (testfile)
    [(_ (testfile ?path) expr ...)
     (let* ([path ?path] [testfile.ss (format "~a.ss" path)] [testfile.so (format "~a.so" path)])
       (with-output-to-file testfile.ss
         (lambda () (begin (write 'expr) (newline)) ...)
         'replace)
       (parameterize ([generate-inspector-information #t])
         (compile-file testfile.ss))
       (load testfile.so)
       #t)]
    [(_ expr ...) (mat/cf (testfile "testfile") expr ...)]))

(define mat-output (make-parameter (current-output-port)))

(let ()

(define mat-load
  (lambda (in)
    (call/cc
      (lambda (k)
        (parameterize ([reset-handler (lambda () (k #f))]
                       [current-expand (current-expand)]
                       [run-cp0
                        (let ([default (run-cp0)])
                          (lambda (cp0 x)
                            (if (enable-cp0) (default cp0 x) x)))])
          (with-exception-handler
            (lambda (c)
              (if (warning? c)
                  (raise-continuable c)
                  (begin
                    (fprintf (mat-output) "Error reading mat input: ")
                    (display-condition c (mat-output))
                    (reset))))
            (lambda () (load in))))))))

(define mat-one-exp
  (lambda (expect th sanitize-all?)
    (define (sanitize-condition c)
      (define sanitize
        (lambda (arg)
          (if sanitize-all?
              (cond
                [(port? arg) 'sanitized-port]
                [else 'sanitized-unhandled-type])
             ; go one level only to avoid getting bit by cyclic structures
              (if (list? arg)
                  (map sanitize1 arg)
                  (sanitize1 arg)))))
      (define sanitize1
        (lambda (arg)
         ; attempt to gloss over fixnum-size differences between
         ; 32- and 64-bit versions
          (cond
            [(ftype-pointer? arg) '<ftype-pointer>]
            [(time? arg) '<time>]
            [(date? arg) '<date>]
            [(and (eq? expect 'error)
                  (real? arg)
                  (if (>= arg 0)
                     ; look for numbers around the size in bits or quantity
                     ; of our 30- and 61-bit fixnums, 32 and 64-bit words
                      (or (or (<= 28 arg 33) (<= (expt 2 28) arg (expt 2 33)))
                          (or (<= 59 arg 65) (<= (expt 2 59) arg (expt 2 65))))
                      (or (or (<= -33 arg -28) (<= (- (expt 2 33)) arg (- (expt 2 28))))
                          (or (<= -65 arg -59) (<= (- (expt 2 65)) arg (- (expt 2 59)))))))
             (if (< arg 0) '<-int> '<int>)]
            [else arg])))
      (let ([sc* (simple-conditions c)])
        (cond
          [(find irritants-condition? sc*) =>
           (lambda (ic)
             (let ([ls (condition-irritants ic)])
               (if (list? ls)
                   (apply condition (make-irritants-condition (map sanitize ls)) (remq ic sc*))
                   c)))]
          [else c])))
    (define (condition-message c)
      (define prefix?
        (lambda (x y)
          (let ([n (string-length x)])
            (and (fx<= n (string-length y))
              (let prefix? ([i 0])
                (or (fx= i n)
                    (and (char=? (string-ref x i) (string-ref y i))
                         (prefix? (fx+ i 1)))))))))
      (define prune-prefix
        (lambda (x y)
          (and (prefix? x y)
               (substring y (string-length x) (string-length y)))))
      (let ([s (call-with-string-output-port
                 (lambda (p) (display-condition c p)))])
        (or (prune-prefix "Exception: " s)
            (prune-prefix "Exception in " s)
            (prune-prefix "Warning: " s)
            (prune-prefix "Warning in " s)
            s)))
    (define (condition-type c)
      (case (fxior (if (warning? c) 1 0) (if (error? c) 2 0) (if (violation? c) 4 0))
        [(1) 'warning]
        [else 'error]))
    (let ([blob '(reset . #f)])
      (call/cc
        (lambda (k)
          (parameterize ([reset-handler (lambda () (k blob))])
            (with-exception-handler
              (lambda (c)
                (let ([t (condition-type c)])
                  (when (or (eq? expect 'warning) (not (eq? t 'warning)))
                    (set! blob (cons t (condition-message (sanitize-condition c))))
                    (reset))))
              (lambda ()
                (case (th)
                  [(#t) 'true]
                  [(#f) 'false]
                  [else 'bogus])))))))))

(define mat-error
  (lambda (src message . args)
    (let ([msg (apply format message args)])
     ; strip out newlines so when we grep we get the whole message
      (do ([i 0 (+ i 1)])
          ((= i (string-length msg)))
        (when (char=? (string-ref msg i) #\newline)
          (string-set! msg i #\space)))
      (if src
          (let ()
            (let ([sfd (source-object-sfd src)] [fp (source-object-bfp src)])
              (call-with-values
                (lambda () (#%$locate-source sfd fp #t))
                (case-lambda
                  [() (fprintf (mat-output) "~a at char ~a of ~a~%" msg fp (source-file-descriptor-path sfd))]
                  [(path line char) (fprintf (mat-output) "~a at line ~a, char ~a of ~a~%" msg line char path)]))))
          (fprintf (mat-output) "~a~%" msg))
      (flush-output-port (mat-output)))))

(define ununicode
 ; sanitizer for expected exception messages to make sure we don't end up
 ; with characters in mat error, experr, and report files so these files
 ; don't end up being O/S (locale) dependent
  (lambda (s)
    (let ([ip (open-input-string s)] [op (open-output-string)])
      (let f ()
        (let ([c (read-char ip)])
          (cond
            [(eof-object? c) (get-output-string op)]
            [(fx> (char->integer c) 127) (fprintf op "U+~x" (char->integer c)) (f)]
            [else (write-char c op) (f)]))))))

(define store-coverage
  (lambda (universe-ct ct path)
    (call-with-port
      (open-file-output-port path
        (file-options replace compressed)
        (buffer-mode block)
        (current-transcoder))
      (lambda (op)
        (put-source-table op
          (if (eq? universe-ct ct)
              ct
              (let ([new-ct (make-source-table)])
                (for-each
                  (lambda (p)
                    (let ([src (car p)] [count (cdr p)])
                      (when (source-table-contains? universe-ct src)
                        (source-table-set! new-ct src count))))
                  (source-table-dump ct))
                new-ct)))))))

(define load-coverage
  (lambda (ct)
    (lambda (path)
      (call-with-port
        (open-file-input-port path
          (file-options compressed)
          (buffer-mode block)
          (current-transcoder))
        (lambda (ip) (get-source-table! ip ct +))))))

(set! coverage-table (make-parameter #f))

(set! mat-file
  (lambda (dir)
    (unless (string? dir)
      (errorf 'mat-file "~s is not a string" dir))
    (unless (file-exists? dir) (mkdir dir))
    (lambda (mat)
      (unless (string? mat)
        (errorf 'mat-file "~s is not a string" mat))
      (let ([ifn (format "~a.ms" mat)] [ofn (format "~a.mo" mat)])
        (parameterize ([current-directory dir]
                       [source-directories (cons ".." (source-directories))]
                       [library-directories (cons ".." (library-directories))])
          (printf "matting ~a with output to ~a/~a~%" ifn dir ofn)
          (delete-file ofn #f)
          (parameterize ([mat-output (open-output-file ofn)])
            (dynamic-wind
                (lambda () #f)
                (lambda ()
                  (let ([go (lambda () (mat-load ifn))] [universe-ct (coverage-table)])
                    (if universe-ct
                        (let-values ([(ct . ignore) (with-profile-tracker go)])
                          (store-coverage universe-ct ct (format "~a.covout" mat)))
                        (go))))
                (lambda () (close-output-port (mat-output))))))))))

(set! record-run-coverage
  (lambda (covout th)
    (let ([universe-ct (coverage-table)])
      (if universe-ct
          (let-values ([(ct . ignore) (with-profile-tracker #t th)])
            (store-coverage universe-ct ct covout))
          (th)))))

(set! load-coverage-files
  (lambda path*
    (let ([ct (make-source-table)])
      (for-each (load-coverage ct) path*)
      ct)))

(set! combine-coverage-files
  (lambda (covout covout*)
    (let ([ct (make-source-table)])
      (for-each (load-coverage ct) covout*)
      (store-coverage ct ct covout))))

(set! coverage-percent
  (lambda (covout . covin*)
    (let ([n (source-table-size (load-coverage-files covout))]
          [d (source-table-size (apply load-coverage-files covin*))])
      (printf "~a: covered ~s of ~s source expressions (~s%)\n"
        covout n d (round (/ (* n 100) d))))))

(set! mat-run
  (case-lambda
    [(name)
     (fprintf (mat-output) "Warning: empty mat for ~s.~%" name)]
    [(name . clauses)
     (fprintf (mat-output) "~%Starting mat ~s.~%" name)
     ; release counters for reclaimed code objects between mat groups to reduce gc time
     (when (compile-profile) (profile-release-counters))
     (do ([clauses clauses (cdr clauses)]
          [count 1 (+ count 1)])
       ((null? clauses) 'done)
       (let ([clause (caar clauses)] [source (cadar clauses)])
         (with-exception-handler
           (lambda (c)
             (if (warning? c)
                 (raise-continuable c)
                 (begin
                   (fprintf (mat-output) "Error printing mat clause: ")
                   (display-condition c (mat-output))
                   (reset))))
           (lambda ()
             (pretty-print clause (mat-output))
             (flush-output-port (mat-output))))
         (if (and (list? clause)
                  (= (length clause) 2)
                  (memq (car clause) '(sanitized-error? error? warning?)))
             (let ([expect (case (car clause) [(sanitized-error? error?) 'error] [(warning?) 'warning])])
               (if (and (= (optimize-level) 3) (eq? expect 'error))
                   (fprintf (mat-output) "Ignoring error check at optimization level 3.~%")
                   (let ([ans (mat-one-exp expect (lambda () (eval (cadr clause))) (eq? (car clause) 'sanitized-error?))])
                     (cond
                       [(and (pair? ans) (eq? (car ans) expect))
                        (fprintf (mat-output)
                          "Expected ~s in mat ~s: \"~a\".~%"
                          expect name (ununicode (cdr ans)))]
                       [else
                         (mat-error source "Bug in mat ~s clause ~s" name count)]))))
             (let ([ans (mat-one-exp #f (lambda () (eval clause)) #f)])
               (cond
                 [(pair? ans)
                  (mat-error source
                    "Error in mat ~s clause ~s: \"~a\""
                    name
                    count
                    (cdr ans))]
                 [(eq? ans 'false)
                  (mat-error source
                    "Bug in mat ~s clause ~s"
                    name
                    count)]
                 [(eq? ans 'true) (void)]
                 [else
                  (mat-error source
                    "Bug (nonboolean, nonstring return value) in mat ~s clause ~s"
                    name
                    count)])))))]))

 );let

(define equivalent-expansion?
 ; same modulo renaming of gensyms
 ; procedure in either input is used as predicate for other
  (lambda (x y)
    (let ([alist '()] [oops? #f])
      (or (let e? ([x x] [y y])
            (or (cond
                  [(procedure? x) (x y)]
                  [(procedure? y) (y x)]
                  [(eqv? x y) #t]
                  [(pair? x)
                   (and (pair? y) (e? (car x) (car y)) (e? (cdr x) (cdr y)))]
                  [(or (and (gensym? x) (symbol? y))
                       (and (gensym? y) (symbol? x)))
                   (cond
                     [(assq x alist) => (lambda (a) (eq? y (cdr a)))]
                     [else (set! alist (cons `(,x . ,y) alist)) #t])]
                  [(string? x) (and (string? y) (string=? x y))]
                  [(bytevector? x) (and (bytevector? y) (bytevector=? x y))]
                  [(vector? x)
                   (and (vector? y)
                        (fx= (vector-length x) (vector-length y))
                        (let f ([i (fx- (vector-length x) 1)])
                          (or (fx< i 0)
                              (and (e? (vector-ref x i) (vector-ref y i))
                                   (f (fx1- i))))))]
                  [(fxvector? x)
                   (and (fxvector? y)
                        (fx= (fxvector-length x) (fxvector-length y))
                        (let f ([i (fx- (fxvector-length x) 1)])
                          (or (fx< i 0)
                              (and (fx= (fxvector-ref x i) (fxvector-ref y i))
                                   (f (fx1- i))))))]
                  [(box? x) (and (box? y) (e? (unbox x) (unbox y)))]
                  [else #f])
                (begin
                  (unless oops?
                    (set! oops? #t)
                    (printf "failure in equivalent-expansion?:\n")
                    (pretty-print x)
                    (printf "is not equivalent to\n")
                    (pretty-print y))
                  #f)))
          (begin
            (printf "original expressions:\n")
            (pretty-print x)
            (printf "is not equivalent to\n")
            (pretty-print y)
            #f)))))

(define *fuzz* 1e-14)

(define ~=
   (lambda (x y)
      (or (= x y)
          (and (fl~= (inexact (real-part x))
                     (inexact (real-part y)))
               (fl~= (inexact (imag-part x))
                     (inexact (imag-part y)))))))

(define fl~=
   (lambda (x y)
      (cond
         [(and (fl>= (flabs x) 2.0) (fl>= (flabs y) 2.0))
          (fl~= (fl/ x 2.0) (fl/ y 2.0))]
         [(and (fl< 0.0 (flabs x) 1.0) (fl< 0.0 (flabs y) 1.0))
          (fl~= (fl* x 2.0) (fl* y 2.0))]
         [else (let ([d (flabs (fl- x y))])
                  (or (fl<= d *fuzz*)
                      (begin (printf "fl~~=: ~s~%" d) #f)))])))

(define cfl~=
   (lambda (x y)
      (and (fl~= (cfl-real-part x) (cfl-real-part y))
           (fl~= (cfl-imag-part x) (cfl-imag-part y)))))

; from ieee.ms
(define ==
   (lambda (x y)
      (and (inexact? x)
           (inexact? y)
           (if (flonum? x)
               (and (flonum? y)
                    (if (fl= x y)
                        (fl= (fl/ 1.0 x) (fl/ 1.0 y))
                        (and (not (fl= x x)) (not (fl= y y)))))
               (and (not (flonum? y))
                    (== (real-part x) (real-part y))
                    (== (imag-part x) (imag-part y)))))))

(define (nan) (/ 0.0 0.0))  ; keeps "pretty-equal?" happy
(define pi (* (asin 1.0) 2))
(define +pi   3.14159265358979323846264)
(define +pi/2 1.57079632679489661923132)
(define +pi/4  .78539816339744830961566)
(define -pi (- +pi))
(define -pi/2 (- +pi/2))
(define -pi/4 (- +pi/4))

; smallest ieee flonum
(define +e 4.940656458412465e-324)
(define -e (- +e))

(define patch-exec-path
  (lambda (p)
    (if (windows?)
        (list->string (subst #\\ #\/ (string->list p)))
        p)))

(module separate-eval-tools (separate-eval run-script separate-compile)
  (define (slurp ip)
    (with-output-to-string
      (lambda ()
        (let f ()
          (let ([c (read-char ip)])
            (unless (eof-object? c)
              (write-char c)
              (f)))))))
  (define ($separate-eval who expr*)
    (let-values ([(to-stdin from-stdout from-stderr pid)
                  (open-process-ports (format "~a -q" (patch-exec-path *scheme*))
                    (buffer-mode block)
                    (native-transcoder))])
      (pretty-print `(#%$enable-check-prelex-flags ,(#%$enable-check-prelex-flags)) to-stdin)
      (for-each (lambda (expr) (pretty-print expr to-stdin)) expr*)
      (close-port to-stdin)
      (let* ([stdout-stuff (slurp from-stdout)]
             [stderr-stuff (slurp from-stderr)])
        (when (string=? stderr-stuff "")
          (printf "$separate-eval command succeeded with\nSTDERR:\n~a\nSTDOUT:\n~a\nEND\n" stderr-stuff stdout-stuff))
        (unless (string=? stderr-stuff "")
          (printf "$separate-eval command failed with\nSTDERR:\n~a\nSTDOUT:\n~a\nEND\n" stderr-stuff stdout-stuff)
          (errorf who "~a" stderr-stuff))
        (close-port from-stdout)
        (close-port from-stderr)
        stdout-stuff)))
  (define (separate-eval . expr*) ($separate-eval 'separate-eval expr*))
  (define (run-script script)
    (let-values ([(to-stdin from-stdout from-stderr pid)
                  (open-process-ports
                    (if (windows?)
                        (format "~a --script ~a" (patch-exec-path *scheme*) script)
                        script)
                    (buffer-mode block)
                    (native-transcoder))])
      (close-port to-stdin)
      (let* ([stdout-stuff (slurp from-stdout)]
             [stderr-stuff (slurp from-stderr)])
        (unless (string=? stderr-stuff "")
          (errorf 'run-script "~a" stderr-stuff))
        (close-port from-stdout)
        (close-port from-stderr)
        stdout-stuff)))
  (define separate-compile
    (case-lambda
      [(x) (separate-compile 'compile-file x)]
      [(cf x) ($separate-eval 'separate-compile `((,cf ,(if (symbol? x) (format "testfile-~a" x) x))))])))

(import separate-eval-tools)

#;(collect-request-handler
  (begin
    (warning #f "installing funky collect request-handler")
    (lambda ()
      (collect)
      (when (= (random 100) 17)
        (collect-maximum-generation (+ (random 254) 1))))))

(define windows?
  (if (memq (machine-type) '(i3nt ti3nt a6nt ta6nt))
      (lambda () #t)
      (lambda () #f)))

(define embedded?
  (lambda () #f))

(define ($record->vector x)
  (let* ([rtd (#%$record-type-descriptor x)]
         [n (length (csv7:record-type-field-names rtd))]
         [v (make-vector (fx+ n 1) (record-type-name rtd))])
    (do ([i 0 (fx+ i 1)])
        ((fx= i n))
      (vector-set! v (fx+ i 1) ((csv7:record-field-accessor rtd i) x)))
    v))

(define $cat_flush "./cat_flush")

(define test-cp0-expansion
  (rec test-cp0-expansion
    (case-lambda
      [(expr expected) (test-cp0-expansion equivalent-expansion? expr expected)]
      [(equiv? expr expected)
       (let ([actual (parameterize ([enable-cp0 #t] [#%$suppress-primitive-inlining #f])
                       (expand/optimize `(let () (import scheme) ,expr)))])
         (unless (equiv? actual expected)
           (errorf 'test-cp0-expansion "expected ~s for ~s, got ~s\n" expected expr actual))
         #t)])))

(define rm-rf
  (lambda (path)
    (when (file-exists? path)
      (let f ([path path])
        (if (file-directory? path)
            (begin
              (for-each (lambda (x) (f (format "~a/~a" path x))) (directory-list path))
              (delete-directory path))
            (delete-file path))))))

(define mkfile
  (lambda (filename . expr*)
    (with-output-to-file filename
      (lambda () (for-each pretty-print expr*))
      'replace)))

(define touch
  (lambda (objfn srcfn)
    (let loop ()
      (let ([p (open-file-input/output-port srcfn (file-options no-fail no-truncate))])
        (put-u8 p (lookahead-u8 p))
        (close-port p))
      (when (file-exists? objfn)
        (unless (time>? (file-modification-time srcfn) (file-modification-time objfn))
          (sleep (make-time 'time-duration 1000000 1))
          (loop))))
    #t))

(define preexisting-profile-dump-entry?
  (let ([ht (make-eq-hashtable)])
    (for-each (lambda (x) (eq-hashtable-set! ht (car x) #t)) (profile-dump))
    (lambda (x) (eq-hashtable-contains? ht (car x)))))

(define heap-check-interval (make-parameter 0))

(collect-request-handler
  (let ([counter 0])
    (lambda ()
      (parameterize ([#%$enable-check-heap
                       (let ([interval (heap-check-interval)])
                         (and (not (fx= interval 0))
                              (let ([n (fxmod (fx+ counter 1) interval)])
                                (set! counter n)
                                (fx= n 0))))])
        (collect)))))
