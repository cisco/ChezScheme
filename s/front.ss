;;; front.ss
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

(begin
(define-who make-parameter
  (case-lambda
    [(init guard) (#2%make-parameter init guard)]
    [(v) (#2%make-parameter v)]))

(when-feature pthreads
(let ()
  (define allocate-thread-parameter
    (let ()
      (define free-list '())  ; list of pairs w/ index as car
      (define index-guardian (make-guardian))
      (lambda (initval)
        (with-tc-mutex
          (let ([index
                 (or (index-guardian)
                     (and (not (null? free-list))
                          (let ([index (car free-list)])
                            (set! free-list (cdr free-list))
                            index))
                     (let* ([n (vector-length ($tc-field 'parameters ($tc)))]
                            [m (fx* (fx+ n 1) 2)])
                       (for-each
                         (lambda (thread)
                           (let ([tc ($thread-tc thread)])
                             (let ([old ($tc-field 'parameters tc)]
                                   [new (make-vector m)])
                               (do ([i (fx- n 1) (fx- i 1)])
                                   ((fx< i 0))
                                   (vector-set! new i (vector-ref old i)))
                               ($tc-field 'parameters tc new))))
                         ($thread-list))
                       (set! free-list
                         (do ([i (fx- m 1) (fx- i 1)]
                              [ls free-list (cons (list i) ls)])
                             ((fx= i n) ls)))
                       (list n)))])
            (let loop ()
              (let ([index (index-guardian)])
                (when index
                  (for-each
                    (lambda (thread)
                      (vector-set!
                        ($tc-field 'parameters ($thread-tc thread))
                        (car index)
                        0))
                    ($thread-list))
                  (set! free-list (cons index free-list))
                  (loop))))
            (for-each
              (lambda (thread)
                (vector-set!
                  ($tc-field 'parameters ($thread-tc thread))
                  (car index)
                  initval))
              ($thread-list))
            (index-guardian index)
            index)))))
  (define set-thread-parameter!
    (lambda (index value)
      (with-tc-mutex
        (vector-set! ($tc-field 'parameters ($tc)) (car index) value))))
  (set-who! make-thread-parameter
    (case-lambda
      [(init guard)
       (unless (procedure? guard) ($oops who "~s is not a procedure" guard))
       (let ([index (allocate-thread-parameter (guard init))])
         (case-lambda
           [() (vector-ref ($tc-field 'parameters ($tc)) (car index))]
           [(u) (set-thread-parameter! index (guard u))]))]
      [(init)
       (let ([index (allocate-thread-parameter init)])
         (case-lambda
           [() (vector-ref ($tc-field 'parameters ($tc)) (car index))]
           [(u) (set-thread-parameter! index u)]))]))
  (set! $allocate-thread-parameter allocate-thread-parameter)
  (set! $set-thread-parameter! set-thread-parameter!))
)

(define case-sensitive ($make-thread-parameter #t (lambda (x) (and x #t))))

(define compile-interpret-simple ($make-thread-parameter #t (lambda (x) (and x #t))))

(define generate-interrupt-trap ($make-thread-parameter #t (lambda (x) (and x #t))))

(define generate-allocation-counts ($make-thread-parameter #f (lambda (x) (and x #t))))

(define generate-instruction-counts ($make-thread-parameter #f (lambda (x) (and x #t))))

(define enable-cross-library-optimization ($make-thread-parameter #t (lambda (x) (and x #t))))

(define enable-arithmetic-left-associative ($make-thread-parameter #f (lambda (x) (and x #t))))

(define enable-unsafe-application ($make-thread-parameter #f (lambda (x) (and x #t))))

(define enable-unsafe-variable-reference ($make-thread-parameter #f (lambda (x) (and x #t))))

(define-who current-generate-id
  ($make-thread-parameter
   (lambda (sym)
     (unless (symbol? sym) ($oops 'default-generate-id "~s is not a symbol" sym))
     (gensym (symbol->string sym)))
   (lambda (p)
     (unless (procedure? p) ($oops who "~s is not a procedure" p))
     p)))

(define enable-type-recovery ($make-thread-parameter #t (lambda (x) (and x #t))))

(define enable-error-source-expression ($make-thread-parameter #t (lambda (x) (and x #t))))

(define machine-type
  (lambda ()
    (constant machine-type-name)))

(define-who $fasl-target ($make-thread-parameter #f))

;;; package stubs are defined here in case we exclude certain packages
(eval-when (compile)
(define-syntax package-stub
  (lambda (x)
    (syntax-case x ()
      [(_ name msg)
       (identifier? #'name)
       #'(package-stub (name name) msg)]
      [(_ (name pub-name) msg)
       #'(define name (lambda args ($oops 'pub-name msg)))])))

(define-syntax package-stubs
  (lambda (x)
    (syntax-case x ()
      [(_ pkg name ...)
       (with-syntax ([msg (format "~a package is not loaded" (datum pkg))])
         #'(begin (package-stub name msg) ...))])))
)

(package-stubs cafe
  waiter-prompt-and-read
  waiter-write
  waiter-prompt-string
  new-cafe)
(package-stubs compile
  ($clear-dynamic-closure-counts compile)
  ($c-make-closure compile)
  ($c-make-code compile)
  compile
  ($compile-backend compile)
  compile-file
  ($compile-host-library compile)
  compile-library
  compile-port
  compile-program
  compile-script
  compile-to-file
  compile-to-port
  compile-whole-library
  compile-whole-program
  ($dynamic-closure-counts compile)
  ($lift-closures compile)
  ($loop-unroll-limit compile)
  make-boot-file
  ($make-boot-file make-boot-file)
  make-boot-header
  ($make-boot-header make-boot-header)
  maybe-compile-file
  maybe-compile-library
  maybe-compile-program
  ($np-boot-code compile)
  ($np-compile compile)
  ($np-get-timers compile)
  ($np-last-pass compile)
  ($np-reset-timers! compile)
  ($np-tracer compile)
  ($optimize-closures compile)
  ($track-dynamic-closure-counts compile)
  ($track-static-closure-counts compile))
(package-stubs fasl
  ($fasl-bld-graph fasl-write)
  ($fasl-enter fasl-write)
  ($fasl-start fasl-write)
  ($fasl-table fasl-write)
  ($fasl-out fasl-write)
  ($fasl-wrf-graph fasl-write)
  fasl-write
  fasl-file)
(package-stubs inspect
  inspect
  inspect/object)
(package-stubs interpret
  interpret)
(package-stubs pretty-print
  pretty-format
  pretty-line-length
  pretty-one-line-limit
  pretty-initial-indent
  pretty-standard-indent
  pretty-maximum-lines
  pretty-print
  pretty-file)
(package-stubs profile
  profile-clear
  profile-dump)
(package-stubs sc-expand
  sc-expand
  ($syntax-dispatch sc-expand)
  syntax-error
  literal-identifier=?
  bound-identifier=?
  free-identifier=?
  identifier?
  generate-temporaries
  syntax->datum
  datum->syntax)
(package-stubs trace
  trace-output-port
  trace-print
  ($trace trace)
  ($untrace untrace)
  ($trace-closure trace))
(package-stubs compiler-support
  $cp0
  $cpvalid
  $cptypes
  $cpletrec
  $cpcheck)
(package-stubs syntax-support
  $uncprep)

(define current-eval
  ($make-thread-parameter
    (lambda args ($oops 'eval "no current evaluator"))
    (lambda (x)
       (unless (procedure? x)
          ($oops 'current-eval "~s is not a procedure" x))
       x)))

(define current-expand
  ($make-thread-parameter
    (lambda args ($oops 'expand "no current expander"))
    (lambda (x)
      (unless (procedure? x)
        ($oops 'current-expand "~s is not a procedure" x))
      x)))

(define eval
  (case-lambda
    [(x) ((current-eval) x)]
    [(x env-spec) ((current-eval) x env-spec)]))

(define expand
  (case-lambda
    [(x) ((current-expand) x)]
    [(x env-spec) ((current-expand) x env-spec)]
    [(x env-spec records?) ((current-expand) x env-spec records?)]
    [(x env-spec records? compiling-a-file) ((current-expand) x env-spec records? compiling-a-file)]
    [(x env-spec records? compiling-a-file outfn) ((current-expand) x env-spec records? compiling-a-file outfn)]))

(define-who eval-syntax-expanders-when
   ($make-thread-parameter '(compile load eval)
      (lambda (x)
         (unless (let check ([x x] [l '(compile load eval visit revisit)])
                    (or (null? x)
                        (and (pair? x)
                             (memq (car x) l)
                             (check (cdr x) (remq (car x) l)))))
            ($oops who "invalid eval-when list ~s" x))
         x)))

(define $compiler-is-loaded? #f)
)
