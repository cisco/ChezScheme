#lang racket/base
(require racket/runtime-path
         racket/match
         racket/file
         racket/pretty
         (only-in "r6rs-lang.rkt"
                  optimize-level)
         (only-in "scheme-lang.rkt"
                  current-expand
                  with-source-path)
         (submod "scheme-lang.rkt" callback)
         "syntax-mode.rkt"
         "r6rs-readtable.rkt"
         "scheme-readtable.rkt"
         "parse-makefile.rkt"
         "config.rkt"
         "strip.rkt")

;; Set `SCHEME_SRC` and `MACH` to specify the ChezScheme source
;; directory and the target machine. Set the `MAKE_BOOT_FOR_CROSS`
;; environment variable to generate just enough to run `configure`
;; for a corss build.

(unless scheme-dir
  (error "set `SCHEME_SRC` environment variable"))
(unless target-machine
  (error "set `MACH` environment variable"))

(define dest-dir
  (or (getenv "SCHEME_WORKAREA") scheme-dir))

;; Writes ".boot" and ".h" files to a "boot" subdirectory of
;; `SCHEME_SRC`.

(define-runtime-path here-dir ".")
(define out-subdir (build-path dest-dir "boot" target-machine))
(define nano-dir (build-path scheme-dir "nanopass"))

(define (status msg)
  (printf "~a\n" msg)
  (flush-output))

(define sources-date
  (for/fold ([d 0]) ([dir (in-list (list here-dir
                                         nano-dir
                                         (build-path scheme-dir "s")))])
    (status (format "Use ~a" dir))
    (for/fold ([d d]) ([f (in-list (directory-list dir))]
                       #:when (regexp-match? #rx"[.](?:rkt|ss|sls|def)$" f))
      (max d (file-or-directory-modify-seconds (build-path dir f))))))

(status (format "Check ~a" out-subdir))
(when (for/and ([f (in-list (list "scheme.h"
                                  "equates.h"
                                  "petite.boot"
                                  "scheme.boot"))])
        (define d (file-or-directory-modify-seconds (build-path out-subdir f) #f (lambda () #f)))
        (and d (d . >= . sources-date)))
  (status "Up-to-date")
  (exit))

;; ----------------------------------------

(define-runtime-module-path r6rs-lang-mod "r6rs-lang.rkt")
(define-runtime-module-path scheme-lang-mod "scheme-lang.rkt")

(define-values (petite-sources scheme-sources)
  (get-sources-from-makefile scheme-dir))

(define ns (make-base-empty-namespace))
(namespace-attach-module (current-namespace) r6rs-lang-mod ns)
(namespace-attach-module (current-namespace) scheme-lang-mod ns)

(namespace-require r6rs-lang-mod ns) ; get `library`

;; Change some bindings from imported to top-level references so that
;; expressions are compiled to reference variables that are updated by
;; loading the Chez Scheme compiler. This approach is better than
;; using `namespace-require/copy`, because we want most primitives to
;; be referenced directly to make the compiler run faster.
(define (reset-toplevels [more '()])
  (for-each (lambda (sym)
              (eval `(define ,sym ,sym) ns))
            (append
             more
             '(identifier?
               datum->syntax
               syntax->list
               syntax->datum
               generate-temporaries
               free-identifier=?
               bound-identifier=?
               make-compile-time-value
               current-eval
               eval
               expand
               compile
               error
               format
               make-variable-transformer))))

(reset-toplevels)

(define (load-if-exists/cd path)
  (when (file-exists? path)
    (load/cd path)))

(status "Load nanopass")
(define (load-nanopass)
  (load/cd (build-path nano-dir "nanopass/helpers.ss"))
  (load/cd (build-path nano-dir "nanopass/syntaxconvert.ss"))
  (load-if-exists/cd (build-path nano-dir "nanopass/records.ss"))
  (load-if-exists/cd (build-path nano-dir "nanopass/nano-syntax-dispatch.ss"))
  (load-if-exists/cd (build-path nano-dir "nanopass/parser.ss"))
  (load-if-exists/cd (build-path nano-dir "nanopass/unparser.ss"))
  (load/cd (build-path nano-dir "nanopass/records.ss"))
  (load/cd (build-path nano-dir "nanopass/meta-syntax-dispatch.ss"))
  (load/cd (build-path nano-dir "nanopass/meta-parser.ss"))
  (load/cd (build-path nano-dir "nanopass/pass.ss"))
  (load/cd (build-path nano-dir "nanopass/language-node-counter.ss"))
  (load/cd (build-path nano-dir "nanopass/unparser.ss"))
  (load/cd (build-path nano-dir "nanopass/language-helpers.ss"))
  (load/cd (build-path nano-dir "nanopass/language.ss"))
  (load/cd (build-path nano-dir "nanopass/nano-syntax-dispatch.ss"))
  (load/cd (build-path nano-dir "nanopass/parser.ss"))
  (load/cd (build-path nano-dir "nanopass.ss")))
(parameterize ([current-namespace ns]
               [current-readtable r6rs-readtable])
  (load-if-exists/cd (build-path nano-dir "nanopass/syntactic-property.sls"))
  (load/cd (build-path nano-dir "nanopass/implementation-helpers.ikarus.ss"))
  (load-nanopass))

(namespace-require ''nanopass ns)

(namespace-require scheme-lang-mod ns)

(reset-toplevels '(run-cp0
                   errorf
                   $oops
                   $undefined-violation
                   generate-interrupt-trap))

(namespace-require `(for-syntax ,r6rs-lang-mod) ns)
(namespace-require `(for-syntax ,scheme-lang-mod) ns)
(namespace-require `(for-meta 2 ,r6rs-lang-mod) ns)
(namespace-require `(for-meta 2 ,scheme-lang-mod) ns)

(namespace-require `(only (submod (file ,(path->string (resolved-module-path-name r6rs-lang-mod))) ikarus) with-implicit)
                   ns)

(define show? #f)
(define orig-eval (let ([e (current-eval)])
                    (lambda args
                      (when show? (pretty-write args))
                      (apply e args))))

(define (call-with-expressions path proc)
  (call-with-input-file*
   path
   (lambda (i)
     (let loop ()
       (define e (read i))
       (unless (eof-object? e)
         (proc e)
         (loop))))))

(define (load-ss path)
  (define-values (base name dir) (split-path (path->complete-path path)))
  (parameterize ([current-directory base])
    (call-with-expressions path eval)))

(parameterize ([current-namespace ns]
               [current-readtable scheme-readtable]
               [compile-allow-set!-undefined #t]
               [current-eval (current-eval)])

  (status "Load cmacros parts")
  (call-with-expressions
   (build-path scheme-dir "s/cmacros.ss")
   (lambda (e)
     (define (define-macro? m)
       (memq m '(define-syntactic-monad define-flags set-flags)))
     (define (define-for-syntax? m)
       (memq m '(lookup-constant flag->mask)))
     (match e
       [`(define-syntax ,m . ,_)
        (when (define-macro? m)
          (orig-eval e))]
       [`(eval-when ,_ (define ,m . ,rhs))
        (when (define-for-syntax? m)
          (orig-eval `(begin-for-syntax (define ,m . ,rhs))))]
       [`(define-flags . ,_)
        (orig-eval e)]
       [_ (void)])))

  (set-current-expand-set-callback!
   (lambda ()
     (start-fully-unwrapping-syntax!)
     (define $uncprep (orig-eval '$uncprep))
     (current-eval
      (lambda (stx)
        (syntax-case stx ()
          [("noexpand" form)
           (orig-eval (strip-$app (strip-$primitive ($uncprep (syntax-e #'form)))))]
          [_
           (orig-eval stx)])))
     (call-with-expressions
      (build-path scheme-dir "s/syntax.ss")
      (lambda (e)
        (let loop ([e e])
          (cond
            [(and (pair? e)
                  (eq? 'define-syntax (car e)))
             ((current-expand) `(define-syntax ,(cadr e)
                                  ',(orig-eval (caddr e))))]
            [(and (pair? e)
                  (eq? 'begin (car e)))
             (for-each loop  (cdr e))]))))
     (status "Install evaluator")
     (current-eval
      (let ([e (current-eval)])
        (lambda (stx)
          (define (go ex)
            (define r (strip-$app
                       (strip-$primitive
                        (if (struct? ex)
                            ($uncprep ex)
                            ex))))
            (e r))
          (let loop ([stx stx])
            (syntax-case* stx (#%top-interaction
                               eval-when compile
                               constant-case architecture else
                               begin
                               include) (lambda (a b)
                                          (eq? (syntax-e a) (syntax-e b)))
              [(#%top-interaction . rest) (loop #'rest)]
              [(eval-when (compile) . rest)
               #'(eval-when (compile eval load) . rest)]
              [(begin e ...)
               (for-each loop (syntax->list #'(e ...)))]
              [(include fn)
               (loop
                #`(begin #,@(with-source-path 'include (syntax->datum #'fn)
                              (lambda (n)
                                (call-with-input-file*
                                 n
                                 (lambda (i)
                                   (let loop ()
                                     (define r (read-syntax n i))
                                     (if (eof-object? r)
                                         '()
                                         (cons r (loop))))))))))]
              [(constant-case architecture [else e ...])
               (loop #`(begin e ...))]
              [(constant-case architecture [(arch ...) e ...] . _)
               (memq (string->symbol target-machine) (syntax->datum #'(arch ...)))
               (loop #`(begin e ...))]
              [(constant-case architecture _ . clauses)
               (loop #`(constant-case architecture . clauses))]
              [_ (go ((current-expand) (syntax->datum stx)))])))))
     (status "Load cmacros using expander")
     (load-ss (build-path scheme-dir "s/cmacros.ss"))
     (status "Continue loading expander")))

  (status "Load enum")
  (load-ss (build-path scheme-dir "s/enum.ss"))
  (eval '(define $annotation-options (make-enumeration '(debug profile))))
  (eval '(define $make-annotation-options (enum-set-constructor $annotation-options)))
  (eval
   '(define-syntax-rule (library-requirements-options id ...)
      (with-syntax ([members ($enum-set-members ($make-library-requirements-options (datum (id ...))))])
        #'($record (record-rtd $library-requirements-options) members))))

  (status "Load cprep")
  (load-ss (build-path scheme-dir "s/cprep.ss"))

  (status "Load expander")
  (load-ss (build-path scheme-dir "s/syntax.ss"))

  (status "Initialize system libraries")
  (define (init-libraries)
    (eval '($make-base-modules))
    (eval '($make-rnrs-libraries))
    (eval '(library-search-handler (lambda args (values #f #f #f))))
    (eval '(define-syntax guard
             (syntax-rules (else)
               [(_ (var clause ... [else e1 e2 ...]) b1 b2 ...)
                ($guard #f (lambda (var) (cond clause ... [else e1 e2 ...]))
                        (lambda () b1 b2 ...))]
               [(_ (var clause1 clause2 ...) b1 b2 ...)
                ($guard #t (lambda (var p) (cond clause1 clause2 ... [else (p)]))
                        (lambda () b1 b2 ...))]))))
  (init-libraries)
  
  (status "Load nanopass using expander")
  (load-ss (build-path nano-dir "nanopass/implementation-helpers.chezscheme.sls"))
  (load-nanopass)

  (status "Load priminfo and primvars")
  (load-ss (build-path scheme-dir "s/priminfo.ss"))
  (load-ss (build-path scheme-dir "s/primvars.ss"))

  (status "Load expander using expander")
  (set-current-expand-set-callback! void)
  (load-ss (build-path scheme-dir "s/syntax.ss"))

  (status "Initialize system libraries in bootstrapped expander")
  (init-libraries)
  
  (status "Declare nanopass in bootstrapped expander")
  (load-ss (build-path nano-dir "nanopass/implementation-helpers.chezscheme.sls"))
  (load-nanopass)

  (status "Load some io.ss declarations")
  (call-with-expressions
   (build-path scheme-dir "s/io.ss")
   (lambda (e)
     (define (want-syntax? id)
       (memq id '(file-options-list eol-style-list error-handling-mode-list)))
     (define (want-val? id)
       (memq id '($file-options $make-file-options $eol-style? buffer-mode? $error-handling-mode?)))
     (let loop ([e e])
       (match e
         [`(let () ,es ...)
          (for-each loop es)]
         [`(begin ,es ...)
          (for-each loop es)]
         [`(define-syntax ,id . ,_)
          (when (want-syntax? id)
            (eval e))]
         [`(set-who! ,id . ,_)
          (when (want-val? id)
            (eval e))]
         [_ (void)]))))

  (status "Load some strip.ss declarations")
  (call-with-expressions
   (build-path scheme-dir "s/strip.ss")
   (lambda (e)
     (let loop ([e e])
       (match e
         [`(let () ,es ...)
          (for-each loop es)]
         [`(begin ,es ...)
          (for-each loop es)]
         [`(set-who! $fasl-strip-options . ,_)
          (eval e)]
         [`(set-who! $make-fasl-strip-options . ,_)
          (eval e)]
         [_ (void)]))))

  (status "Load some 7.ss declarations")
  (call-with-expressions
   (build-path scheme-dir "s/7.ss")
   (lambda (e)
     (let loop ([e e])
       (match e
         [`(let () ,es ...)
          (for-each loop es)]
         [`(begin ,es ...)
          (for-each loop es)]
         [`(define $format-scheme-version . ,_)
          (eval e)]
         [`(define ($compiled-file-header? . ,_) . ,_)
          (eval e)]
         [_ (void)]))))

  (status "Load most front.ss declarations")
  (call-with-expressions
   (build-path scheme-dir "s/front.ss")
   (lambda (e)<
     ;; Skip `package-stubs`, which would undo "syntax.ss" definitions
     (let loop ([e e])
       (match e
         [`(package-stubs . ,_) (void)]
         [`(define-who make-parameter . ,_) (void)]
         [`(begin . ,es) (for-each loop es)]
         [_ (eval e)]))))
  ((orig-eval 'current-eval) eval)
  ((orig-eval 'current-expand) (current-expand))
  ((orig-eval 'enable-type-recovery) #f)
  
  (status "Define $filter-foreign-type")
  (eval `(define $filter-foreign-type
           (lambda (ty)
             (filter-foreign-type ty))))

  (make-directory* out-subdir)
  
  (status "Load mkheader")
  (load-ss (build-path scheme-dir "s/mkheader.ss"))
  (status "Generate headers")
  (eval `(mkscheme.h ,(path->string (build-path out-subdir "scheme.h")) ,target-machine))
  (eval `(mkequates.h ,(path->string (build-path out-subdir "equates.h"))))
  (plumber-flush-all (current-plumber))

  (let ([mkgc.ss (build-path scheme-dir "s/mkgc.ss")])
    (when (file-exists? mkgc.ss)
      (status "Load mkgc")
      (load-ss (build-path scheme-dir "s/mkgc.ss"))
      (status "Generate GC")
      (eval `(mkgc-ocd.inc ,(path->string (build-path out-subdir "gc-ocd.inc"))))
      (eval `(mkgc-oce.inc ,(path->string (build-path out-subdir "gc-oce.inc"))))
      (eval `(mkgc-par.inc ,(path->string (build-path out-subdir "gc-par.inc"))))
      (eval `(mkheapcheck.inc ,(path->string (build-path out-subdir "heapcheck.inc"))))
      (plumber-flush-all (current-plumber))))

  (when (getenv "MAKE_BOOT_FOR_CROSS")
    ;; Working bootfiles are not needed for a cross build (only the
    ;; ".h" files are needed), so just touch dummy files in that case
    ;; to let `configure` work and to communicate xpatch rebuild
    (define (touch p)
      (call-with-output-file* p void #:exists 'truncate))
    (touch (build-path out-subdir "petite.boot"))
    (touch (build-path out-subdir "scheme.boot"))
    (exit))

  (for ([s (in-list '("ftype.ss"
                      "fasl.ss"
                      "reloc.ss"
                      "format.ss"
                      "cp0.ss"
                      "cpvalid.ss"
                      "cpcheck.ss"
                      "cpletrec.ss"
                      "cpcommonize.ss"
                      "cpnanopass.ss"
                      "compile.ss"
                      "back.ss"))])
    (status (format "Load ~a" s))
    (load-ss (build-path scheme-dir "s" s)))

  ((orig-eval 'fasl-compressed) #f)

  (define all-sources (append petite-sources scheme-sources))
  (define (source->so src #:abs? [abs? #t])
    (path->string ((if abs? path->complete-path values) (build-path out-subdir (path-replace-suffix src #".so")))))

  (let ([failed? #f])
    (for ([src (in-list all-sources)])
      (let ([dest (source->so src)])
        (parameterize ([current-directory (build-path scheme-dir "s")])
          ;; (status (format "Compile ~a" src)) - Chez Scheme prints its own message
          (with-handlers (#;[exn:fail? (lambda (exn)
                                       (eprintf "ERROR: ~s\n" (exn-message exn))
                                       (set! failed? #t))])
            (time ((orig-eval 'compile-file) src dest))))))
    (when failed?
      (raise-user-error 'make-boot "compilation failure(s)")))

  (let ([src->so (lambda (src) (source->so #:abs? #f src))])
    (status (format "Writing ~a/petite.boot" target-machine))
    (eval `($make-boot-file ,(path->string (build-path out-subdir "petite.boot"))
                            ',(string->symbol target-machine) '()
                            ,@(map src->so petite-sources)))
    (status (format "Writing ~a/scheme.boot" target-machine))
    (eval `($make-boot-file ,(path->string (build-path out-subdir "scheme.boot"))
                            ',(string->symbol target-machine) '("petite")
                            ,@(map src->so scheme-sources))))

  ;; Clean up
  (for ([src (in-list all-sources)])
    (define so (source->so src))
    (when (file-exists? so)
      (delete-file so))))
