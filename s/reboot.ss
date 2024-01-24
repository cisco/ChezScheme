;; This script automates bootstrapping of the latest implementation of
;; Chez Scheme from a substantially incompatible version --- for
;; example, one with different a different base-lanuage AST, a
;; different representation of syntax objects, a different macro
;; expander, or a different records implementation.
;;
;; The script is specific to the implementation being bootstapped, and
;; it relies on details of the macro expander, which cooperates
;; slightly with boostrapping. So, for example, the script will need
;; updates if the compiler starts using new primitives at compile
;; time. The host Scheme used to bootstrap needs to be new enough to
;; load the current nanopass implementation and to define any macro
;; used in "cmacros.ss" or "syntax.ss"; if something is missing from
;; the host Scheme, then hopefully you can define it here (similar to
;; temporarily adding to "patch.ss").
;;
;; The overall build sequence is based on the one for a normal cross
;; compile: compile a patch that runs on the current system, then use
;; that to compile boot files for the target system. The extra steps
;; here are to first define a replacement layer for records, then load
;; the new macro expander; that expander with the replacement record
;; layer is used to "compile" a patch for the current system, where
;; "compile" is just loading into the current system.

;; Normally, this script is rnu through the `re.boot` makefile target
;; or the `reboot.zuo` script. To run directly:
;;
;;   - create a directory that has a suitable "machine.def" to the
;;     target machine, and
;;
;;   - create a directory that has a suitable "machine.def" for the
;;     host machine, and
;;
;;   - run this script as `scheme --script reboot.ss <target-dir>
;;     <host-dir>` in the Chez Scheme source directory with each given
;;     directory as one containing a "machine.def".
;;
;; Output is written to "boot/<machine>", where "<machine>" is
;; determined by the "<target-dir>/machine.def" file. You can supply a
;; third argument to the script to select a different desination.

;; Implementation:
;;
;; Instead of running Chez Scheme multiple times in new processes, we
;; use a single process and keep replacing bindings in the top-level
;; environment. After setting up some replacement "primitives", we
;; redefine `$primitive` so that it no longer necessarily gets the
;; primitive versions of procedures. Instead, `$primitive` is
;; redefined to look in the top-level environment. This redirect for
;; primitives only needs to work well enough to run the expander and
;; compiler.
;;
;; Since `$primitive` no longer accesses primitives, we need to define
;; here any `$`-prefixed name that is used by the expander and
;; compiler. Mostly, we just define them to be the primitive functions
;; (before `$primitive` is redefined) after all, but they can instead
;; be implemented using other functions.
;;
;; In case there are reader changes, then those have to replicated
;; here in some form, too. the current setup involves adjusting the
;; input-file characters, then rewriting the result of `read`.
;;
;; The trickiest part is loading the macro expander, which defines the
;; layout of syntax objects, but also has literal syntax objects in
;; its implementation. Both the expander and predefined macros are
;; defined using syntax objects and many of the same macros that will
;; be defined. So, it takes a few passes:
;;
;;   - Start by loading "cmacros.ss" and similar so that data about
;;     the target platform, etc., is available. Currently, we assume
;;     that the host Scheme can run "mkheader.ss" and similar, so
;;     we run that first to get ".h" and ".inc" files generated.
;;
;;   - The expander needs nanopass, so load that. The host Scheme
;;     needs to be new enough to run the current nanopass.
;;
;;   - Next, load just the expander implementation, which is defined
;;     as the first big S-expression in "syntax.ss".
;;
;;   - For each subsequent term in "syntax.ss", expand the expression
;;     part with the host Scheme's macro expander. Then, rewrite
;;     syntax-object literals in the expansion into new-expander
;;     literals, using the `$datum->environment-syntax` procedure
;;     exported by the expander for this purpose. Finally, send the
;;     definition through the just-loaded new expander, which
;;     registers the definitions in its system environment.
;;
;;     At this point, we now have the expander and predefined macros
;;     all working with the new expander's representation for syntax
;;     objects and base-language AST. This expander is *not* wired
;;     into `eval` or installed to the real `current-expand` (but it
;;     is installed to a new, user-level `current-expand`). Instead,
;;     the expander must be called directly as `sc-expand`.
;;
;;     When `sc-expand` is running, it may need to evalute via `eval`.
;;     The new expander's AST form is converted by to an S-expression
;;     using the new expander's `$uncprep`. Syntax-object literals are
;;     not converted back, however; the evaluated/compiled code should
;;     operate on new-expander syntax objects.
;;
;;   - Take it from the top by loading "cmacro.ss", etc., using the
;;     new expander, which defines macros to work with the new
;;     expander.
;;
;;   - Load nanopass again, too. The nanopass implementation shouldn't
;;     be any different this second time around, but now it's defined
;;     and registered in the new expander's table of modules.
;;
;;   - Compile the compiler. The compiler implementation is a
;;     hand-crafted list of files that cover everything needed to run
;;     `compile-file` and `$make-boot-file`. Note that this includes
;;     the expander, but this time configured for the target platform
;;     instead of the host platform.
;;
;;     "Compile" just means to expand the compiler's files. Loading
;;     (expansion + eval) in one pass wouldn't work, because that
;;     would redefine some macros that are used in the implementation,
;;     and it would also load a new expander half-way through!
;;
;;   - Initialize the new copy of the macro expander, which is
;;     configured for the target. That include reloading "cmacros.ss"
;;     to get its macros, as well as reloading nanopass. The reloaded
;;     nanopass needs to be the previously saved version, though as
;;     "compiled" for the host platform. And then load the "compiled"
;;     compiler, which includes defining additional macros.
;;
;;   - Set the user-level `current-expand` to the new expander, so the
;;     just-loaded `compile-file` will reach it via a new user-level
;;     `expand`. While the new expander is running via
;;     `current-expand`, set the real `current-expand` to perform the
;;     same dance as before to handle the times when the expander
;;     calls `eval`.
;;
;;   - Run `compile-file` on all of the sources. Run `$make-boot-file`
;;     to create "petite.boot" and "scheme.boot".

(define-values (xc-dir host-dir base-out-dir)
  (let ([l (command-line-arguments)])
    (if (<= 2 (length l) 3)
        (values (car l) (cadr l) (if (= 2 (length l)) "." (caddr l)))
        (error 'reboot "expected <xc-dir> <host-dir> [<out-dir>]"))))

(meta-cond
 [(top-level-bound? 'path-build) (begin)]
 [else
  (define path-build
    (lambda (a b)
      (let ([sep (if (eqv? (string-ref a (sub1 (string-length a))) #\/) "" "/")])
        (string-append a sep b))))])

(let ([machine.def (path-build xc-dir "machine.def")])
  (unless (file-exists? machine.def)
    (error 'reboot "~a not found" machine.def)))

(define (select-config config-dir)
  (source-directories (list config-dir "s" "unicode")))
(select-config xc-dir)
(library-directories '(("." . ".") ("nanopass" . "nanopass")))

(define (status s)
  (printf "~a\n" s)
  (flush-output-port))

;; Read "s/build.zuo" to get the set of sources for "petite.boot"
;; and "scheme.boot", so we don't have a separate copy here.
(define-values (patch-srcs base-srcs compiler-srcs)
  (call-with-input-file
   "s/build.zuo"
   (lambda (i)
     (unless (equal? (get-line i) "#lang zuo")
       (error 'srcs "expected `#lang zuo`from build.zuo"))
     (let ([content (let loop ()
                      (let ([v (read i)])
                        (if (eof-object? v)
                            '()
                            (cons v (loop)))))])
       (define (extract-list id)
         (let loop ([c content])
           (cond
             [(and (list? c)
                   (= 3 (length c))
                   (eq? (car c) 'define)
                   (eq? (cadr c) id))
              (let ([v (caddr c)])
                (cond
                  [(and (list? v) (eq? 'list (car v)))
                   (cdr v)]
                  [else (error "definition did not have the expected right-hand side" v)]))]
             [(list? c)
              (ormap loop c)]
             [else #f])))
       (values (fold-left (lambda (srcs src)
                            (remove src srcs))
                          (map (lambda (s) (string-append (path-root s) ".ss"))
                               (extract-list 'patch-names))
                          ;; not for this cross-compile mode:
                          '("read.ss" "interpret.ss" "cptypes.ss" "ubify.ss" "patch.ss"))
               (extract-list 'base-src-names)
               (extract-list 'compiler-names))))))

;; In case of debugging printfs:
(print-graph #t)

;; We need to keep track of all the user-level "primitives" that we
;; define, so we can carry them over to a new namespace that is
;; created by the new expander.
(define primitive-environment (interaction-environment))
(define primitives '())
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (id . args) . body)
     (begin
       (set! primitives (cons 'id primitives))
       (define (id . args) . body))]
    [(_ id rhs)
     (begin
       (set! primitives (cons 'id primitives))
       (define id rhs))]))

;; Recognize host syntax objects:
(define syntax-object? (record-predicate (record-rtd (syntax x))))

(define orig-syntax->datum syntax->datum)
(define orig-identifier? identifier?)
(define orig-free-identifier=? free-identifier=?)
(define orig-datum->syntax datum->syntax)
(define syntax->datum #%syntax->datum) ; to be replaced by expander

(define orig-make-compile-time-value make-compile-time-value)

;; Start defining "primitives" here vv ----------------------------------------

(define (make-$sputprop meta-key)
  (lambda (sym key val)
    (putprop sym meta-key (cons (cons key val) (getprop sym meta-key '())))))

(define (make-$sgetprop meta-key)
  (lambda (sym key def-val)
    (let ([a (assq key (getprop sym meta-key '()))])
      (if a
          (cdr a)
          def-val))))

(define-primitive (make-$sremprop meta-key)
  (lambda (sym key)
    (let ([a (assq key (getprop sym meta-key '()))])
      (when a
        (putprop sym meta-key (filter
                               (lambda (p)
                                 (not (eq? (car p) key)))
                               (getprop sym meta-key '())))))))

(define-primitive $sputprop (make-$sputprop 'reboot-host))
(define-primitive $sgetprop (make-$sgetprop 'reboot-host))
(define-primitive $sremprop (make-$sremprop 'reboot-host))

;; We'll use these when we're ready to compile/expand for the client,
;; and that way information about primitives for host and client are
;; kept separate
(define client-$sputprop (make-$sputprop 'reboot-client))
(define client-$sgetprop (make-$sgetprop 'reboot-client))
(define client-$sremprop (make-$sremprop 'reboot-client))

(define-primitive ($intern3 s x y)
  (#%$intern3 s x y))

(define-primitive ($undefined-violation id msg)
  (error (syntax->datum id) msg))

(define-primitive $exactnum-real-part real-part)
(define-primitive $exactnum-imag-part imag-part)
(define-primitive $ratio-numerator numerator)
(define-primitive $ratio-denominator denominator)

(define-primitive ($inexactnum? x)
  (and (complex? x)
       (not (real? x))
       (inexact? x)))

(define-primitive ($exactnum? x)
  (and (complex? x)
       (not (real? x))
       (exact? x)))

(define-primitive $enum-set-members #%$enum-set-members)
(define-primitive $make-file-options #%$make-file-options)
(define-primitive $file-options #%$file-options)
(define-primitive $eol-style? #%$eol-style?)
(define-primitive $error-handling-mode? #%$error-handling-mode?)
(define-primitive $open-file-output-port #%$open-file-output-port)
(define-primitive $open-file-input-port #%$open-file-input-port)
(define-primitive $open-bytevector-list-output-port #%$open-bytevector-list-output-port)
(define-primitive $format-scheme-version #%$format-scheme-version)
(define-primitive $fasl-strip-options #%$fasl-strip-options)
(define-primitive $make-fasl-strip-options #%$make-fasl-strip-options)
(define-primitive $port-flags-set? #%$port-flags-set?)

(define-primitive symbol? (lambda (x) (and (#%symbol? x)
                                           (not (eq? x $the-unbound-object)))))
(define-primitive gensym? (lambda (x) (and (symbol? x) (#%gensym? x))))
(define-primitive uninterned-symbol? (lambda (x) #f)) ; assuming not used in compiler

(define-primitive $immediate? (lambda (x)
                                (or (#%$immediate? x)
                                    (eq? x $the-unbound-object))))
(define-primitive $flonum->digits #%$flonum->digits)
(define-primitive $flonum-sign #%$flonum-sign)
(define-primitive $integer-32? #%$integer-32?)
(define-primitive $integer-64? #%$integer-64?)
(define-primitive $fxu< #%$fxu<)
(define-primitive $stencil-vector? (lambda (v) #f))
(define-primitive $system-stencil-vector? (lambda (v) #f))
(define-primitive $symbol-name #%$symbol-name)
(define-primitive immutable-vector (lambda args (vector->immutable-vector (apply vector args))))

(define-primitive ($char-grapheme-other-state) 1) ; probably correct, shouldn't matter for compiler

(define-primitive $ht-minlen #%$ht-minlen)
(define-primitive $ht-veclen #%$ht-veclen)

(define-primitive $rtd-counts? #%$rtd-counts?)

(define (fxwraparound v)
  (cond
    [(fixnum? v)
     v]
    [(zero? (bitwise-and v (add1 (most-positive-fixnum))))
     (bitwise-ior v (- -1 (most-positive-fixnum)))]
    [else
     (bitwise-and v (most-positive-fixnum))]))

(define-primitive (fx+/wraparound a b) (fxwraparound (+ a b)))
(define-primitive (fx-/wraparound a b) (fxwraparound (- a b)))
(define-primitive (fx*/wraparound a b) (fxwraparound (* a b)))
(define-primitive (fxsll/wraparound a b) (fxwraparound (bitwise-arithmetic-shift-left a b)))

(define-primitive (fxpopcount32 x)
  (let* ([x (- x (bitwise-and (arithmetic-shift x -1) #x55555555))]
         [x (+ (bitwise-and x #x33333333) (bitwise-and (arithmetic-shift x -2) #x33333333))]
         [x (bitwise-and (+ x (arithmetic-shift x -4)) #x0f0f0f0f)]
         [x (+ x (arithmetic-shift x -8) (arithmetic-shift x -16) (arithmetic-shift x -24))])
    (bitwise-and x #x3f)))

(define-primitive (fxpopcount x)
  (fx+ (fxpopcount32 (bitwise-and x #xffffffff))
       (fxpopcount32 (arithmetic-shift x -32))))

(define-primitive (fxpopcount16 x)
  (fxpopcount32 (bitwise-and x #xffff)))

(meta-cond
 [(#%$top-level-bound? 'flvector?)
  (define need-vector-filter? #f)
  (define $the-empty-flvector (make-flvector 0))]
 [else
  (define need-vector-filter? #t)
  (define $the-empty-flvector (fxvector 1 2 3 4 5)) ; something self-quoting
  (define-primitive fxvector? (lambda (x) (and (#%fxvector? x)
                                               (not (eq? x $the-empty-flvector)))))
  (define-primitive (flvector) $the-empty-flvector)
  (define-primitive (flvector? v) (eq? v $the-empty-flvector))
  (define-primitive (flvector-length v) 0)
  (define-primitive (flvector-ref v i) (error 'flvector-ref "not possible"))
  ;; used for `read` fixup, needs to use host implementation of records:
  (define-record-type a-flvector
    (fields)
    (nongenerative #{flvector ogen5za1gzb3zzlvv2d2jtm6w-0}))])

(meta-cond
 [#t
  ;; The general approach, which is to implement records ourselves:
  (include "reboot-record-wrap.ss")]
 [else
  ;; If records in the host Scheme have the same representation as the target, we can
  ;; use the host Scheme's implementation of records, and things are about twice as fast:
  (define-primitive ($make-record-type base-rtd parent name fields sealed? opaque? . extras)
    (apply #%$make-record-type base-rtd parent name fields sealed? opaque? extras))
  (define-primitive ($make-record-type-descriptor base-rtd parent name uid sealed? opaque? fields . extras)
    (apply #%$make-record-type-descriptor base-rtd parent name uid sealed? opaque? fields extras))
  (define-primitive ($make-record-constructor-descriptor rts parent protocol name)
    (#%$make-record-constructor-descriptor rts parent protocol name))
  (define-primitive $record #%$record)
  (define-primitive $record? #%$record?)
  (define-primitive $record-type-descriptor #%$record-type-descriptor)
  (define-primitive $make-record-constructor-descriptor #%$make-record-constructor-descriptor)
  (define-primitive $record-type-field-indices #%$record-type-field-indices)
  (define-primitive $object-ref #%$object-ref)
  (define-primitive $sealed-record? #%$sealed-record?)
  (define-primitive $remake-rtd (lambda (rtd compute-field-offsets)
                                  (parameterize ([#%$target-machine ($target-machine)])
                                    (#%$remake-rtd rtd compute-field-offsets))))])

(define-primitive $thread-list #%$thread-list)

(define-primitive $c-bufsiz #%$c-bufsiz)

(define-primitive $separator-character (meta-cond
                                        [(#%$top-level-bound? '$separator-character) #%$separator-character]
                                        [else
                                         (case (machine-type)
                                           [(i3nt a6nt arm64nt ti3nt ta6nt tarm64nt) #\\]
                                           [else #\/])]))

(define-primitive $expand-fp-ftype (lambda (who what r ftype)
                                     (#%$expand-fp-ftype who what r (syntax->datum ftype))))
(define-primitive $ftd? #%$ftd?)
(define-primitive $ftd-as-box? #%$ftd-as-box?)
(define-primitive $filter-foreign-type #%$filter-foreign-type)

(define-primitive $make-fmt->expr #%$make-fmt->expr)
(define-primitive $parse-format-string (lambda args #f))

(define-primitive $set-collect-trip-bytes #%$set-collect-trip-bytes)

;; Parameters added since the oldest host verson that we want to support:
(define-primitive enable-unsafe-application (make-parameter #f))
(define-primitive enable-unsafe-variable-reference (make-parameter #f))
(define-primitive current-generate-id (make-parameter (lambda (sym) (gensym (symbol->string sym)))))
(define-primitive enable-type-recovery (make-parameter #f))
(define-primitive enable-error-source-expression (make-parameter #f))
(define-primitive compile-procedure-realm (make-parameter #f))
(define-primitive compile-omit-concatenate-support (make-parameter #f))
(define-primitive enable-arithmetic-left-associative (make-parameter #f))
(define-primitive self-evaluating-vectors (make-parameter #f))

(define-syntax $lambda/lift-barrier
  (syntax-rules ()
    [(_ fmls body ...) (lambda fmls body ...)]))
(define-syntax $begin-unsafe
  (syntax-rules ()
    [(_ body0 body ...) (begin body0 body ...)]))

(define-primitive $fasl-target (make-parameter #f))
(define-primitive $current-mso (make-parameter #f))
(define-primitive $block-counter (make-parameter 0))
(define-primitive $sfd (make-parameter #f))
(define-primitive $target-machine (make-parameter #f))
(define-primitive $compile-profile (make-parameter #f))
(define-primitive $optimize-closures (make-parameter #t))
(define-primitive $track-dynamic-closure-counts (make-parameter #f))

(define-primitive $the-unbound-object (gensym "unbound"))
(define-primitive ($unbound-object) $the-unbound-object)
(define-primitive ($unbound-object? v)
  (eq? v $the-unbound-object))

(define primitive-substs (make-eq-hashtable))

(define-primitive $top-level-value
  (let ([orig-top-level-bound? top-level-bound?]
        [orig-top-level-value top-level-value])
    (lambda (s)
      (let ([s (hashtable-ref primitive-substs s s)])
        (cond
          [(orig-top-level-bound? s primitive-environment)
           (orig-top-level-value s primitive-environment)]
          [(gensym? s) (#%$top-level-value s)]
          [else
           (unless (eq? s '$capture-fasl-target)
             (printf "  [unbound: ~s]\n" s))
           ($unbound-object)])))))
(define-primitive $set-top-level-value!
  (let ([top-level-bound? top-level-bound?]
        [set-top-level-value! set-top-level-value!]
        [define-top-level-value define-top-level-value])
    (lambda (sym val)
      (let ([sym (hashtable-ref primitive-substs sym sym)])
        (if (top-level-bound? sym  primitive-environment)
            (set-top-level-value! sym val  primitive-environment)
            (define-top-level-value sym val primitive-environment))))))

(define $tc-mutex (and (threaded?) (make-mutex)))

(define tc-table (make-eq-hashtable))
(define ($tc) tc-table)
(define $tc-field
  (case-lambda
   [(sym table) (hashtable-ref table sym 0)]
   [(sym table val) (hashtable-set! table sym val)]))

(define-primitive ($profile-source-data?) #f)
(define-primitive ($profile-block-data?) #f)
(define-primitive ($suppress-primitive-inlining) #f)

(define-primitive $reset-protect #%$reset-protect)
(define-primitive $pass-time
  (lambda (name thunk)
    (thunk)))
(define-primitive $guard #%$guard)

(define-primitive $invoke-library #%$invoke-library)

(define orig-$uncprep #%$uncprep)

(define-primitive $primitive-value
  (let ([orig-top-level-bound? top-level-bound?]
        [orig-top-level-value top-level-value])
    (lambda (s)
      (if (orig-top-level-bound? s primitive-environment)
          (orig-top-level-value s primitive-environment)
          (begin
            (printf "UNBOUND PRIMITIVE ~s\n" s)
            ($unbound-object))))))

;; Make `$primitive` access top-level variables:
(define-syntax $primitive
  (let ([orig-top-level-bound? top-level-bound?])
    (lambda (stx)
      (define (top id)
        (let* ([sym (orig-syntax->datum id)]
               [sym (hashtable-ref primitive-substs sym sym)])
          (if (orig-top-level-bound? sym primitive-environment)
              ;; This works as long as primitives are never locally shadowed,
              ;; (which won't be the case for expanded code, at least):
              (datum->syntax id sym)
              ;; If it's not yet there, defer the lookup, and maybe we
              ;; won't have to fill in the primitive:
              #`($primitive-value (quote #,(datum->syntax id sym))))))
      (syntax-case stx ()
        [(_ id) (top #'id)]
        [(_ level id) (top #'id)]))))

(define-syntax ($foreign-procedure stx)
  (syntax-case stx ()
    [(_ _ name . _)
     #'(lambda args (error 'reboot "expander not expected to call foreign procedure ~s" name))]))

(define-primitive ($oops . args)
  (apply error args))

(define-primitive ($make-source-oops who . args)
  (($top-level-value 'datum->syntax) (or who ($make-interaction-syntax 'unknown))
                                     `(error 'source "oops ~s" '(,who . ,args))))

(define-primitive ($source-warning . args)
  (printf "~s\n" args))

(define-primitive ($open-file-input-port who fn)
  (open-file-input-port fn))

(define-primitive ($source-file-descriptor fn p)
  fn)

(define-primitive ($make-read p . more)
  (let ([l (filtered-file->exps p)])
    (lambda ()
      (if (null? l)
          #!eof
          (let ([a (car l)])
            (set! l (cdr l))
            a)))))

(define-primitive ($map who f . ls)
  (apply map f ls))

(define-primitive subset-mode
  (case-lambda
   [(mode) (unless (eq? mode 'system) (error 'subset-mode "always must be system mode"))]
   [() 'system]))

(define $current-expand current-expand)
(define current-expand (make-parameter #f))

;; End of "primitives" here ^^ ----------------------------------------

(define (noisy-load s)
  (status (format "Loading ~a" s))
  (load s))

(define (file->exps s)
  (call-with-input-file s input->exps))

(define (input->exps i)
  (let loop ()
    (define e (read i))
    (if (eof-object? e)
        '()
        (cons e (loop)))))

;; Deal with anything that the host Scheme's `read` can't handle.
(define (filtered-file->exps s)
  (cond
    [need-vector-filter?
     ((if (input-port? s)
          (lambda (s proc) (proc s))
          call-with-input-file)
      s
      (lambda (i)
        (let* ([str (get-string-all i)]
               [str (list->string
                     ;; Rewrite `#vfl()` to an `a-flvector` instance
                     (let ([vfl (string->list "#vfl()")])
                       (let loop ([l (string->list str)] [quoted #f])
                         (cond
                           [(null? l) '()]
                           [(and (not quoted)
                                 (let loop ([vfl vfl] [l l])
                                   (and (eqv? (car vfl) (car l))
                                        (let ([vfl (cdr vfl)]
                                              [l (cdr l)])
                                          (or (and (null? vfl)
                                                   l)
                                              (and (pair? l)
                                                   (loop vfl l)))))))
                            => (lambda (rest-l)
                                 (append (string->list (format "~s" (make-a-flvector)))
                                         (loop rest-l #f)))]
                           [(eqv? #\" (car l))
                            ;; we don't want to parse too well, so just time out a string
                            ;; after a certain number of characters
                            (cons (car l) (loop (cdr l) (if quoted #f 32)))]
                           [else
                            (cons (car l) (loop (cdr l) (and quoted (> quoted 1) (sub1 quoted))))]))))])
          ;; Replace `a-flvector` instances with the unique empty flvector
          (let ([r (input->exps (open-string-input-port str))])
            ;; replace a flvector with *the* flvector
            (define ht (make-eq-hashtable))
            (let loop ([r r])
              (cond
                [(hashtable-ref ht r #f)
                 => (lambda (v) v)]
                [(a-flvector? r) $the-empty-flvector]
                [(pair? r)
                 (let ([p (cons #f #f)])
                   (hashtable-set! ht r p)
                   (set-car! p (loop (car r)))
                   (set-cdr! p (loop (cdr r)))
                   p)]
                [(vector? r)
                 (let ([v (make-vector (vector-length r))])
                   (hashtable-set! ht r v)
                   (let vec-loop ([i 0])
                     (unless (= i (vector-length r))
                       (vector-set! v i (loop (vector-ref r i)))
                       (vec-loop (add1 i))))
                   v)]
                [else r]))))))]
    [(input-port? s) (input->exps s)]
    [else (file->exps s)]))

(define (noisy-compile-and-load s)
  (status (format "Loading ~a" s))
  (let-values ([(p get) (open-bytevector-output-port)])
    (compile-to-port (file->exps (path-build "s" s)) p)
    (load-compiled-from-port (open-bytevector-input-port (get)))))

(define (load-machine-config)
  (for-each noisy-compile-and-load
            '("cmacros.ss" "priminfo.ss" "primvars.ss")))
(load-machine-config)

;; Loading "cmacros.ss"-defined `constant`s:
(define (set-target-machine mach)
  ($target-machine mach)
  (status (format "Configured for machine: ~a" ($target-machine))))
(set-target-machine (constant machine-type-name))

(define out-dir (path-build (path-build base-out-dir "boot") (symbol->string ($target-machine))))
(let loop ([out-dir out-dir])
  (unless (file-directory? out-dir)
    (let ([parent (path-parent out-dir)])
      (unless (string=? "" parent)
        (loop parent)))
    (mkdir out-dir)))

(status "== Generate headers")
(noisy-load "mkheader.ss")
(mkscheme.h (path-build out-dir "scheme.h") (constant machine-type-name))
(mkequates.h (path-build out-dir "equates.h"))

(status "== Generate GC traversals")
(noisy-load "mkgc.ss")
(mkgc-ocd.inc (path-build out-dir "gc-ocd.inc"))
(mkgc-oce.inc (path-build out-dir "gc-oce.inc"))
(mkgc-par.inc (path-build out-dir "gc-par.inc"))
(mkheapcheck.inc (path-build out-dir "heapcheck.inc"))

(status "== Switching to host mode")
(select-config host-dir)
(load-machine-config)
(set-target-machine (constant machine-type-name))

;; The expander implementation needs nanopass loaded
(status "== Load nanopass")
(load "./nanopass/nanopass.ss")

(status "== Make variables mutable")
(for-each (lambda (sym)
            (when (top-level-bound? sym)
              (unless (top-level-mutable? sym)
                (eval `(define ,sym ',(top-level-value sym))))))
          (environment-symbols (interaction-environment)))

;; These will be refined by the macro expander, but are needed
;; for "setup.ss" to run:
(define ($make-base-modules) (void))
(define ($make-rnrs-libraries) (void))

;; Configure as usual for loading implementaton files
(for-each noisy-load
          '("setup.ss" "env.ss"))

;; ... but use the compiler instead of the interpreter
(current-eval compile)

;; Set up a syntax-object bridge between the old and new worlds

(define ($make-system-syntax datum)
  ($datum->environment-syntax datum ($system-environment)))

(define ($make-interaction-syntax datum)
  (cond
    [(symbol? datum)
     ($datum->environment-syntax datum ($system-environment))]
    [(pair? datum)
     (cons ($make-interaction-syntax (car datum))
           ($make-interaction-syntax (cdr datum)))]
    [(vector? datum)
     (vector-map $make-interaction-syntax datum)]
    [else datum]))

(define (expand-to-non-syntax/system s)
  (define (requote-syntax s)
    (cond
      [(pair? s)
       (if (and (eq? (car s) 'quote)
                (pair? (cdr s))
                (syntax-object? (cadr s)))
           `($make-system-syntax ',(orig-syntax->datum (cadr s)))
           (cons (requote-syntax (car s))
                 (requote-syntax (cdr s))))]
      [else s]))
  (requote-syntax (expand s primitive-environment)))

(status "== Install new expander")

;; first form is the expander's implementation:
(status "Load expander implementation")
(eval (expand-to-non-syntax/system
       (call-with-input-file
        "s/syntax.ss"
        (lambda (in)
          (read in)))))

(define top-wrapped
  (car (generate-temporaries '(x))))

;; initialize the just-loaded expander
(define (init-syntax-libraries)
  ($make-base-modules)
  ($make-rnrs-libraries))
(init-syntax-libraries)
;; Forward reference, of sorts:
(set! $annotation-options (make-enumeration '(debug profile)))
(set! $make-annotation-options (enum-set-constructor $annotation-options))

;; Loading after the expander means that `%uncprep` sees the new
;; syntax-object constructors
(noisy-load "cprep.ss")

;; Load rest of expander with the new expander, but the interpreter
;; still uses the language of the old expander
(define (eval-with-expand s mode eval-mode)
  (let ([e (parameterize ([$current-expand
                           (let ([orig ($current-expand)])
                             (lambda (e . args)
                               (let ([e ($uncprep (cadr e))])
                                 #;(printf "ct ~s\n" e)
                                 (let ([r (apply orig e args)])
                                   #;(printf "ct out ~s\n" (orig-$uncprep r))
                                   r))))])
             (sc-expand s (if (or (eq? mode 'system)
                                  (and (eq? mode 'system-macros)
                                       (pair? s)
                                       (eq? (car s) 'define-syntax)))
                              ;; Define macros in the system environment
                              ($system-environment)
                              (interaction-environment))
                        #f (eq? eval-mode 'expand)))])
    #;(printf "=> ~s\n" e)
    (if (eq? eval-mode 'expand)
        e
        (let ([r (eval e)])
          #;(printf "= ~s\n" r)
          r))))

(define (evalx s)
  (eval-with-expand s 'system-macros 'eval))

(for-each (lambda (sym)
            (let ([val ($top-level-value sym)])
              (evalx `(define ,sym ',val))
              val))
          primitives)

;; Forward reference
(evalx `(define $syntax-match? #f))

;; Prepare to expand the implementations of macros using the host
;; Scheme, so that macro implementations can use macros that are not
;; yet defined. This constrains the implementation of macros defined
;; in "syntax.ss" to use only constructs in the host Scheme
;; implementation.
(define (expand-to-non-syntax s)
  (define (requote-syntax s)
    (cond
      [(pair? s)
       (cons (requote-syntax (car s))
             (requote-syntax (cdr s)))]
      [(vector? s)
       (vector-map requote-syntax s)]
      [(syntax-object? s)
       ($make-interaction-syntax (orig-syntax->datum s))]
      [else s]))
  #;(pretty-print s)
  (requote-syntax (fluid-let ([identifier? orig-identifier?]
                              [free-identifier=? orig-free-identifier=?]
                              [datum->syntax orig-datum->syntax]
                              [syntax->datum orig-syntax->datum])
                    (expand s primitive-environment))))

(define (evalxm s)
  (let ([rhs (expand-to-non-syntax (caddr s))])
    (evalx `(,(car s) ,(cadr s)
                      ;; `values` avoids deferring evaluation:
                      (values ,rhs)))))

(define (evale s)
  (let loop ([s s])
    (cond
      [(and (pair? s)
            (eq? 'begin (car s)))
       (for-each loop (cdr s))]
      [(and (pair? s)
            (eq? 'define-syntax (car s)))
       (evalxm s)]
      [(and (pair? s)
            (eq? 'define (car s)))
       (evalxm s)]
      [(and (pair? s)
            (eq? 'when-feature (car s)))
       (when (equal? "yes" (expand-to-non-syntax `(when-feature ,(cadr s) "yes")))
         (evalxm (caddr s)))]
      [else
       (evalx (expand-to-non-syntax s))])))

;; Load the macro implementations from "syntax.ss", which is
;; everything after the expander's implementation
(status "Load expander macros")
(define (load-syntax-macro-definitions eval)
  (for-each eval
            (cddr (file->exps (path-build "s" "syntax.ss")))))
(load-syntax-macro-definitions evale)

;; like `map`, but in order:
(define (map* f l)
  (let loop ([l l])
    (if (null? l)
        '()
        (let ([v (f (car l))])
          (cons v (loop (cdr l)))))))

;; Not defined in "syntax.ss", but needed to load nanopass:
(define guard-macro
  '(define-syntax guard
     (syntax-rules (else)
       [(_ (var clause ... [else e1 e2 ...]) b1 b2 ...)
        ($guard #f (lambda (var) (cond clause ... [else e1 e2 ...]))
                (lambda () b1 b2 ...))]
       [(_ (var clause1 clause2 ...) b1 b2 ...)
        ($guard #t (lambda (var p) (cond clause1 clause2 ... [else (p)]))
                (lambda () b1 b2 ...))])))
(evale guard-macro)

(define (eval-now v eval-mode)
  (let loop ([v v] [eval-mode eval-mode])
    (cond
      [(and (pair? v) (eq? (car v) 'begin))
       (for-each (lambda (v) (loop v eval-mode)) (cdr v))]
      [(and (pair? v) (eq? (car v) 'eval-when))
       (when (eq? eval-mode 'eval)
         (for-each (lambda (v) (loop v 'eval))
                   (cddr v)))]
      [(and (pair? v) (eq? (car v) 'recompile-requirements))
       (void)]
      [(and (pair? v) (memq (car v) '(library/ct-info library/rt-info)))
       (when (eq? eval-mode 'eval)
         (let ([uid (cadr v)])
           (client-$sputprop uid '*library* ($sgetprop uid '*library* #f))))]
      [(eq? eval-mode 'eval)
       #;(printf "~s\n" v)
       (eval v)]
      [else (void)])))

(define expanded (make-hashtable equal-hash equal?))

(define (expand-and-load* s mode eval-mode)
  (status (format "~a ~a" (if (eq? eval-mode 'compile) "Expanding" "Loading") s))
  (map* (lambda (e)
          #;(printf "~s\n" e)
          (let ([v (eval-with-expand e mode 'expand)])
            #;(printf "~s\n" v)
            (eval-now v eval-mode)
            v))
        (filtered-file->exps s)))

(define (expand-and-load s mode eval-mode)
  (expand-and-load* s mode eval-mode)
  (void))

(define (expand-once-and-load s mode eval-mode)
  (cond
    [(hashtable-ref expanded s #f)
     => (lambda (es)
          (when (eq? eval-mode 'eval)
            (status (format "Loading expanded ~a" s))
            (eval-now (cons 'begin es) 'eval)))]
    [else
     (let ([es (expand-and-load* s mode eval-mode)])
       (hashtable-set! expanded s es))]))

(status "== Setup for using expander")
(define (configure-compile-time)
  (for-each (lambda (s) (unless (eq? s 'ptr-bits) (remprop s '*constant*))) (oblist))
  (expand-and-load "s/cmacros.ss" 'system 'eval)
  (expand-and-load "s/priminfo.ss" 'system 'eval)
  (expand-and-load "s/primvars.ss" 'system 'eval))
(configure-compile-time)

;; Need just `$compiled-file-header?` from "7.ss":
(for-each (lambda (e)
            (let loop ([e e])
              (cond
                [(and (pair? e)
                      (eq? (car e) 'define)
                      (or (eq? (cadr e) '$compiled-file-header?)
                          (and (pair? (cadr e))
                               (eq? (caadr e) '$compiled-file-header?))))
                 (status "Loading part of s/7.ss")
                 ($set-top-level-value! '$compiled-file-header?
                                        (eval-with-expand (if (pair? (cadr e))
                                                              `(lambda ,(cdadr e) . ,(cddr e))
                                                              (caddr e))
                                                          'system
                                                          'eval))]
                [(and (pair? e)
                      (eq? 'begin (car e)))
                 (for-each loop (cdr e))])))
          (file->exps "s/7.ss"))

(status "== Load nanopass using expander")
(define (load-nanopass)
  (define (load-nano s)
    (expand-once-and-load (path-build "nanopass" s) 'user 'eval))
  (load-nano "nanopass/implementation-helpers.chezscheme.sls")
  (load-nano "nanopass/helpers.ss")
  (load-nano "nanopass/syntaxconvert.ss")
  (load-nano "nanopass/records.ss")
  (load-nano "nanopass/nano-syntax-dispatch.ss")
  (load-nano "nanopass/parser.ss")
  (load-nano "nanopass/unparser.ss")
  (load-nano "nanopass/meta-syntax-dispatch.ss")
  (load-nano "nanopass/meta-parser.ss")
  (load-nano "nanopass/pass.ss")
  (load-nano "nanopass/language-node-counter.ss")
  (load-nano "nanopass/language-helpers.ss")
  (load-nano "nanopass/language.ss")
  (load-nano "nanopass.ss"))
(load-nanopass)

(status "== Set configuration to target")
(eval-with-expand `(define-syntax $sputprop (identifier-syntax client-$sputprop)) 'user 'eval)
(eval-with-expand `(define-syntax $sgetprop (identifier-syntax client-$sgetprop)) 'user 'eval)
(hashtable-set! primitive-substs '$sputprop 'client-$sputprop)
(hashtable-set! primitive-substs '$sgetprop 'client-$sgetprop)
(hashtable-set! primitive-substs '$sremprop 'client-$sremprop)
(select-config xc-dir)
(configure-compile-time)

;; "cmacros.ss" may define macros for pthreads that make sense
;; for compiling the target bootfiles, but not the host compiler;
;; in a normal cross build, that's taken care of by "patch.ss",
;; but that doesn't quite work here
(unless (threaded?)
    (when (eq? 'yes (eval-with-expand '(if-feature pthreads 'yes 'no) 'system 'eval))
      (map* (lambda (e)
              ($sputprop (cadr e) 'no-unbound-warning #t)
              (eval-with-expand e 'system 'eval))
            `((define make-thread-parameter make-parameter)
              (define mutex-acquire (lambda (m) (void)))
              (define mutex-release (lambda (m) (void)))
              (define $tc-mutex (void))))))

(status "== Compile compiler")
(define (load-compiler eval-mode)
  (for-each (lambda (s)
              (expand-once-and-load (path-build "s" s) 'system eval-mode))
            patch-srcs))
(load-compiler 'compile)

(status "== Load compiler")

(define saved-libraries ($loaded-libraries))

(load-compiler 'eval)
(load-nanopass) ; declare nanopass, yet again
(configure-compile-time) ; load cmacros, yet again
(eval-with-expand guard-macro 'system 'eval)

($loaded-libraries saved-libraries) ; attach nanopass declarations to newest expander

(set-target-machine (constant machine-type-name))

(status "== Compile bootfiles")
(status " [At this point, `compile-file` is from the loaded compiler]")

(fasl-compressed #f)

(current-expand (lambda args
                  (parameterize ([$current-expand
                                  (let ([orig ($current-expand)])
                                    (lambda (e . args)
                                      (cond
                                        [(and (pair? e)
                                              (equal? "noexpand" (car e)))
                                         (let ([e ($uncprep (cadr e))])
                                           #;(printf "ct ~s\n" e)
                                           (let ([r (apply orig e args)])
                                             #;(printf "ct out ~s\n" (orig-$uncprep r))
                                             r))]
                                        [(equal? e '($target-machine))
                                         (apply orig e args)]
                                        [else
                                         (error 'expand "unexpected nesting: ~s" e)])))])
                  (apply sc-expand args))))

;; Define `expand` to use our `current-expand`
(define expand
  (case-lambda
    [(x) ((current-expand) x)]
    [(x env-spec) ((current-expand) x env-spec)]
    [(x env-spec records?) ((current-expand) x env-spec records?)]
    [(x env-spec records? compiling-a-file) ((current-expand) x env-spec records? compiling-a-file)]
    [(x env-spec records? compiling-a-file outfn) ((current-expand) x env-spec records? compiling-a-file outfn)]))

(define so-suffix (format ".~a" ($target-machine)))

(define (compile-s-file s)
  (let ([start (cpu-time)])
    (parameterize ([optimize-level 3]
                   [debug-level 0])
      (let ([src (path-build "s" s)]
            [dest (path-build xc-dir (string-append (path-root s) so-suffix))])
        (printf "compiling ~a with output to ~a\n" src dest)
        (let* ([exps (filtered-file->exps src)]
               [op (open-file-output-port dest (file-options no-fail))])
          (compile-to-port exps op)
          (close-output-port op))))
    (status (format "  ~as elapsed cpu time" (/ (- (cpu-time) start) 1000.0)))))

(for-each compile-s-file
          base-srcs)
(for-each compile-s-file
          compiler-srcs)

(let* ([src->so (lambda (src) (path-build xc-dir (string-append (path-root src) so-suffix)))])
  (status (format "Writing ~a/petite.boot" out-dir))
  (apply $make-boot-file (path-build out-dir "petite.boot")
         ($target-machine) '()
         (map src->so base-srcs))
  (status (format "Writing ~a/scheme.boot" out-dir))
  (apply $make-boot-file (path-build out-dir "scheme.boot")
         ($target-machine) '("petite")
         (map src->so compiler-srcs)))
