;;; primvars.ms
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

(mat primvars
  (let loop ([ls (oblist)] [bad '()])
    (if (null? ls)
        (or (null? bad)
            (begin
              (pretty-print bad)
              (errorf #f "incorrect library-entry bindings for symbols ~s" bad)))
        (let ([x (car ls)])
          (if (let ([i (#%$sgetprop x '*library-entry* #f)])
                (or (not i) (#%$lookup-library-entry i)))
              (loop (cdr ls) bad)
              (loop (cdr ls) (cons x bad))))))

  (let ()
    (define (get-cte x) (#%$sgetprop x '*cte* #f))
    (define (keyword? x)
      (cond
        [(get-cte x) => (lambda (b) (not (eq? (car b) 'primitive)))]
        [else #f]))
    (define (variable? x)
      (cond
        [(get-cte x) => (lambda (b) (eq? (car b) 'primitive))]
        [else #t]))
    (define (scheme? x) (eq? (#%$sgetprop x '*scheme* #f) x))
    (let loop ([ls (remp gensym? (oblist))] [bad '()])
      (if (null? ls)
          (or (null? bad)
              (begin
                (pretty-print bad)
                (errorf #f "incorrect system/scheme bindings for symbols ~s" bad)))
          (let ([x (car ls)])
            (if (case (#%$symbol-type x)
                  [(system)
                   (and (#%$top-level-bound? x)
                        (top-level-syntax? x)
                        (not (top-level-syntax? x (scheme-environment)))
                        (variable? x)
                        (not (keyword? x))
                        (not (scheme? x)))]
                  [(system-keyword)
                   (and (not (#%$top-level-bound? x))
                        (top-level-syntax? x)
                        (not (top-level-syntax? x (scheme-environment)))
                        (not (variable? x))
                        (keyword? x)
                        (not (scheme? x)))]
                  [(primitive)
                   (and (#%$top-level-bound? x)
                        (top-level-syntax? x)
                        (top-level-syntax? x (scheme-environment))
                        (variable? x)
                        (not (keyword? x))
                        (scheme? x))]
                  [(keyword)
                   (and (not (#%$top-level-bound? x))
                        (top-level-syntax? x)
                        (top-level-syntax? x (scheme-environment))
                        (not (variable? x))
                        (keyword? x)
                        (scheme? x))]
                  [(library-uid) ; same as keyword, except top-evel-bound
                   (and (#%$top-level-bound? x)
                        (top-level-syntax? x)
                        (top-level-syntax? x (scheme-environment))
                        (not (variable? x))
                        (keyword? x)
                        (scheme? x))]
                  [(system-library-uid)
                   (and (#%$top-level-bound? x) ; same as system-keyword, except top-evel-bound
                        (top-level-syntax? x)
                        (not (top-level-syntax? x (scheme-environment)))
                        (not (variable? x))
                        (keyword? x)
                        (not (scheme? x)))]
                  [else
                   (and (not (#%$top-level-bound? x))
                        (top-level-syntax? x)
                        (not (top-level-syntax? x (scheme-environment)))
                        (not (get-cte x))
                        (not (scheme? x)))])
                (loop (cdr ls) bad)
                (loop (cdr ls) (cons x bad))))))
     #t)

  (let ()
    (define (get-cte x) (#%$sgetprop x '*cte* #f))
    (define (scheme? x) (eq? (#%$sgetprop x '*scheme* #f) x))
    (define (mat-id? x)
      (memq x
        '(equivalent-expansion? pretty-equal? mat-run
           show-mat-source-info mat-file enable-cp0 windows? embedded?
           *examples-directory* *scheme*
           *fuzz* ~= fl~= cfl~= == nan pi +pi +pi/2 +pi/4 -pi -pi/2 -pi/4 +e -e
           separate-compile separate-eval run-script patch-exec-path $record->vector
           $cat_flush
           test-cp0-expansion
           mkfile rm-rf touch)))
    (let loop ([ls (remp gensym? (oblist))] [bad '()])
      (if (null? ls)
          (or (null? bad)
              (begin
                (pretty-print bad)
                (errorf #f "incorrect top-level bindings for symbols ~s" bad)))
          (loop (cdr ls)
            (let ([x (car ls)])
              (if (let ([loc (#%$sgetprop x '*top* #f)])
                    (case (#%$symbol-type x)
                      [(keyword) (eq? loc x)]
                      [(primitive)
                       (and
                         (top-level-bound? x)
                         (eq? (top-level-value x) (top-level-value x (scheme-environment)))
                         (eq? loc x))]
                      [else
                        (or (mat-id? x)
                            (not loc)
                            (not (top-level-bound? x))
                            (guard (c [else #t])
                              (#2%top-level-value x)
                              #f))]))
                  bad
                  (cons x bad)))))))
 )

(mat arity
  (or (= (optimize-level) 3)
      (let ()
        (define oops #f)
        (define (prefix=? prefix str)
          (let ([n (string-length prefix)])
            (and (>= (string-length str) n)
                 (string=? (substring str 0 n) prefix))))
        (define (check prim n)
          (define (okay-condition? c)
            (and (violation? c)
                 (message-condition? c)
                 (irritants-condition? c)
                 (let ([msg (condition-message c)] [args (condition-irritants c)])
                   (or (and (prefix=? "incorrect number of arguments" msg)
                            (and (list? args) (= (length args) 1))
                            (let ([unprefixed (#%$sgetprop prim '*unprefixed* prim)])
                              (or (and (procedure? (car args))
                                       (let ([name (#%$procedure-name (car args))])
                                         (or (not name) (equal? name (symbol->string unprefixed)))))
                                  (and (pair? (car args)) (eq? (caar args) unprefixed)))))
                       (and (prefix=? "incorrect argument count" msg)
                            (and (list? args) (= (length args) 1) (string? (car args)))
                            (let ([unprefixed (#%$sgetprop prim '*unprefixed* prim)])
                              (prefix=? (format "(~s" unprefixed) (car args))))))))
          (let ([call `(,prim ,@(make-list n `',(void)))])
            (unless (guard (c [else (okay-condition? c)])
                      (eval `(begin ,call #f)))
              (set! oops #t)
              (printf "no argcount error for ~s\n" call))))
        (for-each
          (lambda (prim)
            (let ([a* (#%$sgetprop prim '*arity* #f)])
              (when a*
                (let loop ([n 0] [a* a*])
                  (cond
                    [(null? a*) (check prim n)]
                    [(= (- -1 (car a*)) n) (void)]
                    [(= (car a*) n) (loop (+ n 1) (cdr a*))]
                    [else (check prim n) (loop (+ n 1) a*)])))))
          (oblist))
        (not oops)))
  (or (= (optimize-level) 3)
      (let ()
        (define oops #f)
        (define (prefix=? prefix str)
          (let ([n (string-length prefix)])
            (and (>= (string-length str) n)
                 (string=? (substring str 0 n) prefix))))
        (define (write-and-load x)
          (with-output-to-file "testfile.ss"
            (lambda () (pretty-print x))
            'replace)
          (load "testfile.ss"))
        (define (check prim n)
          (define (okay-condition? c)
            (and (violation? c)
                 (message-condition? c)
                 (irritants-condition? c)
                 (let ([msg (condition-message c)] [args (condition-irritants c)])
                   (or (and (prefix=? "incorrect number of arguments" msg)
                            (and (list? args) (= (length args) 1))
                            (let ([unprefixed (#%$sgetprop prim '*unprefixed* prim)])
                              (or (and (procedure? (car args))
                                       (let ([name (#%$procedure-name (car args))])
                                         (or (not name) (equal? name (symbol->string unprefixed)))))
                                  (and (pair? (car args)) (eq? (caar args) unprefixed)))))
                       (and (prefix=? "incorrect argument count" msg)
                            (and (list? args) (= (length args) 1) (string? (car args)))
                            (let ([unprefixed (#%$sgetprop prim '*unprefixed* prim)])
                              (prefix=? (format "(~s" unprefixed) (car args))))))))
          (let ([call `(,prim ,@(make-list n '(void)))])
            (let ([warn? #f] [error? #f])
              (guard (c [(okay-condition? c) (set! error? #t)])
                (with-exception-handler
                  (lambda (x) (if (warning? x) (begin (set! warn? #t) (values)) (raise-continuable x)))
                  (lambda () (write-and-load `(begin ,call #f)) #f)))
              (unless warn? (printf "no argcount warning for ~s\n" call) (set! oops #t))
              (unless error? (printf "no argcount error for ~s\n" call) (set! oops #t)))))
        (for-each
          (lambda (prim)
            (let ([a* (#%$sgetprop prim '*arity* #f)])
              (when a*
                (let loop ([n 0] [a* a*])
                  (cond
                    [(null? a*) (check prim n)]
                    [(= (- -1 (car a*)) n) (void)]
                    [(= (car a*) n) (loop (+ n 1) (cdr a*))]
                    [else (check prim n) (loop (+ n 1) a*)])))))
          (oblist))
        (not oops)))
)

(mat check-prim-arg-errors
  (or (= (optimize-level) 3)
      (let ()
        ; check-prim-arg-errors use the signatures in primdata.ss, when possible, to verify that
        ; primitives perform required argument type checks.  for each argument to each primitive
        ; and for each specified 'bad' value, it passes the 'bad' value for that argument and
        ; 'good' values for each other argument.  for some arguments to some primitives, e.g., the
        ; first argument to remove, there is no 'bad' value, so that argument is not checked.
        ;
        ; the test has several deficiencies:
        ;  - for arguments labeled sub-<type>, it cannot determine a 'good' value.  this can be
        ;    addressed only by refining the types given in primdata.ss, including adding
        ;    dependent types for things like list-ref, the range of whose second argument
        ;    depends on its first.
        ;  - it doesn't verify that the raised condition is appropriate, other than ruling out
        ;    warning conditions, non-violation conditions, and invalid memory references.
        (meta define feature*
          (call-with-port
            (open-input-file (let ([fn (format "../s/~a.def" (machine-type))])
                               (if (file-exists? fn) fn (format "../~a" fn))))
            (lambda (ip)
              (let loop ()
                (let ([x (read ip)])
                  (cond
                    [(eof-object? x) '()]
                    [(and (list? x) (eq? (car x) 'features)) (cdr x)]
                    [else (loop)]))))))
        (define-syntax define-symbol-flags*
          (lambda (x)
            (define construct-name
              (lambda (template-identifier . args)
                (datum->syntax
                  template-identifier
                  (string->symbol
                    (apply string-append
                      (map (lambda (x) (format "~a" (syntax->datum x)))
                        args))))))
            (syntax-case x (libraries flags)
              [(_ ([libraries lib ...] [flags shared-flag ...]) entry ...)
               (andmap identifier? #'(shared-flag ...))
               (let ()
                 (define prim-name
                   (lambda (x)
                     (syntax-case x ()
                       [(prefix prim)
                        (and (identifier? #'prefix) (identifier? #'prim))
                        (with-syntax ([prefix:prim (construct-name #'prim #'prefix #'prim)])
                          #'(prim . prefix:prim))]
                       [prim (identifier? #'prim) #'(prim . prim)])))
                 (define ins-and-outs
                   (lambda (ins outs)
                     (syntax-case ins (->)
                       [((in ...) ...) #`(((in ...) #,outs) ...)])))
                 (define do-entry
                   (lambda (x)
                     (syntax-case x (feature sig flags ->)
                       [(prim [feature f] . more)
                        (if (memq (datum f) feature*)
                            (do-entry #'(prim . more))
                            #'(void))]
                       [(prim [flags flag ...]) (do-entry #'(prim [sig] [flags flag ...]))]
                       [(prim [sig [(in ...) ... -> (out ...)] ...] [flags flag ...])
                        (with-syntax ([(unprefixed . prim) (prim-name #'prim)])
                          (with-syntax ([((((in ...) (out ...)) ...) ...)
                                         (map ins-and-outs #'(((in ...) ...) ...) #'((out ...) ...))])
                            #'(fuzz-prim-args 'prim 'unprefixed '(lib ...)
                                '(shared-flag ... flag ...)
                                '([(in ...) . (out ...)] ... ...))))])))
                 #`(begin #,@(map do-entry #'(entry ...))))])))
        (define env
          (let ([env (copy-environment (scheme-environment) #t)])
            (define-syntax def
              (syntax-rules ()
                [(_ name val)
                 (define-top-level-value 'name val env)]))
            (def *env env)
            (let* ([bv (string->utf8 "(if #f #f)")]
                   [binary-input-port (open-bytevector-input-port bv)]
                   [sfd (make-source-file-descriptor "foo" binary-input-port #t)]
                   [source-object (make-source-object sfd 2 3)]
                   [annotation (make-annotation '(if #f #f) source-object '(source expr))])
              (def *binary-input-port binary-input-port)
              (def *sfd sfd)
              (def *source-object source-object)
              (def *annotation annotation))
            (let-values ([(binary-output-port getter) (open-bytevector-output-port)])
              (def *binary-output-port binary-output-port)
              (def *binary-port binary-output-port))
            (def *cost-center (make-cost-center))
            (def *date (current-date))
            (def *phantom-bytevector (make-phantom-bytevector 10))
            (def *eq-hashtable (make-eq-hashtable))
            (def *ftype-pointer (make-ftype-pointer double 0))
            (def *symbol-hashtable (make-hashtable symbol-hash eq?))
            (def *genny (gensym))
            (def *old-hash-table (make-hash-table))
            (let ()
              (define rtd (make-record-type-descriptor 'foo #f #f #f #f '#((mutable x))))
              (define rcd (make-record-constructor-descriptor rtd #f #f))
              (def *rtd rtd)
              (def *rcd rcd)
              (def *record ((record-constructor rcd) 3)))
            (def *sstats (statistics))
            (def *time (make-time 'time-duration 0 5))
            (cond
              [(fx< (fixnum-width) 32)
               (def *max-iptr (- (expt 2 31) 1))
               (def *min-iptr (- (expt 2 31)))
               (def *max-uptr (- (expt 2 32) 1))]
              [(fx< (fixnum-width) 64)
               (def *max-iptr (- (expt 2 63) 1))
               (def *min-iptr (- (expt 2 63)))
               (def *max-uptr (- (expt 2 64) 1))]
              [else (errorf 'fuzz-prim-args "unexpected fixnum width ~s" (fixnum-width))])
            env))
        (define type-table
          (let ()
            (define ht (make-hashtable symbol-hash eq?))
            (define-syntax declare-types
              (syntax-rules ()
                [(_ ((type ...) good bad ...) ...)
                 (begin
                   (let ([payload '(good bad ...)])
                     (for-each
                       (lambda (t) (symbol-hashtable-set! ht t payload))
                       '(type ...)))
                   ...)]))
            (declare-types
              [(annotation) *annotation '() #f]
              [(annotation-options) (annotation-options debug) 1/2 #f]
              [(binary-input-port) *binary-input-port 0 *binary-output-port (current-input-port) #f]
              [(binary-output-port) *binary-output-port *binary-input-port (current-output-port) #f]
              [(binary-port) *binary-output-port (current-input-port) #f]
              [(bit) 0 7 1.0 'a #f]
              [(boolean) #f '()]
              [(box) &a '((a)) #f]
              [(bytevector) '#vu8(0) "a" #f]
              [(cflonum) 0.0+1.0i 0 'a #f]
              [(char) #\a 0 #f]
              [(codec) latin-1-codec 0 #f]
              [(code) (closure-code 'values) 0 #f]
              [(compile-time-value) (make-compile-time-value 17)]
              [(condition) (make-who-condition 'me) 'the-who]
              [(continuation-condition) (call/cc make-continuation-condition) (make-who-condition 'who) #f]
              [(cost-center) *cost-center '(a) #f]
              [(date) *date *time #f]
              [(enum-set) (file-options compressed) 0 #f]
              [(environment) *env '((a . b)) #f]
              [(eq-hashtable) *eq-hashtable *symbol-hashtable #f]
              [(exact-integer) (- (most-negative-fixnum) 1) 1/2 #f]
              [(exception-state) (current-exception-state) 0 #f]
              [(eof/char) #\a 0 #f]
              [(eof/u8) 0 -1 (expt 2 8) "a" #f]
              [(fasl-strip-options) (fasl-strip-options inspector-source) (file-options compressed) #f]
              [(file-options) (file-options compressed) 1/2 #f]
              [(fixnum) -1 'q (+ (most-positive-fixnum) 1) (- (most-negative-fixnum) 1)]
              [(flonum) 0.0 0 0.0+1.0i 'a #f]
              [(ftype-pointer) *ftype-pointer 0 *time #f]
              [(fxvector) '#vfx(0) "a" #f]
              [(gensym) *genny sym #f]
              [(hashtable) *eq-hashtable '((a . b)) #f]
              [(identifier) #'x x 17 #f]
              [(import-spec) (chezscheme) 0 '(a . b) #f]
              [(input-port) (current-input-port) 0 *binary-output-port (transcoded-port *binary-output-port (native-transcoder)) #f]
              [(integer) 0.0 1/2 1.0+0.0i 'a #f]
              [(i/o-encoding-error) (make-i/o-encoding-error 17 23) (make-who-condition 'who) 1/2 #f]
              [(i/o-filename-error) (make-i/o-filename-error 17) (make-who-condition 'who) 3 #f]
              [(i/o-invalid-position-error) (make-i/o-invalid-position-error 17) (make-who-condition 'who) "" #f]
              [(i/o-port-error) (make-i/o-port-error 17) (make-who-condition 'who) '(a) #f]
              [(irritants-condition) (make-irritants-condition 17) (make-who-condition 'who) 'a #f]
              [(length) 0 -1 (+ (most-positive-fixnum) 1) 'a #f]
              [(library-requirements-options) (library-requirements-options import invoke) 1/2 #f]
              [(list) '(a) '#1=(a . #1#) 17 '#() #f]
              [(maybe-binary-output-port) *binary-output-port *binary-input-port (current-output-port)]
              [(maybe-char) #\a 0]
              [(maybe-pathname) "a" 'a]
              [(maybe-procedure) values 0]
              [(maybe-rtd) *rtd *record ""]
              [(maybe-sfd) *sfd '(q)]
              [(maybe-string) "a" 'a]
              [(maybe-symbol) 'a 0 "a"]
              [(maybe-textual-output-port) (current-output-port) 0 *binary-output-port (transcoded-port *binary-input-port (native-transcoder))]
              [(maybe-transcoder) (native-transcoder) 0]
              [(maybe-ufixnum) 0 -1 (+ (most-positive-fixnum) 1) 'a]
              [(maybe-uint) 0 -1 'a]
              [(maybe-who) 'who 17]
              [(message-condition) (make-message-condition 17) (make-who-condition 'who) 'q #f]
              [(number) 1+2i 'oops #f]
              [(old-hash-table) *old-hash-table '((a . b)) #f]
              [(output-port) (current-output-port) 0 *binary-input-port (transcoded-port *binary-input-port (native-transcoder)) #f]
              [(pair) '(a . b) 'a #f]
              [(pathname) "a" 'a #f]
              [(pfixnum) 1 0 #f]
              [(phantom-bytevector) *phantom-bytevector '#vu8(0) #f]
              [(pseudo-random-generator) *pseudo-random-generator #f]
              [(port) (current-input-port) 0 #f]
              [(procedure) values 0 #f]
              [(ptr) 1.0+2.0i]
              [(rational) 1/2 1+2i #f]
              [(rcd) *rcd *rtd "" #f]
              [(real) 1/2 1+2i #f]
              [(record) *record '#(a) #f]
              [(rtd) *rtd *record "" #f]
              [(s16) -1 'q (expt 2 15) (- -1 (expt 2 15)) #f]
              [(s24) -1 'q (expt 2 23) (- -1 (expt 2 23)) #f]
              [(s32) -1 'q (expt 2 31) (- -1 (expt 2 31)) #f]
              [(s40) -1 'q (expt 2 39) (- -1 (expt 2 39)) #f]
              [(s48) -1 'q (expt 2 47) (- -1 (expt 2 47)) #f]
              [(s56) -1 'q (expt 2 55) (- -1 (expt 2 55)) #f]
              [(s64) -1 'q (expt 2 63) (- -1 (expt 2 63)) #f]
              [(s8) -1 'q (expt 2 7) (- -1 (expt 2 7)) #f]
              [(sfd) *sfd '(q)]
              [(sint) -1 'q]
              [(source-condition) (make-source-condition 17) (make-who-condition 'who) #f]
              [(source-object) *source-object '#&a #f]
              [(sstats) *sstats '#(0 2 7 3) #f]
              [(string) "a" 'a #f]
              [(string/bytevector) no-good]
              [(sub-ptr) no-good]
              [(sub-uint sub-ufixnum sub-index sub-length sub-list sub-fixnum sub-flonum sub-integer sub-number sub-port sub-rtd sub-sint sub-string sub-symbol sub-textual-output-port sub-vector maybe-sub-rcd maybe-sub-symbol) no-good #!eof]
              [(symbol) 'a 0 "a" #f]
              [(symbol-hashtable) *symbol-hashtable *eq-hashtable '() #f]
              [(syntax-violation) (make-syntax-violation '(if) #f) 'oops #f]
              [(textual-input-port) (current-input-port) 0 *binary-input-port (transcoded-port *binary-output-port (native-transcoder)) #f]
              [(textual-output-port) (current-output-port) 0 *binary-output-port (transcoded-port *binary-input-port (native-transcoder)) #f]
              [(time) *time "no-time" #f]
              [(timeout) *time "no-time"]
              [(transcoder) (native-transcoder) 0 #f]
              [(u16) 0 -1 (expt 2 16) "a" #f]
              [(u24) 0 -1 (expt 2 24) "a" #f]
              [(u32) 0 -1 (expt 2 32) "a" #f]
              [(u40) 0 -1 (expt 2 40) "a" #f]
              [(u48) 0 -1 (expt 2 48) "a" #f]
              [(u56) 0 -1 (expt 2 56) "a" #f]
              [(u64) 0 -1 (expt 2 64) "a" #f]
              [(u8) 0 -1 (expt 2 8) "a" #f]
              [(u8/s8) -1 'q (expt 2 8) (- -1 (expt 2 7)) #f]
              [(ufixnum) 0 -1 (+ (most-positive-fixnum) 1) 'a #f]
              [(uint) 0 -1 'a #f]
              [(uinteger) 9.0 -1 -1.0 'a #f]
              [(uptr) 0 -1 'a (+ *max-uptr 1) #f]
              [(uptr/iptr) -1 'q (+ *max-uptr 1) (- *min-iptr 1) #f]
              [(vector) '#(a) "a" #f]
              [(who-condition) (make-who-condition 'me) (make-message-condition "hello") 'the-who])
            (meta-cond
              [(memq 'pthreads feature*)
               (declare-types
                 [(condition-object) (make-condition) "not a mutex" #f]
                 [(mutex) (make-mutex) "not a mutex" #f])])
            ht))
        (define (fuzz-prim-args name unprefixed-name lib* flag* in*/out**)
          (define-syntax flags-set?  (syntax-rules () [(_ x ...) (and (memq 'x flag*) ...)]))
          (define good/bad
            (lambda (in* k)
              (unless (null? (remq '... (remq 'ptr in*)))
                (let loop ([in* in*] [rgood* '()] [rbad** '()])
                  (if (null? in*)
                      (k (reverse rgood*) (reverse rbad**))
                      (let ([in (car in*)] [in* (cdr in*)])
                        (cond
                          [(eq? in '...)
                           (assert (not (null? rgood*)))
                           (let ([good (car rgood*)] [bad* (car rbad**)])
                             (loop in* (cdr rgood*) (cdr rbad**))
                             (loop in* rgood* rbad**)
                             (loop in* (cons good rgood*) (cons bad* rbad**))
                             (loop in* (cons* good good rgood*) (cons* bad* bad* rbad**)))]
                          [(pair? in)
                           (loop in*
                             (cons `'(quote ,(let f ([x in])
                                               (cond
                                                 [(pair? x) (cons (f (car x)) (f (cdr x)))]
                                                 [(eq? x 'ptr) 0]
                                                 [else (errorf 'fuzz-prim-args "unhandled type ~s" in)])))
                               rgood*)
                             (cons '((quote ())) rbad**))]
                          [(symbol-hashtable-ref type-table in #f) =>
                           (lambda (good.bad*)
                             (loop in* (cons (car good.bad*) rgood*) (cons (cdr good.bad*) rbad**)))]
                          [else (errorf 'fuzz-prim-args "unhandled type ~s" in)])))))))
          (when (and (flags-set? primitive proc)
                     (not (flags-set? system proc)))
            (for-each
              (lambda (in*)
                (good/bad in*
                  (lambda (good* bad**)
                    (let loop ([good* good*] [bad** bad**] [rgood* '()])
                      (unless (null? good*)
                        (unless (or (memq 'no-good rgood*) (memq 'no-good (cdr good*)))
                          (for-each
                            (lambda (bad)
                              (let ([call `(,name ,@(reverse rgood*) ,bad ,@(cdr good*))])
                                (printf "testing ~s..." call)
                                (flush-output-port)
                                (let ([c (call/cc
                                           (lambda (k)
                                             (with-exception-handler
                                               (lambda (c) (unless (warning? c) (k c)))
                                               (lambda () (eval call env) #f))))])
                                  (if c 
                                      (if (and (violation? c)
                                               (not (and (syntax-violation? c)
                                                         (message-condition? c)
                                                         (equal? (condition-message c) "invalid syntax")))
                                               (not (and (irritants-condition? c)
                                                         ; split up so we can grep for "invalid memory reference" in mat output and not see this
                                                         (member (string-append "invalid" " " "memory reference") (condition-irritants c)))))
                                          (begin
                                            (display-condition c)
                                            (newline))
                                          (errorf 'fuzz-prim-args "unexpected exception occurred evaluating ~s: ~a" call
                                            (with-output-to-string (lambda () (display-condition c)))))
                                      (errorf 'fuzz-prim-args "no exception occurred evaluating ~s" call)))))
                            (car bad**)))
                        (loop (cdr good*) (cdr bad**) (cons (car good*) rgood*)))))))
              (map car in*/out**))))
        (meta-cond
          [(file-exists? "../s/primdata.ss") (include "../s/primdata.ss")]
          [else (include "../../s/primdata.ss")])
        #t))
)

(mat nonprocedure-value
  (begin
    (for-each
      (lambda (x)
        (guard (c [else (unless (equal? (condition-message c) "variable ~:s is not bound")
                          (errorf #f "wrong error for ~s (~a)" x (with-output-to-string (lambda () (display-condition c)))))])
          (parameterize ([optimize-level 2])
            (eval `(,x)))
          (errorf #f "no error for ~s" x)))
      (remp (lambda (x) (or (top-level-bound? x) (top-level-syntax? x))) (oblist)))
    #t)
  (begin
    (for-each
      (lambda (x)
        (guard (c [else (unless (equal? (condition-message c) "attempt to apply non-procedure ~s")
                          (errorf #f "wrong error for ~s (~a)" x (with-output-to-string (lambda () (display-condition c)))))])
          (parameterize ([optimize-level 2])
            (eval `(,x)))
          (errorf #f "no error for ~s" x)))
      (filter (lambda (x) (and (top-level-bound? x) (not (procedure? (top-level-value x))))) (oblist)))
    #t)
)

(mat make-parameter
   (begin (define p (make-parameter #f not)) #t)
   (p)
   (begin (p #f) (p))
   (begin (p #t) (not (p)))
   (begin (define q (make-parameter #t)) #t)
   (q)
   (begin (q #f) (not (q)))
   (begin (q #t) (q))
   (error? (make-parameter 1 2))
   (begin
     (define p
       (make-parameter 5
         (lambda (x) (+ x 1))))
     #t)
   (eqv? (p) 6)
   (error? (p 'a))
   (error? (make-parameter 3 (lambda (x y) x)))
 )

(mat parameterize
   (begin (define p (make-parameter #f not)) #t)
   (begin (define q (make-parameter #t)) #t)
   (begin (p #f) (p))
   (begin (q #t) (q))
   (parameterize ([p #t] [q #f])
      (and (not (p)) (not (q))))
   (not (p))
   (q)
   (parameterize () #t)
   (eq? (parameterize () (define x 4) x) 4)
   (let* ((x (make-parameter 'a)) (f (lambda () (x))))
     (and
       (parameterize ((x 'b))
         (and (eq? (x) 'b) (eq? (f) 'b)))
       (eq? (x) 'a)
       (eq? (f) 'a)))
   (let* ((x (make-parameter 'a)) (f (lambda () (x))))
     (and
       (call/cc
         (lambda (return)
           (parameterize ((x 'b))
             (return (and (eq? (x) 'b) (eq? (f) 'b))))))
       (eq? (x) 'a)
       (eq? (f) 'a)))
   (equal?
     (let* ((x (make-parameter 'a)) (f (lambda () (x))))
       ((call/cc
          (lambda (return)
            (parameterize ((x 'b))
              (call/cc
                (lambda (back)
                  (return back)))
              (let ((ans (f))) (lambda (y) (list ans (x)))))))
        '()))
     '(b a))
   (error? ; invalid number of arguments to #<procedure x>
     (let ([x (lambda (x) #t)]) (parameterize ([x 7]) 4)))
   ; make sure nothing silly happens if we parameterize the same parameter
   (begin (define q (make-parameter 0)) #t)
   (eqv? (parameterize ([q 2] [q 2]) (q)) 2)
   (eqv? (q) 0)
 )

(define id (lambda (x) x))

(define $big (+ (most-positive-fixnum) 1))

(define ok
   (lambda (p v)
      (parameterize ([p v]) (equal? (p) v))))

(mat case-sensitive
   (case-sensitive)
   (ok case-sensitive #f)
   (ok case-sensitive #t)
 )

(mat collect-generation-radix
   (fxpositive? (collect-generation-radix))
   (ok collect-generation-radix 1)
   (error? (collect-generation-radix 'a))
   (error? (collect-generation-radix -1))
   (error? (collect-generation-radix 0))
 )

(mat collect-notify
   (not (collect-notify))
   (ok collect-notify #t)
   (ok collect-notify #f)
 )

(mat collect-request-handler
   (procedure? (collect-request-handler))
   (ok collect-request-handler (collect-request-handler))
   (error? (collect-request-handler #f))
 )

(mat collect-trip-bytes
   (fxpositive? (collect-trip-bytes))
   (ok collect-trip-bytes 100)
   (error? (collect-trip-bytes -100))
   (error? (collect-trip-bytes $big))
 )

(mat current-eval
   (procedure? (current-eval))
   (ok current-eval id)
   (error? (current-eval '#()))
 )

(mat current-input-port
   (input-port? (current-input-port))
   (ok current-input-port (open-input-string ""))
   (error? (current-input-port (open-output-string)))
 )

(mat current-output-port 
   (output-port? (current-output-port))
   (ok current-output-port (open-output-string))
   (error? (current-output-port (open-input-string "hello")))
 )

(mat eval-syntax-expanders-when
   (= (length (eval-syntax-expanders-when)) 3)
   (equal?
     (andmap (lambda (x) (memq x '(compile load eval)))
             (eval-syntax-expanders-when))
     '(eval))
   (ok eval-syntax-expanders-when '(compile))
   (ok eval-syntax-expanders-when '())
   (error? (eval-syntax-expanders-when '(compiling)))
 )

(mat generate-interrupt-trap
   (generate-interrupt-trap)
   (ok generate-interrupt-trap #t)
   (ok generate-interrupt-trap #f)
 )

(mat gensym-count
   (nonnegative? (gensym-count))
   (ok gensym-count 0)
   (ok gensym-count $big)
   (error? (gensym-count "g"))
 )

(mat gensym-prefix
   (string? (gensym-prefix))
   (ok gensym-prefix "hi")
 )

(mat keyboard-interrupt-handler
   (procedure? (keyboard-interrupt-handler))
   (ok keyboard-interrupt-handler id)
   (error? (keyboard-interrupt-handler 0))
 )

(mat optimize-level
   (fx<= 0 (optimize-level) 3)
   (ok optimize-level 0)
   (ok optimize-level 1)
   (ok optimize-level 2)
   (ok optimize-level 3)
   (error? (optimize-level 4))
 )

(mat pretty-line-length
   (fxpositive? (pretty-line-length))
   (ok pretty-line-length 10)
   (error? (pretty-line-length -1))
   (error? (pretty-line-length $big))
 )

(mat pretty-one-line-limit
   (fxpositive? (pretty-one-line-limit))
   (ok pretty-one-line-limit 100)
   (error? (pretty-one-line-limit 0))
   (error? (pretty-one-line-limit $big))
 )

(mat print-gensym
   (print-gensym)
   (ok print-gensym #f)
   (ok print-gensym #t)
   (ok print-gensym 'pretty)
 )

(mat print-graph
   (not (print-graph))
   (ok print-graph #f)
   (ok print-graph #t)
 )

(mat print-length
   (not (print-length))
   (ok print-length 100)
   (ok print-length #f)
   (error? (print-length -1))
   (error? (print-length $big))
   (error? (print-length '()))
 )

(mat print-level
   (not (print-level))
   (ok print-level 100)
   (ok print-level #f)
   (error? (print-level -1))
   (error? (print-level $big))
 )

(mat print-radix
   (fx= (print-radix) 10)
   (ok print-radix 2)
   (ok print-radix 36)
   (error? (print-radix 37))
   (error? (print-radix 1))
 )

(mat timer-interrupt-handler
   (procedure? (timer-interrupt-handler))
   (ok timer-interrupt-handler id)
   (error? (timer-interrupt-handler 'midnight))
 )

(mat trace-output-port
   (eq? (trace-output-port) (console-output-port))
   (ok trace-output-port (open-output-string))
   (error? (trace-output-port (open-input-string "hello")))
 )

