;;; exceptions.ss
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

#|
TODO:
 - teach default handler to:
   - squirrel away continuation for debug as &continuation simple condition
   - say something about calling debug (if &continuation is included)
 - teach reset to handle closing of ports, etc., in system error chain
 - wire into existing error-handling mechanisms, or visa versa
 - replace error calls as appropriate with violation calls,
   syntax-violation calls, etc.
 - fix: unbound variables show up as #{b *top*:b}
   (~:s in message is supposed to take care of this but format isn't being called)
 - mats for system violations and errors
 - deal with error? and warning? mats
|#

(begin
(let ()
  (define (warning-only? c)
    (and (warning? c) (not (serious-condition? c))))

  (let ()
    (define $display-condition
      (lambda (c op prefix? use-cache?)
        (module (print-source)
          (include "types.ss")
          (define (print-position op prefix src start?)
            (call-with-values
              (lambda () ((current-locate-source-object-source) src start? use-cache?))
              (case-lambda
                [()
                 (let ([sfd (source-sfd src)]
                       [fp (if start? (source-bfp src) (source-efp src))])
                   (fprintf op "~a~a char ~a of ~a" prefix
                     (if (eq? start? 'near) "near" "at")
                     fp (source-file-descriptor-name sfd)))]
                [(path line char)
                 (fprintf op "~a~a line ~a, char ~a of ~a" prefix
                   (if (eq? start? 'near) "near" "at")
                   line char path)])))
          (define (print-source op prefix c)
            (cond
              [($src-condition? c)
               (let ([src ($src-condition-src c)])
                 (when (source? src)
                   (print-position op prefix src ($src-condition-start c))))]
              [(source-condition? c)
               (let ([form (source-condition-form c)])
                 (parameterize ([print-level 3] [print-length 6])
                   (fprintf op "~a~s" prefix (syntax->datum form)))
                 (let-values ([(src start?) ($syntax->src form)])
                   (when src (print-position op " " src start?))))]
              [(syntax-violation? c)
               (let ([form (syntax-violation-form c)]
                     [subform (syntax-violation-subform c)])
                 (define (form-gensym-mode)
                   (cond
                     [(eq? (subset-mode) 'system) #f] ; so bootfile fixpoint is reached in safe mode
                     [else (print-gensym)]))
                 (parameterize ([print-level 3] [print-length 6] [print-gensym (form-gensym-mode)])
                   (if subform
                       (fprintf op "~a~s in ~s" prefix (syntax->datum subform) (syntax->datum form))
                       (fprintf op "~a~s" prefix (syntax->datum form))))
                 (let-values ([(src start?) ($syntax->src subform)])
                   (if src
                       (print-position op " " src start?)
                       (let-values ([(src start?) ($syntax->src form)])
                         (when src (print-position op " " src start?))))))])))
        (cond
          [(and (format-condition? c)
             (guard (ignore [#t #f])
               ($report-string #f
                 (and prefix? (if (warning-only? c) "warning" "exception"))
                 (and (who-condition? c) (condition-who c))
                 (condition-message c)
                 (condition-irritants c)))) =>
           (lambda (s)
             (display s op)
             (print-source op " " c))]
          [(message-condition? c)
           (let ([irritants (if (irritants-condition? c) (condition-irritants c) '())])
             (case (and (list? irritants) (length irritants))
               [(0) 
                ($report-string op
                  (and prefix? (if (warning-only? c) "warning" "exception"))
                  (and (who-condition? c) (condition-who c))
                  "~a"
                  (list (condition-message c)))]
               [(1)
                ($report-string op
                  (and prefix? (if (warning-only? c) "warning" "exception"))
                  (and (who-condition? c) (condition-who c))
                  "~a with irritant ~s"
                  (list (condition-message c) (car irritants)))]
               [else
                ($report-string op
                  (and prefix? (if (warning-only? c) "warning" "exception"))
                  (and (who-condition? c) (condition-who c))
                  "~a with irritants ~s"
                  (list (condition-message c) irritants))]))
           (print-source op " " c)]
          [else
           (fprintf op "Exception occurred")
           (cond
             [(condition? c)
              (print-source op " " c)
              (let ([x* (simple-conditions c)])
                (cond
                  [(null? x*)
                   (fprintf op " with empty condition\n")]
                  [else
                   (fprintf op " with condition components:")
                   (for-each
                     (lambda (x i)
                       (let ([rtd (#3%record-rtd x)])
                         (define (print-field i)
                           (if (csv7:record-field-accessible? rtd i)
                               (parameterize ([print-level 3] [print-length 6])
                                 (fprintf op ": ~s" ((csv7:record-field-accessor rtd i) x)))
                               (fprintf op ": (inaccessible)")))
                         (fprintf op "\n~3d. ~a" i (csv7:record-type-name (#3%record-rtd x)))
                         (if (record-type-opaque? rtd)
                             (fprintf op " (opaque)")
                             (let ([name* (csv7:record-type-field-names rtd)])
                               (if (fx= (length name*) 1)
                                   (print-field 0)
                                   (for-each
                                     (lambda (name i)
                                       (fprintf op "\n      ~s" name)
                                       (print-field i))
                                     name* (iota (length name*))))))))
                     x* (iota (length x*)))]))]
             [else (parameterize ([print-level 3] [print-length 6])
                     (fprintf op " with non-condition value ~s" c))])])))

    (set-who! display-condition
      (case-lambda
        [(c) ($display-condition c (current-output-port) #t #f)]
        [(c op)
         (unless (and (output-port? op) (textual-port? op))
           ($oops who "~s is not a textual output port" op))
         ($display-condition c op #t #f)]))

    (set! $make-source-oops
      (lambda (who msg expr)
        #`(assertion-violation '#,who
            #,(call-with-string-output-port
                (lambda (p)
                  ($display-condition (condition
                                        (make-syntax-violation expr #f)
                                        (make-message-condition msg))
                    p #f #t)))))))

  (set! default-exception-handler
    (lambda (c)
      (let ([cep (console-error-port)])
        (with-exception-handler
          (lambda (c)
            (if (i/o-error? c)
                (begin
                  (debug-condition c)
                  (if (debug-on-exception) (debug))
                  (reset))
                (raise-continuable c)))
          (lambda ()
            ; only I/O to cep in handler-protected code---not (debug), not (reset).
            (fresh-line cep)
            (display-condition c cep)
            (newline cep)
            (unless (or (warning-only? c) (debug-on-exception) (= ($cafe) 0) (not (interactive?)))
              (display-string "Type (debug) to enter the debugger.\n" cep))
            (flush-output-port cep))))
      (unless (warning-only? c)
        (debug-condition c)
        (if (debug-on-exception) (debug))
        (reset)))))

(define debug-on-exception
  (make-parameter #f
    (lambda (x) (and x #t))))

(define base-exception-handler
  ($make-thread-parameter
    default-exception-handler
    (lambda (p)
      (unless (procedure? p) ($oops 'default-exception-handler "~s is not a procedure" p))
      p)))

(let ()
  (define create-exception-stack
    (lambda (p)
      (let ([ls (list p)])
        (set-cdr! ls ls)
        ls)))

  (define default-handler
    (lambda (x)
      ((base-exception-handler) x)))

  ;; the initial value of `($current-handler-stack)` in a thread
  ;; is #f; treat that the same as `default-handler-stack`:
  (define default-handler-stack
    (create-exception-stack default-handler))

  (let ()
    (define-record-type exception-state
      (nongenerative)
      (opaque #t)
      (sealed #t)
      (fields (immutable stack)))

    (set-who! create-exception-state
      (case-lambda
        [() (make-exception-state (create-exception-stack default-handler))]
        [(p)
         (unless (procedure? p) ($oops who "~s is not a procedure" p))
         (make-exception-state (create-exception-stack p))]))

    (set-who! current-exception-state
      (case-lambda
        [() (make-exception-state ($current-handler-stack))]
        [(x)
         (unless (exception-state? x)
           ($oops who "~s is not an exception state" x))
         ($current-handler-stack (exception-state-stack x))])))

  (set-who! with-exception-handler
    (lambda (handler thunk)
      (unless (procedure? handler) ($oops who "~s is not a procedure" handler))
      (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
      (parameterize ([$current-handler-stack (cons handler ($current-handler-stack))])
        (thunk))))

  (set-who! raise
    (lambda (obj)
      (let ([stack (or ($current-handler-stack) default-handler-stack)])
        (let ([handler (car stack)])
          (parameterize ([$current-handler-stack (cdr stack)])
            (handler obj)
            (raise (make-non-continuable-violation)))))))

  (set-who! raise-continuable
    (lambda (obj)
      (let ([stack (or ($current-handler-stack) default-handler-stack)])
        (let ([handler (car stack)])
          (parameterize ([$current-handler-stack (cdr stack)])
            (handler obj))))))

  (set-who! $guard
    (lambda (supply-else? guards body)
      (if supply-else?
          ((call/cc
             (lambda (kouter)
               (let ([original-handler-stack ($current-handler-stack)])
                 (with-exception-handler
                   (lambda (arg)
                     ((call/cc
                        (lambda (kinner)
                          (kouter
                            (lambda ()
                              (guards arg
                                (lambda ()
                                  (kinner
                                    (lambda ()
                                      (parameterize ([$current-handler-stack original-handler-stack])
                                        (raise-continuable arg))))))))))))
                   (lambda ()
                     (call-with-values
                       body
                       (case-lambda
                         [(x) (lambda () x)]
                         [vals (lambda () (apply values vals))]))))))))
          ((call/cc
             (lambda (k)
               (with-exception-handler
                 (lambda (arg) (k (lambda () (guards arg))))
                 (lambda ()
                   (call-with-values
                     body
                     (case-lambda
                       [(x) (lambda () x)]
                       [vals (lambda () (apply values vals))]))))))))))
)

(define-syntax guard
  (syntax-rules (else)
    [(_ (var clause ... [else e1 e2 ...]) b1 b2 ...)
     (identifier? #'var)
     ($guard #f (lambda (var) (cond clause ... [else e1 e2 ...]))
                (lambda () b1 b2 ...))]
    [(_ (var clause1 clause2 ...) b1 b2 ...)
     (identifier? #'var)
     ($guard #t (lambda (var p) (cond clause1 clause2 ... [else (p)]))
                (lambda () b1 b2 ...))]))

(let ()
 ; redefine here to get local predicate
  (define-record-type (&condition $make-simple-condition $simple-condition?)
    (nongenerative #{&condition oyb459ue1fphfx4-a}))

  (define-record-type (compound-condition make-compound-condition compound-condition?)
    (nongenerative)
    (sealed #t)
    (fields (immutable components)))

  (define (check-&condition-subtype! who rtd)
    (unless (record-type-descriptor? rtd)
      ($oops who "~s is not a record type descriptor" rtd))
    (unless (let f ([rtd rtd])
              (or (eq? rtd (type-descriptor &condition))
                  (let ([rtd (record-type-parent rtd)])
                    (and rtd (f rtd)))))
      ($oops who "~s does not describe a subtype of &condition" rtd)))

  (record-writer (type-descriptor &condition)
    (lambda (x p wr)
      (fprintf p "#<condition ~a>" (csv7:record-type-name (#3%record-rtd x)))))

  (record-writer (type-descriptor compound-condition)
    (lambda (x p wr)
      (fprintf p "#<compound condition>")))

  (set-who! $compound-condition? compound-condition?)
  (set-who! $compound-condition-components compound-condition-components)

  (set-who! condition
    (case-lambda
      [(x)
       (unless (or ($simple-condition? x) (compound-condition? x))
         ($oops who "~s is not a condition" x))
       x]
      [x*
       (let ([ls (fold-right
                   (lambda (x ls)
                     (cond
                       [($simple-condition? x) (cons x ls)]
                       [(compound-condition? x) (append (compound-condition-components x) ls)]
                       [else ($oops who "~s is not a condition" x)]))
                   '()
                   x*)])
         (if (fx= (length ls) 1)
             (car ls)
             (make-compound-condition ls)))]))

  (set-who! simple-conditions
    (lambda (x)
      (cond
        [($simple-condition? x) (list x)]
        [(compound-condition? x) (compound-condition-components x)]
        [else ($oops who "~s is not a condition" x)])))

  (set! condition?
    (lambda (x)
      (or ($simple-condition? x) (compound-condition? x))))

  (set-who! condition-predicate
    (lambda (rtd)
      (check-&condition-subtype! who rtd)
      (let ([p? (lambda (x) (record? x rtd))])
        (lambda (x)
          (or (p? x)
              (and (compound-condition? x)
                   (ormap p? (compound-condition-components x))))))))

  (set-who! condition-accessor
    (lambda (rtd proc)
      (define accessor-error
        (lambda (x rtd)
          ($oops 'generated-condition-accessor
            "~s is not a condition of the type represented by ~s"
            x rtd)))
      (check-&condition-subtype! who rtd)
      (rec generated-condition-accessor
        (lambda (x)
          (cond
            [(record? x rtd) (proc x)]
            [(compound-condition? x)
             (let f ([ls (compound-condition-components x)])
               (if (null? ls)
                   (accessor-error x rtd)
                   (let ([x (car ls)])
                     (if (record? x rtd)
                         (proc x)
                         (f (cdr ls))))))]
            [else (accessor-error x rtd)]))))))

(define-syntax define-condition-type
  (lambda (x)
    (syntax-case x ()
      [(_ type-name super-type constructor predicate? (field-name accessor) ...)
       (with-syntax ([($accessor ...) (generate-temporaries #'(accessor ...))]
                     [msg (format "~~s is not a condition of type ~a" (datum type-name))])
         #'(begin
             (define-record-type (type-name constructor $predicate?)
               (nongenerative)
               (parent super-type)
               (fields (immutable field-name $accessor) ...))
             (define predicate?
               (lambda (x)
                 (or ($predicate? x)
                     (and ($compound-condition? x)
                          (ormap $predicate? ($compound-condition-components x))))))
             (define accessor
               (lambda (x)
                 (define accessor-error (lambda (x) ($oops 'accessor msg x)))
                 (cond
                   [($predicate? x) ($accessor x)]
                   [($compound-condition? x)
                    (let f ([ls ($compound-condition-components x)])
                      (if (null? ls)
                          (accessor-error x)
                          (let ([x (car ls)])
                            (if ($predicate? x)
                                ($accessor x)
                                (f (cdr ls))))))]
                   [else (accessor-error x)])))
             ...))])))

(eval-when (compile)
(define-syntax define-system-condition-type
  (lambda (x)
    (syntax-case x ()
      [(_ type-name super-type uid constructor predicate? (field-name accessor) ...)
       (with-syntax ([($accessor ...) (generate-temporaries #'(accessor ...))]
                     [msg (format "~~s is not a condition of type ~a" (datum type-name))])
         #'(begin
             (define-record-type (type-name constructor $predicate?)
               (nongenerative uid)
               (parent super-type)
               (fields (immutable field-name $accessor) ...))
             (define predicate?
               (lambda (x)
                 (or ($predicate? x)
                     (and ($compound-condition? x)
                          (ormap $predicate? ($compound-condition-components x))))))
             (define accessor
               (lambda (x)
                 (define accessor-error (lambda (x) ($oops 'accessor msg x)))
                 (cond
                   [($predicate? x) ($accessor x)]
                   [($compound-condition? x)
                    (let f ([ls ($compound-condition-components x)])
                      (if (null? ls)
                          (accessor-error x)
                          (let ([x (car ls)])
                            (if ($predicate? x)
                                ($accessor x)
                                (f (cdr ls))))))]
                   [else (accessor-error x)])))
             ...))])))
)

;;; standard condition types

;;; taking advantage of body-like semantics of begin to arrange for each
;;; condition type's compile-time information to be available for use in
;;; defining its child types, even though the system is compiled with
;;; (eval-syntax-expanders-when) not including compile.
(begin
(let-syntax ([a (syntax-rules () 
                  [(_ &condition) ; leave only &condition visible
                   (define-record-type (&condition make-simple-condition simple-condition?)
                     (nongenerative #{&condition oyb459ue1fphfx4-a}))])])
  (a &condition))

(define-system-condition-type &message &condition #{&message bwptyckgidgnsihx-a}
  make-message-condition message-condition?
  (message condition-message))

(define-system-condition-type &warning &condition #{&warning bwtai41dgaww3fus-a}
  make-warning warning?)

(define-system-condition-type &serious &condition #{&serious bwvzuvr26s58u3l9-a}
  make-serious-condition serious-condition?)

(define-system-condition-type &error &serious #{&error bwyo6misxbfkmrdg-a}
  make-error error?)

(define-system-condition-type &violation &serious #{&violation bw1eic9intowee4m-a}
  make-violation violation?)

(define-system-condition-type &assertion &violation #{&assertion bw33t3z8ebx752vs-a}
  make-assertion-violation assertion-violation?)

(define-system-condition-type &irritants &condition #{&irritants bw6s5uqx4t7jxqmy-a}
  make-irritants-condition irritants-condition?
  (irritants condition-irritants))

(define-system-condition-type &who &condition #{&who bw9ihlhnvcgvped6-a}
  make-who-condition who-condition?
  (who condition-who))

(define-system-condition-type &non-continuable &violation #{&non-continuable bxb7tb8dlup7g15e-a}
  make-non-continuable-violation
  non-continuable-violation?)

(define-system-condition-type &implementation-restriction &violation #{&implementation-restriction bxew42y3cczi8pwl-a}
  make-implementation-restriction-violation
  implementation-restriction-violation?)

(define-system-condition-type &lexical &violation #{&lexical bxhmgtps2u8u0dns-a}
  make-lexical-violation lexical-violation?)

(define-system-condition-type &syntax &violation #{&syntax bxkbskgitdh6r1ey-a}
  make-syntax-violation syntax-violation?
  (form syntax-violation-form)
  (subform syntax-violation-subform))

(define-system-condition-type &undefined &violation #{&undefined bxm04a68jvrijo54-a}
  make-undefined-violation undefined-violation?)

;;; io conditions

(define-system-condition-type &i/o &error #{&i/o bxpqf1xyad0ubcxc-a}
  make-i/o-error i/o-error?)

(define-system-condition-type &i/o-read &i/o #{&i/o-read bxsfrson0v9520oj-a}
  make-i/o-read-error i/o-read-error?)

(define-system-condition-type &i/o-write &i/o #{&i/o-write bxu43jfdrejhuofp-a}
  make-i/o-write-error i/o-write-error?)

(define-system-condition-type &i/o-invalid-position &i/o #{&i/o-invalid-position bxxue953hwstmb6v-a}
  make-i/o-invalid-position-error
  i/o-invalid-position-error?
  (position i/o-error-position))

(define-system-condition-type &i/o-filename &i/o #{&i/o-filename bx0jq0ws8e15dzx4-a}
  make-i/o-filename-error i/o-filename-error?
  (filename i/o-error-filename))

(define-system-condition-type &i/o-file-protection &i/o-filename #{&i/o-file-protection bx282rniyxbg5npc-a}
  make-i/o-file-protection-error
  i/o-file-protection-error?)

(define-system-condition-type &i/o-file-is-read-only &i/o-file-protection #{&i/o-file-is-read-only bx5yeid8pfksxbgj-a}
  make-i/o-file-is-read-only-error
  i/o-file-is-read-only-error?)

(define-system-condition-type &i/o-file-already-exists &i/o-filename #{&i/o-file-already-exists bx8np84yfxt4oy7q-a}
  make-i/o-file-already-exists-error
  i/o-file-already-exists-error?)

(define-system-condition-type &i/o-file-does-not-exist &i/o-filename #{&i/o-file-does-not-exist bybc1zvn6f3ggmyw-a}
  make-i/o-file-does-not-exist-error
  i/o-file-does-not-exist-error?)

(define-system-condition-type &i/o-port &i/o #{&i/o-port byd2dqmdwycr8ap5-a}
  make-i/o-port-error i/o-port-error?
  (pobj i/o-error-port))

(define-system-condition-type &i/o-decoding &i/o-port #{&i/o-decoding bygrphc3ngl3zyhc-a}
  make-i/o-decoding-error i/o-decoding-error?)

(define-system-condition-type &i/o-encoding &i/o-port #{&i/o-encoding byjg073tdyvfrl8i-a}
  make-i/o-encoding-error i/o-encoding-error?
  (cobj i/o-encoding-error-char))

;;; arithmetic conditions

(define-system-condition-type &no-infinities &implementation-restriction #{&no-infinities byl6cyui4g4ri9zq-a}
  make-no-infinities-violation
  no-infinities-violation?)

(define-system-condition-type &no-nans &implementation-restriction #{&no-nans byovopk8uzd3axqx-a}
  make-no-nans-violation no-nans-violation?)

;;; Chez Scheme conditions

(define-system-condition-type &source &condition #{&source byrk0gbylhne2lh4-a}
  make-source-condition source-condition?
  (form source-condition-form))

(define-system-condition-type $&src &condition #{$&src byul0m8re6e47nnb-a}
  $make-src-condition $src-condition?
  (src $src-condition-src)
  (start $src-condition-start))

(define-system-condition-type &format &condition #{&format byxbcdzg5oogzbei-a}
  make-format-condition format-condition?)

(define-system-condition-type &continuation &condition #{&continuation dxr8vukkubd1tr8-a}
  make-continuation-condition continuation-condition?
  (k condition-continuation))

(define-system-condition-type $&recompile &error #{&recompile eb5ipy47b8hscnlzoga59k-0}
  $make-recompile-condition $recompile-condition?
  (importer-path $recompile-importer-path))
)

(let ()
  (define avcond (make-assertion-violation))
  (define econd (make-error))
  (define wcond (make-warning))
  (define fcond (make-format-condition))
  (define favcond (condition avcond fcond))
  (define fecond (condition econd fcond))
  (define fwcond (condition wcond fcond))
  (define ircond (make-implementation-restriction-violation))
  (define fimpcond (condition ircond fcond))
  (define flexcond (condition (make-lexical-violation) (make-i/o-read-error) fcond))
  (define flexcond/ir (condition ircond (make-lexical-violation) (make-i/o-read-error) fcond))

  (define (error-help warning? who whoarg message irritants basecond)
    (unless (or (eq? whoarg #f) (string? whoarg) (symbol? whoarg))
      ($oops who "invalid who argument ~s (message = ~s, irritants = ~s)" whoarg message irritants))
    (unless (string? message)
      ($oops who "invalid message argument ~s (who = ~s, irritants = ~s)" message whoarg irritants))
    (let ([c (if whoarg
                 (if irritants
                     (condition basecond
                       (make-who-condition whoarg)
                       (make-message-condition message)
                       (make-irritants-condition irritants))
                     (condition basecond
                       (make-who-condition whoarg)
                       (make-message-condition message)))
                 (if irritants
                     (condition basecond
                       (make-message-condition message)
                       (make-irritants-condition irritants))
                     (condition basecond
                       (make-message-condition message))))])
      (if warning?
          (raise-continuable c)
          (call/cc
            (lambda (k)
              (raise (condition c (make-continuation-condition k))))))))

  (set-who! assertion-violation
    (lambda (whoarg message . irritants)
      (error-help #f who whoarg message irritants avcond)))

  (set-who! assertion-violationf
    (lambda (whoarg message . irritants)
      (error-help #f who whoarg message irritants favcond)))

  (set-who! $oops
    (lambda (whoarg message . irritants)
      (error-help #f who whoarg message irritants favcond)))

  (set-who! $oops/c
    (lambda (whoarg basecond message . irritants)
      (error-help #f who whoarg message irritants
        (condition basecond fcond))))

  (set-who! $impoops
    (lambda (whoarg message . irritants)
      (error-help #f who whoarg message irritants fimpcond)))

  (set-who! $record-oops
    (lambda (whoarg nonrec rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record-type descriptor" rtd))
      (when (record? nonrec rtd)
        ($oops who "~s actually is of type ~s" nonrec rtd))
      (error-help #f who whoarg "~s is not of type ~s" (list nonrec rtd) favcond)))

  (set-who! error
    (lambda (whoarg message . irritants)
      (error-help #f who whoarg message irritants econd)))

  (set-who! errorf
    (lambda (whoarg message . irritants)
      (error-help #f who whoarg message irritants fecond)))

  (set-who! warning
    (lambda (whoarg message . irritants)
      (error-help #t who whoarg message irritants wcond)))

  (set-who! warningf
    (lambda (whoarg message . irritants)
      (error-help #t who whoarg message irritants fwcond)))

  (let ()
    (define (infer-who form)
      (syntax-case form ()
        [id (identifier? #'id) (datum id)]
        [(id . stuff) (identifier? #'id) (datum id)]
        [_ #f]))
    (set-who! syntax-violation
      (case-lambda
        [(whoarg message form)
         (error-help #f who (or whoarg (infer-who form)) message #f
           (condition avcond (make-syntax-violation form #f)))]
        [(whoarg message form subform)
         (error-help #f who (or whoarg (infer-who form)) message #f
           (make-syntax-violation form subform))])))

  (set-who! syntax-error
    (lambda (form . messages)
      (for-each
        (lambda (m) (unless (string? m) ($oops who "~s is not a string" m)))
        messages)
      (error-help #f who #f 
        (if (null? messages) "invalid syntax" (apply string-append messages))
        #f (make-syntax-violation form #f))))

  (set-who! $undefined-violation
    (lambda (id message)
      (error-help #f who #f message #f
        (condition (make-undefined-violation) (make-syntax-violation id #f)))))

  (set-who! $unknown-undefined-violation
    (lambda ()
      (error-help #f who #f "undefined" #f
        (make-undefined-violation))))

  (set-who! $lexical-error
    (case-lambda
      [(whoarg msg args port ir?)
       (error-help #f who whoarg msg args
         (condition
           (make-i/o-port-error port)
           (if ir? flexcond/ir flexcond)))]
      [(whoarg msg args port src start? ir?)
       (error-help #f who whoarg msg args
         (condition
           (make-i/o-port-error port)
           (if ir? flexcond/ir flexcond)
           ($make-src-condition src start?)))]))

  (set-who! $source-violation
    (lambda (whoarg src start? msg . args)
      (error-help #f who whoarg msg args
        (if src
            (condition favcond ($make-src-condition src start?))
            favcond))))

  (set-who! $source-warning
    (lambda (whoarg src start? msg . args)
      (error-help #t who whoarg msg args
        (if src
            (condition fwcond ($make-src-condition src start?))
            fwcond))))
)
)
