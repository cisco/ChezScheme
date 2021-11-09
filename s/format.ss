;;; format.ss
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

#| TODO:
 * more tests
    - tests of format with #f, #t, or port as first argument; test of printf
      and fprintf, tests that exercise all paths of cp1in format handler
    - verify complete coverage of code paths
    - extract all tests from cltl2
    - more # and v parameter tests
    - ~^ tests:
      - need tests for outside abort, abort in indirect, nested {super-,}abort
        in conditionals, nested {super-,}abort in case-conversion, etc.
      - need tests with one parameter and two parameters
    - ~* and ~:p tests for moving around loop args
    - test float printing with Bob's set of floats
 * use something better than string-append for constructing ~f and ~e output
 * use more efficient dispatch, e.g., have case use binary search for fixnum
   keys; or modify compiler to use jump tables for well-behaved case's
 * look into not hardcoding float-base = 10
 * vparams adds substantial allocation overhead, probably because of the
   compiler's handling of mvlet producers containing if expressions; fix
   the compiler
 * abstract out Chez Scheme specifics, like display-string, $list-length,
   string ports, use of generic port
|#

;;; missing directives
;;;  pretty-printer controls (^_, ~:>, ~i, ~:t ~/name/)

;;; known incompatibilities with Common Lisp
;;;  : [print nil as ()] modifier ignored for ~a
;;;  : [print nil as ()] modifier treated as "print-gensym #f" for ~s
;;;  common lisp doesn't complain when there are unused arguments,
;;;    may not complain when there are too few arguments.  we always
;;;    complain when there are too few and complain when we can determine
;;;    statically that there are too many
;;;  we insist on real argument for ~f, ~e, and ~g; common lisp is
;;;    lax and sends off anything else to ~D.

;;; other notees
;;;  we always assume that format starts at the beginning of a line
;;;    in support of ~&, ~t, and ~<...>

(let ()
  ;;; configuration

  ;; check for too many args at parse time
  (define static-too-many-args-check #t)
  ;; check for too many args at parse time for indirects and loop bodies
  (define indirect-too-many-args-check #f)
  ;; check for too many args at run time.  the check is always suppressed
  ;; when we terminate a format or indirect format as the result of ~^
  (define dynamic-too-many-args-check #f)

  ;;; predicates used to check format parameters
  (define nnfixnum? (lambda (x) (and (fixnum? x) (fx>= x 0))))
  (define true? (lambda (x) #t))
  (define pfixnum? (lambda (x) (and (fixnum? x) (fx> x 0))))
  (define radix? (lambda (x) (and (fixnum? x) (fx<= 2 x 36))))

 ; we require nongenerative records because the compiler embeds parsed
 ; format strings in object files.  force cp1in-parse-format to return #f
 ; to bootstrap after making changes to any of these records
  (define-datatype (#{fmt cgos0c9ufi1rq-fd} (immutable directive))
    (#{newline cgos0c9ufi1rq-ez} n)
    (#{fresh-line cgos0c9ufi1rq-fc} n)
    (#{dup-char cgos0c9ufi1rq-fh} n c)
    (#{display cgos0c9ufi1rq-fi} mincol colinc minpad pad-char left?)
    (#{simple-display cgos0c9ufi1rq-et})
    (#{simple-write cgos0c9ufi1rq-es})
    (#{write cgos0c9ufi1rq-ei} mincol colinc minpad pad-char nogensym? left?)
    (#{cwrite cgos0c9ufi1rq-fk} colon? at?)
    (#{fwrite cgos0c9ufi1rq-fb} w d k oc pc sign?)
    (#{ewrite cgos0c9ufi1rq-ff} w d ew k oc pc ec sign?)
    (#{gwrite cgos0c9ufi1rq-e9} w d ew k oc pc ec sign?)
    (#{$write cgos0c9ufi1rq-eg} d n w pc sign-before-pad? sign?)
    (#{write-radix cgos0c9ufi1rq-eh} base w pc cc ci sign? commas?)
    (#{plural cgos0c9ufi1rq-ey} back-up? y/ies?)
    (#{fancy-radix cgos0c9ufi1rq-fe} colon? at?)
    (#{indirect cgos0c9ufi1rq-e6} splice?)
    (#{goto cgos0c9ufi1rq-fa} n reverse? absolute?)
    (#{tabulate cgos0c9ufi1rq-ek} colnum colinc relative?)
    (#{convert-case cgos0c9ufi1rq-fl} nested-cmd* colon? at?)
    (#{conditional cgos0c9ufi1rq-fo} n cases default)
    (#{conditional/at cgos0c9ufi1rq-fn} consequent)
    (#{conditional/colon cgos0c9ufi1rq-fm} alternative consequent)
    (#{justify cgos0c9ufi1rq-e1} mincol colinc minpad pad-char before? after? initial margin columns segments)
    (#{abort cgos0c9ufi1rq-ft} n m super?)
    (#{iteration cgos0c9ufi1rq-e2} body n sublists? use-remaining? at-least-once?)
    (#{columntrack cgos0c9ufi1rq-fq} body)
  )

  ;;; parse string to list of strings, chars, and fmt records
  (define parse
    (lambda (who cntl)
      (define column? #f)
      (define-syntactic-monad state nargs cmd* stack)
      (define-record-type frame
        (fields (immutable directive) (immutable cmd*))
        (nongenerative))
      (define-record-type cvtcase-frame
        (parent frame)
        (fields (immutable colon?) (immutable at?))
        (nongenerative)
        (sealed #t))
      (define-record-type conditional/at-frame
        (parent frame)
        (nongenerative)
        (sealed #t))
      (define-record-type conditional/colon-frame
        (parent frame)
        (fields (mutable altern))
        (nongenerative)
        (sealed #t)
        (protocol
          (lambda (make-new)
            (lambda (directive cmd*)
              ((make-new directive cmd*) #f)))))
      (define-record-type conditional-frame
        (parent frame)
        (fields (immutable n) (mutable cases) (mutable default?))
        (nongenerative)
        (sealed #t)
        (protocol
          (lambda (make-new)
            (lambda (directive cmd* n)
              ((make-new directive cmd*) n '() #f)))))
      (define-record-type justify-frame
        (parent frame)
        (fields
          (immutable mincol)
          (immutable colinc)
          (immutable minpad)
          (immutable pc)
          (immutable before?)
          (immutable after?)
          (mutable segments)
          (mutable initial)
          (mutable margin)
          (mutable columns))
        (nongenerative)
        (sealed #t)
        (protocol
          (lambda (make-new)
            (lambda (directive cmd* mincol colinc minpad pc before? after?)
              ((make-new directive cmd*) mincol colinc minpad pc before? after? '() #f #f #f)))))
      (define-record-type iteration-frame
        (parent frame)
        (fields (immutable n) (immutable sublists?) (immutable use-remaining?))
        (nongenerative)
        (sealed #t))
      (define incomplete-format-directive
        (lambda (b i)
          ($oops who "incomplete format directive ~s"
            (substring cntl b i))))
      (define (bump x n) (and x n (fx+ x n)))
      (unless (string? cntl)
        ($oops who "~s is not a string" cntl))
      (let ([nmax (fx- (string-length cntl) 1)])
        (define char
          (lambda (i)
            (if (fx> i nmax)
                #!eof
                (string-ref cntl i))))
        (define sfinal
          (state lambda ()
            (unless (null? stack)
              ($oops who "unclosed directive ~a" (frame-directive (car stack))))
            (let ([cmd* (reverse cmd*)])
              (values (if column? (list (fmt-columntrack "" cmd*)) cmd*) nargs))))
        (define s0
          (state lambda (i)
            (let ([c (char i)])
              (state-case c
                [eof (state sfinal ())]
                [(#\~) (state s3 () (fx+ i 1) i)]
                [else (state s1 () (fx+ i 1) i c)]))))
        (define s1
          (state lambda (i b c0)
            (let ([c (char i)])
              (state-case c
                [eof (state sfinal ([cmd* (cons c0 cmd*)]))]
                [(#\~) (state s3 ([cmd* (cons c0 cmd*)]) (fx+ i 1) i)]
                [else (state s2 () (fx+ i 1) b)]))))
        (define s2
          (state lambda (i b)
            (let ([c (char i)])
              (state-case c
                [eof (state sfinal ([cmd* (cons (substring cntl b i) cmd*)]))]
                [(#\~) (state s3 ([cmd* (cons (substring cntl b i) cmd*)]) (fx+ i 1) i)]
                [else (state s2 () (fx+ i 1) b)]))))
        (define s3
          (state lambda (i b)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [(#\~) (state s1 () (fx+ i 1) i #\~)]
                [(#\- #\+) (state s4-sign () (fx+ i 1) b '() i)]
                [((#\0 - #\9)) (state s4-digit () (fx+ i 1) b '() i)]
                [(#\,) (state s4-comma () (fx+ i 1) b '(#f))]
                [(#\') (state s4-quote () (fx+ i 1) b '())]
                [(#\#) (state s4-after-param () (fx+ i 1) b '(hash))]
                [(#\v) (state s4-after-param ([nargs (bump nargs 1)]) (fx+ i 1) b '(v))]
                [else (state s5 () i b '())]))))
        (define s4-sign
          (state lambda (i b p* bp)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [((#\0 - #\9)) (state s4-digit () (fx+ i 1) b p* bp)]
                [else (incomplete-format-directive b i)]))))
        (define s4-quote
          (state lambda (i b p*)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [else (state s4-after-param () (fx+ i 1) b (cons c p*))]))))
        (define s4-after-param
          (state lambda (i b p*)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [(#\,) (state s4-comma () (fx+ i 1) b p*)]
                [else (state s5 () i b (reverse p*))]))))
        (define s4-digit
          (state lambda (i b p* bp)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [((#\0 - #\9)) (state s4-digit () (fx+ i 1) b p* bp)]
                [(#\,) (state s4-comma () (fx+ i 1) b (cons (string->number (substring cntl bp i)) p*))]
                [else (state s5 () i b (reverse (cons (string->number (substring cntl bp i)) p*)))]))))
        (define s4-comma
          (state lambda (i b p*)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [(#\- #\+) (state s4-sign () (fx+ i 1) b p* i)]
                [((#\0 - #\9)) (state s4-digit () (fx+ i 1) b p* i)]
                [(#\,) (state s4-comma () (fx+ i 1) b (cons #f p*))]
                [(#\') (state s4-quote () (fx+ i 1) b p*)]
                [(#\#) (state s4-after-param () (fx+ i 1) b (cons 'hash p*))]
                [(#\v) (state s4-after-param ([nargs (bump nargs 1)]) (fx+ i 1) b (cons 'v p*))]
                [else (state s5 () i b (reverse (cons #f p*)))]))))
        (define s5
          (state lambda (i b p*)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [(#\:) (state s5-colon () (fx+ i 1) b p*)]
                [(#\@) (state s5-at () (fx+ i 1) b p*)]
                [else (state s6 () i b p* #f #f)]))))
        (define s5-colon
          (state lambda (i b p*)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [(#\@) (state s6 () (fx+ i 1) b p* #t #t)]
                [else (state s6 () i b p* #t #f)]))))
        (define s5-at
          (state lambda (i b p*)
            (let ([c (char i)])
              (state-case c
                [eof (incomplete-format-directive b i)]
                [(#\:) (state s6 () (fx+ i 1) b p* #t #t)]
                [else (state s6 () i b p* #f #t)]))))
        (define s6
          (state lambda (i b p* colon? at?)
            (define skip-non-newline-white
              (lambda (i)
                (let ([c (char i)])
                  (state-case c
                    [eof i]
                    [(#\space #\tab #\page #\return)
                     (skip-non-newline-white (fx+ i 1))]
                    [else i]))))
            (let ([c (char i)])
              (define no-colon
                (lambda ()
                  (when colon?
                    ($oops who "~~~c directive has no : flag" c))))
              (define no-at
                (lambda ()
                  (when at?
                    ($oops who "~~~c directive has no @ flag" c))))
              (define too-many-parameters
                (lambda ()
                  ($oops who
                    "too many parameters in ~~~c directive ~s"
                    c (substring cntl b (fx+ i 1)))))
              (define missing-parameter
                (lambda (what)
                  ($oops who
                    "missing required ~s parameter in ~~~c directive ~s"
                    what c (substring cntl b (fx+ i 1)))))
              (define invalid-parameter
                (lambda (what arg)
                  ($oops who
                    "invalid ~s parameter ~a in ~~~c directive ~s"
                    what arg c (substring cntl b (fx+ i 1)))))
              (define misplaced-directive
                (lambda ()
                  ($oops who "misplaced directive ~s"
                    (substring cntl b (fx+ i 1)))))
              (define-syntax parameters
                (lambda (x)
                  (define process-param
                    (lambda (t* param* body)
                      (if (null? param*)
                          body
                          (with-syntax ([body (process-param (cdr t*) (cdr param*) body)]
                                        [t (car t*)])
                            (syntax-case (car param*) (implicit)
                              [(implicit e) #'(let ([t e]) body)]
                              [(type? p)
                               #'(begin
                                   (when (null? p*) (missing-parameter 'p))
                                   (let ([t (car p*)] [p* (cdr p*)])
                                     (when (not t) (missing-parameter 'p))
                                     (unless (or (type? t) (memq t '(hash v))) (invalid-parameter 'p t))
                                     body))]
                              [(type? p default)
                               #'(let ([proc (lambda (t p*) body)])
                                   (if (null? p*)
                                       (proc 'default p*)
                                       (let ([t (car p*)] [p* (cdr p*)])
                                         (if (not t)
                                             (proc default p*)
                                             (begin
                                               (unless (or (type? t) (memq t '(hash v))) (invalid-parameter 'p t))
                                               (proc t p*))))))])))))
                  (syntax-case x ()
                    [(_ ([t param] ...) e1 e2 ...)
                     (process-param
                       #'(t ...)
                       #'(param ...)
                       #'(begin
                           (unless (null? p*) (too-many-parameters))
                           (let () e1 e2 ...)))])))
              (define-syntax directive
                (lambda (x)
                  (define construct-name
                    (lambda (template-identifier . args)
                      (datum->syntax
                        template-identifier
                        (string->symbol
                          (apply string-append
                                 (map (lambda (x)
                                        (if (string? x)
                                            x
                                            (symbol->string (syntax->datum x))))
                                      args))))))
                  (syntax-case x ()
                    [(k (d param ...) n)
                     (with-syntax ([(t ...) (generate-temporaries #'(param ...))]
                                   [fmt-d (construct-name #'d "fmt-" #'d)])
                       (with-implicit (k state cmd* nargs)
                         #'(parameters ([t param] ...)
                             (state s0
                               ([cmd* (cons (fmt-d (substring cntl b (fx+ i 1)) t ...) cmd*)]
                                [nargs (bump nargs n)])
                               (fx+ i 1)))))])))
              (define-syntax parse-radix
                (syntax-rules ()
                  [(_ base)
                   (directive
                     (write-radix [implicit base]
                                  [nnfixnum? w #f]
                                  [char? pad-char #\space]
                                  [char? comma-char #\,]
                                  [pfixnum? comma-interval 3]
                                  [implicit at?]
                                  [implicit colon?])
                     1)]))
              (state-case c
                [eof (incomplete-format-directive b i)]
                [(#\% #\n #\N)
                 (no-at)
                 (no-colon)
                 (if (or (null? p*) (equal? p* '(1)))
                     (state s0 ([cmd* (cons #\newline cmd*)]) (fx+ i 1))
                     (directive (dup-char [nnfixnum? n 1] [implicit #\newline]) 0))]
                [(#\&)
                 (no-at)
                 (no-colon)
                 (directive (fresh-line [nnfixnum? n 1]) 0)]
                [(#\a #\A)
                 (no-colon)
                 (if (null? p*)
                     (directive
                       (simple-display)
                       1)
                     (directive
                       (display [nnfixnum? mincol 0]
                                [pfixnum? colinc 1]
                                [nnfixnum? minpad 0]
                                [char? pad-char #\space]
                                [implicit at?])
                       1))]
                [(#\s #\S #\w #\W)
                 (if (and (null? p*) (not colon?))
                     (directive
                       (simple-write)
                       1)
                     (directive
                       (write [nnfixnum? mincol 0]
                              [pfixnum? colinc 1]
                              [nnfixnum? minpad 0]
                              [char? pad-char #\space]
                              [implicit colon?]
                              [implicit at?])
                       1))]
                [(#\f #\F)
                 (no-colon)
                 (directive
                   (fwrite [nnfixnum? w #f]
                           [nnfixnum? d #f]
                           [fixnum? k 0]
                           [char? overflow-char #f]
                           [char? pad-char #\space]
                           [implicit at?])
                   1)]
                [(#\e #\E)
                 (no-colon)
                 (directive
                   (ewrite [nnfixnum? w #f]
                           [nnfixnum? d #f]
                           [pfixnum? e #f]
                           [fixnum? k 1]
                           [char? overflow-char #f]
                           [char? pad-char #\space]
                           [char? exponent-char #\e]
                           [implicit at?])
                   1)]
                [(#\g #\G)
                 (no-colon)
                 (directive
                   (gwrite [nnfixnum? w #f]
                           [nnfixnum? d #f]
                           [pfixnum? e #f]
                           [fixnum? k 1]            ; assumption
                           [char? overflow-char #f]
                           [char? pad-char #\space]
                           [char? exponent-char #\e]
                           [implicit at?])
                   1)]
                [(#\$)
                 (directive
                   ($write [nnfixnum? d 2]
                           [nnfixnum? n 1]
                           [nnfixnum? w 0]
                           [char? pad-char #\space]
                           [implicit colon?]
                           [implicit at?])
                   1)]
                [(#\c #\C)
                 (directive
                   (cwrite [implicit colon?] [implicit at?])
                   1)]
                [(#\b #\B) (parse-radix 2)]
                [(#\o #\O) (parse-radix 8)]
                [(#\d #\D) (parse-radix 10)]
                [(#\x #\X) (parse-radix 16)]
                [(#\r #\R)
                 (if (null? p*)
                     (directive
                       (fancy-radix [implicit colon?] [implicit at?])
                       1)
                     (directive
                       (write-radix [radix? n 10]
                                    [nnfixnum? w #f]
                                    [char? pad-char #\space]
                                    [char? comma-char #\,]
                                    [pfixnum? comma-interval 3]
                                    [implicit at?]
                                    [implicit colon?])
                       1))]
                [(#\p #\P)
                 (directive
                   (plural [implicit colon?] [implicit at?])
                   (if colon? 0 1))]
                [(#\t #\T)
                 (no-colon)
                 (set! column? #t)
                 (directive
                   (tabulate [nnfixnum? colnum 1]
                             [nnfixnum? colinc 1]
                             [implicit at?])
                   0)]
                [(#\?)
                 (no-colon)
                 (set! column? #t)
                 (directive
                   (indirect [implicit at?])
                   (if at? #f 2))]
                [(#\*)
                 (when (and colon? at?)
                   ($oops who
                     "@ and : modifiers are mutually exclusive for format directive ~~~c"
                     c))
                 (directive
                   (goto [nnfixnum? n #f] [implicit colon?] [implicit at?])
                   #f)]
                [(#\( #|)|#)
                 (parameters ()
                   (state s0
                     ([stack (cons (make-cvtcase-frame (substring cntl b (fx+ i 1)) cmd* colon? at?) stack)]
                      [cmd* '()])
                     (fx+ i 1)))]
                [(#|(|# #\))
                 (no-at)
                 (no-colon)
                 (let ([x (and (not (null? stack)) (car stack))])
                   (unless (cvtcase-frame? x) (misplaced-directive))
                   (let ([nested-cmd* (reverse cmd*)])
                     (let ([stack (cdr stack)] [cmd* (frame-cmd* x)])
                       (directive
                         (convert-case [implicit nested-cmd*]
                                       [implicit (cvtcase-frame-colon? x)]
                                       [implicit (cvtcase-frame-at? x)])
                         0))))]
                [(#\;)
                 (no-at)
                 (let ([x (and (not (null? stack)) (car stack))])
                   (cond
                     [(and (conditional/colon-frame? x)
                           (not colon?)
                           (not (conditional/colon-frame-altern x)))
                      (parameters ()
                        (conditional/colon-frame-altern-set! x (reverse cmd*)))
                      (state s0 ([cmd* '()]) (fx+ i 1))]
                     [(and (conditional-frame? x) (not (conditional-frame-default? x)))
                      (parameters ()
                        (when colon? (conditional-frame-default?-set! x #t))
                        (conditional-frame-cases-set! x
                          (cons (reverse cmd*) (conditional-frame-cases x))))
                      (state s0 ([cmd* '()]) (fx+ i 1))]
                     [(and (justify-frame? x)
                           (or (not colon?)
                               (and (not (justify-frame-initial x))
                                    (null? (justify-frame-segments x)))))
                      (if colon?
                          (parameters ([margin (nnfixnum? n 0)]
                                       [cols (nnfixnum? lw 72)])
                            (set! column? #t)
                            (justify-frame-initial-set! x (reverse cmd*))
                            (justify-frame-margin-set! x margin)
                            (justify-frame-columns-set! x cols))
                          (parameters ()
                            (justify-frame-segments-set! x
                              (cons (reverse cmd*) (justify-frame-segments x)))))
                      (state s0 ([cmd* '()]) (fx+ i 1))]
                     [else (misplaced-directive)]))]
                [(#\^)
                 (no-at)
                 (directive
                   (abort [true? n #f] [true? m #f] [implicit colon?])
                   #f)]
                [(#\{ #|}|#)
                 (when (null? cmd*) (set! column? #t))
                 (parameters ([n (nnfixnum? n #f)])
                   (state s0
                     ([stack (cons (make-iteration-frame (substring cntl b (fx+ i 1)) cmd* n colon? at?) stack)]
                      [cmd* '()])
                     (fx+ i 1)))]
                [(#|{|# #\})
                 (no-at)
                 (let ([x (and (not (null? stack)) (car stack))])
                   (unless (iteration-frame? x) (misplaced-directive))
                   (let ([nested-cmd* (reverse cmd*)])
                     (let ([stack (cdr stack)] [cmd* (frame-cmd* x)])
                       (directive
                         (iteration [implicit nested-cmd*]
                                    [implicit (iteration-frame-n x)]
                                    [implicit (iteration-frame-sublists? x)]
                                    [implicit (iteration-frame-use-remaining? x)]
                                    [implicit colon?])
                         #f))))]
                [(#\[ #|]|#)
                 (if at?
                     (if colon?
                         ($oops who "@ and : modifiers are mutually exclusive for format directive ~~~c" c)
                         (parameters ()
                           (state s0
                             ([stack (cons (make-conditional/at-frame (substring cntl b (fx+ i 1)) cmd*) stack)]
                              [cmd* '()])
                             (fx+ i 1))))
                     (if colon?
                         (parameters ()
                           (state s0
                             ([stack (cons (make-conditional/colon-frame (substring cntl b (fx+ i 1)) cmd*) stack)]
                              [cmd* '()])
                             (fx+ i 1)))
                         (parameters ([n (nnfixnum? n #f)])
                           (state s0
                             ([stack (cons (make-conditional-frame (substring cntl b (fx+ i 1)) cmd* n) stack)]
                              [cmd* '()])
                             (fx+ i 1)))))]
                [(#|[|# #\])
                 (no-at)
                 (no-colon)
                 (let ([x (and (not (null? stack)) (car stack))])
                   (let ([nested-cmd* (reverse cmd*)])
                     (cond
                       [(conditional/at-frame? x)
                        (let ([stack (cdr stack)] [cmd* (frame-cmd* x)])
                          (directive
                            (conditional/at [implicit nested-cmd*])
                            #f))]
                       [(conditional/colon-frame? x)
                        (let ([altern (conditional/colon-frame-altern x)])
                          (unless altern
                            ($oops who "no ~~; found within ~a...~~]" (frame-directive (car stack))))
                          (let ([stack (cdr stack)] [cmd* (frame-cmd* x)])
                            (directive
                              (conditional/colon [implicit altern]
                                                 [implicit nested-cmd*])
                              #f)))]
                       [(conditional-frame? x)
                        (let ([stack (cdr stack)] [cmd* (frame-cmd* x)])
                          (let ([n (conditional-frame-n x)])
                            (if (conditional-frame-default? x)
                                (directive
                                  (conditional [implicit n]
                                               [implicit (list->vector (reverse (conditional-frame-cases x)))]
                                               [implicit nested-cmd*])
                                  #f)
                                (directive
                                  (conditional [implicit n]
                                               [implicit (list->vector (reverse (cons nested-cmd* (conditional-frame-cases x))))]
                                               [implicit '()])
                                  #f))))]
                       [else (misplaced-directive)])))]
                [(#\<)
                 (parameters ([mincol (nnfixnum? mincol 0)]
                              [colinc (nnfixnum? colinc 1)]
                              [minpad (nnfixnum? minpad 0)]
                              [pc (char? pad-char #\space)])
                   (state s0
                     ([stack (cons (make-justify-frame (substring cntl b (fx+ i 1)) cmd* mincol colinc minpad pc colon? at?) stack)]
                      [cmd* '()])
                     (fx+ i 1)))]
                [(#\>)
                 (no-at)
                 (let ([x (and (not (null? stack)) (car stack))])
                   (unless (justify-frame? x) (misplaced-directive))
                   (let ([nested-cmd* (reverse cmd*)])
                     (let ([stack (cdr stack)] [cmd* (frame-cmd* x)])
                       (directive
                         (justify [implicit (justify-frame-mincol x)]
                                  [implicit (justify-frame-colinc x)]
                                  [implicit (justify-frame-minpad x)]
                                  [implicit (justify-frame-pc x)]
                                  [implicit (justify-frame-before? x)]
                                  [implicit (justify-frame-after? x)]
                                  [implicit (justify-frame-initial x)]
                                  [implicit (justify-frame-margin x)]
                                  [implicit (justify-frame-columns x)]
                                  [implicit (reverse (cons nested-cmd* (justify-frame-segments x)))])
                         0))))]
                [(#\~)
                 (no-at)
                 (no-colon)
                 (if (or (null? p*) (equal? p* '(1)))
                     (state s0 ([cmd* (cons #\~ cmd*)]) (fx+ i 1))
                     (directive (dup-char [nnfixnum? n 1] [implicit #\~]) 0))]
                [(#\|)
                 (no-at)
                 (no-colon)
                 (if (or (null? p*) (equal? p* '(1)))
                     (state s0 ([cmd* (cons #\page cmd*)]) (fx+ i 1))
                     (directive (dup-char [nnfixnum? n 1] [implicit #\page]) 0))]
                [(#\return) ; ~\r\n is treated like ~\n
                 (if (eq? (char (fx+ i 1)) #\newline)
                     (state s6 () (fx+ i 1) b p* colon? at?)
                     ($oops who "unrecognized directive ~~~:c" c))]
                [(#\newline)
                 (parameters ()
                   (when (and colon? at?)
                     ($oops who
                       "@ and : modifiers are mutually exclusive for format directive ~~~c"
                       c))
                   (cond
                     [colon? (state s0 () (fx+ i 1))]
                     [at? (state s0 ([cmd* (cons c cmd*)]) (skip-non-newline-white (fx+ i 1)))]
                     [else (state s0 () (skip-non-newline-white (fx+ i 1)))]))]
                [else ($oops who "unrecognized directive ~~~:c" c)]))))
        (state s0 ([nargs 0] [cmd* '()] [stack '()]) 0))))

  ;;; squash together adjacent strings and characters
  (define squash
    (lambda (ls)
      (define insert-string!
        (lambda (s1 i1 s2)
          (let ([n2 (string-length s2)])
            (do ([i1 i1 (fx+ i1 1)] [i2 0 (fx+ i2 1)])
                ((fx= i2 n2))
              (string-set! s1 i1 (string-ref s2 i2))))))
      (define squash0
        (lambda (ls)
          (let ([a (car ls)] [d (cdr ls)])
            (if (null? d)
                ls
                (if (string? a)
                    (let-values ([(s d) (squash1 d (string-length a))])
                      (if (string? s)
                          (begin (insert-string! s 0 a) (cons s d))
                          (cons a d)))
                    (if (char? a)
                        (let-values ([(s d) (squash1 d 1)])
                          (if (string? s)
                              (begin (string-set! s 0 a) (cons s d))
                              (cons a d)))
                        (cons a (squash0 d))))))))
      (define squash1
        (lambda (ls n)
          (if (null? ls)
              (values n ls)
              (let ([a (car ls)] [d (cdr ls)])
                (if (string? a)
                    (let-values ([(s d) (squash1 d (fx+ n (string-length a)))])
                      (let ([s (if (string? s) s (make-string s))])
                        (insert-string! s n a)
                        (values s d)))
                    (if (char? a)
                        (let-values ([(s d) (squash1 d (fx+ n 1))])
                          (let ([s (if (string? s) s (make-string s))])
                            (string-set! s n a)
                            (values s d)))
                        (values n (if (null? d) ls (cons a (squash0 d))))))))))
      (if (null? ls) '() (squash0 ls))))

  ;;; convert simple formats to expressions.  returns #f for other inputs.
  (define (make-fmt->expr build-quote build-seq build-primcall)
    (lambda (src sexpr cmd* op arg*)
      (define-syntax make-seq
        (syntax-rules ()
          [(_ ?a ?d)
           (let ([d ?d])
             (and d
                  (let ([a ?a])
                    (if (null? d) a (build-seq a d)))))]))
      (define-syntax make-call
        (syntax-rules ()
          [(_ src proc arg ...)
           (build-primcall src sexpr 'proc (list arg ...))]))
      (if (null? cmd*)
          (build-quote (void))
          (let f ([cmd* cmd*] [arg* arg*] [src src])
            (if (null? cmd*)
                '()
                (let ([cmd (car cmd*)] [cmd* (cdr cmd*)])
                  (cond
                    [(string? cmd)
                     (make-seq (make-call src display-string (build-quote cmd) op)
                       (f cmd* arg* #f))]
                    [(char? cmd)
                     (make-seq (make-call src write-char (build-quote cmd) op)
                       (f cmd* arg* #f))]
                    [(fmt? cmd)
                     (and (not (null? arg*))
                          (fmt-case cmd
                            [simple-display ()
                              (make-seq (make-call src display (car arg*) op)
                                (f cmd* (cdr arg*) #f))]
                            [simple-write ()
                              (make-seq (make-call src write (car arg*) op)
                                (f cmd* (cdr arg*) #f))]
                            [cwrite (colon? at?)
                              (and (not colon?)
                                   (not at?)
                                   (make-seq (make-call src write-char (car arg*) op)
                                     (f cmd* (cdr arg*) #f)))]
                            [else #f]))]
                    [else ($oops 'fmt->expr "internal error: ~s" cmd)])))))))

  ;;; perform formatting operation from parsed string (cmd*)
  (define dofmt
    (lambda (who fmt-op cntl cmd* arg*)
      (define flonum->digits #%$flonum->digits)
      (define flonum-sign #%$flonum-sign)
      (define (exact-integer? x) (or (fixnum? x) (bignum? x)))
      (define float-base 10) ; hardcoding base 10 for now
      (define fd->string
        (lambda (ls d n sign?)
          (define flonum-digit->char
            (lambda (n)
              (string-ref
                "#00123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                (fx+ n 2))))
          (let ([s (car ls)] [e (cadr ls)] [ls (cddr ls)])
            (let ([op (open-output-string)])
              (if (eqv? s -1)
                  (write-char #\- op)
                  (when sign? (write-char #\+ op)))
              (cond
                [(fx< e 0)
                 (when (fx> n 0) (display (make-string n #\0) op))
                 (write-char #\. op)
                 (if (and (not d) (fx= (car ls) -1)) ; some flavor of 0.0
                     (write-char #\0 op)
                     (do ([e e (fx+ e 1)] [d d (and d (fx- d 1))])
                         ((or (fx>= e -1) (and d (fx= d 0)))
                          (do ([ls ls (cdr ls)] [d d (and d (fx- d 1))])
                              ((if d (fx= d 0) (fx< (car ls) 0)))
                            (write-char (flonum-digit->char (car ls)) op)))
                       (write-char #\0 op)))]
                [(fx= (car ls) -1) ; some flavor of 0.0
                 (display (make-string (if (and (fx= n 0) (eqv? d 0)) 1 n) #\0) op)
                 (write-char #\. op)
                 (display (make-string (or d 1) #\0) op)]
                [else
                 (let ([n (fx- n e 1)])
                   (when (fx> n 0) (display (make-string n #\0) op)))
                 (write-char (flonum-digit->char (car ls)) op)
                 (do ([ls (cdr ls) (cdr ls)] [e e (fx- e 1)])
                     ((fx= e 0)
                      (write-char #\. op)
                      (if (and (not d) (fx< (car ls) 0))
                          (write-char (flonum-digit->char (car ls)) op)
                          (do ([ls ls (cdr ls)] [d d (and d (fx- d 1))])
                              ((if d (fx= d 0) (fx< (car ls) 0)))
                            (write-char (flonum-digit->char (car ls)) op))))
                   (write-char (flonum-digit->char (car ls)) op))])
              (get-output-string op)))))
      (define string-upcase!
        (lambda (s)
          (let ([n (string-length s)])
            (do ([i 0 (fx+ i 1)])
                ((fx= i n))
              (string-set! s i (char-upcase (string-ref s i)))))))
      (define string-downcase!
        (lambda (s)
          (let ([n (string-length s)])
            (do ([i 0 (fx+ i 1)])
                ((fx= i n))
              (string-set! s i (char-downcase (string-ref s i)))))))
      (define string-capitalize!
        (lambda (s)
          (let ([n (string-length s)])
            (define interword
              (lambda (i)
                (unless (fx= i n)
                  (let ([c (string-ref s i)])
                    (if (or (char-alphabetic? c) (char-numeric? c))
                        (begin
                          (string-set! s i (char-upcase c))
                          (intraword (fx+ i 1)))
                        (interword (fx+ i 1)))))))
            (define intraword
              (lambda (i)
                (unless (fx= i n)
                  (let ([c (string-ref s i)])
                    (if (or (char-alphabetic? c) (char-numeric? c))
                        (begin
                          (string-set! s i (char-downcase c))
                          (intraword (fx+ i 1)))
                        (interword (fx+ i 1)))))))
            (interword 0))))
      (define string-capitalize-first!
        (lambda (s)
          (let ([n (string-length s)])
            (unless (fx= (string-length s) 0)
              (string-set! s 0 (char-upcase (string-ref s 0)))
              (do ([i 1 (fx+ i 1)])
                  ((fx= i n))
                (string-set! s i (char-downcase (string-ref s i))))))))
      (define-syntax pad
        (syntax-rules ()
          [(_ mincol colinc minpad pad-char left? op expr)
           (if (and (fx= mincol 0) (fx= minpad 0))
               expr
               (let ([s (let ([op (open-output-string)])
                          expr
                          (get-output-string op))])
                 (unless left? (display s op))
                 (let ([n (let ([n (fxmax 0 (fx- mincol minpad (string-length s)))])
                            (fx+ minpad
                                 (fx* (fxquotient
                                        (fx+ n (fx- colinc 1))
                                        colinc)
                                      colinc)))])
                     (unless (fx= n 0)
                       (display (make-string n pad-char) op)))
                 (when left? (display s op))))]))
      (define (padnum w oc pc op s)
        (if (not w)
            (display s op)
            (let ([n (string-length s)])
              (cond
                [(fx> n w)
                 (if oc
                     (display (make-string w oc) op)
                     (display s op))]
                [else
                 (when (fx< n w) (display (make-string (fx- w n) pc) op))
                 (display s op)]))))
      (define (write-old-roman x op)
        (if (<= 1 x 4999)
            (let f ([x x] [a '(1000 . #\M)] [ls '((500 . #\D) (100 . #\C) (50 . #\L) (10 . #\X) (5 . #\V) (1 . #\I))])
              (if (>= x (car a))
                  (begin (write-char (cdr a) op) (f (- x (car a)) a ls))
                  (unless (null? ls) (f x (car ls) (cdr ls)))))
            (fprintf op "~d" x)))
      (define (write-roman x op)
        (if (<= 1 x 3999)
            (let f ([x x] [a '(1000 . "M")] [ls '((900 . "CM") (500 . "D") (400 . "CD") (100 . "C") (90 . "XC") (50 . "L") (40 . "XL") (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I"))])
              (if (>= x (car a))
                  (begin (display (cdr a) op) (f (- x (car a)) a ls))
                  (unless (null? ls) (f x (car ls) (cdr ls)))))
            (fprintf op "~d" x)))
      (module (write-ordinal write-cardinal)
        (define (f100 x op)
          (cond
            [(>= x 100)
             (f10 (quotient x 100) op)
             (display " hundred" op)
             (let ([x (remainder x 100)])
               (unless (= x 0)
                 (display " " op)
                 (f10 x op)))]
            [else (f10 x op)]))
        (define (f10 x op)
          (cond
            [(>= x 20)
             (display (vector-ref v20 (quotient x 10)) op)
             (let ([x (remainder x 10)])
               (unless (= x 0)
                 (display "-" op)
                 (f10 x op)))]
            [else (display (vector-ref v0 x) op)]))
        (define (f1000000 x op)
          (cond
            [(>= x 1000000)
             (f100 (quotient x 1000000) op)
             (display " million" op)
             (let ([x (remainder x 1000000)])
               (unless (= x 0)
                 (display " " op)
                 (f1000 x op)))]
            [else (f1000 x op)]))
        (define (f1000 x op)
          (cond
            [(<= 1100 x 1999) (f100 x op)]
            [(>= x 1000)
             (f100 (quotient x 1000) op)
             (display " thousand" op)
             (let ([x (remainder x 1000)])
               (unless (= x 0)
                 (display " " op)
                 (f100 x op)))]
            [else (f100 x op)]))
        (define (*f1000000 x op)
          (cond
            [(>= x 1000000)
             (f100 (quotient x 1000000) op)
             (let ([x (remainder x 1000000)])
               (if (= x 0)
                   (display " millionth" op)
                   (begin
                     (display " million " op)
                     (*f1000 x op))))]
            [else (*f1000 x op)]))
        (define (*f1000 x op)
          (cond
            [(<= 1100 x 1999) (*f100 x op)]
            [(>= x 1000)
             (f100 (quotient x 1000) op)
             (let ([x (remainder x 1000)])
               (if (= x 0)
                   (display " thousandth" op)
                   (begin
                     (display " thousand " op)
                     (*f100 x op))))]
            [else (*f100 x op)]))
        (define (*f100 x op)
          (cond
            [(>= x 100)
             (f10 (quotient x 100) op)
             (let ([x (remainder x 100)])
               (if (= x 0)
                   (display " hundredth" op)
                   (begin
                     (display " hundred " op)
                     (*f10 x op))))]
            [else (*f10 x op)]))
        (define (*f10 x op)
          (cond
            [(>= x 20)
             (let ([q (quotient x 10)] [x (remainder x 10)])
               (if (= x 0)
                   (display (vector-ref *v20 q) op)
                   (begin
                     (display (vector-ref v20 q) op)
                     (display "-" op)
                     (*f10 x op))))]
            [else (display (vector-ref *v0 x) op)]))
        (define v20 '#(#f #f twenty thirty forty fifty sixty seventy eighty ninety))
        (define v0 '#(zero one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))
        (define *v20 '#(#f #f twentieth thirtieth fortieth fiftieth sixtieth seventieth eightieth ninetieth))
        (define *v0 '#(zeroth first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth thirteenth fourteenth fifteenth sixteenth seventeenth eighteenth nineteenth))
        (define (write-ordinal x op)
          (if (<= -999999999 x +999999999)
              (if (< x 0)
                  (begin (display "minus " op) (*f1000000 (- x) op))
                  (*f1000000 x op))
              (fprintf op "~:d~a" x
                (let ([n (remainder (abs x) 100)])
                  (if (<= 11 n 19)
                      "th"
                      (case (remainder n 10)
                        [(1) "st"]
                        [(2) "nd"]
                        [(3) "rd"]
                        [else "th"]))))))
        (define (write-cardinal x op)
          (if (<= -999999999 x +999999999)
              (if (< x 0)
                  (begin (display "minus " op) (f1000000 (- x) op))
                  (f1000000 x op))
              (fprintf op "~:d" x))))
      (define cheap-scale
        (lambda (ls k)
          `(,(car ls) ,(fx+ (cadr ls) k) ,@(cddr ls))))
      (define (do-fwrite-d op x w d k oc pc sign? ls)
        (let ([ls (cheap-scale ls k)])
          (padnum w oc pc op
            (fd->string ls d
              (if (and w (fx< (cadr ls) 0) (fx> (fx+ (if (or sign? (fx= (car ls) -1)) 3 2) d) w)) 0 1)
              sign?))))
      (define (do-fwrite op x w d k oc pc sign?)
        (cond
          [d (do-fwrite-d op x w d k oc pc sign?
               (flonum->digits x float-base 'absolute (fx- (fx+ k d))))]
          [w (padnum w oc pc op
               (let ([ls (cheap-scale (flonum->digits x float-base 'normal 0) k)])
                 (let ([s (car ls)] [e (cadr ls)])
                   (if (fx< e 0)
                       (let ([n (fx+ w e (if (or sign? (fx< s 0)) -1 0))])
                         (let f ([ds (cddr ls)] [i n])
                           (if (fx<= i 0)
                               (let ([ls (cheap-scale (flonum->digits x float-base 'absolute (fx- (fx+ k (fxmax (fx- n e 1) 1)))) k)])
                                 (if (fx= (caddr ls) -1) ; rounded to zero?
                                     (if (fx< s 0)
                                         (if (fx< w 4) "-.0" "-0.0")
                                         (if sign?
                                             (if (fx< w 4) "+.0" "+0.0")
                                             (if (fx< w 3) ".0" "0.0")))
                                     (fd->string ls #f 0 sign?)))
                               (if (fx= (cadr ds) -1) ; can't be -2 w/normal
                                   (fd->string ls #f (if (fx= i 1) 0 1) sign?)
                                   (f (cdr ds) (fx- i 1))))))
                       (let ([n (fx+ w (if (or sign? (fx< s 0)) -2 -1))])
                         (let g ([e e] [ds (cddr ls)] [i n])
                           (if (fx< i 0)
                               (if (fx< e -1)
                                   (fd->string (cheap-scale (flonum->digits x float-base 'absolute (fx- (fx+ e 2) k)) k) (and (fx= e -2) 0) 1 sign?)
                                   (fd->string (cheap-scale (flonum->digits x float-base 'absolute (fx- k)) k) 0 1 sign?))
                               (if (fx= (car ds) -1) ; can't be -2 w/normal
                                   (if (fx< e 0)
                                       (fd->string ls (and (fx= e -1) (fx= i 0) 0) 1 sign?)
                                       (if (fx< e (fx- i 1))
                                           (fd->string (cheap-scale (flonum->digits x float-base 'absolute (fx- k (fx- i e))) k) #f 1 sign?)
                                           (fd->string (cheap-scale (flonum->digits x float-base 'absolute (fx- k)) k) 0 1 sign?)))
                                   (g (fx- e 1) (cdr ds) (fx- i 1))))))))))]
          [else (padnum w oc pc op
                  (fd->string
                    (let ([ls (cheap-scale (flonum->digits x float-base 'normal 0) k)])
                      (let f ([e (cadr ls)] [ds (cddr ls)])
                        (if (fx= (car ds) -1) ; w/normal, can't be -2
                            (cheap-scale (flonum->digits x float-base 'absolute (fx- -1 k)) k)
                            (if (fx< e 0)
                                ls
                                (f (fx- e 1) (cdr ds))))))
                    d 1 sign?))]))
      (define (do-ewrite op x w d ew k oc pc ec sign?)
        (cond
          [(fl= x 0.0)
           (padnum w oc pc op
             (let ([ss (if (fx= (flonum-sign x) 1) "-" (if sign? "+" ""))]
                   [es (if ew (make-string ew #\0) "0")])
               (let ([d (and d (if (fx<= k 0) d (fx+ (fx- d k) 1)))])
                 (if (and w (fx> (fx+ (string-length ss) 4 (or d 1) (string-length es)) w))
                     (if (if d (fx= d 0) (fx> k 0))
                         (string-append ss "0." (string ec) "+" es)
                         (string-append ss "." (if d (make-string d #\0) "0") (string ec) "+" es))
                     (string-append ss "0." (if d (make-string d #\0) "0") (string ec) "+" es)))))]
          [d (let ([ls (flonum->digits x float-base 'relative (if (fx<= k 0) (fx- (fx+ d k)) (fx- -1 d)))])
               (let* ([e (fx- (cadr ls) (fx- k 1))]
                      [es (number->string (fxabs e))]
                      [esl (string-length es)])
                 (if (and w oc ew (fx> esl ew))
                     (display (make-string w oc) op)
                     (let ([ew (if ew (fxmax ew esl) esl)])
                       (padnum w oc pc op
                         (string-append
                           (fd->string
                             `(,(car ls) ,(fx- k 1) ,@(cddr ls))
                             (if (fx<= k 0) d (fx+ (fx- d k) 1))
                             (if (and w (fx> (fx+ (if (or sign? (fx= (car ls) -1)) 5 4) ew d) w)) 0 1)
                             sign?)
                           (if ec (string ec) "e")
                           (if (fx< e 0) "-" "+")
                           (make-string (fx- ew esl) #\0)
                           es))))))]
          [w (let ([sign? (or sign? (fx= (flonum-sign x) 1))])
               (let loop ([ew-guess (or ew 1)])
                 (let d ([d (fxmax (fx- w (if sign? 5 4) ew-guess)
                                 (if (fx= k 0) 0 (if (fx< k 0) (fx- 1 k) (fx- k 1))))])
                   (let ([ls (flonum->digits x float-base 'relative (if (fx<= k 0) (fx- (fx+ d k)) (fx- -1 d)))])
                     (let* ([e (fx- (cadr ls) (fx- k 1))]
                            [es (number->string (fxabs e))]
                            [esl (string-length es)])
                       (if (fx> esl ew-guess)
                           (if (and oc ew)
                               (display (make-string w oc) op)
                               (loop esl))
                           (let ([ew (if ew (fxmax ew esl) esl)])
                             (padnum w oc pc op
                               (string-append
                                 (fd->string
                                   `(,(car ls) ,(fx- k 1) ,@(cddr ls))
                                   (and (fx= (fx- k d) 1) (fx>= (fx+ (if sign? 5 4) ew d) w) 0)
                                   (if (fx> (fx+ (if sign? 5 4) ew d) w) 0 1)
                                   sign?)
                                 (if ec (string ec) "e")
                                 (if (fx< e 0) "-" "+")
                                 (make-string (fx- ew esl) #\0)
                                 es)))))))))]
          [else (display
                  (let ([ls (flonum->digits x float-base 'normal 0)])
                    (let ([e (fx- (cadr ls) (fx- k 1))])
                      (string-append
                        (fd->string `(,(car ls) ,(fx- k 1) ,@(cddr ls)) #f 1 sign?)
                        (if ec (string ec) "e")
                        (if (fx< e 0) "-" "+")
                        (let ([op (open-output-string)])
                          (padnum ew #f #\0 op (number->string (fxabs e)))
                          (get-output-string op)))))
                  op)]))
      (define invalid-parameter
        (lambda (who cmd what p)
          ($oops who
            "invalid ~s parameter ~a in directive ~s"
            what p (fmt-directive cmd))))
      (define (outer-loop cmd* arg* op cntl all-arg* super-arg* ct? succ fail)
        (define tostr
          (lambda (cmd* arg* super-arg* succ fail)
            (let ([op (open-output-string)])
              (let ([xop (if ct? (make-format-port op) op)])
                (outer-loop cmd* arg* xop cntl all-arg* super-arg* ct?
                  (lambda (arg*)
                    (when ct? (close-output-port xop))
                    (succ (get-output-string op) arg*))
                  (lambda (arg* super?)
                    (when ct? (close-output-port xop))
                    (fail (get-output-string op) arg* super?)))))))
        (define next
          (lambda (arg*)
            (when (null? arg*)
              ($oops who "too few arguments for control string ~s" cntl))
            (car arg*)))
        (let loop ([cmd* cmd*] [arg* arg*])
          (if (null? cmd*)
              (succ arg*)
              (let ([cmd (car cmd*)])
                (define-syntax vparams
                  (lambda (x)
                    (define process-param
                      (lambda (arg* t* param* body)
                        (if (null? param*)
                            body
                            (with-syntax ([body (process-param arg* (cdr t*) (cdr param*) body)] [arg* arg*] [t (car t*)])
                              (syntax-case (car param*) ()
                                [(type? p)
                                 #'(let-values ([(t arg*)
                                                 (cond
                                                   [(eq? t 'v) (let ([t (next arg*)])
                                                                 (unless (type? t) (invalid-parameter who cmd 'p t))
                                                                 (values t (cdr arg*)))]
                                                   [(eq? t 'hash) (let ([t (length arg*)])
                                                                    (unless (type? t) (invalid-parameter who cmd 'p t))
                                                                    (values t arg*))]
                                                   [else (values t arg*)])])
                                     body)])))))
                    (syntax-case x ()
                      [(_ arg* ([t param] ...) e1 e2 ...)
                       (process-param
                         #'arg*
                         #'(t ...)
                         #'(param ...)
                         #'(let () e1 e2 ...))])))
                (cond
                  [(string? cmd) (display-string cmd op) (loop (cdr cmd*) arg*)]
                  [(char? cmd) (write-char cmd op) (loop (cdr cmd*) arg*)]
                  [(fmt? cmd)
                   (fmt-case cmd
                     [simple-display ()
                      (display (next arg*) op)
                      (loop (cdr cmd*) (cdr arg*))]
                     [simple-write ()
                      (write (next arg*) op)
                      (loop (cdr cmd*) (cdr arg*))]
                     [fresh-line (n)
                      (vparams arg* ([n (nnfixnum? n)])
                        (when (fx> n 0)
                          (fresh-line op)
                          (when (fx> n 1)
                            (display (make-string (fx- n 1) #\newline) op)))
                        (loop (cdr cmd*) arg*))]
                     [display (mincol colinc minpad pad-char left?)
                      (vparams arg* ([mincol (nnfixnum? mincol)]
                                     [colinc (pfixnum? colinc)]
                                     [minpad (nnfixnum? minpad)]
                                     [pad-char (char? pad-char)])
                        (pad mincol colinc minpad pad-char left? op
                          (display (next arg*) op))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [write (mincol colinc minpad pad-char nogensym? left?)
                      (vparams arg* ([mincol (nnfixnum? mincol)]
                                     [colinc (pfixnum? colinc)]
                                     [minpad (nnfixnum? minpad)]
                                     [pad-char (char? pad-char)])
                        (pad mincol colinc minpad pad-char left? op
                          (if nogensym?
                              (parameterize ([print-gensym #f])
                                (write (next arg*) op))
                              (write (next arg*) op)))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [cwrite (colon? at?)
                      (let ([c (next arg*)])
                        (unless (char? c)
                          ($oops who "expected character for ~~c, received ~s" c))
                        (if colon?
                            (let ([x (char-name c)])
                              (if x
                                  (begin
                                    (write-char #\< op)
                                    (display x op)
                                    (write-char #\> op))
                                  (let ([n (char->integer c)])
                                    (if (fx< n #x20)
                                        (begin
                                          (write-char #\^ op)
                                          (write-char (integer->char (fx+ n #x40)) op))
                                        (write-char c op)))))
                            (if at?
                                (write c op)
                                (write-char c op))))
                      (loop (cdr cmd*) (cdr arg*))]
                     [fwrite (w d k oc pc sign?)
                      (vparams arg* ([w (nnfixnum? w)]
                                     [d (nnfixnum? d)]
                                     [k (fixnum? k)]
                                     [oc (char? overflow-char)]
                                     [pc (char? pad-char)])
                        (let ([x (next arg*)])
                          (unless (real? x)
                            ($oops who "expected real number for ~~f, received ~s" x))
                          (let ([x (inexact x)])
                            (if (exceptional-flonum? x)
                                (padnum w oc pc op (number->string x))
                                (do-fwrite op x w d k oc pc sign?))))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [ewrite (w d ew k oc pc ec sign?)
                      (vparams arg* ([w (nnfixnum? w)]
                                     [d (nnfixnum? d)]
                                     [ew (nnfixnum? e)]
                                     [k (fixnum? k)]
                                     [oc (char? overflow-char)]
                                     [pc (char? pad-char)]
                                     [ec (char? exponent-char)])
                        (let ([x (next arg*)])
                          (unless (real? x)
                            ($oops who "expected real number for ~~e, received ~s" x))
                          (let ([x (inexact x)])
                            (if (exceptional-flonum? x)
                                (padnum w oc pc op (number->string x))
                                (if (or (not d) (fx< (fx- d) k (fx+ d 2)))
                                    (do-ewrite op x w d ew k oc pc ec sign?)
                                   ; signaling an error might be kind, but cltl2 says otherwise
                                    (if (and w oc)
                                        (display (make-string w oc) op)
                                        (let ([d (if (fx> k 0) (fx- k 1) (fx- 1 k))])
                                          (do-ewrite op x w d ew k oc pc ec sign?)))))))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [gwrite (w d ew k oc pc ec sign?)
                      (vparams arg* ([w (nnfixnum? w)]
                                     [d (nnfixnum? d)]
                                     [ew (nnfixnum? e)]
                                     [k (fixnum? k)]
                                     [oc (char? overflow-char)]
                                     [pc (char? pad-char)]
                                     [ec (char? exponent-char)])
                        (let ([x (next arg*)])
                          #;(define (ilog x) (fx+ (cadr (flonum->digits x float-base 'normal 0)) 1))
                          (define (ilog x) ; 4x faster and good enough
                            (if (fl= x 0.0)
                                0
                                (fx+ (flonum->fixnum (floor (fl- (fl* (log (flabs x)) (fl/ (log 10))) 1e-10))) 1)))
                          (define significant-digits
                            (lambda (ls)
                              (if (fx< (car ls) 0)
                                  0
                                  (fx+ 1 (significant-digits (cdr ls))))))
                          (unless (real? x)
                            ($oops who "expected real number for ~~g, received ~s" x))
                          (let ([x (inexact x)])
                            (if (exceptional-flonum? x)
                                (padnum w oc pc op (number->string x))
                                (if d
                                    (let f ([n (ilog x)])  ; can x be negative here?
                                      (let ([dd (fx- d n)])
                                        (if (not (fx<= 0 dd d))
                                            (do-ewrite op x w d ew k oc pc ec sign?)
                                            (let ([ls (flonum->digits x float-base 'absolute (fx- dd))])
                                              (let ([actual-n (fx+ (cadr ls) 1)])
                                                (if (fx> actual-n n) ; e.g., .9999 came back as 1.000
                                                    (f actual-n)
                                                    (let* ([ee (if ew (fx+ ew 2) 4)]
                                                           [ww (and w (fx- w ee))])
                                                     ; scale k not used when treated as ~f
                                                      (do-fwrite-d op x ww dd 0 oc pc sign? ls)
                                                      (when w (display (make-string ee #\space) op)))))))))
                                    (let* ([ls (flonum->digits x float-base 'normal 0)]
                                           [n (fx+ (cadr ls) 1)]
                                           [est-d (max (significant-digits (cddr ls)) (min n 7))]
                                           [dd (fx- est-d n)])
                                      (if (fx<= 0 dd est-d)
                                          (let* ([ee (if ew (fx+ ew 2) 4)]
                                                 [ww (and w (fx- w ee))])
                                           ; scale k not used when treated as ~f
                                            (do-fwrite op x ww dd 0 oc pc sign?)
                                           ; suppressing trailing whitespace when (not w)
                                            (when w (display (make-string ee #\space) op)))
                                         ; cltl seems to want our estimated d here (est-d)
                                         ; but original d (#f) makes more sense
                                          (do-ewrite op x w d ew k oc pc ec sign?)))))))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [$write (d n w pc sign-before-pad? sign?)
                      (vparams arg* ([d (nnfixnum? d)]
                                     [n (nnfixnum? n)]
                                     [w (nnfixnum? w)]
                                     [pc (char? pad-char)])
                        (let ([x (next arg*)])
                          (unless (real? x)
                            ($oops who "expected real number for ~~$, received ~s" x))
                          (let ([x (inexact x)])
                            (if (exceptional-flonum? x)
                                (padnum w #f pc op (number->string x))
                                (let ([ls (flonum->digits x float-base 'absolute (fx- d))])
                                  (if (and sign-before-pad? (or sign? (fx= (car ls) -1)))
                                      (begin
                                        (write-char (if (fx= (car ls) -1) #\- #\+) op)
                                        (padnum (fx- w 1) #f pc op
                                          (fd->string (cons 1 (cdr ls)) d n #f)))
                                      (padnum w #f pc op
                                        (fd->string ls d n sign?)))))))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [write-radix (base w pc cc ci sign? commas?)
                      (vparams arg* ([base (radix? n)]
                                     [w (nnfixnum? w)]
                                     [pc (char? pad-char)]
                                     [cc (char? comma-char)]
                                     [ci (pfixnum? comma-interval)])
                        (let ([x (next arg*)])
                          (padnum w #f pc op
                            (cond
                              [(exact-integer? x)
                               (let* ([s (number->string x base)]
                                      [s (if (and sign? (>= x 0)) (string-append "+" s) s)])
                                 (if commas?
                                     (let* ([n (string-length s)]
                                            [sign (let ([c (string-ref s 0)])
                                                    (and (memv c '(#\+ #\-)) c))]
                                            [m (if sign (fx- n 1) n)]
                                            [nc (fxquotient (fx- m 1) ci)]
                                            [s2 (make-string (fx+ n nc))]
                                            [k (fxremainder m ci)]
                                            [k (if (fx= k 0) ci k)])
                                       (define (loop i j k)
                                         (cond
                                           [(fx= i n) s2]
                                           [(fx= k 0)
                                            (string-set! s2 j cc)
                                            (loop i (fx+ j 1) ci)]
                                           [else
                                            (string-set! s2 j (string-ref s i))
                                            (loop (fx+ i 1) (fx+ j 1) (fx- k 1))]))
                                       (cond
                                         [sign
                                          (string-set! s2 0 sign)
                                          (loop 1 1 k)]
                                         [else (loop 0 0 k)]))
                                     s))]
                              [else
                               (let ([op (open-output-string)])
                                 (parameterize ([print-radix base])
                                   (display x op))
                                 (get-output-string op))])))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [plural (back-up? y/ies?)
                      (let ([arg* (if back-up?
                                      (let f ([prev #f] [ls all-arg*])
                                        (if (eq? ls arg*)
                                            (if prev
                                                prev
                                                ($oops who "no previous argument for ~a" (fmt-directive (car cmd*))))
                                            (f ls (cdr ls))))
                                      arg*)])
                        (if (eqv? (next arg*) 1)
                            (when y/ies? (write-char #\y op))
                            (if y/ies?
                                (display "ies" op)
                                (write-char #\s op)))
                        (loop (cdr cmd*) (cdr arg*)))]
                     [fancy-radix (colon? at?)
                      (let ([x (next arg*)])
                        (unless (exact-integer? x)
                          ($oops who "expected exact integer for ~~r, received ~s" x))
                        (if colon?
                            (if at?
                                (write-old-roman x op)
                                (write-ordinal x op))
                            (if at?
                                (write-roman x op)
                                (write-cardinal x op))))
                      (loop (cdr cmd*) (cdr arg*))]
                     [dup-char (n c)
                      (vparams arg* ([n (nnfixnum? n)])
                        (display (make-string n c) op)
                        (loop (cdr cmd*) arg*))]
                     [tabulate (colnum colinc relative?)
                      (vparams arg* ([colnum (nnfixnum? colnum)]
                                     [colinc (nnfixnum? colinc)])
                        (cond
                          [relative?
                           (display (make-string colnum #\space) op)
                           (unless (= colinc 0)
                             (let ([col (output-column op)])
                               (when col
                                 (let ([n (modulo col colinc)])
                                   (unless (= n 0)
                                     (display (make-string (- colinc n) #\space) op))))))]
                          [else
                           (let ([col (output-column op)])
                             (if col
                                 (if (>= col colnum)
                                     (unless (= colinc 0)
                                       (display (make-string (- colinc (modulo (- col colnum) colinc)) #\space) op))
                                     (display (make-string (- colnum col) #\space) op))
                                 (display "  " op)))])
                        (loop (cdr cmd*) arg*))]
                     [indirect (splice?)
                      (let ([xcntl (next arg*)])
                        (unless (string? xcntl)
                          ($oops who "first ~a argument ~s is not a string" (fmt-directive (car cmd*)) xcntl))
                        (let-values ([(xcmd* expected) (parse who xcntl)])
                          (if splice?
                              (outer-loop xcmd* (cdr arg*) op cntl all-arg* #f ct?
                                (lambda (arg*) (loop (cdr cmd*) arg*))
                                (lambda (arg* super?) (loop (cdr cmd*) arg*)))
                              (let* ([arg* (cdr arg*)]
                                     [xarg* (next arg*)])
                                (let ([len ($list-length xarg* who)])
                                  (when (and indirect-too-many-args-check expected)
                                    (check-nargs who expected len xcntl)))
                                (outer-loop xcmd* xarg* op xcntl xarg* #f ct?
                                  (lambda (xarg*)
                                    (when (and dynamic-too-many-args-check (not (null? xarg*)))
                                      ($oops who "too many arguments for control string ~s" xcntl))
                                    (loop (cdr cmd*) (cdr arg*)))
                                  (lambda (xarg* super?)
                                    (loop (cdr cmd*) (cdr arg*))))))))]
                     [conditional (n cases default)
                      (vparams arg* ([n (nnfixnum? n)])
                        (let-values ([(n arg*) (if n (values n arg*) (let ([n (next arg*)]) (values n (cdr arg*))))])
                          (loop
                            (append (if (and (fixnum? n) (fx<= 0 n) (fx< n (vector-length cases)))
                                        (vector-ref cases n)
                                        default)
                                    (cdr cmd*))
                            arg*)))]
                     [conditional/colon (alternative consequent)
                      (let ([arg (next arg*)])
                        (loop (append (if arg consequent alternative) (cdr cmd*))
                              (cdr arg*)))]
                     [conditional/at (consequent)
                      (if (next arg*)
                          (loop (append consequent (cdr cmd*)) arg*)
                          (loop (cdr cmd*) (cdr arg*)))]
                     [justify (mincol colinc minpad pc before? after? initial margin columns segments)
                      (vparams arg* ([mincol (nnfixnum? mincol)]
                                     [colinc (nnfixnum? colinc)]
                                     [minpad (nnfixnum? minpad)]
                                     [pc (char? pad-char)])
                        (let ()
                          (define (process-segments initial complete segments arg*)
                            (if (null? segments)
                                (finalize initial (reverse complete) arg*)
                                (tostr (car segments) arg* #f
                                  (lambda (s arg*) (process-segments initial (cons s complete) (cdr segments) arg*))
                                  (lambda (s arg* super?) (finalize initial (reverse complete) arg*)))))
                          (define (finalize initial segments arg*)
                            (let* ([chars (apply fx+ (map string-length segments))]
                                   [segments (if before?
                                                 (if after?
                                                     `("" ,@segments "")
                                                     `("" ,@segments))
                                                 (if after?
                                                     `(,@segments "")
                                                     (if (null? segments)
                                                         '("")
                                                         segments)))]
                                   [npads (fx- (length segments) 1)]
                                   [size (fx+ chars (fx* minpad npads))]
                                   [size (if (fx<= size mincol)
                                             mincol
                                             (fx+ size (fxmodulo (fx- mincol size) colinc)))])
                              (when initial 
                                (let ([oc (output-column op)])
                                  (when (and oc (fx> (fx+ oc size margin) columns))
                                    (display initial op))))
                              (cond
                                [(fx= npads 0) ; right justify single item
                                 (display (make-string (fx- size chars) pc) op)
                                 (display (car segments) op)]
                                [else
                                 (let* ([pad-amt (fx- size chars)]
                                        [pad-q (fxquotient pad-amt npads)]
                                        [pad-r (fxremainder pad-amt npads)]
                                        [pad-i (if (fx= pad-r 0) 0 (fxquotient npads pad-r))])
                                   (let f ([s (car segments)] [s* (cdr segments)] [i 1] [pad-r pad-r])
                                     (display s op)
                                     (unless (null? s*)
                                       (cond
                                         [(and (fx> pad-r 0) (fx= i 1))
                                          (display (make-string (fx+ pad-q 1) pc) op)
                                          (f (car s*) (cdr s*) pad-i (fx- pad-r 1))]
                                         [else
                                          (display (make-string pad-q pc) op)
                                          (f (car s*) (cdr s*) (fx- i 1) pad-r)]))))]))
                            (loop (cdr cmd*) arg*))
                          (if initial
                              (tostr initial arg* #f
                                (lambda (initial arg*) (process-segments initial '() segments arg*))
                                (lambda (s arg* super?) (finalize #f '() arg*)))
                              (process-segments #f '() segments arg*))))]
                     [goto (n reverse? absolute?)
                      (vparams arg* ([n (nnfixnum? n)])
                        (loop (cdr cmd*)
                              (cond
                                [absolute?
                                 (let ([n (or n 0)])
                                   (unless (fx<= n (length all-arg*))
                                     ($oops who "~a would move beyond argument list" (fmt-directive (car cmd*))))
                                   (list-tail all-arg* n))]
                                [reverse?
                                 (let ([n (or n 1)])
                                   (let ([n (fx- (length all-arg*) (length arg*) n)])
                                     (unless (fx>= n 0)
                                       ($oops who "~a would move before first argument" (fmt-directive (car cmd*))))
                                     (list-tail all-arg* n)))]
                                [else
                                 (let ([n (or n 1)])
                                   (unless (fx<= n (length arg*))
                                     ($oops who "~a would move beyond argument list" (fmt-directive (car cmd*))))
                                   (list-tail arg* n))])))]
                     [convert-case (nested-cmd* colon? at?)
                      (let ()
                        (define convert-display
                          (lambda (s)
                            (if colon?
                                (if at?
                                    (string-upcase! s)
                                    (string-capitalize! s))
                                (if at?
                                    (string-capitalize-first! s)
                                    (string-downcase! s)))
                            (display s op)))
                        (tostr nested-cmd* arg* super-arg*
                          (lambda (s arg*) (convert-display s) (loop (cdr cmd*) arg*))
                          (lambda (s arg* super?) (convert-display s) (fail arg* super?))))]
                     [iteration (body n sublists? use-remaining? at-least-once?)
                      (vparams arg* ([n (nnfixnum? n)])
                        (let-values ([(body body-cntl body-expected arg*)
                                      (if (null? body)
                                          (let ([arg (next arg*)])
                                             (let-values ([(cmd* expected) (parse who arg)])
                                               (values cmd* arg expected (cdr arg*))))
                                          (values body cntl #f arg*))])
                          (if use-remaining?
                              (if sublists?
                                  (let f ([n n] [arg* arg*] [at-least-once? at-least-once?])
                                    (if (or (and n (fx= n 0)) (and (not at-least-once?) (null? arg*)))
                                        (loop (cdr cmd*) arg*)
                                        (let-values ([(xarg* arg*) (if (null? arg*) (values '() '()) (values (car arg*) (cdr arg*)))])
                                          (let ([len ($list-length xarg* who)])
                                            (when (and indirect-too-many-args-check body-expected)
                                              (check-nargs who body-expected len body-cntl)))
                                          (outer-loop body xarg* op body-cntl xarg* arg* ct?
                                            (lambda (xarg*)
                                              (when (and dynamic-too-many-args-check (not (null? xarg*)))
                                                ($oops who "too many arguments for control string ~s" body-cntl))
                                              (f (and n (fx- n 1)) arg* #f))
                                            (lambda (xarg* super?)
                                              (if super?
                                                  (loop (cdr cmd*) arg*)
                                                  (f (and n (fx- n 1)) arg* #f)))))))
                                  (let f ([n n] [arg* arg*] [at-least-once? at-least-once?])
                                    (if (or (and n (fx= n 0)) (and (not at-least-once?) (null? arg*)))
                                        (loop (cdr cmd*) arg*)
                                        (outer-loop body arg* op body-cntl all-arg* #f ct?
                                          (lambda (arg*) (f (and n (fx- n 1)) arg* #f))
                                          (lambda (arg* super?) (f (and n (fx- n 1)) arg* #f))))))
                              (let ([all-larg* (next arg*)])
                                (unless (list? all-larg*)
                                  ($oops who "~s is not a proper list" all-larg*))
                                (if sublists?
                                    (let f ([n n] [larg* all-larg*] [at-least-once? at-least-once?])
                                      (if (or (and n (fx= n 0)) (and (not at-least-once?) (null? larg*)))
                                          (loop (cdr cmd*) (cdr arg*))
                                          (let-values ([(xarg* larg*) (if (null? larg*) (values '() '()) (values (car larg*) (cdr larg*)))])
                                            (let ([len ($list-length xarg* who)])
                                              (when (and indirect-too-many-args-check body-expected)
                                                (check-nargs who body-expected len body-cntl)))
                                            (outer-loop body xarg* op body-cntl xarg* larg* ct?
                                              (lambda (xarg*)
                                                (when (and dynamic-too-many-args-check (not (null? xarg*)))
                                                  ($oops who "too many arguments for control string ~s" body-cntl))
                                                (f (and n (fx- n 1)) larg* #f))
                                              (lambda (xarg* super?)
                                                (if super?
                                                    (loop (cdr cmd*) (cdr arg*))
                                                    (f (and n (fx- n 1)) larg* #f)))))))
                                    (let f ([n n] [larg* all-larg*] [at-least-once? at-least-once?])
                                      (if (or (and n (fx= n 0)) (and (not at-least-once?) (null? larg*)))
                                          (loop (cdr cmd*) (cdr arg*))
                                          (outer-loop body larg* op body-cntl all-larg* #f ct?
                                            (lambda (larg*) (f (and n (fx- n 1)) larg* #f))
                                            (lambda (larg* super?) (f (and n (fx- n 1)) larg* #f))))))))))]
                     [abort (n m super?)
                      (vparams arg* ([n (true? n)] [m (true? m)])
                        (if (if n
                                (if m (eqv? n m) (eqv? n 0))
                                (null? (if super? super-arg*  arg*)))
                            (fail arg* super?)
                            (loop (cdr cmd*) arg*)))]
                     [columntrack (body)
                      (let ([xop (make-format-port op)])
                        (outer-loop body arg* xop cntl arg* super-arg* #t
                          (lambda (arg*)
                            (close-output-port xop)
                            (outer-loop (cdr cmd*) arg* op cntl arg* super-arg* ct? succ fail))
                          (lambda (arg* super?)
                            (close-output-port xop)
                            (fail arg* super?))))]
                     [else ($oops who "internal error: ~s" cmd)])]
                  [else ($oops who "internal error: ~s" cmd)])))))
      (let ([op (or fmt-op (open-output-string))])
        (outer-loop cmd* arg* op cntl arg* #f #f
          (lambda (arg*)
            (when (and dynamic-too-many-args-check (not (null? arg*)))
              ($oops who "too many arguments for control string ~s" cntl))
            (void))
          (lambda (arg* super?) (void)))
        (unless fmt-op (get-output-string op)))))

  (define check-nargs
    (lambda (who expected received cntl)
      (when (and expected received)
        (unless (fx= expected received)
          (if (fx< received expected)
              ($oops who "too few arguments for control string ~s" cntl)
              ($oops who "too many arguments for control string ~s" cntl))))))

  (define format-port-name "format port")
  (define (output-column p)
    (unless (eq? (port-name p) format-port-name)
      ($oops 'format "internal error: port is not a format port"))
    ((port-handler p) 'column p))
 
  (define make-format-port
    (lambda (subop)
      (define column 0)
      (define update-column!
        (lambda (p s n)
          (let f ([i 0] [col 0] [newline? #f])
            (if (fx= i n)
                (begin
                  (set! column (if newline? col (+ column col)))
                  (set-port-bol! p newline?))
                (if (char=? (string-ref s i) #\newline)
                    (f (fx+ i 1) 0 #t)
                    (f (fx+ i 1) (fx+ col 1) newline?))))))
      (define handler
        (message-lambda
          (lambda (msg . args) ($oops 'format-port "operation ~s not handled" msg))
          [(block-write p s n)
           (flush-output-port p)
           (update-column! p s n)
           (block-write subop s n)]
          [(clear-output-port p) (set-textual-port-output-index! p 0)]
          [(close-port p)
           (flush-output-port p)
           (set-textual-port-output-size! p 0)
           (mark-port-closed! p)]
;         [(file-length p) #f]
          [(file-position p) (most-negative-fixnum)]
          [(file-position p pos) ($oops 'format-port "cannot reposition")]
          [(flush-output-port p)
           (let ([b (textual-port-output-buffer p)]
                 [i (textual-port-output-index p)])
             (unless (fx= i 0)
               (update-column! p b i)
               (block-write subop b i)))
           (set-textual-port-output-index! p 0)]
          [(port-name p) format-port-name]
          [(write-char c p)
           (let ([b (textual-port-output-buffer p)]
                 [i (textual-port-output-index p)])
             (string-set! b i c)
             (block-write subop b (fx+ i 1)))
           (set-textual-port-output-index! p 0)]
          [(column p) (flush-output-port p) column]))
      (let ([len 1024])
        (let ([p (make-output-port handler (make-string len))])
          (set-textual-port-output-size! p (fx- len 1))
          (set-port-bol! p #t)
          p))))

  (define go
    (lambda (who op cntl args)
      (let-values ([(cmd* expected) (parse who cntl)])
        (when static-too-many-args-check
          (check-nargs who expected (length args) cntl))
        (dofmt who op cntl cmd* args))))

  (set! format
    (case-lambda
      [(port/cntl cntl/arg . args)
       (cond
         [(port? port/cntl)
          (unless (and (output-port? port/cntl) (textual-port? port/cntl))
            ($oops 'format "~s is not a textual output port" port/cntl))
          (go 'format port/cntl cntl/arg args)]
         [(eq? port/cntl #t) (go 'format (current-output-port) cntl/arg args)]
         [(eq? port/cntl #f) (go 'format #f cntl/arg args)]
         [else (go 'format #f port/cntl (cons cntl/arg args))])]
      [(cntl . args) (go 'format #f cntl args)]))

  (set! $dofmt dofmt)

  (set! $make-fmt->expr make-fmt->expr)

  (set! $parse-format-string
    (lambda (who cntl received)
      (let-values ([(cmd* expected) (parse who cntl)])
        (when static-too-many-args-check
          (check-nargs who expected received cntl))
        (squash cmd*))))

  (set! printf
    (lambda (cntl . args)
      (go 'printf (current-output-port) cntl args)))

  (set! fprintf
    (lambda (op cntl . args)
      (unless (and (output-port? op) (textual-port? op))
        ($oops 'fprintf "~s is not a textual output port" op))
      (go 'fprintf op cntl args))))
