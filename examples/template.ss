#!chezscheme
;;; Copyright 1984-2016 Cisco Systems, Inc.
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
;;; include-template: a simple yet powerful extensible templating mechanism
;;;
;;; Authors: Andrew W. Keep and R. Kent Dybvig
;;;
;;; The syntax (include-template <filename>) expands into an expression whose
;;; value is a string containing the contents of the file named by <filename>,
;;; except each occurrence of @@ within the file is replaced by @, and each
;;; occurrence of @(<scheme expression>) is replaced with the value of
;;; <scheme expression>.  <filename> must be a string literal, and the value
;;; of each <scheme expression> must be a string.  The file named by <filename>
;;; must be present at expand time and need not be present at run time.
;;;
;;; The template system can also be extended using the syntax:
;;;
;;; (define-template-extension <template-pattern> <output-pattern>)
;;;
;;; where:
;;;
;;; <template-pattern>     -> (<initial-pattern> <additional-pattern>*)
;;;                         | <non-template-pattern>
;;; <initial-pattern>      -> <keyword> (<syntax-pattern>*) <template-id>
;;;                         | <keyword> <template-id>
;;; <additional-pattern>   -> <initial-pattern>
;;;                         | (<keyword> (<syntax-pattern>*) <template-id>) ...
;;;                         | (<keyword> <tempalte-id>) ...
;;;                         | (optional <keyword> (<syntax-pattern>*)
;;;                             <template-id>)
;;;                         | (optional <keyword> <template-id>)
;;; <non-template-pattern> -> <keyword> (<syntax-pattern>*)
;;; <keyword>              -> <initial-kw-character> <kw-character>
;;; <initial-kw-character> -> a - z | A - Z | ! | $ | % | & | * | / | : | <
;;;                         | = | > | ? | ^ | _ | ~
;;; <kw-character>         -> <initial-kw-character> | 0 - 9 | - | + | .
;;;
;;; <output-pattern> is treated as a quasisyntax template,
;;; <syntax-pattern> is a syntax-rules pattern and,
;;; <template-id> is any valid scheme identifier.
;;;
;;; For <template-patterns> using a <non-template-pattern> a new @<keyword>
;;; form is created and when the @<keyword>(<syntax-pattern>*) is encountered
;;; in a template, it is immediately replaced with the <output-pattern>.
;;;
;;; For instance an extension that converts numbers to strings can be
;;; implemented as:
;;;
;;; (define-template-extension (num (e)) (number->string e))
;;;
;;; When @num(10) is encountered in a template, the string "10" is generated.
;;;
;;; For <template-patterns> with an <inital-pattern> each <keyword> in the
;;; <template-pattern> the define-template-extension will create a new syntax
;;; form @<keyword> along with an @end<keyword> for the <keyword> from the
;;; <initial-pattern>.  When include-template encounters an
;;; @<initial-keyword> ... @end<initial-keyword> pattern it will match the
;;; <syntax-pattern>* (when supplied) against the following scheme expression
;;; and match the template expressions found between the @<keywords> to the
;;; matching <template-id> bindings.
;;;
;;; For example, we can add a "for" loop extension as:
;;;
;;; (define-template-extension (for ([x e] [xs es] ...) tmpl)
;;;   (let ([t e])
;;;     (apply string-append (map (lambda (x xs ...) tmpl) t es ...))))
;;;
;;;     (for (exprs ...) tmpl)
;;;     =>
;;;     @for (lambda (...)
;;;            (let ([t (read-scheme k...)])
;;;              (cons (incomplete-node @for t) ...)))
;;;     @endfor (lambda (...)
;;;               ---
;;;               (let loop ([t '()] [tmpl '()] ...)
;;;                 (cond
;;;                   [(and (incomplete-node? (car stack)) (eq? (incomplete-node-kw (car stack)) '@for))
;;;                    (with-syntax (['([x e] [xs es] ...) (incomplete-node-stx (car stack))])
;;;                      '(let ([t e])
;;;                         (apply string-append (map (lambda (x xs ...) tmpl) t es ..))))
;;;
;;; In a template if: @for([x '("a" "b" "c")]) got @(t). @endfor
;;; is encountered, it will print the string " got a.  got b.  got c. " for
;;; this expression (which will be produced by the code:
;;; (apply string-append
;;;   (map (lambda (x) (string-append " got " x ". ") '("a" "b" "c"))))
;;;
;;; For a more complex example see the @if/@elif/@else/@endif example at
;;; the end of the library.
;;;
;;; Additional examples are embedded within the tests below #!eof.

;;; The (template-helpers) library supplies scheme procedures that are used at
;;; macro-expansion time by both the include-template and
;;; define-template-extension macros.
(library (template-helpers)
  (export
    incomplete-node? make-incomplete-node
    incomplete-node-type incomplete-node-e* incomplete-node-bfp
    source-string source-error
    read-scheme initial-id-char? id-char?
    make-string-buffer extend-string-buffer! extract-string-and-reset!
    open-positional-string-output-port
    strip-blanks)
  (import (chezscheme))

  (define (source-string sfd bfp)
    (call-with-values
      (lambda () (locate-source sfd bfp))
      (case-lambda
        [() (format "at char position ~s of ~a" bfp
              (source-file-descriptor-path sfd))]
        [(path lp cp) (format "at line ~s, char ~s of ~a" lp cp
                        (source-file-descriptor-path sfd))])))

  (define (source-error sfd bfp msg . args)
    (errorf 'include-template "~? ~a" msg args (source-string sfd bfp)))

  (define (read-scheme k ip sfd bfp)
    (let-values ([(x new-bfp) (get-datum/annotations ip sfd bfp)])
      (let ([x (if (annotation? x) (annotation-expression x) x)])
        (values (datum->syntax k x) new-bfp))))

  (define-record-type incomplete-node (nongenerative) (fields type e* bfp))

  (define (initial-id-char? c)
    (or (char<=? #\a c #\z) (char<=? #\A c #\Z)
        (char=? c #\!) (char<=? #\$ c #\&)
        (char=? c #\*) (char=? c #\/)
        (char=? c #\:) (char<=? #\< c #\?)
        (char=? c #\^) (char=? c #\_) (char=? c #\~)))

  (define (id-char? c)
    (or (initial-id-char? c) (char<=? #\0 c #\9)
        (char=? c #\-) (char=? c #\+) (char=? c #\.)))

  (define-record-type string-buffer (nongenerative)
    (fields (mutable n) (mutable str))
    (protocol (lambda (new) (lambda () (new 0 (make-string 16))))))
  (define (get-buffer tb n required-capacity)
    (let* ([str (string-buffer-str tb)] [len (string-length str)])
      (if (fx< (fx- len n) required-capacity)
          (let ([new-str (make-string (fx* 2 (fx+ len required-capacity)))])
            (string-copy! str 0 new-str 0 n)
            (string-buffer-str-set! tb new-str)
            new-str)
          str)))
  (define (extend-string-buffer! tb c)
    (let ([n (string-buffer-n tb)])
      (string-set! (get-buffer tb n 1) n c)
      (string-buffer-n-set! tb (fx+ n 1))))
  (define (append-to-string-buffer! tb str)
    (let ([n (string-buffer-n tb)] [len (string-length str)])
      (string-copy! (get-buffer tb n len) n str 0 len)
      (string-buffer-n-set! tb (fx+ n len))))
  (define (extract-string-and-reset! tb)
    (let ([str (substring (string-buffer-str tb) 0 (string-buffer-n tb))])
      (string-buffer-n-set! tb 0)
      str))
  (define (open-positional-string-output-port)
    (define-record-type position (nongenerative)
      (fields (mutable line) (mutable column))
      (protocol (lambda (new) (lambda () (new 1 1)))))
    (let ([tb (make-string-buffer)] [pos (make-position)])
      (define (w! str start cnt)
        (let* ([n (string-buffer-n tb)]
               [buf (get-buffer tb n cnt)]
               [end (fx+ start cnt)])
          (let loop! ([i start] [n n] [line (position-line pos)] [column (position-column pos)])
            (if (fx= i end)
                (begin
                  (position-line-set! pos line)
                  (position-column-set! pos column)
                  (string-buffer-n-set! tb n))
                (let ([c (string-ref str i)])
                  (string-set! buf n c)
                  (if (char=? c #\newline)
                      (loop! (fx+ i 1) (fx+ n 1) (fx+ line 1) 1)
                      (loop! (fx+ i 1) (fx+ n 1) line (fx+ column 1)))))))
        cnt)
      (define (gp) (string-buffer-n tb))
      (let ([op (make-custom-textual-output-port "positional-string-output-port" w! gp #f #f)])
        (define (line) (flush-output-port op) (position-line pos))
        (define (column) (flush-output-port op) (position-column pos))
        (define (p) (flush-output-port op) (extract-string-and-reset! tb))
        (values op p line column))))
  
  ;; scan foward for blanks, and if it leads you to a new-line, strip
  ;; the previous blanks back to the new line.
  (define (finish-strip ip stack bfp start-bfp)
    (let ([node-to-strip (car stack)])
      (if (string? node-to-strip)
          (let loop ([i (string-length node-to-strip)])
            (if (fx= i 0)
                (values (cdr stack) bfp)
                (let ([i (fx- i 1)])
                  (let ([c (string-ref node-to-strip i)])
                    (cond
                      [(char=? c #\newline)
                       (values (cons (substring node-to-strip 0 (fx+ i 1)) (cdr stack)) bfp)]
                      [(char-whitespace? c) (loop i)]
                      [else (set-port-position! ip start-bfp) (values stack start-bfp)])))))
          (begin (set-port-position! ip start-bfp) (values stack start-bfp)))))
  (define (strip-blanks ip stack start-bfp)
    (let gather-blanks ([bfp start-bfp])
      (let ([c (read-char ip)])
        (cond
          [(eof-object? c) (finish-strip ip stack bfp start-bfp)]
          [(char=? c #\newline) (finish-strip ip stack (fx+ bfp 1) start-bfp)]
          [(char-whitespace? c) (gather-blanks (fx+ bfp 1))]
          [else (set-port-position! ip start-bfp) (values stack start-bfp)])))))

(library (template)
  (export include-template define-template-extension optional @if @elif @else @endif @for @endfor @num)
  (import (chezscheme) (template-helpers))

  (define-syntax optional (lambda (x) (syntax-violation #f "misplaced aux keyword" x)))

  (define check-string-and-indent
    (lambda (s at indent)
      (unless (string? s)
        (errorf 'include-template "unexpected non-string value ~s of expression ~s" s at))
      (if (= indent 0)
          s
          (let ([ip (open-string-input-port s)])
            (let ([first-line (get-line ip)])
              (if (eof-object? first-line)
                  s
                  (let-values ([(op p) (open-string-output-port)])
                    (display first-line op)
                    (let ([indent (make-string indent #\space)])
                      (let loop ()
                        (let ([line (get-line ip)])
                          (if (eof-object? line)
                              (begin
                                (when (char=? (string-ref s (fx- (string-length s) 1)) #\newline) (newline op))
                                (p))
                              (begin
                                (newline op)
                                (display indent op)
                                (display line op)
                                (loop)))))))))))))

  (define-syntax include-template
    (lambda (x)
      (define (process-template-file r fn k)
        (let* ([bip (open-file-input-port fn)]
               [sfd (make-source-file-descriptor fn bip #t)]
               [ip (transcoded-port bip (native-transcoder))]
               [tb (make-string-buffer)])
          (define (s0 a bfp)
            (let ([c (read-char ip)])
              (cond
                [(eof-object? c)
                 (close-input-port ip)
                 (reverse (cons (extract-string-and-reset! tb) a))]
                [(char=? c #\@) (s1 a (+ bfp 1))]
                [else (extend-string-buffer! tb c) (s0 a (+ bfp 1))])))
          (define (s1 a bfp)
            (let ([c (read-char ip)])
              (cond
                [(eof-object? c) (source-error sfd bfp "expected open paren or @ following @")]
                [(eqv? c #\@) (extend-string-buffer! tb c) (s0 a (+ bfp 1))]
                [(eqv? c #\()
                 (unread-char c ip)
                 (let-values ([(e* new-bfp) (read-scheme k ip sfd bfp)])
                   (syntax-case e* ()
                     [(e)
                      (s0
                        (cons*
                          #`(check-string-and-indent e #,(source-string sfd bfp) (fx- (column) 1))
                          (extract-string-and-reset! tb)
                          a)
                        new-bfp)]
                     [else (source-error sfd bfp "found more than one expression within @(---)")]))]
                [(initial-id-char? c)
                 (let ([str (extract-string-and-reset! tb)])
                   (extend-string-buffer! tb #\@)
                   (extend-string-buffer! tb c)
                   (s2 (cons str a) (+ bfp 1) bfp))]
                [else (source-error sfd bfp "expected open paren or @ following @")])))
          (define (s2 a bfp token-start-bfp)
            (let ([c (read-char ip)])
              (cond
                [(eof-object? c) (close-input-port ip) (finish-identifier a bfp token-start-bfp)]
                [(id-char? c) (extend-string-buffer! tb c) (s2 a (+ bfp 1) token-start-bfp)]
                [else (unread-char c ip) (finish-identifier a bfp token-start-bfp)])))
          (define (finish-identifier a bfp token-bfp)
            (let* ([token (extract-string-and-reset! tb)]
                   [@kw (datum->syntax k (string->symbol token))]
                   [p (r @kw)])
              (unless p (source-error sfd token-bfp "unrecognized token ~a" token))
              (call-with-values (lambda () (p k ip sfd a bfp token-bfp)) s0)))
          (s0 '() 0)))
      (syntax-case x ()
        [(k fn)
         (string? (datum fn))
         (lambda (r)
           (with-syntax ([(e ...) (process-template-file r (datum fn) #'k)])
             #'(let ([filename fn])
                 (let-values ([(op p line column) (open-positional-string-output-port)])
                   (display e op) ...
                   (p)))))])))

  (define-syntax define-template-extension
    (lambda (x)
      (define who 'define-template-extension)
      (define (make-prefix-id prefix kw)
        (datum->syntax kw
          (string->symbol
            (string-append prefix (symbol->string (syntax->datum kw))))))
      (define build-matcher
        (case-lambda
          [(kw)
           (with-syntax ([kw kw] [@kw (make-prefix-id "@" kw)])
             #'[@kw (lambda (k ip sfd stack bfp token-bfp)
                      (let-values ([(stack bfp) (strip-blanks ip stack bfp)])
                        (values (cons (make-incomplete-node 'kw #f token-bfp) stack) bfp)))])]
          [(kw expr)
           (with-syntax ([kw kw] [@kw (make-prefix-id "@" kw)] [(expr ...) expr])
             #'[@kw (lambda (k ip sfd stack bfp token-bfp)
                      (let-values ([(e* new-bfp) (read-scheme k ip sfd bfp)])
                        (syntax-case e* ()
                          [(expr ...)
                           (let-values ([(stack new-bfp) (strip-blanks ip stack new-bfp)])
                             (values (cons (make-incomplete-node 'kw e* token-bfp) stack) new-bfp))]
                          [_ (source-error sfd token-bfp "expected @~s~s syntax, but got @~s~s"
                               'kw '(expr ...) 'kw (syntax->datum e*))])))])]))
      (define (check-id id)
        (let* ([str (symbol->string (syntax->datum id))]
               [len (string-length str)])
          (unless (and (> len 0) (initial-id-char? (string-ref str 0))
                       (let loop ([len len])
                         (or (= len 0)
                             (let ([len (- len 1)])
                               (and (id-char? (string-ref str len)) (loop len))))))
            (syntax-violation who "invalid template keyword" id))))
      (define (check-unique! type ids)
        (let loop ([ids ids])
          (syntax-case ids ()
            [(id rest ...)
             (if (memq (datum id) (datum (rest ...)))
                 (syntax-violation who (format "one or more ~a used more than once" type) #'id #'(rest ...))
                 (loop #'(rest ...)))]
            [() (void)])))
      (define (check-syntax-unique! type maybe-expr*)
        (check-unique! type
          (let f ([stx maybe-expr*] [ids '()])
            (syntax-case stx ()
              [id (and (identifier? #'id) (not (memq (datum id) '(... unquote quote)))) (cons #'id ids)]
              [(a . d) (f #'a (f #'d ids))]
              [_ ids]))))
      (define (build-check kw tmpl x)
        #`(unless #,(if x #`(and #,x #,tmpl) tmpl)
            (source-error sfd token-bfp "found ~s without required ~s" token '#,kw)))
      (define (build-initial-values bindings list?*)
        (fold-right (lambda (binding list? init-val**)
                      (cons
                        (if list?
                            (make-list (length binding) #''())
                            (make-list (length binding) #'#f))
                        init-val**))
          '() bindings list?*))
      (define (build-bodies list?* tmpls updates bindings)
        (let f ([list?* list?*] [tmpls tmpls] [updates updates] [bindings bindings] [rbindings '()])
          (if (null? list?*)
              '()
              (with-syntax ([(checks ...)
                             (if (car list?*)
                                 '()
                                 #`((when #,(car tmpls)
                                      (source-error token-bfp "found more @~s than expected" type))))]
                            [((args ...) ...) (fold-left (lambda (args binding) (cons binding args))
                                                (cons (car updates) (cdr bindings)) rbindings)])
                (cons #'(begin checks ... (loop (cdr stack) '() args ... ...))
                  (f (cdr list?*) (cdr tmpls) (cdr updates) (cdr bindings) (cons (car bindings) rbindings)))))))
      (define (process-template output pat)
        (define (squawk type)
          (syntax-violation who (format "extension cannot start with ~s keyword" type) pat))
        (syntax-case pat (optional)
          [((optional kw (expr ...) tmpl) . rest)
           (and (identifier? #'kw) (identifier? #'tmpl))
           (squawk 'optional)]
          [((optional kw tmpl) . rest)
           (and (identifier? #'kw) (identifier? #'tmpl))
           (squawk 'optional)]
          [((kw (expr ...) tmpl) dots . rest)
           (and (eq? (datum dots) '...) (identifier? #'kw) (identifier? #'tmpl))
           (squawk 'list)]
          [((kw tmpl) dots . rest)
           (and (eq? (datum dots) '...) (identifier? #'kw) (identifier? #'tmpl))
           (squawk 'optional)]
          [(kw (expr ...) tmpl . rest)
           (and (identifier? #'kw) (identifier? #'tmpl))
           (process-rest output #'kw #'rest
             (list (build-matcher #'kw #'(expr ...)))
             #'([tmpl #`(string-append #,@rstack)]
                [(expr ...) (incomplete-node-e* item)]))]
          [(kw tmpl . rest)
           (and (identifier? #'kw) (identifier? #'tmpl))
           (process-rest output #'kw #'rest (list (build-matcher #'kw))
             #'([tmpl #`(string-append #,@rstack)]))]
          [(kw (expr ...))
           (with-syntax ([@kw (make-prefix-id "@" #'kw)] [output output])
             #'([@kw (lambda (k ip sfd stack bfp token-bfp)
                       (let-values ([(e* new-bfp) (read-scheme k ip sfd bfp)])
                         (syntax-case e* ()
                           [(expr ...) (values (cons #`output stack) new-bfp)]
                           [_ (source-error sfd token-bfp "expected @~s~s syntax, but got @~s~s"
                                'kw '(expr ...) 'kw (syntax->datum e*))])))]))]
         [(kw)
          (with-syntax ([@kw (make-prefix-id "@" #'kw)] [output output])
            #'([@kw (lambda (k ip sfd stack bfp indent token-bfp)
                      (values (cons #`output stack) bfp indent))]))]))
      (define (process-rest output first-kw rest as* matches)
        (let f ([pat rest]
                [as* as*]
                [checks '()]
                [kws '()]
                [tmpls '()]
                [list?* '()]
                [bindings '()]
                [updates '()]
                [exprs '()]
                [matches matches])
          (syntax-case pat (optional)
            [((optional kw (expr ...) tmpl) . rest)
             (and (identifier? #'kw) (identifier? #'tmpl))
             (with-syntax ([(t) (generate-temporaries '(t))])
               (f #'rest
                  (cons (build-matcher #'kw #'(expr ...)) as*) checks
                  (cons #'kw kws) (cons #'tmpl tmpls) (cons #f list?*)
                  (cons (list #'tmpl #'t) bindings)
                  (cons (list #'#`(string-append #,@rstack) #'(incomplete-node-e* item)) updates)
                  (cons #'(expr ...) exprs) (cons* #'[tmpl tmpl] #'[(expr ...) #'t] matches)))]
            [((optional kw tmpl) . rest)
             (and (identifier? #'kw) (identifier? #'tmpl))
             (f #'rest
                (cons (build-matcher #'kw) as*) checks
                (cons #'kw kws) (cons #'tmpl tmpls) (cons #f list?*) (cons (list #'tmpl) bindings)
                (cons (list #'#`(string-append #,@rstack)) updates)
                (cons  #f exprs) (cons #'[tmpl tmpl] matches))]
            [((kw (expr ...) tmpl) dots . rest)
             (and (eq? (datum dots) '...) (identifier? #'kw) (identifier? #'tmpl))
             (with-syntax ([(t*) (generate-temporaries '(t*))])
               (f #'rest
                  (cons (build-matcher #'kw #'(expr ...)) as*) checks
                  (cons #'kw kws) (cons #'tmpl tmpls) (cons #t list?*) (cons (list #'tmpl #'t*) bindings)
                  (cons (list #'(cons #`(string-append #,@rstack) tmpl) #'(cons (incomplete-node-e* item) t*)) updates)
                  (cons #'(expr ...) exprs) (cons* #'[(tmpl (... ...)) tmpl] #'[((expr ...) (... ...)) t*] matches)))]
            [((kw tmpl) dots . rest)
             (and (eq? (datum dots) '...) (identifier? #'kw) (identifier? #'tmpl))
             (f #'rest
                (cons (build-matcher #'kw) as*) checks
                (cons #'kw kws) (cons #'tmpl tmpls) (cons #t list?*) (cons (list #'tmpl) bindings)
                (cons (list #'(cons #`(string-append #,@rstack) tmpl)) updates)
                (cons #f exprs) (cons* #'[(tmpl (... ...)) tmpl] matches))]
            [(kw (expr ...) tmpl . rest)
             (and (identifier? #'kw) (identifier? #'tmpl))
             (with-syntax ([(t) (generate-temporaries '(t))])
               (f #'rest
                  (cons (build-matcher #'kw #'(expr ...)) as*)
                  (cons (build-check #'kw #'tmpl #'t) checks)
                  (cons #'kw kws) (cons #'tmpl tmpls) (cons #f list?*) (cons (list #'tmpl #'t) bindings)
                  (cons (list #'#`(string-append #,@rstack) #'(incomplete-node-e* item)) updates)
                  (cons #'(expr ...) exprs) (cons* #'[tmpl tmpl] #'[(expr ...) #'t] matches)))]
            [(kw tmpl . rest)
             (and (identifier? #'kw) (identifier? #'tmpl))
             (f #'rest
                (cons (build-matcher #'kw) as*) 
                (cons (build-check #'kw #'tmpl #f) checks)
                (cons #'kw kws) (cons #'tmpl tmpls) (cons #f list?*) (cons (list #'tmpl) bindings)
                (cons (list #'#`(string-append #,@rstack)) updates)
                (cons  #f exprs) (cons #'[tmpl tmpl] matches))]
            [() 
             (begin
               (for-each check-id kws)
               (check-unique! "keyword" kws)
               (check-unique! "template bindings" tmpls)
               (check-syntax-unique! "scheme syntax matching expressions" exprs)
               (cons
                 (with-syntax ([startkw first-kw]
                               [endkw (make-prefix-id "end" first-kw)]
                               [@endkw (make-prefix-id "@end" first-kw)]
                               [output output]
                               [(matches ...) matches]
                               [(checks ...) checks]
                               [((x ...) ...) bindings]
                               [((init-val ...) ...) (build-initial-values bindings list?*)]
                               [(kw ...) kws]
                               [(body ...) (build-bodies list?* tmpls updates bindings)])
                   #'[@endkw (lambda (k ip sfd stack bfp token-bfp)
                               (let-values ([(stack bfp) (strip-blanks ip stack bfp)])
                                 (let loop ([stack stack] [rstack '()] [x init-val] ... ...)
                                   (if (null? stack)
                                       (source-error sfd token-bfp "found @~s with no initial @~s" 'endkw 'startkw)
                                       (let ([item (car stack)])
                                         (if (incomplete-node? item)
                                             (let ([type (incomplete-node-type item)])
                                               (case type
                                                 [(startkw) checks ...
                                                   (with-syntax (matches ...)
                                                     (values (cons #`output (cdr stack)) bfp))]
                                                 [(kw) body] ...
                                                 [else (source-error sfd token-bfp
                                                         "found unexpected @~s (~a) instead of expected @~s before @~s"
                                                         type (source-string sfd (incomplete-node-bfp item)) 'startkw 'endkw)]))
                                             (loop (cdr stack) (cons item rstack) x ... ...)))))))])
                 as*))]
            [_ (syntax-violation who "unrecognized pattern" pat)])))
      (syntax-case x ()
        [(_ pat output)
         (with-syntax ([([@kw proc] ...) (process-template #'output #'pat)])
           #'(begin (define-syntax @kw (make-compile-time-value proc)) ...))])))

  (define-template-extension (num (e)) (number->string e))

  (define-template-extension (for ([binding e] [bindings es] ...) tmpl)
    (with-output-to-string
      (lambda ()
        (for-each (lambda (binding bindings ...) (display tmpl)) e es ...))))

  (define-template-extension (if (expr) tmpl (elif (exprs) tmpls) ... (optional else else-tmpl))
    (if expr
        tmpl
        #,(let f ([exprs #'(exprs ...)] [tmpls #'(tmpls ...)])
            (if (null? exprs)
                (or #'else-tmpl #'"")
                (with-syntax ([expr (car exprs)] [tmpl (car tmpls)] [else (f (cdr exprs) (cdr tmpls))])
                  #'(if expr
                        tmpl
                        else)))))))
#!eof
-------- saving remainder of file to /tmp/t and running /tmp/t should produce only "end of tests" --------
#!/bin/tcsh

cat >! /tmp/spam.h << END
extern void @(name)(void);
END

cat >! /tmp/spam.c << END
#include <stdio.h>

@((include-template "/tmp/spam.h"))

void @(name)() {
  @(name)();
}
END
scheme -q << END
(import (template))
(unless (equal?
          (let ([name "bob"]) (include-template "/tmp/spam.c"))
          "#include <stdio.h>\n\nextern void bob(void);\n\n\nvoid bob() {\n  bob();\n}\n")
  (error #f "test 1 failed"))
END

cat >! /tmp/spam.c << END
(import (template))
(unless (equal?
          (guard (c [else (with-output-to-string (lambda () (display-condition c)))])
            (expand '(let ([name "bob"]) (include-template "/tmp/spam.c"))))
          "Exception in get-datum/annotations: unexpected end-of-file reading list at line 6, char 4 of /tmp/spam.c")
  (error #f "test 2 failed"))
END

cat >! /tmp/spam.c << END
#include <stdio.h>

@((include-template "/tmp/spam.h"))

void @(name)() {
  @(name)();
  @
}
END
scheme -q << END
(import (template))
(unless (equal?
          (guard (c [else (with-output-to-string (lambda () (display-condition c)))])
            (expand '(let ([name "bob"]) (include-template "/tmp/spam.c"))))
          "Exception in include-template: expected open paren or @ following @ at line 7, char 4 of /tmp/spam.c")
  (error #f "test 3 failed"))
END

cat >! /tmp/spam.c << END
#include <stdio.h>

@((include-template "/tmp/spam.h"))

void @(name)() {
  @(name)();
}
END
echo -n "@" >> /tmp/spam.c
scheme -q << END
(import (template))
(unless (equal?
          (guard (c [else (with-output-to-string (lambda () (display-condition c)))])
            (expand '(let ([name "bob"]) (include-template "/tmp/spam.c"))))
          "Exception in include-template: expected open paren or @ following @ at line 8, char 2 of /tmp/spam.c")
  (error #f "test 4 failed"))
END

cat >! /tmp/spam.c << END
#include <stdio.h>

@((include-template #xGO! "/tmp/spam.h"))

void @(name)() {
  @(name)();
}
END
echo -n "@" >> /tmp/spam.c
scheme -q << END
(import (template))
(unless (equal?
          (guard (c [else (with-output-to-string (lambda () (display-condition c)))])
            (expand '(let ([name "bob"]) (include-template "/tmp/spam.c"))))
          "Exception in get-datum/annotations: invalid number syntax #xGO! at line 3, char 21 of /tmp/spam.c")
  (error #f "test 5 failed"))
END

cat >! /tmp/spam.c << END
#include <stdio.h>

@((include-template))

void @(name)() {
  @(name)();
}
END
scheme -q << END
(import (template))
(unless (equal?
          (guard (c [else (with-output-to-string (lambda () (display-condition c)))])
            (expand '(let ([name "bob"]) (include-template "/tmp/spam.c"))))
          "Exception: invalid syntax (include-template) at line 3, char 3 of /tmp/spam.c")
  (error #f "test 6 failed"))
END

cat >! /tmp/spam.c <<END
#include <stdio.h>

/* function: @(name)
 *
 * @@param: @(name)
 */
@((include-template "/tmp/spam.h"))

void @(name)() {
  @(name)();
}
END
scheme -q << END
(import (template))
(unless (equal? (let ([name "bob"]) (include-template "/tmp/spam.c"))
          "#include <stdio.h>\n\n/* function: bob\n *\n * @param: bob\n */\nextern void bob(void);\n\n\nvoid bob() {\n  bob();\n}\n")
  (error #f "test 7 failed"))
END

cat >! /tmp/rockets <<END
Test
@for([x xs])
@num(x) ...
@endfor
Blast off!
END
scheme -q << END
(import (template))
(unless (equal? (let ([xs '(3 2 1)]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\n")
  (error #f "test 8 failed"))
END
cat >> /tmp/rockets <<END
@if((= y 0))
Again!
@elif((= y 1))
Rockets are awseome!
@elif((= y 2))
Explosions!
@else
That was fun.
@endif
END
scheme -q << END
(import (template))
(unless (equal? (let ([xs '(3 2 1)] [y 0]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nAgain!\n")
  (error #f "test 9 failed"))
END
scheme -q << END
(import (template))
(unless (equal? (let ([xs '(3 2 1)] [y 1]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nRockets are awseome!\n")
  (error #f "test 10 failed"))
END
scheme -q << END
(import (template))
(unless (equal? (let ([xs '(3 2 1)] [y 2]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nExplosions!\n")
  (error #f "test 11 failed"))
END
scheme -q << END
(import (template))
(unless (equal? (let ([xs '(3 2 1)] [y 3]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nThat was fun.\n")
  (error #f "test 12 failed"))
END
cat >> /tmp/rockets <<END
@join(" and " [a as])@(a)@endjoin
END
scheme -q << END
(import (template))
(define-template-extension (join (?str [binding ls-expr] [bindings ls-exprs] ...) tmpl)
  #,(with-syntax ([(t ts ...) (generate-temporaries #'(binding bindings ...))])
      #'(with-output-to-string
          (lambda ()
            (let ([str ?str] [t ls-expr] [ts ls-exprs] ...)
              (if (null? t)
                  ""
                  (let loop ([t t] [ts ts] ...)
                    (let ([binding (car t)] [bindings (car ts)] ... [t (cdr t)] [ts (cdr ts)] ...)
                      (display tmpl)
                      (unless (null? t)
                        (display str)
                        (loop t ts ...))))))))))
(unless (equal? (let ([xs '(3 2 1)] [y 3] [as '()]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nThat was fun.\n")
  (error #f "test 13 failed"))
END
scheme -q << END
(import (template))
(define-template-extension (join (?str [binding ls-expr] [bindings ls-exprs] ...) tmpl)
  #,(with-syntax ([(t ts ...) (generate-temporaries #'(binding bindings ...))])
      #'(with-output-to-string
          (lambda ()
            (let ([str ?str] [t ls-expr] [ts ls-exprs] ...)
              (if (null? t)
                  ""
                  (let loop ([t t] [ts ts] ...)
                    (let ([binding (car t)] [bindings (car ts)] ... [t (cdr t)] [ts (cdr ts)] ...)
                      (display tmpl)
                      (unless (null? t)
                        (display str)
                        (loop t ts ...))))))))))
(unless (equal? (let ([xs '(3 2 1)] [y 3] [as '("a")]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nThat was fun.\na")
  (error #f "test 14 failed"))
END
scheme -q << END
(import (template))
(define-template-extension (join (?str [binding ls-expr] [bindings ls-exprs] ...) tmpl)
  #,(with-syntax ([(t ts ...) (generate-temporaries #'(binding bindings ...))])
      #'(with-output-to-string
          (lambda ()
            (let ([str ?str] [t ls-expr] [ts ls-exprs] ...)
              (if (null? t)
                  ""
                  (let loop ([t t] [ts ts] ...)
                    (let ([binding (car t)] [bindings (car ts)] ... [t (cdr t)] [ts (cdr ts)] ...)
                      (display tmpl)
                      (unless (null? t)
                        (display str)
                        (loop t ts ...))))))))))
(unless (equal? (let ([xs '(3 2 1)] [y 3] [as '("a" "b")]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nThat was fun.\na and b")
  (error #f "test 15 failed"))
END
scheme -q << END
(import (template))
(define-template-extension (join (?str [binding ls-expr] [bindings ls-exprs] ...) tmpl)
  #,(with-syntax ([(t ts ...) (generate-temporaries #'(binding bindings ...))])
      #'(with-output-to-string
          (lambda ()
            (let ([str ?str] [t ls-expr] [ts ls-exprs] ...)
              (if (null? t)
                  ""
                  (let loop ([t t] [ts ts] ...)
                    (let ([binding (car t)] [bindings (car ts)] ... [t (cdr t)] [ts (cdr ts)] ...)
                      (display tmpl)
                      (unless (null? t)
                        (display str)
                        (loop t ts ...))))))))))
(unless (equal? (let ([xs '(3 2 1)] [y 3] [as '("a" "b" "c")]) (include-template "/tmp/rockets"))
          "Test\n3 ...\n2 ...\n1 ...\nBlast off!\nThat was fun.\na and b and c")
  (error #f "test 16 failed"))
END
cat >! /tmp/indent-test.c << END
#include <stdio.h>

int main(int argc, char *argv[]) {
  @(body)
}
END
scheme -q << END
(import (template))
(unless (equal? (let ([body "printf(\"Hello, world!\\\\n\");\nprintf(\"So... uh, what's going on?\\\\n\");\nprintf(\"Well, goodbye then.\\\\n\");"])
                  (include-template "/tmp/indent-test.c"))
          "#include <stdio.h>\n\nint main(int argc, char *argv[]) {\n  printf(\"Hello, world!\\\\n\");\n  printf(\"So... uh, what's going on?\\\\n\");\n  printf(\"Well, goodbye then.\\\\n\");\n}\n")
  (error #f "test 17 failed"))
END
cat >! /tmp/hygeine << END
Hygiene test:

@for([t i*])
  got @(t) .
@endfor

@for([x j*])
  got @(x) and t is @(t).
@endfor

@let([outer-t t])
  @for([x i*] [t j*])
    got @(x) and @(t) and @(outer-t).
  @endfor
@endlet
END
scheme -q << END
(import (template))
(let ()
  (define-template-extension (for ([x e] [xs es] ...) tmpl)
    (let ([t e])
      (apply string-append (map (lambda (x xs ...) tmpl) t es ...))))
  (define-template-extension (let ([x e] [xs es] ...) tmpl)
    (let ([x e] [xs es] ...) tmpl))
  (unless (equal? (let ([t "10"] [i* '("1" "2" "3")] [j* '("a" "b" "c")])
                    (include-template "/tmp/hygeine"))
            "Hygiene test:\n\n  got 1 .\n  got 2 .\n  got 3 .\n\n  got a and t is 10.\n  got b and t is 10.\n  got c and t is 10.\n\n    got 1 and a and 10.\n    got 2 and b and 10.\n    got 3 and c and 10.\n")
    (error #f "test 18 failed")))
END
cat >! /tmp/indent << END
This is to test indents:
  simple indent: @(x)

  nested indents: @(y)   @(x)
END
scheme -q << END
(import (template))
(unless (equal? (let ([x "a\nb\nc\n"] [y "x\ny\nz"])
                  (include-template "/tmp/indent"))
          "This is to test indents:\n  simple indent: a\n                 b\n                 c\n\n\n  nested indents: x\n                  y\n                  z   a\n                      b\n                      c\n\n")
  (error #f "test 19 failed"))
END
echo "end of tests"

