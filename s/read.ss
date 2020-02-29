;;; read.ss
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

;;; this makes loading this file independently bomb
;(define read)
;(define $read)

(begin
(let ()
(include "types.ss")

(define-record-type rcb
  (nongenerative)
  (sealed #t)
  (fields
    ip      ; input port
    sfd     ; a source-file descriptor or #f
    a?      ; if true, wrap s-expressions with source annotations
    who     ; who's calling (read, read-token)
    ))

;;; xdefine, xcall, xmvlet, and xvalues manage implicit arguments and
;;; return values for most of the procedures defined in this file.  This
;;; simplifies the code and makes it much easier to add new arguments
;;; universally.  The implicit variables are:
;;;   [i ] rcb      reader control block
;;;   [io] fp       current file position or #f
;;;   [i ] bfp      beginning file position or #f
;;;   [io] tb       token buffer
;;;   [io] it       insert table (for marks and references)
;;; i: input (xcall argument)
;;; o: output (xvalues return value)

(define-syntax xlambda
  (lambda (x)
    (syntax-case x ()
      ((key args b1 b2 ...)
       (with-implicit (key rcb fp bfp tb it)
         #'(lambda (rcb fp bfp tb it . args) b1 b2 ...))))))

(define-syntax xdefine
  (lambda (x)
    (syntax-case x ()
      ((key (name . args) b1 b2 ...)
       (with-implicit (key rcb fp bfp tb it)
         #'(define (name rcb fp bfp tb it . args) b1 b2 ...))))))

(define-syntax xcall
  (lambda (x)
    (syntax-case x ()
      ((key p arg ...)
       (with-implicit (key rcb fp bfp tb it)
         #'(p rcb fp bfp tb it arg ...))))))

(define-syntax xmvlet
  (lambda (x)
    (syntax-case x ()
      ((key ((arg ...) exp) b1 b2 ...)
       (with-implicit (key fp tb it)
         #'(call-with-values
             (lambda () exp)
             (lambda (fp tb it arg ...) b1 b2 ...)))))))

(define-syntax xvalues
  (lambda (x)
    (syntax-case x ()
      ((key arg ...)
       (with-implicit (key fp tb it)
         #'(values fp tb it arg ...))))))

;;; state-lambda, define-state, *state, and state-return are used to form
;;; scanner states.  They are defined in terms of xdefine and family.
(define-syntax state-lambda
  (lambda (x)
    (syntax-case x ()
      ((key (arg ...) b1 b2 ...)
       (with-implicit (key xlambda)
         #'(xlambda (arg ...) b1 b2 ...))))))

(define-syntax define-state
  (lambda (x)
    (syntax-case x ()
      ((key (name arg ...) b1 b2 ...)
       (with-implicit (key xdefine)
         #'(xdefine (name arg ...) b1 b2 ...))))))

(define-syntax *state ; move to state
  (lambda (x)
    (syntax-case x ()
      ((key s arg ...)
       (with-implicit (key xcall)
         #'(xcall s arg ...))))))

(define-syntax state-return
  (lambda (x)
    (syntax-case x ()
      ((key type value)
       (with-implicit (key xvalues bfp)
         #'(xvalues bfp 'type value))))))

(define-syntax call-with-token
  (lambda (x)
    (syntax-case x ()
      ((key pexpr)
       (with-implicit (key xcall xmvlet bfp)
         #'(let ([p pexpr])
             (xmvlet ((bfp type value) (xcall rd-token))
               (xcall p type value))))))))

(define-syntax with-token
  (lambda (x)
    (syntax-case x ()
      ((key (type value) expr1 expr2 ...)
       (and (identifier? #'type) (identifier? #'value))
       (with-implicit (key xcall xmvlet bfp)
         #'(xmvlet ((bfp type value) (xcall rd-token))
             expr1 expr2 ...))))))

;;; token-buffers are represented as strings and are expanded as necessary
;;; by the stretch form, which otherwise behaves much like string-set!
(define-syntax with-stretch-buffer
  (lambda (x)
    (syntax-case x ()
      ((key i c b1 b2 ...)
       (with-implicit (key tb)
         #'(let ((g i) (t c))
             (let ((tb (if (fx= g (string-length tb)) (string-stretch tb) tb)))
               (string-set! tb g t)
               b1 b2 ...)))))))

;;; insert tables are eq hash tables; could simply use vector that grows
;;; on demand, except that programmer might use widely spread numbers or
;;; even bignums
(define-syntax with-insert-table
  (lambda (x)
    (syntax-case x ()
      ((key b1 b2 ...)
       (with-implicit (key it)
         #'(let ((it (or it (make-eq-hashtable)))) b1 b2 ...))))))

;;; with-read-char, with-peek-char, and with-unread-char
;;; manage the fp (file-position) value
(define-syntax with-read-char
  (lambda (x)
    (syntax-case x ()
      ((key id b1 b2 ...)
       (identifier? #'id)
       (with-implicit (key rcb fp)
         #'(let ([id (read-char (rcb-ip rcb))])
             (let ((fp (and fp (+ fp 1))))
               b1 b2 ...)))))))

(define-syntax with-peek-char
  (lambda (x)
    (syntax-case x ()
      ((key id b1 b2 ...)
       (identifier? #'id)
       (with-implicit (key rcb)
         #'(let ((id (peek-char (rcb-ip rcb)))) b1 b2 ...))))))

(define-syntax with-unread-char
  (lambda (x)
    (syntax-case x ()
      [(key ?c b1 b2 ...)
       (with-implicit (key rcb fp)
         #'(let ([c ?c])
             (unless (eof-object? c) (unread-char c (rcb-ip rcb)))
             (let ([fp (and fp (- fp 1))]) b1 b2 ...)))])))

(define-record-type delayed-record
  (fields (immutable rtd) (immutable vals) (immutable bfp) (immutable efp) (mutable update))
  (nongenerative)
  (sealed #t)
  (protocol
    (lambda (new)
      (lambda (rtd vals bfp efp)
        (new rtd vals bfp efp (lambda (x) x))))))

(define-record-type insert
  (fields (immutable n) (immutable bfp) (immutable efp) (mutable obj) (mutable seen) (mutable visited))
  (nongenerative)
  (sealed #t)
  (protocol
    (lambda (new)
      (lambda (n bfp efp)
        (new n bfp efp #f #f #f)))))

(define char-name-table (make-hashtable char->integer char=?))

(define digit-value
  (lambda (c r)
    (let ([v (cond
               [(char<=? #\0 c #\9) (char- c #\0)]
               [(char<=? #\A c #\Z) (char- c #\7)]
               [(char<=? #\a c #\z) (char- c #\W)]
               [else 36])])
      (and (fx< v r) v))))

(module (maybe-fold/intern maybe-fold/gensym)
  (define (fold-case? ip slashed?)
    (cond
      [slashed? #f]
      [($port-flags-set? ip (constant port-flag-fold-case)) #t]
      [($port-flags-set? ip (constant port-flag-no-fold-case)) #f]
      [else (not (case-sensitive))]))

  (define maybe-fold/intern
    (case-lambda
      [(ip tb n slashed?)
       (if (fold-case? ip slashed?)
           (string->symbol (string-foldcase (substring tb 0 n)))
           ($intern2 tb n))]
      [(ip tb n m slashed1? slashed2?)
       (if (fold-case? ip slashed1?)
           (let* ([s1 (string-foldcase (substring tb 0 n))]
                  [s2 (string-append s1
                        (if (fold-case? ip slashed2?)
                            (string-foldcase (substring tb n m))
                            (substring tb n m)))])
             ($intern3 s2 (string-length s1) (string-length s2)))
           (if (fold-case? ip slashed2?)
               (let ([s2 (string-append
                           (substring tb 0 n)
                           (string-foldcase (substring tb n m)))])
                 ($intern3 s2 n (string-length s2)))
               ($intern3 tb n m)))]))

  (define maybe-fold/gensym
    (lambda (ip tb n slashed?)
      (if (fold-case? ip slashed?)
          (gensym (string-foldcase (substring tb 0 n)))
          (gensym (substring tb 0 n))))))

(define string-stretch
  (lambda (old)
   ; string overhead header plus 1 for null byte
    (define overhead-bytes (+ (constant header-size-string) 1))
   ; allocator will align anyway, so start with as many as we
   ; can without grabbing more real space
    (define smart-size
      (lambda (n)
        (fx- (c-alloc-align (fx+ overhead-bytes (fx* n (constant string-char-bytes))))
             overhead-bytes)))
    (let ([n (string-length old)])
      (if (fx= n 0)
         ; start with at least 5 characters.
          (make-string (smart-size 5))
         ; double n and double the overhead doubles the whole object
         ; and thereby maintains the alignment w/o wasting space
          (let ([new (make-string (fx+ n n overhead-bytes))])
            (do ([i 0 (fx+ i 1)])
                ((fx= i n) new)
              (string-set! new i (string-ref old i))))))))

(define-syntax $make-source-object
  (lambda (stx)
    (syntax-case stx ()
      [(_ sfd-expr bfp-expr efp-expr)
       #'(let ([sfd sfd-expr]
               [bfp bfp-expr]
               [efp efp-expr])
           (if ($current-mso)
               (($current-mso) sfd bfp efp)
               (make-source sfd bfp efp)))])))

(xdefine (rd-error ir? start? msg . args)
  (let ([ip (rcb-ip rcb)])
    (cond
      [(eq? ip (console-input-port)) ($lexical-error (rcb-who rcb) msg args ip ir?)]
      [(not fp)
       (let ([pos (and (port-has-port-position? ip) (port-position ip))])
         (if pos
             ($lexical-error (rcb-who rcb) "~? before file-position ~s of ~s; the character position might differ" (list msg args pos ip) ip ir?)
             ($lexical-error (rcb-who rcb) "~? on ~s" (list msg args ip) ip ir?)))]
      [(rcb-sfd rcb) ($lexical-error (rcb-who rcb) msg args ip ($make-source-object (rcb-sfd rcb) bfp fp) start? ir?)]
      [else ($lexical-error (rcb-who rcb) "~? at char ~a of ~s" (list msg args (if start? bfp fp) ip) ip ir?)])))

(xdefine (rd-eof-error s)
  (xcall rd-error #f #t "unexpected end-of-file reading ~a" s))

(xdefine (rd-delimiter-error c what)
  (xcall rd-error #f #t "invalid delimiter ~a for ~a" c what))

(xdefine (rd-nonstandard-error s)
  (xcall rd-error #f #t "~a syntax is not allowed in #!r6rs mode" s))

(define-syntax nonstandard
  (lambda (x)
    (syntax-case x ()
      [(k str)
       (with-implicit (k rcb xcall)
         #'(when ($port-flags-set? (rcb-ip rcb) (constant port-flag-r6rs))
             (xcall rd-nonstandard-error str)))])))

(xdefine (rd-nonstandard-delimiter-error c)
  (xcall rd-error #f #t "delimiter ~a is not allowed in #!r6rs mode" c))

(define-syntax nonstandard-delimiter
  (lambda (x)
    (syntax-case x ()
      [(k c)
       (with-implicit (k rcb xcall)
         #'(when ($port-flags-set? (rcb-ip rcb) (constant port-flag-r6rs))
             (xcall rd-nonstandard-delimiter-error c)))])))

(define-state (rd-token)
  (let ((bfp fp))
    (with-read-char c
      (state-case c
        [eof (with-unread-char c (state-return eof c))]
        [(#\space #\newline #\tab) (*state rd-token)]
        [((#\a - #\z) (#\A - #\Z))
         (with-stretch-buffer 0 c
           (with-read-char c
             (*state rd-token-symbol c 1 #f rd-token-intern)))]
        [#\( (state-return lparen #f)]
        [#\) (state-return rparen #f)]
        [#\[ (state-return lbrack #f)]
        [#\] (state-return rbrack #f)]
        [#\' (state-return quote 'quote)]
        [((#\0 - #\9)) (with-stretch-buffer 0 c (*state rd-token-number-or-symbol 1))]
        [(#\-) (*state rd-token-minus)]
        [(#\+) (*state rd-token-plus)]
        [#\; (*state rd-token-comment)]
        [#\# (*state rd-token-hash)]
        [#\" (*state rd-token-string 0)]
        [#\. (*state rd-token-dot)]
        [#\, (*state rd-token-comma)]
        [#\` (state-return quote 'quasiquote)]
        [(#\* #\= #\< #\> #\/ #\! #\$ #\% #\& #\: #\? #\^ #\_ #\~)
         (with-stretch-buffer 0 c
           (with-read-char c
             (*state rd-token-symbol c 1 #f rd-token-intern)))]
        [$constituent?
         (with-stretch-buffer 0 c
           (with-read-char c
             (*state rd-token-symbol c 1 #f rd-token-intern)))]
        [#\| (*state rd-token-symbol c 0 #f rd-token-intern)]
        [#\\ (*state rd-token-symbol c 0 #f rd-token-intern)]
        [#\{ (nonstandard "open brace") (state-return atomic '{)]
        [#\} (nonstandard "close brace") (state-return atomic '})]
        [(#\page #\return) (*state rd-token)]
        [char-whitespace? (*state rd-token)]
        [else (*state rd-token-symbol c 0 #f rd-token-intern-nonstandard)]))))

(define-state (rd-token-minus)
  (with-peek-char c
    (state-case c
      [eof (state-return atomic '-)]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#) (state-return atomic '-)]
      [(#\>)
       (with-stretch-buffer 0 #\-
         (with-read-char c
           (*state rd-token-symbol c 1 #f rd-token-intern)))]
      [char-whitespace? (state-return atomic '-)]
      [(#\{ #\} #\' #\` #\,) (nonstandard-delimiter c) (state-return atomic '-)]
      [else (with-stretch-buffer 0 #\- (*state rd-token-number-or-symbol 1))])))

(define-state (rd-token-plus)
  (with-peek-char c
    (state-case c
      [eof (state-return atomic '+)]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#) (state-return atomic '+)]
      [char-whitespace? (state-return atomic '+)]
      [(#\{ #\} #\' #\` #\,) (nonstandard-delimiter c) (state-return atomic '+)]
      [else (with-stretch-buffer 0 #\+ (*state rd-token-number-or-symbol 1))])))

(define-state (rd-token-dot)
  (with-peek-char c
    (state-case c
      [eof (state-return dot #f)]
      [(#\.) (with-read-char c (*state rd-token-dot-dot))]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#) (state-return dot #f)]
      [char-whitespace? (state-return dot #f)]
      [(#\{ #\} #\' #\` #\,) (nonstandard-delimiter c) (state-return dot #f)]
      [else (with-stretch-buffer 0 #\. (*state rd-token-number-or-symbol 1))])))

(define-state (rd-token-dot-dot)
  (with-read-char c
    (state-case c
      [eof (nonstandard ".. symbol") (state-return atomic '..)]
      [(#\.) (*state rd-token-dot-dot-dot)]
      [else (with-stretch-buffer 0 #\.
              (with-stretch-buffer 1 #\.
                (*state rd-token-symbol c 2 #f
                  rd-token-intern-nonstandard)))])))

(define-state (rd-token-dot-dot-dot)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (state-return atomic '...))]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#)
       (with-unread-char c (state-return atomic '...))]
      [char-whitespace?
       (with-unread-char c (state-return atomic '...))]
      [(#\{ #\} #\' #\` #\,)
       (nonstandard-delimiter c)
       (with-unread-char c (state-return atomic '...))]
      [else (with-stretch-buffer 0 #\.
              (with-stretch-buffer 1 #\.
                (with-stretch-buffer 2 #\.
                  (*state rd-token-symbol c 3 #f
                    rd-token-intern-nonstandard))))])))

(define-state (rd-token-comma)
  (with-peek-char c
    (state-case c
      [eof (state-return quote 'unquote)]
      [#\@ (with-read-char c (state-return quote 'unquote-splicing))]
      [else (state-return quote 'unquote)])))

(define-state (rd-token-hash-comma)
  (with-peek-char c
    (state-case c
      [eof (state-return quote 'unsyntax)]
      [#\@ (with-read-char c (state-return quote 'unsyntax-splicing))]
      [else (state-return quote 'unsyntax)])))

(define-state (rd-token-comment)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (*state rd-token))]
      [(#\newline #\return #\nel #\ls) (*state rd-token)]
      [else (*state rd-token-comment)])))

(define-state (rd-token-hash)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (xcall rd-eof-error "# prefix"))]
      [(#\f #\F) (xcall rd-token-delimiter #f "boolean")]
      [(#\t #\T) (xcall rd-token-delimiter #t "boolean")]
      [#\\ (*state rd-token-char)]
      [#\( (state-return vparen #f)] ;) for paren bouncer
      [#\' (state-return quote 'syntax)]
      [#\` (state-return quote 'quasisyntax)]
      [#\, (*state rd-token-hash-comma)]
      [((#\0 - #\9))
       (with-stretch-buffer 0 #\#
         (with-stretch-buffer 1 c
           (*state rd-token-hash-num (digit-value c 10) 2)))]
      [#\@ (state-return fasl #f)]
      [#\[ (nonstandard "#[...] record") (state-return record-brack #f)]
      [#\{ (nonstandard "#{...} gensym") (*state rd-token-gensym)]
      [#\& (nonstandard "#& box") (state-return box #f)]
      [#\; (if (eq? (rcb-who rcb) 'read-token)
               (state-return quote 'datum-comment)
               (xmvlet (() (xcall rd-expression-comment)) (*state rd-token)))]
      [#\! (*state rd-token-hash-bang)]
      [(#\x #\X #\o #\O #\b #\B #\d #\D #\i #\I #\e #\E)
       (with-stretch-buffer 0 #\#
         (with-stretch-buffer 1 c
           (*state rd-token-number 2)))]
      [#\v (*state rd-token-hash-v)]
      [#\% (nonstandard "#% primitive")
           (with-read-char c
             (*state rd-token-symbol c 0 #f
               (state-lambda (n slashed?)
                 (state-return atomic (list '$primitive (maybe-fold/intern (rcb-ip rcb) tb n slashed?))))))]
      [#\: (nonstandard "#: gensym")
           (with-read-char c
             (*state rd-token-symbol c 0 #f
               (state-lambda (n slashed?)
                 (state-return atomic (maybe-fold/gensym (rcb-ip rcb) tb n slashed?)))))]
      [#\| (*state rd-token-block-comment 0)]
      [else (xcall rd-error #f #t "invalid sharp-sign prefix #~c" c)])))

(define-state (rd-token-delimiter x what)
  (with-peek-char c
    (state-case c
      [eof (state-return atomic x)]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#) (state-return atomic x)]
      [char-whitespace? (state-return atomic x)]
      [(#\{ #\} #\' #\` #\,) (nonstandard-delimiter c) (state-return atomic x)]
      [else (xcall rd-delimiter-error c what)])))

(define-state (rd-token-gensym)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (xcall rd-eof-error "gensym"))]
      [else
       (*state rd-token-symbol c 0 #f
         (rec f
           (state-lambda (n slashed1?)
             (with-read-char c
               (state-case c
                 [eof (with-unread-char c (xcall rd-eof-error "gensym"))]
                 [(#\space #\newline #\tab) (*state f n slashed1?)]
                 [else
                  (*state rd-token-symbol c n #f
                    (state-lambda (m slashed2?)
                      (with-read-char c
                        (state-case c
                          [eof (xcall rd-eof-error "gensym")]
                          [(#\}) (state-return atomic (maybe-fold/intern (rcb-ip rcb) tb n m slashed1? slashed2?))]
                          [else (with-unread-char c
                                  (xcall rd-error #f #f
                                    "expected close brace terminating gensym syntax"))]))))])))))])))

(define-state (rd-token-block-comment depth)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (xcall rd-eof-error "block comment"))]
      [#\| (with-peek-char c
             (state-case c
               [eof (xcall rd-eof-error "block comment")]
               [#\# (with-read-char c
                      (if (= depth 0)
                          (*state rd-token)
                          (*state rd-token-block-comment (- depth 1))))]
               [else (*state rd-token-block-comment depth)]))]
      [#\# (with-peek-char c
             (state-case c
               [eof (xcall rd-eof-error "block comment")]
               [#\| (with-read-char c
                      (*state rd-token-block-comment (+ depth 1)))]
               [else (*state rd-token-block-comment depth)]))]
      [else (*state rd-token-block-comment depth)])))

(define-state (rd-token-hash-num n i)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (xcall rd-eof-error "# prefix"))]
      [((#\0 - #\9))
       (with-stretch-buffer i c
         (*state rd-token-hash-num (+ (* n 10) (digit-value c 10)) (fx+ i 1)))]
      [#\( (nonstandard "#<n>(...) vector") (state-return vnparen n)] ; ) for paren bouncer
      [(#\r #\R)
       (nonstandard "#<n>r number prefix")
       (with-stretch-buffer i c
         (*state rd-token-number (fx+ i 1)))]
      [(#\q #\Q) (xcall rd-error #f #t "outdated object file format")]
      [#\# (nonstandard "#<n># insert") (*state rd-token-insert n)]
      [#\= (nonstandard "#<n>= mark") (*state rd-token-mark n)]
      [#\v (*state rd-token-hash-num-v i n)]
      [#\%
       (unless (memv n '(2 3))
         (xcall rd-error #f #t "invalid sharp-sign prefix ~a~a"
                   (substring tb 0 i)
                   c))
       (nonstandard "#<n>% primitive")
       (with-read-char c
         (*state rd-token-symbol c 0 #f
           (state-lambda (m slashed?)
             (state-return atomic (list '$primitive n (maybe-fold/intern (rcb-ip rcb) tb m slashed?))))))]
      [else (xcall rd-error #f #t "invalid sharp-sign prefix ~a~a"
                      (substring tb 0 i)
                      c)])))

(define-state (rd-token-insert n)
  (with-insert-table
    (state-return insert n)))

(define-state (rd-token-mark n)
  (with-insert-table
    (state-return mark n)))

(define-state (rd-token-char)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (xcall rd-eof-error "character"))]
      [(#\x)
       (with-peek-char c1
         (state-case c1
           [eof (state-return atomic c)]
           [((#\0 - #\9) (#\a - #\f) (#\A - #\F))
            (with-stretch-buffer 0 #\x
              (*state rd-token-char-hex 0 1))]
           [else (xcall rd-token-delimiter c "character")]))]
      [((#\a - #\w) (#\y - #\z) (#\A - #\Z))
       (with-peek-char c1
         (state-case c1
           [eof (state-return atomic c)]
           [((#\a - #\z) (#\A - #\Z))
            (*state rd-token-to-delimiter 0 c rd-token-charname)]
           [else (xcall rd-token-delimiter c "character")]))]
      [((#\0 - #\7))
       (with-peek-char c1
         (state-case c1
           [eof (state-return atomic c)]
           [((#\0 - #\7))
            (nonstandard "octal character")
            (with-read-char c1
              (with-read-char c2
                (state-case c2
                  [eof (with-unread-char c2 (xcall rd-eof-error "character"))]
                  [((#\0 - #\7))
                   (let ([v (fx+ (fx* (digit-value c 8) 64)
                                 (fx* (digit-value c1 8) 8)
                                 (digit-value c2 8))])
                     (when (fx> v 255)
                       (xcall rd-error #f #t "invalid character #\\~a~a~a" c c1 c2))
                     (xcall rd-token-delimiter (integer->char v) "character"))]
                  [else (xcall rd-error #f #t "invalid character #\\~a~a" c c1)])))]
           [else (xcall rd-token-delimiter c "character")]))]
      [else (xcall rd-token-delimiter c "character")])))

(define-state (rd-token-char-hex n i)
  (define (int->char n)
    (if (and (fixnum? n) (or (fx<= n #xD7FF) (fx<= #xE000 n #x10FFFF)))
        (integer->char n)
        (xcall rd-error #f #t "invalid hex character escape ~a" (substring tb 0 i))))
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (state-return atomic (int->char n)))]
      [((#\0 - #\9) (#\a - #\f) (#\A - #\F))
       (with-stretch-buffer i c
         (*state rd-token-char-hex (+ (* n 16) (digit-value c 16)) (fx+ i 1)))]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#)
       (with-unread-char c (state-return atomic (int->char n)))]
      [char-whitespace?
       (with-unread-char c (state-return atomic (int->char n)))]
      [(#\{ #\} #\' #\` #\,)
       (nonstandard-delimiter c)
       (with-unread-char c (state-return atomic (int->char n)))]
      [else (*state rd-token-to-delimiter i c rd-token-charname)])))

(define-state (rd-token-charname n c)
  (define r6rs-char-names
    '((nul . #\nul) (alarm . #\alarm) (backspace . #\backspace)
      (tab . #\tab) (linefeed . #\newline) (newline . #\newline)
      (vtab . #\vtab) (page . #\page) (return . #\return)
      (esc . #\esc) (space . #\space) (delete . #\delete)))
  (define (r6rs-char-name x)
    (cond
      [(assq x r6rs-char-names) => cdr]
      [else #f]))
  (with-unread-char c
    (state-return atomic
      (or (let ([x (maybe-fold/intern (rcb-ip rcb) tb n #f)])
            (if ($port-flags-set? (rcb-ip rcb) (constant port-flag-r6rs))
                (r6rs-char-name x)
                (char-name x)))
          (let ([s (substring tb 0 n)])
            (if (and (with-peek-char c (eof-object? c))
                     (valid-prefix? s
                       (map symbol->string
                         (if ($port-flags-set? (rcb-ip rcb) (constant port-flag-r6rs))
                             (map car r6rs-char-names)
                             (let-values ([(keys vals) (hashtable-entries char-name-table)])
                               (apply append (vector->list vals)))))))
                (xcall rd-eof-error "character")
                (xcall rd-error #f #t "invalid character name #\\~a" s)))))))

(module (valid-prefix?)
  (define string-prefix?
    (lambda (x y)
      (let ([n (string-length x)])
        (and (fx<= n (string-length y))
          (let prefix? ([i 0])
            (or (fx= i n)
                (and (char=? (string-ref x i) (string-ref y i))
                     (prefix? (fx+ i 1)))))))))

  (define valid-prefix?
    (lambda (x ls)
      (ormap (lambda (y) (string-prefix? x y)) ls))))

(define-state (rd-token-hash-bang) ; more complex than necessary because #!r6rs need not be delimited
  (*state rd-token-hash-bang2 0
   ; list only those that need not be delimited
    '(("r6rs" . r6rs)
      ("fold-case" . fold-case)
      ("no-fold-case" . no-fold-case)
      ("chezscheme" . chezscheme))))

(define-state (rd-token-hash-bang2 i undelimited*)
  (cond
    [(ormap (lambda (a) (and (fx= (string-length (car a)) i) a)) undelimited*) =>
     (lambda (a)
       (let ([ip (rcb-ip rcb)])
         (case (cdr a)
           [(r6rs) ($set-port-flags! ip (constant port-flag-r6rs)) (*state rd-token)]
           [(fold-case)
            ($reset-port-flags! ip (constant port-flag-no-fold-case))
            ($set-port-flags! ip (constant port-flag-fold-case))
            (*state rd-token)]
           [(no-fold-case)
            ($reset-port-flags! ip (constant port-flag-fold-case))
            ($set-port-flags! ip (constant port-flag-no-fold-case))
            (*state rd-token)]
           [(chezscheme) ($reset-port-flags! ip (constant port-flag-r6rs)) (*state rd-token)]
           [else (xcall rd-error #f #t "unexpected #!~s" (car a))])))]
    [else
     (with-read-char c
       (state-case c
         [eof (with-unread-char c (xcall rd-eof-error "#! syntax"))]
         [else (let ([undelimited* (filter (lambda (a) (char=? (string-ref (car a) i) c)) undelimited*)])
                 (if (null? undelimited*)
                     (*state rd-token-to-delimiter i c
                       (state-lambda (i c)
                         (with-unread-char c
                           (let ([s (substring tb 0 i)])
                             (cond
                               [(string=? s "eof")
                                (nonstandard "#!eof")
                                (state-return atomic #!eof)]
                               [(string=? s "bwp")
                                (nonstandard "#!bwp")
                                (state-return atomic #!bwp)]
                               [(string=? s "base-rtd")
                                (nonstandard "#!base-rtd")
                                (state-return atomic #!base-rtd)]
                               [(and (eof-object? c) (valid-prefix? s '("eof" "bwp" "base-rtd")))
                                (xcall rd-eof-error "#! syntax")]
                               [else (xcall rd-error #f #t "invalid syntax #!~a" s)])))))
                     (with-stretch-buffer i c
                       (*state rd-token-hash-bang2 (fx+ i 1) undelimited*))))]))]))

(define-state (rd-token-hash-v)
  (with-read-char c
    (*state rd-token-to-delimiter 0 c
      (state-lambda (n c)
        (let ([s (substring tb 0 n)])
          (state-case c
            [eof
             (with-unread-char c
               (if (valid-prefix? s '("fx" "u8"))
                   (xcall rd-eof-error "#v prefix")
                   (xcall rd-error #f #t "invalid syntax #v~a" s)))]
            [#\( ;)
             (cond
               [(string=? s "fx") (nonstandard "#vfx(...) fxvector") (state-return vfxparen #f)]
               [(string=? s "u8") (state-return vu8paren #f)]
               [else (xcall rd-error #f #t "invalid syntax #v~a(" s)])] ;)
            [else
             (if (valid-prefix? s '("fx" "u8"))
                 (xcall rd-error #f #t "expected left paren after #v~a prefix" s)
                 (xcall rd-error #f #t "invalid syntax #v~a~a" s c))]))))))

(define-state (rd-token-hash-num-v preflen nelts)
  (with-read-char c
    (*state rd-token-to-delimiter 0 c
      (state-lambda (n c)
        (let ([s (substring tb 0 n)])
          (state-case c
            [eof
             (with-unread-char c
               (if (valid-prefix? s '("fx" "u8"))
                   (xcall rd-eof-error "#v prefix")
                   (xcall rd-error #f #t "invalid syntax #~v,'0dv~a" (- preflen 1) nelts s)))]
            [#\( ;)
             (cond
               [(string=? s "fx") (nonstandard "#<n>vfx(...) fxvector") (state-return vfxnparen nelts)]
               [(string=? s "u8") (nonstandard "#<n>vu8(...) bytevector") (state-return vu8nparen nelts)]
               [else (xcall rd-error #f #t "invalid syntax #~v,'0dv~a(" (- preflen 1) nelts s)])] ;)
            [else
             (if (valid-prefix? s '("fx" "u8"))
                 (xcall rd-error #f #t "expected left paren after #~v,'0dv~a prefix" (- preflen 1) nelts s)
                 (xcall rd-error #f #t "invalid syntax #~v,'0dv~a~a" (- preflen 1) nelts s c))]))))))

(define-state (rd-token-to-delimiter n c next)
  (state-case c
    [eof (xcall next n c)]
    [(#\space #\( #\) #\[ #\] #\" #\; #\# #\{ #\} #\' #\` #\,) (xcall next n c)]
    [char-whitespace? (xcall next n c)]
    [else (with-stretch-buffer n c (with-read-char c (*state rd-token-to-delimiter (fx+ n 1) c next)))]))

(define intraline-whitespace?
  (lambda (c)
    (or (char=? c #\tab)
        (eq? (char-general-category c) 'Zs))))

(define-state (rd-token-string i)
  (let ([char-bfp fp])
    (with-read-char c
      (state-case c
        [eof (with-unread-char c (xcall rd-eof-error "string"))]
        [#\" (state-return atomic (substring tb 0 i))]
        [#\\
         (with-read-char c
           (state-case c
             [eof (with-unread-char c (xcall rd-eof-error "string"))]
             [(#\\ #\")
              (with-stretch-buffer i c
                (*state rd-token-string (fx+ i 1)))]
             [(#\n #\a #\b #\f #\r #\t #\v)
              (with-stretch-buffer i
                (case c
                  [#\a #\bel]
                  [#\b #\backspace]
                  [#\f #\page]
                  [#\n #\newline]
                  [#\r #\return]
                  [#\t #\tab]
                  [#\v #\vt])
                (*state rd-token-string (fx+ i 1)))]
             [#\x (*state rd-token-string-hex-char i 0 char-bfp)]
             [(#\newline #\return #\nel #\ls) (*state rd-token-string-whitespace i c)]
             [intraline-whitespace? (*state rd-token-string-whitespace i c)]
             [((#\0 - #\7))
              (nonstandard "octal string-character")
              (with-read-char c1
                (state-case c1
                  [eof (with-unread-char c1 (xcall rd-eof-error "string"))]
                  [((#\0 - #\7))
                   (with-read-char c2
                     (state-case c2
                       [eof (with-unread-char c2 (xcall rd-eof-error "string"))]
                       [((#\0 - #\7))
                        (let ([v (fx+ (fx* (digit-value c 8) 64)
                                      (fx* (digit-value c1 8) 8)
                                      (digit-value c2 8))])
                          (when (fx> v 255)
                            (let ([bfp char-bfp])
                              (xcall rd-error #f #t "invalid string character \\~c~c~c" c c1 c2)))
                          (with-stretch-buffer i (integer->char v)
                            (*state rd-token-string (fx+ i 1))))]
                       [else
                        (with-unread-char c2
                          (let ([bfp char-bfp])
                            (xcall rd-error #f #t "invalid string character \\~c~c" c c1)))]))]
                  [else
                   (with-unread-char c1
                     (let ([bfp char-bfp])
                       (xcall rd-error #f #t "invalid string character \\~c" c)))]))]
             [(#\')
              (nonstandard "\\' string character")
              (with-stretch-buffer i c
                (*state rd-token-string (fx+ i 1)))]
             [else (let ([bfp char-bfp])
                     (xcall rd-error #f #t "invalid string character \\~c" c))]))]
        [(#\newline #\nel #\ls)
         (with-stretch-buffer i #\newline
           (*state rd-token-string (fx+ i 1)))]
        [(#\return)
         (with-peek-char c
           (state-case c
             [eof (xcall rd-eof-error "string")]
             [(#\newline #\nel)
              (with-read-char c
                (with-stretch-buffer i #\newline
                  (*state rd-token-string (fx+ i 1))))]
             [else
              (with-stretch-buffer i #\newline
                (*state rd-token-string (fx+ i 1)))]))]
        [else
         (with-stretch-buffer i c
           (*state rd-token-string (fx+ i 1)))]))))

(define-state (rd-token-string-whitespace i c)
  (state-case c
    [eof (with-unread-char c (xcall rd-eof-error "string"))]
    [(#\newline #\nel #\ls)
     (*state rd-token-string-skipwhite i)]
    [(#\return)
     (with-peek-char c
       (state-case c
         [eof (xcall rd-eof-error "string")]
         [(#\newline #\nel)
          (with-read-char c (*state rd-token-string-skipwhite i))]
         [else (*state rd-token-string-skipwhite i)]))]
    [intraline-whitespace? (with-read-char c (xcall rd-token-string-whitespace i c))]
    [else (xcall rd-error #f #t "unexpected character ~c after \\<intraline whitespace> in string" c)]))

(define-state (rd-token-string-skipwhite i)
  (with-peek-char c
    (state-case c
      [eof (xcall rd-eof-error "string")]
      [intraline-whitespace? (with-read-char c (*state rd-token-string-skipwhite i))]
      [else (*state rd-token-string i)])))

(xdefine (rd-token-string-hex-char i n char-bfp)
  (with-read-char c1
    (state-case c1
      [eof (with-unread-char c1 (xcall rd-eof-error "string"))]
      [((#\0 - #\9) (#\a - #\f) (#\A - #\F))
       (*state rd-token-string-hex-char i (+ (* n 16) (digit-value c1 16)) char-bfp)]
      [(#\;)
       (if (and (fixnum? n) (or (fx<= n #xD7FF) (fx<= #xE000 n #x10FFFF)))
           (with-stretch-buffer i (integer->char n)
             (*state rd-token-string (fx+ i 1)))
           (let ([bfp char-bfp])
             (xcall rd-error #f #t "invalid code point value ~s in string hex escape" n)))]
      [else
       (with-unread-char c1
         (let ([bfp char-bfp])
           (xcall rd-error #f #t "invalid character ~c in string hex escape" c1)))])))

(xdefine (rd-make-number-or-symbol n)
  (let ([z ($str->num tb n 10 #f ($port-flags-set? (rcb-ip rcb) (constant port-flag-r6rs)))])
    (cond
      [(number? z) z]
      [(eq? z 'norep) (xcall rd-error #t #t "cannot represent ~a" (substring tb 0 n))]
      [(eq? z '!r6rs) (xcall rd-nonstandard-error (format "~a number" (substring tb 0 n)))]
      [else
       (nonstandard (format "~a symbol" (substring tb 0 n)))
       (maybe-fold/intern (rcb-ip rcb) tb n #f)])))

(define-state (rd-token-number-or-symbol i)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c
             (state-return atomic (xcall rd-make-number-or-symbol i)))]
      [((#\0 - #\9) (#\a - #\z) #\- #\+ #\. #\/ #\@ #\# #\|)
       (with-stretch-buffer i c
         (*state rd-token-number-or-symbol (fx+ i 1)))]
      [((#\A - #\Z))
       (with-stretch-buffer i c
         (*state rd-token-number-or-symbol (fx+ i 1)))]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#)
       (with-unread-char c
         (state-return atomic (xcall rd-make-number-or-symbol i)))]
      [char-whitespace?
       (with-unread-char c
         (state-return atomic (xcall rd-make-number-or-symbol i)))]
      [(#\{ #\} #\' #\` #\,)
       (nonstandard-delimiter c)
       (with-unread-char c
         (state-return atomic (xcall rd-make-number-or-symbol i)))]
      [else (*state rd-token-symbol c i #f rd-token-intern-nonstandard)])))

(xdefine (rd-make-number n)
  (let ([z ($str->num tb n 10 #f ($port-flags-set? (rcb-ip rcb) (constant port-flag-r6rs)))])
    (cond
      [(number? z) z]
      [(and (eq? z #f) (with-peek-char c (eof-object? c))) (xcall rd-eof-error "number")]
      [(eq? z '!r6rs) (xcall rd-nonstandard-error (format "~a number" (substring tb 0 n)))]
      [(eq? z 'norep) (xcall rd-error #t #t "cannot represent ~a" (substring tb 0 n))]
      [else (xcall rd-error #f #t "invalid number syntax ~a" (substring tb 0 n))])))

(define-state (rd-token-number i)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c
             (state-return atomic (xcall rd-make-number i)))]
      [((#\0 - #\9) (#\a - #\z) (#\A - #\Z) #\- #\+ #\. #\/ #\@ #\# #\|)
       (with-stretch-buffer i c
         (*state rd-token-number (fx+ i 1)))]
      [(#\space #\( #\) #\[ #\] #\" #\; #\#)
       (with-unread-char c
         (state-return atomic (xcall rd-make-number i)))]
      [char-whitespace?
       (with-unread-char c
         (state-return atomic (xcall rd-make-number i)))]
      [(#\{ #\} #\' #\` #\,)
       (nonstandard-delimiter c)
       (with-unread-char c
         (state-return atomic (xcall rd-make-number i)))]
      [else
       (with-stretch-buffer i c
         (*state rd-token-number (fx+ i 1)))])))

(define-state (rd-token-intern n slashed?)
  (state-return atomic (maybe-fold/intern (rcb-ip rcb) tb n slashed?)))

(define-state (rd-token-intern-nonstandard n slashed?)
  (nonstandard (format "~a symbol" (substring tb 0 n)))
  (state-return atomic (maybe-fold/intern (rcb-ip rcb) tb n slashed?)))

(define-state (rd-token-symbol c i slashed? next)
  (state-case c
    [eof (with-unread-char c (*state next i slashed?))]
    [((#\a - #\z))
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-token-symbol c (fx+ i 1) slashed? next)))] ;[(
    [(#\space #\newline #\) #\])
     (with-unread-char c (*state next i slashed?))]
    [(#\- #\? (#\0 - #\9) #\* #\! #\= #\> #\< #\$ #\% #\& #\/ #\: #\^ #\_ #\~ #\+ #\. #\@)
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-token-symbol c (fx+ i 1) slashed? next)))]
    [((#\A - #\Z))
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-token-symbol c (fx+ i 1) slashed? next)))]
    [#\\
     (with-read-char c
       (state-case c
         [eof (with-unread-char c (xcall rd-eof-error "symbol"))]
         [#\x (*state rd-token-symbol-hex-char i 0 (and fp (fx- fp 2)) slashed? next)]
         [else
          (nonstandard "non-hex back-slash symbol escape")
          (with-stretch-buffer i c
            (with-read-char c
              (*state rd-token-symbol c (fx+ i 1) #t next)))]))]
    [#\| (nonstandard "|...| symbol escape") (*state rd-token-symbol-bar i next)]
    [(#\( #\[ #\" #\; #\#) ;)] for paren bouncer
     (with-unread-char c (*state next i slashed?))]
    [char-whitespace?
     (with-unread-char c (*state next i slashed?))]
    [$constituent?
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-token-symbol c (fx+ i 1) slashed? next)))]
    [$subsequent?
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-token-symbol c (fx+ i 1) slashed? next)))]
    [(#\{ #\} #\' #\` #\,)
     (nonstandard-delimiter c)
     (with-unread-char c (*state next i slashed?))]
    [else
     (nonstandard (format "character ~c in symbol" c))
     (with-stretch-buffer i c
       (with-read-char c
         (*state rd-token-symbol c (fx+ i 1) slashed? next)))]))

(xdefine (rd-token-symbol-hex-char i n char-bfp slashed? next)
  (with-read-char c1
    (state-case c1
      [eof (with-unread-char c1 (xcall rd-eof-error "symbol"))]
      [((#\0 - #\9) (#\a - #\f) (#\A - #\F))
       (*state rd-token-symbol-hex-char i (+ (* n 16) (digit-value c1 16)) char-bfp slashed? next)]
      [(#\;)
       (if (and (fixnum? n) (or (fx<= n #xD7FF) (fx<= #xE000 n #x10FFFF)))
           (with-stretch-buffer i (integer->char n)
             (with-read-char c
               (*state rd-token-symbol c (fx+ i 1) slashed? next)))
           (let ([bfp char-bfp])
             (xcall rd-error #f #t "invalid code point value ~s in symbol hex escape" n)))]
      [else
       (with-unread-char c1
         (let ([bfp char-bfp])
           (xcall rd-error #f #t "invalid character ~c in symbol hex escape" c1)))])))

(define-state (rd-token-symbol-bar i next)
  (with-read-char c
    (state-case c
      [eof (with-unread-char c (xcall rd-eof-error "symbol"))]
      [#\| (with-read-char c (*state rd-token-symbol c i #t next))]
      [else (with-stretch-buffer i c
              (*state rd-token-symbol-bar (fx+ i 1) next))])))

(xdefine (rd-top-level type value)
  (if (case type
        [(eof) #t]
        [(atomic) (eof-object? value)]
        [else #f])
      (values value fp)
      (if (and (or (eq? type 'rparen) (eq? type 'rbrack))
               (eq? (rcb-ip rcb) (console-input-port)))
          (call-with-token rd-top-level)
          (xmvlet ((x stripped-x) (xcall rd type value))
            (values (if it (xcall rd-fix-graph x) x) fp)))))

(xdefine (rd-fix-graph x)
  (define rd-set-car! (lambda (obj idx val) (set-car! obj val)))
  (define rd-set-cdr! (lambda (obj idx val) (set-cdr! obj val)))
  (define rd-set-box! (lambda (obj idx val) (set-box! obj val)))
  (define rd-field-set!
    (lambda (obj i val)
      (let ((d ($record-type-descriptor obj)))
        ((csv7:record-field-mutator d i) obj val))))
  (define rd-set-vector-tail!
    (lambda (x m val)
      (let ((n (vector-length x)))
        (let loop ([m m])
          (unless (fx= m n)
            (vector-set! x m val)
            (loop (fx+ m 1)))))))
  (define work-list '())
  (define add-update!
    (lambda (dr update! obj idx)
      (delayed-record-update-set! dr
        (let ((f (delayed-record-update dr)))
          (lambda (val)
            (update! obj idx val)
            (f val))))))
  (define seen-table (make-eq-hashtable))
  (define (rd-fix-graph x update! obj idx)
    (cond
      [(insert? x)
       (let loop ([x x])
         (unless (insert-seen x)
           (let ([bfp (insert-bfp x)] [fp (insert-efp x)])
             (xcall rd-error #f #t "mark #~s= missing" (insert-n x))))
         (let ((z (insert-obj x)))
           (if (insert-visited x)
               (if (insert? z)
                   (loop z)
                   (begin
                     (update! obj idx z)
                     (when (delayed-record? z)
                       (add-update! z update! obj idx))))
               (begin
                 (insert-visited-set! x #t)
                 (update! obj idx z)
                 (rd-fix-graph z update! obj idx)))))]
     ; we get shared structure from annotations, so we avoid duplicate
     ; processing and possible infinite regression by dropping out here
     ; if we see something we've seen before.
      [(let ([a (eq-hashtable-cell seen-table x #f)])
         (if (cdr a) #t (begin (set-cdr! a #t) #f)))
       (when (delayed-record? x)
         (add-update! x update! obj idx))]
      [(delayed-record? x)
       (rd-fix-graph (delayed-record-vals x) #f #f #f)
       (add-update! x update! obj idx)
       (set! work-list (cons x work-list))]
      [(pair? x)
       (rd-fix-graph (car x) rd-set-car! x #f)
       (rd-fix-graph (cdr x) rd-set-cdr! x #f)]
      [(vector? x)
       (let ([n (vector-length x)])
         (unless (fx= n 0)
           (let ([m ($last-new-vector-element vector-length vector-ref x)])
             (let loop ([i 0])
               (when (fx< i m)
                 (rd-fix-graph (vector-ref x i) vector-set! x i)
                 (loop (fx+ i 1))))
             (rd-fix-graph (vector-ref x m) rd-set-vector-tail! x m))))]
      [($record? x)
       (let ((d ($record-type-descriptor x)))
         (do ([fields (csv7:record-type-field-names d) (cdr fields)]
              [i 0 (+ i 1)])
             ((null? fields))
           (when (csv7:record-field-accessible? d i)
             (rd-fix-graph ((csv7:record-field-accessor d i) x)
               rd-field-set! x i))))]
      [(box? x)
       (rd-fix-graph (unbox x) rd-set-box! x #f)]))
  (let ((p (cons x #f)))
    (rd-fix-graph x rd-set-car! p #f)
    (let loop ((wl work-list) (rwl '()) (progress? #f))
      (if (null? wl)
          (if (null? rwl)
              (car p)
              (if progress?
                  (loop rwl '() #f)
                  (let ([bfp (delayed-record-bfp (car rwl))]
                        [fp (delayed-record-efp (car rwl))])
                    (xcall rd-error #f #t
                      "unresolvable cycle constructing record of type ~s"
                      (delayed-record-rtd (car rwl))))))
          (let* ((dr (car wl))
                 (rtd (delayed-record-rtd dr))
                 (vals (delayed-record-vals dr))
                 (fields (csv7:record-type-field-names rtd)))
            (if (andmap
                  (lambda (f v)
                    (or (not (delayed-record? v))
                        (csv7:record-field-mutable? rtd f)))
                  fields vals)
                (let ((r (apply (record-constructor rtd) vals)))
                  (for-each
                    (lambda (f v)
                      (when (delayed-record? v)
                        (add-update! v rd-field-set! r f)))
                    fields vals)
                  ((delayed-record-update dr) r)
                  (loop (cdr wl) rwl #t))
                (loop (cdr wl) (cons dr rwl) progress?)))))))

(xdefine (rd type value)
  (xmvlet ((x stripped) (xcall rd-help type value))
    (xvalues
      (if (rcb-a? rcb)
          (make-annotation x ($make-source-object (rcb-sfd rcb) bfp fp) stripped)
          x)
      stripped)))

; trick cp0 into not inlining rd-help into rd above, which would result
; in a call-with-values expression that cp1 cannot turn into mvlet and
; concomitant closure creation expenses
(xdefine (GROSS-HACK type value)
  (xcall rd-help type value))

(xdefine (rd-help type value)
  (case type
    [(atomic) (xvalues value value)]
    [(lparen) (xcall rd-paren-list)]
    [(lbrack) (xcall rd-brack-list)]
    [(quote) (xcall rd-quote value)]
    [(vparen) (xcall rd-vector bfp 0)]
    [(vnparen) (xcall rd-sized-vector value)]
    [(vfxparen) (xmvlet ((v) (xcall rd-fxvector bfp 0)) (xvalues v v))]
    [(vfxnparen) (xmvlet ((v) (xcall rd-sized-fxvector value)) (xvalues v v))]
    [(vu8paren) (xmvlet ((v) (xcall rd-bytevector bfp 0)) (xvalues v v))]
    [(vu8nparen) (xmvlet ((v) (xcall rd-sized-bytevector value)) (xvalues v v))]
    [(box) (xcall rd-box)]
    [(fasl)
     (xcall rd-error #f #t
       "unsupported old fasl format detected---use new format with binary i/o")]
    [(mark) (xcall rd-mark value)]
    [(insert) (xcall rd-insert value)]
    [(record-brack) (xcall rd-record)]
    [(rparen) (xcall rd-error #f #t "unexpected close parenthesis")]
    [(rbrack) (xcall rd-error #f #t "unexpected close bracket")]
    [(dot) (xcall rd-error #f #t "unexpected dot (.)")]
   ; eof should be caught elsewhere, but just in case ...
    [(eof) (xcall rd-error #f #f "unexpected end-of-file")]
    [else (xcall rd-error #f #f "unexpected internal token type ~s" type)]))

(xdefine (rd-paren-list)
  (let ([expr-bfp bfp])
    (with-token (type value)
      (case type
        [(rparen) (xvalues '() '())]
        [(rbrack) (let ([bfp expr-bfp]) (xcall rd-error #f #f "parenthesized list terminated by bracket"))]
        [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "list"))]
        [else
         (xmvlet ((first stripped-first) (xcall rd type value))
           (xmvlet ((rest stripped-rest) (xcall rd-paren-tail expr-bfp))
             (xvalues
               (cons first rest)
               (and (rcb-a? rcb) (cons stripped-first stripped-rest)))))]))))

(xdefine (rd-paren-tail expr-bfp)
  (with-token (type value)
    (case type
      [(rparen) (xvalues '() '())]
      [(rbrack) (let ([bfp expr-bfp]) (xcall rd-error #f #f "parenthesized list terminated by bracket"))]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "list"))]
      [(dot)
       (with-token (type value)
         (case type
           [(rparen) (xcall rd-error #f #f "expected one item after dot (.)")]
           [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "list"))]
           [else
            (xmvlet ((x stripped-x) (xcall rd type value))
              (with-token (type value)
                (case type
                  [(rparen) (xvalues x stripped-x)]
                  [(rbrack) (let ([bfp expr-bfp]) (xcall rd-error #f #f "parenthesized list terminated by bracket"))]
                  [(dot) (xcall rd-error #f #t "unexpected dot")]
                  [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "list"))]
                  [else (xcall rd-error #f #t "more than one item found after dot (.)")])))]))]
      [else
       (xmvlet ((first stripped-first) (xcall rd type value))
         (xmvlet ((rest stripped-rest) (xcall rd-paren-tail expr-bfp))
           (xvalues
             (cons first rest)
             (and (rcb-a? rcb) (cons stripped-first stripped-rest)))))])))

(xdefine (rd-brack-list)
  (let ([expr-bfp bfp])
    (with-token (type value)
      (case type
        [(rbrack) (xvalues '() '())]
        [(rparen) (let ([bfp expr-bfp]) (xcall rd-error #f #f "bracketed list terminated by parenthesis"))]
        [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "bracketed list"))]
        [else
         (xmvlet ((first stripped-first) (xcall rd type value))
           (xmvlet ((rest stripped-rest) (xcall rd-brack-tail expr-bfp))
             (xvalues
               (cons first rest)
               (and (rcb-a? rcb) (cons stripped-first stripped-rest)))))]))))

(xdefine (rd-brack-tail expr-bfp)
  (with-token (type value)
    (case type
      [(rbrack) (xvalues '() '())]
      [(rparen) (let ([bfp expr-bfp]) (xcall rd-error #f #f "bracketed list terminated by parenthesis"))]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "bracketed list"))]
      [(dot)
       (with-token (type value)
         (case type
           [(rbrack) (xcall rd-error #f #f "expected one item after dot (.)")]
           [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "bracketed list"))]
           [else
            (xmvlet ((x stripped-x) (xcall rd type value))
              (with-token (type value)
                (case type
                  [(rbrack) (xvalues x stripped-x)]
                  [(rparen) (let ([bfp expr-bfp]) (xcall rd-error #f #f "bracketed list terminated by parenthesis"))]
                  [(dot) (xcall rd-error #f #t "unexpected dot")]
                  [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "bracketed list"))]
                  [else (xcall rd-error #f #t "more than one item found after dot (.)")])))]))]
      [else
       (xmvlet ((first stripped-first) (xcall rd type value))
         (xmvlet ((rest stripped-rest) (xcall rd-brack-tail expr-bfp))
           (xvalues
             (cons first rest)
             (and (rcb-a? rcb) (cons stripped-first stripped-rest)))))])))

(xdefine (rd-quote kind)
  (let ([expr-bfp bfp])
    (with-token (type value)
      (case type
        [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error (symbol->string kind)))]
        [else (xmvlet ((x stripped-x) (xcall rd type value))
                (xvalues
                  (list kind x)
                  (and (rcb-a? rcb) (list kind stripped-x))))]))))

(xdefine (rd-record)
  (let ([expr-bfp bfp])
    (with-token (type name)
      (case type
        [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "record"))]
        [else
         (cond
           [(or (not (eq? type 'atomic)) (not (symbol? name)))
            (xcall rd-error #f #t "non-symbol found after #[")] ;]
           [(or (record-reader (symbol->string name))
                (let ((x ($sgetprop name '*rtd* #f)))
                  (and (record-type-descriptor? x)
                    x))) =>
            (lambda (rtd)
              (let ((decls (csv7:record-type-field-decls rtd)))
                (xmvlet ((vals stripped-vals) (xcall rd-record-tail expr-bfp (length decls) name))
                 ; strip annotations from vals headed for non-ptr fields
                  (let ([vals (if stripped-vals
                                  (map (lambda (decl val sval)
                                         (apply
                                           (lambda (m t n)
                                             (if (eq? (filter-foreign-type t) 'scheme-object)
                                                 val
                                                 sval))
                                           decl))
                                       decls vals stripped-vals)
                                  vals)])
                    (let loop ((fds decls) (vs (or stripped-vals vals)))
                      (if (null? fds)
                          (xvalues
                            (apply (record-constructor rtd) vals)
                            (and (rcb-a? rcb) (apply (record-constructor rtd) stripped-vals)))
                          (if (and (apply (lambda (m t n)
                                            (or (eq? m 'immutable)
                                                (not (eq? (filter-foreign-type t) 'scheme-object))))
                                     (car fds))
                                   (or (insert? (car vs))
                                       (delayed-record? (car vs))))
                              (xvalues
                                (make-delayed-record rtd vals expr-bfp fp)
                                (and (rcb-a? rcb) (make-delayed-record rtd stripped-vals expr-bfp fp)))
                              (loop (cdr fds) (cdr vs)))))))))]
           [else (xcall rd-error #f #t "unrecognized record name ~s" name)])]))))

(xdefine (rd-record-tail expr-bfp n name)
  (with-token (type value)
    (case type
      [(rbrack)
       (if (= n 0)
           (xvalues '() '())
           (let ([bfp expr-bfp])
             (xcall rd-error #f #t "too few fields supplied for record ~s" name)))]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "record"))]
      [else
       (xmvlet ((first stripped-first) (xcall rd type value))
         (if (= n 0)
             (let ([bfp expr-bfp])
               (xcall rd-error #f #t "too many fields supplied for record ~s" name))
             (xmvlet ((rest stripped-rest) (xcall rd-record-tail expr-bfp (- n 1) name))
               (xvalues
                 (cons first rest)
                 (and (rcb-a? rcb) (cons stripped-first stripped-rest))))))])))

(xdefine (rd-vector expr-bfp i)
  (with-token (type value)
    (case type
      [(rparen) (xvalues (make-vector i) (and (rcb-a? rcb) (make-vector i)))]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "vector"))]
      [else
       (xmvlet ((x stripped-x) (xcall rd type value))
         (xmvlet ((v stripped-v) (xcall rd-vector expr-bfp (fx+ i 1)))
           (vector-set! v i x)
           (when (rcb-a? rcb) (vector-set! stripped-v i stripped-x))
           (xvalues v stripped-v)))])))

(xdefine (rd-sized-vector n)
  (unless (and (fixnum? n) (fxnonnegative? n))
    (let ([bfp (and bfp (+ bfp 1))] [fp (and fp (- fp 1))])
      (xcall rd-error #f #t "invalid vector length ~s" n)))
  (xcall rd-fill-vector bfp (make-vector n) (and (rcb-a? rcb) (make-vector n)) 0 n))

(xdefine (rd-fill-vector expr-bfp v stripped-v i n)
  (with-token (type value)
    (case type
      [(rparen)
       (when (fx< 0 i n)
         (let ((prev (vector-ref v (fx- i 1))))
           (do ([i i (fx+ i 1)])
               ((fx= i n))
               (vector-set! v i prev)))
         (when stripped-v
           (let ((prev (vector-ref stripped-v (fx- i 1))))
             (do ([i i (fx+ i 1)])
                 ((fx= i n))
                 (vector-set! stripped-v i prev)))))
       (xvalues v stripped-v)]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "vector"))]
      [else
       (xmvlet ((x stripped-x) (xcall rd type value))
         (unless (fx< i n)
           (let ([bfp expr-bfp])
             (xcall rd-error #f #t "too many vector elements supplied")))
         (vector-set! v i x)
         (and stripped-v (vector-set! stripped-v i stripped-x))
         (xcall rd-fill-vector expr-bfp v stripped-v (fx+ i 1) n))])))

;; an fxvector contains a sequence of fixnum tokens.  we don't handle
;; graph marks and references because to do so generally, we'd have to
;; put non-fixnums (insert records) into the fxvector or perhaps
;; somehow generalize delayed records to handle fxvectors
(xdefine (rd-fxvector expr-bfp i)
  (with-token (type value)
    (case type
      [(rparen) (xvalues (make-fxvector i))]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "fxvector"))]
      [else
       (unless (and (eq? type 'atomic) (fixnum? value))
         (xcall rd-error #f #t "non-fixnum found in fxvector"))
       (xmvlet ((v) (xcall rd-fxvector expr-bfp (fx+ i 1)))
         (fxvector-set! v i value)
         (xvalues v))])))

(xdefine (rd-sized-fxvector n)
  (unless (and (fixnum? n) (fxnonnegative? n))
    (let ([bfp (and bfp (+ bfp 1))] [fp (and fp (- fp 1))])
      (xcall rd-error #f #t "invalid fxvector length ~s" n)))
  (xcall rd-fill-fxvector bfp (make-fxvector n) 0 n))

(xdefine (rd-fill-fxvector expr-bfp v i n)
  (with-token (type value)
    (case type
      [(rparen)
       (when (fx< 0 i n)
         (let ((prev (fxvector-ref v (fx- i 1))))
           (do ([i i (fx+ i 1)])
               ((fx= i n))
               (fxvector-set! v i prev))))
       (xvalues v)]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "fxvector"))]
      [else
       (unless (and (eq? type 'atomic) (fixnum? value))
         (xcall rd-error #f #t "non-fixnum found in fxvector"))
       (unless (fx< i n)
         (let ([bfp expr-bfp])
           (xcall rd-error #f #t "too many fxvector elements supplied")))
       (fxvector-set! v i value)
       (xcall rd-fill-fxvector expr-bfp v (fx+ i 1) n)])))

;; a bytevector contains a sequence of fixnum tokens.  we don't handle
;; graph marks and references because to do so generally, we'd have to
;; put non-fixnums (insert records) into the bytevector or perhaps
;; somehow generalize delayed records to handle bytevectors
(xdefine (rd-bytevector expr-bfp i)
  (with-token (type value)
    (case type
      [(rparen) (xvalues (make-bytevector i))]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "bytevector"))]
      [else
       (unless (and (eq? type 'atomic) (fixnum? value) (fx<= 0 value 255))
         (xcall rd-error #f #t "invalid value ~s found in bytevector" value))
       (xmvlet ((v) (xcall rd-bytevector expr-bfp (fx+ i 1)))
         (bytevector-u8-set! v i value)
         (xvalues v))])))

(xdefine (rd-sized-bytevector n)
  (unless (and (fixnum? n) (fxnonnegative? n))
    (let ([bfp (and bfp (+ bfp 1))] [fp (and fp (- fp 1))])
      (xcall rd-error #f #t "invalid bytevector length ~s" n)))
  (xcall rd-fill-bytevector bfp (make-bytevector n) 0 n))

(xdefine (rd-fill-bytevector expr-bfp v i n)
  (with-token (type value)
    (case type
      [(rparen)
       (when (fx< 0 i n)
         (let ((prev (bytevector-u8-ref v (fx- i 1))))
           (do ([i i (fx+ i 1)])
               ((fx= i n))
               (bytevector-u8-set! v i prev))))
       (xvalues v)]
      [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "bytevector"))]
      [else
       (unless (and (eq? type 'atomic) (fixnum? value) (fx<= 0 value 255))
         (xcall rd-error #f #t "invalid value ~s found in bytevector" value))
       (unless (fx< i n)
         (let ([bfp expr-bfp])
           (xcall rd-error #f #t "too many bytevector elements supplied")))
       (bytevector-u8-set! v i value)
       (xcall rd-fill-bytevector expr-bfp v (fx+ i 1) n)])))

(xdefine (rd-box)
  (let ([expr-bfp bfp])
    (with-token (type value)
      (case type
        [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "box"))]
        [else
         (xmvlet ((x stripped-x) (xcall rd type value))
           (xvalues (box x) (and (rcb-a? rcb) (box stripped-x))))]))))

(xdefine (rd-mark n)
  (let ([a (eq-hashtable-cell it n #f)])
   ; set up insert(s) if not already present
    (unless (cdr a) (set-cdr! a (cons (make-insert n bfp fp) (and (rcb-a? rcb) (make-insert n bfp fp)))))
   ; check for duplicate marks
    (when (insert-seen (cadr a)) (xcall rd-error #f #t "duplicate mark #~s= seen" n))
   ; mark seen before reading so that error comes from second duplicate
    (insert-seen-set! (cadr a) #t)
    (when (rcb-a? rcb) (insert-seen-set! (cddr a) #t))
    (let ([expr-bfp bfp])
      (with-token (type value)
        (case type
          [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "graph mark"))]
          [else
           (xmvlet ((obj stripped-obj) (xcall rd type value))
             (if (rcb-a? rcb)
                 (let ([ins (cadr a)] [stripped-ins (cddr a)])
                   (if (eq? stripped-obj stripped-ins)
                       (begin
                         (insert-obj-set! ins '#1#)
                         (insert-obj-set! stripped-ins '#1#))
                       (begin
                        ; remove annotation below mark to avoid redundant annotation
                         (insert-obj-set! ins (annotation-expression obj))
                         (insert-obj-set! stripped-ins stripped-obj)))
                   (xvalues ins stripped-ins))
                 (let ([ins (cadr a)])
                   (insert-obj-set! ins (if (eq? obj ins) '#1=#1# obj))
                   (xvalues ins #f))))])))))

(xdefine (rd-insert n)
  (let ([a (eq-hashtable-cell it n #f)])
   ; set up insert(s) if not already present
    (unless (cdr a) (set-cdr! a (cons (make-insert n bfp fp) (and (rcb-a? rcb) (make-insert n bfp fp)))))
    (xvalues (cadr a) (and (rcb-a? rcb) (cddr a)))))

(xdefine (rd-expression-comment) ; called from scanner
  (let ([expr-bfp bfp])
    (with-token (type value)
      (case type
        [(eof) (let ([bfp expr-bfp]) (xcall rd-eof-error "s-expression comment"))]
        [else
         (xmvlet ((x stripped-x) (xcall rd type value))
           (xvalues))]))))

(set-who! read-token
  (let ()
    (define read-token
      (lambda (ip sfd fp)
        (when (port-closed? ip)
          ($oops who "not permitted on closed port ~s" ip))
        (let ([fp (or fp
                      (and ($port-flags-set? ip (constant port-flag-char-positions))
                           (port-has-port-position? ip)
                           (port-position ip)))])
          (let ([rcb (make-rcb ip sfd #f who)] [tb ""] [bfp fp] [it #f])
            (with-token (type value)
              (values type value bfp fp))))))
    (case-lambda
      [() (read-token (current-input-port) #f #f)]
      [(ip)
       (unless (and (input-port? ip) (textual-port? ip))
         ($oops who "~s is not a textual input port" ip))
       (read-token ip #f #f)]
      [(ip sfd fp)
       (unless (and (input-port? ip) (textual-port? ip))
         ($oops who "~s is not a textual input port" ip))
       (unless (source-file-descriptor? sfd)
         ($oops who "~s is not a source-file descriptor" sfd))
       (unless (and (integer? fp) (exact? fp) (>= fp 0))
         ($oops who "~s is not a valid file position" fp))
       (read-token ip sfd fp)])))

(let ()
  (define do-read
    (lambda (who ip sfd a? fp)
      (when (port-closed? ip)
        ($oops who "not permitted on closed port ~s" ip))
      (let ([fp (or fp
                    (and ($port-flags-set? ip (constant port-flag-char-positions))
                         (port-has-port-position? ip)
                         (port-position ip)))])
        (let ([rcb (make-rcb ip sfd (and a? sfd fp #t) who)] [tb ""] [bfp fp] [it #f])
          (call-with-token rd-top-level)))))
  (set-who! get-datum
    (lambda (ip)
      (unless (and (input-port? ip) (textual-port? ip))
        ($oops who "~s is not a textual input port" ip))
      (with-values (do-read who ip #f #f #f) (lambda (x fp) x))))
  (set-who! read
    (case-lambda
      [() (with-values (do-read who (current-input-port) #f #f #f) (lambda (x fp) x))]
      [(ip)
       (unless (and (input-port? ip) (textual-port? ip))
         ($oops who "~s is not a textual input port" ip))
       (with-values (do-read who ip #f #f #f) (lambda (x fp) x))]))
  (set-who! get-datum/annotations
    (lambda (ip sfd fp)
      (unless (and (input-port? ip) (textual-port? ip))
        ($oops who "~s is not a textual input port" ip))
      (unless (source-file-descriptor? sfd)
        ($oops who "~s is not a source-file descriptor" sfd))
      (unless (and (integer? fp) (exact? fp) (>= fp 0))
        ($oops who "~s is not a valid file position" fp))
      (do-read who ip sfd #t fp)))
  (set! $make-read
    (lambda (ip sfd fp)
      (define who 'read)
      (unless (and (input-port? ip) (textual-port? ip))
        ($oops who "~s is not a textual input port" ip))
      (unless (or (not sfd) (source-file-descriptor? sfd))
        ($oops who "~s is not a source-file descriptor" sfd))
      (lambda ()
        (let-values ([(x new-fp) (do-read who ip sfd #t fp)])
          (set! fp new-fp)
          x)))))

(set! $open-source-file
  (lambda (sfd)
    (define source-port
      (let ([paths-tried '()])
        (lambda (path)
          (and (not (member path paths-tried))
            (begin
              (set! paths-tried (cons path paths-tried))
              (guard (c [#t #f])
                (let ([ip ($open-file-input-port '$open-source-file path)])
                  (if (let ([new-sfd ($source-file-descriptor path ip)])
                         (and (fx= (source-file-descriptor-crc new-sfd)
                                   (source-file-descriptor-crc sfd))
                              (= (source-file-descriptor-length new-sfd)
                                 (source-file-descriptor-length sfd))))
                       (transcoded-port ip (current-transcoder))
                       (begin (close-input-port ip) #f)))))))))
    (define (search name dir*)
      (and (not (null? dir*))
           (or (source-port
                 (let ([dir (car dir*)])
                   (if (or (string=? dir "") (string=? dir "."))
                       name
                       (format (if (directory-separator?
                                     (string-ref dir
                                       (fx- (string-length dir) 1)))
                                   "~a~a"
                                   "~a/~a")
                         dir name))))
               (search name (cdr dir*)))))
    (let ([name (source-file-descriptor-name sfd)])
      (or (and ($fixed-path? name) (source-port name))
          (let ([dir* (append (source-directories) (map car (library-directories)))])
            (let pathloop ([name name])
              (or (search name dir*)
                  (let ([rest (path-rest name)])
                    (and (not (string=? rest name))
                         (pathloop rest))))))))))

(let ([source-lines-cache (make-weak-eq-hashtable)])

  (set! $locate-source
    (lambda (sfd fp use-cache?)
      (define (binary-search table name)
        (let loop ([lo 0] [hi (vector-length table)])
          (if (fx= (fx+ 1 lo) hi)
              (values name
                      hi
                      (fx+ 1 (fx- fp (vector-ref table lo))))
              (let ([mid (fxsra (fx+ lo hi) 1)])
                (if (< fp (vector-ref table mid))
                    (loop lo mid)
                    (loop mid hi))))))
      (cond
       [(and use-cache?
             (with-tc-mutex (hashtable-ref source-lines-cache sfd #f))) =>
        (lambda (name+table)
          (binary-search (cdr name+table) (car name+table)))]
       [($open-source-file sfd) =>
        (lambda (ip)
          (define name (port-name ip))
          (define table
            ;; Make a vector for the position (counting from zero)
            ;; that starts each line (= vector index + 1)
            (let loop ([fp 0] [accum '(0)])
              (let ([ch (read-char ip)])
                (cond
                  [(eof-object? ch)
                   (close-input-port ip)
                   (list->vector (reverse accum))]
                  [(eqv? ch #\newline)
                   (let ([fp (fx+ fp 1)])
                     (loop fp (cons fp accum)))]
                  [else
                    (loop (fx+ fp 1) accum)]))))
          (when use-cache?
            (with-tc-mutex
              (hashtable-set! source-lines-cache sfd (cons name table))))
          (binary-search table name))]
       [else (values)])))

  (set! $clear-source-lines-cache
   ; called from single-threaded docollect
    (lambda ()
      (hashtable-clear! source-lines-cache))))

(set! $source-file-descriptor
  (let ()
    (define crc16
      (let ([crc16-table
             '#(#x0000 #x1189 #x2312 #x329b #x4624 #x57ad #x6536 #x74bf
                #x8c48 #x9dc1 #xaf5a #xbed3 #xca6c #xdbe5 #xe97e #xf8f7
                #x1081 #x0108 #x3393 #x221a #x56a5 #x472c #x75b7 #x643e
                #x9cc9 #x8d40 #xbfdb #xae52 #xdaed #xcb64 #xf9ff #xe876
                #x2102 #x308b #x0210 #x1399 #x6726 #x76af #x4434 #x55bd
                #xad4a #xbcc3 #x8e58 #x9fd1 #xeb6e #xfae7 #xc87c #xd9f5
                #x3183 #x200a #x1291 #x0318 #x77a7 #x662e #x54b5 #x453c
                #xbdcb #xac42 #x9ed9 #x8f50 #xfbef #xea66 #xd8fd #xc974
                #x4204 #x538d #x6116 #x709f #x0420 #x15a9 #x2732 #x36bb
                #xce4c #xdfc5 #xed5e #xfcd7 #x8868 #x99e1 #xab7a #xbaf3
                #x5285 #x430c #x7197 #x601e #x14a1 #x0528 #x37b3 #x263a
                #xdecd #xcf44 #xfddf #xec56 #x98e9 #x8960 #xbbfb #xaa72
                #x6306 #x728f #x4014 #x519d #x2522 #x34ab #x0630 #x17b9
                #xef4e #xfec7 #xcc5c #xddd5 #xa96a #xb8e3 #x8a78 #x9bf1
                #x7387 #x620e #x5095 #x411c #x35a3 #x242a #x16b1 #x0738
                #xffcf #xee46 #xdcdd #xcd54 #xb9eb #xa862 #x9af9 #x8b70
                #x8408 #x9581 #xa71a #xb693 #xc22c #xd3a5 #xe13e #xf0b7
                #x0840 #x19c9 #x2b52 #x3adb #x4e64 #x5fed #x6d76 #x7cff
                #x9489 #x8500 #xb79b #xa612 #xd2ad #xc324 #xf1bf #xe036
                #x18c1 #x0948 #x3bd3 #x2a5a #x5ee5 #x4f6c #x7df7 #x6c7e
                #xa50a #xb483 #x8618 #x9791 #xe32e #xf2a7 #xc03c #xd1b5
                #x2942 #x38cb #x0a50 #x1bd9 #x6f66 #x7eef #x4c74 #x5dfd
                #xb58b #xa402 #x9699 #x8710 #xf3af #xe226 #xd0bd #xc134
                #x39c3 #x284a #x1ad1 #x0b58 #x7fe7 #x6e6e #x5cf5 #x4d7c
                #xc60c #xd785 #xe51e #xf497 #x8028 #x91a1 #xa33a #xb2b3
                #x4a44 #x5bcd #x6956 #x78df #x0c60 #x1de9 #x2f72 #x3efb
                #xd68d #xc704 #xf59f #xe416 #x90a9 #x8120 #xb3bb #xa232
                #x5ac5 #x4b4c #x79d7 #x685e #x1ce1 #x0d68 #x3ff3 #x2e7a
                #xe70e #xf687 #xc41c #xd595 #xa12a #xb0a3 #x8238 #x93b1
                #x6b46 #x7acf #x4854 #x59dd #x2d62 #x3ceb #x0e70 #x1ff9
                #xf78f #xe606 #xd49d #xc514 #xb1ab #xa022 #x92b9 #x8330
                #x7bc7 #x6a4e #x58d5 #x495c #x3de3 #x2c6a #x1ef1 #x0f78)])
       ; invoke with crc = #xffff for start of data
        (lambda (crc s n)
          (let loop ((i 0) (crc crc))
            (if (fx= i n)
                crc
                (loop (fx+ i 1)
                      (fxlogxor (fxsrl crc 8)
                        (vector-ref crc16-table
                          (fxlogand
                            (fxlogxor crc (bytevector-u8-ref s i))
                            #xff)))))))))
    (define go/reset
      (lambda (ifn ip)
        (let ([pos (port-position ip)])
          (set-port-position! ip 0)
          (let ([sfd (go ifn ip)])
            (set-port-position! ip pos)
            sfd))))
    (define go
      (lambda (ifn ip)
        (let ((buflen (file-buffer-size)))
          (define buf (make-bytevector buflen))
          (let loop ((len 0) (crc #xffff))
            (let ((n (get-bytevector-n! ip buf 0 buflen)))
              (if (eof-object? n)
                  (make-source-file-descriptor ifn len crc)
                  (loop (+ len n) (crc16 crc buf n))))))))
    (case-lambda
      [(ifn ip) (go/reset ifn ip)]
      [(ifn ip reset?)
       (if reset? (go/reset ifn ip) (go ifn ip))])))

(set! char-name
  (let ()
    (define valid-name?
      (lambda (s)
        (let ((n (string-length s)))
          (and (fx> n 1)
               (let allalpha ([i 0])
                 (or (fx= i n)
                     (and (char-alphabetic? (string-ref s i))
                          (allalpha (fx+ i 1)))))
               (not (let allalphahex ([i 0])
                      (or (fx= i n)
                          (let ([c (string-ref s i)])
                            (and (or (char<=? #\a c #\f) (char<=? #\A c #\F))
                                 (allalphahex (fx+ i 1)))))))))))
    (case-lambda
      [(x)
       (if (char? x)
           (let ([ls (hashtable-ref char-name-table x '())])
             (and (not (null? ls)) (car ls)))
           (and (symbol? x) ($sgetprop x '*char-name* #f)))]
      [(x c)
       (unless (and (symbol? x) (valid-name? (symbol->string x)))
         ($oops 'char-name
                "~s is not a valid character name"
                x))
       (with-tc-mutex
         (let ((oldc ($sgetprop x '*char-name* #f)))
           (when oldc
            ; remove x from table entry for oldc
             (hashtable-update! char-name-table oldc
               (lambda (ls) (remq x ls))
               '())))
         (cond
           ((eq? c #f) ($sremprop x '*char-name*))
           ((char? c)
            ; add x to char-name-table entry for c
            (hashtable-update! char-name-table c
              (lambda (ls) (cons x ls))
              '())
            ; make c entry for x
            ($sputprop x '*char-name* c))
           (else ($oops 'char-name "~s is not a character" c))))])))
) ;let

(define source-directories
  (make-parameter '(".")
    (lambda (x)
      (unless (and (list? x) (andmap string? x))
        ($oops 'source-directories "invalid path list ~s" x))
      x)))


(define record-reader
  (case-lambda
    [(name/rtd)
     (cond
       [(record-type-descriptor? name/rtd)
        ($sgetprop (record-type-uid name/rtd) 'reader-record #f)]
       [(string? name/rtd)
        ($sgetprop (string->symbol name/rtd) 'record-reader #f)]
       [(symbol? name/rtd)
        ($sgetprop name/rtd 'record-reader #f)]
       [else ($oops 'record-reader "invalid input ~s" name/rtd)])]
    [(name/rtd rtd/false)
     (cond
       [(not rtd/false)
        (cond
          [(record-type-descriptor? name/rtd)
           (let ([rtd name/rtd])
             (with-tc-mutex
               (cond
                 [($sgetprop (record-type-uid rtd) 'reader-record #f) =>
                  (lambda (name)
                    ($sremprop (record-type-uid rtd) 'reader-record)
                    ($sremprop name 'record-reader))])))]
          [(if (symbol? name/rtd) name/rtd (and (string? name/rtd) (string->symbol name/rtd))) =>
           (lambda (name)
             (with-tc-mutex
               (cond
                 [($sgetprop name 'record-reader #f) =>
                  (lambda (rtd)
                    ($sremprop (record-type-uid rtd) 'reader-record)
                    ($sremprop name 'record-reader))])))]
          [else ($oops 'record-reader "invalid first argument ~s" name/rtd)])]
       [(record-type-descriptor? rtd/false)
        (let ([rtd rtd/false])
          (cond
            [(if (symbol? name/rtd) name/rtd (and (string? name/rtd) (string->symbol name/rtd))) =>
             (lambda (name)
               (with-tc-mutex
                 ($sputprop name 'record-reader rtd)
                 ($sputprop (record-type-uid rtd) 'reader-record name)))]
            [(record-type-descriptor? name/rtd)
             ($oops 'record-reader "~s valid as first argument only when second is #f" name/rtd)]
            [else ($oops 'record-reader "invalid first argument ~s" name/rtd)]))]
       [else ($oops 'record-reader "invalid second argument ~s" rtd/false)])]))

(begin
  (char-name 'space #\space)
  (char-name 'tab #\tab)
  (char-name 'return #\return)
  (char-name 'page #\page)
  (char-name 'linefeed #\linefeed)
  (char-name 'newline #\newline) ; must come after linefeed entry
  (char-name 'backspace #\backspace)
  (char-name 'rubout #\rubout)
  (char-name 'nul #\nul)
  (char-name 'bel #\bel)
  (char-name 'vt #\vt)
  (char-name 'esc #\esc)
  (char-name 'vtab #\vtab)
  (char-name 'delete #\rubout)
  (char-name 'alarm #\bel)
  (char-name 'nel #\nel)
  (char-name 'ls #\ls))
)
