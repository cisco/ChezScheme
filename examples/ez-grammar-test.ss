;;; Copyright 2017 Cisco Systems, Inc.
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

;;; This file contains a sample parser defined via the ez-grammar system
;;; and a simple test of the parser.

;;; This file is organized as follows:
;;;
;;; - (streams) library providing the required exports for ez-grammar and
;;;   the parser.
;;;
;;; - (state-case) library exporting the state-case macro, copped from
;;;   cmacros.ss, for use by the lexer.
;;;
;;; - (lexer) library providing a simple lexer that reads characters
;;;   from a port and produces a corresponding stream of tokens.
;;;
;;; - (parser) library providing the sample parser.
;;;
;;; - ez-grammar-test procedure that tests the sample parser.
;;;
;;; Instructions for running the test are at the end of this file.

(library (streams)
  (export stream-cons stream-car stream-cdr stream-nil stream-null?
    stream-map stream stream-append2 stream-append-all stream-last-forced)
  (import (chezscheme))

  (define stream-cons
    (lambda (x thunk)
      (cons x thunk)))

  (define stream-car
    (lambda (x)
      (car x)))

  (define stream-cdr
    (lambda (x)
      (when (procedure? (cdr x)) (set-cdr! x ((cdr x))))
      (cdr x)))

  (define stream-nil '())

  (define stream-null?
    (lambda (x)
      (null? x)))

  (define stream-map
    (lambda (f x)
      (if (stream-null? x)
          '()
          (stream-cons (f (stream-car x))
            (lambda ()
              (stream-map f (stream-cdr x)))))))

  (define stream
    (lambda xs
      xs))

  (define stream-append2
    (lambda (xs thunk)
      (if (null? xs)
          (thunk)
          (stream-cons (stream-car xs)
            (lambda ()
              (stream-append2 (stream-cdr xs) thunk))))))

  (define stream-append-all
    (lambda (stream$) ;; stream of streams
      (if (stream-null? stream$)
          stream$
          (stream-append2 (stream-car stream$)
            (lambda () (stream-append-all (stream-cdr stream$)))))))

  (define stream-last-forced
    (lambda (x)
      (and (not (null? x))
           (let loop ([x x])
             (let ([next (cdr x)])
               (if (pair? next)
                   (loop next)
                   (car x)))))))
)

(library (state-case)
  (export state-case eof)
  (import (chezscheme))

  ;;; from Chez Scheme Version 9.5.1 cmacros.ss
  (define-syntax state-case
    (lambda (x)
      (define state-case-test
        (lambda (cvar k)
          (with-syntax ((cvar cvar))
            (syntax-case k (-)
              (char
                (char? (datum char))
                #'(char=? cvar char))
              ((char1 - char2)
               (and (char? (datum char1)) (char? (datum char2)))
               #'(char<=? char1 cvar char2))
              (predicate
                (identifier? #'predicate)
                #'(predicate cvar))))))
      (define state-case-help
        (lambda (cvar clauses)
          (syntax-case clauses (else)
            (((else exp1 exp2 ...))
             #'(begin exp1 exp2 ...))
            ((((k ...) exp1 exp2 ...) . more)
             (with-syntax (((test ...)
                            (map (lambda (k) (state-case-test cvar k))
                              #'(k ...)))
                           (rest (state-case-help cvar #'more)))
               #'(if (or test ...) (begin exp1 exp2 ...) rest)))
            (((k exp1 exp2 ...) . more)
             (with-syntax ((test (state-case-test cvar #'k))
                           (rest (state-case-help cvar #'more)))
               #'(if test (begin exp1 exp2 ...) rest))))))
      (syntax-case x (eof)
        ((_ cvar (eof exp1 exp2 ...) more ...)
         (identifier? #'cvar)
         (with-syntax ((rest (state-case-help #'cvar #'(more ...))))
           #'(if (eof-object? cvar)
                 (begin exp1 exp2 ...)
                 rest))))))

  (define-syntax eof
    (lambda (x)
      (syntax-error x "misplaced aux keyword")))
)

(library (lexer)
  (export token? token-type token-value token-bfp token-efp lexer)
  (import (chezscheme) (state-case) (streams))

  (define-record-type token
    (nongenerative)
    (fields type value bfp efp))

  ;; test lexer
  (define lexer
    (lambda (fn ip)
      (define $prev-pos 0)
      (define $pos 0)
      (define ($get-char)
        (set! $pos (+ $pos 1))
        (get-char ip))
      (define ($unread-char c)
        (set! $pos (- $pos 1))
        (unread-char c ip))
      (define ($ws!) (set! $prev-pos $pos))
      (define ($make-token type value)
        (let ([tok (make-token type value $prev-pos $pos)])
          (set! $prev-pos $pos)
          tok))
      (define ($lex-error c)
        (errorf #f "unexpected ~a at character ~s of ~a"
          (if (eof-object? c)
              "eof" 
              (format "character '~c'" c))
          $pos fn))
      (define-syntax lex-error
        (syntax-rules ()
          [(_ ?c)
           (let ([c ?c])
             ($lex-error c)
             (void))]))
      (let-values ([(sp get-buf) (open-string-output-port)])
        (define (return-token type value)
          (stream-cons ($make-token type value) lex))
        (module (identifier-initial? identifier-subsequent?)
          (define identifier-initial?
            (lambda (c)
              (char-alphabetic? c)))
          (define identifier-subsequent?
            (lambda (c)
              (or (char-alphabetic? c) 
                  (char-numeric? c)))))
        (define-syntax define-state-case
          (syntax-rules ()
            [(_ ?def-id ?char-id clause ...)
             (define (?def-id)
               (let ([?char-id ($get-char)])
                 (state-case ?char-id clause ...)))]))
        (define-state-case lex c
          [eof stream-nil]
          [char-whitespace? ($ws!) (lex)]
          [char-numeric? (lex-number c)]
          [#\/ (seen-slash)]
          [identifier-initial? (put-char sp c) (lex-identifier)]
          [#\( (return-token 'lparen #\()]
          [#\) (return-token 'rparen #\))] 
          [#\! (return-token 'bang #\!)] 
          [#\+ (seen-plus)] 
          [#\- (seen-minus)] 
          [#\= (seen-equals)] 
          [#\* (return-token 'binop '*)]
          [#\, (return-token 'sep #\,)]
          [#\; (return-token 'sep #\;)]
          [else (lex-error c)])
        (module (lex-identifier)
          (define (id) (return-token 'id (string->symbol (get-buf))))
          (define-state-case next c
            [eof (id)]
            [identifier-subsequent? (put-char sp c) (next)]
            [else ($unread-char c) (id)])
          (define (lex-identifier) (next)))
        (define-state-case seen-plus c
          [eof (return-token 'binop '+)]
          [char-numeric? (lex-signed-number #\+ c)]
          [else (return-token 'binop '+)])
        (define-state-case seen-minus c
          [eof (return-token 'binop '-)]
          [char-numeric? (lex-signed-number #\- c)]
          [else (return-token 'binop '-)])
        (define-state-case seen-equals c
          [eof (return-token 'binop '=)]
          [#\> (return-token 'big-arrow #f)]
          [else (return-token 'binop '=)])
        (module (lex-number lex-signed-number)
          (define (finish-number)
            (let ([str (get-buf)])
              (let ([n (string->number str 10)])
                (unless n (errorf 'lexer "unexpected number literal ~a" str))
                (return-token 'integer n))))
          (define (num)
            (let ([c ($get-char)])
              (state-case c
                [eof (finish-number)]
                [char-numeric? (put-char sp c) (num)]
                [else ($unread-char c) (finish-number)])))
          (define (lex-signed-number s c)
            (put-char sp s)
            (lex-number c))
          (define (lex-number c)
            (state-case c
              [eof (assert #f)]
              [char-numeric? (put-char sp c) (num)]
              [else (assert #f)])))
        (define-state-case seen-slash c
          [eof (return-token 'binop '/)]
          [#\* (lex-block-comment)]
          [#\/ (lex-comment)]
          [else (return-token 'binop '/)])
        (define-state-case lex-comment c
          [eof (lex)]
          [#\newline ($ws!) (lex)]
          [else (lex-comment)])
        (define (lex-block-comment)
          (define-state-case maybe-end-comment c
            [eof (lex-error c)]
            [#\/ ($ws!) (lex)]
            [else (lex-block-comment)])
          (let ([c ($get-char)])
            (state-case c
              [eof (lex-error c)]
              [#\* (maybe-end-comment)]
              [else (lex-block-comment)])))
        (lex))))

  (record-writer (record-type-descriptor token)
    (lambda (x p wr)
      (put-char p #\[)
      (wr (token-type x) p)
      (put-char p #\,)
      (put-char p #\space)
      (wr (token-value x) p)
      (put-char p #\])
      (put-char p #\:)
      (wr (token-bfp x) p)
      (put-char p #\-)
      (wr (token-efp x) p)))
)

(module parser ()
  (export parse *sfd*)
  (import (chezscheme) (streams) (lexer))
  (define *sfd*)
  (module (define-grammar is sat parse-consumed-all? parse-result-value grammar-trace make-src)
    (define (sep->parser sep)
      (cond
        [(char? sep) (sat (lambda (x) (and (eq? (token-type x) 'sep) (eq? (token-value x) sep))))]
        [(symbol? sep) (sat (lambda (x) (eq? (token-type x) sep)))]
        [else (errorf "don't know how to parse separator: ~s" sep)]))
    (meta define (constant? x) (let ([x (syntax->datum x)]) (or (string? x) (char? x))))
    (define constant->parser
      (lambda (const)
        (define (token-sat type val)
          (sat (lambda (x)
                 (let ([ans (and (token? x) (eqv? (token-type x) type) (eqv? (token-value x) val))])
                   (when (grammar-trace) (printf "    ~s is [~s, ~a]? => ~s~%" x type val ans))
                   ans))))
        (if (string? const)
            (case const
              [else (token-sat 'id (string->symbol const))])
            (case const
              [#\( (token-sat 'lparen const)]
              [#\) (token-sat 'rparen const)]
              [#\! (token-sat 'bang const)]
              [else (errorf 'constant->parser "don't know how to construct a parser for ~a" const)]))))
    (meta define (constant->markdown k)
      (format "~a" k))
    (define binop->parser
      (lambda (binop)
        (define (binop-sat type val)
          (is val
            (where [x <- item] (and (token? x) (eq? (token-type x) type) (eq? (token-value x) val)))))
        (define (unexpected) (errorf 'binop->parser "don't know how to construct a parser for ~a" binop))
        (if (string? binop)
            (binop-sat 'binop
              (case binop
                ["=" '=]
                ["+" '+]
                ["-" '-]
                ["*" '*]
                ["/" '/]
                [else (unexpected)]))
            (unexpected))))
    (define make-src
      (lambda (bfp efp)
        (make-source-object *sfd* bfp efp)))
    (include "ez-grammar.ss"))

  (define token
    (case-lambda
      [(type)
       (is (token-value x)
         (where
           [x <- (sat (lambda (x)
                        (let ([ans (eq? (token-type x) type)])
                          (when (grammar-trace) (printf "    ~s is ~s? => ~s~%" x type ans))
                          ans)))]))]
      [(type val)
       (is (token-value x)
         (where
           [x <- (sat (lambda (x)
                        (let ([ans (and
                                     (eq? (token-type x) type)
                                     (eqv? (token-value x) val))])
                          (when (grammar-trace) (printf "    ~s is [~s, ~s]? => ~s~%" x type val ans))
                          ans)))]))]))

  (define identifier (token 'id))

  (define integer (token 'integer))

  (define-grammar expr (markdown-directory ".")
    (TERMINALS
      (identifier (x y) (DESCRIPTION ("An identifier is ...")))
      (integer (i) (DESCRIPTION ("An integer literal is ..."))))
    (expr (e)
      (BINOP src ((RIGHT "=") (LEFT "+" "-") (LEFT "*" "/")) t) =>
        (lambda (src op x y)
          (make-annotation `(,op ,x ,y) src `(,op ,(annotation-stripped x) ,(annotation-stripped y)))))
    (term (t)
      [test-SEP+ :: src "sepplus" #\( (SEP+ e #\;) #\) =>
        (lambda (src e+)
          (make-annotation `(SEP+ ,@e+) src `(SEP+ ,@(map annotation-stripped e+))))]
      [test-SEP* :: src "sepstar" #\( (SEP* e #\,) #\) =>
        (lambda (src e*)
          (make-annotation `(SEP* ,@e*) src `(SEP* ,@(map annotation-stripped e*))))]
      [test-OPT :: src "opt" #\( (OPT e #f) #\) =>
        (lambda (src maybe-e)
          (if maybe-e
              (make-annotation `(OPT ,maybe-e) src `(OPT ,(annotation-stripped maybe-e)))
              (make-annotation `(OPT) src `(OPT))))]
      [test-K+ :: src "kplus" #\( (K+ e) #\) =>
        (lambda (src e+)
          (make-annotation `(K+ ,@e+) src `(K+ ,@(map annotation-stripped e+))))]
      [test-K* :: src "kstar" #\( (K* e) #\) =>
        (lambda (src e*)
          (make-annotation `(K* ,@e*) src `(K* ,@(map annotation-stripped e*))))]
      [varref :: src x =>
        (lambda (src id)
          (make-annotation `(id ,id) src `(id ,id)))]
      [intref :: src i =>
        (lambda (src n)
          (make-annotation `(int ,n) src `(int ,n)))]
      [group :: src #\( e #\) =>
        (lambda (src e)
          `(group ,src ,e))]))

  (define parse
    (lambda (fn ip)
      (let ([token-stream (lexer fn ip)])
        (define (oops)
          (let ([last-token (stream-last-forced token-stream)])
            (if last-token
                (errorf 'parse "parse error at or before character ~s of ~a" (token-bfp last-token) fn)
                (errorf 'parse "no expressions found in ~a" fn))))
        ;;; return the first result, if any, for which the input stream was entirely consumed.
        (let loop ([res* (expr token-stream)])
          (if (null? res*)
              (oops)
              (let ([res (car res*)])
                (if (parse-consumed-all? res)
                    (parse-result-value res)
                    (loop (cdr res*))))))))))

(define run
  (lambda (fn)
    (import parser)
    (let* ([ip (open-file-input-port fn)]
           [sfd (make-source-file-descriptor fn ip #t)]
           [ip (transcoded-port ip (native-transcoder))])
      (fluid-let ([*sfd* sfd])
        (eval
          `(let ()
             (define-syntax define-ops
               (lambda (x)
                 (syntax-case x ()
                   [(_ op ...)
                    #`(begin
                        (define-syntax op
                          (lambda (x)
                            (let ([src (annotation-source (syntax->annotation x))])
                              (with-syntax ([bfp (source-object-bfp src)] [efp (source-object-efp src)])
                                (syntax-case x ()
                                  [(_ e (... ...)) #'`(op (bfp . efp) ,e (... ...))])))))
                        ...)])))
             (define-ops SEP+ SEP* OPT K+ K* id int group)
             (define-ops = + - * /)
             (define x 'x)
             (define y 'y)
             (define z 'z)
             ,(dynamic-wind
                void
                (lambda () (parse fn ip))
                (lambda () (close-input-port ip)))))))))

(define (ez-grammar-test)
  (define n 0)
  (define test
    (lambda (line* okay?)
      (set! n (+ n 1))
      (let ([fn (format "testfile~s" n)])
        (with-output-to-file fn
          (lambda () (for-each (lambda (line) (printf "~a\n" line)) line*))
          'replace)
        (let ([result (parameterize ([compile-profile #t] [compile-interpret-simple #f])
                        (guard (c [else c]) (run fn)))])
          (guard (c [else #f]) (profile-dump-html))
          (delete-file fn)
          (delete-file "profile.html")
          (delete-file (format "~a.html" fn))
          (unless (okay? result)
            (printf "test ~s failed\n" n)
            (printf "  test code:")
            (for-each (lambda (line) (printf "    ~a\n" line)) line*)
            (printf "  result:\n    ")
            (if (condition? result)
                (begin (display-condition result) (newline))
                (parameterize ([pretty-initial-indent 4])
                  (pretty-print result)))
            (newline))))))

  (define-syntax returns
    (syntax-rules ()
      [(_ k) (lambda (x) (equal? x 'k))]))

  (define-syntax oops
    (syntax-rules ()
      [(_ (c) e1 e2 ...)
       (lambda (c) (and (condition? c) e1 e2 ...))]))

  (test
    '(
       "1347"
       )
    (returns
      (int (0 . 4) 1347)))

  (test
    '(
       "3 /*"
       )
    (oops (c)
      (equal? (condition-message c) "unexpected ~a at character ~s of ~a")
      (equal? (condition-irritants c) '("eof" 6 "testfile2"))))

  (test
    '(
       "3 / 4 + 5 opt(6)"
       )
    (oops (c)
      (equal? (condition-message c) "parse error at or before character ~s of ~a")
      (equal? (condition-irritants c) '(10 "testfile3"))))

  (test
    '(
       "x = y = 5"
       )
    (returns
      (=
       (0 . 9)
       (id (0 . 1) x)
       (= (4 . 9) (id (4 . 5) y) (int (8 . 9) 5)))))

  (test
    '(
       "x = y = x + 5 - z * 7 + 8 / z"
       )
    (returns
      (=
       (0 . 29)
       (id (0 . 1) x)
       (=
        (4 . 29)
        (id (4 . 5) y)
        (+
         (8 . 29)
         (-
          (8 . 21)
          (+ (8 . 13) (id (8 . 9) x) (int (12 . 13) 5))
          (* (16 . 21) (id (16 . 17) z) (int (20 . 21) 7)))
         (/ (24 . 29) (int (24 . 25) 8) (id (28 . 29) z)))))))

  (test
    '(
       "opt(opt(opt()))"
       )
    (returns
      (OPT (0 . 15) (OPT (4 . 14) (OPT (8 . 13))))))

  (test
    '(
       "kstar(3 4 kplus(1 2 3 kstar()))"
       )
    (returns
      (K* (0 . 31)
        (int (6 . 7) 3)
        (int (8 . 9) 4)
        (K+ (10 . 30)
          (int (16 . 17) 1)
          (int (18 . 19) 2)
          (int (20 . 21) 3)
          (K* (22 . 29))))))

  (test
    '(
       "sepplus( opt() ; opt(5) ; sepstar(17, 34) ; sepstar())"
       )
    (returns
      (SEP+ (0 . 54)
        (OPT (9 . 14))
        (OPT (17 . 23) (int (21 . 22) 5))
        (SEP* (26 . 41) (int (34 . 36) 17) (int (38 . 40) 34))
        (SEP* (44 . 53)))))

  (delete-file "expr.md")
  (printf "~s tests ran\n" n)
  )

#!eof

The following should print only "<n> tests ran".

echo '(ez-grammar-test)' | ../bin/scheme -q ez-grammar-test.ss
