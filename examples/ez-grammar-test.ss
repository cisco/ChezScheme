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

  ;;; from Chez Scheme Version 9.4 cmacros.ss
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
        (let ([tok (make-token type value $prev-pos (- $pos 1))])
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
          [#\/ (seen-/)]
          [identifier-initial? (put-char sp c) (lex-identifier)]
          [#\( (return-token 'lparen #\()]
          [#\) (return-token 'rparen #\))] 
          [#\! (return-token 'bang #\!)] 
          [#\+ (seen-plus)] 
          [#\- (seen-minus)] 
          [#\= (seen-equals)] 
          [else (lex-error c)])
        (module (lex-identifier)
          (define (id) (return-token 'id (string->symbol (get-buf))))
          (define-state-case next c
            [eof (id)]
            [identifier-subsequent? (put-char sp c) (next)]
            [else ($unread-char c) (id)])
          (define (lex-identifier) (next)))
        (define-state-case seen-plus c
          [eof (lex-error c)]
          [char-numeric? (lex-signed-number #\+ c)]
          [else (lex-error c)])
        (define-state-case seen-minus c
          [eof (lex-error c)]
          [char-numeric? (lex-signed-number #\- c)]
          [else (lex-error c)])
        (define-state-case seen-equals c
          [eof (lex-error c)]
          [#\> (return-token 'big-arrow #f)]
          [else (lex-error c)])
        (module (lex-number lex-signed-number)
          (define (finish-number)
            (let ([str (get-buf)])
              (let ([n (string->number str 10)])
                (unless n (errorf 'parse-ftc "unexpected number literal ~a" str))
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
        (define-state-case seen-/ c
          [eof (lex-error c)]
          [#\* (lex-block-comment)]
          [#\/ (lex-comment)]
          [else (lex-error c)])
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

(library (parser)
  (export parse)
  (import (chezscheme) (streams) (lexer))
  (module (define-grammar is sat parse-consumed-all? parse-result-value grammar-trace make-src)
    (define (sep->parser sep)
      (cond
        [(char? sep) (sat (lambda (x) (eq? (token-value x) sep)))]
        [(symbol? sep) (sat (lambda (x) (eq? (token-type x) sep)))]
        [else (errorf "don't know how to parse separator: ~s" sep)]))
    (meta define (constant? x) (let ([x (syntax->datum x)]) (or (string? x) (char? x))))
    (define constant->parser
      (let ()
        (define (token-sat type val)
          (sat (lambda (x)
                 (let ([ans (and (token? x) (eqv? (token-type x) type) (eqv? (token-value x) val))])
                   (when (grammar-trace) (printf "    ~s is [~s, ~a]? => ~s~%" x type val ans))
                   ans))))
        (lambda (const)
          (if (string? const)
              (case const
                ["=>" (token-sat 'big-arrow #f)]
                [else (token-sat 'id (string->symbol const))])
              (case const
                [#\( (token-sat 'lparen const)]
                [#\) (token-sat 'rparen const)]
                [#\! (token-sat 'bang const)]
                [else (errorf 'constant->parser "don't know how to construct a parser for ~a" const)])))))
    (define make-src (lambda (bfp efp) (and (<= bfp efp) (cons bfp efp))))
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

  (define-grammar expr
    (expr
      [integer :: src (token 'integer) =>
        (lambda (src n)
          `(int ,src ,n))]
      [becomes :: src "=>" expr =>
        (lambda (src e)
          `(=> ,src ,e))]
      [becomes! :: src "=>" #\! expr =>
        (lambda (src e)
          `(=>! ,src ,e))]
      [group :: src #\( expr #\) =>
        (lambda (src e)
          `(group ,src ,e))]))

  (define parse
    (lambda (fn)
      (let ([ip (open-input-file fn)])
        (dynamic-wind
          void
          (lambda ()
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
                          (loop (cdr res*))))))))
          (lambda () (close-input-port ip))))))
  )

(define (ez-grammar-test)
  (import (parser))
  (with-output-to-file "ez-grammar-test1"
    (lambda ()
      (for-each display
        '(
           "1347\n"
           )))
    'replace)

  (with-output-to-file "ez-grammar-test2"
    (lambda ()
      (for-each display
        '(
           "\n"
           "/* hello */ => ( => 1253) /* goodbye\n"
           "         111111111122222222223333333333\n"
           "123456789012345678901234567890123456789\n"
           "*/\n"
           )))
    'replace)

  (with-output-to-file "ez-grammar-test3err"
    (lambda ()
      (for-each display
        '(
           "\n"
           "/* hello */ => (=> 1253 =>) /* goodbye\n"
           "         111111111122222222223333333333\n"
           "123456789012345678901234567890123456789\n"
           "*/\n"
           )))
    'replace)

  (with-output-to-file "ez-grammar-test4err"
    (lambda ()
      (for-each display
        '(
           "3 /*\n"
           )))
    'replace)

  (unless (guard (c [else #f]) (equal? (parse "ez-grammar-test1") (quote (int (0 . 3) 1347))))
    (printf "test 1 failed\n"))
  (delete-file "ez-grammar-test1")
  (unless (guard (c [else #f]) (equal? (parse "ez-grammar-test2") (quote (=> (13 . 25) (group (16 . 25) (=> (18 . 24) (int (21 . 24) 1253)))))))
    (printf "test 2 failed\n"))
  (delete-file "ez-grammar-test2")
  (unless (guard (c [else (and (equal? (condition-message c) "parse error at or before character ~s of ~a") (equal? (condition-irritants c) (quote (25 "ez-grammar-test3err"))))]) (parse "ez-grammar-test3err") #f)
    (printf "test 3 failed\n"))
  (delete-file "ez-grammar-test3err")
  (unless (guard (c [else (and (equal? (condition-message c) "unexpected ~a at character ~s of ~a") (equal? (condition-irritants c) (quote ("eof" 6 "ez-grammar-test4err"))))]) (parse "ez-grammar-test4err") #f)
    (printf "test 4 failed\n"))
  (delete-file "ez-grammar-test4err")
  (printf "end of tests\n"))

#!eof

The following should print only "end of tests".

echo '(ez-grammar-test)' | scheme -q ez-grammar-test.ss
