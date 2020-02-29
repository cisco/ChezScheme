;;; 5_4.ss
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

;;; character and string functions

(begin
(define substring
   (lambda (s1 m n)
      (unless (string? s1)
         ($oops 'substring "~s is not a string" s1))
      (let ([k (string-length s1)])
         (unless (and (fixnum? m) (fixnum? n) (fx<= 0 m n k))
            ($oops 'substring
                   "~s and ~s are not valid start/end indices for ~s"
                   m n s1))
         (let ([s2 (make-string (fx- n m))])
            (do ([j 0 (fx+ j 1)] [i m (fx+ i 1)])
                ((fx= i n) s2)
                (string-set! s2 j (string-ref s1 i)))))))

(define-who string-append
  (case-lambda
    [(s1 s2)
     (unless (string? s1) ($oops who "~s is not a string" s1))
     (unless (string? s2) ($oops who "~s is not a string" s2))
     (let ([n1 (string-length s1)] [n2 (string-length s2)])
       (let ([n (+ n1 n2)])
         (unless (fixnum? n) ($oops who "result string size ~s is not a fixnum" n))
         (let ([s (make-string n)])
           (string-copy! s1 0 s 0 n1)
           (string-copy! s2 0 s n1 n2)
           s)))]
    [args
     (let f ([ls args] [n 0])
       (if (null? ls)
           (if (fixnum? n)
               (make-string n)
               ($oops who "result string size ~s is not a fixnum" n))
           (let ([s1 (car ls)])
             (unless (string? s1) ($oops who "~s is not a string" s1))
             (let ([m (string-length s1)])
               (let ([s2 (f (cdr ls) (+ n m))])
                 (string-copy! s1 0 s2 n m)
                 s2)))))]))

(define string->list
   (lambda (s)
      (unless (string? s)
         ($oops 'string->list "~s is not a string" s))
      (let loop ([i (fx- (string-length s) 1)] [l '()])
        (if (fx> i 0)
            (loop (fx- i 2)
                  (list* (string-ref s (fx- i 1))
                         (string-ref s i)
                         l))
            (if (fx= i 0)
                (cons (string-ref s 0) l)
                l)))))

(define list->string
   (lambda (x)
      (let ([s (make-string ($list-length x 'list->string))])
         (do ([ls x (cdr ls)] [i 0 (fx+ i 1)])
             ((null? ls) s)
             (let ([c (car ls)])
                (unless (char? c)
                   ($oops 'list->string "~s is not a character" c))
                (string-set! s i c))))))

(define-who string-copy
  (lambda (s1)
    (unless (string? s1)
      ($oops who "~s is not a string" s1))
    (let ([n (string-length s1)])
      (let ([s2 (make-string n)])
        ($byte-copy!
          s1 (constant string-data-disp)
          s2 (constant string-data-disp)
          (fx* n (constant string-char-bytes)))
        s2))))

(define-who string-copy!
  (lambda (s1 i1 s2 i2 k)
    (unless (string? s1) ($oops who "~s is not a string" s1))
    (unless (mutable-string? s2) ($oops who "~s is not a mutable string" s2))
    (let ([n1 (string-length s1)] [n2 (string-length s2)])
      (unless (and (fixnum? i1) (fx>= i1 0))
        ($oops who "invalid start value ~s" i1))
      (unless (and (fixnum? i2) (fx>= i2 0))
        ($oops who "invalid start value ~s" i2))
      (unless (and (fixnum? k) (fx>= k 0))
        ($oops who "invalid count ~s" k))
      (unless (fx<= k (fx- n1 i1)) ; avoid overflow
        ($oops who "index ~s + count ~s is beyond the end of ~s" i1 k s1))
      (unless (fx<= k (fx- n2 i2)) ; avoid overflow
        ($oops who "index ~s + count ~s is beyond the end of ~s" i2 k s2))
    ; whew!
      (#3%string-copy! s1 i1 s2 i2 k))))

(set-who! string->immutable-string
  (lambda (v)
    (cond
      [(immutable-string? v) v]
      [(eqv? v "") ($tc-field 'null-immutable-string ($tc))]
      [else
       (unless (string? v) ($oops who "~s is not a string" v))
       (let ([v2 (string-copy v)])
         ($string-set-immutable! v2)
         v2)])))

(define-who substring-fill!
   (lambda (s m n c)
      (unless (mutable-string? s)
         ($oops who "~s is not a mutable string" s))
      (unless (char? c)
         ($oops who "~s is not a character" c))
      (let ([k (string-length s)])
         (unless (and (fixnum? m) (fixnum? n) (fx<= 0 m n k))
            ($oops who
                   "~s and ~s are not valid start/end indices for ~s"
                   m n s))
         (do ([i m (fx+ i 1)])
             ((fx= i n))
             (string-set! s i c)))))

(set! string-for-each
  (case-lambda
    [(p s)
     (unless (procedure? p) ($oops 'string-for-each "~s is not a procedure" p))
     (unless (string? s) ($oops 'string-for-each "~s is not a string" s))
     (#3%string-for-each p s)]
    [(p s t)
     (unless (procedure? p) ($oops 'string-for-each "~s is not a procedure" p))
     (unless (string? s) ($oops 'string-for-each "~s is not a string" s))
     (unless (string? t) ($oops 'string-for-each "~s is not a string" t))
     (let ([n (string-length s)])
       (unless (fx= (string-length t) n)
         ($oops 'string-for-each "lengths of input string ~s and ~s differ" s t))
       (unless (fx= n 0)
         (let loop ([i 0])
           (let ([j (fx+ i 1)])
             (if (fx= j n)
                 (p (string-ref s i) (string-ref t i))
                 (begin
                   (p (string-ref s i) (string-ref t i))
                   (loop j)))))))]
    [(p s . t*)
     (unless (procedure? p) ($oops 'string-for-each "~s is not a procedure" p))
     (unless (string? s) ($oops 'string-for-each "~s is not a string" s))
     (for-each (lambda (t) (unless (string? t) ($oops 'string-for-each "~s is not a string" t))) t*)
     (let ([n (string-length s)])
       (for-each
         (lambda (t)
           (unless (fx= (string-length t) n)
             ($oops 'string-for-each "lengths of input string ~s and ~s differ" s t)))
         t*)
       (unless (fx= n 0)
         (let loop ([i 0])
           (let ([j (fx+ i 1)])
             (if (fx= j n)
                 (apply p (string-ref s i) (map (lambda (t) (string-ref t i)) t*))
                 (begin
                   (apply p (string-ref s i) (map (lambda (t) (string-ref t i)) t*))
                   (loop j)))))))]))

;;; The following code is covered by the following copyright/license.

;;; Copyright (C) 2008  Abdulaziz Ghuloum, R. Kent Dybvig
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(let ()
  (include "../unicode/unicode-char-cases.ss")
  (include "../unicode/unicode-charinfo.ss")

  (define char-error
    (lambda (who what)
      ($oops who "~s is not a character" what)))

  (define string-error
    (lambda (who what)
      ($oops who "~s is not a string" what)))

  (set! $string-char-foldcase (lambda (c) ($str-foldcase c)))

  (let ()
    (define-syntax define-char-op
      (syntax-rules ()
        [(_ name unsafe-op)
         (set-who! name
           (lambda (c)
             (if (char? c) 
                 (unsafe-op c)
                 ($oops who "~s is not a character" c))))]))
    
    (define-char-op char-upcase $char-upcase)
    (define-char-op char-downcase $char-downcase)
    (define-char-op char-titlecase $char-titlecase)
    (define-char-op char-foldcase $char-foldcase)
    (define-char-op char-whitespace? $char-whitespace?)
    (define-char-op char-lower-case? $char-lower-case?)
    (define-char-op char-upper-case? $char-upper-case?)
    (define-char-op char-title-case? $char-title-case?)
    (define-char-op char-numeric? $char-numeric?)
    (define-char-op char-alphabetic? $char-alphabetic?)
    (define-char-op char-general-category $char-category)
    (define-char-op $constituent? $char-constituent?)
    (define-char-op $subsequent? $char-subsequent?)
  )

  (let ()
    (define (check-chars who ls)
      (let loop ([ls ls])
        (and (not (null? ls))
             (let ([x (car ls)])
               (if (char? x)
                   (loop (cdr ls))
                   (char-error who x))))))

    (define-syntax char-relop
      (lambda (x)
        (syntax-case x ()
          [(_ name filter) #'(char-relop name name filter)]
          [(_ name pred filter)
           (let ()
             (define (foo xname onearg)
               #`(set-who! #,xname
                   (case-lambda
                     [(x1 x2)
                      (if (char? x1)
                          (if (char? x2)
                              (#3%pred (filter x1) (filter x2))
                              (char-error who x2))
                          (char-error who x1))]
                     [(x1 x2 x3)
                      (if (char? x1)
                          (if (char? x2)
                              (if (char? x3)
                                  (let ([x2 (filter x2)])
                                    (and (#3%pred (filter x1) x2)
                                         (#3%pred x2 (filter x3))))
                                  (char-error who x3))
                              (char-error who x2))
                          (char-error who x1))]
                     #,@(if onearg (list onearg) '())
                     [(x1 x2 . rest)
                      (if (char? x1)
                          (let loop ([x1 (filter x1)] [x2 x2] [ls rest])
                            (if (char? x2)
                                (let ([x2 (filter x2)])
                                  (if (#3%pred x1 x2)
                                      (or (null? ls) (loop x2 (car ls) (cdr ls)))
                                      (check-chars who ls)))
                                (char-error who x2)))
                          (char-error who x1))])))
             #`(begin
                 #,(foo #'#(r6rs: name) #f)
                 #,(foo #'name #'[(x) (if (char? x) #t (char-error who x))])))])))

    (char-relop char<? values)
    (char-relop char<=? values)
    (char-relop char=? values)
    (char-relop char>=? values)
    (char-relop char>? values)

    (char-relop char-ci<? char<? $char-foldcase)
    (char-relop char-ci<=? char<=? $char-foldcase)
    (char-relop char-ci=? char=? $char-foldcase)
    (char-relop char-ci>=? char>=? $char-foldcase)
    (char-relop char-ci>? char>? $char-foldcase)
  )

  (let ()
    (define (handle-special str ac) 
      (define (chars ac n)
        (cond
          [(null? ac) n]
          [else 
           (chars (cdr ac)
             (let f ([p (cdar ac)] [n n])
               (cond
                 [(pair? p) (f (cdr p) (fx+ n 1))]
                 [else n])))]))
      (define (extend src ac src-len dst-len)
        (let f ([str str] [dst (make-string dst-len)] [i 0] [j 0] [ac (reverse ac)] [sigma* '()])
          (cond
            [(null? ac) 
             (string-copy! str i dst j (fx- src-len i))
             (do-sigmas dst sigma*)]
            [else
             (let ([idx (caar ac)] [c* (cdar ac)] [ac (cdr ac)])
               (let ([cnt (fx- idx i)])
                 (string-copy! str i dst j cnt)
                 (let g ([str str]       [dst dst] 
                         [i (fx+ i cnt)] [j (fx+ j cnt)] 
                         [ac ac]         [c* c*]) 
                   (cond
                     [(pair? c*) 
                      (string-set! dst j (car c*))
                      (g str dst i (fx+ j 1) ac (cdr c*))]
                     [(char? c*)
                      (string-set! dst j c*)
                      (f str dst (fx+ i 1) (fx+ j 1) ac sigma*)]
                     [else ; assume c* = sigma
                      (f str dst (fx+ i 1) (fx+ j 1) ac (cons j sigma*))]))))])))
      (define (do-sigmas str sigma*)
        (define nonfinal-sigma #\x3c3)
        (define final-sigma #\x3c2)
        (define (final? i)
          (define (scan i incr n)
            (and (not (fx= i n))
              (or ($char-cased? (string-ref str i))
                  (and ($char-case-ignorable? (string-ref str i))
                       (scan (fx+ i incr) incr n)))))
          (and (scan (fx- i 1) -1 -1) (not (scan (fx+ i 1) +1 (string-length str)))))
       ; scanning requires we have some character in place...guess nonfinal sigma
        (for-each (lambda (i) (string-set! str i nonfinal-sigma)) sigma*)
        (for-each (lambda (i) (when (final? i) (string-set! str i final-sigma))) sigma*)
        str)
      (let* ([src-len (string-length str)]
             [dst-len (chars ac src-len)])
         (if (fx= dst-len src-len)
             (do-sigmas str (map car ac))
             (extend str ac src-len dst-len))))

    (define (string-change-case str cvt-char)
      (let ([n (string-length str)])
        (let f ([str str] [dst (make-string n)] [i 0] [n n] [ac '()])
          (cond
            [(fx= i n) 
             (if (null? ac) 
                 dst
                 (handle-special dst ac))]
            [else
             (let ([c/ls (cvt-char (string-ref str i))])
               (cond
                 [(char? c/ls)
                  (string-set! dst i c/ls)
                  (f str dst (fx+ i 1) n ac)]
                 [else
                  (f str dst (fx+ i 1) n 
                     (cons (cons i c/ls) ac))]))]))))

    (set-who! string-upcase
      (lambda (s)
        (unless (string? s) (string-error who s))
        (string-change-case s $str-upcase)))

    (set-who! string-foldcase
      (lambda (s)
        (unless (string? s) (string-error who s))
        (string-change-case s $str-foldcase)))

    (set-who! string-downcase
      (lambda (s)
        (unless (string? s) (string-error who s))
        (string-change-case s $str-downcase)))

    (set-who! string-titlecase
      (lambda (str)
        (unless (string? str) (string-error who str))
        (let* ([n (string-length str)] [dst (make-string n)])
          (define (trans2 s i seen-cased? ac)
            (if (fx= i n)
                (handle-special dst ac)
                (s i seen-cased? ac)))
          (define (trans1 s i c/ls seen-cased? ac)
            (cond
              [(char? c/ls)
               (string-set! dst i c/ls)
               (trans2 s (fx+ i 1) seen-cased? ac)]
              [else
               (trans2 s (fx+ i 1) seen-cased? (cons (cons i c/ls) ac))]))
          (define (trans s i c seen-cased? ac)
            (if seen-cased?
                (trans1 s i ($str-downcase c) #t ac)
                (if ($char-cased? c)
                    (trans1 s i ($str-titlecase c) #t ac)
                    (trans1 s i c #f ac))))
          ; NB: if used as a pattern for word breaking, take care not to break between CR & LF (WB3)
          ; NB: and between regional-indicators (WB13c).  also take care not to let handling of WB6 and
          ; NB: WB7 here prevent breaks in, e.g., "a." when not followed by, e.g., another letter.
          (define (s0 i ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-aletter? c) (trans sAletter i c #f ac)]
                [($wb-hebrew-letter? c) (trans sHebrewletter i c #f ac)]
                [($wb-numeric? c) (trans sNumeric i c #f ac)]
                [($wb-katakana? c) (trans sKatakana i c #f ac)]
                [($wb-extendnumlet? c) (trans sExtendnumlet i c #f ac)]
                [($wb-regional-indicator? c) (trans sRegionalIndicator i c #f ac)]
                [else (string-set! dst i c)
                      (let ([i (fx+ i 1)])
                        (if (fx= i n)
                            (handle-special dst ac)
                            (s0 i ac)))])))
          (define (sAletter i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB5
                [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB5
                [(or ($wb-midletter? c) ($wb-midnumlet? c) ($wb-single-quote? c)) (trans sWB6/WB7/WB7a i c seen-cased? ac)] ; WB6/WB7
                [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB9
                [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
                [(or ($wb-extend? c) ($wb-format? c)) (trans sAletter i c seen-cased? ac)] ; WB4
                [else (s0 i ac)]))) ; WB14
          (define (sHebrewletter i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB5
                [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB5
                [(or ($wb-midletter? c) ($wb-midnumlet? c) ($wb-single-quote? c)) (trans sWB6/WB7/WB7a i c seen-cased? ac)] ; WB6/WB7/WB7a
                [($wb-double-quote? c) (trans sWB7b/WB7c i c seen-cased? ac)] ; WB7b, WB7c
                [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB9
                [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
                [(or ($wb-extend? c) ($wb-format? c)) (trans sHebrewletter i c seen-cased? ac)] ; WB4
                [else (s0 i ac)]))) ; WB14
          (define (sWB6/WB7/WB7a i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB6, WB7
                [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB6, WB7
                [(or ($wb-extend? c) ($wb-format? c)) (trans sWB6/WB7/WB7a i c seen-cased? ac)] ; WB4
                ; word break actually should/could have occurred one character earlier if we got here
                ; from sAletter rather than sHebrewletter but that was before a midlet, midnumlet, or single
                ; quote which has no titlecase
                [else (s0 i ac)]))) ; WB14
          (define (sWB7b/WB7c i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB7b, WB7c
                [(or ($wb-extend? c) ($wb-format? c)) (trans sWB7b/WB7c i c seen-cased? ac)] ; WB4
                ; word break actually should/could have occurred one character earlier
                ; but that was before a double quote which has no titlecase
                [else (s0 i ac)]))) ; WB14
          (define (sSingleQuote i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; finishing WB6, WB7
                [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; finishing WB6, WB7
                [(or ($wb-extend? c) ($wb-format? c)) (trans sSingleQuote i c seen-cased? ac)] ; WB4
                [else (s0 i ac)]))) ; WB14
          (define (sNumeric i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB8
                [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB10
                [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB10
                [(or ($wb-midnum? c) ($wb-midnumlet? c) ($wb-single-quote? c)) (trans sWB11/WB12 i c seen-cased? ac)] ; WB11, WB12
                [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)]
                [(or ($wb-extend? c) ($wb-format? c)) (trans sNumeric i c seen-cased? ac)] ; WB4
                [else (s0 i ac)]))) ; WB14
          (define (sWB11/WB12 i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)]
                [(or ($wb-extend? c) ($wb-format? c)) (trans sWB11/WB12 i c seen-cased? ac)] ; WB4
                ; word break actually should/could have occurred one character earlier
                ; but that was before a midnum, midnumlet, or single quote which has no titltecase
                [else (s0 i ac)]))) ; WB14
          (define (sKatakana i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-katakana? c) (trans sKatakana i c seen-cased? ac)] ; WB13
                [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
                [(or ($wb-extend? c) ($wb-format? c)) (trans sKatakana i c seen-cased? ac)] ; WB4
                [else (s0 i ac)]))) ; WB14
          (define (sExtendnumlet i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-extendnumlet? c) (trans sExtendnumlet i c seen-cased? ac)] ; WB13a
                [($wb-aletter? c) (trans sAletter i c seen-cased? ac)] ; WB13b
                [($wb-hebrew-letter? c) (trans sHebrewletter i c seen-cased? ac)] ; WB13b
                [($wb-numeric? c) (trans sNumeric i c seen-cased? ac)] ; WB13b
                [($wb-katakana? c) (trans sKatakana i c seen-cased? ac)] ; WB13b
                [(or ($wb-extend? c) ($wb-format? c)) (trans sExtendnumlet i c seen-cased? ac)] ; WB4
                [else (s0 i ac)]))) ; WB14
          (define (sRegionalIndicator i seen-cased? ac)
            (let ([c (string-ref str i)])
              (cond
                [($wb-regional-indicator? c) (trans sRegionalIndicator i c seen-cased? ac)] ; WB13c
                [(or ($wb-extend? c) ($wb-format? c)) (trans sExtendnumlet i c seen-cased? ac)] ; WB4
                [else (s0 i ac)]))) ; WB14
          (if (fx= n 0) dst (s0 0 '())))))
  )

  (let ()
    (define-syntax string-relop
      (syntax-rules ()
        [(_ (name x1 x2) pred)
         (set! name
           (rec name
             (case-lambda
               [(x1 x2)
                (if (string? x1)
                    (if (string? x2)
                        pred
                        (string-error 'name x2))
                    (string-error 'name x1))]
               [(x1) (begin (name x1 "") #t)]
               [(x1 x2 . rest)
                (let loop ([x1 x1] [x2 x2] [ls rest])
                  (if (or (null? ls) (loop x2 (car ls) (cdr ls)))
                      (name x1 x2)
                      (begin (name x1 x2) #f)))])))]))

    (define-syntax r6rs:string-relop
      (syntax-rules ()
        [(_ (name x1 x2) pred)
         (set-who! #(r6rs: name) ; implies (rec name ---)
           (case-lambda
             [(x1 x2)
              (if (string? x1)
                  (if (string? x2)
                      pred
                      (string-error 'name x2))
                  (string-error 'name x1))]
             [(x1 x2 . rest)
              (let loop ([x1 x1] [x2 x2] [ls rest])
                (if (or (null? ls) (loop x2 (car ls) (cdr ls)))
                    (name x1 x2)
                    (begin (name x1 x2) #f)))]))]))

    (define string-equal?
      (lambda (s1 s2)
        (or (eq? s1 s2)
            (let ([n (string-length s1)])
              (and (fx= n (string-length s2))
                   (let f ([i 0])
                     (or (fx= i n)
                         (and (char=? (string-ref s1 i) (string-ref s2 i))
                              (f (fx+ i 1))))))))))

    (define string-less?
      (lambda (s1 s2)
        (and (not (eq? s1 s2))
             (let ([n1 (string-length s1)] [n2 (string-length s2)])
               (let f ([i 0])
                 (and (not (fx= i n2))
                      (or (fx= i n1)
                          (let ([c1 (string-ref s1 i)]
                                [c2 (string-ref s2 i)])
                            (or (char<? c1 c2)
                                (and (char=? c1 c2) (f (fx+ i 1))))))))))))

    (define string-ci-equal?
      (lambda (s1 s2)
        (or (eq? s1 s2)
            (let ([n1 (string-length s1)] [n2 (string-length s2)])
              (if (fx= n1 0)
                  (fx= n2 0)
                  (and (not (fx= n2 0))
                       (let f ([i1 1]
                               [i2 1]
                               [c1* ($str-foldcase (string-ref s1 0))]
                               [c2* ($str-foldcase (string-ref s2 0))])
                         (if (char? c1*)
                             (if (char? c2*)
                                 (and (char=? c1* c2*)
                                      (if (fx= i1 n1)
                                          (fx= i2 n2)
                                          (and (not (fx= i2 n2))
                                               (f (fx+ i1 1) (fx+ i2 1)
                                                 ($str-foldcase (string-ref s1 i1))
                                                 ($str-foldcase (string-ref s2 i2))))))
                                 (and (char=? c1* (car c2*))
                                      (not (fx= i1 n1))
                                      (f (fx+ i1 1) i2
                                        ($str-foldcase (string-ref s1 i1))
                                        (cdr c2*))))
                             (if (char? c2*)
                                 (and (char=? (car c1*) c2*)
                                      (not (fx= i2 n2))
                                      (f i1 (fx+ i2 1) (cdr c1*)
                                        ($str-foldcase (string-ref s2 i2))))
                                 (and (char=? (car c1*) (car c2*))
                                      (f i1 i2 (cdr c1*) (cdr c2*))))))))))))

    (define string-ci-less?
      (lambda (s1 s2)
        (and (not (eq? s1 s2))
             (let ([n1 (string-length s1)] [n2 (string-length s2)])
               (and (not (fx= n2 0))
                    (or (fx= n1 0)
                        (let f ([i1 1]
                                [i2 1]
                                [c1* ($str-foldcase (string-ref s1 0))]
                                [c2* ($str-foldcase (string-ref s2 0))])
                          (if (char? c1*)
                              (if (char? c2*)
                                  (or (char<? c1* c2*)
                                      (and (char=? c1* c2*)
                                           (not (fx= i2 n2))
                                           (or (fx= i1 n1)
                                               (f (fx+ i1 1) (fx+ i2 1)
                                                 ($str-foldcase (string-ref s1 i1))
                                                 ($str-foldcase (string-ref s2 i2))))))
                                  (or (char<? c1* (car c2*))
                                      (and (char=? c1* (car c2*))
                                           (or (fx= i1 n1)
                                               (f (fx+ i1 1) i2
                                                 ($str-foldcase (string-ref s1 i1))
                                                 (cdr c2*))))))
                              (if (char? c2*)
                                  (or (char<? (car c1*) c2*)
                                      (and (char=? (car c1*) c2*)
                                           (not (fx= i2 n2))
                                           (f i1 (fx+ i2 1) (cdr c1*)
                                             ($str-foldcase (string-ref s2 i2)))))
                                  (or (char<? (car c1*) (car c2*))
                                      (and (char=? (car c1*) (car c2*))
                                           (f i1 i2 (cdr c1*) (cdr c2*)))))))))))))

    (string-relop (string=? x1 x2) (string-equal? x1 x2))
    (string-relop (string<? x1 x2) (string-less? x1 x2))
    (string-relop (string>? x1 x2) (string-less? x2 x1))
    (string-relop (string<=? x1 x2) (not (string-less? x2 x1)))
    (string-relop (string>=? x1 x2) (not (string-less? x1 x2)))

    (string-relop (string-ci=? x1 x2) (string-ci-equal? x1 x2))
    (string-relop (string-ci<? x1 x2) (string-ci-less? x1 x2))
    (string-relop (string-ci>? x1 x2) (string-ci-less? x2 x1))
    (string-relop (string-ci<=? x1 x2) (not (string-ci-less? x2 x1)))
    (string-relop (string-ci>=? x1 x2) (not (string-ci-less? x1 x2)))
 
    (r6rs:string-relop (string=? x1 x2) (string-equal? x1 x2))
    (r6rs:string-relop (string<? x1 x2) (string-less? x1 x2))
    (r6rs:string-relop (string>? x1 x2) (string-less? x2 x1))
    (r6rs:string-relop (string<=? x1 x2) (not (string-less? x2 x1)))
    (r6rs:string-relop (string>=? x1 x2) (not (string-less? x1 x2)))

    (r6rs:string-relop (string-ci=? x1 x2) (string-ci-equal? x1 x2))
    (r6rs:string-relop (string-ci<? x1 x2) (string-ci-less? x1 x2))
    (r6rs:string-relop (string-ci>? x1 x2) (string-ci-less? x2 x1))
    (r6rs:string-relop (string-ci<=? x1 x2) (not (string-ci-less? x2 x1)))
    (r6rs:string-relop (string-ci>=? x1 x2) (not (string-ci-less? x1 x2)))
  )

  (let ()
    (module (hangul-sbase hangul-slimit $hangul-decomp
             hangul-lbase hangul-llimit
             hangul-vbase hangul-vlimit
             hangul-tbase hangul-tlimit
             hangul-vcount hangul-tcount)
     ; adapted from UAX #15
      (define SBase #xAC00)
      (define LBase #x1100)
      (define VBase #x1161)
      (define TBase #x11A7)
      (define LCount 19)
      (define VCount 21)
      (define TCount 28)
      (define NCount (* VCount TCount))
      (define SCount (* LCount NCount))
      (define hangul-sbase (integer->char SBase))
      (define hangul-slimit (integer->char (+ SBase SCount -1)))
      (define hangul-lbase (integer->char LBase))
      (define hangul-llimit (integer->char (+ LBase LCount -1)))
      (define hangul-vbase (integer->char VBase))
      (define hangul-vlimit (integer->char (+ VBase VCount -1)))
      (define hangul-tbase (integer->char TBase))
      (define hangul-tlimit (integer->char (+ TBase TCount -1)))
      (define hangul-vcount VCount)
      (define hangul-tcount TCount)
      (define ($hangul-decomp c)
        (let ([SIndex (char- c hangul-sbase)])
          (let ([L (integer->char (fx+ LBase (fxdiv SIndex NCount)))]
                [V (integer->char (fx+ VBase (fxdiv (fxmod SIndex NCount) TCount)))]
                [adj (fxmod SIndex TCount)])
            (if (fx= adj 0)
                (cons* L V)
                (cons* L V (integer->char (fx+ TBase adj))))))))

    (define $decompose
     ; might should optimize for sequences of ascii characters
      (lambda (s canonical?)
        (let ([n (string-length s)] [ac '()])
          (define (canonical>? c1 c2)
            (fx> ($char-combining-class c1) ($char-combining-class c2)))
          (define (sort-and-flush comb*)
            (unless (null? comb*)
              (set! ac (append (list-sort canonical>? comb*) ac))))
          (define ($char-decomp c)
            (if (and (char<=? hangul-sbase c) (char<=? c hangul-slimit))
                ($hangul-decomp c)
                (if canonical?
                    ($str-decomp-canon c)
                    ($str-decomp-compat c))))
          (define (push-and-go c* c** i comb*)
            (if (char? c*)
                (go c* c** i comb*)
                (go (car c*) (cons (cdr c*) c**) i comb*)))
          (define (pop-and-go c** i comb*)
            (if (null? c**)
                (if (fx= i n)
                    (sort-and-flush comb*)
                    (go (string-ref s i) '() (fx+ i 1) comb*))
                (push-and-go (car c**) (cdr c**) i comb*)))
          (define (go c c** i comb*)
            (let ([c* ($char-decomp c)])
              (if (eq? c c*) ; should be eqv?
                  (if (fxzero? ($char-combining-class c))
                      (begin
                        (sort-and-flush comb*)
                        (set! ac (cons c ac))
                        (pop-and-go c** i '()))
                      (pop-and-go c** i (cons c comb*)))
                  (push-and-go c* c** i comb*))))
          (pop-and-go '() 0 '())
          (list->string (reverse ac)))))

    (define $compose
      (let ([comp-table #f])
        (define (lookup-composite c1 c2)
          (hashtable-ref comp-table (cons c1 c2) #f))
        (define (init!)
          (set! comp-table
            (make-hashtable
              (lambda (x)
                (fxxor
                  (fxsll (char->integer (car x)) 7)
                  (char->integer (cdr x))))
              (lambda (x y)
                (and (char=? (car x) (car y))
                     (char=? (cdr x) (cdr y))))))
          (vector-for-each
            (lambda (c* c) (hashtable-set! comp-table c* c))
            (car ($composition-pairs))
            (cdr ($composition-pairs))))
        (lambda (s)
          (unless comp-table (init!))
          (let ([ac '()] [n (string-length s)])
            (define (dump c acc)
              (set! ac (cons c ac))
              (unless (null? acc) (set! ac (append acc ac))))
            (define (s0 i)
              (unless (fx= i n)
                (let ([c (string-ref s i)])
                  (if (fxzero? ($char-combining-class c))
                      (s1 (fx+ i 1) c)
                      (begin (set! ac (cons c ac)) (s0 (fx+ i 1)))))))
            (define (s1 i c)
              (if (fx= i n)
                  (set! ac (cons c ac))
                  (let ([c1 (string-ref s i)])
                    (cond
                      [(and (and (char<=? hangul-lbase c) 
                                 (char<=? c hangul-llimit))
                            (and (char<=? hangul-vbase c1)
                                 (char<=? c1 hangul-vlimit)))
                       (s1 (fx+ i 1)
                           (let ([lindex (char- c hangul-lbase)]
                                 [vindex (char- c1 hangul-vbase)])
                             (integer->char
                               (fx+ (char->integer hangul-sbase)
                                    (fx* (fx+ (fx* lindex hangul-vcount) vindex)
                                         hangul-tcount)))))]
                      [(and (and (char<=? hangul-sbase c)
                                 (char<=? c hangul-slimit))
                            (and (char<=? hangul-tbase c1)
                                 (char<=? c1 hangul-tlimit))
                            (let ([sindex (char- c hangul-sbase)])
                              (fxzero? (fxmod sindex hangul-tcount))))
                       (let ([tindex (char- c1 hangul-tbase)])
                         (s1 (fx+ i 1) (integer->char (fx+ (char->integer c) tindex))))]
                      [else (s2 i c -1 '())]))))
            (define (s2 i c class acc)
              (if (fx= i n)
                  (dump c acc)
                  (let ([c1 (string-ref s i)])
                    (let ([class1 ($char-combining-class c1)])
                      (cond
                        [(and (fx< class class1) (lookup-composite c c1)) =>
                         (lambda (c) (s2 (fx+ i 1) c class acc))]
                        [(fx= class1 0)
                         (dump c acc)
                         (s1 (fx+ i 1) c1)]
                        [else (s2 (fx+ i 1) c class1 (cons c1 acc))])))))
            (s0 0)
            (list->string (reverse ac))))))

    (set-who! string-normalize-nfd
      (lambda (s)
        (unless (string? s) (string-error who s))
        ($decompose s #t)))

    (set-who! string-normalize-nfkd
      (lambda (s)
        (unless (string? s) (string-error who s))
        ($decompose s #f)))

    (set-who! string-normalize-nfc
      (lambda (s)
        (unless (string? s) (string-error who s))
        ($compose ($decompose s #t))))

    (set-who! string-normalize-nfkc
      (lambda (s)
        (unless (string? s) (string-error who s))
        ($compose ($decompose s #f))))
  )
)
)
