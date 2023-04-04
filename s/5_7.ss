;;; 5_7.ss
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

;;; symbol functions

(begin
(define property-list
   (lambda (s)
      (unless (symbol? s)
         ($oops 'property-list "~s is not a symbol" s))
      (list-copy ($symbol-property-list s))))

(define putprop
   (lambda (s p v)
      (if (symbol? s)
          (let pt ([pl ($symbol-property-list s)])
             (cond
                [(null? pl)
                 ($set-symbol-property-list! s
                    (cons p (cons v ($symbol-property-list s))))]
                [(eq? (car pl) p)
                 (set-car! (cdr pl) v)]
                [else (pt (cdr (cdr pl)))]))
          ($oops 'putprop "~s is not a symbol" s))))

(define remprop
   (lambda (s p)
      (if (symbol? s)
          (let pt ([pl ($symbol-property-list s)] [prev #f])
             (cond
                [(null? pl) (void)]
                [(eq? (car pl) p)
                 (if prev
                     (set-cdr! prev (cdr (cdr pl)))
                     ($set-symbol-property-list! s (cdr (cdr pl))))]
                [else (pt (cdr (cdr pl)) (cdr pl))]))
          ($oops 'remprop "~s is not a symbol" s))))

(define $sgetprop
  (lambda (s p d)
    (unless (symbol? s) ($oops '$sgetprop "~s is not a symbol" s))
    (let gt ([pl ($system-property-list s)])
      (if (null? pl)
          d
          (if (eq? (car pl) p)
              (car (cdr pl))
              (gt (cdr (cdr pl))))))))

(define $sputprop
  (lambda (s p v)
    (unless (symbol? s) ($oops '$sputprop "~s is not a symbol" s))
    (let ((plist ($system-property-list s)))
      (let pt ([pl plist])
        (if (null? pl)
            ($set-system-property-list! s (cons p (cons v plist)))
            (if (eq? (car pl) p)
                (set-car! (cdr pl) v)
                (pt (cdr (cdr pl)))))))))

(define $sremprop
  (lambda (s p)
    (unless (symbol? s) ($oops '$sremprop "~s is not a symbol" s))
    (let rp ([pl ($system-property-list s)] [prev #f])
      (unless (null? pl)
        (if (eq? (car pl) p)
            (if prev
                (set-cdr! prev (cdr (cdr pl)))
                ($set-system-property-list! s (cdr (cdr pl))))
            (rp (cdr (cdr pl)) (cdr pl)))))))
)

(eval-when (compile) (optimize-level 3))

(let ([prefix "g"] [count 0])
  (define generate-unique-name
   ; a-z must come first in alphabet.  separator must not be in alphabet.
    (let ([suffix 0])
      (define unique-id (foreign-procedure "(cs)unique_id" () scheme-object))
      (define (make-session-key)
        (define alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
        (define separator #\-)
        (define b (string-length alphabet))
        (define digit->char (lambda (n) (string-ref alphabet n)))
        (list->string
          (let loop ([n (unique-id)] [a (list separator)])
            (if (< n b)
               ; ensure name starts with letter.  assumes a-z first in alphabet.
                (if (< n 26)
                    (cons (digit->char n) a)
                    (cons* (string-ref alphabet 0) (digit->char n) a))
                (loop (quotient n b) (cons (digit->char (remainder n b)) a))))))
      (define (session-key)
        (or $session-key
          (let ([k (make-session-key)])
            (set! $session-key k)
            (set! suffix -1)
            k)))
      (lambda ()
        (define alphabet "0123456789")
        (define b (string-length alphabet))
        (define digit->char (lambda (n) (string-ref alphabet n)))
        (let* ([k (session-key)] [n (string-length k)])
          (set! suffix (fx+ suffix 1))
          (let f ([i 0])
            (if (fx= i n)
                (let g ([suffix suffix] [n (fx+ n 1)])
                  (if (< suffix b)
                      (let ([s (make-string n)])
                        (string-set! s i (digit->char suffix))
                        s)
                      (let ([s (g (quotient suffix b) (fx+ n 1))])
                        (string-set! s (fx+ i (fx- (string-length s) n))
                          (digit->char (remainder suffix b)))
                        s)))
                (let ([s (f (fx+ i 1))])
                  (string-set! s i (string-ref k i))
                  s)))))))
  (define generate-pretty-name
    (lambda ()
      (let ([count (let ([n count]) (set! count (+ n 1)) n)]
            [prefix prefix])
        (if (and (string? prefix) (fixnum? count))
            (let ([n1 (string-length prefix)])
              (let l1 ([n (fx+ n1 1)] [d 10])
                (if (fx> d count)
                    (let ([s (make-string n)])
                      (let l2 ([i (fx- n1 1)])
                        (unless (fx< i 0)
                          (string-set! s i (string-ref prefix i))
                          (l2 (fx- i 1))))
                      (let l3 ([i (fx- n 1)] [q count])
                        (unless (fx< i n1)
                          (string-set! s i
                            (string-ref "0123456789" (fxremainder q 10)))
                          (l3 (fx- i 1) (fxquotient q 10))))
                      s)
                    (l1 (fx+ n 1) (fx* d 10)))))
            (parameterize ([print-radix 10])
              (format "~a~a" prefix count))))))
  (define $strings->gensym
    (foreign-procedure "(cs)s_strings_to_gensym"
      (scheme-object scheme-object)
      scheme-object))
  (set! $gensym->pretty-name
    (lambda (x)
      (with-tc-mutex
        (cond
          [($symbol-name x) => cdr] ; someone beat us to it
          [else
           (let ([name (generate-pretty-name)])
             ($set-symbol-name! x (cons #f name))
             name)]))))
  (set-who! gensym->unique-string
    (lambda (sym)
      (unless (symbol? sym) ($oops who "~s is not a gensym" sym))
      (let ([name ($symbol-name sym)])
        (or (and (pair? name) (car name)) ; get out quick if name already recorded
          (begin
            (unless (or (not name) (pair? name)) ($oops who "~s is not a gensym" sym))
            (with-tc-mutex
             ; grab name again once safely inside the critical section
              (let ([name ($symbol-name sym)])
                (if (not name)
                    (let ([uname (generate-unique-name)])
                      ($string-set-immutable! uname)
                      ($intern-gensym sym (cons uname (generate-pretty-name)))
                      uname)
                    (or (car name)
                        (let ([uname (generate-unique-name)])
                          ($string-set-immutable! uname)
                          ($intern-gensym sym (cons uname (cdr name)))
                          uname))))))))))
  (set! gensym-prefix
    (case-lambda
      [() prefix]
      [(x) (set! prefix x)]))
  (set! gensym-count
    (case-lambda
      [() count]
      [(x)
       (unless (and (or (fixnum? x) (bignum? x)) (>= x 0))
         ($oops 'gensym-count "~s is not a nonnegative integer" x))
       (set! count x)])) 
  (set-who! gensym
    (case-lambda
      [() (#3%gensym)]
      [(pretty-name)
       (if (immutable-string? pretty-name)
           (#3%$gensym pretty-name)
           (if (string? pretty-name)
               (#3%$gensym (string->immutable-string pretty-name))
               ($oops who "~s is not a string" pretty-name)))]
      [(pretty-name unique-name)
       (unless (string? pretty-name) ($oops who "~s is not a string" pretty-name))
       (unless (string? unique-name) ($oops who "~s is not a string" unique-name))
       ($strings->gensym pretty-name unique-name)]))
  (set-who! $gensym
    (case-lambda
      [() (#3%$gensym)]
      [(pretty-name)
       (unless (immutable-string? pretty-name) ($oops who "~s is not an immutable string" pretty-name))
       (#3%$gensym pretty-name)]
      [(pretty-name unique-name)
       (unless (immutable-string? pretty-name) ($oops who "~s is not an immutable string" pretty-name))
       (unless (immutable-string? unique-name) ($oops who "~s is not an immutable string" unique-name))
       ($strings->gensym pretty-name unique-name)])))
