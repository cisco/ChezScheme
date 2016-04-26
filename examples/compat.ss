;;; compat.ss
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

;;; miscellaneous definitions to make this version compatible
;;; (where possible) with previous versions...and to a small extent with
;;; other versions of scheme and other dialects of lisp as well

;;; use only those items that you need to avoid introducing accidental
;;; dependencies on other items.

(define-syntax define!
  (syntax-rules ()
    ((_ x v) (begin (set! x v) 'x))))

(define-syntax defrec!
  (syntax-rules ()
    ((_ x v) (define! x (rec x v)))))

(define-syntax begin0
  (syntax-rules ()
    ((_ x y ...) (let ((t x)) y ... t))))

(define-syntax recur
  (syntax-rules ()
    ((_ f ((i v) ...) e1 e2 ...)
     (let f ((i v) ...) e1 e2 ...))))

(define-syntax trace-recur
  (syntax-rules ()
    ((_ f ((x v) ...) e1 e2 ...)
     (trace-let f ((x v) ...) e1 e2 ...))))

(define swap-box!
  (lambda (b v)
    (if (box? b)
        (let ((x (unbox b))) (set-box! b v) x)
        (error 'swap-box! "~s is not a box" b))))

(define cull
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'cull "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
         [(pair? l)
          (if (pred? (car l))
              (cons (car l) (f (cdr l)))
              (f (cdr l)))]
         [(null? l) '()]
         [else (error 'cull "~s is not a proper list" ls)]))))

(define cull! cull)

(define mem
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'mem "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
        [(pair? l) (if (pred? (car l)) l (f (cdr l)))]
        [(null? l) #f]
        [else (error 'mem "~s is not a proper list" ls)]))))

(define rem
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'rem "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
        [(pair? l)
         (if (pred? (car l))
             (f (cdr l))
             (cons (car l) (f (cdr l))))]
        [(null? l) '()]
        [else (error 'rem "~s is not a proper list" ls)]))))

(define rem!
  (lambda (pred? ls)
    (unless (procedure? pred?)
      (error 'rem! "~s is not a procedure" pred?))
    (let f ([l ls])
      (cond
        [(pair? l)
         (if (pred? (car l))
             (f (cdr l))
             (begin
               (set-cdr! l (f (cdr l)))
               l))]
        [(null? l) '()]
        [else (error 'rem! "~s is not a proper list" ls)]))))

(define ass
  (lambda (pred? alist)
    (unless (procedure? pred?)
      (error 'ass "~s is not a procedure" pred?))
    (let loop ([l alist])
      (cond
        [(and (pair? l) (pair? (car l)))
         (if (pred? (caar l))
             (car l)
             (loop (cdr l)))]
        [(null? l) #f]
        [else (error 'ass "improperly formed alist ~s" alist)]))))

(define prompt-read
  (lambda (fmt . args)
    (apply printf fmt args)
    (read)))

(define tree-copy
  (rec tree-copy
    (lambda (x)
      (if (pair? x)
          (cons (tree-copy (car x)) (tree-copy (cdr x)))
          x))))

(define ferror error)

(define *most-negative-short-integer* (most-negative-fixnum))
(define *most-positive-short-integer* (most-positive-fixnum))

(define *most-negative-fixnum* (most-negative-fixnum))
(define *most-positive-fixnum* (most-positive-fixnum))

(define *eof* (read-char (open-input-string "")))

(define short-integer? fixnum?)
(define big-integer? bignum?)
(define ratio? ratnum?)
(define float? flonum?)

(define bound? top-level-bound?)
(define global-value top-level-value)
(define set-global-value! set-top-level-value!)
(define define-global-value define-top-level-value)
(define symbol-value top-level-value)
(define set-symbol-value! set-top-level-value!)

(define put putprop)
(define get getprop)

(define copy-list list-copy)
(define copy-tree tree-copy)
(define copy-string string-copy)
(define copy-vector vector-copy)

(define intern string->symbol)
(define symbol-name symbol->string)
(define string->uninterned-symbol gensym)
(define make-temp-symbol string->uninterned-symbol)
(define uninterned-symbol? gensym?)
(define temp-symbol? uninterned-symbol?)

(define compile-eval compile)

(define closure? procedure?)

(define =? =)
(define <? <)
(define >? >)
(define <=? <=)
(define >=? >=)

(define float exact->inexact)
(define rational inexact->exact)

(define char-equal? char=?)
(define char-less? char<?)
(define string-equal? string=?)
(define string-less? string<?)

; following defn conflicts with new r6rs mod
#;(define mod modulo)

(define flush-output flush-output-port)
(define clear-output clear-output-port)
(define clear-input clear-input-port)

(define mapcar map)
(define mapc for-each)
(define true #t)
(define false #f)
(define t #t)
(define nil '())

(define macro-expand expand)

;;; old macro and structure definition

;;; thanks to Michael Lenaghan (MichaelL@frogware.com) for suggesting
;;; various improvements.
(define-syntax define-macro!
  (lambda (x)
    (syntax-case x ()
      [(k (name arg1 ... . args)
          form1
          form2
          ...)
       #'(k name (arg1 ... . args)
           form1
           form2
           ...)]
      [(k (name arg1 arg2 ...)
          form1
          form2
          ...)
       #'(k name (arg1 arg2 ...)
           form1
           form2
           ...)]
      [(k name args . forms)
       (identifier? #'name)
       (letrec ((add-car
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cadr ,@(cdr access)))
                     ((cadr) `(caadr ,@(cdr access)))
                     ((cddr) `(caddr ,@(cdr access)))
                     ((cdddr) `(cadddr ,@(cdr access)))
                     (else `(car ,access)))))
                (add-cdr
                 (lambda (access)
                   (case (car access)
                     ((cdr) `(cddr ,@(cdr access)))
                     ((cadr) `(cdadr ,@(cdr access)))
                     ((cddr) `(cdddr ,@(cdr access)))
                     ((cdddr) `(cddddr ,@(cdr access)))
                     (else `(cdr ,access)))))
                (parse
                 (lambda (l access)
                   (cond
                     ((null? l) '())
                     ((symbol? l) `((,l ,access)))
                     ((pair? l)
                      (append!
                        (parse (car l) (add-car access))
                        (parse (cdr l) (add-cdr access))))
                     (else
                      (syntax-error #'args
                        (format "invalid ~s parameter syntax" (datum k))))))))
         (with-syntax ((proc (datum->syntax-object #'k
                               (let ((g (gensym)))
                                 `(lambda (,g)
                                    (let ,(parse (datum args) `(cdr ,g))
                                      ,@(datum forms)))))))
           #'(define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((k1 . r)
                    (datum->syntax-object #'k1
                      (proc (syntax-object->datum x)))))))))])))

(alias define-macro define-macro!)
(alias defmacro define-macro!)

(define-macro! define-struct! (name . slots)
    `(begin
        (define ,name
            (lambda ,slots
                (vector ',name ,@slots)))
        (define ,(string->symbol (format "~a?" name))
            (lambda (x)
                (and (vector? x)
                    (= (vector-length x) (1+ ,(length slots)))
                    (eq? ',name (vector-ref x 0)))))
         ,@(\#make-accessors name slots)
         ',name))

(define \#make-accessors
    (lambda (name slots)
        (recur f ((n 1) (slots slots))
            (if (not (null? slots))
                (let*
                    ((afn (string->symbol (format "~a-~a" name (car slots))))
                     (sfn (string->symbol (format "~a!" afn))))
                    `((define-macro! ,afn (x) `(vector-ref ,x ,,n))
                      (define-macro! ,sfn (x v) `(vector-set! ,x ,,n ,v))
                      ,@(f (1+ n) (cdr slots))))
                '()))))
