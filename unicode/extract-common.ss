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

(define-syntax assert
  (syntax-rules ()
    [(_ e) (unless e (syntax-error #'e "assertion failed"))]))

(module ((define-table maker accessor mutator))
  (define-syntax maker
    (syntax-rules ()
      [(_ make-inner x t) (make-inner t x)]
      [(_ make-inner x t1 t2 ...)
       (let ([v (make-vector t1)])
         (do ([i 0 (fx+ i 1)])
             ((fx= i t1))
           (vector-set! v i (maker make-inner x t2 ...)))
         v)]))
  (define-syntax accessor
    (syntax-rules ()
      [(_ inner-ref tbl i t) (inner-ref tbl i)]
      [(_ inner-ref tbl i t1 t2 ...)
       (let ([d (* t2 ...)])
         (accessor inner-ref (vector-ref tbl (fxdiv i d))
           (fxmod i d) t2 ...))]))
  (define-syntax mutator
    (syntax-rules ()
      [(_ inner-set! tbl i x t) (inner-set! tbl i x)]
      [(_ inner-set! tbl i x t1 t2 ...)
       (let ([d (* t2 ...)])
         (mutator inner-set! (vector-ref tbl (fxdiv i d))
           (fxmod i d) x t2 ...))]))
  (define-syntax define-table
    (lambda (x)
      (define accessor-code
        (lambda (x)
          (syntax-case x ()
            [(inner-ref tbl i t) #'(inner-ref tbl i)]
            [(inner-ref tbl i t1 t2 ...)
             (with-syntax ([(d) (generate-temporaries '(d))])
               (with-syntax ([body (accessor-code 
                                     #'(inner-ref
                                         (vector-ref tbl (fxdiv i d))
                                         (fxmod i d) t2 ...))])
                 #'(let ([d (* t2 ...)]) body)))])))
      (syntax-case x ()
        [(_ (make-table table-ref table-set! table-ref-code)
            (make-inner inner-ref inner-set!) size dim ...)
         (with-syntax ([(t1 t2 ...) (generate-temporaries #'(size dim ...))]
                       [code (accessor-code 
                               #'(inner-ref tbl i
                                            (/ size (* dim ...))
                                            dim ...))])
           #'(module (make-table table-ref table-set! table-ref-code)
               (define t2 dim) ...
               (define t1 (/ size (* t2 ...)))
               (define make-table (lambda (x) (maker make-inner x t1 t2 ...)))
               (define table-ref-code '(lambda (tbl i) code))
               (define table-ref (lambda (tbl i) (accessor inner-ref tbl i t1 t2 ...)))
               (define table-set! (lambda (tbl i x) (mutator inner-set! tbl i x t1 t2 ...)))))]))))

(define (with-output-to-file* file thunk)
  (when (file-exists? file) (delete-file file))
  (with-output-to-file file thunk))

(define common-equal?
  (lambda (x y)
    (cond
      [(eq? x y) #t]
      [(vector? x)
       (and (vector? y)
         (let ([n (vector-length x)])
           (and (fx= (vector-length y) n)
             (let f ([i 0])
               (or (fx= i n)
                   (and (eq? (vector-ref x i) (vector-ref y i))
                        (f (fx+ i 1))))))))]
      [(pair? x) (and (pair? y) (eq? (car x) (car y)) (eq? (cdr x) (cdr y)))]
      [else (equal? x y)])))

(define cache '())
#;(define commonize ; 5.8s
  (lambda (x)
    (or (find (lambda (y) (common-equal? y x)) cache)
        (begin (set! cache (cons x cache)) x))))
#;(define commonize ; 2.6s
  (let ([cache-table (make-hashtable equal-hash common-equal?)])
    (lambda (x)
      (or (hashtable-ref cache-table x #f)
          (begin
            (set! cache (cons x cache)) ; for sizeof
            (hashtable-set! cache-table x x)
            x)))))
(define commonize ; 1.9s
  (lambda (x)
    (let ([v (find (lambda (y) (common-equal? y x)) cache)])
      (if v
          (begin (set! cache (cons v (remq v cache))) v)
          (begin (set! cache (cons x cache)) x)))))

(define commonize*
  (lambda (x)
    (cond
      [(vector? x)
       (let ([n (vector-length x)])
         (do ([i 0 (fx+ i 1)])
             ((fx= i n))
           (vector-set! x i (commonize* (vector-ref x i)))))
       (commonize x)]
      [(bytevector? x) (commonize x)]
      [(pair? x)
       (set-car! x (commonize* (car x)))
       (set-cdr! x (commonize* (cdr x)))
       (commonize x)]
      [else x])))

(define (sizeof ls) (compute-size ls))

(define (hex->num x) (string->number x 16))
