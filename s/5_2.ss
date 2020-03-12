;;; 5_2.ss
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

;;; list and pair functions

(begin
(define atom?
   (lambda (x)
      (not (pair? x))))

(define list?
   (lambda (x)
      (let loop ([hare x] [tortoise x])
         (if (pair? hare)
             (let ([hare (cdr hare)])
                (if (pair? hare)
                    (and (not (eq? hare tortoise))
                         (loop (cdr hare) (cdr tortoise)))
                    (null? hare)))
             (null? hare)))))

(define null?
   (lambda (x)
      (eq? x '())))

(define caar (lambda (x) (#2%caar x)))
(define cadr (lambda (x) (#2%cadr x)))
(define cdar (lambda (x) (#2%cdar x)))
(define cddr (lambda (x) (#2%cddr x)))
(define caaar (lambda (x) (#2%caaar x)))
(define caadr (lambda (x) (#2%caadr x)))
(define cadar (lambda (x) (#2%cadar x)))
(define caddr (lambda (x) (#2%caddr x)))
(define cdaar (lambda (x) (#2%cdaar x)))
(define cdadr (lambda (x) (#2%cdadr x)))
(define cddar (lambda (x) (#2%cddar x)))
(define cdddr (lambda (x) (#2%cdddr x)))
(define caaaar (lambda (x) (#2%caaaar x)))
(define caaadr (lambda (x) (#2%caaadr x)))
(define caadar (lambda (x) (#2%caadar x)))
(define caaddr (lambda (x) (#2%caaddr x)))
(define cadaar (lambda (x) (#2%cadaar x)))
(define cadadr (lambda (x) (#2%cadadr x)))
(define caddar (lambda (x) (#2%caddar x)))
(define cadddr (lambda (x) (#2%cadddr x))) 
(define cdaaar (lambda (x) (#2%cdaaar x)))
(define cdaadr (lambda (x) (#2%cdaadr x)))
(define cdadar (lambda (x) (#2%cdadar x)))
(define cdaddr (lambda (x) (#2%cdaddr x)))
(define cddaar (lambda (x) (#2%cddaar x)))
(define cddadr (lambda (x) (#2%cddadr x)))
(define cdddar (lambda (x) (#2%cdddar x)))
(define cddddr (lambda (x) (#2%cddddr x)))

(define $list-length)
(define length)
(define list-ref)
(define list-tail)
(define list-head)

(let ()
   (define improper-list-error
      (lambda (who ls)
         ($oops who "~s is not a proper list" ls)))
   (define circular-list-error
      (lambda (who ls)
         ($oops who "~s is circular" ls)))
   (define index-range-error
      (lambda (who ls n)
         ($oops who "index ~s is out of range for list ~s" n ls)))
   (define index-type-error
      (lambda (who n)
         ($oops who "index ~s is not an exact nonnegative integer" n)))
   (define index-range/improper-list-error
      (lambda (who tail ls n)
         (if (null? tail)
             (index-range-error who ls n)
             (improper-list-error who ls))))
   (define list-length
     (lambda (ls who)
       (let loop ([hare ls] [i 0])
         (if (pair? hare)
             (let ([hare (cdr hare)])
               (if (pair? hare)
                   (if (fx<= i 10000)
                       (loop (cdr hare) (fx+ i 2))
                       (let loop ([hare hare] [tortoise hare] [i (fx+ i 1)])
                         (if (pair? hare)
                             (let ([hare (cdr hare)])
                               (if (pair? hare)
                                   (if (eq? hare tortoise)
                                       (circular-list-error who ls)
                                       (loop (cdr hare)
                                             (cdr tortoise)
                                             (fx+ i 2)))
                                   (if (null? hare)
                                       (fx+ i 1)
                                       (improper-list-error who ls))))
                             (if (null? hare)
                                 i
                                 (improper-list-error who ls)))))
                   (if (null? hare)
                       (fx+ i 1)
                       (improper-list-error who ls))))
             (if (null? hare)
                 i
                 (improper-list-error who ls))))))
   (define list-tail-cycle
     (lambda (ls n)
       (let loop ((fast (cdr ls)) (i 1))
         (if (eq? fast ls)
             (let ((i (remainder n i)))
               (do ((ls ls (cdr ls)) (i i (fx- i 1)))
                   ((fx= i 0) ls)))
             (loop (cdr fast) (fx+ i 1))))))
   (define fx-list-tail
     (lambda (fast slow i)
       (if (fx> i 0)
           (if (pair? fast)
               (let ((fast (cdr fast)))
                 (if (fx> i 1)
                     (if (not (eq? fast slow))
                         (if (pair? fast)
                             (fx-list-tail (cdr fast) (cdr slow) (fx- i 2))
                             (values 'error fast i))
                         (values 'cycle fast (fx- i 1)))
                     (values 'okay fast (fx- i 1))))
               (values 'error fast i))
           (values 'okay fast i))))
   (set! $list-length (lambda (ls who) (list-length ls who)))
   (set! length
      (lambda (ls)
         (list-length ls 'length)))
   (set! list-ref
     (lambda (ls n)
       (cond
         [(and (fixnum? n) (fx<= 0 n 1000))
          (let loop ([l ls] [i n])
            (if (pair? l)
                (if (fx> i 1)
                    (let ([l (cdr l)])
                      (if (pair? l)
                          (loop (cdr l) (fx- i 2))
                          (index-range/improper-list-error 'list-ref l ls n)))
                    (if (fx= i 0)
                        (car l)
                        (let ([l (cdr l)])
                          (if (pair? l)
                              (car l)
                              (index-range/improper-list-error 'list-ref l ls n)))))
                (index-range/improper-list-error 'list-ref l ls n)))]
         [(and (or (fixnum? n) (bignum? n)) (>= n 0))
          (let ((m (min n (most-positive-fixnum))))
            (call-with-values
              (lambda () (fx-list-tail ls ls m))
              (lambda (what fast i)
                (cond
                  [(and (eq? what 'okay) (pair? fast))
                   ; can't happen with bignum input
                   (car fast)]
                  [(eq? what 'cycle)
                   (car (list-tail-cycle fast (+ i (- n m))))]
                  [else (index-range/improper-list-error 'list-ref fast ls n)]))))]
         [else (index-type-error 'list-ref n)])))
   (set! list-tail
     (lambda (ls n)
       (cond
         [(and (fixnum? n) (fx<= 0 n 1000))
          (let loop ([l ls] [i n])
            (if (fx> i 1)
                (if (pair? l)
                    (let ([l (cdr l)])
                      (if (pair? l)
                          (loop (cdr l) (fx- i 2))
                          (index-range/improper-list-error 'list-tail l ls n)))
                    (index-range/improper-list-error 'list-tail l ls n))
                (if (fx= i 0)
                    l
                    (if (pair? l)
                        (cdr l)
                        (index-range/improper-list-error 'list-tail l ls n)))))]
         [(and (or (fixnum? n) (bignum? n)) (>= n 0))
          (let ((m (min n (most-positive-fixnum))))
            (call-with-values
              (lambda () (fx-list-tail ls ls m))
              (lambda (what fast i)
                (cond
                  [(eq? what 'okay) ; can't happen with bignum input
                   fast]
                  [(eq? what 'cycle) (list-tail-cycle fast (+ i (- n m)))]
                  [else (index-range/improper-list-error 'list-tail fast ls n)]))))]
         [else (index-type-error 'list-tail n)])))
   (set! list-head
     (lambda (orig-ls orig-n)
       (unless (and (fixnum? orig-n) (fx>= orig-n 0))
         ($oops 'list-head "invalid index ~s" orig-n))
       (let f ([ls orig-ls] [n orig-n])
         (cond
           [(fx<= n 1)
            (if (fx= n 0)
                '()
                (if (pair? ls)
                    (list (car ls))
                    (index-range/improper-list-error 'list-head ls orig-ls orig-n)))]
           [(pair? ls)
            (let ([a (car ls)] [ls (cdr ls)])
              (if (pair? ls)
                  (list* a (car ls) (f (cdr ls) (fx- n 2)))
                  (index-range/improper-list-error 'list-head ls orig-ls orig-n)))]
           [else (index-range/improper-list-error 'list-head ls orig-ls orig-n)]))))
   (set! last-pair
     (lambda (ls)
       (unless (pair? ls)
         ($oops 'last-pair "~s is not a pair" ls))
       (let loop ((fast ls) (slow ls))
         (let ((fast1 (cdr fast)))
           (if (pair? fast1)
               (let ((fast2 (cdr fast1)))
                 (if (pair? fast2)
                     (if (not (eq? fast1 slow))
                         (loop fast2 (cdr slow))
                         (circular-list-error 'last-pair ls))
                     fast1))
               fast))))))

(define make-list
   (rec make-list
      (case-lambda
         [(n) (make-list n (void))]
         [(n x)
          (unless (and (fixnum? n) (fx>= n 0))
             ($oops 'make-list "invalid size ~s" n))
          (let loop ([n n] [ls '()])
             (if (fx= n 0) ls (loop (fx- n 1) (cons x ls))))])))

(define-who list-copy
  (lambda (ls)
    ($list-length ls who)
    (let f ([ls ls])
      (if (null? ls)
          ls
          (cons (car ls) (f (cdr ls)))))))

(define-who append
  (rec append
    (case-lambda
      [() '()]
      [(x1 x2)
       ($list-length x1 who)
       (let f ([ls x1])
         (if (null? ls) x2 (cons (car ls) (f (cdr ls)))))]
      [(x1 . xr)
       (let f ([x1 x1] [xr xr])
         (if (null? xr) x1 (append x1 (f (car xr) (cdr xr)))))])))

(define-who append!
  (let ()
    (define (do-append! x1 x2)
      (if (null? x1)
          x2
          (let f ([ls x1])
            (if (null? (cdr ls))
                (begin (set-cdr! ls x2) x1)
                (f (cdr ls))))))
    (case-lambda
      [() '()]
      [(x1 x2)
       ($list-length x1 who)
       (do-append! x1 x2)]
      [(x1 . xr)
       (let f ([x1 x1] [xr xr])
         (if (null? xr)
             x1
             (begin
               ($list-length x1 who) ; make sure all checks occur before first set-cdr!
               (do-append! x1 (f (car xr) (cdr xr))))))])))

(define-who reverse
  (lambda (ls)
    ($list-length ls who)
    (do ([ls ls (cdr ls)] [a '() (cons (car ls) a)])
        ((null? ls) a))))

(define-who reverse!
  (lambda (ls)
    (#%$list-length ls who)
    (let loop ([l ls] [a '()])
      (cond
        [(pair? l) (let ([x (cdr l)]) (set-cdr! l a) (loop x l))]
        [(null? l) a]
        [else
         (let loop ([l a] [a l])
           (let ([x (cdr l)]) (set-cdr! l a) (loop x l)))]))))

(let ()
(define-syntax do-assoc
  (syntax-rules ()
    ((_ x alist who pred?)
     (let loop ((fast alist) (slow alist))
       (cond
         [(pair? fast)
          (let ((a (car fast)))
            (if (pair? a)
                (if (pred? (car a) x)
                    a
                    (let ((fast (cdr fast)))
                      (cond
                        [(pair? fast)
                         (let ((a (car fast)))
                           (if (pair? a)
                               (if (pred? (car a) x)
                                   a
                                   (if (eq? fast slow)
                                       (cyclic-alist who alist)
                                       (loop (cdr fast) (cdr slow))))
                               (improper-alist who alist)))]
                        [(null? fast) #f]
                        [else (improper-alist who alist)])))
                (improper-alist who alist)))]
         [(null? fast) #f]
         [else (improper-alist who alist)])))))

(define improper-alist
  (lambda (who alist)
    ($oops who "improperly formed alist ~s" alist)))

(define cyclic-alist
  (lambda (who alist)
    ($oops who "cyclic alist ~s" alist)))

(define ass-eq?
  (lambda (x alist who)
    (do-assoc x alist who eq?)))

(set! assq
  (lambda (x alist)
    (ass-eq? x alist 'assq)))

(set! assv
  (lambda (x alist)
    (if (or (symbol? x) (#%$immediate? x))
        (ass-eq? x alist 'assv)
        (do-assoc x alist 'assv eqv?))))

(set! assoc
  (lambda (x alist)
    (cond
      [(string? x)
       (do-assoc x alist 'assoc
         (lambda (x y) (and (string? x) (string=? x y))))]
      [(or (symbol? x) (#%$immediate? x))
       (ass-eq? x alist 'assoc)]
      [else
       (do-assoc x alist 'assoc equal?)])))

(set! assp
  (lambda (pred? alist)
    (unless (procedure? pred?)
      ($oops 'assp "~s is not a procedure" pred?))
    (let loop ((fast alist) (slow alist))
      (cond
        [(pair? fast)
         (let ((a (car fast)))
           (if (pair? a)
               (if (pred? (car a))
                   a
                   (let ((fast (cdr fast)))
                     (cond
                       [(pair? fast)
                        (let ((a (car fast)))
                          (if (pair? a)
                              (if (pred? (car a))
                                  a
                                  (if (eq? fast slow)
                                      (cyclic-alist 'assp alist)
                                      (loop (cdr fast) (cdr slow))))
                              (improper-alist 'assp alist)))]
                       [(null? fast) #f]
                       [else (improper-alist 'assp alist)])))
               (improper-alist 'assp alist)))]
        [(null? fast) #f]
        [else (improper-alist 'assp alist)]))))
)

(let ()
(define improper-list
  (lambda (who ls)
    ($oops who "improper list ~s" ls)))

(define cyclic-list
  (lambda (who ls)
    ($oops who "cyclic list ~s" ls)))

(define-syntax do-member
  (syntax-rules ()
    ((_ x ls who pred?)
     (let loop ((fast ls) (slow ls))
       (cond
         [(pair? fast)
          (if (pred? (car fast) x)
              fast
              (let ((fast (cdr fast)))
                (cond
                  [(pair? fast)
                   (if (pred? (car fast) x)
                       fast
                       (if (eq? fast slow)
                           (cyclic-list who ls)
                           (loop (cdr fast) (cdr slow))))]
                  [(null? fast) #f]
                  [else (improper-list who ls)])))]
         [(null? fast) #f]
         [else (improper-list who ls)])))))

(define mem-eq?
  (lambda (x ls who)
    (do-member x ls who eq?)))

(set! memq
  (lambda (x ls)
    (mem-eq? x ls 'memq)))

(set! memv
  (lambda (x ls)
    (if (or (symbol? x) (fixnum? x) (char? x) (procedure? x))
        (mem-eq? x ls 'memv)
        (do-member x ls 'memv eqv?))))

(set! member
  (lambda (x ls)
    (cond
      [(string? x)
       (do-member x ls 'member
         (lambda (x y) (and (string? x) (string=? x y))))]
      [(or (symbol? x) (fixnum? x) (char? x) (procedure? x))
       (mem-eq? x ls 'member)]
      [else
       (do-member x ls 'member equal?)])))

(set! memp
  (lambda (pred? ls)
    (unless (procedure? pred?)
      ($oops 'memp "~s is not a procedure" pred?))
   (let loop ((fast ls) (slow ls))
     (cond
       [(pair? fast)
        (if (pred? (car fast))
            fast
            (let ((fast (cdr fast)))
              (cond
                [(pair? fast)
                 (if (pred? (car fast))
                     fast
                     (if (eq? fast slow)
                         (cyclic-list 'memp ls)
                         (loop (cdr fast) (cdr slow))))]
                [(null? fast) #f]
                [else (improper-list 'memp ls)])))]
       [(null? fast) #f]
       [else (improper-list 'memp ls)]))))

(set! find
  (lambda (pred? ls)
    (unless (procedure? pred?)
      ($oops 'find "~s is not a procedure" pred?))
   (let loop ((fast ls) (slow ls))
     (cond
       [(pair? fast)
        (if (pred? (car fast))
            (car fast)
            (let ((fast (cdr fast)))
              (cond
                [(pair? fast)
                 (if (pred? (car fast))
                     (car fast)
                     (if (eq? fast slow)
                         (cyclic-list 'find ls)
                         (loop (cdr fast) (cdr slow))))]
                [(null? fast) #f]
                [else (improper-list 'find ls)])))]
       [(null? fast) #f]
       [else (improper-list 'find ls)]))))
)

(let ()
(define improper-list
  (lambda (who ls)
    ($oops who "~s is not a proper list" ls)))

(define-syntax do-remove
  (syntax-rules ()
    ((_ x ls pred?)
     (let f ((x x) (fast ls) (slow ls))
       (if (pair? fast)
           (let ((fast1 (cdr fast)))
             (if (pair? fast1)
                 (and (not (eq? fast1 slow))
                      (let ((fast2 (cdr fast1)))
                        (let ((rest (f x fast2 (cdr slow))))
                          (and rest
                               (if (not (pred? (car fast) x))
                                   (if (not (pred? (car fast1) x))
                                       (if (eq? rest fast2)
                                           fast
                                           (list* (car fast) (car fast1) rest))
                                       (cons (car fast) rest))
                                   (if (not (pred? (car fast1) x))
                                       (if (eq? rest fast2)
                                           fast1
                                           (cons (car fast1) rest))
                                       rest))))))
                 (and (null? fast1)
                      (if (not (pred? (car fast) x))
                          fast
                          '()))))
           (and (null? fast) '()))))))

(define rem-eq?
  (lambda (x l)
    (do-remove x l eq?)))

(set! remq
  (lambda (x ls)
    (or (rem-eq? x ls)
        (improper-list 'remq ls))))

(set! remv
  (lambda (x ls)
    (or (if (or (symbol? x) (fixnum? x) (char? x) (procedure? x))
            (rem-eq? x ls)
            (do-remove x ls eqv?))
        (improper-list 'remv ls))))

(set! remove
  (lambda (x ls)
    (or (cond
          [(string? x)
           (do-remove x ls 
             (lambda (x y) (and (string? x) (string=? x y))))]
          [(or (symbol? x) (fixnum? x) (char? x) (procedure? x))
           (rem-eq? x ls)]
          [else
           (do-remove x ls equal?)])
        (improper-list 'remove ls))))

(set! remp
  (lambda (pred? ls)
    (unless (procedure? pred?)
      ($oops 'remp "~s is not a procedure" pred?))
    (or (let f ((pred? pred?) (fast ls) (slow ls))
          (if (pair? fast)
              (let ((fast1 (cdr fast)))
                (if (pair? fast1)
                    (and (not (eq? fast1 slow))
                         (let ((fast2 (cdr fast1)))
                           (let ((rest (f pred? fast2 (cdr slow))))
                             (and rest
                                  (if (not (pred? (car fast)))
                                      (if (not (pred? (car fast1)))
                                          (if (eq? rest fast2)
                                              fast
                                              (list* (car fast) (car fast1) rest))
                                          (cons (car fast) rest))
                                      (if (not (pred? (car fast1)))
                                          (if (eq? rest fast2)
                                              fast1
                                              (cons (car fast1) rest))
                                          rest))))))
                    (and (null? fast1)
                         (if (not (pred? (car fast)))
                             fast
                             '()))))
              (and (null? fast) '())))
        (improper-list 'remp ls))))


(set! filter
  (lambda (pred? ls)
    (unless (procedure? pred?)
      ($oops 'filter "~s is not a procedure" pred?))
    (or (let f ((pred? pred?) (fast ls) (slow ls))
          (if (pair? fast)
              (let ((fast1 (cdr fast)))
                (if (pair? fast1)
                    (and (not (eq? fast1 slow))
                         (let ((fast2 (cdr fast1)))
                           (let ((rest (f pred? fast2 (cdr slow))))
                             (and rest
                                  (if (pred? (car fast))
                                      (if (pred? (car fast1))
                                          (if (eq? rest fast2)
                                              fast
                                              (list* (car fast) (car fast1) rest))
                                          (cons (car fast) rest))
                                      (if (pred? (car fast1))
                                          (if (eq? rest fast2)
                                              fast1
                                              (cons (car fast1) rest))
                                          rest))))))
                    (and (null? fast1)
                         (if (pred? (car fast))
                             fast
                             '()))))
              (and (null? fast) '())))
        (improper-list 'filter ls))))

(set! partition
  (lambda (pred? ls)
    (unless (procedure? pred?)
      ($oops 'partition "~s is not a procedure" pred?))
    (let f ([pred? pred?] [fast ls] [slow ls] [ls ls])
      (if (pair? fast)
          (let ([fast1 (cdr fast)])
            (if (pair? fast1)
                (if (eq? fast1 slow)
                    (improper-list 'partition ls)
                    (let ([fast2 (cdr fast1)])
                      (let-values ([(ins outs) (f pred? fast2 (cdr slow) ls)])
                        (if (pred? (car fast))
                            (if (pred? (car fast1))
                                (values
                                  (if (eq? ins fast2)
                                      fast
                                      (list* (car fast) (car fast1) ins))
                                  outs)
                                (values
                                  (cons (car fast) ins)
                                  (if (eq? outs fast2)
                                      fast1
                                      (cons (car fast1) outs))))
                            (if (pred? (car fast1))
                                (values
                                  (if (eq? ins fast2)
                                      fast1
                                      (cons (car fast1) ins))
                                  (cons (car fast) outs))
                                (values
                                  ins
                                  (if (eq? outs fast2)
                                      fast
                                      (list* (car fast) (car fast1) outs))))))))
                (if (null? fast1)
                    (if (pred? (car fast))
                        (values fast '())
                        (values '() fast))
                    (improper-list 'partition ls))))
          (if (null? fast)
              (values '() '())
              (improper-list 'partition ls))))))
)

(let ()
(define-syntax do-rem!
  (syntax-rules ()
    ((_ pred?)
     (rec rem!
       (lambda (x ls)
         (if (not (null? ls))
             (if (not (pred? (car ls) x))
                 (begin
                   (let loop ((ls (cdr ls)) (prev ls))
                     (unless (null? ls)
                       (if (not (pred? (car ls) x))
                           (loop (cdr ls) ls)
                           (set-cdr! prev (rem! x (cdr ls))))))
                   ls)
                 (rem! x (cdr ls)))
             '()))))))

(define rem-eq?! (do-rem! eq?))

(set! remq!
  (lambda (x ls)
    ($list-length ls 'remq!)
    (rem-eq?! x ls)))

(set! remv!
  (lambda (x ls)
    ($list-length ls 'remv!)
    (if (or (symbol? x) (fixnum? x) (char? x) (procedure? x))
        (rem-eq?! x ls)
        ((do-rem! eqv?) x ls))))

(set! remove!
  (lambda (x ls)
    ($list-length ls 'remove!)
    (if (or (symbol? x) (fixnum? x) (char? x) (procedure? x))
        (rem-eq?! x ls)
        ((do-rem! equal?) x ls))))
)

(define substq
   (lambda (new old tree)
      (let f ([tree tree])
         (if (eq? old tree)
             new
             (if (pair? tree)
                 (let ([a (f (car tree))] [d (f (cdr tree))])
                    (if (and (eq? a (car tree)) (eq? d (cdr tree)))
                        tree
                        (cons a d)))
                 tree)))))

(define substq!
   (lambda (new old tree)
      (let f ([tree tree])
         (if (eq? old tree)
             new
             (if (pair? tree)
                 (begin
                    (set-car! tree (f (car tree)))
                    (set-cdr! tree (f (cdr tree)))
                    tree)
                 tree)))))

(define substv
   (lambda (new old tree)
      (let f ([tree tree])
         (if (eqv? old tree)
             new
             (if (pair? tree)
                 (let ([a (f (car tree))] [d (f (cdr tree))])
                    (if (and (eq? a (car tree)) (eq? d (cdr tree)))
                        tree
                        (cons a d)))
                 tree)))))

(define substv!
   (lambda (new old tree)
      (let f ([tree tree])
         (if (eqv? old tree)
             new
             (if (pair? tree)
                 (begin
                    (set-car! tree (f (car tree)))
                    (set-cdr! tree (f (cdr tree)))
                    tree)
                 tree)))))

(define subst
   (lambda (new old tree)
      (let f ([tree tree])
         (if (equal? old tree)
             new
             (if (pair? tree)
                 (let ([a (f (car tree))] [d (f (cdr tree))])
                    (if (and (eq? a (car tree)) (eq? d (cdr tree)))
                        tree
                        (cons a d)))
                 tree)))))

(define subst!
   (lambda (new old tree)
      (let f ([tree tree])
         (if (equal? old tree)
             new
             (if (pair? tree)
                 (begin
                    (set-car! tree (f (car tree)))
                    (set-cdr! tree (f (cdr tree)))
                    tree)
                 tree)))))

(let ()
  (define ($iota n ls)
    (if (fx> n 0)
        ($iota (fx- n 2) (list* (fx- n 1) n ls))
        (if (fx= n 0)
            (cons 0 ls)
            ls)))

 ; (iota n) => (0 1 ... n-1)
  (set! iota
    (lambda (n)
      (unless (and (fixnum? n) (fx>= n 0))
        ($oops 'iota "~s is not a nonnegative fixnum" n))
      ($iota (fx- n 1) '())))

 ; (enumerate '(a1 a2 ... aN)) => (0 1 ... n-1)
  (set! enumerate
    (lambda (ls)
      ($iota (fx- ($list-length ls 'enumerate) 1) '()))))
)
