;;; hash.ms
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

(mat old-hash-table
  (error? (get-hash-table '((a . b)) 'a #f))
  (error? (put-hash-table! (list (cons 'a 'b)) 'a 'b))
  (error? (remove-hash-table! (list (cons 'a 'b)) 'a))
  (error? (hash-table-map '((a . b)) cons))
  (error? (hash-table-for-each '((a . b)) cons))
  (begin
    (define $h-ht (make-hash-table))
    (hash-table? $h-ht))
  (not (hash-table? 3))
  (not (hash-table? '$h-ht))
  (null? (hash-table-map $h-ht list))
  (eq? (let ([n 0])
         (hash-table-for-each $h-ht (lambda (x y) (set! n (+ n 1))))
         n)
       0)
  (equal?
    (begin
      (put-hash-table! $h-ht 'ham 'spam)
      (hash-table-map $h-ht list))
    '((ham spam)))
  (error? ; wrong number of args
    (hash-table-map $h-ht (lambda (x) x)))
  (error? ; wrong number of args
    (hash-table-for-each $h-ht (lambda (x) x)))
  ((lambda (x y) (or (equal? x y) (equal? x (reverse y))))
    (begin
      (put-hash-table! $h-ht 'cram 'sham)
      (hash-table-map $h-ht list))
    '((ham spam) (cram sham)))
  ((lambda (x y) (or (equal? x y) (equal? x (reverse y))))
    (begin
      (put-hash-table! $h-ht 'ham 'jam)
      (hash-table-map $h-ht list))
    '((ham jam) (cram sham)))
  (eq? (get-hash-table $h-ht 'ham #f) 'jam)
  (eq? (get-hash-table $h-ht 'cram #f) 'sham)
  (eq? (get-hash-table $h-ht 'sham #f) #f)
  (equal? (get-hash-table $h-ht 'jam "rats") "rats")
  (eq? (let ([n 0])
         (hash-table-for-each $h-ht (lambda (x y) (set! n (+ n 1))))
         n)
       2)
  ((lambda (x y) (or (equal? x y) (equal? x (reverse y))))
   (let ([keys '()] [vals '()])
     (hash-table-for-each $h-ht
       (lambda (k v)
         (set! keys (cons k keys))
         (set! vals (cons v vals))))
     (map cons vals keys))
   '((jam . ham) (sham . cram)))
  (eq? (collect (collect-maximum-generation)) (void))
  ((lambda (x y) (or (equal? x y) (equal? x (reverse y))))
   (let ([keys '()] [vals '()])
     (hash-table-for-each $h-ht
       (lambda (k v)
         (set! keys (cons k keys))
         (set! vals (cons v vals))))
     (map cons vals keys))
   '((jam . ham) (sham . cram)))
  (eq? (begin
         (remove-hash-table! $h-ht 'ham)
         (get-hash-table $h-ht 'ham 'gone!))
       'gone!)
  (equal?
    (hash-table-map $h-ht list)
    '((cram sham)))
  (eq? (collect (collect-maximum-generation)) (void))
  (equal?
    (hash-table-map $h-ht list)
    '((cram sham)))
  (eq? (begin
         (remove-hash-table! $h-ht 'ham)
         (get-hash-table $h-ht 'ham 'gone!))
       'gone!)
  (equal?
    (hash-table-map $h-ht list)
    '((cram sham)))
  (eq? (begin
         (remove-hash-table! $h-ht 'sham)
         (get-hash-table $h-ht 'ham 'never-there!))
       'never-there!)
  (equal?
    (hash-table-map $h-ht list)
    '((cram sham)))
  (eq? (begin
         (remove-hash-table! $h-ht 'cram)
         (get-hash-table $h-ht 'cram 'gone-too!))
       'gone-too!)
  (null? (hash-table-map $h-ht list))

 ; fasling out eq hash tables
  (equal?
    (let ([x (cons 'y '!)])
      (define ht (make-hash-table))
      (put-hash-table! ht x 'because)
      (put-hash-table! ht 'foo "foo")
      (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
        (fasl-write (list x ht) p)
        (close-port p))
      (let-values ([(x2 ht2)
                    (apply values
                      (call-with-port
                        (open-file-input-port "testfile.ss")
                        fasl-read))])
        (list
          (get-hash-table ht2 x2 #f)
          (get-hash-table ht2 'foo #f))))
    '(because "foo"))

 ; weak hash table tests
  (begin
    (define $h-ht (make-hash-table #t))
    (hash-table? $h-ht))
  (null?
    (begin
      (put-hash-table! $h-ht (string #\a) 'yea!)
      (collect (collect-maximum-generation))
      (hash-table-map $h-ht cons)))
  (eq? (let ([n 0])
         (hash-table-for-each $h-ht (lambda (x y) (set! n (+ n 1))))
         n)
       0)
  (let ([s (string #\a)])
    (put-hash-table! $h-ht s 666)
    (equal? (get-hash-table $h-ht s #f) 666))
  (null?
    (begin
      (collect (collect-maximum-generation))
      (hash-table-map $h-ht cons)))

 ; make sure that nonweak hash tables are nonweak (explicit #f arg)
  (begin
    (define $h-ht (make-hash-table #f))
    (hash-table? $h-ht))
  (equal?
    (begin
      (put-hash-table! $h-ht (string #\a) "bc")
      (collect (collect-maximum-generation))
      (hash-table-map $h-ht string-append))
    '("abc"))

 ; make sure that nonweak hash tables are nonweak (implicit #f arg)
  (begin
    (define $h-ht (make-hash-table))
    (hash-table? $h-ht))
  (equal?
    (begin
      (put-hash-table! $h-ht (string #\a) "bc")
      (collect (collect-maximum-generation))
      (hash-table-map $h-ht string-append))
    '("abc"))

 ; stress tests
  (let () ; nonweak
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-hash-table))
    (let* ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (= (length (hash-table-map ht (lambda (x y) x)))
                    (- n (length drop)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (get-hash-table ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eq? (get-hash-table ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (put-hash-table! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (remove-hash-table! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (remove-hash-table! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))

  (let () ; weak
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-hash-table #t))
    (let* ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (<= (length (hash-table-map ht (lambda (x y) x)))
                     (- n (length drop)))
                 (begin
                   (collect (collect-maximum-generation))
                   (= (length (hash-table-map ht (lambda (x y) x)))
                      (length keep)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (get-hash-table ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eq? (get-hash-table ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (put-hash-table! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (remove-hash-table! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (remove-hash-table! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))
)

(mat tlc
  (critical-section
    (let ()
      (define ht (make-eq-hashtable))
      (define keyval '(a . b))
      (define next 0)
      (define tlc (#%$make-tlc ht keyval next))
      (define tlc2 (#%$make-tlc ht keyval next))
      (and
        (#%$tlc? tlc)
        (not (#%$tlc? keyval))
        (eq? (#%$tlc-ht tlc) ht)
        (eq? (#%$tlc-keyval tlc) keyval)
        (eqv? (#%$tlc-next tlc) next)
        (begin
          (#%$set-tlc-next! tlc tlc2)
          (eq? (#%$tlc-next tlc) tlc2)))))
)

(define $vector-andmap
  (lambda (p . v*)
    (apply andmap p (map vector->list v*))))

(define $vector-append
  (lambda v*
    (list->vector (apply append (map vector->list v*)))))

(define $vector-member?
  (lambda (x v)
    (let ([n (vector-length v)])
      (let f ([i 0])
        (and (not (fx= i n))
          (or (equal? (vector-ref v i) x)
              (f (fx+ i 1))))))))

(define same-elements?
  (lambda (v1 v2)
    (let ([n (vector-length v1)])
      (define (each-in? v1 v2)
        (let f ([i 0])
          (or (fx= i n)
              (and ($vector-member? (vector-ref v1 i) v2)
                   (f (fx+ i 1))))))
      (and (fx= (vector-length v2) n)
           (each-in? v1 v2)
           (each-in? v2 v1)))))

(define equal-entries?
  (lambda (ht keys vals)
    (define-syntax same-entries?
      (syntax-rules ()
        [(_ e1 keys2 vals2)
         (let-values ([(keys1 vals1) e1])
           (and
            (same-elements? keys1 keys2)
            (same-elements? vals1 vals2)))]))

    (and
     (same-elements? (hashtable-keys ht) keys)
     (same-elements? (hashtable-values ht) vals)
     (same-entries? (hashtable-entries ht) keys vals)
     (or (not (hashtable-mutable? ht))
         (same-elements? (hashtable-cells ht) (vector-map cons keys vals)))
     (same-elements? (#%$hashtable-cells ht) (vector-map cons keys vals))

     (same-elements? (r6rs:hashtable-keys ht) keys)
     (same-entries? (r6rs:hashtable-entries ht) keys vals)

     ;; Check requested sizes > hash table size
     (andmap (lambda (size)
               (and
                (same-elements? (hashtable-keys ht size) keys)
                (same-elements? (hashtable-values ht size) vals)
                (same-entries? (hashtable-entries ht size) keys vals)
                (or (not (hashtable-mutable? ht))
                    (same-elements? (hashtable-cells ht size) (vector-map cons keys vals)))))
             (list (add1 (hashtable-size ht))
                   (expt 2 1000)))

     ;; Make sure request of 0 always works:
     (same-elements? (hashtable-keys ht 0) '#())
     (same-elements? (hashtable-values ht 0) '#())
     (same-entries? (hashtable-entries ht 0) '#() '#())
     (or (not (hashtable-mutable? ht))
         (same-elements? (hashtable-cells ht 0) '#()))
     (same-elements? (#%$hashtable-cells ht 0) '#())

     (or
      (< (hashtable-size ht) 2)
      ;; Check request of size 2:
      (let ([twos (lambda (v)
                     (let i-loop ([i 0])
                       (cond
                        [(= i (vector-length v))
                         '()]
                        [else
                         (let j-loop ([j (add1 i)])
                           (cond
                            [(= j (vector-length v))
                             (i-loop (add1 i))]
                            [else
                             (cons (vector (vector-ref v i) (vector-ref v j))
                                   (j-loop (add1 j)))]))])))])
        (let ([keyss (twos keys)]
              [valss (twos vals)])
          (and
           (let ([got-keys (hashtable-keys ht 2)])
             (ormap (lambda (keys)
                      (same-elements? got-keys keys))
                    keyss))
           (let ([got-vals (hashtable-values ht 2)])
             (ormap (lambda (vals)
                      (same-elements? got-vals vals))
                    valss))
           (let-values ([(got-keys got-vals) (hashtable-entries ht 2)])
             (ormap (lambda (keys vals)
                      (and (same-elements? got-keys keys)
                           (same-elements? got-vals vals)))
                    keyss valss))
           (or (not (hashtable-mutable? ht))
               (let ([got-cells (hashtable-cells ht 2)])
                 (ormap (lambda (keys vals)
                          (same-elements? got-cells (vector-map cons keys vals)))
                        keyss valss)))
           (let ([got-cells (#%$hashtable-cells ht 2)])
             (ormap (lambda (keys vals)
                      (same-elements? got-cells (vector-map cons keys vals)))
                    keyss valss)))))))))

(mat hashtable-arguments
 ; make-eq-hashtable
  (error? ; wrong argument count
    (make-eq-hashtable 3 #t))
  (error? ; invalid size
    (make-eq-hashtable -1))
  (error? ; invalid size
    (make-eq-hashtable #t))
  (error? ; invalid size
    (make-eq-hashtable #f))
 ; make-hashtable
  (error? ; wrong argument count
    (make-hashtable))
  (error? ; wrong argument count
    (make-hashtable equal-hash))
  (error? ; wrong argument count
    (make-hashtable equal-hash equal? 45 53))
  (error? ; not a procedure
    (make-hashtable 'a equal? 45))
  (error? ; not a procedure
    (make-hashtable equal-hash 'a 45))
  (error? ; invalid size
    (make-hashtable equal-hash equal? 'a))
  (error? ; invalid size
    (make-hashtable equal-hash equal? -45))
  (error? ; invalid size
    (make-hashtable equal-hash equal? 45.0))
 ; make-eqv-hashtable
  (error? ; wrong argument count
    (make-eqv-hashtable 3 #t))
  (error? ; invalid size
    (make-eqv-hashtable -1))
  (error? ; invalid size
    (make-eqv-hashtable #t))
  (error? ; invalid size
    (make-eqv-hashtable #f))
  (begin
    (define $ht (make-eq-hashtable))
    (define $imht (hashtable-copy $ht))
    (define $ht2 (make-eq-hashtable 50))
    (and (hashtable? $ht)
         (eq-hashtable? $ht)
         (hashtable-mutable? $ht)
         (not (hashtable-weak? $ht))
         (not (eq-hashtable-weak? $ht))
         (not (hashtable-ephemeron? $ht))
         (not (eq-hashtable-ephemeron? $ht))
         (hashtable? $imht)
         (eq-hashtable? $imht)
         (not (hashtable-mutable? $imht))
         (not (hashtable-weak? $imht))
         (not (eq-hashtable-weak? $imht))
         (not (hashtable-ephemeron? $imht))
         (not (eq-hashtable-ephemeron? $imht))
         (hashtable? $ht2)
         (eq-hashtable? $ht2)
         (hashtable-mutable? $ht2)
         (not (hashtable-weak? $ht2))
         (not (eq-hashtable-weak? $ht2))
         (not (hashtable-ephemeron? $ht2))
         (not (eq-hashtable-ephemeron? $ht2))))
  (not (hashtable? 3))
  (not (hashtable? (make-vector 3)))
  (not (eq-hashtable? 3))
  (not (eq-hashtable? (make-vector 3)))
 ; hashtable?
  (error? ; wrong argument count
    (hashtable?))
  (error? ; wrong argument count
    (hashtable? $ht 3))
  (error? ; wrong argument count
    (eq-hashtable?))
  (error? ; wrong argument count
    (eq-hashtable? $ht 3))
 ; hashtable-mutable?
  (error? ; not a hashtable
    (hashtable-mutable? (make-vector 3)))
  (error? ; wrong argument count
    (hashtable-mutable?))
  (error? ; wrong argument count
    (hashtable-mutable? $ht 3))
 ; hashtable-size
  (error? ; wrong argument count
    (hashtable-size))
  (error? ; wrong argument count
    (hashtable-size $ht 3))
  (error? ; not a hashtable
    (hashtable-size 'hello))
 ; hashtable-ref
  (error? ; wrong argument count
    (hashtable-ref))
  (error? ; wrong argument count
    (hashtable-ref $ht))
  (error? ; wrong argument count
    (hashtable-ref $ht 'a))
  (error? ; wrong argument count
    (hashtable-ref $ht 'a 'b 'c))
  (error? ; not a hashtable
    (hashtable-ref '(hash . table) 'a 'b))
 ; hashtable-contains?
  (error? ; wrong argument count
    (hashtable-contains?))
  (error? ; wrong argument count
    (hashtable-contains? $ht))
  (error? ; wrong argument count
    (hashtable-contains? $ht 'a 'b))
  (error? ; not a hashtable
    (hashtable-contains? '(hash . table) 'a))
 ; hashtable-set!
  (error? ; wrong argument count
    (hashtable-set!))
  (error? ; wrong argument count
    (hashtable-set! $ht))
  (error? ; wrong argument count
    (hashtable-set! $ht 'a))
  (error? ; wrong argument count
    (hashtable-set! $ht 'a 'b 'c))
  (error? ; not a hashtable
    (hashtable-set! '(hash . table) 'a 'b))
  (error? ; hashtable not mutable
    (hashtable-set! $imht 'a 'b))
 ; hashtable-update!
  (error? ; wrong argument count
    (hashtable-update!))
  (error? ; wrong argument count
    (hashtable-update! $ht))
  (error? ; wrong argument count
    (hashtable-update! $ht 'a values))
  (error? ; wrong argument count
    (hashtable-update! $ht 'a values 'c 'd))
  (error? ; not a hashtable
    (hashtable-update! '(hash . table) 'a values 'b))
  (error? ; hashtable not mutable
    (hashtable-update! $imht 'a values 'b))
  (error? ; not a procedure
    (hashtable-update! $ht 'a "not a procedure" 'b))
 ; hashtable-cell
  (error? ; wrong argument count
    (hashtable-cell))
  (error? ; wrong argument count
    (hashtable-cell $ht))
  (error? ; wrong argument count
    (hashtable-cell $ht 'a))
  (error? ; wrong argument count
    (hashtable-cell $ht 'a 'b 'c))
  (error? ; not a hashtable
    (hashtable-cell '(hash . table) 'a 'b))
  (error? ; immutable hash table
    (hashtable-cell (hashtable-copy $ht #f) 'a 'b))
 ; hashtable-ref-cell
  (error? ; wrong argument count
    (hashtable-ref-cell))
  (error? ; wrong argument count
   (hashtable-ref-cell $ht))
  (error? ; wrong argument count
    (hashtable-ref-cell $ht 'a 'b))
  (error? ; not a hashtable
    (hashtable-ref-cell '(hash . table) 'a))
  (error? ; immutable hash table
    (hashtable-ref-cell (hashtable-copy $ht #f) 'a))
 ; hashtable-delete!
  (error? ; wrong argument count
    (hashtable-delete!))
  (error? ; wrong argument count
    (hashtable-delete! $ht))
  (error? ; wrong argument count
    (hashtable-delete! $ht 'a 'b))
  (error? ; not a hashtable
    (hashtable-delete! '(hash . table) 'a))
  (error? ; hashtable not mutable
    (hashtable-delete! $imht 'a))
 ; hashtable-copy
  (error? ; wrong argument count
    (hashtable-copy))
  (error? ; wrong argument count
    (hashtable-copy $ht #t 17))
  (error? ; not a hashtable
    (hashtable-copy '(hash . table) #t))
 ; hashtable-clear!
  (error? ; wrong argument count
    (hashtable-clear!))
  (error? ; wrong argument count
    (hashtable-clear! $ht 17 'foo))
  (error? ; not a hashtable
    (hashtable-clear! '(hash . table)))
  (error? ; not a hashtable
    (hashtable-clear! '(hash . table) 17))
  (error? ; hashtable not mutable
    (hashtable-clear! $imht))
  (error? ; hashtable not mutable
    (hashtable-clear! $imht 32))
  (error? ; invalid size
    (hashtable-clear! $ht #t))
 ; hashtable-keys
  (error? ; wrong argument count
    (hashtable-keys))
  (error? ; wrong argument count
    (hashtable-keys $ht 72 43))
  (error? ; not a hashtable
    (hashtable-keys '(hash . table)))
  (error? ; bad size
    (hashtable-keys $ht -79))
  (error? ; bad size
    (hashtable-keys $ht 'not-an-unsigned-integer))
  (error? ; wrong argument count
    (r6rs:hashtable-keys))
  (error? ; wrong argument count
    (r6rs:hashtable-keys $ht 72))
  (error? ; not a hashtable
    (r6rs:hashtable-keys '(hash . table)))
 ; hashtable-values
  (error? ; wrong argument count
    (hashtable-values))
  (error? ; wrong argument count
    (hashtable-values $ht 72 43))
  (error? ; not a hashtable
    (hashtable-values '(hash . table)))
  (error? ; bad size
    (hashtable-values $ht -79))
  (error? ; bad size
    (hashtable-values $ht 'not-an-unsigned-integer))
 ; hashtable-entries
  (error? ; wrong argument count
    (hashtable-entries))
  (error? ; wrong argument count
    (hashtable-entries $ht 72 43))
  (error? ; not a hashtable
    (hashtable-entries '(hash . table)))
  (error? ; bad size
    (hashtable-entries $ht -79))
  (error? ; bad size
    (hashtable-entries $ht 'not-an-unsigned-integer))
  (error? ; wrong argument count
    (r6rs:hashtable-entries))
  (error? ; wrong argument count
    (r6rs:hashtable-entries $ht 72))
  (error? ; not a hashtable
    (r6rs:hashtable-entries '(hash . table)))
 ; hashtable-cells
  (error? ; wrong argument count
    (hashtable-cells))
  (error? ; wrong argument count
    (hashtable-cells $ht 72 43))
  (error? ; not a hashtable
    (hashtable-cells 5))
  (error? ; not a hashtable
    (hashtable-cells '(hash . table)))
  (error? ; bad size
    (hashtable-cells $ht -79))
  (error? ; bad size
    (hashtable-cells $ht 'not-an-unsigned-integer))
  (error? ; immutable hash table
    (hashtable-cells (hashtable-copy $ht #f)))
  (error? ; immutable hash table
    (hashtable-cells (hashtable-copy $ht #f) 5))
 ; hashtable-hash-function
  (error? ; wrong argument count
    (hashtable-hash-function))
  (error? ; wrong argument count
    (hashtable-hash-function $ht $ht))
  (error? ; not a hsshtable
    (hashtable-hash-function '(hash . table)))
 ; hashtable-equivalence-function
  (error? ; wrong argument count
    (hashtable-equivalence-function))
  (error? ; wrong argument count
    (hashtable-equivalence-function $ht $ht))
  (error? ; not a hsshtable
    (hashtable-equivalence-function '(hash . table)))
 ; hashtable-weak?
  (error? ; wrong argument count
    (hashtable-weak?))
  (error? ; wrong argument count
    (hashtable-weak? $ht 3))
  (error? ; not a hashtable
    (hashtable-weak? '(hash . table)))
 ; hashtable-ephemeron?
  (error? ; wrong argument count
    (hashtable-ephemeron?))
  (error? ; wrong argument count
    (hashtable-ephemeron? $ht 3))
  (error? ; not a hashtable
    (hashtable-ephemeron? '(hash . table)))
)

(mat hash-return-value
  ; hashtable-ref
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) "oops") equal?)])
      (hashtable-ref ht 'any #f)))
  #;(error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) -7) equal?)])
      (hashtable-ref ht 'any #f)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 3.5) equal?)])
      (hashtable-ref ht 'any #f)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 1+2i) equal?)])
      (hashtable-ref ht 'any #f)))
  ; hashtable-contains?
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) "oops") equal?)])
      (hashtable-contains? ht 'any)))
  #;(error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) -7) equal?)])
      (hashtable-contains? ht 'any)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 3.5) equal?)])
      (hashtable-contains? ht 'any)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 1+2i) equal?)])
      (hashtable-contains? ht 'any)))
  ; hashtable-set!
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) "oops") equal?)])
      (hashtable-set! ht 'any 'spam)))
  #;(error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) -7) equal?)])
      (hashtable-set! ht 'any 'spam)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 3.5) equal?)])
      (hashtable-set! ht 'any 'spam)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 1+2i) equal?)])
      (hashtable-set! ht 'any 'spam)))
  ; hashtable-update!
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) "oops") equal?)])
      (hashtable-update! ht 'any values 'spam)))
  #;(error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) -7) equal?)])
      (hashtable-update! ht 'any values 'spam)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 3.5) equal?)])
      (hashtable-update! ht 'any values 'spam)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 1+2i) equal?)])
      (hashtable-update! ht 'any values 'spam)))
  ; hashtable-cell
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) "oops") equal?)])
      (hashtable-cell ht 'any 0)))
  #;(error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) -7) equal?)])
      (hashtable-cell ht 'any 0)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 3.5) equal?)])
      (hashtable-cell ht 'any 0)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 1+2i) equal?)])
      (hashtable-cell ht 'any 0)))
  ; hashtable-ref-cell
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) "oops") equal?)])
      (hashtable-ref-cell ht 'any)))
  #;(error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) -7) equal?)])
      (hashtable-ref-cell ht 'any)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 3.5) equal?)])
      (hashtable-ref-cell ht 'any)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 1+2i) equal?)])
      (hashtable-ref-cell ht 'any)))
  ; hashtable-delete!
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) "oops") equal?)])
      (hashtable-delete! ht 'any)))
  #;(error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) -7) equal?)])
      (hashtable-delete! ht 'any)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 3.5) equal?)])
      (hashtable-delete! ht 'any)))
  (error? ; invalid hash-function return value
    (let ([ht (make-hashtable (lambda (x) 1+2i) equal?)])
      (hashtable-delete! ht 'any)))
)

(mat eq-hashtable-arguments
 ; make-weak-eq-hashtable
  (error? ; wrong argument count
    (make-weak-eq-hashtable 3 #t))
  (error? ; invalid size
    (make-weak-eq-hashtable -1))
  (error? ; invalid size
    (make-weak-eq-hashtable #t))
  (error? ; invalid size
    (make-weak-eq-hashtable #f))
 ; make-weak-eq-hashtable
  (error? ; wrong argument count
    (make-ephemeron-eq-hashtable 3 #t))
  (error? ; invalid size
    (make-ephemeron-eq-hashtable -1))
  (error? ; invalid size
    (make-ephemeron-eq-hashtable #t))
  (error? ; invalid size
    (make-ephemeron-eq-hashtable #f))
  (begin
    (define $wht (make-weak-eq-hashtable 50))
    (define $eht (make-ephemeron-eq-hashtable 50))
    (define $imht (hashtable-copy $wht))
    (define $imeht (hashtable-copy $eht))
    (define $wht2 (make-weak-eq-hashtable))
    (define $eht2 (make-ephemeron-eq-hashtable))
    (and (hashtable? $wht)
         (hashtable? $eht)
         (eq-hashtable? $wht)
         (eq-hashtable? $eht)
         (hashtable-weak? $wht)
         (not (hashtable-ephemeron? $wht))
         (hashtable-ephemeron? $eht)
         (not (hashtable-weak? $eht))
         (eq-hashtable-weak? $wht)
         (not (eq-hashtable-ephemeron? $wht))
         (eq-hashtable-ephemeron? $eht)
         (not (eq-hashtable-weak? $eht))
         (hashtable-mutable? $wht)
         (hashtable-mutable? $eht)
         (hashtable? $imht)
         (hashtable? $imeht)
         (eq-hashtable? $imht)
         (eq-hashtable? $imeht)
         (hashtable-weak? $imht)
         (not (hashtable-ephemeron? $imht))
         (hashtable-ephemeron? $imeht)
         (not (hashtable-weak? $imeht))
         (eq-hashtable-weak? $imht)
         (not (eq-hashtable-ephemeron? $imht))
         (eq-hashtable-ephemeron? $imeht)
         (not (eq-hashtable-weak? $imeht))
         (not (hashtable-mutable? $imht))
         (not (hashtable-mutable? $imeht))
         (hashtable? $wht2)
         (hashtable? $eht2)
         (eq-hashtable? $wht2)
         (eq-hashtable? $eht2)
         (hashtable-weak? $wht2)
         (not (hashtable-ephemeron? $wht2))
         (hashtable-ephemeron? $eht2)
         (not (hashtable-weak? $eht2))
         (eq-hashtable-weak? $wht2)
         (not (eq-hashtable-ephemeron? $ht2))
         (eq-hashtable-ephemeron? $eht2)
         (not (eq-hashtable-weak? $eht2))
         (hashtable-mutable? $wht2)
         (hashtable-mutable? $eht2)))
 ; eq-hashtable-ref
  (error? ; wrong argument count
    (eq-hashtable-ref))
  (error? ; wrong argument count
    (eq-hashtable-ref $wht))
  (error? ; wrong argument count
    (eq-hashtable-ref $wht 'a))
  (error? ; wrong argument count
    (eq-hashtable-ref $wht 'a 'b 'c))
  (error? ; not a hashtable
    (eq-hashtable-ref '(hash . table) 'a 'b))
 ; eq-hashtable-contains?
  (error? ; wrong argument count
    (eq-hashtable-contains?))
  (error? ; wrong argument count
    (eq-hashtable-contains? $wht))
  (error? ; wrong argument count
    (eq-hashtable-contains? $wht 'a 'b))
  (error? ; not a hashtable
    (eq-hashtable-contains? '(hash . table) 'a))
 ; eq-hashtable-set!
  (error? ; wrong argument count
    (eq-hashtable-set!))
  (error? ; wrong argument count
    (eq-hashtable-set! $wht))
  (error? ; wrong argument count
    (eq-hashtable-set! $wht 'a))
  (error? ; wrong argument count
    (eq-hashtable-set! $wht 'a 'b 'c))
  (error? ; not a hashtable
    (eq-hashtable-set! '(hash . table) 'a 'b))
  (error? ; hashtable not mutable
    (eq-hashtable-set! $imht 'a 'b))
 ; eq-hashtable-update!
  (error? ; wrong argument count
    (eq-hashtable-update!))
  (error? ; wrong argument count
    (eq-hashtable-update! $wht))
  (error? ; wrong argument count
    (eq-hashtable-update! $wht 'a values))
  (error? ; wrong argument count
    (eq-hashtable-update! $wht 'a values 'c 'd))
  (error? ; not a hashtable
    (eq-hashtable-update! '(hash . table) 'a values 'b))
  (error? ; hashtable not mutable
    (eq-hashtable-update! $imht 'a values 'b))
  (error? ; not a procedure
    (eq-hashtable-update! $wht 'a "not a procedure" 'b))
 ; eq-hashtable-delete!
  (error? ; wrong argument count
    (eq-hashtable-delete!))
  (error? ; wrong argument count
    (eq-hashtable-delete! $wht))
  (error? ; wrong argument count
    (eq-hashtable-delete! $wht 'a 'b))
  (error? ; not a hashtable
    (eq-hashtable-delete! '(hash . table) 'a))
  (error? ; hashtable not mutable
    (eq-hashtable-delete! $imht 'a))
 ; eq-hashtable-cell
  (error? ; wrong argument count
    (eq-hashtable-cell))
  (error? ; wrong argument count
    (eq-hashtable-cell $wht))
  (error? ; wrong argument count
    (eq-hashtable-cell $wht 'a))
  (error? ; wrong argument count
    (eq-hashtable-cell $wht 'a 'b 'c))
  (error? ; not a hashtable
    (eq-hashtable-cell '(hash . table) 'a 'b))
  (error? ; immutable hash table
    (eq-hashtable-cell (hashtable-copy $wht #f) 'a 'b))
 ; eq-hashtable-ref-cell
  (error? ; wrong argument count
    (eq-hashtable-ref-cell))
  (error? ; wrong argument count
    (eq-hashtable-ref-cell $wht))
  (error? ; wrong argument count
    (eq-hashtable-ref-cell $wht 'a 'b))
  (error? ; not a hashtable
    (eq-hashtable-ref-cell '(hash . table) 'a))
  (error? ; immutable hash table
    (eq-hashtable-ref-cell (hashtable-copy $wht #f) 'a))
 ; eq-hashtable-try-atomic-cell
  (error? ; wrong argument count
    (eq-hashtable-try-atomic-cell))
  (error? ; wrong argument count
    (eq-hashtable-try-atomic-cell $wht))
  (error? ; wrong argument count
    (eq-hashtable-try-atomic-cell $wht 'a))
  (error? ; wrong argument count
    (eq-hashtable-try-atomic-cell $wht 'a 'b 'c))
  (error? ; not a hashtable
    (eq-hashtable-try-atomic-cell '(hash . table) 'a 'b))
  (error? ; immutable hash table
    (eq-hashtable-try-atomic-cell (hashtable-copy $wht #f) 'a 'b))
 ; eq-hashtable-weak?
  (error? ; wrong argument count
    (eq-hashtable-weak?))
  (error? ; wrong argument count
    (eq-hashtable-weak? $ht 3))
  (error? ; not a hashtable
    (eq-hashtable-weak? '(hash . table)))
 ; eq-hashtable-ephemeron?
  (error? ; wrong argument count
    (eq-hashtable-ephemeron?))
  (error? ; wrong argument count
    (eq-hashtable-ephemeron? $ht 3))
  (error? ; not a hashtable
    (eq-hashtable-ephemeron? '(hash . table)))
)

(mat symbol-hashtable-arguments
  (begin
    (define $symht (make-hashtable symbol-hash eq? 50))
    (define $imsymht (hashtable-copy $symht))
    #t)
 ; symbol-hashtable-ref
  (error? ; wrong argument count
    (symbol-hashtable-ref))
  (error? ; wrong argument count
    (symbol-hashtable-ref $symht))
  (error? ; wrong argument count
    (symbol-hashtable-ref $symht 'a))
  (error? ; wrong argument count
    (symbol-hashtable-ref $symht 'a 'b 'c))
  (error? ; not a hashtable
    (symbol-hashtable-ref '(hash . table) 'a 'b))
  (error? ; not a symbol hashtable
    (symbol-hashtable-ref $ht 'a 'b))
  (error? ; not a symbol
    (symbol-hashtable-ref $symht '(a) 'b))
  (error? ; not a symbol
    (hashtable-ref $symht '(a) 'b))
 ; symbol-hashtable-contains?
  (error? ; wrong argument count
    (symbol-hashtable-contains?))
  (error? ; wrong argument count
    (symbol-hashtable-contains? $symht))
  (error? ; wrong argument count
    (symbol-hashtable-contains? $symht 'a 'b))
  (error? ; not a hashtable
    (symbol-hashtable-contains? '(hash . table) 'a))
  (error? ; not a symbol hashtable
    (symbol-hashtable-contains? $ht 'a))
  (error? ; not a symbol
    (symbol-hashtable-contains? $symht '(a)))
  (error? ; not a symbol
    (hashtable-contains? $symht '(a)))
 ; symbol-hashtable-set!
  (error? ; wrong argument count
    (symbol-hashtable-set!))
  (error? ; wrong argument count
    (symbol-hashtable-set! $symht))
  (error? ; wrong argument count
    (symbol-hashtable-set! $symht 'a))
  (error? ; wrong argument count
    (symbol-hashtable-set! $symht 'a 'b 'c))
  (error? ; not a hashtable
    (symbol-hashtable-set! '(hash . table) 'a 'b))
  (error? ; not a symbol hashtable
    (symbol-hashtable-set! $ht 'a 'b))
  (error? ; not a symbol
    (symbol-hashtable-set! $symht '(a) 'b))
  (error? ; not a symbol
    (hashtable-set! $symht '(a) 'b))
  (error? ; hashtable not mutable
    (symbol-hashtable-set! $imsymht 'a 'b))
 ; symbol-hashtable-update!
  (error? ; wrong argument count
    (symbol-hashtable-update!))
  (error? ; wrong argument count
    (symbol-hashtable-update! $symht))
  (error? ; wrong argument count
    (symbol-hashtable-update! $symht 'a values))
  (error? ; wrong argument count
    (symbol-hashtable-update! $symht 'a values 'c 'd))
  (error? ; not a hashtable
    (symbol-hashtable-update! '(hash . table) 'a values 'b))
  (error? ; not a symbol hashtable
    (symbol-hashtable-update! $ht 'a values 'b))
  (error? ; not a symbol
    (symbol-hashtable-update! $symht '(a) values 'b))
  (error? ; not a symbol
    (hashtable-update! $symht '(a) values 'b))
  (error? ; hashtable not mutable
    (symbol-hashtable-update! $imsymht 'a values 'b))
  (error? ; not a procedure
    (symbol-hashtable-update! $symht 'a "not a procedure" 'b))
 ; symbol-hashtable-delete!
  (error? ; wrong argument count
    (symbol-hashtable-delete!))
  (error? ; wrong argument count
    (symbol-hashtable-delete! $symht))
  (error? ; wrong argument count
    (symbol-hashtable-delete! $symht 'a 'b))
  (error? ; not a hashtable
    (symbol-hashtable-delete! '(hash . table) 'a))
  (error? ; not a symbol hashtable
    (symbol-hashtable-delete! $ht 'a))
  (error? ; not a symbol
    (symbol-hashtable-delete! $symht '(a)))
  (error? ; not a symbol
    (hashtable-delete! $symht '(a)))
  (error? ; hashtable not mutable
    (symbol-hashtable-delete! $imsymht 'a))
 ; symbol-hashtable-cell
  (error? ; wrong argument count
    (symbol-hashtable-cell))
  (error? ; wrong argument count
    (symbol-hashtable-cell $symht))
  (error? ; wrong argument count
    (symbol-hashtable-cell $symht 'a))
  (error? ; wrong argument count
    (symbol-hashtable-cell $symht 'a 'b 'c))
  (error? ; not a hashtable
    (symbol-hashtable-cell '(hash . table) 'a 'b))
  (error? ; not a symbol hashtable
    (symbol-hashtable-cell $ht 'a 'b))
  (error? ; not a symbol
    (symbol-hashtable-cell $symht '(a) 'b))
  (error? ; not a symbol
    (symbol-hashtable-cell $symht '(a) 'b))
  (error? ; not a symbol
    (hashtable-cell $symht '(a) 'b))
  (error? ; immutable hash table
    (symbol-hashtable-cell (hashtable-copy $symht #f) 'a 'b))
 ; symbol-hashtable-ref-cell
  (error? ; wrong argument count
    (symbol-hashtable-ref-cell))
  (error? ; wrong argument count
    (symbol-hashtable-ref-cell $symht))
  (error? ; wrong argument count
    (symbol-hashtable-ref-cell $symht 'a 'b))
  (error? ; not a hashtable
    (symbol-hashtable-ref-cell '(hash . table) 'a))
  (error? ; not a symbol hashtable
    (symbol-hashtable-ref-cell $ht 'a))
  (error? ; not a symbol
    (symbol-hashtable-ref-cell $symht '(a)))
  (error? ; not a symbol
    (hashtable-ref-cell $symht '(a)))
  (error? ; immutable hash table
    (symbol-hashtable-ref-cell (hashtable-copy $symht #f) 'a))
)

(mat eqv-hashtable-arguments
 ; make-weak-eqv-hashtable
  (error? ; wrong argument count
    (make-weak-eqv-hashtable 3 #t))
  (error? ; invalid size
    (make-weak-eqv-hashtable -1))
  (error? ; invalid size
    (make-weak-eqv-hashtable #t))
  (error? ; invalid size
    (make-weak-eqv-hashtable #f))
 ; make-ephemeron-eqv-hashtable
  (error? ; wrong argument count
    (make-ephemeron-eqv-hashtable 3 #t))
  (error? ; invalid size
    (make-ephemeron-eqv-hashtable -1))
  (error? ; invalid size
    (make-ephemeron-eqv-hashtable #t))
  (error? ; invalid size
    (make-ephemeron-eqv-hashtable #f))
)

(mat nonweak-eq-hashtable
  (begin
    (define h (make-eq-hashtable 32))
    (and (hashtable? h)
         (eq-hashtable? h)
         (hashtable-mutable? h)
         (not (eq-hashtable-weak? h))
         (not (eq-hashtable-ephemeron? h))
         (not (hashtable-weak? h))
         (not (hashtable-ephemeron? h))))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h 'a 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 'b)
       (hashtable-contains? h 'c))
    '(#t #f #f))
  (eqv? (hashtable-set! h 'b 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 'b)
       (hashtable-contains? h 'c))
    '(#t #t #f))
  (eqv? (hashtable-set! h 'c 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 'b)
       (hashtable-contains? h 'c))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (same-elements? (hashtable-keys h) '#(a b c))
  (same-elements? (hashtable-values h) '#(bval cval aval))
  (equal-entries? h '#(b c a) '#(bval cval aval))
  (same-elements? (hashtable-cells h) '#((b . bval) (c . cval) (a . aval)))
  (same-elements? (hashtable-cells h (expt 2 100)) '#((b . bval) (c . cval) (a . aval)))
  (let ([cells (hashtable-cells h 2)])
    (or (same-elements? cells '#((b . bval) (c . cval)))
        (same-elements? cells '#((b . bval) (a . aval)))
        (same-elements? cells '#((c . cval) (a . aval)))))
  #;(same-elements? (list->vector (hashtable-map h cons)) '#((a . aval) (b . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (b . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (b . bval) (c . cval)))
  (equal? (hashtable-ref h 'a 1) 'aval)
  (equal? (hashtable-ref h 'b #f) 'bval)
  (equal? (hashtable-ref h 'c 'nope) 'cval)
  (eqv? (hashtable-delete! h 'b) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#(a c) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (eq-hashtable? h2)
         (hashtable-mutable? h2)
         (not (hashtable-weak? h2))
         (not (eq-hashtable-weak? h2))
         (not (hashtable-ephemeron? h2))
         (not (eq-hashtable-ephemeron? h2))))
  (eq? (hashtable-hash-function h2) #f)
  (eq? (hashtable-equivalence-function h2) eq?)
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h 'a 1)
      (hashtable-ref h 'b #f)
      (hashtable-ref h 'c 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 'a 1)
      (hashtable-ref h2 'b #f)
      (hashtable-ref h2 'c 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv?
    (hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h 'q #f) 18)
  (eqv?
    (hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h 'q #f) 19)
  (equal? (hashtable-size h) 1)
 ; test hashtable-copy when some keys may have moved
  (let ([t (parameterize ([collect-request-handler void])
             (let ([h4a (make-eq-hashtable 32)]
                   [k* (map list (make-list 100))])
               (for-each (lambda (x) (hashtable-set! h4a x x)) k*)
               (collect)
              ; create copy after collection but before otherwise touching h4a
               (let ([h4b (hashtable-copy h4a #t)])
                 (andmap
                   (lambda (k) (eq? (hashtable-ref h4b k #f) k))
                   k*))))])
    (collect)
    t)

 ; test for proper shrinkage
  (eqv?
    (let ([ht (make-eq-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (#%$hashtable-veclen ht))
    32)
)

(mat weak-eq-hashtable
  (begin
    (define ka (list 'a))
    (define kb (list 'b))
    (define kc (list 'c))
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    #t)
  (begin
    (define h (make-weak-eq-hashtable 32))
    (and (hashtable? h)
         (eq-hashtable? h)
         (hashtable-mutable? h)
         (hashtable-weak? h)
         (eq-hashtable-weak? h)))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h ka 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc))
    '(#t #f #f))
  (eqv? (hashtable-set! h kb 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc))
    '(#t #t #f))
  (eqv? (hashtable-set! h kc 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (same-elements? (hashtable-keys h) '#((a) (b) (c)))
  (same-elements? (hashtable-values h) '#(bval cval aval))
  (equal-entries? h '#((a) (b) (c)) '#(aval bval cval))
  (same-elements? (hashtable-cells h) (vector (cons ka 'aval) (cons kb 'bval) (cons kc 'cval)))
  (same-elements? (hashtable-cells h (expt 2 100)) (vector (cons ka 'aval) (cons kb 'bval) (cons kc 'cval)))
  (let ([cells (hashtable-cells h 2)])
    (or (same-elements? cells (vector (cons ka 'aval) (cons kb 'bval)))
        (same-elements? cells (vector (cons ka 'aval) (cons kc 'cval)))
        (same-elements? cells (vector (cons kb 'bval) (cons kc 'cval)))))
  (andmap weak-pair? (vector->list (hashtable-cells h)))
  #;(same-elements? (list->vector (hashtable-map h cons)) '#(((a) . aval) ((b) . bval) ((c) . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    '#(((a) . aval) ((b) . bval) ((c) . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    '#(((a) . aval) ((b) . bval) ((c) . cval)))
  (equal? (hashtable-ref h ka 1) 'aval)
  (equal? (hashtable-ref h kb #f) 'bval)
  (equal? (hashtable-ref h kc 'nope) 'cval)
  (eqv? (hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#((a) (c)) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (eq-hashtable? h2)
         (hashtable-mutable? h2)
         (eq-hashtable-weak? h2)
         (hashtable-weak? h2)))
  (eq? (hashtable-hash-function h2) #f)
  (eq? (hashtable-equivalence-function h2) eq?)
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h ka 1)
      (hashtable-ref h kb #f)
      (hashtable-ref h kc 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 ka 1)
      (hashtable-ref h2 kb #f)
      (hashtable-ref h2 kc 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 18)
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  #;(eqv? (hashtable-map h (lambda args (error #f "oops"))) '())
  #;(eqv? (hashtable-for-each h (lambda args (error #f "oops"))) (void))
  #;(eqv? (hashtable-for-each-cell h (lambda args (error #f "oops"))) (void))
  (equal? (hashtable-ref h ky #f) #f)
  (eqv?
    (hashtable-set! h ky 'toad)
    (void))
  (equal? (hashtable-ref h ky #f) 'toad)
  (equal? (hashtable-ref h kz #f) #f)
  (eqv?
    (hashtable-update! h kz list 'frog)
    (void))
  (equal? (hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (hashtable-ref h kz #f) 'toad))
  (eqv? (hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (eq-hashtable? h3)
         (not (hashtable-mutable? h3))
         (eq-hashtable-weak? h3)
         (hashtable-weak? h3)))
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (equal-entries? h3 '#((a) (c)) '#(aval cval))
  (equal?
    (begin
      (set! ka (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(1 1))
  (equal-entries? h2 '#((c)) '#(cval))
  (equal-entries? h3 '#((c)) '#(cval))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    1)
  (equal-entries? h2 '#((c)) '#(cval))

 ; test for proper shrinkage
  (eqv?
    (let ([ht (make-weak-eq-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (#%$hashtable-veclen ht))
    32)

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let* ([ht (make-weak-eq-hashtable 32)]
           [len (#%$hashtable-veclen ht)])
      (hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (hashtable-delete! ht 'a)
      (list (hashtable-size ht) (= (#%$hashtable-veclen ht) len)))
    '(0 #t))

  ; test that weak-hashtable values *do* make keys reachable
  (let ([wk1 (list 1)]
        [wk2 (list 2)]
        [wk3 (list 3)]
        [wk4 (list 4)]
        [ht (make-weak-eq-hashtable)])
    (hashtable-set! ht wk1 wk1)
    (hashtable-set! ht wk2 wk1)
    (hashtable-set! ht wk3 wk3)
    (hashtable-set! ht wk4 wk2)
    (collect (collect-maximum-generation))
    (and
     (equal-entries? ht '#((1) (2) (3) (4)) '#((1) (1) (3) (2)))
     (equal? (hashtable-ref ht wk1 #f) wk1)
     (equal? (hashtable-ref ht wk2 #f) wk1)
     (equal? (hashtable-ref ht wk3 #f) wk3)
     (equal? (hashtable-ref ht wk4 #f) wk2)
     (begin
       (set! wk1 #f)
       (set! wk2 #f)
       (set! wk3 #f)
       (collect (collect-maximum-generation))
       (and
        (equal-entries? ht '#((1) (2) (3) (4)) '#((1) (1) (3) (2)))
        (equal? (hashtable-ref ht wk4 #f) '(2))
        (begin
          (set! wk4 #f)
          (collect (collect-maximum-generation))
          (equal-entries? ht '#((1) (2) (3)) '#((1) (1) (3))))))))
)

(mat ephemeron-eq-hashtable
  (begin
    (define ka (list 'a)) ; will map to self  \ Doesn't do anything to check
    (define kb (list 'b)) ; will map to kc \  | ephemeronness, but just in
    (define kc (list 'c)) ; will map to kb /  / case.
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    #t)
  (begin
    (define h (make-ephemeron-eq-hashtable 32))
    (and (hashtable? h)
         (eq-hashtable? h)
         (hashtable-mutable? h)
         (hashtable-ephemeron? h)
         (eq-hashtable-ephemeron? h)))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h ka ka) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc))
    '(#t #f #f))
  (eqv? (hashtable-set! h kb kc) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc))
    '(#t #t #f))
  (eqv? (hashtable-set! h kc kb) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (same-elements? (hashtable-keys h) '#((a) (b) (c)))
  (same-elements? (hashtable-values h) '#((a) (b) (c)))
  (equal-entries? h '#((a) (b) (c)) '#((a) (c) (b)))
  (same-elements? (hashtable-cells h) (vector (cons ka ka) (cons kb kc) (cons kc kb)))
  (same-elements? (hashtable-cells h (expt 2 100)) (vector (cons ka ka) (cons kb kc) (cons kc kb)))
  (let ([cells (hashtable-cells h 2)])
    (or (same-elements? cells (vector (cons ka ka) (cons kb kc)))
        (same-elements? cells (vector (cons ka ka) (cons kc kb)))
        (same-elements? cells (vector (cons kb kc) (cons kc kb)))))
  (andmap ephemeron-pair? (vector->list (hashtable-cells h)))
  #;(same-elements? (list->vector (hashtable-map h cons)) '#(((a) . a) ((b) . c) ((c) . b)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    '#(((a) . a) ((b) . c) ((c) . b)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    '#(((a) . a) ((b) . c) ((c) . b)))
  (equal? (hashtable-ref h ka 1) '(a))
  (equal? (hashtable-ref h kb #f) '(c))
  (equal? (hashtable-ref h kc 'nope) '(b))
  (eqv? (hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#((a) (c)) '#((a) (b)))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (eq-hashtable? h2)
         (hashtable-mutable? h2)
         (eq-hashtable-ephemeron? h2)
         (hashtable-ephemeron? h2)))
  (eq? (hashtable-hash-function h2) #f)
  (eq? (hashtable-equivalence-function h2) eq?)
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#((a) (c)) '#((a) (b)))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h ka 1)
      (hashtable-ref h kb #f)
      (hashtable-ref h kc 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 ka 1)
      (hashtable-ref h2 kb #f)
      (hashtable-ref h2 kc 'nope))
    '(2 (a) #f (b)))
  (equal-entries? h2 '#((a) (c)) '#((a) (b)))
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 18)
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (equal-entries? h '#() '#())
  #;(eqv? (hashtable-map h (lambda args (error #f "oops"))) '())
  #;(eqv? (hashtable-for-each h (lambda args (error #f "oops"))) (void))
  #;(eqv? (hashtable-for-each-cell h (lambda args (error #f "oops"))) (void))
  (equal? (hashtable-ref h ky #f) #f)
  (eqv?
    (hashtable-set! h ky 'toad)
    (void))
  (equal? (hashtable-ref h ky #f) 'toad)
  (equal? (hashtable-ref h kz #f) #f)
  (eqv?
    (hashtable-update! h kz list 'frog)
    (void))
  (equal? (hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (hashtable-ref h kz #f) 'toad))
  (eqv? (hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (eq-hashtable? h3)
         (not (hashtable-mutable? h3))
         (eq-hashtable-ephemeron? h3)
         (hashtable-ephemeron? h3)))
  (equal-entries? h2 '#((a) (c)) '#((a) (b)))
  (equal-entries? h3 '#((a) (c)) '#((a) (b)))
  (equal?
    (begin
      (set! ka (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(1 1))
  (equal-entries? h2 '#((c)) '#((b)))
  (equal-entries? h3 '#((c)) '#((b)))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    1)
  (equal-entries? h2 '#((c)) '#((b)))

 ; test for proper shrinkage
  (eqv?
    (let ([ht (make-ephemeron-eq-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (#%$hashtable-veclen ht))
    32)

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let* ([ht (make-ephemeron-eq-hashtable 32)]
           [len (#%$hashtable-veclen ht)])
      (hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (hashtable-delete! ht 'a)
      (list (hashtable-size ht) (= (#%$hashtable-veclen ht) len)))
    '(0 #t))

  ; test that ephemeron-hashtable values don't make keys reachable
  (let ([wk1 (list 1)]
        [wk2 (list 2)]
        [wk3 (list 3)]
        [wk4 (list 4)]
        [ht (make-ephemeron-eq-hashtable)])
    (hashtable-set! ht wk1 wk1)
    (hashtable-set! ht wk2 wk1)
    (hashtable-set! ht wk3 wk3)
    (hashtable-set! ht wk4 wk2)
    (collect (collect-maximum-generation))
    (and
     (equal-entries? ht '#((1) (2) (3) (4)) '#((1) (1) (3) (2)))
     (equal? (hashtable-ref ht wk1 #f) wk1)
     (equal? (hashtable-ref ht wk2 #f) wk1)
     (equal? (hashtable-ref ht wk3 #f) wk3)
     (equal? (hashtable-ref ht wk4 #f) wk2)
     (begin
       (set! wk1 #f)
       (set! wk2 #f)
       (set! wk3 #f)
       (collect (collect-maximum-generation))
       (and
        (equal-entries? ht '#((1) (2) (4)) '#((1) (1) (2)))
        (equal? (hashtable-ref ht wk4 #f) '(2))
        (begin
          (set! wk4 #f)
          (collect (collect-maximum-generation))
          (equal-entries? ht '#() '#()))))))
)

(mat eq-hashtable-cell
  (let ()
    (define-record fribble (x))
    (define random-object
      (lambda (x)
        (case (random 9)
          [(0) (cons 'a 'b)]
          [(1) (vector 'c)]
          [(2) (string #\a #\b)]
          [(3) (make-fribble 'q)]
          [(4) (gensym)]
          [(5) (open-output-string)]
          [(6) (fxvector 15 55)]
          [(7) (lambda () x)]
          [(8) (flvector 15.0 55.0)]
          [else (box 'top)])))
    (let ([ls1 (let f ([n 10000])
                 (if (fx= n 0)
                     '()
                     (cons
                       (cons (random-object 4) (random-object 7))
                       (f (fx- n 1)))))]
          [ht (make-eq-hashtable)]
          [wht (make-weak-eq-hashtable)]
          [eht (make-ephemeron-eq-hashtable)])
      (let ([ls2 (map (lambda (a1) (eq-hashtable-cell ht (car a1) (cdr a1))) ls1)]
            [ls2-2 (map (lambda (a1) (let loop ()
                                       (define c (eq-hashtable-try-atomic-cell ht (car a1) (cdr a1)))
                                       (or c (loop))))
                        ls1)]
            [ls3 (map (lambda (a1) (hashtable-cell wht (car a1) (cdr a1))) ls1)]
            [ls4 (map (lambda (a1) (hashtable-cell eht (car a1) (cdr a1))) ls1)])
        (let ([ls2* (map (lambda (a1) (eq-hashtable-ref-cell ht (car a1))) ls1)]
              [ls3* (map (lambda (a1) (hashtable-ref-cell wht (car a1))) ls1)]
              [ls4* (map (lambda (a1) (hashtable-ref-cell eht (car a1))) ls1)])
          (unless (andmap (lambda (a2 a2* a3 a3* a4 a4*)
                            (and (eq? a2 a2*)
                                 (eq? a3 a3*)
                                 (eq? a4 a4*)))
                          ls2 ls2* ls3 ls3* ls4 ls4*)
            (errorf #f "hashtable-ref-cell and hashtable-cell do not retrieve the same cells")))
        (unless (andmap (lambda (a1 a2 a2-2 a3 a4)
                          (and (eq? (car a1) (car a2))
                               (eq? a2 a2-2)
                               (eq? (car a2) (car a3))
                               (eq? (car a2) (car a4))))
                        ls1 ls2 ls2-2 ls3 ls4)
          (errorf #f "keys are not eq"))
        (unless (andmap (lambda (a1 a2 a3 a4)
                          (and (eq? (cdr a1) (cdr a2))
                               (eq? (cdr a2) (cdr a3))
                               (eq? (cdr a2) (cdr a4))))
                        ls1 ls2 ls3 ls4)
          (errorf #f "values are not eq"))
        (for-each (lambda (a1)
                    (let ([o (random-object 3)])
                      ;; Value refers to key:
                      (hashtable-set! eht o (list o (car a1)))))
                  ls1)
        (for-each
          (lambda (a1)
            (when (fx< (random 10) 5)
              (set-car! a1 #f)))
          ls1)
        (parameterize ([collect-maximum-generation-threshold-factor 0])
          (let loop ([i (min (expt (collect-generation-radix) (collect-maximum-generation)) 1000)])
            (unless (fx= i 0)
              (collect)
              (unless (andmap (lambda (a2 a3 a4) (and (eq? (car a2) (car a3)) (eq? (car a2) (car a4))))
                              ls2 ls3 ls4)
                (errorf #f "a2/a3/a4 keys not eq after collection"))
              (unless (and (andmap (lambda (a3) (not (bwp-object? (car a3)))) ls3)
                           (andmap (lambda (a4) (not (bwp-object? (car a4)))) ls4))
                (errorf #f "keys have been bwp'd"))
              (loop (fx- i 1)))))
        (for-each
          (lambda (a2)
            (hashtable-delete! ht (car a2))
            (set-car! a2 #f))
          ls2)
        (unless (and (equal? (hashtable-keys ht) '#())
                     (equal? (hashtable-values ht) '#())
                     (zero? (hashtable-size ht)))
          (errorf #f "ht has not been cleared out"))
        (parameterize ([collect-maximum-generation-threshold-factor 0])
          (let loop ([i (min (expt (collect-generation-radix) (collect-maximum-generation)) 1000)])
            (unless (fx= i 0)
              (collect)
              (unless (andmap (lambda (a1 a3 a4)
                                (or (not (car a1))
                                    (and (eq? (car a1) (car a3))
                                         (eq? (car a1) (car a4)))))
                              ls1 ls3 ls4)
                (errorf #f "a1/a3/a4 keys not eq after collection"))
              (loop (fx- i 1)))))
        (for-each
          (lambda (a1 a3 a4)
            (unless (or (car a1)
                        (and (bwp-object? (car a3))
                             (bwp-object? (car a4))))
              (errorf #f "~s has not been bwp'd I" (car a3))))
          ls1 ls3 ls4)
        (for-each (lambda (a1) (set-car! a1 #f)) ls1)
        (collect (collect-maximum-generation))
        (unless (and (andmap (lambda (a3) (bwp-object? (car a3))) ls3)
                     (andmap (lambda (a4) (bwp-object? (car a4))) ls4))
          (errorf #f "keys have not been bwp'd II"))
        (unless (and (equal? (hashtable-keys wht) '#())
                     (equal? (hashtable-values wht) '#())
                     (zero? (hashtable-size wht)))
          (errorf #f "wht has not been cleared out"))
        (unless (and (equal? (hashtable-keys eht) '#())
                     (equal? (hashtable-values eht) '#())
                     (zero? (hashtable-size eht)))
          (errorf #f "eht has not been cleared out"))))
    #t)
)

(mat $nonweak-eq-hashtable
  (begin
    (define h (make-eq-hashtable 32))
    (and (hashtable? h)
         (eq-hashtable? h)
         (hashtable-mutable? h)
         (not (eq-hashtable-weak? h))
         (not (hashtable-weak? h))
         (not (eq-hashtable-ephemeron? h))
         (not (hashtable-ephemeron? h))))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (eq-hashtable-set! h 'a 'aval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h 'a)
       (eq-hashtable-contains? h 'b)
       (eq-hashtable-contains? h 'c))
    '(#t #f #f))
  (eqv? (eq-hashtable-set! h 'b 'bval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h 'a)
       (eq-hashtable-contains? h 'b)
       (eq-hashtable-contains? h 'c))
    '(#t #t #f))
  (eqv? (eq-hashtable-set! h 'c 'cval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h 'a)
       (eq-hashtable-contains? h 'b)
       (eq-hashtable-contains? h 'c))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (same-elements? (hashtable-keys h) '#(a b c))
  (same-elements? (hashtable-values h) '#(bval cval aval))
  (equal-entries? h '#(b c a) '#(bval cval aval))
  (same-elements? (hashtable-cells h) '#((a . aval) (b . bval) (c . cval)))
  (same-elements? (hashtable-cells h (expt 2 100)) '#((a . aval) (b . bval) (c . cval)))
  (let ([cells (hashtable-cells h 2)])
    (or (same-elements? cells '#((a . aval) (b . bval)))
        (same-elements? cells '#((a . aval) (c . cval)))
        (same-elements? cells '#((b . bval) (c . cval)))))
  (equal? (eq-hashtable-ref h 'a 1) 'aval)
  (equal? (eq-hashtable-ref h 'b #f) 'bval)
  (equal? (eq-hashtable-ref h 'c 'nope) 'cval)
  (eqv? (eq-hashtable-delete! h 'b) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#(a c) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (eq-hashtable? h2)
         (hashtable-mutable? h2)
         (not (eq-hashtable-weak? h2))
         (not (hashtable-weak? h2))))
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (eq-hashtable-ref h 'a 1)
      (eq-hashtable-ref h 'b #f)
      (eq-hashtable-ref h 'c 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (eq-hashtable-ref h2 'a 1)
      (eq-hashtable-ref h2 'b #f)
      (eq-hashtable-ref h2 'c 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv?
    (eq-hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (eq-hashtable-ref h 'q #f) 18)
  (eqv?
    (eq-hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (eq-hashtable-ref h 'q #f) 19)
  (equal? (hashtable-size h) 1)
 ; test hashtable-copy when some keys may have moved
  (let ([t (parameterize ([collect-request-handler void])
             (let ([h4a (make-eq-hashtable 32)]
                   [k* (map list (make-list 100))])
               (for-each (lambda (x) (eq-hashtable-set! h4a x x)) k*)
               (collect)
              ; create copy after collection but before otherwise touching h4a
               (let ([h4b (hashtable-copy h4a #t)])
                 (andmap
                   (lambda (k) (eq? (eq-hashtable-ref h4b k #f) k))
                   k*))))])
    (collect)
    t)

 ; test for proper shrinkage, etc.
  (equal?
    (let* ([ht (make-eq-hashtable)] [minlen (#%$hashtable-veclen ht)])
      (define power-of-two? (lambda (n) (fx= (fxbit-count n) 1)))
      (let f ([i 0])
        (unless (fx= i (expt 2 17))
          (let ([k (fx* i 2)])
            (eq-hashtable-set! ht k i)
            (f (fx+ i 1))
            (assert (eq-hashtable-contains? ht k))
            (assert (power-of-two? (#%$hashtable-veclen ht)))
            (eq-hashtable-delete! ht k))))
      (list (hashtable-size ht) (fx= (#%$hashtable-veclen ht) minlen)))
    '(0 #t))

  (equal?
    (let ([ht (make-eq-hashtable 32)])
      (define power-of-two? (lambda (n) (fx= (fxbit-count n) 1)))
      (let f ([i 0])
        (unless (fx= i (expt 2 17))
          (let ([k (fx* i 2)])
            (eq-hashtable-set! ht k i)
            (f (fx+ i 1))
            (assert (eq-hashtable-contains? ht k))
            (assert (power-of-two? (#%$hashtable-veclen ht)))
            (eq-hashtable-delete! ht k))))
      (list (hashtable-size ht) (#%$hashtable-veclen ht)))
    '(0 32))
)

(mat $weak-eq-hashtable
  (begin
    (define ka (list 'a))
    (define kb (list 'b))
    (define kc (list 'c))
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    #t)
  (begin
    (define h (make-weak-eq-hashtable 32))
    (and (hashtable? h)
         (eq-hashtable? h)
         (hashtable-mutable? h)
         (eq-hashtable-weak? h)
         (hashtable-weak? h)))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (eq-hashtable-set! h ka 'aval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h ka)
       (eq-hashtable-contains? h kb)
       (eq-hashtable-contains? h kc))
    '(#t #f #f))
  (eqv? (eq-hashtable-set! h kb 'bval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h ka)
       (eq-hashtable-contains? h kb)
       (eq-hashtable-contains? h kc))
    '(#t #t #f))
  (eqv? (eq-hashtable-set! h kc 'cval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h ka)
       (eq-hashtable-contains? h kb)
       (eq-hashtable-contains? h kc))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (same-elements? (hashtable-keys h) '#((a) (b) (c)))
  (same-elements? (hashtable-values h) '#(aval bval cval))
  (equal-entries? h '#((a) (b) (c)) '#(aval bval cval))
  (same-elements? (hashtable-cells h) (vector (cons ka 'aval) (cons kb 'bval) (cons kc 'cval)))
  (same-elements? (hashtable-cells h (expt 2 100)) (vector (cons ka 'aval) (cons kb 'bval) (cons kc 'cval)))
  (let ([cells (hashtable-cells h 2)])
    (or (same-elements? cells (vector (cons ka 'aval) (cons kb 'bval)))
        (same-elements? cells (vector (cons ka 'aval) (cons kc 'cval)))
        (same-elements? cells (vector (cons kb 'bval) (cons kc 'cval)))))
  (andmap weak-pair? (vector->list (hashtable-cells h)))
  (equal? (eq-hashtable-ref h ka 1) 'aval)
  (equal? (eq-hashtable-ref h kb #f) 'bval)
  (equal? (eq-hashtable-ref h kc 'nope) 'cval)
  (eqv? (eq-hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#((a) (c)) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (eq-hashtable? h2)
         (hashtable-mutable? h2)
         (hashtable-weak? h2)
         (eq-hashtable-weak? h2)))
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (eq-hashtable-ref h ka 1)
      (eq-hashtable-ref h kb #f)
      (eq-hashtable-ref h kc 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (eq-hashtable-ref h2 ka 1)
      (eq-hashtable-ref h2 kb #f)
      (eq-hashtable-ref h2 kc 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (eqv?
    (eq-hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (eq-hashtable-ref h kq #f) 18)
  (eqv?
    (eq-hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (eq-hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (equal-entries? h '#() '#())
  (equal? (eq-hashtable-ref h ky #f) #f)
  (eqv?
    (eq-hashtable-set! h ky 'toad)
    (void))
  (equal? (eq-hashtable-ref h ky #f) 'toad)
  (equal? (eq-hashtable-ref h kz #f) #f)
  (eqv?
    (eq-hashtable-update! h kz list 'frog)
    (void))
  (equal? (eq-hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (eq-hashtable-ref h kz #f) 'toad))
  (eqv? (eq-hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (eq-hashtable? h3)
         (not (hashtable-mutable? h3))
         (eq-hashtable-weak? h3)
         (hashtable-weak? h3)))
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (equal-entries? h3 '#((a) (c)) '#(aval cval))
  (equal?
    (begin
      (set! ka (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(1 1))
  (equal-entries? h2 '#((c)) '#(cval))
  (equal-entries? h3 '#((c)) '#(cval))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    1)
  (equal-entries? h2 '#((c)) '#(cval))

 ; test for proper shrinkage
  (eqv?
    (let ([ht (make-weak-eq-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (eq-hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (eq-hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (#%$hashtable-veclen ht))
    32)

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let* ([ht (make-weak-eq-hashtable 32)]
           [len (#%$hashtable-veclen ht)])
      (eq-hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (eq-hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (eq-hashtable-delete! ht 'a)
      (list (hashtable-size ht) (= (#%$hashtable-veclen ht) len)))
    '(0 #t))
  )

(mat $ephemeron-eq-hashtable
  (begin
    (define ka (list 'a))
    (define kb (list 'b))
    (define kc (list 'c))
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    #t)
  (begin
    (define h (make-ephemeron-eq-hashtable 32))
    (and (hashtable? h)
         (eq-hashtable? h)
         (hashtable-mutable? h)
         (eq-hashtable-ephemeron? h)
         (hashtable-ephemeron? h)))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (eq-hashtable-set! h ka 'aval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h ka)
       (eq-hashtable-contains? h kb)
       (eq-hashtable-contains? h kc))
    '(#t #f #f))
  (eqv? (eq-hashtable-set! h kb 'bval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h ka)
       (eq-hashtable-contains? h kb)
       (eq-hashtable-contains? h kc))
    '(#t #t #f))
  (eqv? (eq-hashtable-set! h kc 'cval) (void))
  (equal?
    (list
       (eq-hashtable-contains? h ka)
       (eq-hashtable-contains? h kb)
       (eq-hashtable-contains? h kc))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (same-elements? (hashtable-keys h) '#((a) (b) (c)))
  (same-elements? (hashtable-values h) '#(aval bval cval))
  (equal-entries? h '#((a) (b) (c)) '#(aval bval cval))
  (same-elements? (hashtable-cells h) (vector (cons ka 'aval) (cons kb 'bval) (cons kc 'cval)))
  (same-elements? (hashtable-cells h (expt 2 100)) (vector (cons ka 'aval) (cons kb 'bval) (cons kc 'cval)))
  (let ([cells (hashtable-cells h 2)])
    (or (same-elements? cells (vector (cons ka 'aval) (cons kb 'bval)))
        (same-elements? cells (vector (cons ka 'aval) (cons kc 'cval)))
        (same-elements? cells (vector (cons kb 'bval) (cons kc 'cval)))))
  (andmap ephemeron-pair? (vector->list (hashtable-cells h)))
  (equal? (eq-hashtable-ref h ka 1) 'aval)
  (equal? (eq-hashtable-ref h kb #f) 'bval)
  (equal? (eq-hashtable-ref h kc 'nope) 'cval)
  (eqv? (eq-hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#((a) (c)) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (eq-hashtable? h2)
         (hashtable-mutable? h2)
         (hashtable-ephemeron? h2)
         (eq-hashtable-ephemeron? h2)))
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (eq-hashtable-ref h ka 1)
      (eq-hashtable-ref h kb #f)
      (eq-hashtable-ref h kc 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (eq-hashtable-ref h2 ka 1)
      (eq-hashtable-ref h2 kb #f)
      (eq-hashtable-ref h2 kc 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (eqv?
    (eq-hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (eq-hashtable-ref h kq #f) 18)
  (eqv?
    (eq-hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (eq-hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (equal-entries? h '#() '#())
  (equal? (eq-hashtable-ref h ky #f) #f)
  (eqv?
    (eq-hashtable-set! h ky 'toad)
    (void))
  (equal? (eq-hashtable-ref h ky #f) 'toad)
  (equal? (eq-hashtable-ref h kz #f) #f)
  (eqv?
    (eq-hashtable-update! h kz list 'frog)
    (void))
  (equal? (eq-hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (eq-hashtable-ref h kz #f) 'toad))
  (eqv? (eq-hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (eq-hashtable? h3)
         (not (hashtable-mutable? h3))
         (eq-hashtable-ephemeron? h3)
         (hashtable-ephemeron? h3)))
  (equal-entries? h2 '#((a) (c)) '#(aval cval))
  (equal-entries? h3 '#((a) (c)) '#(aval cval))
  (equal?
    (begin
      (set! ka (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(1 1))
  (equal-entries? h2 '#((c)) '#(cval))
  (equal-entries? h3 '#((c)) '#(cval))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    1)
  (equal-entries? h2 '#((c)) '#(cval))

 ; test for proper shrinkage
  (eqv?
    (let ([ht (make-ephemeron-eq-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (eq-hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (eq-hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (#%$hashtable-veclen ht))
    32)

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let* ([ht (make-ephemeron-eq-hashtable 32)]
           [len (#%$hashtable-veclen ht)])
      (eq-hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (eq-hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (eq-hashtable-delete! ht 'a)
      (list (hashtable-size ht) (= (#%$hashtable-veclen ht) len)))
    '(0 #t))
)

(mat eq-strange
  (begin
    (define $ht (make-eq-hashtable))
    (define $wht (make-weak-eq-hashtable))
    (define $eht (make-ephemeron-eq-hashtable))
    (and (hashtable? $ht)
         (eq-hashtable? $ht)
         (hashtable? $wht)
         (eq-hashtable? $wht)
         (hashtable? $eht)
         (eq-hashtable? $eht)))
  (eqv? (hashtable-set! $ht #f 75) (void))
  (eqv? (hashtable-ref $ht #f 80) 75)
  (eqv? (hashtable-set! $wht #f 75) (void))
  (eqv? (hashtable-ref $wht #f 80) 75)
  (eqv? (hashtable-set! $eht #f 75) (void))
  (eqv? (hashtable-ref $eht #f 80) 75)
  (eqv? (hashtable-set! $ht #!bwp "hello") (void))
  (equal? (hashtable-ref $ht #!bwp "goodbye") "hello")
  (eqv? (hashtable-set! $wht #!bwp "hello") (void))
  (and (member (hashtable-ref $wht #!bwp "goodbye") '("hello" "goodbye")) #t)
  (eqv? (hashtable-set! $eht #!bwp "hello") (void))
  (and (member (hashtable-ref $eht #!bwp "goodbye") '("hello" "goodbye")) #t)
 ; make sure that association isn't added before procedure is called
  (equal?
    (begin
      (hashtable-update! $ht 'cupie
        (lambda (x) (hashtable-ref $ht 'cupie (cons 'barbie x)))
        'doll)
      (hashtable-ref $ht 'cupie 'oops))
   '(barbie . doll))
  (equal?
    (begin
      (hashtable-update! $wht 'cupie
        (lambda (x) (hashtable-ref $wht 'cupie (cons 'barbie x)))
        'doll)
      (hashtable-ref $wht 'cupie 'oops))
   '(barbie . doll))
  (equal?
    (begin
      (hashtable-update! $eht 'cupie
        (lambda (x) (hashtable-ref $eht 'cupie (cons 'barbie x)))
        'doll)
      (hashtable-ref $eht 'cupie 'oops))
   '(barbie . doll))
)

(mat eq-hashtable-stress
 ; stress tests
  (let () ; nonweak
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-eq-hashtable 4))
    (let ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (= (hashtable-size ht) (- n (length drop)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (hashtable-ref ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eq? (hashtable-ref ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (hashtable-set! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (hashtable-delete! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (hashtable-delete! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))

  (let () ; weak
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-weak-eq-hashtable 4))
    (let ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (<= (hashtable-size ht) (- n (length drop)))
                 (begin
                   (collect (collect-maximum-generation))
                   (= (hashtable-size ht) (length keep)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (hashtable-ref ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eq? (hashtable-ref ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (hashtable-set! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (hashtable-delete! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (hashtable-delete! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))

  (let () ; ephemeron
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-ephemeron-eq-hashtable 4))
    (let ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (<= (hashtable-size ht) (- n (length drop)))
                 (begin
                   (collect (collect-maximum-generation))
                   (= (hashtable-size ht) (length keep)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (hashtable-ref ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eq? (hashtable-ref ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (hashtable-set! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (hashtable-delete! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (hashtable-delete! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))

)

(mat nonweak-eqv-hashtable
  (begin
    (define h (make-eqv-hashtable 32))
    (and (hashtable? h)
         (not (eq-hashtable? h))
         (hashtable-mutable? h)
         (not (hashtable-weak? h))
         (not (hashtable-ephemeron? h))))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eqv?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h 'a 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 3.4)
       (hashtable-contains? h 'c))
    '(#t #f #f))
  (eqv? (hashtable-set! h 3.4 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 3.4)
       (hashtable-contains? h 'c))
    '(#t #t #f))
  (eqv? (hashtable-set! h 'c 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 3.4)
       (hashtable-contains? h 'c))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (equal-entries? h '#(3.4 c a) '#(bval cval aval))
  #;(same-elements? (list->vector (hashtable-map h cons)) '#((a . aval) (3.4 . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (3.4 . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (3.4 . bval) (c . cval)))
  (equal? (hashtable-ref h 'a 1) 'aval)
  (equal? (hashtable-ref h 3.4 #f) 'bval)
  (equal? (hashtable-ref h 'c 'nope) 'cval)
  (eqv? (hashtable-delete! h 3.4) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#(a c) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (hashtable-mutable? h2)
         (not (hashtable-weak? h2))
         (not (hashtable-ephemeron? h2))))
  (eq? (hashtable-hash-function h2) #f)
  (eq? (hashtable-equivalence-function h2) eqv?)
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h 'a 1)
      (hashtable-ref h 3.4 #f)
      (hashtable-ref h 'c 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 'a 1)
      (hashtable-ref h2 3.4 #f)
      (hashtable-ref h2 'c 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv?
    (hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h 'q #f) 18)
  (eqv?
    (hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h 'q #f) 19)
  (equal? (hashtable-size h) 1)
 ; test hashtable-copy when some keys may have moved
  (let ([t (parameterize ([collect-request-handler void])
             (let ([h4a (make-eqv-hashtable 32)]
                   [k* (map list (make-list 100))])
               (for-each (lambda (x) (hashtable-set! h4a x x)) k*)
               (collect)
              ; create copy after collection but before otherwise touching h4a
               (let ([h4b (hashtable-copy h4a #t)])
                 (andmap
                   (lambda (k) (eqv? (hashtable-ref h4b k #f) k))
                   k*))))])
    (collect)
    t)

 ; test for proper shrinkage
  (equal?
    (let ([ht (make-eqv-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (call-with-values (lambda () (#%$hashtable-veclen ht)) cons))
    '(32 . 32))

 (begin
  (hashtable-set! h +nan.0 'nan)
  #t)
 (eq? 'nan (hashtable-ref h (abs +nan.0) #f))
)

(mat weak-eqv-hashtable
  (begin
    (define ka (list 'a))
    (define kb (list 'b))
    (define kc (list 'c))
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    (define km -5.75)
    (define kn 17)
    (define ko (+ (most-positive-fixnum) 5))
    #t)
  (begin
    (define h (make-weak-eqv-hashtable 32))
    (and (hashtable? h)
         (not (eq-hashtable? h))
         (hashtable-mutable? h)
         (hashtable-weak? h)))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eqv?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h ka 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #f #f #f #f #f))
  (eqv? (hashtable-set! h kb 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #f #f #f #f))
  (eqv? (hashtable-set! h kc 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #f #f #f))
  (eqv? (hashtable-set! h km 'mval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #f #f))
  (eqv? (hashtable-set! h kn 'nval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #f))
  (eqv? (hashtable-set! h ko 'oval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #t))
  (equal? (hashtable-size h) 6)
  (equal-entries? h `#((a) (b) (c) -5.75 17 ,ko) '#(aval bval cval mval nval oval))
  #;(same-elements?
    (list->vector (hashtable-map h cons))
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  (eq? (hashtable-ref h ka 1) 'aval)
  (eq? (hashtable-ref h kb #f) 'bval)
  (eq? (hashtable-ref h kc 'nope) 'cval)
  (eq? (hashtable-ref h (+ 2 -7.75) 'ugh) 'mval)
  (eq? (hashtable-ref h (/ 34 2) 'ugh) 'nval)
  (eq? (hashtable-ref h (+ (most-positive-fixnum) 7 -2) 'ugh) 'oval)
  (eqv? (hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (hashtable-mutable? h2)
         (hashtable-weak? h2)))
  (eq? (hashtable-hash-function h2) #f)
  (eq? (hashtable-equivalence-function h2) eqv?)
  (equal? (hashtable-size h2) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h ka 1)
      (hashtable-ref h kb #f)
      (hashtable-ref h kc 'nope)
      (hashtable-ref h km 'nope)
      (hashtable-ref h kn 'nope)
      (hashtable-ref h ko 'nope))
   '(0 1 #f nope nope nope nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 ka 1)
      (hashtable-ref h2 kb #f)
      (hashtable-ref h2 kc 'nope)
      (hashtable-ref h2 (- (+ km 1) 1) 'nope)
      (hashtable-ref h2 (- (+ kn 1) 1) 'nope)
      (hashtable-ref h2 (- (+ ko 1) 1) 'nope))
    '(5 aval #f cval mval nval oval))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 18)
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (equal-entries? h '#() '#())
  (equal? (hashtable-ref h ky #f) #f)
  (eqv?
    (hashtable-set! h ky 'toad)
    (void))
  (equal? (hashtable-ref h ky #f) 'toad)
  (equal? (hashtable-ref h kz #f) #f)
  (eqv?
    (hashtable-update! h kz list 'frog)
    (void))
  (equal? (hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (hashtable-ref h kz #f) 'toad))
  (eqv? (hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (not (hashtable-mutable? h3))
         (hashtable-weak? h3)))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal-entries? h3 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal?
    (begin
      (set! ka (void))
      (set! km (void))
      (set! kn (void))
      (set! ko (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(2 2))
  (equal-entries? h2 `#((c) 17) '#(cval nval))
  (equal-entries? h3 `#((c) 17) '#(cval nval))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    2)
  (equal-entries? h2 `#((c) 17) '#(cval nval))

 ; test for proper shrinkage
  (equal?
    (let ([ht (make-weak-eqv-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (call-with-values (lambda () (#%$hashtable-veclen ht)) cons))
    '(32 . 32))

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let ([ht (make-weak-eqv-hashtable 32)])
      (hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (hashtable-delete! ht 'a)
      (list (hashtable-size ht)
            (let-values ([(n1 n2) (#%$hashtable-veclen ht)])
              (= n1 n2 32))))
    '(0 #t))
  )

(mat ephemeron-eqv-hashtable
  (begin
    (define ka (list 'a))
    (define kb (list 'b))
    (define kc (list 'c))
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    (define km -5.75)
    (define kn 17)
    (define ko (+ (most-positive-fixnum) 5))
    #t)
  (begin
    (define h (make-ephemeron-eqv-hashtable 32))
    (and (hashtable? h)
         (not (eq-hashtable? h))
         (hashtable-mutable? h)
         (hashtable-ephemeron? h)))
  (eq? (hashtable-hash-function h) #f)
  (eq? (hashtable-equivalence-function h) eqv?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h ka 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #f #f #f #f #f))
  (eqv? (hashtable-set! h kb 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #f #f #f #f))
  (eqv? (hashtable-set! h kc 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #f #f #f))
  (eqv? (hashtable-set! h km 'mval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #f #f))
  (eqv? (hashtable-set! h kn 'nval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #f))
  (eqv? (hashtable-set! h ko 'oval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #t))
  (equal? (hashtable-size h) 6)
  (equal-entries? h `#((a) (b) (c) -5.75 17 ,ko) '#(aval bval cval mval nval oval))
  #;(same-elements?
    (list->vector (hashtable-map h cons))
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  (eq? (hashtable-ref h ka 1) 'aval)
  (eq? (hashtable-ref h kb #f) 'bval)
  (eq? (hashtable-ref h kc 'nope) 'cval)
  (eq? (hashtable-ref h (+ 2 -7.75) 'ugh) 'mval)
  (eq? (hashtable-ref h (/ 34 2) 'ugh) 'nval)
  (eq? (hashtable-ref h (+ (most-positive-fixnum) 7 -2) 'ugh) 'oval)
  (eqv? (hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (hashtable-mutable? h2)
         (hashtable-ephemeron? h2)))
  (eq? (hashtable-hash-function h2) #f)
  (eq? (hashtable-equivalence-function h2) eqv?)
  (equal? (hashtable-size h2) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h ka 1)
      (hashtable-ref h kb #f)
      (hashtable-ref h kc 'nope)
      (hashtable-ref h km 'nope)
      (hashtable-ref h kn 'nope)
      (hashtable-ref h ko 'nope))
   '(0 1 #f nope nope nope nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 ka 1)
      (hashtable-ref h2 kb #f)
      (hashtable-ref h2 kc 'nope)
      (hashtable-ref h2 (- (+ km 1) 1) 'nope)
      (hashtable-ref h2 (- (+ kn 1) 1) 'nope)
      (hashtable-ref h2 (- (+ ko 1) 1) 'nope))
    '(5 aval #f cval mval nval oval))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 18)
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (equal-entries? h '#() '#())
  (equal? (hashtable-ref h ky #f) #f)
  (eqv?
    (hashtable-set! h ky 'toad)
    (void))
  (equal? (hashtable-ref h ky #f) 'toad)
  (equal? (hashtable-ref h kz #f) #f)
  (eqv?
    (hashtable-update! h kz list 'frog)
    (void))
  (equal? (hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (hashtable-ref h kz #f) 'toad))
  (eqv? (hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (not (hashtable-mutable? h3))
         (hashtable-ephemeron? h3)))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal-entries? h3 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal?
    (begin
      (set! ka (void))
      (set! km (void))
      (set! kn (void))
      (set! ko (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(2 2))
  (equal-entries? h2 `#((c) 17) '#(cval nval))
  (equal-entries? h3 `#((c) 17) '#(cval nval))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    2)
  (equal-entries? h2 `#((c) 17) '#(cval nval))

 ; test for proper shrinkage
  (equal?
    (let ([ht (make-ephemeron-eqv-hashtable 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (call-with-values (lambda () (#%$hashtable-veclen ht)) cons))
    '(32 . 32))

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let ([ht (make-ephemeron-eqv-hashtable 32)])
      (hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (hashtable-delete! ht 'a)
      (list (hashtable-size ht)
            (let-values ([(n1 n2) (#%$hashtable-veclen ht)])
              (= n1 n2 32))))
    '(0 #t))
)

(mat eqv-hashtable-cell
  (let ()
    (define-record fribble (x))
    (define random-object
      (lambda (x)
        (case (random 9)
          [(0) (cons 'a 3.4)]
          [(1) (vector 'c)]
          [(2) (string #\a #\b)]
          [(3) (make-fribble 'q)]
          [(4) (gensym)]
          [(5) (open-output-string)]
          [(6) (fxvector 15 55)]
          [(7) (lambda () x)]
          [(8) (flvector 15.0 55.0)]
          [else (box 'top)])))
    (let ([ls1 (let f ([n 10000])
                 (if (fx= n 0)
                     '()
                     (cons
                       (cons (random-object 4) (random-object 7))
                       (f (fx- n 1)))))]
          [ht (make-eqv-hashtable)]
          [wht (make-weak-eqv-hashtable)]
          [eht (make-ephemeron-eqv-hashtable)])
      (let ([ls2 (map (lambda (a1) (hashtable-cell ht (car a1) (cdr a1))) ls1)]
            [ls3 (map (lambda (a1) (hashtable-cell wht (car a1) (cdr a1))) ls1)]
            [ls4 (map (lambda (a1) (hashtable-cell eht (car a1) (cdr a1))) ls1)])
        (let ([ls2* (map (lambda (a1) (hashtable-ref-cell ht (car a1))) ls1)]
              [ls3* (map (lambda (a1) (hashtable-ref-cell wht (car a1))) ls1)]
              [ls4* (map (lambda (a1) (hashtable-ref-cell eht (car a1))) ls1)])
          (unless (andmap (lambda (a2 a2* a3 a3* a4 a4*)
                            (and (eq? a2 a2*)
                                 (eq? a3 a3*)
                                 (eq? a4 a4*)))
                          ls2 ls2* ls3 ls3* ls4 ls4*)
            (errorf #f "hashtable-ref-cell and hashtable-cell do not retrieve the same cells")))
        (unless (andmap (lambda (a1 a2 a3 a4)
                          (and (eqv? (car a1) (car a2))
                               (eqv? (car a2) (car a3))
                               (eqv? (car a2) (car a4))))
                        ls1 ls2 ls3 ls4)
          (errorf #f "keys are not eqv"))
        (unless (andmap (lambda (a1 a2 a3 a4)
                          (and (eqv? (cdr a1) (cdr a2))
                               (eqv? (cdr a2) (cdr a3))
                               (eqv? (cdr a2) (cdr a4))))
                        ls1 ls2 ls3 ls4)
          (errorf #f "values are not eqv"))
        (for-each (lambda (a1)
                    (let ([o (random-object 3)])
                      ;; Value refers to key:
                      (hashtable-set! eht o (list o (car a1)))))
                  ls1)
        (for-each
          (lambda (a1)
            (when (fx< (random 10) 5)
              (set-car! a1 #f)))
          ls1)
        (parameterize ([collect-maximum-generation-threshold-factor 0])
          (let loop ([i (min (expt (collect-generation-radix) (collect-maximum-generation)) 1000)])
            (unless (fx= i 0)
              (collect)
              (unless (andmap (lambda (a2 a3 a4) (and (eqv? (car a2) (car a3)) (eqv? (car a2) (car a4))))
                              ls2 ls3 ls4)
                (errorf #f "a2/a3/a4 keys not eqv after collection"))
              (unless (and (andmap (lambda (a3) (not (bwp-object? (car a3)))) ls3)
                           (andmap (lambda (a4) (not (bwp-object? (car a4)))) ls4))
                (errorf #f "keys have been bwp'd"))
              (loop (fx- i 1)))))
        (for-each
          (lambda (a2)
            (hashtable-delete! ht (car a2))
            (set-car! a2 #f))
          ls2)
        (unless (and (equal? (hashtable-keys ht) '#())
                     (equal? (hashtable-values ht) '#())
                     (zero? (hashtable-size ht)))
          (errorf #f "ht has not been cleared out"))
        (parameterize ([collect-maximum-generation-threshold-factor 0])
          (let loop ([i (min (expt (collect-generation-radix) (collect-maximum-generation)) 1000)])
            (unless (fx= i 0)
              (collect)
              (unless (andmap (lambda (a1 a3 a4)
                                (or (not (car a1))
                                    (and (eqv? (car a1) (car a3))
                                         (eqv? (car a1) (car a4)))))
                              ls1 ls3 ls4)
                (errorf #f "a1/a3/a4 keys not eqv after collection"))
              (loop (fx- i 1)))))
        (for-each
          (lambda (a1 a3 a4)
            (unless (or (car a1)
                        (and (bwp-object? (car a3))
                             (bwp-object? (car a4))))
              (errorf #f "~s has not been bwp'd I" (car a3))))
          ls1 ls3 ls4)
        (for-each (lambda (a1) (set-car! a1 #f)) ls1)
        (collect (collect-maximum-generation))
        (unless (and (andmap (lambda (a3) (bwp-object? (car a3))) ls3)
                     (andmap (lambda (a4) (bwp-object? (car a4))) ls4))
          (errorf #f "keys have not been bwp'd II"))
        (unless (and (equal? (hashtable-keys wht) '#())
                     (equal? (hashtable-values wht) '#())
                     (zero? (hashtable-size wht)))
          (errorf #f "wht has not been cleared out"))
        (unless (and (equal? (hashtable-keys eht) '#())
                     (equal? (hashtable-values eht) '#())
                     (zero? (hashtable-size eht)))
          (errorf #f "eht has not been cleared out"))))
    #t)
  )

(mat eqv-strange
  (begin
    (define $ht (make-eqv-hashtable))
    (define $wht (make-weak-eqv-hashtable))
    (define $eht (make-weak-eqv-hashtable))
    (and (hashtable? $ht)
         (hashtable? $wht)
         (hashtable? $eht)))
  (eqv? (hashtable-set! $ht #f 75) (void))
  (eqv? (hashtable-ref $ht #f 80) 75)
  (eqv? (hashtable-set! $wht #f 75) (void))
  (eqv? (hashtable-ref $wht #f 80) 75)
  (eqv? (hashtable-set! $eht #f 75) (void))
  (eqv? (hashtable-ref $eht #f 80) 75)
  (eqv? (hashtable-set! $ht #!bwp "hello") (void))
  (equal? (hashtable-ref $ht #!bwp "goodbye") "hello")
  (eqv? (hashtable-set! $wht #!bwp "hello") (void))
  (eqv? (hashtable-set! $eht #!bwp "hello") (void))
  (and (member (hashtable-ref $wht #!bwp "goodbye") '("hello" "goodbye")) #t)
  (and (member (hashtable-ref $eht #!bwp "goodbye") '("hello" "goodbye")) #t)
 ; make sure that association isn't added before procedure is called
  (equal?
    (begin
      (hashtable-update! $ht 'cupie
        (lambda (x) (hashtable-ref $ht 'cupie (cons 'barbie x)))
        'doll)
      (hashtable-ref $ht 'cupie 'oops))
   '(barbie . doll))
  (equal?
    (begin
      (hashtable-update! $wht 'cupie
        (lambda (x) (hashtable-ref $wht 'cupie (cons 'barbie x)))
        'doll)
      (hashtable-ref $wht 'cupie 'oops))
   '(barbie . doll))
  (equal?
    (begin
      (hashtable-update! $eht 'cupie
        (lambda (x) (hashtable-ref $eht 'cupie (cons 'barbie x)))
        'doll)
      (hashtable-ref $eht 'cupie 'oops))
   '(barbie . doll))
)

(mat eqv-hashtable-stress
 ; stress tests
  (let () ; nonweak
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-eqv-hashtable 4))
    (let ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (= (hashtable-size ht) (- n (length drop)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (hashtable-ref ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eqv? (hashtable-ref ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (hashtable-set! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (hashtable-delete! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (hashtable-delete! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))

  (let () ; weak
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-weak-eqv-hashtable 4))
    (let ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (<= (hashtable-size ht) (- n (length drop)))
                 (begin
                   (collect (collect-maximum-generation))
                   (= (hashtable-size ht) (length keep)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (hashtable-ref ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eqv? (hashtable-ref ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (hashtable-set! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (hashtable-delete! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (hashtable-delete! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))

  (let () ; ephemeron
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-ephemeron-eqv-hashtable 4))
    (let ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (<= (hashtable-size ht) (- n (length drop)))
                 (begin
                   (collect (collect-maximum-generation))
                   (= (hashtable-size ht) (length keep)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (hashtable-ref ht k #f))
                             (cond
                               [(string? k) k]
                               [(pair? k) (car k)]
                               [(vector? k) (vector-ref k 0)])))
                         keep)
                 (andmap (lambda (k) (eqv? (hashtable-ref ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (case (pick '(string pair vector))
                         [(string) s]
                         [(pair) (list s)]
                         [(vector) (vector s)])])
                (hashtable-set! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (hashtable-delete! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (hashtable-delete! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))

)

(mat symbol-hashtable
  (let ([ht (make-hashtable symbol-hash eq?)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) eq?)))
  (let ([ht (make-hashtable symbol-hash eqv?)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) eqv?)))
  (let ([ht (make-hashtable symbol-hash equal?)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) equal?)))
  (let ([ht (make-hashtable symbol-hash symbol=?)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) symbol=?)))
  (let ([ht (make-hashtable symbol-hash eq? 17)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) eq?)))
  (let ([ht (make-hashtable symbol-hash eqv? 17)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) eqv?)))
  (let ([ht (make-hashtable symbol-hash equal? 17)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) equal?)))
  (let ([ht (make-hashtable symbol-hash symbol=? 17)])
    (and (symbol-hashtable? ht) (eq? (hashtable-equivalence-function ht) symbol=?)))
  (begin
    (define h (make-hashtable symbol-hash eq? 32))
    (and (hashtable? h)
         (symbol-hashtable? h)
         (hashtable-mutable? h)
         (not (eq-hashtable? h))
         (not (hashtable-weak? h))
         (not (hashtable-ephemeron? h))))
  (eq? (hashtable-hash-function h) symbol-hash)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h 'a 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 'b)
       (hashtable-contains? h 'c))
    '(#t #f #f))
  (eqv? (hashtable-set! h 'b 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 'b)
       (hashtable-contains? h 'c))
    '(#t #t #f))
  (eqv? (hashtable-set! h 'c 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h 'a)
       (hashtable-contains? h 'b)
       (hashtable-contains? h 'c))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (equal-entries? h '#(b c a) '#(bval cval aval))
  #;(same-elements? (list->vector (hashtable-map h cons)) '#((a . aval) (b . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (b . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (b . bval) (c . cval)))
  (equal? (hashtable-ref h 'a 1) 'aval)
  (equal? (hashtable-ref h 'b #f) 'bval)
  (equal? (hashtable-ref h 'c 'nope) 'cval)
  (eqv? (hashtable-delete! h 'b) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#(a c) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (symbol-hashtable? h2)
         (hashtable-mutable? h2)
         (not (hashtable-weak? h2))
         (not (hashtable-ephemeron? h2))
         (not (eq-hashtable? h2))))
  (eq? (hashtable-hash-function h2) symbol-hash)
  (eq? (hashtable-equivalence-function h2) eq?)
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h 'a 1)
      (hashtable-ref h 'b #f)
      (hashtable-ref h 'c 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 'a 1)
      (hashtable-ref h2 'b #f)
      (hashtable-ref h2 'c 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv?
    (hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h 'q #f) 18)
  (eqv?
    (hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h 'q #f) 19)
  (equal? (hashtable-size h) 1)
 ; test hashtable-copy when some keys may have moved
 ; symbol hashes don't change, but keeping test adapted from eq-hashtable mats anyway
  (let ([t (parameterize ([collect-request-handler void])
             (let ([h4a (make-hashtable symbol-hash eqv? 32)]
                   [k* (list-head (oblist) 100)])
               (for-each (lambda (x) (hashtable-set! h4a x x)) k*)
               (collect)
              ; create copy after collection but before otherwise touching h4a
               (let ([h4b (hashtable-copy h4a #t)])
                 (andmap
                   (lambda (k) (eq? (hashtable-ref h4b k #f) k))
                   k*))))])
    (collect)
    t)
 ; test for proper shrinkage
  (eqv?
    (let ([ht (make-hashtable symbol-hash equal? 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (list-head (oblist) 1000)) (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (#%$hashtable-veclen ht))
    32)
)

(mat $symbol-hashtable
  (begin
    (define h (make-hashtable symbol-hash eq? 32))
    (and (hashtable? h)
         (symbol-hashtable? h)
         (hashtable-mutable? h)
         (not (eq-hashtable? h))
         (not (hashtable-weak? h))
         (not (hashtable-ephemeron? h))))
  (eq? (hashtable-hash-function h) symbol-hash)
  (eq? (hashtable-equivalence-function h) eq?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (symbol-hashtable-set! h 'a 'aval) (void))
  (equal?
    (list
       (symbol-hashtable-contains? h 'a)
       (symbol-hashtable-contains? h 'b)
       (symbol-hashtable-contains? h 'c))
    '(#t #f #f))
  (eqv? (symbol-hashtable-set! h 'b 'bval) (void))
  (equal?
    (list
       (symbol-hashtable-contains? h 'a)
       (symbol-hashtable-contains? h 'b)
       (symbol-hashtable-contains? h 'c))
    '(#t #t #f))
  (eqv? (symbol-hashtable-set! h 'c 'cval) (void))
  (equal?
    (list
       (symbol-hashtable-contains? h 'a)
       (symbol-hashtable-contains? h 'b)
       (symbol-hashtable-contains? h 'c))
    '(#t #t #t))
  (equal? (hashtable-size h) 3)
  (equal-entries? h '#(b c a) '#(bval cval aval))
  #;(same-elements? (list->vector (hashtable-map h cons)) '#((a . aval) (b . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (b . bval) (c . cval)))
  #;(same-elements?
    (let ([v (make-vector 3)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    '#((a . aval) (b . bval) (c . cval)))
  (equal? (symbol-hashtable-ref h 'a 1) 'aval)
  (equal? (symbol-hashtable-ref h 'b #f) 'bval)
  (equal? (symbol-hashtable-ref h 'c 'nope) 'cval)
  (eqv? (symbol-hashtable-delete! h 'b) (void))
  (equal? (hashtable-size h) 2)
  (equal-entries? h '#(a c) '#(aval cval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (symbol-hashtable? h2)
         (hashtable-mutable? h2)
         (not (hashtable-weak? h2))
         (not (hashtable-ephemeron? h2))
         (not (eq-hashtable? h2))))
  (eq? (hashtable-hash-function h2) symbol-hash)
  (eq? (hashtable-equivalence-function h2) eq?)
  (equal? (hashtable-size h2) 2)
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (symbol-hashtable-ref h 'a 1)
      (symbol-hashtable-ref h 'b #f)
      (symbol-hashtable-ref h 'c 'nope))
   '(0 1 #f nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (symbol-hashtable-ref h2 'a 1)
      (symbol-hashtable-ref h2 'b #f)
      (symbol-hashtable-ref h2 'c 'nope))
    '(2 aval #f cval))
  (equal-entries? h2 '#(a c) '#(aval cval))
  (eqv?
    (symbol-hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (symbol-hashtable-ref h 'q #f) 18)
  (eqv?
    (symbol-hashtable-update! h 'q
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (symbol-hashtable-ref h 'q #f) 19)
  (equal? (hashtable-size h) 1)
  (let ([g (gensym)] [s "feisty"])
    (let ([a (symbol-hashtable-cell h g s)])
      (and (pair? a)
           (eq? (car a) g)
           (eq? (cdr a) s)
           (eq? a (symbol-hashtable-ref-cell h g))
           (begin
             (hashtable-set! h g 'feisty)
             (eq? (cdr a) 'feisty))
           (begin
             (set-cdr! a (list "feisty"))
             (equal? (hashtable-ref h g #f) '("feisty"))))))
  (eq? (symbol-hashtable-ref-cell h (gensym)) #f)
 ; test hashtable-copy when some keys may have moved
 ; symbol hashes don't change, but keeping test adapted from eq-hashtable mats anyway
  (let ([t (parameterize ([collect-request-handler void])
             (let ([h4a (make-hashtable symbol-hash eqv? 32)]
                   [k* (list-head (oblist) 100)])
               (for-each (lambda (x) (symbol-hashtable-set! h4a x x)) k*)
               (collect)
              ; create copy after collection but before otherwise touching h4a
               (let ([h4b (hashtable-copy h4a #t)])
                 (andmap
                   (lambda (k) (eq? (symbol-hashtable-ref h4b k #f) k))
                   k*))))])
    (collect)
    t)
 ; test for proper shrinkage
  (eqv?
    (let ([ht (make-hashtable symbol-hash equal? 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (symbol-hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (list-head (oblist) 1000)) (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (symbol-hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (#%$hashtable-veclen ht))
    32)
)

(mat symbol-hashtable-stress
 ; stress tests
  (let () ; nonweak
    (define pick
      (lambda (ls)
        (list-ref ls (random (length ls)))))
    (define ht (make-hashtable symbol-hash eq? 4))
    (let ([ls (remq '|| (oblist))] [n (if (pb?) 5000 50000)])
      (let f ([i 0] [keep '()] [drop '()])
        (if (= i n)
            (and (= (hashtable-size ht) (- n (length drop)))
                 (andmap (lambda (k)
                           (string=?
                             (symbol->string (hashtable-ref ht k #f))
                             (symbol->string k)))
                         keep)
                 (andmap (lambda (k) (eq? (hashtable-ref ht k 'no) 'no))
                         drop))
            (let* ([x (pick ls)] [s (string-copy (symbol->string x))])
              (let ([k (gensym s)])
                (hashtable-set! ht k x)
                (let ([keep (if (= (modulo i 5) 0) (cons k keep) keep)])
                  (if (= (modulo i 17) 5)
                      (let ([k (pick keep)])
                        (hashtable-delete! ht k)
                        (let ([drop (cons k drop)])
                          (when (= (random 5) 3)
                            (hashtable-delete! ht (pick drop)))
                          (f (+ i 1) (remq k keep) drop)))
                      (f (+ i 1) keep drop)))))))))
)

(mat generic-hashtable
  (begin
    (define $ght-keys1 '#(a b c d e f g))
    (define $ght-vals1 '#(1 3 5 7 9 11 13))
    (define $ght (make-hashtable equal-hash equal? 8))
    (vector-for-each
      (lambda (x i) (hashtable-set! $ght x i))
      $ght-keys1
      $ght-vals1)
    (hashtable? $ght))
  (not (eq-hashtable? $ght))
  (eq? (hashtable-hash-function $ght) equal-hash)
  (eq? (hashtable-equivalence-function $ght) equal?)
  (eq? (hashtable-mutable? $ght) #t)
  (not (hashtable-weak? $ght))
  (not (hashtable-ephemeron? $ght))
  (eqv? (hashtable-size $ght) (vector-length $ght-keys1))
  (eqv? (#%$hashtable-veclen $ght) 8)
  (same-elements? (hashtable-keys $ght) $ght-keys1)
  (same-elements? (hashtable-values $ght) $ght-vals1)
  (equal-entries? $ght $ght-keys1 $ght-vals1)
  (same-elements? (hashtable-cells $ght) (vector-map cons $ght-keys1 $ght-vals1))
  (begin
    (define $ght-keys2 '#((a . b) (1 . 2) 3/4 3.4 3.5 1e23 #e1e50 1+1i 3+3.2i -15 #e1e-50 #1=(a . #1#) (#2=(#2# b c))))
    (define $ght-vals2 '#(a b c d e f g h i j k l m))
    (vector-for-each
      (lambda (x i) (hashtable-set! $ght x i))
      $ght-keys2
      $ght-vals2)
    (eq? (hashtable-size $ght) (+ (vector-length $ght-keys1) (vector-length $ght-keys2))))
  (> (#%$hashtable-veclen $ght) 8)
  (equal-entries? $ght ($vector-append $ght-keys1 $ght-keys2) ($vector-append $ght-vals1 $ght-vals2))
  #;(same-elements?
    (list->vector (hashtable-map $ght cons))
    (vector-map cons ($vector-append $ght-keys1 $ght-keys2) ($vector-append $ght-vals1 $ght-vals2)))
  #;(same-elements?
    (let ([v (make-vector (+ (vector-length $ght-keys1) (vector-length $ght-keys2)))] [i 0])
      (hashtable-for-each $ght (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    (vector-map cons ($vector-append $ght-keys1 $ght-keys2) ($vector-append $ght-vals1 $ght-vals2)))
  #;(same-elements?
    (let ([v (make-vector (+ (vector-length $ght-keys1) (vector-length $ght-keys2)))] [i 0])
      (hashtable-for-each-cell $ght (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    (vector-map cons ($vector-append $ght-keys1 $ght-keys2) ($vector-append $ght-vals1 $ght-vals2)))
  ($vector-andmap
    (lambda (k v) (equal? (hashtable-ref $ght k #f) v))
    $ght-keys1
    $ght-vals1)
  ($vector-andmap
    (lambda (k v) (equal? (hashtable-ref $ght k #f) v))
    $ght-keys2
    $ght-vals2)
  ($vector-andmap
    (lambda (k v) (equal? (hashtable-ref $ght k #f) v))
    '#((a . b) (1 . 2) 3/4 3.4 3.5 1e23 #e1e50 1+1i 3+3.2i -15 #e1e-50 #3=(a . #3#) (#4=(#4# b c)))
    $ght-vals2)
  ($vector-andmap
    (lambda (k) (hashtable-contains? $ght k))
    $ght-keys1)
  ($vector-andmap
    (lambda (k) (hashtable-contains? $ght k))
    $ght-keys2)
  (not (hashtable-contains? $ght '(not a key)))
  (eq? (hashtable-ref $ght '(not a key) 'not-a-key) 'not-a-key)
  (begin
    (define $ght2 (hashtable-copy $ght))
    (and (hashtable? $ght2)
         (not (hashtable-mutable? $ght2))
         (not (hashtable-weak? $ght2))
         (not (hashtable-ephemeron? $ght2))))
  (eq? (hashtable-hash-function $ght) equal-hash)
  (eq? (hashtable-equivalence-function $ght) equal?)
  (begin
    (define $ght3 (hashtable-copy $ght #t))
    (and (hashtable? $ght3)
         (hashtable-mutable? $ght3)
         (not (hashtable-weak? $ght3))
         (not (hashtable-ephemeron? $ght3))))
  (eq? (hashtable-hash-function $ght) equal-hash)
  (eq? (hashtable-equivalence-function $ght) equal?)
  (begin
    (vector-for-each
      (lambda (k) (hashtable-delete! $ght k))
      $ght-keys1)
    #t)
  (equal-entries? $ght $ght-keys2 $ght-vals2)
  (eqv? (hashtable-size $ght) (vector-length $ght-keys2))
  (begin
    (vector-for-each
      (lambda (k) (hashtable-delete! $ght k))
      $ght-keys2)
    #t)
  (equal-entries? $ght '#() '#())
  (eqv? (hashtable-size $ght) 0)
  (eqv? (#%$hashtable-veclen $ght) 8)
 ; make sure copies are unaffected by deletions
  (eq? (hashtable-size $ght2) (+ (vector-length $ght-keys1) (vector-length $ght-keys2)))
  (equal-entries? $ght2 ($vector-append $ght-keys1 $ght-keys2) ($vector-append $ght-vals1 $ght-vals2))
  (eq? (hashtable-size $ght3) (+ (vector-length $ght-keys1) (vector-length $ght-keys2)))
  (equal-entries? $ght3 ($vector-append $ght-keys1 $ght-keys2) ($vector-append $ght-vals1 $ght-vals2))
  (begin
    (hashtable-clear! $ght3)
    (and
      (eqv? (hashtable-size $ght3) 0)
      (eqv? (hashtable-size $ght2) (+ (vector-length $ght-keys1) (vector-length $ght-keys2)))))
  (error? ; not mutable
    (hashtable-clear! $ght2))
  (error? ; not mutable
    (hashtable-delete! $ght2 (vector-ref $ght-keys2 0)))
  (error? ; not mutable
    (hashtable-update! $ght2 (vector-ref $ght-keys2 0)
      (lambda (x) (cons x x))
      'oops))
  (error? ; not mutable
    (hashtable-update! $ght2 '(not a key)
      (lambda (x) (cons x x))
      'oops))
  (eqv?
    (hashtable-update! $ght3 '(a . b)
      (lambda (x) (+ x 15))
      17)
    (void))
  (eqv?
    (hashtable-update! $ght3 '(a . b)
      (lambda (x) (+ x 29))
      17)
    (void))
  (eqv?
    (hashtable-update! $ght3 1e23
      (lambda (x) (- x 5))
      19)
    (void))
  (equal?
    (let ([a (hashtable-cell $ght3 '(a . b) 17)])
      (set-cdr! a (+ (cdr a) 100))
      a)
    '((a . b) . 161))
  (equal?
    (let ([a (hashtable-cell $ght3 #vu8(1 2 3) 'bv)])
      (set-cdr! a (cons (cdr a) 'vb))
      a)
    '(#vu8(1 2 3) . (bv . vb)))
  (eq? (hashtable-cell $ght3 #vu8(1 2 3) 'bv)
       (hashtable-ref-cell $ght3 #vu8(1 2 3)))
  (eq? (hashtable-ref-cell $ght3 (gensym)) #f)
  (equal-entries? $ght3 '#((a . b) 1e23 #vu8(1 2 3)) '#(161 14 (bv . vb)))
  (let () ; carl's test program, with a few additions
    (define cov:prof-hash
      (lambda (V)
        (* (vector-ref V 0) (vector-ref V 1) (vector-ref V 2))))
    (define cov:prof-equal?
      (lambda (V W)
        (let ((rv (and (= (vector-ref V 0) (vector-ref W 0))
                       (= (vector-ref V 1) (vector-ref W 1))
                       (= (vector-ref V 2) (vector-ref W 2)))))
          rv)))
    (define make-random-vector-key
      (lambda ()
        (vector (random 20000) (random 100) (random 1000))))
    (define test-hash
      (lambda (n)
        (let ([ht (make-hashtable cov:prof-hash cov:prof-equal?)])
          (let loop ([i 0])
            (let ([str (make-random-vector-key)])
              (hashtable-set! ht str i)
              (hashtable-update! ht str (lambda (x) (* x 2)) -1)
              (let ([a (hashtable-cell ht str 'a)]) (set-cdr! a (- (cdr a))))
              (cond
                [(= i n) (= (hashtable-size ht) 1000)]
                [(and (hashtable-contains? ht str)
                      (= (hashtable-ref ht str #f) (* i -2)))
                 (when (= (hashtable-size ht) 1000)
                   (hashtable-delete! ht str))
                 (loop (+ i 1))]
                [else (errorf 'test-hash "hashtable failure for key ~s" str)]))))))
    (test-hash 100000))
)

(mat generic-hashtable-arguments
  (error? ; wrong argument count
    (make-weak-hashtable))
  (error? ; wrong argument count
    (make-weak-hashtable equal-hash))
  (error? ; wrong argument count
    (make-weak-hashtable equal-hash equal? 45 53))
  (error? ; not a procedure
    (make-weak-hashtable 'a equal? 45))
  (error? ; not a procedure
    (make-weak-hashtable equal-hash 'a 45))
  (error? ; invalid size
    (make-weak-hashtable equal-hash equal? 'a))
  (error? ; invalid size
    (make-weak-hashtable equal-hash equal? -45))
  (error? ; invalid size
    (make-weak-hashtable equal-hash equal? 45.0))
  (error? ; wrong argument count
    (make-ephemeron-hashtable))
  (error? ; wrong argument count
    (make-ephemeron-hashtable equal-hash))
  (error? ; wrong argument count
    (make-ephemeron-hashtable equal-hash equal? 45 53))
  (error? ; not a procedure
    (make-ephemeron-hashtable 'a equal? 45))
  (error? ; not a procedure
    (make-ephemeron-hashtable equal-hash 'a 45))
  (error? ; invalid size
    (make-ephemeron-hashtable equal-hash equal? 'a))
  (error? ; invalid size
    (make-ephemeron-hashtable equal-hash equal? -45))
  (error? ; invalid size
    (make-ephemeron-hashtable equal-hash equal? 45.0)))

(mat weak-equal-hashtable
  (begin
    (define ka (list 'a))
    (define kb (list 'b))
    (define kc (list 'c))
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    (define km -5.75)
    (define kn 17)
    (define ko (+ (most-positive-fixnum) 5))
    #t)
  (begin
    (define h (make-weak-hashtable equal-hash equal? 32))
    (and (hashtable? h)
         (not (eq-hashtable? h))
         (hashtable-mutable? h)
         (hashtable-weak? h)))
  (eq? (hashtable-hash-function h) equal-hash)
  (eq? (hashtable-equivalence-function h) equal?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h ka 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #f #f #f #f #f))
  (eqv? (hashtable-set! h kb 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #f #f #f #f))
  (eqv? (hashtable-set! h kc 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #f #f #f))
  (eqv? (hashtable-set! h km 'mval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #f #f))
  (eqv? (hashtable-set! h kn 'nval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #f))
  (eqv? (hashtable-set! h ko 'oval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #t))
  (equal? (hashtable-size h) 6)
  (equal-entries? h `#((a) (b) (c) -5.75 17 ,ko) '#(aval bval cval mval nval oval))
  #;(same-elements?
    (list->vector (hashtable-map h cons))
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  (eq? (hashtable-ref h ka 1) 'aval)
  (eq? (hashtable-ref h kb #f) 'bval)
  (eq? (hashtable-ref h kc 'nope) 'cval)
  (eq? (hashtable-ref h (+ 2 -7.75) 'ugh) 'mval)
  (eq? (hashtable-ref h (/ 34 2) 'ugh) 'nval)
  (eq? (hashtable-ref h (+ (most-positive-fixnum) 7 -2) 'ugh) 'oval)
  (eqv? (hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (hashtable-mutable? h2)
         (hashtable-weak? h2)))
  (eq? (hashtable-hash-function h2) equal-hash)
  (eq? (hashtable-equivalence-function h2) equal?)
  (equal? (hashtable-size h2) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h ka 1)
      (hashtable-ref h kb #f)
      (hashtable-ref h kc 'nope)
      (hashtable-ref h km 'nope)
      (hashtable-ref h kn 'nope)
      (hashtable-ref h ko 'nope))
   '(0 1 #f nope nope nope nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 ka 1)
      (hashtable-ref h2 kb #f)
      (hashtable-ref h2 kc 'nope)
      (hashtable-ref h2 (- (+ km 1) 1) 'nope)
      (hashtable-ref h2 (- (+ kn 1) 1) 'nope)
      (hashtable-ref h2 (- (+ ko 1) 1) 'nope))
    '(5 aval #f cval mval nval oval))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 18)
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (equal-entries? h '#() '#())
  (equal? (hashtable-ref h ky #f) #f)
  (eqv?
    (hashtable-set! h ky 'toad)
    (void))
  (equal? (hashtable-ref h ky #f) 'toad)
  (equal? (hashtable-ref h kz #f) #f)
  (eqv?
    (hashtable-update! h kz list 'frog)
    (void))
  (equal? (hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (hashtable-ref h kz #f) 'toad))
  (eqv? (hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (not (hashtable-mutable? h3))
         (hashtable-weak? h3)))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal-entries? h3 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal?
    (begin
      (set! ka (void))
      (set! km (void))
      (set! kn (void))
      (set! ko (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(2 2))
  (equal-entries? h2 `#((c) 17) '#(cval nval))
  (equal-entries? h3 `#((c) 17) '#(cval nval))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    2)
  (equal-entries? h2 `#((c) 17) '#(cval nval))

 ; test for proper shrinkage
  (equal?
    (let ([ht (make-weak-hashtable equal-hash equal? 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (call-with-values (lambda () (#%$hashtable-veclen ht)) cons))
    '(32 . 32))

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let ([ht (make-weak-hashtable equal-hash equal? 32)])
      (hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (hashtable-delete! ht 'a)
      (list (hashtable-size ht)
            (let-values ([(n1 n2) (#%$hashtable-veclen ht)])
              (= n1 n2 32))))
    '(0 #t))
  )

(mat ephemeron-equal-hashtable
  (begin
    (define ka (list 'a))
    (define kb (list 'b))
    (define kc (list 'c))
    (define kq (list 'q))
    (define ky (list 'y))
    (define kz (list 'z))
    (define km -5.75)
    (define kn 17)
    (define ko (+ (most-positive-fixnum) 5))
    #t)
  (begin
    (define h (make-ephemeron-hashtable equal-hash equal? 32))
    (and (hashtable? h)
         (not (eq-hashtable? h))
         (hashtable-mutable? h)
         (hashtable-ephemeron? h)))
  (eq? (hashtable-hash-function h) equal-hash)
  (eq? (hashtable-equivalence-function h) equal?)
  (equal? (hashtable-size h) 0)
  (same-elements? (hashtable-keys h) '#())
  (same-elements? (hashtable-values h) '#())
  (equal-entries? h '#() '#())
  (same-elements? (hashtable-cells h) '#())
  (same-elements? (hashtable-cells h 0) '#())
  (same-elements? (hashtable-cells h 10) '#())
  (eqv? (hashtable-set! h ka 'aval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #f #f #f #f #f))
  (eqv? (hashtable-set! h kb 'bval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #f #f #f #f))
  (eqv? (hashtable-set! h kc 'cval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #f #f #f))
  (eqv? (hashtable-set! h km 'mval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #f #f))
  (eqv? (hashtable-set! h kn 'nval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #f))
  (eqv? (hashtable-set! h ko 'oval) (void))
  (equal?
    (list
       (hashtable-contains? h ka)
       (hashtable-contains? h kb)
       (hashtable-contains? h kc)
       (hashtable-contains? h km)
       (hashtable-contains? h kn)
       (hashtable-contains? h ko))
    '(#t #t #t #t #t #t))
  (equal? (hashtable-size h) 6)
  (equal-entries? h `#((a) (b) (c) -5.75 17 ,ko) '#(aval bval cval mval nval oval))
  #;(same-elements?
    (list->vector (hashtable-map h cons))
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each h (lambda (key val) (vector-set! v i (cons key val)) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  #;(same-elements?
    (let ([v (make-vector 6)] [i 0])
      (hashtable-for-each-cell h (lambda (a) (vector-set! v i a) (set! i (fx+ i 1))))
      v)
    `#(((a) . aval) ((b) . bval) ((c) . cval) (-5.75 . mval) (17 . nval) (,ko . oval)))
  (eq? (hashtable-ref h ka 1) 'aval)
  (eq? (hashtable-ref h kb #f) 'bval)
  (eq? (hashtable-ref h kc 'nope) 'cval)
  (eq? (hashtable-ref h (+ 2 -7.75) 'ugh) 'mval)
  (eq? (hashtable-ref h (/ 34 2) 'ugh) 'nval)
  (eq? (hashtable-ref h (+ (most-positive-fixnum) 7 -2) 'ugh) 'oval)
  (eqv? (hashtable-delete! h kb) (void))
  (equal? (hashtable-size h) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (begin
    (define h2 (hashtable-copy h #t))
    (and (hashtable? h2)
         (hashtable-mutable? h2)
         (hashtable-ephemeron? h2)))
  (eq? (hashtable-hash-function h2) equal-hash)
  (eq? (hashtable-equivalence-function h2) equal?)
  (equal? (hashtable-size h2) 5)
  (equal-entries? h `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv? (hashtable-clear! h 4) (void))
  (equal?
    (list
      (hashtable-size h)
      (hashtable-ref h ka 1)
      (hashtable-ref h kb #f)
      (hashtable-ref h kc 'nope)
      (hashtable-ref h km 'nope)
      (hashtable-ref h kn 'nope)
      (hashtable-ref h ko 'nope))
   '(0 1 #f nope nope nope nope))
  (equal-entries? h '#() '#())
  (equal?
    (list
      (hashtable-size h2)
      (hashtable-ref h2 ka 1)
      (hashtable-ref h2 kb #f)
      (hashtable-ref h2 kc 'nope)
      (hashtable-ref h2 (- (+ km 1) 1) 'nope)
      (hashtable-ref h2 (- (+ kn 1) 1) 'nope)
      (hashtable-ref h2 (- (+ ko 1) 1) 'nope))
    '(5 aval #f cval mval nval oval))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 18)
  (eqv?
    (hashtable-update! h kq
      (lambda (x) (+ x 1))
      17)
    (void))
  (equal? (hashtable-ref h kq #f) 19)
  (equal? (hashtable-size h) 1)
  (equal-entries? h '#((q)) '#(19))
  (eqv?
    (begin
      (set! kq (void))
      (collect (collect-maximum-generation))
      (hashtable-size h))
    0)
  (equal-entries? h '#() '#())
  (equal? (hashtable-ref h ky #f) #f)
  (eqv?
    (hashtable-set! h ky 'toad)
    (void))
  (equal? (hashtable-ref h ky #f) 'toad)
  (equal? (hashtable-ref h kz #f) #f)
  (eqv?
    (hashtable-update! h kz list 'frog)
    (void))
  (equal? (hashtable-ref h kz #f) '(frog))
  (equal-entries?
    h
    (vector kz ky)
    (vector (hashtable-ref h kz #f) 'toad))
  (eqv? (hashtable-ref h '(zippo) 'nil) 'nil)
  (begin
    (define h3 (hashtable-copy h2 #f))
    (and (hashtable? h3)
         (not (hashtable-mutable? h3))
         (hashtable-ephemeron? h3)))
  (equal-entries? h2 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal-entries? h3 `#((a) (c) -5.75 17 ,ko) '#(aval cval mval nval oval))
  (equal?
    (begin
      (set! ka (void))
      (set! km (void))
      (set! kn (void))
      (set! ko (void))
      (collect (collect-maximum-generation))
      (list (hashtable-size h2) (hashtable-size h3)))
    '(2 2))
  (equal-entries? h2 `#((c) 17) '#(cval nval))
  (equal-entries? h3 `#((c) 17) '#(cval nval))
  (eqv?
    (begin
      (set! h3 (void))
      (collect (collect-maximum-generation))
      (hashtable-size h2))
    2)
  (equal-entries? h2 `#((c) 17) '#(cval nval))

 ; test for proper shrinkage
  (equal?
    (let ([ht (make-ephemeron-hashtable equal-hash equal? 32)])
      (for-each
        (lambda (k*) (for-each (lambda (k) (hashtable-delete! ht k)) k*))
        (let ([k** (map (lambda (x) (map list (make-list 1000)))
                        (make-list 100))])
          (for-each
            (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
            k**)
          k**))
      (call-with-values (lambda () (#%$hashtable-veclen ht)) cons))
    '(32 . 32))

 ; test for proper shrinkage as objects are bwp'd
 ; uses delete to trigger final shrinkage
  (equal?
    (let ([ht (make-ephemeron-hashtable equal-hash equal? 32)])
      (hashtable-set! ht 'a 'b)
      (for-each
        (lambda (k*) (map (lambda (k) (hashtable-set! ht k 75)) k*))
        (map (lambda (x) (map list (make-list 1000))) (make-list 100)))
      (collect (collect-maximum-generation))
      (hashtable-delete! ht 'a)
      (list (hashtable-size ht)
            (let-values ([(n1 n2) (#%$hashtable-veclen ht)])
              (= n1 n2 32))))
    '(0 #t))
)

(mat hash-functions
 ; equal-hash
  (error? ; wrong argument count
    (equal-hash))
  (error? ; wrong argument count
    (equal-hash 0 0))
 ; symbol-hash
  (error? ; wrong argument count
    (symbol-hash))
  (error? ; wrong argument count
    (symbol-hash 'a 'a))
  (error? ; not a symbol
    (symbol-hash "hello"))
 ; string-hash
  (error? ; wrong argument count
    (string-hash))
  (error? ; wrong argument count
    (string-hash 'a 'a))
  (error? ; not a string
    (string-hash 'hello))
 ; string-ci-hash
  (error? ; wrong argument count
    (string-ci-hash))
  (error? ; wrong argument count
    (string-ci-hash 'a 'a))
  (error? ; not a string
    (string-ci-hash 'hello))
  (let ([hc (equal-hash '(a b c))])
    (and (integer? hc)
         (exact? hc)
         (>= hc 0)
         (= (equal-hash '(a b c)) hc)))
  (let ([hc (string-hash "hello")])
    (and (integer? hc)
         (exact? hc)
         (>= hc 0)
         (= (string-hash "hello") hc)))
  (let ([hc (string-ci-hash "hello")])
    (and (integer? hc)
         (exact? hc)
         (>= hc 0)
         (= (string-ci-hash "HelLo") hc)))
  (let ([hc (equal-hash (stencil-vector 3 'one 'two))])
    (and (integer? hc)
         (exact? hc)
         (>= hc 0)
         (= (equal-hash (stencil-vector 3 'one 'two)) hc)))
  (let f ([ls (oblist)])
    (define okay?
      (lambda (x)
        (let ([hc (symbol-hash x)])
          (and (integer? hc)
               (exact? hc)
               (>= hc 0)
               (= (symbol-hash x) hc)))))
    (and (okay? (car ls))
         (let g ([ls ls] [n 10])
           (or (null? ls)
               (if (= n 0)
                   (f ls)
                   (g (cdr ls) (- n 1)))))))
 ; adapted from Flatt's r6rs tests for string-ci=?
  (eqv? (string-ci-hash "z") (string-ci-hash "Z"))
  (not (eqv? (string-ci-hash "z") (string-ci-hash "a")))
  (eqv? (string-ci-hash "Stra\xDF;e") (string-ci-hash "Strasse"))
  (eqv? (string-ci-hash "Stra\xDF;e") (string-ci-hash "STRASSE"))
  (eqv? (string-ci-hash "\x39E;\x391;\x39F;\x3A3;") (string-ci-hash "\x3BE;\x3B1;\x3BF;\x3C2;"))
  (eqv? (string-ci-hash "\x39E;\x391;\x39F;\x3A3;") (string-ci-hash "\x3BE;\x3B1;\x3BF;\x3C3;"))
)

(mat fasl-eq-hashtable
 ; fasling out eq hash tables
  (equal?
    (let ([x (cons 'y '!)])
      (define ht (make-eq-hashtable))
      (eq-hashtable-set! ht x 'because)
      (eq-hashtable-set! ht 'foo "foo")
      (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
        (fasl-write (list x ht) p)
        (close-port p))
      (let-values ([(x2 ht2)
                    (apply values
                      (call-with-port
                        (open-file-input-port "testfile.ss")
                        fasl-read))])
        (list
          (eq-hashtable-weak? ht2)
          (eq-hashtable-ephemeron? ht2)
          (eq-hashtable-ref ht2 x2 #f)
          (eq-hashtable-ref ht2 'foo #f))))
    '(#f #f because "foo"))
 ; fasling out weak eq hash table
  (equal?
    (with-interrupts-disabled
      (let ([x (cons 'y '!)])
        (define ht (make-weak-eq-hashtable))
        (eq-hashtable-set! ht x 'because)
        (eq-hashtable-set! ht 'foo "foo")
        (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
          (fasl-write (list x ht) p)
          (close-port p))
        (let-values ([(x2 ht2)
                      (apply values
                        (call-with-port
                          (open-file-input-port "testfile.ss")
                          fasl-read))])
          (list
            (eq-hashtable-weak? ht2)
            (eq-hashtable-ephemeron? ht2)
            (eq-hashtable-ref ht2 x2 #f)
            (eq-hashtable-ref ht2 'foo #f)))))
    '(#t #f because "foo"))
  (equal?
    (let ([ht2 (cadr (call-with-port
                       (open-file-input-port "testfile.ss")
                       fasl-read))])
      (collect (collect-maximum-generation))
      (list
        (hashtable-keys ht2)
        (eq-hashtable-ref ht2 'foo #f)))
    '(#(foo) "foo"))
 ; fasling out ephemeron eq hash table
  (equal?
    (with-interrupts-disabled
      (let ([x (cons 'y '!)])
        (define ht (make-ephemeron-eq-hashtable))
        (eq-hashtable-set! ht x 'because)
        (eq-hashtable-set! ht 'foo "foo")
        (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
          (fasl-write (list x ht) p)
          (close-port p))
        (let-values ([(x2 ht2)
                      (apply values
                        (call-with-port
                          (open-file-input-port "testfile.ss")
                          fasl-read))])
          (list
            (eq-hashtable-weak? ht2)
            (eq-hashtable-ephemeron? ht2)
            (eq-hashtable-ref ht2 x2 #f)
            (eq-hashtable-ref ht2 'foo #f)))))
    '(#f #t because "foo"))
  (equal?
    (let ([ht2 (cadr (call-with-port
                       (open-file-input-port "testfile.ss")
                       fasl-read))])
      (collect (collect-maximum-generation))
      (list
        (hashtable-keys ht2)
        (eq-hashtable-ref ht2 'foo #f)))
    '(#(foo) "foo"))
 ; fasling eq hash tables via compile-file
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(module ($feh-ls $feh-ht)
             (define-syntax ls
               (let ([ls '(1 2 3)])
                 (lambda (x)
                   #`(quote #,(datum->syntax #'* ls)))))
             (define $feh-ls ls)
             (define $feh-ht
               (let ()
                 (define-syntax a
                   (let ([ht (make-eq-hashtable)])
                     (eq-hashtable-set! ht 'q 'p)
                     (eq-hashtable-set! ht ls (cdr ls))
                     (eq-hashtable-set! ht (cdr ls) (cddr ls))
                     (eq-hashtable-set! ht (cddr ls) ls)
                     (lambda (x) #`(quote #,(datum->syntax #'* ht)))))
                 a)))))
      'replace)
    (compile-file "testfile")
    (load "testfile.so")
    #t)
  (eq? (eq-hashtable-ref $feh-ht 'q #f) 'p)
  (eq? (eq-hashtable-ref $feh-ht $feh-ls #f) (cdr $feh-ls))
  (eq? (eq-hashtable-ref $feh-ht (cdr $feh-ls) #f) (cddr $feh-ls))
  (eq? (eq-hashtable-ref $feh-ht (cddr $feh-ls) #f) $feh-ls)
  (begin
    (eq-hashtable-set! $feh-ht 'p 'r)
    #t)
  (eq? (eq-hashtable-ref $feh-ht 'p #f) 'r)
  (begin
    (eq-hashtable-set! $feh-ht 'q 'not-p)
    #t)
  (eq? (eq-hashtable-ref $feh-ht 'q #f) 'not-p)
)

(mat fasl-symbol-hashtable
 ; fasling out symbol hash tables
  (equal?
    (let ()
      (define ht (make-hashtable symbol-hash eq?))
      (symbol-hashtable-set! ht 'why? 'because)
      (symbol-hashtable-set! ht 'foo "foo")
      (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
        (fasl-write ht p)
        (close-port p))
      (let ([ht2 (call-with-port (open-file-input-port "testfile.ss") fasl-read)])
        (list
          (symbol-hashtable-ref ht2 'why? #f)
          (symbol-hashtable-ref ht2 'foo #f))))
    '(because "foo"))
  (#%$fasl-file-equal? "testfile.ss" "testfile.ss")
  (eqv? (strip-fasl-file "testfile.ss" "testfile1.ss" (fasl-strip-options)) (void))
  (#%$fasl-file-equal? "testfile.ss" "testfile1.ss")
  (equal?
    (let ([ht2 (call-with-port (open-file-input-port "testfile1.ss" (file-options compressed)) fasl-read)])
      (list
        (symbol-hashtable-ref ht2 'why? #f)
        (symbol-hashtable-ref ht2 'foo #f)))
    '(because "foo"))
  (begin
    (call-with-port (open-file-output-port "testfile1.ss" (file-options replace))
      (lambda (p)
        (fasl-write (call-with-port (open-file-input-port "testfile.ss") fasl-read) p)))
    #t)
  (#%$fasl-file-equal? "testfile.ss" "testfile1.ss")
  (#%$fasl-file-equal? "testfile1.ss" "testfile.ss")
  (begin
    (call-with-port (open-file-output-port "testfile1.ss" (file-options replace))
      (lambda (p)
        (let ([ht (call-with-port (open-file-input-port "testfile.ss") fasl-read)])
          (symbol-hashtable-set! ht 'why? 'why-not?)
          (fasl-write ht p))))
    #t)
  (not (#%$fasl-file-equal? "testfile.ss" "testfile1.ss"))
  (not (#%$fasl-file-equal? "testfile1.ss" "testfile.ss"))
  (begin
    (call-with-port (open-file-output-port "testfile1.ss" (file-options replace))
      (lambda (p)
        (let ([ht (call-with-port (open-file-input-port "testfile.ss") fasl-read)])
          (symbol-hashtable-set! ht (gensym) 'foiled)
          (fasl-write ht p))))
    #t)
  (not (#%$fasl-file-equal? "testfile.ss" "testfile1.ss"))
  (not (#%$fasl-file-equal? "testfile1.ss" "testfile.ss"))

 ; fasling symbol hash tables via compile-file
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '(define $fsh-ht
             (let ()
               (define-syntax a
                 (let ([ht (make-hashtable symbol-hash symbol=?)])
                   (symbol-hashtable-set! ht 'q 'p)
                   (symbol-hashtable-set! ht 'p 's)
                   (let ([g (gensym "hello")])
                     (symbol-hashtable-set! ht g g)
                     (symbol-hashtable-set! ht 'g g))
                   (lambda (x) #`(quote #,(datum->syntax #'* ht)))))
               a))))
      'replace)
    (compile-file "testfile")
    (load "testfile.so")
    #t)
  (eq? (symbol-hashtable-ref $fsh-ht 'q #f) 'p)
  (eq? (symbol-hashtable-ref $fsh-ht 'p #f) 's)
  (let ([g (symbol-hashtable-ref $fsh-ht 'g #f)])
    (eq? (symbol-hashtable-ref $fsh-ht g #f) g))
  (eq? (symbol-hashtable-ref $fsh-ht 'spam #f) #f)
  (begin
    (symbol-hashtable-set! $fsh-ht 'p 'r)
    #t)
  (eq? (symbol-hashtable-ref $fsh-ht 'p #f) 'r)
  (begin
    (symbol-hashtable-set! $fsh-ht 'q 'not-p)
    #t)
  (eq? (symbol-hashtable-ref $fsh-ht 'q #f) 'not-p)
)

(mat fasl-other-hashtable
 ; can't fasl out other kinds of hashtables
  (error?
    (let ([x (cons 'y '!)])
      (define ht (make-eqv-hashtable))
      (hashtable-set! ht x 'because)
      (hashtable-set! ht 'foo "foo")
      (hashtable-set! ht 3.1415 "pi")
      (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
        (with-exception-handler
          (lambda (c) (close-port p) (raise-continuable c))
          (lambda () (fasl-write (list x ht) p))))))
  (error?
    (let ([x (cons 'y '!)])
      (define ht (make-hashtable string-hash string=?))
      (hashtable-set! ht "hello" 'goodbye)
      (let ([p (open-file-output-port "testfile.ss" (file-options replace))])
        (with-exception-handler
          (lambda (c) (close-port p) (raise-continuable c))
          (lambda () (fasl-write (list x ht) p))))))
)

(mat ht
  (begin
    (display-string (separate-eval `(parameterize ([source-directories
                                                    ',(source-directories)])
                                      (set! pb? (lambda () ,(pb?)))
                                      (load ,(find-source "ht.ss")))))
    #t)
)
