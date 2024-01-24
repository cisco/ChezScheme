;;; 5_6.ss
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

;;; vector and sorting functions

(let ()
(define ($vector->list v n)
  (let loop ([i (fx- n 1)] [ls '()])
    (if (fx> i 0)
        (loop
          (fx- i 2)
          (list* (vector-ref v (fx- i 1)) (vector-ref v i) ls))
        (if (fx= i 0) (cons (vector-ref v 0) ls) ls))))

(define ($list->vector ls n)
  (let ([v (make-vector n)])
    (let loop ([ls ls] [i 0])
      (unless (null? ls)
        (vector-set! v i (car ls))
        (let ([ls (cdr ls)])
          (unless (null? ls)
            (vector-set! v (fx+ i 1) (car ls))
            (loop (cdr ls) (fx+ i 2))))))
    v))

(define ($vector-copy! v1 v2 n delta)
  (let loop ([i (fx- n 1)])
    (cond
      [(fx> i 0)
       (vector-set! v2 (fx+ i delta) (vector-ref v1 i))
       (let ([i (fx- i 1)]) (vector-set! v2 (fx+ i delta) (vector-ref v1 i)))
       (loop (fx- i 2))]
      [(fx= i 0) (vector-set! v2 (fx+ i delta) (vector-ref v1 i))])))

(let ()
  (define (not-a-vector who v)
    ($oops who "~s is not a vector" v))
  
  (define (check-vector-range who v start len)
    (unless (vector? v)
      (not-a-vector who v))
    (unless (and (fixnum? start) (fx>= start 0))
      ($oops who "invalid start value ~s" start))
    (unless (and (fixnum? len) (fx>= len 0))
      ($oops who "invalid count ~s" len))
    (unless (fx<= len (fx- (vector-length v) start)) ; avoid overflow
      ($oops who "index ~s + count ~s is beyond the end of ~s" start len v)))

  (define (append-vectors who vs)
    (let ([len (let loop ([vs vs])
                 (cond
                   [(null? vs) 0]
                   [else
                    (let ([v (car vs)])
                      (unless (vector? v) (not-a-vector who v))
                      (fx+ (vector-length v) (loop (cdr vs))))]))])
      (let ([dest (make-vector len)])
        (let loop ([vs vs] [i 0])
          (cond
            [(null? vs) dest]
            [else
             (let* ([v (car vs)]
                    [len (vector-length v)])
               ($vector-copy! v dest len i)
               (loop (cdr vs) (fx+ i len)))])))))
    
  (set-who! vector-copy
    (case-lambda
      [(v)
       (unless (vector? v) (not-a-vector who v))
       (#3%vector-copy v 0 (vector-length v))]
      [(v start len)
       (check-vector-range who v start len)
       (#3%vector-copy v start len)]))

  (set-who! immutable-vector-copy
    (case-lambda
      [(v)
       (unless (vector? v) (not-a-vector who v))
       (#3%immutable-vector-copy v)]
      [(v start len)
       (check-vector-range who v start len)
       (#3%immutable-vector-copy v start len)]))

  (set-who! vector-append
    (case-lambda
      [(v)
       (unless (vector? v) (not-a-vector who v))
       (vector-copy v)]
      [(v1 v2)
       (unless (vector? v1) (not-a-vector who v1))
       (unless (vector? v2) (not-a-vector who v2))
       (#3%vector-append v1 v2)]
      [(v1 v2 v3)
       (unless (vector? v1) (not-a-vector who v1))
       (unless (vector? v2) (not-a-vector who v2))
       (unless (vector? v3) (not-a-vector who v3))
       (#3%vector-append v1 v2 v3)]
      [vs
       (append-vectors who vs)]))

  (set-who! immutable-vector-append
    (case-lambda
      [(v)
       (unless (vector? v) (not-a-vector who v))
       (immutable-vector-copy v)]
      [(v1 v2)
       (unless (vector? v1) (not-a-vector who v1))
       (unless (vector? v2) (not-a-vector who v2))
       (#3%immutable-vector-append v1 v2)]
      [(v1 v2 v3)
       (unless (vector? v1) (not-a-vector who v1))
       (unless (vector? v2) (not-a-vector who v2))
       (unless (vector? v3) (not-a-vector who v3))
       (#3%immutable-vector-append v1 v2 v3)]
      [vs
       (let ([v (append-vectors who vs)])
         (cond
           [(eq? v '#())
            (immutable-vector)]
           [else
            ($vector-set-immutable! v)
            v]))]))

  (set-who! vector-set/copy
    (lambda (v idx val)
      (unless (vector? v) (not-a-vector who v))
      (unless (and (fixnum? idx) (fx<= 0 idx) (fx< idx (vector-length v)))
        ($oops who "~s is not a valid index for ~s" idx v))
      (#3%vector-set/copy v idx val)))

  (set-who! immutable-vector-set/copy
    (lambda (v idx val)
      (unless (vector? v) (not-a-vector who v))
      (unless (and (fixnum? idx) (fx<= 0 idx) (fx< idx (vector-length v)))
        ($oops who "~s is not a valid index for ~s" idx v))
      (#3%immutable-vector-set/copy v idx val))))

(set! vector->list
  (lambda (v)
    (unless (vector? v)
      ($oops 'vector->list "~s is not a vector" v))
    ($vector->list v (vector-length v))))

(set! list->vector
  (lambda (ls)
    ($list->vector ls ($list-length ls 'list->vector))))

(set-who! vector->immutable-vector
  (lambda (v)
    (unless (vector? v) ($oops who "~s is not a vector" v))
    (#3%vector->immutable-vector v)))

(set-who! vector-fill!
  (lambda (v obj)
    (unless (mutable-vector? v) ($oops who "~s is not a mutable vector" v))
    (let ([n (vector-length v)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i n))
        (vector-set! v i obj)))))

(set! fxvector->list
  (lambda (v)
    (unless (fxvector? v)
      ($oops 'fxvector->list "~s is not an fxvector" v))
    (let loop ([i (fx- (fxvector-length v) 1)] [l '()])
      (if (fx> i 0)
          (loop
            (fx- i 2)
            (list* (fxvector-ref v (fx- i 1)) (fxvector-ref v i) l))
          (if (fx= i 0) (cons (fxvector-ref v 0) l) l)))))

(set! list->fxvector
  (lambda (x)
    (let ([v (make-fxvector ($list-length x 'list->fxvector))])
      (do ([ls x (cdr ls)] [i 0 (fx+ i 1)])
          ((null? ls) v)
        (let ([n (car ls)])
          (unless (fixnum? n)
            ($oops 'list->fxvector "~s is not a fixnum" n))
          (fxvector-set! v i n))))))

(set! fxvector-copy
  (lambda (fxv1)
    (unless (fxvector? fxv1)
      ($oops 'fxvector-copy "~s is not an fxvector" fxv1))
    (let ([n (fxvector-length fxv1)])
      (let ([fxv2 (make-fxvector n)])
        (if (fx<= n 10)
            (let loop ([i (fx- n 1)])
              (cond
                [(fx> i 0)
                 (fxvector-set! fxv2 i (fxvector-ref fxv1 i))
                 (let ([i (fx- i 1)]) (fxvector-set! fxv2 i (fxvector-ref fxv1 i)))
                 (loop (fx- i 2))]
                [(fx= i 0) (fxvector-set! fxv2 i (fxvector-ref fxv1 i))]))
            ($ptr-copy! fxv1 (constant fxvector-data-disp) fxv2
              (constant fxvector-data-disp) n))
        fxv2))))

(set! flvector->list
  (lambda (v)
    (unless (flvector? v)
      ($oops 'flvector->list "~s is not an flvector" v))
    (let loop ([i (fx- (flvector-length v) 1)] [l '()])
      (if (fx> i 0)
          (loop
            (fx- i 2)
            (list* (flvector-ref v (fx- i 1)) (flvector-ref v i) l))
          (if (fx= i 0) (cons (flvector-ref v 0) l) l)))))

(set! list->flvector
  (lambda (x)
    (let ([v (make-flvector ($list-length x 'list->flvector))])
      (do ([ls x (cdr ls)] [i 0 (fx+ i 1)])
          ((null? ls) v)
        (let ([n (car ls)])
          (unless (flonum? n)
            ($oops 'list->flvector "~s is not a flonum" n))
          (flvector-set! v i n))))))

(set! flvector-copy
  (lambda (flv1)
    (unless (flvector? flv1)
      ($oops 'flvector-copy "~s is not an flvector" flv1))
    (let ([n (flvector-length flv1)])
      (let ([flv2 (make-flvector n)])
        (if (fx<= n 10)
            (let loop ([i (fx- n 1)])
              (cond
                [(fx> i 0)
                 (flvector-set! flv2 i (flvector-ref flv1 i))
                 (let ([i (fx- i 1)]) (flvector-set! flv2 i (flvector-ref flv1 i)))
                 (loop (fx- i 2))]
                [(fx= i 0) (flvector-set! flv2 i (flvector-ref flv1 i))]))
            ($byte-copy! flv1 (constant flvector-data-disp) flv2
              (constant flvector-data-disp) (fx* n (constant flonum-bytes))))
        flv2))))

(set! vector-map
  (case-lambda
    [(p v)
     (unless (procedure? p) ($oops 'vector-map "~s is not a procedure" p))
     (unless (vector? v) ($oops 'vector-map "~s is not a vector" v))
     (#3%vector-map p v)]
    [(p u v)
     (unless (procedure? p) ($oops 'vector-map "~s is not a procedure" p))
     (unless (vector? u) ($oops 'vector-map "~s is not a vector" u))
     (unless (vector? v) ($oops 'vector-map "~s is not a vector" v))
     (let ([n (vector-length u)])
       (unless (fx= (vector-length v) n)
         ($oops 'vector-map "lengths of input vectors ~s and ~s differ" u v))
       (let f ([i (fx- n 1)])
         (if (fx> i 0)
             (let ([x1 (p (vector-ref u i) (vector-ref v i))]
                   [x2 (let ([j (fx- i 1)])
                         (p (vector-ref u j) (vector-ref v j)))])
               (let ([vout (f (fx- i 2))])
                 (vector-set! vout i x1)
                 (vector-set! vout (fx- i 1) x2)
                 vout))
             (make-vector n
               (if (fx= i 0)
                   (p (vector-ref u 0) (vector-ref v 0))
                   0)))))]
    [(p u . v*)
     (unless (procedure? p) ($oops 'vector-map "~s is not a procedure" p))
     (unless (vector? u) ($oops 'vector-map "~s is not a vector" u))
     (for-each (lambda (v) (unless (vector? v) ($oops 'vector-map "~s is not a vector" v))) v*)
     (let ([n (vector-length u)])
       (for-each
         (lambda (v)
           (unless (fx= (vector-length v) n)
             ($oops 'vector-map "lengths of input vectors ~s and ~s differ" u v)))
         v*)
       (let f ([i (fx- n 1)])
         (if (fx> i 0)
             (let ([x1 (apply p
                         (vector-ref u i)
                         (map (lambda (v) (vector-ref v i)) v*))]
                   [x2 (let ([j (fx- i 1)])
                         (apply p
                           (vector-ref u j)
                           (map (lambda (v) (vector-ref v j)) v*)))])
               (let ([vout (f (fx- i 2))])
                 (vector-set! vout i x1)
                 (vector-set! vout (fx- i 1) x2)
                 vout))
             (make-vector n
               (if (fx= i 0)
                   (apply p
                     (vector-ref u 0)
                     (map (lambda (v) (vector-ref v 0)) v*))
                   0)))))]))

(set! vector-for-each
  (case-lambda
    [(p v)
     (unless (procedure? p) ($oops 'vector-for-each "~s is not a procedure" p))
     (unless (vector? v) ($oops 'vector-for-each "~s is not a vector" v))
     (#3%vector-for-each p v)]
    [(p u v)
     (unless (procedure? p) ($oops 'vector-for-each "~s is not a procedure" p))
     (unless (vector? u) ($oops 'vector-for-each "~s is not a vector" u))
     (unless (vector? v) ($oops 'vector-for-each "~s is not a vector" v))
     (let ([n (vector-length u)])
       (unless (fx= (vector-length v) n)
         ($oops 'vector-for-each "lengths of input vectors ~s and ~s differ" u v))
       (unless (fx= n 0)
         (let loop ([i 0])
           (let ([j (fx+ i 1)])
             (if (fx= j n)
                 (p (vector-ref u i) (vector-ref v i))
                 (begin
                   (p (vector-ref u i) (vector-ref v i))
                   (loop j)))))))]
    [(p u . v*)
     (unless (procedure? p) ($oops 'vector-for-each "~s is not a procedure" p))
     (unless (vector? u) ($oops 'vector-for-each "~s is not a vector" u))
     (for-each (lambda (v) (unless (vector? v) ($oops 'vector-for-each "~s is not a vector" v))) v*)
     (let ([n (vector-length u)])
       (for-each
         (lambda (v)
           (unless (fx= (vector-length v) n)
             ($oops 'vector-for-each "lengths of input vectors ~s and ~s differ" u v)))
         v*)
       (unless (fx= n 0)
         (let loop ([i 0])
           (let ([j (fx+ i 1)])
             (if (fx= j n)
                 (apply p (vector-ref u i) (map (lambda (v) (vector-ref v i)) v*))
                 (begin
                   (apply p (vector-ref u i) (map (lambda (v) (vector-ref v i)) v*))
                   (loop j)))))))]))

(let ()
  (module (dovsort!)
   ;; dovsort! is a modified version of Olin Shiver's code for opportunistic
   ;; vector merge sort, based on a version found in the MzScheme Version 360
   ;; source code, which contains the following copyright notice.

   ;; This code is
   ;;     Copyright (c) 1998 by Olin Shivers.
   ;; The terms are: You may do as you please with this code, as long as
   ;; you do not delete this notice or hold me responsible for any outcome
   ;; related to its use.
   ;;
   ;; Blah blah blah. Don't you think source files should contain more lines
   ;; of code than copyright notice?

   ;; This merge sort is "opportunistic" -- the leaves of the merge tree are
   ;; contiguous runs of already sorted elements in the vector. In the best
   ;; case -- an already sorted vector -- it runs in linear time. Worst case
   ;; is still O(n lg n) time.

   ;; RKD: performance is a bit worse on average than a straightforward
   ;; merge-sort for random input vectors, but speed for sorted or mostly
   ;; sorted vectors is much better.

   ;; RKD: The following issues with the original code have been addressed:
   ;;  - tail-len is bound but not used.
   ;;  - len is computed before it is known to be needed; it would be
   ;;    (marginally) better to remove the binding for len and replace
   ;;    (= pfxlen len) with (= pfxlen (- r l)).
   ;;  - In the %vector-merge-sort! loop computing pfxlen2, (fx<= j pfxlen)
   ;;    should be (fx<= j*2 pfxlen); otherwise pfxlen2 is actually the first
   ;;    power of two greater than pfxlen.  Fixing this improved performance by
   ;;    about 20% for sort using predicate < for a list of 10^6 random
   ;;    integers between 0 and 1000.  (pfxlen2 computation later flushed
   ;;    entirely; just using pfxlen, which is simpler and usually faster.)
   ;;  - The temp need not be a copy of the input vector, just a vector of
   ;;    the appropriate length.
    (define (merge elt< target v1 v2 l len1 len2)
     ; assumes target != v1, but v2 may be v1 or target
     ; merge v1[l,l+len1-1] and v2[l+len1,l+len1+len2-1] into target[l,l+len1+len2-1]
      (let* ([r1 (fx+ l len1)] [r2 (fx+ r1 len2)])
        (let lp ([i l] [j l] [x (vector-ref v1 l)] [k r1] [y (vector-ref v2 r1)])
          (if (elt< y x)
              (let ([k (fx+ k 1)])
                (vector-set! target i y)
                (if (fx< k r2)
                    (lp (fx+ i 1) j x k (vector-ref v2 k))
                    (vblit v1 j target (fx+ i 1) r1)))
              (let ([j (fx+ j 1)])
                (vector-set! target i x)
                (if (fx< j r1)
                    (lp (fx+ i 1) j (vector-ref v1 j) k y)
                    (unless (eq? v2 target)
                      (vblit v2 k target (fx+ i 1) r2))))))))
    (define (vblit fromv j tov i n)
      (let lp ([j j] [i i])
        (vector-set! tov i (vector-ref fromv j))
        (let ([j (fx+ j 1)])
          (unless (fx= j n) (lp j (fx+ i 1))))))
    (define (getrun elt< v l r) ; assumes l < r
      (let lp ([i (fx+ l 1)] [x (vector-ref v l)])
        (if (fx= i r)
            (fx- i l)
            (let ([y (vector-ref v i)])
              (if (elt< y x) (fx- i l) (lp (fx+ i 1) y))))))
    (define (dovsort! elt< v0 n)
      (let ([temp0 (make-vector n)])
        (define (recur l want)
         ; sort v0[l,l+len-1] for some len where 0 < want <= len <= (n-l).
         ; that is, sort *at least* want elements in v0 starting at index l.
         ; may put the result into either v0[l,l+len-1] or temp0[l,l+len-1].
         ; does not alter either vector outside this range.  returns two
         ; values: the number of values sorted and the vector holding the
         ; sorted values.
          (let lp ([pfxlen (getrun elt< v0 l n)] [v v0] [temp temp0])
           ; v[l,l+pfxlen-1] holds a sorted version of v0[l,l+pfxlen-1]
            (if (or (fx>= pfxlen want) (fx= pfxlen (fx- n l)))
                (values pfxlen v)
                (let-values ([(outlen outvec) (recur (fx+ l pfxlen) pfxlen)])
                  (merge elt< temp v outvec l pfxlen outlen)
                  (lp (fx+ pfxlen outlen) temp v)))))
       ; return v0 or temp0 containing sorted values
        (let-values ([(outlen outvec) (recur 0 n)]) outvec))))

  (define (dolsort elt< ls n)
    (cond
      [(fx= n 1) (cons (car ls) '())]
      [(fx= n 2)
       (let ([x (car ls)] [y (cadr ls)])
         (if (elt< y x) (list y x) (list x y)))]
      [else
       (let ([i (fxsrl n 1)])
         (dolmerge elt<
           (dolsort elt< ls i)
           (dolsort elt< (list-tail ls i) (fx- n i))))]))

  (define (dolmerge elt< ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(elt< (car ls2) (car ls1))
       (cons (car ls2) (dolmerge elt< ls1 (cdr ls2)))]
      [else (cons (car ls1) (dolmerge elt< (cdr ls1) ls2))]))

   (define (dolsort! elt< ls n loc)
     (if (fx= n 1)
         (begin (set-cdr! ls '()) ls)
         (let ([i (fxsrl n 1)])
           (let ([tail (list-tail ls i)])
             (dolmerge! elt<
               (dolsort! elt< ls i loc)
               (dolsort! elt< tail (fx- n i) loc)
               loc)))))

   (define (dolmerge! elt< ls1 ls2 loc)
     (let loop ([ls1 ls1] [ls2 ls2] [loc loc])
       (cond
         [(null? ls1) (set-cdr! loc ls2)]
         [(null? ls2) (set-cdr! loc ls1)]
         [(elt< (car ls2) (car ls1))
          (set-cdr! loc ls2)
          (loop ls1 (cdr ls2) ls2)]
         [else (set-cdr! loc ls1) (loop (cdr ls1) ls2 ls1)]))
     (cdr loc))

  (set-who! vector-sort
    (lambda (elt< v)
      (unless (procedure? elt<) ($oops who "~s is not a procedure" elt<))
      (unless (vector? v) ($oops who "~s is not a vector" v))
      (let ([n (vector-length v)])
        (if (fx<= n 1) v (dovsort! elt< (vector-copy v 0 n) n)))))

  (set-who! vector-sort!
    (lambda (elt< v)
      (unless (procedure? elt<) ($oops who "~s is not a procedure" elt<))
      (unless (mutable-vector? v) ($oops who "~s is not a mutable vector" v))
      (let ([n (vector-length v)])
        (unless (fx<= n 1)
          (let ([outvec (dovsort! elt< v n)])
            (unless (eq? outvec v)
              ($vector-copy! outvec v n 0)))))))

  (set-who! list-sort
    (lambda (elt< ls)
      (unless (procedure? elt<) ($oops who "~s is not a procedure" elt<))
      (let ([n ($list-length ls who)])
        (if (fx< n 25)
            (if (fx<= n 1) ls (dolsort elt< ls n))
            ($vector->list (dovsort! elt< ($list->vector ls n) n) n)))))

  (set-who! sort
    (lambda (elt< ls)
      (unless (procedure? elt<) ($oops who "~s is not a procedure" elt<))
      (let ([n ($list-length ls who)])
        (if (fx< n 25)
            (if (fx<= n 1) ls (dolsort elt< ls n))
            ($vector->list (dovsort! elt< ($list->vector ls n) n) n)))))

  (set-who! merge
    (lambda (elt< ls1 ls2)
      (unless (procedure? elt<)
        ($oops who "~s is not a procedure" elt<))
      ($list-length ls1 who)
      ($list-length ls2 who)
      (dolmerge elt< ls1 ls2)))

  (set-who! sort!
    (lambda (elt< ls)
      (unless (procedure? elt<) ($oops who "~s is not a procedure" elt<))
      (let ([n ($list-length ls who)])
        (if (fx< n 25)
            (if (fx<= n 1) ls (dolsort! elt< ls n (list '())))
            (let ([v (dovsort! elt< ($list->vector ls n) n)])
              (let loop ([ls ls] [i 0])
                (unless (null? ls)
                  (set-car! ls (vector-ref v i))
                  (let ([ls (cdr ls)])
                    (unless (null? ls)
                      (set-car! ls (vector-ref v (fx+ i 1)))
                      (loop (cdr ls) (fx+ i 2))))))
              ls)))))

  (set-who! merge!
    (lambda (elt< ls1 ls2)
      (unless (procedure? elt<)
        ($oops who "~s is not a procedure" elt<))
      ($list-length ls1 who)
      ($list-length ls2 who)
      (dolmerge! elt< ls1 ls2 (list '())))))
)

;; compiled with generate-interrupt-trap #f and optimize-level 3 so
;; that stencil updates won't be interrupted by a GC while a newly
;; allocated stencil is filled in
(eval-when (compile)
  (generate-interrupt-trap #f)
  (optimize-level 3))

(let ()
  (define-syntax define-stencil-vector-functions
    (lambda (stx)
      (syntax-case stx ()
        [(_ $make-stencil-vector
            stencil-vector
            stencil-vector?
            stencil-vector-for-update?
            stencil-vector-length
            stencil-vector-mask
            stencil-vector-ref
            stencil-vector-set!
            stencil-vector-truncate!
            stencil-vector-update
            $stencil-vector-do-update)
         #'(let ()
             ;; Call with non-zero n
             (define (stencil-vector-copy! to-v to-i from-v from-i n)
               (cond
                 [(fx= n 1)
                  ($stencil-vector-fill-set! to-v to-i (stencil-vector-ref from-v from-i))]
                 [else
                  ($stencil-vector-fill-set! to-v to-i (stencil-vector-ref from-v from-i))
                  ($stencil-vector-fill-set! to-v (fx+ to-i 1) (stencil-vector-ref from-v (fx+ from-i 1)))
                  (let ([n (fx- n 2)])
                    (unless (fx= n 0)
                      (stencil-vector-copy! to-v (fx+ to-i 2) from-v (fx+ from-i 2) n)))]))

             (define (do-stencil-vector-update v mask remove-bits add-bits vals)
               (let* ([new-n (fxpopcount (fxior (fx- mask remove-bits) add-bits))]
                      [new-v ($make-stencil-vector new-n (fxior (fx- mask remove-bits) add-bits))])
                 ;; `new-v` is not initialized, so don't let a GC happen until we're done filling it in
                 (let loop ([to-i 0] [from-i 0] [mask mask] [remove-bits remove-bits] [add-bits add-bits] [vals vals])
                   (unless (fx= to-i new-n)
                     (let* ([pre-remove-mask (fx- (fxxor remove-bits (fxand remove-bits (fx- remove-bits 1))) 1)]
                            [pre-add-mask (fx- (fxxor add-bits (fxand add-bits (fx- add-bits 1))) 1)]
                            [keep-mask (fxand mask pre-remove-mask pre-add-mask)]
                            [kept-n (cond
                                      [(fx= 0 keep-mask) 0]
                                      [else
                                       (let ([keep-n (fxpopcount keep-mask)])
                                         (stencil-vector-copy! new-v to-i v from-i keep-n)
                                         keep-n)])])
                       (let ([to-i (fx+ to-i kept-n)]
                             [from-i (fx+ from-i kept-n)]
                             [mask (fx- mask keep-mask)])
                         (cond
                           [($fxu< pre-add-mask pre-remove-mask)
                            ;; an add bit happens before a remove bit
                            ($stencil-vector-fill-set! new-v to-i (car vals))
                            (loop (fx+ to-i 1) from-i mask remove-bits (fx- add-bits (fx+ pre-add-mask 1)) (cdr vals))]
                           [else
                            ;; a remove bit happens before an add bit (or we're at the end)
                            (let ([remove-bit (fx+ pre-remove-mask 1)])
                              (loop to-i (fx+ from-i 1) (fx- mask remove-bit) (fx- remove-bits remove-bit) add-bits vals))])))))
                 new-v))

             (define (stencil-vector-replace-one v bit val)
               (let* ([mask (stencil-vector-mask v)]
                      [n (fxpopcount mask)]
                      [new-v ($make-stencil-vector n mask)])
                 ;; `new-v` is not initialized, so don't let a GC happen until we're done filling it in
                 (stencil-vector-copy! new-v 0 v 0 n)
                 (let ([i (fxpopcount (fxand mask (fx- bit 1)))])
                   ($stencil-vector-fill-set! new-v i val))
                 new-v))

             (define (stencil-vector-replace-two v bits val1 val2)
               (let* ([mask (stencil-vector-mask v)]
                      [n (fxpopcount mask)]
                      [new-v ($make-stencil-vector n mask)])
                 ;; `new-v` is not initialized, so don't let a GC happen until we're done filling it in
                 (stencil-vector-copy! new-v 0 v 0 n)
                 (let ([i1 (fxpopcount (fxand mask (fx- (fxxor bits (fxand bits (fx- bits 1))) 1)))])
                   ($stencil-vector-fill-set! new-v i1 val1)
                   (let ([i2 (fxpopcount (fxand mask (fx- (fxand bits (fx- bits 1)) 1)))])
                     ($stencil-vector-fill-set! new-v i2 val2)))
                 new-v))

             (set-who! stencil-vector-mask-width (lambda () (constant stencil-vector-mask-bits)))

             (set-who! stencil-vector-length
               (lambda (v)
                 (unless (stencil-vector? v)
                   ($oops who "~s is not a stencil vector" v))
                 (fxpopcount (stencil-vector-mask v))))

             (set-who! stencil-vector-ref
               (lambda (v i)
                 (unless (stencil-vector? v)
                   ($oops who "~s is not a stencil vector" v))
                 (unless (and (fixnum? i)
                              (fx< -1 i (fxpopcount (stencil-vector-mask v))))
                   ($oops who "~s is not a valid index for ~s" i v))
                 (#3%stencil-vector-ref v i)))

             (set-who! stencil-vector-set!
               (lambda (v i val)
                 (unless (stencil-vector? v)
                   ($oops who "~s is not a stencil vector" v))
                 (unless (and (fixnum? i)
                              (fx< -1 i (fxpopcount (stencil-vector-mask v))))
                   ($oops who "~s is not a valid index for ~s" i v))
                 (#3%stencil-vector-set! v i val)))
             
             (set-who! stencil-vector
               (lambda (mask . vals)
                 (unless (and (fixnum? mask)
                              (fx< -1 mask (fxsll 1 (constant stencil-vector-mask-bits))))
                   ($oops who "invalid mask ~s" mask))
                 (let ([n (fxpopcount mask)])
                   (unless (fx= (length vals) n)
                     ($oops who "mask ~s does not match given number of items ~s" mask (length vals)))
                   (let ([v ($make-stencil-vector n mask)])
                     ;; `new-v` is not initialized, so don't let a GC happen until we're done filling it in
                     (let loop ([i 0] [vals vals])
                       (unless (fx= i n)
                         ($stencil-vector-fill-set! v i (car vals))
                         (loop (fx+ i 1) (cdr vals))))
                     v))))

             (set-who! stencil-vector-update
               (lambda (v remove-bits add-bits . vals)
                 (unless (stencil-vector-for-update? v)
                   ($oops who "~s is not a stencil vector" v))
                 (let ([mask (stencil-vector-mask v)])
                   (unless (and (fixnum? remove-bits)
                                (fx< -1 remove-bits (fxsll 1 (constant stencil-vector-mask-bits))))
                     ($oops who "invalid removal mask ~s" remove-bits))
                   (unless (fx= remove-bits (fxand remove-bits mask))
                     ($oops who "mask of stencil vector ~s does not have all bits in ~s" v remove-bits))
                   (unless (and (fixnum? add-bits)
                                (fx< -1 add-bits (fxsll 1 (constant stencil-vector-mask-bits))))
                     ($oops who "invalid addition mask ~s" add-bits))
                   (unless (fx= 0 (fxand add-bits (fx- mask remove-bits)))
                     ($oops who "mask of stencil vector ~s already has bits in ~s" v add-bits))
                   (unless (fx= (fxpopcount add-bits) (length vals))
                     ($oops who "addition mask ~s does not match given number of items ~s" add-bits (length vals)))
                   (do-stencil-vector-update v mask remove-bits add-bits vals))))

             (set-who! stencil-vector-truncate!
               (lambda (v new-mask)
                 (unless (stencil-vector-for-update? v)
                   ($oops who "~s is not a stencil vector" v))
                 (unless (and (fixnum? new-mask)
                              (fx< -1 new-mask (fxsll 1 (constant stencil-vector-mask-bits))))
                   ($oops who "invalid mask ~s" new-mask))
                 (let ([old-mask (stencil-vector-mask v)])
                   (unless (fx<= (fxpopcount new-mask) (fxpopcount old-mask))
                     ($oops who "new mask ~s for ~s is larger than old mask" new-mask v))
                   (stencil-vector-truncate! v new-mask))))

             ;; unsafe variant, which assumes that the arguments are consistent;
             ;; recognize the case where all slots are replaced
             (set-who! $stencil-vector-do-update
               (case-lambda
                [(v remove-bits add-bits x)
                 (let ([mask (stencil-vector-mask v)])
                   (cond
                     [(fx= 0 (fx- mask remove-bits))
                      ;; not using any data from `v`
                      (stencil-vector add-bits x)]
                     [(fx= add-bits remove-bits)
                      ;; updating one element of `v`:
                      (stencil-vector-replace-one v add-bits x)]
                     [else
                      (do-stencil-vector-update v mask remove-bits add-bits (list x))]))]
                [(v remove-bits add-bits x y)
                 (let ([mask (stencil-vector-mask v)])
                   (cond
                     [(fx= 0 (fx- mask remove-bits))
                      ;; not using any data from `v`
                      (stencil-vector add-bits x y)]
                     [(fx= add-bits remove-bits)
                      ;; updating two elements of `v`:
                      (stencil-vector-replace-two v add-bits x y)]
                     [else
                      (do-stencil-vector-update v mask remove-bits add-bits (list x y))]))]
                [(v remove-bits add-bits x y z)
                 (let ([mask (stencil-vector-mask v)])
                   (if (fx= 0 (fx- mask remove-bits))
                       (stencil-vector add-bits x y z)
                       (do-stencil-vector-update v mask remove-bits add-bits (list x y z))))]
                [(v remove-bits add-bits . vals)
                 (do-stencil-vector-update v (stencil-vector-mask v) remove-bits add-bits vals)])))])))

  (define-stencil-vector-functions
    $make-stencil-vector
    stencil-vector
    stencil-vector?
    stencil-vector?
    stencil-vector-length
    stencil-vector-mask
    stencil-vector-ref
    stencil-vector-set!
    stencil-vector-truncate!
    stencil-vector-update
    $stencil-vector-do-update)

  ;; used by the reader:
  (set-who! $make-empty-stencil-vector
    (lambda (mask)
      (let* ([n (fxpopcount mask)]
             [v ($make-stencil-vector n mask)])
        (let loop ([i 0])
          (unless (fx= i n)
            ($stencil-vector-fill-set! v i 0)
            (loop (fx+ i 1))))
        v)))

  ;; `$`-prefixed variants work on regular and system stencils, unless
  ;; `system` is also in the name
  (define-stencil-vector-functions
    $make-system-stencil-vector
    $system-stencil-vector
    $stencil-vector?
    $system-stencil-vector?
    $stencil-vector-length
    $stencil-vector-mask
    $stencil-vector-ref
    $stencil-vector-set!
    $system-stencil-vector-truncate!
    $system-stencil-vector-update
    $system-stencil-vector-do-update))
