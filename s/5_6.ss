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

(define ($vector-copy! v1 v2 n)
  (if (fx<= n 10)
      (let loop ([i (fx- n 1)])
        (cond
          [(fx> i 0)
           (vector-set! v2 i (vector-ref v1 i))
           (let ([i (fx- i 1)]) (vector-set! v2 i (vector-ref v1 i)))
           (loop (fx- i 2))]
          [(fx= i 0) (vector-set! v2 i (vector-ref v1 i))]))
      ($ptr-copy! v1 (constant vector-data-disp) v2
        (constant vector-data-disp) n)))

(define ($vector-copy v1 n)
  (let ([v2 (make-vector n)])
    ($vector-copy! v1 v2 n)
    v2))

(set! vector->list
  (lambda (v)
    (unless (vector? v)
      ($oops 'vector->list "~s is not a vector" v))
    ($vector->list v (vector-length v))))

(set! list->vector
  (lambda (ls)
    ($list->vector ls ($list-length ls 'list->vector))))

(set! vector-copy
  (lambda (v)
    (unless (vector? v)
      ($oops 'vector-copy "~s is not a vector" v))
    ($vector-copy v (vector-length v))))

(set-who! vector->immutable-vector
  (lambda (v)
    (cond
      [(immutable-vector? v) v]
      [(eqv? v '#()) ($tc-field 'null-immutable-vector ($tc))]
      [else
       (unless (vector? v) ($oops who "~s is not a vector" v))
       (let ([v2 (vector-copy v)])
         ($vector-set-immutable! v2)
         v2)])))

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

(set-who! fxvector->immutable-fxvector
  (lambda (v)
    (cond
      [(immutable-fxvector? v) v]
      [(eqv? v '#vfx()) ($tc-field 'null-immutable-fxvector ($tc))]
      [else
       (unless (fxvector? v) ($oops who "~s is not a fxvector" v))
       (let ([v2 (fxvector-copy v)])
         ($fxvector-set-immutable! v2)
         v2)])))

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
        (if (fx<= n 1) v (dovsort! elt< ($vector-copy v n) n)))))

  (set-who! vector-sort!
    (lambda (elt< v)
      (unless (procedure? elt<) ($oops who "~s is not a procedure" elt<))
      (unless (mutable-vector? v) ($oops who "~s is not a mutable vector" v))
      (let ([n (vector-length v)])
        (unless (fx<= n 1)
          (let ([outvec (dovsort! elt< v n)])
            (unless (eq? outvec v)
              ($vector-copy! outvec v n)))))))

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
