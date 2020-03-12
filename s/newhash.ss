;;; newhash.ss
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

#|
Documentation notes:
- hashtable-copy can create immutable weak eq hashtables. an immutable weak
  hashtable is immutable in the sense that it cannot be modified by
  hashtable-set! or hashtable-update!, but the disappearance of key, val
  pairs can be detected with hashtable-size, hashtable-keys, and
  hashtable-entries.
- symbols are collectable, so weak hash tables should not be used to create
  permanent associations with symbols as keys
|#

#|
; csv7:
(define make-hash-table)                 ; weakflag
(define hash-table?)                     ; x
(define put-hash-table!)                 ; hashtable key obj
(define get-hash-table)                  ; hashtable key default
(define remove-hash-table!)              ; hashtable key
(define hash-table-map)                  ; hashtable proc
(define hash-table-for-each)             ; hashtable proc

;;; r6rs:
(define make-eq-hashtable)               ; [k], k >= 0
(define make-eqv-hashtable)              ; [k], k >= 0
(define make-hashtable)                  ; hashproc equivproc [k], k >= 0
(define hashtable?)                      ; x
(define hashtable-size)                  ; hashtable
(define hashtable-ref)                   ; hashtable key default
(define hashtable-set!)                  ; hashtable key obj
(define hashtable-delete!)               ; hashtable key
(define hashtable-contains?)             ; hashtable key
(define hashtable-update!)               ; hashtable key proc default
(define hashtable-copy)                  ; hashtable [mutableflag]
(define hashtable-clear!)                ; hashtable [k], k >= 0
(define hashtable-keys)                  ; hashtable
(define hashtable-entries)               ; hashtable
(define hashtable-cells)                 ; hashtable
(define hashtable-equivalence-function)  ; hashtable
(define hashtable-hash-function)         ; hashtable
(define hashtable-mutable?)              ; hashtable
(define equal-hash)                      ; obj
(define string-hash)                     ; string
(define string-ci-hash)                  ; string
(define symbol-hash)                     ; symbol

;;; other generic hash operators
(define hashtable-cell)
(define hashtable-weak?)                 ; hashtable
(define hashtable-ephemeron?)            ; hashtable

;;; eq-hashtable operators
(define make-weak-eq-hashtable)          ; [k], k >= 0
(define eq-hashtable-ref)                ; eq-hashtable key default
(define eq-hashtable-contains?)          ; eq-hashtable key
(define eq-hashtable-set!)               ; eq-hashtable key obj
(define eq-hashtable-update!)            ; eq-hashtable key proc default
(define eq-hashtable-cell)               ; eq-hashtable key default
(define eq-hashtable-delete!)            ; eq-hashtable key
(define eq-hashtable-weak?)              ; eq-hashtable
(define eq-hashtable-ephemeron?)         ; eq-hashtable

;;; eq-hashtable operators
(define make-symbol-hashtable)           ; [k], k >= 0
(define symbol-hashtable-ref)            ; symbol-hashtable key default
(define symbol-hashtable-contains?)      ; symbol-hashtable key
(define symbol-hashtable-set!)           ; symbol-hashtable key obj
(define symbol-hashtable-update!)        ; symbol-hashtable key proc default
(define symbol-hashtable-cell)           ; symbol-hashtable key default
(define symbol-hashtable-delete!)        ; symbol-hashtable key

;;; eqv-hashtable operators
(define make-weak-eqv-hashtable)         ; [k], k >= 0
  
;;; unsafe eq-hashtable operators
(define $make-eq-hashtable)              ; fxminlen subtype, fxminlen = 2^n, n >= 0
(define $eq-hashtable-keys)              ; eq-hashtable
(define $eq-hashtable-values)            ; eq-hashtable
(define $eq-hashtable-entries)           ; eq-hashtable
(define $eq-hashtable-cells)             ; eq-hashtable
(define $eq-hashtable-copy)              ; eq-hashtable [mutableflag]
(define $eq-hashtable-clear!)            ; eq-hashtable [fxminlen]

;;; inspection
(define $hashtable-veclen)
(define $hashtable-report)
|#

(let ()
  (include "hashtable-types.ss")

  (define do-hash
    (lambda (hash x mask who)
      ; NB: the hash function should return a nonnegative exact integer.
      ; NB: we check only that it returns an exact integer, i.e., extend the semantics to
      ; NB: allow negative exact integers.
      (let ([i (hash x)])
        (cond
          [(fixnum? i) (fxlogand i mask)]
          [(bignum? i) (logand i mask)]
          [else ($oops who "invalid hash-function ~s return value ~s for ~s" hash i x)]))))

  (define size->minlen
    (lambda (who k)
      (define maxbits (fx- (fixnum-width) 4))
      (cond
        [(and (fixnum? k) (fx>= k 0))
         (fxmax 8 (fxsll 1 (fxmin maxbits (fxlength (fx- k 1)))))]
        [(and (bignum? k) (>= k 0)) (fxsll 1 maxbits)]
        [else ($oops who "invalid size argument ~s" k)])))

  (define $gen-hashtable-ref
    (lambda (h x v who)
      (let ([vec (ht-vec h)] [equiv? (gen-ht-equiv? h)])
        (let loop ([b (vector-ref vec (do-hash (gen-ht-hash h) x (fx- (vector-length vec) 1) who))])
          (if (null? b)
              v
              (let ([a (car b)])
                (if (equiv? (car a) x) (cdr a) (loop (cdr b)))))))))

  (define $gen-hashtable-contains?
    (lambda (h x who)
      (let ([vec (ht-vec h)] [equiv? (gen-ht-equiv? h)])
        (let loop ([b (vector-ref vec (do-hash (gen-ht-hash h) x (fx- (vector-length vec) 1) who))])
          (and (not (null? b))
               (or (equiv? (caar b) x)
                   (loop (cdr b))))))))

  (module ($gen-hashtable-set! $gen-hashtable-update! $gen-hashtable-cell $gen-hashtable-delete!)
    (define-syntax incr-size!
      (syntax-rules ()
        [(_ h vec who)
         (let ([size (fx+ (ht-size h) 1)] [n (vector-length vec)])
           (ht-size-set! h size)
           (when (and (fx> size n) (fx< n (fxsrl (most-positive-fixnum) 1)))
             (adjust! h vec (fxsll n 1) who)))]))

    (define-syntax decr-size!
      (syntax-rules ()
        [(_ h vec who)
         (let ([size (fx- (ht-size h) 1)] [n (vector-length vec)])
           (ht-size-set! h size)
           (when (and (fx< size (fxsrl n 2)) (fx> n (ht-minlen h)))
             (adjust! h vec (fxsrl n 1) who)))]))

    (define adjust!
      (lambda (h vec1 n2 who)
        (let ([vec2 (make-vector n2 '())]
              [mask2 (fx- n2 1)]
              [hash (gen-ht-hash h)])
          (vector-for-each
            (lambda (b)
              (for-each
                (lambda (a)
                  (let ([hc (do-hash hash (car a) mask2 who)])
                    (vector-set! vec2 hc (cons a (vector-ref vec2 hc)))))
                b))
            vec1)
          (ht-vec-set! h vec2))))

    (define $gen-hashtable-set!
      (lambda (h x v who)
        (let ([vec (ht-vec h)] [equiv? (gen-ht-equiv? h)])
          (let ([idx (do-hash (gen-ht-hash h) x (fx- (vector-length vec) 1) who)])
            (let ([bucket (vector-ref vec idx)])
              (let loop ([b bucket])
                (if (null? b)
                    (begin
                      (vector-set! vec idx (cons (cons x v) bucket))
                      (incr-size! h vec who))
                    (let ([a (car b)])
                      (if (equiv? (car a) x) (set-cdr! a v) (loop (cdr b)))))))))))

    (define $gen-hashtable-update!
      (lambda (h x p v who)
        (let ([vec (ht-vec h)] [equiv? (gen-ht-equiv? h)])
          (let ([idx (do-hash (gen-ht-hash h) x (fx- (vector-length vec) 1) who)])
            (let ([bucket (vector-ref vec idx)])
              (let loop ([b bucket])
                (if (null? b)
                    (begin
                      (vector-set! vec idx (cons (cons x (p v)) bucket))
                      (incr-size! h vec who))
                    (let ([a (car b)])
                      (if (equiv? (car a) x)
                          (set-cdr! a (p (cdr a)))
                          (loop (cdr b)))))))))))

    (define $gen-hashtable-cell
      (lambda (h x v who)
        (let ([vec (ht-vec h)] [equiv? (gen-ht-equiv? h)])
          (let ([idx (do-hash (gen-ht-hash h) x (fx- (vector-length vec) 1) who)])
            (let ([bucket (vector-ref vec idx)])
              (let loop ([b bucket])
                (if (null? b)
                    (let ([a (cons x v)])
                      (vector-set! vec idx (cons a bucket))
                      (incr-size! h vec who)
                      a)
                    (let ([a (car b)])
                      (if (equiv? (car a) x)
                          a
                          (loop (cdr b)))))))))))

    (define $gen-hashtable-delete!
      (lambda (h x who)
        (let ([vec (ht-vec h)] [equiv? (gen-ht-equiv? h)])
          (let ([idx (do-hash (gen-ht-hash h) x (fx- (vector-length vec) 1) who)])
            (let loop ([b (vector-ref vec idx)] [p #f])
              (unless (null? b)
                (let ([a (car b)])
                  (if (equiv? (car a) x)
                      (begin
                        (if p (set-cdr! p (cdr b)) (vector-set! vec idx (cdr b)))
                        (decr-size! h vec who))
                      (loop (cdr b) b))))))))))

  (module ($gen-hashtable-copy $symbol-hashtable-copy)
    (define copy-hashtable-vector
      (lambda (h)
        (let* ([vec1 (ht-vec h)]
               [n (vector-length vec1)]
               [vec2 (make-vector n '())])
          (do ([i 0 (fx+ i 1)])
            ((fx= i n))
            (vector-set! vec2 i
              (map (lambda (a) (cons (car a) (cdr a)))
                (vector-ref vec1 i))))
          vec2)))

    (define $gen-hashtable-copy
      (lambda (h mutable?)
        (make-gen-ht 'generic mutable? (copy-hashtable-vector h) (ht-minlen h) (ht-size h)
          (gen-ht-hash h) (gen-ht-equiv? h))))

    (define $symbol-hashtable-copy
      (lambda (h mutable?)
        (make-symbol-ht 'symbol mutable? (copy-hashtable-vector h) (ht-minlen h) (ht-size h)
          (symbol-ht-equiv? h)))))

  (define $ht-hashtable-clear!
    (lambda (h minlen)
      (ht-vec-set! h (make-vector minlen '()))
      (ht-minlen-set! h minlen)
      (ht-size-set! h 0)))

  (define $ht-hashtable-keys
    (lambda (h max-sz)
      (let ([size (fxmin max-sz (ht-size h))])
        (let ([keys (make-vector size)]
              [vec (ht-vec h)])
          (let ([n (vector-length vec)])
            (let f ([i 0] [ikey 0])
              (unless (or (fx= i n) (fx= ikey size))
                (let g ([b (vector-ref vec i)] [ikey ikey])
                  (if (or (null? b) (fx= ikey size))
                      (f (fx+ i 1) ikey)
                      (begin
                        (vector-set! keys ikey (caar b))
                        (g (cdr b) (fx+ ikey 1))))))))
          keys))))

  (define $ht-hashtable-values
    (lambda (h max-sz)
      (let ([size (fxmin max-sz (ht-size h))])
        (let ([vals (make-vector size)]
              [vec (ht-vec h)])
          (let ([n (vector-length vec)])
            (let f ([i 0] [ival 0])
              (unless (or (fx= i n) (fx= ival size))
                (let g ([b (vector-ref vec i)] [ival ival])
                  (if (or (null? b) (fx= ival size))
                      (f (fx+ i 1) ival)
                      (begin
                        (vector-set! vals ival (cdar b))
                        (g (cdr b) (fx+ ival 1))))))))
          vals))))

  (define $ht-hashtable-entries
    (lambda (h max-sz)
      (let ([size (fxmin max-sz (ht-size h))])
        (let ([keys (make-vector size)]
              [vals (make-vector size)]
              [vec (ht-vec h)])
          (let ([n (vector-length vec)])
            (let f ([i 0] [ikey 0])
              (unless (or (fx= i n) (fx= ikey size))
                (let g ([b (vector-ref vec i)] [ikey ikey])
                  (if (or (null? b) (fx= ikey size))
                      (f (fx+ i 1) ikey)
                      (let ([a (car b)])
                        (vector-set! keys ikey (car a))
                        (vector-set! vals ikey (cdr a))
                        (g (cdr b) (fx+ ikey 1))))))))
          (values keys vals)))))

  (define $ht-hashtable-cells
    (lambda (h max-sz)
      (let ([size (fxmin max-sz (ht-size h))])
        (let ([cells (make-vector size)]
              [vec (ht-vec h)])
          (let ([n (vector-length vec)])
            (let f ([i 0] [icell 0])
              (unless (or (fx= i n) (fx= icell size))
                (let g ([b (vector-ref vec i)] [icell icell])
                  (if (or (null? b) (fx= icell size))
                      (f (fx+ i 1) icell)
                      (let ([a (car b)])
                        (vector-set! cells icell a)
                        (g (cdr b) (fx+ icell 1))))))))
          cells))))

  (define eqv-generic?
    (lambda (x)
      ; all numbers except fixnums must go through generic hashtable
      (or (flonum? x) (bignum? x) (ratnum? x) ($exactnum? x) ($inexactnum? x))))

  (define $eqv-hashtable-ref
    (lambda (h x v who)
      (if (eqv-generic? x)
          ($gen-hashtable-ref (eqv-ht-genht h) x v who)
          (#3%eq-hashtable-ref (eqv-ht-eqht h) x v))))

  (define $eqv-hashtable-contains?
    (lambda (h x who)
      (if (eqv-generic? x)
          ($gen-hashtable-contains? (eqv-ht-genht h) x who)
          (#3%eq-hashtable-contains? (eqv-ht-eqht h) x))))

  (define $eqv-hashtable-set!
    (lambda (h x v who)
      (if (eqv-generic? x)
          ($gen-hashtable-set! (eqv-ht-genht h) x v who)
          (#3%eq-hashtable-set! (eqv-ht-eqht h) x v))))

  (define $eqv-hashtable-update!
    (lambda (h x p v who)
      (if (eqv-generic? x)
          ($gen-hashtable-update! (eqv-ht-genht h) x p v who)
          (#3%eq-hashtable-update! (eqv-ht-eqht h) x p v))))

  (define $eqv-hashtable-cell
    (lambda (h x v who)
      (if (eqv-generic? x)
          ($gen-hashtable-cell (eqv-ht-genht h) x v who)
          (#3%eq-hashtable-cell (eqv-ht-eqht h) x v))))

  (define $eqv-hashtable-delete!
    (lambda (h x who)
      (if (eqv-generic? x)
          ($gen-hashtable-delete! (eqv-ht-genht h) x who)
          (#3%eq-hashtable-delete! (eqv-ht-eqht h) x))))

  (define $eqv-hashtable-copy
    (lambda (h mutable?)
      (make-eqv-ht 'eqv mutable?
        ($eq-hashtable-copy (eqv-ht-eqht h) mutable?)
        ($gen-hashtable-copy (eqv-ht-genht h) mutable?))))

  (module ($eqv-hashtable-keys $eqv-hashtable-values $eqv-hashtable-entries $eqv-hashtable-cells)
    (define vector-append
      (lambda (v1 v2)
        (let ([n1 (vector-length v1)] [n2 (vector-length v2)])
          (if (fx= n1 0)
              v2
              (if (fx= n2 0)
                  v1
                  (let ([v (make-vector (fx+ n1 n2))])
                    (do ([i 0 (fx+ i 1)])
                        ((fx= i n1))
                      (vector-set! v i (vector-ref v1 i)))
                    (do ([i 0 (fx+ i 1)] [j n1 (fx+ j 1)])
                        ((fx= i n2))
                      (vector-set! v j (vector-ref v2 i)))
                    v))))))
    (define $eqv-hashtable-keys
      (lambda (h max-sz)
        (let* ([keys1 ($eq-hashtable-keys (eqv-ht-eqht h) max-sz)]
               [keys2 ($ht-hashtable-keys (eqv-ht-genht h) (fx- max-sz (vector-length keys1)))])
          (vector-append keys1 keys2))))
    (define $eqv-hashtable-values
      (lambda (h max-sz)
        (let* ([vals1 ($eq-hashtable-values (eqv-ht-eqht h) max-sz)]
               [vals2 ($ht-hashtable-values (eqv-ht-genht h) (fx- max-sz (vector-length vals1)))])
          (vector-append vals1 vals2))))
    (define $eqv-hashtable-entries
      (lambda (h max-sz)
        (let*-values ([(keys1 vals1) ($eq-hashtable-entries (eqv-ht-eqht h) max-sz)]
                      [(keys2 vals2) ($ht-hashtable-entries (eqv-ht-genht h) (fx- max-sz (vector-length keys1)))])
          (values
            (vector-append keys1 keys2)
            (vector-append vals1 vals2)))))
    (define $eqv-hashtable-cells
      (lambda (h max-sz)
        (let* ([cells1 ($eq-hashtable-cells (eqv-ht-eqht h) max-sz)]
               [cells2 ($ht-hashtable-cells (eqv-ht-genht h) (fx- max-sz (vector-length cells1)))])
          (vector-append cells1 cells2)))))

  (define number-hash
    (lambda (z)
      (cond
        [(fixnum? z) (if (fx< z 0) (fxnot z) z)]
        [(flonum? z) ($flhash z)]
        [(bignum? z) (modulo z (most-positive-fixnum))]
        [(ratnum? z) (number-hash (+ (* (numerator z) 5) (denominator z)))]
        [else (logxor (lognot (number-hash (real-part z))) (number-hash (imag-part z)))])))

  (set! $make-eq-hashtable ; assumes minlen is a power of two >= 1
    (lambda (minlen subtype)
      (make-eq-ht 'eq #t ($make-eqhash-vector minlen) minlen 0 subtype)))

  (set-who! $hashtable-veclen
    (lambda (h)
      (unless (xht? h) ($oops who "~s is not a hashtable" h))
      (case (xht-type h)
        [(eqv) (values (vector-length (ht-vec (eqv-ht-eqht h))) (vector-length (ht-vec (eqv-ht-genht h))))]
        [else (vector-length (ht-vec h))])))

  (set-who! $ht-veclen
    (lambda (h)
      (unless (ht? h) ($oops who "~s is not an ht" h))
      (vector-length (ht-vec h))))

  (set-who! $ht-minlen
    (lambda (h)
      (unless (ht? h) ($oops who "~s is not an ht" h))
      (ht-minlen h)))

  (let ()
    (define report
      (lambda (h bucket-length)
        (define (rnd n) (/ (round (* (inexact n) 100)) 100))
        (let ([vec (ht-vec h)])
          (let ([n (vector-length vec)])
            (let f ([i 0] [cnt 0] [m 0] [ss 0])
              (if (= i n)
                  (let ([mean (/ cnt n)])
                    (printf
                      "size, count, max, mean, std = ~s, ~s, ~s, ~s, ~s~%"
                      n cnt m (rnd mean)
                      (rnd (* (sqrt (- (/ ss n) (* mean mean)))))))
                  (let ([k (bucket-length (vector-ref vec i))])
                    (f (+ i 1) (+ cnt k) (max k m) (+ ss (* k k))))))))))
    (define eq-bucket-length
      (lambda (b)
        (if (fixnum? b) 0 (fx1+ (eq-bucket-length ($tlc-next b))))))
    (set-who! $hashtable-report
      (lambda (h)
        (unless (xht? h) ($oops who "~s is not a hashtable" h))
        (case (xht-type h)
          [(eq) (report h eq-bucket-length)]
          [(eqv)
           (report (eqv-ht-eqht h) eq-bucket-length)
           (report (eqv-ht-genht h) length)]
          [else (report h length)]))))

 ; csv7 interface
  (set! make-hash-table
    (case-lambda
      [() ($make-eq-hashtable (constant hashtable-default-size) (constant eq-hashtable-subtype-normal))]
      [(weak?) ($make-eq-hashtable (constant hashtable-default-size)
                                   (if weak?
                                       (constant eq-hashtable-subtype-weak)
                                       (constant eq-hashtable-subtype-normal)))]))

  (set! hash-table?
    (lambda (x)
      (eq-ht? x)))

  (set-who! put-hash-table!
    (lambda (h x v)
      (unless (eq-ht? h) ($oops who "~s is not an eq hashtable" h))
      (unless (xht-mutable? h) ($oops who "~s is not mutable" h))
      (#3%eq-hashtable-set! h x v)))

  (set-who! get-hash-table
    (lambda (h x d)
      (unless (eq-ht? h) ($oops who "~s is not an eq hashtable" h))
      (#3%eq-hashtable-ref h x d)))

  (set-who! remove-hash-table!
    (lambda (h x)
      (unless (eq-ht? h) ($oops who "~s is not an eq hashtable" h))
      (unless (xht-mutable? h) ($oops who "~s is not mutable" h))
      (#3%eq-hashtable-delete! h x)))

  (set-who! hash-table-map
    (lambda (h p)
      (unless (eq-ht? h) ($oops who "~s is not an eq hashtable" h))
      (unless (procedure? p) ($oops who "~s is not a procedure" p))
      (let-values ([(keys vals) ($eq-hashtable-entries h (most-positive-fixnum))])
        (let f ([i (vector-length keys)] [ls '()])
          (if (fx= i 0)
              ls
              (let ([i (fx- i 1)])
                (f i (cons (p (vector-ref keys i) (vector-ref vals i)) ls))))))))

  (set-who! hash-table-for-each
    (lambda (h p)
      (unless (eq-ht? h) ($oops who "~s is not an eq hashtable" h))
      (unless (procedure? p) ($oops who "~s is not a procedure" p))
      (let-values ([(keys vals) ($eq-hashtable-entries h (most-positive-fixnum))])
        (vector-for-each p keys vals))))

  (set-who! make-eq-hashtable
    (case-lambda
      [() ($make-eq-hashtable (constant hashtable-default-size) (constant eq-hashtable-subtype-normal))]
      [(k) ($make-eq-hashtable (size->minlen who k) (constant eq-hashtable-subtype-normal))]))

  (set-who! make-weak-eq-hashtable
    (case-lambda
      [() ($make-eq-hashtable (constant hashtable-default-size) (constant eq-hashtable-subtype-weak))]
      [(k) ($make-eq-hashtable (size->minlen who k) (constant eq-hashtable-subtype-weak))]))

  (set-who! make-ephemeron-eq-hashtable
    (case-lambda
      [() ($make-eq-hashtable (constant hashtable-default-size) (constant eq-hashtable-subtype-ephemeron))]
      [(k) ($make-eq-hashtable (size->minlen who k) (constant eq-hashtable-subtype-ephemeron))]))

  (let ()
    (define $make-hashtable
      (lambda (minlen hash equiv?)
        (if (and (eq? hash symbol-hash)
                 (or (eq? equiv? eq?)
                     (eq? equiv? symbol=?)
                     (eq? equiv? eqv?)
                     (eq? equiv? equal?)))
            (make-symbol-ht 'symbol #t (make-vector minlen '()) minlen 0 equiv?)
            (make-gen-ht 'generic #t (make-vector minlen '()) minlen 0 hash equiv?))))
    (define $make-eqv-hashtable
      (lambda (minlen subtype)
        (make-eqv-ht 'eqv #t
          ($make-eq-hashtable minlen subtype)
          ($make-hashtable minlen number-hash eqv?))))
    (set-who! make-hashtable
      (case-lambda
        [(hash equiv?)
         (unless (procedure? hash) ($oops who "~s is not a procedure" hash))
         (unless (procedure? equiv?) ($oops who "~s is not a procedure" equiv?))
         ($make-hashtable (constant hashtable-default-size) hash equiv?)]
        [(hash equiv? k)
         (unless (procedure? hash) ($oops who "~s is not a procedure" hash))
         (unless (procedure? equiv?) ($oops who "~s is not a procedure" equiv?))
         ($make-hashtable (size->minlen who k) hash equiv?)]))
    (set-who! make-eqv-hashtable
      (case-lambda
        [() ($make-eqv-hashtable (constant hashtable-default-size) (constant eq-hashtable-subtype-normal))]
        [(k) ($make-eqv-hashtable (size->minlen who k) (constant eq-hashtable-subtype-normal))]))
    (set-who! make-weak-eqv-hashtable
      (case-lambda
        [() ($make-eqv-hashtable (constant hashtable-default-size) (constant eq-hashtable-subtype-weak))]
        [(k) ($make-eqv-hashtable (size->minlen who k) (constant eq-hashtable-subtype-weak))]))
    (set-who! make-ephemeron-eqv-hashtable
      (case-lambda
        [() ($make-eqv-hashtable (constant hashtable-default-size) (constant eq-hashtable-subtype-ephemeron))]
        [(k) ($make-eqv-hashtable (size->minlen who k) (constant eq-hashtable-subtype-ephemeron))])))

  (set! eq-hashtable-ref
    (lambda (h x v)
      (unless (eq-ht? h)
        ($oops 'eq-hashtable-ref "~s is not an eq hashtable" h))
      (#3%eq-hashtable-ref h x v)))

  (set! eq-hashtable-contains?
    (lambda (h x)
      (unless (eq-ht? h)
        ($oops 'eq-hashtable-contains? "~s is not an eq hashtable" h))
      (#3%eq-hashtable-contains? h x)))

  (set! eq-hashtable-set!
    (lambda (h x v)
      (unless (eq-ht? h)
        ($oops 'eq-hashtable-set! "~s is not an eq hashtable" h))
      (unless (xht-mutable? h)
        ($oops 'eq-hashtable-set! "~s is not mutable" h))
      (#3%eq-hashtable-set! h x v)))

  (set! eq-hashtable-update!
    (lambda (h x p v)
      (unless (eq-ht? h)
        ($oops 'eq-hashtable-update! "~s is not an eq hashtable" h))
      (unless (xht-mutable? h)
        ($oops 'eq-hashtable-update! "~s is not mutable" h))
      (unless (procedure? p)
        ($oops 'eq-hashtable-update! "~s is not a procedure" p))
      (#3%eq-hashtable-update! h x p v)))

  (set! eq-hashtable-cell
    (lambda (h x v)
      (unless (eq-ht? h)
        ($oops 'eq-hashtable-cell "~s is not an eq hashtable" h))
      (#3%eq-hashtable-cell h x v)))

  (set! eq-hashtable-delete!
    (lambda (h x)
      (unless (eq-ht? h)
        ($oops 'eq-hashtable-delete! "~s is not an eq hashtable" h))
      (unless (xht-mutable? h)
        ($oops 'eq-hashtable-delete! "~s is not mutable" h))
      (#3%eq-hashtable-delete! h x)))

  (set-who! eq-hashtable-weak?
    (lambda (h)
      (unless (eq-ht? h) ($oops who "~s is not an eq hashtable" h))
      (eq? (constant eq-hashtable-subtype-weak) (eq-ht-subtype h))))

  (set-who! eq-hashtable-ephemeron?
    (lambda (h)
      (unless (eq-ht? h) ($oops who "~s is not an eq hashtable" h))
      (eq? (constant eq-hashtable-subtype-ephemeron) (eq-ht-subtype h))))

  (set-who! hashtable-weak?
    (lambda (h)
      (unless (xht? h) ($oops who "~s is not a hashtable" h))
      (case (xht-type h)
        [(eq) (eq? (constant eq-hashtable-subtype-weak) (eq-ht-subtype h))]
        [(eqv) (eq? (constant eq-hashtable-subtype-weak) (eq-ht-subtype (eqv-ht-eqht h)))]
        [else #f])))

  (set-who! hashtable-ephemeron?
    (lambda (h)
      (unless (xht? h) ($oops who "~s is not a hashtable" h))
      (case (xht-type h)
        [(eq) (eq? (constant eq-hashtable-subtype-ephemeron) (eq-ht-subtype h))]
        [(eqv) (eq? (constant eq-hashtable-subtype-ephemeron) (eq-ht-subtype (eqv-ht-eqht h)))]
        [else #f])))

  (set-who! symbol-hashtable-ref
    (lambda (h x v)
      (unless (symbol-ht? h) ($oops who "~s is not a symbol hashtable" h))
      (unless (symbol? x) ($oops who "~s is not a symbol" x))
      (#3%symbol-hashtable-ref h x v)))

  (set-who! symbol-hashtable-contains?
    (lambda (h x)
      (unless (symbol-ht? h) ($oops who "~s is not a symbol hashtable" h))
      (unless (symbol? x) ($oops who "~s is not a symbol" x))
      (#3%symbol-hashtable-contains? h x)))

  (set-who! symbol-hashtable-set!
    (lambda (h x v)
      (unless (symbol-ht? h) ($oops who "~s is not a symbol hashtable" h))
      (unless (symbol? x) ($oops who "~s is not a symbol" x))
      (unless (xht-mutable? h) ($oops who "~s is not mutable" h))
      (#3%symbol-hashtable-set! h x v)))

  (set-who! symbol-hashtable-update!
    (lambda (h x p v)
      (unless (symbol-ht? h) ($oops who "~s is not a symbol hashtable" h))
      (unless (symbol? x) ($oops who "~s is not a symbol" x))
      (unless (xht-mutable? h) ($oops who "~s is not mutable" h))
      (unless (procedure? p)
        ($oops who "~s is not a procedure" p))
      (#3%symbol-hashtable-update! h x p v)))

  (set-who! symbol-hashtable-cell
    (lambda (h x v)
      (unless (symbol-ht? h) ($oops who "~s is not a symbol hashtable" h))
      (unless (symbol? x) ($oops who "~s is not a symbol" x))
      (#3%symbol-hashtable-cell h x v)))

  (set-who! symbol-hashtable-delete!
    (lambda (h x)
      (unless (symbol-ht? h) ($oops who "~s is not a symbol hashtable" h))
      (unless (symbol? x) ($oops who "~s is not a symbol" x))
      (unless (xht-mutable? h) ($oops who "~s is not mutable" h))
      (#3%symbol-hashtable-delete! h x)))

  (set-who! hashtable-ref
    (lambda (h x v)
      (unless (xht? h)
        ($oops who "~s is not a hashtable" h))
      (case (xht-type h)
        [(eq) (#3%eq-hashtable-ref h x v)]
        [(symbol)
         (unless (symbol? x) ($oops 'symbol-hash "~s is not a symbol" x))
         (#3%symbol-hashtable-ref h x v)]
        [(eqv) ($eqv-hashtable-ref h x v who)]
        [else ($gen-hashtable-ref h x v who)])))

  (set-who! hashtable-contains?
    (lambda (h x)
      (unless (xht? h)
        ($oops who "~s is not a hashtable" h))
      (case (xht-type h)
        [(eq) (#3%eq-hashtable-contains? h x)]
        [(symbol)
         (unless (symbol? x) ($oops 'symbol-hash "~s is not a symbol" x))
         (#3%symbol-hashtable-contains? h x)]
        [(eqv) ($eqv-hashtable-contains? h x who)]
        [else ($gen-hashtable-contains? h x who)])))

  (set-who! hashtable-set!
    (lambda (h x v)
      (unless (xht? h)
        ($oops who "~s is not a hashtable" h))
      (unless (xht-mutable? h)
        ($oops who "~s is not mutable" h))
      (case (xht-type h)
        [(eq) (#3%eq-hashtable-set! h x v)]
        [(symbol)
         (unless (symbol? x) ($oops 'symbol-hash "~s is not a symbol" x))
         (#3%symbol-hashtable-set! h x v)]
        [(eqv) ($eqv-hashtable-set! h x v who)]
        [else ($gen-hashtable-set! h x v who)])))

  (set-who! hashtable-update!
    (lambda (h x p v)
      (unless (xht? h)
        ($oops who "~s is not a hashtable" h))
      (unless (xht-mutable? h)
        ($oops who "~s is not mutable" h))
      (unless (procedure? p)
        ($oops who "~s is not a procedure" p))
      (case (xht-type h)
        [(eq) (#3%eq-hashtable-update! h x p v)]
        [(symbol)
         (unless (symbol? x) ($oops 'symbol-hash "~s is not a symbol" x))
         (#3%symbol-hashtable-update! h x p v)]
        [(eqv) ($eqv-hashtable-update! h x p v who)]
        [else ($gen-hashtable-update! h x p v who)])))

  (set-who! hashtable-cell
    (lambda (h x v)
      (unless (xht? h)
        ($oops who "~s is not a hashtable" h))
      (case (xht-type h)
        [(eq) (#3%eq-hashtable-cell h x v)]
        [(symbol)
         (unless (symbol? x) ($oops 'symbol-hash "~s is not a symbol" x))
         (#3%symbol-hashtable-cell h x v)]
        [(eqv) ($eqv-hashtable-cell h x v who)]
        [else ($gen-hashtable-cell h x v who)])))

  (set-who! hashtable-delete!
    (lambda (h x)
      (unless (xht? h)
        ($oops who "~s is not a hashtable" h))
      (unless (xht-mutable? h)
        ($oops who "~s is not mutable" h))
      (case (xht-type h)
        [(eq) (#3%eq-hashtable-delete! h x)]
        [(symbol)
         (unless (symbol? x) ($oops 'symbol-hash "~s is not a symbol" x))
         (#3%symbol-hashtable-delete! h x)]
        [(eqv) ($eqv-hashtable-delete! h x who)]
        [else ($gen-hashtable-delete! h x who)])))

  (set! hashtable-copy
    (rec hashtable-copy
      (case-lambda
        [(h) (hashtable-copy h #f)]
        [(h mutable?)
         (unless (xht? h)
           ($oops 'hashtable-copy "~s is not a hashtable" h))
         (case (xht-type h)
           [(eq) ($eq-hashtable-copy h (and mutable? #t))]
           [(symbol) ($symbol-hashtable-copy h (and mutable? #t))]
           [(eqv) ($eqv-hashtable-copy h (and mutable? #t))]
           [else ($gen-hashtable-copy h (and mutable? #t))])])))

  (set-who! hashtable-clear!
    (let ()
      (case-lambda
        [(h)
         (unless (xht? h)
           ($oops who "~s is not a hashtable" h))
         (unless (xht-mutable? h)
           ($oops who "~s is not mutable" h))
         (case (xht-type h)
           [(eq) ($eq-hashtable-clear! h (ht-minlen h))]
           [(eqv)
            (let ([h (eqv-ht-eqht h)]) ($eq-hashtable-clear! h (ht-minlen h)))
            (let ([h (eqv-ht-genht h)]) ($ht-hashtable-clear! h (ht-minlen h)))]
           [else ($ht-hashtable-clear! h (ht-minlen h))])]
        [(h k)
         (unless (xht? h)
           ($oops who "~s is not a hashtable" h))
         (unless (xht-mutable? h)
           ($oops who "~s is not mutable" h))
         (let ([minlen (size->minlen who k)])
           (case (xht-type h)
             [(eq) ($eq-hashtable-clear! h minlen)]
             [(eqv)
              ($eq-hashtable-clear! (eqv-ht-eqht h) minlen)
              ($ht-hashtable-clear! (eqv-ht-genht h) minlen)]
             [else ($ht-hashtable-clear! h minlen)]))])))

  (let ()
    (define (invalid-length who max-sz)
      ($oops who "~s is not a valid length" max-sz))
    (define (invalid-table who h)
      ($oops who "~s is not a hashtable" h))

    (define-syntax hashtable-content-dispatch
      (syntax-rules ()
        [(_ who $eq-hashtable-content $eqv-hashtable-content $ht-hashtable-content)
         (let ()
           (define (dispatch h max-sz)
             (unless (xht? h) (invalid-table who h))
             (case (xht-type h)
               [(eq) ($eq-hashtable-content h max-sz)]
               [(eqv) ($eqv-hashtable-content h max-sz)]
               [else ($ht-hashtable-content h max-sz)]))
           (case-lambda
            [(h max-sz)
             (cond
              [(fixnum? max-sz)
               (unless (fx>= max-sz 0) (invalid-length who max-sz))
               (dispatch h max-sz)]
              [(bignum? max-sz)
               (unless (>= max-sz 0) (invalid-length who max-sz))
               (dispatch h (most-positive-fixnum))]
              [else (invalid-length who max-sz)])]
            [(h) (dispatch h (most-positive-fixnum))]))]))

    (set-who! hashtable-keys
      (hashtable-content-dispatch who
                                  $eq-hashtable-keys
                                  $eqv-hashtable-keys
                                  $ht-hashtable-keys))

    (set-who! #(r6rs: hashtable-keys)
      (lambda (h)
        (unless (xht? h) (invalid-table who h))
        (case (xht-type h)
          [(eq) ($eq-hashtable-keys h (most-positive-fixnum))]
          [(eqv) ($eqv-hashtable-keys h (most-positive-fixnum))]
          [else ($ht-hashtable-keys h (most-positive-fixnum))])))

    (set-who! hashtable-values
      (hashtable-content-dispatch who
                                  $eq-hashtable-values
                                  $eqv-hashtable-values
                                  $ht-hashtable-values))

    (set-who! hashtable-entries
      (hashtable-content-dispatch who
                                  $eq-hashtable-entries
                                  $eqv-hashtable-entries
                                  $ht-hashtable-entries))

    (set-who! #(r6rs: hashtable-entries)
      (lambda (h)
        (unless (xht? h) (invalid-table who h))
        (case (xht-type h)
          [(eq) ($eq-hashtable-entries h (most-positive-fixnum))]
          [(eqv) ($eqv-hashtable-entries h (most-positive-fixnum))]
          [else ($ht-hashtable-entries h (most-positive-fixnum))])))

    (set-who! hashtable-cells
      (hashtable-content-dispatch who
                                  $eq-hashtable-cells
                                  $eqv-hashtable-cells
                                  $ht-hashtable-cells)))

  (set! hashtable-size
    (lambda (h)
      (unless (xht? h) ($oops 'hashtable-size "~s is not a hashtable" h))
      (if (eq? (xht-type h) 'eqv)
          (fx+ (ht-size (eqv-ht-eqht h))
               (ht-size (eqv-ht-genht h)))
          (ht-size h))))

  (set! hashtable-mutable?
    (lambda (h)
      (unless (xht? h)
        ($oops 'hashtable-mutable? "~s is not a hashtable" h))
      (xht-mutable? h)))

  (set! hashtable?
    (lambda (x)
      (xht? x)))

  (set! eq-hashtable?
    (lambda (x)
      (eq-ht? x)))

  (set! symbol-hashtable?
    (lambda (x)
      (symbol-ht? x)))

  (set-who! $hashtable-size->minlen
    (lambda (k)
      (size->minlen who k)))

  (set-who! hashtable-hash-function
    (lambda (h)
      (unless (xht? h) ($oops who "~s is not an eq hashtable" h))
      (case (xht-type h)
        [(eq eqv) #f]
        [(symbol) symbol-hash]
        [else (gen-ht-hash h)])))

  (set-who! hashtable-equivalence-function
    (lambda (h)
      (unless (xht? h) ($oops who "~s is not an eq hashtable" h))
      (case (xht-type h)
        [(eq) eq?]
        [(symbol) (symbol-ht-equiv? h)]
        [(eqv) eqv?]
        [else (gen-ht-equiv? h)])))

  (let ()
    (define (hcabs hc) (if (fx< hc 0) (fxnot hc) hc))

    (define (update hc k)
      (fxlogxor (#3%fx+ (#3%fxsll hc 2) hc) k))

    (define bytevector-hash
      (lambda (bv)
        (define (bvupdate hc bv i)
          (update hc (bytevector-u8-ref bv i)))
        (let ([n (bytevector-length bv)])
          (if (fx<= n 16)
              (do ([i 0 (fx+ i 1)] [hc 440697712 (bvupdate hc bv i)])
                ((fx= i n) (hcabs hc)))
              (do ([i 0 (fx+ i 1)]
                   [hc 440697712 (bvupdate hc bv i)])
                  ((fx= i 5)
                   (do ([i (fx- n 5) (fx+ i 1)]
                        [hc hc (bvupdate hc bv i)])
                       ((fx= i n)
                        (let ([stride (fxsrl n 4)])
                          (do ([i 5 (fx+ i stride)]
                               [hc hc (bvupdate hc bv i)])
                              ((fx>= i n) (hcabs hc))))))))))))

    (set-who! string-hash
      (lambda (s)
        (define (strupdate hc s i)
          (update hc (char->integer (string-ref s i))))
        (unless (string? s) ($oops who "~s is not a string" s))
        (let ([n (string-length s)])
          (if (fx<= n 16)
              (do ([i 0 (fx+ i 1)] [hc 523658599 (strupdate hc s i)])
                  ((fx= i n) (hcabs hc)))
              (do ([i 0 (fx+ i 1)]
                   [hc 523658599 (strupdate hc s i)])
                  ((fx= i 5)
                   (do ([i (fx- n 5) (fx+ i 1)]
                        [hc hc (strupdate hc s i)])
                       ((fx= i n)
                        (let ([stride (fxsrl n 4)])
                          (do ([i 5 (fx+ i stride)]
                               [hc hc (strupdate hc s i)])
                              ((fx>= i n) (hcabs hc))))))))))))

    (set-who! string-ci-hash
      (lambda (s)
        (define (charupdate hc c) (update hc (char->integer c)))
        (unless (string? s) ($oops who "~s is not a string" s))
        (let ([n (string-length s)])
          (let f ([i 0] [hc 523658599])
            (if (fx= i n)
                (hcabs hc)
                (let g ([c* ($string-char-foldcase (string-ref s i))] [hc hc])
                  (if (char? c*)
                      (f (fx+ i 1) (charupdate hc c*))
                      (g (cdr c*) (charupdate hc (car c*))))))))))

    (set-who! symbol-hash
      (lambda (x)
        (unless (symbol? x) ($oops who "~s is not a symbol" x))
        (or ($symbol-hash x)
            (and (gensym? x) (begin (gensym->unique-string x) ($symbol-hash x)))
            ($oops who "symbol hash is not set for ~s" x))))

    (set-who! equal-hash
      (lambda (x)
        (define (f x hc i)
          (let ([i (fx- i 1)])
            (cond
              [(fx<= i 0) (values hc 0)]
              [(pair? x)
               (let ([i/2 (fxsrl (fx+ i 1) 1)])
                 (let-values ([(hc i^) (f (car x) (update hc 119001092) i/2)])
                   (f (cdr x) hc (fx+ (fx- i i/2) i^))))]
              [(vector? x)
               (let ([n (vector-length x)] [hc (update hc 513566316)])
                 (if (fx= n 0)
                     (values hc i)
                     (let g ([j 0] [hc hc] [i i])
                       (if (or (fx= j n) (fx= i 0))
                           (values hc i)
                           (let ([i/2 (fxsrl (fx+ i 1) 1)])
                             (let-values ([(hc i^) (f (vector-ref x j) hc i/2)])
                               (g (fx+ j 1) hc (fx+ (fx- i i/2) i^))))))))]
              [(null? x) (values (update hc 496904691) i)]
              [(box? x) (f (unbox x) (update hc 410225874) i)]
              [(symbol? x) (values (update hc (symbol-hash x)) i)]
              [(string? x) (values (update hc (string-hash x)) i)]
              [(number? x) (values (update hc (number-hash x)) i)]
              [(bytevector? x) (values (update hc (bytevector-hash x)) i)]
              [(boolean? x) (values (update hc (if x 336200167 307585980)) i)]
              [(char? x) (values (update hc (char->integer x)) i)]
              [(and ($record? x) ($record-hash-procedure x))
               => (lambda (rec-hash)
                    (let ([new-i i])
                      (let ([sub-hc (rec-hash
                                      x
                                      (lambda (v)
                                        (if (fx<= new-i 0)
                                            0
                                            (let-values ([(sub-hc sub-i) (f v 0 i)])
                                              (set! new-i sub-i)
                                              sub-hc))))])
                        (let ([hc (update hc (if (fixnum? sub-hc)
                                                 sub-hc
                                                 (modulo (abs sub-hc) (greatest-fixnum))))])
                          (values hc new-i)))))]
              [else (values (update hc 120634730) i)])))
        (let-values ([(hc i) (f x 523658599 64)])
          (hcabs hc)))))

  (record-writer (type-descriptor hashtable)
    (lambda (x p wr)
      (display "#<hashtable>" p)))

  (record-writer (type-descriptor eq-ht)
    (lambda (x p wr)
      (display "#<eq hashtable>" p)))

  (record-writer (type-descriptor eqv-ht)
    (lambda (x p wr)
      (display "#<eqv hashtable>" p)))
)

;;; eq hashtable operations must be compiled with
;;; generate-interrupt-trap #f and optimize-level 3
;;; so they can't be interrupted by a collection
;;; see also library routines in library.ss
(eval-when (compile)
  (generate-interrupt-trap #f)
  (optimize-level 3))

(let ()
  (include "hashtable-types.ss")

  (set! $eq-hashtable-keys
    (lambda (h max-sz)
      (let ([vec (ht-vec h)] [size (fxmin max-sz (ht-size h))])
        (let ([n (vector-length vec)] [keys (make-vector size)])
          (let outer ([i 0] [j 0])
            (if (or (fx= i n) (fx= j size))
                keys
                (let inner ([b (vector-ref vec i)] [j j])
                  (if (or (fixnum? b) (fx= j size))
                      (outer (fx+ i 1) j)
                      (let ([keyval ($tlc-keyval b)])
                        (vector-set! keys j (car keyval))
                        (inner ($tlc-next b) (fx+ j 1)))))))))))

  (set! $eq-hashtable-values
    (lambda (h max-sz)
      (let ([vec (ht-vec h)] [size (fxmin max-sz (ht-size h))])
        (let ([n (vector-length vec)] [vals (make-vector size)])
          (let outer ([i 0] [j 0])
            (if (or (fx= i n) (fx= j size))
                vals
                (let inner ([b (vector-ref vec i)] [j j])
                  (if (or (fixnum? b) (fx= j size))
                      (outer (fx+ i 1) j)
                      (let ([keyval ($tlc-keyval b)])
                        (vector-set! vals j (cdr keyval))
                        (inner ($tlc-next b) (fx+ j 1)))))))))))

  (set! $eq-hashtable-entries
    (lambda (h max-sz)
      (let ([vec (ht-vec h)] [size (fxmin max-sz (ht-size h))])
        (let ([n (vector-length vec)]
              [keys (make-vector size)]
              [vals (make-vector size)])
          (let outer ([i 0] [j 0])
            (if (or (fx= i n) (fx= j size))
                (values keys vals)
                (let inner ([b (vector-ref vec i)] [j j])
                  (if (or (fixnum? b) (fx= j size))
                      (outer (fx+ i 1) j)
                      (let ([keyval ($tlc-keyval b)])
                        (vector-set! keys j (car keyval))
                        (vector-set! vals j (cdr keyval))
                        (inner ($tlc-next b) (fx+ j 1)))))))))))

  (set! $eq-hashtable-cells
    (lambda (h max-sz)
      (let ([vec (ht-vec h)] [size (fxmin max-sz (ht-size h))])
        (let ([n (vector-length vec)] [cells (make-vector size)])
          (let outer ([i 0] [j 0])
            (if (or (fx= i n) (fx= j size))
                cells
                (let inner ([b (vector-ref vec i)] [j j])
                  (if (or (fixnum? b) (fx= j size))
                      (outer (fx+ i 1) j)
                      (let ([keyval ($tlc-keyval b)])
                        (vector-set! cells j keyval)
                        (inner ($tlc-next b) (fx+ j 1)))))))))))

  (set! $eq-hashtable-copy
    (lambda (h1 mutable?)
      (let ([subtype (eq-ht-subtype h1)])
        (let* ([vec1 (ht-vec h1)]
               [n (vector-length vec1)]
               [vec2 ($make-eqhash-vector n)]
               [h2 (make-eq-ht 'eq mutable? vec2 (ht-minlen h1) (ht-size h1) subtype)])
          (let outer ([i 0])
            (if (fx= i n)
                h2
                (begin
                  (vector-set! vec2 i
                    (let inner ([b (vector-ref vec1 i)])
                      (if (fixnum? b)
                          b
                          ($make-tlc h2
                            (let* ([keyval ($tlc-keyval b)] [key (car keyval)] [val (cdr keyval)])
                              (cond
                               [(eq? subtype (constant eq-hashtable-subtype-normal)) (cons key val)]
                               [(eq? subtype (constant eq-hashtable-subtype-weak)) (weak-cons key val)]
                               [else (ephemeron-cons key val)]))
                            (inner ($tlc-next b))))))
                  (outer (fx+ i 1)))))
          h2))))

  (set! $eq-hashtable-clear!
    (lambda (h minlen)
      (let* ([vec (ht-vec h)] [n (vector-length vec)])
        (do ([i 0 (fx+ i 1)])
            ((fx= i n))
          (let loop ([b (vector-ref vec i)])
            (if (fixnum? b)
                (vector-set! vec i i)
                (let ([next ($tlc-next b)])
                  ($set-tlc-next! b #f)
                  (loop next)))))
        (ht-size-set! h 0)
        (unless (fx= n minlen)
          (ht-vec-set! h ($make-eqhash-vector minlen))))))
  
  (let ()
    ;; An equal/hash mapping contains an equal or hash procedure (or #f)
    ;; plus the rtd where the procedure was installed. It also has a weak
    ;; list of uids for child rtds that have inherited the setting, in
    ;; case the rtd's setting changes.
    (define-record-type equal/hash
      (fields maybe-proc rtd (mutable inheritors))
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (new)
          (lambda (maybe-proc rtd)
            (new maybe-proc rtd '())))))

    (let ()
      (define (get-equal/hash who rtd key)
        (unless (record-type-descriptor? rtd)
          ($oops who "~s is not a record-type descriptor" rtd))
        (let ([e/h ($sgetprop (record-type-uid  rtd) key #f)])
          (and e/h
               (eq? (equal/hash-rtd e/h) rtd)
               (equal/hash-maybe-proc e/h))))
      (define (set-equal/hash! who rtd key proc)
        (unless (record-type-descriptor? rtd)
          ($oops who "~s is not a record-type descriptor" rtd))
        (unless (or (not proc) (procedure? proc))
          ($oops who "~s is not a procedure or #f" proc))
        (with-tc-mutex
          (let* ([uid (record-type-uid rtd)]
                 [old-e/h ($sgetprop uid key #f)])
            ;; Remove the old record from anywhere that it's inherited,
            ;; and a later lookup will re-inherit:
            (when old-e/h
              (for-each
                (lambda (uid)
                  (unless (bwp-object? uid)
                    (when (eq? ($sgetprop uid key #f) old-e/h)
                      ($sremprop uid key))))
                (equal/hash-inheritors old-e/h)))
            (if proc
                ($sputprop uid key (make-equal/hash proc rtd))
                ($sremprop uid key)))))
      (set-who! record-type-equal-procedure
        (case-lambda
          [(rtd) (get-equal/hash who rtd 'equal-proc)]
          [(rtd equal-proc) (set-equal/hash! who rtd 'equal-proc equal-proc)]))
      (set-who! record-type-hash-procedure
        (case-lambda
          [(rtd) (get-equal/hash who rtd 'hash-proc)]
          [(rtd hash-proc) (set-equal/hash! who rtd 'hash-proc hash-proc)])))

    (let ()
      ;; Gets an `equal/hash` record for the given rtd, finding
      ;; it from a parent rtd and caching if necessary:
      (define (lookup-equal/hash record key)
        (let* ([rtd ($record-type-descriptor record)] [uid (record-type-uid rtd)])
          ; Get out quick w/o mutex if equal/hash record is present
          (or ($sgetprop uid key #f)
              (with-tc-mutex
                (let f ([uid uid] [rtd rtd])
                  ;; Double-check first time around to avoid a race
                  (or ($sgetprop uid key #f)
                      (let ([parent-rtd (record-type-parent rtd)])
                        (if parent-rtd
                            ;; Cache parent's value, and register as an inheritor:
                            (let ([e/h (f (record-type-uid parent-rtd) parent-rtd)])
                              (equal/hash-inheritors-set! e/h (weak-cons uid (equal/hash-inheritors e/h)))
                              ($sputprop uid key e/h)
                              e/h)
                            ;; Cache an empty `equal/hash` record:
                            (let ([e/h (make-equal/hash #f rtd)])
                              ($sputprop uid key e/h)
                              e/h)))))))))
      (let ()
        (define (lookup-equal-procedure record1 record2)
          (let ([e/h (lookup-equal/hash record1 'equal-proc)])
            (let ([proc (equal/hash-maybe-proc e/h)])
              (if proc
                  (and
                    (eq? (equal/hash-rtd (lookup-equal/hash record2 'equal-proc)) (equal/hash-rtd e/h))
                    proc)
                  (let ([default-proc (default-record-equal-procedure)])
                    (and default-proc
                         (not (equal/hash-maybe-proc (lookup-equal/hash record2 'equal-proc)))
                         default-proc))))))
        (set-who! $record-equal-procedure
          (lambda (record1 record2)
            (lookup-equal-procedure record1 record2)))
        (set-who! record-equal-procedure
          (lambda (record1 record2)
            (unless ($record? record1) ($oops who "~s is not a record" record1))
            (unless ($record? record2) ($oops who "~s is not a record" record2))
            (lookup-equal-procedure record1 record2))))
      (let ()
        (define (lookup-hash-procedure record)
          (or (equal/hash-maybe-proc (lookup-equal/hash record 'hash-proc))
              (default-record-hash-procedure)))
        (set-who! $record-hash-procedure
          (lambda (record)
            (lookup-hash-procedure record)))
        (set-who! record-hash-procedure
          (lambda (record)
            (unless ($record? record) ($oops who "~s is not a record" record))
            (lookup-hash-procedure record))))))
)
