;; The eq-bitset implementation assumes that the addresses of an
;; object won't change, so only use an eq-bitset while interrupts are
;; disabled from start to end of the eq-bitset use. The eq-bitset
;; implementation also assumes that `$fxaddress` is a perfect hash for
;; a non-immediate object.

(define (make-eq-bitset)
  (make-vector (fxsll 1 (constant eq-bitset-l1-bits)) #f))

(constant-case eq-bitset-l2-bits
  [(0)
   (define (get-l1 bm n)
     (let* ([l1 (fxsrl n (constant eq-bitset-l1-shift))]
            [bm1 (vector-ref bm l1)])
       (or bm1
           (let ([bm1 (make-fxvector (fxsll 1 (constant eq-bitset-l4-bits)) 0)])
             (vector-set! bm l1 bm1)
             bm1))))
   (define (get-l2 bm1 n) bm1)
   (define (get-l3 bm1 n) bm1)]
  [else
   (define (get-l1 bm n)
     (let* ([l1 (fxsrl n (constant eq-bitset-l1-shift))]
            [bm1 (vector-ref bm l1)])
       (or bm1
           (let ([bm1 (make-vector (fxsll 1 (constant eq-bitset-l2-bits)) #f)])
             (vector-set! bm l1 bm1)
             bm1))))

   (define (get-l2 bm1 n)
     (let* ([l2 (fxand (fxsrl n (constant eq-bitset-l2-shift))
                       (constant eq-bitset-l2-mask))]
            [bm2 (vector-ref bm1 l2)])
       (or bm2
           (let ([bm2 (make-vector (fxsll 1 (constant eq-bitset-l3-bits)) #f)])
             (vector-set! bm1 l2 bm2)
             bm2))))
   
   (define (get-l3 bm2 n)
     (let* ([l3 (fxand (fxsrl n (constant eq-bitset-l3-shift))
                       (constant eq-bitset-l3-mask))]
            [bm3 (vector-ref bm2 l3)])
       (or bm3
           (let ([bm3 (make-fxvector (fxsll 1 (constant eq-bitset-l4-bits)) 0)])
             (vector-set! bm2 l3 bm3)
             bm3))))])
   
(define (get-l4-index n)
  (fxand (fxsrl n (constant eq-bitset-l4-shift)) (constant eq-bitset-l4-mask)))

(define (get-lo-index n)
  (fxand (fxsrl n (constant eq-bitset-discard-bits))
         (constant eq-bitset-lo-mask)))

(define (eq-bitset-member? bm p)
  (let* ([n ($fxaddress p)]
         [a (get-l3 (get-l2 (get-l1 bm n) n) n)]
         [i (get-l4-index n)])
    (fxbit-set? (fxvector-ref a i) (get-lo-index n))))

(define (eq-bitset-add! bm p)
  (let* ([n ($fxaddress p)]
         [a (get-l3 (get-l2 (get-l1 bm n) n) n)]
         [i (get-l4-index n)])
    (fxvector-set! a i (fxior (fxvector-ref a i)
                              (fxsll 1 (get-lo-index n))))))

(define (eq-bitset-remove! bm p)
  (let* ([n ($fxaddress p)]
         [a (get-l3 (get-l2 (get-l1 bm n) n) n)]
         [i (get-l4-index n)])
    (fxvector-set! a i (fxand (fxvector-ref a i)
                              (fxnot (fxsll 1 (get-lo-index n)))))))
