;; Don't try to use `meta-cond` to pick these implementations. The "target"
;; in `target-fixnum?` refers to the target of compilation at the point when
;; `target-fixnum?` is called. That can be different than the host for which
;; the implementation of `target-fixnum?` is being compiled. Using `meta-cond`
;; with `(fixnum-width)` compares to a *meta*-host: the machine being used to
;; compile the host-machine code of a compiler to the target.
;;
;; In terms of cross-compilation steps:
;;  * building patch files: host is meta-host
;;  * using patch files to build compiler: host is target
;; A `meta-cond` would compare the meta-host to the target, which is not
;; right for that second case.

(define target-fixnum?
  (cond
    [(= (constant fixnum-bits) (fixnum-width)) fixnum?]
    [(< (constant fixnum-bits) (fixnum-width))
     (lambda (x)
       (and (fixnum? x)
            (fx<= (constant most-negative-fixnum) x (constant most-positive-fixnum))))]
    [else
     (lambda (x)
       (or (fixnum? x)
           (and (bignum? x)
                (<= (constant most-negative-fixnum) x (constant most-positive-fixnum)))))]))

(define target-bignum?
  (cond
    [(= (constant fixnum-bits) (fixnum-width)) bignum?]
    [(< (constant fixnum-bits) (fixnum-width))
     (lambda (x)
       (or (bignum? x)
           (and (fixnum? x)
                (not (fx<= (constant most-negative-fixnum) x (constant most-positive-fixnum))))))]
    [else
     (lambda (x)
       (and (bignum? x)
            (not (<= (constant most-negative-fixnum) x (constant most-positive-fixnum)))))]))

(define target-fixnum-power-of-two
  (let ([vec (list->vector
              (do ([i 0 (fx+ i 1)] [m 2 (* m 2)] [ls '() (cons m ls)])
                  ((not (target-fixnum? m)) (reverse ls))))])
    (lambda (x)
      (and (target-fixnum? x)
           (let ([end (vector-length vec)])
             (let f ([i 0])
               (and (fx< i end)
                    (let ([n (vector-ref vec i)] [next (fx+ i 1)])
                      (if (= x n)
                          next
                          (and (> x n) (f next)))))))))))
