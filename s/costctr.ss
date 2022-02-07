;;; costctr.ss
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

(let ()
  (if-feature pthreads
    (define-record-type ($cost-center $make-cost-center $cost-center?)
      (fields
        (mutable level)
        (mutable instr-count)
        (mutable alloc-count)
        (mutable time-ns)
        (mutable time-s)
        (immutable mutex))
      (nongenerative #{cost-center fgbx8g23emx4rf0txn2sr0-1})
      (opaque #t)
      (protocol
        (lambda (new)
          (lambda ()
            (new (make-thread-parameter 0) 0 0 0 0 (make-mutex))))))
    (define-record-type ($cost-center $make-cost-center $cost-center?)
      (fields
        (mutable level)
        (mutable instr-count)
        (mutable alloc-count)
        (mutable time-ns)
        (mutable time-s))
      (nongenerative #{cost-center fgbx8g23emx4rf0txn2sr0-2})
      (opaque #t)
      (protocol
        (lambda (new)
          (lambda () (new 0 0 0 0 0))))))

  (define-syntax cc-level
    (lambda (x)
      (syntax-case x ()
        [(_ x)
         (if-feature pthreads
           #'(($cost-center-level x))
           #'($cost-center-level x))])))

  (define-syntax cc-level-set!
    (lambda (x)
      (syntax-case x ()
        [(_ x v)
         (if-feature pthreads
           #'(($cost-center-level x) v)
           #'($cost-center-level-set! x v))])))

  (define $with-cost-center
    (let ()
      (define who 'with-cost-center)
      (define-syntax with-mutex-if-threaded
        (lambda (x)
          (syntax-case x ()
            [(_ mexp e0 e1 ...)
             (if-feature pthreads
               #'(with-mutex mexp e0 e1 ...)
               #'(begin e0 e1 ...))])))
      (define mod-
        (lambda (x y)
          (let ([r (- x y)])
            (if (< r 0) (+ (expt 2 64) r) r))))
      (lambda (timed? cc th)
        (define-record-type saved
          (sealed #t)
          (nongenerative)
          (fields (mutable alloc) (mutable intr) (mutable time)))
        (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
        (unless (procedure? th) ($oops who "~s is not a procedure" th))
        (let ([saved (make-saved 0 0 #f)])
          (dynamic-wind #t
            (lambda ()
              (let ([level (cc-level cc)])
                (cc-level-set! cc (fx+ level 1))
                (when (fx= level 0)
                  (saved-alloc-set! saved ($object-ref 'unsigned-64 ($tc) (constant tc-alloc-counter-disp)))
                  (saved-intr-set! saved ($object-ref 'unsigned-64 ($tc) (constant tc-instr-counter-disp)))
                  (when timed? (saved-time-set! saved (current-time 'time-thread))))))
            th
            (lambda ()
              (let ([level (cc-level cc)])
                (cc-level-set! cc (fx- level 1))
                (when (fx= level 1)
                  ; grab time first -- to use up as little as possible
                  (let* ([curr-time (and timed? (current-time 'time-thread))]
                         [alloc-count (mod- ($object-ref 'unsigned-64 ($tc) (constant tc-alloc-counter-disp)) 
                                        (saved-alloc saved))]
                         [instr-count (mod- ($object-ref 'unsigned-64 ($tc) (constant tc-instr-counter-disp))
                                        (saved-intr saved))])
                    (with-mutex-if-threaded ($cost-center-mutex cc)
                      ($cost-center-alloc-count-set! cc
                        (+ ($cost-center-alloc-count cc) alloc-count))
                      ($cost-center-instr-count-set! cc
                        (+ ($cost-center-instr-count cc) instr-count))
                      (when timed?
                        (let ([saved-time (saved-time saved)])
                          (let-values ([(s ns) (let ([ns (- (time-nanosecond curr-time) (time-nanosecond saved-time))]
                                                     [s (- (time-second curr-time) (time-second saved-time))])
                                                 (if (< ns 0)
                                                     (values (- s 1) (+ ns (expt 10 9)))
                                                     (values s ns)))])
                            (let-values ([(s ns)  (let ([ns (+ ($cost-center-time-ns cc) ns)]
                                                        [s (+ ($cost-center-time-s cc) s)])
                                                    (if (>= ns (expt 10 9))
                                                        (values (+ s 1) (- ns (expt 10 9)))
                                                        (values s ns)))])
                              ($cost-center-time-s-set! cc s)
                              ($cost-center-time-ns-set! cc ns)))))))))))))))

  (set-who! cost-center-instruction-count
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      ($cost-center-instr-count cc)))

  (set-who! cost-center-allocation-count
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      (ash ($cost-center-alloc-count cc) (constant log2-ptr-bytes))))

  (set-who! cost-center-time
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      (make-time 'time-duration ($cost-center-time-ns cc) ($cost-center-time-s cc))))
  
  (set-who! reset-cost-center!
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      ($cost-center-instr-count-set! cc 0)
      ($cost-center-alloc-count-set! cc 0)
      ($cost-center-time-s-set! cc 0)
      ($cost-center-time-ns-set! cc 0)))

  (set! cost-center? (lambda (x) ($cost-center? x)))

  (set! make-cost-center (lambda () ($make-cost-center)))

  (set! with-cost-center
    (rec with-cost-center
      (case-lambda
        [(cc th) ($with-cost-center #f cc th)]
        [(timed? cc th) ($with-cost-center timed? cc th)])))

  (record-writer (record-type-descriptor $cost-center)
    (lambda (x p wr)
      (let ([ns ($cost-center-time-ns x)] [s ($cost-center-time-s x)])
        (fprintf p "#<cost center~[~2*~:; t=~d.~9,'0d~]~[~:; i=~:*~s~]~[~:; a=~:*~s~]>"
          (+ ns s) s ns
          ($cost-center-instr-count x)
          ($cost-center-alloc-count x))))))
