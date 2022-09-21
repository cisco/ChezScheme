#! ../bin/scheme --script

;;; ht.ss
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

#;(optimize-level 3)
(collect-request-handler void)

(module M (eqht symht gen-set eq-set sym-set gen-ref eq-ref
           sym-ref print-htstats)
  (define (eqht) (make-eq-hashtable))
  (define (symht) (make-hashtable symbol-hash eq?))
  (define refsym* (oblist))
  (define setsym*
    (fold-left
      (lambda (ls x i) (if (fx< (modulo i 10) 1) ls (cons x ls)))
      '()
      refsym*
      (enumerate refsym*)))
  (define gen-set
    (lambda (ht n)
      (do ([n n (fx- n 1)])
          ((fx= n 0) ht)
        (for-each
          (lambda (x) (hashtable-set! ht x (list n)))
          setsym*))))
  (define eq-set
    (lambda (ht n)
      (do ([n n (fx- n 1)])
          ((fx= n 0) ht)
        (for-each
          (lambda (x) (eq-hashtable-set! ht x (list n)))
          setsym*))))
  (define sym-set
    (lambda (ht n)
      (do ([n n (fx- n 1)])
          ((fx= n 0) ht)
        (for-each
          (lambda (x) (symbol-hashtable-set! ht x (list n)))
          setsym*))))
  (define maybe-car (lambda (x) (and x (car x))))
  (define gen-ref
    (lambda (ht n)
      (let f ([n n] [x #f])
        (if (fx= n 0)
            x
            (do ([sym* refsym* (cdr sym*)]
                 [x x (maybe-car (hashtable-ref ht (car sym*) #f))])
                ((null? sym*) (f (fx- n 1) x)))))))
  (define eq-ref
    (lambda (ht n)
      (let f ([n n] [x #f])
        (if (fx= n 0)
            x
            (do ([sym* refsym* (cdr sym*)]
                 [x x (maybe-car (eq-hashtable-ref ht (car sym*) #f))])
                ((null? sym*) (f (fx- n 1) x)))))))
  (define sym-ref
    (lambda (ht n)
      (let f ([n n] [x #f])
        (if (fx= n 0)
            x
            (do ([sym* refsym* (cdr sym*)]
                 [x x (maybe-car (symbol-hashtable-ref ht (car sym*) #f))])
                ((null? sym*) (f (fx- n 1) x)))))))
  (define print-htstats
    (let ()
      (include "hashtable-types.ss")
      (lambda (ht)
        (let ([ls** (map (if (eq-ht? ht)
                             (lambda (b)
                               (do ([b b (#%$tlc-next b)]
                                    [ls '() (cons
                                              (car (#%$tlc-keyval b))
                                              ls)])
                                   ((fixnum? b) ls)))
                             (lambda (ls) (map car ls)))
                         (vector->list (ht-vec ht)))])
          (let* ([n* (map length ls**)] [len (length n*)])
            (printf "min = ~d, max = ~d, avg = ~,2f, med = ~d, stddev = ~,2f\n"
              (apply min n*) (apply max n*) (/ (apply + n*) len)
              (list-ref (sort < n*) (quotient len 2))
              (let* ([mu (/ (apply + n*) len)])
                (sqrt
                  (/ (apply + (map (lambda (n) (expt (- n mu) 2)) n*))
                     len))))
            (printf
              "a max-size bucket: ~s\n"
              (let ([n (apply max n*)])
                (cdr (find
                       (lambda (n.ls) (= (car n.ls) n))
                       (map cons n* ls**)))))))))))

(collect 0 1)

(let ()
  (import M)
  (define millis
    (lambda (t)
      (+ (* (time-second t) 1000)
         (round (/ (time-nanosecond t) 1000000)))))
  (define runs 10)
  (define iterations (cond
                       [(pb?) 100]
                       [else 1000]))
  (define-syntax run
    (syntax-rules ()
      [(_ ?set ?ref ?make-ht)
       (let ([set ?set] [ref ?ref] [make-ht ?make-ht])
         (let loop ([runs runs] [st 0] [rt 0])
           (if (fx= runs 0)
               (begin
                 (printf "(time (~s ~s ~d) ~d)\n" '?set '?make-ht
                   iterations st)
                 (printf "(time (~s ~s ~d) ~d)\n" '?ref '?make-ht
                   iterations rt))
               (let ([ht (make-ht)])
                 (let* ([st (begin
                              (collect 0 1)
                              (let ([t (current-time 'time-process)])
                                (set ht iterations)
                                (let ([t (time-difference
                                           (current-time 'time-process)
                                           t)])
                                  (+ st (millis t)))))]
                        [rt (begin
                              (collect 0 1)
                              (let ([t (current-time 'time-process)])
                                (ref ht iterations)
                                (let ([t (time-difference
                                           (current-time 'time-process)
                                           t)])
                                  (+ rt (millis t)))))])
                   (when (= runs 1) (print-htstats ht))
                   (loop (fx- runs 1) st rt))))))]))
  (run gen-set gen-ref eqht)
  (run gen-set gen-ref symht)
  (run eq-set eq-ref eqht)
  (run sym-set sym-ref symht))
