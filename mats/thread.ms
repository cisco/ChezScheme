;;; thread.ms
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

; why can't this come after misc.ms?

(define-syntax when-threaded
  (lambda (x)
    (syntax-case x ()
      [(_ e ...)
       (if (threaded?)
           #'(begin (void) e ...)
           #'(void))])))

(mat engine-thread
  (let ()
    (define fattercode
      '(module (fatterfib fatter slimmer zero)
        (define zero? null?)
        (define 1+ list)
        (define 1- car)
        (define zero '())
        (define (fatter n)
          (if (= n 0)
              zero
              (1+ (fatter (- n 1)))))
        (define (slimmer x)
          (if (zero? x)
              0
              (+ (slimmer (1- x)) 1)))
        (define fatter+
          (lambda (x y)
            (if (zero? y)
                x
                (fatter+ (1+ x) (1- y)))))
        (define fatterfib
          (lambda (x)
            (if (or (zero? x) (zero? (1- x)))
                (1+ zero)
                (fatter+ (fatterfib (1- x)) (fatterfib (1- (1- x)))))))))
    (define fatter-eval
      (lambda (eval loc)
        (lambda ()
          (let f ([n 10] [a 0])
            (if (= n 0)
                (set-car! loc a)
                (f (- n 1)
                   (+ a (eval `(let ()
                                 ,fattercode
                                 (slimmer (fatterfib (fatter 10))))))))))))
  
    (define (engine-eval-test n)
      (define thread-list '())
      (define fork-thread
        (lambda (x)
          (set! thread-list (cons (make-engine x) thread-list))))
      (define locs (map list (make-list n 0)))
      (for-each
        (lambda (x) (fork-thread (fatter-eval interpret x)))
        locs)
      (let f ([q thread-list])
        (unless (null? q)
          ((car q)
            10
            (lambda (t . r) (f (cdr q)))
            (lambda (x) (f (append (cdr q) (list x)))))))
      (map car locs))

    (equal? (engine-eval-test 7) '(890 890 890 890 890 890 890)))
 )

(when-threaded
(mat thread
  (let ([m (make-mutex)] [c (make-condition)]
        [m2 (make-mutex 'mname)] [c2 (make-condition 'cname)])
    (and (mutex? m)
         (thread-condition? c)
         (mutex? m2)
         (thread-condition? c2)
         (not (mutex? c))
         (not (thread-condition? m))
         (not (mutex? c2))
         (not (thread-condition? m2))
         (not (mutex-name m))
         (not (condition-name c))
         (eq? 'mname (mutex-name m2))
         (eq? 'cname (condition-name c2))
         (not (mutex? 'mutex))
         (not (thread-condition? 'condition))))
  (thread? (get-initial-thread))
  (begin
    (include "thread-check.ss")
    (define $time-in-range?
      (lambda (start stop target)
        (let ([t (time-difference stop start)])
          (<= (abs (- (+ (time-second t) (* (time-nanosecond t) 1e-9)) target))
              0.2))))
    (define-syntax $retry-for-timing
      ;; timing tests can go wrong, especially on a share machine like
      ;; one for CI, so try a few times to reduce the chance of
      ;; failure due to a real-time delay
      (lambda (stx)
        (syntax-case stx ()
          [(_ e) #'(let loop ([n 5])
                     (or e
                         (and (> n 0)
                              (loop (- n 1)))))])))
    (andmap procedure? (list $threads $fib $thread-check $time-in-range?)))
  ($thread-check)
  (not (= (let ([n #f])
            (fork-thread (lambda () (set! n (get-thread-id))))
            (let f () (if n n (begin ($yield) (f)))))
          0))
  ($thread-check)
  (= (let ([n #f])
       (fork-thread (lambda () (set! n (get-process-id))))
       (let f () (if n n (begin ($yield) (f)))))
     (get-process-id))
  ($thread-check)
  (let ()
    (define fat+
      (lambda (x y)
        (if (zero? y)
            x
            (fat+ (1+ x) (1- y)))))
    (define fatfib
      (lambda (x)
        (if (< x 2)
            1
            (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
    (define-syntax parallel-list
      (syntax-rules ()
        [(_ x ...)
         (let ([v (make-vector (length '(x ...)) #f)]
               [m (make-mutex)]
               [c (make-condition)]
               [n (length '(x ...))])
           (and
            (with-mutex m
              (map (lambda (p i) (p i))
                (list (lambda (i)
                        (fork-thread
                         (lambda ()
                           (vector-set! v i x)
                           (with-mutex m
                             (set! n (- n 1))
                             (when (= n 0)
                               (condition-signal c))))))
                  ...)
                (iota (length '(x ...))))
              (condition-wait c m (make-time 'time-duration 0 60)))
            (vector->list v)))]))
    (define result
      (parallel-list (fatfib 26) (fatfib 27) (fatfib 28)
        (fatfib 29) (fatfib 30) (fatfib 31)))
    (or (equal? result '(196418 317811 514229 832040 1346269 2178309))
        (errorf #f "result=~s" result)))
  ($thread-check)
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m (make-time 'time-duration 250000000 1))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 1.25))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m
                   (add-duration start (make-time 'time-duration 250000000 1)))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 1.25))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m (make-time 'time-duration 0 -1))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 0.0))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m
                   (add-duration start (make-time 'time-duration 0 -1)))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 0.0))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (fork-thread
        (lambda ()
          (with-mutex m (sleep (make-time 'time-duration 250000000 0)))))
       (let* ([start (current-time)]
              [r (condition-wait c m (make-time 'time-duration 100000000 0))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 0.25))))))
  (let ([count 300] [live 0] [live-m (make-mutex)])
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test n)
        (set! live n)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread
            (lambda ()
              (chew 0)
              (with-mutex live-m (set! live (- live 1)))))))
      (gc-test 4)
      (chew 0))
     ; wait for the others to die
    (let f () (unless (= live 0) ($yield) (f)))
    #t)
  ($thread-check)
  (let ([count 300] [live 0] [live-m (make-mutex)])
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test n)
        (set! live n)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread
            (lambda ()
              (chew 0)
              (with-mutex live-m (set! live (- live 1)))))))
      (let ([m1 (make-mutex)]
            [m2 (make-mutex)]
            [c1 (make-condition)]
            [c2 (make-condition)])
       ; suspend one thread on a condition, waiting for it to get there
        (with-mutex m1
          (fork-thread
            (lambda ()
              (with-mutex m1
                (condition-signal c1)
                (condition-wait c1 m1)
                (condition-signal c1))))
          (condition-wait c1 m1))
       ; try to get another suspended on a mutex
        (mutex-acquire m2)
        (fork-thread
          (lambda ()
            (with-mutex m2 (condition-signal c2))))
       ; start some threads a-allocating 
        (gc-test 3)
       ; join in the fun
        (chew 0)
       ; release the suspended threads
        (with-mutex m1
          (condition-signal c1)
          (condition-wait c1 m1))
        (condition-wait c2 m2)
        (mutex-release m2)))
   ; wait for the others to die
    (let f () (unless (= live 0) ($yield) (f)))
    #t)
  ($thread-check)
  (let ([count 300] [live 0] [live-m (make-mutex)])
    (define fat+
      (lambda (x y)
        (if (zero? y)
            x
            (fat+ (1+ x) (1- y)))))
    (define fatfib
      (lambda (x)
        (if (< x 2)
            1
            (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test n)
        (set! live n)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread
            (lambda ()
              (chew 0)
              (with-mutex live-m (set! live (- live 1)))))))
      (let ([m1 (make-mutex)]
            [m2 (make-mutex)]
            [c1 (make-condition)]
            [c2 (make-condition)])
       ; suspend one thread on a condition, waiting for it to get there
        (with-mutex m1
          (fork-thread
            (lambda ()
              (with-mutex m1
                (condition-signal c1)
                (condition-wait c1 m1)
                (condition-signal c1))))
          (condition-wait c1 m1))
       ; try to get another suspended on a mutex
        (mutex-acquire m2)
        (fork-thread
          (lambda ()
            (with-mutex m2 (condition-signal c2))))
       ; start some threads a-allocating 
        (gc-test 5)
       ; create threads that die quickly while waiting for
       ; our count collections to occur
        (let ([live 0] [live-m (make-mutex)])
          (let f ()
            (when (> count 0)
              (fatfib 20)
              (with-mutex live-m
               ; cap the number of quickly dying threads, or ti3nb
               ; runs out of memory somewhere later in the mat run.
               ; theory is that virtual address space gets chopped
               ; up and that the main thread can't access virtual
               ; memory previously assigned to a child thread.
                (if (< live 20)
                   ; don't panic if we try to create too many...
                    (guard (c [#t (values)])
                      (fork-thread
                        (lambda ()
                          (with-mutex live-m (set! live (- live 1)))))
                      (set! live (+ live 1)))))
              (f))))
       ; release the suspended threads
        (with-mutex m1
          (condition-signal c1)
          (condition-wait c1 m1))
        (condition-wait c2 m2)
        (mutex-release m2)))
   ; wait for the others to die
    (let f () (unless (= live 0) ($yield) (f)))
    #t)
  ($thread-check)
  (let ([count 30])
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test4 n)
        (define fib
          (lambda (x)
            (if (< x 2)
                1
                (+ (fib (- x 1)) (fib (- x 2))))))
        ; stall collection for a bit
        (disable-interrupts)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread (lambda () (enable-interrupts) (chew 0))))
        (fib 35)
        (enable-interrupts))
      (gc-test4 4)
      (chew 0)
      #t))
  ($thread-check)
  (eqv?
    (let ()
      (define m (make-mutex))
      (define fib
        (lambda (n)
          (if (with-mutex m (< n 2))
              1
              (+ (fib (- n 1)) (fib (- n 2))))))
      (fib 20))
    10946)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define mgrab
      (lambda (n)
        (unless (fxzero? n)
          (mutex-acquire m)
          (mutex-release m)
          (mgrab (fx- n 1)))))
    (mgrab 100)
    #t)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define recmgrab
      (lambda (n)
        (set! m (make-mutex))
        (let f ([n n])
          (unless (fxzero? n)
            (mutex-acquire m)
            (f (fx- n 1))
            (mutex-release m)))
        (fork-thread (lambda () (mutex-acquire m) #;(pretty-print 'bye)))))
    (recmgrab 100)
    #t)
  ($thread-check)
  (eqv?
    (let ([cnt 0] [ans 0])
      (define m (make-mutex))
      (mutex-acquire m)
      (fork-thread
        (rec f
          (lambda ()
            (with-mutex m (set! cnt (+ cnt 1)))
            (if (= ans 0)
                (begin ($yield) (f))
                (set! ans (- ans))))))
      (mutex-release m)
      (let f () (when (= cnt 0) (begin ($yield) (f))))
      (set! ans 17)
      (let f () (if (< ans 0) ans (begin ($yield) (f)))))
    -17)
  ($thread-check)
  (eqv?
    (let ()
      (define ans 0)
      (define cnt 0)
      (define m (make-mutex))
      (define c (make-condition))
      (define (f n)
        (fork-thread
          (lambda ()
            (with-mutex m
              (set! ans (+ ans n))
              (set! cnt (+ cnt 1))
              (condition-wait c m)
              (set! cnt (- cnt 1))))))
      (f 1)
      (f 2)
      (f 4)
      (let f () (unless (= cnt 3) (begin ($yield) (f))))
      (with-mutex m (condition-broadcast c))
      (let f () (unless (= cnt 0) (begin ($yield) (f))))
      ans)
    7)
  ($thread-check)
  (let ([nthreads (length ($threads))])
    (define-record bounded-queue (i)
      ([vec (make-vector i)]
       [mutex (make-mutex)]
       [ready (make-condition)]
       [room (make-condition)]
       [waiting 0]
       [die? #f]))
    
    (define queue-empty?
      (lambda (bq)
        (with-mutex (bounded-queue-mutex bq)
          (= (bounded-queue-i bq)
             (vector-length (bounded-queue-vec bq))))))
    
    (define enqueue!
      (lambda (item bq)
        (let ([mutex (bounded-queue-mutex bq)])
          (with-mutex mutex
            (let loop ()
              (unless (bounded-queue-die? bq)
                (if (zero? (bounded-queue-i bq))
                    (begin
                      (condition-wait (bounded-queue-room bq) mutex)
                     ; we grab the mutex when we wake up, but some other thread may
                     ; beat us to the punch
                      (loop))
                    (let ([i (- (bounded-queue-i bq) 1)])
                      (vector-set! (bounded-queue-vec bq) i item)
                      (set-bounded-queue-i! bq i)
                      (unless (zero? (bounded-queue-waiting bq))
                        (condition-signal (bounded-queue-ready bq)))))))))))
    
    (define dequeue!
      (lambda (bq)
        (let ([mutex (bounded-queue-mutex bq)])
          (with-mutex mutex
            (let loop ()
              (unless (bounded-queue-die? bq)
                (if (= (bounded-queue-i bq) (vector-length (bounded-queue-vec bq)))
                    (begin
                      (set-bounded-queue-waiting! bq
                        (+ (bounded-queue-waiting bq) 1))
                      (condition-wait (bounded-queue-ready bq) mutex)
                      (set-bounded-queue-waiting! bq
                        (- (bounded-queue-waiting bq) 1))
                     ; we grab the mutex when we wake up, but some other thread may
                     ; beat us to the punch
                      (loop))
                    (let ([i (bounded-queue-i bq)])
                      (let ([item (vector-ref (bounded-queue-vec bq) i)])
                        (set-bounded-queue-i! bq (+ i 1))
                        (condition-signal (bounded-queue-room bq))
                        item)))))))))
    
    (define job-queue)
    
    (define fib
      (lambda (n)
        (if (< n 2)
            n
            (+ (fib (- n 2)) (fib (- n 1))))))

    (define make-job
      (let ([count 0])
        (lambda (n)
          (set! count (+ count 1))
          (printf "Adding job #~s = (lambda () (fib ~s))\n" count n)
          (cons count (lambda () (fib n))))))
    
    (define make-producer
      (lambda (n)
        (rec producer
          (lambda ()
            (printf "producer ~s posting a job\n" n)
            (enqueue! (make-job (+ 20 (random 10))) job-queue)
            (if (bounded-queue-die? job-queue)
                (printf "producer ~s dying\n" n)
                (producer))))))
    
    (define make-consumer
      (lambda (n)
        (rec consumer
          (lambda ()
            (printf "consumer ~s looking for a job~%" n)
            (let ([job (dequeue! job-queue)])
              (if (bounded-queue-die? job-queue)
                  (printf "consumer ~s dying\n" n)
                  (begin
                    (printf "consumer ~s executing job #~s~%" n (car job))
                    (printf "consumer ~s computed:  ~s~%" n ((cdr job)))
                    (consumer))))))))
    
    (define (bq-test np nc)
      (set! job-queue (make-bounded-queue (max nc np)))
      (do ([np np (- np 1)])
          ((<= np 0))
          (fork-thread (make-producer np)))
      (do ([nc nc (- nc 1)])
          ((<= nc 0))
          (fork-thread (make-consumer nc))))

    (bq-test 3 4)
    (set! ans (fib 35))
   ; flush out the waiting producers and consumers
    (set-bounded-queue-die?! job-queue #t)
    (let ()
      (let f ()
        (unless (= (length ($threads)) nthreads)
          (with-mutex (bounded-queue-mutex job-queue)
            (condition-signal (bounded-queue-room job-queue))
            (condition-signal (bounded-queue-ready job-queue)))
          ($yield)
          (f))))
    (eqv? ans 9227465))
  ($thread-check)
  (let ([ans #f])
    (define fattercode
      '(module (fatterfib fatter slimmer zero)
        (define zero? null?)
        (define 1+ list)
        (define 1- car)
        (define zero '())
        (define (fatter n)
          (if (= n 0)
              zero
              (1+ (fatter (- n 1)))))
        (define (slimmer x)
          (if (zero? x)
              0
              (+ (slimmer (1- x)) 1)))
        (define fatter+
          (lambda (x y)
            (if (zero? y)
                x
                (fatter+ (1+ x) (1- y)))))
        (define fatterfib
          (lambda (x)
            (if (or (zero? x) (zero? (1- x)))
                (1+ zero)
                (fatter+ (fatterfib (1- x)) (fatterfib (1- (1- x)))))))))
    (define fatter-eval
      (lambda (eval loc)
        (lambda ()
          (let f ([n 10] [a 0])
            (if (= n 0)
                (set-car! loc a)
                (f (- n 1)
                   (+ a (eval `(let ()
                                 ,fattercode
                                 (slimmer (fatterfib (fatter 10))))))))))))
  
    (define (thread-eval-test n)
      (define locs (map list (make-list n 0)))
      (for-each
       ; compiler is not reentrant wrt threads, so must use interpret
        (lambda (loc) (fork-thread (fatter-eval interpret loc)))
        locs)
      (let f ()
        (when (ormap zero? (map car locs))
          ($yield)
          (f)))
      (map car locs))

    (equal? (thread-eval-test 7) '(890 890 890 890 890 890 890)))
  ($thread-check)
  (equal?
    (let ([x1 #f] [x2 #f] [x3 #f] [x4 #f] [x5 #f])
      (define p (make-thread-parameter 0))
      (set! x1 (p))
      (p 3)
      (set! x2 (p))
      (fork-thread
        (lambda ()
          (set! x3 (p))
          (p 5)
          (set! x4 (p))))
      (let f () (unless x4 ($yield) (f)))
      (set! x5 (p))
      (list x1 x2 x3 x4 x5))
    '(0 3 3 5 3))
  ($thread-check)
  (equal?
    (let ([x1 #f] [x2 #f] [x3 #f] [x4 #f] [x5 #f])
      (define p (make-thread-parameter 0))
      (define m (make-mutex))
      (define c (make-condition))
      (set! x1 (p))
      (p 3)
      (set! x2 (p))
      (with-mutex m
        (fork-thread
          (lambda ()
            (set! x3 (p))
            (p 5)
            (set! x4 (p))
            (with-mutex m (condition-signal c))))
        (condition-wait c m))
      (set! x5 (p))
      (list x1 x2 x3 x4 x5))
    '(0 3 3 5 3))
  ($thread-check)
  (let ()
    (define done? #f)
    (define m (make-mutex))
    (define spin
      (lambda ()
        (unless done?
          (if (mutex-acquire m #f)
              (set! done? #t)
              (begin ($yield) (spin))))))
    (fork-thread spin)
    (fork-thread spin)
    (spin)
    done?)
  ($thread-check)
  (eqv?
    (let ()
      (define-record accumulator ()
        ([acc 0]
         [increments 0]
         [(immutable mutex) (make-mutex)]))
      (define incr!
        (lambda (a n)
          (with-mutex (accumulator-mutex a)
            (set-accumulator-acc! a (+ (accumulator-acc a) n))
            (set-accumulator-increments! a (+ (accumulator-increments a) 1)))))
      (define fact
        (lambda (x)
          (if (= x 0)
              1
              (* x (fact (- x 1))))))
      (define foo (lambda (x) (string-length (number->string (fact x)))))
      (let ()
        (define acc (make-accumulator))
        (define bar
          (lambda (x)
            (fork-thread
              (lambda ()
                (incr! acc (foo x))))))
        (bar 1100)
        (bar 1200)
        (bar 1300)
        (bar 1400)
        (bar 1500)
        (bar 1400)
        (bar 1300)
        (bar 1200)
        (bar 1100)
        (let f ()
          (unless (= (accumulator-increments acc) 9)
            ($yield)
            (f)))
        (accumulator-acc acc)))
    30777)
  ($thread-check)
  (let ()
    (define fat+
      (lambda (x y)
        (if (zero? y)
            x
            (fat+ (1+ x) (1- y)))))
    (define fatfib
      (lambda (x)
        (if (< x 2)
            1
            (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
    (set! ans (fatfib 30))
    (collect)
    (eqv? ans 1346269))
  ($thread-check)
  (eq?
    (let ()
      (define bt (foreign-procedure "(cs)backdoor_thread" (scheme-object) iptr))
      (let f ([n 100])
        (let ([q 0])
          (unless (= n 0)
            (let ([p (lambda ()
                       (let ([count 10])
                         (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
                         (parameterize ([collect-request-handler
                                         (lambda ()
                                           (set! count (- count 1))
                                           (collect))])
                           (chew 0)))
                       (set! q (+ q 7)))])
              (lock-object p)
              (bt p)
              (let f () (when (= q 0) ($yield) (f)))
              (let f () (unless (= (length ($threads)) 1) ($yield) (f)))
              (unlock-object p))
            (unless (= q 14) (errorf #f "~s isn't 14" q))
            (f (- n 1)))))
      'cool)
    'cool)
  ($thread-check)
  ;; copy of the test above, this time trying an immobile box instead of locked procedure
  (eq?
    (let ()
      (define bt (foreign-procedure "(cs)backdoor_thread" (scheme-object) iptr))
      (let f ([n 100])
        (let ([q 0])
          (unless (= n 0)
            (let ([p (lambda ()
                       (let ([count 10])
                         (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
                         (parameterize ([collect-request-handler
                                         (lambda ()
                                           (set! count (- count 1))
                                           (collect))])
                           (chew 0)))
                       (set! q (+ q 7)))])
              (let ([b (box-immobile p)])
                (bt b)
                (let f () (when (= q 0) ($yield) (f)))
                (let f () (unless (= (length ($threads)) 1) ($yield) (f)))
                (keep-live b)))
            (unless (= q 14) (errorf #f "~s isn't 14" q))
            (f (- n 1)))))
      'cool)
    'cool)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define c (make-condition))
    (define nthreads 4)
    (define saved-condition #f)
    (with-mutex m
      (let f ([n nthreads])
        (unless (= n 0)
          (fork-thread
            (lambda ()
              (guard (c [else (set! saved-condition c)])
                (let g ([k 1000])
                  (unless (= k 0)
                    (let ([op (open-file-output-port (format "testfile~s.ss" n) (file-options replace))])
                      (port-file-compressed! op)
                      (put-u8 op 104)
                      (close-port op))
                    (g (- k 1)))))
              (with-mutex m
                (set! nthreads (- nthreads 1))
                (when (= nthreads 0)
                  (condition-signal c)))))
          (f (- n 1))))
      (condition-wait c m))
    (when saved-condition (raise saved-condition))
    #t)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define c (make-condition))
    (define nthreads 4)
    (define saved-condition #f)
    (with-mutex m
      (let f ([n nthreads])
        (unless (= n 0)
          (fork-thread
            (lambda ()
              (guard (c [else (set! saved-condition c)])
                (let g ([k 1000])
                  (unless (= k 0)
                    (let ([ip (open-file-input-port (format "testfile~s.ss" n))])
                      (port-file-compressed! ip)
                      (let ([b (get-u8 ip)])
                        (unless (eqv? b 104)
                          (error #f "thread ~s read wrong value ~s" n b)))
                      (close-port ip))
                    (g (- k 1)))))
              (with-mutex m
                (set! nthreads (- nthreads 1))
                (when (= nthreads 0) (condition-signal c)))))
          (f (- n 1))))
      (condition-wait c m))
    (when saved-condition (raise saved-condition))
    #t)
  ($thread-check)
  (begin
    (module ($tt-count $tt-done!)
      (define n 0)
      (define done-mutex (make-mutex))
      (define $tt-count (lambda () n))
      (define ($tt-done!)
        (with-mutex done-mutex 
          (set! n (+ n 1)))))
    (define $tt-spam (make-thread-parameter 'main))
    (define $tt-run (make-thread-parameter void))
    (define $tt-global-mutex (make-mutex))
    (define $tt-fat
      (make-thread-parameter
        (let f ([n 100] [ls (oblist)])
          (define (pick-rem ls)
            (let f ([ls ls] [i (random (length ls))])
              (if (fx= i 0)
                  (values (car ls) (cdr ls))
                  (let-values ([(x d) (f (cdr ls) (fx- i 1))])
                    (values x (cons (car ls) d))))))
          (if (= n 0)
              '()
              (let-values ([(x ls) (pick-rem ls)])
                (cons x (f (- n 1) ls)))))))
   ; original test used copy-environment, which we don't use because
   ; environments are never collected and we don't want the remaining
   ; mats to take forever to run because the heap is full of lots of
   ; unreclaimable junk.  So we simulate some of the chores of copying
   ; an environment
    (define ($tt-chew ls)
      (let ([g* (map
                  (lambda (x)
                    (let ([g (gensym)])
                      (putprop g '*cte* (cons 'alias x))
                      (when (#%$top-level-bound? x)
                        (#%$set-top-level-value! g (#%$top-level-value x)))
                      g))
                  ls)])
        (map (lambda (g) (cdr (getprop g '*cte*))) g*)))
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '($tt-run (lambda ()
                      (do ([i 2 (1- i)])
                        ((= i 0))
                        (with-mutex $tt-global-mutex
                          (printf
                            "thread ~s iteration ~s\n"
                            ($tt-spam)
                            i))
                        (with-mutex $tt-global-mutex
                          ($tt-fat ($tt-chew ($tt-fat)))))))))
      'replace)
    (parameterize ([collect-request-handler
                    (lambda ()
                      (printf "~s collecting\n" ($tt-spam))
                      (collect))]
                   [collect-notify #t])
      (time
        (do ([i 15 (1- i)])
            ((= i 0))
          (printf "Pass ~a~%" i)
          (time
            (let* ([thread-count 25]
                   #;[compile-mutex (make-mutex)]
                   [thread-finished-mutex (make-mutex)]
                   [thread-finished-count 0]
                   [program-finished-condition (make-condition)]
                   [console-mutex $tt-global-mutex]
                   [saved-condition #f])
              (do ([i thread-count (1- i)])
                  ((= i 0))
                (mutex-acquire $tt-global-mutex)
                (parameterize ([$tt-fat ($tt-chew ($tt-fat))])
                  (mutex-release $tt-global-mutex)
                  (fork-thread
                    (lambda ()
                      (guard (c [else (set! saved-condition c)])
                        ($tt-spam i)
                        (with-mutex console-mutex
                          (printf "Starting thread ~s...~%" i))
                       ; mutex should no longer be needed as of v7.3
                        #;(with-mutex compile-mutex (load "testfile.ss"))
                       ; let's see if we can load from source w/o a mutex
                        (load "testfile.ss")
                        (($tt-run))
                       ; now let's see if we can compile and load object code
                       ; w/o a mutex
                        (compile-file "testfile.ss" (format "testfile.~s" i))
                        (load (format "testfile.~s" i))
                        (($tt-run))
                        (with-mutex console-mutex
                          (printf "Finished ~s.~%" i)))
                      ($tt-done!)
                      (with-mutex thread-finished-mutex
                        (set! thread-finished-count
                          (1+ thread-finished-count))
                        (when (= thread-finished-count
                                 thread-count)
                          (condition-signal
                            program-finished-condition)))))))
              (with-mutex thread-finished-mutex
                (unless (= thread-finished-count thread-count)
                  (condition-wait
                    program-finished-condition
                    thread-finished-mutex)))
              ($tt-done!)
              (when saved-condition (raise saved-condition)))))))
    (eqv? ($tt-count) 390))
  ($thread-check)
 ; following tests garbage collection of code created and run by multiple
 ; threads.  gc is done by one thread.  will the others find their instruction
 ; and data caches properly synchronized?
  (let ([thread-count 2] [iterations 10000])
    (equal?
      (parameterize ([collect-trip-bytes (expt 2 15)]
                     [collect-generation-radix 2]) ; using 1 risks extreme slowness via major collections
        (let ([out '()]
              [out-mutex (make-mutex)]
              [out-condition (make-condition)]
              [error? #f])
          (do ([i 0 (+ i 1)])
              ((= i thread-count))
            (fork-thread
              (lambda ()
                (guard (c [#t (with-mutex out-mutex (set! error? c))])
                  (let ([n (eval `(let ()
                                    (define zero? null?)
                                    (define 1+ list)
                                    (define 1- car)
                                    (define zero '())
                                    (define (fatter n)
                                      (if (= n 0)
                                          zero
                                          (1+ (fatter (- n 1)))))
                                    (define (slimmer x)
                                      (if (zero? x)
                                          0
                                          (+ (slimmer (1- x)) 1)))
                                    (define fatter+
                                      (lambda (x y)
                                        (if (zero? y)
                                            x
                                            (fatter+ (1+ x) (1- y)))))
                                    (define fatterfib
                                      (lambda (x)
                                        (if (or (zero? x) (zero? (1- x)))
                                            (1+ zero)
                                            (fatter+ (fatterfib (1- x)) (fatterfib (1- (1- x)))))))
                                    (let loop ([n ,iterations] [a 0])
                                      (if (= n 0)
                                          a
                                          (loop (- n 1) (+ a (slimmer (fatterfib (fatter 10)))))))))])
                    (with-mutex out-mutex
                      (set! out (cons n out))
                      (condition-signal out-condition)))))))
          (let f ()
            (printf "waiting for ~s more thread(s)\n" (- thread-count (length out)))
            (with-mutex out-mutex
              (unless (or error? 
                          (= (length out) thread-count)
                          (= (length ($threads)) $nthreads))
              (condition-wait out-condition out-mutex)))
            (cond
              [error? (raise error?)]
              [(or (= (length out) thread-count)
                   (= (length ($threads)) $nthreads))
               (printf "done\n")
               out]
              [else (f)]))))
      (make-list thread-count (* 89 iterations))))
  ($thread-check)
 ; following tries to verify proper synchronization when top-level exports
 ; build up system property list.  need a module with at least 21 exports to
 ; force syntax.ss to consult property list for imports
  (let ([thread-count 20]
        [iterations 100]
        [syms '(a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
                a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24)])
    (equal?
      (let ([out '()] [out-mutex (make-mutex)] [error? #f])
        (do ([i 0 (+ i 1)])
            ((= i thread-count))
          (fork-thread
            (lambda ()
              (guard (c [#t (set! error? c)])
                (do ([j 0 (+ j 1)])
                    ((= j iterations))
                  (eval `(letrec-syntax
                           ([frob 
                             (syntax-rules ()
                               [(_ m r) (frobh m r ,syms ,syms ())])]
                            [frobh
                             (syntax-rules ()
                               [(_ m r (s ...) () (q ...))
                                (module m (r q ...)
                                  (define-syntax r
                                    (identifier-syntax
                                      (list ,(+ (* i thread-count) j) q ...)))
                                  (define q 's)
                                  ...)]
                               [(_ m r (a ...) (p0 p ...) (q ...))
                                (frobh m r (a ...) (p ...) ($tx q ...))])])
                           (frob
                             ,(string->symbol (format "m~s-~s" i j))
                             $tx)))
                  (let ([v (eval `(let ()
                                    (import ,(string->symbol (format "m~s-~s" i j)))
                                    $tx))])
                    (with-mutex out-mutex
                      (set! out (cons v out)))))))))
        (let f ([n 0])
          (cond
            [error? (raise error?)]
            [(or (= (length out) (* thread-count iterations))
                 (= (length ($threads)) $nthreads))
             (sort (lambda (x y) (< (car x) (car y))) out)]
            [else
             (when (= (modulo n 100000) 0) (printf "waiting ~s ~s\n" n (length out)))
             (f (+ n 1))])))
      (sort (lambda (x y) (< (car x) (car y)))
        (let f ([i 0])
          (if (= i thread-count)
              '()
              (let g ([j 0])
                (if (= j iterations)
                    (f (+ i 1))
                    (cons (cons (+ (* i thread-count) j) syms) (g (+ j 1))))))))))
  ($thread-check)
  (let ([thread-count 20]
        [iterations 100])
    (equal?
      (let ([out '()] [out-mutex (make-mutex)] [error? #f])
        (do ([i 0 (+ i 1)])
            ((= i thread-count))
          (fork-thread
            (lambda ()
              (guard (c [#t (set! error? c)])
                (do ([j 0 (+ j 1)])
                    ((= j iterations))
                  (eval `(letrec-syntax
                           ([frob 
                             (syntax-rules ()
                               [(_ a)
                                (begin
                                  (define-syntax $ty (identifier-syntax ,(+ (* i thread-count) j)))
                                  (define-syntax a (identifier-syntax $ty)))])])
                           (frob ,(string->symbol (format "a~s-~s" i j)))))
                  (let ([v (eval (string->symbol (format "a~s-~s" i j)))])
                    (with-mutex out-mutex
                      (set! out (cons v out)))))))))
        (let f ([n 0])
          (cond
            [error? (raise error?)]
            [(or (= (length out) (* thread-count iterations))
                 (= (length ($threads)) $nthreads))
             (sort < out)]
            [else
             (when (= (modulo n 100000) 0) (printf "waiting ~s ~s\n" n (length out)))
             (f (+ n 1))])))
      (sort <
        (let f ([i 0])
          (if (= i thread-count)
              '()
              (let g ([j 0])
                (if (= j iterations)
                    (f (+ i 1))
                    (cons (+ (* i thread-count) j) (g (+ j 1))))))))))
  ($thread-check)
  ; this mat has some inherent starvation issues, with the main thread
  ; looping rather than waiting on a condition at initialization time and
  ; other threads looping rather than waiting on a condition when looking
  ; for work to steal.  these looping threads can hog the cpu without
  ; doing anything useful, causing progress to stall or halt.  this
  ; manifests as occasional indefinite delays under Windows and OpenBSD,
  ; and it has the potential to cause the same on other operating systems.
  ; it's not clear how to fix the mat without changing it fundamentally.
  #;(eqv?
    (let () ; from Ryan Newton
    
      (define-syntax ASSERT
        (lambda (x)
          (syntax-case x ()
            [(_ expr) #'(or expr (errorf 'ASSERT "failed: ~s" (IFCHEZ #'expr (format-syntax #'expr))))]
            ;; This form is (ASSERT integer? x) returning the value of x.
            [(_ fun val) #'(let ([v val])
                             (if (fun v) v                       
                                 (errorf 'ASSERT "failed: ~s\n Value which did not satisfy above predicate: ~s" 
                                        (IFCHEZ #'fun (format-syntax #'fun))
                                        v)))]
            )))
    
      (define test-depth 25) ;; Make a tree with 2^test-depth nodes.
    
      (define vector-build
        (lambda (n f)
          (let ([v (make-vector n)])
            (do ([i 0 (fx+ i 1)])
                ((= i n) v)
              (vector-set! v i (f i))
              ))))
      
      ;; STATE:
    
      ;; Each thread's stack has a list of frames, from newest to oldest.
      ;; We use a lock-free approach for mutating/reading the frame list.
      ;; Therefore, a thief might steal an old inactive frame, but this poses no problem.
      ;; 
      ;; A thread's "stack" must be as efficient as possible, because
      ;; it essentially replaces the native scheme stack where par calls
      ;; are concerned.  (I wonder if continuations can serve any purpose here.)
      ;; Note, head is the "bottom" and tail is the "top".  We add to tail.
      (define-record shadowstack (id head tail frames))
      
      ;; Frames are locked individually.
      ;; status may be 'available, 'stolen, or 'done
      (define-record shadowframe  (mut status oper argval))
    
      ;; There's also a global list of threads:
      (define allstacks '#()) ;; This is effectively immutable.
      (define par-finished #f)
      ;; And a mutex for global state:
      (define global-mut (make-mutex))
      (define threads-registered 1)
    
      ;; A new stack has no frames, but has a (hopefully) unique ID:
      (define (new-stack) 
        (make-shadowstack (random 10000) 
          0            ;; Head pointer.
          0            ;; Tail pointer.
          (vector-build 50 
            (lambda (_) (make-shadowframe (make-mutex) #f #f #f)))))
    
      ;; A per-thread parameter.
      (define this-stack (make-thread-parameter (new-stack)))
      
      ;; Mutated below:
      (define numprocessors #f)
    
      ;; We spin until everybody is awake.
      (define (wait-for-everybody)
        (let wait-for-threads ([n 0])
         ; attempt to prevent apparent occasional starvation on openbsd
          (when (= n 0) (printf "  ~s ~s\n" threads-registered numprocessors))
          (unless (= threads-registered numprocessors)
            (wait-for-threads (mod n 1000)))))
    
        ;; DEBUGGING:
      ;;  Pick a print:
      ;   (define (print . args) (with-mutex global-mut (apply printf args) (flush-output-port)))
      ;   (define (print . args) (apply printf args))
         (define (print . args) (void)) ;; fizzle
    
    
      ;; ----------------------------------------
    
      (define (init-par num-cpus)
        (printf "\n  Initializing PAR system for ~s threads.\n" num-cpus)
        (with-mutex global-mut   
          (ASSERT (eq? threads-registered 1))
          (set! numprocessors num-cpus)
          (set! allstacks (make-vector num-cpus))
          (vector-set! allstacks 0 (this-stack))
          ;; We fork N-1 threads (the original one counts)
          (do ([i 1 (fx+ i 1)]) ([= i num-cpus] (void))
            (vector-set! allstacks i (make-worker))))
        (wait-for-everybody)
        (printf "Everyone's awake!\n"))
    
      (define (shutdown-par) (set! par-finished #t))
      (define (par-status) 
        (printf "Par status:\n  par-finished ~s\n  allstacks: ~s\n  stacksizes: ~s\n\n"
                par-finished (vector-length allstacks)
                (map shadowstack-tail (vector->list allstacks))))
    
      ;; ----------------------------------------
    
      ;; Try to do work and mark it as done.
      (define (steal-work! frame)
        (and (eq? 'available (shadowframe-status frame))
             (mutex-acquire (shadowframe-mut frame) #f) ;; Don't block on it
             ;; From here on out we've got the mutex:
             (if (eq? 'available (shadowframe-status frame)) ;; If someone beat us here, we fizzle
                 #t 
                 (begin (mutex-release (shadowframe-mut frame)) 
                        #f))
             (begin 
               ;;(printf "STOLE work! ~s\n" frame)
               (set-shadowframe-status! frame 'stolen)
               (mutex-release (shadowframe-mut frame)) 
               ;; Then let go to do the real work:
               (set-shadowframe-argval! frame 
                  ((shadowframe-oper frame) (shadowframe-argval frame)))
               (set-shadowframe-status! frame 'done)           
               #t)))
    
      (define (find-and-steal-once!)
        (let* ([ind (random numprocessors)]
               [stack (vector-ref allstacks ind)])
          (let* ([frames (shadowstack-frames stack)]
                 [tl     (shadowstack-tail stack)])
            (let frmloop ([i 0])
              (if (fx= i tl) 
                  #f ;; No work on this processor, try again. 
                  (if (steal-work! (vector-ref frames i))
                      #t
                      (frmloop (fx+ 1 i))))))))
    
      (define (make-worker)
        (define stack (new-stack))
        (fork-thread (lambda ()                
                       (this-stack stack) ;; Initialize stack. 
                       (with-mutex global-mut ;; Register our existence.
                         (set! threads-registered (add1 threads-registered)))
                       ;; Steal work forever:
                       (let forever ()
                         (unless par-finished
                           (find-and-steal-once!)
                           (forever)))))
        stack)
    
      (define-syntax pcall
        (syntax-rules ()
          [(_ op (f x) e2)
           (let ([stack (this-stack)])
             (define (push! oper val)
               (let ([frame (vector-ref (shadowstack-frames stack) (shadowstack-tail stack))])
                 ;; Initialize the frame
                 (set-shadowframe-oper!   frame oper)
                 (set-shadowframe-argval! frame val)
                 (set-shadowframe-status! frame  'available)
                 (set-shadowstack-tail! stack (fx+ (shadowstack-tail stack) 1))
                 frame))
             (define (pop!) (set-shadowstack-tail! stack (fx- (shadowstack-tail stack) 1)))
    
             (let ([frame (push! f x)])
               (let ([val1 e2])
                 ;; We're the parent, when we get to this frame, we lock it off from all other comers.
                 ;; Thieves should do non-blocking probes.
                 (let waitloop ()
                   (mutex-acquire (shadowframe-mut frame))
                   (case (shadowframe-status frame)
                     [(available)             
                      (set-shadowframe-status! frame 'stolen) ;; Just in case...
                      (pop!) ;; Pop before we even start the thunk.
                      (mutex-release (shadowframe-mut frame))
                      (op ((shadowframe-oper frame) (shadowframe-argval frame))
                          val1)]
                     ;; Oops, they may be waiting to get back in here and set the result, let's get out quick.
                     [(stolen) 
                      ;; Let go of this so they can finish and mark it as done.
                      (mutex-release (shadowframe-mut frame))                
                      (find-and-steal-once!) ;; Meanwhile we should go try to make ourselves useful..                 
                      (waitloop)] ;; When we're done with that come back and see if our outsourced job is done.
                     ;; It was stolen and is now completed:
                     [else (pop!) 
                           (mutex-release (shadowframe-mut frame))
                           (op (shadowframe-argval frame) val1)]))
    
                 )))]))
    
      ;; Returns values in a list
      (define-syntax par
        (syntax-rules ()
          [(_ a b) (pcall list ((lambda (_) a) #f) b)]))
    
      (define-syntax parmv
        (syntax-rules ()
          [(_ a b) (pcall values ((lambda (_) a) #f) b)]))
    
      ;;================================================================================
    
      (init-par 4)
    
      (let ()
        (define (tree n)
          (if (fxzero? n) 1
              (pcall fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
        (let ([n (time (tree test-depth))])
          (shutdown-par)
          n)))
    33554432)
  ($thread-check)
 ; make sure thread can return zero values
  (eqv?
    (let ([x #f])
      (fork-thread
        (lambda ()
          (call/cc
            (lambda (k)
              (set! x 'frob)
              (k)
              (set! x 'borf)))))
      (let f () (unless x ($yield) (f)))
      x)
    'frob)
  ($thread-check)
  (equal?
    (let ()
      (define n 10)
      (define m (make-mutex))
      (define c (make-condition))
      (define-ftype A (struct (x double) (y iptr)))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define ls '())
      (ftype-set! A (x) a 3.4)
      (ftype-set! A (y) a 1)
      (with-mutex m
        (do ([n n (fx- n 1)])
          ((fx= n 0))
          (fork-thread
            (lambda ()
              (define fat+
                (lambda (x y)
                  (if (zero? y)
                      x
                      (fat+ (1+ x) (1- y)))))
              (define fatfib
                (lambda (x)
                  (if (< x 2)
                      1
                      (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
              (do ([i (fx+ 1000 n) (fx- i 1)] [q (fatfib 4) (fatfib 4)])
                ((fx= i 0)
                 (with-mutex m
                   (set! ls (cons q ls))
                   (condition-signal c)))
                (if (odd? n)
                    (ftype-locked-incr! A (y) a)
                    (when (ftype-locked-decr! A (y) a)
                      (printf "woohoo!\n")))))))
        (let loop ()
          (if (equal? (length ls) n)
              (list (ftype-ref A (x) a) (ftype-ref A (y) a))
              (begin
                (condition-wait c m)
                (loop))))))
    '(3.4 -4))
  ($thread-check)
  (begin
    (load-shared-object (let ([p (find-source "foreign1.so")])
                          p
                          (if (or (path-absolute? p)
                                  (not (equal? "" (path-parent p))))
                              p
                              ;; specify relative to current directory:
                              (path-build "." p))))
    #t)
  (equal?
    (let ()
      (define ff-locked-incr! (foreign-procedure "locked_incr" ((* iptr)) boolean))
      (define ff-locked-decr! (foreign-procedure "locked_decr" ((* iptr)) boolean))
      (define n 10)
      (define m (make-mutex))
      (define c (make-condition))
      (define-ftype A (struct (x double) (y iptr)))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define ls '())
      (ftype-set! A (x) a 3.4)
      (ftype-set! A (y) a 1)
      (ff-locked-incr! (ftype-&ref A (y) a)) ; try once in main thread, in case this errors
      (ff-locked-decr! (ftype-&ref A (y) a))
      (with-mutex m
        (do ([n n (fx- n 1)])
          ((fx= n 0))
          (fork-thread
            (lambda ()
              (define fat+
                (lambda (x y)
                  (if (zero? y)
                      x
                      (fat+ (1+ x) (1- y)))))
              (define fatfib
                (lambda (x)
                  (if (< x 2)
                      1
                      (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
              (do ([i (fx+ 1000 n) (fx- i 1)] [q (fatfib 10) (fatfib 10)])
                ((fx= i 0)
                 (with-mutex m
                   (set! ls (cons q ls))
                   (condition-signal c)))
                (if (odd? n)
                    (ff-locked-incr! (ftype-&ref A (y) a))
                    (when (ff-locked-decr! (ftype-&ref A (y) a))
                      (printf "hoowoo!\n")))))))
        (let loop ()
          (if (equal? (length ls) n)
              (list (ftype-ref A (x) a) (ftype-ref A (y) a))
              (begin
                (condition-wait c m)
                (loop))))))
    '(3.4 -4))
  ($thread-check)
  (eqv?
    (let ()
      (define n 10)
      (define m (make-mutex))
      (define c (make-condition))
      (define-ftype A (struct (x double) (y iptr)))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define ls '())
      (ftype-set! A (x) a 3.5)
      (ftype-init-lock! A (y) a)
      (with-mutex m
        (do ([n n (fx- n 1)])
          ((fx= n 0))
          (fork-thread
            (lambda ()
              (define fat+
                (lambda (x y)
                  (if (zero? y)
                      x
                      (fat+ (1+ x) (1- y)))))
              (define fatfib
                (lambda (x)
                  (if (< x 2)
                      1
                      (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
              (do ([i (+ 1000 n) (fx- i 1)] [q (fatfib 4) (fatfib 4)])
                ((fx= i 0)
                 (with-mutex m
                   (set! ls (cons q ls))
                   (condition-signal c)))
                ; disable collections so we don't rendezvous for collection while holding the lock,
                ; leaving some other thread spinning hopelessly in ftype-spin-lock!
                (with-interrupts-disabled
                  (if (odd? i)
                      (let loop () (unless (ftype-lock! A (y) a) (printf "waiting\n") (loop)))
                      (ftype-spin-lock! A (y) a))
                  (memory-order-acquire)
                  (ftype-set! A (x) a ((if (odd? n) + -) (ftype-ref A (x) a) 1.0))
                  (memory-order-release)
                  (ftype-unlock! A (y) a))))))
        (let loop ()
          (if (equal? (length ls) n)
              (ftype-ref A (x) a)
              (begin
                (condition-wait c m)
                (loop))))))
    -1.5)
  ($thread-check)
  (parameterize ([collect-request-handler void])
    (define (expect-error what thunk)
      (guard (c [(and (message-condition? c)
                      (equal? (condition-message c)
                        (format "~a is defunct" what)))])
        (thunk)
        (error #f "error expected")))
    (let ([g (make-guardian #t)])
      (g (make-mutex))
      (collect)
      (let ([m (g)])
        (expect-error 'mutex (lambda () (mutex-acquire m)))
        (expect-error 'mutex (lambda () (mutex-release m)))
        (expect-error 'mutex (lambda () (condition-wait (make-condition) m))))
      (g (make-condition))
      (collect)
      (let ([c (g)])
        (expect-error 'condition (lambda () (condition-wait c (make-mutex))))
        (expect-error 'condition (lambda () (condition-broadcast c)))
        (expect-error 'condition (lambda () (condition-signal c))))))
 )

(mat make-thread-parameter
  (begin (define p (make-thread-parameter #f not)) #t)
  (p)
  (begin (p #f) (p))
  (begin (p #t) (not (p)))
  (begin (define q (make-thread-parameter #t)) #t)
  (q)
  (begin (q #f) (not (q)))
  (begin (q #t) (q))
  (or (= (optimize-level) 3)
      (guard (c [(and (message-condition? c)
                    (equal? (condition-message c) "~s is not a procedure")
                    (irritants-condition? c)
                    (equal? (condition-irritants c) (list 2)))])
      (make-thread-parameter 1 2)))
  (begin
    (define p
      (make-thread-parameter 5
        (lambda (x) (+ x 1))))
    #t)
  (eqv? (p) 6)
  (or (= (optimize-level) 3)
      (guard (c [(and (message-condition? c)
                      (equal? (condition-message c) "~s is not a number")
                      (irritants-condition? c)
                      (equal? (condition-irritants c) (list 'a)))])
        (p 'a)))
 )

(mat cas
 (begin
   (define (check container container-ref container-cas!)
     (let ([N 1000]
           [M 4]
           [done 0]
           [m (make-mutex)]
           [c (make-condition)])
       (define (bump)
         (let loop ([i 0])
           (unless (= i N)
             (let ([v (container-ref container)])
               (if (container-cas! container v (add1 v))
                   (loop (add1 i))
                   (loop i)))))
         (mutex-acquire m)
         (set! done (add1 done))
         (condition-signal c)
         (mutex-release m))
       (let loop ([j 0])
         (when (< j M)
           (fork-thread bump)
           (loop (add1 j))))
       (mutex-acquire m)
       (let loop ()
         (cond
          [(= done M)
           (mutex-release m)]
          [else
           (condition-wait c m)
           (loop)]))
       (= (container-ref container) (* M N))))
   (check (box 0) unbox box-cas!))
 (check (vector 1 0 2) (lambda (v) (vector-ref v 1)) (lambda (v o n) (vector-cas! v 1 o n))))

(mat rendezvous-in-main
  ;; make sure that every collect rendezvous triggers the handler in the
  ;; main thread
  (let ([done? #f]
        [gc-ids (list (get-thread-id))]
        [gc-count 0]
        [m #%$tc-mutex] ; using tc-mutex means main thread is not deactivated
        [c (make-condition)])
    (parameterize ([collect-request-handler
                    (let ([orig (collect-request-handler)])
                      (lambda ()
                        (set! gc-count (add1 gc-count))
                        (unless (memv (get-thread-id) gc-ids)
                          (set! gc-ids (cons (get-thread-id) gc-ids)))
                        (orig)))])
      (let loop ([i 4])
        (unless (= i 0)
          (fork-thread (lambda () (let loop ()
                                    (unless (with-mutex m
                                               (unless done?
                                                 (condition-wait c m))
                                               done?)
                                      (collect-rendezvous)
                                      (loop)))))
          (loop (sub1 i))))
      (let loop ()
        (unless (> gc-count 1000)
          (with-mutex m (condition-broadcast c))
          (collect-rendezvous)
          (loop)))
      (with-mutex m
        (set! done? #t)
        (condition-broadcast c))
      (equal? gc-ids (list (get-thread-id)))))
  )

(mat memory-consistency
  (equal? (memory-order-acquire) (void))
  (equal? (memory-order-release) (void))
  ;; Try to make a thread see a partially constructed box
  (let ([ids '(one two three four)])
    (let ([m (make-mutex)]
          [c (make-condition)]
          [ok? #t]
          [running (length ids)]
          [v (make-vector 1000 (box (car ids)))])
      (let loop ([i running])
        (unless (= i 0)
          (fork-thread (lambda ()
                         (let ([id (list-ref ids (sub1 i))]
                               [failed? #f])
                           (let loop ([j 10000])
                             (cond
                               [(fx= j 0)
                                (mutex-acquire m)
                                (set! running (sub1 running))
                                (condition-signal c)
                                (set! ok? (and ok? (not failed?)))
                                (mutex-release m)]
                               [else
                                (let loop ([i 0])
                                  (unless (fx= i (vector-length v))
                                    (let ([b (vector-ref v i)])
                                      (unless (and (box? b)
                                                   (memq (unbox b) ids))
                                        (set! failed? #t)))
                                    (vector-set! v i (box id))
                                    (loop (fx+ i 1))))
                                (loop (fx- j 1))])))))
          (loop (sub1 i))))
      (mutex-acquire m)
      (let loop ()
        (cond
          [(not (zero? running))
           (condition-wait c m)
           (loop)]
          [else
           (mutex-release m)]))
      ok?))
)

(mat parallel-gc-regression
  (equal?
   ;; Create threads that refer to data owned by other threads, and try to
   ;; mix things up to see if we can provoke a problem in parallel collection.
   (parameterize ([collect-request-handler
                   (lambda ()
                     (let ([g (collect-maximum-generation)])
                       (collect (- g 1) (min 1 (- g 1)) (- g 1))))])
     (let loop ([j 50])
       (unless (= j 0)
         (let* ([N 8]
                [DEPTH 24]
                [common (make-vector N)])

           (define (spin i)
             (define (fib n)
               (cond
                 [(= n 0) #t]
                 [(= n 1) #f]
                 [else (cons (fib (- n 1)) (fib (- n 2)))]))
             (vector-set! common i (fib DEPTH))
             (when (= i (sub1 N))
               (collect-rendezvous)))

           (define (work i)
             (lambda ()
               (thread-preserve-ownership!)
               (set-virtual-register! 0 (make-vector 128))
               (vector-set! (virtual-register 0) 0 (make-eq-hashtable))
               (map (lambda (j)
                      (let ([r (random N)])
                        (vector-set! common r (make-vector 10 ))))
                    '(1 2 3))
               (map (lambda (j)
                      (let ([r (random N)])
                        (set-virtual-register! j (vector-ref common r))))
                    '(1 2 3))
               (spin i)
               (unless (hashtable? (vector-ref (virtual-register 0) 0))
                 (error 'oops "something went wrong"))))

           (define ths
             (let loop ([i 0])
               (cond
                 [(= i N) '()]
                 [else
                  (let ([th (fork-thread (work i))])
                    (vector-set! common i th)
                    (cons th (loop (add1 i))))])))

           (for-each thread-join ths)

           (loop (sub1 j))))))
   (void))
)

(mat wait-for-threads
  (begin
    ;; To avoid breaking later tests that use `(collect)`,
    ;; wait for any threads created here to exit
    (let ()
      (define $threads (foreign-procedure "(cs)threads" () scheme-object))
      (let loop ()
        (unless (= 1 (length ($threads)))
          (sleep (make-time 'time-duration 10000 0))
          (loop))))
    #t)
)

)
