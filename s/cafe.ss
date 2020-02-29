;;; cafe.ss
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

(begin
(define default-prompt-and-read
  (lambda (n)
    (unless (and (integer? n) (>= n 0))
       ($oops 'default-prompt-and-read
              "~s is not a nonnegative integer"
              n))
    (let ([prompt (waiter-prompt-string)])
      (unless (string=? prompt "")
        (do ([n n (- n 1)])
            ((= n 0)
             (write-char #\space (console-output-port))
             (flush-output-port (console-output-port)))
            (display prompt (console-output-port))))
      (let ([x (read (console-input-port))])
         (when (and (eof-object? x) (not (string=? prompt "")))
            (newline (console-output-port))
            (flush-output-port (console-output-port)))
         x))))

(define waiter-prompt-and-read
   ($make-thread-parameter
      default-prompt-and-read
      (lambda (p)
         (unless (procedure? p)
            ($oops 'waiter-prompt-and-read "~s is not a procedure" p))
         p)))

(define waiter-write
   ($make-thread-parameter
      (lambda (x)
         (unless (eq? x (void))
            (pretty-print x (console-output-port)))
         (flush-output-port (console-output-port)))
      (lambda (p)
         (unless (procedure? p)
            ($oops 'waiter-write "~s is not a procedure" p))
         p)))

(define waiter-prompt-string
   ($make-thread-parameter
      ">"
      (lambda (s)
         (unless (string? s)
            ($oops 'waiter-prompt-string "~s is not a string" s))
         s)))

(define new-cafe)

(let ()
(define-threaded waiter-expr)
(define-threaded waiter-stat1)
(define-threaded waiter-stat2)
(define-threaded waiter-total-stats)

(define sstats-sum
  (lambda (a b)
    (define sstats-time-add
      (lambda (f a b)
        (add-duration (f a) (f b))))
    (make-sstats
      (sstats-time-add sstats-cpu a b)
      (sstats-time-add sstats-real a b)
      (+ (sstats-bytes a) (sstats-bytes b))
      (+ (sstats-gc-count a) (sstats-gc-count b))
      (sstats-time-add sstats-gc-cpu a b)
      (sstats-time-add sstats-gc-real a b)
      (+ (sstats-gc-bytes a) (sstats-gc-bytes b)))))

(define waiter
   (lambda (cafe eval)
      (let ([x ((waiter-prompt-and-read) cafe)])
         (when (eof-object? x) (exit))
         (fluid-let ([waiter-total-stats (make-sstats
                                           (make-time 'time-duration 0 0)
                                           (make-time 'time-duration 0 0)
                                           0
                                           0
                                           (make-time 'time-duration 0 0)
                                           (make-time 'time-duration 0 0)
                                           0)]
                     [waiter-expr x]
                     [waiter-stat1 (void)]
                     [waiter-stat2 (void)])
            (dynamic-wind #t
               (lambda ()
                  (set! waiter-stat1 (statistics))
                  (set! waiter-stat2 (statistics)))
               (lambda ()
                  (parameterize ([$interrupt waiter-interrupt])
                     (top-level eval x)))
               (lambda ()
                  (let ([s (statistics)])
                     (set! waiter-total-stats
                           (sstats-sum (sstats-difference
                                          (sstats-difference s waiter-stat2)
                                          (sstats-difference waiter-stat2
                                                             waiter-stat1))
                                       waiter-total-stats)))))))
      (waiter cafe eval)))

; This marks the "top-level" continuation for the debugger
(define top-level
  (lambda (eval x)
    (call/cc ; grab continuation & start a new stack segment
      (rec new-cafe
        (lambda (k)
          ($current-stack-link $null-continuation) ; toss what's below
          (call-with-values
            (lambda () (eval x))
            (lambda args (for-each (waiter-write) args)))
          (k))))))

(define waiter-interrupt
   (lambda ()
      (call/cc
         (lambda (k)
            (parameterize ([$interrupt void])
               (let ([s (statistics)])
                  (set! waiter-total-stats
                        (sstats-sum (sstats-difference
                                       (sstats-difference s waiter-stat2)
                                       (sstats-difference waiter-stat2
                                                          waiter-stat1))
                                    waiter-total-stats)))
               (clear-input-port (console-input-port))
               (let ([waiter (call/cc
                                (lambda (k)
                                   (rec f (lambda () (k f)))))])
                  (fprintf (console-output-port) "break> ")
                  (flush-output-port (console-output-port))
                  (case (let ([x (parameterize ([$interrupt waiter]
                                                [reset-handler waiter])
                                    (read (console-input-port)))])
                           (if (eof-object? x) 
                               (begin (newline (console-output-port))
                                      (flush-output-port (console-output-port))
                                      'exit)
                               x))
                     [(exit e)
                      (void)]
                     [(statistics s)
                      (parameterize ([print-level 2] [print-length 2])
                         (fprintf (console-output-port)
                                  "(time ~s)~%"
                                  waiter-expr))
                      (sstats-print waiter-total-stats (console-output-port))
                      (flush-output-port (console-output-port))
                      (waiter)]
                     [(reset r quit q)
                      (reset)]
                     [(abort a)
                      (abort)]
                     [(new-cafe n)
                      (new-cafe)
                      (waiter)]
                     [(inspect i)
                      (inspect k)
                      (waiter)]
                     [(?)
                      (fprintf (console-output-port) "
Type e to exit interrupt handler and continue
     r or q to reset scheme
     a to abort scheme
     n to enter new cafe
     i to inspect current continuation
     s to display statistics

")
                      (flush-output-port (console-output-port))
                      (waiter)]
                     [else
                      (fprintf (console-output-port)
                               "Invalid command.  Type ? for options.~%")
                      (flush-output-port (console-output-port))
                      (waiter)]))
               (set! waiter-stat1 (statistics))
               (set! waiter-stat2 (statistics)))))))

(set! $cafe ($make-thread-parameter 0))

(set! new-cafe
  (let ()
    (rec new-cafe
      (case-lambda
        [() (new-cafe eval)]
        [(eval)
         (unless (procedure? eval)
           ($oops 'new-cafe "~s is not a procedure" eval))
         (call/cc
           (lambda (k1)
             (parameterize ([exit-handler k1] [reset-handler (reset-handler)])
               (let ((k2 k1))
                 (reset-handler (lambda () (k2)))
                 (call/cc (lambda (k) (set! k2 k)))
                 (parameterize ([$cafe (+ ($cafe) 1)] [$interrupt reset])
                   (with-exception-handler
                     (lambda (c) ((base-exception-handler) c))
                     (lambda ()
                       (waiter ($cafe) eval))))))))]))))
)
)
