;;; engine.ss
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

;;; Notes:
;;; The engine code defines three functions: make-engine,
;;; engine-block, and engine-return.

;;; Keyboard interrupts are caught while an engine is running
;;; and the engine disabled while the handler is running.

;;; All of the engine code is defined within local state
;;; containing the following variables:
;;;   *active*  true iff an engine is running
;;;   *exit*    the continuation to the engine invoker
;;;   *keybd*   the saved keyboard interrupt handler
;;;   *timer*   the saved timer interrupt handler


(let ()

(define-threaded *exit*)
(define-threaded *keybd*)
(define-threaded *timer*)
(define-threaded *active* #f)

(define cleanup
  (lambda (who)
    (unless *active* ($oops who "no engine active"))
    (set! *active* #f)
    (keyboard-interrupt-handler *keybd*)
    (timer-interrupt-handler *timer*)
    (set! *keybd* (void))
    (set! *exit* (void))
    (set! *timer* (void))))

(define setup
  (lambda (exit)
    (set! *active* #t)
    (set! *keybd* (keyboard-interrupt-handler))
    (keyboard-interrupt-handler (exception *keybd*))
    (set! *timer* (timer-interrupt-handler))
    (timer-interrupt-handler block)
    (set! *exit* exit)))

(define block
 ; disable engine and return the continuation
  (lambda ()
    (let ([exit *exit*])
      (cleanup 'engine-block)
      (set-timer (call/cc (lambda (k) (exit (lambda () k))))))))

(define return
 ; disable engine and return list (ticks value ...)
  (lambda (args)
    (let ([n (set-timer 0)])
      (let ([exit *exit*])
        (cleanup 'engine-return)
        (exit (lambda () (cons n args)))))))

(define exception
 ; disable engine while calling the handler
  (lambda (handler)
    (lambda args
      (let ([ticks (set-timer 0)])
        (let ([exit *exit*])
          (cleanup 'engine-exception)
          (apply handler args)
          (setup exit)
          (if (= ticks 0) (block) (set-timer ticks)))))))

(define run-engine
 ; run a continuation as an engine
  (lambda (k ticks)
    ((call/cc
       (lambda (exit)
         (set-timer 0)
         (when *active* ($oops 'engine "cannot nest engines"))
         (setup exit)
         (k ticks))))))

(define eng
 ; create an engine from a procedure or continuation
  (lambda (k)
    (lambda (ticks complete expire)
      (unless (and (fixnum? ticks) (not (negative? ticks)))
        ($oops 'engine "invalid ticks ~s" ticks))
      (unless (procedure? complete)
        ($oops 'engine "~s is not a procedure" complete))
      (unless (procedure? expire)
        ($oops 'engine "~s is not a procedure" expire))
      (if (= ticks 0)
          (expire (eng k))
          (let ([x (run-engine k ticks)])
            (if (procedure? x)
                (expire (eng x))
                (apply complete x)))))))

(set! engine-return (lambda args (return args)))

(set! engine-block (lambda () (set-timer 0) (block)))

(set! make-engine
  (lambda (x)
    (unless (procedure? x) ($oops 'make-engine "~s is not a procedure" x))
    (eng (lambda (ticks) 
           (with-exception-handler
             (lambda (c)
               (let ([ticks (set-timer 0)])
                 (let ([exit *exit*])
                   (cleanup 'raise)
                   (call/cc
                     (lambda (k)
                       (exit
                         (lambda ()
                           (let-values ([vals (raise-continuable c)])
                             (setup exit)
                             (if (= ticks 0) (block) (set-timer ticks))
                             (apply k vals)))))))))
             (lambda ()
               (set-timer ticks)
               (call-with-values x (lambda args (return args)))))))))
)
