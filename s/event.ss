;;; event.ss
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
(define stop-event-timer
   (lambda ()
      ($set-timer (most-positive-fixnum))))

(define start-event-timer
  (lambda ()
   ; set timer by way of $event, so recurrent calls to "set-timer" or
   ; "{dis,en}able-interrupts" can't prevent interrupts
    ($event)))

(set! set-timer
  (lambda (ticks)
    (unless (and (fixnum? ticks) (fx>= ticks 0))
       ($oops 'set-timer "~s is not a nonnegative fixnum" ticks))
    (let ([ticks-left (stop-event-timer)])
      (let ([t ($tc-field 'timer-ticks ($tc))])
        (if (fx> ticks 0)
            (begin
              ($tc-field 'something-pending ($tc) #t)
              ($tc-field 'timer-ticks ($tc) ticks))
            ($tc-field 'timer-ticks ($tc) #f))
        (if (fx= ($tc-field 'disable-count ($tc)) 0)
            (let ([old (if t (fx+ t ticks-left) 0)])
              (start-event-timer)
              old)
            (or t 0))))))

(set! disable-interrupts
  (lambda ()
    (let ([ticks (stop-event-timer)])
      (let ([disable-count ($tc-field 'disable-count ($tc))])
        (when (and (fx= disable-count 0) ($tc-field 'timer-ticks ($tc)))
          ($tc-field 'timer-ticks ($tc) (fx+ ($tc-field 'timer-ticks ($tc)) ticks)))
        (when (fx= disable-count (most-positive-fixnum))
          ($oops 'disable-interrupts
                 "too many consecutive calls to disable-interrupts"))
        (let ([disable-count (fx+ disable-count 1)])
          ($tc-field 'disable-count ($tc) disable-count)
          disable-count)))))

(set! enable-interrupts
  (lambda ()
    (let ([ticks (stop-event-timer)])
      (let ([disable-count (fx- ($tc-field 'disable-count ($tc)) 1)])
        (case disable-count
          [(-1) ($set-timer ticks) 0]
          [(0) ($tc-field 'disable-count ($tc) 0)
               (start-event-timer)
               0]
          [else ($tc-field 'disable-count ($tc) disable-count)
                disable-count])))))
)
