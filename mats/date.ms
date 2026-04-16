;;; date.ms
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

(mat time
  (error? ; wrong number of arguments
    (make-time))
  (error? ; wrong number of arguments
    (make-time 'time-utc))
  (error? ; wrong number of arguments
    (make-time 'time-utc 17))
  (error? ; wrong number of arguments
    (make-time 'time-utc 17 0 50))
  (error? ; invalid type
    (make-time 'time-nonsense 17 0))
  (error? ; invalid seconds
    (make-time 'time-utc 0 #f))
  (error? ; invalid nanoseconds
    (make-time 'time-utc -1 17))
  (error? ; invalid nanoseconds
    (make-time 'time-utc #e1e9 17))
  (error? ; invalid nanoseconds
    (make-time 'time-utc #f 17))
  (error? ; wrong number of arguments
    (time?))
  (error? ; wrong number of arguments
    (time? #f 3))
  (begin
    (define $time-t1 (make-time 'time-utc (- #e1e9 1) #e1e9))
    (and (time? $time-t1) (not (date? $time-t1))))
  (error? ; wrong number of arguments
    (time-type))
  (error? ; wrong number of arguments
    (time-type $time-t1 #t))
  (error? ; not a time record
    (time-type 17))
  (error? ; wrong number of arguments
    (time-second))
  (error? ; wrong number of arguments
    (time-second $time-t1 #t))
  (error? ; not a time record
    (time-second 17))
  (error? ; wrong number of arguments
    (time-nanosecond))
  (error? ; wrong number of arguments
    (time-nanosecond $time-t1 #t))
  (error? ; not a time record
    (time-nanosecond 17))
  (error? ; wrong number of arguments
    (set-time-type!))
  (error? ; wrong number of arguments
    (set-time-type! $time-t1))
  (error? ; wrong number of arguments
    (set-time-type! $time-t1 'time-utc 0))
  (error? ; not a time record
    (set-time-type! 'time-utc 'time-utc))
  (error? ; invalid type
    (set-time-type! $time-t1 'time-nonsense))
  (error? ; wrong number of arguments
    (set-time-second!))
  (error? ; wrong number of arguments
    (set-time-second! $time-t1))
  (error? ; wrong number of arguments
    (set-time-second! $time-t1 5000 0))
  (error? ; not a time record
    (set-time-second! 5000 5000))
  (error? ; invalid second
    (set-time-second! $time-t1 'time-utc))
  (error? ; wrong number of arguments
    (set-time-nanosecond!))
  (error? ; wrong number of arguments
    (set-time-nanosecond! $time-t1))
  (error? ; wrong number of arguments
    (set-time-nanosecond! $time-t1 5000 0))
  (error? ; not a time record
    (set-time-nanosecond! 5000 5000))
  (error? ; invalid nanosecond
    (set-time-nanosecond! $time-t1 -1))
  (error? ; invalid nanosecond
    (set-time-nanosecond! $time-t1 'time-utc))
  (error? ; invalid nanosecond
    (set-time-nanosecond! $time-t1 #e1e9))
  (error?  ; wrong number of arguments
    (current-time 'time-utc #t))
  (error?  ; invalid type
    (current-time 'time-nonsense))
  (begin
    (define $time-t2 (current-time 'time-utc))
    (and (time? $time-t2) (not (date? $time-t2))))
  (begin
    (define $time-t3 (current-time 'time-monotonic))
    (and (time? $time-t3) (not (date? $time-t3))))
  (begin
    (define $time-t4 (current-time 'time-duration))
    (and (time? $time-t4) (not (date? $time-t4))))
  (begin
    (define $time-t5 (current-time 'time-process))
    (and (time? $time-t5) (not (date? $time-t5))))
  (begin
    (define $time-t6 (current-time 'time-thread))
    (and (time? $time-t6) (not (date? $time-t6))))
  (begin
    (define $time-t7 (current-time 'time-collector-cpu))
    (and (time? $time-t7) (not (date? $time-t7))))
  (begin
    (define $time-t8 (current-time 'time-collector-real))
    (and (time? $time-t8) (not (date? $time-t8))))
  (eqv? (time-type $time-t1) 'time-utc)
  (eqv? (time-type $time-t2) 'time-utc)
  (eqv? (time-type $time-t3) 'time-monotonic)
  (eqv? (time-type $time-t4) 'time-duration)
  (eqv? (time-type $time-t5) 'time-process)
  (eqv? (time-type $time-t6) 'time-thread)
  (eqv? (time-type $time-t7) 'time-collector-cpu)
  (eqv? (time-type $time-t8) 'time-collector-real)
  (eqv? (time-second $time-t1) #e1e9)
  (eqv? (time-nanosecond $time-t1) (- #e1e9 1))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (>= x 0))) (time-second $time-t2))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (>= x 0))) (time-second $time-t3))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (>= x 0))) (time-second $time-t4))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (>= x 0))) (time-second $time-t5))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (>= x 0))) (time-second $time-t6))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (>= x 0))) (time-second $time-t7))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (>= x 0))) (time-second $time-t8))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x (- #e1e9 1)))) (time-nanosecond $time-t2))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x (- #e1e9 1)))) (time-nanosecond $time-t3))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x (- #e1e9 1)))) (time-nanosecond $time-t4))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x (- #e1e9 1)))) (time-nanosecond $time-t5))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x (- #e1e9 1)))) (time-nanosecond $time-t6))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x (- #e1e9 1)))) (time-nanosecond $time-t7))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x (- #e1e9 1)))) (time-nanosecond $time-t8))
  (eqv?
    (let ([sec (+ (time-second (current-time 'time-thread)) 3)]
          [cnt 0]
          [ans 0])
      (define fib (lambda (x) (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2))))))
      (let f ()
        (when (< (time-second (current-time 'time-thread)) sec)
          (for-each
            (lambda (t)
              (let ([n (time-nanosecond (current-time t))])
                (unless (<= 0 n #e1e9)
                  (errorf #f "(time-nanosecond (current-time '~s)) = ~s" t n))))
            '(time-utc time-monotonic time-duration time-process time-thread))
          (set! ans (+ ans (fib 20)))
          (set! cnt (+ cnt 1))
          (f)))
      (/ ans cnt))
    6765)
  (begin
    (set-time-type! $time-t1 'time-monotonic)
    (eqv? (time-type $time-t1) 'time-monotonic))
  (begin
    (set-time-second! $time-t1 3)
    (eqv? (time-second $time-t1) 3))
  (begin
    (set-time-nanosecond! $time-t1 3000)
    (eqv? (time-nanosecond $time-t1) 3000))
  (error? ; wrong number of arguments
    (time=?))
  (error? ; wrong number of arguments
    (time=? $time-t1))
  (error? ; wrong number of arguments
    (time=? $time-t1 $time-t1 $time-t1))
  (error? ; invalid argument
    (time=? $time-t1 3))
  (error? ; invalid argument
    (time=? car $time-t1))
  (error? ; different types
    (time=? $time-t4 $time-t5))
  (error? ; wrong number of arguments
    (time<?))
  (error? ; wrong number of arguments
    (time<? $time-t1))
  (error? ; wrong number of arguments
    (time<? $time-t1 $time-t1 $time-t1))
  (error? ; invalid argument
    (time<? $time-t1 3))
  (error? ; invalid argument
    (time<? car $time-t1))
  (error? ; different types
    (time<? $time-t4 $time-t5))
  (error? ; wrong number of arguments
    (time<=?))
  (error? ; wrong number of arguments
    (time<=? $time-t1))
  (error? ; wrong number of arguments
    (time<=? $time-t1 $time-t1 $time-t1))
  (error? ; invalid argument
    (time<=? $time-t1 3))
  (error? ; invalid argument
    (time<=? car $time-t1))
  (error? ; different types
    (time<=? $time-t4 $time-t5))
  (error? ; wrong number of arguments
    (time>?))
  (error? ; wrong number of arguments
    (time>? $time-t1))
  (error? ; wrong number of arguments
    (time>? $time-t1 $time-t1 $time-t1))
  (error? ; invalid argument
    (time>? $time-t1 3))
  (error? ; invalid argument
    (time>? car $time-t1))
  (error? ; different types
    (time>? $time-t4 $time-t5))
  (error? ; wrong number of arguments
    (time>=?))
  (error? ; wrong number of arguments
    (time>=? $time-t1))
  (error? ; wrong number of arguments
    (time>=? $time-t1 $time-t1 $time-t1))
  (error? ; invalid argument
    (time>=? $time-t1 3))
  (error? ; invalid argument
    (time>=? car $time-t1))
  (error? ; different types
    (time>=? $time-t4 $time-t5))
  (time=? $time-t1 $time-t1)
  (time<=? $time-t1 $time-t1)
  (time>=? $time-t1 $time-t1)
  (not (time<? $time-t1 $time-t1))
  (not (time>? $time-t1 $time-t1))
  (equal?
    (let ([ta (make-time 'time-duration 200 #e1e19)]
          [tb (make-time 'time-duration 300 #e1e20)]
          [tc (make-time 'time-duration 300 #e1e20)]
          [td (make-time 'time-duration 301 #e1e20)]
          [te (make-time 'time-duration 400 #e1e21)])
      (define-syntax foo
        (syntax-rules ()
          [(_ x ...)
           (list
             (let ([t x])
               (list (time<? t x) ...
                     (time<=? t x) ...
                     (time=? t x) ...
                     (time>=? t x) ...
                     (time>? t x) ...))
             ...)]))
      (foo ta tb tc td te))
    '((#f #t #t #t #t
       #t #t #t #t #t
       #t #f #f #f #f
       #t #f #f #f #f
       #f #f #f #f #f)
      (#f #f #f #t #t
       #f #t #t #t #t
       #f #t #t #f #f
       #t #t #t #f #f
       #t #f #f #f #f)
      (#f #f #f #t #t
       #f #t #t #t #t
       #f #t #t #f #f
       #t #t #t #f #f
       #t #f #f #f #f)
      (#f #f #f #f #t
       #f #f #f #t #t
       #f #f #f #t #f
       #t #t #t #t #f
       #t #t #t #f #f)
      (#f #f #f #f #f
       #f #f #f #f #t
       #f #f #f #f #t
       #t #t #t #t #t
       #t #t #t #t #f)))
  (error? (time-difference $time-t2 $time-t3))
  (error? (add-duration $time-t3 $time-t2))
  (error? (subtract-duration $time-t3 $time-t2))
  (let ([t (make-time 'time-duration 1000000 -20)])
    (and (time? t)
         (not (date? t))
         (eqv? (time-second t) -20)
         (eqv? (time-nanosecond t) 1000000)))
  (equal?
    (let ([t1 (make-time 'time-process 999999999 7)]
          [t2 (make-time 'time-duration 10 2)])
      (let ([t3 (add-duration t1 t2)]
            [t4 (subtract-duration t1 t2)])
        (let ([t5 (time-difference t3 t1)]
              [t6 (time-difference t1 t3)]
              [t7 (time-difference t1 t4)]
              [t8 (time-difference t4 t1)])
          (list
            (list (time-second t3) (time-nanosecond t3))
            (list (time-second t4) (time-nanosecond t4))
            (time=? t5 t2)
            (list (time-second t6) (time-nanosecond t6))
            (time=? t7 t2)
            (list (time-second t8) (time-nanosecond t8))))))
    '((10 9) (5 999999989) #t (-3 999999990) #t (-3 999999990)))
  (error? (copy-time (current-date)))
  (begin
    (define $new-time-t2 (copy-time $time-t2))
    (time? $new-time-t2))
  (not (eq? $new-time-t2 $time-t2))
  (time=? $new-time-t2 $time-t2)
)

(mat date
  (error? ; wrong number of arguments
    (make-date))
  (error? ; wrong number of arguments
    (make-date 0))
  (error? ; wrong number of arguments
    (make-date 0 0))
  (error? ; wrong number of arguments
    (make-date 0 0 0))
  (error? ; wrong number of arguments
    (make-date 0 0 0 0))
  (error? ; wrong number of arguments
    (make-date 0 0 0 0 1))
  (error? ; wrong number of arguments
    (make-date 0 0 0 0 1 1))
  (error? ; wrong number of arguments
    (make-date 0 0 0 0 1 1 2007 0 0))
  (error? ; invalid nanosecond
    (make-date -1 0 0 0 1 1 2007 0))
  (error? ; invalid nanosecond
    (make-date #e1e9 0 0 0 1 1 2007 0))
  (error? ; invalid nanosecond
    (make-date 'zero 0 0 0 1 1 2007 0))
  (error? ; invalid second
    (make-date 0 -1 0 0 1 1 2007 0))
  (error? ; invalid second
    (make-date 0 62 0 0 1 1 2007 0))
  (error? ; invalid second
    (make-date 0 "hello" 0 0 1 1 2007 0))
  (error? ; invalid minute
    (make-date 0 0 -1 0 1 1 2007 0))
  (error? ; invalid minute
    (make-date 0 0 60 0 1 1 2007 0))
  (error? ; invalid minute
    (make-date 0 0 "hello" 0 1 1 2007 0))
  (error? ; invalid hour
    (make-date 0 0 0 -1 1 1 2007 0))
  (error? ; invalid hour
    (make-date 0 0 0 24 1 1 2007 0))
  (error? ; invalid hour
    (make-date 0 0 0 "hello" 1 1 2007 0))
  (error? ; invalid day
    (make-date 0 0 0 0 0 1 2007 0))
  (error? ; invalid day
    (make-date 0 0 0 0 32 1 2007 0))
  (error? ; invalid day
    (make-date 0 0 0 0 31 11 2007 0))
  (error? ; invalid day
    (make-date 0 0 0 0 29 2 2007 0))
  (error? ; invalid day
    (make-date 0 0 0 0 "hello" 1 2007 0))
  (error? ; invalid month
    (make-date 0 0 0 0 1 0 2007 0))
  (error? ; invalid month
    (make-date 0 0 0 0 1 13 2007 0))
  (error? ; invalid month
    (make-date 0 0 0 0 1 'eleven 2007 0))
  (error? ; invalid year
    (make-date 0 0 0 0 1 1 'mmvii 0))
  (error? ; invalid tz
    (make-date 0 0 0 0 1 1 2007 (* -25 60 60)))
  (error? ; invalid tz
    (make-date 0 0 0 0 1 1 2007 (* 25 60 60)))
  (error? ; invalid tz
    (make-date 0 0 0 0 1 1 2007 'est))
  (error? ; invalid tz
    (make-date 0 0 0 0 1 1 2007 "est"))
  (error? ; wrong number of arguments
    (date?))
  (error? ; wrong number of arguments
    (date? #f 3))
  (begin
    (define $date-d1 (make-date 1 2 3 4 5 6 1970 8))
    (and (date? $date-d1) (not (time? $date-d1))))
  (error? ; wrong number of arguments
    (date-nanosecond))
  (error? ; wrong number of arguments
    (date-nanosecond $date-d1 #t))
  (error? ; not a date record
    (date-nanosecond 17))
  (error? ; not a date record
    (date-nanosecond $time-t1))
  (error? ; wrong number of arguments
    (date-nanosecond))
  (error? ; wrong number of arguments
    (date-nanosecond $date-d1 #t))
  (error? ; not a date record
    (date-nanosecond 17))
  (error? ; not a date record
    (date-nanosecond $time-t1))
  (error? ; wrong number of arguments
    (date-second))
  (error? ; wrong number of arguments
    (date-second $date-d1 #t))
  (error? ; not a date record
    (date-second 17))
  (error? ; not a date record
    (date-second $time-t1))
  (error? ; wrong number of arguments
    (date-minute))
  (error? ; wrong number of arguments
    (date-minute $date-d1 #t))
  (error? ; not a date record
    (date-minute 17))
  (error? ; not a date record
    (date-minute $time-t1))
  (error? ; wrong number of arguments
    (date-hour))
  (error? ; wrong number of arguments
    (date-hour $date-d1 #t))
  (error? ; not a date record
    (date-hour 17))
  (error? ; not a date record
    (date-hour $time-t1))
  (error? ; wrong number of arguments
    (date-day))
  (error? ; wrong number of arguments
    (date-day $date-d1 #t))
  (error? ; not a date record
    (date-day 17))
  (error? ; not a date record
    (date-day $time-t1))
  (error? ; wrong number of arguments
    (date-month))
  (error? ; wrong number of arguments
    (date-month $date-d1 #t))
  (error? ; not a date record
    (date-month 17))
  (error? ; not a date record
    (date-month $time-t1))
  (error? ; wrong number of arguments
    (date-year))
  (error? ; wrong number of arguments
    (date-year $date-d1 #t))
  (error? ; not a date record
    (date-year 17))
  (error? ; not a date record
    (date-year $time-t1))
  (error? ; wrong number of arguments
    (date-week-day))
  (error? ; wrong number of arguments
    (date-week-day $date-d1 #t))
  (error? ; not a date record
    (date-week-day 17))
  (error? ; not a date record
    (date-week-day $time-t1))
  (error? ; wrong number of arguments
    (date-year-day))
  (error? ; wrong number of arguments
    (date-year-day $date-d1 #t))
  (error? ; not a date record
    (date-year-day 17))
  (error? ; not a date record
    (date-year-day $time-t1))
  (error? ; wrong number of arguments
    (date-dst?))
  (error? ; wrong number of arguments
    (date-dst? $date-d1 #t))
  (error? ; not a date record
    (date-dst? 17))
  (error? ; not a date record
    (date-dst? $time-t1))
  (error? ; wrong number of arguments
    (date-zone-offset))
  (error? ; wrong number of arguments
    (date-zone-offset $date-d1 #t))
  (error? ; not a date record
    (date-zone-offset 17))
  (error? ; not a date record
    (date-zone-offset $time-t1))
  (error? ; wrong number of arguments
    (date-zone-name))
  (error? ; wrong number of arguments
    (date-zone-name $date-d1 #t))
  (error? ; not a date record
    (date-zone-name 17))
  (error? ; not a date record
    (date-zone-name $time-t1))
  (error?  ; wrong number of arguments
    (current-date 0 #t))
  (error?  ; invalid offset
    (current-date (* -25 60 60)))
  (error?  ; invalid offset
    (current-date (* 25 60 60)))
  (begin
    (define $date-d2 (current-date))
    (and (date? $date-d2) (not (time? $date-d2))))
  (begin
    (define $date-d3 (current-date (* -5 60 60)))
    (and (date? $date-d3) (not (time? $date-d3))))
  (begin
    (define $date-d4 (current-date (* 10 60 60)))
    (and (date? $date-d4) (not (time? $date-d4))))
  (begin
    (define $date-d5 (make-date 0 1 1 1 15 6 2016))
    (and (date? $date-d5) (not (time? $date-d5))))
  (date? (make-date 0 0 0 0 1 1 1970 -24))
  (date? (make-date 999999999 59 59 23 31 12 2007 24))
  (begin
    (define $date-d8 (make-date 999999999 59 59 23 31 12 2007 24))
    #t)
  (eqv? (fixnum? 999999999)
        (fixnum? (date-nanosecond $date-d8)))
  (eqv? (date-nanosecond $date-d1) 1)
  (eqv? (date-second $date-d1) 2)
  (eqv? (date-minute $date-d1) 3)
  (eqv? (date-hour $date-d1) 4)
  (eqv? (date-day $date-d1) 5)
  (eqv? (date-month $date-d1) 6)
  (eqv? (date-year $date-d1) 1970)
  (eqv? (date-zone-offset $date-d1) 8)
  (boolean? (date-dst? $date-d5))
  (fixnum? (date-zone-offset $date-d5))
  (eqv? (date-zone-name $date-d1) #f)
  (or (string? (date-zone-name $date-d2))
      (not (date-zone-name $date-d2)))
  (eqv? (date-zone-name $date-d3) #f)
  (eqv? (date-zone-name $date-d4) #f)
  (or (string? (date-zone-name $date-d5))
      (not (date-zone-name $date-d5)))
  (begin
    (define (plausible-dst? d)
      ;; Recognize a few time zone names and correlate with the DST field.
      ;; Names like "EST" appear on Unix variants, while the long names
      ;; show up on Windows.
      (cond
       [(member (date-zone-name d) '("EST" "CST" "MST" "PST"
				     "Eastern Standard Time"
				     "Central Standard Time"
				     "Mountain Standard Time"
				     "Pacific Standard Time"))
        (eqv? (date-dst? d) #f)]
       [(member (date-zone-name d) '("EDT" "CDT" "MDT" "PDT"
				     "Eastern Daylight Time"
				     "Central Daylight Time"
				     "Mountain Daylight Time"
				     "Pacific Daylight Time"))
        (eqv? (date-dst? d) #t)]
       [else #t]))
    (plausible-dst? $date-d5))
  (begin
    (define $date-d6 (make-date 0 1 1 1 15 1 2016))
    (plausible-dst? $date-d6))
 ; check whether tz offsets are set according to DST, assuming that
 ; DST always means a 1-hour shift
  (let ([delta (time-second (time-difference (date->time-utc $date-d5)
                                             (date->time-utc $date-d6)))]
        [no-dst-delta (* 152 24 60 60)]; 152 days
        [hour-delta (* 60 60)])
    (cond
     [(and (date-dst? $date-d5) (not (date-dst? $date-d6)))
      ;; Northern-hemisphere DST reduces delta
      (= delta (- no-dst-delta hour-delta))]
     [(and (not (date-dst? $date-d5)) (date-dst? $date-d6))
      ;; Southern-hemisphere DST increases delta
      (= delta (+ no-dst-delta hour-delta))]
     [else
      ;; No DST or always DST
      (= delta no-dst-delta)]))
 ; check to make sure dst isn't screwing with our explicitly created dates
 ; when we call mktime to fill in wday and yday
  (let f ([mon 1])
    (or (= mon 13)
        (and (andmap
               (lambda (day)
                 (let ([d (make-date 5 6 7 8 day mon 2007 -18000)])
                   (and (eqv? (date-nanosecond d) 5)
                        (eqv? (date-second d) 6)
                        (eqv? (date-minute d) 7)
                        (eqv? (date-hour d) 8)
                        (eqv? (date-day d) day)
                        (eqv? (date-month d) mon)
                        (eqv? (date-year d) 2007)
                        (eqv? (date-zone-offset d) -18000))))
               '(5 10 15 20 25))
             (f (+ mon 1)))))
  (eqv? (date-zone-offset $date-d3) (* -5 60 60))
  (eqv? (date-zone-offset $date-d4) (* 10 60 60))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x 999999999))) (date-nanosecond $date-d2))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x 999999999))) (date-nanosecond $date-d3))
  ((lambda (x) (and (or (fixnum? x) (bignum? x)) (<= 0 x 999999999))) (date-nanosecond $date-d4))
  ((lambda (x) (and (fixnum? x) (<= 0 x 61))) (date-second $date-d2))
  ((lambda (x) (and (fixnum? x) (<= 0 x 61))) (date-second $date-d3))
  ((lambda (x) (and (fixnum? x) (<= 0 x 61))) (date-second $date-d4))
  ((lambda (x) (and (fixnum? x) (<= 0 x 59))) (date-minute $date-d2))
  ((lambda (x) (and (fixnum? x) (<= 0 x 59))) (date-minute $date-d3))
  ((lambda (x) (and (fixnum? x) (<= 0 x 59))) (date-minute $date-d4))
  ((lambda (x) (and (fixnum? x) (<= 0 x 23))) (date-hour $date-d2))
  ((lambda (x) (and (fixnum? x) (<= 0 x 23))) (date-hour $date-d3))
  ((lambda (x) (and (fixnum? x) (<= 0 x 23))) (date-hour $date-d4))
  ((lambda (x) (and (fixnum? x) (<= 1 x 31))) (date-day $date-d2))
  ((lambda (x) (and (fixnum? x) (<= 1 x 31))) (date-day $date-d3))
  ((lambda (x) (and (fixnum? x) (<= 1 x 31))) (date-day $date-d4))
  ((lambda (x) (and (fixnum? x) (<= 1 x 12))) (date-month $date-d2))
  ((lambda (x) (and (fixnum? x) (<= 1 x 12))) (date-month $date-d3))
  ((lambda (x) (and (fixnum? x) (<= 1 x 12))) (date-month $date-d4))
  ((lambda (x) (and (fixnum? x) (<= 1900 x 2038))) (date-year $date-d2))
  ((lambda (x) (and (fixnum? x) (<= 1900 x 2038))) (date-year $date-d3))
  ((lambda (x) (and (fixnum? x) (<= 1900 x 2038))) (date-year $date-d4))
  (let ([s (date-and-time)])
    (and (fixnum? (read (open-input-string (substring s 8 10))))
         (fixnum? (read (open-input-string (substring s 20 24))))))
  (let ([d (current-date)])
    (let ([s (date-and-time d)])
      (and (= (read (open-input-string (substring s 8 10))) (date-day d))
           (= (read (open-input-string (substring s 11 13))) (date-hour d))
           (= (read (open-input-string (substring s 20 24))) (date-year d)))))
)

(mat conversions/sleep
  (error? (date->time-utc (current-time)))
  (error? (time-utc->date (current-date)))
  (error? (sleep 20))
  (time? (date->time-utc (current-date)))
  (date? (time-utc->date (current-time 'time-utc)))
  (let ([t (current-time 'time-utc)])
    (sleep (make-time 'time-duration 0 1))
    (time<? t (date->time-utc (current-date))))
  (let ([t (current-time)])
    (and
     (time=? (date->time-utc (time-utc->date t)) t)
     (time=? (date->time-utc (time-utc->date t -86400)) t)
     (time=? (date->time-utc (time-utc->date t 0)) t)
     (time=? (date->time-utc (time-utc->date t 86400)) t)))
)

(mat time&date-printing
  (equal? 
    (with-output-to-string (lambda () (pretty-print (make-time 'time-duration 1 -1))))
    "#<time-duration -0.999999999>\n")
  (equal?
    (with-output-to-string (lambda () (write (time-utc->date (make-time 'time-utc 708626501 1427137297) -14400))))
    "#<date Mon Mar 23 15:01:37 2015>")
)
