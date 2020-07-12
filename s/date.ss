;;; date.ss
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

;;; disagreements with SRFI 19:
;;;  - nanoseconds are limited to 999,999,999 (SRFI 19: 9,999,999)
;;;  - seconds are limited to 61 (SRFI 19: 60)
;;;  - days range from 1 to 31, includsive (SRFI 19: 0 to 31, inclusive)
;;;  - years range from 1901 to about 2038, inclusive (SRFI 19: not clear)
;;;    - years start at 1970 under Windows
;;;  - current-date tz-offset defaults to local offset (SRFI 19: not specified)

;;; questions about SRFI 19:
;;;  - must tai times be supported?

;;; can't read past copyright notice in srfi 19 reference implementation.
;;; is it really as restrictive as it appears?

;;; suck in srfi 19 tests, which seem only to be covered by license in
;;; srfi 19 description.

;;; won't be implemented from SRFI 19 except as add-on:
;;;  - constants time-duration, time-monotonic, time-process, time-tai,
;;;    time-thread, and time-utc (violates no non-procedure value policy)

;;; not yet implemented from SRFI 19:
;;;  - time procedures
;;;    time-resolution [ts-time]
;;;    time-monotonic->time-utc    ; may be impossible unless we roll our own
;;;    time-monotonic->time-utc!   ; monotonic (= tai) based on utc plus leap
;;;    time-utc->time-monotonic    ; seconds.  yuck.
;;;    time-utc->time-monotonic!
;;;  - date procedures
;;;    date-week-number
;;;    date->time-monotonic
;;;    time-monotonic->date
;;;    date->string
;;;    string->date
;;;  - julian dates
;;;    current-julian-day
;;;    current-modified-julian-day
;;;    date->julian-day
;;;    date->modified-julian-day
;;;    julian-day->date
;;;    julian-day->time-monotonic
;;;    julian-day->time-utc
;;;    modified-julian-day->date
;;;    modified-julian-day->time-monotonic
;;;    modified-julian-day->time-utc
;;;    time-monotonic->julian-day
;;;    time-monotonic->modified-julian-day
;;;    time-utc->julian-day
;;;    time-utc->modified-julian-day
;;;  - tai times
;;;    ts-type 'time-tai
;;;    date->time-tai
;;;    time-monotonic->time-tai
;;;    time-monotonic->time-tai!
;;;    time-tai->date
;;;    time-tai->time-monotonic
;;;    time-tai->time-monotonic!
;;;    time-tai->time-utc
;;;    time-tai->time-utc!
;;;    time-utc->time-tai
;;;    time-utc->time-tai!
;;;    julian-day->time-tai
;;;    modified-julian-day->time-tai
;;;    time-tai->julian-day
;;;    time-tai->modified-julian-day

(let ()
  (define $clock-gettime ; clock_id -> tspair
    (foreign-procedure "(cs)clock_gettime"
      (integer-32)
      scheme-object))

  (define $gmtime ; #f|tzoff X #f|tspair -> dtvec  (returns #f on error)
    (foreign-procedure "(cs)gmtime"
      (scheme-object scheme-object)
      scheme-object))

  (define $asctime ; #f | dtvec -> string          (returns #f on error)
    (foreign-procedure "(cs)asctime"
      (scheme-object)
      scheme-object))

  (define $mktime ; dtvec -> tspair                (returns #f on error)
    (foreign-procedure "(cs)mktime"
      (scheme-object)
      scheme-object))

  (define-record-type ts ; keep in sync with S_condition_wait in c/thread.c
    (fields (mutable typeno) (immutable pair))
    (nongenerative #{ts a5dq4nztnmq6xlgp-a})
    (sealed #t))

  (define ts-type->typeno
    (lambda (who type)
      (case type
        [(time-process) (constant time-process)]
        [(time-thread) (constant time-thread)]
        [(time-duration) (constant time-duration)]
        [(time-monotonic) (constant time-monotonic)]
        [(time-utc) (constant time-utc)]
        [(time-collector-cpu) (constant time-collector-cpu)]
        [(time-collector-real) (constant time-collector-real)]
        [else ($oops who "unrecognized time type ~s" type)])))

  (define ts-typeno->type
    (lambda (typeno)
      (cond
        [(eq? typeno (constant time-process)) 'time-process]
        [(eq? typeno (constant time-thread)) 'time-thread]
        [(eq? typeno (constant time-duration)) 'time-duration]
        [(eq? typeno (constant time-monotonic)) 'time-monotonic]
        [(eq? typeno (constant time-utc)) 'time-utc]
        [(eq? typeno (constant time-collector-cpu)) 'time-collector-cpu]
        [(eq? typeno (constant time-collector-real)) 'time-collector-real]
        [else ($oops 'time-internal "unexpected typeno ~s" typeno)])))

  (define ts-sec (lambda (ts) (car (ts-pair ts))))
  (define ts-nsec (lambda (ts) (cdr (ts-pair ts))))
  (define set-ts-sec! (lambda (ts n) (set-car! (ts-pair ts) n)))
  (define set-ts-nsec! (lambda (ts n) (set-cdr! (ts-pair ts) n)))

  (define (check-ts who ts)
    (unless (ts? ts)
      ($oops who "~s is not a time record" ts)))

  (define (check-ts-sec who sec)
    (unless (or (fixnum? sec) (bignum? sec))
      ($oops who "invalid number of seconds ~s" sec)))

  (define (check-same-type who t1 t2)
    (unless (fx= (ts-typeno t1) (ts-typeno t2))
      ($oops who "types of ~s and ~s differ" t1 t2)))

  (define (check-type-duration who t)
    (unless (fx= (ts-typeno t) (constant time-duration))
      ($oops who "~s does not have type time-duration" t)))

  (define-record-type dt
    (fields (immutable vec))
    (nongenerative #{dt a5jhglnb7tr8ubed-a})
    (sealed #t))

  (define (check-dt who dt)
    (unless (dt? dt)
      ($oops who "~s is not a date record" dt)))

  (define (check-nsec who nsec)
    (unless (and (or (fixnum? nsec) (bignum? nsec)) (<= 0 nsec 999999999))
      ($oops who "invalid nanosecond ~s" nsec)))

  (define (check-sec who sec)
    (unless (and (fixnum? sec) (fx<= 0 sec 61))
      ($oops who "invalid second ~s" sec)))

  (define (check-min who min)
    (unless (and (fixnum? min) (fx<= 0 min 59))
      ($oops who "invalid minute ~s" min)))

  (define (check-hour who hour)
    (unless (and (fixnum? hour) (fx<= 0 hour 23))
      ($oops who "invalid hour ~s" hour)))

  (define (check-day who day)
    (unless (and (fixnum? day) (fx<= 1 day 31))
      ($oops who "invalid day ~s" day)))

  (define (check-mon who mon)
    (unless (and (fixnum? mon) (fx<= 1 mon 12))
      ($oops who "invalid month ~s" mon)))

  (define (check-year who year)
    (unless (and (fixnum? year) (fx>= year 1901))
      ($oops who "invalid year ~s" year)))

  (define (check-tz who tz)
    (unless (and (fixnum? tz)
                ; being generous here...
                 (fx<= (* -24 60 60) tz (* 24 60 60)))
      ($oops who "invalid time-zone offset ~s" tz)))

  (define $copy-time
    (lambda (t)
      (let ([p (ts-pair t)])
        (make-ts (ts-typeno t) (cons (car p) (cdr p))))))

  (record-writer (type-descriptor ts)
    (lambda (x p wr)
      (let ([type (ts-typeno->type (ts-typeno x))] [sec (ts-sec x)] [nsec (ts-nsec x)])
        (if (and (< sec 0) (> nsec 0))
            (fprintf p "#<~s -~d.~9,'0d>" type (- -1 sec) (- 1000000000 nsec))
            (fprintf p "#<~s ~d.~9,'0d>" type sec nsec)))))

  (record-writer (type-descriptor dt)
    (lambda (x p wr)
      (fprintf p "#<date~@[ ~a~]>"
        ($asctime (dt-vec x)))))

  (set-who! make-time
    (lambda (type nsec sec)
      (let ([typeno (ts-type->typeno who type)])
        (check-nsec who nsec)
        (check-ts-sec who sec)
        (make-ts typeno (cons sec nsec)))))

  (set! time? (lambda (x) (ts? x)))

  (set-who! time-type
    (lambda (ts)
      (check-ts who ts)
      (ts-typeno->type (ts-typeno ts))))

  (set-who! time-second
    (lambda (ts)
      (check-ts who ts)
      (ts-sec ts)))

  (set-who! time-nanosecond
    (lambda (ts)
      (check-ts who ts)
      (ts-nsec ts)))

  (set-who! set-time-type!
    (lambda (ts type)
      (check-ts who ts)
      (ts-typeno-set! ts (ts-type->typeno who type))))

  (set-who! set-time-second!
    (lambda (ts sec)
      (check-ts who ts)
      (check-ts-sec who sec)
      (set-ts-sec! ts sec)))

  (set-who! set-time-nanosecond!
    (lambda (ts nsec)
      (check-ts who ts)
      (check-nsec who nsec)
      (set-ts-nsec! ts nsec)))

  (set-who! time=?
    (lambda (t1 t2)
      (check-ts who t1)
      (check-ts who t2)
      (check-same-type who t1 t2)
      (and (= (ts-sec t1) (ts-sec t2))
           (= (ts-nsec t1) (ts-nsec t2)))))

  (set-who! time<?
    (lambda (t1 t2)
      (check-ts who t1)
      (check-ts who t2)
      (check-same-type who t1 t2)
      (or (< (ts-sec t1) (ts-sec t2))
          (and (= (ts-sec t1) (ts-sec t2))
               (< (ts-nsec t1) (ts-nsec t2))))))

  (set-who! time<=?
    (lambda (t1 t2)
      (check-ts who t1)
      (check-ts who t2)
      (check-same-type who t1 t2)
      (or (< (ts-sec t1) (ts-sec t2))
          (and (= (ts-sec t1) (ts-sec t2))
               (<= (ts-nsec t1) (ts-nsec t2))))))

  (set-who! time>=?
    (lambda (t1 t2)
      (check-ts who t1)
      (check-ts who t2)
      (check-same-type who t1 t2)
      (or (> (ts-sec t1) (ts-sec t2))
          (and (= (ts-sec t1) (ts-sec t2))
               (>= (ts-nsec t1) (ts-nsec t2))))))

  (set-who! time>?
    (lambda (t1 t2)
      (check-ts who t1)
      (check-ts who t2)
      (check-same-type who t1 t2)
      (or (> (ts-sec t1) (ts-sec t2))
          (and (= (ts-sec t1) (ts-sec t2))
               (> (ts-nsec t1) (ts-nsec t2))))))

  (let ([f (lambda (t1 t2 who)
             (check-ts who t1)
             (check-ts who t2)
             (check-same-type who t1 t2)
             (let-values ([(sec nsec)
                           (let ([sec (- (ts-sec t1) (ts-sec t2))]
                                 [nsec (- (ts-nsec t1) (ts-nsec t2))])
                             (if (< nsec 0) (values (- sec 1) (+ nsec 1000000000)) (values sec nsec)))])
               (make-ts (constant time-duration) (cons sec nsec))))])
    (set-who! time-difference (lambda (t1 t2) (f t1 t2 who)))
    (set-who! time-difference! (lambda (t1 t2) (f t1 t2 who))))

  (let ([f (lambda (t1 t2 who)
             (check-ts who t1)
             (check-ts who t2)
             (check-type-duration who t2)
             (let-values ([(sec nsec)
                           (let ([sec (- (ts-sec t1) (ts-sec t2))]
                                 [nsec (- (ts-nsec t1) (ts-nsec t2))])
                             (if (< nsec 0) (values (- sec 1) (+ nsec 1000000000)) (values sec nsec)))])
               (make-ts (ts-typeno t1) (cons sec nsec))))])
    (set-who! subtract-duration (lambda (t1 t2) (f t1 t2 who)))
    (set-who! subtract-duration! (lambda (t1 t2) (f t1 t2 who))))

  (let ([f (lambda (t1 t2 who)
             (check-ts who t1)
             (check-ts who t2)
             (check-type-duration who t2)
             (let-values ([(sec nsec)
                           (let ([sec (+ (time-second t1) (time-second t2))]
                                 [nsec (+ (time-nanosecond t1) (time-nanosecond t2))])
                             (if (>= nsec 1000000000) (values (+ sec 1) (- nsec 1000000000)) (values sec nsec)))])
               (make-ts (ts-typeno t1) (cons sec nsec))))])
    (set-who! add-duration (lambda (t1 t2) (f t1 t2 who)))
    (set-who! add-duration! (lambda (t1 t2) (f t1 t2 who))))

  (set-who! copy-time
    (lambda (t)
      (check-ts who t)
      ($copy-time t)))

  (set-who! current-time
    (case-lambda
      [() (let ([typeno (constant time-utc)])
            (make-ts typeno ($clock-gettime typeno)))]
      [(type)
       (case type
         [(time-collector-cpu) ($copy-time ($gc-cpu-time))]
         [(time-collector-real) ($copy-time ($gc-real-time))]
         [else (let ([typeno (ts-type->typeno who type)])
                 (make-ts typeno ($clock-gettime typeno)))])]))

  (set-who! current-date
    (case-lambda
      [()
       (let ([dtvec ($gmtime #f #f)])
         (unless dtvec ($oops who "failed"))
         (make-dt dtvec))]
      [(tz)
       (check-tz who tz)
       (let ([dtvec ($gmtime tz #f)])
         (unless dtvec ($oops who "failed"))
         (make-dt dtvec))]))

  (set-who! date-and-time ; ptime|#f -> string
    (case-lambda
      [() (or ($asctime #f) ($oops who "failed"))]
      [(dt)
       (check-dt who dt)
       (or ($asctime (dt-vec dt))
           ($oops who "failed for date record ~s" dt))]))

  (set-who! make-date
    (let ([do-make-date
           (lambda (nsec sec min hour day mon year tz tz-provided?)
             (check-nsec who nsec)
             (check-sec who sec)
             (check-min who min)
             (check-hour who hour)
            ; need more accurate check for day based on year and month
             (check-day who day)
             (check-mon who mon)
             (check-year who year)
             (when tz-provided?
               (check-tz who tz))
            ; keep in sync with cmacros.ss declarations of dtvec-nsec, etc.
             (let ([dtvec (vector nsec sec min hour day mon (- year 1900) 0 #f 0 tz #f)])
               (unless ($mktime dtvec) ; for effect on dtvec
                 ($oops who "invalid combination of arguments"))
               (unless (fx= (vector-ref dtvec (constant dtvec-mday)) day)
                 ($oops who "invalid day ~s for month ~s and year ~s" day mon year))
               (make-dt dtvec)))])
      (case-lambda
        [(nsec sec min hour day mon year tz)
         (do-make-date nsec sec min hour day mon year tz #t)]
        [(nsec sec min hour day mon year)
         (do-make-date nsec sec min hour day mon year #f #f)])))

  (set! date? (lambda (x) (dt? x)))

  (let ()
    (define-syntax date-getter
      (syntax-rules ()
        [(_ name index)
         (set! name
           (lambda (dt)
             (check-dt 'name dt)
             (vector-ref (dt-vec dt) index)))]))

    (date-getter date-nanosecond (constant dtvec-nsec))
    (date-getter date-second (constant dtvec-sec))
    (date-getter date-minute (constant dtvec-min))
    (date-getter date-hour (constant dtvec-hour))
    (date-getter date-day (constant dtvec-mday))
    (date-getter date-month (constant dtvec-mon))
   ; date-year is below
    (date-getter date-week-day (constant dtvec-wday))
    (date-getter date-year-day (constant dtvec-yday))
    (date-getter date-dst? (constant dtvec-isdst))
    (date-getter date-zone-offset (constant dtvec-tzoff))
    (date-getter date-zone-name (constant dtvec-tzname)))

  (set-who! date-year
    (lambda (dt)
      (check-dt who dt)
      (+ (vector-ref (dt-vec dt) (constant dtvec-year)) 1900)))

  #;(set-who! date-week-number
    (lambda (dt dowsw)
      (unless (or (eq? dossw 0) (eq? dossw 1)) 
        ($oops who "invalid week starting day" dossw))
      ???))

  (set-who! time-utc->date
    (case-lambda
      [(t)
       (unless (and (ts? t) (eq? (ts-typeno t) (constant time-utc)))
         ($oops who "~s is not a utc time record" t))
       (let ([dtvec ($gmtime #f (ts-pair t))])
         (unless dtvec ($oops who "failed"))
         (make-dt dtvec))]
      [(t tz)
       (unless (and (ts? t) (eq? (ts-typeno t) (constant time-utc)))
         ($oops who "~s is not a utc time record" t))
       (check-tz who tz)
       (let ([dtvec ($gmtime tz (ts-pair t))])
         (unless dtvec ($oops who "failed"))
         (make-dt dtvec))]))

  (set-who! date->time-utc
    (lambda (dt)
      (check-dt who dt)
      (let ([p ($mktime (vector-copy (dt-vec dt)))])
        (unless p ($oops who "conversion failed for ~s" dt))
        (make-ts (constant time-utc) p))))
)
