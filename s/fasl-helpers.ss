;;; fasl-helpers.ss
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

(module (put8 put16 put32 put64 put-iptr put-uptr)
  (define (bit-mask k) (- (ash 1 k) 1))
  (define put8
    (lambda (p n)
      (put-u8 p (fxlogand n (bit-mask 8)))))
  (define put16-le
    (cond
      [(>= (most-positive-fixnum) (bit-mask 16))
       (lambda (p n)
         (put-u8 p (fxlogand n (bit-mask 8)))
         (put-u8 p (fxlogand (fxsrl n 8) (bit-mask 8))))]
      [else ($oops 'put16-le "unsupported fixnum size")]))
  (define put16-be
    (cond
      [(>= (most-positive-fixnum) (bit-mask 16))
       (lambda (p n)
         (put-u8 p (fxlogand (fxsrl n 8) (bit-mask 8)))
         (put-u8 p (fxlogand n (bit-mask 8))))]
      [else ($oops 'put16-be "unsupported fixnum size")]))
  (define put16
    (lambda (p n)
      (constant-case fasl-endianness
        [(little) (put16-le p n)]
        [(big) (put16-be p n)])))
  (define put32-le
    (cond
      [(>= (most-positive-fixnum) (bit-mask 32))
       (lambda (p n)
         (put-u8 p (fxlogand n (bit-mask 8)))
         (put-u8 p (fxlogand (fxsrl n 8) (bit-mask 8)))
         (put-u8 p (fxlogand (fxsrl n 16) (bit-mask 8)))
         (put-u8 p (fxlogand (fxsrl n 24) (bit-mask 8))))]
      [(>= (most-positive-fixnum) (bit-mask 24))
       (lambda (p n)
         (cond
           [(fixnum? n)
            (put-u8 p (fxlogand n (bit-mask 8)))
            (put-u8 p (fxlogand (fxsrl n 8) (bit-mask 8)))
            (put-u8 p (fxlogand (fxsrl n 16) (bit-mask 8)))
            (put-u8 p (fxlogand (fxsra n 24) (bit-mask 8)))]
           [else
            (let ([n (logand n (bit-mask 16))])
              (put-u8 p (fxlogand n (bit-mask 8)))
              (put-u8 p (fxsrl n 8)))
            (let ([n (ash n -16)])
              (put-u8 p (fxlogand n (bit-mask 8)))
              (put-u8 p (fxlogand (fxsra n 8) (bit-mask 8))))]))]
      [else ($oops 'put32-le "unsupported fixnum size")]))
  (define put32-be
    (cond
      [(>= (most-positive-fixnum) (bit-mask 32))
       (lambda (p n)
         (put-u8 p (fxlogand (fxsrl n 24) (bit-mask 8)))
         (put-u8 p (fxlogand (fxsrl n 16) (bit-mask 8)))
         (put-u8 p (fxlogand (fxsrl n 8) (bit-mask 8)))
         (put-u8 p (fxlogand n (bit-mask 8))))]
      [(>= (most-positive-fixnum) (bit-mask 24))
       (lambda (p n)
         (cond
           [(fixnum? n)
            (put-u8 p (fxlogand (fxsra n 24) (bit-mask 8)))
            (put-u8 p (fxlogand (fxsrl n 16) (bit-mask 8)))
            (put-u8 p (fxlogand (fxsrl n 8) (bit-mask 8)))
            (put-u8 p (fxlogand n (bit-mask 8)))]
           [else
            (let ([n (ash n -16)])
              (put-u8 p (fxlogand (fxsra n 8) (bit-mask 8)))
              (put-u8 p (fxlogand n (bit-mask 8))))
            (let ([n (logand n (bit-mask 16))])
              (put-u8 p (fxsrl n 8))
              (put-u8 p (fxlogand n (bit-mask 8))))]))]
      [else ($oops 'put32-be "unsupported fixnum size")]))
  (define put32
    (lambda (p n)
      (constant-case fasl-endianness
        [(little) (put32-le p n)]
        [(big) (put32-be p n)])))
  (define put64-le
    (lambda (p n)
      (cond
        [(and (>= (most-positive-fixnum) (bit-mask 32)) (fixnum? n))
         (put32-le p (fxlogand n (bit-mask 32)))
         (put32-le p (ash n -32))]
        [else
         (put32-le p (logand n (bit-mask 32)))
         (put32-le p (ash n -32))])))
  (define put64-be
    (lambda (p n)
      (cond
        [(and (>= (most-positive-fixnum) (bit-mask 32)) (fixnum? n))
         (put32-be p (ash n -32))
         (put32-be p (fxlogand n (bit-mask 32)))]
        [else
         (put32-be p (ash n -32))
         (put32-be p (logand n (bit-mask 32)))])))
  (define put64
    (lambda (p n)
      (constant-case fasl-endianness
        [(little) (put64-le p n)]
        [(big) (put64-be p n)])))
  (define put-iptr
    (lambda (p n0)
      (let f ([n (if (< n0 0) (- n0) n0)] [cbit 0])
        (if (and (fixnum? n) (fx<= n 63))
            (put8 p (fxlogor (if (< n0 0) #x80 0) (fxsll n 1) cbit))
            (begin
              (f (ash n -7) 1)
              (put-u8 p (fxlogor (fxsll (logand n #x7f) 1) cbit)))))))
  (define put-uptr
    (lambda (p n)
      (unless (>= n 0)
        ($oops 'compiler-internal "put-uptr received negative input ~s" n))
      (let f ([n n] [cbit 0])
        (if (and (fixnum? n) (fx<= n 127))
            (put-u8 p (fxlogor n cbit))
            (begin
              (f (ash n -7) 128)
              (put-u8 p (fxlogor (logand n #x7f) cbit)))))))
)

(define emit-header
  (case-lambda
    [(p version mtype) (emit-header p version mtype '())]
    [(p version mtype bootfiles)
     (define (put-str p s)
       (let ([n (string-length s)])
         (do ([i 0 (fx+ i 1)])
             ((fx= i n))
           (let* ([c (string-ref s i)] [k (char->integer c)])
             (unless (fx<= k 255)
               ($oops #f "cannot handle bootfile name character ~s whose integer code exceeds 255" c))
             (put-u8 p k)))))
     (put-bytevector p (constant fasl-header))
     (put-uptr p version)
     (put-uptr p mtype)
     (put-u8 p (char->integer #\())
     (let f ([bootfiles bootfiles] [sep? #f])
       (unless (null? bootfiles)
         (cond
           [(string? (car bootfiles))
            (when sep? (put-u8 p (char->integer #\space)))
            (put-str p (car bootfiles))
            (f (cdr bootfiles) #t)]
           [else
            ;; strip produces dependenices as a sequence of bytes
            (put-u8 p (car bootfiles))
            (f (cdr bootfiles) #f)])))
     (put-u8 p (char->integer #\)))]))
