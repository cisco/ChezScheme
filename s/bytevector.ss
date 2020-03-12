;;; bytevector.ss
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
  (define-syntax signed-value-pred
    (lambda (x)
      (syntax-case x ()
        [(_ ?bits)
         (let ([bits (syntax->datum #'?bits)])
           (unless (and (fixnum? bits)
                        (fx> bits 0)
                        (fx= (* (fxquotient bits 8) 8) bits))
             (syntax-error #'?bits "invalid bits"))
           (cond
             [(fx<= bits (constant fixnum-bits))
              (with-syntax ([limit- (- (expt 2 (- bits 1)))]
                            [limit+ (- (expt 2 (- bits 1)) 1)])
                #'(lambda (k) (and (fixnum? k) (fx<= limit- k limit+))))]
             [(fx= bits (constant fixnum-bits)) #'fixnum?]
             [else
              (with-syntax ([limit- (- (expt 2 (- bits 1)))]
                            [limit+ (- (expt 2 (- bits 1)) 1)])
                #'(lambda (k)
                    (or (fixnum? k)
                        (and (bignum? k) (<= limit- k limit+)))))]))])))

  (define-syntax unsigned-value-pred
    (lambda (x)
      (syntax-case x ()
        [(_ ?bits)
         (let ([bits (syntax->datum #'?bits)])
           (unless (and (fixnum? bits)
                        (fx> bits 0)
                        (fx= (* (fxquotient bits 8) 8) bits))
             (syntax-error #'?bits "invalid bits"))
           (cond
             [(fx< bits (constant fixnum-bits))
              (with-syntax ([limit+ (expt 2 bits)])
                #'(lambda (k) (and (fixnum? k) ($fxu< k limit+))))]
             [(fx= bits (constant fixnum-bits))
              #'(lambda (k) (and (fixnum? k) (fx>= k 0)))]
             [else
              (with-syntax ([limit+ (- (expt 2 bits) 1)])
                #'(lambda (k)
                    (if (fixnum? k)
                        (fx>= k 0)
                        (and (bignum? k) (<= 0 k limit+)))))]))])))

  (define (not-a-bytevector who v)
    ($oops who "~s is not a bytevector" v))

  (define (not-a-mutable-bytevector who v)
    ($oops who "~s is not a mutable bytevector" v))

  (define (invalid-index who v i)
    ($oops who "invalid index ~s for bytevector ~s" i v))

  (define (invalid-fill-value who fill)
    ($oops who "~s is not a valid fill value" fill))

  (define (invalid-value who x)
    ($oops who "invalid value ~s" x))

  (define (size-multiple-error who n size)
    ($oops who "bytevector length ~s is not a multiple of size ~s"
      n size))

  (define (unrecognized-endianness who eness)
    ($oops who "unrecognized endianness ~s" eness))

  (define (invalid-size who size)
    ($oops who "invalid size ~s" size))

  (define (invalid-size-or-index who size v i)
    (if (and (fixnum? i) ($fxu< i (bytevector-length v)))
        (if ($fxu< size (bytevector-length v))
            ($oops who "invalid index ~s for ~s-byte field of bytevector ~s" i size v)
            ($oops who "invalid size ~s for bytevector ~s" size v))
        (invalid-index who v i)))

  (define (fill? x) (and (fixnum? x) (fx<= -128 x 255)))

  (define-syntax unaligned-ref-check
    (syntax-rules ()
      [(_ who ?size v i)
       (let ([size ?size])
         (unless (and (fixnum? i)
                      (fx>= i 0)
                      (fx< i (fx- (bytevector-length v) (fx- size 1))))
           (invalid-size-or-index who size v i)))]))

  (module ($bytevector-sint-little-ref $bytevector-uint-little-ref)
    (define (load-little v i size a)
      (cond
        [(fx>= size 3)
         (load-little v (fx- i 3) (fx- size 3)
           (logor (ash a 24)
             (fxlogor
               (fxsll (bytevector-u8-ref v i) 16)
               (fxsll (bytevector-u8-ref v (fx- i 1)) 8)
               (bytevector-u8-ref v (fx- i 2)))))]
        [(fx= size 0) a]
        [(fx= size 1) (logor (ash a 8) (bytevector-u8-ref v i))]
        [else (logor (ash a 16)
                (fxlogor
                  (fxsll (bytevector-u8-ref v i) 8)
                  (bytevector-u8-ref v (fx- i 1))))]))

    (define ($bytevector-sint-little-ref v i size)
      (let ([i (fx+ i size -1)])
        (load-little v (fx- i 1) (fx- size 1) (bytevector-s8-ref v i))))

    (define ($bytevector-uint-little-ref v i size)
      (let ([i (fx+ i size -1)])
        (load-little v (fx- i 1) (fx- size 1) (bytevector-u8-ref v i)))))

  (module ($bytevector-sint-big-ref $bytevector-uint-big-ref)
    (define (load-big v i size a)
      (cond
        [(fx>= size 3)
         (load-big v (fx+ i 3) (fx- size 3)
           (logor (ash a 24)
             (fxlogor
               (fxsll (bytevector-u8-ref v i) 16)
               (fxsll (bytevector-u8-ref v (fx+ i 1)) 8)
               (bytevector-u8-ref v (fx+ i 2)))))]
        [(fx= size 0) a]
        [(fx= size 1) (logor (ash a 8) (bytevector-u8-ref v i))]
        [else (logor (ash a 16)
                (fxlogor
                  (fxsll (bytevector-u8-ref v i) 8)
                  (bytevector-u8-ref v (fx+ i 1))))]))

    (define ($bytevector-sint-big-ref v i size)
      (load-big v (fx+ i 1) (fx- size 1) (bytevector-s8-ref v i)))

    (define ($bytevector-uint-big-ref v i size)
      (load-big v (fx+ i 1) (fx- size 1) (bytevector-u8-ref v i))))

  (define ($bytevector-int-little-set! v i k size)
    (let store-little! ([i i] [k k] [size size])
      (cond
        [(fx>= size 4)
         (let ([k (logand k #xffffff)])
           (bytevector-u8-set! v i (fxlogand k #xff))
           (bytevector-u8-set! v (fx+ i 1) (fxlogand (fxsra k 8) #xff))
           (bytevector-u8-set! v (fx+ i 2) (fxsra k 16)))
         (store-little! (fx+ i 3) (ash k -24) (fx- size 3))]
        [(fx= size 1) ($bytevector-set! v i k)]
        [(fx= size 2)
         (bytevector-u8-set! v i (fxlogand k #xff))
         ($bytevector-set! v (fx+ i 1) (fxsra k 8))]
        [else
         (bytevector-u8-set! v i (fxlogand k #xff))
         (bytevector-u8-set! v (fx+ i 1) (fxlogand (fxsra k 8) #xff))
         ($bytevector-set! v (fx+ i 2) (fxsra k 16))])))

  (define ($bytevector-int-big-set! v i k size)
    (let store-big! ([i (fx+ i size -1)] [k k] [size size])
      (cond
        [(fx>= size 4)
         (let ([k (logand k #xffffff)])
           (bytevector-u8-set! v (fx- i 2) (fxsra k 16))
           (bytevector-u8-set! v (fx- i 1) (fxlogand (fxsra k 8) #xff))
           (bytevector-u8-set! v i (fxlogand k #xff)))
         (store-big! (fx- i 3) (ash k -24) (fx- size 3))]
        [(fx= size 1) ($bytevector-set! v i k)]
        [(fx= size 2)
         ($bytevector-set! v (fx- i 1) (fxsra k 8))
         (bytevector-u8-set! v i (fxlogand k #xff))]
        [else
         ($bytevector-set! v (fx- i 2) (fxsra k 16))
         (bytevector-u8-set! v (fx- i 1) (fxlogand (fxsra k 8) #xff))
         (bytevector-u8-set! v i (fxlogand k #xff))])))

  (module ($bytevector-s16-ref $bytevector-u16-ref
           $bytevector-s24-ref $bytevector-u24-ref
           $bytevector-s32-ref $bytevector-u32-ref
           $bytevector-s40-ref $bytevector-u40-ref
           $bytevector-s48-ref $bytevector-u48-ref
           $bytevector-s56-ref $bytevector-u56-ref
           $bytevector-s64-ref $bytevector-u64-ref)
    (meta-cond
      [(fx> (constant fixnum-bits) 56)
       (define logor56 fxlogor)
       (define sll56 fxsll)]
      [else
       (define logor56 logor)
       (define sll56 ash)])

    (define (little-ref-s16 v i)
      (fxlogor (fxsll (#3%bytevector-s8-ref v (fx+ i 1)) 8)
               (#3%bytevector-u8-ref v i)))
    (define (big-ref-s16 v i)
      (fxlogor (fxsll (#3%bytevector-s8-ref v i) 8)
               (#3%bytevector-u8-ref v (fx+ i 1))))
    (define (little-ref-u16 v i)
      (fxlogor (fxsll (#3%bytevector-u8-ref v (fx+ i 1)) 8)
               (#3%bytevector-u8-ref v i)))
    (define (big-ref-u16 v i)
      (fxlogor (fxsll (#3%bytevector-u8-ref v i) 8)
               (#3%bytevector-u8-ref v (fx+ i 1))))
    (define (little-ref-s24 v i)
      (fxlogor (fxsll (#3%bytevector-s8-ref v (fx+ i 2)) 16)
               (little-ref-u16 v i)))
    (define (big-ref-s24 v i)
      (fxlogor (fxsll (#3%bytevector-s8-ref v i) 16)
               (big-ref-u16 v (fx+ i 1))))
    (define (little-ref-u24 v i)
      (fxlogor (fxsll (#3%bytevector-u8-ref v (fx+ i 2)) 16)
               (little-ref-u16 v i)))
    (define (big-ref-u24 v i)
      (fxlogor (fxsll (#3%bytevector-u8-ref v i) 16)
               (big-ref-u16 v (fx+ i 1))))
    (define (little-ref-s32 v i)
      (logor56 (sll56 (little-ref-s16 v (fx+ i 2)) 16)
               (little-ref-u16 v i)))
    (define (big-ref-s32 v i)
      (logor56 (sll56 (big-ref-s16 v i) 16)
               (big-ref-u16 v (fx+ i 2))))
    (define (little-ref-u32 v i)
      (logor56 (sll56 (little-ref-u16 v (fx+ i 2)) 16)
               (little-ref-u16 v i)))
    (define (big-ref-u32 v i)
      (logor56 (sll56 (big-ref-u16 v i) 16)
               (big-ref-u16 v (fx+ i 2))))
    (define (little-ref-s40 v i)
      (logor56 (sll56(#3%bytevector-s8-ref v (fx+ i 4)) 32)
               (little-ref-u32 v i)))
    (define (big-ref-s40 v i)
      (logor56 (sll56(#3%bytevector-s8-ref v i) 32)
               (big-ref-u32 v (fx+ i 1))))
    (define (little-ref-u40 v i)
      (logor56 (sll56(#3%bytevector-u8-ref v (fx+ i 4)) 32)
               (little-ref-u32 v i)))
    (define (big-ref-u40 v i)
      (logor56 (sll56(#3%bytevector-u8-ref v i) 32)
               (big-ref-u32 v (fx+ i 1))))
    (define (little-ref-s48 v i)
      (logor56 (sll56(little-ref-s16 v (fx+ i 4)) 32)
               (little-ref-u32 v i)))
    (define (big-ref-s48 v i)
      (logor56 (sll56(big-ref-s16 v i) 32)
               (big-ref-u32 v (fx+ i 2))))
    (define (little-ref-u48 v i)
      (logor56 (sll56(little-ref-u16 v (fx+ i 4)) 32)
               (little-ref-u32 v i)))
    (define (big-ref-u48 v i)
      (logor56 (sll56(big-ref-u16 v i) 32)
               (big-ref-u32 v (fx+ i 2))))
    (define (little-ref-s56 v i)
      (logor56 (sll56(little-ref-s24 v (fx+ i 4)) 32)
               (little-ref-u32 v i)))
    (define (big-ref-s56 v i)
      (logor56 (sll56(big-ref-s24 v i) 32)
               (big-ref-u32 v (fx+ i 3))))
    (define (little-ref-u56 v i)
      (logor56 (sll56(little-ref-u24 v (fx+ i 4)) 32)
               (little-ref-u32 v i)))
    (define (big-ref-u56 v i)
      (logor56 (sll56(big-ref-u24 v i) 32)
               (big-ref-u32 v (fx+ i 3))))
    (define (little-ref-s64 v i)
      (logor (ash (little-ref-s32 v (fx+ i 4)) 32)
             (little-ref-u32 v i)))
    (define (big-ref-s64 v i)
      (logor (ash (big-ref-s32 v i) 32)
             (big-ref-u32 v (fx+ i 4))))
    (define (little-ref-u64 v i)
      (logor (ash (little-ref-u32 v (fx+ i 4)) 32)
             (little-ref-u32 v i)))
    (define (big-ref-u64 v i)
      (logor (ash (big-ref-u32 v i) 32)
             (big-ref-u32 v (fx+ i 4))))

    (define-syntax bytevector-*-ref
      (lambda (x)
        (define p2?
          (lambda (n)
            (let f ([i 1])
              (or (fx= i n)
                  (and (not (fx> i n)) (f (fxsll i 1)))))))
        (syntax-case x ()
          [(kwd s/u bits)
           (with-syntax ([prim-name (construct-name #'kwd "bytevector-" #'s/u #'bits "-ref")]
                         [native-name (construct-name #'kwd "bytevector-" #'s/u #'bits "-native-ref")]
                         [little-set! (construct-name #'kwd "little-ref-" #'s/u #'bits)]
                         [big-set! (construct-name #'kwd "big-ref-" #'s/u #'bits)])
             #`(lambda (v i eness who)
                 (unless (bytevector? v) (not-a-bytevector who v))
                 (unaligned-ref-check who (fxquotient bits 8) v i)
                 (case eness
                   [(big)
                    #,(cond
                        [(constant unaligned-integers) #`(#3%prim-name v i 'big)]
                        [(and (eq? (constant native-endianness) 'big) (p2? (datum bits)))
                         #`(if (fx= (fxlogand i (fx- (fxquotient bits 8) 1)) 0)
                               (#3%native-name v i)
                               (big-ref v i))]
                        [else #`(big-ref v i)])]
                   [(little)
                    #,(cond
                        [(constant unaligned-integers) #`(#3%prim-name v i 'little)]
                        [(and (eq? (constant native-endianness) 'little) (p2? (datum bits)))
                         #`(if (fx= (fxlogand i (fx- (fxquotient bits 8) 1)) 0)
                               (#3%native-name v i)
                               (little-ref v i))]
                        [else #`(little-ref v i)])]
                   [else (unrecognized-endianness who eness)])))])))

    (define $bytevector-s16-ref (bytevector-*-ref s 16))
    (define $bytevector-u16-ref (bytevector-*-ref u 16))
    (define $bytevector-s24-ref (bytevector-*-ref s 24))
    (define $bytevector-u24-ref (bytevector-*-ref u 24))
    (define $bytevector-s32-ref (bytevector-*-ref s 32))
    (define $bytevector-u32-ref (bytevector-*-ref u 32))
    (define $bytevector-s40-ref (bytevector-*-ref s 40))
    (define $bytevector-u40-ref (bytevector-*-ref u 40))
    (define $bytevector-s48-ref (bytevector-*-ref s 48))
    (define $bytevector-u48-ref (bytevector-*-ref u 48))
    (define $bytevector-s56-ref (bytevector-*-ref s 56))
    (define $bytevector-u56-ref (bytevector-*-ref u 56))
    (define $bytevector-s64-ref (bytevector-*-ref s 64))
    (define $bytevector-u64-ref (bytevector-*-ref u 64))
  )

  (module ($bytevector-s16-set! $bytevector-u16-set!
           $bytevector-s24-set! $bytevector-u24-set!
           $bytevector-s32-set! $bytevector-u32-set!
           $bytevector-s40-set! $bytevector-u40-set!
           $bytevector-s48-set! $bytevector-u48-set!
           $bytevector-s56-set! $bytevector-u56-set!
           $bytevector-s64-set! $bytevector-u64-set!)
    (meta-cond
      [(fx> (constant fixnum-bits) 56)
       (define logand56 fxlogand)
       (define sra56 fxsra)]
      [else
       (define logand56 logand)
       (define sra56 (lambda (x y) (ash x (fx- y))))])

    (define (little-set-s16! v i k)
      (#3%bytevector-u8-set! v i (fxlogand k #xff))
      (#3%bytevector-s8-set! v (fx+ i 1) (fxsra k 8)))
    (define (big-set-s16! v i k)
      (#3%bytevector-s8-set! v i (fxsra k 8))
      (#3%bytevector-u8-set! v (fx+ i 1) (fxlogand k #xff)))
    (define (little-set-u16! v i k)
      (#3%bytevector-u8-set! v i (fxlogand k #xff))
      (#3%bytevector-u8-set! v (fx+ i 1) (fxsra k 8)))
    (define (big-set-u16! v i k)
      (#3%bytevector-u8-set! v i (fxsra k 8))
      (#3%bytevector-u8-set! v (fx+ i 1) (fxlogand k #xff)))
    (define (little-set-s24! v i k)
      (little-set-u16! v i (fxlogand k #xffff))
      (#3%bytevector-s8-set! v (fx+ i 2) (fxsra k 16)))
    (define (big-set-s24! v i k)
      (#3%bytevector-s8-set! v i (fxsra k 16))
      (big-set-u16! v (fx+ i 1) (fxlogand k #xffff)))
    (define (little-set-u24! v i k)
      (little-set-u16! v i (fxlogand k #xffff))
      (#3%bytevector-u8-set! v (fx+ i 2) (fxsra k 16)))
    (define (big-set-u24! v i k)
      (#3%bytevector-u8-set! v i (fxsra k 16))
      (big-set-u16! v (fx+ i 1) (fxlogand k #xffff)))
    (define (little-set-s32! v i k)
      (little-set-u16! v i (logand56 k #xffff))
      (little-set-s16! v (fx+ i 2) (sra56 k 16)))
    (define (big-set-s32! v i k)
      (big-set-s16! v i (sra56 k 16))
      (big-set-u16! v (fx+ i 2) (logand56 k #xffff)))
    (define (little-set-u32! v i k)
      (little-set-u16! v i (logand56 k #xffff))
      (little-set-u16! v (fx+ i 2) (sra56 k 16)))
    (define (big-set-u32! v i k)
      (big-set-u16! v i (sra56 k 16))
      (big-set-u16! v (fx+ i 2) (logand56 k #xffff)))
    (define (little-set-s40! v i k)
      (little-set-u32! v i (logand56 k #xffffffff))
      (#3%bytevector-s8-set! v (fx+ i 4) (sra56 k 32)))
    (define (big-set-s40! v i k)
      (#3%bytevector-s8-set! v i (sra56 k 32))
      (big-set-u32! v (fx+ i 1) (logand56 k #xffffffff)))
    (define (little-set-u40! v i k)
      (little-set-u32! v i (logand56 k #xffffffff))
      (#3%bytevector-u8-set! v (fx+ i 4) (sra56 k 32)))
    (define (big-set-u40! v i k)
      (#3%bytevector-u8-set! v i (sra56 k 32))
      (big-set-u32! v (fx+ i 1) (logand56 k #xffffffff)))
    (define (little-set-s48! v i k)
      (little-set-u32! v i (logand56 k #xffffffff))
      (little-set-s16! v (fx+ i 4) (sra56 k 32)))
    (define (big-set-s48! v i k)
      (big-set-s16! v i (sra56 k 32))
      (big-set-u32! v (fx+ i 2) (logand56 k #xffffffff)))
    (define (little-set-u48! v i k)
      (little-set-u32! v i (logand56 k #xffffffff))
      (little-set-u16! v (fx+ i 4) (sra56 k 32)))
    (define (big-set-u48! v i k)
      (big-set-u16! v i (sra56 k 32))
      (big-set-u32! v (fx+ i 2) (logand56 k #xffffffff)))
    (define (little-set-s56! v i k)
      (little-set-u32! v i (logand56 k #xffffffff))
      (little-set-s24! v (fx+ i 4) (sra56 k 32)))
    (define (big-set-s56! v i k)
      (big-set-s24! v i (sra56 k 32))
      (big-set-u32! v (fx+ i 3) (logand56 k #xffffffff)))
    (define (little-set-u56! v i k)
      (little-set-u32! v i (logand56 k #xffffffff))
      (little-set-u24! v (fx+ i 4) (sra56 k 32)))
    (define (big-set-u56! v i k)
      (big-set-u24! v i (sra56 k 32))
      (big-set-u32! v (fx+ i 3) (logand56 k #xffffffff)))
    (define (little-set-s64! v i k)
      (little-set-u32! v i (logand k #xffffffff))
      (little-set-s32! v (fx+ i 4) (ash k -32)))
    (define (big-set-s64! v i k)
      (big-set-s32! v i (ash k -32))
      (big-set-u32! v (fx+ i 4) (logand k #xffffffff)))
    (define (little-set-u64! v i k)
      (little-set-u32! v i (logand k #xffffffff))
      (little-set-u32! v (fx+ i 4) (ash k -32)))
    (define (big-set-u64! v i k)
      (big-set-u32! v i (ash k -32))
      (big-set-u32! v (fx+ i 4) (logand k #xffffffff)))

    (define-syntax bytevector-*-set!
      (lambda (x)
        (define p2?
          (lambda (n)
            (let f ([i 1])
              (or (fx= i n)
                  (and (not (fx> i n)) (f (fxsll i 1)))))))
        (syntax-case x ()
          [(kwd s/u bits)
           (with-syntax ([prim-name (construct-name #'kwd "bytevector-" #'s/u #'bits "-set!")]
                         [native-name (construct-name #'kwd "bytevector-" #'s/u #'bits "-native-set!")]
                         [little-set! (construct-name #'kwd "little-set-" #'s/u #'bits "!")]
                         [big-set! (construct-name #'kwd "big-set-" #'s/u #'bits "!")]
                         [value-pred (if (free-identifier=? #'s/u #'s)
                                         #'signed-value-pred
                                         #'unsigned-value-pred)])
             #`(let ([value-okay? (value-pred bits)])
                 (lambda (v i k eness who)
                   (unless (mutable-bytevector? v) (not-a-mutable-bytevector who v))
                   (unaligned-ref-check who (fxquotient bits 8) v i)
                   (unless (value-okay? k) (invalid-value who k))
                   (case eness
                     [(big)
                      #,(cond
                          [(and (constant unaligned-integers) (>= (constant ptr-bits) (datum bits)))
                           #`(#3%prim-name v i k 'big)]
                          [(and (eq? (constant native-endianness) 'big) (fx>= (constant ptr-bits) (datum bits)) (p2? (datum bits)))
                           #`(if (fx= (fxlogand i (fx- (fxquotient bits 8) 1)) 0)
                                 (#3%native-name v i k)
                                 (big-set! v i k))]
                          [else #`(big-set! v i k)])]
                     [(little)
                      #,(cond
                          [(and (constant unaligned-integers) (>= (constant ptr-bits) (datum bits)))
                           #`(#3%prim-name v i k 'little)]
                          [(and (eq? (constant native-endianness) 'little) (fx>= (constant ptr-bits) (datum bits)) (p2? (datum bits)))
                           #`(if (fx= (fxlogand i (fx- (fxquotient bits 8) 1)) 0)
                                 (#3%native-name v i k)
                                 (little-set! v i k))]
                          [else #`(little-set! v i k)])]
                     [else (unrecognized-endianness who eness)]))))])))

    (define $bytevector-s16-set! (bytevector-*-set! s 16))
    (define $bytevector-u16-set! (bytevector-*-set! u 16))
    (define $bytevector-s24-set! (bytevector-*-set! s 24))
    (define $bytevector-u24-set! (bytevector-*-set! u 24))
    (define $bytevector-s32-set! (bytevector-*-set! s 32))
    (define $bytevector-u32-set! (bytevector-*-set! u 32))
    (define $bytevector-s40-set! (bytevector-*-set! s 40))
    (define $bytevector-u40-set! (bytevector-*-set! u 40))
    (define $bytevector-s48-set! (bytevector-*-set! s 48))
    (define $bytevector-u48-set! (bytevector-*-set! u 48))
    (define $bytevector-s56-set! (bytevector-*-set! s 56))
    (define $bytevector-u56-set! (bytevector-*-set! u 56))
    (define $bytevector-s64-set! (bytevector-*-set! s 64))
    (define $bytevector-u64-set! (bytevector-*-set! u 64))
  )

  (set! native-endianness
    (lambda ()
      (#2%native-endianness)))

  (set-who! make-bytevector
    (case-lambda
      [(n fill)
       (unless (and (fixnum? n) (not ($fxu< (constant maximum-bytevector-length) n)))
         ($oops who "~s is not a valid bytevector length" n))
       (unless (fill? fill) (invalid-fill-value who fill))
       (#3%make-bytevector n fill)]
      [(n)
       (unless (and (fixnum? n) (not ($fxu< (constant maximum-bytevector-length) n)))
         ($oops who "~s is not a valid bytevector length" n))
       (#3%make-bytevector n)]))

  (set! bytevector? (lambda (x) (#2%bytevector? x)))

  (set! bytevector-length
    (lambda (v)
      (#2%bytevector-length v)))

  (set-who! $bytevector-set-immutable!
    (lambda (v)
      (unless (bytevector? v)
        ($oops who "~s is not a bytevector" v))
      (#3%$bytevector-set-immutable! v)))

  (set-who! mutable-bytevector?
    (lambda (v)
      (#3%mutable-bytevector? v)))

  (set-who! immutable-bytevector?
    (lambda (v)
      (#3%immutable-bytevector? v)))

  (set! bytevector-s8-ref
    (lambda (v i)
      (#2%bytevector-s8-ref v i)))

  (set! bytevector-u8-ref
    (lambda (v i)
      (#2%bytevector-u8-ref v i)))

  (set! bytevector-s8-set!
    (lambda (v i byte)
      (#2%bytevector-s8-set! v i byte)))

  (set! bytevector-u8-set!
    (lambda (v i octet)
      (#2%bytevector-u8-set! v i octet)))

  (set-who! $bytevector-set!
    (lambda (v i fill)
      (if ($bytevector-set!-check? 8 v i)
          (begin
            (unless (fill? fill) (invalid-value who fill))
            (#3%$bytevector-set! v i fill))
          (if (mutable-bytevector? v)
              (invalid-index who v i)
              (not-a-mutable-bytevector who v)))))

  (set-who! bytevector-s16-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 16 v i)
          (#3%bytevector-s16-native-ref v i)
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-u16-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 16 v i)
          (#3%bytevector-u16-native-ref v i)
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-s16-native-set!
    (let ([value-okay? (signed-value-pred 16)])
      (lambda (v i k)
        (if ($bytevector-set!-check? 16 v i)
            (begin
              (unless (value-okay? k) (invalid-value who k))
              (#3%bytevector-s16-native-set! v i k))
            (if (mutable-bytevector? v)
                (invalid-index who v i)
                (not-a-mutable-bytevector who v))))))

  (set-who! bytevector-u16-native-set!
    (let ([value-okay? (unsigned-value-pred 16)])
      (lambda (v i k)
        (if ($bytevector-set!-check? 16 v i)
            (begin
              (unless (value-okay? k) (invalid-value who k))
              (#3%bytevector-u16-native-set! v i k))
            (if (mutable-bytevector? v)
                (invalid-index who v i)
                (not-a-mutable-bytevector who v))))))

  (set-who! bytevector-s32-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 32 v i)
          (#3%bytevector-s32-native-ref v i)
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-u32-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 32 v i)
          (#3%bytevector-u32-native-ref v i)
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-s32-native-set!
    (let ([value-okay? (signed-value-pred 32)])
      (lambda (v i k)
        (if ($bytevector-set!-check? 32 v i)
            (begin
              (unless (value-okay? k) (invalid-value who k))
              (#3%bytevector-s32-native-set! v i k))
            (if (mutable-bytevector? v)
                (invalid-index who v i)
                (not-a-mutable-bytevector who v))))))

  (set-who! bytevector-u32-native-set!
    (let ([value-okay? (unsigned-value-pred 32)])
      (lambda (v i k)
        (if ($bytevector-set!-check? 32 v i)
            (begin
              (unless (value-okay? k) (invalid-value who k))
              (#3%bytevector-u32-native-set! v i k))
            (if (mutable-bytevector? v)
                (invalid-index who v i)
                (not-a-mutable-bytevector who v))))))

  (set-who! bytevector-s64-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 64 v i)
          (constant-case ptr-bits
            [(64) (#3%bytevector-s64-native-ref v i)]
            [(32)
             (constant-case native-endianness
               [(big)
                (logor (ash (#3%bytevector-s32-native-ref v i) 32)
                       (#3%bytevector-u32-native-ref v (fx+ i 4)))]
               [(little)
                (logor (ash (#3%bytevector-s32-native-ref v (fx+ i 4)) 32)
                       (#3%bytevector-u32-native-ref v i))])])
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-u64-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 64 v i)
          (constant-case ptr-bits
            [(64) (#3%bytevector-u64-native-ref v i)]
            [(32)
             (constant-case native-endianness
               [(big)
                (logor (ash (#3%bytevector-u32-native-ref v i) 32)
                       (#3%bytevector-u32-native-ref v (fx+ i 4)))]
               [(little)
                (logor (ash (#3%bytevector-u32-native-ref v (fx+ i 4)) 32)
                       (#3%bytevector-u32-native-ref v i))])])
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-s64-native-set!
    (let ([value-okay? (signed-value-pred 64)])
      (lambda (v i k)
        (if ($bytevector-set!-check? 64 v i)
            (begin
              (unless (value-okay? k) (invalid-value who k))
              (constant-case ptr-bits
                [(64) (#3%bytevector-s64-native-set! v i k)]
                [(32)
                 (constant-case native-endianness
                   [(big)
                    (#3%bytevector-s32-native-set! v i (ash k -32))
                    (#3%bytevector-u32-native-set! v (fx+ i 4) (logand k (- (expt 2 32) 1)))]
                   [(little)
                    (#3%bytevector-s32-native-set! v (fx+ i 4) (ash k -32))
                    (#3%bytevector-u32-native-set! v i (logand k (- (expt 2 32) 1)))])]))
            (if (mutable-bytevector? v)
                (invalid-index who v i)
                (not-a-mutable-bytevector who v))))))

  (set-who! bytevector-u64-native-set!
    (let ([value-okay? (unsigned-value-pred 64)])
      (lambda (v i k)
        (if ($bytevector-set!-check? 64 v i)
            (begin
              (unless (value-okay? k) (invalid-value who k))
              (constant-case ptr-bits
                [(64) (#3%bytevector-u64-native-set! v i k)]
                [(32)
                 (constant-case native-endianness
                   [(big)
                    (#3%bytevector-u32-native-set! v i (ash k -32))
                    (#3%bytevector-u32-native-set! v (fx+ i 4) (logand k (- (expt 2 32) 1)))]
                   [(little)
                    (#3%bytevector-u32-native-set! v (fx+ i 4) (ash k -32))
                    (#3%bytevector-u32-native-set! v i (logand k (- (expt 2 32) 1)))])]))
            (if (mutable-bytevector? v)
                (invalid-index who v i)
                (not-a-mutable-bytevector who v))))))

  (set-who! bytevector-ieee-single-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 32 v i)
          (#3%bytevector-ieee-single-native-ref v i)
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-ieee-double-native-ref
    (lambda (v i)
      (if ($bytevector-ref-check? 64 v i)
          (#3%bytevector-ieee-double-native-ref v i)
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v)))))

  (set-who! bytevector-ieee-single-native-set!
    (lambda (v i x)
      (if ($bytevector-set!-check? 32 v i)
         ; inline routine checks to make sure x is a real number
          (#3%bytevector-ieee-single-native-set! v i x)
          (if (mutable-bytevector? v)
              (invalid-index who v i)
              (not-a-mutable-bytevector who v)))))

  (set-who! bytevector-ieee-double-native-set!
    (lambda (v i x)
      (if ($bytevector-set!-check? 64 v i)
         ; inline routine checks to make sure x is a real number
          (#3%bytevector-ieee-double-native-set! v i x)
          (if (mutable-bytevector? v)
              (invalid-index who v i)
              (not-a-mutable-bytevector who v)))))

  (set-who! bytevector-copy
    (lambda (v)
      (unless (bytevector? v) (not-a-bytevector who v))
      (let* ([n (bytevector-length v)] [v2 (make-bytevector n)])
        ($ptr-copy! v (constant bytevector-data-disp) v2
          (constant bytevector-data-disp)
          (fxsrl
            (fx+ n (fx- (constant ptr-bytes) 1))
            (constant log2-ptr-bytes)))
        v2)))

  (set-who! bytevector-copy!
    (lambda (v1 i1 v2 i2 k)
      (unless (bytevector? v1) (not-a-bytevector who v1))
      (unless (mutable-bytevector? v2) (not-a-mutable-bytevector who v2))
      (let ([n1 (bytevector-length v1)] [n2 (bytevector-length v2)])
        (unless (and (fixnum? i1) (fx>= i1 0))
          ($oops who "invalid start value ~s" i1))
        (unless (and (fixnum? i2) (fx>= i2 0))
          ($oops who "invalid start value ~s" i2))
        (unless (and (fixnum? k) (fx>= k 0))
          ($oops who "invalid count ~s" k))
        (unless (fx<= k (fx- n1 i1)) ; avoid overflow
          ($oops who "index ~s + count ~s is beyond the end of ~s" i1 k v1))
        (unless (fx<= k (fx- n2 i2)) ; avoid overflow
          ($oops who "index ~s + count ~s is beyond the end of ~s" i2 k v2))
       ; whew!
        (#3%bytevector-copy! v1 i1 v2 i2 k))))

  (set-who! bytevector->immutable-bytevector
    (lambda (v)
      (cond
        [(immutable-bytevector? v) v]
        [(eqv? v '#vu8()) ($tc-field 'null-immutable-bytevector ($tc))]
        [else
         (unless (bytevector? v) ($oops who "~s is not a bytevector" v))
         (let ([v2 (bytevector-copy v)])
           ($bytevector-set-immutable! v2)
           v2)])))

  (set-who! bytevector-fill!
    (lambda (v fill)
      (unless (mutable-bytevector? v) (not-a-mutable-bytevector who v))
      (unless (fill? fill) (invalid-fill-value who fill))
      (#3%bytevector-fill! v fill)))

  (set-who! bytevector=?
    (lambda (v1 v2)
      (unless (bytevector? v1) (not-a-bytevector who v1))
      (unless (bytevector? v2) (not-a-bytevector who v2))
      (#3%bytevector=? v1 v2)))

  (set-who! $bytevector-ref-check?
    (lambda (bits v i)
     ; inlined handles only constant bits argument
      (case bits
        [(8) (#2%$bytevector-ref-check? 8 v i)]
        [(16) (#2%$bytevector-ref-check? 16 v i)]
        [(32) (#2%$bytevector-ref-check? 32 v i)]
        [(64) (#2%$bytevector-ref-check? 64 v i)]
        [else ($oops who "invalid bits argument ~s" bits)])))

  (set-who! $bytevector-set!-check?
    (lambda (bits v i)
     ; inlined handles only constant bits argument
      (case bits
        [(8) (#2%$bytevector-set!-check? 8 v i)]
        [(16) (#2%$bytevector-set!-check? 16 v i)]
        [(32) (#2%$bytevector-set!-check? 32 v i)]
        [(64) (#2%$bytevector-set!-check? 64 v i)]
        [else ($oops who "invalid bits argument ~s" bits)])))

  (set-who! bytevector-s16-ref
    (lambda (v i eness)
      ($bytevector-s16-ref v i eness who)))

  (set-who! bytevector-u16-ref
    (lambda (v i eness)
      ($bytevector-u16-ref v i eness who)))

  (set-who! bytevector-s24-ref
    (lambda (v i eness)
      ($bytevector-s24-ref v i eness who)))

  (set-who! bytevector-u24-ref
    (lambda (v i eness)
      ($bytevector-u24-ref v i eness who)))

  (set-who! bytevector-s32-ref
    (lambda (v i eness)
      ($bytevector-s32-ref v i eness who)))

  (set-who! bytevector-u32-ref
    (lambda (v i eness)
      ($bytevector-u32-ref v i eness who)))

  (set-who! bytevector-s40-ref
    (lambda (v i eness)
      ($bytevector-s40-ref v i eness who)))

  (set-who! bytevector-u40-ref
    (lambda (v i eness)
      ($bytevector-u40-ref v i eness who)))

  (set-who! bytevector-s48-ref
    (lambda (v i eness)
      ($bytevector-s48-ref v i eness who)))

  (set-who! bytevector-u48-ref
    (lambda (v i eness)
      ($bytevector-u48-ref v i eness who)))

  (set-who! bytevector-s56-ref
    (lambda (v i eness)
      ($bytevector-s56-ref v i eness who)))

  (set-who! bytevector-u56-ref
    (lambda (v i eness)
      ($bytevector-u56-ref v i eness who)))

  (set-who! bytevector-s64-ref
    (lambda (v i eness)
      ($bytevector-s64-ref v i eness who)))

  (set-who! bytevector-u64-ref
    (lambda (v i eness)
      ($bytevector-u64-ref v i eness who)))

  (set-who! bytevector-s16-set!
    (lambda (v i k eness)
      ($bytevector-s16-set! v i k eness who)))

  (set-who! bytevector-u16-set!
    (lambda (v i k eness)
      ($bytevector-u16-set! v i k eness who)))

  (set-who! bytevector-s24-set!
    (lambda (v i k eness)
      ($bytevector-s24-set! v i k eness who)))

  (set-who! bytevector-u24-set!
    (lambda (v i k eness)
      ($bytevector-u24-set! v i k eness who)))

  (set-who! bytevector-s32-set!
    (lambda (v i k eness)
      ($bytevector-s32-set! v i k eness who)))

  (set-who! bytevector-u32-set!
    (lambda (v i k eness)
      ($bytevector-u32-set! v i k eness who)))

  (set-who! bytevector-s40-set!
    (lambda (v i k eness)
      ($bytevector-s40-set! v i k eness who)))

  (set-who! bytevector-u40-set!
    (lambda (v i k eness)
      ($bytevector-u40-set! v i k eness who)))

  (set-who! bytevector-s48-set!
    (lambda (v i k eness)
      ($bytevector-s48-set! v i k eness who)))

  (set-who! bytevector-u48-set!
    (lambda (v i k eness)
      ($bytevector-u48-set! v i k eness who)))

  (set-who! bytevector-s56-set!
    (lambda (v i k eness)
      ($bytevector-s56-set! v i k eness who)))

  (set-who! bytevector-u56-set!
    (lambda (v i k eness)
      ($bytevector-u56-set! v i k eness who)))

  (set-who! bytevector-s64-set!
    (lambda (v i k eness)
      ($bytevector-s64-set! v i k eness who)))

  (set-who! bytevector-u64-set!
    (lambda (v i k eness)
      ($bytevector-u64-set! v i k eness who)))

  (set-who! bytevector-ieee-single-ref
    (lambda (v i eness)
      (define (swap-ref v i)
        (bytevector-ieee-single-native-ref
          (bytevector
            (bytevector-u8-ref v (fx+ i 3))
            (bytevector-u8-ref v (fx+ i 2))
            (bytevector-u8-ref v (fx+ i 1))
            (bytevector-u8-ref v i))
          0))
      (define (noswap-ref v i)
        (bytevector-ieee-single-native-ref
          (bytevector
            (bytevector-u8-ref v i)
            (bytevector-u8-ref v (fx+ i 1))
            (bytevector-u8-ref v (fx+ i 2))
            (bytevector-u8-ref v (fx+ i 3)))
          0))
      (unless (bytevector? v) (not-a-bytevector who v))
      (unaligned-ref-check who 4 v i)
      (if (or (constant unaligned-floats) (fx= (fxlogand i 3) 0))
          (if (eq? eness (native-endianness))
              (#3%bytevector-ieee-single-native-ref v i)
              (if (constant-case native-endianness
                    [(little) (eq? eness 'big)]
                    [(big) (eq? eness 'little)])
                  (swap-ref v i)
                  (unrecognized-endianness who eness)))
          (if (eq? eness (native-endianness))
              (noswap-ref v i)
              (if (constant-case native-endianness
                    [(little) (eq? eness 'big)]
                    [(big) (eq? eness 'little)])
                  (swap-ref v i)
                  (unrecognized-endianness who eness))))))

  (set-who! bytevector-ieee-double-ref
    (lambda (v i eness)
      (define (swap-ref v i)
        (bytevector-ieee-double-native-ref
          (bytevector
            (bytevector-u8-ref v (fx+ i 7))
            (bytevector-u8-ref v (fx+ i 6))
            (bytevector-u8-ref v (fx+ i 5))
            (bytevector-u8-ref v (fx+ i 4))
            (bytevector-u8-ref v (fx+ i 3))
            (bytevector-u8-ref v (fx+ i 2))
            (bytevector-u8-ref v (fx+ i 1))
            (bytevector-u8-ref v i))
          0))
      (define (noswap-ref v i)
        (bytevector-ieee-double-native-ref
          (bytevector
            (bytevector-u8-ref v i)
            (bytevector-u8-ref v (fx+ i 1))
            (bytevector-u8-ref v (fx+ i 2))
            (bytevector-u8-ref v (fx+ i 3))
            (bytevector-u8-ref v (fx+ i 4))
            (bytevector-u8-ref v (fx+ i 5))
            (bytevector-u8-ref v (fx+ i 6))
            (bytevector-u8-ref v (fx+ i 7)))
          0))
      (unless (bytevector? v) (not-a-bytevector who v))
      (unaligned-ref-check who 8 v i)
      (if (or (constant unaligned-floats) (fx= (fxlogand i 7) 0))
          (if (eq? eness (native-endianness))
              (#3%bytevector-ieee-double-native-ref v i)
              (if (constant-case native-endianness
                    [(little) (eq? eness 'big)]
                    [(big) (eq? eness 'little)])
                  (swap-ref v i)
                  (unrecognized-endianness who eness)))
          (if (eq? eness (native-endianness))
              (noswap-ref v i)
              (if (constant-case native-endianness
                    [(little) (eq? eness 'big)]
                    [(big) (eq? eness 'little)])
                  (swap-ref v i)
                  (unrecognized-endianness who eness))))))

  (set-who! bytevector-ieee-single-set!
    (lambda (v i x eness)
      (define (swap-set! v i x)
        (let ([v2 (make-bytevector 4)])
          (bytevector-ieee-single-native-set! v2 0 x)
          (bytevector-u8-set! v i (bytevector-u8-ref v2 3))
          (bytevector-u8-set! v (fx+ i 1) (bytevector-u8-ref v2 2))
          (bytevector-u8-set! v (fx+ i 2) (bytevector-u8-ref v2 1))
          (bytevector-u8-set! v (fx+ i 3) (bytevector-u8-ref v2 0))))
      (define (noswap-set! v i x)
        (let ([v2 (make-bytevector 4)])
          (bytevector-ieee-single-native-set! v2 0 x)
          (bytevector-u8-set! v i (bytevector-u8-ref v2 0))
          (bytevector-u8-set! v (fx+ i 1) (bytevector-u8-ref v2 1))
          (bytevector-u8-set! v (fx+ i 2) (bytevector-u8-ref v2 2))
          (bytevector-u8-set! v (fx+ i 3) (bytevector-u8-ref v2 3))))
      (unless (mutable-bytevector? v) (not-a-mutable-bytevector who v))
      (unaligned-ref-check who 4 v i)
      (let ([x ($real->flonum x who)])
        (if (or (constant unaligned-floats) (fx= (fxlogand i 3) 0))
            (if (eq? eness (native-endianness))
                (#3%bytevector-ieee-single-native-set! v i x)
                (if (constant-case native-endianness
                      [(little) (eq? eness 'big)]
                      [(big) (eq? eness 'little)])
                    (swap-set! v i x)
                    (unrecognized-endianness who eness)))
            (if (eq? eness (native-endianness))
                (noswap-set! v i x)
                (if (constant-case native-endianness
                      [(little) (eq? eness 'big)]
                      [(big) (eq? eness 'little)])
                    (swap-set! v i x)
                    (unrecognized-endianness who eness)))))))

  (set-who! bytevector-ieee-double-set!
    (lambda (v i x eness)
      (define (swap-set! v i x)
        (let ([v2 (make-bytevector 8)])
          (bytevector-ieee-double-native-set! v2 0 x)
          (bytevector-u8-set! v i (bytevector-u8-ref v2 7))
          (bytevector-u8-set! v (fx+ i 1) (bytevector-u8-ref v2 6))
          (bytevector-u8-set! v (fx+ i 2) (bytevector-u8-ref v2 5))
          (bytevector-u8-set! v (fx+ i 3) (bytevector-u8-ref v2 4))
          (bytevector-u8-set! v (fx+ i 4) (bytevector-u8-ref v2 3))
          (bytevector-u8-set! v (fx+ i 5) (bytevector-u8-ref v2 2))
          (bytevector-u8-set! v (fx+ i 6) (bytevector-u8-ref v2 1))
          (bytevector-u8-set! v (fx+ i 7) (bytevector-u8-ref v2 0))))
      (define (noswap-set! v i x)
        (let ([v2 (make-bytevector 8)])
          (bytevector-ieee-double-native-set! v2 0 x)
          (bytevector-u8-set! v i (bytevector-u8-ref v2 0))
          (bytevector-u8-set! v (fx+ i 1) (bytevector-u8-ref v2 1))
          (bytevector-u8-set! v (fx+ i 2) (bytevector-u8-ref v2 2))
          (bytevector-u8-set! v (fx+ i 3) (bytevector-u8-ref v2 3))
          (bytevector-u8-set! v (fx+ i 4) (bytevector-u8-ref v2 4))
          (bytevector-u8-set! v (fx+ i 5) (bytevector-u8-ref v2 5))
          (bytevector-u8-set! v (fx+ i 6) (bytevector-u8-ref v2 6))
          (bytevector-u8-set! v (fx+ i 7) (bytevector-u8-ref v2 7))))
      (unless (mutable-bytevector? v) (not-a-mutable-bytevector who v))
      (unaligned-ref-check who 8 v i)
      (let ([x ($real->flonum x who)])
        (if (or (constant unaligned-floats) (fx= (fxlogand i 7) 0))
            (if (eq? eness (native-endianness))
                (#3%bytevector-ieee-double-native-set! v i x)
                (if (constant-case native-endianness
                      [(little) (eq? eness 'big)]
                      [(big) (eq? eness 'little)])
                    (swap-set! v i x)
                    (unrecognized-endianness who eness)))
            (if (eq? eness (native-endianness))
                (noswap-set! v i x)
                (if (constant-case native-endianness
                      [(little) (eq? eness 'big)]
                      [(big) (eq? eness 'little)])
                    (swap-set! v i x)
                    (unrecognized-endianness who eness)))))))

  (let ()
    (define ($bytevector-s8-ref v i eness who)
      (if ($bytevector-ref-check? 8 v i)
          (begin
            (unless (memq eness '(little big)) (unrecognized-endianness who eness))
            (#3%bytevector-s8-ref v i))
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v))))

    (define ($bytevector-u8-ref v i eness who)
      (if ($bytevector-ref-check? 8 v i)
          (begin
            (unless (memq eness '(little big)) (unrecognized-endianness who eness))
            (#3%bytevector-u8-ref v i))
          (if (bytevector? v)
              (invalid-index who v i)
              (not-a-bytevector who v))))

    (set-who! bytevector-sint-ref
      (lambda (v i eness size)
        (case size
          [(1) ($bytevector-s8-ref v i eness who)]
          [(2) ($bytevector-s16-ref v i eness who)]
          [(4) ($bytevector-s32-ref v i eness who)]
          [(8) ($bytevector-s64-ref v i eness who)]
          [else
           (unless (bytevector? v) (not-a-bytevector who v))
           (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
           (unaligned-ref-check who size v i)
           (case eness
             [(big) ($bytevector-sint-big-ref v i size)]
             [(little) ($bytevector-sint-little-ref v i size)]
             [else (unrecognized-endianness who eness)])])))

    (set-who! bytevector-uint-ref
      (lambda (v i eness size)
        (case size
          [(1) ($bytevector-u8-ref v i eness who)]
          [(2) ($bytevector-u16-ref v i eness who)]
          [(4) ($bytevector-u32-ref v i eness who)]
          [(8) ($bytevector-u64-ref v i eness who)]
          [else
           (unless (bytevector? v) (not-a-bytevector who v))
           (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
           (unaligned-ref-check who size v i)
           (case eness
             [(big) ($bytevector-uint-big-ref v i size)]
             [(little) ($bytevector-uint-little-ref v i size)]
             [else (unrecognized-endianness who eness)])]))))

  (let ()
    (define $bytevector-s8-set!
      (let ([value-okay? (signed-value-pred 8)])
        (lambda (v i k eness who)
          (if ($bytevector-set!-check? 8 v i)
              (begin
                (unless (value-okay? k) (invalid-value who k))
                (unless (memq eness '(little big)) (unrecognized-endianness who eness))
                (#3%bytevector-s8-set! v i k))
              (if (mutable-bytevector? v)
                  (invalid-index who v i)
                  (not-a-mutable-bytevector who v))))))

    (define $bytevector-u8-set!
      (let ([value-okay? (unsigned-value-pred 8)])
        (lambda (v i k eness who)
          (if ($bytevector-set!-check? 8 v i)
              (begin
                (unless (value-okay? k) (invalid-value who k))
                (unless (memq eness '(little big)) (unrecognized-endianness who eness))
                (#3%bytevector-u8-set! v i k))
              (if (mutable-bytevector? v)
                  (invalid-index who v i)
                  (not-a-mutable-bytevector who v))))))

    (set-who! bytevector-sint-set!
      (lambda (v i k eness size)
        (case size
          [(1) ($bytevector-s8-set! v i k eness who)]
          [(2) ($bytevector-s16-set! v i k eness who)]
          [(4) ($bytevector-s32-set! v i k eness who)]
          [(8) ($bytevector-s64-set! v i k eness who)]
          [else
           (unless (mutable-bytevector? v) (not-a-mutable-bytevector who v))
           (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
           (unaligned-ref-check who size v i)
           (unless (and (or (fixnum? k) (bignum? k))
                        (let ([k (ash k (fx- 1 (fx* size 8)))])
                          (or (fx= k 0) (fx= k -1))))
             (invalid-value who k))
           (case eness
             [(big) ($bytevector-int-big-set! v i k size)]
             [(little) ($bytevector-int-little-set! v i k size)]
             [else (unrecognized-endianness who eness)])])))

    (set-who! bytevector-uint-set!
      (lambda (v i k eness size)
        (case size
          [(1) ($bytevector-u8-set! v i k eness who)]
          [(2) ($bytevector-u16-set! v i k eness who)]
          [(4) ($bytevector-u32-set! v i k eness who)]
          [(8) ($bytevector-u64-set! v i k eness who)]
          [else
           (unless (mutable-bytevector? v) (not-a-mutable-bytevector who v))
           (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
           (unaligned-ref-check who size v i)
           (unless (and (or (fixnum? k) (bignum? k))
                        (fx= (ash k (fx- (fx* size 8))) 0))
             (invalid-value who k))
           (case eness
             [(big) ($bytevector-int-big-set! v i k size)]
             [(little) ($bytevector-int-little-set! v i k size)]
             [else (unrecognized-endianness who eness)])]))))

  (let ()
    (define-syntax bv->list
      (syntax-rules ()
        [(_ bytes ref)
         (lambda (v who)
           (unless (bytevector? v) (not-a-bytevector who v))
           (let ([n (bytevector-length v)])
             (unless (fx= (fxlogand n (fx- bytes 1)) 0)
               (size-multiple-error who n bytes))
             (let loop ([i (fx- n bytes)] [ls '()])
               (if (fx> i 0)
                   (loop
                     (fx- i (fx* bytes 2))
                     (list* (ref v (fx- i bytes)) (ref v i) ls))
                   (if (fx= i 0) (cons (ref v 0) ls) ls)))))]))

    (define $bytevector->s8-list (bv->list 1 bytevector-s8-ref))
    (define $bytevector->u8-list (bv->list 1 bytevector-u8-ref))
    (define $bytevector->s16-native-list (bv->list 2 bytevector-s16-native-ref))
    (define $bytevector->u16-native-list (bv->list 2 bytevector-u16-native-ref))
    (define $bytevector->s32-native-list (bv->list 4 bytevector-s32-native-ref))
    (define $bytevector->u32-native-list (bv->list 4 bytevector-u32-native-ref))
    (define $bytevector->s64-native-list (bv->list 8 bytevector-s64-native-ref))
    (define $bytevector->u64-native-list (bv->list 8 bytevector-u64-native-ref))

    (set-who! bytevector->s8-list
      (lambda (v)
        ($bytevector->s8-list v who)))

    (set-who! bytevector->u8-list
      (lambda (v)
        ($bytevector->u8-list v who)))

    (set-who! bytevector->sint-list
      (lambda (v eness size)
        (define (big->list v size)
          (unless (bytevector? v) (not-a-bytevector who v))
          (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
          (let ([n (bytevector-length v)])
            (unless (fx= (fxremainder n size) 0) (size-multiple-error who n size))
            (let f ([i 0])
              (if (fx= i n)
                  '()
                  (cons ($bytevector-sint-big-ref v i size)
                        (f (fx+ i size)))))))
        (define (little->list v size)
          (unless (bytevector? v) (not-a-bytevector who v))
          (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
          (let ([n (bytevector-length v)])
            (unless (fx= (fxremainder n size) 0) (size-multiple-error who n size))
            (let f ([i 0])
              (if (fx= i n)
                  '()
                  (cons ($bytevector-sint-little-ref v i size)
                        (f (fx+ i size)))))))
        (if (eq? eness (native-endianness))
            (case size
              [(1) ($bytevector->s8-list v who)]
              [(2) ($bytevector->s16-native-list v who)]
              [(4) ($bytevector->s32-native-list v who)]
              [(8) ($bytevector->s64-native-list v who)]
              [else
               (constant-case native-endianness
                 [(little) (little->list v size)]
                 [(big) (big->list v size)])])
            (constant-case native-endianness
              [(little)
               (if (eq? eness 'big)
                   (big->list v size)
                   (unrecognized-endianness who eness))]
              [(big)
               (if (eq? eness 'little)
                   (little->list v size)
                   (unrecognized-endianness who eness))]))))

    (set-who! bytevector->uint-list
      (lambda (v eness size)
        (define (big->list v size)
          (unless (bytevector? v) (not-a-bytevector who v))
          (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
          (let ([n (bytevector-length v)])
            (unless (fx= (fxremainder n size) 0) (size-multiple-error who n size))
            (let f ([i 0])
              (if (fx= i n)
                  '()
                  (cons ($bytevector-uint-big-ref v i size)
                        (f (fx+ i size)))))))
        (define (little->list v size)
          (unless (bytevector? v) (not-a-bytevector who v))
          (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
          (let ([n (bytevector-length v)])
            (unless (fx= (fxremainder n size) 0) (size-multiple-error who n size))
            (let f ([i 0])
              (if (fx= i n)
                  '()
                  (cons ($bytevector-uint-little-ref v i size)
                        (f (fx+ i size)))))))
        (if (eq? eness (native-endianness))
            (case size
              [(1) ($bytevector->u8-list v who)]
              [(2) ($bytevector->u16-native-list v who)]
              [(4) ($bytevector->u32-native-list v who)]
              [(8) ($bytevector->u64-native-list v who)]
              [else
               (constant-case native-endianness
                 [(little) (little->list v size)]
                 [(big) (big->list v size)])])
            (constant-case native-endianness
              [(little)
               (if (eq? eness 'big)
                   (big->list v size)
                   (unrecognized-endianness who eness))]
              [(big)
               (if (eq? eness 'little)
                   (little->list v size)
                   (unrecognized-endianness who eness))]))))
  )

  (let ()
    (define-syntax list->bv
      (syntax-rules ()
        [(_ bytes set! vokay?)
         (let ([value-okay? vokay?])
           (lambda (ls who)
             (let* ([n ($list-length ls who)]
                    [v (make-bytevector (fx* n bytes))])
               (let loop ([ls ls] [i 0])
                 (unless (null? ls)
                   (let ([k (car ls)])
                     (unless (value-okay? k) (invalid-value who k))
                     (set! v i k))
                   (let ([ls (cdr ls)])
                     (unless (null? ls)
                       (let ([k (car ls)])
                         (unless (value-okay? k) (invalid-value who k))
                         (set! v (fx+ i bytes) k))
                       (loop (cdr ls) (fx+ i (fx* bytes 2)))))))
               v)))]))

    (define $s8-list->bytevector (list->bv 1 bytevector-s8-set! (signed-value-pred 8)))
    (define $u8-list->bytevector (list->bv 1 bytevector-u8-set! (unsigned-value-pred 8)))
    (define $s16-native-list->bytevector (list->bv 2 bytevector-s16-native-set! (signed-value-pred 16)))
    (define $u16-native-list->bytevector (list->bv 2 bytevector-u16-native-set! (unsigned-value-pred 16)))
    (define $s32-native-list->bytevector (list->bv 4 bytevector-s32-native-set! (signed-value-pred 32)))
    (define $u32-native-list->bytevector (list->bv 4 bytevector-u32-native-set! (unsigned-value-pred 32)))
    (define $s64-native-list->bytevector (list->bv 8 bytevector-s64-native-set! (signed-value-pred 64)))
    (define $u64-native-list->bytevector (list->bv 8 bytevector-u64-native-set! (unsigned-value-pred 64)))

    (set-who! s8-list->bytevector
      (lambda (ls)
        ($s8-list->bytevector ls who)))

    (set-who! u8-list->bytevector
      (lambda (ls)
        ($u8-list->bytevector ls who)))

    (set-who! sint-list->bytevector
      (lambda (ls eness size)
        (define (list->big v size)
          (let ([n ($list-length ls who)])
            (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
            (let ([v (make-bytevector (fx* n size))])
              (let f ([ls ls] [i 0])
                (unless (null? ls)
                  (let ([k (car ls)])
                    (unless (and (or (fixnum? k) (bignum? k))
                                 (let ([k (ash k (fx- 1 (fx* size 8)))])
                                   (or (fx= k 0) (fx= k -1))))
                      (invalid-value who k))
                    ($bytevector-int-big-set! v i k size))
                  (f (cdr ls) (fx+ i size))))
              v)))
        (define (list->little v size)
          (let ([n ($list-length ls who)])
            (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
            (let ([v (make-bytevector (fx* n size))])
              (let f ([ls ls] [i 0])
                (unless (null? ls)
                  (let ([k (car ls)])
                    (unless (and (or (fixnum? k) (bignum? k))
                                 (let ([k (ash k (fx- 1 (fx* size 8)))])
                                   (or (fx= k 0) (fx= k -1))))
                      (invalid-value who k))
                    ($bytevector-int-little-set! v i k size))
                  (f (cdr ls) (fx+ i size))))
              v)))
        (if (eq? eness (native-endianness))
            (case size
              [(1) ($s8-list->bytevector ls who)]
              [(2) ($s16-native-list->bytevector ls who)]
              [(4) ($s32-native-list->bytevector ls who)]
              [(8) ($s64-native-list->bytevector ls who)]
              [else
               (constant-case native-endianness
                 [(little) (list->little ls size)]
                 [(big) (list->big ls size)])])
            (constant-case native-endianness
              [(little)
               (if (eq? eness 'big)
                   (list->big ls size)
                   (unrecognized-endianness who eness))]
              [(big)
               (if (eq? eness 'little)
                   (list->little ls size)
                   (unrecognized-endianness who eness))]))))

    (set-who! uint-list->bytevector
      (lambda (ls eness size)
        (define (list->big v size)
          (let ([n ($list-length ls who)])
            (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
            (let ([v (make-bytevector (fx* n size))])
              (let f ([ls ls] [i 0])
                (unless (null? ls)
                  (let ([k (car ls)])
                    (unless (and (or (fixnum? k) (bignum? k))
                                 (fx= (ash k (fx- (fx* size 8))) 0))
                      (invalid-value who k))
                    ($bytevector-int-big-set! v i k size))
                  (f (cdr ls) (fx+ i size))))
              v)))
        (define (list->little v size)
          (let ([n ($list-length ls who)])
            (unless (and (fixnum? size) (fx> size 0)) (invalid-size who size))
            (let ([v (make-bytevector (fx* n size))])
              (let f ([ls ls] [i 0])
                (unless (null? ls)
                  (let ([k (car ls)])
                    (unless (and (or (fixnum? k) (bignum? k))
                                 (fx= (ash k (fx- (fx* size 8))) 0))
                      (invalid-value who k))
                    ($bytevector-int-little-set! v i k size))
                  (f (cdr ls) (fx+ i size))))
              v)))
        (if (eq? eness (native-endianness))
            (case size
              [(1) ($u8-list->bytevector ls who)]
              [(2) ($u16-native-list->bytevector ls who)]
              [(4) ($u32-native-list->bytevector ls who)]
              [(8) ($u64-native-list->bytevector ls who)]
              [else
               (constant-case native-endianness
                 [(little) (list->little ls size)]
                 [(big) (list->big ls size)])])
            (constant-case native-endianness
              [(little)
               (if (eq? eness 'big)
                   (list->big ls size)
                   (unrecognized-endianness who eness))]
              [(big)
               (if (eq? eness 'little)
                   (list->little ls size)
                   (unrecognized-endianness who eness))]))))
  )

  (let ()
    ;; Store uncompressed size as u64, using low bits to indicate compression format:
    (define uncompressed-length-length (ftype-sizeof integer-64))
    ;; Always big-endian, so that compressed data is portable.
    (define uncompressed-length-endianness (endianness big))

    (define fp-bytevector-compress-size
      (foreign-procedure "(cs)bytevector_compress_size" (iptr int) uptr))
    (define fp-bytevector-compress
      (foreign-procedure "(cs)bytevector_compress" (scheme-object iptr iptr scheme-object iptr iptr int) scheme-object))
    (define fp-bytevector-uncompress
      (foreign-procedure "(cs)bytevector_uncompress" (scheme-object iptr iptr scheme-object iptr iptr int) scheme-object))

    (let ()
      (define (compress who bv fmt offset)
        (let* ([dest-max-len (fp-bytevector-compress-size (bytevector-length bv) fmt)]
               [dest-alloc-len (min (+ dest-max-len offset) (constant maximum-bytevector-length))]
               [dest-bv (make-bytevector dest-alloc-len)])
          (let ([r (fp-bytevector-compress dest-bv offset (fx- dest-alloc-len offset) bv 0 (bytevector-length bv) fmt)])
            (if (string? r)
                ($oops who r bv)
                (bytevector-truncate! dest-bv (fx+ r offset))))))

      (set-who! $bytevector-compress
        (lambda (bv fmt)
          (compress who bv fmt 0)))

      (set-who! bytevector-compress
        (lambda (bv)
          (unless (bytevector? bv) (not-a-bytevector who bv))
          (let* ([fmt ($tc-field 'compress-format ($tc))]
                 [dest-bv (compress who bv fmt uncompressed-length-length)])
            (let ([tag (bitwise-ior
                         (bitwise-arithmetic-shift-left (bytevector-length bv) (constant COMPRESS-FORMAT-BITS))
                         fmt)])
              ($bytevector-u64-set! dest-bv 0 tag uncompressed-length-endianness who)
              dest-bv)))))

    (let ()
      (define (uncompress who bv dest-length fmt offset)
        (unless (and (fixnum? dest-length) ($fxu< dest-length (constant maximum-bytevector-length)))
          ($oops who "bytevector ~s claims invalid uncompressed size ~s" bv dest-length))
        (let ([dest-bv (make-bytevector dest-length)])
          (let ([r (fp-bytevector-uncompress dest-bv 0 dest-length bv offset (fx- (bytevector-length bv) offset) fmt)])
            (cond
              [(string? r) ($oops who r bv)]
              [(fx= r dest-length) dest-bv]
              [else ($oops who "uncompressed size ~s for ~s is smaller than expected size ~s" r bv dest-length)]))))

      (set-who! $bytevector-uncompress
        (lambda (bv dest-length fmt)
          (uncompress who bv dest-length fmt 0)))

      (set-who! bytevector-uncompress
        (lambda (bv)
          (unless (bytevector? bv) (not-a-bytevector who bv))
          (unless (>= (bytevector-length bv) uncompressed-length-length) ($oops who "invalid data in source bytevector ~s" bv))
          (let* ([tag ($bytevector-u64-ref bv 0 uncompressed-length-endianness who)]
                 [fmt (logand tag (fx- (fxsll 1 (constant COMPRESS-FORMAT-BITS)) 1))]
                 [dest-length (bitwise-arithmetic-shift-right tag (constant COMPRESS-FORMAT-BITS))])
            (uncompress who bv dest-length fmt uncompressed-length-length))))))
)
