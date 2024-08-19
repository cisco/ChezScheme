;;; bytevector.ms
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

(define dynamic-native-endianness?
  (case (machine-type)
    [(pb tpb pb32 tpb32) #t]
    [else #f]))

(define test-endian-sensitive-cp0-expansion
  (if dynamic-native-endianness?
      (case-lambda
       [(expr expected) #t]
       [(equiv? expr expected) #t])
      test-cp0-expansion))

(mat native-endianness
 ; wrong argument count
  (error? (native-endianness 'big))

  (and (memq (native-endianness) '(big little)) #t)
  (eq? (native-endianness)
       (case (machine-type)
         [(i3le ti3le i3nt ti3nt a6nt ta6nt arm64nt tarm64nt i3ob ti3ob i3fb ti3fb i3nb ti3nb
                i3osx ti3osx a6le ta6le a6nb ta6nb
                a6osx ta6osx a6ios ta6ios a6fb ta6fb a6ob ta6ob a6s2 ta6s2 i3s2 ti3s2 i3qnx ti3qnx
                arm32le tarm32le arm64le tarm64le arm64osx tarm64osx arm64ios tarm64ios rv64le trv64le
                la64le tla64le)
          'little]
         [(ppc32le tppc32le ppc32osx tppc32osx) 'big]
         [(pb tpb) (native-endianness)]
         [(pb64l tpb64l pb32l tpb32l) 'little]
         [(pb64b tpb64b pb32b tpb32b) 'big]
         [else (errorf #f "unrecognized machine type")]))
)

(mat endianness
 ; invalid endianness
  (error? (endianness spam))
  (error? (endianness 'big))
  (error? (endianness "little"))

 ; invalid syntax
  (error? (endianness))
  (error? (endianness . big))
  (error? (endianness big little))

  (eq? (endianness big) 'big)
  (eq? (endianness little) 'little)
  (eq? (let ([big 'large]) (endianness big)) 'big)
  (eq? (let ([little 'small]) (endianness little)) 'little)
)

(mat make-bytevector
 ; wrong argument count
  (error? (make-bytevector))
  (error? (make-bytevector 0 0 0))

 ; invalid size
  (error? (make-bytevector -1))
  (error? (make-bytevector -1 0))
  (error? (make-bytevector (+ (most-positive-fixnum) 1)))
  (error? (make-bytevector (+ (most-positive-fixnum) 1) -1))
  (error? (begin (make-bytevector 'a -1) #f))

 ; invalid fill
  (error? (make-bytevector 3 'a))
  (error? (make-bytevector 10 256))
  (error? (make-bytevector 10 -129))
  (error? (make-bytevector 10 (+ (most-positive-fixnum) 1)))
  (error? (begin (make-bytevector 10 (- (most-negative-fixnum) 1)) #f))

  (eqv? (bytevector-length (make-bytevector 10)) 10)
  (eqv? (let ([n 11]) (bytevector-length (make-bytevector n))) 11)
  (eqv? (bytevector-length (make-bytevector 100)) 100)
  (eqv? (bytevector-length (make-bytevector (+ 100 17))) 117)
  (eq? (make-bytevector 0) #vu8())
  (let ([x (make-bytevector 10)])
    (and (= (bytevector-length x) 10)
         (andmap fixnum? (bytevector->s8-list x))))
  (do ([n -128 (fx+ n 1)])
      ((fx= n 128) #t)
    (let ([v (make-bytevector 3)])
      (unless (and (fixnum? (bytevector-s8-ref v 0))
                   (fixnum? (bytevector-s8-ref v 1))
                   (fixnum? (bytevector-s8-ref v 2)))
        (errorf #f "wrong value for ~s" n))))
  (do ([q 10000 (fx- q 1)])
      ((fx= q 0) #t)
    (do ([n -128 (fx+ n 1)])
        ((fx= n 128) #t)
      (let ([v (make-bytevector 3 n)])
        (unless (and (eqv? (bytevector-s8-ref v 0) n)
                     (eqv? (bytevector-s8-ref v 1) n)
                     (eqv? (bytevector-s8-ref v 2) n))
          (errorf #f "wrong value for ~s" n)))))
  (do ([q 10000 (fx- q 1)])
      ((fx= q 0) #t)
    (do ([n 0 (fx+ n 1)])
        ((fx= n 255) #t)
      (let ([v (make-bytevector 3 n)])
        (unless (and (eqv? (bytevector-u8-ref v 0) n)
                     (eqv? (bytevector-u8-ref v 1) n)
                     (eqv? (bytevector-u8-ref v 2) n))
          (errorf #f "wrong value for ~s" n)))))
)

(mat bytevector
 ; invalid value
  (error? (bytevector 3 4 256))
  (error? (bytevector 3 4 -129))
  (error? (bytevector 3 4 500))
  (error? (bytevector 3 4 -500))
  (error? (bytevector 3 4 1e100))
  (error? (begin (bytevector 3 4 #e1e100) #f))

  (eqv? (bytevector) #vu8())
  (equal? (bytevector 7 7 7 7 7 7 7 7 7 7) (make-bytevector 10 7))
  (equal? (bytevector 2 2) (make-bytevector (- 4 2) (+ 1 1)))
  (eqv? (bytevector) (make-bytevector (- 4 4) (+ 1 1)))
  (eqv? (bytevector) (make-bytevector (- 4 4) (+ 1 1)))
  (equal? (bytevector 1) #vu8(1))
  (equal? (bytevector -1) #vu8(255))
  (equal? (bytevector -1 2) #vu8(255 2))
  (equal? (bytevector 2 -1) #vu8(2 255))
  (equal?
    (letrec-syntax ([z (syntax-rules ()
                         [(_) (list (bytevector))]
                         [(_ x ... y) (cons (bytevector x ... y) (z x ...))])])
      (z 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))
    '(#vu8(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
      #vu8(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
      #vu8(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
      #vu8(1 2 3 4 5 6 7 8 9 10 11 12 13 14)
      #vu8(1 2 3 4 5 6 7 8 9 10 11 12 13)
      #vu8(1 2 3 4 5 6 7 8 9 10 11 12)
      #vu8(1 2 3 4 5 6 7 8 9 10 11)
      #vu8(1 2 3 4 5 6 7 8 9 10)
      #vu8(1 2 3 4 5 6 7 8 9)
      #vu8(1 2 3 4 5 6 7 8)
      #vu8(1 2 3 4 5 6 7)
      #vu8(1 2 3 4 5 6)
      #vu8(1 2 3 4 5)
      #vu8(1 2 3 4)
      #vu8(1 2 3)
      #vu8(1 2)
      #vu8(1)
      #vu8()))
  (equal?
    (letrec-syntax ([z (syntax-rules ()
                         [(_) (list (bytevector))]
                         [(_ x ... y) (cons (bytevector x ... y) (z x ...))])])
      (z -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16 -17))
    '(#vu8(255 254 253 252 251 250 249 248 247 246 245 244 243 242 241 240 239)
      #vu8(255 254 253 252 251 250 249 248 247 246 245 244 243 242 241 240)
      #vu8(255 254 253 252 251 250 249 248 247 246 245 244 243 242 241)
      #vu8(255 254 253 252 251 250 249 248 247 246 245 244 243 242)
      #vu8(255 254 253 252 251 250 249 248 247 246 245 244 243)
      #vu8(255 254 253 252 251 250 249 248 247 246 245 244)
      #vu8(255 254 253 252 251 250 249 248 247 246 245)
      #vu8(255 254 253 252 251 250 249 248 247 246)
      #vu8(255 254 253 252 251 250 249 248 247)
      #vu8(255 254 253 252 251 250 249 248)
      #vu8(255 254 253 252 251 250 249)
      #vu8(255 254 253 252 251 250)
      #vu8(255 254 253 252 251)
      #vu8(255 254 253 252)
      #vu8(255 254 253)
      #vu8(255 254)
      #vu8(255)
      #vu8()))
  (equal?
    (let ([a 1] [c -3] [d -4] [e 5] [f 6] [h -8] [k 11] [l -12] [p -16] [q 17])
      (letrec-syntax ([z (syntax-rules ()
                           [(_) (list (bytevector))]
                           [(_ x ... y) (cons (bytevector x ... y) (z x ...))])])
        (z a 2 c d e f -7 h 9 -10 k l -13 -14 15 p q)))
    '(#vu8(1 2 253 252 5 6 249 248 9 246 11 244 243 242 15 240 17)
      #vu8(1 2 253 252 5 6 249 248 9 246 11 244 243 242 15 240)
      #vu8(1 2 253 252 5 6 249 248 9 246 11 244 243 242 15)
      #vu8(1 2 253 252 5 6 249 248 9 246 11 244 243 242)
      #vu8(1 2 253 252 5 6 249 248 9 246 11 244 243)
      #vu8(1 2 253 252 5 6 249 248 9 246 11 244)
      #vu8(1 2 253 252 5 6 249 248 9 246 11)
      #vu8(1 2 253 252 5 6 249 248 9 246)
      #vu8(1 2 253 252 5 6 249 248 9)
      #vu8(1 2 253 252 5 6 249 248)
      #vu8(1 2 253 252 5 6 249)
      #vu8(1 2 253 252 5 6)
      #vu8(1 2 253 252 5)
      #vu8(1 2 253 252)
      #vu8(1 2 253)
      #vu8(1 2)
      #vu8(1)
      #vu8()))
  (equal?
    (let ([a -1] [c 3] [d 4] [e -5] [f -6] [h 8] [k -11] [l 12] [p 16] [q -17])
      (letrec-syntax ([z (syntax-rules ()
                           [(_) (list (bytevector))]
                           [(_ x ... y) (cons (bytevector x ... y) (z x ...))])])
        (z a -2 c d e f 7 h -9 10 k l 13 14 -15 p q)))
    '(#vu8(255 254 3 4 251 250 7 8 247 10 245 12 13 14 241 16 239)
      #vu8(255 254 3 4 251 250 7 8 247 10 245 12 13 14 241 16)
      #vu8(255 254 3 4 251 250 7 8 247 10 245 12 13 14 241)
      #vu8(255 254 3 4 251 250 7 8 247 10 245 12 13 14)
      #vu8(255 254 3 4 251 250 7 8 247 10 245 12 13)
      #vu8(255 254 3 4 251 250 7 8 247 10 245 12)
      #vu8(255 254 3 4 251 250 7 8 247 10 245)
      #vu8(255 254 3 4 251 250 7 8 247 10)
      #vu8(255 254 3 4 251 250 7 8 247)
      #vu8(255 254 3 4 251 250 7 8)
      #vu8(255 254 3 4 251 250 7)
      #vu8(255 254 3 4 251 250)
      #vu8(255 254 3 4 251)
      #vu8(255 254 3 4)
      #vu8(255 254 3)
      #vu8(255 254)
      #vu8(255)
      #vu8()))
  (equal? (apply bytevector (make-list 20000 #xc7))
          (u8-list->bytevector (make-list 20000 #xc7)))
  (let ([n0 1] [n1 -2] [n4 5])
    (let ([x (bytevector n0 n1 3 -4 n4)])
      (and (bytevector? x)
           (equal? (bytevector->s8-list x) '(1 -2 3 -4 5))
           (equal? (bytevector->u8-list x) '(1 254 3 252 5))
           (eqv? (bytevector-s8-ref x 0) 1)
           (eqv? (bytevector-u8-ref x 0) 1)
           (eqv? (bytevector-s8-ref x 1) -2)
           (eqv? (bytevector-u8-ref x 1) 254)
           (eqv? (bytevector-s8-ref x 2) 3)
           (eqv? (bytevector-u8-ref x 2) 3)
           (eqv? (bytevector-s8-ref x 3) -4)
           (eqv? (bytevector-u8-ref x 3) 252)
           (eqv? (bytevector-s8-ref x 4) 5)
           (eqv? (bytevector-u8-ref x 4) 5))))
  (begin
    (define $bv-f
      (lambda (a b c d e f g h i j k l m n o p q r s t u v w x y z)
        (letrec-syntax ([foo (syntax-rules ()
                               [(_) (list (bytevector))]
                               [(_ x ... y) (cons (bytevector x ... y) (foo x ...))])])
          (foo a b c d e f g h i j k l m n o p q r s t u v w x y z))))
    #t)
  (equal?
    ($bv-f 101 -102 103 -104 -105 106 107 -108 -109 -110 111 112 113 114 -115 -116 -117 -118 119 120 121 -122 -123 124 -125 126)
    '(#vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131 126)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112)
      #vu8(101 154 103 152 151 106 107 148 147 146 111)
      #vu8(101 154 103 152 151 106 107 148 147 146)
      #vu8(101 154 103 152 151 106 107 148 147)
      #vu8(101 154 103 152 151 106 107 148)
      #vu8(101 154 103 152 151 106 107)
      #vu8(101 154 103 152 151 106)
      #vu8(101 154 103 152 151)
      #vu8(101 154 103 152)
      #vu8(101 154 103)
      #vu8(101 154)
      #vu8(101)
      #vu8()))
  (begin
    (define $bv-g
      (lambda (a c e g i k m o q s u w y)
        (letrec-syntax ([foo (syntax-rules ()
                               [(_) (list (bytevector))]
                               [(_ x ... y) (cons (bytevector x ... y) (foo x ...))])])
          (foo a -102 c -104 e 106 g -108 i -110 k 112 m 114 o -116 q -118 s 120 u -122 w 124 y 126))))
    #t)
  (equal?
    ($bv-g 101 103 -105 107 -109 111 113 -115 -117 119 121 -123 -125)
    '(#vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131 126)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112)
      #vu8(101 154 103 152 151 106 107 148 147 146 111)
      #vu8(101 154 103 152 151 106 107 148 147 146)
      #vu8(101 154 103 152 151 106 107 148 147)
      #vu8(101 154 103 152 151 106 107 148)
      #vu8(101 154 103 152 151 106 107)
      #vu8(101 154 103 152 151 106)
      #vu8(101 154 103 152 151)
      #vu8(101 154 103 152)
      #vu8(101 154 103)
      #vu8(101 154)
      #vu8(101)
      #vu8()))
  (begin
    (define $bv-h
      (lambda (b d f h j l n p r t v x z)
        (letrec-syntax ([foo (syntax-rules ()
                               [(_) (list (bytevector))]
                               [(_ x ... y) (cons (bytevector x ... y) (foo x ...))])])
          (foo 101 b 103 d -105 f 107 h -109 j 111 l 113 n -115 p -117 r 119 t 121 v -123 x -125 z))))
    #t)
  (equal?
    ($bv-h -102 -104 106 -108 -110 112 114 -116 -118 120 -122 124 126)
    '(#vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131 126)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112)
      #vu8(101 154 103 152 151 106 107 148 147 146 111)
      #vu8(101 154 103 152 151 106 107 148 147 146)
      #vu8(101 154 103 152 151 106 107 148 147)
      #vu8(101 154 103 152 151 106 107 148)
      #vu8(101 154 103 152 151 106 107)
      #vu8(101 154 103 152 151 106)
      #vu8(101 154 103 152 151)
      #vu8(101 154 103 152)
      #vu8(101 154 103)
      #vu8(101 154)
      #vu8(101)
      #vu8()))
  (begin
    (define $bv-i-ls* '())
    (define $bv-i
      (lambda (b d f h j l n p r t v x z)
        (define this)
        (define (init!) (set! $bv-i-ls* (cons '() $bv-i-ls*)) (set! this 0))
        (define (bump!) (set! this (fx+ this 1)) (set-car! $bv-i-ls* (cons this (car $bv-i-ls*))))
        (define-syntax plink (syntax-rules () [(_ x) (begin (bump!) x)]))
        (letrec-syntax ([foo (syntax-rules ()
                               [(_) (list (bytevector))]
                               [(_ x ... y) (cons (begin (init!) (bytevector (plink x) ... (plink y))) (foo x ...))])])
          (foo 101 b 103 d -105 f 107 h -109 j 111 l 113 n -115 p -117 r 119 t 121 v -123 x -125 z))))
    #t)
  (equal?
    ($bv-i -102 -104 106 -108 -110 112 114 -116 -118 120 -122 124 126)
    '(#vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131 126)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124 131)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133 124)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134 133)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121 134)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120 121)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119 120)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138 119)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139 138)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140 139)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141 140)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114 141)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113 114)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112 113)
      #vu8(101 154 103 152 151 106 107 148 147 146 111 112)
      #vu8(101 154 103 152 151 106 107 148 147 146 111)
      #vu8(101 154 103 152 151 106 107 148 147 146)
      #vu8(101 154 103 152 151 106 107 148 147)
      #vu8(101 154 103 152 151 106 107 148)
      #vu8(101 154 103 152 151 106 107)
      #vu8(101 154 103 152 151 106)
      #vu8(101 154 103 152 151)
      #vu8(101 154 103 152)
      #vu8(101 154 103)
      #vu8(101 154)
      #vu8(101)
      #vu8()))
  (equal?
    (sort (lambda (ls1 ls2) (fx<= (length ls1) (length ls2))) $bv-i-ls*)
    '((1)
      (2 1)
      (3 2 1)
      (4 3 2 1)
      (5 4 3 2 1)
      (6 5 4 3 2 1)
      (7 6 5 4 3 2 1)
      (8 7 6 5 4 3 2 1)
      (9 8 7 6 5 4 3 2 1)
      (10 9 8 7 6 5 4 3 2 1)
      (11 10 9 8 7 6 5 4 3 2 1)
      (12 11 10 9 8 7 6 5 4 3 2 1)
      (13 12 11 10 9 8 7 6 5 4 3 2 1)
      (14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
      (26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
)

(mat bytevector-syntax
  (eq? #vu8() '#vu8())
  (eq? '#0vu8() #vu8())
  (equal?
    '(#vu8(1 2 3) #3vu8(1 2 3) #6vu8(1 2 3))
    (list (bytevector 1 2 3) (bytevector 1 2 3) (bytevector 1 2 3 3 3 3)))
  (let ([x (with-input-from-string "#10vu8()" read)])
    (and (= (bytevector-length x) 10)
         (andmap fixnum? (bytevector->u8-list x))))
)

(mat bytevector-length
 ; wrong argument count
  (error? (bytevector-length))
  (error? (begin (bytevector-length #vu8() '#vu8()) #f))

 ; not a bytevector
  (error? (bytevector-length '(a b c)))
  (error? (begin (bytevector-length '(a b c)) #f))

  (eqv? (bytevector-length #vu8(3 252 5)) 3)
  (eqv? (bytevector-length '#100vu8(5 4 3)) 100)
  (eqv? (bytevector-length #vu8()) 0)
)

(mat $bytevector-ref-check?
  (let ([bv (make-bytevector 3)] [imm-bv (bytevector->immutable-bytevector (make-bytevector 3))] [not-bv (make-fxvector 3)])
    (let ([i-1 -1] [i0 0] [i1 1] [i2 2] [i3 3] [ifalse #f] [ibig (+ (most-positive-fixnum) 1)])
      (and
        (not (#%$bytevector-ref-check? 8 not-bv i0))
        (not (#%$bytevector-ref-check? 8 bv ifalse))
        (not (#%$bytevector-ref-check? 8 bv i-1))
        (not (#%$bytevector-ref-check? 8 imm-bv i-1))
        (#%$bytevector-ref-check? 8 bv 0)
        (#%$bytevector-ref-check? 8 bv 1)
        (#%$bytevector-ref-check? 8 bv 2)
        (#%$bytevector-ref-check? 8 imm-bv 0)
        (#%$bytevector-ref-check? 8 imm-bv 1)
        (#%$bytevector-ref-check? 8 imm-bv 2)
        (#%$bytevector-ref-check? 8 bv i0)
        (#%$bytevector-ref-check? 8 bv i1)
        (#%$bytevector-ref-check? 8 bv i2)
        (#%$bytevector-ref-check? 8 imm-bv i0)
        (#%$bytevector-ref-check? 8 imm-bv i1)
        (#%$bytevector-ref-check? 8 imm-bv i2)
        (not (#%$bytevector-ref-check? 8 bv 3))
        (not (#%$bytevector-ref-check? 8 bv i3))
        (not (#%$bytevector-ref-check? 8 bv ibig))
        (not (#%$bytevector-ref-check? 8 imm-bv 3))
        (not (#%$bytevector-ref-check? 8 imm-bv i3))
        (not (#%$bytevector-ref-check? 8 imm-bv ibig)))))
  (let ([n 128])
    (let ([bv (make-bytevector n)] [imm-bv (bytevector->immutable-bytevector (make-bytevector n))] [not-bv (make-fxvector n)])
      (and
        (let ([i 0])
          (and (not (#%$bytevector-ref-check? 8 not-bv i))
               (not (#%$bytevector-ref-check? 16 not-bv i))
               (not (#%$bytevector-ref-check? 32 not-bv i))
               (not (#%$bytevector-ref-check? 64 not-bv i))))
        (let f ([i -1])
          (or (fx< i -8)
              (and (not (#%$bytevector-ref-check? 8 bv i))
                   (not (#%$bytevector-ref-check? 16 bv i))
                   (not (#%$bytevector-ref-check? 32 bv i))
                   (not (#%$bytevector-ref-check? 64 bv i))
                   (not (#%$bytevector-ref-check? 8 imm-bv i))
                   (not (#%$bytevector-ref-check? 16 imm-bv i))
                   (not (#%$bytevector-ref-check? 32 imm-bv i))
                   (not (#%$bytevector-ref-check? 64 imm-bv i))
                   (f (fx* i 2)))))
        (let f ([i 0])
          (or (fx= i n)
              (and (#%$bytevector-ref-check? 8 bv i)
                   (if (and (fx= (modulo i 2) 0) (fx<= (fx+ i 2) n))
                       (and (#%$bytevector-ref-check? 16 bv i)
                            (#%$bytevector-ref-check? 16 imm-bv i))
                       (not (or (#%$bytevector-ref-check? 16 bv i)
                                (#%$bytevector-ref-check? 16 imm-bv i))))
                   (if (and (fx= (modulo i 4) 0) (fx<= (fx+ i 4) n))
                       (and (#%$bytevector-ref-check? 32 bv i)
                            (#%$bytevector-ref-check? 32 imm-bv i))
                       (not (or (#%$bytevector-ref-check? 32 bv i)
                                (#%$bytevector-ref-check? 32 imm-bv i))))
                   (if (and (fx= (modulo i 8) 0) (fx<= (fx+ i 8) n))
                       (and (#%$bytevector-ref-check? 64 bv i)
                            (#%$bytevector-ref-check? 64 imm-bv i))
                       (not (or (#%$bytevector-ref-check? 64 bv i)
                                (#%$bytevector-ref-check? 64 imm-bv i))))
                   (f (fx+ i 1)))))
        (let ([i n])
          (and (not (#%$bytevector-ref-check? 8 bv i))
               (not (#%$bytevector-ref-check? 16 bv i))
               (not (#%$bytevector-ref-check? 32 bv i))
               (not (#%$bytevector-ref-check? 64 bv i))
               (not (#%$bytevector-ref-check? 8 imm-bv i))
               (not (#%$bytevector-ref-check? 16 imm-bv i))
               (not (#%$bytevector-ref-check? 32 imm-bv i))
               (not (#%$bytevector-ref-check? 64 imm-bv i))))
        (let ([i (+ (most-positive-fixnum) 1)])
          (and (not (#%$bytevector-ref-check? 8 bv i))
               (not (#%$bytevector-ref-check? 16 bv i))
               (not (#%$bytevector-ref-check? 32 bv i))
               (not (#%$bytevector-ref-check? 64 bv i))
               (not (#%$bytevector-ref-check? 8 imm-bv i))
               (not (#%$bytevector-ref-check? 16 imm-bv i))
               (not (#%$bytevector-ref-check? 32 imm-bv i))
               (not (#%$bytevector-ref-check? 64 imm-bv i)))))))
)

(mat bytevector-s8-ref
 ; wrong argument count
  (error? (bytevector-s8-ref))
  (error? (bytevector-s8-ref #vu8(3 252 5)))
  (error? (begin (bytevector-s8-ref #vu8(3 252 5) 0 5) #f))

 ; not a bytevector
  (error? (bytevector-s8-ref '#(3 -4 5) 2))
  (error? (begin (bytevector-s8-ref '(3 -4 5) 2) #f))

 ; invalid index
  (error? (bytevector-s8-ref #vu8(3 252 5) 3))
  (error? (bytevector-s8-ref #vu8(3 252 5) -1))
  (error? (begin (bytevector-s8-ref #vu8(3 252 5) 'a) #f))

  (eqv? (bytevector-s8-ref #vu8(3 252 5) 0) 3)
  (eqv? (bytevector-s8-ref #vu8(3 252 5) 1) -4)
  (eqv? (bytevector-s8-ref #vu8(3 252 5) 2) 5)
  (do ([n -128 (fx+ n 1)])
      ((fx= n 128) #t)
    (unless (eqv? (bytevector-s8-ref (bytevector 15 n 35) 1) n)
      (errorf #f "wrong value for ~s" n)))
  (do ([n 128 (fx+ n 1)])
      ((fx= n 256) #t)
    (unless (eqv? (bytevector-s8-ref (bytevector 15 n 35) 1) (- n 256))
      (errorf #f "wrong value for ~s" n)))
)

(mat bytevector-u8-ref
 ; wrong argument count
  (error? (bytevector-u8-ref))
  (error? (bytevector-u8-ref #vu8(3 252 5)))
  (error? (begin (bytevector-u8-ref #vu8(3 252 5) 0 5) #f))

 ; not a bytevector
  (error? (bytevector-u8-ref '#(3 -4 5) 2))
  (error? (begin (bytevector-u8-ref '(3 -4 5) 2) #f))

 ; invalid index
  (error? (bytevector-u8-ref #vu8(3 252 5) 3))
  (error? (bytevector-u8-ref #vu8(3 252 5) -1))
  (error? (begin (bytevector-u8-ref #vu8(3 252 5) 'a) #f))

  (eqv? (bytevector-u8-ref #vu8(3 252 5) 0) 3)
  (eqv? (bytevector-u8-ref #vu8(3 252 5) 1) 252)
  (eqv? (bytevector-u8-ref #vu8(3 252 5) 2) 5)
  (do ([n -128 (fx+ n 1)])
      ((fx= n 0) #t)
    (unless (eqv? (bytevector-u8-ref (bytevector 15 n 35) 1) (+ 256 n))
      (errorf #f "wrong value for ~s" n)))
  (do ([n 0 (fx+ n 1)])
      ((fx= n 256) #t)
    (unless (eqv? (bytevector-u8-ref (bytevector 15 n 35) 1) n)
      (errorf #f "wrong value for ~s" n)))
)

(mat bytevector-s8-set!
  (begin
    (define $v1 (bytevector 3 4 5))
    (and (bytevector? $v1) (equal? $v1 #vu8(3 4 5))))

 ; wrong argument count
  (error? (bytevector-s8-set!))
  (error? (bytevector-s8-set! $v1))
  (error? (bytevector-s8-set! $v1 2))
  (error? (begin (bytevector-s8-set! $v1 2 3 4) #f))

 ; not a bytevector
  (error? (bytevector-s8-set! (list 3 4 5) 2 3))
  (error? (begin (bytevector-s8-set! (list 3 4 5) 2 3) #f))

 ; invalid index
  (error? (bytevector-s8-set! $v1 3 3))
  (error? (bytevector-s8-set! $v1 -1 3))
  (error? (begin (bytevector-s8-set! $v1 'a 3) #f))

 ; invalid value
  (error? (bytevector-s8-set! $v1 2 -129))
  (error? (bytevector-s8-set! $v1 2 128))
  (error? (begin (bytevector-s8-set! $v1 0 'd) #f))

 ; make sure no damage done
  (and (bytevector? $v1) (equal? $v1 #vu8(3 4 5)))

  (let ((v (bytevector 3 4 5)))
    (and (begin (bytevector-s8-set! v 0 33) (equal? v #vu8(33 4 5)))
         (begin (bytevector-s8-set! v 1 -44) (equal? v #vu8(33 212 5)))
         (begin (bytevector-s8-set! v 2 55) (equal? v #vu8(33 212 55)))))
  (let ([v (bytevector 3 4 5)])
    (do ([n -128 (fx+ n 1)])
        ((fx= n 128) #t)
      (bytevector-s8-set! v 1 n)
      (unless (and (eqv? (bytevector-s8-ref v 0) 3)
                   (eqv? (bytevector-s8-ref v 1) n)
                   (eqv? (bytevector-s8-ref v 2) 5))
        (errorf #f "wrong value for ~s" n))))
)

(mat bytevector-u8-set!
  (begin
    (define $v1 (bytevector 3 4 5))
    (and (bytevector? $v1) (equal? $v1 #vu8(3 4 5))))

 ; wrong argument count
  (error? (bytevector-u8-set!))
  (error? (bytevector-u8-set! $v1))
  (error? (bytevector-u8-set! $v1 2))
  (error? (begin (bytevector-u8-set! $v1 2 3 4) #f))

 ; not a bytevector
  (error? (bytevector-u8-set! (list 3 4 5) 2 3))
  (error? (begin (bytevector-u8-set! (list 3 4 5) 2 3) #f))

 ; invalid index
  (error? (bytevector-u8-set! $v1 3 3))
  (error? (bytevector-u8-set! $v1 -1 3))
  (error? (begin (bytevector-u8-set! $v1 'a 3) #f))

 ; invalid value
  (error? (bytevector-u8-set! $v1 2 -1))
  (error? (bytevector-u8-set! $v1 0 256))
  (error? (begin (bytevector-u8-set! $v1 0 'd) #f))

 ; make sure no damage done
  (and (bytevector? $v1) (equal? $v1 #vu8(3 4 5)))

  (let ((v (bytevector 3 4 5)))
    (and (begin (bytevector-u8-set! v 0 33) (equal? v #vu8(33 4 5)))
         (begin (bytevector-u8-set! v 1 128) (equal? v #vu8(33 128 5)))
         (begin (bytevector-u8-set! v 2 55) (equal? v #vu8(33 128 55)))))
  (let ([v (bytevector 3 4 5)])
    (do ([n 0 (fx+ n 1)])
        ((fx= n 256) #t)
      (bytevector-u8-set! v 1 n)
      (unless (and (eqv? (bytevector-u8-ref v 0) 3)
                   (eqv? (bytevector-u8-ref v 1) n)
                   (eqv? (bytevector-u8-ref v 2) 5))
        (errorf #f "wrong value for ~s" n))))
)

(module (big-endian->signed little-endian->signed native->signed
         big-endian->unsigned little-endian->unsigned native->unsigned)
  (define (signed n) (if (>= n 128) (- n 256) n))

  (define (big-endian->signed . args)
    (let f ([args (cdr args)] [a (signed (car args))])
      (if (null? args)
          a
          (f (cdr args) (logor (ash a 8) (car args))))))

  (define (little-endian->signed . args)
    (let f ([args args])
      (if (null? (cdr args))
          (signed (car args))
          (logor (ash (f (cdr args)) 8) (car args)))))

  (define (native->signed . args)
    (case (native-endianness)
      [(big) (apply big-endian->signed args)]
      [(little) (apply little-endian->signed args)]
      [else
       (errorf 'native->signed
         "unhandled endianness ~s"
         (native-endianness))]))

  (define (big-endian->unsigned . args)
    (let f ([args (cdr args)] [a (car args)])
      (if (null? args)
          a
          (f (cdr args) (logor (ash a 8) (car args))))))

  (define (little-endian->unsigned . args)
    (let f ([args args])
      (if (null? args)
          0
          (logor (ash (f (cdr args)) 8) (car args)))))

  (define (native->unsigned . args)
    (case (native-endianness)
      [(big) (apply big-endian->unsigned args)]
      [(little) (apply little-endian->unsigned args)]
      [else
       (errorf 'native->unsigned
         "unhandled endianness ~s"
         (native-endianness))])))

(mat bytevector-s16-native-ref
 ; wrong argument count
  (error? (bytevector-s16-native-ref))
  (error? (bytevector-s16-native-ref #vu8(3 252 5)))
  (error? (begin (bytevector-s16-native-ref #vu8(3 252 5) 0 0) #f))

 ; not a bytevector
  (error? (bytevector-s16-native-ref '#(3 252 5) 0))
  (error? (begin (bytevector-s16-native-ref '#(3 252 5) 0) #f))

 ; invalid index
  (error? (bytevector-s16-native-ref #vu8(3 252 5) -1))
  (error? (bytevector-s16-native-ref #vu8(3 252 5) 1))
  (error? (bytevector-s16-native-ref #vu8(3 252 5) 2))
  (error? (bytevector-s16-native-ref #vu8(3 252 5) 3))
  (error? (begin (bytevector-s16-native-ref #vu8(3 252 5) 4.0) #f))

  (eqv?
    (bytevector-s16-native-ref #vu8(3 252 5) 0)
    (native->signed 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-s16-native-ref v 0)
        (bytevector-s16-native-ref v 2)
        (bytevector-s16-native-ref v 4)
        (bytevector-s16-native-ref v i)
        (bytevector-s16-native-ref v 6)
        (bytevector-s16-native-ref v 8)))
    (list
      (native->signed 3 252)
      (native->signed 5 17)
      (native->signed 23 55)
      (native->signed 23 55)
      (native->signed 250 89)
      (native->signed 200 201)))

  (test-endian-sensitive-cp0-expansion eqv?
    '(bytevector-s16-native-ref #vu8(3 252 5) 0)
    (native->signed 3 252))
  (or
   dynamic-native-endianness?
   (equal?
    ;; list doesn't get inlined, so take if off the front of the list
    (cdr (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
           (expand/optimize '(let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
                               (list
                                 (bytevector-s16-native-ref v 0)
                                 (bytevector-s16-native-ref v 2)
                                 (bytevector-s16-native-ref v 4)
                                 (bytevector-s16-native-ref v i)
                                 (bytevector-s16-native-ref v 6)
                                 (bytevector-s16-native-ref v 8))))))
      (list
         (native->signed 3 252)
         (native->signed 5 17)
         (native->signed 23 55)
         (native->signed 23 55)
         (native->signed 250 89)
         (native->signed 200 201))))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-s16-native-ref (bytevector i j) 0)
                    (native->signed i j))
        (errorf #f "failed for ~s and ~s" i j))))
)

(mat bytevector-u16-native-ref
 ; wrong argument count
  (error? (bytevector-u16-native-ref))
  (error? (bytevector-u16-native-ref #vu8(3 252 5)))
  (error? (begin (bytevector-u16-native-ref #vu8(3 252 5) 0 0) #f))

 ; not a bytevector
  (error? (bytevector-u16-native-ref '#(3 252 5) 0))
  (error? (begin (bytevector-u16-native-ref '#(3 252 5) 0) #f))

 ; invalid index
  (error? (bytevector-u16-native-ref #vu8(3 252 5) -1))
  (error? (bytevector-u16-native-ref #vu8(3 252 5) 1))
  (error? (bytevector-u16-native-ref #vu8(3 252 5) 2))
  (error? (bytevector-u16-native-ref #vu8(3 252 5) 3))
  (error? (begin (bytevector-u16-native-ref #vu8(3 252 5) 4.0) #f))

  (eqv?
    (bytevector-u16-native-ref #vu8(3 252 5) 0)
    (native->unsigned 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-u16-native-ref v 0)
        (bytevector-u16-native-ref v 2)
        (bytevector-u16-native-ref v 4)
        (bytevector-u16-native-ref v i)
        (bytevector-u16-native-ref v 6)
        (bytevector-u16-native-ref v 8)))
    (list
      (native->unsigned 3 252)
      (native->unsigned 5 17)
      (native->unsigned 23 55)
      (native->unsigned 23 55)
      (native->unsigned 250 89)
      (native->unsigned 200 201)))

  (test-endian-sensitive-cp0-expansion eqv?
    '(bytevector-u16-native-ref #vu8(3 252 5) 0)
    (native->unsigned 3 252))
  (or
   dynamic-native-endianness?
   (equal?
    ;; list doesn't get inlined, so take if off the front of the list
    (cdr (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
           (expand/optimize '(let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
                               (list
                                 (bytevector-u16-native-ref v 0)
                                 (bytevector-u16-native-ref v 2)
                                 (bytevector-u16-native-ref v 4)
                                 (bytevector-u16-native-ref v i)
                                 (bytevector-u16-native-ref v 6)
                                 (bytevector-u16-native-ref v 8))))))
      (list
         (native->unsigned 3 252)
         (native->unsigned 5 17)
         (native->unsigned 23 55)
         (native->unsigned 23 55)
         (native->unsigned 250 89)
         (native->unsigned 200 201))))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-u16-native-ref (bytevector i j) 0)
                    (native->unsigned i j))
        (errorf #f "failed for ~s and ~s" i j))))
)

(mat bytevector-s16-native-set!
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s16-native-set!))
  (error? (bytevector-s16-native-set! $v1))
  (error? (bytevector-s16-native-set! $v1 0))
  (error? (begin (bytevector-s16-native-set! $v1 0 0 15) #f))

 ; not a bytevector
  (error? (bytevector-s16-native-set! (make-vector 10) 0 0))
  (error? (begin (bytevector-s16-native-set! (make-vector 10) 0 0) #f))

 ; invalid index
  (error? (bytevector-s16-native-set! $v1 -1 0))
  (error? (bytevector-s16-native-set! $v1 1 0))
  (error? (bytevector-s16-native-set! $v1 3 0))
  (error? (bytevector-s16-native-set! $v1 5 0))
  (error? (bytevector-s16-native-set! $v1 7 0))
  (error? (bytevector-s16-native-set! $v1 9 0))
  (error? (bytevector-s16-native-set! $v1 11 0))
  (error? (begin (bytevector-s16-native-set! $v1 'q 0) #f))

 ; invalid value
  (error? (bytevector-s16-native-set! $v1 0 #x8000))
  (error? (bytevector-s16-native-set! $v1 2 #x-8001))
  (error? (begin (bytevector-s16-native-set! $v1 4 "hello") #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad)))

  (begin
    (bytevector-s16-native-set! $v1 0 -1)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 0 (native->signed #x80 #x00))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 0 (native->signed #x00 #x80))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 0 (native->signed #x7f #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 0 (native->signed #xff #x7f))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 0 (native->signed #xff #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 0 #x0000)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 2 (native->signed #xf3 #x45))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 4 (native->signed #x23 #xc7))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 6 (native->signed #x3a #x1c))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-s16-native-set! $v1 8 (native->signed #xe3 #xd7))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-s16-native-set! v 0 (native->signed i j))
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))
)

(mat bytevector-u16-native-set!
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u16-native-set!))
  (error? (bytevector-u16-native-set! $v1))
  (error? (bytevector-u16-native-set! $v1 0))
  (error? (begin (bytevector-u16-native-set! $v1 0 0 15) #f))

 ; not a bytevector
  (error? (bytevector-u16-native-set! (make-vector 10) 0 0))
  (error? (begin (bytevector-u16-native-set! (make-vector 10) 0 0) #f))

 ; invalid index
  (error? (bytevector-u16-native-set! $v1 -1 0))
  (error? (bytevector-u16-native-set! $v1 1 0))
  (error? (bytevector-u16-native-set! $v1 3 0))
  (error? (bytevector-u16-native-set! $v1 5 0))
  (error? (bytevector-u16-native-set! $v1 7 0))
  (error? (bytevector-u16-native-set! $v1 9 0))
  (error? (bytevector-u16-native-set! $v1 11 0))
  (error? (begin (bytevector-u16-native-set! $v1 'q 0) #f))

 ; invalid value
  (error? (bytevector-u16-native-set! $v1 0 #x10000))
  (error? (bytevector-u16-native-set! $v1 2 #x-1))
  (error? (begin (bytevector-u16-native-set! $v1 4 "hello") #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad)))

  (begin
    (bytevector-u16-native-set! $v1 0 #xffff)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 0 (native->unsigned #x80 #x00))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 0 (native->unsigned #x00 #x80))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 0 (native->unsigned #x7f #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 0 (native->unsigned #xff #x7f))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 0 (native->unsigned #xff #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 0 #x0000)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 2 (native->unsigned #xf3 #x45))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 4 (native->unsigned #x23 #xc7))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 6 (native->unsigned #x3a #x1c))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-u16-native-set! $v1 8 (native->unsigned #xe3 #xd7))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-u16-native-set! v 0 (native->unsigned i j))
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))
)

(mat bytevector-s16-ref
 ; wrong argument count
  (error? (bytevector-s16-ref))
  (error? (bytevector-s16-ref #vu8(3 252 5)))
  (error? (begin (bytevector-s16-ref #vu8(3 252 5) 0 0 'big) #f))

 ; not a bytevector
  (error? (bytevector-s16-ref '#(3 252 5) 0 'big))
  (error? (begin (bytevector-s16-ref '#(3 252 5) 0 'big) #f))

 ; invalid index
  (error? (bytevector-s16-ref #vu8(3 252 5) -1 (native-endianness)))
  (error? (bytevector-s16-ref #vu8(3 252 5) 2 'big))
  (error? (bytevector-s16-ref #vu8(3 252 5) 3 'little))
  (error? (begin (bytevector-s16-ref #vu8(3 252 5) 4.0 'big) #f))

 ; invalid endianness
  (error? (bytevector-s16-ref #vu8(3 252 5) 0 'bigger))
  (error? (bytevector-s16-ref #vu8(3 252 5) 0 "little"))
  (error? (begin (bytevector-s16-ref #vu8(3 252 5) 0 #t) #f))

 ; aligned accesses, endianness native
  (eqv?
    (bytevector-s16-ref #vu8(3 252 5) 0 (native-endianness))
    (native->signed 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-s16-ref v 0 (native-endianness))
        (bytevector-s16-ref v 2 (native-endianness))
        (bytevector-s16-ref v 4 (native-endianness))
        (bytevector-s16-ref v i (native-endianness))
        (bytevector-s16-ref v 6 (native-endianness))
        (bytevector-s16-ref v 8 (native-endianness))))
    (list
      (native->signed 3 252)
      (native->signed 5 17)
      (native->signed 23 55)
      (native->signed 23 55)
      (native->signed 250 89)
      (native->signed 200 201)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-s16-ref (bytevector i j) 0 (native-endianness))
                    (native->signed i j))
        (errorf #f "failed for ~s and ~s" i j))))

 ; aligned accesses, endianness big
  (eqv?
    (bytevector-s16-ref #vu8(3 252 5) 0 'big)
    (big-endian->signed 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-s16-ref v 0 'big)
        (bytevector-s16-ref v 2 'big)
        (bytevector-s16-ref v 4 'big)
        (bytevector-s16-ref v i 'big)
        (bytevector-s16-ref v 6 'big)
        (bytevector-s16-ref v 8 'big)))
    (list
      (big-endian->signed 3 252)
      (big-endian->signed 5 17)
      (big-endian->signed 23 55)
      (big-endian->signed 23 55)
      (big-endian->signed 250 89)
      (big-endian->signed 200 201)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-s16-ref (bytevector i j) 0 'big)
                    (big-endian->signed i j))
        (errorf #f "failed for ~s and ~s" i j))))

 ; aligned accesses, endianness little
  (eqv?
    (bytevector-s16-ref #vu8(3 252 5) 0 'little)
    (little-endian->signed 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-s16-ref v 0 'little)
        (bytevector-s16-ref v 2 'little)
        (bytevector-s16-ref v 4 'little)
        (bytevector-s16-ref v i 'little)
        (bytevector-s16-ref v 6 'little)
        (bytevector-s16-ref v 8 'little)))
    (list
      (little-endian->signed 3 252)
      (little-endian->signed 5 17)
      (little-endian->signed 23 55)
      (little-endian->signed 23 55)
      (little-endian->signed 250 89)
      (little-endian->signed 200 201)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-s16-ref (bytevector i j) 0 'little)
                    (little-endian->signed i j))
        (errorf #f "failed for ~s and ~s" i j))))

 ; unaligned accesses, endianness mixed
  (eqv?
    (bytevector-s16-ref #vu8(3 252 5) 1 (native-endianness))
    (native->signed 252 5))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 5])
      (list
        (bytevector-s16-ref v 1 (native-endianness))
        (bytevector-s16-ref v 3 'little)
        (bytevector-s16-ref v 5 'big)
        (bytevector-s16-ref v i 'big)
        (bytevector-s16-ref v 7 'little)
        (bytevector-s16-ref v 9 (native-endianness))))
    (list
      (native->signed 252 5)
      (little-endian->signed 17 23)
      (big-endian->signed 55 250)
      (big-endian->signed 55 250)
      (little-endian->signed 89 200)
      (native->signed 201 128)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-s16-ref (bytevector 0 i j) 1 'little)
                    (little-endian->signed i j))
        (errorf #f "failed for ~s and ~s (little)" i j))
      (unless (eqv? (bytevector-s16-ref (bytevector 0 i j) 1 'big)
                    (big-endian->signed i j))
        (errorf #f "failed for ~s and ~s (big)" i j))
      (unless (eqv? (bytevector-s16-ref (bytevector 0 i j) 1 (native-endianness))
                    (native->signed i j))
        (errorf #f "failed for ~s and ~s (native)" i j))))
)

(mat bytevector-u16-ref
 ; wrong argument count
  (error? (bytevector-u16-ref))
  (error? (bytevector-u16-ref #vu8(3 252 5)))
  (error? (begin (bytevector-u16-ref #vu8(3 252 5) 0 0 'big) #f))

 ; not a bytevector
  (error? (bytevector-u16-ref '#(3 252 5) 0 'big))
  (error? (begin (bytevector-u16-ref '#(3 252 5) 0 'big) #f))

 ; invalid index
  (error? (bytevector-u16-ref #vu8(3 252 5) -1 (native-endianness)))
  (error? (bytevector-u16-ref #vu8(3 252 5) 2 'little))
  (error? (bytevector-u16-ref #vu8(3 252 5) 3 'big))
  (error? (begin (bytevector-u16-ref #vu8(3 252 5) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-u16-ref #vu8(3 252 5) 0 'bigger))
  (error? (bytevector-u16-ref #vu8(3 252 5) 0 "little"))
  (error? (begin (bytevector-u16-ref #vu8(3 252 5) 0 #t) #f))

 ; aligned accesses, endianness native
  (eqv?
    (bytevector-u16-ref #vu8(3 252 5) 0 (native-endianness))
    (native->unsigned 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-u16-ref v 0 (native-endianness))
        (bytevector-u16-ref v 2 (native-endianness))
        (bytevector-u16-ref v 4 (native-endianness))
        (bytevector-u16-ref v i (native-endianness))
        (bytevector-u16-ref v 6 (native-endianness))
        (bytevector-u16-ref v 8 (native-endianness))))
    (list
      (native->unsigned 3 252)
      (native->unsigned 5 17)
      (native->unsigned 23 55)
      (native->unsigned 23 55)
      (native->unsigned 250 89)
      (native->unsigned 200 201)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-u16-ref (bytevector i j) 0 (native-endianness))
                    (native->unsigned i j))
        (errorf #f "failed for ~s and ~s" i j))))

 ; aligned accesses, endianness big
  (eqv?
    (bytevector-u16-ref #vu8(3 252 5) 0 'big)
    (big-endian->unsigned 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-u16-ref v 0 'big)
        (bytevector-u16-ref v 2 'big)
        (bytevector-u16-ref v 4 'big)
        (bytevector-u16-ref v i 'big)
        (bytevector-u16-ref v 6 'big)
        (bytevector-u16-ref v 8 'big)))
    (list
      (big-endian->unsigned 3 252)
      (big-endian->unsigned 5 17)
      (big-endian->unsigned 23 55)
      (big-endian->unsigned 23 55)
      (big-endian->unsigned 250 89)
      (big-endian->unsigned 200 201)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-u16-ref (bytevector i j) 0 'big)
                    (big-endian->unsigned i j))
        (errorf #f "failed for ~s and ~s" i j))))

 ; aligned accesses, endianness little
  (eqv?
    (bytevector-u16-ref #vu8(3 252 5) 0 'little)
    (little-endian->unsigned 3 252))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 4])
      (list
        (bytevector-u16-ref v 0 'little)
        (bytevector-u16-ref v 2 'little)
        (bytevector-u16-ref v 4 'little)
        (bytevector-u16-ref v i 'little)
        (bytevector-u16-ref v 6 'little)
        (bytevector-u16-ref v 8 'little)))
    (list
      (little-endian->unsigned 3 252)
      (little-endian->unsigned 5 17)
      (little-endian->unsigned 23 55)
      (little-endian->unsigned 23 55)
      (little-endian->unsigned 250 89)
      (little-endian->unsigned 200 201)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-u16-ref (bytevector i j) 0 'little)
                    (little-endian->unsigned i j))
        (errorf #f "failed for ~s and ~s" i j))))

 ; unaligned accesses, endianness mixed
  (eqv?
    (bytevector-u16-ref #vu8(3 252 5) 1 (native-endianness))
    (native->unsigned 252 5))
  (equal?
    (let ([v #vu8(3 252 5 17 23 55 250 89 200 201 128)] [i 5])
      (list
        (bytevector-u16-ref v 1 (native-endianness))
        (bytevector-u16-ref v 3 'little)
        (bytevector-u16-ref v 5 'big)
        (bytevector-u16-ref v i 'big)
        (bytevector-u16-ref v 7 'little)
        (bytevector-u16-ref v 9 (native-endianness))))
    (list
      (native->unsigned 252 5)
      (little-endian->unsigned 17 23)
      (big-endian->unsigned 55 250)
      (big-endian->unsigned 55 250)
      (little-endian->unsigned 89 200)
      (native->unsigned 201 128)))

  (do ([i 0 (fx+ i 1)])
      ((fx= i (expt 2 8)) #t)
    (do ([j 0 (fx+ j 1)])
        ((fx= j (expt 2 8)))
      (unless (eqv? (bytevector-u16-ref (bytevector 0 i j) 1 'little)
                    (little-endian->unsigned i j))
        (errorf #f "failed for ~s and ~s (little)" i j))
      (unless (eqv? (bytevector-u16-ref (bytevector 0 i j) 1 'big)
                    (big-endian->unsigned i j))
        (errorf #f "failed for ~s and ~s (big)" i j))
      (unless (eqv? (bytevector-u16-ref (bytevector 0 i j) 1 (native-endianness))
                    (native->unsigned i j))
        (errorf #f "failed for ~s and ~s (native)" i j))))
)

(mat bytevector-s16-set!
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s16-set!))
  (error? (bytevector-s16-set! $v1))
  (error? (bytevector-s16-set! $v1 0 0))
  (error? (begin (bytevector-s16-set! $v1 0 0 0 (native-endianness)) #f))

 ; not a bytevector
  (error? (bytevector-s16-set! (make-vector 10) 0 0 'big))
  (error? (begin (bytevector-s16-set! (make-vector 10) 0 0 'big) #f))

 ; invalid index
  (error? (bytevector-s16-set! $v1 -1 0 (native-endianness)))
  (error? (bytevector-s16-set! $v1 10 0 (native-endianness)))
  (error? (bytevector-s16-set! $v1 11 0 'big))
  (error? (begin (bytevector-s16-set! $v1 'q 0 'little) #f))

 ; invalid value
  (error? (bytevector-s16-set! $v1 0 #x8000 (native-endianness)))
  (error? (bytevector-s16-set! $v1 1 #x8000 (native-endianness)))
  (error? (bytevector-s16-set! $v1 2 #x-8001 'big))
  (error? (bytevector-s16-set! $v1 3 #x-8001 'big))
  (error? (bytevector-s16-set! $v1 4 "hello" 'little))
  (error? (begin (bytevector-s16-set! $v1 5 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-s16-set! $v1 0 0 'bigger))
  (error? (bytevector-s16-set! $v1 0 0 "little"))
  (error? (begin (bytevector-s16-set! $v1 0 0 #t) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad)))

 ; aligned accesses, endianness native
  (begin
    (bytevector-s16-set! $v1 0 -1 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (native->signed #x80 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (native->signed #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (native->signed #x7f #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (native->signed #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (native->signed #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 #x0000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 2 (native->signed #xf3 #x45) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 4 (native->signed #x23 #xc7) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 6 (native->signed #x3a #x1c) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 8 (native->signed #xe3 #xd7) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-s16-set! v 0 (native->signed i j) (native-endianness))
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))

 ; aligned accesses, endianness little
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s16-set! $v1 0 -1 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (little-endian->signed #x80 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (little-endian->signed #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (little-endian->signed #x7f #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (little-endian->signed #xff #x7f) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (little-endian->signed #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 #x0000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 2 (little-endian->signed #xf3 #x45) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 4 (little-endian->signed #x23 #xc7) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 6 (little-endian->signed #x3a #x1c) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 8 (little-endian->signed #xe3 #xd7) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-s16-set! v 0 (little-endian->signed i j) 'little)
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))

 ; aligned accesses, endianness big
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s16-set! $v1 0 -1 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (big-endian->signed #x80 #x00) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (big-endian->signed #x00 #x80) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (big-endian->signed #x7f #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (big-endian->signed #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 (big-endian->signed #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 0 #x0000 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 2 (big-endian->signed #xf3 #x45) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 4 (big-endian->signed #x23 #xc7) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 6 (big-endian->signed #x3a #x1c) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 8 (big-endian->signed #xe3 #xd7) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-s16-set! v 0 (big-endian->signed i j) 'big)
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s16-set! $v1 1 -1 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xff #xff #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 1 (native->signed #x80 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 1 (little-endian->signed #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 1 (little-endian->signed #x7f #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 1 (native->signed #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 1 (big-endian->signed #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xff #xff #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 1 #x0000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 3 (big-endian->signed #xf3 #x45) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 5 (little-endian->signed #x23 #xc7) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 7 (native->signed #x3a #x1c) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad))))
  (begin
    (bytevector-s16-set! $v1 9 (big-endian->signed #xe3 #xd7) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7))))

  (let ([v (bytevector 0 #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-s16-set! v 1 (native->signed i j) (native-endianness))
        (unless (equal? v (bytevector 0 i j))
          (errorf #f "failed for ~s and ~s (native)" i j))
        (bytevector-u8-set! v 1 #xc7)
        (bytevector-u8-set! v 2 #xc7)
        (bytevector-s16-set! v 1 (big-endian->signed i j) 'big)
        (unless (equal? v (bytevector 0 i j))
          (errorf #f "failed for ~s and ~s (big)" i j))
        (bytevector-u8-set! v 1 #xc7)
        (bytevector-u8-set! v 2 #xc7)
        (bytevector-s16-set! v 1 (little-endian->signed i j) 'little)
        (unless (equal? v (bytevector 0 i j))
          (errorf #f "failed for ~s and ~s (little)" i j)))))
)

(mat bytevector-u16-set!
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u16-set!))
  (error? (bytevector-u16-set! $v1))
  (error? (bytevector-u16-set! $v1 0 0))
  (error? (begin (bytevector-u16-set! $v1 0 0 0 (native-endianness)) #f))

 ; not a bytevector
  (error? (bytevector-u16-set! (make-vector 10) 0 0 'big))
  (error? (begin (bytevector-u16-set! (make-vector 10) 0 0 'big) #f))

 ; invalid index
  (error? (bytevector-u16-set! $v1 -1 0 (native-endianness)))
  (error? (bytevector-u16-set! $v1 10 0 'big))
  (error? (bytevector-u16-set! $v1 11 0 'big))
  (error? (begin (bytevector-u16-set! $v1 'q 0 'little) #f))

 ; invalid value
  (error? (bytevector-u16-set! $v1 0 #x10000 (native-endianness)))
  (error? (bytevector-u16-set! $v1 1 #x10000 (native-endianness)))
  (error? (bytevector-u16-set! $v1 2 #x-1 'little))
  (error? (bytevector-u16-set! $v1 3 #x-1 'little))
  (error? (bytevector-u16-set! $v1 4 "hello" 'big))
  (error? (begin (bytevector-u16-set! $v1 5 "hello" 'big) #f))

 ; invalid endianness
  (error? (bytevector-u16-set! $v1 0 0 'bigger))
  (error? (bytevector-u16-set! $v1 0 0 "little"))
  (error? (begin (bytevector-u16-set! $v1 0 0 #t) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad)))

 ; aligned accesses, endianness native
  (begin
    (bytevector-u16-set! $v1 0 #xffff (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (native->unsigned #x80 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (native->unsigned #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (native->unsigned #x7f #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (native->unsigned #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (native->unsigned #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 #x0000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 2 (native->unsigned #xf3 #x45) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 4 (native->unsigned #x23 #xc7) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 6 (native->unsigned #x3a #x1c) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 8 (native->unsigned #xe3 #xd7) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-u16-set! v 0 (native->unsigned i j) (native-endianness))
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))

 ; aligned accesses, endianness little
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u16-set! $v1 0 #xffff 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (little-endian->unsigned #x80 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (little-endian->unsigned #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (little-endian->unsigned #x7f #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (little-endian->unsigned #xff #x7f) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (little-endian->unsigned #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 #x0000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 2 (little-endian->unsigned #xf3 #x45) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 4 (little-endian->unsigned #x23 #xc7) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 6 (little-endian->unsigned #x3a #x1c) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 8 (little-endian->unsigned #xe3 #xd7) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-u16-set! v 0 (little-endian->unsigned i j) 'little)
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))

 ; aligned accesses, endianness big
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u16-set! $v1 0 #xffff 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (big-endian->unsigned #x80 #x00) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (big-endian->unsigned #x00 #x80) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (big-endian->unsigned #x7f #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (big-endian->unsigned #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 (big-endian->unsigned #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xff #xff #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 0 #x0000 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xad #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 2 (big-endian->unsigned #xf3 #x45) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 4 (big-endian->unsigned #x23 #xc7) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 6 (big-endian->unsigned #x3a #x1c) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 8 (big-endian->unsigned #xe3 #xd7) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(0 0 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7 #xad))))

  (let ([v (bytevector #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-u16-set! v 0 (big-endian->unsigned i j) 'big)
        (unless (equal? v (bytevector i j))
          (errorf #f "failed for ~s and ~s" i j)))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 11 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xad #xad #xad #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u16-set! $v1 1 #xffff 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xff #xff #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 1 (native->unsigned #x80 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x80 #x00 #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 1 (little-endian->unsigned #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x80 #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 1 (little-endian->unsigned #x7f #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x7f #xff #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 1 (native->unsigned #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xff #x7f #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 1 (big-endian->unsigned #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #xff #xff #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 1 #x0000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xad #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 3 (big-endian->unsigned #xf3 #x45) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 5 (little-endian->unsigned #x23 #xc7) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #x23 #xc7 #xad #xad #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 7 (native->unsigned #x3a #x1c) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #x23 #xc7 #x3a #x1c #xad #xad))))
  (begin
    (bytevector-u16-set! $v1 9 (big-endian->unsigned #xe3 #xd7) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 #vu8(#xad #x00 #x00 #xf3 #x45 #x23 #xc7 #x3a #x1c #xe3 #xd7))))

  (let ([v (bytevector 0 #xc7 #xc7)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (expt 2 8)) #t)
      (do ([j 0 (fx+ j 1)])
          ((fx= j (expt 2 8)))
        (bytevector-u16-set! v 1 (native->unsigned i j) (native-endianness))
        (unless (equal? v (bytevector 0 i j))
          (errorf #f "failed for ~s and ~s (native)" i j))
        (bytevector-u8-set! v 1 #xc7)
        (bytevector-u8-set! v 2 #xc7)
        (bytevector-u16-set! v 1 (big-endian->unsigned i j) 'big)
        (unless (equal? v (bytevector 0 i j))
          (errorf #f "failed for ~s and ~s (big)" i j))
        (bytevector-u8-set! v 1 #xc7)
        (bytevector-u8-set! v 2 #xc7)
        (bytevector-u16-set! v 1 (little-endian->unsigned i j) 'little)
        (unless (equal? v (bytevector 0 i j))
          (errorf #f "failed for ~s and ~s (little)" i j)))))
)

(mat bytevector-s24-ref
 ; wrong argument count
  (error? (bytevector-s24-ref))
  (error? (bytevector-s24-ref #vu8(3 252 5 0)))
  (error? (bytevector-s24-ref #vu8(3 252 5 0) 0))
  (error? (begin (bytevector-s24-ref #vu8(3 252 5 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-s24-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-s24-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-s24-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-s24-ref #vu8(3 252 5 0 0 0 0) 6 'little))
  (error? (bytevector-s24-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-s24-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-s24-ref #vu8(3 252 5 0 0 0 0) 0 'bigger))
  (error? (bytevector-s24-ref #vu8(3 252 5 0 0 0 0) 0 "little"))
  (error? (begin (bytevector-s24-ref #vu8(3 252 5 0 0 0 0) 0 #t) #f))

 ; 32-bit aligned accesses, endianness native
  (eqv?
    (bytevector-s24-ref #vu8(3 252 5 32 65 87 20) 0 (native-endianness))
    (native->signed 3 252 5))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s24-ref v 0 (native-endianness))
        (bytevector-s24-ref v 4 (native-endianness))
        (bytevector-s24-ref v 8 (native-endianness))
        (bytevector-s24-ref v 12 (native-endianness))))
    (list
      (native->signed 30 100 200)
      (native->signed 249 199 99)
      (native->signed 248 189 190)
      (native->signed 24 25 26)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
      (unless (eqv? (bytevector-s24-ref (apply bytevector ls) 0 (native-endianness))
                    (apply native->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; 32-bit aligned accesses, endianness big
  (eqv?
    (bytevector-s24-ref #vu8(3 252 5 32 65 87 20) 0 'big)
    (big-endian->signed 3 252 5))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s24-ref v 0 'big)
        (bytevector-s24-ref v 4 'big)
        (bytevector-s24-ref v 8 'big)
        (bytevector-s24-ref v 12 'big)))
    (list
      (big-endian->signed 30 100 200)
      (big-endian->signed 249 199 99)
      (big-endian->signed 248 189 190)
      (big-endian->signed 24 25 26)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
      (unless (eqv? (bytevector-s24-ref (apply bytevector ls) 0 'big)
                    (apply big-endian->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; 32-bit aligned accesses, endianness little
  (eqv?
    (bytevector-s24-ref #vu8(3 252 5 32 65 87 20) 0 'little)
    (little-endian->signed 3 252 5))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s24-ref v 0 'little)
        (bytevector-s24-ref v 4 'little)
        (bytevector-s24-ref v 8 'little)
        (bytevector-s24-ref v 12 'little)))
    (list
      (little-endian->signed 30 100 200)
      (little-endian->signed 249 199 99)
      (little-endian->signed 248 189 190)
      (little-endian->signed 24 25 26)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
      (unless (eqv? (bytevector-s24-ref (apply bytevector ls) 0 'little)
                    (apply little-endian->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; not 32-bit aligned, endianness mixed
  (eqv?
    (bytevector-s24-ref #vu8(3 252 5 32 65 87 20) 3 (native-endianness))
    (native->signed 32 65 87))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s24-ref v 1 'little)
        (bytevector-s24-ref v 6 'big)
        (bytevector-s24-ref v 11 (native-endianness))
        (bytevector-s24-ref v 15 'little)))
    (list
      (little-endian->signed 100 200 250)
      (big-endian->signed 99 29 248)
      (native->signed 207 24 25)
      (little-endian->signed 27 28 29)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-s24-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-s24-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-s24-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (eval `(bytevector-s24-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-s24-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-s24-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-u24-ref
 ; wrong argument count
  (error? (bytevector-u24-ref))
  (error? (bytevector-u24-ref #vu8(3 252 5 0)))
  (error? (bytevector-u24-ref #vu8(3 252 5 0) 0))
  (error? (begin (bytevector-u24-ref #vu8(3 252 5 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-u24-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-u24-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-u24-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-u24-ref #vu8(3 252 5 0 0 0 0) 6 'little))
  (error? (bytevector-u24-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-u24-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-u24-ref #vu8(0 1 2 3 4 5 6 7) 0 'bigger))
  (error? (bytevector-u24-ref #vu8(0 1 2 3 4 5 6 7) 0 "little"))
  (error? (begin (bytevector-u24-ref #vu8(0 1 2 3 4 5 6 7) 0 #t) #f))

 ; 32-bit aligned accesses, endianness native
  (eqv?
    (bytevector-u24-ref #vu8(3 252 5 32 65 87 20) 0 (native-endianness))
    (native->unsigned 3 252 5))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u24-ref v 0 (native-endianness))
        (bytevector-u24-ref v 4 (native-endianness))
        (bytevector-u24-ref v 8 (native-endianness))
        (bytevector-u24-ref v 12 (native-endianness))))
    (list
      (native->unsigned 30 100 200)
      (native->unsigned 249 199 99)
      (native->unsigned 248 189 190)
      (native->unsigned 24 25 26)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
      (unless (eqv? (bytevector-u24-ref (apply bytevector ls) 0 (native-endianness))
                    (apply native->unsigned ls))
        (errorf #f "failed for ~s" ls))))

 ; 32-bit aligned accesses, endianness big
  (eqv?
    (bytevector-u24-ref #vu8(3 252 5 32 65 87 20) 0 'big)
    (big-endian->unsigned 3 252 5))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u24-ref v 0 'big)
        (bytevector-u24-ref v 4 'big)
        (bytevector-u24-ref v 8 'big)
        (bytevector-u24-ref v 12 'big)))
    (list
      (big-endian->unsigned 30 100 200)
      (big-endian->unsigned 249 199 99)
      (big-endian->unsigned 248 189 190)
      (big-endian->unsigned 24 25 26)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
      (unless (eqv? (bytevector-u24-ref (apply bytevector ls) 0 'big)
                    (apply big-endian->unsigned ls))
        (errorf #f "failed for ~s" ls))))

 ; 32-bit aligned accesses, endianness little
  (eqv?
    (bytevector-u24-ref #vu8(3 252 5 32 65 87 20) 0 'little)
    (little-endian->unsigned 3 252 5))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u24-ref v 0 'little)
        (bytevector-u24-ref v 4 'little)
        (bytevector-u24-ref v 8 'little)
        (bytevector-u24-ref v 12 'little)))
    (list
      (little-endian->unsigned 30 100 200)
      (little-endian->unsigned 249 199 99)
      (little-endian->unsigned 248 189 190)
      (little-endian->unsigned 24 25 26)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
      (unless (eqv? (bytevector-u24-ref (apply bytevector ls) 0 'little)
                    (apply little-endian->unsigned ls))
        (errorf #f "failed for ~s" ls))))

 ; not 32-bit aligned accesses, endianness mixed
  (eqv?
    (bytevector-u24-ref #vu8(3 252 5 32 65 87 20) 3 (native-endianness))
    (native->unsigned 32 65 87))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29)])
      (list
        (bytevector-u24-ref v 1 'little)
        (bytevector-u24-ref v 6 'big)
        (bytevector-u24-ref v 11 (native-endianness))
        (bytevector-u24-ref v 15 'little)))
    (list
      (little-endian->unsigned 100 200 250)
      (big-endian->unsigned 99 29 248)
      (native->unsigned 207 24 25)
      (little-endian->unsigned 27 28 29)))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-u24-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-u24-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-u24-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (eval `(bytevector-u24-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-u24-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-u24-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  ;; catch live-pointer mask issue
  (or (not #%$compiler-is-loaded?)
      (#%$suppress-primitive-inlining)
      (< (optimize-level) 3)
      (let ()
        (define-syntax (test-width x)
          (syntax-case x ()
            [(_ width) #'(test-width width 3)]
            [(_ width prim-optimize-level)
             (memq (datum width) '(24 32 40 48 56 64))
             (with-syntax ([smash! (datum->syntax #'top
                                     (string->symbol
                                      (format "bytevector-u~s-set!" (datum width))))])
               #'(let ()
                   (define bait (cons 0 0))
                   (define addr (#%$object-address bait 0))
                   (define type-mask (- (ftype-sizeof uptr) 1))
                   (define wrong-tag-bits
                     (logand type-mask
                             (#%$object-address values 0)))
                   ;; try to poison non-closure
                   (set-car! bait bait)
                   (set-cdr! bait bait)
                   (let ([expected (make-bytevector 9 0)]
                         [s1 "#b001"]
                         ;; masquerade pair as a closure
                         [s2 (format "#b~b"
                               (logand (- (ash 1 (datum width)) 1)
                                       (bitwise-copy-bit-field addr 0 (integer-length type-mask)
                                         wrong-tag-bits)))])
                     (define (get-index i) (collect) (read (open-input-string s1)))
                     (define (get-value i) (collect) (read (open-input-string s2)))
                     (smash! expected (get-index 0) (get-value 0) 'big)
                     (do ([i 0 (fx+ i 1)]) ((fx= i 1000))
                       (let ([actual (make-bytevector 9 0)])
                         (($primitive prim-optimize-level smash!)
                          actual (get-index i) (get-value i) 'big)
                         (unless (equal? actual expected)
                           (raise `(mismatch: expected ,expected actual ,actual))))))
                   #t))]))
        (test-width 32)
        (test-width 64)))

)

(mat bytevector-s24-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s24-set!))
  (error? (bytevector-s24-set! $v1))
  (error? (bytevector-s24-set! $v1 0))
  (error? (bytevector-s24-set! $v1 0 0))
  (error? (begin (bytevector-s24-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-s24-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-s24-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-s24-set! $v1 -1 0 'big))
  (error? (bytevector-s24-set! $v1 21 0 (native-endianness)))
  (error? (bytevector-s24-set! $v1 22 0 'little))
  (error? (bytevector-s24-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-s24-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-s24-set! $v1 0 (expt 2 23) 'big))
  (error? (bytevector-s24-set! $v1 4 (- -1 (expt 2 23)) (native-endianness)))
  (error? (begin (bytevector-s24-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-s24-set! $v1 0 0 'huge))
  (error? (bytevector-s24-set! $v1 4 0 "tiny"))
  (error? (begin (bytevector-s24-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

 ; 32-bit aligned accesses, endianness native
  (begin
    (bytevector-s24-set! $v1 0 -1 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (native->signed #x80 #x00 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (native->signed #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x80 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (native->signed #x7f #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (native->signed #xff #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #x7f #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (native->signed #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 #x000000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 4 (native->signed #xf3 #x45 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 8 (native->signed #x23 #xc7 #xe8) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 12 (native->signed #x3a #x1c #x59) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 16 (native->signed #xe3 #xd7 #xa9) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xe3 #xd7 #xa9 #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-s24-set! v 0 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; 32-bit aligned accesses, endianness big
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s24-set! $v1 0 -1 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (big-endian->signed #x80 #x00 #x00) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (big-endian->signed #x00 #x00 #x80) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x80 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (big-endian->signed #x7f #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (big-endian->signed #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #x7f #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (big-endian->signed #xff #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 #x000000 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 4 (big-endian->signed #xf3 #x45 #x19) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 8 (big-endian->signed #x23 #xc7 #xe8) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 12 (big-endian->signed #x3a #x1c #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 16 (big-endian->signed #xe3 #xd7 #xa9) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xe3 #xd7 #xa9 #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-s24-set! v 0 (apply big-endian->signed ls) 'big)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; 32-bit aligned accesses, endianness little
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s24-set! $v1 0 -1 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (little-endian->signed #x80 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (little-endian->signed #x00 #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x80 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (little-endian->signed #x7f #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (little-endian->signed #xff #xff #x7f) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #x7f #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 (little-endian->signed #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 0 #x000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 4 (little-endian->signed #xf3 #x45 #x19) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 8 (little-endian->signed #x23 #xc7 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 12 (little-endian->signed #x3a #x1c #x59) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 16 (little-endian->signed #xe3 #xd7 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xe3 #xd7 #xa9 #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-s24-set! v 0 (apply little-endian->signed ls) 'little)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; not 32-bit aligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s24-set! $v1 1 -1 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 1 (little-endian->signed #x80 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x80 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 1 (native->signed #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 1 (native->signed #x7f #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x7f #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 1 (big-endian->signed #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 1 (little-endian->signed #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 1 #x000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 5 (native->signed #xf3 #x45 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 10 (little-endian->signed #x23 #xc7 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #x23 #xc7 #xe8 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 15 (big-endian->signed #x3a #x1c #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #x23 #xc7 #xe8 #xad #xad #x3a
                        #x1c #x59 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s24-set! $v1 20 (little-endian->signed #xe3 #xd7 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #x23 #xc7 #xe8 #xad #xad #x3a
                        #x1c #x59 #xad #xad #xe3 #xd7 #xa9))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-s24-set! v 1 (apply big-endian->signed ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-s24-set! v 1 (apply little-endian->signed (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-s24-set! v 1 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (eval `(bytevector-s24-set! ,v 1 ,(apply big-endian->signed ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s24-set! ,v 1 ,(apply little-endian->signed (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s24-set! ,v 1 ,(apply native->signed ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-u24-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u24-set!))
  (error? (bytevector-u24-set! $v1))
  (error? (bytevector-u24-set! $v1 0))
  (error? (bytevector-u24-set! $v1 0 0))
  (error? (if (bytevector-u24-set! $v1 0 0 'big 0) #f #t))

 ; not a bytevector
  (error? (bytevector-u24-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (if (bytevector-u24-set! (make-vector 10) 0 0 (native-endianness)) #f #t))

 ; invalid index
  (error? (bytevector-u24-set! $v1 -1 0 'big))
  (error? (bytevector-u24-set! $v1 21 0 (native-endianness)))
  (error? (bytevector-u24-set! $v1 22 0 'little))
  (error? (bytevector-u24-set! $v1 23 0 (native-endianness)))
  (error? (if (bytevector-u24-set! $v1 'q 0 'big) #f #t))

 ; invalid value
  (error? (bytevector-u24-set! $v1 0 (expt 2 24) 'big))
  (error? (bytevector-u24-set! $v1 4 #x-1 (native-endianness)))
  (error? (if (bytevector-u24-set! $v1 8 "hello" 'little) #f #t))

 ; invalid endianness
  (error? (bytevector-u24-set! $v1 0 0 'huge))
  (error? (bytevector-u24-set! $v1 4 0 "tiny"))
  (error? (if (bytevector-u24-set! $v1 8 0 $v1) #f #t))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

 ; 32-bit aligned accesses, endianness native
  (begin
    (bytevector-u24-set! $v1 0 #xffffff (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (native->unsigned #x80 #x00 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (native->unsigned #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x80 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (native->unsigned #x7f #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (native->unsigned #xff #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #x7f #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (native->unsigned #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 #x000000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 4 (native->unsigned #xf3 #x45 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 8 (native->unsigned #x23 #xc7 #xe8) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 12 (native->unsigned #x3a #x1c #x59) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 16 (native->unsigned #xe3 #xd7 #xa9) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xe3 #xd7 #xa9 #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-u24-set! v 0 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; 32-bit aligned accesses, endianness big
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u24-set! $v1 0 #xffffff 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (big-endian->unsigned #x80 #x00 #x00) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (big-endian->unsigned #x00 #x00 #x80) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x80 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (big-endian->unsigned #x7f #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (big-endian->unsigned #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #x7f #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (big-endian->unsigned #xff #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 #x000000 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 4 (big-endian->unsigned #xf3 #x45 #x19) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 8 (big-endian->unsigned #x23 #xc7 #xe8) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 12 (big-endian->unsigned #x3a #x1c #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 16 (big-endian->unsigned #xe3 #xd7 #xa9) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xe3 #xd7 #xa9 #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-u24-set! v 0 (apply big-endian->unsigned ls) 'big)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; 32-bit aligned accesses, endianness little
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u24-set! $v1 0 #xffffff 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (little-endian->unsigned #x80 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (little-endian->unsigned #x00 #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x80 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (little-endian->unsigned #x7f #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (little-endian->unsigned #xff #xff #x7f) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #x7f #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 (little-endian->unsigned #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 0 #x000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 4 (little-endian->unsigned #xf3 #x45 #x19) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 8 (little-endian->unsigned #x23 #xc7 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 12 (little-endian->unsigned #x3a #x1c #x59) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 16 (little-endian->unsigned #xe3 #xd7 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #xad #xf3 #x45 #x19 #xad
                        #x23 #xc7 #xe8 #xad #x3a #x1c #x59 #xad
                        #xe3 #xd7 #xa9 #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-u24-set! v 0 (apply little-endian->unsigned ls) 'little)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; not 32-bit aligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u24-set! $v1 1 #xffffff 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 1 (little-endian->unsigned #x80 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x80 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 1 (native->unsigned #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 1 (native->unsigned #x7f #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x7f #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 1 (big-endian->unsigned #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 1 (little-endian->unsigned #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 1 #x000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 5 (native->unsigned #xf3 #x45 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 10 (little-endian->unsigned #x23 #xc7 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #x23 #xc7 #xe8 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 15 (big-endian->unsigned #x3a #x1c #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #x23 #xc7 #xe8 #xad #xad #x3a
                        #x1c #x59 #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u24-set! $v1 20 (little-endian->unsigned #xe3 #xd7 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #xad #xf3 #x45 #x19
                        #xad #xad #x23 #xc7 #xe8 #xad #xad #x3a
                        #x1c #x59 #xad #xad #xe3 #xd7 #xa9))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (bytevector-u24-set! v 1 (apply big-endian->unsigned ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-u24-set! v 1 (apply little-endian->unsigned (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-u24-set! v 1 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 3))])
        (eval `(bytevector-u24-set! ,v 1 ,(apply big-endian->unsigned ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u24-set! ,v 1 ,(apply little-endian->unsigned (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u24-set! ,v 1 ,(apply native->unsigned ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-s32-native-ref
 ; wrong argument count
  (error? (bytevector-s32-native-ref))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0)))
  (error? (begin (bytevector-s32-native-ref #vu8(3 252 5 0) 0 0) #f))

 ; not a bytevector
  (error? (bytevector-s32-native-ref '#(3 252 5 0 0 0 0) 0))
  (error? (begin (bytevector-s32-native-ref '#(3 252 5 0 0 0 0) 0) #f))

 ; invalid index
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) -1))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 1))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 2))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 3))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 4))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 5))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 6))
  (error? (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 7))
  (error? (begin (bytevector-s32-native-ref #vu8(3 252 5 0 0 0 0) 4.0) #f))

  (eqv?
    (bytevector-s32-native-ref #vu8(3 252 5 32 65 87 20) 0)
    (native->signed 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s32-native-ref v 0)
        (bytevector-s32-native-ref v 4)
        (bytevector-s32-native-ref v 8)
        (bytevector-s32-native-ref v 12)))
    (list
      (native->signed 30 100 200 250)
      (native->signed 249 199 99 29)
      (native->signed 248 189 190 207)
      (native->signed 24 25 26 27)))

  (test-endian-sensitive-cp0-expansion eqv?
    '(bytevector-s32-native-ref #vu8(3 252 5 32 65 87 20) 0)
    (native->signed 3 252 5 32))
  (or
   dynamic-native-endianness?
   (equal?
    (cdr (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
           (expand/optimize '(let ([v '#vu8(30 100 200 250
                                             249 199 99 29
                                             248 189 190 207
                                             24 25 26 27
                                             28 29 30)])
                               (list
                                 (bytevector-s32-native-ref v 0)
                                 (bytevector-s32-native-ref v 4)
                                 (bytevector-s32-native-ref v 8)
                                 (bytevector-s32-native-ref v 12))))))
    (list
      (native->signed 30 100 200 250)
      (native->signed 249 199 99 29)
      (native->signed 248 189 190 207)
      (native->signed 24 25 26 27))))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-s32-native-ref (apply bytevector ls) 0)
                    (apply native->signed ls))
        (errorf #f "failed for ~s" ls))))
)

(mat bytevector-u32-native-ref
 ; wrong argument count
  (error? (bytevector-u32-native-ref))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0)))
  (error? (begin (bytevector-u32-native-ref #vu8(3 252 5 0) 0 0) #f))

 ; not a bytevector
  (error? (bytevector-u32-native-ref '#(3 252 5 0 0 0 0) 0))
  (error? (begin (bytevector-u32-native-ref '#(3 252 5 0 0 0 0) 0) #f))

 ; invalid index
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) -1))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 1))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 2))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 3))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 4))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 5))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 6))
  (error? (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 7))
  (error? (begin (bytevector-u32-native-ref #vu8(3 252 5 0 0 0 0) 4.0) #f))

  (eqv?
    (bytevector-u32-native-ref #vu8(3 252 5 32 65 87 20) 0)
    (native->unsigned 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u32-native-ref v 0)
        (bytevector-u32-native-ref v 4)
        (bytevector-u32-native-ref v 8)
        (bytevector-u32-native-ref v 12)))
    (list
      (native->unsigned 30 100 200 250)
      (native->unsigned 249 199 99 29)
      (native->unsigned 248 189 190 207)
      (native->unsigned 24 25 26 27)))

  (test-endian-sensitive-cp0-expansion eqv?
    '(bytevector-u32-native-ref #vu8(3 252 5 32 65 87 20) 0)
    (native->unsigned 3 252 5 32))
  (or
   dynamic-native-endianness?
   (equal?
    (cdr (parameterize ([optimize-level 2] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
           (expand/optimize '(let ([v '#vu8(30 100 200 250
                                             249 199 99 29
                                             248 189 190 207
                                             24 25 26 27
                                             28 29 30)])
                               (list
                                 (bytevector-u32-native-ref v 0)
                                 (bytevector-u32-native-ref v 4)
                                 (bytevector-u32-native-ref v 8)
                                 (bytevector-u32-native-ref v 12))))))
    (list
      (native->unsigned 30 100 200 250)
      (native->unsigned 249 199 99 29)
      (native->unsigned 248 189 190 207)
      (native->unsigned 24 25 26 27))))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-u32-native-ref (apply bytevector ls) 0)
                    (apply native->unsigned ls))
        (errorf #f "failed for ~s" ls))))
)

(mat bytevector-s32-native-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s32-native-set!))
  (error? (bytevector-s32-native-set! $v1))
  (error? (bytevector-s32-native-set! $v1 0))
  (error? (begin (bytevector-s32-native-set! $v1 0 0 15) #f))

 ; not a bytevector
  (error? (bytevector-s32-native-set! (make-vector 10) 0 0))
  (error? (begin (bytevector-s32-native-set! (make-vector 10) 0 0) #f))

 ; invalid index
  (error? (bytevector-s32-native-set! $v1 -1 0))
  (error? (bytevector-s32-native-set! $v1 1 0))
  (error? (bytevector-s32-native-set! $v1 2 0))
  (error? (bytevector-s32-native-set! $v1 3 0))
  (error? (bytevector-s32-native-set! $v1 5 0))
  (error? (bytevector-s32-native-set! $v1 6 0))
  (error? (bytevector-s32-native-set! $v1 7 0))
  (error? (bytevector-s32-native-set! $v1 9 0))
  (error? (bytevector-s32-native-set! $v1 10 0))
  (error? (bytevector-s32-native-set! $v1 11 0))
  (error? (bytevector-s32-native-set! $v1 13 0))
  (error? (bytevector-s32-native-set! $v1 14 0))
  (error? (bytevector-s32-native-set! $v1 15 0))
  (error? (bytevector-s32-native-set! $v1 17 0))
  (error? (bytevector-s32-native-set! $v1 18 0))
  (error? (bytevector-s32-native-set! $v1 19 0))
  (error? (bytevector-s32-native-set! $v1 20 0))
  (error? (bytevector-s32-native-set! $v1 21 0))
  (error? (bytevector-s32-native-set! $v1 22 0))
  (error? (bytevector-s32-native-set! $v1 23 0))
  (error? (begin (bytevector-s32-native-set! $v1 'q 0) #f))

 ; invalid value
  (error? (bytevector-s32-native-set! $v1 0 #x80000000))
  (error? (bytevector-s32-native-set! $v1 4 #x-80000001))
  (error? (begin (bytevector-s32-native-set! $v1 8 "hello") #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (begin
    (bytevector-s32-native-set! $v1 0 -1)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 0 (native->signed #x80 #x00 #x00 #x00))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 0 (native->signed #x00 #x00 #x00 #x80))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 0 (native->signed #x7f #xff #xff #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 0 (native->signed #xff #xff #xff #x7f))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 0 (native->signed #xff #xff #xff #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 0 #x00000000)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 4 (native->signed #xf3 #x45 #x23 #x19))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 8 (native->signed #x23 #xc7 #x72 #xe8))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 12 (native->signed #x3a #x1c #x22 #x59))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-native-set! $v1 16 (native->signed #xe3 #xd7 #xc2 #xa9))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-s32-native-set! v 0 (apply native->signed ls))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-u32-native-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u32-native-set!))
  (error? (bytevector-u32-native-set! $v1))
  (error? (bytevector-u32-native-set! $v1 0))
  (error? (begin (bytevector-u32-native-set! $v1 0 0 15) #f))

 ; not a bytevector
  (error? (bytevector-u32-native-set! (make-vector 10) 0 0))
  (error? (begin (bytevector-u32-native-set! (make-vector 10) 0 0) #f))

 ; invalid index
  (error? (bytevector-u32-native-set! $v1 -1 0))
  (error? (bytevector-u32-native-set! $v1 1 0))
  (error? (bytevector-u32-native-set! $v1 2 0))
  (error? (bytevector-u32-native-set! $v1 3 0))
  (error? (bytevector-u32-native-set! $v1 5 0))
  (error? (bytevector-u32-native-set! $v1 6 0))
  (error? (bytevector-u32-native-set! $v1 7 0))
  (error? (bytevector-u32-native-set! $v1 9 0))
  (error? (bytevector-u32-native-set! $v1 10 0))
  (error? (bytevector-u32-native-set! $v1 11 0))
  (error? (bytevector-u32-native-set! $v1 13 0))
  (error? (bytevector-u32-native-set! $v1 14 0))
  (error? (bytevector-u32-native-set! $v1 15 0))
  (error? (bytevector-u32-native-set! $v1 17 0))
  (error? (bytevector-u32-native-set! $v1 18 0))
  (error? (bytevector-u32-native-set! $v1 19 0))
  (error? (bytevector-u32-native-set! $v1 20 0))
  (error? (bytevector-u32-native-set! $v1 21 0))
  (error? (bytevector-u32-native-set! $v1 22 0))
  (error? (bytevector-u32-native-set! $v1 23 0))
  (error? (begin (bytevector-u32-native-set! $v1 'q 0) #f))

 ; invalid value
  (error? (bytevector-u32-native-set! $v1 0 #x100000000))
  (error? (bytevector-u32-native-set! $v1 4 #x-1))
  (error? (begin (bytevector-u32-native-set! $v1 8 "hello") #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (begin
    (bytevector-u32-native-set! $v1 0 #xffffffff)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 0 (native->unsigned #x80 #x00 #x00 #x00))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 0 (native->unsigned #x00 #x00 #x00 #x80))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 0 (native->unsigned #x7f #xff #xff #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 0 (native->unsigned #xff #xff #xff #x7f))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 0 (native->unsigned #xff #xff #xff #xff))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 0 #x00000000)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 4 (native->unsigned #xf3 #x45 #x23 #x19))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 8 (native->unsigned #x23 #xc7 #x72 #xe8))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 12 (native->unsigned #x3a #x1c #x22 #x59))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-native-set! $v1 16 (native->unsigned #xe3 #xd7 #xc2 #xa9))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-u32-native-set! v 0 (apply native->unsigned ls))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-s32-ref
 ; wrong argument count
  (error? (bytevector-s32-ref))
  (error? (bytevector-s32-ref #vu8(3 252 5 0)))
  (error? (bytevector-s32-ref #vu8(3 252 5 0) 0))
  (error? (begin (bytevector-s32-ref #vu8(3 252 5 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-s32-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-s32-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-s32-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-s32-ref #vu8(3 252 5 0 0 0 0) 6 'little))
  (error? (bytevector-s32-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-s32-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-s32-ref $v1 0 'bigger))
  (error? (bytevector-s32-ref $v1 0 "little"))
  (error? (begin (bytevector-s32-ref $v1 0 #t) #f))

 ; aligned accesses, endianness native
  (eqv?
    (bytevector-s32-ref #vu8(3 252 5 32 65 87 20) 0 (native-endianness))
    (native->signed 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s32-ref v 0 (native-endianness))
        (bytevector-s32-ref v 4 (native-endianness))
        (bytevector-s32-ref v 8 (native-endianness))
        (bytevector-s32-ref v 12 (native-endianness))))
    (list
      (native->signed 30 100 200 250)
      (native->signed 249 199 99 29)
      (native->signed 248 189 190 207)
      (native->signed 24 25 26 27)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-s32-ref (apply bytevector ls) 0 (native-endianness))
                    (apply native->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; aligned accesses, endianness big
  (eqv?
    (bytevector-s32-ref #vu8(3 252 5 32 65 87 20) 0 'big)
    (big-endian->signed 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s32-ref v 0 'big)
        (bytevector-s32-ref v 4 'big)
        (bytevector-s32-ref v 8 'big)
        (bytevector-s32-ref v 12 'big)))
    (list
      (big-endian->signed 30 100 200 250)
      (big-endian->signed 249 199 99 29)
      (big-endian->signed 248 189 190 207)
      (big-endian->signed 24 25 26 27)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-s32-ref (apply bytevector ls) 0 'big)
                    (apply big-endian->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; aligned accesses, endianness little
  (eqv?
    (bytevector-s32-ref #vu8(3 252 5 32 65 87 20) 0 'little)
    (little-endian->signed 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s32-ref v 0 'little)
        (bytevector-s32-ref v 4 'little)
        (bytevector-s32-ref v 8 'little)
        (bytevector-s32-ref v 12 'little)))
    (list
      (little-endian->signed 30 100 200 250)
      (little-endian->signed 249 199 99 29)
      (little-endian->signed 248 189 190 207)
      (little-endian->signed 24 25 26 27)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-s32-ref (apply bytevector ls) 0 'little)
                    (apply little-endian->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; unaligned accesses, endianness mixed
  (eqv?
    (bytevector-s32-ref #vu8(3 252 5 32 65 87 20) 3 (native-endianness))
    (native->signed 32 65 87 20))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-s32-ref v 1 'little)
        (bytevector-s32-ref v 6 'big)
        (bytevector-s32-ref v 11 (native-endianness))
        (bytevector-s32-ref v 15 'little)))
    (list
      (little-endian->signed 100 200 250 249)
      (big-endian->signed 99 29 248 189)
      (native->signed 207 24 25 26)
      (little-endian->signed 27 28 29 30)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 5))])
      (unless (eqv? (bytevector-s32-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-s32-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-s32-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-u32-ref
 ; wrong argument count
  (error? (bytevector-u32-ref))
  (error? (bytevector-u32-ref #vu8(3 252 5 0)))
  (error? (bytevector-u32-ref #vu8(3 252 5 0) 0))
  (error? (begin (bytevector-u32-ref #vu8(3 252 5 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-u32-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-u32-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-u32-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-u32-ref #vu8(3 252 5 0 0 0 0) 6 'little))
  (error? (bytevector-u32-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-u32-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-u32-ref $v1 0 'bigger))
  (error? (bytevector-u32-ref $v1 0 "little"))
  (error? (begin (bytevector-u32-ref $v1 0 #t) #f))

 ; aligned accesses, endianness native
  (eqv?
    (bytevector-u32-ref #vu8(3 252 5 32 65 87 20) 0 (native-endianness))
    (native->unsigned 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u32-ref v 0 (native-endianness))
        (bytevector-u32-ref v 4 (native-endianness))
        (bytevector-u32-ref v 8 (native-endianness))
        (bytevector-u32-ref v 12 (native-endianness))))
    (list
      (native->unsigned 30 100 200 250)
      (native->unsigned 249 199 99 29)
      (native->unsigned 248 189 190 207)
      (native->unsigned 24 25 26 27)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-u32-ref (apply bytevector ls) 0 (native-endianness))
                    (apply native->unsigned ls))
        (errorf #f "failed for ~s" ls))))

 ; aligned accesses, endianness big
  (eqv?
    (bytevector-u32-ref #vu8(3 252 5 32 65 87 20) 0 'big)
    (big-endian->unsigned 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u32-ref v 0 'big)
        (bytevector-u32-ref v 4 'big)
        (bytevector-u32-ref v 8 'big)
        (bytevector-u32-ref v 12 'big)))
    (list
      (big-endian->unsigned 30 100 200 250)
      (big-endian->unsigned 249 199 99 29)
      (big-endian->unsigned 248 189 190 207)
      (big-endian->unsigned 24 25 26 27)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-u32-ref (apply bytevector ls) 0 'big)
                    (apply big-endian->unsigned ls))
        (errorf #f "failed for ~s" ls))))

 ; aligned accesses, endianness little
  (eqv?
    (bytevector-u32-ref #vu8(3 252 5 32 65 87 20) 0 'little)
    (little-endian->unsigned 3 252 5 32))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u32-ref v 0 'little)
        (bytevector-u32-ref v 4 'little)
        (bytevector-u32-ref v 8 'little)
        (bytevector-u32-ref v 12 'little)))
    (list
      (little-endian->unsigned 30 100 200 250)
      (little-endian->unsigned 249 199 99 29)
      (little-endian->unsigned 248 189 190 207)
      (little-endian->unsigned 24 25 26 27)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
      (unless (eqv? (bytevector-u32-ref (apply bytevector ls) 0 'little)
                    (apply little-endian->unsigned ls))
        (errorf #f "failed for ~s" ls))))

 ; unaligned accesses, endianness mixed
  (eqv?
    (bytevector-u32-ref #vu8(3 252 5 32 65 87 20) 3 (native-endianness))
    (native->unsigned 32 65 87 20))
  (equal?
    (let ([v '#vu8(30 100 200 250
                   249 199 99 29
                   248 189 190 207
                   24 25 26 27
                   28 29 30)])
      (list
        (bytevector-u32-ref v 1 'little)
        (bytevector-u32-ref v 6 'big)
        (bytevector-u32-ref v 11 (native-endianness))
        (bytevector-u32-ref v 15 'little)))
    (list
      (little-endian->unsigned 100 200 250 249)
      (big-endian->unsigned 99 29 248 189)
      (native->unsigned 207 24 25 26)
      (little-endian->unsigned 27 28 29 30)))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 5))])
      (unless (eqv? (bytevector-u32-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-u32-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-u32-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-s32-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s32-set!))
  (error? (bytevector-s32-set! $v1))
  (error? (bytevector-s32-set! $v1 0))
  (error? (bytevector-s32-set! $v1 0 0))
  (error? (begin (bytevector-s32-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-s32-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-s32-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-s32-set! $v1 -1 0 'big))
  (error? (bytevector-s32-set! $v1 20 0 'little))
  (error? (bytevector-s32-set! $v1 21 0 (native-endianness)))
  (error? (bytevector-s32-set! $v1 22 0 'little))
  (error? (bytevector-s32-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-s32-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-s32-set! $v1 0 #x80000000 'big))
  (error? (bytevector-s32-set! $v1 4 #x-80000001 (native-endianness)))
  (error? (begin (bytevector-s32-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-s32-set! $v1 0 #x7ffffff 'huge))
  (error? (bytevector-s32-set! $v1 4 #x-80000000 "tiny"))
  (error? (begin (bytevector-s32-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

 ; aligned accesses, endianness native
  (begin
    (bytevector-s32-set! $v1 0 -1 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (native->signed #x80 #x00 #x00 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (native->signed #x00 #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (native->signed #x7f #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (native->signed #xff #xff #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (native->signed #xff #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 #x00000000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 4 (native->signed #xf3 #x45 #x23 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 8 (native->signed #x23 #xc7 #x72 #xe8) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 12 (native->signed #x3a #x1c #x22 #x59) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 16 (native->signed #xe3 #xd7 #xc2 #xa9) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-s32-set! v 0 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; aligned accesses, endianness big
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s32-set! $v1 0 -1 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (big-endian->signed #x80 #x00 #x00 #x00) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (big-endian->signed #x00 #x00 #x00 #x80) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (big-endian->signed #x7f #xff #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (big-endian->signed #xff #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (big-endian->signed #xff #xff #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 #x00000000 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 4 (big-endian->signed #xf3 #x45 #x23 #x19) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 8 (big-endian->signed #x23 #xc7 #x72 #xe8) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 12 (big-endian->signed #x3a #x1c #x22 #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 16 (big-endian->signed #xe3 #xd7 #xc2 #xa9) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-s32-set! v 0 (apply big-endian->signed ls) 'big)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; aligned accesses, endianness little
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s32-set! $v1 0 -1 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (little-endian->signed #x80 #x00 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (little-endian->signed #x00 #x00 #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (little-endian->signed #x7f #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (little-endian->signed #xff #xff #xff #x7f) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 (little-endian->signed #xff #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 0 #x00000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 4 (little-endian->signed #xf3 #x45 #x23 #x19) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 8 (little-endian->signed #x23 #xc7 #x72 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 12 (little-endian->signed #x3a #x1c #x22 #x59) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 16 (little-endian->signed #xe3 #xd7 #xc2 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-s32-set! v 0 (apply little-endian->signed ls) 'little)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s32-set! $v1 1 -1 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xff #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 1 (little-endian->signed #x80 #x00 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x80 #x00 #x00 #x00 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 1 (native->signed #x00 #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x80 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 1 (native->signed #x7f #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x7f #xff #xff #xff #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 1 (big-endian->signed #xff #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #x7f #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 1 (little-endian->signed #xff #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xff #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 1 #x00000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 5 (native->signed #xf3 #x45 #x23 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 10 (little-endian->signed #x23 #xc7 #x72 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #x23 #xc7 #x72 #xe8 #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 15 (big-endian->signed #x3a #x1c #x22 #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #x23 #xc7 #x72 #xe8 #xad #x3a
                        #x1c #x22 #x59 #xad #xad #xad #xad))))
  (begin
    (bytevector-s32-set! $v1 19 (little-endian->signed #xe3 #xd7 #xc2 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #x23 #xc7 #x72 #xe8 #xad #x3a
                        #x1c #x22 #x59 #xe3 #xd7 #xc2 #xa9))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-s32-set! v 1 (apply big-endian->signed ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-s32-set! v 1 (apply little-endian->signed (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-s32-set! v 1 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-u32-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u32-set!))
  (error? (bytevector-u32-set! $v1))
  (error? (bytevector-u32-set! $v1 0))
  (error? (bytevector-u32-set! $v1 0 0))
  (error? (if (bytevector-u32-set! $v1 0 0 'big 0) #f #t))

 ; not a bytevector
  (error? (bytevector-u32-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (if (bytevector-u32-set! (make-vector 10) 0 0 (native-endianness)) #f #t))

 ; invalid index
  (error? (bytevector-u32-set! $v1 -1 0 'big))
  (error? (bytevector-u32-set! $v1 20 0 'little))
  (error? (bytevector-u32-set! $v1 21 0 (native-endianness)))
  (error? (bytevector-u32-set! $v1 22 0 'little))
  (error? (bytevector-u32-set! $v1 23 0 (native-endianness)))
  (error? (if (bytevector-u32-set! $v1 'q 0 'big) #f #t))

 ; invalid value
  (error? (bytevector-u32-set! $v1 0 #x100000000 'big))
  (error? (bytevector-u32-set! $v1 4 #x-1 (native-endianness)))
  (error? (if (bytevector-u32-set! $v1 8 "hello" 'little) #f #t))

 ; invalid endianness
  (error? (bytevector-u32-set! $v1 0 #xfffffff 'huge))
  (error? (bytevector-u32-set! $v1 4 0 "tiny"))
  (error? (if (bytevector-u32-set! $v1 8 0 $v1) #f #t))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

 ; aligned accesses, endianness native
  (begin
    (bytevector-u32-set! $v1 0 #xffffffff (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (native->unsigned #x80 #x00 #x00 #x00) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (native->unsigned #x00 #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (native->unsigned #x7f #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (native->unsigned #xff #xff #xff #x7f) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (native->unsigned #xff #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 #x00000000 (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 4 (native->unsigned #xf3 #x45 #x23 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 8 (native->unsigned #x23 #xc7 #x72 #xe8) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 12 (native->unsigned #x3a #x1c #x22 #x59) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 16 (native->unsigned #xe3 #xd7 #xc2 #xa9) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-u32-set! v 0 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; aligned accesses, endianness big
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u32-set! $v1 0 #xffffffff 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (big-endian->unsigned #x80 #x00 #x00 #x00) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (big-endian->unsigned #x00 #x00 #x00 #x80) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (big-endian->unsigned #x7f #xff #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (big-endian->unsigned #xff #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (big-endian->unsigned #xff #xff #xff #xff) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 #x00000000 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 4 (big-endian->unsigned #xf3 #x45 #x23 #x19) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 8 (big-endian->unsigned #x23 #xc7 #x72 #xe8) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 12 (big-endian->unsigned #x3a #x1c #x22 #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 16 (big-endian->unsigned #xe3 #xd7 #xc2 #xa9) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-u32-set! v 0 (apply big-endian->unsigned ls) 'big)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; aligned accesses, endianness little
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u32-set! $v1 0 #xffffffff 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (little-endian->unsigned #x80 #x00 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (little-endian->unsigned #x00 #x00 #x00 #x80) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x80 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (little-endian->unsigned #x7f #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x7f #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (little-endian->unsigned #xff #xff #xff #x7f) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #x7f #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 (little-endian->unsigned #xff #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xff #xff #xff #xff #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 0 #x00000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 4 (little-endian->unsigned #xf3 #x45 #x23 #x19) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 8 (little-endian->unsigned #x23 #xc7 #x72 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 12 (little-endian->unsigned #x3a #x1c #x22 #x59) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 16 (little-endian->unsigned #xe3 #xd7 #xc2 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #xf3 #x45 #x23 #x19
                        #x23 #xc7 #x72 #xe8 #x3a #x1c #x22 #x59
                        #xe3 #xd7 #xc2 #xa9 #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-u32-set! v 0 (apply little-endian->unsigned ls) 'little)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u32-set! $v1 1 #xffffffff 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xff #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 1 (little-endian->unsigned #x80 #x00 #x00 #x00) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x80 #x00 #x00 #x00 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 1 (native->unsigned #x00 #x00 #x00 #x80) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x80 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 1 (native->unsigned #x7f #xff #xff #xff) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x7f #xff #xff #xff #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 1 (big-endian->unsigned #xff #xff #xff #x7f) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #x7f #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 1 (little-endian->unsigned #xff #xff #xff #xff) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xff #xff #xff #xff #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 1 #x00000000 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 5 (native->unsigned #xf3 #x45 #x23 #x19) (native-endianness))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 10 (little-endian->unsigned #x23 #xc7 #x72 #xe8) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #x23 #xc7 #x72 #xe8 #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 15 (big-endian->unsigned #x3a #x1c #x22 #x59) 'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #x23 #xc7 #x72 #xe8 #xad #x3a
                        #x1c #x22 #x59 #xad #xad #xad #xad))))
  (begin
    (bytevector-u32-set! $v1 19 (little-endian->unsigned #xe3 #xd7 #xc2 #xa9) 'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #x00 #x00 #x00 #x00 #xf3 #x45 #x23
                        #x19 #xad #x23 #xc7 #x72 #xe8 #xad #x3a
                        #x1c #x22 #x59 #xe3 #xd7 #xc2 #xa9))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 4))])
        (bytevector-u32-set! v 1 (apply big-endian->unsigned ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-u32-set! v 1 (apply little-endian->unsigned (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-u32-set! v 1 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-s40-ref
 ; wrong argument count
  (error? (bytevector-s40-ref))
  (error? (bytevector-s40-ref #vu8(3 252 5 0 0)))
  (error? (bytevector-s40-ref #vu8(3 252 5 0 0) 0))
  (error? (begin (bytevector-s40-ref #vu8(3 252 5 0 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-s40-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-s40-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-s40-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-s40-ref #vu8(3 252 5 0 0 0 0) 3 'little))
  (error? (bytevector-s40-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-s40-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-s40-ref #vu8(0 1 2 3 4 5 6 7) 0 'bigger))
  (error? (bytevector-s40-ref #vu8(0 1 2 3 4 5 6 7) 0 "little"))
  (error? (begin (bytevector-s40-ref #vu8(0 1 2 3 4 5 6 7) 0 #t) #f))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
      (unless (eqv? (bytevector-s40-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-s40-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-s40-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
      (unless (eqv? (eval `(bytevector-s40-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-s40-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-s40-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-u40-ref
 ; wrong argument count
  (error? (bytevector-u40-ref))
  (error? (bytevector-u40-ref #vu8(3 252 5 0 0)))
  (error? (bytevector-u40-ref #vu8(3 252 5 0 0) 0))
  (error? (begin (bytevector-u40-ref #vu8(3 252 5 0 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-u40-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-u40-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-u40-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-u40-ref #vu8(3 252 5 0 0 0 0) 3 'little))
  (error? (bytevector-u40-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-u40-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-u40-ref #vu8(0 1 2 3 4 5 6 7) 0 'bigger))
  (error? (bytevector-u40-ref #vu8(0 1 2 3 4 5 6 7) 0 "little"))
  (error? (begin (bytevector-u40-ref #vu8(0 1 2 3 4 5 6 7) 0 #t) #f))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
      (unless (eqv? (bytevector-u40-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-u40-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-u40-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
      (unless (eqv? (eval `(bytevector-u40-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-u40-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-u40-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-s40-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s40-set!))
  (error? (bytevector-s40-set! $v1))
  (error? (bytevector-s40-set! $v1 0))
  (error? (bytevector-s40-set! $v1 0 0))
  (error? (begin (bytevector-s40-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-s40-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-s40-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-s40-set! $v1 -1 0 'big))
  (error? (bytevector-s40-set! $v1 19 0 (native-endianness)))
  (error? (bytevector-s40-set! $v1 22 0 'little))
  (error? (bytevector-s40-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-s40-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-s40-set! $v1 0 (expt 2 39) 'big))
  (error? (bytevector-s40-set! $v1 4 (- -1 (expt 2 39)) (native-endianness)))
  (error? (begin (bytevector-s40-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-s40-set! $v1 0 #x7ffffff 'huge))
  (error? (bytevector-s40-set! $v1 4 #x-80000000 "tiny"))
  (error? (begin (bytevector-s40-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 5))])
        (bytevector-s40-set! v 1 (apply big-endian->signed ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-s40-set! v 1 (apply little-endian->signed (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-s40-set! v 1 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 5))])
        (eval `(bytevector-s40-set! ,v 1 ,(apply big-endian->signed ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s40-set! ,v 1 ,(apply little-endian->signed (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s40-set! ,v 1 ,(apply native->signed ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-u40-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u40-set!))
  (error? (bytevector-u40-set! $v1))
  (error? (bytevector-u40-set! $v1 0))
  (error? (bytevector-u40-set! $v1 0 0))
  (error? (begin (bytevector-u40-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-u40-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-u40-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-u40-set! $v1 -1 0 'big))
  (error? (bytevector-u40-set! $v1 19 0 (native-endianness)))
  (error? (bytevector-u40-set! $v1 22 0 'little))
  (error? (bytevector-u40-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-u40-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-u40-set! $v1 0 (expt 2 40) 'big))
  (error? (bytevector-u40-set! $v1 4 -1 (native-endianness)))
  (error? (begin (bytevector-u40-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-u40-set! $v1 0 0 'huge))
  (error? (bytevector-u40-set! $v1 4 0 "tiny"))
  (error? (begin (bytevector-u40-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 5))])
        (bytevector-u40-set! v 1 (apply big-endian->unsigned ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-u40-set! v 1 (apply little-endian->unsigned (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-u40-set! v 1 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 5))])
        (eval `(bytevector-u40-set! ,v 1 ,(apply big-endian->unsigned ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u40-set! ,v 1 ,(apply little-endian->unsigned (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u40-set! ,v 1 ,(apply native->unsigned ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-s48-ref
 ; wrong argument count
  (error? (bytevector-s48-ref))
  (error? (bytevector-s48-ref #vu8(3 252 5 0 0 0)))
  (error? (bytevector-s48-ref #vu8(3 252 5 0 0 0) 0))
  (error? (begin (bytevector-s48-ref #vu8(3 252 5 0 0 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-s48-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-s48-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-s48-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-s48-ref #vu8(3 252 5 0 0 0 0) 2 'little))
  (error? (bytevector-s48-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-s48-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-s48-ref #vu8(0 1 2 3 4 5 6 7) 0 'bigger))
  (error? (bytevector-s48-ref #vu8(0 1 2 3 4 5 6 7) 0 "little"))
  (error? (begin (bytevector-s48-ref #vu8(0 1 2 3 4 5 6 7) 0 #t) #f))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
      (unless (eqv? (bytevector-s48-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-s48-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-s48-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
      (unless (eqv? (eval `(bytevector-s48-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-s48-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-s48-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-u48-ref
 ; wrong argument count
  (error? (bytevector-u48-ref))
  (error? (bytevector-u48-ref #vu8(3 252 5 0 0 0)))
  (error? (bytevector-u48-ref #vu8(3 252 5 0 0 0) 0))
  (error? (begin (bytevector-u48-ref #vu8(3 252 5 0 0 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-u48-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-u48-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-u48-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-u48-ref #vu8(3 252 5 0 0 0 0) 2 'little))
  (error? (bytevector-u48-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-u48-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-u48-ref #vu8(0 1 2 3 4 5 6 7) 0 'bigger))
  (error? (bytevector-u48-ref #vu8(0 1 2 3 4 5 6 7) 0 "little"))
  (error? (begin (bytevector-u48-ref #vu8(0 1 2 3 4 5 6 7) 0 #t) #f))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
      (unless (eqv? (bytevector-u48-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-u48-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-u48-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
      (unless (eqv? (eval `(bytevector-u48-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-u48-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-u48-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-s48-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s48-set!))
  (error? (bytevector-s48-set! $v1))
  (error? (bytevector-s48-set! $v1 0))
  (error? (bytevector-s48-set! $v1 0 0))
  (error? (begin (bytevector-s48-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-s48-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-s48-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-s48-set! $v1 -1 0 'big))
  (error? (bytevector-s48-set! $v1 18 0 (native-endianness)))
  (error? (bytevector-s48-set! $v1 22 0 'little))
  (error? (bytevector-s48-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-s48-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-s48-set! $v1 0 (expt 2 47) 'big))
  (error? (bytevector-s48-set! $v1 4 (- -1 (expt 2 47)) (native-endianness)))
  (error? (begin (bytevector-s48-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-s48-set! $v1 0 0 'huge))
  (error? (bytevector-s48-set! $v1 4 0 "tiny"))
  (error? (begin (bytevector-s48-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
        (bytevector-s48-set! v 1 (apply big-endian->signed ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-s48-set! v 1 (apply little-endian->signed (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-s48-set! v 1 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
        (eval `(bytevector-s48-set! ,v 1 ,(apply big-endian->signed ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s48-set! ,v 1 ,(apply little-endian->signed (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s48-set! ,v 1 ,(apply native->signed ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-u48-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u48-set!))
  (error? (bytevector-u48-set! $v1))
  (error? (bytevector-u48-set! $v1 0))
  (error? (bytevector-u48-set! $v1 0 0))
  (error? (begin (bytevector-u48-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-u48-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-u48-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-u48-set! $v1 -1 0 'big))
  (error? (bytevector-u48-set! $v1 18 0 (native-endianness)))
  (error? (bytevector-u48-set! $v1 22 0 'little))
  (error? (bytevector-u48-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-u48-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-u48-set! $v1 0 (expt 2 48) 'big))
  (error? (bytevector-u48-set! $v1 4 -1 (native-endianness)))
  (error? (begin (bytevector-u48-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-u48-set! $v1 0 0 'huge))
  (error? (bytevector-u48-set! $v1 4 0 "tiny"))
  (error? (begin (bytevector-u48-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
        (bytevector-u48-set! v 1 (apply big-endian->unsigned ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-u48-set! v 1 (apply little-endian->unsigned (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-u48-set! v 1 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 6))])
        (eval `(bytevector-u48-set! ,v 1 ,(apply big-endian->unsigned ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u48-set! ,v 1 ,(apply little-endian->unsigned (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u48-set! ,v 1 ,(apply native->unsigned ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-s56-ref
 ; wrong argument count
  (error? (bytevector-s56-ref))
  (error? (bytevector-s56-ref #vu8(3 252 5 0 0 0 0)))
  (error? (bytevector-s56-ref #vu8(3 252 5 0 0 00 ) 0))
  (error? (begin (bytevector-s56-ref #vu8(3 252 5 0 0 0 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-s56-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-s56-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-s56-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-s56-ref #vu8(3 252 5 0 0 0 0) 1 'little))
  (error? (bytevector-s56-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-s56-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-s56-ref #vu8(0 1 2 3 4 5 6 7) 0 'bigger))
  (error? (bytevector-s56-ref #vu8(0 1 2 3 4 5 6 7) 0 "little"))
  (error? (begin (bytevector-s56-ref #vu8(0 1 2 3 4 5 6 7) 0 #t) #f))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-s56-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-s56-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-s56-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (eval `(bytevector-s56-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->signed (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-s56-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-s56-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->signed (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-u56-ref
 ; wrong argument count
  (error? (bytevector-u56-ref))
  (error? (bytevector-u56-ref #vu8(3 252 5 0 0 0 0)))
  (error? (bytevector-u56-ref #vu8(3 252 5 0 0 00 ) 0))
  (error? (begin (bytevector-u56-ref #vu8(3 252 5 0 0 0 0) 0 (native-endianness) 0) #f))

 ; not a bytevector
  (error? (bytevector-u56-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (begin (bytevector-u56-ref '#(3 252 5 0 0 0 0) 0 'big) #f))

 ; invalid index
  (error? (bytevector-u56-ref #vu8(3 252 5 0 0 0 0) -1 'big))
  (error? (bytevector-u56-ref #vu8(3 252 5 0 0 0 0) 1 'little))
  (error? (bytevector-u56-ref #vu8(3 252 5 0 0 0 0) 7 (native-endianness)))
  (error? (begin (bytevector-u56-ref #vu8(3 252 5 0 0 0 0) 4.0 'little) #f))

 ; invalid endianness
  (error? (bytevector-u56-ref #vu8(0 1 2 3 4 5 6 7) 0 'bigger))
  (error? (bytevector-u56-ref #vu8(0 1 2 3 4 5 6 7) 0 "little"))
  (error? (begin (bytevector-u56-ref #vu8(0 1 2 3 4 5 6 7) 0 #t) #f))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-u56-ref (apply bytevector ls) 1 (native-endianness))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (bytevector-u56-ref (apply bytevector ls) 1 'little)
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (bytevector-u56-ref (apply bytevector ls) 1 'big)
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (eval `(bytevector-u56-ref ,(apply bytevector ls) 1 (native-endianness)))
                    (apply native->unsigned (cdr ls)))
        (errorf #f "failed for ~s (native)" ls))
      (unless (eqv? (eval `(bytevector-u56-ref ,(apply bytevector ls) 1 'little))
                    (apply little-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (little)" ls))
      (unless (eqv? (eval `(bytevector-u56-ref ,(apply bytevector ls) 1 'big))
                    (apply big-endian->unsigned (cdr ls)))
        (errorf #f "failed for ~s (big)" ls))))
)

(mat bytevector-s56-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s56-set!))
  (error? (bytevector-s56-set! $v1))
  (error? (bytevector-s56-set! $v1 0))
  (error? (bytevector-s56-set! $v1 0 0))
  (error? (begin (bytevector-s56-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-s56-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-s56-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-s56-set! $v1 -1 0 'big))
  (error? (bytevector-s56-set! $v1 17 0 (native-endianness)))
  (error? (bytevector-s56-set! $v1 22 0 'little))
  (error? (bytevector-s56-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-s56-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-s56-set! $v1 0 (expt 2 55) 'big))
  (error? (bytevector-s56-set! $v1 4 (- -1 (expt 2 55)) (native-endianness)))
  (error? (begin (bytevector-s56-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-s56-set! $v1 0 0 'huge))
  (error? (bytevector-s56-set! $v1 4 0 "tiny"))
  (error? (begin (bytevector-s56-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
        (bytevector-s56-set! v 1 (apply big-endian->signed ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-s56-set! v 1 (apply little-endian->signed (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-s56-set! v 1 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
        (eval `(bytevector-s56-set! ,v 1 ,(apply big-endian->signed ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s56-set! ,v 1 ,(apply little-endian->signed (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-s56-set! ,v 1 ,(apply native->signed ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-u56-set!
  (begin
    (define $v1 (make-bytevector 23 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u56-set!))
  (error? (bytevector-u56-set! $v1))
  (error? (bytevector-u56-set! $v1 0))
  (error? (bytevector-u56-set! $v1 0 0))
  (error? (begin (bytevector-u56-set! $v1 0 0 'big 0) #f))

 ; not a bytevector
  (error? (bytevector-u56-set! (make-vector 10) 0 0 (native-endianness)))
  (error? (begin (bytevector-u56-set! (make-vector 10) 0 0 (native-endianness)) #f))

 ; invalid index
  (error? (bytevector-u56-set! $v1 -1 0 'big))
  (error? (bytevector-u56-set! $v1 17 0 (native-endianness)))
  (error? (bytevector-u56-set! $v1 22 0 'little))
  (error? (bytevector-u56-set! $v1 23 0 (native-endianness)))
  (error? (begin (bytevector-u56-set! $v1 'q 0 'big) #f))

 ; invalid value
  (error? (bytevector-u56-set! $v1 0 (expt 2 56) 'big))
  (error? (bytevector-u56-set! $v1 4 -1 (native-endianness)))
  (error? (begin (bytevector-u56-set! $v1 8 "hello" 'little) #f))

 ; invalid endianness
  (error? (bytevector-u56-set! $v1 0 0 'huge))
  (error? (bytevector-u56-set! $v1 4 0 "tiny"))
  (error? (begin (bytevector-u56-set! $v1 8 0 $v1) #f))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
        (bytevector-u56-set! v 1 (apply big-endian->unsigned ls) 'big)
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-u56-set! v 1 (apply little-endian->unsigned (reverse ls)) 'little)
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-u56-set! v 1 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 1000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 7))])
        (eval `(bytevector-u56-set! ,v 1 ,(apply big-endian->unsigned ls) 'big))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u56-set! ,v 1 ,(apply little-endian->unsigned (reverse ls)) 'little))
        (unless (equal? v (apply bytevector #xc7 (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (eval `(bytevector-u56-set! ,v 1 ,(apply native->unsigned ls) (native-endianness)))
        (unless (equal? v (apply bytevector #xc7 ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-s64-native-ref
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-s64-native-ref))
  (error? (bytevector-s64-native-ref $v1))
  (error? (if (bytevector-s64-native-ref $v1 0 0) #f #t))

 ; not a bytevector
  (error? (bytevector-s64-native-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0))
  (error? (if (bytevector-s64-native-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0) #f #t))

 ; invalid index
  (error? (bytevector-s64-native-ref $v1 -1))
  (error? (bytevector-s64-native-ref $v1 1))
  (error? (bytevector-s64-native-ref $v1 2))
  (error? (bytevector-s64-native-ref $v1 3))
  (error? (bytevector-s64-native-ref $v1 4))
  (error? (bytevector-s64-native-ref $v1 5))
  (error? (bytevector-s64-native-ref $v1 6))
  (error? (bytevector-s64-native-ref $v1 7))
  (error? (bytevector-s64-native-ref $v1 9))
  (error? (bytevector-s64-native-ref $v1 18))
  (error? (bytevector-s64-native-ref $v1 27))
  (error? (bytevector-s64-native-ref $v1 36))
  (error? (bytevector-s64-native-ref $v1 45))
  (error? (bytevector-s64-native-ref $v1 54))
  (error? (bytevector-s64-native-ref $v1 63))
  (error? (bytevector-s64-native-ref $v1 73))
  (error? (bytevector-s64-native-ref $v1 82))
  (error? (bytevector-s64-native-ref $v1 91))
  (error? (bytevector-s64-native-ref $v1 96))
  (error? (bytevector-s64-native-ref $v1 97))
  (error? (bytevector-s64-native-ref $v1 98))
  (error? (bytevector-s64-native-ref $v1 99))
  (error? (bytevector-s64-native-ref $v1 100))
  (error? (bytevector-s64-native-ref $v1 101))
  (error? (bytevector-s64-native-ref $v1 102))
  (error? (bytevector-s64-native-ref $v1 103))
  (error? (if (bytevector-s64-native-ref $v1 4.0) #f #t))

  (eqv? (bytevector-s64-native-ref $v1 0) 0)
  (eqv? (bytevector-s64-native-ref $v1 8) -1)
  (eqv? (bytevector-s64-native-ref $v1 16)
        (native->signed #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-s64-native-ref $v1 24)
        (native->signed #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-s64-native-ref $v1 32)
        (native->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-s64-native-ref $v1 40)
        (native->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-native-ref $v1 48)
        (native->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-s64-native-ref $v1 56)
        (native->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-native-ref $v1 64)
        (native->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-s64-native-ref $v1 72)
        (native->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-s64-native-ref $v1 80)
        (native->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-s64-native-ref $v1 88)
        (native->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (test-endian-sensitive-cp0-expansion eqv? `(bytevector-s64-native-ref ,$v1 0) 0)
  (test-endian-sensitive-cp0-expansion eqv? `(bytevector-s64-native-ref ,$v1 8) -1)
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 16)
    (native->signed #x7f #xff #xff #xff #xff #xff #xff #xff))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 24)
    (native->signed #xff #xff #xff #xff #xff #xff #xff #x7f))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 32)
    (native->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 40)
    (native->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 48)
    (native->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 56)
    (native->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 64)
    (native->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 72)
    (native->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 80)
    (native->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-s64-native-ref ,$v1 88)
    (native->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-s64-native-ref (apply bytevector ls) 0)
                    (apply native->signed ls))
        (errorf #f "failed for ~s" ls))))
)

(mat bytevector-u64-native-ref
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-u64-native-ref))
  (error? (bytevector-u64-native-ref $v1))
  (error? (if (bytevector-u64-native-ref $v1 0 0) #f #t))

 ; not a bytevector
  (error? (bytevector-u64-native-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0))
  (error? (if (bytevector-u64-native-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0) #f #t))

 ; invalid index
  (error? (bytevector-u64-native-ref $v1 -1))
  (error? (bytevector-u64-native-ref $v1 1))
  (error? (bytevector-u64-native-ref $v1 2))
  (error? (bytevector-u64-native-ref $v1 3))
  (error? (bytevector-u64-native-ref $v1 4))
  (error? (bytevector-u64-native-ref $v1 5))
  (error? (bytevector-u64-native-ref $v1 6))
  (error? (bytevector-u64-native-ref $v1 7))
  (error? (bytevector-u64-native-ref $v1 9))
  (error? (bytevector-u64-native-ref $v1 18))
  (error? (bytevector-u64-native-ref $v1 27))
  (error? (bytevector-u64-native-ref $v1 36))
  (error? (bytevector-u64-native-ref $v1 45))
  (error? (bytevector-u64-native-ref $v1 54))
  (error? (bytevector-u64-native-ref $v1 63))
  (error? (bytevector-u64-native-ref $v1 73))
  (error? (bytevector-u64-native-ref $v1 82))
  (error? (bytevector-u64-native-ref $v1 91))
  (error? (bytevector-u64-native-ref $v1 96))
  (error? (bytevector-u64-native-ref $v1 97))
  (error? (bytevector-u64-native-ref $v1 98))
  (error? (bytevector-u64-native-ref $v1 99))
  (error? (bytevector-u64-native-ref $v1 100))
  (error? (bytevector-u64-native-ref $v1 101))
  (error? (bytevector-u64-native-ref $v1 102))
  (error? (bytevector-u64-native-ref $v1 103))
  (error? (if (bytevector-u64-native-ref $v1 4.0) #f #t))

  (eqv? (bytevector-u64-native-ref $v1 0) 0)
  (eqv? (bytevector-u64-native-ref $v1 8) (- (expt 2 64) 1))
  (eqv? (bytevector-u64-native-ref $v1 16)
        (native->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-u64-native-ref $v1 24)
        (native->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-u64-native-ref $v1 32)
        (native->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-u64-native-ref $v1 40)
        (native->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-native-ref $v1 48)
        (native->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-u64-native-ref $v1 56)
        (native->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-native-ref $v1 64)
        (native->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-u64-native-ref $v1 72)
        (native->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-u64-native-ref $v1 80)
        (native->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-u64-native-ref $v1 88)
        (native->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (test-endian-sensitive-cp0-expansion eqv? `(bytevector-u64-native-ref ,$v1 0) 0)
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 8) 
    (- (expt 2 64) 1))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 16)
    (native->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 24)
    (native->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 32)
    (native->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 40)
    (native->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 48)
    (native->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 56)
    (native->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 64)
    (native->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 72)
    (native->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 80)
    (native->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (test-endian-sensitive-cp0-expansion eqv? 
    `(bytevector-u64-native-ref ,$v1 88)
    (native->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-u64-native-ref (apply bytevector ls) 0)
                    (apply native->unsigned ls))
        (errorf #f "failed for ~s" ls))))
)

(mat bytevector-s64-native-set!
  (begin
    (define $v1 (make-bytevector 39 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s64-native-set!))
  (error? (bytevector-s64-native-set! $v1))
  (error? (bytevector-s64-native-set! $v1 0))
  (error? (if (bytevector-s64-native-set! $v1 0 0 15) #f #t))

 ; not a bytevector
  (error? (bytevector-s64-native-set! (make-vector 10) 0 0))
  (error? (if (bytevector-s64-native-set! (make-vector 10) 0 0) #f #t))

 ; invalid index
  (error? (bytevector-s64-native-set! $v1 -1 0))
  (error? (bytevector-s64-native-set! $v1 1 0))
  (error? (bytevector-s64-native-set! $v1 2 0))
  (error? (bytevector-s64-native-set! $v1 3 0))
  (error? (bytevector-s64-native-set! $v1 4 0))
  (error? (bytevector-s64-native-set! $v1 5 0))
  (error? (bytevector-s64-native-set! $v1 6 0))
  (error? (bytevector-s64-native-set! $v1 7 0))
  (error? (bytevector-s64-native-set! $v1 9 0))
  (error? (bytevector-s64-native-set! $v1 10 0))
  (error? (bytevector-s64-native-set! $v1 11 0))
  (error? (bytevector-s64-native-set! $v1 12 0))
  (error? (bytevector-s64-native-set! $v1 13 0))
  (error? (bytevector-s64-native-set! $v1 14 0))
  (error? (bytevector-s64-native-set! $v1 15 0))
  (error? (bytevector-s64-native-set! $v1 17 0))
  (error? (bytevector-s64-native-set! $v1 20 0))
  (error? (bytevector-s64-native-set! $v1 23 0))
  (error? (bytevector-s64-native-set! $v1 28 0))
  (error? (bytevector-s64-native-set! $v1 32 0))
  (error? (bytevector-s64-native-set! $v1 33 0))
  (error? (bytevector-s64-native-set! $v1 34 0))
  (error? (bytevector-s64-native-set! $v1 35 0))
  (error? (bytevector-s64-native-set! $v1 36 0))
  (error? (bytevector-s64-native-set! $v1 37 0))
  (error? (bytevector-s64-native-set! $v1 38 0))
  (error? (bytevector-s64-native-set! $v1 39 0))
  (error? (if (bytevector-s64-native-set! $v1 'q 0) #f #t))

 ; invalid value
  (error? (bytevector-s64-native-set! $v1 0 #x8000000000000000))
  (error? (bytevector-s64-native-set! $v1 8 #x-8000000000000001))
  (error? (if (bytevector-s64-native-set! $v1 16 "hello") #f #t))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (begin
    (bytevector-s64-native-set! $v1 0 0)
    (bytevector-s64-native-set! $v1 8 -1)
    (bytevector-s64-native-set! $v1 16
      (native->signed #x7f #xff #xff #xff #xff #xff #xff #xff))
    (bytevector-s64-native-set! $v1 24
      (native->signed #xff #xff #xff #xff #xff #xff #xff #x7f))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #x7f
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s64-native-set! $v1 0
      (native->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
    (bytevector-s64-native-set! $v1 8
      (native->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
    (bytevector-s64-native-set! $v1 16
      (native->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
    (bytevector-s64-native-set! $v1 24
      (native->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s64-native-set! $v1 0
      (native->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
    (bytevector-s64-native-set! $v1 8
      (native->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
    (bytevector-s64-native-set! $v1 16
      (native->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
    (bytevector-s64-native-set! $v1 24
      (native->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78
                        #xad #xad #xad #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
        (bytevector-s64-native-set! v 0 (apply native->signed ls))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-u64-native-set!
  (begin
    (define $v1 (make-bytevector 39 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u64-native-set!))
  (error? (bytevector-u64-native-set! $v1))
  (error? (bytevector-u64-native-set! $v1 0))
  (error? (if (bytevector-u64-native-set! $v1 0 0 15) #f #t))

 ; not a bytevector
  (error? (bytevector-u64-native-set! (make-vector 10) 0 0))
  (error? (if (bytevector-u64-native-set! (make-vector 10) 0 0) #f #t))

 ; invalid index
  (error? (bytevector-u64-native-set! $v1 -1 0))
  (error? (bytevector-u64-native-set! $v1 1 0))
  (error? (bytevector-u64-native-set! $v1 2 0))
  (error? (bytevector-u64-native-set! $v1 3 0))
  (error? (bytevector-u64-native-set! $v1 4 0))
  (error? (bytevector-u64-native-set! $v1 5 0))
  (error? (bytevector-u64-native-set! $v1 6 0))
  (error? (bytevector-u64-native-set! $v1 7 0))
  (error? (bytevector-u64-native-set! $v1 9 0))
  (error? (bytevector-u64-native-set! $v1 10 0))
  (error? (bytevector-u64-native-set! $v1 11 0))
  (error? (bytevector-u64-native-set! $v1 12 0))
  (error? (bytevector-u64-native-set! $v1 13 0))
  (error? (bytevector-u64-native-set! $v1 14 0))
  (error? (bytevector-u64-native-set! $v1 15 0))
  (error? (bytevector-u64-native-set! $v1 17 0))
  (error? (bytevector-u64-native-set! $v1 20 0))
  (error? (bytevector-u64-native-set! $v1 23 0))
  (error? (bytevector-u64-native-set! $v1 28 0))
  (error? (bytevector-u64-native-set! $v1 32 0))
  (error? (bytevector-u64-native-set! $v1 33 0))
  (error? (bytevector-u64-native-set! $v1 34 0))
  (error? (bytevector-u64-native-set! $v1 35 0))
  (error? (bytevector-u64-native-set! $v1 36 0))
  (error? (bytevector-u64-native-set! $v1 37 0))
  (error? (bytevector-u64-native-set! $v1 38 0))
  (error? (bytevector-u64-native-set! $v1 39 0))
  (error? (if (bytevector-u64-native-set! $v1 'q 0) #f #t))

 ; invalid value
  (error? (bytevector-u64-native-set! $v1 0 #x10000000000000000))
  (error? (bytevector-u64-native-set! $v1 8 #x-1))
  (error? (if (bytevector-u64-native-set! $v1 16 "hello") #f #t))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

  (begin
    (bytevector-u64-native-set! $v1 0 0)
    (bytevector-u64-native-set! $v1 8 #xffffffffffffffff)
    (bytevector-u64-native-set! $v1 16
      (native->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff))
    (bytevector-u64-native-set! $v1 24
      (native->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #x7f
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u64-native-set! $v1 0
      (native->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
    (bytevector-u64-native-set! $v1 8
      (native->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
    (bytevector-u64-native-set! $v1 16
      (native->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
    (bytevector-u64-native-set! $v1 24
      (native->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u64-native-set! $v1 0
      (native->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
    (bytevector-u64-native-set! $v1 8
      (native->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
    (bytevector-u64-native-set! $v1 16
      (native->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
    (bytevector-u64-native-set! $v1 24
      (native->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78
                        #xad #xad #xad #xad #xad #xad #xad))))

  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
        (bytevector-u64-native-set! v 0 (apply native->unsigned ls))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))
)

(mat bytevector-s64-ref
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-s64-ref))
  (error? (bytevector-s64-ref $v1))
  (error? (bytevector-s64-ref $v1 0))
  (error? (if (bytevector-s64-ref $v1 0 'big 0) #f #t))

 ; not a bytevector
  (error? (bytevector-s64-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little))
  (error? (if (bytevector-s64-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little) #f #t))

 ; invalid index
  (error? (bytevector-s64-ref $v1 -1 'big))
  (error? (bytevector-s64-ref $v1 96 'little))
  (error? (bytevector-s64-ref $v1 97 'big))
  (error? (bytevector-s64-ref $v1 98 'little))
  (error? (bytevector-s64-ref $v1 99 'big))
  (error? (bytevector-s64-ref $v1 100 'little))
  (error? (bytevector-s64-ref $v1 101 'big))
  (error? (bytevector-s64-ref $v1 102 'little))
  (error? (bytevector-s64-ref $v1 103 'big))
  (error? (if (bytevector-s64-ref $v1 4.0 (native-endianness)) #f #t))

 ; invalid endianness
  (error? (bytevector-s64-ref $v1 0 ''bonkers))
  (error? (bytevector-s64-ref $v1 0 'get-real))
  (error? (if (bytevector-s64-ref $v1 0 1e23) #f #t))

 ; (not bothering with native endianness, since it's either big or little)

 ; aligned accesses, endianness little
  (eqv? (bytevector-s64-ref $v1 0 'little) 0)
  (eqv? (bytevector-s64-ref $v1 8 'little) -1)
  (eqv? (bytevector-s64-ref $v1 16 'little)
        (little-endian->signed #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-s64-ref $v1 24 'little)
        (little-endian->signed #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-s64-ref $v1 32 'little)
        (little-endian->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-s64-ref $v1 40 'little)
        (little-endian->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-ref $v1 48 'little)
        (little-endian->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-s64-ref $v1 56 'little)
        (little-endian->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-ref $v1 64 'little)
        (little-endian->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-s64-ref $v1 72 'little)
        (little-endian->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-s64-ref $v1 80 'little)
        (little-endian->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-s64-ref $v1 88 'little)
        (little-endian->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-s64-ref (apply bytevector ls) 0 'little)
                    (apply little-endian->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; aligned accesses, endianness big
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

  (eqv? (bytevector-s64-ref $v1 0 'big) 0)
  (eqv? (bytevector-s64-ref $v1 8 'big) -1)
  (eqv? (bytevector-s64-ref $v1 16 'big)
        (big-endian->signed #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-s64-ref $v1 24 'big)
        (big-endian->signed #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-s64-ref $v1 32 'big)
        (big-endian->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-s64-ref $v1 40 'big)
        (big-endian->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-ref $v1 48 'big)
        (big-endian->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-s64-ref $v1 56 'big)
        (big-endian->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-ref $v1 64 'big)
        (big-endian->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-s64-ref $v1 72 'big)
        (big-endian->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-s64-ref $v1 80 'big)
        (big-endian->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-s64-ref $v1 88 'big)
        (big-endian->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-s64-ref (apply bytevector ls) 0 'big)
                    (apply big-endian->signed ls))
        (errorf #f "failed for ~s" ls))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1
      '#vu8(#xc7
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00     ; 1
            #xc7
            #xff #xff #xff #xff #xff #xff #xff #xff     ; 10
            #xc7
            #x7f #xff #xff #xff #xff #xff #xff #xff     ; 19
            #xc7
            #xff #xff #xff #xff #xff #xff #xff #x7f     ; 28
            #xc7
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00     ; 37
            #xc7
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80     ; 46
            #xc7
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff     ; 55
            #xc7 #xc7
            #xff #xff #xff #xff #x00 #x00 #x00 #x80     ; 65
            #xc7
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89     ; 74
            #xc7
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12     ; 83
            #xc7
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef     ; 92
            #xc7
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78))   ; 101
    (bytevector? $v1))

  (eqv? (bytevector-s64-ref $v1 1 'big) 0)
  (eqv? (bytevector-s64-ref $v1 10 'little) -1)
  (eqv? (bytevector-s64-ref $v1 19 (native-endianness))
        (native->signed #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-s64-ref $v1 28 'big)
        (big-endian->signed #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-s64-ref $v1 37 'little)
        (little-endian->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-s64-ref $v1 46 'big)
        (big-endian->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-ref $v1 55 'little)
        (little-endian->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-s64-ref $v1 65 'big)
        (big-endian->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-s64-ref $v1 74 'little)
        (little-endian->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-s64-ref $v1 83 (native-endianness))
        (native->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-s64-ref $v1 92 'big)
        (big-endian->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-s64-ref $v1 101 'little)
        (little-endian->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-s64-ref (apply bytevector #x3e ls) 1 (native-endianness))
                    (apply native->signed ls))
        (errorf #f "failed for ~s" ls))
      (unless (eqv? (bytevector-s64-ref (apply bytevector #x3e ls) 1 'big)
                    (apply big-endian->signed ls))
        (errorf #f "failed for ~s" ls))
      (unless (eqv? (bytevector-s64-ref (apply bytevector #x3e ls) 1 'little)
                    (apply little-endian->signed ls))
        (errorf #f "failed for ~s" ls))))
)

(mat bytevector-u64-ref
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-u64-ref))
  (error? (bytevector-u64-ref $v1))
  (error? (bytevector-u64-ref $v1 0))
  (error? (if (bytevector-u64-ref $v1 0 'big 0) #f #t))

 ; not a bytevector
  (error? (bytevector-u64-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little))
  (error? (if (bytevector-u64-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little) #f #t))

 ; invalid index
  (error? (bytevector-u64-ref $v1 -1 'big))
  (error? (bytevector-u64-ref $v1 96 'little))
  (error? (bytevector-u64-ref $v1 97 'big))
  (error? (bytevector-u64-ref $v1 98 'little))
  (error? (bytevector-u64-ref $v1 99 'big))
  (error? (bytevector-u64-ref $v1 100 'little))
  (error? (bytevector-u64-ref $v1 101 'big))
  (error? (bytevector-u64-ref $v1 102 'little))
  (error? (bytevector-u64-ref $v1 103 'big))
  (error? (if (bytevector-u64-ref $v1 4.0 (native-endianness)) #f #t))

 ; invalid endianness
  (error? (bytevector-u64-ref $v1 0 ''bonkers))
  (error? (bytevector-u64-ref $v1 0 'get-real))
  (error? (if (bytevector-u64-ref $v1 0 1e23) #f #t))

 ; (not bothering with native endianness, since it's either big or little)

 ; aligned accesses, endianness little
  (eqv? (bytevector-u64-ref $v1 0 'little) 0)
  (eqv? (bytevector-u64-ref $v1 8 'little) #xffffffffffffffff)
  (eqv? (bytevector-u64-ref $v1 16 'little)
        (little-endian->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-u64-ref $v1 24 'little)
        (little-endian->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-u64-ref $v1 32 'little)
        (little-endian->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-u64-ref $v1 40 'little)
        (little-endian->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-ref $v1 48 'little)
        (little-endian->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-u64-ref $v1 56 'little)
        (little-endian->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-ref $v1 64 'little)
        (little-endian->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-u64-ref $v1 72 'little)
        (little-endian->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-u64-ref $v1 80 'little)
        (little-endian->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-u64-ref $v1 88 'little)
        (little-endian->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

 ; aligned accesses, endianness big
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

  (eqv? (bytevector-u64-ref $v1 0 'big) 0)
  (eqv? (bytevector-u64-ref $v1 8 'big) #xffffffffffffffff)
  (eqv? (bytevector-u64-ref $v1 16 'big)
        (big-endian->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-u64-ref $v1 24 'big)
        (big-endian->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-u64-ref $v1 32 'big)
        (big-endian->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-u64-ref $v1 40 'big)
        (big-endian->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-ref $v1 48 'big)
        (big-endian->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-u64-ref $v1 56 'big)
        (big-endian->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-ref $v1 64 'big)
        (big-endian->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-u64-ref $v1 72 'big)
        (big-endian->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-u64-ref $v1 80 'big)
        (big-endian->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-u64-ref $v1 88 'big)
        (big-endian->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-u64-ref (apply bytevector ls) 0 'big)
                    (apply big-endian->unsigned ls))
        (errorf #f "failed for ~s" ls))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1
      '#vu8(#xc7
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00     ; 1
            #xc7
            #xff #xff #xff #xff #xff #xff #xff #xff     ; 10
            #xc7
            #x7f #xff #xff #xff #xff #xff #xff #xff     ; 19
            #xc7
            #xff #xff #xff #xff #xff #xff #xff #x7f     ; 28
            #xc7
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00     ; 37
            #xc7
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80     ; 46
            #xc7
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff     ; 55
            #xc7 #xc7
            #xff #xff #xff #xff #x00 #x00 #x00 #x80     ; 65
            #xc7
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89     ; 74
            #xc7
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12     ; 83
            #xc7
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef     ; 92
            #xc7
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78))   ; 101
    (bytevector? $v1))

  (eqv? (bytevector-u64-ref $v1 1 'big) 0)
  (eqv? (bytevector-u64-ref $v1 10 'little) #xffffffffffffffff)
  (eqv? (bytevector-u64-ref $v1 19 (native-endianness))
        (native->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff))
  (eqv? (bytevector-u64-ref $v1 28 'big)
        (big-endian->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f))
  (eqv? (bytevector-u64-ref $v1 37 'little)
        (little-endian->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (eqv? (bytevector-u64-ref $v1 46 'big)
        (big-endian->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-ref $v1 55 'little)
        (little-endian->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff))
  (eqv? (bytevector-u64-ref $v1 65 'big)
        (big-endian->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80))
  (eqv? (bytevector-u64-ref $v1 74 'little)
        (little-endian->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89))
  (eqv? (bytevector-u64-ref $v1 83 (native-endianness))
        (native->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12))
  (eqv? (bytevector-u64-ref $v1 92 'big)
        (big-endian->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef))
  (eqv? (bytevector-u64-ref $v1 101 'little)
        (little-endian->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78))

  (do ([i 10000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
      (unless (eqv? (bytevector-u64-ref (apply bytevector #x3e ls) 1 (native-endianness))
                    (apply native->unsigned ls))
        (errorf #f "failed for ~s" ls))
      (unless (eqv? (bytevector-u64-ref (apply bytevector #x3e ls) 1 'big)
                    (apply big-endian->unsigned ls))
        (errorf #f "failed for ~s" ls))
      (unless (eqv? (bytevector-u64-ref (apply bytevector #x3e ls) 1 'little)
                    (apply little-endian->unsigned ls))
        (errorf #f "failed for ~s" ls))))
)

(mat bytevector-s64-set!
  (begin
    (define $v1 (make-bytevector 39 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-s64-set!))
  (error? (bytevector-s64-set! $v1))
  (error? (bytevector-s64-set! $v1 0))
  (error? (bytevector-s64-set! $v1 0 0))
  (error? (if (bytevector-s64-set! $v1 0 0 'big 15) #f #t))

 ; not a bytevector
  (error? (bytevector-s64-set! (make-vector 10) 0 0 'big))
  (error? (if (bytevector-s64-set! (make-vector 10) 0 0 'big) #f #t))

 ; invalid index
  (error? (bytevector-s64-set! $v1 -1 0 'big))
  (error? (bytevector-s64-set! $v1 32 0 'little))
  (error? (bytevector-s64-set! $v1 33 0 'big))
  (error? (bytevector-s64-set! $v1 34 0 'little))
  (error? (bytevector-s64-set! $v1 35 0 (native-endianness)))
  (error? (bytevector-s64-set! $v1 36 0 'big))
  (error? (bytevector-s64-set! $v1 37 0 'little))
  (error? (bytevector-s64-set! $v1 38 0 'big))
  (error? (bytevector-s64-set! $v1 39 0 'little))
  (error? (if (bytevector-s64-set! $v1 'q 0 (native-endianness)) #f #t))

 ; invalid value
  (error? (bytevector-s64-set! $v1 0 #x8000000000000000 'little))
  (error? (bytevector-s64-set! $v1 8 #x-8000000000000001 'big))
  (error? (if (bytevector-s64-set! $v1 16 "hello" (native-endianness)) #f #t))

 ; invalid endianness
  (error? (bytevector-s64-set! $v1 0 0 'gorgeous))
  (error? (bytevector-s64-set! $v1 0 0 '#(ravenous)))
  (error? (if (bytevector-s64-set! $v1 0 0 #t) #f #t))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

 ; (not bothering with native endianness, since it's either big or little)

 ; aligned accesses, endianness little
  (begin
    (bytevector-s64-set! $v1 0 0 'little)
    (bytevector-s64-set! $v1 8 -1 'little)
    (bytevector-s64-set! $v1 16
      (little-endian->signed #x7f #xff #xff #xff #xff #xff #xff #xff)
      'little)
    (bytevector-s64-set! $v1 24
      (little-endian->signed #xff #xff #xff #xff #xff #xff #xff #x7f)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #x7f
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s64-set! $v1 0
      (little-endian->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      'little)
    (bytevector-s64-set! $v1 8
      (little-endian->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)
      'little)
    (bytevector-s64-set! $v1 16
      (little-endian->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff)
      'little)
    (bytevector-s64-set! $v1 24
      (little-endian->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s64-set! $v1 0
      (little-endian->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89)
      'little)
    (bytevector-s64-set! $v1 8
      (little-endian->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12)
      'little)
    (bytevector-s64-set! $v1 16
      (little-endian->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef)
      'little)
    (bytevector-s64-set! $v1 24
      (little-endian->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; aligned accesses, endianness big
  (begin
    (bytevector-s64-set! $v1 0 0 'big)
    (bytevector-s64-set! $v1 8 -1 'big)
    (bytevector-s64-set! $v1 16
      (big-endian->signed #x7f #xff #xff #xff #xff #xff #xff #xff)
      'big)
    (bytevector-s64-set! $v1 24
      (big-endian->signed #xff #xff #xff #xff #xff #xff #xff #x7f)
      'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #x7f
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s64-set! $v1 0
      (little-endian->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      'little)
    (bytevector-s64-set! $v1 8
      (little-endian->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)
      'little)
    (bytevector-s64-set! $v1 16
      (little-endian->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff)
      'little)
    (bytevector-s64-set! $v1 24
      (little-endian->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-s64-set! $v1 0
      (little-endian->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89)
      'little)
    (bytevector-s64-set! $v1 8
      (little-endian->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12)
      'little)
    (bytevector-s64-set! $v1 16
      (little-endian->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef)
      'little)
    (bytevector-s64-set! $v1 24
      (little-endian->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; aligned accesses, endianness mixed
  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
        (bytevector-s64-set! v 0 (apply native->signed ls) (native-endianness))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-s64-set! v 0 (apply big-endian->signed (reverse ls)) 'big)
        (unless (equal? v (apply bytevector (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-s64-set! v 0 (apply little-endian->signed ls) 'little)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 36 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 1
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 10
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 19
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad)))) ; 28

  (begin
    (bytevector-s64-set! $v1 1 0 'big)
    (bytevector-s64-set! $v1 10 -1 'little)
    (bytevector-s64-set! $v1 19
      (big-endian->signed #x7f #xff #xff #xff #xff #xff #xff #xff)
      'big)
    (bytevector-s64-set! $v1 28
      (little-endian->signed #xff #xff #xff #xff #xff #xff #xff #x7f)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xad
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #xad
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xad
                        #xff #xff #xff #xff #xff #xff #xff #x7f))))

  (begin
    (define $v1 (make-bytevector 37 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 2
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 11
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 20
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad)))) ; 29

  (begin
    (bytevector-s64-set! $v1 2
      (little-endian->signed #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      'little)
    (bytevector-s64-set! $v1 11
      (big-endian->signed #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)
      'big)
    (bytevector-s64-set! $v1 20
      (big-endian->signed #x80 #x00 #x00 #x00 #xff #xff #xff #xff)
      'big)
    (bytevector-s64-set! $v1 29
      (little-endian->signed #xff #xff #xff #xff #x00 #x00 #x00 #x80)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad
                        #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xad
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #xad
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xad
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80))))

  (begin
    (define $v1 (make-bytevector 38 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 3
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 12
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 21
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad)))) ; 30

  (begin
    (bytevector-s64-set! $v1 3
      (big-endian->signed #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89)
      'big)
    (bytevector-s64-set! $v1 12
      (little-endian->signed #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12)
      'little)
    (bytevector-s64-set! $v1 21
      (little-endian->signed #x78 #x89 #x9a #xab #xbc #xcd #xde #xef)
      'little)
    (bytevector-s64-set! $v1 30
      (big-endian->signed #xef #xde #xcd #xbc #xab #x9a #x89 #x78)
      'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad
                        #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #xad
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #xad
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xad
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78))))

  (let ([v (make-bytevector 15)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([idx (fx+ (modulo i 7) 1)])
        (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
          (bytevector-fill! v #xc7)
          (bytevector-s64-set! v idx (apply native->signed ls) (native-endianness))
          (unless (equal? v
                    (apply bytevector
                      (append
                        (make-list idx #xc7)
                        ls
                        (make-list (fx- 7 idx) #xc7))))
            (errorf #f "failed for ~s (native)" ls))
          (bytevector-s64-set! v idx
            (apply big-endian->signed (reverse ls))
            'big)
          (unless (equal? v
                    (apply bytevector
                      (append
                        (make-list idx #xc7)
                        (reverse ls)
                        (make-list (fx- 7 idx) #xc7))))
            (errorf #f "failed for ~s (big)" ls))
          (bytevector-s64-set! v idx
            (apply little-endian->signed ls)
            'little)
          (unless (equal? v
                    (apply bytevector
                      (append
                        (make-list idx #xc7)
                        ls
                        (make-list (fx- 7 idx) #xc7))))
            (errorf #f "failed for ~s (little)" ls))))))
)

(mat bytevector-u64-set!
  (begin
    (define $v1 (make-bytevector 39 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; wrong argument count
  (error? (bytevector-u64-set!))
  (error? (bytevector-u64-set! $v1))
  (error? (bytevector-u64-set! $v1 0))
  (error? (bytevector-u64-set! $v1 0 0))
  (error? (if (bytevector-u64-set! $v1 0 0 'big 15) #f #t))

 ; not a bytevector
  (error? (bytevector-u64-set! (make-vector 10) 0 0 'big))
  (error? (if (bytevector-u64-set! (make-vector 10) 0 0 'big) #f #t))

 ; invalid index
  (error? (bytevector-u64-set! $v1 -1 0 'big))
  (error? (bytevector-u64-set! $v1 32 0 'little))
  (error? (bytevector-u64-set! $v1 33 0 'big))
  (error? (bytevector-u64-set! $v1 34 0 'little))
  (error? (bytevector-u64-set! $v1 35 0 (native-endianness)))
  (error? (bytevector-u64-set! $v1 36 0 'big))
  (error? (bytevector-u64-set! $v1 37 0 'little))
  (error? (bytevector-u64-set! $v1 38 0 'big))
  (error? (bytevector-u64-set! $v1 39 0 'little))
  (error? (if (bytevector-u64-set! $v1 'q 0 (native-endianness)) #f #t))

 ; invalid value
  (error? (bytevector-u64-set! $v1 0 #x10000000000000000 'little))
  (error? (bytevector-u64-set! $v1 8 #x-1 'big))
  (error? (if (bytevector-u64-set! $v1 16 "hello" (native-endianness)) #f #t))

 ; invalid endianness
  (error? (bytevector-u64-set! $v1 0 0 'gorgeous))
  (error? (bytevector-u64-set! $v1 0 0 '#(ravenous)))
  (error? (if (bytevector-u64-set! $v1 0 0 #t) #f #t))

 ; make sure no damage done
  (and
    (bytevector? $v1)
    (equal? $v1 '#vu8(#xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad #xad
                      #xad #xad #xad #xad #xad #xad #xad)))

 ; (not bothering with native endianness, since it's either big or little)

 ; aligned accesses, endianness little
  (begin
    (bytevector-u64-set! $v1 0 0 'little)
    (bytevector-u64-set! $v1 8 #xffffffffffffffff 'little)
    (bytevector-u64-set! $v1 16
      (little-endian->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff)
      'little)
    (bytevector-u64-set! $v1 24
      (little-endian->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #x7f
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u64-set! $v1 0
      (little-endian->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      'little)
    (bytevector-u64-set! $v1 8
      (little-endian->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)
      'little)
    (bytevector-u64-set! $v1 16
      (little-endian->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff)
      'little)
    (bytevector-u64-set! $v1 24
      (little-endian->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u64-set! $v1 0
      (little-endian->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89)
      'little)
    (bytevector-u64-set! $v1 8
      (little-endian->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12)
      'little)
    (bytevector-u64-set! $v1 16
      (little-endian->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef)
      'little)
    (bytevector-u64-set! $v1 24
      (little-endian->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; aligned accesses, endianness big
  (begin
    (bytevector-u64-set! $v1 0 0 'big)
    (bytevector-u64-set! $v1 8 #xffffffffffffffff 'big)
    (bytevector-u64-set! $v1 16
      (big-endian->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff)
      'big)
    (bytevector-u64-set! $v1 24
      (big-endian->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f)
      'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #x7f
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u64-set! $v1 0
      (little-endian->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      'little)
    (bytevector-u64-set! $v1 8
      (little-endian->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)
      'little)
    (bytevector-u64-set! $v1 16
      (little-endian->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff)
      'little)
    (bytevector-u64-set! $v1 24
      (little-endian->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80
                        #xad #xad #xad #xad #xad #xad #xad))))

  (begin
    (bytevector-u64-set! $v1 0
      (little-endian->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89)
      'little)
    (bytevector-u64-set! $v1 8
      (little-endian->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12)
      'little)
    (bytevector-u64-set! $v1 16
      (little-endian->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef)
      'little)
    (bytevector-u64-set! $v1 24
      (little-endian->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78
                        #xad #xad #xad #xad #xad #xad #xad))))

 ; aligned accesses, endianness mixed
  (let ([v (bytevector #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7 #xc7)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
        (bytevector-u64-set! v 0 (apply native->unsigned ls) (native-endianness))
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls))
        (bytevector-u64-set! v 0 (apply big-endian->unsigned (reverse ls)) 'big)
        (unless (equal? v (apply bytevector (reverse ls)))
          (errorf #f "failed for ~s" ls))
        (bytevector-u64-set! v 0 (apply little-endian->unsigned ls) 'little)
        (unless (equal? v (apply bytevector ls))
          (errorf #f "failed for ~s" ls)))))

 ; unaligned accesses, endianness mixed
  (begin
    (define $v1 (make-bytevector 36 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 1
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 10
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 19
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad)))) ; 28

  (begin
    (bytevector-u64-set! $v1 1 0 'big)
    (bytevector-u64-set! $v1 10 #xffffffffffffffff 'little)
    (bytevector-u64-set! $v1 19
      (big-endian->unsigned #x7f #xff #xff #xff #xff #xff #xff #xff)
      'big)
    (bytevector-u64-set! $v1 28
      (little-endian->unsigned #xff #xff #xff #xff #xff #xff #xff #x7f)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xad
                        #xff #xff #xff #xff #xff #xff #xff #xff
                        #xad
                        #x7f #xff #xff #xff #xff #xff #xff #xff
                        #xad
                        #xff #xff #xff #xff #xff #xff #xff #x7f))))

  (begin
    (define $v1 (make-bytevector 37 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 2
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 11
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 20
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad)))) ; 29

  (begin
    (bytevector-u64-set! $v1 2
      (little-endian->unsigned #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      'little)
    (bytevector-u64-set! $v1 11
      (big-endian->unsigned #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)
      'big)
    (bytevector-u64-set! $v1 20
      (big-endian->unsigned #x80 #x00 #x00 #x00 #xff #xff #xff #xff)
      'big)
    (bytevector-u64-set! $v1 29
      (little-endian->unsigned #xff #xff #xff #xff #x00 #x00 #x00 #x80)
      'little)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad
                        #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                        #xad
                        #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                        #xad
                        #x80 #x00 #x00 #x00 #xff #xff #xff #xff
                        #xad
                        #xff #xff #xff #xff #x00 #x00 #x00 #x80))))

  (begin
    (define $v1 (make-bytevector 38 #xad))
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 3
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 12
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad     ; 21
                        #xad
                        #xad #xad #xad #xad #xad #xad #xad #xad)))) ; 30

  (begin
    (bytevector-u64-set! $v1 3
      (big-endian->unsigned #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89)
      'big)
    (bytevector-u64-set! $v1 12
      (little-endian->unsigned #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12)
      'little)
    (bytevector-u64-set! $v1 21
      (little-endian->unsigned #x78 #x89 #x9a #xab #xbc #xcd #xde #xef)
      'little)
    (bytevector-u64-set! $v1 30
      (big-endian->unsigned #xef #xde #xcd #xbc #xab #x9a #x89 #x78)
      'big)
    (and
      (bytevector? $v1)
      (equal? $v1 '#vu8(#xad #xad #xad
                        #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
                        #xad
                        #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
                        #xad
                        #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
                        #xad
                        #xef #xde #xcd #xbc #xab #x9a #x89 #x78))))

  (let ([v (make-bytevector 15)])
    (do ([i 10000 (fx- i 1)])
        ((fx= i 0) #t)
      (let ([idx (fx+ (modulo i 7) 1)])
        (let ([ls (map (lambda (x) (random (expt 2 8))) (make-list 8))])
          (bytevector-fill! v #xc7)
          (bytevector-u64-set! v idx (apply native->unsigned ls) (native-endianness))
          (unless (equal? v
                    (apply bytevector
                      (append
                        (make-list idx #xc7)
                        ls
                        (make-list (fx- 7 idx) #xc7))))
            (errorf #f "failed for ~s (native)" ls))
          (bytevector-u64-set! v idx
            (apply big-endian->unsigned (reverse ls))
            'big)
          (unless (equal? v
                    (apply bytevector
                      (append
                        (make-list idx #xc7)
                        (reverse ls)
                        (make-list (fx- 7 idx) #xc7))))
            (errorf #f "failed for ~s (big)" ls))
          (bytevector-u64-set! v idx
            (apply little-endian->unsigned ls)
            'little)
          (unless (equal? v
                    (apply bytevector
                      (append
                        (make-list idx #xc7)
                        ls
                        (make-list (fx- 7 idx) #xc7))))
            (errorf #f "failed for ~s (little)" ls))))))
)

(mat bytevector-ieee-single-native-ref
  (begin
    (define $v1
      (case (native-endianness)
        [(little)
         '#vu8(#x00 #x00 #x00 #x00   ; 0.0
               #x00 #x00 #x00 #x00   ; 0.0   ; extra for consistent mat errors between big- and little-endian machines
               #x00 #x00 #x80 #x3f   ; 1.0
               #x00 #x00 #x80 #xbf   ; -1.0
               #x00 #x00 #xc0 #x3f   ; 1.5
               #x00 #x00 #xc0 #xbf   ; -1.5
               #xad #xe6 #xd5 #x65   ; #b1.10101011110011010101101e1001100
               #x00 #x00 #x80 #x7f   ; +inf.0
               #x00 #x00 #x80 #xff   ; -inf.0
               #x01 #x02 #x03)]
        [(big)
         '#vu8(#x00 #x00 #x00 #x00   ; 0.0
               #x00 #x00 #x00 #x00   ; 0.0   ; extra for consistent mat errors between big- and little-endian machines
               #x3f #x80 #x00 #x00   ; 1.0
               #xbf #x80 #x00 #x00   ; -1.0
               #x3f #xc0 #x00 #x00   ; 1.5
               #xbf #xc0 #x00 #x00   ; -1.5
               #x65 #xd5 #xe6 #xad   ; #b1.10101011110011010101101e1001100
               #x7f #x80 #x00 #x00   ; +inf.0
               #xff #x80 #x00 #x00   ; -inf.0
               #x01 #x02 #x03)]
        [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-ieee-single-native-ref))
  (error? (bytevector-ieee-single-native-ref $v1))
  (error? (if (bytevector-ieee-single-native-ref $v1 0 0) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-single-native-ref '#(3 252 5 0 0 0 0) 0))
  (error? (if (bytevector-ieee-single-native-ref '#(3 252 5 0 0 0 0) 0) #f #t))

 ; invalid index
  (error? (bytevector-ieee-single-native-ref $v1 -1))
  (error? (bytevector-ieee-single-native-ref $v1 1))
  (error? (bytevector-ieee-single-native-ref $v1 2))
  (error? (bytevector-ieee-single-native-ref $v1 3))
  (error? (bytevector-ieee-single-native-ref $v1 5))
  (error? (bytevector-ieee-single-native-ref $v1 6))
  (error? (bytevector-ieee-single-native-ref $v1 7))
  (error? (bytevector-ieee-single-native-ref $v1 9))
  (error? (bytevector-ieee-single-native-ref $v1 10))
  (error? (bytevector-ieee-single-native-ref $v1 11))
  (error? (bytevector-ieee-single-native-ref $v1 13))
  (error? (bytevector-ieee-single-native-ref $v1 14))
  (error? (bytevector-ieee-single-native-ref $v1 15))
  (error? (bytevector-ieee-single-native-ref $v1 17))
  (error? (bytevector-ieee-single-native-ref $v1 18))
  (error? (bytevector-ieee-single-native-ref $v1 19))
  (error? (bytevector-ieee-single-native-ref $v1 21))
  (error? (bytevector-ieee-single-native-ref $v1 22))
  (error? (bytevector-ieee-single-native-ref $v1 23))
  (error? (bytevector-ieee-single-native-ref $v1 25))
  (error? (bytevector-ieee-single-native-ref $v1 26))
  (error? (bytevector-ieee-single-native-ref $v1 27))
  (error? (bytevector-ieee-single-native-ref $v1 29))
  (error? (bytevector-ieee-single-native-ref $v1 30))
  (error? (bytevector-ieee-single-native-ref $v1 31))
  (error? (bytevector-ieee-single-native-ref $v1 33))
  (error? (bytevector-ieee-single-native-ref $v1 34))
  (error? (bytevector-ieee-single-native-ref $v1 35))
  (error? (bytevector-ieee-single-native-ref $v1 36))
  (error? (bytevector-ieee-single-native-ref $v1 37))
  (error? (bytevector-ieee-single-native-ref $v1 38))
  (error? (bytevector-ieee-single-native-ref $v1 39))
  (error? (if (bytevector-ieee-single-native-ref $v1 4.0) #f #t))

  (eqv? (bytevector-ieee-single-native-ref $v1 0) 0.0)
  (eqv? (bytevector-ieee-single-native-ref $v1 4) 0.0)
  (eqv? (bytevector-ieee-single-native-ref $v1 8) 1.0)
  (eqv? (bytevector-ieee-single-native-ref $v1 12) -1.0)
  (eqv? (bytevector-ieee-single-native-ref $v1 16) 1.5)
  (eqv? (bytevector-ieee-single-native-ref $v1 20) -1.5)
  (eqv? (bytevector-ieee-single-native-ref $v1 24) #b1.10101011110011010101101e1001100)
  (eqv? (bytevector-ieee-single-native-ref $v1 28) +inf.0)
  (eqv? (bytevector-ieee-single-native-ref $v1 32) -inf.0)
)

(mat bytevector-ieee-double-native-ref
  (begin
    (define $v1
      (case (native-endianness)
        [(little)
         '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0
               #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f   ; 1.0
               #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf   ; -1.0
               #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #x3f   ; 1.5
               #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #xbf   ; -1.5
               #xef #xcd #xab #x89 #x67 #x45 #xa3 #x9c   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101
               #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x7f   ; +inf.0
               #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xff   ; -inf.0
               #x01 #x02 #x03 #x04 #x05 #x06 #x07)]
        [(big)
         '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0
               #x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.0
               #xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.0
               #x3f #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.5
               #xbf #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.5
               #x9c #xa3 #x45 #x67 #x89 #xab #xcd #xef   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101
               #x7f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; +inf.0
               #xff #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; -inf.0
               #x01 #x02 #x03 #x04 #x05 #x06 #x07)]
        [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-ieee-double-native-ref))
  (error? (bytevector-ieee-double-native-ref $v1))
  (error? (if (bytevector-ieee-double-native-ref $v1 0 0) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-double-native-ref '#(3 252 5 0 0 0 0) 0))
  (error? (if (bytevector-ieee-double-native-ref '#(3 252 5 0 0 0 0) 0) #f #t))

 ; invalid index
  (error? (bytevector-ieee-double-native-ref $v1 -1))
  (error? (bytevector-ieee-double-native-ref $v1 1))
  (error? (bytevector-ieee-double-native-ref $v1 2))
  (error? (bytevector-ieee-double-native-ref $v1 3))
  (error? (bytevector-ieee-double-native-ref $v1 4))
  (error? (bytevector-ieee-double-native-ref $v1 5))
  (error? (bytevector-ieee-double-native-ref $v1 6))
  (error? (bytevector-ieee-double-native-ref $v1 7))
  (error? (bytevector-ieee-double-native-ref $v1 9))
  (error? (bytevector-ieee-double-native-ref $v1 10))
  (error? (bytevector-ieee-double-native-ref $v1 11))
  (error? (bytevector-ieee-double-native-ref $v1 12))
  (error? (bytevector-ieee-double-native-ref $v1 13))
  (error? (bytevector-ieee-double-native-ref $v1 14))
  (error? (bytevector-ieee-double-native-ref $v1 15))
  (error? (bytevector-ieee-double-native-ref $v1 17))
  (error? (bytevector-ieee-double-native-ref $v1 18))
  (error? (bytevector-ieee-double-native-ref $v1 19))
  (error? (bytevector-ieee-double-native-ref $v1 20))
  (error? (bytevector-ieee-double-native-ref $v1 21))
  (error? (bytevector-ieee-double-native-ref $v1 22))
  (error? (bytevector-ieee-double-native-ref $v1 23))
  (error? (bytevector-ieee-double-native-ref $v1 25))
  (error? (bytevector-ieee-double-native-ref $v1 26))
  (error? (bytevector-ieee-double-native-ref $v1 27))
  (error? (bytevector-ieee-double-native-ref $v1 28))
  (error? (bytevector-ieee-double-native-ref $v1 29))
  (error? (bytevector-ieee-double-native-ref $v1 30))
  (error? (bytevector-ieee-double-native-ref $v1 31))
  (error? (bytevector-ieee-double-native-ref $v1 33))
  (error? (bytevector-ieee-double-native-ref $v1 42))
  (error? (bytevector-ieee-double-native-ref $v1 51))
  (error? (bytevector-ieee-double-native-ref $v1 60))
  (error? (bytevector-ieee-double-native-ref $v1 69))
  (error? (bytevector-ieee-double-native-ref $v1 70))
  (error? (bytevector-ieee-double-native-ref $v1 71))
  (error? (if (bytevector-ieee-double-native-ref $v1 4.0) #f #t))

  (eqv? (bytevector-ieee-double-native-ref $v1 0) 0.0)
  (eqv? (bytevector-ieee-double-native-ref $v1 8) 1.0)
  (eqv? (bytevector-ieee-double-native-ref $v1 16) -1.0)
  (eqv? (bytevector-ieee-double-native-ref $v1 24) 1.5)
  (eqv? (bytevector-ieee-double-native-ref $v1 32) -1.5)
  (eqv? (bytevector-ieee-double-native-ref $v1 40) #b-1.0011010001010110011110001001101010111100110111101111e-1000110101)
  (eqv? (bytevector-ieee-double-native-ref $v1 48) +inf.0)
  (eqv? (bytevector-ieee-double-native-ref $v1 56) -inf.0)
)

(mat bytevector-ieee-single-native-set!
  (begin
    (define $v1 (make-bytevector 35 #xeb))
    (and (bytevector? $v1)
         (equal? $v1
           '#vu8(#xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb))))

 ; wrong argument count
  (error? (bytevector-ieee-single-native-set!))
  (error? (bytevector-ieee-single-native-set! $v1))
  (error? (bytevector-ieee-single-native-set! $v1 0))
  (error? (if (bytevector-ieee-single-native-set! $v1 0 0.0 0.0) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-single-native-set! '#(3 252 5 0 0 0 0) 0 0.0))
  (error? (if (bytevector-ieee-single-native-set! '#(3 252 5 0 0 0 0) 0 0.0) #f #t))

 ; invalid index
  (error? (bytevector-ieee-single-native-set! $v1 -1 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 1 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 2 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 3 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 5 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 6 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 7 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 9 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 10 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 11 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 13 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 14 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 15 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 17 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 18 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 19 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 21 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 22 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 23 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 25 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 26 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 27 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 29 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 30 0.0))
  (error? (bytevector-ieee-single-native-set! $v1 31 0.0))
  (error? (if (bytevector-ieee-single-native-set! $v1 4.0 0.0) #f #t))

 ; invalid value
  (error? (bytevector-ieee-single-native-set! $v1 0 1+2i))
  (error? (bytevector-ieee-single-native-set! $v1 0 1.0+3.0i))
  (error? (bytevector-ieee-single-native-set! $v1 0 1.0+0.0i))
  (error? (bytevector-ieee-single-native-set! $v1 0 1.0-0.0i))
  (error? (if (bytevector-ieee-single-native-set! $v1 0 "oops") #f #t))

 ; make sure no damage done
  (and (bytevector? $v1)
       (equal? $v1
         '#vu8(#xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb)))

  (begin
    (bytevector-ieee-single-native-set! $v1 0 0.0)
    (bytevector-ieee-single-native-set! $v1 4 1)
    (bytevector-ieee-single-native-set! $v1 8 -1)
    (bytevector-ieee-single-native-set! $v1 12 3/2)
    (bytevector-ieee-single-native-set! $v1 16 -3/2)
    (bytevector-ieee-single-native-set! $v1 20 #b1.10101011110011010101101e1001100)
    (bytevector-ieee-single-native-set! $v1 24 +inf.0)
    (bytevector-ieee-single-native-set! $v1 28 -inf.0)
    (and (bytevector? $v1)
         (equal? $v1
           (case (native-endianness)
             [(little)
              '#vu8(#x00 #x00 #x00 #x00   ; 0.0
                    #x00 #x00 #x80 #x3f   ; 1.0
                    #x00 #x00 #x80 #xbf   ; -1.0
                    #x00 #x00 #xc0 #x3f   ; 1.5
                    #x00 #x00 #xc0 #xbf   ; -1.5
                    #xad #xe6 #xd5 #x65   ; #b1.10101011110011010101101e1001100
                    #x00 #x00 #x80 #x7f   ; +inf.0
                    #x00 #x00 #x80 #xff   ; -inf.0
                    #xeb #xeb #xeb)]
             [(big)
              '#vu8(#x00 #x00 #x00 #x00   ; 0.0
                    #x3f #x80 #x00 #x00   ; 1.0
                    #xbf #x80 #x00 #x00   ; -1.0
                    #x3f #xc0 #x00 #x00   ; 1.5
                    #xbf #xc0 #x00 #x00   ; -1.5
                    #x65 #xd5 #xe6 #xad   ; #b1.10101011110011010101101e1001100
                    #x7f #x80 #x00 #x00   ; +inf.0
                    #xff #x80 #x00 #x00   ; -inf.0
                    #xeb #xeb #xeb)]
             [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))))
)

(mat bytevector-ieee-double-native-set!
  (begin
    (define $v1 (make-bytevector 71 #xeb))
    (and (bytevector? $v1)
         (equal? $v1
           '#vu8(#xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb))))

 ; wrong argument count
  (error? (bytevector-ieee-double-native-set!))
  (error? (bytevector-ieee-double-native-set! $v1))
  (error? (bytevector-ieee-double-native-set! $v1 0))
  (error? (if (bytevector-ieee-double-native-set! $v1 0 0.0 0.0) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-double-native-set! '#(3 252 5 0 0 0 0) 0 0.0))
  (error? (if (bytevector-ieee-double-native-set! '#(3 252 5 0 0 0 0) 0 0.0) #f #t))

 ; invalid index
  (error? (bytevector-ieee-double-native-set! $v1 -1 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 1 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 2 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 3 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 4 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 5 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 6 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 7 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 9 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 10 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 11 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 12 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 13 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 14 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 15 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 17 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 18 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 19 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 20 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 21 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 22 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 23 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 25 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 26 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 27 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 28 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 29 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 30 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 31 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 33 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 42 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 51 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 60 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 69 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 70 0.0))
  (error? (bytevector-ieee-double-native-set! $v1 71 0.0))
  (error? (if (bytevector-ieee-double-native-set! $v1 4.0 0.0) #f #t))

 ; invalid value
  (error? (bytevector-ieee-double-native-set! $v1 0 1+2i))
  (error? (bytevector-ieee-double-native-set! $v1 0 1.0-7.3i))
  (error? (bytevector-ieee-double-native-set! $v1 0 -i))
  (error? (bytevector-ieee-double-native-set! $v1 0 1.0+0.0i))
  (error? (bytevector-ieee-double-native-set! $v1 0 1.0-0.0i))
  (error? (if (bytevector-ieee-double-native-set! $v1 0 "oops") #f #t))

 ; make sure no damage done
  (and (bytevector? $v1)
       (equal? $v1
         '#vu8(#xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb)))

  (begin
    (bytevector-ieee-double-native-set! $v1 0 0.0)
    (bytevector-ieee-double-native-set! $v1 8 1)
    (bytevector-ieee-double-native-set! $v1 16 -1)
    (bytevector-ieee-double-native-set! $v1 24 3/2)
    (bytevector-ieee-double-native-set! $v1 32 -3/2)
    (bytevector-ieee-double-native-set! $v1 40 #b-1.0011010001010110011110001001101010111100110111101111e-1000110101)
    (bytevector-ieee-double-native-set! $v1 48 +inf.0)
    (bytevector-ieee-double-native-set! $v1 56 -inf.0)
    (and (bytevector? $v1)
         (equal? $v1
           (case (native-endianness)
             [(little)
              '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0
                    #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f   ; 1.0
                    #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf   ; -1.0
                    #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #x3f   ; 1.5
                    #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #xbf   ; -1.5
                    #xef #xcd #xab #x89 #x67 #x45 #xa3 #x9c   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101
                    #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x7f   ; +inf.0
                    #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xff   ; -inf.0
                    #xeb #xeb #xeb #xeb #xeb #xeb #xeb)]
             [(big)
              '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0
                    #x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.0
                    #xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.0
                    #x3f #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.5
                    #xbf #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.5
                    #x9c #xa3 #x45 #x67 #x89 #xab #xcd #xef   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101
                    #x7f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; +inf.0
                    #xff #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; -inf.0
                    #xeb #xeb #xeb #xeb #xeb #xeb #xeb)]
             [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))))
)

(mat bytevector-ieee-single-ref
  (begin
    (define $vlittle
      '#vu8(#x00 #x00 #x00 #x00   ; 0.0   ; 0
            #xc7
            #x00 #x00 #x00 #x00   ; 0.0   ; 5
            #xc7
            #x00 #x00 #x80 #x3f   ; 1.0   ; 10
            #xc7
            #x00 #x00 #x80 #xbf   ; -1.0  ; 15
            #xc7
            #x00 #x00 #xc0 #x3f   ; 1.5   ; 20
            #xc7
            #x00 #x00 #xc0 #xbf   ; -1.5  ; 25
            #xc7
            #xad #xe6 #xd5 #x65   ; #b1.10101011110011010101101e1001100 ; 30
            #xc7
            #x00 #x00 #x80 #x7f   ; +inf.0 ; 35
            #xc7
            #x00 #x00 #x80 #xff   ; -inf.0 ; 40
            #xc7))
    (define $vbig
      '#vu8(#x00 #x00 #x00 #x00   ; 0.0   ; 0
            #xc7
            #x00 #x00 #x00 #x00   ; 0.0   ; 5
            #xc7
            #x3f #x80 #x00 #x00   ; 1.0   ; 10
            #xc7
            #xbf #x80 #x00 #x00   ; -1.0  ; 15
            #xc7
            #x3f #xc0 #x00 #x00   ; 1.5   ; 20
            #xc7
            #xbf #xc0 #x00 #x00   ; -1.5  ; 25
            #xc7
            #x65 #xd5 #xe6 #xad   ; #b1.10101011110011010101101e1001100 ; 30
            #xc7
            #x7f #x80 #x00 #x00   ; +inf.0 ; 35
            #xc7
            #xff #x80 #x00 #x00   ; -inf.0 ; 40
            #xc7))
    (define $vnative
      (case (native-endianness)
        [(little) $vlittle]
        [(big) $vbig]
        [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))
    (andmap bytevector? (list $vlittle $vbig $vnative)))

 ; wrong argument count
  (error? (bytevector-ieee-single-ref))
  (error? (bytevector-ieee-single-ref $vnative))
  (error? (bytevector-ieee-single-ref $vnative 0))
  (error? (if (bytevector-ieee-single-ref $vnative 0 'big 0) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-single-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (if (bytevector-ieee-single-ref '#(3 252 5 0 0 0 0) 0 'big) #f #t))

 ; invalid index
  (error? (bytevector-ieee-single-ref $vnative -1 'big))
  (error? (bytevector-ieee-single-ref $vnative 42 'little))
  (error? (bytevector-ieee-single-ref $vnative 43 'big))
  (error? (bytevector-ieee-single-ref $vnative 44 (native-endianness)))
  (error? (bytevector-ieee-single-ref $vnative 45 'little))
  (error? (if (bytevector-ieee-single-ref $vnative 4.0 'big) #f #t))

 ; invalid endianness
  (error? (bytevector-ieee-single-ref $vnative 0 "nuts"))
  (error? (bytevector-ieee-single-ref $vnative 0 'crazy))
  (error? (if (bytevector-ieee-single-ref $vnative 0 35) #f #t))

  (eqv? (bytevector-ieee-single-ref $vnative 0 (native-endianness)) 0.0)
  (eqv? (bytevector-ieee-single-ref $vnative 5 (native-endianness)) 0.0)
  (eqv? (bytevector-ieee-single-ref $vnative 10 (native-endianness)) 1.0)
  (eqv? (bytevector-ieee-single-ref $vnative 15 (native-endianness)) -1.0)
  (eqv? (bytevector-ieee-single-ref $vnative 20 (native-endianness)) 1.5)
  (eqv? (bytevector-ieee-single-ref $vnative 25 (native-endianness)) -1.5)
  (eqv? (bytevector-ieee-single-ref $vnative 30 (native-endianness)) #b1.10101011110011010101101e1001100)
  (eqv? (bytevector-ieee-single-ref $vnative 35 (native-endianness)) +inf.0)
  (eqv? (bytevector-ieee-single-ref $vnative 40 (native-endianness)) -inf.0)

  (eqv? (bytevector-ieee-single-ref $vlittle 0 'little) 0.0)
  (eqv? (bytevector-ieee-single-ref $vlittle 5 'little) 0.0)
  (eqv? (bytevector-ieee-single-ref $vlittle 10 'little) 1.0)
  (eqv? (bytevector-ieee-single-ref $vlittle 15 'little) -1.0)
  (eqv? (bytevector-ieee-single-ref $vlittle 20 'little) 1.5)
  (eqv? (bytevector-ieee-single-ref $vlittle 25 'little) -1.5)
  (eqv? (bytevector-ieee-single-ref $vlittle 30 'little) #b1.10101011110011010101101e1001100)
  (eqv? (bytevector-ieee-single-ref $vlittle 35 'little) +inf.0)
  (eqv? (bytevector-ieee-single-ref $vlittle 40 'little) -inf.0)

  (eqv? (bytevector-ieee-single-ref $vbig 0 'big) 0.0)
  (eqv? (bytevector-ieee-single-ref $vbig 5 'big) 0.0)
  (eqv? (bytevector-ieee-single-ref $vbig 10 'big) 1.0)
  (eqv? (bytevector-ieee-single-ref $vbig 15 'big) -1.0)
  (eqv? (bytevector-ieee-single-ref $vbig 20 'big) 1.5)
  (eqv? (bytevector-ieee-single-ref $vbig 25 'big) -1.5)
  (eqv? (bytevector-ieee-single-ref $vbig 30 'big) #b1.10101011110011010101101e1001100)
  (eqv? (bytevector-ieee-single-ref $vbig 35 'big) +inf.0)
  (eqv? (bytevector-ieee-single-ref $vbig 40 'big) -inf.0)
)

(mat bytevector-ieee-double-ref
  (begin
    (define $vlittle
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0    ; 0
            #xed
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f   ; 1.0    ; 9
            #xed
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf   ; -1.0   ; 18
            #xed
            #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #x3f   ; 1.5    ; 27
            #xed
            #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #xbf   ; -1.5   ; 36
            #xed
            #xef #xcd #xab #x89 #x67 #x45 #xa3 #x9c   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101 ; 45
            #xed
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x7f   ; +inf.0 ; 54
            #xed
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xff)) ; -inf.0 ; 63
    (define $vbig
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0    ; 0
            #xed
            #x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.0    ; 9
            #xed
            #xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.0   ; 18
            #xed
            #x3f #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.5    ; 27
            #xed
            #xbf #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.5   ; 36
            #xed
            #x9c #xa3 #x45 #x67 #x89 #xab #xcd #xef   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101 ; 45
            #xed
            #x7f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; +inf.0 ; 54
            #xed
            #xff #xf0 #x00 #x00 #x00 #x00 #x00 #x00)) ; -inf.0 ; 63
    (define $vnative
      (case (native-endianness)
        [(little) $vlittle]
        [(big) $vbig]
        [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))
    (andmap bytevector? (list $vlittle $vbig $vnative)))

 ; wrong argument count
  (error? (bytevector-ieee-double-ref))
  (error? (bytevector-ieee-double-ref $vnative))
  (error? (bytevector-ieee-double-ref $vnative 0))
  (error? (if (bytevector-ieee-double-ref $vnative 0 'big 0) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-double-ref '#(3 252 5 0 0 0 0) 0 'big))
  (error? (if (bytevector-ieee-double-ref '#(3 252 5 0 0 0 0) 0 'big) #f #t))

 ; invalid index
  (error? (bytevector-ieee-double-ref $vnative -1 'big))
  (error? (bytevector-ieee-double-ref $vnative 64 'big))
  (error? (bytevector-ieee-double-ref $vnative 65 (native-endianness)))
  (error? (bytevector-ieee-double-ref $vnative 66 'little))
  (error? (bytevector-ieee-double-ref $vnative 67 'big))
  (error? (bytevector-ieee-double-ref $vnative 68 (native-endianness)))
  (error? (bytevector-ieee-double-ref $vnative 69 'little))
  (error? (bytevector-ieee-double-ref $vnative 70 'big))
  (error? (bytevector-ieee-double-ref $vnative 71 'little))
  (error? (if (bytevector-ieee-double-ref $vnative 4.0 'big) #f #t))

 ; invalid endianness
  (error? (bytevector-ieee-double-ref $vnative 0 "nuts"))
  (error? (bytevector-ieee-double-ref $vnative 0 'crazy))
  (error? (if (bytevector-ieee-double-ref $vnative 0 35) #f #t))

  (eqv? (bytevector-ieee-double-ref $vnative 0 (native-endianness)) 0.0)
  (eqv? (bytevector-ieee-double-ref $vnative 9 (native-endianness)) 1.0)
  (eqv? (bytevector-ieee-double-ref $vnative 18 (native-endianness)) -1.0)
  (eqv? (bytevector-ieee-double-ref $vnative 27 (native-endianness)) 1.5)
  (eqv? (bytevector-ieee-double-ref $vnative 36 (native-endianness)) -1.5)
  (eqv? (bytevector-ieee-double-ref $vnative 45 (native-endianness)) #b-1.0011010001010110011110001001101010111100110111101111e-1000110101)
  (eqv? (bytevector-ieee-double-ref $vnative 54 (native-endianness)) +inf.0)
  (eqv? (bytevector-ieee-double-ref $vnative 63 (native-endianness)) -inf.0)

  (eqv? (bytevector-ieee-double-ref $vlittle 0 'little) 0.0)
  (eqv? (bytevector-ieee-double-ref $vlittle 9 'little) 1.0)
  (eqv? (bytevector-ieee-double-ref $vlittle 18 'little) -1.0)
  (eqv? (bytevector-ieee-double-ref $vlittle 27 'little) 1.5)
  (eqv? (bytevector-ieee-double-ref $vlittle 36 'little) -1.5)
  (eqv? (bytevector-ieee-double-ref $vlittle 45 'little) #b-1.0011010001010110011110001001101010111100110111101111e-1000110101)
  (eqv? (bytevector-ieee-double-ref $vlittle 54 'little) +inf.0)
  (eqv? (bytevector-ieee-double-ref $vlittle 63 'little) -inf.0)

  (eqv? (bytevector-ieee-double-ref $vbig 0 'big) 0.0)
  (eqv? (bytevector-ieee-double-ref $vbig 9 'big) 1.0)
  (eqv? (bytevector-ieee-double-ref $vbig 18 'big) -1.0)
  (eqv? (bytevector-ieee-double-ref $vbig 27 'big) 1.5)
  (eqv? (bytevector-ieee-double-ref $vbig 36 'big) -1.5)
  (eqv? (bytevector-ieee-double-ref $vbig 45 'big) #b-1.0011010001010110011110001001101010111100110111101111e-1000110101)
  (eqv? (bytevector-ieee-double-ref $vbig 54 'big) +inf.0)
  (eqv? (bytevector-ieee-double-ref $vbig 63 'big) -inf.0)
)

(mat bytevector-ieee-single-set!
  (begin
    (define $v1 (make-bytevector 39 #xeb))
    (and (bytevector? $v1)
         (equal? $v1
           '#vu8(#xeb #xeb #xeb #xeb     ; 0
                 #xeb
                 #xeb #xeb #xeb #xeb     ; 5
                 #xeb
                 #xeb #xeb #xeb #xeb     ; 10
                 #xeb
                 #xeb #xeb #xeb #xeb     ; 15
                 #xeb
                 #xeb #xeb #xeb #xeb     ; 20
                 #xeb
                 #xeb #xeb #xeb #xeb     ; 25
                 #xeb
                 #xeb #xeb #xeb #xeb     ; 30
                 #xeb
                 #xeb #xeb #xeb #xeb)))) ; 35

 ; wrong argument count
  (error? (bytevector-ieee-single-set!))
  (error? (bytevector-ieee-single-set! $v1))
  (error? (bytevector-ieee-single-set! $v1 0))
  (error? (bytevector-ieee-single-set! $v1 0 0.0))
  (error? (if (bytevector-ieee-single-set! $v1 0 0.0 'big 'bigger) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-single-set! '#(3 252 5 0 0 0 0) 0 0.0 'little))
  (error? (if (bytevector-ieee-single-set! '#(3 252 5 0 0 0 0) 0 0.0 'little) #f #t))

 ; invalid index
  (error? (bytevector-ieee-single-set! $v1 -1 0.0 'little))
  (error? (bytevector-ieee-single-set! $v1 36 0.0 'little))
  (error? (bytevector-ieee-single-set! $v1 37 0.0 'big))
  (error? (bytevector-ieee-single-set! $v1 38 0.0 'big))
  (error? (bytevector-ieee-single-set! $v1 39 0.0 'little))
  (error? (if (bytevector-ieee-single-set! $v1 4.0 0.0 (native-endianness)) #f #t))

 ; invalid value
  (error? (bytevector-ieee-single-set! $v1 0 1+2i 'big))
  (error? (bytevector-ieee-single-set! $v1 0 1.0+3.0i 'little))
  (error? (bytevector-ieee-single-set! $v1 0 1.0+0.0i 'big))
  (error? (bytevector-ieee-single-set! $v1 0 1.0-0.0i (native-endianness)))
  (error? (if (bytevector-ieee-single-set! $v1 0 "oops" 'little) #f #t))

 ; invalid endianness
  (error? (bytevector-ieee-single-set! $v1 0 0.0 "ouch"))
  (error? (bytevector-ieee-single-set! $v1 0 0.0 'what?))
  (error? (if (bytevector-ieee-single-set! $v1 0 0.0 #\newline) #f #t))

 ; make sure no damage done
  (and (bytevector? $v1)
       (equal? $v1
         '#vu8(#xeb #xeb #xeb #xeb
               #xeb
               #xeb #xeb #xeb #xeb
               #xeb
               #xeb #xeb #xeb #xeb
               #xeb
               #xeb #xeb #xeb #xeb
               #xeb
               #xeb #xeb #xeb #xeb
               #xeb
               #xeb #xeb #xeb #xeb
               #xeb
               #xeb #xeb #xeb #xeb
               #xeb
               #xeb #xeb #xeb #xeb)))

  (begin
    (define $vlittle
      '#vu8(#x00 #x00 #x00 #x00   ; 0.0
            #xeb
            #x00 #x00 #x80 #x3f   ; 1.0
            #xeb
            #x00 #x00 #x80 #xbf   ; -1.0
            #xeb
            #x00 #x00 #xc0 #x3f   ; 1.5
            #xeb
            #x00 #x00 #xc0 #xbf   ; -1.5
            #xeb
            #xad #xe6 #xd5 #x65   ; #b1.10101011110011010101101e1001100
            #xeb
            #x00 #x00 #x80 #x7f   ; +inf.0
            #xeb
            #x00 #x00 #x80 #xff)) ; -inf.0

    (define $vbig
      '#vu8(#x00 #x00 #x00 #x00   ; 0.0
            #xeb
            #x3f #x80 #x00 #x00   ; 1.0
            #xeb
            #xbf #x80 #x00 #x00   ; -1.0
            #xeb
            #x3f #xc0 #x00 #x00   ; 1.5
            #xeb
            #xbf #xc0 #x00 #x00   ; -1.5
            #xeb
            #x65 #xd5 #xe6 #xad   ; #b1.10101011110011010101101e1001100
            #xeb
            #x7f #x80 #x00 #x00   ; +inf.0
            #xeb
            #xff #x80 #x00 #x00)) ; -inf.0

    (define $vnative
      (case (native-endianness)
        [(little) $vlittle]
        [(big) $vbig]
        [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))
    (andmap bytevector? (list $vlittle $vbig $vnative)))

  (begin
    (bytevector-ieee-single-set! $v1 0 0.0 (native-endianness))
    (bytevector-ieee-single-set! $v1 5 1 (native-endianness))
    (bytevector-ieee-single-set! $v1 10 -1 (native-endianness))
    (bytevector-ieee-single-set! $v1 15 3/2 (native-endianness))
    (bytevector-ieee-single-set! $v1 20 -3/2 (native-endianness))
    (bytevector-ieee-single-set! $v1 25 #b1.10101011110011010101101e1001100 (native-endianness))
    (bytevector-ieee-single-set! $v1 30 +inf.0 (native-endianness))
    (bytevector-ieee-single-set! $v1 35 -inf.0 (native-endianness))
    (and (bytevector? $v1) (equal? $v1 $vnative)))

  (begin
    (bytevector-ieee-single-set! $v1 0 0.0 'little)
    (bytevector-ieee-single-set! $v1 5 1 'little)
    (bytevector-ieee-single-set! $v1 10 -1 'little)
    (bytevector-ieee-single-set! $v1 15 3/2 'little)
    (bytevector-ieee-single-set! $v1 20 -3/2 'little)
    (bytevector-ieee-single-set! $v1 25 #b1.10101011110011010101101e1001100 'little)
    (bytevector-ieee-single-set! $v1 30 +inf.0 'little)
    (bytevector-ieee-single-set! $v1 35 -inf.0 'little)
    (and (bytevector? $v1) (equal? $v1 $vlittle)))

  (begin
    (bytevector-ieee-single-set! $v1 0 0.0 'big)
    (bytevector-ieee-single-set! $v1 5 1 'big)
    (bytevector-ieee-single-set! $v1 10 -1 'big)
    (bytevector-ieee-single-set! $v1 15 3/2 'big)
    (bytevector-ieee-single-set! $v1 20 -3/2 'big)
    (bytevector-ieee-single-set! $v1 25 #b1.10101011110011010101101e1001100 'big)
    (bytevector-ieee-single-set! $v1 30 +inf.0 'big)
    (bytevector-ieee-single-set! $v1 35 -inf.0 'big)
    (and (bytevector? $v1) (equal? $v1 $vbig)))
)

(mat bytevector-ieee-double-set!
  (begin
    (define $v1 (make-bytevector 71 #xeb))
    (and (bytevector? $v1)
         (equal? $v1
           '#vu8(#xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 0
                 #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 9
                 #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 18
                 #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 27
                 #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 36
                 #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 45
                 #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 54
                 #xeb
                 #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb)))) ; 63

 ; wrong argument count
  (error? (bytevector-ieee-double-set!))
  (error? (bytevector-ieee-double-set! $v1))
  (error? (bytevector-ieee-double-set! $v1 0))
  (error? (bytevector-ieee-double-set! $v1 0 0.0))
  (error? (if (bytevector-ieee-double-set! $v1 0 0.0 'big 'bigger) #f #t))

 ; not a bytevector
  (error? (bytevector-ieee-double-set! '#(3 252 5 0 0 0 0) 0 0.0 'little))
  (error? (if (bytevector-ieee-double-set! '#(3 252 5 0 0 0 0) 0 0.0 'little) #f #t))

 ; invalid index
  (error? (bytevector-ieee-double-set! $v1 -1 0.0 'little))
  (error? (bytevector-ieee-double-set! $v1 64 0.0 'little))
  (error? (bytevector-ieee-double-set! $v1 65 0.0 'little))
  (error? (bytevector-ieee-double-set! $v1 66 0.0 'little))
  (error? (bytevector-ieee-double-set! $v1 67 0.0 'little))
  (error? (bytevector-ieee-double-set! $v1 68 0.0 'little))
  (error? (bytevector-ieee-double-set! $v1 69 0.0 'big))
  (error? (bytevector-ieee-double-set! $v1 70 0.0 'big))
  (error? (bytevector-ieee-double-set! $v1 71 0.0 'little))
  (error? (if (bytevector-ieee-double-set! $v1 4.0 0.0 (native-endianness)) #f #t))

 ; invalid value
  (error? (bytevector-ieee-double-set! $v1 0 1+2i 'big))
  (error? (bytevector-ieee-double-set! $v1 0 1.0+3.0i 'little))
  (error? (bytevector-ieee-double-set! $v1 0 1.0+0.0i 'big))
  (error? (bytevector-ieee-double-set! $v1 0 1.0-0.0i (native-endianness)))
  (error? (if (bytevector-ieee-double-set! $v1 0 "oops" 'little) #f #t))

 ; invalid endianness
  (error? (bytevector-ieee-double-set! $v1 0 0.0 "ouch"))
  (error? (bytevector-ieee-double-set! $v1 0 0.0 'what?))
  (error? (if (bytevector-ieee-double-set! $v1 0 0.0 #\newline) #f #t))

 ; make sure no damage done
  (and (bytevector? $v1)
       (equal? $v1
         '#vu8(#xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 0
               #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 9
               #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 18
               #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 27
               #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 36
               #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 45
               #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb     ; 54
               #xeb
               #xeb #xeb #xeb #xeb #xeb #xeb #xeb #xeb)))  ; 63

  (begin
    (define $vlittle
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0
            #xeb
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f   ; 1.0
            #xeb
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf   ; -1.0
            #xeb
            #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #x3f   ; 1.5
            #xeb
            #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #xbf   ; -1.5
            #xeb
            #xef #xcd #xab #x89 #x67 #x45 #xa3 #x9c   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101
            #xeb
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x7f   ; +inf.0
            #xeb
            #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xff)) ; -inf.0

    (define $vbig
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; 0.0
            #xeb
            #x3f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.0
            #xeb
            #xbf #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.0
            #xeb
            #x3f #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; 1.5
            #xeb
            #xbf #xf8 #x00 #x00 #x00 #x00 #x00 #x00   ; -1.5
            #xeb
            #x9c #xa3 #x45 #x67 #x89 #xab #xcd #xef   ; #b-1.0011010001010110011110001001101010111100110111101111e-1000110101
            #xeb
            #x7f #xf0 #x00 #x00 #x00 #x00 #x00 #x00   ; +inf.0
            #xeb
            #xff #xf0 #x00 #x00 #x00 #x00 #x00 #x00)) ; -inf.0

    (define $vnative
      (case (native-endianness)
        [(little) $vlittle]
        [(big) $vbig]
        [else (errorf #f "mat doesn't handled endianness ~s" (native-endianness))]))
    (andmap bytevector? (list $vlittle $vbig $vnative)))

  (begin
    (bytevector-ieee-double-set! $v1 0 0.0 (native-endianness))
    (bytevector-ieee-double-set! $v1 9 1 (native-endianness))
    (bytevector-ieee-double-set! $v1 18 -1 (native-endianness))
    (bytevector-ieee-double-set! $v1 27 3/2 (native-endianness))
    (bytevector-ieee-double-set! $v1 36 -3/2 (native-endianness))
    (bytevector-ieee-double-set! $v1 45 #b-1.0011010001010110011110001001101010111100110111101111e-1000110101 (native-endianness))
    (bytevector-ieee-double-set! $v1 54 +inf.0 (native-endianness))
    (bytevector-ieee-double-set! $v1 63 -inf.0 (native-endianness))
    (and (bytevector? $v1) (equal? $v1 $vnative)))

  (begin
    (bytevector-ieee-double-set! $v1 0 0.0 'big)
    (bytevector-ieee-double-set! $v1 9 1 'big)
    (bytevector-ieee-double-set! $v1 18 -1 'big)
    (bytevector-ieee-double-set! $v1 27 3/2 'big)
    (bytevector-ieee-double-set! $v1 36 -3/2 'big)
    (bytevector-ieee-double-set! $v1 45 #b-1.0011010001010110011110001001101010111100110111101111e-1000110101 'big)
    (bytevector-ieee-double-set! $v1 54 +inf.0 'big)
    (bytevector-ieee-double-set! $v1 63 -inf.0 'big)
    (and (bytevector? $v1) (equal? $v1 $vbig)))

  (begin
    (bytevector-ieee-double-set! $v1 0 0.0 'little)
    (bytevector-ieee-double-set! $v1 9 1 'little)
    (bytevector-ieee-double-set! $v1 18 -1 'little)
    (bytevector-ieee-double-set! $v1 27 3/2 'little)
    (bytevector-ieee-double-set! $v1 36 -3/2 'little)
    (bytevector-ieee-double-set! $v1 45 #b-1.0011010001010110011110001001101010111100110111101111e-1000110101 'little)
    (bytevector-ieee-double-set! $v1 54 +inf.0 'little)
    (bytevector-ieee-double-set! $v1 63 -inf.0 'little)
    (and (bytevector? $v1) (equal? $v1 $vlittle)))
)

(mat bytevector-sint-ref
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-sint-ref))
  (error? (bytevector-sint-ref $v1))
  (error? (bytevector-sint-ref $v1 0))
  (error? (bytevector-sint-ref $v1 0 'big))
  (error? (if (bytevector-sint-ref $v1 0 'big 5 0) #f #t))

 ; not a bytevector
  (error? (bytevector-sint-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little 1))
  (error? (if (bytevector-sint-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little 1) #f #t))

 ; invalid index
  (error? (bytevector-sint-ref $v1 -1 'big 1))
  (error? (bytevector-sint-ref $v1 -1 'big 2))
  (error? (bytevector-sint-ref $v1 -1 'big 3))
  (error? (bytevector-sint-ref $v1 -1 'big 4))
  (error? (bytevector-sint-ref $v1 -1 'big 8))
  (error? (bytevector-sint-ref $v1 -1 'big 9))
  (error? (if (bytevector-sint-ref $v1 -1 'big 10) #f #t))

  (error? (bytevector-sint-ref $v1 96 'little 8))
  (error? (bytevector-sint-ref $v1 96 'little 9))
  (error? (bytevector-sint-ref $v1 97 'big 7))
  (error? (bytevector-sint-ref $v1 98 'little 6))
  (error? (bytevector-sint-ref $v1 99 'big 5))
  (error? (bytevector-sint-ref $v1 100 'big 4))
  (error? (bytevector-sint-ref $v1 100 'big 5))
  (error? (bytevector-sint-ref $v1 100 'big 8))
  (error? (bytevector-sint-ref $v1 101 'big 3))
  (error? (bytevector-sint-ref $v1 101 'little 4))
  (error? (bytevector-sint-ref $v1 102 'little 2))
  (error? (bytevector-sint-ref $v1 102 'big 3))
  (error? (bytevector-sint-ref $v1 103 'big 1))
  (error? (bytevector-sint-ref $v1 103 'big 2))
  (error? (bytevector-sint-ref $v1 103 'big 3))
  (error? (if (bytevector-sint-ref $v1 4.0 (native-endianness) 3) #f #t))

 ; invalid endianness
  (error? (bytevector-sint-ref $v1 0 'bonkers 1))
  (error? (bytevector-sint-ref $v1 0 'bonkers 2))
  (error? (bytevector-sint-ref $v1 0 'bonkers 3))
  (error? (bytevector-sint-ref $v1 0 'bonkers 4))
  (error? (bytevector-sint-ref $v1 0 'bonkers 8))
  (error? (if (bytevector-sint-ref $v1 0 'bonkers 35) #f #t))

 ; invalid size
  (error? (bytevector-sint-ref $v1 0 'little 0))
  (error? (bytevector-sint-ref $v1 1 'big -1))
  (error? (if (bytevector-sint-ref $v1 4 'little 'byte) #f #t))

 ; constant args
  (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
    (let ()
      (define-syntax a
        (lambda (x)
          (define (sublist ls i j)
            (list-head (list-tail ls i) j))
          (let* ([ls '(1 254 3 252 5 250 7 249 8 248
                       9 247 10 246 40 216 80 176 100 156)]
                 [n (length ls)])
            #`(let ()
                (define v '#,(apply bytevector ls))
                (list #,@(let f ([i 0])
                          (if (fx= i n)
                              '()
                              #`((list #,@(let g ([j 1])
                                           (if (fx<= j (fx- n i))
                                               #`((eqv? (bytevector-sint-ref v #,i 'little #,j)
                                                        #,(apply little-endian->signed (sublist ls i j)))
                                                  (eqv? (bytevector-sint-ref v #,i 'big #,j)
                                                        #,(apply big-endian->signed (sublist ls i j)))
                                                  #,@(g (fx+ j 1)))
                                               '())))
                                 #,@(f (fx+ i 1))))))))))
      a))

 ; nonconstant args
  (do ([i 100 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random 256)) (make-list (random 25)))])
      (unless (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
                (let ()
                  (define (sublist ls i j)
                    (list-head (list-tail ls i) j))
                  (let ([n (length ls)])
                    (define v (apply bytevector ls))
                    (let f ([i 0])
                      (if (fx= i n)
                          '()
                          (cons (let g ([j 1])
                                  (if (fx<= j (fx- n i))
                                      (cons*
                                        (eqv? (bytevector-sint-ref v i 'little j)
                                              (apply little-endian->signed (sublist ls i j)))
                                        (eqv? (bytevector-sint-ref v i 'big j)
                                              (apply big-endian->signed (sublist ls i j)))
                                        (g (fx+ j 1)))
                                      '()))
                                (f (fx+ i 1))))))))
        (pretty-print ls)
        (errorf #f "failed for for ~s" ls))))
)

(mat bytevector-uint-ref
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-uint-ref))
  (error? (bytevector-uint-ref $v1))
  (error? (bytevector-uint-ref $v1 0))
  (error? (bytevector-uint-ref $v1 0 'big))
  (error? (if (bytevector-uint-ref $v1 0 'big 5 0) #f #t))

 ; not a bytevector
  (error? (bytevector-uint-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little 1))
  (error? (if (bytevector-uint-ref '#(3 252 5 0 0 0 0 0 0 0 0) 0 'little 1) #f #t))

 ; invalid index
  (error? (bytevector-uint-ref $v1 -1 'big 1))
  (error? (bytevector-uint-ref $v1 -1 'big 2))
  (error? (bytevector-uint-ref $v1 -1 'big 3))
  (error? (bytevector-uint-ref $v1 -1 'big 4))
  (error? (bytevector-uint-ref $v1 -1 'big 8))
  (error? (bytevector-uint-ref $v1 -1 'big 9))
  (error? (if (bytevector-uint-ref $v1 -1 'big 10) #f #t))

  (error? (bytevector-uint-ref $v1 96 'little 8))
  (error? (bytevector-uint-ref $v1 96 'little 9))
  (error? (bytevector-uint-ref $v1 97 'big 7))
  (error? (bytevector-uint-ref $v1 98 'little 6))
  (error? (bytevector-uint-ref $v1 99 'big 5))
  (error? (bytevector-uint-ref $v1 100 'big 4))
  (error? (bytevector-uint-ref $v1 100 'big 5))
  (error? (bytevector-uint-ref $v1 100 'big 8))
  (error? (bytevector-uint-ref $v1 101 'big 3))
  (error? (bytevector-uint-ref $v1 101 'little 4))
  (error? (bytevector-uint-ref $v1 102 'little 2))
  (error? (bytevector-uint-ref $v1 102 'big 3))
  (error? (bytevector-uint-ref $v1 103 'big 1))
  (error? (bytevector-uint-ref $v1 103 'big 2))
  (error? (bytevector-uint-ref $v1 103 'big 3))
  (error? (if (bytevector-uint-ref $v1 4.0 (native-endianness) 3) #f #t))

 ; invalid endianness
  (error? (bytevector-uint-ref $v1 0 'bonkers 1))
  (error? (bytevector-uint-ref $v1 0 'bonkers 2))
  (error? (bytevector-uint-ref $v1 0 'bonkers 3))
  (error? (bytevector-uint-ref $v1 0 'bonkers 4))
  (error? (bytevector-uint-ref $v1 0 'bonkers 8))
  (error? (if (bytevector-uint-ref $v1 0 'bonkers 35) #f #t))

 ; invalid size
  (error? (bytevector-uint-ref $v1 0 'little 0))
  (error? (bytevector-uint-ref $v1 0 'little (+ (bytevector-length $v1) 1)))
  (error? (bytevector-uint-ref $v1 7 'little (- (bytevector-length $v1) 6)))
  (error? (bytevector-uint-ref #vu8(1 2 3 4) 0 'big 32))
  (error? (bytevector-uint-ref $v1 1 'big -1))
  (error? (if (bytevector-uint-ref $v1 4 'little 'byte) #f #t))

 ; constant args
  (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
    (let ()
      (define-syntax a
        (lambda (x)
          (define (sublist ls i j)
            (list-head (list-tail ls i) j))
          (let* ([ls '(1 254 3 252 5 250 7 249 8 248
                       9 247 10 246 40 216 80 176 100 156)]
                 [n (length ls)])
            #`(let ()
                (define v '#,(apply bytevector ls))
                (list #,@(let f ([i 0])
                          (if (fx= i n)
                              '()
                              #`((list #,@(let g ([j 1])
                                           (if (fx<= j (fx- n i))
                                               #`((eqv? (bytevector-uint-ref v #,i 'little #,j)
                                                        #,(apply little-endian->unsigned (sublist ls i j)))
                                                  (eqv? (bytevector-uint-ref v #,i 'big #,j)
                                                        #,(apply big-endian->unsigned (sublist ls i j)))
                                                  #,@(g (fx+ j 1)))
                                               '())))
                                 #,@(f (fx+ i 1))))))))))
      a))

 ; nonconstant args
  (do ([i 100 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random 256)) (make-list (random 25)))])
      (unless (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
                (let ()
                  (define (sublist ls i j)
                    (list-head (list-tail ls i) j))
                  (let ([n (length ls)])
                    (define v (apply bytevector ls))
                    (let f ([i 0])
                      (if (fx= i n)
                          '()
                          (cons (let g ([j 1])
                                  (if (fx<= j (fx- n i))
                                      (cons*
                                        (eqv? (bytevector-uint-ref v i 'little j)
                                              (apply little-endian->unsigned (sublist ls i j)))
                                        (eqv? (bytevector-uint-ref v i 'big j)
                                              (apply big-endian->unsigned (sublist ls i j)))
                                        (g (fx+ j 1)))
                                      '()))
                                (f (fx+ i 1))))))))
        (pretty-print ls)
        (errorf #f "failed for for ~s" ls))))
)

(mat bytevector-sint-set!
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-sint-set!))
  (error? (bytevector-sint-set! $v1))
  (error? (bytevector-sint-set! $v1 0))
  (error? (bytevector-sint-set! $v1 0 7))
  (error? (bytevector-sint-set! $v1 0 7 'big))
  (error? (if (bytevector-sint-set! $v1 0 7 'big 5 0) #f #t))

 ; not a bytevector
  (error? (bytevector-sint-set! '#(3 252 5 0 0 0 0 0 0 0 0) 0 7 'little 1))
  (error? (if (bytevector-sint-set! '#(3 252 5 0 0 0 0 0 0 0 0) 0 7 'little 1) #f #t))

 ; invalid index
  (error? (bytevector-sint-set! $v1 -1 7 'big 1))
  (error? (bytevector-sint-set! $v1 -1 7 'big 2))
  (error? (bytevector-sint-set! $v1 -1 7 'big 3))
  (error? (bytevector-sint-set! $v1 -1 7 'big 4))
  (error? (bytevector-sint-set! $v1 -1 7 'big 8))
  (error? (bytevector-sint-set! $v1 -1 7 'big 9))
  (error? (if (bytevector-sint-set! $v1 -1 7 'big 10) #f #t))

  (error? (bytevector-sint-set! $v1 96 7 'little 8))
  (error? (bytevector-sint-set! $v1 96 7 'little 9))
  (error? (bytevector-sint-set! $v1 97 7 'big 7))
  (error? (bytevector-sint-set! $v1 98 7 'little 6))
  (error? (bytevector-sint-set! $v1 99 7 'big 5))
  (error? (bytevector-sint-set! $v1 100 7 'big 4))
  (error? (bytevector-sint-set! $v1 100 7 'big 5))
  (error? (bytevector-sint-set! $v1 100 7 'big 8))
  (error? (bytevector-sint-set! $v1 101 7 'big 3))
  (error? (bytevector-sint-set! $v1 101 7 'little 4))
  (error? (bytevector-sint-set! $v1 102 7 'little 2))
  (error? (bytevector-sint-set! $v1 102 7 'big 3))
  (error? (bytevector-sint-set! $v1 103 7 'big 1))
  (error? (bytevector-sint-set! $v1 103 7 'big 2))
  (error? (bytevector-sint-set! $v1 103 7 'big 3))
  (error? (if (bytevector-sint-set! $v1 4.0 7 (native-endianness) 3) #f #t))

 ; invalid value
  (error? (bytevector-sint-set! $v1 0 #x-81 'big 1))
  (error? (bytevector-sint-set! $v1 0 #x-81 'little 1))
  (error? (bytevector-sint-set! $v1 0 #x80 'big 1))
  (error? (bytevector-sint-set! $v1 0 #x80 'little 1))
  (error? (bytevector-sint-set! $v1 0 #x-8001 'big 2))
  (error? (bytevector-sint-set! $v1 0 #x-8001 'little 2))
  (error? (bytevector-sint-set! $v1 0 #x8000 'big 2))
  (error? (bytevector-sint-set! $v1 0 #x8000 'little 2))
  (error? (bytevector-sint-set! $v1 0 #x-800001 'big 3))
  (error? (bytevector-sint-set! $v1 0 #x-800001 'little 3))
  (error? (bytevector-sint-set! $v1 0 #x800000 'big 3))
  (error? (bytevector-sint-set! $v1 0 #x800000 'little 3))
  (error? (bytevector-sint-set! $v1 0 #x-80000001 'big 4))
  (error? (bytevector-sint-set! $v1 0 #x-80000001 'little 4))
  (error? (bytevector-sint-set! $v1 0 #x80000000 'big 4))
  (error? (bytevector-sint-set! $v1 0 #x80000000 'little 4))
  (error? (bytevector-sint-set! $v1 0 #x-8000000000000001 'big 8))
  (error? (bytevector-sint-set! $v1 0 #x-8000000000000001 'little 8))
  (error? (bytevector-sint-set! $v1 0 #x8000000000000000 'big 8))
  (error? (bytevector-sint-set! $v1 0 #x8000000000000000 'little 8))
  (error? (bytevector-sint-set! $v1 0 #x-80000000000000000001 'big 10))
  (error? (bytevector-sint-set! $v1 0 #x-80000000000000000001 'little 10))
  (error? (bytevector-sint-set! $v1 0 #x80000000000000000000 'big 10))
  (error? (if (bytevector-sint-set! $v1 0 #x80000000000000000000 'little 10) #f #t))

 ; invalid endianness
  (error? (bytevector-sint-set! $v1 0 7 'bonkers 1))
  (error? (bytevector-sint-set! $v1 0 7 'bonkers 2))
  (error? (bytevector-sint-set! $v1 0 7 'bonkers 3))
  (error? (bytevector-sint-set! $v1 0 7 'bonkers 4))
  (error? (bytevector-sint-set! $v1 0 7 'bonkers 8))
  (error? (if (bytevector-sint-set! $v1 0 7 'bonkers 35) #f #t))

 ; invalid size
  (error? (bytevector-sint-set! $v1 0 7 'little 0))
  (error? (bytevector-sint-set! $v1 1 7 'big -1))
  (error? (if (bytevector-sint-set! $v1 4 7 'little 'byte) #f #t))

 ; constant args
  (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
    (let ()
      (define-syntax a
        (lambda (x)
          (define (sublist ls i j)
            (list-head (list-tail ls i) j))
          (define (cmp-vec ls i j)
            (apply bytevector
              `(,@(make-list i #xc7)
                ,@(sublist ls i j)
                ,@(make-list (fx- (length ls) (+ i j)) #xc7))))
          (let* ([ls '(1 254 3) #;'(1 254 3 252 5 250 7 249 8 248
                       9 247 10 246 40 216 80 176 100 156)]
                 [n (length ls)])
            #`(list #,@(let f ([i 0])
                         (if (fx= i n)
                           '()
                           #`((list #,@(let g ([j 1])
                                        (if (fx<= j (fx- n i))
                                            #`((equal?
                                                 (let ([v (make-bytevector #,n #xc7)])
                                                   (bytevector-sint-set! v #,i
                                                     #,(apply little-endian->signed (sublist ls i j))
                                                     'little #,j)
                                                   v)
                                                 '#,(cmp-vec ls i j))
                                               (equal?
                                                 (let ([v (make-bytevector #,n #xc7)])
                                                   (bytevector-sint-set! v #,i
                                                     #,(apply big-endian->signed (sublist ls i j))
                                                     'big #,j)
                                                   v)
                                                 '#,(cmp-vec ls i j))
                                               #,@(g (fx+ j 1)))
                                            '())))
                              #,@(f (fx+ i 1)))))))))
      a))

 ; nonconstant args
  (do ([i 100 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random 256)) (make-list (random 25)))])
      (unless (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
                (let ()
                  (define (sublist ls i j)
                    (list-head (list-tail ls i) j))
                  (define (cmp-vec ls i j)
                    (apply bytevector
                      `(,@(make-list i #xc7)
                        ,@(sublist ls i j)
                        ,@(make-list (fx- (length ls) (+ i j)) #xc7))))
                  (let ([n (length ls)])
                    (let f ([i 0])
                      (if (fx= i n)
                          '()
                           (cons (let g ([j 1])
                                   (if (fx<= j (fx- n i))
                                       (cons*
                                         (equal?
                                           (let ([v (make-bytevector n #xc7)])
                                             (bytevector-sint-set! v i
                                               (apply little-endian->signed (sublist ls i j))
                                               'little j)
                                             v)
                                           (cmp-vec ls i j))
                                         (equal?
                                           (let ([v (make-bytevector n #xc7)])
                                             (bytevector-sint-set! v i
                                               (apply big-endian->signed (sublist ls i j))
                                               'big j)
                                             v)
                                           (cmp-vec ls i j))
                                         (g (fx+ j 1)))
                                       '()))
                                 (f (fx+ i 1))))))))
        (pretty-print ls)
        (errorf #f "failed for for ~s" ls))))
)

(mat bytevector-uint-set!
  (begin
    (define $v1
      '#vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #xff #xff #xff #xff #xff #xff #xff #xff
            #x7f #xff #xff #xff #xff #xff #xff #xff
            #xff #xff #xff #xff #xff #xff #xff #x7f
            #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00
            #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
            #x80 #x00 #x00 #x00 #xff #xff #xff #xff
            #xff #xff #xff #xff #x00 #x00 #x00 #x80
            #x12 #x23 #x34 #x45 #x56 #x67 #x78 #x89
            #x89 #x78 #x67 #x56 #x45 #x34 #x23 #x12
            #x78 #x89 #x9a #xab #xbc #xcd #xde #xef
            #xef #xde #xcd #xbc #xab #x9a #x89 #x78
            #xfe #xed #xdc #xcb #xba #xa9 #x98))
    (bytevector? $v1))

 ; wrong argument count
  (error? (bytevector-uint-set!))
  (error? (bytevector-uint-set! $v1))
  (error? (bytevector-uint-set! $v1 0))
  (error? (bytevector-uint-set! $v1 0 7))
  (error? (bytevector-uint-set! $v1 0 7 'big))
  (error? (if (bytevector-uint-set! $v1 0 7 'big 5 0) #f #t))

 ; not a bytevector
  (error? (bytevector-uint-set! '#(3 252 5 0 0 0 0 0 0 0 0) 0 7 'little 1))
  (error? (if (bytevector-uint-set! '#(3 252 5 0 0 0 0 0 0 0 0) 0 7 'little 1) #f #t))

 ; invalid index
  (error? (bytevector-uint-set! $v1 -1 7 'big 1))
  (error? (bytevector-uint-set! $v1 -1 7 'big 2))
  (error? (bytevector-uint-set! $v1 -1 7 'big 3))
  (error? (bytevector-uint-set! $v1 -1 7 'big 4))
  (error? (bytevector-uint-set! $v1 -1 7 'big 8))
  (error? (bytevector-uint-set! $v1 -1 7 'big 9))
  (error? (if (bytevector-uint-set! $v1 -1 7 'big 10) #f #t))

  (error? (bytevector-uint-set! $v1 96 7 'little 8))
  (error? (bytevector-uint-set! $v1 96 7 'little 9))
  (error? (bytevector-uint-set! $v1 97 7 'big 7))
  (error? (bytevector-uint-set! $v1 98 7 'little 6))
  (error? (bytevector-uint-set! $v1 99 7 'big 5))
  (error? (bytevector-uint-set! $v1 100 7 'big 4))
  (error? (bytevector-uint-set! $v1 100 7 'big 5))
  (error? (bytevector-uint-set! $v1 100 7 'big 8))
  (error? (bytevector-uint-set! $v1 101 7 'big 3))
  (error? (bytevector-uint-set! $v1 101 7 'little 4))
  (error? (bytevector-uint-set! $v1 102 7 'little 2))
  (error? (bytevector-uint-set! $v1 102 7 'big 3))
  (error? (bytevector-uint-set! $v1 103 7 'big 1))
  (error? (bytevector-uint-set! $v1 103 7 'big 2))
  (error? (bytevector-uint-set! $v1 103 7 'big 3))
  (error? (if (bytevector-uint-set! $v1 4.0 7 (native-endianness) 3) #f #t))

 ; invalid value
  (error? (bytevector-uint-set! $v1 0 #x-1 'big 1))
  (error? (bytevector-uint-set! $v1 0 #x-1 'little 1))
  (error? (bytevector-uint-set! $v1 0 #x100 'big 1))
  (error? (bytevector-uint-set! $v1 0 #x100 'little 1))
  (error? (bytevector-uint-set! $v1 0 #x-1 'big 2))
  (error? (bytevector-uint-set! $v1 0 #x-1 'little 2))
  (error? (bytevector-uint-set! $v1 0 #x10000 'big 2))
  (error? (bytevector-uint-set! $v1 0 #x10000 'little 2))
  (error? (bytevector-uint-set! $v1 0 #x-1 'big 3))
  (error? (bytevector-uint-set! $v1 0 #x-1 'little 3))
  (error? (bytevector-uint-set! $v1 0 #x1000000 'big 3))
  (error? (bytevector-uint-set! $v1 0 #x1000000 'little 3))
  (error? (bytevector-uint-set! $v1 0 #x-1 'big 4))
  (error? (bytevector-uint-set! $v1 0 #x-1 'little 4))
  (error? (bytevector-uint-set! $v1 0 #x100000000 'big 4))
  (error? (bytevector-uint-set! $v1 0 #x100000000 'little 4))
  (error? (bytevector-uint-set! $v1 0 #x-1 'big 8))
  (error? (bytevector-uint-set! $v1 0 #x-1 'little 8))
  (error? (bytevector-uint-set! $v1 0 #x10000000000000000 'big 8))
  (error? (bytevector-uint-set! $v1 0 #x10000000000000000 'little 8))
  (error? (bytevector-uint-set! $v1 0 #x-1 'big 10))
  (error? (bytevector-uint-set! $v1 0 #x-1 'little 10))
  (error? (bytevector-uint-set! $v1 0 #x100000000000000000000 'big 10))
  (error? (if (bytevector-uint-set! $v1 0 #x100000000000000000000 'little 10) #f #t))

 ; invalid endianness
  (error? (bytevector-uint-set! $v1 0 7 'bonkers 1))
  (error? (bytevector-uint-set! $v1 0 7 'bonkers 2))
  (error? (bytevector-uint-set! $v1 0 7 'bonkers 3))
  (error? (bytevector-uint-set! $v1 0 7 'bonkers 4))
  (error? (bytevector-uint-set! $v1 0 7 'bonkers 8))
  (error? (if (bytevector-uint-set! $v1 0 7 'bonkers 35) #f #t))

 ; invalid size
  (error? (bytevector-uint-set! $v1 0 7 'little 0))
  (error? (bytevector-uint-set! $v1 1 7 'big -1))
  (error? (if (bytevector-uint-set! $v1 4 7 'little 'byte) #f #t))

 ; constant args
  (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
    (let ()
      (define-syntax a
        (lambda (x)
          (define (sublist ls i j)
            (list-head (list-tail ls i) j))
          (define (cmp-vec ls i j)
            (apply bytevector
              `(,@(make-list i #xc7)
                ,@(sublist ls i j)
                ,@(make-list (fx- (length ls) (+ i j)) #xc7))))
          (let* ([ls '(1 254 3) #;'(1 254 3 252 5 250 7 249 8 248
                       9 247 10 246 40 216 80 176 100 156)]
                 [n (length ls)])
            #`(list #,@(let f ([i 0])
                         (if (fx= i n)
                           '()
                           #`((list #,@(let g ([j 1])
                                        (if (fx<= j (fx- n i))
                                            #`((equal?
                                                 (let ([v (make-bytevector #,n #xc7)])
                                                   (bytevector-uint-set! v #,i
                                                     #,(apply little-endian->unsigned (sublist ls i j))
                                                     'little #,j)
                                                   v)
                                                 '#,(cmp-vec ls i j))
                                               (equal?
                                                 (let ([v (make-bytevector #,n #xc7)])
                                                   (bytevector-uint-set! v #,i
                                                     #,(apply big-endian->unsigned (sublist ls i j))
                                                     'big #,j)
                                                   v)
                                                 '#,(cmp-vec ls i j))
                                               #,@(g (fx+ j 1)))
                                            '())))
                              #,@(f (fx+ i 1)))))))))
      a))

 ; nonconstant args
  (do ([i 100 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (map (lambda (x) (random 256)) (make-list (random 25)))])
      (unless (andmap (lambda (b*) (andmap (lambda (b) (eq? b #t)) b*))
                (let ()
                  (define (sublist ls i j)
                    (list-head (list-tail ls i) j))
                  (define (cmp-vec ls i j)
                    (apply bytevector
                      `(,@(make-list i #xc7)
                        ,@(sublist ls i j)
                        ,@(make-list (fx- (length ls) (+ i j)) #xc7))))
                  (let ([n (length ls)])
                    (let f ([i 0])
                      (if (fx= i n)
                          '()
                           (cons (let g ([j 1])
                                   (if (fx<= j (fx- n i))
                                       (cons*
                                         (equal?
                                           (let ([v (make-bytevector n #xc7)])
                                             (bytevector-uint-set! v i
                                               (apply little-endian->unsigned (sublist ls i j))
                                               'little j)
                                             v)
                                           (cmp-vec ls i j))
                                         (equal?
                                           (let ([v (make-bytevector n #xc7)])
                                             (bytevector-uint-set! v i
                                               (apply big-endian->unsigned (sublist ls i j))
                                               'big j)
                                             v)
                                           (cmp-vec ls i j))
                                         (g (fx+ j 1)))
                                       '()))
                                 (f (fx+ i 1))))))))
        (pretty-print ls)
        (errorf #f "failed for for ~s" ls))))
)

(mat bytevector-copy
 ; wrong argument count
  (error? (bytevector-copy))
  (error? (if (bytevector-copy #vu8() '#vu8()) #f #t))

 ; not a bytevector
  (error? (bytevector-copy '(a b c)))
  (error? (if (bytevector-copy '(a b c)) #f #t))

  (equal? (bytevector-copy #vu8()) '#vu8())
  (equal? (bytevector-copy #vu8(3 252 5)) '#vu8(3 252 5))
  (let* ([x1 (bytevector 1 2 3)] [x2 (bytevector-copy x1)])
    (and (equal? x2 x1) (not (eq? x2 x1))))
)

(mat bytevector-copy!
  (begin
    (define $v1 (bytevector 1 2 3 4))
    (define $v2 (bytevector 255 254 253 252 251 250 249 248 247))
    (and (bytevector? $v1)
         (bytevector? $v2)
         (eqv? (bytevector-length $v1) 4)
         (eqv? (bytevector-length $v2) 9)))

 ; wrong number of arguments
  (error? (bytevector-copy!))
  (error? (bytevector-copy! $v2))
  (error? (bytevector-copy! $v2 3))
  (error? (bytevector-copy! $v2 3 $v1))
  (error? (bytevector-copy! $v2 3 $v1 1))
  (error? (if (bytevector-copy! $v2 3 $v1 1 2 3) #f #t))

 ; not bytevector
  (error? (bytevector-copy! 0 0 $v2 0 0))
  (error? (if (bytevector-copy! $v1 0 (vector 1 2 3) 0 0) #f #t))

 ; bad index
  (error? (bytevector-copy! $v1 -1 $v2 0 0))
  (error? (bytevector-copy! $v1 0 $v2 -1 0))
  (error? (bytevector-copy! $v1 'a $v2 0 0))
  (error? (bytevector-copy! $v1 0 $v2 0.0 0))
  (error? (bytevector-copy! $v1 (+ (most-positive-fixnum) 1) $v2 0 0))
  (error? (if (bytevector-copy! $v1 0 $v2 (+ (most-positive-fixnum) 1) 0) #f #t))

 ; bad count
  (error? (bytevector-copy! $v1 0 $v2 0 -1))
  (error? (bytevector-copy! $v1 0 $v2 0 (+ (most-positive-fixnum) 1)))
  (error? (if (bytevector-copy! $v1 0 $v2 0 'a) #f #t))

 ; beyond end
  (error? (bytevector-copy! $v1 0 $v2 0 5))
  (error? (bytevector-copy! $v2 0 $v1 0 5))
  (error? (bytevector-copy! $v1 1 $v2 0 4))
  (error? (bytevector-copy! $v2 0 $v1 1 4))
  (error? (bytevector-copy! $v1 2 $v2 0 3))
  (error? (bytevector-copy! $v2 0 $v1 2 3))
  (error? (bytevector-copy! $v1 3 $v2 0 2))
  (error? (bytevector-copy! $v2 0 $v1 3 2))
  (error? (bytevector-copy! $v1 4 $v2 0 1))
  (error? (bytevector-copy! $v2 0 $v1 4 1))
  (error? (bytevector-copy! $v2 0 $v1 0 500))
  (error? (if (bytevector-copy! $v2 500 $v1 0 0) #f #t))

 ; make sure no damage done
  (and (bytevector? $v1)
       (bytevector? $v2)
       (equal? $v1 #vu8(1 2 3 4))
       (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247)))

  (begin
    (bytevector-copy! $v2 3 $v1 1 2)
    (and (equal? $v1 #vu8(1 252 251 4))
         (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247))))
  (begin
    (bytevector-copy! $v2 6 $v1 2 2)
    (and (equal? $v1 #vu8(1 252 249 248))
         (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247))))
  (begin
    (bytevector-copy! $v2 0 $v1 4 0)
    (and (equal? $v1 #vu8(1 252 249 248))
         (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247))))
  (begin
    (bytevector-copy! $v2 3 $v1 4 0)
    (and (equal? $v1 #vu8(1 252 249 248))
         (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247))))
  (begin
    (bytevector-copy! $v2 3 $v2 4 0)
    (and (equal? $v1 #vu8(1 252 249 248))
         (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247))))
  (begin
    (bytevector-copy! $v2 2 $v1 1 3)
    (and (equal? $v1 #vu8(1 253 252 251))
         (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247))))
  (begin
    (bytevector-copy! $v1 0 $v2 3 4)
    (and (equal? $v1 #vu8(1 253 252 251))
         (equal? $v2 #vu8(255 254 253 1 253 252 251 248 247))))
  (begin
    (bytevector-copy! $v2 0 $v2 3 5)
    (and (equal? $v1 #vu8(1 253 252 251))
         (equal? $v2 #vu8(255 254 253 255 254 253 1 253 247))))
  (begin
    (bytevector-copy! $v2 4 $v2 2 5)
    (and (equal? $v1 #vu8(1 253 252 251))
         (equal? $v2 #vu8(255 254 254 253 1 253 247 253 247))))
  (begin
    (bytevector-copy! $v2 1 $v2 1 7)
    (and (equal? $v1 #vu8(1 253 252 251))
         (equal? $v2 #vu8(255 254 254 253 1 253 247 253 247))))
)

(mat bytevector-truncate!
  (begin
    (define $v (bytevector 1 2 3 4 5 6 7 8 9))
    (and (bytevector? $v)
         (fx= (bytevector-length $v) 9)
         (bytevector=? $v #vu8(1 2 3 4 5 6 7 8 9))))

 ; wrong number of arguments
  (error? (bytevector-truncate!))
  (error? (bytevector-truncate! $v))
  (error? (bytevector-truncate! $v 3 15))

 ; not bytevector
  (error? (bytevector-truncate! 0 0))
  (error? (if (bytevector-truncate! (string #\a #\b #\c) 2) #f #t))

 ; bad length
  (error? (bytevector-truncate! $v -1))
  (error? (bytevector-truncate! $v 10))
  (error? (bytevector-truncate! $v 1000))
  (error? (bytevector-truncate! $v (+ (most-positive-fixnum) 1)))
  (error? (bytevector-truncate! $v 'a))

  (begin
    (bytevector-truncate! $v 9)
    (and (bytevector? $v)
         (fx= (bytevector-length $v) 9)
         (bytevector=? $v #vu8(1 2 3 4 5 6 7 8 9))))

  (begin
    (bytevector-truncate! $v 8)
    (and (bytevector? $v)
         (fx= (bytevector-length $v) 8)
         (bytevector=? $v #vu8(1 2 3 4 5 6 7 8))))

  (begin
    (bytevector-truncate! $v 6)
    (and (bytevector? $v)
         (fx= (bytevector-length $v) 6)
         (bytevector=? $v #vu8(1 2 3 4 5 6))))

  (begin
    (bytevector-truncate! $v 3)
    (and (bytevector? $v)
         (fx= (bytevector-length $v) 3)
         (bytevector=? $v #vu8(1 2 3))))

  (begin
    (define $v2 (bytevector-truncate! $v 0))
    (and (eqv? $v2 #vu8())
         (bytevector? $v)
         (fx= (bytevector-length $v) 3)
         (bytevector=? $v #vu8(1 2 3))))
)

(mat bytevector-fill!
  (begin
    (define $v1 (bytevector 1 2 3 4))
    (define $v2 (bytevector 255 254 253 252 251 250 249 248 247))
    (and (bytevector? $v1)
         (bytevector? $v2)
         (eqv? (bytevector-length $v1) 4)
         (eqv? (bytevector-length $v2) 9)))

 ; wrong argument count
  (error? (bytevector-fill!))
  (error? (bytevector-fill! $v1))
  (error? (begin (bytevector-fill! $v1 0 0) #f))

 ; not a bytevector
  (error? (bytevector-fill! 'a 3))
  (error? (begin (let ([v (vector 1)]) (bytevector-fill! v 3)) #f))

 ; invalid fill
  (error? (bytevector-fill! $v1 -129))
  (error? (bytevector-fill! $v1 256))
  (error? (begin (bytevector-fill! $v1 'a) #f))

 ; make sure no damage done
  (and (bytevector? $v1)
       (bytevector? $v2)
       (equal? $v1 #vu8(1 2 3 4))
       (equal? $v2 #vu8(255 254 253 252 251 250 249 248 247)))

  (begin
    (bytevector-fill! $v1 -128)
    (and (bytevector? $v1)
         (equal? $v1 #vu8(128 128 128 128))))
  (begin
    (bytevector-fill! $v1 -1)
    (and (bytevector? $v1)
         (equal? $v1 #vu8(255 255 255 255))))
  (begin
    (bytevector-fill! $v1 0)
    (and (bytevector? $v1)
         (equal? $v1 #vu8(0 0 0 0))))
  (begin
    (bytevector-fill! $v1 127)
    (and (bytevector? $v1)
         (equal? $v1 #vu8(127 127 127 127))))
  (begin
    (bytevector-fill! $v1 128)
    (and (bytevector? $v1)
         (equal? $v1 #vu8(128 128 128 128))))
  (begin
    (bytevector-fill! $v1 255)
    (and (bytevector? $v1)
         (equal? $v1 #vu8(255 255 255 255))))
  (begin
    (bytevector-fill! $v2 -128)
    (and (bytevector? $v2)
         (equal? $v2 #vu8(128 128 128 128 128 128 128 128 128))))
  (begin
    (bytevector-fill! $v2 -1)
    (and (bytevector? $v2)
         (equal? $v2 #vu8(255 255 255 255 255 255 255 255 255))))
  (begin
    (bytevector-fill! $v2 0)
    (and (bytevector? $v2)
         (equal? $v2 #vu8(0 0 0 0 0 0 0 0 0))))
  (begin
    (bytevector-fill! $v2 127)
    (and (bytevector? $v2)
         (equal? $v2 #vu8(127 127 127 127 127 127 127 127 127))))
  (begin
    (bytevector-fill! $v2 128)
    (and (bytevector? $v2)
         (equal? $v2 #vu8(128 128 128 128 128 128 128 128 128))))
  (begin
    (bytevector-fill! $v2 255)
    (and (bytevector? $v2)
         (equal? $v2 #vu8(255 255 255 255 255 255 255 255 255))))

  (let ([v (bytevector-copy '#5vu8(1 2 3 4 5))])
    (and (equal? v '#5vu8(1 2 3 4 5))
         (begin
           (bytevector-fill! v 9)
           (equal? v '#5vu8(9)))))
  (let ([v (bytevector-copy '#5vu8(1 2 3 4 5))])
    (and (equal? v '#5vu8(1 2 3 4 5))
         (begin
           (bytevector-fill! v -17)
           (equal? v '#5vu8(239)))))
  (do ([q 10000 (fx- q 1)])
      ((fx= q 0) #t)
    (let ([v (bytevector 3 4 5)])
      (do ([n -128 (fx+ n 1)])
          ((fx= n 128) #t)
        (bytevector-fill! v n)
        (unless (and (eqv? (bytevector-s8-ref v 0) n)
                     (eqv? (bytevector-s8-ref v 1) n)
                     (eqv? (bytevector-s8-ref v 2) n))
          (errorf #f "wrong value for ~s" n)))))
  (do ([q 10000 (fx- q 1)])
      ((fx= q 0) #t)
    (let ([v (bytevector 3 4 5)])
      (do ([n 0 (fx+ n 1)])
          ((fx= n 255) #t)
        (bytevector-fill! v n)
        (unless (and (eqv? (bytevector-u8-ref v 0) n)
                     (eqv? (bytevector-u8-ref v 1) n)
                     (eqv? (bytevector-u8-ref v 2) n))
          (errorf #f "wrong value for ~s" n)))))
)

(mat s8-list->bytevector
 ; wrong argument count
  (error? (s8-list->bytevector))
  (error? (begin (s8-list->bytevector '(1 -2 3) '(1 -2 3)) #t))

 ; not a list
  (error? (s8-list->bytevector '#(a b c)))
  (error? (begin (s8-list->bytevector '#(a b c)) #t))

 ; improper or cyclic list
  (error? (s8-list->bytevector '(1 2 . 3)))
  (error? (s8-list->bytevector (let ([ls (list 1 2 3)]) (set-cdr! (cddr ls) (cdr ls)) ls)))

 ; invalid value
  (error? (s8-list->bytevector '(1 -129 3)))
  (error? (begin (s8-list->bytevector '(1 128 3)) #t))

  (equal? (s8-list->bytevector '(1 -2 3)) #vu8(1 254 3))
  (equal? (s8-list->bytevector '()) #vu8())
  (do ([n -128 (fx+ n 1)])
      ((fx= n 128) #t)
    (let ([v (s8-list->bytevector (list 3 n 4))])
      (unless (and (eqv? (bytevector-s8-ref v 0) 3)
                   (eqv? (bytevector-s8-ref v 1) n)
                   (eqv? (bytevector-s8-ref v 2) 4))
        (errorf #f "wrong value for ~s" n))))
)

(mat u8-list->bytevector
 ; wrong argument count
  (error? (u8-list->bytevector))
  (error? (begin (u8-list->bytevector '(1 2 3) '(1 2 3)) #t))

 ; not a bytevector
  (error? (u8-list->bytevector '#(a b c)))
  (error? (begin (u8-list->bytevector '#(a b c)) #t))

 ; invalid value
  (error? (u8-list->bytevector '(1 -129 3)))
  (error? (begin (u8-list->bytevector '(1 -1 3)) #t))

 ; improper or cyclic list
  (error? (u8-list->bytevector '(1 2 . 3)))
  (error? (u8-list->bytevector (let ([ls (list 1 2 3)]) (set-cdr! (cddr ls) (cdr ls)) ls)))

  (equal? (u8-list->bytevector '(1 2 3)) #vu8(1 2 3))
  (equal? (u8-list->bytevector '()) #vu8())
  (do ([n 0 (fx+ n 1)])
      ((fx= n 255) #t)
    (let ([v (u8-list->bytevector (list 3 n 4))])
      (unless (and (eqv? (bytevector-u8-ref v 0) 3)
                   (eqv? (bytevector-u8-ref v 1) n)
                   (eqv? (bytevector-u8-ref v 2) 4))
        (errorf #f "wrong value for ~s" n))))
)

(mat bytevector->s8-list
 ; wrong argument count
  (error? (bytevector->s8-list))
  (error? (begin (bytevector->s8-list #vu8(1 2 3) '#vu8(1 2 3)) #t))

 ; not a bytevector
  (error? (begin (bytevector->s8-list "hello") #t))
  (error? (bytevector->s8-list '(a b c)))

  (equal? (bytevector->s8-list #vu8(1 255 3)) '(1 -1 3))
  (equal? (bytevector->s8-list #vu8(1 255 253 4)) '(1 -1 -3 4))
  (equal? (bytevector->s8-list #vu8()) '())
)

(mat bytevector->u8-list
 ; wrong argument count
  (error? (bytevector->u8-list))
  (error? (begin (bytevector->u8-list #vu8(1 2 3) '#vu8(1 2 3)) #t))

 ; not a bytevector
  (error? (bytevector->u8-list "hello"))
  (error? (begin (bytevector->u8-list '(a b c)) #t))

  (equal? (bytevector->u8-list #vu8(1 2 3)) '(1 2 3))
  (equal? (bytevector->u8-list #vu8(1 255 253 4)) '(1 255 253 4))
  (equal? (bytevector->u8-list #vu8()) '())
)

(mat sint-list->bytevector
 ; wrong argument count
  (error? (sint-list->bytevector))
  (error? (sint-list->bytevector '(1 3 7) 'little))
  (error? (begin (sint-list->bytevector '(1 -3 7) 'big 1 0) #t))

 ; not a list
  (error? (sint-list->bytevector '#(a b c) 'little 1))
  (error? (begin (sint-list->bytevector '#(a b c) 'little 1) #t))

 ; improper or cyclic list
  (error? (sint-list->bytevector '(1 2 . 3) 'little 1))
  (error? (sint-list->bytevector (let ([ls (list 1 2 3)]) (set-cdr! (cddr ls) (cdr ls)) ls) 'little 1))

 ; invalid value
  (error? (sint-list->bytevector '(0 #x-81 0) 'big 1))
  (error? (sint-list->bytevector '(0 #x-81 0) 'little 1))
  (error? (sint-list->bytevector '(0 #x80 0) (native-endianness) 1))
  (error? (sint-list->bytevector '(0 #x80 0) 'little 1))
  (error? (sint-list->bytevector '(0 #x-8001 0) (native-endianness) 2))
  (error? (sint-list->bytevector '(0 #x-8001 0) 'little 2))
  (error? (sint-list->bytevector '(0 #x8000 0) 'big 2))
  (error? (sint-list->bytevector '(0 #x8000 0) 'little 2))
  (error? (sint-list->bytevector '(0 #x-800001 0) 'big 3))
  (error? (sint-list->bytevector '(0 #x-800001 0) 'little 3))
  (error? (sint-list->bytevector '(0 #x800000 0) 'big 3))
  (error? (sint-list->bytevector '(0 #x800000 0) (native-endianness) 3))
  (error? (sint-list->bytevector '(0 #x-80000001 0) 'big 4))
  (error? (sint-list->bytevector '(0 #x-80000001 0) 'little 4))
  (error? (sint-list->bytevector '(0 #x80000000 0) (native-endianness) 4))
  (error? (sint-list->bytevector '(0 #x80000000 0) 'little 4))
  (error? (sint-list->bytevector '(0 #x-8000000000000001 0) 'big 8))
  (error? (sint-list->bytevector '(0 #x-8000000000000001 0) (native-endianness) 8))
  (error? (sint-list->bytevector '(0 #x8000000000000000 0) 'big 8))
  (error? (sint-list->bytevector '(0 #x8000000000000000 0) 'little 8))
  (error? (sint-list->bytevector '(0 #x-80000000000000000001 0) (native-endianness) 10))
  (error? (sint-list->bytevector '(0 #x-80000000000000000001 0) 'little 10))
  (error? (sint-list->bytevector '(0 #x80000000000000000000 0) 'big 10))
  (error? (begin (sint-list->bytevector '(0 #x80000000000000000000 0) 'little 10) #t))

 ; invalid endianness
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 1))
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 2))
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 3))
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 4))
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 6))
  (error? (begin (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 12) #t))

 ; invalid size
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big -1))
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big 0))
  (error? (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big 1.0))
  (error? (begin (sint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big "oops") #t))

  (equal?
    (sint-list->bytevector '(#x-1 #x01 #x02 #x-5 #x-80 #x7f) 'little 1)
    #vu8(#xff #x01 #x02 #xfb #x80 #x7f))

  (equal?
    (sint-list->bytevector '(#x7f #x-80 -5 #x2 #x1 -1) 'big 1)
    #vu8(#x7f #x80 #xfb #x2 #x1 #xff))

  (equal?
    (sint-list->bytevector '(#x-ff #x2FB #x-7f81) 'big 2)
    #vu8(#xff #x01 #x02 #xfb #x80 #x7f))

  (equal?
    (sint-list->bytevector
      (list (little-endian->signed #xff 1 3 #xa0)
            (little-endian->signed #x71 #x82 #x95 #x61)
            (little-endian->signed #x91 #xa2 #xb5 #xc1)
            (little-endian->signed 5 2 3 4))
      'little 4)
    #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x61 #x91 #xa2 #xb5 #xc1 5 2 3 4))

  (equal?
    (sint-list->bytevector
      (list (little-endian->signed #xff 1 3 #xa0 #x55)
            (little-endian->signed #x71 #x82 #x95 #x61 #x85)
            (little-endian->signed #x91 #xa2 #xb5 #xc1 #x99)
            (little-endian->signed 5 2 3 4 6))
      'little 5)
    #vu8(#xff #x01 #x03 #xa0 #x55 #x71 #x82 #x95 #x61 #x85 #x91 #xa2 #xb5 #xc1 #x99 5 2 3 4 6))

  (equal?
    (sint-list->bytevector
      (list (little-endian->signed #xff 1 3 #xa0 #x71 #x82 #x95 #x98)
            (little-endian->signed #x91 #xa2 #xb5 #xc1 5 2 3 4))
      'little 8)
    #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x98 #x91 #xa2 #xb5 #xc1 5 2 3 4))

  (do ([q 500 (fx- q 1)])
      ((fx= q 0) #t)
    (do ([i 1 (fx+ i 1)])
        ((fx= i 25))
      (let ([ls* (map (lambda (x) (map (lambda (x) (random 256)) (make-list i)))
                      (make-list (random 10)))])
        (unless (equal?
                  (sint-list->bytevector (map (lambda (ls) (apply little-endian->signed ls)) ls*) 'little i)
                  (apply bytevector (apply append ls*)))
          (pretty-print ls*)
          (errorf #f "failed for ~s (little)" ls*))
        (unless (equal?
                  (sint-list->bytevector (map (lambda (ls) (apply big-endian->signed ls)) ls*) 'big i)
                  (apply bytevector (apply append ls*)))
          (pretty-print ls*)
          (errorf #f "failed for ~s (big)" ls*)))))
)

(mat uint-list->bytevector
 ; wrong argument count
  (error? (uint-list->bytevector))
  (error? (uint-list->bytevector '(1 3 7) 'little))
  (error? (begin (uint-list->bytevector '(1 -3 7) 'big 1 0) #t))

 ; not a list
  (error? (uint-list->bytevector '#(a b c) 'little 1))
  (error? (begin (uint-list->bytevector '#(a b c) 'little 1) #t))

 ; improper or cyclic list
  (error? (uint-list->bytevector '(1 2 . 3) 'little 1))
  (error? (uint-list->bytevector (let ([ls (list 1 2 3)]) (set-cdr! (cddr ls) (cdr ls)) ls) 'little 1))

 ; invalid value
  (error? (uint-list->bytevector '(0 #x-1 0) 'big 1))
  (error? (uint-list->bytevector '(0 #x-1 0) 'little 1))
  (error? (uint-list->bytevector '(0 #x100 0) (native-endianness) 1))
  (error? (uint-list->bytevector '(0 #x100 0) 'little 1))
  (error? (uint-list->bytevector '(0 x-1 0) (native-endianness) 2))
  (error? (uint-list->bytevector '(0 x-1 0) 'little 2))
  (error? (uint-list->bytevector '(0 #x10000 0) 'big 2))
  (error? (uint-list->bytevector '(0 #x10000 0) 'little 2))
  (error? (uint-list->bytevector '(0 x-1 0) 'big 3))
  (error? (uint-list->bytevector '(0 x-1 0) 'little 3))
  (error? (uint-list->bytevector '(0 #x1000000 0) 'big 3))
  (error? (uint-list->bytevector '(0 #x1000000 0) (native-endianness) 3))
  (error? (uint-list->bytevector '(0 x-1 0) 'big 4))
  (error? (uint-list->bytevector '(0 x-1 0) 'little 4))
  (error? (uint-list->bytevector '(0 #x100000000 0) (native-endianness) 4))
  (error? (uint-list->bytevector '(0 #x100000000 0) 'little 4))
  (error? (uint-list->bytevector '(0 x-1 0) 'big 8))
  (error? (uint-list->bytevector '(0 x-1 0) (native-endianness) 8))
  (error? (uint-list->bytevector '(0 #x10000000000000000 0) 'big 8))
  (error? (uint-list->bytevector '(0 #x10000000000000000 0) 'little 8))
  (error? (uint-list->bytevector '(0 x-1 0) (native-endianness) 10))
  (error? (uint-list->bytevector '(0 x-1 0) 'little 10))
  (error? (uint-list->bytevector '(0 #x100000000000000000000 0) 'big 10))
  (error? (begin (uint-list->bytevector '(0 #x100000000000000000000 0) 'little 10) #t))

 ; invalid endianness
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 1))
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 2))
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 3))
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 4))
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 6))
  (error? (begin (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'spam 12) #t))

 ; invalid size
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big -1))
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big 0))
  (error? (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big 1.0))
  (error? (begin (uint-list->bytevector '(1 3 7 4 3 -3 5 -6 127 -128 -50 50) 'big "oops") #t))

  (equal?
    (uint-list->bytevector '(#xff #x01 #x02 #xfb #x80 #x7f) 'little 1)
    #vu8(#xff #x01 #x02 #xfb #x80 #x7f))

  (equal?
    (uint-list->bytevector '(#x7f #x80 #xfb #x2 #x1 #xff) 'big 1)
    #vu8(#x7f #x80 #xfb #x2 #x1 #xff))

  (equal?
    (uint-list->bytevector '(#xff01 #x2FB #x807f) 'big 2)
    #vu8(#xff #x01 #x02 #xfb #x80 #x7f))

  (equal?
    (uint-list->bytevector
      (list (little-endian->unsigned #xff 1 3 #xa0)
            (little-endian->unsigned #x71 #x82 #x95 #x61)
            (little-endian->unsigned #x91 #xa2 #xb5 #xc1)
            (little-endian->unsigned 5 2 3 4))
      'little 4)
    #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x61 #x91 #xa2 #xb5 #xc1 5 2 3 4))

  (equal?
    (uint-list->bytevector
      (list (little-endian->unsigned #xff 1 3 #xa0 #x55)
            (little-endian->unsigned #x71 #x82 #x95 #x61 #x85)
            (little-endian->unsigned #x91 #xa2 #xb5 #xc1 #x99)
            (little-endian->unsigned 5 2 3 4 6))
      'little 5)
    #vu8(#xff #x01 #x03 #xa0 #x55 #x71 #x82 #x95 #x61 #x85 #x91 #xa2 #xb5 #xc1 #x99 5 2 3 4 6))

  (equal?
    (uint-list->bytevector
      (list (little-endian->unsigned #xff 1 3 #xa0 #x71 #x82 #x95 #x98)
            (little-endian->unsigned #x91 #xa2 #xb5 #xc1 5 2 3 4))
      'little 8)
    #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x98 #x91 #xa2 #xb5 #xc1 5 2 3 4))

  (do ([q 500 (fx- q 1)])
      ((fx= q 0) #t)
    (do ([i 1 (fx+ i 1)])
        ((fx= i 25))
      (let ([ls* (map (lambda (x) (map (lambda (x) (random 256)) (make-list i)))
                      (make-list (random 10)))])
        (unless (equal?
                  (uint-list->bytevector (map (lambda (ls) (apply little-endian->unsigned ls)) ls*) 'little i)
                  (apply bytevector (apply append ls*)))
          (pretty-print ls*)
          (errorf #f "failed for ~s (little)" ls*))
        (unless (equal?
                  (uint-list->bytevector (map (lambda (ls) (apply big-endian->unsigned ls)) ls*) 'big i)
                  (apply bytevector (apply append ls*)))
          (pretty-print ls*)
          (errorf #f "failed for ~s (big)" ls*)))))
)

(mat bytevector->sint-list
 ; wrong argument count
  (error? (bytevector->sint-list))
  (error? (bytevector->sint-list #vu8(1 3 7) 'little))
  (error? (begin (bytevector->sint-list #vu8(1 253 7) 'big 1 0) #t))

 ; not a bytevector
  (error? (bytevector->sint-list '#(a b c) 'little 1))
  (error? (begin (bytevector->sint-list '#(a b c) 'little 1) #t))

 ; invalid endianness
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 1))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 2))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 3))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 4))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 6))
  (error? (begin (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 12) #t))

 ; invalid size
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big -1))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 0))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 1.0))
  (error? (begin (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big "oops") #t))

 ; length not multiple of size
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 5))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'little 7))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 8))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) (native-endianness) 9))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 10))
  (error? (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'little 11))
  (error? (begin (bytevector->sint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) (native-endianness) 50) #t))

  (equal?
    (bytevector->sint-list #vu8(#xff #x01 #x02 #xfb #x80 #x7f) 'little 1)
    '(#x-1 #x01 #x02 #x-5 #x-80 #x7f))

  (equal?
    (bytevector->sint-list #vu8(#x7f #x80 #xfb #x2 #x1 #xff) 'big 1)
    '(#x7f #x-80 -5 #x2 #x1 -1))

  (equal?
    (bytevector->sint-list #vu8(#xff #x01 #x02 #xfb #x80 #x7f) 'big 2)
    '(#x-ff #x2FB #x-7f81))

  (equal?
    (bytevector->sint-list
      #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x61 #x91 #xa2 #xb5 #xc1 5 2 3 4)
      'little 4)
    (list (little-endian->signed #xff 1 3 #xa0)
          (little-endian->signed #x71 #x82 #x95 #x61)
          (little-endian->signed #x91 #xa2 #xb5 #xc1)
          (little-endian->signed 5 2 3 4)))

  (equal?
    (bytevector->sint-list
      #vu8(#xff #x01 #x03 #xa0 #x55 #x71 #x82 #x95 #x61 #x85 #x91 #xa2 #xb5 #xc1 #x99 5 2 3 4 6)
      'little 5)
    (list (little-endian->signed #xff 1 3 #xa0 #x55)
          (little-endian->signed #x71 #x82 #x95 #x61 #x85)
          (little-endian->signed #x91 #xa2 #xb5 #xc1 #x99)
          (little-endian->signed 5 2 3 4 6)))

  (equal?
    (bytevector->sint-list
      #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x98 #x91 #xa2 #xb5 #xc1 5 2 3 4)
      'little 8)
    (list (little-endian->signed #xff 1 3 #xa0 #x71 #x82 #x95 #x98)
          (little-endian->signed #x91 #xa2 #xb5 #xc1 5 2 3 4)))

  (do ([q 500 (fx- q 1)])
      ((fx= q 0) #t)
    (do ([i 1 (fx+ i 1)])
        ((fx= i 25))
      (let ([ls* (map (lambda (x) (map (lambda (x) (random 256)) (make-list i)))
                      (make-list (random 10)))])
        (unless (equal?
                  (bytevector->sint-list (apply bytevector (apply append ls*)) 'little i)
                  (map (lambda (ls) (apply little-endian->signed ls)) ls*))
          (pretty-print ls*)
          (errorf #f "failed for ~s (little)" ls*))
        (unless (equal?
                  (bytevector->sint-list (apply bytevector (apply append ls*)) 'big i)
                  (map (lambda (ls) (apply big-endian->signed ls)) ls*))
          (pretty-print ls*)
          (errorf #f "failed for ~s (big)" ls*)))))
)

(mat bytevector->uint-list
 ; wrong argument count
  (error? (bytevector->uint-list))
  (error? (bytevector->uint-list #vu8(1 3 7) 'little))
  (error? (begin (bytevector->uint-list #vu8(1 253 7) 'big 1 0) #t))

 ; not a bytevector
  (error? (bytevector->uint-list '#(a b c) 'little 1))
  (error? (begin (bytevector->uint-list '#(a b c) 'little 1) #t))

 ; invalid endianness
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 1))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 2))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 3))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 4))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 6))
  (error? (begin (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'spam 12) #t))

 ; invalid size
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big -1))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 0))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 1.0))
  (error? (begin (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big "oops") #t))

 ; length not multiple of size
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 5))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'little 7))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 8))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) (native-endianness) 9))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'big 10))
  (error? (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) 'little 11))
  (error? (begin (bytevector->uint-list #vu8(1 3 7 4 3 253 5 250 127 128 206 50) (native-endianness) 50) #t))

  (equal?
    (bytevector->uint-list #vu8(#xff #x01 #x02 #xfb #x80 #x7f) 'little 1)
    '(#xff #x01 #x02 #xfb #x80 #x7f))

  (equal?
    (bytevector->uint-list #vu8(#x7f #x80 #xfb #x2 #x1 #xff) 'big 1)
    '(#x7f #x80 #xfb #x2 #x1 #xff))

  (equal?
    (bytevector->uint-list #vu8(#xff #x01 #x02 #xfb #x80 #x7f) 'big 2)
    '(#xff01 #x2FB #x807f))

  (equal?
    (bytevector->uint-list
      #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x61 #x91 #xa2 #xb5 #xc1 5 2 3 4)
      'little 4)
    (list (little-endian->unsigned #xff 1 3 #xa0)
          (little-endian->unsigned #x71 #x82 #x95 #x61)
          (little-endian->unsigned #x91 #xa2 #xb5 #xc1)
          (little-endian->unsigned 5 2 3 4)))

  (equal?
    (bytevector->uint-list
      #vu8(#xff #x01 #x03 #xa0 #x55 #x71 #x82 #x95 #x61 #x85 #x91 #xa2 #xb5 #xc1 #x99 5 2 3 4 6)
      'little 5)
    (list (little-endian->unsigned #xff 1 3 #xa0 #x55)
          (little-endian->unsigned #x71 #x82 #x95 #x61 #x85)
          (little-endian->unsigned #x91 #xa2 #xb5 #xc1 #x99)
          (little-endian->unsigned 5 2 3 4 6)))

  (equal?
    (bytevector->uint-list
      #vu8(#xff #x01 #x03 #xa0 #x71 #x82 #x95 #x98 #x91 #xa2 #xb5 #xc1 5 2 3 4)
      'little 8)
    (list (little-endian->unsigned #xff 1 3 #xa0 #x71 #x82 #x95 #x98)
          (little-endian->unsigned #x91 #xa2 #xb5 #xc1 5 2 3 4)))

  (do ([q 500 (fx- q 1)])
      ((fx= q 0) #t)
    (do ([i 1 (fx+ i 1)])
        ((fx= i 25))
      (let ([ls* (map (lambda (x) (map (lambda (x) (random 256)) (make-list i)))
                      (make-list (random 10)))])
        (unless (equal?
                  (bytevector->uint-list (apply bytevector (apply append ls*)) 'little i)
                  (map (lambda (ls) (apply little-endian->unsigned ls)) ls*))
          (pretty-print ls*)
          (errorf #f "failed for ~s (little)" ls*))
        (unless (equal?
                  (bytevector->uint-list (apply bytevector (apply append ls*)) 'big i)
                  (map (lambda (ls) (apply big-endian->unsigned ls)) ls*))
          (pretty-print ls*)
          (errorf #f "failed for ~s (big)" ls*)))))
)

(mat bytevector=?
 ; wrong argument count
  (error? (bytevector=?))
  (error? (bytevector=? #vu8()))
  (error? (begin (bytevector=? #vu8() '#vu8() '#vu8()) #t))

 ; not a bytevector
  (error? (bytevector=? #vu8() 'a))
  (error? (begin (bytevector=? "a" #vu8()) #t))

  (bytevector=? #vu8() (bytevector))
  (bytevector=? #vu8() (make-bytevector 0))
  (bytevector=? #vu8() (make-bytevector 0 17))
  (bytevector=? #vu8() (make-bytevector 0 -17))
  (not (bytevector=? #vu8() (bytevector 1)))
  (not (bytevector=? #vu8() (make-bytevector 1)))
  (not (bytevector=? #vu8() (make-bytevector 1 17)))
  (not (bytevector=? #vu8() (make-bytevector 1 -17)))
  (bytevector=? #vu8(1 2 3 4) (bytevector 1 2 3 4))
  (not (bytevector=? #vu8(1 2 3 4) (bytevector 1 2 4 3)))
  (not (bytevector=? #vu8(1 2 3 4) (bytevector 1 2 3)))
  (not (bytevector=? #vu8(1 2 3 4) (bytevector 1 2)))
  (not (bytevector=? #vu8(1 2 3 4) (bytevector 1)))
  (not (bytevector=? #vu8(1 2 3 4) (bytevector)))
  (bytevector=? (bytevector 255 254 253) (bytevector -1 -2 -3))
  (do ([n 1 (fx+ n 1)])
      ((fx= n 1000) #t)
    (let* ([v1 (u8-list->bytevector
                 (map (lambda (x) (random 256)) (make-list n)))]
           [v2 (bytevector-copy v1)])
      (when (eq? v1 v2) (errorf #f "copy is eq to original"))
      (unless (bytevector=? v1 v2)
        (pretty-print v1)
        (errorf #f "first bytevector=? failed for ~s (see output for vector)" n))
      (do ([i 0 (fx+ i 1)])
          ((fx= i n))
        (let ([k (bytevector-u8-ref v2 i)])
          (bytevector-u8-set! v2 i (fxmodulo (fx+ k 1) 256))
          (when (bytevector=? v1 v2)
            (pretty-print v1)
            (pretty-print v2)
            (errorf #f "second bytevector=? failed for n=~s and i=~s (see output for vector)" n i))
          (bytevector-u8-set! v2 i k))
        (unless (bytevector=? v1 v2)
          (pretty-print v1)
          (errorf #f "third bytevector=? failed for n=~s and i=~s (see output for vector)" n i)))))
)

(mat r6rs-bytevector-examples
  (equal?
    (let ([b (u8-list->bytevector '(1 2 3 4 5 6 7 8))])
      (bytevector-copy! b 0 b 3 4)
      (bytevector->u8-list b))
    '(1 2 3 1 2 3 4 8))


  (equal?
    (let ([b1 (make-bytevector 16 -127)]
          [b2 (make-bytevector 16 255)])
      (list
        (bytevector-s8-ref b1 0)
        (bytevector-u8-ref b1 0)
        (bytevector-s8-ref b2 0)
        (bytevector-u8-ref b2 0)))
    '(-127 129 -1 255))

  (equal?
    (let ([b (make-bytevector 16 -127)])
      (bytevector-s8-set! b 0 -126)
      (bytevector-u8-set! b 1 246)
      (list
        (bytevector-s8-ref b 0)
        (bytevector-u8-ref b 0)
        (bytevector-s8-ref b 1)
        (bytevector-u8-ref b 1)))
    '(-126 130 -10 246))

  (begin
    (define $bv (make-bytevector 16 -127))
    (bytevector? $bv))

  (eqv?
    (begin
      (bytevector-uint-set! $bv 0 (- (expt 2 128) 3)
        (endianness little) 16)
      (bytevector-uint-ref $bv 0 (endianness little) 16))
    #xfffffffffffffffffffffffffffffffd)

  (eqv? (bytevector-sint-ref $bv 0 (endianness little) 16) -3)

  (equal?
    (bytevector->u8-list $bv)
    '(253 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))

  (eqv?
    (begin
      (bytevector-uint-set! $bv 0 (- (expt 2 128) 3)
        (endianness big) 16)
      (bytevector-uint-ref $bv 0 (endianness big) 16))
    #xfffffffffffffffffffffffffffffffd)

  (eqv? (bytevector-sint-ref $bv 0 (endianness big) 16) -3)

  (equal?
    (bytevector->u8-list $bv)
    '(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253))

  (equal?
    (let ([b (u8-list->bytevector '(1 2 3 255 1 2 1 2))])
      (bytevector->sint-list b (endianness little) 2))
    '(513 -253 513 513))

  (equal?
    (let ([b (u8-list->bytevector '(1 2 3 255 1 2 1 2))])
      (bytevector->uint-list b (endianness little) 2))
    '(513 65283 513 513))

  (begin
    (define $bv
      (u8-list->bytevector
        '(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253)))
    (bytevector? $bv))

  (eqv? (bytevector-u16-ref $bv 14 (endianness little)) 65023)
  (eqv? (bytevector-s16-ref $bv 14 (endianness little)) -513)
  (eqv? (bytevector-u16-ref $bv 14 (endianness big)) 65533)
  (eqv? (bytevector-s16-ref $bv 14 (endianness big)) -3)

  (eqv?
    (begin
      (bytevector-u16-set! $bv 0 12345 (endianness little))
      (bytevector-u16-ref $bv 0 (endianness little)))
    12345)

  (eqv?
    (begin
      (bytevector-u16-native-set! $bv 0 12345)
      (bytevector-u16-native-ref $bv 0))
    12345)

  (and (memv (bytevector-u16-ref $bv 0 (endianness little)) '(12345 14640)) #t)

  (begin
    (define $bv
      (u8-list->bytevector
        '(255 255 255 255 255 255 255 255
          255 255 255 255 255 255 255 253)))
    (bytevector? $bv))

  (eqv? (bytevector-u32-ref $bv 12 (endianness little)) 4261412863)
  (eqv? (bytevector-s32-ref $bv 12 (endianness little)) -33554433)
  (eqv? (bytevector-u32-ref $bv 12 (endianness big)) 4294967293)
  (eqv? (bytevector-s32-ref $bv 12 (endianness big)) -3)

  (begin
    (define $bv
      (u8-list->bytevector
        '(255 255 255 255 255 255 255 255
          255 255 255 255 255 255 255 253)))
    (bytevector? $bv))

  (eqv? (bytevector-u64-ref $bv 8 (endianness little)) '18302628885633695743)
  (eqv? (bytevector-s64-ref $bv 8 (endianness little)) '-144115188075855873)
  (eqv? (bytevector-u64-ref $bv 8 (endianness big)) '18446744073709551613)
  (eqv? (bytevector-s64-ref $bv 8 (endianness big)) '-3)
)

(mat refimpl-tests
  ; rkd: the following tests are adapted from the bytevector reference
  ; implementation tests bytevector-tests.sch, which is:
  
  ; Copyright 2007 William D Clinger.
  ;
  ; Permission to copy this software, in whole or in part, to use this
  ; software for any lawful purpose, and to redistribute this software
  ; is granted subject to the restriction that all copies made of this
  ; software must include this copyright notice in full.
  ;
  ; I also request that you send me a copy of any improvements that you
  ; make to this software so that they may be incorporated within it to
  ; the benefit of the Scheme community.

  ; rkd: commented out some tests (look for "rkd") because they are
  ; implementation-dependent or require non-R6RS functionality or behavior.
  (begin
    ; rkd: writing code to a file first to get useful file positions for errors
    (with-output-to-file "testfile-bytevector.ss"
      (lambda ()
        (pretty-print '
          (define (bytevector-refimpl-tests)
            (define *random-stress-tests* 100)
            (define *random-stress-test-max-size* 50)

            ; rkd: rewrote to support for our test infrastructure
            (define okay? #t)
            (define-syntax test
              (syntax-rules (=> error)
                ((test exp => result)
                 (guard (c [#t (display-condition c) (newline) (set! okay? #f)])
                   (unless (equal? exp 'result) (syntax-error #'exp "failed"))))))

            (define (basic-bytevector-tests)
              (test (endianness big) => big)
              (test (endianness little) => little)

              (test (or (eq? (native-endianness) 'big)
                        (eq? (native-endianness) 'little)) => #t)

              (test (bytevector? (vector)) => #f)
              (test (bytevector? (make-bytevector 3)) => #t)

              (test (bytevector-length (make-bytevector 44)) => 44)

              (test (let ((b1 (make-bytevector 16 -127))
                          (b2 (make-bytevector 16 255)))
                      (list
                        (bytevector-s8-ref b1 0)
                        (bytevector-u8-ref b1 0)
                        (bytevector-s8-ref b2 0)
                        (bytevector-u8-ref b2 0))) => (-127 129 -1 255))

              (test (let ((b (make-bytevector 16 -127)))
                      (bytevector-s8-set! b 0 -126)
                      (bytevector-u8-set! b 1 246)
                      (list
                        (bytevector-s8-ref b 0)
                        (bytevector-u8-ref b 0)
                        (bytevector-s8-ref b 1)
                        (bytevector-u8-ref b 1))) => (-126 130 -10 246))

              (let ()
                (define b (make-bytevector 16 -127))
                (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)

                (test (bytevector-uint-ref b 0 (endianness little) 16)
                  => #xfffffffffffffffffffffffffffffffd)

                (test (bytevector-sint-ref b 0 (endianness little) 16) => -3)

                (test (bytevector->u8-list b)
                  => (253 255 255 255 255 255 255 255
                       255 255 255 255 255 255 255 255))

                (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness big) 16)

                (test (bytevector-uint-ref b 0 (endianness big) 16)
                  => #xfffffffffffffffffffffffffffffffd)

                (test (bytevector-sint-ref b 0 (endianness big) 16) => -3)

                (test (bytevector->u8-list b)
                  => (255 255 255 255 255 255 255 255
                       255 255 255 255 255 255 255 253)))

              (let ()
                (define b
                  (u8-list->bytevector
                    '(255 255 255 255 255 255 255 255
                       255 255 255 255 255 255 255 253)))

                (test (bytevector-u16-ref b 14 (endianness little)) => 65023)

                (test (bytevector-s16-ref b 14 (endianness little)) => -513)

                (test (bytevector-u16-ref b 14 (endianness big)) => 65533)

                (test (bytevector-s16-ref b 14 (endianness big)) => -3)

                (bytevector-u16-set! b 0 12345 (endianness little))

                (test (bytevector-u16-ref b 0 (endianness little)) => 12345)

                (bytevector-u16-native-set! b 0 12345)

                (test (bytevector-u16-native-ref b 0) => 12345))

              (let ()
                (define b
                  (u8-list->bytevector
                    '(255 255 255 255 255 255 255 255
                       255 255 255 255 255 255 255 253)))

                (test (bytevector-u32-ref b 12 (endianness little)) => 4261412863)

                (test (bytevector-s32-ref b 12 (endianness little)) => -33554433)

                (test (bytevector-u32-ref b 12 (endianness big)) => 4294967293)

                (test (bytevector-s32-ref b 12 (endianness big)) => -3))

              (let ()
                (define b
                  (u8-list->bytevector
                    '(255 255 255 255 255 255 255 255
                       255 255 255 255 255 255 255 253)))

                (test (bytevector-u64-ref b 8 (endianness little))
                  => 18302628885633695743)

                (test (bytevector-s64-ref b 8 (endianness little))
                  => -144115188075855873)

                (test (bytevector-u64-ref b 8 (endianness big))
                  => 18446744073709551613)

                (test (bytevector-s64-ref b 8 (endianness big)) => -3))

              (let ()
                (define b1 (u8-list->bytevector '(255 2 254 3 255)))
                (define b2 (u8-list->bytevector '(255 3 254 2 255)))
                (define b3 (u8-list->bytevector '(255 3 254 2 255)))
                (define b4 (u8-list->bytevector '(255 3 255)))

                (test (bytevector=? b1 b2) => #f)
                (test (bytevector=? b2 b3) => #t)
                (test (bytevector=? b3 b4) => #f)
                (test (bytevector=? b4 b3) => #f))

              (let ()
                (define b
                  (u8-list->bytevector
                    '(63 240 0 0 0 0 0 0)))

                (test (bytevector-ieee-single-ref b 4 'little) => 0.0)

                (test (bytevector-ieee-double-ref b 0 'big) => 1.0)

                (bytevector-ieee-single-native-set! b 4 3.0)

                (test (bytevector-ieee-single-native-ref b 4) => 3.0)

                (bytevector-ieee-double-native-set! b 0 5.0)

                (test (bytevector-ieee-double-native-ref b 0) => 5.0)

                (bytevector-ieee-double-set! b 0 1.75 'big)

                (test (bytevector->u8-list b) => (63 252 0 0 0 0 0 0)))

              (let ((b (make-bytevector 7 12)))
                (bytevector-fill! b 127)
                (test (bytevector->u8-list b) => (127 127 127 127 127 127 127)))

              (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
                (bytevector-copy! b 0 b 3 4)
                (test (bytevector->u8-list b) => (1 2 3 1 2 3 4 8))
                (test (bytevector=? b (bytevector-copy b)) => #t))

              (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
                (test (bytevector->sint-list b (endianness little) 2)
                  => (513 -253 513 513))
                (test (bytevector->uint-list b (endianness little) 2)
                  => (513 65283 513 513))))

            (define (ieee-bytevector-tests)

              (define (roundtrip x getter setter! k endness)
                (let ((b (make-bytevector 100)))
                  (setter! b k x endness)
                  (getter b k endness)))

              (define (->single x)
                (roundtrip
                  x bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big))

              (define (->double x)
                (roundtrip
                  x bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big))

              ; Single precision, offset 0, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               0 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
                => -0.2822580337524414)

              ; Single precision, offset 0, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               0 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
                => -0.2822580337524414)

              ; Single precision, offset 1, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               1 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
                => -0.2822580337524414)

              ; Single precision, offset 1, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               1 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
                => -0.2822580337524414)

              ; Single precision, offset 2, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               2 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
                => -0.2822580337524414)

              ; Single precision, offset 2, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               2 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
                => -0.2822580337524414)

              ; Single precision, offset 3, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               3 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
                => -0.2822580337524414)

              ; Single precision, offset 3, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-single-ref bytevector-ieee-single-set!
                               3 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
                => -0.2822580337524414)

              ; Double precision, offset 0, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               0 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
                => -0.2822580337524414)

              ; Double precision, offset 0, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               0 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
                => -0.2822580337524414)

              ; Double precision, offset 1, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               1 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
                => -0.2822580337524414)

              ; Double precision, offset 1, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               1 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
                => -0.2822580337524414)

              ; Double precision, offset 2, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               2 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
                => -0.2822580337524414)

              ; Double precision, offset 2, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               2 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
                => -0.2822580337524414)

              ; Double precision, offset 3, big-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               3 'big)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
                => -0.2822580337524414)

              ; Double precision, offset 3, little-endian

              (test (roundtrip
                      +inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
                => +inf.0)

              (test (roundtrip
                      -inf.0
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
                => -inf.0)

              (test (let ((x (roundtrip
                               +nan.0
                               bytevector-ieee-double-ref bytevector-ieee-double-set!
                               3 'little)))
                      (= x x))
                => #f)

              (test (roundtrip
                      1e10
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
                => 1e10)

              (test (roundtrip
                      -0.2822580337524414
                      bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
                => -0.2822580337524414)

              ; Denormalized numbers.

              (do ((x (expt .5 100) (* .5 x)))
                ((= x 0.0))
                (let ((y (->single x)))
                  (test (or (= y 0.0) (= x y)) => #t)))

              (do ((x (expt .5 100) (* .5 x)))
                ((= x 0.0))
                (let ((y (->double x)))
                  (test (= x y) => #t))))

            (define (string-bytevector-tests)

              ; rkd: rewrote to support for our test infrastructure
              (define-syntax test-roundtrip
                (syntax-rules ()
                  [(_ bvec tostring tobvec)
                   (let* ((s1 (tostring bvec))
                          (b2 (tobvec s1))
                          (s2 (tostring b2)))
                     (test (string=? s1 s2) => #t))]))

              (define random
                (letrec ((random14
                           (lambda (n)
                             (set! x (remainder (+ (* a x) c) (+ m 1)))
                             (remainder (quotient x 8) n)))
                         (a 701)
                         (x 1)
                         (c 743483)
                         (m 524287)
                         (loop
                           (lambda (q r n)
                             (if (zero? q)
                                 (remainder r n)
                                 (loop (quotient q 16384)
                                   (+ (* 16384 r) (random14 16384))
                                   n)))))
                  (lambda (n)
                    (if (< n 16384)
                        (random14 n)
                        (loop (quotient n 16384) (random14 16384) n)))))

              ; Returns a random bytevector of length up to n.

              (define (random-bytevector n)
                (let* ((n (random n))
                       (bv (make-bytevector n)))
                  (do ((i 0 (+ i 1)))
                    ((= i n) bv)
                    (bytevector-u8-set! bv i (random 256)))))

              ; Returns a random bytevector of even length up to n.

              (define (random-bytevector2 n)
                (let* ((n (random n))
                       (n (if (odd? n) (+ n 1) n))
                       (bv (make-bytevector n)))
                  (do ((i 0 (+ i 1)))
                    ((= i n) bv)
                    (bytevector-u8-set! bv i (random 256)))))

              ; Returns a random bytevector of multiple-of-4 length up to n.

              (define (random-bytevector4 n)
                (let* ((n (random n))
                       (n (* 4 (round (/ n 4))))
                       (bv (make-bytevector n)))
                  (do ((i 0 (+ i 1)))
                    ((= i n) bv)
                    (bytevector-u8-set! bv i (random 256)))))

              (test (bytevector=? (string->utf8 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
                      '#vu8(#x6b
                             #x7f
                             #b11000010 #b10000000
                             #b11011111 #b10111111
                             #b11100000 #b10100000 #b10000000
                             #b11101111 #b10111111 #b10111111))
                => #t)

              (test (bytevector=? (string->utf8 "\x010000;\x10ffff;")
                      '#vu8(#b11110000 #b10010000 #b10000000 #b10000000
                             #b11110100 #b10001111 #b10111111 #b10111111))
                => #t)

              (test (string=? (utf8->string '#vu8(#x61                             ; a
                                                   #xc0 #x62                        ; ?b
                                                   #xc1 #x63                        ; ?c
                                                   #xc2 #x64                        ; ?d
                                                   #x80 #x65                        ; ?e
                                                   #xc0 #xc0 #x66                   ; ??f
                                                   #xe0 #x67                        ; ?g
                                                   ))
                      "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e\xfffd;\xfffd;f\xfffd;g")
                => #t)

              #; ; rkd: implementation dependent number of replacement characters
              (test (string=? (utf8->string '#vu8(#xe0 #x80 #x80 #x68              ; ???h
                                                   #xe0 #xc0 #x80 #x69              ; ???i
                                                   #xf0 #x6a                        ; ?j
                                                   ))
                      "\xfffd;\xfffd;\xfffd;h\xfffd;\xfffd;\xfffd;i\xfffd;j")
                => #t)

              #; ; rkd: implementation dependent number of replacement characters
              (test (string=? (utf8->string '#vu8(#x61                             ; a
                                                   #xf0 #x80 #x80 #x80 #x62         ; ????b
                                                   #xf0 #x90 #x80 #x80 #x63         ; .c
                                                   ))
                      "a\xfffd;\xfffd;\xfffd;\xfffd;b\x10000;c")
                => #t)

              (test (string=? (utf8->string '#vu8(#x61                             ; a
                                                   #xf0 #xbf #xbf #xbf #x64         ; .d
                                                   #xf0 #xbf #xbf #x65              ; ?e
                                                   #xf0 #xbf #x66                   ; ?f
                                                   ))
                      "a\x3ffff;d\xfffd;e\xfffd;f")
                => #t)

              #; ; rkd: implementation dependent number of replacement characters
              (test (string=? (utf8->string '#vu8(#x61                             ; a
                                                   #xf4 #x8f #xbf #xbf #x62         ; .b
                                                   #xf4 #x90 #x80 #x80 #x63         ; ????c
                                                   ))

                      "a\x10ffff;b\xfffd;\xfffd;\xfffd;\xfffd;c")
                => #t)

              (test (string=? (utf8->string '#vu8(#x61                             ; a
                                                   #xf5 #x80 #x80 #x80 #x64         ; ????d
                                                   ))

                      "a\xfffd;\xfffd;\xfffd;\xfffd;d")
                => #t)

              ; ignores BOM signature

              (test (string=? (utf8->string '#vu8(#xef #xbb #xbf #x61 #x62 #x63 #x64))
                      "abcd")
                => #t)

              (test-roundtrip (random-bytevector 10) utf8->string string->utf8)

              (do ((i 0 (+ i 1)))
                ((= i *random-stress-tests*))
                (test-roundtrip (random-bytevector *random-stress-test-max-size*)
                  utf8->string string->utf8))

              (test (bytevector=? (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
                      '#vu8(#x00 #x6b
                             #x00 #x7f
                             #x00 #x80
                             #x07 #xff
                             #x08 #x00
                             #xff #xff))
                => #t)

              (test (bytevector=? (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                                    'little)
                      '#vu8(#x6b #x00
                             #x7f #x00
                             #x80 #x00
                             #xff #x07
                             #x00 #x08
                             #xff #xff))
                => #t)

              (test (bytevector=? (string->utf16 "\x010000;\xfdcba;\x10ffff;")
                      '#vu8(#xd8 #x00 #xdc #x00
                             #xdb #xb7 #xdc #xba
                             #xdb #xff #xdf #xff))
                => #t)

              (test (bytevector=? (string->utf16 "\x010000;\xfdcba;\x10ffff;" 'little)
                      '#vu8(#x00 #xd8 #x00 #xdc
                             #xb7 #xdb #xba #xdc
                             #xff #xdb #xff #xdf))
                => #t)

              (test (bytevector=? (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd")
                      (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd" 'big))
                => #t)

              #; ; rkd: utf16->string requires endianness argument
              (test (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                      (utf16->string
                        '#vu8(#x00 #x6b
                               #x00 #x7f
                               #x00 #x80
                               #x07 #xff
                               #x08 #x00
                               #xff #xff)))
                => #t)

              (test (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                      (utf16->string
                        '#vu8(#x00 #x6b
                               #x00 #x7f
                               #x00 #x80
                               #x07 #xff
                               #x08 #x00
                               #xff #xff)
                        'big))
                => #t)

              #; ; rkd: utf16->string requires endianness argument
              (test (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                      (utf16->string
                        '#vu8(#xfe #xff     ; big-endian BOM
                               #x00 #x6b
                               #x00 #x7f
                               #x00 #x80
                               #x07 #xff
                               #x08 #x00
                               #xff #xff)))
                => #t)

              (test (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                      (utf16->string
                        '#vu8(#x6b #x00
                               #x7f #x00
                               #x80 #x00
                               #xff #x07
                               #x00 #x08
                               #xff #xff)
                        'little))
                => #t)

              #; ; rkd: utf16->string requires endianness argument
              (test (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                      (utf16->string
                        '#vu8(#xff #xfe     ; little-endian BOM
                               #x6b #x00
                               #x7f #x00
                               #x80 #x00
                               #xff #x07
                               #x00 #x08
                               #xff #xff)))
                => #t)

              (let ((tostring        utf16->string)
                    (tostring-big    (lambda (bv) (utf16->string bv 'big)))
                    (tostring-little (lambda (bv) (utf16->string bv 'little)))
                    (tobvec          string->utf16)
                    (tobvec-big      (lambda (s) (string->utf16 s 'big)))
                    (tobvec-little   (lambda (s) (string->utf16 s 'little))))

                (do ((i 0 (+ i 1)))
                  ((= i *random-stress-tests*))
                  #; ; rkd: utf16->string requires endianness argument
                  (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                    tostring tobvec)
                  (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                    tostring-big tobvec-big)
                  (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                    tostring-little tobvec-little)))

              (test (bytevector=? (string->utf32 "abc")
                      '#vu8(#x00 #x00 #x00 #x61
                             #x00 #x00 #x00 #x62
                             #x00 #x00 #x00 #x63))
                => #t)

              (test (bytevector=? (string->utf32 "abc" 'big)
                      '#vu8(#x00 #x00 #x00 #x61
                             #x00 #x00 #x00 #x62
                             #x00 #x00 #x00 #x63))
                => #t)

              (test (bytevector=? (string->utf32 "abc" 'little)
                      '#vu8(#x61 #x00 #x00 #x00
                             #x62 #x00 #x00 #x00
                             #x63 #x00 #x00 #x00))
                => #t)

              #; ; rkd: utf32->string requires endianness argument
              (test (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                      (utf32->string
                        '#vu8(#x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)))
                => #t)

              (test (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                      (utf32->string
                        '#vu8(#x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)
                        'big))
                => #t)

              #; ; rkd: utf32->string requires endianness argument
              (test (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                      (utf32->string
                        '#vu8(#x00 #x00 #xfe #xff   ; big-endian BOM
                               #x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)))
                => #t)

              (test (string=? "\xfeff;a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                      (utf32->string
                        '#vu8(#x00 #x00 #xfe #xff   ; big-endian BOM
                               #x00 #x00 #x00 #x61
                               #x00 #x00 #xd9 #x00
                               #x00 #x00 #x00 #x62
                               #x00 #x00 #xdd #xab
                               #x00 #x00 #x00 #x63
                               #x00 #x11 #x00 #x00
                               #x00 #x00 #x00 #x64
                               #x01 #x00 #x00 #x65
                               #x00 #x00 #x00 #x65)
                        'big
                        ; rkd: added endianness-mandatory? flag
                        #t))
                => #t)

              (test (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                      (utf32->string
                        '#vu8(#x61 #x00 #x00 #x00
                               #x00 #xd9 #x00 #x00
                               #x62 #x00 #x00 #x00
                               #xab #xdd #x00 #x00
                               #x63 #x00 #x00 #x00
                               #x00 #x00 #x11 #x00
                               #x64 #x00 #x00 #x00
                               #x65 #x00 #x00 #x01
                               #x65 #x00 #x00 #x00)
                        'little))
                => #t)

              #; ; rkd: utf32->string requires endianness argument
              (test (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                      (utf32->string
                        '#vu8(#xff #xfe #x00 #x00   ; little-endian BOM
                               #x61 #x00 #x00 #x00
                               #x00 #xd9 #x00 #x00
                               #x62 #x00 #x00 #x00
                               #xab #xdd #x00 #x00
                               #x63 #x00 #x00 #x00
                               #x00 #x00 #x11 #x00
                               #x64 #x00 #x00 #x00
                               #x65 #x00 #x00 #x01
                               #x65 #x00 #x00 #x00)))
                => #t)

              (test (string=? "\xfeff;a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                      (utf32->string
                        '#vu8(#xff #xfe #x00 #x00   ; little-endian BOM
                               #x61 #x00 #x00 #x00
                               #x00 #xd9 #x00 #x00
                               #x62 #x00 #x00 #x00
                               #xab #xdd #x00 #x00
                               #x63 #x00 #x00 #x00
                               #x00 #x00 #x11 #x00
                               #x64 #x00 #x00 #x00
                               #x65 #x00 #x00 #x01
                               #x65 #x00 #x00 #x00)
                        'little
                        ; rkd: added endianness-mandatory? flag
                        #t))
                => #t)

              (let ((tostring        utf32->string)
                    (tostring-big    (lambda (bv) (utf32->string bv 'big)))
                    (tostring-little (lambda (bv) (utf32->string bv 'little)))
                    (tobvec          string->utf32)
                    (tobvec-big      (lambda (s) (string->utf32 s 'big)))
                    (tobvec-little   (lambda (s) (string->utf32 s 'little))))

                (do ((i 0 (+ i 1)))
                  ((= i *random-stress-tests*))
                  #; ; rkd: utf32->string requires endianness argument
                  (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                    tostring tobvec)
                  (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                    tostring-big tobvec-big)
                  (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                    tostring-little tobvec-little)))

              )

            ; Tests string <-> bytevector conversion on strings
            ; that contain every Unicode scalar value.
            (define (exhaustive-string-bytevector-tests)

              ; Tests throughout an inclusive range.

              (define (test-char-range lo hi tostring tobytevector)
                (let* ((n (+ 1 (- hi lo)))
                       (s (make-string n))
                       (replacement-character (integer->char #xfffd)))
                  (do ((i lo (+ i 1)))
                    ((> i hi))
                    (let ((c (if (or (<= 0 i #xd7ff)
                                     (<= #xe000 i #x10ffff))
                                 (integer->char i)
                                 replacement-character)))
                      (string-set! s (- i lo) c)))
                  (test (string=? (tostring (tobytevector s)) s) => #t)))

              (define (test-exhaustively name tostring tobytevector)
                (display "Testing ")
                (display name)
                (display " conversions...")
                (newline)
                (test-char-range 0 #xffff tostring tobytevector)
                (test-char-range #x10000 #x1ffff tostring tobytevector)
                (test-char-range #x20000 #x2ffff tostring tobytevector)
                (test-char-range #x30000 #x3ffff tostring tobytevector)
                (test-char-range #x40000 #x4ffff tostring tobytevector)
                (test-char-range #x50000 #x5ffff tostring tobytevector)
                (test-char-range #x60000 #x6ffff tostring tobytevector)
                (test-char-range #x70000 #x7ffff tostring tobytevector)
                (test-char-range #x80000 #x8ffff tostring tobytevector)
                (test-char-range #x90000 #x9ffff tostring tobytevector)
                (test-char-range #xa0000 #xaffff tostring tobytevector)
                (test-char-range #xb0000 #xbffff tostring tobytevector)
                (test-char-range #xc0000 #xcffff tostring tobytevector)
                (test-char-range #xd0000 #xdffff tostring tobytevector)
                (test-char-range #xe0000 #xeffff tostring tobytevector)
                (test-char-range #xf0000 #xfffff tostring tobytevector)
                (test-char-range #x100000 #x10ffff tostring tobytevector))

              ; Feel free to replace this with your favorite timing macro.

              (define (timeit x) x)

              (timeit (test-exhaustively "UTF-8" utf8->string string->utf8))

              #; ; rkd: utf16->string requires endianness argument
              (timeit (test-exhaustively "UTF-16" utf16->string string->utf16))

              (timeit (test-exhaustively "UTF-16BE"
                        (lambda (bv) (utf16->string bv 'big))
                        (lambda (s) (string->utf16 s 'big))))

              (timeit (test-exhaustively "UTF-16LE"
                        (lambda (bv) (utf16->string bv 'little))
                        (lambda (s) (string->utf16 s 'little))))

              #; ; rkd: utf32->string requires endianness argument
              (timeit (test-exhaustively "UTF-32" utf32->string string->utf32))

              (timeit (test-exhaustively "UTF-32BE"
                        (lambda (bv) (utf32->string bv 'big))
                        (lambda (s) (string->utf32 s 'big))))

              (timeit (test-exhaustively "UTF-32LE"
                        (lambda (bv) (utf32->string bv 'little))
                        (lambda (s) (string->utf32 s 'little)))))

            (basic-bytevector-tests)
            (ieee-bytevector-tests)
            (string-bytevector-tests)
            (exhaustive-string-bytevector-tests)
            okay?)))
      'replace)
    #t)
  (begin
    (load "testfile-bytevector.ss")
    #t)
  (bytevector-refimpl-tests)
)

(mat tspl/csug-examples
  (equal? '#vu8(1 2 3) #vu8(1 2 3))
  (equal? #vu8(1 2 3) #vu8(1 2 3))
  (equal? #vu8(#x3f #x7f #xbf #xff) #vu8(63 127 191 255))
  (equal? (endianness little) 'little)
  (equal? (endianness big) 'big)
  (error? (endianness "spam"))
  (equal? (symbol? (native-endianness)) #t)
  (equal? (bytevector? #vu8()) #t)
  (equal? (bytevector? '#()) #f)
  (equal? (bytevector? "abc") #f)
  (equal? (bytevector) #vu8())
  (equal? (bytevector 1 3 5) #vu8(1 3 5))
  (equal? (bytevector -1 -3 -5) #vu8(255 253 251))
  (equal? (make-bytevector 0) #vu8())
  (equal? (make-bytevector 0 7) #vu8())
  (equal? (make-bytevector 5 7) #vu8(7 7 7 7 7))
  (equal? (make-bytevector 5 -7) #vu8(249 249 249 249 249))
  (equal? (bytevector-length #vu8()) 0)
  (equal? (bytevector-length #vu8(1 2 3)) 3)
  (equal? (bytevector-length (make-bytevector 300)) 300)
  (equal? (bytevector=? #vu8() #vu8()) #t)
  (equal? (bytevector=? (make-bytevector 3 0) #vu8(0 0 0)) #t)
  (equal? (bytevector=? (make-bytevector 5 0) #vu8(0 0 0)) #f)
  (equal? (bytevector=? #vu8(1 127 128 255) #vu8(255 128 127 1)) #f)
  (equal?
    (let ([v (make-bytevector 6)])
      (bytevector-fill! v 255)
      v)
    #vu8(255 255 255 255 255 255))
  
  (equal?
    (let ([v (make-bytevector 6)])
      (bytevector-fill! v -128)
      v)
    #vu8(128 128 128 128 128 128))
  (equal? (bytevector-copy #vu8(1 127 128 255)) #vu8(1 127 128 255))
  
  (equal?
    (let ([v #vu8(1 127 128 255)])
      (eq? v (bytevector-copy v)))
    #f)
  (begin
    (define $v1 #vu8(31 63 95 127 159 191 223 255))
    (define $v2 (make-bytevector 10 0))
    (bytevector-copy! $v1 2 $v2 1 4)
    (equal? $v2 #vu8(0 95 127 159 191 0 0 0 0 0)))
   
  (begin
    (bytevector-copy! $v1 5 $v2 7 3)
    (equal? $v2 #vu8(0 95 127 159 191 0 0 191 223 255)))
   
  (begin
    (bytevector-copy! $v2 3 $v2 0 6)
    (equal? $v2 #vu8(159 191 0 0 191 223 0 191 223 255)))
   
  (begin
    (bytevector-copy! $v2 0 $v2 1 9)
    (equal? $v2 #vu8(159 159 191 0 0 191 223 0 191 223)))

  (equal? (bytevector-u8-ref #vu8(1 127 128 255) 0) 1)
  (equal? (bytevector-u8-ref #vu8(1 127 128 255) 2) 128)
  (equal? (bytevector-u8-ref #vu8(1 127 128 255) 3) 255)
  (equal? (bytevector-s8-ref #vu8(1 127 128 255) 0) 1)
  (equal? (bytevector-s8-ref #vu8(1 127 128 255) 1) 127)
  (equal? (bytevector-s8-ref #vu8(1 127 128 255) 2) -128)
  (equal? (bytevector-s8-ref #vu8(1 127 128 255) 3) -1)
  (equal?
    (let ([v (make-bytevector 5 -1)])
      (bytevector-u8-set! v 2 128)
      v)
    #vu8(255 255 128 255 255))
  (equal?
    (let ([v (make-bytevector 4 0)])
      (bytevector-s8-set! v 1 100)
      (bytevector-s8-set! v 2 -100)
      v)
    #vu8(0 100 156 0))
  (equal? (bytevector->u8-list (make-bytevector 0)) '())
  (equal? (bytevector->u8-list #vu8(1 127 128 255)) '(1 127 128 255))
  
  (equal?
    (let ([v #vu8(1 2 3 255)])
      (apply * (bytevector->u8-list v)))
    1530)
  
  (equal? (bytevector->s8-list (make-bytevector 0)) '())
  (equal? (bytevector->s8-list #vu8(1 127 128 255)) '(1 127 -128 -1))
  
  (equal?
    (let ([v #vu8(1 2 3 255)])
      (apply * (bytevector->s8-list v)))
    -6)
  (equal? (u8-list->bytevector '()) #vu8())
  (equal? (u8-list->bytevector '(1 127 128 255)) #vu8(1 127 128 255))
  
  (equal?
    (let ([v #vu8(1 2 3 4 5)])
      (let ([ls (bytevector->u8-list v)])
        (u8-list->bytevector (map * ls ls))))
    #vu8(1 4 9 16 25))
  
  (equal? (s8-list->bytevector '()) #vu8())
  (equal? (s8-list->bytevector '(1 127 -128 -1)) #vu8(1 127 128 255))
  
  (equal?
    (let ([v #vu8(1 2 3 4 5)])
      (let ([ls (bytevector->s8-list v)])
        (s8-list->bytevector (map - ls))))
    #vu8(255 254 253 252 251))
  (begin
    (define $v #vu8(#x12 #x34 #xfe #x56 #xdc #xba #x78 #x98))
    (bytevector? $v))
  
  (equal?
    (case (native-endianness)
      [(big)
       (list
         (equal? (bytevector-u16-native-ref $v 2) #xfe56)
         (equal? (bytevector-s16-native-ref $v 2) #x-1aa)
         (equal? (bytevector-s16-native-ref $v 6) #x7898)
  
         (equal? (bytevector-u32-native-ref $v 0) #x1234fe56)
         (equal? (bytevector-s32-native-ref $v 0) #x1234fe56)
         (equal? (bytevector-s32-native-ref $v 4) #x-23458768)
  
         (equal? (bytevector-u64-native-ref $v 0) #x1234fe56dcba7898)
         (equal? (bytevector-s64-native-ref $v 0) #x1234fe56dcba7898))]
      [(little)
       (list
         (equal? (bytevector-u16-native-ref $v 2) #x56fe)
         (equal? (bytevector-s16-native-ref $v 2) #x56fe)
         (equal? (bytevector-s16-native-ref $v 6) #x-6788)
  
         (equal? (bytevector-u32-native-ref $v 0) #x56fe3412)
         (equal? (bytevector-s32-native-ref $v 0) #x56fe3412)
         (equal? (bytevector-s32-native-ref $v 4) #x-67874524)
  
         (equal? (bytevector-u64-native-ref $v 0) #x9878badc56fe3412)
         (equal? (bytevector-s64-native-ref $v 0) #x-67874523a901cbee))]
      [else (errorf #f "mat does not handle endianness ~s" (native-endianness))])
    '(#t #t #t #t #t #t #t #t))

  (let ()
    (define v (make-bytevector 8 0))
    (bytevector-u16-native-set! v 0 #xfe56)
    (bytevector-s16-native-set! v 2 #x-1aa)
    (bytevector-s16-native-set! v 4 #x7898)
    (case (native-endianness)
      [(big) (equal? v #vu8(#xfe #x56 #xfe #x56 #x78 #x98 #x00 #x00))]
      [(little) (equal? v #vu8(#x56 #xfe #x56 #xfe #x98 #x78 #x00 #x00))]
      [else (errorf #f "mat does not handle endianness ~s" (native-endianness))]))
  
  (let ()
    (define v (make-bytevector 16 0))
    (bytevector-u32-native-set! v 0 #x1234fe56)
    (bytevector-s32-native-set! v 4 #x1234fe56)
    (bytevector-s32-native-set! v 8 #x-23458768)
    (case (native-endianness)
      [(big) (equal? v #vu8(#x12 #x34 #xfe #x56 #x12 #x34 #xfe #x56
                            #xdc #xba #x78 #x98 #x00 #x00 #x00 #x00))]
      [(little) (equal? v #vu8(#x56 #xfe #x34 #x12 #x56 #xfe #x34 #x12
                               #x98 #x78 #xba #xdc #x00 #x00 #x00 #x00))]
      [else (errorf #f "mat does not handle endianness ~s" (native-endianness))]))
  
  (let ()
    (define v (make-bytevector 24 0))
    (bytevector-u64-native-set! v 0 #x1234fe56dcba7898)
    (bytevector-s64-native-set! v 8 #x1234fe56dcba7898)
    (bytevector-s64-native-set! v 16 #x-67874523a901cbee)
    (case (native-endianness)
      [(big) (equal? v #vu8(#x12 #x34 #xfe #x56 #xdc #xba #x78 #x98
                            #x12 #x34 #xfe #x56 #xdc #xba #x78 #x98
                            #x98 #x78 #xba #xdc #x56 #xfe #x34 #x12))]
      [(little) (equal? v #vu8(#x98 #x78 #xba #xdc #x56 #xfe #x34 #x12
                               #x98 #x78 #xba #xdc #x56 #xfe #x34 #x12
                               #x12 #x34 #xfe #x56 #xdc #xba #x78 #x98))]
      [else (errorf #f "mat does not handle endianness ~s" (native-endianness))]))

  (begin
    (define $v #vu8(#x12 #x34 #xfe #x56 #xdc #xba #x78 #x98 #x9a #x76))
    (bytevector? $v))
  (equal? (bytevector-u16-ref $v 0 (endianness big)) #x1234)
  (equal? (bytevector-s16-ref $v 1 (endianness big)) #x34fe)
  (equal? (bytevector-s16-ref $v 5 (endianness big)) #x-4588)
  
  (equal? (bytevector-u32-ref $v 2 'big) #xfe56dcba)
  (equal? (bytevector-s32-ref $v 3 'big) #x56dcba78)
  (equal? (bytevector-s32-ref $v 4 'big) #x-23458768)
  
  (equal? (bytevector-u64-ref $v 0 'big) #x1234fe56dcba7898)
  (equal? (bytevector-s64-ref $v 1 'big) #x34fe56dcba78989a)
  
  (equal? (bytevector-u16-ref $v 0 (endianness little)) #x3412)
  (equal? (bytevector-s16-ref $v 1 (endianness little)) #x-1cc)
  (equal? (bytevector-s16-ref $v 5 (endianness little)) #x78ba)
  
  (equal? (bytevector-u32-ref $v 2 'little) #xbadc56fe)
  (equal? (bytevector-s32-ref $v 3 'little) #x78badc56)
  (equal? (bytevector-s32-ref $v 4 'little) #x-67874524)
  
  (equal? (bytevector-u64-ref $v 0 'little) #x9878badc56fe3412)
  (equal? (bytevector-s64-ref $v 1 'little) #x-6567874523a901cc)

  (let ()
    (define v (make-bytevector 8 0))
    (bytevector-u16-set! v 0 #xfe56 (endianness big))
    (bytevector-s16-set! v 3 #x-1aa (endianness little))
    (bytevector-s16-set! v 5 #x7898 (endianness big))
    (equal? v #vu8(#xfe #x56 #x0 #x56 #xfe #x78 #x98 #x0)))
  
  (let ()
    (define v (make-bytevector 16 0))
    (bytevector-u32-set! v 0 #x1234fe56 'little)
    (bytevector-s32-set! v 6 #x1234fe56 'big)
    (bytevector-s32-set! v 11 #x-23458768 'little)
    (equal? v #vu8(#x56 #xfe #x34 #x12 #x0 #x0
                   #x12 #x34 #xfe #x56 #x0
                   #x98 #x78 #xba #xdc #x0)))
  
  (let ()
    (define v (make-bytevector 28 0))
    (bytevector-u64-set! v 0 #x1234fe56dcba7898 'little)
    (bytevector-s64-set! v 10 #x1234fe56dcba7898 'big)
    (bytevector-s64-set! v 19 #x-67874523a901cbee 'big)
    (equal? v #vu8(#x98 #x78 #xba #xdc #x56 #xfe #x34 #x12 #x0 #x0
               #x12 #x34 #xfe #x56 #xdc #xba #x78 #x98 #x0
               #x98 #x78 #xba #xdc #x56 #xfe #x34 #x12 #x0)))

  (let ()
    (define v #vu8(#x12 #x34 #xfe #x56 #xdc #xba #x78 #x98 #x9a #x76))
    (and
      (equal? (bytevector-uint-ref v 0 'big 1) #x12)
      (equal? (bytevector-uint-ref v 0 'little 1) #x12)
      (equal? (bytevector-uint-ref v 1 'big 3) #x34fe56)
      (equal? (bytevector-uint-ref v 2 'little 7) #x9a9878badc56fe)
  
      (equal? (bytevector-sint-ref v 2 'big 1) #x-02)
      (equal? (bytevector-sint-ref v 1 'little 6) #x78badc56fe34)
      (equal? (bytevector-sint-ref v 2 'little 7) #x-6567874523a902)
  
      (equal? (bytevector-sint-ref (make-bytevector 1000 -1) 0 'big 1000) -1)))

  (let ()
    (define v (make-bytevector 5 0))
    (bytevector-uint-set! v 1 #x123456 (endianness big) 3)
    (equal? v #vu8(0 #x12 #x34 #x56 0)))
  
  (let ()
    (define v (make-bytevector 7 -1))
    (bytevector-sint-set! v 1 #x-8000000000 (endianness little) 5)
    (equal? v #vu8(#xff 0 0 0 0 #x80 #xff)))

  (equal? (bytevector->uint-list (make-bytevector 0) 'little 3) '())
  
  (equal?
    (let ([v #vu8(1 2 3 4 5 6)])
      (bytevector->uint-list v 'big 3))
    '(#x010203 #x040506))
  
  (equal?
    (let ([v (make-bytevector 80 -1)])
      (bytevector->sint-list v 'big 20))
    '(-1 -1 -1 -1))
  (equal? (uint-list->bytevector '() 'big 25) #vu8())
  (equal? (sint-list->bytevector '(0 -1) 'big 3) #vu8(0 0 0 #xff #xff #xff))
  
  (equal?
    (let ()
      (define (f size)
        (let ([ls (list (- (expt 2 (- (* 8 size) 1)))
                        (- (expt 2 (- (* 8 size) 1)) 1))])
          (sint-list->bytevector ls 'little size)))
      (f 6))
    #vu8(#x00 #x00 #x00 #x00 #x00 #x80 #xff #xff #xff #xff #xff #x7f))

  (begin
    (define $v (make-bytevector 8 0))
    (bytevector-ieee-single-native-set! $v 0 .125)
    (bytevector-ieee-single-native-set! $v 4 -3/2)
    (equal?
      (list
        (bytevector-ieee-single-native-ref $v 0)
        (bytevector-ieee-single-native-ref $v 4))
      '(0.125 -1.5)))
  
  (begin
    (bytevector-ieee-double-native-set! $v 0 1e23)
    (equal? (bytevector-ieee-double-native-ref $v 0) 1e23))

  (begin
    (define $v (make-bytevector 10 #xc7))
    (bytevector-ieee-single-set! $v 1 .125 'little)
    (bytevector-ieee-single-set! $v 6 -3/2 'big)
    (equal?
      (list
        (bytevector-ieee-single-ref $v 1 'little)
        (bytevector-ieee-single-ref $v 6 'big))
      '(0.125 -1.5)))
  (equal? $v #vu8(#xc7 #x0 #x0 #x0 #x3e #xc7 #xbf #xc0 #x0 #x0))
  
  (begin
    (bytevector-ieee-double-set! $v 1 1e23 'big)
    (equal? (bytevector-ieee-double-ref $v 1 'big) 1e23))
)

#;(mat bytevector-logical
 ; A reference implementation in scheme
  (begin
    (define $bytevector-blurp
      (lambda (f)
        (lambda (bv1 bv2)
          (let ([len1 (bytevector-length bv1)]
                [len2 (bytevector-length bv2)])
            (let ([len (max len1 len2)])
              (if (fx= len 0)
                  bv1
                  (let ([new (make-bytevector len)])
                    (define endianness 'big)
                    (define (uint-ref bv len)
                      (if (fx= len 0)
                          0
                          (bytevector-uint-ref bv 0 endianness len)))
                    (bytevector-uint-set! new 0
                      (f (uint-ref bv1 len1) (uint-ref bv2 len2))
                      endianness len)
                    new)))))))

    (define $bytevector-and ($bytevector-blurp bitwise-and))

    (define $bytevector-ior ($bytevector-blurp bitwise-ior))

    (define $bytevector-xor ($bytevector-blurp bitwise-xor))
    
    (define $bytevector-not 
      (lambda (bv)
        (let ([len (bytevector-length bv)])
          (if (fx= len 0)
              bv
              (let ([new (make-bytevector len)])
                #;
                (bytevector-uint-set! new 0
                  (- (- (expt 256 len) 1)
                     (bytevector-uint-ref bv 0 (native-endianness) len))
                  (native-endianness) len)
                (bytevector-sint-set! new 0
                  (bitwise-not
                    (bytevector-sint-ref bv 0 (native-endianness) len))
                  (native-endianness) len)
                new)))))

    (define $make-random-bytevector
      (lambda (len)
        (let ([bv (make-bytevector len)])
          (do ([n len (- n 1)])
              ((zero? n) bv)
              (bytevector-u8-set! bv (- n 1) (random 256))))))

    #t)

 ; Currently the reference implementation is the only implementation,
 ; so go ahead and use it for the tests and the random tests below.
  (define bytevector-and $bytevector-and)
  (define bytevector-ior $bytevector-ior)
  (define bytevector-xor $bytevector-xor)
  (define bytevector-not $bytevector-not)

  (error? (bytevector-not '#()))
  (error? (bytevector-not 75))
  (error? (bytevector-not #vu8(5) '#()))
  (error? (bytevector-not 75 #vu8(5)))
  (equal? (bytevector-not #vu8()) #vu8())
  (equal? (bytevector-not #vu8(23)) #vu8(232))
  (equal? (bytevector-not #vu8(23 129)) #vu8(232 126))
  (equal? (bytevector-not #vu8(23 129 99)) #vu8(232 126 156))
  (equal? (bytevector-not #vu8(#x7f #xff #xff #xff)) #vu8(128 0 0 0))
  (equal? (bytevector-not #vu8(#xff #xff #xff #xff)) #vu8(0 0 0 0))
  (equal?
    (bytevector-not #vu8(#x00 #x00 #x00 #x00))
    #vu8(#xff #xff #xff #xff))
  (equal? (bytevector-not #vu8(0 255 170 85)) #vu8(255 0 85 170))
  (equal?
    (bytevector-not #vu8(#x00 #x00 #x00 #x02))
    #vu8(#xff #xff #xff #xfd))
  (equal?
    (bytevector-not #vu8(#x0f #xff #xff #xff #xff #xff #xff #xff #xff #xff))
    #vu8(#xf0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

  (error? (bytevector-and '#()))
  (error? (bytevector-and 75))
  (error? (bytevector-and #vu8(5) '#()))
  (error? (bytevector-and 75 #vu8(5)))
  (equal?
    (bytevector-and #vu8() #vu8())
    #vu8())
  (equal?
    (bytevector-and #vu8(#xff #xff #xff) #vu8(#x54 #x27 #x86))
    #vu8(#x54 #x27 #x86))
  (equal?
    (bytevector-and #vu8(#x00 #x00 #x00) #vu8(#x54 #x27 #x86))
    #vu8(#x00 #x00 #x00))
  (equal?
    (bytevector-and #vu8(#x65 #x33 #xf0) #vu8(#x54 #x27 #x86))
    #vu8(#x44 #x23 #x80))
  (equal?
    (bytevector-and #vu8(#x65 #x33 #xf0 #x75 #x83 #x99 #x41)
                    #vu8(#x54 #x27 #x86 #x99 #x87 #x76 #x63))
    #vu8(#x44 #x23 #x80 #x11 #x83 #x10 #x41))
  (equal?
    (bytevector-and #vu8(#x65 #x33 #xf0 #x75 #x83 #x99)
                    #vu8(#x54 #x27 #x86 #x99 #x87 #x76))
    #vu8(#x44 #x23 #x80 #x11 #x83 #x10))
  (equal?
    (bytevector-and #vu8(#x0 #x0 #x0 #x0) #vu8(#x0 #x0 #x0 #x0))
    #vu8(#x0 #x0 #x0 #x0))
  (equal?
    (bytevector-and #vu8(#xff #xff #xff #xff) #vu8(#x0 #x0 #x0 #x0))
    #vu8(#x0 #x0 #x0 #x0))
  (equal?
    (bytevector-and #vu8(#x0 #x0 #x0 #x0) #vu8(#xff #xff #xff #xff))
    #vu8(#x0 #x0 #x0 #x0))
  (equal?
    (bytevector-and #vu8(20) #vu8(0))
    #vu8(0))
  (equal?
    (bytevector-and #vu8(20) #vu8(#xff))
    #vu8(20))
  (equal?
    (bytevector-and #vu8(#x0f #xff #xff #xff #xff #xff #xff #xff #xff #xff)
                    #vu8(#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff))
    #vu8(#x0f #xff #xff #xff #xff #xff #xff #xff #xff #xff))
  (equal?
    (bytevector-and #vu8(#x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
                    #vu8(#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff))
    #vu8(#x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11))
  (equal?
    (bytevector-and
      #vu8(#x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
      #vu8(#x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22))
    #vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)) 
  (equal?
    (bytevector-and
      #vu8(#x12 #x12 #x12 #x12 #x12 #x12 #x12 #x12 #x12 #x12 #x12 #x12 #x12)
      #vu8(#x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22))
    #vu8(#x02 #x02 #x02 #x02 #x02 #x02 #x02 #x02 #x02 #x02 #x02 #x02 #x02))
  (equal?
    (bytevector-and
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03)
      #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1f #x36 #x65 #x67))
    #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x30 #x01 #x03))
  (equal? 
    (bytevector-and
      #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1f #x36 #x65 #x67)
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03))
    #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x30 #x01 #x03))
 ; different length bytevectors, how should they work?
  (equal?
    (bytevector-and
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03)
      #vu8(#x1f #x36 #x65 #x67))
    #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x30 #x01 #x03))
  (equal? 
    (bytevector-and
      #vu8(#x1f #x36 #x65 #x67)
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03))
    #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x30 #x01 #x03))

  (error? (bytevector-ior '#()))
  (error? (bytevector-ior 75))
  (error? (bytevector-ior #vu8(5) '#()))
  (error? (bytevector-ior 75 #vu8(5)))
  (equal? 
    (bytevector-ior #vu8() #vu8())
    #vu8())
  (equal?
    (bytevector-ior #vu8(0 0 0) #vu8(0 0 0))
    #vu8(0 0 0))
  (equal?
    (bytevector-ior #vu8(#xff #xff #xff #xff) #vu8(0 0 0 0))
    #vu8(#xff #xff #xff #xff))
  (equal?
    (bytevector-ior #vu8(0 0 0 0) #vu8(#xff #xff #xff #xff))
    #vu8(#xff #xff #xff #xff))
  (equal?
    (bytevector-ior #vu8(#xff #xff #xff) #vu8(#x54 #x27 #x86))
    #vu8(#xff #xff #xff))
  (equal?
    (bytevector-ior #vu8(#x00 #x00 #x00) #vu8(#x54 #x27 #x86))
    #vu8(#x54 #x27 #x86))
  (equal?
    (bytevector-ior #vu8(#x65 #x33 #xf0) #vu8(#x54 #x27 #x86))
    #vu8(#x75 #x37 #xf6))
  (equal?
    (bytevector-ior #vu8(#x65 #x33 #xf0 #x75 #x83 #x99 #x41)
                    #vu8(#x54 #x27 #x86 #x99 #x87 #x76 #x63))
    #vu8(#x75 #x37 #xf6 #xfd #x87 #xff #x63))
  (equal?
    (bytevector-ior #vu8(#x65 #x33 #xf0 #x75 #x83 #x99)
                    #vu8(#x54 #x27 #x86 #x99 #x87 #x76))
    #vu8(#x75 #x37 #xf6 #xfd #x87 #xff))
  (equal?
    (bytevector-ior #vu8(20) #vu8(#xff))
    #vu8(#xff))
  (equal? 
    (bytevector-ior
      #vu8(#x1 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
      #vu8(#x2 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22))
    #vu8(#x3 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33))
  (equal?
    (bytevector-ior
      #vu8(#x1 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21)
      #vu8(#x2 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22))
    #vu8(#x3 #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23))
  (equal?
    (bytevector-ior
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03)
      #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1f #x36 #x65 #x67))
    #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x1f #xbe #xf5 #x67))
  (equal?
    (bytevector-ior
      #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1f #x36 #x65 #x67)
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03))
    #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x1f #xbe #xf5 #x67))
 ; different size bytevectors how should the work?
  (equal?
    (bytevector-ior
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03)
      #vu8(#x1f #x36 #x65 #x67))
    #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x1f #xbe #xf5 #x67))
  (equal?
    (bytevector-ior
      #vu8(#x1f #x36 #x65 #x67)
      #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x01 #xb8 #x91 #x03))
    #vu8(#x2 #xb2 #x25 #xd2 #x7f #x49 #xc1 #xfe #xd3 #x1f #xbe #xf5 #x67))

  (error? (bytevector-xor '#()))
  (error? (bytevector-xor 75))
  (error? (bytevector-xor #vu8(5) '#()))
  (error? (bytevector-xor 75 #vu8(5)))
  (equal?
    (bytevector-xor #vu8() #vu8())
    #vu8())
  (equal?
    (bytevector-xor #vu8(#xff #xff #xff) #vu8(#x00 #x00 #x00))
    #vu8(#xff #xff #xff))
  (equal?
    (bytevector-xor #vu8(#x00 #x00 #x00) #vu8(#xff #xff #xff))
    #vu8(#xff #xff #xff))
  (equal?
    (bytevector-xor #vu8(#xff #xff #xff) #vu8(#xff #xff #xff))
    #vu8(#x00 #x00 #x00))
  (equal?
    (bytevector-xor #vu8(#x0f #x0f #x0f #x0f) #vu8(#xff #xff #xff #xff))
    #vu8(#xf0 #xf0 #xf0 #xf0))
  (equal?
    (bytevector-xor #vu8(#x00 #x14) #vu8(#xff #xff))
    #vu8(#xff #xeb))
  (equal?
    (bytevector-xor
      #vu8(#x1 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
      #vu8(#x2 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22))
    #vu8(#x3 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33))
  (equal?
    (bytevector-xor
       #vu8(#x1 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21)
       #vu8(#x2 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22))
    #vu8(#x3 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03))
  (equal?
    (bytevector-xor
      #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x01 #xB8 #x91 #x03)
      #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1F #x36 #x65 #x67))
    #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x1E #x8E #xF4 #x64))
  (equal?
    (bytevector-xor
      #vu8(#x0 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x1F #x36 #x65 #x67)
      #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x01 #xB8 #x91 #x03))
    #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x1E #x8E #xF4 #x64))
 ; different length bytevectors: how should they work?
   (equal?
    (bytevector-xor
      #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x01 #xB8 #x91 #x03)
      #vu8(#x1F #x36 #x65 #x67))
    #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x1E #x8E #xF4 #x64))
  (equal?
    (bytevector-xor
      #vu8(#x1F #x36 #x65 #x67)
      #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x01 #xB8 #x91 #x03))
    #vu8(#x2 #xB2 #x25 #xD2 #x7F #x49 #xC1 #xFE #xD3 #x1E #x8E #xF4 #x64))

 ; random tests
  (do ([n 1000 (fx- n 1)])
      ((fxzero? n) #t)
    (let ([size (random 30)])
      (let ([bv1 ($make-random-bytevector size)]
            [bv2 ($make-random-bytevector size)])
        (unless (equal? (bytevector-not bv1)
                        ($bytevector-not bv1))
          (errorf #f "bytevector-not failed on ~s" bv1))
        (unless (equal? (bytevector-and bv1 bv2)
                        ($bytevector-and bv1 bv2))
          (errorf #f "bytevector-and failed on ~s and ~s" bv1 bv2))
        (unless (equal? (bytevector-and bv2 bv1)
                        ($bytevector-and bv2 bv1))
          (errorf #f "bytevector-and failed on ~s and ~s" bv2 bv1))
        (unless (equal? (bytevector-and bv1 bv1)
                        ($bytevector-and bv1 bv1))
          (errorf #f "bytevector-and failed on ~s and ~s" bv1 bv1))
        (unless (equal? (bytevector-ior bv1 bv2)
                        ($bytevector-ior bv1 bv2))
          (errorf #f "bytevector-ior failed on ~s and ~s" bv1 bv2))
        (unless (equal? (bytevector-ior bv2 bv1)
                        ($bytevector-ior bv2 bv1))
          (errorf #f "bytevector-ior failed on ~s and ~s" bv2 bv1))
        (unless (equal? (bytevector-ior bv1 bv1)
                        ($bytevector-ior bv1 bv1))
          (errorf #f "bytevector-ior failed on ~s and ~s" bv1 bv1))
        (unless (equal? (bytevector-xor bv1 bv2)
                        ($bytevector-xor bv1 bv2))
          (errorf #f "bytevector-xor failed on ~s and ~s" bv1 bv2))
        (unless (equal? (bytevector-xor bv2 bv1)
                        ($bytevector-xor bv2 bv1))
          (errorf #f "bytevector-xor failed on ~s and ~s" bv2 bv1))
        (unless (equal? (bytevector-xor bv1 bv1)
                        ($bytevector-xor bv1 bv1))
          (errorf #f "bytevector-xor failed on ~s and ~s" bv1 bv1)))))
)

(mat bytevector->immutable-bytevector
    (begin
      (define immutable-100-bytevector
        (bytevector->immutable-bytevector (make-bytevector 100 42)))
      #t)
    
    (immutable-bytevector? immutable-100-bytevector)
    (not (mutable-bytevector? immutable-100-bytevector))
    
    (equal? (make-bytevector 100 42) immutable-100-bytevector)
    (eq? immutable-100-bytevector
         (bytevector->immutable-bytevector immutable-100-bytevector))
    
    (not (immutable-bytevector? (make-bytevector 5)))
    (mutable-bytevector? (make-bytevector 5))
    
    (immutable-bytevector? (bytevector->immutable-bytevector (bytevector)))
    (not (mutable-bytevector? (bytevector->immutable-bytevector (bytevector))))
    (not (immutable-bytevector? (bytevector)))
    (mutable-bytevector? (bytevector))
    
    (not (immutable-bytevector? (bytevector-copy immutable-100-bytevector)))

    ;; Make sure `...set!` functions check for mutability:
    (error? (bytevector-uint-set! immutable-100-bytevector 0 1 (endianness big) 4))
    (error? (bytevector-sint-set! immutable-100-bytevector 0 1 (endianness big) 4))
    (error? (bytevector-u8-set! immutable-100-bytevector 0 1))
    (error? (bytevector-s8-set! immutable-100-bytevector 0 1))
    (error? (bytevector-u16-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-s16-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-u16-native-set! immutable-100-bytevector 0 1))
    (error? (bytevector-s16-native-set! immutable-100-bytevector 0 1))
    (error? (bytevector-u24-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-s24-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-u32-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-s32-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-u32-native-set! immutable-100-bytevector 0 1))
    (error? (bytevector-s32-native-set! immutable-100-bytevector 0 1))
    (error? (bytevector-u40-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-s40-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-u48-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-s48-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-u56-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-s56-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-u64-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-s64-set! immutable-100-bytevector 0 1 (endianness big)))
    (error? (bytevector-u64-native-set! immutable-100-bytevector 0 1))
    (error? (bytevector-s64-native-set! immutable-100-bytevector 0 1))
    (error? (bytevector-ieee-single-set! immutable-100-bytevector 0 1.0 (endianness big)))
    (error? (bytevector-ieee-double-set! immutable-100-bytevector 0 1.0 (endianness big)))
    (error? (bytevector-ieee-single-native-set! immutable-100-bytevector 0 1.0))
    (error? (bytevector-ieee-double-native-set! immutable-100-bytevector 0 1.0))
    
    (error? (bytevector-fill! immutable-100-bytevector 0))
    (error? (bytevector-copy! '#vu8(4 5 6) 0 immutable-100-bytevector 0 3))
    (error? (bytevector-truncate! immutable-100-bytevector 1))

    ;; Make sure `...ref!` functions *don't* accidentally check for mutability:
    (number? (bytevector-uint-ref immutable-100-bytevector 0 (endianness big) 4))
    (number? (bytevector-sint-ref immutable-100-bytevector 0 (endianness big) 4))
    (number? (bytevector-u8-ref immutable-100-bytevector 0))
    (number? (bytevector-s8-ref immutable-100-bytevector 0))
    (number? (bytevector-u16-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-s16-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-u16-native-ref immutable-100-bytevector 0))
    (number? (bytevector-s16-native-ref immutable-100-bytevector 0))
    (number? (bytevector-u24-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-s24-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-u32-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-s32-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-u32-native-ref immutable-100-bytevector 0))
    (number? (bytevector-s32-native-ref immutable-100-bytevector 0))
    (number? (bytevector-u40-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-s40-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-u48-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-s48-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-u56-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-s56-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-u64-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-s64-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-u64-native-ref immutable-100-bytevector 0))
    (number? (bytevector-s64-native-ref immutable-100-bytevector 0))
    (number? (bytevector-ieee-single-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-ieee-double-ref immutable-100-bytevector 0 (endianness big)))
    (number? (bytevector-ieee-single-native-ref immutable-100-bytevector 0))
    (number? (bytevector-ieee-double-native-ref immutable-100-bytevector 0))
)

(mat bytevector-compress
    (parameters [compress-format 'gzip 'lz4] [compress-level 'minimum 'low 'medium 'high 'maximum])
    (error? (bytevector-compress 7))
    (error? (bytevector-compress "hello"))
    (error? (bytevector-uncompress 7))
    (error? (bytevector-uncompress "hello"))
    (begin
      (define (round-trip-bytevector-compress bv)
        (and
          (equal? (let ([c-bv (#%$bytevector-compress bv 0)])
                    (#%$bytevector-uncompress c-bv 0 (bytevector-length c-bv) (bytevector-length bv) 0))
                  bv)
          (equal? (bytevector-uncompress (bytevector-compress bv)) bv)))
      (round-trip-bytevector-compress (string->utf8 "hello")))
    (round-trip-bytevector-compress '#vu8())
    (round-trip-bytevector-compress (apply bytevector
                                           (let loop ([i 0])
                                             (if (= i 4096)
                                                 '()
                                                 (cons (bitwise-and i 255)
                                                       (loop (+ i 1)))))))
    (round-trip-bytevector-compress 
      (call-with-port (open-file-input-port (find-source "prettytest.ss")) get-bytevector-all))
    (error?
     ;; Need at least 8 bytes for result size
     (bytevector-uncompress '#vu8()))
    (error?
     ;; Need at least 8 bytes for result size
     (bytevector-uncompress '#vu8(0 0 0 0 0 0 255)))
    (error?
     ;; Claming a too-large size in the header should fail with a suitable message:
     (bytevector-uncompress '#vu8(255 255 255 255 255 255 255 255 1 2 3)))
)
