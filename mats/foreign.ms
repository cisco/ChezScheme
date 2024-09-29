;;; foreign.ms
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

(define-syntax machine-case
  (lambda (x)
    (syntax-case x ()
      [(_ [(a ...) e ...] m ...)
       (if (memq (machine-type) (datum (a ...)))
           #'(begin (void) e ...)
           #'(machine-case m ...))]
      [(_ [else e ...]) #'(begin (void) e ...)]
      [(_) #'(void)])))

(meta-cond
 [(and (pb?)
       (not (guard (x [#t #f]) (eval '(foreign-callable void () void)))))
  (void)]
 [else

#;(define-syntax foreign-struct-mat
 (syntax-rules ()
   [(_ name n)
    (mat name
       (set! fs-size
          ((foreign-procedure (format "s~a_size" n) () unsigned-32)))
       (set! fs-align
          ((foreign-procedure (format "s~a_align" n) () unsigned-32)))
       (set! fs-get-s
          (eval `(foreign-procedure ,(format "get_s~a" n) (char)
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-get-sp
          (foreign-procedure (format "get_s~ap" n) (char)
             foreign-pointer))
       (set! fs-s_f1_s
          (eval `(foreign-procedure ,(format "s~a_f1_s~a" n n)
                    ((foreign-object ,fs-size ,fs-align)
                     (foreign-object ,fs-size ,fs-align))
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-sp_f1_s
          (eval `(foreign-procedure ,(format "s~ap_f1_s~a" n n)
                    (foreign-pointer
                     (foreign-object ,fs-size ,fs-align))
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-s_f1_sp
          (eval `(foreign-procedure ,(format "s~a_f1_s~ap" n n)
                    ((foreign-object ,fs-size ,fs-align)
                     foreign-pointer)
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-sp_f1_sp
          (eval `(foreign-procedure ,(format "s~ap_f1_s~ap" n n)
                    (foreign-pointer
                     foreign-pointer)
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-s_f2_s
          (eval `(foreign-procedure ,(format "s~a_f2_s~a" n n)
                    (integer-32
                     (foreign-object ,fs-size ,fs-align)
                     (foreign-object ,fs-size ,fs-align))
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-sp_f2_s
          (eval `(foreign-procedure ,(format "s~ap_f2_s~a" n n)
                    (integer-32
                     foreign-pointer
                     (foreign-object ,fs-size ,fs-align))
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-s_f2_sp
          (eval `(foreign-procedure ,(format "s~a_f2_s~ap" n n)
                    (integer-32
                     (foreign-object ,fs-size ,fs-align)
                     foreign-pointer)
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-sp_f2_sp
          (eval `(foreign-procedure ,(format "s~ap_f2_s~ap" n n)
                    (integer-32
                     foreign-pointer
                     foreign-pointer)
                    (foreign-object ,fs-size ,fs-align))))
       (set! fs-s_f3_s
          (eval `(foreign-procedure ,(format "s~a_f3_s~a" n n)
                    ((foreign-object ,fs-size ,fs-align)
                     (foreign-object ,fs-size ,fs-align))
                    boolean)))
       (set! fs-sp_f3_s
          (eval `(foreign-procedure ,(format "s~ap_f3_s~a" n n)
                    (foreign-pointer
                     (foreign-object ,fs-size ,fs-align))
                    boolean)))
       (set! fs-s_f3_sp
          (eval `(foreign-procedure ,(format "s~a_f3_s~ap" n n)
                    ((foreign-object ,fs-size ,fs-align)
                     foreign-pointer)
                    boolean)))
       (set! fs-sp_f3_sp
          (eval `(foreign-procedure ,(format "s~ap_f3_s~ap" n n)
                    (foreign-pointer
                     foreign-pointer)
                    boolean)))

       (set! fs-a (fs-get-s #\a))
       (string? fs-a)
       (set! fs-ap (fs-get-sp #\a))
       (integer? fs-ap)
       (set! fs-b (fs-get-s #\b))
       (string? fs-b)
       (set! fs-bp (fs-get-sp #\b))
       (integer? fs-bp)


       (fs-s_f3_s fs-a fs-a)
       (fs-s_f3_s fs-a fs-ap)
       (fs-s_f3_s fs-ap fs-a)
       (fs-s_f3_s fs-ap fs-ap)
       (fs-sp_f3_s fs-a fs-a)
       (fs-sp_f3_s fs-a fs-ap)
       (fs-sp_f3_s fs-ap fs-a)
       (fs-sp_f3_s fs-ap fs-ap)
       (fs-s_f3_sp fs-a fs-a)
       (fs-s_f3_sp fs-a fs-ap)
       (fs-s_f3_sp fs-ap fs-a)
       (fs-s_f3_sp fs-ap fs-ap)
       (fs-sp_f3_sp fs-a fs-a)
       (fs-sp_f3_sp fs-a fs-ap)
       (fs-sp_f3_sp fs-ap fs-a)
       (fs-sp_f3_sp fs-ap fs-ap)

       (not (fs-s_f3_s fs-a fs-b))
       (not (fs-s_f3_s fs-a fs-bp))
       (not (fs-s_f3_s fs-ap fs-b))
       (not (fs-s_f3_s fs-ap fs-bp))
       (not (fs-sp_f3_s fs-a fs-b))
       (not (fs-sp_f3_s fs-a fs-bp))
       (not (fs-sp_f3_s fs-ap fs-b))
       (not (fs-sp_f3_s fs-ap fs-bp))
       (not (fs-s_f3_sp fs-a fs-b))
       (not (fs-s_f3_sp fs-a fs-bp))
       (not (fs-s_f3_sp fs-ap fs-b))
       (not (fs-s_f3_sp fs-ap fs-bp))
       (not (fs-sp_f3_sp fs-a fs-b))
       (not (fs-sp_f3_sp fs-a fs-bp))
       (not (fs-sp_f3_sp fs-ap fs-b))
       (not (fs-sp_f3_sp fs-ap fs-bp))

       (fs-sp_f3_sp (fs-s_f1_s fs-ap fs-bp) (fs-sp_f1_s fs-a fs-bp))
       (fs-sp_f3_sp (fs-s_f1_sp fs-ap fs-b) (fs-sp_f1_sp fs-a fs-b))

       (fs-sp_f3_sp (fs-s_f2_s 1 fs-ap fs-bp) (fs-sp_f2_s 1 fs-a fs-bp))
       (fs-sp_f3_sp (fs-s_f2_sp 1 fs-ap fs-b) (fs-sp_f2_sp 1 fs-a fs-b))
      )]))

(define-syntax auto-mat-ick
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       (let ((ls (let f ([ls (string->list (datum name))])
                   (if (null? ls)
                       '()
                       (cons (car ls) (f (cddr ls)))))))
         (with-syntax ([((p v) ...)
                        (map (lambda (c)
                               (case (syntax->datum c)
                                 [(#\n) `(,(syntax integer-32)
                                          ,(random 1000))]
                                 [(#\s) `(,(syntax single-float)
                                          ,(truncate (random 1000.0)))]
                                 [(#\d) `(,(syntax double-float)
                                          ,(truncate (random 1000.0)))]))
                             ls)])
           (syntax (= (let ([x (foreign-procedure name (p ...) double-float)])
                        (x v ...))
                      (+ v ...)))))))))

(define foreign1.so (let ([p (find-source "foreign1.so")])
                      p
                      (if (or (path-absolute? p)
                              (not (equal? "" (path-parent p))))
                          p
                          ;; specify relative to current directory:
                          (path-build "." p))))
(printf "found ~s in ~s\n" foreign1.so (source-directories))

(machine-case
  [(i3ob ti3ob a6ob ta6ob a6s2 ta6s2 i3s2 ti3s2 i3qnx ti3qnx)
   (mat load-shared-object
      (file-exists? foreign1.so)
      (begin (load-shared-object foreign1.so) #t)
      (begin (load-shared-object "libc.so") #t)
      (error? (load-shared-object 3))
    )
   ]
  [(i3le ti3le a6le ta6le arm32le tarm32le arm64le tarm64le ppc32le tppc32le rv64le trv64le la64le tla64le)
   (mat load-shared-object
      (file-exists? foreign1.so)
      (begin (load-shared-object foreign1.so) #t)
      (begin (load-shared-object "libc.so.6") #t)
      (error? (load-shared-object 3))
    )
   ]
  [(i3fb ti3fb a6fb ta6fb)
   (mat load-shared-object
      (file-exists? foreign1.so)
      (begin (load-shared-object foreign1.so) #t)
      (begin (load-shared-object "libc.so.7") #t)
      (error? (load-shared-object 3))
    )
   ]
  [(i3nb ti3nb a6nb ta6nb)
   (mat load-shared-object
      (file-exists? foreign1.so)
      (begin (load-shared-object foreign1.so) #t)
      (begin (load-shared-object "libc.so") #t)
      (error? (load-shared-object 3))
    )
   ]
  [(i3nt ti3nt a6nt ta6nt arm64nt tarm64nt)
   (mat load-shared-object
      (file-exists? foreign1.so)
      (begin (load-shared-object foreign1.so) #t)
      (begin (load-shared-object "msvcrt.dll") #t)
      (begin (load-shared-object "kernel32.dll") #t)
      (error? (load-shared-object 3))
    )
   ]
  [(i3osx ti3osx a6osx ta6osx ppc32osx tppc32osx arm64osx tarm64osx
          a6ios ta6ios arm64ios tarm64ios)
   (mat load-shared-object
     (file-exists? foreign1.so)
      (begin (load-shared-object foreign1.so) #t)
      (begin (load-shared-object "libc.dylib") #t)
      #t
      (error? (load-shared-object 3))
    )
   ]
  [(pb pb32l pb32b pb64l pb64b tpb tpb32l tpb32b tpb64l tpb64b)
   (mat load-shared-object
     (file-exists? foreign1.so)
     (begin (load-shared-object foreign1.so) #t)
     (let ([try-load (lambda (path)
                       (guard (x [#t #f])
                              (load-shared-object path)
                              #t))])
       (or (try-load "libc.so.7")
           (try-load "libc.so.6")
           (try-load "libc.dylib")))
     (error? (load-shared-object 3)))]
  [else
   (mat foreign-procedure
      (error? (foreign-procedure "foo" () scheme-object))
      (begin (define (idint32 x)
            (errorf 'idint32 "invalid foreign-procedure argument ~s" x))
          (procedure? idint32))
      (error? (idint32 #x80000000))
      (error? (idint32 #x80000001))
      (error? (idint32 #xffffffff))
      (error? (idint32 #x8000000080000000))
      (error? (idint32 #x-80000001))
      (error? (idint32 #x-8000000080000000))
      (error? (idint32 #f))
      (error? (idint32 "hi"))
      (begin (define (iduns32 x)
            (errorf 'iduns32 "invalid foreign-procedure argument ~s" x))
             (procedure? iduns32))
         (error? (iduns32 #x100000000))
      (error? (iduns32 #x8000000080000000))
      (error? (iduns32 -1))
      (error? (iduns32 #x-7fffffff))
      (error? (iduns32 #x-80000000))
      (error? (iduns32 #x-80000001))
      (error? (iduns32 #x-8000000080000000))
      (error? (iduns32 #f))
      (error? (iduns32 "hi"))
      (begin (define (idfix x)
            (errorf 'idfix "invalid foreign-procedure argument ~s" x))
             (procedure? idfix))
      (error? (idfix (+ (most-positive-fixnum) 1)))
      (error? (idfix (- (most-negative-fixnum) 1)))
      (error? (errorf 'id "return value ~s is out of range" #x7fffffff))
      (error? (errorf 'id "return value ~s is out of range" #x-80000000))
      (error? (errorf 'id "invalid foreign-procedure argument ~s" 0))
      (error? (errorf 'id "return value ~s is out of range" #x7fffffff))
      (error? (errorf 'id "invalid foreign-procedure argument ~s" 'foo))
      (error? (foreign-procedure 'abcde (integer-32) integer-32))
      (error? (errorf 'float_id "invalid foreign-procedure argument ~s" 0))
    )
   ])

(mat load-shared-object-f
  (equal?
    (with-input-from-string
      (separate-eval
        '(begin
           (printf "(~S " (foreign-entry? "memcpy"))
           (load-shared-object #f)
           (printf "~S)~%" (foreign-entry? "memcpy"))
           (void)))
      read)
    '(#f #t)))

(mat foreign-entry?
   (foreign-entry? "id")
   (foreign-entry? "idid")
   (foreign-entry? "ididid")
   (not (foreign-entry? "foo")))

(mat foreign-procedure
   (procedure? (foreign-procedure "idiptr" (scheme-object) scheme-object))
   (error? (foreign-procedure "i do not exist" (scheme-object) scheme-object))
   (error? (begin (foreign-procedure "i do not exist" () scheme-object) 'q))
   (error? (if (foreign-procedure "i do not exist" () scheme-object) 'q 'q))
   (error? (foreign-procedure 'foo () scheme-object))
   (error? (begin (foreign-procedure 'foo () scheme-object) 'q))
   (error? (if (foreign-procedure 'foo () scheme-object) 'q 'q))

   (eq? 'foo ((foreign-procedure "idiptr" (scheme-object) scheme-object) 'foo))

   (parameterize ([current-eval interpret])
       (eq? 'foo ((foreign-procedure "idiptr" (scheme-object) scheme-object) 'foo)))

   (not (eq? 'foo ((foreign-procedure "idiptr" (scheme-object) void) 'foo)))

   (begin (define idint32 (foreign-procedure "id" (integer-32) integer-32))
          (procedure? idint32))
   (eqv? (idint32 0) 0)
   (eqv? (idint32 #x7fffffff) #x7fffffff)
   (eqv? (idint32 -1) -1)
   (eqv? (idint32 #x-7fffffff) #x-7fffffff)
   (eqv? (idint32 #x-80000000) #x-80000000)
   (eqv? (idint32 #x80000000) (+ #x-100000000 #x80000000))
   (eqv? (idint32 #x80000001) (+ #x-100000000 #x80000001))
   (eqv? (idint32 #xffffffff) (+ #x-100000000 #xffffffff))
   (error? (idint32 #x100000000))
   (error? (idint32 #x100000001))
   (error? (idint32 #xfffffffffffffffffffffffffffff))
   (error? (idint32 #x8000000080000000))
   (error? (idint32 #x-80000001))
   (error? (idint32 #x-8000000080000000))
   (error? (idint32 #f))
   (error? (idint32 "hi"))

   (begin (define iduns32 (foreign-procedure "id" (unsigned-32) unsigned-32))
          (procedure? iduns32))
   (eqv? (iduns32 0) 0)
   (eqv? (iduns32 #x7fffffff) #x7fffffff)
   (eqv? (iduns32 #x80000000) #x80000000)
   (eqv? (iduns32 #x80000001) #x80000001)
   (eqv? (iduns32 #x88000000) #x88000000)
   (eqv? (iduns32 #xffffffff) #xffffffff)
   (error? (iduns32 #x100000000))
   (error? (iduns32 #x8000000080000000))
   (eqv? (iduns32 -1) (+ #x100000000 -1))
   (eqv? (iduns32 #x-7fffffff) (+ #x100000000 #x-7fffffff))
   (eqv? (iduns32 #x-80000000) (+ #x100000000 #x-80000000))
   (error? (iduns32 #x-80000001))
   (error? (iduns32 #x-ffffffff))
   (error? (iduns32 #x-fffffffffffffffffffffffffffffffff))
   (error? (iduns32 #x-80000001))
   (error? (iduns32 #x-8000000080000000))
   (error? (iduns32 #f))
   (error? (iduns32 "hi"))

   (eqv? #xffffffff ((foreign-procedure "id" (integer-32) unsigned-32) -1))
   (eqv? -1 ((foreign-procedure "id" (unsigned-32) integer-32) #xffffffff))

   (begin (define idfix (foreign-procedure "idiptr" (fixnum) fixnum))
          (procedure? idfix))
   (eqv? 0 (idfix 0))
   (eqv? -1 (idfix -1))
   (eqv? (quotient (most-positive-fixnum) 2)
         (idfix (quotient (most-positive-fixnum) 2)))
   (eqv? (quotient (most-negative-fixnum) 2)
         (idfix (quotient (most-negative-fixnum) 2)))
   (eqv? (most-positive-fixnum) (idfix (most-positive-fixnum)))
   (eqv? (most-negative-fixnum) (idfix (most-negative-fixnum)))
   (error? (idfix (+ (most-positive-fixnum) 1)))
   (error? (idfix (- (most-negative-fixnum) 1)))

; we've eliminated the return range checks---caveat emptor
;   (error? ((foreign-procedure "id" (integer-32) fixnum) #x7fffffff))
;   (error? ((foreign-procedure "id" (integer-32) fixnum) #x-80000000))
;   (error? ((foreign-procedure "id" (integer-32) char) #x7fffffff))

   (error? (foreign-procedure "id" (booleen) char))
   (error? (foreign-procedure "id" (integer-32 integer-34) char))
   (error? (foreign-procedure "id" () chare))
   (error? (foreign-procedure "id" (void) char))

   ((foreign-procedure "id" (boolean) boolean) #t)
   (not ((foreign-procedure "id" (boolean) boolean) #f))
   ((foreign-procedure "id" (boolean) boolean) 0)
   (= 1 ((foreign-procedure "id" (boolean) integer-32) #t))
   (= 1 ((foreign-procedure "id" (boolean) integer-32) 0))
   (= 0 ((foreign-procedure "id" (boolean) integer-32) #f))
   (not ((foreign-procedure "id" (integer-32) boolean) 0))
   ((foreign-procedure "id" (integer-32) boolean) 1)

   (char=? #\a ((foreign-procedure "id" (char) char) #\a))
   (= 0 ((foreign-procedure "id" (char) integer-32) #\nul))
   (char=? #\nul ((foreign-procedure "id" (integer-32) char) 0))
   (eqv? ((foreign-procedure "id" (integer-32) char) -1) #\377)
   (error? ((foreign-procedure "id" (char) void) 0))

   (let ([s "now is the time for all good men"])
      (string=? s ((foreign-procedure "idiptr" (string) string) s)))
   (let ([s "now is the time for all good men"])
      (not (eq? s ((foreign-procedure "idiptr" (string) string) s))))
  ; assuming iptr is same size as char *:
   (let ([id1 (foreign-procedure "idiptr" (string) string)]
         [id2 (foreign-procedure "idiptr" (string) iptr)]
         [id3 (foreign-procedure "idiptr" (iptr) string)])
      (and (eq? (id1 #f) #f) (eq? (id2 #f) 0) (eq? (id3 0) #f)))
   (let ()
     (define $string->bytevector
       (lambda (s)
         (let ([n (string-length s)])
           (let ([bv (make-bytevector (+ n 1))])
             (do ([i 0 (fx+ i 1)])
                 ((fx= i n))
               (bytevector-u8-set! bv i (char->integer (string-ref s i))))
             (bytevector-u8-set! bv n 0)
             bv))))
     (let ([s "now is the time for all good men"]
           [r "                                "])
       (let ([bv ($string->bytevector r)])
         ((foreign-procedure (if (windows?) "windows_strcpy" "strcpy") (u8* string) void) bv s)
         (= 0 ((foreign-procedure (if (windows?) "windows_strcmp" "strcmp") (u8* string) integer-32) bv s)))))
   (error? ((foreign-procedure "id" (string) void) 'foo))

   (= ((foreign-procedure "idid" (integer-32) integer-32) #xc7c7c7) #xc7c7c7)
   (= ((foreign-procedure "ididid" (integer-32) integer-32) #x7c7c7c7c)
      #x7c7c7c7c)

   (= ((foreign-procedure "id" (unsigned-32) unsigned-32) #x80000000)
      #x80000000)
   (= ((foreign-procedure "id" (unsigned-32) integer-32) #x80000000)
      #x-80000000)

   (error? (foreign-procedure 'abcde (integer-32) integer-32))
   (let ([template
          (lambda (x)
             (foreign-procedure x (char) boolean))])
      (let ([id (template "id")]
            [idid (template "idid")]
            [ididid (template "ididid")])
         (and (eqv? (id #\nul) #f)
              (eqv? (idid #\001) #t)
              (eqv? (idid #\a) #t))))

   (= 0.0 ((foreign-procedure "float_id" (double-float) double-float) 0.0))
   (= 1.1 ((foreign-procedure "float_id" (double-float) double-float) 1.1))
   (error? ((foreign-procedure "float_id" (double-float) void) 0))

   (let ([fid (foreign-procedure "float_id" (double-float) double-float)])
     (let f ((n 10000))
       (or (= n 0)
           (let ([x (random 1.0)])
             (and (eqv? x (fid x))
                  (f (- n 1)))))))

   (= (+ (*  1 29) (*  2 31) (*  3 37) (*  5 41) (*  7 43)
         (* 11 47) (* 13 49) (* 17 53) (* 19 59) (* 23 61))
      ((foreign-procedure "testten"
          (integer-32 integer-32 integer-32 integer-32 integer-32
           integer-32 integer-32 integer-32 integer-32 integer-32)
          integer-32)
       29 31 37 41 43 47 49 53 59 61))

   (= (+ 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8)
      ((foreign-procedure "flsum8"
          (double-float double-float double-float double-float
           double-float double-float double-float double-float)
          double-float)
       1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8))

   (= (+ 1 2 3 4 5 6.75 7 8.5)
      ((foreign-procedure "sparcfltest"
          (integer-32 integer-32 integer-32 integer-32
           integer-32 double-float integer-32 double-float)
          double-float)
       1 2 3 4 5 6.75 7 8.5))

   (= (+ 1 2 3.3)
      ((foreign-procedure "mipsfltest1"
          (integer-32 integer-32 double-float)
          double-float)
       1 2 3.3))

   (= (+ 1 2.2 3.3)
      ((foreign-procedure "mipsfltest2"
          (integer-32 double-float double-float)
          double-float)
       1 2.2 3.3))

   (= (+ 1 2.25 3 4.5 5 6.75 7 8.25 9.5 10.75 11.25 12.5 13.75 14.25 15.5
       16.75 17.25 18.75 19.25)
      ((foreign-procedure "ppcfltest"
          (integer-32 double-float integer-32 double-float integer-32
           double-float integer-32 double-float double-float double-float
           double-float double-float double-float double-float double-float
           double-float double-float double-float double-float)
          double-float)
       1 2.25 3 4.5 5 6.75 7 8.25 9.5 10.75 11.25 12.5 13.75 14.25 15.5
       16.75 17.25 18.75 19.25))

   (= (+ 1 2.25 3 4.5 5
         (expt 2 36) 6.75 7 8.25
         (expt 2 39) 75
         9.5 10.75 11.25 12.5
         13.75 14.25 15.5
         20 16.75 21 (expt 2 37) 18.75 22
         19.25)
      ((foreign-procedure "ppcfltest2"
         (integer-32 double-float integer-32 double-float integer-32
          integer-64 double-float integer-32 double-float
          ; next integer should be stack-allocated with the PPC ABI
          integer-64 integer-32
          ; but next four floats should still get registers
          double-float double-float double-float double-float
          ; and remaining floags and ints should go on the stack
          double-float single-float double-float
          integer-32 double-float integer-32 integer-64 double-float integer-32
          double-float)
         double-float)
        1 2.25 3 4.5 5
        (expt 2 36) 6.75 7 8.25
        (expt 2 39) 75
        9.5 10.75 11.25 12.5
        13.75 14.25 15.5
        20 16.75 21 (expt 2 37) 18.75 22
        19.25))

   ((foreign-procedure "chk_data" () boolean))
   ((foreign-procedure "chk_bss" () boolean))
   ((foreign-procedure "chk_malloc" () boolean))

   (begin
     (define $fp-tlv (foreign-procedure "(cs)s_tlv" (ptr) ptr))
     (define $fp-stlv! (foreign-procedure "(cs)s_stlv" (ptr ptr) void))
     #t)

   (equal?
     (let ()
       (define-syntax list-in-order
         (syntax-rules ()
           [(_) '()]
           [(_ e . rest) (let ([t e]) (cons t (list-in-order . rest)))]))
       (list-in-order
         ($fp-tlv 'cons)
         ($fp-stlv! '$fp-spam 'yum)
         ($fp-tlv '$fp-spam)
         (top-level-value '$fp-spam)))
     `(,cons ,(void) yum yum))

   (equal?
     (let ()
       (define-syntax list-in-order
         (syntax-rules ()
           [(_) '()]
           [(_ e . rest) (let ([t e]) (cons t (list-in-order . rest)))]))
       (parameterize ([interaction-environment (copy-environment (scheme-environment))])
         (list-in-order
           (define-top-level-value 'foo 17)
           ($fp-tlv 'foo)
           ($fp-stlv! 'bar 55)
           ($fp-tlv 'bar)
           (top-level-value 'bar))))
     `(,(void) 17 ,(void) 55 55))

  (equal?
    (parameterize ([interaction-environment (copy-environment (scheme-environment))])
     ; should have no effect
      ($fp-stlv! cons 3)
      (list
        (#%$tc-field 'disable-count (#%$tc))
        cons
        ($fp-tlv 'cons)))
    `(0 ,cons ,cons))

  (equal?
    (parameterize ([interaction-environment (copy-environment (scheme-environment))])
     ; should have no effect
      ($fp-stlv! 'let 3)
      (list
        (#%$tc-field 'disable-count (#%$tc))
        (eval '(let ((x 23)) x))))
    '(0 23))

  (equal?
    (let ([x ($fp-tlv '$fp-i-am-not-bound)])
      (list (#%$tc-field 'disable-count (#%$tc)) x))
    `(0 ,(#%$unbound-object)))

  (equal?
    (let ([x ($fp-tlv 'let)])
      (list (#%$tc-field 'disable-count (#%$tc)) x))
    `(0 ,(#%$unbound-object)))

  (equal? ((foreign-procedure "(cs)s_test_schlib" () void)) (void))

  (begin
    (define $siv (foreign-procedure "(cs)Sinteger_value" (ptr) void))
    (define $si32v (foreign-procedure "(cs)Sinteger32_value" (ptr) void))
    (define $si64v (foreign-procedure "(cs)Sinteger64_value" (ptr) void))
    (define ($check p n)
      (or (= (optimize-level) 3)
          (guard (c [(and (assertion-violation? c)
                          (irritants-condition? c)
                          (equal? (condition-irritants c) (list n)))
                     #t])
            (p n)
            #f)))
    #t)

 ; make sure no errors for in-range inputs
  (begin
    ($si32v (- (expt 2 32) 1))
    ($si32v (- (expt 2 31)))
    ($si64v (- (expt 2 64) 1))
    ($si64v (- (expt 2 63)))
    (if (< (fixnum-width) 32)
        (begin ; assume 32-bit words
          ($siv (- (expt 2 32) 1))
          ($siv (- (expt 2 31))))
        (begin ; assume 64-bit words
          ($siv (- (expt 2 64) 1))
          ($siv (- (expt 2 63)))))
    #t)

 ; check barely out-of-range inputs
  ($check $si32v (expt 2 32))
  ($check $si32v (- -1 (expt 2 31)))
  ($check $si64v (expt 2 64))
  ($check $si64v (- -1 (expt 2 63)))
  ($check $siv (expt 2 (if (< (fixnum-width) 32) 32 64)))
  ($check $siv (- -1 (expt 2 (if (< (fixnum-width) 32) 31 63))))

 ; check further out-of-range inputs
  ($check $si32v (expt 2 36))
  ($check $si32v (- -1 (expt 2 35)))
  ($check $si64v (expt 2 68))
  ($check $si64v (- -1 (expt 2 67)))
  ($check $siv (expt 2 (if (< (fixnum-width) 32) 36 68)))
  ($check $siv (- -1 (expt 2 (if (< (fixnum-width) 32) 35 67))))
  ($check $si32v (expt 2 100))
  ($check $si32v (- -1 (expt 2 100)))
  ($check $si64v (expt 2 100))
  ($check $si64v (- -1 (expt 2 100)))
  ($check $siv (expt 2 100))
  ($check $siv (- -1 (expt 2 100)))
)

(mat round-trip
  (begin
    (define rt-int (foreign-procedure "rt_int" (iptr) ptr))
    (define rt-int32 (foreign-procedure "rt_int32" (integer-32) ptr))
    (define rt-int64 (foreign-procedure "rt_int64" (integer-64) ptr))
    (define rt-uint (foreign-procedure "rt_uint" (uptr) ptr))
    (define rt-uint32 (foreign-procedure "rt_uint32" (unsigned-32) ptr))
    (define rt-uint64 (foreign-procedure "rt_uint64" (unsigned-64) ptr))
    (define (check expected ls)
      (and (list? ls)
           (= (length ls) 4)
           (let ([expected (or expected (car ls))])
             (andmap (lambda (x) (eqv? expected x)) ls))))
    (define rt-fix (if (< (fixnum-width) 32) rt-int32 rt-int64))
    (define (check-fixnum expected)
      (let ([actual (rt-fix expected)])
        (and (fixnum? expected)
             (andmap fixnum? actual)
             (check expected actual))))
    #t)
  (check-fixnum -1)
  (check-fixnum 0)
  (check-fixnum 1)
  (check-fixnum (most-positive-fixnum))
  (check-fixnum (most-negative-fixnum))

  (check 0 (rt-int 0))
  (check -1 (rt-int -1))
  (check #f (rt-int (ash 1 (- (* 8 (foreign-sizeof 'iptr)) 1))))
  (check #f (rt-int (- (ash 1 (- (* 8 (foreign-sizeof 'iptr)) 1)))))
  (let ([n (+ -1 (ash 1 (- (* 8 (foreign-sizeof 'iptr)) 1)))])
    (check n (rt-int n)))
  (let ([n (- (ash 1 (* 8 (foreign-sizeof 'iptr))) 1)])
    (check -1 (rt-int n)))
  (check 0 (rt-int32 0))
  (check -1 (rt-int32 -1))
  (check #f (rt-int32 (ash 1 31)))
  (check #f (rt-int32 (- (ash 1 31))))
  (let ([n (+ -1 (ash 1 31))])
    (check n (rt-int32 n)))
  (check 0 (rt-int64 0))
  (check -1 (rt-int64 -1))
  (check #f (rt-int64 (ash 1 63)))
  (check #f (rt-int64 (- (ash 1 63))))
  (let ([n (+ -1 (ash 1 63))])
    (check n (rt-int64 n)))

  (check 0 (rt-uint 0))
  (check #f (rt-uint -1))
  (check #f (rt-uint (ash 1 (- (* 8 (foreign-sizeof 'iptr)) 1))))
  (check #f (rt-uint (- (ash 1 (- (* 8 (foreign-sizeof 'iptr)) 1)))))
  (let ([n (+ -1 (ash 1 (- (* 8 (foreign-sizeof 'iptr)) 1)))])
    (check n (rt-uint n)))
  (let ([n (- (ash 1 (* 8 (foreign-sizeof 'iptr))) 1)])
    (check n (rt-uint n)))
  (check 0 (rt-uint32 0))
  (check #f (rt-uint32 -1))
  (check #f (rt-uint32 (ash 1 31)))
  (check #f (rt-uint32 (- (ash 1 31))))
  (let ([n (+ -1 (ash 1 31))])
    (check n (rt-uint32 n)))
  (let ([n (- (ash 1 32) 1)])
    (check n (rt-uint32 n)))
  (check 0 (rt-uint64 0))
  (check #f (rt-uint64 -1))
  (check #f (rt-uint64 (ash 1 63)))
  (check #f (rt-uint64 (- (ash 1 63))))
  (let ([n (+ -1 (ash 1 63))])
    (check n (rt-uint64 n)))
  (let ([n (- (ash 1 64) 1)])
    (check n (rt-uint64 n)))
)

(mat convert
  (begin
    (define to-int (foreign-procedure "to_int" (ptr) iptr))
    (define to-int32 (foreign-procedure "to_int32" (ptr) integer-32))
    (define to-int64 (foreign-procedure "to_int64" (ptr) integer-64))
    (define to-uint (foreign-procedure "to_uint" (ptr) uptr))
    (define to-uint32 (foreign-procedure "to_uint32" (ptr) unsigned-32))
    (define to-uint64 (foreign-procedure "to_uint64" (ptr) unsigned-64))
    (define (check-error reason who f arg)
      (define fp-names
        (case who
          [(to-int) '("Sinteger_value")]
          [(to-int32) '("Sinteger32_value")]
          [(to-int64) '("Sinteger64_value")]
          ;; These currently call their integer counterparts and cast the
          ;; result, so the condition-who will report the integer entry point.
          [(to-uint) '("Sunsigned_value" "Sinteger_value")]
          [(to-uint32) '("Sunsigned32_value" "Sinteger32_value")]
          [(to-uint64) '("Sunsigned64_value" "Sinteger64_value")]
          [else '("unexpected")]))
      (guard (c [else
                 ;; Check error message explicitly rather than relying on the
                 ;; diff-based expected-error machinery since the offending
                 ;; values may differ on 32- vs. 64-bit platforms.
                 (assert (member arg (condition-irritants c)))
                 (assert (equal? (condition-message c) reason))
                 (assert (member (condition-who c) fp-names))
                 #t])
        (f arg)
        #f))
    (define out-of-range "~s is out of range")
    (define not-integer "~s is not an integer")
    (define-syntax must-fail
      (syntax-rules ()
        [(_ reason f n) (check-error reason 'f f n)]))
    #t)

  (equal? 0 (to-int 0))
  (equal? 0 (to-int32 0))
  (equal? 0 (to-int64 0))
  (equal? 0 (to-uint 0))
  (equal? 0 (to-uint32 0))
  (equal? 0 (to-uint64 0))
  (equal? -1 (to-int -1))
  (equal? -1 (to-int32 -1))
  (equal? -1 (to-int64 -1))
  (equal? (- (ash 1 (* 8 (foreign-sizeof 'uptr))) 1) (to-uint -1))
  (equal? (- (ash 1 32) 1) (to-uint32 -1))
  (equal? (- (ash 1 64) 1) (to-uint64 -1))

  (must-fail not-integer to-int "one")
  (must-fail not-integer to-int32 'two)
  (must-fail not-integer to-int64 '(3))
  (must-fail not-integer to-uint "four")
  (must-fail not-integer to-uint32 'five)
  (must-fail not-integer to-uint64 '(6))

  (must-fail out-of-range to-int (ash 1 65))
  (must-fail out-of-range to-int (- (- (ash 1 (- (* 8 (foreign-sizeof 'iptr)) 1))) 1))
  (must-fail out-of-range to-int32 (ash 1 33))
  (must-fail out-of-range to-int32 (- (- (ash 1 31)) 1))
  (must-fail out-of-range to-int64 (ash 1 65))
  (must-fail out-of-range to-int64 (- (- (ash 1 63)) 1))
  (must-fail out-of-range to-uint (ash 1 65))
  (must-fail out-of-range to-uint (- (- (ash 1 (- (* 8 (foreign-sizeof 'uptr)) 1))) 1))
  (must-fail out-of-range to-uint32 (ash 1 33))
  (must-fail out-of-range to-uint32 (- (- (ash 1 31)) 1))
  (must-fail out-of-range to-uint64 (ash 1 65))
  (must-fail out-of-range to-uint64 (- (- (ash 1 63)) 1))
)

(mat try-convert
  (begin
    (define m-p-f (most-positive-fixnum))
    (define m-n-f (most-negative-fixnum))
    (define ptr-bits (* 8 (foreign-sizeof 'ptr)))
    (define (test signed? bits prefix in-range* out-of-range*)
      (define sign-bit (and signed? (- bits 1)))
      (define fp-v2 (foreign-procedure (format "~a2" prefix) (ptr) ptr))
      (define fp-v3 (foreign-procedure (format "~a3" prefix) (ptr) ptr))
      (define (fp-name fp)
        (format "~a~a" prefix (assert (cond [(eq? fp fp-v2) 2] [(eq? fp fp-v3) 3] [else #f]))))
      (define (expect expected fp arg)
        (let ([actual (fp arg)])
          (unless (equal? expected actual)
            (errorf #f "expected ~s but got ~s from ~a on ~s\n" expected actual (fp-name fp) arg))))
      (define in-range-for-all*
        (let ([ls (list 1 7654321 #xffffff #xffffff0)])
          (cons 0
            (if (not sign-bit)
                ls
                (append ls (map - ls)
                  (let ([hi (ash 1 sign-bit)])
                    (list hi (- hi) (+ (- hi) 1) (+ hi 1))))))))
      (define out-of-range-for-all*
        (let ([ls (list #e1e30 #e-1e30 (ash 1 64) (- (ash 1 64)))])
          (if (not sign-bit)
              ls
              (let ([hi (ash 1 (+ sign-bit 1))])
                (list* hi (- hi) (- (- hi) 123) (+ hi 27) (- (- (ash hi -1)) 1) ls)))))
      (assert (string=? (substring prefix 4 7) (if sign-bit "int" "uns")))
      ;; try some non-integer cases
      (for-each
       (lambda (bogus)
         (expect '(0 0) fp-v2 bogus)
         (expect '(0 0 "~s is not an integer") fp-v3 bogus))
       '(#f 0.0 #t 3.2 "wrong" bad))
      ;; try in-range cases
      (for-each
       (lambda (good)
         (define expected
           (if (not sign-bit)
               (if (< good 0)
                   (bitwise-bit-field (bitwise-not (- -1 good)) 0 bits)
                   good)
               (if (or (< good 0) (not (logbit? sign-bit good)))
                   good
                   (- (bitwise-bit-field (+ (bitwise-not good) 1) 0 (+ sign-bit 1))))))
         (expect `(1 ,expected) fp-v2 good)
         (expect `(1 ,expected "untouched") fp-v3 good))
       (append in-range-for-all* in-range*))
      ;; try out-of-range cases
      (for-each
       (lambda (bad)
         (expect '(0 0) fp-v2 bad)
         (expect `(0 0 "~s is out of range") fp-v3 bad))
       (append out-of-range-for-all* out-of-range*))
      #t)
    #t)
  (test #t ptr-bits "try_integer_value"
    (list ;; extra in-range checks
     m-n-f
     m-p-f
     ;; hit third return in bignum case
     (- m-n-f 1)
     (+ m-p-f 1))
    '())
  (test #t 32 "try_integer32_value" '() '())
  (test #t 64 "try_integer64_value"
    (list m-p-f m-n-f
      (ash 1 60)
      (ash 1 61)
      (ash 1 62)
      (- -1 (ash 1 60))
      (- -1 (ash 1 61))
      (- -1 (ash 1 62)))
    '())
  (test #f ptr-bits "try_unsigned_value"
    (list m-p-f (+ m-p-f 1) m-n-f
      (- (ash 1 (- ptr-bits 1)))
      (- (- (ash 1 (- ptr-bits 1)) 1)))
    (list (- (+ (ash 1 (- ptr-bits 1)) 1))))
  (test #f 32 "try_unsigned32_value"
    (list
     (+ (ash 1 30) 1)
     (- (+ (ash 1 30) 1))
     (- (ash 1 31)))
    (list (ash 1 32) (- (ash 1 32)) (- -1 (ash 1 31))))
  (test #f 64 "try_unsigned64_value"
    (list m-p-f (+ m-p-f 1) m-n-f (- 1 (ash 1 63)) #x-8000000000000000)
    (list (- -1 (ash 1 63))))
)

(mat foreign-sizeof
  (equal?
    (list
      (foreign-sizeof 'integer-8)
      (foreign-sizeof 'unsigned-8)
      (foreign-sizeof 'integer-16)
      (foreign-sizeof 'unsigned-16)
      (foreign-sizeof 'integer-24)
      (foreign-sizeof 'unsigned-24)
      (foreign-sizeof 'integer-32)
      (foreign-sizeof 'unsigned-32)
      (foreign-sizeof 'integer-40)
      (foreign-sizeof 'unsigned-40)
      (foreign-sizeof 'integer-48)
      (foreign-sizeof 'unsigned-48)
      (foreign-sizeof 'integer-56)
      (foreign-sizeof 'unsigned-56)
      (foreign-sizeof 'integer-64)
      (foreign-sizeof 'unsigned-64)
      (foreign-sizeof 'single-float)
      (foreign-sizeof 'double-float))
    '(1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 4 8))
  ((foreign-procedure "check_types" (int int int int int int int int int) boolean)
   (foreign-sizeof 'char)
   (foreign-sizeof 'wchar)
   (foreign-sizeof 'short)
   (foreign-sizeof 'int)
   (foreign-sizeof 'long)
   (foreign-sizeof 'long-long)
   (foreign-sizeof 'float)
   (foreign-sizeof 'double)
   (foreign-sizeof 'void*))
  (equal? (foreign-sizeof 'unsigned) (foreign-sizeof 'int))
  (equal? (foreign-sizeof 'unsigned-int) (foreign-sizeof 'int))
  (equal? (foreign-sizeof 'unsigned-short) (foreign-sizeof 'short))
  (equal? (foreign-sizeof 'unsigned-long) (foreign-sizeof 'long))
  (equal? (foreign-sizeof 'unsigned-long-long) (foreign-sizeof 'long-long))
  (equal? (foreign-sizeof 'boolean) (foreign-sizeof 'int))
  (equal? (foreign-sizeof 'stdbool) (foreign-sizeof 'stdbool))
  (equal? (foreign-sizeof 'fixnum) (foreign-sizeof 'iptr))
  (equal? (foreign-sizeof 'scheme-object) (foreign-sizeof 'void*))
  (equal? (foreign-sizeof 'ptr) (foreign-sizeof 'void*))
  (equal? (foreign-sizeof 'iptr) (foreign-sizeof 'void*))
  (equal? (foreign-sizeof 'uptr) (foreign-sizeof 'void*))
  (error? (foreign-sizeof))
  (error? (foreign-sizeof 'int 'int))
  (error? (foreign-sizeof 'i-am-not-a-type))
  (error? (foreign-sizeof '1))
)

(mat foreign-bytevectors
 ; test u8*, u16*, u32*
  (begin
    (define u8*->u8* (foreign-procedure "u8_star_to_u8_star" (u8*) u8*))
    (define u16*->u16* (foreign-procedure "u16_star_to_u16_star" (u16*) u16*))
    (define u32*->u32* (foreign-procedure "u32_star_to_u32_star" (u32*) u32*))
    #t)
  (equal? (u8*->u8* #vu8(1 2 3 4 0)) #vu8(2 3 4))
  (equal? (u16*->u16* #vu8(1 2 3 4 5 6 7 8 0 0)) #vu8(3 4 5 6 7 8))
  (equal? (u32*->u32* #vu8(1 2 3 4 5 6 7 8 9 10 11 12 0 0 0 0)) #vu8(5 6 7 8 9 10 11 12))

  (eq? (u8*->u8* #vu8(1 0)) #vu8())
  (eq? (u16*->u16* #vu8(1 2 0 0)) #vu8())
  (eq? (u32*->u32* #vu8(1 2 3 4 0 0 0 0)) #vu8())

  (eq? (u8*->u8* #f) #f)
  (eq? (u16*->u16* #f) #f)
  (eq? (u32*->u32* #f) #f)

  (error? (u8*->u8* "hello"))
  (error? (u16*->u16* "hello"))
  (error? (u32*->u32* "hello"))
  (error? (u8*->u8* 0))
  (error? (u16*->u16* 0))
  (error? (u32*->u32* 0))

  (begin
    (define call-u8* (foreign-procedure "call_u8_star" (ptr u8*) u8*))
    (define call-u16* (foreign-procedure "call_u16_star" (ptr u16*) u16*))
    (define call-u32* (foreign-procedure "call_u32_star" (ptr u32*) u32*))
    (define $bytevector-map
      (lambda (p bv)
        (u8-list->bytevector (map p (bytevector->u8-list bv)))))
    #t)
  (equal?
    (call-u8* (foreign-callable
                (lambda (x) ($bytevector-map (lambda (x) (if (= x 255) 0 (+ x 100))) x))
                (u8*) u8*)
      #vu8(1 2 3 4 5 255 0 ))
    '#vu8(103 104 105))
  (equal?
    (call-u16* (foreign-callable
                 (lambda (x) ($bytevector-map (lambda (x) (if (= x 255) 0 (+ x 100))) x))
                 (u16*) u16*)
      #vu8(1 2 3 4 5 6 255 255 0 0))
    '#vu8(105 106))
  (equal?
    (call-u32* (foreign-callable
                 (lambda (x) ($bytevector-map (lambda (x) (if (= x 255) 0 (+ x 100))) x))
                 (u32*) u32*)
      #vu8(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 255 255 255 255 0 0 0 0))
    '#vu8(109 110 111 112 113 114 115 116 117 118 119 120))
  (error?
    (let ([frotz (foreign-callable
                   (lambda (x) (list x (bytevector-length x)))
                   (u8*) u8*)])
      (call-u8* frotz #vu8(1 2 3 4 5 0))))
  (error?
    (call-u16* (foreign-callable
                 (lambda (x) (list x (bytevector-length x)))
                 (u16*) u16*)
      #vu8(1 2 3 4 5 6 0 0)))
  (error?
    (call-u32* (foreign-callable
                 (lambda (x) (list x (bytevector-length x)))
                 (u32*) u32*)
      #vu8(1 2 3 4 5 6 7 8 0 0 0 0)))
  (error?
    (call-u8* (foreign-callable
                (lambda (x) (list x (bytevector-length x)))
                (u8*) u8*)
      '#(1 2 3 4 5 0)))
  (error?
    (call-u16* (foreign-callable
                 (lambda (x) (list x (bytevector-length x)))
                 (u16*) u16*)
      '#(1 2 3 4 5 6 0 0)))
  (error?
    (call-u32* (foreign-callable
                 (lambda (x) (list x (bytevector-length x)))
                 (u32*) u32*)
      '#(1 2 3 4 5 6 7 8 0 0 0 0)))
)

(mat foreign-strings
 ; test utf-8, utf-16le, utf-16be, utf-32le, utf-32be, string, wstring
  (begin
    (define utf-8->utf-8 (foreign-procedure "u8_star_to_u8_star" (utf-8) utf-8))
    (define utf-16le->utf-16le (foreign-procedure "u16_star_to_u16_star" (utf-16le) utf-16le))
    (define utf-16be->utf-16be (foreign-procedure "u16_star_to_u16_star" (utf-16be) utf-16be))
    (define utf-32le->utf-32le (foreign-procedure "u32_star_to_u32_star" (utf-32le) utf-32le))
    (define utf-32be->utf-32be (foreign-procedure "u32_star_to_u32_star" (utf-32be) utf-32be))
    (define string->string (foreign-procedure "char_star_to_char_star" (string) string))
    (define wstring->wstring (foreign-procedure "wchar_star_to_wchar_star" (wstring) wstring))
    #t)
  (equal? (utf-8->utf-8 "hello") "ello")
  (equal? (utf-16le->utf-16le "hello") "ello")
  (equal? (utf-16be->utf-16be "hello") "ello")
  (equal? (utf-32le->utf-32le "hello") "ello")
  (equal? (utf-32be->utf-32be "hello") "ello")
  (equal? (string->string "hello") "ello")
  (equal? (wstring->wstring "hello") "ello")

  (eq? (utf-8->utf-8 "h") "")
  (eq? (utf-16le->utf-16le "h") "")
  (eq? (utf-16be->utf-16be "h") "")
  (eq? (utf-32le->utf-32le "h") "")
  (eq? (utf-32be->utf-32be "h") "")
  (eq? (string->string "h") "")
  (eq? (wstring->wstring "h") "")

  (eq? (utf-8->utf-8 #f) #f)
  (eq? (utf-16le->utf-16le #f) #f)
  (eq? (utf-16be->utf-16be #f) #f)
  (eq? (utf-32le->utf-32le #f) #f)
  (eq? (utf-32be->utf-32be #f) #f)
  (eq? (string->string #f) #f)
  (eq? (wstring->wstring #f) #f)

  (error? (utf-8->utf-8 #vu8(1 2 3 4 0 0 0 0)))
  (error? (utf-16le->utf-16le #vu8(1 2 3 4 0 0 0 0)))
  (error? (utf-16be->utf-16be #vu8(1 2 3 4 0 0 0 0)))
  (error? (utf-32le->utf-32le #vu8(1 2 3 4 0 0 0 0)))
  (error? (utf-32be->utf-32be #vu8(1 2 3 4 0 0 0 0)))
  (error? (string->string #vu8(1 2 3 4 0 0 0 0)))
  (error? (wstring->wstring #vu8(1 2 3 4 0 0 0 0)))

  (error? (utf-8->utf-8 0))
  (error? (utf-16le->utf-16le 0))
  (error? (utf-16be->utf-16be 0))
  (error? (utf-32le->utf-32le 0))
  (error? (utf-32be->utf-32be 0))
  (error? (string->string 0))
  (error? (wstring->wstring 0))

  (begin
    (define call-utf-8 (foreign-procedure "call_u8_star" (ptr utf-8) utf-8))
    (define call-utf-16le (foreign-procedure "call_u16_star" (ptr utf-16le) utf-16le))
    (define call-utf-16be (foreign-procedure "call_u16_star" (ptr utf-16be) utf-16be))
    (define call-utf-32le (foreign-procedure "call_u32_star" (ptr utf-32le) utf-32le))
    (define call-utf-32be (foreign-procedure "call_u32_star" (ptr utf-32be) utf-32be))
    (define call-string (foreign-procedure "call_string" (ptr string) string))
    (define call-wstring (foreign-procedure "call_wstring" (ptr wstring) wstring))
    #t)
  (equal?
    (call-utf-8 (foreign-callable
                  (lambda (x) (string-append x "$q"))
                  (utf-8) utf-8)
      "hello")
    "llo$q")
  (equal?
    (call-utf-16le (foreign-callable
                     (lambda (x) (string-append x "$q"))
                     (utf-16le) utf-16le)
      "hello")
    "llo$q")
  (equal?
    (call-utf-16be (foreign-callable
                     (lambda (x) (string-append x "$q"))
                     (utf-16be) utf-16be)
      "hello")
    "llo$q")
  (equal?
    (call-utf-32le (foreign-callable
                     (lambda (x) (string-append x "$q"))
                     (utf-32le) utf-32le)
      "hello")
    "llo$q")
  (equal?
    (call-utf-32be (foreign-callable
                     (lambda (x) (string-append x "$q"))
                     (utf-32be) utf-32be)
      "hello")
    "llo$q")
  (equal?
    (call-string (foreign-callable
                   (lambda (x) (string-append x "$q"))
                   (string) string)
      "hello")
    "llo$q")
  (equal?
    (call-wstring (foreign-callable
                    (lambda (x) (string-append x "$q"))
                    (wstring) wstring)
      "hello")
    "llo$q")
  (error?
    (call-utf-8 (foreign-callable
                  (lambda (x) (list x (string-length x)))
                  (utf-8) utf-8)
      "hello"))
  (error?
    (call-utf-16le (foreign-callable
                     (lambda (x) (list x (string-length x)))
                     (utf-16le) utf-16le)
      "hello"))
  (error?
    (call-utf-16be (foreign-callable
                     (lambda (x) (list x (string-length x)))
                     (utf-16be) utf-16be)
      "hello"))
  (error?
    (call-utf-32le (foreign-callable
                     (lambda (x) (list x (string-length x)))
                     (utf-32le) utf-32le)
      "hello"))
  (error?
    (call-utf-32be (foreign-callable
                     (lambda (x) (list x (string-length x)))
                     (utf-32be) utf-32be)
      "hello"))
  (error?
    (call-string (foreign-callable
                   (lambda (x) (list x (string-length x)))
                   (string) string)
      "hello"))
  (error?
    (call-wstring (foreign-callable
                    (lambda (x) (list x (string-length x)))
                    (wstring) wstring)
      "hello"))
)

(mat foreign-fixed-types
 ; test {integer,unsigned}-8, {single,double}-float
  (begin
    (define i8-to-i8 (foreign-procedure "i8_to_i8" (integer-8 int) integer-8))
    (define u8-to-u8 (foreign-procedure "u8_to_u8" (unsigned-8 int) unsigned-8))
    (define i16-to-i16 (foreign-procedure "i16_to_i16" (integer-16 int) integer-16))
    (define u16-to-u16 (foreign-procedure "u16_to_u16" (unsigned-16 int) unsigned-16))
    (define i24-to-i24 (foreign-procedure "i32_to_i32" (integer-24 int) integer-24))
    (define u24-to-u24 (foreign-procedure "u32_to_u32" (unsigned-24 int) unsigned-24))
    (define i32-to-i32 (foreign-procedure "i32_to_i32" (integer-32 int) integer-32))
    (define u32-to-u32 (foreign-procedure "u32_to_u32" (unsigned-32 int) unsigned-32))
    (define i40-to-i40 (foreign-procedure "i64_to_i64" (integer-40 int) integer-40))
    (define u40-to-u40 (foreign-procedure "u64_to_u64" (unsigned-40 int) unsigned-40))
    (define i48-to-i48 (foreign-procedure "i64_to_i64" (integer-48 int) integer-48))
    (define u48-to-u48 (foreign-procedure "u64_to_u64" (unsigned-48 int) unsigned-48))
    (define i56-to-i56 (foreign-procedure "i64_to_i64" (integer-56 int) integer-56))
    (define u56-to-u56 (foreign-procedure "u64_to_u64" (unsigned-56 int) unsigned-56))
    (define i64-to-i64 (foreign-procedure "i64_to_i64" (integer-64 int) integer-64))
    (define u64-to-u64 (foreign-procedure "u64_to_u64" (unsigned-64 int) unsigned-64))
    (define sf-to-sf (foreign-procedure "sf_to_sf" (single-float) single-float))
    (define df-to-df (foreign-procedure "df_to_df" (double-float) double-float))
    (define $test-int-to-int
      (lambda (fp size signed?)
        (define n10000 (expt 256 size))
        (define nffff (- n10000 1))
        (define nfffe (- nffff 1))
        (define n8000 (ash n10000 -1))
        (define n8001 (+ n8000 1))
        (define n7fff (- n8000 1))
        (define n7ffe (- n7fff 1))
        (define n100 (expt 16 size))
        (define n101 (+ n100 1))
        (define nff (- n100 1))
        (define nfe (- nff 1))
        (define n80 (ash n100 -1))
        (define n81 (+ n80 1))
        (define n7f (- n80 1))
        (define n7e (- n7f 1))
        (define (expect x k)
          (if signed?
              (if (<= (- n8000) x nffff)
                  (mod0 (+ x k) n10000)
                  'err)
              (if (<= (- n8000) x nffff)
                  (mod (+ x k) n10000)
                  'err)))
        (define (check x)
          (define (do-one x k)
            (let ([a (expect x k)])
              (if (eq? a 'err)
                  (or (= (optimize-level) 3)
                      (guard (c [#t (display-condition c) (newline) #t])
                        (fp x k)
                        (printf "no error for x = ~x, k = ~d\n" x k)
                        #f))
                  (or (eqv? (fp x k) a)
                      (begin
                        (printf "incorrect answer ~x should be ~x for x = ~x, k = ~d\n" (fp x k) a x k)
                        #f)))))
          (list
            (do-one x 1)
            (do-one x -1)
            (do-one (- x) 1)
            (do-one (- x) -1)))
        (andmap
          (lambda (x) (and (list? x) (= (length x) 4) (andmap (lambda (x) (eq? x #t)) x)))
          (list
            (check n10000)
            (check nffff)
            (check nfffe)
            (check n8001)
            (check n8000)
            (check n7fff)
            (check n7ffe)
            (check n101)
            (check n100)
            (check nff)
            (check nfe)
            (check n81)
            (check n80)
            (check n7f)
            (check n7e)
            (check 73)
            (check 5)
            (check 1)
            (check 0)))))
    #t)
  ($test-int-to-int i8-to-i8 1 #t)
  ($test-int-to-int u8-to-u8 1 #f)
  ($test-int-to-int i16-to-i16 2 #t)
  ($test-int-to-int u16-to-u16 2 #f)
  ($test-int-to-int i24-to-i24 3 #t)
  ($test-int-to-int u24-to-u24 3 #f)
  ($test-int-to-int i32-to-i32 4 #t)
  ($test-int-to-int u32-to-u32 4 #f)
  ($test-int-to-int i40-to-i40 5 #t)
  ($test-int-to-int u40-to-u40 5 #f)
  ($test-int-to-int i48-to-i48 6 #t)
  ($test-int-to-int u48-to-u48 6 #f)
  ($test-int-to-int i56-to-i56 7 #t)
  ($test-int-to-int u56-to-u56 7 #f)
  ($test-int-to-int i64-to-i64 8 #t)
  ($test-int-to-int u64-to-u64 8 #f)
  (eqv? (sf-to-sf 73.5) 74.5)
  (eqv? (df-to-df 73.5) 74.5)

  (error? (i8-to-i8 'qqq 0))
  (error? (u8-to-u8 'qqq 0))
  (error? (i16-to-i16 'qqq 0))
  (error? (u16-to-u16 'qqq 0))
  (error? (i24-to-i24 'qqq 0))
  (error? (u24-to-u24 'qqq 0))
  (error? (i32-to-i32 'qqq 0))
  (error? (u32-to-u32 'qqq 0))
  (error? (i64-to-i64 'qqq 0))
  (error? (u64-to-u64 'qqq 0))
  (error? (i8-to-i8 0 "oops"))
  (error? (u8-to-u8 0 "oops"))
  (error? (i16-to-i16 0 "oops"))
  (error? (u16-to-u16 0 "oops"))
  (error? (i32-to-i32 0 "oops"))
  (error? (u32-to-u32 0 "oops"))
  (error? (i64-to-i64 0 "oops"))
  (error? (u64-to-u64 0 "oops"))

  (error? (sf-to-sf 'qqq))
  (error? (df-to-df 'qqq))

  (begin
    (define call-i8 (foreign-procedure "call_i8" (ptr integer-8 int int) integer-8))
    (define call-u8 (foreign-procedure "call_u8" (ptr unsigned-8 int int) unsigned-8))
    (define call-i16 (foreign-procedure "call_i16" (ptr integer-16 int int) integer-16))
    (define call-u16 (foreign-procedure "call_u16" (ptr unsigned-16 int int) unsigned-16))
    (define call-i24 (foreign-procedure "call_i32" (ptr integer-24 int int) integer-24))
    (define call-u24 (foreign-procedure "call_u32" (ptr unsigned-24 int int) unsigned-24))
    (define call-i32 (foreign-procedure "call_i32" (ptr integer-32 int int) integer-32))
    (define call-u32 (foreign-procedure "call_u32" (ptr unsigned-32 int int) unsigned-32))
    (define call-i40 (foreign-procedure "call_i64" (ptr integer-40 int int) integer-40))
    (define call-u40 (foreign-procedure "call_u64" (ptr unsigned-40 int int) unsigned-40))
    (define call-i48 (foreign-procedure "call_i64" (ptr integer-48 int int) integer-48))
    (define call-u48 (foreign-procedure "call_u64" (ptr unsigned-48 int int) unsigned-48))
    (define call-i56 (foreign-procedure "call_i64" (ptr integer-56 int int) integer-56))
    (define call-u56 (foreign-procedure "call_u64" (ptr unsigned-56 int int) unsigned-56))
    (define call-i64 (foreign-procedure "call_i64" (ptr integer-64 int int) integer-64))
    (define call-u64 (foreign-procedure "call_u64" (ptr unsigned-64 int int) unsigned-64))
    (define call-sf (foreign-procedure "call_sf" (ptr single-float int int) single-float))
    (define call-df (foreign-procedure "call_df" (ptr double-float int int) double-float))
    (define call-varargs-df (foreign-procedure "call_varargs_df" (ptr double-float int int) double-float))
    (define call-varargs-i7df (foreign-procedure "call_varargs_i7df" (ptr int
                                                                                    double-float double-float double-float
                                                                                    double-float double-float double-float
                                                                                    double-float)
                                                 double-float))
    (define call-varargs-dfii (foreign-procedure "call_varargs_dfii" (ptr double-float int int) double-float))
    (define call-varargs-dfidf (foreign-procedure "call_varargs_dfidf" (ptr double-float int double-float) double-float))
    (define call-varargs-dfsfi (foreign-procedure "call_varargs_dfsfi" (ptr double-float single-float int) double-float))
    (define ($test-call-int signed? size call-int make-fc)
      (define n10000 (expt 256 size))
      (define nffff (- n10000 1))
      (define nfffe (- nffff 1))
      (define n8000 (ash n10000 -1))
      (define n8001 (+ n8000 1))
      (define n7fff (- n8000 1))
      (define n7ffe (- n7fff 1))
      (define n100 (expt 16 size))
      (define n101 (+ n100 1))
      (define nff (- n100 1))
      (define nfe (- nff 1))
      (define n80 (ash n100 -1))
      (define n81 (+ n80 1))
      (define n7f (- n80 1))
      (define n7e (- n7f 1))
      (define (expect x m k)
        (if signed?
            (if (<= (- n8000) x nffff)
                (mod0 (+ x m k) n10000)
                'err)
            (if (<= (- n8000) x nffff)
                (mod (+ x m k) n10000)
                'err)))
      (define fc (make-fc values))
      (define fp (lambda (x m k) (call-int fc x m k)))
      (define (check x)
        (define (do-one x m k)
          (let ([a (expect x m k)])
            (if (eq? a 'err)
                (or (= (optimize-level) 3)
                    (guard (c [#t (display-condition c) (newline) #t]) (fp x m k)))
                (eqv? (fp x m k) a))))
        (list
          (do-one x 0 0)
          (do-one x 5 7)
          (do-one x -5 7)
          (do-one x 5 -7)
          (do-one x -5 -7)
          (do-one (- x) 0 0)
          (do-one (- x) 5 7)
          (do-one (- x) -5 7)
          (do-one (- x) 5 -7)
          (do-one (- x) -5 -7)))
      (andmap
        (lambda (x) (and (list? x) (= (length x) 10) (andmap (lambda (x) (eq? x #t)) x)))
        (list
          (check n10000)
          (check nffff)
          (check nfffe)
          (check n8001)
          (check n8000)
          (check n7fff)
          (check n7ffe)
          (check n101)
          (check n100)
          (check nff)
          (check nfe)
          (check n81)
          (check n80)
          (check n7f)
          (check n7e)
          (check 73)
          (check 5)
          (check 1)
          (check 0))))
    #t)
  ($test-call-int #t (foreign-sizeof 'integer-8) call-i8
    (lambda (p) (foreign-callable p (integer-8) integer-8)))
  ($test-call-int #t (foreign-sizeof 'integer-16) call-i16
    (lambda (p) (foreign-callable p (integer-16) integer-16)))
  ($test-call-int #t (foreign-sizeof 'integer-24) call-i24
    (lambda (p) (foreign-callable p (integer-24) integer-24)))
  ($test-call-int #t (foreign-sizeof 'integer-32) call-i32
    (lambda (p) (foreign-callable p (integer-32) integer-32)))
  ($test-call-int #t (foreign-sizeof 'integer-40) call-i40
    (lambda (p) (foreign-callable p (integer-40) integer-40)))
  ($test-call-int #t (foreign-sizeof 'integer-48) call-i48
    (lambda (p) (foreign-callable p (integer-48) integer-48)))
  ($test-call-int #t (foreign-sizeof 'integer-56) call-i56
    (lambda (p) (foreign-callable p (integer-56) integer-56)))
  ($test-call-int #t (foreign-sizeof 'integer-64) call-i64
     (lambda (p) (foreign-callable p (integer-64) integer-64)))
  ($test-call-int #f (foreign-sizeof 'unsigned-8) call-u8
    (lambda (p) (foreign-callable p (unsigned-8) unsigned-8)))
  ($test-call-int #f (foreign-sizeof 'unsigned-16) call-u16
    (lambda (p) (foreign-callable p (unsigned-16) unsigned-16)))
  ($test-call-int #f (foreign-sizeof 'unsigned-24) call-u24
    (lambda (p) (foreign-callable p (unsigned-24) unsigned-24)))
  ($test-call-int #f (foreign-sizeof 'unsigned-32) call-u32
    (lambda (p) (foreign-callable p (unsigned-32) unsigned-32)))
  ($test-call-int #f (foreign-sizeof 'unsigned-40) call-u40
    (lambda (p) (foreign-callable p (unsigned-40) unsigned-40)))
  ($test-call-int #f (foreign-sizeof 'unsigned-48) call-u48
    (lambda (p) (foreign-callable p (unsigned-48) unsigned-48)))
  ($test-call-int #f (foreign-sizeof 'unsigned-56) call-u56
    (lambda (p) (foreign-callable p (unsigned-56) unsigned-56)))
  ($test-call-int #f (foreign-sizeof 'unsigned-64) call-u64
    (lambda (p) (foreign-callable p (unsigned-64) unsigned-64)))
  (equal?
    (call-sf
      (foreign-callable
        (lambda (x) (+ x 5))
        (single-float) single-float)
      73.25 7 23)
    108.25)
  (equal?
    (call-df
      (foreign-callable
        (lambda (x) (+ x 5))
        (double-float) double-float)
      73.25 7 23)
    108.25)
  (equal?
    (call-varargs-df
      (foreign-callable
        __varargs
        (lambda (x y) (+ x y 5))
        (double-float double-float) double-float)
      10.25 20 300)
    325.5)
  (equal?
    (call-varargs-i7df
      (foreign-callable
        __varargs
        (lambda (i a b c d e f g) (+ i a b c d e f g 7))
        (int double-float double-float double-float double-float double-float double-float double-float) double-float)
      1 2.2 3.2 4.5 6.7 8.9 10.1 11.5)
    55.1)

  (equal?
   (call-varargs-dfii
    (foreign-callable
     (__varargs_after 2)
     (lambda (x y z) (+ x y z))
     (double-float int int) double-float)
    10.25 20 300)
   620.25)

  (equal?
   (call-varargs-dfidf
    (foreign-callable
     (__varargs_after 2)
     (lambda (x y z) (+ x y z))
     (double-float int double-float) double-float)
    10.25 20 300.25)
   330.75)

  (equal?
   (call-varargs-dfsfi
    (foreign-callable
     (__varargs_after 2)
     (lambda (x y z) (+ x y z))
     (double-float single-float int) double-float)
    10.25 20.0 300)
   620.5)
  
  ;(define call-varargs-dfsfi (foreign-procedure #;__varargs #;2 "call_varargs_dfsfi" (ptr double-float single-float int) double-float))

  (error?
    (call-i8
      (foreign-callable
        (lambda (x) '(- x 7))
        (integer-8) integer-8)
      73 0 0))
  (error?
    (call-u8
      (foreign-callable
        (lambda (x) '(- x 7))
        (unsigned-8) unsigned-8)
      73 0 0))
  (error?
    (call-i16
      (foreign-callable
        (lambda (x) '(- x 7))
        (integer-16) integer-16)
      73 0 0))
  (error?
    (call-u16
      (foreign-callable
        (lambda (x) '(- x 7))
        (unsigned-16) unsigned-16)
      73 0 0))
  (error?
    (call-i32
      (foreign-callable
        (lambda (x) '(- x 7))
        (integer-32) integer-32)
      73 0 0))
  (error?
    (call-u32
      (foreign-callable
        (lambda (x) '(- x 7))
        (unsigned-32) unsigned-32)
      73 0 0))
  (error?
    (call-i64
      (foreign-callable
        (lambda (x) '(- x 7))
        (integer-64) integer-64)
      73 0 0))
  (error?
    (call-u64
      (foreign-callable
        (lambda (x) '(- x 7))
        (unsigned-64) unsigned-64)
      73 0 0))
  (error?
    (call-sf
      (foreign-callable
        (lambda (x) '(- x 7))
        (single-float) single-float)
      73.25 0 0))
  (error?
    (call-df
      (foreign-callable
        (lambda (x) '(- x 7))
        (double-float) double-float)
      73.25 0 0))
  (error?
    (call-varargs-df
      (foreign-callable
        __varargs
        (lambda (x y) '(- x 7))
        (double-float double-float) double-float)
      73.25 0 0))

  (begin
    (define u32xu32->u64
      (foreign-procedure "u32xu32_to_u64" (unsigned-32 unsigned-32)
        unsigned-64))
    (define i32xu32->i64
      (foreign-procedure "i32xu32_to_i64" (integer-32 unsigned-32)
        integer-64))
    (define call-i32xu32->i64
      (foreign-procedure "call_i32xu32_to_i64"
        (ptr integer-32 unsigned-32 int)
        integer-64))
    (define fc-i32xu32->i64
      (foreign-callable i32xu32->i64
        (integer-32 unsigned-32)
        integer-64))
    #t)

  (eqv? (u32xu32->u64 #xFFFFFFFF #xFFFFFFFF) #xFFFFFFFFFFFFFFFF)
  (eqv? (u32xu32->u64 #xFF3FFFFF #xFFFFF0FF) #xFF3FFFFFFFFFF0FF)
  (eqv? (u32xu32->u64 #xFFFFFFFF #xF0000000) #xFFFFFFFFF0000000)

  (eqv? (i32xu32->i64 #x0 #x5) #x5)
  (eqv? (i32xu32->i64 #x7 #x5) #x700000005)
  (eqv? (i32xu32->i64 #xFFFFFFFF #xFFFFFFFF) #x-1)
  (eqv? (fixnum? (i32xu32->i64 #xFFFFFFFF #xFFFFFFFF)) #t)
  (eqv? (i32xu32->i64 #xFFFFFFFF #xFFFFFFFE) #x-2)
  (eqv? (i32xu32->i64 #xFFFFFFFF #x00000000) #x-100000000)
  (eqv? (i32xu32->i64 #xFFFFFFFE #x00000000) #x-200000000)
  (eqv? (i32xu32->i64 #xFFFFFFFF #x00000001) #x-FFFFFFFF)
  (eqv? (i32xu32->i64 #x0 #xFFFFFFFF) #xFFFFFFFF)
  (eqv? (i32xu32->i64 #x7FFFFFFF #xFFFFFFFF) #x7FFFFFFFFFFFFFFF)
  (eqv? (i32xu32->i64 #x80000000 #x00000000) #x-8000000000000000)

  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #x0 #x5 #x13) #x18)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #x7 #x5 7) #x70000000C)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #xFFFFFFFF #xFFFFFFFF -3) #x-4)
  (eqv? (fixnum? (call-i32xu32->i64 fc-i32xu32->i64 #xFFFFFFFF #xFFFFFFFF 0)) #t)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #xFFFFFFFF #xFFFFFFFE -1) #x-3)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #xFFFFFFFF #x00000000 0) #x-100000000)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #xFFFFFFFE #x00000000 0) #x-200000000)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #xFFFFFFFF #x00000001 0) #x-FFFFFFFF)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #x0 #xFFFFFFFF 0) #xFFFFFFFF)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #x7FFFFFFF #xFFFFFFFF 0) #x7FFFFFFFFFFFFFFF)
  (eqv? (call-i32xu32->i64 fc-i32xu32->i64 #x80000000 #x00000000 0) #x-8000000000000000)

 ; check for 64-bit alignment issues
  (begin
    (define ufoo64a
      (foreign-procedure "ufoo64a" (unsigned-64 unsigned-64 unsigned-64 unsigned-64 unsigned-64 unsigned-64 unsigned-64)
        unsigned-64))
    (define ufoo64b
      (foreign-procedure "ufoo64b" (integer-32 unsigned-64 unsigned-64 unsigned-64 unsigned-64 unsigned-64 unsigned-64 unsigned-64)
        unsigned-64))
    (define test-ufoo
      (lambda (foo x a b c d e f g)
        (eqv? (foo x a b c d e f g)
              (mod (+ x (- a b) (- c d) (- e f) g) (expt 2 64)))))
    #t)
  (test-ufoo (lambda (x a b c d e f g) (+ x (ufoo64a a b c d e f g)))
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #x0700000000000080)
  (test-ufoo ufoo64b
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #x0700000000000080)
  (test-ufoo (lambda (x a b c d e f g) (+ x (ufoo64a a b c d e f g)))
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #xC700000000000080)
  (test-ufoo ufoo64b
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #xC700000000000080)
  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (cons (random (expt 2 32))
                    (map random (make-list 7 (expt 2 64))))])
      (unless (apply test-ufoo
                (lambda (x a b c d e f g)
                  (+ x (ufoo64a a b c d e f g)))
                ls)
        (pretty-print ls)
        (errorf #f "failed for ufoo64a on ~s" ls))
      (unless (apply test-ufoo ufoo64b ls)
        (pretty-print ls)
        (errorf #f "failed for ufoo64b on ~s" ls))))
  (begin
    (define ifoo64a
      (foreign-procedure "ifoo64a" (integer-64 integer-64 integer-64 integer-64 integer-64 integer-64 integer-64)
        integer-64))
    (define ifoo64b
      (foreign-procedure "ifoo64b" (integer-32 integer-64 integer-64 integer-64 integer-64 integer-64 integer-64 integer-64)
        integer-64))
    (define test-ifoo
      (lambda (foo x a b c d e f g)
        (eqv? (foo x a b c d e f g)
              (mod0 (+ x (- a b) (- c d) (- e f) g) (expt 2 64)))))
    #t)
  (test-ifoo (lambda (x a b c d e f g) (+ x (ifoo64a a b c d e f g)))
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #x0700000000000080)
  (test-ifoo ifoo64b
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #x0700000000000080)
  (test-ifoo (lambda (x a b c d e f g) (+ x (ifoo64a a b c d e f g)))
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #xC700000000000080)
  (test-ifoo ifoo64b
    #x0000000010000000
    #x0000000120000000
    #x0000002003000000
    #x0000030000400000
    #x0000400000050000
    #x0005000000006000
    #x0060000000000700
    #xC700000000000080)
  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([ls (cons (- (random (expt 2 32)) (expt 2 31))
                    (map (lambda (n) (- (random n) (expt 2 31))) (make-list 7 (expt 2 64))))])
      (unless (apply test-ifoo
                (lambda (x a b c d e f g)
                  (+ x (ifoo64a a b c d e f g)))
                ls)
        (pretty-print ls)
        (errorf #f "failed for ifoo64a on ~s" ls))
      (unless (apply test-ifoo ifoo64b ls)
        (pretty-print ls)
        (errorf #f "failed for ifoo64b on ~s" ls))))
)

(mat foreign-C-types
 ; test void*, int, unsigned, float, etc.
  (begin
    (define int-to-int (foreign-procedure "int_to_int" (int int) int))
    (define unsigned-to-unsigned (foreign-procedure "unsigned_to_unsigned" (unsigned int) unsigned))
    (define unsigned-int-to-unsigned-int (foreign-procedure "unsigned_to_unsigned" (unsigned-int int) unsigned-int))
    (define char-to-char (foreign-procedure "char_to_char" (char) char))
    (define wchar-to-wchar (foreign-procedure "wchar_to_wchar" (wchar) wchar))
    (define short-to-short (foreign-procedure "short_to_short" (short int) short))
    (define unsigned-short-to-unsigned-short (foreign-procedure "unsigned_short_to_unsigned_short" (unsigned-short int) unsigned-short))
    (define long-to-long (foreign-procedure "long_to_long" (long int) long))
    (define unsigned-long-to-unsigned-long (foreign-procedure "unsigned_long_to_unsigned_long" (unsigned-long int) unsigned-long))
    (define long-long-to-long-long (foreign-procedure "long_long_to_long_long" (long-long int) long-long))
    (define unsigned-long-long-to-unsigned-long-long (foreign-procedure "unsigned_long_long_to_unsigned_long_long" (unsigned-long-long int) unsigned-long-long))
    (define float-to-float (foreign-procedure "float_to_float" (float) float))
    (define double-to-double (foreign-procedure "double_to_double" (double) double))
    (define iptr-to-iptr (foreign-procedure "iptr_to_iptr" (iptr int) iptr))
    (define uptr-to-uptr (foreign-procedure "uptr_to_uptr" (uptr int) uptr))
    (define void*-to-void* (foreign-procedure "uptr_to_uptr" (void* int) void*))
    #t)
  ($test-int-to-int int-to-int (foreign-sizeof 'int) #t)
  ($test-int-to-int unsigned-to-unsigned (foreign-sizeof 'unsigned) #f)
  ($test-int-to-int unsigned-int-to-unsigned-int (foreign-sizeof 'unsigned-int) #f)
  ($test-int-to-int short-to-short (foreign-sizeof 'short) #t)
  ($test-int-to-int unsigned-short-to-unsigned-short (foreign-sizeof 'unsigned-short) #f)
  ($test-int-to-int long-to-long (foreign-sizeof 'long) #t)
  ($test-int-to-int unsigned-long-to-unsigned-long (foreign-sizeof 'unsigned-long) #f)
  ($test-int-to-int long-long-to-long-long (foreign-sizeof 'long-long) #t)
  ($test-int-to-int unsigned-long-long-to-unsigned-long-long (foreign-sizeof 'unsigned-long-long) #f)
  ($test-int-to-int iptr-to-iptr (foreign-sizeof 'iptr) #t)
  ($test-int-to-int uptr-to-uptr (foreign-sizeof 'uptr) #f)
  ($test-int-to-int void*-to-void* (foreign-sizeof 'void*) #f)

  (eqv? (char-to-char #\a) #\A)
  (eqv? (wchar-to-wchar #\x3bb) #\x39b)
  (eqv? (float-to-float 73.5) 74.5)
  (eqv? (double-to-double 73.5) 74.5)

  (error? (int-to-int 'qqq 0))
  (error? (unsigned-to-unsigned 'qqq 0))
  (error? (unsigned-int-to-unsigned-int 'qqq 0))
  (error? (unsigned-short-to-unsigned-short 'qqq 0))
  (error? (short-to-short 'qqq 0))
  (error? (long-to-long 'qqq 0))
  (error? (unsigned-long-to-unsigned-long 'qqq 0))
  (error? (long-long-to-long-long 'qqq 0))
  (error? (unsigned-long-long-to-unsigned-long-long 'qqq 0))
  (error? (iptr-to-iptr 'qqq 0))
  (error? (uptr-to-uptr 'qqq 0))
  (error? (void*-to-void* 'qqq 0))
  (error? (int-to-int 0 "oops"))
  (error? (unsigned-to-unsigned 0 "oops"))
  (error? (unsigned-int-to-unsigned-int 0 "oops"))
  (error? (unsigned-short-to-unsigned-short 0 "oops"))
  (error? (short-to-short 0 "oops"))
  (error? (long-to-long 0 "oops"))
  (error? (unsigned-long-to-unsigned-long 0 "oops"))
  (error? (long-long-to-long-long 0 "oops"))
  (error? (unsigned-long-long-to-unsigned-long-long 0 "oops"))
  (error? (iptr-to-iptr 0 "oops"))
  (error? (uptr-to-uptr 0 "oops"))
  (error? (void*-to-void* 0 "oops"))

  (error? (char-to-char 73))
  (error? (char-to-char #\x100))
  (error? (wchar-to-wchar 73))
  (or (= (optimize-level) 3)
      (if (eq? (foreign-sizeof 'wchar) 16)
          (guard? (c [#t]) (wchar-to-char #\x10000) #f)
          #t))
  (error? (float-to-float 'qqq.5))
  (error? (double-to-double 'qqq.5))

  (begin
    (define call-int (foreign-procedure "call_int" (ptr int int int) int))
    (define call-unsigned (foreign-procedure "call_unsigned" (ptr unsigned int int) unsigned))
    (define call-unsigned-int (foreign-procedure "call_unsigned" (ptr unsigned-int int int) unsigned-int))
    (define call-char (foreign-procedure "call_char" (ptr char int int) char))
    (define call-wchar (foreign-procedure "call_wchar" (ptr wchar int int) wchar))
    (define call-short (foreign-procedure "call_short" (ptr short int int) short))
    (define call-unsigned-short (foreign-procedure "call_unsigned_short" (ptr unsigned-short int int) unsigned-short))
    (define call-long (foreign-procedure "call_long" (ptr long int int) long))
    (define call-unsigned-long (foreign-procedure "call_unsigned_long" (ptr unsigned-long int int) unsigned-long))
    (define call-long-long (foreign-procedure "call_long_long" (ptr long-long int int) long-long))
    (define call-unsigned-long-long (foreign-procedure "call_unsigned_long_long" (ptr unsigned-long-long int int) unsigned-long-long))
    (define call-float (foreign-procedure "call_float" (ptr float int int) float))
    (define call-double (foreign-procedure "call_double" (ptr double int int) double))
    (define call-iptr (foreign-procedure "call_iptr" (ptr iptr int int) iptr))
    (define call-uptr (foreign-procedure "call_uptr" (ptr uptr int int) uptr))
    (define call-void* (foreign-procedure "call_uptr" (ptr void* int int) void*))
    #t)
  ($test-call-int #t (foreign-sizeof 'int) call-int
    (lambda (p) (foreign-callable p (int) int)))
  ($test-call-int #f (foreign-sizeof 'unsigned) call-unsigned
    (lambda (p) (foreign-callable p (unsigned) unsigned)))
  ($test-call-int #f (foreign-sizeof 'unsigned-int) call-unsigned-int
    (lambda (p) (foreign-callable p (unsigned-int) unsigned-int)))
  ($test-call-int #t (foreign-sizeof 'short) call-short
    (lambda (p) (foreign-callable p (short) short)))
  ($test-call-int #f (foreign-sizeof 'unsigned-short) call-unsigned-short
    (lambda (p) (foreign-callable p (unsigned-short) unsigned-short)))
  ($test-call-int #t (foreign-sizeof 'long) call-long
    (lambda (p) (foreign-callable p (long) long)))
  ($test-call-int #f (foreign-sizeof 'unsigned-long) call-unsigned-long
    (lambda (p) (foreign-callable p (unsigned-long) unsigned-long)))
  ($test-call-int #t (foreign-sizeof 'long-long) call-long-long
    (lambda (p) (foreign-callable p (long-long) long-long)))
  ($test-call-int #f (foreign-sizeof 'unsigned-long-long) call-unsigned-long-long
    (lambda (p) (foreign-callable p (unsigned-long-long) unsigned-long-long)))
  ($test-call-int #t (foreign-sizeof 'iptr) call-iptr
    (lambda (p) (foreign-callable p (iptr) iptr)))
  ($test-call-int #f (foreign-sizeof 'uptr) call-uptr
    (lambda (p) (foreign-callable p (uptr) uptr)))
  ($test-call-int #f (foreign-sizeof 'void*) call-void*
    (lambda (p) (foreign-callable p (void*) void*)))
  (equal?
    (call-char
      (foreign-callable
        (lambda (x) (integer->char (+ (char->integer x) 5)))
        (char) char)
      #\a 7 11)
    #\x)
  (equal?
    (call-wchar
      (foreign-callable
        (lambda (x) (integer->char (+ (char->integer x) 5)))
        (wchar) wchar)
      #\x3bb 7 11)
    #\x3d2)
  (equal?
    (call-float
      (foreign-callable
        (lambda (x) (+ x 5))
        (float) single-float)
      73.25 7 23)
    108.25)
  (equal?
    (call-double
      (foreign-callable
        (lambda (x) (+ x 5))
        (double) double-float)
      73.25 7 23)
    108.25)

  (error?
    (call-int
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (int) int)
      73 0 0))
  (error?
    (call-unsigned
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (unsigned) unsigned)
      73 0 0))
  (error?
    (call-unsigned-int
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (unsigned-int) unsigned-int)
      73 0 0))
  (error?
    (call-char
      (foreign-callable
        (lambda (x) (list x))
        (char) char)
      #\a 0 0))
  (error?
    (call-wchar
      (foreign-callable
        (lambda (x) (list x))
        (wchar) wchar)
      #\a 0 0))
  (error?
    (call-short
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (short) short)
      73 0 0))
  (error?
    (call-unsigned-short
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (unsigned-short) unsigned-short)
      73 0 0))
  (error?
    (call-long
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (long) long)
      73 0 0))
  (error?
    (call-unsigned-long
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (unsigned-long) unsigned-long)
      73 0 0))
  (error?
    (call-long-long
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (long-long) long-long)
      73 0 0))
  (error?
    (call-unsigned-long-long
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (unsigned-long-long) unsigned-long-long)
      73 0 0))
  (error?
    (call-float
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (float) float)
      73.25 0 0))
  (error?
    (call-double
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (double) double)
      73.25 0 0))
  (error?
    (call-iptr
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (iptr) iptr)
      73 0 0))
  (error?
    (call-uptr
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (uptr) uptr)
      73 0 0))
  (error?
    (call-void*
      (foreign-callable
        (lambda (x) (list x (+ x 1)))
        (void*) void*)
      73 0 0))
)

(mat foreign-ftype
  (begin
    (define-ftype A (struct [x double] [y wchar]))
    (define-ftype B (struct [x (array 10 A)] [y A]))
    (define B->*int (foreign-procedure "uptr_to_uptr" ((* B) int) (* int)))
    (define B->A (foreign-procedure "uptr_to_uptr" ((* B) int) (* A)))
    (define B->uptr (foreign-procedure "uptr_to_uptr" ((* B) int) uptr))
    (define uptr->A (foreign-procedure "uptr_to_uptr" (uptr int) (* A)))
    (define b ((foreign-procedure (if (windows?) "windows_malloc" "malloc") (ssize_t) (* B)) (ftype-sizeof B)))
    #t)
  (eqv?
    (ftype-pointer-address (uptr->A (ftype-pointer-address (ftype-&ref B (y) b)) 0))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (eqv?
    (ftype-pointer-address (uptr->A (ftype-pointer-address b) (* 10 (ftype-sizeof A))))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (eqv?
    (B->uptr b (* 10 (ftype-sizeof A)))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (eqv?
    (ftype-pointer-address (B->A b (* 10 (ftype-sizeof A))))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (begin
    (define uptr->uptr (foreign-callable values (uptr) uptr))
    (define uptr->A (foreign-callable (lambda (a) (make-ftype-pointer A a)) (uptr) (* A)))
    (define B->uptr (foreign-callable ftype-pointer-address ((* B)) uptr))
    (define B->A (foreign-callable (lambda (b) (ftype-&ref B (y) b)) ((* B)) (* A)))
    (define call-B->A (foreign-procedure "call_uptr" (ptr (* B) int int) (* A)))
    #t)
  (eqv?
    (ftype-pointer-address (call-B->A uptr->uptr b (* 5 (ftype-sizeof A)) (* 5 (ftype-sizeof A))))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (eqv?
    (ftype-pointer-address (call-B->A uptr->A b (* 5 (ftype-sizeof A)) (* 5 (ftype-sizeof A))))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (eqv?
    (ftype-pointer-address (call-B->A B->uptr b (* 5 (ftype-sizeof A)) (* 5 (ftype-sizeof A))))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (eqv?
    (ftype-pointer-address (call-B->A B->A b 0 0))
    (ftype-pointer-address (ftype-&ref B (y) b)))
  (begin
    ((foreign-procedure (if (windows?) "windows_free" "free") ((* B)) void) b)
    (set! b #f)
    #t)
  (error? ; unrecognized foreign-procedure argument ftype name
    (foreign-procedure "foo" ((* broken)) void))
  (error? ; invalid foreign-procedure argument type specifier
    (foreign-procedure "foo" ((+ * -)) void))
  (error? ; invalid foreign-procedure argument type specifier
    (foreign-procedure "foo" ((* * *)) void))
  (error? ; invalid foreign-procedure argument type specifier
    (foreign-procedure "foo" ((struct [a int])) void))
  (error? ; invalid foreign-procedure argument type specifier
    (foreign-procedure "foo" (hag) void))
  (error? ; unrecognized foreign-procedure return ftype name
    (foreign-procedure "foo" () (* broken)))
  (error? ; invalid foreign-procedure return type specifier
    (foreign-procedure "foo" () (+ * -)))
  (error? ; invalid foreign-procedure return type specifier
    (foreign-procedure "foo" () (* * *)))
  (error? ; invalid foreign-procedure argument type specifier
    (foreign-procedure "foo" () ((struct [a int]))))
  (error? ; invalid foreign-procedure return type specifier
    (foreign-procedure "foo" () hag))
  (error? ; invalid (non-base) ... ftype
    (foreign-procedure "foo" (A) void))
  (error? ; invalid (non-base) ... ftype
    (foreign-procedure "foo" () A))
  (begin
    (meta-cond
      [(eq? (native-endianness) 'little)
       (define-ftype swap-fixnum (endian big fixnum))]
      [(eq? (native-endianness) 'big)
       (define-ftype swap-fixnum (endian little fixnum))])
    #t)
  (error? ; invalid (swapped) ... ftype
    (foreign-procedure "foo" (swap-fixnum) void))
  (error? ; invalid (swapped) ... ftype
    (foreign-procedure "foo" () swap-fixnum))
  (error? ; invalid syntax
    (define-ftype foo (function "wtf" () void) +))
  (error? ; invalid convention
    (define-ftype foo (function "wtf" () void)))
  (error? ; invalid argument type void
    (define-ftype foo (function (void) int)))
  (equal?
    (let ()
      (define-ftype foo (function (int) void))
      (list (ftype-pointer? (make-ftype-pointer foo 0))
            (ftype-pointer? foo (make-ftype-pointer double 0))
            (ftype-pointer? foo (make-ftype-pointer foo 0))))
    '(#t #f #t))
  (error? ; non-function ftype with "memcpy" address
    (define $fp-bvcopy (make-ftype-pointer double "memcpy")))
  (error? ; unrecognized ftype
    (define $fp-bvcopy (make-ftype-pointer spam "memcpy")))
  (error? ; invalid syntax 
    (define $fp-bvcopy (make-ftype-pointer (struct [x int]) "memcpy")))
  (error? ; invalid function-ftype result type specifier u8
    (let ()
      (define-ftype foo (function (u8* u8* size_t) u8))
      (define $fp-bvcopy (make-ftype-pointer foo "memcpy"))))
  (error? ; invalid function-ftype argument type specifier u8
    (let ()
      (define-ftype foo (function (u8* u8 size_t) u8*))
      (define $fp-bvcopy (make-ftype-pointer foo "memcpy"))))
  (begin
    (define-ftype memcpy_t (function (u8* u8* size_t) u8*))
    (define $fp-bvcopy (ftype-ref memcpy_t () (make-ftype-pointer memcpy_t "memcpy")))
    #t)
  (let ([bv1 (string->utf8 "hello")] [bv2 (make-bytevector 5)])
    ($fp-bvcopy bv2 bv1 5)
    (and (bytevector=? bv1 bv2) (bytevector=? bv1 (string->utf8 "hello"))))
  (begin
    (define-ftype bvcopy-t (function (u8* u8* size_t) u8*))
    (define $fp-bvcopy (ftype-ref bvcopy-t () (make-ftype-pointer bvcopy-t "memcpy")))
    #t)
  (let ([bv1 (string->utf8 "hello")] [bv2 (make-bytevector 5)])
    ($fp-bvcopy bv2 bv1 5)
    (and (bytevector=? bv1 bv2) (bytevector=? bv1 (string->utf8 "hello"))))
  ;; No longer an error since make-ftype-pointer also serves to make foreign-pointers
  #;(error? ; "memcpy" is not a procedure
      (make-ftype-pointer memcpy_t "memcpy"))
  (error? ; unrecognized ftype
    (make-ftype-pointer spam +))
  (error? ; non-function ftype
    (make-ftype-pointer double +))
  (error? ; invalid syntax
    (make-ftype-pointer (struct [x int]) +))
  (eqv?
    (let ()
      (define-ftype foo (function (int int) double))
      (define code
        (make-ftype-pointer foo
          (lambda (x y) (inexact (+ x y)))))
      (let ([code-object (foreign-callable-code-object (ftype-pointer-address code))])
        (dynamic-wind
          (lambda () (lock-object code-object))
          (lambda ()
            (define f (ftype-ref foo () code))
            (f 3 4))
          (lambda () (unlock-object code-object)))))
    7.0)
  (eqv?
    (let ()
      (define-ftype foo (function (int int) double))
      (define code
        (make-ftype-pointer foo
          (lambda (x y) (inexact (+ x y)))))
      (define f (ftype-ref foo () code))
      (let ([x (f 8 4)])
        (unlock-object (foreign-callable-code-object (ftype-pointer-address code)))
        x))
    12.0)
  (eqv?
    (let ()
      (define-ftype foo (function (void* void*) ptrdiff_t))
      (define code (make-ftype-pointer foo -))
      (let ([code-object (foreign-callable-code-object (ftype-pointer-address code))])
        (dynamic-wind
          (lambda () (lock-object code-object))
          (lambda () ((ftype-ref foo () code) 17 (* (most-positive-fixnum) 2)))
          (lambda () (unlock-object code-object)))))
    (- 17 (* (most-positive-fixnum) 2)))
  (eqv?
    (let ()
      (define-ftype foo (function (void* void*) ptrdiff_t))
      (define code (make-ftype-pointer foo -))
      (let ([x ((ftype-ref foo () code) 19 (* (most-positive-fixnum) 2))])
        (unlock-object (foreign-callable-code-object (ftype-pointer-address code)))
        x))
    (- 19 (* (most-positive-fixnum) 2)))
  (eqv?
    (let ()
      (define-ftype foo (function (int int) size_t))
      (define code (make-ftype-pointer foo -))
      (let ([code-object (foreign-callable-code-object (ftype-pointer-address code))])
        (dynamic-wind
          (lambda () (lock-object code))
          (lambda () ((ftype-ref foo () code) 17 32))
          (lambda () (unlock-object code)))))
    (- (expt 2 (* (ftype-sizeof size_t) 8)) 15))
  (eqv?
    (let ()
      (define-ftype foo (function (int int) size_t))
      (define code (make-ftype-pointer foo -))
      (let ([x ((ftype-ref foo () code) 17 32)])
        (unlock-object (foreign-callable-code-object (ftype-pointer-address code)))
        x))
    (- (expt 2 (* (ftype-sizeof size_t) 8)) 15))

  (error? ; not a string
    (foreign-entry #e1e6))

  (error? ; no entry for "i am not defined"
    (foreign-entry "i am not defined"))

  (begin
    (define-ftype F (function (size_t) int))
    (define malloc-fptr1 (make-ftype-pointer F (if (windows?) "windows_malloc" "malloc")))
    (define malloc-fptr2 (make-ftype-pointer F (foreign-entry (if (windows?) "windows_malloc" "malloc"))))
    #t)

  (equal?
    (foreign-address-name (ftype-pointer-address malloc-fptr1))
    (if (windows?) "windows_malloc" "malloc"))

  (equal?
    (foreign-address-name (ftype-pointer-address malloc-fptr2))
    (if (windows?) "windows_malloc" "malloc"))

  (eqv?
    (ftype-pointer-address malloc-fptr1)
    (ftype-pointer-address malloc-fptr2))

  (procedure?
    (ftype-ref F () malloc-fptr1))

  (begin
    (define-ftype SF (struct [i int] [f (* F)]))
    (define sf (make-ftype-pointer SF (foreign-alloc (ftype-sizeof SF))))
    (ftype-set! SF (i) sf 10)
    (ftype-set! SF (f) sf malloc-fptr2)
    #t)

  (ftype-pointer? F (ftype-ref SF (f) sf))

  (procedure? (ftype-ref SF (f *) sf))

  (error?
    (begin
      (define-ftype A (struct [x double] [y wchar]))
      (define-ftype B (struct [x (array 10 A)] [y A]))
      ; see if defns above mess up defn below
      (define-ftype
        [A (function ((* B)) (* B))]
        [B (struct [x A])])))

  (begin
    (define-ftype A (struct [x double] [y wchar]))
    (define-ftype B (struct [x (array 10 A)] [y A]))
    ; see if defns above mess up defn below
    (define-ftype
      [A (function ((* B)) (* B))]
      [B (struct [x (* A)])])
    (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
    (define a (ftype-ref A () (make-ftype-pointer A "idiptr")))
    #t)
  (eqv? (ftype-pointer-address (a b)) (ftype-pointer-address b))

  (begin
    (define-ftype
      [A (function ((* B)) (* B))]
      [B (struct [x (* A)])])
    (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
    (define a (ftype-ref A () (make-ftype-pointer A "idiptr")))
    #t)
  (eqv? (ftype-pointer-address (a b)) (ftype-pointer-address b))

  (begin
    (define-ftype
      [B (struct [x (* A)])]
      [A (function ((* B)) (* B))])
    (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
    (define a (ftype-ref A () (make-ftype-pointer A "idiptr")))
    #t)
  (eqv? (ftype-pointer-address (a b)) (ftype-pointer-address b))

  (begin
    (define-ftype A (function ((* A)) (* A)))
    (define a (make-ftype-pointer A "idiptr"))
    #t)
  (eqv? (ftype-pointer-address ((ftype-ref A () a) a)) (ftype-pointer-address a))

  (begin 
    (define-ftype A (struct [x uptr] [y uptr]))
    (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
    (define ff-init-lock (foreign-procedure "init_lock" ((* uptr)) void))
    (define ff-spinlock (foreign-procedure "spinlock" ((* uptr)) void))
    (define ff-unlock (foreign-procedure "unlock" ((* uptr)) void))
    (define ff-locked-incr (foreign-procedure "locked_incr" ((* uptr)) boolean))
    (define ff-locked-decr (foreign-procedure "locked_decr" ((* uptr)) boolean))
    #t)
  (eq? (ff-init-lock (ftype-&ref A (x) a)) (void))
  (retry-for-spurious (ftype-lock! A (x) a))
  (not (ftype-lock! A (x) a))
  (eq? (ftype-unlock! A (x) a) (void))
  (eq? (ff-spinlock (ftype-&ref A (x) a)) (void))
  (not (ftype-lock! A (x) a))
  (eq? (ff-unlock (ftype-&ref A (x) a)) (void))
  (retry-for-spurious (ftype-lock! A (x) a))
  (eq? (ff-unlock (ftype-&ref A (x) a)) (void))
  (eq? (ff-spinlock (ftype-&ref A (x) a)) (void))
  (not (ftype-lock! A (x) a))
  (eq? (ff-unlock (ftype-&ref A (x) a)) (void))
  (eq? (ftype-set! A (y) a 1) (void))
  (not (ff-locked-incr (ftype-&ref A (y) a)))
  (eqv? (ftype-ref A (y) a) 2)
  (not (ff-locked-decr (ftype-&ref A (y) a)))
  (ff-locked-decr (ftype-&ref A (y) a))
  (eqv? (ftype-ref A (y) a) 0)
  (not (ff-locked-decr (ftype-&ref A (y) a)))
  (ff-locked-incr (ftype-&ref A (y) a))
  (not (ftype-locked-incr! A (y) a))
  (ftype-locked-decr! A (y) a)
  (not (ftype-locked-decr! A (y) a))
  (ftype-locked-incr! A (y) a)
  (not (ftype-locked-incr! A (y) a))
)

(mat foreign-anonymous
  (eqv?
    (let ([addr ((foreign-procedure "idiptr_addr" () iptr))])
      (define idiptr (foreign-procedure addr (scheme-object) scheme-object))
      (idiptr 'friggle))
    'friggle)
)

(machine-case
  [(i3nt ti3nt)
   (mat i3nt-stdcall
     (let ()
        (define (win32:number-32-ptr->number n32ptr)
          (+ (fx+ (char->integer (string-ref n32ptr 0))
               (fxsll (char->integer (string-ref n32ptr 1)) 8)
               (fxsll (char->integer (string-ref n32ptr 2)) 16))
            (* (char->integer (string-ref n32ptr 3)) #x1000000)))
        (define (win32:GetVolumeSerialNumber root)
          (define f-proc
            (foreign-procedure __stdcall "GetVolumeInformationA"
              (string string unsigned-32 string string string string unsigned-32)
              boolean))
          (let ([vol-sid (make-string 4)]
                [max-filename-len (make-string 4)]
                [sys-flags (make-string 4)])
            (and (f-proc root #f 0 vol-sid max-filename-len sys-flags #f 0)
                 (win32:number-32-ptr->number vol-sid))))
        (number? (win32:GetVolumeSerialNumber "C:\\"))))])

(mat single-float
  (= (let ((x (foreign-procedure "sxstos" (single-float single-float)
                single-float)))
       (x 3.0 5.0))
     15)
  (let ((args '(1.25 2.25 3.25 4.25 5.25 6.25 7.25 8.25 9.25 10.25 11.25 12.25)))
    (= (apply + args)
       (apply
         (foreign-procedure "singlesum12"
           (single-float single-float single-float single-float
            single-float single-float single-float single-float
            single-float single-float single-float single-float)
           single-float)
         args)))

  (let ([many_ints_and_floats
         (foreign-procedure "many_ints_and_floats" (int int int int int int float (* float)  int (* float) int float (* float) int)
                            iptr)])
    (= (many_ints_and_floats -1 -2 -3 1 2 3 4.0 (make-ftype-pointer float 5) 6 (make-ftype-pointer float 7) 8 9.0 (make-ftype-pointer float 4) 11)
       (+ -1 -2 -3 1 2 3 4 5 6 7 8 9 4 11)))

  (begin
    (define-ftype three_floats
      (struct (a float) (b float) (c float)))
    (define three-floats
      (let ([p (make-ftype-pointer three_floats (foreign-alloc (ftype-sizeof three_floats)))])
        (ftype-set! three_floats (a) p 10.0)
        (ftype-set! three_floats (b) p 11.0)
        (ftype-set! three_floats (c) p 12.0)
        p))
    #t)

  (let ([proc
         (foreign-procedure "many_doubles_and_three_floats_and_float" (double double double double double double double double (& three_floats) float)
                            double)])
    (= (proc 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 three-floats 9.0)
       (+ 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 11.0 12.0 9.0)))

  (let ([proc
         (foreign-procedure "many_doubles_and_float_and_three_floats" (double double double double double double double double float (& three_floats))
                            double)])
    (= (proc 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 three-floats)
       (+ 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0)))

  (let ([proc
         (foreign-procedure "many_doubles_and_float_and_three_floats" (double double double double double double double double float (& three_floats))
                            double)])
    (= (proc 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 91.0 three-floats)
       (+ 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 91.0 10.0 11.0 12.0)))

  (let ([proc
         (foreign-procedure "many_doubles_and_three_floats_and_three_floats" (double double double double double double double double (& three_floats) (& three_floats))
                            double)])
    (= (proc 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 three-floats (let ([p (make-ftype-pointer three_floats (foreign-alloc (ftype-sizeof three_floats)))])
                                                            (ftype-set! three_floats (a) p 34.0)
                                                            (ftype-set! three_floats (b) p 35.0)
                                                            (ftype-set! three_floats (c) p 36.0)
                                                            p))
       (+ 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 11.0 12.0 34.0 35.0 36.0)))

 )

(mat auto-mat-icks
  (auto-mat-ick "d1d2")
  (auto-mat-ick "s1s2")
  (auto-mat-ick "s1d1")
  (auto-mat-ick "d1s1")
  (auto-mat-ick "n1n2n3n4")
  (auto-mat-ick "d1n1d2")
  (auto-mat-ick "d1n1n2")
  (auto-mat-ick "s1n1n2")
  (auto-mat-ick "n1n2n3d1")
  (auto-mat-ick "n1n2n3s1")
  (auto-mat-ick "n1n2d1")
  (auto-mat-ick "n1d1")
  (auto-mat-ick "s1s2s3s4")
  (auto-mat-ick "s1n1s2n2")
  (auto-mat-ick "d1s1s2")
  (auto-mat-ick "s1s2d1")
  (auto-mat-ick "n1s1n2s2")
  (auto-mat-ick "n1s1n2n3")
  (auto-mat-ick "n1n2s1n3")
  (auto-mat-ick "d1d2s1s2")
  (auto-mat-ick "d1d2n1n2")
  (auto-mat-ick "s1d1s2s3")
 )

(mat foreign-callable
  (begin
    ;; We don't have to use `lock-object` on the result of a `foreign-callable`,
    ;; because it is immobile. We have to keep it live, though.
    (define-syntax with-object-kept-live
      (lambda (x)
        (syntax-case x ()
          [(_ id expr)
           (identifier? #'id)
           #'(let ([v expr])
               (keep-live id)
               v)])))
    #t)

  (error? ; spam is not a procedure
    (foreign-callable 'spam () void))
  (error? ; spam is not a procedure
    (begin (foreign-callable 'spam () void) 'q))
  (error? ; spam is not a procedure
    (if (foreign-callable 'spam () void) 'q 'p))
  (equal?
    (let ()
      (define Sinvoke2
        (foreign-procedure "Sinvoke2"
          (scheme-object scheme-object iptr)
          scheme-object))
      (define Fcons
        (foreign-callable
          (lambda (x y)
            (collect)
            (let ([ls (map (lambda (x) (make-vector 200 x)) (make-list 100))])
              (collect)
              (collect)
              (collect)
              (collect)
              (collect)
              (cons (length ls) (cons x y))))
          (scheme-object iptr)
          scheme-object))
      (define (go) (Sinvoke2 Fcons 4 5))
      (define initial-result (go))
      (let loop ([i 100])
        (if (zero? i)
            initial-result
            (and (equal? initial-result (go))
                 (loop (sub1 i))))))
    '(100 4 . 5))
  (eqv?
    (let ()
      (define Sinvoke2
        (foreign-procedure "Sinvoke2"
          (scheme-object scheme-object iptr)
          scheme-object))
      (define fxFsum
        (foreign-callable
          (lambda (x y)
            (if (fx= x 0)
                y
                (fx+ x (Sinvoke2 fxFsum (fx- x 1) y))))
          (scheme-object iptr)
          scheme-object))
      (define (fxgosum n) (Sinvoke2 fxFsum n 0))
      (fxgosum 20))
    210)
  (eqv?
    (let ()
      (define Sinvoke2
        (foreign-procedure "Sinvoke2"
          (scheme-object scheme-object iptr)
          scheme-object))
      (define Fsum
        (foreign-callable
          (lambda (x y)
            (if (= x 0)
                y
                (+ x (Sinvoke2 Fsum (- x 1) y))))
          (scheme-object iptr)
          scheme-object))
      (define (gosum n) (Sinvoke2 Fsum n (most-positive-fixnum)))
      (gosum 20))
    (+ (most-positive-fixnum) 210))
  (let ()
    (define Fargtest
      (foreign-callable
        (lambda (bool char fixnum double single string)
          (list string single double fixnum char bool))
        (boolean char fixnum double-float single-float string)
        scheme-object))
    (define Sargtest
      (foreign-procedure "Sargtest"
        (iptr boolean char fixnum double-float single-float string)
        scheme-object))
    (define args1 (list #t #\Q 12345 3.1415 2.0 "hit me"))
    (define args2 (list #f #\newline -51293 3.1415 2.5 ""))
    (define args3 (list #f #\newline -51293 3.1415 2.5 #f))
    (let ()
      (define addr
        (foreign-callable-entry-point Fargtest))
      (let ()
          (collect (collect-maximum-generation))
          (collect (collect-maximum-generation))
          (with-object-kept-live
           Fargtest
           (and
            (equal? (apply Sargtest addr args1) (reverse args1))
            (equal? (apply Sargtest addr args2) (reverse args2))
            (equal? (apply Sargtest addr args3) (reverse args3)))))))
  (let ()
    (define Fargtest2
      (foreign-callable
        (lambda (x1 x2 x3 x4 x5 x6)
          (list x6 x5 x4 x3 x2 x1))
        (short int char double short char)
        scheme-object))
    (define Sargtest2
      (foreign-procedure "Sargtest2"
        (iptr short int char double short char)
        scheme-object))
    (define args1 (list 32123 #xc7c7c7 #\% 3.1415 -32768 #\!))
    (define args2 (list 17 #x-987654 #\P -521.125 -1955 #\Q))
    (define args3 (list -7500 #x987654 #\? +inf.0 3210 #\7))
    (let ()
      (define addr
        (foreign-callable-entry-point Fargtest2))
      (let ()
          (collect (collect-maximum-generation))
          (collect (collect-maximum-generation))
          (with-object-kept-live
           Fargtest2
           (and
            (equal? (apply Sargtest2 addr args1) (reverse args1))
            (equal? (apply Sargtest2 addr args2) (reverse args2))
            (equal? (apply Sargtest2 addr args3) (reverse args3)))))))
  (let ()
    (define Frvtest_int32
      (foreign-callable
        (lambda (x) (* x x))
        (scheme-object)
        integer-32))
    (define Srvtest_int32
      (foreign-procedure "Srvtest_int32"
        (scheme-object scheme-object)
        integer-32))
    (and
      (eqv? (Srvtest_int32 Frvtest_int32 16) 256)
      (eqv? (Srvtest_int32 Frvtest_int32 #x8000) #x40000000)))
  (let ()
    (define Frvtest_uns32
      (foreign-callable
        (lambda (x) (- (* x x) 1))
        (scheme-object)
        unsigned-32))
    (define Srvtest_uns32
      (foreign-procedure "Srvtest_uns32"
        (scheme-object scheme-object)
        unsigned-32))
    (and
      (eqv? (Srvtest_uns32 Frvtest_uns32 16) 255)
      (eqv? (Srvtest_uns32 Frvtest_uns32 #x10000) #xffffffff)))
  (let ()
    (define Frvtest_single
      (foreign-callable
        (lambda (x) (* x x))
        (scheme-object)
        single-float))
    (define Srvtest_single
      (foreign-procedure "Srvtest_single"
        (scheme-object scheme-object)
        single-float))
    (eqv? (Srvtest_single Frvtest_single 16.0) 256.0))
  (let ()
    (define Frvtest_double
      (foreign-callable
        (lambda (x) (* x x))
        (scheme-object)
        double-float))
    (define Srvtest_double
      (foreign-procedure "Srvtest_double"
        (scheme-object scheme-object)
        double-float))
    (eqv? (Srvtest_double Frvtest_double 16.0) 256.0))
  (let ()
    (define Frvtest_char
      (foreign-callable
        (lambda (x) (string-ref x 3))
        (scheme-object)
        char))
    (define Srvtest_char
      (foreign-procedure "Srvtest_char"
        (scheme-object scheme-object)
        char))
    (eqv? (Srvtest_char Frvtest_char "abcdefg") #\d))
  (let ()
    (define Frvtest_boolean
      (foreign-callable
        (lambda (x) (equal? x "abcdefg"))
        (scheme-object)
        boolean))
    (define Srvtest_boolean
      (foreign-procedure "Srvtest_int32"
        (scheme-object scheme-object)
        boolean))
    (and
      (eqv? (Srvtest_boolean Frvtest_boolean "abcdefg") #t)
      (eqv? (Srvtest_boolean Frvtest_boolean "gfedcba") #f)))
  (let ()
    (define Frvtest_stdbool
      (foreign-callable
        (lambda (x) (equal? x "abCDEfg"))
        (scheme-object)
        stdbool))
    (define Srvtest_stdbool
      (foreign-procedure "Srvtest_stdbool"
        (scheme-object scheme-object)
        stdbool))
    (and
      (eqv? (Srvtest_stdbool Frvtest_stdbool "abCDEfg") #t)
      (eqv? (Srvtest_stdbool Frvtest_stdbool "gfEDCba") #f)))
  (let ()
    (define Frvtest_fixnum
      (foreign-callable
        (lambda (x) (* x x))
        (scheme-object)
        fixnum))
    (define Srvtest_fixnum
      (foreign-procedure "Srvtest_int32"
        (scheme-object scheme-object)
        fixnum))
    (eqv? (Srvtest_fixnum Frvtest_fixnum 16) 256))
  (let ()
    (define Frvtest_fixnum
      (foreign-callable
        (lambda (x) (* x x))
        (scheme-object)
        void))
    (define Srvtest_fixnum
      (foreign-procedure "Srvtest_int32"
        (scheme-object scheme-object)
        void))
    (eqv? (Srvtest_fixnum Frvtest_fixnum 16) (void)))
  #;(error? (foreign-callable values (scheme-object) foreign-pointer))
  #;(error? (foreign-callable values (scheme-object) (foreign-object 16 4)))
  #;(error? (foreign-callable values (foreign-pointer) void))
  #;(error? (foreign-callable values ((foreign-object 16 4)) void))
  (equal?
    (let ([x 5])
      (define call-twice (foreign-procedure "call_twice" (void* int int) void))
      (let ([co (foreign-callable (lambda (y) (set! x (+ x y))) (int) void)])
        (with-object-kept-live
         co
         (call-twice (foreign-callable-entry-point co) 7 31)))
      x)
    43)
  (equal?
    (let ()
     ; foreign_callable example adapted from foreign.stex
      (define cb-init
        (foreign-procedure "cb_init" () void))
      (define register-callback
        (foreign-procedure "register_callback" (char iptr) void))
      (define event-loop
        (foreign-procedure "event_loop" (string) void))

      (define callback
        (lambda (p)
          (let ([code (foreign-callable p (char) void)])
            (foreign-callable-entry-point code))))
      (let ()
        (define ouch
          (callback
            (lambda (c)
              (printf "Ouch! Hit by '~c'~%" c))))
        (define rats
          (callback
            (lambda (c)
              (printf "Rats! Received '~c'~%" c))))
  
        (cb-init)
        (register-callback #\a ouch)
        (register-callback #\c rats)
        (register-callback #\e ouch)
  
        (parameterize ([current-output-port (open-output-string)])
          (event-loop "abcde")
          (get-output-string (current-output-port)))))
    (format "Ouch! Hit by 'a'~%Rats! Received 'c'~%Ouch! Hit by 'e'~%"))
   ; make sure foreign-procedure's code-object is properly locked when
   ; calling back into Scheme
   (begin
     (define call-collect (lambda () (collect) (collect (collect-maximum-generation))))
     (define code (foreign-callable call-collect () void))
     (collect)
     #t)
   ; this form needs to be after the preceding form and not part of it, so that when
   ; we lock code we don't also lock the code object created by foreign-procedure
   (begin
     (with-object-kept-live
      code
      ((foreign-procedure (foreign-callable-entry-point code) () scheme-object)))
     #t)

  (not (locked-object?
         (let ()
           (define cb (foreign-callable (lambda (i) i) (int) int))
           (define unlock-callback (foreign-procedure "unlock_callback" (void*) void))
           (lock-object cb)
           (unlock-callback (foreign-callable-entry-point cb))
           cb)))
  (not (locked-object?
         (let ()
           (define cb (foreign-callable (lambda (i) i) (int) int))
           (define unlock-callback (foreign-procedure "unlock_callback" (void*) void))
           (lock-object cb)
           (collect)
           (unlock-callback (foreign-callable-entry-point cb))
           cb)))
  (equal?
    (let ()
      (define cb (foreign-callable (lambda (i) (+ i 10)) (int) int))
      (define call-and-unlock (foreign-procedure "call_and_unlock" (void* int) int))
      (lock-object cb)
      (let ([ans (call-and-unlock (foreign-callable-entry-point cb) 5)])
        (list (locked-object? cb) ans)))
    '(#f 15))
  (equal?
    (let ()
      (define cb (foreign-callable (lambda (i) (+ i 10)) (int) int))
      (define call-and-unlock (foreign-procedure "call_and_unlock" (void* int) int))
      (lock-object cb)
      (collect)
      (let ([ans (call-and-unlock (foreign-callable-entry-point cb) 3)])
        (list (locked-object? cb) ans)))
    '(#f 13))
  (begin
    (define $stack-depth (cond
                           [(pb?) 1000] ; interpreter frames are big
                           [else 8000]))
    (define $base-value 37)
    #t)
  (eqv? ; make sure foreign-callable does it's overflow checks
    (let ()
      (define-ftype foo (function (fixnum fixnum) fixnum))
      (define f (lambda (n m) (if (fx= n 0) m (g (fx- n 1) (fx+ m 1)))))
      (define fptr (make-ftype-pointer foo f))
      (define g (ftype-ref foo () fptr))
      (let ([v (f $stack-depth $base-value)])
        (unlock-object
          (foreign-callable-code-object
            (ftype-pointer-address fptr)))
        v))
    (+ $stack-depth $base-value))
  (begin
    (define $with-exit-proc
      ; if you change this, consider changing the definition of with-exit-proc
      ; in foreign.stex
      (lambda (p)
        (define th (lambda () (call/cc p)))
        (define-ftype ->ptr (function () ptr))
        (let ([fptr (make-ftype-pointer ->ptr th)])
          (let ([v ((ftype-ref ->ptr () fptr))])
            (unlock-object
              (foreign-callable-code-object
                (ftype-pointer-address fptr)))
            v))))
    #t)
  (eqv? ; make sure we can jump out of a deep nest of C/Scheme calls
    (let ()
      (define *k*)
      (define-ftype foo (function (fixnum fixnum) fixnum))
      (define f (lambda (n m) (if (fx= n 0) (*k* m) (g (fx- n 1) (fx+ m 1)))))
      (define fptr (make-ftype-pointer foo f))
      (define g (ftype-ref foo () fptr))
      (let ([v ($with-exit-proc
                 (lambda (k)
                   (set! *k* k)
                   (f $stack-depth $base-value)))])
        (unlock-object
          (foreign-callable-code-object
            (ftype-pointer-address fptr)))
        v))
    (+ $stack-depth $base-value))
  (eqv? ; make sure we can jump out a few frames at a time
    (let ()
      (define-ftype foo (function (fixnum fixnum ptr) fixnum))
      (define f
        (lambda (n m k)
          (if (fx= n 0)
              (k m)
              (if (fx= (fxmodulo n 10) 0)
                  (k (call/cc
                       (lambda (k)
                         (g (fx- n 1) (fx+ m 1) k))))
                  (g (fx- n 1) (fx+ m 1) k)))))
      (define fptr (make-ftype-pointer foo f))
      (define g (ftype-ref foo () fptr))
      (let ([v ($with-exit-proc
                 (lambda (k)
                   (f $stack-depth $base-value k)))])
        (unlock-object
          (foreign-callable-code-object
            (ftype-pointer-address fptr)))
        v))
    (+ $stack-depth $base-value))
  (or (= (optimize-level) 3)
      ; make sure we can jump out a few frames at a time, returning from
      ; each with an invalid number of values, just for fun
      (eqv?
        ($with-exit-proc
          (lambda (ignore)
            (define *m*)
            (define *k*)
            (define-ftype foo (function (fixnum fixnum) fixnum))
            (define f
              (lambda (n m)
                (if (fx= n 0)
                    (begin (set! *m* m) (values))
                    (if (fx= (fxmodulo n 10) 0)
                        (begin
                          (set! *m*
                            (call/cc
                              (lambda (k)
                                (fluid-let ([*k* k])
                                  (g (fx- n 1) (fx+ m 1))))))
                          (values))
                        (g (fx- n 1) (fx+ m 1))))))
            (define fptr (make-ftype-pointer foo f))
            (define g (ftype-ref foo () fptr))
            (with-exception-handler
             (lambda (c) (*k* *m*))
             (lambda ()
               (call/cc
                (lambda (k)
                  (fluid-let ([*k* k]) (f $stack-depth $base-value))))))
            (unlock-object
              (foreign-callable-code-object
                (ftype-pointer-address fptr)))
            *m*))
        (+ $stack-depth $base-value)))
  (or (= (optimize-level) 3)
      ; similarly, but with a ptr return value so the values error is signaled
      ; by S_call_help rather than the foreign-procedure wrapper
      (eqv?
        ($with-exit-proc
          (lambda (ignore)
            (define *m*)
            (define *k*)
            (define-ftype foo (function (fixnum fixnum) ptr))
            (define f
              (lambda (n m)
                (if (fx= n 0)
                    (begin (set! *m* m) (values))
                    (if (fx= (fxmodulo n 10) 0)
                        (begin
                          (set! *m*
                            (call/cc
                              (lambda (k)
                                (fluid-let ([*k* k])
                                  (g (fx- n 1) (fx+ m 1))))))
                          (values))
                        (g (fx- n 1) (fx+ m 1))))))
            (define fptr (make-ftype-pointer foo f))
            (define g (ftype-ref foo () fptr))
            (with-exception-handler
              (lambda (c) (*k* *m*))
              (lambda ()
                (call/cc
                  (lambda (k)
                    (fluid-let ([*k* k]) (f $stack-depth $base-value))))))
            (unlock-object
              (foreign-callable-code-object
                (ftype-pointer-address fptr)))
            *m*))
        (+ $stack-depth $base-value)))
  (or (= (optimize-level) 3)
      ; make sure we can jump out a few frames at a time, returning from
      ; each with an fasl-reading error, just for fun
      (eqv?
        (let ()
          (define *m*)
          (define *k*)
          (define ip (open-file-input-port (find-source "mat.ss")))
          (define-ftype foo (function (fixnum fixnum) fixnum))
          (define f
            (lambda (n m)
              (if (fx= n 0)
                  (begin (set! *m* m) (fasl-read ip))
                  (if (fx= (fxmodulo n 10) 0)
                      (begin
                        (set! *m*
                          (call/cc
                            (lambda (k)
                              (fluid-let ([*k* k])
                                (g (fx- n 1) (fx+ m 1))))))
                        (fasl-read ip))
                      (g (fx- n 1) (fx+ m 1))))))
          (define fptr (make-ftype-pointer foo f))
          (define g (ftype-ref foo () fptr))
          ; position "fasl" file at eof to make sure fasl-read isn't tripped up
          ; by something that appears almost valid
          (get-bytevector-all ip)
          (with-exception-handler
            (lambda (c) (*k* *m*))
            (lambda ()
              ($with-exit-proc
                (lambda (k)
                  (fluid-let ([*k* k]) (f $stack-depth $base-value))))))
          (unlock-object
            (foreign-callable-code-object
              (ftype-pointer-address fptr)))
          *m*)
        (+ $stack-depth $base-value)))
  ;; A callable isn't locked, but it's immobile
  (equal?
    (let ()
      (define Sinvoke2
        (foreign-procedure "Sinvoke2"
          (scheme-object scheme-object iptr)
          scheme-object))
      (define Fcons
        (foreign-callable
          (lambda (k y)
            (collect) ; might crash if `Fcons` were mobile
            (k (locked-object? Fcons)))
          (scheme-object iptr)
          scheme-object))
      (list
       ;; Call and normal callable return:
       (let ([v (Sinvoke2 Fcons (lambda (x) x) 5)])
         (list v (locked-object? Fcons)))
       ;; Escape from callable:
       (let ([v ($with-exit-proc (lambda (k) (Sinvoke2 Fcons k 5)))])
         (list v (locked-object? Fcons)))))
    '((#f #f) (#f #f)))

  ;; Make sure the code pointer for a call into a
  ;; foreign procedure is correctly saved for locking
  ;; when entering a callback as a callable:
  (equal?
   (let ()
     (define v 0)
     (define call_many_times (foreign-procedure "call_many_times" (void*) void))
     (define work
       (lambda (n)
         ;; This loop needs to be non-allocating, but
         ;; causes varying numbers of ticks
         ;; to be used up.
         (let loop ([n (bitwise-and n #xFFFF)])
           (unless (zero? n)
             (set! v (add1 v))
             (loop (bitwise-arithmetic-shift-right n 1))))))
     (define handler (foreign-callable work (long) void))
     (with-object-kept-live
      handler
      (call_many_times (foreign-callable-entry-point handler)))
     (unlock-object handler)
     v)
   14995143)

  (equal?
   (let ()
     (define v 0)
     (define call_many_times_bv (foreign-procedure "call_many_times_bv" (void*) void))
     (define work
       (lambda (bv)
         (set! v (+ v (bytevector-u8-ref bv 0)))
         ;; Varying work, as above:
         (let loop ([n (bitwise-and (bytevector-u8-ref bv 1) #xFFFF)])
           (unless (zero? n)
             (set! v (add1 v))
             (loop (bitwise-arithmetic-shift-right n 1))))))
     (define handlers (list (foreign-callable work (u8*) void)
                            (foreign-callable work (u16*) void)
                            (foreign-callable work (u32*) void)))
     (map lock-object handlers)
     (for-each (lambda (handler)
                 (call_many_times_bv (foreign-callable-entry-point handler)))
               handlers)
     (map unlock-object handlers)
     v)
   103500000)

  ;; regression test related to saving registers that hold allocated
  ;; callable argument
  (let* ([call-with-many-args (foreign-procedure "call_with_many_args" (void*) boolean)]
         [result #f]
         [cb (foreign-callable
              (lambda (i s1 s2 s3 s4 i2 s6 s7 i3)
                (set! result
                      (and (eqv? i 0)
                           (equal? (string->utf8 "this") s1)
                           (equal? (string->utf8 "is") s2)
                           (equal? (string->utf8 "working") s3)
                           (equal? (string->utf8 "just") s4)
                           (eqv? i2 1)
                           (equal? (string->utf8 "fine") s6)
                           (equal? (string->utf8 "or does it?") s7)
                           (eqv? i3 2))))
              (int u8* u8* u8* u8* int u8* u8* int)
              void)])
    (with-object-kept-live
     cb
     (call-with-many-args (foreign-callable-entry-point cb)))
    result)

)

(machine-case
  [(i3nt ti3nt)
   (mat i3nt-stdcall-foreign-callable
     (equal?
       (let ()
         (define Sinvoke2
           (foreign-procedure "Sinvoke2_stdcall"
             (scheme-object scheme-object iptr)
             scheme-object))
         (define Fcons
           (foreign-callable __stdcall
             (lambda (x y)
               (collect)
               (let ([ls (make-list 20000 #\z)])
                 (collect)
                 (collect)
                 (collect)
                 (collect)
                 (collect)
                 (cons (length ls) (cons x y))))
             (scheme-object iptr)
             scheme-object))
         (define (go) (Sinvoke2 Fcons 4 5))
         (go))
       '(20000 4 . 5))
     (eqv?
       (let ()
         (define Sinvoke2
           (foreign-procedure "Sinvoke2_stdcall"
             (scheme-object scheme-object iptr)
             scheme-object))
         (define fxFsum
           (foreign-callable __stdcall
             (lambda (x y)
               (if (fx= x 0)
                   y
                   (fx+ x (Sinvoke2 fxFsum (fx- x 1) y))))
             (scheme-object iptr)
             scheme-object))
         (define (fxgosum n) (Sinvoke2 fxFsum n 0))
         (fxgosum 20))
       210)
     (eqv?
       (let ()
         (define Sinvoke2
           (foreign-procedure "Sinvoke2_stdcall"
             (scheme-object scheme-object iptr)
             scheme-object))
         (define Fsum
           (foreign-callable __stdcall
             (lambda (x y)
               (if (= x 0)
                   y
                   (+ x (Sinvoke2 Fsum (- x 1) y))))
             (scheme-object iptr)
             scheme-object))
         (define (gosum n) (Sinvoke2 Fsum n (most-positive-fixnum)))
         (gosum 20))
       536871121))
   (mat i3nt-com
     (eqv?
       (let ()
         (define com-instance ((foreign-procedure "get_com_instance" () iptr)))
         ((foreign-procedure __com 0 (iptr int) int) com-instance 3)
         ((foreign-procedure __com 4 (iptr int) int) com-instance 17))
       37))])

(mat die-gracefully-without-stderr
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports (format "~a -q" (patch-exec-path *scheme*))
                  (buffer-mode block)
                  (native-transcoder))])
    (fprintf to-stdin "(error #f \"oops 1\")\n")
    (flush-output-port to-stdin)
    (let ([s1 (get-line from-stderr)])
      (close-port from-stderr)
      (fprintf to-stdin "(error #f \"oops 2\")\n") ; this message should disappear
      (flush-output-port to-stdin)
      (fprintf to-stdin "(+ 17 44)\n")
      (flush-output-port to-stdin)
      (let ([s2 (get-line from-stdout)])
        (fprintf to-stdin "(reset-handler abort)\n")
        (fprintf to-stdin "(reset-handler)\n")
        (flush-output-port to-stdin)
        (let ([s3 (get-line from-stdout)])
          (close-port from-stdout)
          (fprintf to-stdin "'hello\n") ; should cause exception, then abort (via reset)
          (flush-output-port to-stdin)
          (let ([pid^ (machine-case
                        [(i3nt ti3nt a6nt ta6nt arm64nt tarm64nt) pid]
                        [else ((foreign-procedure "waitpid" (int (* int) int) int) pid (make-ftype-pointer int 0) 0)])])
            (and
              (equal? s1 "Exception: oops 1")
              (equal? s2 "61")
              (equal? s3 "#<procedure abort>")
              (eqv? pid^ pid)))))))
)

(mat varargs
  (begin
    (define load-libc
      (machine-case
        [(i3ob ti3ob a6ob ta6ob a6s2 ta6s2 i3s2 ti3s2 i3qnx ti3qnx i3nb ti3nb a6nb ta6nb)
         '(load-shared-object "libc.so")]
        [(i3le ti3le a6le ta6le arm32le tarm32le arm64le tarm64le ppc32le tppc32le rv64le trv64le la64le tla64le)
         '(load-shared-object "libc.so.6")]
        [(i3fb ti3fb a6fb ta6fb)
         '(load-shared-object "libc.so.7")]
        [(i3nt ti3nt a6nt ta6nt arm64nt tarm64nt)
         '(load-shared-object "msvcrt.dll")]
        [(i3osx ti3osx a6osx ta6osx ppc32osx tppc32osx arm64osx tarm64osx
                a6ios ta6ios arm64ios tarm64ios)
         '(load-shared-object "libc.dylib")]
        [(pb pb32l pb32b pb64l pb64b tpb tpb32l tpb32b tpb64l tpb64b)
         '(let ([try-load (lambda (path)
                            (guard (x [#t #f])
                                   (load-shared-object path)
                                   #t))])
            (or (try-load "libc.so.7")
                (try-load "libc.so.6")
                (try-load "libc.dylib")))]
        [else (error 'load-libc "unrecognized machine type ~s" (machine-type))]))
    (define varargs_df (foreign-procedure (__varargs_after 1) "varargs_df" (double int int) double))
    (define varargs_dfii (foreign-procedure (__varargs_after 2) "varargs_dfii" (double int int) double))
    (define varargs_dfidf (foreign-procedure (__varargs_after 2) "varargs_dfidf" (double int double) double))
    (define varargs_sfdfi (foreign-procedure (__varargs_after 2) "varargs_sfdfi" (float double int) double))
    (define varargs_i7df (foreign-procedure (__varargs_after 1) "varargs_i7df" (int double double double double double double double)
                                            double))
    #t)
  (equal?
    (with-input-from-string
      (separate-eval
        `(begin
           ,load-libc
           (define f (foreign-procedure __varargs "printf" (string double) int))
           (f "(%g)" 3.5)
           (void)))
      read)
    '(3.5))
  (equal?
    (with-input-from-string
      (separate-eval
        `(begin
           ,load-libc
           (define f (foreign-procedure __varargs "printf" (string double double double double double double) int))
           (f "(%g %g %g %g %g %g)" 3.5 2.5 -1.5 6.75 8.25 -9.5)
           (void)))
      read)
    '(3.5 2.5 -1.5 6.75 8.25 -9.5))
  (equal?
    (with-input-from-string
      (separate-eval
        `(begin
           ,load-libc
           (define f (foreign-procedure __varargs "printf" (string double double double double double double double double) int))
           (f "(%g %g %g %g %g %g %g %g)" 3.5 2.5 -1.5 6.75 8.25 -9.5 1e32 -4.5)
           (void)))
      read)
    '(3.5 2.5 -1.5 6.75 8.25 -9.5 1e32 -4.5))
  (equal?
    (with-input-from-string
      (separate-eval
        `(begin
           ,load-libc
           (define f (foreign-procedure __varargs "printf" (string double double double double double double double double double double) int))
           (f "(%g %g %g %g %g %g %g %g %g %g)" 3.5 2.5 -1.5 6.75 8.25 -9.5 1e32 -4.5 7.25 -0.5)
           (void)))
      read)
    '(3.5 2.5 -1.5 6.75 8.25 -9.5 1e32 -4.5 7.25 -0.5))

  (equal? (let ([cb (foreign-callable __varargs
                                      (lambda (x y) (+ x y 5))
                                      (double-float double-float) double-float)])
            (with-object-kept-live
             cb
             ((foreign-procedure __varargs (foreign-callable-entry-point cb)
                                 (double-float double-float) double-float)
              3.4 5.5)))
          13.9)
  (equal? (let ([cb (foreign-callable __varargs
                                      (lambda (x y) (+ x y 5))
                                      (double-float double-float) single-float)])
            (with-object-kept-live
             cb
             ((foreign-procedure __varargs (foreign-callable-entry-point cb)
                                 (double-float double-float) single-float)
              3.5 -5.25)))
          3.25)

  (equal?
   (varargs_df 13.5 7 10)
   30.5)
  (equal?
   (varargs_dfii 13.5 -7 -10)
   -3.5)
  (equal?
   (varargs_dfidf 13.5 10 7.5)
   31.0)
  (equal?
   (varargs_sfdfi 10.5 3.25 8)
   21.75)
  (equal?
   (varargs_i7df 1 2.0 3.0 4.0 5.0 6.0 7.0 8.0)
   36.0)
)

(mat structs
  (begin
    (define-ftype i8 integer-8)
    (define-ftype u8 unsigned-8)
    (define-ftype u16 unsigned-16)
    (define-ftype i64 integer-64)
    (define-syntax check*
      (syntax-rules ()
        [(_ (conv ...) T s [vi ...] [T-ref ...] [T-set! ...])
         (let ()
           (define-ftype callback (function conv ... ((& T)) double))
           (define-ftype callback-two (function conv ... ((& T) (& T)) double))
           (define-ftype pre-int-callback (function conv ... (int (& T)) double))
           (define-ftype pre-double-callback (function conv ... (double (& T)) double))
           (define-ftype callback-r (function conv ... () (& T)))
           (define get (foreign-procedure conv ... (format "f4_get~a" s)
                                          () (& T)))
           (define sum (foreign-procedure conv ... (format "f4_sum~a" s)
                                          ((& T)) double))
           (define sum_two (foreign-procedure conv ... (format "f4_sum_two~a" s)
                                              ((& T) (& T)) double))
           (define sum_pre_int (foreign-procedure conv ... (format "f4_sum_pre_int~a" s)
                                                  (int (& T)) double))
           (define sum_pre_int_int (foreign-procedure conv ... (format "f4_sum_pre_int_int~a" s)
                                                      (int int (& T)) double))
           (define sum_pre_int_int_int_int (foreign-procedure conv ... (format "f4_sum_pre_int_int_int_int~a" s)
                                                              (int int int int (& T)) double))
           (define sum_pre_int_int_int_int_int_int (foreign-procedure conv ... (format "f4_sum_pre_int_int_int_int_int_int~a" s)
                                                                      (int int int int int int (& T)) double))
           (define sum_post_int (foreign-procedure conv ... (format "f4_sum~a_post_int" s)
                                                   ((& T) int) double))
           (define sum_pre_double (foreign-procedure conv ... (format "f4_sum_pre_double~a" s)
                                                     (double (& T)) double))
           (define sum_pre_double_double (foreign-procedure conv ... (format "f4_sum_pre_double_double~a" s)
                                                            (double double (& T)) double))
           (define sum_pre_double_double_double_double (foreign-procedure conv ... (format "f4_sum_pre_double_double_double_double~a" s)
                                                                          (double double double double (& T)) double))
           (define sum_pre_double_double_double_double_double_double_double_double
             (foreign-procedure conv ... (format "f4_sum_pre_double_double_double_double_double_double_double_double~a" s)
                                (double double double double double double double double (& T)) double))
           (define sum_post_double (foreign-procedure conv ... (format "f4_sum~a_post_double" s)
                                                      ((& T) double) double))
           (define cb_send (foreign-procedure conv ... (format "f4_cb_send~a" s)
                                              ((* callback)) double))
           (define cb_send_two (foreign-procedure conv ... (format "f4_cb_send_two~a" s)
                                                  ((* callback-two)) double))
           (define cb_send_pre_int (foreign-procedure conv ... (format "f4_cb_send_pre_int~a" s)
                                                      ((* pre-int-callback)) double))
           (define cb_send_pre_double (foreign-procedure conv ... (format "f4_cb_send_pre_double~a" s)
                                                         ((* pre-double-callback)) double))
           (define sum_cb (foreign-procedure conv ... (format "f4_sum_cb~a" s)
                                             ((* callback-r)) double))
           (define-syntax with-callback
             (syntax-rules ()
               [(_ ([id rhs])
                   body)
                (let ([id rhs])
                  (let ([v body])
                    (unlock-object
                     (foreign-callable-code-object
                      (ftype-pointer-address id)))
                    v))]))
           (and (let ([v (make-ftype-pointer T (malloc_at_boundary (ftype-sizeof T)))])
                  (get v)
                  (and (= (T-ref v) vi)
                       ...
                       (begin
                         (free_at_boundary (ftype-pointer-address v))
                         #t)))
                (let ([a (make-ftype-pointer T (malloc_at_boundary (ftype-sizeof T)))])
                  (T-set! a) ...
                  (and (= (+ vi ...) (sum a))
                       (= (+ vi ... vi ...) (sum_two a a))
                       (= (+ 8 vi ...) (sum_pre_int 8 a))
                       (= (+ 8 9 vi ...) (sum_pre_int_int 8 9 a))
                       (= (+ 8 9 10 11 vi ...) (sum_pre_int_int_int_int 8 9 10 11 a))
                       (= (+ 8 9 10 11 12 13 vi ...) (sum_pre_int_int_int_int_int_int 8 9 10 11 12 13 a))
                       (= (+ 8 vi ...) (sum_post_int a 8))
                       (= (+ 8.25 vi ...) (sum_pre_double 8.25 a))
                       (= (+ 8.25 9.25 vi ...) (sum_pre_double_double 8.25 9.25 a))
                       (= (+ 8.25 9.25 10.25 11.25 vi ...) (sum_pre_double_double_double_double 8.25 9.25 10.25 11.25 a))
                       (= (+ 8.25 9.25 10.25 11.25 12.25 13.25 14.25 15.25 vi ...)
                          (sum_pre_double_double_double_double_double_double_double_double
                           8.25 9.25 10.25 11.25 12.25 13.25 14.25 15.25 a))
                       (= (+ 8.25 vi ...) (sum_post_double a 8.25))
                       (= (+ 1.0 vi ...) (with-callback ([cb (make-ftype-pointer
                                                              callback
                                                              (lambda (r)
                                                                (exact->inexact (+ (T-ref r) ...))))])
                                           (cb_send cb)))
                       (= (+ 1.0 vi ... vi ...) (with-callback ([cb (make-ftype-pointer
                                                                     callback-two
                                                                     (lambda (r1 r2)
                                                                       (exact->inexact (+ (T-ref r1) ...
                                                                                          (T-ref r2) ...))))])
                                                  (cb_send_two cb)))
                       (= (+ 1.0 8 vi ...) (with-callback ([cb (make-ftype-pointer
                                                                pre-int-callback
                                                                (lambda (v r)
                                                                  (exact->inexact (+ v (T-ref r) ...))))])
                                             (cb_send_pre_int cb)))
                       (= (+ 1.0 8.25 vi ...) (with-callback ([cb (make-ftype-pointer
                                                                   pre-double-callback
                                                                   (lambda (v r)
                                                                     (exact->inexact (+ v (T-ref r) ...))))])
                                                (cb_send_pre_double cb)))
                       (= (+ vi ...) (with-callback ([cb (make-ftype-pointer
                                                          callback-r
                                                          (lambda (r)
                                                            (T-set! r) ...))])
                                       (sum_cb cb)))
                       (begin
                         (free_at_boundary (ftype-pointer-address a))
                         #t)))))]))
    (define-syntax check*t
      (syntax-rules ()
        [(_ arg ...)
         (and (check* () arg ...)
              (check* (__collect_safe) arg ...))]))
    (define-syntax check-n
      (syntax-rules ()
        [(_ [ni ti vi] ...)
         (let ()
           (define-ftype T (struct [ni ti] ...))
           (define s (apply string-append
                            "_struct"
                            (let loop ([l '(ti ...)])
                              (cond
                               [(null? l) '()]
                               [else (cons (format "_~a" (car l))
                                           (loop (cdr l)))]))))
           (check*t T s
                    [vi ...]
                    [(lambda (a) (ftype-ref T (ni) a)) ...]
                    [(lambda (a) (ftype-set! T (ni) a vi)) ...]))]))
    (define-syntax check
      (syntax-rules ()
        [(_ t1 v1)
         (check*t t1 (format "_~a" 't1)
                  [v1]
                  [(lambda (a) (ftype-ref t1 () a))]
                  [(lambda (a) (ftype-set! t1 () a v1))])]))
    (define-syntax check-union
      (syntax-rules ()
        [(_ [n0 t0 v0] [ni ti vi] ...)
         (let ()
           (define-ftype T (union [n0 t0] [ni ti] ...))
           (define s (apply string-append
                            "_union"
                            (let loop ([l '(t0 ti ...)])
                              (cond
                               [(null? l) '()]
                               [else (cons (format "_~a" (car l))
                                           (loop (cdr l)))]))))
           (check*t T s
                    [v0]
                    [(lambda (a) (ftype-ref T (n0) a))]
                    [(lambda (a) (ftype-set! T (n0) a v0))]))]))
    (define-syntax check-1
      (syntax-rules ()
        [(_ t1 v1)
         (check-n [x t1 v1])]))
    (define-syntax check-2
      (syntax-rules ()
        [(_ t1 t2 v1 v2)
         (check-n [x t1 v1] [y t2 v2])]))
    (define-syntax check-2-set
      (syntax-rules ()
        [(_ t x)
         (and
          (check-2 t i8 (+ 1 x) 10)
          (check-2 t short (+ 2 x) 20)
          (check-2 t long (+ 3 x) 30)
          (check-2 t i64 (+ 5 x) 50)
          (check-2 short t 6 (+ 60 x))
          (check-2 long t 7 (+ 70 x))
          (check-2 i64 t 9 (+ 90 x))
          (check-2 i8 t 10 (+ 100 x)))]))
    (define-syntax check-3
      (syntax-rules ()
        [(_ t1 t2 t3 v1 v2 v3)
         (check-n [x t1 v1] [y t2 v2] [z t3 v3])]))
    (define-syntax check-3-set
      (syntax-rules ()
        [(_ t x)
         (and
          (check-3 t i8 int (+ 1 x) 10 100)
          (check-3 t short int (+ 2 x) 20 200)
          (check-3 t long int (+ 3 x) 30 300)
          (check-3 t i64 int (+ 5 x) 50 500)
          (check-3 short t int 6 (+ 60 x) 600)
          (check-3 long t int 7 (+ 70 x) 700)
          (check-3 i64 t int 9 (+ 90 x) 900)
          (check-3 i8 t int 10 (+ 100 x) 1000))]))
    (define malloc_at_boundary (foreign-procedure "malloc_at_boundary"
                                                  (int) uptr))
    (define free_at_boundary (foreign-procedure "free_at_boundary"
                                                (uptr) void))
    #t)
  (check i8 -11)
  (check u8 129)
  (check short -22)
  (check u16 33022)
  (check long 33)
  (check int 44)
  (check i64 49)
  (check float 55.0)
  (check double 66.0)
  (check-1 i8 -12)
  (check-1 u8 212)
  (check-1 short -23)
  (check-1 u16 33023)
  (check-1 long 34)
  (check-1 int 45)
  (check-1 i64 48)
  (check-1 float 56.0)
  (check-1 double 67.0)
  (check-2-set int 0)
  (check-2-set float 0.5)
  (check-2-set double 0.25)
  (check-2 int int 4 40)
  (check-2 float float 4.5 40.5)
  (check-2 double double 4.25 40.25)
  (check-3-set int 0)
  (check-3-set float 0.5)
  (check-3-set double 0.25)
  (check-3 i8 i8 i8 4 38 127)
  (check-3 short short short 4 39 399)
  (check-3 int int int 4 40 400)
  (check-3 float float float 4.5 40.5 400.5)
  (check-3 double double double 4.25 40.25 400.25)
  (check-n [x i8 1] [y i8 2] [z i8 3] [w i8 4] [q i8 5])
  (check-n [x i8 1] [y i8 2] [z i8 3] [w i8 4] [q i8 5] [r i8 6] [s i8 7])
  (check-union [x i8 -17])
  (check-union [x u8 217])
  (check-union [x short -27])
  (check-union [x u16 33027])
  (check-union [x long 37])
  (check-union [x int 47])
  (check-union [x i64 49])
  (check-union [x float 57.0])
  (check-union [x double 77.0])
  (check-union [x i8 18] [y int 0])
  (check-union [x short 28] [y int 0])
  (check-union [x long 38] [y int 0])
  (check-union [x int 48] [y int 0])
  (check-union [x i64 43] [y int 0])
  (check-union [x float 58.0] [y int 0])
  (check-union [x double 68.0] [y int 0])

  (let ()
    (define-ftype T (struct
                        [ds (union [d1 double]
                                   [d2 double])]
                        [d3 double]))
    (define s (apply string-append
                     "_union"
                     (let loop ([l '(t0 ti ...)])
                       (cond
                         [(null? l) '()]
                         [else (cons (format "_~a" (car l))
                                     (loop (cdr l)))]))))
    (check*t T
             "_struct_uniondoubledouble_double"
             [99.0 -12.5]
             [(lambda (a) (ftype-ref T (ds d1) a)) (lambda (a) (ftype-ref T (d3) a))]
             [(lambda (a) (ftype-set! T (ds d1) a 99.0)) (lambda (a) (ftype-set! T (d3) a -12.5))]))

  ;; Check that `__collect_safe` saves argument and result floating-point registers
  ;; while activating and deactivating a thread
  (let ()
    (define-ftype T (struct [d double] [i integer-8] [n int]))
    (define sum_pre_double_double_double_double_double_double_double_double
      (foreign-procedure __collect_safe
                         "f4_sum_pre_double_double_double_double_double_double_double_double_struct_double_i8_int"
                         (double double double double double double double double (& T))
                         double))
    (let* ([p (foreign-alloc (ftype-sizeof T))]
           [a (make-ftype-pointer T p)])
      (ftype-set! T (d) a 1.25)
      (ftype-set! T (i) a 10)
      (ftype-set! T (n) a 100)
      (let loop ([i 1000000])
        (cond
         [(zero? i) (foreign-free p) #t]
         [else
          (let ([v (sum_pre_double_double_double_double_double_double_double_double 8.25 9.25 10.25 11.25 12.25 13.25 14.25 15.25 a)])
            (and (= 205.25 v)
                 (loop (sub1 i))))]))))
  (let ()
    (define-ftype T (struct [d double] [i integer-8] [n int]))
    (define-ftype callback (function __collect_safe ((& T)) double))
    (define cb_send (foreign-procedure __collect_safe
                                       "f4_cb_send_struct_double_i8_int"
                                       ((* callback)) double))
    (let ([cb (make-ftype-pointer
               callback
               (lambda (r)
                 (+ (ftype-ref T (d) r)
                    (ftype-ref T (i) r)
                    (ftype-ref T (n) r))))])
      (let loop ([i 1000000])
        (cond
         [(zero? i)
          (unlock-object
           (foreign-callable-code-object
            (ftype-pointer-address cb)))
          #t]
         [else
          (let ([v (cb_send cb)])
            (and (= v 112.25)
                 (loop (sub1 i))))]))))
  )

(mat collect-safe
  (error? (foreign-procedure __collect_safe "unknown" (utf-8) void))
  (error? (foreign-procedure __collect_safe "unknown" (utf-16be) void))
  (error? (foreign-procedure __collect_safe "unknown" (utf-16le) void))
  (error? (foreign-procedure __collect_safe "unknown" (utf-32be) void))
  (error? (foreign-procedure __collect_safe "unknown" (utf-32le) void))
  (error? (foreign-procedure __collect_safe "unknown" (string) void))
  (error? (foreign-procedure __collect_safe "unknown" (wstring) void))
  (error? (foreign-callable __collect_safe (lambda () #f) () utf-8))
  (error? (foreign-callable __collect_safe (lambda () #f) () utf-16le))
  (error? (foreign-callable __collect_safe (lambda () #f) () utf-16be))
  (error? (foreign-callable __collect_safe (lambda () #f) () utf-32le))
  (error? (foreign-callable __collect_safe (lambda () #f) () utf-32be))
  (error? (foreign-callable __collect_safe (lambda () #f) () string))
  (error? (foreign-callable __collect_safe (lambda () #f) () wstring))
  (begin
    (define-ftype thread-callback-T (function __collect_safe (double) double))
    (define (call-with-thread-callback cb-proc proc)
      (let* ([exception #f]
             [callback (make-ftype-pointer thread-callback-T
                         (lambda (arg)
                           ;; Don't let an exception reset this thread.
                           (guard [c (else (set! exception c) 0.0)]
                             (cb-proc arg))))]
             [r (proc callback)])
        (unlock-object
         (foreign-callable-code-object
          (ftype-pointer-address callback)))
        (when exception (raise exception))
        r))
    (define (call-in-unknown-thread-1 proc arg n-times)
      ;; Baseline implementation that uses the current thread
      (let loop ([i 0] [arg arg])
        (cond
         [(= i n-times) arg]
         [else (loop (fx+ i 1) (proc arg))])))
    (define call-in-unknown-thread-2
      ;; Call in the current thread, but through the foreign procedure
      (if (threaded?)
          (let ([call (foreign-procedure "call_in_unknown_thread"
                                         ((* thread-callback-T) double int boolean boolean)
                                         double)])
            (lambda (proc arg n-times)
              (call-with-thread-callback
               proc
               (lambda (callback) (call callback arg n-times #f #t)))))
          call-in-unknown-thread-1))
    (define call-in-unknown-thread-3
      ;; Call in a truly unknown thread:
      (if (threaded?)
          (let ([call (foreign-procedure "call_in_unknown_thread"
                                         ((* thread-callback-T) double int boolean boolean)
                                         double)])
            (lambda (proc arg n-times)
              (call-with-thread-callback
               proc
               (lambda (callback) (call callback arg n-times #t #t)))))
          call-in-unknown-thread-1))
    (define call-in-unknown-thread-4
      ;; In an truly unknown thread, but also using `__collect_safe` to
      ;; deactivate the current thread instead of using `Sdeactivate_thread`
      ;; within the foreign function:
      (if (threaded?)
          (let ([call (foreign-procedure __collect_safe "call_in_unknown_thread"
                                         ((* thread-callback-T) double int boolean boolean)
                                         double)])
            (lambda (proc arg n-times)
              (call-with-thread-callback
               proc
               (lambda (callback) (call callback arg n-times #t #f)))))
          call-in-unknown-thread-1))
    #t)
  ;; These tests will pass only if `collect` can run, where `collect`
  ;; can run only if a single thread is active
  (equal? (call-in-unknown-thread-1 (lambda (n) (collect 0) (+ n 1.0)) 3.5 1)
          4.5)
  (equal? (call-in-unknown-thread-2 (lambda (n) (collect 0) (+ n 1.0)) 3.5 2)
          5.5)
  (equal? (call-in-unknown-thread-3 (lambda (n) (collect 0) (+ n 1.0)) 3.5 3)
          6.5)
  (equal? (call-in-unknown-thread-4 (lambda (n) (collect 0) (+ n 1.0)) 3.5 4)
          7.5)
  (equal? (let loop ([n 10.0])
            (call-in-unknown-thread-4
             (lambda (n)
               (cond
                [(zero? n) (collect) 0.5]
                [else (+ 1.0 (loop (- n 1.0)))]))
             n
             1))
          10.5)
  ;; Try to crash a `__collect_safe` foreign-procedure call by moving the
  ;; return address out from under the foreign procedure. This attempt
  ;; should fail, because deactivating a thread first locks the
  ;; current code object.
  (or (not (threaded?))
      (let ([m (make-mutex)]
            [exception #f]
            [done? #f]
            [ok? #t])
        (fork-thread (lambda ()
                       ;; Don't let an exception reset this thread.
                       (guard [c (else (set! exception c))]
                         (let loop ([i 10])
                           (unless (zero? i)
                             (let ([spin (eval '(foreign-procedure __collect_safe "spin_a_while" (int unsigned unsigned) unsigned))])
                               (spin 100000000 0 1))
                             (loop (sub1 i)))))
                       (mutex-acquire m)
                       (set! done? #t)
                       (mutex-release m)))
        (let loop ()
          (mutex-acquire m)
          (let ([done? done?])
            (mutex-release m)
            (unless done?
              (let loop ([i 10])
                (unless (zero? i)
                  (eval '(foreign-procedure "spin_a_while" () void))
                  (loop (sub1 i))))
              (loop))))
        (when exception (raise exception))
        ok?))

  ;; make sure that lots of threads created to perform very little work
  ;; doesn't use a huge amount of memory --- that is, that some of the
  ;; threads trigger a GC
  (or (not (threaded?))
      (let ([old-max (maximum-memory-bytes)])
        (let ([call (foreign-procedure __collect_safe
                                       "call_in_many_unknown_threads" (uptr int int)
                                       int)]
              ;; manage callable pointer directly so that callback is minimal
              [callable (foreign-callable (lambda (n) n) (int) int)])
          (lock-object callable)
          (and
           (equal? 17
                   (call (foreign-callable-entry-point callable)
                         17
                         10000))
           (begin
             (unlock-object callable)
             (< (maximum-memory-bytes) (* 1.2 old-max)))))))
)

(machine-case
  [(i3nt ti3nt)
   (mat i3nt-stdcall-collect-safe
     (equal?
       (let ()
         (define sum (foreign-procedure __collect_safe __stdcall "_sum_stdcall@8" (int int) int))
	 (sum 3 7))
        10)
     (equal?
      (let ()
        (define Sinvoke2
          (foreign-procedure __collect_safe "Sinvoke2_stdcall"
            (scheme-object scheme-object iptr)
            scheme-object))
        (define Fcons
          (foreign-callable __collect_safe __stdcall
            (lambda (x y) (cons x y))
            (scheme-object iptr)
            scheme-object))
        (Sinvoke2 Fcons 41 51))
      '(41 . 51)))
    (mat i3nt-com-thread
     (eqv?
       (let ()
         (define com-instance ((foreign-procedure "get_com_instance" () iptr)))
         ((foreign-procedure __collect_safe __com 0 (iptr int) int) com-instance 3)
         ((foreign-procedure __collect_safe __com 4 (iptr int) int) com-instance 17))
       37))])

])

(mat reference-bytevector
  (error? (make-reference-bytevector -1))
  (error? (bytevector-reference-ref #vu8(1 2 3) 0))
  (error? (bytevector-reference-ref (make-reference-bytevector 8) -8))
  (< 3 (foreign-sizeof 'ptr)) ;; assumption for some of the following tests
  (error? (bytevector-reference-ref (make-reference-bytevector 3) 1))
  (error? (bytevector-reference-ref (make-reference-bytevector 3) 0))
  (error? (bytevector-reference-ref (make-reference-bytevector 3) -1))
  (error? (guard (x [#t (error 'bytevector-reference-ref "failed as expected")])
                 ;; guard the exception to hide the size of the vector, which
                 ;; varies with the machine's word size
                 (bytevector-reference-ref (make-reference-bytevector (foreign-sizeof 'ptr)) 1)))
  (error? (bytevector-reference-ref (make-reference-bytevector 8) 'oops))
  (error? (bytevector-reference*-ref (make-reference-bytevector 8) -8))
  (error? (bytevector-reference*-ref (make-reference-bytevector 8) 'oops))
  (error? (bytevector-reference*-ref (make-reference-bytevector 3) 1))
  (error? (bytevector-reference*-ref (make-reference-bytevector 3) 0))
  (error? (bytevector-reference*-ref (make-reference-bytevector 3) -1))
  (error? (guard (x [#t (error 'bytevector-reference-ref "failed as expected")])
                 (bytevector-reference-ref (make-reference*-bytevector (foreign-sizeof 'ptr)) 1)))
  (error? (bytevector-reference-set! (make-reference-bytevector 8) -8 #false))
  (error? (bytevector-reference-set! (make-reference-bytevector 8) 'oops #false))
  (error? (bytevector-reference-set! (make-reference-bytevector 3) 1 #false))
  (error? (bytevector-reference-set! (make-reference-bytevector 3) 0 #false))
  (error? (guard (x [#t (error 'bytevector-reference-set! "failed as expected")])
                 (bytevector-reference-set! (make-reference-bytevector  (foreign-sizeof 'ptr)) 1 #false)))
  (error? (reference-address->object #f))
  (error? (reference*-address->object #f))

  (equal? #false (bytevector-reference-ref (make-reference-bytevector (foreign-sizeof 'ptr)) 0))
  (equal? #false (bytevector-reference*-ref (make-reference-bytevector (foreign-sizeof 'ptr)) 0))
  (equal? (void) (bytevector-reference-set! (make-reference-bytevector (foreign-sizeof 'ptr)) 0 #false))

  ;; ensure that a GC correctly handles extra bytes that don't fit into a word:
  (let ([make (lambda ()
                (let ([bv (make-reference-bytevector (- (foreign-sizeof 'ptr) 1))]
                      [bv2 (make-reference-bytevector (+ (foreign-sizeof 'ptr) 1))])
                  (bytevector-u8-set! bv 0 72)
                  (bytevector-u8-set! bv2 (foreign-sizeof 'ptr) 73)
                  (list bv bv2)))])
    (let ([pre (make)])
      (collect-rendezvous)
      (equal? pre
              (make))))

  (not (reference-bytevector? #vu8(1 2 3)))
  (not (reference-bytevector? 7))
  (begin
    (define $reftest-bv (make-reference-bytevector (* 2 (foreign-sizeof 'ptr))))
    (define $reftest-bv-odd (make-reference-bytevector (* 3 (foreign-sizeof 'ptr))))
    (reference-bytevector? $reftest-bv))
  (eqv? (* 2 (foreign-sizeof 'ptr)) (bytevector-length $reftest-bv))
  (eqv? (* 3 (foreign-sizeof 'ptr)) (bytevector-length $reftest-bv-odd))
  (eq? #f (bytevector-reference-ref $reftest-bv 0))
  (eq? #f (bytevector-reference-ref $reftest-bv-odd 0))
  (begin
    (define $reftest-bv2 (bytevector 1 2 3 4 5 6))
    (bytevector-reference-set! $reftest-bv 0 $reftest-bv2)
    (bytevector-reference-set! $reftest-bv-odd 0 $reftest-bv2)
    (collect)
    (eq? $reftest-bv2 (bytevector-reference-ref $reftest-bv 0)))
  (eq? $reftest-bv2 (bytevector-reference-ref $reftest-bv-odd 0))
  (with-interrupts-disabled
   (eqv? (if (= (foreign-sizeof 'ptr) 8)
             (bytevector-u64-native-ref $reftest-bv 0)
             (bytevector-u32-native-ref $reftest-bv 0))
         (object->reference-address $reftest-bv2)))
  (with-interrupts-disabled
   (and (eq? $reftest-bv2
             (reference-address->object (object->reference-address $reftest-bv2)))
        (eq? $reftest-bv2
             (reference*-address->object (object->reference-address $reftest-bv2)))))
  (begin
    (define $reftest-bv3 (bytevector 5 6 7 8))
    (bytevector-reference-set! $reftest-bv (foreign-sizeof 'ptr) $reftest-bv3)
    (bytevector-reference-set! $reftest-bv-odd (foreign-sizeof 'ptr) $reftest-bv3)
    (collect)
    (eq? $reftest-bv2 (bytevector-reference-ref $reftest-bv 0)))
  (eq? $reftest-bv2 (bytevector-reference-ref $reftest-bv-odd 0))
  (eq? $reftest-bv3 (bytevector-reference-ref $reftest-bv (foreign-sizeof 'ptr)))
  (eq? $reftest-bv3 (bytevector-reference*-ref $reftest-bv (foreign-sizeof 'ptr)))
  (eq? $reftest-bv3 (bytevector-reference-ref $reftest-bv-odd (foreign-sizeof 'ptr)))
  (eq? $reftest-bv3 (bytevector-reference*-ref $reftest-bv-odd (foreign-sizeof 'ptr)))

  (let ()
    (lock-object $reftest-bv3)
    (let ([p (if (= (foreign-sizeof 'ptr) 8)
                 (bytevector-u64-native-ref $reftest-bv 8)
                 (bytevector-u32-native-ref $reftest-bv 4))])
      (foreign-set! 'unsigned-8 p 1 77)
      (equal? $reftest-bv3 #vu8(5 77 7 8))))

  (begin
    (unlock-object $reftest-bv3)
    (define $reftest-mem4 (foreign-alloc 20))
    (if (= (foreign-sizeof 'ptr) 8)
        (bytevector-u64-native-set! $reftest-bv 8 $reftest-mem4)
        (bytevector-u32-native-set! $reftest-bv 4 $reftest-mem4))
    (if (= (foreign-sizeof 'ptr) 8)
        (bytevector-u64-native-set! $reftest-bv-odd 8 $reftest-mem4)
        (bytevector-u32-native-set! $reftest-bv-odd 4 $reftest-mem4))
    (eqv? $reftest-mem4 (bytevector-reference*-ref $reftest-bv (foreign-sizeof 'ptr))))
  (eqv? $reftest-mem4 (bytevector-reference*-ref $reftest-bv-odd (foreign-sizeof 'ptr)))

  (begin
    ;; clear reference to `$reftest-mem4` before we free it:
    (if (= (foreign-sizeof 'ptr) 8)
        (bytevector-u64-native-set! $reftest-bv 8 0)
        (bytevector-u32-native-set! $reftest-bv 4 0))
    (if (= (foreign-sizeof 'ptr) 8)
        (bytevector-u64-native-set! $reftest-bv-odd 8 0)
        (bytevector-u32-native-set! $reftest-bv-odd 4 0))
    (foreign-free $reftest-mem4)
    (define $reftest-flv (flvector 3.0 6.0 7.0))
    (bytevector-reference-set! $reftest-bv 0 $reftest-flv)
    (bytevector-reference-set! $reftest-bv-odd 0 $reftest-flv)
    (collect)
    (eq? $reftest-flv (bytevector-reference-ref $reftest-bv 0)))
  (eq? $reftest-flv (bytevector-reference-ref $reftest-bv-odd 0))
  (with-interrupts-disabled
   (eqv? (if (= (foreign-sizeof 'ptr) 8)
             (bytevector-u64-native-ref $reftest-bv 0)
             (bytevector-u32-native-ref $reftest-bv 0))
         (object->reference-address $reftest-flv)))
  (with-interrupts-disabled
   (eq? $reftest-flv
        (reference-address->object (object->reference-address $reftest-flv))))

  (let ()
    (lock-object $reftest-flv)
    (let ([p (if (= (foreign-sizeof 'ptr) 8)
                 (bytevector-u64-native-ref $reftest-bv 0)
                 (bytevector-u32-native-ref $reftest-bv 0))])
      (foreign-set! 'double p 8 77.0)
      (equal? $reftest-flv #vfl(3.0 77.0 7.0))))

  (let ([b (box 45)])
    (bytevector-reference-set! $reftest-bv 0 b)
    (collect)
    (eq? b (bytevector-reference-ref $reftest-bv 0)))

  (reference-bytevector? (make-immobile-reference-bytevector 16))
  (let* ([i (make-immobile-reference-bytevector 16)]
         [p (#%$object-address i 0)]
         [cp (object->reference-address i)])
    (collect)
    (and (eqv? p (#%$object-address i 0))
         (eqv? cp (object->reference-address i))))
  (let ([i (make-immobile-reference-bytevector 16)])
    (bytevector-reference-set! i 0 '#(hello))
    (collect)
    (equal? '#(hello) (bytevector-reference-ref i 0)))

  (begin
    (bytevector-reference-set! $reftest-bv 0 #f)
    (eq? #f (bytevector-reference-ref $reftest-bv 0)))
  (begin
    (bytevector-reference-set! $reftest-bv-odd 0 #f)
    (eq? #f (bytevector-reference-ref $reftest-bv-odd 0)))
)
