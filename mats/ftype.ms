;;; ftype.ms
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

(mat ftype-sizeof
  (equal?
    (list
      (ftype-sizeof integer-8)
      (ftype-sizeof unsigned-8)
      (ftype-sizeof integer-16)
      (ftype-sizeof unsigned-16)
      (ftype-sizeof integer-24)
      (ftype-sizeof unsigned-24)
      (ftype-sizeof integer-32)
      (ftype-sizeof unsigned-32)
      (ftype-sizeof integer-40)
      (ftype-sizeof unsigned-40)
      (ftype-sizeof integer-48)
      (ftype-sizeof unsigned-48)
      (ftype-sizeof integer-56)
      (ftype-sizeof unsigned-56)
      (ftype-sizeof integer-64)
      (ftype-sizeof unsigned-64)
      (ftype-sizeof single-float)
      (ftype-sizeof double-float))
    '(1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 4 8))
  (eqv? (ftype-sizeof char) (foreign-sizeof 'char))
  (eqv? (ftype-sizeof wchar) (foreign-sizeof 'wchar))
  (eqv? (ftype-sizeof short) (foreign-sizeof 'short))
  (eqv? (ftype-sizeof unsigned-short) (foreign-sizeof 'unsigned-short))
  (eqv? (ftype-sizeof int) (foreign-sizeof 'int))
  (eqv? (ftype-sizeof unsigned) (foreign-sizeof 'unsigned))
  (eqv? (ftype-sizeof unsigned-int) (foreign-sizeof 'unsigned-int))
  (eqv? (ftype-sizeof long) (foreign-sizeof 'long))
  (eqv? (ftype-sizeof unsigned-long) (foreign-sizeof 'unsigned-long))
  (eqv? (ftype-sizeof long-long) (foreign-sizeof 'long-long))
  (eqv? (ftype-sizeof unsigned-long-long) (foreign-sizeof 'unsigned-long-long))
  (eqv? (ftype-sizeof float) (foreign-sizeof 'float))
  (eqv? (ftype-sizeof single-float) (foreign-sizeof 'single-float))
  (eqv? (ftype-sizeof double) (foreign-sizeof 'double))
  (eqv? (ftype-sizeof double-float) (foreign-sizeof 'double-float))
  (eqv? (ftype-sizeof void*) (foreign-sizeof 'void*))
  (eqv? (ftype-sizeof iptr) (foreign-sizeof 'iptr))
  (eqv? (ftype-sizeof uptr) (foreign-sizeof 'uptr))
)

(mat ftype-setup
  (begin
    (define max-integer-alignment
      (if (or (> (fixnum-width) 32)
              (memq (machine-type) '(i3nt ti3nt i3qnx ti3qnx arm32le tarm32le ppc32le tppc32le)))
          8
          4))
    (define max-float-alignment
      (if (or (> (fixnum-width) 32)
              (memq (machine-type) '(i3nt ti3nt arm32le tarm32le ppc32le tppc32le)))
          8
          4))
    (define-syntax fptr-free
      (syntax-rules ()
        [(_ fptr)
         (begin
           (foreign-free (ftype-pointer-address fptr))
           (set! fptr #f))]))
    (define-syntax free-after
      (syntax-rules ()
        [(_ fptr e1 e2 ...)
         (let ([ans (begin e1 e2 ...)])
           (fptr-free fptr)
           ans)]))
    #t)
)

(mat ftype
   (error? ; misplaced function type
     (define-ftype IV1 (struct [i integer-8] [f (function (int) int)])))

   (error? ; misplaced function type
     (define-ftype IV1 (union [i uptr] [f (function (int) int)])))

   (error? ; misplaced function type
     (define-ftype IV1 (array 10 (function (int) int))))

   (error? ; misplaced function type
     (let ()
       (define-ftype F1 (function (int) int))
       (define-ftype IV1 (struct [i integer-8] [f F1]))
       3))

   (error? ; misplaced function type
     (let ()
       (define-ftype F1 (function (int) int))
       (define-ftype IV1 (union [i uptr] [f F1]))
       3))

   (error? ; misplaced function type
     (let ()
       (define-ftype F1 (function (int) int))
       (define-ftype IV1 (array 10 F1))
       3))

   (error? ; misplaced function type
     (let ()
       (define-ftype
         [F1 (function (int) int)]
         [IV1 (struct [i integer-8] [f F1])])
       3))

   (begin
     (define-ftype F1 (function (int) int))
     #t)

   (error? ; function ftypes have unknown size
     (ftype-sizeof F1))

   (error? ; cannot calculate offset for function index 10
     (ftype-ref F1 () (make-ftype-pointer F1 0) 10))

   (error? ; cannot calculate offset for function index 1
     (ftype-&ref F1 () (make-ftype-pointer F1 0) 1))

   (error? ; cannot assign non-scalar type
     (ftype-set! F1 () (make-ftype-pointer F1 0) 0 'foo))

   (begin
     (define-ftype F2 (struct [a1 int] [f (* (function (int) int))]))
     #t)

   (error? ; cannot calculate offset for function index 1
     (ftype-ref F2 (f 1) (make-ftype-pointer F2 0)))

   (error? ; cannot calculate offset for function index 14
     (ftype-&ref F2 (f 14) (make-ftype-pointer F2 0)))

   (error? ; cannot calculate offset for function index 7
     (ftype-set! F2 (f 7) (make-ftype-pointer F2 0) 'foo))


 ; ----------------
  (begin
    (define-ftype Aa (struct [a1 integer-8] [a2 integer-16] [a3 integer-8]))
    (define-ftype Ab (struct [b1 integer-8]))
    (define-ftype Ac (struct [c1 Aa] [c2 Ab] [c3 double]))
    #t)
  
  (equal?
    (let ([x (make-ftype-pointer Ac 0)])
      (list
        (ftype-sizeof Aa)
        (ftype-sizeof Ab)
        (ftype-sizeof Ac)
        (ftype-pointer-address (ftype-&ref Ac (c1 a1) x))
        (ftype-pointer-address (ftype-&ref Ac (c1 a2) x))
        (ftype-pointer-address (ftype-&ref Ac (c1 a3) x))
        (ftype-pointer-address (ftype-&ref Ac (c2 b1) x))
        (ftype-pointer-address (ftype-&ref Ac (c3) x))))
    '(6 1 16 0 2 4 6 8))

  (begin
    (define addr (foreign-alloc (ftype-sizeof Ac)))
    (define x (make-ftype-pointer Ac addr))
    #t)

  (ftype-pointer? x)
  (ftype-pointer? Ac x)
  (not (ftype-pointer? Ab x))
  (eqv? (ftype-pointer-address x) addr)
  (eqv? (ftype-pointer-address (ftype-&ref Ac (c1 a1) x)) (+ addr 0))
  (eqv? (ftype-pointer-address (ftype-&ref Ac (c1 a2) x)) (+ addr 2))
  (eqv? (ftype-pointer-address (ftype-&ref Ac (c1 a3) x)) (+ addr 4))
  (eqv? (ftype-pointer-address (ftype-&ref Ac (c2 b1) x)) (+ addr 6))
  (eqv? (ftype-pointer-address (ftype-&ref Ac (c3) x)) (+ addr 8))

  (error? ; not an ftype pointer
    (ftype-&ref Aa (a1) 75))
  (error? ; ftype mismatch
    (ftype-&ref Ab (b1) x))

  (eqv? (ftype-pointer-address (ftype-&ref Ac (c1) x)) (+ addr 0))

  (error? ; unexpected accessor b1
    (ftype-&ref Ac (b1) x))
  (error? ; unexpected accessor 0
    (ftype-&ref Ac (c1 0) x))

  (begin
    (ftype-set! Ac (c1 a1) x 7)
    (ftype-set! Ac (c1 a2) x 30000)
    (ftype-set! Ac (c1 a3) x -15)
    (ftype-set! Ac (c2 b1) x #xFF)
    (ftype-set! Ac (c3) x 3.25)
    #t)

  (error? ; unexpected accessor b1
    (ftype-set! Ac (b1) x 7))
  (error? ; unexpected accessor 0
    (ftype-set! Ac (c1 0) x 7))
  (error? ; ftype mismatch
    (ftype-set! Ab (b1) x 7))
  (error? ; #\a is not an integer-8
    (ftype-set! Ac (c1 a1) x #\a))
  (error? ; 30000 is not an integer-8
    (ftype-set! Ac (c1 a1) x 30000))

  (eqv? (ftype-ref Ac (c1 a1) x) 7)
  (eqv? (ftype-ref Ac (c1 a2) x) 30000)
  (eqv? (ftype-ref Ac (c1 a3) x) -15)
  (eqv? (ftype-ref Ac (c2 b1) x) -1)
  (eqv? (ftype-ref Ac (c3) x) 3.25)
  (eqv? (ftype-ref Aa (a1) (ftype-&ref Ac (c1) x)) 7)
  (eqv? (ftype-ref Aa (a2) (ftype-&ref Ac (c1) x)) 30000)
  (eqv? (ftype-ref Aa (a3) (ftype-&ref Ac (c1) x)) -15)
  (eqv? (ftype-ref Ab (b1) (ftype-&ref Ac (c2) x)) -1)
  (eqv? (ftype-ref double () (ftype-&ref Ac (c3) x)) 3.25)
  (let ([y (ftype-&ref Ac (c3) x)])
    (= (ftype-pointer-address (ftype-&ref double () y))
       (ftype-pointer-address y)))
  (eqv? (foreign-ref 'double (ftype-pointer-address (ftype-&ref Ac (c3) x)) 0) 3.25)
  (let ()
    (define-syntax cast
      (syntax-rules ()
        [(_ ftype x)
         (make-ftype-pointer ftype (ftype-pointer-address x))]))
    (define-ftype double-array (array 1 double))
    (eqv? (ftype-ref double-array (0)
            (cast double-array (ftype-&ref Ac (c3) x)))
      3.25))
  (let ()
    (define-syntax cast
      (syntax-rules ()
        [(_ ftype x)
         (make-ftype-pointer ftype (ftype-pointer-address x))]))
    (define-ftype double-array (array 1 double))
    (let ([y (cast double-array (ftype-&ref Ac (c3) x))])
      (and (ftype-pointer? y)
           (eqv? (ftype-pointer-address y) (ftype-pointer-address (ftype-&ref Ac (c3) x)))
           (ftype-pointer=? y (ftype-&ref Ac (c3) x))
           (eqv? (ftype-ref double-array (0) y) 3.25))))

  (error? ; unexpected accessor b1
    (ftype-ref Ac (b1) x))
  (error? ; unexpected accessor 0
    (ftype-ref Ac (c1 0) x))
  (error? ; ftype mismatch
    (ftype-ref Ab (b1) x))
  (error? ; ftype mismatch
    (ftype-ref Aa (a1) (ftype-&ref Ac (c2) x)))

  (begin
    (foreign-free addr)
    #t)

 ; ----------------

  (begin
    (define-ftype Ba (struct [a1 integer-8] [a2 integer-32] [a3 integer-8]))
    (define-ftype Bb (struct [b1 integer-8]))
    (define-ftype Bc (struct [c1 Ba] [c2 Bb] [c3 double]))
    #t)
    
  (equal?
    (let ([x (make-ftype-pointer Bc 0)])
      (list
        (ftype-sizeof Ba)
        (ftype-sizeof Bb)
        (ftype-sizeof Bc)
        (ftype-pointer-address (ftype-&ref Bc (c1 a1) x))
        (ftype-pointer-address (ftype-&ref Bc (c1 a2) x))
        (ftype-pointer-address (ftype-&ref Bc (c1 a3) x))
        (ftype-pointer-address (ftype-&ref Bc (c2 b1) x))
        (ftype-pointer-address (ftype-&ref Bc (c3) x))))
    '(12 1 24 0 4 8 12 16))

 ; ----------------

  (begin
    (define-ftype Ca (struct [a1 integer-8] [a2 double] [a3 integer-8]))
    (define-ftype Cb (struct [b1 integer-8]))
    (define-ftype Cc (struct [c1 Ca] [c2 Cb] [c3 double]))
    #t)

  (equal?
    (let ([x (make-ftype-pointer Cc 0)])
      (list
        (ftype-sizeof Ca)
        (ftype-sizeof Cb)
        (ftype-sizeof Cc)
        (ftype-pointer-address (ftype-&ref Cc (c1 a1) x))
        (ftype-pointer-address (ftype-&ref Cc (c1 a2) x))
        (ftype-pointer-address (ftype-&ref Cc (c1 a3) x))
        (ftype-pointer-address (ftype-&ref Cc (c2 b1) x))
        (ftype-pointer-address (ftype-&ref Cc (c3) x))))
    (if (< max-float-alignment 8)
        '(16 1 28 0 4 12 16 20)
        '(24 1 40 0 8 16 24 32)))
    
 ; ----------------

  (begin
    (define-ftype Da (struct [a1 integer-8] [a2 integer-64] [a3 integer-8]))
    (define-ftype Db (struct [b1 integer-8]))
    (define-ftype Dc (struct [c1 Da] [c2 Db] [c3 integer-64]))
    #t)

  (equal?
    (let ([x (make-ftype-pointer Dc 0)])
      (list
        (ftype-sizeof Da)
        (ftype-sizeof Db)
        (ftype-sizeof Dc)
        (ftype-pointer-address (ftype-&ref Dc (c1 a1) x))
        (ftype-pointer-address (ftype-&ref Dc (c1 a2) x))
        (ftype-pointer-address (ftype-&ref Dc (c1 a3) x))
        (ftype-pointer-address (ftype-&ref Dc (c2 b1) x))
        (ftype-pointer-address (ftype-&ref Dc (c3) x))))
    (if (< max-integer-alignment 8)
        '(16 1 28 0 4 12 16 20)
        '(24 1 40 0 8 16 24 32)))
    
 ; ----------------

  (begin
    (define-ftype Ea
      (struct
        [x integer-32]
        [y double-float]
        [z (array 25 (struct [_ integer-16] [b integer-16]))]
        [w (struct
             [a integer-32]
             [b (union
                  [b1 (struct [a integer-32] [b integer-32])]
                  [b2 (struct [a integer-8] [b double])])])]
        [v (* Ac)]))
    #t)

  (equal?
    (let ([x (make-ftype-pointer Ea 0)])
      (list
        (ftype-sizeof Ea)
        (ftype-pointer-address (ftype-&ref Ea (x) x))
        (ftype-pointer-address (ftype-&ref Ea (y) x))
        (ftype-pointer-address (ftype-&ref Ea (z) x))
        (ftype-pointer-address (ftype-&ref Ea (w) x))
        (ftype-pointer-address (ftype-&ref Ea (v) x))
        (ftype-pointer-address (ftype-&ref Ea (z 0) x))
        (ftype-pointer-address (ftype-&ref Ea (z 4 b) x))
        (ftype-pointer-address (ftype-&ref Ea (w a) x))
        (ftype-pointer-address (ftype-&ref Ea (w b) x))
        (ftype-pointer-address (ftype-&ref Ea (w b b1) x))
        (ftype-pointer-address (ftype-&ref Ea (w b b1 a) x))
        (ftype-pointer-address (ftype-&ref Ea (w b b1 b) x))
        (ftype-pointer-address (ftype-&ref Ea (w b b2) x))
        (ftype-pointer-address (ftype-&ref Ea (w b b2 a) x))
        (ftype-pointer-address (ftype-&ref Ea (w b b2 b) x))))
    (if (< max-float-alignment 8)
        '(132 0 4 12 112 128 12 30 112 116 116 116 120 116 116 120)
        '(152 0 8 16 120 144 16 34 120 128 128 128 132 128 128 136)))

  (begin
    (define-ftype Eb
      (packed
        (struct
          [x integer-32]
          [y double-float]
          [z (array 25 (struct [_ integer-16] [b integer-16]))]
          [w (struct
               [a integer-32]
               [b (union
                    [b1 (struct [a integer-32] [b integer-32])]
                    [b2 (struct [a integer-8] [b double])])])]
          [v (* Ac)])))
    #t)

  (equal?
    (let ([x (make-ftype-pointer Eb 0)])
      (list
        (ftype-sizeof Eb)
        (ftype-pointer-address (ftype-&ref Eb (x) x))
        (ftype-pointer-address (ftype-&ref Eb (y) x))
        (ftype-pointer-address (ftype-&ref Eb (z) x))
        (ftype-pointer-address (ftype-&ref Eb (w) x))
        (ftype-pointer-address (ftype-&ref Eb (v) x))
        (ftype-pointer-address (ftype-&ref Eb (z 0) x))
        (ftype-pointer-address (ftype-&ref Eb (z 4 b) x))
        (ftype-pointer-address (ftype-&ref Eb (w a) x))
        (ftype-pointer-address (ftype-&ref Eb (w b) x))
        (ftype-pointer-address (ftype-&ref Eb (w b b1) x))
        (ftype-pointer-address (ftype-&ref Eb (w b b1 a) x))
        (ftype-pointer-address (ftype-&ref Eb (w b b1 b) x))
        (ftype-pointer-address (ftype-&ref Eb (w b b2) x))
        (ftype-pointer-address (ftype-&ref Eb (w b b2 a) x))
        (ftype-pointer-address (ftype-&ref Eb (w b b2 b) x))))
    (if (< (fixnum-width) 32)
        '(129 0 4 12 112 125 12 30 112 116 116 116 120 116 116 117)
        '(133 0 4 12 112 125 12 30 112 116 116 116 120 116 116 117)))

 ; ----------------

  (equal?
    (let ()
      (define-ftype A (struct [a1 integer-32]))
      (define-ftype B (struct [b1 A] [b2 (* A)]))
      (define x (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
      (define y (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (ftype-set! B (b2) x y)
      (ftype-set! A (a1) y 72)
      (ftype-set! B (b1 a1) x -35)
      (free-after x
        (free-after y
          (list (ftype-ref A (a1) y) (ftype-ref B (b1 a1) x) (ftype-ref B (b2 * a1) x)))))
    '(72 -35 72))

  (begin
    (define base-ftype*
      `((short . "short")
        (unsigned-short . "unsigned short")
        (int . "int")
        (unsigned . "unsigned")
        (unsigned-int . "unsigned int")
        (long . "long")
        (unsigned-long . "unsigned long")
        (long-long . "int64_t")
        (unsigned-long-long . "uint64_t")
        (char . "char")
        (wchar . "wchar")
        (float . "float")
        (double . "double")
        (void* . "void *")
        (iptr . ,(if (< (fixnum-width) 32) "int32_t" "int64_t"))
        (uptr . ,(if (< (fixnum-width) 32) "uint32_t" "uint64_t"))
        (fixnum . ,(if (< (fixnum-width) 32) "int32_t" "int64_t"))
        (boolean . "int")
        (integer-8 . "int8_t")
        (unsigned-8 . "uint8_t")
        (integer-16 . "int16_t")
        (unsigned-16 . "uint16_t")
        (integer-32 . "int32_t")
        (unsigned-32 . "uint32_t")
        (integer-64 . "int64_t")
        (unsigned-64 . "uint64_t")
        (single-float . "float")
        (double-float . "double")))

    (define ftype-paths
      (lambda (name ftype alist)
        (map reverse
          (let f ([ftype ftype] [path (list name)] [path* '()])
            (if (symbol? ftype)
                (cond
                  [(assq ftype alist) =>
                   (lambda (a) (f (cdr a) path path*))]
                  [else (cons path path*)])
                (cons path
                  (record-case ftype
                    [(struct) field*
                     (fold-right
                       (lambda (field path*)
                         (f (cadr field) (cons (car field) path) path*))
                       path* field*)]
                    [(union) field*
                     (fold-right
                       (lambda (field path*)
                         (f (cadr field) (cons (car field) path) path*))
                       path* field*)]
                    [(array) (length ftype)
                     (if (= length 0)
                         path*
                         (f ftype (cons (- length 1) path) path*))]
                    [(*) (ftype) path*]
                    [else
                     (errorf 'ftype-paths "can't handle ~s" ftype)])))))))

    (define ftype-code
      (lambda (ftype name)
        (if (symbol? ftype)
            (cond
              [(assq ftype base-ftype*) =>
               (lambda (a) (format "~a ~a;" (cdr a) name))]
              [else (format "typedef_~a ~a;" ftype name)])
            (record-case ftype
              [(struct) field*
               (format "struct { ~{~a ~}} ~a;"
                 (map
                   (lambda (field) (ftype-code (cadr field) (car field)))
                   field*)
                 name)]
              [(union) field* 
               (format "union { ~{~a ~}} ~a;"
                 (map
                   (lambda (field) (ftype-code (cadr field) (car field)))
                   field*)
                 name)]
              [(array) (length ftype)
               (ftype-code ftype (format "~a[~d]" name length))]
              [(*) (ftype)
               (ftype-code ftype (format "*~a" name))]
              [else
               (errorf 'ftype-code "can't handle ~s" ftype)]))))

    (define C-test-code
      (lambda (ftype-defn* path* ndefs npaths i* j*)
        (let ([ndefs (length ftype-defn*)])
          (printf "#include \"~a/ftype.h\"\n\
                   #define offset(x, y) (int)((char *)&y - (char *)&x)\n\
                   EXPORT int *foo() {\n\
                     ~{~a\n~}\
                     static int a[~d];\n\
                     ~{~a\n~}\
                     ~{~a\n~}\
                     return a;\
                   }\n"
            *mats-dir*
            (map
              (lambda (ftype-defn)
                (format "typedef ~a typedef_~a ~a;"
                  (ftype-code (cdr ftype-defn) (format "typedef_~a" (car ftype-defn)))
                  (car ftype-defn)
                  (car ftype-defn)))
              ftype-defn*)
            (+ ndefs npaths)
            (map
              (lambda (i ftype-defn)
                (format "a[~a] = sizeof(~a);" i (car ftype-defn)))
              i* ftype-defn*)
            (map
              (lambda (j path)
                (format "a[~d] = offset(~a,~a~{~a~});"
                  j
                  (car path)
                  (car path)
                  (map (lambda (x)
                         (if (and (integer? x) (exact? x))
                             (format "[~d]" x)
                             (format ".~a" x)))
                    (cdr path))))
              j* path*)))))

    (define C-compile&load
      (lambda (testfile thunk)
        (let ([testfile.c (format "testfile-~a.c" testfile)]
              [testfile.so (format "testfile-~a.~:[so~;dll~]" testfile
                             (windows?))])
          (with-output-to-file testfile.c thunk 'replace)
          (unless (= (case (machine-type)
                       [(i3osx ti3osx)
                        (system (format "cc -m32 -dynamiclib -o ~a ~a" testfile.so testfile.c))]
                       [(a6osx a6osx)
                        (system (format "cc -m64 -dynamiclib -o ~a ~a" testfile.so testfile.c))]
                       [(a6nt ta6nt)
                        (system (format "set cl= && ~a\\..\\c\\vs.bat amd64 && cl /DWIN32 /Fe~a /LD /MD /nologo ~a"
                                  (patch-exec-path *mats-dir*) testfile.so testfile.c))]
                       [(i3nt ti3nt)
                        (system (format "set cl= && ~a\\..\\c\\vs.bat x86 && cl /DWIN32 /Fe~a /LD /MD /nologo ~a"
                                  (patch-exec-path *mats-dir*) testfile.so testfile.c))]
                       [(arm32le tarm32le)
                        (system (format "cc -fPIC -shared -o ~a ~a" testfile.so testfile.c))]
                       [else ; this should work for most intel-based systems that use gcc...
                        (if (> (fixnum-width) 32)
                            (system (format "cc -m64 -fPIC -shared -o ~a ~a" testfile.so testfile.c))
                            (system (format "cc -m32 -fPIC -shared -o ~a ~a" testfile.so testfile.c)))])
                     0)
            (errorf 'ftype-test "C compilation failed"))
          (load-shared-object (format "./~a" testfile.so)))))

    (define-syntax ftype-test
      (lambda (x)
        (syntax-case x ()
          [(_ testfile (id ftype) ...)
           (with-syntax ([((path ...) ...)
                          (let ([id* (datum (id ...))]
                                [ftype* (datum (ftype ...))])
                            (let ([alist (map cons id* ftype*)])
                              (map
                                (lambda (id ftype)
                                  (map (lambda (x) (datum->syntax #'* x))
                                    (ftype-paths id ftype alist)))
                                id* ftype*)))])
               (let ([ndefs (length #'(ftype ...))]
                     [npaths (length #'(path ... ...))])
                 (with-syntax ([(i ...) (enumerate #'(ftype ...))]
                               [(j ...) (list-tail (enumerate #'(ftype ... path ... ...)) ndefs)]
                               [((idx . pathx) ...) #'(path ... ...)])
                   #`(begin
                       (define-ftype id ftype) ...
                       (define-ftype result-type (array #,(+ ndefs npaths) int))
                       (C-compile&load testfile
                         (lambda ()
                           (C-test-code
                             '((id . ftype) ...) '(path ... ...)
                             #,ndefs #,npaths
                             '(i ...) '(j ...))))

                       (let ([results (make-ftype-pointer result-type
                                        ((foreign-procedure "foo" () void*)))]
                             [status #t])
                         (let ([Scheme-size (ftype-sizeof id)] [C-size (ftype-ref result-type (i) results)])
                           (unless (= Scheme-size C-size)
                             (printf "sizeof check failed for ~s (C says ~s, Scheme says ~s)\n" 'ftype C-size Scheme-size)
                             (set! status #f)))
                         ...
                         (let ([Scheme-addr (ftype-pointer-address (ftype-&ref idx pathx (make-ftype-pointer idx 0)))]
                               [C-addr (ftype-ref result-type (j) results)])
                           (unless (= Scheme-addr C-addr)
                             (printf "address check failed for ~s (C says ~s, Scheme says ~s)\n"
                               (cons 'idx 'pathx) C-addr Scheme-addr)
                             (set! status #f)))
                         ...
                         status)))))])))

    #t)

 ; can pack as many of these together as we want
 ; should avoid too many ftype-test forms to avoid
 ; excessive number of shared object
 ; NB. choose a different testfile name for each
  (ftype-test "ftype1"
    [Aa (struct [a1 integer-8] [a2 integer-16] [a3 integer-8])]
    [Ab (struct [b1 integer-8])]
    [Ac (struct [c1 Aa] [c2 Ab] [c3 double])]

    [A int]
    [B (struct [a int] [b char])]
    [C (struct [c1 B] [c2 A] [c3 double])]
    [D (struct
         [x integer-32]
         [y double-float]
         [z (array 25 (struct [a integer-16] [b integer-16]))]
         [w (struct
              [a integer-32]
              [b (union
                   [b1 (struct [a integer-32] [b integer-32])]
                   [b2 (struct [a integer-8] [b double])])])]
         [v (* C)])]
    [E (struct
         [z (array 25 (struct [a unsigned-short] [b unsigned]))]
         [x unsigned-long]
         [w (struct
              [a long-long]
              [b (union
                   [b1 (struct [a int] [b int])]
                   [b2 (struct [a char] [b double])])])]
         [y double]
         [u (array 9 float)]
         [v (* C)]
         [t char])]
    [F (struct
         [a integer-32]
         [b double])]
    [G (struct
         [a double]
         [b integer-32])]
    [H (struct
         [a integer-32]
         [b (union
              [b1 double]
              [b2 (struct [b2a integer-32] [b2b integer-32])])])]
    [I (struct
         [a integer-32]
         [b (array 1 double)])]
    [J (struct
         [a (array 1 double)]
         [b integer-32])]
    [K1 (union
          [a double]
          [b (struct
               [a integer-32]
               [b integer-32])])]
    [K2 (struct
          [a K1]
          [b integer-32])]
    [K2x (struct
           [a integer-32]
           [b (union
                [a double]
                [b (struct
                     [a integer-32]
                     [b integer-32])])])]
    [K3 (struct
          [a integer-32]
          [b K1])]
    [K3x (struct
           [a integer-32]
           [b (union
                [a double]
                [b (struct
                     [a integer-32]
                     [b integer-32])])])]
    [M1 (union
          [b (struct
               [a integer-32]
               [b double])]
          [a double])]
    [M2 (struct [a M1] [b integer-32])]
    [M3 (struct [a integer-32] [b M1])]
    [N1 (struct [a integer-32] [b integer-64])]
  )

 ; ----------------

  (equal?
    (let ()
       (define-ftype A
         (struct
           [a1 double]
           [a2 float]
           [a3 long-long]
           [a4 unsigned-long-long]
           [a5 long]
           [a6 unsigned-long]
           [a7 int]
           [a8 unsigned]
           [a9 unsigned-int]
           [a10 short]
           [a11 unsigned-short]
           [a12 wchar]
           [a13 char]
           [a14 boolean]
           [a15 fixnum]
           [a16 iptr]
           [a17 uptr]
           [a18 void*]))
       (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
       (free-after a
         (ftype-set! A (a1) a 3.5)
         (ftype-set! A (a2) a -4.5)
         (ftype-set! A (a3) a -30000)
         (ftype-set! A (a4) a #xabcdef02)
         (ftype-set! A (a5) a -30001)
         (ftype-set! A (a6) a #xabcdef03)
         (ftype-set! A (a7) a -30002)
         (ftype-set! A (a8) a #xabcdef04)
         (ftype-set! A (a9) a #xabcdef05)
         (ftype-set! A (a10) a -30003)
         (ftype-set! A (a11) a #xab06)
         (ftype-set! A (a12) a #\a)
         (ftype-set! A (a13) a #\b)
         (ftype-set! A (a14) a 'hello)
         (ftype-set! A (a15) a (most-positive-fixnum))
         (ftype-set! A (a16) a -30004)
         (ftype-set! A (a17) a #xabcdef07)
         (ftype-set! A (a18) a 25000)
         (list
           (ftype-ref A (a1) a)
           (ftype-ref A (a2) a)
           (ftype-ref A (a3) a)
           (ftype-ref A (a4) a)
           (ftype-ref A (a5) a)
           (ftype-ref A (a6) a)
           (ftype-ref A (a7) a)
           (ftype-ref A (a8) a)
           (ftype-ref A (a9) a)
           (ftype-ref A (a10) a)
           (ftype-ref A (a11) a)
           (ftype-ref A (a12) a)
           (ftype-ref A (a13) a)
           (ftype-ref A (a14) a)
           (ftype-ref A (a15) a)
           (ftype-ref A (a16) a)
           (ftype-ref A (a17) a)
           (ftype-ref A (a18) a))))
    `(3.5
      -4.5
      -30000
      #xabcdef02
      -30001
      #xabcdef03
      -30002
      #xabcdef04
      #xabcdef05
      -30003
      #xab06
      #\a
      #\b
      #t
      ,(most-positive-fixnum)
      -30004
      #xabcdef07
      25000))

  (begin
    (define-ftype A
      (array 3
        (struct
          [a int]
          [b short])))
    (define x (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
    (do ([i 0 (fx+ i 1)])
        ((fx= i 3))
      (ftype-set! A (i a) x (expt 2 i))
      (ftype-set! A (i b) x (- 1 (expt 2 i))))
    #t)

  (eqv? (ftype-ref A (0 a) x) 1)
  (eqv? (ftype-ref A (0 b) x) 0)
  (eqv? (ftype-ref A (1 a) x) 2)
  (eqv? (ftype-ref A (1 b) x) -1)
  (eqv? (ftype-ref A (2 a) x) 4)
  (eqv? (ftype-ref A (2 b) x) -3)

  (error? ; invalid index
    (ftype-ref A (3 a) x))
  (error? ; invalid index
    (ftype-ref A (-1 a) x))
  (error? ; invalid index
    (ftype-ref A (x a) x))
  (error? ; invalid index
    (ftype-ref A (1.0 a) x))
  (error? ; invalid index
    (ftype-&ref A (3) x))
  (error? ; invalid index
    (ftype-&ref A (-1) x))
  (error? ; invalid index
    (ftype-&ref A (x) x))
  (error? ; invalid index
    (ftype-&ref A (1.0) x))
  (error? ; invalid index
    (ftype-&ref A (3 a) x))
  (error? ; invalid index
    (ftype-&ref A (-1 a) x))
  (error? ; invalid index
    (ftype-&ref A (x a) x))
  (error? ; invalid index
    (ftype-&ref A (1.0 a) x))
  (error? ; invalid index
    (ftype-set! A (3 a) x 0))
  (error? ; invalid index
    (ftype-set! A (-1 a) x 0))
  (error? ; invalid index
    (ftype-set! A (x a) x 0))
  (error? ; invalid index
    (ftype-set! A (1.0 a) x 0))
  (error? ; invalid value
    (ftype-set! A (1 a) x 3.2))
  (error? ; invalid index
    (ftype-set! A (1 a) x #\a))
  (error? ; invalid index
    (ftype-set! A (1 a) x (expt 2 1000)))
  (error? ; target cannot be referenced
    (ftype-ref A (1) x))
  (error? ; target cannot be assigned
    (ftype-set! A (1) x 0))

  (begin
    (fptr-free x)
    #t)

 ; ----------------

  (begin
    (define-ftype Q
      (struct
        [x integer-16]
        [y (array 100 integer-32)]))
    (define x (make-ftype-pointer Q (foreign-alloc (- (ftype-sizeof Q) (* (ftype-sizeof integer-32) (- 100 10))))))
    #t)
  (eqv? (ftype-sizeof Q) 404)
  (eqv? (ftype-pointer-address (ftype-&ref Q (y) (make-ftype-pointer Q 0))) 4)
  (begin
    (do ([i 0 (fx+ i 1)])
        ((fx= i 10))
      (ftype-set! Q (y i) x (+ (* i 3) 2)))
    #t)
  (equal?
    (map (lambda (i) (ftype-ref Q (y i) x)) (iota 10))
    (map (lambda (i) (+ (* i 3) 2)) (iota 10)))
  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype A (struct [x double]))
    (define-ftype B (struct [head int] [tail (* A)]))
    (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
    (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
    (ftype-set! B (tail) b a)
    (ftype-set! B (head) b 17)
    (ftype-set! A (x) a 3.25)
    #t)
  (equal?
    (ftype-pointer->sexpr a)
    '(struct [x 3.25]))
  (equal? 
    (ftype-pointer->sexpr b)
    '(struct [head 17] [tail (* (struct [x 3.25]))]))
  (error? ; not a scalar
    (ftype-ref B (tail *) b))
  (ftype-pointer? (ftype-ref B (tail) b))
  (begin
    (ftype-set! A (x) (ftype-ref B (tail) b) -5.5)
    #t)
  (eqv? (ftype-ref B (tail * x) b) -5.5)

  (begin
    (fptr-free a)
    (fptr-free b)
    #t)
 ; ----------------
  (begin
    (define-ftype Qlist
      (struct
        [head int]
        [tail (* Qlist)]))
    (define x (make-ftype-pointer Qlist (foreign-alloc (ftype-sizeof Qlist))))
    (ftype-set! Qlist (head) x 17)
    (ftype-set! Qlist (tail) x x)
    #t)
  (eqv? (ftype-ref Qlist (head) x) 17)
  (eqv? (ftype-ref Qlist (tail * head) x) 17)
  (eqv? (ftype-ref Qlist (tail * tail * tail * tail * head) x) 17)
  (equal?
    (ftype-pointer->sexpr x)
    '#0=(struct [head 17] [tail (* #0#)]))
  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype
      [Qfrob (struct
               [head int]
               [tail (* Qsnark)])]
      [Qsnark (struct
                [head int]
                [tail (* Qfrob)])])
    (define x (make-ftype-pointer Qfrob (foreign-alloc (ftype-sizeof Qfrob))))
    (ftype-set! Qfrob (head) x 17)
    (define y (make-ftype-pointer Qsnark (foreign-alloc (ftype-sizeof Qsnark))))
    (ftype-set! Qfrob (tail) x y)
    (ftype-set! Qfrob (tail * head) x -57)
    (ftype-set! Qfrob (tail * tail) x x)
    #t)
  (eqv? (ftype-ref Qfrob (head) x) 17)
  (eqv? (ftype-ref Qfrob (tail * head) x) -57)
  (eqv? (ftype-ref Qfrob (tail * tail * tail * tail * head) x) 17)
  (eqv? (ftype-ref Qfrob (tail * tail * tail * tail * tail * head) x) -57)
  (eqv? (ftype-ref Qsnark (head) (ftype-ref Qfrob (tail) x)) -57)
  (equal?
    (ftype-pointer->sexpr x)
    '#1=(struct
          [head 17]
          [tail (* (struct [head -57] [tail (* #1#)]))]))
  (begin
    (fptr-free x)
    (fptr-free y)
    #t)

 ; ----------------
  (error? ; invalid recursive or forward reference
    (define-ftype
      [Qfrob (struct
               [head int]
               [xtra Qfrob]
               [tail (* Qsnark)])]
      [Qsnark (struct
                [head int]
                [tail (* Qfrob)])]))
  (error? ; invalid recursive or forward reference
    (define-ftype
      [Qfrob (struct
               [head int]
               [xtra Qsnark]
               [tail (* Qsnark)])]
      [Qsnark (struct
                [head int]
                [tail (* Qfrob)])]))

 ; ----------------
  (begin
    (define-ftype
      [Qfrob (struct
               [head int]
               [tail (* Qsnark)])]
      [Qsnark (struct
                [head int]
                [xtra Qfrob]
                [tail (* Qfrob)])])
    (define x (make-ftype-pointer Qfrob (foreign-alloc (ftype-sizeof Qfrob))))
    (ftype-set! Qfrob (head) x 17)
    (define y (make-ftype-pointer Qsnark (foreign-alloc (ftype-sizeof Qsnark))))
    (ftype-set! Qfrob (tail) x y)
    (ftype-set! Qfrob (tail * head) x -57)
    (ftype-set! Qfrob (tail * tail) x x)
    (ftype-set! Qfrob (tail * xtra head) x 83)
    (ftype-set! Qfrob (tail * xtra tail) x (ftype-ref Qfrob (tail) x))
    #t)
  (eqv? (ftype-ref Qfrob (head) x) 17)
  (eqv? (ftype-ref Qfrob (tail * head) x) -57)
  (eqv? (ftype-ref Qfrob (tail * tail * tail * tail * head) x) 17)
  (eqv? (ftype-ref Qfrob (tail * tail * tail * tail * tail * head) x) -57)
  (eqv? (ftype-ref Qsnark (head) (ftype-ref Qfrob (tail) x)) -57)
  (eqv? (ftype-ref Qfrob (tail * xtra head) x) 83)
  (eqv? (ftype-ref Qfrob (tail * xtra tail * head) x) -57)
  (equal?
    (ftype-pointer-ftype x)
    '(struct
       [head int]
       [tail (* Qsnark)]))
  (equal?
    (ftype-pointer-ftype (ftype-ref Qfrob (tail) x))
    '(struct
       [head int]
       [xtra Qfrob]
       [tail (* Qfrob)]))
  (equal?
    (ftype-pointer->sexpr x)
    '#2=(struct
          [head 17]
          [tail (* #3=(struct
                        [head -57]
                        [xtra (struct [head 83] [tail (* #3#)])]
                        [tail (* #2#)]))]))
  (begin
    (fptr-free x)
    (fptr-free y)
    #t)

 ; ----------------
  (begin
    (define-ftype A (bits [x unsigned 3] [y unsigned 5]))
    (define-ftype B (* A))
    (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
    (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
    (ftype-set! B () b a)
    #t)

  (begin
    (ftype-set! A (x) a 3)
    (ftype-set! A (y) a 31)
    #t)

  (eqv? (ftype-ref A (x) a) 3)
  (eqv? (ftype-ref A (y) a) 31)
  (eqv? (ftype-ref B (* x) b) 3)
  (eqv? (ftype-ref B (* y) b) 31)

  (begin
    (ftype-set! A (x) a 6)
    (ftype-set! A (y) a 21)
    #t)

  (eqv? (ftype-ref A (x) a) 6)
  (eqv? (ftype-ref A (y) a) 21)
  (eqv? (ftype-ref B (* x) b) 6)
  (eqv? (ftype-ref B (* y) b) 21)

  (begin
    (fptr-free a)
    (fptr-free b)
    #t)

 ; ----------------
  (begin
    (define-ftype Q
      (struct
        [x integer-16]
        [y (array 0 iptr)]))
    (define qlen 17)
    (define q (make-ftype-pointer Q (foreign-alloc (+ (ftype-sizeof Q) (* qlen (ftype-sizeof iptr))))))
    (do ([i 0 (fx+ i 1)]) ((fx= i qlen)) (ftype-set! Q (y i) q (* i 7)))
    #t)

  (error? ; invalid index
    (ftype-ref Q (y -1) q))
  (error? ; invalid index
    (ftype-ref Q (y 3.2) q))
  (error? ; invalid index
    (ftype-ref Q (y (+ (most-positive-fixnum) 1)) q))
  (error? ; invalid index
    (ftype-set! Q (y -1) q 7))
  (error? ; invalid index
    (ftype-set! Q (y 3.2) q 7))
  (error? ; invalid index
    (ftype-set! Q (y (+ (most-positive-fixnum) 1)) q 7))
  (error? ; invalid index
    (ftype-&ref Q (y -1) q))
  (error? ; invalid index
    (ftype-&ref Q (y 3.2) q))
  (error? ; invalid index
    (ftype-&ref Q (y (+ (most-positive-fixnum) 1)) q))
  (error? ; invalid index
    (ftype-locked-incr! Q (y -1) q))
  (error? ; invalid index
    (ftype-locked-decr! Q (y 3.2) q))
  (error? ; invalid index
    (ftype-lock! Q (y (+ (most-positive-fixnum) 1)) q))
  (error? ; invalid index
    (ftype-spin-lock! Q (y (+ (most-positive-fixnum) 1)) q))
  (eqv? (ftype-ref Q (y 0) q) 0)
  (eqv? (ftype-ref Q (y 7) q) 49)
  (eqv? (ftype-ref Q (y 16) q) 112)

  (begin
    (fptr-free q)
    #t)

 ; ----------------
  (guard (c [(and (message-condition? c)
                  (equal? (condition-message c) "non-fixnum overall size for ftype"))
             #t])
    (eval
      '(meta-cond
         [(= (fixnum-width) 30)
          (define-ftype Q
            (struct
              [x integer-16]
              [y (array #xFFFFFFF integer-32)]))]
         [(= (fixnum-width) 61)
          (define-ftype Q
            (struct
              [x integer-16]
              [y (array #xFFFFFFFFFFFFFFF integer-32)]))]
         [else (errorf #f "unexpected fixnum-width")]))
    #t)

 ; ----------------
  (begin
    (define-syntax $dfvalerr
      (syntax-rules ()
        [(_ type)
         (let ()
           (define-ftype A (endian big type))
           (define x (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
           (guard (c [#t (fptr-free x) (raise c)])
             (ftype-set! A () x 'oops)))]))
    #t)

  (error? ($dfvalerr (* float)))
  (error? ($dfvalerr integer-8))
  (error? ($dfvalerr unsigned-8))
  (error? ($dfvalerr integer-16))
  (error? ($dfvalerr unsigned-16))
  (error? ($dfvalerr integer-32))
  (error? ($dfvalerr unsigned-32))
  (error? ($dfvalerr integer-64))
  (error? ($dfvalerr unsigned-64))
  (error? ($dfvalerr double-float))
  (error? ($dfvalerr single-float))
  (error? ($dfvalerr char))
  (error? ($dfvalerr wchar))
  (error? ($dfvalerr fixnum))
  (error? ($dfvalerr iptr))
  (error? ($dfvalerr uptr))
  (error? ($dfvalerr void*))
  (error? ($dfvalerr int))
  (error? ($dfvalerr unsigned))
  (error? ($dfvalerr unsigned-int))
  (error? ($dfvalerr short))
  (error? ($dfvalerr unsigned-short))
  (error? ($dfvalerr long))
  (error? ($dfvalerr unsigned-long))
  (error? ($dfvalerr long-long))
  (error? ($dfvalerr unsigned-long-long))
  (error? ($dfvalerr double))
  (error? ($dfvalerr float))

 ; ----------------
  (begin
    (define-syntax $dfvalerr
      (syntax-rules ()
        [(_ type)
         (let ()
           (define-ftype A (endian little type))
           (define x (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
           (guard (c [#t (fptr-free x) (raise c)])
             (ftype-set! A () x 'oops)))]))
    #t)

  (error? ($dfvalerr (* float)))
  (error? ($dfvalerr integer-8))
  (error? ($dfvalerr unsigned-8))
  (error? ($dfvalerr integer-16))
  (error? ($dfvalerr unsigned-16))
  (error? ($dfvalerr integer-32))
  (error? ($dfvalerr unsigned-32))
  (error? ($dfvalerr integer-64))
  (error? ($dfvalerr unsigned-64))
  (error? ($dfvalerr double-float))
  (error? ($dfvalerr single-float))
  (error? ($dfvalerr char))
  (error? ($dfvalerr wchar))
  (error? ($dfvalerr fixnum))
  (error? ($dfvalerr iptr))
  (error? ($dfvalerr uptr))
  (error? ($dfvalerr void*))
  (error? ($dfvalerr int))
  (error? ($dfvalerr unsigned))
  (error? ($dfvalerr unsigned-int))
  (error? ($dfvalerr short))
  (error? ($dfvalerr unsigned-short))
  (error? ($dfvalerr long))
  (error? ($dfvalerr unsigned-long))
  (error? ($dfvalerr long-long))
  (error? ($dfvalerr unsigned-long-long))
  (error? ($dfvalerr double))
  (error? ($dfvalerr float))

 ; ----------------
  (begin
    (define-syntax $dfvalerr
      (syntax-rules ()
        [(_ type)
         (begin
           (with-output-to-file "testfile.ss"
             (lambda ()
               (pretty-print
                 '(let ()
                    (define-ftype A type)
                    (define x (make-ftype-pointer A 0))
                    (ftype-set! A () x 'oops))))
             'replace)
           (load "testfile.ss"))]))
    #t)

  (error? ($dfvalerr (* float)))
  (error? ($dfvalerr integer-8))
  (error? ($dfvalerr unsigned-8))
  (error? ($dfvalerr integer-16))
  (error? ($dfvalerr unsigned-16))
  (error? ($dfvalerr integer-32))
  (error? ($dfvalerr unsigned-32))
  (error? ($dfvalerr integer-64))
  (error? ($dfvalerr unsigned-64))
  (error? ($dfvalerr double-float))
  (error? ($dfvalerr single-float))
  (error? ($dfvalerr char))
  (error? ($dfvalerr wchar))
  (error? ($dfvalerr fixnum))
  (error? ($dfvalerr iptr))
  (error? ($dfvalerr uptr))
  (error? ($dfvalerr void*))
  (error? ($dfvalerr int))
  (error? ($dfvalerr unsigned))
  (error? ($dfvalerr unsigned-int))
  (error? ($dfvalerr short))
  (error? ($dfvalerr unsigned-short))
  (error? ($dfvalerr long))
  (error? ($dfvalerr unsigned-long))
  (error? ($dfvalerr long-long))
  (error? ($dfvalerr unsigned-long-long))
  (error? ($dfvalerr double))
  (error? ($dfvalerr float))

 ; ----------------
  (error? ; invalid syntax
    (ftype-sizeof (struct [a int])))
  (error? ; invalid syntax
    (make-ftype-pointer (struct [a int]) 0))
  (error? ; invalid syntax
    (ftype-pointer? (struct [a int]) 0))
  (error? ; invalid syntax
    (ftype-&ref (struct [a int]) (a) x))
  (error? ; invalid syntax
    (ftype-ref (struct [a int]) (a) x))
  (error? ; invalid syntax
    (ftype-set! (struct [a int]) (a) x 0))

 ; ----------------
  (begin
    (define-ftype A (packed (struct [a char] [b int])))
    (define-ftype B (struct [a A] [b (* A)]))
    (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
    #t)

  (ftype-pointer? A (ftype-&ref B (a) b))
  (ftype-pointer? A (ftype-ref B (b) b))

  (begin
    (fptr-free b)
    #t)

 ; ----------------
 (begin
   ;; Bind a native built-in type as a normal ftype (note this fixes the endianness).
   (define-ftype integer-8 integer-8)
   (define-ftype float float)
   (define-ftype S (struct [i integer-8] [f float]))
   #t)
 (let ([int-fptr (make-ftype-pointer integer-8 (foreign-alloc (ftype-sizeof integer-8)))]
       [flo-fptr (make-ftype-pointer float (foreign-alloc (ftype-sizeof float)))]
       [s-fptr (make-ftype-pointer S (foreign-alloc (ftype-sizeof S)))])
   (ftype-set! integer-8 () int-fptr 42)
   (ftype-set! float () flo-fptr 7.125)
   (ftype-set! S (i) s-fptr 75)
   (ftype-set! S (f) s-fptr 8.25)
   (let ([a (ftype-ref integer-8 () int-fptr)]
         [b (ftype-ref float () flo-fptr)]
         [c (ftype-ref S (i) s-fptr)]
         [d (ftype-ref S (f) s-fptr)])
     (foreign-free (ftype-pointer-address int-fptr))
     (foreign-free (ftype-pointer-address flo-fptr))
     (foreign-free (ftype-pointer-address s-fptr))
     (equal? (list a b c d) (list 42 7.125 75 8.25))))

 (begin
   ;; Show that binding does not interfere with native types.
   (define-syntax unsigned-16 (make-compile-time-value "Non-interfering binding"))
   (let ([fptr (make-ftype-pointer unsigned-16 0)])
     (= (ftype-pointer-address fptr) 0)))
)

(mat ftype-pointer-address-optimizations
  (begin
    (define-ftype A (struct (x iptr)))
    (define-ftype B (struct (x uptr)))
    (define a1 (make-ftype-pointer A 0))
    (define a1-also (make-ftype-pointer A 0))
    (define a2 (make-ftype-pointer A (+ (most-positive-fixnum) 1)))
    (define a2-also (make-ftype-pointer A (+ (most-positive-fixnum) 1)))
    #t)

  (error? (ftype-pointer-null? '()))
  (error? (ftype-pointer=? "oops" a1))
  (error? (ftype-pointer=? a1 17))

  (ftype-pointer-null? a1)
  (= (ftype-pointer-address a1) 0)
  (r6rs:= (ftype-pointer-address a1) 0)
  (eqv? (ftype-pointer-address a1) 0)
  (equal? (ftype-pointer-address a1) 0)
  (= 0 (ftype-pointer-address a1))
  (r6rs:= 0 (ftype-pointer-address a1))
  (eqv? 0 (ftype-pointer-address a1))
  (equal? 0 (ftype-pointer-address a1))
  (not (< (ftype-pointer-address a1) 0))

  (not (ftype-pointer-null? a2))
  (not (= (ftype-pointer-address a2) 0))
  (not (r6rs:= (ftype-pointer-address a2) 0))
  (not (eqv? (ftype-pointer-address a2) 0))
  (not (equal? (ftype-pointer-address a2) 0))
  (not (= 0 (ftype-pointer-address a2)))
  (not (r6rs:= 0 (ftype-pointer-address a2)))
  (not (eqv? 0 (ftype-pointer-address a2)))
  (not (equal? 0 (ftype-pointer-address a2)))
  (not (< (ftype-pointer-address a2) 0))

  (ftype-pointer=? a1 a1-also)
  (= (ftype-pointer-address a1) (ftype-pointer-address a1-also))
  (r6rs:= (ftype-pointer-address a1) (ftype-pointer-address a1-also))
  (eqv? (ftype-pointer-address a1) (ftype-pointer-address a1-also))
  (equal? (ftype-pointer-address a1) (ftype-pointer-address a1-also))
  (ftype-pointer=? a2 a2-also)
  (= (ftype-pointer-address a2) (ftype-pointer-address a2-also))
  (r6rs:= (ftype-pointer-address a2) (ftype-pointer-address a2-also))
  (eqv? (ftype-pointer-address a2) (ftype-pointer-address a2-also))
  (equal? (ftype-pointer-address a2) (ftype-pointer-address a2-also))
  (not (ftype-pointer=? a1 a2))
  (not (= (ftype-pointer-address a2) (ftype-pointer-address a1)))
  (not (r6rs:= (ftype-pointer-address a2) (ftype-pointer-address a1)))
  (not (eqv? (ftype-pointer-address a2) (ftype-pointer-address a1)))
  (not (equal? (ftype-pointer-address a2) (ftype-pointer-address a1)))

  (begin
    (define $f1
      (lambda (a)
        (ftype-pointer-null? a)))
    (define $f2a
      (lambda (a)
        (#%= (#3%ftype-pointer-address a1) 0)))
    (define $f2b
      (lambda (a)
        (#%r6rs:= (#3%ftype-pointer-address a1) 0)))
    (define $f3
      (lambda (a)
        (#%eqv? (#3%ftype-pointer-address a) 0)))
    (define $f4
      (lambda (a)
        (#%equal? (#3%ftype-pointer-address a) 0)))
    (define $f5a
      (lambda (a)
        (#%= 0 (#3%ftype-pointer-address a))))
    (define $f5b
      (lambda (a)
        (#%r6rs:= 0 (#3%ftype-pointer-address a))))
    (define $f6
      (lambda (a)
        (#%eqv? 0 (#3%ftype-pointer-address a))))
    (define $f7
      (lambda (a)
        (#%equal? 0 (#3%ftype-pointer-address a))))
    (define $f8
      (lambda (a b)
        (ftype-pointer=? a b)))
    (define $f9a
      (lambda (a b)
        (#%= (#3%ftype-pointer-address b) (#3%ftype-pointer-address a))))
    (define $f9b
      (lambda (a b)
        (#%r6rs:= (#3%ftype-pointer-address b) (#3%ftype-pointer-address a))))
    (define $f10
      (lambda (a b)
        (#%eqv? (#3%ftype-pointer-address b) (#3%ftype-pointer-address a))))
    (define $f11
      (lambda (a b)
        (#%equal? (#3%ftype-pointer-address b) (#3%ftype-pointer-address a))))
    #t)

  ; check to make sure we don't allocate a bignum while checking
  (let ([s0 (statistics)])
    (do ([n 1000 (fx- n 1)])
        ((fx= n 0))
      ($f1 a1)
      ($f2a a1)
      ($f2b a1)
      ($f3 a1)
      ($f4 a1)
      ($f5a a1)
      ($f5b a1)
      ($f6 a1)
      ($f7 a1))
    (let ([s1 (statistics)])
      (<= (- (sstats-bytes s1) (sstats-bytes s0)) 1000)))

  (or (eq? (current-eval) interpret)
      (eq? (compile-profile) 'source)
      (let ([s0 (statistics)])
        (do ([n 1000 (fx- n 1)])
          ((fx= n 0))
          ($f1 a2)
          ($f2a a2)
          ($f2b a2)
          ($f3 a2)
          ($f4 a2)
          ($f5a a2)
          ($f5b a2)
          ($f6 a2)
          ($f7 a2))
        (let ([s1 (statistics)])
          (<= (- (sstats-bytes s1) (sstats-bytes s0)) 1000))))

  (let ([s0 (statistics)])
    (do ([n 1000 (fx- n 1)])
      ((fx= n 0))
      ($f8 a1-also a1)
      ($f9a a1-also a1)
      ($f9b a1-also a1)
      ($f10 a1-also a1)
      ($f11 a1-also a1))
    (let ([s1 (statistics)])
      (<= (- (sstats-bytes s1) (sstats-bytes s0)) 1000)))

  (or (eq? (current-eval) interpret)
      (eq? (compile-profile) 'source)
      (let ([s0 (statistics)])
        (do ([n 1000 (fx- n 1)])
          ((fx= n 0))
          ($f8 a1 a2)
          ($f9a a1 a2)
          ($f9b a1 a2)
          ($f10 a1 a2)
          ($f11 a1 a2))
        (let ([s1 (statistics)])
          (<= (- (sstats-bytes s1) (sstats-bytes s0)) 1000))))

  (or (eq? (current-eval) interpret)
      (eq? (compile-profile) 'source)
      (let ([s0 (statistics)])
        (do ([n 1000 (fx- n 1)])
          ((fx= n 0))
          ($f8 a2-also a2)
          ($f9a a2-also a2)
          ($f9b a2-also a2)
          ($f10 a2-also a2)
          ($f11 a2-also a2))
        (let ([s1 (statistics)])
          (<= (- (sstats-bytes s1) (sstats-bytes s0)) 1000))))

  (begin
    (define x (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
    #t)
  (begin
    (define $not-much-alloc?
      (lambda (require-cp0? p)
        (or (eq? (current-eval) interpret)
            (#%$suppress-primitive-inlining)
            (eq? (compile-profile) 'source)
            (not (= (optimize-level) 3))
            (and require-cp0? (not (enable-cp0)))
            (let ([s0 (statistics)])
              (and (let f ([n 1000])
                     (or (fx= n 0)
                         (begin
                           (let ([x (p n)]) (unless (eq? x #t) (errorf #f "p returned non-#t value ~s for n=~s" x n)))
                           (f (fx- n 1)))))
                   (let ([s1 (statistics)])
                     (<= (- (sstats-bytes s1) (sstats-bytes s0)) 1000)))))))
    #t)

  ; might should also check ftype-&ref, ftype-locked-decr!, ftype-init-lock,
  ; ftype-lock!, ftype-spin-lock!, and ftype-unlock!, plus more flavors of
  ; ftype-ref (including bit-field references) and all the others.
  ($not-much-alloc? #f
    (lambda (n)
      (ftype-set! A (x) x (fx+ n 10))
      (and (fx= (ftype-ref B (x) (make-ftype-pointer B (ftype-pointer-address x))) (fx+ n 10))
           (begin
             (ftype-set! B (x) (make-ftype-pointer B (ftype-pointer-address x)) (fx+ n 19))
             (and (fx= (ftype-ref A (x) x) (fx+ n 19))
                  (begin
                    (ftype-locked-incr! B (x) (make-ftype-pointer B (ftype-pointer-address x)))
                    (fx= (ftype-ref A (x) x) (fx+ n 20))))))))

  (begin
    (define $ftp1 (make-ftype-pointer A 0))
    (define $ftp2 (make-ftype-pointer A (+ (most-positive-fixnum) 1)))
    ; this should cost the same at o=3 whether address is a fixnum or bignum
    (define $mkftp (lambda (x) (make-ftype-pointer B (ftype-pointer-address x))))
    #t)

  (or (eq? (current-eval) interpret)
      (#%$suppress-primitive-inlining)
      (eq? (compile-profile) 'source)
      (not (= (optimize-level) 3))
      (<=
        -100
        (- (let ([s0 (statistics)])
             (ftype-pointer?
               (do ([n 100 (fx- n 1)] [x $ftp1 ($mkftp x)])
                 ((fx= n 0) x)))
             (let ([s1 (statistics)])
               (- (sstats-bytes s1) (sstats-bytes s0))))
           (let ([s0 (statistics)])
             (ftype-pointer?
               (do ([n 100 (fx- n 1)] [x $ftp2 ($mkftp x)])
                 ((fx= n 0) x)))
             (let ([s1 (statistics)])
               (- (sstats-bytes s1) (sstats-bytes s0)))))
        100))

  (begin
    (fptr-free x)
    #t)

  ($not-much-alloc? #t
    (let ()
      (define-ftype A (struct [x (array 10 int)] [y (bits [y1 unsigned 3] [y2 unsigned 12] [y3 signed 17])]))
      (define-ftype B (* A))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
      (lambda (n)
        (and
          (eqv? (ftype-set! B () b a) (void))
          (eqv? (ftype-set! A (x 3) a 17) (void))
          (eqv? (ftype-set! A (y y1) a 5) (void))
          (eqv? (ftype-set! A (y y2) a 2795) (void))
          (eqv? (ftype-set! A (y y3) a -9493) (void))
          (eqv? (ftype-ref A (x 3) (ftype-ref B () b)) 17)
          (eqv? (ftype-set! A (x 3) (ftype-ref B () b) 37) (void))
          (eqv? (ftype-ref A (x 3) (ftype-ref B () b)) 37)
          (eqv? (ftype-ref A (y y1) (ftype-ref B () b)) 5)
          (eqv? (ftype-ref A (y y2) (ftype-ref B () b)) 2795)
          (eqv? (ftype-ref A (y y3) (ftype-ref B () b)) -9493)
          (eqv? (ftype-set! A (y y1) (ftype-ref B () b) 6) (void))
          (eqv? (ftype-set! A (y y2) (ftype-ref B () b) 1037) (void))
          (eqv? (ftype-set! A (y y3) (ftype-ref B () b) 9493) (void))
          (eqv? (ftype-ref A (y y1) (ftype-ref B () b)) 6)
          (eqv? (ftype-ref A (y y2) (ftype-ref B () b)) 1037)
          (eqv? (ftype-ref A (y y3) (ftype-ref B () b)) 9493)))))

  ($not-much-alloc? #t
    (let ()
      (define-ftype A (struct [x (array 10 int)] [y (bits [y1 unsigned 3] [y2 unsigned 12] [y3 signed 17])]))
      (define-ftype B (* A))
      (define-ftype BB (struct [b1 char] [b2 B]))
      (define-ftype BBB (* BB))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define bb (make-ftype-pointer BB (foreign-alloc (ftype-sizeof BB))))
      (define bbb (make-ftype-pointer BBB (foreign-alloc (ftype-sizeof BBB))))
      (lambda (n)
        (and
          (eqv? (ftype-set! BB (b2) bb a) (void))
          (eqv? (ftype-set! BBB () bbb bb) (void))
          (eqv? (ftype-set! A (x 3) a 17) (void))
          (eqv? (ftype-set! A (y y1) a 5) (void))
          (eqv? (ftype-set! A (y y2) a 2795) (void))
          (eqv? (ftype-set! A (y y3) a -9493) (void))
          (eqv? (ftype-ref A (x 3) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) 17)
          (eqv? (ftype-set! A (x 3) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb))) 37) (void))
          (eqv? (ftype-ref A (x 3) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) 37)
          (eqv? (ftype-ref A (y y1) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) 5)
          (eqv? (ftype-ref A (y y2) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) 2795)
          (eqv? (ftype-ref A (y y3) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) -9493)
          (eqv? (ftype-set! A (y y1) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb))) 6) (void))
          (eqv? (ftype-set! A (y y2) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb))) 1037) (void))
          (eqv? (ftype-set! A (y y3) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb))) 9493) (void))
          (eqv? (ftype-ref A (y y1) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) 6)
          (eqv? (ftype-ref A (y y2) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) 1037)
          (eqv? (ftype-ref A (y y3) (ftype-ref B () (ftype-&ref BB (b2) (ftype-ref BBB () bbb)))) 9493)))))

  ($not-much-alloc? #t
    (let ()
      (define-ftype A (struct [x (array 10 int)] [y (bits [y1 unsigned 3] [y2 unsigned 12] [y3 signed 17])]))
      (define-ftype C (struct [c1 int] [c2 A]))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define c (make-ftype-pointer C (foreign-alloc (ftype-sizeof C))))
      (lambda (n)
        (and
          (ftype-set! C (c2 x 7) c 53)
          (eqv? (ftype-ref A (x 7) (ftype-&ref C (c2) c)) 53)
          (eqv? (ftype-set! A (x 7) (ftype-&ref C (c2) c) 71) (void))
          (eqv? (ftype-ref A (x 7) (ftype-&ref C (c2) c)) 71)))))

  ($not-much-alloc? #t
    (let ()
      (define-ftype A (struct [x (array 10 int)] [y (bits [y1 unsigned 3] [y2 unsigned 12] [y3 signed 17])]))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define a-addr (ftype-pointer-address a))
      (lambda (n)
        (and
          (eqv? (ftype-set! A (x 3) (make-ftype-pointer A (ftype-pointer-address a)) n) (void))
          (eqv? (ftype-ref A (x 3) (make-ftype-pointer A (ftype-pointer-address a))) n)
          (eqv? (ftype-set! A (x 3) (make-ftype-pointer A (ftype-pointer-address a)) (- n 3)) (void))
          (eqv? (ftype-ref A (x 3) (make-ftype-pointer A a-addr)) (- n 3))))))
    
  ($not-much-alloc? #t
    (let ()
      (define-ftype A iptr)
      (define-ftype B (* A))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
      (ftype-set! A () a 0)
      (ftype-set! B () b a)
      (lambda (n)
        (and
          (not (ftype-locked-incr! A () (ftype-ref B () b)))
          (ftype-locked-decr! A () (ftype-ref B () b))))))

  ($not-much-alloc? #t
    (let ()
      (define-ftype A iptr)
      (define-ftype B (* A))
      (define-ftype BB (* B))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define b (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
      (define bb (make-ftype-pointer BB (foreign-alloc (ftype-sizeof BB))))
      (ftype-set! A () a 0)
      (ftype-set! B () b a)
      (ftype-set! BB () bb b)
      (lambda (n)
        (and
          (eq? (ftype-spin-lock! A () (ftype-ref B () (ftype-ref BB () bb))) (void))
          (eq? (ftype-unlock! A () (ftype-ref B () (ftype-ref BB () bb))) (void))))))
  )

(mat ftype-odd
  (begin
    (define-ftype O
      (struct
        [i (struct
             [i24 integer-24]
             [i40 integer-40]
             [i48 integer-48]
             [i56 integer-56])]
        [u (struct
             [u56 unsigned-56]
             [u48 unsigned-48]
             [u40 unsigned-40]
             [u24 unsigned-24])]))
    #t)
    
  (equal?
    (let ([x (make-ftype-pointer O 0)])
      (list
        (ftype-sizeof O)
        (ftype-pointer-address (ftype-&ref O (i i24) x))
        (ftype-pointer-address (ftype-&ref O (i i40) x))
        (ftype-pointer-address (ftype-&ref O (i i48) x))
        (ftype-pointer-address (ftype-&ref O (i i56) x))
        (ftype-pointer-address (ftype-&ref O (u u56) x))
        (ftype-pointer-address (ftype-&ref O (u u48) x))
        (ftype-pointer-address (ftype-&ref O (u u40) x))
        (ftype-pointer-address (ftype-&ref O (u u24) x))))
    '(44 0 3 8 14 22 30 36 41))

  (begin
    (define o (make-ftype-pointer O (foreign-alloc (ftype-sizeof O))))
    #t)

  (begin
    (ftype-set! O (i i24) o 0)
    (ftype-set! O (i i40) o 0)
    (ftype-set! O (i i48) o 0)
    (ftype-set! O (i i56) o 0)
    (ftype-set! O (u u24) o 0)
    (ftype-set! O (u u40) o 0)
    (ftype-set! O (u u48) o 0)
    (ftype-set! O (u u56) o 0)
    (equal?
      (list 
        (ftype-ref O (i i24) o)
        (ftype-ref O (i i40) o)
        (ftype-ref O (i i48) o)
        (ftype-ref O (i i56) o)
        (ftype-ref O (u u24) o)
        (ftype-ref O (u u40) o)
        (ftype-ref O (u u48) o)
        (ftype-ref O (u u56) o))
      '(0 0 0 0 0 0 0 0)))

  (let ([n24 (- (ash 1 24) 1)]
        [n40  (- (ash 1 40) 1)]
        [n48  (- (ash 1 48) 1)]
        [n56  (- (ash 1 56) 1)])
    (ftype-set! O (i i24) o -1)
    (ftype-set! O (i i40) o -1)
    (ftype-set! O (i i48) o -1)
    (ftype-set! O (i i56) o -1)
    (ftype-set! O (u u24) o -1)
    (ftype-set! O (u u40) o -1)
    (ftype-set! O (u u48) o -1)
    (ftype-set! O (u u56) o -1)
    (equal?
      (list 
        (ftype-ref O (i i24) o)
        (ftype-ref O (i i40) o)
        (ftype-ref O (i i48) o)
        (ftype-ref O (i i56) o)
        (ftype-ref O (u u24) o)
        (ftype-ref O (u u40) o)
        (ftype-ref O (u u48) o)
        (ftype-ref O (u u56) o))
      (list -1 -1 -1 -1 n24 n40 n48 n56)))

  (let ([n24 (- (ash 1 24) 1)]
        [n40  (- (ash 1 40) 1)]
        [n48  (- (ash 1 48) 1)]
        [n56  (- (ash 1 56) 1)])
    (ftype-set! O (i i24) o n24)
    (ftype-set! O (i i40) o n40)
    (ftype-set! O (i i48) o n48)
    (ftype-set! O (i i56) o n56)
    (ftype-set! O (u u24) o n24)
    (ftype-set! O (u u40) o n40)
    (ftype-set! O (u u48) o n48)
    (ftype-set! O (u u56) o n56)
    (equal?
      (list 
        (ftype-ref O (i i24) o)
        (ftype-ref O (i i40) o)
        (ftype-ref O (i i48) o)
        (ftype-ref O (i i56) o)
        (ftype-ref O (u u24) o)
        (ftype-ref O (u u40) o)
        (ftype-ref O (u u48) o)
        (ftype-ref O (u u56) o))
      (list -1 -1 -1 -1 n24 n40 n48 n56)))

  (let ([n24 (- (ash 1 23))]
        [n40  (- (ash 1 39))]
        [n48  (- (ash 1 47))]
        [n56  (- (ash 1 55))])
    (ftype-set! O (i i24) o n24)
    (ftype-set! O (i i40) o n40)
    (ftype-set! O (i i48) o n48)
    (ftype-set! O (i i56) o n56)
    (ftype-set! O (u u24) o n24)
    (ftype-set! O (u u40) o n40)
    (ftype-set! O (u u48) o n48)
    (ftype-set! O (u u56) o n56)
    (equal?
      (list 
        (ftype-ref O (i i24) o)
        (ftype-ref O (i i40) o)
        (ftype-ref O (i i48) o)
        (ftype-ref O (i i56) o)
        (ftype-ref O (u u24) o)
        (ftype-ref O (u u40) o)
        (ftype-ref O (u u48) o)
        (ftype-ref O (u u56) o))
      (list n24 n40 n48 n56 (- n24) (- n40) (- n48) (- n56))))

  (equal?
    (ftype-pointer->sexpr o)
    '(struct
       [i (struct
            [i24 #x-800000]
            [i40 #x-8000000000]
            [i48 #x-800000000000]
            [i56 #x-80000000000000])]
       [u (struct
            [u56 #x80000000000000]
            [u48 #x800000000000]
            [u40 #x8000000000]
            [u24 #x800000])]))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([i24 (- (random (ash 1 24)) (ash 1 23))]
          [i40 (- (random (ash 1 40)) (ash 1 39))]
          [i48 (- (random (ash 1 48)) (ash 1 47))]
          [i56 (- (random (ash 1 56)) (ash 1 55))]
          [u24 (- (random (ash #b11 23)) (ash 1 23))]
          [u40 (- (random (ash #b11 39)) (ash 1 39))]
          [u48 (- (random (ash #b11 47)) (ash 1 47))]
          [u56 (- (random (ash #b11 55)) (ash 1 55))])
      (ftype-set! O (i i24) o i24)
      (ftype-set! O (i i40) o i40)
      (ftype-set! O (i i48) o i48)
      (ftype-set! O (i i56) o i56)
      (ftype-set! O (u u24) o u24)
      (ftype-set! O (u u40) o u40)
      (ftype-set! O (u u48) o u48)
      (ftype-set! O (u u56) o u56)
      (and
        (= (ftype-ref O (i i24) o) i24)
        (= (ftype-ref O (i i40) o) i40)
        (= (ftype-ref O (i i48) o) i48)
        (= (ftype-ref O (i i56) o) i56)
        (= (ftype-ref O (u u24) o) u24)
        (= (ftype-ref O (u u40) o) u40)
        (= (ftype-ref O (u u48) o) u48)
        (= (ftype-ref O (u u56) o) u56))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([i24 (- (random (ash 1 24)) (ash 1 23))]
          [i40 (- (random (ash 1 40)) (ash 1 39))]
          [i48 (- (random (ash 1 48)) (ash 1 47))]
          [i56 (- (random (ash 1 56)) (ash 1 55))]
          [u24 (- (random (ash #b11 23)) (ash 1 23))]
          [u40 (- (random (ash #b11 39)) (ash 1 39))]
          [u48 (- (random (ash #b11 47)) (ash 1 47))]
          [u56 (- (random (ash #b11 55)) (ash 1 55))])
      (ftype-set! O (u u56) o u56)
      (ftype-set! O (u u48) o u48)
      (ftype-set! O (u u40) o u40)
      (ftype-set! O (u u24) o u24)
      (ftype-set! O (i i56) o i56)
      (ftype-set! O (i i48) o i48)
      (ftype-set! O (i i40) o i40)
      (ftype-set! O (i i24) o i24)
      (and
        (= (ftype-ref O (i i24) o) i24)
        (= (ftype-ref O (i i40) o) i40)
        (= (ftype-ref O (i i48) o) i48)
        (= (ftype-ref O (i i56) o) i56)
        (= (ftype-ref O (u u24) o) u24)
        (= (ftype-ref O (u u40) o) u40)
        (= (ftype-ref O (u u48) o) u48)
        (= (ftype-ref O (u u56) o) u56))))

  (begin
    (fptr-free o)
    #t)

 ; ----------------

  (begin
    (define-ftype O
      (packed
        ; NB: tests with this version will cause unaligned access errors on
        ; NB: machines that don't support unalinged accesses
        (struct
          [i (struct
               [i24 integer-24]
               [i40 integer-40]
               [i48 integer-48]
               [i56 integer-56])]
          [u (struct
               [u56 unsigned-56]
               [u48 unsigned-48]
               [u40 unsigned-40]
               [u24 unsigned-24])])))
    #t)
    
  (equal?
    (let ([x (make-ftype-pointer O 0)])
      (list
        (ftype-sizeof O)
        (ftype-pointer-address (ftype-&ref O (i i24) x))
        (ftype-pointer-address (ftype-&ref O (i i40) x))
        (ftype-pointer-address (ftype-&ref O (i i48) x))
        (ftype-pointer-address (ftype-&ref O (i i56) x))
        (ftype-pointer-address (ftype-&ref O (u u56) x))
        (ftype-pointer-address (ftype-&ref O (u u48) x))
        (ftype-pointer-address (ftype-&ref O (u u40) x))
        (ftype-pointer-address (ftype-&ref O (u u24) x))))
    '(42 0 3 8 14 21 28 34 39))

  (begin
    (define o (make-ftype-pointer O (foreign-alloc (ftype-sizeof O))))
    #t)

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([i24 (- (random (ash 1 24)) (ash 1 23))]
          [i40 (- (random (ash 1 40)) (ash 1 39))]
          [i48 (- (random (ash 1 48)) (ash 1 47))]
          [i56 (- (random (ash 1 56)) (ash 1 55))]
          [u24 (- (random (ash #b11 23)) (ash 1 23))]
          [u40 (- (random (ash #b11 39)) (ash 1 39))]
          [u48 (- (random (ash #b11 47)) (ash 1 47))]
          [u56 (- (random (ash #b11 55)) (ash 1 55))])
      (ftype-set! O (i i24) o i24)
      (ftype-set! O (i i40) o i40)
      (ftype-set! O (i i48) o i48)
      (ftype-set! O (i i56) o i56)
      (ftype-set! O (u u24) o u24)
      (ftype-set! O (u u40) o u40)
      (ftype-set! O (u u48) o u48)
      (ftype-set! O (u u56) o u56)
      (and
        (= (ftype-ref O (i i24) o) i24)
        (= (ftype-ref O (i i40) o) i40)
        (= (ftype-ref O (i i48) o) i48)
        (= (ftype-ref O (i i56) o) i56)
        (= (ftype-ref O (u u24) o) u24)
        (= (ftype-ref O (u u40) o) u40)
        (= (ftype-ref O (u u48) o) u48)
        (= (ftype-ref O (u u56) o) u56))))

  (do ([i 1000 (fx- i 1)])
      ((fx= i 0) #t)
    (let ([i24 (- (random (ash 1 24)) (ash 1 23))]
          [i40 (- (random (ash 1 40)) (ash 1 39))]
          [i48 (- (random (ash 1 48)) (ash 1 47))]
          [i56 (- (random (ash 1 56)) (ash 1 55))]
          [u24 (- (random (ash #b11 23)) (ash 1 23))]
          [u40 (- (random (ash #b11 39)) (ash 1 39))]
          [u48 (- (random (ash #b11 47)) (ash 1 47))]
          [u56 (- (random (ash #b11 55)) (ash 1 55))])
      (ftype-set! O (u u56) o u56)
      (ftype-set! O (u u48) o u48)
      (ftype-set! O (u u40) o u40)
      (ftype-set! O (u u24) o u24)
      (ftype-set! O (i i56) o i56)
      (ftype-set! O (i i48) o i48)
      (ftype-set! O (i i40) o i40)
      (ftype-set! O (i i24) o i24)
      (and
        (= (ftype-ref O (i i24) o) i24)
        (= (ftype-ref O (i i40) o) i40)
        (= (ftype-ref O (i i48) o) i48)
        (= (ftype-ref O (i i56) o) i56)
        (= (ftype-ref O (u u24) o) u24)
        (= (ftype-ref O (u u40) o) u40)
        (= (ftype-ref O (u u48) o) u48)
        (= (ftype-ref O (u u56) o) u56))))

  (begin
    (fptr-free o)
    #t)
)

(mat ftype-indexing
  (begin
    (define-ftype pdouble (* double))
    (define ftype-indexing-test
      (lambda (init-array!)
        (define ls '(2.17 3.14 1.85 10.75 18.32))
        (equal?
          (let ([doubles (make-ftype-pointer double (foreign-alloc (* (ftype-sizeof double) 5)))]
                [pdoubles (make-ftype-pointer pdouble (foreign-alloc (ftype-sizeof pdouble)))])
            (ftype-set! pdouble () pdoubles doubles)
            (init-array! doubles ls)
            (let ([v (list
                       (ftype-ref double () doubles)
                       (ftype-ref double () doubles *)
                       (ftype-ref double () doubles 0)
                       (ftype-ref double () doubles 1)
                       (ftype-ref double () doubles 2)
                       (ftype-ref double () doubles 3)
                       (ftype-ref double () doubles 4)
                       (ftype-ref pdouble (*) pdoubles)
                       (ftype-ref pdouble (0) pdoubles)
                       (ftype-ref pdouble (1) pdoubles)
                       (ftype-ref pdouble (2) pdoubles)
                       (ftype-ref pdouble (3) pdoubles)
                       (ftype-ref pdouble (4) pdoubles))])
              (foreign-free (ftype-pointer-address doubles))
              (foreign-free (ftype-pointer-address pdoubles))
              v))
          `(,(car ls) ,(car ls) ,@ls ,(car ls) ,@ls))))
    #t)

  (ftype-indexing-test
    (lambda (d ls)
      (unless (null? ls)
        (let f ([dbl (car ls)] [ls (cdr ls)] [d d])
          (ftype-set! double () d dbl)
          (unless (null? ls)
            (f (car ls) (cdr ls) 
              (make-ftype-pointer double
                (+ (ftype-sizeof double)
                   (ftype-pointer-address d)))))))))
  (ftype-indexing-test
    (lambda (d ls)
      (unless (null? ls)
        (let f ([dbl (car ls)] [ls (cdr ls)] [idx 0])
          (ftype-set! double () d idx dbl)
          (unless (null? ls)
            (f (car ls) (cdr ls) (fx+ idx 1)))))))
  (ftype-indexing-test
    (lambda (d ls)
      (unless (null? ls)
        (let f ([dbl (car ls)] [ls (cdr ls)] [idx 0])
          (ftype-set! double () (ftype-&ref double () d idx) * dbl)
          (unless (null? ls)
            (f (car ls) (cdr ls) (fx+ idx 1)))))))
  (ftype-indexing-test
    (lambda (d ls)
      (unless (null? ls)
        (let ([pdbl (make-ftype-pointer pdouble (foreign-alloc (ftype-sizeof pdouble)))])
          (ftype-set! pdouble () pdbl (ftype-&ref double () d *))
          (let f ([dbl (car ls)] [ls (cdr ls)] [idx 0])
            (ftype-set! pdouble (idx) pdbl * dbl)
            (unless (null? ls)
              (f (car ls) (cdr ls) (fx+ idx 1))))))))
  (ftype-indexing-test
    (lambda (d ls)
      (unless (null? ls)
        (let ([pdbl (make-ftype-pointer pdouble (foreign-alloc (ftype-sizeof pdouble)))])
          (ftype-set! pdouble () pdbl (ftype-&ref double () d (length ls)))
          (let ([ls (reverse ls)])
            (let f ([dbl (car ls)] [ls (cdr ls)] [idx 0])
              (ftype-set! pdouble ((- -1 idx)) pdbl * dbl)
              (unless (null? ls)
                (f (car ls) (cdr ls) (fx+ idx 1)))))))))

  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double 0)])
      (ftype-&ref double () doubles 4.5)))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double 0)])
      (ftype-&ref double () doubles (most-positive-fixnum))))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double (foreign-alloc (* (ftype-sizeof double) 5)))]
          [pdoubles (make-ftype-pointer pdouble (foreign-alloc (ftype-sizeof pdouble)))])
      (ftype-set! pdouble () pdoubles doubles)
      (guard (c [#t (foreign-free (ftype-pointer-address doubles))
                    (foreign-free (ftype-pointer-address pdoubles))
                    (raise c)])
        (pretty-print (ftype-&ref pdouble ('a) pdoubles)))))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double 0)])
      (ftype-ref double () doubles 4.5)))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double 0)])
      (ftype-ref double () doubles (most-positive-fixnum))))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double (foreign-alloc (* (ftype-sizeof double) 5)))]
          [pdoubles (make-ftype-pointer pdouble (foreign-alloc (ftype-sizeof pdouble)))])
      (ftype-set! pdouble () pdoubles doubles)
      (guard (c [#t (foreign-free (ftype-pointer-address doubles))
                    (foreign-free (ftype-pointer-address pdoubles))
                    (raise c)])
        (pretty-print (ftype-ref pdouble ('a) pdoubles)))))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double 0)])
      (ftype-set! double () doubles 4.5 7.0)))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double 0)])
      (ftype-set! double () doubles (most-positive-fixnum) 7.0)))
  (error? ; invalid index
    (let ([doubles (make-ftype-pointer double (foreign-alloc (* (ftype-sizeof double) 5)))]
          [pdoubles (make-ftype-pointer pdouble (foreign-alloc (ftype-sizeof pdouble)))])
      (ftype-set! pdouble () pdoubles doubles)
      (guard (c [#t (foreign-free (ftype-pointer-address doubles))
                    (foreign-free (ftype-pointer-address pdoubles))
                    (raise c)])
        (pretty-print (ftype-set! pdouble ('a) pdoubles 7.0)))))

  (begin
    (define-ftype A (struct [x int] [y double]))
    (define-ftype pA (* A))
    (define ftype-indexing-test
      (lambda (init-array!)
        (define int* '(2 3 4 -5 -6))
        (define dbl* '(2.0 3.0 4.0 -5.0 -6.0))
        (let ([array (make-ftype-pointer A (foreign-alloc (* (ftype-sizeof A) (length int*))))]
              [parray (make-ftype-pointer pA (foreign-alloc (ftype-sizeof pA)))])
          (ftype-set! pA () parray array)
          (init-array! array int* dbl*)
          (let ([v (and (eqv? (ftype-ref A (x) array) (car int*))
                        (eqv? (ftype-ref A (y) array) (car dbl*))
                        (eqv? (ftype-ref A (x) array *) (car int*))
                        (eqv? (ftype-ref A (y) array *) (car dbl*))
                        (andmap
                          (lambda (int dbl i)
                            (and
                              (eqv? (ftype-ref A (x) array i) int)
                              (eqv? (ftype-ref A (y) array i) dbl)))
                          int* dbl* (enumerate int*))
                        (eqv? (ftype-ref pA (* x) parray) (car int*))
                        (eqv? (ftype-ref pA (* y) parray) (car dbl*))
                        (andmap
                          (lambda (int dbl i)
                            (and
                              (eqv? (ftype-ref pA (i x) parray) int)
                              (eqv? (ftype-ref pA (i y) parray) dbl)))
                          int* dbl* (enumerate int*)))])
            (foreign-free (ftype-pointer-address array))
            (foreign-free (ftype-pointer-address parray))
            v))))
    #t)

  (ftype-indexing-test
    (lambda (array int* dbl*)
      (unless (null? int*)
        (for-each
          (lambda (int dbl i)
            (ftype-set! A (x)
              (make-ftype-pointer A
                (+ (ftype-pointer-address array)
                   (* (ftype-sizeof A) i)))
              int)
            (ftype-set! A (y)
              (make-ftype-pointer A
                (+ (ftype-pointer-address array)
                   (* (ftype-sizeof A) i)))
              dbl))
          int* dbl* (enumerate int*)))))
  (ftype-indexing-test
    (lambda (array int* dbl*)
      (unless (null? int*)
        (for-each
          (lambda (int dbl i)
            (ftype-set! A (x) array i int)
            (ftype-set! A (y) array i dbl))
          int* dbl* (enumerate int*)))))

  ; test for source info attached to index errors
  ; ...first with invalid value for optional index subform
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A int)
            (define (foo x i) (ftype-&ref A () x i))
            (foo (make-ftype-pointer A 0) 'q))))
      'replace)
    #t)
  (error? ; invalid index q w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A int)
            (define (foo x i) (ftype-ref A () x i))
            (foo (make-ftype-pointer A 0) 'q))))
      'replace)
    #t)
  (error? ; invalid index q w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A int)
            (define (foo x i) (ftype-set! A () x i 55))
            (foo (make-ftype-pointer A 0) 'q))))
      'replace)
    #t)
  (error? ; invalid index q w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A uptr)
            (define (foo x i) (ftype-locked-incr! A () x i))
            (foo (make-ftype-pointer A 0) 'q))))
      'replace)
    #t)
  (error? ; invalid index q w/source info
    (load "testfile.ss"))

  ; now with invalid array accessor
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A (array 17 int))
            (define (foo x i) (ftype-&ref A (i) x))
            (foo (make-ftype-pointer A 0) 25))))
      'replace)
    #t)
  (error? ; invalid index 25 w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A (array 17 int))
            (define (foo x i) (ftype-ref A (i) x))
            (foo (make-ftype-pointer A 0) 25))))
      'replace)
    #t)
  (error? ; invalid index 25 w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A (array 17 int))
            (define (foo x i) (ftype-set! A (i) x 55))
            (foo (make-ftype-pointer A 0) 25))))
      'replace)
    #t)
  (error? ; invalid index 25 w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A (array 17 uptr))
            (define (foo x i) (ftype-locked-incr! A (i) x))
            (foo (make-ftype-pointer A 0) 25))))
      'replace)
    #t)
  (error? ; invalid index 25 w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A (array 17 int))
            (eval '(define (foo x i) (ftype-&ref A (i) x)))
            (foo (make-ftype-pointer A 0) 25))))
      'replace)
    #t)
  (error? ; invalid index 25 w/o source info
    (load "testfile.ss"))

  ; test for source info attached to fptr errors
  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A int)
            (define (foo x) (ftype-&ref A () x))
            (foo (make-ftype-pointer double 0)))))
      'replace)
    #t)
  (error? ; ftype mismatch w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A int)
            (define (foo x) (ftype-ref A () x))
            (foo 17))))
      'replace)
    #t)
  (error? ; 17 is not an fptr w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A int)
            (define (foo x) (ftype-set! A () x 55))
            (foo (make-ftype-pointer double 0)))))
      'replace)
    #t)
  (error? ; ftype mismatch w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A int)
            (define (foo x y) (ftype-set! A () x y))
            (foo (make-ftype-pointer A 0) (make-ftype-pointer double 0)))))
      'replace)
    #t)
  (error? ; ftype mismatch w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A uptr)
            (define (foo x) (ftype-locked-incr! A () x))
            (foo 17))))
      'replace)
    #t)
  (error? ; 17 is not an fptr w/source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A uptr)
            (eval '(define (foo x) (ftype-locked-incr! A () x)))
            (foo 17))))
      'replace)
    #t)
  (error? ; 17 is not an fptr w/o source info
    (load "testfile.ss"))

  (begin
    (with-output-to-file "testfile.ss"
      (lambda ()
        (for-each pretty-print
          '((define-ftype A (* uptr))
            (define (foo x n) (ftype-ref A (n) x))
            (define x (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
            (define y (make-ftype-pointer uptr (foreign-alloc (ftype-sizeof uptr))))
            (ftype-set! A () x y)
            (guard (c [else
                        (foreign-free (ftype-pointer-address x))
                        (foreign-free (ftype-pointer-address y))
                        (raise c)])
              (foo x 'a)))))
      'replace)
    #t)
  (error? ; invalid index a for A
    (load "testfile.ss"))
)

(mat ftype-inheritance
  (begin
    (define-ftype A (struct [a double] [b int]))
    (define-ftype Bl (endian little (struct [a double] [b int])))
    (define-ftype Bb (endian big (struct [a double] [b int])))
    (define-ftype C (union [a int] [b unsigned]))
    (define-ftype D double)
    (define-ftype Dl (endian little double))
    (define-ftype Db (endian big double))
    (define-ftype E (packed (struct [a double] [b int])))
    (define-ftype G (packed (array 5 double)))
    (define-ftype Gu (array 5 double))
    (define-ftype H (struct [a (endian big G)] [b int]))
    (define-ftype I (struct [a Gu] [b int]))
    (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
    (define bl (make-ftype-pointer Bl (foreign-alloc (ftype-sizeof Bl))))
    (define bb (make-ftype-pointer Bb (foreign-alloc (ftype-sizeof Bb))))
    (define c (make-ftype-pointer C (foreign-alloc (ftype-sizeof C))))
    (define d (make-ftype-pointer D (foreign-alloc (ftype-sizeof D))))
    (define e (make-ftype-pointer E (foreign-alloc (ftype-sizeof E))))
    (define f (make-ftype-pointer double (foreign-alloc (ftype-sizeof double))))
    (define g (make-ftype-pointer G (foreign-alloc (ftype-sizeof G))))
    (define h (make-ftype-pointer H (foreign-alloc (ftype-sizeof H))))
    (define i (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
    (ftype-set! A (a) a 3.14)
    (ftype-set! A (b) a 75)
    (ftype-set! Bl (a) bl -3.14)
    (ftype-set! Bl (b) bl -75)
    (ftype-set! Bb (a) bb -3.14)
    (ftype-set! Bb (b) bb -75)
    (ftype-set! C (a) c -750)
    (ftype-set! D () d 3.0)
    (ftype-set! E (a) e -3.1415)
    (ftype-set! E (b) e -7755)
    (ftype-set! G (0) g 88.5)
    (ftype-set! H (a 0) h 100.5)
    (ftype-set! I (a 0) i 100.5)
    (ftype-set! double () f -3.0)
    #t)

  (error? ; ftype mismatch
    (ftype-ref A (a) bl))
  (error? ; ftype mismatch
    (ftype-ref A (a) bb))
  (error? ; ftype mismatch
    (ftype-ref A (a) c))
  (error? ; ftype mismatch
    (ftype-ref A (a) d))
  (error? ; ftype mismatch
    (ftype-ref A (a) e))
  (error? ; ftype mismatch
    (ftype-ref A (a) f))

  (error? ; ftype mismatch
    (ftype-ref Bl (b) a))
  (error? ; ftype mismatch
    (ftype-ref Bl (b) c))
  (error? ; ftype mismatch
    (ftype-ref Bl (b) d))
  (error? ; ftype mismatch
    (ftype-ref Bl (b) e))
  (error? ; ftype mismatch
    (ftype-ref Bl (b) f))

  (error? ; ftype mismatch
    (ftype-set! E (a) a 0.0))
  (error? ; ftype mismatch
    (ftype-set! E (a) bl 0.0))
  (error? ; ftype mismatch
    (ftype-set! E (a) bb 0.0))
  (error? ; ftype mismatch
    (ftype-set! E (a) c 0))
  (error? ; ftype mismatch
    (ftype-set! E (a) d 0.0))
  (error? ; ftype mismatch
    (ftype-set! E (a) f 0.0))

  (error? ; ftype mismatch
    (ftype-ref int () c))
  (error? ; ftype mismatch
    (ftype-ref unsigned () c))
  (error? ; ftype mismatch
    (ftype-set! int () c 0))
  (error? ; ftype mismatch
    (ftype-set! unsigned () c 0))

  (eqv? (ftype-ref A (a) a) 3.14)
  (eqv? (ftype-ref D () a) 3.14)
  (eqv? (ftype-ref double () a) 3.14)
  (eqv? (ftype-set! D () a -3.5) (void))
  (eqv? (ftype-ref A (a) a) -3.5)
  (eqv? (ftype-set! double () a 666.6) (void))
  (eqv? (ftype-ref A (a) a) 666.6)

  (error? ; ftype mismatch
    (ftype-ref int () a))

  (eqv? (ftype-ref Bl (a) bl) -3.14)
  (or (not (eq? (native-endianness) 'little))
      (eqv? (ftype-ref D () bl) -3.14))
  (eqv? (ftype-ref Dl () bl) -3.14)
  (or (not (eq? (native-endianness) 'little))
      (eqv? (ftype-ref double () bl) -3.14))
  (error? ; invalid syntax
    (ftype-ref (endian little double) () bl))

  (eqv? (ftype-ref Bb (a) bb) -3.14)
  (or (not (eq? (native-endianness) 'big))
      (eqv? (ftype-ref D () bb) -3.14))
  (eqv? (ftype-ref Db () bb) -3.14)
  (or (not (eq? (native-endianness) 'big))
      (eqv? (ftype-ref double () bb) -3.14))
  (error? ; invalid syntax
    (ftype-ref (endian big double) () bb))

  (eqv? (ftype-ref E (a) e) -3.1415)
  (eqv? (ftype-ref D () e) -3.1415)
  (eqv? (ftype-ref double () e) -3.1415)
  (eqv? (ftype-set! D () e 3.1416) (void))
  (eqv? (ftype-ref E (a) e) 3.1416)
  (eqv? (ftype-set! double () e -3.1416) (void))
  (eqv? (ftype-ref E (a) e) -3.1416)

  (eqv? (ftype-ref G (0) g) 88.5)
  (eqv? (ftype-ref D () g) 88.5)
  (eqv? (ftype-ref double () g) 88.5)
  (eqv? (ftype-set! D () g 3.1416) (void))
  (eqv? (ftype-ref G (0) g) 3.1416)
  (eqv? (ftype-set! double () g -3.1416) (void))
  (eqv? (ftype-ref G (0) g) -3.1416)

  (eqv? (ftype-ref H (a 0) h) 100.5)
  (eqv? (ftype-ref G (0) h) 100.5)
  (eqv? (ftype-ref D () h) 100.5)
  (eqv? (ftype-ref double () h) 100.5)
  (eqv? (ftype-set! D () h 3.1416) (void))
  (eqv? (ftype-ref H (a 0) h) 3.1416)
  (eqv? (ftype-set! double () h -3.1416) (void))
  (eqv? (ftype-ref H (a 0) h) -3.1416)

  (eqv? (ftype-ref I (a 0) i) 100.5)
  (eqv? (ftype-ref Gu (0) i) 100.5)
  (eqv? (ftype-ref D () i) 100.5)
  (eqv? (ftype-ref double () i) 100.5)
  (eqv? (ftype-set! D () i 3.1416) (void))
  (eqv? (ftype-ref I (a 0) i) 3.1416)
  (eqv? (ftype-set! double () i -3.1416) (void))
  (eqv? (ftype-ref I (a 0) i) -3.1416)

  (begin
    (fptr-free a)
    (fptr-free bl)
    (fptr-free bb)
    (fptr-free c)
    (fptr-free d)
    (fptr-free e)
    (fptr-free f)
    (fptr-free g)
    (fptr-free h)
    (fptr-free i)
    #t)
)

(mat ftype-lock-operations ; also tested in thread.ms
  (begin
    (meta-cond
      [(eq? (native-endianness) 'little)
       (define-ftype swapped-iptr (endian big iptr))]
      [else
       (define-ftype swapped-iptr (endian little iptr))])
    (define-ftype A
      (struct
        [a double]
        [b wchar]
        [c uptr]
        [d float]
        [e integer-16]
        [f (struct
             (f1 iptr)
             (f2 (array 3 (union (f3a fixnum) (f3b iptr)))))]
        [g (* iptr)]
        [h swapped-iptr]))
    (define g (make-ftype-pointer iptr (foreign-alloc (ftype-sizeof iptr))))
    (define x (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
    (ftype-set! A (g) x g)
    (define $idx 2)
    #t)

  (error? ; invalid syntax
    (ftype-locked-incr!))
  (error? ; invalid syntax
    (ftype-locked-incr! A))
  (error? ; invalid syntax
    (ftype-locked-incr! A x))
  (error? ; invalid syntax
    (ftype-locked-incr! A (a . b) x))
  (error? ; not an ftype
    (ftype-locked-incr! x () x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-incr! A (a) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-incr! A (b) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-incr! A (d) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-incr! A (e) x))
  (error? ; unsupported non-base
    (ftype-locked-incr! A (f) x))
  (error? ; unsupported non-base
    (ftype-locked-incr! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-locked-incr! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-locked-incr! A (f f2 0) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-incr! A (f f2 0 f3a) x))
  (error? ; unsupported non-base
    (ftype-locked-incr! A (g) x))
  (error? ; unsupported swapped
    (ftype-locked-incr! A (h) x))

  (error? ; invalid syntax
    (ftype-locked-decr!))
  (error? ; invalid syntax
    (ftype-locked-decr! A))
  (error? ; invalid syntax
    (ftype-locked-decr! A x))
  (error? ; invalid syntax
    (ftype-locked-decr! A (a . b) x))
  (error? ; not an ftype
    (ftype-locked-decr! x () x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-decr! A (a) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-decr! A (b) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-decr! A (d) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-decr! A (e) x))
  (error? ; unsupported non-base
    (ftype-locked-decr! A (f) x))
  (error? ; unsupported non-base
    (ftype-locked-decr! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-locked-decr! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-locked-decr! A (f f2 0) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-locked-decr! A (f f2 0 f3a) x))
  (error? ; unsupported non-base
    (ftype-locked-decr! A (g) x))
  (error? ; unsupported swapped
    (ftype-locked-decr! A (h) x))

  (error? ; invalid syntax
    (ftype-init-lock!))
  (error? ; invalid syntax
    (ftype-init-lock! A))
  (error? ; invalid syntax
    (ftype-init-lock! A x))
  (error? ; invalid syntax
    (ftype-init-lock! A (a . b) x))
  (error? ; not an ftype
    (ftype-init-lock! x () x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-init-lock! A (a) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-init-lock! A (b) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-init-lock! A (d) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-init-lock! A (e) x))
  (error? ; unsupported non-base
    (ftype-init-lock! A (f) x))
  (error? ; unsupported non-base
    (ftype-init-lock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-init-lock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-init-lock! A (f f2 0) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-init-lock! A (f f2 0 f3a) x))
  (error? ; unsupported non-base
    (ftype-init-lock! A (g) x))
  (error? ; unsupported swapped
    (ftype-init-lock! A (h) x))

  (error? ; invalid syntax
    (ftype-lock!))
  (error? ; invalid syntax
    (ftype-lock! A))
  (error? ; invalid syntax
    (ftype-lock! A x))
  (error? ; invalid syntax
    (ftype-lock! A (a . b) x))
  (error? ; not an ftype
    (ftype-lock! x () x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-lock! A (a) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-lock! A (b) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-lock! A (d) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-lock! A (e) x))
  (error? ; unsupported non-base
    (ftype-lock! A (f) x))
  (error? ; unsupported non-base
    (ftype-lock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-lock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-lock! A (f f2 0) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-lock! A (f f2 0 f3a) x))
  (error? ; unsupported non-base
    (ftype-lock! A (g) x))
  (error? ; unsupported swapped
    (ftype-lock! A (h) x))

  (error? ; invalid syntax
    (ftype-spin-lock!))
  (error? ; invalid syntax
    (ftype-spin-lock! A))
  (error? ; invalid syntax
    (ftype-spin-lock! A x))
  (error? ; invalid syntax
    (ftype-spin-lock! A (a . b) x))
  (error? ; not an ftype
    (ftype-spin-lock! x () x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-spin-lock! A (a) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-spin-lock! A (b) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-spin-lock! A (d) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-spin-lock! A (e) x))
  (error? ; unsupported non-base
    (ftype-spin-lock! A (f) x))
  (error? ; unsupported non-base
    (ftype-spin-lock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-spin-lock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-spin-lock! A (f f2 0) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-spin-lock! A (f f2 0 f3a) x))
  (error? ; unsupported non-base
    (ftype-spin-lock! A (g) x))
  (error? ; unsupported swapped
    (ftype-spin-lock! A (h) x))

  (error? ; invalid syntax
    (ftype-unlock!))
  (error? ; invalid syntax
    (ftype-unlock! A))
  (error? ; invalid syntax
    (ftype-unlock! A x))
  (error? ; invalid syntax
    (ftype-unlock! A (a . b) x))
  (error? ; not an ftype
    (ftype-unlock! x () x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-unlock! A (a) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-unlock! A (b) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-unlock! A (d) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-unlock! A (e) x))
  (error? ; unsupported non-base
    (ftype-unlock! A (f) x))
  (error? ; unsupported non-base
    (ftype-unlock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-unlock! A (f f2) x))
  (error? ; unsupported non-base
    (ftype-unlock! A (f f2 0) x))
  (error? ; unsupported non-integer or non-word-size
    (ftype-unlock! A (f f2 0 f3a) x))
  (error? ; unsupported non-base
    (ftype-unlock! A (g) x))
  (error? ; unsupported swapped
    (ftype-unlock! A (h) x))

  (begin
    (ftype-set! A (c) x 0)
    (ftype-set! A (f f1) x 0)
    (ftype-set! A (f f2 1 f3b) x 0)
    (ftype-set! A (f f2 $idx f3b) x 0)
    (ftype-set! A (g *) x 0)
    #t)

  (not (ftype-locked-incr! A (c) x))
  (not (ftype-locked-incr! A (f f1) x))
  (not (ftype-locked-incr! A (f f2 1 f3b) x))
  (not (ftype-locked-incr! A (f f2 $idx f3b) x))
  (not (ftype-locked-incr! A (g *) x))

  (ftype-locked-decr! A (c) x)
  (ftype-locked-decr! A (f f1) x)
  (ftype-locked-decr! A (f f2 1 f3b) x)
  (ftype-locked-decr! A (f f2 $idx f3b) x)
  (ftype-locked-decr! A (g *) x)

  (not (ftype-locked-decr! A (c) x))
  (not (ftype-locked-decr! A (f f1) x))
  (not (ftype-locked-decr! A (f f2 1 f3b) x))
  (not (ftype-locked-decr! A (f f2 $idx f3b) x))
  (not (ftype-locked-decr! A (g *) x))

  (not (ftype-locked-decr! A (c) x))
  (not (ftype-locked-decr! A (f f1) x))
  (not (ftype-locked-decr! A (f f2 1 f3b) x))
  (not (ftype-locked-decr! A (f f2 $idx f3b) x))
  (not (ftype-locked-decr! A (g *) x))

  (not (ftype-locked-incr! A (c) x))
  (not (ftype-locked-incr! A (f f1) x))
  (not (ftype-locked-incr! A (f f2 1 f3b) x))
  (not (ftype-locked-incr! A (f f2 $idx f3b) x))
  (not (ftype-locked-incr! A (g *) x))

  (ftype-locked-incr! A (c) x)
  (ftype-locked-incr! A (f f1) x)
  (ftype-locked-incr! A (f f2 1 f3b) x)
  (ftype-locked-incr! A (f f2 $idx f3b) x)
  (ftype-locked-incr! A (g *) x)

  (equal?
    (list
      (ftype-ref A (c) x)
      (ftype-ref A (f f1) x)
      (ftype-ref A (f f2 1 f3b) x)
      (ftype-ref A (f f2 $idx f3b) x)
      (ftype-ref A (g *) x))
    '(0 0 0 0 0))

  (begin
    (ftype-init-lock! A (c) x)
    (ftype-init-lock! A (f f1) x)
    (ftype-init-lock! A (f f2 1 f3b) x)
    (ftype-init-lock! A (f f2 $idx f3b) x)
    (ftype-init-lock! A (g *) x)
    #t)

  (ftype-lock! A (c) x)
  (ftype-lock! A (f f1) x)
  (ftype-lock! A (f f2 1 f3b) x)
  (ftype-lock! A (f f2 $idx f3b) x)
  (ftype-lock! A (g *) x)

  (not (ftype-lock! A (c) x))
  (not (ftype-lock! A (f f1) x))
  (not (ftype-lock! A (f f2 1 f3b) x))
  (not (ftype-lock! A (f f2 $idx f3b) x))
  (not (ftype-lock! A (g *) x))

  (eq? (ftype-unlock! A (c) x) (void))
  (eq? (ftype-unlock! A (f f1) x) (void))
  (eq? (ftype-unlock! A (f f2 1 f3b) x) (void))
  (eq? (ftype-unlock! A (f f2 $idx f3b) x) (void))
  (eq? (ftype-unlock! A (g *) x) (void))

  (eq? (ftype-spin-lock! A (c) x) (void))
  (eq? (ftype-spin-lock! A (f f1) x) (void))
  (eq? (ftype-spin-lock! A (f f2 1 f3b) x) (void))
  (eq? (ftype-spin-lock! A (f f2 $idx f3b) x) (void))
  (eq? (ftype-spin-lock! A (g *) x) (void))

  (not (ftype-lock! A (c) x))
  (not (ftype-lock! A (f f1) x))
  (not (ftype-lock! A (f f2 1 f3b) x))
  (not (ftype-lock! A (f f2 $idx f3b) x))
  (not (ftype-lock! A (g *) x))

  (begin
    (fptr-free x)
    (fptr-free g)
    #t)
)

(mat ftype-compile-file
 ; first, load from source
  (begin
    (with-output-to-file "testfile-ftype1.ss"
      (lambda ()
        (pretty-print
          '(define-ftype fcf-A (struct [a double] [b wchar])))
        (pretty-print
          '(define a (make-ftype-pointer fcf-A (foreign-alloc (ftype-sizeof fcf-A))))))
      'replace)
    (load "testfile-ftype1.ss")
    #t)

  (begin
    (ftype-set! fcf-A (a) a 3.4)
    (ftype-set! fcf-A (b) a #\$)
    #t)

  (eqv? (ftype-ref fcf-A (a) a) 3.4)
  (eqv? (ftype-ref fcf-A (b) a) #\$)
  (eqv? (ftype-ref double () a) 3.4)

 ; now try compile-file and load the object file
  (begin
    (with-output-to-file "testfile-ftype1.ss"
      (lambda ()
        (pretty-print
          '(define-ftype fcf-A (struct [a double] [b wchar])))
        (pretty-print
          '(define a (make-ftype-pointer fcf-A (foreign-alloc (ftype-sizeof fcf-A))))))
      'replace)
    (for-each separate-compile '(ftype1))
    (load "testfile-ftype1.so")
    #t)

  (begin
    (ftype-set! fcf-A (a) a 3.4)
    (ftype-set! fcf-A (b) a #\$)
    #t)

  (eqv? (ftype-ref fcf-A (a) a) 3.4)
  (eqv? (ftype-ref fcf-A (b) a) #\$)
  (eqv? (ftype-ref double () a) 3.4)

  (begin
    (define old-a a)
    (load "testfile-ftype1.so")
    #t)

  (begin
    (ftype-set! fcf-A (a) old-a 3.4)
    (ftype-set! fcf-A (b) old-a #\$)
    #t)

  (eqv? (ftype-ref fcf-A (a) old-a) 3.4)
  (eqv? (ftype-ref fcf-A (b) old-a) #\$)
  (eqv? (ftype-ref double () old-a) 3.4)

  ; check fasling of recursive ftype definitions
  (begin
    (with-output-to-file "testfile-ftype2.ss"
      (lambda ()
        (pretty-print
          '(define-ftype fcf-B
             (struct
               [data double]
               [next (* fcf-B)]))))
      'replace)
    (separate-compile "testfile-ftype2")
    (load "testfile-ftype2.so")
    #t)
  (equal?
    (ftype-pointer-ftype (make-ftype-pointer fcf-B 0))
    '(struct
       [data double]
       [next (* fcf-B)]))
  ; directly check that cyclic rtd fasl'd in okay
  (let ([ftd (record-rtd (make-ftype-pointer fcf-B 0))])
    (let ([ftd2 (caddr (cadr ((record-accessor (record-rtd ftd) 0) ftd)))])
      (eq? ((record-accessor (record-rtd ftd2) 0) ftd2) ftd)))
  ; indirectly check
  (let* ([addr (foreign-alloc (ftype-sizeof fcf-B))]
         [x (make-ftype-pointer fcf-B addr)])
    (dynamic-wind
      void
      (lambda ()
        (ftype-set! fcf-B (next) x (make-ftype-pointer fcf-B 0))
        (ftype-pointer? (ftype-ref fcf-B (next) x)))
      (lambda () (foreign-free addr))))
  ; regression test: verify that we can fasl in a cyclic ftd that's already registered on its uid
  (begin
    (mkfile "testfile-ftype3.ss"
      '(define-ftype
         [ftype3-A (* ftype3-B)]
         [ftype3-B (struct [h ftype3-A])]))
    (compile-file "testfile-ftype3")
    #t)
  (begin ; once should prove it
    (load "testfile-ftype3.so")
    (ftype-pointer? ftype3-A (make-ftype-pointer ftype3-B 0)))
  (begin ; twice for that warm fuzzy feeling
    (load "testfile-ftype3.so")
    (ftype-pointer? ftype3-A (make-ftype-pointer ftype3-B 0)))
  (begin
    (mkfile "testfile-ftype4.ss"
      '(define-ftype
         [ftype4-A (struct [q (* ftype4-B)])]
         [ftype4-B (struct [h (* ftype4-A)])]))
    (compile-file "testfile-ftype4")
    #t)
  (begin ; once should prove it
    (load "testfile-ftype4.so")
      (ftype-pointer? ftype4-A (make-ftype-pointer ftype4-A 0)))
  (begin ; twice for that warm fuzzy feeling
    (load "testfile-ftype4.so")
    (ftype-pointer? ftype4-B (make-ftype-pointer ftype4-B 0)))
  (begin
    (mkfile "testfile-ftype5.ss"
      '(define-ftype
         [ftype5-A (struct [q (* ftype4-A)])]))
    (compile-file "testfile-ftype5")
    #t)
  (begin
    (load "testfile-ftype5.so")
    (ftype-pointer? ftype5-A (make-ftype-pointer ftype5-A 0)))
)

(mat ftype-bits
  (begin
    (define z (make-ftype-pointer unsigned-32 (foreign-alloc (ftype-sizeof unsigned-32))))
    (ftype-set! unsigned-32 () z #b101101011010111010)
    #t)

  (equal?
    (list 
      (#%$fptr-ref-bits 'unsigned-32 #f #f z 0 0 4)
      (#%$fptr-ref-bits 'unsigned-32 #f #f z 0 0 5)
      (#%$fptr-ref-bits 'unsigned-32 #f #f z 0 0 6)
      (#%$fptr-ref-bits 'unsigned-32 #f #f z 0 0 7)
      (#%$fptr-ref-bits 'unsigned-32 #f #f z 0 1 7)
      (#%$fptr-ref-bits 'unsigned-32 #f #f z 0 1 6))
    '(10 26 58 58 29 29))

  (equal?
    (list 
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 0 4)
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 0 5)
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 0 6)
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 0 7)
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 1 7)
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 1 6))
    '(-6 -6 -6 58 29 -3))

  (begin
    (#%$fptr-set-bits! 'unsigned-32 #f z 0 1 6 5)
    (#%$fptr-set-bits! 'unsigned-32 #f z 0 6 10 -3)
    (#%$fptr-set-bits! 'unsigned-32 #f z 0 10 15 10)
    #t)

  (equal?
    (list
      (#%$fptr-ref-bits 'unsigned-32 #f #f z 0 1 6)
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 6 10)
      (#%$fptr-ref-bits 'unsigned-32 #f #t z 0 10 15))
    '(5 -3 10))

  (begin
    (fptr-free z)
    #t)

 ; ----------------
  (begin
    (define-ftype Bbits
      (endian little
        (union
          [a1 (struct
                [a1 unsigned-16]
                [a2 unsigned-8]
                [a3 unsigned-64]
                [a4 unsigned-32])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 1]
                      [a2 signed 15])]
                [a2 (bits
                      [a1 signed 3]
                      [a2 signed 5])]
                [a3 (bits
                      [a1 signed 50]
                      [a2 signed 14])]
                [a4 (bits
                      [a1 signed 19]
                      [a2 signed 13])])]
          [a3 (struct
                [a1 (bits
                      [a1 unsigned 1]
                      [a2 unsigned 15])]
                [a2 (bits
                      [a1 unsigned 3]
                      [a2 unsigned 5])]
                [a3 (bits
                      [a1 unsigned 50]
                      [a2 unsigned 14])]
                [a4 (bits
                      [a1 unsigned 19]
                      [a2 unsigned 13])])])))
    (define x (make-ftype-pointer Bbits (foreign-alloc (ftype-sizeof Bbits))))
    #t)

  (error? ;; invalid value 113886 for bit field of size 1
    (ftype-set! Bbits (a2 a1 a1) x #x1bcde))

  (error? ;; invalid value #\a for bit field of size 3
    (ftype-set! Bbits (a2 a2 a1) x #\a))

  (error? ;; invalid value oops for bit field of size 14
    (ftype-set! Bbits (a3 a3 a2) x 'oops))

  (begin
    (ftype-set! Bbits (a1 a1) x #xabce)
    (ftype-set! Bbits (a1 a2) x #xde)
    (ftype-set! Bbits (a1 a3) x #xf9357c18d679e35b)
    (ftype-set! Bbits (a1 a4) x #x7c18d679)
    #t)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) #x0)
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (- #x55e7 (expt 2 15)))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (- #x6 (expt 2 3)))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (- #x1b (expt 2 5)))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) #x17c18d679e35b)
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (- #x3e4d (expt 2 14)))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) #xd679)
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) #xf83)

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) #x0)
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) #x55e7)
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) #x6)
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) #x1b)
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) #x17c18d679e35b)
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) #x3e4d)
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) #xd679)
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) #xf83)

  (begin
    (ftype-set! Bbits (a1 a1) x #x7c7c)
    (ftype-set! Bbits (a1 a2) x #xa8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x91919191)
    #t)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x #x-1)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #x7c7d)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x #x0)
    (ftype-set! Bbits (a2 a1 a2) x (- #x55e7 (expt 2 15)))
    (ftype-set! Bbits (a2 a2 a1) x (- #x6 (expt 2 3)))
    (ftype-set! Bbits (a2 a2 a2) x (- #x1b (expt 2 5)))
    (ftype-set! Bbits (a2 a3 a1) x #x17c18d679e35b)
    (ftype-set! Bbits (a2 a3 a2) x (- #x3e4d (expt 2 14)))
    (ftype-set! Bbits (a2 a4 a1) x #xd679)
    (ftype-set! Bbits (a2 a4 a2) x #xf83)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #xabce)
  (eqv? (ftype-ref Bbits (a1 a2) x) #xde)
  (eqv? (ftype-ref Bbits (a1 a3) x) #xf9357c18d679e35b)
  (eqv? (ftype-ref Bbits (a1 a4) x) #x7c18d679)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) #x0)
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (- #x55e7 (expt 2 15)))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (- #x6 (expt 2 3)))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (- #x1b (expt 2 5)))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) #x17c18d679e35b)
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (- #x3e4d (expt 2 14)))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) #xd679)
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) #xf83)

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) #x0)
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) #x55e7)
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) #x6)
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) #x1b)
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) #x17c18d679e35b)
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) #x3e4d)
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) #xd679)
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) #xf83)

  (begin
    (ftype-set! Bbits (a1 a1) x #xc7c7)
    (ftype-set! Bbits (a1 a2) x #xa8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x91919191)
    #t)

  (begin
    (ftype-set! Bbits (a3 a1 a1) x #x0)
    (ftype-set! Bbits (a3 a1 a2) x #x55e7)
    (ftype-set! Bbits (a3 a2 a1) x #x6)
    (ftype-set! Bbits (a3 a2 a2) x #x1b)
    (ftype-set! Bbits (a3 a3 a1) x #x17c18d679e35b)
    (ftype-set! Bbits (a3 a3 a2) x #x3e4d)
    (ftype-set! Bbits (a3 a4 a1) x #xd679)
    (ftype-set! Bbits (a3 a4 a2) x #xf83)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #xabce)
  (eqv? (ftype-ref Bbits (a1 a2) x) #xde)
  (eqv? (ftype-ref Bbits (a1 a3) x) #xf9357c18d679e35b)
  (eqv? (ftype-ref Bbits (a1 a4) x) #x7c18d679)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) #x0)
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (- #x55e7 (expt 2 15)))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (- #x6 (expt 2 3)))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (- #x1b (expt 2 5)))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) #x17c18d679e35b)
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (- #x3e4d (expt 2 14)))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) #xd679)
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) #xf83)

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) #x0)
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) #x55e7)
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) #x6)
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) #x1b)
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) #x17c18d679e35b)
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) #x3e4d)
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) #xd679)
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) #xf83)

  (begin
    (fptr-free x)
    #t)

 ; ----------------

  (begin
    (define-ftype Ebits (bits [x signed 32]))
    (define ebits (make-ftype-pointer Ebits 0))
    #t)

  (error? ;; invalid value oops for type bit-field
    (ftype-set! Ebits (x) ebits 'oops))

  (error? ;; invalid value <int> for type bit-field
    (ftype-set! Ebits (x) ebits (expt 2 32)))

 ; ----------------
  (begin
    (define-ftype Bbits
      (endian big
        (union
          [a1 (struct
                [a1 unsigned-16]
                [a2 unsigned-8]
                [a3 unsigned-64]
                [a4 unsigned-32])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 1]
                      [a2 signed 15])]
                [a2 (bits
                      [a1 signed 3]
                      [a2 signed 5])]
                [a3 (bits
                      [a1 signed 50]
                      [a2 signed 14])]
                [a4 (bits
                      [a1 signed 19]
                      [a2 signed 13])])]
          [a3 (struct
                [a1 (bits
                      [a1 unsigned 1]
                      [a2 unsigned 15])]
                [a2 (bits
                      [a1 unsigned 3]
                      [a2 unsigned 5])]
                [a3 (bits
                      [a1 unsigned 50]
                      [a2 unsigned 14])]
                [a4 (bits
                      [a1 unsigned 19]
                      [a2 unsigned 13])])])))
    (define x (make-ftype-pointer Bbits (foreign-alloc (ftype-sizeof Bbits))))
    #t)

  (begin
    (ftype-set! Bbits (a1 a1) x #xabce)
    (ftype-set! Bbits (a1 a2) x #xde)
    (ftype-set! Bbits (a1 a3) x #xf9357c18d679e35b)
    (ftype-set! Bbits (a1 a4) x #x7c18d679)
    #t)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) -1)
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) #x2bce)
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (- #x6 (expt 2 3)))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (- #x1e (expt 2 5)))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (- #x3e4d5f06359e7 (expt 2 50)))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (- #x235b (expt 2 14)))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) #x3e0c6)
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (- #x1679 (expt 2 13)))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) 1)
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) #x2bce)
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) #x6)
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) #x1e)
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) #x3e4d5f06359e7)
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) #x235b)
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) #x3e0c6)
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) #x1679)

  (begin
    (ftype-set! Bbits (a1 a1) x #x7c7c)
    (ftype-set! Bbits (a1 a2) x #xa8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x91919191)
    #t)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x -1)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #xfc7c)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x -1)
    (ftype-set! Bbits (a2 a1 a2) x #x2bce)
    (ftype-set! Bbits (a2 a2 a1) x (- #x6 (expt 2 3)))
    (ftype-set! Bbits (a2 a2 a2) x (- #x1e (expt 2 5)))
    (ftype-set! Bbits (a2 a3 a1) x (- #x3e4d5f06359e7 (expt 2 50)))
    (ftype-set! Bbits (a2 a3 a2) x (- #x235b (expt 2 14)))
    (ftype-set! Bbits (a2 a4 a1) x #x3e0c6)
    (ftype-set! Bbits (a2 a4 a2) x (- #x1679 (expt 2 13)))
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #xabce)
  (eqv? (ftype-ref Bbits (a1 a2) x) #xde)
  (eqv? (ftype-ref Bbits (a1 a3) x) #xf9357c18d679e35b)
  (eqv? (ftype-ref Bbits (a1 a4) x) #x7c18d679)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) -1)
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) #x2bce)
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (- #x6 (expt 2 3)))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (- #x1e (expt 2 5)))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (- #x3e4d5f06359e7 (expt 2 50)))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (- #x235b (expt 2 14)))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) #x3e0c6)
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (- #x1679 (expt 2 13)))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) 1)
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) #x2bce)
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) #x6)
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) #x1e)
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) #x3e4d5f06359e7)
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) #x235b)
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) #x3e0c6)
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) #x1679)

  (begin
    (ftype-set! Bbits (a1 a1) x #xc7c7)
    (ftype-set! Bbits (a1 a2) x #xa8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x91919191)
    #t)

  (begin
    (ftype-set! Bbits (a3 a1 a1) x 1)
    (ftype-set! Bbits (a3 a1 a2) x #x2bce)
    (ftype-set! Bbits (a3 a2 a1) x #x6)
    (ftype-set! Bbits (a3 a2 a2) x #x1e)
    (ftype-set! Bbits (a3 a3 a1) x #x3e4d5f06359e7)
    (ftype-set! Bbits (a3 a3 a2) x #x235b)
    (ftype-set! Bbits (a3 a4 a1) x #x3e0c6)
    (ftype-set! Bbits (a3 a4 a2) x #x1679)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #xabce)
  (eqv? (ftype-ref Bbits (a1 a2) x) #xde)
  (eqv? (ftype-ref Bbits (a1 a3) x) #xf9357c18d679e35b)
  (eqv? (ftype-ref Bbits (a1 a4) x) #x7c18d679)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) -1)
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) #x2bce)
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (- #x6 (expt 2 3)))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (- #x1e (expt 2 5)))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (- #x3e4d5f06359e7 (expt 2 50)))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (- #x235b (expt 2 14)))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) #x3e0c6)
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (- #x1679 (expt 2 13)))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) 1)
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) #x2bce)
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) #x6)
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) #x1e)
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) #x3e4d5f06359e7)
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) #x235b)
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) #x3e0c6)
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) #x1679)

  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype Cbits
      (endian little
        (union
          [a1 (struct
                [a1 unsigned-64]
                [a2 unsigned-64]
                [a3 unsigned-64]
                [a4 unsigned-64]
                [a5 unsigned-64]
                [a6 unsigned-64]
                [a7 unsigned-64])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 64])]
                [a2 (bits
                      [a1 unsigned 64])]
                [a3 (bits
                      [a1 unsigned 63]
                      [a2 signed 1])]
                [a4 (bits
                      [a1 unsigned 1]
                      [a2 signed 63])]
                [a5 (bits
                      [a1 signed 32]
                      [a2 unsigned 16]
                      [a3 signed 8]
                      [a4 unsigned 5]
                      [a5 signed 3])]
                [a6 (bits
                      [a1 unsigned 5]
                      [a2 signed 8]
                      [a3 unsigned 16]
                      [a4 signed 32]
                      [a5 signed 3])]
                [a7 (bits
                      [a1 unsigned 32]
                      [a2 signed 16]
                      [a3 unsigned 8]
                      [a4 signed 5]
                      [a5 unsigned 3])])])))
    (define x (make-ftype-pointer Cbits (foreign-alloc (ftype-sizeof Cbits))))
    #t)

  (begin
    (ftype-set! Cbits (a1 a1) x #x923456789abcdef9)
    (ftype-set! Cbits (a1 a2) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a1 a3) x #x923456789abcdef9)
    (ftype-set! Cbits (a1 a4) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a1 a5) x #x923456789abcdef9)
    (ftype-set! Cbits (a1 a6) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a1 a7) x #x923456789abcdef9)
    #t)

  (eqv? (ftype-ref Cbits (a2 a1 a1) x) (- #x923456789abcdef9 (expt 2 64)))
  (eqv? (ftype-ref Cbits (a2 a2 a1) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a2 a3 a1) x) #x123456789abcdef9)
  (eqv? (ftype-ref Cbits (a2 a3 a2) x) -1)
  (eqv? (ftype-ref Cbits (a2 a4 a1) x) 0)
  (eqv? (ftype-ref Cbits (a2 a4 a2) x) (- (ash #xda3c2d784b69f01e -1) (expt 2 63)))
  (eqv? (ftype-ref Cbits (a2 a5 a1) x) (- #x9abcdef9 (expt 2 32)))
  (eqv? (ftype-ref Cbits (a2 a5 a2) x) #x5678)
  (eqv? (ftype-ref Cbits (a2 a5 a3) x) #x34)
  (eqv? (ftype-ref Cbits (a2 a5 a4) x) #x12)
  (eqv? (ftype-ref Cbits (a2 a5 a5) x) #x-4)
  (eqv? (ftype-ref Cbits (a2 a6 a1) x) #x1e)
  (eqv? (ftype-ref Cbits (a2 a6 a2) x) #x-80)
  (eqv? (ftype-ref Cbits (a2 a6 a3) x) #x5b4f)
  (eqv? (ftype-ref Cbits (a2 a6 a4) x) (- #xD1E16BC2 (expt 2 32)))
  (eqv? (ftype-ref Cbits (a2 a6 a5) x) #x-2)
  (eqv? (ftype-ref Cbits (a2 a7 a1) x) #x9abcdef9)
  (eqv? (ftype-ref Cbits (a2 a7 a2) x) #x5678)
  (eqv? (ftype-ref Cbits (a2 a7 a3) x) #x34)
  (eqv? (ftype-ref Cbits (a2 a7 a4) x) (- #x12 (expt 2 5)))
  (eqv? (ftype-ref Cbits (a2 a7 a5) x) #x4)

  (begin
    (ftype-set! Cbits (a1 a1) x 0)
    (ftype-set! Cbits (a1 a2) x 0)
    (ftype-set! Cbits (a1 a3) x 0)
    (ftype-set! Cbits (a1 a4) x 0)
    (ftype-set! Cbits (a1 a5) x 0)
    (ftype-set! Cbits (a1 a6) x 0)
    (ftype-set! Cbits (a1 a7) x 0)
    #t)

  (begin
    (ftype-set! Cbits (a2 a1 a1) x (- #x923456789abcdef9 (expt 2 64)))
    (ftype-set! Cbits (a2 a2 a1) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a2 a3 a1) x #x123456789abcdef9)
    (ftype-set! Cbits (a2 a3 a2) x -1)
    (ftype-set! Cbits (a2 a4 a1) x 0)
    (ftype-set! Cbits (a2 a4 a2) x (- (ash #xda3c2d784b69f01e -1) (expt 2 63)))
    (ftype-set! Cbits (a2 a5 a1) x (- #x9abcdef9 (expt 2 32)))
    (ftype-set! Cbits (a2 a5 a2) x #x5678)
    (ftype-set! Cbits (a2 a5 a3) x #x34)
    (ftype-set! Cbits (a2 a5 a4) x #x12)
    (ftype-set! Cbits (a2 a5 a5) x #x-4)
    (ftype-set! Cbits (a2 a6 a1) x #x1e)
    (ftype-set! Cbits (a2 a6 a2) x #x-80)
    (ftype-set! Cbits (a2 a6 a3) x #x5b4f)
    (ftype-set! Cbits (a2 a6 a4) x (- #xD1E16BC2 (expt 2 32)))
    (ftype-set! Cbits (a2 a6 a5) x #x-2)
    (ftype-set! Cbits (a2 a7 a1) x #x9abcdef9)
    (ftype-set! Cbits (a2 a7 a2) x #x5678)
    (ftype-set! Cbits (a2 a7 a3) x #x34)
    (ftype-set! Cbits (a2 a7 a4) x #x12)
    (ftype-set! Cbits (a2 a7 a5) x #x4)
    #t)

  (eqv? (ftype-ref Cbits (a1 a1) x) #x923456789abcdef9)
  (eqv? (ftype-ref Cbits (a1 a2) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a1 a3) x) #x923456789abcdef9)
  (eqv? (ftype-ref Cbits (a1 a4) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a1 a5) x) #x923456789abcdef9)
  (eqv? (ftype-ref Cbits (a1 a6) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a1 a7) x) #x923456789abcdef9)

  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype Cbits
      (endian big
        (union
          [a1 (struct
                [a1 unsigned-64]
                [a2 unsigned-64]
                [a3 unsigned-64]
                [a4 unsigned-64]
                [a5 unsigned-64]
                [a6 unsigned-64]
                [a7 unsigned-64])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 64])]
                [a2 (bits
                      [a1 unsigned 64])]
                [a3 (bits
                      [a1 unsigned 63]
                      [a2 signed 1])]
                [a4 (bits
                      [a1 unsigned 1]
                      [a2 signed 63])]
                [a5 (bits
                      [a1 signed 32]
                      [a2 unsigned 16]
                      [a3 signed 8]
                      [a4 unsigned 5]
                      [a5 signed 3])]
                [a6 (bits
                      [a1 unsigned 5]
                      [a2 signed 8]
                      [a3 unsigned 16]
                      [a4 signed 32]
                      [a5 signed 3])]
                [a7 (bits
                      [a1 unsigned 32]
                      [a2 signed 16]
                      [a3 unsigned 8]
                      [a4 signed 5]
                      [a5 unsigned 3])])])))
    (define x (make-ftype-pointer Cbits (foreign-alloc (ftype-sizeof Cbits))))
    #t)

  (begin
    (ftype-set! Cbits (a1 a1) x #x923456789abcdef9)
    (ftype-set! Cbits (a1 a2) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a1 a3) x #x923456789abcdef9)
    (ftype-set! Cbits (a1 a4) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a1 a5) x #x923456789abcdefe)
    (ftype-set! Cbits (a1 a6) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a1 a7) x #x923456789abcdefe)
    #t)

  (eqv? (ftype-ref Cbits (a2 a1 a1) x) (- #x923456789abcdef9 (expt 2 64)))
  (eqv? (ftype-ref Cbits (a2 a2 a1) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a2 a3 a1) x) #x491A2B3C4D5E6F7C)
  (eqv? (ftype-ref Cbits (a2 a3 a2) x) -1)
  (eqv? (ftype-ref Cbits (a2 a4 a1) x) 1)
  (eqv? (ftype-ref Cbits (a2 a4 a2) x) (- #x5A3C2D784B69F01E (expt 2 63)))
  (eqv? (ftype-ref Cbits (a2 a5 a1) x) (- #x92345678 (expt 2 32)))
  (eqv? (ftype-ref Cbits (a2 a5 a2) x) #x9abc)
  (eqv? (ftype-ref Cbits (a2 a5 a3) x) (- #xde (expt 2 8)))
  (eqv? (ftype-ref Cbits (a2 a5 a4) x) #x1f)
  (eqv? (ftype-ref Cbits (a2 a5 a5) x) (- 6 (expt 2 3)))
  (eqv? (ftype-ref Cbits (a2 a6 a1) x) #x1b)
  (eqv? (ftype-ref Cbits (a2 a6 a2) x) #x47)
  (eqv? (ftype-ref Cbits (a2 a6 a3) x) #x85af)
  (eqv? (ftype-ref Cbits (a2 a6 a4) x) #x96d3e03)
  (eqv? (ftype-ref Cbits (a2 a6 a5) x) (- #x6 (expt 2 3)))
  (eqv? (ftype-ref Cbits (a2 a7 a1) x) #x92345678)
  (eqv? (ftype-ref Cbits (a2 a7 a2) x) (- #x9abc (expt 2 16)))
  (eqv? (ftype-ref Cbits (a2 a7 a3) x) #xde)
  (eqv? (ftype-ref Cbits (a2 a7 a4) x) (- #x1f (expt 2 5)))
  (eqv? (ftype-ref Cbits (a2 a7 a5) x) 6)

  (begin
    (ftype-set! Cbits (a1 a1) x 0)
    (ftype-set! Cbits (a1 a2) x 0)
    (ftype-set! Cbits (a1 a3) x 0)
    (ftype-set! Cbits (a1 a4) x 0)
    (ftype-set! Cbits (a1 a5) x 0)
    (ftype-set! Cbits (a1 a6) x 0)
    (ftype-set! Cbits (a1 a7) x 0)
    #t)

  (begin
    (ftype-set! Cbits (a2 a1 a1) x (- #x923456789abcdef9 (expt 2 64)))
    (ftype-set! Cbits (a2 a2 a1) x #xda3c2d784b69f01e)
    (ftype-set! Cbits (a2 a3 a1) x #x491A2B3C4D5E6F7C)
    (ftype-set! Cbits (a2 a3 a2) x -1)
    (ftype-set! Cbits (a2 a4 a1) x 1)
    (ftype-set! Cbits (a2 a4 a2) x (- #x5A3C2D784B69F01E (expt 2 63)))
    (ftype-set! Cbits (a2 a5 a1) x (- #x92345678 (expt 2 32)))
    (ftype-set! Cbits (a2 a5 a2) x #x9abc)
    (ftype-set! Cbits (a2 a5 a3) x (- #xde (expt 2 8)))
    (ftype-set! Cbits (a2 a5 a4) x #x1f)
    (ftype-set! Cbits (a2 a5 a5) x (- 6 (expt 2 3)))
    (ftype-set! Cbits (a2 a6 a1) x #x1b)
    (ftype-set! Cbits (a2 a6 a2) x #x47)
    (ftype-set! Cbits (a2 a6 a3) x #x85af)
    (ftype-set! Cbits (a2 a6 a4) x #x96d3e03)
    (ftype-set! Cbits (a2 a6 a5) x (- #x6 (expt 2 3)))
    (ftype-set! Cbits (a2 a7 a1) x #x92345678)
    (ftype-set! Cbits (a2 a7 a2) x (- #x9abc (expt 2 16)))
    (ftype-set! Cbits (a2 a7 a3) x #xde)
    (ftype-set! Cbits (a2 a7 a4) x (- #x1f (expt 2 5)))
    (ftype-set! Cbits (a2 a7 a5) x 6)
  #t)

  (eqv? (ftype-ref Cbits (a1 a1) x) #x923456789abcdef9)
  (eqv? (ftype-ref Cbits (a1 a2) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a1 a3) x) #x923456789abcdef9)
  (eqv? (ftype-ref Cbits (a1 a4) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a1 a5) x) #x923456789abcdefe)
  (eqv? (ftype-ref Cbits (a1 a6) x) #xda3c2d784b69f01e)
  (eqv? (ftype-ref Cbits (a1 a7) x) #x923456789abcdefe)

  (begin
    (fptr-free x)
    #t)
)

(mat ftype-odd-bits
  (begin
    (define-ftype Bbits
      (endian little
        (union
          [a1 (struct
                [a1 unsigned-24]
                [a2 unsigned-40]
                [a3 unsigned-56]
                [a4 unsigned-48])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 1]
                      [a2 signed 23])]
                [a2 (bits
                      [a1 signed 3]
                      [a2 signed 37])]
                [a3 (bits
                      [a1 signed 42]
                      [a2 signed 14])]
                [a4 (bits
                      [a1 signed 19]
                      [a2 signed 29])])]
          [a3 (struct
                [a1 (bits
                      [a1 unsigned 1]
                      [a2 unsigned 23])]
                [a2 (bits
                      [a1 unsigned 3]
                      [a2 unsigned 37])]
                [a3 (bits
                      [a1 unsigned 42]
                      [a2 unsigned 14])]
                [a4 (bits
                      [a1 unsigned 19]
                      [a2 unsigned 29])])])))
    (define x (make-ftype-pointer Bbits (foreign-alloc (ftype-sizeof Bbits))))
    (define unsigned-bit-field
      (lambda (n start end)
        (bitwise-bit-field n start end)))
    (define signed-bit-field
      (lambda (n start end)
        (let ([n (bitwise-bit-field n start end)])
          (if (fx= (bitwise-arithmetic-shift-right n (fx- end start 1)) 0)
              n
              (- n (bitwise-arithmetic-shift-left 1 (fx- end start)))))))
    #t)

  (error? ;; invalid value 113886 for bit field of size 1
    (ftype-set! Bbits (a2 a1 a1) x #x1bcde))

  (error? ;; invalid value #\a for bit field of size 3
    (ftype-set! Bbits (a2 a2 a1) x #\a))

  (error? ;; invalid value oops for bit field of size 14
    (ftype-set! Bbits (a3 a3 a2) x 'oops))

  (begin
    (define A1 #xabcfde) 
    (define A2 #xde13752b) 
    (define A3 #xf93578d679e35b) 
    (define A4 #x7c18d679)
    #t)

  (begin
    (ftype-set! Bbits (a1 a1) x A1)
    (ftype-set! Bbits (a1 a2) x A2)
    (ftype-set! Bbits (a1 a3) x A3)
    (ftype-set! Bbits (a1 a4) x A4)
    #t)

  (equal?
    (list
      (ftype-ref Bbits (a1 a1) x)
      (ftype-ref Bbits (a1 a2) x)
      (ftype-ref Bbits (a1 a3) x)
      (ftype-ref Bbits (a1 a4) x))
    (list A1 A2 A3 A4))

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) (signed-bit-field A1 0 1))
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (signed-bit-field A1 1 24))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (signed-bit-field A2 0 3))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (signed-bit-field A2 3 40))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (signed-bit-field A3 0 42))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (signed-bit-field A3 42 56))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) (signed-bit-field A4 0 19))
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (signed-bit-field A4 19 48))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) (unsigned-bit-field A1 0 1))
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) (unsigned-bit-field A1 1 24))
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) (unsigned-bit-field A2 0 3))
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) (unsigned-bit-field A2 3 40))
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) (unsigned-bit-field A3 0 42))
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) (unsigned-bit-field A3 42 56))
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) (unsigned-bit-field A4 0 19))
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) (unsigned-bit-field A4 19 48))

  (begin
    (ftype-set! Bbits (a1 a1) x #x7c7c7c)
    (ftype-set! Bbits (a1 a2) x #xa8a8a8a8a8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x919191919191)
    #t)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x #x-1)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #x7c7c7d)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x (signed-bit-field A1 0 1))
    (ftype-set! Bbits (a2 a1 a2) x (signed-bit-field A1 1 24))
    (ftype-set! Bbits (a2 a2 a1) x (signed-bit-field A2 0 3))
    (ftype-set! Bbits (a2 a2 a2) x (signed-bit-field A2 3 40))
    (ftype-set! Bbits (a2 a3 a1) x (signed-bit-field A3 0 42))
    (ftype-set! Bbits (a2 a3 a2) x (signed-bit-field A3 42 56))
    (ftype-set! Bbits (a2 a4 a1) x (signed-bit-field A4 0 19))
    (ftype-set! Bbits (a2 a4 a2) x (signed-bit-field A4 19 48))
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) A1)
  (eqv? (ftype-ref Bbits (a1 a2) x) A2)
  (eqv? (ftype-ref Bbits (a1 a3) x) A3)
  (eqv? (ftype-ref Bbits (a1 a4) x) A4)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) (signed-bit-field A1 0 1))
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (signed-bit-field A1 1 24))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (signed-bit-field A2 0 3))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (signed-bit-field A2 3 40))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (signed-bit-field A3 0 42))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (signed-bit-field A3 42 56))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) (signed-bit-field A4 0 19))
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (signed-bit-field A4 19 48))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) (unsigned-bit-field A1 0 1))
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) (unsigned-bit-field A1 1 24))
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) (unsigned-bit-field A2 0 3))
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) (unsigned-bit-field A2 3 40))
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) (unsigned-bit-field A3 0 42))
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) (unsigned-bit-field A3 42 56))
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) (unsigned-bit-field A4 0 19))
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) (unsigned-bit-field A4 19 48))

  (begin
    (ftype-set! Bbits (a1 a1) x #x7c7c7c)
    (ftype-set! Bbits (a1 a2) x #xa8a8a8a8a8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x919191919191)
    #t)

  (begin
    (ftype-set! Bbits (a3 a1 a1) x (unsigned-bit-field A1 0 1))
    (ftype-set! Bbits (a3 a1 a2) x (unsigned-bit-field A1 1 24))
    (ftype-set! Bbits (a3 a2 a1) x (unsigned-bit-field A2 0 3))
    (ftype-set! Bbits (a3 a2 a2) x (unsigned-bit-field A2 3 40))
    (ftype-set! Bbits (a3 a3 a1) x (unsigned-bit-field A3 0 42))
    (ftype-set! Bbits (a3 a3 a2) x (unsigned-bit-field A3 42 56))
    (ftype-set! Bbits (a3 a4 a1) x (unsigned-bit-field A4 0 19))
    (ftype-set! Bbits (a3 a4 a2) x (unsigned-bit-field A4 19 48))
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) A1)
  (eqv? (ftype-ref Bbits (a1 a2) x) A2)
  (eqv? (ftype-ref Bbits (a1 a3) x) A3)
  (eqv? (ftype-ref Bbits (a1 a4) x) A4)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) (signed-bit-field A1 0 1))
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (signed-bit-field A1 1 24))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (signed-bit-field A2 0 3))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (signed-bit-field A2 3 40))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (signed-bit-field A3 0 42))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (signed-bit-field A3 42 56))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) (signed-bit-field A4 0 19))
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (signed-bit-field A4 19 48))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) (unsigned-bit-field A1 0 1))
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) (unsigned-bit-field A1 1 24))
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) (unsigned-bit-field A2 0 3))
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) (unsigned-bit-field A2 3 40))
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) (unsigned-bit-field A3 0 42))
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) (unsigned-bit-field A3 42 56))
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) (unsigned-bit-field A4 0 19))
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) (unsigned-bit-field A4 19 48))

  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype Bbits
      (endian big
        (union
          [a1 (struct
                [a1 unsigned-24]
                [a2 unsigned-40]
                [a3 unsigned-56]
                [a4 unsigned-48])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 1]
                      [a2 signed 23])]
                [a2 (bits
                      [a1 signed 3]
                      [a2 signed 37])]
                [a3 (bits
                      [a1 signed 42]
                      [a2 signed 14])]
                [a4 (bits
                      [a1 signed 19]
                      [a2 signed 29])])]
          [a3 (struct
                [a1 (bits
                      [a1 unsigned 1]
                      [a2 unsigned 23])]
                [a2 (bits
                      [a1 unsigned 3]
                      [a2 unsigned 37])]
                [a3 (bits
                      [a1 unsigned 42]
                      [a2 unsigned 14])]
                [a4 (bits
                      [a1 unsigned 19]
                      [a2 unsigned 29])])])))
    (define x (make-ftype-pointer Bbits (foreign-alloc (ftype-sizeof Bbits))))
    (define unsigned-bit-field
      (lambda (n start end)
        (bitwise-bit-field n start end)))
    (define signed-bit-field
      (lambda (n start end)
        (let ([n (bitwise-bit-field n start end)])
          (if (fx= (bitwise-arithmetic-shift-right n (fx- end start 1)) 0)
              n
              (- n (bitwise-arithmetic-shift-left 1 (fx- end start)))))))
    #t)

  (error? ;; invalid value 113886 for bit field of size 1
    (ftype-set! Bbits (a2 a1 a1) x #x1bcde))

  (error? ;; invalid value #\a for bit field of size 3
    (ftype-set! Bbits (a2 a2 a1) x #\a))

  (error? ;; invalid value oops for bit field of size 14
    (ftype-set! Bbits (a3 a3 a2) x 'oops))

  (begin
    (define A1 #xabcfde) 
    (define A2 #xde13752b) 
    (define A3 #xf93578d679e35b) 
    (define A4 #x7c18d679)
    #t)

  (begin
    (ftype-set! Bbits (a1 a1) x A1)
    (ftype-set! Bbits (a1 a2) x A2)
    (ftype-set! Bbits (a1 a3) x A3)
    (ftype-set! Bbits (a1 a4) x A4)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) A1)
  (eqv? (ftype-ref Bbits (a1 a2) x) A2)
  (eqv? (ftype-ref Bbits (a1 a3) x) A3)
  (eqv? (ftype-ref Bbits (a1 a4) x) A4)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) (signed-bit-field A1 23 24))
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (signed-bit-field A1 0 23))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (signed-bit-field A2 37 40))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (signed-bit-field A2 0 37))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (signed-bit-field A3 14 56))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (signed-bit-field A3 0 14))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) (signed-bit-field A4 29 48))
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (signed-bit-field A4 0 29))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) (unsigned-bit-field A1 23 24))
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) (unsigned-bit-field A1 0 23))
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) (unsigned-bit-field A2 37 40))
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) (unsigned-bit-field A2 0 37))
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) (unsigned-bit-field A3 14 56))
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) (unsigned-bit-field A3 0 14))
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) (unsigned-bit-field A4 29 48))
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) (unsigned-bit-field A4 0 29))

  (begin
    (ftype-set! Bbits (a1 a1) x #x7c7c7c)
    (ftype-set! Bbits (a1 a2) x #xa8a8a8a8a8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x919191919191)
    #t)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x #x-1)
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) #xfc7c7c)

  (begin
    (ftype-set! Bbits (a2 a1 a1) x (signed-bit-field A1 23 24))
    (ftype-set! Bbits (a2 a1 a2) x (signed-bit-field A1 0 23))
    (ftype-set! Bbits (a2 a2 a1) x (signed-bit-field A2 37 40))
    (ftype-set! Bbits (a2 a2 a2) x (signed-bit-field A2 0 37))
    (ftype-set! Bbits (a2 a3 a1) x (signed-bit-field A3 14 56))
    (ftype-set! Bbits (a2 a3 a2) x (signed-bit-field A3 0 14))
    (ftype-set! Bbits (a2 a4 a1) x (signed-bit-field A4 29 48))
    (ftype-set! Bbits (a2 a4 a2) x (signed-bit-field A4 0 29))
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) A1)
  (eqv? (ftype-ref Bbits (a1 a2) x) A2)
  (eqv? (ftype-ref Bbits (a1 a3) x) A3)
  (eqv? (ftype-ref Bbits (a1 a4) x) A4)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) (signed-bit-field A1 23 24))
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (signed-bit-field A1 0 23))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (signed-bit-field A2 37 40))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (signed-bit-field A2 0 37))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (signed-bit-field A3 14 56))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (signed-bit-field A3 0 14))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) (signed-bit-field A4 29 48))
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (signed-bit-field A4 0 29))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) (unsigned-bit-field A1 23 24))
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) (unsigned-bit-field A1 0 23))
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) (unsigned-bit-field A2 37 40))
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) (unsigned-bit-field A2 0 37))
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) (unsigned-bit-field A3 14 56))
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) (unsigned-bit-field A3 0 14))
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) (unsigned-bit-field A4 29 48))
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) (unsigned-bit-field A4 0 29))

  (begin
    (ftype-set! Bbits (a1 a1) x #x7c7c7c)
    (ftype-set! Bbits (a1 a2) x #xa8a8a8a8a8)
    (ftype-set! Bbits (a1 a3) x #x3b3b3b3b3b3b3b)
    (ftype-set! Bbits (a1 a4) x #x919191919191)
    #t)

  (begin
    (ftype-set! Bbits (a3 a1 a1) x (unsigned-bit-field A1 23 24))
    (ftype-set! Bbits (a3 a1 a2) x (unsigned-bit-field A1 0 23))
    (ftype-set! Bbits (a3 a2 a1) x (unsigned-bit-field A2 37 40))
    (ftype-set! Bbits (a3 a2 a2) x (unsigned-bit-field A2 0 37))
    (ftype-set! Bbits (a3 a3 a1) x (unsigned-bit-field A3 14 56))
    (ftype-set! Bbits (a3 a3 a2) x (unsigned-bit-field A3 0 14))
    (ftype-set! Bbits (a3 a4 a1) x (unsigned-bit-field A4 29 48))
    (ftype-set! Bbits (a3 a4 a2) x (unsigned-bit-field A4 0 29))
    #t)

  (eqv? (ftype-ref Bbits (a1 a1) x) A1)
  (eqv? (ftype-ref Bbits (a1 a2) x) A2)
  (eqv? (ftype-ref Bbits (a1 a3) x) A3)
  (eqv? (ftype-ref Bbits (a1 a4) x) A4)

  (eqv? (ftype-ref Bbits (a2 a1 a1) x) (signed-bit-field A1 23 24))
  (eqv? (ftype-ref Bbits (a2 a1 a2) x) (signed-bit-field A1 0 23))
  (eqv? (ftype-ref Bbits (a2 a2 a1) x) (signed-bit-field A2 37 40))
  (eqv? (ftype-ref Bbits (a2 a2 a2) x) (signed-bit-field A2 0 37))
  (eqv? (ftype-ref Bbits (a2 a3 a1) x) (signed-bit-field A3 14 56))
  (eqv? (ftype-ref Bbits (a2 a3 a2) x) (signed-bit-field A3 0 14))
  (eqv? (ftype-ref Bbits (a2 a4 a1) x) (signed-bit-field A4 29 48))
  (eqv? (ftype-ref Bbits (a2 a4 a2) x) (signed-bit-field A4 0 29))

  (eqv? (ftype-ref Bbits (a3 a1 a1) x) (unsigned-bit-field A1 23 24))
  (eqv? (ftype-ref Bbits (a3 a1 a2) x) (unsigned-bit-field A1 0 23))
  (eqv? (ftype-ref Bbits (a3 a2 a1) x) (unsigned-bit-field A2 37 40))
  (eqv? (ftype-ref Bbits (a3 a2 a2) x) (unsigned-bit-field A2 0 37))
  (eqv? (ftype-ref Bbits (a3 a3 a1) x) (unsigned-bit-field A3 14 56))
  (eqv? (ftype-ref Bbits (a3 a3 a2) x) (unsigned-bit-field A3 0 14))
  (eqv? (ftype-ref Bbits (a3 a4 a1) x) (unsigned-bit-field A4 29 48))
  (eqv? (ftype-ref Bbits (a3 a4 a2) x) (unsigned-bit-field A4 0 29))

  (begin
    (fptr-free x)
    #t)

)

(mat ftype-endian
  (equal?
    (let ()
      (define-ftype A (endian native double))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (free-after a
        (ftype-set! A () a 3.5)
        (ftype-ref A () a)))
    3.5)
  (equal?
    (let ()
      (define-ftype A (endian big double))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (free-after a
        (ftype-set! A () a 3.5)
        (ftype-ref A () a)))
    3.5)
  (equal?
    (let ()
      (define-ftype A (endian little double))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (free-after a
        (ftype-set! A () a 3.5)
        (ftype-ref A () a)))
    3.5)
  (equal?
    (let ()
       (define-ftype A
         (endian big
           (struct
             [a1 double]
             [a2 float]
             [a3 long-long]
             [a4 unsigned-long-long]
             [a5 long]
             [a6 unsigned-long]
             [a7 int]
             [a8 unsigned]
             [a9 unsigned-int]
             [a10 short]
             [a11 unsigned-short]
             [a12 wchar]
             [a13 char]
             [a14 boolean]
             [a15 fixnum]
             [a16 iptr]
             [a17 uptr]
             [a18 void*])))
       (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
       (free-after a
         (ftype-set! A (a1) a 3.5)
         (ftype-set! A (a2) a -4.5)
         (ftype-set! A (a3) a -30000)
         (ftype-set! A (a4) a #xabcdef02)
         (ftype-set! A (a5) a -30001)
         (ftype-set! A (a6) a #xabcdef03)
         (ftype-set! A (a7) a -30002)
         (ftype-set! A (a8) a #xabcdef04)
         (ftype-set! A (a9) a #xabcdef05)
         (ftype-set! A (a10) a -30003)
         (ftype-set! A (a11) a #xab06)
         (ftype-set! A (a12) a #\a)
         (ftype-set! A (a13) a #\b)
         (ftype-set! A (a14) a 'hello)
         (ftype-set! A (a15) a (most-positive-fixnum))
         (ftype-set! A (a16) a -30004)
         (ftype-set! A (a17) a #xabcdef07)
         (ftype-set! A (a18) a 25000)
         (list
           (ftype-ref A (a1) a)
           (ftype-ref A (a2) a)
           (ftype-ref A (a3) a)
           (ftype-ref A (a4) a)
           (ftype-ref A (a5) a)
           (ftype-ref A (a6) a)
           (ftype-ref A (a7) a)
           (ftype-ref A (a8) a)
           (ftype-ref A (a9) a)
           (ftype-ref A (a10) a)
           (ftype-ref A (a11) a)
           (ftype-ref A (a12) a)
           (ftype-ref A (a13) a)
           (ftype-ref A (a14) a)
           (ftype-ref A (a15) a)
           (ftype-ref A (a16) a)
           (ftype-ref A (a17) a)
           (ftype-ref A (a18) a))))
    `(3.5
      -4.5
      -30000
      #xabcdef02
      -30001
      #xabcdef03
      -30002
      #xabcdef04
      #xabcdef05
      -30003
      #xab06
      #\a
      #\b
      #t
      ,(most-positive-fixnum)
      -30004
      #xabcdef07
      25000))
  (equal?
    (let ()
       (define-ftype A
         (endian little
           (struct
             [a1 double]
             [a2 float]
             [a3 long-long]
             [a4 unsigned-long-long]
             [a5 long]
             [a6 unsigned-long]
             [a7 int]
             [a8 unsigned]
             [a9 unsigned-int]
             [a10 short]
             [a11 unsigned-short]
             [a12 wchar]
             [a13 char]
             [a14 boolean]
             [a15 fixnum]
             [a16 iptr]
             [a17 uptr]
             [a18 void*])))
       (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
       (free-after a
         (ftype-set! A (a1) a 3.5)
         (ftype-set! A (a2) a -4.5)
         (ftype-set! A (a3) a -30000)
         (ftype-set! A (a4) a #xabcdef02)
         (ftype-set! A (a5) a -30001)
         (ftype-set! A (a6) a #xabcdef03)
         (ftype-set! A (a7) a -30002)
         (ftype-set! A (a8) a #xabcdef04)
         (ftype-set! A (a9) a #xabcdef05)
         (ftype-set! A (a10) a -30003)
         (ftype-set! A (a11) a #xab06)
         (ftype-set! A (a12) a #\a)
         (ftype-set! A (a13) a #\b)
         (ftype-set! A (a14) a 'hello)
         (ftype-set! A (a15) a (most-positive-fixnum))
         (ftype-set! A (a16) a -30004)
         (ftype-set! A (a17) a #xabcdef07)
         (ftype-set! A (a18) a 25000)
         (list
           (ftype-ref A (a1) a)
           (ftype-ref A (a2) a)
           (ftype-ref A (a3) a)
           (ftype-ref A (a4) a)
           (ftype-ref A (a5) a)
           (ftype-ref A (a6) a)
           (ftype-ref A (a7) a)
           (ftype-ref A (a8) a)
           (ftype-ref A (a9) a)
           (ftype-ref A (a10) a)
           (ftype-ref A (a11) a)
           (ftype-ref A (a12) a)
           (ftype-ref A (a13) a)
           (ftype-ref A (a14) a)
           (ftype-ref A (a15) a)
           (ftype-ref A (a16) a)
           (ftype-ref A (a17) a)
           (ftype-ref A (a18) a))))
    `(3.5
      -4.5
      -30000
      #xabcdef02
      -30001
      #xabcdef03
      -30002
      #xabcdef04
      #xabcdef05
      -30003
      #xab06
      #\a
      #\b
      #t
      ,(most-positive-fixnum)
      -30004
      #xabcdef07
      25000))
  (equal?
    (let ()
       (define-ftype A
         (endian native
           (struct
             [a1 double]
             [a2 float]
             [a3 long-long]
             [a4 unsigned-long-long]
             [a5 long]
             [a6 unsigned-long]
             [a7 int]
             [a8 unsigned]
             [a9 unsigned-int]
             [a10 short]
             [a11 unsigned-short]
             [a12 wchar]
             [a13 char]
             [a14 boolean]
             [a15 fixnum]
             [a16 iptr]
             [a17 uptr]
             [a18 void*])))
       (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
       (free-after a
         (ftype-set! A (a1) a 3.5)
         (ftype-set! A (a2) a -4.5)
         (ftype-set! A (a3) a -30000)
         (ftype-set! A (a4) a #xabcdef02)
         (ftype-set! A (a5) a -30001)
         (ftype-set! A (a6) a #xabcdef03)
         (ftype-set! A (a7) a -30002)
         (ftype-set! A (a8) a #xabcdef04)
         (ftype-set! A (a9) a #xabcdef05)
         (ftype-set! A (a10) a -30003)
         (ftype-set! A (a11) a #xab06)
         (ftype-set! A (a12) a #\a)
         (ftype-set! A (a13) a #\b)
         (ftype-set! A (a14) a 'hello)
         (ftype-set! A (a15) a (most-positive-fixnum))
         (ftype-set! A (a16) a -30004)
         (ftype-set! A (a17) a #xabcdef07)
         (ftype-set! A (a18) a 25000)
         (list
           (ftype-ref A (a1) a)
           (ftype-ref A (a2) a)
           (ftype-ref A (a3) a)
           (ftype-ref A (a4) a)
           (ftype-ref A (a5) a)
           (ftype-ref A (a6) a)
           (ftype-ref A (a7) a)
           (ftype-ref A (a8) a)
           (ftype-ref A (a9) a)
           (ftype-ref A (a10) a)
           (ftype-ref A (a11) a)
           (ftype-ref A (a12) a)
           (ftype-ref A (a13) a)
           (ftype-ref A (a14) a)
           (ftype-ref A (a15) a)
           (ftype-ref A (a16) a)
           (ftype-ref A (a17) a)
           (ftype-ref A (a18) a))))
    `(3.5
      -4.5
      -30000
      #xabcdef02
      -30001
      #xabcdef03
      -30002
      #xabcdef04
      #xabcdef05
      -30003
      #xab06
      #\a
      #\b
      #t
      ,(most-positive-fixnum)
      -30004
      #xabcdef07
      25000))

 ; ----------------
  (begin
    (define-ftype Aendian
      (union
        [a1 (endian native
              (struct
                [a1 integer-64]
                [a2 integer-32]
                [a3 integer-16]))]
        [a2 (endian big
              (struct
                [a1 integer-64]
                [a2 integer-32]
                [a3 integer-16]))]
        [a3 (endian little
              (struct
                [a1 integer-64]
                [a2 integer-32]
                [a3 integer-16]))]))
    (define x (make-ftype-pointer Aendian (foreign-alloc (ftype-sizeof Aendian))))
    (define xcheck
      (lambda (x1 x2 x3)
        (define iswap
          (lambda (k n)
            (let ([n (if (< n 0) (+ (expt 2 k) n) n)])
              (do ([i 0 (fx+ i 8)]
                   [m 0 (logor (ash m 8) (bitwise-bit-field n i (+ i 8)))])
                ((fx= i k) (if (>= m (expt 2 (- k 1))) (- m (expt 2 k)) m))))))
        (define okay?
          (let ([s1 (iswap 64 x1)] [s2 (iswap 32 x2)] [s3 (iswap 16 x3)])
            (lambda (eness)
              (and
                (equal? (ftype-ref Aendian (a1 a1) x)
                        (if (eq? eness (native-endianness)) x1 s1))
                (equal? (ftype-ref Aendian (a1 a2) x)
                        (if (eq? eness (native-endianness)) x2 s2))
                (equal? (ftype-ref Aendian (a1 a3) x)
                        (if (eq? eness (native-endianness)) x3 s3))
                (equal? (ftype-ref Aendian (a2 a1) x)
                        (if (eq? eness 'big) x1 s1))
                (equal? (ftype-ref Aendian (a2 a2) x)
                        (if (eq? eness 'big) x2 s2))
                (equal? (ftype-ref Aendian (a2 a3) x)
                        (if (eq? eness 'big) x3 s3))
                (equal? (ftype-ref Aendian (a3 a1) x)
                        (if (eq? eness 'little) x1 s1))
                (equal? (ftype-ref Aendian (a3 a2) x)
                        (if (eq? eness 'little) x2 s2))
                (equal? (ftype-ref Aendian (a3 a3) x)
                        (if (eq? eness 'little) x3 s3))))))
        (and
          (begin
            (ftype-set! Aendian (a1 a1) x x1)
            (ftype-set! Aendian (a1 a2) x x2)
            (ftype-set! Aendian (a1 a3) x x3)
            (okay? (native-endianness)))
          (begin
            (ftype-set! Aendian (a2 a1) x x1)
            (ftype-set! Aendian (a2 a2) x x2)
            (ftype-set! Aendian (a2 a3) x x3)
            (okay? 'big))
          (begin
            (ftype-set! Aendian (a3 a1) x x1)
            (ftype-set! Aendian (a3 a2) x x2)
            (ftype-set! Aendian (a3 a3) x x3)
            (okay? 'little)))))
    #t)

  (xcheck 0 0 0)
  (xcheck -1 -1 -1)
  (xcheck 15 25 35)
  (xcheck -15 -25 -35)
  (xcheck #x123456780fedcba9 #x4ca97531 #x3efa)
  (xcheck #x-123456780fedcba9 #x-4ca97531 #x-3efa)

  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype Bendian
      (union
        [a1 (endian native
              (struct
                [a1 unsigned-64]
                [a2 unsigned-32]
                [a3 unsigned-16]))]
        [a2 (endian big
              (struct
                [a1 unsigned-64]
                [a2 unsigned-32]
                [a3 unsigned-16]))]
        [a3 (endian little
              (struct
                [a1 unsigned-64]
                [a2 unsigned-32]
                [a3 unsigned-16]))]))
    (define x (make-ftype-pointer Bendian (foreign-alloc (ftype-sizeof Bendian))))
    (define xcheck
      (lambda (x1 x2 x3)
        (define uswap
          (lambda (k n)
            (do ([i 0 (fx+ i 8)]
                 [m 0 (logor (ash m 8) (bitwise-bit-field n i (+ i 8)))])
              ((fx= i k) m))))
        (define okay?
          (let ([s1 (uswap 64 x1)] [s2 (uswap 32 x2)] [s3 (uswap 16 x3)])
            (lambda (eness)
              (and
                (equal? (ftype-ref Bendian (a1 a1) x)
                        (if (eq? eness (native-endianness)) x1 s1))
                (equal? (ftype-ref Bendian (a1 a2) x)
                        (if (eq? eness (native-endianness)) x2 s2))
                (equal? (ftype-ref Bendian (a1 a3) x)
                        (if (eq? eness (native-endianness)) x3 s3))
                (equal? (ftype-ref Bendian (a2 a1) x)
                        (if (eq? eness 'big) x1 s1))
                (equal? (ftype-ref Bendian (a2 a2) x)
                        (if (eq? eness 'big) x2 s2))
                (equal? (ftype-ref Bendian (a2 a3) x)
                        (if (eq? eness 'big) x3 s3))
                (equal? (ftype-ref Bendian (a3 a1) x)
                        (if (eq? eness 'little) x1 s1))
                (equal? (ftype-ref Bendian (a3 a2) x)
                        (if (eq? eness 'little) x2 s2))
                (equal? (ftype-ref Bendian (a3 a3) x)
                        (if (eq? eness 'little) x3 s3))))))
        (and
          (begin
            (ftype-set! Bendian (a1 a1) x x1)
            (ftype-set! Bendian (a1 a2) x x2)
            (ftype-set! Bendian (a1 a3) x x3)
            (okay? (native-endianness)))
          (begin
            (ftype-set! Bendian (a2 a1) x x1)
            (ftype-set! Bendian (a2 a2) x x2)
            (ftype-set! Bendian (a2 a3) x x3)
            (okay? 'big))
          (begin
            (ftype-set! Bendian (a3 a1) x x1)
            (ftype-set! Bendian (a3 a2) x x2)
            (ftype-set! Bendian (a3 a3) x x3)
            (okay? 'little)))))
    #t)

  (xcheck 0 0 0)
  (xcheck #xffffffffffffffff #xffffffff #xffff)
  (xcheck #x8000000000000015 #x80000025 #x8035)
  (xcheck #x123456780fedcba9 #x4ca97531 #x3efa)
  (xcheck #xf23456780fedcba9 #xdca97531 #x9efa)

  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype Abits
      (endian little
        (union
          [a1 (struct
                [a1 unsigned-32]
                [a2 unsigned-32]
                [a3 unsigned-32]
                [a4 unsigned-32]
                [a5 unsigned-32]
                [a6 unsigned-32]
                [a7 unsigned-32]
                [a8 unsigned-32]
                [a9 unsigned-32]
                [a10 unsigned-32]
                [a11 unsigned-32]
                [a12 unsigned-32]
                [a13 unsigned-32]
                [a14 unsigned-32]
                [a15 unsigned-32]
                [a16 unsigned-32]
                [a17 unsigned-32]
                [a18 unsigned-32]
                [a19 unsigned-32]
                [a20 unsigned-32]
                [a21 unsigned-32])]
          [a2 (struct
                [a1 (bits
                      [_ signed 4]
                      [a1 signed 1]
                      [a2 signed 2]
                      [a3 signed 3]
                      [a4 signed 4]
                      [a5 signed 5]
                      [a6 signed 6]
                      [a7 signed 7])]
                [a2 (bits
                      [_ signed 5]
                      [a8 signed 8]
                      [a9 signed 9]
                      [a10 signed 10])]
                [a3 (bits
                      [a11 signed 11]
                      [a12 signed 12]
                      [_ signed 9])]
                [a4 (bits
                      [a13 signed 13]
                      [_ signed 5]
                      [a14 signed 14])]
                [a5 (bits
                      [_ signed 1]
                      [a15 signed 15]
                      [a16 signed 16])]
                [a6 (bits [a17 signed 17] [_ signed 15])]
                [a7 (bits [_ signed 14] [a18 signed 18])]
                [a8 (bits [a19 signed 19] [_ signed 13])]
                [a9 (bits [_ signed 12] [a20 signed 20])]
                [a10 (bits [a21 signed 21] [_ signed 11])]
                [a11 (bits [_ signed 10] [a22 signed 22])]
                [a12 (bits [a23 signed 23] [_ signed 9])]
                [a13 (bits [_ signed 8] [a24 signed 24])]
                [a14 (bits [a25 signed 25] [_ signed 7])]
                [a15 (bits [_ signed 6] [a26 signed 26])]
                [a16 (bits [a27 signed 27] [_ signed 5])]
                [a17 (bits [_ signed 4] [a28 signed 28])]
                [a18 (bits [a29 signed 29] [_ signed 3])]
                [a19 (bits [_ signed 2] [a30 signed 30])]
                [a20 (bits [a31 signed 31] [_ signed 1])]
                [a21 (bits [a32 signed 32])])]
          [a3 (struct
                [a1 (bits
                      [_ unsigned 4]
                      [a1 unsigned 1]
                      [a2 unsigned 2]
                      [a3 unsigned 3]
                      [a4 unsigned 4]
                      [a5 unsigned 5]
                      [a6 unsigned 6]
                      [a7 unsigned 7])]
                [a2 (bits
                      [_ unsigned 5]
                      [a8 unsigned 8]
                      [a9 unsigned 9]
                      [a10 unsigned 10])]
                [a3 (bits
                      [a11 unsigned 11]
                      [a12 unsigned 12]
                      [_ unsigned 9])]
                [a4 (bits
                      [a13 unsigned 13]
                      [_ unsigned 5]
                      [a14 unsigned 14])]
                [a5 (bits
                      [_ unsigned 1]
                      [a15 unsigned 15]
                      [a16 unsigned 16])]
                [a6 (bits [a17 unsigned 17] [_ unsigned 15])]
                [a7 (bits [_ unsigned 14] [a18 unsigned 18])]
                [a8 (bits [a19 unsigned 19] [_ unsigned 13])]
                [a9 (bits [_ unsigned 12] [a20 unsigned 20])]
                [a10 (bits [a21 unsigned 21] [_ unsigned 11])]
                [a11 (bits [_ unsigned 10] [a22 unsigned 22])]
                [a12 (bits [a23 unsigned 23] [_ unsigned 9])]
                [a13 (bits [_ unsigned 8] [a24 unsigned 24])]
                [a14 (bits [a25 unsigned 25] [_ unsigned 7])]
                [a15 (bits [_ unsigned 6] [a26 unsigned 26])]
                [a16 (bits [a27 unsigned 27] [_ unsigned 5])]
                [a17 (bits [_ unsigned 4] [a28 unsigned 28])]
                [a18 (bits [a29 unsigned 29] [_ unsigned 3])]
                [a19 (bits [_ unsigned 2] [a30 unsigned 30])]
                [a20 (bits [a31 unsigned 31] [_ unsigned 1])]
                [a21 (bits [a32 unsigned 32])])])))
    (define x (make-ftype-pointer Abits (foreign-alloc (ftype-sizeof Abits))))
    (define (get-a1)
      (list
        (ftype-ref Abits (a1 a1) x)
        (ftype-ref Abits (a1 a2) x)
        (ftype-ref Abits (a1 a3) x)
        (ftype-ref Abits (a1 a4) x)
        (ftype-ref Abits (a1 a5) x)
        (ftype-ref Abits (a1 a6) x)
        (ftype-ref Abits (a1 a7) x)
        (ftype-ref Abits (a1 a8) x)
        (ftype-ref Abits (a1 a9) x)
        (ftype-ref Abits (a1 a10) x)
        (ftype-ref Abits (a1 a11) x)
        (ftype-ref Abits (a1 a12) x)
        (ftype-ref Abits (a1 a13) x)
        (ftype-ref Abits (a1 a14) x)
        (ftype-ref Abits (a1 a15) x)
        (ftype-ref Abits (a1 a16) x)
        (ftype-ref Abits (a1 a17) x)
        (ftype-ref Abits (a1 a18) x)
        (ftype-ref Abits (a1 a19) x)
        (ftype-ref Abits (a1 a20) x)
        (ftype-ref Abits (a1 a21) x)))
    (define (get-a2)
      (list
        (ftype-ref Abits (a2 a1 a1) x)
        (ftype-ref Abits (a2 a1 a2) x)
        (ftype-ref Abits (a2 a1 a3) x)
        (ftype-ref Abits (a2 a1 a4) x)
        (ftype-ref Abits (a2 a1 a5) x)
        (ftype-ref Abits (a2 a1 a6) x)
        (ftype-ref Abits (a2 a1 a7) x)
        (ftype-ref Abits (a2 a2 a8) x)
        (ftype-ref Abits (a2 a2 a9) x)
        (ftype-ref Abits (a2 a2 a10) x)
        (ftype-ref Abits (a2 a3 a11) x)
        (ftype-ref Abits (a2 a3 a12) x)
        (ftype-ref Abits (a2 a4 a13) x)
        (ftype-ref Abits (a2 a4 a14) x)
        (ftype-ref Abits (a2 a5 a15) x)
        (ftype-ref Abits (a2 a5 a16) x)
        (ftype-ref Abits (a2 a6 a17) x)
        (ftype-ref Abits (a2 a7 a18) x)
        (ftype-ref Abits (a2 a8 a19) x)
        (ftype-ref Abits (a2 a9 a20) x)
        (ftype-ref Abits (a2 a10 a21) x)
        (ftype-ref Abits (a2 a11 a22) x)
        (ftype-ref Abits (a2 a12 a23) x)
        (ftype-ref Abits (a2 a13 a24) x)
        (ftype-ref Abits (a2 a14 a25) x)
        (ftype-ref Abits (a2 a15 a26) x)
        (ftype-ref Abits (a2 a16 a27) x)
        (ftype-ref Abits (a2 a17 a28) x)
        (ftype-ref Abits (a2 a18 a29) x)
        (ftype-ref Abits (a2 a19 a30) x)
        (ftype-ref Abits (a2 a20 a31) x)
        (ftype-ref Abits (a2 a21 a32) x)))
    (define (get-a3)
      (list
        (ftype-ref Abits (a3 a1 a1) x)
        (ftype-ref Abits (a3 a1 a2) x)
        (ftype-ref Abits (a3 a1 a3) x)
        (ftype-ref Abits (a3 a1 a4) x)
        (ftype-ref Abits (a3 a1 a5) x)
        (ftype-ref Abits (a3 a1 a6) x)
        (ftype-ref Abits (a3 a1 a7) x)
        (ftype-ref Abits (a3 a2 a8) x)
        (ftype-ref Abits (a3 a2 a9) x)
        (ftype-ref Abits (a3 a2 a10) x)
        (ftype-ref Abits (a3 a3 a11) x)
        (ftype-ref Abits (a3 a3 a12) x)
        (ftype-ref Abits (a3 a4 a13) x)
        (ftype-ref Abits (a3 a4 a14) x)
        (ftype-ref Abits (a3 a5 a15) x)
        (ftype-ref Abits (a3 a5 a16) x)
        (ftype-ref Abits (a3 a6 a17) x)
        (ftype-ref Abits (a3 a7 a18) x)
        (ftype-ref Abits (a3 a8 a19) x)
        (ftype-ref Abits (a3 a9 a20) x)
        (ftype-ref Abits (a3 a10 a21) x)
        (ftype-ref Abits (a3 a11 a22) x)
        (ftype-ref Abits (a3 a12 a23) x)
        (ftype-ref Abits (a3 a13 a24) x)
        (ftype-ref Abits (a3 a14 a25) x)
        (ftype-ref Abits (a3 a15 a26) x)
        (ftype-ref Abits (a3 a16 a27) x)
        (ftype-ref Abits (a3 a17 a28) x)
        (ftype-ref Abits (a3 a18 a29) x)
        (ftype-ref Abits (a3 a19 a30) x)
        (ftype-ref Abits (a3 a20 a31) x)
        (ftype-ref Abits (a3 a21 a32) x)))
    (define (set-a1! ls)
      (map
        (lambda (f v) (f v))
        (list
          (lambda (v) (ftype-set! Abits (a1 a1) x v))
          (lambda (v) (ftype-set! Abits (a1 a2) x v))
          (lambda (v) (ftype-set! Abits (a1 a3) x v))
          (lambda (v) (ftype-set! Abits (a1 a4) x v))
          (lambda (v) (ftype-set! Abits (a1 a5) x v))
          (lambda (v) (ftype-set! Abits (a1 a6) x v))
          (lambda (v) (ftype-set! Abits (a1 a7) x v))
          (lambda (v) (ftype-set! Abits (a1 a8) x v))
          (lambda (v) (ftype-set! Abits (a1 a9) x v))
          (lambda (v) (ftype-set! Abits (a1 a10) x v))
          (lambda (v) (ftype-set! Abits (a1 a11) x v))
          (lambda (v) (ftype-set! Abits (a1 a12) x v))
          (lambda (v) (ftype-set! Abits (a1 a13) x v))
          (lambda (v) (ftype-set! Abits (a1 a14) x v))
          (lambda (v) (ftype-set! Abits (a1 a15) x v))
          (lambda (v) (ftype-set! Abits (a1 a16) x v))
          (lambda (v) (ftype-set! Abits (a1 a17) x v))
          (lambda (v) (ftype-set! Abits (a1 a18) x v))
          (lambda (v) (ftype-set! Abits (a1 a19) x v))
          (lambda (v) (ftype-set! Abits (a1 a20) x v))
          (lambda (v) (ftype-set! Abits (a1 a21) x v)))
        ls))
    (define (set-a2! ls)
      (map
        (lambda (f v) (f v))
        (list
          (lambda (v) (ftype-set! Abits (a2 a1 a1) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a2) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a3) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a4) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a5) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a6) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a7) x v))
          (lambda (v) (ftype-set! Abits (a2 a2 a8) x v))
          (lambda (v) (ftype-set! Abits (a2 a2 a9) x v))
          (lambda (v) (ftype-set! Abits (a2 a2 a10) x v))
          (lambda (v) (ftype-set! Abits (a2 a3 a11) x v))
          (lambda (v) (ftype-set! Abits (a2 a3 a12) x v))
          (lambda (v) (ftype-set! Abits (a2 a4 a13) x v))
          (lambda (v) (ftype-set! Abits (a2 a4 a14) x v))
          (lambda (v) (ftype-set! Abits (a2 a5 a15) x v))
          (lambda (v) (ftype-set! Abits (a2 a5 a16) x v))
          (lambda (v) (ftype-set! Abits (a2 a6 a17) x v))
          (lambda (v) (ftype-set! Abits (a2 a7 a18) x v))
          (lambda (v) (ftype-set! Abits (a2 a8 a19) x v))
          (lambda (v) (ftype-set! Abits (a2 a9 a20) x v))
          (lambda (v) (ftype-set! Abits (a2 a10 a21) x v))
          (lambda (v) (ftype-set! Abits (a2 a11 a22) x v))
          (lambda (v) (ftype-set! Abits (a2 a12 a23) x v))
          (lambda (v) (ftype-set! Abits (a2 a13 a24) x v))
          (lambda (v) (ftype-set! Abits (a2 a14 a25) x v))
          (lambda (v) (ftype-set! Abits (a2 a15 a26) x v))
          (lambda (v) (ftype-set! Abits (a2 a16 a27) x v))
          (lambda (v) (ftype-set! Abits (a2 a17 a28) x v))
          (lambda (v) (ftype-set! Abits (a2 a18 a29) x v))
          (lambda (v) (ftype-set! Abits (a2 a19 a30) x v))
          (lambda (v) (ftype-set! Abits (a2 a20 a31) x v))
          (lambda (v) (ftype-set! Abits (a2 a21 a32) x v)))
        ls))
    (define (set-a3! ls)
      (map
        (lambda (f v) (f v))
        (list
          (lambda (v) (ftype-set! Abits (a3 a1 a1) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a2) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a3) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a4) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a5) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a6) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a7) x v))
          (lambda (v) (ftype-set! Abits (a3 a2 a8) x v))
          (lambda (v) (ftype-set! Abits (a3 a2 a9) x v))
          (lambda (v) (ftype-set! Abits (a3 a2 a10) x v))
          (lambda (v) (ftype-set! Abits (a3 a3 a11) x v))
          (lambda (v) (ftype-set! Abits (a3 a3 a12) x v))
          (lambda (v) (ftype-set! Abits (a3 a4 a13) x v))
          (lambda (v) (ftype-set! Abits (a3 a4 a14) x v))
          (lambda (v) (ftype-set! Abits (a3 a5 a15) x v))
          (lambda (v) (ftype-set! Abits (a3 a5 a16) x v))
          (lambda (v) (ftype-set! Abits (a3 a6 a17) x v))
          (lambda (v) (ftype-set! Abits (a3 a7 a18) x v))
          (lambda (v) (ftype-set! Abits (a3 a8 a19) x v))
          (lambda (v) (ftype-set! Abits (a3 a9 a20) x v))
          (lambda (v) (ftype-set! Abits (a3 a10 a21) x v))
          (lambda (v) (ftype-set! Abits (a3 a11 a22) x v))
          (lambda (v) (ftype-set! Abits (a3 a12 a23) x v))
          (lambda (v) (ftype-set! Abits (a3 a13 a24) x v))
          (lambda (v) (ftype-set! Abits (a3 a14 a25) x v))
          (lambda (v) (ftype-set! Abits (a3 a15 a26) x v))
          (lambda (v) (ftype-set! Abits (a3 a16 a27) x v))
          (lambda (v) (ftype-set! Abits (a3 a17 a28) x v))
          (lambda (v) (ftype-set! Abits (a3 a18 a29) x v))
          (lambda (v) (ftype-set! Abits (a3 a19 a30) x v))
          (lambda (v) (ftype-set! Abits (a3 a20 a31) x v))
          (lambda (v) (ftype-set! Abits (a3 a21 a32) x v)))
        ls))
    (define a3-c7c7c7c7
      '(#b0
        #b10
        #b111
        #b0001
        #b11111
        #b111000
        #b1100011
        #b00111110
        #b000111110
        #b1100011111
        #b11111000111
        #b100011111000
        #b0011111000111
        #b11000111110001
        #b110001111100011
        #b1100011111000111
        #b11100011111000111
        #b110001111100011111
        #b1111100011111000111
        #b11000111110001111100
        #b001111100011111000111
        #b1100011111000111110001
        #b10001111100011111000111
        #b110001111100011111000111
        #b1110001111100011111000111
        #b11000111110001111100011111
        #b111110001111100011111000111
        #b1100011111000111110001111100
        #b00111110001111100011111000111
        #b110001111100011111000111110001
        #b1000111110001111100011111000111
        #b11000111110001111100011111000111))
    (define a3-13579bdf
      '(#b1
        #b10
        #b111
        #b0110
        #b11110
        #b101010
        #b0001001
        #b11011110
        #b010111100
        #b0001001101
        #b01111011111
        #b101011110011
        #b1101111011111
        #b00010011010101
        #b100110111101111
        #b0001001101010111
        #b11001101111011111
        #b000100110101011110
        #b1111001101111011111
        #b00010011010101111001
        #b101111001101111011111
        #b0001001101010111100110
        #b10101111001101111011111
        #b000100110101011110011011
        #b1010101111001101111011111
        #b00010011010101111001101111
        #b011010101111001101111011111
        #b0001001101010111100110111101
        #b10011010101111001101111011111
        #b000100110101011110011011110111
        #b0010011010101111001101111011111
        #b00010011010101111001101111011111))
    (define a2-from-a3
      (lambda (ls)
        (map (lambda (i n)
               (let* ([radix/2 (expt 2 i)])
                 (if (>= n radix/2)
                     (- n (ash radix/2 1))
                     n)))
          (enumerate ls) ls)))
    #t)
  (begin
    (set-a1! (make-list 21 0))
    #t)
  (equal?
    (get-a2)
    (make-list 32 0))
  (equal?
    (get-a3)
    (make-list 32 0))
  (begin
    (set-a1! (make-list 21 #xffffffff))
    #t)
  (equal?
    (get-a2)
    (make-list 32 -1))
  (equal?
    (get-a3)
    (do ([n 32 (fx- n 1)]
         [ls '() (cons (- (expt 2 n) 1) ls)])
      ((= n 0) ls)))
  (begin
    (set-a1! (make-list 21 #xc7c7c7c7))
    #t)
  (equal?
    (get-a3)
    a3-c7c7c7c7)
  (equal?
    (get-a2)
    (a2-from-a3 a3-c7c7c7c7))
  (begin
    (ftype-set! Abits (a1 a1) x #x13579bdf)
    (ftype-set! Abits (a1 a2) x #x13579bdf)
    (ftype-set! Abits (a1 a3) x #x13579bdf)
    (ftype-set! Abits (a1 a4) x #x13579bdf)
    (ftype-set! Abits (a1 a5) x #x13579bdf)
    (ftype-set! Abits (a1 a6) x #x13579bdf)
    (ftype-set! Abits (a1 a7) x #x13579bdf)
    (ftype-set! Abits (a1 a8) x #x13579bdf)
    (ftype-set! Abits (a1 a9) x #x13579bdf)
    (ftype-set! Abits (a1 a10) x #x13579bdf)
    (ftype-set! Abits (a1 a11) x #x13579bdf)
    (ftype-set! Abits (a1 a12) x #x13579bdf)
    (ftype-set! Abits (a1 a13) x #x13579bdf)
    (ftype-set! Abits (a1 a14) x #x13579bdf)
    (ftype-set! Abits (a1 a15) x #x13579bdf)
    (ftype-set! Abits (a1 a16) x #x13579bdf)
    (ftype-set! Abits (a1 a17) x #x13579bdf)
    (ftype-set! Abits (a1 a18) x #x13579bdf)
    (ftype-set! Abits (a1 a19) x #x13579bdf)
    (ftype-set! Abits (a1 a20) x #x13579bdf)
    (ftype-set! Abits (a1 a21) x #x13579bdf)
    #t)
  (equal?
    (get-a3)
    a3-13579bdf)
  (equal?
    (get-a2)
    (a2-from-a3 a3-13579bdf))
  (begin
    (set-a1! (make-list 21 0))
    (set-a3! a3-c7c7c7c7)
    #t)
  (equal?
    (get-a3)
    a3-c7c7c7c7)
  (equal?
    (get-a2)
    (a2-from-a3 a3-c7c7c7c7))
  (equal?
    (get-a1)
    '(#xc7c7c7c0
      #xc7c7c7c0
      #x0047c7c7
      #xc7c407c7
      #xc7c7c7c6
      #x0001c7c7
      #xc7c7c000
      #x0007c7c7
      #xc7c7c000
      #x0007c7c7
      #xc7c7c400
      #x0047c7c7
      #xc7c7c700
      #x01c7c7c7
      #xc7c7c7c0
      #x07c7c7c7
      #xc7c7c7c0
      #x07c7c7c7
      #xc7c7c7c4
      #x47c7c7c7
      #xc7c7c7c7))
  (begin
    (set-a1! (make-list 21 0))
    (set-a2! (a2-from-a3 a3-13579bdf))
    #t)
  (equal?
    (get-a3)
    a3-13579bdf)
  (equal?
    (get-a2)
    (a2-from-a3 a3-13579bdf))
  (equal?
    (get-a1)
    '(#x13579bd0
      #x13579bc0
      #x00579bdf
      #x13541bdf
      #x13579bde
      #x00019bdf
      #x13578000
      #x00079bdf
      #x13579000
      #x00179bdf
      #x13579800
      #x00579bdf
      #x13579b00
      #x01579bdf
      #x13579bc0
      #x03579bdf
      #x13579bd0
      #x13579bdf
      #x13579bdc
      #x13579bdf
      #x13579bdf))

  (begin
    (fptr-free x)
    #t)

 ; ----------------
  (begin
    (define-ftype Abits
      (endian big
        (union
          [a1 (struct
                [a1 unsigned-32]
                [a2 unsigned-32]
                [a3 unsigned-32]
                [a4 unsigned-32]
                [a5 unsigned-32]
                [a6 unsigned-32]
                [a7 unsigned-32]
                [a8 unsigned-32]
                [a9 unsigned-32]
                [a10 unsigned-32]
                [a11 unsigned-32]
                [a12 unsigned-32]
                [a13 unsigned-32]
                [a14 unsigned-32]
                [a15 unsigned-32]
                [a16 unsigned-32]
                [a17 unsigned-32]
                [a18 unsigned-32]
                [a19 unsigned-32]
                [a20 unsigned-32]
                [a21 unsigned-32])]
          [a2 (struct
                [a1 (bits
                      [_ signed 4]
                      [a1 signed 1]
                      [a2 signed 2]
                      [a3 signed 3]
                      [a4 signed 4]
                      [a5 signed 5]
                      [a6 signed 6]
                      [a7 signed 7])]
                [a2 (bits
                      [_ signed 5]
                      [a8 signed 8]
                      [a9 signed 9]
                      [a10 signed 10])]
                [a3 (bits
                      [a11 signed 11]
                      [a12 signed 12]
                      [_ signed 9])]
                [a4 (bits
                      [a13 signed 13]
                      [_ signed 5]
                      [a14 signed 14])]
                [a5 (bits
                      [_ signed 1]
                      [a15 signed 15]
                      [a16 signed 16])]
                [a6 (bits [a17 signed 17] [_ signed 15])]
                [a7 (bits [_ signed 14] [a18 signed 18])]
                [a8 (bits [a19 signed 19] [_ signed 13])]
                [a9 (bits [_ signed 12] [a20 signed 20])]
                [a10 (bits [a21 signed 21] [_ signed 11])]
                [a11 (bits [_ signed 10] [a22 signed 22])]
                [a12 (bits [a23 signed 23] [_ signed 9])]
                [a13 (bits [_ signed 8] [a24 signed 24])]
                [a14 (bits [a25 signed 25] [_ signed 7])]
                [a15 (bits [_ signed 6] [a26 signed 26])]
                [a16 (bits [a27 signed 27] [_ signed 5])]
                [a17 (bits [_ signed 4] [a28 signed 28])]
                [a18 (bits [a29 signed 29] [_ signed 3])]
                [a19 (bits [_ signed 2] [a30 signed 30])]
                [a20 (bits [a31 signed 31] [_ signed 1])]
                [a21 (bits [a32 signed 32])])]
          [a3 (struct
                [a1 (bits
                      [_ unsigned 4]
                      [a1 unsigned 1]
                      [a2 unsigned 2]
                      [a3 unsigned 3]
                      [a4 unsigned 4]
                      [a5 unsigned 5]
                      [a6 unsigned 6]
                      [a7 unsigned 7])]
                [a2 (bits
                      [_ unsigned 5]
                      [a8 unsigned 8]
                      [a9 unsigned 9]
                      [a10 unsigned 10])]
                [a3 (bits
                      [a11 unsigned 11]
                      [a12 unsigned 12]
                      [_ unsigned 9])]
                [a4 (bits
                      [a13 unsigned 13]
                      [_ unsigned 5]
                      [a14 unsigned 14])]
                [a5 (bits
                      [_ unsigned 1]
                      [a15 unsigned 15]
                      [a16 unsigned 16])]
                [a6 (bits [a17 unsigned 17] [_ unsigned 15])]
                [a7 (bits [_ unsigned 14] [a18 unsigned 18])]
                [a8 (bits [a19 unsigned 19] [_ unsigned 13])]
                [a9 (bits [_ unsigned 12] [a20 unsigned 20])]
                [a10 (bits [a21 unsigned 21] [_ unsigned 11])]
                [a11 (bits [_ unsigned 10] [a22 unsigned 22])]
                [a12 (bits [a23 unsigned 23] [_ unsigned 9])]
                [a13 (bits [_ unsigned 8] [a24 unsigned 24])]
                [a14 (bits [a25 unsigned 25] [_ unsigned 7])]
                [a15 (bits [_ unsigned 6] [a26 unsigned 26])]
                [a16 (bits [a27 unsigned 27] [_ unsigned 5])]
                [a17 (bits [_ unsigned 4] [a28 unsigned 28])]
                [a18 (bits [a29 unsigned 29] [_ unsigned 3])]
                [a19 (bits [_ unsigned 2] [a30 unsigned 30])]
                [a20 (bits [a31 unsigned 31] [_ unsigned 1])]
                [a21 (bits [a32 unsigned 32])])])))
    (define x (make-ftype-pointer Abits (foreign-alloc (ftype-sizeof Abits))))
    (define (get-a1)
      (list
        (ftype-ref Abits (a1 a1) x)
        (ftype-ref Abits (a1 a2) x)
        (ftype-ref Abits (a1 a3) x)
        (ftype-ref Abits (a1 a4) x)
        (ftype-ref Abits (a1 a5) x)
        (ftype-ref Abits (a1 a6) x)
        (ftype-ref Abits (a1 a7) x)
        (ftype-ref Abits (a1 a8) x)
        (ftype-ref Abits (a1 a9) x)
        (ftype-ref Abits (a1 a10) x)
        (ftype-ref Abits (a1 a11) x)
        (ftype-ref Abits (a1 a12) x)
        (ftype-ref Abits (a1 a13) x)
        (ftype-ref Abits (a1 a14) x)
        (ftype-ref Abits (a1 a15) x)
        (ftype-ref Abits (a1 a16) x)
        (ftype-ref Abits (a1 a17) x)
        (ftype-ref Abits (a1 a18) x)
        (ftype-ref Abits (a1 a19) x)
        (ftype-ref Abits (a1 a20) x)
        (ftype-ref Abits (a1 a21) x)))
    (define (get-a2)
      (list
        (ftype-ref Abits (a2 a1 a1) x)
        (ftype-ref Abits (a2 a1 a2) x)
        (ftype-ref Abits (a2 a1 a3) x)
        (ftype-ref Abits (a2 a1 a4) x)
        (ftype-ref Abits (a2 a1 a5) x)
        (ftype-ref Abits (a2 a1 a6) x)
        (ftype-ref Abits (a2 a1 a7) x)
        (ftype-ref Abits (a2 a2 a8) x)
        (ftype-ref Abits (a2 a2 a9) x)
        (ftype-ref Abits (a2 a2 a10) x)
        (ftype-ref Abits (a2 a3 a11) x)
        (ftype-ref Abits (a2 a3 a12) x)
        (ftype-ref Abits (a2 a4 a13) x)
        (ftype-ref Abits (a2 a4 a14) x)
        (ftype-ref Abits (a2 a5 a15) x)
        (ftype-ref Abits (a2 a5 a16) x)
        (ftype-ref Abits (a2 a6 a17) x)
        (ftype-ref Abits (a2 a7 a18) x)
        (ftype-ref Abits (a2 a8 a19) x)
        (ftype-ref Abits (a2 a9 a20) x)
        (ftype-ref Abits (a2 a10 a21) x)
        (ftype-ref Abits (a2 a11 a22) x)
        (ftype-ref Abits (a2 a12 a23) x)
        (ftype-ref Abits (a2 a13 a24) x)
        (ftype-ref Abits (a2 a14 a25) x)
        (ftype-ref Abits (a2 a15 a26) x)
        (ftype-ref Abits (a2 a16 a27) x)
        (ftype-ref Abits (a2 a17 a28) x)
        (ftype-ref Abits (a2 a18 a29) x)
        (ftype-ref Abits (a2 a19 a30) x)
        (ftype-ref Abits (a2 a20 a31) x)
        (ftype-ref Abits (a2 a21 a32) x)))
    (define (get-a3)
      (list
        (ftype-ref Abits (a3 a1 a1) x)
        (ftype-ref Abits (a3 a1 a2) x)
        (ftype-ref Abits (a3 a1 a3) x)
        (ftype-ref Abits (a3 a1 a4) x)
        (ftype-ref Abits (a3 a1 a5) x)
        (ftype-ref Abits (a3 a1 a6) x)
        (ftype-ref Abits (a3 a1 a7) x)
        (ftype-ref Abits (a3 a2 a8) x)
        (ftype-ref Abits (a3 a2 a9) x)
        (ftype-ref Abits (a3 a2 a10) x)
        (ftype-ref Abits (a3 a3 a11) x)
        (ftype-ref Abits (a3 a3 a12) x)
        (ftype-ref Abits (a3 a4 a13) x)
        (ftype-ref Abits (a3 a4 a14) x)
        (ftype-ref Abits (a3 a5 a15) x)
        (ftype-ref Abits (a3 a5 a16) x)
        (ftype-ref Abits (a3 a6 a17) x)
        (ftype-ref Abits (a3 a7 a18) x)
        (ftype-ref Abits (a3 a8 a19) x)
        (ftype-ref Abits (a3 a9 a20) x)
        (ftype-ref Abits (a3 a10 a21) x)
        (ftype-ref Abits (a3 a11 a22) x)
        (ftype-ref Abits (a3 a12 a23) x)
        (ftype-ref Abits (a3 a13 a24) x)
        (ftype-ref Abits (a3 a14 a25) x)
        (ftype-ref Abits (a3 a15 a26) x)
        (ftype-ref Abits (a3 a16 a27) x)
        (ftype-ref Abits (a3 a17 a28) x)
        (ftype-ref Abits (a3 a18 a29) x)
        (ftype-ref Abits (a3 a19 a30) x)
        (ftype-ref Abits (a3 a20 a31) x)
        (ftype-ref Abits (a3 a21 a32) x)))
    (define (set-a1! ls)
      (map
        (lambda (f v) (f v))
        (list
          (lambda (v) (ftype-set! Abits (a1 a1) x v))
          (lambda (v) (ftype-set! Abits (a1 a2) x v))
          (lambda (v) (ftype-set! Abits (a1 a3) x v))
          (lambda (v) (ftype-set! Abits (a1 a4) x v))
          (lambda (v) (ftype-set! Abits (a1 a5) x v))
          (lambda (v) (ftype-set! Abits (a1 a6) x v))
          (lambda (v) (ftype-set! Abits (a1 a7) x v))
          (lambda (v) (ftype-set! Abits (a1 a8) x v))
          (lambda (v) (ftype-set! Abits (a1 a9) x v))
          (lambda (v) (ftype-set! Abits (a1 a10) x v))
          (lambda (v) (ftype-set! Abits (a1 a11) x v))
          (lambda (v) (ftype-set! Abits (a1 a12) x v))
          (lambda (v) (ftype-set! Abits (a1 a13) x v))
          (lambda (v) (ftype-set! Abits (a1 a14) x v))
          (lambda (v) (ftype-set! Abits (a1 a15) x v))
          (lambda (v) (ftype-set! Abits (a1 a16) x v))
          (lambda (v) (ftype-set! Abits (a1 a17) x v))
          (lambda (v) (ftype-set! Abits (a1 a18) x v))
          (lambda (v) (ftype-set! Abits (a1 a19) x v))
          (lambda (v) (ftype-set! Abits (a1 a20) x v))
          (lambda (v) (ftype-set! Abits (a1 a21) x v)))
        ls))
    (define (set-a2! ls)
      (map
        (lambda (f v) (f v))
        (list
          (lambda (v) (ftype-set! Abits (a2 a1 a1) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a2) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a3) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a4) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a5) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a6) x v))
          (lambda (v) (ftype-set! Abits (a2 a1 a7) x v))
          (lambda (v) (ftype-set! Abits (a2 a2 a8) x v))
          (lambda (v) (ftype-set! Abits (a2 a2 a9) x v))
          (lambda (v) (ftype-set! Abits (a2 a2 a10) x v))
          (lambda (v) (ftype-set! Abits (a2 a3 a11) x v))
          (lambda (v) (ftype-set! Abits (a2 a3 a12) x v))
          (lambda (v) (ftype-set! Abits (a2 a4 a13) x v))
          (lambda (v) (ftype-set! Abits (a2 a4 a14) x v))
          (lambda (v) (ftype-set! Abits (a2 a5 a15) x v))
          (lambda (v) (ftype-set! Abits (a2 a5 a16) x v))
          (lambda (v) (ftype-set! Abits (a2 a6 a17) x v))
          (lambda (v) (ftype-set! Abits (a2 a7 a18) x v))
          (lambda (v) (ftype-set! Abits (a2 a8 a19) x v))
          (lambda (v) (ftype-set! Abits (a2 a9 a20) x v))
          (lambda (v) (ftype-set! Abits (a2 a10 a21) x v))
          (lambda (v) (ftype-set! Abits (a2 a11 a22) x v))
          (lambda (v) (ftype-set! Abits (a2 a12 a23) x v))
          (lambda (v) (ftype-set! Abits (a2 a13 a24) x v))
          (lambda (v) (ftype-set! Abits (a2 a14 a25) x v))
          (lambda (v) (ftype-set! Abits (a2 a15 a26) x v))
          (lambda (v) (ftype-set! Abits (a2 a16 a27) x v))
          (lambda (v) (ftype-set! Abits (a2 a17 a28) x v))
          (lambda (v) (ftype-set! Abits (a2 a18 a29) x v))
          (lambda (v) (ftype-set! Abits (a2 a19 a30) x v))
          (lambda (v) (ftype-set! Abits (a2 a20 a31) x v))
          (lambda (v) (ftype-set! Abits (a2 a21 a32) x v)))
        ls))
    (define (set-a3! ls)
      (map
        (lambda (f v) (f v))
        (list
          (lambda (v) (ftype-set! Abits (a3 a1 a1) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a2) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a3) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a4) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a5) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a6) x v))
          (lambda (v) (ftype-set! Abits (a3 a1 a7) x v))
          (lambda (v) (ftype-set! Abits (a3 a2 a8) x v))
          (lambda (v) (ftype-set! Abits (a3 a2 a9) x v))
          (lambda (v) (ftype-set! Abits (a3 a2 a10) x v))
          (lambda (v) (ftype-set! Abits (a3 a3 a11) x v))
          (lambda (v) (ftype-set! Abits (a3 a3 a12) x v))
          (lambda (v) (ftype-set! Abits (a3 a4 a13) x v))
          (lambda (v) (ftype-set! Abits (a3 a4 a14) x v))
          (lambda (v) (ftype-set! Abits (a3 a5 a15) x v))
          (lambda (v) (ftype-set! Abits (a3 a5 a16) x v))
          (lambda (v) (ftype-set! Abits (a3 a6 a17) x v))
          (lambda (v) (ftype-set! Abits (a3 a7 a18) x v))
          (lambda (v) (ftype-set! Abits (a3 a8 a19) x v))
          (lambda (v) (ftype-set! Abits (a3 a9 a20) x v))
          (lambda (v) (ftype-set! Abits (a3 a10 a21) x v))
          (lambda (v) (ftype-set! Abits (a3 a11 a22) x v))
          (lambda (v) (ftype-set! Abits (a3 a12 a23) x v))
          (lambda (v) (ftype-set! Abits (a3 a13 a24) x v))
          (lambda (v) (ftype-set! Abits (a3 a14 a25) x v))
          (lambda (v) (ftype-set! Abits (a3 a15 a26) x v))
          (lambda (v) (ftype-set! Abits (a3 a16 a27) x v))
          (lambda (v) (ftype-set! Abits (a3 a17 a28) x v))
          (lambda (v) (ftype-set! Abits (a3 a18 a29) x v))
          (lambda (v) (ftype-set! Abits (a3 a19 a30) x v))
          (lambda (v) (ftype-set! Abits (a3 a20 a31) x v))
          (lambda (v) (ftype-set! Abits (a3 a21 a32) x v)))
        ls))
    (define a3-c7c7c7c7
      '(#b0
        #b11
        #b111
        #b0001
        #b11110
        #b001111
        #b1000111
        #b11111000
        #b111110001
        #b1111000111
        #b11000111110
        #b001111100011
        #b1100011111000
        #b00011111000111
        #b100011111000111
        #b1100011111000111
        #b11000111110001111
        #b111100011111000111
        #b1100011111000111110
        #b01111100011111000111
        #b110001111100011111000
        #b0001111100011111000111
        #b11000111110001111100011
        #b110001111100011111000111
        #b1100011111000111110001111
        #b11110001111100011111000111
        #b110001111100011111000111110
        #b0111110001111100011111000111
        #b11000111110001111100011111000
        #b000111110001111100011111000111
        #b1100011111000111110001111100011
        #b11000111110001111100011111000111))
    (define a3-13579bdf
      '(#b0
        #b01
        #b101
        #b0101
        #b11100
        #b110111
        #b1011111
        #b01101010
        #b111100110
        #b1111011111
        #b00010011010
        #b101111001101
        #b0001001101010
        #b01101111011111
        #b001001101010111
        #b1001101111011111
        #b00010011010101111
        #b111001101111011111
        #b0001001101010111100
        #b01111001101111011111
        #b000100110101011110011
        #b0101111001101111011111
        #b00010011010101111001101
        #b010101111001101111011111
        #b0001001101010111100110111
        #b11010101111001101111011111
        #b000100110101011110011011110
        #b0011010101111001101111011111
        #b00010011010101111001101111011
        #b010011010101111001101111011111
        #b0001001101010111100110111101111
        #b00010011010101111001101111011111))
    (define a2-from-a3
      (lambda (ls)
        (map (lambda (i n)
               (let* ([radix/2 (expt 2 i)])
                 (if (>= n radix/2)
                     (- n (ash radix/2 1))
                     n)))
          (enumerate ls) ls)))
    #t)
  (begin
    (set-a1! (make-list 21 0))
    #t)
  (equal?
    (get-a2)
    (make-list 32 0))
  (equal?
    (get-a3)
    (make-list 32 0))
  (begin
    (set-a1! (make-list 21 #xffffffff))
    #t)
  (equal?
    (get-a2)
    (make-list 32 -1))
  (equal?
    (get-a3)
    (do ([n 32 (fx- n 1)]
         [ls '() (cons (- (expt 2 n) 1) ls)])
      ((= n 0) ls)))
  (begin
    (set-a1! (make-list 21 #xc7c7c7c7))
    #t)
  (equal?
    (get-a3)
    a3-c7c7c7c7)
  (equal?
    (get-a2)
    (a2-from-a3 a3-c7c7c7c7))
  (begin
    (ftype-set! Abits (a1 a1) x #x13579bdf)
    (ftype-set! Abits (a1 a2) x #x13579bdf)
    (ftype-set! Abits (a1 a3) x #x13579bdf)
    (ftype-set! Abits (a1 a4) x #x13579bdf)
    (ftype-set! Abits (a1 a5) x #x13579bdf)
    (ftype-set! Abits (a1 a6) x #x13579bdf)
    (ftype-set! Abits (a1 a7) x #x13579bdf)
    (ftype-set! Abits (a1 a8) x #x13579bdf)
    (ftype-set! Abits (a1 a9) x #x13579bdf)
    (ftype-set! Abits (a1 a10) x #x13579bdf)
    (ftype-set! Abits (a1 a11) x #x13579bdf)
    (ftype-set! Abits (a1 a12) x #x13579bdf)
    (ftype-set! Abits (a1 a13) x #x13579bdf)
    (ftype-set! Abits (a1 a14) x #x13579bdf)
    (ftype-set! Abits (a1 a15) x #x13579bdf)
    (ftype-set! Abits (a1 a16) x #x13579bdf)
    (ftype-set! Abits (a1 a17) x #x13579bdf)
    (ftype-set! Abits (a1 a18) x #x13579bdf)
    (ftype-set! Abits (a1 a19) x #x13579bdf)
    (ftype-set! Abits (a1 a20) x #x13579bdf)
    (ftype-set! Abits (a1 a21) x #x13579bdf)
    #t)
  (equal?
    (get-a3)
    a3-13579bdf)
  (equal?
    (get-a2)
    (a2-from-a3 a3-13579bdf))
  (begin
    (set-a1! (make-list 21 0))
    (set-a3! a3-c7c7c7c7)
    #t)
  (equal?
    (get-a3)
    a3-c7c7c7c7)
  (equal?
    (get-a2)
    (a2-from-a3 a3-c7c7c7c7))
  (equal?
    (get-a1)
    '(#x07c7c7c7
      #x07c7c7c7
      #xc7c7c600
      #xc7c007c7
      #x47c7c7c7
      #xc7c78000
      #x0003c7c7
      #xc7c7c000
      #x0007c7c7
      #xc7c7c000
      #x0007c7c7
      #xc7c7c600
      #x00c7c7c7
      #xc7c7c780
      #x03c7c7c7
      #xc7c7c7c0
      #x07c7c7c7
      #xc7c7c7c0
      #x07c7c7c7
      #xc7c7c7c6
      #xc7c7c7c7))
  (begin
    (set-a1! (make-list 21 0))
    (set-a2! (a2-from-a3 a3-13579bdf))
    #t)
  (equal?
    (get-a3)
    a3-13579bdf)
  (equal?
    (get-a2)
    (a2-from-a3 a3-13579bdf))
  (equal?
    (get-a1)
    '(#x03579bdf
      #x03579bdf
      #x13579a00
      #x13501bdf
      #x13579bdf
      #x13578000
      #x00039bdf
      #x13578000
      #x00079bdf
      #x13579800
      #x00179bdf
      #x13579a00
      #x00579bdf
      #x13579b80
      #x03579bdf
      #x13579bc0
      #x03579bdf
      #x13579bd8
      #x13579bdf
      #x13579bde
      #x13579bdf))

  (begin
    (fptr-free x)
    #t)
)

(mat ftype-inspection
  (begin
    (define-ftype Qa
      (struct
        [x short]
        [y long]))
    (define-ftype Q
      (struct
        [x (packed integer-32)]
        [y double-float]
        [z (array 4 (struct [_ integer-16] [b integer-16]))]
        [w (endian big
             (union
               [a integer-32]
               [b unsigned-32]))]
        [v (* Qa)]
        [u (array 3 float)]
        [t char]
        [s (endian little
             (array 2
               (bits
                 [x unsigned 3]
                 [y signed 4]
                 [_ unsigned 17]
                 [z unsigned 8])))]))
    (define q (make-ftype-pointer Q (foreign-alloc (ftype-sizeof Q))))
    (ftype-set! Q (x) q -73)
    (ftype-set! Q (y) q 3.25)
    (ftype-set! Q (z 0 b) q 11)
    (ftype-set! Q (z 1 b) q -15)
    (ftype-set! Q (z 2 b) q 53)
    (ftype-set! Q (z 3 b) q -71)
    (ftype-set! Q (w a) q -1)
    (ftype-set! Q (v) q (make-ftype-pointer Qa (foreign-alloc (ftype-sizeof Qa))))
    (ftype-set! Q (v * x) q 7)
    (ftype-set! Q (v * y) q -503)
    (ftype-set! Q (u 0) q 1.0)
    (ftype-set! Q (u 1) q 2.0)
    (ftype-set! Q (u 2) q 3.0)
    (ftype-set! Q (t) q #\$)
    (ftype-set! Q (s 0 x) q 5)
    (ftype-set! Q (s 0 y) q -2)
    (ftype-set! Q (s 0 z) q 225)
    (ftype-set! Q (s 1 x) q 2)
    (ftype-set! Q (s 1 y) q 7)
    (ftype-set! Q (s 1 z) q 47)
    #t)

  (equal?
    (ftype-pointer-ftype q)
    '(struct
       [x (packed integer-32)]
       [y double-float]
       [z (array 4 (struct [_ integer-16] [b integer-16]))]
       [w (endian big
            (union
              [a integer-32]
              [b unsigned-32]))]
       [v (* Qa)]
       [u (array 3 float)]
       [t char]
       [s (endian little
            (array 2
              (bits
                [x unsigned 3]
                [y signed 4]
                [_ unsigned 17]
                [z unsigned 8])))]))

  (eq? ; verify sharing in internal type field
    (ftype-pointer-ftype (ftype-&ref Q (s) q))
    (cadr (list-ref (ftype-pointer-ftype q) 8)))

  (equal?
    (ftype-pointer->sexpr q)
    '(struct
       [x -73]
       [y 3.25]
       [z (array 4
            (struct [_ _] [b 11])
            (struct [_ _] [b -15])
            (struct [_ _] [b 53])
            (struct [_ _] [b -71]))]
       [w (union [a -1] [b #xffffffff])]
       [v (* (struct [x 7] [y -503]))]
       [u (array 3 1.0 2.0 3.0)]
       [t #\$]
       [s (array 2
            (bits [x 5] [y -2] [_ _] [z 225])
            (bits [x 2] [y 7] [_ _] [z 47]))]))

  (begin
    (fptr-free q)
    #t)

 ; ----------------

  (begin
    (define-ftype big-wchar (endian big wchar))
    (define-ftype little-wchar (endian little wchar))
    (define-ftype Q
      (struct
        [a (array 10 char)]
        [b (array 10 wchar)]
        [c (endian big (array 10 wchar))]
        [d (endian little (array 10 wchar))]
        [e (* char)]
        [f (* wchar)]
        [g (* big-wchar)]
        [h (* little-wchar)]
        [i (* char)]
        [j (* wchar)]))
    (define q (make-ftype-pointer Q (foreign-alloc (ftype-sizeof Q))))

    (define-syntax ftype-set-char-array!
      (syntax-rules ()
        [(_ maxlen ftype (a ...) fptr str)
         (let ([len (min (string-length str) maxlen)])
           (do ([i 0 (fx+ i 1)])
               ((fx= i len))
             (ftype-set! ftype (a ... i) fptr (string-ref str i)))
           (when (< len maxlen) (ftype-set! ftype (a ... len) fptr #\nul)))]))

    (ftype-set-char-array! 10 Q (a) q "abcd")
    (ftype-set-char-array! 10 Q (b) q "abcdefghijklmnop")
    (ftype-set-char-array! 10 Q (c) q "ABCDEFGHIJKLMNOP")
    (ftype-set-char-array! 10 Q (d) q "ABCDEFG")
    
    (define-syntax ftype-set-string!
      (syntax-rules ()
        [(_ char ftype (a ...) fptr str p)
         (let ([len (string-length str)])
           (set! p (make-ftype-pointer char (foreign-alloc (fx* (ftype-sizeof char) (fx+ len 1)))))
           (do ([i 0 (fx+ i 1)])
             ((fx= i len))
             (ftype-set! char () p i (string-ref str i)))
           (ftype-set! char () p len #\nul)
           (ftype-set! ftype (a ...) fptr p))]))

    (ftype-set-string! char Q (e) q "hello!" q-e)
    (ftype-set-string! wchar Q (f) q "Hello!" q-f)
    (ftype-set-string! big-wchar Q (g) q "HELLO!" q-g)
    (ftype-set-string! little-wchar Q (h) q "GoodBye" q-h)

    (ftype-set! Q (i) q (make-ftype-pointer char 0))
    (ftype-set! Q (j) q (make-ftype-pointer wchar 1))

    #t)

  (if (memq (machine-type) '(ti3ob ta6ob)) ; avoid openbsd/pthreads signal bugs
      (error #f "openbsd pthreads + signals is fubar")
      (equal?
        (ftype-pointer->sexpr q)
        '(struct
           [a "abcd"]
           [b "abcdefghij"]
           [c "ABCDEFGHIJ"]
           [d "ABCDEFG"]
           [e "hello!"]
           [f "Hello!"]
           [g "HELLO!"]
           [h "GoodBye"]
           [i null]
           [j (* invalid)])))

  (if (memq (machine-type) '(ti3ob ta6ob)) ; avoid openbsd/pthreads signal bugs
      (error #f "openbsd pthreads + signals is fubar")
      (equal?
        (ftype-pointer->sexpr (make-ftype-pointer Q 0))
        '(struct
           [a (array 10 invalid)]
           [b (array 10 invalid)]
           [c (array 10 invalid)]
           [d (array 10 invalid)]
           [e invalid]
           [f invalid]
           [g invalid]
           [h invalid]
           [i invalid]
           [j invalid])))

  (begin
    (fptr-free q-e)
    (fptr-free q-f)
    (fptr-free q-g)
    (fptr-free q-h)
    (fptr-free q)
    #t)

 ; ----------------

  (begin
    (define-ftype A (endian little double))
    (define-ftype B (endian big double))
    #t)

  (equal?
    (ftype-pointer-ftype (make-ftype-pointer A 0))
    (case (native-endianness)
      [(big) '(endian little double)]
      [(little) 'double]
      [else (errorf #f "unexpected native endianness")]))

  (equal?
    (ftype-pointer-ftype (make-ftype-pointer B 0))
    (case (native-endianness)
      [(big) 'double]
      [(little) '(endian big double)]
      [else (errorf #f "unexpected native endianness")]))

  (begin
    (define-ftype A (endian little char))
    (define-ftype B (endian big char))
    #t)

  (eq? (ftype-pointer-ftype (make-ftype-pointer A 0)) 'char)
  (eq? (ftype-pointer-ftype (make-ftype-pointer B 0)) 'char)
)

(mat discarded-refs
  (begin
    (define-ftype A
      (endian big
        (struct
          [a1 double]
          [a2 float]
          [a3 long-long]
          [a4 unsigned-long-long]
          [a5 long]
          [a6 unsigned-long]
          [a7 int]
          [a8 unsigned]
          [a9 unsigned-int]
          [a10 short]
          [a11 unsigned-short]
          [a12 wchar]
          [a13 char]
          [a14 boolean]
          [a15 fixnum]
          [a16 iptr]
          [a17 uptr]
          [a18 void*])))
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (ftype-ref A (a1) x)
           (ftype-ref A (a2) x)
           (ftype-ref A (a3) x)
           (ftype-ref A (a4) x)
           (ftype-ref A (a5) x)
           (ftype-ref A (a6) x)
           (ftype-ref A (a7) x)
           (ftype-ref A (a8) x)
           (ftype-ref A (a9) x)
           (ftype-ref A (a10) x)
           (ftype-ref A (a11) x)
           (ftype-ref A (a12) x)
           (ftype-ref A (a13) x)
           (ftype-ref A (a14) x)
           (ftype-ref A (a15) x)
           (ftype-ref A (a16) x)
           (ftype-ref A (a17) x)
           (ftype-ref A (a18) x)
           x)))
    '(lambda (x) x))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (ftype-&ref A (a1) x)
           (ftype-&ref A (a2) x)
           (ftype-&ref A (a3) x)
           (ftype-&ref A (a4) x)
           (ftype-&ref A (a5) x)
           (ftype-&ref A (a6) x)
           (ftype-&ref A (a7) x)
           (ftype-&ref A (a8) x)
           (ftype-&ref A (a9) x)
           (ftype-&ref A (a10) x)
           (ftype-&ref A (a11) x)
           (ftype-&ref A (a12) x)
           (ftype-&ref A (a13) x)
           (ftype-&ref A (a14) x)
           (ftype-&ref A (a15) x)
           (ftype-&ref A (a16) x)
           (ftype-&ref A (a17) x)
           (ftype-&ref A (a18) x)
           x)))
    '(lambda (x) x))
  (begin
    (define-ftype A
      (endian little
        (struct
          [a1 double]
          [a2 float]
          [a3 long-long]
          [a4 unsigned-long-long]
          [a5 long]
          [a6 unsigned-long]
          [a7 int]
          [a8 unsigned]
          [a9 unsigned-int]
          [a10 short]
          [a11 unsigned-short]
          [a12 wchar]
          [a13 char]
          [a14 boolean]
          [a15 fixnum]
          [a16 iptr]
          [a17 uptr]
          [a18 void*])))
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (ftype-ref A (a1) x)
           (ftype-ref A (a2) x)
           (ftype-ref A (a3) x)
           (ftype-ref A (a4) x)
           (ftype-ref A (a5) x)
           (ftype-ref A (a6) x)
           (ftype-ref A (a7) x)
           (ftype-ref A (a8) x)
           (ftype-ref A (a9) x)
           (ftype-ref A (a10) x)
           (ftype-ref A (a11) x)
           (ftype-ref A (a12) x)
           (ftype-ref A (a13) x)
           (ftype-ref A (a14) x)
           (ftype-ref A (a15) x)
           (ftype-ref A (a16) x)
           (ftype-ref A (a17) x)
           (ftype-ref A (a18) x)
           x)))
    '(lambda (x) x))
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (ftype-&ref A (a1) x)
           (ftype-&ref A (a2) x)
           (ftype-&ref A (a3) x)
           (ftype-&ref A (a4) x)
           (ftype-&ref A (a5) x)
           (ftype-&ref A (a6) x)
           (ftype-&ref A (a7) x)
           (ftype-&ref A (a8) x)
           (ftype-&ref A (a9) x)
           (ftype-&ref A (a10) x)
           (ftype-&ref A (a11) x)
           (ftype-&ref A (a12) x)
           (ftype-&ref A (a13) x)
           (ftype-&ref A (a14) x)
           (ftype-&ref A (a15) x)
           (ftype-&ref A (a16) x)
           (ftype-&ref A (a17) x)
           (ftype-&ref A (a18) x)
           x)))
    '(lambda (x) x))
  (begin
    (define-ftype A
      (endian big
        (union
          [a1 (struct
                [a1 unsigned-16]
                [a2 unsigned-8]
                [a3 unsigned-64]
                [a4 unsigned-32])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 1]
                      [a2 signed 15])]
                [a2 (bits
                      [a1 signed 3]
                      [a2 signed 5])]
                [a3 (bits
                      [a1 signed 50]
                      [a2 signed 14])]
                [a4 (bits
                      [a1 signed 19]
                      [a2 signed 13])])]
          [a3 (struct
                [a1 (bits
                      [a1 unsigned 1]
                      [a2 unsigned 15])]
                [a2 (bits
                      [a1 unsigned 3]
                      [a2 unsigned 5])]
                [a3 (bits
                      [a1 unsigned 50]
                      [a2 unsigned 14])]
                [a4 (bits
                      [a1 unsigned 19]
                      [a2 unsigned 13])])])))
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (ftype-ref A (a1 a1) x)
           (ftype-ref A (a1 a2) x)
           (ftype-ref A (a1 a3) x)
           (ftype-ref A (a1 a4) x)
           (ftype-ref A (a2 a1 a1) x)
           (ftype-ref A (a2 a1 a2) x)
           (ftype-ref A (a2 a2 a1) x)
           (ftype-ref A (a2 a2 a2) x)
           (ftype-ref A (a2 a3 a1) x)
           (ftype-ref A (a2 a3 a2) x)
           (ftype-ref A (a2 a4 a1) x)
           (ftype-ref A (a2 a4 a2) x)
           (ftype-ref A (a3 a1 a1) x)
           (ftype-ref A (a3 a1 a2) x)
           (ftype-ref A (a3 a2 a1) x)
           (ftype-ref A (a3 a2 a2) x)
           (ftype-ref A (a3 a3 a1) x)
           (ftype-ref A (a3 a3 a2) x)
           (ftype-ref A (a3 a4 a1) x)
           (ftype-ref A (a3 a4 a2) x)
           x)))
    '(lambda (x) x))
  (begin
    (define-ftype A
      (endian little
        (union
          [a1 (struct
                [a1 unsigned-16]
                [a2 unsigned-8]
                [a3 unsigned-64]
                [a4 unsigned-32])]
          [a2 (struct
                [a1 (bits
                      [a1 signed 1]
                      [a2 signed 15])]
                [a2 (bits
                      [a1 signed 3]
                      [a2 signed 5])]
                [a3 (bits
                      [a1 signed 50]
                      [a2 signed 14])]
                [a4 (bits
                      [a1 signed 19]
                      [a2 signed 13])])]
          [a3 (struct
                [a1 (bits
                      [a1 unsigned 1]
                      [a2 unsigned 15])]
                [a2 (bits
                      [a1 unsigned 3]
                      [a2 unsigned 5])]
                [a3 (bits
                      [a1 unsigned 50]
                      [a2 unsigned 14])]
                [a4 (bits
                      [a1 unsigned 19]
                      [a2 unsigned 13])])])))
    #t)
  (equivalent-expansion?
    (parameterize ([optimize-level 3] [enable-cp0 #t] [#%$suppress-primitive-inlining #f])
      (expand/optimize
        '(lambda (x)
           (ftype-ref A (a1 a1) x)
           (ftype-ref A (a1 a2) x)
           (ftype-ref A (a1 a3) x)
           (ftype-ref A (a1 a4) x)
           (ftype-ref A (a2 a1 a1) x)
           (ftype-ref A (a2 a1 a2) x)
           (ftype-ref A (a2 a2 a1) x)
           (ftype-ref A (a2 a2 a2) x)
           (ftype-ref A (a2 a3 a1) x)
           (ftype-ref A (a2 a3 a2) x)
           (ftype-ref A (a2 a4 a1) x)
           (ftype-ref A (a2 a4 a2) x)
           (ftype-ref A (a3 a1 a1) x)
           (ftype-ref A (a3 a1 a2) x)
           (ftype-ref A (a3 a2 a1) x)
           (ftype-ref A (a3 a2 a2) x)
           (ftype-ref A (a3 a3 a1) x)
           (ftype-ref A (a3 a3 a2) x)
           (ftype-ref A (a3 a4 a1) x)
           (ftype-ref A (a3 a4 a2) x)
           x)))
    '(lambda (x) x))
)
