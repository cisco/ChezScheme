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

#|
todo:
  - consider adding uid form, with warnings if nested ftypes do not
    also have uid forms...need to check ftd generative? flag.
    alternatively, consider implementing textual ftype equality.
    would need some sort of union-find algorithm and a couple of extra
    indirects to reduce the cost of checks.  either way, generalize
    syntaxes that now require ftype names to allow arbitrary ftypes
  - consider passing $fptr-ref-xxx, $fptr-set-xxx! more info to
    produce better error messages
  - consider support for variable-length arrays.  there may be no good
    way to do so.  don't want to make ftds generative, but can't
    avoid doing so if the lengths aren't known until run time since
    each evaluation of an ftype form could result in different sizes.
    as an alternative, perhaps give some way to define array-length
    constants, e.g., (define-ftype-constant x 10).
  - consider moving verify-ftype-pointer checks into $fptr-&ref,
    $fptr-ref, and $fptr-set! to reduce the amount of generated code.
    we'd end up doing more checks that way when pointer indirects are
    followed and new fptrs are generated, but that probably isn't a
    big deal.  would need to pass $fptr-ref a who argument for use in
    following pointers from ftype-&ref and ftype-set!
|#

#|
(define-ftype ftype-name ftype) [syntax]

ftype-name -> identifier

ftype ->
  ftype-name
  (* ftype)
  (struct (field-name ftype) ...)
  (union (field-name ftype) ...)
  (array length ftype)
  (bits (field-name signedness bits) ...)
  (function (arg-type ...) result-type)
  (function conv ... (arg-type ...) result-type)
  (packed ftype)
  (unpacked ftype)
  (endian endianness ftype)

length -> exact nonnegative integer

field-name -> identifier

signedness -> signed | unsigned

bits -> exact positive integer

endianness -> native | big | little | swapped

built-in ftype names:
    short | unsigned-short
    int | unsigned | unsigned-int
    long | unsigned-long
    long-long | unsigned-long-long
    char | wchar
    float | double
    void* | iptr | uptr
    fixnum | boolean | stdbool
    integer-8 | unsigned-8
    integer-16 | unsigned-16
    integer-24 | unsigned-24
    integer-32 | unsigned-32
    integer-40 | unsigned-40
    integer-48 | unsigned-48
    integer-56 | unsigned-56
    integer-64 | unsigned-64
    single-float | double-float
    size_t | ssize_t | ptrdiff_t | wchar_t

notes:
  - underscore ( _ ) can be used as the field name for one or
    more fields of a struct or union.  such fields are included
    in the layout but are considered unnamed and cannot be accessed
    via the ftype operators described below.

  - non-underscore field names are handled symbolically, i.e.,
    they are treated as symbols rather than identifiers.  each
    symbol must be unique (as a symbol) with respect to the other
    field names within a single struct or union, but need not be
    unique with respect to field names in other structs or
    unions, including those nested inside the struct or union.

  - by default, padding is inserted where appropriate to maintain
    proper alignment of multibyte scalar values in an attempt to
    mirror the target machine's (often poorly documented) C struct
    layout conventions.  for packed ftypes (ftypes wrapped in a
    packed form with no closer enclosing unpacked form), this
    padding is not inserted.

  - the overall size of an ftype (including padding) must be a fixnum.

  - the total size n of the fields within an ftype bits form must
    be 8, 16, 24, 32, 40, 48, 56, or 64.  padding must be added manually
    if needed.  (Performance might suffer when the total is not a power
    of two or is 64 on a 32-bit machine.)  For little-endian machines,
    the first field occupies the low-order bits of the container, with
    each subsequent field just above the preceding field, while for
    big-endian machines, the first field occupies the high-order bits,
    with each subsequent field just below the preceding field.

  - ftyp<e pointers are records encapsulating an ftype descriptor
    (ftd) along with the address of the foreign object, except that
    pointers of type void* are just addresses.  the encapsulated
    ftd is used to verify the applicability of an ftype-&ref,
    ftype-ref, or ftype-set! operation.

  - two ftypes are considered equivalent if and only if their uids are
    equivalent.  a fresh uid is created each time an ftype declaration
    is expanded.  thus, textually distinct ftypes are not considered
    equivalent even if they are identical in structure.

  - all signed or unsigned integer fields (including bit fields) can
    be set to an exact integer in the range -2^{k-1}..+2^k-1, where
    k is the size in bits of the integer

  - most run-time checks are disabled at optimize-level 3.

  - the use of packed ftypes or the use of improperly aligned
    addresses can result in unaligned references, which are inefficient
    on some machines and result in invalid-memory reference exceptions
    on others.

ftype operators:

(ftype-sizeof ftype-name) [syntax]

   returns the size in bytes of an object with type ftype-name.

(make-ftype-pointer ftype-name address) [syntax]

  creates an ftype pointer encapsulating an ftype descriptor (ftd)
  for the named ftype along with the address.

(ftype-pointer? expr) [syntax]
(ftype-pointer? ftype-name expr) [syntax]

   in the first form, return #t if the value of expr is an ftype
   pointer, otherwise #f.  in the second form, returns #t if the
   value of expr is an ftype pointer of the named ftype,
   otherwise #f.

(ftype-pointer-address fptr) [procedure]

   returns the address encapsulated within fptr.

(ftype-pointer-null? fptr) [procedure]

   returns #t if the address encapsulated within fptr is 0,
   otherwise #f.

(ftype-pointer=? fptr1 fptr2) [procedure]

   returns #t if the addresses encapsulated within fptr are
   the same, otherwise #f.

(ftype-pointer-ftype fptr) [procedure]

   returns an s-expression representation of fptr's ftype.  the
   s-expression should not be modified.

(ftype-pointer->sexpr fptr) [procedure]

   returns an s-expression representation of the foreign object
   pointed to by fptr.  the s-expression should not be modified.

(ftype-&ref ftype-name (access ...) fptr-expr) [syntax]

   returns an ftype pointer to the named field of the foreign object
   pointed to by the value of fptr-expr, which must be an ftype
   pointer of the named ftype.  each access must be a field name or
   array index expression, as appropriate for the named ftype; it is a
   syntax error if this is not the case.  the values of all array indices
   must be in bounds for the array.  any nonnegative index is considered
   in bounds for an array of size zero.  the return value is a freshly
   allocated ftype pointer, except that if (access ...) is empty, the
   return value might be eq to the value of fptr-expr.

(ftype-ref ftype-name (access ...) fptr-expr) [syntax]

   returns the value of the specified field of the foreign object pointed
   to by the value of fptr-expr, which must be an ftype pointer of the
   named ftype.  The access and fptr-expr requirements stated under
   ftype-&ref apply as well to ftype-ref.  In addition, the field must
   be a scalar field, i.e., one of the base types, an alias for one of
   the base types, or a pointer.  If the result is a pointer value,
   other than one declared as void*, the return value is a freshly
   allocated ftype pointer.

   It (access ...) is empty, the "specified field" is the object pointed
   to by the ftype pointer.  For example, if x is an ftype-pointer pointing
   to a double, (ftype-ref double () x) returns the double.

(ftype-set! ftype-name (access ...) fptr-expr val-expr) [syntax]

   sets the value of the named field of the foreign object pointed to
   by the value of fptr-expr, which must be an ftype pointer of the
   named ftype, to the value of val-expr.  The access and fptr-expr
   requirements stated under ftype-&ref and ftype-ref apply as well
   to ftype-set!.  val-expr must evaluate to a value appropriate for
   the identified field; for pointer fields other than those declared as
   void*, the value must be an ftype pointer with the appropriate ftype.
   Otherwise, the value of val-expr must be of the appropriate type,
   e.g., a character for types char and wchar and an integer of the
   appropriate size for int and unsigned.
|#

(begin
(let ()
  (include "types.ss")
  (define-syntax rtd/fptr
    (let ([rtd ($make-record-type #!base-rtd #f
                 '#{ftype-pointer a9pth58056u34h517jsrqv-0}
                 '((immutable uptr address))
                 #f
                 #f)])
      (lambda (x) #`'#,rtd)))
  (define $fptr? (record-predicate rtd/fptr))
  (define $ftype-pointer-address (record-accessor rtd/fptr 0))
  (define-syntax rtd/ftd
    (let ([rtd ($make-record-type #!base-rtd #!base-rtd
                 '#{rtd/ftd a9pth58056u34h517jsrqv-1}
                 '((immutable ptr stype)
                   (immutable ptr size)
                   (immutable ptr alignment))
                 #f
                 #f)])
      (lambda (x) #`'#,rtd)))
  (define ftd? (record-predicate rtd/ftd))
  (define ftd-stype (record-accessor rtd/ftd 0))
  (define ftd-size (record-accessor rtd/ftd 1))
  (define ftd-alignment (record-accessor rtd/ftd 2))
  (define-syntax define-ftd-record-type
    (lambda (x)
      (define construct-name
        (lambda (template-identifier . args)
          (datum->syntax
            template-identifier
            (string->symbol
              (apply string-append
                     (map (lambda (x)
                            (if (string? x)
                                x
                                (symbol->string (syntax->datum x))))
                          args))))))
      (define ftd-field
        (lambda (field)
          (syntax-case field (mutable)
            [field-name
             (identifier? #'field-name)
             #'field-name]
            [(mutable field-name)
             (identifier? #'field-name)
             #'field-name])))
      (define ftd-accessors
        (lambda (record-name field*)
          (define accessor
            (lambda (field-name ordinal)
              #`(define #,(construct-name field-name "ftd-" record-name "-" field-name)
                  (record-accessor rtd #,ordinal))))
          (define mutator
            (lambda (field-name ordinal)
              #`(define #,(construct-name field-name "ftd-" record-name "-" field-name "-set!")
                  (record-mutator rtd #,ordinal))))
          (let f ([field* field*] [ordinal 0])
            (if (null? field*)
                '()
                (syntax-case (car field*) (mutable)
                  [field-name
                   (identifier? #'field-name)
                   (cons (accessor #'field-name ordinal)
                     (f (cdr field*) (+ ordinal 1)))]
                  [(mutable field-name)
                   (identifier? #'field-name)
                   (cons (mutator #'field-name ordinal)
                     (cons (accessor #'field-name ordinal)
                       (f (cdr field*) (+ ordinal 1))))])))))
      (syntax-case x ()
        [(_ record-name ?uid field ...)
         (with-syntax ([(field-name ...) (map ftd-field #'(field ...))]
                       [constructor-name (construct-name #'record-name "make-ftd-" #'record-name)])
           #`(begin
               (define-syntax rtd
                 (let ([rtd ($make-record-type #!base-rtd rtd/ftd
                              '?uid
                              '(field ...)
                              #t
                              #f)])
                   (lambda (x) #`'#,rtd)))
               (define constructor-name
                 (lambda (parent uid stype size alignment field-name ...)
                   ($make-record-type rtd parent (or uid #,(symbol->string (datum record-name))) '() #f #f stype size alignment field-name ...)))
               (define #,(construct-name #'record-name "ftd-" #'record-name "?")
                 (record-predicate rtd))
               #,@(ftd-accessors #'record-name #'(field ...))))])))

  (define-ftd-record-type base #{rtd/ftd-base a9pth58056u34h517jsrqv-18} eness type)
  (define-ftd-record-type struct #{rtd/ftd-struct a9pth58056u34h517jsrqv-3} field*)
  (define-ftd-record-type union #{rtd/ftd-union a9pth58056u34h517jsrqv-4} field*)
  (define-ftd-record-type array #{rtd/ftd-array a9pth58056u34h517jsrqv-5} length ftd)
  (define-ftd-record-type pointer #{rtd/ftd-pointer a9pth58056u34h517jsrqv-6} (mutable ftd))
  (define-ftd-record-type bits #{rtd/ftd-ibits a9pth58056u34h517jsrqv-19} eness field*)
  (define-ftd-record-type function #{rtd/ftd-function a9pth58056u34h517jsrqv-11} conv* arg-type* result-type)
  (module (pointer-size alignment pointer-alignment native-base-ftds swap-base-ftds big-base-ftds little-base-ftds)
    (define alignment
      (lambda (max-alignment size)
        (gcd max-alignment size)))
    (define pointer-size (/ (constant address-bits) 8))
    (define pointer-alignment (gcd (constant max-integer-alignment) pointer-size))
    (define base-types
      '(short unsigned-short int unsigned unsigned-int long
        unsigned-long long-long unsigned-long-long char wchar float
        double void* iptr uptr fixnum boolean stdbool integer-8 unsigned-8
        integer-16 unsigned-16 integer-24 unsigned-24 integer-32 unsigned-32
        integer-40 unsigned-40 integer-48 unsigned-48 integer-56 unsigned-56
        integer-64 unsigned-64 single-float double-float wchar_t size_t ssize_t ptrdiff_t))
    (define-who mfb
      (lambda (eness)
        (lambda (ty)
          (define-syntax make
            (syntax-rules ()
              [(_ type bytes pred)
               (cond
                 [(or (and (not (eq? eness 'native)) (fx= bytes 1))
                      (eq? (constant native-endianness) eness))
                  (find (lambda (ftd) (eq? (ftd-base-type ftd) ty)) native-base-ftds)]
                 [(or (and (eq? eness 'little)
                           (eq? (constant native-endianness) 'big))
                      (and (eq? eness 'big)
                           (eq? (constant native-endianness) 'little)))
                  (find (lambda (ftd) (eq? (ftd-base-type ftd) ty)) swap-base-ftds)]
                 [else
                   (make-ftd-base rtd/fptr
                     ; creating static gensym so base ftypes are nongenerative to support
                     ; separate compilation of ftype definitions and uses.  creating unique
                     ; name so this works even when this file is reloaded, e.g., as a patch
                     ; file.  machine-type is included in the unique name so that we get
                     ; a different rtd/ftd with the correct "extras" (including size and
                     ; alignment) when cross compiling between machines with different
                     ; base-type characteristics.
                     (let ([pname (format "~a~a" ty (case eness
                                                      [(native) ""]
                                                      [(swapped) "s"]
                                                      [(big) "b"]
                                                      [(little) "l"]))])
                       (let ([gstring (format "~aa9pth58056u34h517jsrqv-~s-~a" pname (constant machine-type-name) pname)])
                         ($intern3 gstring (string-length pname) (string-length gstring))))
                     (if (eq? eness 'native)
                         ty
                         `(endian ,eness ,ty))
                     bytes (alignment (if (memq 'type '(single-float double-float)) (constant max-float-alignment) (constant max-integer-alignment)) bytes) eness ty)])]))
          (record-datatype cases (filter-foreign-type ty) make
            ($oops who "unrecognized type ~s" ty)))))
    (define native-base-ftds (map (mfb 'native) base-types))
    (define swap-base-ftds (map (mfb 'swapped) base-types))
    (define big-base-ftds (map (mfb 'big) base-types))
    (define little-base-ftds (map (mfb 'little) base-types)))
  (define expand-field-names
    (lambda (x*)
      (let f ([x* x*] [seen* '()])
        (if (null? x*)
            '()
            (let ([x (car x*)] [x* (cdr x*)])
              (unless (identifier? x) (syntax-error x "invalid field name"))
              (if (free-identifier=? x #'_)
                  (cons #f (f x* seen*))
                  (let ([s (syntax->datum x)])
                    (if (memq s seen*)
                        (syntax-error x "duplicate field name")
                        (cons s (f x* (cons s seen*)))))))))))
  (define expand-ftype-name
    (case-lambda
      [(r ftype) (expand-ftype-name r ftype #t)]
      [(r ftype error?)
       (cond
         [(let ([maybe-ftd (r ftype)]) (and maybe-ftd (ftd? maybe-ftd) maybe-ftd)) => (lambda (ftd) ftd)]
         [(find (let ([x (syntax->datum ftype)])
                  (lambda (ftd) (eq? (ftd-base-type ftd) x)))
            native-base-ftds)]
         [else (and error? (syntax-error ftype "unrecognized ftype name"))])]))
  (define expand-ftype
    (case-lambda
      [(r defid ftype) (expand-ftype r '() defid ftype)]
      [(r def-alist defid ftype)
       (define (check-size ftd)
         (unless (ftd-function? ftd)
           (let ([size (ftd-size ftd)])
             (unless (and (>= size 0) (< size (constant most-positive-fixnum)))
               (syntax-error ftype "non-fixnum overall size for ftype"))))
         ftd)
       (check-size
         (let f/flags ([ftype ftype] [defid defid] [stype (syntax->datum ftype)] [packed? #f] [eness 'native] [funok? #t])
           (define (pad n k) (if packed? n (logand (+ n (- k 1)) (- k))))
           (define (native-ftds)
             (case eness
               [(native) native-base-ftds]
               [(swapped) swap-base-ftds]
               [(big) big-base-ftds]
               [(little) little-base-ftds]
               [else (error 'eness "unexpected ~s" eness)]))
           (let f ([ftype ftype] [defid defid] [stype stype] [funok? funok?])
             (if (identifier? ftype)
                 (cond
                   [(assp (lambda (x) (bound-identifier=? ftype x)) def-alist) =>
                    (lambda (a)
                      (let ([ftd (let ([ftd (cdr a)])
                                   (if (ftd? ftd)
                                       ftd
                                       (or (find (let ([x (syntax->datum ftype)])
                                                   (lambda (ftd)
                                                     (eq? (ftd-base-type ftd) x)))
                                             (native-ftds))
                                           ftd)))])
                        (unless (ftd? ftd)
                          (syntax-error ftype "recursive or forward reference outside pointer field"))
                        (unless funok?
                          (when (ftd-function? ftd)
                            (syntax-error ftype "unexpected function ftype name outside pointer field")))
                        ftd))]
                   [(let ([maybe-ftd (r ftype)]) (and maybe-ftd (ftd? maybe-ftd) maybe-ftd)) =>
                    (lambda (ftd)
                      (unless funok?
                        (when (ftd-function? ftd)
                          (syntax-error ftype "unexpected function ftype name outside pointer field")))
                      ftd)]
                   [(find (let ([x (syntax->datum ftype)])
                            (lambda (ftd) (eq? (ftd-base-type ftd) x)))
                          (native-ftds))]
                   [else (syntax-error ftype "unrecognized ftype name")])
                 (syntax-case ftype ()
                   [(struct-kwd (field-name ftype) ...)
                    (eq? (datum struct-kwd) 'struct)
                    (let loop ([id* (expand-field-names #'(field-name ...))]
                               [ftd* (map (lambda (ftype stype) (f ftype #f stype #f))
                                          #'(ftype ...) (datum (ftype ...)))]
                               [offset 0] [alignment 1] [field* '()])
                      (if (null? id*)
                          (let ([field* (reverse field*)])
                            (make-ftd-struct (if (null? field*) rtd/fptr (caddar field*))
                              (and defid (symbol->string (syntax->datum defid)))
                              stype (pad offset alignment) alignment field*))
                          (let ([ftd (car ftd*)])
                            (let ([offset (pad offset (ftd-alignment ftd))])
                              (loop (cdr id*) (cdr ftd*)
                                (+ offset ($ftd-size ftd))
                                (max alignment (ftd-alignment ftd))
                                (cons (list (car id*) offset ftd) field*))))))]
                   [(union-kwd (field-name ftype) ...)
                    (eq? (datum union-kwd) 'union)
                    (let ([id* (expand-field-names #'(field-name ...))]
                          [ftd* (map (lambda (ftype stype) (f ftype #f stype #f))
                                     #'(ftype ...) (datum (ftype ...)))])
                      (let ([alignment (apply max 1 (map ftd-alignment ftd*))])
                        (make-ftd-union rtd/fptr
                          (and defid (symbol->string (syntax->datum defid)))
                          stype
                          (pad (apply max 0 (map $ftd-size ftd*)) alignment)
                          alignment
                          (map cons id* ftd*))))]
                   [(array-kwd ?n ftype)
                    (eq? (datum array-kwd) 'array)
                    (let ([n (datum ?n)])
                      (unless (and (integer? n) (exact? n) (>= n 0))
                        (syntax-error #'?n "invalid array size"))
                      (let ([ftd (f #'ftype #f (datum ftype) #f)])
                        (make-ftd-array ftd
                          (and defid (symbol->string (syntax->datum defid)))
                          stype
                          (* n ($ftd-size ftd)) ; use `$ftd-size` for PPC Mac OS
                          (ftd-alignment ftd)
                          n ftd)))]
                   [(bits-kwd (field-name signedness bits) ...)
                    (eq? (datum bits-kwd) 'bits)
                    (let ()
                      (define parse-fields
                        (lambda ()
                          (define signed?
                            (lambda (s)
                              (case (syntax->datum s)
                                [(signed) #t]
                                [(unsigned) #f]
                                [else (syntax-error s "invalid bit-field signedness specifier")])))
                          (let f ([id* (expand-field-names #'(field-name ...))]
                                  [s* #'(signedness ...)]
                                  [bits* #'(bits ...)]
                                  [bit-offset 0])
                            (if (null? id*)
                                (values bit-offset '())
                                (let ([bits (syntax->datum (car bits*))])
                                  (unless (and (fixnum? bits) (fx>= bits 1))
                                    (syntax-error (car bits*) "invalid bit-field bit count"))
                                  (let-values ([(bit-size field*) (f (cdr id*) (cdr s*) (cdr bits*) (+ bit-offset bits))])
                                    (values bit-size
                                      (let ([start (if (or (eq? eness 'big)
                                                           (and (eq? eness 'native)
                                                                (eq? (constant native-endianness) 'big))
                                                           (and (eq? eness 'swapped)
                                                                (eq? (constant native-endianness) 'little)))
                                                       (- bit-size bit-offset bits)
                                                       bit-offset)])
                                        (cons (list (car id*) (signed? (car s*)) start (+ start bits))
                                              field*)))))))))
                      (when (and (eq? (constant native-endianness) 'unknown)
                                 (not (or (eq? eness 'little) (eq? eness 'big))))
                        (syntax-error ftype "bit fields require a specific endianness"))
                      (let-values ([(bit-size field*) (parse-fields)])
                        (unless (memq bit-size '(8 16 24 32 40 48 56 64))
                          (syntax-error ftype "bit counts do not add up to 8, 16, 32, or 64"))
                           (let ([offset (fxsrl bit-size 3)])
                             (make-ftd-bits rtd/fptr
                               (and defid (symbol->string (syntax->datum defid)))
                               stype offset (alignment (constant max-integer-alignment) offset)
                               eness field*))))]
                   [(*-kwd ftype)
                    (eq? (datum *-kwd) '*)
                    (cond
                      [(and (identifier? #'ftype)
                            (assp (lambda (x) (bound-identifier=? #'ftype x)) def-alist)) =>
                       (lambda (a)
                         (if (ftd? (cdr a))
                             (make-ftd-pointer rtd/fptr
                               (and defid (symbol->string (syntax->datum defid)))
                               stype pointer-size pointer-alignment (cdr a))
                             (let ([ftd (make-ftd-pointer rtd/fptr
                                          (and defid (symbol->string (syntax->datum defid)))
                                          stype pointer-size pointer-alignment #f)])
                               (set-cdr! a (cons ftd (cdr a)))
                               ftd)))]
                      [else (make-ftd-pointer rtd/fptr
                              (and defid (symbol->string (syntax->datum defid)))
                              stype pointer-size pointer-alignment (f #'ftype #f (datum ftype) #t))])]
                   [(function-kwd (arg-type ...) result-type)
                    (eq? (datum function-kwd) 'function)
                    (f #'(function-kwd #f (arg-type ...) result-type) #f stype funok?)]
                   [(function-kwd conv ... (arg-type ...) result-type)
                    (eq? (datum function-kwd) 'function)
                    (let ()
                      (define filter-type
                        (lambda (r x result?)
                          (let ([what (if result? 'result 'argument)])
                            (or ($fp-filter-type (expand-fp-ftype 'function-ftype what r x def-alist) result?)
                                (syntax-error x (format "invalid function-ftype ~s type specifier" what))))))
                      (unless funok? (syntax-error ftype "unexpected function ftype outside pointer field"))
                      (make-ftd-function rtd/fptr
                        (and defid (symbol->string (syntax->datum defid)))
                        stype #f #f
                        ($filter-conv 'function-ftype #'(conv ...) (length #'(arg-type ...)))
                        (map (lambda (x) (filter-type r x #f)) #'(arg-type ...))
                        (filter-type r #'result-type #t)))]
                   [(packed-kwd ftype)
                    (eq? (datum packed-kwd) 'packed)
                    (f/flags #'ftype #f stype #t eness funok?)]
                   [(unpacked-kwd ftype)
                    (eq? (datum unpacked-kwd) 'unpacked)
                    (f/flags #'ftype #f stype #f eness funok?)]
                   [(endian-kwd ?eness ftype)
                    (eq? (datum endian-kwd) 'endian)
                    (let ([new-eness (datum ?eness)])
                      (unless (memq new-eness '(big little native swapped))
                        (syntax-error #'?eness "invalid endianness"))
                      (let ([eness (case new-eness
                                     [(swapped) (case eness
                                                  [(little) 'big]
                                                  [(big) 'little]
                                                  [(native) 'swapped]
                                                  [(swapped) 'native])]
                                     [else new-eness])])
                        (f/flags #'ftype #f stype packed? eness funok?)))]
                   [_ (syntax-error ftype "invalid ftype")])))))]))
  (define expand-fp-ftype
    (lambda (who what r ftype def-alist)
      (syntax-case ftype ()
        [(*/&-kwd ftype-name)
         (and (or (eq? (datum */&-kwd) '*)
                  (eq? (datum */&-kwd) '&))
              (identifier? #'ftype-name))
         (let* ([stype (syntax->datum ftype)]
                [ftd
                 (cond
                  [(assp (lambda (x) (bound-identifier=? #'ftype-name x)) def-alist) =>
                   (lambda (a)
                     (if (ftd? (cdr a))
                         (make-ftd-pointer rtd/fptr #f stype pointer-size pointer-alignment (cdr a))
                         (let ([ftd (make-ftd-pointer rtd/fptr #f stype pointer-size pointer-alignment #f)])
                           (set-cdr! a (cons ftd (cdr a)))
                           ftd)))]
                  [(expand-ftype-name r #'ftype-name #f) =>
                   (lambda (ftd)
                     (make-ftd-pointer rtd/fptr #f stype pointer-size pointer-alignment ftd))]
                  [else (syntax-error #'ftype-name (format "unrecognized ~s ~s ftype name" who what))])])
           ;; Scheme-side argument is a pointer to a value, but foreign side has two variants:
           (if (eq? (datum */&-kwd) '&)
               (cond
                [(ftd-array? (ftd-pointer-ftd ftd))
                 (syntax-error ftype (format "array value invalid as ~a ~s" who what))]
                [else
                 (box ftd)]) ; boxed ftd => pass/receive the value (as opposed to a pointer to the value)
               ftd))]    ; plain ftd => pass/receive a pointer to the value
        [_ (cond
             [(and (identifier? ftype) (expand-ftype-name r ftype #f)) =>
              (lambda (ftd)
                (unless (ftd-base? ftd)
                  (syntax-error ftype (format "invalid (non-base) ~s ~s ftype" who what)))
                (unless (eq? (ftd-base-eness ftd) 'native)
                  (syntax-error ftype (format "invalid (not native) ~s ~s ftype" who what)))
                (ftd-base-type ftd))]
             [else (syntax->datum ftype)])])))
  (define-who indirect-ftd-pointer
    (lambda (x)
      (cond
       [(ftd? x)
        (if (ftd-pointer? x)
            (ftd-pointer-ftd x)
            ($oops who "~s is not an ftd-pointer" x))]
       [(box? x)
        (box (indirect-ftd-pointer (unbox x)))]
       [else x])))
  (define-who expand-ftype-defns
    (lambda (r defid* ftype*)
      (define patch-pointer-ftds!
        (lambda (id ftd)
          (lambda (pointer-ftd)
            (ftd-pointer-ftd-set! pointer-ftd ftd))))
      (let ([alist (map list defid*)])
        (for-each
          (lambda (defid ftype a)
            (let ([ftd (expand-ftype r alist defid ftype)])
              (for-each (patch-pointer-ftds! defid ftd) (cdr a))
              (set-cdr! a ftd)))
          defid* ftype* alist)
        (map cdr alist))))
  (define unsigned-type
    (lambda (size)
      (case size
        [(1) 'unsigned-8]
        [(2) 'unsigned-16]
        [(3) 'unsigned-24]
        [(4) 'unsigned-32]
        [(5) 'unsigned-40]
        [(6) 'unsigned-48]
        [(7) 'unsigned-56]
        [(8) 'unsigned-64]
        [else ($oops 'unsigned-type "unexpected size ~s" size)])))
  (define-record-type src-info
    (nongenerative #{src-info sls7d75lyfm0jejerbq3n-0})
    (sealed #f)
    (fields src)
    (protocol
      (lambda (new)
        (lambda (expr)
          (new
            (let ([a (syntax->annotation expr)])
              (and (and a (fxlogtest (annotation-flags a) (constant annotation-debug)))
                   (annotation-source a))))))))
  (define-record-type field-info
    (parent src-info)
    (nongenerative #{field-info sls7d75lyfm0jejerbq3n-1})
    (sealed #t)
    (fields type)
    (protocol
      (lambda (pargs->new)
        (lambda (type expr)
          ((pargs->new expr) type)))))
  (define-record-type ftd-info
    (parent src-info)
    (nongenerative #{ftd-info sls7d75lyfm0jejerbq3n-2})
    (sealed #t)
    (fields who ftd)
    (protocol
      (lambda (pargs->new)
        (lambda (whoid expr ftd)
          ((pargs->new expr) (syntax->datum whoid) ftd)))))
  (define-record-type index-info
    (parent src-info)
    (nongenerative #{index-info sls7d75lyfm0jejerbq3n-3})
    (sealed #t)
    (fields who ftd pointer?)
    (protocol
      (lambda (pargs->new)
        (lambda (whoid expr ftd pointer?)
          ((pargs->new expr) (syntax->datum whoid) ftd pointer?)))))

  (define-syntax use-foreign
    (syntax-rules ()
      [(_ op type info fptr offset val)
       (op 'type ($ftype-pointer-address fptr) offset val)]
      [(_ op type fptr offset)
       (op 'type ($ftype-pointer-address fptr) offset)]))
  (define-syntax multi-int
    (syntax-rules ()
      [(_ op type (fast-op arg ...))
       (constant-case native-endianness
         [(unknown) (use-foreign op type arg ...)]
         [else (fast-op arg ...)])]))
  (define-syntax wide
    (syntax-rules ()
      [(_ op type (fast-op arg ...))
       (constant-case ptr-bits
         [(64) (fast-op arg ...)]
         [(32) (use-foreign op type arg ...)])]))
  (define-syntax multi-int/wide
    (syntax-rules ()
      [(_ op type (fast-op arg ...))
       (constant-case ptr-bits
         [(64) (multi-int op type (fast-op arg ...))]
         [(32) (use-foreign op type arg ...)])]))    
  (define-syntax swapped-endianness
    (lambda (stx)
      (syntax-case stx ()
        [(_)
         (constant-case native-endianness
           [(little) #''big]
           [(big) #''little]
           [(unknown) #'(if (eq? (native-endianness) 'little)
                            'big
                            'little)])])))
  (define simplify-eness
    (lambda (eness type)
      (case type
        [(integer-8 unsigned-8) 'native]
        [else
         (case eness
           [(little) (constant-case native-endianness
                       [(big) 'swapped]
                       [(little) 'native]
                       [else eness])]
           [(big) (constant-case native-endianness
                    [(big) 'native]
                    [(little) 'swapped]
                    [else eness])]
           [else eness])])))
  (record-writer rtd/ftd
    (lambda (x p wr)
      (fprintf p "#<ftd ~s>" (record-type-name x))))
  (record-writer rtd/fptr
    (lambda (x p wr)
      (fprintf p "#<ftype-pointer ~s ~s>" (record-type-name (record-rtd x)) ($ftype-pointer-address x))))
  (set! $verify-ftype-address
    (lambda (who addr)
      (define address?
        (lambda (x)
          (constant-case address-bits
            [(32) ($integer-32? x)]
            [(64) ($integer-64? x)])))
      (unless (address? addr)
        (if (or (procedure? addr) (string? addr))
            ($oops who "non-function ftype with ~s address" addr)
            ($oops who "invalid address ~s" addr)))))
  (set! $verify-ftype-pointer
    (lambda (info fptr)
      (unless (record? fptr (ftd-info-ftd info))
        ($source-violation (ftd-info-who info) (src-info-src info) #t
          (if ($fptr? fptr)
              "ftype mismatch for ~s"
              "~s is not an ftype pointer")
          fptr))))
  (set! $invalid-ftype-index
    (lambda (info i)
      ($source-violation (index-info-who info) (src-info-src info) #t
        "invalid index ~s for ~:[~;indirection of ~]~s" i (index-info-pointer? info) (index-info-ftd info))))
  (set! $trans-define-ftype
    (lambda (x)
      (lambda (r)
        (syntax-case x ()
          [(_ ftype-name ftype)
           (identifier? #'ftype-name)
           #`(define-syntax ftype-name
               (make-compile-time-value
                 '#,(car (expand-ftype-defns r #'(ftype-name) #'(ftype)))))]
          [(_ [ftype-name ftype] ...)
           (andmap identifier? #'(ftype-name ...))
           (with-syntax ([(ftd ...) (expand-ftype-defns r #'(ftype-name ...) #'(ftype ...))])
             #'(begin
                 (define-syntax ftype-name
                   (make-compile-time-value 'ftd))
                 ...))]))))
  (set! $trans-make-ftype-pointer
    (lambda (x)
      (lambda (r)
        (syntax-case x ()
          [(_ ftype ?addr)
           (identifier? #'ftype)
           (let ([ftd (expand-ftype-name r #'ftype)])
             (with-syntax ([addr-expr
                            (if (ftd-function? ftd)
                                #`(let ([x ?addr])
                                    (cond
                                      ;; we need to make a code object, lock it, set addr to
                                      ;; (foreign-callable-entry-point code-object)
                                      [(procedure? x)
                                       (let ([co #,($make-foreign-callable 'make-ftype-pointer
                                                     (ftd-function-conv* ftd)
                                                     #'x
                                                     (map indirect-ftd-pointer (ftd-function-arg-type* ftd))
                                                     (indirect-ftd-pointer (ftd-function-result-type ftd)))])
                                         (lock-object co)
                                         (foreign-callable-entry-point co))]
                                      ;; otherwise, it is a string, so lookup the foreign-entry
                                      [(string? x) (foreign-entry x)]
                                      ;; otherwise, assume it is an address, let normal check
                                      ;; complain otherwise
                                      [else x]))
                                #'?addr)])
               #`($make-fptr '#,ftd
                   #,(if (or (fx= (optimize-level) 3)
                             (syntax-case #'addr-expr (ftype-pointer-address)
                               [(ftype-pointer-address x) #t]
                               [else #f]))
                         #'addr-expr
                         #'(let ([addr addr-expr])
                             ($verify-ftype-address 'make-ftype addr)
                             addr)))))]))))
  (set! $trans-ftype-pointer?
    (lambda (x)
      (lambda (r)
        (syntax-case x ()
          [(_ x) #`(record? x '#,rtd/fptr)]
          [(_ ftype x) (identifier? #'ftype) #`(record? x '#,(expand-ftype-name r #'ftype))]))))
  (set-who! ftype-pointer-address
    (lambda (fptr)
      (unless ($fptr? fptr) ($oops who "~s is not an ftype pointer" fptr))
      ($ftype-pointer-address fptr)))
  (set-who! ftype-pointer-null?
    (lambda (fptr)
      (unless ($fptr? fptr) ($oops who "~s is not an ftype pointer" fptr))
      (#3%ftype-pointer-null? fptr)))
  (set-who! ftype-pointer=?
    (lambda (fptr1 fptr2)
      (unless ($fptr? fptr1) ($oops who "~s is not an ftype pointer" fptr1))
      (unless ($fptr? fptr2) ($oops who "~s is not an ftype pointer" fptr2))
      (#3%ftype-pointer=? fptr1 fptr2)))
  (set-who! ftype-pointer-ftype
    (lambda (fptr)
      (unless ($fptr? fptr) ($oops who "~s is not an ftype pointer" fptr))
      (ftd-stype (record-rtd fptr))))
  (set-who! ftype-pointer->sexpr
    (lambda (fptr)
      (module (record replay)
        (define ht (make-eqv-hashtable))
        (define-syntax record
          (syntax-rules ()
            [(_ ?fptr expr)
             (let ([fptr ?fptr])
               (let ([addr (ftype-pointer-address fptr)])
                 (cond
                   [(hashtable-ref ht addr #f) => (lambda (x) fptr)]
                   [else
                    (hashtable-set! ht addr #t)
                    (let ([x expr])
                      (hashtable-set! ht addr x)
                      x)])))]))
        (define replay
          (lambda (x)
            (let f ([x x])
              (if ($fptr? x)
                  (hashtable-ref ht (ftype-pointer-address x) #f)
                  (begin
                    (when (pair? x)
                      (set-car! x (f (car x)))
                      (set-cdr! x (f (cdr x))))
                    x)))
            x)))
      (unless ($fptr? fptr) ($oops who "~s is not an ftype pointer" fptr))
      (replay
        (let fptr->sexpr ([fptr fptr])
          (record fptr
            (let f ([fptr fptr] [ftd (record-rtd fptr)] [offset 0])
              (cond
                [(ftd-struct? ftd)
                 `(struct
                    ,@(map (lambda (field)
                             (if (car field)
                                 `(,(car field) ,(f fptr (caddr field) (+ offset (cadr field))))
                                 '(_ _)))
                        (ftd-struct-field* ftd)))]
                [(ftd-union? ftd)
                 `(union
                    ,@(map (lambda (field)
                             (if (car field)
                                 `(,(car field) ,(f fptr (cdr field) offset))
                                 '(_ _)))
                        (ftd-union-field* ftd)))]
                [(ftd-array? ftd)
                 (let ([n (ftd-array-length ftd)]
                       [ftd (ftd-array-ftd ftd)])
                   (if (and (ftd-base? ftd) (memq (ftd-base-type ftd) '(char wchar)))
                       (let g ([i 0])
                         (if (fx= i n)
                             (make-string n)
                             (let ([c (f fptr ftd (+ offset (* i (ftd-size ftd))))])
                               (if (or (eq? c 'invalid) (eqv? c #\nul))
                                   (if (fx= i 0) `(array ,n invalid) (make-string i))
                                   (let ([s (g (fx+ i 1))])
                                     (string-set! s i c)
                                     s)))))
                       `(array ,n
                          ,@(let g ([i 0])
                              (if (fx= i n)
                                  '()
                                  (cons (f fptr ftd (+ offset (* i (ftd-size ftd))))
                                        (g (fx+ i 1))))))))]
                [(ftd-pointer? ftd)
                 (cond
                   [(guard (c [#t #f]) ($fptr-fptr-ref fptr offset (ftd-pointer-ftd ftd))) =>
                    (lambda (fptr)
                      (if (zero? (ftype-pointer-address fptr))
                          'null
                          (let ([ftd (ftd-pointer-ftd ftd)])
                            (if (and (ftd-base? ftd) (memq (ftd-base-type ftd) '(char wchar)))
                                (let g ([i 0])
                                  (let ([c (f fptr ftd (* i (ftd-size ftd)))])
                                    (if (or (eq? c 'invalid) (eqv? c #\nul))
                                        (if (fx= i 0) '(* invalid) (make-string i))
                                        (let ([s (g (fx+ i 1))])
                                          (string-set! s i c)
                                          s))))
                                `(* ,(fptr->sexpr fptr))))))]
                   [else 'invalid])]
                [(ftd-function? ftd)
                 (let ([addr (ftype-pointer-address fptr)])
                   `(function ,(or (foreign-address-name addr) addr)))]
                [(ftd-bits? ftd)
                 (let ([type (unsigned-type (ftd-size ftd))])
                   `(bits
                      ,@(map (lambda (field)
                               (apply
                                 (lambda (id signed? start end)
                                   (if id
                                       `(,id
                                         ,(guard (c [#t 'invalid])
                                            ($fptr-ref-bits type (ftd-bits-eness ftd) signed?
                                              fptr offset start end)))
                                       '(_ _)))
                                 field))
                          (ftd-bits-field* ftd))))]
                [(ftd-base? ftd)
                 (guard (c [#t 'invalid])
                   ($fptr-ref (filter-foreign-type (ftd-base-type ftd))
                     (ftd-base-eness ftd) fptr offset))]
                [else ($oops '$fptr->sexpr "unhandled ftd ~s" ftd)])))))))
  (set! $unwrap-ftype-pointer
    (lambda (fptr)
      (let f ([ftd (record-rtd fptr)])
        (cond
          [(ftd-struct? ftd)
           `(struct
              ,@(map (lambda (field)
                       `(,(car field) . ,($fptr-&ref fptr (cadr field) (caddr field))))
                  (ftd-struct-field* ftd)))]
          [(ftd-union? ftd)
           `(union
              ,@(map (lambda (field)
                       `(,(car field) . ,($fptr-&ref fptr 0 (cdr field))))
                  (ftd-union-field* ftd)))]
          [(ftd-array? ftd)
           (let ([n (ftd-array-length ftd)]
                 [ftd (ftd-array-ftd ftd)])
             `(array ,n
                ,(lambda (i)
                   (unless (and (fixnum? i) (if (fx= n 0) (fx>= i 0) ($fxu< i n)))
                     (errorf '$dump-foreign-type "invalid index ~s for array of length ~s" i n))
                   ($fptr-&ref fptr (* i (ftd-size ftd)) ftd))))]
          [(ftd-pointer? ftd)
           (let ([ftd (ftd-pointer-ftd ftd)])
             `(* ,(lambda () ($fptr-fptr-ref fptr 0 ftd))
                 ,(lambda (who v)
                    ($verify-ftype-pointer (make-ftd-info who #f ftd) v)
                    (#3%$fptr-fptr-set! fptr 0 v))))]
          [(ftd-function? ftd)
           (let ([addr (ftype-pointer-address fptr)])
             `(function ,(foreign-address-name addr)))]
          [(ftd-bits? ftd)
           (let ([type (unsigned-type (ftd-size ftd))])
             `(bits
                ,@(map (lambda (field)
                         (apply
                           (lambda (id signed? start end)
                             `(,id ,(lambda ()
                                      (guard (c [#t 'invalid])
                                        ($fptr-ref-bits type (ftd-bits-eness ftd) signed? fptr 0 start end)))
                                   ,(lambda (v)
                                      (#2%$fptr-set-bits! type (ftd-bits-eness ftd) fptr 0
                                        start end v))))
                           field))
                    (ftd-bits-field* ftd))))]
          [(ftd-base? ftd)
           (let ([type (filter-foreign-type (ftd-base-type ftd))])
             `(base
                ,type
                ,(lambda () (guard (c [#t 'invalid]) ($fptr-ref type (ftd-base-eness ftd) fptr 0)))
                ,(lambda (v) (#2%$fptr-set! (ftd-base-type ftd) type (ftd-base-eness ftd) fptr 0 v))))]
          [else ($oops '$unwrap-ftype-pointer "unhandled ftd ~s" ftd)]))))
  (set! $trans-ftype-sizeof
    (lambda (x)
      (lambda (r)
        (syntax-case x ()
          [(_ ftype)
           (identifier? #'ftype)
           (let ([ftd (expand-ftype-name r #'ftype)])
             (when (ftd-function? ftd)
               ($oops 'ftype-sizeof "function ftypes have unknown size"))
             ($ftd-size ftd))]))))
  (set! $ftd?
    (lambda (x)
      (ftd? x)))
  (set! $ftd-size
    (lambda (ftd)
      (constant-case special-initial-field-alignment?
        [(#f) (ftd-size ftd)]
        [else
         ;; PPC32 Mac OS: if the first field of a compound type is size 8,
         ;; then size is rounded up to an alignment of 8. This doesn't apply
         ;; if the compound type is inside another one and not at the start.
         (let ([initial (let loop ([ftd ftd])
                          (cond
                           [(ftd-struct? ftd)
                            (loop (caddr (car (ftd-struct-field* ftd))))]
                           [(ftd-union? ftd)
                            (apply max (map (lambda (f) (loop (cdr f))) (ftd-union-field* ftd)))]
                           [(ftd-array? ftd)
                            (loop (ftd-array-ftd ftd))]
                           [else (ftd-size ftd)]))])
           (if (fx= initial 8)
               (fxlogand (fx+ (ftd-size ftd) 7) (fxlognot 7))
               (ftd-size ftd)))])))
  (set! $ftd-as-box? ; represents `(& <ftype>)` from `$expand-fp-ftype`
    (lambda (x)
      (and (box? x) (ftd? (unbox x)))))
  (set! $ftd-alignment
    (lambda (x)
      (ftd-alignment x)))
  (set! $ftd-compound?
    (lambda (x)
      (or (ftd-struct? x)
          (ftd-union? x)
          (ftd-array? x))))
  (set! $ftd-union?
    (lambda (x)
      (or (ftd-union? x)
          (and (ftd-struct? x)
               (ormap (lambda (f) ($ftd-union? (caddr f)))
                      (ftd-struct-field* x)))
          (and (ftd-array? x)
               ($ftd-union? (ftd-array-ftd x))))))
  (set! $ftd-unsigned?
    (lambda (x)
      (and (ftd-base? x)
           (case (ftd-base-type x)
             [(unsigned-8 unsigned-16 unsigned-32 unsigned-64) #t]
             [else #f]))))
  (set! $ftd->members
    (lambda (x)
      ;; Currently used for x86_64 and arm32 ABI: Returns a list of
      ;;  (list 'integer/'float size offset)
      (reverse
       (let loop ([x x] [offset 0] [accum '()])
         (cond
           [(ftd-base? x)
            (cons (list (case (ftd-base-type x)
                          [(double double-float float single-float)
                           'float]
                          [else 'integer])
                        (ftd-size x)
                        offset)
                  accum)]
           [(ftd-struct? x)
            (let struct-loop ([field* (ftd-struct-field* x)] [accum accum])
              (cond
                [(null? field*) accum]
                [else (let* ([fld (car field*)]
                             [sub-ftd (caddr fld)]
                             [sub-offset (cadr fld)])
                        (struct-loop (cdr field*)
                                     (loop sub-ftd (+ offset sub-offset) accum)))]))]
           [(ftd-union? x)
            (let union-loop ([field* (ftd-union-field* x)] [accum accum])
              (cond
                [(null? field*) accum]
                [else (let* ([fld (car field*)]
                             [sub-ftd (cdr fld)])
                        (union-loop (cdr field*)
                                    (loop sub-ftd offset accum)))]))]
           [(ftd-array? x)
            (let ([elem-ftd (ftd-array-ftd x)])
              (let array-loop ([len (ftd-array-length x)] [offset offset] [accum accum])
                (cond
                  [(fx= len 0) accum]
                  [else (array-loop (fx- len 1)
                                    (+ offset (ftd-size elem-ftd))
                                    (loop elem-ftd offset accum))])))]
           [else (cons (list 'integer (ftd-size x) offset) accum)])))))
  (set! $ftd-atomic-category
    (lambda (x)
      ;; Currently used for PowerPC32 ABI
      (cond
       [(ftd-base? x)
	(case (ftd-base-type x)
	  [(double double-float float single-float)
	   'float]
	  [(unsigned-short unsigned unsigned-int
			   unsigned-long unsigned-long-long
			   unsigned-8 unsigned-16 unsigned-32 unsigned-64)
	   'unsigned]
	  [else 'integer])]
       [else 'integer])))
  (set! $ftd-ffi-encode ;; for pb libffi binding
    (lambda (x)
      (cond
        [(ftd-base? x)
	 (case (ftd-base-type x)
	   [(double double-float) (constant ffi-typerep-double)]
	   [(float single-float) (constant ffi-typerep-float)]
           [(integer-8) (constant ffi-typerep-sint8)]
           [(unsigned-8) (constant ffi-typerep-uint8)]
           [(integer-16 short) (constant ffi-typerep-sint16)]
           [(unsigned-16 unsigned-short) (constant ffi-typerep-uint16)]
           [(integer-32 int) (constant ffi-typerep-sint32)]
           [(unsigned-32 unsigned-int) (constant ffi-typerep-uint32)]
           [(integer-64) (constant ffi-typerep-sint64)]
           [(unsigned-64) (constant ffi-typerep-uint64)]
           [else (constant ffi-typerep-pointer)])]
        [(ftd-struct? x)
         (list->vector
          (let struct-loop ([field* (ftd-struct-field* x)])
            (cond
              [(null? field*) '()]
              [else (let* ([fld (car field*)]
                           [sub-ftd (caddr fld)])
                      (cons ($ftd-ffi-encode sub-ftd)
                            (struct-loop (cdr field*))))])))]
        [(ftd-union? x)
         (let union-loop ([field* (ftd-union-field* x)])
           (cond
             [(null? field*) '()]
             [else (let* ([fld (car field*)]
                          [sub-ftd (cdr fld)])
                     (cons ($ftd-ffi-encode sub-ftd)
                           (union-loop (cdr field*))))]))]
        [(ftd-array? x)
         (let ([elem-ftd (ftd-array-ftd x)])
           (cons ($ftd-ffi-encode elem-ftd)
                 (ftd-array-length x)))]
        [else (constant ffi-typerep-pointer)])))
  (set! $expand-fp-ftype ; for foreign-procedure, foreign-callable
    (lambda (who what r ftype)
      (indirect-ftd-pointer
        (expand-fp-ftype who what r ftype '()))))
  (let ()
    (define-who ftype-access-code
      (lambda (whoid ftd a* fptr-expr offset)
        (let loop ([ftd ftd] [a* a*] [fptr-expr fptr-expr] [offset offset] [idx* '()])
          (if (null? a*)
              (values fptr-expr offset ftd idx* #f)
              (let ([a (car a*)])
                (cond
                  [(ftd-struct? ftd)
                   (let ([s (syntax->datum a)])
                     (cond
                       [(and (symbol? s) (assq s (ftd-struct-field* ftd))) =>
                        (lambda (field)
                          (let ([offset #`(#3%fx+ #,offset #,(cadr field))] [ftd (caddr field)])
                            (loop ftd (cdr a*) fptr-expr offset idx*)))]
                       [else (syntax-error a "unexpected accessor")]))]
                  [(ftd-union? ftd)
                   (let ([s (syntax->datum a)])
                     (cond
                       [(and (symbol? s) (assq s (ftd-union-field* ftd))) =>
                        (lambda (field)
                          (let ([ftd (cdr field)])
                            (loop ftd (cdr a*) fptr-expr offset idx*)))]
                       [else (syntax-error a "unexpected accessor")]))]
                  [(ftd-array? ftd)
                   (let ([elt-ftd (ftd-array-ftd ftd)] [len (ftd-array-length ftd)])
                     (if (memv (syntax->datum a) '(* 0))
                         (loop elt-ftd (cdr a*) fptr-expr offset idx*)
                         (let ([a-id (car (generate-temporaries (list #'i)))])
                           (loop elt-ftd (cdr a*) fptr-expr
                             #`(#3%fx+ #,offset (#3%fx* #,a-id #,(ftd-size elt-ftd)))
                             (cons (list ftd a-id a len) idx*)))))]
                  [(ftd-pointer? ftd)
                   (let ([elt-ftd (ftd-pointer-ftd ftd)])
                     (let ([fptr-expr #`(#3%$fptr-fptr-ref #,fptr-expr #,offset '#,elt-ftd)])
                        (if (memv (syntax->datum a) '(* 0))
                            (loop elt-ftd (cdr a*) fptr-expr 0 idx*)
                            (let ([a-id (car (generate-temporaries (list #'i)))])
                              (loop elt-ftd (cdr a*) fptr-expr
                                (trans-idx a-id a elt-ftd (make-index-info whoid a ftd #f))
                                (cons (list ftd a-id a #f) idx*))))))]
                  [(ftd-bits? ftd)
                   (let ([s (syntax->datum a)])
                     (cond
                       [(and (symbol? s) (assq s (ftd-bits-field* ftd))) =>
                        (lambda (field)
                          (unless (null? (cdr a*))
                            (syntax-error (cadr a*) "unexpected accessor"))
                          (values fptr-expr offset ftd idx* field))]
                       [else (syntax-error a "unexpected accessor")]))]
                  [(ftd-base? ftd) (syntax-error a "unexpected accessor")]
                  [(ftd-function? ftd) (syntax-error a "unexpected accessor")]
                  [else ($oops who "unhandled ftd ~s" ftd)]))))))
    (define trans-bitfield
      (lambda (ftd signed? offset start end do-base do-bits)
        (define (little-endian?)
          (or (eq? (ftd-bits-eness ftd) 'little)
              (and (eq? (constant native-endianness) 'little)
                   (eq? (ftd-bits-eness ftd) 'native))
              (and (eq? (constant native-endianness) 'big)
                   (eq? (ftd-bits-eness ftd) 'swapped))))
        (let ([width (fx- end start)])
          (cond
            [(and (fx= width 8) (fx= (mod start 8) 0))
             (do-base (if signed? 'integer-8 'unsigned-8) #f
               #`(fx+ #,offset
                      #,(if (little-endian?)
                            (div start 8)
                            (fx- (ftd-size ftd) (div start 8) 1))))]
            [(and (fx= width 16) (fx= (mod start 16) 0))
             (do-base (if signed? 'integer-16 'unsigned-16) (ftd-bits-eness ftd)
               #`(fx+ #,offset
                      #,(if (little-endian?)
                            (div start 8)
                            (fx- (ftd-size ftd) (div start 8) 2))))]
            [(and (fx= width 32) (fx= (mod start 32) 0))
             (do-base (if signed? 'integer-32 'unsigned-32) (ftd-bits-eness ftd)
               #`(fx+ #,offset
                      #,(if (little-endian?)
                            (div start 8)
                            (fx- (ftd-size ftd) (div start 8) 4))))]
            [(and (fx= width 64) (fx= start 0))
             (do-base (if signed? 'integer-64 'unsigned-64) (ftd-bits-eness ftd) offset)]
            [else
             (or (and (and (fx= (ftd-size ftd) 8) (fx= (constant ptr-bits) 32))
                      (cond
                        [(and (fx>= start 0) (fx<= end 32))
                         (do-bits 4 (if (little-endian?) offset #`(fx+ #,offset 4)) start end)]
                        [(and (fx>= start 32) (fx<= end 64))
                         (do-bits 4 (if (little-endian?) #`(fx+ #,offset 4) offset) (fx- start 32) (fx- end 32))]
                        [else #f]))
                 (do-bits (ftd-size ftd) offset start end))]))))
    (define trans-idx
      (lambda (?idx ?orig-idx ftd info)
        (if (memv (syntax->datum ?idx) '(* 0))
            0
            (if (ftd-function? ftd)
                (syntax-error ?orig-idx "cannot calculate offset for function index")
                (let ([size (ftd-size ftd)])
                  (if (fx= (optimize-level) 3)
                      #`(#3%fx* #,size #,?idx)
                      #`(let ([idx #,?idx])
                          (or (and (fixnum? idx)
                                   (let ([offset (* #,size idx)])
                                     (and (fixnum? offset)
                                          (fixnum? (+ offset #,(fx- size 1)))
                                          offset)))
                              ($invalid-ftype-index '#,info idx)))))))))
    (set! $trans-ftype-&ref
      (lambda (q)
        (define trans
          (lambda (ftype a* fptr-expr ?idx)
           (lambda (r)
             (let ([ftd (expand-ftype-name r ftype)])
               (let ([fptr-expr (if (fx= (optimize-level) 3)
                                    fptr-expr
                                    #`(let ([fptr #,fptr-expr])
                                        ($verify-ftype-pointer '#,(make-ftd-info 'ftype-&ref fptr-expr ftd) fptr)
                                        fptr))])
                 (if (and (null? a*) (memv (syntax->datum ?idx) '(* 0)))
                     fptr-expr
                     #`(let ([offset #,(trans-idx ?idx ?idx ftd (make-index-info #'ftype-&ref ?idx ftd #t))])
                         #,(let-values ([(fptr-expr offset ftd idx* bitfield)
                                         (ftype-access-code #'ftype-&ref ftd a* fptr-expr #'offset)])
                             (when bitfield (syntax-error q "cannot take address of bit field"))
                             (with-syntax ([((containing-ftd a-id a len) ...) idx*])
                               (with-syntax ([(info ...) (map (lambda (a containing-ftd) (make-index-info 'ftype-&ref a containing-ftd #f)) #'(a ...) #'(containing-ftd ...))])
                                 #`(let ([a-id a] ...)
                                     (unless (or #,(fx= (optimize-level) 3) (not len))
                                       (unless (and (fixnum? a-id) (if (eqv? len 0) (fx>= a-id 0) ($fxu< a-id len)))
                                         ($invalid-ftype-index 'info a-id)))
                                     ...
                                     (#3%$fptr-&ref #,fptr-expr #,offset '#,ftd))))))))))))
        (syntax-case q ()
          [(_ ftype (a ...) fptr-expr)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr 0)]
          [(_ ftype (a ...) fptr-expr ?idx)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr #'?idx)])))
    (set! $trans-ftype-ref
      (lambda (q)
        (define trans
          (lambda (ftype a* fptr-expr ?idx)
            (lambda (r)
              (let ([ftd (expand-ftype-name r ftype)])
                (let ([fptr-expr (if (fx= (optimize-level) 3)
                                     fptr-expr
                                     #`(let ([fptr #,fptr-expr])
                                         ($verify-ftype-pointer '#,(make-ftd-info 'ftype-ref fptr-expr ftd) fptr)
                                         fptr))])
                  #`(let ([offset #,(trans-idx ?idx ?idx ftd (make-index-info #'ftype-ref ?idx ftd #t))])
                      #,(let-values ([(fptr-expr offset ftd idx* bitfield)
                                      (ftype-access-code #'ftype-ref ftd a* fptr-expr #'offset)])
                          (define (do-base type eness offset)
                            (let ([eness (simplify-eness eness type)])
                              (case eness
                                [(native swapped)
                                 (with-syntax ([$fptr-ref-x (datum->syntax #'kwd
                                                              (string->symbol
                                                               (format "$fptr-ref-~:[~;swap-~]~a"
                                                                       (eq? eness 'swapped) type)))])
                                   #`(#3%$fptr-ref-x #,fptr-expr #,offset))]
                                [else
                                 (with-syntax ([type (datum->syntax #'kwd type)]
                                               [eness (datum->syntax #'kwd eness)])
                                   #`(#3%$fptr-ref 'type 'eness #,fptr-expr #,offset))])))
                          (with-syntax ([((containing-ftd a-id a len) ...) idx*])
                            (with-syntax ([(info ...) (map (lambda (a containing-ftd) (make-index-info 'ftype-ref a containing-ftd #f)) #'(a ...) #'(containing-ftd ...))])
                              #`(let ([a-id a] ...)
                                  (unless (or #,(fx= (optimize-level) 3) (not len))
                                    (unless (and (fixnum? a-id) (if (eqv? len 0) (fx>= a-id 0) ($fxu< a-id len)))
                                      ($invalid-ftype-index 'info a-id)))
                                  ...
                                  #,(cond
                                      [bitfield
                                        (apply
                                          (lambda (id signed? start end)
                                            (trans-bitfield ftd signed? offset start end do-base
                                              (lambda (size offset start end)
                                                (let ([eness (simplify-eness (ftd-bits-eness ftd) (unsigned-type size))])
                                                  (case eness
                                                    [(native swapped)
                                                     (with-syntax ([$fptr-ref-bits-x (datum->syntax #'*
                                                                                       (string->symbol
                                                                                        (format "$fptr-ref-~:[u~;i~]bits-~:[~;swap-~]~a"
                                                                                                signed?
                                                                                                (eq? eness 'swapped)
                                                                                                (unsigned-type size))))])
                                                       #`(#3%$fptr-ref-bits-x #,fptr-expr #,offset #,start #,end))]
                                                    [else
                                                     (with-syntax ([type (datum->syntax #'kwd (unsigned-type size))]
                                                                   [eness (datum->syntax #'* eness)])
                                                       #`(#3%$fptr-ref-bits 'type 'eness '#,signed? #,fptr-expr #,offset #,start #,end))])))))
                                          bitfield)]
                                      [(ftd-base? ftd) (do-base (filter-foreign-type (ftd-base-type ftd)) (ftd-base-eness ftd) offset)]
                                      [(ftd-pointer? ftd) #`(#3%$fptr-fptr-ref #,fptr-expr #,offset '#,(ftd-pointer-ftd ftd))]
                                      [(ftd-function? ftd) 
                                       ($make-foreign-procedure 'make-ftype-pointer
                                         (ftd-function-conv* ftd)
                                         #f
                                         #`($fptr-offset-addr #,fptr-expr offset)
                                         (map indirect-ftd-pointer (ftd-function-arg-type* ftd))
                                         (indirect-ftd-pointer (ftd-function-result-type ftd)))]
                                      [else (syntax-error q "non-scalar value cannot be referenced")])))))))))))
        (syntax-case q ()
          [(_ ftype (a ...) fptr-expr)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr 0)]
          [(_ ftype (a ...) fptr-expr ?idx)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr #'?idx)])))
    (set! $trans-ftype-set!
      (lambda (q)
        (define trans
          (lambda (ftype a* fptr-expr ?idx val-expr)
            (lambda (r)
              (let ([ftd (expand-ftype-name r ftype)])
                (let ([fptr-expr (if (fx= (optimize-level) 3)
                                     fptr-expr
                                     #`(let ([fptr #,fptr-expr])
                                         ($verify-ftype-pointer '#,(make-ftd-info 'ftype-set! fptr-expr ftd) fptr)
                                         fptr))])
                  #`(let ([offset #,(trans-idx ?idx ?idx ftd (make-index-info #'ftype-set! ?idx ftd #t))] [val #,val-expr])
                      #,(let-values ([(fptr-expr offset ftd idx* bitfield)
                                      (ftype-access-code #'ftype-set! ftd a* fptr-expr #'offset)])
                          (define (do-base orig-type)
                            (lambda (type eness offset)
                              (let ([eness (simplify-eness eness type)])
                                (case eness
                                  [(native swapped)
                                   (with-syntax ([$fptr-set-x! (datum->syntax #'kwd
                                                                 (string->symbol
                                                                  (format "$fptr-set-~:[~;swap-~]~a!"
                                                                          (eq? eness 'swapped) type)))])
                                     #`($fptr-set-x! '#,(make-field-info orig-type val-expr) #,fptr-expr #,offset val))]
                                  [else
                                   (with-syntax ([type (datum->syntax #'kwd type)]
                                                 [eness (datum->syntax #'kwd eness)])
                                     #`($fptr-set! '#,(make-field-info orig-type val-expr) 'type 'eness #,fptr-expr #,offset val))]))))
                          (with-syntax ([((containing-ftd a-id a len) ...) idx*])
                            (with-syntax ([(info ...) (map (lambda (a containing-ftd) (make-index-info 'ftype-set! a containing-ftd #f)) #'(a ...) #'(containing-ftd ...))])
                              #`(let ([a-id a] ...)
                                  (unless (or #,(fx= (optimize-level) 3) (not len))
                                    (unless (and (fixnum? a-id) (if (eqv? len 0) (fx>= a-id 0) ($fxu< a-id len)))
                                      ($invalid-ftype-index 'info a-id)))
                                  ...
                                  #,(cond
                                      [bitfield
                                        (apply
                                          (lambda (id signed? start end)
                                            (trans-bitfield ftd signed? offset start end (do-base 'bit-field)
                                              (lambda (size offset start end)
                                                (let ([eness (simplify-eness (ftd-bits-eness ftd) (unsigned-type size))])
                                                  (case eness
                                                    [(native swapped)
                                                     (with-syntax ([$fptr-set-bits-x! (datum->syntax #'*
                                                                                        (string->symbol
                                                                                         (format "$fptr-set-bits-~:[~;swap-~]~a!"
                                                                                           (eq? eness 'swapped)
                                                                                           (unsigned-type size))))])
                                                       #`($fptr-set-bits-x! #,fptr-expr #,offset #,start #,end val))]
                                                    [else
                                                     (with-syntax ([type (datum->syntax #'kwd (unsigned-type size))]
                                                                   [eness (datum->syntax #'* eness)])
                                                       #`($fptr-set-bits! 'type 'eness #,fptr-expr #,offset #,start #,end val))])))))
                                          bitfield)]
                                      [(ftd-base? ftd)
                                       (let ([orig-type (ftd-base-type ftd)])
                                         ((do-base orig-type) (filter-foreign-type orig-type) (ftd-base-eness ftd) offset))]
                                      [(ftd-pointer? ftd)
                                       #`(begin
                                           (unless #,(fx= (optimize-level) 3)
                                             ($verify-ftype-pointer '#,(make-ftd-info 'ftype-set! val-expr (ftd-pointer-ftd ftd)) val))
                                           (#3%$fptr-fptr-set! #,fptr-expr #,offset val))]
                                      [else (syntax-error q "non-scalar value cannot be assigned")])))))))))))
        (syntax-case q ()
          [(_ ftype (a ...) fptr-expr val-expr)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr 0 #'val-expr)]
          [(_ ftype (a ...) fptr-expr ?idx val-expr)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr #'?idx #'val-expr)])))
    (set-who! $trans-ftype-locked-op!
      (lambda (who q prim)
        (define trans
          (lambda (ftype a* fptr-expr ?idx)
            (lambda (r)
              (let ([ftd (expand-ftype-name r ftype)])
                (let ([fptr-expr (if (fx= (optimize-level) 3)
                                     fptr-expr
                                     #`(let ([fptr #,fptr-expr])
                                         ($verify-ftype-pointer '#,(make-ftd-info who fptr-expr ftd) fptr)
                                         fptr))])
                  #`(let ([offset #,(trans-idx ?idx ?idx ftd (make-index-info who ?idx ftd #t))])
                      #,(let-values ([(fptr-expr offset ftd idx* bitfield)
                                      (ftype-access-code who ftd a* fptr-expr #'offset)])
                          (with-syntax ([((containing-ftd a-id a len) ...) idx*])
                            (with-syntax ([(info ...) (map (lambda (a containing-ftd) (make-index-info who a containing-ftd #f)) #'(a ...) #'(containing-ftd ...))])
                              #`(let ([a-id a] ...)
                                  (unless (or #,(fx= (optimize-level) 3) (not len))
                                    (unless (and (fixnum? a-id) (if (eqv? len 0) (fx>= a-id 0) ($fxu< a-id len)))
                                      ($invalid-ftype-index 'info a-id)))
                                  ...
                                  #,(cond
                                      [(ftd-base? ftd)
                                       (let ([type (filter-foreign-type (ftd-base-type ftd))])
                                         (unless (memq type
                                                   (constant-case ptr-bits
                                                     [(64) '(unsigned-64 integer-64)]
                                                     [(32) '(unsigned-32 integer-32)]))
                                           (syntax-error q "locked operation on non-integer or non-word-size field unsupported"))
                                         (unless (eq? (ftd-base-eness ftd) 'native)
                                           (syntax-error q "locked operation on non-native field unsupported"))
                                         #`(($primitive 3 #,prim) #,fptr-expr #,offset))]
                                      [else (syntax-error q "locked operation on non-base-type field unsupported")])))))))))))
        (syntax-case q ()
          [(_ ftype (a ...) fptr-expr)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr 0)]
          [(_ ftype (a ...) fptr-expr ?idx)
           (identifier? #'ftype)
           (trans #'ftype #'(a ...) #'fptr-expr #'?idx)])))
    (set! $trans-ftype-guardian
      (lambda (q)
        (lambda (r)
          (syntax-case q ()
            [(_ ftype)
             (identifier? #'ftype)
             (let ([ftd (expand-ftype-name r #'ftype)])
               (unless (let lockable? ([ftd ftd])
                         (cond
                           [(ftd-base? ftd)
                            (let ([type (filter-foreign-type (ftd-base-type ftd))])
                              (and (memq type
                                     (constant-case ptr-bits
                                       [(64) '(unsigned-64 integer-64)]
                                       [(32) '(unsigned-32 integer-32)]))
                                   (eq? 'native (ftd-base-eness ftd))))]
                           [(ftd-struct? ftd)
                            (let ([ls (ftd-struct-field* ftd)])
                              (if (null? ls)
                                  #f
                                  (lockable? (caddr (car ls)))))]
                           [(ftd-union? ftd) (ormap lockable? (map cdr (ftd-union-field* ftd)))]
                           [(ftd-array? ftd) (lockable? (ftd-array-ftd ftd))]
                           [else #f]))
                 (syntax-error q "first field must be a word-sized integer with native endianness"))
               #`(($primitive #,(if (fx= (optimize-level) 3) 3 2) $make-ftype-guardian) '#,ftd))])))))
 ; procedural entry point for inspector to simplify bootstrapping
  (set! $ftype-pointer? (lambda (x) ($fptr? x)))
  (set! $make-fptr
    (lambda (ftd addr)
      (#2%$make-fptr ftd addr)))
  (set! $fptr-offset-addr
    (lambda (fptr offset)
      (#3%$fptr-offset-addr fptr offset)))
  (set! $fptr-&ref
    (lambda (fptr offset ftd)
      (#3%$fptr-&ref fptr offset ftd)))
  (set! $fptr-fptr-ref
    (lambda (fptr offset ftd)
      (#3%$fptr-fptr-ref fptr offset ftd)))

  (set! $fptr-ref-integer-8
    (lambda (fptr offset)
      (#3%$fptr-ref-integer-8 fptr offset)))
  (set! $fptr-ref-unsigned-8
    (lambda (fptr offset)
      (#3%$fptr-ref-unsigned-8 fptr offset)))

  (set! $fptr-ref-integer-16
    (lambda (fptr offset)
      (#3%$fptr-ref-integer-16 fptr offset)))
  (set! $fptr-ref-unsigned-16
    (lambda (fptr offset)
      (#3%$fptr-ref-unsigned-16 fptr offset)))
  (set! $fptr-ref-swap-integer-16
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-integer-16 fptr offset)))
  (set! $fptr-ref-swap-unsigned-16
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-unsigned-16 fptr offset)))

  (set! $fptr-ref-integer-24
    (lambda (fptr offset)
      (multi-int foreign-ref integer-24
                 (#3%$fptr-ref-integer-24 fptr offset))))
  (set! $fptr-ref-unsigned-24
    (lambda (fptr offset)
      (multi-int foreign-ref unsigned-24
                 (#3%$fptr-ref-unsigned-24 fptr offset))))
  (set! $fptr-ref-swap-integer-24
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref integer-24
                 (#3%$fptr-ref-swap-integer-24 fptr offset))))
  (set! $fptr-ref-swap-unsigned-24
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref unsigned-24
                 (#3%$fptr-ref-swap-unsigned-24 fptr offset))))

  (set! $fptr-ref-integer-32
    (lambda (fptr offset)
      (#3%$fptr-ref-integer-32 fptr offset)))
  (set! $fptr-ref-unsigned-32
    (lambda (fptr offset)
      (#3%$fptr-ref-unsigned-32 fptr offset)))
  (set! $fptr-ref-swap-integer-32
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-integer-32 fptr offset)))
  (set! $fptr-ref-swap-unsigned-32
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-unsigned-32 fptr offset)))

  (set! $fptr-ref-integer-40
    (lambda (fptr offset)
      (multi-int foreign-ref integer-40
                 (#3%$fptr-ref-integer-40 fptr offset))))
  (set! $fptr-ref-unsigned-40
    (lambda (fptr offset)
      (multi-int foreign-ref unsigned-40
                 (#3%$fptr-ref-unsigned-40 fptr offset))))
  (set! $fptr-ref-swap-integer-40
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref integer-40
                 (#3%$fptr-ref-swap-integer-40 fptr offset))))
  (set! $fptr-ref-swap-unsigned-40
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref unsigned-40
                 (#3%$fptr-ref-swap-unsigned-40 fptr offset))))

  (set! $fptr-ref-integer-48
    (lambda (fptr offset)
      (multi-int foreign-ref integer-48
                 (#3%$fptr-ref-integer-48 fptr offset))))
  (set! $fptr-ref-unsigned-48
    (lambda (fptr offset)
      (multi-int foreign-ref unsigned-48
                 (#3%$fptr-ref-unsigned-48 fptr offset))))
  (set! $fptr-ref-swap-integer-48
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref integer-48
                 (#3%$fptr-ref-swap-integer-48 fptr offset))))
  (set! $fptr-ref-swap-unsigned-48
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref unsigned-48
                 (#3%$fptr-ref-swap-unsigned-48 fptr offset))))

  (set! $fptr-ref-integer-56
    (lambda (fptr offset)
      (multi-int foreign-ref integer-56
                 (#3%$fptr-ref-integer-56 fptr offset))))
  (set! $fptr-ref-unsigned-56
    (lambda (fptr offset)
      (multi-int foreign-ref unsigned-56
                 (#3%$fptr-ref-unsigned-56 fptr offset))))
  (set! $fptr-ref-swap-integer-56
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref integer-56
                 (#3%$fptr-ref-swap-integer-56 fptr offset))))
  (set! $fptr-ref-swap-unsigned-56
    (lambda (fptr offset)
      (multi-int $foreign-swap-ref unsigned-56
                 (#3%$fptr-ref-swap-unsigned-56 fptr offset))))

  (set! $fptr-ref-integer-64
    (lambda (fptr offset)
      (#3%$fptr-ref-integer-64 fptr offset)))
  (set! $fptr-ref-unsigned-64
    (lambda (fptr offset)
      (#3%$fptr-ref-unsigned-64 fptr offset)))
  (set! $fptr-ref-swap-integer-64
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-integer-64 fptr offset)))
  (set! $fptr-ref-swap-unsigned-64
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-unsigned-64 fptr offset)))

  (set! $fptr-ref-double-float
    (lambda (fptr offset)
      (#3%$fptr-ref-double-float fptr offset)))
  (set! $fptr-ref-swap-double-float
    (lambda (fptr offset)
      (constant-case ptr-bits
        [(64) (#3%$fptr-ref-swap-double-float fptr offset)]
        [(32) (let ([bv (make-bytevector 8)])
                (bytevector-u64-set! bv 0
                  (foreign-ref 'unsigned-64 ($ftype-pointer-address fptr) offset)
                  (swapped-endianness))
                ($object-ref 'double-float bv (constant bytevector-data-disp)))])))
    
  (set! $fptr-ref-single-float
    (lambda (fptr offset)
      (#3%$fptr-ref-single-float fptr offset)))
  (set! $fptr-ref-swap-single-float
    (lambda (fptr offset)
      (constant-case ptr-bits
        [(64) (#3%$fptr-ref-swap-single-float fptr offset)]
        [(32) (let ([bv (make-bytevector 4)])
                (bytevector-u32-set! bv 0
                  (foreign-ref 'unsigned-32 ($ftype-pointer-address fptr) offset)
                  (swapped-endianness))
                ($object-ref 'single-float bv (constant bytevector-data-disp)))])))
    
  (set! $fptr-ref-char
    (lambda (fptr offset)
      (#3%$fptr-ref-char fptr offset)))

  (set! $fptr-ref-wchar
    (lambda (fptr offset)
      (#3%$fptr-ref-wchar fptr offset)))
  (set! $fptr-ref-swap-wchar
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-wchar fptr offset)))

  (set! $fptr-ref-boolean
    (lambda (fptr offset)
      (#3%$fptr-ref-boolean fptr offset)))
  (set! $fptr-ref-swap-boolean
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-boolean fptr offset)))

  (set! $fptr-ref-stdbool
    (lambda (fptr offset)
      (#3%$fptr-ref-stdbool fptr offset)))
  (set! $fptr-ref-swap-stdbool
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-stdbool fptr offset)))

  (set! $fptr-ref-fixnum
    (lambda (fptr offset)
      (#3%$fptr-ref-fixnum fptr offset)))
  (set! $fptr-ref-swap-fixnum
    (lambda (fptr offset)
      (#3%$fptr-ref-swap-fixnum fptr offset)))

  (set-who! $fptr-ref
    (lambda (ty eness fptr offset)
      (define-syntax proc
        (lambda (x)
          (syntax-case x (scheme-object)
            [(_ scheme-object bytes pred) #'($oops who "unexpected type ~s" ty)]
            [(_ type bytes pred)
             (if (memq (datum type) '(char integer-8 unsigned-8))
                 (datum->syntax #'*
                   (string->symbol
                     (format "$fptr-ref-~a" (datum type))))
                 #`(if (or (eq? eness 'swapped)
                           (eq? eness (swapped-endianness)))
                       #,(datum->syntax #'*
                           (string->symbol
                             (format "$fptr-ref-swap-~a" (datum type))))
                       #,(datum->syntax #'*
                           (string->symbol
                             (format "$fptr-ref-~a" (datum type))))))])))
        ((record-datatype cases ty proc
           ($oops who "unrecognized type ~s" ty))
         fptr offset)))

  (set-who! $fptr-fptr-set!
    (lambda (fptr offset val)
      (#3%$fptr-fptr-set! fptr offset val)))

  (let ()
    (define invalid-value
      (lambda (info val)
        ($source-violation 'ftype-set! (src-info-src info) #t
          "invalid value ~s for type ~s" val (field-info-type info))))
    (set! $fptr-set-integer-8!
      (lambda (info fptr offset val)
        (unless ($integer-8? val) (invalid-value info val))
        (#3%$fptr-set-integer-8! info fptr offset val)))
    (set! $fptr-set-unsigned-8!
      (lambda (info fptr offset val)
        (unless ($integer-8? val) (invalid-value info val))
        (#3%$fptr-set-unsigned-8! info fptr offset val)))

    (set! $fptr-set-integer-16!
      (lambda (info fptr offset val)
        (unless ($integer-16? val) (invalid-value info val))
        (#3%$fptr-set-integer-16! info fptr offset val)))
    (set! $fptr-set-unsigned-16!
      (lambda (info fptr offset val)
        (unless ($integer-16? val) (invalid-value info val))
        (#3%$fptr-set-unsigned-16! info fptr offset val)))
    (set! $fptr-set-swap-integer-16!
      (lambda (info fptr offset val)
        (unless ($integer-16? val) (invalid-value info val))
        (#3%$fptr-set-swap-integer-16! info fptr offset val)))
    (set! $fptr-set-swap-unsigned-16!
      (lambda (info fptr offset val)
        (unless ($integer-16? val) (invalid-value info val))
        (#3%$fptr-set-swap-unsigned-16! info fptr offset val)))

    (set! $fptr-set-integer-24!
      (lambda (info fptr offset val)
        (unless ($integer-24? val) (invalid-value info val))
        (multi-int foreign-set! integer-24
                   (#3%$fptr-set-integer-24! info fptr offset val))))
    (set! $fptr-set-unsigned-24!
      (lambda (info fptr offset val)
        (unless ($integer-24? val) (invalid-value info val))
        (multi-int foreign-set! unsigned-24
                   (#3%$fptr-set-unsigned-24! info fptr offset val))))
    (set! $fptr-set-swap-integer-24!
      (lambda (info fptr offset val)
        (unless ($integer-24? val) (invalid-value info val))
        (multi-int $foreign-swap-set! integer-24
                   (#3%$fptr-set-swap-integer-24! info fptr offset val))))
    (set! $fptr-set-swap-unsigned-24!
      (lambda (info fptr offset val)
        (unless ($integer-24? val) (invalid-value info val))
        (multi-int $foreign-swap-set! unsigned-24
                   (#3%$fptr-set-swap-unsigned-24! info fptr offset val))))

    (set! $fptr-set-integer-32!
      (lambda (info fptr offset val)
        (unless ($integer-32? val) (invalid-value info val))
        (#3%$fptr-set-integer-32! info fptr offset val)))
    (set! $fptr-set-unsigned-32!
      (lambda (info fptr offset val)
        (unless ($integer-32? val) (invalid-value info val))
        (#3%$fptr-set-unsigned-32! info fptr offset val)))
    (set! $fptr-set-swap-integer-32!
      (lambda (info fptr offset val)
        (unless ($integer-32? val) (invalid-value info val))
        (#3%$fptr-set-swap-integer-32! info fptr offset val)))
    (set! $fptr-set-swap-unsigned-32!
      (lambda (info fptr offset val)
        (unless ($integer-32? val) (invalid-value info val))
        (#3%$fptr-set-swap-unsigned-32! info fptr offset val)))

    (set! $fptr-set-integer-40!
      (lambda (info fptr offset val)
        (unless ($integer-40? val) (invalid-value info val))
        (multi-int/wide foreign-set! integer-40
                         (#3%$fptr-set-integer-40! info fptr offset val))))
    (set! $fptr-set-unsigned-40!
      (lambda (info fptr offset val)
        (unless ($integer-40? val) (invalid-value info val))
        (multi-int/wide foreign-set! unsigned-40
                         (#3%$fptr-set-unsigned-40! info fptr offset val))))
    (set! $fptr-set-swap-integer-40!
      (lambda (info fptr offset val)
        (unless ($integer-40? val) (invalid-value info val))
        (multi-int/wide $foreign-swap-set! integer-40
                         (#3%$fptr-set-swap-integer-40! info fptr offset val))))
    (set! $fptr-set-swap-unsigned-40!
      (lambda (info fptr offset val)
        (unless ($integer-40? val) (invalid-value info val))
        (multi-int/wide $foreign-swap-set! unsigned-40
                         (#3%$fptr-set-swap-unsigned-40! info fptr offset val))))

    (set! $fptr-set-integer-48!
      (lambda (info fptr offset val)
        (unless ($integer-48? val) (invalid-value info val))
        (multi-int/wide foreign-set! integer-48
                         (#3%$fptr-set-integer-48! info fptr offset val))))
    (set! $fptr-set-unsigned-48!
      (lambda (info fptr offset val)
        (unless ($integer-48? val) (invalid-value info val))
        (multi-int/wide foreign-set! unsigned-48
                         (#3%$fptr-set-unsigned-48! info fptr offset val))))
    (set! $fptr-set-swap-integer-48!
      (lambda (info fptr offset val)
        (unless ($integer-48? val) (invalid-value info val))
        (multi-int/wide $foreign-swap-set! integer-48
                        (#3%$fptr-set-swap-integer-48! info fptr offset val))))
    (set! $fptr-set-swap-unsigned-48!
      (lambda (info fptr offset val)
        (unless ($integer-48? val) (invalid-value info val))
        (multi-int/wide $foreign-swap-set! unsigned-48
                        (#3%$fptr-set-swap-unsigned-48! info fptr offset val))))
    
    (set! $fptr-set-integer-56!
      (lambda (info fptr offset val)
        (unless ($integer-56? val) (invalid-value info val))
        (multi-int/wide foreign-set! integer-56
                        (#3%$fptr-set-integer-56! info fptr offset val))))
    (set! $fptr-set-unsigned-56!
      (lambda (info fptr offset val)
        (unless ($integer-56? val) (invalid-value info val))
        (multi-int/wide foreign-set! unsigned-56
                        (#3%$fptr-set-unsigned-56! info fptr offset val))))
    (set! $fptr-set-swap-integer-56!
      (lambda (info fptr offset val)
        (unless ($integer-56? val) (invalid-value info val))
        (multi-int/wide $foreign-swap-set! integer-56
                        (#3%$fptr-set-swap-integer-56! info fptr offset val))))
    (set! $fptr-set-swap-unsigned-56!
      (lambda (info fptr offset val)
        (unless ($integer-56? val) (invalid-value info val))
        (multi-int/wide $foreign-swap-set! unsigned-56
                        (#3%$fptr-set-swap-unsigned-56! info fptr offset val))))

    (set! $fptr-set-integer-64!
      (lambda (info fptr offset val)
        (unless ($integer-64? val) (invalid-value info val))
        (wide foreign-set! integer-64
              (#3%$fptr-set-integer-64! info fptr offset val))))
    (set! $fptr-set-unsigned-64!
      (lambda (info fptr offset val)
        (unless ($integer-64? val) (invalid-value info val))
        (wide foreign-set! unsigned-64
              (#3%$fptr-set-unsigned-64! info fptr offset val))))
    (set! $fptr-set-swap-integer-64!
      (lambda (info fptr offset val)
        (unless ($integer-64? val) (invalid-value info val))
        (wide $foreign-swap-set! integer-64
              (#3%$fptr-set-swap-integer-64! info fptr offset val))))
    (set! $fptr-set-swap-unsigned-64!
      (lambda (info fptr offset val)
        (unless ($integer-64? val) (invalid-value info val))
        (wide $foreign-swap-set! unsigned-64
              (#3%$fptr-set-swap-unsigned-64! info fptr offset val))))

    (set! $fptr-set-double-float!
      (lambda (info fptr offset val)
        (unless (flonum? val) (invalid-value info val))
        (#3%$fptr-set-double-float! info fptr offset val)))
    (set! $fptr-set-swap-double-float!
      (lambda (info fptr offset val)
        (unless (flonum? val) (invalid-value info val))
        (constant-case ptr-bits
          [(64) (#3%$fptr-set-swap-double-float! info fptr offset val)]
          [(32) (let ([bv (make-bytevector 8)])
                  ($object-set! 'double-float bv (constant bytevector-data-disp) val)
                  (foreign-set! 'unsigned-64 ($ftype-pointer-address fptr) offset
                    (bytevector-u64-ref bv 0
                      (swapped-endianness))))])))

    (set! $fptr-set-single-float!
      (lambda (info fptr offset val)
        (unless (flonum? val) (invalid-value info val))
        (#3%$fptr-set-single-float! info fptr offset val)))
    (set! $fptr-set-swap-single-float!
      (lambda (info fptr offset val)
        (unless (flonum? val) (invalid-value info val))
        (let ([bv (make-bytevector 4)])
          ($object-set! 'single-float bv (constant bytevector-data-disp) val)
          (foreign-set! 'unsigned-32 ($ftype-pointer-address fptr) offset
            (bytevector-u32-ref bv 0
              (swapped-endianness))))))

    (set! $fptr-set-char!
      (lambda (info fptr offset val)
        (unless (char? val) (invalid-value info val))
        (#3%$fptr-set-char! info fptr offset val)))

    (set! $fptr-set-wchar!
      (lambda (info fptr offset val)
        (unless (char? val) (invalid-value info val))
        (#3%$fptr-set-wchar! info fptr offset val)))
    (set! $fptr-set-swap-wchar!
      (lambda (info fptr offset val)
        (unless (char? val) (invalid-value info val))
        (#3%$fptr-set-swap-wchar! info fptr offset val)))

    (set! $fptr-set-boolean!
      (lambda (info fptr offset val)
        (#3%$fptr-set-boolean! info fptr offset val)))
    (set! $fptr-set-swap-boolean!
      (lambda (info fptr offset val)
        (#3%$fptr-set-swap-boolean! info fptr offset val)))

    (set! $fptr-set-stdbool!
      (lambda (info fptr offset val)
        (#3%$fptr-set-stdbool! info fptr offset val)))
    (set! $fptr-set-swap-stdbool!
      (lambda (info fptr offset val)
        (#3%$fptr-set-swap-stdbool! info fptr offset val)))

    (set! $fptr-set-fixnum!
      (lambda (info fptr offset val)
        (unless (fixnum? val) (invalid-value info val))
        (#3%$fptr-set-fixnum! info fptr offset val)))
    (set! $fptr-set-swap-fixnum!
      (lambda (info fptr offset val)
        (unless (fixnum? val) (invalid-value info val))
        (#3%$fptr-set-swap-fixnum! info fptr offset val)))
    )

  (set-who! $fptr-set!
    (lambda (orig-type ty eness fptr offset val)
      (define-syntax proc
        (lambda (x)
          (syntax-case x (scheme-object)
            [(_ scheme-object bytes pred) #'($oops who "unexpected type ~s" ty)]
            [(_ type bytes pred)
             (if (memq (datum type) '(char integer-8 unsigned-8))
                 #`($primitive 2
                     #,(datum->syntax #'*
                         (string->symbol
                           (format "$fptr-set-~a!" (datum type)))))
                 #`(if (or (eq? eness 'swapped)
                           (eq? eness (swapped-endianness)))
                       ($primitive 2
                         #,(datum->syntax #'*
                             (string->symbol
                               (format "$fptr-set-swap-~a!" (datum type)))))
                       ($primitive 2
                         #,(datum->syntax #'*
                             (string->symbol
                               (format "$fptr-set-~a!" (datum type)))))))])))
      ((record-datatype cases ty proc
         ($oops who "unrecognized type ~s" ty))
       orig-type fptr offset val)))

  (let ()
    (define-syntax $fptr-ref-ibits
      (lambda (x)
        (syntax-case x ()
          [(kwd k swap?)
           (with-syntax ([$fptr-ref-x (datum->syntax #'kwd
                                        (string->symbol
                                          (format "$fptr-ref-~:[~;swap-~]unsigned-~a"
                                            (datum swap?)
                                            (datum k))))])
             (if (<= (expt 2 (datum k)) (constant most-positive-fixnum))
                 #'(lambda (fptr offset start end)
                     (let ([radix (fxsll 1 (fx- end start))])
                       (let ([n (fxlogand
                                  (fxsra ($fptr-ref-x fptr offset) start)
                                  (fx- radix 1))])
                         (if (fx>= n (fxsra radix 1)) (fx- n radix) n))))
                 #'(lambda (fptr offset start end)
                     (let ([radix (bitwise-arithmetic-shift-left 1 (fx- end start))])
                       (let ([n (logand
                                  (bitwise-arithmetic-shift-right
                                    ($fptr-ref-x fptr offset)
                                    start)
                                  (- radix 1))])
                         (if (>= n (bitwise-arithmetic-shift-right radix 1))
                             (- n radix)
                             n))))))])))
    (set! $fptr-ref-ibits-unsigned-8 ($fptr-ref-ibits 8 #f))
    (set! $fptr-ref-ibits-swap-unsigned-16 ($fptr-ref-ibits 16 #t))
    (set! $fptr-ref-ibits-unsigned-16 ($fptr-ref-ibits 16 #f))
    (set! $fptr-ref-ibits-swap-unsigned-24 ($fptr-ref-ibits 24 #t))
    (set! $fptr-ref-ibits-unsigned-24 ($fptr-ref-ibits 24 #f))
    (set! $fptr-ref-ibits-swap-unsigned-32 ($fptr-ref-ibits 32 #t))
    (set! $fptr-ref-ibits-unsigned-32 ($fptr-ref-ibits 32 #f))
    (set! $fptr-ref-ibits-swap-unsigned-40 ($fptr-ref-ibits 40 #t))
    (set! $fptr-ref-ibits-unsigned-40 ($fptr-ref-ibits 40 #f))
    (set! $fptr-ref-ibits-swap-unsigned-48 ($fptr-ref-ibits 48 #t))
    (set! $fptr-ref-ibits-unsigned-48 ($fptr-ref-ibits 48 #f))
    (set! $fptr-ref-ibits-swap-unsigned-56 ($fptr-ref-ibits 56 #t))
    (set! $fptr-ref-ibits-unsigned-56 ($fptr-ref-ibits 56 #f))
    (set! $fptr-ref-ibits-swap-unsigned-64 ($fptr-ref-ibits 64 #t))
    (set! $fptr-ref-ibits-unsigned-64 ($fptr-ref-ibits 64 #f)))

  (let ()
    (define-syntax $fptr-ref-ubits
      (lambda (x)
        (syntax-case x ()
          [(kwd k swap?)
           (with-syntax ([$fptr-ref-x (datum->syntax #'kwd
                                        (string->symbol
                                          (format "$fptr-ref-~:[~;swap-~]unsigned-~a"
                                            (datum swap?)
                                            (datum k))))])
             (if (<= (expt 2 (datum k)) (constant most-positive-fixnum))
                 #'(lambda (fptr offset start end)
                     (let ([radix (fxsll 1 (fx- end start))])
                       (fxlogand
                         (fxsrl ($fptr-ref-x fptr offset) start)
                         (fx- radix 1))))
                 #'(lambda (fptr offset start end)
                     (let ([radix (bitwise-arithmetic-shift-left 1 (fx- end start))])
                       (logand
                         (bitwise-arithmetic-shift-right ($fptr-ref-x fptr offset) start)
                         (- radix 1))))))])))
    (set! $fptr-ref-ubits-unsigned-8 ($fptr-ref-ubits 8 #f))
    (set! $fptr-ref-ubits-swap-unsigned-16 ($fptr-ref-ubits 16 #t))
    (set! $fptr-ref-ubits-unsigned-16 ($fptr-ref-ubits 16 #f))
    (set! $fptr-ref-ubits-swap-unsigned-24 ($fptr-ref-ubits 24 #t))
    (set! $fptr-ref-ubits-unsigned-24 ($fptr-ref-ubits 24 #f))
    (set! $fptr-ref-ubits-swap-unsigned-32 ($fptr-ref-ubits 32 #t))
    (set! $fptr-ref-ubits-unsigned-32 ($fptr-ref-ubits 32 #f))
    (set! $fptr-ref-ubits-swap-unsigned-40 ($fptr-ref-ubits 40 #t))
    (set! $fptr-ref-ubits-unsigned-40 ($fptr-ref-ubits 40 #f))
    (set! $fptr-ref-ubits-swap-unsigned-48 ($fptr-ref-ubits 48 #t))
    (set! $fptr-ref-ubits-unsigned-48 ($fptr-ref-ubits 48 #f))
    (set! $fptr-ref-ubits-swap-unsigned-56 ($fptr-ref-ubits 56 #t))
    (set! $fptr-ref-ubits-unsigned-56 ($fptr-ref-ubits 56 #f))
    (set! $fptr-ref-ubits-swap-unsigned-64 ($fptr-ref-ubits 64 #t))
    (set! $fptr-ref-ubits-unsigned-64 ($fptr-ref-ubits 64 #f)))

  (set-who! $fptr-ref-bits
    (lambda (ty eness signed? fptr offset start end)
      (define-syntax proc
        (lambda (x)
          (syntax-case x ()
            [(_ type)
             (if (memq (datum type) '(char integer-8 unsigned-8))
                 #`(if signed?
                       #,(datum->syntax #'*
                           (string->symbol
                             (format "$fptr-ref-ibits-~a" (datum type))))
                       #,(datum->syntax #'*
                           (string->symbol
                             (format "$fptr-ref-ubits-~a" (datum type)))))
                 #`(if (or (eq? eness 'swapped)
                           (eq? eness (swapped-endianness)))
                       (if signed?
                           #,(datum->syntax #'*
                               (string->symbol
                                 (format "$fptr-ref-ibits-swap-~a" (datum type))))
                           #,(datum->syntax #'*
                               (string->symbol
                                 (format "$fptr-ref-ubits-swap-~a" (datum type)))))
                       (if signed?
                           #,(datum->syntax #'*
                               (string->symbol
                                 (format "$fptr-ref-ibits-~a" (datum type))))
                           #,(datum->syntax #'*
                               (string->symbol
                                 (format "$fptr-ref-ubits-~a" (datum type)))))))])))
        ((case ty
           [(unsigned-8) (proc unsigned-8)]
           [(unsigned-16) (proc unsigned-16)]
           [(unsigned-24) (proc unsigned-24)]
           [(unsigned-32) (proc unsigned-32)]
           [(unsigned-40) (proc unsigned-40)]
           [(unsigned-48) (proc unsigned-48)]
           [(unsigned-56) (proc unsigned-56)]
           [(unsigned-64) (proc unsigned-64)]
           [else ($oops who "unexpected type ~s" ty)])
         fptr offset start end)))

  (let ()
    (define-syntax $fptr-set-bits!
      (lambda (x)
        (syntax-case x ()
          [(kwd k swap?)
           (with-syntax ([orig-type (datum->syntax #'kwd
                                      (string->symbol
                                        (format "unsigned-~a" (datum k))))]
                         [$fptr-ref-x (datum->syntax #'kwd
                                        (string->symbol
                                          (format "$fptr-ref-~:[~;swap-~]unsigned-~a"
                                            (datum swap?)
                                            (datum k))))]
                         [$fptr-set-x! (datum->syntax #'kwd
                                         (string->symbol
                                           (format "$fptr-set-~:[~;swap-~]unsigned-~a!"
                                             (datum swap?)
                                             (datum k))))])
             (if (<= (expt 2 (datum k)) (constant most-positive-fixnum))
                 #'(lambda (fptr offset start end val)
                     (let* ([size (fx- end start)]
                            [radix (fxsll 1 size)]
                            [radix/2 (fxsrl radix 1)])
                       (unless (and (integer? val) (exact? val) (>= val (- radix/2)) (< val radix))
                         ($oops 'ftype-set! "invalid value ~s for bit field of size ~s" val size))
                       ($fptr-set-x! 'orig-type fptr offset
                         (fxlogor
                           (fxlogand
                             ($fptr-ref-x fptr offset)
                             (fxlognot (fxsll (- radix 1) start)))
                           (fxsll
                             (if (fx< val 0) (fx+ val radix) val)
                             start)))))
                 #'(lambda (fptr offset start end val)
                     (let* ([size (fx- end start)]
                            [radix (bitwise-arithmetic-shift-left 1 size)]
                            [radix/2 (bitwise-arithmetic-shift-right radix 1)])
                       (unless (and (integer? val) (exact? val) (>= val (- radix/2)) (< val radix))
                         ($oops 'ftype-set! "invalid value ~s for bit field of size ~s" val size))
                       ($fptr-set-x! 'orig-type fptr offset
                         (logor
                           (logand
                             ($fptr-ref-x fptr offset)
                             (lognot (bitwise-arithmetic-shift-left (- radix 1) start)))
                           (bitwise-arithmetic-shift-left
                             (if (< val 0) (+ val radix) val)
                             start)))))))])))
    (set! $fptr-set-bits-unsigned-8! ($fptr-set-bits! 8 #f))
    (set! $fptr-set-bits-swap-unsigned-16! ($fptr-set-bits! 16 #t))
    (set! $fptr-set-bits-unsigned-16! ($fptr-set-bits! 16 #f))
    (set! $fptr-set-bits-swap-unsigned-24! ($fptr-set-bits! 24 #t))
    (set! $fptr-set-bits-unsigned-24! ($fptr-set-bits! 24 #f))
    (set! $fptr-set-bits-swap-unsigned-32! ($fptr-set-bits! 32 #t))
    (set! $fptr-set-bits-unsigned-32! ($fptr-set-bits! 32 #f))
    (set! $fptr-set-bits-swap-unsigned-40! ($fptr-set-bits! 40 #t))
    (set! $fptr-set-bits-unsigned-40! ($fptr-set-bits! 40 #f))
    (set! $fptr-set-bits-swap-unsigned-48! ($fptr-set-bits! 48 #t))
    (set! $fptr-set-bits-unsigned-48! ($fptr-set-bits! 48 #f))
    (set! $fptr-set-bits-swap-unsigned-56! ($fptr-set-bits! 56 #t))
    (set! $fptr-set-bits-unsigned-56! ($fptr-set-bits! 56 #f))
    (set! $fptr-set-bits-swap-unsigned-64! ($fptr-set-bits! 64 #t))
    (set! $fptr-set-bits-unsigned-64! ($fptr-set-bits! 64 #f)))

  (set-who! $fptr-set-bits!
    (lambda (ty eness fptr offset start end val)
      (define-syntax proc
        (lambda (x)
          (syntax-case x ()
            [(_ type)
             (if (memq (datum type) '(char integer-8 unsigned-8))
                 (datum->syntax #'*
                   (string->symbol
                     (format "$fptr-set-bits-~a!" (datum type))))
                 #`(if (or (eq? eness 'swapped)
                           (eq? eness (swapped-endianness)))
                       ($primitive 2
                         #,(datum->syntax #'*
                             (string->symbol
                               (format "$fptr-set-bits-swap-~a!" (datum type)))))
                       ($primitive 2
                         #,(datum->syntax #'*
                             (string->symbol
                               (format "$fptr-set-bits-~a!" (datum type)))))))])))
        ((case ty
           [(unsigned-8) (proc unsigned-8)]
           [(unsigned-16) (proc unsigned-16)]
           [(unsigned-24) (proc unsigned-24)]
           [(unsigned-32) (proc unsigned-32)]
           [(unsigned-40) (proc unsigned-40)]
           [(unsigned-48) (proc unsigned-48)]
           [(unsigned-56) (proc unsigned-56)]
           [(unsigned-64) (proc unsigned-64)]
           [else ($oops who "unexpected type ~s" ty)])
         fptr offset start end val)))

  (set! $fptr-locked-incr!
    (lambda (fptr offset)
      (#3%$fptr-locked-incr! fptr offset)))

  (set! $fptr-locked-decr!
    (lambda (fptr offset)
      (#3%$fptr-locked-decr! fptr offset)))

  (set! $fptr-init-lock!
    (lambda (fptr offset)
      (#3%$fptr-init-lock! fptr offset)))

  (set! $fptr-lock!
    (lambda (fptr offset)
      (#3%$fptr-lock! fptr offset)))

  (set! $fptr-spin-lock!
    (lambda (fptr offset)
      (#3%$fptr-spin-lock! fptr offset)))

  (set! $fptr-unlock!
    (lambda (fptr offset)
      (#3%$fptr-unlock! fptr offset)))
)

(define-syntax define-ftype (lambda (x) ($trans-define-ftype x)))
(define-syntax make-ftype-pointer (lambda (x) ($trans-make-ftype-pointer x)))
(define-syntax ftype-pointer? (lambda (x) ($trans-ftype-pointer? x)))
(define-syntax ftype-sizeof (lambda (x) ($trans-ftype-sizeof x)))
(define-syntax ftype-guardian (lambda (x) ($trans-ftype-guardian x)))
(define-syntax ftype-&ref (lambda (x) ($trans-ftype-&ref x)))
(define-syntax ftype-ref (lambda (x) ($trans-ftype-ref x)))
(define-syntax ftype-locked-incr! (lambda (x) ($trans-ftype-locked-op! #'ftype-locked-incr! x #'$fptr-locked-incr!)))
(define-syntax ftype-locked-decr! (lambda (x) ($trans-ftype-locked-op! #'ftype-locked-decr! x #'$fptr-locked-decr!)))
(define-syntax ftype-init-lock! (lambda (x) ($trans-ftype-locked-op! #'ftype-init-lock! x #'$fptr-init-lock!)))
(define-syntax ftype-lock! (lambda (x) ($trans-ftype-locked-op! #'ftype-lock! x #'$fptr-lock!)))
(define-syntax ftype-spin-lock! (lambda (x) ($trans-ftype-locked-op! #'ftype-spin-lock! x #'$fptr-spin-lock!)))
(define-syntax ftype-unlock! (lambda (x) ($trans-ftype-locked-op! #'ftype-unlock! x #'$fptr-unlock!)))
(define-syntax ftype-set! (lambda (x) ($trans-ftype-set! x)))
)
