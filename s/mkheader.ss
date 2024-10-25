;;; mkheader.ss
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

;;; requires cmacros.ss
(disable-unbound-warning
  mkscheme.h
  mkequates.h
)

(define mkscheme.h)
(define mkequates.h)
(let ()
  (define op)
  (define-syntax pr
    (syntax-rules ()
      ((_ fmt arg ...) (fprintf op fmt arg ...))))
  (define nl
    (lambda ()
      (newline op)))
  (define-syntax $
    (syntax-rules ()
      ((_ x) (constant x))))
  (define-syntax comment
    (syntax-rules ()
      ((_ fmt arg ...) (pr "/* ~a */~%" (format fmt arg ...)))))
  (define sanitize
    (lambda (x)
      (list->string
        (fold-right (lambda (x rest) 
                      (case x
                        [(#\-) (cons #\_ rest)]
                        [(#\+) (cons #\_ rest)]
                        [(#\?) (cons #\p rest)]
                        [(#\>) rest]
                        [(#\*) (cons #\s rest)]
                        [(#\=) (cons* #\e #\q #\l rest)]
                        [(#\?) (cons #\p rest)]
                        [(#\$) (cons* #\s #\y #\s #\_ rest)]
                        [else (cons x rest)]))
          '()
          (string->list (symbol->string x))))))
  (define sanitize-type
    (lambda (x) 
      (list->string
        (map (lambda (x)
               (case x
                 [(#\-) #\_]
                 [else x]))
          (string->list (symbol->string x))))))
  (define def
    (case-lambda
      [(lhs rhs) (pr "#define ~a ~a~%" lhs rhs)]
      [(name args rhs) (pr "#define ~a~a ~a~%" name args rhs)]))
  (define export
    (lambda (tresult name targs)
      (pr "EXPORT ~a ~a~a;~%" tresult name targs)))
  (define &ref
    (lambda (cast x disp)
      (format "(~aTO_VOIDP((uptr)(~a)~:[+~;-~]~d))" cast x (fx< disp 0) (abs disp))))
  (define ref
    (lambda (cast x disp)
      (format "(*~a)" (&ref cast x disp))))
  (define defref-help
    (lambda (ref name struct field)
      (cond
        [(assq field (getprop struct '*fields* '())) =>
         (lambda (a)
           (apply
             (lambda (field type disp len)
               (putprop (string->symbol (format "~a-~a" struct field)) '*c-ref* (if len
                                                                                    (cons name len)
                                                                                    name))
               (if len
                   (def (format "~s(x,i)" name)
                        (format (if (eq? ref &ref) "(~a+i)" "(~a[i])")
                                (&ref (format "(~a *)" (sanitize-type type)) "x" disp)))
                   (def (format "~s(x)" name)
                        (ref (format "(~a *)" (sanitize-type type)) "x" disp))))
             a))]
        [else ($oops 'defref-help "undefined field ~s-~s" struct field)])))
  (define defset-help
    (lambda (name struct field)
      (cond
        [(assq field (getprop struct '*fields* '())) =>
         (lambda (a)
           (apply
             (lambda (field type disp len)
               (unless (eq? type 'ptr)
                 ($oops 'defset-help "non-ptr type ~s" type))
               (if len
                   (def (format "~s(x,i,y)" name)
                        (format "DIRTYSET((~a+i),(y))"
                          (&ref "(ptr *)" "x" disp)))
                   (def (format "~s(x,y)" name)
                        (format "DIRTYSET(~a,(y))"
                          (&ref "(ptr *)" "x" disp)))))
             a))]
        [else ($oops 'defset-help "undefined field ~s-~s" struct field)])))
  (define-syntax defref
    (syntax-rules ()
      [(_ name struct field)
       (defref-help ref 'name 'struct 'field)]))
  (define-syntax definit ; presently same as defref
    (syntax-rules ()
      [(_ name struct field)
       (defref name struct field)]))
  (define-syntax defset
    (syntax-rules ()
      [(_ name struct field)
       (defset-help 'name 'struct 'field)]))
  (define access-help
    (lambda (arg idx struct field)
      (cond
        [(assq field (getprop struct '*fields* '())) =>
         (lambda (a)
           (apply
             (lambda (field type disp len)
               (if (not idx)
                   (if (not len)
                       (ref (format "(~a *)" (sanitize-type type)) arg disp)
                       ($oops 'access "no idx provided for array field ~s-~s" struct field))
                   (if len
                       (format "(~a[~a])" (&ref (format "(~a *)" (sanitize-type type)) arg disp) idx)
                       ($oops 'access "no idx provided for array field ~s-~s" struct field))))
             a))]
        [else ($oops 'access "undefined field ~s-~s" struct field)])))
  (define-syntax access
    (syntax-rules ()
      [(_ arg struct field)
       (access-help arg #f 'struct 'field)]
      [(_ arg idx struct field)
       (access-help arg idx 'struct 'field)]))
  (define typep
    (lambda (x mask tag)
      (if (= mask (constant byte-constant-mask))
          (format "((uptr)(~a)==0x~x)" x tag)
          (format "(((uptr)(~a)&0x~x)==0x~x)" x mask tag))))
  (define deftypep
    (lambda (name mask tag)
      (def name "(x)" (typep "x" mask tag))))
  (define deftotypep
    (let ((type-disp (- ($ typemod) ($ type-typed-object))))
      (lambda (name mask tag)
        (def name "(x)"
          (format "(~a &&\\~%    ~a)"
            (typep "x" ($ mask-typed-object) ($ type-typed-object))
            (typep (ref "(ptr *)" "x" type-disp) mask tag))))))
  (define scheme-version ; adapted from 7.ss
    (let ([n (constant scheme-version)])
      (if (= (logand n 255) 0)
          (format "~d.~d.~d"
             (ash n -24)
             (logand (ash n -16) 255)
             (logand (ash n -8) 255))
          (format "~d.~d.~d-pre-release.~d"
            (ash n -24)
            (logand (ash n -16) 255)
            (logand (ash n -8) 255)
            (logand n 255)))))

  (set-who! mkscheme.h
    (lambda (ofn target-machine)
      (fluid-let ([op (if (output-port? ofn)
                          ofn
                          (open-output-file ofn 'replace))])
        (comment "scheme.h for Chez Scheme Version ~a (~a)" scheme-version target-machine)
  
        (nl)
        (comment "Do not edit this file.  It is automatically generated and")
        (comment "specifically tailored to the version of Chez Scheme named")
        (comment "above.  Always be certain that you have the correct scheme.h")
        (comment "for the version of Chez Scheme you are using.")
  
        (nl)
        (comment "Warning: Some macros may evaluate arguments more than once.")
       
        (constant-case architecture
          [(pb)
           (nl)
           (pr "#if !defined(_LARGEFILE64_SOURCE) && !defined(FEATURE_WINDOWS)\n")
           (pr "# define _LARGEFILE64_SOURCE\n") ; needed on some 32-bit platforms before <stdint.h>
           (pr "#endif\n")
           (pr "#include <stdint.h>\n")]
          [else (void)])

        (nl) (comment "Specify declaration of exports.")
        (pr "#ifdef _WIN32~%")
        (pr "#  if __cplusplus~%")
        (pr "#    ifdef SCHEME_IMPORT~%")
        (pr "#      define EXPORT extern \"C\" __declspec (dllimport)~%")
        (pr "#    elif SCHEME_STATIC~%")
        (pr "#      define EXPORT extern \"C\"~%")
        (pr "#    else~%")
        (pr "#      define EXPORT extern \"C\" __declspec (dllexport)~%")
        (pr "#    endif~%")
        (pr "#  else~%")
        (pr "#    ifdef SCHEME_IMPORT~%")
        (pr "#      define EXPORT extern __declspec (dllimport)~%")
        (pr "#    elif SCHEME_STATIC~%")
        (pr "#      define EXPORT extern~%")
        (pr "#    else~%")
        (pr "#      define EXPORT extern __declspec (dllexport)~%")
        (pr "#    endif~%")
        (pr "#  endif~%")
        (pr "#else~%")
        (pr "#  if __cplusplus~%")
        (pr "#    define EXPORT extern \"C\"~%")
        (pr "#  else~%")
        (pr "#    define EXPORT extern~%")
        (pr "#  endif~%")
        (pr "#endif~%")
  
        (nl) (comment "Chez Scheme Version and machine type")
        (pr "#define VERSION \"~a\"~%" scheme-version)
        (pr "#define MACHINE_TYPE \"~a\"~%" target-machine)

        (nl)
        (comment "Integer typedefs") ;; only for types used by exports
        (pr "typedef ~a Sint32_t;~%" (constant typedef-i32))
        (pr "typedef ~a Suint32_t;~%" (constant typedef-u32))
        (pr "typedef ~a Sint64_t;~%" (constant typedef-i64))
        (pr "typedef ~a Suint64_t;~%" (constant typedef-u64))

        (nl)
        (comment "All Scheme objects are of type ptr.  Type iptr and")
        (comment "uptr are signed and unsigned ints of the same size")
        (comment "as a ptr")
        (pr "typedef ~a ptr;~%" (constant typedef-ptr))
        (pr "typedef ~a iptr;~%" (constant typedef-iptr))
        (pr "typedef ~a uptr;~%" (constant typedef-uptr))
        (pr "typedef ptr xptr;~%")

        (nl)
        (comment "The `uptr` and `ptr` types are the same width, but `ptr`")
        (comment "can be either an integer type or a pointer type; it may")
        (comment "be larger than a pointer type.")
        (comment "Use `TO_VOIDP` to get from the `uptr`/`ptr` world to the")
        (comment "C pointer worlds, and use `TO_PTR` to go the other way.")
        (pr "#ifdef PORTABLE_BYTECODE~%")
        (pr "# define TO_VOIDP(p) ((void *)(intptr_t)(p))~%")
        (pr "# define TO_PTR(p) ((ptr)(intptr_t)(p))~%")
        (pr "#else~%")
        (pr "# define TO_VOIDP(p) ((void *)(p))~%")
        (pr "# define TO_PTR(p) ((ptr)(p))~%")
        (pr "#endif~%")

        (nl)
        (comment "String elements are 32-bit tagged char objects")
        (pr "typedef ~a string_char;~%" (constant typedef-string-char))
  
        (nl)
        (comment "Bytevector elements are 8-bit unsigned \"octets\"")
        (pr "typedef unsigned char octet;~%")
  
        (nl) (comment "Type predicates")
        (deftypep "Sfixnump" ($ mask-fixnum) ($ type-fixnum))
        (deftypep "Scharp" ($ mask-char) ($ type-char))
        (deftypep "Snullp" ($ mask-nil) ($ snil))
        (deftypep "Seof_objectp" ($ mask-eof) ($ seof))
        (deftypep "Sbwp_objectp" ($ mask-bwp) ($ sbwp))
        (deftypep "Sbooleanp" ($ mask-boolean) ($ type-boolean))
  
        (deftypep "Spairp" ($ mask-pair) ($ type-pair))
        (deftypep "Ssymbolp" ($ mask-symbol) ($ type-symbol))
        (deftypep "Sprocedurep" ($ mask-closure) ($ type-closure))
        (deftypep "Sflonump" ($ mask-flonum) ($ type-flonum))
  
        (deftotypep "Svectorp" ($ mask-vector) ($ type-vector))
        (deftotypep "Sfxvectorp" ($ mask-fxvector) ($ type-fxvector))
        (deftotypep "Sflvectorp" ($ mask-flvector) ($ type-flvector))
        (deftotypep "Sbytevectorp" ($ mask-bytevector) ($ type-bytevector))
        (deftotypep "Sstringp" ($ mask-string) ($ type-string))
        (deftotypep "Sstencil_vectorp" ($ mask-stencil-vector) ($ type-stencil-vector))
        (deftotypep "Ssystem_stencil_vectorp" ($ mask-sys-stencil-vector) ($ type-sys-stencil-vector))
        (deftotypep "Sany_stencil_vectorp" ($ mask-any-stencil-vector) ($ type-any-stencil-vector))
        (deftotypep "Sbignump" ($ mask-bignum) ($ type-bignum))
        (deftotypep "Sboxp" ($ mask-box) ($ type-box))
        (deftotypep "Sinexactnump" ($ mask-inexactnum) ($ type-inexactnum))
        (deftotypep "Sexactnump" ($ mask-exactnum) ($ type-exactnum))
        (deftotypep "Sratnump" ($ mask-ratnum) ($ type-ratnum))
  
        (deftotypep "Sinputportp" ($ mask-input-port) ($ type-input-port))
        (deftotypep "Soutputportp" ($ mask-output-port) ($ type-output-port))
        (deftotypep "Srecordp" ($ mask-record) ($ type-record))
  
        (nl) (comment "Accessors")
        (def "Sfixnum_value(x)" (format "((iptr)(x)/~d)" ($ fixnum-factor)))
        (def "Schar_value(x)" (format "((string_char)((uptr)(x)>>~d))" ($ char-data-offset)))
        (def "Sboolean_value(x)" "((x) != Sfalse)")
  
        (defref Scar pair car)
        (defref Scdr pair cdr)

        (defref Sflonum_value flonum data)
  
        (def "Svector_length(x)"
          (format "((iptr)((uptr)~a>>~d))"
            (access "x" vector type)
            ($ vector-length-offset)))
        (defref Svector_ref vector data)
        
        (def "Sfxvector_length(x)"
          (format "((iptr)((uptr)~a>>~d))"
            (access "x" fxvector type)
            ($ fxvector-length-offset)))
        (defref Sfxvector_ref fxvector data)
        
        (def "Sflvector_length(x)"
          (format "((iptr)((uptr)~a>>~d))"
            (access "x" flvector type)
            ($ flvector-length-offset)))
        (defref Sflvector_ref flvector data)

        (def "Sbytevector_length(x)"
          (format "((iptr)((uptr)~a>>~d))"
            (access "x" bytevector type)
            ($ bytevector-length-offset)))
        (defref Sbytevector_u8_ref bytevector data)
        (comment "Warning: Sbytevector_data(x) returns a pointer into x.")
        (def "Sbytevector_data(x)" "&Sbytevector_u8_ref(x,0)")
  
        (def "Sstring_length(x)"
          (format "((iptr)((uptr)~a>>~d))"
            (access "x" string type)
            ($ string-length-offset)))
        (def "Sstring_ref(x,i)"
          (format "Schar_value~a" (access "x" "i" string data)))

        (defref Sunbox box ref)
  
        (def "Sstencil_vector_length(x)"
          (format "Spopcount(((uptr)~a)>>~d)"
            (access "x" stencil-vector type)
            ($ stencil-vector-mask-offset)))
        (defref Sstencil_vector_ref stencil-vector data)
        
        (export "iptr" "Sinteger_value" "(ptr)")
        (def "Sunsigned_value(x)" "(uptr)Sinteger_value(x)")
        (export "Sint32_t" "Sinteger32_value" "(ptr)")
        (def "Sunsigned32_value(x)" "(Suint32_t)Sinteger32_value(x)")
        (export "Sint64_t" "Sinteger64_value" "(ptr)")
        (export "int" "Stry_integer_value" "(ptr, iptr*, const char**)")
        (export "int" "Stry_integer32_value" "(ptr, Sint32_t*, const char**)")
        (export "int" "Stry_integer64_value" "(ptr, Sint64_t*, const char**)")
        (def "Sunsigned64_value(x)" "(Suint64_t)Sinteger64_value(x)")
        (export "int" "Stry_unsigned_value" "(ptr, uptr*, const char**)")
        (export "int" "Stry_unsigned32_value" "(ptr, Suint32_t*, const char**)")
        (export "int" "Stry_unsigned64_value" "(ptr, Suint64_t*, const char**)")
  
        (nl) (comment "Mutators")
        (export "void" "Sset_box" "(ptr, ptr)")
        (export "void" "Sset_car" "(ptr, ptr)")
        (export "void" "Sset_cdr" "(ptr, ptr)")
        (def "Sstring_set(x,i,c)"
          (format "((void)(~a = (string_char)(uptr)Schar(c)))"
            (access "x" "i" string data)))
        (def "Sfxvector_set(x,i,n)" "((void)(Sfxvector_ref(x,i) = (n)))")
        (def "Sflvector_set(x,i,n)" "((void)(Sflvector_ref(x,i) = (n)))")
        (def "Sbytevector_u8_set(x,i,n)" "((void)(Sbytevector_u8_ref(x,i) = (n)))")
        (export "void" "Svector_set" "(ptr, iptr, ptr)")
  
        (nl) (comment "Constructors")
        (def "Sfixnum(x)" (format "((ptr)(uptr)((x)*~d))" ($ fixnum-factor)))
        (def "Schar(x)"
          (format "((ptr)(uptr)((x)<<~d|0x~x))"
            ($ char-data-offset)
            ($ type-char)))
        (def "Snil" (format "((ptr)0x~x)" ($ snil)))
        (def "Strue" (format "((ptr)0x~x)" ($ strue)))
        (def "Sfalse" (format "((ptr)0x~x)" ($ sfalse)))
        (def "Sboolean(x)" "((x)?Strue:Sfalse)")
        (def "Sbwp_object" (format "((ptr)0x~x)" ($ sbwp)))
        (def "Seof_object" (format "((ptr)0x~x)" ($ seof)))
        (def "Svoid" (format "((ptr)0x~x)" ($ svoid)))
  
        (export "ptr" "Scons" "(ptr, ptr)")
        (export "ptr" "Sstring_to_symbol" "(const char *)")
        (export "ptr" "Ssymbol_to_string" "(ptr)")
        (export "ptr" "Sflonum" "(double)")
        (export "ptr" "Smake_vector" "(iptr, ptr)")
        (export "ptr" "Smake_fxvector" "(iptr, ptr)")
        (export "ptr" "Smake_flvector" "(iptr, double)")
        (export "ptr" "Smake_bytevector" "(iptr, int)")
        (export "ptr" "Smake_string" "(iptr, int)")
        (export "ptr" "Smake_uninitialized_string" "(iptr)")
        (export "ptr" "Sstring" "(const char *)")
        (export "ptr" "Sstring_of_length" "(const char *, iptr)")
        (export "ptr" "Sstring_utf8" "(const char*, iptr)")
        (export "ptr" "Sbox" "(ptr)")
        (export "ptr" "Sinteger" "(iptr)")
        (export "ptr" "Sunsigned" "(uptr)")
        (export "ptr" "Sinteger32" "(Sint32_t)")
        (export "ptr" "Sunsigned32" "(Suint32_t)")
        (export "ptr" "Sinteger64" "(Sint64_t)")
        (export "ptr" "Sunsigned64" "(Suint64_t)")

        (nl) (comment "Records")
        (defref Srecord_uniform_ref record data)
        (export "ptr" "Srecord_type" "(ptr)")
        (export "ptr" "Srecord_type_parent" "(ptr)")
        (export "int" "Srecord_type_uniformp" "(ptr)")
        (export "uptr" "Srecord_type_size" "(ptr)")

        (nl) (comment "Miscellaneous")
        (export "ptr" "Stop_level_value" "(ptr)")
        (export "void" "Sset_top_level_value" "(ptr, ptr)")
        (export "void" "Slock_object" "(ptr)")
        (export "void" "Sunlock_object" "(ptr)")
        (export "int" "Slocked_objectp" "(ptr)")
        (export "void" "Sforeign_symbol" "(const char *, void *)")
        (export "void" "Sregister_symbol" "(const char *, void *)")
  
        (nl) (comment "Support for calls into Scheme")
        (export "ptr" "Scall0" "(ptr)")
        (export "ptr" "Scall1" "(ptr, ptr)")
        (export "ptr" "Scall2" "(ptr, ptr, ptr)")
        (export "ptr" "Scall3" "(ptr, ptr, ptr, ptr)")
        (export "void" "Sinitframe" "(iptr)")
        (export "void" "Sput_arg" "(iptr, ptr)")
        (export "ptr" "Scall" "(ptr, iptr)")
        (constant-case architecture
          [(pb)
           (def "Sforeign_callable_entry_point(x)"
                "TO_PTR(Svector_ref(x, 2))")
           (export "ptr" "Sforeign_callable_code_object" "(void*)")]
          [else
           (comment "Warning: Sforeign_callable_entry_point(x) returns a pointer into x.")
           (def "Sforeign_callable_entry_point(x)"
                (&ref "(void (*)(void))" "x" ($ code-data-disp)))
           (def "Sforeign_callable_code_object(x)"
                (&ref "(ptr)" "x" (- ($ code-data-disp))))])
  
        (nl) (comment "Customization support.")
        (export "const char *" "Skernel_version" "(void)")
        (export "void" "Sretain_static_relocation" "(void)")
        (export "void" "Sset_verbose" "(int)")
        (export "void" "Sscheme_init" "(void (*)(void))")
        (export "void" "Sregister_boot_file" "(const char *)")
        (export "void" "Sregister_boot_executable_relative_file" "(const char *, const char *)")
        (export "void" "Sregister_boot_relative_file" "(const char *)")
        (export "void" "Sregister_boot_file_fd" "(const char *, int)")
        (export "void" "Sregister_boot_file_fd_region" "(const char *, int, iptr, iptr, int)")
        (export "void" "Sregister_boot_file_bytes" "(const char *, void *, iptr)")
        (export "void" "Sregister_heap_file" "(const char *)")
        (export "void" "Scompact_heap" "(void)")
        (export "void" "Ssave_heap" "(const char *, int)")
        (export "void" "Sbuild_heap" "(const char *, void (*)(void))")
        (export "void" "Senable_expeditor" "(const char *)")
        (export "int"  "Sscheme_start" "(int, const char *[])")
        (export "int"  "Sscheme_script" "(const char *, int, const char *[])")
        (export "int"  "Sscheme_program" "(const char *, int, const char *[])")
        (export "void" "Sscheme_deinit" "(void)")
        (export "void" "Sscheme_register_signal_registerer" "(void (*f)(int))")
        (constant-case architecture
          [(pb)
           (export "void" "Sregister_pbchunks" "(void **, int, int)")]
          [else (void)])

        (when-feature pthreads
        (nl) (comment "Thread support.")
          (export "int" "Sactivate_thread" "(void)")
          (export "void" "Sdeactivate_thread" "(void)")
          (export "int" "Sdestroy_thread" "(void)")
        )

        (let ()
          (define (gen-windows pre post)
            (nl) (comment "Windows support.")
            (pre)
            (pr "#include <wchar.h>~%")
            (export "char *" "Sgetenv" "(const char *)")
            (export "wchar_t *" "Sutf8_to_wide" "(const char *)")
            (export "char *" "Swide_to_utf8" "(const wchar_t *)")
            (post))
          (constant-case architecture
            [(pb)
             (gen-windows (lambda ()
                            (pr "#if defined(FEATURE_WINDOWS)\n"))
                          (lambda ()
                            (pr "#endif\n")))]
            [else
             (when-feature windows (gen-windows void void))]))

        (nl) (comment "Features.")
        (for-each
          (lambda (x) (pr "#define FEATURE_~@:(~a~)~%" (sanitize x)))
          (feature-list))

        (constant-case architecture
          [(pb)
           (nl) (comment "C call prototypes.")
           (pr "#include <stdint.h>\n")
           (for-each
            (lambda (proto+id)
              (let ([proto (car proto+id)])
                (define (sym->type s)
                  (case s
                    [(int8) 'int8_t]
                    [(int16) 'int16_t]
                    [(int32) 'int32_t]
                    [(uint32) 'uint32_t]
                    [(int64) 'int64_t]
                    [(uint64) 'uint64_t]
                    [else s]))
                (define (clean-type s)
                  (case s
                    [(void*) 'voids]
                    [else s]))
                (pr "typedef ~a (*pb_~a_t)(~a);~%"
                    (sym->type (car proto))
                    (apply string-append
                           (symbol->string (clean-type (car proto)))
                           (map (lambda (s) (format "_~a" (clean-type s)))
                                (cdr proto)))
                    (if (null? (cdr proto))
                        ""
                        (apply string-append
                               (symbol->string (sym->type (cadr proto)))
                               (map (lambda (s) (format ", ~a" (sym->type s)))
                                    (cddr proto)))))))
            (reverse (constant pb-prototype-table)))]
          [else (void)])

        (nl) (comment "Locking macros.")
        (constant-case architecture
          [(x86)
           (if-feature windows
             ;; Using compiler intrinsics on 32-bit Windows because the inline
             ;; assembler does not support anonymous labels, and using named
             ;; labels leads to label name conflicts if SPINLOCK is used more
             ;; than once in the same C procedure.
             (begin
               (pr "#define INITLOCK(addr) (*((long *) addr) = 0)~%")

               (nl)
               (pr "#define SPINLOCK(addr)                         \\~%")
               (pr "{                                              \\~%")
               (pr "  while (_InterlockedExchange(addr, 1) != 0) { \\~%")
               (pr "    while(*((long *) addr) != 0);              \\~%")
               (pr "  }                                            \\~%")
               (pr "} while(0)                                       ~%")

               (nl)
               (pr "#define UNLOCK(addr) (*((long *) addr) = 0)~%")

               (nl)
               (pr "#define LOCKED_INCR(addr, res) (res = (-1 == _InterlockedExchangeAdd(addr, 1)))~%")

               (nl)
               (pr "#define LOCKED_DECR(addr, res) (res = (1 == _InterlockedExchangeAdd(addr, -1)))~%"))
             (begin
               (pr "#define INITLOCK(addr)     \\~%")
               (pr "  __asm__ __volatile__ (\"movl $0, (%0)\"\\~%")
               (pr "                        :             \\~%")
               (pr "                        : \"r\" (addr)  \\~%")
               (pr "                        : \"memory\")~%")

               (nl)
               (pr "#define SPINLOCK(addr)      \\~%")
               (pr "  __asm__ __volatile__ (\"0:\\n\\t\"\\~%")
               (pr "                        \"movl $1, %%eax\\n\\t\"\\~%")
               (pr "                        \"xchgl (%0), %%eax\\n\\t\"\\~%")
               (pr "                        \"cmpl $0, %%eax\\n\\t\"\\~%")
               (pr "                        \"je 2f\\n\\t\"\\~%")
               (pr "                        \"1:\\n\\t\"\\~%")
               (pr "                        \"pause\\n\\t\"\\~%")
               (pr "                        \"cmpl $0, (%0)\\n\\t\"\\~%")
               (pr "                        \"je 0b\\n\\t\"\\~%")
               (pr "                        \"jmp 1b\\n\\t\"\\~%")
               (pr "                        \"2:\"\\~%")
               (pr "                        :                \\~%")
               (pr "                        : \"r\" (addr)     \\~%")
               (pr "                        : \"eax\", \"flags\", \"memory\")~%")

               (nl)
               (pr "#define UNLOCK(addr)     \\~%")
               (pr "  __asm__ __volatile__ (\"movl $0, (%0)\"\\~%")
               (pr "                        :             \\~%")
               (pr "                        : \"r\" (addr)  \\~%")
               (pr "                        : \"memory\")~%")

               (nl)
               (pr "#define LOCKED_INCR(addr, ret) \\~%")
               (pr "  __asm__ __volatile__ (\"lock; incl (%1)\\n\\t\"\\~%")
               (pr "                        \"sete %b0\\n\\t\"\\~%")
               (pr "                        \"movzx %b0, %0\\n\\t\"\\~%")
               (pr "                        : \"=q\" (ret)   \\~%")
               (pr "                        : \"r\" (addr)   \\~%")
               (pr "                        : \"flags\", \"memory\")~%")

               (nl)
               (pr "#define LOCKED_DECR(addr, ret) \\~%")
               (pr "  __asm__ __volatile__ (\"lock; decl (%1)\\n\\t\"\\~%")
               (pr "                        \"sete %b0\\n\\t\"\\~%")
               (pr "                        \"movzx %b0, %0\\n\\t\"\\~%")
               (pr "                        : \"=q\" (ret)   \\~%")
               (pr "                        : \"r\" (addr)   \\~%")
               (pr "                        : \"flags\", \"memory\")~%")))]
          [(x86_64)
           (if-feature windows
             ;; Visual C for 64-bit Windows does not support inline assembler, so we are using
             ;; intrinsics here instead.  At /O2, VC seems to produced assembly
             ;; code similar to our hand-code assembler.
             ;; Note that using the Acquire or Release version of these functions (or the
             ;; equivalent _acq or _rel versions of the intrinsics) produces calls to the
             ;; intrinsic rather than the inlined assembly produced by the intrinsics used here,
             ;; despite the documentation indicating the Acquire and Release vesions produce
             ;; better performing code.
             (begin
               (pr "#define INITLOCK(addr) (*((long long *) addr) = 0)~%")

               (nl)
               (pr "#define SPINLOCK(addr)                           \\~%")
               (pr "{                                                \\~%")
               (pr "  while (_InterlockedExchange64(addr, 1) != 0) { \\~%")
               (pr "    while(*((long long *) addr) != 0);           \\~%")
               (pr "  }                                              \\~%")
               (pr "} while(0)                                         ~%")

               (nl)
               (pr "#define UNLOCK(addr) (*((long long *) addr) = 0)~%")

               (nl)
               (pr "#define LOCKED_INCR(addr, res) (res = (-1 == _InterlockedExchangeAdd64(addr, 1)))~%")

               (nl)
               (pr "#define LOCKED_DECR(addr, res) (res = (1 == _InterlockedExchangeAdd64(addr, -1)))~%"))
             (begin
               (pr "#define INITLOCK(addr)     \\~%")
               (pr "  __asm__ __volatile__ (\"movq $0, (%0)\"\\~%")
               (pr "                        :             \\~%")
               (pr "                        : \"r\" (addr)  \\~%")
               (pr "                        : \"memory\")~%")

               (nl)
               (pr "#define SPINLOCK(addr)      \\~%")
               (pr "  __asm__ __volatile__ (\"0:\\n\\t\"\\~%")
               (pr "                        \"movq $1, %%rax\\n\\t\"\\~%")
               (pr "                        \"xchgq (%0), %%rax\\n\\t\"\\~%")
               (pr "                        \"cmpq $0, %%rax\\n\\t\"\\~%")
               (pr "                        \"je 2f\\n\\t\"\\~%")
               (pr "                        \"1:\\n\\t\"\\~%")
               (pr "                        \"pause\\n\\t\"\\~%")
               (pr "                        \"cmpq $0, (%0)\\n\\t\"\\~%")
               (pr "                        \"je 0b\\n\\t\"\\~%")
               (pr "                        \"jmp 1b\\n\\t\"\\~%")
               (pr "                        \"2:\"\\~%")
               (pr "                        :                \\~%")
               (pr "                        : \"r\" (addr)     \\~%")
               (pr "                        : \"rax\", \"flags\", \"memory\")~%")

               (nl)
               (pr "#define UNLOCK(addr)     \\~%")
               (pr "  __asm__ __volatile__ (\"movq $0, (%0)\"\\~%")
               (pr "                        :             \\~%")
               (pr "                        : \"r\" (addr)  \\~%")
               (pr "                        :\"memory\")~%")

               (nl)
               (pr "#define LOCKED_INCR(addr, ret) \\~%")
               (pr "  __asm__ __volatile__ (\"lock; incq (%1)\\n\\t\"\\~%")
               (pr "                        \"sete %b0\\n\\t\"\\~%")
               (pr "                        \"movzx %b0, %0\\n\\t\"\\~%")
               (pr "                        : \"=q\" (ret)   \\~%")
               (pr "                        : \"r\" (addr)   \\~%")
               (pr "                        : \"flags\", \"memory\")~%")

               (nl)
               (pr "#define LOCKED_DECR(addr, ret) \\~%")
               (pr "  __asm__ __volatile__ (\"lock; decq (%1)\\n\\t\"\\~%")
               (pr "                        \"sete %b0\\n\\t\"\\~%")
               (pr "                        \"movzx %b0, %0\\n\\t\"\\~%")
               (pr "                        : \"=q\" (ret)   \\~%")
               (pr "                        : \"r\" (addr)   \\~%")
               (pr "                        : \"flags\", \"memory\")~%")))]
          [(ppc32)
           (let ([reg (constant-case machine-type-name
                        [(ppc32osx tppc32osx) ""]
                        [else "%%"])])
            (pr "#define INITLOCK(addr)     \\~%")
            (pr "  __asm__ __volatile__ (\"li ~ar0, 0\\n\\t\"\\~%" reg)
            (pr "                        \"stw ~ar0, 0(%0)\\n\\t\"\\~%" reg)
            (pr "                        :             \\~%")
            (pr "                        : \"b\" (addr)\\~%")
            (pr "                        :\"memory\", \"r0\")~%")

            (nl)
            (pr "#define SPINLOCK(addr)      \\~%")
            (pr "  __asm__ __volatile__ (\"0:\\n\\t\"\\~%")                    ; top:
            (pr "                        \"lwarx ~ar0, 0, %0\\n\\t\"\\~%" reg) ;  start lock acquisition
            (pr "                        \"cmpwi ~ar0, 0\\n\\t\"\\~%" reg)     ;  see if someone already owns the lock
            (pr "                        \"bne 1f\\n\\t\"\\~%")                ;  if so, go to our try_again loop
            (pr "                        \"li ~ar0, 1\\n\\t\"\\~%" reg)        ;  attempt to store the value 1
            (pr "                        \"stwcx. ~ar0, 0, %0\\n\\t\"\\~%" reg); 
            (pr "                        \"beq 2f\\n\\t\"\\~%")                ;  if we succeed, we own the lock
            (pr "                        \"1:\\n\\t\"\\~%")                    ; again:
            (pr "                        \"isync\\n\\t\"\\~%")                 ;  sync things to pause the processor 
            (pr "                        \"lwz ~ar0, 0(%0)\\n\\t\"\\~%" reg)   ;  try a non-reserved load to see if we are likely to succeed
            (pr "                        \"cmpwi ~ar0, 0\\n\\t\"\\~%" reg)     ;  if it is = 0, try to acquire at start
            (pr "                        \"beq 0b\\n\\t\"\\~%")                ;
            (pr "                        \"b 1b\\n\\t\"\\~%")                  ;  otherwise loop through the try again
            (pr "                        \"2:\\n\\t\"\\~%")                    ; done:
            (pr "                        :                \\~%")
            (pr "                        : \"b\" (addr)\\~%")
            (pr "                        : \"cc\", \"memory\", \"r0\")~%")

            (nl)
            (pr "#define UNLOCK(addr)     \\~%")
            (pr "  __asm__ __volatile__ (\"li ~ar0, 0\\n\\t\"\\~%" reg)
            (pr "                        \"stw ~ar0, 0(%0)\\n\\t\"\\~%" reg)
            (pr "                        :             \\~%")
            (pr "                        : \"b\" (addr)\\~%")
            (pr "                        :\"memory\", \"r0\")~%")

            (nl)
            (pr "#define LOCKED_INCR(addr, ret) \\~%")
            (pr "  __asm__ __volatile__ (\"li %0, 0\\n\\t\"\\~%")
            (pr "                        \"0:\\n\\t\"\\~%")
            (pr "                        \"lwarx ~ar12, 0, %1\\n\\t\"\\~%" reg)
            (pr "                        \"addi ~ar12, ~ar12, 1\\n\\t\"\\~%" reg reg)
            (pr "                        \"stwcx. ~ar12, 0, %1\\n\\t\"\\~%" reg)
            (pr "                        \"bne 0b\\n\\t\"\\~%")
            (pr "                        \"cmpwi ~ar12, 0\\n\\t\"\\~%" reg)
            (pr "                        \"bne 1f\\n\\t\"\\~%")
            (pr "                        \"li %0, 1\\n\\t\"\\~%")
            (pr "                        \"1:\\n\\t\"\\~%")
            (pr "                        : \"=&r\" (ret)\\~%")
            (pr "                        : \"r\" (addr)\\~%")
            (pr "                        : \"cc\", \"memory\", \"r12\")~%")

            (nl)
            (pr "#define LOCKED_DECR(addr, ret) \\~%")
            (pr "  __asm__ __volatile__ (\"li %0, 0\\n\\t\"\\~%")
            (pr "                        \"0:\\n\\t\"\\~%")
            (pr "                        \"lwarx ~ar12, 0, %1\\n\\t\"\\~%" reg)
            (pr "                        \"addi ~ar12, ~ar12, -1\\n\\t\"\\~%" reg reg)
            (pr "                        \"stwcx. ~ar12, 0, %1\\n\\t\"\\~%" reg)
            (pr "                        \"bne 0b\\n\\t\"\\~%")
            (pr "                        \"cmpwi ~ar12, 0\\n\\t\"\\~%" reg)
            (pr "                        \"bne 1f\\n\\t\"\\~%")
            (pr "                        \"li %0, 1\\n\\t\"\\~%")
            (pr "                        \"1:\\n\\t\"\\~%")
            (pr "                        : \"=&r\" (ret)\\~%")
            (pr "                        : \"r\" (addr)\\~%")
            (pr "                        : \"cc\", \"memory\", \"r12\")~%"))]
          [(arm32)
            (pr "#define INITLOCK(addr)     \\~%")
            (pr "  __asm__ __volatile__ (\"mov r12, #0\\n\\t\"\\~%")
            (pr "                        \"str r12, [%0, #0]\\n\\t\"\\~%")
            (pr "                        :             \\~%")
            (pr "                        : \"r\" (addr)\\~%")
            (pr "                        :\"memory\", \"r12\")~%")

            (nl)
            (pr "#define SPINLOCK(addr)      \\~%")
            (pr "  __asm__ __volatile__ (\"0:\\n\\t\"\\~%")
            (pr "                        \"ldrex r12, [%0]\\n\\t\"\\~%")
            (pr "                        \"cmp r12, #0\\n\\t\"\\~%")
            (pr "                        \"bne 1f\\n\\t\"\\~%")
            (pr "                        \"mov r12, #1\\n\\t\"\\~%")
            (pr "                        \"strex r7, r12, [%0]\\n\\t\"\\~%")
            (pr "                        \"cmp r7, #0\\n\\t\"\\~%")
            (pr "                        \"beq 2f\\n\\t\"\\~%")
            (pr "                        \"1:\\n\\t\"\\~%")
            (pr "                        \"ldr r12, [%0, #0]\\n\\t\"\\~%")
            (pr "                        \"cmp r12, #0\\n\\t\"\\~%")
            (pr "                        \"beq 0b\\n\\t\"\\~%")
            (pr "                        \"b 1b\\n\\t\"\\~%")
            (pr "                        \"2:\\n\\t\"\\~%")
            (pr "                        :                \\~%")
            (pr "                        : \"r\" (addr)\\~%")
            (pr "                        : \"cc\", \"memory\", \"r12\", \"r7\")~%")

            (nl)
            (pr "#define UNLOCK(addr)     \\~%")
            (pr "  __asm__ __volatile__ (\"mov r12, #0\\n\\t\"\\~%")
            (pr "                        \"str r12, [%0, #0]\\n\\t\"\\~%")
            (pr "                        :             \\~%")
            (pr "                        : \"r\" (addr)\\~%")
            (pr "                        :\"memory\", \"r12\")~%")

            (nl)
            (pr "#define LOCKED_INCR(addr, ret) \\~%")
            (pr "  __asm__ __volatile__ (\"mov %0, #0\\n\\t\"\\~%")
            (pr "                        \"0:\\n\\t\"\\~%")
            (pr "                        \"ldrex r12, [%1]\\n\\t\"\\~%")
            (pr "                        \"add r12, r12, #1\\n\\t\"\\~%")
            (pr "                        \"strex r7, r12, [%1]\\n\\t\"\\~%")
            (pr "                        \"cmp r7, #0\\n\\t\"\\~%")
            (pr "                        \"bne 0b\\n\\t\"\\~%")
            (pr "                        \"cmp r12, #0\\n\\t\"\\~%")
            (pr "                        \"it eq\\n\\t\"\\~%")
            (pr "                        \"moveq %0, #1\\n\\t\"\\~%")
            (pr "                        : \"=&r\" (ret)\\~%")
            (pr "                        : \"r\" (addr)\\~%")
            (pr "                        : \"cc\", \"memory\", \"r12\", \"r7\")~%")

            (nl)
            (pr "#define LOCKED_DECR(addr, ret) \\~%")
            (pr "  __asm__ __volatile__ (\"mov %0, #0\\n\\t\"\\~%")
            (pr "                        \"0:\\n\\t\"\\~%")
            (pr "                        \"ldrex r12, [%1]\\n\\t\"\\~%")
            (pr "                        \"sub r12, r12, #1\\n\\t\"\\~%")
            (pr "                        \"strex r7, r12, [%1]\\n\\t\"\\~%")
            (pr "                        \"cmp r7, #0\\n\\t\"\\~%")
            (pr "                        \"bne 0b\\n\\t\"\\~%")
            (pr "                        \"cmp r12, #0\\n\\t\"\\~%")
            (pr "                        \"it eq\\n\\t\"\\~%")
            (pr "                        \"moveq %0, #1\\n\\t\"\\~%")
            (pr "                        : \"=&r\" (ret)\\~%")
            (pr "                        : \"r\" (addr)\\~%")
            (pr "                        : \"cc\", \"memory\", \"r12\", \"r7\")~%")]
          [(arm64)
           (if-feature windows
             (begin
               (pr "#define INITLOCK(addr) (*((long long *) addr) = 0)~%")

               (nl)
               (pr "#define SPINLOCK(addr)                           \\~%")
               (pr "{                                                \\~%")
               (pr "  while (_InterlockedExchange64(addr, 1) != 0) { \\~%")
               (pr "    while(*((long long *) addr) != 0);           \\~%")
               (pr "  }                                              \\~%")
               (pr "} while(0)                                         ~%")

               (nl)
               (pr "#define UNLOCK(addr) (*((long long *) addr) = 0)~%")

               (nl)
               (pr "#define LOCKED_INCR(addr, res) (res = (-1 == _InterlockedExchangeAdd64(addr, 1)))~%")

               (nl)
               (pr "#define LOCKED_DECR(addr, res) (res = (1 == _InterlockedExchangeAdd64(addr, -1)))~%"))
             (begin
               (pr "#define INITLOCK(addr)     \\~%")
               (pr "  __asm__ __volatile__ (\"mov x12, #0\\n\\t\"\\~%")
               (pr "                        \"str x12, [%0, #0]\\n\\t\"\\~%")
               (pr "                        :             \\~%")
               (pr "                        : \"r\" (addr)\\~%")
               (pr "                        :\"memory\", \"x12\")~%")

               (nl)
               (pr "#define SPINLOCK(addr)      \\~%")
               (pr "  __asm__ __volatile__ (\"0:\\n\\t\"\\~%")
               (pr "                        \"ldxr x12, [%0, #0]\\n\\t\"\\~%")
               (pr "                        \"cmp x12, #0\\n\\t\"\\~%")
               (pr "                        \"bne 1f\\n\\t\"\\~%")
               (pr "                        \"mov x12, #1\\n\\t\"\\~%")
               (pr "                        \"stxr w7, x12, [%0]\\n\\t\"\\~%")
               (pr "                        \"cmp w7, #0\\n\\t\"\\~%")
               (pr "                        \"beq 2f\\n\\t\"\\~%")
               (pr "                        \"1:\\n\\t\"\\~%")
               (pr "                        \"ldr x12, [%0, #0]\\n\\t\"\\~%")
               (pr "                        \"cmp x12, #0\\n\\t\"\\~%")
               (pr "                        \"beq 0b\\n\\t\"\\~%")
               (pr "                        \"b 1b\\n\\t\"\\~%")
               (pr "                        \"2:\\n\\t\"\\~%")
               (pr "                        :                \\~%")
               (pr "                        : \"r\" (addr)\\~%")
               (pr "                        : \"cc\", \"memory\", \"x12\", \"x7\")~%")

               (nl)
               (pr "#define UNLOCK(addr)     \\~%")
               (pr "  __asm__ __volatile__ (\"mov x12, #0\\n\\t\"\\~%")
               (pr "                        \"str x12, [%0, #0]\\n\\t\"\\~%")
               (pr "                        :             \\~%")
               (pr "                        : \"r\" (addr)\\~%")
               (pr "                        :\"memory\", \"x12\")~%")

               (nl)
               (pr "#define LOCKED_INCR(addr, ret) \\~%")
               (pr "  do {\\~%")
               (pr "  long _return_;\\~%")
               (pr "  __asm__ __volatile__ (\"mov %0, #0\\n\\t\"\\~%")
               (pr "                        \"0:\\n\\t\"\\~%")
               (pr "                        \"ldxr x12, [%1, #0]\\n\\t\"\\~%")
               (pr "                        \"add x12, x12, #1\\n\\t\"\\~%")
               (pr "                        \"stxr w7, x12, [%1]\\n\\t\"\\~%")
               (pr "                        \"cmp w7, #0\\n\\t\"\\~%")
               (pr "                        \"bne 0b\\n\\t\"\\~%")
               (pr "                        \"cmp x12, #0\\n\\t\"\\~%")
               (pr "                        \"bne 1f\\n\\t\"\\~%")
               (pr "                        \"mov %0, #1\\n\\t\"\\~%")
               (pr "                        \"1:\\n\\t\"\\~%")
               (pr "                        : \"=&r\" (_return_)\\~%")
               (pr "                        : \"r\" (addr)\\~%")
               (pr "                        : \"cc\", \"memory\", \"x12\", \"x7\");\\~%")
               (pr "  ret = _return_;\\~%")
               (pr "  } while (0)~%")

               (nl)
               (pr "#define LOCKED_DECR(addr, ret) \\~%")
               (pr "  do {\\~%")
               (pr "  long _return_;\\~%")
               (pr "  __asm__ __volatile__ (\"mov %0, #0\\n\\t\"\\~%")
               (pr "                        \"0:\\n\\t\"\\~%")
               (pr "                        \"ldxr x12, [%1, #0]\\n\\t\"\\~%")
               (pr "                        \"sub x12, x12, #1\\n\\t\"\\~%")
               (pr "                        \"stxr w7, x12, [%1]\\n\\t\"\\~%")
               (pr "                        \"cmp w7, #0\\n\\t\"\\~%")
               (pr "                        \"bne 0b\\n\\t\"\\~%")
               (pr "                        \"cmp x12, #0\\n\\t\"\\~%")
               (pr "                        \"bne 1f\\n\\t\"\\~%")
               (pr "                        \"mov %0, #1\\n\\t\"\\~%")
               (pr "                        \"1:\\n\\t\"\\~%")
               (pr "                        : \"=&r\" (_return_)\\~%")
               (pr "                        : \"r\" (addr)\\~%")
               (pr "                        : \"cc\", \"memory\", \"x12\", \"x7\");\\~%")
               (pr "  ret = _return_;\\~%")
               (pr "  } while (0)~%")))]
          [(riscv64)
           (pr "#define INITLOCK(addr) \\~%")
           (pr "  __asm__ __volatile__ (\"li t0, 0\\n\\t\" \\~%")
           (pr "                        \"sd t0, (%0)\\n\\t\" \\~%")
           (pr "                        : \\~%")
           (pr "                        : \"r\" (addr)  \\~%")
           (pr "                        : \"memory\", \"t0\")~%")

           (nl)
           (pr "#define SPINLOCK(addr) \\~%")
           (pr "  __asm__ __volatile__ (\"1:\\n\\t\" \\~%")
           (pr "                        \"lr.d t0, (%[Addr])\\n\\t\" \\~%")
           (pr "                        \"bne  t0, zero, 2f\\n\\t\" \\~%")
           (pr "                        \"li   t0, 1\\n\\t\" \\~%")
           (pr "                        \"sc.d t1, t0, (%[Addr])\\n\\t\" \\~%")
           (pr "                        \"beq  t1, zero, 3f\\n\\t\" \\~%")
           (pr "                        \"2:\\n\\t\" \\~%")
           (pr "                        \"ld   t0, (%[Addr])\\n\\t\" \\~%")
           (pr "                        \"beq  t0, zero, 1b\\n\\t\" \\~%")
           (pr "                        \"j 2b\\n\\t\" \\~%")
           (pr "                        \"3:\\n\\t\" \\~%")
           (pr "                        : \\~%")
           (pr "                        : [Addr] \"r\" (addr) \\~%")
           (pr "                        : \"memory\", \"t0\", \"t1\")~%")

           (nl)
           (pr "#define UNLOCK(addr) \\~%")
           (pr "  __asm__ __volatile__ (\"li t0, 0\\n\\t\" \\~%")
           (pr "                        \"sd t0, (%0)\\n\\t\" \\~%")
           (pr "                        : \\~%")
           (pr "                        : \"r\" (addr) \\~%")
           (pr "                        : \"memory\", \"t0\")~%")

           (nl)
           (pr "#define LOCKED_INCR(addr, ret) \\~%")
           (pr "  do { \\~%")
           (pr "    long _return_; \\~%")
           (pr "    __asm__ __volatile__ (\"li %0, 0\\n\\t\" \\~%")
           (pr "                          \"1:\\n\\t\" \\~%")
           (pr "                          \"lr.d t0, (%1)\\n\\t\" \\~%")
           (pr "                          \"addi t0, t0, 1\\n\\t\" \\~%")
           (pr "                          \"sc.d t1, t0, (%1)\\n\\t\" \\~%")
           (pr "                          \"bne  t1, zero, 1b\\n\\t\" \\~%")
           (pr "                          \"bne  t0, zero, 2f\\n\\t\" \\~%")
           (pr "                          \"li   %0, 1\\n\\t\" \\~%")
           (pr "                          \"2:\\n\\t\" \\~%")
           (pr "                          : \"=&r\" (_return_) \\~%")
           (pr "                          : \"r\" (addr) \\~%")
           (pr "                          : \"memory\", \"t0\", \"t1\" \\~%")
           (pr "                          ); \\~%")
           (pr "    ret = _return_; \\~%")
           (pr "  } while (0)~%")

           (nl)
           (pr "#define LOCKED_DECR(addr, ret) \\~%")
           (pr "  do { \\~%")
           (pr "    long _return_; \\~%")
           (pr "    __asm__ __volatile__ (\"li %0, 0\\n\\t\" \\~%")
           (pr "                          \"1:\\n\\t\" \\~%")
           (pr "                          \"lr.d t0, (%1)\\n\\t\" \\~%")
           (pr "                          \"addi t0, t0, -1\\n\\t\" \\~%")
           (pr "                          \"sc.d t1, t0, (%1)\\n\\t\" \\~%")
           (pr "                          \"bne  t1, zero, 1b\\n\\t\" \\~%")
           (pr "                          \"bne  t0, zero, 2f\\n\\t\" \\~%")
           (pr "                          \"li   %0, 1\\n\\t\" \\~%")
           (pr "                          \"2:\\n\\t\" \\~%")
           (pr "                          : \"=&r\" (_return_) \\~%")
           (pr "                          : \"r\" (addr) \\~%")
           (pr "                          : \"memory\", \"t0\", \"t1\" \\~%")
           (pr "                          ); \\~%")
           (pr "    ret = _return_; \\~%")
           (pr " } while (0)~%")]
          [(loongarch64)
           (pr "#define INITLOCK(addr) \\~%")
           (pr "  __asm__ __volatile__ (\"st.d $r0, %0, 0   \\n\\t\" \\~%")
           (pr "                        : \\~%")
           (pr "                        : \"r\" (addr) \\~%")
           (pr "                        : \"memory\")~%")

           (nl)
           (pr "#define SPINLOCK(addr) \\~%")
           (pr "  __asm__ __volatile__ (\"1:\\n\\t\" \\~%")
           (pr "                        \"ll.d $r12, %[Addr], 0   \\n\\t\" \\~%")
           (pr "                        \"bnez $r12, 3f        \\n\\t\" \\~%")
           (pr "                        \"addi.d $r12, $r0, 1  \\n\\t\" \\~%")
           (pr "                        \"sc.d $r12, %[Addr], 0   \\n\\t\" \\~%")
           ;; success when r12 != 0, which is different from riscv
           (pr "                        \"bnez $r12, 2f        \\n\\t\" \\~%")
           (pr "                        \"3:\\n\\t\" \\~%")
           (pr "                        \"ld.d $r12, %[Addr], 0   \\n\\t\" \\~%")
           (pr "                        \"beqz $r12, 1b        \\n\\t\" \\~%")
           (pr "                        \"b 3b                 \\n\\t\" \\~%")
           (pr "                        \"2:\\n\\t\" \\~%")
           (pr "                        : \\~%")
           (pr "                        : [Addr] \"r\" (addr) \\~%")
           (pr "                        : \"memory\", \"r12\")~%")

           (nl)
           (pr "#define UNLOCK(addr) \\~%")
           (pr "  __asm__ __volatile__ (\"st.d $r0, %0, 0    \\n\\t\" \\~%")
           (pr "                        : \\~%")
           (pr "                        : \"r\" (addr) \\~%")
           (pr "                        : \"memory\")~%")

           (nl)
           (pr "#define LOCKED_INCR(addr, ret)                                  \\~%")
           (pr "  do {                                                         \\~%")
           (pr "       long _ret_;                                             \\~%")
           (pr "       long _temp_;                                            \\~%")
           (pr "       __asm__ __volatile__ (\"addi.d %[_ret_], $r0, 0   \\n\\t\"  \\~%")
           (pr "                             \"1:\\n\\t\"                          \\~%")
           (pr "                             \"ll.d   $r12, %[_addr_], 0       \\n\\t\" \\~%")
           (pr "                             \"addi.d $r12, $r12, 1      \\n\\t\" \\~%")
           (pr "                             \"add.d %[_temp_], $r0, $r12 \\n\\t\" \\~%")
           (pr "                             \"sc.d   $r12, %[_addr_], 0       \\n\\t\" \\~%")
           (pr "                             \"beqz   $r12, 1b\\n\\t\"             \\~%")
           (pr "                             \"bnez   %[_temp_], 2f     \\n\\t\"   \\~%")
           (pr "                             \"addi.d %[_ret_], $r0, 1   \\n\\t\"  \\~%")
           (pr "                             \"2:\\n\\t\"                          \\~%")
           (pr "                             : [_ret_] \"=&r\" (_ret_), [_temp_] \"=&r\" (_temp_) \\~%")
           (pr "                             : [_addr_] \"r\" (addr)             \\~%")
           (pr "                             : \"memory\", \"r12\"   \\~%")
           (pr "               );                                              \\~%")
           (pr "       ret = _ret_;                                            \\~%")
           (pr "  } while (0)~%")

           (nl)
           (pr "#define LOCKED_DECR(addr, ret)                                 \\~%")
           (pr "  do {                                                         \\~%")
           (pr "       long _ret_;                                             \\~%")
           (pr "       long _temp_;                                            \\~%")
           (pr "       __asm__ __volatile__ (\"addi.d %[_ret_], $r0, 0   \\n\\t\"  \\~%")
           (pr "                             \"1:\\n\\t\"                          \\~%")
           (pr "                             \"ll.d   $r12, %[_addr_], 0     \\n\\t\" \\~%")
           (pr "                             \"addi.d $r12, $r12, -1      \\n\\t\" \\~%")
           (pr "                             \"add.d %[_temp_], $r0, $r12 \\n\\t\" \\~%")
           (pr "                             \"sc.d   $r12, %[_addr_], 0       \\n\\t\" \\~%")
           (pr "                             \"beqz   $r12, 1b\\n\\t\"             \\~%")
           (pr "                             \"bnez   %[_temp_], 2f     \\n\\t\"   \\~%")
           (pr "                             \"addi.d %[_ret_], $r0, 1   \\n\\t\"  \\~%")
           (pr "                             \"2:\\n\\t\"                          \\~%")
           ;; use `r` for addr so we can get the right addr operand
           (pr "                             : [_ret_] \"=&r\" (_ret_), [_temp_] \"=&r\" (_temp_) \\~%")
           (pr "                             : [_addr_] \"r\" (addr)               \\~%")
           (pr "                             : \"memory\", \"r12\"  \\~%")
           (pr "               );                                              \\~%")
           (pr "       ret = _ret_;                                            \\~%")
           (pr "  } while (0)~%")]
          [(pb)
           (pr "#define INITLOCK(addr) (*((long *) addr) = 0)~%")
           (pr "#define UNLOCK(addr) (*((long *) addr) = 0)~%")
           (if-feature pthreads
             (begin
               (pr "#define SPINLOCK(addr) S_pb_spinlock(addr)~%")
               (pr "#define LOCKED_INCR(addr, res) (res = S_pb_locked_adjust(addr, 1))~%")
               (pr "#define LOCKED_DECR(addr, res) (res = S_pb_locked_adjust(addr, -1))~%")
               (export "void" "S_pb_spinlock" "(void*)")
               (export "int" "S_pb_locked_adjust" "(void*, int)"))
             (begin
               (pr "#define SPINLOCK(addr) (*((long *) addr) = 1)~%")
               (pr "#define LOCKED_INCR(addr, res) (res = ((*(uptr*)addr)++ == -1))~%")
               (pr "#define LOCKED_DECR(addr, res) (res = ((*(uptr*)addr)-- == 1))~%")))]
          [else
            ($oops who "asm locking code is not yet defined for ~s" (constant architecture))]))))

  (set! mkequates.h
    (lambda (ofn)
      (fluid-let ([op (if (output-port? ofn)
                          ofn
                          (open-output-file ofn 'replace))])
        (comment "equates.h for Chez Scheme Version ~a" scheme-version)
  
        (nl)
        (comment "Do not edit this file.  It is automatically generated and")
        (comment "specifically tailored to the version of Chez Scheme named")
        (comment "above.  Always be certain that you have the correct version")
        (comment "of this file for the version of Chez Scheme you are using.")
  
        (nl)
        (comment "Warning: Some macros may evaluate arguments more than once.")
  
        (nl)
        (comment "Integer typedefs")
        (pr "typedef ~a I8;~%" (constant typedef-i8))
        (pr "typedef ~a U8;~%" (constant typedef-u8))
        (pr "typedef ~a I16;~%" (constant typedef-i16))
        (pr "typedef ~a U16;~%" (constant typedef-u16))
        (pr "typedef ~a I32;~%" (constant typedef-i32))
        (pr "typedef ~a U32;~%" (constant typedef-u32))
        (pr "typedef ~a I64;~%" (constant typedef-i64))
        (pr "typedef ~a U64;~%" (constant typedef-u64))

        (nl)
        (comment "constants from cmacros.ss")
        (for-each
          (lambda (x)
            (cond
              [(getprop x '*constant* #f) =>
               (lambda (k)
                 (let ([type (getprop x '*constant-ctype* #f)]
                       [c-name (sanitize x)])
                   (putprop x '*c-name* c-name)
                   (def c-name
                     (if (or (fixnum? k) (bignum? k))
                         (if (< k 0)
                             (if (or (not type) (eq? type 'int))
                                 (format "-0x~x" (- k))
                                 (format "(~s)-0x~x" type (- k)))
                             (if (or (not type) (eq? type 'int))
                                 (format "0x~x" k)
                                 (format "(~s)0x~x" type k)))
                         (if (not type)
                             (if (eq? k #f)
                                 "0"
                                 (if (eq? k #t)
                                     "1"
                                     (format "~s" k)))
                             (format "(~s)~s" type k))))))]))
          (sort (lambda (x y)
                  (string<? (symbol->string x) (symbol->string y)))
                (oblist)))
        (nl)
        (comment "constants from declare-c-entries")
        (for-each
          (lambda (x)
            (cond
              [($sgetprop x '*c-entry* #f) =>
               (lambda (k)
                 (def (format "CENTRY_~a" (sanitize x)) k))]))
          (sort (lambda (x y)
                  (string<? (symbol->string x) (symbol->string y)))
                (oblist)))

        (nl)
        (comment "displacements for records")
        (let ()
          (define print-field-disps
            (lambda (prefix rtd)
              (let-values ([(pm mpm flds size)
                            ((let () (include "layout.ss") compute-field-offsets)
                             'mkheader
                             (fx- (constant typemod) (constant type-typed-object))
                             (cons '(immutable scheme-object rtd) (csv7:record-type-field-decls rtd)))])
                (for-each
                  (lambda (fld)
                    (def (format "~a_~a_disp" prefix (sanitize (fld-name fld)))
                         (fld-byte fld)))
                  flds))))
          (print-field-disps "eq_hashtable" (let () (include "hashtable-types.ss") (record-type-descriptor eq-ht)))
          (print-field-disps "symbol_hashtable" (let () (include "hashtable-types.ss") (record-type-descriptor symbol-ht)))
          (print-field-disps "code_info" (let () (include "types.ss") (record-type-descriptor code-info))))

        (nl)
        (comment "derived endianness")
        (case (constant native-endianness)
          [(little)
           (def "native_endianness_is_little" 1)
           (def "native_endianness_is_big" 0)]
          [(big)
           (def "native_endianness_is_little" 0)
           (def "native_endianness_is_big" 1)]
          [else
           (def "native_endianness_is_little" 0)
           (def "native_endianness_is_big" 0)])
        (case (constant fasl-endianness)
          [(little)
           (def "fasl_endianness_is_little" 1)
           (def "fasl_endianness_is_big" 0)]
          [else
           (def "fasl_endianness_is_little" 0)
           (def "fasl_endianness_is_big" 1)])

        (nl)
        (comment "predicates")
        (deftypep "Simmediatep" ($ mask-immediate) ($ type-immediate))
        (deftotypep "Sportp" ($ mask-port) ($ type-port))
        (deftotypep "Scodep" ($ mask-code) ($ type-code))

        (nl)
        (comment "structure accessors")

        (definit INITCAR pair car)
        (definit INITCDR pair cdr)
        (defset SETCAR pair car)
        (defset SETCDR pair cdr)

        (defref BOXTYPE box type)
        (definit INITBOXREF box ref)
        (defset SETBOXREF box ref)

        (defref EPHEMERONPREVREF ephemeron prev-ref)
        (definit INITEPHEMERONPREVREF ephemeron prev-ref)
        (defref EPHEMERONNEXT ephemeron next)
        (definit INITEPHEMERONNEXT ephemeron next)

        (defref TLCTYPE tlc type)
        (defref TLCKEYVAL tlc keyval)
        (defref TLCHT tlc ht)
        (defref TLCNEXT tlc next)
        (definit INITTLCKEYVAL tlc keyval)
        (definit INITTLCHT tlc ht)
        (definit INITTLCNEXT tlc next)
        (defset SETTLCNEXT tlc next)

        (defref PHANTOMTYPE phantom type)
        (defref PHANTOMLEN  phantom length)

        (defref SYMVAL symbol value)
        (defref SYMPVAL symbol pvalue)
        (defref SYMPLIST symbol plist)
        (defref SYMNAME symbol name)
        (defref SYMSPLIST symbol splist)
        (defref SYMHASH symbol hash)

        (definit INITSYMVAL symbol value)
        (definit INITSYMPVAL symbol pvalue)
        (definit INITSYMPLIST symbol plist)
        (definit INITSYMNAME symbol name)
        (definit INITSYMSPLIST symbol splist)
        (definit INITSYMHASH symbol hash)

        (defset SETSYMVAL symbol value)
        (defset SETSYMPVAL symbol pvalue)
        (defset SETSYMPLIST symbol plist)
        (defset SETSYMNAME symbol name)
        (defset SETSYMSPLIST symbol splist)
        (defset SETSYMHASH symbol hash)

        (defref VECTTYPE vector type)

        (definit INITVECTIT vector data)
        (defset SETVECTIT vector data)

        (defref FXVECTOR_TYPE fxvector type)
        (defref FXVECTIT fxvector data)

        (defref FLVECTOR_TYPE flvector type)
        (defref FLVECTIT flvector data)

        (defref BYTEVECTOR_TYPE bytevector type)
        (defref BVIT bytevector data)

        (defref STENVECTTYPE stencil-vector type)
        (definit INITSTENVECTIT stencil-vector data)

        (defref INEXACTNUM_TYPE inexactnum type)
        (defref INEXACTNUM_REAL_PART inexactnum real)
        (defref INEXACTNUM_IMAG_PART inexactnum imag)

        (defref EXACTNUM_TYPE exactnum type)
        (defref EXACTNUM_REAL_PART exactnum real)
        (defref EXACTNUM_IMAG_PART exactnum imag)
        (defref EXACTNUM_PAD exactnum pad)

        (defref RATTYPE ratnum type)
        (defref RATNUM ratnum numerator)
        (defref RATDEN ratnum denominator)
        (defref RATPAD ratnum pad)

        (defref CLOSENTRY closure code)
        (defref CLOSIT closure data)

        (defref FLODAT flonum data)

        (defref PORTTYPE port type)
        (defref PORTNAME port name)
        (defref PORTHANDLER port handler)
        (defref PORTINFO port info)
        (defref PORTOCNT port ocount)
        (defref PORTOLAST port olast)
        (defref PORTOBUF port obuffer)
        (defref PORTICNT port icount)
        (defref PORTILAST port ilast)
        (defref PORTIBUF port ibuffer)

        (defref STRTYPE string type)
        (defref STRIT string data)

        (defref BIGTYPE bignum type)
        (defref BIGIT bignum data)

        (defref CODETYPE code type)
        (defref CODELEN code length)
        (defref CODERELOC code reloc)
        (defref CODENAME code name)
        (defref CODEARITYMASK code arity-mask)
        (defref CODEFREE code closure-length)
        (defref CODEINFO code info)
        (defref CODEPINFOS code pinfo*)
        (defref CODEIT code data)

        (defref RELOCSIZE reloc-table size)
        (defref RELOCCODE reloc-table code)
        (defref RELOCIT reloc-table data)

        (defref CONTCODE continuation code)
        (defref CONTSTACK continuation stack)
        (defref CONTLENGTH continuation stack-length)
        (defref CONTCLENGTH continuation stack-clength)
        (defref CONTLINK continuation link)
        (defref CONTRET continuation return-address)
        (defref CONTWINDERS continuation winders)
        (defref CONTATTACHMENTS continuation attachments)

        (defref RTDCOUNTSTYPE rtd-counts type)
        (defref RTDCOUNTSTIMESTAMP rtd-counts timestamp)
        (defref RTDCOUNTSIT rtd-counts data)

        (defref RECORDDESCANCESTRY record-type ancestry)
        (defref RECORDDESCSIZE record-type size)
        (defref RECORDDESCPM record-type pm)
        (defref RECORDDESCMPM record-type mpm)
        (defref RECORDDESCNAME record-type name)
        (defref RECORDDESCFLDS record-type flds)
        (defref RECORDDESCFLAGS record-type flags)
        (defref RECORDDESCUID record-type uid)
        (defref RECORDDESCCOUNTS record-type counts)

        (defref RECORDINSTTYPE record type)
        (defref RECORDINSTIT record data)

       ; derived accessors
        (def "CLOSCODE(p)" "((ptr)((uptr)CLOSENTRY(p)-code_data_disp))")
        (def "CODEENTRYPOINT(x)" "((ptr)((uptr)(x)+code_data_disp))")
        (def "SETCLOSCODE(p,x)" "(CLOSENTRY(p) = CODEENTRYPOINT(x))")

        (def "SYMCODE(p)" "((ptr)((uptr)SYMPVAL(p)-code_data_disp))")
        (def "INITSYMCODE(p,x)" "(INITSYMPVAL(p) = CODEENTRYPOINT(x))")
        (def "SETSYMCODE(p,x)" "SETSYMPVAL(p,CODEENTRYPOINT(x))")

        (def "BIGLEN(x)" "((iptr)((uptr)BIGTYPE(x) >> bignum_length_offset))")
        (def "BIGSIGN(x)" "((BIGTYPE(x) & mask_bignum_sign) >> bignum_sign_offset)")
        (def "SETBIGLENANDSIGN(x,xl,xs)"
             "BIGTYPE(x) = (uptr)(xl) << bignum_length_offset | (xs) << bignum_sign_offset | type_bignum")

        (def "CLOSLEN(p)" "CODEFREE(CLOSCODE(p))")

        (defref GUARDIANOBJ guardian-entry obj)
        (defref GUARDIANREP guardian-entry rep)
        (defref GUARDIANTCONC guardian-entry tconc)
        (defref GUARDIANNEXT guardian-entry next)
        (defref GUARDIANORDERED guardian-entry ordered?)
        (defref GUARDIANPENDING guardian-entry pending)

        (definit INITGUARDIANOBJ guardian-entry obj)
        (definit INITGUARDIANREP guardian-entry rep)
        (definit INITGUARDIANTCONC guardian-entry tconc)
        (definit INITGUARDIANNEXT guardian-entry next)
        (definit INITGUARDIANORDERED guardian-entry ordered?)
        (definit INITGUARDIANPENDING guardian-entry pending)

        (defref FORWARDMARKER forward marker)
        (defref FORWARDADDRESS forward address)

        (defref CACHEDSTACKSIZE cached-stack size)
        (defref CACHEDSTACKLINK cached-stack link)

        (defref RPHEADERFRAMESIZE rp-header frame-size)
        (defref RPHEADERLIVEMASK rp-header livemask)
        (defref RPHEADERTOPLINK rp-header toplink)

        (defref RPCOMPACTHEADERMASKANDSIZE rp-compact-header mask+size+mode)
        (defref RPCOMPACTHEADERTOPLINK rp-compact-header toplink)

        (defref VFASLHEADER_DATA_SIZE vfasl-header data-size)
        (defref VFASLHEADER_TABLE_SIZE vfasl-header table-size)
        (defref VFASLHEADER_RESULT_OFFSET vfasl-header result-offset)
        (defref VFASLHEADER_VSPACE_REL_OFFSETS vfasl-header vspace-rel-offsets)
        (defref VFASLHEADER_SYMREF_COUNT vfasl-header symref-count)
        (defref VFASLHEADER_RTDREF_COUNT vfasl-header rtdref-count)
        (defref VFASLHEADER_SINGLETONREF_COUNT vfasl-header singletonref-count)

        (nl)
        (comment "machine types")
        (pr "#define machine_type_names ")
        (pr "{~{\"~a\"~^, ~}}~%"
          (let ([v (make-vector (constant machine-type-limit) 'undefined)])
            (for-each (lambda (a) (vector-set! v (car a) (cdr a))) (constant machine-type-alist))
            (vector->list v)))

        (nl)
        (comment "allocation-space names")
        (pr "#define alloc_space_names ")
        (pr "~{\"~a\"~^, ~}~%" (constant space-cname-list))

        (nl)
        (comment "allocation-space characters")
        (pr "#define alloc_space_chars ")
        (pr "~{\'~a\'~^, ~}~%" (constant space-char-list))

        (nl)
        (comment "threads")
        (when-feature pthreads
          (pr "#define scheme_feature_pthreads 1~%"))
        (defref THREADTYPE thread type)
        (defref THREADTC thread tc)

        (nl)
        (comment "thread-context data")
        (let ()
          (define-syntax alpha
            (let ()
              (define CSAFE
                (lambda (sym)
                  (string->symbol
                    (list->string
                      (map char-upcase
                           (remv #\- (string->list (symbol->string sym))))))))
              (let ([tc-field-list (sort
                                     (lambda (x y)
                                       (string<? (symbol->string x) (symbol->string y)))
                                     tc-field-list)])
                (with-syntax ([(param ...)
                               (map (lambda (x) (datum->syntax #'* x))
                                 tc-field-list)]
                              [(PARAM ...)
                               (map (lambda (x) (datum->syntax #'* x))
                                 (map CSAFE tc-field-list))])
                  (lambda (x)
                    #'(begin (defref PARAM tc param) ...))))))
          alpha)

       ; get ARGREGS for free from above; prefer ARGREG
        (defref ARGREG tc arg-regs)
        (defref VIRTREG tc virtual-registers)

        (nl)
        (comment "library entries we access from C code")
        (def "library_nonprocedure_code"
             (libspec-index (lookup-libspec nonprocedure-code)))
        (def "library_dounderflow"
             (libspec-index (lookup-libspec dounderflow)))
        (def "library_popcount_slow"
             (libspec-index (lookup-libspec popcount-slow)))
        (def "library_cpu_features"
             (libspec-index (lookup-libspec cpu-features)))

      )))
)
