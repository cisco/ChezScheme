;;; np-languages.ss
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

#!chezscheme
(module np-languages ()
  (export sorry! var? var-index var-index-set! prelex->uvar make-tmp make-assigned-tmp
    make-unspillable make-cpvar make-restricted-unspillable
    uvar? uvar-name uvar-type uvar-source
    uvar-referenced? uvar-referenced! uvar-assigned? uvar-assigned!
    uvar-was-closure-ref? uvar-was-closure-ref!
    uvar-unspillable? uvar-spilled? uvar-spilled! uvar-local-save? uvar-local-save!
    uvar-seen? uvar-seen! uvar-loop? uvar-loop! uvar-poison? uvar-poison!
    uvar-in-prefix? uvar-in-prefix!
    uvar-location uvar-location-set!
    uvar-move* uvar-move*-set!
    uvar-conflict*
    uvar-ref-weight uvar-ref-weight-set! uvar-save-weight uvar-save-weight-set!
    uvar-live-count uvar-live-count-set!
    uvar
    fv-offset
    var-spillable-conflict* var-spillable-conflict*-set!
    var-unspillable-conflict* var-unspillable-conflict*-set!
    uvar-degree uvar-degree-set!
    uvar-info-lambda uvar-info-lambda-set!
    uvar-iii uvar-iii-set!
    ur?
    block make-block block? block-label block-effect* block-src* block-pseudo-src block-in-link* block-flags
    block-label-set! block-effect*-set! block-src*-set! block-pseudo-src-set! block-in-link*-set! block-flags-set! 
    block-live-in block-live-in-set! block-fp-offset block-fp-offset-set!
    block-depth block-depth-set! block-loop-headers block-loop-headers-set!
    block-weight block-weight-set!
    block-index block-index-set!
    block-pariah! block-seen! block-finished! block-return-point! block-repeater! block-loop-header!
    block-pariah? block-seen? block-finished? block-return-point? block-repeater? block-loop-header?
    L1 unparse-L1 L2 unparse-L2 L3 unparse-L3 L4 unparse-L4
    L4.5 unparse-L4.5 L4.75 unparse-L4.75 L4.875 unparse-L4.875
    L5 unparse-L5 L6 unparse-L6 L7 unparse-L7
    L9 unparse-L9 L9.5 unparse-L9.5 L9.75 unparse-L9.75
    L10 unparse-L10 L10.5 unparse-L10.5 L11 unparse-L11
    L11.5 unparse-L11.5 L12 unparse-L12 L13 unparse-L13 L13.5 unparse-L13.5 L14 unparse-L14
    L15a unparse-L15a L15b unparse-L15b L15c unparse-L15c L15d unparse-L15d
    L15e unparse-L15e
    L16 unparse-L16
    info null-info
    live-info make-live-info live-info-live live-info-live-set! live-info-useless live-info-useless-set!
    primitive-pure? primitive-type primitive-handler primitive-handler-set!
    %primitive value-primitive? pred-primitive? effect-primitive?
    fv? $make-fv make-reg reg? reg-name reg-tc-disp reg-callee-save? reg-mdinfo
    reg-precolored reg-precolored-set!
    label? label-name
    libspec-label? make-libspec-label libspec-label-libspec libspec-label-live-reg*
    local-label? make-local-label
    local-label-func local-label-func-set!
    local-label-offset local-label-offset-set!
    local-label-iteration local-label-iteration-set!
    local-label-block local-label-block-set!
    local-label-overflow-check local-label-overflow-check-set!
    local-label-trap-check local-label-trap-check-set!
    direct-call-label? make-direct-call-label
    direct-call-label-referenced direct-call-label-referenced-set!
    Lsrc Lsrc? Ltype Ltype? unparse-Ltype unparse-Lsrc
    lookup-primref primref? primref-level primref-name primref-flags primref-arity
    preinfo-src preinfo-sexpr preinfo-lambda-name preinfo-lambda-flags preinfo-lambda-libspec
    prelex-name prelex-name-set!)

  (import (nanopass))
  (include "base-lang.ss")

 ; r6rs says a quote subform should be a datum, not must be a datum
 ; chez scheme allows a quote subform to be any value
  (define datum? (lambda (x) #t))

  (define-record-type var
    (fields (mutable index) (mutable spillable-conflict*) (mutable unspillable-conflict*))
    (nongenerative)
    (protocol (lambda (new) (lambda () (new #f #f #f)))))

  (define-record-type (fv $make-fv fv?)
    (parent var)
    (fields offset)
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (pargs->new)
        (lambda (offset)
          ((pargs->new) offset)))))

  (module ()
    (record-writer (record-type-descriptor fv)
      (lambda (x p wr)
        (fprintf p "fv~s" (fv-offset x)))))

  (define-record-type reg
    (parent var)
    (fields name mdinfo tc-disp callee-save? (mutable precolored))
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (pargs->new)
        (lambda (name mdinfo tc-disp callee-save?)
          ((pargs->new) name mdinfo tc-disp callee-save? #f)))))

  (module ()
    (record-writer (record-type-descriptor reg)
      (lambda (x p wr)
        (write (reg-name x) p))))

  (define-syntax define-flag-field
    (lambda (exp)
      (syntax-case exp ()
        ((_k type-name field (flag mask) ...)
         (let ()
           (define getter-name
             (lambda (f)
               (construct-name #'_k #'type-name "-" f "?")))
           (define setter-name
             (lambda (f)
               (construct-name #'_k #'type-name "-" f "!")))
           (with-syntax ([field-ref (construct-name #'_k #'type-name "-" #'field)]
                         [field-set! (construct-name #'_k #'type-name "-" #'field "-set!")]
                         [(flag-ref ...) (map getter-name #'(flag ...))]
                         [(flag-set! ...) (map setter-name #'(flag ...))]
                         [f->m (construct-name #'_k #'type-name "-" #'field "-mask")])
             #'(begin
                 (define-flags f->m (flag mask) ...)
                 (define flag-ref
                   (lambda (x)
                     (any-set? (f->m flag) (field-ref x))))
                 ...
                 (define flag-set!
                   (lambda (x bool)
                     (field-set! x
                       (let ([flags (field-ref x)])
                         (if bool
                             (set-flags (f->m flag) flags)
                             (reset-flags (f->m flag) flags))))))
                 ...)))))))

  (define-flag-field uvar flags
    (referenced         #b00000000001)
    (assigned           #b00000000010)
    (unspillable        #b00000000100)
    (spilled            #b00000001000)
    (seen               #b00000010000)
    (was-closure-ref    #b00000100000)
    (loop               #b00001000000)
    (in-prefix          #b00010000000)
    (local-save         #b00100000000)
    (poison             #b01000000000)
  )

  (define-record-type (uvar $make-uvar uvar?)
    (parent var)
    (fields 
      name
      source
      type
      conflict*
      (mutable flags)
      (mutable info-lambda)
      (mutable location)
      (mutable move*)
      (mutable degree)
      (mutable iii)           ; inspector info index
      (mutable ref-weight)    ; must be a fixnum!
      (mutable save-weight)   ; must be a fixnum!
      (mutable live-count)    ; must be a fixnum!
     )
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (pargs->new)
        (lambda (name source type conflict* flags)
          ((pargs->new) name source type conflict* flags #f #f '() #f #f 0 0 0)))))
  (define prelex->uvar
    (lambda (x)
      ($make-uvar (prelex-name x) (prelex-source x) 'ptr '()
        (if (prelex-referenced x)
            (if (prelex-assigned x)
                (uvar-flags-mask referenced assigned)
                (uvar-flags-mask referenced))
            (if (prelex-assigned x)
                (uvar-flags-mask assigned)
                (uvar-flags-mask))))))
  (define make-tmp
    (case-lambda
      [(name) (make-tmp name 'ptr)]
      [(name type) ($make-uvar name #f type '() (uvar-flags-mask referenced))]))
  (define make-assigned-tmp
    (case-lambda
      [(name) (make-assigned-tmp name 'ptr)]
      [(name type) ($make-uvar name #f type '() (uvar-flags-mask referenced assigned))]))
  (define make-unspillable
    (lambda (name)
      ($make-uvar name #f 'ptr '() (uvar-flags-mask referenced unspillable))))
  (define make-cpvar
    (lambda ()
      (include "types.ss")
      ;; NB: cpsymbol is not a source object. Why is it put into the uvar-source field?
      ($make-uvar 'cp cpsymbol 'ptr '() (uvar-flags-mask referenced))))
  (define make-restricted-unspillable
    (lambda (name conflict*)
      ($make-uvar name #f 'uptr conflict* (uvar-flags-mask referenced assigned unspillable))))

  (module ()
    (record-writer (record-type-descriptor uvar)
      (lambda (x p wr)
        (write (lookup-unique-uvar x) p))))

  (define lookup-unique-uvar
    (let ([ht (make-eq-hashtable)])
      (lambda (x)
        (or (eq-hashtable-ref ht x #f)
            (let ([sym (gensym (symbol->string (uvar-name x)))])
              (eq-hashtable-set! ht x sym)
              sym)))))

  (define-record-type info (nongenerative))

  (define null-info (make-info))

  (module ()
    (record-writer (record-type-descriptor info)
      (lambda (x p wr)
        (fprintf p "#<info>"))))

  (define-record-type label
    (nongenerative)
    (fields name))

  (define-record-type libspec-label
    (parent label)
    (nongenerative)
    (sealed #t)
    (fields libspec live-reg*)
    (protocol
      (lambda (pargs->new)
        (lambda (name libspec live-reg*)
          ((pargs->new name) libspec live-reg*)))))

  ; TODO: need better abstraction for reusing record fields for
  ; different purposes in different passes.
  (define-record-type local-label
    (parent label)
    (nongenerative)
    (fields (mutable func) (mutable offset) (mutable iteration) (mutable block)
      ; following used by place-overflow-and-trap-check pass
      (mutable overflow-check) (mutable trap-check))
    (protocol
      (lambda (pargs->new)
        (lambda (name)
          ((pargs->new name) #f #f #f #f 'no 'no)))))

  (define-record-type direct-call-label
    (parent local-label)
    (nongenerative)
    (sealed #t)
    (fields (mutable referenced))
    (protocol
      (lambda (pargs->new)
        (lambda (name)
          ((pargs->new name) #f)))))

  (module ()
    (define lookup-unique-label
      (let ([ht (make-eq-hashtable)])
        (lambda (x)
          (or (eq-hashtable-ref ht x #f)
              (let ([sym (gensym (symbol->string (label-name x)))])
                (eq-hashtable-set! ht x sym)
                sym)))))
    (record-writer (record-type-descriptor local-label)
      (lambda (x p wr)
        (write (lookup-unique-label x) p)))
    (record-writer (record-type-descriptor libspec-label)
      (lambda (x p wr)
        (write (label-name x) p))))

  (define maybe-var?
    (lambda (x)
      (or (eq? x #f) (var? x))))

  (define maybe-label?
    (lambda (x)
      (or (eq? x #f) (label? x))))

 ; language to replace prelex with uvar, create info records out of some of the complex
 ; records, and make sure other record types have been discarded.  also formally sets up
 ; CaseLambdaClause as entry point for language.
  (define-language L1
    (terminals
      (uvar (x))
      (datum (d))
      (source-object (src))
      (info (info))
      (fixnum (interface))
      (primref (pr))
    )
    (entry CaseLambdaExpr)
    (Expr (e body)
      le
      x
      pr
      (quote d)
      (call info e0 e1 ...)                                 => (e0 e1 ...)
      (if e0 e1 e2)
      (seq e0 e1)
      (set! x e)
      (letrec ([x le] ...) body)
      (moi)                                                 => "moi"
      (foreign info e)
      (fcallable info e)
      (profile src)                                         => (profile)
      (pariah)
    )
    (CaseLambdaExpr (le)
      (case-lambda info cl ...)                             => (case-lambda cl ...)
    )
    (CaseLambdaClause (cl)
      (clause (x* ...) interface body)
    ))

 ; from this point on, if a uvar x is bound to a lambda expression le by letrec,
 ; (uvar-info-lambda x) must be equal to le's info-lambda

 ; introducing let
  (define-language L2 (extends L1)
    (entry CaseLambdaExpr)
    (Expr (e body)
      (+ (let ([x e] ...) body))))

 ; removes moi; also adds name to info-lambda & info-foreign
  (define-language L3 (extends L2)
    (entry CaseLambdaExpr)
    (Expr (e body)
      (- (moi))))

 ; removes assignable indefinite-extent variables from the language
  (define-language L4 (extends L3)
    (entry CaseLambdaExpr)
    (Expr (e body)
      (- (set! x e))))

 ; introducing mvlet, and mvcall
  (define-language L4.5 (extends L4)
    (terminals
      (+ (maybe-label (mdcl))))
    (entry CaseLambdaExpr)
    (Expr (e body)
      (- (call info e0 e1 ...))
      (+ (call info mdcl e0 e1 ...)                       => (call mdcl e0 e1 ...)
         (mvcall info e1 e2)                              => (mvcall e1 e2)
         (mvlet e ((x** ...) interface* body*) ...))))

 ; removes foreign, adds foreign-call, updates fcallable
  (define-language L4.75 (extends L4.5)
    (entry CaseLambdaExpr)
    (terminals
      (+ (label (l))))
    (Expr (e body)
      (- (foreign info e)
         (fcallable info e))
      (+ (label l body)
         (foreign-call info e e* ...)
         (fcallable info))))

 ; adds loop form
  (define-language L4.875 (extends L4.75)
    (entry CaseLambdaExpr)
    (Expr (e body)
      (+ (loop x (x* ...) body) => (loop x body))))

 ; moves all case lambda expressions into rhs of letrec
  (define-language L5 (extends L4.875)
    (entry CaseLambdaExpr)
    (Expr (e body)
      (- le)))

 ; replaces letrec with labels and closures forms
  (define-language L6 (extends L5)
    (terminals
      (+ (maybe-var (mcp))))
    (entry CaseLambdaExpr)
    (Expr (e body)
      (- (letrec ([x le] ...) body))
      (+ (closures ([x* (x** ...) le*] ...) body)))
    (CaseLambdaClause (cl)
      (- (clause (x* ...) interface body))
      (+ (clause (x* ...) mcp interface body))))

 ; move labels to top level and expands closures forms to more primitive operations
  (define-language L7 (extends L6)
    (terminals
      (- (uvar (x))
         (fixnum (interface)))
      (+ (var (x))
         (primitive (prim)) ; moved up one language to support closure instrumentation
         (fixnum (interface offset))
         (immediate (imm))))
    (entry Program)
    (Program (prog)
      (+ (labels ([l* le*] ...) l)                     => (labels ([l* le*] ...) (l))))
    (CaseLambdaExpr (le)
      (+ (fcallable info l)                            => (fcallable info l)))
    (Lvalue (lvalue)
      (+ x
         (mref e1 e2 imm)))
    (Expr (e body)
      (- x
         (fcallable info)
         (closures ([x* (x** ...) le*] ...) body)
         (call info mdcl e0 e1 ...))
      (+ lvalue
         (alloc info e)                                => (alloc info e)
         (literal info)                                => info
         (label-ref l offset)
         (immediate imm)                               => imm
         ; moved up one language to support closure instrumentation
         (inline info prim e* ...)                     => (inline info prim e* ...)
         (call info mdcl (maybe e0) e1 ...)            => (call mdcl e0 e1 ...)
         (set! lvalue e)
         ; these two forms are added here so expand-inline handlers can expand into them
         (values info e* ...)
         (goto l))))

  (define-record-type primitive
    (fields name type pure? (mutable handler))
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (new)
        (lambda (name type pure?)
          (new name type pure? (lambda args (sorry! name "no primitive handler defined")))))))

  (module ()
    (record-writer (record-type-descriptor primitive)
      (lambda (x p wr)
        (fprintf p "~s" (primitive-name x)))))

  (define value-primitive?
    (lambda (x)
      (and (primitive? x)
           (eq? (primitive-type x) 'value))))

  (define pred-primitive?
    (lambda (x)
      (and (primitive? x)
           (eq? (primitive-type x) 'pred))))

  (define effect-primitive?
    (lambda (x)
      (and (primitive? x)
           (eq? (primitive-type x) 'effect))))

  (define-syntax declare-primitive
    (lambda (x)
      (syntax-case x ()
        [(_ name type pure?)
         (with-syntax ([%name (construct-name #'name "%" #'name)])
           #'(begin
               (define %name (make-primitive 'name 'type pure?))
               (export %name)))])))

  (define-syntax %primitive
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (let ([a (syntax->annotation #'name)]
               [sym (string->symbol (format "%~a" (datum name)))])
           (datum->syntax #'name
             (if a (make-annotation sym (annotation-source a) sym) sym)))])))

  (declare-primitive asmlibcall! effect #f)
  (declare-primitive c-call effect #f)
  (declare-primitive c-simple-call effect #f)
  (declare-primitive c-simple-return effect #f)
  (declare-primitive deactivate-thread effect #f) ; threaded version only
  (declare-primitive fl* effect #f)
  (declare-primitive fl+ effect #f)
  (declare-primitive fl- effect #f)
  (declare-primitive fl/ effect #f)
  (declare-primitive fldl effect #f) ; x86
  (declare-primitive flds effect #f) ; x86
  (declare-primitive flsqrt effect #f) ; not implemented for some ppc32 (so we don't use it)
  (declare-primitive flt effect #f)
  (declare-primitive inc-cc-counter effect #f)
  (declare-primitive inc-profile-counter effect #f)
  (declare-primitive invoke-prelude effect #f)
  (declare-primitive keep-live effect #f)
  (declare-primitive load-double effect #f)
  (declare-primitive load-double->single effect #f)
  (declare-primitive load-single effect #f)
  (declare-primitive load-single->double effect #f)
  (declare-primitive locked-decr! effect #f)
  (declare-primitive locked-incr! effect #f)
  (declare-primitive pause effect #f)
  (declare-primitive push effect #f)
  (declare-primitive pop-multiple effect #f) ; arm
  (declare-primitive push-multiple effect #f) ; arm
  (declare-primitive remember effect #f)
  (declare-primitive restore-flrv effect #f)
  (declare-primitive restore-lr effect #f) ; ppc
  (declare-primitive save-flrv effect #f)
  (declare-primitive save-lr effect #f) ; ppc
  (declare-primitive store effect #f)
  (declare-primitive store-double effect #f)
  (declare-primitive store-single effect #f)
  (declare-primitive store-single->double effect #f)
  (declare-primitive store-with-update effect #f) ; ppc
  (declare-primitive unactivate-thread effect #f) ; threaded version only
  (declare-primitive vpush-multiple effect #f) ; arm
  (declare-primitive vpop-multiple effect #f) ; arm
  (declare-primitive cas effect #f)

  (declare-primitive < pred #t)
  (declare-primitive <= pred #t)
  (declare-primitive > pred #t)
  (declare-primitive >= pred #t)
  (declare-primitive condition-code pred #t)
  (declare-primitive eq? pred #t)
  (declare-primitive fl< pred #t)
  (declare-primitive fl<= pred #t)
  (declare-primitive fl= pred #t)
  (declare-primitive lock! pred #f)
  (declare-primitive logtest pred #t)
  (declare-primitive log!test pred #t)
  (declare-primitive type-check? pred #t)
  (declare-primitive u< pred #t)

  (declare-primitive - value #t)
  (declare-primitive / value #t)
  (declare-primitive + value #t)
  (declare-primitive +/ovfl value #f)
  (declare-primitive +/carry value #f)
  (declare-primitive -/ovfl value #f)
  (declare-primitive -/eq value #f)
  (declare-primitive asmlibcall value #f)
  (declare-primitive fstpl value #f) ; x86 only
  (declare-primitive fstps value #f) ; x86 only
  (declare-primitive get-double value #t) ; x86_64
  (declare-primitive get-tc value #f) ; threaded version only
  (declare-primitive activate-thread value #f) ; threaded version only
  (declare-primitive lea1 value #t)
  (declare-primitive lea2 value #t)
  (declare-primitive load value #t)
  (declare-primitive logand value #t)
  (declare-primitive logor value #t)
  (declare-primitive logxor value #t)
  (declare-primitive lognot value #t)
  (declare-primitive move value #t)
  (declare-primitive * value #t)
  (declare-primitive */ovfl value #f)
  (declare-primitive pop value #f)
  (declare-primitive read-performance-monitoring-counter value #t) ; on x86/x86_64 actually side-effects edx/rdx
  (declare-primitive read-time-stamp-counter value #t) ; on x86/x86_64 actually side-effects edx/rdx
  (declare-primitive sext8 value #t)
  (declare-primitive sext16 value #t)
  (declare-primitive sext32 value #t) ; 64-bit only
  (declare-primitive sll value #t)
  (declare-primitive srl value #t)
  (declare-primitive sra value #t)
  (declare-primitive trunc value #t)
  (declare-primitive zext8 value #t)
  (declare-primitive zext16 value #t)
  (declare-primitive zext32 value #t) ; 64-bit only

  (define immediate?
    (let ([low (- (bitwise-arithmetic-shift-left 1 (fx- (constant ptr-bits) 1)))]
          [high (- (bitwise-arithmetic-shift-left 1 (constant ptr-bits)) 1)])
      (if (and (eqv? (constant most-negative-fixnum) (most-negative-fixnum))
               (eqv? (constant most-positive-fixnum) (most-positive-fixnum)))
          (lambda (x) (or (fixnum? x) (and (bignum? x) (<= low x high))))
          (lambda (x) (and (or (fixnum? x) (bignum? x)) (<= low x high))))))

  (define imm->ptr
    (lambda (x)
      (cond
        [(= x (constant sfalse)) #f]
        [(= x (constant strue)) #t]
        [(= x (constant svoid)) (void)]
        [(= x (constant snil)) '()]
        [(= x (constant seof)) #!eof]
        [(= x (constant sunbound)) ($unbound-object)]
        [(= x (constant sbwp)) #!bwp]
        [(= (logand x (constant mask-fixnum)) (constant type-fixnum))
         (ash (- x (constant type-fixnum)) (- (constant fixnum-offset)))]
        [(= (logand x (constant mask-char)) (constant type-char))
         (integer->char (/ (- x (constant type-char)) (constant char-factor)))]
        [else ($oops 'cpnanopass-internal "imm->ptr got unrecognized immediate: ~s" x)])))

 ; specifies the representation for simple scheme constants: #t, #f, (void),
 ; '(), (eof-object), ($unbound-object), #!bwp, characters, and fixnums as
 ; scheme-object ptrs and inlines primitive calls
  (define-language L9 (extends L7)
    (entry Program)
    (terminals
      (- (datum (d))
         (primref (pr)))
      (+ (symbol (sym))))
    (CaseLambdaExpr (le)
      (+ (hand-coded sym)))
    (Expr (e body)
      (- (quote d)
         pr)))

 ; determine where we should be placing interrupt and overflow
  (define-language L9.5 (extends L9)
    (entry Program)
    (terminals
      (+ (boolean (ioc))))
    (Expr (e body)
      (+ (trap-check ioc e)
         (overflow-check e))))

 ; remove the loop form
  (define-language L9.75 (extends L9.5)
    (entry Program)
    (Expr (e body)
      (- (loop x (x* ...) body))))

 ; bindings are replaced with combination of a locals form and a series of set!
 ; expressions; value is broken into three categories: Triv, Rhs, and Expr.  Triv
 ; expressions can appear as arguments to call and inline, or in any Rhs or Tail
 ; location, and are considered simple enough for the instruction selector to handle.
 ; Rhs expressions can appear on the right-hand-side of a set! or anywhere arbitrary
 ; Exprs can appear.  Exprs appear in the body of a case-lambda clause.
  (define-language L10 (extends L9.75)
    (terminals
      (+ (uvar (local))))
    (entry Program)
    (CaseLambdaClause (cl)
      (- (clause (x* ...) mcp interface body))
      (+ (clause (x* ...) (local* ...) mcp interface body)))
    (Lvalue (lvalue)
      (- (mref e1 e2 imm))
      (+ (mref x1 x2 imm)))
    (Triv (t)
      (+ lvalue
         (literal info)                          => info
         (immediate imm)                         => (quote imm)
         (label-ref l offset)))
    (Rhs (rhs)
      (+ t
         (call info mdcl (maybe t0) t1 ...)      => (call mdcl t0 t1 ...)
         (alloc info t)                          => (alloc info t)
         (inline info prim t* ...)               => (inline info prim t* ...)
         (mvcall info e t)                       => (mvcall e t)
         (foreign-call info t t* ...)))
    (Expr (e body)
      (- lvalue
         (values info e* ...)
         (literal info)
         (immediate imm)
         (label-ref l offset)
         (call info mdcl (maybe e0) e1 ...)
         (inline info prim e* ...)
         (alloc info e)
         (let ([x e] ...) body)
         (set! lvalue e)
         (mvcall info e1 e2)
         (foreign-call info e e* ...))
      (+ rhs
         (values info t* ...)
         (set! lvalue rhs))))

  (define-language L10.5 (extends L10)
    (entry Program)
    (Rhs (rhs)
      (- (call info mdcl (maybe t0) t1 ...)
         (mvcall info e t))
      (+ (mvcall info mdcl (maybe t0) t1 ... (t* ...)) => (mvcall mdcl t0 t1 ... (t* ...))))
    (Expr (e body)
      (- (mvlet e ((x** ...) interface* body*) ...))
      (+ (mvset info (mdcl (maybe t0) t1 ...) (t* ...) ((x** ...) interface* l*) ...) =>
            (mvset (mdcl t0 t1 ...) (t* ...) ((x** ...) interface* l*) ...)
         (mlabel e (l* e*) ...))))

 ; expressions are normalized into Tail, Pred, or Effect context; primrefs
 ; are converted into inline expressions; make-closure,
 ; closure-ref, and closure-set! are converted into inline calls; numbers and
 ; labels used as arguments to make-closure, closure-ref, and closure-set! are
 ; marked as literals so they will not be turned into scheme constants again.
  (define-language L11 (extends L10.5)
    (terminals
      (- (primitive (prim)))
      (+ (value-primitive (value-prim))
         (pred-primitive (pred-prim))
         (effect-primitive (effect-prim))))
    (entry Program)
    (CaseLambdaClause (cl)
      (- (clause (x* ...) (local* ...) mcp interface body))
      (+ (clause (x* ...) (local* ...) mcp interface tlbody)))
    (Rhs (rhs)
      (- (inline info prim t* ...))
      (+ (inline info value-prim t* ...)               => (inline info value-prim t* ...)))
    (Expr (e body)
      (- rhs
         (label l body)
         (set! lvalue rhs)
         (if e0 e1 e2)
         (seq e0 e1)
         (mvset info (mdcl (maybe t0) t1 ...) (t* ...) ((x** ...) interface* l*) ...)
         (values info t* ...)
         (goto l)
         (mlabel e (l* e*) ...)
         (pariah)
         (trap-check ioc e)
         (overflow-check e)
         (profile src)))
    (Tail (tl tlbody)
      (+ rhs
         (if p0 tl1 tl2)
         (seq e0 tl1)
         (values info t* ...)                          => (values t* ...)
         (goto l)))
    (Pred (p pbody)
      (+ (true)                                        => #t
         (false)                                       => #f
         (inline info pred-prim t* ...)                => (inline info pred-prim t* ...)
         (if p0 p1 p2)
         (seq e0 p1)
         (goto l)
         (mlabel p (l* p*) ...)))
    (Effect (e ebody)
      (+ (nop)
         (label l)
         (goto l)
         (pariah)
         (trap-check ioc)
         (overflow-check)
         (profile src)                                 => (profile)
         (set! lvalue rhs)
         (inline info effect-prim t* ...)              => (inline info effect-prim t* ...)
         (if p0 e1 e2)
         (seq e0 e1)
         (mvset info (mdcl (maybe t0) t1 ...) (t* ...) ((x** ...) interface* l*) ...) =>
            (mvset (mdcl t0 t1 ...) (t* ...) ((x** ...) interface* l*) ...)
         (mvcall info mdcl (maybe t0) t1 ... (t* ...)) => (mvcall mdcl t0 t1 ... (t* ...))
         (foreign-call info t t* ...)
         (tail tl))))

  (define-language L11.5 (extends L11)
    (entry Program)
    (terminals
      (- (boolean (ioc))))
    (Effect (e body)
      (- (trap-check ioc))))

  (define-language L12 (extends L11.5)
    (terminals
      (- (fixnum (interface offset))
         (label (l)))
      (+ (fixnum (fixed-args offset))
         (label (l dcl))))
    (entry Program)
    (CaseLambdaExpr (le)
      (- (case-lambda info cl ...))
      (+ (lambda info (local* ...) tlbody)                => (lambda (local* ...) tlbody)))
    (CaseLambdaClause (cl)
      (- (clause (x* ...) (local* ...) mcp interface tlbody)))
    (Tail (tl tlbody)
      (+ (entry-point (x* ...) dcl mcp tlbody)))
    (Effect (e ebody)
      (- (mvset info (mdcl (maybe t0) t1 ...) (t* ...) ((x** ...) interface* l*) ...))
      (+ (do-rest fixed-args)
         (mvset info (mdcl (maybe t0) t1 ...) (t* ...) ((x** ...) ...) ebody)
         ; mventry-point and mverror-point can appear only within an mvset ebody
         ; ideally, grammar would reflect this
         (mventry-point (x* ...) l)
         (mverror-point))))

  (define exact-integer?
    (lambda (x)
      (and (integer? x) (exact? x))))

 ; calling conventions are imposed; clauses no longer have formals (they are
 ; now locals set by arguments from argument registers and frame); calls no
 ; longer have arguments; case-lambda is resposible for dispatching to correct
 ; clause, even when the game is being played
  (define-language L13
    (terminals
      (fixnum (max-fv offset))
      (fv (fv))
      (reg (reg))
      (var (x nfv cnfv var))
      (uvar (local))
      (effect-primitive (effect-prim))
      (pred-primitive (pred-prim))
      (value-primitive (value-prim))
      (immediate (imm fs))
      (exact-integer (lpm))
      (info (info))
      (maybe-label (mrvl))
      (label (l rpl))
      (source-object (src))
      (symbol (sym)))
    (Program (prog)
      (labels ([l* le*] ...) l)                   => (letrec ([l* le*] ...) (l)))
    (CaseLambdaExpr (le)
      (lambda info max-fv (local* ...) tlbody)    => (lambda (local* ...) tlbody)
      (hand-coded sym))
    (Lvalue (lvalue)
      x
      (mref x1 x2 imm))
    (Triv (t)
      lvalue
      (literal info)                              => info
      (immediate imm)                             => imm
      (label-ref l offset))
    (Rhs (rhs)
      t
      (alloc info t)                              => (alloc info t)
      (inline info value-prim t* ...)             => (inline info value-prim t* ...))
    (Pred (p pbody)
      (inline info pred-prim t* ...)              => (inline info pred-prim t* ...)
      (true)
      (false)
      (if p0 p1 p2)
      (seq e0 p1)
      (goto l)
      (mlabel p (l* p*) ...))
    (Effect (e ebody)
      (overflow-check)
      (overflood-check)
      (fcallable-overflow-check)
      (new-frame info rpl* ... rpl)
      (return-point info rpl mrvl (cnfv* ...))
      (rp-header mrvl fs lpm)
      (remove-frame info)
      (restore-local-saves info)
      (shift-arg reg imm info)
      (set! lvalue rhs)
      (inline info effect-prim t* ...)            => (inline info effect-prim t* ...)
      (nop)
      (pariah)
      (if p0 e1 e2)
      (seq e0 e1)
      (label l)
      (goto l)
      (tail tl)
      (profile src)                               => (profile)
      (check-live reg* ...))
    (Tail (tl tlbody)
      (jump t (var* ...))
      (joto l (nfv* ...))
      (asm-return reg* ...)
      (asm-c-return info reg* ...)
      (if p0 tl1 tl2)
      (seq e0 tl1)
      (goto l)))

  (define-language L13.5 (extends L13)
    (terminals
      (- (symbol (sym))))
    (entry Program)
    (CaseLambdaExpr (le)
      (- (hand-coded sym))))

  (define-language L14 (extends L13.5)
    (entry Program)
    (Rhs (rhs)
      (- (alloc info t))))

  (define-record-type block
    (fields
      (mutable label)
      (mutable effect*)
      (mutable src*)
      (mutable pseudo-src)
      (mutable in-link*)
      (mutable flags)
      (mutable fp-offset)
      (mutable live-in)
      (mutable depth)
      (mutable loop-headers)
      (mutable index)
      (mutable weight))
    (nongenerative)
    (protocol
      (lambda (new)
        (lambda ()
          (new #f '() '() #f '() (block-flags-mask) #f 'uninitialized 0 #f #f #f)))))

  (define-flag-field block flags
    (pariah         #b000001)
    (seen           #b000010)
    (finished       #b000100)
    (return-point   #b001000)
    (repeater       #b010000)
    (loop-header    #b100000))

  (define-record-type live-info
    (nongenerative)
    (sealed #t)
    (fields 
      (mutable live)
      (mutable useless))
    (protocol
      (lambda (new)
        (case-lambda
          [() (new 'uninitialized #f)]
          [(live) (new live #f)]))))

  (module ()
    (record-writer (record-type-descriptor live-info)
      (lambda (x p wr)
        (if (eq? (live-info-live x) 'uninitialized)
            (display-string "#<live-info>" p)
            (fprintf p "#<live-info ~s>" (live-info-live x))))))

  (define-language L15a
    (terminals
      (var (x cnfv var))
      (reg (reg))
      (uvar (local))
      (effect-primitive (effect-prim))
      (pred-primitive (pred-prim))
      (value-primitive (value-prim))
      (immediate (imm fs))
      (exact-integer (lpm))
      (live-info (live-info))
      (info (info))
      (label (l rpl))
      (maybe-label (mrvl))
      (fixnum (max-fv offset))
      (block (block entry-block)))
    (Program (pgm)
      (labels ([l* le*] ...) l)                  => (letrec ([l* le*] ...) (l)))
    (CaseLambdaExpr (le)
      (lambda info max-fv (local* ...) (entry-block* ...) (block* ...)) => (lambda (local* ...) (entry-block* ...) (block* ...)))
    (Dummy (dumdum) (dummy))
    (Lvalue (lvalue)
      x
      (mref x1 x2 imm))
    (Triv (t)
      lvalue
      (literal info)                            => info
      (immediate imm)                           => imm
      (label-ref l offset))
    (Rhs (rhs)
      t
      (inline info value-prim t* ...))
    (Pred (p)
      (inline live-info info pred-prim t* ...))
    (Effect (e)
      (overflow-check live-info)
      (overflood-check live-info)
      (fcallable-overflow-check live-info)
      (return-point info rpl mrvl (cnfv* ...))
      (rp-header mrvl fs lpm)
      (remove-frame live-info info)
      (restore-local-saves live-info info)
      (shift-arg live-info reg imm info)
      (set! live-info lvalue rhs)
      (inline live-info info effect-prim t* ...)
      (check-live live-info reg* ...))
    (Tail (tl)
      (goto l)
      (jump live-info t (var* ...))
      (asm-return reg* ...)
      (asm-c-return info reg* ...)))

  (define-language L15b (extends L15a)
    (terminals
      (- (var (x cnfv var))
         (reg (reg))
         (label (l rpl)))
      (+ (var (x var))
         (label (l))))
    (Effect (e)
      (- (remove-frame live-info info)
         (restore-local-saves live-info info)
         (return-point info rpl mrvl (cnfv* ...))
         (shift-arg live-info reg imm info)
         (check-live live-info reg* ...))
      (+ (fp-offset live-info imm)))
    (Tail (tl)
      (- (jump live-info t (var* ...))
         (asm-return reg* ...)
         (asm-c-return info reg* ...))
      (+ (jump live-info t)
         (asm-return)
         (asm-c-return info))))

  (define ur?
    (lambda (x)
      (or (reg? x) (uvar? x))))

  (define-language L15c (extends L15b)
    (terminals
      (- (var (x var)))
      (+ (ur (x))))
    ; NB: base and index are really either regs or (mref %sfp %zero imm)
    (Lvalue (lvalue)
      (- (mref x1 x2 imm))
      (+ (mref lvalue1 lvalue2 imm)))
    (Effect (e)
      (- (fp-offset live-info imm))))

  (define-language L15d (extends L15c)
    (terminals
      (- (pred-primitive (pred-prim))
         (value-primitive (value-prim))
         (effect-primitive (effect-prim)))
      (+ (procedure (proc)) => $procedure-name))
    (entry Program)
    (Lvalue (lvalue)
      (- (mref lvalue1 lvalue2 imm))
      (+ (mref x1 x2 imm)))
    (Rhs (rhs)
      (- (inline info value-prim t* ...))
      (+ (asm info proc t* ...) => (asm proc t* ...)))
    (Effect (e)
      (- (inline live-info info effect-prim t* ...)
         (overflow-check live-info)
         (overflood-check live-info)
         (fcallable-overflow-check live-info))
      (+ (asm info proc t* ...) => (asm proc t* ...)
         (move-related x1 x2)
         (overflow-check p e* ...)))
    (Pred (p pbody)
      (- (inline live-info info pred-prim t* ...))
      (+ (asm info proc t* ...) => (asm proc t* ...)))
    (Tail (tl)
      (- (jump live-info t))
      (+ (jump t))))

  (define-language L15e (extends L15d)
    (terminals
      (- (ur (x)))
      (+ (reg (x))))
    (entry Program)
    (CaseLambdaExpr (le)
      (- (lambda info max-fv (local* ...) (entry-block* ...) (block* ...)))
      (+ (lambda info (entry-block* ...) (block* ...)) => (lambda (entry-block* ...) (block* ...))))
    (Effect (e)
      (- (set! live-info lvalue rhs)
         (move-related x1 x2))
      (+ (set! lvalue rhs))))

  (define-language L16 (extends L15e)
    (entry Program)
    (Effect (e)
      (- (overflow-check p e* ...))))

  (meta-cond
    [(not (eqv? (optimize-level) 3))
     (pretty-format 'define-language
       '(alt
          (_ var #f ('terminals #f x ...) #f (_ _ #f ...) ...)
          (_ var ('extends x) #f ('definitions #f x ...) #f ('terminals #f x ...) #f (_ _ #f ...) ...)
          (_ var #f ('definitions #f x ...) #f ('terminals #f x ...) #f (_ _ #f ...) ...)
          (_ var ('extends x) #f ('terminals #f x ...))
          (_ var ('extends x) #f ('terminals #f x ...) #f (_ _ #f ...) ...)
          (_ var ('extends x) #f (_ _ #f ...) ...)))
     (pretty-format 'labels '(_ ([bracket x e] 0 ...) #f e ...))
     (pretty-format 'blocks '(_ #f [bracket (x ...) 0 e] ...))])

  (primitive-handler-set! %keep-live
    (lambda (info x)
      (with-output-language (L15d Effect)
        `(asm ,info ,(lambda (code*) code*)))))
)
