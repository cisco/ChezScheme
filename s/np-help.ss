;; Helpers for "cpnanopass.ss" and "cpprim.ss", especially
;; "%"-prefixed macros that abbreviate expression constructions

(define-syntax tc-disp
  (lambda (x)
    (syntax-case x ()
      [(_ name)
       (case (datum name)
         [(%ac0) (constant tc-ac0-disp)]
         [(%ac1) (constant tc-ac1-disp)]
         [(%sfp) (constant tc-sfp-disp)]
         [(%cp) (constant tc-cp-disp)]
         [(%esp) (constant tc-esp-disp)]
         [(%ap) (constant tc-ap-disp)]
         [(%eap) (constant tc-eap-disp)]
         [(%trap) (constant tc-trap-disp)]
         [(%xp) (constant tc-xp-disp)]
         [(%yp) (constant tc-yp-disp)]
         [(%save1) (constant tc-save1-disp)]
         [else #f])])))

(define-syntax %type-check
  (lambda (x)
    (syntax-case x ()
      [(k mask type expr)
       (with-implicit (k $type-check quasiquote)
         #'($type-check (constant mask) (constant type) `expr))])))

(define-syntax %typed-object-check ; NB: caller must bind e
  (lambda (x)
    (syntax-case x ()
      [(k mask type expr)
       (with-implicit (k quasiquote %type-check %constant %mref)
         #'`(if ,(%type-check mask-typed-object type-typed-object expr)
                ,(%type-check mask type
                   ,(%mref expr ,(constant typed-object-type-disp)))
                ,(%constant sfalse)))])))

(define-syntax %seq
  (lambda (x)
    (syntax-case x ()
      [(k e1 ... e2)
       (with-implicit (k quasiquote)
         #``#,(fold-right (lambda (x body) #`(seq #,x #,body))
                #'e2 #'(e1 ...)))])))

(define-syntax %mref
  (lambda (x)
    (syntax-case x ()
      [(k e0 e1 imm type)
       (with-implicit (k quasiquote)
         #'`(mref e0 e1 imm type))]
      [(k e0 e1 imm)
       (with-implicit (k quasiquote)
         #'`(mref e0 e1 imm uptr))]
      [(k e0 imm)
       (with-implicit (k quasiquote)
         #'`(mref e0 ,%zero imm uptr))])))

(define-syntax %inline
  (lambda (x)
    (syntax-case x ()
      [(k name e ...)
       (with-implicit (k quasiquote)
         #'`(inline ,null-info ,(%primitive name) e ...))])))

(define-syntax %lea
  (lambda (x)
    (syntax-case x ()
      [(k base offset)
       (with-implicit (k quasiquote)
         #'`(inline ,(make-info-lea offset) ,%lea1 base))]
      [(k base index offset)
       (with-implicit (k quasiquote)
         #'`(inline ,(make-info-lea offset) ,%lea2 base index))])))

(define-syntax %constant
  (lambda (x)
    (syntax-case x ()
      [(k x)
       (with-implicit (k quasiquote)
         #'`(immediate ,(constant x)))])))

(define-syntax %tc-ref
  (lambda (x)
    (define-who field-type
      (lambda (struct field)
        (cond
          [(assq field (getprop struct '*fields* '())) =>
           (lambda (a)
             (apply
               (lambda (field type disp len) type)
               a))]
          [else ($oops who "undefined field ~s-~s" struct field)])))
    (syntax-case x ()
      [(k field) #'(k ,%tc field)]
      [(k e-tc field)
       (if (memq (field-type 'tc (datum field)) '(ptr xptr uptr iptr))
           (with-implicit (k %mref)
             #`(%mref e-tc
                 #,(lookup-constant
                     (string->symbol
                       (format "tc-~a-disp" (datum field))))))
           (syntax-error x "non-ptr-size tc field"))])))

(define-syntax %constant-alloc
  (lambda (x)
    (syntax-case x ()
      [(k tag size) #'(k tag size #f #f)]
      [(k tag size save-flrv?) #'(k tag size save-flrv? #f)]
      [(k tag size save-flrv? save-asm-ra?)
       (with-implicit (k quasiquote)
         #'`(alloc
              ,(make-info-alloc (constant tag) save-flrv? save-asm-ra?)
              (immediate ,(c-alloc-align size))))])))

(define-syntax %mv-jump
  (lambda (x)
    (syntax-case x ()
      [(k ret-reg (live ...))
       (with-implicit (k quasiquote %mref %inline %constant)
          #'`(if ,(%inline logtest ,(%mref ret-reg ,(constant compact-return-address-mask+size+mode-disp))
                           ,(%constant compact-header-mask))
                 ;; compact: use regular return or error?
                 (if ,(%inline logtest ,(%mref ret-reg ,(constant compact-return-address-mask+size+mode-disp))
                               ,(%constant compact-header-values-error-mask))
                     ;; values error:
                     (jump (literal ,(make-info-literal #f 'library-code
                                                        (lookup-libspec values-error)
                                                        (constant code-data-disp)))
                           (live ...))
                     ;; regular return point:
                     (jump ret-reg (live ...)))
                 ;; non-compact rp-header
                 (jump ,(%mref ret-reg ,(constant return-address-mv-return-address-disp)) (live ...))))])))    



; for use only after mdcl field has been added to the call syntax
(define-syntax %primcall
  (lambda (x)
    (syntax-case x ()
      [(k src sexpr prim arg ...)
       (identifier? #'prim)
       (with-implicit (k quasiquote)
         #``(call ,(make-info-call src sexpr #f #f #f) #f
              ,(lookup-primref 3 'prim)
              arg ...))])))

(define-syntax define-$type-check
  (lambda (x)
    (syntax-case x ()
      [(k L) (with-implicit (k $type-check)
               #'(define $type-check
                   (lambda (mask type expr)
                     (with-output-language L
                       (cond
                         [(fx= type 0) (%inline log!test ,expr (immediate ,mask))]
                         [(= mask (constant byte-constant-mask)) (%inline eq? ,expr (immediate ,type))]
                         [else (%inline type-check? ,expr (immediate ,mask) (immediate ,type))])))))])))

(include "target-fixnum.ss")

(define unfix
  (lambda (imm)
    (ash imm (fx- (constant fixnum-offset)))))

(define fix
  (lambda (imm)
    (ash imm (constant fixnum-offset))))

(define ptr->imm
  (lambda (x)
    (cond
      [(eq? x #f) (constant sfalse)]
      [(eq? x #t) (constant strue)]
      [(eq? x (void)) (constant svoid)]
      [(null? x) (constant snil)]
      [(eof-object? x) (constant seof)]
      [($unbound-object? x) (constant sunbound)]
      [(bwp-object? x) (constant sbwp)]
      [(eq? x '#1=#1#) (constant black-hole)]
      [(target-fixnum? x) (fix x)]
      [(char? x) (+ (* (constant char-factor) (char->integer x)) (constant type-char))]
      [else #f])))

(define-syntax ref-reg
  (lambda (x)
    (syntax-case x ()
      [(k reg)
       (identifier? #'reg)
       (if (real-register? (datum reg))
           #'reg
           (with-implicit (k %mref) #`(%mref ,%tc ,(tc-disp reg))))])))

(define (fp-type? type)
  (nanopass-case (Ltype Type) type
    [(fp-double-float) #t]
    [(fp-single-float) #t]
    [else #f]))
