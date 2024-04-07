;;; pb.ss

;; The pb (portable bytecode) interpreter is implemented by "pb.c".
;; The intent is that the machine uses 64-bit Scheme object
;; representations and a runtime-determined endianness, so code
;; compiled as portable bytecode can run on any machine (as long as
;; the C compiler supports 64-bit integers for the kernel's
;; implementation, where care is taken for the conversion between C
;; pointers and Scheme object addresses). That way, a single set of pb
;; boot files can be used to bootstrap the compiler for any supporrted
;; platform.

;; The pb machine can be configured (through ".def") for 32-bit Scheme
;; object representations and a specific endianness.

;; In all configurations, the pb machine uses 32-bit instructions. The
;; fasl format of instructuctions is always little-endian, and the
;; machine-code content is swapped on load for a big-endian
;; environment.

;; The pb binstruction set is load--store and vaguely similar to Arm.
;; One difference is that there's a single flag for branching:
;; signalling arithemtic, bitwise, and comparison operations set the
;; flag for a specific condition, such as "overflow" or "equal", and
;; the branch variants are "branch if true" or "branch if false".
;; The intent is that a test is always immediately followed by a
;; branch, and a branch is always immediately preceded by a test.

;; Each 32-bit instruction has one of these formats, shown with low
;; bit on the left (like byte order for a little-endian machine):
;;
;;     low byte                        high byte
;;        8          8          8          8 
;;  -----------------------------------------------
;;  |    op    |    reg    |     immed/reg        |
;;  -----------------------------------------------
;;  -----------------------------------------------
;;  |    op    | reg | reg |     immed/reg        |
;;  -----------------------------------------------
;;  -----------------------------------------------
;;  |    op    | reg |          immed             |
;;  -----------------------------------------------
;;  -----------------------------------------------
;;  |    op    |             immed                |
;;  -----------------------------------------------
;;
;; Integer and floating-point registers (up to 16 of each) are
;; different, and an `op` determines which bank is meant for a `reg`
;; reference. The low-bits `reg` in the byte after the `op` tends to
;; be the destination register. The long `immed` form is mainly for
;; branches. See "cmacros.ss" for the `op` constructions.

;; Foreign-procedure calls always supported for specific prototypes,
;; which are generally the ones for functions implemented the Chez
;; Scheme kernel. Supported prototypes are specified in "cmacros.ss".
;; Foreign callables are not always supported. All foreign-call
;; arguments and results are passed in registers for the
;; always-supported set of protypoes.

;; Foreign-call procedures and callables may be supported for other
;; prototypes (e.g., depending on whether libffi is available). Those
;; calls pass arguments and receive results in a special "arena"
;; space, analogous to page-sized call stack.

;; The `pb-literal` instruction could be considered variable-width, in
;; that its is followed by the literal data to load into a register.
;; Or `pb-literal` could be considered a combination load and jump to
;; skip over the data.

;;; SECTION 1: registers

(define-registers
  (reserved
    [%tc                        #t  0 uptr]
    [%sfp                       #t  1 uptr]
    [%ap                        #t  2 uptr]
    [%trap                      #t  3 uptr])
  (allocable
    [%ac0                       #f  4 uptr]
    [%xp                        #f  5 uptr]
    [%ts                        #f  6 uptr]
    [%td                        #f  7 uptr]
    [%cp                        #f  8 uptr]
    [%r9  %Carg1 %Cretval       #f  9 uptr]
    [%r10 %Carg2                #f 10 uptr]
    [%r11 %Carg3                #f 11 uptr]
    [%r12 %Carg4                #f 12 uptr]
    [%r13 %Carg5                #f 13 uptr]
    [%r14 %Carg6                #f 14 uptr]
    [%r15 %Carg7                #f 15 uptr]
    [%fp1                       #f  0 fp]
    [%fp2 %Cfparg1 %Cfpretval   #f  1 fp]
    [%fp3 %Cfparg2              #f  2 fp]
    [%fp4 %Cfparg3              #f  3 fp]
    [%fp5 %Cfparg4              #f  4 fp]
    [%fp6 %Cfparg5              #f  5 fp]
    [%fp7 %Cfparg6              #f  6 fp]
    [%fp8                       #f  7 fp])
  (machine-dependent))

;;; SECTION 2: instructions
(module (md-handle-jump ; also sets primitive handlers
         mem->mem
         fpmem->fpmem
         coercible?
         coerce-opnd)
  (import asm-module)

  (define imm-signed16?
    (lambda (x)<
      (nanopass-case (L15c Triv) x
        [(immediate ,imm) (signed16? imm)]
        [else #f])))

  (define mref->mref
    (lambda (a k)
      (define return
        (lambda (x0 x1 imm type)
          ;; load & store instructions support index or offset, but not both
          (safe-assert (or (eq? x1 %zero) (eqv? imm 0)))
          (k (with-output-language (L15d Triv) `(mref ,x0 ,x1 ,imm ,type)))))
      (nanopass-case (L15c Triv) a
        [(mref ,lvalue0 ,lvalue1 ,imm ,type)
         (lvalue->ur lvalue0
           (lambda (x0)
             (lvalue->ur lvalue1
               (lambda (x1)
                 (cond
                   [(and (eq? x1 %zero) (signed16? imm))
                    (return x0 %zero imm type)]
                   [(and (not (eq? x1 %zero)) (signed16? imm))
                    (if (eqv? imm 0)
                        (return x0 x1 0 type)
                        (let ([u (make-tmp 'u)])
                          (seq
                           (build-set! ,u (asm ,null-info ,(asm-add #f) ,x1 (immediate ,imm)))
                           (return x0 u 0 type))))]
                   [else
                    (let ([u (make-tmp 'u)])
                      (seq
                        (build-set! ,u (immediate ,imm))
                        (if (eq? x1 %zero)
                            (return x0 u 0 type)
                            (seq
                              (build-set! ,u (asm ,null-info ,(asm-add #f) ,u ,x1))
                              (return x0 u 0 type)))))])))))])))

  (define mem->mem
    (lambda (a k)
      (cond
        [(literal@? a)
         (let ([u (make-tmp 'u)])
           (seq
             (build-set! ,u ,(literal@->literal a))
             (k (with-output-language (L15d Lvalue) `(mref ,u ,%zero 0 uptr)))))]
        [else (mref->mref a k)])))

  (define fpmem->fpmem mem->mem)

  ;; `define-instruction` code takes care of `ur` and `fpur`, to which
  ;; all type-compatible values must convert
  (define-syntax coercible?
    (syntax-rules ()
      [(_ ?a ?aty*)
       (let ([a ?a] [aty* ?aty*])
         (or (and (memq 'signed16 aty*) (imm-signed16? a))
             (and (memq 'mem aty*) (mem? a))
             (and (memq 'fpmem aty*) (fpmem? a))))]))

  ;; `define-instruction` doesn't try to cover `ur` and `fpur`
  (define-syntax coerce-opnd ; passes k something compatible with aty*
    (syntax-rules ()
      [(_ ?a ?aty* ?k)
       (let ([a ?a] [aty* ?aty*] [k ?k])
         (cond
           [(and (memq 'mem aty*) (mem? a)) (mem->mem a k)]
           [(and (memq 'fpmem aty*) (fpmem? a)) (fpmem->fpmem a k)]
           [(and (memq 'signed16 aty*) (imm-signed16? a)) (k (imm->imm a))]
           [(or (memq 'ur aty*)
                (memq 'fpur aty*))
            (cond
              [(ur? a) (k a)]
              [(imm? a)
               (let ([u (make-tmp 'u)])
                 (seq
                   (build-set! ,u ,(imm->imm a))
                   (k u)))]
              [(or (mem? a) (fpmem? a))
               (let ([type (if (fpmem? a) 'fp 'uptr)])
                 (mem->mem a
                   (lambda (a)
                     (let ([u (make-tmp 'u type)])
                       (seq
                        (build-set! ,u ,a)
                        (k u))))))]
              [else (sorry! 'coerce-opnd "unexpected operand ~s" a)])]
           [else (sorry! 'coerce-opnd "cannot coerce ~s to ~s" a aty*)]))]))

  (define md-handle-jump
    (lambda (t)
      (with-output-language (L15d Tail)
        (define long-form
          (lambda (e)
            (let ([tmp (make-tmp 'utmp)])
              (values
                (in-context Effect `(set! ,(make-live-info) ,tmp ,e))
                `(jump ,tmp)))))
        (nanopass-case (L15c Triv) t
          [,lvalue
           (if (mem? lvalue)
               (mem->mem lvalue (lambda (e) (values '() `(jump ,e))))
               (values '() `(jump ,lvalue)))]
          [(literal ,info)
           (guard (and (not (info-literal-indirect? info))
                       (memq (info-literal-type info) '(entry library-code))))
           (values '() `(jump (literal ,info)))]
          [(label-ref ,l ,offset)
           (values '() `(jump (label-ref ,l ,offset)))]
          [else (long-form t)]))))

  (define info-cc-eq (make-info-condition-code 'eq? #f #t))
  (define asm-eq (asm-relop info-cc-eq))

  ; x is not the same as z in any clause that follows a clause where (x z)
  ; and y is coercible to one of its types, however:
  ; WARNING: do not assume that if x isn't the same as z then x is independent
  ; of z, since x might be an mref with z as it's base or index

  (define-instruction value (- -/ovfl -/eq -/pos)
    [(op (z ur) (x ur) (y signed16))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub op) ,x ,y))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-sub op) ,x ,y))])

  (define-instruction value (+ +/ovfl +/carry)
    [(op (z ur) (x ur) (y signed16))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (memq op '(+/ovfl +/carry))) ,x ,y))]
    [(op (z ur) (x signed16) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (memq op '(+/ovfl +/carry))) ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-add (memq op '(+/ovfl +/carry))) ,x ,y))])

  (define-instruction value (* */ovfl)
    [(op (z ur) (x ur) (y signed16))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-mul (memq op '(*/ovfl))) ,x ,y))]
    [(op (z ur) (x signed16) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-mul (memq op '(*/ovfl))) ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-mul (memq op '(*/ovfl))) ,x ,y))])

  (define-instruction value (/)
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-div ,x ,y))])

  (define-instruction value (logand logor logxor)
    [(op (z ur) (x ur) (y signed16))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logical op) ,x ,y))]
    [(op (z ur) (x signed16) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logical op) ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logical op) ,x ,y))])

  (define-instruction value (lognot)
    [(op (z ur) (x ur))
     `(set! ,(make-live-info) ,z (asm ,info ,asm-lognot ,x))])

  (define-instruction value (sll srl sra slol)
    [(op (z ur) (x ur) (y signed16 ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logical op) ,x ,y))])

  (define-instruction value (move)
    [(op (z mem) (x ur))
     `(set! ,(make-live-info) ,z ,x)]
    [(op (z ur) (x ur mem signed16))
     `(set! ,(make-live-info) ,z ,x)])

  (let ()
    (define build-lea1
      (lambda (info z x)
        (let ([offset (info-lea-offset info)])
          (with-output-language (L15d Effect)
            (cond
              [(signed16? offset)
               `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,x (immediate ,offset)))]
              [else
               (let ([u (make-tmp 'u)])
                 (seq
                  `(set! ,(make-live-info) ,u (immediate ,offset))
                  `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,x ,u))))])))))
          
    (define-instruction value lea1
      [(op (z ur) (x ur)) (build-lea1 info z x)])

    (define-instruction value lea2
      [(op (z ur) (x ur) (y ur))
       (let ([u (make-tmp 'u)])
         (seq
          (build-lea1 info u x)
          `(set! ,(make-live-info) ,z (asm ,info ,(asm-add #f) ,y ,u))))]))

  (let ()
    (define imm-zero (with-output-language (L15d Triv) `(immediate 0)))
    (define load/store
      (lambda (x y w k) ; x ur, y ur, w ur or imm
        (with-output-language (L15d Effect)
          (if (ur? w)
              (if (eq? y %zero)
                  (k x w)
                  (let ([u (make-tmp 'u)])
                    (seq
                      `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,y ,w))
                      (k x u))))
              (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
                (cond
                  [(and (eq? y %zero) (signed16? n))
                   (let ([w (in-context Triv `(immediate ,n))])
                     (k x w))]
                  [(eqv? n 0)
                   (k x y)]
                  [(signed16? n)
                   (let ([u (make-tmp 'u)])
                     (seq
                      `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x (immediate ,n)))
                      (k u y)))]
                  [else 
                   (let ([u (make-tmp 'u)])
                     (seq
                       `(set! ,(make-live-info) ,u (immediate ,n))
                       (if (eq? y %zero)
                           (k x u)
                           (seq
                            `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,u))
                            (k u y)))))]))))))
    (define-instruction value (load)
      [(op (z ur) (x ur) (y ur) (w ur signed16))
       (let ([type (info-load-type info)])
         (load/store x y w
           (lambda (x y)
             (let ([instr `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-load type) ,x ,y))])
               (if (info-load-swapped? info)
                   (seq
                     instr
                     `(set! ,(make-live-info) ,z (asm ,null-info ,(asm-swap type) ,z)))
                   instr)))))])
    (define-instruction effect (store)
      [(op (x ur) (y ur) (w ur signed16) (z ur))
       (let ([type (info-load-type info)])
         (load/store x y w
           (lambda (x y)
             (if (info-load-swapped? info)
                 (let ([u (make-tmp 'unique-bob)])
                   (seq
                     `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-swap type) ,z))
                     `(asm ,null-info ,(asm-store type) ,x ,y ,u)))
                 `(asm ,null-info ,(asm-store type) ,x ,y ,z)))))]))

  (define-instruction value (load-single->double)
    [(op (x fpur) (y fpmem)) `(set! ,(make-live-info) ,x (asm ,null-info ,asm-fpmove-single ,y))])

  (define-instruction effect (store-double->single)
    [(op (x fpmem) (y fpur)) `(asm ,info ,asm-fpmove-single ,x ,y)])

  (define-instruction value (single->double double->single)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fl-cvt op) ,y))])

  (define-instruction value (fpt)
    [(op (x fpur) (y ur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpt ,y))])

  (define-instruction value (fptrunc)
    [(op (x ur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fptrunc ,y))])

  (define-instruction value (fpsingle)
    [(op (x fpur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsingle ,y))])

  (define-instruction value (fpmove)
    [(op (x fpmem) (y fpur)) `(set! ,(make-live-info) ,x ,y)]
    [(op (x fpur) (y fpmem fpur)) `(set! ,(make-live-info) ,x ,y)])

  (constant-case ptr-bits
    [(64)
     (let ()
       (define (mem->mem mem new-type)
         (nanopass-case (L15d Triv) mem
           [(mref ,x0 ,x1 ,imm ,type)
            (with-output-language (L15d Lvalue) `(mref ,x0 ,x1 ,imm ,new-type))]))

       (define-instruction value (fpcastto)
         [(op (x mem) (y fpur)) `(set! ,(make-live-info) ,(mem->mem x 'fp) ,y)]
         [(op (x ur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcastto ,y))])
       
       (define-instruction value (fpcastfrom)
         [(op (x fpmem) (y ur)) `(set! ,(make-live-info) ,(mem->mem x 'uptr) ,y)]
         [(op (x fpur) (y ur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcastfrom ,y))]))]
    [(32)
     (let ()
       (define (mem->mem mem delta)
         (nanopass-case (L15d Triv) mem
           [(mref ,x0 ,x1 ,imm ,type)
            (let ([delta (constant-case native-endianness
                           [(little) (if (eq? delta 'lo) 0 4)]
                           [(big) (if (eq? delta 'hi) 0 4)])])
              (with-output-language (L15d Lvalue) `(mref ,x0 ,x1 ,(fx+ imm delta) uptr)))]))

       (define-instruction value (fpcastto/hi)
         [(op (x ur) (y fpmem)) `(set! ,(make-live-info) ,x ,(mem->mem y 'hi))]
         [(op (x ur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpcastto 'hi) ,y))])
    
       (define-instruction value (fpcastto/lo)
         [(op (x ur) (y fpmem)) `(set! ,(make-live-info) ,x ,(mem->mem y 'lo))]
         [(op (x ur) (y fpur)) `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpcastto 'lo) ,y))])
    
       (define-instruction value (fpcastfrom)
         [(op (x fpmem) (hi ur) (lo ur)) (seq
                                          `(set! ,(make-live-info) ,(mem->mem x 'lo) ,lo)
                                          `(set! ,(make-live-info) ,(mem->mem x 'hi) ,hi))]
         [(op (x fpur) (hi ur) (lo ur)) `(set! ,(make-live-info) ,x (asm ,info ,asm-fpcastfrom ,lo ,hi))]))])

  (define-instruction value (fp+ fp- fp/ fp*)
    [(op (x fpur) (y fpur) (z fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-fpop-2 op) ,y ,z))])

  (define-instruction value (fpsqrt)
    [(op (x fpur) (y fpur))
     `(set! ,(make-live-info) ,x (asm ,info ,asm-fpsqrt ,y))])

  (define-instruction pred (fp= fp< fp<=)
    [(op (x fpur) (y fpur))
     (let ([info (make-info-condition-code op #f #f)])
       (values '() `(asm ,info ,(asm-fp-relop info) ,x ,y)))])

  (define-instruction effect (inc-cc-counter)
    [(op (x ur) (w signed16) (z ur signed16))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,w))
        `(asm ,info ,asm-inc! ,u ,z)))])

  (define-instruction effect (inc-profile-counter)
    [(op (x mem) (y signed16))
     (nanopass-case (L15d Triv) x
       [(mref ,x0 ,x1 ,imm ,type)
        (let ([u (make-tmp 'u)])
          (seq
           `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x0 ,(if (eq? x1 %zero)
                                                                              `(immediate ,imm)
                                                                              x1)))
           `(asm ,info ,asm-inc! ,u ,y)))])])

  (define-instruction value (read-time-stamp-counter)
    [(op (z ur)) `(set! ,(make-live-info) ,z (immediate 0))])

  (define-instruction value (read-performance-monitoring-counter)
    [(op (z ur) (x ur)) `(set! ,(make-live-info) ,z (immediate 0))])

  (define-instruction value (asmlibcall)
    [(op (z ur))
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(set! ,(make-live-info) ,z (asm ,info ,(asm-library-call (info-asmlib-libspec info)) ,u ,(info-kill*-live*-live* info) ...))))])

  (define-instruction effect (asmlibcall!)
    [(op)
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(asm ,info ,(asm-library-call! (info-asmlib-libspec info)) ,u ,(info-kill*-live*-live* info) ...)))])

  (define-instruction effect (c-simple-call)
    [(op)
     (let ([u (make-tmp 'u)])
       (seq
        `(set! ,(make-live-info) ,u (asm ,null-info ,asm-kill))
        `(asm ,info ,(asm-c-simple-call (info-c-simple-call-entry info)) ,u)))])

  (define-instruction pred (eq? u< < > <= >= logtest log!test)
    [(op (y signed16) (x ur))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #t #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))]
    [(op (x ur) (y ur signed16))
     (let ([info (if (eq? op 'eq?) info-cc-eq (make-info-condition-code op #f #t))])
       (values '() `(asm ,info ,(asm-relop info) ,x ,y)))])

  (define-instruction pred (condition-code)
    [(op) (values '() `(asm ,info ,(asm-condition-code info)))])

  (define-instruction pred (type-check?)
    [(op (x ur) (mask signed16 ur) (type signed16 ur))
     (let ([tmp (make-tmp 'u)])
       (values
         (with-output-language (L15d Effect)
           `(set! ,(make-live-info) ,tmp (asm ,null-info ,(asm-logical 'logand) ,x ,mask)))
         `(asm ,info-cc-eq ,asm-eq ,tmp ,type)))])

  (define-instruction effect (call-arena-in)
    [(op (x ur) (off signed16)) `(asm ,info ,(asm-call-arena (constant pb-call-arena-in)) ,x ,off)])
  (define-instruction effect (fp-call-arena-in)
    [(op (x fpur) (off signed16)) `(asm ,info ,(asm-call-arena (constant pb-fp-call-arena-in)) ,x ,off)])

  (define-instruction value (call-arena-out)
    [(op (x ur) (off signed16))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-call-arena (constant pb-call-arena-out)) ,off))])
  (define-instruction value (fp-call-arena-out)
    [(op (x fpur) (off signed16))
     `(set! ,(make-live-info) ,x (asm ,info ,(asm-call-arena (constant pb-fp-call-arena-out)) ,off))])

  (let ()
    (define (addr-reg x y w k)
      (with-output-language (L15d Effect)
        (let ([n (nanopass-case (L15d Triv) w [(immediate ,imm) imm])])
          (cond
            [(and (eq? y %zero) (fx= n 0))
             (k x)]
            [else
             (let ([u (make-tmp 'u)])
               (cond
                 [(eq? y %zero)
                  (seq
                   `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,w))
                   (k u))]
                 [(fx= n 0)
                  (seq
                   `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,y))
                   (k u))]
                 [else
                  (seq
                   `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,x ,y))
                   `(set! ,(make-live-info) ,u (asm ,null-info ,(asm-add #f) ,u ,w))
                   (k u))]))]))))

    (define-instruction pred (lock!)
      [(op (x ur) (y ur) (w signed16))
       (addr-reg x y w (lambda (u)
                         (values '() `(asm ,info-cc-eq ,(asm-lock! info-cc-eq) ,u))))])

    (define-instruction effect (locked-incr!)
      [(op (x ur) (y ur) (w signed16))
       (addr-reg x y w (lambda (u)
                         ;; signals on zero after increment
                         `(asm ,info ,asm-inc! ,u (immediate 1))))])
    (define-instruction effect (locked-decr!)
      [(op (x ur) (y ur) (w signed16))
       (addr-reg x y w (lambda (u)
                         ;; signals on zero after decrement
                         `(asm ,info ,asm-inc! ,u (immediate -1))))])

    (define-instruction effect (cas)
      [(op (x ur) (y ur) (w signed16) (old ur) (new ur))
       (addr-reg x y w (lambda (u)
                         ;; signals on successful swap
                         `(asm ,info ,asm-cas! ,u ,old ,new)))]))

  (define-instruction effect (store-store-fence)
    [(op)
     `(asm ,info ,(asm-fence (constant pb-fence-store-store)))])
    
  (define-instruction effect (acquire-fence)
    [(op)
     `(asm ,info ,(asm-fence (constant pb-fence-acquire)))])

  (define-instruction effect (release-fence)
    [(op)
     `(asm ,info ,(asm-fence (constant pb-fence-release)))])

  (define-instruction effect (pause)
    ;; NB: use sqrt or something like that?
    [(op) '()])

  (define-instruction effect (c-call)
    [(op (x ur) (y signed16)) `(asm ,info ,asm-indirect-call ,x ,y ,(info-kill*-live*-live* info) ...)])

  (define-instruction effect (c-stack-call)
    [(op (x ur) (y ur)) `(asm ,info ,asm-stack-call ,x ,y)])

  (define-instruction effect save-flrv
    [(op) '()])

  (define-instruction effect restore-flrv
    [(op) '()])

  (define-instruction effect (invoke-prelude)
    [(op) '()])
)

;;; SECTION 3: assembler
(module asm-module (; required exports
                     asm-move asm-load asm-store asm-swap asm-library-call asm-library-call! asm-library-jump
                     asm-mul asm-div asm-add asm-sub asm-logical asm-lognot
                     asm-fp-relop asm-relop
                     asm-indirect-jump asm-literal-jump
                     asm-direct-jump asm-return-address asm-jump asm-conditional-jump
                     asm-indirect-call asm-condition-code
                     asm-fpmove-single asm-fl-cvt asm-fpt asm-fpmove asm-fpcastto asm-fpcastfrom
                     asm-fptrunc asm-fpsingle
                     asm-call-arena asm-stack-call
                     asm-inc! asm-lock! asm-cas! asm-fence
                     asm-fpop-2 asm-fpsqrt asm-c-simple-call
                     asm-return asm-c-return asm-size asm-nop
                     asm-enter asm-foreign-call asm-foreign-callable
                     asm-kill
                     signed16?)

  (define ax-register?
    (case-lambda
      [(x) (record-case x [(reg) r #t] [else #f])]
      [(x reg) (record-case x [(reg) r (eq? r reg)] [else #f])]))

  (define-who ax-ea-reg-code
    (lambda (ea)
      (record-case ea
        [(reg) r (reg-mdinfo r)]
        [else (sorry! who "ea=~s" ea)])))

  (define ax-reg?
    (lambda (ea)
      (record-case ea
        [(reg) ignore #t]
        [else #f])))

  (define ax-imm?
    (lambda (ea)
      (record-case ea
        [(imm) ignore #t]
        [else #f])))

  (define-who ax-imm-data
    (lambda (ea)
      (record-case ea
        [(imm) (n) n]
        [else (sorry! who "ax-imm-data ea=~s" ea)])))

  ; define-op sets up assembly op macros--
  ; the opcode and all other expressions are passed to the specified handler--
  (define-syntax define-op
    (lambda (x)
      (syntax-case x ()
        [(k op handler e ...)
         (with-syntax ([op (construct-name #'k "asmop-" #'op)])
           #'(define-syntax op
               (syntax-rules ()
                 [(_ mneu arg (... ...))
                  (handler 'mneu e ... arg (... ...))])))])))

  (define-syntax emit
    (lambda (x)
      (syntax-case x ()
        [(k op x ...)
         (with-syntax ([emit-op (construct-name #'k "asmop-" #'op)])
           #'(emit-op op x ...))])))

  (define-op nop     nop-op)
  (define-op literal literal-op)

  (define-op mov   mov-op (constant pb-i->i))
  (define-op fpmov mov-op (constant pb-d->d))

  (define-op movzi movi-op #f) ; 16-bit immediate, shifted
  (define-op movki movi-op #t) ; 16-bit immediate, shifted

  (define-op add   signal-bin-op (constant pb-add))
  (define-op sub   signal-bin-op (constant pb-sub))
  (define-op mul   signal-bin-op (constant pb-mul))
  (define-op div   bin-op (constant pb-div))

  (define-op subz  signal-bin-op (constant pb-subz)) ; signals on 0 instead of overflow
  (define-op subp  signal-bin-op (constant pb-subp)) ; signals on positive

  (define-op land  bin-op (constant pb-and))
  (define-op lior  bin-op (constant pb-ior))
  (define-op lxor  bin-op (constant pb-xor))
  (define-op lnot  un-op  (constant pb-not))

  (define-op lsl   bin-op (constant pb-lsl))
  (define-op lsr   bin-op (constant pb-lsr))
  (define-op asr   bin-op (constant pb-asr))
  (define-op lslo  bin-op (constant pb-lslo))

  (define-op rev   rev-op)

  (define-op eq    cmp-op (constant pb-eq))
  (define-op lt    cmp-op (constant pb-lt))
  (define-op gt    cmp-op (constant pb-gt))
  (define-op le    cmp-op (constant pb-le))
  (define-op ge    cmp-op (constant pb-ge))
  (define-op ab    cmp-op (constant pb-ab)) ; above: unsigned compare
  (define-op bl    cmp-op (constant pb-bl)) ; below: unsigned compare
  (define-op cs    cmp-op (constant pb-cs)) ; bits in common
  (define-op cc    cmp-op (constant pb-cc)) ; no bits in common

  (define-op ld    load-op)
  (define-op st    store-op)

  (define-op fadd  fp-bin-op (constant pb-add))
  (define-op fsub  fp-bin-op (constant pb-sub))
  (define-op fmul  fp-bin-op (constant pb-mul))
  (define-op fdiv  fp-bin-op (constant pb-div))
  
  (define-op fpeq  fp-cmp-op (constant pb-eq))
  (define-op fplt  fp-cmp-op (constant pb-lt))
  (define-op fple  fp-cmp-op (constant pb-le))

  (define-op fsqrt fp-un-op   (constant pb-sqrt))

  (define-op mov.s->d mov-op (constant pb-s->d))
  (define-op mov.d->s mov-op (constant pb-d->s))
  (define-op mov.i->d mov-op (constant pb-i->d))
  (define-op mov.d->i mov-op (constant pb-d->i))

  (define-op mov.d->s->d mov-op (constant pb-d->s->d))

  ;; 64-bit versions
  (define-op mov.i*>d mov-op (constant pb-i-bits->d-bits))
  (define-op mov.d*>i mov-op (constant pb-d-bits->i-bits))

  ;; 32-bit versions
  (define-op mov.ii*>d mov2-op (constant pb-i-i-bits->d-bits))
  (define-op mov.d*l>i mov-op (constant pb-d-lo-bits->i-bits))
  (define-op mov.d*h>i mov-op (constant pb-d-hi-bits->i-bits))

  (define-op btrue branch-op (constant pb-true))
  (define-op bfals branch-op (constant pb-fals))
  (define-op b     branch-op (constant pb-always))
  (define-op b*    branch-indirect-op)

  (define-op lock  lock-op)
  (define-op cas   cas-op)
  (define-op inc   inc-op)

  (define-op fence fence-op)

  (define-op call-arena call-arena-op)
  (define-op stack-call stack-call-op)

  (define-op call   call-op)
  (define-op interp interp-op)
  (define-op ret    ret-op)
  (define-op adr    adr-op) ; use only for an address after an rpheader (or compact)

  (define nop-op
    (lambda (op code*)
      (emit-code (op code*)
        (constant pb-nop))))
  
  (define literal-op
    (lambda (op dest code*)
      (emit-code (op dest code*)
        (constant pb-literal)
        (ax-ea-reg-code dest))))

  (define movi-op
    (lambda (op keep? dest imm shift code*)
      (emit-code (op dest imm shift code*)
        (fx+ (constant pb-mov16)
             (if keep?
                 (constant pb-keep-bits)
                 (constant pb-zero-bits))
             shift)
        (ax-ea-reg-code dest)
        imm)))

  (define mov-op
    (lambda (op mode dest src code*)
      (emit-code (op dest src code*)
        (fx+ (constant pb-mov)
             mode)
        (ax-ea-reg-code dest)
        (ax-ea-reg-code src))))

  (define mov2-op
    (lambda (op mode dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        (fx+ (constant pb-mov)
             mode)
        (ax-ea-reg-code dest)
        (ax-ea-reg-code src0)
        (ax-ea-reg-code src1))))

  (define signal-bin-op
    (lambda (op opcode set-cc? dest src0 src1 code*)
      (cond
        [(ax-reg? src1)
         (emit-code (op set-cc? dest src0 src1 code*)
           (fx+ (constant pb-bin-op)
                (if set-cc?
                    (constant pb-signal)
                    (constant pb-no-signal))
                opcode
                (constant pb-register))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src0)
           (ax-ea-reg-code src1))]
        [else
         (emit-code (op set-cc? dest src0 src1 code*)
           (fx+ (constant pb-bin-op)
                (if set-cc?
                    (constant pb-signal)
                    (constant pb-no-signal))
                opcode
                (constant pb-immediate))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src0)
           (ax-imm-data src1))])))

  (define bin-op
    (lambda (op opcode dest src0 src1 code*)
      (cond
        [(ax-reg? src1)
         (emit-code (op dest src0 src1 code*)
           (fx+ (constant pb-bin-op)
                (constant pb-no-signal)
                opcode
                (constant pb-register))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src0)
           (ax-ea-reg-code src1))]
        [else
         (emit-code (op dest src0 src1 code*)
           (fx+ (constant pb-bin-op)
                (constant pb-no-signal)
                opcode
                (constant pb-immediate))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src0)
           (ax-imm-data src1))])))

  (define un-op
    (lambda (op opcode dest src code*)
      (cond
        [(ax-reg? src)
         (emit-code (op dest src code*)
           (fx+ (constant pb-un-op)
                opcode
                (constant pb-register))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src))]
        [else
         (emit-code (op dest src code*)
           (fx+ (constant pb-un-op)
                opcode
                (constant pb-immediate))
           (ax-ea-reg-code dest)
           (ax-imm-data src))])))

  (define rev-op
    (lambda (op size dest src code*)
      (emit-code (op dest src code*)
        (fx+ (constant pb-rev-op)
             size
             (constant pb-register))
        (ax-ea-reg-code dest)
        (ax-ea-reg-code src))))

  (define cmp-op
    (lambda (op opcode src0 src1 code*)
      (cond
        [(ax-reg? src1)
         (emit-code (op src0 src1 code*)
           (fx+ (constant pb-cmp-op)
                opcode
                (constant pb-register))
           (ax-ea-reg-code src0)
           (ax-ea-reg-code src1))]
        [else
         (emit-code (op src0 src1 code*)
           (fx+ (constant pb-cmp-op)
                opcode
                (constant pb-immediate))
           (ax-ea-reg-code src0)
           (ax-imm-data src1))])))

  (define load-op
    (lambda (op size dest src0 src1 code*)
      (cond
        [(ax-reg? src1)
         (emit-code (op size dest src0 src1 code*)
           (fx+ (constant pb-ld-op)
                size
                (constant pb-register))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src0)
           (ax-ea-reg-code src1))]
        [else
         (emit-code (op size dest src0 src1 code*)
           (fx+ (constant pb-ld-op)
                size
                (constant pb-immediate))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src0)
           (ax-imm-data src1))])))

  (define store-op
    (lambda (op size dest0 dest1 src code*)
      (cond
        [(ax-reg? dest1)
         (emit-code (op size dest0 dest1 src code*)
           (fx+ (constant pb-st-op)
                size
                (constant pb-register))
           (ax-ea-reg-code src)
           (ax-ea-reg-code dest0)
           (ax-ea-reg-code dest1))]
        [else
         (emit-code (op size dest0 dest1 src code*)
           (fx+ (constant pb-st-op)
                size
                (constant pb-immediate))
           (ax-ea-reg-code src)
           (ax-ea-reg-code dest0)
           (ax-imm-data dest1))])))

  (define fp-bin-op
    (lambda (op opcode dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        (fx+ (constant pb-fp-bin-op)
             opcode
             (constant pb-register))
        (ax-ea-reg-code dest)
        (ax-ea-reg-code src0)
        (ax-ea-reg-code src1))))

  (define fp-un-op
    (lambda (op opcode dest src code*)
      (emit-code (op dest src code*)
        (fx+ (constant pb-fp-un-op)
             opcode
             (constant pb-register))
        (ax-ea-reg-code dest)
        (ax-ea-reg-code src))))

  (define fp-cmp-op
    (lambda (op opcode src0 src1 code*)
      (emit-code (op src0 src1 code*)
        (fx+ (constant pb-fp-cmp-op)
             opcode
             (constant pb-register))
        (ax-ea-reg-code src0)
        (ax-ea-reg-code src1))))

  (define-who branch-op
    (lambda (op sel addr code*)
      (record-case addr
        [(reg) r
         (emit-code (op sel addr code*)
           (fx+ (constant pb-b-op)
                sel
                (constant pb-register))
           0
           (reg-mdinfo r))]
        [(imm) (n)
         (emit-code (op sel addr code*)
           (fx+ (constant pb-b-op)
                sel
                (constant pb-immediate))
           n)]
        [(label) (offset l)
         (emit-code (op sel addr code*)
           (fx+ (constant pb-b-op)
                sel
                (constant pb-immediate))
           offset)]
        [else
         (sorry! who "unrecognized destination ~s" addr)])))

  (define branch-indirect-op
    (lambda (op src0 src1 code*)
      (cond
        [(ax-reg? src1)
         (emit-code (op src0 src1 code*)
           (fx+ (constant pb-b*-op)
                (constant pb-register))
           (ax-ea-reg-code src0)
           (ax-ea-reg-code src1))]
        [else
         (emit-code (op src0 src1 code*)
           (fx+ (constant pb-b*-op)
                (constant pb-immediate))
           (ax-ea-reg-code src0)
           (ax-imm-data src1))])))

  (define ret-op
    (lambda (op code*)
      (emit-code (op code*)
        (constant pb-return)
        0
        0)))

  (define call-op
    (lambda (op dest proto code*)
      (emit-code (op dest code*)
        (constant pb-call)
        (ax-ea-reg-code dest)
        (ax-imm-data proto))))

  (define interp-op
    (lambda (op dest code*)
      (emit-code (op dest code*)
        (constant pb-interp)
        (ax-ea-reg-code dest)
        0)))

  (define adr-op
    (lambda (op dest offset code*)
      (emit-code (op dest offset code*)
        (constant pb-adr)
        (bitwise-ior (ax-ea-reg-code dest)
                     ;; offset is in instructions:
                     (bitwise-arithmetic-shift offset (fx- 4 2))))))

  (define call-arena-op
    (lambda (op opcode reg delta code*)
      (emit-code (op reg delta code*)
        opcode
        (ax-ea-reg-code reg)
        (ax-imm-data delta))))

  (define stack-call-op
    (lambda (op dest proto code*)
      (emit-code (op dest proto code*)
        (constant pb-stack-call)
        (ax-ea-reg-code dest)
        (ax-ea-reg-code proto))))

  (define inc-op
    (lambda (op dest src code*)
      (cond
        [(ax-reg? src)
         (emit-code (op dest src code*)
           (fx+ (constant pb-inc)
                (constant pb-register))
           (ax-ea-reg-code dest)
           (ax-ea-reg-code src))]
        [else
         (emit-code (op dest src code*)
           (fx+ (constant pb-inc)
                (constant pb-immediate))
           (ax-ea-reg-code dest)
           (ax-imm-data src))])))

  (define lock-op
    (lambda (op dest code*)
      (emit-code (op dest code*)
        (constant pb-lock)
        (ax-ea-reg-code dest)
        0)))

  (define cas-op
    (lambda (op dest src0 src1 code*)
      (emit-code (op dest src0 src1 code*)
        (constant pb-cas)
        (ax-ea-reg-code dest)
        (ax-ea-reg-code src0)
        (ax-ea-reg-code src1))))

  (define fence-op
    (lambda (op kind code*)
      (emit-code (op kind code*)
        (fx+ (constant pb-fence)
             kind))))

  (define-syntax emit-code
    (lambda (x)
      (syntax-case x ()
        [(_ (op opnd ... ?code*) chunk ...)
         (let ([safe-check (lambda (e)
                             (if (fx= (debug-level) 0)
                                 e
                                 #`(let ([code #,e])
                                     (unless (<= 0 code (sub1 (expt 2 32)))
                                       (sorry! 'emit-code "bad result ~s for ~s"
                                               code
                                               (list op opnd ...)))
                                     code)))])
           #`(cons (build long #,(safe-check #`(byte-fields chunk ...)))
                   (aop-cons* `(asm ,op ,opnd ...) ?code*)))])))

  (define-syntax build
    (syntax-rules ()
      [(_ x e)
       (and (memq (datum x) '(byte word long)) (integer? (datum e)))
       (begin
         (safe-assert (fixnum? (datum e)))
         (quote (x . e)))]
      [(_ x e)
       (memq (datum x) '(byte word long))
       (cons 'x e)]))

  (define-syntax byte-fields
    (syntax-rules ()
      [(byte-fields op d r/i)
       (+ op
          (bitwise-arithmetic-shift-left d 8)
          (bitwise-arithmetic-shift-left (fxand r/i #xFFFF) 16))]
      [(byte-fields op d r r/i)
       (+ op
          (bitwise-arithmetic-shift-left d 8)
          (bitwise-arithmetic-shift-left r 12)
          (bitwise-arithmetic-shift-left (fxand r/i #xFFFF) 16))]
      [(byte-fields op i)
       (+ op
          (bitwise-arithmetic-shift-left (fxand i #xFFFFFF) 8))]
      [(byte-fields op)
       op]))

  (define signed16?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 15)) imm (fx- (expt 2 15) 1)))))
  
  (define signed24?
    (lambda (imm)
      (and (fixnum? imm) (fx<= (fx- (expt 2 23)) imm (fx- (expt 2 23) 1)))))
  
  (define asm-size
    (lambda (x)
      (case (car x)
        [(asm pb-abs pb-proc) 0]
        [(long) 4]
        [else (constant-case ptr-bits
                [(64) 8]
                [(32) 4])])))

  (define ax-reloc
    (lambda (dest code*)
      (emit literal dest
        (emit nop
          (constant-case ptr-bits
            [(64) (emit nop code*)]
            [else code*])))))

  (define ax-movi
    (lambda (dest n code*) 
      (let loop ([n n] [shift 0] [init? #t])
        (cond
          [(or (eqv? n 0) (fx= shift 4))
           (if init?
               ;; make sure 0 is installed
               (emit movzi dest 0 0 code*)
               code*)]
          [else
           (let ([m (logand n #xFFFF)])
             (cond
               [(eqv? m 0)
                (loop (bitwise-arithmetic-shift-right n 16) (fx+ shift 1) init?)]
               [else
                (let ([code* (loop (bitwise-arithmetic-shift-right n 16) (fx+ shift 1) #f)])
                  (if init?
                      (emit movzi dest m shift code*)
                      (emit movki dest m shift code*)))]))]))))

  (define-who asm-move
    (lambda (code* dest src)
      ;; move pseudo instruction used by set! case in select-instruction
      ;; guarantees dest is a reg and src is reg, mem, or imm OR dest is
      ;; mem and src is reg.
      (Trivit (dest src)
        (define (bad!) (sorry! who "unexpected combination of src ~s and dest ~s" src dest))
        (cond
          [(ax-reg? dest)
           (record-case src
             [(reg) ignore (emit mov dest src code*)]
             [(imm) (n)
              (ax-movi dest n code*)]
             [(literal) stuff
              (ax-reloc dest
                (asm-helper-relocation code* (cons 'pb-abs stuff)))]
             [(disp) (n breg)
              (safe-assert (signed16? n))
              (emit ld (constant pb-int64) dest `(reg . ,breg) `(imm ,n) code*)]
             [(index) (n ireg breg)
              (safe-assert (eqv? n 0))
              (emit ld (constant pb-int64) dest `(reg . ,breg) `(reg . ,ireg) code*)]
             [else (bad!)])]
          [(ax-reg? src)
           (record-case dest
             [(disp) (n breg)
              (safe-assert (signed16? n))
              (emit st (constant pb-int64) `(reg . ,breg) `(imm ,n) src code*)]
             [(index) (n ireg breg)
              (safe-assert (eqv? n 0))
              (emit st (constant pb-int64) `(reg . ,breg) `(reg . ,ireg) src code*)]
             [else (bad!)])]
          [else (bad!)]))))

  (define asm-add
    (lambda (set-cc?)
      (lambda (code* dest src0 src1)
        (Trivit (dest src0 src1)
          (emit add set-cc? dest src0 src1 code*)))))

  (define asm-sub
    (lambda (op)
      (lambda (code* dest src0 src1)
        (Trivit (dest src0 src1)
          (cond
            [(eq? op '-/eq)
             (emit subz #t dest src0 src1 code*)]
            [(eq? op '-/pos)
             (emit subp #t dest src0 src1 code*)]
            [else
             (emit sub (eq? op '-/ovfl) dest src0 src1 code*)])))))

  (define asm-mul
    (lambda (set-cc?)
      (lambda (code* dest src0 src1)
        (Trivit (dest src0 src1)
          (emit mul set-cc? dest src0 src1 code*)))))

  (define asm-div
    (lambda (code* dest src0 src1)
      (Trivit (dest src0 src1)
        (emit div dest src0 src1 code*))))

  (define asm-logical
    (lambda (op)
      (lambda (code* dest src0 src1)
        (Trivit (dest src0 src1)
          (case op
            [(logand) (emit land dest src0 src1 code*)]
            [(logor)  (emit lior dest src0 src1 code*)]
            [(logxor) (emit lxor dest src0 src1 code*)]
            [(sll)    (emit lsl dest src0 src1 code*)]
            [(srl)    (emit lsr dest src0 src1 code*)]
            [(sra)    (emit asr dest src0 src1 code*)]
            [(slol)   (emit lslo dest src0 src1 code*)]
            [else ($oops 'asm-logical "unexpected ~s" op)])))))

  (define asm-lognot
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit lnot dest src code*))))

  (define-who asm-fl-cvt
    (lambda (op)
      (lambda (code* dest src)
        (Trivit (dest src)
          (case op
            [(single->double)
             (emit mov.s->d dest src code*)]
            [(double->single)
             (emit mov.d->s dest src code*)]
            [else (sorry! who "unrecognized op ~s" op)])))))

  (define-who asm-load
    (lambda (type)
      (lambda (code* dest base index/offset)
        (Trivit (dest base index/offset)
          (case type
            [(integer-64 unsigned-64) (emit ld (constant pb-int64) dest base index/offset code*)]
            [(integer-32) (emit ld (constant pb-int32) dest base index/offset code*)]
            [(unsigned-32) (emit ld (constant pb-uint32) dest base index/offset code*)]
            [(integer-16) (emit ld (constant pb-int16) dest base index/offset code*)]
            [(unsigned-16) (emit ld (constant pb-uint16) dest base index/offset code*)]
            [(integer-8) (emit ld (constant pb-int8) dest base index/offset code*)]
            [(unsigned-8) (emit ld (constant pb-uint8) dest base index/offset code*)]
            [(double) (emit ld (constant pb-double) dest base index/offset code*)]
            [(float) (emit ld (constant pb-single) dest base index/offset code*)]
            [else (sorry! who "unexpected mref type ~s" type)])))))

  (define-who asm-store
    (lambda (type)
      (lambda (code* base index/offset src)
        (Trivit (base index/offset src)
          (case type
            [(integer-64 unsigned-64) (emit st (constant pb-int64) base index/offset src code*)]
            [(integer-32 unsigned-32) (emit st (constant pb-int32) base index/offset src code*)]
            [(integer-16 unsigned-16) (emit st (constant pb-int16) base index/offset src code*)]
            [(integer-8 unsigned-8) (emit st (constant pb-int8) base index/offset src code*)]
            [(double) (emit st (constant pb-double) base index/offset src code*)]
            [(float) (emit st (constant pb-single) base index/offset src code*)]
            [else (sorry! who "unexpected mref type ~s" type)])))))
  
  (define-who asm-fpop-2
    (lambda (op)
      (lambda (code* dest src1 src2)
        (Trivit (dest src1 src2)
          (case op
            [(fp+) (emit fadd dest src1 src2 code*)]
            [(fp-) (emit fsub dest src1 src2 code*)]
            [(fp*) (emit fmul dest src1 src2 code*)]
            [(fp/) (emit fdiv dest src1 src2 code*)]
            [else (sorry! who "unrecognized op ~s" op)])))))

  (define asm-fpsqrt
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit fsqrt dest src code*))))

  (define asm-fpsingle
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit mov.d->s->d dest src code*))))

  (define asm-fptrunc
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit mov.d->i dest src code*))))

  (define asm-fpt
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit mov.i->d dest src code*))))

  (define-who asm-fpmove
    ;; fpmove pseudo instruction is used by set! case in
    ;; select-instructions! and generate-code; at most one of src or
    ;; dest can be an mref
    (lambda (code* dest src)
      (gen-fpmove who code* dest src #t)))

  (define-who asm-fpmove-single
    (lambda (code* dest src)
      (gen-fpmove who code* dest src #f)))

  (define gen-fpmove
    (lambda (who code* dest src double?)
      (Trivit (dest src)
        (record-case dest
          [(disp) (imm reg)
           (emit st (if double? (constant pb-double) (constant pb-single)) `(reg . ,reg) `(imm ,imm) src code*)]
          [(index) (n ireg breg)
           (emit st (if double? (constant pb-double) (constant pb-single)) `(reg . ,breg) `(reg . ,ireg) src code*)]
          [else
           (record-case src
             [(disp) (imm reg)
              (emit ld (if double? (constant pb-double) (constant pb-single)) dest `(reg . ,reg) `(imm ,imm) code*)]
             [(index) (n ireg breg)
              (emit ld (if double? (constant pb-double) (constant pb-single)) dest `(reg . ,breg) `(reg . ,ireg) code*)]
             [else (emit fpmov dest src code*)])]))))

  (constant-case ptr-bits
    [(64)
     (define asm-fpcastto
       (lambda (code* dest src)
         (Trivit (dest src)
           (emit mov.d*>i dest src code*))))  
     
     (define asm-fpcastfrom
       (lambda (code* dest src)
         (Trivit (dest src)
           (emit mov.i*>d dest src code*))))]
    [(32)
     (define asm-fpcastto
       (lambda (part)
         (lambda (code* dest src)
           (Trivit (dest src)
             (if (eq? part 'hi)
                 (emit mov.d*h>i dest src code*)
                 (emit mov.d*l>i dest src code*))))))
     
     (define asm-fpcastfrom
       (lambda (code* dest src-lo src-hi)
         (Trivit (dest src-lo src-hi)
           (emit mov.ii*>d dest src-lo src-hi code*))))])

  (define-who asm-swap
    (lambda (type)
      (lambda (code* dest src)
        (Trivit (dest src)
          (case type
            [(integer-64 unsigned-64) (emit rev (constant pb-int64) dest src code*)]
            [(integer-32) (emit rev (constant pb-int32) dest src code*)]
            [(unsigned-32) (emit rev (constant pb-uint32) dest src code*)]
            [(integer-16) (emit rev (constant pb-int16) dest src code*)]
            [(unsigned-16) (emit rev (constant pb-uint16) dest src code*)]
            [else (sorry! who "unexpected asm-swap type argument ~s" type)])))))

  (define (asm-call-arena opcode)
    (lambda (code* reg off)
      (Trivit (reg off)
        (emit call-arena opcode reg off code*))))

  (define asm-stack-call
    (lambda (code* dest proto)
      (Trivit (dest proto)
        (emit stack-call dest proto code*))))

  (define asm-inc!
    (lambda (code* dest src)
      (Trivit (dest src)
        (emit inc dest src code*))))

  (define asm-lock!
    (lambda (info)
      (lambda (l1 l2 offset dest)
        (values
         (Trivit (dest)
           (emit lock dest '()))
         (asm-conditional-jump info l1 l2 offset)))))

  (define asm-cas!
    (lambda (code* dest old new)
      (Trivit (dest old new)
        (emit cas dest old new code*))))

  (define-who asm-fence
    (lambda (kind)
      (lambda (code*)
        (emit fence kind code*))))

  (define-who asm-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (values
         (Trivit (x y)
           (define-syntax sel
             (lambda (stx)
               (syntax-case stx ()
                 [(_ pos neg)
                  #`(if (info-condition-code-reversed? info)
                        (emit neg x y '())
                        (emit pos x y '()))])))
           (case (info-condition-code-type info)
             [(eq?) (emit eq x y '())]
             [(u<) (sel bl ab)]
             [(<) (sel lt gt)]
             [(>) (sel gt lt)]
             [(<=) (sel le ge)]
             [(>=) (sel ge le)]
             [(logtest) (emit cs x y '())]
             [(log!test) (emit cc x y '())]
             [else (sorry! who "unexpected ~s" (info-condition-code-type info))]))
         (asm-conditional-jump info l1 l2 offset)))))

  (define-who asm-fp-relop
    (lambda (info)
      (lambda (l1 l2 offset x y)
        (Trivit (x y)
          (values
           (case (info-condition-code-type info)
             [(fp=) (emit fpeq x y '())]
             [(fp<) (emit fplt x y '())]
             [(fp<=) (emit fple x y '())]
             [else (sorry! who "unrecognized ~s" (info-condition-code-type info))])
           (asm-conditional-jump info l1 l2 offset))))))

  (define asm-condition-code
    (lambda (info)
      (rec asm-check-flag-internal
        (lambda (l1 l2 offset)
          (values '() (asm-conditional-jump info l1 l2 offset))))))

  (define asm-library-jump
    (lambda (l)
      (asm-helper-jump '()
        `(pb-proc ,(constant code-data-disp) (library-code ,(libspec-label-libspec l))))))

  (define asm-library-call
    (lambda (libspec)
      (let ([target `(pb-proc ,(constant code-data-disp) (library-code ,libspec))])
        (lambda (code* dest jmptmp . ignore)
          (asm-helper-call code* jmptmp #t target)))))

  (define asm-library-call!
    (lambda (libspec)
      (let ([target `(pb-proc ,(constant code-data-disp) (library-code ,libspec))])
        (lambda (code* jmptmp . ignore)
          (asm-helper-call code* jmptmp #t target)))))

  (define asm-c-simple-call
    (lambda (entry)
      (let ([target `(pb-proc 0 (entry ,entry))])
        (lambda (code* jmptmp . ignore)
          (asm-helper-call code* jmptmp #f target)))))
    
  (define-who asm-indirect-call
    (lambda (code* dest proto . ignore)
      (Trivit (dest proto)
        (unless (ax-reg? dest) (sorry! who "unexpected dest ~s" dest))
        (emit call dest proto code*))))

  (define asm-direct-jump
    (lambda (l offset)
      (let ([offset (adjust-return-point-offset offset l)])
        (asm-helper-jump '() (make-funcrel 'pb-proc l offset)))))

  (define asm-literal-jump
    (lambda (info)
      (asm-helper-jump '()
        `(pb-proc ,(info-literal-offset info) (,(info-literal-type info) ,(info-literal-addr info))))))

  (define-who asm-indirect-jump
    (lambda (src)
      (Trivit (src)
        (record-case src
          [(reg) ignore (emit b src '())]
          [(disp) (n breg)
           (assert (signed16? n))
           (emit b* `(reg . ,breg) `(imm ,n) '())]
          [(index) (n ireg breg)
           (safe-assert (eqv? n 0))
           (emit b* `(reg . ,breg) `(reg . ,ireg) '())]
          [else (sorry! who "unexpected src ~s" src)]))))

  (define-who asm-return-address
    (lambda (dest l incr-offset next-addr)
      (make-rachunk dest l incr-offset next-addr
        (cond
          [(local-label-offset l) =>
           (lambda (offset)
             (let ([incr-offset (adjust-return-point-offset incr-offset l)])
               (let ([disp (fx- next-addr (fx- offset incr-offset))])
                 (unless (zero? (bitwise-and disp #b11))
                   (sorry! who "displacement is not a multiple of the instruction size" disp))
                 (unless (<= (- (expt 2 21)) disp (sub1 (expt 2 21)))
                   (sorry! who "displacement too large for adr ~s" disp))
                 (emit adr `(reg . ,dest) disp '()))))]
          [else
           (asm-move '() dest (with-output-language (L16 Triv) `(label-ref ,l ,incr-offset)))]))))

  (define-who asm-jump
    (lambda (l next-addr)
      (make-gchunk l next-addr
        (cond
          [(local-label-offset l) =>
           (lambda (offset)
             (let ([disp (fx- next-addr offset)])
               (cond
                 [(eqv? disp 0) '()]
                 [else
                  (safe-assert (signed24? disp))
                  (emit b `(label ,disp ,l) '())])))]
          [else
            ;; label must be somewhere above.  generate something so that a hard loop
            ;; doesn't get dropped.  this also has some chance of being the right size
            ;; for the final branch instruction.
            (emit b `(label 0 ,l) '())]))))

  (define-who asm-conditional-jump
    (lambda (info l1 l2 next-addr)
      (make-cgchunk info l1 l2 next-addr
        (let ()
          (define get-disp-opnd
            (lambda (next-addr l)
              (if (local-label? l)
                  (cond
                    [(local-label-offset l) =>
                     (lambda (offset)
                       (let ([disp (fx- next-addr offset)])
                         (safe-assert (signed24? disp))
                         (values disp `(label ,disp ,l))))]
                    [else (values 0 `(label 0 ,l))])
                  (sorry! who "unexpected label ~s" l))))
          
          (let-values ([(disp1 opnd1) (get-disp-opnd next-addr l1)]
                       [(disp2 opnd2) (get-disp-opnd next-addr l2)])
            (cond
              [(fx= disp1 0)
               (emit bfals opnd2 '())]
              [(fx= disp2 0)
               (emit btrue opnd1 '())]
              [else
               (let-values ([(disp1 opnd1) (get-disp-opnd (fx+ next-addr 4) l1)])
                 (emit btrue opnd1 (emit b opnd2 '())))]))))))

  (define asm-helper-jump
    (lambda (code* reloc)
      (let ([jmptmp (cons 'reg %ts)])
        (ax-reloc jmptmp
          (emit b jmptmp
            (asm-helper-relocation code* reloc))))))

  (define asm-helper-call
    (lambda (code* jmptmp interp? reloc)
      (ax-reloc `(reg . ,jmptmp)
        (let ([code* (asm-helper-relocation code* reloc)])
          (if interp?
              (emit interp `(reg . ,jmptmp) code*)
              (emit call `(reg . ,jmptmp) `(imm ,(constant pb-call-void)) code*))))))

  (define asm-helper-relocation
    (lambda (code* reloc)
      (cons* reloc (aop-cons* `(asm "relocation:" ,reloc) code*))))

  (define asm-return (lambda () (emit ret '())))

  (define asm-c-return (lambda (info) (emit ret '())))

  (define asm-enter values)

  (define asm-kill
    (lambda (code* dest)
      code*))

  (define asm-nop
    (lambda ()
      (make-chunk (emit nop '()))))

  (module (asm-foreign-call asm-foreign-callable)
    (define int-argument-regs (lambda () (list %Carg1 %Carg2 %Carg3 %Carg4 %Carg5 %Carg6 %Carg7)))
    (define fp-argument-regs (lambda () (list %Cfparg1 %Cfparg2 %Cfparg3 %Cfparg4 %Cfparg5 %Cfparg6)))

    (define prototypes (constant pb-prototype-table))

    (define (is-result-as-arg? info)
      (nanopass-case (Ltype Type) (info-foreign-result-type info)
        [(fp-ftd& ,ftd) #t]
        [else #f]))

    (define (adjust-active? info)
      (if-feature pthreads
        (memq 'adjust-active (info-foreign-conv* info))
        #f))

    (define (make-type-desc-literal info args-enc res-enc)
      (let ([result-as-arg? (is-result-as-arg? info)]
            [varargs-after (ormap (lambda (conv)
                                    (and (pair? conv) (eq? (car conv) 'varargs) (cdr conv)))
                                  (info-foreign-conv* info))])
        (make-info-literal #f 'object
                           (list->vector
                            (cons* #f
                                   (constant ffi-default-abi)
                                   (or varargs-after 0)
                                   (adjust-active? info)
                                   (car res-enc)
                                   result-as-arg?
                                   (if result-as-arg?
                                       (cdr args-enc)
                                       args-enc)))
                           0)))

    (define 64-bit-type-on-32-bit?
      (lambda (type)
        (nanopass-case (Ltype Type) type
          [(fp-integer ,bits)
           (constant-case ptr-bits
             [(64) #f]
             [(32) (fx= bits 64)])]
          [(fp-unsigned ,bits)
           (constant-case ptr-bits
             [(64) #f]
             [(32) (fx= bits 64)])]
          [else #f])))

    (define do-types/arena
      (with-output-language (L13 Effect)
        (let ()
          (define load-double/unboxed
            (lambda (off)
              (lambda (x) ; unboxed
                `(seq
                  (set! ,%Cfparg1 ,x)
                  ,(%inline fp-call-arena-in ,%Cfparg1 (immediate ,off))))))
          (define load-double/boxed
            (lambda (off)
              (lambda (x) ; requires var
                `(seq
                  (set! ,%Cfparg1 ,(%mref ,x ,%zero ,(constant flonum-data-disp) fp))
                  ,(%inline fp-call-arena-in ,%Cfparg1 (immediate ,off))))))
          (define load-int
            (lambda (off)
              (lambda (x)
                `(seq
                  (set! ,%Carg1 ,x)
                  ,(%inline call-arena-in ,%Carg1 (immediate ,off))))))
          (define load-two-int
            (lambda (off)
              (lambda (lo hi)
                (%seq
                 (set! ,%Carg1 ,lo)
                 ,(%inline call-arena-in ,%Carg1 (immediate ,off))
                 (set! ,%Carg1 ,hi)
                 ,(%inline call-arena-in ,%Carg1 (immediate ,(fx+ off 4)))))))
          (define save-double/unboxed
            (lambda (off)
              (lambda (lvalue) ; unboxed
                `(set! ,lvalue ,(%inline fp-call-arena-out (immediate ,off))))))
          (define save-double/boxed
            (lambda (off)
              (lambda (x) ; requires var
                `(seq
                  (set! ,%Cfparg1 ,(%inline fp-call-arena-out (immediate ,off)))
                  (set! ,(%mref ,x ,%zero ,(constant flonum-data-disp) fp) ,%Cfparg1)))))
          (define save-int
            (lambda (off)
              (lambda (lvalue)
                `(set! ,lvalue ,(%inline call-arena-out (immediate ,off))))))
          (define save-two-int
            (lambda (off)
              (lambda (lo hi)
                `(seq
                  (set! ,lo ,(%inline call-arena-out (immediate ,off)))
                  (set! ,hi ,(%inline call-arena-out (immediate ,(fx+ off 4))))))))

          (lambda (types in? unboxed-fp?)
            (let loop ([types types] [locs '()] [encs '()] [off 0])
              (if (null? types)
                  (values (reverse locs) (reverse encs))
                  (let ([type (car types)]
                        [types (cdr types)])
                    (nanopass-case (Ltype Type) type
                      [(fp-double-float)
                       (loop types
                             (cons (if in?
                                       (if unboxed-fp?
                                           (load-double/unboxed off)
                                           (load-double/boxed off))
                                       (if unboxed-fp?
                                           (save-double/unboxed off)
                                           (save-double/boxed off)))
                                   locs)
                             (cons (constant ffi-typerep-double) encs)
                             (fx+ off 8))]
                      [(fp-single-float)
                       (loop types
                             (cons (if in?
                                       (if unboxed-fp?
                                           (load-double/unboxed off)
                                           (load-double/boxed off))
                                       (if unboxed-fp?
                                           (save-double/unboxed off)
                                           (save-double/boxed off)))
                                   locs)
                             (cons (constant ffi-typerep-float) encs)
                             (fx+ off 8))]
                      [(fp-ftd& ,ftd)
                       (loop types
                             (cons (if in?
                                       (load-int off)
                                       (save-int off))
                                   locs)
                             (cons (let ([e ($ftd-ffi-encode ftd)])
                                     (if ($ftd-compound? ftd)
                                         e
                                         (box e)))
                                   encs)
                             (fx+ off (constant ptr-bytes)))]
                      [(fp-void)
                       (loop types
                             (cons (lambda () `(nop)) locs)
                             (cons (constant ffi-typerep-void)
                                   encs)
                             off)]
                      [else
                       (cond
                         [(64-bit-type-on-32-bit? type)
                          (loop types
                                (cons (if in?
                                          (load-two-int off)
                                          (save-two-int off))
                                      locs)
                                (nanopass-case (Ltype Type) type
                                  [(fp-integer ,bits) (cons (constant ffi-typerep-sint64) encs)]
                                  [else (cons (constant ffi-typerep-uint64) encs)])
                                (fx+ off 8))]
                         [else
                          (loop types
                                (cons (if in?
                                          (load-int off)
                                          (save-int off))
                                      locs)
                                (nanopass-case (Ltype Type) type
                                  [(fp-integer ,bits)
                                   (case bits
                                     [(64) (cons (constant ffi-typerep-sint64) encs)]
                                     [(32) (cons (constant ffi-typerep-sint32) encs)]
                                     [(16) (cons (constant ffi-typerep-sint16) encs)]
                                     [else (cons (constant ffi-typerep-sint8) encs)])]
                                  [(fp-unsigned ,bits)
                                   (case bits
                                     [(64) (cons (constant ffi-typerep-uint64) encs)]
                                     [(32) (cons (constant ffi-typerep-uint32) encs)]
                                     [(16) (cons (constant ffi-typerep-uint16) encs)]
                                     [else (cons (constant ffi-typerep-uint8) encs)])]
                                  [else (cons (constant ffi-typerep-pointer) encs)])
                                (fx+ off (constant ptr-bytes)))])]))))))))

    (define-who asm-foreign-call
      (with-output-language (L13 Effect)
        (letrec ([load-double-reg
                  (lambda (fpreg)
                    (lambda (x) ; unboxed
                      `(set! ,fpreg ,x)))]
                 [load-int-reg
                  (lambda (ireg)
                    (lambda (x)
                      `(set! ,ireg ,x)))]
                 [load-two-int-regs
                  (lambda (lo-ireg hi-ireg)
                    (lambda (lo hi)
                      `(seq
                        (set! ,lo-ireg ,lo)
                        (set! ,hi-ireg ,hi))))]
                 [do-args/reg
                  (lambda (in-types)
                    (let loop ([types in-types] [locs '()] [live* '()] [int* (int-argument-regs)] [fp* (fp-argument-regs)])
                      (if (null? types)
                          (values locs live*)
                          (let ([type (car types)]
                                [types (cdr types)])
                            (nanopass-case (Ltype Type) type
                              [(fp-double-float)
                               (when (null? fp*) (sorry! who "too many floating-point arguments"))
                               (loop types
                                     (cons (load-double-reg (car fp*)) locs)
                                     (cons (car fp*) live*)
                                     int* (cdr fp*))]
                              [(fp-single-float)
                               (when (null? fp*) (sorry! who "too many floating-point arguments"))
                               (loop types
                                     (cons (load-double-reg (car fp*)) locs)
                                     (cons (car fp*) live*)
                                     int* (cdr fp*))]
                              [(fp-ftd& ,ftd)
                               (sorry! who "indirect arguments not supported")]
                              [else
                               (when (null? int*) (sorry! who "too many integer/pointer arguments: ~s" (length in-types)))
                               (cond
                                 [(64-bit-type-on-32-bit? type)
                                  (when (null? (cdr int*)) (sorry! who "too many integer/pointer arguments: ~s" (length in-types)))
                                  (loop types
                                        (cons (load-two-int-regs (car int*) (cadr int*)) locs)
                                        (cons* (cadr int*) (car int*) live*)
                                        (cddr int*) fp*)]
                                 [else
                                  (loop types
                                        (cons (load-int-reg (car int*)) locs)
                                        (cons (car int*) live*)
                                        (cdr int*) fp*)])])))))]
                 [do-result/reg
                  (lambda (type)
                    (nanopass-case (Ltype Type) type
                      [(fp-double-float)
                       (values (lambda (lvalue) ; unboxed
                                 `(set! ,lvalue ,%Cfpretval))
                               (list %Cfpretval))]
                      [(fp-single-float)
                       (values (lambda (lvalue) ; unboxed
                                 `(set! ,lvalue ,(%inline single->double ,%Cfpretval)))
                               (list %Cfpretval))]
                      [(fp-ftd& ,ftd)
                       (sorry! who "unhandled result type ~s" type)]
                      [else
                       (when (64-bit-type-on-32-bit? type)
                         (sorry! who "unhandled result type ~s" type))
                       (values (lambda (lvalue) `(set! ,lvalue ,%Cretval))
                               (list %Cretval))]))]
                 [get-prototype
                  (lambda (type*)
                    (let* ([prototype 
                            (map (lambda (type)
                                   (nanopass-case (Ltype Type) type
                                     [(fp-double-float) 'double]
                                     [(fp-single-float) 'float]
                                     [(fp-integer ,bits)
                                      (constant-case ptr-bits
                                        [(64) (case bits
                                                [(8) 'int8]
                                                [(16) 'int16]
                                                [(32) 'int32]
                                                [else 'uptr])]
                                        [(32) (case bits
                                                [(8) 'int8]
                                                [(16) 'int16]
                                                [(32) 'uptr]
                                                [else 'int64])])]
                                     [(fp-unsigned ,bits)
                                      (constant-case ptr-bits
                                        [(64) (case bits
                                                [(8) 'uint8]
                                                [(16) 'uint16]
                                                [(32) 'uint32]
                                                [else 'uptr])]
                                        [(32) (case bits
                                                [(8) 'uint8]
                                                [(16) 'uint16]
                                                [(32) 'uptr]
                                                [else 'int64])])]
                                     [(fp-scheme-object) 'uptr]
                                     [(fp-fixnum) 'uptr]
                                     [(fp-u8*) 'void*]
                                     [(fp-ftd ,ftd) 'void*]
                                     [(fp-void) 'void]
                                     [else (if (eq? (subset-mode) 'system)
                                               (sorry! who "unhandled type in prototype ~s" type)
                                               #f)]))
                                 type*)]
                           [a (assoc prototype prototypes)])
                      (cond
                        [(not a)
                         (when (eq? (subset-mode) 'system)
                           (sorry! who "unsupported prototype ~a" prototype))
                         #f]
                        [else (cdr a)])))])
          (lambda (info)
            (let* ([arg-type* (info-foreign-arg-type* info)]
                   [result-type (info-foreign-result-type info)])
              (let ([prototype (and (not (adjust-active? info))
                                    (not (ormap (lambda (conv)
                                                  (and (pair? conv) (eq? (car conv) 'varargs) (cdr conv)))
                                                (info-foreign-conv* info)))
                                    (get-prototype (cons result-type arg-type*)))])
                (cond
                  [prototype
                   (let-values ([(locs arg-live*) (do-args/reg arg-type*)]
                                [(get-result result-live*) (do-result/reg result-type)])
                     (values
                      (lambda () `(nop))
                      (reverse locs)
                      (lambda (t0 not-varargs?)
                        (let ([info (make-info-kill*-live* (add-caller-save-registers result-live*) arg-live*)])
                          `(inline ,info ,%c-call ,t0 (immediate ,prototype))))
                      get-result
                      (lambda () `(nop))))]
                  [else
                   (let-values ([(locs args-enc) (do-types/arena arg-type* #t #t)]
                                [(res-locs res-enc) (do-types/arena (list result-type) #f #t)])
                     (values
                      (lambda () `(nop))
                      locs
                      (lambda (t0 not-varargs?)
                        `(seq
                          (set! ,%Carg1 (literal ,(make-type-desc-literal info args-enc res-enc)))
                            (inline ,null-info ,%c-stack-call ,t0 ,%Carg1)))
                      (car res-locs)
                      (lambda () `(nop))))])))))))

    (define-who asm-foreign-callable
      (with-output-language (L13 Effect)
        (lambda (info)
          (let-values ([(locs args-enc) (do-types/arena (info-foreign-arg-type* info) #f #f)]
                       [(res-locs res-enc) (do-types/arena (list (info-foreign-result-type info)) #t #f)])
            (values
             (lambda () `(nop))
             locs
             (let ([c-result (if (is-result-as-arg? info)
                                 (lambda () `(nop))
                                 (car res-locs))])
               (lambda args
                 `(seq
                   ;; this literal is recognized by `$instiate-code-object`, similar to how the
                   ;; procedure cookie is recognized
                   (set! ,%Carg7 (literal ,(make-type-desc-literal info args-enc res-enc)))
                   ,(apply c-result args))))
             (lambda ()
               ;; Keeping %Carg7 live so the type-description load can't be optimized way
               (in-context Tail `(asm-c-return ,null-info ,%Carg7)))))))))
)
