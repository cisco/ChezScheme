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

(define-syntax disable-unbound-warning
  (syntax-rules ()
    ((_ name ...)
     (eval-when (compile load eval)
       ($sputprop 'name 'no-unbound-warning #t) ...))))

(disable-unbound-warning
  lookup-constant
  flag->mask
  construct-name
  tc-field-list
)

(define-syntax define-constant
  (lambda (x)
    (syntax-case x ()
      ((_ ctype x y)
       (and (identifier? #'ctype) (identifier? #'x))
       #'(eval-when (compile load eval)
           (putprop 'x '*constant-ctype* 'ctype)
           (putprop 'x '*constant* y)))
      ((_ x y)
       (identifier? #'x)
       #'(eval-when (compile load eval)
           (putprop 'x '*constant* y))))))

(eval-when (compile load eval)
(define lookup-constant
   (let ([flag (box #f)])
      (lambda (x)
         (unless (symbol? x)
            ($oops 'lookup-constant "~s is not a symbol" x))
         (let ([v (getprop x '*constant* flag)])
            (when (eq? v flag)
               ($oops 'lookup-constant "undefined constant ~s" x))
            v))))
)

(define-syntax constant
  (lambda (x)
    (syntax-case x ()
      ((_ x)
       (identifier? #'x)
       #`'#,(datum->syntax #'x
              (lookup-constant (datum x)))))))

(define-syntax constant-case
  (syntax-rules (else)
    [(_ const [(k ...) e1 e2 ...] ... [else ee1 ee2 ...])
     (meta-cond
       [(member (constant const) '(k ...)) e1 e2 ...]
       ...
       [else ee1 ee2 ...])]
    [(_ const [(k ...) e1 e2 ...] ...)
     (meta-cond
       [(member (constant const) '(k ...)) e1 e2 ...]
       ...
       [else (syntax-error #'const
               (format "unhandled value ~s" (constant const)))])]))

(eval-when (compile load eval)
(define construct-name
  (lambda (template-identifier . args)
    (datum->syntax
      template-identifier
      (string->symbol
        (apply string-append
          (map (lambda (x) (format "~a" (syntax->datum x)))
            args))))))
)

(define-syntax macro-define-structure
  (lambda (x)
    (define constant?
      (lambda (x)
        (or (let ((x (syntax->datum x)))
              (or (boolean? x) (string? x) (char? x) (number? x)))
            (syntax-case x (quote)
              ((quote obj) #t)
              (else #f)))))
    (syntax-case x ()
      ((_ (name id1 ...))
       (andmap identifier? #'(name id1 ...))
       #'(macro-define-structure (name id1 ...) ()))
      ((_ (name id1 ...) ((id2 init) ...))
       (and (andmap identifier? #'(name id1 ... id2 ...))
            (andmap constant? #'(init ...)))
       (with-syntax
         ((constructor (construct-name #'name "make-" #'name))
          (predicate (construct-name #'name #'name "?"))
          ((index-name ...)
           (map (lambda (x) (construct-name x #'name "-" x "-index"))
                #'(id1 ... id2 ...)))
          ((access ...)
           (map (lambda (x) (construct-name x #'name "-" x))
                #'(id1 ... id2 ...)))
          ((assign ...)
           (map (lambda (x) (construct-name x "set-" #'name "-" x "!"))
                #'(id1 ... id2 ...)))
          (structure-length (fx+ (length #'(id1 ... id2 ...)) 1))
          ((index ...)
           (let f ((i 1) (ids #'(id1 ... id2 ...)))
              (if (null? ids)
                  '()
                  (cons i (f (fx+ i 1) (cdr ids)))))))
         #'(begin
             (define-syntax constructor
               (syntax-rules ()
                 ((_ id1 ...)
                  (#%vector 'name id1 ... init ...))))
             (define-syntax predicate
               (syntax-rules ()
                 ((_ x)
                  (let ((t x))
                    (and (#%vector? x)
                         (#3%fx= (#3%vector-length x) structure-length)
                         (#%eq? (#3%vector-ref x 0) 'name))))))
             (define-constant index-name index)
             ...
             (define-syntax access
               (syntax-rules ()
                 ((_ x) (#%vector-ref x index))))
             ...
             (define-syntax assign
               (syntax-rules ()
                 ((_ x update) (#%vector-set! x index update))))
             ...))))))

(define-syntax type-case
  (syntax-rules (else)
    [(_ expr
        [(pred1 pred2 ...) e1 e2 ...] ...
        [else ee1 ee2 ...])
     (let ([t expr])
       (cond
         [(or (pred1 t) (pred2 t) ...) e1 e2 ...]
         ...
         [else ee1 ee2 ...]))]))

;;; machine-case and float-type-case call eval to pick up the
;;; system value of $target-machine under the assumption that
;;; we'll be in system mode when we expand the macro

(define-syntax machine-case
  (lambda (x)
    (let ((target-machine (eval '($target-machine))))
      (let loop ((x (syntax-case x () ((_ m ...) #'(m ...)))))
        (syntax-case x (else)
          ((((a1 a2 ...) e ...) m1 m2 ...)
           (let ((machines (datum (a1 a2 ...))))
             (if (memq target-machine machines)
                 (if (null? #'(e ...))
                     (begin
                       (printf "Warning: empty machine-case clause for ~s~%"
                               machines)
                       #'($oops 'assembler
                                "empty machine-case clause for ~s"
                                '(a1 a2 ...)))
                     #'(begin e ...))
                 (loop (cdr x)))))
          (((else e1 e2 ...)) #'(begin e1 e2 ...)))))))

(define-syntax float-type-case
  (lambda (x)
    (syntax-case x (ieee else)
      ((_ ((ieee tag ...) e1 e2 ...) m ...)
       #t ; all currently supported machines are ieee
       #'(begin e1 e2 ...))
      ((_ ((tag1 tag2 ...) e1 e2 ...) m ...)
       #'(float-type-case ((tag2 ...) e1 e2 ...) m ...))
      ((_ (() e1 e2 ...) m ...)
       #'(float-type-case m ...))
      ((_ (else e1 e2 ...))
       #'(begin e1 e2 ...)))))
(define-syntax ieee
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

;; layout of our flags field:
;; bit 0: needs head space?
;; bit 1 - 9: upper 9 bits of index (lower bit is the needs head space index
;; bit 10 - 12: interface
;; bit 13: closure?
;; bit 14: error?
;; bit 15: has-headroom-version?
(macro-define-structure (libspec name flags))

(define-constant libspec-does-not-expect-headroom-index 0)
(define-constant libspec-index-offset 0)
(define-constant libspec-index-size 10)
(define-constant libspec-index-base-offset 1)
(define-constant libspec-index-base-size 9)
(define-constant libspec-interface-offset 10)
(define-constant libspec-interface-size 3)
(define-constant libspec-closure-index 13)
(define-constant libspec-error-index 14)
(define-constant libspec-has-does-not-expect-headroom-version-index 15)
(define-constant libspec-fake-index 16)

(define-syntax make-libspec-flags
  (lambda (x)
    (syntax-case x ()
      [(_ index-base does-not-expect-headroom? closure? interface error? has-does-not-expect-headroom-version?)
       #'(begin
           (unless (fx>= (- (expt 2 (constant libspec-index-base-size)) 1) index-base 0)
             ($oops 'make-libspec-flags "libspec base index exceeds ~s-bit bound: ~s"
               (constant libspec-index-base-size) index-base))
           (unless (fx>= (- (expt 2 (constant libspec-interface-size)) 1) interface 0)
             ($oops 'make-libspec-flags "libspec interface exceeds ~s-bit bound: ~s"
               (constant libspec-interface-size) interface))
           (when (and does-not-expect-headroom? (not has-does-not-expect-headroom-version?))
             ($oops 'make-libspec-flags
               "creating invalid version of libspec that does not expect headroom"))
           (fxlogor
             (if does-not-expect-headroom?
                 (fxsll 1 (constant libspec-does-not-expect-headroom-index))
                 0)
             (fxsll index-base (constant libspec-index-base-offset))
             (fxsll interface (constant libspec-interface-offset))
             (if closure? (fxsll 1 (constant libspec-closure-index)) 0)
             (if error? (fxsll 1 (constant libspec-error-index)) 0)
             (if has-does-not-expect-headroom-version?
                 (fxsll 1 (constant libspec-has-does-not-expect-headroom-version-index))
                 0)))])))

(define-syntax libspec-does-not-expect-headroom?
  (syntax-rules ()
    [(_ ?libspec)
     (fxbit-set? (libspec-flags ?libspec) (constant libspec-does-not-expect-headroom-index))]))

(define-syntax libspec-index
  (syntax-rules ()
    [(_ ?libspec)
     (fxbit-field (libspec-flags ?libspec)
       (constant libspec-index-offset)
       (fx+ (constant libspec-index-size) (constant libspec-index-offset)))]))

(define-syntax libspec-interface
  (syntax-rules ()
    [(_ ?libspec)
     (fxbit-field (libspec-flags ?libspec)
       (constant libspec-interface-offset)
       (fx+ (constant libspec-interface-size) (constant libspec-interface-offset)))]))

(define-syntax libspec-closure?
  (syntax-rules ()
    [(_ ?libspec)
     (fxbit-set? (libspec-flags ?libspec) (constant libspec-closure-index))]))

(define-syntax libspec-error?
  (syntax-rules ()
    [(_ ?libspec)
     (fxbit-set? (libspec-flags ?libspec) (constant libspec-error-index))]))

(define-syntax libspec-has-does-not-expect-headroom-version?
  (syntax-rules ()
    [(_ ?libspec)
     (fxbit-set? (libspec-flags ?libspec) (constant libspec-has-does-not-expect-headroom-version-index))]))

(define-syntax libspec->does-not-expect-headroom-libspec
  (syntax-rules ()
    [(_ ?libspec)
     (let ([libspec ?libspec])
       (unless (libspec-has-does-not-expect-headroom-version? libspec)
         ($oops #f "generating invalid libspec for ~s that does not expect headroom"
           (libspec-name libspec)))
       (make-libspec (libspec-name libspec)
         (fxlogor (libspec-flags libspec)
           (fxsll 1 (constant libspec-does-not-expect-headroom-index)))))]))

(define-syntax libspec->headroom-libspec
  (syntax-rules ()
    [(_ ?libspec)
     (let ([libspec ?libspec])
       (make-libspec (libspec-name libspec)
         (fxlogand (libspec-flags libspec)
           (fxlognot (fxsll 1 (constant libspec-does-not-expect-headroom-index))))))]))

(define-syntax return-values
  (syntax-rules ()
    ((_ args ...) (values args ...))))

(define-syntax with-values
  (syntax-rules ()
    ((_ producer proc)
     (call-with-values (lambda () producer) proc))))

(define-syntax meta-assert
  (lambda (x)
    (syntax-case x ()
      [(_ e)
       #`(let-syntax ([t (if e (lambda () #'(void)) #,(#%$make-source-oops #f "failed meta-assertion" #'e))])
           (void))])))

(define-syntax features
  (lambda (x)
    (syntax-case x ()
      [(k foo ...)
       (with-implicit (k feature-list when-feature unless-feature if-feature)
         #'(begin
             (define-syntax feature-list
               (syntax-rules ()
                 [(_) '(foo ...)]))
             (define-syntax when-feature
               (syntax-rules (foo ...)
                 [(_ foo e1 e2 (... ...)) (begin e1 e2 (... ...))] ...
                 [(_ bar e1 e2 (... ...)) (void)]))
             (define-syntax unless-feature
               (syntax-rules (foo ...)
                 [(_ foo e1 e2 (... ...)) (void)] ...
                 [(_ bar e1 e2 (... ...)) (begin e1 e2 (... ...))]))
             (define-syntax if-feature
               (syntax-rules (foo ...)
                 [(_ foo e1 e2) e1] ...
                 [(_ bar e1 e2) e2]))))])))

(define-constant scheme-version #x00090501)

(define-syntax define-machine-types
  (lambda (x)
    (syntax-case x ()
      [(_ name ...)
       (with-syntax ([(value ...) (enumerate (datum (name ...)))]
                     [(cname ...)
                      (map (lambda (name)
                             (construct-name name "machine-type-" name))
                           #'(name ...))])
         #'(begin
             (define-constant cname value) ...
             (define-constant machine-type-alist '((value . name) ...))
             (define-constant machine-type-limit (+ (max value ...) 1))))])))

(define-machine-types
  any
  i3le      ti3le
  i3nt      ti3nt
  i3fb      ti3fb
  i3ob      ti3ob
  i3osx     ti3osx
  a6le      ta6le
  a6osx     ta6osx
  a6ob      ta6ob
  a6s2      ta6s2
  i3s2      ti3s2
  a6fb      ta6fb
  i3nb      ti3nb
  a6nb      ta6nb
  a6nt      ta6nt
  i3qnx     ti3qnx
  arm32le   tarm32le
  ppc32le   tppc32le
)

(include "machine.def")

(define-constant machine-type-name (cdr (assv (constant machine-type) (constant machine-type-alist))))

(define-syntax log2
  (syntax-rules ()
    [(_ n) (integer-length (- n 1))]))

; a string-char is a 32-bit equivalent of a ptr char: identical to a
; ptr char on 32-bit machines and the low-order half of a ptr char on
; 64-bit machines.
(define-constant string-char-bits 32)
(define-constant string-char-bytes 4)
(define-constant string-char-offset (log2 (constant string-char-bytes)))

(define-constant ptr-bytes (/ (constant ptr-bits) 8)) ; size in bytes
(define-constant log2-ptr-bytes (log2 (constant ptr-bytes)))

;;; ordinary types must be no more than 8 bits long
(define-constant ordinary-type-bits 8)    ; smallest addressable unit

; (typemod = type modulus)
; The typemod defines the range of primary types and is also the
; offset that we subtract off of the actual addresses before adding
; in the primary type tag to obtain a ptr.
;
; The typemod imposes a lower bound on our choice of alignment
; since the low n bits of aligned addresses must be zero so that
; we can steal those bits for type tags.
;
; Leaving the typemod at 8 for 64-bit ports, means that we "waste"
; a bit of primary type space.  If we ever attempt to reclaim this
; bit, we must remember that flonums are actually represented by two
; primary type codes, ie. 1xxx and 0xxx, see also the comment under
; byte-alignment.
(define-constant typemod 8)
(define-constant primary-type-bits (log2 (constant typemod)))

; We must have room for forward marker and forward pointer, hence two ptrs.
; We sometimes violate this for flonums since we "extract" the real
; and imag part by returning pointers into the inexactnum structure.
; This is safe since we never forward flonums.
(define-constant byte-alignment
  (max (constant typemod) (* 2 (constant ptr-bytes))))

;;; fasl codes---see fasl.c for documentation of representation

(define-constant fasl-type-header 0)
(define-constant fasl-type-box 1)
(define-constant fasl-type-symbol 2)
(define-constant fasl-type-ratnum 3)
(define-constant fasl-type-vector 4)
(define-constant fasl-type-inexactnum 5)
(define-constant fasl-type-closure 6)
(define-constant fasl-type-pair 7)
(define-constant fasl-type-flonum 8)
(define-constant fasl-type-string 9)
(define-constant fasl-type-large-integer 10)
(define-constant fasl-type-code 11)
(define-constant fasl-type-immediate 12)
(define-constant fasl-type-entry 13)
(define-constant fasl-type-library 14)
(define-constant fasl-type-library-code 15)
(define-constant fasl-type-graph 16)
(define-constant fasl-type-graph-def 17)
(define-constant fasl-type-graph-ref 18)
(define-constant fasl-type-gensym 19)
(define-constant fasl-type-exactnum 20)
; 21
(define-constant fasl-type-fasl-size 22)
(define-constant fasl-type-record 23)
(define-constant fasl-type-rtd 24)
(define-constant fasl-type-small-integer 25)
(define-constant fasl-type-base-rtd 26)
(define-constant fasl-type-fxvector 27)
(define-constant fasl-type-ephemeron 28)
(define-constant fasl-type-bytevector 29)
(define-constant fasl-type-weak-pair 30)
(define-constant fasl-type-eq-hashtable 31)
(define-constant fasl-type-symbol-hashtable 32)
(define-constant fasl-type-group 33)
(define-constant fasl-type-visit 34)
(define-constant fasl-type-revisit 35)

(define-constant fasl-type-immutable-vector 36)
(define-constant fasl-type-immutable-string 37)
(define-constant fasl-type-immutable-fxvector 38)
(define-constant fasl-type-immutable-bytevector 39)
(define-constant fasl-type-immutable-box 40)

(define-constant fasl-fld-ptr 0)
(define-constant fasl-fld-u8 1)
(define-constant fasl-fld-i16 2)
(define-constant fasl-fld-i24 3)
(define-constant fasl-fld-i32 4)
(define-constant fasl-fld-i40 5)
(define-constant fasl-fld-i48 6)
(define-constant fasl-fld-i56 7)
(define-constant fasl-fld-i64 8)
(define-constant fasl-fld-single 9)
(define-constant fasl-fld-double 10)

(define-constant fasl-header
  (bytevector (constant fasl-type-header) 0 0 0
    (char->integer #\c) (char->integer #\h) (char->integer #\e) (char->integer #\z)))

(define-syntax define-enumerated-constants
  (lambda (x)
    (syntax-case x ()
      [(_ reloc-name ...)
       (with-syntax ([(i ...) (enumerate #'(reloc-name ...))])
         #'(begin
             (define-constant reloc-name i)
             ...))])))

(define-syntax define-reloc-constants
  (lambda (x)
    (syntax-case x ()
      [(_ (all x ...) (arch y ...) ...)
       #`(constant-case architecture
           [(arch) (define-enumerated-constants x ... y ...)]
           ...)])))

(define-reloc-constants
  (all reloc-abs)
  (x86 reloc-rel)
  (sparc reloc-sparcabs reloc-sparcrel)
  (sparc64 reloc-sparc64abs reloc-sparc64rel)
  (ppc reloc-ppccall reloc-ppcload)
  (x86_64 reloc-x86_64-call reloc-x86_64-jump)
  (arm32 reloc-arm32-abs reloc-arm32-call reloc-arm32-jump)
  (ppc32 reloc-ppc32-abs reloc-ppc32-call reloc-ppc32-jump))

(constant-case ptr-bits
  [(64)
   (define-constant reloc-extended-format #x1)
   (define-constant reloc-type-offset 1)
   (define-constant reloc-type-mask #x7)
   (define-constant reloc-code-offset-offset 4)
   (define-constant reloc-code-offset-mask #x3ffffff)
   (define-constant reloc-item-offset-offset 30)
   (define-constant reloc-item-offset-mask #x3ffffff)]
  [(32)
   (define-constant reloc-extended-format #x1)
   (define-constant reloc-type-offset 1)
   (define-constant reloc-type-mask #x7)
   (define-constant reloc-code-offset-offset 4)
   (define-constant reloc-code-offset-mask #x3ff)
   (define-constant reloc-item-offset-offset 14)
   (define-constant reloc-item-offset-mask #x3ffff)])

(macro-define-structure (reloc type item-offset code-offset long?))

(define-constant SERROR    #x0000)
(define-constant STRVNCATE #x0001) ; V for U to avoid msvc errno.h conflict
(define-constant SREPLACE  #x0002)
(define-constant SAPPEND   #x0003)
(define-constant SDEFAULT  #x0004)

(define-constant OPEN-ERROR-OTHER 0)
(define-constant OPEN-ERROR-PROTECTION 1)
(define-constant OPEN-ERROR-EXISTS 2)
(define-constant OPEN-ERROR-EXISTSNOT 3)

(define-constant SEOF -1)

(define-constant SICONV-DUNNO 0)
(define-constant SICONV-INVALID 1)
(define-constant SICONV-INCOMPLETE 2)
(define-constant SICONV-NOROOM 3)

;;; port flag masks are always single bits

(define-constant port-flag-input             #x01)
(define-constant port-flag-output            #x02)
(define-constant port-flag-binary            #x04)
(define-constant port-flag-closed            #x08)
(define-constant port-flag-file              #x10)
(define-constant port-flag-compressed        #x20)
(define-constant port-flag-exclusive         #x40)
(define-constant port-flag-bol               #x80)
(define-constant port-flag-eof              #x100)
(define-constant port-flag-block-buffered   #x200)
(define-constant port-flag-line-buffered    #x400)
(define-constant port-flag-input-mode       #x800)
(define-constant port-flag-char-positions  #x1000)
(define-constant port-flag-r6rs            #x2000)
(define-constant port-flag-fold-case       #x4000)
(define-constant port-flag-no-fold-case    #x8000)

(define-constant port-flags-offset         (constant ordinary-type-bits))

;;; allcaps versions are pre-shifted by port-flags-offset
(define-constant PORT-FLAG-INPUT (ash (constant port-flag-input) (constant port-flags-offset)))
(define-constant PORT-FLAG-OUTPUT (ash (constant port-flag-output) (constant port-flags-offset)))
(define-constant PORT-FLAG-BINARY (ash (constant port-flag-binary) (constant port-flags-offset)))
(define-constant PORT-FLAG-CLOSED (ash (constant port-flag-closed) (constant port-flags-offset)))
(define-constant PORT-FLAG-FILE (ash (constant port-flag-file) (constant port-flags-offset)))
(define-constant PORT-FLAG-COMPRESSED (ash (constant port-flag-compressed) (constant port-flags-offset)))
(define-constant PORT-FLAG-EXCLUSIVE (ash (constant port-flag-exclusive) (constant port-flags-offset)))
(define-constant PORT-FLAG-BOL (ash (constant port-flag-bol) (constant port-flags-offset)))
(define-constant PORT-FLAG-EOF (ash (constant port-flag-eof) (constant port-flags-offset)))
(define-constant PORT-FLAG-BLOCK-BUFFERED (ash (constant port-flag-block-buffered) (constant port-flags-offset)))
(define-constant PORT-FLAG-LINE-BUFFERED (ash (constant port-flag-line-buffered) (constant port-flags-offset)))
(define-constant PORT-FLAG-INPUT-MODE (ash (constant port-flag-input-mode) (constant port-flags-offset)))
(define-constant PORT-FLAG-CHAR-POSITIONS (ash (constant port-flag-char-positions) (constant port-flags-offset)))
(define-constant PORT-FLAG-R6RS (ash (constant port-flag-r6rs) (constant port-flags-offset)))
(define-constant PORT-FLAG-FOLD-CASE (ash (constant port-flag-fold-case) (constant port-flags-offset)))
(define-constant PORT-FLAG-NO-FOLD-CASE (ash (constant port-flag-no-fold-case) (constant port-flags-offset)))

;;; c-error codes
(define-constant ERROR_OTHER 0)
(define-constant ERROR_CALL_UNBOUND 1)
(define-constant ERROR_CALL_NONPROCEDURE_SYMBOL 2)
(define-constant ERROR_CALL_NONPROCEDURE 3)
(define-constant ERROR_CALL_ARGUMENT_COUNT 4)
(define-constant ERROR_RESET 5)
(define-constant ERROR_NONCONTINUABLE_INTERRUPT 6)
(define-constant ERROR_VALUES 7)
(define-constant ERROR_MVLET 8)

;;; object-file tags
(define-constant visit-tag 0)
(define-constant revisit-tag 1)

;;; allocation spaces
(define-constant space-locked #x20)         ; lock flag
(define-constant space-old #x40)            ; oldspace flag

(define-syntax define-alloc-spaces
  (lambda (x)
    (syntax-case x (real swept unswept unreal)
      [(_ (real
            (swept
              (swept-name swept-cname swept-cchar swept-value)
              ...
              (last-swept-name last-swept-cname last-swept-cchar last-swept-value))
            (unswept
              (unswept-name unswept-cname unswept-cchar unswept-value)
              ...
              (last-unswept-name last-unswept-cname last-unswept-cchar last-unswept-value)))
          (unreal
            (unreal-name unreal-cname unreal-cchar unreal-value)
            ...
            (last-unreal-name last-unreal-cname last-unreal-cchar last-unreal-value)))
       (with-syntax ([(real-name ...) #'(swept-name ... last-swept-name unswept-name ... last-unswept-name)]
                     [(real-cname ...) #'(swept-cname ... last-swept-cname unswept-cname ... last-unswept-cname)]
                     [(real-cchar ...) #'(swept-cchar ... last-swept-cchar unswept-cchar ... last-unswept-cchar)]
                     [(real-value ...) #'(swept-value ... last-swept-value unswept-value ... last-unswept-value)])
         (with-syntax ([(name ...) #'(real-name ... unreal-name ... last-unreal-name)]
                       [(cname ...) #'(real-cname ... unreal-cname ... last-unreal-cname)]
                       [(cchar ...) #'(real-cchar ... unreal-cchar ... last-unreal-cchar)]
                       [(value ...) #'(real-value ... unreal-value ... last-unreal-value)])
           (with-syntax ([(space-name ...) (map (lambda (n) (construct-name n "space-" n)) #'(name ...))])
             (unless (< (syntax->datum #'last-unreal-value) (constant space-locked))
               ($oops 'define-alloc-spaces "conflict with space-locked"))
             (unless (< (syntax->datum #'last-unreal-value) (constant space-old))
               ($oops 'define-alloc-spaces "conflict with space-old"))
             #'(begin
                 (define-constant space-name value) ...
                 (define-constant real-space-alist '((real-name . real-value) ...))
                 (define-constant space-cname-list '(cname ...))
                 (define-constant space-char-list '(cchar ...))
                 (define-constant max-sweep-space last-swept-value)
                 (define-constant max-real-space last-unswept-value)
                 (define-constant max-space last-unreal-value)))))])))
  
(define-alloc-spaces
  (real
    (swept
      (new "new" #\n 0)                  ; all generation 0 objects allocated here
      (impure "impure" #\i 1)            ; most mutable objects allocated here (all ptrs)
      (symbol "symbol" #\x 2)            ;
      (port "port" #\q 3)                ;
      (weakpair "weakpr" #\w 4)          ;
      (ephemeron "emph" #\e 5)           ;
      (pure "pure" #\p 6)                ; swept immutable objects allocated here (all ptrs)
      (continuation "cont" #\k 7)        ;
      (code "code" #\c 8)                ;
      (pure-typed-object "p-tobj" #\r 9) ;
      (impure-record "ip-rec" #\s 10))   ;
    (unswept
      (data "data" #\d 11)))             ; unswept objects allocated here
  (unreal
    (empty "empty" #\e 12)))             ; available segments

;;; enumeration of types for which gc tracks object counts
;;; also update gc.c

(define-constant countof-pair 0)
(define-constant countof-symbol 1)
(define-constant countof-flonum 2)
(define-constant countof-closure 3)
(define-constant countof-continuation 4)
(define-constant countof-bignum 5)
(define-constant countof-ratnum 6)
(define-constant countof-inexactnum 7)
(define-constant countof-exactnum 8)
(define-constant countof-box 9)
(define-constant countof-port 10)
(define-constant countof-code 11)
(define-constant countof-thread 12)
(define-constant countof-tlc 13)
(define-constant countof-rtd-counts 14)
(define-constant countof-stack 15)
(define-constant countof-relocation-table 16)
(define-constant countof-weakpair 17)
(define-constant countof-vector 18)
(define-constant countof-string 19)
(define-constant countof-fxvector 20)
(define-constant countof-bytevector 21)
(define-constant countof-locked 22)
(define-constant countof-guardian 23)
(define-constant countof-oblist 24)
(define-constant countof-ephemeron 25)
(define-constant countof-types 26)

;;; type-fixnum is assumed to be all zeros by at least by vector, fxvector,
;;; and bytevector index checks
(define-constant type-fixnum           0) ; #b100/#b000 32-bit, #b000 64-bit
(define-constant type-pair         #b001)
(define-constant type-flonum       #b010)
(define-constant type-symbol       #b011)
; #b100 occupied by fixnums on 32-bit machines, unused on 64-bit machines
(define-constant type-closure      #b101)
(define-constant type-immediate    #b110)
(define-constant type-typed-object #b111)

;;; note: for type-char, leave at least fixnum-offset zeros at top of
;;; type byte to simplify char->integer conversion
(define-constant type-boolean       #b00000110)
(define-constant ptr sfalse         #b00000110)
(define-constant ptr strue          #b00001110)
(define-constant type-char          #b00010110)
(define-constant ptr sunbound       #b00011110)
(define-constant ptr snil           #b00100110)
(define-constant ptr forward-marker #b00101110)
(define-constant ptr seof           #b00110110)
(define-constant ptr svoid          #b00111110)
(define-constant ptr black-hole     #b01000110)
(define-constant ptr sbwp           #b01001110)

;;; on 32-bit machines, vectors get two primary tag bits, including
;;; one for the immutable flag, and so do bytevectors, so their maximum
;;; lengths are equal to the most-positive fixnum on 32-bit machines.
;;; strings and fxvectors get only one primary tag bit each and have
;;; to use a different bit for the immutable flag, so their maximum
;;; lengths are equal to 1/2 of the most-positive fixnum on 32-bit
;;; machines.  taking sizes of vector, bytevector, string, and fxvector
;;; elements into account, a vector can occupy up to 1/2 of virtual
;;; memory, a string or fxvector up to 1/4, and a bytevector up to 1/8.

;;; on 64-bit machines, vectors get only one of the primary tag bits,
;;; bytevectors still get two (but don't need two), and strings and
;;; fxvectors still get one.  all have maximum lengths equal to the
;;; most-positive fixnum.

;;; vector type/length field must look like a fixnum.  an immutable bit sits just above the fixnum tag, with the length above that.
(define-constant type-vector (constant type-fixnum))
; #b000 occupied by vectors on 32- and 64-bit machines
(define-constant type-bytevector             #b01)
(define-constant type-string                #b010)
(define-constant type-fxvector              #b011)
; #b100 occupied by vectors on 32-bit machines, unused on 64-bit machines
; #b101 occupied by type-immutable-bytevector
(define-constant type-other-number         #b0110) ; bit 3 reset for numbers
(define-constant type-bignum              #b00110) ; bit 4 reset for bignums
(define-constant type-positive-bignum    #b000110)
(define-constant type-negative-bignum    #b100110)
(define-constant type-ratnum           #b00010110) ; bit 4 set for non-bignum numbers
(define-constant type-inexactnum       #b00110110)
(define-constant type-exactnum         #b01010110)
(define-constant type-box               #b0001110) ; bit 3 set for non-numbers
(define-constant type-immutable-box    #b10001110) ; low 7 bits match `type-box`
(define-constant type-port             #b00011110)
; #b00101110 (forward_marker) must not be used
(define-constant type-code             #b00111110)
(define-constant type-thread           #b01001110)
(define-constant type-tlc              #b01011110)
(define-constant type-rtd-counts       #b01101110)
(define-constant type-record                #b111)

(define-constant code-flag-system         #b0001)
(define-constant code-flag-continuation   #b0010)

(define-constant fixnum-bits
  (case (constant ptr-bits)
    [(64) 61]
    [(32) 30]
    [else ($oops 'fixnum-bits "expected reasonable native bit width (eg. 32 or 64)")]))
(define-constant iptr most-positive-fixnum
                 (- (expt 2 (- (constant fixnum-bits) 1)) 1))
(define-constant iptr most-negative-fixnum
                 (- (expt 2 (- (constant fixnum-bits) 1))))

(define-constant fixnum-offset (- (constant ptr-bits) (constant fixnum-bits)))

; string length field (high bits) + immutabilty is stored with type
(define-constant string-length-offset      4)
(define-constant string-immutable-flag
  (expt 2 (- (constant string-length-offset) 1)))
(define-constant iptr maximum-string-length
  (min (- (expt 2 (fx- (constant ptr-bits) (constant string-length-offset))) 1)
       (constant most-positive-fixnum)))

(define-constant bignum-sign-offset        5)
(define-constant bignum-length-offset      6)
(define-constant iptr maximum-bignum-length
  (min (- (expt 2 (fx- (constant ptr-bits) (constant bignum-length-offset))) 1)
       (constant most-positive-fixnum)))
(define-constant bigit-bits                32)
(define-constant bigit-bytes               (/ (constant bigit-bits) 8))

; vector length field (high bits) + immutabilty is stored with type
(define-constant vector-length-offset (fx+ 1 (constant fixnum-offset)))
(define-constant vector-immutable-flag
  (expt 2 (- (constant vector-length-offset) 1)))
(define-constant iptr maximum-vector-length
  (min (- (expt 2 (fx- (constant ptr-bits) (constant vector-length-offset))) 1)
       (constant most-positive-fixnum)))

; fxvector length field (high bits) + immutabilty is stored with type
(define-constant fxvector-length-offset 4)
(define-constant fxvector-immutable-flag
  (expt 2 (- (constant fxvector-length-offset) 1)))
(define-constant iptr maximum-fxvector-length
  (min (- (expt 2 (fx- (constant ptr-bits) (constant fxvector-length-offset))) 1)
       (constant most-positive-fixnum)))

; bytevector length field (high bits) + immutabilty is stored with type
(define-constant bytevector-length-offset 3)
(define-constant bytevector-immutable-flag
  (expt 2 (- (constant bytevector-length-offset) 1)))
(define-constant iptr maximum-bytevector-length
  (min (- (expt 2 (fx- (constant ptr-bits) (constant bytevector-length-offset))) 1)
          (constant most-positive-fixnum)))

(define-constant code-flags-offset         (constant ordinary-type-bits))

(define-constant char-data-offset 8)

(define-constant type-binary-port
  (fxlogor (ash (constant port-flag-binary) (constant port-flags-offset))
           (constant type-port)))
(define-constant type-textual-port (constant type-port))
(define-constant type-input-port
  (fxlogor (ash (constant port-flag-input) (constant port-flags-offset))
           (constant type-port)))
(define-constant type-binary-input-port
  (fxlogor (ash (constant port-flag-binary) (constant port-flags-offset))
           (constant type-input-port)))
(define-constant type-textual-input-port (constant type-input-port))
(define-constant type-output-port
  (fxlogor (ash (constant port-flag-output) (constant port-flags-offset))
           (constant type-port)))
(define-constant type-binary-output-port
  (fxlogor (ash (constant port-flag-binary) (constant port-flags-offset))
           (constant type-output-port)))
(define-constant type-textual-output-port (constant type-output-port))
(define-constant type-io-port
  (fxlogor (constant type-input-port)
           (constant type-output-port)))
(define-constant type-system-code
  (fxlogor (constant type-code)
           (fxsll (constant code-flag-system)
                  (constant code-flags-offset))))
(define-constant type-continuation-code
  (fxlogor (constant type-code)
           (fxsll (constant code-flag-continuation)
                  (constant code-flags-offset))))

;; type checks are generally performed by applying the mask to the object
;; then comparing against the type code.  a mask equal to
;; (constant byte-constant-mask) implies that the object being
;; type-checked must have zeros in all but the low byte if it is to pass
;; the check so that anything between a byte and full word comparison
;; can be used.

(define-constant byte-constant-mask (- (ash 1 (constant ptr-bits)) 1))

(define-constant mask-fixnum (- (ash 1 (constant fixnum-offset)) 1))

;;; octets are fixnums in the range 0..255
(define-constant mask-octet (lognot (ash #xff (constant fixnum-offset))))
(define-constant type-octet (constant type-fixnum))

(define-constant mask-pair         #b111)
(define-constant mask-flonum       #b111)
(define-constant mask-symbol       #b111)
(define-constant mask-closure      #b111)
(define-constant mask-immediate    #b111)
(define-constant mask-typed-object #b111)

(define-constant mask-boolean #b11110111)
(define-constant mask-char          #xFF)
(define-constant mask-false   (constant byte-constant-mask))
(define-constant mask-eof     (constant byte-constant-mask))
(define-constant mask-unbound (constant byte-constant-mask))
(define-constant mask-nil     (constant byte-constant-mask))
(define-constant mask-bwp     (constant byte-constant-mask))

;;; vector type/length field must look like a fixnum.  an immutable bit sits just above the fixnum tag, with the length above that.
(define-constant mask-vector (constant mask-fixnum))
(define-constant mask-bytevector         #b11)
(define-constant mask-string            #b111)
(define-constant mask-fxvector          #b111)
(define-constant mask-other-number     #b1111)
(define-constant mask-bignum          #b11111)
(define-constant mask-bignum-sign    #b100000)
(define-constant mask-signed-bignum
  (fxlogor
    (constant mask-bignum)
    (constant mask-bignum-sign)))
(define-constant mask-ratnum       (constant byte-constant-mask))
(define-constant mask-inexactnum   (constant byte-constant-mask))
(define-constant mask-exactnum     (constant byte-constant-mask))
(define-constant mask-rtd-counts   (constant byte-constant-mask))
(define-constant mask-record            #b111)
(define-constant mask-port               #xFF)
(define-constant mask-binary-port
  (fxlogor (fxsll (constant port-flag-binary) (constant port-flags-offset))
           (constant mask-port)))
(define-constant mask-textual-port (constant mask-binary-port))
(define-constant mask-input-port
  (fxlogor (fxsll (constant port-flag-input) (constant port-flags-offset))
           (fx- (fxsll 1 (constant port-flags-offset)) 1)))
(define-constant mask-binary-input-port
  (fxlogor (fxsll (constant port-flag-binary) (constant port-flags-offset))
           (constant mask-input-port)))
(define-constant mask-textual-input-port (constant mask-binary-input-port))
(define-constant mask-output-port
  (fxlogor (fxsll (constant port-flag-output) (constant port-flags-offset))
           (fx- (fxsll 1 (constant port-flags-offset)) 1)))
(define-constant mask-binary-output-port
  (fxlogor (fxsll (constant port-flag-binary) (constant port-flags-offset))
           (constant mask-output-port)))
(define-constant mask-textual-output-port (constant mask-binary-output-port))
(define-constant mask-box                #x7F)
(define-constant mask-code               #xFF)
(define-constant mask-system-code
  (fxlogor (fxsll (constant code-flag-system) (constant code-flags-offset))
           (fx- (fxsll 1 (constant code-flags-offset)) 1)))
(define-constant mask-continuation-code
  (fxlogor (fxsll (constant code-flag-continuation) (constant code-flags-offset))
           (fx- (fxsll 1 (constant code-flags-offset)) 1)))
(define-constant mask-thread       (constant byte-constant-mask))
(define-constant mask-tlc          (constant byte-constant-mask))

(define-constant type-mutable-vector (constant type-vector))
(define-constant type-immutable-vector
  (fxlogor (constant type-vector) (constant vector-immutable-flag)))
(define-constant mask-mutable-vector
  (fxlogor (constant mask-vector) (constant vector-immutable-flag)))

(define-constant type-mutable-string (constant type-string))
(define-constant type-immutable-string
  (fxlogor (constant type-string) (constant string-immutable-flag)))
(define-constant mask-mutable-string
  (fxlogor (constant mask-string) (constant string-immutable-flag)))

(define-constant type-mutable-fxvector (constant type-fxvector))
(define-constant type-immutable-fxvector
  (fxlogor (constant type-fxvector) (constant fxvector-immutable-flag)))
(define-constant mask-mutable-fxvector
  (fxlogor (constant mask-fxvector) (constant fxvector-immutable-flag)))

(define-constant type-mutable-bytevector (constant type-bytevector))
(define-constant type-immutable-bytevector
  (fxlogor (constant type-bytevector) (constant bytevector-immutable-flag)))
(define-constant mask-mutable-bytevector
  (fxlogor (constant mask-bytevector) (constant bytevector-immutable-flag)))

(define-constant type-mutable-box (constant type-box))
(define-constant mask-mutable-box (constant byte-constant-mask))

(define-constant fixnum-factor        (expt 2 (constant fixnum-offset)))
(define-constant vector-length-factor (expt 2 (constant vector-length-offset)))
(define-constant string-length-factor (expt 2 (constant string-length-offset)))
(define-constant bignum-length-factor (expt 2 (constant bignum-length-offset)))
(define-constant fxvector-length-factor (expt 2 (constant fxvector-length-offset)))
(define-constant bytevector-length-factor (expt 2 (constant bytevector-length-offset)))
(define-constant char-factor          (expt 2 (constant char-data-offset)))

;;; record-datatype must be defined before we include layout.ss
;;; (maybe should move into that file??)
;;; We allow Scheme inputs for both signed and unsigned integers to range from
;;; -2^(b-1)..2^b-1, e.g., for 32-bit, -2^31..2^32-1.
(macro-define-structure (fld name mutable? type byte))

(eval-when (compile load eval)
(define-syntax foreign-datatypes
  (identifier-syntax
    '((scheme-object (constant ptr-bytes) (lambda (x) #t))
      (double-float 8 flonum?)
      (single-float 4 flonum?)
      (integer-8 1 $integer-8?)
      (unsigned-8 1 $integer-8?)
      (integer-16 2 $integer-16?)
      (unsigned-16 2 $integer-16?)
      (integer-24 3 $integer-24?)
      (unsigned-24 3 $integer-24?)
      (integer-32 4 $integer-32?)
      (unsigned-32 4 $integer-32?)
      (integer-40 5 $integer-40?)
      (unsigned-40 5 $integer-40?)
      (integer-48 6 $integer-48?)
      (unsigned-48 6 $integer-48?)
      (integer-56 7 $integer-56?)
      (unsigned-56 7 $integer-56?)
      (integer-64 8 $integer-64?)
      (unsigned-64 8 $integer-64?)
      (fixnum (constant ptr-bytes) fixnum?)
      (char 1 $foreign-char?)
      (wchar (fxsrl (constant wchar-bits) 3) $foreign-wchar?)
      (boolean (fxsrl (constant int-bits) 3) (lambda (x) #t)))))
)

(define-syntax record-datatype
  (with-syntax ((((type bytes pred) ...)
                 (datum->syntax #'* foreign-datatypes)))
    (lambda (x)
      (syntax-case x (list cases)
        [(_ list) #''(type ...)]
        [(_ cases ty handler else-expr)
         #'(case ty
             [(type) (handler type bytes pred)]
             ...
             [else else-expr])]))))

(define-syntax c-alloc-align
  (syntax-rules ()
    ((_ n)
     (fxlogand (fx+ n (fx- (constant byte-alignment) 1))
               (fxlognot (fx- (constant byte-alignment) 1))))))

(eval-when (compile load eval)
(define-syntax filter-foreign-type
 ; for $object-ref, foreign-ref, etc.
 ; foreign-procedure and foriegn-callable have their own
 ; filter-type in syntax.ss
  (with-syntax ([alist (datum->syntax #'*
                         `((ptr . scheme-object)
                           (iptr .
                             ,(constant-case ptr-bits
                                [(32) 'integer-32]
                                [(64) 'integer-64]))
                           (uptr .
                             ,(constant-case ptr-bits
                                [(32) 'unsigned-32]
                                [(64) 'unsigned-64]))
                           (void* .
                             ,(constant-case ptr-bits
                                [(32) 'unsigned-32]
                                [(64) 'unsigned-64]))
                           (int .
                             ,(constant-case int-bits
                                [(32) 'integer-32]
                                [(64) 'integer-64]))
                           (unsigned .
                             ,(constant-case int-bits
                                [(32) 'unsigned-32]
                                [(64) 'unsigned-64]))
                           (unsigned-int .
                             ,(constant-case int-bits
                                [(32) 'unsigned-32]
                                [(64) 'unsigned-64]))
                           (short .
                             ,(constant-case short-bits
                                [(16) 'integer-16]
                                [(32) 'integer-32]))
                           (unsigned-short .
                             ,(constant-case short-bits
                                [(16) 'unsigned-16]
                                [(32) 'unsigned-32]))
                           (long .
                             ,(constant-case long-bits
                                [(32) 'integer-32]
                                [(64) 'integer-64]))
                           (unsigned-long .
                             ,(constant-case long-bits
                                [(32) 'unsigned-32]
                                [(64) 'unsigned-64]))
                           (long-long .
                             ,(constant-case long-long-bits
                                [(64) 'integer-64]))
                           (unsigned-long-long .
                             ,(constant-case long-long-bits
                                [(64) 'unsigned-64]))
                           (wchar_t . wchar)
                           (size_t .
                             ,(constant-case size_t-bits
                                [(32) 'unsigned-32]
                                [(64) 'unsigned-64]))
                           (ssize_t .
                             ,(constant-case size_t-bits
                                [(32) 'integer-32]
                                [(64) 'integer-64]))
                           (ptrdiff_t .
                             ,(constant-case ptrdiff_t-bits
                                [(32) 'integer-32]
                                [(64) 'integer-64]))
                           (float . single-float)
                           (double . double-float)))])
    (syntax-rules ()
      [(_ ?x)
       (let ([x ?x])
         (cond
           [(assq x 'alist) => cdr]
           [else x]))])))
(define-syntax filter-scheme-type
 ; for define-primitive-structure-disps
  (with-syntax ([alist (datum->syntax #'*
                         `((byte . signed-8)
                           (octet . unsigned-8)
                           (I32 . integer-32)
                           (U32 . unsigned-32)
                           (I64 . integer-64)
                           (U64 . unsigned-64)
                           (bigit .
                             ,(constant-case bigit-bits
                                [(16) 'unsigned-16]
                                [(32) 'unsigned-32]))
                           (string-char .
                             ,(constant-case string-char-bits
                                [(32) 'unsigned-32]))))])
    (syntax-rules ()
      [(_ ?x)
       (let ([x ?x])
         (cond
           [(assq x 'alist) => cdr]
           [else x]))])))
)

(define-syntax define-primitive-structure-disps
  (lambda (x)
    (include "layout.ss")
    (define make-name-field-disp
      (lambda (name field-name)
        (construct-name name name "-" field-name "-disp")))
    (define split
      (lambda (ls)
        (let f ([x (car ls)] [ls (cdr ls)])
          (if (null? ls)
              (list '() x)
              (let ([rest (f (car ls) (cdr ls))])
                (list (cons x (car rest)) (cadr rest)))))))
    (define get-fld-byte
      (lambda (fn flds)
        (let loop ([flds flds])
          (let ([fld (car flds)])
            (if (eq? (fld-name fld) fn)
                (fld-byte fld)
                (loop (cdr flds)))))))
    (define parse-field
      (lambda (field)
        (syntax-case field (constant)
          [(field-type field-name)
           (list #'field-type #'field-name #f)]
          [(field-type field-name n)
           (integer? (datum n))
           (list #'field-type #'field-name (datum n))]
          [(field-type field-name (constant sym))
           (list #'field-type #'field-name
                 (lookup-constant (datum sym)))])))
    (syntax-case x ()
      [(_ name type (field1 field2 ...))
       (andmap identifier? #'(name type))
       (with-syntax ([((field-type field-name field-length) ...)
                      (map parse-field #'(field1 field2 ...))])
         (with-values (compute-field-offsets 'define-primitive-structure-disps
                        (- (constant typemod) (lookup-constant (datum type)))
                        (map (lambda (type name len)
                               (list (filter-scheme-type type)
                                     name
                                     (or len 1)))
                             (datum (field-type ...))
                             (datum (field-name ...))
                             #'(field-length ...)))
           (lambda (pm mpm flds size)
             (let ([var? (eq? (car (last-pair #'(field-length ...))) 0)])
               (with-syntax ([(name-field-disp ...)
                              (map (lambda (fn)
                                     (make-name-field-disp #'name fn))
                                   (datum (field-name ...)))]
                             [(field-disp ...)
                              (map (lambda (fn) (get-fld-byte fn flds))
                                   (datum (field-name ...)))]
                             [size (if var? size (c-alloc-align size))]
                             [size-name
                              (construct-name
                                #'name
                                (if var? "header-size-" "size-")
                                #'name)])
                 #'(begin
                     (putprop
                       'name
                       '*fields*
                       (map list
                            '(field-name ...)
                            '(field-type ...)
                            '(field-disp ...)
                            '(field-length ...)))
                     (define-constant size-name size)
                     (define-constant name-field-disp field-disp)
                     ...))))))])))

(define-primitive-structure-disps typed-object type-typed-object
  ([iptr type]))

(define-primitive-structure-disps pair type-pair
  ([ptr car]
   [ptr cdr]))

(define-constant pair-shift (log2 (constant size-pair)))

(define-primitive-structure-disps box type-typed-object
  ([iptr type]
   [ptr ref]))

(define-primitive-structure-disps ephemeron type-pair
  ([ptr car]
   [ptr cdr]
   [ptr next] ; `next` is needed by the GC to keep track of pending ephemerons
   [ptr trigger-next])) ; `trigger-next` is similar, but for segment-specific lists

(define-primitive-structure-disps tlc type-typed-object
  ([iptr type]
   [ptr keyval]
   [ptr ht]
   [ptr next]))

(define-primitive-structure-disps symbol type-symbol
  ([ptr value]
   [ptr pvalue]
   [ptr plist]
   [ptr name]
   [ptr splist]
   [ptr hash]))

(define-primitive-structure-disps ratnum type-typed-object
  ([iptr type]
   [ptr numerator]
   [ptr denominator]))

(define-primitive-structure-disps vector type-typed-object
  ([iptr type]
   [ptr data 0]))

(define-primitive-structure-disps fxvector type-typed-object
  ([iptr type]
   [ptr data 0]))

(constant-case ptr-bits
  [(32)
   (define-primitive-structure-disps bytevector type-typed-object
     ([iptr type]
      [ptr pad] ; pad needed to maintain double-word alignment for data
      [octet data 0]))]
  [(64)
   (define-primitive-structure-disps bytevector type-typed-object
     ([iptr type]
      [octet data 0]))])

; WARNING: implementation of real-part and imag-part assumes that
; flonums are subobjects of inexactnums.
(define-primitive-structure-disps flonum type-flonum
  ([double data]))

; on 32-bit systems, the iptr pad will have no effect above and
; beyond the normal padding.  on 64-bit systems, the pad
; guarantees that the forwarding address will not overwrite
; real-part, which may share storage with a flonum that has
; not yet been forwarded.
(define-primitive-structure-disps inexactnum type-typed-object
  ([iptr type]
   [iptr pad]
   [double real]
   [double imag]))

(define-primitive-structure-disps exactnum type-typed-object
  ([iptr type]
   [ptr real]
   [ptr imag]))

(define-primitive-structure-disps closure type-closure
  ([ptr code]
   [ptr data 0]))

(define-primitive-structure-disps port type-typed-object
  ([iptr type]
   [ptr handler]
   [iptr ocount]
   [iptr icount]
   [ptr olast]
   [ptr obuffer]
   [ptr ilast]
   [ptr ibuffer]
   [ptr info]
   [ptr name]))

(define-primitive-structure-disps string type-typed-object
  ([iptr type]
   [string-char data 0]))

(define-primitive-structure-disps bignum type-typed-object
  ([iptr type]
   [bigit data 0]))

(define-primitive-structure-disps code type-typed-object
  ([iptr type]
   [iptr length]
   [ptr reloc]
   [ptr name]
   [ptr arity-mask]
   [iptr closure-length]
   [ptr info]
   [ptr pinfo*]
   [octet data 0]))

(define-primitive-structure-disps reloc-table typemod
  ([iptr size]
   [ptr code]
   [uptr data 0]))

(define-primitive-structure-disps continuation type-closure
  ([ptr code]
   [ptr stack]
   [iptr stack-length]
   [iptr stack-clength]
   [ptr link]
   [ptr return-address]
   [ptr winders]))

(define-primitive-structure-disps record type-typed-object
  ([ptr type]
   [ptr data 0]))

(define-primitive-structure-disps thread type-typed-object
  ([ptr type] [uptr tc]))

(define-constant virtual-register-count 16)

;;; make sure gc sweeps all ptrs
(define-primitive-structure-disps tc typemod
  ([void* arg-regs (constant asm-arg-reg-max)]
   [void* ac0]
   [void* ac1]
   [void* sfp]
   [void* cp]
   [void* esp]
   [void* ap]
   [void* eap]
   [void* ret]
   [void* trap]
   [void* xp]
   [void* yp]
   [void* ts]
   [void* td]
   [void* real_eap]
   [ptr virtual-registers (constant virtual-register-count)]
   [ptr guardian-entries]
   [ptr cchain]
   [ptr code-ranges-to-flush]
   [U32 random-seed]
   [I32 active]
   [void* scheme-stack]
   [ptr stack-cache]
   [ptr stack-link]
   [iptr scheme-stack-size]
   [ptr winders]
   [ptr U]
   [ptr V]
   [ptr W]
   [ptr X]
   [ptr Y]
   [ptr something-pending]
   [ptr timer-ticks]
   [ptr disable-count]
   [ptr signal-interrupt-pending]
   [ptr keyboard-interrupt-pending]
   [ptr threadno]
   [ptr current-input]
   [ptr current-output]
   [ptr current-error]
   [ptr block-counter]
   [ptr sfd]
   [ptr current-mso]
   [ptr target-machine]
   [ptr fxlength-bv]
   [ptr fxfirst-bit-set-bv]
   [ptr null-immutable-vector]
   [ptr null-immutable-fxvector]
   [ptr null-immutable-bytevector]
   [ptr null-immutable-string]
   [ptr meta-level]
   [ptr compile-profile]
   [ptr generate-inspector-information]
   [ptr generate-profile-forms]
   [ptr optimize-level]
   [ptr subset-mode]
   [ptr suppress-primitive-inlining]
   [ptr default-record-equal-procedure]
   [ptr default-record-hash-procedure]
   [U64 instr-counter]
   [U64 alloc-counter]
   [ptr parameters]))

(define tc-field-list
  (let f ([ls (oblist)] [params '()])
    (if (null? ls)
        params
        (f (cdr ls)
           (let* ([sym (car ls)]
                  [str (symbol->string sym)]
                  [n (string-length str)])
             (if (and (> n 8)
                      (string=? (substring str 0 3) "tc-")
                      (string=? (substring str (- n 5) n) "-disp")
                      (getprop sym '*constant* #f))
                 (cons (string->symbol (substring str 3 (- n 5))) params)
                 params))))))

(define-constant unactivate-mode-noop       0)
(define-constant unactivate-mode-deactivate 1)
(define-constant unactivate-mode-destroy    2)

(define-primitive-structure-disps rtd-counts type-typed-object
  ([iptr type]
   [U64 timestamp]
   [uptr data 256]))

(define-primitive-structure-disps record-type type-typed-object
  ([ptr type]
   [ptr parent]
   [ptr size]
   [ptr pm]
   [ptr mpm]
   [ptr name]
   [ptr flds]
   [ptr flags]
   [ptr uid]
   [ptr counts]))

(define-constant rtd-generative #b0001)
(define-constant rtd-opaque     #b0010)
(define-constant rtd-sealed     #b0100)

; we do this as a macro here since we want the freshest version possible
; in syntax.ss when we use it as a patch, whereas we want the old
; version in non-patched record.ss, so he can operate on host-system
; record types.
(define-syntax make-record-call-args
  (identifier-syntax
    (lambda (flds size e*)
      (let f ((flds flds) (b (constant record-data-disp)) (e* e*))
        (if (null? flds)
            (if (< b (+ size (constant record-type-disp)))
                (cons 0 (f flds (+ b (constant ptr-bytes)) e*))
                '())
            (let ((fld (car flds)))
              (cond
                [(< b (fld-byte fld))
                 (cons 0 (f flds (+ b (constant ptr-bytes)) e*))]
                [(> b (fld-byte fld))
                 (f (cdr flds) b (cdr e*))]
                [else ; (= b (fld-byte fld))
                 (cons (if (eq? (filter-foreign-type (fld-type fld)) 'scheme-object) (car e*) 0)
                       (f (cdr flds)
                          (+ b (constant ptr-bytes))
                          (cdr e*)))])))))))

(define-primitive-structure-disps guardian-entry typemod
  ([ptr obj]
   [ptr rep]
   [ptr tconc]
   [ptr next]))

;;; forwarding addresses are recorded with a single forward-marker
;;; bit pattern (a special Scheme object) followed by the forwarding
;;; address, a ptr to the forwarded object.
(define-primitive-structure-disps forward typemod
  ([ptr marker]
   [ptr address]))

(define-primitive-structure-disps cached-stack typemod
  ([iptr size]
   [ptr link]))

(define-primitive-structure-disps rp-header typemod
  ([ptr livemask]
   [uptr toplink]
   [iptr frame-size]
   [uptr mv-return-address]))
(define-constant return-address-mv-return-address-disp
  (- (constant rp-header-mv-return-address-disp) (constant size-rp-header)))
(define-constant return-address-frame-size-disp
  (- (constant rp-header-frame-size-disp) (constant size-rp-header)))
(define-constant return-address-toplink-disp
  (- (constant rp-header-toplink-disp) (constant size-rp-header)))
(define-constant return-address-livemask-disp
  (- (constant rp-header-livemask-disp) (constant size-rp-header)))

(define-syntax bigit-type
  (lambda (x)
    (with-syntax ([type (datum->syntax #'* (filter-scheme-type 'bigit))])
      #''type)))

(define-syntax string-char-type
  (lambda (x)
    (with-syntax ([type (datum->syntax #'* (filter-scheme-type 'string-char))])
      #''type)))

(define-constant annotation-debug 1)
(define-constant annotation-profile 2)

(eval-when (compile load eval)
(define flag->mask
  (lambda (m e)
    (cond
      [(fixnum? m) m]
      [(and (symbol? m) (assq m e)) => cdr]
      [(and (list? m) (eq? (car m) 'or))
       (let f ((ls (cdr m)))
         (if (null? ls)
             0
             (fxlogor (flag->mask (car ls) e) (f (cdr ls)))))]
      [(and (list? m) (eq? (car m) 'sll) (= (length m) 3))
       (fxsll (flag->mask (cadr m) e) (lookup-constant (caddr m)))]
      [else ($oops 'flag->mask "invalid mask ~s" m)])))
)

(define-syntax define-flags
  (lambda (exp)
    (define mask-environment
      (lambda (flags masks)
        (let f ((flags flags) (masks masks) (e '()))
          (if (null? flags)
              e
              (let ((mask (flag->mask (car masks) e)))
                (f (cdr flags) (cdr masks)
                   (cons `(,(car flags) . ,mask) e)))))))
    (syntax-case exp ()
      ((_k name (flag mask) ...)
       (with-syntax ((env (datum->syntax #'_k
                            (mask-environment
                              (datum (flag ...))
                              (datum (mask ...))))))
         #'(define-syntax name
             (lambda (x)
               (syntax-case x ()
                 ((_k f (... ...))
                  (datum->syntax #'_k
                    (flag->mask `(or ,@(datum (f (... ...)))) 'env)))))))))))

(define-syntax any-set?
  (syntax-rules ()
    ((_ mask x)
     (not (fx= (fxlogand mask x) 0)))))

(define-syntax all-set?
  (syntax-rules ()
    ((_ mask x)
     (let ((m mask)) (fx= (fxlogand m x) m)))))

(define-syntax set-flags
  (syntax-rules ()
    ((_ mask x)
     (fxlogor mask x))))

(define-syntax reset-flags
  (syntax-rules ()
    ((_ mask x)
     (fxlogand (fxlognot mask) x))))

;;; prim-mask notes:
;;;  - pure prim can (but need not) return same (by eqv?) value for same
;;;    (by eqv?) args and causes no side effects
;;;  - pure is not set when primitive can cause an effect, observe an effect,
;;;    or allocate a mutable object.  So set-car!, car, cons, equal?, and
;;;    list? are not pure, while pair?, +, <, and char->integer are pure.
;;;  - an mifoldable primitive can be folded in a machine-independent way
;;;    when it gets constant arguments.  we don't fold primitives that depend
;;;    on machine characteristics, like most-positive-fixnum.  (but we do
;;;    have cp0 handlers for almost all of them that do the right thing.)
;;;  - mifoldable does not imply pure.  can fold car when it gets a constant
;;;    (and thus immutable) argument, but it is not pure.
;;;  - pure does not imply mifoldable, since a pure primitive might not be
;;;    machine-independent.
(define-flags prim-mask
  (system                   #b00000000000000000000001)
  (primitive                #b00000000000000000000010)
  (keyword                  #b00000000000000000000100)
  (r5rs                     #b00000000000000000001000)
  (ieee                     #b00000000000000000010000)
  (proc                     #b00000000000000000100000)
  (discard                  #b00000000000000001000000)
  (unrestricted             #b00000000000000010000000)
  (true                     #b00000000000000100000000)
  (mifoldable               #b00000000000001000000000)
  (cp02                     #b00000000000010000000000)
  (cp03                     #b00000000000100000000000)
  (system-keyword           #b00000000001000000000000)
  (r6rs                     #b00000000010000000000000)
  (pure                 (or #b00000000100000000000000 discard))
  (library-uid              #b00000001000000000000000)
  (boolean-valued           #b00000010000000000000000)
  (abort-op                 #b00000100000000000000000)
  (unsafe                   #b00001000000000000000000)
  (arith-op                 (or proc pure true))
  (alloc                    (or proc discard true))
  ; would be nice to check that these and only these actually have cp0 partial folders
  (partial-folder           (or cp02 cp03))
)

(define-flags cp0-info-mask
  (pure-known                    #b0000000001)
  (pure                          #b0000000010)
  (ivory-known                   #b0000000100)
  (ivory                         #b0000001000)
  (simple-known                  #b0000010000)
  (simple                        #b0000100000)
  (boolean-valued-known          #b0001000000)
  (boolean-valued                #b0010000000)
)

(define-syntax define-flag-field
  (lambda (exp)
    (syntax-case exp ()
      ((k struct field (flag mask) ...)
       (let ()
         (define getter-name
           (lambda (f)
             (construct-name #'k #'struct "-" f)))
         (define setter-name
           (lambda (f)
             (construct-name #'k "set-" #'struct "-" f "!")))
         (with-syntax ((field-ref (getter-name #'field))
                       (field-set! (construct-name #'k #'struct "-" #'field "-set!"))
                       ((flag-ref ...) (map getter-name #'(flag ...)))
                       ((flag-set! ...) (map setter-name #'(flag ...)))
                       (f->m (construct-name #'k #'struct "-" #'field
                               "-mask")))
           #'(begin
               (define-flags f->m (flag mask) ...)
               (define-syntax flag-ref
                 (lambda (x)
                   (syntax-case x ()
                     ((kk x) (with-implicit (kk field-ref)
                               #'(any-set? (f->m flag) (field-ref x)))))))
               ...
               (define-syntax flag-set!
                 (lambda (x)
                   (syntax-case x ()
                     ((kk x bool)
                      (with-implicit (kk field-ref field-set!)
                        #'(let ((t x))
                            (field-set! t
                              (if bool
                                  (set-flags (f->m flag) (field-ref t))
                                  (reset-flags (f->m flag) (field-ref t))))))))))
               ...)))))))

;;; compile-time-environment structures

(define-constant prelex-is-flags-offset 8)
(define-constant prelex-was-flags-offset 16)
(define-constant prelex-sticky-mask         #b11111111)
(define-constant prelex-is-mask     #b1111111100000000)

(define-flag-field prelex flags
 ; sticky flags:
  (immutable-value      #b0000000000000001)
 ; is flags:
  (assigned             #b0000000100000000)
  (referenced           #b0000001000000000)
  (seen                 #b0000010000000000)
  (multiply-referenced  #b0000100000000000)
 ; was flags:
  (was-assigned         (sll assigned prelex-was-flags-offset))
  (was-referenced       (sll referenced prelex-was-flags-offset))
  (was-multiply-referenced   (sll multiply-referenced prelex-was-flags-offset))
 ; aggregate flags:
  (seen/referenced      (or seen referenced))
  (seen/assigned        (or seen assigned))
  (referenced/assigned  (or referenced assigned))
)

(macro-define-structure ($c-func)
  ([code-record #f]      ; (code func free ...)
   [code-object #f]      ; actual code object created by c-mkcode
   [closure-record #f]   ; (closure . func), if constant
   [closure #f]))        ; actual closure created by c-mkcode, if constant

(define-syntax negated-flonum?
  (syntax-rules ()
    ((_ x) (fx= ($flonum-sign x) 1))))

(define-syntax $nan?
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (float-type-case
         [(ieee) (not (fl= x x))])))))

(define-syntax infinity?
  (syntax-rules ()
    ((_ e)
     (let ([x e])
       (float-type-case
         [(ieee) (and (exceptional-flonum? x) (not ($nan? x)))])))))

(define-syntax exceptional-flonum?
  (syntax-rules ()
    ((_ x)
     (float-type-case
       [(ieee) (fx= ($flonum-exponent x) #x7ff)]))))

(define-syntax on-reset
  (syntax-rules ()
    ((_ oops e1 e2 ...)
     ($reset-protect (lambda () e1 e2 ...) (lambda () oops)))))

(define-syntax $make-thread-parameter
  (if-feature pthreads
    (identifier-syntax make-thread-parameter)
    (identifier-syntax make-parameter)))

(define-syntax define-threaded
  (if-feature pthreads
    (syntax-rules ()
      [(_ var) (define-threaded var 'var)]
      [(_ var expr)
       (begin
         (define tmp ($make-thread-parameter expr))
         (define-syntax var
           (identifier-syntax
             (id (tmp))
             ((set! id val) (tmp val)))))])
    (identifier-syntax define)))

(define-syntax define-syntactic-monad
  (lambda (x)
    (syntax-case x ()
      ((_ name formal ...)
       (andmap identifier? #'(name formal ...))
       #'(define-syntax name
          (lambda (x)
            (syntax-case x (lambda define)
              ((key lambda more-formals . body)
               (with-implicit (key formal ...)
                 #'(lambda (formal ... . more-formals) . body)))
              ((key define (proc-name . more-formals) . body)
               (with-implicit (key formal ...)
                 #'(define proc-name (lambda (formal ... . more-formals) . body))))
              ((key proc ((x e) (... ...)) arg (... ...))
               (andmap identifier? #'(x (... ...)))
               (with-implicit (key formal ...)
                 (for-each
                   (lambda (x)
                     (unless (let mem ((ls #'(formal ...)))
                               (and (not (null? ls))
                                    (or (free-identifier=? x (car ls))
                                        (mem (cdr ls)))))
                       (syntax-error x (format "undeclared ~s monad binding" 'name))))
                   #'(x (... ...)))
                 #'(let ((x e) (... ...))
                     (proc formal ... arg (... ...)))))
              ((key proc) #'(key proc ())))))))))

(define-syntax make-binding
  (syntax-rules ()
    ((_ type value) (cons type value))))
(define-syntax binding-type (syntax-rules () ((_ b) (car b))))
(define-syntax binding-value (syntax-rules () ((_ b) (cdr b))))
(define-syntax set-binding-type!
  (syntax-rules ()
    ((_ b v) (set-car! b v))))
(define-syntax set-binding-value!
  (syntax-rules ()
    ((_ b v) (set-cdr! b v))))
(define-syntax binding?
  (syntax-rules ()
    ((_ x) (let ((t x)) (and (pair? t) (symbol? (car t)))))))

;;; heap/stack mangement constants

(define-constant collect-interrupt-index 1)
(define-constant timer-interrupt-index 2)
(define-constant keyboard-interrupt-index 3)
(define-constant signal-interrupt-index 4)
(define-constant maximum-interrupt-index 4)

(define-constant ignore-event-flag 0)

(define-constant default-timer-ticks 1000)
(define-constant default-collect-trip-bytes
  (expt 2 (+ 20 (constant log2-ptr-bytes))))
(define-constant default-heap-reserve-ratio 1.0)
(define-constant static-generation 255)
(define-constant default-max-nonstatic-generation 4)

(constant-case address-bits
  [(32)
   (constant-case segment-table-levels
     [(1) (define-constant segment-t1-bits 19)]    ; table size: .5M words = 2M bytes
     [(2) (define-constant segment-t2-bits 9)      ; outer-table size: .5k words = 2k bytes
          (define-constant segment-t1-bits 10)])   ; inner-table size: 1k words = 4k bytes
   (define-constant segment-offset-bits 13)        ; segment size: 8k bytes (2k ptrs)
   (define-constant card-offset-bits 8)]           ; card size: 256 bytes (64 ptrs)
  [(64)
   (constant-case segment-table-levels
     [(2) (define-constant segment-t2-bits 25)     ; outer-table size: 32M words = 268M bytes
          (define-constant segment-t1-bits 25)]    ; inner-table size: 32M words = 268M bytes
     [(3) (define-constant segment-t3-bits 17)     ; outer-table size: 128k words = 1M bytes
          (define-constant segment-t2-bits 17)     ; middle-table size: 128k words = 1M bytes
          (define-constant segment-t1-bits 16)])   ; inner-table size: 64k words = 512k bytes
   (define-constant segment-offset-bits 14)        ; segment size: 16k bytes (2k ptrs)
   (define-constant card-offset-bits 9)])          ; card size: 512 bytes (64 ptrs)

(define-constant bytes-per-segment (ash 1 (constant segment-offset-bits)))
(define-constant segment-card-offset-bits (- (constant segment-offset-bits) (constant card-offset-bits)))
;;; cards-per-segment must be a multiple of ptr-bits, since gc sometimes
;;; processes dirty bytes in iptr-sized pieces
(define-constant cards-per-segment (ash 1 (constant segment-card-offset-bits)))
(define-constant bytes-per-card (ash 1 (constant card-offset-bits)))

;;; minimum-segment-request is the minimum number of segments
;;; requested from the O/S when Scheme runs out of memory.
(define-constant minimum-segment-request 128)

;;; alloc_waste_maximum determines the maximum amount wasted if a large
;;; object request or remembered-set scan request is made from Scheme
;;; (through S_get_more_room or S_scan_remembered_set).  if more than
;;; alloc_maximum_waste bytes remain between ap and eap, ap is left
;;; unchanged.
(define-constant alloc-waste-maximum (ash (constant bytes-per-segment) -3))

;;; default-stack-size determines the length in bytes of the runtime stack
;;; used for execution of scheme programs.  Since the stack is extended
;;; automatically by copying part of the stack into a continuation,
;;; it is not necessary to make the number very large, except for
;;; efficiency.  Since the cost of invoking continuations is bounded by
;;; default-stack-size, it should not be made excessively large.
;;; stack-slop determines how much of the stack is available for routines
;;; that use a bounded amount of stack space, and thus don't need to
;;; check for stack overflow.

;; Make default stack size a multiple of the segment size, but leave room for
;; two ptrs at the end (a forward marker and a pointer to the next segment of
;; this type --- used by garbage collector).
(define-constant default-stack-size
  (- (* 4 (constant bytes-per-segment)) (* 2 (constant ptr-bytes))))
(define-constant stack-slop (ceiling (/ (constant default-stack-size) 64)))
(define-constant stack-frame-limit (fxsrl (constant stack-slop) 1))
;; one-shot-headroom must include stack-slop so min factor below is 2
(define-constant one-shot-headroom (fx* (constant stack-slop) 3))
;; shot-1-shot-flag is inserted into continuation length field to mark
;; a one-shot continuation shot.  it must look like a negative byte
;; offset
(define-constant unscaled-shot-1-shot-flag -1)
(define-constant scaled-shot-1-shot-flag
  (* (constant unscaled-shot-1-shot-flag) (constant ptr-bytes)))

;;; underflow limit determines how much we're willing to copy on
;;; stack underflow/continuation invocation
(define-constant underflow-limit (* (constant ptr-bytes) 16))

;;; check assumptions
(let ([x (fxsrl (constant type-char)
           (fx- (constant char-data-offset)
                (constant fixnum-offset)))])
  (unless (fx= (fxlogand x (constant mask-fixnum)) (constant type-fixnum))
    ($oops 'cmacros.ss
      "expected type-char/fixnum relationship does not hold")))

(define-syntax with-tc-mutex
  (if-feature pthreads
    (syntax-rules ()
      [(_ e1 e2 ...)
       (dynamic-wind
         (lambda () (disable-interrupts) (mutex-acquire $tc-mutex))
         (lambda () e1 e2 ...)
         (lambda () (mutex-release $tc-mutex) (enable-interrupts)))])
    (identifier-syntax critical-section)))

(define-constant hashtable-default-size 8)

(define-constant eq-hashtable-subtype-normal 0)
(define-constant eq-hashtable-subtype-weak 1)
(define-constant eq-hashtable-subtype-ephemeron 2)

; keep in sync with make-date
(define-constant dtvec-nsec 0)
(define-constant dtvec-sec 1)
(define-constant dtvec-min 2)
(define-constant dtvec-hour 3)
(define-constant dtvec-mday 4)
(define-constant dtvec-mon 5)
(define-constant dtvec-year 6)
(define-constant dtvec-wday 7)
(define-constant dtvec-yday 8)
(define-constant dtvec-isdst 9)
(define-constant dtvec-tzoff 10)
(define-constant dtvec-tzname 11)
(define-constant dtvec-size 12)

(define-constant time-process 0)
(define-constant time-thread 1)
(define-constant time-duration 2)
(define-constant time-monotonic 3)
(define-constant time-utc 4)
(define-constant time-collector-cpu 5)
(define-constant time-collector-real 6)

(define-syntax make-winder
  (syntax-rules ()
    [(_ critical? in out) (vector critical? in out)]))
(define-syntax winder-critical? (syntax-rules () [(_ w) (vector-ref w 0)]))
(define-syntax winder-in (syntax-rules () [(_ w) (vector-ref w 1)]))
(define-syntax winder-out (syntax-rules () [(_ w) (vector-ref w 2)]))

(define-syntax winder?
  (syntax-rules ()
    [(_ ?w)
     (let ([w ?w])
       (and (vector? w)
            (fx= (vector-length w) 3)
            (boolean? (winder-critical? w))
            (procedure? (winder-in w))
            (procedure? (winder-out w))))]))

(define-syntax default-run-cp0
  (lambda (x)
    (syntax-case x ()
      [(k) (datum->syntax #'k '(lambda (cp0 x) (cp0 (cp0 x))))])))

;;; A state-case expression must take the following form:
;;;   (state-case var eof-clause clause ... else-clause)
;;; eof-clause and else-clause must take the form
;;;   (eof exp1 exp2 ...)
;;;   (else exp1 exp2 ...)
;;; and the remaining clauses must take the form
;;;   (char-set exp1 exp2 ...)
;;; The value of var must be an eof object or a character.
;;; state-case selects the first clause matching the value of var and
;;; evaluates the expressions exp1 exp2 ... of that clause.  If the
;;; value of var is an eof-object, eof-clause is selected.  Otherwise,
;;; the clauses clause ... are considered from left to right.  If the
;;; value of var is in the set of characters defined by the char-set of
;;; a given clause, the clause is selected.  If no other clause is
;;; selected, else-clause is selected.

;;; char-set may be
;;;   * a single character, e.g., #\a, or
;;;   * a list of subkeys, each of which is
;;;     - a single character, or
;;;     - a character range, e.g., (#\a - #\z)
;;; For example, (#\$ (#\a - #\z) (#\A - #\Z)) specifies the set
;;; containing $ and the uppercase and lowercase letters.
(define-syntax state-case
  (lambda (x)
    (define state-case-test
      (lambda (cvar k)
        (with-syntax ((cvar cvar))
          (syntax-case k (-)
            (char
             (char? (datum char))
             #'(char=? cvar char))
            ((char1 - char2)
             (and (char? (datum char1)) (char? (datum char2)))
             #'(char<=? char1 cvar char2))
            (predicate
             (identifier? #'predicate)
             #'(predicate cvar))))))
    (define state-case-help
      (lambda (cvar clauses)
        (syntax-case clauses (else)
          (((else exp1 exp2 ...))
           #'(begin exp1 exp2 ...))
          ((((k ...) exp1 exp2 ...) . more)
           (with-syntax (((test ...)
                          (map (lambda (k) (state-case-test cvar k))
                               #'(k ...)))
                         (rest (state-case-help cvar #'more)))
             #'(if (or test ...) (begin exp1 exp2 ...) rest)))
          (((k exp1 exp2 ...) . more)
           (with-syntax ((test (state-case-test cvar #'k))
                         (rest (state-case-help cvar #'more)))
             #'(if test (begin exp1 exp2 ...) rest))))))
    (syntax-case x (eof)
      ((_ cvar (eof exp1 exp2 ...) more ...)
       (identifier? #'cvar)
       (with-syntax ((rest (state-case-help #'cvar #'(more ...))))
         #'(if (eof-object? cvar)
               (begin exp1 exp2 ...)
               rest))))))

;; the following (old) version of state-case creates a set of vectors sc1, ...
;; corresponding to each state-case in the file and records the frequency
;; with which each clause (numbered from 0) matches.  this is how the reader
;; is "tuned".
;   (let ([n 0])
;      (extend-syntax (state-case)
;         [(state-case exp more ...)
;          (with ([cvar (gensym)]
;                 [statvar (string->symbol (format "sc~a" (set! n (1+ n))))]
;                 [size (length '(more ...))])
;             (let ([cvar exp])
;                (unless (top-level-bound? 'statvar)
;                   (printf "creating ~s~%" 'statvar)
;                   (set! statvar (make-vector size 0)))
;                (state-case-help statvar 0 cvar more ...)))]))
;
;   (extend-syntax (state-case-help else)
;      [(state-case-help svar i cvar) (rd-character-error cvar)]
;      [(state-case-help svar i cvar [else exp1 exp2 ...])
;       (if (char<=? #\nul cvar #\rubout)
;           (begin (vector-set! svar i (1+ (vector-ref svar i))) exp1 exp2 ...)
;           (rd-character-error cvar))]
;      [(state-case-help svar i cvar [(k1 ...) exp1 exp2 ...] more ...)
;       (if (or (state-case-test cvar k1) ...)
;           (begin (vector-set! svar i (1+ (vector-ref svar i))) exp1 exp2 ...)
;           (with ([i (1+ 'i)])
;              (state-case-help svar i cvar more ...)))]
;      [(state-case-help svar i cvar [k1 exp1 exp2 ...] more ...)
;       (if (state-case-test cvar k1)
;           (begin (vector-set! svar i (1+ (vector-ref svar i))) exp1 exp2 ...)
;           (with ([i (1+ 'i)])
;              (state-case-help svar i cvar more ...)))])

(define-syntax message-lambda
  (lambda (x)
    (define (group i* clause*)
      (let* ([n (fx+ (apply fxmax -1 i*) 1)] [v (make-vector n '())])
        (for-each
          (lambda (i clause)
            (vector-set! v i (cons clause (vector-ref v i))))
          i* clause*)
        (let f ([i 0])
          (if (fx= i n)
              '()
              (let ([ls (vector-ref v i)])
                (if (null? ls)
                    (f (fx+ i 1))
                    (cons (reverse ls) (f (fx+ i 1)))))))))
    (syntax-case x ()
      [(_ ?err [(k arg ...) b1 b2 ...] ...)
       (let ([clause** (group (map length #'((arg ...) ...))
                              #'([(k arg ...) b1 b2 ...] ...))])
         #`(let ([err ?err])
             (case-lambda
               #,@(map (lambda (clause*)
                         (with-syntax ([([(k arg ...) b1 b2 ...] ...) clause*]
                                       [(t0 t1 ...)
                                        (with-syntax ([([(k arg ...) . body] . rest) clause*])
                                          (generate-temporaries #'(k arg ...)))])
                           #'[(t0 t1 ...)
                              (case t0
                                [(k) (let ([arg t1] ...) b1 b2 ...)]
                                ...
                                [else (err t0 t1 ...)])]))
                    clause**)
               [(msg . args) (apply err msg args)])))])))

(define-syntax set-who!
  (lambda (x)
    (syntax-case x ()
      [(k #(prefix id) e)
       (and (identifier? #'prefix) (identifier? #'id))
       (with-implicit (k who)
         (with-syntax ([ext-id (construct-name #'id #'prefix #'id)])
           #'(set! ext-id (let ([who 'id]) (rec id e)))))]
      [(k id e)
       (identifier? #'id)
       (with-implicit (k who)
         #'(set! id (let ([who 'id]) e)))])))

(define-syntax define-who
  (lambda (x)
    (syntax-case x ()
      [(k (id . args) b1 b2 ...)
       #'(k id (lambda args b1 b2 ...))]
      [(k #(prefix id) e)
       (and (identifier? #'prefix) (identifier? #'id))
       (with-implicit (k who)
         (with-syntax ([ext-id (construct-name #'id #'prefix #'id)])
           #'(define ext-id (let ([who 'id]) (rec id e)))))]
      [(k id e)
       (identifier? #'id)
       (with-implicit (k who)
         #'(define id (let ([who 'id]) e)))])))

(define-syntax trace-define-who
  (lambda (x)
    (syntax-case x ()
      [(k (id . args) b1 b2 ...)
       #'(k id (lambda args b1 b2 ...))]
      [(k id e)
       (identifier? #'id)
       (with-implicit (k who)
         #'(trace-define id (let ([who 'id]) e)))])))

(define-syntax safe-assert
  (lambda (x)
    (syntax-case x ()
      [(_ e1 e2 ...)
       (if (fx= (debug-level) 0)
           #'(void)
           #'(begin (assert e1) (assert e2) ...))])))

(define-syntax self-evaluating?
  (syntax-rules ()
    [(_ ?x)
     (let ([x ?x])
       (or (number? x)
           (boolean? x)
           (char? x)
           (string? x)
           (bytevector? x)
           (fxvector? x)
           (memq x '(#!eof #!bwp #!base-rtd))))]))

;;; datatype support
(define-syntax define-datatype
  (lambda (x)
    (define iota
      (case-lambda
        [(n) (iota 0 n)]
        [(i n) (if (= n 0) '() (cons i (iota (+ i 1) (- n 1))))]))
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
    (syntax-case x ()
      [(_ dtname (vname field ...) ...)
       (identifier? #'dtname)
       #'(define-datatype (dtname) (vname field ...) ...)]
      [(_ (dtname dtfield-spec ...) (vname field ...) ...)
       (and (andmap identifier? #'(vname ...)) (andmap identifier? #'(field ... ...)))
       (let ()
         (define split-name
           (lambda (x)
             (let ([sym (syntax->datum x)])
               (if (gensym? sym)
                   (cons (datum->syntax x (string->symbol (symbol->string sym))) x)
                   (cons x (datum->syntax x (gensym (symbol->string sym))))))))
         (with-syntax ([(dtname . dtuid) (split-name #'dtname)]
                       [((vname . vuid) ...) (map split-name #'(vname ...))]
                       [(dtfield ...)
                        (map (lambda (spec)
                               (syntax-case spec (immutable mutable)
                                 [(immutable name) (identifier? #'name) #'name]
                                 [(mutable name) (identifier? #'name) #'name]
                                 [_ (syntax-error spec "invalid datatype field specifier")]))
                             #'(dtfield-spec ...))])
           (with-syntax ([dtname? (construct-name #'dtname #'dtname "?")]
                         [dtname-case (construct-name #'dtname #'dtname "-case")]
                         [dtname-variant (construct-name #'dtname #'dtname "-variant")]
                         [(dtname-dtfield ...)
                          (map (lambda (field)
                                 (construct-name #'dtname #'dtname "-" field))
                               #'(dtfield ...))]
                         [(dtname-dtfield-set! ...)
                          (fold-right
                            (lambda (dtf ls)
                              (syntax-case dtf (mutable immutable)
                                [(immutable name) ls]
                                [(mutable name) (cons (construct-name #'dtname #'dtname "-" #'name "-set!") ls)]))
                            '()
                            #'(dtfield-spec ...))]
                         [((vname-field ...) ...)
                          (map (lambda (vname fields)
                                 (map (lambda (field)
                                        (construct-name #'dtname
                                          vname "-" field))
                                      fields))
                               #'(vname ...)
                               #'((field ...) ...))]
                         [(raw-make-vname ...)
                          (map (lambda (x)
                                 (construct-name #'dtname
                                   "make-" x))
                               #'(vname ...))]
                         [(make-vname ...)
                          (map (lambda (x)
                                 (construct-name #'dtname
                                   #'dtname "-" x))
                               #'(vname ...))]
                        ; wash away gensyms for dtname-case
                         [(pretty-vname ...)
                          (map (lambda (vname)
                                 (construct-name vname vname))
                               #'(vname ...))]
                         [(i ...) (iota (length #'(vname ...)))]
                         [((fvar ...) ...) (map generate-temporaries #'((field ...) ...))])
             #'(module (dtname? (dtname-case dtname-variant vname-field ... ...) dtname-dtfield ... dtname-dtfield-set! ... make-vname ...)
                 (define-record-type dtname
                   (nongenerative dtuid)
                   (fields (immutable variant) dtfield-spec ...))
                 (module (make-vname vname-field ...)
                   (define-record-type (vname make-vname vname?)
                     (nongenerative vuid)
                     (parent dtname)
                     (fields (immutable field) ...)
                     (protocol
                       (lambda (make-new)
                         (lambda (dtfield ... field ...)
                           ((make-new i dtfield ...) field ...))))))
                 ...
                 (define-syntax dtname-case
                   (lambda (x)
                     (define make-clause
                       (lambda (x)
                         (syntax-case x (pretty-vname ...)
                           [(pretty-vname (fvar ...) e1 e2 (... ...))
                            #'((i) (let ([fvar (vname-field t)] ...)
                                     e1 e2 (... ...)))]
                           ...)))
                     (syntax-case x (else)
                       [(__ e0
                            (v (fld (... ...)) e1 e2 (... ...))
                            (... ...)
                            (else e3 e4 (... ...)))
                       ; could discard else clause if all variants are mentioned
                        (with-syntax ([(clause (... ...))
                                       (map make-clause
                                            #'((v (fld (... ...)) e1 e2 (... ...))
                                               (... ...)))])
                          #'(let ([t e0])
                              (case (dtname-variant t)
                                clause
                                (... ...)
                                (else e3 e4 (... ...)))))]
                       [(__ e0
                            (v (fld (... ...)) e1 e2 (... ...))
                            (... ...))
                        (let f ([ls1 (list #'pretty-vname ...)])
                          (or (null? ls1)
                              (and (let g ([ls2 #'(v (... ...))])
                                     (if (null? ls2)
                                         (syntax-error x
                                           (format "unhandled `~s' variant in"
                                             (syntax->datum (car ls1))))
                                         (or (literal-identifier=? (car ls1) (car ls2))
                                             (g (cdr ls2)))))
                                   (f (cdr ls1)))))
                        (with-syntax ([(clause (... ...))
                                       (map make-clause
                                            #'((v (fld (... ...)) e1 e2 (... ...))
                                               (... ...)))])
                          #'(let ([t e0])
                              (case (dtname-variant t)
                                clause
                                (... ...))))])))))))])))

; support for changing from old to new nongenerative record types
(define-syntax update-record-type
  (syntax-rules ()
    [(_ (name make-name pred?) (accessor ...) (mutator ...) old-defn new-defn)
     (module (name make-name pred? accessor ... mutator ...)
       (module old (pred? accessor ... mutator ...) old-defn)
       (module new (name make-name pred? accessor ... mutator ...) new-defn)
       (import (only new name make-name))
       (define pred?
         (lambda (x)
           (or ((let () (import old) pred?) x)
               ((let () (import new) pred?) x))))
       (define accessor
         (lambda (x)
           ((if ((let () (import old) pred?) x)
                (let () (import old) accessor)
                (let () (import new) accessor))
            x)))
       ...
       (define mutator
         (lambda (x v)
           ((if ((let () (import old) pred?) x)
                (let () (import old) mutator)
                (let () (import new) mutator))
            x v)))
       ...)]))

(define-syntax type-check
  (lambda (x)
    (syntax-case x ()
      [(_ who type arg)
       (identifier? #'type)
       #`(let ([x arg])
           (unless (#,(construct-name #'type #'type "?") x)
             ($oops who #,(format "~~s is not a ~a" (datum type)) x)))]
      [(_ who type pred arg)
       (string? (datum type))
       #`(let ([x arg])
           (unless (pred x)
             ($oops who #,(format "~~s is not a ~a" (datum type)) x)))])))
 
(eval-when (load eval)
(define-syntax lookup-libspec
  (lambda (x)
    (syntax-case x ()
      [(_ x)
       (identifier? #'x)
       #`(quote #,(datum->syntax #'x
                    (let ((x (datum x)))
                      (or ($sgetprop x '*libspec* #f)
                          ($oops 'lookup-libspec "~s is undefined" x)))))])))

(define-syntax lookup-does-not-expect-headroom-libspec
  (lambda (x)
    (syntax-case x ()
      [(_ x)
       (identifier? #'x)
       #`(quote #,(datum->syntax #'x
                    (let ((x (datum x)))
                      (or ($sgetprop x '*does-not-expect-headroom-libspec* #f)
                          ($oops 'lookup-does-not-expect-headroom-libspec "~s is undefined" x)))))])))

(define-syntax lookup-c-entry
  (lambda (x)
    (syntax-case x ()
      ((_ x)
       (identifier? #'x)
       (let ((sym (datum x)))
         (datum->syntax #'x
           (or ($sgetprop sym '*c-entry* #f)
               ($oops 'lookup-c-entry "~s is undefined" sym))))))))

(let ()
  (define-syntax declare-library-entries
    (lambda (x)
      (syntax-case x ()
        ((_ (name closure? interface error? has-does-not-expect-headroom-version?) ...)
         (with-syntax ([(index-base ...) (enumerate (datum (name ...)))])
           (for-each (lambda (name closure? interface error? has-does-not-expect-headroom-version?)
                       (define (nnfixnum? x) (and (fixnum? x) (fxnonnegative? x)))
                       (unless (and (symbol? name)
                                    (boolean? closure?)
                                    (nnfixnum? interface)
                                    (boolean? error?))
                         ($oops 'declare-library-entries "invalid entry for ~s" name)))
             (datum (name ...))
             (datum (closure? ...))
             (datum (interface ...))
             (datum (error? ...))
             (datum (has-does-not-expect-headroom-version? ...)))
           #`(begin
               (define-constant library-entry-vector-size #,(* (length (datum (index-base ...))) 2))
               (for-each (lambda (xname xindex-base xclosure? xinterface xerror? xhas-does-not-expect-headroom-version?)
                           ($sputprop xname '*libspec*
                             (make-libspec xname
                               (make-libspec-flags xindex-base #f xclosure? xinterface xerror? xhas-does-not-expect-headroom-version?)))
                           (when xhas-does-not-expect-headroom-version?
                             ($sputprop xname '*does-not-expect-headroom-libspec*
                               (make-libspec xname 
                                 (make-libspec-flags xindex-base #t xclosure? xinterface xerror? xhas-does-not-expect-headroom-version?)))))
                 '(name ...)
                 '(index-base ...)
                 '(closure? ...)
                 '(interface ...)
                 '(error? ...)
                 '(has-does-not-expect-headroom-version? ...))))))))

  (declare-library-entries
     (main #f 0 #f #f) ;; fake entry for main, never called directly (part of fasl load)
     (car #f 1 #t #t)
     (cdr #f 1 #t #t)
     (unbox #f 1 #t #t)
     (set-box! #f 2 #t #t)
     (= #f 2 #f #t)
     (< #f 2 #f #t)
     (> #f 2 #f #t)
     (<= #f 2 #f #t)
     (>= #f 2 #f #t)
     (+ #f 2 #f #t)
     (- #f 2 #f #t)
     (* #f 2 #f #t)
     (/ #f 2 #f #t)
     (unsafe-read-char #f 1 #f #t)
     (safe-read-char #f 1 #f #t)
     (unsafe-peek-char #f 1 #f #t)
     (safe-peek-char #f 1 #f #t)
     (unsafe-write-char #f 2 #f #t)
     (safe-write-char #f 2 #f #t)
     (unsafe-newline #f 1 #f #t)
     (safe-newline #f 1 #f #t)
     ($top-level-value #f 1 #f #t)
     (event #f 0 #f #t)
     (zero? #f 1 #f #t)
     (1+ #f 1 #f #t)
     (1- #f 1 #f #t)
     (fx+ #f 2 #t #t)
     (fx- #f 2 #t #t)
     (fx= #f 2 #t #t)
     (fx< #f 2 #t #t)
     (fx> #f 2 #t #t)
     (fx<= #f 2 #t #t)
     (fx>= #f 2 #t #t)
     (fl+ #f 2 #t #t)
     (fl- #f 2 #t #t)
     (fl* #f 2 #t #t)
     (fl/ #f 2 #t #t)
     (fl= #f 2 #t #t)
     (fl< #f 2 #t #t)
     (fl> #f 2 #t #t)
     (fl<= #f 2 #t #t)
     (fl>= #f 2 #t #t)
     (callcc #f 1 #f #f)
     (display-string #f 2 #f #t)
     (cfl* #f 2 #f #t)
     (cfl+ #f 2 #f #t)
     (cfl- #f 2 #f #t)
     (cfl/ #f 2 #f #t)
     (negate #f 1 #f #t)
     (flnegate #f 1 #t #t)
     (call-error #f 0 #f #f)
     (unsafe-unread-char #f 2 #f #t)
     (map-car #f 1 #f #t)
     (map-cons #f 2 #f #t)
     (fx1+ #f 1 #t #t)
     (fx1- #f 1 #t #t)
     (fxzero? #f 1 #t #t)
     (fxpositive? #f 1 #t #t)
     (fxnegative? #f 1 #t #t)
     (fxnonpositive? #f 1 #t #t)
     (fxnonnegative? #f 1 #t #t)
     (fxeven? #f 1 #t #t)
     (fxodd? #f 1 #t #t)
     (fxlogor #f 2 #t #t)
     (fxlogxor #f 2 #t #t)
     (fxlogand #f 2 #t #t)
     (fxlognot #f 1 #t #t)
     (fxsll #f 2 #f #t)
     (fxsrl #f 2 #t #t)
     (fxsra #f 2 #t #t)
     (append #f 2 #f #t)
     (values-error #f 0 #f #f)
     (dooverflow #f 0 #f #f)
     (dooverflood #f 0 #f #f)
     (nonprocedure-code #f 0 #f #f)
     (dounderflow #f 0 #f #f)
     (dofargint32 #f 1 #f #f)
     (map-cdr #f 1 #f #t)
     (dofretint32 #f 1 #f #f)
     (dofretuns32 #f 1 #f #f)
     (domvleterr #f 0 #f #f)
     (doargerr #f 0 #f #f)
     (get-room #f 0 #f #f)
     (map1 #f 2 #f #t)
     (map2 #f 3 #f #t)
     (for-each1 #f 2 #f #t)
     (vector-ref #f 2 #t #t)
     (vector-set! #f 3 #t #t)
     (vector-length #f 1 #t #t)
     (string-ref #f 2 #t #t)
     (string-set! #f 3 #f #t)
     (string-length #f 1 #t #t)
     (char=? #f 2 #t #t)
     (char<? #f 2 #t #t)
     (char>? #f 2 #t #t)
     (char<=? #f 2 #t #t)
     (char>=? #f 2 #t #t)
     (char->integer #f 1 #t #t)
     (memv #f 2 #f #t)
     (eqv? #f 2 #f #t)
     (set-car! #f 2 #t #t)
     (set-cdr! #f 2 #t #t)
     (caar #f 1 #t #t)
     (cadr #f 1 #t #t)
     (cdar #f 1 #t #t)
     (cddr #f 1 #t #t)
     (caaar #f 1 #t #t)
     (caadr #f 1 #t #t)
     (cadar #f 1 #t #t)
     (caddr #f 1 #t #t)
     (cdaar #f 1 #t #t)
     (cdadr #f 1 #t #t)
     (cddar #f 1 #t #t)
     (cdddr #f 1 #t #t)
     (caaaar #f 1 #t #t)
     (caaadr #f 1 #t #t)
     (caadar #f 1 #t #t)
     (caaddr #f 1 #t #t)
     (cadaar #f 1 #t #t)
     (cadadr #f 1 #t #t)
     (caddar #f 1 #t #t)
     (cadddr #f 1 #t #t)
     (cdaaar #f 1 #t #t)
     (cdaadr #f 1 #t #t)
     (cdadar #f 1 #t #t)
     (cdaddr #f 1 #t #t)
     (cddaar #f 1 #t #t)
     (cddadr #f 1 #t #t)
     (cdddar #f 1 #t #t)
     (cddddr #f 1 #t #t)
     (dounderflow* #f 2 #f #t)
     (call1cc #f 1 #f #f)
     (dorest0 #f 0 #f #f)
     (dorest1 #f 0 #f #f)
     (dorest2 #f 0 #f #f)
     (dorest3 #f 0 #f #f)
     (dorest4 #f 0 #f #f)
     (dorest5 #f 0 #f #f)
     (add1 #f 1 #f #t)
     (sub1 #f 1 #f #t)
     (-1+ #f 1 #f #t)
     (fx* #f 2 #t #t)
     (dofargint64 #f 1 #f #f)
     (dofretint64 #f 1 #f #f)
     (dofretuns64 #f 1 #f #f)
     (apply0 #f 2 #f #t)
     (apply1 #f 3 #f #t)
     (apply2 #f 4 #f #t)
     (apply3 #f 5 #f #t)
     (logand #f 2 #f #t)
     (logor #f 2 #f #t)
     (logxor #f 2 #f #t)
     (lognot #f 1 #f #t)
     (flround #f 1 #f #t)
     (fxlogtest #f 2 #t #t)
     (fxlogbit? #f 2 #f #t)
     (logtest #f 2 #f #t)
     (logbit? #f 2 #f #t)
     (fxlogior #f 2 #t #t)
     (logior #f 2 #f #t)
     (fxlogbit0 #f 2 #t #t)
     (fxlogbit1 #f 2 #t #t)
     (logbit0 #f 2 #f #t)
     (logbit1 #f 2 #f #t)
     (vector-set-fixnum! #f 3 #t #t)
     (fxvector-ref #f 2 #t #t)
     (fxvector-set! #f 3 #t #t)
     (fxvector-length #f 1 #t #t)
     (scan-remembered-set #f 0 #f #f)
     (fold-left1 #f 3 #f #t)
     (fold-left2 #f 4 #f #t)
     (fold-right1 #f 3 #f #t)
     (fold-right2 #f 4 #f #t)
     (for-each2 #f 3 #f #t)
     (vector-map1 #f 2 #f #t)
     (vector-map2 #f 3 #f #t)
     (vector-for-each1 #f 2 #f #t)
     (vector-for-each2 #f 3 #f #t)
     (bytevector-length #f 1 #t #t)
     (bytevector-s8-ref #f 2 #t #t)
     (bytevector-u8-ref #f 2 #t #t)
     (bytevector-s8-set! #f 3 #f #t)
     (bytevector-u8-set! #f 3 #f #t)
     (bytevector=? #f 2 #f #f)
     (real->flonum #f 2 #f #t)
     (unsafe-port-eof? #f 1 #f #t)
     (unsafe-lookahead-u8 #f 1 #f #t)
     (unsafe-unget-u8 #f 2 #f #t)
     (unsafe-get-u8 #f 1 #f #t)
     (unsafe-lookahead-char #f 1 #f #t)
     (unsafe-unget-char #f 2 #f #t)
     (unsafe-get-char #f 1 #f #t)
     (unsafe-put-u8 #f 2 #f #t)
     (put-bytevector #f 4 #f #t)
     (unsafe-put-char #f 2 #f #t)
     (put-string #f 4 #f #t)
     (string-for-each1 #f 2 #f #t)
     (string-for-each2 #f 3 #f #t)
     (fx=? #f 2 #t #t)
     (fx<? #f 2 #t #t)
     (fx>? #f 2 #t #t)
     (fx<=? #f 2 #t #t)
     (fx>=? #f 2 #t #t)
     (fl=? #f 2 #t #t)
     (fl<? #f 2 #t #t)
     (fl>? #f 2 #t #t)
     (fl<=? #f 2 #t #t)
     (fl>=? #f 2 #t #t)
     (bitwise-and #f 2 #f #t)
     (bitwise-ior #f 2 #f #t)
     (bitwise-xor #f 2 #f #t)
     (bitwise-not #f 1 #f #t)
     (fxior #f 2 #t #t)
     (fxxor #f 2 #t #t)
     (fxand #f 2 #t #t)
     (fxnot #f 1 #t #t)
     (fxarithmetic-shift-left #f 2 #f #t)
     (fxarithmetic-shift-right #f 2 #t #t)
     (fxarithmetic-shift #f 2 #f #t)
     (bitwise-bit-set? #f 2 #f #t)
     (fxbit-set? #f 2 #f #t)
     (fxcopy-bit #f 2 #t #t)
     (reverse #f 1 #f #t)
     (andmap1 #f 2 #f #t)
     (ormap1 #f 2 #f #t)
     (put-bytevector-some #f 4 #f #t)
     (put-string-some #f 4 #f #t)
     (dofretu8* #f 1 #f #f)
     (dofretu16* #f 1 #f #f)
     (dofretu32* #f 1 #f #f)
     (eq-hashtable-ref #f 3 #f #t)
     (eq-hashtable-contains? #f 2 #f #t)
     (eq-hashtable-cell #f 3 #f #t)
     (eq-hashtable-set! #f 3 #f #t)
     (eq-hashtable-update! #f 4 #f #t)
     (eq-hashtable-delete! #f 2 #f #t)
     (symbol-hashtable-ref #f 3 #f #t)
     (symbol-hashtable-contains? #f 2 #f #t)
     (symbol-hashtable-cell #f 3 #f #t)
     (symbol-hashtable-set! #f 3 #f #t)
     (symbol-hashtable-update! #f 4 #f #t)
     (symbol-hashtable-delete! #f 2 #f #t)
     (safe-port-eof? #f 1 #f #t)
     (safe-lookahead-u8 #f 1 #f #t)
     (safe-unget-u8 #f 2 #f #t)
     (safe-get-u8 #f 1 #f #t)
     (safe-lookahead-char #f 1 #f #t)
     (safe-unget-char #f 2 #f #t)
     (safe-get-char #f 1 #f #t)
     (safe-put-u8 #f 2 #f #t)
     (safe-put-char #f 2 #f #t)
     (safe-unread-char #f 2 #f #t)
     (dorest0 #f 0 #f #t)
     (dorest1 #f 0 #f #t)
     (dorest2 #f 0 #f #t)
     (dorest3 #f 0 #f #t)
     (dorest4 #f 0 #f #t)
     (dorest5 #f 0 #f #t)
     (nuate #f 0 #f #t)
     (virtual-register #f 1 #t #t)
     (set-virtual-register! #f 1 #t #t)
  ))

(let ()
  (define-syntax declare-c-entries
    (lambda (x)
      (syntax-case x ()
        ((_ x ...)
         (andmap identifier?  #'(x ...))
         (with-syntax ((size (length (datum (x ...))))
                       ((i ...) (enumerate (datum (x ...)))))
           #'(let ([name-vec (make-vector size)])
               (define-constant c-entry-vector-size size)
               (define-constant c-entry-name-vector name-vec)
               (for-each (lambda (s n)
                           (vector-set! name-vec n s)
                           ($sputprop s '*c-entry* n))
                 '(x ...)
                 '(i ...))))))))

  (declare-c-entries
     thread-context
     get-thread-context
     handle-apply-overflood
     handle-docall-error
     handle-overflow
     handle-overflood
     handle-nonprocedure-symbol
     thread-list
     split-and-resize
     raw-collect-cond
     raw-tc-mutex
     activate-thread
     deactivate-thread
     unactivate-thread
     handle-values-error
     handle-mvlet-error
     handle-arg-error
     foreign-entry
     install-library-entry
     get-more-room
     scan-remembered-set
     instantiate-code-object
     Sreturn
     Scall-one-result
     Scall-any-results
  ))
)
