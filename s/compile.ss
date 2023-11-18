;;; compile.ss
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

;;; use fixnum arithmetic in code building & output routines

(let ()
(import (nanopass))
(include "types.ss")
(include "base-lang.ss")
(include "expand-lang.ss")

; for tracing:
#;(define-syntax do-trace
  (syntax-rules ()
    ((_ . r) (trace-output . r))))
; no tracing:
(define-syntax do-trace
  (syntax-rules ()
    ((_ . r) r)))

(define trace-output
   (lambda (fun . args)
      (when ($assembly-output)
        (fprintf ($assembly-output) "~s ====>~%" ($procedure-name fun)))
      (let ([x (apply fun args)])
         (when ($assembly-output)
            (parameterize ([print-graph #t])
               (pretty-print x ($assembly-output))
               (newline ($assembly-output))))
          x)))

(define cheat?
  (lambda (x)
    (nanopass-case (Lsrc Expr) x
      [,pr #t]
      [(quote ,d) #t]
      [(if ,e0 ,e1 ,e2) (and (cheat? e0) (cheat? e1) (cheat? e2))]
      [(seq ,e1 ,e2) (and (cheat? e1) (cheat? e2))]
      [(call ,preinfo ,e ,e* ...)
       (and (andmap cheat? e*) (cheat? e))]
      [else #f])))

(define cheat-eval
  (rec compile
    (lambda (x)
      (nanopass-case (Lsrc Expr) x
        [,pr ($top-level-value (primref-name pr))]
        [(quote ,d) d]
        [(if ,e0 ,e1 ,e2)
         (compile (if (compile e0) e1 e2))]
        [(seq ,e1 ,e2) (compile e1) (compile e2)]
        [(call ,preinfo ,e ,e* ...)
         (#2%apply (compile e) (map compile e*))]
        [else ($oops #f "unexpected form ~s" x)]))))

(define c-compile
  (lambda (x)
    (with-output-language (Lsrc Expr)
      ($c-make-closure
        ; pretending main is a library routine to avoid argument-count check
        (let ([x `(case-lambda ,(make-preinfo-lambda #f #f (lookup-libspec main) #f (constant code-flag-lift-barrier)) (clause () 0 ,x))])
          ($np-compile x #f))))))

(define c-set-code-quad!
   (foreign-procedure "(cs)s_set_code_quad"
      (scheme-object scheme-object scheme-object)
      void))

(define lookup-c-entry-index
   (foreign-procedure "(cs)lookup_c_entry"
      (iptr)
      scheme-object))

(define-who (c-mkcode x)
  (define (mkcode x)
    (record-case x
      [(object) (x) x]
      [(entry) (i) (lookup-c-entry-index i)]
      [(library) (x) ($lookup-library-entry (libspec-index x) #t)]
      [(library-code) (x)
       ($closure-code ($lookup-library-entry (libspec-index x) #t))]
      [(closure) func
       ; call mkcode on code record first or we might set func-closure field multiple times
       (let ([cp (mkcode ($c-func-code-record func))])
         ; i.e., the remainder must be atomic wrt mkcode
         (or ($c-func-closure func)
             (let ([p ($make-closure (constant code-data-disp) cp)])
               (set-$c-func-closure! func p)
               p)))]
      [(code) (func subtype free name arity-mask size code-list info pinfo*)
       (or ($c-func-code-object func)
           (let ([p ($make-code-object subtype free name arity-mask size info pinfo*)])
             (set-$c-func-code-object! func p)
             (let mkc0 ([c* code-list]
                        [a (constant code-data-disp)]
                        [r* '()]
                        [ra 0]
                        [x* '()])
               (if (null? c*)
                   ($make-relocation-table! p (reverse r*) (reverse x*))
                   (let ([c (car c*)])
                     (record-case c
                       [(word) n
                        ($set-code-word! p a n)
                        (mkc0 (cdr c*) (fx+ a 2) r* ra x*)]
                       [(byte) n
                        ($set-code-byte! p a n)
                        (mkc0 (cdr c*) (fx+ a 1) r* ra x*)]
                       [(long) n
                        ($set-code-long! p a n)
                        (mkc0 (cdr c*) (fx+ a 4) r* ra x*)]
                       [(quad) n
                        ($set-code-quad! p a n)
                        (mkc0 (cdr c*) (fx+ a 8) r* ra x*)]
                       [(code-top-link) ()
                        (constant-case ptr-bits
                          [(64)
                           ($set-code-quad! p a a)
                           (mkc0 (cdr c*) (fx+ a 8) r* ra x*)]
                          [(32)
                           ($set-code-long! p a a)
                           (mkc0 (cdr c*) (fx+ a 4) r* ra x*)])]
                       [(abs) (n x)
                        (let ([x* (cons (mkcode x) x*)])
                          (let ([r ($reloc (constant reloc-abs) n (fx- a ra))])
                            (constant-case ptr-bits
                              [(64) (mkc0 (cdr c*) (fx+ a 8) (cons r r*) a x*)]
                              [(32) (mkc0 (cdr c*) (fx+ a 4) (cons r r*) a x*)])))]
                       [else
                        (constant-case architecture
                          [(x86)
                           (record-case c
                             [(rel) (n x)
                              (let ([x* (cons (mkcode x) x*)])
                                (let ([r ($reloc (constant reloc-rel) n (fx- a ra))])
                                  (mkc0 (cdr c*) (fx+ a 4) (cons r r*) a x*)))]
                             [else (c-assembler-output-error c)])]
                          [(arm32)
                           (record-case c
                             [(arm32-abs) (n x)
                              (let ([a1 (fx- a 4)]) ; [4-byte ldr, 4-byte bra,] 4-byte value
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-arm32-abs) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(arm32-call) (n x)
                              (let ([a1 (fx- a 16)]) ; 4-byte ldr, 4-byte bra, 4-byte value, 4-byte blx
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-arm32-call) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(arm32-jump) (n x)
                              (let ([a1 (fx- a 16)]) ; 4-byte ldr, 4-byte bra, 4-byte value, 4-byte bx
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-arm32-jump) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [else (c-assembler-output-error c)])]
                          [(arm64)
                           (record-case c
                             [(arm64-abs) (n x)
                              (let ([a1 (fx- a 16)]) ; movz, movk, movk, movk
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-arm64-abs) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(arm64-call) (n x)
                              (let ([a1 (fx- a 20)]) ; movz, movk, movk, movk, bl
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-arm64-call) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(arm64-jump) (n x)
                              (let ([a1 (fx- a 20)]) ; movz, movk, movk, movk, b
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-arm64-jump) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [else (c-assembler-output-error c)])]
                          [(ppc32)
                           (record-case c
                             [(ppc32-abs) (n x)
                              (let ([a1 (fx- a 8)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-ppc32-abs) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(ppc32-call) (n x)
                              (let ([a1 (fx- a 16)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-ppc32-call) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(ppc32-jump) (n x)
                              (let ([a1 (fx- a 16)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-ppc32-jump) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [else (c-assembler-output-error c)])]
                          [(x86_64)
                           (record-case c
                             [(x86_64-jump) (n x)
                              (let ([a1 (fx- a 12)] [x* (cons (mkcode x) x*)])
                                (let ([r ($reloc (constant reloc-x86_64-jump) n (fx- a1 ra))])
                                  (mkc0 (cdr c*) a (cons r r*) a1 x*)))]
                             [(x86_64-call) (n x)
                              (let ([a1 (fx- a 12)] [x* (cons (mkcode x) x*)])
                                (let ([r ($reloc (constant reloc-x86_64-call) n (fx- a1 ra))])
                                  (mkc0 (cdr c*) a (cons r r*) a1 x*)))]
                             [(x86_64-popcount) (n x)
                              (let ([a1 (fx- a 12)] [x* (cons (mkcode x) x*)])
                                (let ([r ($reloc (constant reloc-x86_64-popcount) n (fx- a1 ra))])
                                  (mkc0 (cdr c*) a (cons r r*) a1 x*)))]
                             [else (c-assembler-output-error c)])]
                          [(riscv64)
                           (record-case c
                             [(riscv64-abs) (n x)
                              (let ([a1 (fx- a 20)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-riscv64-abs) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(riscv64-jump) (n x)
                              (let ([a1 (fx- a 24)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-riscv64-jump) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(riscv64-call) (n x)
                              (let ([a1 (fx- a 24)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-riscv64-call) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [else (c-assembler-output-error c)])]
                          [(loongarch64)
                           (record-case c
                             [(loongarch64-abs) (n x)
                              (let ([a1 (fx- a 20)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-loongarch64-abs) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(loongarch64-jump) (n x)
                              (let ([a1 (fx- a 24)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-loongarch64-jump) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(loongarch64-call) (n x)
                              (let ([a1 (fx- a 24)])
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-loongarch64-call) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [else (c-assembler-output-error c)])]
                          [(pb)
                           (record-case c
                             [(pb-abs) (n x)
                              (let ([a1 (fx- a (constant ptr-bytes))]) ; nops
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-pb-abs) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [(pb-proc) (n x)
                              (let ([a1 (fx- a (constant ptr-bytes) 4)]) ; nops, b/call
                                (let ([x* (cons (mkcode x) x*)])
                                  (let ([r ($reloc (constant reloc-pb-proc) n (fx- a1 ra))])
                                    (mkc0 (cdr c*) a (cons r r*) a1 x*))))]
                             [else (c-assembler-output-error c)])]
                          [else (c-assembler-output-error c)])]))))
             p))]
      [else (c-assembler-output-error x)]))
 ; rationale for the critical section:
 ;  (1) the code objects we create here may be mutually recursive, and we
 ;      need for them all to be in the same generation.
 ;  (2) code objects are created without relocation tables, and linked
 ;      after relocation tables are added, potentially confusing the
 ;      collector.  this could be addressed by maintaining a LINKED flag
 ;      in the code-object header.
 ;  (3) we record code modifications as code objects are allocated, then
 ;      flush once at the end to avoid multiple flushes.
 ; rationale for the dynamic-wind:
 ;   we have to flush the instruction cache even if mkcode errors out or is
 ;   interrupted with a noncontinuable interrupt so that no code modifications
 ;   are recorded for code objects that have been dropped and for which the
 ;   memory containing them has been returned to the O/S.
  (critical-section
    (dynamic-wind
      void
      (lambda () (mkcode x))
      $flush-instruction-cache)))

(define c-build-fasl
  (lambda (x t a?)
    (let build ([x x] [d 0])
      (record-case x
        [(object) (x) ($fasl-enter x t a? d)]
        [(closure) func
         ($fasl-bld-graph x t a? d #f
           (lambda (x t a? d)
             (build ($c-func-code-record func) d)))]
        [(code) stuff
         ($fasl-bld-graph x t a? d #f
           (lambda (x t a? d)
             (record-case x
               [(code) (func subtype free name arity-mask size code-list info pinfo*)
                ($fasl-enter name t a? d)
                ($fasl-enter arity-mask t a? d)
                ($fasl-enter info t a? d)
                ($fasl-enter pinfo* t a? d)
                (for-each
                  (lambda (x)
                    (record-case x
                      [(abs) (n x) (build x d)]
                      [else
                       (constant-case architecture
                         [(x86)
                          (record-case x
                            [(rel) (n x) (build x d)]
                            [else (void)])]
                         [(x86_64)
                          (record-case x
                            [(x86_64-jump x86_64-call x86_64-popcount) (n x) (build x d)]
                            [else (void)])]
                         [(arm32)
                          (record-case x
                            [(arm32-abs arm32-call arm32-jump) (n x) (build x d)]
                            [else (void)])]
                         [(arm64)
                          (record-case x
                            [(arm64-abs arm64-call arm64-jump) (n x) (build x d)]
                            [else (void)])]
                         [(ppc32)
                          (record-case x
                            [(ppc32-abs ppc32-call ppc32-jump) (n x) (build x d)]
                            [else (void)])]
                         [(riscv64)
                          (record-case x
                            [(riscv64-abs riscv64-call riscv64-jump) (n x) (build x d)]
                           [else (void)])]
                         [(loongarch64)
                          (record-case x
                            [(loongarch64-abs loongarch64-call loongarch64-jump) (n x) (build x d)]
                            [else (void)])]
                         [(pb)
                          (record-case x
                            [(pb-abs pb-proc) (n x) (build x d)]
                            [else (void)])])]))
                  code-list)])))]))))

(include "fasl-helpers.ss")

(define c-assembler-output-error
   (lambda (x)
      ($oops 'compile-internal
             "invalid assembler output ~s"
             x)))

(define (c-faslobj x t p a?)
  (let faslobj ([x x])
    (record-case x
      [(object) (x) ($fasl-out x p t a?)]
      [(entry) (i)
       (put-u8 p (constant fasl-type-entry))
       (put-uptr p i)]
      [(library) (x)
       (put-u8 p (constant fasl-type-library))
       (put-uptr p (libspec-index x))]
      [(library-code) (x)
       (put-u8 p (constant fasl-type-library-code))
       (put-uptr p (libspec-index x))]
      [(closure) func
       ($fasl-wrf-graph x p t a?
         (lambda (x p t a?)
           (put-u8 p (constant fasl-type-closure))
           (put-uptr p (constant code-data-disp))
           (faslobj ($c-func-code-record func))))]
      [(code) (func subtype free name arity-mask size code-list info pinfo*)
       ($fasl-wrf-graph x p t a?
         (lambda (x p t a?)
           (put-u8 p (constant fasl-type-code))
           (put-u8 p subtype)
           (put-uptr p free)
           (put-uptr p size)
           ($fasl-out name p t a?)
           ($fasl-out arity-mask p t a?)
           ($fasl-out info p t a?)
           ($fasl-out pinfo* p t a?)
           (let prf0 ([c* code-list]
                      [a (constant code-data-disp)]
                      [r* '()]
                      [ra 0]
                      [x* '()])
              (if (null? c*)
                  (begin
                    (let ([actual-size (- a (constant code-data-disp))])
                      (unless (= actual-size size)
                        ($oops 'c-faslcode
                          "wrote ~s bytes, expected ~s bytes"
                          actual-size size)))
                    (put-uptr p (fold-left (lambda (m r) (fx+ m (if (reloc-long? r) 3 1))) 0 r*))
                    (for-each
                      (lambda (r x)
                        (let ([item-offset (reloc-item-offset r)])
                          (put-u8 p
                            (let* ([k (fxsll (reloc-type r) 2)]
                                   [k (if (eqv? item-offset 0) k (fxlogor k 2))])
                              (if (reloc-long? r) (fxlogor k 1) k)))
                          (put-uptr p (reloc-code-offset r))
                          (unless (eqv? item-offset 0) (put-uptr p item-offset))
                          (faslobj x)))
                      (reverse r*)
                      (reverse x*)))
                  (let ([c (car c*)])
                    (record-case c
                      [(word) n
                       (put16 p n)
                       (prf0 (cdr c*) (fx+ a 2) r* ra x*)]
                      [(byte) n
                       (put8 p n)
                       (prf0 (cdr c*) (fx+ a 1) r* ra x*)]
                      [(long) n
                       (put32 p n)
                       (prf0 (cdr c*) (fx+ a 4) r* ra x*)]
                      [(quad) n
                       (put64 p n)
                       (prf0 (cdr c*) (fx+ a 8) r* ra x*)]
                      [(code-top-link) ()
                       (constant-case ptr-bits
                         [(64)
                          (put64 p a)
                          (prf0 (cdr c*) (fx+ a 8) r* ra x*)]
                         [(32)
                          (put32 p a)
                          (prf0 (cdr c*) (fx+ a 4) r* ra x*)])]
                      [(abs) (n x)
                       (let ([r ($reloc (constant reloc-abs) n (fx- a ra))])
                         (constant-case ptr-bits
                           [(64)
                            (put64 p 0)
                            (prf0 (cdr c*) (fx+ a 8) (cons r r*) a (cons x x*))]
                           [(32)
                            (put32 p 0)
                            (prf0 (cdr c*) (fx+ a 4) (cons r r*) a (cons x x*))]))]
                      [else
                       (constant-case architecture
                         [(x86)
                          (record-case c
                            [(rel) (n x)
                             (put32 p 0)
                             (let ([r ($reloc (constant reloc-rel) n (fx- a ra))])
                               (prf0 (cdr c*) (fx+ a 4) (cons r r*) a (cons x x*)))]
                            [else (c-assembler-output-error c)])]
                         [(arm32)
                          (record-case c
                            [(arm32-abs) (n x)
                             (let ([a1 (fx- a 4)]) ; [4-byte ldr, 4-byte bra,] 4-byte value
                               (let ([r ($reloc (constant reloc-arm32-abs) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(arm32-call) (n x)
                             (let ([a1 (fx- a 16)]) ; 4-byte ldr, 4-byte bra, 4-byte value, 4-byte blx
                               (let ([r ($reloc (constant reloc-arm32-call) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(arm32-jump) (n x)
                             (let ([a1 (fx- a 16)]) ; 4-byte ldr, 4-byte bra, 4-byte value, 4-byte bx
                               (let ([r ($reloc (constant reloc-arm32-jump) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [else (c-assembler-output-error c)])]
                         [(arm64)
                          (record-case c
                            [(arm64-abs) (n x)
                             (let ([a1 (fx- a 16)]) ; movz, movk, movk, movk
                               (let ([r ($reloc (constant reloc-arm64-abs) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(arm64-call) (n x)
                             (let ([a1 (fx- a 20)]) ; movz, movk, movk, movk, bl
                               (let ([r ($reloc (constant reloc-arm64-call) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(arm64-jump) (n x)
                             (let ([a1 (fx- a 20)]) ; movz, movk, movk, movk, b
                               (let ([r ($reloc (constant reloc-arm64-jump) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [else (c-assembler-output-error c)])]
                         [(ppc32)
                          (record-case c
                            [(ppc32-abs) (n x)
                             (let ([a1 (fx- a 8)])
                               (let ([r ($reloc (constant reloc-ppc32-abs) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(ppc32-call) (n x)
                             (let ([a1 (fx- a 16)])
                               (let ([r ($reloc (constant reloc-ppc32-call) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(ppc32-jump) (n x)
                             (let ([a1 (fx- a 16)])
                               (let ([r ($reloc (constant reloc-ppc32-jump) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [else (c-assembler-output-error c)])]
                         [(x86_64)
                          (record-case c
                            [(x86_64-jump) (n x)
                             (let ([a1 (fx- a 12)]) ; 10-byte moviq followed by 2-byte jmp
                               (let ([r ($reloc (constant reloc-x86_64-jump) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(x86_64-call) (n x)
                             (let ([a1 (fx- a 12)]) ; 10-byte moviq followed by 2-byte call
                               (let ([r ($reloc (constant reloc-x86_64-call) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(x86_64-popcount) (n x)
                             (let ([a1 (fx- a 12)]) ; like a call, for worst case
                               (let ([r ($reloc (constant reloc-x86_64-popcount) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [else (c-assembler-output-error c)])]
                         [(riscv64)
                          (record-case c
                            [(riscv64-abs) (n x)
                             (let ([a1 (fx- a 20)])
                               (let ([r ($reloc (constant reloc-riscv64-abs) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(riscv64-jump) (n x)
                             (let ([a1 (fx- a 24)])
                               (let ([r ($reloc (constant reloc-riscv64-jump) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(riscv64-call) (n x)
                             (let ([a1 (fx- a 24)])
                               (let ([r ($reloc (constant reloc-riscv64-call) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [else (c-assembler-output-error c)])]
                         [(loongarch64)
                          (record-case c
                            [(loongarch64-abs) (n x)
                             (let ([a1 (fx- a 20)])
                               (let ([r ($reloc (constant reloc-loongarch64-abs) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(loongarch64-jump) (n x)
                             (let ([a1 (fx- a 24)])
                               (let ([r ($reloc (constant reloc-loongarch64-jump) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(loongarch64-call) (n x)
                             (let ([a1 (fx- a 24)])
                               (let ([r ($reloc (constant reloc-loongarch64-call) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [else (c-assembler-output-error c)])]
                         [(pb)
                          (record-case c
                            [(pb-abs) (n x)
                             (let ([a1 (fx- a (constant ptr-bytes))]) ; nops
                               (let ([r ($reloc (constant reloc-pb-abs) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [(pb-proc) (n x)
                             (let ([a1 (fx- a (constant ptr-bytes) 4)]) ; nops, b/call
                               (let ([r ($reloc (constant reloc-pb-proc) n (fx- a1 ra))])
                                 (prf0 (cdr c*) a (cons r r*) a1 (cons x x*))))]
                            [else (c-assembler-output-error c)])]
                         [else (c-assembler-output-error c)])]))))))]
      [else (c-assembler-output-error x)])))

(define (c-print-fasl x p situation external?-pred omit-rtds?)
  (let ([t ($fasl-table external?-pred)]
        [a? (let ([flags (fxlogor
                           (if (generate-inspector-information) (constant annotation-debug) 0)
                           (if (eq? ($compile-profile) 'source) (constant annotation-profile) 0)
                           (if omit-rtds? (constant fasl-omit-rtds) 0))])
              (and (not (fx= flags 0)) flags))])
     (c-build-fasl x t a?)
     ($fasl-start p t situation x a?
       (lambda (x p) (c-faslobj x t p a?)))))

(define-record-type visit-chunk
  (nongenerative)
  (fields chunk))

(define-record-type revisit-chunk
  (nongenerative)
  (fields chunk))

(define-who (host-machine-type)
  (let ([m (machine-type)])
    (let lookup ([ra* (constant machine-type-alist)])
      (if (null? ra*)
          ($oops who "unrecognized machine type ~s" m)
          (if (eq? (cdar ra*) m) (caar ra*) (lookup (cdr ra*)))))))

(define with-whacked-optimization-locs
  (lambda (x1 th)
    (define ht (make-eq-hashtable))
    (define-pass whack! : Lexpand (ir f) -> * ()
      (Outer : Outer (ir) -> * ()
        [,inner (Inner ir)]
        [(group ,[] ,[]) (values)]
        [(visit-only ,[]) (values)]
        [(revisit-only ,[]) (values)]
        [else (values)])
      (Inner : Inner (ir) -> * ()
        [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
         (for-each f db*)
         (values)]
        [else (values)]))
    (whack! x1
      (lambda (db)
        (when db
          (eq-hashtable-set! ht db (unbox db))
          (set-box! db '()))))
    (th)
    (whack! x1
      (lambda (db)
        (when db
          (set-box! db (eq-hashtable-ref ht db '())))))))

(define check-prelex-flags
  (lambda (x after)
    (when ($enable-check-prelex-flags)
      ($pass-time 'cpcheck-prelex-flags (lambda () (do-trace $cpcheck-prelex-flags x after))))))

(define cptypes
  (lambda (x)
    (if (enable-type-recovery)
        ($pass-time 'cptypes (lambda () ($cptypes x)))
        x)))

(define compile-file-help
  (lambda (op hostop wpoop source-table machine sfd do-read outfn external?-pred omit-rtds?)
    (parameterize ([$target-machine machine]
                   [$sfd sfd]
                   [$current-mso ($current-mso)]
                   [$block-counter 0]
                   [optimize-level (optimize-level)]
                   [debug-level (debug-level)]
                   [run-cp0 (run-cp0)]
                   [cp0-effort-limit (cp0-effort-limit)]
                   [cp0-score-limit (cp0-score-limit)]
                   [cp0-outer-unroll-limit (cp0-outer-unroll-limit)]
                   [generate-inspector-information (generate-inspector-information)]
                   [generate-procedure-source-information (generate-procedure-source-information)]
                   [$compile-profile ($compile-profile)]
                   [generate-interrupt-trap (generate-interrupt-trap)]
                   [$optimize-closures ($optimize-closures)]
                   [$lift-closures ($lift-closures)]
                   [enable-cross-library-optimization (enable-cross-library-optimization)]
                   [generate-covin-files (generate-covin-files)]
                   [enable-arithmetic-left-associative (enable-arithmetic-left-associative)]
                   [enable-error-source-expression (enable-error-source-expression)]
                   [enable-unsafe-application (enable-unsafe-application)]
                   [enable-type-recovery (enable-type-recovery)])
      (emit-header op (constant scheme-version) (constant machine-type))
      (when hostop (emit-header hostop (constant scheme-version) (host-machine-type)))
      (when wpoop (emit-header wpoop (constant scheme-version) (host-machine-type)))
      (let cfh0 ([n 1] [rrcinfo** '()] [rlpinfo** '()] [rfinal** '()])
        (let ([x0 ($pass-time 'read do-read)])
          (if (eof-object? x0)
              (compile-file-help2 op (reverse rrcinfo**) (reverse rlpinfo**) (reverse rfinal**) external?-pred omit-rtds?)
              (let ()
                (define source-info-string
                  (and (or ($assembly-output) (expand-output) (expand/optimize-output))
                       (with-output-to-string
                         (lambda ()
                           (printf "expression #~s" n)
                           (when (and (annotation? x0) (fxlogtest (annotation-flags x0) (constant annotation-debug)))
                             (let ((s (annotation-source x0)))
                               (call-with-values
                                 (lambda () ((current-locate-source-object-source) s #t #t))
                                 (case-lambda
                                   [() (void)]
                                   [(path line char) (printf " on line ~s" line)]))))))))
                (when ($assembly-output)
                  (when source-info-string
                    (fprintf ($assembly-output) "~%;; ~a\n" source-info-string))
                  (parameterize ([print-graph #t])
                    (pretty-print (if (annotation? x0) (annotation-stripped x0) x0)
                      ($assembly-output)))
                  (flush-output-port ($assembly-output)))
                (let ([x1 ($pass-time 'expand
                            (lambda ()
                              (expand x0 (if (eq? (subset-mode) 'system) ($system-environment) (interaction-environment)) #t #t outfn)))])
                  (check-prelex-flags x1 'expand)
                  ($uncprep x1 #t) ; populate preinfo sexpr fields
                  (check-prelex-flags x1 'uncprep)
                  (when source-table ($insert-profile-src! source-table x1))
                  (when wpoop
                    ; cross-library optimization locs might be set by cp0 during the expander's compile-time
                    ; evaluation of library forms.  since we have no need for the optimization information in
                    ; the wpo file, we temporarily whack the optimization locs while writing the wpo file.
                    (with-whacked-optimization-locs x1
                      (lambda ()
                        ($with-fasl-target (host-machine-type)
                          (lambda ()
                            (parameterize ([$target-machine (machine-type)])
                              (let ([t ($fasl-table)])
                                ($fasl-enter x1 t (constant annotation-all) 0)
                                ($fasl-start wpoop t (constant fasl-type-visit-revisit) x1 (constant annotation-all)
                                             (lambda (x p) ($fasl-out x p t (constant annotation-all)))))))))))
                  (let-values ([(rcinfo* lpinfo* final*) (compile-file-help1 x1 source-info-string)])
                    (when hostop
                      ; the host library file contains expander output possibly augmented with
                      ; cross-library optimization information inserted by cp0.  this write must come
                      ; after cp0, at least, so that cp0 has a chance to insert that information.
                      ($with-fasl-target (host-machine-type)
                        (lambda ()
                          (parameterize ([$target-machine (machine-type)])
                            (let ([t ($fasl-table)])
                              ($fasl-enter x1 t (constant annotation-all) 0)
                              ($fasl-start hostop t (constant fasl-type-visit-revisit) x1 (constant annotation-all)
                                           (lambda (x p) ($fasl-out x p t (constant annotation-all)))))))))
                    (cfh0 (+ n 1) (cons rcinfo* rrcinfo**) (cons lpinfo* rlpinfo**) (cons final* rfinal**)))))))))))

(define library/program-info?
  (lambda (x)
    (or (program-info? x) (library-info? x))))

(define-who compile-file-help1
  (lambda (x1 source-info-string)
    (define-who expand-Lexpand
      (lambda (e)
        ; we might want to export expand-Inner from syntax.ss instead of $build-install-library/ct-code
        ; and $build-install-library/rt-code
        (define-pass expand-Inner : Lexpand (ir) -> Lexpand ()
          (Inner : Inner (ir) -> Inner ()
            [,lsrc lsrc] ; NB: workaround for nanopass tag snafu
            [(program ,uid ,body) ($build-invoke-program uid body)]
            [(library/ct ,uid (,export-id* ...) ,import-code ,visit-code)
             ($build-install-library/ct-code uid export-id* import-code visit-code)]
            [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
             ($build-install-library/rt-code uid dl* db* dv* de* body)]
            [else ir]))
        (with-output-language (Lsrc Expr)
          (define (lambda-chunk lsrc)
            ; pretending main is a library routine to avoid argument-count check
            `(case-lambda ,(make-preinfo-lambda #f #f (lookup-libspec main) #f (constant code-flag-lift-barrier))
               (clause () 0 ,lsrc)))
          (define (visit lsrc e* rchunk*)
            (define (rchunks) (cons (make-visit-chunk (lambda-chunk lsrc)) rchunk*))
            (if (null? e*)
                (rchunks)
                (let f ([e (car e*)] [e* (cdr e*)])
                  (nanopass-case (Lexpand Outer) e
                    [(group ,outer1 ,outer2) (f outer1 (cons outer2 e*))]
                    [(visit-only ,lsrc2) (visit `(seq ,lsrc ,lsrc2) e* rchunk*)]
                    [else (common e e* (rchunks))]))))
          (define (revisit lsrc e* rchunk*)
            (define (rchunks) (cons (make-revisit-chunk (lambda-chunk lsrc)) rchunk*))
            (if (null? e*)
                (rchunks)
                (let f ([e (car e*)] [e* (cdr e*)])
                  (nanopass-case (Lexpand Outer) e
                    [(group ,outer1 ,outer2) (f outer1 (cons outer2 e*))]
                    [(revisit-only ,lsrc2) (revisit `(seq ,lsrc ,lsrc2) e* rchunk*)]
                    [else (common e e* (rchunks))]))))
          (define (visit-revisit lsrc e* rchunk*)
            (define (rchunks) (cons (lambda-chunk lsrc) rchunk*))
            (if (null? e*)
                (rchunks)
                (let f ([e (car e*)] [e* (cdr e*)])
                  (nanopass-case (Lexpand Outer) e
                    [(group ,outer1 ,outer2) (f outer1 (cons outer2 e*))]
                    [,lsrc2 (visit-revisit `(seq ,lsrc ,lsrc2) e* rchunk*)]
                    [else (common e e* (rchunks))]))))
          (define (unwrap-inner e)
            (nanopass-case (Lexpand Inner) e
              [(library/ct-info ,linfo/ct) linfo/ct]
              [(library/rt-info ,linfo/rt) linfo/rt]
              [(program-info ,pinfo) pinfo]
              [else e]))
          (define (common e e* rchunk*)
            (nanopass-case (Lexpand Outer) e
              [(visit-only ,lsrc) (visit lsrc e* rchunk*)]
              [(revisit-only ,lsrc) (revisit lsrc e* rchunk*)]
              [,lsrc (visit-revisit lsrc e* rchunk*)]
              [else (let ([rchunk* (cons (nanopass-case (Lexpand Outer) e
                                           [(visit-only ,inner) (make-visit-chunk (unwrap-inner inner))]
                                           [(revisit-only ,inner) (make-revisit-chunk (unwrap-inner inner))]
                                           [(recompile-info ,rcinfo) rcinfo]
                                           [,inner (unwrap-inner inner)]
                                           [else (sorry! who "unexpected Outer ~s" e)])
                                     rchunk*)])
                      (if (null? e*) rchunk* (start (car e*) (cdr e*) rchunk*)))]))
          (define (start e e* rchunk*)
            (nanopass-case (Lexpand Outer) e
              [(group ,outer1 ,outer2) (start outer1 (cons outer2 e*) rchunk*)]
              [else (common e e* rchunk*)]))
          (reverse (start (expand-Inner e) '() '())))))
    (when (expand-output)
      (when source-info-string
        (fprintf (expand-output) "~%;; expand output for ~a\n" source-info-string))
      (pretty-print ($uncprep x1) (expand-output))
      (flush-output-port (expand-output)))
    (let loop ([chunk* (expand-Lexpand x1)] [rx2b* '()] [rfinal* '()] [rlpinfo* '()] [rrcinfo* '()])
      (if (null? chunk*)
          (begin
            (when (expand/optimize-output)
              (when source-info-string
                (fprintf (expand/optimize-output) "~%;; expand/optimize output for ~a\n" source-info-string))
              (let ([e* (map (lambda (x2b)
                               (define (finish x2b)
                                 ($uncprep
                                   (cond
                                     [(recompile-info? x2b) (with-output-language (Lexpand Outer) `(recompile-info ,x2b))]
                                     [(library/ct-info? x2b) (with-output-language (Lexpand Inner) `(library/ct-info ,x2b))]
                                     [(library/rt-info? x2b) (with-output-language (Lexpand Inner) `(library/rt-info ,x2b))]
                                     [(program-info? x2b) (with-output-language (Lexpand Inner) `(program-info ,x2b))]
                                     [else 
                                       (nanopass-case (Lsrc Expr) x2b
                                         [(case-lambda ,preinfo (clause () ,interface ,body)) body]
                                         [else (sorry! 'compile-file-help "unexpected optimizer output ~s" x2b)])])))
                               (if (pair? x2b)
                                   (case (car x2b)
                                     [(visit-stuff) `(eval-when (visit) ,(finish (cdr x2b)))]
                                     [(revisit-stuff) `(eval-when (revisit) ,(finish (cdr x2b)))]
                                     [else (sorry! who "unrecognized stuff ~s" x2b)])
                                   (finish x2b)))
                          rx2b*)])
                (pretty-print (if (fx= (length e*) 1) (car e*) `(begin ,@(reverse e*))) (expand/optimize-output))
                (flush-output-port (expand/optimize-output))))
            (values (reverse rrcinfo*) (reverse rlpinfo*) (reverse rfinal*)))
          (let ([x1 (car chunk*)] [chunk* (cdr chunk*)])
            (define finish-compile
              (lambda (x1 f)
                (if (library/program-info? x1)
                    (loop chunk* (cons (f x1) rx2b*) rfinal* (cons (f `(object ,x1)) rlpinfo*) rrcinfo*)
                    (let* ([waste (check-prelex-flags x1 'before-cpvalid)]
                           [x2 ($pass-time 'cpvalid (lambda () (do-trace $cpvalid x1)))]
                           [waste (check-prelex-flags x2 'cpvalid)]
                           [x2a (let ([cpletrec-ran? #f])
                                  (let ([x ((run-cp0)
                                            (lambda (x)
                                              (set! cpletrec-ran? #t)
                                              (let* ([x ($pass-time 'cp0 (lambda () (do-trace $cp0 x)))]
                                                     [waste (check-prelex-flags x 'cp0)]
                                                     [x (cptypes x)]
                                                     [waste (check-prelex-flags x 'cptypes)]
                                                     [x ($pass-time 'cpletrec (lambda () (do-trace $cpletrec x)))]
                                                     [waste (check-prelex-flags x 'cpletrec)])
                                                x))
                                            x2)])
                                    (if cpletrec-ran?
                                        x
                                        (let ([x ($pass-time 'cpletrec (lambda () (do-trace $cpletrec x)))])
                                          (check-prelex-flags x 'cpletrec)
                                          x))))]
                           [x2b ($pass-time 'cpcheck (lambda () (do-trace $cpcheck x2a)))]
                           [waste (check-prelex-flags x2b 'cpcheck)]
                           [x2b ($pass-time 'cpcommonize (lambda () (do-trace $cpcommonize x2b)))]
                           [waste (check-prelex-flags x2b 'cpcommonize)]
                           [x7 (do-trace $np-compile x2b #t)]
                           [x8 ($c-make-closure x7)])
                      (loop chunk* (cons (f x2b) rx2b*) (cons (f x8) rfinal*) rlpinfo* rrcinfo*)))))
            (cond
              [(recompile-info? x1) (loop chunk* (cons x1 rx2b*) rfinal* rlpinfo* (cons x1 rrcinfo*))]
              [(visit-chunk? x1) (finish-compile (visit-chunk-chunk x1) (lambda (x) `(visit-stuff . ,x)))]
              [(revisit-chunk? x1) (finish-compile (revisit-chunk-chunk x1) (lambda (x) `(revisit-stuff . ,x)))]
              [else (finish-compile x1 values)]))))))

(define compile-file-help2
  (lambda (op rcinfo** lpinfo** final** external?-pred omit-rtds?)
    (define (libreq-hash x) (symbol-hash (libreq-uid x)))
    (define (libreq=? x y) (eq? (libreq-uid x) (libreq-uid y)))
    (let ([import-ht (make-hashtable libreq-hash libreq=?)]
          [include-ht (make-hashtable string-hash string=?)])
      (for-each
        (lambda (rcinfo*)
          (for-each
            (lambda (rcinfo)
              (for-each
                (lambda (x) (hashtable-set! import-ht x #t))
                (recompile-info-import-req* rcinfo))
              (for-each
                (lambda (x) (hashtable-set! include-ht x #t))
                (recompile-info-include-req* rcinfo)))
            rcinfo*))
        rcinfo**)
      (let ([import-req* (vector->list (hashtable-keys import-ht))]
            [include-req* (vector->list (hashtable-keys include-ht))])
        ; the first entry is always, if needed, a recompile-info record with recompile information for the entire object file
        ($pass-time 'pfasl
          (lambda ()
            (unless (and (compile-omit-concatenate-support) (null? import-req*) (null? include-req*))
              (c-print-fasl `(object ,(make-recompile-info import-req* include-req*)) op (constant fasl-type-visit-revisit) #f #f))
            (for-each
              (lambda (final*)
                (for-each
                  (lambda (x)
                    (record-case x
                      [(visit-stuff) x (c-print-fasl x op (constant fasl-type-visit) external?-pred omit-rtds?)]
                      [(revisit-stuff) x (c-print-fasl x op (constant fasl-type-revisit) external?-pred omit-rtds?)]
                      [else (c-print-fasl x op (constant fasl-type-visit-revisit) external?-pred omit-rtds?)]))
                  final*))
              (append lpinfo**
                      (if (compile-omit-concatenate-support)
                          final**
                          ;; inserting #t after lpinfo as an end-of-header marker
                          (cons (list `(object #t)) final**))))))))))

(define (new-extension new-ext fn)
  (let ([old-ext (path-extension fn)])
    (format "~a.~a"
      (if (or (string=? old-ext "") (string=? old-ext new-ext)) fn (path-root fn))
      new-ext)))

(module (with-object-file with-host-file with-wpo-file with-coverage-file)
  (define call-with-port/cleanup
    (lambda (ofn op p)
      (on-reset (delete-file ofn #f)
        (on-reset (close-port op)
          (p op))
        (close-port op))))

  (define with-object-file
    (lambda (who ofn p)
      (call-with-port/cleanup ofn
        ($open-file-output-port who ofn
          (file-options replace))
        p)))

  (define with-host-file
    (lambda (who ofn p)
      (if ofn
          (call-with-port/cleanup ofn
            ($open-file-output-port who ofn
              (file-options replace))
            p)
          (p #f))))

  (define with-wpo-file
    (lambda (who ofn p)
      (if (generate-wpo-files)
          (let ([ofn (new-extension "wpo" ofn)])
            (call-with-port/cleanup ofn
              ($open-file-output-port who ofn
                (file-options replace))
              p))
          (p #f))))

  (define with-coverage-file
    (lambda (who ofn p)
      (if (generate-covin-files)
          (let ([ofn (new-extension "covin" ofn)])
            (call-with-port/cleanup ofn
              ($open-file-output-port who ofn
                (file-options compressed replace)
                (buffer-mode block)
                (current-transcoder))
              (lambda (op)
                (let ([source-table (make-source-table)])
                  (p source-table)
                  (put-source-table op source-table)))))
          (p #f)))))

(set! $compile-host-library
  (lambda (who iofn)
    (let ([ip ($open-file-input-port who iofn)])
      (on-reset (close-port ip)
        (let loop ([rx1* '()] [rcinfo* '()] [rother* '()])
          (let ([x1 (fasl-read ip)])
            (cond
              [(eof-object? x1)
               (close-port ip)
               (unless (null? rx1*)
                 (unless (null? rother*) ($oops 'compile-library "unexpected value ~s read from file ~s that also contains ~s" (car rother*) iofn (car rx1*)))
                 (with-object-file who iofn
                   (lambda (op)
                     (emit-header op (constant scheme-version) (constant machine-type))
                       (let loop ([x1* (reverse rx1*)] [rrcinfo** (list rcinfo*)] [rlpinfo** '()] [rfinal** '()])
                         (if (null? x1*)
                             (compile-file-help2 op (reverse rrcinfo**) (reverse rlpinfo**) (reverse rfinal**) #f #f)
                             (let-values ([(rcinfo* lpinfo* final*)
                                           (let ([x1 (car x1*)])
                                             (if (recompile-info? x1)
                                                 (values (list x1) '() '())
                                                 (compile-file-help1 (car x1*) "host library")))])
                               (loop (cdr x1*) (cons rcinfo* rrcinfo**) (cons lpinfo* rlpinfo**) (cons final* rfinal**))))))))]
              [(recompile-info? x1) (loop rx1* (cons x1 rcinfo*) rother*)]
              [(Lexpand? x1) (loop (cons x1 rx1*) rcinfo* rother*)]
              [else (loop rx1* rcinfo* (cons x1 rother*))])))))))

(let ()
  (define-record-type node (nongenerative)
    (fields (mutable depend*) (mutable use-count))
    (protocol
      (lambda (new)
        (lambda ()
          (new #f 0)))))
  (define-record-type program-node (nongenerative) (sealed #t) (parent node)
    (fields pinfo (mutable ir))
    (protocol
      (lambda (pargs->new)
        (lambda (pinfo)
          ((pargs->new) pinfo #f)))))
  (define program-node-uid
    (lambda (node)
      (program-info-uid (program-node-pinfo node))))
  (define program-node-invoke-req*
    (lambda (node)
      (program-info-invoke-req* (program-node-pinfo node))))

  (define-record-type library-node (nongenerative) (parent node)
    (fields binary? (mutable ctinfo) (mutable rtinfo) (mutable ctir) (mutable rtir) (mutable visible?) fn)
    (protocol
      (lambda (pargs->new)
        (lambda (binary? ctinfo rtinfo visible? fn)
          (safe-assert (or ctinfo rtinfo))
          ((pargs->new) binary? ctinfo rtinfo #f #f visible? fn)))))
  (define library-node-path
    (lambda (node)
      (library-info-path (or (library-node-ctinfo node) (library-node-rtinfo node)))))
  (define library-node-uid
    (lambda (node)
      (library-info-uid (or (library-node-ctinfo node) (library-node-rtinfo node)))))
  (define library-node-version
    (lambda (node)
      (library-info-version (or (library-node-ctinfo node) (library-node-rtinfo node)))))    
  (define library-node-invoke-req*
    (lambda (node)
      (library/rt-info-invoke-req* (library-node-rtinfo node))))
  (define library-node-import-req*
    (lambda (node)
      (library/ct-info-import-req* (library-node-ctinfo node))))            

  (define read-input-file
    (lambda (who ifn)
      (call-with-port ($open-file-input-port who ifn)
        (lambda (ip)
          (on-reset (close-port ip)
            (let ([hash-bang-line
                   (let ([start-pos (port-position ip)])
                     (if (and (eqv? (get-u8 ip) (char->integer #\#))
                              (eqv? (get-u8 ip) (char->integer #\!))
                              (let ([b (lookahead-u8 ip)])
                                (or (eqv? b (char->integer #\space))
                                    (eqv? b (char->integer #\/)))))
                         (let-values ([(op get-bv) (open-bytevector-output-port)])
                           (put-u8 op (char->integer #\#))
                           (put-u8 op (char->integer #\!))
                           (let loop ()
                             (let ([b (get-u8 ip)])
                               (unless (eof-object? b)
                                 (put-u8 op b)
                                 (unless (eqv? b (char->integer #\newline))
                                   (loop)))))
                           (get-bv))
                         (begin (set-port-position! ip start-pos) #f)))])
              (if ($compiled-file-header? ip)
                  (let loop ([rls '()])
                    (let ([x (fasl-read ip)])
                      (cond
                        [(eof-object? x) (values hash-bang-line (reverse rls))]
                        [(Lexpand? x) (loop (cons x rls))]
                        [else ($oops who "unexpected wpo file object ~s" x)])))
                  ($oops who "input file is source ~s" ifn))))))))

  (define find-library
    (lambda (who path what library-ext*)
      (with-values
        ($library-search who path (library-directories) library-ext*)
        (lambda (src-path lib-path lib-exists?)
          (and lib-exists?
               (begin
                 (when (and src-path
                            (eq? (library-timestamp-mode) 'modification-time)
                            (time<? (file-modification-time lib-path) (file-modification-time src-path)))
                   (warningf who "~a file ~a is older than source file ~a" what lib-path src-path))
                 (when (import-notify) (fprintf (console-output-port) "reading ~a\n" lib-path))
                 lib-path))))))

  (define build-graph
    (lambda (who ir* ifn capture-program? capture-wpo? libs-visible?)
      (let ([libs (make-hashtable symbol-hash eq?)] [wpo* '()])
        (define lookup-path
          (lambda (uid)
            (cond
              [(symbol-hashtable-ref libs uid #f) => library-node-path]
              [else uid])))
        (define read-library
          (lambda (path libs-visible?)
            (cond
              [(find-library who path "wpo" (map (lambda (ext) (cons (car ext) (string-append (path-root (cdr ext)) ".wpo"))) (library-extensions))) =>
               (lambda (fn)
                 (let*-values ([(hash-bang-line ir*) (read-input-file who fn)]
                               [(no-program node* ignore-rcinfo*) (process-ir*! ir* fn #f libs-visible?)])
                   (values fn node*)))]
              [(find-library who path "so" (library-extensions)) =>
               (lambda (fn) (values fn (read-binary-file path fn libs-visible?)))]
              [else ($oops who "unable to locate expanded library file for library ~s" path)])))
        (define read-binary-file
          (lambda (path fn libs-visible?)
            (call-with-port ($open-file-input-port who fn)
              (lambda (ip)
                (on-reset (close-port ip)
                  (if ($compiled-file-header? ip)
                      (let ([libs-in-file '()])
                        (let loop! ()
                          (let ([x (fasl-read ip)])
                            (if (eof-object? x) 
                                (begin
                                  (for-each
                                    (lambda (node)
                                      (unless (library-node-ctinfo node)
                                        ($oops who "missing compile-time information for ~s" (library-node-path node)))
                                      (unless (library-node-rtinfo node)
                                        ($oops who "missing run-time information for ~s" (library-node-path node))))
                                    libs-in-file)
                                  libs-in-file)
                                (begin
                                  (cond
                                    [(recompile-info? x)]
                                    [(procedure? x)]
                                    [(library/ct-info? x)
                                     (let ([node (record-ct-lib! x #t fn libs-visible?)])
                                       (when node (set! libs-in-file (cons node libs-in-file))))]
                                    [(library/rt-info? x)
                                     (let ([node (record-rt-lib! x #t fn libs-visible?)])
                                       (when node (set! libs-in-file (cons node libs-in-file))))]
                                    [(program-info? x) ($oops who "found program while looking for library ~s in ~a" path fn)]
                                    ; NB: this is here to support the #t inserted by compile-file-help2 after header information
                                    [(eq? x #t)]
                                    [else ($oops who "unexpected value ~s read from ~a" x fn)])
                                  (loop!))))))
                      ($oops who "malformed binary input file ~s" fn)))))))
        (define process-ir*!
          (lambda (ir* ifn capture-program? libs-visible?)
            (define outer-who who)
            (let ([libs-in-file '()] [maybe-program #f] [rcinfo* '()])
              (define-pass process-ir! : Lexpand (ir) -> * ()
                (Outer : Outer (ir situation) -> * ()
                  [(recompile-info ,rcinfo) (set! rcinfo* (cons rcinfo rcinfo*)) (values)]
                  [(group ,[] ,[]) (values)]
                  [(visit-only ,[inner 'visit ->]) (values)]
                  [(revisit-only ,[inner 'revisit ->]) (values)])
                (Inner : Inner (ir situation) -> * ()
                  [,lsrc ($oops outer-who "expected program or library form, but encountered top-level expression ~s processing file ~a" ($uncprep lsrc) ifn)]
                  [(library/ct-info ,linfo/ct)
                   (let ([node (record-ct-lib! linfo/ct #f ifn libs-visible?)])
                     (when node (set! libs-in-file (cons node libs-in-file))))
                   (values)]
                  [(library/rt-info ,linfo/rt)
                   (let ([node (record-rt-lib! linfo/rt #f ifn libs-visible?)])
                     (when node (set! libs-in-file (cons node libs-in-file))))
                   (values)]
                  [(program-info ,pinfo)
                    (unless capture-program? ($oops outer-who "found program while reading library wpo file ~a" ifn))
                    (when (eq? situation 'visit) ($oops outer-who "encountered visit-only program while processing file ~s" ifn))
                    (when maybe-program ($oops outer-who "found multiple programs in entry file ~a" ifn))
                    (set! maybe-program (make-program-node pinfo))
                    (values)])
                (Program : Program (ir situation) -> * ()
                  [(program ,uid ,body)
                   (unless capture-program? ($oops outer-who "found program while reading library wpo file ~a" ifn))
                   (when (eq? situation 'visit) ($oops outer-who "encountered visit-only program while processing file ~s" ifn))
                   (unless maybe-program ($oops outer-who "unable to locate program descriptor for ~s" uid))
                   (unless (eq? uid (program-node-uid maybe-program))
                     ($oops outer-who "expected code for program uid ~s, but found code for program uid ~s" (program-node-uid maybe-program) uid))
                   (program-node-ir-set! maybe-program ir)
                   (values)])
                (ctLibrary : ctLibrary (ir situation) -> * ()
                  [(library/ct ,uid (,export-id* ...) ,import-code ,visit-code)
                   (when (eq? situation 'revisit) ($oops outer-who "encountered revisit-only compile-time library ~s while processing file ~s" (lookup-path uid) ifn))
                   (record-ct-lib-ir! uid ir)
                   (values)])
                (rtLibrary : rtLibrary (ir situation) -> * ()
                  [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
                   (when (eq? situation 'visit) ($oops outer-who "encountered visit-only run-time library ~s while processing file ~s" (lookup-path uid) ifn))
                   (record-rt-lib-ir! uid ir)
                   (values)])
                (when capture-wpo? (set! wpo* (cons ir wpo*)))
                (Outer ir 'load))
              (for-each process-ir! ir*)
              (for-each
                (lambda (node)
                  (unless (library-node-ctinfo node)
                    ($oops who "missing compile-time information for ~s" (library-node-path node)))
                  (unless (library-node-rtinfo node)
                    ($oops who "missing run-time information for ~s" (library-node-path node)))
                  (unless (library-node-ctir node)
                    ($oops who "missing compile-time code for ~s" (library-node-path node)))
                  (unless (library-node-rtir node)
                    ($oops who "missing run-time code for ~s" (library-node-path node))))
                libs-in-file)
              (values maybe-program libs-in-file rcinfo*))))
        (define record-ct-lib!
          (lambda (linfo/ct binary? ifn libs-visible?)
            (let* ([uid (library-info-uid linfo/ct)]
                   [cell (symbol-hashtable-cell libs uid #f)]
                   [node (cdr cell)])
              (if node
                  (if (library-node-ctinfo node)
                      ($oops who "encountered library ~s in ~a, but had already encountered it in ~a"
                        (library-info-path linfo/ct) ifn (library-node-fn node))
                      (begin (library-node-ctinfo-set! node linfo/ct) #f))
                  (let ([node (make-library-node binary? linfo/ct #f (or libs-visible? binary?) ifn)])
                    (set-cdr! cell node)
                    node)))))
        (define record-rt-lib!
          (lambda (linfo/rt binary? ifn libs-visible?)
            (let* ([uid (library-info-uid linfo/rt)]
                   [cell (symbol-hashtable-cell libs uid #f)]
                   [node (cdr cell)])
              (if node
                  (if (library-node-rtinfo node)
                      ($oops who "encountered library ~s in ~a, but had already encountered it in ~a"
                        (library-info-path linfo/rt) ifn (library-node-fn node))
                      (begin (library-node-rtinfo-set! node linfo/rt) #f))
                  (let ([node (make-library-node binary? #f linfo/rt (or libs-visible? binary?) ifn)])
                    (set-cdr! cell node)
                    node)))))
        (define record-ct-lib-ir!
          (lambda (uid ir)
            (let ([node (symbol-hashtable-ref libs uid #f)])
              (unless node ($oops "missing descriptor for compile-time library code ~s" uid))
              (library-node-ctir-set! node ir))))
        (define record-rt-lib-ir!
          (lambda (uid ir)
            (let ([node (symbol-hashtable-ref libs uid #f)])
              (unless node ($oops "missing descriptor for run-time library code ~s" uid))
              (library-node-rtir-set! node ir))))
        (define chase-library
          (lambda (req libs-visible?)
            (let ([a (symbol-hashtable-cell libs (libreq-uid req) #f)])
              (cond
                [(cdr a) =>
                 (lambda (node)
                   (when libs-visible?
                     (unless (library-node-visible? node)
                       (library-node-visible?-set! node #t)
                       (chase-library-dependencies! node))))]
                [else
                 (let ([path (libreq-path req)])
                   (let-values ([(fn node*) (read-library path libs-visible?)])
                     (unless (symbol-hashtable-ref libs (libreq-uid req) #f)
                       ($oops who "~s does not define expected compilation instance of library ~s" fn path))
                     (for-each chase-library-dependencies! node*)))]))))
        (define find-dependencies
          (lambda (req* maybe-import-req*)
            (let ([dep* (map (lambda (req)
                               (let ([node (symbol-hashtable-ref libs (libreq-uid req) #f)])
                                 (node-use-count-set! node (fx+ (node-use-count node) 1))
                                 node))
                             req*)])
              (if maybe-import-req*
                  (fold-right (lambda (req dep*)
                                (let ([node (symbol-hashtable-ref libs (libreq-uid req) #f)])
                                  (if node
                                      (begin
                                        (node-use-count-set! node (fx+ (node-use-count node) 1))
                                        (cons node dep*))
                                      dep*)))
                    dep* maybe-import-req*)
                  dep*))))
        (define chase-program-dependencies!
          (lambda (node)
            (for-each (lambda (req) (chase-library req libs-visible?)) (program-node-invoke-req* node))
            (node-depend*-set! node (find-dependencies (program-node-invoke-req* node) #f))))
        (define chase-library-dependencies!
          (lambda (node)
            (if (library-node-visible? node)
                (for-each
                  (lambda (req)
                    (unless ($system-library? (libreq-path req))
                      (chase-library req (library-node-visible? node))))
                  (library-node-import-req* node))
                (for-each
                  (lambda (req) (chase-library req (library-node-visible? node)))
                  (library-node-invoke-req* node)))
            (unless (node-depend* node)
              (node-depend*-set! node
                (find-dependencies
                  (library-node-invoke-req* node)
                  (and (library-node-visible? node) (library-node-import-req* node)))))))
        (let-values ([(maybe-program node* rcinfo*) (process-ir*! ir* ifn capture-program? libs-visible?)])
          (when capture-program?
            (unless maybe-program ($oops who "missing entry program in file ~a" ifn))
            (unless (program-node-ir maybe-program) ($oops who "loading ~a did not define expected program pieces" ifn))
            (chase-program-dependencies! maybe-program))
          (for-each chase-library-dependencies! node*)
          (let-values ([(visible* invisible*) (partition library-node-visible? (vector->list (hashtable-values libs)))])
            (values maybe-program visible* invisible* rcinfo* wpo*))))))

  (define topological-sort
    (lambda (program-entry library-entry*)
      (define topological-sort
        (lambda (dep* node*)
          (if (null? dep*)
              node*
              (let* ([dep (car dep*)] [use-count (node-use-count dep)])
                (node-use-count-set! dep (fx- use-count 1))
                (if (fx= use-count 1)
                    (topological-sort (cdr dep*) (topological-sort (node-depend* dep) (cons dep node*)))
                    (topological-sort (cdr dep*) node*))))))
      (fold-right
        (lambda (entry node*) (topological-sort (node-depend* entry) (cons entry node*)))
        (if program-entry (topological-sort (node-depend* program-entry) '()) '())
        (filter (lambda (node) (fx= (node-use-count node) 0)) library-entry*))))

  (define void-pr (lookup-primref 3 'void))

  (with-output-language (Lsrc Expr)
    (define build-install-library/ct-code
      (lambda (node)
        (nanopass-case (Lexpand ctLibrary) (library-node-ctir node)
          [(library/ct ,uid (,export-id* ...) ,import-code ,visit-code)
           (if (library-node-visible? node)
               ($build-install-library/ct-code uid export-id* import-code visit-code)
               void-pr)])))

    (define build-void (let ([void-rec `(quote ,(void))]) (lambda () void-rec)))

    (define gen-var (lambda (sym) (make-prelex sym 0 #f #f)))
    (define build-let
      (lambda (ids exprs body)
        `(call ,(make-preinfo-call) ,(build-lambda ids body) ,exprs ...)))

    (define build-lambda
      (lambda (ids body)
        `(case-lambda ,(make-preinfo-lambda)
           (clause (,ids ...) ,(length ids) ,body))))

    (define build-call
      (lambda (e . e*)
        `(call ,(make-preinfo-call) ,e ,e* ...)))

    (define-syntax build-primcall
      ; written as a macro to give lookup-primref a chance to lookup the primref at expansion time
      (syntax-rules ()
        [(_ ?name ?arg ...) (build-call (lookup-primref 3 ?name) ?arg ...)]))

    (define-syntax build-primref
      (syntax-rules ()
        [(_ ?level ?name) (lookup-primref ?level ?name)]))

    (define build-install-library/rt-code
      (lambda (node thunk)
        (build-primcall '$install-library/rt-code `(quote ,(library-node-uid node)) thunk)))

    (define-pass patch : Lsrc (ir env exts-table) -> Lsrc ()
      (definitions
        (define with-initialized-ids
          (lambda (old-id* proc)
            (let ([new-id* (map (lambda (old-id)
                                  (let ([new-id (make-prelex
                                                  (prelex-name old-id)
                                                  (let ([flags (prelex-flags old-id)])
                                                    (fxlogor
                                                      (fxlogand flags (constant prelex-sticky-mask))
                                                      (fxsll (fxlogand flags (constant prelex-is-mask))
                                                        (constant prelex-was-flags-offset))))
                                                  (prelex-source old-id)
                                                  #f)])
                                    (prelex-operand-set! old-id new-id)
                                    new-id))
                             old-id*)])
              (let-values ([v* (proc new-id*)])
                (for-each (lambda (old-id) (prelex-operand-set! old-id #f)) old-id*)
                (apply values v*)))))
        (define build-ref
          (case-lambda
            [(x) (build-ref #f x)]
            [(src x)
             (let ([x (prelex-operand x)])
               (safe-assert (prelex? x))
               (if (prelex-referenced x)
                   (set-prelex-multiply-referenced! x #t)
                   (set-prelex-referenced! x #t))
               `(ref ,src ,x))])))
      (Expr : Expr (ir) -> Expr ()
        [(ref ,maybe-src ,x) (build-ref maybe-src x)]
        [(call ,preinfo ,pr (quote ,d))
         (guard (eq? (primref-name pr) '$top-level-value) (symbol? d))
         (cond
           [(symbol-hashtable-ref env d #f) => (lambda (x) (build-ref (preinfo-src preinfo) x))]
           [else ir])]
        [(set! ,maybe-src ,x ,[e])
         (let ([x (prelex-operand x)])
           (safe-assert (prelex? x))
           (set-prelex-assigned! x #t)
           `(set! ,maybe-src ,x ,e))]
        [(letrec ([,x* ,e*] ...) ,body)
         (with-initialized-ids x*
           (lambda (x*)
             `(letrec ([,x* ,(map Expr e*)] ...) ,(Expr body))))]
        [(letrec* ([,x* ,e*] ...) ,body)
         (with-initialized-ids x*
           (lambda (x*)
             `(letrec* ([,x* ,(map Expr e*)] ...) ,(Expr body))))]
        [(cte-optimization-loc ,box ,e ,exts)
         (define new-exts (or (hashtable-ref exts-table exts #f)
                              (let ([new-exts (map (lambda (p)
                                                     (let ([x (car p)])
                                                       (cons (or (prelex-operand x) x) (cdr p))))
                                                   exts)])
                                (hashtable-set! exts-table exts new-exts)
                                new-exts)))
         (let ([e (Expr e)])
           `(cte-optimization-loc ,box ,e ,new-exts))])
      (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
        [(clause (,x* ...) ,interface ,body)
         (with-initialized-ids x*
           (lambda (x*)
             `(clause (,x* ...) ,interface ,(Expr body))))]))

    (define build-top-level-set!*
      (lambda (node)
        (nanopass-case (Lexpand rtLibrary) (library-node-rtir node)
          [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
           (let ([exts ($build-library-exts dl* dv*)])
             (fold-right
               (lambda (dl db dv body)
                 (if dl
                     `(seq ,(build-primcall '$set-top-level-value! `(quote ,dl)
                              `(cte-optimization-loc ,db (ref #f ,dv) ,exts))
                        ,body)
                     body))
               (build-void) dl* db* dv*))])))

    (define make-patch-env
      (lambda (cluster*)
        (let ([patch-env (make-hashtable symbol-hash eq?)])
          (for-each
            (lambda (cluster)
              (for-each
                (lambda (node)
                  (unless (library-node-binary? node)
                    (nanopass-case (Lexpand rtLibrary) (library-node-rtir node)
                      [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
                       (for-each (lambda (label var)
                                   (when label
                                     (symbol-hashtable-set! patch-env label var)))
                         dl* dv*)])))
                cluster))
            cluster*)
          patch-env)))

    (define build-combined-program-ir
      (lambda (program node*)
        `(seq
           ,(build-primcall 'for-each 
              (build-primref 3 '$mark-pending!)
              `(quote ,(map library-node-uid (remp library-node-binary? node*))))
           ,(patch
              (fold-right
                (lambda (node combined-body)
                  (if (library-node-binary? node)
                      `(seq
                         ,(build-primcall '$invoke-library
                            `(quote ,(library-node-path node))
                            `(quote ,(library-node-version node))
                            `(quote ,(library-node-uid node)))
                         ,combined-body)
                      (nanopass-case (Lexpand rtLibrary) (library-node-rtir node)
                        [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
                         `(letrec* ([,dv* ,de*] ...)
                            (seq ,body
                              (seq
                                ,(build-install-library/rt-code node
                                   (if (library-node-visible? node)
                                       (build-lambda '() (build-top-level-set!* node))
                                       void-pr))
                                ,combined-body)))])))
                (nanopass-case (Lexpand Program) (program-node-ir program)
                  [(program ,uid ,body) body])
                node*)
              (make-patch-env (list node*))
              (make-eq-hashtable)))))

    (define build-combined-library-ir
      (lambda (cluster*)
        (define build-mark-invoked!
          (lambda (node)
            (build-primcall '$mark-invoked! `(quote ,(library-node-uid node)))))
        
        (define build-cluster
          (lambda (node* cluster-body)
            (fold-right
              (lambda (node cluster-body)
                (nanopass-case (Lexpand rtLibrary) (library-node-rtir node)
                  [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
                   `(letrec* ([,dv* ,de*] ...)
                      (seq ,body
                        (seq
                          ,(if (library-node-visible? node)
                               `(seq ,(build-top-level-set!* node) ,(build-mark-invoked! node))
                               (build-mark-invoked! node))
                          ,cluster-body)))]))
              cluster-body node*)))
        (patch
          ; example: D imports C; C imports A, B; B imports A; A imports nothing
          ;          have wpos for D, A, B; obj for C
          ; (let ([lib-f (void)])
          ;   (set! lib-f
          ;     (lambda (idx)
          ;       (letrec ([A-local ---] ...)
          ;         A-body
          ;         (begin ($top-level-set! A-export A-local) ...)
          ;         (letrec ([B-local ---] ...)
          ;           B-body
          ;           (begin ($top-level-set! B-export B-local) ...)
          ;           (let ([t (lambda (idx)
          ;                      (letrec ([D-local ---] ...)
          ;                        D-body
          ;                        (begin ($top-level-set! D-export B-local) ...)
          ;                        (set! lib-f (lambda (idx) (void)))))])
          ;             (if (eqv? idx 0)
          ;                 (set! lib-f t)
          ;                 (t idx)))))))
          ;   ($install-library/rt-code 'A-uid (lambda () (lib-f 0)))
          ;   ($install-library/rt-code 'B-uid (lambda () (lib-f 0)))
          ;   ($install-library/rt-code 'D-uid (lambda () (lib-f 1)))
          ;   (void))
          (let ([lib-f (gen-var 'lib-f)])
            (let ([cluster-idx* (enumerate cluster*)])
              (build-let (list lib-f) (list (build-void))
                `(seq 
                   (set! #f ,lib-f
                     ,(let f ([cluster* cluster*] [cluster-idx* cluster-idx*])
                        (let ([idx (gen-var 'idx)])
                          (build-lambda (list idx)
                            (build-cluster (car cluster*)
                              (let ([cluster* (cdr cluster*)])
                                (if (null? cluster*)
                                    (let ([idx (gen-var 'idx)])
                                      `(set! #f ,lib-f ,(build-lambda (list idx) (build-void))))
                                    (let ([t (gen-var 't)])
                                      (build-let (list t) (list (f cluster* (cdr cluster-idx*)))
                                        `(if ,(build-primcall 'eqv? `(ref #f ,idx) `(quote ,(car cluster-idx*)))
                                             (set! #f ,lib-f (ref #f ,t))
                                             ,(build-call `(ref #f ,t) `(ref #f ,idx))))))))))))
                   ,(fold-right (lambda (cluster cluster-idx body)
                                  (fold-right (lambda (node body)
                                                `(seq
                                                   ,(build-install-library/rt-code node
                                                      (if (library-node-visible? node)
                                                          (build-lambda '()
                                                            (build-call `(ref #f ,lib-f) `(quote ,cluster-idx)))
                                                          void-pr))
                                                   ,body))
                                    body cluster))
                      (build-void) cluster* cluster-idx*)))))
          (make-patch-env cluster*)
          (make-eq-hashtable)))))

  (with-output-language (Lexpand Outer)
    (define add-recompile-info
      (lambda (rcinfo* body)
        (fold-left
          (lambda (body rcinfo)
            `(group (recompile-info ,rcinfo) ,body))
          body
          rcinfo*)))

    (define requirements-join
      (lambda (req* maybe-collected-invoke-req*)
        (define (->libreq node)
          (make-libreq
            (library-node-path node)
            (library-node-version node)
            (library-node-uid node)))
        (if maybe-collected-invoke-req*
            (let f ([invoke-req* maybe-collected-invoke-req*])
              (if (null? invoke-req*)
                  req*
                  (let* ([invoke-req (car invoke-req*)] [uid (library-node-uid invoke-req)])
                    (if (memp (lambda (req) (eq? (libreq-uid req) uid)) req*)
                        (f (cdr invoke-req*))
                        (cons (->libreq invoke-req) (f (cdr invoke-req*)))))))
            req*)))

    (define add-library/rt-records
      (lambda (maybe-ht node* body)
        (fold-left
          (lambda (body node)
            (if (library-node-binary? node)
                body
                (let* ([info (library-node-rtinfo node)]
                       [uid (library-info-uid info)])
                  `(group (revisit-only
                            (library/rt-info
                              ,(make-library/rt-info
                                 (library-info-path info)
                                 (library-info-version info)
                                 uid
                                 (library-node-visible? node)
                                 (requirements-join
                                   (library/rt-info-invoke-req* info)
                                   (and maybe-ht (symbol-hashtable-ref maybe-ht uid #f))))))
                     ,body))))
          body node*)))

    (define add-library/ct-records
      (lambda (maybe-ht visit-lib* body)
        (fold-left
          (lambda (body visit-lib)
            (if (library-node-binary? visit-lib)
                body
                (let* ([info (library-node-ctinfo visit-lib)]
                       [uid (library-info-uid info)])
                  `(group (visit-only
                            (library/ct-info
                              ,(make-library/ct-info
                                 (library-info-path info)
                                 (library-info-version info)
                                 uid
                                 (library-node-visible? visit-lib)
                                 (requirements-join
                                   (library/ct-info-import-req* info)
                                   (and maybe-ht (symbol-hashtable-ref maybe-ht uid #f)))
                                 (library/ct-info-visit-visit-req* info)
                                 (library/ct-info-visit-req* info))))
                     ,body))))
          body visit-lib*)))

    (define add-program-record
      (lambda (node body)
        `(group (revisit-only
                  (program-info
                    ,(make-program-info
                       (program-node-uid node)
                       ; NB: possibly list direct or indirect binary library reqs here
                       (program-node-invoke-req* node))))
           ,body)))

    (define add-visit-lib-install*
      (lambda (visit-lib* body)
        (fold-left (lambda (body visit-lib)
                     (if (library-node-binary? visit-lib)
                         body
                         `(group (visit-only ,(build-install-library/ct-code visit-lib)) ,body)))
          body visit-lib*)))

    (define build-cluster*
      (lambda (node* ht)
        (define (add-deps! node deps)
          (symbol-hashtable-set! ht (library-node-uid node) deps))
        (define (s-entry/binary node* rcluster* deps)
          (if (null? node*)
              (reverse rcluster*)
              (let ([node (car node*)])
                (if (library-node-binary? node)
                    (s-entry/binary (cdr node*) rcluster* (cons node deps))
                    (begin
                      (add-deps! node deps)
                      (s-source (cdr node*) (list node) rcluster* (list node)))))))
        (define (s-source node* rnode* rcluster* deps)
          (if (null? node*)
              (reverse (cons (reverse rnode*) rcluster*))
              (let ([node (car node*)])
                (if (library-node-binary? node)
                    (s-entry/binary (cdr node*) (cons (reverse rnode*) rcluster*)
                      (cons node deps))
                    (begin
                      (add-deps! node deps)
                      (s-source (cdr node*) (cons node rnode*) rcluster* deps))))))
        (s-entry/binary node* '() '())))

    (define build-program-body
      (lambda (program-entry node* visit-lib* invisible* rcinfo*)
        (add-recompile-info rcinfo*
          (add-library/rt-records #f node*
            (add-library/ct-records #f visit-lib*
              (add-library/ct-records #f invisible*
                (add-program-record program-entry
                  (add-visit-lib-install* visit-lib*
                    (add-visit-lib-install* invisible*
                      `(revisit-only ,(build-combined-program-ir program-entry node*)))))))))))

    (define build-library-body
      (lambda (node* visit-lib* rcinfo*)
        (let* ([collected-req-ht (make-hashtable symbol-hash eq?)]
               [cluster* (build-cluster* node* collected-req-ht)])
          (add-recompile-info rcinfo*
            (add-library/rt-records collected-req-ht node*
              (add-library/ct-records collected-req-ht visit-lib*
                (add-visit-lib-install* visit-lib*
                  `(revisit-only ,(build-combined-library-ir cluster*))))))))))

  (define finish-compile
    (lambda (who msg ifn ofn hash-bang-line x1)
      (with-object-file who ofn
        (lambda (op)
          (with-coverage-file who ofn
            (lambda (source-table)
              (when hash-bang-line (put-bytevector op hash-bang-line))
              (parameterize ([$target-machine (constant machine-type-name)]
                             ; dummy sfd for block-profile optimization
                             [$sfd (make-source-file-descriptor ifn #xc7 #xc7c7)]
                             [$block-counter 0])
                (when source-table ($insert-profile-src! source-table x1))
                (emit-header op (constant scheme-version) (constant machine-type))
                (let-values ([(rcinfo* lpinfo* final*) (compile-file-help1 x1 msg)])
                  (compile-file-help2 op (list rcinfo*) (list lpinfo*) (list final*) #f #f)))))))))

  (define write-wpo-file
    (lambda (who ofn ir*)
      (with-wpo-file who ofn
        (lambda (wpoop)
          (when wpoop
            (emit-header wpoop (constant scheme-version) (host-machine-type))
            ($with-fasl-target (host-machine-type)
              (lambda ()
                (parameterize ([$target-machine (machine-type)])
                  (let ([t ($fasl-table)])
                    (let ([x (fold-left (lambda (outer ir) (with-output-language (Lexpand Outer) `(group ,outer ,ir)))
                               (car ir*) (cdr ir*))])
                      ($fasl-enter x t (constant annotation-all) 0)
                      ($fasl-start wpoop t (constant fasl-type-visit-revisit) x (constant annotation-all)
                                   (lambda (x p) ($fasl-out x p t (constant annotation-all))))))))))))))

  (define build-required-library-list
    (lambda (node* visit-lib*)
      (let ([ht (make-hashtable symbol-hash eq?)])
        (fold-left
          (lambda (ls node)
            (if (and (library-node-binary? node) (not (symbol-hashtable-contains? ht (library-node-uid node))))
                (cons (library-node-path node) ls)
                ls))
          (fold-left
            (lambda (ls node)
              (if (library-node-binary? node)
                  (begin
                    (symbol-hashtable-set! ht (library-node-uid node) #t)
                    (cons (library-node-path node) ls))
                  ls))
            '() node*)
          visit-lib*))))

  ;; TODO: Add automatic recompliation ala scheme import/load-library
  (set-who! compile-whole-program
    (rec compile-whole-program
      (case-lambda
        [(ifn ofn) (compile-whole-program ifn ofn #f)]
        [(ifn ofn libs-visible?)
         (unless (string? ifn) ($oops who "~s is not a string" ifn))
         (unless (string? ofn) ($oops who "~s is not a string" ofn))
         (let*-values ([(hash-bang-line ir*) (read-input-file who ifn)]
                       [(program-entry lib* invisible* rcinfo* no-wpo*) (build-graph who ir* ifn #t #f libs-visible?)])
           (safe-assert program-entry)
           (safe-assert (null? no-wpo*))
           (let ([node* (topological-sort program-entry lib*)])
             (finish-compile who "whole program" ifn ofn hash-bang-line
               (build-program-body program-entry node* lib* invisible* rcinfo*))
             (build-required-library-list node* lib*)))])))

  (set-who! compile-whole-library
    (lambda (ifn ofn)
      (unless (string? ifn) ($oops who "~s is not a string" ifn))
      (unless (string? ofn) ($oops who "~s is not a string" ofn))
      (let*-values ([(hash-bang-line ir*) (read-input-file who ifn)]
                    [(no-program lib* invisible* rcinfo* wpo*) (build-graph who ir* ifn #f (generate-wpo-files) #t)])
        (safe-assert (not no-program))
        (safe-assert (null? invisible*))
        (safe-assert (or (not (generate-wpo-files)) (not (null? wpo*))))
        (when (null? lib*) ($oops "did not find libraries in input file ~s" ifn))
        (let ([node* (topological-sort #f lib*)])
          (write-wpo-file who ofn wpo*)
          (finish-compile who "whole library" ifn ofn hash-bang-line
            (build-library-body node* lib* rcinfo*))
          (build-required-library-list node* lib*))))))

(set! $c-make-code
   (lambda (func subtype free name arity-mask size code-list info pinfo*)
      (let ([code `(code ,func
                         ,subtype
                         ,free
                         ,(if (symbol? name)
                              (symbol->string name)
                              (and (string? name) name))
                         ,arity-mask
                         ,size
                         ,code-list
                         ,info
                         ,pinfo*)])
         (set-$c-func-code-record! func code)
         code)))

(set! $c-make-closure
  (lambda (func)
    (or ($c-func-closure-record func)
        (let ([x `(closure . ,func)])
          (set-$c-func-closure-record! func x)
          x))))

(set-who! compile
  (rec compile
    (case-lambda
      [(x0)
       (compile x0
         (if (eq? (subset-mode) 'system)
             ($system-environment)
             (interaction-environment)))]
      [(x0 env-spec)
       (define-pass expand-Lexpand : Lexpand (ir) -> Lsrc ()
         (Inner : Inner (ir) -> Expr ()
           [,lsrc lsrc]
           [(program ,uid ,body) ($build-invoke-program uid body)]
           [(library/ct ,uid (,export-id* ...) ,import-code ,visit-code)
            ($build-install-library/ct-code uid export-id* import-code visit-code)]
           [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
            ($build-install-library/rt-code uid dl* db* dv* de* body)]
           [else (sorry! who "unexpected Lexpand record ~s" ir)])
         (Outer : Outer (ir) -> Expr ()
           [(group ,[e1] ,[e2]) `(seq ,e1 ,e2)]
           [,inner (Inner inner)]
           [else (sorry! who "unexpected Lexpand record ~s" ir)]))
       (unless (environment? env-spec) ($oops who "~s is not an environment" env-spec))
       ((parameterize ([$target-machine (constant machine-type-name)] [$sfd #f])
          (let* ([x1 (expand-Lexpand ($pass-time 'expand (lambda () (expand x0 env-spec #t))))]
                 [waste ($uncprep x1 #t)] ; populate preinfo sexpr fields
                 [waste (when (and (expand-output) (not ($noexpand? x0)))
                          (pretty-print ($uncprep x1) (expand-output))
                          (flush-output-port (expand-output)))]
                 [x2 ($pass-time 'cpvalid (lambda () ($cpvalid x1)))]
                 [x2a (let ([cpletrec-ran? #f])
                        (let ([x ((run-cp0)
                                  (lambda (x)
                                    (set! cpletrec-ran? #t)
                                    (let* ([x ($pass-time 'cp0 (lambda () ($cp0 x)))]
                                           [x (cptypes x)])
                                      ($pass-time 'cpletrec (lambda () ($cpletrec x)))))
                                  x2)])
                          (if cpletrec-ran? x
                              ($pass-time 'cpletrec (lambda () ($cpletrec x))))))]
                 [x2b ($pass-time 'cpcheck (lambda () ($cpcheck x2a)))]
                 [x2b ($pass-time 'cpcommonize (lambda () ($cpcommonize x2b)))])
            (when (and (expand/optimize-output) (not ($noexpand? x0)))
              (pretty-print ($uncprep x2b) (expand/optimize-output))
              (flush-output-port (expand/optimize-output)))
            (if (and (compile-interpret-simple)
                     (not ($assembly-output))
                     (cheat? x2b))
                (lambda () (cheat-eval x2b))
                ($compile-backend x2b)))))])))

(set! $compile-backend
  (lambda (x2)
    (c-mkcode (c-compile x2))))

(let ()
  (define emit-boot-header
    (lambda (op machine bootfiles)
      (emit-header op (constant scheme-version) (constant machine-type) (map path-root (map path-last bootfiles)))
      (when (null? bootfiles)
        (parameterize ([$target-machine machine] [$sfd #f])
          (c-print-fasl ($np-boot-code 'error-invoke) op (constant fasl-type-visit-revisit) #f #f)
          (c-print-fasl ($np-boot-code 'invoke) op (constant fasl-type-visit-revisit) #f #f)
          ($fasl-base-rtd #!base-rtd op)))))

  (define do-make-boot-file
    (lambda (who outfn machine bootfile* infn*)
      (unless (string? outfn) ($oops who "~s is not a string" outfn))
      (unless (symbol? machine) ($oops who "~s is not a symbol" machine))
      (unless (eq? machine (constant machine-type-name))
        ($oops who "compiler for ~s is not loaded" machine))
      (unless (and (list? bootfile*) (andmap string? bootfile*))
        ($oops who "~s is not a list of strings" bootfile*))
      (for-each
        (lambda (infn) (unless (string? infn) ($oops who "~s is not a string" infn)))
        infn*)
      (with-object-file who outfn
        (lambda (op)
          (with-coverage-file who outfn
            (lambda (source-table)
              (unless (and (eq? who 'make-boot-file) (null? bootfile*))
                (emit-boot-header op machine bootfile*))
              (for-each
                (lambda (infn)
                  (let ([ip ($open-file-input-port who infn)])
                    (on-reset (close-port ip)
                      (if ($compiled-file-header? ip)
                          (begin
                            (let* ([bufsiz (file-buffer-size)] [buf (make-bytevector bufsiz)])
                              (let loop ()
                                (let ([n (get-bytevector-n! ip buf 0 bufsiz)])
                                  (unless (eof-object? n)
                                    (put-bytevector op buf 0 n)
                                    (loop)))))
                            (when source-table
                              (guard (c [else (void)])
                                (let ([ip ($open-file-input-port who (new-extension "covin" infn)
                                            (file-options compressed)
                                            (buffer-mode block)
                                            (current-transcoder))])
                                  (on-reset (close-port ip)
                                    (get-source-table! ip source-table))
                                  (close-port ip)))))
                          (let ([sfd ($source-file-descriptor infn ip)])
                            ; whack ip so close-port calls close the text port
                            (set! ip (transcoded-port ip (current-transcoder)))
                            (compile-file-help op #f #f source-table machine sfd ($make-read ip sfd 0) outfn #f #f))))
                    (close-port ip)))
                infn*)))))))

  (define do-make-boot-header
    ; create boot loader (invoke) for entry into Scheme from C
    (lambda (who out machine bootfiles)
      (unless (string? out) ($oops who "~s is not a string" out))
      (unless (symbol? machine) ($oops who "~s is not a symbol" machine))
      (unless (eq? machine (constant machine-type-name))
        ($oops who "compiler for ~s is not loaded" machine))
      (for-each (lambda (x)
                  (unless (string? x)
                    ($oops who "~s is not a string" x)))
        bootfiles)
      (with-object-file who out
        (lambda (op)
          (emit-boot-header op machine bootfiles)))))

  (set-who! make-boot-file
    (lambda (outfn bootfile* . infn*)
      (do-make-boot-file who outfn (constant machine-type-name) bootfile* infn*)))

  (set-who! $make-boot-file
    (lambda (outfn machine bootfile* . infn*)
      (do-make-boot-file who outfn machine bootfile* infn*)))

  (set-who! make-boot-header
   ; exported interface: machine-type implicit and requires one or more
   ; subordinate boot files
    (lambda (out bootfile . bootfiles)
      (do-make-boot-header who out (constant machine-type-name) (cons bootfile bootfiles))))

  (set-who! $make-boot-header
    ; create boot loader (invoke) for entry into Scheme from C
    (lambda (out machine . bootfiles)
      (do-make-boot-header who out machine bootfiles)))

  (set! $emit-boot-header emit-boot-header)
  )

(set-who! $write-fasl-bytevectors
  (lambda (p bv* size situation kind)
    (define (append-bvs bv*)
      (if (and (pair? bv*)
               (null? (cdr bv*)))
          (car bv*)
          (let f ([bv* bv*] [n 0])
            (if (null? bv*)
                (if (fixnum? n)
                    (make-bytevector n)
                    ($oops 'fasl-write "fasl output is too large to compress"))
                (let ([bv1 (car bv*)])
                  (let ([m (bytevector-length bv1)])
                    (let ([bv2 (f (cdr bv*) (+ n m))])
                      (bytevector-copy! bv1 0 bv2 n m)
                      bv2)))))))
    (put-u8 p situation)
    (if (and (>= size 100) (fasl-compressed))
        (let* ([fmt ($tc-field 'compress-format ($tc))]
               [bv (append-bvs bv*)]
               [uncompressed-size-bv (call-with-bytevector-output-port (lambda (bvp) (put-uptr bvp (bytevector-length bv))))]
               [bv ($bytevector-compress bv fmt)])
          (put-uptr p (+ 2 (bytevector-length uncompressed-size-bv) (bytevector-length bv)))
          (put-u8 p 
                  (cond
                    [(eqv? fmt (constant COMPRESS-GZIP)) (constant fasl-type-gzip)]
                    [(eqv? fmt (constant COMPRESS-LZ4)) (constant fasl-type-lz4)]
                    [else ($oops 'fasl-write "unexpected $compress-format value ~s" fmt)]))
          (put-u8 p kind)
          (put-bytevector p uncompressed-size-bv)
          (put-bytevector p bv))
        (begin
          (put-uptr p (+ size 2))
          (put-u8 p (constant fasl-type-uncompressed))
          (put-u8 p kind)
          (for-each (lambda (bv) (put-bytevector p bv)) bv*)))))

(let ()
  (define (libreq-hash x) (symbol-hash (libreq-uid x)))
  (define (libreq=? x y) (eq? (libreq-uid x) (libreq-uid y)))
  (define do-concatenate-object-files
    (lambda (who outfn infn*)
      (unless (string? outfn) ($oops who "~s is not a string" outfn))
      (for-each (lambda (infn) (unless (string? infn) ($oops who "~s is not a string" infn))) infn*)
      (let ([import-ht (make-hashtable libreq-hash libreq=?)]
            [include-ht (make-hashtable string-hash string=?)])
        (let in-loop ([infn* infn*] [rip* '()])
          (if (null? infn*)
              (let ([ip* (reverse rip*)])
                (with-object-file who outfn
                  (lambda (op)
                    (emit-header op (constant scheme-version) (constant machine-type))
                    (c-print-fasl `(object ,(make-recompile-info
                                              (vector->list (hashtable-keys import-ht))
                                              (vector->list (hashtable-keys include-ht))))
                      op (constant fasl-type-visit-revisit) #f #f)
                    (for-each (lambda (ip)
                                (let loop () ;; NB: This loop consumes one entry past the last library/program info record,
                                             ;; which we presume is the #t end-of-header marker.
                                  (let ([ty (lookahead-u8 ip)])
                                    (unless (eof-object? ty)
                                      ;; perhaps should verify ty here.
                                      (let ([x (fasl-read ip)])
                                        (when (or (library-info? x) (program-info? x))
                                          (c-print-fasl `(object ,x) op ty #f #f)
                                          (loop)))))))
                      ip*)
                    ;; inserting #t after lpinfo as an end-of-header marker
                    (c-print-fasl `(object #t) op (constant fasl-type-visit-revisit) #f #f)
                    (let* ([bufsiz (file-buffer-size)] [buf (make-bytevector bufsiz)])
                      (for-each (lambda (ip)
                                  (let loop ()
                                    (let ([n (get-bytevector-n! ip buf 0 bufsiz)])
                                      (unless (eof-object? n)
                                        (put-bytevector op buf 0 n)
                                        (loop))))
                                  (close-port ip))
                        ip*)))))
              (let* ([fn (car infn*)]
                     [ip ($open-file-input-port who fn)])
                (on-reset (close-port ip)
                  ;; NB: Does not currently support files beginning with a #! line.  Add that here if desired.
                  (unless ($compiled-file-header? ip) ($oops who "missing header for compiled file ~s" fn))
                  (let ([rcinfo (fasl-read ip)])
                    (unless (recompile-info? rcinfo) ($oops who "expected recompile info at start of ~s, found ~a" fn rcinfo))
                    (for-each
                      (lambda (x)
                        ;; NB: this could be enhanced to perform additional checks for compatible versions
                        (hashtable-set! import-ht x x))
                      (recompile-info-import-req* rcinfo))
                    (for-each
                      (lambda (x) (hashtable-set! include-ht x #t))
                      (recompile-info-include-req* rcinfo))
                    (in-loop (cdr infn*) (cons ip rip*))
                    ))))))))

  (set-who! concatenate-object-files
    (lambda (outfn infn0 . infn*)
      (do-concatenate-object-files who outfn (cons infn0 infn*))))
  )

(set-who! compile-port
  (rec compile-port
    (case-lambda
      [(ip op) (compile-port ip op #f)]
      [(ip op sfd) (compile-port ip op sfd #f)]
      [(ip op sfd wpoop) (compile-port ip op sfd wpoop #f)]
      [(ip op sfd wpoop covop) (compile-port ip op sfd wpoop covop (constant machine-type-name))]
      [(ip op sfd wpoop covop machine) (compile-port ip op sfd wpoop covop machine #f)]
      [(ip op sfd wpoop covop machine hostop)
       (unless (and (input-port? ip) (textual-port? ip))
         ($oops who "~s is not a textual input port" ip))
       (unless (and (output-port? op) (binary-port? op))
         ($oops who "~s is not a binary output port" op))
       (when ($port-flags-set? op (constant port-flag-compressed)) ($compressed-warning who op))
       (when sfd
         (unless (source-file-descriptor? sfd)
           ($oops who "~s is not a source-file descriptor or #f" sfd)))
       (when wpoop
         (unless (and (output-port? wpoop) (binary-port? wpoop))
           ($oops who "~s is not a binary output port or #f" wpoop))
         (when ($port-flags-set? wpoop (constant port-flag-compressed)) ($compressed-warning who wpoop)))
       (when covop
         (unless (and (output-port? covop) (textual-port? covop))
           ($oops who "~s is not a textual output port or #f" covop)))
       (unless (symbol? machine) ($oops who "~s is not a symbol" machine))
       (unless (eq? machine (constant machine-type-name))
         ($oops who "compiler for ~s is not loaded" machine))
       (when hostop
         (unless (and (output-port? hostop) (binary-port? hostop))
           ($oops who "~s is not a binary output port or #f" hostop))
         (when ($port-flags-set? hostop (constant port-flag-compressed)) ($compressed-warning who hostop)))
       (let ([source-table (and covop (make-source-table))])
         (let ([fp (and (port-has-port-position? ip)
                        (let ([fp (port-position ip)])
                          (if ($port-flags-set? ip (constant port-flag-char-positions))
                              fp
                              (and (eqv? fp 0) fp))))])
           (compile-file-help op hostop wpoop source-table machine sfd ($make-read ip sfd fp) #f #f #f)
           (when covop (put-source-table covop source-table))))])))

(set-who! compile-to-port
  (rec compile-to-port
    (case-lambda
      [(sexpr* op) (compile-to-port sexpr* op #f)]
      [(sexpr* op sfd) (compile-to-port sexpr* op sfd #f)]
      [(sexpr* op sfd wpoop) (compile-to-port sexpr* op sfd wpoop #f)]
      [(sexpr* op sfd wpoop covop) (compile-to-port sexpr* op sfd wpoop covop (constant machine-type-name))]
      [(sexpr* op sfd wpoop covop machine) (compile-to-port sexpr* op sfd wpoop covop machine #f)]
      [(sexpr* op sfd wpoop covop machine hostop) (compile-to-port sexpr* op sfd wpoop covop machine hostop #f)]
      [(sexpr* op sfd wpoop covop machine hostop external?-pred) (compile-to-port sexpr* op sfd wpoop covop machine hostop external?-pred #f)]
      [(sexpr* op sfd wpoop covop machine hostop external?-pred omit-rtds?)
       (define do-compile-to-port
         (lambda ()
           (let ([source-table (and covop (make-source-table))])
             (compile-file-help op hostop wpoop source-table machine sfd
               (lambda ()
                 (if (null? sexpr*)
                     (eof-object)
                     (let ([x (car sexpr*)])
                       (set! sexpr* (cdr sexpr*))
                       x)))
               (port-name op)
               external?-pred omit-rtds?)
             (when covop (put-source-table covop source-table)))))
       (unless (list? sexpr*)
         ($oops who "~s is not a proper list" sexpr*))
       (unless (and (output-port? op) (binary-port? op))
         ($oops who "~s is not a binary output port" op))
       (when ($port-flags-set? op (constant port-flag-compressed)) ($compressed-warning who op))
       (when sfd
         (unless (source-file-descriptor? sfd)
           ($oops who "~s is not a source-file descriptor or #f" sfd)))
       (when wpoop
         (unless (and (output-port? wpoop) (binary-port? wpoop))
           ($oops who "~s is not a binary output port or #f" wpoop))
         (when ($port-flags-set? wpoop (constant port-flag-compressed)) ($compressed-warning who wpoop)))
       (when covop
         (unless (and (output-port? covop) (textual-port? covop))
           ($oops who "~s is not a textual output port or #f" covop)))
       (unless (symbol? machine) ($oops who "~s is not a symbol" machine))
       (unless (eq? machine (constant machine-type-name))
         ($oops who "compiler for ~s is not loaded" machine))
       (when hostop
         (unless (and (output-port? hostop) (binary-port? hostop))
           ($oops who "~s is not a binary output port or #f" hostop))
         (when ($port-flags-set? hostop (constant port-flag-compressed)) ($compressed-warning who hostop)))
       (when external?-pred
         (unless (procedure? external?-pred)
           ($oops who "~s is not a procedure or #f" external?-pred))
         (unless (= (length sexpr*) 1)
           ($oops who "external predicate allowed only with a single expression")))
       (if (and (= (length sexpr*) 1) (pair? (car sexpr*)) (eq? (caar sexpr*) 'top-level-program))
           (let ([library-collector (make-parameter '())])
             (parameterize ([$require-libraries library-collector])
               (do-compile-to-port))
             (library-collector))
           (do-compile-to-port))])))

(let ()
  (define (in&out in)
    (let ([ext (path-extension in)])
      (cond
        [(string=? ext "") (values (format "~a.ss" in) (format "~a.so" in))]
        [(string=? ext "so") (values in (format "~a.so" in))]
        [else (values in (format "~a.so" (path-root in)))])))

  (define (do-compile-to-file who out hostout machine sfd do-read)
    (with-object-file who out
      (lambda (op)
        (with-host-file who hostout
          (lambda (hostop)
            (with-wpo-file who out
              (lambda (wpoop)
                (with-coverage-file who out
                  (lambda (source-table)
                    (compile-file-help op hostop wpoop source-table machine sfd do-read out #f #f))))))))))

  (define (do-compile-file who in out hostout machine r6rs?)
    (unless (string? in) ($oops who "~s is not a string" in))
    (unless (string? out) ($oops who "~s is not a string" out))
    (unless (symbol? machine) ($oops who "~s is not a symbol" machine))
    (unless (eq? machine (constant machine-type-name)) ($oops who "compiler for ~s is not loaded" machine))
    (when (compile-file-message) (printf "compiling ~a with output to ~a~@[ (host output to ~a)~]\n" in out hostout))
    (let ([ip ($open-file-input-port who in)])
      (on-reset (close-port ip)
        (let ([sfd ($source-file-descriptor in ip)])
          ; whack existing ip so close-port calls close the text port
          (set! ip (transcoded-port ip (current-transcoder)))
          (when r6rs? ($set-port-flags! ip (constant port-flag-r6rs)))
          (let ([fp (let ([start-pos (port-position ip)])
                      (if (and (eqv? (read-char ip) #\#)
                               (eqv? (read-char ip) #\!)
                               (memv (read-char ip) '(#\space #\/)))
                          (let loop ([fp 3])
                            (let ([c (read-char ip)])
                              (if (eof-object? c)
                                  fp
                                  (let ([fp (+ fp 1)])
                                    (if (char=? c #\newline)
                                        fp
                                        (loop fp))))))
                          (begin
                            (set-port-position! ip start-pos)
                            0)))])
            (do-compile-to-file who out hostout machine sfd ($make-read ip sfd fp)))))
      (close-port ip)))

  (define (do-compile-script who in out machine r6rs?)
    (define ($make-read-program ip sfd fp)
      (let ([do-read ($make-read ip sfd fp)])
        (lambda ()
          (let f ([form* '()])
            (let ([x (do-read)])
              (if (eof-object? x)
                  (if (null? form*) x `(top-level-program ,@(reverse form*)))
                  (f (cons x form*))))))))
    (unless (string? in) ($oops who "~s is not a string" in))
    (unless (string? out) ($oops who "~s is not a string" out))
    (unless (symbol? machine) ($oops who "~s is not a symbol" machine))
    (unless (eq? machine (constant machine-type-name)) ($oops who "compiler for ~s is not loaded" machine))
    (when (compile-file-message) (printf "compiling ~a with output to ~a\n" in out))
    (let ([ip ($open-file-input-port who in)])
      (on-reset (close-port ip)
        (let ([sfd ($source-file-descriptor in ip)])
          ; whack existing ip so close-port calls close the text port
          (set! ip (transcoded-port ip (current-transcoder)))
          (when r6rs? ($set-port-flags! ip (constant port-flag-r6rs)))
          (let ([start-pos (port-position ip)])
            (if (and (eqv? (read-char ip) #\#)
                     (eqv? (read-char ip) #\!)
                     (memv (lookahead-char ip) '(#\space #\/)))
                ; copy #! line
                (with-object-file who out
                  (lambda (op)
                    (with-wpo-file who out
                      (lambda (wpoop)
                        (with-coverage-file who out
                          (lambda (source-table)
                            (put-u8 op (char->integer #\#))
                            (put-u8 op (char->integer #\!))
                            (when wpoop (put-u8 wpoop (char->integer #\#)))
                            (when wpoop (put-u8 wpoop (char->integer #\!)))
                            (let ([fp (let loop ([fp 2])
                                        (let ([c (read-char ip)])
                                          (when (eof-object? c)
                                            ($oops who "unexpected eof reading script header on ~s" in))
                                          (let ([n (char->integer c)])
                                            (unless (fx< n 256)
                                              ($oops who
                                                "integer code for ~s script header character ~s is too large to copy to output port"
                                                in c))
                                            (put-u8 op n)
                                            (when wpoop (put-u8 wpoop n)))
                                          (let ([fp (+ fp 1)])
                                            (if (char=? c #\newline) fp (loop fp)))))])
                              (compile-file-help op #f wpoop source-table machine sfd ((if r6rs? $make-read-program $make-read) ip sfd fp) out #f #f))))))))
                ; no #! line
                (with-object-file who out
                  (lambda (op)
                    (set-port-position! ip start-pos)
                    (with-wpo-file who out
                      (lambda (wpoop)
                        (with-coverage-file who out
                          (lambda (source-table)
                            (compile-file-help op #f wpoop source-table machine sfd ((if r6rs? $make-read-program $make-read) ip sfd 0) out #f #f)))))))))))
      (close-port ip))
    (unless-feature windows (chmod out #o755)))

  (set-who! compile-file
    (case-lambda
      [(in out machine) (do-compile-file who in out #f machine #f)]
      [(in out) (do-compile-file who in out #f (constant machine-type-name) #f)]
      [(in)
       (unless (string? in) ($oops who "~s is not a string" in))
       (let-values ([(in out) (in&out in)])
         (do-compile-file who in out #f (constant machine-type-name) #f))]))

  (set-who! compile-library
    (let ()
      (define do-compile-library
        (lambda (in out machine)
          (do-compile-file who in out
            (and (not (eq? machine (machine-type)))
                 (format "~a.~s" (path-root out) (machine-type)))
            machine
            #t)))
      (case-lambda
        [(in out machine) (do-compile-library in out machine)]
        [(in out) (do-compile-library in out (constant machine-type-name))]
        [(in)
         (unless (string? in) ($oops who "~s is not a string" in))
         (let-values ([(in out) (in&out in)])
           (do-compile-library in out (constant machine-type-name)))])))

  (set-who! compile-script
    (case-lambda
      [(in out machine) (do-compile-script who in out machine #f)]
      [(in out) (do-compile-script who in out (constant machine-type-name) #f)]
      [(in)
       (unless (string? in) ($oops who "~s is not a string" in))
       (let-values ([(in out) (in&out in)])
         (do-compile-script who in out (constant machine-type-name) #f))]))

  (set-who! compile-program
    (let ()
      (define (do-compile-program in out machine)
        (let ([library-collector (make-parameter '())])
          (parameterize ([$require-libraries library-collector])
            (do-compile-script who in out machine #t))
          (library-collector)))
      (case-lambda
        [(in out machine) (do-compile-program in out machine)]
        [(in out) (do-compile-program in out (constant machine-type-name))]
        [(in)
         (unless (string? in) ($oops who "~s is not a string" in))
         (let-values ([(in out) (in&out in)])
           (do-compile-program in out (constant machine-type-name)))])))

  (set-who! maybe-compile-file
    (case-lambda
      [(in out)
       (unless (string? in) ($oops who "~s is not a string" in))
       (unless (string? out) ($oops who "~s is not a string" out))
       ($maybe-compile-file who in out compile-file)
       (void)]
      [(in)
       (unless (string? in) ($oops who "~s is not a string" in))
       (let-values ([(in out) (in&out in)])
         ($maybe-compile-file who in out compile-file))
       (void)]))

  (set-who! maybe-compile-library
    (case-lambda
      [(in out)
       (unless (string? in) ($oops who "~s is not a string" in))
       (unless (string? out) ($oops who "~s is not a string" out))
       ($maybe-compile-file who in out (compile-library-handler))
       (void)]
      [(in)
       (unless (string? in) ($oops who "~s is not a string" in))
       (let-values ([(in out) (in&out in)])
         ($maybe-compile-file who in out (compile-library-handler)))
       (void)]))

  (set-who! maybe-compile-program
    (case-lambda
      [(in out)
       (unless (string? in) ($oops who "~s is not a string" in))
       (unless (string? out) ($oops who "~s is not a string" out))
       ($maybe-compile-file who in out (compile-program-handler))
       (void)]
      [(in)
       (unless (string? in) ($oops who "~s is not a string" in))
       (let-values ([(in out) (in&out in)])
         ($maybe-compile-file who in out (compile-program-handler)))
       (void)]))

  (set-who! compile-to-file
    (rec compile-to-file
      (case-lambda
        [(sexpr* out) (compile-to-file sexpr* out #f #f)]
        [(sexpr* out sfd) (compile-to-file sexpr* out sfd #f)]
        [(sexpr* out sfd force-host-out?)
         (unless (list? sexpr*) ($oops who "~s is not a proper list" sexpr*))
         (unless (string? out) ($oops who "~s is not a string" out))
         (when sfd (unless (source-file-descriptor? sfd) ($oops who "~s is not a source-file descriptor or #f" sfd)))
         (unless (boolean? force-host-out?) ($oops who "~s is not a boolean" force-host-out?))
         (let ([library? (and (= (length sexpr*) 1) (pair? (car sexpr*)) (eq? (caar sexpr*) 'library))]
               [program? (and (= (length sexpr*) 1) (pair? (car sexpr*)) (eq? (caar sexpr*) 'top-level-program))])
           (define (go)
             (do-compile-to-file who out
               (and library?
                    (or force-host-out?
                        (not (eq? (constant machine-type-name) (machine-type))))
                    (format "~a.~s" (path-root out) (machine-type)))
               (constant machine-type-name)
               sfd
               (lambda ()
                 (if (null? sexpr*)
                     (eof-object)
                     (let ([x (car sexpr*)])
                       (set! sexpr* (cdr sexpr*))
                       x)))))
           (if program?
               (let ([library-collector (make-parameter '())])
                 (parameterize ([$require-libraries library-collector]) (go))
                 (library-collector))
               (go)))]))))
);let
