;;; cprep.ss
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


(let ()
  (import (nanopass))
  (include "types.ss")
  (include "base-lang.ss")
  (include "expand-lang.ss")

  (define-who Lexpand-to-go
    (lambda (x go)
      (define-pass go-Inner : (Lexpand Inner) (ir) -> * (val)
        (Inner : Inner (ir) -> * (val)
          [,lsrc (go lsrc)]
          [(program ,uid ,body) (go ($build-invoke-program uid body))]
          [(library/ct ,uid (,export-id* ...) ,import-code ,visit-code)
           (go ($build-install-library/ct-code uid export-id* import-code visit-code))]
          [(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...) ,body)
           (go ($build-install-library/rt-code uid dl* db* dv* de* body))]
          [(library/ct-info ,linfo/ct)
           `(library/ct-info ,(library-info-uid linfo/ct) ,(library/ct-info-import-req* linfo/ct)
              ,(library/ct-info-visit-visit-req* linfo/ct)
              ,(library/ct-info-visit-req* linfo/ct))]
          [(library/rt-info ,linfo/rt) `(library/rt-info ,(library-info-uid linfo/rt) ,(library/rt-info-invoke-req* linfo/rt))]
          [(program-info ,pinfo) `(program-info ,(program-info-invoke-req* pinfo))])
        (Inner ir))
      (let ([x* (let f ([x x] [x* '()])
                  (nanopass-case (Lexpand Outer) x
                    [(group ,outer1 ,outer2) (f outer1 (f outer2 x*))]
                    [(visit-only ,inner) (cons `(eval-when (visit) ,(go-Inner inner)) x*)]
                    [(revisit-only ,inner) (cons `(eval-when (revisit) ,(go-Inner inner)) x*)]
                    [,inner (cons (go-Inner inner) x*)]
                    [(recompile-info ,rcinfo) (cons `(recompile-requirements ,(recompile-info-import-req* rcinfo) ,(recompile-info-include-req* rcinfo)) x*)]
                    [else (sorry! who "unexpected language form ~s" x)]))])
        (safe-assert (not (null? x*)))
        (cond
          [(= (length x*) 1) (car x*)]
          [else `(begin ,@x*)]))))

  (set-who! $uncprep
    (rec $uncprep
      (case-lambda
        [(x) ($uncprep x #f)]
        [(x sexpr?)
         (define cache-sexpr
           (lambda (preinfo thunk)
             (if sexpr?
                 (or (preinfo-sexpr preinfo)
                     (let ([sexpr (thunk)])
                       (preinfo-sexpr-set! preinfo sexpr)
                       sexpr))
                 (thunk))))
         (define get-name
           (lambda (x)
             (if sexpr? (prelex-name x) (prelex-uname x))))
         (define uncprep-lambda-clause
           (lambda (cl)
             (nanopass-case (Lsrc CaseLambdaClause) cl
               [(clause (,x* ...) ,interface ,body)
                `(,(if (fx< interface 0)
                       (let f ((x* x*))
                         (if (pair? (cdr x*))
                             (cons (get-name (car x*)) (f (cdr x*)))
                             (get-name (car x*))))
                       (map get-name x*))
                   ,@(uncprep-sequence body '()))])))
         (define uncprep-sequence
           (lambda (x ls)
             (nanopass-case (Lsrc Expr) x
               [(profile ,src) (guard (not (null? ls))) ls]
               [(seq ,e1 ,e2)
                (uncprep-sequence e1
                  (uncprep-sequence e2 ls))]
               [else (cons (uncprep x) ls)])))
         (define uncprep-fp-conv
           (lambda (x*)
             (map (lambda (x)
                    (case x
                      [(i3nt-stdcall) '__stdcall]
                      [(i3nt-com) '__com]
                      [(adjust-active) '__collect_safe]
                      [else #f]))
                  x*)))
         (define-who uncprep-fp-specifier
           (lambda (x)
             (nanopass-case (Ltype Type) x
               [(fp-void) 'void]
               [(fp-integer ,bits)
                (case bits
                  [(8) 'integer-8]
                  [(16) 'integer-16]
                  [(32) 'integer-32]
                  [(64) 'integer-64]
                  [else ($oops who "invalid integer size ~s" bits)])]
               [(fp-unsigned ,bits)
                (case bits
                  [(8) 'unsigned-8]
                  [(16) 'unsigned-16]
                  [(32) 'unsigned-32]
                  [(64) 'unsigned-64]
                  [else ($oops who "invalid unsigned size ~s" bits)])]
               [(fp-scheme-object) 'scheme-object]
               [(fp-u8*) 'u8*]
               [(fp-u16*) 'u16*]
               [(fp-u32*) 'u32*]
               [(fp-fixnum) 'fixnum]
               [(fp-double-float) 'double-float]
               [(fp-single-float) 'single-float]
               [(fp-ftd ,ftd) 'ftype]
               [(fp-ftd& ,ftd) 'ftype])))
         (define uncprep
           (lambda (x)
             (define keyword?
               (lambda (x)
                 (memq x
                   ; UPDATE THIS if new keywords are added
                   '(let $primitive quote begin case-lambda
                      library-case-lambda lambda if set!
                      letrec letrec* $foreign-procedure
                      $foreign-callable eval-when))))
             (nanopass-case (Lsrc Expr) x
               [(ref ,maybe-src ,x) (get-name x)]
               [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
                (guard (fx= (length e*) interface))
                (cache-sexpr preinfo0
                  (lambda ()
                    (if (null? x*)
                        (uncprep body)
                        `(let ,(map (lambda (x e)
                                      `(,(get-name x) ,(uncprep e)))
                                 x* e*)
                           ,@(uncprep-sequence body '())))))]
               [(call ,preinfo ,pr (quote ,d))
                (guard (eq? (primref-name pr) '$top-level-value) (symbol? d)
                  (not (or (eq? ($real-sym-name d (interaction-environment)) d) (keyword? d))))
                (cache-sexpr preinfo
                  (lambda ()
                    ($real-sym-name d (interaction-environment))))]
               [(call ,preinfo ,pr (quote ,d) ,e)
                (guard (eq? (primref-name pr) '$set-top-level-value!) (symbol? d)
                  (not (or (eq? ($real-sym-name d (interaction-environment)) d) (keyword? d))))
                (cache-sexpr preinfo
                  (lambda ()
                    `(set! ,($real-sym-name d (interaction-environment)) ,(uncprep e))))]
               [(call ,preinfo ,e ,e* ...)
                (cache-sexpr preinfo
                  (lambda ()
                    `(,(uncprep e) ,@(map uncprep e*))))]
               [,pr (let ([sym (primref-name pr)])
                      (if sexpr?
                          ($sgetprop sym '*unprefixed* sym)
                          `($primitive ,(primref-level pr) ,sym)))]
               [(quote ,d)
                (cond
                  [(eq? d (void)) '(#2%void)]
                  [(self-evaluating? d) d]
                  [else `(quote ,d)])]
               [(seq ,e1 ,e2)
                (let ([ls (uncprep-sequence x '())])
                  (if (null? (cdr ls))
                      (car ls)
                      `(begin ,@ls)))]
               [(case-lambda ,preinfo ,cl* ...)
                (cache-sexpr preinfo
                  (lambda ()
                    (let ((cl* (map uncprep-lambda-clause cl*)))
                      (if (and (not (null? cl*)) (null? (cdr cl*)))
                          `(lambda ,@(car cl*))
                          `(case-lambda ,@cl*)))))]
               [(if ,[e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
               [(set! ,maybe-src ,x ,[e]) `(set! ,(get-name x) ,e)]
               [(letrec ([,x* ,[e*]] ...) ,body)
                `(letrec ,(map (lambda (x e) `(,(get-name x) ,e)) x* e*)
                   ,@(uncprep-sequence body '()))]
               [(letrec* ([,x* ,[e*]] ...) ,body)
                `(letrec* ,(map (lambda (x e) `(,(get-name x) ,e)) x* e*)
                   ,@(uncprep-sequence body '()))]
               [(foreign (,conv* ...) ,name ,[e] (,arg-type* ...) ,result-type)
                `($foreign-procedure ,(uncprep-fp-conv conv*) ,name ,e
                   ,(map uncprep-fp-specifier arg-type*)
                   ,(uncprep-fp-specifier result-type))]
               [(fcallable (,conv* ...) ,[e] (,arg-type* ...) ,result-type)
                `($foreign-callable ,(uncprep-fp-conv conv*) ,e
                   ,(map uncprep-fp-specifier arg-type*)
                   ,(uncprep-fp-specifier result-type))]
               [(record-ref ,rtd ,type ,index ,[e]) `(record-ref ,rtd ',type ,e ,index)]
               [(record-set! ,rtd ,type ,index ,[e1] ,[e2]) `(record-set! ,rtd ',type ,e1 ,index ,e2)]
               [(record ,rtd ,[rtd-expr] ,[e*] ...) `(record ,rtd ,rtd-expr ,@e*)]
               [(record-type ,rtd ,[e]) `(record-type ,rtd ,e)]
               [(record-cd ,rcd ,rtd-expr ,[e]) `(record-cd ,rcd ,e)]
               [(immutable-list (,e* ...) ,[e]) e]
               [(moi) ''moi]
               [(pariah) `(pariah (void))]
               [(profile ,src) `(void)]
               [(cte-optimization-loc ,box ,[e]) e]
               ; for debugging:
               [(cpvalid-defer ,[e]) `(cpvalid-defer ,e)]
               [else ($oops who "unexpected record ~s" x)])))
         (Lexpand-to-go x uncprep)])))

  (let ()
    (define (default-env)
      (if (eq? (subset-mode) 'system)
          ($system-environment)
          (interaction-environment)))
    (define e/o
      (lambda (who cte? x env)
        (define (go x)
          ($uncprep
            ($cpcommonize
              ($cpcheck
                (let ([cpletrec-ran? #f])
                  (let ([x ((run-cp0)
                            (lambda (x)
                              (set! cpletrec-ran? #t)
                              ($cpletrec ($cp0 x $compiler-is-loaded?)))
                            ($cpvalid x))])
                    (if cpletrec-ran? x ($cpletrec x))))))))
        (unless (environment? env)
          ($oops who "~s is not an environment" env))
        ; claim compiling-a-file to get cte as well as run-time code
        (Lexpand-to-go (expand x env #t cte?) go)))
    (set-who! expand/optimize
      (case-lambda
        [(x) (e/o who #f x (default-env))]
        [(x env) (e/o who #f x env)]))
    (set-who! $expand/cte/optimize
      (case-lambda
        [(x) (e/o who #t x (default-env))]
        [(x env) (e/o who #t x env)]))
    (set-who! $expand/cte
      (rec expand/cte
        (case-lambda
          [(x) (expand/cte x (default-env))]
          [(x env)
           (unless (environment? env)
             ($oops who "~s is not an environment" env))
           ; claim compiling-a-file to get cte as well as run-time code
           ($uncprep (expand x env #t #t))]))))

  (set-who! $cpcheck-prelex-flags
    (lambda (x after-pass)
      (import (nanopass))
      (include "base-lang.ss")

      (define-pass cpcheck-prelex-flags : Lsrc (ir) -> Lsrc ()
        (definitions
          #;(define sorry!
            (lambda (who str . arg*)
              (apply fprintf (console-output-port) str arg*)
              (newline (console-output-port))))
          (define initialize-id!
            (lambda (id)
              (prelex-flags-set! id
                (let ([flags (prelex-flags id)])
                  (fxlogor
                    (fxlogand flags (constant prelex-sticky-mask))
                    (fxsll (fxlogand flags (constant prelex-is-mask))
                      (constant prelex-was-flags-offset))))))))
        (Expr : Expr (ir) -> Expr ()
          [(ref ,maybe-src ,x)
           (when (prelex-operand x) (sorry! who "~s has an operand after ~s (src ~s)" x after-pass maybe-src))
           (unless (prelex-was-referenced x) (sorry! who "~s referenced but not so marked after ~s (src ~s)" x after-pass maybe-src))
           (when (prelex-referenced x)
             (unless (prelex-was-multiply-referenced x) (sorry! who "~s multiply referenced but not so marked after ~s (src ~s)" x after-pass maybe-src))
             (set-prelex-multiply-referenced! x #t))
           (set-prelex-referenced! x #t)
           `(ref ,maybe-src ,x)]
          [(set! ,maybe-src ,x ,[e])
           (unless (prelex-was-assigned x) (sorry! who "~s assigned but not so marked after ~s (src ~s)" x after-pass maybe-src))
           (set-prelex-assigned! x #t)
           `(set! ,maybe-src ,x ,e)]
          [(letrec ([,x* ,e*] ...) ,body)
           (for-each initialize-id! x*)
           `(letrec ([,x* ,(map Expr e*)] ...) ,(Expr body))]
          [(letrec* ([,x* ,e*] ...) ,body)
           (for-each initialize-id! x*)
           `(letrec* ([,x* ,(map Expr e*)] ...) ,(Expr body))])
        (CaseLambdaClause : CaseLambdaClause (ir) -> CaseLambdaClause ()
          [(clause (,x* ...) ,interface ,body)
           (for-each initialize-id! x*)
           `(clause (,x* ...) ,interface ,(Expr body))]))
      (Lexpand-to-go x cpcheck-prelex-flags)))

  (set-who! $insert-profile-src! ; called from compiler only
    (lambda (st x)
      ; NB: the output should be *, but nanopass won't autogenerate the pass
      (define-pass record-coverage-info! : Lsrc (ir) -> Lsrc ()
        (Expr : Expr (ir) -> Expr ()
          [(profile ,src) (source-table-set! st src 0) `(profile ,src)]))
      (Lexpand-to-go x record-coverage-info!)))
  )
