"cpvalid.ss"
;;; cpvalid.ss
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

;;; see comments relating to both cpvalid and cpletrec at front of
;;; cpletrec.ss

(define undefined-variable-warnings
  ($make-thread-parameter #f (lambda (x) (and x #t))))

(define $cpvalid)
(let ()
  (import (nanopass))
  (include "base-lang.ss")

  (define-pass cpvalid : Lsrc (x) -> Lsrc ()
    (definitions
      (with-output-language (Lsrc Expr)
        (define build-let
          (lambda (ids vals body)
            (if (null? ids)
                body
                `(call ,(make-preinfo)
                   (case-lambda ,(make-preinfo-lambda)
                     (clause (,ids ...) ,(length ids) ,body))
                   ,vals ...))))
        (define build-letrec
          (lambda (ids vals body)
            (if (null? ids)
                ; dropping source here; could attach to body or add source record
                body
                `(letrec ([,ids ,vals] ...) ,body))))
        (define build-letrec*
          (lambda (ids vals body)
            (if (null? ids)
                ; dropping source here; could attach to body or add source record
                body
                `(letrec* ([,ids ,vals] ...) ,body)))))

      (define-record-type proxy
        (fields (mutable state))
        (nongenerative)
        (sealed #t)
        (protocol
          (lambda (new)
            (lambda ()
              (new 'protectable)))))

      (define-syntax with-protected
        (syntax-rules ()
          [(_ p e)
           (identifier? #'p)
           (begin
             (when p (proxy-state-set! p 'protected))
             (let-values ([t (let ()
                               (define-syntax p
                                 (lambda (x)
                                   (syntax-error x "can't reference proxy inside with-protected")))
                               e)])
               (when p (proxy-state-set! p 'protectable))
               (apply values t)))]))

      (define-syntax with-unprotected
        (syntax-rules ()
          [(_ p e)
           (identifier? #'p)
           (begin
             (when p (proxy-state-set! p 'unprotected))
             (let-values ([t (let ()
                               (define-syntax p
                                 (lambda (x)
                                   (syntax-error x "can't reference proxy inside with-unprotected")))
                               e)])
               (when p (proxy-state-set! p 'protectable))
               (apply values t)))]))

      (module (with-info with-valid* with-valid** with-proxy with-proxy*
                prelex-info-proxy prelex-info-valid-flag
                set-prelex-info-unsafe! prelex-info-unsafe
                set-prelex-info-referenced! prelex-info-referenced)
        (define-record-type info
          (fields (mutable proxy) (mutable unsafe) (mutable valid-flag) (mutable referenced))
          (nongenerative)
          (sealed #t)
          (protocol
            (lambda (new)
              (lambda ()
                (new #f #f #f #f)))))

        (define-syntax with-info
          (syntax-rules ()
            [(_ ids-expr e)
              (let ([ids ids-expr])
                (for-each
                  (lambda (id)
                    (safe-assert (not (prelex-operand id)))
                    (prelex-operand-set! id (make-info)))
                  ids)
                (let-values ([t e])
                  (for-each
                    (lambda (id)
                      (safe-assert (prelex-operand id))
                      (prelex-operand-set! id #f))
                    ids)
                  (apply values t)))]))

        (define set-prelex-info-valid-flag!
          (lambda (id val)
            (info-valid-flag-set! (prelex-operand id) val)))

        (define prelex-info-valid-flag
          (lambda (id)
            (let ([info (prelex-operand id)])
              (and info (info-valid-flag info)))))

        (define-syntax with-valid*
          (syntax-rules ()
            [(_ valid-flag-expr ids-expr e)
              (let ([valid-flag valid-flag-expr] [ids ids-expr])
                (for-each (lambda (id) (set-prelex-info-valid-flag! id valid-flag)) ids)
                (let-values ([t e])
                  (for-each (lambda (id) (set-prelex-info-valid-flag! id #f)) ids)
                  (apply values t)))]))

        (define-syntax with-valid**
          (syntax-rules ()
            [(_ valid-flags-expr ids-expr e)
              (let ([valid-flags valid-flags-expr] [ids ids-expr])
                (for-each (lambda (id vf) (set-prelex-info-valid-flag! id vf)) ids valid-flags)
                (let-values ([t e])
                  (for-each (lambda (id) (set-prelex-info-valid-flag! id #f)) ids)
                  (apply values t)))]))

        (define-who set-prelex-info-proxy!
          (lambda (id val)
            (let ([info (prelex-operand id)])
              (safe-assert info)
              (info-proxy-set! info val))))

        (define prelex-info-proxy
          (lambda (id)
            (let ([info (prelex-operand id)])
              (and info (info-proxy info)))))

        (define-syntax with-proxy
          (syntax-rules ()
            [(_ proxy-expr id-expr e)
              (let ([proxy proxy-expr] [id id-expr])
                (set-prelex-info-proxy! id proxy)
                (let ([t e])
                  (set-prelex-info-proxy! id #f)
                  t))]))

        (define-syntax with-proxy*
          (syntax-rules ()
            [(_ proxy-expr ids-expr e)
              (let ([proxy proxy-expr] [ids ids-expr])
                (for-each (lambda (id) (set-prelex-info-proxy! id proxy)) ids)
                (let-values ([t e])
                  (for-each (lambda (id) (set-prelex-info-proxy! id #f)) ids)
                  (apply values t)))]))

        (define set-prelex-info-unsafe!
          (lambda (id val)
            (info-unsafe-set! (prelex-operand id) val)))

        (define prelex-info-unsafe
          (lambda (id)
            (info-unsafe (prelex-operand id))))

        (define set-prelex-info-referenced!
          (lambda (id val)
            (let ([info (prelex-operand id)])
              (when info (info-referenced-set! info val)))))

        (define prelex-info-referenced
          (lambda (id)
            (info-referenced (prelex-operand id)))))

      (with-output-language (Lsrc Expr)
        (define insert-valid-check
          (lambda (what maybe-src id p x)
            (if (and p (not (eq? (proxy-state p) 'protected)))
                (let ([valid-flag (prelex-info-valid-flag id)])
                  (if valid-flag
                      (let ([name (prelex-name id)])
                        (let ([mesg (format "attempt to ~a undefined variable ~~s" what)])
                          (when (undefined-variable-warnings)
                            ($source-warning #f maybe-src #t (format "possible ~a" mesg) name))
                          (if (prelex-referenced valid-flag)
                              (set-prelex-multiply-referenced! valid-flag #t)
                              (set-prelex-referenced! valid-flag #t))
                          `(seq
                             (if (ref #f ,valid-flag)
                                 (quote ,(void))
                                 (call ,(make-preinfo) ,(lookup-primref 2 '$source-violation)
                                   (quote #f)
                                   (quote ,maybe-src)
                                   (quote #t)
                                   (quote ,mesg)
                                   (quote ,name)))
                             ,x)))
                      x))
                x))))

      ; wl = worklist
      ; dl = deferred list
      (define (process-letrec-bindings cpvalid proxy proxy-ids ids vals unsafe* dl?)
        (let f ([wl (map list ids vals unsafe*)] [dl '()] [oops #f])
          (if (null? wl)
              (if oops
                  (f dl '() #f)
                  (with-proxy* proxy proxy-ids
                    (map/ormap
                      (lambda (x)
                        (apply (lambda (id val unsafe)
                                 (let-values ([(val dl?) (cpvalid val proxy dl?)])
                                   (values (cons id val) dl?)))
                          x))
                      dl)))
              (apply (lambda (id val unsafe)
                       (define update
                         (lambda (x)
                           (apply (lambda (id val unsafe)
                                    (if (or unsafe (prelex-info-referenced id))
                                        (begin (set-prelex-info-referenced! id #f)
                                          (list id val #t))
                                        x))
                             x)))
                       (if unsafe
                           (let ([val (with-unprotected proxy
                                        (let ([proxy (make-proxy)])
                                          (with-proxy* proxy proxy-ids
                                            (first-value (cpvalid val proxy #f)))))])
                             (let-values ([(ls dl?) (f (map update (cdr wl)) (map update dl) #t)])
                               (values (cons (cons id val) ls) dl?)))
                           (f (cdr wl) (cons (car wl) dl) oops)))
                (car wl)))))

      (define map/ormap
        (case-lambda
          [(p ls)
            (if (null? ls)
                (values '() #f)
                (let-values ([(x b1) (p (car ls))]
                             [(ls b2) (map/ormap p (cdr ls))])
                  (values (cons x ls) (or b1 b2))))]
          [(p ls1 ls2)
            (if (null? ls1)
                (values '() #f)
                (let-values ([(x b1) (p (car ls1) (car ls2))]
                             [(ls b2) (map/ormap p (cdr ls1) (cdr ls2))])
                  (values (cons x ls) (or b1 b2))))]))

      (define deferred?
        (lambda (x)
          (nanopass-case (Lsrc Expr) x
            [(cpvalid-defer ,e) #t]
            [else #f])))

      (with-output-language (Lsrc Expr)
        (define defer-or-not
          (lambda (dl? x)
            (values
              (if (and dl? (not (deferred? x)))
                  `(cpvalid-defer ,x)
                  x)
              dl?))))

      (define-syntax first-value
        (syntax-rules ()
          [(_ e) (let-values ([(x . r) e]) x)]))

      (define undefer*
        (lambda (ls proxy dl?)
          (map/ormap
            (lambda (x) (undefer x proxy dl?))
            ls))))

    (undefer : Expr (x proxy dl?) -> Expr (dl?)
      [(cpvalid-defer ,[undefer-helper : e dl?]) (values e dl?)]
      [else (values x #f)])

    (undefer-helper : Expr (x proxy dl?) -> Expr (dl?)
      [(ref ,maybe-src ,x) (values x #f)]
      [(quote ,d) (values x #f)]
      [,pr (values x #f)]
      ; recognize canonical form of a let after expansion
      [(call ,preinfo0
         (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,[undefer : body body-dl?]))
         ,e* ...)
       (guard (fx= (length e*) interface))
       (let-values ([(e* args-dl?) (undefer* e* proxy dl?)])
         (defer-or-not (or body-dl? args-dl?)
           `(call ,preinfo0
              (case-lambda ,preinfo1
                (clause (,x* ...) ,interface ,body))
              ,e* ...)))]
      [(call ,preinfo ,[undefer : e fun-dl?] ,e* ...)
       (let-values ([(e* args-dl?) (undefer* e* proxy dl?)])
         (defer-or-not (or fun-dl? args-dl?)
           `(call ,preinfo ,e ,e* ...)))]
      [(if ,[undefer : e0 dl0?] ,[undefer : e1 dl1?] ,[undefer : e2 dl2?])
       (defer-or-not (or dl0? dl1? dl2?) `(if ,e0 ,e1 ,e2))]
      [(case-lambda ,preinfo ,cl* ...)
       (cpvalid `(case-lambda ,preinfo ,cl* ...) proxy dl?)]
      [(seq ,[undefer : e1 dl1?] ,[undefer : e2 dl2?])
       (defer-or-not (or dl1? dl2?) `(seq ,e1 ,e2))]
      [(set! ,maybe-src ,x ,[undefer : e dl?])
       (defer-or-not dl? `(set! ,maybe-src ,x ,e))]
      [(letrec ([,x* ,e*] ...) ,[undefer : body body-dl?])
       (let-values ([(e* vals-dl?) (undefer* e* proxy dl?)])
         (defer-or-not (or body-dl? vals-dl?)
           `(letrec ([,x* ,e*] ...) ,body)))]
      [(letrec* ([,x* ,e*] ...) ,[undefer : body body-dl?])
       (let-values ([(e* vals-dl?) (undefer* e* proxy dl?)])
         (defer-or-not (or body-dl? vals-dl?)
           `(letrec* ([,x* ,e*] ...) ,body)))]
      [(foreign (,conv ...) ,name ,[undefer : e dl?] (,arg-type* ...) ,result-type)
       (defer-or-not dl? `(foreign (,conv ...) ,name ,e (,arg-type* ...) ,result-type))]
      [(fcallable (,conv ...) ,[undefer : e dl?] (,arg-type* ...) ,result-type)
       (defer-or-not dl? `(fcallable (,conv ...) ,e (,arg-type* ...) ,result-type))]
      [(cte-optimization-loc ,box ,[undefer : e dl?])
       (defer-or-not dl? `(cte-optimization-loc ,box ,e))]
      [(pariah) (values x #f)]
      [(profile ,src) (values x #f)]
      [(moi) (values x #f)]
      [else (sorry! who "unexpected record ~s" x)])

    (CaseLambdaClause : CaseLambdaClause (ir proxy) -> CaseLambdaClause ()
      [(clause (,x* ...) ,interface ,body)
       (let-values ([(body dl?) (with-protected proxy (cpvalid body #f #f))])
         `(clause (,x* ...) ,interface ,body))])

    (cpvalid : Expr (x proxy dl?) -> Expr (dl?)
      [(ref ,maybe-src ,x)
       (set-prelex-info-referenced! x #t)
       (values
         (let ([p (prelex-info-proxy x)])
           ; unsafe => x might be called.  this can only happen if x has
           ; gotten into the unprotected state
           (when (and p (eq? (proxy-state p) 'unprotected))
             (set-prelex-info-unsafe! x #t))
           (insert-valid-check "reference" maybe-src x p `(ref ,maybe-src ,x)))
         #f)]
      [,pr (values x #f)]
      [(quote ,d) (values x #f)]
      [(call ,preinfo ,pr ,e* ...)
       (guard (all-set? (prim-mask (or proc discard)) (primref-flags pr)))
       (let-values ([(e* dl?) (map/ormap (lambda (e) (cpvalid e proxy dl?)) e*)])
         (defer-or-not dl? `(call ,preinfo ,pr ,e* ...)))]
      ; recognize canonical form of a let after expansion
      [(call ,preinfo0 (case-lambda ,preinfo1 (clause (,x* ...) ,interface ,body)) ,e* ...)
       (guard (fx= (length e*) interface))
       (let ([proxy (or proxy (make-proxy))])
         (with-info x*
           (with-proxy* proxy x*
             (let-values ([(body body-dl?) (cpvalid body proxy dl?)])
               (let-values ([(e* dl?)
                             (map/ormap
                               (lambda (arg id)
                                 (if (prelex-info-unsafe id)
                                     (with-unprotected proxy (cpvalid arg #f #f))
                                     (cpvalid arg proxy dl?)))
                               e* x*)])
                 (defer-or-not (or dl? body-dl?)
                   `(call ,preinfo0
                      (case-lambda ,preinfo1
                        (clause (,x* ...) ,interface ,body))
                      ,e* ...)))))))]
      [(call ,preinfo ,e ,e* ...)
       (values
         (with-unprotected proxy
           `(call ,preinfo
              ,(first-value (cpvalid e #f #f))
              ,(map (lambda (x) (first-value (cpvalid x #f #f))) e*) ...))
         #f)]
      [(case-lambda ,preinfo ,cl* ...)
       (if dl?
           (values `(cpvalid-defer ,x) #t)
           (values
             `(case-lambda ,preinfo ,(map (lambda (cl) (CaseLambdaClause cl proxy)) cl*) ...)
             #f))]
      [(set! ,maybe-src ,x ,e)
       (let-values ([(e dl?)
                      ; rhs is unsafe only if x is referenced
                      (if (prelex-referenced x)
                          (with-unprotected proxy (cpvalid e #f #f))
                          (cpvalid e proxy dl?))])
         (defer-or-not dl?
           (insert-valid-check "assign" maybe-src x (prelex-info-proxy x)
             (first-value
               (defer-or-not dl?
                 `(set! ,maybe-src ,x ,e))))))]
      [(letrec ([,x* ,e*] ...) ,body)
       (with-info x*
         (let*-values ([(proxy) (or proxy (make-proxy))]
                       [(valid-flag) (make-prelex* 'valid?)]
                       [(body body-dl?) (with-proxy* proxy x* (cpvalid body proxy dl?))]
                       [(unsafe*) (map prelex-info-unsafe x*)])
           (for-each
             (lambda (id)
               (set-prelex-info-unsafe! id #f)
               (set-prelex-info-referenced! id #f))
             x*)
           (let*-values ([(alist dl?) (with-valid* valid-flag x*
                                        (process-letrec-bindings cpvalid proxy x* x* e* unsafe* dl?))]
                         [(e*) (map (lambda (id) (cdr (assq id alist))) x*)])
             (defer-or-not (or dl? body-dl?)
               (if (prelex-referenced valid-flag)
                   (begin
                     (set-prelex-assigned! valid-flag #t)
                     (build-let (list valid-flag) (list `(quote #f))
                       (first-value
                         (let-values ([(body body-dl?) (defer-or-not body-dl?
                                                         `(seq
                                                            (set! #f ,valid-flag (quote #t))
                                                            ,body))])
                           (defer-or-not (or dl? body-dl?)
                             (build-letrec x* e* body))))))
                   (build-letrec x* e* body))))))]
      [(letrec* ([,x* ,e*] ...) ,body)
       ; - we do unprotected parts of each rhs plus unsafe lambda pieces
       ;   first and leave remaining lambda expressions to do later.
       ; - a full-blown flow analysis could be even nicer and even make it
       ;   possible to detect references and assignments that are surely
       ;   bad.
       (with-info x*
         (let*-values ([(proxy) (or proxy (make-proxy))]
                       [(valid-flags) (map (lambda (id) (make-prelex* 'valid?)) x*)]
                       [(body body-dl?) (with-proxy* proxy x* (cpvalid body proxy dl?))]
                       [(unsafe*) (map prelex-info-unsafe x*)])
           (define-record-type welt (nongenerative) (sealed #t)
             (fields id (mutable val) unsafe (mutable forbidden-ids) (mutable valid-flags)))
           (define (make-welts x* e* unsafe* valid-flags)
             (let f ([x* x*] [e* e*] [unsafe* unsafe*] [valid-flags valid-flags])
               (if (null? x*)
                   '()
                   (cons (make-welt (car x*) (car e*) (car unsafe*) x* valid-flags)
                     (f (cdr x*) (cdr e*) (cdr unsafe*) (cdr valid-flags))))))
           (define (process-ws w* d*)
             (if (null? w*)
                 (process-letrec-bindings undefer proxy '()
                   (map welt-id d*)
                   (map welt-val d*)
                   (map welt-unsafe d*)
                   dl?)
                 (let ([w (car w*)])
                   (let ([id (welt-id w)]
                         [val (welt-val w)]
                         [unsafe (welt-unsafe w)]
                         [forbidden-ids (welt-forbidden-ids w)]
                         [valid-flags (welt-valid-flags w)])
                     (if (prelex-info-referenced id)
                         (let ([val (with-proxy* proxy forbidden-ids
                                      (with-unprotected proxy
                                        (with-valid** valid-flags forbidden-ids
                                          (first-value
                                            ; could obviate this test with
                                            ; cpvalid-defer case in cpvalid
                                            (if (deferred? val)
                                                (undefer val #f #f)
                                                (cpvalid val #f #f))))))])
                           (let-values ([(ls dl?) (process-ds (cdr w*) d* id (car valid-flags))])
                             (values (cons (cons id val) ls) dl?)))
                         (let-values ([(val dl?) (with-proxy* proxy forbidden-ids
                                                   (with-unprotected proxy
                                                     (with-valid** valid-flags forbidden-ids
                                                       (cpvalid val #f #t))))])
                           (if dl?
                               (begin
                                 ; deferred parts of rhs can reference own lhs, so remove it from forbidden list
                                 (welt-val-set! w val)
                                 (welt-forbidden-ids-set! w (cdr forbidden-ids))
                                 (welt-valid-flags-set! w (cdr valid-flags))
                                 (process-ds (cdr w*) (cons w d*) id (car valid-flags)))
                               (let-values ([(ls dl?) (process-ds (cdr w*) d* id (car valid-flags))])
                                 (values (cons (cons id val) ls) dl?)))))))))
           (define (process-ds w* d* okay-before-id okay-before-valid-flags)
             ; it's okay to reference any rhs before okay-before-id
             ; trim forbidden lists accordingly
             (for-each
               (lambda (w)
                 (cond
                   [(memq okay-before-id (welt-forbidden-ids w)) =>
                     (lambda (x*)
                       (welt-forbidden-ids-set! w x*)
                       (welt-valid-flags-set! w
                         (memq okay-before-valid-flags (welt-valid-flags w))))]))
               d*)
             (let f ([d* d*] [new-d* '()] [oops? #f])
               (if (null? d*)
                   (if oops?
                       (f new-d* '() #f)
                       (process-ws w* new-d*))
                   (let* ([w (car d*)] [id (welt-id w)])
                     (if (prelex-info-referenced id)
                         (let ([val (with-proxy* proxy (welt-forbidden-ids w)
                                      (with-unprotected proxy
                                        (with-valid** (welt-valid-flags w) (welt-forbidden-ids w)
                                          (first-value (undefer (welt-val w) #f #f)))))])
                           (let-values ([(ls dl?) (f (cdr d*) new-d* #t)])
                             (values (cons (cons id val) ls) dl?)))
                         (f (cdr d*) (cons w new-d*) oops?))))))
           (for-each
             (lambda (id)
               (set-prelex-info-unsafe! id #f)
               (set-prelex-info-referenced! id #f))
             x*)
           (let*-values ([(alist dl?) (process-ws (make-welts x* e* unsafe* valid-flags) '())]
                         [(e*) (map (lambda (id) (cdr (assq id alist))) x*)]
                         [(x* e* valid-flags)
                          (let f ([x* x*] [e* e*] [valid-flags valid-flags])
                            (if (null? x*)
                                (values '() '() '())
                                (let ([id (car x*)] [val (car e*)] [vf (car valid-flags)])
                                  (let-values ([(x* e* valid-flags) (f (cdr x*) (cdr e*) (cdr valid-flags))])
                                    (if (prelex-referenced vf)
                                        (begin
                                          (set-prelex-assigned! vf #t)
                                          (values
                                            (list* id (make-prelex* 'dummy) x*) 
                                            (list* val `(set! #f ,vf (quote #t)) e*)
                                            (cons vf valid-flags)))
                                        (values
                                          (cons id x*) 
                                          (cons val e*)
                                          valid-flags))))))])
             (defer-or-not (or dl? body-dl?)
               (build-let valid-flags (make-list (length valid-flags) `(quote #f))
                 (first-value
                   (defer-or-not (or dl? body-dl?)
                     (build-letrec* x* e* body))))))))]
      [(if ,[cpvalid : e0 dl0?] ,[cpvalid : e1 dl1?] ,[cpvalid : e2 dl2?])
       (defer-or-not (or dl0? dl1? dl2?) `(if ,e0 ,e1 ,e2))]
      [(seq ,[cpvalid : e1 dl1?] ,[cpvalid : e2 dl2?])
       (defer-or-not (or dl1? dl2?) `(seq ,e1 ,e2))]
      [(foreign (,conv ...) ,name ,[cpvalid : e dl?] (,arg-type* ...) ,result-type)
       (defer-or-not dl? `(foreign (,conv ...) ,name ,e (,arg-type* ...) ,result-type))]
      [(fcallable (,conv ...) ,[cpvalid : e dl?] (,arg-type* ...) ,result-type)
       (defer-or-not dl? `(fcallable (,conv ...) ,e (,arg-type* ...) ,result-type))]
      [(cte-optimization-loc ,box ,[cpvalid : e dl?])
       (defer-or-not dl? `(cte-optimization-loc ,box ,e))]
      [(pariah) (values x #f)]
      [(profile ,src) (values x #f)]
      [(moi) (values x #f)]
      [else (sorry! who "unexpected record ~s" x)])
    (first-value (cpvalid x #f #f)))

  (set! $cpvalid
    (lambda (x)
      (if (= (optimize-level) 3) x (cpvalid x)))))
